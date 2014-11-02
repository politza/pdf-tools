/* Copyright (C) 2013, 2014  Andreas Politz
 * 
 * Author: Andreas Politz <politza@fh-trier.de>
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>. */

#include <assert.h>
#include <err.h>
#include <error.h>
#include <glib.h>
#include <poppler.h>
#include <cairo.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "synctex_parser.h"
#include "epdfinfo.h"
#include "config.h"


/* declarations */
static arg_t *parse_args(const ctxt_t *ctx, const char *args, size_t len,
                         const cmd_t *cmd);
static ptrdiff_t parse_next_arg (const char *args, char *buf);
static void free_args (arg_t *args, size_t n);
static void free_doc (doc_t *doc);
static doc_t *open_document (const ctxt_t *ctx, const char *filename, const char *passwd, GError **error);
static size_t release_documents (const ctxt_t *ctx, gboolean all);
static char* strchomp (char *str);
static char* mktempfile();
static void print_escaped(const char *str, int );
static void printf_error(const char *fmt, ...);
static void
print_action_dest (PopplerDocument *doc, PopplerAction *action);
static void
print_action (PopplerDocument *doc, PopplerAction *action);

static GList*
get_annots_for_page (doc_t *doc, gint pn);
static annot_t*
get_annot_by_key (doc_t *doc, const gchar *key);
static annot_t*
get_annot_by_key_or_error (doc_t *doc, const gchar *key);
static void
print_annot (const annot_t *annot, /* const */ PopplerPage *page);

static void cmd_features (const ctxt_t *ctx, const arg_t *args);
static void cmd_open (const ctxt_t *ctx, const arg_t *args);
static void cmd_close (const ctxt_t *ctx, const arg_t *args);
static void cmd_save (const ctxt_t *ctx, const arg_t *args);
static void cmd_closeall (const ctxt_t *ctx, const arg_t *args);
static void cmd_search (const ctxt_t *ctx, const arg_t *args);
static void
cmd_outline_walk (PopplerDocument *doc, PopplerIndexIter *iter, int depth);
static void cmd_outline (const ctxt_t *ctx, const arg_t *args);
static void cmd_metadata (const ctxt_t *ctx, const arg_t *args);
static void cmd_quit (const ctxt_t *ctx, const arg_t *args);
static void cmd_npages (const ctxt_t *ctx, const arg_t *args);
static void cmd_pagelinks(const ctxt_t *ctx, const arg_t *args);
static void cmd_gettext(const ctxt_t *ctx, const arg_t *args);
static void cmd_getselection (const ctxt_t *ctx, const arg_t *args);
static void cmd_pagesize(const ctxt_t *ctx, const arg_t *args);

static void cmd_getattachment_from_annot (const ctxt_t *ctx, const arg_t *args);
static void cmd_getattachments (const ctxt_t *ctx, const arg_t *args);

static void cmd_getannots(const ctxt_t *ctx, const arg_t *args);
static void cmd_getannot(const ctxt_t *ctx, const arg_t *args);

#ifdef HAVE_POPPLER_ANNOT_WRITE
static void cmd_addannot (const ctxt_t *ctx, const arg_t *args);
static void cmd_delannot (const ctxt_t *ctx, const arg_t *args);
static void cmd_editannot (const ctxt_t *ctx, const arg_t *args);
#endif  /* HAVE_POPPLER_ANNOT_WRITE */
static void cmd_synctex_forward_search (const ctxt_t *ctx, const arg_t *args);
static void cmd_synctex_backward_search (const ctxt_t *ctx, const arg_t *args);


/* command specs */
const args_spec_t cmd_features_spec[] = {};

const args_spec_t cmd_open_spec[] =
  {
    ARG_STRING_NONEMPTY,        /* filename */
    ARG_STRING,                 /* password */
  };

const args_spec_t cmd_close_spec[] =
  {
    ARG_STRING_NONEMPTY         /* filename */
  };

const args_spec_t cmd_search_spec[] =
  {
    ARG_DOC,
    ARG_FLAG,                   /* ignore-case */
    ARG_NATNUM,                 /* first page */
    ARG_NATNUM,                 /* last page */
    ARG_STRING_NONEMPTY,        /* search string */
  };

const args_spec_t cmd_metadata_spec[] =
  {
    ARG_DOC,
  };

const args_spec_t cmd_outline_spec[] =
  {
    ARG_DOC,
  };

const args_spec_t cmd_quit_spec[] = {};

const args_spec_t cmd_npages_spec[] =
  {
    ARG_DOC
  };

const args_spec_t cmd_pagelinks_spec[] =
  {
    ARG_DOC,
    ARG_NATNUM                  /* page number */
  };

const args_spec_t cmd_gettext_spec[] =
  {
    ARG_DOC,
    ARG_NATNUM,                 /* page number */
    ARG_EDGE,                   /* x0 */
    ARG_EDGE,                   /* y0 */
    ARG_EDGE,                   /* x1 */
    ARG_EDGE,                   /* y1 */
    ARG_NATNUM                  /* selection-style */
  };

const args_spec_t cmd_getselection_spec[] =
  {
    ARG_DOC,
    ARG_NATNUM,                 /* page number */
    ARG_EDGE,                   /* x0 */
    ARG_EDGE,                   /* y0 */
    ARG_EDGE,                   /* x1 */
    ARG_EDGE,                   /* y1 */
    ARG_NATNUM                  /* selection-style */
  };

const args_spec_t cmd_pagesize_spec[] =
  {
    ARG_DOC,
    ARG_NATNUM                  /* page number */
  };

const args_spec_t cmd_getannots_spec[] =
  {
    ARG_DOC,
    ARG_NATNUM,                 /* first page */
    ARG_NATNUM                  /* last page */
  };

const args_spec_t cmd_getannot_spec[] =
  {
    ARG_DOC,
    ARG_STRING_NONEMPTY,        /* annotation's key */
  };

const args_spec_t cmd_getattachment_from_annot_spec[] =
  {
    ARG_DOC,
    ARG_STRING_NONEMPTY,        /* annotation's name */
    ARG_FLAG                    /* save attachment */
  };

/* document-level attachments */
const args_spec_t cmd_getattachments_spec[] =
  {
    ARG_DOC,
    ARG_FLAG,        /* save attachments */
  };

#ifdef HAVE_POPPLER_ANNOT_WRITE
const args_spec_t cmd_save_spec[] =
  {
    ARG_DOC,
  };


const args_spec_t cmd_delannot_spec[] =
  {
    ARG_DOC,
    ARG_STRING_NONEMPTY         /* Annotation's key */
  };

const args_spec_t cmd_editannot_spec[] =
  {
    ARG_DOC,
    ARG_STRING_NONEMPTY,        /* Key */
    ARG_STRING,                 /* Bitmask of set values */
                                /* Edges */
    ARG_EDGE,                   /* x1 */
    ARG_EDGE,                   /* y1 */
    ARG_EDGE_OR_NEG,            /* x2 or keep width, if negative */
    ARG_EDGE_OR_NEG,            /* y2 or keep height, if negative */

    ARG_STRING,                 /* color */
    ARG_STRING,                 /* contents */
    ARG_STRING,                 /* label (markup only) */
    ARG_NATNUM,                 /* isopen (text only) */
    ARG_STRING,                 /* icon (text only) */
  };

const args_spec_t cmd_addannot_spec[] =
  {
    ARG_DOC,
    ARG_NATNUM,                 /* page number */
    ARG_EDGE,                   /* x0 */
    ARG_EDGE,                   /* y0 */
    ARG_EDGE,                   /* x1 */
    ARG_EDGE                    /* y1 */
  };

#endif  /* HAVE_POPPLER_ANNOT_WRITE */

const args_spec_t cmd_synctex_forward_search_spec[] =
  {
    ARG_DOC,
    ARG_STRING_NONEMPTY,        /* source file */
    ARG_NATNUM,                 /* line number */
    ARG_NATNUM                  /* column number */
  };

const args_spec_t cmd_synctex_backward_search_spec[] =
  {
    ARG_DOC,
    ARG_NATNUM,                 /* page number */
    ARG_EDGE,                   /* x */
    ARG_EDGE                    /* y */
  };

static const cmd_t cmds [] =
  {
    /* Basic */
    {"features", cmd_features, cmd_features_spec, G_N_ELEMENTS (cmd_features_spec)},
    {"open", cmd_open, cmd_open_spec, G_N_ELEMENTS (cmd_open_spec)},
    {"close", cmd_close, cmd_close_spec, G_N_ELEMENTS (cmd_close_spec)},
    {"quit", cmd_quit, cmd_quit_spec, G_N_ELEMENTS (cmd_quit_spec)},

    /* General Informations */
    {"search", cmd_search, cmd_search_spec, G_N_ELEMENTS (cmd_search_spec)},
    {"metadata", cmd_metadata, cmd_metadata_spec, G_N_ELEMENTS (cmd_metadata_spec)},
    {"outline", cmd_outline, cmd_outline_spec, G_N_ELEMENTS (cmd_outline_spec)},
    {"number-of-pages", cmd_npages, cmd_npages_spec, G_N_ELEMENTS (cmd_npages_spec)},
    {"pagelinks", cmd_pagelinks, cmd_pagelinks_spec , G_N_ELEMENTS (cmd_pagelinks_spec)},
    {"gettext", cmd_gettext, cmd_gettext_spec, G_N_ELEMENTS (cmd_gettext_spec)},
    {"getselection", cmd_getselection, cmd_getselection_spec,
     G_N_ELEMENTS (cmd_getselection_spec)},
    {"pagesize", cmd_pagesize, cmd_pagesize_spec, G_N_ELEMENTS (cmd_pagesize_spec)},

    /* Annotations */
    {"getannots", cmd_getannots, cmd_getannots_spec , G_N_ELEMENTS (cmd_getannots_spec)},
    {"getannot", cmd_getannot, cmd_getannot_spec, G_N_ELEMENTS (cmd_getannot_spec)},
#ifdef HAVE_POPPLER_ANNOT_WRITE
    {"addannot", cmd_addannot, cmd_addannot_spec, G_N_ELEMENTS (cmd_addannot_spec)},
    {"delannot", cmd_delannot, cmd_delannot_spec, G_N_ELEMENTS (cmd_delannot_spec)},
    {"editannot", cmd_editannot, cmd_editannot_spec
     , G_N_ELEMENTS (cmd_editannot_spec)},
    {"save", cmd_save, cmd_save_spec, G_N_ELEMENTS (cmd_save_spec)} ,
#endif
    /* Attachments */
    {"getattachment-from-annot", cmd_getattachment_from_annot,
     cmd_getattachment_from_annot_spec,
     G_N_ELEMENTS (cmd_getattachment_from_annot_spec)},
    {"getattachments", cmd_getattachments, cmd_getattachments_spec,
     G_N_ELEMENTS (cmd_getattachments_spec)},
    /* Synctex */
    {"synctex-forward-search", cmd_synctex_forward_search, cmd_synctex_forward_search_spec,
     G_N_ELEMENTS (cmd_synctex_forward_search_spec)},
    {"synctex-backward-search", cmd_synctex_backward_search, cmd_synctex_backward_search_spec,
     G_N_ELEMENTS (cmd_synctex_backward_search_spec)}
  };

static const char *poppler_action_type_strings[] =
  {
    "unknown",
    "none",
    "goto-dest",
    "goto-remote",
    "launch",
    "uri",
    "goto-dest", /* "named",  */
    "movie",
    "rendition",
    "ocg-state",
    "javascript"
  };

static const char *poppler_annot_type_strings[] =
  {
    "unknown",
    "text",
    "link",
    "free-text",
    "line",
    "square",
    "circle",
    "polygon",
    "poly-line",
    "highlight",
    "underline",
    "squiggly",
    "strike-out",
    "stamp",
    "caret",
    "ink",
    "popup",
    "file",
    "sound",
    "movie",
    "widget",
    "screen",
    "printer-mark",
    "trap-net",
    "watermark",
    "3d"
  };

static const char *poppler_annot_text_state_strings[] =
  {
    "marked",
    "unmarked",
    "accepted",
    "rejected",
    "cancelled",
    "completed",
    "none",
    "unknown"
  };  

int main(int argc, char **argv)
{
  ctxt_t ctx;
  char *line;
  size_t buflen = IN_BUF_LEN;
  ssize_t read;
  char *error_log = "/dev/null";
  
  if (argc > 2)
    {
      fprintf(stderr, "usage: epdfinfo [ERROR-LOGFILE]\n");
      exit (EXIT_FAILURE);
    }
  if (argc == 2)
    error_log = argv[1];
  
  if (! freopen (error_log, "w", stderr))
    err (2, "Unable to redirect stderr");

  g_type_init ();
  line = (char*) g_malloc( IN_BUF_LEN * sizeof (char));             
  ctx.documents = g_hash_table_new (g_str_hash, g_str_equal);

  while ((read = getline (&line, &buflen, stdin)) != -1) 
    {
      size_t cmd_end;
      arg_t *cmd_args;
      int i;

      if (read <= 1 || line[read - 1] != '\n')
        continue;
      
      line[read - 1] = '\0';
      cmd_end = strcspn (line, ":");

      for (i = 0; i < G_N_ELEMENTS (cmds);  i++)
        {
          if (strncmp (cmds[i].name, line, strlen(cmds[i].name)) == 0)
            {
              if (cmds[i].nargs == 0
                  || (cmd_args = parse_args (&ctx, line + cmd_end,
                                             read - cmd_end, cmds + i)))
                {
                  cmds[i].execute (&ctx, cmd_args);
                  if (cmds[i].nargs > 0)
                    free_args (cmd_args, cmds[i].nargs);
                }
              break;
            }
        }
      if (G_N_ELEMENTS (cmds) == i)
        {
          if (cmd_end < read)
            line[cmd_end] = '\0';
          printf_error ("Unknown command: %s", line);
        }
      /* release_documents (&ctx, FALSE); */
    }

  if (ferror (stdin))
    err (2, NULL);
  exit (EXIT_SUCCESS);
}

static arg_t*
parse_args(const ctxt_t *ctx, const char *args, size_t len,
           const cmd_t *cmd)
{
  gboolean failure = FALSE;
  int i;
  ssize_t read;
  GError *gerror = NULL;
  char *buf = g_malloc (len * sizeof (char));
  arg_t *cmd_args = g_malloc0 (cmd->nargs * sizeof (arg_t));
  
  for (i = 0; i < cmd->nargs && !failure; ++i)
    {
      if (! (read = parse_next_arg (args, buf)))
        {
          printf_error ("Command `%s' expects %d argument(s), %d given",
                        cmd->name, cmd->nargs, i);
          failure = TRUE;
          break;
        }
      
      args = args + read;

      switch (cmd->args_spec[i])
        {
        case ARG_DOC:
          {
            doc_t *doc = open_document (ctx, buf, NULL, &gerror);
            if (doc == NULL)
              {
                printf_error ("Error opening %s:%s"
                              , buf, gerror ? gerror->message : "?");
                failure = TRUE;
                if (gerror)
                  {
                    g_error_free (gerror);
                    gerror = NULL;
                  }
              }
            else
              cmd_args[i].value.doc = doc;
            break;
          }
        case ARG_FLAG:
          if (strcmp (buf, "0") && strcmp (buf, "1"))
            {
              printf_error ("Expected 0 or 1:%s", buf);
              failure = TRUE;
            }
          else
            cmd_args[i].value.flag = *buf == '1';
          break;
        case ARG_STRING_NONEMPTY:
          if (! *buf)
            {
              printf_error ("Non-empty string expected");
              failure = TRUE;
              break;
            }
          /* fall through */
        case ARG_STRING:
          cmd_args[i].value.string = g_strdup (buf);
          break;
        case ARG_NATNUM:
          {
            char *endptr;
            long n = strtol (buf, &endptr, 0);
            if (*endptr || (n < 0))
              {
                printf_error ("Expected natural number:%s", buf);
                failure = TRUE;
              }
            else
              cmd_args[i].value.natnum = n;
            break;
          }
        case ARG_EDGE_OR_NEG:
        case ARG_EDGE:
          {
            char *endptr;
            double n = strtod (buf, &endptr);
            if (*endptr || (cmd->args_spec[i] == ARG_EDGE && n < 0.0) || n > 1.0)
              {
                printf_error ("Expected 0 <= float <= 1:%s", buf);
                failure = TRUE;
              }
            else
              cmd_args[i].value.edge = n;
            break;
          }
        default:
          error (2, 0, "switch fell through");
        }
      cmd_args[i].type = cmd->args_spec[i];
    }

  if (!failure && *args)
    {
      printf_error ("Command `%s' accepts at most %d argument(s)", cmd->name, cmd->nargs);
      failure = TRUE;
    }
      
  g_free(buf);
  if (failure)
    {
      free_args (cmd_args, cmd->nargs);
      return NULL;
    }

  return cmd_args;
}

static ptrdiff_t
parse_next_arg (const char *args, char *buf)
{
  char *buf_p = buf;
  const char *args_p = args;
  gboolean esc = FALSE;

  if (*args)
    {
      assert (*args == ':');
      ++args;
    }
  while (*args)
    {
      if (esc)
        {
          if (*args == 'n')
            {
              ++args;
              *buf_p++ = '\n';
            }
          else
            *buf_p++ = *args++;
          esc = FALSE;
        }
      else if (*args == '\\')
        {
          ++args;
          esc = TRUE;
        }
      else if (*args != ':')
        {
          *buf_p++ = *args++;
        }
      else
        {
          break;
        }
    }
  *buf_p = '\0';
  return args - args_p;
}
static doc_t*
open_document (const ctxt_t *ctx, const char *filename
               , const char *passwd, GError **gerror)
{
  char *uri;
  doc_t *doc = g_hash_table_lookup (ctx->documents, filename);

  if (NULL != doc)
    {
      doc->last_used = time (NULL);
      return doc;
    }

  doc = g_malloc0(sizeof (doc_t));
  uri = g_filename_to_uri (filename, NULL, gerror);
  if (uri != NULL)
    doc->pdf = poppler_document_new_from_file(uri, passwd, gerror);

  if (NULL == doc->pdf)
    {
      g_free (doc);
      doc = NULL;
    }
  else
    {
      doc->filename = g_strdup (filename);
      doc->passwd = g_strdup (passwd);
      doc->allow_auto_release = passwd != NULL;
      doc->last_used = time (NULL);
      g_hash_table_insert (ctx->documents, doc->filename, doc);
    }
  g_free (uri);
  return doc;
}

static size_t
release_documents (const ctxt_t *ctx, gboolean all)
{
  time_t now = time (NULL);
  GHashTableIter iter;
  gpointer key, value;
  size_t removed = 0;

  g_hash_table_iter_init (&iter, ctx->documents);
  while (g_hash_table_iter_next (&iter, &key, &value))
    {
      doc_t *doc = (doc_t*) value;
      if (all || (doc->allow_auto_release
                  && doc->last_used + RELEASE_DOC_TIMEOUT < now))
        {
          free_doc (doc);
          g_hash_table_iter_remove (&iter);
          ++removed;
        }
    }
  return removed;
}
             
static void
print_escaped (const char *str, int print_add)
{
  if (str)
    {
      while (*str)
        {
          switch (*str)
            {
            case '\n':
              printf ("\\n");
              break;
            case '\\':
              printf ("\\\\");
              break;
            case ':':
              printf ("\\:");
              break;
            default:
              putchar (*str);
            }
          ++str;
        }
    }

  switch (print_add)
    {
    case NL:
      putchar ('\n');
      break;
    case COLON:
      putchar (':');
      break;
    }
}

static void printf_error (const char *fmt, ...)
{
  va_list va;
  puts ("ERR");
  va_start (va, fmt);
  vprintf (fmt, va);
  va_end (va);
  puts ("\n.");
}

/* PDF Actions */

static gboolean
is_handled_action (PopplerAction *action)
{
  if (! action)
    return FALSE;
  
  switch (action->any.type)
    {
    case POPPLER_ACTION_GOTO_REMOTE:
    case POPPLER_ACTION_GOTO_DEST:
    case POPPLER_ACTION_NAMED:
      /* case POPPLER_ACTION_LAUNCH: */
    case POPPLER_ACTION_URI:
      return TRUE;
    default: break;      
    }
  return FALSE;
}

static void
print_action (PopplerDocument *doc, PopplerAction *action)
{
  if (! is_handled_action (action))
    return;

  print_escaped (poppler_action_type_strings[action->any.type], COLON);
  print_escaped (action->any.title, COLON);
  switch (action->any.type)
    {
    case POPPLER_ACTION_GOTO_REMOTE:
    case POPPLER_ACTION_GOTO_DEST:
    case POPPLER_ACTION_NAMED:
      print_action_dest (doc, action);
      putchar ('\n');
      break;
    case POPPLER_ACTION_LAUNCH:
      print_escaped (action->launch.file_name, COLON);
      print_escaped (action->launch.params, NL);
      break;
    case POPPLER_ACTION_URI:
      print_escaped (action->uri.uri, NL);
      break;
    default: break;
    }
}

static void
print_action_dest (PopplerDocument *doc, PopplerAction *action)
{
  PopplerDest *dest = NULL;
  gboolean free_dest = FALSE;
  double width, height, top;
  PopplerPage *page;
  int saved_stdin;
    
  if (action->any.type == POPPLER_ACTION_GOTO_DEST
      && action->goto_dest.dest->type == POPPLER_DEST_NAMED)
    {
      DISCARD_STDOUT (saved_stdin);
      /* poppler_document_find_dest reports errors to stdout, so
         discard them. */
      dest = poppler_document_find_dest
        (doc, action->goto_dest.dest->named_dest);
      UNDISCARD_STDOUT (saved_stdin);
      free_dest = TRUE;
    }
  else if (action->any.type == POPPLER_ACTION_NAMED)
      
    {
      DISCARD_STDOUT (saved_stdin);
      dest = poppler_document_find_dest (doc, action->named.named_dest);
      UNDISCARD_STDOUT (saved_stdin);
      free_dest = TRUE;
    }
  
  else if (action->any.type == POPPLER_ACTION_GOTO_REMOTE)
    {
      print_escaped (action->goto_remote.file_name, COLON);
      dest = action->goto_remote.dest;
    }
  else if (action->any.type == POPPLER_ACTION_GOTO_DEST)
    dest = action->goto_dest.dest;

  if (!dest
      || dest->type == POPPLER_DEST_UNKNOWN
      || dest->page_num < 1
      || dest->page_num > poppler_document_get_n_pages (doc))
    {
      printf (":");
      goto theend;
    }
  
  printf ("%d:", dest->page_num);

  if (action->type == POPPLER_ACTION_GOTO_REMOTE
      || NULL == (page = poppler_document_get_page (doc, dest->page_num - 1)))
    {
      goto theend;
    }

  poppler_page_get_size (page, &width, &height);
  g_object_unref (page);
  top = (height - dest->top) / height;

  /* adapted from xpdf */
  switch (dest->type)
    {
    case POPPLER_DEST_XYZ:
      if (dest->change_top)
        printf ("%f", top);
      break;
    case POPPLER_DEST_FIT:
    case POPPLER_DEST_FITB:
    case POPPLER_DEST_FITH:
    case POPPLER_DEST_FITBH:
      putchar ('0');
      break;
    case POPPLER_DEST_FITV:
    case POPPLER_DEST_FITBV:
    case POPPLER_DEST_FITR:
      printf ("%f", top);
      break;
    default: break;
    }

 theend:
  if (free_dest)
    poppler_dest_free (dest);
}


/* Annotations */

static gint
annots_cmp_edges (const annot_t *a1, const annot_t *a2)
{
  PopplerRectangle *e1 = &a1->amap->area;
  PopplerRectangle *e2 = &a2->amap->area;

  return (e1->y1 > e2->y1 ? -1
          : e1->y1 < e2->y1 ? 1
          : e1->x1 < e2->x1 ? -1
          : e1->x1 != e2->x1);
}

static GList*
get_annots_for_page (doc_t *doc, gint pn)
{

  GList *annot_list, *item;
  PopplerPage *page;
  gint i = 0;
  gint npages = poppler_document_get_n_pages (doc->pdf);

  if (pn < 1 || pn > npages)
    return NULL;

  if (! doc->annotations.pages)
    doc->annotations.pages = g_malloc0 (npages * sizeof(GList*));

  if (doc->annotations.pages[pn - 1])
    return doc->annotations.pages[pn - 1];
  
  if (! doc->annotations.keys)
    doc->annotations.keys = g_hash_table_new (g_str_hash, g_str_equal);

  page = poppler_document_get_page (doc->pdf, pn - 1);
  if (NULL == page)
    return NULL;
  
  annot_list = poppler_page_get_annot_mapping (page);
  for (item = annot_list; item; item = item->next)
    {
      PopplerAnnotMapping *map = (PopplerAnnotMapping *)item->data;
      gchar *key = g_strdup_printf ("annot-%d-%d", pn, i);
      annot_t *a = g_malloc (sizeof (annot_t));
      a->amap = map;
      a->key = key;
      doc->annotations.pages[pn - 1] =
        g_list_prepend (doc->annotations.pages[pn - 1], a);
      assert (NULL == g_hash_table_lookup (doc->annotations.keys, key));
      g_hash_table_insert (doc->annotations.keys, key, a);
      ++i;
    }
  g_list_free (annot_list);
  g_object_unref (page);
  return doc->annotations.pages[pn - 1];
}

static annot_t*
get_annot_by_key (doc_t *doc, const gchar *key)
{
  if (! doc->annotations.keys)
    return NULL;

  return g_hash_table_lookup (doc->annotations.keys, key);
}

static annot_t*
get_annot_by_key_or_error (doc_t *doc, const gchar *key)
{
  annot_t * a = get_annot_by_key (doc, key);
  if (! a)
      printf_error ("No such annotation: %s", key);
  return a;
}


static void
print_annot (const annot_t *annot, /* const */ PopplerPage *page)
{
  double width, height;
  PopplerAnnotMapping *m;
  const gchar *key;
  PopplerAnnot *a;
  PopplerAnnotMarkup *ma;
  PopplerAnnotText *ta;
  PopplerRectangle r;
  PopplerColor *color;
  gchar *text;
  gdouble opacity;

  if (! annot || ! page)
    return;
  
  m = annot->amap;
  key = annot->key;
  a = m->annot;
  poppler_page_get_size (page, &width, &height);
      
  r.x1 = m->area.x1;
  r.x2 = m->area.x2;
  r.y1 = height - m->area.y2;
  r.y2 = height - m->area.y1;

  /* >>> Simple Annotation >>> */
  /* Page */
  printf ("%d:", poppler_page_get_index (page) + 1);
  /* Area */
  printf ("%f %f %f %f:", r.x1 / width, r.y1 / height
          , r.x2 / width, r.y2 / height); 
      
  /* Type */
  printf ("%s:", poppler_annot_type_strings[poppler_annot_get_annot_type (a)]);
  /* Internal Key */
  print_escaped (key, COLON);

  /* Flags */
  printf ("%d:", poppler_annot_get_flags (a));

  /* Color */
  color = poppler_annot_get_color (a);
  if (color)
    {
      /* Reduce 2 Byte to 1 Byte color space  */
      printf ("#%.2x%.2x%.2x", (color->red >> 8)
              , (color->green >> 8)
              , (color->blue >> 8));
      g_free (color);
    }

  putchar (':');

  /* Text Contents */
  text = poppler_annot_get_contents (a);
  print_escaped (text, COLON);
  g_free (text);

  /* Modified Date */
  text = poppler_annot_get_modified (a);
  if (text)
    print_escaped (text, NONE);
  else
    print_escaped (text, NONE);
  g_free (text);

  /* <<< Simple Annotation <<< */

  /* >>> Markup Annotation >>> */
  if (! POPPLER_IS_ANNOT_MARKUP (a))
    {
      putchar ('\n');
      return;
    }

  putchar (':');
  ma = POPPLER_ANNOT_MARKUP (a);
  /* Label */
  text = poppler_annot_markup_get_label (ma);
  print_escaped (text, COLON);
  g_free (text);

  /* Subject */
  text = poppler_annot_markup_get_subject (ma);
  print_escaped (text, COLON);
  g_free (text);

  /* Opacity */
  opacity = poppler_annot_markup_get_opacity (ma);
  printf ("%f:", opacity);

  /* Popup (Area + isOpen) */
  if (poppler_annot_markup_has_popup (ma)
      && poppler_annot_markup_get_popup_rectangle (ma, &r))
    {
      gdouble tmp = r.y1;
      r.y1 = height - r.y2;
      r.y2 = height - tmp;
      printf ("%f %f %f %f:%d:", r.x1 / width, r.y1 / height
              , r.x2 / width, r.y2 / height
              , poppler_annot_markup_get_popup_is_open (ma) ? 1 : 0);
          
    }
  else
    printf ("::");

  /* Creation Date */
  text = poppler_annot_markup_get_created (ma);
  if (text)
    {
      print_escaped (text, NONE);
      g_free (text);
    }

  /* <<< Markup Annotation <<< */

  /* >>> Markup Text Annotation >>> */
  if (! POPPLER_IS_ANNOT_TEXT (a))
    {
      putchar ('\n');
      return;
    }
  putchar (':');
  ta = POPPLER_ANNOT_TEXT (a);
  /* Text Icon */
  text = poppler_annot_text_get_icon (ta);
  print_escaped (text, COLON);
  g_free (text);
  /* Text State */
  text = (gchar*)
    poppler_annot_text_state_strings
    [poppler_annot_text_get_state (ta)];
  printf ("%s:%d\n", text, poppler_annot_text_get_is_open (ta));
  /* <<< Markup Text Annotation <<< */
}

static void
print_attachment (PopplerAttachment *att, gboolean do_save)
{
  time_t time;
  
  print_escaped (att->name, COLON);
  print_escaped (att->description, COLON);
  if (att->size + 1 != 0)
    printf ("%" G_GSIZE_FORMAT ":", att->size);
  else
    printf ("-1:");
  time = (time_t) att->mtime;
  print_escaped (time > 0 ? strchomp (ctime (&time)) : "", COLON);
  time = (time_t) att->ctime;
  print_escaped (time > 0 ? strchomp (ctime (&time)) : "", COLON);
  print_escaped (att->checksum ? att->checksum->str : "" , COLON);
  if (do_save)
    {
      char *filename = mktempfile ();
      GError *error = NULL;
      if (filename)
        {
          if (! poppler_attachment_save (att, filename, &error))
            {
              fprintf (stderr, "Writing attachment failed: %s"
                       , error ? error->message : "reason unknown");
              if (error)
                g_free (error);
            }
          else
            {
              print_escaped (filename, NONE);
            }
          free (filename);
        }
    }
  putchar ('\n');
}


/* command implementations */

/* Name: features
   Args: None
   Returns: A list of compile-time features.
   Errors: None
*/
static void
cmd_features (const ctxt_t *ctx, const arg_t *args)
{
  const char *features[] = {
#ifdef HAVE_POPPLER_FIND_OPTS
    "case-sensitive-search",
#else
    "no-case-sensitive-search",
#endif
#ifdef HAVE_POPPLER_ANNOT_WRITE
    "write-annotations"
#else
    "no-write-annotations"
#endif
  };
  int i;
  OK_BEG ();
  for (i = 0; i < G_N_ELEMENTS (features); ++i)
    {
      printf ("%s", features[i]);
      if (i < G_N_ELEMENTS (features) - 1)
        putchar (':');
    }
  putchar ('\n');
  OK_END ();
}
  

/* Name: open
   Args: filename password
   Returns: Nothing
   Errors: If file can't be opened or is not a PDF document.
*/
static void
cmd_open (const ctxt_t *ctx, const arg_t *args)
{
  char *filename = args[0].value.string;
  char *passwd = args[1].value.string;
  GError *gerror = NULL;
  doc_t *doc;
  
  if (! *passwd)
    passwd = NULL;
  
  if (*filename != '/')
    {
      printf_error ("Filename must be absolute:%s", filename);
      return;
    }

  doc = open_document(ctx, filename, passwd, &gerror);
  if (NULL == doc)
    {
      printf_error ("Error opening %s:%s"
                    , filename, gerror ? gerror->message : "?");
      if (gerror)
        {
          g_error_free (gerror);
          gerror = NULL;
        }
    }
  else
    {
      doc->allow_auto_release = 0;
      OK ();
    }
}

/* Name: close
   Args: filename
   Returns: 1 if file was open, otherwise 0.
   Errors: None
*/
static void
cmd_close (const ctxt_t *ctx, const arg_t *args)
{
  doc_t *doc = g_hash_table_lookup(ctx->documents, args->value.string);
  OK_BEG ();
  puts (doc ? "1" : "0");
  g_hash_table_remove (ctx->documents, args->value.string);
  free_doc (doc);
  OK_END ();
}

/* Name: closeall
   Args: None
   Returns: Nothing
   Errors: None
*/
static void
cmd_closeall (const ctxt_t *ctx, const arg_t *args)
{
  release_documents (ctx, TRUE);
  OK ();
}
/* Name: search
   Args: filename ignorecase firstpage lastpage searchstring
   
   firstpage and lastpage may be 0, in which they are taken to be the
   first, respc. last, page of the document.
   
   Returns: A list of matches:
   page edges matched-text
   Errors:  None
*/

static void
cmd_search(const ctxt_t *ctx, const arg_t *args)
{
  PopplerDocument *doc = args[0].value.doc->pdf;
  gboolean ignore_case = args[1].value.flag;
  int first = args[2].value.natnum;
  int last = args[3].value.natnum;
  char *string = args[4].value.string;
  GList *list, *item;
  double width, height;
  int pn;
#ifdef HAVE_POPPLER_FIND_OPTS
  PopplerFindFlags flags = ignore_case ? 0 : POPPLER_FIND_CASE_SENSITIVE;
#endif

  first = MAX(1, first);
  if (last <= 0)
    last = poppler_document_get_n_pages (doc);
  else
    last = MIN(last, poppler_document_get_n_pages (doc));

  OK_BEG ();
  for (pn = first; pn <= last; ++pn)
    {
      PopplerPage *page = poppler_document_get_page(doc, pn - 1);

      if (! page)
        continue;

#ifdef HAVE_POPPLER_FIND_OPTS
      list = poppler_page_find_text_with_options(page, string, flags);
#else
      list = poppler_page_find_text(page, string);
#endif

      poppler_page_get_size (page, &width, &height);

      for (item = list; item; item = item->next)
        {
          gchar *line;
          PopplerRectangle *r = item->data;
          gdouble y1 =  r->y1;

          r->y1 = height - r->y2;
          r->y2 = height - y1;

          printf ("%d:%f %f %f %f:", pn
                  , r->x1 / width, r->y1 / height
                  , r->x2 / width, r->y2 / height);
          line = strchomp (poppler_page_get_selected_text
                           (page, POPPLER_SELECTION_LINE, r));
          print_escaped (line, NL);
          g_free (line);
          poppler_rectangle_free (r);
        }
      g_list_free (list);
      g_object_unref (page);
    }
  OK_END ();
}

/* Name: metadata
   Args: filename
   Returns: PDF's metadata
   Errors: None
   
   title author subject keywords creator producer pdf-version create-date mod-date

   Dates are in seconds since the epoche.
   
*/

static void
cmd_metadata (const ctxt_t *ctx, const arg_t *args)
{
  PopplerDocument *doc = args[0].value.doc->pdf;
  time_t date;
  gchar *md[6];
  gchar *title;
  int i;
  char *time_str;

  OK_BEG ();
  
  title = poppler_document_get_title (doc);
  print_escaped (title, COLON);
  g_free (title);

  md[0] = poppler_document_get_author (doc);
  md[1] = poppler_document_get_subject (doc);
  md[2] = poppler_document_get_keywords (doc);
  md[3] = poppler_document_get_creator (doc);
  md[4] = poppler_document_get_producer (doc);
  md[5] = poppler_document_get_pdf_version_string (doc);
  
  for (i = 0; i < 6; ++i)
    {
      print_escaped (md[i], COLON);
      g_free (md[i]);
    }

  date = poppler_document_get_creation_date (doc);
  time_str = strchomp (ctime (&date));
  print_escaped (time_str ? time_str : "", COLON);
  date = poppler_document_get_modification_date (doc);
  time_str = strchomp (ctime (&date));
  print_escaped (time_str ? time_str : "", NL);
  OK_END ();
}

static void
cmd_outline_walk (PopplerDocument *doc, PopplerIndexIter *iter, int depth)
{
  do
    {
      PopplerIndexIter *child;
      PopplerAction *action = poppler_index_iter_get_action (iter);

      if (! action)
        continue; 
      
      if (is_handled_action (action))
        {
          printf ("%d:", depth);
          print_action (doc, action);
        }

      child = poppler_index_iter_get_child (iter);
      if (child)
        {
          cmd_outline_walk (doc, child, depth + 1);
        }
      poppler_action_free (action);
      poppler_index_iter_free (child);
    } while (poppler_index_iter_next (iter));
}

/* Name: outline
   Args: filename

   Returns: The documents outline (or index) as a, possibly empty,
   list of records:

   tree-level ACTION

   See cmd_pagelinks for how ACTION is constructed.  
   
   Errors: None
*/

static void
cmd_outline (const ctxt_t *ctx, const arg_t *args)
{
  PopplerIndexIter *iter = poppler_index_iter_new (args->value.doc->pdf);
  OK_BEG ();
  if (iter)
    {
      cmd_outline_walk (args->value.doc->pdf, iter, 1);
      poppler_index_iter_free (iter);
    }
  OK_END ();
}

/* Name: quit
   Args: None
   Returns: Nothing
   Errors: None

   Close all documents and exit.
*/

static void
cmd_quit (const ctxt_t *ctx, const arg_t *args)
{
  release_documents (ctx, TRUE);
  OK ();
  exit (EXIT_SUCCESS);
}

/* Name: number-of-pages
   Args: filename
   Returns: The number of pages.
   Errors: None
*/

static void
cmd_npages (const ctxt_t *ctx, const arg_t *args)
{
  int npages = poppler_document_get_n_pages (args->value.doc->pdf);
  OK_BEG ();
  printf ("%d\n", npages);
  OK_END ();
}

/* Name: pagelinks
   Args: filename page
   Returns: A list of linkmaps:

   edges ACTION ,

   where ACTION is one of

   'goto-dest' title page top
   'goto-remote' title filename page top
   'uri' title URI
   'launch' title program arguments

   top is desired vertical position, filename is the target PDF of the
   `goto-remote' link.
   
   Errors: None
*/

static void
cmd_pagelinks(const ctxt_t *ctx, const arg_t *args)
{
  PopplerDocument *doc = args[0].value.doc->pdf;
  PopplerPage *page;
  int pn = args[1].value.natnum;
  double width, height;
  GList *link_map, *item;

  if (pn < 1 || pn > poppler_document_get_n_pages (doc))
    {
      printf_error ("No such page %d", pn);
      return;
    }

  page = poppler_document_get_page (doc, pn - 1);
  poppler_page_get_size (page, &width, &height);
  link_map = poppler_page_get_link_mapping (page);
  
  OK_BEG ();
  for (item = g_list_last (link_map); item; item = item->prev)
    {

      PopplerLinkMapping *link = item->data;
      PopplerRectangle *r = &link->area;
      double x = r->y1;
      r->y1 = height - r->y2;
      r->y2 = height - x;
        
      printf ("%f %f %f %f:"
              , r->x1 / width, r->y1 / height
              , r->x2 / width, r->y2 / height);
      print_action (doc, link->action);
    }
  OK_END ();
  g_object_unref (page);
  poppler_page_free_link_mapping (link_map);
}

/* Name: gettext
   Args: filename page edges selection-style
   Returns: The selection's text.
   Errors: If page is out of range.

   For the selection-style argument see getselection command.
*/

static void
cmd_gettext(const ctxt_t *ctx, const arg_t *args)
{
  PopplerDocument *doc = args[0].value.doc->pdf;
  int pn = args[1].value.natnum;
  double x1 = args[2].value.edge; 
  double y1 = args[3].value.edge;
  double x2 = args[4].value.edge;
  double y2 = args[5].value.edge;
  int selection_style = args[5].value.natnum;
  PopplerPage *page;
  double width, height;
  gchar *text;
  PopplerRectangle r;
  
  switch (selection_style)
    {
    case POPPLER_SELECTION_GLYPH: break;
    case POPPLER_SELECTION_LINE: break;
    case POPPLER_SELECTION_WORD: break;
    default: selection_style = POPPLER_SELECTION_GLYPH;
    }

  if (pn <= 0 || pn > poppler_document_get_n_pages (doc))
    {
      printf_error ("No such page %d", pn);
      return;
    }

  page = poppler_document_get_page (doc, pn - 1);
  poppler_page_get_size (page, &width, &height);
  r.x1 = x1 * width;
  r.x2 = x2 * width;
  r.y1 = y1 * height;
  r.y2 = y2 * height;
  /* printf ("%f %f %f %f , %f %f\n", r.x1, r.y1, r.x2, r.y2, width, height); */
  text = poppler_page_get_selected_text (page, selection_style, &r);

  OK_BEG ();
  print_escaped (text, NL);
  OK_END ();

  g_free (text);
  g_object_unref (page);
}

/* Name: getselection
   Args: filename page edges selection-selection_style
   Returns: The selection's text.
   Errors: If page is out of range.

   selection-selection_style should be as follows.

   0 (POPPLER_SELECTION_GLYPH)
	glyph is the minimum unit for selection

   1 (POPPLER_SELECTION_WORD)
	word is the minimum unit for selection

   2 (POPPLER_SELECTION_LINE)
	line is the minimum unit for selection 
*/

static void
cmd_getselection (const ctxt_t *ctx, const arg_t *args)
{
  PopplerDocument *doc = args[0].value.doc->pdf;
  int pn = args[1].value.natnum;
  double x1 = args[2].value.edge; 
  double y1 = args[3].value.edge;
  double x2 = args[4].value.edge;
  double y2 = args[5].value.edge;
  int selection_style = args[5].value.natnum;
  
  cairo_region_t *region;
  int i;
  PopplerPage *page;
  double width, height;
  PopplerRectangle r;
  
  switch (selection_style)
    {
    case POPPLER_SELECTION_GLYPH: break;
    case POPPLER_SELECTION_LINE: break;
    case POPPLER_SELECTION_WORD: break;
    default: selection_style = POPPLER_SELECTION_GLYPH;
    }
  
  if (pn <= 0 || pn > poppler_document_get_n_pages (doc))
    {
      printf_error ("No such page %d", pn);
      return;
    }
  page = poppler_document_get_page (doc, pn - 1);
  poppler_page_get_size (page, &width, &height);
  r.x1 = x1 * width;
  r.x2 = x2 * width;
  r.y1 = y1 * height;
  r.y2 = y2 * height;

  region = poppler_page_get_selected_region (page, 1.0, POPPLER_SELECTION_GLYPH, &r);

  OK_BEG ();
  for (i = 0; i < cairo_region_num_rectangles (region); ++i)
    {
      cairo_rectangle_int_t r;

      cairo_region_get_rectangle (region, i, &r);
      printf ("%f %f %f %f\n",
              r.x / width,
              r.y / height,
              (r.x + r.width) / width,
              (r.y + r.height) / height);
    }
  OK_END ();

  cairo_region_destroy (region);
  g_object_unref (page);
}

/* Name: pagesize
   Args: filename page
   Returns: width height
   Errors: If page is out of range.
*/

static void
cmd_pagesize(const ctxt_t *ctx, const arg_t *args)
{
  PopplerDocument *doc = args[0].value.doc->pdf;
  int pn = args[1].value.natnum;
  PopplerPage *page;
  double width, height;
  
  if (pn < 1 || pn > poppler_document_get_n_pages (doc))
    {
      printf_error ("No such page %d", pn);
      return;
    }
  
  page = poppler_document_get_page (doc, pn - 1);
  poppler_page_get_size (page, &width, &height);
  g_object_unref (page);
  
  OK_BEG ();
  printf ("%f:%f\n", width, height);
  OK_END ();
}

/* Annotations */

/* Name: getannots
   Args: filename firstpage lastpage
   Returns: The list of annotations of this page.

   For all annotations
   
   page edges type key flags color contents mod-date

   ,where
       
   name is a document-unique name,
   flags is PopplerAnnotFlag bitmask,
   color is 3-byte RGB hex number and

   Then

   label subject opacity popup-edges popup-is-open create-date 

   if this is a markup annotation and additionally

   text-icon text-state

   for markup text annotations.
   
   Errors: If page is out of range.
*/

static void
cmd_getannots(const ctxt_t *ctx, const arg_t *args)
{
  PopplerDocument *doc = args[0].value.doc->pdf;
  gint first = args[1].value.natnum;
  gint last = args[2].value.natnum;
  GList *list;
  gint pn;
  
  first = MAX(1, first);
  if (last <= 0)
    last = poppler_document_get_n_pages (doc);
  else
    last = MIN(last, poppler_document_get_n_pages (doc));

  OK_BEG ();
  for (pn = first; pn <= last; ++pn)
    {
      GList *annots = get_annots_for_page (args->value.doc, pn);
      PopplerPage *page = poppler_document_get_page (doc, pn - 1);
      if (! page)
        continue;

      for (list = annots; list; list = list->next)
        {
          annot_t *annot = (annot_t *)list->data;
          print_annot (annot, page);
        }
      g_object_unref (page);
    }
  OK_END ();
}

/* Name: getannot
   Args: filename name
   Returns: The annotation for name, see cmd_getannots.
   Errors: If no annotation named ,name' exists.
*/

static void
cmd_getannot (const ctxt_t *ctx, const arg_t *args)
{
  doc_t *doc = args->value.doc;
  const gchar *key = args[1].value.string;
  PopplerPage *page = NULL;
  annot_t *a = get_annot_by_key (doc, key);
  gint index;
  if (! a)
    {
      printf_error ("No such annotation: %s", key);
      return;
    }

  index = poppler_annot_get_page_index (a->amap->annot);
  if (index >= 0)
    page = poppler_document_get_page (doc->pdf, index);
  if (! page)
    {
      printf_error("Unable to get page: %d", index + 1);
      return;
    }
  OK_BEG ();
  print_annot (a, page);
  OK_END ();
  g_object_unref (page);
}

/* Name: getannot_attachment
   Args: filename name [output-filename]
   Returns: name description size mtime ctime output-filename
   Errors: If no annotation named ,name' exists or output-filename is
   not writable.
*/

static void
cmd_getattachment_from_annot (const ctxt_t *ctx, const arg_t *args)
{
  doc_t *doc = args->value.doc;
  const gchar *key = args[1].value.string;
  gboolean do_save = args[2].value.flag;
  PopplerAttachment *att;
  annot_t *a = get_annot_by_key (doc, key);

  if (! a)
    {
      printf_error ("No such annotation: %s", key);
      return;
    }
  if (! POPPLER_IS_ANNOT_FILE_ATTACHMENT (a->amap->annot))
    {
      printf_error ("Not a file annotation: %s", key);
      return;
    }
  att = poppler_annot_file_attachment_get_attachment
    (POPPLER_ANNOT_FILE_ATTACHMENT (a->amap->annot));
  if (! att)
    {
      printf_error ("Unable to get attachment: %s", key);
      return;
    }
  
  OK_BEG ();
  print_attachment (att, do_save);
  g_object_unref (att);
  OK_END ();
}

static void
cmd_getattachments (const ctxt_t *ctx, const arg_t *args)
{
  doc_t *doc = args->value.doc;
  gboolean do_save = args[1].value.flag;
  GList *item;
  GList *attmnts = poppler_document_get_attachments (doc->pdf);
  
  OK_BEG ();
  for (item = attmnts; item; item = item->next)
    {
      PopplerAttachment *att = (PopplerAttachment*) item->data;
      print_attachment (att, do_save);
      g_object_unref (att);
    }
  g_list_free (attmnts);

  OK_END ();
}

#ifdef HAVE_POPPLER_ANNOT_WRITE
/* Name: addannot
   Args: filename page edges
   Returns: The new annotation.
   Errors: If page does not exist.
*/

static void
cmd_addannot (const ctxt_t *ctx, const arg_t *args)
{
  
  doc_t *doc = args->value.doc;
  gint pn = args[1].value.natnum;
  double x1 = args[2].value.edge; 
  double y1 = args[3].value.edge;
  double x2 = args[4].value.edge;
  double y2 = args[5].value.edge;
  int i;
  PopplerPage *page;
  double width, height;
  PopplerAnnot *pannot;
  PopplerRectangle rect;
  PopplerAnnotMapping *amap;
  annot_t *annot;
  gchar *key;
  GList *annots;

  if (pn < 0 || pn > poppler_document_get_n_pages (doc->pdf))
    {
      printf_error ("No such page %d", pn);
      return;
    }
  page = poppler_document_get_page (doc->pdf, pn - 1);
  poppler_page_get_size (page, &width, &height);
  rect.x1 = x1 * width;
  rect.x2 = x2 * width;
  rect.y1 = height - (y2 * height);
  rect.y2 = height - (y1 * height);

  pannot = poppler_annot_text_new (doc->pdf, &rect);
  amap = poppler_annot_mapping_new ();
  amap->area = rect;
  amap->annot = pannot;
  annots = get_annots_for_page (doc, pn);

  i = g_list_length (annots);
  key = g_strdup_printf ("annot-%d-%d", pn, i);
  while (g_hash_table_lookup (doc->annotations.keys, key))
    {
      g_free (key);
      key = g_strdup_printf ("annot-%d-%d", pn, ++i);
    }
  annot = g_malloc (sizeof (annot_t));
  annot->amap = amap;
  annot->key = key;
  doc->annotations.pages[pn - 1] =
    g_list_prepend (annots, annot);
  g_hash_table_insert (doc->annotations.keys, key, annot);
  poppler_page_add_annot (page, pannot);
  OK_BEG ();
  print_annot (annot, page);
  OK_END ();
  g_object_unref (page);
}

static void
cmd_delannot (const ctxt_t *ctx, const arg_t *args)
{
  doc_t *doc = args->value.doc;
  const gchar *key = args[1].value.string;
  PopplerPage *page = NULL;
  annot_t *annot = get_annot_by_key (doc, key);
  gint pn;
  if (! annot)
    {
      printf_error ("No such annotation: %s", key);
      return;
    }

  pn = poppler_annot_get_page_index (annot->amap->annot) + 1;
  if (pn >= 1)
    page = poppler_document_get_page (doc->pdf, pn - 1);
  if (! page)
    {
      printf_error("Unable to get page: %d", pn);
      return;
    }
  poppler_page_remove_annot (page, annot->amap->annot);
  doc->annotations.pages[pn - 1] =
    g_list_remove (doc->annotations.pages[pn - 1], annot);
  g_hash_table_remove (doc->annotations.keys, annot->key);
  poppler_annot_mapping_free(annot->amap);
  g_free (annot->key);
  g_free (annot);
  g_object_unref (page);
  OK ();
}

static void
cmd_save (const ctxt_t *ctx, const arg_t *args)
{
  doc_t *doc = args->value.doc;
  char *filename = mktempfile ();
  GError *gerror = NULL;
  gchar *uri;
  gboolean success = FALSE;
  
  if (!filename)
    {
      printf_error ("Unable to create temporary file");
      return;
    }
      
  uri = g_filename_to_uri (filename, NULL, &gerror);

  if (uri)
    {
      success = poppler_document_save (doc->pdf, uri, &gerror);
      g_free (uri);
    }
  if (! success)
    {
      printf_error ("Error while saving %s:%s"
                    , filename, gerror ? gerror->message : "?");
      if (gerror)
        g_error_free (gerror);
      return;
    }
  OK_BEG ();
  print_escaped (filename, NL);
  OK_END ();
}

static void
cmd_editannot (const ctxt_t *ctx, const arg_t *args)
{
  doc_t *doc = args->value.doc;
  char *key = args[1].value.string;
  annot_t *annot = get_annot_by_key_or_error (doc, key);
  char *setmask = args[2].value.string;
  guint r,g,b;                  /* Colors */
  PopplerAnnot *pannot;
  gint index;
  PopplerPage *page;
  
  if (! annot)
    return;

  pannot = annot->amap->annot;
  /* 0. Edges 3-6
     1. Color 7
     2. Contents 8
     3. Label 9 
     4. Is-Open 10
     5. Icon 11
  */
  
  if (strlen (setmask) != 6)
    {
      printf_error ("Set value mask mismatch: %s", setmask);
      return;
    }

  /* Don't set anything, if some value is invalid. */

  if (setmask[1] != '0')
    {
      char *color = args[7].value.string;
      if (! strlen (color) == 7
          || 3 != sscanf (color, "#%2x%2x%2x", &r, &g, &b))
        {
          printf_error ("Invalid color: %s", color);
          return;
        }
    }
  if (setmask[3] != '0' && ! POPPLER_IS_ANNOT_MARKUP (pannot))
    {
      printf_error ("Not a markup annotation: %s", key);
      return;
    }
  if (setmask[4] != '0'
      && ! POPPLER_IS_ANNOT_TEXT (pannot))
    {
      printf_error ("Not a text annotation: %s", key);
      return;
    }
  if (setmask[5] != '0' && ! POPPLER_IS_ANNOT_TEXT (pannot))
    {
      printf_error ("Not a text annotation: %s", key);
      return;
    }

  /* Now set the new values. */
  if (setmask[0] != '0')
    {
      PopplerRectangle *area = &annot->amap->area;
      index = poppler_annot_get_page_index (pannot);
      page = poppler_document_get_page (doc->pdf, index);
      double width, height, x1, y1, x2, y2;

      poppler_page_get_size (page, &width, &height);

      x1 = args[3].value.edge; 
      y1 = args[4].value.edge;
      x2 = args[5].value.edge;
      y2 = args[6].value.edge;

      /* Translate Gravity and maybe keep the width and height. */
      if (x2 < 0)
        area->x2 +=  (x1 * width) - area->x1;
      else
        area->x2 = x2 * width;

      if (y2 < 0)
        area->y1 -=  (y1 * height) - (height - area->y2);
      else
        area->y1 = height - (y2 * height);

      area->x1 = x1 * width;
      area->y2 = height - (y1 * height);

      poppler_annot_set_rectangle (pannot, area);
      g_object_unref (page);
    }

  if (setmask[1] != '0')
    {
      PopplerColor pcolor;

      pcolor.red = r << 8;
      pcolor.green = g << 8;
      pcolor.blue = b << 8;
      poppler_annot_set_color (pannot, &pcolor);
    }

  if (setmask[2] != '0')
    {
      char *contents = args[8].value.string;
      poppler_annot_set_contents (pannot, contents);
    }

  if (setmask[3] != '0')
    {
      char *label = args[9].value.string;
      PopplerAnnotMarkup *ma = POPPLER_ANNOT_MARKUP (pannot);
      poppler_annot_markup_set_label (ma, label);
    }

  if (setmask[4] != '0')
    {
      int open = args[10].value.natnum;
      PopplerAnnotText *ta = POPPLER_ANNOT_TEXT (pannot);
      poppler_annot_text_set_is_open (ta, open);
    }

  if (setmask[5] != '0')
    {
      char *icon = args[11].value.string;
      PopplerAnnotText *ta;
      ta = POPPLER_ANNOT_TEXT (pannot);
      poppler_annot_text_set_icon (ta, icon);
    }

  index = poppler_annot_get_page_index (pannot);
  page = poppler_document_get_page (doc->pdf, index);
  OK_BEG ();
  print_annot (annot, page);
  OK_END ();
  g_object_unref (page);
}

#endif  /* HAVE_POPPLER_ANNOT_WRITE */

static void
cmd_synctex_forward_search (const ctxt_t *ctx, const arg_t *args)
{
  doc_t *doc = args[0].value.doc;
  const char *source = args[1].value.string;
  int line = args[2].value.natnum;
  int column = args[3].value.natnum;
  synctex_scanner_t scanner =
    synctex_scanner_new_with_output_file (doc->filename, NULL, 1);
  synctex_node_t node;  

  if (!scanner)
    {
      printf_error ("Unable to create synctex scanner,\
 did you run latex with `--synctex=1' ?");
      return;
    }

  if (! synctex_display_query (scanner, source, line, column)
      || ! (node = synctex_next_result (scanner)))
    {
      printf_error ("Destination not found");
      goto theend;
    }

  {
    int pn = synctex_node_page (node);
    PopplerPage *page = poppler_document_get_page(doc->pdf, pn - 1);
    float x1 =  synctex_node_box_visible_h (node);
    float y1 =  synctex_node_box_visible_v (node)
      - synctex_node_box_visible_height (node);
    float x2 = synctex_node_box_visible_width (node) + x1;
    float y2 = synctex_node_box_visible_depth (node)
      + synctex_node_box_visible_height (node) + y1;
    double width, height;
    
    if (! page)
      {
        printf_error ("Destination not found");
        goto theend;
      }
    poppler_page_get_size (page, &width, &height);
    x1 /= width;
    y1 /= height;
    x2 /= width;
    y2 /= height;
    
    OK_BEG ();
    printf("%d:%f:%f:%f:%f\n", pn, x1, y1, x2, y2);
    OK_END ();
    g_object_unref (page);
  }
 theend:
  synctex_scanner_free (scanner);
}

static void
cmd_synctex_backward_search (const ctxt_t *ctx, const arg_t *args)
{
  doc_t *doc = args[0].value.doc;
  int pn = args[1].value.natnum;
  double x = args[2].value.edge;
  double y = args[3].value.edge;
  synctex_scanner_t scanner =
    synctex_scanner_new_with_output_file (doc->filename, NULL, 1);
  const char *filename;
  PopplerPage *page;
  synctex_node_t node;  
  double width, height;
  int line, column;

  if (!scanner)
    {
     printf_error ("Unable to create synctex scanner,\
 did you run latex with `--synctex=1' ?");
      return;
    }

  page = poppler_document_get_page(doc->pdf, pn - 1);
  if (! page)
    {
      printf_error ("Destination not found");
      goto free_scanner;
    }
  poppler_page_get_size (page, &width, &height);
  x = x * width;
  y = y * height;

  if (! synctex_edit_query (scanner, pn, x, y)
      || ! (node = synctex_next_result (scanner))
      || ! (filename =
            synctex_scanner_get_name (scanner, synctex_node_tag (node))))
    {
      printf_error ("Destination not found");
      goto free_page_and_scanner;
    }

  line = synctex_node_line (node);
  column = synctex_node_column (node);
  OK_BEG ();
  printf("%s:%d:%d\n", filename, line, column);
  OK_END ();

 free_page_and_scanner:
  g_object_unref (page);
 free_scanner:
  synctex_scanner_free (scanner);
}



/* utility functions */

static void
free_args (arg_t *args, size_t n)
{
  int i;
  if (! args)
    return;
  
  for (i = 0; i < n; ++i)
    {
      switch (args[i].type)
        {
        case ARG_STRING:
        case ARG_STRING_NONEMPTY:
          g_free(args[i].value.string);
          break;
        default: break;
        }
    }
  g_free (args);
}

static void
free_doc (doc_t *doc)
{
  if (! doc)
    return;
  g_free (doc->filename);
  g_free (doc->passwd);
  if (doc->annotations.pages)
    {
      int npages = poppler_document_get_n_pages (doc->pdf);
      int i;
      for (i = 0; i < npages; ++i)
        {
          GList *item;
          GList *annots  = doc->annotations.pages[i];
          for (item = annots; item; item = item->next)
            {
              annot_t *a = (annot_t*) item->data;
              poppler_annot_mapping_free(a->amap);
              g_free (a->key);
              g_free (a);
            }
          g_list_free (annots);
        }
      g_hash_table_destroy (doc->annotations.keys);
      g_free (doc->annotations.pages);
    }
  g_object_unref (doc->pdf);
  g_free (doc);
}

static char*
strchomp (char *str)
{
  if (! str || ! *str)
    return str;
  
  size_t len = strlen (str);
  if (str[len - 1] == '\n')
    str[len - 1] = '\0';
  return str;
}

static char*
mktempfile()
{        
  char *filename = NULL;
  int tries = 3;
  while (! filename && tries-- > 0)
    {
      filename =  tempnam(NULL, "epdfinfo");
      if (filename)
        {
          int fd = open(filename, O_CREAT | O_EXCL | O_RDONLY, S_IRWXU);
          if (fd > 0)
            close (fd);
          else
            {
              free (filename);
              filename = NULL;
            }
        }
    }
  if (! filename)
    fprintf (stderr, "Unable to create tempfile");

  return filename;
}
