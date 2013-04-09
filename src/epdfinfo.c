// Copyright (C) 2013  Andreas Politz

// Author: Andreas Politz <politza@fh-trier.de>

// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

#include "config.h"
#include <assert.h>
#include <err.h>
#include <error.h>
#include <glib.h>
#include <poppler.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <sys/types.h>
#include "epdfinfo.h"


/* declarations */
static arg_t *parse_args(const ctxt_t *ctx, const char *args, size_t len,
                         const cmd_t *cmd);
static ptrdiff_t parse_next_arg (const char *args, char *buf);
static void free_args (arg_t *args, size_t n);
static void free_doc (doc_t *doc);
static doc_t *open_document (const ctxt_t *ctx, const char *filename, const char *passwd, GError **error);
static size_t release_documents (const ctxt_t *ctx, gboolean all);
static char* strchomp (char *str);
static void print_escaped(const char *str, gboolean last_arg);
static void printf_error(const char *fmt, ...);
static void
print_action_dest (PopplerDocument *doc, PopplerAction *action);
static void
print_action (PopplerDocument *doc, PopplerAction *action);
  
static void cmd_open (const ctxt_t *ctx, const arg_t *args);
static void cmd_close (const ctxt_t *ctx, const arg_t *args);
static void cmd_closeall (const ctxt_t *ctx, const arg_t *args);
static void cmd_search (const ctxt_t *ctx, const arg_t *args);
static void
cmd_search_regex(PopplerDocument *doc, const char *regex,
                 int first, int last, gboolean ignore_case);
static void
cmd_search_string(PopplerDocument *doc, const char *string,
                  int first, int last, gboolean ignore_case);
#ifdef HAVE_POPPLER_INDEX_ITER
static void
cmd_outline_walk (PopplerDocument *doc, PopplerIndexIter *iter, int depth);
#endif
static void cmd_outline (const ctxt_t *ctx, const arg_t *args);
static void cmd_metadata (const ctxt_t *ctx, const arg_t *args);
static void cmd_quit (const ctxt_t *ctx, const arg_t *args);
static void cmd_npages (const ctxt_t *ctx, const arg_t *args);
static void cmd_pagelinks(const ctxt_t *ctx, const arg_t *args);
static void cmd_gettext(const ctxt_t *ctx, const arg_t *args);
static void cmd_pagesize(const ctxt_t *ctx, const arg_t *args);


/* command specs */
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
    ARG_EDGE                    /* y1 */
  };

const args_spec_t cmd_pagesize_spec[] =
  {
    ARG_DOC,
    ARG_NATNUM                  /* page number */
  };


static const cmd_t cmds [] =
  {
    {"open", cmd_open, cmd_open_spec, G_N_ELEMENTS (cmd_open_spec)},
    {"close", cmd_close, cmd_close_spec, G_N_ELEMENTS (cmd_close_spec)},
    {"search", cmd_search, cmd_search_spec, G_N_ELEMENTS (cmd_search_spec)},
    {"metadata", cmd_metadata, cmd_metadata_spec, G_N_ELEMENTS (cmd_metadata_spec)},
    {"outline", cmd_outline, cmd_outline_spec, G_N_ELEMENTS (cmd_outline_spec)},
    {"quit", cmd_quit, cmd_quit_spec, G_N_ELEMENTS (cmd_quit_spec)},
    {"number-of-pages", cmd_npages, cmd_npages_spec, G_N_ELEMENTS (cmd_npages_spec)},
    {"pagelinks", cmd_pagelinks, cmd_pagelinks_spec, G_N_ELEMENTS (cmd_pagelinks_spec)},
    {"gettext", cmd_gettext, cmd_gettext_spec, G_N_ELEMENTS (cmd_gettext_spec)},
    {"pagesize", cmd_pagesize, cmd_pagesize_spec, G_N_ELEMENTS (cmd_pagesize_spec)}
  };

static const char *poppler_action_type_strings[] =
  {
    "unknown",
    "none",
    "goto-dest",
    "goto-remote",
    "launch",
    "uri",
    "goto-dest", //"named", 
    "movie",
    "rendition",
    "ocg-state",
    "javascript"
  };

void err_handler (const gchar *err)
{
  /* FIXME: Handle glib errors. */
}

int main(int argc, char **argv)
{
  ctxt_t ctx;
  char line[IN_BUF_LEN];
  size_t len = 0;
  ssize_t read;

  if (argc != 1)
    {
      fprintf(stderr, "usage: epdfinfo\n");
      exit (EXIT_FAILURE);
    }

  g_type_init ();
  g_set_printerr_handler (err_handler);
                          
  ctx.documents = g_hash_table_new (g_str_hash, g_str_equal);

  while (fgets (line, IN_BUF_LEN, stdin))
    {
      size_t cmd_end;
      arg_t *cmd_args;
      int i;

      read = strlen (line);
      if (read <= 1)
        continue;
      else if (line[read - 1] != '\n')
        {
          printf_error ("Input line is to long (max %d bytes)", IN_BUF_LEN);
          continue;
        }
        
      line[read - 1] = '\0';
      cmd_end = strcspn (line, ":");

      for (i = 0; i < G_N_ELEMENTS (cmds);  i++)
        {
          if (strncmp (cmds[i].name, line, strlen (cmds[i].name)) == 0)
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
      release_documents (&ctx, FALSE);
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
  const args_spec_t *spec = cmd->args_spec;
  arg_t *cmd_args = g_malloc0 (cmd->nargs * sizeof (arg_t));
  
  for (i = 0; i < cmd->nargs && !failure; ++i)
    {
      if (! (read = parse_next_arg (args, buf)))
        {
          printf_error ("Command expects %d argument(s), %d given", cmd->nargs, i);
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
        case ARG_EDGE:
          {
            char *endptr;
            double n = strtod (buf, &endptr);
            if (*endptr || (n < 0.0) || n > 1.0)
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
      if (doc->last_used)
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
      if (all || (doc->last_used && doc->last_used + RELEASE_DOC_TIMEOUT < now))
        {
          free_doc (doc);
          g_hash_table_iter_remove (&iter);
          ++removed;
        }
    }
  return removed;
}
             
static void
print_escaped (const char *str, gboolean last_arg)
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

  if (last_arg)
    putchar ('\n');
  else
    putchar (':');
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

static gboolean
do_handle_action (PopplerAction *action)
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
    }
  return FALSE;
}

static void
print_action (PopplerDocument *doc, PopplerAction *action)
{
  if (! do_handle_action (action))
    return;

  print_escaped (poppler_action_type_strings[action->any.type], FALSE);
  print_escaped (action->any.title, FALSE);
  switch (action->any.type)
    {
    case POPPLER_ACTION_GOTO_REMOTE:
    case POPPLER_ACTION_GOTO_DEST:
    case POPPLER_ACTION_NAMED:
      print_action_dest (doc, action);
      putchar ('\n');
      break;
    case POPPLER_ACTION_LAUNCH:
      print_escaped (action->launch.file_name, FALSE);
      print_escaped (action->launch.params, TRUE);
      break;
    case POPPLER_ACTION_URI:
      print_escaped (action->uri.uri, TRUE);
      break;
    }
}

static void
print_action_dest (PopplerDocument *doc, PopplerAction *action)
{
  PopplerDest *dest = NULL;
  gboolean free_dest = FALSE;
  double width, height, top, left;
  PopplerPage *page;
    
  if (action->any.type == POPPLER_ACTION_GOTO_DEST
      && action->goto_dest.dest->type == POPPLER_DEST_NAMED)
    {
      dest = poppler_document_find_dest
        (doc, action->goto_dest.dest->named_dest);
      free_dest = TRUE;
    }
  else if (action->any.type == POPPLER_ACTION_NAMED)
      
    {
      dest = poppler_document_find_dest (doc, action->named.named_dest);
      free_dest = TRUE;
    }
  
  else if (action->any.type == POPPLER_ACTION_GOTO_REMOTE)
    {
      print_escaped (action->goto_remote.file_name, FALSE);
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
  left = dest->left / width;

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
    }

 theend:
  if (free_dest)
    poppler_dest_free (dest);
}


/* command implementations */

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
      doc->last_used = 0;
      OK ();
    }
}

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

static void
cmd_closeall (const ctxt_t *ctx, const arg_t *args)
{
  release_documents (ctx, TRUE);
  OK ();
}

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
#ifdef HAVE_POPPLER_GET_TEXT
          line = strchomp (poppler_page_get_selected_text
                           (page, POPPLER_SELECTION_LINE, r));
          print_escaped (line, TRUE);
          g_free (line);
#else
          putchar ('\n');
#endif
          poppler_rectangle_free (r);
        }
      g_list_free (list);
      g_object_unref (page);
    }
  OK_END ();
}


static void
cmd_metadata (const ctxt_t *ctx, const arg_t *args)
{
#ifdef HAVE_POPPLER_GET_TITLE
  PopplerDocument *doc = args[0].value.doc->pdf;
  time_t date;
  guint minor = 0, major = 0;
  gchar *md[6];
  gchar *title;
  int i;

  OK_BEG ();
  
  title = poppler_document_get_title (doc);
  print_escaped (title, FALSE);
  g_free (title);

#ifndef HAVE_POPPLER_GET_METADATA
  putchar ('\n');
#else
  md[0] = poppler_document_get_author (doc);
  md[1] = poppler_document_get_subject (doc);
  md[2] = poppler_document_get_keywords (doc);
  md[3] = poppler_document_get_creator (doc);
  md[4] = poppler_document_get_producer (doc);
  md[5] = poppler_document_get_pdf_version_string (doc);
  
  for (i = 0; i < 6; ++i)
    {
      print_escaped (md[i], FALSE);
      g_free (md[i]);
    }

  date = poppler_document_get_creation_date (doc);
  printf ("%lld:", (gint64) date);
  date = poppler_document_get_modification_date (doc);
  printf ("%lld\n", (gint64) date);
#endif
  OK_END ();
#else
  printf_error ("The metadata command is not supported by this version of epdfinfo");
#endif  /* HAVE_POPPLER_GET_TITLE */
}

                
//static void print_pdf_dest (const ctxt_t *ctx, Poppler

#ifdef HAVE_POPPLER_INDEX_ITER
static void
cmd_outline_walk (PopplerDocument *doc, PopplerIndexIter *iter, int depth)
{
  do
    {
      PopplerIndexIter *child;
      PopplerAction *action = poppler_index_iter_get_action (iter);

      if (! action)
        continue; 
      
      if (do_handle_action (action))
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
#endif

static void
cmd_outline (const ctxt_t *ctx, const arg_t *args)
{
#ifdef HAVE_POPPLER_INDEX_ITER  
  PopplerIndexIter *iter = poppler_index_iter_new (args->value.doc->pdf);
  OK_BEG ();
  if (iter)
    {
      cmd_outline_walk (args->value.doc->pdf, iter, 1);
      poppler_index_iter_free (iter);
    }
  OK_END ();
#else
  printf_error ("The outline command is not supported by this version of epdfinfo");
#endif
}

static void
cmd_quit (const ctxt_t *ctx, const arg_t *args)
{
  release_documents (ctx, TRUE);
  OK ();
  exit (EXIT_SUCCESS);
}

static void
cmd_npages (const ctxt_t *ctx, const arg_t *args)
{
  int npages = poppler_document_get_n_pages (args->value.doc->pdf);
  OK_BEG ();
  printf ("%d\n", npages);
  OK_END ();
}

static void
cmd_pagelinks(const ctxt_t *ctx, const arg_t *args)
{
  PopplerDocument *doc = args[0].value.doc->pdf;
  PopplerPage *page;
  int pn = args[1].value.natnum;
  double width, height;
  GList *link_map, *item;
  char *text;
  
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

static void
cmd_gettext(const ctxt_t *ctx, const arg_t *args)
{
#ifdef HAVE_POPPLER_GET_TEXT
  PopplerDocument *doc = args[0].value.doc->pdf;
  int pn = args[1].value.natnum;
  double x1 = args[2].value.edge; 
  double y1 = args[3].value.edge;
  double x2 = args[4].value.edge;
  double y2 = args[5].value.edge;
  PopplerPage *page;
  double width, height;
  char *text;
  PopplerRectangle r;
  
  if (pn < 0 || pn > poppler_document_get_n_pages (doc))
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
  text = poppler_page_get_selected_text (page, POPPLER_SELECTION_GLYPH, &r);

  OK_BEG ();
  print_escaped (text, TRUE);
  OK_END ();

  g_free (text);
  g_object_unref (page);
#else
  printf_error ("The gettext command is not supported by this version of epdfinfo");
#endif
}

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

