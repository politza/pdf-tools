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

#include <config.h>

#include <assert.h>
#ifdef HAVE_ERR_H
#  include <err.h>
#endif
#ifdef HAVE_ERROR_H
#  include <error.h>
#endif
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
#include <errno.h>
#include <png.h>
#include <math.h>
#include <regex.h>
#include "synctex_parser.h"
#include "epdfinfo.h"


/* ================================================================== *
 * Helper Functions
 * ================================================================== */

#ifndef HAVE_ERR_H
/**
 * Print error message and quit.
 *
 * @param eval Return code
 * @param fmt Formatting string
 */
static void
err(int eval, const char *fmt, ...)
{
  va_list args;

  fprintf (stderr, "epdfinfo: ");
  if (fmt != NULL)
    {
      va_start (args, fmt);
      vfprintf (stderr, fmt, args);
      va_end (args);
      fprintf (stderr, ": %s\n", strerror(errno));
    }
  else
    {
      fprintf (stderr, "\n");
    }

  fflush (stderr);
  exit (eval);
}
#endif

#ifndef HAVE_GETLINE
/**
 * Read one line from a file.
 *
 * @param lineptr Pointer to malloc() allocated buffer
 * @param n Pointer to size of buffer
 * @param stream File pointer to read from
 */
static ssize_t
getline(char **lineptr, size_t *n, FILE *stream)
{
  size_t len = 0;
  int ch;

  if ((lineptr == NULL) || (n == NULL))
    {
      errno = EINVAL;
      return -1;
    }

  if (*lineptr == NULL)
    {
      *lineptr = malloc (128);
      *n = 128;
    }

  while ((ch = fgetc (stream)) != EOF)
    {
      (*lineptr)[len] = ch;

      if (++len >= *n)
        {
          *n += 128;
          *lineptr = realloc (*lineptr, *n);
        }

      if (ch == '\n')
        break;
    }
  (*lineptr)[len] = '\0';

  if (!len)
    {
      len = -1;
    }

  return len;
}
#endif

/**
 * Free a list of command arguments.
 *
 * @param args An array of command arguments.
 * @param n The length of the array.
 */
static void
free_command_args (command_arg_t *args, size_t n)
{
  if (! args)
    return;

  g_free (args);
}

/**
 * Free resources held by document.
 *
 * @param doc The document to be freed.
 */
static void
free_document (document_t *doc)
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
              annotation_t *a = (annotation_t*) item->data;
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

/**
 * Parse a list of whitespace separated double values.
 *
 * @param str The input string.
 * @param values[out] Values are put here.
 * @param nvalues How many values to parse.
 *
 * @return TRUE, if str contained exactly nvalues, else FALSE.
 */
static gboolean
parse_double_list (const char *str, gdouble *values, size_t nvalues)
{
  char *end;
  int i;

  if (! str)
    return FALSE;

  errno = 0;
  for (i = 0; i < nvalues; ++i)
    {
      gdouble n = g_ascii_strtod (str, &end);

      if (str == end || errno)
        return FALSE;

      values[i] = n;
      str = end;
    }

  if (*end)
    return FALSE;

  return TRUE;
}

static gboolean
parse_rectangle (const char *str, PopplerRectangle *r)
{
  gdouble values[4];

  if (! r)
    return FALSE;

  if (! parse_double_list (str, values, 4))
    return FALSE;

  r->x1 = values[0];
  r->y1 = values[1];
  r->x2 = values[2];
  r->y2 = values[3];

  return TRUE;
}

static gboolean
parse_edges_or_position (const char *str, PopplerRectangle *r)
{
  return (parse_rectangle (str, r)
          && r->x1 >= 0 && r->x1 <= 1
          && r->x2 <= 1
          && r->y1 >= 0 && r->y1 <= 1
          && r->y2 <= 1);
}

static gboolean
parse_edges (const char *str, PopplerRectangle *r)
{
  return (parse_rectangle (str, r)
          && r->x1 >= 0 && r->x1 <= 1
          && r->x2 >= 0 && r->x2 <= 1
          && r->y1 >= 0 && r->y1 <= 1
          && r->y2 >= 0 && r->y2 <= 1);
}

/**
 * Print a string properly escaped for a response.
 *
 * @param str The string to be printed.
 * @param suffix_char Append a newline if NEWLINE, a colon if COLON.
 */
static void
print_response_string (const char *str, enum suffix_char suffix)
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

  switch (suffix)
    {
    case NEWLINE:
      putchar ('\n');
      break;
    case COLON:
      putchar (':');
      break;
    default: ;
    }
}


/**
 * Print a formatted error response.
 *
 * @param fmt The printf-like format string.
 */
static void
printf_error_response (const char *fmt, ...)
{
  va_list va;
  puts ("ERR");
  va_start (va, fmt);
  vprintf (fmt, va);
  va_end (va);
  puts ("\n.");
  fflush (stdout);
}

/**
 * Remove one trailing newline character.  Does nothing, if str does
 * not end with a newline.
 *
 * @param str The string.
 *
 * @return str with trailing newline removed.
 */
static char*
strchomp (char *str)
{
  size_t length;

  if (! str)
    return str;

  length = strlen (str);
  if (str[length - 1] == '\n')
    str[length - 1] = '\0';

  return str;
}

/**
 * Create a new, temporary file and returns its name.
 *
 * @return The filename.
 */
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

static void
image_recolor (cairo_surface_t * surface, const PopplerColor * fg,
	       const PopplerColor * bg)
{
  /* uses a representation of a rgb color as follows:
     - a lightness scalar (between 0,1), which is a weighted average of r, g, b,
     - a hue vector, which indicates a radian direction from the grey axis,
     inside the equal lightness plane.
     - a saturation scalar between 0,1. It is 0 when grey, 1 when the color is
     in the boundary of the rgb cube.
   */

  const unsigned int page_width = cairo_image_surface_get_width (surface);
  const unsigned int page_height = cairo_image_surface_get_height (surface);
  const int rowstride = cairo_image_surface_get_stride (surface);
  unsigned char *image = cairo_image_surface_get_data (surface);

  /* RGB weights for computing lightness. Must sum to one */
  static const double a[] = { 0.30, 0.59, 0.11 };

  const double f = 65535.;
  const double rgb_fg[] = {
    fg->red / f, fg->green / f, fg->blue / f
  };
  const double rgb_bg[] = {
    bg->red / f, bg->green / f, bg->blue / f
  };

  const double rgb_diff[] = {
    rgb_bg[0] - rgb_fg[0],
    rgb_bg[1] - rgb_fg[1],
    rgb_bg[2] - rgb_fg[2]
  };

  unsigned int y;
  for (y = 0; y < page_height * rowstride; y += rowstride)
    {
      unsigned char *data = image + y;

      unsigned int x;
      for (x = 0; x < page_width; x++, data += 4)
	{
	  /* Careful. data color components blue, green, red. */
	  const double rgb[3] = {
	    (double) data[2] / 256.,
	    (double) data[1] / 256.,
	    (double) data[0] / 256.
	  };

	  /* compute h, s, l data   */
	  double l = a[0] * rgb[0] + a[1] * rgb[1] + a[2] * rgb[2];

	  /* linear interpolation between dark and light with color ligtness as
	   * a parameter */
	  data[2] =
	    (unsigned char) round (255. * (l * rgb_diff[0] + rgb_fg[0]));
	  data[1] =
	    (unsigned char) round (255. * (l * rgb_diff[1] + rgb_fg[1]));
	  data[0] =
	    (unsigned char) round (255. * (l * rgb_diff[2] + rgb_fg[2]));
	}
    }
}

/**
 * Render a PDF page.
 *
 * @param pdf The PDF document.
 * @param page The page to be rendered.
 * @param width The desired width of the image.
 *
 * @return A cairo_t context encapsulating the rendered image, or
 *         NULL, if rendering failed for some reason.
 */
static cairo_surface_t*
image_render_page(PopplerDocument *pdf, PopplerPage *page,
                  int width, gboolean do_render_annotaions,
                  const render_options_t *options)
{
  cairo_t *cr = NULL;
  cairo_surface_t *surface = NULL;
  double pt_width, pt_height;
  int height;
  double scale = 1;

  if (! page || ! pdf)
    return NULL;

  if (width < 1)
    width = 1;

  poppler_page_get_size (page, &pt_width, &pt_height);
  scale = width / pt_width;
  height = (int) ((scale * pt_height) + 0.5);

  surface = cairo_image_surface_create (CAIRO_FORMAT_ARGB32,
                                        width, height);

  if (cairo_surface_status (surface) != CAIRO_STATUS_SUCCESS)
    {
      fprintf (stderr, "Failed to create cairo surface\n");
      goto error;
    }

  cr = cairo_create (surface);
  if (cairo_status(cr) != CAIRO_STATUS_SUCCESS)
    {
      fprintf (stderr, "Failed to create cairo handle\n");
      goto error;
    }

  cairo_translate (cr, 0, 0);
  cairo_scale (cr, scale, scale);
  /* Render w/o annotations. */
  if (! do_render_annotaions || (options && options->printed))
    poppler_page_render_for_printing_with_options
      (page, cr, POPPLER_PRINT_DOCUMENT);
  else
    poppler_page_render (page, cr) ;
  if (cairo_status(cr) != CAIRO_STATUS_SUCCESS)
    {
      fprintf (stderr, "Failed to render page\n");
      goto error;
    }

  /* This makes the colors look right. */
  cairo_set_operator (cr, CAIRO_OPERATOR_DEST_OVER);
  cairo_set_source_rgb (cr, 1., 1., 1.);

  cairo_paint (cr);

  if (options && options->usecolors)
    image_recolor (surface, &options->fg, &options->bg);

  cairo_destroy (cr);

  return surface;

 error:
  if (surface != NULL)
    cairo_surface_destroy (surface);
  if (cr != NULL)
    cairo_destroy (cr);
  return NULL;
}

/**
 * Write an image to a filename.
 *
 * @param cr The cairo context encapsulating the image.
 * @param filename The filename to be written to.
 * @param type The desired image type.
 *
 * @return 1 if the image was written successfully, else 0.
 */
static gboolean
image_write (cairo_surface_t *surface, const char *filename, enum image_type type)
{

  int i, j;
  unsigned char *data;
  int width, height;
  FILE *file = NULL;
  gboolean success = 0;

  if (! surface ||
      cairo_surface_status(surface) != CAIRO_STATUS_SUCCESS)
    {
      fprintf (stderr, "Invalid cairo surface\n");
      return 0;
    }

  if (! (file = fopen (filename, "wb")))
    {
      fprintf (stderr, "Can not open file: %s\n", filename);
      return 0;
    }

  cairo_surface_flush (surface);
  width = cairo_image_surface_get_width (surface);
  height = cairo_image_surface_get_height (surface);
  data = cairo_image_surface_get_data (surface);

  switch (type)
    {
    case PPM:
      {
        unsigned char *buffer = g_malloc (width * height * 3);
        unsigned char *buffer_p = buffer;

        fprintf (file, "P6\n%d %d\n255\n", width, height);
        for (i = 0; i < width * height; ++i, data += 4, buffer_p += 3)
          ARGB_TO_RGB (buffer_p, data);
        fwrite (buffer, 1, width * height * 3, file);
        g_free (buffer);
        success = 1;
      }
      break;
    case PNG:
      {
        png_infop info_ptr = NULL;
        png_structp png_ptr = NULL;
        unsigned char *row = NULL;

        png_ptr = png_create_write_struct (PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
        if (!png_ptr)
          goto finalize;

        info_ptr = png_create_info_struct(png_ptr);
        if (!info_ptr)
          goto finalize;

        if (setjmp(png_jmpbuf(png_ptr)))
          goto finalize;

        png_init_io (png_ptr, file);
        png_set_compression_level (png_ptr, 1);
        png_set_IHDR (png_ptr, info_ptr, width, height,
                      8, PNG_COLOR_TYPE_RGB, PNG_INTERLACE_NONE,
                      PNG_COMPRESSION_TYPE_BASE,
                      PNG_FILTER_TYPE_DEFAULT);

        png_set_filter (png_ptr, PNG_FILTER_TYPE_BASE,
                        PNG_FILTER_NONE);
        png_write_info (png_ptr, info_ptr);
        row = g_malloc (3 * width);
        for (i = 0; i < height; ++i)
          {
            unsigned char *row_p = row;
            for (j = 0; j < width; ++j, data += 4, row_p += 3)
              {
                ARGB_TO_RGB (row_p, data);
              }
            png_write_row (png_ptr, row);
          }
        png_write_end (png_ptr, NULL);
        success = 1;
      finalize:
        if (png_ptr)
          png_destroy_write_struct (&png_ptr, &info_ptr);
        if (row)
          g_free (row);
        if (! success)
          fprintf (stderr, "Error writing png data\n");
      }
      break;
    default:
      internal_error ("switch fell through");
    }

  fclose (file);
  return success;
}

static void
image_write_print_response(cairo_surface_t *surface, enum image_type type)
{
  char *filename = mktempfile ();

  perror_if_not (filename, "Unable to create temporary file");
  if (image_write (surface, filename, type))
    {
      OK_BEGIN ();
      print_response_string (filename, NEWLINE);
      OK_END ();
    }
  else
    {
      printf_error_response ("Unable to write image");
    }
  free (filename);
 error:
  return;
}

static void
region_print (cairo_region_t *region, double width, double height)
{
  int i;

  for (i = 0; i < cairo_region_num_rectangles (region); ++i)
    {
      cairo_rectangle_int_t r;

      cairo_region_get_rectangle (region, i, &r);
      printf ("%f %f %f %f",
              r.x / width,
              r.y / height,
              (r.x + r.width) / width,
              (r.y + r.height) / height);
      if (i < cairo_region_num_rectangles (region) - 1)
        putchar (':');
    }
  if (0 == cairo_region_num_rectangles (region))
    printf ("0.0 0.0 0.0 0.0");
}

/**
 * Return a string representation of a PopplerActionType.
 *
 * @param type The PopplerActionType.
 *
 * @return Its string representation.
 */
static const char *
xpoppler_action_type_string(PopplerActionType type)
{
  switch (type)
    {
    case POPPLER_ACTION_UNKNOWN: return "unknown";
    case POPPLER_ACTION_NONE: return "none";
    case POPPLER_ACTION_GOTO_DEST: return "goto-dest";
    case POPPLER_ACTION_GOTO_REMOTE: return "goto-remote";
    case POPPLER_ACTION_LAUNCH: return "launch";
    case POPPLER_ACTION_URI: return "uri";
    case POPPLER_ACTION_NAMED: return "goto-dest"; /* actually "named" */
    case POPPLER_ACTION_MOVIE: return "movie";
    case POPPLER_ACTION_RENDITION: return "rendition";
    case POPPLER_ACTION_OCG_STATE: return "ocg-state";
    case POPPLER_ACTION_JAVASCRIPT: return "javascript";
    default: return "invalid";
    }
}

/**
 * Return a string representation of a PopplerAnnotType.
 *
 * @param type The PopplerAnnotType.
 *
 * @return Its string representation.
 */
static const char *
xpoppler_annot_type_string (PopplerAnnotType type)
{
  switch (type)
    {
    case POPPLER_ANNOT_UNKNOWN: return "unknown";
    case POPPLER_ANNOT_TEXT: return "text";
    case POPPLER_ANNOT_LINK: return "link";
    case POPPLER_ANNOT_FREE_TEXT: return "free-text";
    case POPPLER_ANNOT_LINE: return "line";
    case POPPLER_ANNOT_SQUARE: return "square";
    case POPPLER_ANNOT_CIRCLE: return "circle";
    case POPPLER_ANNOT_POLYGON: return "polygon";
    case POPPLER_ANNOT_POLY_LINE: return "poly-line";
    case POPPLER_ANNOT_HIGHLIGHT: return "highlight";
    case POPPLER_ANNOT_UNDERLINE: return "underline";
    case POPPLER_ANNOT_SQUIGGLY: return "squiggly";
    case POPPLER_ANNOT_STRIKE_OUT: return "strike-out";
    case POPPLER_ANNOT_STAMP: return "stamp";
    case POPPLER_ANNOT_CARET: return "caret";
    case POPPLER_ANNOT_INK: return "ink";
    case POPPLER_ANNOT_POPUP: return "popup";
    case POPPLER_ANNOT_FILE_ATTACHMENT: return "file";
    case POPPLER_ANNOT_SOUND: return "sound";
    case POPPLER_ANNOT_MOVIE: return "movie";
    case POPPLER_ANNOT_WIDGET: return "widget";
    case POPPLER_ANNOT_SCREEN: return "screen";
    case POPPLER_ANNOT_PRINTER_MARK: return "printer-mark";
    case POPPLER_ANNOT_TRAP_NET: return "trap-net";
    case POPPLER_ANNOT_WATERMARK: return "watermark";
    case POPPLER_ANNOT_3D: return "3d";
    default: return "invalid";
    }
}

/**
 * Return a string representation of a PopplerAnnotTextState.
 *
 * @param type The PopplerAnnotTextState.
 *
 * @return Its string representation.
 */
static const char *
xpoppler_annot_text_state_string (PopplerAnnotTextState state)
{
  switch (state)
    {
    case POPPLER_ANNOT_TEXT_STATE_MARKED: return "marked";
    case POPPLER_ANNOT_TEXT_STATE_UNMARKED: return "unmarked";
    case POPPLER_ANNOT_TEXT_STATE_ACCEPTED: return "accepted";
    case POPPLER_ANNOT_TEXT_STATE_REJECTED: return "rejected";
    case POPPLER_ANNOT_TEXT_STATE_CANCELLED: return "cancelled";
    case POPPLER_ANNOT_TEXT_STATE_COMPLETED: return "completed";
    case POPPLER_ANNOT_TEXT_STATE_NONE: return "none";
    case POPPLER_ANNOT_TEXT_STATE_UNKNOWN:
    default: return "unknown";
    }
};

static document_t*
document_open (const epdfinfo_t *ctx, const char *filename,
               const char *passwd, GError **gerror)
{
  char *uri;
  document_t *doc = g_hash_table_lookup (ctx->documents, filename);

  if (NULL != doc)
    return doc;

  doc = g_malloc0(sizeof (document_t));
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
      g_hash_table_insert (ctx->documents, doc->filename, doc);
    }
  g_free (uri);
  return doc;
}

/**
 * Split command args into a list of strings.
 *
 * @param args The colon separated list of arguments.
 * @param nargs[out] The number of returned arguments.
 *
 * @return The list of arguments, which should be freed by the caller.
 */
static char **
command_arg_split (const char *args, int *nargs)
{
  char **list = g_malloc (sizeof (char*) * 16);
  int i = 0;
  size_t allocated = 16;
  char *buffer = NULL;
  gboolean last = FALSE;

  if (! args)
    goto theend;

  buffer = g_malloc (strlen (args) + 1);

  while (*args || last)
    {
      gboolean esc = FALSE;
      char *buffer_p = buffer;

      while (*args && (*args != ':' || esc))
        {
          if (esc)
            {
              if (*args == 'n')
                {
                  ++args;
                  *buffer_p++ = '\n';
                }
              else
                {
                  *buffer_p++ = *args++;
                }
              esc = FALSE;
            }
          else if (*args == '\\')
            {
              ++args;
              esc = TRUE;
            }
          else
            {
              *buffer_p++ = *args++;
            }
        }

      *buffer_p = '\0';

      if (i >= allocated)
        {
          allocated = 2 * allocated + 1;
          list = g_realloc (list, sizeof (char*) * allocated);
        }
      list[i++] = g_strdup (buffer);

      last = FALSE;
      if (*args)
        {
          ++args;
          if (! *args)
            last = TRUE;
        }
    }

 theend:
  g_free (buffer);
  *nargs = i;

  return list;
}

static gboolean
command_arg_parse_arg (const epdfinfo_t *ctx, const char *arg,
                       command_arg_t *cmd_arg, command_arg_type_t type,
                       gchar **error_msg)
{
  GError *gerror = NULL;

  if (! arg || !cmd_arg)
    return FALSE;

  switch (type)
    {
    case ARG_DOC:
      {
        document_t *doc = document_open (ctx, arg, NULL, &gerror);
        cerror_if_not (doc, error_msg,
                       "Error opening %s:%s", arg,
                       gerror ? gerror->message : "Unknown reason");

        cmd_arg->value.doc = doc;
        break;
      }
    case ARG_BOOL:
      cerror_if_not (! strcmp (arg, "0") || ! strcmp (arg, "1"),
                     error_msg, "Expected 0 or 1:%s", arg);
      cmd_arg->value.flag = *arg == '1';
      break;
    case ARG_NONEMPTY_STRING:
      cerror_if_not (*arg, error_msg, "Non-empty string expected");
      /* fall through */
    case ARG_STRING:
      cmd_arg->value.string = arg;
      break;
    case ARG_NATNUM:
      {
        char *endptr;
        long n = strtol (arg, &endptr, 0);
        cerror_if_not (! (*endptr || (n < 0)), error_msg,
                       "Expected natural number:%s", arg);
        cmd_arg->value.natnum = n;
      }
      break;
    case ARG_EDGES_OR_POSITION:
      {
        PopplerRectangle *r = &cmd_arg->value.rectangle;
        cerror_if_not (parse_edges_or_position (arg, r),
                       error_msg,
                       "Expected a relative position or rectangle: %s", arg);
      }
      break;
    case ARG_EDGES:
      {
        PopplerRectangle *r = &cmd_arg->value.rectangle;
        cerror_if_not (parse_edges (arg, r),
                       error_msg,
                       "Expected a relative rectangle: %s", arg);
      }
      break;
    case ARG_EDGE_OR_NEGATIVE:
    case ARG_EDGE:
      {
        char *endptr;
        double n = strtod (arg, &endptr);
        cerror_if_not (! (*endptr || (type != ARG_EDGE_OR_NEGATIVE && n < 0.0) || n > 1.0),
                       error_msg, "Expected a relative edge: %s", arg);
        cmd_arg->value.edge = n;
      }
      break;
    case ARG_COLOR:
      {
        guint r,g,b;
        cerror_if_not ((strlen (arg) == 7
                        && 3 == sscanf (arg, "#%2x%2x%2x", &r, &g, &b)),
                       error_msg, "Invalid color: %s", arg);
        cmd_arg->value.color.red = r << 8;
        cmd_arg->value.color.green = g << 8;
        cmd_arg->value.color.blue = b << 8;
      }
      break;
    case ARG_INVALID:
    default:
      internal_error ("switch fell through");
    }

  cmd_arg->type = type;

  return TRUE;
 error:
  if (gerror)
    {
      g_error_free (gerror);
      gerror = NULL;
    }
  return FALSE;
}

/**
 * Parse arguments for a command.
 *
 * @param ctx The epdfinfo context.
 * @param args A string holding the arguments.  This is either empty
 *             or the suffix of the command starting at the first
 *             colon after the command name.
 * @param len The length of args.
 * @param cmd The command for which the arguments should be parsed.
 *
 * @return
 */
static command_arg_t*
command_arg_parse(epdfinfo_t *ctx, char **args, int nargs,
                  const command_t *cmd, gchar **error_msg)
{
  command_arg_t *cmd_args = g_malloc0 (cmd->nargs * sizeof (command_arg_t));
  int i;

  if (nargs < cmd->nargs - 1
      || (nargs == cmd->nargs - 1
          &&  cmd->args_spec[cmd->nargs - 1] != ARG_REST)
      || (nargs > cmd->nargs
          && (cmd->nargs == 0
              || cmd->args_spec[cmd->nargs - 1] != ARG_REST)))
    {
      if (error_msg)
        {
          *error_msg =
            g_strdup_printf ("Command `%s' expects %d argument(s), %d given",
                             cmd->name, cmd->nargs, nargs);
        }
      goto failure;
    }

  for (i = 0; i < cmd->nargs; ++i)
    {
      if (i == cmd->nargs - 1 && cmd->args_spec[i] == ARG_REST)
        {
          cmd_args[i].value.rest.args = args + i;
          cmd_args[i].value.rest.nargs = nargs - i;
          cmd_args[i].type = ARG_REST;
        }
      else if (i >= nargs
               || ! command_arg_parse_arg (ctx, args[i], cmd_args + i,
                                           cmd->args_spec[i], error_msg))
        {
          goto failure;
        }
    }

  return cmd_args;

 failure:
  free_command_args (cmd_args, cmd->nargs);
  return NULL;
}

static void
command_arg_print(const command_arg_t *arg)
{
  switch (arg->type)
    {
    case ARG_INVALID:
      printf ("[invalid]");
      break;
    case ARG_DOC:
      print_response_string (arg->value.doc->filename, NONE);
      break;
    case ARG_BOOL:
      printf ("%d", arg->value.flag ? 1 : 0);
      break;
    case ARG_NONEMPTY_STRING:   /* fall */
    case ARG_STRING:
      print_response_string (arg->value.string, NONE);
      break;
    case ARG_NATNUM:
      printf ("%ld", arg->value.natnum);
      break;
    case ARG_EDGE_OR_NEGATIVE:  /* fall */
    case ARG_EDGE:
      printf ("%f", arg->value.edge);
      break;
    case ARG_EDGES_OR_POSITION: /* fall */
    case ARG_EDGES:
      {
        const PopplerRectangle *r = &arg->value.rectangle;
        if (r->x2 < 0 && r->y2 < 0)
          printf ("%f %f", r->x1, r->y1);
        else
          printf ("%f %f %f %f", r->x1, r->y1, r->x2, r->y2);
        break;
      }
    case ARG_COLOR:
      {
        const PopplerColor *c = &arg->value.color;
        printf ("#%.2x%.2x%.2x", c->red >> 8,
                c->green >> 8, c->blue >> 8);
        break;
      }
    case ARG_REST:
      {
        int i;
        for (i = 0; i < arg->value.rest.nargs; ++i)
          print_response_string (arg->value.rest.args[i], COLON);
        if (arg->value.rest.nargs > 0)
          print_response_string (arg->value.rest.args[i], NONE);
        break;
      }
    default:
      internal_error ("switch fell through");
    }
}

static size_t
command_arg_type_size(command_arg_type_t type)
{
  command_arg_t arg;
  switch (type)
    {
    case ARG_INVALID: return 0;
    case ARG_DOC: return sizeof (arg.value.doc);
    case ARG_BOOL: return sizeof (arg.value.flag);
    case ARG_NONEMPTY_STRING:   /* fall */
    case ARG_STRING: return sizeof (arg.value.string);
    case ARG_NATNUM: return sizeof (arg.value.natnum);
    case ARG_EDGE_OR_NEGATIVE:  /* fall */
    case ARG_EDGE: return sizeof (arg.value.edge);
    case ARG_EDGES_OR_POSITION: /* fall */
    case ARG_EDGES: return sizeof (arg.value.rectangle);
    case ARG_COLOR: return sizeof (arg.value.color);
    case ARG_REST: return sizeof (arg.value.rest);
    default:
      internal_error ("switch fell through");
      return 0;
    }
}


/* ------------------------------------------------------------------ *
 * PDF Actions
 * ------------------------------------------------------------------ */

static gboolean
action_is_handled (PopplerAction *action)
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
    default: ;
    }
  return FALSE;
}

static void
action_print_destination (PopplerDocument *doc, PopplerAction *action)
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
      print_response_string (action->goto_remote.file_name, COLON);
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
    default: ;
    }

 theend:
  if (free_dest)
    poppler_dest_free (dest);
}

static void
action_print (PopplerDocument *doc, PopplerAction *action)
{
  if (! action_is_handled (action))
    return;

  print_response_string (xpoppler_action_type_string (action->any.type), COLON);
  print_response_string (action->any.title, COLON);
  switch (action->any.type)
    {
    case POPPLER_ACTION_GOTO_REMOTE:
    case POPPLER_ACTION_GOTO_DEST:
    case POPPLER_ACTION_NAMED:
      action_print_destination (doc, action);
      putchar ('\n');
      break;
    case POPPLER_ACTION_LAUNCH:
      print_response_string (action->launch.file_name, COLON);
      print_response_string (action->launch.params, NEWLINE);
      break;
    case POPPLER_ACTION_URI:
      print_response_string (action->uri.uri, NEWLINE);
      break;
    default:
      ;
    }
}


/* ------------------------------------------------------------------ *
 * PDF Annotations and Attachments
 * ------------------------------------------------------------------ */

/* static gint
 * annotation_cmp_edges (const annotation_t *a1, const annotation_t *a2)
 * {
 *   PopplerRectangle *e1 = &a1->amap->area;
 *   PopplerRectangle *e2 = &a2->amap->area;
 *
 *   return (e1->y1 > e2->y1 ? -1
 *           : e1->y1 < e2->y1 ? 1
 *           : e1->x1 < e2->x1 ? -1
 *           : e1->x1 != e2->x1);
 * } */

static GList*
annoation_get_for_page (document_t *doc, gint pn)
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
      annotation_t *a = g_malloc (sizeof (annotation_t));
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

static annotation_t*
annotation_get_by_key (document_t *doc, const gchar *key)
{
  if (! doc->annotations.keys)
    return NULL;

  return g_hash_table_lookup (doc->annotations.keys, key);
}

#ifdef HAVE_POPPLER_ANNOT_MARKUP
void
annotation_translate_quadrilateral (PopplerPage *page, PopplerQuadrilateral *q, gboolean inverse)
{
  PopplerRectangle cbox;
  gdouble xs, ys;

  poppler_page_get_crop_box (page, &cbox);
  xs = MIN (cbox.x1, cbox.x2);
  ys = MIN (cbox.y1, cbox.y2);

  if (inverse)
    {
      xs = -xs; ys = -ys;
    }

  q->p1.x -= xs, q->p2.x -= xs; q->p3.x -= xs; q->p4.x -= xs;
  q->p1.y -= ys, q->p2.y -= ys; q->p3.y -= ys; q->p4.y -= ys;
}

static cairo_region_t*
annotation_markup_get_text_regions (PopplerPage *page, PopplerAnnotTextMarkup *a)
{
  GArray *quads = poppler_annot_text_markup_get_quadrilaterals (a);
  int i;
  cairo_region_t *region = cairo_region_create ();
  gdouble height;

  poppler_page_get_size (page, NULL, &height);

  for (i = 0; i < quads->len; ++i)
    {
      PopplerQuadrilateral *q = &g_array_index (quads, PopplerQuadrilateral, i);
      cairo_rectangle_int_t r;

      annotation_translate_quadrilateral (page, q, FALSE);
      q->p1.y = height - q->p1.y;
      q->p2.y = height - q->p2.y;
      q->p3.y = height - q->p3.y;
      q->p4.y = height - q->p4.y;

      r.x = (int) (MIN (q->p1.x, MIN (q->p2.x, MIN (q->p3.x, q->p4.x))) + 0.5);
      r.y = (int) (MIN (q->p1.y, MIN (q->p2.y, MIN (q->p3.y, q->p4.y))) + 0.5);
      r.width = (int) (MAX (q->p1.x, MAX (q->p2.x, MAX (q->p3.x, q->p4.x))) + 0.5)
                - r.x;
      r.height = (int) (MAX (q->p1.y, MAX (q->p2.y, MAX (q->p3.y, q->p4.y))) + 0.5)
                 - r.y;

      cairo_region_union_rectangle (region, &r);
    }
  g_array_unref (quads);
  return region;
}

/**
 * Append quadrilaterals equivalent to region to an array.
 *
 * @param page The page of the annotation.  This is used to get the
 *             text regions and pagesize.
 * @param region The region to add.
 * @param garray[in,out] An array of PopplerQuadrilateral, where the
 *              new quadrilaterals will be appended.
 */
static void
annotation_markup_append_text_region (PopplerPage *page, PopplerRectangle *region,
                                      GArray *garray)
{
  gdouble height;
  /* poppler_page_get_selection_region is deprecated w/o a
     replacement.  (poppler_page_get_selected_region returns a union
     of rectangles.) */
  GList *regions =
    poppler_page_get_selection_region (page, 1.0, POPPLER_SELECTION_GLYPH, region);
  GList *item;

  poppler_page_get_size (page, NULL, &height);
  for (item = regions; item; item = item->next)
    {
      PopplerRectangle *r = item->data;
      PopplerQuadrilateral q;

      q.p1.x = r->x1;
      q.p1.y = height - r->y1;
      q.p2.x = r->x2;
      q.p2.y = height - r->y1;
      q.p4.x = r->x2;
      q.p4.y = height - r->y2;
      q.p3.x = r->x1;
      q.p3.y = height - r->y2;

      annotation_translate_quadrilateral (page, &q, TRUE);
      g_array_append_val (garray, q);
    }
  g_list_free (regions);
}

#endif
/**
 * Create a new annotation.
 *
 * @param doc The document for which to create it.
 * @param type The type of the annotation.
 * @param r The rectangle where annotation will end up on the page.
 *
 * @return The new annotation, or NULL, if the annotation type is
 *         not available.
 */
static PopplerAnnot*
annotation_new (const epdfinfo_t *ctx, document_t *doc, PopplerPage *page,
                const char *type, PopplerRectangle *r,
                const command_arg_t *rest, char **error_msg)
{

  PopplerAnnot *a = NULL;
  int nargs = rest->value.rest.nargs;
#ifdef HAVE_POPPLER_ANNOT_MARKUP
  char * const *args = rest->value.rest.args;
  int i;
  GArray *garray = NULL;
  command_arg_t carg;
  double width, height;
  cairo_region_t *region = NULL;
#endif

  if (! strcmp (type, "text"))
    {
      cerror_if_not (nargs == 0, error_msg, "%s", "Too many arguments");
      return poppler_annot_text_new (doc->pdf, r);
    }

#ifdef HAVE_POPPLER_ANNOT_MARKUP
  garray = g_array_new (FALSE, FALSE, sizeof (PopplerQuadrilateral));
  poppler_page_get_size (page, &width, &height);
  for (i = 0; i < nargs; ++i)
    {
      PopplerRectangle *rr = &carg.value.rectangle;

      error_if_not (command_arg_parse_arg (ctx, args[i], &carg,
                                           ARG_EDGES, error_msg));
      rr->x1 *= width; rr->x2 *= width;
      rr->y1 *= height; rr->y2 *= height;
      annotation_markup_append_text_region (page, rr, garray);
    }
  cerror_if_not (garray->len > 0, error_msg, "%s",
                 "Unable to create empty markup annotation");

  if (! strcmp (type, "highlight"))
    a = poppler_annot_text_markup_new_highlight (doc->pdf, r, garray);
  else if (! strcmp (type, "squiggly"))
    a = poppler_annot_text_markup_new_squiggly (doc->pdf, r, garray);
  else if (! strcmp (type, "strike-out"))
    a = poppler_annot_text_markup_new_strikeout (doc->pdf, r, garray);
  else if (! strcmp (type, "underline"))
    a = poppler_annot_text_markup_new_underline (doc->pdf, r, garray);
  else
    cerror_if_not (0, error_msg, "Unknown annotation type: %s", type);

#endif
 error:
#ifdef HAVE_POPPLER_ANNOT_MARKUP
  if (garray) g_array_unref (garray);
  if (region) cairo_region_destroy (region);
#endif
  return a;
}

static gboolean
annotation_edit_validate (const epdfinfo_t *ctx, const command_arg_t *rest,
                          PopplerAnnot *annotation, char **error_msg)
{
  int nargs = rest->value.rest.nargs;
  char * const *args = rest->value.rest.args;
  int i = 0;
  command_arg_t carg;

  const char* error_fmt =
    "Can modify `%s' property only for %s annotations";

  while (i < nargs)
    {
      command_arg_type_t atype = ARG_INVALID;
      const char *key = args[i++];

      cerror_if_not (i < nargs, error_msg, "Missing a value argument");

      if (! strcmp (key, "flags"))
        atype = ARG_NATNUM;
      else if (! strcmp (key, "color"))
        atype = ARG_COLOR;
      else if (! strcmp (key, "contents"))
        atype = ARG_STRING;
      else if (! strcmp (key, "edges"))
        atype = ARG_EDGES_OR_POSITION;
      else if (! strcmp (key, "label"))
        {
          cerror_if_not (POPPLER_IS_ANNOT_MARKUP (annotation), error_msg,
                         error_fmt, key, "markup");
          atype = ARG_STRING;
        }
      else if (! strcmp (key, "opacity"))
        {
          cerror_if_not (POPPLER_IS_ANNOT_MARKUP (annotation), error_msg,
                         error_fmt, key, "markup");
          atype = ARG_EDGE;
        }
      else if (! strcmp (key, "popup"))
        {
          cerror_if_not (POPPLER_IS_ANNOT_MARKUP (annotation), error_msg,
                         error_fmt, key, "markup");
          atype = ARG_EDGES;
        }
      else if (! strcmp (key, "popup-is-open"))
        {
          cerror_if_not (POPPLER_IS_ANNOT_MARKUP (annotation), error_msg,
                         error_fmt, key, "markup");
          atype = ARG_BOOL;
        }
      else if (! strcmp (key, "icon"))
        {
          cerror_if_not (POPPLER_IS_ANNOT_TEXT (annotation), error_msg,
                         error_fmt, key, "text");
          atype = ARG_STRING;
        }
      else if (! strcmp (key, "is-open"))
        {
          cerror_if_not (POPPLER_IS_ANNOT_TEXT (annotation), error_msg,
                         error_fmt, key, "text");
          atype = ARG_BOOL;
        }
      else
        {
          cerror_if_not (0, error_msg,
                         "Unable to modify property `%s'", key);
        }

      if (! command_arg_parse_arg (ctx, args[i++], &carg, atype, error_msg))
        return FALSE;
    }

  return TRUE;

 error:
  return FALSE;
}

static void
annotation_print (const annotation_t *annot, /* const */ PopplerPage *page)
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
  cairo_region_t *region = NULL;

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

#ifdef HAVE_POPPLER_ANNOT_MARKUP
  if (POPPLER_IS_ANNOT_TEXT_MARKUP (a))
    {
      region = annotation_markup_get_text_regions (page, POPPLER_ANNOT_TEXT_MARKUP (a));
      perror_if_not (region, "%s", "Unable to extract annotation's text regions");
    }
#endif

  /* >>> Any Annotation >>> */
  /* Page */
  printf ("%d:", poppler_page_get_index (page) + 1);
  /* Area */
  printf ("%f %f %f %f:", r.x1 / width, r.y1 / height
          , r.x2 / width, r.y2 / height);

  /* Type */
  printf ("%s:", xpoppler_annot_type_string (poppler_annot_get_annot_type (a)));
  /* Internal Key */
  print_response_string (key, COLON);

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
  print_response_string (text, COLON);
  g_free (text);

  /* Modified Date */
  text = poppler_annot_get_modified (a);
  print_response_string (text, NONE);
  g_free (text);

  /* <<< Any Annotation <<< */

  /* >>> Markup Annotation >>> */
  if (! POPPLER_IS_ANNOT_MARKUP (a))
    {
      putchar ('\n');
      goto theend;
    }

  putchar (':');
  ma = POPPLER_ANNOT_MARKUP (a);
  /* Label */
  text = poppler_annot_markup_get_label (ma);
  print_response_string (text, COLON);
  g_free (text);

  /* Subject */
  text = poppler_annot_markup_get_subject (ma);
  print_response_string (text, COLON);
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
  text = xpoppler_annot_markup_get_created (ma);
  if (text)
    {
      print_response_string (text, NONE);
      g_free (text);
    }

  /* <<< Markup Annotation <<< */

  /* >>>  Text Annotation >>> */
  if (POPPLER_IS_ANNOT_TEXT (a))
    {
      putchar (':');
      ta = POPPLER_ANNOT_TEXT (a);
      /* Text Icon */
      text = poppler_annot_text_get_icon (ta);
      print_response_string (text, COLON);
      g_free (text);
      /* Text State */
      printf ("%s:%d",
              xpoppler_annot_text_state_string (poppler_annot_text_get_state (ta)),
              poppler_annot_text_get_is_open (ta));
    }
#ifdef HAVE_POPPLER_ANNOT_MARKUP
  /* <<< Text Annotation <<< */
  else if (POPPLER_IS_ANNOT_TEXT_MARKUP (a))
    {
      /* >>> Markup Text Annotation >>> */
      putchar (':');
      region_print (region, width, height);
      /* <<< Markup Text Annotation <<< */
    }
#endif
  putchar ('\n');
 theend:
#ifdef HAVE_POPPLER_ANNOT_MARKUP
 error:
#endif
  if (region) cairo_region_destroy (region);
}

static void
attachment_print (PopplerAttachment *att, const char *id, gboolean do_save)
{
  time_t time;

  print_response_string (id, COLON);
  print_response_string (att->name, COLON);
  print_response_string (att->description, COLON);
  if (att->size + 1 != 0)
    printf ("%" G_GSIZE_FORMAT ":", att->size);
  else
    printf ("-1:");
  time = (time_t) att->mtime;
  print_response_string (time > 0 ? strchomp (ctime (&time)) : "", COLON);
  time = (time_t) att->ctime;
  print_response_string (time > 0 ? strchomp (ctime (&time)) : "", COLON);
  print_response_string (att->checksum ? att->checksum->str : "" , COLON);
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
              print_response_string (filename, NONE);
            }
          free (filename);
        }
    }
  putchar ('\n');
}



/* ================================================================== *
 * Server command implementations
 * ================================================================== */

/* Name: features
   Args: None
   Returns: A list of compile-time features.
   Errors: None
*/

const command_arg_type_t cmd_features_spec[] = {};

static void
cmd_features (const epdfinfo_t *ctx, const command_arg_t *args)
{
  const char *features[] = {
#ifdef HAVE_POPPLER_FIND_OPTS
    "case-sensitive-search",
#else
    "no-case-sensitive-search",
#endif
#ifdef HAVE_POPPLER_ANNOT_WRITE
    "writable-annotations",
#else
    "no-writable-annotations",
#endif
#ifdef HAVE_POPPLER_ANNOT_MARKUP
    "markup-annotations"
#else
    "no-markup-annotations"
#endif
  };
  int i;
  OK_BEGIN ();
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

const command_arg_type_t cmd_open_spec[] =
  {
    ARG_NONEMPTY_STRING,        /* filename */
    ARG_STRING,                 /* password */
  };

static void
cmd_open (const epdfinfo_t *ctx, const command_arg_t *args)
{
  const char *filename = args[0].value.string;
  const char *passwd = args[1].value.string;
  GError *gerror = NULL;
  document_t *doc;

  if (! *passwd)
    passwd = NULL;

  doc = document_open(ctx, filename, passwd, &gerror);
  perror_if_not (doc, "Error opening %s:%s", filename,
                gerror ? gerror->message : "unknown error");
  OK ();

 error:
  if (gerror)
    {
      g_error_free (gerror);
      gerror = NULL;
    }
}

/* Name: close
   Args: filename
   Returns: 1 if file was open, otherwise 0.
   Errors: None
*/

const command_arg_type_t cmd_close_spec[] =
  {
    ARG_NONEMPTY_STRING         /* filename */
  };

static void
cmd_close (const epdfinfo_t *ctx, const command_arg_t *args)
{
  document_t *doc = g_hash_table_lookup(ctx->documents, args->value.string);

  g_hash_table_remove (ctx->documents, args->value.string);
  free_document (doc);
  OK_BEGIN ();
  puts (doc ? "1" : "0");
  OK_END ();
}

/* Name: closeall
   Args: None
   Returns: Nothing
   Errors: None
*/
static void
cmd_closeall (const epdfinfo_t *ctx, const command_arg_t *args)
{
  GHashTableIter iter;
  gpointer key, value;

  g_hash_table_iter_init (&iter, ctx->documents);
  while (g_hash_table_iter_next (&iter, &key, &value))
    {
      document_t *doc = (document_t*) value;
      free_document (doc);
      g_hash_table_iter_remove (&iter);
    }
  OK ();
}


const command_arg_type_t cmd_search_regexp_spec[] =
  {
    ARG_DOC,
    ARG_NATNUM,                 /* first page */
    ARG_NATNUM,                 /* last page */
    ARG_NONEMPTY_STRING,        /* regexp */
    ARG_NATNUM,                 /* compile flags */
    ARG_NATNUM                  /* match flags */
  };

static void
cmd_search_regexp(const epdfinfo_t *ctx, const command_arg_t *args)
{
  PopplerDocument *doc = args[0].value.doc->pdf;
  int first = args[1].value.natnum;
  int last = args[2].value.natnum;
  const gchar *regexp = args[3].value.string;
  GRegexCompileFlags cflags = args[4].value.natnum;
  GRegexMatchFlags mflags = args[5].value.natnum;
  double width, height;
  int pn;
  GError *gerror = NULL;
  GRegex *re = NULL;

  NORMALIZE_PAGE_ARG (doc, &first, &last);

  re = g_regex_new (regexp, cflags, mflags, &gerror);
  perror_if_not (NULL == gerror, "Invalid regexp: %s", gerror->message);

  OK_BEGIN ();
  for (pn = first; pn <= last; ++pn)
    {
      PopplerPage *page = poppler_document_get_page(doc, pn - 1);
      char *text;
      PopplerRectangle *rectangles = NULL;
      guint nrectangles;
      GMatchInfo *match = NULL;

      if (! page)
        continue;

      text = poppler_page_get_text (page);
      poppler_page_get_text_layout (page, &rectangles, &nrectangles);
      poppler_page_get_size (page, &width, &height);
      g_regex_match (re, text, 0, &match);

      while (g_match_info_matches (match))
        {
          const double scale = 100.0;
          gint start, end, ustart, ulen;
          gchar *string = NULL;
          gchar *line = NULL;
          int i;

          /* Does this ever happen ? */
          if (! g_match_info_fetch_pos (match, 0, &start, &end))
            continue;

          string = g_match_info_fetch (match, 0);
          ustart = g_utf8_strlen (text, start);
          ulen = g_utf8_strlen (string, -1);

          cairo_region_t *region = cairo_region_create ();
          /* Merge matched glyph rectangles. Scale them so we're able
             to use cairo . */
          if (ulen > 0)
            {
              assert (ustart < nrectangles
                      && ustart + ulen <= nrectangles);
              line = poppler_page_get_selected_text
                     (page, POPPLER_SELECTION_LINE, rectangles + ustart);

              for (i = ustart; i < ustart + ulen; ++i)
                {
                  PopplerRectangle *r = rectangles + i;
                  cairo_rectangle_int_t c;

                  c.x = (int) (scale * r->x1 + 0.5);
                  c.y = (int) (scale * r->y1 + 0.5);
                  c.width = (int) (scale * (r->x2 - r->x1) + 0.5);
                  c.height = (int) (scale * (r->y2 - r->y1) + 0.5);

                  cairo_region_union_rectangle (region, &c);
                }

            }

          printf ("%d:", pn);
          print_response_string (string, COLON);
          print_response_string (strchomp (line), COLON);
          region_print (region, width * scale, height * scale);
          putchar ('\n');
          cairo_region_destroy (region);
          g_free (string);
          g_free (line);
          g_match_info_next (match, NULL);
        }
      g_free (rectangles);
      g_object_unref (page);
      g_free (text);
      g_match_info_free (match);
    }
  OK_END ();

 error:
  if (re) g_regex_unref (re);
  if (gerror) g_error_free (gerror);
}

const command_arg_type_t cmd_regexp_flags_spec[] =
  {
  };

static void
cmd_regexp_flags (const epdfinfo_t *ctx, const command_arg_t *args)
{
  OK_BEGIN ();
  printf ("caseless:%d\n", G_REGEX_CASELESS);
  printf ("multiline:%d\n", G_REGEX_MULTILINE);
  printf ("dotall:%d\n", G_REGEX_DOTALL);
  printf ("extended:%d\n", G_REGEX_EXTENDED);
  printf ("anchored:%d\n", G_REGEX_ANCHORED);
  printf ("dollar-endonly:%d\n", G_REGEX_DOLLAR_ENDONLY);
  printf ("ungreedy:%d\n", G_REGEX_UNGREEDY);
  printf ("raw:%d\n", G_REGEX_RAW);
  printf ("no-auto-capture:%d\n", G_REGEX_NO_AUTO_CAPTURE);
  printf ("optimize:%d\n", G_REGEX_OPTIMIZE);
  printf ("dupnames:%d\n", G_REGEX_DUPNAMES);
  printf ("newline-cr:%d\n", G_REGEX_NEWLINE_CR);
  printf ("newline-lf:%d\n", G_REGEX_NEWLINE_LF);
  printf ("newline-crlf:%d\n", G_REGEX_NEWLINE_CRLF);

  printf ("match-anchored:%d\n", G_REGEX_MATCH_ANCHORED);
  printf ("match-notbol:%d\n", G_REGEX_MATCH_NOTBOL);
  printf ("match-noteol:%d\n", G_REGEX_MATCH_NOTEOL);
  printf ("match-notempty:%d\n", G_REGEX_MATCH_NOTEMPTY);
  printf ("match-partial:%d\n", G_REGEX_MATCH_PARTIAL);
  printf ("match-newline-cr:%d\n", G_REGEX_MATCH_NEWLINE_CR);
  printf ("match-newline-lf:%d\n", G_REGEX_MATCH_NEWLINE_LF);
  printf ("match-newline-crlf:%d\n", G_REGEX_MATCH_NEWLINE_CRLF);
  printf ("match-newline-any:%d\n", G_REGEX_MATCH_NEWLINE_ANY);

  OK_END ();
}


const command_arg_type_t cmd_search_string_spec[] =
  {
    ARG_DOC,
    ARG_NATNUM,                 /* first page */
    ARG_NATNUM,                 /* last page */
    ARG_NONEMPTY_STRING,        /* search string */
    ARG_BOOL,                   /* ignore-case */
  };

static void
cmd_search_string(const epdfinfo_t *ctx, const command_arg_t *args)
{
  PopplerDocument *doc = args[0].value.doc->pdf;
  int first = args[1].value.natnum;
  int last = args[2].value.natnum;
  const char *string = args[3].value.string;
  gboolean ignore_case = args[4].value.flag;
  GList *list, *item;
  double width, height;
  int pn;
#ifdef HAVE_POPPLER_FIND_OPTS
  PopplerFindFlags flags = ignore_case ? 0 : POPPLER_FIND_CASE_SENSITIVE;
#endif

  NORMALIZE_PAGE_ARG (doc, &first, &last);
  OK_BEGIN ();
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
          gchar *line, *match;
          PopplerRectangle *r = item->data;
          gdouble y1 =  r->y1;

          r->y1 = height - r->y2;
          r->y2 = height - y1;

          printf ("%d:", pn);
          line = strchomp (poppler_page_get_selected_text
                           (page, POPPLER_SELECTION_LINE, r));
          match = strchomp (poppler_page_get_selected_text
                           (page, POPPLER_SELECTION_GLYPH, r));
          print_response_string (match, COLON);
          print_response_string (line, COLON);
          printf ("%f %f %f %f\n",
                  r->x1 / width, r->y1 / height,
                  r->x2 / width, r->y2 / height);
          g_free (line);
          g_free (match);
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

const command_arg_type_t cmd_metadata_spec[] =
  {
    ARG_DOC,
  };

static void
cmd_metadata (const epdfinfo_t *ctx, const command_arg_t *args)
{
  PopplerDocument *doc = args[0].value.doc->pdf;
  time_t date;
  gchar *md[6];
  gchar *title;
  int i;
  char *time_str;

  OK_BEGIN ();

  title = poppler_document_get_title (doc);
  print_response_string (title, COLON);
  g_free (title);

  md[0] = poppler_document_get_author (doc);
  md[1] = poppler_document_get_subject (doc);
  md[2] = poppler_document_get_keywords (doc);
  md[3] = poppler_document_get_creator (doc);
  md[4] = poppler_document_get_producer (doc);
  md[5] = poppler_document_get_pdf_version_string (doc);

  for (i = 0; i < 6; ++i)
    {
      print_response_string (md[i], COLON);
      g_free (md[i]);
    }

  date = poppler_document_get_creation_date (doc);
  time_str = strchomp (ctime (&date));
  print_response_string (time_str ? time_str : "", COLON);
  date = poppler_document_get_modification_date (doc);
  time_str = strchomp (ctime (&date));
  print_response_string (time_str ? time_str : "", NEWLINE);
  OK_END ();
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
cmd_outline_walk (PopplerDocument *doc, PopplerIndexIter *iter, int depth)
{
  do
    {
      PopplerIndexIter *child;
      PopplerAction *action = poppler_index_iter_get_action (iter);

      if (! action)
        continue;

      if (action_is_handled (action))
        {
          printf ("%d:", depth);
          action_print (doc, action);
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

const command_arg_type_t cmd_outline_spec[] =
  {
    ARG_DOC,
  };

static void
cmd_outline (const epdfinfo_t *ctx, const command_arg_t *args)
{
  PopplerIndexIter *iter = poppler_index_iter_new (args->value.doc->pdf);
  OK_BEGIN ();
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


const command_arg_type_t cmd_quit_spec[] = {};

static void
cmd_quit (const epdfinfo_t *ctx, const command_arg_t *args)
{
  cmd_closeall (ctx, args);
  exit (EXIT_SUCCESS);
}

/* Name: number-of-pages
   Args: filename
   Returns: The number of pages.
   Errors: None
*/


const command_arg_type_t cmd_number_of_pages_spec[] =
  {
    ARG_DOC
  };

static void
cmd_number_of_pages (const epdfinfo_t *ctx, const command_arg_t *args)
{
  int npages = poppler_document_get_n_pages (args->value.doc->pdf);
  OK_BEGIN ();
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


const command_arg_type_t cmd_pagelinks_spec[] =
  {
    ARG_DOC,
    ARG_NATNUM                  /* page number */
  };

static void
cmd_pagelinks(const epdfinfo_t *ctx, const command_arg_t *args)
{
  PopplerDocument *doc = args[0].value.doc->pdf;
  PopplerPage *page = NULL;
  int pn = args[1].value.natnum;
  double width, height;
  GList *link_map = NULL, *item;

  page = poppler_document_get_page (doc, pn - 1);
  perror_if_not (page, "No such page %d", pn);
  poppler_page_get_size (page, &width, &height);
  link_map = poppler_page_get_link_mapping (page);

  OK_BEGIN ();
  for (item = g_list_last (link_map); item; item = item->prev)
    {

      PopplerLinkMapping *link = item->data;
      PopplerRectangle *r = &link->area;
      gdouble y1 = r->y1;
      /* LinkMappings have a different gravity. */
      r->y1 = height - r->y2;
      r->y2 = height - y1;

      if (! action_is_handled (link->action))
        continue;

      printf ("%f %f %f %f:",
              r->x1 / width, r->y1 / height,
              r->x2 / width, r->y2 / height);
      action_print (doc, link->action);
    }
  OK_END ();
 error:
  if (page) g_object_unref (page);
  if (link_map) poppler_page_free_link_mapping (link_map);
}

/* Name: gettext
   Args: filename page edges selection-style
   Returns: The selection's text.
   Errors: If page is out of range.

   For the selection-style argument see getselection command.
*/


const command_arg_type_t cmd_gettext_spec[] =
  {
    ARG_DOC,
    ARG_NATNUM,                 /* page number */
    ARG_EDGES,                  /* selection */
    ARG_NATNUM                  /* selection-style */
  };

static void
cmd_gettext(const epdfinfo_t *ctx, const command_arg_t *args)
{
  PopplerDocument *doc = args[0].value.doc->pdf;
  int pn = args[1].value.natnum;
  PopplerRectangle r = args[2].value.rectangle;
  int selection_style = args[3].value.natnum;
  PopplerPage *page = NULL;
  double width, height;
  gchar *text = NULL;

  switch (selection_style)
    {
    case POPPLER_SELECTION_GLYPH: break;
    case POPPLER_SELECTION_LINE: break;
    case POPPLER_SELECTION_WORD: break;
    default: selection_style = POPPLER_SELECTION_GLYPH;
    }

  page = poppler_document_get_page (doc, pn - 1);
  perror_if_not (page, "No such page %d", pn);
  poppler_page_get_size (page, &width, &height);
  r.x1 = r.x1 * width;
  r.x2 = r.x2 * width;
  r.y1 = r.y1 * height;
  r.y2 = r.y2 * height;
  /* printf ("%f %f %f %f , %f %f\n", r.x1, r.y1, r.x2, r.y2, width, height); */
  text = poppler_page_get_selected_text (page, selection_style, &r);

  OK_BEGIN ();
  print_response_string (text, NEWLINE);
  OK_END ();

 error:
  g_free (text);
  if (page) g_object_unref (page);
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


const command_arg_type_t cmd_getselection_spec[] =
  {
    ARG_DOC,
    ARG_NATNUM,                 /* page number */
    ARG_EDGES,       /* selection */
    ARG_NATNUM                  /* selection-style */
  };

static void
cmd_getselection (const epdfinfo_t *ctx, const command_arg_t *args)
{
  PopplerDocument *doc = args[0].value.doc->pdf;
  int pn = args[1].value.natnum;
  PopplerRectangle r = args[2].value.rectangle;
  int selection_style = args[3].value.natnum;
  gdouble width, height;
  cairo_region_t *region = NULL;
  PopplerPage *page = NULL;
  int i;

  switch (selection_style)
    {
    case POPPLER_SELECTION_GLYPH: break;
    case POPPLER_SELECTION_LINE: break;
    case POPPLER_SELECTION_WORD: break;
    default: selection_style = POPPLER_SELECTION_GLYPH;
    }

  page = poppler_document_get_page (doc, pn - 1);
  perror_if_not (page, "No such page %d", pn);
  poppler_page_get_size (page, &width, &height);

  r.x1 = r.x1 * width;
  r.x2 = r.x2 * width;
  r.y1 = r.y1 * height;
  r.y2 = r.y2 * height;

  region = poppler_page_get_selected_region (page, 1.0, selection_style, &r);

  OK_BEGIN ();
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

 error:
  if (region) cairo_region_destroy (region);
  if (page) g_object_unref (page);
}

/* Name: pagesize
   Args: filename page
   Returns: width height
   Errors: If page is out of range.
*/


const command_arg_type_t cmd_pagesize_spec[] =
  {
    ARG_DOC,
    ARG_NATNUM                  /* page number */
  };

static void
cmd_pagesize(const epdfinfo_t *ctx, const command_arg_t *args)
{
  PopplerDocument *doc = args[0].value.doc->pdf;
  int pn = args[1].value.natnum;
  PopplerPage *page = NULL;
  double width, height;


  page = poppler_document_get_page (doc, pn - 1);
  perror_if_not (page, "No such page %d", pn);
  poppler_page_get_size (page, &width, &height);

  OK_BEGIN ();
  printf ("%f:%f\n", width, height);
  OK_END ();

 error:
  if (page) g_object_unref (page);
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


const command_arg_type_t cmd_getannots_spec[] =
  {
    ARG_DOC,
    ARG_NATNUM,                 /* first page */
    ARG_NATNUM                  /* last page */
  };

static void
cmd_getannots(const epdfinfo_t *ctx, const command_arg_t *args)
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

  OK_BEGIN ();
  for (pn = first; pn <= last; ++pn)
    {
      GList *annots = annoation_get_for_page (args->value.doc, pn);
      PopplerPage *page = poppler_document_get_page (doc, pn - 1);

      if (! page)
        continue;

      for (list = annots; list; list = list->next)
        {
          annotation_t *annot = (annotation_t *)list->data;
          annotation_print (annot, page);
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


const command_arg_type_t cmd_getannot_spec[] =
  {
    ARG_DOC,
    ARG_NONEMPTY_STRING,        /* annotation's key */
  };

static void
cmd_getannot (const epdfinfo_t *ctx, const command_arg_t *args)
{
  document_t *doc = args->value.doc;
  const gchar *key = args[1].value.string;
  PopplerPage *page = NULL;
  annotation_t *a = annotation_get_by_key (doc, key);
  gint index;

  perror_if_not (a, "No such annotation: %s", key);
  index = poppler_annot_get_page_index (a->amap->annot);
  if (index >= 0)
    page = poppler_document_get_page (doc->pdf, index);
  perror_if_not (page, "Unable to get page %d", index + 1);

  OK_BEGIN ();
  annotation_print (a, page);
  OK_END ();

 error:
  if (page) g_object_unref (page);
}

/* Name: getannot_attachment
   Args: filename name [output-filename]
   Returns: name description size mtime ctime output-filename
   Errors: If no annotation named ,name' exists or output-filename is
   not writable.
*/


const command_arg_type_t cmd_getattachment_from_annot_spec[] =
  {
    ARG_DOC,
    ARG_NONEMPTY_STRING,        /* annotation's name */
    ARG_BOOL                    /* save attachment */
  };

static void
cmd_getattachment_from_annot (const epdfinfo_t *ctx, const command_arg_t *args)
{
  document_t *doc = args->value.doc;
  const gchar *key = args[1].value.string;
  gboolean do_save = args[2].value.flag;
  PopplerAttachment *att = NULL;
  annotation_t *a = annotation_get_by_key (doc, key);
  gchar *id = NULL;

  perror_if_not (a, "No such annotation: %s", key);
  perror_if_not (POPPLER_IS_ANNOT_FILE_ATTACHMENT (a->amap->annot),
                "Not a file annotation: %s", key);
  att = poppler_annot_file_attachment_get_attachment
        (POPPLER_ANNOT_FILE_ATTACHMENT (a->amap->annot));
  perror_if_not (att, "Unable to get attachment: %s", key);
  id = g_strdup_printf ("attachment-%s", key);

  OK_BEGIN ();
  attachment_print (att, id, do_save);
  OK_END ();

 error:
  if (att) g_object_unref (att);
  if (id) g_free (id);
}


/* document-level attachments */
const command_arg_type_t cmd_getattachments_spec[] =
  {
    ARG_DOC,
    ARG_BOOL,        /* save attachments */
  };

static void
cmd_getattachments (const epdfinfo_t *ctx, const command_arg_t *args)
{
  document_t *doc = args->value.doc;
  gboolean do_save = args[1].value.flag;
  GList *item;
  GList *attmnts = poppler_document_get_attachments (doc->pdf);
  int i;

  OK_BEGIN ();
  for (item = attmnts, i = 0; item; item = item->next, ++i)
    {
      PopplerAttachment *att = (PopplerAttachment*) item->data;
      gchar *id = g_strdup_printf ("attachment-document-%d", i);

      attachment_print (att, id, do_save);
      g_object_unref (att);
      g_free (id);
    }
  g_list_free (attmnts);

  OK_END ();
}

#ifdef HAVE_POPPLER_ANNOT_WRITE

const command_arg_type_t cmd_addannot_spec[] =
  {
    ARG_DOC,
    ARG_NATNUM,                 /* page number */
    ARG_STRING,                 /* type */
    ARG_EDGES_OR_POSITION,      /* edges or position (uses default size) */
    ARG_REST,                  /* markup regions */
  };

static void
cmd_addannot (const epdfinfo_t *ctx, const command_arg_t *args)
{

  document_t *doc = args->value.doc;
  gint pn = args[1].value.natnum;
  const char *type_string = args[2].value.string;
  PopplerRectangle r = args[3].value.rectangle;
  int i;
  PopplerPage *page = NULL;
  double width, height;
  PopplerAnnot *pa;
  PopplerAnnotMapping *amap;
  annotation_t *a;
  gchar *key;
  GList *annotations;
  gdouble y2;
  char *error_msg = NULL;

  page = poppler_document_get_page (doc->pdf, pn - 1);
  perror_if_not (page, "Unable to get page %d", pn);
  poppler_page_get_size (page, &width, &height);
  r.x1 *= width; r.x2 *= width;
  r.y1 *= height; r.y2 *= height;
  if (r.y2 < 0)
    r.y2 = r.y1 + 24;
  if (r.x2 < 0)
    r.x2 = r.x1 + 24;
  y2 = r.y2;
  r.y2 = height - r.y1;
  r.y1 = height - y2;

  pa = annotation_new (ctx, doc, page, type_string, &r, &args[4], &error_msg);
  perror_if_not (pa, "Creating annotation failed: %s",
                 error_msg ? error_msg : "Reason unknown");
  amap = poppler_annot_mapping_new ();
  amap->area = r;
  amap->annot = pa;
  annotations = annoation_get_for_page (doc, pn);

  i = g_list_length (annotations);
  key = g_strdup_printf ("annot-%d-%d", pn, i);
  while (g_hash_table_lookup (doc->annotations.keys, key))
    {
      g_free (key);
      key = g_strdup_printf ("annot-%d-%d", pn, ++i);
    }
  a = g_malloc (sizeof (annotation_t));
  a->amap = amap;
  a->key = key;
  doc->annotations.pages[pn - 1] =
    g_list_prepend (annotations, a);
  g_hash_table_insert (doc->annotations.keys, key, a);
  poppler_page_add_annot (page, pa);
  OK_BEGIN ();
  annotation_print (a, page);
  OK_END ();

 error:
  if (page) g_object_unref (page);
  if (error_msg) g_free (error_msg);
}


const command_arg_type_t cmd_delannot_spec[] =
  {
    ARG_DOC,
    ARG_NONEMPTY_STRING         /* Annotation's key */
  };

static void
cmd_delannot (const epdfinfo_t *ctx, const command_arg_t *args)
{
  document_t *doc = args->value.doc;
  const gchar *key = args[1].value.string;
  PopplerPage *page = NULL;
  annotation_t *a = annotation_get_by_key (doc, key);
  gint pn;

  perror_if_not (a, "No such annotation: %s", key);
  pn = poppler_annot_get_page_index (a->amap->annot) + 1;
  if (pn >= 1)
    page = poppler_document_get_page (doc->pdf, pn - 1);
  perror_if_not (page, "Unable to get page %d", pn);
  poppler_page_remove_annot (page, a->amap->annot);
  doc->annotations.pages[pn - 1] =
    g_list_remove (doc->annotations.pages[pn - 1], a);
  g_hash_table_remove (doc->annotations.keys, a->key);
  poppler_annot_mapping_free(a->amap);
  OK ();

 error:
  if (a)
    {
      g_free (a->key);
      g_free (a);
    }
  if (page) g_object_unref (page);
}

const command_arg_type_t cmd_editannot_spec[] =
  {
    ARG_DOC,
    ARG_NONEMPTY_STRING,        /* annotation key */
    ARG_REST                    /* (KEY VALUE ...) */
  };

static void
cmd_editannot (const epdfinfo_t *ctx, const command_arg_t *args)
{
  document_t *doc = args->value.doc;
  const char *key = args[1].value.string;
  int nrest_args = args[2].value.rest.nargs;
  char * const *rest_args = args[2].value.rest.args;
  annotation_t *a = annotation_get_by_key (doc, key);
  PopplerAnnot *pa;
  PopplerPage *page = NULL;
  int i = 0;
  gint index;
  char *error_msg = NULL;
  command_arg_t carg;
  const char *unexpected_parse_error = "Internal error while parsing arg `%s'";

  perror_if_not (a, "No such annotation: %s", key);
  pa = a->amap->annot;
  perror_if_not (annotation_edit_validate (ctx, &args[2], pa, &error_msg),
                 "%s", error_msg);
  index = poppler_annot_get_page_index (pa);
  page = poppler_document_get_page (doc->pdf, index);
  perror_if_not (page, "Unable to get page %d for annotation", index);

  for (i = 0; i < nrest_args; ++i)
    {
      const char *key = rest_args[i++];

      if (! strcmp (key, "flags"))
        {
          perror_if_not (command_arg_parse_arg  (ctx, rest_args[i], &carg,
                                                 ARG_NATNUM, NULL),
                         unexpected_parse_error, rest_args[i]);
          poppler_annot_set_flags (pa, carg.value.natnum);
        }
      else if (! strcmp (key, "color"))
        {
          perror_if_not (command_arg_parse_arg  (ctx, rest_args[i], &carg,
                                                 ARG_COLOR, NULL),
                         unexpected_parse_error, rest_args[i]);
          poppler_annot_set_color (pa, &carg.value.color);
        }
      else if (! strcmp (key, "contents"))
        {
          perror_if_not (command_arg_parse_arg  (ctx, rest_args[i], &carg,
                                                 ARG_STRING, NULL),
                         unexpected_parse_error, rest_args[i]);
          poppler_annot_set_contents (pa, carg.value.string);
        }
      else if (! strcmp (key, "edges"))
        {
          PopplerRectangle *area = &a->amap->area;
          gdouble width, height;
          PopplerRectangle r;

          perror_if_not (command_arg_parse_arg  (ctx, rest_args[i], &carg,
                                                 ARG_EDGES_OR_POSITION, NULL),
                         unexpected_parse_error, rest_args[i]);
          r = carg.value.rectangle;
          poppler_page_get_size (page, &width, &height);

          /* Translate Gravity and maybe keep the width and height. */
          if (r.x2 < 0)
            area->x2 +=  (r.x1 * width) - area->x1;
          else
            area->x2 = r.x2 * width;

          if (r.y2 < 0)
            area->y1 -=  (r.y1 * height) - (height - area->y2);
          else
            area->y1 = height - (r.y2 * height);

          area->x1 = r.x1 * width;
          area->y2 = height - (r.y1 * height);

          xpoppler_annot_set_rectangle (pa, area);
        }
      else if (! strcmp (key, "label"))
        {
          PopplerAnnotMarkup *ma = POPPLER_ANNOT_MARKUP (pa);
          perror_if_not (command_arg_parse_arg  (ctx, rest_args[i], &carg,
                                                 ARG_STRING, NULL),
                         unexpected_parse_error, rest_args[i]);
          poppler_annot_markup_set_label (ma, carg.value.string);
        }
      else if (! strcmp (key, "opacity"))
        {
          PopplerAnnotMarkup *ma = POPPLER_ANNOT_MARKUP (pa);
          perror_if_not (command_arg_parse_arg  (ctx, rest_args[i], &carg,
                                                 ARG_EDGE, NULL),
                         unexpected_parse_error, rest_args[i]);
          poppler_annot_markup_set_opacity (ma, carg.value.edge);
        }
      else if (! strcmp (key, "popup"))
        {
          PopplerAnnotMarkup *ma = POPPLER_ANNOT_MARKUP (pa);
          perror_if_not (command_arg_parse_arg  (ctx, rest_args[i], &carg,
                                                 ARG_EDGES, NULL),
                         unexpected_parse_error, rest_args[i]);
          poppler_annot_markup_set_popup (ma, &carg.value.rectangle);
        }
      else if (! strcmp (key, "popup-is-open"))
        {
          PopplerAnnotMarkup *ma = POPPLER_ANNOT_MARKUP (pa);
          perror_if_not (command_arg_parse_arg  (ctx, rest_args[i], &carg,
                                                 ARG_BOOL, NULL),
                         unexpected_parse_error, rest_args[i]);
          poppler_annot_markup_set_popup_is_open (ma, carg.value.flag);
        }
      else if (! strcmp (key, "icon"))
        {
          PopplerAnnotText *ta = POPPLER_ANNOT_TEXT (pa);
          perror_if_not (command_arg_parse_arg  (ctx, rest_args[i], &carg,
                                                 ARG_STRING, NULL),
                         unexpected_parse_error, rest_args[i]);
          poppler_annot_text_set_icon (ta, carg.value.string);
        }
      else if (! strcmp (key, "is-open"))
        {
          PopplerAnnotText *ta = POPPLER_ANNOT_TEXT (pa);
          perror_if_not (command_arg_parse_arg  (ctx, rest_args[i], &carg,
                                                 ARG_BOOL, NULL),
                         unexpected_parse_error, rest_args[i]);
          poppler_annot_text_set_is_open (ta, carg.value.flag);
        }
      else
        {
          perror_if_not (0, "internal error: annotation property validation failed");
        }
    }

  OK_BEGIN ();
  annotation_print (a, page);
  OK_END ();

 error:
  if (error_msg) g_free (error_msg);
  if (page) g_object_unref (page);
}

const command_arg_type_t cmd_save_spec[] =
  {
    ARG_DOC,
  };

static void
cmd_save (const epdfinfo_t *ctx, const command_arg_t *args)
{
  document_t *doc = args->value.doc;
  char *filename = mktempfile ();
  GError *gerror = NULL;
  gchar *uri;
  gboolean success = FALSE;

  if (!filename)
    {
      printf_error_response ("Unable to create temporary file");
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
      printf_error_response ("Error while saving %s:%s"
                    , filename, gerror ? gerror->message : "?");
      if (gerror)
        g_error_free (gerror);
      return;
    }
  OK_BEGIN ();
  print_response_string (filename, NEWLINE);
  OK_END ();
}

#endif  /* HAVE_POPPLER_ANNOT_WRITE */


const command_arg_type_t cmd_synctex_forward_search_spec[] =
  {
    ARG_DOC,
    ARG_NONEMPTY_STRING,        /* source file */
    ARG_NATNUM,                 /* line number */
    ARG_NATNUM                  /* column number */
  };

static void
cmd_synctex_forward_search (const epdfinfo_t *ctx, const command_arg_t *args)
{
  document_t *doc = args[0].value.doc;
  const char *source = args[1].value.string;
  int line = args[2].value.natnum;
  int column = args[3].value.natnum;
  synctex_scanner_p scanner = NULL;
  synctex_node_p node;
  float x1, y1, x2, y2;
  PopplerPage *page = NULL;
  double width, height;
  int pn;

  scanner = synctex_scanner_new_with_output_file (doc->filename, NULL, 1);
  perror_if_not (scanner, "Unable to create synctex scanner,\
 did you run latex with `--synctex=1' ?");

  perror_if_not (synctex_display_query (scanner, source, line, column, 0)
                && (node = synctex_scanner_next_result (scanner)),
                "Destination not found");

  pn = synctex_node_page (node);
  page = poppler_document_get_page(doc->pdf, pn - 1);
  perror_if_not (page, "Page not found");
  x1 =  synctex_node_box_visible_h (node);
  y1 =  synctex_node_box_visible_v (node)
        - synctex_node_box_visible_height (node);
  x2 = synctex_node_box_visible_width (node) + x1;
  y2 = synctex_node_box_visible_depth (node)
       + synctex_node_box_visible_height (node) + y1;
  poppler_page_get_size (page, &width, &height);
  x1 /= width;
  y1 /= height;
  x2 /= width;
  y2 /= height;

  OK_BEGIN ();
  printf("%d:%f:%f:%f:%f\n", pn, x1, y1, x2, y2);
  OK_END ();

 error:
  if (page) g_object_unref (page);
  if (scanner) synctex_scanner_free (scanner);
}


const command_arg_type_t cmd_synctex_backward_search_spec[] =
  {
    ARG_DOC,
    ARG_NATNUM,                 /* page number */
    ARG_EDGE,                   /* x */
    ARG_EDGE                    /* y */
  };

static void
cmd_synctex_backward_search (const epdfinfo_t *ctx, const command_arg_t *args)
{
  document_t *doc = args[0].value.doc;
  int pn = args[1].value.natnum;
  double x = args[2].value.edge;
  double y = args[3].value.edge;
  synctex_scanner_p scanner = NULL;
  const char *filename;
  PopplerPage *page = NULL;
  synctex_node_p node;
  double width, height;
  int line, column;

  scanner = synctex_scanner_new_with_output_file (doc->filename, NULL, 1);
  perror_if_not (scanner, "Unable to create synctex scanner,\
 did you run latex with `--synctex=1' ?");

  page = poppler_document_get_page(doc->pdf, pn - 1);
  perror_if_not (page, "Page not found");
  poppler_page_get_size (page, &width, &height);
  x = x * width;
  y = y * height;

  if (! synctex_edit_query (scanner, pn, x, y)
      || ! (node = synctex_scanner_next_result (scanner))
      || ! (filename =
            synctex_scanner_get_name (scanner, synctex_node_tag (node))))
    {
      printf_error_response ("Destination not found");
      goto error;
    }

  line = synctex_node_line (node);
  column = synctex_node_column (node);

  OK_BEGIN ();
  print_response_string (filename, COLON);
  printf("%d:%d\n", line, column);
  OK_END ();

 error:
  if (page) g_object_unref (page);
  if (scanner) synctex_scanner_free (scanner);
}


const command_arg_type_t cmd_renderpage_spec[] =
  {
    ARG_DOC,
    ARG_NATNUM,                 /* page number */
    ARG_NATNUM,                 /* width */
    ARG_REST,                   /* commands */
  };

static void
cmd_renderpage (const epdfinfo_t *ctx, const command_arg_t *args)
{
  document_t *doc = args[0].value.doc;
  int pn = args[1].value.natnum;
  int width = args[2].value.natnum;
  int nrest_args = args[3].value.rest.nargs;
  char * const *rest_args = args[3].value.rest.args;
  PopplerPage *page = poppler_document_get_page(doc->pdf, pn - 1);
  cairo_surface_t *surface = NULL;
  cairo_t *cr = NULL;
  command_arg_t rest_arg;
  gchar *error_msg = NULL;
  double pt_width, pt_height;
  PopplerColor fg = { 0, 0, 0 };
  PopplerColor bg = { 65535, 0, 0 };
  double alpha = 1.0;
  double line_width = 1.5;
  PopplerRectangle cb = {0.0, 0.0, 1.0, 1.0};
  int i = 0;

  perror_if_not (page, "No such page %d", pn);
  poppler_page_get_size (page, &pt_width, &pt_height);
  surface = image_render_page (doc->pdf, page, width, 1,
                               &doc->options.render);
  perror_if_not (surface, "Failed to render page %d", pn);

  if (! nrest_args)
    goto theend;

  cr = cairo_create (surface);
  cairo_scale (cr, width / pt_width, width / pt_width);

  while (i < nrest_args)
    {
      const char* keyword;

      perror_if_not (command_arg_parse_arg (ctx, rest_args[i], &rest_arg,
                                            ARG_STRING, &error_msg),
                     "%s", error_msg);
      keyword = rest_arg.value.string;
      ++i;

      perror_if_not (i < nrest_args, "Keyword is `%s' missing an argument",
                     keyword);

      if (! strcmp (keyword, ":foreground")
          || ! strcmp (keyword, ":background"))
        {
          perror_if_not (command_arg_parse_arg (ctx, rest_args[i], &rest_arg,
                                                ARG_COLOR, &error_msg),
                         "%s", error_msg);
          ++i;
          if (! strcmp (keyword, ":foreground"))
            fg = rest_arg.value.color;
          else
            bg = rest_arg.value.color;

        }
      else if (! strcmp (keyword, ":alpha"))
        {
          perror_if_not (command_arg_parse_arg (ctx, rest_args[i], &rest_arg,
                                                ARG_EDGE, &error_msg),
                         "%s", error_msg);
          ++i;
          alpha = rest_arg.value.edge;
        }
      else if (! strcmp (keyword, ":crop-to")
               || ! strcmp (keyword, ":highlight-region")
               || ! strcmp (keyword, ":highlight-text")
               || ! strcmp (keyword, ":highlight-line"))
        {
          PopplerRectangle *r;
          perror_if_not (command_arg_parse_arg (ctx, rest_args[i], &rest_arg,
                                                ARG_EDGES, &error_msg),
                         "%s", error_msg);

          ++i;
          r = &rest_arg.value.rectangle;

          if (! strcmp (keyword, ":crop-to"))
            {
              gdouble w = (cb.x2 - cb.x1);
              gdouble h = (cb.y2 - cb.y1);
              gdouble x1 = cb.x1;
              gdouble y1 = cb.y1;

              cb.x1 = r->x1 * w + x1;
              cb.x2 = r->x2 * w + x1;
              cb.y1 = r->y1 * h + y1;
              cb.y2 = r->y2 * h + y1;

            }
          else
            {
              r->x1 = pt_width * r->x1 * (cb.x2 - cb.x1) + pt_width * cb.x1;
              r->x2 = pt_width * r->x2 * (cb.x2 - cb.x1) + pt_width * cb.x1;
              r->y1 = pt_height * r->y1 * (cb.y2 - cb.y1) + pt_height * cb.y1;
              r->y2 = pt_height * r->y2 * (cb.y2 - cb.y1) + pt_height * cb.y1;

              if (! strcmp (keyword, ":highlight-region"))
                {
                  const double deg = M_PI / 180.0;
                  double rad;

                  r->x1 += line_width / 2;
                  r->x2 -= line_width / 2;
                  r->y1 += line_width / 2;
                  r->y2 -= line_width / 2;

                  rad = MIN (5, MIN (r->x2 - r->x1, r->y2 - r->y1) / 2.0);

                  cairo_move_to (cr, r->x1 , r->y1 + rad);
                  cairo_arc (cr, r->x1 + rad, r->y1 + rad, rad, 180 * deg, 270 * deg);
                  cairo_arc (cr, r->x2 - rad, r->y1 + rad, rad, 270 * deg, 360 * deg);
                  cairo_arc (cr, r->x2 - rad, r->y2 - rad, rad, 0 * deg, 90 * deg);
                  cairo_arc (cr, r->x1 + rad, r->y2 - rad, rad, 90 * deg, 180 * deg);
                  cairo_close_path (cr);

                  cairo_set_source_rgba (cr,
                                         bg.red / 65535.0,
                                         bg.green / 65535.0,
                                         bg.blue / 65535.0, alpha);
                  cairo_fill_preserve (cr);
                  cairo_set_source_rgba (cr,
                                         fg.red / 65535.0,
                                         fg.green / 65535.0,
                                         fg.blue / 65535.0, 1.0);
                  cairo_set_line_width (cr, line_width);
                  cairo_stroke (cr);
                }
              else
                {
                  gboolean is_single_line = ! strcmp (keyword, ":highlight-line");

                  if (is_single_line)
                    {
                      gdouble m = r->y1 + (r->y2 - r->y1) / 2;

                      /* Make the rectangle flat, otherwise poppler frequently
                         renders neighboring lines.*/
                      r->y1 = m;
                      r->y2 = m;
                    }

                  poppler_page_render_selection (page, cr, r, NULL,
                                                 POPPLER_SELECTION_GLYPH, &fg, &bg);
                }
            }
        }
      else
        perror_if_not (0, "Unknown render command: %s", keyword);
    }
  if (cb.x1 != 0 || cb.y1 != 0 || cb.x2 != 1 || cb.y2 != 1)
    {
      int height = cairo_image_surface_get_height (surface);
      cairo_rectangle_int_t r = {(int) (width * cb.x1 + 0.5),
                                 (int) (height * cb.y1 + 0.5),
                                 (int) (width * (cb.x2 - cb.x1) + 0.5),
                                 (int) (height * (cb.y2 - cb.y1) + 0.5)};
      cairo_surface_t *nsurface =
        cairo_image_surface_create (CAIRO_FORMAT_ARGB32, r.width, r.height);
      perror_if_not (cairo_surface_status (surface) == CAIRO_STATUS_SUCCESS,
                     "%s", "Failed to create cairo surface");
      cairo_destroy (cr);
      cr = cairo_create (nsurface);
      perror_if_not (cairo_status (cr) == CAIRO_STATUS_SUCCESS,
                     "%s", "Failed to create cairo context");
      cairo_set_source_surface (cr, surface, -r.x, -r.y);
      cairo_paint (cr);
      cairo_surface_destroy (surface);
      surface = nsurface;
    }

 theend:
  image_write_print_response (surface, PNG);

 error:
  if (error_msg) g_free (error_msg);
  if (cr) cairo_destroy (cr);
  if (surface) cairo_surface_destroy (surface);
  if (page) g_object_unref (page);
}

const command_arg_type_t cmd_boundingbox_spec[] =
  {
    ARG_DOC,
    ARG_NATNUM,                 /* page number */
    /* region */
  };

static void
cmd_boundingbox (const epdfinfo_t *ctx, const command_arg_t *args)
{
  document_t *doc = args[0].value.doc;
  int pn = args[1].value.natnum;
  PopplerPage *page = poppler_document_get_page(doc->pdf, pn - 1);
  cairo_surface_t *surface = NULL;
  int width, height;
  double pt_width, pt_height;
  unsigned char *data, *data_p;
  PopplerRectangle bbox;
  int i, j;

  perror_if_not (page, "No such page %d", pn);
  poppler_page_get_size (page, &pt_width, &pt_height);
  surface = image_render_page (doc->pdf, page, (int) pt_width, 1,
                               &doc->options.render);

  perror_if_not (cairo_surface_status(surface) == CAIRO_STATUS_SUCCESS,
                "Failed to render page");

  width = cairo_image_surface_get_width (surface);
  height = cairo_image_surface_get_height (surface);
  data = cairo_image_surface_get_data (surface);

  /* Determine the bbox by comparing each pixel in the 4 corner
     stripes with the origin. */
  for (i = 0; i < width; ++i)
    {
      data_p = data + 4 * i;
      for (j = 0; j < height; ++j, data_p += 4 * width)
        {
          if (! ARGB_EQUAL (data, data_p))
            break;
        }
      if (j < height)
        break;
    }
  bbox.x1 = i;

  for (i = width - 1; i > -1; --i)
    {
      data_p = data + 4 * i;
      for (j = 0; j < height; ++j, data_p += 4 * width)
        {
          if (! ARGB_EQUAL (data, data_p))
            break;
        }
      if (j < height)
        break;
    }
  bbox.x2 = i + 1;

  for (i = 0; i < height; ++i)
    {
      data_p = data + 4 * i * width;
      for (j = 0; j < width; ++j, data_p += 4)
        {
          if (! ARGB_EQUAL (data, data_p))
            break;
        }
      if (j < width)
        break;
    }
  bbox.y1 = i;

  for (i = height - 1; i > -1; --i)
    {
      data_p = data + 4 * i * width;
      for (j = 0; j < width; ++j, data_p += 4)
        {
          if (! ARGB_EQUAL (data, data_p))
            break;
        }
      if (j < width)
        break;
    }
  bbox.y2 = i + 1;

  OK_BEGIN ();
  if (bbox.x1 >= bbox.x2 || bbox.y1 >= bbox.y2)
    {
      /* empty page */
      puts ("0:0:1:1");
    }
  else
    {
      printf ("%f:%f:%f:%f\n",
              bbox.x1 / width,
              bbox.y1 / height,
              bbox.x2 / width,
              bbox.y2 / height);
    }
  OK_END ();

 error:
  if (surface) cairo_surface_destroy (surface);
  if (page) g_object_unref (page);
}

const command_arg_type_t cmd_charlayout_spec[] =
  {
    ARG_DOC,
    ARG_NATNUM,                 /* page number */
    ARG_EDGES_OR_POSITION,      /* region or position */
  };

static void
cmd_charlayout(const epdfinfo_t *ctx, const command_arg_t *args)
{
  PopplerDocument *doc = args[0].value.doc->pdf;
  int pn = args[1].value.natnum;
  PopplerRectangle region = args[2].value.rectangle;
  double width, height;
  PopplerPage *page = poppler_document_get_page(doc, pn - 1);
  char *text = NULL;
  char *text_p;
  PopplerRectangle *rectangles = NULL;
  guint nrectangles;
  int i;
  gboolean have_position = region.y2 < 0;

  perror_if_not (page, "No such page %d", pn);

  text = poppler_page_get_text (page);
  text_p = text;
  poppler_page_get_text_layout (page, &rectangles, &nrectangles);
  poppler_page_get_size (page, &width, &height);
  region.x1 *= width;
  region.x2 *= width;
  region.y1 *= height;
  region.y2 *= height;

  OK_BEGIN ();
  for (i = 0; i < nrectangles && *text_p; ++i)
    {
      PopplerRectangle *r = &rectangles[i];
      char *nextc = g_utf8_offset_to_pointer (text_p, 1);

      if ((have_position
           && region.x1 >= r->x1
           && region.x1 <= r->x2
           && region.y1 >= r->y1
           && region.y1 <= r->y2)
          || (! have_position
              && r->x1 >= region.x1
              && r->y1 >= region.y1
              && r->x2 <= region.x2
              && r->y2 <= region.y2))
        {
          char endc = *nextc;

          printf ("%f %f %f %f:",
                  r->x1 / width, r->y1 / height,
                  r->x2 / width, r->y2 / height);
          *nextc = '\0';
          print_response_string (text_p, NEWLINE);
          *nextc = endc;
        }
      text_p = nextc;
    }
  OK_END ();

  g_free (rectangles);
  g_object_unref (page);
  g_free (text);

 error:
  return;
}

const document_option_t document_options [] =
  {
    DEC_DOPT (":render/usecolors", ARG_BOOL, render.usecolors),
    DEC_DOPT (":render/printed", ARG_BOOL, render.printed),
    DEC_DOPT (":render/foreground", ARG_COLOR, render.fg),
    DEC_DOPT (":render/background", ARG_COLOR, render.bg),
  };

const command_arg_type_t cmd_getoptions_spec[] =
  {
    ARG_DOC,
  };

static void
cmd_getoptions(const epdfinfo_t *ctx, const command_arg_t *args)
{
  document_t *doc = args[0].value.doc;
  int i;
  OK_BEGIN ();
  for (i = 0; i < G_N_ELEMENTS (document_options); ++i)
    {
      command_arg_t arg;

      arg.type = document_options[i].type;
      memcpy (&arg.value,
              ((char*) &doc->options) + document_options[i].offset,
              command_arg_type_size (arg.type));
      print_response_string (document_options[i].name, COLON);
      command_arg_print (&arg);
      puts("");
    }
  OK_END ();
}

const command_arg_type_t cmd_setoptions_spec[] =
  {
    ARG_DOC,
    ARG_REST                    /* key value pairs */
  };

static void
cmd_setoptions(const epdfinfo_t *ctx, const command_arg_t *args)
{
  int i = 0;
  document_t *doc = args[0].value.doc;
  int nrest = args[1].value.rest.nargs;
  char * const *rest = args[1].value.rest.args;
  gchar *error_msg = NULL;
  document_options_t opts = doc->options;
  const size_t nopts = G_N_ELEMENTS (document_options);

  perror_if_not (nrest % 2 == 0, "Even number of key/value pairs expected");

  while (i < nrest)
    {
      int j;
      command_arg_t key, value;

      perror_if_not (command_arg_parse_arg
                     (ctx, rest[i], &key, ARG_NONEMPTY_STRING, &error_msg),
                     "%s", error_msg);

      ++i;
      for (j = 0; j < nopts; ++j)
        {
          const document_option_t *dopt = &document_options[j];
          if (! strcmp (key.value.string, dopt->name))
            {
              perror_if_not (command_arg_parse_arg
                             (ctx, rest[i], &value, dopt->type, &error_msg),
                             "%s", error_msg);
              memcpy (((char*) &opts) + dopt->offset,
                      &value.value, command_arg_type_size (value.type));
              break;
            }
        }
      perror_if_not (j < nopts, "Unknown option: %s", key.value.string);
      ++i;
    }
  doc->options = opts;
  cmd_getoptions (ctx, args);

 error:
  if (error_msg) g_free (error_msg);
}

const command_arg_type_t cmd_pagelabels_spec[] =
  {
    ARG_DOC,
  };

static void
cmd_pagelabels(const epdfinfo_t *ctx, const command_arg_t *args)
{
  PopplerDocument *doc = args[0].value.doc->pdf;
  int i;

  OK_BEGIN ();
  for (i = 0; i < poppler_document_get_n_pages (doc); ++i)
    {
      PopplerPage *page = poppler_document_get_page(doc, i);
      gchar *label = poppler_page_get_label (page);

      print_response_string (label ? label : "", NEWLINE);
      g_object_unref (page);
      g_free (label);
    }
  OK_END ();
}

const command_arg_type_t cmd_ping_spec[] =
  {
    ARG_STRING                  /* any message */
  };

static void
cmd_ping (const epdfinfo_t *ctx, const command_arg_t *args)
{
  const gchar *msg = args[0].value.string;
  OK_BEGIN ();
  print_response_string (msg, NEWLINE);
  OK_END ();
}


/* ================================================================== *
 * Main
 * ================================================================== */

static const command_t commands [] =
  {
    /* Basic */
    DEC_CMD (ping),
    DEC_CMD (features),
    DEC_CMD (open),
    DEC_CMD (close),
    DEC_CMD (quit),
    DEC_CMD (getoptions),
    DEC_CMD (setoptions),

    /* Searching */
    DEC_CMD2 (search_string, "search-string"),
    DEC_CMD2 (search_regexp, "search-regexp"),
    DEC_CMD2 (regexp_flags, "regexp-flags"),

    /* General Information */
    DEC_CMD (metadata),
    DEC_CMD (outline),
    DEC_CMD2 (number_of_pages, "number-of-pages"),
    DEC_CMD (pagelinks),
    DEC_CMD (gettext),
    DEC_CMD (getselection),
    DEC_CMD (pagesize),
    DEC_CMD (boundingbox),
    DEC_CMD (charlayout),

    /* General Information */
    DEC_CMD (metadata),
    DEC_CMD (outline),
    DEC_CMD2 (number_of_pages, "number-of-pages"),
    DEC_CMD (pagelinks),
    DEC_CMD (gettext),
    DEC_CMD (getselection),
    DEC_CMD (pagesize),
    DEC_CMD (boundingbox),
    DEC_CMD (charlayout),
    DEC_CMD (pagelabels),

    /* Annotations */
    DEC_CMD (getannots),
    DEC_CMD (getannot),
#ifdef HAVE_POPPLER_ANNOT_WRITE
    DEC_CMD (addannot),
    DEC_CMD (delannot),
    DEC_CMD (editannot),
    DEC_CMD (save),
#endif

    /* Attachments */
    DEC_CMD2 (getattachment_from_annot, "getattachment-from-annot"),
    DEC_CMD (getattachments),

    /* Synctex */
    DEC_CMD2 (synctex_forward_search, "synctex-forward-search"),
    DEC_CMD2 (synctex_backward_search, "synctex-backward-search"),

    /* Rendering */
    DEC_CMD (renderpage),
  };

int main(int argc, char **argv)
{
  epdfinfo_t ctx = {0};
  char *line = NULL;
  ssize_t read;
  size_t line_size;
  const char *error_log = "/dev/null";

#ifdef __MINGW32__
  error_log = "NUL";
  _setmode(_fileno(stdin), _O_BINARY);
  _setmode(_fileno(stdout), _O_BINARY);
#endif

  if (argc > 2)
    {
      fprintf(stderr, "usage: epdfinfo [ERROR-LOGFILE]\n");
      exit (EXIT_FAILURE);
    }
  if (argc == 2)
    error_log = argv[1];

  if (! freopen (error_log, "a", stderr))
    err (2, "Unable to redirect stderr");

#if ! GLIB_CHECK_VERSION(2,36,0)
  g_type_init ();
#endif

  ctx.documents = g_hash_table_new (g_str_hash, g_str_equal);

  setvbuf (stdout, NULL, _IOFBF, BUFSIZ);

  while ((read = getline (&line, &line_size, stdin)) != -1)
    {
      int nargs = 0;
      command_arg_t *cmd_args = NULL;
      char **args = NULL;
      gchar *error_msg = NULL;
      int i;

      if (read <= 1 || line[read - 1] != '\n')
        {
          fprintf (stderr, "Skipped parts of a line: `%s'\n", line);
          goto next_line;
        }

      line[read - 1] = '\0';
      args = command_arg_split (line, &nargs);
      if (nargs == 0)
        continue;

      for (i = 0; i < G_N_ELEMENTS (commands);  i++)
        {
          if (! strcmp (commands[i].name, args[0]))
            {
              if (commands[i].nargs == 0
                  || (cmd_args = command_arg_parse (&ctx, args + 1, nargs - 1,
                                                    commands + i, &error_msg)))
                {
                  commands[i].execute (&ctx, cmd_args);
                  if (commands[i].nargs > 0)
                    free_command_args (cmd_args, commands[i].nargs);
                }
              else
                {
                  printf_error_response ("%s", error_msg ? error_msg :
                                         "Unknown error (this is a bug)");
                }
              if (error_msg)
                g_free (error_msg);
              break;
            }
        }
      if (G_N_ELEMENTS (commands) == i)
        {
          printf_error_response ("Unknown command: %s", args[0]);
        }
      for (i = 0; i < nargs; ++i)
        g_free (args[i]);
      g_free (args);
    next_line:
      free (line);
      line = NULL;
    }

  if (ferror (stdin))
    err (2, NULL);
  exit (EXIT_SUCCESS);
}
