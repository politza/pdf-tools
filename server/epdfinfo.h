// Copyright (C) 2013, 2014  Andreas Politz

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

#ifndef _EPDF_H_
#define _EPDF_H_ _EPDF_H_
#include "config.h"
#include <glib.h>
#include <poppler.h>
#include <png.h>

/* Some library functions print warnings to stdout, inhibit it. */
#define DISCARD_STDOUT(saved_fd)                \
  do {                                          \
    int fd;                                     \
    fflush(stdout);                             \
    saved_fd = dup(1);                          \
    fd = open("/dev/null", O_WRONLY);           \
    dup2(fd, 1);                                \
    close(fd);                                  \
  } while (0)

#define UNDISCARD_STDOUT(saved_fd)              \
  do {                                          \
    fflush(stdout);                             \
    dup2(saved_fd, 1);                          \
    close(saved_fd);                            \
  } while (0)

/* Writing responses */
#define OK_BEGIN()                              \
  do {                                          \
    puts("OK");                                 \
  } while (0)

#define OK_END()                                \
  do {                                          \
    puts(".");                                  \
    fflush (stdout);                            \
  } while (0)

#define OK()                                    \
  do {                                          \
    puts ("OK\n.");                             \
    fflush (stdout);                            \
  } while (0)

/* Dealing with image data. */
#ifdef WORDS_BIGENDIAN
#define ARGB_TO_RGB(rgb, argb)                  \
  do {                                          \
    rgb[0] = argb[1];                           \
    rgb[1] = argb[2];                           \
    rgb[2] = argb[3];                           \
  } while (0)

#define ARGB_EQUAL(argb1, argb2)                \
  (argb1[1] == argb2[1]                         \
   && argb1[2] == argb2[2]                      \
   && argb1[3] == argb2[3])

#else
#define ARGB_TO_RGB(rgb, argb)                  \
  do {                                          \
    rgb[0] = argb[2];                           \
    rgb[1] = argb[1];                           \
    rgb[2] = argb[0];                           \
  } while (0)

#define ARGB_EQUAL(argb1, argb2)                \
  (argb1[0] == argb2[0]                         \
   && argb1[1] == argb2[1]                      \
   && argb1[2] == argb2[2])
#endif

#define NORMALIZE_PAGE_ARG(doc, first, last)                    \
  *first = MAX(1, *first);                                      \
  if (*last <= 0)                                               \
    *last = poppler_document_get_n_pages (doc);                 \
  else                                                          \
    *last = MIN(*last, poppler_document_get_n_pages (doc));

/* png_jmpbuf is supposed to be not available in older versions of
   libpng. */
#ifndef png_jmpbuf
#  define png_jmpbuf(png_ptr) ((png_ptr)->jmpbuf)
#endif

#ifndef HAVE_ERROR_H
#  define error(status, errno, fmt, args...)                    \
  do {                                                          \
    int error = (errno);                                        \
    fflush (stdout);                                            \
    fprintf (stderr, "%s: " fmt, PACKAGE_NAME, ## args);        \
    if (error)                                                  \
      fprintf (stderr, ": %s", strerror (error));               \
    fprintf (stderr, "\n");                                     \
    exit (status);                                              \
  } while (0)
#endif

#define internal_error(fmt, args...)                            \
  error (2, 0, "internal error in %s: " fmt, __func__, ## args)

#define error_if_not(expr)                      \
  if (! (expr)) goto error;

#define perror_if_not(expr, fmt, args...)       \
  do {                                          \
    if (! (expr))                               \
      {                                         \
        printf_error_response ((fmt), ## args); \
        goto error;                             \
      }                                         \
  } while (0)

#define cerror_if_not(expr, error_msg, fmt, args...)            \
  do {                                                          \
    if (! (expr))                                               \
      {                                                         \
        if (error_msg)                                          \
          *(error_msg) = g_strdup_printf((fmt), ## args);       \
        goto error;                                             \
      }                                                         \
  } while (0)

/* Declare commands */
#define DEC_CMD(name)                            \
  {#name, cmd_ ## name, cmd_ ## name ## _spec,  \
   G_N_ELEMENTS (cmd_ ## name ## _spec)}

#define DEC_CMD2(command, name)                          \
  {name, cmd_ ## command, cmd_ ## command ## _spec,     \
   G_N_ELEMENTS (cmd_ ## command ## _spec)}

/* Declare option */
#define DEC_DOPT(name, type, sname)                      \
  {name, type, offsetof (document_options_t, sname)}

enum suffix_char { NONE, COLON, NEWLINE};

enum image_type { PPM, PNG };

typedef struct
{
  PopplerAnnotMapping *amap;
  gchar *key;
} annotation_t;

typedef enum
  {
    ARG_INVALID = 0,
    ARG_DOC,
    ARG_BOOL,
    ARG_STRING,
    ARG_NONEMPTY_STRING,
    ARG_NATNUM,
    ARG_EDGE,
    ARG_EDGE_OR_NEGATIVE,
    ARG_EDGES,
    ARG_EDGES_OR_POSITION,
    ARG_COLOR,
    ARG_REST
  } command_arg_type_t;

typedef struct
{
  const char *name; 
  command_arg_type_t type;
  size_t offset;
} document_option_t;

typedef struct
{
  PopplerColor bg, fg;
  gboolean usecolors;
  gboolean printed;
} render_options_t;

typedef struct
{
  render_options_t render;
} document_options_t;

typedef struct
{
  PopplerDocument *pdf;
  char *filename;
  char *passwd;
  struct
  {
    GHashTable *keys;             /* key => page */
    GList **pages;                /* page array  */
  } annotations;
  document_options_t options;
} document_t;

typedef struct
{
  command_arg_type_t type;
  union
  {
    gboolean flag;
    const char *string;
    long natnum;
    document_t *doc;
    gdouble edge;
    PopplerColor color;
    PopplerRectangle rectangle;
#ifdef HAVE_POPPLER_ANNOT_MARKUP
    PopplerQuadrilateral quadrilateral;
#endif
    struct
    {
      char * const *args;
      int nargs;
    } rest;
  } value;
} command_arg_t;

typedef struct
{
  GHashTable *documents;
} epdfinfo_t;

typedef struct
{
  const char *name;
  void (* execute) (const epdfinfo_t *ctxt, const command_arg_t *args);
  const command_arg_type_t *args_spec;
  int nargs;
} command_t;

/* Defined in poppler-hack.cc */
#ifdef HAVE_POPPLER_ANNOT_WRITE
extern void xpoppler_annot_set_rectangle (PopplerAnnot*, PopplerRectangle*);
#endif
extern gchar *xpoppler_annot_markup_get_created (PopplerAnnotMarkup*);
#endif  /* _EPDF_H_ */
