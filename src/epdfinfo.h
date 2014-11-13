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

#define INPUT_BUFFER_SIZE (4096 * 4)
#define MAX_COMMAND_NARGS 1024

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

#define INT()                                   \
  do {                                          \
    puts ("INT\n.")                             \
    fflush (stdout);                            \
  } while (0)

#ifdef WORDS_BIGENDIAN
#define ARGB_TO_RGB(rgb, argb)                  \
  do {                                          \
  rgb[0] = argb[1];                             \
  rgb[1] = argb[2];                             \
  rgb[2] = argb[3];                             \
  } while (0)

#define ARGB_EQUAL(argb1, argb2)                \
  (argb1[1] == argb2[1]                         \
   && argb1[2] == argb2[2]                      \
   && argb1[3] == argb2[3])

#else
#define ARGB_TO_RGB(rgb, argb)                  \
  do {                                          \
  rgb[0] = argb[2];                             \
  rgb[1] = argb[1];                             \
  rgb[2] = argb[0];                             \
  } while (0)

#define ARGB_EQUAL(argb1, argb2)                \
  (argb1[0] == argb2[0]                         \
   && argb1[1] == argb2[1]                      \
   && argb1[2] == argb2[2])
#endif

#ifndef png_jmpbuf
#  define png_jmpbuf(png_ptr) ((png_ptr)->jmpbuf)
#endif

#define internal_error(fmt, args...)                            \
  error (2, 0, "internal error in %s: " fmt, __func__, ## args)

enum { NONE, COLON, NEWLINE};

enum image_type { PPM, PNG };

typedef struct
{
  PopplerAnnotMapping *amap;
  gchar *key;
} annotation_t;

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
} document_t;
  
typedef enum 
{
    ARG_NULL = 0,
    ARG_DOC,
    ARG_FLAG,
    ARG_STRING,
    ARG_NATNUM,
    ARG_STRING_NONEMPTY,
    ARG_REAL,
    ARG_NONNEG_REAL,
    ARG_RECTANGLE,
    ARG_NONNEG_RECTANGLE,
    ARG_COLOR,
    ARG_REST
} command_arg_spec_t;

typedef struct
{
  union
  {
    gboolean flag;
    char *string;
    long natnum;
    document_t *doc;
    double real;
    PopplerColor color;
    PopplerRectangle rectangle;
    struct
    {
      char **args;
      int nargs;
    } rest;
  } value;
  command_arg_spec_t type;
} command_arg_t;

typedef struct
{
  GHashTable *documents;
} epdfinfo_t;

typedef struct
{
  const char *name;             /* Name des Kommandos */
  void (* execute) (const epdfinfo_t *ctxt, const command_arg_t *args);
  const command_arg_spec_t *args_spec; /* Art der Argumente */
  int nargs;                    /* Anzahl Argumente */
} command_t;

extern void poppler_annot_set_rectangle (PopplerAnnot*, PopplerRectangle*);
extern gchar *poppler_annot_markup_get_created (PopplerAnnotMarkup*);

#endif  /* _EPDF_H_ */
