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

#ifndef _EPDF_H_
#define _EPDF_H_ _EPDF_H_
#include "config.h"
#include <glib.h>
#include <poppler.h>

#define IN_BUF_LEN 4096
#define RELEASE_DOC_TIMEOUT (15 * 60)

#define OK_BEG()                                \
  do {                                          \
    puts("OK");                                 \
  } while (0)

#define OK_END()                                \
  do {                                          \
    puts(".");                                  \
  } while (0)

#define OK()                                    \
  do {                                          \
    puts ("OK\n.");                             \
  } while (0)


enum { NONE, COLON, NL};

typedef struct
{
  PopplerAnnotMapping *amap;
  gchar *key;
} annot_t;

typedef struct
{
  PopplerDocument *pdf;
  char *filename;
  char *passwd;
  time_t last_used;
  gboolean allow_auto_release;
  struct
  {
    GHashTable *keys;             /* key => page */
    GList **pages;                /* page array  */
  } annotations;
} doc_t;
  
typedef enum args_spec
{
    ARG_NULL = 0,
    ARG_DOC,
    ARG_FLAG,
    ARG_STRING,
    ARG_NATNUM,
    ARG_STRING_NONEMPTY,
    ARG_EDGE,
    ARG_EDGE_OR_NEG,
    ARG_COLOR
} args_spec_t;

typedef struct
{
  union
  {
    gboolean flag;
    char *string;
    long natnum;
    doc_t *doc;
    double edge;
  } value;
  args_spec_t type;
} arg_t;

typedef struct
{
  GHashTable *documents;
} ctxt_t;

typedef struct
{
  const char *name;             /* Name des Kommandos */
  void (* execute) (const ctxt_t *ctxt, const arg_t *args);
  const args_spec_t *args_spec; /* Art der Argumente */
  int nargs;                    /* Anzahl Argumente */
} cmd_t;

#endif  /* _EPDF_H_ */
