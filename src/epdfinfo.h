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


typedef struct
{
  PopplerDocument *pdf;
  char *filename;
  char *passwd;
  time_t last_used;
  char *tmp_dir;
} doc_t;

typedef enum args_spec
{
    ARG_NULL = 0,
    ARG_DOC,
    ARG_FLAG,
    ARG_STRING,
    ARG_NATNUM,
    ARG_STRING_NONEMPTY,
    ARG_EDGE
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
