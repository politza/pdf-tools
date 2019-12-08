#include <stdio.h>
#define printf(fmt, args...) (fprintf (stderr, (fmt), ## args))
#define SYNCTEX_INLINE
