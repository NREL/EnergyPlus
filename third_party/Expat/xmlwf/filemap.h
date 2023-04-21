/* Copyright (c) 1998, 1999 Thai Open Source Software Center Ltd
   See the file COPYING for copying permission.
*/

#include <stddef.h>


#ifdef _MSC_VER
#include <io.h>
#define x_open _open
#define x_close _close
#define x_read _read
#else
#define x_open open
#define x_close close
#define x_read read
#endif


#ifdef XML_UNICODE
int filemap(const wchar_t *name,
            void (*processor)(const void *, size_t,
                              const wchar_t *, void *arg),
            void *arg);
#else
int filemap(const char *name,
            void (*processor)(const void *, size_t,
                              const char *, void *arg),
            void *arg);
#endif
