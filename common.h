// common.h

#ifndef COMMON_H
#define COMMON_H

#include <std/typeinfo.h>

#ifdef HANDLE_EXCEPTIONS
#define THROW(x) throw x
#define throw2(x, y) throw (x, y)
#define throw0() throw ()
#else HANDLE_EXCEPTIONS
#define THROW(x) assert ("error" == #x)
#define throw(x)
#define throw2(x, y)
#define throw0()
#endif  HANDLE_EXCEPTIONS

#if 0
#define BUG_DYNAMIC_CAST(T, V) dynamic_cast<T> (V)
#else
#define BUG_DYNAMIC_CAST(T, V) (T) V
#endif

#endif COMMON_H
