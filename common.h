// common.h
//
// This file should contain compiler specific workarounds for bugs and
// global constants.

#ifndef COMMON_H
#define COMMON_H

// Portability code.

#include <assert.h>
#include <string>

#define exception _BUG_EXCPETION
#include <math.h>
#undef exception

#ifdef __sparc__
#include <ieeefp.h>
#else
#define finite(x) 1
// ((x) <= 0.0 ¦¦ (x) >= 0.0)
#define rint(x) ((int)x)
#endif

#ifdef __unix

// If you can delete const objects.
#define CONST_DELETE

#include <fcntl.h>
#include <osfcn.h>
#include <strstream.h>

#else
// Define these for Borland C++ 5.0.1
#define HANDLE_NAMESPACES
#define BORLAND_TEMPLATES
#define HANDLE_EXCEPTIONS
#define BORLAND_C_STR
#define BORLAND_EOF

// Needed in BCC for `close'.
#include <io.h>
// Borland C++ 5.01 doesn't spell stream with an m.
#include <strstrea.h>

#endif

#ifdef HANDLE_EXCEPTIONS
#include <stdexcept>
#define THROW(x) throw x
#define throw1(x) throw (x)
#define throw2(x, y) throw (x, y)
#define throw0() throw ()
#else HANDLE_EXCEPTIONS
#define THROW(x) do { assert ("error" == #x); abort (); } while (0)
#define throw1(x)
#define throw2(x, y)
#define throw0()
#endif  HANDLE_EXCEPTIONS

#ifdef HANDLE_NAMESPACES
using namespace std;
#endif

#if 0
#define BUG_DYNAMIC_CAST(T, V) dynamic_cast<T> (V)
#else
#define BUG_DYNAMIC_CAST(T, V) (T) V
#endif

// Shared code.

static const double dt = 1.0;	// time step.

// From Mumit's STL newbie guide.
template <class ForwardIterator>
void sequence_delete (ForwardIterator first, ForwardIterator last) {
  while (first != last)
    delete *first++;
}

#endif COMMON_H
