// common.h
//
// This file should contain compiler specific workarounds for bugs and
// global constants.

#ifndef COMMON_H
#define COMMON_H

// Portability code.

#include <assert.h>
#include <string>

#ifdef EGCS
#include <math.h>
#else
#define exception _BUG_EXCPETION
#include <math.h>
#undef exception
#endif

#ifdef __sparc__
#include <ieeefp.h>
#else
#define finite(x) 1
// ((x) <= 0.0 ¦¦ (x) >= 0.0)
#define rint(x) ((int)x)
#endif

#ifdef __unix

// Unix doesn't have DLL keywords.
#define EXPORT
#define IMPORT

// If you can delete const objects.
#define CONST_DELETE

#ifdef EGCS
// Egcs can initialize a vector from two list iterators.
#define HAS_TEMPLATE_MEMBERS
#else
// The g++ 2.7.2 find template doesn't work.
#define BUGGY_FIND
#endif

#include <unistd.h>
#include <strstream.h>

// GNU doesn't mind unused global constants.
#define GLOBAL_CONSTANT

#else

// WIN32 DLL keywords.
#define EXPORT _export
#define IMPORT _import

// Define these for Borland C++ 5.0.1
#define HANDLE_NAMESPACES
#define BORLAND_TEMPLATES
#define HANDLE_EXCEPTIONS
#define BORLAND_EOF
// #define BORLAND_PERMISSIONS

// Needed in BCC for `close'.
#include <io.h>
// Borland C++ 5.01 doesn't spell stream with an m.
#include <strstrea.h>

// Borland complains about unused global constants unless "extern".
#define GLOBAL_CONSTANT extern

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

GLOBAL_CONSTANT const double dt = 1.0;	// time step.

// From Mumit's STL newbie guide.
template <class ForwardIterator>
void sequence_delete (ForwardIterator first, ForwardIterator last) {
  while (first != last)
    delete *first++;
}

#endif COMMON_H
