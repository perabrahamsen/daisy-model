// common.h
//
// This file should contain compiler specific workarounds for bugs and
// global constants.

#ifndef COMMON_H
#define COMMON_H

#include <assert.h>
#include <string>

#ifdef __sparc__
#include <ieeefp.h>
#else
#define finite(x) 1
// ((x) <= 0.0 ¦¦ (x) >= 0.0)
#define rint(x) ((int)x)
#endif


// Comment out for NT.
// #include <std/typeinfo.h>

#define exception _BUG_EXCPETION
#include <math.h>
#undef exception

// Define these for Borland C++ 5.0.1
#define HANDLE_NAMESPACES
#define BORLAND_TEMPLATES
//#define HANDLE_EXCEPTIONS
#define BORLAND_C_STR
#define BORLAND_EOF

// If you can delete const objects.
// #define CONST_DELETE

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

static const double dt = 1.0;	// time step.

// From Mumit's STL newbie guide.
template <class ForwardIterator>
void sequence_delete (ForwardIterator first, ForwardIterator last) {
  while (first != last)
    delete *first++;
}

inline double pF2h (double pF)
{ 
  return -pow (10, pF);
}

inline double h2pF (double h)
{
  return log10 (-h);
}

#endif COMMON_H
