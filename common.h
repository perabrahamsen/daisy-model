// common.h
//
// This file should contain compiler specific workarounds for bugs and
// global constants.

#ifndef COMMON_H
#define COMMON_H

#include <std/typeinfo.h>

#define exception _BUG_EXCPETION
#include <math.h>
#undef exception

#ifdef HANDLE_EXCEPTIONS
#define THROW(x) throw x
#define throw2(x, y) throw (x, y)
#define throw0() throw ()
#else HANDLE_EXCEPTIONS
#define THROW(x) do { assert ("error" == #x); abort (); } while (0)
#define throw(x)
#define throw2(x, y)
#define throw0()
#endif  HANDLE_EXCEPTIONS

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

#endif COMMON_H
