// common.h
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
//
// This file is part of Daisy.
// 
// Daisy is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser Public License as published by
// the Free Software Foundation; either version 2.1 of the License, or
// (at your option) any later version.
// 
// Daisy is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser Public License for more details.
// 
// You should have received a copy of the GNU Lesser Public License
// along with Daisy; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

//
// This file should contain compiler specific workarounds for bugs and
// global constants.

#ifndef COMMON_H
#define COMMON_H

// Portability code.

#if (defined (__GNUC__) && __GNUC__ < 3) \
	|| (defined (__BORLANDC__) && __BORLANDC__ < 0x0550)
#define BROKEN_HEADERS
#endif

#ifdef __sun__
#define pow(x, y) (daisy_assert (x >= 0), (pow)(x, y))
#define sqrt(x) (daisy_assert (x >= 0), (sqrt)(x))
#define log(x) (daisy_assert (x > 0), (log)(x))
#define acos(x) (daisy_assert (x >= -1 && x <= 1), (acos(x)))
#define asin(x) (daisy_assert (x >= -1 && x <= 1), (asin(x)))
#endif

#define WORKING_EXCEPTIONS

#if defined (__BORLANDC__) && __BORLANDC__ < 0x0550
#define EMPTY_TEMPLATE
#else
#define EMPTY_TEMPLATE template<>
#endif

#ifdef __GNUC__
// Only gcc has a C++ safe <math.h>.
#include <math.h>
#else
#define exception _BUG_EXCEPTION
#include <math.h>
#undef exception
#endif

#ifdef __GNUC__

#include <unistd.h>

// GCC doesn't have DLL keywords.
#define EXPORT
#define IMPORT

// If you can delete const objects.
#define CONST_DELETE

#define HAS_TEMPLATE_MEMBERS

// GNU doesn't mind unused global constants.
#define GLOBAL_CONSTANT

#elif defined (VISUALCPP)

#pragma warning (disable: 4786 4503)
#pragma warning (3: 4019 4032 4057 4061 4125 4130 4152 4189 4201 4706)

#define CONST_DELETE
#define HAS_TEMPLATE_MEMBERS
#define GLOBAL_CONSTANT

// Work around broken for-scoping
#define for if(0);else for

#else /* BORLAND */

// WIN32 DLL keywords.
#define EXPORT _export
#define IMPORT _import

// Borland complains about unused global constants unless "extern".
#define GLOBAL_CONSTANT extern

#if __BORLANDC__ < 0x0550
// Define these for Borland C++ 5.0.1
#define BORLAND_TEMPLATES
#define BORLAND_EOF
#define BORLAND_PRAGMA

// #define BORLAND_PERMISSIONS

// Needed in BCC for 'close'.
#include <io.h>

// BC++ 5.01 hasn't <ostream.h>
#define MISSING_OSTREAM

#endif
#endif

#if !defined (__unix) && !defined (__CYGWIN__)
// When running a pure DOS, don't use stderr.
#define USELESS_STDERR
#endif

#include <stdexcept>

using namespace std;

// Shared code.

GLOBAL_CONSTANT const double dt = 1.0;	// time step.

// From Mumit's STL newbie guide.
template <class ForwardIterator>
void sequence_delete (ForwardIterator first, ForwardIterator last) {
  while (first != last)
    delete *first++;
}

template <class ForwardIterator>
void map_delete (ForwardIterator first, ForwardIterator last) 
{
  while (first != last)
    {
      delete (*first).second;
      (*first).second = NULL;
      first++;
    }
}

#endif // COMMON_H
