// common.h
//
// This file should contain compiler specific workarounds for bugs and
// global constants.

#ifndef COMMON_H
#define COMMON_H

// Portability code.

#include <assert.h>
#include <string>

#if (defined (__GNUC__) && __GNUC__ < 3) \
	|| (defined (__BORLANDC__) && __BORLANDC__ < 0x0550)
#define BROKEN_HEADERS
#endif

#if !defined (__CYGWIN__) && !defined (MINGW) && !defined (VISUALCPP) && !defined (__BORLANDC__)
// Doesn't work under cygwin
#define pow(x, y) (assert (x >= 0), (pow)(x, y))
#define sqrt(x) (assert (x >= 0), (sqrt)(x))
#define log(x) (assert (x > 0), (log)(x))
#define acos(x) (assert (x >= -1 && x <= 1), (acos(x)))
#define asin(x) (assert (x >= -1 && x <= 1), (asin(x)))
#endif

#define WORKING_EXCEPTIONS

#ifdef __GNUC__
// Only gcc has a C++ safe <math.h>.
#include <math.h>
#else
#define exception _BUG_EXCEPTION
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

#ifdef __GNUC__

// GCC doesn't have DLL keywords.
#define EXPORT
#define IMPORT

// If you can delete const objects.
#define CONST_DELETE

#define HAS_TEMPLATE_MEMBERS

#include <unistd.h>

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
#define BORLAND_ASSERT

// #define BORLAND_PERMISSIONS

// Needed in BCC for 'close'.
#include <io.h>

// BC++ 5.01 hasn't <ostream.h>
#define MISSING_OSTREAM

#endif
#endif

#if defined (__unix) 
// Unix path names.
#define PATH_SEPARATOR ":"
#define DIRECTORY_SEPARATOR "/"

#else

// WinDOS path names.
#define PATH_SEPARATOR ";"
#define DIRECTORY_SEPARATOR "\\"

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


#endif // COMMON_H
