// common.h
//
// This file should contain compiler specific workarounds for bugs and
// global constants.

#ifndef COMMON_H
#define COMMON_H

// Portability code.

#include <assert.h>
#include <string>

#if !defined (__CYGWIN__) && !defined (MINGW)
// Doesn't work under cygwin
#define pow(x, y) (assert (x >= 0), (pow)(x, y))
#define sqrt(x) (assert (x >= 0), (sqrt)(x))
#define log(x) (assert (x > 0), (log)(x))
#define acos(x) (assert (x >= -1 && x <= 1), (acos(x)))
#define asin(x) (assert (x >= -1 && x <= 1), (asin(x)))
#define WORKING_EXCEPTIONS
#endif

#ifdef __GNUC__
// Only gcc has a C++ safe <math.h>.
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

#else

// WIN32 DLL keywords.
#define EXPORT _export
#define IMPORT _import

// Define these for Borland C++ 5.0.1
#define BORLAND_TEMPLATES
#define BORLAND_EOF
#define BORLAND_PRAGMA
#define BORLAND_ASSERT

// #define BORLAND_PERMISSIONS

// Needed in BCC for `close'.
#include <io.h>
// Needed in BCC for `chdir'.
#include <dir.h>

// Borland complains about unused global constants unless "extern".
#define GLOBAL_CONSTANT extern

// BC++ 5.01 hasn't <ostream.h>
#define MISSING_OSTREAM

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

#ifdef MISSING_OSTREAM
#include <iostream.h>
#else 
#include <ostream.h>
#endif

struct Syntax;
struct AttributeList;

#define CERR (Options::error ())
#define CWAR (Options::warning ())
#define COUT (Options::message ()) 

class Options
{
public: 
  const string program_name;
  static ostream& message ();
  static ostream& warning ();
  static ostream& error ();
  static int find_file (const string& name);
  void usage () const;
  Options (int& argc, char**& argv, 
	   Syntax& syntax, AttributeList& alist);
};

#endif // COMMON_H
