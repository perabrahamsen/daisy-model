// tmpstream.C -- Temporary stream object for extracting strings.
// 
// I'd use "ostringstream", but it isn't in GCC 2.95.2.

#include "tmpstream.h"

#include <assert.h>

#ifdef __GNUC__
#include <strstream.h>
#else
// Borland C++ 5.01 doesn't spell stream with an m.
#include <strstrea.h>
#endif

struct TmpStream::Implementation
{
  ostrstream out;
  bool done;
  const char* str;

  Implementation ()
    : done (false),
      str (NULL)
  { }
  ~Implementation ()
  { 
    if (str)
      delete str;
  }
};

ostream& 
TmpStream::operator () ()
{
  assert (!impl.done);
  return impl.out;
}

const char* 
TmpStream::str ()
{
  if (!impl.done)
    {
      impl.done = true;
      impl.out << '\0';
      impl.str = impl.out.str ();
    }
  assert (impl.str);
  return impl.str;
}
  
TmpStream::TmpStream ()
  : impl (*new Implementation)
{ }

TmpStream::~TmpStream ()
{ delete &impl; }
