// tmpstream.C -- Temporary stream object for extracting strings.
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
// I'd use "ostringstream", but it isn't in GCC 2.95.2.

#include "tmpstream.h"
#include "assertion.h"

#if defined (__BORLANDC__)
// Borland C++ 5.01 doesn't spell stream with an m.
#include <strstrea.h>
#else
#include <strstream>
#endif

using namespace std;

struct TmpStream::Implementation
{
  ostrstream out;
  bool done;
  char* str;

  Implementation ()
    : done (false),
      str (NULL)
  { }
  ~Implementation ()
  { 
    if (str)
      delete [] str;
  }
};

ostream& 
TmpStream::operator () ()
{
  daisy_assert (!impl.done);
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
  daisy_assert (impl.str);
  return impl.str;
}
  
TmpStream::TmpStream ()
  : impl (*new Implementation)
{ }

TmpStream::~TmpStream ()
{ delete &impl; }
