// tmpstream.h -- Temporary stream object for extracting strings.
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

#ifndef TMPSTREAM_H
#define TMPSTREAM_H

#include "common.h"

#if defined (MISSING_OSTREAM)
#include <iostream.h>
#elif defined (BROKEN_HEADERS)
#include <ostream.h>
#else
#include <ostream>
#endif

class TmpStream
{
  // Content.
  struct Implementation;
  Implementation& impl;
  
  // Use.
public:
  ostream& operator () ();
  const char* str ();
  
  // Create and Destroy.
public:
  TmpStream ();
  ~TmpStream ();
};

#endif // TMPSTREAM_H
