// tmpstream.h -- Temporary stream object for extracting strings.
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
