// tmpstream.h -- Temporary stream object for extracting strings.
// 
// I'd use "ostringstream", but it isn't in GCC 2.95.2.

#ifndef TMPSTREAM_H
#define TMPSTREAM_H

#include "common.h"

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
