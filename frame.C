// frame.C

#include "frame.h"

const Column* 
Frame::column () const
{ return parent ? parent->column () : NULL; }

bool
Frame::match_column (const Column& col) const
{
  const Column *const c = column (); 
  return (c == NULL || c == &col);
}
  
Frame::Frame (const Frame* p)
  : parent (p)
{ }

Frame::~Frame ()
{ }
