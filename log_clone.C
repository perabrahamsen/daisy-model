// log_clone.C
//
// Clone an object by using its log function.

#include "log_clone.h"

#ifdef BORLAND_PRAGMA
#pragma warn -rvl
#endif

bool
LogClone::match (const Daisy&)
{ assert (false); return false; }

#ifdef BORLAND_PRAGMA
#pragma warn +rvl
#endif

bool 
LogClone::check (const string &) const
{ assert (false); return false; }
void
LogClone::done ()
{ assert (false); }

const AttributeList& 
LogClone::result ()
{ return alist (); }

LogClone::LogClone (const string& name, 
		    const Syntax& syntax, const AttributeList& alist)
  : LogAList (alist)
{ 
  push (name, syntax, alist);
}

LogClone::~LogClone ()
{
  // Check stacks.
  assert (syntax_stack.size () == 1U);
  assert (alist_stack.size () == 1U);
  assert (library_stack.size () == 1U);
  
  // Cleanup.
  delete &alist ();
  pop ();
  assert (nested == 0);
}
