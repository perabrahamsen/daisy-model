// log.C

#include "log.h"
#include "alist.h"
#include "library.h"
#include "syntax.h"
#include "common.h"

Librarian<Log>::Content* Librarian<Log>::content = NULL;

const char *const Log::description = "\
Running a simulation is uninteresting, unless you can get access to\n\
the results in one way or another.  The purpose of the `log' component\n\
is to provide this access.  Most `log' models does this by writing a\n\
summary of the state to a log file.";

struct Log::Implementation
{
  list<const Geometry*> geometries;
};

void 
Log::open_alist (const string& name, const AttributeList&)
{ open (name); }

void 
Log::close_alist ()
{ close (); }

void 
Log::open_geometry (const Geometry& g)
{ 
  impl.geometries.push_back (&g);
}

void 
Log::close_geometry ()
{
  impl.geometries.pop_back ();
}

const Geometry*
Log::geometry ()
{
  return impl.geometries.back ();
}

void 
Log::done ()
{ }

Log::Log ()
  : impl (*new Implementation ())
{ }

Log::~Log ()
{
  delete &impl;
}
