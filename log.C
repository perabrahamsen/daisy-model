// log.C

#include "log.h"
#include "alist.h"
#include "library.h"
#include "syntax.h"
#include "common.h"

Librarian<Log>::Content* Librarian<Log>::content = NULL;

struct Log::Implementation
{
  list<const Geometry*> geometries;
};

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

Log::Log ()
  : impl (*new Implementation ())
{ }

Log::~Log ()
{
  delete &impl;
}
