// filter.C

#include "filter.h"

Librarian<Filter>::Content* Librarian<Filter>::content = NULL;

bool
Filter::check_derived (const string& name, const Library& library) const
{
  const string* type = &name;
  bool looking = true;

  while (looking && !check (*type))
    {
      if (library.check (*type))
	{
	  const AttributeList alist = library.lookup (*type);
	  if (alist.check ("type"))
	    type = &alist.name ("type");
	  else
	    looking = false;
	}
      else
	looking = false;
    }
  return looking;
}


Filter& 
Filter::lookup_derived (const string& name, const Library& library) const
{
  const string* type = &name;
  bool looking = true;

  while (looking && !check (*type))
    {
      if (library.check (*type))
	{
	  const AttributeList alist = library.lookup (*type);
	  if (alist.check ("type"))
	    type = &alist.name ("type");
	  else
	    assert (false);
	}
      else
	assert (false);
    }
  return lookup (*type);
}

const vector<double>
Filter::select (const Geometry&, const vector<double>& value)
{ return value; }

void
Filter::accumulate (const Geometry&, const vector<double>&)
{ }

bool
Filter::accumulating () const
{ return false; }

bool
Filter::check (const Library&, int /* size */) const
{ return true; }

bool
Filter::check (const Syntax&, int /* size */) const
{ return true; }

bool
Filter::check (const Syntax::type, int /* size */) const
{ return true; }

Filter::Filter ()
{ }

Filter::~Filter ()
{ }
