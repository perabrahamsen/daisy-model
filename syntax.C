// syntax.C

#include "syntax.h"
#include "alist.h"
#include "log.h"
#include "library.h"
#include <map.h>

struct Syntax::Implementation
{
  
  typedef map<string, type, less<string> > type_map;
  typedef map<string, required, less<string> > status_map;
  typedef map<string, const Syntax*, less<string> > syntax_map;
  typedef map<string, const FTable*, less<string> > ftable_map;
  typedef map<string, int, less<string> > size_map;
  typedef map<string, const Library*, less<string> > library_map;
  typedef map<string, derive_fun, less<string> > derive_map;
  type_map types;
  status_map status;
  syntax_map syntax;
  ftable_map ftables;
  size_map size;
  library_map libraries;
  derive_map derived;
  bool check (string name, const AttributeList& vl, const Log& log,
	      bool sparse);
  bool check (const AttributeList& vl);
  Syntax::type lookup (string key) const;
};    

bool 
Syntax::Implementation::check (string name, const AttributeList& vl,
			       const Log& log, bool sparse)
{

  bool error = false;

  for (status_map::const_iterator i = status.begin ();
       i != status.end ();
       i++)
    {
      string key = (*i).first;
      required state = status[key];
      if (!sparse && state == Mandatory && !vl.check (key))
	{
	  log.err () << "Attributte " << key << " missing\n";
	  error = true;
	}
      else if (types[key] == Sequence && vl.check (key))
	{
	  const Library& lib = *libraries[key];
	  const ::Sequence& seq = vl.sequence (key);
	  for (::Sequence::const_iterator j = seq.begin ();
	       j != seq.end ();
	       j++)
	    {
	      const AttributeList& al = **j;
	      if (!al.check ("type"))
		{
		  log.err () << "Non object found \n";
		  error = true;
		}
	      else if (!lib.syntax (al.name ("type")).check (key, al, 
							     log, sparse))
		error = true;
	    }
	}
      else if (types[key] == Object && vl.check (key))
	{
	  const Library& lib = *libraries[key];
	  const AttributeList& al = vl.list (key);
	  if (!al.check ("type"))
	    {
	      log.err () << "Non object found \n";
	      error = true;
	    }
	  else if (!lib.syntax (al.name ("type")).check (al))
	    error = true;
	}
      else if (types[key] == List && vl.check (key)
	       && !syntax[key]->check (key, vl.list (key), log,
				       sparse || (state == Sparse)))
	error = true;
    }
  if (error)
    log.err () << "in " << name << "\n";

  return !error;
}

bool 
Syntax::Implementation::check (const AttributeList& vl)
{
  for (status_map::const_iterator i = status.begin ();
       i != status.end ();
       i++)
    {
      string key = (*i).first;
      if (status[key] == Mandatory && !vl.check (key))
	return false;
      else if (types[key] == Sequence && vl.check (key))
	{
	  const ::Sequence& seq = vl.sequence (key);
	  for (::Sequence::const_iterator j = seq.begin ();
	       j != seq.end ();
	       i++)
	    if (!syntax[key]->check (**j))
	      return false;
	}
      else if (types[key] == Sequence && vl.check (key))
	{
	  const Library& lib = *libraries[key];
	  const ::Sequence& seq = vl.sequence (key);
	  for (::Sequence::const_iterator j = seq.begin ();
	       j != seq.end ();
	       j++)
	    {
	      const AttributeList& al = **j;
	      if (!al.check ("type")
		  || !lib.syntax (al.name ("type")).check (al))
		return false;
	    }
	}
      else if (types[key] == Object && vl.check (key))
	{
	  const Library& lib = *libraries[key];
	  const AttributeList& al = vl.list (key);
	  if (!al.check ("type") || !lib.syntax (al.name ("type")).check (al))
	    return false;
	}
      else if (types[key] == List
	       && vl.check (key)
	       && !syntax[key]->check (vl.list (key)))
	return false;
    }
  return true;
}

Syntax::type 
Syntax::Implementation::lookup (string key) const
{
  type_map::const_iterator i = types.find (key);

  if (i == types.end ())
    return Syntax::Error;
  else
    return (*i).second;
}

bool
Syntax::check (string name, const AttributeList& vl,
	       const Log& log, bool sparse) const
{
  return impl.check (name, vl, log, sparse);
}

bool
Syntax::check (const AttributeList& vl) const
{
  return impl.check (vl);
}

Syntax::type 
Syntax::lookup (string key) const
{
  return impl.lookup (key);
}

Syntax::required
Syntax::status (string key) const
{
  return impl.status[key];
}

const Syntax&
Syntax::syntax (string key) const
{
  return *impl.syntax[key];
}

const FTable*
Syntax::function (string key) const
{
  return impl.ftables[key];
}

const Library&
Syntax::library (string key) const
{
  return *impl.libraries[key];
}

derive_fun
Syntax::derive (string key) const
{
  return impl.derived[key];
}

int
Syntax::size (string key) const
{
  Implementation::size_map::const_iterator i = impl.size.find (key);

  if (i == impl.size.end ())
    return -1;
  else
    return (*i).second;
}

void
Syntax::add (string key, type t, required req)
{
  impl.types[key] = t;
  impl.status[key] = req;
}

void
Syntax::add (string key, const Syntax& s, required req)
{
  add (key, List, req);
  impl.syntax[key] = &s;
}

void
Syntax::add (string key, const FTable* f, required req)
{
  add (key, Function, req);
  impl.ftables[key] = f;
}

void
Syntax::add (string key, int s, required req)
{
  add (key, Array, req);
  impl.size[key] = s;
}

void
Syntax::add_output (string key, const Syntax& s, required req)
{
  add (key, Output, req);
  impl.syntax[key] = &s;
}  

void 
Syntax::add_class (string key, const Library& l, derive_fun fun)
{
  add (key, Class, Optional);
  impl.libraries[key] = &l;
  impl.derived[key] = fun;
}

void 
Syntax::add_object (string key, const Library& l, required req)
{
  add (key, Object, req);
  impl.libraries[key] = &l;
}

void 
Syntax::add_sequence (string key, const Library& l, required req)
{
  add (key, Sequence, req);
  impl.libraries[key] = &l;
}

void 
Syntax::add_layers (string key, const Library& l, required req)
{
  add (key, Layers, req);
  impl.libraries[key] = &l;
}

Syntax::Syntax () : impl (*new Implementation ())
{ }

Syntax::~Syntax ()
{
  delete &impl;
}
