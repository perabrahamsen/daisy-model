// syntax.C

#include "syntax.h"
#include "alist.h"
#include "library.h"
#include <map.h>

struct Syntax::Implementation
{
  list<string> order;
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
  bool check (const AttributeList& vl, string name);
  Syntax::type lookup (string key) const;
};    

bool 
Syntax::Implementation::check (const AttributeList& vl, string name)
{
  bool error = false;

  for (status_map::const_iterator i = status.begin ();
       i != status.end ();
       i++)
    {
      string key = (*i).first;
      if(status[key] != Const && status[key] != InOut)
	/* Do nothing */;
      else if (!vl.check (key))
	{
	      cerr << "Attributte " << key << " missing\n";
	      error = true;
	}
      else if (types[key] == Object)
	if (size[key] != Singleton)
	  {
	    const Library& lib = *libraries[key];
	    const vector<const AttributeList*>& seq = vl.list_sequence (key);
	    for (vector<const AttributeList*>::const_iterator j = seq.begin ();
		 j != seq.end ();
		 j++)
	      {
		const AttributeList& al = **j;
		if (!al.check ("type"))
		  {
		    cerr << "Non object found \n";
		    error = true;
		  }
		else if (!lib.syntax (al.name ("type")) .check (al, key))
		  error = true;
	      }
	  }
	else 
	  {
	    const Library& lib = *libraries[key];
	    const AttributeList& al = vl.list (key);
	    if (!al.check ("type"))
	      {
		cerr << "Non object found \n";
		error = true;
	      }
	    else if (!lib.syntax (al.name ("type")).check (al))
	      error = true;
	  }
      else if (types[key] == List)
	if (size[key] != Singleton)
	  {
	    const vector<const AttributeList*>& seq = vl.list_sequence (key);
	    for (vector<const AttributeList*>::const_iterator j = seq.begin ();
		 j != seq.end ();
		 j++)
	      {
		const AttributeList& al = **j;
		if (!syntax[key]->check (al, key))
		  error = true;
	      }
	  }
	else if (!syntax[key]->check (vl.list (key), key))
	  error = true;
    }
  if (error)
    cerr << "in " << name << "\n";

  return !error;
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
Syntax::check (const AttributeList& vl, string name) const
{
  return impl.check (vl, name);
}

Syntax::type 
Syntax::lookup (string key) const
{
  return impl.lookup (key);
}

Syntax::required
Syntax::status (string key) const
{
  assert (impl.status.find (key) != impl.status.end ());
  return impl.status[key];
}

const Syntax&
Syntax::syntax (string key) const
{
  assert (impl.syntax.find (key) != impl.syntax.end ());
  return *impl.syntax[key];
}

const FTable*
Syntax::function (string key) const
{
  assert (impl.ftables.find (key) != impl.ftables.end ());
  return impl.ftables[key];
}

const Library&
Syntax::library (string key) const
{
  assert (impl.libraries.find (key) != impl.libraries.end ());
  return *impl.libraries[key];
}

derive_fun
Syntax::derive (string key) const
{
  assert (impl.derived.find (key) != impl.derived.end ());
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

bool 
Syntax::ordered () const
{
  return impl.order.size () > 0;
}

const list<string>& 
Syntax::order () const
{
  return impl.order;
}

void
Syntax::add (string key, type t, required req, int s)
{
  impl.size[key] = s;
  impl.types[key] = t;
  impl.status[key] = req;
}

void
Syntax::add (string key, const Syntax& s, required req, int sz)
{
  add (key, List, req, sz);
  impl.syntax[key] = &s;
}

void
Syntax::add (string key, const FTable* f, required req, int s)
{
  add (key, Function, req, s);
  impl.ftables[key] = f;
}

void 
Syntax::add (string key, const Library& l, required req, int s)
{
  add (key, Object, req, s);
  impl.libraries[key] = &l;
}

void
Syntax::add_filter (string key, const Syntax& s, required req)
{
  add (key, Filter, req);
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
Syntax::order (const list<string>& order)
{
  impl.order = order;
}

void 
Syntax::order (string one)
{
  assert (impl.order.size () == 0);
  impl.order.push_back (one);
}

void 
Syntax::order (string one, string two)
{
  assert (impl.order.size () == 0);
  impl.order.push_back (one);
  impl.order.push_back (two);
}

void 
Syntax::order (string one, string two, string three)
{
  assert (impl.order.size () == 0);
  impl.order.push_back (one);
  impl.order.push_back (two);
  impl.order.push_back (three);
}

void 
Syntax::order (string one, string two, string three, string four)
{
  assert (impl.order.size () == 0);
  impl.order.push_back (one);
  impl.order.push_back (two);
  impl.order.push_back (three);
  impl.order.push_back (four);
}

void 
Syntax::order (string one, string two, string three, string four, string five)
{
  assert (impl.order.size () == 0);
  impl.order.push_back (one);
  impl.order.push_back (two);
  impl.order.push_back (three);
  impl.order.push_back (four);
  impl.order.push_back (five);
}

Syntax::Syntax () : impl (*new Implementation ())
{ }

Syntax::~Syntax ()
{
  delete &impl;
}
