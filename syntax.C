// syntax.C

#include "syntax.h"
#include "alist.h"
#include "library.h"
#include <map>

struct Syntax::Implementation
{
  list<string> order;
  typedef map<string, type, less<string> > type_map;
  typedef map<string, category, less<string> > status_map;
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
  void dump (int indent) const;
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
      if(status[key] != Const && status[key] != State)
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

void 
Syntax::Implementation::dump (int indent) const
{
  if (order.size ())
    {
      cout << "[order";
      for (list<string>::const_iterator i = order.begin ();
	   i != order.end ();
	   i++)
	cout << " " << *i;
      cout << "]\n";
      for (int j = 0; j < indent; j++)
	cout << " ";
    }
  for (type_map::const_iterator i = types.begin ();
       i != types.end ();
       i++)
    {
      if (i != types.begin ())
	{
	  cout << "\n";
	  for (int j = 0; j < indent; j++)
	    cout << " ";
	}
      const string name = (*i).first;
      const type t = (*i).second;
      cout << "(" << name << " " << type_name (t) << " " 
	   << category_name ((*status.find (name)).second) << " ";
      switch ((*size.find (name)).second)
	{
	case Singleton:
	  cout << "Singleton";
	  break;
	case Sequence:
	  cout << "Sequence";
	  break;
	default:
	  cout << "[" << (*size.find (name)).second << "]";
	}
      switch (t)
	{
	case List:
	  cout << "\n";
	  for (unsigned int j = 0; j < indent + name.length () + 2; j++)
	    cout << " ";
	  (*syntax.find (name)).second->dump (indent + name.length () + 2);
	  break;
	case Class: 
	  cout << "\n";
	  for (unsigned int j = 0; j < indent + name.length () + 2; j++)
	    cout << " ";
	  (*libraries.find (name)).second->dump (indent + name.length () + 2);
	  break;
	case Object:
	  cout << " " << (*libraries.find (name)).second->name ();
	  break;
	default:
	  break;
	}
      cout << ")";
    }
}

  // Each syntax entry should have an associated type.
const char* 
Syntax::type_name (type t)
{
  static const char * const names[] = 
  { "Number", "List", "CSMP", "Function", "Boolean", "String",
    "Date", "Integer", "Filter", "Class", "Object", "Error" };
  return names[t];
}
    
const char* Syntax::category_name (category c)
{
  static const char * const names[] = 
  { "Const", "State", "Optional", "LogOnly" };
  return names[c];
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

bool
Syntax::is_const (string key) const
{
  assert (impl.status.find (key) != impl.status.end ());
  return impl.status[key] == Const;
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
Syntax::add (string key, type t, category req, int s)
{
  impl.size[key] = s;
  impl.types[key] = t;
  impl.status[key] = req;
}

void
Syntax::add (string key, const Syntax& s, category req, int sz)
{
  add (key, List, req, sz);
  impl.syntax[key] = &s;
}

void
Syntax::add (string key, const FTable* f, category req, int s)
{
  add (key, Function, req, s);
  impl.ftables[key] = f;
}

void 
Syntax::add (string key, const Library& l, category req, int s)
{
  add (key, Object, req, s);
  impl.libraries[key] = &l;
}

void
Syntax::add_filter (string key, const Syntax& s, category req)
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

void 
Syntax::dump (int indent) const
{
  impl.dump (indent);
}

Syntax::Syntax () : impl (*new Implementation ())
{ }

Syntax::~Syntax ()
{
  delete &impl;
}

void
check (const AttributeList& al, string s, bool& ok)
{
  if (!al.check (s))
    {
      cerr << "Missing " << s << "\n";
      ok = false;
    }
}

void
non_negative (double v, string s, bool& ok, int index)
{
  if (v < 0.0)
    {
      cerr << "Negative " << s;
      if (index >= 0)
	cerr << "[" << index << "]";
      cerr << "\n";
      ok = false;
    }
}

void
is_fraction (double v, string s, bool& ok, int index)
{
  if (v < 0.0 || v > 1.0)
    {
      cerr << "fraction " << s << " must be between 0 and 1";
      if (index >= 0)
	cerr << "[" << index << "]";
      cerr << "\n";
      ok = false;
    }
}
