// syntax.C

#include "syntax.h"
#include "alist.h"
#include "library.h"
#include <map>
#include <algorithm>

const int Syntax::Singleton = -117;
const int Syntax::Sequence = -3212;
const int Syntax::Unspecified = -666;

struct Syntax::Implementation
{
  check_fun checker;
  vector<string> order;
  typedef map<string, type, less<string> > type_map;
  typedef map<string, category, less<string> > status_map;
  typedef map<string, const Syntax*, less<string> > syntax_map;
  typedef map<string, int, less<string> > size_map;
  typedef map<string, const ::Library*, less<string> > library_map;
  typedef map<string, derive_fun, less<string> > derive_map;
  type_map types;
  status_map status;
  syntax_map syntax;
  size_map size;
  library_map libraries;
  derive_map derived;
  bool check (const AttributeList& vl, const string& name);
  Syntax::type lookup (const string& key) const;
  int order_number (const string& name) const;
  void dump (int indent) const;
  void entries (vector<string>& result) const;
  Implementation (check_fun c)
    : checker (c)
  { }
};    

bool 
Syntax::Implementation::check (const AttributeList& vl, const string& name)
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
	    const ::Library& lib = *libraries[key];
	    const vector<AttributeList*>& seq = vl.alist_sequence (key);
	    for (vector<AttributeList*>::const_iterator j = seq.begin ();
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
	    const ::Library& lib = *libraries[key];
	    const AttributeList& al = vl.alist (key);
	    if (!al.check ("type"))
	      {
		cerr << "Non object found \n";
		error = true;
	      }
	    else if (!lib.syntax (al.name ("type")).check (al))
	      error = true;
	  }
      else if (types[key] == AList)
	if (size[key] != Singleton)
	  {
	    const vector<AttributeList*>& seq = vl.alist_sequence (key);
	    for (vector<AttributeList*>::const_iterator j = seq.begin ();
		 j != seq.end ();
		 j++)
	      {
		const AttributeList& al = **j;
		if (!syntax[key]->check (al, key))
		  error = true;
	      }
	  }
	else if (!syntax[key]->check (vl.alist (key), key))
	  error = true;
    }
  if (!error && checker && !checker (vl))
    error = true;

  if (error)
    cerr << "in " << name << "\n";

  return !error;
}

Syntax::type 
Syntax::Implementation::lookup (const string& key) const
{
  type_map::const_iterator i = types.find (key);

  if (i == types.end ())
    return Syntax::Error;
  else
    return (*i).second;
}

int
Syntax::Implementation::order_number (const string& name) const
{
  for (unsigned int j = 0; j < order.size (); j++)
    if (order[j] == name)
      return j;
  return -1;
}

void 
Syntax::Implementation::dump (int indent) const
{
  if (order.size ())
    {
      cout << "[order";
      for (vector<string>::const_iterator i = order.begin ();
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
	case AList:
	  {
	    cout << "\n";
	    for (unsigned int j = 0; j < indent + name.length () + 2; j++)
	      cout << " ";
	    (*syntax.find (name)).second->dump (indent + name.length () + 2);
	    break;
	  }
	case Library: 
	  {
	    cout << "\n";
	    for (unsigned int j = 0; j < indent + name.length () + 2; j++)
	      cout << " ";
	    (*libraries.find (name)).second->dump (indent + name.length () + 2);
	    break;
	  }
	case Object:
	  cout << " " << (*libraries.find (name)).second->name ();
	  break;
	default:
	  break;
	}
      cout << ")";
    }
}

void
Syntax::Implementation::entries (vector<string>& result) const
{
  // All the ordered items first.
  result = order;

  for (type_map::const_iterator i = types.begin ();
       i != types.end ();
       i++)
    {
      const string name = (*i).first;
      
      if (order_number (name) < 0)
	result.push_back (name);
    }
}

// Each syntax entry should have an associated type.

static const char * const type_names[] = 
{ "Number", "AList", "CSMP", "Boolean", "String",
  "Date", "Integer", "Object", "Library", "Error", NULL };

const char* 
Syntax::type_name (type t)
{
  assert (sizeof (type_names) / sizeof  (const char*) == Error + 2);
  return type_names[t];
}
    
Syntax::type
Syntax::type_number (const char* name)
{ 
  for (int i = 0; type_names[i]; i++)
    if (strcmp (name, type_names[i]) == 0)
      return static_cast<type> (i);
  return Error;
}

static const char * const category_names[] = 
{ "Const", "State", "Optional", "LogOnly", NULL };

const char* Syntax::category_name (category c)
{ return category_names[c]; }

int
Syntax::category_number (const char* name)
{ 
  for (int i = 0; category_names[i]; i++)
    if (strcmp (name, category_names[i]) == 0)
      return i;
  return -1;
}

bool
Syntax::check (const AttributeList& vl, const string& name) const
{
  return impl.check (vl, name);
}

Syntax::type 
Syntax::lookup (const string& key) const
{
  return impl.lookup (key);
}

bool
Syntax::is_const (const string& key) const
{
  assert (impl.status.find (key) != impl.status.end ());
  return impl.status[key] == Const;
}

const Syntax&
Syntax::syntax (const string& key) const
{
  assert (impl.syntax.find (key) != impl.syntax.end ());
  return *impl.syntax[key];
}

const ::Library&
Syntax::library (const string& key) const
{
  assert (impl.libraries.find (key) != impl.libraries.end ());
  return *impl.libraries[key];
}

derive_fun
Syntax::derive (const string& key) const
{
  assert (impl.derived.find (key) != impl.derived.end ());
  return impl.derived[key];
}

int
Syntax::size (const string& key) const
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

const vector<string>& 
Syntax::order () const
{
  return impl.order;
}

int
Syntax::order (const string& name) const
{
  return impl.order_number (name);
}

void
Syntax::add (const string& key, type t, category req, int s)
{
  impl.size[key] = s;
  impl.types[key] = t;
  impl.status[key] = req;
}

void
Syntax::add (const string& key, const Syntax& s, category req, int sz)
{
  add (key, AList, req, sz);
  impl.syntax[key] = &s;
}

void 
Syntax::add (const string& key, const ::Library& l, category req, int s)
{
  add (key, Object, req, s);
  impl.libraries[key] = &l;
}

void 
Syntax::add_library (const string& key, const ::Library& l, derive_fun fun)
{
  add (key, Library, Optional);
  impl.libraries[key] = &l;
  impl.derived[key] = fun;
}

void 
Syntax::order (const vector<string>& order)
{
  impl.order = order;
}

void 
Syntax::order (const string& one)
{
  assert (impl.order.size () == 0);
  impl.order.push_back (one);
}

void 
Syntax::order (const string& one, const string& two)
{
  assert (impl.order.size () == 0);
  impl.order.push_back (one);
  impl.order.push_back (two);
}

void 
Syntax::order (const string& one, const string& two, const string& three)
{
  assert (impl.order.size () == 0);
  impl.order.push_back (one);
  impl.order.push_back (two);
  impl.order.push_back (three);
}

void 
Syntax::order (const string& one, const string& two, const string& three,
	       const string& four)
{
  assert (impl.order.size () == 0);
  impl.order.push_back (one);
  impl.order.push_back (two);
  impl.order.push_back (three);
  impl.order.push_back (four);
}

void 
Syntax::order (const string& one, const string& two, const string& three,
	       const string& four, const string& five)
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

void
Syntax::entries (vector<string>& result) const
{
  impl.entries (result);
}

Syntax::Syntax (check_fun c) : impl (*new Implementation (c))
{ }

Syntax::~Syntax ()
{
  delete &impl;
}

void
check (const AttributeList& al, const string& s, bool& ok)
{
  if (!al.check (s))
    {
      cerr << "Missing " << s << "\n";
      ok = false;
    }
}

void
non_negative (double v, const string& s, bool& ok, int index)
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
non_positive (double v, const string& s, bool& ok, int index)
{
  if (v > 0.0)
    {
      cerr << "Positive " << s;
      if (index >= 0)
	cerr << "[" << index << "]";
      cerr << "\n";
      ok = false;
    }
}

void
is_fraction (double v, const string& s, bool& ok, int index)
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
