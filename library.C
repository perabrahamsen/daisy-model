// library.C

#include "library.h"
#include "alist.h"
#include "syntax.h"
#include <map>

struct Library::Implementation
{
  const char *const name;
  typedef map<string, const AttributeList*, less<string> > alist_map;
  typedef map<string, const Syntax*, less<string> > syntax_map;
  alist_map alists;
  syntax_map syntaxen;
  const AttributeList& lookup (string) const;
  bool check (string) const;
  void add (string, const AttributeList&, const Syntax&);
  const Syntax& syntax (string) const;
  void dump (int indent) const;
  Implementation (const char* n) 
    : name (n)
  { }
};

const AttributeList&
Library::Implementation::lookup (string key) const
{ 
  alist_map::const_iterator i = alists.find (key);

  if (i == alists.end ())
    THROW (UninitializedValue ());

  return *(*i).second;
}

bool
Library::Implementation::check (string key) const
{ 
  alist_map::const_iterator i = alists.find (key);

  if (i == alists.end ())
    return false;

  return true;
}

void
Library::Implementation::add (string key, const AttributeList& value, const Syntax& syntax)
{
  alists[key] = &value;
  syntaxen[key] = &syntax;
}

const Syntax& 
Library::Implementation::syntax (string key) const
{ 
  syntax_map::const_iterator i = syntaxen.find (key);

  if (i == syntaxen.end ())
    THROW (UninitializedValue ());

  return *(*i).second;
}

void
Library::Implementation::dump (int indent) const
{
  for (syntax_map::const_iterator i = syntaxen.begin ();
       i != syntaxen.end ();
       i++)
    {
      if (i != syntaxen.begin ())
	{
	  cout << "\n";
	  for (int j = 0; j < indent; j++)
	    cout << " ";
	}
      const string name = (*i).first;
      const Syntax* syntax = (*i).second;
      cout << "(" << name << " ";
      syntax->dump (indent + name.length () + 2);
      cout << ")";
    }
}

const char* 
Library:: name () const
{
  return impl.name;
}

const AttributeList&
Library::lookup (string key) const
{ 
  return impl.lookup (key);
}

bool
Library::check (string key) const
{ 
  return impl.check (key);
}

void
Library::add (string key, const AttributeList& value, const Syntax& syntax)
{
  impl.add (key, value, syntax);
}

const Syntax& 
Library::syntax (string key) const
{ 
  return impl.syntax (key);
}

void
Library::dump (int indent) const
{
  impl.dump (indent);
}

Library::Library (const char *const name) : impl (*new Implementation (name))
{ }

Library::~Library ()
{
    delete &impl;
}
