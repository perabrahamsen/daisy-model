// library.C

#include "library.h"
#include "alist.h"
#include "syntax.h"
#include <map>

struct Library::Implementation
{
  const string name;
  typedef map<string, AttributeList*, less<string> > alist_map;
  typedef map<string, const Syntax*, less<string> > syntax_map;
  alist_map alists;
  syntax_map syntaxen;
  AttributeList& lookup (string) const;
  bool check (string) const;
  void add (string, AttributeList&, const Syntax&);
  const Syntax& syntax (string) const;
  void dump (int indent) const;
  void entries (vector<string>&) const;
  Implementation (const char* n) 
    : name (n)
    { 
      cerr << "Creating library `" << name << "'.\n";
    }
  ~Implementation ()
    {
      cerr << "Deleting library `" << name << "'.\n";
    }
};

AttributeList&
Library::Implementation::lookup (string key) const
{ 
  alist_map::const_iterator i = alists.find (key);

  if (i == alists.end ())
    THROW (AttributeList::Uninitialized ());

  if (true || name == "crop")
    {
      cout << "(get " << name << " " << key << "\n  ";
      (*i).second->dump (syntax (key), 2);
      cout << ")\n";
    }
  else
    cerr << "Lookup alist for `" << key << "' in library `" << name << "'.\n";

  return *(*i).second;
}

bool
Library::Implementation::check (string key) const
{ 
  cerr << "Check `" << key << "' in library `" << name << "'.\n";
  alist_map::const_iterator i = alists.find (key);

  if (i == alists.end ())
    return false;

  return true;
}

void
Library::Implementation::add (string key, AttributeList& value, const Syntax& syntax)
{
  if (true || name == "crop")
    {
      cout << "(def" << name << " " << key << "\n  ";
      value.dump (syntax, 2);
      cerr << ")\n";
    }
  alists[key] = &value;
  syntaxen[key] = &syntax;
}

const Syntax& 
Library::Implementation::syntax (string key) const
{ 
  cerr << "Lookup syntax for `" << key << "' in library `" << name << "'.\n";
  syntax_map::const_iterator i = syntaxen.find (key);

  if (i == syntaxen.end ())
    THROW (AttributeList::Uninitialized ());

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

void
Library::Implementation::entries (vector<string>& result) const
{
  for (syntax_map::const_iterator i = syntaxen.begin ();
       i != syntaxen.end ();
       i++)
    {
      result.push_back ((*i).first);
    }
}

const string&
Library:: name () const
{
  return impl.name;
}

AttributeList&
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
Library::add (string key, AttributeList& value, const Syntax& syntax)
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

void
Library::entries (vector<string>& result) const
{
  impl.entries (result);
}

Library::Library (const char *const name) : impl (*new Implementation (name))
{ }

Library::~Library ()
{ delete &impl; }
