// library.C

#include "library.h"
#include "alist.h"
#include "syntax.h"
#include <map>

struct Library::Implementation
{
  static map<string, Library*, less<string>/**/>* all;

  const string name;
  typedef map<string, AttributeList*, less<string> > alist_map;
  typedef map<string, const Syntax*, less<string> > syntax_map;
  alist_map alists;
  syntax_map syntaxen;
  static void all_entries (vector<string>& libraries);
  AttributeList& lookup (string) const;
  bool check (string) const;
  void add (string, AttributeList&, const Syntax&);
  void remove (string);
  const Syntax& syntax (string) const;
  void dump (int indent) const;
  void entries (vector<string>&) const;
  Implementation (const char* n) 
    : name (n)
    { }
  ~Implementation ()
    { all->erase (all->find (name)); }
};

map<string, Library*, less<string> >* Library::Implementation::all;

void
Library::Implementation::all_entries (vector<string>& libraries)
{ 
  for (map<string, Library*, less<string> >::const_iterator i = all->begin (); 
       i != all->end ();
       i++)
    libraries.push_back ((*i).first); 
}

AttributeList&
Library::Implementation::lookup (string key) const
{ 
  alist_map::const_iterator i = alists.find (key);

  if (i == alists.end ())
    THROW (AttributeList::Uninitialized ());

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
Library::Implementation::add (string key, AttributeList& value,
			      const Syntax& syntax)
{
  alists[key] = &value;
  syntaxen[key] = &syntax;
}

void
Library::Implementation::remove (string key)
{
  alists.erase (alists.find (key));
  syntaxen.erase (syntaxen.find (key));
}

const Syntax& 
Library::Implementation::syntax (string key) const
{ 
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

Library& 
Library::find (string name)
{ return *(*Implementation::all)[name]; }

void
Library::all (vector<string>& libraries)
{ 
  Implementation::all_entries (libraries);
}

const string&
Library:: name () const
{ return impl.name; }

AttributeList&
Library::lookup (string key) const
{ return impl.lookup (key); }

bool
Library::check (string key) const
{ return impl.check (key); }

void
Library::add (string key, AttributeList& value, const Syntax& syntax)
{ impl.add (key, value, syntax); }

void
Library::remove (string key)
{ impl.remove (key); }

const Syntax& 
Library::syntax (string key) const
{ return impl.syntax (key); }

void
Library::dump (int indent) const
{ impl.dump (indent); }

void
Library::entries (vector<string>& result) const
{ impl.entries (result); }

Library::Library (const char *const name) : impl (*new Implementation (name))
{ 
  if (Implementation::all == NULL)
    // Buglet: we never delete this.
    Implementation::all = new map<string, Library*, less<string> >;
  (*Implementation::all)[name] = this; 
}

Library::~Library ()
{ delete &impl; }
