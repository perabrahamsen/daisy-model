// library.C

#include "library.h"
#include "alist.h"
#include "syntax.h"
#include <map>

struct Library::Implementation
{
  typedef map<string, Library*, less<string>/**/> library_map;
  static library_map* all;

  // We give each parsed object an increasing sequence number.
  static int sequence;

  const string name;
  derive_fun derive;
  typedef map<string, AttributeList*, less<string> > alist_map;
  typedef map<string, const Syntax*, less<string> > syntax_map;
  alist_map alists;
  syntax_map syntaxen;
  static void all_entries (vector<string>& libraries);
  AttributeList& lookup (const string&) const;
  bool check (const string&) const;
  void add (const string&, AttributeList&, const Syntax&);
  void remove (const string&);
  const Syntax& syntax (const string&) const;
  void dump (int indent) const;
  void entries (vector<string>&) const;
  static void load_syntax (Syntax&, AttributeList&);
  Implementation (const char* n, derive_fun d) 
    : name (n),
      derive (d)
    { }
  ~Implementation ()
    { all->erase (all->find (name)); }
};

map<string, Library*, less<string> >* Library::Implementation::all;
int Library::Implementation::sequence;

void
Library::Implementation::all_entries (vector<string>& libraries)
{ 
  for (library_map::const_iterator i = all->begin (); 
       i != all->end ();
       i++)
    libraries.push_back ((*i).first); 
}

AttributeList&
Library::Implementation::lookup (const string& key) const
{ 
  alist_map::const_iterator i = alists.find (key);

  if (i == alists.end ())
    assert (false);

  return *(*i).second;
}

bool
Library::Implementation::check (const string& key) const
{ 
  alist_map::const_iterator i = alists.find (key);

  if (i == alists.end ())
    return false;

  return true;
}

void
Library::Implementation::add (const string& key, AttributeList& value,
			      const Syntax& syntax)
{
  alists[key] = &value;
  syntaxen[key] = &syntax;
}

void
Library::Implementation::remove (const string& key)
{
  alists.erase (alists.find (key));
  syntaxen.erase (syntaxen.find (key));
}

const Syntax& 
Library::Implementation::syntax (const string& key) const
{ 
  syntax_map::const_iterator i = syntaxen.find (key);

  if (i == syntaxen.end ())
    assert (false);

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
	  COUT << "\n";
	  for (int j = 0; j < indent; j++)
	    COUT << " ";
	}
      const string& name = (*i).first;
      const Syntax* syntax = (*i).second;
      COUT << "(" << name << " ";
      syntax->dump (indent + name.length () + 2);
      COUT << ")";
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

void
Library::Implementation::load_syntax (Syntax& syntax, AttributeList&)
{
  const string def = "def";
  for (library_map::const_iterator i = all->begin (); 
       i != all->end ();
       i++)
    { 
      const string& name = (*i).first;
      Library& library = *((*i).second);
      syntax.add_library (def + name, library);
    }
}

Library& 
Library::find (const string& name)
{ return *(*Implementation::all)[name]; }

void
Library::all (vector<string>& libraries)
{ 
  Implementation::all_entries (libraries);
}

int
Library::get_sequence ()
{ 
  Implementation::sequence++;
  // Nobody will ever need more than two billion objects --- Per 1998.
  assert (Implementation::sequence > 0);
  return Implementation::sequence;
}

const string&
Library:: name () const
{ return impl.name; }

AttributeList&
Library::lookup (const string& key) const
{ return impl.lookup (key); }

bool
Library::check (const string& key) const
{ return impl.check (key); }

void
Library::add (const string& key, AttributeList& value, const Syntax& syntax)
{ impl.add (key, value, syntax); }

void 
Library::add_derived (const string& name, AttributeList& al,
		      const string& super)
{ impl.derive (name, al, super); }

void
Library::remove (const string& key)
{ impl.remove (key); }

const Syntax& 
Library::syntax (const string& key) const
{ return impl.syntax (key); }

void
Library::dump (int indent) const
{ impl.dump (indent); }

void
Library::entries (vector<string>& result) const
{ impl.entries (result); }

void 
Library::load_syntax (Syntax& syntax, AttributeList& alist)
{ Implementation::load_syntax (syntax, alist); }

Library::Library (const char *const name, derive_fun derive) 
  : impl (*new Implementation (name, derive))
{ 
  if (Implementation::all == NULL)
    // Buglet: we never delete this.
    Implementation::all = new map<string, Library*, less<string> >;
  (*Implementation::all)[name] = this; 
}

Library::~Library ()
{ delete &impl; }
