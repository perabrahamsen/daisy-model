// column.C

#include "column.h"
#include "library.h"
#include "alist.h"
#include "syntax.h"
#include <map>

static Library* Column_library = NULL;
typedef map<string, Column::constructor, less<string> > Column_map_type;
static Column_map_type* Column_constructors;

const Library&
Column::library ()
{
  assert (Column_library);
  return *Column_library;
}

void
Column::add_type (const string name, 
		  const AttributeList& al, 
		  const Syntax& syntax,
		  constructor cons)
{
  assert (Column_library);
  Column_library->add (name, al, syntax);
  Column_constructors->insert(Column_map_type::value_type (name, cons));
}

void 
Column::derive_type (string name, const AttributeList& al, string super)
{
  add_type (name, 
	    al, library ().syntax (super),
	    (*Column_constructors)[super]);
}

Column*
Column::create (const AttributeList& al)
{
  assert (al.check ("type"));
  string name = al.name ("type");
  assert (library ().check (name));
  assert (library ().syntax (name).check (al));
  return (*Column_constructors)[name](al);
}

Column::Column (string n)
  : name (n)
{ }

Column::~Column ()
{ }

ColumnList::ColumnList (const vector<const AttributeList*>& sequence)
{
  for (vector<const AttributeList*>::const_iterator i = sequence.begin ();
       i != sequence.end ();
       i++)
    push_back (Column::create (**i));
}

ColumnList::~ColumnList ()
{
  for (const_iterator i = begin (); i != end (); i++)
    delete *i;
}

int Column_init::count;

Column_init::Column_init ()
{ 
  if (count++ == 0)
    {
      Column_library = new Library ();
      Column_constructors = new Column_map_type ();
    }
  assert (count > 0);
}

Column_init::~Column_init ()
{ 
  if (--count == 0)
    {
      delete Column_library;
      Column_library = NULL;
      delete Column_constructors;
      Column_constructors = NULL;
    }
  assert (count >= 0);
}
