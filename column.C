// column.C

#include "column.h"
#include "alist.h"
#include "syntax.h"
#include "library.h"

#include <map.h>

static Library* Column_par_library = NULL;
static Library* Column_var_library = NULL;
typedef map<string, Column::constructor, less<string> > Column_map_type;
static Column_map_type* Column_constructors;

const Library&
Column::par_library ()
{
  assert (Column_par_library);
  return *Column_par_library;
}

const Library&
Column::var_library ()
{
  assert (Column_var_library);
  return *Column_var_library;
}

void
Column::add_type (const string name, 
		  const AttributeList& parList, 
		  const Syntax* parSyntax,
		  const AttributeList& varList, 
		  const Syntax* varSyntax,
		  constructor cons)
{
  assert (Column_par_library);
  assert (Column_var_library);
  Column_par_library->add (name, parList, parSyntax);
  Column_var_library->add (name, varList, varSyntax);
  Column_constructors->insert(Column_map_type::value_type (name, cons));
}

void 
Column::derive_type (string name, const AttributeList& al, string super)
{
  add_type (name, 
	    al, par_library ().syntax (super),
	    var_library ().lookup (super), var_library ().syntax (super),
	    (*Column_constructors)[super]);
}

Column*
Column::create (const string name, const AttributeList& var)
{
  assert (par_library ().check (name));
  return (*Column_constructors)[name]
    (name, par_library ().lookup (name), var);
}

Column::Column (string n)
  : name (n)
{ }

Column::~Column ()
{ }

int Column_init::count;

Column_init::Column_init ()
{ 
  if (count++ == 0)
    {
      Column_par_library = new Library ();
      Column_var_library = new Library ();
      Column_constructors = new Column_map_type ();
    }
  assert (count > 0);
}

Column_init::~Column_init ()
{ 
  if (--count == 0)
    {
      delete Column_par_library;
      Column_par_library = NULL;
      delete Column_var_library;
      Column_var_library = NULL;
      delete Column_constructors;
      Column_constructors = NULL;
    }
  assert (count >= 0);
}
