// column.C

#include "column.h"
#include "library.h"
#include "alist.h"
#include "syntax.h"
#include <map>

Librarian<Column>::Content* Librarian<Column>::content = NULL;

Column::Column (const string& n)
  : name (n)
{ }

Column::~Column ()
{ }

const Column* 
ColumnList::find (const string& name) const
{
  for (const_iterator i = begin (); i != end (); i++)
    if ((*i)->name == name)
      return *i;
  return NULL;
}

ColumnList::ColumnList (const vector<AttributeList*>& sequence)
{
  for (vector<AttributeList*>::const_iterator i = sequence.begin ();
       i != sequence.end ();
       i++)
    push_back (&Librarian<Column>::create (**i));
}

ColumnList::~ColumnList ()
{
  // Borland C++ don't want a const_iterator here.
  for (iterator i = begin (); i != end (); i++)
    delete *i;
}
