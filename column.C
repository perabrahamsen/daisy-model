// column.C

#include "column.h"
#include "library.h"
#include "alist.h"
#include "syntax.h"
#include <map>

Librarian<Column>::Content* Librarian<Column>::content = NULL;

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
    push_back (&Librarian<Column>::create (**i));
}

ColumnList::~ColumnList ()
{
  // Borland C++ don't want a const_iterator here.
  for (iterator i = begin (); i != end (); i++)
    delete *i;
}
