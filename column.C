// column.C

#include "column.h"
#include "log.h"
#include <map>

Librarian<Column>::Content* Librarian<Column>::content = NULL;

const char *const Column::description = "\
A `column' is an one-dimensional vertical description of the\n\
soil/crop/atmosphere system.  The column component contains most of\n\
the other processes in Daisy as submodels.";

void
Column::output (Log& log, Filter& filter) const
{
  log.output ("size", filter, size);
}

void
Column::load_syntax (Syntax& syntax, AttributeList& alist)
{
  syntax.add ("size", Syntax::Unknown (), Syntax::State,
	      "Area covered by this column, for use by the `merge' action.\n\
The dimension is up to you, as long as all columns use the same unit.");
  alist.add ("size", 1.0);
}

Column::Column (const AttributeList& al)
  : alist (al),
    name (al.name ("type")),
    size (alist.number ("size"))
{ }

Column::~Column ()
{ }

