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

