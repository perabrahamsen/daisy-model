// printer.C

#include "printer.h"
#include "alist.h"

Librarian<Printer>::Content* Librarian<Printer>::content = NULL;

Printer::Printer (const string& n)
  : name (n)
{ }

Printer::~Printer ()
{ }
