// printer.C

#include "printer.h"

Librarian<Printer>::Content* Librarian<Printer>::content = NULL;

const char *const Printer::description = "\
The 'printer' component is responsible for converting the internal\n\
format into various internal format.  I.e., it performs the\n\
opposite function of the 'parser' component.  This is used for --\n\
among other things -- creating checkpoints of the state.";

Printer::Printer (const string& n)
  : name (n)
{ }

Printer::~Printer ()
{ }
