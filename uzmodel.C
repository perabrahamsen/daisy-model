// uzmodel.C

#include "uzmodel.h"

UZtop::~UZtop ()
{ }

UZbottom::~UZbottom ()
{ }

UZmodel::UZmodel (string n)
  : name (n)
{ }

UZmodel::~UZmodel ()
{ }

Librarian<UZmodel>::Content* Librarian<UZmodel>::content = NULL;

const char *const UZmodel::description = "\
The `uzmodel' component handles the vertical water movement in the\n\
unsaturated zone.";
