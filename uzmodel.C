// uzmodel.C

#include "uzmodel.h"

bool
UZtop::soil_top () const
{ return false; }

UZtop::~UZtop ()
{ }

bool 
UZbottom::is_lysimeter () const
{ return false; }

UZbottom::~UZbottom ()
{ }

void
UZmodel::has_macropores (bool)
{ }

UZmodel::UZmodel (string n)
  : name (n)
{ }

UZmodel::~UZmodel ()
{ }

template<>
Librarian<UZmodel>::Content* Librarian<UZmodel>::content = NULL;

const char *const UZmodel::description = "\
The 'uzmodel' component handles the vertical water movement in the\n\
unsaturated zone.";
