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
