// treelog.C -- Log hierarchical information.

#include "treelog.h"

Treelog::Open::Open (Treelog& l, const string& name)
  : log (l)
{ log.open (name); }

Treelog::Open::~Open ()
{ log.close (); }

void
Treelog::entry (const string&)
{ count++; }

class TreelogNull : public Treelog
{
  // Nesting.
public:
  void open (const string&)
  { }
  void close ()
  { }

  // Create and Destroy.
public:
  TreelogNull ()
  { }
  ~TreelogNull ()
  { }
};

static TreelogNull nulllog;

Treelog&
Treelog::null ()
{ return nulllog; }

Treelog::Treelog ()
{ }

Treelog::~Treelog ()
{ }
