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

Treelog&
Treelog::null ()
{
  static class TreelogNull : public Treelog
  {
    // Nesting.
  public:
    virtual void open (const string&)
    { }
    virtual void close ()
    { }

    // Create and Destroy.
  public:
    TreelogNull ()
    { }
    ~TreelogNull ()
    { }
  } nulllog;

  return nulllog;
}

Treelog::Treelog ()
{ }

Treelog::~Treelog ()
{ }
