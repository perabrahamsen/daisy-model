// groundwater_deep.C --- Free drainage.

#include "groundwater.h"

class GroundwaterDeep : public Groundwater
{
  // UZbottom.
public:
  bool flux_bottom () const
  { return true; }
  bool accept_bottom (double)
  { return true; }

  // Simulation.
public:
  void tick (const Time&)
  { }
  double table () const
  { return 42.42e42; }

  // Create and Destroy.
public:
  GroundwaterDeep (const AttributeList& al)
    : Groundwater (al)
  { }
  ~GroundwaterDeep ()
  { }
};

static struct GroundwaterDeepSyntax
{
  static Groundwater& make (const AttributeList& al)
  { return *new GroundwaterDeep (al); }

  GroundwaterDeepSyntax ()
    { 
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "Deep groundwater, free drainage.");
      Groundwater::load_syntax (syntax, alist);
      Librarian<Groundwater>::add_type ("deep", alist, syntax, &make);
    }
} GroundwaterDeep_syntax;
