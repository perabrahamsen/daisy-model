// groundwater_lysimeter.C

#include "groundwater.h"

class GroundwaterLysimeter : public Groundwater
{
  // UZbottom.
public:
  bool flux_bottom () const
  { return true; }
  bool accept_bottom (double)
  { return true; }
  bool is_lysimeter () const
  { return true; }

  // Simulation.
public:
  void tick (const Time&)
  { }
  double table () const
  { return 1.0; }

  // Create and Destroy.
public:
  GroundwaterLysimeter (const AttributeList& al)
    : Groundwater (al)
  { }
  ~GroundwaterLysimeter ()
  { }
};

static struct GroundwaterLysimeterSyntax
{
  static Groundwater& make (const AttributeList& al)
  { return *new GroundwaterLysimeter (al); }

  GroundwaterLysimeterSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("descriptions", "Lysimeter bottom.");
    Groundwater::load_syntax (syntax, alist);
    Librarian<Groundwater>::add_type ("lysimeter", alist, syntax, &make);
  }
} GroundwaterLysimeter_syntax;


