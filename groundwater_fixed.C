// groundwater_fixed.C --- Fixed, high groundwater level.

#include "groundwater.h"

class GroundwaterFixed : public Groundwater
{
  // Content.
private:
  const double depth;
  
  // UZbottom.
public:
  bool flux_bottom () const
  { return false; }
  bool accept_bottom (double)
  { return true; }

  // Simulation.
public:
  void tick (const Time&)
  { }
  double table () const
  { return depth; }

  // Create and Destroy.
public:
  GroundwaterFixed (const AttributeList& al)
    : Groundwater (al),
      depth (al.number ("table"))
  { }
  ~GroundwaterFixed ()
  { }
};

static struct GroundwaterFixedSyntax
{
  static Groundwater& make (const AttributeList& al)
  { return *new GroundwaterFixed (al); }
  GroundwaterFixedSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Fixed high groundwater level.");
    Groundwater::load_syntax (syntax, alist);
    syntax.add ("table", "cm", Syntax::Const,
		"Groundwater level.\n\
Positive numbers indicate free drainage.");
    syntax.order ("table");
    Librarian<Groundwater>::add_type ("fixed", alist, syntax, &make);
  }
} GroundwaterFixed_syntax;


