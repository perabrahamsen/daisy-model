// svat_none.C -- No production stress.

#include "svat.h"

struct SVAT_none : public SVAT
{
  // Simulation.
  void tick (const Weather&, const Vegetation&,
	     const Surface&, const Soil&, const SoilHeat&,
	     const SoilWater&, const Pet&,
	     double /* canopy_ea */, double /* snow_ea */,
	     double /* pond_ea */, double /* soil_ea */,
             double /* crop_ea */, double /* crop_ep */)
  { }
  double production_stress () const
  { return -1; }

  // Create.
  SVAT_none (const AttributeList& al)
    : SVAT (al)
  { }
};

static struct SVAT_NoneSyntax
{
  static SVAT&
  make (const AttributeList& al)
  { return *new SVAT_none (al); }
  SVAT_NoneSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    SVAT::load_syntax (syntax, alist);
    Librarian<SVAT>::add_type ("none", alist, syntax, &make);
  }
} SVAT_none_syntax;
