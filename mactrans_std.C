// mactrans_std.C -- Standard preferential flow model.

#include "mactrans.h"
#include "soil_water.h"
#include "soil.h"
#include "csmp.h"
#include "mathlib.h"

struct MactransStandard : public Mactrans
{
  // Simulation.
 void tick (const Soil& soil, const SoilWater&,
	    const vector<double>& C,
	    vector<double>& S,
	    vector<double>& S_p,
	    vector<double>& J_p);
  void output (Log& /* log */, Filter& /* filter */) const
    { }

  // Create and Destroy.
  MactransStandard (const AttributeList& al)
    : Mactrans (al)
    { }
  ~MactransStandard ()
    { }
};

void 
MactransStandard::tick (const Soil& soil, const SoilWater& soil_water,
			const vector<double>& C,
			vector<double>& S_m,
			vector<double>& S_p,
			vector<double>& J_p)
{ 
  for (unsigned int i = 0; i < soil.size (); i++)
    {
      // Amount of water entering this layer through macropores.
      const double water_in_above = -soil_water.q_p (i); // [cm]
      const double water_out_below = -soil_water.q_p (i+1); // [cm]
      const double delta_water = water_in_above - water_out_below; // [cm]
      
      // Amount of matter entering this layer through macropores.
      const double matter_in_above = -J_p[i]; // [g/cm^2]
      double delta_matter;	// [g/cm^2]

      if (delta_water < 0.0)
	// Water leaves the layer.
	{
	  // Water leaves with the concentration in this layer.
	  delta_matter = C[i] * delta_water;
	  assert (delta_water <= 0.0);
	}
      else if (delta_water > 0.0)
	// Water enters the layer.
	{
	  // Fraction of water entering the layer through the
	  // macropore, which also stayes here.
	  assert (water_in_above > 0.0);
	  const double water_fraction 
	    = approximate (delta_water, water_in_above) 
	    ? 1.0
	    : delta_water / water_in_above;
	  assert (water_fraction >= 0.0);
	  assert (water_fraction <= 1.0);

	  // Matter stayes with the water.
	  delta_matter = matter_in_above * water_fraction;
	  assert (delta_matter >= 0.0);
	}
      else
	delta_matter = 0.0;

      // Update source with stuff entering the layer.
      S_p[i] = delta_matter / soil.dz (i) / dt;
      S_m[i] += S_p[i];

      // Find amount of stuff leaving the layer.
      if (delta_matter == 0.0)
	J_p[i+1] = J_p[i];
      else if (approximate (matter_in_above, delta_matter))
	J_p[i+1] = 0.0;
      else
	{
	  J_p[i+1] = -(matter_in_above - delta_matter);
	  assert (J_p[i+1] < 0.0);
	}
    }
  
    // Check that the sink terms add up.
  if (fabs (soil.total (S_p) + J_p[0]) > 1.0e-11)
    CERR << "BUG: Total S_p = `" << (soil.total (S_p) + J_p[0])
	 << "' solute\n";
}

static struct MactransStandardSyntax
{
  static Mactrans&
  make (const AttributeList& al)
    { return *new MactransStandard (al); }
  MactransStandardSyntax ()
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "Solute follows water.");
      Librarian<Mactrans>::add_type ("default", alist, syntax, &make);
    }
} MactransStandard_syntax;
