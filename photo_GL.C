// photo_GL.C -- Leaf photosynthesis based on Goudriaan and Laar, 1978.
// 
// Copyright 1996-2001,2005 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001,2005 KVL.
//
// This file is part of Daisy.
// 
// Daisy is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser Public License as published by
// the Free Software Foundation; either version 2.1 of the License, or
// (at your option) any later version.
// 
// Daisy is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser Public License for more details.
// 
// You should have received a copy of the GNU Lesser Public License
// along with Daisy; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

#include "photo.h"
#include "block.h"
#include "canopy_std.h"
#include "phenology.h"
#include "plf.h"
#include "alist.h"
#include "syntax.h"
#include "submodel.h"
#include "mathlib.h"
#include <sstream>
#include "check.h"

using namespace std;

// Chemical constants affecting the crop.
const double molWeightCH2O = 30.0; // [gCH2O/mol]
const double molWeightCO2 = 44.0; // [gCO2/mol]

class PhotoGL : public Photo
{
  // Parameters.
private:
    double Qeff;	// Quantum efficiency at low light
    double Fm;		// Max assimilation rate
    const PLF& TempEff;	// Temperature effect, photosynthesis
    const PLF& DSEff;	// Development stage effect, photosynthesis
    const PLF& DAPEff;	// Age effect, photosynthesis

  // Simulation.
public:
  double assimilate (const double, const double, 
		     double Ta, const double cropN,
                     const std::vector<double>& PAR,
                     const std::vector<double>& PAR_Height,
                     const double PAR_LAI,
		     const std::vector<double>& fraction,
                     CanopyStandard& canopy,
                     Phenology& development, Treelog&);
  void output (Log&) const
  { }
  
  // Create and Destroy.
public:
  PhotoGL (Block& al)
    : Photo (al),
      Qeff (al.number ("Qeff")),
      Fm (al.number ("Fm")),
      TempEff (al.plf ("TempEff")),
      DSEff (al.plf ("DSEff")),
      DAPEff (al.plf ("DAPEff"))
  { }
  ~PhotoGL ()
  { }
};

double
PhotoGL::assimilate (const double,  const double, 
		     const double Ta, const double,
                     const vector<double>& PAR,
                     const vector<double>& PAR_height,
                     const double PAR_LAI,
		     const std::vector<double>& ,
                     CanopyStandard& canopy,
                     Phenology& development,
                     Treelog& msg) 
{
  // sugar production [gCH2O/m2/h] by canopy photosynthesis.
  const PLF& LAIvsH = canopy.LAIvsH;
  const double DS = development.DS;
  const double DAP = development.DAP;

  // Temperature effect and development stage effect
  const double Teff = TempEff (Ta) * DSEff (DS) * DAPEff (DAP);

  // One crop: daisy_assert (approximate (canopy.CAI, bioclimate.CAI ()));
  if (!approximate (LAIvsH (canopy.Height), canopy.CAI))
    {
      std::ostringstream tmp;
      tmp << "Bug: CAI below top: " << LAIvsH (canopy.Height)
	     << " Total CAI: " << canopy.CAI << "\n";
      canopy.CanopyStructure (DS);
      tmp << "Adjusted: CAI below top: " << LAIvsH (canopy.Height)
	     << " Total CAI: " << canopy.CAI;
      msg.error (tmp.str ());
    }

 // CAI below the current leaf layer.
  double prevLA = LAIvsH (PAR_height[0]);
  // Assimilate produced by canopy photosynthesis
  double Ass = 0.0;
  // Accumulated CAI, for testing purposes.
  double accCAI =0.0;
  // Number of computational intervals in the canopy.
  const int No = PAR.size () - 1;
  daisy_assert (No > 0);
  daisy_assert (No == PAR_height.size () - 1);

  // CAI in each interval.
  const double dCAI = PAR_LAI / No;

  // True, if we haven't reached the top of the crop yet.
  bool top_crop = true;

  for (int i = 0; i < No; i++)
    {
      const double height = PAR_height[i+1];
      daisy_assert (height < PAR_height[i]);

      if (top_crop && height <= canopy.Height)
	{
	  // We count day hours at the top of the crop.
	  top_crop = false;
	  if (PAR[i] > 0.5 * 25.0)
	    development.light_hour ();
	}
      // Leaf Area index for a given leaf layer
      const double LA = prevLA - LAIvsH (height);
      daisy_assert (LA >= 0.0);
      if (LA > 0)
	{
	  prevLA = LAIvsH (height);
	  accCAI += LA;

	  const double dPAR = (PAR[i] - PAR[i+1]) / dCAI;

	  // Leaf Photosynthesis [gCO2/m2/h]
	  const double F = Fm * (1.0 - exp (- (Qeff * dPAR / Fm)));

	  Ass += LA * F;
	}
    }
  daisy_assert (approximate (accCAI, canopy.CAI));

  return (molWeightCH2O / molWeightCO2) * Teff * Ass;
}

const AttributeList& 
Photo::default_model ()
{
  static AttributeList alist;
  
  if (!alist.check ("type"))
    {
      PLF DS_null_eff;
      DS_null_eff.add (0.0, 1.00);
      DS_null_eff.add (2.0, 1.00);
      alist.add ("DSEff",DS_null_eff);
      alist.add ("DAPEff",DS_null_eff);

      alist.add ("used_to_be_a_submodel", true);
      alist.add ("type", "GL");
    }
  return alist;
}

static struct Photo_GLSyntax
{
  static Photo&
  make (Block& al)
  { return *new PhotoGL (al); }
  Photo_GLSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    PLF DS_null_eff;
    DS_null_eff.add (0.0, 1.00);
    DS_null_eff.add (2.0, 1.00);

    alist.add ("description", "Goudriaan and Laar, 1978.");

    syntax.add ("Qeff", "(g CO2/m^2/h)/(W/m^2)", Syntax::Const,
                "Quantum efficiency at low light.");
    syntax.add ("Fm", "g CO2/m^2/h", Check::positive (), Syntax::Const,
                "Maximum assimilation rate.");
    syntax.add ("TempEff", "dg C", Syntax::None (), Check::non_negative (),
                Syntax::Const,
                "Temperature factor for assimilate production.");
    syntax.add ("DSEff", "DS", Syntax::None (), Check::non_negative (),
                Syntax::Const, "\
Development stage factor for assimilate production.");
    alist.add ("DSEff",DS_null_eff);
    syntax.add ("DAPEff", "d", Syntax::None (), Check::non_negative (),
                Syntax::Const, "Age factor for assimilate production.\n\
Age is given as day after planting.");
    alist.add ("DAPEff",DS_null_eff);

    Librarian<Photo>::add_type ("GL", alist, syntax, &make);
  }
} PhotoGL_syntax;
