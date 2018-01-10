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

#define BUILD_DLL
#include "photo.h"
#include "block_model.h"
#include "canopy_std.h"
#include "phenology.h"
#include "plf.h"
#include "frame.h"
#include "mathlib.h"
#include <sstream>
#include "check.h"
#include "librarian.h"
#include "treelog.h"

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
  double assimilate (const Units&, const double, const double, const double, 
                     const double, 
                     const double, const double, const double, 
		     double Ta, double Tc, double Tl, const double cropN,
                     const std::vector<double>& PAR,
                     const std::vector<double>& PAR_Height,
                     const double PAR_LAI,
		     const std::vector<double>& fraction,
                     double dt,
                     CanopyStandard& canopy,
                     Phenology& development, Treelog&);
  double stomata_conductance() const
  { return -1.; } // [mol m^-2 s^-1]

  void output (Log&) const
  { }
  
  // Create and Destroy.
public:
  bool handle_N_stress () const
  { return false; }
  bool handle_water_stress () const
  { return false; }
  static void load_syntax (Frame&);
  PhotoGL (const BlockModel& al)
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
PhotoGL::assimilate (const Units&, const double, const double, const double, 
                     const double, const double, const double, const double, 
		     const double Ta, const double, const double, const double,
                     const std::vector<double>& PAR,
                     const std::vector<double>& PAR_height,
                     const double PAR_LAI,
		     const std::vector<double>& fraction,
                     const double,
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

  for (int i = 0; i < No; i++)
    {
      const double height = PAR_height[i+1];
      daisy_assert (height < PAR_height[i]);

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

	  Ass += /* fraction[i] * */ LA * F;
	}
    }
  daisy_assert (approximate (accCAI, canopy.CAI));

  return (molWeightCH2O / molWeightCO2) * Teff * Ass;
}

static struct Photo_GLSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new PhotoGL (al); }
  void load_frame (Frame& frame) const
  { 
    PLF DS_null_eff;
    DS_null_eff.add (0.0, 1.00);
    DS_null_eff.add (2.0, 1.00);

    frame.declare ("Qeff", "(g CO2/m^2/h)/(W/m^2)", Attribute::Const,
                "Quantum efficiency at low light.");
    frame.declare ("Fm", "g CO2/m^2/h", Check::positive (), Attribute::Const,
                "Maximum assimilation rate.");
    frame.declare ("TempEff", "dg C", Attribute::None (), Check::non_negative (),
                Attribute::Const,
                "Temperature factor for assimilate production.");
    frame.declare ("DSEff", "DS", Attribute::None (), Check::non_negative (),
                Attribute::Const, "\
Development stage factor for assimilate production.");
    frame.set ("DSEff",DS_null_eff);
    frame.declare ("DAPEff", "d", Attribute::None (), Check::non_negative (),
                Attribute::Const, "Age factor for assimilate production.\n\
Age is given as day after planting.");
    frame.set ("DAPEff",DS_null_eff);
  }
  Photo_GLSyntax () 
    : DeclareModel (Photo::component, "GL", "Goudriaan and Laar, 1978.")
  { }
} PhotoGL_syntax;

// photo_GL.C ends here.
