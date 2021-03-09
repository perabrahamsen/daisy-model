// difrad_DPF.C -- Diffuse radiation using the model of De Pury and Farquhar, 1997.
// 
// Copyright 2006 Birgitte Gjettermann and KVL
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
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.See the
// GNU Lesser Public License for more details.
// 
// You should have received a copy of the GNU Lesser Public License
// along with Daisy; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

#define BUILD_DLL

#include "difrad.h"
#include "block_model.h"
#include "weather.h"
#include "fao.h"
#include "mathlib.h"
#include "check.h"
#include "librarian.h"
#include "frame.h"
#include <sstream>

struct DifradDPF : public Difrad
{
  // Parameters.
  private:
  const double fa; // The proportion of attenuated radiation that reaches the surface as diffuse radiation [fraction]
  const double a;  // Atmospheric transmission coefficeint of PAR []

  // Simulation.
  double value (const Time& time, const Weather& weather, Treelog&)
  {
    // Solar elevation angle and atmospheric pressure
    const double sin_beta = weather.sin_solar_elevation_angle ();

    if (sin_beta < 0.01)
      return 1.0;

    const double P = weather.air_pressure (); //[Pa]
   
    // Atmospheric pressure at sea level
    const double P0 = 1.013E5; //[Pa]
    // The optical air mass, m:
    const double m = (P/P0)/sin_beta; // []
       
    // Extra-terrestrial PAR from weather.C
    const double I_e = weather.extraterrestrial_radiation ();//[W/m^2]
   
    // Beam PAR calculated from extra-terrestrial PAR
    const double I_b = pow(a, m) * I_e * sin_beta;
   
    // Diffuse radiation under a cloudless sky
    const double I_d = fa * (1 - pow(a, m)) * I_e * sin_beta;
   
    // Fraction of diffuse radiation
    const double I_total = I_d + I_b;
    if (iszero (I_total))
      return 1.0;
    const double fd = I_d /(I_total);
    daisy_assert (std::isfinite (fd));
    return bound (0.0, fd, 1.0); 
  }

  void output (Log& log) const
  {
    Difrad::output (log);
  }

// Create and Destroy.
  public:
  DifradDPF (const BlockModel& al)
    : Difrad (al),
       fa (al.number ("fa")),
       a (al.number ("a"))
    { }
  ~DifradDPF ()
    { }
};

static struct DifradDPFSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new DifradDPF (al); }
  DifradDPFSyntax ()
    : DeclareModel (Difrad::component, "DPF", "\
Diffuse radiation calculated using the model of De Pury and Farquhar, 1997.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.set_strings ("cite", "pf1997simple");
    frame.declare ("fa", Attribute::Fraction (), Check::positive (), Attribute::Const, "\
Diffuse radiation proportion.\n\
Proportion of attenuated radiation that reaches the surface as diffuse\n\
radiation.");
    frame.set ("fa", 0.5);

    frame.declare ("a", Attribute::None (), Check::positive (), Attribute::Const, "\
Atmospheric transmission coefficient of PAR.\n\
Value around 0.6-0.9 depending on dust particles.");
    frame.set ("a", 0.84);
    
  }
} DifradDPF_syntax;
