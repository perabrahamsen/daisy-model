// raddist_DPF.C -- Radiation distribution sun-shade model of De Pury & Farquhar (1997)
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


#include "raddist.h"
#include "syntax.h"
#include "vegetation.h"
#include "weather.h"
#include "mathlib.h"
#include <sstream>

using namespace std;

struct RaddistDPF : public Raddist
{
  // Simulation.
  void tick (std::vector <double>& fraction_sun_LAI, std::vector <double>& sun_PAR, 
	     std::vector <double>& total_PAR, double global_radiation, 
	     double diffuse_radiation, double sin_beta, const Vegetation&, Treelog&);
  void output (Log& log) const
  {
    Raddist::output (log);
  }

  // Create.
  RaddistDPF (Block& al)
    : Raddist (al)
  { }
};

void RaddistDPF::tick (std::vector <double>& fraction_sun_LAI,
		       std::vector <double>& sun_PAR, 
		       std::vector <double>& total_PAR, const double global_radiation, 
		       const double diffuse_radiation, const double sin_beta,
		       const Vegetation& vegetation,
		       Treelog&)
{
  daisy_assert (std::isfinite (diffuse_radiation));
  daisy_assert (std::isfinite (global_radiation));
  const size_t No = fraction_sun_LAI.size ();
  daisy_assert (No + 1 == total_PAR.size());
  daisy_assert (No + 1 == sun_PAR.size());

  const double LAI = vegetation.LAI ();

  // No LAI
  if (iszero (LAI))
  {
    std::fill (&total_PAR[0], &total_PAR[No+1], 0.0);
    std::fill (&sun_PAR[0], &sun_PAR[No+1], 0.0); 
    std::fill (&fraction_sun_LAI[0], &fraction_sun_LAI[No], 0.0);
    return;
  }
  // Vectors for calculation
  std::vector<double> beam_PAR (No+1, 0.0);
  std::vector<double> dif_PAR (No+1, 0.0);
  std::vector<double> dir_beam_PAR (No+1, 0.0);
  std::vector<double> beam_scat1_PAR (No+1, 0.0);
  std::vector<double> beam_scat2_PAR (No+1, 0.0);
  std::vector<double> dif_sun_PAR (No+1, 0.0);
  
  // Beam radiation above the canopy:
  const double IRb0 = global_radiation-diffuse_radiation;
  // Diffuse radiation above the canopy:
  const double IRd0 = diffuse_radiation;

 // Leaf scattering coefficient of PAR
  const double o = 0.15; //wheat
  // Extinction coefficient of beam and scattered beam and diffuse scattered PAR, respectively 
  daisy_assert (std::isnormal (sin_beta));
  double kb  = 0.50 / sin_beta;
  if(kb > 8.0) 
    kb = 8.0;
  if(kb < 0.0)
    kb = 8.0;
  const double kbs = kb *sqrt(1.0-o); // = 0.46 / sin_beta;
  const double kds = 0.719;
  // Reflection coefficient of beam and diffuse PAR, respectively
   const double Pcd = 0.036;
   //  const double Ph = 1.0-sqrt(1.0 - o)/(1.0+sqrt(1.0-o));
   const double one_plus_kb = 1.0+kb;
   daisy_assert (std::isnormal (one_plus_kb));
   const double Pcb = 0.029; //1.0-exp((-2.0*Ph*kb)/one_plus_kb);
  
  
  // Total PAR
  // Fill beam PAR (cummulative)
   daisy_assert (std::isfinite (IRb0));
  radiation_distribution (No, LAI, Pcb, IRb0, kbs, beam_PAR);
  assert_non_negative (beam_PAR);
  // Fill diffuse PAR (cummulative)
  radiation_distribution (No, LAI, Pcd, IRd0, kds, dif_PAR);
  assert_non_negative (dif_PAR);

  // Sunlit PAR
  // Fill direct beam PAR without scattering (cummulative)
  radiation_distribution (No, LAI, o, IRb0, kb, dir_beam_PAR);
  // Fill direct beam PAR with scattering 1 (cummulative)
  radiation_distribution (No, LAI, Pcb, IRb0, kbs+kb, beam_scat1_PAR);
  // Fill direct beam PAR with scattering 1 (cummulative)
  radiation_distribution (No, LAI, o, IRb0, 2*kb, beam_scat2_PAR);
  // Fill diffuse PAR sunlit (cummulative)
  radiation_distribution (No, LAI, Pcd, IRd0, kds+kb, dif_sun_PAR);

  for (int i = 0; i <= No; i++)
    {
      const double kds_plus_kb = kds + kb;
      daisy_assert (std::isnormal (kds_plus_kb));
      const double kbs_plus_kb = kbs + kb;
      daisy_assert (std::isnormal (kbs_plus_kb));
      sun_PAR[i] = std::max (0.0, 
			     dir_beam_PAR[i] 
			     + (beam_scat1_PAR[i]*(kbs /kbs_plus_kb))
			     - (beam_scat2_PAR[i] / 2.0)
			     + (dif_sun_PAR[i]*(kds /(kds_plus_kb))));
      daisy_assert (sun_PAR[i] >= 0.0);
      daisy_assert (dif_PAR[i] >= 0.0);
      daisy_assert (beam_PAR[i] >= 0.0);

      total_PAR[i] = dif_PAR[i] + beam_PAR[i];
      daisy_assert (total_PAR[i] >= 0.0);
    }
  
  const double dLAI = LAI/(No + 0.);

  for (int i = 0; i <= No - 1; i++)
    {
      fraction_sun_LAI[i]=(exp(-kb * dLAI*i));
      daisy_assert (fraction_sun_LAI[i] >= 0.0);    
      daisy_assert (fraction_sun_LAI[i] <= 1.0);
    }
}

static struct RaddistDPFSyntax
{
  static Raddist&
  make (Block& al)
  { return *new RaddistDPF (al); }
  RaddistDPFSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", 
	       "Sun-shade model of radiation distribution in the canopy.");
    Raddist::load_syntax (syntax, alist);
    Librarian<Raddist>::add_type ("sun-shade", alist, syntax, &make);
  }
} RaddistDPF_syntax;


