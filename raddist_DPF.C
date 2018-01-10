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

#define BUILD_DLL

#include "raddist.h"
#include "block_model.h"
#include "vegetation.h"
#include "weather.h"
#include "mathlib.h"
#include "check.h"
#include "librarian.h"
#include "log.h"
#include "frame.h"
#include "plf.h"
#include <sstream>

struct RaddistDPF : public Raddist
{
  // Parameters.
private:
  const double sigma_PAR;//  Leaf scattering coefficient of PAR []
  const double sigma_NIR;//  Leaf scattering coefficient of NIR []
  const double Ps_PAR;  // Soil reflection coefficient of PAR []
  const double Ps_NIR;  // Soil reflection coefficient of NIR []
  const PLF Ps_PAR_SWE;	// Soil water effect on PAR, [pF] -> []
  const PLF Ps_NIR_SWE;	// Soil water effect on PAR, [pF] -> []

public:
  // Log variables
  double IRb0; // Beam radiation above the canopy []
  double IRd0; // Diffuse radiation above the canopy []
  double Ph_PAR; // Canopy reflection coefficeint of beam PAR for horizontal leaves []
  double Pcb_PAR;// Canopy reflection coefficeint of beam PAR for 
                 // uniform leaf-angel distribution
  double Pscb_PAR;  // Canopy-soil reflection coefficeint of beam PAR for 
                    // uniform leaf-angel distribution []
  double Pscd_PAR;  // Canopy-soil reflection coefficeint of diffuse PAR for 
                    // uniform leaf-angel distribution []

  double Ph_NIR;   // Canopy reflection coefficeint of beam NIR for horizontal leaves
  double Pcb_NIR;  // Canopy reflection coefficeint of beam NIR for 
                   // uniform leaf-angel distribution []
  double Pscb_NIR; // Canopy-soil reflection coefficeint of beam NIR for 
                   // uniform leaf-angel distribution []
  double Pscd_NIR; // Canopy-soil reflection coefficeint of diffuse NIR for 
                   // uniform leaf-angel distribution []

  // Simulation.
  void tick (std::vector <double>& fraction_sun_LAI,
             std::vector <double>& sun_PAR, std::vector <double>& total_PAR, 
             std::vector <double>& sun_NIR, std::vector <double>& total_NIR, 
             const double global_radiation, const double diffuse_radiation, 
             const double min_sin_beta, const double sin_beta,
             const Vegetation&, const double pF, Treelog&);

  void output(Log& log) const;

  // Create.
  RaddistDPF (const BlockModel& al)
    : Raddist (al),
      sigma_PAR (al.number ("sigma_PAR")),
      sigma_NIR (al.number ("sigma_NIR")),
      Ps_PAR (al.number ("Ps_PAR")),
      Ps_NIR (al.number ("Ps_NIR")),
      Ps_PAR_SWE (al.plf ("Ps_PAR_SWE")),
      Ps_NIR_SWE (al.plf ("Ps_NIR_SWE")),
      IRb0(-42.42e42),
      IRd0(-42.42e42),
      Ph_PAR(-42.42e42),
      Pcb_PAR(-42.42e42),
      Pscb_PAR(-42.42e42),
      Pscd_PAR(-42.42e42),
      Ph_NIR(-42.42e42),
      Pcb_NIR(-42.42e42),
      Pscb_NIR(-42.42e42),
      Pscd_NIR(-42.42e42)
  { }
};

void RaddistDPF::tick (std::vector <double>& fraction_sun_LAI,
		       std::vector <double>& sun_PAR, 
		       std::vector <double>& total_PAR,  
                       std::vector <double>& sun_NIR, 
		       std::vector <double>& total_NIR, 
                       const double global_radiation, 
		       const double diffuse_radiation,
                       const double min_sin_beta,
                       double sin_beta,
		       const Vegetation& vegetation,
		       const double pF,
		       Treelog& msg)
{
  TREELOG_MODEL (msg);
  daisy_assert (std::isfinite (diffuse_radiation));
  daisy_assert (std::isfinite (global_radiation));
  const size_t No = fraction_sun_LAI.size ();
  daisy_assert (No + 1 == total_PAR.size());
  daisy_assert (No + 1 == sun_PAR.size());

  if (sin_beta < min_sin_beta)
    // Ingen SUNLIT
    {
      if (global_radiation < 5.0)
	{
	  std::fill(fraction_sun_LAI.begin (), fraction_sun_LAI.end (), 0.0);
	  std::fill( sun_PAR.begin (), sun_PAR.end (), 0.0);      
	  std::fill( total_PAR.begin (), total_PAR.end (), 0.0);
	  std::fill( sun_NIR.begin (), sun_NIR.end (), 0.0);
	  std::fill( total_NIR.begin (), total_NIR.end (), 0.0);
	  return;
	}
      if (global_radiation > 25)
	{
	  std::ostringstream tmp;
	  tmp << "Global radiation is " << global_radiation << " W/m^2 while sun is near or below the horizon;  sun angle = " << (std::asin (sin_beta) * 180.0 / M_PI) << " dg, which is less than " << (std::asin (min_sin_beta) * 180.0 / M_PI) << " dg; Pretenting sun angle is " << (std::asin (min_sin_beta) * 180.0 / M_PI);
	  msg.warning (tmp.str ());
	}
      sin_beta = min_sin_beta;
    }

  const double LAI = vegetation.LAI ();

  // Vectors for calculation
  std::vector<double> beam_PAR (No+1, 0.0);
  std::vector<double> dif_PAR (No+1, 0.0);
  std::vector<double> dir_beam_PAR (No+1, 0.0);
  std::vector<double> beam_scat1_PAR (No+1, 0.0);
  std::vector<double> beam_scat2_PAR (No+1, 0.0);
  std::vector<double> dif_sun_PAR (No+1, 0.0);

  std::vector<double> beam_NIR (No+1, 0.0);
  std::vector<double> dif_NIR (No+1, 0.0);
  std::vector<double> dir_beam_NIR (No+1, 0.0);
  std::vector<double> beam_scat1_NIR (No+1, 0.0);
  std::vector<double> beam_scat2_NIR (No+1, 0.0);
  std::vector<double> dif_sun_NIR (No+1, 0.0);
  
  // Beam radiation above the canopy:
  IRb0 = global_radiation - diffuse_radiation;
  // Diffuse radiation above the canopy:
  IRd0 = diffuse_radiation;

  // Extinction coefficient for black leaves in direct-beam irradiance 
  daisy_assert (std::isnormal (sin_beta)); //sin_beta er solhøjden
  double kb =  0.50 / sin_beta;
  daisy_assert (kb > 0.0);


  // clang crashes without volatile here
  volatile
  // Apple LLVM version 8.1.0 (clang-802.0.38)
  // Target: x86_64-apple-darwin16.4.0
  // Thread model: posix
  // InstalledDir: /Library/Developer/CommandLineTools/usr/bin


  // Diffuse transmission coefficeint, Tau_d.
  // Assuming homogen distributed in the hemisphere of diffuse radiation
  double Tau_d = 0.;
  const double dgamma = M_PI/100.;

  //Tau_d integrated over the hemisphere
  for(double i = 0.; i < M_PI/2.; i += dgamma)
    {
      const double gamma = (i + dgamma)/2.; // indfaldsvinklen (radian)
      const double kb_gamma = bound (0.0, 0.5 / cos(gamma), 8.0);
      Tau_d += 2.* exp(-kb_gamma * LAI) * sin(gamma) * cos(gamma) * dgamma;
    }
  // Extinction coefficient for black leaves in diffuse radiation 
  double kd;
  
  if (LAI < 1e-10)
    // Extinction coefficient is irrelevant without LAI.
    kd = 1.0;
  else if (Tau_d > 0.99)
    // Tau_d can only be large if LAI is small.
    kd = 1.0;
  else
    {
      daisy_assert (Tau_d > 0.0);
      kd = -log(Tau_d)/LAI; //note: log == ln i C++
    }
  daisy_assert (kd >= 0.0);

  // ------------------------------------------------
  // FOR Photosynthetically Active Radiation (PAR):
  // ------------------------------------------------
  // Canopy extinction coefficient in direct-beam PAR
  const double kbs_PAR = kb * sqrt(1.0-sigma_PAR);
  // Canopy extinction coefficient in diffuse PAR
  const double kds_PAR = kd * sqrt(1.0-sigma_PAR);

  // Canopy reflection coefficeint of direct PAR for 
  // horizontal leaves, Ph_PAR
  Ph_PAR = (1. - sqrt(1.0-sigma_PAR))/(1. + sqrt(1.0-sigma_PAR));

  // Canopy reflection coefficeint of direct PAR for 
  // uniform leaf-angel distribution, Pcb_PAR
  Pcb_PAR = 1. - exp((-2. * Ph_PAR * kb) / (1. + kb));
  daisy_assert (Pcb_PAR >= 0.0);

  // Include effect of soil water.
  const double Ps_PAR1 = Ps_PAR * Ps_PAR_SWE (pF);
  // Canopy-soil reflection coefficeint of beam irradiance for 
  // uniform leaf-angel distribution, Pscb
  const double aa_PAR = (Pcb_PAR - Ps_PAR1)/(Pcb_PAR * Ps_PAR1 - 1.);
  const double bb_PAR = exp(-2. * sqrt(1.0-sigma_PAR) * kb * LAI);
  Pscb_PAR = (Pcb_PAR + aa_PAR * bb_PAR)/(1. + Pcb_PAR * aa_PAR * bb_PAR);
  daisy_assert (Pscb_PAR >= 0.0);

  // Canopy-soil reflection coefficeint of diffuse irradiance for 
  // uniform leaf-angel distribution, Pscd
  const double cc_PAR = exp(-2. * sqrt(1.0-sigma_PAR) * kd * LAI);
  Pscd_PAR = (Pcb_PAR + aa_PAR * cc_PAR)/(1. + Pcb_PAR * aa_PAR * cc_PAR);
  daisy_assert (Pscd_PAR >= 0.0);

  // Fraction of Photosynthetically Active Radiation (PAR) in Shortwave
  // incoming radiation. 
  //  static const double PARinSi = 0.50;	

  // Total PAR
  // Fill beam PAR (cummulative)
  daisy_assert (std::isfinite (IRb0));
  radiation_distribution (No, LAI, Pscb_PAR, IRb0, kbs_PAR, beam_PAR, PARinSi);
  daisy_non_negative (beam_PAR);
  // Fill diffuse PAR (cummulative)
  radiation_distribution (No, LAI, Pscd_PAR, IRd0, kds_PAR, dif_PAR, PARinSi);
  daisy_non_negative (dif_PAR);

  // Sunlit PAR
  // Fill direct beam PAR without scattering (cummulative)
  radiation_distribution (No, LAI, sigma_PAR, IRb0, kb, dir_beam_PAR, PARinSi);
  // Fill direct beam PAR with scattering 1 (cummulative)
  radiation_distribution (No, LAI, Pscb_PAR, IRb0, kbs_PAR + kb, beam_scat1_PAR, PARinSi);
  // Fill direct beam PAR with scattering 1 (cummulative)
  radiation_distribution (No, LAI, sigma_PAR, IRb0, 2. * kb, beam_scat2_PAR, PARinSi);
  // Fill diffuse PAR sunlit (cummulative)
  radiation_distribution (No, LAI, Pscd_PAR, IRd0, kds_PAR + kb, dif_sun_PAR, PARinSi);

  for (int i = 0; i <= No; i++)
    {
      const double kds_plus_kb_PAR = kds_PAR + kb;
      daisy_assert (std::isnormal (kds_plus_kb_PAR));
      const double kbs_plus_kb_PAR = kbs_PAR + kb;
      daisy_assert (std::isnormal (kbs_plus_kb_PAR));
      sun_PAR[i] = std::max (0.0, 
			     dir_beam_PAR[i] 
			     + (beam_scat1_PAR[i]*(kbs_PAR /kbs_plus_kb_PAR))
			     - (beam_scat2_PAR[i] / 2.0)
			     + (dif_sun_PAR[i]*(kds_PAR /(kds_plus_kb_PAR))));
      daisy_assert (sun_PAR[i] >= 0.0);
      daisy_assert (dif_PAR[i] >= 0.0);
      daisy_assert (beam_PAR[i] >= 0.0);

      total_PAR[i] = dif_PAR[i] + beam_PAR[i];
      daisy_assert (total_PAR[i] >= 0.0);
    }
  
  // ------------------------------------------------
  // For Near Infra-red Radiation (NIR):
  // ------------------------------------------------
  // Canopy extinction coefficient in direct-beam NIR
  const double kbs_NIR = kb * sqrt(1.0-sigma_NIR);
  // Canopy extinction coefficient in diffuse NIR
  const double kds_NIR = kd * sqrt(1.0-sigma_NIR);

  // Canopy reflection coefficeint of direct NIR for 
  // horizontal leaves, Ph_NIR
  Ph_NIR = (1. - sqrt(1.0-sigma_NIR))/(1. + sqrt(1.0-sigma_NIR));

  // Canopy reflection coefficeint of direct NIR for 
  // uniform leaf-angel distribution, Pcb_NIR
  Pcb_NIR = 1. - exp((-2. * Ph_NIR * kb) / (1. + kb));
  daisy_assert (Pcb_NIR >= 0.0);

  // Include effect of soil water.
  const double Ps_NIR1 = Ps_NIR * Ps_NIR_SWE (pF);

  // Canopy-soil reflection coefficeint of beam irradiance for 
  // uniform leaf-angel distribution, Pscb
  const double aa_NIR = (Pcb_NIR - Ps_NIR1)/(Pcb_NIR * Ps_NIR1 - 1.);
  
  const double bb_NIR = exp(-2. * sqrt(1.0-sigma_NIR) * kb * LAI);
  Pscb_NIR = (Pcb_NIR + aa_NIR * bb_NIR)/(1. + Pcb_NIR * aa_NIR * bb_NIR);
  daisy_assert (Pscb_NIR >= 0.0);

  // Canopy-soil reflection coefficeint of diffuse irradiance for 
  // uniform leaf-angel distribution, Pscd
  const double cc_NIR = exp(-2. * sqrt(1.0-sigma_NIR) * kd * LAI);
  Pscd_NIR = (Pcb_NIR + aa_NIR * cc_NIR)/(1. + Pcb_NIR * aa_NIR * cc_NIR);
  daisy_assert (Pscd_NIR >= 0.0);


  // Fraction of Photosynthetically Active Radiation (NIR) in Shortwave
  // incoming radiation. 
  //static const double NIRinSi = 0.50;	

  // Total NIR
  // Fill beam NIR (cummulative)
  daisy_assert (std::isfinite (IRb0));
  radiation_distribution (No, LAI, Pscb_NIR, IRb0, kbs_NIR, beam_NIR, NIRinSi);
  daisy_non_negative (beam_NIR);
  // Fill diffuse NIR (cummulative)
  radiation_distribution (No, LAI, Pscd_NIR, IRd0, kds_NIR, dif_NIR, NIRinSi);
  daisy_non_negative (dif_NIR);

  // Sunlit NIR
  // Fill direct beam NIR without scattering (cummulative)
  radiation_distribution (No, LAI, sigma_NIR, IRb0, kb, dir_beam_NIR, NIRinSi);
  // Fill direct beam NIR with scattering 1 (cummulative)
  radiation_distribution (No, LAI, Pscb_NIR, IRb0, kbs_NIR + kb, beam_scat1_NIR, NIRinSi);
  // Fill direct beam NIR with scattering 1 (cummulative)
  radiation_distribution (No, LAI, sigma_NIR, IRb0, 2. * kb, beam_scat2_NIR, NIRinSi);
  // Fill diffuse NIR sunlit (cummulative)
  radiation_distribution (No, LAI, Pscd_NIR, IRd0, kds_NIR + kb, dif_sun_NIR, NIRinSi);

  for (int i = 0; i <= No; i++)
    {
      const double kds_plus_kb_NIR = kds_NIR + kb;
      daisy_assert (std::isnormal (kds_plus_kb_NIR));
      const double kbs_plus_kb_NIR = kbs_NIR + kb;
      daisy_assert (std::isnormal (kbs_plus_kb_NIR));
      sun_NIR[i] = std::max (0.0, 
			     dir_beam_NIR[i] 
			     + (beam_scat1_NIR[i]*(kbs_NIR /kbs_plus_kb_NIR))
			     - (beam_scat2_NIR[i] / 2.0)
			     + (dif_sun_NIR[i]*(kds_NIR /(kds_plus_kb_NIR))));
      daisy_assert (sun_NIR[i] >= 0.0);
      daisy_assert (dif_NIR[i] >= 0.0);
      daisy_assert (beam_NIR[i] >= 0.0);

      total_NIR[i] = dif_NIR[i] + beam_NIR[i];
      daisy_assert (total_NIR[i] >= 0.0);
    }

  // --------------------------------------------------  
  // LAI fraction in sunlit:
  const double dLAI = LAI/(No + 0.);

  for (int i = 0; i <= No - 1; i++)
    {
      fraction_sun_LAI[i]=(exp(-kb * dLAI*(i+0.5)));
      daisy_assert (fraction_sun_LAI[i] >= 0.0);    
      daisy_assert (fraction_sun_LAI[i] <= 1.0);
    }
}


void
RaddistDPF::output(Log& log) const
{
  output_variable (IRb0, log);
  output_variable (IRd0, log);
  output_variable (Ph_PAR, log);
  output_variable (Pcb_PAR, log);
  output_variable (Pscb_PAR, log);
  output_variable (Pscd_PAR, log);
  output_variable (Ph_NIR, log);
  output_variable (Pcb_NIR, log);
  output_variable (Pscb_NIR, log);
  output_variable (Pscd_NIR, log);

}

static struct RaddistDPFSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new RaddistDPF (al); }
  RaddistDPFSyntax ()
    : DeclareModel (Raddist::component, "sun-shade", 
	       "Sun-shade model of radiation distribution in the canopy.")
  { }
  void load_frame (Frame& frame) const
  {

    frame.declare ("sigma_PAR", Attribute::None (), Check::positive (), Attribute::Const,
                "Leaf scattering coefficient of PAR. sigma_PAR=0,15 (Houborg, 2006)");
    frame.set ("sigma_PAR", 0.15);

    frame.declare ("sigma_NIR", Attribute::None (), Check::positive (), Attribute::Const,
                "Leaf scattering coefficient of NIR. sigma_NIR=0,83 (Houborg, 2006)");
    frame.set ("sigma_NIR", 0.83);

    frame.declare ("Ps_PAR", Attribute::None (), Check::positive (), Attribute::Const,
                "Soil reflection coefficient of PAR, Ps_PAR = 0.1 (Houborg, 2006)");
    frame.set ("Ps_PAR", 0.1); 

    frame.declare ("Ps_NIR", Attribute::None (), Check::positive (), Attribute::Const,
                "Soil reflection coefficient of NIR, Ps_NIR = 0.18 (Houborg, 2006)");
    frame.set ("Ps_NIR", 0.18);
    frame.declare ("Ps_PAR_SWE", "pF", Attribute::None (), Attribute::Const, "\
Effect of soil water on Ps_PAR.");
    frame.set ("Ps_PAR_SWE", PLF::always_1 ());
    frame.declare ("Ps_NIR_SWE", "pF", Attribute::None (), Attribute::Const, "\
Effect of soil water on Ps_BIR.");
    frame.set ("Ps_NIR_SWE", PLF::always_1 ());
 
    frame.declare ("IRb0", "W m^-2", Attribute::LogOnly, "Beam radiation above the canopy");
    frame.declare ("IRd0", "W m^-2", Attribute::LogOnly,
                "Diffuse radiation above the canopy ");
    frame.declare ("Ph_PAR", "", Attribute::LogOnly, 
                "Canopy reflection coefficeint of beam PAR for horizontal leaves");
    frame.declare ("Pcb_PAR", "", Attribute::LogOnly, "Canopy reflection coefficeint of beam PAR for uniform leaf-angel distribution");
    frame.declare ("Pscb_PAR", "", Attribute::LogOnly, "Canopy-soil reflection coefficeint of beam PAR for uniform leaf-angel distribution");
    frame.declare ("Pscd_PAR", "", Attribute::LogOnly, "Canopy-soil reflection coefficeint of diffuse PAR for uniform leaf-angel distribution");
    frame.declare ("Ph_NIR", "", Attribute::LogOnly, 
                "Canopy reflection coefficeint of beam NIR for horizontal leaves");
    frame.declare ("Pcb_NIR", "", Attribute::LogOnly, "Canopy reflection coefficeint of beam NIR for uniform leaf-angel distribution");
    frame.declare ("Pscb_NIR", "", Attribute::LogOnly, "Canopy-soil reflection coefficeint of beam NIR for uniform leaf-angel distribution");
    frame.declare ("Pscd_NIR", "", Attribute::LogOnly, "Canopy-soil reflection coefficeint of diffuse NIR for uniform leaf-angel distribution");
  }
} RaddistDPF_syntax;


