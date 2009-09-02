// photo_Farquhar.C -- Leaf photosynthesis based on Farquhar et al., 1980 and Ball et al. 1987.
// 
// Copyright 1996-2001,2005 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001,2005-2006 KVL.
// Copyright 2006 Birgitte Gjettermann.
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

#include "photo_Farquhar.h"
#include "block_model.h"
#include "rubiscoNdist.h"
#include "resistance.h"
#include "ABAeffect.h"
#include "stomatacon.h"
#include "bioclimate.h"
#include "canopy_std.h"
#include "phenology.h"
#include "log.h"
#include "plf.h"
#include "mathlib.h"
#include "treelog.h"
#include "check.h"
#include "librarian.h"
#include "frame.h"
#include <sstream>

PhotoFarquhar::PhotoFarquhar (const BlockModel& al)
  : Photo (al),
    Xn (al.number ("Xn")),
    O2_atm (al.number ("O2_atm")),
    Gamma25 (al.number ("Gamma25")),
    Ea_Gamma (al.number ("Ea_Gamma")),
    Ptot (al.number("Ptot")),
    m (al.number("m")),
    b (al.number("b")),
    gbw (al.number("gbw")),
    rubiscoNdist (Librarian::build_item<RubiscoNdist> (al, "N-dist")),
    ABAeffect (Librarian::build_item<ABAEffect> (al, "ABAeffect")),
    Stomatacon (Librarian::build_item<StomataCon> (al, "Stomatacon"))
{ }

PhotoFarquhar::~PhotoFarquhar ()
{ }

void
PhotoFarquhar::crop_Vmax_total (const std::vector<double>& rubisco_Ndist /* [mol/m²leaf] */,  
				std::vector<double>& cropVm) const
{
  const int No = cropVm.size ();
  daisy_assert (rubisco_Ndist.size () == No);
  // Fill photosynthetic capacity Vm for each canopy layer in vector
  for (int i = 0; i < No; i++)
     cropVm[i] = Xn * rubisco_Ndist[i]; //[mol/m² leaf/s]
}


// Arrhenius temperature response function used for Kc, Ko og Gamma
double
PhotoFarquhar::Arrhenius (const double k25, const double Ea, const double T) const 
{
  const double R = 8.314; //Gas constant, J/(mol K) 
  return k25 * exp(Ea * (T-25.0)/(298.0*R*(T+273.0)));//Pa OK!
}

double 
first_root_of_square_equation (double a, double b, double c)
{
  const double D = sqrt (pow2 (b) - 4.0 * a * c);
  const double x1 = (-b - D) / (2.0 * a);
  // Return the first solution.
  return x1;
}

double 
second_root_of_square_equation (double a, double b, double c)
{
  const double D = sqrt (pow2 (b) - 4.0 * a * c);
  const double x2 = (-b + D) / (2.0 * a);
  // Return the second solution.
  return x2;
}

double
PhotoFarquhar:: Sat_vapor_pressure (const double T /*[degree C]*/) const 
{
  const double a = 5.818e-4;
  const double b = 1.408e-2;
  const double c = 1.675;
  const double d = 4.222e1;
  const double e = 6.174e2;
  const double Vp = (((a * T + b) * T + c)* T + d)* T + e; //[Pa]
  return Vp;
}

double
PhotoFarquhar::GSTModel (const double CO2_atm, double ABA_effect, double pn,
                         double rel_hum /*[unitless]*/, 
                         double LA, double fraction,
                         double gbw/*[mol/m2 leaf/s]*/, 
                         const double Tc, const double Tl, double& hs,
                         double& cs, Treelog& msg) 
{

  const double wsf = ABA_effect; //water stress function []

  const double intercept = b * LA * fraction; // min conductance 
  daisy_assert (gbw >0.0);
  const double rbw = 1./gbw;   //[s*m2 leaf/mol]

  //leaf surface CO2
  cs = CO2_atm - (1.4 * pn * Ptot * rbw); //[Pa] 
  daisy_assert (cs > 0.0);

  //Net photosynthesis
  double pz; //[mol/m²leaf/s]
  if(pn <= 0.) 
    pz = 1.0e-12;
  else 
    pz = pn;

  // Saturated vapor pressure in the air
  const double va = rel_hum * Sat_vapor_pressure (Tc);
  const double wa = va / Ptot;    //[unitless] 
  // Relative saturated vapor pressure at the leaf surface
  const double wi = Sat_vapor_pressure (Tl) / Ptot;

  // Interpolation between limiting factors.
  const double aa = wsf * m * pz * Ptot /(cs-Gamma); //[mol/m2/s]
  const double bb = intercept + (1./rbw)-(wsf * m * pz * Ptot/(cs-Gamma));//[mol/m2/s]
  const double cc = (- wa /(wi * rbw)) - intercept;//[mol/m2/s]
  daisy_assert (aa > 0.0);
  // Relative humidity at leaf surface
  hs = second_root_of_square_equation(aa, bb, cc); //[]
  if(hs >= 1.)
    hs = 0.9;

  const double Ds = wi * (1.0 - hs) * Ptot; 

  //stomatal conductance
  double gsw; 
  if(pn <= 0.0)
    gsw = intercept;//[mol/m²leaf/s]
  else 
    gsw = Stomatacon->stomata_con (wsf /*[]*/, m /*[]*/, hs /*[]*/,
                                   pz /*[mol/m²leaf/s]*/, Ptot /*[Pa]*/, 
                                   cs /*[Pa]*/, Gamma /*[Pa]*/, 
                                   intercept /*[mol/m²leaf/s]*/, 
                                   CO2_atm /*[Pa]*/, Ds/*[Pa]*/, msg);//[mol/m²leaf/s] 

  return gsw; //stomatal conductance [mol/m²leaf/s]
}

double 
PhotoFarquhar::stomata_conductance() const
{ return gs_ms; } // [m s^-1]

double
PhotoFarquhar::assimilate (const Units& units,
                           const double ABA_xylem, const double psi_c, const double rel_hum, 
			   const double CO2_atm,
			   const double, const double Tc, const double Tl,
                           const double cropN,
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
 
  // CAI (total)  below the current leaf layer.
  double prevLA = LAIvsH (PAR_height[0]); 

  // Assimilate produced by canopy photosynthesis
  double Ass_ = 0.0;
  double pn = 0.0;

  // Accumulated CAI, for testing purposes.
  double accCAI =0.0;

  // Number of computational intervals in the canopy.
  const int No = PAR.size () - 1;
  daisy_assert (No > 0);
  daisy_assert (No == PAR_height.size () - 1);
  
  // N-distribution and photosynthetical capacity 
  std::vector<double> rubisco_Ndist (No, 0.0);
  std::vector<double> crop_Vm_total (No, 0.0);

  // Photosynthetic capacity (for logging)
  while (Vm_vector.size () < No)
    Vm_vector.push_back (0.0);
  // Potential electron transport rate (for logging)
  while (Jm_vector.size () < No)
    Jm_vector.push_back (0.0);
  // Photosynthetic N-leaf distribution (for logging)
  while (Nleaf_vector.size () < No)
    Nleaf_vector.push_back (0.0);
  // Brutto assimilate production (for logging)
  while (Ass_vector.size () < No)
    Ass_vector.push_back (0.0);
  // LAI (for logging)
  while (LAI_vector.size () < No)
    LAI_vector.push_back (0.0);

  rubiscoNdist->rubiscoN_distribution (units,
                                       PAR_height, prevLA, DS,
                                       rubisco_Ndist/*[mol/m²leaf]*/, 
				       cropN /*[g/m²area]*/, msg);
  crop_Vmax_total (rubisco_Ndist, crop_Vm_total);  


  // Stomata CO2 pressure (for logging)
  while (ci_vector.size () < No)
    ci_vector.push_back (0.0);//[Pa]

  // Leaf surface CO2 pressure  (for logging)
  while (cs_vector.size () < No)
    cs_vector.push_back (0.0);//[Pa]

  // Leaf surface relative humidity (for logging)
  while (hs_vector.size () < No)
    hs_vector.push_back (0.0);//[]

  // Stomata conductance (for logging)
  while (gs_vector.size () < No)
    gs_vector.push_back (0.0);//[m/s]
     
  Gamma = Arrhenius (Gamma25, Ea_Gamma, Tl);//Pa

  // CAI in each interval.
  const double dCAI = PAR_LAI / No;
  
  for (int i = 0; i < No; i++)
    {
      const double height = PAR_height[i+1];
      daisy_assert (height < PAR_height[i]);

      // Leaf Area index for a given leaf layer
      const double LA = prevLA - LAIvsH (height);
      daisy_assert (LA >= 0.0);

      prevLA = LAIvsH (height);
      accCAI += LA;

      if (LA * fraction [i] > 0)
	{  
	  // PAR in mol/m2/s = PAR in W/m2 * 0.0000046
	  const double dPAR = (PAR[i] - PAR[i+1])/dCAI * 0.0000046; //W/m2->mol/m²leaf/s
	  // log variable
	  PAR_ += dPAR * dCAI * 3600.0; //mol/m²area/h/fraction

	  // Photosynthetic rubisco capacity 
	  const double vmax25 = crop_Vm_total[i]*fraction[i];//[mol/m²leaf/s/fracti.]
	  daisy_assert (vmax25 >= 0.0);

	  // Photosynthetic effect of Xylem ABA and crown water potential.
	  ABA_effect = ABAeffect->ABA_effect(ABA_xylem, psi_c,  msg);//[unitless]
          
	  // leaf respiration
	  const double rd = respiration_rate(vmax25, Tl);
	  daisy_assert (rd >= 0.0);

	  //solving photosynthesis and stomatacondctance model for each layer
	  double ci  = 0.5 * CO2_atm;//first guess for ci, [Pa]
          double& hs = hs_vector[i];
          hs = 0.5;              // first guess of hs []
          double& cs = cs_vector[i];
	  double gsw = LA / 5.0; //first gues for stomatal cond,[mol/s/m²leaf]
	  const int maxiter = 150;
	  int iter = 0;
	  double lastci;

	  do
	    {
	      lastci = ci; //Stomata CO2 pressure 

	      //Calculating ci and "net"photosynthesis
	      CxModel(CO2_atm, pn, ci, dPAR /*[mol/m²leaf/s]*/, 
                      gsw, Tl, vmax25, rd, msg);//[mol/m²leaf/s/fraction]
	      gsw = GSTModel(CO2_atm, ABA_effect, pn, rel_hum, LA, 
                             fraction[i], gbw, Tc, Tl, hs, cs,
                             msg);//[mol/s/m²leaf/fraction]

	      iter++;
	      if(iter > maxiter)
		{
		  std::ostringstream tmp;
		  tmp << "total iterations in assimilation model exceed " << maxiter;
		  msg.warning (tmp.str ());
		  break;
		}
	    }
	  // while (std::fabs (lastci-ci)> 0.01);
          while (std::fabs (lastci-ci)> 0.01);

	  // Leaf brutto photosynthesis [gCO2/m2/h] 
	  const double pn_ = (pn+rd) * molWeightCO2 * 3600.0;//mol CO2/m²leaf/s->g CO2/m²leaf/h
	  const double rd_ = (rd) * molWeightCO2 * 3600.0;   //mol CO2/m²/s->g CO2/m²/h
	  const double Vm_ = V_m(vmax25, Tl); //[mol/m² leaf/s/fraction]
	  const double Jm_ = J_m(vmax25, Tl); //[mol/m² leaf/s/fraction]

	  daisy_assert (pn_>= 0.0);
	  Ass_ += LA * pn_; // [g/m²area/h] 
	  Res += LA * rd_;  // [g/m²area/h] 
	  daisy_assert (Ass_ >= 0.0);

	  //log variables:
	  Ass_vector[i]+= pn_* (molWeightCH2O / molWeightCO2) * LA;//[g CH2O/m²area/h]
	  Nleaf_vector[i]+= rubisco_Ndist[i] * LA * fraction[i]; //[mol N/m²area]OK
	  gs_vector[i]+= gsw * LA * fraction[i];     //[mol/m² area/s]
	  ci_vector[i]+= ci * fraction[i];  //[Pa] OK
	  Vm_vector[i]+= Vm_ * 1000.0 * LA * fraction[i]; //[mmol/m² area/s]OK
	  Jm_vector[i]+= Jm_ * 1000.0 * LA * fraction[i]; //[mmol/m² area/s]OK
	  LAI_vector[i] += LA * fraction[i];//OK
	  
	  ci_middel += ci * fraction[i]/(No + 0.0);// [Pa]   OK
	  gs += LA * gsw * fraction[i]; 
	  Ass += LA * pn_ * (molWeightCH2O / molWeightCO2);//[g CH2O/m2 area/h] OK
	  LAI += LA * fraction[i];//OK
	  Vmax += 1000.0 * LA * fraction[i] * Vm_;   //[mmol/m² area/s]
	  jm += 1000.0 * LA * fraction[i] * Jm_;     //[mmol/m² area/s]
	  leafPhotN += rubisco_Ndist[i] * LA *fraction[i]; //[mol N/m²area]; 
	  fraction_total += fraction[i]/(No + 0.0);
	}
    }
  daisy_assert (approximate (accCAI, canopy.CAI));
  daisy_assert (Ass_ >= 0.0);

  // Omregning af gs(mol/(m2s)) til gs_ms (m/s) foretages ved 
  // gs_ms = gs * (R * T)/P:
  gs_ms = gs * (Resistance::R * (Tl + Resistance::TK)) / Resistance::P_surf; //[m s^-1] 

  return (molWeightCH2O / molWeightCO2)* Ass_;    // Assimilate [g CH2O/m2/h]
}

void
PhotoFarquhar::clear ()
{
  std::fill(gs_vector.begin (), gs_vector.end (), 0.0);
  std::fill(ci_vector.begin (), ci_vector.end (), 0.0);
  std::fill(Vm_vector.begin (), Vm_vector.end (), 0.0);
  std::fill(Jm_vector.begin (), Jm_vector.end (), 0.0);
  std::fill(Nleaf_vector.begin (), Nleaf_vector.end (), 0.0);
  std::fill(Ass_vector.begin (), Ass_vector.end (), 0.0);
  std::fill(LAI_vector.begin (), LAI_vector.end (), 0.0);
  ci_middel = 0.0;
  gs = 0.0;
  gs_ms = 0.0;
  Ass = 0.0;
  Res = 0.0;
  PAR_ = 0.0;
  LAI = 0.0;
  Vmax = 0.0;
  jm = 0.0;
  leafPhotN = 0.0;
  fraction_total = 0.0;
  ABA_effect = 1.0;
}

void
PhotoFarquhar::output(Log& log) const
{
  output_variable (Ass_vector, log);
  output_variable (Nleaf_vector, log);
  output_variable (cs_vector, log);
  output_variable (hs_vector, log);
  output_variable (gs_vector, log);
  output_variable (ci_vector, log);
  output_variable (Vm_vector, log);
  output_variable (Jm_vector, log);
  output_variable (ci_middel, log);
  output_variable (gs, log);
  output_variable (gs_ms, log);
  output_variable (Ass, log);
  output_variable (Res, log);
  output_variable (LAI, log);
  output_variable (LAI_vector, log);
  output_variable (PAR_, log);
  output_variable (Vmax, log);
  output_variable (jm, log);
  output_variable (leafPhotN, log);
  output_variable (fraction_total, log);
  output_variable (ABA_effect, log);
}

bool 
PhotoFarquhar::handle_N_stress () const
{ return true; }

bool 
PhotoFarquhar::handle_water_stress () const
{ return true; }

static struct PhotoFarquharSyntax : DeclareBase
{
  PhotoFarquharSyntax ()
    : DeclareBase (Photo::component, "Farquhar", "\
Base parameterization for Farquhar derived photosynthesis models.\n\
\n\
Farquhar et al. (1980) photosynthesis and Ball et al. (1987)\n\
stomataconductance model coupled as described by Collatz et al., 1991.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare ("Xn", "mol/mol/s", Check::positive (), Attribute::Const,
                "Slope of relationship between leaf rubisco N and Vmax.\n\
Xn = 1.16E-3 mol/mol/s for wheat (de Pury & Farquhar, 1997)");
    frame.set ("Xn", 1.16e-3);

    frame.declare ("O2_atm", "Pa", Check::positive (), Attribute::Const,
                "O2 partial pressure of atmosphere");
    frame.set ("O2_atm", 20500.0);

    frame.declare ("Gamma25", "Pa", Check::positive (), Attribute::Const,
                   "CO2 compensation point of photosynthesis.\n\
Gamma25 = 3.69 Pa for wheat (Collatz et al., 1991)");
    frame.set ("Gamma25", 3.69);

    frame.declare ("Ea_Gamma", "J/mol", Check::positive (), Attribute::Const,
                "Actimation energy for Gamma. Ea_Gamma = 29000 (Jordan & Ogren, 1984)");
    frame.set ("Ea_Gamma", 29000.);

    frame.declare ("Ptot", "Pa", Check::positive (), Attribute::Const,
                "Atmospheric pressure. Ptot = 100000 Pa");
    frame.set ("Ptot", 1.0E5);

    frame.declare ("m", Attribute::None (), Check::positive (), Attribute::Const,
                "Stomatal slope factor. Ball and Berry (1982): m = 9 for soyabean. Wang and Leuning(1998): m = 11 for wheat");

    frame.declare ("b", "mol/m^2/s", Check::positive (), Attribute::Const,
                "Stomatal intercept factor, Ball and Berry (1982) & Wang and Leuning(1998): (0.01 mol/m2/s)");

    frame.declare ("gbw", "mol/m^2/s", Check::positive (), Attribute::Const,
                "Leaf boundary conductance of water. gbw = 2 mol/m²/s (Collatz et al., 1991");
    frame.set ("gbw", 2.00);

    //log variables
    frame.declare ("ABA_effect", Attribute::None (), Attribute::LogOnly,
                "Water stress effect induced by ABA and crown water potential");
    frame.declare ("ci_vector", "Pa", Attribute::LogOnly, Attribute::CanopyCells, "CO2 pressure in Stomatal in each layer.");
    frame.declare ("Vm_vector", "mmol/m^2/s", Attribute::LogOnly, Attribute::CanopyCells, "Photosynthetic capacity in each layer.");
    frame.declare ("Jm_vector", "mmol/m^2/s", Attribute::LogOnly, Attribute::CanopyCells, "Potential rate of electron transport in each layer.");
    frame.declare ("cs_vector", "Pa", Attribute::LogOnly, Attribute::CanopyCells, "CO2 pressure at leaf surface.");
    frame.declare_fraction ("hs_vector", 
                            Attribute::LogOnly, Attribute::CanopyCells, "\
Relative humidity at leaf surface.");
    frame.declare ("gs_vector", "mol/m^2/s", Attribute::LogOnly, Attribute::CanopyCells, "Stomata cunductance in each layer.");
    frame.declare ("Nleaf_vector", "mol N/m^2", Attribute::LogOnly, Attribute::CanopyCells, "Distribution of photosynthetic N-leaf.");
    frame.declare ("Ass_vector", "mol CH2O/m^2/h", Attribute::LogOnly, Attribute::CanopyCells, "Brutto assimilate.");
    frame.declare ("LAI_vector", "mol CH2O/m^2/s", Attribute::LogOnly, Attribute::CanopyCells, "LAI.");

    frame.declare ("ci_middel", "Pa", Attribute::LogOnly, "Stomata average CO2 pressure.");
    frame.declare ("Gamma", "Pa", Attribute::LogOnly, "\
CO2 compensation point of photosynthesis.");
    frame.declare ("gs", "mol/m^2/s", Attribute::LogOnly, "Stomata conductance.");
    frame.declare ("gs_ms", "m/s", Attribute::LogOnly, "Stomata conductance.");
    frame.declare ("Ass", "g CH2O/m^2/h", Attribute::LogOnly, "'Net' leaf assimilate of CO2 (brutto photosynthesis).");
    frame.declare ("Res", "g CH2O/m^2/h", Attribute::LogOnly, "Farquhar leaf respiration.");
    frame.declare ("LAI", "", Attribute::LogOnly, "Leaf area index for the canopy used in photosynthesis.");
    frame.declare ("PAR_", "mol/m^2/h", Attribute::LogOnly, "PAR.");
    frame.declare ("Vmax", "[mmol/m^2/s]", Attribute::LogOnly, "Photosynthetic Rubisco capacity.");
    frame.declare ("jm", "[mmol/m^2/s]", Attribute::LogOnly, "Potential rate of electron transport.");
    frame.declare ("leafPhotN", "[mol N/m^2]", Attribute::LogOnly, "Content of photosynthetic active leaf N.");
    frame.declare ("fraction_total", "", Attribute::LogOnly, "Fraction of leaf contributing to the photosynthesis.");

    // Models
    frame.declare_object ("N-dist", RubiscoNdist::component, 
                       "Rubisco N-distribution in the canopy layer.");
    frame.set ("N-dist", "exp");

    frame.declare_object ("ABAeffect", ABAEffect::component, 
                       "The effect of xylem ABA on stomata conductivity.");
    frame.set ("ABAeffect", "ABA-exp");

    frame.declare_object ("Stomatacon", StomataCon::component, 
                       "Stomata conductance of water vapor.");
    frame.set ("Stomatacon", "Leuning");
  }
} PhotoFarquhar_syntax;

// photo_Farquhar.C ends here.
