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
#include "block.h"
#include "rubiscoNdist.h"
#include "ABAeffect.h"
#include "bioclimate.h"
#include "canopy_std.h"
#include "phenology.h"
#include "log.h"
#include "plf.h"
#include "alist.h"
#include "syntax.h"
#include "submodel.h"
#include "mathlib.h"
#include <sstream>
#include "check.h"
#include "librarian.h"

using namespace std;

PhotoFarquhar::PhotoFarquhar (Block& al)
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
    ABAeffect (Librarian::build_item<ABAEffect> (al, "ABAeffect"))
{ }

PhotoFarquhar::~PhotoFarquhar ()
{ }

void
PhotoFarquhar::crop_Vmax_total (const vector <double>& rubisco_Ndist/*[mol/m²leaf]*/,  
				std::vector <double>& cropVm) const
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
PhotoFarquhar:: GSTModel(const double CO2_atm, double ABA_effect, double pn, double rel_hum /*[unitless]*/, 
			 double LA, double fraction, double gbw/*[mol/m2/s]*/, 
			 const double Ta, const double Tl, Treelog&) 
{
  const double wsf = ABA_effect; //water stress function
  const double intercept = b * LA * fraction; //min conductance 
  daisy_assert (gbw >0.0);
  const double rbw = 1./gbw;   //[s*m2/mol]

  //leaf surface CO2
  const double cs = CO2_atm - (1.4 * pn * Ptot * rbw); //[Pa] 
  daisy_assert (cs > 0.0);

  double pz; //[mol/m2/s]
  if(pn <= 0.) 
    pz = 1.0e-12;
  else 
    pz = pn;

  // Saturated vapor pressure in the air
  const double va = rel_hum * Sat_vapor_pressure (Ta);
  const double wa = va / Ptot;    //[unitless] 
  // Relative saturated vapor pressure at the leaf surface
  const double wi = Sat_vapor_pressure (Tl) / Ptot;
  const double Gamma = Arrhenius (Gamma25, Ea_Gamma, Tl);//Pa

  // Interpolation between limiting factors.
  const double aa = wsf * m * pz * Ptot /(cs-Gamma); //[mol/m2/s]
  const double bb = intercept + (1./rbw)-(wsf * m * pz * Ptot/(cs-Gamma));//[mol/m2/s]
  const double cc = (- wa /(wi * rbw)) - intercept;//[mol/m2/s]
  daisy_assert (aa > 0.0);
  double hs = second_root_of_square_equation(aa, bb, cc); 
 
  if(hs > 1.)
    hs = 1.0;

  double gsw; //stomatal conductance
  if(pn <= 0.0)
    gsw = intercept;//[mol/m2/s]
  else 
    {
      daisy_assert (cs > Gamma);
      gsw = wsf * (m*hs*pz*Ptot) /(cs-Gamma) + intercept;//[mol/m2/s]  
    }
  daisy_assert (gsw >= 0.0);

  return gsw;//conductance[mol/m2/s]
}

double
PhotoFarquhar::assimilate (const double ABA_xylem, const double rel_hum, 
			   const double CO2_atm,
			   const double Ta, const double Tl, const double cropN,
			   const vector<double>& PAR, 
			   const vector<double>& PAR_height,
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

  //only the sunlit fraction is logged.
  fraction_sun = 0.0;
  sun_LAI = 0.0;

  // Accumulated CAI, for testing purposes.
  double accCAI =0.0;

  // Number of computational intervals in the canopy.
  const int No = PAR.size () - 1;
  daisy_assert (No > 0);
  daisy_assert (No == PAR_height.size () - 1);
  
  // N-distribution and photosynthetical capacity 
  vector<double> rubisco_Ndist (No, 0.0);
  vector<double> crop_Vm_total (No, 0.0);

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
  // sunlit LAI (for logging)
  while (sun_LAI_vector.size () < No)
    sun_LAI_vector.push_back (0.0);

  rubiscoNdist->rubiscoN_distribution (PAR_height, prevLA, DS, rubisco_Ndist/*[mol/m²leaf]*/, 
				       cropN /*[g/m²area]*/, msg);
  crop_Vmax_total (rubisco_Ndist, crop_Vm_total);  


  // Stomata CO2 preassure (for logging)
  while (ci_vector.size () < No)
    ci_vector.push_back (0.0);//[Pa]

  // Stomata conductance (for logging)
  while (gs_vector.size () < No)
    gs_vector.push_back (0.0);//[m/s]
     
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
	  const double dPAR = (PAR[i] - PAR[i+1])/dCAI * 0.0000046; // W/m2->mol/m2/s
	  // log variable
	  PAR_ += dPAR * dCAI * 3600.0; //mol/m2/h/fraction

	  // Photosynthetic rubisco capacity 
	  const double vmax25 = crop_Vm_total[i]*fraction[i];//[mol/m²leaf/s/fracti.]
	  daisy_assert (vmax25 >= 0.0);

	  // Photosynthetic effect of Xylem ABA.
	  const double ABA_effect = ABAeffect->ABA_effect(ABA_xylem, msg);

	  // leaf respiration
	  const double rd = respiration_rate(vmax25, Tl);
	  daisy_assert (rd >= 0.0);

	  //solving photosynthesis and stomatacondctance model for each layer
	  double ci  = 0.5 * CO2_atm;//first guess for ci, Pa
	  double gsw = LA / 5.0; //first gues for stomatal cond. gsw, [mol/s/m²]

	  const int maxiter = 150;
	  int iter = 0;
	  double lastci;

	  do
	    {
	      lastci = ci; //Stomata CO2 pressure 

	      //Calculating ci and "net"photosynthesis
	      CxModel(CO2_atm, pn, ci, dPAR, gsw, Tl, vmax25, rd, msg);//[mol/m²leaf/s/fraction]
	      gsw = GSTModel(CO2_atm, ABA_effect, pn, rel_hum, LA, fraction[i], gbw, Ta, Tl, msg);//[mol/s/m²leaf/fraction]

	      iter++;
	      if(iter > maxiter)
		{
		  std::ostringstream tmp;
		  tmp << "total iterations in assimilation model exceed " << maxiter;
		  msg.warning (tmp.str ());
		  break;
		}
	    }
	  while ((lastci-ci)> 0.01);

	  // Leaf brutto photosynthesis [gCO2/m2/h] 
	  const double pn_ = (pn+rd) * molWeightCO2 * 3600.0;//mol CO2/m²leaf/s->g CO2/m²leaf/h
	  const double rd_ = (rd) * molWeightCO2 * 3600.0;   //mol CO2/m²/s->g CO2/m²/h
	  const double Vm_ = V_m(vmax25, Tl); //[mol/m² leaf/s/fraction]
	  const double Jm_ = J_m(vmax25, Tl); //[mol/m² leaf/s/fraction]

	  daisy_assert (pn_>= 0.0);
	  Ass_ += LA * pn_; // [g/m²area/s] 
	  Res += LA * rd_;  // [g/m²area/s] 
	  daisy_assert (Ass_ >= 0.0);

	  //log variables:
	  Ass_vector[i]+= pn_* (molWeightCH2O / molWeightCO2) * LA;//[g CH2O/m²area/h]
	  Nleaf_vector[i]+= rubisco_Ndist[i] * LA * fraction[i]; //[mol N/m²area]OK
	  gs_vector[i]+= gsw * LA * fraction[i];    //[mol/m² area/s]
	  ci_vector[i]+= ci * fraction[i];  //[Pa] OK
	  Vm_vector[i]+= Vm_ * 1000.0 * LA * fraction[i]; //[mmol/m² area/s]OK
	  Jm_vector[i]+= Jm_ * 1000.0 * LA * fraction[i]; //[mmol/m² area/s]OK
	  sun_LAI_vector[i] = LA * fraction[i];//OK

	  ci_middel += ci * fraction[i]/(No + 0.0);// [Pa]   OK
	  gs += LA * gsw * fraction[i]; 
	  Ass += LA * pn_ * (molWeightCH2O / molWeightCO2);//[g CH2O/m2 area/h] OK
	  sun_LAI += LA * fraction[i];//OK
	  LAI += LA * fraction[i];//OK
	  Vmax += 1000.0 * LA * fraction[i] * Vm_;   //[mmol/m² area/s]
	  jm += 1000.0 * LA * fraction[i] * Jm_;     //[mmol/m² area/s]
	  leafPhotN += rubisco_Ndist[i] * LA *fraction[i]; //[mol N/m²area]; 
	  fraction_total += fraction[i]/(No + 0.0);
	  fraction_sun += fraction[i]/(No + 0.0);
	}
    }
  daisy_assert (approximate (accCAI, canopy.CAI));
  daisy_assert (Ass_ >= 0.0);

  return (molWeightCH2O / molWeightCO2)* Ass_;         //g CH2O /m2/h
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
  std::fill(sun_LAI_vector.begin (), sun_LAI_vector.end (), 0.0);
  ci_middel = 0.0;
  gs = 0.0;
  Ass = 0.0;
  Res = 0.0;
  PAR_ = 0.0;
  sun_LAI = 0.0;
  LAI = 0.0;
  Vmax = 0.0;
  jm = 0.0;
  leafPhotN = 0.0;
  fraction_sun = 0.0;
  fraction_total = 0.0;
}

void
PhotoFarquhar::output(Log& log) const
{
  output_variable (Ass_vector, log);
  output_variable (Nleaf_vector, log);
  output_variable (gs_vector, log);
  output_variable (ci_vector, log);
  output_variable (Vm_vector, log);
  output_variable (Jm_vector, log);
  output_variable (ci_middel, log);
  output_variable (gs, log);
  output_variable (Ass, log);
  output_variable (Res, log);
  output_variable (LAI, log);
  output_variable (sun_LAI, log);
  output_variable (sun_LAI_vector, log);
  output_variable (PAR_, log);
  output_variable (Vmax, log);
  output_variable (jm, log);
  output_variable (leafPhotN, log);
  output_variable (fraction_sun, log);
  output_variable (fraction_total, log);
}

void 
PhotoFarquhar::load_syntax (Syntax& syntax, AttributeList& alist)
{
  alist.add ("base_model", "Farquhar");

  alist.add ("description", "Faquhar et al. (1980) photosynthesis and Ball et al. (1987) stomataconductance model coupled as described by Collatz et al., 1991.");

  syntax.add ("Xn", "mol/mol/s", Check::positive (), Syntax::Const,
	      "Slope of relationship between leaf rubisco N and Vmax, Xn = 1.16E-3 mol/mol/s for wheat (de Pury & Farquhar, 1997)");
  alist.add ("Xn", 1.16e-3);

  syntax.add ("O2_atm", "Pa", Check::positive (), Syntax::Const,
	      "O2 partial pressure of atmosphere");
  alist.add ("O2_atm", 20500.0);

  syntax.add ("Gamma25", "Pa", Check::positive (), Syntax::Const,
	      "CO2 compensation point of photosynthesis. Gamma25 = 3.69 Pa for wheat (Collatz et al., 1991)");
  alist.add ("Gamma25", 3.69);

  syntax.add ("Ea_Gamma", "J/mol", Check::positive (), Syntax::Const,
	      "Actimation energy for Gamma. Ea_Gamma = 29000 (Jordan & Ogren, 1984)");
  alist.add ("Ea_Gamma", 29000.);

  syntax.add ("Ptot", "Pa", Check::positive (), Syntax::Const,
	      "Atmospheric pressure. Ptot = 100000 Pa");
  alist.add ("Ptot", 1.0E5);

  syntax.add ("m", Syntax::None (), Check::positive (), Syntax::Const,
	      "Stomatal slope factor. Ball and Berry (1982): m = 9 for soyabean. Wang and Leuning(1998): m = 11 for wheat");

  syntax.add ("b", "mol/m^2/s", Check::positive (), Syntax::Const,
	      "Stomatal intercept factor, Ball and Berry (1982) & Wang and Leuning(1998): (0.01 mol/m2/s)");

    syntax.add ("gbw", "s/m^2/mol", Check::positive (), Syntax::Const,
                "Leaf boundary conductance of water. gbw = 2 s/m2/mol (Collatz et al., 1991");
    alist.add ("gbw", 2.00);

  //log variables
  syntax.add ("ci_vector", "Pa", Syntax::LogOnly, Syntax::Sequence, "CO2 pressure in Stomatal in each layer.");
  syntax.add ("Vm_vector", "mmol/m^2/s", Syntax::LogOnly, Syntax::Sequence, "Photosynthetic capacity in each layer.");
  syntax.add ("Jm_vector", "mmol/m^2/s", Syntax::LogOnly, Syntax::Sequence, "Potential rate of electron transport in each layer.");
  syntax.add ("gs_vector", "mol/m^2/s", Syntax::LogOnly, Syntax::Sequence, "Stomata cunductance in each layer.");
  syntax.add ("Nleaf_vector", "mol N/m^2", Syntax::LogOnly, Syntax::Sequence, "Distribution of photosynthetic N-leaf.");
  syntax.add ("Ass_vector", "mol CH2O/m^2/s", Syntax::LogOnly, Syntax::Sequence, "Brutto assimilate.");
  syntax.add ("sun_LAI_vector", "mol CH2O/m^2/s", Syntax::LogOnly, Syntax::Sequence, "sunlit LAI.");

  syntax.add ("ci_middel", "Pa", Syntax::LogOnly, "Stomata average CO2 pressure.");
  syntax.add ("gs", "mol/m^2/s", Syntax::LogOnly, "Stomata conductance.");
  syntax.add ("Ass", "g CH2O/m^2", Syntax::LogOnly, "'Net' leaf assimilate of CO2 (brutto photosynthesis).");
  syntax.add ("Res", "g CH2O/m^2", Syntax::LogOnly, "Farquhar leaf respiration.");
  syntax.add ("LAI", "", Syntax::LogOnly, "Leaf area index for the canopy used in photosynthesis.");
  syntax.add ("sun_LAI", "", Syntax::LogOnly, "Leaf area index for the sunlit fraction.");
  syntax.add ("PAR_", "mol/m^2/day", Syntax::LogOnly, "PAR.");
  syntax.add ("Vmax", "[mmol/m^2/s]", Syntax::LogOnly, "Photosynthetic Rubisco capacity.");
  syntax.add ("jm", "[mmol/m^2/s]", Syntax::LogOnly, "Potential rate of electron transport.");
  syntax.add ("leafPhotN", "[mol N/m^2]", Syntax::LogOnly, "Content of photosynthetic active leaf N.");
  syntax.add ("fraction_sun", "", Syntax::LogOnly, "Fraction of sunlit in the canopy.");
  syntax.add ("fraction_total", "", Syntax::LogOnly, "Fraction of leaf contributing to the photosynthesis.");

  // Models
  syntax.add_object ("N-dist", RubiscoNdist::component, 
		     "Rubisco N-distribution in the canopy layer.");
  alist.add ("N-dist", RubiscoNdist::default_model ());

  syntax.add_object ("ABAeffect", ABAEffect::component, 
		     "The effect of xylem ABA on stomata conductivity.");
  alist.add ("ABAeffect", ABAEffect::default_model ());

}

static struct PhotoFarquharSyntax
{
  PhotoFarquharSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    PhotoFarquhar::load_syntax (syntax, alist);

    Librarian::add_base (Photo::component, alist, syntax);
  }
} PhotoFarquhar_syntax;
