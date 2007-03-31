// photo_Farquhar.C -- Leaf photosynthesis based on De Pury & Farquhar, 1997.
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

#include "photo.h"
#include "block.h"
#include "cropNdist.h"
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

// Chemical constants affecting the crop.
const double molWeightCH2O = 30.0; // [gCH2O/mol]
const double molWeightCO2 = 44.0;  // [gCO2/mol]
const double molWeightH2O = 18.0;  // [gH2O/mol]
const double molWeightN = 14.0;    // [gN/mol]
class PhotoFarquhar : public Photo
{
  // Parameters.
private:
  const PLF& TempEff;	// Temperature effect, photosynthesis   
  const double S;     // Electron transport temperature response parametre 
  const double H;     // Curvature parameter of Jm
  const double Ko25;  // Michaelis-Menten constant of Rubisco for O2 at 25 degrees
  const double Kc25;  // Michaelis-Menten constant of Rubisco for CO2 at 25 degrees
  const double O2_atm;// Oxygen partial pressure of the atmosphere
  const double CO2_atm;//CO2 partial pressure of atmosphere
  const double Gamma25;//CO2 compensation point of photosynthesis
  const double c_Vm;  // Temperature scaling constant for Vm
  const double Ea_Vm; // Activation energy for Vm, (65330 J/mol)
  const double Eda_Vm;// Deactivation energy for Vm, (202900 J/mol)
  const double Ea_Jm; // Activation energy for Jm, (37000 J/mol)
  const double Ea_ko; // Activation energy for ko, (36000 J/mol)
  const double Ea_kc; // Activation energy for kc, (59400 J/mol)
  const double Ea_rd; // Activation energy for rd, (66400 J/mol)
  const double Ea_Gamma; // Activation energy for Gamma
  const double Sv;    // Entropy term
  const double gbw;   // Leaf boundary conductance of water 
  const double theta; // Curvature of leaf response of electron transport to irradiance
  const double beta;  // Curvanture
  const double alfa;  // Quantum efficiency. Fraction of PAR effec. absorbed by PSII
  const double Ptot;  // Atmospheric pressure
  const double m;     // Stomatal slope factor, Ball and Berry model
  const double b;     // Stomatal intercept factor, Ball and Berry model
  std::auto_ptr<CropNdist> cropNdist;// Crop N distribution model.
  std::auto_ptr<ABAEffect> ABAeffect;// ABA-xylem effect on photosynthesis.

  // Log variable.
  std::vector<double> ci_vector; // Stomata CO2 pressure
  std::vector<double> Vm_vector; // Photosynthetic capacity  
  std::vector<double> Nleaf_vector; // Distribution of photosynthetic N-leaf  
  std::vector<double> Ass_vector; // Brutto assimilate  
  std::vector<double> Jm_vector; // Potential rate of electron transport
  std::vector<double> gs_vector; // Stomata cunductance
  std::vector<double> sun_LAI_vector; // sunlit LAI
  double ci_middel;              // Average stomata CO2 pressure per LAI units
  double Ass;                    // 'Netto' assimilate of CO2 
  double Res;                    // Leaf respiration of CO2 
  double LAI;                    // Leaf Area index for the canopy
  double sun_LAI;                // Leaf Area index for the sunlit fraction
  double PAR_;                   // Photosynthetic active radiation
  double gs;                     // Stomata conductance
  double Vmax;                   // Photosynthetic Rubisco capacity
  double jm;                     // Potential rate of electron transport 
  double leafPhotN;              // Content of photosynthetic active leaf N
  double fraction_sun;           // fraction of sunlit in the canopy.
  double fraction_total;         // total fraction of leaf active in photosynthesis.
    
  // Simulation.
public:
  double Arrhenius (const double k25, const double Ea, double T) const;  
  double V_m (const double Vm_25m, double T) const;
  double J_m (const double vmax25, const double T) const;
  double Sat_vapor_pressure (const double T) const;
  double C3Model (double& pn, double& ci, const double Q, const double gsw, 
		  const double T, const double vmax, const double rd, Treelog& msg);
  double GSTModel(double ABA, double pn, double vp_ref, const double LA, 
		  const double fraction, const double Ta, const double Tl,  
		  double gb, Treelog& msg);
  double assimilate (const double ABA_xylem, const double rel_hum, 
		     double Ta, double Tl, const double cropN,
		     const std::vector<double>& PAR,
		     const std::vector<double>& PAR_Height,
		     const double PAR_LAI,
		     const std::vector<double>& fraction,
                     double dt,
		     CanopyStandard& canopy,
		     Phenology& development, Treelog&);
  void clear ();
  void output (Log& log) const;
  
  // Create and Destroy.
public:
  PhotoFarquhar (Block& al)
    : Photo (al),
      TempEff (al.plf ("TempEff")),
      S (al.number ("S")),
      H (al.number ("H")),
      Ko25 (al.number ("Ko25")),
      Kc25 (al.number ("Kc25")),
      O2_atm (al.number ("O2_atm")),
      CO2_atm (al.number ("CO2_atm")),
      Gamma25 (al.number ("Gamma25")),
      c_Vm (al.number ("c_Vm")),
      Ea_Vm (al.number ("Ea_Vm")),
      Eda_Vm (al.number ("Eda_Vm")),
      Ea_Jm (al.number ("Ea_Jm")),
      Ea_ko (al.number ("Ea_ko")),
      Ea_kc (al.number ("Ea_kc")),
      Ea_rd (al.number ("Ea_rd")),
      Ea_Gamma (al.number ("Ea_Gamma")),
      Sv (al.number ("Sv")),
      gbw (al.number("gbw")),
      theta (al.number("theta")),
      beta (al.number("beta")),
      alfa (al.number("alfa")),
      Ptot (al.number("Ptot")),
      m (al.number("m")),
      b (al.number("b")),
      cropNdist (Librarian::build_item<CropNdist> (al, "N-dist")),
      ABAeffect (Librarian::build_item<ABAEffect> (al, "ABAeffect"))
  { }
  ~PhotoFarquhar ()
  { }
};

#if 0
//leaf temperature of sunlit and shaded leafs
double
PhotoFarquhar::Tl (const double k, const double T)
{
  //leaf temperature sunlit and shaded leaf
  const double tl;
  return tl;
}
#endif

// Arrhenius temperature response function used for Kc, Ko og Gamma
double
PhotoFarquhar::Arrhenius (const double k25, const double Ea, const double T) const 
{
  const double R = 8.314; //Gas constant, J/(mol K) 
  return k25 * exp(Ea * (T-25.0)/(298.0*R*(T+273.0)));//Pa OK!
}

// Temperature response function for Vmax according to Bernacchi et al., 2001.
double
PhotoFarquhar::V_m (const double vm_25, const double T) const
{
  const double R = 8.314; //Gas constant, J/(mol K) 
  const double a_ = exp(c_Vm-Ea_Vm/(R*(T+273.15))); 
  const double c_ = 1.+(exp((Sv *(T+273.15)-Eda_Vm)/(R*(T+273.15))));
  return vm_25 * a_/c_;  //mol/m2/s
}

// Temperature response function for Jm according to De Pury & Farquhar (1997).
double
PhotoFarquhar::J_m (const double vmax25, const double T/*[degree C]*/) const
{
  const double Jm25 = 2.1 * vmax25;
  const double R = 8.314; //Gas constant, J/(mol K) 
  const double a_ = Jm25 * exp((T+273.0-298.0)*Ea_Jm/(298.0*R*(T+273.0)));
  const double b_ = 1.+exp((298.0*S-H)/(298.0*R));
  const double c_ = 1.+exp(S*(T+273.0)-H)/(R*(T+273.0));
  double J_m = a_*b_/c_;     //mol/m2/s  

  // Temperature effect and development stage effect of photo_gl
  const double Teff = TempEff (T);
  return J_m * Teff; 
}

inline double pow2 (double x)
{ return x * x; }

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
  const double x1 = (-b + D) / (2.0 * a);
  // Return the first solution.
  return x1;
}

double 
PhotoFarquhar::C3Model (double& pn, double& ci, 
			const double PAR /*[mol/m²/s]*/, 
			const double gsw /*[mol/m²/s]*/, const double T, 
			const double vmax25 /*[mol/m² leaf/s]*/, 
			const double rd /*[mol/m² leaf/s]*/, Treelog& msg)  
{
  // Updating temperature dependent parameters:
  const double Vm = V_m(vmax25, T); //[mol/m² leaf/s]
  daisy_assert (Vm >=0.0);
  const double Jm = J_m(vmax25, T); //[mol/m² leaf/s]
  daisy_assert (Jm >=0.0);
  const double Ko = Arrhenius(Ko25, Ea_ko, T); //[Pa]
  daisy_assert (Ko > 0.0);
  const double Kc = Arrhenius(Kc25, Ea_kc, T); //[Pa]
  const double Gamma = Arrhenius (Gamma25, Ea_Gamma, T);//[Pa]
  const double Kcl = Kc *(1.+ O2_atm/Ko); //[Pa]
  daisy_assert (gbw > 0.0);
  daisy_assert (gsw > 0.0);
  double rbw = 1./gbw; // leaf boundary resistance to water vapor, [m²leaf*s/mol]
  double rsw = 1./gsw; // stomatal resistance to water [m²leaf*s/mol]
  
  //Newton Raphsons solution to 'net'photosynthesis and stomatal conductance.
  const int maxiter = 150;
  int aiter; 
  double lastci, newci;

  if (isnormal (ci)) 
    lastci = ci;
  else 
    lastci = CO2_atm;
  newci = 0.;
  aiter = 0;

  while(std::abs(newci - lastci) > 0.01)
    {
      if (aiter > maxiter)
	{
	  std::ostringstream tmp;
	  tmp << "Bug: total iterations in C3 model exceed " << maxiter;
	  msg.error (tmp.str ());
	  break;
	}
      // Gross CO2 uptake limited by Rubisco
      daisy_assert ((ci + Kcl)>0.0); 
      const double wc = Vm*(ci-Gamma)/(ci+Kcl); // Rubisco limited, [mol/m² leaf/s]
      const double Ile = PAR * alfa;            // PAR effectively absorbed by PSII [mol/m² leaf/s]
      const double J = first_root_of_square_equation(theta, -(Ile+Jm), Ile*Jm); // [mol/m² leaf/s]

      // Gross CO2 uptake limited by RuBP
      daisy_assert ((ci + 2.0 * Gamma)> 0.0);
      const double we = J * (ci-Gamma)/(ci + 2.* Gamma);//[mol/m² leaf/s]
      daisy_assert (Gamma >= 0.0);
      daisy_assert (ci >= 0.0);
      daisy_assert(((wc+we)*(we+wc)-(4.*beta*we*wc)) >= 0.0);
      daisy_assert ((beta)> 0.0);
      const double p = first_root_of_square_equation(beta, -(wc+we), we*wc);//[mol/m² leaf/s]
      // Net CO2 uptake
      pn = p - rd; // [mol/m² leaf/s] 
      
      //Total resistance to CO2
      const double gtc = 1./(1.4*rbw+1.6*rsw);   //[mol/m² leaf/s]
      newci = ((gtc * CO2_atm /Ptot)-pn)/gtc*Ptot;//[Pa]
      
      double dp;
      if (wc < we) 
	dp = Vm * (Kcl+Gamma)/((ci+Kcl)*(ci+Kcl));//[mol/m² leaf/s/Pa]
      else 
	dp = 3.0 * J *(Gamma/((ci+2. * Gamma)*(ci+2.* Gamma))); //[mol/m² leaf/s/Pa]

      const double dcc = -(Ptot/gtc)*dp; // [unitless]
      newci = ci-(newci-ci)/(dcc-1.);    // Newtons next iteration, [Pa]
      lastci = ci; 
      if (newci > 0.) 
	ci = newci;
      else 
	{
	  ci = 0.5 * CO2_atm;
	  newci = ci;
	}
      aiter = 1 + aiter;
    }  
  return pn; // [mol/m² leaf/s] 
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
PhotoFarquhar:: GSTModel(double ABA, double pn, double rel_hum /*[unitless]*/, 
			 double LA, double fraction, double gbw/*[mol/m2/s]*/, 
			 const double Ta, const double Tl, Treelog&) 
{
  const double wsf = ABA; //water stress function
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
  vector<double> crop_Ndist (No, 0.0);
  vector<double> crop_Vmax_total (No, 0.0);

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


  cropNdist->cropN_distribution (prevLA, crop_Ndist/*[mol/m²leaf]*/, 
				 crop_Vmax_total/*[mol/m²leaf/s]*/, 
				 cropN /*[g/m²area]*/, msg);

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
	  const double vmax25 = crop_Vmax_total[i]*fraction[i];//[mol/m²leaf/s/fracti.]
	  daisy_assert (vmax25 >= 0.0);

	  // Photosynthetic effect of Xylem ABA.
	  const double ABA = ABAeffect->ABA_effect(ABA_xylem, msg);

	  // leaf respiration
	  const double rd_25 = 0.0089 * vmax25;// [mol/m² leaf/s]
	  const double rd = Arrhenius(rd_25, Ea_rd, Tl); 
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

	      //Transpiration(); returnerer Tl

	      //Calculating ci and "net"photosynthesis
	      pn = C3Model(pn, ci, dPAR, gsw, Tl, vmax25, rd, msg);//[mol/m²leaf/s/fraction]
	      gsw = GSTModel(ABA, pn, rel_hum, LA, fraction[i], gbw, Ta, Tl, msg);//[mol/s/m²leaf/fraction]

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
	  Nleaf_vector[i]+= crop_Ndist[i] * LA * fraction[i]; //[mol N/m²area]OK
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
	  leafPhotN += crop_Ndist[i] * LA *fraction[i]; //[mol N/m²area]; 
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

static struct Photo_FarquharSyntax
{
  static Model& make (Block& al)
  { return *new PhotoFarquhar (al); }
  Photo_FarquharSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    PLF DS_null_eff;
    DS_null_eff.add (0.0, 1.00);
    DS_null_eff.add (2.0, 1.00);

    alist.add ("description", "Photosynthesis by De Pury & Farquhar (1997) and stomataconductance model by Collatz et al., 1991.");

    syntax.add ("TempEff", "dg C", Syntax::None (), Check::non_negative (),
                Syntax::Const,
                "Temperature factor for assimilate production.");

    syntax.add ("Kc25", "Pa", Check::positive (), Syntax::Const,
                "Micahyaelis-Menten constant of Rubisco for CO2. Kc25 = 40.4 Pa for wheat (Collatz et al.,1991) ");

    alist.add ("Kc25", 40.4);

    syntax.add ("Ko25", "Pa", Check::positive (), Syntax::Const,
                "Micahaelis-Menten constant of Rubisco for O2 at 25 degrees. Ko25 = 24800 Pa for wheat (Collatz et al., 1991)");
    alist.add ("Ko25", 24800.);

    syntax.add ("Gamma25", "Pa", Check::positive (), Syntax::Const,
                "CO2 compensation point of photosynthesis. Gamma25 = 3.69 Pa for wheat (Collatz et al., 1991)");
    alist.add ("Gamma25", 3.69);

    
    syntax.add ("S", "J/mol/K", Check::positive (), Syntax::Const,
                "Electron transport temperature response parameter,(De Pury & Farquhar, 1997)");
    alist.add ("S", 710.);
    
    syntax.add ("H", "J/mol", Check::positive (), Syntax::Const,
                "Curvature parameter of Jm, (De Pury & Farquhar, 1997)");
    alist.add ("H", 220000.);
    
    syntax.add ("c_Vm", "", Check::positive (), Syntax::Const,
                "Temperature scaling constant for Vmax. c_Vm, = 26.35 (Bernacchi et al., 2001)");
    alist.add ("c_Vm", 26.35);
    
    syntax.add ("Ea_Vm", "J/mol", Check::positive (), Syntax::Const,
                "Activation energy for Vmax. Ea_Vm = 65330 J/mol (Ball, 1988)");
    alist.add ("Ea_Vm", 65330.);

    syntax.add ("Eda_Vm", "J/mol", Check::positive (), Syntax::Const,
                "Deactimation energy for Vmax. Eda_Vm = 202900 J/mol");
    alist.add ("Eda_Vm", 202900.);
   
    syntax.add ("Ea_Jm", "J/mol", Check::positive (), Syntax::Const,
                "Actimation energy for Jm. Ea_Jm = 37000 J/mol (Farquhar et al., 1980).");
    alist.add ("Ea_Jm", 37000.);

    syntax.add ("Ea_ko", "J/mol", Check::positive (), Syntax::Const,
                "Actimation energy for ko. Ea_ko 0 36000 J/mol (Badger & Collatz, 1977).");
    alist.add ("Ea_ko", 36000.);

    syntax.add ("Ea_kc", "J/mol", Check::positive (), Syntax::Const,
                "Actimation energy for kc. Ea_kc = 59400 J/mol (Badger & Collatz, 1977)");
    alist.add ("Ea_kc", 59400.);

    syntax.add ("Ea_rd", "J/mol", Check::positive (), Syntax::Const,
                "Actimation energy for rd. Ea_rd = 66400 J/mol (Farquhar et al., 1980)");
    alist.add ("Ea_rd", 66400.);

    syntax.add ("Ea_Gamma", "J/mol", Check::positive (), Syntax::Const,
                "Actimation energy for Gamma. Ea_Gamma = 29000 (Jordan & Ogren, 1984)");
    alist.add ("Ea_Gamma", 29000.);

    syntax.add ("Sv", "J/mol/K", Check::positive (), Syntax::Const,
                "Entropy term. Sv = 650 J/mol/K");
    alist.add ("Sv", 650.);

    syntax.add ("gbw", "mol/m2/s", Check::positive (), Syntax::Const,
                "Leaf boundary conductance of water. gbw = 2 mol/m2/s (Collatz et al., 1991");
    alist.add ("gbw", 2.00);

    syntax.add ("theta", "", Check::positive (), Syntax::Const,
                "Curvature of leaf response of electron transport to irradiance, (De Pury & Farquhar, 1997");
    alist.add ("theta", 0.7);
    
    syntax.add ("beta", " ", Check::positive (), Syntax::Const,
                "Curvanture");
    alist.add ("beta", 1.0);

    syntax.add ("alfa", "mol/mol", Check::positive (), Syntax::Const,
                "Fraction of PAR effectively absorbed by PSII, ");
    alist.add ("alfa", 0.08);

    syntax.add ("CO2_atm", "Pa", Check::positive (), Syntax::Const,
                "CO2 partial pressure of atmosphere");
    alist.add ("CO2_atm", 35.0);

    syntax.add ("O2_atm", "Pa", Check::positive (), Syntax::Const,
                "O2 partial pressure of atmosphere");
    alist.add ("O2_atm", 20500.0);

    syntax.add ("Ptot", "Pa", Check::positive (), Syntax::Const,
                "Atmospheric pressure. Ptot = 100000 Pa");
    alist.add ("Ptot", 1.0E5);

    syntax.add ("m", " ", Check::positive (), Syntax::Const,
                "Stomatal slope factor. Ball and Berry (1982): m = 9 for soyabean. Wang and Leuning(1998): m = 11 for wheat");
    alist.add ("m", 11.0);

    syntax.add ("b", "mol/m2/s", Check::positive (), Syntax::Const,
                "Stomatal intercept factor, Ball and Berry (1982) & Wang and Leuning(1998): (0.01 mol/m2/s)");
    alist.add ("b", 0.01);

    //log variables
    syntax.add ("ci_vector", "Pa", Syntax::LogOnly, Syntax::Sequence, "CO2 pressure in Stomatal in each layer.");
    syntax.add ("Vm_vector", "mmol/m2/s", Syntax::LogOnly, Syntax::Sequence, "Photosynthetic capacity in each layer.");
    syntax.add ("Jm_vector", "mmol/m2/s", Syntax::LogOnly, Syntax::Sequence, "Potential rate of electron transport in each layer.");
    syntax.add ("gs_vector", "mol/m2/s", Syntax::LogOnly, Syntax::Sequence, "Stomata cunductance in each layer.");
    syntax.add ("Nleaf_vector", "mol N/m2", Syntax::LogOnly, Syntax::Sequence, "Distribution of photosynthetic N-leaf.");
    syntax.add ("Ass_vector", "mol CH2O/m2/s", Syntax::LogOnly, Syntax::Sequence, "Brutto assimilate.");
    syntax.add ("sun_LAI_vector", "mol CH2O/m2/s", Syntax::LogOnly, Syntax::Sequence, "sunlit LAI.");

    syntax.add ("ci_middel", "Pa", Syntax::LogOnly, "Stomata average CO2 pressure.");
    syntax.add ("gs", "mol/m2/s", Syntax::LogOnly, "Stomata conductance.");
    syntax.add ("Ass", "g CH2O/m2", Syntax::LogOnly, "'Net' leaf assimilate of CO2.");
    syntax.add ("Res", "g CH2O/m2", Syntax::LogOnly, "Farquhar leaf respiration.");
    syntax.add ("LAI", "", Syntax::LogOnly, "Leaf area index for the canopy used in photosynthesis.");
    syntax.add ("sun_LAI", "", Syntax::LogOnly, "Leaf area index for the sunlit fraction.");
    syntax.add ("PAR_", "mol/m2/day", Syntax::LogOnly, "PAR.");
    syntax.add ("Vmax", "[mmol/m2/s]", Syntax::LogOnly, "Photosynthetic Rubisco capacity.");
    syntax.add ("jm", "[mmol/m2/s]", Syntax::LogOnly, "Potential rate of electron transport.");
    syntax.add ("leafPhotN", "[mol N/m2]", Syntax::LogOnly, "Content of photosynthetic active leaf N.");
    syntax.add ("fraction_sun", "", Syntax::LogOnly, "Fraction of sunlit in the canopy.");
    syntax.add ("fraction_total", "", Syntax::LogOnly, "Fraction of leaf contributing to the photosynthesis.");

    // Models
    syntax.add_object ("N-dist", CropNdist::component, 
                       "N-distribution in the canopy layer.");
    alist.add ("N-dist", CropNdist::default_model ());

    syntax.add_object ("ABAeffect", ABAEffect::component, 
                       "The effect of xylem ABA on stomata conductivity.");
    alist.add ("ABAeffect", ABAEffect::default_model ());

    Librarian::add_type (Photo::component, "FC_C3", alist, syntax, &make);
  }

} PhotoFarquhar_syntax;
