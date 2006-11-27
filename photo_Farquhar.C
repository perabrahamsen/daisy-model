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

using namespace std;

// Chemical constants affecting the crop.
const double molWeightCH2O = 30.0; // [gCH2O/mol]
const double molWeightCO2 = 44.0;  // [gCO2/mol]

class PhotoFarquhar : public Photo
{
  // Parameters.
private:
  const double Vm25;  // Photosynthetic Rubisco capacity per unit leaf area at 25 degrees, (110 umol/m2/h)
  const double Jm25;  // Potential rate of electron transport per unit leaf area at 25 degrees, (2*110 umol/m2/h)
  const double S;     // Electron transport temperature response parametre, (710 J/mol/K) 
  const double H;     // Curvature parameter of Jm, (220000 J/mol)
  const double Ko25;  // Michaelis-Menten constant of Rubisco for O2 at 25 degrees, (24800 Pa)  
  const double Kc25;  // Michaelis-Menten constant of Rubisco for CO2 at 25 degrees, (40.4 Pa)
  const double O2_atm;// Oxygen partial pressure of the atmosphere, 20500 Pa 
  const double CO2_atm;//CO2 partial pressure of atmosphere, 35 Pa
  const double Gamma25;//CO2 compensation point of photosynthesis, 3.69 Pa
  const double rd25;  // leaf respiration per unit leaf area (0,0089*110 umol/m2/h)
  const double c_Vm;  // Temperature scaling constant for Vm, 26.35 J/mol
  const double Ea_Vm; // Activation energy for Vm, (65330 J/mol)
  const double Ea_Jm; // Activation energy for Jm, (37000 J/mol)
  const double Ea_ko; // Activation energy for ko, (36000 J/mol)
  const double Ea_kc; // Activation energy for kc, (59400 J/mol)
  const double Ea_rd; // Activation energy for rd, (66400 J/mol)
  const double Ea_Gamma; // Activation energy for Gamma, (2900 J/mol)
  const double Sv;    // Entropy term, 0,65 ??
  const double gbw;   // Leaf boundary conductance of water 
  const double theta; // Curvature of leaf response of electron transport to irradiance, 0.7 
  const double beta;  // Curvanture, 1
  const double alfa;  // Quantum efficiency. Fraction of PAR effectively absorbed by PSII, 0.08
  const double Ptot;  // Atmospheric pressure, (100000 Pa)
  const double m;     // Stomatal slope factor, Ball and Berry model, (unitless)
  const double b;     // Stomatal intercept factor, Ball and Berry model, (0.01 mol/m2/s)
  std::auto_ptr<CropNdist> cropNdist;// Crop N distribution model.

   
  // Log variable.
  std::vector<double> ci_vector; // Stomata CO2 pressure
  double ci_middel;              // Average stomata CO2 pressure per LAI units
  double Ass;                    // Netto assimilate of CO2 
  double LA;                     // Leaf Area index for a given leaf layer
  double PAR_;                   // Photosynthetic active radiation
    
  // Simulation.
public:
  double Conductance(double gb, Bioclimate& kd, Bioclimate& kb, double& Lai) const;
  double Transpiration(double Rni, double gr, double Ls) const;
  double Arrhenius (const double k25, const double Ea, double T) const;  
  double V_m (const double Vm_25m, double T)const;
  double J_m (double T) const;
  double C3Model (double& pn, double& ci, const double Q, const double gsw, 
		  const double T, const double vmax, const double rd, Treelog& msg);
  double GSTModel(double pn, double vp_ref, const double LA, 
		  const double fraction, double T, 
		  double gb, Treelog& msg);
  double assimilate (double T, const double cropN,
		     const std::vector<double>& PAR,
		     const std::vector<double>& PAR_Height,
		     const double PAR_LAI,
		     const std::vector<double>& fraction,
		     CanopyStandard& canopy,
		     Phenology& development, Treelog&);
  void clear ();
  void output (Log& log) const;
  
  // Create and Destroy.
public:
  PhotoFarquhar (Block& al)
    : Photo (al),
      Vm25 (al.number ("Vm25")),
      Jm25 (al.number ("Jm25")),
      S (al.number ("S")),
      H (al.number ("H")),
      Ko25 (al.number ("Ko25")),
      Kc25 (al.number ("Kc25")),
      O2_atm (al.number ("O2_atm")),
      CO2_atm (al.number ("CO2_atm")),
      Gamma25 (al.number ("Gamma25")),
      rd25 (al.number ("rd25")),
      c_Vm (al.number ("c_Vm")),
      Ea_Vm (al.number ("Ea_Vm")),
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
      cropNdist (Librarian<CropNdist>::build_item (al, "N-dist"))
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

// Boundary layer conductance of sunlit and shaded canopy fractions
double
PhotoFarquhar::Conductance(double gb, Bioclimate&/* kd*/, Bioclimate&/* kb*/, double& /*Lai*/) const
{
#if 0

  double Tref = 17.09; //??

  // Radiative conductance m/s
  // Resistance to radioactive transfer
  const double rho = 5.67*10e-8; //Stefan-Boltzmann constant W/m2/K
  const double pcp = -4.0252 * Tref + 1283.52; // J/m3/K  
  double R = pcp /(4*rho*(Tref+273)*(Tref+273)*(Tref+273)); // s/m
  //
  double grtotal = 1./R*0.99*2*(1.-exp(-kd*Lai)); //m/s
  double grsun   = 1./R*0.99*2*kd*(1.-exp(-(kd+kb)*Lai))/(kd+kb); //m/s
  double grshade = grtotal - grsun; //m/s

  const double gbconv = -0.1377*Tref+43.85818; //multiplikationsfaktor -> mol/m²/s
  //
  gb = gb * gbconv; // mol/m²/s
 
#endif
  return gb;
}
//Transpiration is the evaporation of water from aerial parts of plants, especially leaves but also stems, flowers and fruits. Transpiration is a side effect of the plant needing to open its stomata in order to obtain carbon dioxide gas from the air for photosynthesis.
double 
PhotoFarquhar::Transpiration(double/* Rni*/, double /*gr */, double/* Ls*/)const
{
  return -42.42e42;
}

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
  const double c_ = 1.+(exp((Sv *(T+273.15)-Ea_Vm)/(R*(T+273.15))));
  return vm_25 * a_/c_; //mol/m2/s
}

// Temperature response function for Jm according to De Pury & Farquhar (1997).
double
PhotoFarquhar::J_m (const double T/*[degree C]*/) const
{
  const double R = 8.314; //Gas constant, J/(mol K) 
  const double a_ = Jm25 * exp((T+273.0-298.0)*Ea_Jm/(298.0*R*(T+273.0)));
  const double b_ = 1.+exp((298.0*S-H)/(298.0*R));
  const double c_ = 1.+exp(S*(T+273.0)-H)/(R*(T+273.0));
  const double J_m = a_*b_/c_;     //mol/m2/s   OK!
  return J_m; 
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
PhotoFarquhar::C3Model (double& pn, double& ci, const double PAR /*[mol/m²/s]*/, 
			const double gsw /*[mol/m²/s]*/, const double T, 
			const double vmax /*[mol/m² leaf/s]*/, 
			const double rd /*[mol/m² leaf/s]*/, Treelog& msg)  
{
  // Updating teymperature dependent parameters:
  const double Vm = V_m(vmax, T); //[mol/m² leaf/s]
  daisy_assert (Vm >=0.0);
  const double Jm = J_m(T); //[mol/m² leaf/s]
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

      if (iszero((Ile+Jm)*(Ile+Jm)-(4.*theta*Ile*Jm)))
	{
	  std::ostringstream tmp;
	  tmp << "Bug: negative: " << 4.*theta*Ile*Jm 
	      << ", Ile = " << Ile << "Jm = " << Jm;
	  msg.message (tmp.str ());
	}
      daisy_assert(((Ile+Jm)*(Ile+Jm)-(4.*theta*Ile*Jm)) >= 0);
      daisy_assert (theta > 0.0);
      //const double J = (Ile+Jm-sqrt((Ile+Jm)*(Ile+Jm)-(4.*theta*Ile*Jm)))/(2.*theta);
      const double J = first_root_of_square_equation(theta, -(Ile+Jm), Ile*Jm); // [mol/m² leaf/s]

      // Gross CO2 uptake limited by RuBP
      daisy_assert ((ci + 2.0 * Gamma)> 0.0);
      const double we = J * (ci-Gamma)/(ci+2.*Gamma);//[mol/m² leaf/s]
      daisy_assert (Gamma >= 0.0);
      daisy_assert (ci >= 0.0);
      daisy_assert(((wc+we)*(we+wc)-(4.*beta*we*wc)) >= 0.0);
      daisy_assert ((beta)> 0.0);
      // const double p = ((wc+we)-sqrt((wc+we)*(we+wc)-(4.*beta*we*wc)))/(2.*beta);
      const double p = first_root_of_square_equation(beta ,-(wc+we), we*wc);//[mol/m² leaf/s]
      // Net CO2 uptake
      pn = p - rd; //net CO2 uptake [mol/m² leaf/s] 
      
      //Total resistance to CO2
      const double gtc = 1./(1.4*rbw+1.6*rsw);   //[mol/m² leaf/s]
      newci = ((gtc * CO2_atm /Ptot)-pn)/gtc*Ptot;//[Pa]

      double dp; 
      if (wc < we) 
	dp = Vm*(Kcl+Gamma)/((ci+Kcl)*(ci+Kcl));//[mol/m² leaf/s/Pa]
      else 
	dp = 3.0*J*(Gamma/((ci+2.*Gamma)*(ci+2.*Gamma))); //[mol/m² leaf/s/Pa]

      const double dcc = -(Ptot/gtc)*dp; // [unitless]
      newci = ci-(newci-ci)/(dcc-1.);    // Newtons next iteration, [Pa]
      lastci = ci; 
      if (newci > 0.) 
	ci = newci;
      else 
	{
	  ci = 0.5*CO2_atm;
	  newci = ci;
	}
      aiter =1+aiter;
    }  
  return pn;
}

double
PhotoFarquhar:: GSTModel(double pn, double rh_ref /*[unitless]*/, double LA,
			 double fraction, double gbw/*[mol/m2/s]*/, double T, 
			 Treelog& /*msg*/) 
{
  //The saturation water-vapour preassure
  const double fff = 1.0007 + 3.46 * 0.001;// enheder??
  const double aaa = 0.061121;
  const double bbb = 17.502;
  const double ccc = 240.97;
  // The saturation water-vapour pressure increase exponential with leaf temperature 
  double est = 10. * fff * (aaa * exp((bbb * T)/(ccc + T))); //[kPa]
  double wi = est * 1000. / Ptot;     //[unitless], water-vapour pressure at leaf surface relative to atmosphere
  const double vp_ref = rh_ref * est; //[kPa]
  double wa = vp_ref * 1000./Ptot;    //[unitless] 

  // Interception at leafsurface?? hvad er det??
  const double wsf = 1.0;      //[unitless], water stress factor
  const double intercept = b * LA * fraction * wsf; //[mol/m2/s]
  const double rbw = 1./gbw;   //[s*m2/mol]
  const double cs = CO2_atm - (1.4 * pn * Ptot * rbw); //[Pa]
  
  double pz; //[mol/m2/s]
  if(pn <= 0.) 
    pz = 1.0e-12;
  else 
    pz = pn;

  daisy_assert (cs > 0.0);
  const double aa = m * pz * Ptot / cs; //[mol/m2/s]
  const double bb = intercept + (1./rbw) - (m * pz * Ptot/cs);//[mol/m2/s]
  const double cc = (- wa /(wi * rbw)) - intercept;//[mol/m2/s]
  daisy_assert (aa > 0.0);

  double hs = (-bb + (sqrt(bb * bb - 4.* aa * cc)))/(2.* aa);//[unitless]
  if(hs > 1.)
    hs = 1.0;
  const double Ds = wi * (1.0 - hs) * Ptot;//[Pa]
  const double gamma = Arrhenius (Gamma25, Ea_Gamma, T);//Pa

  double gsw; //stomatal conductance
  if(pn <= 0.0) 
    gsw = intercept;//[mol/m2/s]
  else 
    gsw = ((m*pz*Ptot)/((cs-gamma)*(1.+ Ds/1000.)))+intercept;//[mol/m2/s]
  
  return gsw;//conductance
}

double
PhotoFarquhar::assimilate (const double T, const double cropN,
			   const vector<double>& PAR, 
			   const vector<double>& PAR_height,
			   const double PAR_LAI,
			   const std::vector<double>& fraction,
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
 
  // CAI below the current leaf layer.
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
  vector<double> crop_Ndist (No, 0.0);
  vector<double> crop_Vmax_total (No, 0.0);
  
  cropNdist->cropN_distribution (prevLA, crop_Ndist, crop_Vmax_total/*[mol/m²/s]*/, 
				 cropN /*[g/m²area]*/, msg);
  
  // Stomata CO2 preassure
  while (ci_vector.size () < No)
    ci_vector.push_back (0.0);
  double ci_acc = 0.0;
     
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
	  if (PAR[i] > 0.5 * 25.0)      //W/m2
	    development.light_hour ();
	}
      // Leaf Area index for a given leaf layer
      LA = prevLA - LAIvsH (height);
      daisy_assert (LA >= 0.0);
      if (LA > 0)
	{
	  prevLA = LAIvsH (height);
	  accCAI += LA;
	  
	  // PAR in mol/m2/s = PAR in W/m2 * 0.0000046
	  const double dPAR = (PAR[i] - PAR[i+1])/dCAI * 0.0000046; // W/m2->mol/m2/s
	  // log variable
	  PAR_ += dPAR * dCAI * 3600.0; //mol/m2/h

	  // Relative humidity
	  const double rh_ref = 0.67; //Relative humiditye, [unitless]
	  
	  // Photosynthetic rubisco capacity 
	  const double vmax = crop_Vmax_total[i] * fraction[i];// [mol/m² leaf/s]
	  daisy_assert (vmax >= 0.0);

	  // leaf respiration
	  const double rd_25 = 0.0089 * vmax;// [mol/m² leaf/s]
	  const double rd = Arrhenius(rd_25, Ea_rd, T); 
	  daisy_assert (rd >= 0.0);

	  //solving photosynthesis and stomatacondctance model for each layer
	  double ci  = 0.5 * CO2_atm;//first guess for ci, Pa
	  double gsw = LA / 5.0; //first gues for stomatal cond. gsw, mol/s/m2 

	  const int maxiter = 150;
	  int iter = 0;
	  double lastci;
	  
	  do
	    {
	      lastci = ci; //Stomata CO2 pressure 

	      //Transpiration(); returnerer Tl

	      //Calculating ci and "net"photosynthesis
	      pn = C3Model(pn, ci, dPAR, gsw, T, vmax, rd, msg);//[mol/m²/s]
	      gsw = GSTModel(pn, rh_ref, LA, fraction[i], gbw, T, msg);

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
	  const double pn_ = (pn + rd) * molWeightCO2 * 3600.0;  //mol CO2/m2/s->g CO2/m2/h
	  daisy_assert (pn_>= 0.0);
	  Ass_ += LA * pn_;
	  daisy_assert (Ass_ >= 0.0);
	  ci_acc += ci * LA;  

	  //log variables:
	  Ass += LA * pn_;
	  ci_vector[i]+= ci;
	}
    }
  daisy_assert (approximate (accCAI, canopy.CAI));

  ci_middel += ci_acc/accCAI;
  daisy_assert (Ass_ >= 0.0);
  return (molWeightCH2O / molWeightCO2)* Ass_;         //g CH2O /m2/h
}

void
PhotoFarquhar::clear ()
{
  std::fill(ci_vector.begin (), ci_vector.end (), 0.0);
  ci_middel = 0.0;
  Ass = 0.0;
  PAR_ = 0.0;
  LA = 0.0;
}

void
PhotoFarquhar::output(Log& log) const
{
  output_variable (ci_vector, log);
  output_variable (ci_middel, log);
  output_variable (Ass, log);
  output_variable (LA, log);
  output_variable (PAR_, log);
}

static struct Photo_FarquharSyntax
{
  static Photo&
  make (Block& al)
  { return *new PhotoFarquhar (al); }
  Photo_FarquharSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    PLF DS_null_eff;
    DS_null_eff.add (0.0, 1.00);
    DS_null_eff.add (2.0, 1.00);

    alist.add ("description", "Photosynthesis by De Pury & Farquhar (1997) and stomataconductance model by Collatz et al., 1991.");

    syntax.add ("Vm25", "mol/m^2/s", Check::positive (), Syntax::Const,
                "Photosynthetic Rubisco capacity per unit leaf area at 25 degrees. For wheat Vm25 = 110e-6 ( De Pury & Farquhar, 1997). For Barley Vm25 = 105e-6 (Thorgeirsson & Soegaard, 1999).");
    alist.add ("Vm25", 1.0e-6*110.);

    syntax.add ("Jm25", "mol/m^2/s", Check::positive (), Syntax::Const,
                "Potential rate of electron transport per unit leaf area at 25 degrees. Jm25 = 2.1*Vm25");
    alist.add ("Jm25", 1.0e-6*2.1*110.);

    syntax.add ("Kc25", "Pa", Check::positive (), Syntax::Const,
                "Micahyaelis-Menten constant of Rubisco for CO2. Kc25 = 40.4 Pa for wheat (Collatz et al., ) ");

    alist.add ("Kc25", 40.4);

    syntax.add ("Ko25", "Pa", Check::positive (), Syntax::Const,
                "Micahaelis-Menten constant of Rubisco for O2 at 25 degrees. Ko25 = 24800 Pa for wheat (Collatz et al., )");
    alist.add ("Ko25", 24800.);

    syntax.add ("Gamma25", "Pa", Check::positive (), Syntax::Const,
                "CO2 compensation point of photosynthesis. Gamma25 = 3.69 Pa for wheat? (Collatz et al., )");
    alist.add ("Gamma25", 3.69);

    syntax.add ("rd25", "mol/m2/s", Check::positive (), Syntax::Const,
                "Leaf respiration at 25 degrees. rd25 = 0.0089*Vm25, De Pury & Farquhar, 1997)");
    alist.add ("rd25", 1.0e-6*0.0089*110.);
    
    syntax.add ("S", "J/mol/K", Check::positive (), Syntax::Const,
                "Electron transport temperature response parameter,(De Pury & Farquhar, 1997)");
    alist.add ("S", 710.);
    
    syntax.add ("H", "J/mol", Check::positive (), Syntax::Const,
                "Curvature parameter of Jm, (De Pury & Farquhar, 1997)");
    alist.add ("H", 220000.);
    
    syntax.add ("c_Vm", "", Check::positive (), Syntax::Const,
                "Temperature scaling constant for Vmax. c_Vm, = 26.350 (Bernacchi et al., 2001)");
    alist.add ("c_Vm", 26.350);
    
    syntax.add ("Ea_Vm", "J/mol", Check::positive (), Syntax::Const,
                "Actimation energy for Vmax. Ea_Vm = 65330 J/mol (Ball, 1988)");
    alist.add ("Ea_Vm", 65330.);
   
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
                "Entropy term. Sv = 0.65 J/mol/K (Bernacchi et al., 2001?)");
    alist.add ("Sv", 0.65);

    syntax.add ("gbw", "s/m2/mol", Check::positive (), Syntax::Const,
                "Leaf boundary conductance of water. gbw = 2 s/m2/mol (Collatz et al., 1991");
    alist.add ("gbw", 2.00);

    syntax.add ("theta", " ", Check::positive (), Syntax::Const,
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
                "Stomatal slope factor. Collatz et al., 1991: m = 9 for  Wang and Leuning: m = 11 for wheat");
    alist.add ("m", 11.0);

    syntax.add ("b", "mol/m2/s", Check::positive (), Syntax::Const,
                "Stomatal intercept factor, Ball and Berry model, (0.01 mol/m2/s)");
    alist.add ("b", 0.01);

    syntax.add ("ci_vector", "Pa", Syntax::LogOnly, Syntax::Sequence, "CO2 pressure in Stomatal in each layer.");
    syntax.add ("ci_middel", "Pa", Syntax::LogOnly, "Average CO2 pressure in Stomatal per LAI.");
    syntax.add ("Ass", "g CO2/m2", Syntax::LogOnly, "Netto leaf assimilate of CO2.");
    syntax.add ("LA", "", Syntax::LogOnly, "Leaf area index.");
    syntax.add ("PAR_", "mol/m2/day", Syntax::LogOnly, "PAR.");


    Librarian<Photo>::add_type ("Farquhar", alist, syntax, &make);
    syntax.add ("N-dist", Librarian<CropNdist>::library (), 
		"N-distribution in the canopy layer.");
    alist.add ("N-dist", CropNdist::default_model ());


  }

} PhotoFarquhar_syntax;
