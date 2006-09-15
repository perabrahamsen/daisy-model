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
#include "bioclimate.h"
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
const double molWeightCO2 = 44.0;  // [gCO2/mol]

class PhotoFarquhar : public Photo
{
  // Parameters.
private:
    const double Qeff;	// Quantum efficiency at low light
    //double Vmax;      // Max assimilation rate 
    //double Vmsun;     // 
    //double Vmshade;   //
    //double Jm;        // Potential rate of electron transport per unit leaf area
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
    const double Ea_Vm; // Activation energy for Vm, (65.33 KJ/mol)
    const double Ea_Jm; // Activation energy for Jm, (37000 J/mol)
    const double Ea_ko; // Activation energy for ko, (36000 J/mol)
    const double Ea_kc; // Activation energy for kc, (59400 J/mol)
    const double Ea_rd; // Activation energy for rd, (66400 J/mol)
    const double Ea_Gamma; // Activation energy for Gamma, (2900 J/mol)
    const double Sv;    // Entropy term, 0,65 ??
    const double theta; // Curvature of leaf response of electron transport to irradiance, 0.7 
    const double beta;  // Curvanture, 1
    const double alfa;  // Quantum efficiency. Fraction of PAR effectively absorbed by PSII, 0.08
    const double Ptot;  // Atmospheric pressure, (100000 Pa)
    const double m;     // Stomatal slope factor, Ball and Berry model, (unitless)
    const double b;     // Stomatal intercept factor, Ball and Berry model, (0.01 mol/m2/s)
    
    const PLF& TempEff;	// Temperature effect, photosynthesis
    const PLF& DSEff;	// Development stage effect, photosynthesis
    const PLF& DAPEff;	// Age effect, photosynthesis

// Simulation.
public:
    double Conductance(double gb, Bioclimate& kd, Bioclimate& kb, double& Lai) const;
    double Transpiration(double Rni, double gbw, double gr, double Ls) const;
    double Arrhenius (const double k25, const double Ea, double T) const;  
    double V_m (double T)const;
    double J_m (double T) const;
    double C3Model (double Q, double gbw,double gsw, double T, double& rsw, 
		    double& ci,  Treelog& msg) const;
    double GSTModel(double vp_ref, double pn, double LA, double T, 
		    double gb, double Ls, Treelog& msg) const;

    double assimilate (double T,
		       const std::vector<double>& PAR,
		       const std::vector<double>& PAR_Height,
		       const double PAR_LAI,
		       CanopyStandard& canopy,
		       Phenology& development, Treelog&) const;
    void output (Log&) const
      { }

// Create and Destroy.
public:
    PhotoFarquhar (Block& al)
      : Photo (al),
      Qeff (al.number ("Qeff")),
      
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
      theta (al.number("theta")),
      beta (al.number("beta")),
      alfa (al.number("alfa")),
      Ptot (al.number("Ptot")),
      m (al.number("m")),
      b (al.number("b")),

      TempEff (al.plf ("TempEff")),
      DSEff (al.plf ("DSEff")),
      DAPEff (al.plf ("DAPEff"))
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
PhotoFarquhar::Conductance(double gb, Bioclimate& kd, Bioclimate& kb, double& Lai) const
{
#if 0

  /*
  // ??
  const double rh_ref = 0.67;
  double vp_ref = rh_ref * e_sat; //kPa
  //relative to atmosphere
  double wa = vp_ref*1000/Ptot;  //Pa/Pa -> unitless
  */ 

  double Tref = 17.09; //??

  // Radiative conductance m/s
  // Resistance to radioactive transfer
  const double rho = 5.67*10e-8; //Stefan-Boltzmann constant W/m2/K
  const double pcp = -4.0252 * Tref + 1283.52; // J/m3/K  
  double R = pcp /(4*rho*(Tref+273)*(Tref+273)*(Tref+273)); // s/m
  //
  double grtotal = 1/R*0.99*2*(1-exp(-kd*Lai)); //m/s
  double grsun   = 1/R*0.99*2*kd*(1-exp(-(kd+kb)*Lai))/(kd+kb); //m/s
  double grshade = grtotal - grsun; //m/s

  
  const double gbconv = -0.1377*Tref+43.85818; //multiplikationsfaktor -> mol/m²/s
  //
  gb = gb * gbconv; // mol/m²/s
 
#endif
  return gb;
}
//Transpiration is the evaporation of water from aerial parts of plants, especially leaves but also stems, flowers and fruits. Transpiration is a side effect of the plant needing to open its stomata in order to obtain carbon dioxide gas from the air for photosynthesis.
double 
PhotoFarquhar::Transpiration(double Rni/**/, double gbw/**/, double gr /**/,
			     double Ls/**/)const
{

}

// Arrhenius temperature response function used for Kc, Ko
double
PhotoFarquhar::Arrhenius (const double k25, const double Ea, const double T) const 
{
  const double R = 8.314; //Gas constant, J/(mol K) 
  return k25 * exp(Ea * (T-25.0)/(298.0*R*(T+273.0)));//Pa OK!
}

// Temperature response function for Vmax according to Bernacchi et al., 2001.
double
PhotoFarquhar::V_m (const double T) const
{
  const double R = 8.314; //Gas constant, J/(mol K) 
  const double a = exp((c_Vm-Ea_Vm)/(R*(T+273.15))); //unitless ?
  const double c = 1+(exp((Sv *(T+273.15)-Ea_Vm)/(R*(T+273.15))));
  return Vm25 * a/c; //umol/m2/h 
}

// Temperature response function for Jm according to De Pury & Farquhar (1997).
double
PhotoFarquhar::J_m (const double T) const
{
  const double R = 8.314; //Gas constant, J/(mol K) 
  return Jm25 * exp((T+273.0-298.0)*Ea_Jm/(298.0*R*(T+273.0)))*
             (1+exp((298.0*S-H)/(298.0*R)))/
             (1+exp(S*(T+273.0)-H)/(R*(T+273.0))); //umol/m2/h   OK!
}
double 
PhotoFarquhar::C3Model (double Q /* */, double gbw /* */, double gsw /* */,
			double T, double& rsw /* */, double& ci, Treelog& msg) const 
{
  // Updating temperature dependent parameters:
  const double Vm = V_m(T); //umol/m2/h
  const double Jm = J_m(T); //umol/m2/h
  const double Ko = Arrhenius(Ko25, Ea_ko, T);//Pa
  const double Kc = Arrhenius(Kc25, Ea_kc, T);//Pa
  const double rd = Arrhenius(rd25, Ea_rd, T);//umol/m2/h
  const double Gamma = Arrhenius (Gamma25, Ea_Gamma, T);//Pa

  const double Kcl = Kc *(1+ O2_atm/Ko); //Pa

  double rbw = 1/gbw; //boundary layer resistance to water vapor, m2*s/mol
  rsw = 1/gsw; 
  
  //Newton Raphsons solution to ...
  const double maxiter = 150.0;
  double aiter, lastci, newci;
  
  if (ci == 0) lastci = CO2_atm;//Pa
  else lastci = ci;
  newci = 0;
  aiter = 0;
  double pn; //netto assimilate

  while(std::abs(newci - lastci) > 0.01)
    {
      if (aiter < maxiter)
	{
	  double wc = Vm*(ci-Gamma)/(ci+Kcl); // Rubisco limited, umol/m2/h
          double Ile = Q * alfa;              // PAR effectively absorbed by PSII
	  double J = (Ile+Jm-sqrt((Ile+Jm)*(Ile+Jm)-(4*theta*Ile*Jm)))/(2*theta); 
	  double we = J * (ci-Gamma)/(ci+2*Gamma);
	  double p = ((wc+we)-sqrt((wc+we)*(we+wc)-(4*beta*we*wc)))/(2*beta); 
	  pn = p - rd; //net CO2 uptake mol/m2/s 
	  double gtc = 1/(1.4*rbw+1.6*rsw);   // Total conductance to CO2
	  newci = ((gtc * CO2_atm /Ptot) -pn)/(gtc)*Ptot;
	  double dp;
	  if (wc < we) dp = Vm*(Kcl+Gamma)/((ci+Kcl)*(ci+Kcl));
	  else dp = 3.0*J*(Gamma/((ci+2*Gamma)*(ci+2*Gamma))); 
	  double dcc = -(Ptot/gtc)*dp;
	  newci = ci-(newci-ci)/(dcc-1); // Newtons next iteration
	  lastci = ci; 
	  if (newci > 0) ci = newci;
	  else ci = 0.5*CO2_atm;
	  aiter = +aiter;
        }
      else 
	{
	  std::ostringstream tmp;
	  tmp << "Bug: total iterations in C3 model exceed " << maxiter;
	  msg.error (tmp.str ());
	  break;
	}
    }
  return pn;
}

double
PhotoFarquhar:: GSTModel(double vp_ref, double pn, double LA, double gbw, double T, double Ls, Treelog& msg) const
{
  double wa = vp_ref * 1000/Ptot; //Pa, water-vapour preassure in atmosphere?
 
  //The saturation water-vapour preassure
  const double fff = 1.0007+3.46*0.001;
  const double aaa = 0.061121;
  const double bbb = 17.502;
  const double ccc = 240.97;
  // The saturation water-vapour pressure increase exponential with leaf temperature 
  double est = 10*fff*(aaa*exp((bbb*T)/(ccc+T))); //kPa
  double wi = est *1000 / Ptot;//Pa/Pa -> unitless, water-vapour pressure at leaf surface relative to atmosphere? 

  // Interception at leafsurface?? hvad er det??
  const double b = 0.01;  //?? unit??
  const double wsf = 1.0; //?? water stress factor
  double intercept = b*LA*wsf;
  double rbw = 1/gbw;     //gbw = bulk leaf boundary conductance of canopy
  double cs = CO2_atm-(1.4*pn*Ptot*rbw);
  
  double pz; //
  if(pn <= 0) pz = 1.0e-12;
  else pz = pn;

  double aa = m*pz*Ptot/cs;
  double bb = intercept+(1/rbw)-(m*pz*Ptot/cs);
  double cc = (-wa/(wi*rbw))-intercept;
  double hs = (-bb+(sqrt(bb*bb-4*aa*cc)))/(2*aa);
  if(hs > 1) hs = 1.0;
  double Ds = wi*(1.0-hs)*Ptot;//Pa
  //double es = hs*wi*Ptot;      //pa
  const double gamma = Arrhenius (Gamma25, Ea_Gamma, T);//Pa

  double gsw; //stomatal conductance
  if(pn <= 0) gsw = intercept;
  else gsw = ((m*pz*Ptot)/((cs-gamma)*(1+Ds/1000)))+intercept;
  
  return gsw;
}

double
PhotoFarquhar::assimilate (const double T,
                     const vector<double>& PAR,
                     const vector<double>& PAR_height,
                     const double PAR_LAI,
                     CanopyStandard& canopy,
                     Phenology& development,
                     Treelog& msg) const
{
  // sugar production [gCH2O/m2/h] by canopy photosynthesis.
  const PLF& LAIvsH = canopy.LAIvsH;
  const double DS = development.DS;
  const double DAP = development.DAP;

  // Temperature effect and development stage effect
  const double Teff = TempEff (T) * DSEff (DS) * DAPEff (DAP);

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
	  
	  double kd = 1.0;
	  double vp_ref = 1.0;
	  double gb = 1.0; // Conductance(gb, kd, kb, LA);
	  double Ls;

	  //solving photosynthesis and stomatacondctance model for each layer
	  double lastcil, lastci, newcil, cil, ci; //Stomata CO2 pressure 
	  double aiter, iter; 
	  const double maxiter = 150;
	  //int sunconfirmatin;

	  ci = 0.5*CO2_atm;  //first guess for ci
	  double rsw = 5/LA; //first gues for rsw
	  double gsw = 1/rsw;//first gues for stomatal conductance gsw 

	  iter = 0;
	  lastcil = CO2_atm;
	  newcil = ci;
	     
	  if(iter < maxiter) 
	    {
	      while ((lastcil-newcil)> 0.01)
		{
		  lastci = ci; 
		  iter = iter + 1.0;
		  //Transpiration();
		  
		  double pn = C3Model(dPAR, gb, gsw, T, rsw, ci, msg);
		  ci = GSTModel(vp_ref, pn, LA, gb,T, Ls, msg);
		  lastcil=lastci;
		  newcil = ci;
		  std::ostringstream tmp;
		  tmp <<"ci ="<< ci <<", pn = "<< pn <<", antal iteratione = "<< iter;
		}
	    }
	  else 
	    {
	      std::ostringstream tmp;
	      tmp << "Bug: total iterations in assimilation model exceed "
		  << maxiter;
	      msg.error (tmp.str ());
	    }
	  
      	  // Leaf Photosynthesis [gCO2/m2/h] (De Pury & Farquhar, 1997)
#if 0
	  const double wc = Vm;
	  const double F = Vmax * (1.0 - exp (- (Qeff * dPAR / Vmax)));

	  Ass += LA * F;
#endif
	}
    }
  daisy_assert (approximate (accCAI, canopy.CAI));

  return (molWeightCH2O / molWeightCO2) * Teff * Ass;
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

    syntax.add ("Qeff", "(g CO2/m^2/h)/(W/m^2)", Syntax::Const,
                "Quantum efficiency at low light.");


    syntax.add ("Vm25", "umol/m^2/h", Check::positive (), Syntax::Const,
                "Photosynthetic Rubisco capacity per unit leaf area at 25 degrees. For wheat Vm25 = 110 ( De Pury & Farquhar, 1997). For Barley Vm25 = 105 (Thorgeirsson & Soegaard, 1999).");
    alist.add ("Vm25", 110.);

    syntax.add ("Jm25", "umol/m^2/h", Check::positive (), Syntax::Const,
                "Potential rate of electron transport per unit leaf area at 25 degrees. Jm25 = 2.1*Vm25");
    alist.add ("Jm25", 2.1*110);

    syntax.add ("Kc25", "Pa", Check::positive (), Syntax::Const,
                "Micahaelis-Menten constant of Rubisco for CO2. Kc25 = 40.4 Pa for wheat (Collatz et al., ) ");

    alist.add ("Kc25", 40.4);

    syntax.add ("Ko25", "Pa", Check::positive (), Syntax::Const,
                "Micahaelis-Menten constant of Rubisco for O2 at 25 degrees. Ko25 = 24800 Pa for wheat (Collatz et al., )");
    alist.add ("Ko25", 24800.);

    syntax.add ("Gamma25", "Pa", Check::positive (), Syntax::Const,
                "CO2 compensation point of photosynthesis. Gamma25 = 3.69 Pa for wheat? (Collatz et al., )");
    alist.add ("Gamma25", 3.69);

    syntax.add ("rd25", "J/mol", Check::positive (), Syntax::Const,
                "Leaf respiration at 25 degrees. rd25 = 0.0089*Vm25, De Pury & Farquhar, 1997)");
    alist.add ("rd25", 0.0089*110.);
    
    syntax.add ("S", "J/mol/K", Check::positive (), Syntax::Const,
                "Electron transport temperature response parameter,(De Pury & Farquhar, 1997)");
    alist.add ("S", 710.);
    
    syntax.add ("H", "J/mol", Check::positive (), Syntax::Const,
                "Curvature parameter of Jm, (De Pury & Farquhar, 1997)");
    alist.add ("H", 220000.);
    
    syntax.add ("c_Vm", "J/mol", Check::positive (), Syntax::Const,
                "Temperature scaling constant for Vmax. c_Vm, = 26350 J/mol (Bernacchi et al., 2001)");
    alist.add ("c_Vm", 26350.);
    
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
    alist.add ("Ptot", 100000.0);

    syntax.add ("m", " ", Check::positive (), Syntax::Const,
                "Stomatal slope factor. Collatz et al., 1991: m = 9 for  Wang and Leuning: m = 11 for wheat");
    alist.add ("m", 11.0);

    syntax.add ("b", "mol/m2/s", Check::positive (), Syntax::Const,
                "Stomatal intercept factor, Ball and Berry model, (0.01 mol/m2/s)");
    alist.add ("b", 0.01);

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

    Librarian<Photo>::add_type ("Farquhar", alist, syntax, &make);
  }

} PhotoFarquhar_syntax;
