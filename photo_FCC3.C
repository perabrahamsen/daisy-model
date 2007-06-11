// photo_FCC3.C -- Leaf photosynthesis for C3 crops based on Farquhar et al., 1980 and Ball et al. 1987.
// 
// Copyright 1996-2001,2005 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001,2005-2006 KVL.
// Copyright 2006,2007 Birgitte Gjettermann.
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

#include "photo_Farquhar.h"
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

class PhotoFCC3 : public PhotoFarquhar
{
  // Parameters.
private:
  const PLF& TempEff;	// Temperature effect, photosynthesis   
  const double S;     // Electron transport temperature response parametre 
  const double H;     // Curvature parameter of Jm
  const double Ko25;  // Michaelis-Menten constant of Rubisco for O2 at 25 degrees
  const double Kc25;  // Michaelis-Menten constant of Rubisco for CO2 at 25 degrees
  const double c_Vm;  // Temperature scaling constant for Vm
  const double Ea_Vm; // Activation energy for Vm, (65330 J/mol)
  const double Eda_Vm;// Deactivation energy for Vm, (202900 J/mol)
  const double Ea_Jm; // Activation energy for Jm, (37000 J/mol)
  const double Ea_ko; // Activation energy for ko, (36000 J/mol)
  const double Ea_kc; // Activation energy for kc, (59400 J/mol)
  const double Ea_rd; // Activation energy for rd, (66400 J/mol)
  const double Sv;    // Entropy term
  const double theta; // Curvature of leaf response of electron transport to irradiance
  const double beta;  // Curvanture
  const double alfa;  // Quantum efficiency. Fraction of PAR effec. absorbed by PSII

  // Simulation.
public:
  double V_m (const double Vm_25m, double T) const;
  double J_m (const double vmax25, const double T) const;
  void CxModel (double& pn, double& ci, const double Q, const double gsw, 
		  const double T, const double vmax, const double rd, Treelog& msg)const;
  double respiration_rate(const double Vm_25, const double Tl) const;

  // Create and Destroy.
public:
  PhotoFCC3 (Block& al)
    : PhotoFarquhar (al),
      TempEff (al.plf ("TempEff")),
      S (al.number ("S")),
      H (al.number ("H")),
      Ko25 (al.number ("Ko25")),
      Kc25 (al.number ("Kc25")),
      c_Vm (al.number ("c_Vm")),
      Ea_Vm (al.number ("Ea_Vm")),
      Eda_Vm (al.number ("Eda_Vm")),
      Ea_Jm (al.number ("Ea_Jm")),
      Ea_ko (al.number ("Ea_ko")),
      Ea_kc (al.number ("Ea_kc")),
      Ea_rd (al.number ("Ea_rd")),
      Sv (al.number ("Sv")),
      theta (al.number("theta")),
      beta (al.number("beta")),
      alfa (al.number("alfa"))
  { }
  ~PhotoFCC3 ()
  { }
};

// Temperature response function for Vmax according to Bernacchi et al., 2001.
double
PhotoFCC3::V_m (const double vm_25, const double T) const
{
  const double R = 8.314; //Gas constant, J/(mol K) 
  const double a_ = exp(c_Vm-Ea_Vm/(R*(T+273.15))); 
  const double c_ = 1.+(exp((Sv *(T+273.15)-Eda_Vm)/(R*(T+273.15))));
  return vm_25 * a_/c_;  //mol/m2/s
}

// Temperature response function for Jm according to De Pury & Farquhar (1997).
double
PhotoFCC3::J_m (const double vmax25, const double T/*[degree C]*/) const
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

void 
PhotoFCC3::CxModel (double& pn, double& ci, 
		    const double PAR /*[mol/m²/s]*/, 
		    const double gsw /*[mol/m²/s]*/, const double T, 
		    const double vmax25 /*[mol/m² leaf/s]*/, 
		    const double rd /*[mol/m² leaf/s]*/, Treelog& msg) const  
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
}

double
PhotoFCC3::respiration_rate (const double Vm_25, const double Tl) const 
{
  // leaf respiration
  const double rd_25 = 0.0089 * Vm_25;// [mol/m² leaf/s]
  const double rd = Arrhenius(rd_25, Ea_rd, Tl); 
  return rd;
}

static struct Photo_FCC3Syntax
{
  static Model& make (Block& al)
  { return *new PhotoFCC3 (al); }
  Photo_FCC3Syntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    PhotoFarquhar::load_syntax(syntax, alist);


    alist.add ("description", "Photosynthesis for C3 crops described by Faquhar et al. (1980).");

    syntax.add ("TempEff","dg C", Syntax::None (), Check::non_negative (),Syntax::Const,
		"Temperature factor for assimilate production.");


    syntax.add ("Kc25", "Pa", Check::positive (), Syntax::Const,
                "Micahyaelis-Menten constant of Rubisco for CO2. Kc25 = 40.4 Pa for wheat (Collatz et al.,1991) ");

    alist.add ("Kc25", 40.4);

    syntax.add ("Ko25", "Pa", Check::positive (), Syntax::Const,
                "Micahaelis-Menten constant of Rubisco for O2 at 25 degrees. Ko25 = 24800 Pa for wheat (Collatz et al., 1991)");
    alist.add ("Ko25", 24800.);

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

    syntax.add ("Sv", "J/mol/K", Check::positive (), Syntax::Const,
                "Entropy term. Sv = 650 J/mol/K");
    alist.add ("Sv", 650.);
  
    syntax.add ("theta", "", Check::positive (), Syntax::Const,
                "Curvature of leaf response of electron transport to irradiance, (De Pury & Farquhar, 1997");
    alist.add ("theta", 0.7);
    
    syntax.add ("beta", " ", Check::positive (), Syntax::Const,
                "Curvanture");
    alist.add ("beta", 1.0);

    syntax.add ("alfa", "mol/mol", Check::positive (), Syntax::Const,
                "Fraction of PAR effectively absorbed by PSII, ");
    alist.add ("alfa", 0.08);

    alist.add ("m", 11.0);
    alist.add ("b", 0.01);

    Librarian::add_type (Photo::component, "FC_C3", alist, syntax, &make);
  }

} PhotoFCC3_syntax;
