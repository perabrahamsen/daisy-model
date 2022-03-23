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

#define BUILD_DLL
#include "photo_Farquhar.h"
#include "block_model.h"
#include "rubiscoNdist.h"
#include "bioclimate.h"
#include "canopy_std.h"
#include "phenology.h"
#include "log.h"
#include "plf.h"
#include "frame.h"
#include "mathlib.h"
#include <sstream>
#include "check.h"
#include "librarian.h"

class PhotoFCC3 : public PhotoFarquhar
{
  // Parameters.
private:
  const PLF& TempEff;   // Temperature effect, photosynthesis   
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
  void CxModel (const double CO2_atm, const double O2_atm, const double Ptot, 
                double& pn, double& ci, 
                const double Q, const double gsw, const double gbw,
                const double T,
                const double vmax, const double rd, Treelog& msg)const;
  double respiration_rate(const double Vm_25, const double Tl) const;

  // Create and Destroy.
public:
  PhotoFCC3 (const BlockModel& al)
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
PhotoFCC3::CxModel (const double CO2_atm, 
                    const double O2_atm, const double Ptot, 
                    double& pn, double& ci, 
                    const double PAR /*[mol/m²leaf/s]*/, 
                    const double gsw /*[mol/m²leaf/s]*/, 
                    const double gbw /*[mol/m²leaf/s]*/, 
                    const double T, 
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
  const double rbw = 1./gbw; // leaf boundary resistance to water vapor, [m²leaf*s/mol]
  const double rsw = 1./gsw; // stomatal resistance to water [m²leaf*s/mol]

  //Total conductance of CO2
  const double gtc = 1./(1.4*rbw+1.6*rsw);   //[mol/m² leaf/s]
  
  const double Ile = PAR * alfa; // PAR effectively absorbed by PSII [mol/m² leaf/s]
  const double J 
    = first_root_of_square_equation(theta, -(Ile+Jm), Ile*Jm); // [mol/m² leaf/s]

  // We now have two equations with two unknowns.
  //   p = find_p (ci)
  //   ci = find_ci (p)
  // By substitution, we get
  //   find_ci (find_p (ci)) - ci = 0
  // This can be solved with Newton-Raphson.

  struct Solve_ci
  {
    const double Gamma, Vm, Jm, Ko, Kc, Kcl;
    const double gtc, rbw, rsw,Ile, beta, J, rd, CO2_atm, Ptot;
    
    double find_wc (const double ci) // Eq 2
    {// Gross CO2 uptake limited by Rubisco
      daisy_assert ((ci + Kcl) > 0.0); 
      const double wc = Vm * ( ci - Gamma)/( ci + Kcl);//Rubisco limited,[mol/m²leaf/s]
     return wc;
    }
    double find_we (const double ci) // Eq 4
    { // Gross CO2 uptake limited by RuBP
      daisy_assert ((ci + 2.0 * Gamma) > 0.0);
      const double we		//[mol/m² leaf/s]
	= J * (ci - Gamma)/(4.0*(ci + 2. * Gamma));
      return we; 
    }
    double find_p (const double ci) //[mol/m² leaf/s]
    {
       // Gross CO2 uptake limited by Rubisco and RuBP.
      daisy_assert (Gamma >= 0.0);
      daisy_assert (ci >= 0.0);
      const double we = find_we(ci); //[mol/m² leaf/s]
      const double wc = find_wc(ci); //[mol/m² leaf/s]
      daisy_assert(((wc+we)*(we+wc)-(4.*beta*we*wc)) >= 0.0);
      daisy_assert ((beta)> 0.0);
      const double p 
	= first_root_of_square_equation(beta, -(wc+we), we*wc);//[mol/m² leaf/s]
      return p;
    }

    double find_p_derived (const double ci) // [mol/m² leaf/s]
    { 
      const double we = find_we(ci); //[mol/m² leaf/s]
      const double wc = find_wc(ci); //[mol/m² leaf/s]
      // The derivative of p with regard to ci; p'(ci).
      double dp;
      if (wc < we) 
        dp = Vm * (Kcl+Gamma)/((ci+Kcl)*(ci+Kcl));//[mol/m² leaf/s/Pa]
      else 
        dp = 3.0 * J *(Gamma/((ci+2. * Gamma)*(ci+2.* Gamma))); //[mol/m² leaf/s/Pa]
      return dp;
    }
    
    double find_ci (const double p) // [Pa]
    { 
      // Net CO2 uptake
      const double pn = p - rd; // [mol/m² leaf/s] 
      return CO2_atm - (pn/gtc) * Ptot; 
    }
    double find_ci_derived ()
    { return - Ptot / gtc; }
    double function (const double ci)
    { return find_ci (find_p (ci)) - ci; }
    double derived (const double ci)
    { return find_ci_derived () * find_p_derived (ci)  - 1.0; }

    double next_value (const double prev_value)
    { return prev_value - function (prev_value) / derived (prev_value); }

    double solve (const double initial_guess)
    {
      //Newton Raphson's solution.
      const int maxiter = 150;
      int iter = 0;
      double new_guess = initial_guess;
      double last_guess;
      do
	{
	  if (iter > maxiter)
	    throw "Too many iterations in Newton-Raphson";
	  iter++;
	  last_guess = new_guess;
	  //std::ostringstream tmp;
	  //tmp << "Value of new_guess = " << new_guess << " iter = " << iter ;
	  //Assertion::message (tmp.str ());
	  new_guess = next_value (last_guess);
	  if (!(new_guess > 0.)) 
	    new_guess = 0.5 * CO2_atm;
	}
      while (std::abs(new_guess - last_guess) > 0.01);

      return new_guess;
    }

    Solve_ci (const double Gamma_, const double Vm_, const double Jm_, 
	      const double Ko_, const double Kc_, const double Kcl_, 
	      const double gtc_, const double rbw_, const double rsw_, 
	      const double Ile_, const double beta_, const double J_, 
	      const double rd_, const double CO2_atm_, const double Ptot_)
      : Gamma (Gamma_),
	Vm (Vm_),
	Jm (Jm_),
	Ko (Ko_),
	Kc (Kc_),
	Kcl (Kcl_),
	//gbw (gbw_),
	//gsw (gsw_),
	gtc (gtc_),
	rbw (rbw_),
        rsw (rsw_),
	Ile (Ile_),
	beta (beta_),
	J (J_),
	rd (rd_),
	CO2_atm (CO2_atm_),
	Ptot (Ptot_)
    { }
  };

  Solve_ci solve_ci (Gamma, Vm, Jm, Ko, Kc, Kcl, gtc, rbw, rsw, Ile, beta, J, 
		     rd, CO2_atm, Ptot);

  if (!std::isnormal (ci)) 
    ci =0.5 * CO2_atm;
      
  daisy_assert (ci >= 0.0);
  ci = solve_ci.solve (ci);
  
  pn = solve_ci.find_p(ci)-rd; 
}

double
PhotoFCC3::respiration_rate (const double Vm_25, const double Tl) const 
{
  // leaf respiration
  const double rd_25 = 0.0089 * Vm_25;// [mol/m² leaf/s]
  const double rd = Arrhenius(rd_25, Ea_rd, Tl); 
  return rd;
}

static struct Photo_FCC3Syntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new PhotoFCC3 (al); }
  Photo_FCC3Syntax () 
    : DeclareModel (Photo::component, "FC_C3", "Farquhar", "\
Photosynthesis for C3 crops described by Faquhar et al. (1980).")
  { }
  void load_frame (Frame& frame) const
  {
    frame.set_strings ("cite", "pf1997simple");
    frame.declare ("TempEff", "dg C", Attribute::None (),
               Check::non_negative (), Attribute::Const,
                "Temperature factor for assimilate production.");

    frame.declare ("Kc25", "Pa", Check::positive (), Attribute::Const,
                "Micahyaelis-Menten constant of Rubisco for CO2. Kc25 = 40.4 Pa for wheat (Collatz et al.,1991) ");

    frame.set ("Kc25", 40.4);

    frame.declare ("Ko25", "Pa", Check::positive (), Attribute::Const,
                "Micahaelis-Menten constant of Rubisco for O2 at 25 degrees. Ko25 = 24800 Pa for wheat (Collatz et al., 1991)");
    frame.set ("Ko25", 24800.);

    frame.declare ("S", "J/mol/K", Check::positive (), Attribute::Const,
                "Electron transport temperature response parameter,(De Pury & Farquhar, 1997)");
    frame.set ("S", 710.);
    
    frame.declare ("H", "J/mol", Check::positive (), Attribute::Const,
                "Curvature parameter of Jm, (De Pury & Farquhar, 1997)");
    frame.set ("H", 220000.);
    
    frame.declare ("c_Vm", Attribute::Unknown (), Check::positive (), Attribute::Const,
                "Temperature scaling constant for Vmax. c_Vm, = 26.35 (Bernacchi et al., 2001)");
    frame.set ("c_Vm", 26.35);
    
    frame.declare ("Ea_Vm", "J/mol", Check::positive (), Attribute::Const,
                "Activation energy for Vmax. Ea_Vm = 65330 J/mol (Ball, 1988)");
    frame.set ("Ea_Vm", 65330.);

    frame.declare ("Eda_Vm", "J/mol", Check::positive (), Attribute::Const,
                "Deactimation energy for Vmax. Eda_Vm = 202900 J/mol");
    frame.set ("Eda_Vm", 202900.);
   
    frame.declare ("Ea_Jm", "J/mol", Check::positive (), Attribute::Const,
                "Actimation energy for Jm. Ea_Jm = 37000 J/mol (Farquhar et al., 1980).");
    frame.set ("Ea_Jm", 37000.);

    frame.declare ("Ea_ko", "J/mol", Check::positive (), Attribute::Const,
                "Actimation energy for ko. Ea_ko 0 36000 J/mol (Badger & Collatz, 1977).");
    frame.set ("Ea_ko", 36000.);

    frame.declare ("Ea_kc", "J/mol", Check::positive (), Attribute::Const,
                "Actimation energy for kc. Ea_kc = 59400 J/mol (Badger & Collatz, 1977)");
    frame.set ("Ea_kc", 59400.);

    frame.declare ("Ea_rd", "J/mol", Check::positive (), Attribute::Const,
                "Actimation energy for rd. Ea_rd = 66400 J/mol (Farquhar et al., 1980)");
    frame.set ("Ea_rd", 66400.);

    frame.declare ("Sv", "J/mol/K", Check::positive (), Attribute::Const,
                "Entropy term. Sv = 650 J/mol/K");
    frame.set ("Sv", 650.);
  
    frame.declare ("theta", Attribute::Unknown (), Check::positive (), Attribute::Const,
                "Curvature of leaf response of electron transport to irradiance, (De Pury & Farquhar, 1997");
    frame.set ("theta", 0.7);
    
    frame.declare ("beta", Attribute::Unknown (), Check::positive (), Attribute::Const,
                "Curvature, Collatz et al., 1991");
    frame.set ("beta", 0.95);

    frame.declare ("alfa", "mol/mol", Check::positive (), Attribute::Const,
                "Fraction of PAR effectively absorbed by PSII, ");
    frame.set ("alfa", 0.08);

    // Ball & Berry parameters:
    // frame.set ("m", 11.0);
    // frame.set ("b", 0.01);
  }

} PhotoFCC3_syntax;

// photo_FCC3.C ends here.
