// hydraulic_M_vGBS.C
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
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
//
// Brunschwick model for the Van Genuchten-Mualem version
// Weber et al., 2019 WRR and Streck and Weber, 2020 VZJ
// The analytical solutions needs the calculation of incomplete beta function 
// for complex numbers, and this is based on complex_functions.H and hyp_2F1.H

#define BUILD_DLL

#include "hydraulic.h"
#include "block_model.h"
#include "plf.h"
#include "mathlib.h"
#include "librarian.h"
#include "frame.h"
#include <iostream>
#include <fstream>
#include <complex>
#include "hyp_2F1.h"



//#define SIGN(a) (((a) < 0) ? (-1) : (1))
//#define min(a,b)            (((a) < (b)) ? (a) : (b))
//#define max(a,b)            (((a) > (b)) ? (a) : (b))

#ifndef M_LN10
const double M_LN10 = 2.302585092994046;
#endif

class HydraulicM_vGBS : public Hydraulic
{
	// Content.
	const double alpha;
	const double a;		// - alpha
	const double n;
	const double m;		// 1 - 1/n
	const double l;		// tortuosity parameter   
	const double pf0;
	const double afilm;
	const double Theta_cap;
	const double Theta_nc;
	const double Ks_cap;
	const double Ks_nc;
	const double Gama_h_cap0 = pow(1.0 / (1.0 + pow(alpha * pow(10.0,pf0), n)), m);
	double sumk;
	double Gama_h_nc0;
	mutable PLF M_;
	PLF pF_Theta;

	// Use.
public:
	double Theta(double h) const;
	double K(double h) const;
	double Cw2(double h) const;
	double h(double Theta) const;
	double M(double h) const;
private:
	double Se(double h) const;
	double Se_h_cap(double h) const;
	double Se_h_nc(double h) const;
	double Beta_inc(const double x, const double barg1) const;

	// Create and Destroy.
public:
	HydraulicM_vGBS(const BlockModel&);
	~HydraulicM_vGBS();
};


double
HydraulicM_vGBS::Beta_inc(const double x, const double barg1) const
{
	return pow(x, barg1)/barg1*real(hyp_2F1(barg1, 1.0, barg1+1.0, x));
}



double
HydraulicM_vGBS::Theta(const double h) const
{
	return Theta_cap*Se_h_cap(h)+Theta_nc*Se_h_nc(h);
}


double
HydraulicM_vGBS::K(const double h) const
{
	if (h < 0.0)
	{
		const double Gama_h_cap = pow(1.0 / (1.0 + pow(alpha * pow(10.0, log10(abs(h))), n)), m);
		
		return Ks_cap*pow(Se_h_cap(h),l)*pow(1.0-pow((1.0-pow(Gama_h_cap,1.0/m))/(1.0- pow(Gama_h_cap0, 1.0 / m)),m),2.0)+
			Ks_nc*pow(pow(10.0,pf0),-afilm*(1.0- Se_h_nc(h)));
		   


	}
	else
		return Ks_cap+Ks_nc;
}

double
HydraulicM_vGBS::Cw2(const double h) const
{
	const double Gama_h_cap = pow(1.0 / (1.0 + pow(alpha * pow(10.0, log10(abs(h))), n)), m);

	if (h < 0.0)
		return Theta_cap * (n - 1.0)*h*-pow(alpha, n)*pow(abs(h), n - 2.0)*pow(pow(alpha, n)*pow(abs(h), n) + 1.0, 1.0 / n - 2.0)
		+ Theta_nc * (-1.0 / Gama_h_nc0)*(Gama_h_cap - 1.0) / h / M_LN10;

	else
		return 0.0;
}

double
HydraulicM_vGBS::h(const double Theta) const
{
	if (Theta < Theta_cap+Theta_nc)
		
	     return pF2h(pF_Theta(Theta));
	else
		return 0.0;
}

double
HydraulicM_vGBS::M(double h) const
{
	if (M_.size() == 0)
		K_to_M(M_, 500);

	return M_(h);
}

double
HydraulicM_vGBS::Se_h_cap(double h) const
{
	const double Gama_h_cap = pow(1.0 / (1.0 + pow(alpha * pow(10.0, log10(abs(h))), n)), m);
	if (h < 0.0)
		return (Gama_h_cap - Gama_h_cap0) / (1.0 - Gama_h_cap0);
	    
	else
		return 1.0;
}


double
HydraulicM_vGBS::Se_h_nc(double h) const
{
	const double Gama_h_nc = (1.0 / n / M_LN10)*(Beta_inc(1.0 + pow(alpha*pow(10.0, log10(abs(h))), n), 1.0) - Beta_inc(1.0 + pow(alpha*pow(10.0, log10(abs(h))), n), 1.0 / n) + n - 1.0 / n + (1.0 / n - 1.0)*sumk);
	if(h<0.0)
	return 1.0-Gama_h_nc / Gama_h_nc0;
	else
	return 1.0;
}



double
HydraulicM_vGBS::Se(double h) const
{
	 
	const double Se_h = (Se_h_cap(h)*Theta_cap + Se_h_nc(h) * Theta_nc) / (Theta_cap + Theta_nc);

	 daisy_assert(Se_h >= 0.0);
	 daisy_assert(Se_h <= 1.0);
	return Se_h;
	}

HydraulicM_vGBS::HydraulicM_vGBS(const BlockModel& al)
	: Hydraulic(al),
	alpha(al.number("alpha")),
	a(-alpha),
	n(al.number("n")),
	m(1.0 - 1.0 / n),
	l(al.number("l")),
	pf0(al.number("pf0")),
	afilm(al.number("afilm")),
	Ks_nc(al.number("Ks_nc")),
	Theta_nc(al.number("Theta_nc")),
	Theta_cap(al.number("Theta_cap")),
	Ks_cap(al.number("Ks_cap")),
	M_()
{
	//sumk = [this] {double res = 0; for (int k = 0.0; k < 19.5; k++) res += 1.0 / k / (k + 1.0) / (n*k + 1.0); return res; }();
	
	sumk = 0.0;
	for (double k = 1.0; k < 20.5; k++)
	{
		sumk = sumk + 1.0 / k / (k + 1.0) / (n*k + 1.0);
	}
	Gama_h_nc0 = (1.0 / n / M_LN10)*(Beta_inc(1.0 + pow(alpha*pow(10.0, pf0), n), 1.0) - Beta_inc(1.0 + pow(alpha*pow(10.0, pf0), n), 1.0 / n) + n - 1.0 / n + (1.0 / n - 1.0)*sumk);
	Theta_sat = Theta_cap + Theta_nc;
	const double pf_min = -6.0;
	const double pf_max = pf0;
	const double dpf = 0.05;
	const int nsteps = (abs(pf_min)+pf_max)/dpf;
	for (int i = nsteps; i>=0; --i)	
	{
		const double x = pf_min+ dpf * i;
		const double y = Theta(-pow(10.0, x));
		pF_Theta.add(y,x);	// code block to be executed
	}
}  //initiate the table

HydraulicM_vGBS::~HydraulicM_vGBS()
{ }

// Add the HydraulicM_vGBS syntax to the syntax table.

static struct HydraulicM_vGBSSyntax : public DeclareModel
{
	Model* make(const BlockModel& al) const
	{
		return new HydraulicM_vGBS(al);
	}


	HydraulicM_vGBSSyntax()
		: DeclareModel(Hydraulic::component, "M_vGBS",
			"Brunschwick version of van Genuchten retention curve model with Mualem and Tokunaga theory.\n\
             \n\
			 The model is described in Weber et al. (2019). In Daisy the analytical solution of Weber and Streck (2020) \n\
             has been implemented for the Van Genuchten (1980) model. The h(theta) function is calculated with lookup tables")
	{ }
	void load_frame(Frame& frame) const
	{
		frame.declare("Theta_cap", Attribute::None(), Attribute::Const,
			"saturated water content of the capillary part");
		frame.declare("Theta_nc", Attribute::None(), Attribute::Const,
			"saturated water content of the non-capillary part");
		frame.declare("alpha", "cm^-1", Attribute::Const,
			"van Genuchten alpha.");
		frame.declare("n", Attribute::None(), Attribute::Const,
			"van Genuchten n.");
		frame.declare("l", Attribute::None(), Attribute::Const,
			"tortuosity parameter.");
		frame.set("l", 0.5);
		frame.declare("pf0", Attribute::None(), Attribute::Const,
			"pf at oven dryness");
		frame.set("pf0", 6.8);
		frame.declare("Ks_cap", "cm/h", Attribute::Const,
			"saturated conductivity of the capillary part");
		frame.declare("Ks_nc", "cm/h", Attribute::Const,
			"saturated conductivity of the non-capillary part");
		frame.set_cited("Ks_nc", pow(10.0, -1.72) / 24.0, "Table 3", "weber2020modular");
		frame.declare("afilm", Attribute::None(), Attribute::Const,
			"slope of loglog film conductivity");
		frame.set_cited("afilm", 1.5,"", "weber2019modular");
	}
} hydraulicM_vGBS_syntax;
