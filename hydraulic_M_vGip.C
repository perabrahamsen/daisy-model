// hydraulic_M_vGip.C
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
// Modified van Genuchten retention curve model with Mualem theory.
// Ippisch et al. 2006 AWR

#define BUILD_DLL

#include "hydraulic.h"
#include "block_model.h"
#include "plf.h"
#include "mathlib.h"
#include "librarian.h"
#include "frame.h"

class HydraulicM_vGip : public Hydraulic
{
	// Content.
	const double alpha;
	const double a;		// - alpha
	const double n;
	const double m;		// 1 - 1/n
	const double l;		// tortuosity parameter   
	const double he;		// air entry pressure head
	mutable PLF M_;

	// Use.
public:
	double Theta(double h) const;
	double K(double h) const;
	double Cw2(double h) const;
	double h(double Theta) const;
	double M(double h) const;
private:
	double Se(double h) const;
	double Se_star(double h) const;

	// Create and Destroy.
public:
	HydraulicM_vGip(const BlockModel&);
	~HydraulicM_vGip();
};

double
HydraulicM_vGip::Theta(const double h) const
{
	return Se_star(h) * (Theta_sat - Theta_res) + Theta_res;
}


double
HydraulicM_vGip::Se_star(const double h) const
{
	if (h < he)
	{
		return (1 / Se(he))*Se(h);
	}
	else
		return 1.0;
}




double
HydraulicM_vGip::K(const double h) const
{
	if (h < he)
	{
		const double Se_he = Se(he);
		const double Se_h = Se_star(h);
		// return K_sat * pow (Se_h, l)
		//* pow (1.0 - pow (1.0 - pow (Se_h, 1.0/m), m), 2.0);

		return K_sat * pow(Se_h, l)
			* pow(1.0 - pow(1.0 - pow(Se_h*Se_he, 1.0 / m), m), 2.0) / pow(1.0 - pow(1.0 - pow(Se_he, 1.0 / m), m), 2.0);



	}
	else
		return K_sat;
}

double
HydraulicM_vGip::Cw2(const double h) const
{
	if (h < 0.0)
		return -(1 / Se(he))*((Theta_sat - Theta_res)
			* (m * (pow(1.0 / (1.0 + pow(a * h, n)), m - 1.0)
				* (n * (pow(a * h, n - 1.0) * a))))
			/ pow(1.0 + pow(a * h, n), 2.0));
	else
		return 0.0;
}

double
HydraulicM_vGip::h(const double Theta) const
{
	daisy_assert(Theta_res <= Theta);
	if (Theta < Theta_sat)
		return pow(pow(Theta_res / (Theta_res - Theta_sat)
			+ Theta / (Theta_sat - Theta_res), -1.0 / m)
			- 1.0, 1.0 / n) / a;
	else
		return 0.0;
}

double
HydraulicM_vGip::M(double h) const
{
	if (M_.size() == 0)
		K_to_M(M_, 500);

	return M_(h);
}

double
HydraulicM_vGip::Se(double h) const
{
		const double Se_h = pow(1.0 / (1.0 + pow(a * h, n)), m);
		daisy_assert(Se_h >= 0.0);
		daisy_assert(Se_h <= 1.0);
		return Se_h;
	}

HydraulicM_vGip::HydraulicM_vGip(const BlockModel& al)
	: Hydraulic(al),
	alpha(al.number("alpha")),
	a(-alpha),
	n(al.number("n")),
	m(1 - 1 / n),
	l(al.number("l")),
	he(al.number("he")),
	M_()
{ }

HydraulicM_vGip::~HydraulicM_vGip()
{ }

// Add the HydraulicM_vGip syntax to the syntax table.

static struct HydraulicM_vGipSyntax : public DeclareModel
{
	Model* make(const BlockModel& al) const
	{
		return new HydraulicM_vGip(al);
	}


	HydraulicM_vGipSyntax()
		: DeclareModel(Hydraulic::component, "M_vGip",
			"Modified van Genuchten retention curve model with Mualem theory.")
	{ }
	void load_frame(Frame& frame) const
	{
		Hydraulic::load_Theta_res(frame);
		Hydraulic::load_K_sat(frame);
		frame.declare("alpha", "cm^-1", Attribute::Const,
			"van Genuchten alpha.");
		frame.declare("n", Attribute::None(), Attribute::Const,
			"van Genuchten n.");
		frame.declare("l", Attribute::None(), Attribute::Const,
			"tortuosity parameter.");
		frame.set("l", 0.5);
		frame.declare("he", "cm", Attribute::Const,
			"Bubbling pressure.");
		frame.set("he", -2.0);
	}
} hydraulicM_vGip_syntax;
