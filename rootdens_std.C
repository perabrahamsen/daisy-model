// rootdens_std.C -- Default model for calculating root density.
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


#include "rootdens.h"
#include "geometry.h"

class RootdensStandard : public Rootdens
{
  // Parameters.
private: 
  double SpRtLength;		// Specific root length [m/g]
  double DensRtTip;		// Root density at (pot) penetration depth

  // Simulation.
public:
  void set_density (vector<double>& Density,
		    const Geometry& geometry, 
		    const double Depth, const double PotRtDpt,
		    const double WRoot);

  // Create.
public:
  RootdensStandard (const AttributeList&);
};

void
RootdensStandard::set_density (vector<double>& Density,
			       const Geometry& geometry, 
			       const double Depth, const double PotRtDpt,
			       const double WRoot)
{
  // Dimensional conversion.
  static const double m_per_cm = 0.01;

  const double LengthPrArea
    = max (m_per_cm * SpRtLength * WRoot, 0.12 * PotRtDpt); /*cm/cm2*/
  double a = density_distribution_parameter (LengthPrArea / 
					     (PotRtDpt * DensRtTip));
  double L0 = DensRtTip * exp (a);
  a /= PotRtDpt;
  if (Depth < PotRtDpt)
    {
      double Lz = L0 * exp (-a * Depth);
      a = density_distribution_parameter (LengthPrArea / (Depth * Lz)) / Depth;
    }

  unsigned int i = 0;
  for (; i == 0 || -geometry.zplus (i-1) < Depth; i++)
    Density[i] = L0 * exp (a * geometry.z (i));
  assert (i < geometry.size ());
  for (; i < geometry.size (); i++)
    Density[i] = 0.0;
}

RootdensStandard::RootdensStandard (const AttributeList& al)
  : Rootdens (al),
    SpRtLength (al.number ("SpRtLength")),
    DensRtTip (al.number ("DensRtTip"))
{ }

static struct RootdensStandardSyntax
{
  static Rootdens&
  make (const AttributeList& al)
  { return *new RootdensStandard (al); }
  RootdensStandardSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList (Rootdens::default_model ());
    syntax.add ("SpRtLength", "m/g", Syntax::Const,
		"Specific root length");
    syntax.add ("DensRtTip", "cm/cm^3", Syntax::Const,
		"Root density at (potential) penetration depth.");

    Librarian<Rootdens>::add_type ("default", alist, syntax, &make);
  }
} RootdensStandard_syntax;
