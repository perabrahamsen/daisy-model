// rootdens_G_P.C -- Gerwitz and Page model for calculating root density.
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
#include "check.h"
#include "message.h"

class Rootdens_G_P : public Rootdens
{
  // Parameters.
private: 
  const double SpRtLength;	// Specific root length [m/g]
  const double DensRtTip;	// Root density at (pot) pen. depth [cm/cm^3]
  const double MinDens;		// Minimal root density [cm/cm^3]

  // Simulation.
public:
  void set_density (vector<double>& Density,
		    const Geometry& geometry, 
		    const double Depth, const double PotRtDpt,
		    const double WRoot);

  // Create.
public:
  Rootdens_G_P (const AttributeList&);
};

void
Rootdens_G_P::set_density (vector<double>& Density,
			   const Geometry& geometry, 
			   const double Depth, const double PotRtDpt,
			   const double WRoot)
{
  // Dimensional conversion.
  static const double m_per_cm = 0.01;

  const double MinLengthPrArea = (DensRtTip * 1.2) * PotRtDpt;
  const double LengthPrArea
    = max (m_per_cm * SpRtLength * WRoot, MinLengthPrArea); // [cm/cm^2]
  double a = density_distribution_parameter (LengthPrArea / 
					     (PotRtDpt * DensRtTip));
  double L0 = DensRtTip * exp (a);
  a /= PotRtDpt;
  if (Depth < PotRtDpt)
    {
      double Lz = L0 * exp (-a * Depth);
      a = density_distribution_parameter (LengthPrArea / (Depth * Lz)) / Depth;
    }

  // Check minimum density
  double extra = 0.0;
  if (MinDens > 0.0 && WRoot > 0.0)
    {
      assert (L0 > 0.0);
      assert (a > 0.0);
      const double too_low = -log (MinDens / L0) / a; // [cm]

      if (too_low < Depth)
	{
	  // We don't have MinDens all the way down.
	  const double NewLengthPrArea 
	    =  LengthPrArea - MinDens * Depth; // [cm/cm^2]
#if 1
	  CERR << "too_low = " << too_low 
	       << ", NewLengthPrArea = " << NewLengthPrArea
	       << "MinLengthPrArea = " << MinLengthPrArea << "\n";
#endif	    
	  if (too_low > 0.0 && NewLengthPrArea > too_low * DensRtTip * 1.2)
	    {
	      // There is enough to have MinDens all the way, spend
	      // the rest using the standard model until the point
	      // where the standard model would give too little..
	      a = density_distribution_parameter (NewLengthPrArea
						  / (too_low * DensRtTip));
	      L0 = DensRtTip * exp (a);
	      a /= too_low;
	      extra = MinDens;
	    }
	  else
	    {
	      // There is too little, use uniform density all the way.
	      L0 = 0.0;
	      extra = LengthPrArea / Depth;
	    }
	}
    }

  unsigned int i = 0;
  for (; i == 0 || -geometry.zplus (i-1) < Depth; i++)
    Density[i] = extra + L0 * exp (a * geometry.z (i));

  assert (i < geometry.size ());
  for (; i < geometry.size (); i++)
    Density[i] = 0.0;

}

Rootdens_G_P::Rootdens_G_P (const AttributeList& al)
  : Rootdens (al),
    SpRtLength (al.number ("SpRtLength")),
    DensRtTip (al.number ("DensRtTip")),
    MinDens (al.number ("MinDens"))
{ }

const AttributeList& 
Rootdens::default_model ()
{
  static AttributeList alist;
  
  if (!alist.check ("type"))
    {
      alist.add ("type", "Gerwitz+Page74");
      alist.add ("description", 
		 "Use exponential function for root density.\n\
\n\
See Gerwitz, S. and E.R. Page (1974): An empirical mathematical model\n\
to describe plant root systems.  J. Appl. Ecol. 11, 773-781.");
      alist.add ("SpRtLength", 100.0);
      alist.add ("DensRtTip", 0.1);
      alist.add ("MinDens", 0.0);
    }
  return alist;
}

static struct Rootdens_G_PSyntax
{
  static Rootdens&
  make (const AttributeList& al)
  { return *new Rootdens_G_P (al); }
  Rootdens_G_PSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList (Rootdens::default_model ());
    syntax.add ("SpRtLength", "m/g", Check::positive (), Syntax::Const,
		"Specific root length");
    syntax.add ("DensRtTip", "cm/cm^3", Check::positive (), Syntax::Const,
		"Root density at (potential) penetration depth.");
    syntax.add ("MinDens", "cm/cm^3", Check::non_negative (), Syntax::Const,
		"Minimal root density\n\
Root density will never be below this, as long as there is enough root mass.\n\
Extra root mass will be distributed according to Gerwitz and Page.\n\
If there are too little root mass, the root will have the same density\n\
all the way down.");
    Librarian<Rootdens>::add_type (alist.name ("type"), alist, syntax, &make);
  }
} Rootdens_G_P_syntax;
