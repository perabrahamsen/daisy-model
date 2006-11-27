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
#include "block.h"
#include "geometry.h"
#include "log.h"
#include "check.h"
#include <sstream>
#include "mathlib.h"

using namespace std;

struct Rootdens_G_P : public Rootdens
{
  // Parameters.
  const double DensRtTip;	// Root density at (pot) pen. depth [cm/cm^3]
  const double MinDens;		// Minimal root density [cm/cm^3]
  // Log variables.
  double a;                     // Form "parameter" [cm^-1].
  double L0;                    // Root density at soil surface [cm/cm^3]

  // Simulation.
  static double density_distribution_parameter (double a);
  void set_density (Treelog&, vector<double>& Density,
		    const Geometry& geo, 
		    double Depth, double PotRtDpt,
		    double WRoot, double DS);
  void output (Log& log) const;

  // Create.
  Rootdens_G_P (Block&);
};

double
Rootdens_G_P::density_distribution_parameter (double a)
{
  daisy_assert (a > 1.0);
  double x, y, z, x1, y1, z1, x2, y2, z2;

  if (1 + a > exp (1.0))
    {
      x1 = 1.0;
      y1 = exp (x1);
      z1 = 1 + a * x1;
      x2 = 20.0;
      y2 = exp (x2);
      z2 = 1 + a * x2;
      while ((z1 - y1) * (z2 - y2) > 0)
	{
	  x1 = x2;
	  y1 = y2;
	  z1 = z2;
	  x2++;
	  y2 = exp (x2);
	  z2 = 1 + a * x2;
	}
    }
  else 
    {
      x1 = 0.3;
      y1 = exp (x1);
 //     z1 = 1 + a * x1;
      x2 = 1.0;
      y2 = exp (x2);
 //     z2 = 1 + a * x2;
    }

  x = (y2 * (x2 - 1) - y1 * (x1 - 1)) / (y2 - y1);
  y = exp (x);
  z = 1 + a * x;
  while (fabs (2 * (z - y) / (z + y)) > 1.0e-5)
    {
      if (z - y > 0)
	{
	  x1 = x;
	  y1 = y;
	  // z1 = z;
	}
      else
	{
	  x2 = x;
	  y2 = y;
	  // z2 = z;
	}
      x = (y2 * (x2 - 1) - y1 * (x1 - 1)) / (y2 - y1);
      y = exp (x);
      z = 1 + a * x;
    }
  return x;
}

void
Rootdens_G_P::set_density (Treelog& msg,
			   vector<double>& Density,
			   const Geometry& geo, 
			   const double Depth, const double PotRtDpt,
			   const double WRoot, const double)
{
  // Dimensional conversion.
  static const double m_per_cm = 0.01;

  const double MinLengthPrArea = (DensRtTip * 1.2) * PotRtDpt;
  const double LengthPrArea
    = max (m_per_cm * SpRtLength * WRoot, MinLengthPrArea); // [cm/cm^2]
  a = density_distribution_parameter (LengthPrArea / 
                                      (PotRtDpt * DensRtTip));
  L0 = DensRtTip * exp (a);
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
      daisy_assert (L0 > 0.0);
      daisy_assert (a > 0.0);
      const double too_low = -log (MinDens / L0) / a; // [cm]

      if (too_low < Depth)
	{
	  // We don't have MinDens all the way down.
	  const double NewLengthPrArea 
	    =  LengthPrArea - MinDens * Depth; // [cm/cm^2]
#if 1
	  Treelog::Open nest (msg, "RootDens G+P");
	  std::ostringstream tmp;
	  tmp << "too_low = " << too_low 
		 << ", NewLengthPrArea = " << NewLengthPrArea
		 << "MinLengthPrArea = " << MinLengthPrArea;
	  msg.warning (tmp.str ());
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

  const size_t size = geo.cell_size ();
  daisy_assert (Density.size () == size);
#if 1
  for (size_t i = 0; i < size; i++)
    {
      const double f = geo.fraction_in_z_interval (i, 0.0, -Depth);
      const double d = -geo.z (i);
      if (f > 0.01)
        Density[i] = f * (extra + L0 * exp (- a * d));
      else
        Density[i] = 0.0;
    }
#else // 0
  unsigned int i = 0;
  for (; i == 0 || -geo.zplus (i-1) < Depth; i++)
    {
      daisy_assert (i < geo.size ());
      Density[i] = extra + L0 * exp (a * geo.z (i));
    }

  for (; i < geo.size (); i++)
    Density[i] = 0.0;
#endif // 0
}

void 
Rootdens_G_P::output (Log& log) const
{
  output_variable (a, log); 
  output_variable (L0, log); 
}

Rootdens_G_P::Rootdens_G_P (Block& al)
  : Rootdens (al),
    DensRtTip (al.number ("DensRtTip")),
    MinDens (al.number ("MinDens")),
    a (-42.42e42),
    L0 (-42.42e42)
{ }

const AttributeList& 
Rootdens::default_model ()
{
  static AttributeList alist;
  
  if (!alist.check ("type"))
    {
      Syntax dummy;
      Rootdens::load_syntax (dummy, alist);
      alist.add ("type", "Gerwitz+Page74");
      alist.add ("DensRtTip", 0.1);
      alist.add ("MinDens", 0.0);
    }
  return alist;
}

static struct Rootdens_G_PSyntax
{
  static Rootdens&
  make (Block& al)
  { return *new Rootdens_G_P (al); }
  Rootdens_G_PSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", 
	       "Use exponential function for root density.\n\
\n\
See Gerwitz, S. and E.R. Page (1974): An empirical mathematical model\n\
to describe plant root systems.  J. Appl. Ecol. 11, 773-781.");

    Rootdens::load_syntax (syntax, alist);
    syntax.add ("DensRtTip", "cm/cm^3", Check::positive (), Syntax::Const,
		"Root density at (potential) penetration depth.");
    alist.add ("DensRtTip", 0.1);
    syntax.add ("MinDens", "cm/cm^3", Check::non_negative (), Syntax::Const,
		"Minimal root density\n\
Root density will never be below this, as long as there is enough root mass.\n\
Extra root mass will be distributed according to Gerwitz and Page.\n\
If there are too little root mass, the root will have the same density\n\
all the way down.");
    alist.add ("MinDens", 0.0);
    syntax.add ("a", "cm^-1", Syntax::LogOnly, "Form parameter.\n\
Calculated from 'DensRtTip'.");
    syntax.add ("L0", "cm/cm^3", Syntax::LogOnly,
                "Root density at soil surface.");
    Librarian<Rootdens>::add_type ("Gerwitz+Page74", alist, syntax, &make);
  }
} Rootdens_G_P_syntax;
