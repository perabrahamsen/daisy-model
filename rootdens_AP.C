// rootdens_AP.C -- Anders Pedersen variant of Gerwitz and Page.
// 
// Copyright 1996-2001, 2004 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001, 2004 KVL.
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

#include "rootdens.h"
#include "block.h"
#include "geometry.h"
#include "log.h"
#include "plf.h"
#include "check.h"
#include "mathlib.h"
#include "librarian.h"
#include <sstream>

struct Rootdens_AP : public Rootdens
{
  // Parameters.
  const PLF a_DS;               // Form "parameter" DS -> [cm^-1]
  const double q;               // Extra root depth [cm]

  // Log variables.
  double a;                     // Form "parameter" [cm^-1]
  double L0;                    // Root density at soil surface [cm/cm^3]

  // simulation.
  void set_density (const Geometry& geo, 
		    double SoilDepth, double CropDepth,
		    const double /* CropWidth [cm] */,
		    double WRoot, double DS,
		    std::vector<double>& Density, 
		    Treelog& );
  void output (Log& log) const;

  // Create.
  void initialize (const Geometry&, double /* row_width */, Treelog&)
  { }
  Rootdens_AP (Block&);
};

void
Rootdens_AP::set_density (const Geometry& geo, 
                          const double SoilDepth, const double CropDepth,
			  const double /* CropWidth [cm] */,
                          const double WRoot, const double DS,
			  std::vector<double>& Density,
                          Treelog&)
{
  const double Depth = std::min (SoilDepth, CropDepth);
  a = a_DS (DS);
  static const double m_per_cm = 0.01;
  const double LengthPrArea = m_per_cm * SpRtLength * WRoot; // [cm/cm^2]
  const double d_m = Depth + q;
  L0 = LengthPrArea 
    / ((1.0 / a) * (1.0 - exp (-a * Depth))
       + ((exp (-a * Depth) / (Depth - d_m))
          * (-0.5 * sqr (d_m) - 0.5 * sqr (Depth) + d_m * Depth)));
  daisy_assert (L0 >= 0.0);

  PLF tip;                      // Linear decrease downto Depth + q;
  tip.add (Depth, L0 * exp (- a * Depth));
  tip.add (d_m, 0.0);

#if 1
  const size_t size = geo.cell_size ();
  for (size_t i = 0; i < size; i++)
    {
      const double d = -geo.z (i);
      const double f_top = geo.fraction_in_z_interval (i, 0.0, -Depth);
      const double f_tip = geo.fraction_in_z_interval (i, -Depth, -d_m);
      Density[i] = L0 * exp (- a * d) * f_top + tip (d) * f_tip;
    }
#else // 0
  daisy_assert (Density.size () == geo.size ());
  unsigned int i = 0;
  // Use GP down to Depth.
  for (; i == 0 || -geo.zplus (i-1) < Depth; i++)
    {
      daisy_assert (i < geo.size ());
      Density[i] = L0 * exp (a * geo.z (i));
    }
  // Linear decrease downto Depth + q;
  for (; i == 0 || -geo.zplus (i-1) < d_m; i++)
    {
      daisy_assert (i < geo.size ());
      // BUG: Should this be "+="? pa 2006-04-20.
      Density[i] += tip (-geo.z (i));
    }
  // No roots below.
  for (; i < geo.size (); i++)
    Density[i] = 0.0;
#endif // 0
}

void 
Rootdens_AP::output (Log& log) const
{
  output_variable (a, log); 
  output_variable (L0, log); 
}

Rootdens_AP::Rootdens_AP (Block& al)
  : Rootdens (al),
    a_DS (al.plf ("a_DS")),
    q (al.number ("q")),
    a (-42.42e42),
    L0 (-42.42e42)
  
{ }

static struct Rootdens_APSyntax
{
  static Model& make (Block& al)
  { return *new Rootdens_AP (al); }
  Rootdens_APSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    Rootdens::load_base (syntax, alist);
    alist.add ("description", 
	       "Use exponential function for root density.\n\
In this variant of Gerwitz and Page, 'a' is specified as a function of\n\
development stage.");
    alist.add_strings ("cite", "gp74");

    syntax.add ("a_DS", "DS", "cm^-1", Syntax::Const, 
                "Form parameter as a function of development stage.");
    syntax.add ("q", "cm", Check::non_negative (), Syntax::Const, 
                "Extra root length below max rooting depth.\n\
Root density will decrease linearly from the GP calculated amount\n\
at max rooting depth to zero 'q' further down.");
    syntax.add ("a", "cm^-1", Syntax::LogOnly, "Form parameter.\n\
Calculated from 'a_DS'.");
    syntax.add ("L0", "cm/cm^3", Syntax::LogOnly,
                "Root density at soil surface.");
    Librarian::add_type (Rootdens::component, "Anders Pedersen", alist, syntax, &make);
  }
} Rootdens_AP_syntax;
