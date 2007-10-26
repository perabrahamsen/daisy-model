// rootdens_GP1D.C -- Gerwitz and Page model for calculating root density.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
// Copyright 2007 Per Abrahamsen and KVL.
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
#include "check.h"
#include "mathlib.h"
#include "librarian.h"
#include "iterative.h"

#include <sstream>

struct Rootdens_GP1D : public Rootdens
{
  // Parameters.
  const double DensRtTip;	// Root density at (pot) pen. depth. [cm/cm^3]
  const double DensIgnore;	// Ignore cells below this density. [cm/cm^3]

  // Log variables.
  double a;                     // Form parameter. [cm^-1]
  double L0;                    // Root density at soil surface. [cm/cm^3]
  double k;			// Scale factor due to soil limit. []

  // LogProduct
  struct InvW
  {
    static double derived (const double W)
    { return std::exp (W) + W * std::exp (W); }
    const double k;
    double operator()(const double W) const
    { return W * std::exp (W) - k; }
    InvW (const double k_)
      : k (k_)
    { }
  };

  // simulation.
  void set_density (Treelog&, std::vector<double>& Density,
		    const Geometry& geo, 
		    double Depth, double PotRtDpt,
		    double WRoot, double DS);
  void uniform (const Geometry& geo, const double l_r, const double d_a,
		std::vector<double>& Density);
  void output (Log& log) const;

  // Create.
  Rootdens_GP1D (Block&);
};

void
Rootdens_GP1D::set_density (Treelog& msg,
			    std::vector<double>& Density  /* [cm/cm^3] */,
			    const Geometry& geo, 
			    const double Depth /* [cm] */, 
			    const double PotRtDpt /* [cm] */,
			    const double WRoot /* [g DM/m^2] */, const double)
{
  const size_t cell_size = geo.cell_size ();

  // Check input.
  daisy_assert (Density.size () == cell_size);
  daisy_assert (PotRtDpt > 0);
  daisy_assert (WRoot > 0);

  static const double m_per_cm = 0.01;

  // Root dry matter.
  const double M_r = WRoot /* [g/m^2] */ * m_per_cm * m_per_cm; // [g/cm^2]

  // Specific root length.
  const double S_r = SpRtLength /* [m/g] */ / m_per_cm; // [cm/g]

  // Root length (Eq 3).
  const double l_r = S_r * M_r;	// [cm/cm^2]
  
  // Potential depth.
  const double d = PotRtDpt;	// [cm]

  // Actual depth.
  const double d_a = Depth;	// [cm]

  // Minimum density.
  const double L_m = DensRtTip;	// [cm/cm^3]

  // Minimum root length in root zone.
  const double l_m = L_m * d;	// [cm/cm^2]

  // Minimum root length as fraction of total root length.
  const double D = l_m / l_r;	// []

  // Identity: W = - a d
  // Solve: W * exp (W) = -D (Eq 6):
  // IW (W) = W ^* exp (W) - D
  // Since we know D, we can construct the function f.
  const InvW f (-D);		// [] -> []

  // ... And the derived df/dW

  // The function f has a local minimum at -1.
  const double W_min = -1;	   // []

  // There is no solution when -D is below the value at W_min.
  const double D_max = -f (W_min); // []

  // Too little root mass to fill the root zone.
  if (D > D_max)
    {
      // We warn once.
      static bool warn_about_to_little_root = true;
      if (warn_about_to_little_root)
	{
	  warn_about_to_little_root = false;
	  msg.warning ("Not enough root mass to fill root zone.  \
Using uniform distribution.");
	}
      uniform (geo, l_r, d_a, Density);
      return;
    }

  // There are two solutions to f (W) = 0, we are interested in the
  // one for W < W_min.  We start with a guess of -2.
  const double W = Newton (-2.0, f, f.derived);

  // Check the solution.
  const double f_W = f (W);
  if (!approximate (D, D + f_W))
    {
      std::ostringstream tmp;
      tmp << "Newton's methods did not converge.\n";
      tmp << "W = " << W << ", f (W) = " << f_W << ", D = " << D << "\n";
      (void) Newton (-2, f, f.derived, &tmp);
      tmp << "Using uniform distribution.";
      msg.error (tmp.str ());
      uniform (geo, l_r, d_a, Density);
      return;
    }

  // Find a from W (Eq 7):
  a = -W / d;			// [cm^-1]
  // and L0 from a (Eq 8):
  L0 = L_m  * std::exp (a * d);	// [cm/cm^3]

  // Fill Density.
  for (size_t cell = 0; cell < cell_size; cell++)
    {
      // TODO: Using cell average would be better than cell center.
      const double z = geo.z (cell);
      Density[cell] = L0 * std::exp (-a * z);
    }

  // Lowest density worth calculating on.
  const double L_epsilon = DensIgnore; // [cm/cm^3]

  // We find the total root length from cells above minimum.
  double l_i = 0;
  for (size_t cell = 0; cell < cell_size; cell++)
    {
      // Density in cell.
      double L_c = Density[cell];

      // Eliminate low density cells.
      if (L_c < L_epsilon)
	L_c = 0.0;

      // Eliminate roots below actual root depth.
      // TODO:  We really would like the soil maximum instead
      L_c *= geo.fraction_in_z_interval (cell, 0, -d_a);

      // Add and update.
      l_i += L_c * geo.cell_volume (cell);
      Density[cell] = L_c;
    }
  
  // No roots, nothing to do.
  if (iszero (l_i))
    {
      k = -1.0;
      if (l_r > 0)
	{
	  msg.error ("We lost all roots.  Using uniform distribution");
	  uniform (geo, l_r, d_a, Density);
	}
      return;
    }
  
  // Scale factor.
  k = l_r / l_i;		// Eq. 13
  for (size_t cell = 0; cell < cell_size; cell++)
    Density[cell] *= k;		// Eq. 12
}

void
Rootdens_GP1D::uniform (const Geometry& geo, const double l_r, const double d_a,
			std::vector<double>& Density)
{
  const size_t cell_size = geo.cell_size ();

  // Uniform distribution parameters.
  a = 0;
  L0 = l_r / d_a;
  for (size_t cell = 0; cell < cell_size; cell++)
    {
      const double f = geo.fraction_in_z_interval (cell, 0, -d_a);
      Density[cell] = f * L0;
    }
}

void 
Rootdens_GP1D::output (Log& log) const
{
  output_variable (a, log); 
  output_variable (L0, log); 
  output_variable (k, log); 
}

Rootdens_GP1D::Rootdens_GP1D (Block& al)
  : Rootdens (al),
    DensRtTip (al.number ("DensRtTip")),
    DensIgnore (al.number ("DensIgnore", DensRtTip)),
    a (-42.42e42),
    L0 (-42.42e42),
    k (-42.42e42)
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

static struct Rootdens_GP1DSyntax
{
  static Model& make (Block& al)
  { return *new Rootdens_GP1D (al); }
  Rootdens_GP1DSyntax ()
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
    syntax.add ("DensIgnore", "cm/cm^3", Check::positive (),
		Syntax::OptionalConst,
		"Ignore cells with less than this root density.\n\
By default, this is the same as DensRtTip.");
    syntax.add ("a", "cm^-1", Syntax::LogOnly, "Form parameter.\n\
Calculated from 'DensRtTip'.");
    syntax.add ("L0", "cm/cm^3", Syntax::LogOnly,
                "Root density at soil surface.");
    syntax.add ("k", Syntax::None (), Syntax::LogOnly,
                "Scale factor due to soil limit.\n\
\n\
Some roots might be below the soil imposed maximum root depth, or in areas\n\
with a density lower than the limit specified by DensIgnore.\n\
These roots will be re distributed within the root zone by multiplying the\n\
density with this scale factor.");

    Librarian::add_type (Rootdens::component, "Gerwitz+Page74", alist, syntax, &make);
  }
} Rootdens_GP1D_syntax;
