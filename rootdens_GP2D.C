// rootdens_GP2D.C -- Gerwitz and Page model extended for row crops.
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

static const double default_DensRtTip = 0.1;

struct Rootdens_GP2D : public Rootdens
{
  // Parameters.
  const double row_position;	// Horizontal position of row crops. [cm]
  const double row_distance;	// Distance betweeen rows. [cm]
  const double DensRtTip;	// Root density at (pot) pen. depth. [cm/cm^3]
  const double DensIgnore;	// Ignore cells below this density. [cm/cm^3]

  // Log variables.
  double a_z;                     // Form parameter. [cm^-1]
  double a_x;                     // Form parameter. [cm^-1]
  double L00;		      // Root density at row at soil surface. [cm/cm^3]
  double k;			// Scale factor due to soil limit. []

  // LogSquare
  struct InvQ
  {
    static double derived (const double Q)
    { return 2.0 * Q * std::exp (Q) + sqr (Q) * std::exp (Q); }
    const double k;
    double operator()(const double Q) const
    { return sqr (Q) * std::exp (Q) - k; }
    InvQ (const double k_)
      : k (k_)
    { }
  };

  // simulation.
  void set_density (const Geometry& geo, 
		    double SoilDepth, double CropDepth, double CropWidth,
		    double WRoot, double DS, std::vector<double>& Density,
		    Treelog&);
  void limit_depth (const Geometry& geo, 
		    const double l_r /* [cm/cm^2] */,
		    const double d_s /* [cm] */, 
		    const double d_a /* [cm] */, 
		    std::vector<double>& Density  /* [cm/cm^3] */,
		    Treelog& msg);
  void uniform (const Geometry& geo, const double l_r, const double d_a,
		std::vector<double>& Density);
  void output (Log& log) const;

  // Create.
  void initialize (const Geometry&, double /* row_width */, Treelog&);
  static void load_syntax (Syntax&, AttributeList&);
  explicit Rootdens_GP2D (Block&);
  explicit Rootdens_GP2D (const AttributeList&);
};

void
Rootdens_GP2D::set_density (const Geometry& geo, 
			    const double SoilDepth /* [cm] */, 
			    const double CropDepth /* [cm] */,
			    const double CropWidth /* [cm] */,
			    const double WRoot /* [g DM/m^2] */, const double,
			    std::vector<double>& Density  /* [cm/cm^3] */,
			    Treelog& msg)
{
  const size_t cell_size = geo.cell_size ();

  // Check input.
  daisy_assert (Density.size () == cell_size);
  daisy_assert (CropDepth > 0);
  daisy_assert (WRoot > 0);

  static const double m_per_cm = 0.01;

  // Row distance.
  const double R = row_distance; /* [cm] */ 

  // Root dry matter per area.
  const double M_r = WRoot /* [g/m^2] */ * m_per_cm * m_per_cm; // [g/cm^2]

  // Specific root length.
  const double S_r = SpRtLength /* [m/g] */ / m_per_cm; // [cm/g]

  // Root length (\ref{eq:root_length} Eq 3).
  const double l_r = S_r * M_r;	// [cm/cm^2]

  // Root length per half row.
  const double l_R = l_r * 0.5 * R; // [cm/cm]

  // Crop width.
  const double w_c = CropWidth;	// [cm]

  // Potential depth.
  const double d_c = CropDepth;	// [cm]

  // Soil depth.
  const double d_s = SoilDepth;	// [cm]

  // Actual depth.
  const double d_a = std::min (d_c, d_s); // [cm]

  // Minimum density.
  const double L_m = DensRtTip;	// [cm/cm^3]

  // Minimum root length in root zone.
  const double l_m = L_m * d_c;	// [cm/cm^2]

  // Minimum root length as fraction of total root length.
  const double D = l_m / l_r;	// []

  // Identity: Q = -a_z d_c
  // Solve: Q^2 * exp (Q) = D (\eqref{eq:logsquare} Eq 23):
  // IQ (Q) = Q^2 * exp (Q) + D
  // Since we know D, we can construct the function g.
  const InvQ g (D);		// [] -> []

  // The function g has a local maximum at -2.
  const double Q_max = -2;	   // []

  // There is no solution when D is above the value at Q_max.
  const double D_max = sqr (Q_max) * std::exp (Q_max); // []

  // Too little root mass to fill the root zone.
  if (D > D_max)
    {
      // We warn once.
      static bool warn_about_to_little_root = true;
      if (warn_about_to_little_root)
	{
	  // warn_about_to_little_root = false;
	  std::ostringstream tmp;
	  tmp << "Min ratio is " << D << ", max is " << D_max << ".\n"
	      << "Not enough root mass to fill root zone.\n"
	      << "Using uniform distribution.";
	  msg.warning (tmp.str ());
	}
      uniform (geo, l_r, d_a, Density);
      return;
    }

  // There are three solutions to g (Q) = 0, we are interested in the
  // one for Q < Q_max.  We start with a guess of -3.
  const double Q = Newton (-3.0, g, g.derived);
  
  // Check the solution.
  const double g_Q = g (Q);
  if (!approximate (D, D - g_Q))
    {
      std::ostringstream tmp;
      tmp << "Newton's methods did not converge.\n";
      tmp << "Q = " << Q << ", g (Q) = " << g_Q << ", D = " << D << "\n";
      (void) Newton (-3.0, g, g.derived, &tmp);
      tmp << "Using uniform distribution.";
      msg.error (tmp.str ());
      uniform (geo, l_r, d_a, Density);
      return;
    }

  // Find a_z from Q (\ref{eq:Qaz} Eq 22):
  a_z = -Q / d_c;		// [cm^-1]

  // Find a_x from a_z (\ref{eq:aztoax} Eq 20):
  a_x = (d_c / w_c) *  a_z;	// [cm^-1]

  // and L00 from a (\ref{{eq:root-integral2} Eq 16):
  L00 = l_R  * a_z * a_x;	// [cm/cm^3]

  // Fill Density.
  for (size_t cell = 0; cell < cell_size; cell++)
    {
      const double z = -geo.z (cell); // Positive below ground. [cm]
      double x = geo.x (cell) - row_position; // Rel. pos to row.

      const double row_center = row_distance / 2.0; // Row center. [cm]

      // While x is on the other size of the right center, go one row to the
      // left.
      while (x > row_center)
	x -= row_distance;

      // While x is on the other size of the left center, go one row to the
      // right.
      while (x < -row_center)
	x += row_distance;

      // Make it positive (mirror in row).
      x = std::fabs (x);

      // We should now be between the crop row, and the right center.
      daisy_assert (x <= row_center);
      
      // \ref{eq:Lzxstar-solved} Eq 27
      Density[cell] = L00 * std::exp (-a_z * z) 
	* (std::exp (-a_x * x) + std::exp (-a_x * (row_distance - x)))
	/ (1.0 - std::pow (1.0/std::exp (1.0), a_x * row_distance));
    }

  // Redistribute roots from outside root zone.
  limit_depth (geo, l_r, d_s, d_a, Density, msg);
}

void
Rootdens_GP2D::limit_depth (const Geometry& geo, 
			    const double l_r /* [cm/cm^2] */,
			    const double d_s /* [cm] */, 
			    const double d_a /* [cm] */, 
			    std::vector<double>& Density  /* [cm/cm^3] */,
			    Treelog& msg)
{
  const size_t cell_size = geo.cell_size ();
			    
  // Lowest density worth calculating on.
  const double L_epsilon = DensIgnore; // [cm/cm^3]

  // We find the total root length from cells above minimum.
  double l_i = 0;		// Integrated root length [cm]
  for (size_t cell = 0; cell < cell_size; cell++)
    {
      // Density in cell.
      double L_c = Density[cell];

      // Eliminate low density cells.
      if (L_c < L_epsilon)
	L_c = 0.0;

      // Eliminate roots below actual root depth.
      // TODO:  We really would like the soil maximum instead
      L_c *= geo.fraction_in_z_interval (cell, 0, -d_s);

      // Add and update.
      l_i += L_c * geo.cell_volume (cell);
      Density[cell] = L_c;
    }
  l_i /= geo.surface_area ();	// Per area [cm/cm^2]

  // No roots, nothing to do.
  if (iszero (l_i))
    {
      if (l_r > 0)
	{
	  msg.error ("We lost all roots.  Using uniform distribution");
	  uniform (geo, l_r, d_a, Density);
	}
      k = -1.0;
      return;
    }
  
  // Scale factor.
  k = l_r / l_i;		// \ref{eq:scale-factor} Eq. 13
  for (size_t cell = 0; cell < cell_size; cell++)
    Density[cell] *= k;		// \ref{eq:limited-depth} Eq. 12
}

void
Rootdens_GP2D::uniform (const Geometry& geo, const double l_r, const double d_a,
			std::vector<double>& Density)
{
  const size_t cell_size = geo.cell_size ();

  // Uniform distribution parameters.
  a_z = 0;
  a_x = 0;
  L00 = std::max (l_r / d_a, DensRtTip);
  for (size_t cell = 0; cell < cell_size; cell++)
    {
      const double f = geo.fraction_in_z_interval (cell, 0, -d_a);
      Density[cell] = f * L00;
    }
  k = 1.0;
}

void 
Rootdens_GP2D::output (Log& log) const
{
  output_variable (a_z, log); 
  output_variable (a_x, log); 
  output_variable (L00, log); 
  output_variable (k, log); 
}

void 
Rootdens_GP2D::initialize (const Geometry& geo, double row_width, Treelog& msg)
{ 
  const double geo_width = geo.right () - geo.left ();
  if (!approximate (geo_width, row_width))
    {
      std::ostringstream tmp;
      tmp << "Row width (" << row_width << ") should match geometry width ("
          << geo_width << ")";
      msg.warning (tmp.str ());
    }
  if (!approximate (row_distance, row_width))
    {
      std::ostringstream tmp;
      tmp << "Row width (" << row_width << ") does not match root distance ("
          << row_distance << ")";
      msg.warning (tmp.str ());
    }
}

void
Rootdens_GP2D::load_syntax (Syntax& syntax, AttributeList& alist)
{
  Rootdens::load_base (syntax, alist);
  syntax.add ("row_position", "cm", Syntax::Const, "\
Horizontal position of row crops.");
  alist.add ("row_position", 0.0);
  syntax.add ("row_distance", "cm", Syntax::Const, 
              "Distance between rows of crops.");
  syntax.add ("DensRtTip", "cm/cm^3", Check::positive (), Syntax::Const,
              "Root density at (potential) penetration depth.");
  alist.add ("DensRtTip", default_DensRtTip);
  syntax.add ("DensIgnore", "cm/cm^3", Check::positive (),
              Syntax::OptionalConst,
              "Ignore cells with less than this root density.\n\
By default, this is the same as DensRtTip.");
  syntax.add ("a_z", "cm^-1", Syntax::LogOnly, "Form parameter.\n\
Calculated from 'DensRtTip'.");
  syntax.add ("a_x", "cm^-1", Syntax::LogOnly, "Form parameter.\n\
Calculated from 'DensRtTip'.");
  syntax.add ("L00", "cm/cm^3", Syntax::LogOnly,
              "Root density at row crop at soil surface.");
  syntax.add ("k", Syntax::None (), Syntax::LogOnly,
              "Scale factor due to soil limit.\n\
\n\
Some roots might be below the soil imposed maximum root depth, or in areas\n\
with a density lower than the limit specified by 'DensIgnore'.\n\
These roots will be re distributed within the root zone by multiplying the\n\
density with this scale factor.");
}

Rootdens_GP2D::Rootdens_GP2D (Block& al)
  : Rootdens (al),
    row_position (al.number ("row_position")),
    row_distance (al.number ("row_distance")),
    DensRtTip (al.number ("DensRtTip")),
    DensIgnore (al.number ("DensIgnore", DensRtTip)),
    a_z (-42.42e42),
    a_x (-42.42e42),
    L00 (-42.42e42),
    k (-42.42e42)
{ }

Rootdens_GP2D::Rootdens_GP2D (const AttributeList& al)
  : Rootdens (al),
    row_position (al.number ("row_position")),
    row_distance (al.number ("row_distance")),
    DensRtTip (al.number ("DensRtTip")),
    DensIgnore (al.number ("DensIgnore", DensRtTip)),
    a_z (-42.42e42),
    a_x (-42.42e42),
    L00 (-42.42e42),
    k (-42.42e42)
{ }

std::auto_ptr<Rootdens> 
Rootdens::create_row (const double row_width, const double row_position)
{
  Syntax dummy;
  AttributeList alist;
  Rootdens_GP2D::load_syntax (dummy, alist);
  alist.add ("type", "GP2D");
  alist.add ("row_position", row_position);
  alist.add ("row_distance", row_width);
  return std::auto_ptr<Rootdens> (new Rootdens_GP2D (alist)); 
}

static struct Rootdens_GP2DSyntax
{
  static Model& make (Block& al)
  { return *new Rootdens_GP2D (al); }
  Rootdens_GP2DSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", 
	       "Use exponential function for root density in row crops.\n\
\n\
This is a two dimension model (z, x), where the z-axis is vertical,\n\
and the x-axis is horizontal and ortogonal to the row.  The row is\n\
assumed to be uniform (dense), allowing us to ignore that dimension.\n\
\n\
We assume the root density decrease with horizontal distance to row,\n\
as well as depth below row.\n\
\n\
See Gerwitz, S. and E.R. Page (1974): An empirical mathematical model\n\
to describe plant root systems.  J. Appl. Ecol. 11, 773-781.");
    Rootdens_GP2D::load_syntax (syntax, alist);
    
    Librarian::add_type (Rootdens::component, "GP2D", alist, syntax, &make);
  }
} Rootdens_GP2D_syntax;

// rootdens_GP2D.C ends here.
