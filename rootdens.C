// rootdens.C
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

#define BUILD_DLL

#include "rootdens.h"
#include "block.h"
#include "syntax.h"
#include "alist.h"
#include "check.h"
#include "librarian.h"

const char *const Rootdens::component = "rootdens";

void
Rootdens::load_syntax (Syntax& syntax, AttributeList& alist)
{
  syntax.add ("SpRtLength", "m/g", Check::positive (), Syntax::Const,
	      "Specific root length");
  alist.add ("SpRtLength", 100.0);
}

Rootdens::Rootdens (Block& al)
  : name (al.identifier ("type")),
    SpRtLength (al.number ("SpRtLength"))
{ }

Rootdens::~Rootdens ()
{ }

static Librarian Rootdens_init (Rootdens::component, "\
Root density calculations.");

// rootdens.C ends here


#include "program.h"
#include "iterative.h"
#include "mathlib.h"
#include <sstream>

struct ProgramGP2D : public Program
{
  // Parameters
  const double depth;		// [m]
  const double radius;		// [m]
  const double row_distance;	// [m]
  const double weight_per_half_row; // [g/m]
  const double SpRtLength;	// [m/g]
  const double DensRtTip;	// [m/m^3]
  const double l_r;		// [m/m]
  const double table_step;	// [m]
  const int table_size;
  const bool show_iterations;
  const bool show_both_solutions;

  // The distribution.
  struct Qinv
  {
    const double k;
    
    double operator()(const double Q) const
    { return sqr (Q) * std::exp (Q) - k; }

    Qinv (const double k_)
      : k (k_)
    { }
  };
  static double dQinv (const double Q)
  // The derivative of the function Qinv function.
  { return 2.0 * Q * std::exp (Q) + sqr (Q) * std::exp (Q); }

  void solution (const double Q, std::ostream& out)
  {
    // Parameters.
    const double a_z = -Q / depth; // [m^-1]
    const double a_x = (depth/radius) * a_z; // [m^-1]
    const double L_00 = l_r * a_z * a_x;
    out << "Q = " << Q << " gives a_z = " << a_z
	<< " [m^-1], a_x = " << a_x << " [m^-1], and L_00 = "
	<< L_00 << " [m/m^3]\n";

    // Show table?
    if (table_size < 1)
      return;

    // Table.
    out << "Density table:\n";
    for (int column = 0; column < table_size; column++)
      out << "\t" << column * table_step * 100;
    out << "\n";
    for (int row = 0; row < table_size; row++)
      {
	const double z = row * table_step;
	out << z * 100;
	for (int column = 0; column < table_size; column++)
	  {
	    const double x = column * table_step;
	    out << "\t" << L_00 * std::exp (-a_z * z) * std::exp (-a_x * x);
	  }
	out << "\n";
      }
  }

  // Use.
  bool run (Treelog& msg)
  {
    std::ostringstream out;
    std::ostream *const iterative_stream = show_iterations ? &out : NULL;

    out << "There are " << l_r << " meter roots per meter half row [m/m].\n";

    const double min_zone = depth * radius * DensRtTip; // [m/m]
    out << "With a depth of " << depth << " [m] and a radius of " 
	<< radius << " [m]\n"
	<< "the minimum length of root to fill up the whole root zone with a\n"
	<< "density of " << DensRtTip << " [m/m3] is " 
	<< min_zone << " [m/m]\n";
    const double min_zone_fraction = min_zone / l_r; // []
    out << "This is "  << min_zone_fraction * 100 << " [%] of the total.\n";
    
    const double Q_min = 0.0;	// [] Local minimum.
    const double Q_max = -2.0;	// [] Local maximum.
    const double max_zone_fraction = std::exp (Q_max) * sqr (Q_max);
    out << "The highest fraction that we can find a solution for is " 
	<< max_zone_fraction * 100 << " [%]\n";

    if (min_zone_fraction > max_zone_fraction)
      out << "No solutions can be found\n";
    else if (!(min_zone_fraction < max_zone_fraction))
      // Equal written weirdly.
      {
	out << "One solutions can be found.\n";
	solution (Q_max, out);
      }
    else
      // Lower than.
      {
	out << "Two solutions can be found.\n"
	    << "One should be in the interval [" << Q_max 
	    << ";" << Q_min << "].\n";

	Qinv f (min_zone_fraction);

	if (show_both_solutions)
	  {
	    out << "We try bisection in that interval.\n";
	    const double Q_upper_bisection 
	      = bisection (Q_max, Q_min, f, iterative_stream);
	    out << "We also try Newton's method.\n";
	    const double Q_upper_Newton
	      = Newton ((Q_max + Q_min) / 2.0, f, dQinv, iterative_stream);
	    out << "The two solutions " << Q_upper_bisection 
		<< " (bisection) and " << Q_upper_bisection 
		<< " (Newton) differ with " 
		<< fabs (Q_upper_bisection - Q_upper_Newton) << " ";
	    if (approximate (Q_upper_bisection, Q_upper_Newton))
	      {
		out << "a good match\n"
		    << "The solution for this interval yields:\n";
		solution ((Q_upper_bisection + Q_upper_Newton) / 2.0, out);
	      }
	    else
	      {
		out << "which is too much.\n"
		    << "The solution found with bisection yields:\n";
		solution (Q_upper_bisection, out);
		out << "While the solution found with Newton yields:\n";
		solution (Q_upper_Newton, out);
	      }
	  }
	else
	  out << "\
That gives a flat root profile, with much root mass outside the root zone.\n\
We ignore that solution.\n";

	out << "For the solution below " << Q_max 
	    << " bisection needs a lower boundary.\n"
	    << "We just keep doubling the value until we get negative.\n";
	
	double Q_lower = Q_max; 
	for (int iterations = 0; f (Q_lower) > 0; iterations++)
	  {
	    Q_lower *= 2.0;
	    if (show_iterations)
	      out << iterations << ": f (" << Q_lower << ") = " << f (Q_lower)
		  << "\n";
	    iterations++;
	  }
	
	out << "Now we can try bisection in the interval [" << Q_lower << ";"
	    << Q_max << "]\n";
	
	const double Q_lower_bisection 
	  = bisection (Q_lower, Q_max, f, iterative_stream);
	out << "As we know the derivative, we can also try Newton's method.\n";
	const double Q_lower_Newton
	  = Newton ((Q_lower + Q_max) / 2.0, f, dQinv, iterative_stream);
	out << "The two solutions " << Q_lower_bisection << " (bisection) and " 
	    << Q_lower_bisection << " (Newton) differ with " 
	    << fabs (Q_lower_bisection - Q_lower_Newton);
	if (approximate (Q_lower_bisection, Q_lower_Newton))
	  {
	    out << ", a good match.\n"
		<< "The solution for this interval yields:\n";
	    solution ((Q_lower_bisection + Q_lower_Newton) / 2.0, out);
	  }
	else
	  {
	    out << ", which is too much.\n"
		<< "The solution found with bisection yields:\n";
	    solution (Q_lower_bisection, out);
	    out << "While the solution found with Newton yields:\n";
	    solution (Q_lower_Newton, out);
	  }
      }

    msg.message (out.str ());
    return true;
  }

  // Create and Destroy.
  void initialize (Block&)
  { };
  bool check (Treelog&)
  { return true; }
  ProgramGP2D (Block& al)
    : Program (al),
      depth (al.number ("depth")),
      radius (al.number ("width") * 0.5),
      row_distance (al.number ("row_distance")),
      weight_per_half_row (al.number ("dry_matter") * row_distance * 0.5),
      SpRtLength (al.number ("SpRtLength")),
      DensRtTip (al.number ("DensRtTip")),
      l_r (weight_per_half_row * SpRtLength),
      table_step (al.number ("table_step")),
      table_size (al.integer ("table_size")),
      show_iterations (al.flag ("show_iterations")),
      show_both_solutions (al.flag ("show_both_solutions"))
  { }
  ~ProgramGP2D ()
  { }
};

static struct ProgramGP2DSyntax
{
  static Model& make (Block& al)
  { return *new ProgramGP2D (al); }

  ProgramGP2DSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "\
Calculate parameters for a 2D version of the root density distribution\n\
described by Gerwitz & Page, 1974.");
    
    syntax.add ("depth", "m", Syntax::Const, "Max root depth.");
    syntax.add ("width", "m", Syntax::Const, "Max root width.");
    syntax.add ("dry_matter", "g/m^2", Syntax::Const, "Root dry_matter.");
    syntax.add ("row_distance", "m", Syntax::Const, "Distance between rows.");
    syntax.add ("SpRtLength", "m/g", Check::positive (), Syntax::Const,
		"Specific root length");
    alist.add ("SpRtLength", 100.0);
    syntax.add ("DensRtTip", "m/m^3", Check::positive (), Syntax::Const,
		"Root density at (potential) penetration depth.");
    alist.add ("DensRtTip", 0.1 /* [cm/cm^3] */ * 1e4);

    syntax.add ("table_step", "m", Syntax::Const, 
		"Distance to use for rows and columns in density table.");
    alist.add ("table_step", 0.1);
    syntax.add ("table_size", Syntax::Integer, Syntax::Const, "\
Number of rows and columns in density table.");
    alist.add ("table_size", 10);
    syntax.add ("show_iterations", Syntax::Boolean, Syntax::Const, "\
Show the iteration steps in the numeric solution.");
    alist.add ("show_iterations", false);
    syntax.add ("show_both_solutions", Syntax::Boolean, Syntax::Const, "\
Also show the flat solution, where much of the root dry mass is outside the \
root zone.");
    alist.add ("show_both_solutions", false);

    Librarian::add_type (Program::component, "GP2D", alist, syntax, &make);
  }
} ProgramGP2Dsyntax;


