// biopore.C --- A single class of biopores.
// 
// Copyright 2008 Per Abrahamsen and KU.
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

#include "biopore.h"
#include "block.h"
#include "frame.h"
#include "librarian.h"
#include "scope_multi.h"
#include "scope_id.h"
#include "units.h"
#include "check.h"
#include "geometry.h"
#include "log.h"
#include "treelog.h"
#include "assertion.h"
#include "mathlib.h"
#include <sstream>

// biopore component.

const char *const Biopore::component = "biopore";

symbol
Biopore::library_id () const
{
  static const symbol id (component);
  return id;
}

symbol
Biopore::x_symbol ()
{
  static const symbol x ("x");
  return x;
}

double
Biopore::max_infiltration_rate (const Geometry& geo, size_t e) const // [cm/h]
{
  const size_t cell = geo.edge_other (e, Geometry::cell_above);
  daisy_assert (cell < geo.cell_size ());

  // Based on Poiseuille equation.
  // Q = pi r^4 rho g L / ( 8 L mu)
  //
  //        Q [cm^3/h]    Infiltration rate
  //  delta P [cm]        Pressure drop.
  //        L [cm]        Length op cylinder.
  //       mu [cm^3/cm/h] Dynamic viscosity.
  //        r [cm]        Biopore radius.
  //        M [cm^-2]     Macropore density.
  //     

  const double rho = 1.0;   // [g/cm^3]
  const double g = 9.81;      // [m/s^2]
  const double g1 = g * 100.0 * 3600.0 * 3600.0;        // [cm/h^2]
  const double mu = (1.0020 / 100.0) * 3600.0;          // [g/cm/h] At 20 dg C. 
  const double M = density (cell);                      // [cm^-2]
  const double r = diameter * 0.5;                      // [cm]
  const double r4 = r * r * r * r;                      // [cm^4]
  const double Q = M_PI * r4 * rho * g1 / (8 * mu);     // [cm^3/h]
  const double q = Q * M;                               // [cm/h]
  
  return q;
}

void 
Biopore::infiltrate (const Geometry& geo, size_t e,
                     const double amount, const double dt)
{
  const double edge_area = geo.edge_area (e);
  const double total_area = geo.surface_area ();
  const double edge_flux = amount / dt;
  const double total_flux = edge_flux * edge_area / total_area;
  infiltration += total_flux;
}

void 
Biopore::solute_infiltrate (const symbol chem, 
                            const Geometry& geo, const size_t e,
                            const double amount /* [g] */, 
                            const double dt)
{
  const double total_area = geo.surface_area ();
  solute_infiltration.add_value (chem, solute_infiltration.unit (), 
                                 amount / total_area / dt);
}

void 
Biopore::clear ()
{ 
  infiltration = 0.0; 
  solute_infiltration.clear ();
}

double 
Biopore::matrix_to_biopore (double K_xx, double M_c, double r_c, 
                            double h, double h_3)
{
  const double S = - 4*M_PI*M_c*K_xx*(h-h_3) / 
    (log(M_PI*M_c*r_c*r_c));
  daisy_assert (std::isfinite (S));
  
  if (S < 0.0)
    {
      std::ostringstream tmp;
      tmp << "Bug: S = " << S << " (should be positive), M_c = " << M_c
          << ", r_c = " << r_c << ", K_xx = " << K_xx << ", h = " << h
          << ", h_3 = " << h_3 << ", pi M_c r_c^2 = " << M_PI*M_c*r_c*r_c;
      Assertion::error (tmp.str ());
      return 0.0;
    }
  return S;
}

double 
Biopore::biopore_to_matrix (double R_wall, double M_c, double r_c,
                            double h, double h_3)
{
  const double S = 4*M_PI*M_c*(h-h_3) / 
    (R_wall*log(M_PI*M_c*r_c*r_c));
  daisy_assert (S >= 0.0);
  return S;
}

void 
Biopore::scale_sink (const double scale)
{
  const size_t cell_size = S.size ();
  for (size_t c = 0; c < cell_size; c++)
    S[c] *= scale;
}

void 
Biopore::output_base (Log& log) const
{
  output_variable (S, log);
  output_variable (infiltration, log);
  output_submodule (solute_infiltration, "solute_infiltration", log);
}

bool
Biopore::initialize_base (const Units& units, 
                          const Geometry& geo, const Scope& parent_scope, 
                          Treelog& msg)
{ 
  static const symbol per_square_centimeter ("cm^-2");

  ScopeID own_scope (x_symbol (), Units::cm ());
  ScopeMulti scope (own_scope, parent_scope);
  
  if (!density_expr->initialize (units, scope, msg))
    return false;

  if (!density_expr->check_dim (units, scope, per_square_centimeter, msg))
    return false;

  const size_t cell_size = geo.cell_size ();
  density_cell.reserve (cell_size);
  double value = -42.42e42;
  bool ok = true;
  for (size_t c = 0; c < cell_size; c++)
    {
      if (geo.cell_bottom (c) >= height_start || geo.cell_top (c) <= height_end)
        // Outside z interval.
        density_cell.push_back (0.0);
      else
        {
          own_scope.add (x_symbol (), geo.cell_x (c));
          if (!density_expr->tick_value (units, value,
                                         per_square_centimeter, scope, msg))
            ok = false;
          density_cell.push_back (value);
        }
    }
  daisy_assert (density_cell.size () == cell_size);

  // Sink term.
  S.insert (S.begin (), cell_size, 0.0);

  return ok;
}

bool 
Biopore::check_base (const Geometry& geo, Treelog& msg) const
{
  bool ok = true;
  const size_t cell_size = geo.cell_size ();

  if (cell_size != density_cell.size ())
    {
      msg.error ("Initialization of cell density failed");
      ok = false;
    }

  const double radius = diameter * 0.5;
  const double area = M_PI * radius * radius;
  for (size_t c = 0; c < cell_size && ok; c++)
    {
      if (density_cell[c] * area >= 0.5)
        {
          std::ostringstream tmp;
          tmp << "Biopore domain occupies " << density_cell[c] * area * 100.0
              << "% of available space in cell @ " << geo.cell_name (c) 
              << ", which is just silly";
          msg.error (tmp.str ());
          ok = false;
        }
    }
  return ok;
}

Biopore::Biopore (Block& al)
  : ModelAListed (al.alist ()),
    density_expr (Librarian::build_item<Number> (al, "density")),
    height_start (al.number ("height_start")),
    height_end (al.number ("height_end")),
    diameter (al.number ("diameter")),
    infiltration (0.0),
    solute_infiltration (al.units ().get_unit (IM::flux_unit ()))
{ }

Biopore::~Biopore ()
{ }

static struct BioporeInit : public DeclareComponent 
{
  BioporeInit ()
    : DeclareComponent (Biopore::component, "\
A single class of biopores.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.add_object ("density", Number::component, 
                         Value::Const, Value::Singleton, "\
Biopore density [cm^-2] as a function of 'x' [cm].");
    frame.add ("height_start", "cm", Check::non_positive (), Value::Const, 
                "Biopores starts at this depth (a negative number).");
    frame.add ("height_end", "cm", Check::non_positive (), Value::Const, 
                "Biopores ends at this depth (a negative number).");
    frame.add ("diameter", "cm", Check::positive (),
                Value::Const, "Biopore diameter.");
    frame.add ("S", "cm^3/cm^3/h", Value::LogOnly, Value::Sequence,
                "Sink from matrix domain to biopore.");
    frame.add ("infiltration", "cm/h", Value::LogOnly, "\
Surface infiltration.");
    IM::add_syntax (frame.syntax (), frame.alist (), Value::LogOnly,
                    "solute_infiltration", 
                    IM::flux_unit (),
                    "Rate of solute infiltration through surface.");
  }
} Biopore_init;

// biopore.C ends here.
