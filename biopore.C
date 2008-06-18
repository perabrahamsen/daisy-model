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
#include "alist.h"
#include "librarian.h"
#include "scope_multi.h"
#include "scope_id.h"
#include "units.h"
#include "check.h"
#include "geometry.h"

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
Biopore::matrix_to_biopore (double K_xx, double M_c, double r_c, 
                            double h, double h_3)
{
  return - 4*M_PI*M_c*K_xx*(h-h_3) / 
    (log(M_PI*M_c*r_c*r_c));
}


double 
Biopore::biopore_to_matrix (double R_wall, double M_c, double r_c,
                            double h, double h_3)
{
  return 4*M_PI*M_c*(h-h_3) / 
    (R_wall*log(M_PI*M_c*r_c*r_c));
}





bool
Biopore::initialize_base (const Geometry& geo, const Scope& parent_scope, 
                          Treelog& msg)
{ 
  if (!density_expr->initialize (msg))
    return false;

  static const symbol per_square_meter ("m^-2");

  ScopeID own_scope (x_symbol (), Units::cm ());
  ScopeMulti scope (own_scope, parent_scope);
  
  if (!density_expr->check_dim (scope, per_square_meter, msg))
    return false;

  const size_t cell_size = geo.cell_size ();
  density_cell.reserve (cell_size);
  double value = -42.42e42;
  bool ok = true;
  for (size_t c = 0; c < cell_size; c++)
    {
      const double cell_z = geo.cell_z (c);
      if (cell_z > height_start || cell_z < height_end)
        // Outside z interval.
        density_cell.push_back (0.0);
      else
        {
          own_scope.set_number (x_symbol (), geo.cell_x (c));
          if (!density_expr->tick_value (value, per_square_meter, scope, msg))
            ok = false;
          density_cell.push_back (value);
        }
    }
  daisy_assert (density_cell.size () == cell_size);

  return ok;
}

bool 
Biopore::check_base (const Geometry& geo, Treelog& msg) const
{
  bool ok = true;
  if (geo.cell_size () != density_cell.size ())
    {
      msg.error ("Initialization of cell density failed");
      ok = false;
    }
  return ok;
}

void 
Biopore::load_base (Syntax& syntax, AttributeList&)
{
  syntax.add_object ("density", Number::component, 
                       Syntax::Const, Syntax::Singleton, "\
Root density [m^-2] as a function of 'x' [cm].");
  syntax.add ("height_start", "cm", Check::non_positive (), Syntax::Const, 
	      "Biopores starts at this depth (a negative number).");
  syntax.add ("height_end", "cm", Check::non_positive (), Syntax::Const, 
	      "Biopores ends at this depth (a negative number).");
  syntax.add ("diameter", "cm", Check::positive (),
              Syntax::Const, "Biopore diameter.");
}

Biopore::Biopore (Block& al)
  : ModelAListed (al.alist ()),
    density_expr (Librarian::build_item<Number> (al, "density")),
    height_start (al.number ("height_start")),
    height_end (al.number ("height_end")),
    diameter (al.number ("diameter"))
{ }

Biopore::~Biopore ()
{ }

static Librarian Biopore_init (Biopore::component, "\
A single class of biopores.");

// biopore.C ends here.
