// biopore_matrix.C --- Static vertical biopores with a capacity.
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
#include "imvec.h"
#include "block.h"
#include "vcheck.h"
#include "librarian.h"
#include "submodeler.h"
#include "geometry.h"
#include "soil.h"
#include "soil_heat.h"
#include "secondary.h"
#include "volume_box.h"
#include "log.h"
#include "check.h"
#include "anystate.h"
#include "chemical.h"
#include <sstream>

// The 'matrix' model.

struct BioporeMatrix : public Biopore
{
  // Parameters.
  /* const */ std::vector<double> xplus; // [cm]
  const double R_primary;       // [h/cm]
  const double R_secondary;     // [h/cm]
  
  // State.
  std::vector<double> h_bottom; // [cm]
  std::auto_ptr<IMvec> solute;  // [g]
  struct MyContent : public Anystate::Content
  {
    std::vector<double> h_bottom;
    std::auto_ptr<Anystate::Content> clone () const
    { 
      std::auto_ptr<Anystate::Content> copy (new MyContent (h_bottom)); 
      return copy;
    }
    MyContent (const std::vector<double> (h))
      : h_bottom (h)
    { }
  };
  Anystate get_state () const;
  void set_state (const Anystate&);
  bool converge (const Anystate&, double max_abs, double max_rel) const;
  
  // Utilities.
  /* const */ double dy;                    // [cm]
  std::vector<size_t> column;
  std::vector<double> added_water; // [cm^3]
  std::vector<double> density_column; // [cm^-2]

  double air_bottom (const size_t c) const // Lowest point with air [cm]
  { 
    daisy_assert (c < column.size ());
    const size_t col = column[c];
    daisy_assert (col < h_bottom.size ());
    return height_end + h_bottom[col]; 
  }

  // Simulation.
  double capacity (const Geometry& geo, size_t e, const double dt /* [h] */)
    /* [cm] */ const;
  void infiltrate (const Geometry&, size_t e, double amount /* [cm] */);

  double matrix_biopore_matrix (size_t c, const Geometry& geo, 
                                const Soil& soil, bool active, 
                                double K_xx, double h) const;
  double matrix_biopore_drain (size_t c, const Geometry& geo, 
                               const Soil& soil, bool active, 
                               double K_xx, double h) const
  {return 0.0;}
  void update_matrix_sink (const Geometry& geo,    
                           const Soil& soil,  
                           const SoilHeat& soil_heat, 
                           const std::vector<bool>& active,
                           const double pressure_initiate,
                           const std::vector<double>& h,
                           const double dt);
  void add_water (size_t c, double amount /* [cm^3] */);
  void update_water ();
  double column_water (const size_t col);
  void add_to_sink (std::vector<double>& S_matrix, std::vector<double>&);
  void add_solute (symbol chem, size_t cell, const double amount /* [g] */);
  void matrix_solute (const Geometry& geo, const double dt, 
                      const Chemical& chemical, std::vector<double>& S_chem,
                      Treelog& msg);
  void output (Log&) const;

  // Create and Destroy.
  bool initialize (const Geometry& geo, const Scope& scope, double,
                   Treelog& msg);
  bool check (const Geometry& geo, Treelog& msg) const
  { return check_base (geo, msg); }
  BioporeMatrix (Block& al);
};

Anystate
BioporeMatrix::get_state () const
{
  std::auto_ptr<Anystate::Content> copy (new MyContent (h_bottom));
  return Anystate (copy);
}
 
void 
BioporeMatrix::set_state (const Anystate& state)
{
  const MyContent& content = static_cast<const MyContent&> (state.inspect ());
  h_bottom = content.h_bottom;
}

bool 
BioporeMatrix::converge (const Anystate& state, 
                         const double max_abs, const double max_rel) const
{ 
  const MyContent& content = static_cast<const MyContent&> (state.inspect ());
  const size_t h_size = h_bottom.size ();
  daisy_assert (h_size == content.h_bottom.size ());
  const double max_h = height_start - height_end;
  daisy_assert (max_h > 0.0);
  for (size_t i = 0; i < h_size; i++)
    {
      // Check capacity.
      if (h_bottom[i] < 0.0 || content.h_bottom[i] < 0.0)
        return false;
      if (h_bottom[i] > max_h || content.h_bottom[i] > max_h)
        return false;
      // Check difference.
      if (   fabs (h_bottom[i] - content.h_bottom[i]) > max_abs
          && (   iszero (content.h_bottom[i])
              || iszero (h_bottom[i])
              || (  fabs ((h_bottom[i] - content.h_bottom[i]) 
                          / content.h_bottom[i])
                  > max_rel)))
        return false;
    }

  return true; 
}

double 
BioporeMatrix::capacity (const Geometry& geo, size_t e, const double dt) const 
{
  // Maximum based on infiltration rate.
  const double max_infiltration // [cm/h]
    = max_infiltration_rate (geo, e) * dt; 

  // Maximum based on capacity left.
  const size_t cell = geo.edge_other (e, Geometry::cell_above);
  daisy_assert (cell < geo.cell_size ());
  daisy_assert (cell < column.size ());
  const size_t col = column[cell];
  daisy_assert (col < density_column.size ());
  const double density = density_column[col]; // [cm^-2]
  const double radius = diameter / 2.0;       // [cm]
  const double area = radius * radius * M_PI; // [cm^2]
  const double height = -air_bottom (cell);   // [cm];
  const double max_capacity = height * area / density; // [cm]

  // Choose the lower limit;
  return std::min (max_infiltration, max_capacity);
}

void 
BioporeMatrix::infiltrate (const Geometry& geo, const size_t e,
                           const double amount /* [cm] */)
{ 
  const size_t cell = geo.edge_other (e, Geometry::cell_above);
  daisy_assert (cell < geo.cell_size ());
  daisy_assert (cell < column.size ());
  const double area = geo.edge_area (e);
  add_water (cell, area * amount);
}

double 
BioporeMatrix::matrix_biopore_matrix (size_t c, const Geometry& geo, 
                                      const Soil& soil, const bool active, 
                                      const double K_xx, const double h) const
{
  if (!std::isnormal (density (c)))
    // No biopores here.
    return 0.0;

  const Secondary& secondary = soil.secondary_domain (c);
  const bool use_primary = secondary.none ();
  const double R_wall = use_primary ? R_primary : R_secondary; // [h]  
  const size_t col = column[c];
  const double M_c = density_column[col];
  const double r_c = diameter / 2.0;
  const double h_3 = air_bottom (c) - geo.cell_z (c);

  double S; 
  if (h_bottom[col] > 0.0 && h_3>0.0 && h_3>h)
    S = -biopore_to_matrix (R_wall, M_c, r_c, h, h_3);
  else if (active && h>h_3)
    S = matrix_to_biopore (K_xx, M_c, r_c, h, h_3);
  else 
    S = 0.0;
  return S;
}

void
BioporeMatrix::update_matrix_sink (const Geometry& geo,    
                                   const Soil& soil,  
                                   const SoilHeat& soil_heat, 
                                   const std::vector<bool>& active,
                                   const double pressure_initiate,
                                   const std::vector<double>& h, 
                                   const double dt)
{
  const size_t cell_size = geo.cell_size ();
  for (size_t c = 0; c < cell_size; c++)
    {
      const double h_cond = std::min(pressure_initiate, h[c]);
      const double T = soil_heat.T (c);
      const double h_ice = 0.0;    //ice ignored 
      const double K_zz = soil.K (c, h_cond, h_ice, T);
      const double K_xx = K_zz * soil.anisotropy (c);
      S[c] = matrix_biopore_matrix (c, geo, soil, active[c], K_xx, h[c]);
      add_water (c, S[c] * dt * geo.cell_volume (c));
    }
}

void 
BioporeMatrix::add_water (size_t c, double amount /* [cm^3] */)
{
  daisy_assert (c < column.size ());
  const size_t col = column[c];
  daisy_assert (col < added_water.size ());
  added_water[col] += amount; 
}

void
BioporeMatrix::update_water ()
{ 
  const size_t column_size = xplus.size ();
  daisy_assert (added_water.size () == xplus.size ());
  double xminus = 0.0;
  for (size_t i = 0; i < column_size; i++)
    {
      const double water_volume = added_water[i];  // [cm^3]
      if (std::fabs (water_volume) < 1e-100)
        continue;
      const double density = density_column[i]; // [cm^-2]
      const double radius = diameter * 0.5;     // [cm]
      const double area = M_PI * radius * radius;  // [cm^2]
      const double soil_fraction = density * area; // []
      const double soil_volume = water_volume / soil_fraction; // [cm^3]
      const double dx = xplus[i] - xminus;                     // [cm]
      const double dz = soil_volume / (dx * dy);               // [cm]
      h_bottom[i] += dz;                                       // [cm]
      added_water[i] = 0.0;                                    // [cm^3]
      xminus = xplus[i];                                       // [cm]
    }
}

double
BioporeMatrix::column_water (const size_t col)
{
  daisy_assert (col < xplus.size ());
  const double xminus = (col == 0) ? 0.0 : xplus[col-1]; // [cm]
  const double dx = xplus[col] - xminus;      // [cm]
  const double dz = h_bottom[col];            // [cm]
  const double soil_volume = dz * dx * dy;    // [cm^3]
  const double density = density_column[col]; // [cm^-2]
  const double radius = diameter * 0.5;     // [cm]
  const double area = M_PI * radius * radius;  // [cm^2]
  const double soil_fraction = density * area; // []
  const double water_volume = soil_volume * soil_fraction; // [cm^3]
  return water_volume;
}

void 
BioporeMatrix::add_to_sink (std::vector<double>& S_matrix,
                            std::vector<double>&)
{
  const size_t cell_size = S.size ();
  daisy_assert (S_matrix.size () == cell_size);
  for (size_t c = 0; c < cell_size; c++)
    S_matrix[c] += S[c];
}

void 
BioporeMatrix::add_solute (const symbol chem, 
                           const size_t cell, const double amount /* [g] */)
{
  daisy_assert (cell < column.size ());
  const size_t col = column[cell];
  solute->add_value (chem, col, amount);
}

void 
BioporeMatrix::matrix_solute (const Geometry& geo, const double dt, 
                              const Chemical& chemical,
                              std::vector<double>& S_chem_total,
                              Treelog& msg)
{
  const symbol chem = chemical.name;
  const size_t cell_size = geo.cell_size ();
  daisy_assert (S_chem_total.size () == cell_size);
  std::vector<double> S_chem (cell_size, 0.0);

  // Water that left each column.
  const size_t column_size = xplus.size ();
  std::vector<double> water_left (column_size, 0.0); // [cm^3 W]

  // From matrix to biopore.
  for (size_t c = 0; c < cell_size; c++)
    {
      // Water.
      const double cell_volume = geo.cell_volume (c); // [cm^3 S]
      const double water_sink = S[c]; // [cm^3 W/cm^3 S/h]
      const double water = -cell_volume * water_sink * dt; // [cm^3 W]
      if (water_sink <= 0.0)
        // Keep track of water that is leaving for later.
        {
          const size_t col = column[c];
          water_left[col] += water;
          continue;
        }
      daisy_assert (water < 0.0);

      // Matrix concentration.
      const double C = chemical.C_secondary (c); // [g/cm^3 W]

      // Add to solute sink.
      S_chem[c] = water_sink * C; // [g/cm^3 S/h]

      // Add to biopore.
      const double M = C * water; // [g]
      add_solute (chem, c, M);
    }
  
  // From biopore to matrix.
  for (size_t c = 0; c < cell_size; c++)
    {
      const double water_sink = S[c]; // [cm^3 W/cm^3 S/h]
      if (water_sink >= 0.0)
        // Water entering biopore, ignore.
        continue;

      // Add it solute sink.
      const size_t col = column[c];
      const double total_water  // [cm^3 W]
        = column_water (col) + water_left[col];
      const double M = solute->get_value (chem, col);
      const double C = M / total_water; // [g/cm^3 W]
      S_chem[c] = water_sink * C;
    }

  // Remove from biopores.
  for (size_t c = 0; c < cell_size; c++)
    {
      if (S_chem[c] >= 0.0)
        continue;

      const double cell_volume = geo.cell_volume (c); // [cm^3 S]
      const double M = S_chem[c] * cell_volume * dt;  // [g]
      add_solute (chem, c, M);
    }

  // Export.
  for (size_t c = 0; c < cell_size; c++)
    S_chem_total[c] += S_chem[c];
}

void
BioporeMatrix::output (Log& log) const
{
  output_base (log);
  output_variable (h_bottom, log);
  output_submodule (*solute, "solute", log);
}

bool 
BioporeMatrix::initialize (const Geometry& geo, const Scope& scope, double,
                           Treelog& msg)
{ 
  bool ok = true;

  // base.
  if (!initialize_base (geo, scope, msg))
    ok = false;

  // xplus.
  if (xplus.size () == 0)
    geo.fill_xplus (xplus);
  const size_t column_size = xplus.size ();
  daisy_assert (column_size > 0);

  // h_bottom.
  if (h_bottom.size () == 0)
    h_bottom.insert (h_bottom.end (), column_size, 0.0);

  if (h_bottom.size () != column_size)
    {
      msg.error ("Number of elements in 'h_bottom' does not match 'xplus'");
      ok = false;
    }

  // dy.
  dy = geo.back () - geo.front ();

  // column.
  daisy_assert (column.size () == 0);
  const size_t cell_size = geo.cell_size ();
  for (size_t c = 0; c < cell_size; c++)
    {
      const double x = geo.cell_x (c);
      for (size_t i = 0; i < column_size; i++)
        if (x < xplus[i])
          {
            column.push_back (i);
            goto found;
          }
      column.push_back (0U);
      {
        std::ostringstream tmp;
        tmp << "cell[" << c << "].x = " << x << ", > xplus[" << column_size - 1 
            << "] = " << xplus[column_size-1];
        msg.error (tmp.str ());
      }
      ok = false;
    found:
      ;
    }
  daisy_assert (column.size () == cell_size);

  // added_water.
  added_water.insert (added_water.end (), column_size, 0.0);
  daisy_assert (added_water.size () == column_size);

  // density_column.
  if (density_cell.size () != cell_size)
    return false;

  double xminus = 0.0;
  for (size_t i = 0; i < column_size; i++)
    {
      VolumeBox square ("square", height_end, height_start, xminus, xplus[i]);
      const double volume = square.volume ();
      daisy_assert (volume > 0.0);
      const double content = geo.total_soil (density_cell, square);
      const double density = content / volume;
      density_column.push_back (density);
      xminus = xplus[i];
    }

  return ok;
}

BioporeMatrix::BioporeMatrix (Block& al)
  : Biopore (al),
    xplus (al.check ("xplus") 
           ? al.number_sequence ("xplus") 
           : std::vector<double> ()),
    R_primary (al.number ("R_primary")),
    R_secondary (al.number ("R_secondary", R_primary)),
    h_bottom (al.check ("h_bottom") 
              ? al.number_sequence ("h_bottom") 
              : std::vector<double> ()),
    solute (al.check ("solute")
            ? new IMvec (al, "solute")
            : NULL)
{ }

static struct BioporeMatrixSyntax
{
  static Model& make (Block& al)
  { return *new BioporeMatrix (al); }

  BioporeMatrixSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Biopores that ends in the matrix.");
    Biopore::load_base (syntax, alist);

    syntax.add ("xplus", "cm", Check::positive (), 
                Syntax::OptionalConst, Syntax::Sequence,
                "Right side of each biopore interval.\n\
Water and chemical content is tracked individually for each interval.\n\
By default, use intervals as specified by the geometry.");
    syntax.add_check ("xplus", VCheck::increasing ());
    syntax.add ("R_primary", "h", Check::positive (), Syntax::Const, "\
Resistance for water moving from biopore through wall to primary domain.");
    syntax.add ("R_secondary", "h", Check::positive (), 
                Syntax::OptionalConst, "\
Resistance for water moving from biopore through wall to secondary domain.\n\
If not specified, this will be identical to 'R_primary'.");
    syntax.add ("h_bottom", "cm", Syntax::OptionalState, Syntax::Sequence,
                "Pressure at the bottom of the biopores in each interval.");

    static const symbol C_unit ("g");
    IMvec::add_syntax (syntax, alist, Syntax::OptionalState, "solute", C_unit,
                       "Chemical concentration in biopore intervals.");

    Librarian::add_type (Biopore::component, "matrix", alist, syntax, &make);
  }
} BioporeMatrix_syntax;

// biopore_matrix.C ends here.
