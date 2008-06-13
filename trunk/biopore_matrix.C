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
#include "soil_water.h"
#include "secondary.h"
#include "volume_box.h"
#include "log.h"
#include "check.h"
#include <sstream>

// The 'matrix' model.

struct BioporeMatrix : public Biopore
{
  // Parameters.
  /* const */ std::vector<double> xplus; // [cm]
  const double diameter;        // [cm]
  const double R_primary;       // [h/cm]
  const double R_secondary;     // [h/cm]

  // State.
  std::vector<double> h_bottom; // [cm]
  std::auto_ptr<IMvec> solute;  // [g/cm^3]
  
  // Utilities.
  /* const */ double dy;                    // [cm]
  std::vector<size_t> column;
  std::vector<double> added_water; // [cm^3]
  std::vector<double> density_column; // [cm^-2]

  // Simulation.
  bool to_drain () const 
  { return false; }
  double air_bottom (const size_t c) const // Lowest point with air [cm]
  { 
    daisy_assert (c < column.size ());
    const size_t col = column[c];
    daisy_assert (col < h_bottom.size ());
    return height_end + h_bottom[col]; 
  }
  void add_water (size_t c, double amount /* [cm^3] */)
  {
    daisy_assert (c < column.size ());
    const size_t col = column[c];
    daisy_assert (col < added_water.size ());
    added_water[col] += amount; 
  }
  void extract_water (const size_t c, const double volume /* [cm^3] */ ,
                      const double Theta /* [cm^3/cm^3] */,
                      const double dt /* [h] */,
                      std::vector<double>& S_drain /* [cm^3/cm^3/h] */,
                      std::vector<double>& S_matrix, Treelog&);
  void release_water (const Geometry& geo, const Soil& soil, 
                      const SoilWater& soil_water,
                      const double dt /* [h] */,
                      std::vector<double>& S_matrix, Treelog&);
  void update_water ();
  void output (Log&) const;

  // Create and Destroy.
  bool initialize (const Geometry& geo, const Scope& scope, double,
                   Treelog& msg);
  bool check (const Geometry& geo, Treelog& msg) const
  { return check_base (geo, msg); }
  BioporeMatrix (Block& al);
};

void 
BioporeMatrix::extract_water (const size_t c, const double volume /* [cm^3] */ ,
                              const double Theta /* [cm^3/cm^3] */,
                              const double dt /* [h] */,
                              std::vector<double>& /* [cm^3/cm^3/h] */,
                              std::vector<double>& S_matrix, Treelog& msg)
{
  std::ostringstream tmp;
  tmp << "Extracting " << Theta << " [] water over " << dt
      << " hours from cell " << c;
  msg.message (tmp.str ());

  daisy_assert (c < S_matrix.size ());
  S_matrix[c] += Theta / dt;
  daisy_assert (c < column.size ());
  const size_t col = column[c];
  daisy_assert (col < h_bottom.size ());
  added_water[col] += Theta * volume;
}

void 
BioporeMatrix::release_water (const Geometry& geo, const Soil& soil, 
                              const SoilWater& soil_water,
                              const double dt /* [h] */,
                              std::vector<double>& S_matrix, Treelog& msg)
{
  const double circumference = M_PI * diameter; // [cm]
  
  const size_t cell_size = geo.cell_size ();
  for (size_t c = 0; c < cell_size; c++)
    { 
      const double dens = density (c); // [cm^-2]
      if (dens < 1e-42)
        // No biopores of this class in this cell.
        continue;

      const double cell_z = geo.cell_z (c); // [cm]
      if (cell_z > height_start || cell_z < height_end)
        // Outside interval.
        continue;

      const double h_biopore =  air_bottom (c) - cell_z; // [cm]
      if (h_biopore < 1e-1)
        // No water significant water in biopore.
        continue;
      const double h_matrix = soil_water.h (c); // [cm]
      if (h_biopore < h_matrix + 1e-5)
        // Pressure in biopore not significantly above pressure in matrix.
        continue;
      const double dh = h_biopore - h_matrix; // [cm]
      daisy_assert (dh > 0.0);

      // Find resistance.
      const Secondary& secondary = soil.secondary_domain (c);
      const bool use_primary = secondary.none ();
      const double R = use_primary ? R_primary : R_secondary; // [h]
      
      // Find sink
      const double wall_per_area = circumference * dens; // [cm^-1]
      
      const double S = dh * wall_per_area / R; // [h^-1]

      std::ostringstream tmp;
      tmp << "Releasing " << S * dt << " [] water over " << dt
          << " hours in cell " << c
          << "\nz = " << cell_z << ", h_matrix = " << h_matrix 
          << ", h_biopore = " << h_biopore << ", dh = " << dh
          << "\ncircumference = " << circumference << ", dens = " << dens
          << ", wall/area = " << wall_per_area << ", R = " << R;
      msg.message (tmp.str ());

      S_matrix[c] -= S;
      const double volume = geo.cell_volume (c); // [cm^3]
      added_water[column[c]] -= S * volume * dt; // [cm^3]
    }
}

void
BioporeMatrix::update_water ()
{ 
  const size_t column_size = xplus.size ();
  daisy_assert (added_water.size () == xplus.size ());
  double xminus = 0.0;
  for (size_t i = 0; i < column_size; i++)
    {
      const double density = density_column[i]; // [cm^-2]
      const double radius = diameter * 0.5;     // [cm]
      const double area = M_PI * radius * radius;  // [cm^2]
      const double soil_fraction = density * area; // []
      const double water_volume = added_water[i];  // [cm^3]
      const double soil_volume = water_volume / soil_fraction; // [cm^3]
      const double dx = xplus[i] - xminus;                     // [cm]
      const double dz = soil_volume / (dx * dy);               // [cm]
      h_bottom[i] += dz;                                          // [cm]
      added_water[i] = 0.0;                                    // [cm^3]
      xminus = xplus[i];                                       // [cm]
    }
}

void
BioporeMatrix::output (Log& log) const
{
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

  double xminus = 0;
  for (size_t i = 0; i < column_size; i++)
    {
      VolumeBox square ("square", height_end, height_start, xminus, xplus[i]);
      const double volume = square.volume ();
      daisy_assert (volume > 0.0);
      const double content = geo.total_soil (density_cell, square);
      static const double m2_per_cm2 = 0.01 * 0.01;
      const double density = m2_per_cm2 * content / volume;
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
    diameter (al.number ("diameter")),
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
    syntax.add ("diameter", "cm", Check::positive (),
                Syntax::Const, "Biopore diameter.");
    syntax.add ("R_primary", "h", Check::positive (), Syntax::Const, "\
Resistance for water moving from biopore through wall to primary domain.");
    syntax.add ("R_secondary", "h", Check::positive (), 
                Syntax::OptionalConst, "\
Resistance for water moving from biopore through wall to secondary domain.\n\
If not specified, this will be identical to 'R_primary'.");
    syntax.add ("h_bottom", "cm", Syntax::OptionalState, Syntax::Sequence,
		"Pressure at the bottom of the biopores in each interval.");

    static const symbol C_unit ("g/cm^3");
    IMvec::add_syntax (syntax, alist, Syntax::OptionalState, "solute", C_unit,
                       "Chemical concentration in biopore intervals.");

    Librarian::add_type (Biopore::component, "matrix", alist, syntax, &make);
  }
} BioporeMatrix_syntax;

// biopore_matrix.C ends here.
