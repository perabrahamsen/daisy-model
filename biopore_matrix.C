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
#include "groundwater.h"
#include <sstream>

// The 'matrix' model.

struct BioporeMatrix : public Biopore
{
  // Parameters.
  /* const */ std::vector<double> xplus; // [cm]
  const double R_primary;       // [h/cm]
  const double R_secondary;     // [h/cm]
  const int debug;

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
  
  // Log variable.
  int iterations;

  // Utilities.
  /* const */ double dy;                    // [cm]
  std::vector<size_t> column;
  std::vector<double> added_water; // [cm^3]
  std::vector<double> density_column; // [cm^-2]

  double height_to_volume (const size_t col, const double dz /* [h] */) const;
  double volume_to_height (size_t col, double volume /* [cm^3] */) const;
  double air_bottom (const size_t c) const // Lowest point with air [cm]
  { 
    daisy_assert (c < column.size ());
    const size_t col = column[c];
    daisy_assert (col < h_bottom.size ());
    return height_end + h_bottom[col]; 
  }
  double total_water () const;

  // Simulation.
  double capacity (const Geometry& geo, size_t e, const double dt /* [h] */)
    /* [cm] */ const;
  void infiltrate (const Geometry&, size_t e, double amount /* [cm] */);

  double matrix_biopore_matrix (size_t c, const Geometry& geo, 
                                const Soil& soil, bool active, 
                                const double h_barrier, double M_c,
                                double pressure_end, 
                                double K_xx, double h3_bottom, double h) const;
  double matrix_biopore_drain (size_t c, const Geometry& geo, 
                               const Soil& soil, bool active, 
                               double K_xx, double h) const
  {return 0.0;}
  void find_matrix_sink (const Geometry& geo, const Soil& soil,  
                         const SoilHeat& soil_heat, 
                         const std::vector<bool>& active,
                         const double h_barrier,
                         const double pressure_initiate,
                         const double pressure_end,
                         const std::vector<double>& h3_bottom, 
                         const std::vector<double>& h, 
                         const double dt,
                         std::vector<double>& S3) const;
  void update_matrix_sink (const Geometry& geo,    
                           const Soil& soil,  
                           const SoilHeat& soil_heat, 
                           const std::vector<bool>& active,
                           const double h_barrier,
                           const size_t max_iterations,
                           const double max_absolute_difference,
                           const double max_relative_difference,
                           const double pressure_initiate,
                           const double pressure_end,
                           const std::vector<double>& h,
                           const double dt);
  void add_water (size_t c, double amount /* [cm^3] */);
  void update_water ();
  double column_water (const size_t col) const;
  void add_to_sink (std::vector<double>& S_matrix, std::vector<double>&) const;
  void add_solute (symbol chem, size_t cell, const double amount /* [g] */);
  void matrix_solute (const Geometry& geo, const double dt, 
                      const Chemical& chemical, std::vector<double>& source_chem,
                      Treelog& msg);
  void output (Log&) const;

  // Create and Destroy.
  bool initialize (const Units&, 
                   const Geometry& geo, const Scope& scope, 
                   const Groundwater& groundwater,
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

double
BioporeMatrix::height_to_volume (const size_t col,
                                const double dz /* [h] */) const
{
  daisy_assert (col < xplus.size ());
  const double xminus = (col == 0) ? 0.0 : xplus[col-1]; // [cm]
  const double dx = xplus[col] - xminus;      // [cm]
  const double soil_volume = dz * dx * dy;    // [cm^3]
  const double density = density_column[col]; // [cm^-2]
  const double radius = diameter * 0.5;     // [cm]
  const double area = M_PI * radius * radius;  // [cm^2]
  const double soil_fraction = density * area; // []
  const double water_volume = soil_volume * soil_fraction; // [cm^3]
  return water_volume;
}

double 
BioporeMatrix::volume_to_height (const size_t col,
                                 const double water_volume /* [cm^3] */) const
{
  daisy_assert (col < xplus.size ());
  const double density = density_column[col]; // [cm^-2]
  if (!std::isnormal (density))
    {
      // No biopores here.
      daisy_assert (iszero (water_volume));
      return 0.0;
    }

  const double xminus = col == 0 ? 0.0 : xplus[col-1];
  const double radius = diameter * 0.5;       // [cm]
  const double area = M_PI * radius * radius; // [cm^2]
  const double soil_fraction = density * area;             // []
  const double soil_volume = water_volume / soil_fraction; // [cm^3]
  const double dx = xplus[col] - xminus;                   // [cm]
  const double dz = soil_volume / (dx * dy);               // [cm]
  return dz;
}

double 
BioporeMatrix::total_water () const
{
  const size_t col_size = xplus.size ();
  double total = 0.0;
  for (size_t col = 0; col < col_size; col++)
    total += height_to_volume (col, h_bottom[col]);
  return total;
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
                                      const double h_barrier, double M_c,
                                      const double pressure_end,
                                      const double K_xx, 
                                      const double h3_bottom, 
                                      const double h) const
{
  if (!std::isnormal (density (c)))
    // No biopores here.
    return 0.0;

  const Secondary& secondary = soil.secondary_domain (c);
  const bool use_primary = secondary.none ();
  const double R_wall = use_primary ? R_primary : R_secondary; // [h]  
  const double r_c = diameter / 2.0; // [cm]
  const double cell_z = geo.cell_z (c); // [cm]
  const double z_air = height_end + h3_bottom;
  const double h3_cell = std::max (z_air - cell_z, pressure_end); // [cm]
  const double low_point = geo.cell_bottom (c); // [cm]
  const double h3_min = low_point - cell_z; // [cm]
  daisy_assert (h3_min < 0.0);

  double S; 
  if (h3_bottom > 0.0 && h3_cell>h3_min && h3_cell>h + h_barrier)
    {
      const double high_point = geo.cell_top (c);
      double wall_fraction;
      if (z_air < high_point)
        wall_fraction = (z_air - low_point) /(high_point - low_point);
      else 
        wall_fraction = 1.0;      
      S = - wall_fraction * biopore_to_matrix (R_wall, M_c, r_c, h, h3_cell);
    }
  else if (active && h>h3_cell + h_barrier)
    S = matrix_to_biopore (K_xx, M_c, r_c, h, h3_cell);
  else 
    S = 0.0;
  return S;
}

void
BioporeMatrix::find_matrix_sink (const Geometry& geo,    
                                 const Soil& soil,  
                                 const SoilHeat& soil_heat, 
                                 const std::vector<bool>& active,
                                 const double h_barrier,
                                 const double pressure_initiate,
                                 const double pressure_end,
                                 const std::vector<double>& h3_bottom, 
                                 const std::vector<double>& h, 
                                 const double dt,
                                 std::vector<double>& S3) const
{
  // Find sink terms per cell.
  const size_t cell_size = geo.cell_size ();
  for (size_t c = 0; c < cell_size; c++)
    {
      const double h_cond = std::min(pressure_initiate, h[c]);
      const double T = soil_heat.T (c);
      const double h_ice = 0.0;    //ice ignored 
      const double K_zz = soil.K (c, h_cond, h_ice, T);
      const double K_xx = K_zz * soil.anisotropy (c);
      const size_t col = column[c];
      const double M_c = density_column[col]; // [cm^-2]
      S3[c] = matrix_biopore_matrix (c, geo, soil, active[c], h_barrier, M_c,
                                     pressure_end, K_xx, h3_bottom[col], h[c]);
    }
}

void
BioporeMatrix::update_matrix_sink (const Geometry& geo,    
                                   const Soil& soil,  
                                   const SoilHeat& soil_heat, 
                                   const std::vector<bool>& active,
                                   const double h_barrier,
                                   const size_t max_iterations,
                                   const double max_absolute_difference,
                                   const double max_relative_difference,
                                   const double pressure_initiate,
                                   const double pressure_end,
                                   const std::vector<double>& h, 
                                   const double dt)
{
  // Find initial guess of sink terms per cell, plus the coresponding added
  // removed water volume per column.
  const double h_capacity = height_start - height_end;
  const size_t col_size = xplus.size ();
  const size_t cell_size = geo.cell_size ();

  // Find estimate based on highest pressured active cell.
  std::vector<double> h3_soil (col_size, 0.0);
  for (size_t c = 0; c < cell_size; c++)
    {
      if (!active[c])
        // Only active cells may participate.
        continue;

      const size_t col = column[c];
      const double soil_cell = geo.cell_z (c) + h[c];
      h3_soil[col] = bound (h3_soil[col], soil_cell - height_end, h_capacity);
    }
  
  // Initial guess and interval.
  std::vector<double> h3_min (col_size);
  std::vector<double> h3_max (col_size);
  std::vector<double> guess (col_size);
  const double soil_weight = 0.5;      // Relative weight of soil and history.
  const double history_weight = 1.0 - soil_weight;
  for (size_t col = 0; col < col_size; col++)
    {
      h3_min[col] = std::min (h_bottom[col], h3_soil[col]);
      h3_max[col] = std::max (h_bottom[col], h3_soil[col]);
      guess[col] = soil_weight * h3_soil[col] + history_weight * h_bottom[col];
    }

  // Main iteration loop.
  for (iterations = 0; iterations < max_iterations; iterations++)
    {
      find_matrix_sink (geo, soil, soil_heat, active,
                        h_barrier, pressure_initiate, pressure_end,
                        guess, h, dt, S);
      
      // Added water volume.
      std::vector<double> water (col_size, 0.0);
      for (size_t c = 0; c < cell_size; c++)
        {
          const size_t col = column[c];
          water[col] += S[c] * dt * geo.cell_volume (c);
        }

      double max_abs = 0.0;
      double max_rel = 0.0;

      // New value.
      std::vector<double> value = h_bottom;

      for (size_t col = 0; col < col_size; col++)
        {
          // Add S term.
          value[col] += volume_to_height (col, water[col]);

          if (debug > 0 && std::isnormal (water[col]))
            {
              std::ostringstream tmp;
              tmp << iterations << ":" << col << " min = " << h3_min[col] 
                  << ", max = " << h3_max[col] << ", guess = " << guess[col] 
                  << ", value = " << value[col]
                  << ", bottom = " << h_bottom[col]
                  << ", abs = " << max_abs << ", rel = " << max_rel;
              Assertion::message (tmp.str ());
            }

          // Adjust interval and find new guess.
          if (value[col] > guess[col])
            {
              h3_min[col] = guess[col];
              
              if (value[col] > h3_max[col])
                guess[col] = (h3_min[col] + h3_max[col] * 3) / 4.0;
              else
                guess[col] = value[col];
            }
          else if (value[col] < guess[col])
            {
              h3_max[col] = guess[col];

              if (value[col] < h3_min[col])
                guess[col] = (h3_min[col] * 3 + h3_max[col]) / 4.0;
              else
                guess[col] = value[col];
            }
          else 
            // We found it! 
            h3_min[col] = h3_max[col] = guess[col];

          // We check old differences, so that S will be no worse that that.
          const double h3_diff = h3_max[col] - h3_min[col];
          daisy_assert (h3_diff >= 0.0);
          max_abs = std::max (max_abs, h3_diff);
          if (h3_max[col] > max_absolute_difference)
            max_rel = std::max (max_rel, h3_diff / h3_max[col]);
        }
      
      // Did we find a solution?
      if (max_rel < max_relative_difference 
          || max_abs < max_absolute_difference)
        break;
    }
  
  // Try to make sure we have something to scale.
  for (size_t col = 0; col < col_size; col++)
    {
      if (guess[col] < h_bottom[col])
        guess[col] = h3_max[col];
      else if (guess[col] > h_bottom[col])
        guess[col] = h3_min[col];
      
      find_matrix_sink (geo, soil, soil_heat, active,
                        h_barrier, pressure_initiate, pressure_end, 
                        guess, h, dt, S);
    }

  // Find added and removed water.
  std::vector<double> vol_added (col_size, 0.0);
  std::vector<double> vol_removed (col_size, 0.0);
  for (size_t c = 0; c < cell_size; c++)
    {
      const double vol = S[c] * dt * geo.cell_volume (c);
      const size_t col = column[c];
      if (S[c] > 0.0)
        vol_added[col] += vol;
      else
        vol_removed[col] -= vol;
    }

  // Find number to multiply sink and sources with, in order to avoid
  // that the macropores underflow or overflow.
  std::vector<double> scale_added (col_size, 1.0);
  std::vector<double> scale_removed (col_size, 1.0);
  bool do_scale = false;
  const double h_epsilon = 1e-10;
  for (size_t col = 0; col < col_size; col++)
    {
      const double h_added = volume_to_height (col, vol_added[col]);
      daisy_assert (h_added >= 0.0);
      const double h_removed = volume_to_height (col, vol_removed[col]);
      daisy_assert (h_removed >= 0.0);
      const double h_new = h_bottom[col] + h_added - h_removed;

      // Too little?
      if (h_new < h3_min[col] - h_epsilon && h_removed > h_epsilon)
        {
          // Solve h_bottom + h_added - scale * h_removed = h3_min
          scale_removed[col]
            = (h_bottom[col] + h_added - h3_min[col]) / h_removed;
          daisy_assert (std::isfinite (scale_added[col]));
          if (scale_removed[col] > 1.0)
            {
              std::ostringstream tmp;
              tmp << "scale[" << col << "] removed = " << scale_removed[col] 
                  << "; max = " << h3_max[col] << "; min = " << h3_min[col]
                  << "; added = " << h_added << "; removed = " << h_removed
                  << "; old = " << h_bottom[col] << "; new = " << h_new;
              daisy_bug (tmp.str ());
              scale_removed[col] = 1.0;
            }
          daisy_assert (scale_removed[col] >= 0.0);
          do_scale = true;
        }
      else if (h_new > h3_max[col] + h_epsilon && h_added > h_epsilon)
        {
          // Solve h_bottom + scale * h_added - h_removed = h3_max
          scale_added[col]
            = (h3_max[col] + h_removed - h_bottom[col]) / h_added;
          daisy_assert (std::isfinite (scale_added[col]));
          if (scale_added[col] > 1.0)
            {
              std::ostringstream tmp;
              tmp << "scale[" << col << "] added = " << scale_added[col] 
                  << "; max = " << h3_max[col] << "; min = " << h3_min[col]
                  << "; added = " << h_added << "; removed = " << h_removed
                  << "; old = " << h_bottom[col] << "; new = " << h_new;
              daisy_bug (tmp.str ());
              scale_added[col] = 1.0;
            }
          daisy_assert (scale_added[col] >= 0.0);
          do_scale = true;
        }

      // Update added water.
      daisy_assert (iszero (added_water[col]));
      added_water[col] = vol_added[col] * scale_added[col]
        - vol_removed[col] * scale_removed[col];
    }

  if (!do_scale)
    return;

  // Now scale the sink terms.
  for (size_t c = 0; c < cell_size; c++)
    {
      const size_t col = column[c];
      if (S[c] > 0.0)
        S[c] *= scale_added[col];
      else
        S[c] *= scale_removed[col];
    }

  // Test results.
  const double total_S = geo.total_soil (S) * dt;
  double total_water = 0.0;
  for (size_t col = 0; col < col_size; col++)
    {
      total_water += added_water[col];
      if (debug > 0 && std::isnormal (added_water[col]))
        {
          std::ostringstream tmp;
          tmp << "after: " << col << " min = " << h3_min[col] 
              << ", max = " << h3_max[col] << ", guess = " << guess[col] 
              << ", added " << volume_to_height (col, added_water[col]) 
              << " [cm], dt = " << dt << " [h]";
          Assertion::message (tmp.str ());
        }
    }
  if (!approximate (total_S, total_water) && std::fabs (total_water) > 1e-10)
    {
      std::ostringstream tmp;
      tmp << "S " << total_S << " [cm^3] != added " << total_water
          << " [cm^3]";
      daisy_bug (tmp.str ());
    }
}

void 
BioporeMatrix::add_water (const size_t c, const double amount /* [cm^3] */)
{
  daisy_assert (c < column.size ());
  const size_t col = column[c];
  daisy_assert (col < added_water.size ());
  added_water[col] += amount; 
}

void
BioporeMatrix::update_water ()
{ 
  const size_t col_size = xplus.size ();
  daisy_assert (added_water.size () == col_size);
  daisy_assert (h_bottom.size () == col_size);
  for (size_t col = 0; col < col_size; col++)
    {
      h_bottom[col] += volume_to_height (col, added_water[col]);
      added_water[col] = 0.0;
    }
}

double
BioporeMatrix::column_water (const size_t col) const
{ return height_to_volume (col, h_bottom[col]); }

void 
BioporeMatrix::add_to_sink (std::vector<double>& S_matrix,
                            std::vector<double>&) const
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
                              std::vector<double>& source_chem,
                              Treelog& msg)
{
  TREELOG_MODEL (msg);
  const symbol chem = chemical.name;
  const size_t cell_size = geo.cell_size ();
  daisy_assert (source_chem.size () == cell_size);
  std::vector<double> sink_chem (cell_size, 0.0);

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
      sink_chem[c] = water_sink * C; // [g/cm^3 S/h]

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
      sink_chem[c] = water_sink * C;
    }

  // Remove from biopores.
  for (size_t c = 0; c < cell_size; c++)
    {
      if (sink_chem[c] >= 0.0)
        continue;

      const double cell_volume = geo.cell_volume (c); // [cm^3 S]
      const double M = sink_chem[c] * cell_volume * dt;  // [g]
      add_solute (chem, c, M);
    }

  // Export.
  for (size_t c = 0; c < cell_size; c++)
    source_chem[c] -= sink_chem[c];
}

void
BioporeMatrix::output (Log& log) const
{
  output_base (log);
  output_variable (h_bottom, log);
  output_submodule (*solute, "solute", log);
  output_lazy (total_water (), "water", log);
  output_variable (iterations, log);
}

bool 
BioporeMatrix::initialize (const Units& units,
                           const Geometry& geo, const Scope& scope, 
                           const Groundwater& groundwater, Treelog& msg)
{ 
  bool ok = true;

  // base.
  if (!initialize_base (units, geo, scope, msg))
    ok = false;

  // xplus.
  if (xplus.size () == 0)
    geo.fill_xplus (xplus);
  const size_t column_size = xplus.size ();
  daisy_assert (column_size > 0);

  // h_bottom.
  if (h_bottom.size () == 0)
    {
      double z_bottom = height_end;
      switch (groundwater.bottom_type ())
        {
        case Groundwater::pressure:
        case Groundwater::lysimeter:
          z_bottom = groundwater.table ();
          break;
        case Groundwater::forced_flux:
          if (groundwater.is_pipe ())
            z_bottom = groundwater.pipe_height ();
          break;
        case Groundwater::free_drainage:
          break;
        default:
          msg.error ("Unsupported groundwater model '" 
                     + groundwater.name + "'");
          ok = false;
        }
      const double h_bottom_start 
        = bound (0.0, z_bottom - height_end, height_start - height_end);
      h_bottom.insert (h_bottom.end (), column_size, h_bottom_start);
    }

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
    debug (al.integer ("debug")),
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
    syntax.add ("debug", Syntax::Integer, Syntax::Const, "Debug level.\n\
Increase value to get more debug message.");
    alist.add ("debug", 0);
    syntax.add ("h_bottom", "cm", Syntax::OptionalState, Syntax::Sequence,
                "Pressure at the bottom of the biopores in each interval.");
    static const symbol C_unit ("g");
    IMvec::add_syntax (syntax, alist, Syntax::OptionalState, "solute", C_unit,
                       "Chemical concentration in biopore intervals.");
    syntax.add ("water", "cm^3", Syntax::LogOnly, "Water content.");    
    syntax.add ("iterations", Syntax::Integer, Syntax::LogOnly, 
                "Number of iterations used for finding a solution.");
      
    Librarian::add_type (Biopore::component, "matrix", alist, syntax, &make);
  }
} BioporeMatrix_syntax;

// biopore_matrix.C ends here.
