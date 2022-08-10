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
#include "im.h"
#include "block_model.h"
#include "vcheck.h"
#include "librarian.h"
#include "submodeler.h"
#include "geometry.h"
#include "soil.h"
#include "volume_box.h"
#include "log.h"
#include "check.h"
#include "anystate.h"
#include "chemical.h"
#include "groundwater.h"
#include "treelog.h"
#include "frame.h"
#include "mathlib.h"
#include "metalib.h"
#include "library.h"
#include <sstream>
#include <numeric>

// The 'matrix' model.

struct BioporeMatrix : public Biopore
{
  const Metalib& metalib;
  
  // Parameters.
  /* const */ std::vector<double> xplus; // [cm]
  const double K_wall_relative;       // []
  const int debug;
  const int max_iterations;     // Convergence.
  const double max_absolute_difference; // [cm]
  const double max_relative_difference; // []
  const bool allow_upward_flow;

  // State.
  std::vector<double> h_bottom; // [cm]
  IMvec solute;			// [g]
  struct MyContent : public Anystate::Content
  {
    std::vector<double> h_bottom;
    std::unique_ptr<Anystate::Content> clone () const
    { 
      std::unique_ptr<Anystate::Content> copy (new MyContent (h_bottom)); 
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
  std::vector<double> h3_soil;
  std::vector<double> z3_lowest;
  std::vector<double> Theta;    // [cm^3/cm^3], cell based water content.
  IMvec M;			// [g/cm^3], cell based solute content.

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
  double total_solute (const Geometry& geo, 
		       const symbol chem) const //[g/m^2]
  {
    double total = 0.0;		// [g]
    Library& library = metalib.library (Chemical::component);
    std::set<symbol> found;
    for (auto i: solute)
      if (library.is_derived_from (i, chem))
	{
	  const std::vector<double>& solute_array = solute.get_array (i);
	  total += std::accumulate (solute_array.begin (), solute_array.end (),
				    0.0);
	}
    return total / geo.surface_area ()
      * 10000.0; // [cm^2/m^2]
  }
  void get_solute (IM&) const;

  // Simulation.
  double infiltration_capacity (const Geometry& geo, size_t e,
                                const double dt /* [h] */)
    /* [cm] */ const;

  void infiltrate (const Geometry&, size_t e, double amount /* [cm] */,
                   double dt /* [h] */);
  void solute_infiltrate (const symbol chem, 
                          const Geometry& geo, const size_t e,
                          const double amount /* [g] */, 
                          const double dt);

  double matrix_biopore_matrix (size_t c, const Geometry& geo, 
                                bool active, 
                                const double h_barrier, double M_c,
                                double pressure_limit, 
                                double K_xx, double K_crack,
                                double z3_lowest, double h3_bottom, 
                                double h) const;
  double matrix_biopore_drain (size_t c, const Geometry& geo, 
                               const Soil& soil, bool active, 
                               double K_xx, double h) const
  {return 0.0;}
  void find_matrix_sink (const Geometry& geo, 
                         const std::vector<bool>& active,
                         const std::vector<double>& K, 
                         const std::vector<double>& K_crack, 
                         const double h_barrier,
                         const double pressure_limit,
                         const std::vector<double>& h3_bottom, 
                         const std::vector<double>& h, 
                         std::vector<double>& S3) const;
  void forward_sink (const Geometry& geo,    
                     const std::vector<bool>& active,
                     const std::vector<double>& K, 
                     const std::vector<double>& K_crack, 
                     const double h_barrier,
                     const double pressure_limit,
                     const std::vector<double>& h, 
                     std::vector<double>& S3) const;
  void tick_source (const Geometry& geo, const std::vector<bool>& active,
                    const std::vector<double>& h);
  void update_matrix_sink (const Geometry& geo,    
                           const std::vector<bool>& active,
                           const std::vector<double>& K,
                           const std::vector<double>& K_crack,
                           const double h_barrier,
                           const double pressure_limit,
                           const std::vector<double>& h,
                           const double dt);
  void add_water (size_t c, double amount /* [cm^3] */);
  void update_water ();
  double column_water (const size_t col) const;
  void update_cell_water (const Geometry&, const double dt);
  void update_soil_tertiary (std::vector<double>& Theta_p,
                             std::vector<double>& q_p);
  void add_to_sink (std::vector<double>& S_B2M, 
                    std::vector<double>& S_M2B, 
                    std::vector<double>&,
                    std::vector<double>&) const;
  void add_solute (symbol chem, size_t cell, const double amount /* [g] */);
  void remove_solute (const symbol chem);
  void matrix_solute (const Geometry& geo, const double dt, 
                      Chemical& chemical, Treelog& msg);
  void update_cell_solute (const Geometry& geo, const symbol chem, 
			   const double dt);

  void output (Log&) const;

  // Create and Destroy.
  bool initialize (const Units&, 
                   const Geometry& geo, const Scope& scope, 
                   const Groundwater& groundwater,
                   Treelog& msg);
  bool check (const Geometry& geo, Treelog& msg) const
  { return check_base (geo, msg); }
  BioporeMatrix (const BlockModel& al);
};

Anystate
BioporeMatrix::get_state () const
{
  std::unique_ptr<Anystate::Content> copy (new MyContent (h_bottom));
  return Anystate (std::move (copy));
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
  daisy_assert (density > 0.0);

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

void
BioporeMatrix::get_solute (IM& im) const
{
  const Unit& my_unit = solute.unit ();
  const size_t col_size = xplus.size ();
  for (IMvec::const_iterator i = solute.begin (); i != solute.end (); ++i)
    {
      const symbol chem = *i;
      const std::vector<double>& array = solute.get_array (chem);
      daisy_assert (array.size () <= col_size);
      const double sum = std::accumulate (array.begin (), array.end (), 0.0);
      im.add_value (chem, my_unit, sum);
    }
}

double 
BioporeMatrix::infiltration_capacity (const Geometry& geo, size_t e,
                                      const double dt) const 
{
  // Any macropores that reach the surface.
  if (height_start < 0.0)
    return 0.0;

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
  const double max_capacity = height * area * density; // [cm]

  if (debug > 0)
    {
      std::ostringstream tmp;
      tmp << "max_inf = " << max_infiltration 
          << " [cm], max_cap = " << max_capacity << " [cm]";
      Assertion::message (tmp.str ());
    }

  // Choose the lower limit;
#if 0
  daisy_assert (max_infiltration >= 0); // May fail due to numerics.
  daisy_assert (max_capacity >= 0);
#endif
  return std::max (std::min (max_infiltration, max_capacity),
		   0.0);
}

void 
BioporeMatrix::infiltrate (const Geometry& geo, const size_t e,
                           const double amount /* [cm] */, const double dt)
{ 
  Biopore::infiltrate (geo, e, amount, dt);
  const size_t cell = geo.edge_other (e, Geometry::cell_above);
  daisy_assert (cell < geo.cell_size ());
  daisy_assert (cell < column.size ());
  const double area = geo.edge_area (e);
  add_water (cell, area * amount);
}

void 
BioporeMatrix::solute_infiltrate (const symbol chem, 
                                  const Geometry& geo, const size_t edge,
                                  const double amount /* [g] */, 
                                  const double dt)
{ 
  if (!iszero (amount) && debug > 0)
    {
      std::ostringstream tmp;
      tmp << "Infiltrate " << amount << " g '" << chem 
          << "' through " << geo.edge_name (edge);
      Assertion::message (tmp.str ()); 
    }
  Biopore::solute_infiltrate (chem, geo, edge, amount, dt);
  const size_t cell = geo.edge_other (edge, Geometry::cell_above);
  add_solute (chem, cell, amount);
}

double 
BioporeMatrix::matrix_biopore_matrix (size_t c, const Geometry& geo, 
                                      const bool active, 
                                      const double h_barrier, double M_c,
                                      const double pressure_limit,
                                      const double K_xx, const double K_crack,
                                      const double z3_lowest,
                                      const double h3_bottom, 
                                      const double h) const
{
  if (!std::isnormal (density (c)))
    // No biopores here.
    return 0.0;

  // The radius of the biopores.
  const double r_c = diameter / 2.0; // [cm]

  // The height above ground of the center of the cell (negative).
  const double cell_z = geo.cell_z (c); // [cm]

  // The height above ground of the top of the water in the biopore (negative).
  const double z_air = height_end + h3_bottom;

  // The pressure in the biopore at the middle of the cell.
  const double h3_cell = z_air - cell_z; // [cm]

  // The height of the bottom of the cell, above ground (negative).
  const double cell_bottom = geo.cell_bottom (c);

  // The above ground height of the lowest point of the cell with macropores.
  const double low_point = std::max (height_end, cell_bottom); // [cm]
  
  // To move water from the biopore to matrix, it is not enough that
  // the pressure is higher in the biopore.  It also needs to be
  // positive, at least in part of the cell.  The 'h3_min' value is
  // the lowest value of 'h3_cell' that corresponds to a water level
  // in the biopore that overlaps part of the cell.
  const double h3_min = low_point - cell_z; // [cm]
  // 'h3_min' will usually be negative, but if the macropore domain
  // ends above the middle of the cell.

  // Now we find the source/sink term S.  Positive S here denotes a
  // sink in the matrix domain, but a source in the tertiary domain.
  double S; 
  if (h3_bottom > 0.0 && h3_cell>h3_min && h3_cell>h + h_barrier)
    {
      const double cell_top = geo.cell_top (c);
      const double high_point = std::min (height_start, cell_top);
      const double wall_top = std::min (z_air, high_point);
      const double wall_fraction
        = (wall_top - low_point) / (cell_top - cell_bottom);
      daisy_assert (std::isfinite (wall_fraction));

      // The resistence to be overcome for water leaving the biopore is
      // different in the primary and secondary domain, in general we
      // assume the water will have a much easier time leaving the
      // biopores if the soil has cracks.
      const double S1 = - wall_fraction * biopore_to_primary (K_xx, 
                                                              K_wall_relative,
                                                              M_c, r_c,
                                                              h, h3_cell);
      daisy_assert (std::isfinite (S1));
      const double S2 = (K_crack < 0.0)
        ? 0.0
        : - wall_fraction * biopore_to_secondary (K_crack, M_c, r_c, h3_cell);
      daisy_assert (std::isfinite (S2));
      S = std::min (S1, S2);

#if 0 // The matrix can be drier than pF 6.
      if (h < -100000)
        {
          std::ostringstream tmp;
          tmp << "S1 = " << S1
              << "\nS2  = " << S2
              << "\nS  = " << S
              << "\nK_xx  = " << K_xx
              << "\nK_crack  = " << K_crack
              << "\nK_wall_relative  = " << K_wall_relative
              << "\nwall_fraction  = " << wall_fraction
              << "\nM_c  = " << M_c
              << "\nr_c  = " << r_c
              << "\nh  = " << h
              << "\nh3_cell  = " << h3_cell;
          Assertion::message (tmp.str ());
        }
#endif
    }
  else if ((allow_upward_flow || cell_z > z3_lowest) 
           && active && h>h3_cell + h_barrier)
    {
      // The largest pressure gradient between the domains are
      // pressure_limit, above that we claim air will disrupt the suction.
      const double h3_suck = std::max (h3_cell, h + pressure_limit);
      S = matrix_to_biopore (K_xx, M_c, r_c, h, h3_suck)
        * geo.fraction_in_z_interval (c, height_start, height_end);
      daisy_assert (std::isfinite (S));
    }
  else 
    S = 0.0;
  return S;
}

void
BioporeMatrix::find_matrix_sink (const Geometry& geo,    
                                 const std::vector<bool>& active,
                                 const std::vector<double>& K, 
                                 const std::vector<double>& K_crack, 
                                 const double h_barrier,
                                 const double pressure_limit,
                                 const std::vector<double>& h3_bottom, 
                                 const std::vector<double>& h, 
                                 std::vector<double>& S3) const
{
  // Find sink terms per cell.
  const size_t cell_size = geo.cell_size ();
  for (size_t c = 0; c < cell_size; c++)
    {
      const size_t col = column[c];
      const double M_c = density_column[col]; // [cm^-2]
      daisy_assert (std::isfinite (S3[c]));
      S3[c] += matrix_biopore_matrix (c, geo, active[c], h_barrier, M_c,
                                      pressure_limit, K[c], K_crack[c],
                                      z3_lowest[col], h3_bottom[col], h[c]);
      daisy_assert (std::isfinite (S3[c]));
    }
}

void
BioporeMatrix::forward_sink (const Geometry& geo,    
                             const std::vector<bool>& active,
                             const std::vector<double>& K, 
                             const std::vector<double>& K_crack, 
                             const double h_barrier,
                             const double pressure_limit,
                             const std::vector<double>& h, 
                             std::vector<double>& S3) const
{ find_matrix_sink (geo, active, K, K_crack, h_barrier, pressure_limit, 
                    h_bottom, h, S3); }


void
BioporeMatrix::tick_source (const Geometry& geo, 
                            const std::vector<bool>& active,
                            const std::vector<double>& h)
{
  const size_t col_size = xplus.size ();
  const size_t cell_size = geo.cell_size ();

  // Find lowest unsaturated cell in each column.
  daisy_assert (z3_lowest.size () == col_size);
  std::fill (z3_lowest.begin (), z3_lowest.end (), 0.0);
  for (size_t c = 0; c < cell_size; c++)
    {
      if (h[c] > -0.01)
        // Saturated.
        continue;

      const size_t col = column[c];
      const double z = geo.cell_z (c);
      if (z < z3_lowest[col])
        z3_lowest[col] = z;
    }

  // Find estimate based on highest pressured active cell.
  const double h_capacity = height_start - height_end;
  daisy_assert (h3_soil.size () == col_size);
  std::fill (h3_soil.begin (), h3_soil.end (), 0.0);
  for (size_t c = 0; c < cell_size; c++)
    {
      if (!active[c])
        // Only active cells may participate.
        continue;

      const size_t col = column[c];
      const double soil_cell = geo.cell_z (c) + h[c];
      h3_soil[col] = bound (h3_soil[col], soil_cell - height_end, h_capacity);
    }
}

void
BioporeMatrix::update_matrix_sink (const Geometry& geo,    
                                   const std::vector<bool>& active,
                                   const std::vector<double>& K, 
                                   const std::vector<double>& K_crack, 
                                   const double h_barrier,
                                   const double pressure_limit,
                                   const std::vector<double>& h, 
                                   const double dt)
{
  // Find initial guess of sink terms per cell, plus the coresponding added
  // removed water volume per column.
  const size_t col_size = xplus.size ();
  const size_t cell_size = geo.cell_size ();

  daisy_assert (S.size () == cell_size);

  // Initial guess and interval.
  std::vector<double> h3_min (col_size, 0.0);
  std::vector<double> h3_max (col_size);
  std::vector<double> guess (col_size);
  const double soil_weight = 0.5;      // Relative weight of soil and history.
  const double history_weight = 1.0 - soil_weight;
  for (size_t col = 0; col < col_size; col++)
    {
      // h3_min[col] = std::min (h_bottom[col], h3_soil[col]);
      h3_max[col] = std::max (h_bottom[col], h3_soil[col]);
      guess[col] = soil_weight * h3_soil[col] + history_weight * h_bottom[col];
    }

  // Set to true if we want our convergence criteria based on
  // bisection interval, rather than old and new value for one step.  
  const bool converge_interval = true;
  std::vector<double> old_value = h_bottom;

  // Main iteration loop.
  for (iterations = 0; iterations < max_iterations; iterations++)
    {
      std::fill (S.begin (), S.end (), 0.0);
      find_matrix_sink (geo, active, K, K_crack,
                        h_barrier, pressure_limit,
                        guess, h, S);
      
      // Added water volume.
      std::vector<double> water (col_size, 0.0);
      for (size_t c = 0; c < cell_size; c++)
        {
          const size_t col = column[c];
          water[col] += S[c] * dt * geo.cell_volume (c);
        }

      double max_abs = 0.0;
      double max_rel = 0.0;

      // New guess.
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

          // Maximum number of iterations with one step before
          // switching to pure bisection.
          const int max_one_step = 5;

          // Adjust interval and find new guess.
          if (value[col] > guess[col])
            {
              h3_min[col] = guess[col];
              
              if (iterations > max_one_step || value[col] > h3_max[col])
                guess[col] = (h3_min[col] + h3_max[col] * 3) / 4.0;
              else
                guess[col] = value[col];
            }
          else if (value[col] < guess[col])
            {
              h3_max[col] = guess[col];

              if (iterations > max_one_step || value[col] < h3_min[col])
                guess[col] = (h3_min[col] * 3 + h3_max[col]) / 4.0;
              else
                guess[col] = value[col];
            }
          else 
            // We found it! 
            h3_min[col] = h3_max[col] = guess[col];

          // We check old differences, so that S will be no worse that that.
          const double h3_diff = h3_max[col] - h3_min[col];
          daisy_assert (std::isfinite (h3_diff));
          if (h3_diff < -max_absolute_difference)
            {
              std::ostringstream tmp;
              tmp << iterations << ":" << col << " min = " << h3_min[col] 
                  << ", max = " << h3_max[col] << ", guess = " << guess[col] 
                  << ", value = " << value[col]
                  << ", bottom = " << h_bottom[col]
                  << ", abs = " << max_abs << ", rel = " << max_rel;
              daisy_bug (tmp.str ());
            }
          
          if (converge_interval)
            {
              max_abs = std::max (max_abs, h3_diff);
              if (h3_max[col] > max_absolute_difference)
                max_rel = std::max (max_rel, h3_diff / h3_max[col]);
            }
          else
            {
              const double guess_diff = std::fabs (value[col] - old_value[col]);
              max_abs = std::max (max_abs, guess_diff);
              if (value[col] > max_absolute_difference)
                max_rel = std::max (max_rel, old_value[col] / value[col]);
            }

        }
      // Did we find a solution?
      if (max_rel < max_relative_difference 
          || max_abs < max_absolute_difference)
        break;

      if (converge_interval) 
        old_value = value;
   }
  
  // Try to make sure we have something to scale.
  for (size_t col = 0; col < col_size; col++)
    {
      if (guess[col] < h_bottom[col])
        guess[col] = h3_max[col];
      else if (guess[col] > h_bottom[col])
        guess[col] = h3_min[col];
      
      std::fill (S.begin (), S.end (), 0.0);
      find_matrix_sink (geo, active, K, K_crack,
                        h_barrier, pressure_limit, 
                        guess, h, S);
    }

  // Find added and removed water.
  std::vector<double> vol_added (col_size, 0.0);
  std::vector<double> vol_removed (col_size, 0.0);
  for (size_t c = 0; c < cell_size; c++)
    {
      daisy_assert (std::isfinite (S[c]));
      const double vol = S[c] * dt * geo.cell_volume (c);
      const size_t col = column[c];
      if (S[c] > 0.0)
        vol_added[col] += vol;
      else if (S[c] < 0.0)
        vol_removed[col] -= vol;
      else
        daisy_assert (iszero (S[c]));
      
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
      daisy_assert (std::isfinite (vol_removed[col]));
      daisy_assert (vol_removed[col] >= 0.0);
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
      daisy_assert (std::isfinite (S[c]));
      if (S[c] > 0.0)
        S[c] *= scale_added[col];
      else
        S[c] *= scale_removed[col];
      daisy_assert (std::isfinite (S[c]));
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
BioporeMatrix::update_cell_water (const Geometry& geo, const double dt)
{
  const size_t cell_size = geo.cell_size ();
  const std::vector<double> Theta_old = Theta;
  const size_t col_size = xplus.size ();
  daisy_assert (h_bottom.size () == col_size);
  std::fill (Theta.begin (), Theta.end (), 0.0);
  daisy_assert (Theta.size () == cell_size);
  double last_x = 0.0;
  double total_water = 0.0;     // [cm^3]

  for (size_t col = 0; col < col_size; col++)
    {
      const double next_x = xplus[col];
      if (h_bottom[col] > 0.0)
        {
          double top = height_end + std::max (h_bottom[col], 1e-10);
	  const double water = column_water (col);
	  geo.add_soil (Theta, top, height_end, last_x, next_x, water);
	  total_water += water;
        }
      last_x = next_x;
    }
  daisy_approximate (total_water, geo.total_soil (Theta));

  // Update flux
  std::vector<double> source (cell_size, 0.0);
  for (size_t c = 0; c < cell_size; c++)
    source[c] = S[c] - (Theta[c] - Theta_old[c]) / dt;

  geo.biopore_pass_below (source, q);
}

void
BioporeMatrix::update_soil_tertiary (std::vector<double>& Theta_p,
                                     std::vector<double>& q_p)
{
  const size_t edge_size = q.size ();
  daisy_assert (edge_size == q_p.size ());
  for (size_t i = 0; i < edge_size; i++)
    q_p[i] += q[i];

  const size_t cell_size = Theta.size ();
  daisy_assert (cell_size == Theta_p.size ());
  for (size_t i = 0; i < cell_size; i++)
    Theta_p[i] += Theta[i];
}

void 
BioporeMatrix::add_to_sink (std::vector<double>& S_B2M,
                            std::vector<double>& S_M2B,
                            std::vector<double>&, std::vector<double>&) const
{
  const size_t cell_size = S.size ();
  daisy_assert (S_B2M.size () == cell_size);
  daisy_assert (S_M2B.size () == cell_size);
  for (size_t c = 0; c < cell_size; c++)
    {
      daisy_assert (std::isfinite (S[c]));
      daisy_assert (S_M2B[c] >= 0.0);
      daisy_assert (S_B2M[c] >= 0.0);
      if (S[c] > 0)
        S_M2B[c] += S[c];
      else
        S_B2M[c] -= S[c];
      daisy_assert (S_M2B[c] >= 0.0);
      daisy_assert (S_B2M[c] >= 0.0);
    }
}

void 
BioporeMatrix::add_solute (const symbol chem, 
                           const size_t cell, const double amount /* [g] */)
{
  daisy_assert (cell < column.size ());
  const size_t col = column[cell];
  solute.add_value (chem, col, amount);
}

void 
BioporeMatrix::remove_solute (const symbol chem)
{
  Library& library = metalib.library (Chemical::component);
  std::set<symbol> found;
  for (auto i: solute)
    if (library.is_derived_from (i, chem))
      found.insert (i);
  
  for (auto i: found)
    {
      std::vector<double>& array = solute.get_array (i);
      std::fill (array.begin (), array.end (), 0.0);
    }
}

void 
BioporeMatrix::matrix_solute (const Geometry& geo, const double dt, 
                              Chemical& chemical, Treelog& msg)
{
  TREELOG_MODEL (msg);
  const symbol chem = chemical.objid;
  const size_t cell_size = geo.cell_size ();
  std::vector<double>& sink_chem = S_chem.get_array (chem);
  sink_chem.resize (cell_size);
  std::fill (sink_chem.begin (), sink_chem.end (), 0.0);

  // Old content.
  const std::vector<double>& old_array = solute.get_array (chem);
  const double old_content
    = std::accumulate (old_array.begin (), old_array.end (), 0.0);

  // Water that left each column.
  const size_t column_size = xplus.size ();
  std::vector<double> water_left (column_size, 0.0); // [cm^3 W]

  // From matrix to biopore.
  for (size_t c = 0; c < cell_size; c++)
    {
      // Water.
      const double cell_volume = geo.cell_volume (c); // [cm^3 S]
      const double water_sink = S[c]; // [cm^3 W/cm^3 S/h]
      const double water = cell_volume * water_sink * dt; // [cm^3 W]
      if (water <= 0.0)
        // Keep track of water that is leaving for later.
        {
          const size_t col = column[c];
          water_left[col] -= water;
          continue;
        }
      
      // Matrix concentration.
      const double C = chemical.C_to_drain (c); // [g/cm^3 W]
      daisy_assert (C >= 0.0);

      // Add to solute sink.
      sink_chem[c] = water_sink * C; // [g/cm^3 S/h]

      // Add to biopore.
      const double M = C * water; // [g]
      daisy_assert (M >= 0.0);
      daisy_approximate (sink_chem[c] * cell_volume * dt, M);
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
      if (total_water <= 0.0)
        {
          if (total_water < -1e99)
            {
              std::ostringstream tmp;
              tmp << col << ": column_water (" << column_water (col)
                  << ") + water left (" << water_left[col] 
                  << ") = total_water (" << total_water << ") < 0";
              msg.bug (tmp.str ());
            }
          continue;
        }
      daisy_assert (total_water > 0.0);
      const double M = solute.get_value (chem, col); // [g]
      if (M <= 0.0)
        continue;
      const double C = M / total_water; // [g/cm^3 W]
      sink_chem[c] = water_sink * C; // [g/cm^3 S/h]
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
  double total_in = 0.0;
  double total_out = 0.0;
  for (size_t c = 0; c < cell_size; c++)
    {
      const double amount = sink_chem[c] * geo.cell_volume (c) * dt;
      if (amount > 0)
        total_in += amount;
      else 
        total_out -= amount;
    }
  const std::vector<double>& new_array = solute.get_array (chem);
  const double new_content
    = std::accumulate (new_array.begin (), new_array.end (), 0.0);
  const double growth = total_in - total_out;
  if (!balance (old_content, new_content, growth)
      && !approximate (new_content + total_out, old_content + total_in))
    {
      const double error = old_content + growth - new_content;
      std::ostringstream tmp;
      tmp << "In: " << total_in << ", out: " << total_out
          << ", old: " << old_content << ", new: " << new_content 
          << ", gain: " << error;
      msg.message (tmp.str ());
    }

  update_cell_solute (geo, chem, dt);
  const std::vector<double> empty_cell (cell_size, 0.0);
  const std::vector<double>& M_array = M.get_array (chem);
  const std::vector<double>& M_S = S_chem.get_array (chem);
  const std::vector<double>& Jc = J.get_array (chem);
  if (M_array.size () > 0)
      chemical.add_tertiary (M_array, Jc, M_S, empty_cell, empty_cell);
}

void
BioporeMatrix::update_cell_solute (const Geometry& geo, const symbol chem,
				   const double dt)
{
  const std::vector<double>& solute_array 
    = solute.get_array (chem);
  if (solute_array.size () == 0)
    return;

  const size_t cell_size = geo.cell_size ();
  const size_t max_col_size = xplus.size ();
  const size_t col_size = solute_array.size ();
  daisy_assert (max_col_size >= col_size);
  
  std::vector<double>& M_array = M.get_array (chem);
  M_array.resize (cell_size, 0.0);
  std::vector<double> M_old = M_array;
  std::fill (M_array.begin (), M_array.end (), 0.0);

  double last_x = 0.0;
  double total_M = 0.0;     // [g]
  for (size_t col = 0; col < col_size; col++)
    {
      const double next_x = xplus[col];

      const double amount = solute_array[col];
      if (amount > 0.0)
	{
	  double top = height_end + std::max (h_bottom[col], 1e-10);
	  geo.add_soil (M_array, top, height_end, last_x, next_x, 
			amount);
	  total_M += amount;
	}
    }
  daisy_approximate (total_M, geo.total_soil (M_array));

  // Update flux

  std::vector<double>& J_array = J.get_array (chem);
  const size_t edge_size = geo.edge_size ();
  J_array.resize (edge_size);
  const std::vector<double>& M_S = S_chem.get_array (chem);
  std::vector<double> M_source (cell_size, 0.0);
  for (size_t c = 0; c < cell_size; c++)
    M_source[c] = M_S[c] - (M_array[c] - M_old[c]) / dt;
  geo.biopore_pass_below (M_source, J_array);
}

void
BioporeMatrix::output (Log& log) const
{
  output_base (log);
  output_variable (h_bottom, log);
  output_submodule (solute, "solute", log);
  output_lazy (total_water (), "water", log);
  output_variable (iterations, log);
  output_variable (h3_soil, log);
  output_variable (z3_lowest, log);
  output_variable (Theta, log);
  output_submodule (M, "M", log);
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
        case Groundwater::free_drainage:
          break;
        default:
          msg.error ("Unsupported groundwater model '" 
                     + groundwater.objid + "'");
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

  // h3_soil.
  h3_soil.insert (h3_soil.end (), column_size, 0.0);
  daisy_assert (h3_soil.size () == column_size);

  // z3_lowest.
  z3_lowest.insert (z3_lowest.end (), column_size, 0.0);
  daisy_assert (z3_lowest.size () == column_size);

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

  // Cell water content.
  Theta.insert (Theta.begin (), cell_size, 0.0);
  update_cell_water (geo, 1.0);
  std::fill (q.begin (), q.end (), 0.0);
  return ok;
}

BioporeMatrix::BioporeMatrix (const BlockModel& al)
  : Biopore (al),
    metalib (al.metalib ()),
    xplus (al.check ("xplus") 
           ? al.number_sequence ("xplus") 
           : std::vector<double> ()),
    K_wall_relative (al.number ("K_wall_relative")),
    debug (al.integer ("debug")),
    max_iterations (al.integer ("max_iterations")),
    max_absolute_difference (al.number ("max_absolute_difference")),
    max_relative_difference (al.number ("max_relative_difference")),
    allow_upward_flow (al.flag ("allow_upward_flow")),
    h_bottom (al.check ("h_bottom") 
              ? al.number_sequence ("h_bottom") 
              : std::vector<double> ()),
    solute (al, "solute"),
    M (al, "M")
{ }

static struct BioporeMatrixSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new BioporeMatrix (al); }

  BioporeMatrixSyntax ()
    : DeclareModel (Biopore::component, "matrix", "\
Biopores that ends in the matrix.")
  { }
  static void load_solute (Frame& frame)
  { IMvec::add_syntax (frame, Attribute::OptionalState, Attribute::Variable,
		       IM::mass_unit ()); }
  static void load_M (Frame& frame)
  { IMvec::add_syntax (frame, Attribute::LogOnly, Attribute::SoilCells,
		       IM::soil_unit ()); }
  void load_frame (Frame& frame) const
  { 
    frame.declare ("xplus", "cm", Check::positive (), 
                Attribute::OptionalConst, Attribute::Variable,
                "Right side of each biopore interval.\n\
Water and chemical content is tracked individually for each interval.\n\
By default, use intervals as specified by the geometry.");
    frame.set_check ("xplus", VCheck::increasing ());
    frame.declare ("K_wall_relative", Attribute::None (), 
                   Check::positive (), Attribute::Const, "\
Relative conductivity of biopore wall compared to matrix.");
    frame.declare_integer ("debug", Attribute::Const, "Debug level.\n\
Increase value to get more debug message.");
    frame.set ("debug", 0);
    frame.declare ("h_bottom", "cm", Attribute::OptionalState, Attribute::Variable,
                "Pressure at the bottom of the biopores in each interval.");
    frame.declare_submodule_sequence ("solute", Attribute::State, "\
Chemical concentration in biopore intervals.", load_solute);
    frame.set_empty ("solute");
    frame.declare ("water", "cm^3", Attribute::LogOnly, "Water content.");    
    frame.declare_integer ("iterations", Attribute::LogOnly, 
                "Number of iterations used for finding a solution.");
    frame.declare ("h3_soil", "cm", Attribute::LogOnly, Attribute::Variable,
                "Pressure suggested by the soil for each interval.");
    frame.declare ("z3_lowest", "cm", Attribute::LogOnly, Attribute::Variable,
                   "Depth of lowest unsaturated cell in each interval.\n\
Water may not enter the macropore below this depth.");
    frame.declare ("Theta", "cm^3/cm^3", 
                   Attribute::LogOnly, Attribute::SoilCells, "\
Water content in this biopore class.");
    frame.declare_submodule_sequence ("M", Attribute::LogOnly, "\
Chemical content in soil cells.", load_M);
    frame.declare_integer ("max_iterations", Attribute::Const, "\
Maximum number of iterations when seeking convergence.");
    frame.set ("max_iterations", 50);
    frame.declare ("max_absolute_difference", "cm", Attribute::Const, "\
Maximum absolute difference in biopore content for convergence.");
    frame.set ("max_absolute_difference", 0.02);
    frame.declare ("max_relative_difference", Attribute::None (), Attribute::Const, "\
Maximum relative difference in biopore content for convergence.");
    frame.set ("max_relative_difference", 0.001);
    frame.declare_boolean ("allow_upward_flow", Attribute::Const, "\
Allow water to enter from saturated soil at the bottom of the biopore.\n\
And leave in unsaturated soil above.");
    frame.set ("allow_upward_flow", true);
  }
} BioporeMatrix_syntax;

// biopore_matrix.C ends here.
