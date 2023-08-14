// soil_water.C
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

#include "soil_water.h"
#include "geometry.h"
#include "soil.h"
#include "soil_heat.h"
#include "groundwater.h"
#include "log.h"
#include "librarian.h"
#include "block.h"
#include "check.h"
#include "treelog.h"
#include "assertion.h"
#include "mathlib.h"
#include "frame_submodel.h"
#include <sstream>

void
SoilWater::clear ()
{
  fill (S_sum_.begin (), S_sum_.end (), 0.0);
  fill (S_drain_.begin (), S_drain_.end (), 0.0);
  fill (S_indirect_drain_.begin (), S_indirect_drain_.end (), 0.0);
  fill (S_soil_drain_.begin (), S_soil_drain_.end (), 0.0);
  fill (S_root_.begin (), S_root_.end (), 0.0);
  fill (S_incorp_.begin (), S_incorp_.end (), 0.0);
  fill (S_B2M_.begin (), S_B2M_.end (), 0.0);
  fill (S_M2B_.begin (), S_M2B_.end (), 0.0);
  fill (S_p_drain_.begin (), S_p_drain_.end (), 0.0);
  fill (tillage_.begin (), tillage_.end (), 0.0);
  fill (S_ice_ice.begin (), S_ice_ice.end (), 0.0);
  fill (S_ice_water_.begin (), S_ice_water_.end (), 0.0);
  fill (S_forward_total_.begin (), S_forward_total_.end (), 0.0);
  fill (S_forward_sink_.begin (), S_forward_sink_.end (), 0.0);

  fill (q_primary_.begin (), q_primary_.end (), 0.0);
  fill (q_secondary_.begin (), q_secondary_.end (), 0.0);
  fill (q_matrix_.begin (), q_matrix_.end (), 0.0);
  fill (q_tertiary_.begin (), q_tertiary_.end (), 0.0);
}

void 
SoilWater::freeze (const Soil&, const size_t c, const double rate /* [h^-1] */)
{
  static const double rho_water = 1.0; // [g/cm^3]
  static const double rho_ice = 0.917; // [g/cm^3]

  daisy_assert (c < S_ice_ice.size ());
  daisy_assert (c < S_ice_water_.size ());
  S_ice_water_[c] += rate;
  S_ice_ice[c] -= rate * rho_water / rho_ice;
  daisy_assert (c < S_sum_.size ());
  S_sum_[c] += rate;
}

void
SoilWater::drain (const std::vector<double>& v, Treelog& msg)
{
  forward_sink (v, v);

  daisy_assert (S_sum_.size () == v.size ());
  daisy_assert (S_drain_.size () == v.size ());
  daisy_assert (S_soil_drain_.size () == v.size ());
  for (unsigned i = 0; i < v.size (); i++)
    {
      if (v[i] < -1e-8)
        {
          std::ostringstream tmp;
          tmp << "draining " << v[i] << " [h^-1] from cell " << i;
          msg.debug (tmp.str ());
        }
      S_sum_[i] += v[i];
      S_drain_[i] += v[i];
      S_soil_drain_[i] += v[i];
    }
}

void
SoilWater::forward_sink (const std::vector<double>& total, 
                         const std::vector<double>& sink)
{
  daisy_assert (S_forward_total_.size () == total.size ());
  for (size_t i = 0; i < total.size (); i++)
    S_forward_total_[i] += total[i];
  daisy_assert (S_forward_sink_.size () == sink.size ());
  for (size_t i = 0; i < sink.size (); i++)
    S_forward_sink_[i] += sink[i];
}

void
SoilWater::root_uptake (const std::vector<double>& v)
{
  daisy_assert (S_sum_.size () == v.size ());
  daisy_assert (S_root_.size () == v.size ());
  for (unsigned i = 0; i < v.size (); i++)
    {
      S_sum_[i] += v[i];
      S_root_[i] += v[i];
    }
}

double 
SoilWater::content_surface (const Geometry& geo, 
                            const double from, const double to) const
{ return geo.total_surface (Theta_, from, to); }

double 
SoilWater::velocity_cell_primary (const Geometry& geo, const size_t c) const
{ 
  const double Theta = this->Theta_primary (c);
  if (Theta < 1e-100)
    return 0.0;

  double t_q_z = 0.0;
  double t_q_x = 0.0;
  double t_area_z = 0.0;
  double t_area_x = 0.0;

  const std::vector<size_t>& cell_edges = geo.cell_edges (c);
  for (size_t e = 0; e < cell_edges.size (); e++)
    {
      const size_t edge = cell_edges[e];
      const double q = this->q_primary (edge);
      const double area = geo.edge_area (edge);
      const double sin_angle = geo.edge_sin_angle (edge);
      const double cos_angle = cos (asin (sin_angle));
      const double area_z = sin_angle * area;
      const double area_x = cos_angle * area;
      t_q_z += area_z * q;
      t_q_x += area_x * q;
      t_area_z += area_z;
      t_area_x += area_x;
    }

  const double q_z = t_q_z / t_area_z;
  const double q_x = t_q_x / t_area_x;
  const double q = sqrt (sqr (q_z) + sqr (q_x));
  const double v = q / Theta;
  return v;
}

double 
SoilWater::velocity_cell_secondary (const Geometry& geo, const size_t c)const
{ 
  const double Theta = this->Theta_secondary (c);
  if (Theta < 1e-100)
    return 0.0;

  double t_q_z = 0.0;
  double t_q_x = 0.0;
  double t_area_z = 0.0;
  double t_area_x = 0.0;

  const std::vector<size_t>& cell_edges = geo.cell_edges (c);
  for (size_t e = 0; e < cell_edges.size (); e++)
    {
      const size_t edge = cell_edges[e];
      const double q = this->q_secondary (edge);
      const double area = geo.edge_area (edge);
      const double sin_angle = geo.edge_sin_angle (edge);
      const double cos_angle = cos (asin (sin_angle));
      const double area_z = sin_angle * area;
      const double area_x = cos_angle * area;
      t_q_z += area_z * q;
      t_q_x += area_x * q;
      t_area_z += area_z;
      t_area_x += area_x;
    }

  const double q_z = t_q_z / t_area_z;
  const double q_x = t_q_x / t_area_x;
  const double q = sqrt (sqr (q_z) + sqr (q_x));
  const double v = q / Theta;
  return v;
}

double
SoilWater::Theta_ice (const Soil& soil, const size_t i, const double h) const
{ return soil.Theta (i, h, h_ice (i)); }
 
void 
SoilWater::set_content (const size_t i, const double h, const double Theta)
{
  daisy_assert (i < h_.size ());
  h_[i] = h;
  daisy_assert (i < Theta_.size ());
  Theta_[i] = Theta;
}
 
void
SoilWater::set_flux (const size_t i, const double q)
{
  daisy_assert (std::isfinite (q));
  daisy_assert (i < q_matrix_.size ());
  q_matrix_[i] = q;
}

void 
SoilWater::set_matrix (const std::vector<double>& h,
                       const std::vector<double>& Theta,
                       const std::vector<double>& q)
{
  daisy_assert (h_.size () == h.size ());
  daisy_assert (Theta_.size () == Theta.size ());
  for (size_t c = 0; c < h.size (); c++)
    {
      daisy_assert (std::isfinite (h[c]));
      daisy_assert (std::isfinite (Theta[c]));
      daisy_assert (Theta[c] >= 0.0);
    }

  daisy_assert (q_matrix_.size () == q.size ());
  for (size_t e = 0; e < q.size (); e++)
    daisy_assert (std::isfinite (q[e]));

  h_ = h;
  Theta_ = Theta;
  q_matrix_ = q;
}

void 
SoilWater::set_tertiary (const std::vector<double>& Theta_p,
                         const std::vector<double>& q_p,
                         const std::vector<double>& S_B2M,
                         const std::vector<double>& S_M2B,
                         const std::vector<double>& S_drain,
                         const std::vector<double>& S_tertiary_drain)
{
  const size_t cell_size = Theta_p.size ();
  daisy_assert (Theta_tertiary_.size () == cell_size);
  daisy_assert (q_tertiary_.size () == q_p.size ());
  daisy_assert (S_M2B_.size () == cell_size);
  daisy_assert (S_B2M_.size () == cell_size);
  daisy_assert (S_M2B.size () == cell_size);
  daisy_assert (S_drain.size () == cell_size);
  daisy_assert (S_indirect_drain_.size () == cell_size);
  daisy_assert (S_tertiary_drain.size () == cell_size);
  daisy_assert (S_sum_.size () == cell_size);
  daisy_assert (S_p_drain_.size () == cell_size);
  Theta_tertiary_ = Theta_p;
  q_tertiary_ = q_p;
  for (size_t c = 0; c < cell_size; c++)
    {
      daisy_assert (S_B2M[c] >= 0.0);
      daisy_assert (S_M2B[c] >= 0.0);
      S_sum_[c] += S_M2B[c] - S_B2M[c] + S_drain[c];
      S_B2M_[c] += S_B2M[c];
      S_M2B_[c] += S_M2B[c];
      S_drain_[c] += S_drain[c];
      S_indirect_drain_[c] += S_drain[c];
      S_p_drain_[c] += S_tertiary_drain[c];
    }
}

void 
SoilWater::tick_source (const Geometry& geo, const Soil& soil, Treelog& msg)
{
  const size_t cell_size = geo.cell_size ();

  sink_dt = 0.0;
  sink_cell = Geometry::cell_error;
  for (size_t c = 0; c < cell_size; c++)
    {
      const double S = S_forward_total (c);
      if (!std::isnormal (S))
        continue;
      static const double h_wp = -15000.0;
      static const double h_sat = 0.0;
      const double h_ice = this->h_ice (c);
      const double Theta_wp = soil.Theta (c, h_wp, h_ice);
      const double Theta_sat = soil.Theta (c, h_sat, h_ice);
      const double Theta_max = Theta_sat - Theta_wp;
      if (Theta_max < 0.01)
        // Mostly ice?
        continue;
      const double dt = Theta_max * max_sink_change / std::fabs (S);
      if (std::isnormal (dt)
          && (!std::isnormal (sink_dt) || std::fabs (sink_dt) > std::fabs (dt)))
        {
          sink_dt = dt;
          sink_cell = c;
        }
    }
}
 
double 
SoilWater::suggest_dt ()
{ return std::fabs (sink_dt); }

void
SoilWater::tick_before (const Geometry& geo, const Soil& soil, 
                        const double dt, Treelog& msg)
{
  TREELOG_SUBMODEL (msg, "SoilWater");

  const size_t cell_size = geo.cell_size ();

  // Fluxify management operations.
  daisy_assert (tillage_.size () == cell_size);
  for (size_t i = 0; i < cell_size; i++)
    tillage_[i] /= dt;

  // External sink.
  for (size_t i = 0; i < cell_size; i++)
    {
      S_incorp_[i] += S_permanent_[i];
      S_sum_[i] += S_incorp_[i];
    }
}

void
SoilWater::reset_old ()
{
  // Remember old values.
  h_old_ = h_;
  Theta_old_ = Theta_;
  Theta_primary_old_ = Theta_primary_;
  Theta_secondary_old_ = Theta_secondary_;
  X_ice_old_ = X_ice_;
}

void
SoilWater::tick_ice (const Geometry& geo, const Soil& soil, 
                     const double dt, Treelog& msg)
{
  TREELOG_SUBMODEL (msg, "SoilWater");

  const size_t cell_size = geo.cell_size ();

  // Ice first.
  for (size_t i = 0; i < cell_size; i++)
    {
      const double porosity = soil.Theta (i, 0.0, 0.0);
      const double Theta_res = soil.Theta_res (i);

      X_ice_[i] -= S_ice_ice[i] * dt;

      // Move extra ice to buffer.
      const double total_ice = X_ice_[i] + X_ice_buffer_[i];
      if (total_ice > 0.0)
        {
#ifdef THETA_RES
          if (Theta_[i] < Theta_res)
            {
              std::ostringstream tmp;
              tmp << "Theta[" << i << "] = " << Theta_[i]
                  << ", less than Theta_res = " << Theta_res;
              daisy_warning (tmp.str ());
            }
#endif
          const double Theta_lim = std::max (Theta_res, 
                                             Theta_[i] - S_ice_water_[i] * dt);

          const double available_space = porosity - Theta_lim - 1e-9;
          X_ice_[i] = std::min (available_space, total_ice);
          X_ice_buffer_[i] = total_ice - X_ice_[i];
        }
      else
        {
          X_ice_[i] = 0.0;
          X_ice_buffer_[i] = total_ice;
        }
      daisy_approximate (X_ice_[i] + X_ice_buffer_[i], total_ice);

      // Update ice pressure.
      h_ice_[i] = soil.h (i, porosity - X_ice_[i]);
    }
}

void
SoilWater::tick_after (const Geometry& geo,
                       const Soil& soil, const SoilHeat& soil_heat, 
                       const bool initial, 
                       Treelog& msg)
{
  TREELOG_SUBMODEL (msg, "SoilWater");

  // We need old K for primary/secondary flux division.
  std::vector<double> K_old = K_cell_;

  // Update cells.
  const size_t cell_size = geo.cell_size ();
  daisy_assert (K_cell_.size () == cell_size);
  daisy_assert (Cw2_.size () == cell_size);
  daisy_assert (h_.size () == cell_size);
  daisy_assert (h_ice_.size () == cell_size);
  daisy_assert (K_old.size () == cell_size);
  daisy_assert (Theta_.size () == cell_size);
  daisy_assert (Theta_primary_.size () == cell_size);
  daisy_assert (Theta_secondary_.size () == cell_size);
  daisy_assert (Theta_tertiary_.size () == cell_size);

  double z_low = geo.top ();
  table_low = NAN;
  double z_high = geo.bottom ();
  table_high = NAN;

  for (size_t c = 0; c < cell_size; c++)
    {
      // Groundwater table.
      const double z = geo.cell_top (c);
      const double h = h_[c];
      const double table = z + h;
      if (h < 0)
        {
          if (approximate (z, z_low))
            {
              if (!std::isfinite (table_low)
                  || table < table_low)
                table_low = table;
            }
          else if (z < z_low)
            {
              z_low = z;
              table_low = table;
            }
        }
      else if (approximate (z, z_high))
        {
          if (!std::isfinite (table_high)
              || table > table_high)
            table_high = table;
        }
      else if (z > z_high)
        {
          table_high = table;
          z_high = z;
        }

      // Conductivity.
      K_cell_[c] = soil.K (c, h_[c], h_ice_[c], soil_heat.T (c));
      
      // Specific water capacity.
      Cw2_[c] = soil.Cw2 (c, h_[c]);

      // Primary and secondary water.
      if (Theta_[c] <= 0.0)
        {
          std::ostringstream tmp;
          tmp << "Theta[" << c << "] = " << Theta_[c];
          daisy_bug (tmp.str ());
          Theta_[c] = 1e-9;
        }
      const double h_lim = soil.h_secondary (c);
      if (h_lim >= 0.0 || h_[c] <= h_lim)
        {
          // No active secondary domain.
          Theta_primary_[c] = Theta_[c];
          Theta_secondary_[c] = 0.0;
        }
      else 
        {
          // Secondary domain activated.
          const double Theta_lim = soil.Theta (c, h_lim, h_ice_[c]);
          daisy_assert (Theta_lim > 0.0);
          if (Theta_[c] >= Theta_lim)
            {
              Theta_primary_[c] = Theta_lim;
              Theta_secondary_[c] = Theta_[c] - Theta_lim;
            }
          else
            { 
              std::ostringstream tmp;
              tmp << "h[" << c << "] = " << h_[c] 
                  << "; Theta[" << c << "] = " << Theta_[c] 
                  << "\nh_lim = " << h_lim << "; Theta_lim = " << Theta_lim
                  << "\nStrenge h > h_lim, yet Theta <= Theta_lim";
              msg.bug (tmp.str ());
              Theta_primary_[c] = Theta_[c];
              Theta_secondary_[c] = 0.0;
            }
        }
    }

#if 0
  if (!std::isfinite (table_high))
    {
      // No saturated cell, use lowest unsaturated.
      // daisy_assert (std::isfinite (table_low));
      table_high = table_low;
    }
  else if (!std::isfinite (table_low))
    {
      // No unsaturated cell, use highest saturated.
      // daisy_assert (std::isfinite (table_high));
      table_low = table_high;
    }
#endif
  // Initialize
  if (initial)
    {
      K_old = K_cell_;
      Theta_primary_old_ = Theta_primary_;
      Theta_secondary_old_ = Theta_secondary_;
    }
  daisy_assert (K_old.size () == cell_size);
  daisy_assert (Theta_primary_old_.size () == cell_size);
  daisy_assert (Theta_secondary_old_.size () == cell_size);

  // Update edges.
  const size_t edge_size = geo.edge_size ();
  daisy_assert (q_matrix_.size () == edge_size);
  daisy_assert (q_primary_.size () == edge_size);
  daisy_assert (q_secondary_.size () == edge_size);
  daisy_assert (q_matrix_.size () == edge_size);
  daisy_assert (q_matrix_.size () == edge_size);
  for (size_t e = 0; e < edge_size; e++)
    {
      // By default, all flux is in primary domain.
      q_primary_[e] = q_matrix_[e];
      q_secondary_[e] = 0.0;
      
      // Find average K.
      double K_edge = 0.0;
      double K_lim = 0.0;
      
      // K may be discontinious at h_lim in case of cracks.
      const double h_fudge = 0.01;

      // Contributions from target.
      const int to = geo.edge_to (e);
      if (geo.cell_is_internal (to))
        {
          if (iszero (Theta_secondary_old (to)) || 
              iszero (Theta_secondary (to)))
            continue;
          K_edge += 0.5 * (K_old[to] + K_cell (to));
          const double h_lim = soil.h_secondary (to) - h_fudge;
          K_lim += soil.K (to, h_lim, h_ice (to), soil_heat.T (to));
        }
      
      // Contributions from source.
      const int from = geo.edge_from (e);
      if (geo.cell_is_internal (from))
        {
          
          if (iszero (Theta_secondary_old (from)) || 
              iszero (Theta_secondary (from)))
            continue;
          K_edge += 0.5 * (K_old[from] + K_cell (from));
          
          const double h_lim = soil.h_secondary (from) - h_fudge;
          K_lim += soil.K (from, h_lim, h_ice (from), soil_heat.T (from));
        }
      
      daisy_assert (K_lim > 0.0);
      daisy_assert (K_edge > 0.0);
      
      // BUGLET:  We use in effect arithmetic average here for K.
      daisy_assert (std::isnormal (K_edge));
      // This may not have been what was used for calculating q matrix.
      const double K_factor = K_lim / K_edge;
      daisy_assert (std::isfinite (K_factor));
      if (K_factor < 0.99999)
        {
          q_primary_[e] = q_matrix_[e] * K_factor;
          q_secondary_[e] = q_matrix_[e] - q_primary_[e];
        }
    }
}

void
SoilWater::mass_balance (const Geometry& geo, double dt, Treelog& msg) const
{
  const size_t edge_size = geo.edge_size ();
  const double total_sink = 10 * geo.total_surface (S_sum_) * dt;
  const double total_old = 10 * geo.total_surface (Theta_old_);
  const double total_new = 10 * geo.total_surface (Theta_);
  double total_boundary_input = 0.0;
  for (size_t e = 0; e < edge_size; e++)
    {
      if (geo.edge_is_internal (e))
        continue;
      const double in_sign 
        = geo.cell_is_internal (geo.edge_to (e)) ? 1.0 : -1.0;
      const double q = q_primary_[e] + q_secondary_[e];
      const double area = geo.edge_area (e);
      total_boundary_input += q * area * in_sign * dt;
    }
  total_boundary_input /= geo.surface_area ();
  total_boundary_input *= 10;
  const double total_expected 
    = total_old - total_sink + total_boundary_input;
  const double total_error = total_expected - total_new;
  const double total_error_rate = total_error / dt;
  const double max_error = total_old * 1e-6;
  const double max_error_rate = 0.001;
  if (std::fabs (total_error) > max_error
      || std::fabs (total_error_rate) > max_error_rate)
    {
      const double total_diff = total_new - total_old;
      static double accumulated_error = 0.0;
      accumulated_error += total_error;
      std::ostringstream tmp;
      tmp << "Water balance: old (" << total_old
          << ") - sink (" << total_sink 
          << ") + boundary input (" << total_boundary_input
          << ") != " << total_expected << " mm, got " << total_new 
          << " mm, difference is " << total_diff 
          << " mm, error is " << total_error
          << " mm, accumulated " << accumulated_error
          << " mm, dt = " << dt
          << " h, error rate is " << total_error_rate
          << " mm/h";
      for (size_t e = 0; e < edge_size; e++)
        {
          if (geo.edge_is_internal (e))
            continue;
          if (iszero (q_primary_[e])
              && iszero (q_secondary_[e])
              && iszero (q_matrix_[e]))
            continue;
          tmp << "\nedge " << geo.edge_name (e)
              << ": primary " << q_primary_[e]
              << ", secondary = " << q_secondary_[e];
          if (!approximate (q_primary_[e] + q_secondary_[e], q_matrix_[e]))
            tmp << ", NOT MATCHING matrix = " << q_matrix_[e];
          tmp << ", tertiary = " << q_tertiary_[e]
              << ", area = " << geo.edge_area (e);
        }
      msg.error (tmp.str ());
    }
}

void 
SoilWater::incorporate (const Geometry& geo, const double flux,
                        const double from, const double to)
{ geo.add_surface (S_incorp_, from, to, -flux); }

void 
SoilWater::incorporate (const Geometry& geo, const double flux,
                        const Volume& volume)
{ geo.add_surface (S_incorp_, volume, -flux); }

double
SoilWater::mix (const Geometry& geo, const Soil& soil, 
                const SoilHeat& soil_heat, const double from, 
                const double to, Treelog& msg)
{
  geo.mix (Theta_, from, to, tillage_);
  return overflow (geo, soil, soil_heat, msg);
}

double
SoilWater::swap (const Geometry& geo, const Soil& soil, 
                 const SoilHeat& soil_heat, const double from, 
                 const double middle, const double to,
                 Treelog& msg)
{
  geo.swap (Theta_, from, middle, to, tillage_);
  return overflow (geo, soil, soil_heat, msg);
}

double
SoilWater::overflow (const Geometry& geo, 
                     const Soil& soil, const SoilHeat& soil_heat, 
                     Treelog& msg)
{
  const size_t cell_size = geo.cell_size ();
  double extra = 0.0;           // [cm^3]
  for (size_t c = 0; c < cell_size; c++)
    {
      const double h_sat = 0.0;
      const double Theta_sat = soil.Theta (c, h_sat, h_ice (c));
      const double Theta_extra = std::max (Theta_[c] - Theta_sat, 0.0);
      Theta_[c] -= Theta_extra;
      tillage_[c] -= Theta_extra;
      extra += Theta_extra * geo.cell_volume (c);
      const double new_h = soil.h (c, Theta_[c]);
      if (h_[c] < 0.0 || new_h < 0)
        h_[c] = new_h;
    }
  tick_after (geo, soil, soil_heat, false, msg);

  extra /= geo.surface_area (); // [cm]
  extra *= 10.0;                // [mm]
  return extra;
}

void 
SoilWater::output (Log& log) const
{
  output_value (h_, "h", log);
  output_value (Theta_, "Theta", log);
  output_value (Theta_primary_, "Theta_primary", log);
  output_value (Theta_secondary_, "Theta_secondary", log);
  output_value (Theta_tertiary_, "Theta_p", log);
  output_value (S_sum_, "S_sum", log);
  output_value (S_root_, "S_root", log);
  output_value (S_drain_, "S_drain", log);
  output_value (S_indirect_drain_, "S_indirect_drain", log);
  output_value (S_soil_drain_, "S_soil_drain", log);
  output_value (S_incorp_, "S_incorp", log);
  output_value (tillage_, "tillage", log);
  output_value (S_B2M_, "S_B2M", log);
  output_value (S_M2B_, "S_M2B", log);
  output_value (S_p_drain_, "S_p_drain", log);
  output_value (S_permanent_, "S_permanent", log);
  output_value (S_ice_ice, "S_ice", log);
  output_value (S_ice_water_, "S_ice_water", log);
  output_value (S_forward_total_, "S_forward_total", log);
  output_value (S_forward_sink_, "S_forward_sink", log);
  output_value (X_ice_, "X_ice", log);
  output_value (X_ice_buffer_, "X_ice_buffer", log);
  output_value (h_ice_, "h_ice", log);
  output_value (q_matrix_, "q", log);
  output_value (q_primary_, "q_primary", log);
  output_value (q_secondary_, "q_secondary", log);
  output_value (q_tertiary_, "q_p", log);
  output_value (K_cell_, "K", log);
  output_value (Cw2_, "Cw2", log);
  if (std::isnormal (sink_dt))
    {
      output_value (sink_dt, "dt", log);
      output_variable (sink_cell, log);
    }
  if (std::isfinite (table_low))
    output_variable (table_low, log);
  if (std::isfinite (table_high))
    output_variable (table_high, log);
}

double
SoilWater::MaxExfiltration (const Geometry& geo, const size_t edge,
                            const Soil& soil, const double T) const
{
  const size_t n = geo.edge_other (edge, Geometry::cell_above);
  const double h0 = h (n);
  const double K0 = soil.K (n, h0, h_ice (n), T);
  if (max_exfiltration_gradient > 0.0)
    return K0 * max_exfiltration_gradient;
  const double Cw2 = soil.Cw2 (n, h0);
  const double Theta0 = Theta (n);
  const double Theta_surf = soil.Theta_res (n);
  const double delta_Theta = Theta0 - Theta_surf;
  const double z0 = geo.cell_z (n);
  // Darcy formulated for Theta between middle of node and soil surface.
  return - (K0 / Cw2) * (delta_Theta / z0);
}

double
SoilWater::infiltration (const Geometry& geo) const
{
  const size_t edge_size = geo.edge_size ();
  
  double sum = 0.0;

  for (size_t e = 0; e < edge_size; e++)
    if (geo.edge_to (e) == Geometry::cell_above)
      sum -= (q_matrix (e) + q_tertiary (e)) * geo.edge_area (e);
  
  const double mm_per_cm = 10.0;
  return mm_per_cm * sum / geo.surface_area ();
}

bool 
SoilWater::check (const size_t n, Treelog& msg) const
{
  bool ok = true;

  if (Theta_.size () != n)
    {
      std::ostringstream tmp;
      tmp << "You have " << n 
          << " intervals but " << Theta_.size () << " Theta values";
      msg.error (tmp.str ());
      ok = false;
    }
  if (h_.size () != n)
    {
      std::ostringstream tmp;
      tmp << "You have " << n 
          << " intervals but " << h_.size () << " h values";
      msg.error (tmp.str ());
      ok = false;
    }
  if (X_ice_.size () != n)
    {
      std::ostringstream tmp;
      tmp << "You have " << n 
          << " intervals but " << X_ice_.size () << " X_ice values";
      msg.error (tmp.str ());
      ok = false;
    }
  if (X_ice_buffer_.size () != n)
    {
      std::ostringstream tmp;
      tmp << "You have " << n 
          << " intervals but " << X_ice_buffer_.size () 
          << " X_ice_buffer values";
      msg.error (tmp.str ());
      ok = false;
    }
  return ok;
}

static void
load_h (Frame& frame)
{ Geometry::add_layer (frame, "cm", Check::none (),
		       Attribute::Const, "Soil water pressure."); }

static void
load_Theta (Frame& frame)
{ Geometry::add_layer (frame, Attribute::Fraction (), Check::none (),
		       Attribute::Const, 
                       "Soil water content."); }

void
SoilWater::load_syntax (Frame& frame)
{
  frame.declare ("default_h", "cm", Check::none (), 
                 Attribute::OptionalConst, "\
Default h at surface if neither it nor Theta is specified.\n\
The depth will be added to the pressure.\n\
By default, this will be based on either distance to groundwater\n\
or field capacy (pF 2), whichever is lower.");
  
  frame.declare ("max_exfiltration_gradient", "cm/cm", Check::positive (), 
                 Attribute::OptionalConst,
                 "Maximal pressure gradient for calculating exfiltration.\n\
The gradient is assumed from center of top node to surface of top node.\n\
By default, there is no maximum.");
  frame.declare ("max_sink_change", Attribute::None (), Check::positive (), 
                 Attribute::Const,
                 "Largest change to available water within a timestep.\n\
This is used for calculating the suggested timestep.  The suggested\n\
timestep will be small enough that the change water due to forward\n\
calculated sinks (S_forward) alone is less than the specified value.\n\
\n\
Plant available water is defined as the difference between saturation\n\
and wilting point.");
  frame.set ("max_sink_change", 0.1);
  frame.declare_boolean ("use_last", Attribute::Const, "\
Use last value specified for 'h' or 'Theta' for the rest of the profile.");
  frame.set ("use_last", true);
  Geometry::add_layer (frame, Attribute::OptionalState, "h", load_h);
  Geometry::add_layer (frame, Attribute::OptionalState, "Theta", load_Theta);
  frame.declare ("Theta_primary", "cm^3/cm^3", Attribute::LogOnly, Attribute::SoilCells,
                 "Water content in primary matrix system.\n\
Conventionally, this is the intra-aggregate pores.");
  frame.declare ("Theta_secondary", "cm^3/cm^3", Attribute::LogOnly, 
                 Attribute::SoilCells,
                 "Water content in secondary matrix system.\n\
Conventionally, this is the inter-aggregate pores.");
  frame.declare ("Theta_p", "cm^3/cm^3", Attribute::LogOnly, 
                 Attribute::SoilCells,
                 "Water content in tertiary (biopore) system.");
  frame.declare ("S_sum", "cm^3/cm^3/h", Attribute::LogOnly, Attribute::SoilCells,
                 "Total water sink (due to root uptake and macropores).");
  frame.declare ("S_root", "cm^3/cm^3/h", Attribute::LogOnly, Attribute::SoilCells,
                 "Water sink due to root uptake.");
  frame.declare ("S_drain", "cm^3/cm^3/h", Attribute::LogOnly, Attribute::SoilCells,
                 "Matrix water sink due to soil drainage.");
  frame.declare ("S_indirect_drain", "cm^3/cm^3/h", 
                 Attribute::LogOnly, Attribute::SoilCells, "\
Matrix water sink due to soil drainage, only counting biopores.");
  frame.declare ("S_soil_drain", "cm^3/cm^3/h", 
                 Attribute::LogOnly, Attribute::SoilCells, "\
Matrix water sink due to soil drainage, not counting biopores.");
  frame.declare ("S_incorp", "cm^3/cm^3/h", Attribute::LogOnly, Attribute::SoilCells,
                 "Incorporated water sink, typically from subsoil irrigation.");
  frame.declare ("tillage", "cm^3/cm^3/h", Attribute::LogOnly, Attribute::SoilCells,
                 "Changes in water content due to tillage operations.");
  frame.declare ("S_B2M", "cm^3/cm^3/h",
                 Attribute::LogOnly, Attribute::SoilCells,
                 "Water flowing from biopores to matrix.");
  frame.declare ("S_M2B", "cm^3/cm^3/h", 
                 Attribute::LogOnly, Attribute::SoilCells,
                 "Water flowing from matrix to biopores.\n\
This does not include drain or drain connected biopores.");
  frame.declare ("S_p_drain", "cm^3/cm^3/h", 
                 Attribute::LogOnly, Attribute::SoilCells,
                 "Water moving from biopores to drain.\
This only reflect water in the tertiary domain, not in the matrix.");
  frame.declare ("S_permanent", "cm^3/cm^3/h", Attribute::State, Attribute::SoilCells,
                 "Permanent water sink, e.g. subsoil irrigation.");
  frame.set_empty ("S_permanent");
  frame.declare ("S_ice", "cm^3/cm^3/h",
                 Attribute::LogOnly, Attribute::SoilCells, "\
Ice sink due to thawing or freezing.");
  frame.declare ("S_ice_water", "cm^3/cm^3/h", 
                 Attribute::LogOnly, Attribute::SoilCells, "\
Water sink due to thawing or freezing.");
  frame.declare ("S_forward_total", "cm^3/cm^3/h",
                 Attribute::LogOnly, Attribute::SoilCells, "\
Sink (including source terms) at beginning of timestep.\n\
Used for limiting size of timestep.\n\
Currently this includes drain and tertiary domain (biopores).");
  frame.declare ("S_forward_sink", "cm^3/cm^3/h",
                 Attribute::LogOnly, Attribute::SoilCells, "\
Sink (excluding source terms) at beginning of timestep.\n\
Used for limiting size of timestep.\n\
Currently this includes drain and tertiary domain (biopores).");
  frame.declare_fraction ("X_ice", Attribute::OptionalState, Attribute::SoilCells,
                          "Ice volume fraction in soil.");
  frame.declare ("X_ice_buffer", Attribute::None (), 
                 Attribute::OptionalState, Attribute::SoilCells,
                 "Ice volume that didn't fit the soil durin freezing.");
  frame.declare ("h_ice", Attribute::None (), Attribute::LogOnly, Attribute::SoilCells,
                 "Pressure at which all air is out of the matrix.\n\
When there are no ice, this is 0.0.  When there are ice, the ice is\n\
presummed to occupy the large pores, so it is h (Theta_sat - X_ice).");
  frame.declare ("q", "cm/h", Attribute::LogOnly, Attribute::SoilEdges,
                 "Matrix water flux (positive numbers mean upward).");  
  frame.declare ("q_primary", "cm/h",
                 Attribute::LogOnly, Attribute::SoilEdges, "\
Primary domain water flux (positive numbers mean upward).");  
  frame.declare ("q_secondary", "cm/h",
                 Attribute::LogOnly, Attribute::SoilEdges, "\
Secondary domain water flux (positive numbers mean upward).");  
  frame.declare ("q_p", "cm/h", Attribute::LogOnly, Attribute::SoilEdges,
                 "Water flux in macro pores (positive numbers mean upward).");
  frame.declare ("K", "cm/h", Attribute::LogOnly, Attribute::SoilCells,
                 "Hydraulic conductivity.");
  frame.declare ("Cw2", "cm^-1", Attribute::LogOnly, Attribute::SoilCells,
                 "Specific water capacity.");
  frame.declare ("dt", "h", Attribute::LogOnly, "\
Suggested timestep length (based on S_forward).\n\
The absolute value is used, negative numbers indicate source based limits.");
  frame.declare_integer ("sink_cell", Attribute::LogOnly, "\
Cell with largest forward sink compared to available water.");
  frame.declare ("table_low", "cm", Attribute::LogOnly, "\
Groundwater table estimated by presure in lowest unsaturated cell.\n\
If there are multiple unsaturated cells in the same depth, the one\n\
with the lowest pressure will be used.");
  frame.declare ("table_high", "cm", Attribute::LogOnly, "\
Groundwater table estimated by presure in highest saturated cell.\n\
If there are multiple saturated cells in the same depth, the one\n\
with the highest pressure will be used.");
}

void
SoilWater::initialize (const FrameSubmodel& al, const Geometry& geo,
                       const Soil& soil, const SoilHeat& soil_heat,
                       const Groundwater& groundwater, Treelog& msg)
{
  Treelog::Open nest (msg, "SoilWater");

  const size_t cell_size = geo.cell_size ();
  const size_t edge_size = geo.edge_size ();

  // Ice must be first.
  if (al.check ("X_ice"))
    {
      X_ice_ = al.number_sequence ("X_ice");
      if (X_ice_.size () == 0)
        X_ice_.push_back (0.0);
      while (X_ice_.size () < cell_size)
        X_ice_.push_back (X_ice_[X_ice_.size () - 1]);
    }
  else 
    X_ice_.insert (X_ice_.begin (), cell_size, 0.0);

  if (al.check ("X_ice_buffer"))
    {
      X_ice_buffer_ = al.number_sequence ("X_ice_buffer");
      if (X_ice_buffer_.size () == 0)
        X_ice_buffer_.push_back (0.0);
      while (X_ice_buffer_.size () < cell_size)
        X_ice_buffer_.push_back (X_ice_buffer_[X_ice_buffer_.size () - 1]);
    }
  else 
    X_ice_buffer_.insert (X_ice_buffer_.begin (), cell_size, 0.0);

  for (size_t i = 0; i < cell_size; i++)
    {
      const double Theta_sat = soil.Theta (i, 0.0, 0.0);
      daisy_assert (Theta_sat >= X_ice_[i]);
      h_ice_.push_back (soil.h (i, Theta_sat - X_ice_[i]));
    }
  daisy_assert (h_ice_.size () == cell_size);

  geo.initialize_layer (Theta_, al, "Theta", msg);
  geo.initialize_layer (h_, al, "h", msg);

  // If both are specified, make sure they are consistent.
  for (size_t i = 0; i < Theta_.size () && i < h_.size (); i++)
    {
      const double Theta_h = soil.Theta (i, h_[i], h_ice (i));
      if (!approximate (Theta_[i], Theta_h))
        {
          std::ostringstream tmp;
          tmp << "Theta[" << i << "] (" << Theta_[i] << ") != Theta (" 
              << h_[i] << ") (" << Theta_h << ")";
          msg.error (tmp.str ());
        }
      Theta_[i] = Theta_h;
    }

  size_t h_size = h_.size ();
  size_t Theta_size = Theta_.size ();
  
  // Fill out with last value.
  if (use_last && Theta_size > 0)
    while (Theta_.size () < cell_size)
      Theta_.push_back (Theta_[Theta_.size () - 1]);
  if (use_last && h_size > 0)
    while (h_.size () < cell_size)
      h_.push_back (h_[h_.size () - 1]);
  h_size = h_.size ();
  Theta_size = Theta_.size ();


  // h based on Theta.
  for (size_t i = h_size; i < Theta_size; i++)
    h_.push_back (soil.h (i, Theta_[i]));
  h_size = h_.size ();

  // Groundwater based pressure.
  if (std::isfinite (default_h))
    {
      for (size_t i = h_size; i < cell_size; i++)
	h_.push_back (default_h - geo.cell_z (i));
    }
  else if (groundwater.table () > 0.0)
    {
      const double h_pF2 = -100.0; // pF 2.0;
      for (size_t i = h_size; i < cell_size; i++)
	h_.push_back (h_pF2);
    }
  else
    {
      const double table = groundwater.table ();

      for (size_t i = h_size; i < cell_size; i++)
	h_.push_back (std::max (-100.0, table - geo.cell_z (i)));
    }                        
  daisy_assert (h_.size () == cell_size);

  // Theta based on h.
  for (size_t i = Theta_size; i < cell_size; i++)
    Theta_.push_back (soil.Theta (i, h_[i], h_ice (i)));
  daisy_assert (Theta_.size () == cell_size);

  // Sources.
  S_sum_.insert (S_sum_.begin (), cell_size, 0.0);
  S_root_.insert (S_root_.begin (), cell_size, 0.0);
  S_drain_.insert (S_drain_.begin (), cell_size, 0.0);
  S_indirect_drain_.insert (S_indirect_drain_.begin (), cell_size, 0.0);
  S_soil_drain_.insert (S_soil_drain_.begin (), cell_size, 0.0);
  S_incorp_.insert (S_incorp_.begin (), cell_size, 0.0);
  tillage_.insert (tillage_.begin (), cell_size, 0.0);
  S_B2M_.insert (S_B2M_.begin (), cell_size, 0.0);
  S_M2B_.insert (S_M2B_.begin (), cell_size, 0.0);
  S_p_drain_.insert (S_p_drain_.begin (), cell_size, 0.0);
  if (S_permanent_.size () < cell_size)
    S_permanent_.insert (S_permanent_.end (), cell_size - S_permanent_.size (),
                         0.0);
  S_ice_ice.insert (S_ice_ice.begin (), cell_size, 0.0);
  S_ice_water_.insert (S_ice_water_.begin (), cell_size, 0.0);
  S_forward_total_.insert (S_forward_total_.begin (), cell_size, 0.0);
  S_forward_sink_.insert (S_forward_sink_.begin (), cell_size, 0.0);

  // Fluxes.
  q_primary_.insert (q_primary_.begin (), edge_size, 0.0);
  q_secondary_.insert (q_secondary_.begin (), edge_size, 0.0);
  q_matrix_.insert (q_matrix_.begin (), edge_size, 0.0);
  q_tertiary_.insert (q_tertiary_.begin (), edge_size, 0.0);

  // Update conductivity and primary/secondary water.
  Theta_primary_.insert (Theta_primary_.begin (), cell_size, -42.42e42);
  Theta_secondary_.insert (Theta_secondary_.begin (), cell_size, -42.42e42);
  Theta_tertiary_.insert (Theta_tertiary_.begin (), cell_size, 0.0);
  K_cell_.insert (K_cell_.begin (), cell_size, 0.0);
  Cw2_.insert (Cw2_.begin (), cell_size, -42.42e42);
  tick_after (geo, soil,  soil_heat, true, msg);

  // We just assume no changes.
  h_old_ = h_;
  Theta_old_ = Theta_;
  X_ice_old_ = X_ice_;
}

SoilWater::SoilWater (const Block& al)
  : default_h (al.number ("default_h", NAN)),
    max_exfiltration_gradient (al.number ("max_exfiltration_gradient", -1.0)),
    max_sink_change (al.number ("max_sink_change")),
    use_last (al.flag ("use_last")),
    S_permanent_ (al.number_sequence ("S_permanent")),
    sink_dt (NAN),
    sink_cell (Geometry::cell_error),
    table_low (NAN),
    table_high (NAN)
{ }

SoilWater::~SoilWater ()
{ }

static DeclareSubmodel 
soil_water_submodel (SoilWater::load_syntax, "SoilWater", "\
Keep track of water and pressure in the soil matrix.");

// soil_water.C ends here.
