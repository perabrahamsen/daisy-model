// movement_rect.C --- Movement in a rectangular 2D grid.
// 
// Copyright 2006 Per Abrahamsen and KVL.
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

#include "movement.h"
#include "geometry_rect.h"
#include "soil.h"
#include "soil_water.h"
#include "soil_heat.h"
#include "solute.h"
#include "element.h"
#include "surface.h"
#include "groundwater.h"
#include "weather.h"
#include "log.h"
#include "submodeler.h"

#include <sstream>

struct MovementRect : public Movement
{
  // Geometry.
  std::auto_ptr<GeometryRect> geo;
  Geometry& geometry () const;

  // Water.
  std::auto_ptr<UZmodel> uzdefault;
  std::auto_ptr<UZmodel> uzreserve;
  std::auto_ptr<UZ1D> uzhor;
  void macro_tick (const Soil&, SoilWater&, Surface&, Treelog&);
  void water_column (const Soil& soil, const SoilHeat& soil_heat, 
                     Surface& surface, Groundwater& groundwater,
                     const size_t top_cell, const size_t bottom_cell,
                     const std::vector<double>& S,
                     std::vector<double>& h_old,
                     const std::vector<double>& Theta_old,
                     const std::vector<double>& h_ice,
                     std::vector<double>& h,
                     std::vector<double>& Theta,
                     const size_t q_offset,
                     std::vector<double>& q,
                     std::vector<double>& q_p,
                     Treelog& msg);
  // Solute.
  std::auto_ptr<Transport> transport; // Solute transport model in matrix.
  std::auto_ptr<Transport> reserve; // Reserve solute transport model in matr.
  std::auto_ptr<Transport> last_resort; // Last resort solute transport model.
  std::auto_ptr<Transport> transport_solid; // Pseudo transport for non-solutes
  void solute (const Soil& soil, const SoilWater& soil_water,
               const double J_in, Solute& solute,
               Treelog& msg);
  void element (const Soil& soil, const SoilWater& soil_water,
                Element& element, Adsorption& adsorption,
                const double diffusion_coefficient, Treelog& msg);
  static void flow (const GeometryRect& geo, 
                    const Soil& soil, 
                    const SoilWater& soil_water, 
                    const std::string& name,
                    std::vector<double>& M, 
                    std::vector<double>& C, 
                    std::vector<double>& S, 
                    std::vector<double>& /* S_p */, 
                    std::vector<double>& J, 
                    std::vector<double>& /* J_p */, 
                    Adsorption& adsorption,
                    double /* diffusion_coefficient */,
                    Treelog& msg);

  // Management.
  void ridge (Surface&, const Soil&, const SoilWater&, const AttributeList&);

  // Heat.
  /* const */ double delay;	// Period delay [ cm/rad ??? ]
  double bottom_heat (const Time&, const Weather&) const ;
  std::vector<double> default_heat (const Soil&, 
                                    const Time&, const Weather&);
  double surface_snow_T (const Soil&, const SoilWater&, const SoilHeat&,
                         const double T_snow, const double K_snow,
                         const double dZs) const;

  // Simulation.
  void tick (const Soil& soil, SoilWater& soil_water, SoilHeat& soil_heat,
             Surface& surface, Groundwater& groundwater, const Time&,
             const Weather&, Treelog& msg);
  void output (Log&) const;

  // Create.
  bool check (Treelog&) const;
  void initialize (const AttributeList&, const Soil&, const Groundwater&, 
                   Treelog&);
  MovementRect (Block& al);

};

Geometry& 
MovementRect::geometry () const
{ return *geo; }

void
MovementRect::macro_tick (const Soil&, SoilWater&, Surface&, Treelog&)
{ }

void
MovementRect::water_column (const Soil& soil, const SoilHeat& soil_heat, 
                            Surface& surface, Groundwater& groundwater,
                            const size_t top_cell, const size_t bottom_cell,
                            const std::vector<double>& S,
                            std::vector<double>& h_old,
                            const std::vector<double>& Theta_old,
                            const std::vector<double>& h_ice,
                            std::vector<double>& h,
                            std::vector<double>& Theta,
                            const size_t q_offset,
                            std::vector<double>& q,
                            std::vector<double>& q_p,
                            Treelog& msg)
{
  // Find top edge.
  const size_t top_edge = top_cell + q_offset;
  daisy_assert (geo->edge_to (top_edge) == Geometry::cell_above);
  daisy_assert (q.size () > bottom_cell + 1);

  // Limit for ridging.
  const size_t first = top_cell +
    (surface.top_type (*geo, top_edge) == Surface::soil
     ?  surface.last_cell (*geo, top_edge) : 0);

  // Limit for groundwater table.
  size_t last = bottom_cell;
  if (groundwater.bottom_type () == Groundwater::pressure)
    {
      if (groundwater.table () <= geo->zminus (bottom_cell))
        throw ("Groundwater table in or below lowest cell.");

      while (groundwater.table () > geo->zminus (last) && last > first)
        last--;

      // Pressure at the last cell is equal to the water above it.
      for (size_t i = last + 1; i <= bottom_cell; i++)
        h_old[i] = h[i] = groundwater.table () - geo->z (i);
    }

  bool ok = true;

  // Calculate matrix flow next.
  try
    {
      ok = uzdefault->tick (msg, *geo, soil, soil_heat,
                            first, surface, top_edge, last, groundwater,
                            S, h_old, Theta_old, h_ice, h, Theta, 
                            q_offset, q);
    }
  catch (const char* error)
    {
      msg.warning (std::string ("UZ problem: ") + error);
      ok = false;
    }
  catch (const std::string& error)
    {
      msg.warning (std::string ("UZ trouble: ") + error);
      ok = false;
    }
  if (!ok)
    {
      msg.message ("Using reserve uz model.");
      uzreserve->tick (msg, *geo, soil, soil_heat,
                       first, surface, top_edge, last, groundwater,
                       S, h_old, Theta_old, h_ice, h, Theta, 
                       q_offset, q);
    }

  for (size_t i = last + 2; i <= bottom_cell + 1; i++)
    {
      daisy_assert (q.size () > i + q_offset);
      q[i + q_offset] = q[i-1 + q_offset];
      q_p[i + q_offset] = q_p[i-1 + q_offset];
    }

  // Update Theta below groundwater table.
  if (groundwater.bottom_type () == Groundwater::pressure)
    {
      for(size_t i = last + 1; i < soil.size (); i++)
        Theta[i] = soil.Theta (i, h[i], h_ice[i]);
    }
}

void
MovementRect::solute (const Soil& soil, const SoilWater& soil_water,
                      const double J_in, Solute& solute,
                      Treelog& msg)
{ 
  Treelog::Open nest (msg, "Movement: " + name);
  const size_t edge_size = geo->edge_size ();
  const size_t cell_size = geo->cell_size ();

  solute.tick (cell_size, soil_water);

  // Upper border.
  for (size_t e = 0; e < edge_size; e++)
    {
      if (geo->edge_to (e) != Geometry::cell_above)
        continue;

      solute.J_p[e] = 0.0;
      solute.J[e] = J_in;
    }

  // Flow.
  flow (*geo, soil, soil_water, solute.submodel, 
        solute.M_, solute.C_, 
        solute.S, solute.S_p,
        solute.J, solute.J_p, 
        *solute.adsorption, solute.diffusion_coefficient (), msg);
}

void 
MovementRect::element (const Soil& soil, const SoilWater& soil_water,
                       Element& element, Adsorption& adsorption,
                       const double diffusion_coefficient, Treelog& msg)
{
  element.tick (geo->cell_size (), soil_water);
  flow (*geo, soil, soil_water, "DOM", 
        element.M, element.C, element.S, element.S_p, 
        element.J, element.J_p, 
        adsorption, diffusion_coefficient, msg);
}

void
MovementRect::flow (const GeometryRect& geo, 
                    const Soil& soil, 
                    const SoilWater& soil_water, 
                    const std::string& name,
                    std::vector<double>& M, 
                    std::vector<double>& C, 
                    std::vector<double>& S, 
                    std::vector<double>& /* S_p */, 
                    std::vector<double>& J, 
                    std::vector<double>& /* J_p */, 
                    Adsorption& adsorption,
                    double /* diffusion_coefficient */,
                    Treelog& msg)
{
  const size_t edge_size = geo.edge_size ();
  const size_t cell_size = geo.cell_size ();

  // Remember old content for checking mass balance later.
  const double old_content = geo.total (M);
  double in = 0.0;	
  double out = 0.0; 

  // Upper border.
  for (size_t e = 0; e < edge_size; e++)
    {
      if (geo.edge_to (e) == Geometry::cell_above)
        {
          const size_t n = geo.edge_from (e);
          M[n] -= J[e] * geo.edge_area (e) / geo.volume (n);
          in -= J[e] * geo.edge_area (e);
        }
      else
        daisy_assert (!std::isnormal (J[e]));
    }

  // Cell fluxes.
  for (size_t n = 0; n < cell_size; n++)
    {
      M[n] += S[n] * dt;
      C[n] = adsorption.M_to_C (soil, soil_water.Theta (n), n, M[n]);

      if (!(M[n] >= 0.0))
        {

          std::ostringstream tmp;
          tmp << "BUG: M[" << n << "] = " << M[n] 
              << " (J[0] = " << J[0] << ") S[" << n << "] = " << S[n];
          msg.error (tmp.str ());
          M[n] = C[n] = 0.0;
        }
      daisy_assert (M[n] >= 0.0);
      daisy_assert (C[n] >= 0.0);
    }

  // Mass balance.
  const double new_content = geo.total (M);
  const double delta_content = new_content - old_content;
  const double source = geo.total (S);
  // BUG: ASSume uniform boundaries.
  const double expected = source + in - out;
  if (!approximate (delta_content, expected)
      && new_content < fabs (expected) * 1e10)
    {
      std::ostringstream tmp;
      tmp << __FILE__ << ":" << __LINE__ << ": " << name
          << ": mass balance new - old != source + in - out\n"
          << new_content << " - " << old_content << " != " 
          << source << " + " << in << " - " << out << " (error "
          << (delta_content - expected) << ")";
      msg.error (tmp.str ());
    }
}

void
MovementRect::ridge (Surface&, const Soil&, const SoilWater&, 
                     const AttributeList&)
{ throw "Can't make ridges on a rectangular grid"; }

double
MovementRect::surface_snow_T (const Soil& soil,
                              const SoilWater& soil_water,
                              const SoilHeat& soil_heat,
                              const double T_snow,
                              const double K_snow,
                              const double dZs) const
{
  // We just use the first cell, ignore rest of surface.

  // Information about soil.
  const double K_soil 
    = soil.heat_conductivity (0, soil_water.Theta (0),
                              soil_water.X_ice (0)) 
    * 1e-7 * 100.0 / 3600.0; // [erg/cm/h/dg C] -> [W/m/dg C]
  const double Z = -geo->z (0) / 100.0; // [cm] -> [m]
  const double T_soil = geo->content_at (soil_heat, &SoilHeat::T, Z); // [dg C]

  return (K_soil / Z * T_soil + K_snow / dZs * T_snow) 
    / (K_soil / Z + K_snow / dZs);
}

double 
MovementRect::bottom_heat (const Time& time, const Weather& weather) const 
{ return weather.T_normal (time, delay); }

std::vector<double> 
MovementRect::default_heat (const Soil& soil, 
                            const Time& time, const Weather& weather)
{
  // Fetch average temperatur.
  const double rad_per_day = 2.0 * M_PI / 365.0;

  // Calculate delay.
  const double pF_2_0 = -100.0;
  double k = 0;
  double C = 0;

  std::vector<double> T;
  T.insert (T.begin (), geo->cell_size (), -42.42e42);

  const size_t cell_rows = geo->cell_rows ();
  const size_t cell_columns = geo->cell_columns ();
  
  for (size_t row = 0; row < cell_rows; row++)
    {
      for (size_t column = 0; column < cell_columns; column++)
        {
          const size_t cell = geo->cell_index (row, column);
          const double volume = geo->volume (cell);
          const double Theta_pF_2_0 = soil.Theta (cell, pF_2_0, 0.0);
          k += volume * soil.heat_conductivity (cell, Theta_pF_2_0, 0.0);
          C += volume * soil.heat_capacity (cell, Theta_pF_2_0, 0.0);
        }
      const double depth = geo->zplus (geo->cell_index (row, 0));
      const double a = k / C;
      delay = depth / sqrt (24.0 * 2.0 * a / rad_per_day);
      const double heat = bottom_heat (time, weather);

      for (size_t column = 0; column < cell_columns; column++)
        T[geo->cell_index (row, column)] = heat;
    }
  return T;
}

void
MovementRect::tick (const Soil& soil, SoilWater& soil_water, 
                    SoilHeat& soil_heat,
                    Surface& surface, Groundwater& groundwater, 
                    const Time&,
                    const Weather&, Treelog& msg) 
{
  const size_t cell_size = geo->cell_size ();
  const size_t edge_size = geo->edge_size ();
  const size_t cell_rows = geo->cell_rows ();
  const size_t cell_columns = geo->cell_columns ();
  const size_t edge_rows = geo->edge_rows ();

  soil_water.tick (cell_size, soil, msg); 

  // Vertical movement.
  for (size_t col = 0; col < cell_columns; col++)
    {
      // Find relevant cells.
      const size_t c_first = col * cell_rows;
      const size_t c_last = (col + 1U) * cell_rows - 1U;

      // Find relevant edges.
      const size_t e_first = col * edge_rows;
      const size_t e_last = (col + 1U) * edge_rows - 1U;

      // Check that they match.
      daisy_assert (geo->edge_to (e_first) == Geometry::cell_above);
      daisy_assert (geo->edge_from (e_first) == c_first);
      daisy_assert (geo->edge_to (e_last) == c_last);
      daisy_assert (geo->edge_from (e_last) == Geometry::cell_below);

      water_column (soil, soil_heat, surface, groundwater, 
                    c_first, c_last,
                    soil_water.S_sum_, soil_water.h_old_, 
                    soil_water.Theta_old_,
                    soil_water.h_ice_, soil_water.h_, soil_water.Theta_,
                    col, soil_water.q_, soil_water.q_p_,
                    msg);
   }
  // Update surface and groundwater reservoirs.
  for (size_t edge = 0; edge < edge_size; edge++)
    {
      if (geo->edge_to (edge) == Geometry::cell_above)
        surface.accept_top (soil_water.q (edge) * dt, *geo, edge, msg);
      if (geo->edge_from (edge) == Geometry::cell_below)
        groundwater.accept_bottom ((soil_water.q (edge)
                                    + soil_water.q_p (edge)) * dt,
                                   *geo, edge);
    }

#if 1
  // Horizontal movement.
  for (size_t row = 0; row < cell_rows; row++)
    {
      std::vector<size_t> cells;
      std::vector<int> edges;
      
      for (size_t col = 0; col < cell_columns; col++)
        cells.push_back (geo->cell_index (row, col));

      edges.push_back (-1);
      for (size_t col = 1; col < cell_columns; col++)
        {
          const int edge = geo->edge_index (cells[col-1], cells[col]);
          daisy_assert (edge >= 0);
          daisy_assert (edge < geo->edge_size ());
          daisy_assert (geo->edge_from (edge) == cells[col-1]);
          daisy_assert (geo->edge_to (edge) == cells[col]);
          daisy_assert (approximate (geo->z (cells[col-1]),
                                     geo->z (cells[col])));
          edges.push_back (edge);
        }
      edges.push_back (-1);
      daisy_assert (cells.size () + 1 == edges.size ());

      SMM1D smm (*geo, soil, soil_water, soil_heat, cells, edges);

      try 
        {
          uzhor->tick (smm, 0.0, msg);
        }
      catch (const char* error)
        {
          msg.warning (std::string ("UZhor problem: ") + error);
        }
      catch (const std::string& error)
        {
          msg.warning (std::string ("UZhor trouble: ") + error);
        }
    }
#endif
}

void 
MovementRect::output (Log&) const
{ }

bool
MovementRect::check (Treelog&) const
{ return true; }

void 
MovementRect::initialize (const AttributeList&,
                          const Soil&, const Groundwater&, 
                          Treelog&)
{
  const bool has_macropores = false;
  uzdefault->has_macropores (has_macropores);
  uzreserve->has_macropores (has_macropores);
}

MovementRect::MovementRect (Block& al)
  : Movement (al),
    geo (submodel<GeometryRect> (al, "Geometry")),
    uzdefault (Librarian<UZmodel>::build_item (al, "UZdefault")),
    uzreserve (Librarian<UZmodel>::build_item (al, "UZreserve")),
    uzhor (Librarian<UZ1D>::build_item (al, "UZhor")),
    transport (Librarian<Transport>::build_item (al, "transport")),
    reserve (Librarian<Transport>::build_item (al, "transport_reserve")),
    last_resort (Librarian<Transport>::build_item (al, 
                                                   "transport_last_resort")),
    transport_solid (Librarian<Transport>::build_item (al, "transport_solid"))
{ }

static struct MovementRectSyntax
{
  static Movement& make (Block& al)
  { return *new MovementRect (al); }

  MovementRectSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", 
               "Two dimensional movement in a rectangular grid.");
    syntax.add_submodule ("Geometry", alist, Syntax::State,
                          "Discretization of the soil.",
                          GeometryRect::load_syntax);
    syntax.add ("UZdefault", Librarian<UZmodel>::library (),
                "Main water transport model in unsaturated zone.");
    alist.add ("UZdefault", UZmodel::default_model ());
    syntax.add ("UZreserve", Librarian<UZmodel>::library (),
                "Reserve transport model if UZtop fails.");
    alist.add ("UZreserve", UZmodel::reserve_model ());
    syntax.add ("UZhor", Librarian<UZmodel>::library (),
                "Horizonatl transport model for soil matrix water.");
    alist.add ("UZhor", UZ1D::default_model ());
    syntax.add ("transport", Librarian<Transport>::library (), 
                "Solute transport model in matrix.");
    alist.add ("transport", Transport::default_model ());
    syntax.add ("transport_reserve", Librarian<Transport>::library (),
                "Reserve solute transport if the primary model fails.");
    alist.add ("transport_reserve", Transport::reserve_model ());
    syntax.add ("transport_last_resort", Librarian<Transport>::library (),
                "Last resort solute transport if the reserve model fails.");
    alist.add ("transport_last_resort", Transport::none_model ());
    syntax.add ("transport_solid", Librarian<Transport>::library (),
                "Transport model for non-dissolvable chemicals.\n\
Should be 'none'.");
    alist.add ("transport_solid", Transport::none_model ());
 
    Librarian<Movement>::add_type ("rectangle", alist, syntax, &make);
  }
} MovementRect_syntax;
