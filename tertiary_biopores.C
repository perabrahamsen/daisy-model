// tertiary_biopores.C --- Static vertical biopores with a capacity.
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

#include "tertiary.h"
#include "biopore.h"
#include "memutils.h"
#include "librarian.h"
#include "block_model.h"
#include "check.h"
#include "vcheck.h"
#include "geometry.h"
#include "soil.h"
#include "soil_water.h"
#include "soil_heat.h"
#include "log.h"
#include "anystate.h"
#include "surface.h"
#include "chemical.h"
#include "groundwater.h"
#include "im.h"
#include "units.h"
#include "treelog.h"
#include "assertion.h"
#include "mathlib.h"
#include "frame.h"
#include <sstream>

struct TertiaryBiopores : public Tertiary
{
  // Parameters.
  const auto_vector<Biopore*> classes; // List of biopore classes.
  const double pressure_initiate;// Pressure needed to init pref.flow [cm]
  const double pressure_end;	 // Pressure after pref.flow has been init [cm]
  const double pressure_limit;   // Limit to biopore sucktion [cm]
  const double pressure_barrier; // Pressure difference requires for S [cm]
  const double pond_max;	 // Pond height before activating pref.flow [cm]
  const enum active_msg_t { msg_cell, msg_range, msg_none } active_msg;

  // Helper.
  std::unique_ptr<Scalar> per_surface_area;
  std::vector<double> K;        // Adjusted matrix2biopore conductivity [cm/h]
  std::vector<double> K_crack;  // Secondary domain conductivity [cm/h]

  // Identity.
  bool has_macropores ()
  { return classes.size () > 0; }

  // State.
  std::vector<bool> active;      // Biopore activity 
  struct MyContent : public Anystate::Content
  {
    std::vector<Anystate> states;
    std::unique_ptr<Anystate::Content> clone () const
    { 
      std::unique_ptr<Anystate::Content> copy (new MyContent (states)); 
      return copy;
    }
    MyContent (const std::vector<Anystate>& s)
      : states (s)
    { }
  };
  Anystate get_state () const;
  void set_state (const Anystate&);

  // Timestep.
  mutable double old_dt;        // Lowest dt suggested by weather for event [h]

  // Log variables.
  double water_volume;          // Volume of water stored in biopores.
  double water_height;          // -||- converted to height.
  IM solute_mass;               // Mass of solutes in biopores.
  IM solute_storage;            // -||- per surface area.
  double ddt;
  
  // Infiltration.
  double infiltration_capacity (const Geometry&, size_t e, double dt);
  void infiltrate (const Geometry&, size_t e, double amount, double dt);
  void clear ();

  // - For use by column.
  void tick_source (const Geometry&, const Soil&, const SoilHeat&, 
                    SoilWater&, Treelog&);
  double suggest_dt (double weather_dt, double max_pond) const;
  void tick (const Units&, const Geometry& geo, const Soil& soil, 
             const SoilHeat& soil_heat, const double dt, 
             SoilWater& soil_water, Surface& surface, Treelog& msg);
  void remove_solute (const symbol chem)
  { 
    for (auto b : classes)
      b->remove_solute (chem);
  }
  double total_solute (const Geometry& geo, const symbol chem) const // [g/m^2]
  { 
    double total = 0.0;
    for (auto b : classes)
      total += b->total_solute (geo, chem);
    return total;
  }

  // - For use in Richard's Equation.
  void matrix_sink (std::vector<double>& S_B2M,
                    std::vector<double>& S_M2B,
                    std::vector<double>& S_drain,
                    std::vector<double>& S_tertiary_drain) const;
  
  void update_biopores (const Geometry& geo, 
                        const Soil& soil,  
                        const std::vector<double>& h,
                        const double dt);
  void update_water ();
  void find_implicit_water (const Anystate& old_state, 
                            const Geometry& geo, 
                            const Soil& soil,  
                            const std::vector<double>& h,
                            const double dt);
  void update_active (const Geometry&, const std::vector<double>& h, Treelog&);

  // - For use in Movement::solute.
  void solute (const Geometry&, const SoilWater&,
               const std::map<size_t, double>& J_tertiary,
               const double /* dt */,
               Chemical&, Treelog&);

  // - Output.
  double total_water () const;  // [cm^3]
  void get_solute (IM& im) const; // [g]
  void output (Log&) const;
  
  // Create and Destroy.
public:
  bool initialize (const Units&, 
                   const Geometry&, const Soil&, SoilWater&, 
                   const Scope& parent_scope, 
                   const Groundwater&, Treelog& msg);
  bool check (const Geometry&, Treelog& msg) const;
  explicit TertiaryBiopores (const BlockModel& al);
};

Anystate
TertiaryBiopores::get_state () const
{
  const size_t classes_size = classes.size ();
  std::vector<Anystate> biopore_state;
  for (size_t b = 0; b < classes_size; b++)
    biopore_state.push_back (classes[b]->get_state ());

  std::unique_ptr<Anystate::Content> copy (new MyContent (biopore_state));
  return Anystate (std::move (copy));
}
 
void 
TertiaryBiopores::set_state (const Anystate& state)
{
  const MyContent& content = static_cast<const MyContent&> (state.inspect ());
  const size_t classes_size = classes.size ();
  daisy_assert (classes_size == content.states.size ());
  for (size_t b = 0; b < classes_size; b++)
    classes[b]->set_state (content.states[b]);
}

double
TertiaryBiopores::infiltration_capacity (const Geometry& geo, size_t e, const double dt)
{
  double sum = 0.0; 
  const size_t classes_size = classes.size ();
  for (size_t b = 0; b < classes_size; b++)
    sum += classes[b]->infiltration_capacity (geo, e, dt);
  return sum;
}

void
TertiaryBiopores::infiltrate (const Geometry& geo, const size_t e, 
                              double amount, const double dt)
{
  const size_t cell = geo.edge_other (e, Geometry::cell_above);
  daisy_assert (cell < geo.cell_size ());

  // Find total macropore density.
  double total_density = 0.0;
  const size_t classes_size = classes.size ();
  for (size_t b = 0; b < classes_size; b++)
    total_density += classes[b]->top_density (cell);
  
  // We divide the water relative to the biopore density.  However,
  // some biopores may not be able to contain their share of the
  // water.  If so, they are filled to capacity, and the remaining
  // water is redistributed to the remaining classes.  We implement
  // this in two steps.
  std::vector<Biopore*> remaining = classes;

  // Step one. Distribute to all biopore classes that are limited by
  // capacity.  We have to try this repeatedly, as when we remove one
  // biopore class from consideration, the remaining classes will have
  // to pick up a larger share.
  std::vector<Biopore*>::iterator i = remaining.begin ();
  while (i != remaining.end ())
    {
      // Check that there is something to do.
      if (total_density <= 0 || amount <= 0.0)
        return;

      Biopore& biopore = *(*i);
      const double density = biopore.top_density (cell);
      if (!std::isnormal (density))
        {
          // No macropores here.
          i++;
          continue;
        }

      daisy_assert (total_density > 0.0);
      const double share = amount * density / total_density;
      const double capacity = biopore.infiltration_capacity (geo, e, dt);
      if (capacity <= share)
        {
          // Insuffient space, fill it up.  
          biopore.infiltrate (geo, e, capacity, dt);

          // Now remove the biopore class for consideration.
          amount -= capacity;
          total_density -= density;
          remaining.erase (i);
          i = remaining.begin (); // Retry with new values.
        }
      else
        // Sufficient capacity, handle in next loop.
        i++;
    }

  // Step two.  Distribute to remaining biopore classes relative to
  // density.  We know they have the capacity.

  // Check that there is something to do.
  if (total_density <= 0 || amount <= 0.0)
    return;

  for (std::vector<Biopore*>::iterator i = remaining.begin ();
       i != remaining.end ();
       i++)
    {
      Biopore& biopore = *(*i);
      const double density = biopore.top_density (cell);
      if (!std::isnormal (density))
        // No macropores here.
        continue;

      // Infiltrate according to relative density.
      daisy_assert (total_density > 0.0);
      const double share = amount * density / total_density;
      biopore.infiltrate (geo, e, share, dt);
    }
}

void
TertiaryBiopores::clear ()
{
  const size_t classes_size = classes.size ();
  for (size_t b = 0; b < classes_size; b++)
    classes[b]->clear ();
}

void
TertiaryBiopores::tick_source (const Geometry& geo, const Soil& soil,
                               const SoilHeat& soil_heat, 
                               SoilWater& soil_water, Treelog& msg)
{
  TREELOG_MODEL (msg);

  // Clear old infiltration.
  clear ();

  // Find matrix state.
  const std::vector<double>& h = soil_water.h_all ();
  update_active (geo, h, msg);

  // Find conductivity.
  const size_t cell_size = geo.cell_size ();
  daisy_assert (K.size () == cell_size);
  daisy_assert (K_crack.size () == cell_size);
  for (size_t c = 0; c < cell_size; c++)
    {
      const double h_cond = std::min(pressure_initiate, h[c]);
      const double T = soil_heat.T (c);
      const double h_ice = soil_water.h_ice (c); 
      const double K_zz = soil.K (c, h_cond, h_ice, T);
      const double anisotropy = soil.anisotropy_cell (c);
      const double K_xx = K_zz * anisotropy;
      K[c] = K_xx;
      const double h_lim = soil.h_secondary (c);
      if (h_lim >= 0.0)
        K_crack[c] = -42.42e24;
      else
        K_crack[c] = soil.K (c, h_lim + 0.001, h_ice, T) * anisotropy;
    }
  // Prepare classes and find forward source.
  std::vector<double> S_forward_total (cell_size, 0.0);
  std::vector<double> S_forward_sink (cell_size, 0.0);
  std::vector<double> S_forward (cell_size);
  for (size_t b = 0; b < classes.size (); b++)
    {
      classes[b]->tick_source (geo, active, h);
      std::fill (S_forward.begin (), S_forward.end (), 0.0);
      classes[b]->forward_sink (geo, active, K, K_crack, pressure_barrier,
                                pressure_limit, h, S_forward);
      for (size_t c = 0; c < cell_size; c++)
        {
          S_forward_total[c] += S_forward[c];
          if (S_forward[c] > 0.0)
            S_forward_sink[c] += S_forward[c];
        }
    }

  soil_water.forward_sink (S_forward_total, S_forward_sink);
}

double 
TertiaryBiopores::suggest_dt (const double weather_dt,
                              const double max_pond) const
{
  if (max_pond > pond_max)
    {
      if (std::isnormal (weather_dt) && weather_dt < old_dt)
        old_dt = weather_dt;
      
      return old_dt;
    }
  else
    old_dt = weather_dt;

  return 0.0;
}

void
TertiaryBiopores::tick (const Units&, const Geometry& geo, const Soil& soil, 
                        const SoilHeat& soil_heat, const double dt, 
                        SoilWater& soil_water, Surface& surface, Treelog& msg)
{
  TREELOG_MODEL (msg);

  // Flux.
  const size_t edge_size = geo.edge_size ();
  std::vector<double> q_tertiary (edge_size, 0.0);

  // Infiltration.
  const std::vector<size_t>& edge_above = geo.cell_edges (Geometry::cell_above);
  const size_t edge_above_size = edge_above.size ();

  // Find new flux.
  for (size_t i = 0; i < edge_above_size; i++)
    {
      const size_t edge = edge_above[i];
      
      if (surface.h_top (geo, edge) < pond_max)
        q_tertiary[edge] = 0.0;
      else
        {
          const double in_sign 
            = geo.cell_is_internal (geo.edge_to (edge)) ? 1.0 : -1.0;
          const double max_surface 
            = in_sign * surface.q_top (geo, edge, dt) * dt;
          const double flux_in
            = std::min (infiltration_capacity (geo, edge, dt), 
                        max_surface) / dt;
          q_tertiary[edge] = in_sign * flux_in;
        }
    }
  
  // Apply flux. 
  for (size_t i = 0; i < edge_above_size; i++)
    {
      const size_t edge = edge_above[i];
      const double in_sign 
        = geo.cell_is_internal (geo.edge_to (edge)) ? 1.0 : -1.0;
      const double flux_in = q_tertiary[edge] * in_sign;
      surface.accept_top (in_sign * flux_in * dt, geo, edge, dt, msg);
      infiltrate (geo, edge, flux_in * dt, dt);
    }
  update_water ();

  // Update soil and surface water.
  surface.update_pond_average (geo);

  // Soil matrix exchange.
  const size_t cell_size = geo.cell_size ();
  std::vector<double> S_drain (cell_size, 0.0);
  std::vector<double> S_B2M (cell_size, 0.0);
  std::vector<double> S_M2B (cell_size, 0.0);
  std::vector<double> S_tertiary_drain (cell_size, 0.0);

  // Keep original state.
  const Anystate old_state = get_state ();

  // Find an implicit solution.
  const std::vector<double>& h = soil_water.h_all ();
  ddt = dt;
  find_implicit_water (old_state, geo, soil, h, ddt);
  
  // Limit sink.
  matrix_sink (S_B2M, S_M2B, S_drain, S_tertiary_drain);
  for (size_t c = 0; c < cell_size; c++)
    {
      const double Theta_loss = (S_drain[c] + S_M2B[c] - S_B2M[c]) * ddt;
      if (Theta_loss <= 0.01)
        // Less than one percent, ignore it.
        continue;
      const double Theta = soil_water.Theta (c);
      const double h_ice = soil_water.h_ice (c);
      const double Theta_min = soil.Theta (c, pressure_end, h_ice);
      const double allowed_loss = Theta - Theta_min;
      if (allowed_loss < 0.01)
        // Ice?
        continue;
      if (Theta_loss < allowed_loss + 0.01)
        // Less than one percent above allowed loss, deal with it.
        continue;
      
      // Scale down ddt so our loss does not exceed the allowed.
      const double factor = allowed_loss / Theta_loss;
      daisy_assert (factor > 0.0);
      daisy_assert (factor <= 1.0);
      ddt *= factor;
    }

  // Update tertiary state with new ddt.
  find_implicit_water (old_state, geo, soil, h, ddt);

  // Scale sink with timestep.
  const double scale = ddt / dt;
  if (!approximate (scale, 1.0))
    {
      for (size_t b = 0; b < classes.size (); b++)
        classes[b]->scale_sink (scale);
    }

  // Update tertiary water.
  for (size_t b = 0; b < classes.size (); b++)
    classes[b]->update_cell_water (geo, dt);

  // Make it official.
  std::fill (S_B2M.begin (), S_B2M.end (), 0.0);
  std::fill (S_M2B.begin (), S_M2B.end (), 0.0);
  std::fill (S_drain.begin (), S_drain.end (), 0.0);
  std::fill (S_tertiary_drain.begin (), S_tertiary_drain.end (), 0.0);
  matrix_sink (S_B2M, S_M2B, S_drain, S_tertiary_drain);

  std::vector<double> Theta_p (cell_size, 0.0);
  std::vector<double> q_p (edge_size, 0.0);
  for (size_t b = 0; b < classes.size (); b++)
    classes[b]->update_soil_tertiary (Theta_p, q_p);

  soil_water.set_tertiary (Theta_p, q_p, 
                           S_B2M, S_M2B, S_drain, S_tertiary_drain);
}

void 
TertiaryBiopores::matrix_sink (std::vector<double>& S_B2M,
                               std::vector<double>& S_M2B,
                               std::vector<double>& S_drain, 
                               std::vector<double>& S_tertiary_drain) const
{
  for (size_t b = 0; b < classes.size (); b++)
    classes[b]->add_to_sink (S_B2M, S_M2B, S_drain, S_tertiary_drain);
}

void 
TertiaryBiopores::update_biopores (const Geometry& geo, 
                                   const Soil& soil,  
                                   const std::vector<double>& h,
                                   const double dt) 
{
  for (size_t b = 0; b < classes.size (); b++)
    classes[b]->update_matrix_sink (geo, active, K, K_crack,
                                    pressure_barrier, 
                                    pressure_limit, h, dt);
}

void
TertiaryBiopores::update_water ()
{
  for (size_t b = 0; b < classes.size (); b++)
    classes[b]->update_water ();
}

void
TertiaryBiopores::find_implicit_water (const Anystate& old_state, 
                                       const Geometry& geo, 
                                       const Soil& soil,  
                                       const std::vector<double>& h,
                                       const double dt)
{
  // Reset water content to begining of timestep.
  set_state (old_state);
  const Anystate new_state = get_state ();
  // Find added water with "new water content".
  update_biopores (geo, soil, h, dt);
  // Reset water content to begining of timestep.
  set_state (old_state);
  // Add water to get new state.
  update_water ();

  // Update logs.
  water_volume = total_water ();
  water_height = water_volume / geo.surface_area ();
  get_solute (solute_mass);
  solute_storage 
    = solute_mass.multiply (*per_surface_area, solute_storage.unit ());
}

void
TertiaryBiopores::update_active (const Geometry& geo,
                                 const std::vector<double>& h,
                                 Treelog& msg)
{
  double top_act = geo.bottom () - 1.0;
  double bottom_act = geo.top () + 1.0;
  double top_dea = geo.bottom () - 1.0;
  double bottom_dea = geo.top () + 1.0;

  const size_t cell_size = geo.cell_size ();
  daisy_assert (active.size () == cell_size);
  for (size_t c = 0; c < cell_size; c++)
    {
      if (active[c])    // Biopore is active 
        {
          if (h[c] < pressure_end)
            {
              active[c] = false;
              bottom_dea = std::min (geo.cell_bottom (c), bottom_act);
              top_dea = std::max (geo.cell_top (c), top_act);
              if (active_msg == msg_cell)
                {
                  std::ostringstream tmp;
                  tmp << "h[" << c << "] = " << h[c] << ", deactivated";
                  msg.message (tmp.str ());
                }
            }
        }
      else              // Biopore is not active 
        {
          if (h[c] > pressure_initiate)
            {
              active[c] = true;
              bottom_act = std::min (geo.cell_bottom (c), bottom_dea);
              top_act = std::max (geo.cell_top (c), top_dea);
              if (active_msg == msg_cell)
                {
                  std::ostringstream tmp;
                  tmp << "h[" << c << "] = " << h[c] << ", activated";
                  msg.message (tmp.str ());
                }
            }
        }
    }
  if (active_msg == msg_range 
      && (top_act > bottom_act || top_dea > bottom_dea))
    {
      std::ostringstream tmp;
      tmp << "Biopores";
      if (top_act > bottom_act)
        tmp << " " << top_act << " cm to " << bottom_act 
            << " cm activated";
      if (top_dea > bottom_dea)
        tmp << " " << top_dea << " cm to " << bottom_dea 
            << " cm deactivated";
      msg.message (tmp.str ());  
    }
}

void 
TertiaryBiopores::solute (const Geometry& geo, const SoilWater& soil_water,
                          const std::map<size_t, double>& J_tertiary,
                          const double dt,
                          Chemical& chemical, Treelog& msg)
{
  const symbol chem = chemical.objid;

  // Surface infiltration.
  const std::vector<size_t>& edge_above = geo.cell_edges (Geometry::cell_above);
  const size_t edge_above_size = edge_above.size ();

  for (size_t i = 0; i < edge_above_size; i++)
    {
      const size_t edge = edge_above[i];
      const size_t cell = geo.edge_other (edge, Geometry::cell_above);
      std::map<size_t, double>::const_iterator p = J_tertiary.find (edge);
      if (p == J_tertiary.end ())
        continue;               // Nothing to do.

      // Find total amount.
      const double in_sign 
        = geo.cell_is_internal (geo.edge_to (edge)) ? 1.0 : -1.0;
      const double J_in = p->second * in_sign; // [g/cm^2/h]
      const double area = geo.edge_area (edge); // [cm^2]
      const double total_in = J_in * area * dt; // [g]
      daisy_assert (total_in >= 0.0); // No mass flux out.
      if (total_in < 1e-100)
        continue;               // Nothing to see here, move along.

      // Find total macropore density.
      double total_density = 0.0;
      const size_t classes_size = classes.size ();
      for (size_t b = 0; b < classes_size; b++)
        total_density += classes[b]->top_density (cell);
      daisy_assert (total_density > 0.0);

      // Give each class its share.
      for (size_t b = 0; b < classes.size (); b++)
        {
          Biopore& biopore = *classes[b];
          const double density = biopore.top_density (cell);
          const double share = total_in * density / total_density;
          biopore.solute_infiltrate (chem, geo, edge, share, dt);
        }
    }

  // Matrix exchange.
  for (size_t b = 0; b < classes.size (); b++)
    classes[b]->matrix_solute (geo, dt, chemical, msg);
}

double
TertiaryBiopores::total_water () const
{
  double total = 0.0;
  for (size_t b = 0; b < classes.size (); b++)
    total += classes[b]->total_water ();
  return total;
}

void
TertiaryBiopores::get_solute (IM& im) const
{
  im.clear ();
  for (size_t b = 0; b < classes.size (); b++)
    classes[b]->get_solute (im);
}
  
void 
TertiaryBiopores::output (Log& log) const
{
  output_list (classes, "classes", log, Biopore::component); 
  // TODO: output_variable (active, log);
  output_variable (water_volume, log);
  output_variable (water_height, log);
  output_submodule (solute_mass, "solute_mass", log);
  output_submodule (solute_storage, "solute_storage", log);
  output_variable (ddt, log);
}


bool 
TertiaryBiopores::initialize (const Units& units,
                              const Geometry& geo, const Soil&, 
			      SoilWater& soil_water,
                              const Scope& scope,
                              const Groundwater& groundwater, 
                              Treelog& msg)
{ 
  TREELOG_MODEL (msg);
  bool ok = true;

  const size_t cell_size = geo.cell_size ();
  const size_t edge_size = geo.edge_size ();
  std::vector<double> Theta_p (cell_size, 0.0);
  std::vector<double> q_p (edge_size, 0.0);
  for (size_t b = 0; b < classes.size (); b++)
    {
      Treelog::Open nest2 (msg, "classes", b, classes[b]->objid);
      if (!classes[b]->initialize (units, geo, scope, groundwater, msg))
        ok = false;
      classes[b]->update_soil_tertiary (Theta_p, q_p);
    }
  const std::vector<double> empty_sink (cell_size, 0.0);
  soil_water.set_tertiary (Theta_p, q_p, 
                           empty_sink, empty_sink, empty_sink, empty_sink);
  const double table = groundwater.table () > 0.0
    ? geo.bottom () - 1000.0
    : groundwater.table ();

  while (active.size () < cell_size)
    active.push_back (geo.cell_z (active.size ()) < table);
  while (K.size () < cell_size)
    K.push_back (NAN);
  while (K_crack.size () < cell_size)
    K_crack.push_back (NAN);

  water_volume = total_water ();
  water_height = water_volume / geo.surface_area ();

  static const symbol per_area ("cm^-2");
  per_surface_area.reset (new Scalar (1.0 / geo.surface_area (),
                                      units.get_unit (per_area)));

  return ok;
}

bool 
TertiaryBiopores::check (const Geometry& geo, Treelog& msg) const
{
  bool ok = true;
  for (size_t b = 0; b < classes.size (); b++)
    {
      Treelog::Open (msg, "classes", b, classes[b]->objid);
      if (!classes[b]->check (geo, msg))
        ok = false;
    }
  return ok;
}

TertiaryBiopores::TertiaryBiopores (const BlockModel& al)
  : Tertiary (al),
    classes (Librarian::build_vector<Biopore> (al, "classes")),
    pressure_initiate (al.number ("pressure_initiate")),
    pressure_end (al.number ("pressure_end")),
    pressure_limit (al.number ("pressure_limit", pressure_end)),
    pressure_barrier (al.number ("pressure_barrier")),
    pond_max (al.number ("pond_max")),
    active_msg (al.name ("active_msg") == "cell"
                ? msg_cell
                : ((al.name ("active_msg") == "range")
                   ? msg_range : msg_none)),
    active (al.check ("active")
            ? al.flag_sequence ("active")
            : std::vector<bool> ()),
    old_dt (al.number ("old_dt")),
    water_volume (-42.42e42),
    water_height (-42.42e42),
    solute_mass (al.units ().get_unit (IM::mass_unit ())),
    solute_storage (al.units ().get_unit (IM::storage_unit ())),
    ddt (-42.42e42)
{ }

static struct TertiaryBioporesSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new TertiaryBiopores (al); }

  TertiaryBioporesSyntax ()
    : DeclareModel (Tertiary::component, "biopores", "Tertiary domain divided into biopore classes.")
  { }
  static void load_mass (Frame& frame)
  { IM::add_syntax (frame, Attribute::LogOnly, IM::mass_unit ()); }
  static void load_storage (Frame& frame)
  { IM::add_syntax (frame, Attribute::LogOnly, IM::storage_unit ()); }

  void load_frame (Frame& frame) const
  { 

    frame.declare_object ("classes", Biopore::component, 
                       Attribute::State, Attribute::Variable,
                       "List of biopore classes.");
    frame.declare ("pressure_initiate", "cm", Attribute::Const, 
                "Pressure needed to activate biopore flow.");
    frame.set ("pressure_initiate", -3.0);
    frame.declare ("pressure_end", "cm", Attribute::Const, 
                "Pressure below which biopore flow is deactivated.");
    frame.set ("pressure_end", -30.0);
    frame.declare ("pressure_limit", "cm", Check::non_positive (),
                Attribute::OptionalConst, "\
Limit to pressure difference for moving matrix water gradient to biopores.\n\
\n\
The idea is that the water is extracted from the matrix by a\n\
hanging water column in the biopore, and that the suction is equal\n\
to the height of this water column.  The pressure limit is then the\n\
maximal length of the column, or the point where the column breaks.\n\
\n\
By default, this is equal to 'pressure_end'.");
    frame.declare ("pressure_barrier", "cm", Check::non_negative (), Attribute::Const,
                "Pressure barrier between matrix and biopore domain.\n\
If the pressure difference between the matrix and biopores is below\n\
this value, no water will tranfer between the domains.  If you specify\n\
a too small value for this parameter, the solution may be unstable.");
    frame.set ("pressure_barrier", 5.0);
    frame.declare ("pond_max", "cm", Check::non_negative (), Attribute::Const, "\
Maximum height of ponding before spilling into biopores.\n\
After macropores are activated pond will have this height.");
    frame.set ("pond_max", 0.05);
    frame.declare_string ("active_msg", Attribute::Const, "\
Control biopore activation and deactivation reports.\n\
\n\
Possible values:\n\
  cell: Report for each cell.\n\
  range: Report for vertical range.\n\
  none: No reports.");
    static VCheck::Enum active_check ("cell", "range", "none");
    frame.set_check ("active_msg", active_check);
    frame.set ("active_msg", "range");

    frame.declare_boolean ("active", Attribute::OptionalState,
                Attribute::SoilCells, "Active biopores in cells.");
    frame.declare ("old_dt", "cm", Check::non_negative (), Attribute::State, "\
Lowest dt imposed by weather during current event.\n\
In case of an event, the biopore module will suggest to keep a timestep\n\
that corresponds to the lowest suggestion from the weather module during\n\
the event.");
    frame.set ("old_dt", 0.0);
    frame.declare ("water_volume", "cm^3", Attribute::LogOnly, "Water volume.");    
    frame.declare ("water_height", "cm", Attribute::LogOnly,
                "Water volume multiplied with surface area.");
    frame.declare_submodule_sequence ("solute_mass", Attribute::LogOnly, "\
Total amount of solutes in biopores.", load_mass);
    frame.declare_submodule_sequence ("solute_storage", Attribute::LogOnly, "\
Total amount of solutes in biopores divided by surface area.", load_storage);
    frame.declare ("ddt", "h", Attribute::LogOnly, "Emulated timestep.\n\
Timestep scaled for available water.");    
    frame.declare_integer ("deactivate_steps", Attribute::State, 
                "No matrix exchange for this number of timesteps.\n\
Automatically set when matrix pressure is in a disarray, such as after\n\
tillage operations, or calls to reserve models.");
    frame.set ("deactivate_steps", 3);
  }
} TertiaryBiopores_syntax;

// tertiary_biopores.C ends here.
