// biopore_biopores.C --- Static vertical biopores with a capacity.
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
#include "block.h"
#include "check.h"
#include "geometry.h"
#include "soil.h"
#include "soil_water.h"
#include "soil_heat.h"
#include "log.h"
#include "anystate.h"
#include "surface.h"

struct TertiaryBiopores : public Tertiary
{
  // Parameters.
  const auto_vector<Biopore*> classes; // List of biopore classes.
  const double pressure_initiate;// Pressure needed to init pref.flow [cm]
  const double pressure_end;	 // Pressure after pref.flow has been init [cm]
  const double pond_max;	 // Pond height before activating pref.flow [mm]
  const bool use_small_timesteps_; // True, iff we want to calculate S in R.E.

  // Identity.
  bool has_macropores ()
  { return true; }

  // State.
  std::vector<bool> active;      // Biopore activity 
  struct MyContent : public Anystate::Content
  {
    std::vector<Anystate> states;
    std::auto_ptr<Anystate::Content> clone () const
    { 
      std::auto_ptr<Anystate::Content> copy (new MyContent (states)); 
      return copy;
    }
    MyContent (const std::vector<Anystate>& s)
      : states (s)
    { }
  };
  Anystate get_state () const;
  void set_state (const Anystate&);
  bool converge (const Anystate&);

  // Infiltration.
  double capacity (const Geometry&, size_t e);        // Max flux.
  void infiltrate (const Geometry&, size_t e, double amount); // Add it.

  // Simulation.
  bool use_small_timesteps ()
  { return use_small_timesteps_; }

  // - For use by column.
  void tick (const Geometry& geo, const Soil& soil, 
             const SoilHeat& soil_heat, const double dt, 
             SoilWater& soil_water, Surface& surface, Treelog& msg);

  // - For use in Richard's Equation.
  double matrix_biopores_matrix (size_t c, const Geometry& geo, // Matrix 
                                 const Soil& soil,              // sink term.
                                 double K_xx, double h) const;
  double matrix_biopores_drain (size_t c, const Geometry& geo, // Matrix
                                const Soil& soil,              // sink term.
                                double K_xx, double h) const;
  
  void matrix_sink (const Geometry& geo, const Soil& soil,  
                    const SoilHeat& soil_heat, 
                    const std::vector<double>& h,
                    std::vector<double>& S_matrix,
                    std::vector<double>& S_drain) const;
  
  void update_biopores (const Geometry& geo, 
                        const Soil& soil,  
                        const SoilHeat& soil_heat, 
                        const std::vector<double>& h,
                        const double dt);
  void update_water ();
  bool find_implicit_water (const Anystate& old_state, 
                            const Geometry& geo, 
                            const Soil& soil,  
                            const SoilHeat& soil_heat, 
                            const std::vector<double>& h,
                            const double dt);
  void update_active (const std::vector<double>& h);

  // - For use in Movement::solute.
  void solute (const Geometry&, const SoilWater&,
               const std::map<size_t, double>& J_tertiary,
               const double /* dt */,
               Chemical&, Treelog&)
  { /* TODO */ }

  // - Output.
  void output (Log&) const;
  
  // Create and Destroy.
public:
  bool initialize (const Geometry&, const Soil&, const Scope& parent_scope, 
                   const double pipe_position, Treelog& msg);
  bool check (const Geometry&, Treelog& msg) const;
  TertiaryBiopores (Block& al);
};

Anystate
TertiaryBiopores::get_state () const
{
  const size_t classes_size = classes.size ();
  std::vector<Anystate> biopore_state;
  for (size_t b = 0; b < classes_size; b++)
    {
      const Biopore& biopore = *classes[b];
      biopore_state.push_back (biopore.get_state ());
    }
  std::auto_ptr<Anystate::Content> copy (new MyContent (biopore_state));
  return Anystate (copy);
}
 
void 
TertiaryBiopores::set_state (const Anystate& state)
{
  const MyContent& content = static_cast<const MyContent&> (state.inspect ());
  const size_t classes_size = classes.size ();
  daisy_assert (classes_size == content.states.size ());
  for (size_t b = 0; b < classes_size; b++)
    {
      Biopore& biopore = *classes[b];
      biopore.set_state (content.states[b]);
    }
}

bool 
TertiaryBiopores::converge (const Anystate& state)
{  
  const double max_abs = 0.02;
  const double max_rel = 0.001;

  const MyContent& content = static_cast<const MyContent&> (state.inspect ());
  const size_t classes_size = classes.size ();
  daisy_assert (classes_size == content.states.size ());
  for (size_t b = 0; b < classes_size; b++)
    {
      Biopore& biopore = *classes[b];
      if (!biopore.converge (content.states[b], max_abs, max_rel))
        return false;
    }
  return true;
}

double
TertiaryBiopores::capacity (const Geometry& geo, size_t e)
{ }

void
TertiaryBiopores::infiltrate (const Geometry& geo, size_t e, double amount)
{ }

void
TertiaryBiopores::tick (const Geometry& geo, const Soil& soil, 
                        const SoilHeat& soil_heat, const double dt, 
                        SoilWater& soil_water, Surface& surface, Treelog& msg)
{
  Treelog::Open nest (msg, component + std::string (":") + name);

  // Flux.
  const size_t edge_size = geo.edge_size ();
  std::vector<double> q_tertiary (edge_size, 0.0);

  // Infiltration.
  const std::vector<size_t>& edge_above = geo.cell_edges (Geometry::cell_above);
  const size_t edge_above_size = edge_above.size ();
  
  for (size_t i = 0; i < edge_above_size; i++)
    {
      const size_t edge = edge_above[i];
      const double in_sign 
        = geo.cell_is_internal (geo.edge_to (edge)) ? 1.0 : -1.0;
      
      const double max_surface = in_sign * surface.q_top (geo, edge);
      const double flux_in = std::min (capacity (geo, edge), max_surface);
      surface.accept_top (in_sign * flux_in, geo, edge, dt, msg);
      infiltrate (geo, edge, flux_in);
    }

  // Update soil water.
  soil_water.set_tertiary_flux (q_tertiary);

  // Soil matrix exchange.
  const size_t cell_size = geo.cell_size ();
  std::vector<double> S_drain (cell_size, 0.0);
  std::vector<double> S_matrix (cell_size, 0.0);

  if (!use_small_timesteps ())
    {
      // Handle in Richard's Equation.
      Anystate old_state = get_state ();

      std::vector<double> h;
      const size_t cell_size = geo.cell_size ();
      for (size_t c = 0; c < cell_size; c++)
        h.push_back (soil_water.h (c));
      
      update_active (h);
      if (find_implicit_water (old_state, geo, soil, soil_heat, h, dt))
        matrix_sink (geo, soil, soil_heat, h, S_matrix, S_drain);
      else
        msg.warning ("State did not converge, ignoring tertiary transport");
    }

  soil_water.drain (S_drain);
}

double 
TertiaryBiopores::matrix_biopores_matrix (size_t c, const Geometry& geo, 
                                          const Soil& soil, 
                                          double K_xx, double h) const
{
  double sum = 0.0;
  for (size_t b = 0; b < classes.size (); b++)
    {
      const Biopore& biopore = *classes[b];
      sum += biopore.matrix_biopore_matrix(c, geo, soil, active[c], K_xx, h);
    }
  return sum;
}

double 
TertiaryBiopores::matrix_biopores_drain (size_t c, const Geometry& geo, 
                                         const Soil& soil,  
                                         double K_xx, double h) const
{
  double sum = 0.0;
  for (size_t b = 0; b < classes.size (); b++)
    {
      const Biopore& biopore = *classes[b];
      sum += biopore.matrix_biopore_drain(c, geo, soil, active[c], K_xx, h);
    }
  return sum;
}


void 
TertiaryBiopores::matrix_sink (const Geometry& geo, 
                               const Soil& soil,  
                               const SoilHeat& soil_heat, 
                               const std::vector<double>& h,
                               std::vector<double>& S_matrix,
                               std::vector<double>& S_drain) const
{
  const size_t cell_size = geo.cell_size ();
  for (size_t c = 0; c < cell_size; c++)
    {
      const double h_cond = std::min(pressure_initiate, h[c]);
      const double T = soil_heat.T (c);
      const double h_ice = 0.0;    //ice ignored 
      const double K_zz = soil.K (c, h_cond, h_ice, T);
      const double K_xx = K_zz * soil.anisotropy (c);
      S_matrix[c] =  matrix_biopores_matrix (c, geo, soil, K_xx, h[c]);
      S_drain[c] =  matrix_biopores_drain (c, geo, soil, K_xx, h[c]); 
    }
}
 
void 
TertiaryBiopores::update_biopores (const Geometry& geo, 
                                   const Soil& soil,  
                                   const SoilHeat& soil_heat, 
                                   const std::vector<double>& h,
                                   const double dt) 
{
  const size_t cell_size = geo.cell_size ();
  for (size_t c = 0; c < cell_size; c++)
    {
      const double vol = geo.cell_volume (c);
      const double h_cond = std::min(pressure_initiate, h[c]);
      const double T = soil_heat.T (c);
      const double h_ice = 0.0;    //ice ignored 
      const double K_zz = soil.K (c, h_cond, h_ice, T);
      const double K_xx = K_zz * soil.anisotropy (c);
     
      for (size_t b = 0; b < classes.size (); b++)
        {
          Biopore& biopore = *classes[b];
          const double S 
            = biopore.matrix_biopore_matrix(c, geo, soil, 
                                            active[c], K_xx, h[c])
            + biopore.matrix_biopore_drain(c, geo, soil,
                                           active[c], K_xx, h[c]);
          biopore.add_water (c, -S * dt * vol);
        }
    }
}

void
TertiaryBiopores::update_water ()
{
  for (size_t b = 0; b < classes.size (); b++)
    classes[b]->update_water ();
}

bool
TertiaryBiopores::find_implicit_water (const Anystate& old_state, 
                                       const Geometry& geo, 
                                       const Soil& soil,  
                                       const SoilHeat& soil_heat, 
                                       const std::vector<double>& h,
                                       const double dt)
{
  const int max_iter = 12;
  for (int iter = 0; iter < max_iter; iter++)
    {
      const Anystate new_state = get_state ();
      // Find added water with "new water content".
      update_biopores (geo, soil, soil_heat, h, dt);
      // Reset water content to begining of timestep.
      set_state (old_state);
      // Add water to get new state.
      update_water ();
      // Check if they converge.
      if (converge (new_state))
        // If so, we are finished.
        return true;
    }
  set_state (old_state);
  return false;
}

void
TertiaryBiopores::update_active (const std::vector<double>& h)
{
  for (size_t c = 0; c < active.size (); c++)
    {
      if (active[c])    // Biopore is active 
        {
          if (h[c] < pressure_end)
            active[c] = false;
        }
      else              // Biopore is not active 
        {
          if (h[c] > pressure_initiate)
            active[c] = true;
        }
    }
}

void 
TertiaryBiopores::output (Log& log) const
{
  output_list (classes, "classes", log, Biopore::component); 
  // TODO: output_variable (active, log);
}


bool 
TertiaryBiopores::initialize (const Geometry& geo, const Soil&, 
                              const Scope& scope, const double pipe_position, 
                              Treelog& msg)
{ 
  Treelog::Open nest (msg, component + std::string (": ") + name);
  bool ok = true;
  for (size_t b = 0; b < classes.size (); b++)
    {
      Treelog::Open (msg, "classes", b, classes[b]->name);
      if (!classes[b]->initialize (geo, scope, pipe_position, msg))
        ok = false;
    }
  const size_t cell_size = geo.cell_size ();
  while (active.size () < cell_size)
    active.push_back (false);
  return ok;
}

bool 
TertiaryBiopores::check (const Geometry& geo, Treelog& msg) const
{
  bool ok = true;
  for (size_t b = 0; b < classes.size (); b++)
    {
      Treelog::Open (msg, "classes", b, classes[b]->name);
      if (!classes[b]->check (geo, msg))
        ok = false;
    }
  return ok;
}

TertiaryBiopores::TertiaryBiopores (Block& al)
  : Tertiary (al),
    classes (Librarian::build_vector<Biopore> (al, "classes")),
    pressure_initiate (al.number ("pressure_initiate")),
    pressure_end (al.number ("pressure_end")),
    pond_max (al.number ("pond_max")),
    use_small_timesteps_ (al.flag ("use_small_timesteps")),
    active (al.check ("active")
            ? al.flag_sequence ("active")
            : std::vector<bool> ())
{ }

static struct TertiaryBioporesSyntax
{
  static Model& make (Block& al)
  { return *new TertiaryBiopores (al); }

  TertiaryBioporesSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Tertiary domain divided into biopore classes.");

    syntax.add_object ("classes", Biopore::component, 
                       Syntax::State, Syntax::Sequence,
                       "List of biopore classes.");
    syntax.add ("pressure_initiate", "cm", Syntax::Const, 
                "Pressure needed to initiate biopore flow.");
    alist.add ("pressure_initiate", -3.0);
    syntax.add ("pressure_end", "cm", Syntax::Const, 
                "Pressure after biopore flow has been initiated.");
    alist.add ("pressure_end", -30.0);
    syntax.add ("pond_max", "mm", Check::non_negative (), Syntax::Const, "\
Maximum height of ponding before spilling into biopores.\n\
After macropores are activated pond will have this height.");
    alist.add ("pond_max", 0.5);
    syntax.add ("use_small_timesteps", Syntax::Boolean, Syntax::Const,
                "True iff the sink is allowed to change within a timestep.");
    alist.add ("use_small_timesteps", true);
    syntax.add ("active", Syntax::Boolean, Syntax::OptionalState,
                Syntax::Sequence, "Active biopores in cells.");
    Librarian::add_type (Tertiary::component, "biopores", alist, syntax, &make);
  }
} TertiaryBiopores_syntax;

// tertiary_biopores.C ends here.
