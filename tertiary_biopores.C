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
#include "log.h"

struct TertiaryBiopores : public Tertiary
{
  // Parameters.
  const auto_vector<Biopore*> classes; // List of biopore classes.
  const double pressure_initiate;// Pressure needed to init pref.flow [cm]
  const double pressure_end;	 // Pressure after pref.flow has been init [cm]
  const double pond_max;	 // Pond height before activating pref.flow [mm]

  // Identity.
  bool has_macropores ()
  { return true; }

  // Simulation.
  void extract_water (const Geometry&, const Soil&, const SoilWater&,
                      const double dt,
                      std::vector<double>& S_drain,
                      std::vector<double>& S_matrix, Treelog& msg);
  void release_water (const Geometry&, const Soil&, const SoilWater&,
                      const double dt,
                      std::vector<double>& S_matrix, Treelog& msg);
  void update_water_content ();
  void tick_water (const Geometry&, const Soil&, const SoilWater&,
                   const double dt,
                   Surface& surface,
                   std::vector<double>& S_drain,
                   std::vector<double>& S_matrix, 
                   std::vector<double>& q_tertiary, Treelog& msg);
  void update_water (const Geometry&, const Soil&, 
                     const std::vector<double>& h_matrix,
                     const double dt,
                     std::vector<double>& S_drain,
                     std::vector<double>& S_matrix, 
                     std::vector<double>& q_tertiary, 
                     Treelog& msg)
  { }
  void solute (const Geometry&, const SoilWater&,
               const std::map<size_t, double>& J_tertiary,
               const double /* dt */,
               Chemical&, Treelog&)
  { }
  void output (Log&) const;
  
  // Create and Destroy.
public:
  bool initialize (const Geometry&, const Soil&, const Scope& parent_scope, 
                   const double pipe_position, Treelog& msg);
  bool check (const Geometry&, Treelog& msg) const;
  TertiaryBiopores (Block& al);
};

void
TertiaryBiopores::extract_water (const Geometry& geo, const Soil& soil,
                                 const SoilWater& soil_water,
                                 const double dt,
                                 std::vector<double>& S_drain,
                                 std::vector<double>& S_matrix,
                                 Treelog& msg)
{
  const size_t cell_size = geo.cell_size ();

  for (size_t c = 0; c < cell_size; c++)
    {
      const double h = soil_water.h (c);

      if (h > pressure_initiate)
        {
          // Find total density for active pores, and deepest air filled pore.
          const double cell_z = geo.cell_z (c);
          double total_density = 0.0;  // Total density of all active pores.
          double lowest_pore = cell_z; // Lowest unsaturated pore bottom.
          for (size_t b = 0; b < classes.size (); b++)
            {
              const Biopore& biopore = *classes[b];
              const double air_bottom = biopore.air_bottom (c);

              if (air_bottom < cell_z)
                // Active pore class.
                total_density += biopore.density (c);

              if (air_bottom < lowest_pore)
                // This is the lowest until now.
                lowest_pore = air_bottom;
            }
            
          if (total_density < 1e-42)
            // No biopores.
            continue;

          // We empty the matrix to 'pressure_end', but only if there
          // are pores with air sufficently deep.
          const double pore_pressure = lowest_pore - cell_z;
          const double end = pore_pressure > pressure_end
            ? pore_pressure
            : pressure_end;

          if (end > h - 1e-5)
            // Ignore trivial pressure difference.
            continue;

          // Find corresponding loss.
          const double h_ice = soil_water.h_ice (c);
          const double loss = soil.Theta (c, h, h_ice) 
            - soil.Theta (c, end, h_ice);
          
          if (loss < 1e-5)
            // Ignore trivial losss.
            continue;

          const double volume = geo.cell_volume (c); // [cm^3]

          // Distribute to biopore classes.
          for (size_t b = 0; b < classes.size (); b++)
            {
              Biopore& biopore = *classes[b];
              const double air_bottom = biopore.air_bottom (c);

              if (air_bottom > cell_z - 1e-5)
                // Biopore is filled.
                continue;

              const double density = biopore.density (c);
              if (density < 1e-42)
                // No biopores of this class in this cell.
                continue;

              // Extract it.
              const double fraction = density / total_density;
              biopore.extract_water (c, volume, fraction * loss,
                                     dt, S_drain, S_matrix, msg);
            }
        }
    }
}

void
TertiaryBiopores::release_water (const Geometry& geo, const Soil& soil,
                                 const SoilWater& soil_water,
                                 const double dt,
                                 std::vector<double>& S_matrix, Treelog& msg)
{
  for (size_t b = 0; b < classes.size (); b++)
    classes[b]->release_water (geo, soil, soil_water, dt, S_matrix, msg);
}

void
TertiaryBiopores::update_water_content ()
{
  for (size_t b = 0; b < classes.size (); b++)
    classes[b]->update_water ();
}

void
TertiaryBiopores::tick_water (const Geometry& geo, const Soil& soil,
                              const SoilWater& soil_water,
                              const double dt,
                              Surface& surface,
                              std::vector<double>& S_drain,
                              std::vector<double>& S_matrix,
                              std::vector<double>& /* q_tertiary */, 
                              Treelog& msg)
{
  extract_water (geo, soil, soil_water, dt, S_drain, S_matrix, msg);
  update_water_content ();
  release_water (geo, soil, soil_water, dt, S_matrix, msg);
  update_water_content ();
}

void 
TertiaryBiopores::output (Log& log) const
{ output_list (classes, "classes", log, Biopore::component); }

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
    pond_max (al.number ("pond_max"))
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

    Librarian::add_type (Tertiary::component, "biopores", alist, syntax, &make);
  }
} TertiaryBiopores_syntax;

// tertiary_biopores.C ends here.
