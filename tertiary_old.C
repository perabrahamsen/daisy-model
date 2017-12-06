// tertiary_old.C --- Instant movement in vertical geometry.
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
#include "geometry1d.h"
#include "soil.h"
#include "soil_water.h"
#include "chemical.h"
#include "macro.h"
#include "mactrans.h"
#include "librarian.h"
#include "block_model.h"
#include "surface.h"
#include "groundwater.h"
#include "treelog.h"
#include "frame.h"
#include "assertion.h"
#include <sstream>

struct TertiaryOld : public Tertiary
{
  // Parameters.
  std::unique_ptr<Macro> macro;   // Water transport model in macropores.
  std::unique_ptr<Mactrans> mactrans; // Solute transport model in macropores.

  // Identity.
  bool has_macropores ()
  { return macro.get () && !macro->none (); }

  // Simulation.
  void tick_source (const Geometry&, const Soil&, const SoilHeat&, 
                    SoilWater&, Treelog&)
  { }
  void tick (const Units&, const Geometry& geo, const Soil& soil, 
             const SoilHeat& soil_heat, const double dt, 
             SoilWater& soil_water, Surface& surface, Treelog& msg);
  void tick_water (const Geometry&, const Soil&, const SoilWater&, 
                   const SoilHeat&, 
                   const double dt,
                   Surface& surface,
                   std::vector<double>& S_drain,
                   std::vector<double>& S_matrix, 
                   std::vector<double>& q_tertiary, 
                   Treelog& msg);
  void remove_solute (const symbol)
  { }
  double total_solute (const Geometry&, const symbol) const //[g/m^2]
  { return 0.0; }
  void solute (const Geometry&, const SoilWater&, 
               const std::map<size_t, double>& J_tertiary,
               const double dt, Chemical&, Treelog&);
  void output (Log&) const;
  
  // Create and Destroy.
public:
  bool initialize (const Units&, 
                   const Geometry&, const Soil&, SoilWater&, 
                   const Scope& parent_scope, 
                   const Groundwater&, Treelog& msg);
  bool check (const Geometry&, Treelog& msg) const;
  TertiaryOld (const BlockModel& al);
};

void
TertiaryOld::tick (const Units&, const Geometry& geo, const Soil& soil, 
                   const SoilHeat& soil_heat, const double dt, 
                   SoilWater& soil_water, Surface& surface, Treelog& msg)
{
  TREELOG_MODEL (msg);
  const size_t cell_size = geo.cell_size ();
  std::vector<double> Theta_p (cell_size, 0.0);
  std::vector<double> S_drain (cell_size, 0.0);
  std::vector<double> S_matrix (cell_size, 0.0);
  std::vector<double> S_tertiary_drain (cell_size, 0.0);
  const size_t edge_size = geo.edge_size ();
  std::vector<double> q_tertiary (edge_size, 0.0);
  this->tick_water (geo, soil, soil_water, soil_heat, dt, surface,
                    S_drain, S_matrix, q_tertiary, msg);
  std::vector<double> S_B2M (cell_size, 0.0);
  std::vector<double> S_M2B (cell_size, 0.0);
  for (size_t c = 0; c < cell_size; c++)
    {
      if (S_matrix[c] > 0)
        S_M2B[c] += S_matrix[c];
      else
        S_B2M[c] -= S_matrix[c];
    }
  soil_water.set_tertiary (Theta_p, q_tertiary, 
                           S_B2M, S_M2B, S_drain, S_tertiary_drain);
}

void
TertiaryOld::tick_water (const Geometry& geometry, const Soil& soil,
                         const SoilWater& soil_water, const SoilHeat&,
                         const double dt,
                         Surface& surface,
                         std::vector<double>& /* S_drain */,
                         std::vector<double>& S_p,
                         std::vector<double>& q_p,
                         Treelog& msg)
{
  const Geometry1D& geo = dynamic_cast<const Geometry1D&> (geometry);

  if (!macro.get ())			// No macropores.
    return;

  // Cells.
  const size_t cell_size = geo.cell_size ();
  std::vector<double> h_ice (cell_size);
  std::vector<double> h (cell_size);
  std::vector<double> Theta (cell_size);
  std::vector<double> S_sum (cell_size);
  
  for (size_t c = 0; c < cell_size; c++)
    {
      h_ice[c] = soil_water.h_ice (c);
      h[c] = soil_water.h (c);
      Theta[c] = soil_water.Theta (c);
      S_sum[c] = soil_water.S_sum (c);
    }

  // Calculate and update.
  macro->tick (geo, soil, 0, soil.size () - 1, surface, 
               h_ice, h, Theta, S_sum, S_p, q_p, dt, msg);
}

void
TertiaryOld::solute (const Geometry& geometry, const SoilWater& soil_water, 
                     const std::map<size_t, double>& J_tertiary,
                     const double dt,
                     Chemical& solute, Treelog& msg)
{
  const Geometry1D& geo = dynamic_cast<const Geometry1D&> (geometry);

  // Cells.
  const size_t cell_size = geo.cell_size ();
  std::vector<double> M (cell_size);
  std::vector<double> C (cell_size);
  std::vector<double> S (cell_size);
  std::vector<double> S_p (cell_size, 0.0);
  for (size_t c = 0; c < cell_size; c++)
    {
      M[c] = solute.M_total (c);
      C[c] = solute.C_secondary (c);
      S[c] = solute.S_secondary (c) + solute.S_primary (c);
    }
  
  // Edges.
  const size_t edge_size = geo.edge_size ();
  std::vector<double> J_p (edge_size, 0.0);
  
  // Forced flux:
  for (std::map<size_t, double>::const_iterator i = J_tertiary.begin ();
       i != J_tertiary.end ();
       i++)
    {
      const size_t e = (*i).first;
      const double flux = (*i).second;
      J_p[e] = flux;
    }

  mactrans->tick (geo, soil_water, M, C, S, S_p, J_p, dt, msg);

  solute.set_tertiary (S_p, J_p);
}

void 
TertiaryOld::output (Log&) const
{ }

bool 
TertiaryOld::initialize (const Units&,
                         const Geometry& geometry,
                         const Soil& soil, SoilWater& soil_water, 
                         const Scope& scope, const Groundwater& groundwater, 
                         Treelog& msg)
{ 
  TREELOG_MODEL (msg);

  if (!dynamic_cast<const Geometry1D*> (&geometry))
    {
      msg.error ("\
This tertiary water transport model only works with the 'vertical' movement model");
      return false;
    }
  const Geometry1D& geo = dynamic_cast<const Geometry1D&> (geometry);

  const size_t cell_size = geo.cell_size ();
  bool ok = true;

  if (macro.get ())
    msg.debug ("User specified macropores");
  else if (soil.humus (0) + soil.clay (0) > 0.05)
    // More than 5% clay (and humus) in first horizon.
    {
      // Find first non-clay layer.
      size_t lay = 1;
      while (lay < cell_size && soil.humus (lay) + soil.clay (lay) > 0.05)
        lay++;

      // Don't go below 1.5 m.
      double height = std::max (geo.zplus (lay-1), -150.0);

      // Add them.
      macro = Macro::create (height);

      std::ostringstream tmp;
      tmp << "Adding macropores to " << height << " cm";
      msg.debug (tmp.str ());
    }
  else
    msg.debug ("No macropores");

  return ok;
}

bool 
TertiaryOld::check (const Geometry& geo, Treelog& msg) const
{
  
  bool ok = true;
  if (!dynamic_cast<const Geometry1D*> (&geo))
    {
      msg.error ("\
This tertiary water transport model only works with the 'vertical' movement model");
      ok = false;
    }

  return ok;
}

TertiaryOld::TertiaryOld (const BlockModel& al)
  : Tertiary (al),
    macro (al.check ("macro")
	   ? Librarian::build_item<Macro> (al, "macro")
	   : NULL), 
    mactrans  (Librarian::build_item<Mactrans> (al, "mactrans"))
{ }

static struct TertiaryOldSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new TertiaryOld (al); }

  TertiaryOldSyntax ()
    : DeclareModel (Tertiary::component, "old", "\
Tertiary water and solute movement based on the obsolete 'macro'\n\
and 'mactrans' components.  Provided for backward compatibility.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_object ("macro", Macro::component,
                      Attribute::OptionalState, Attribute::Singleton,
                      "Preferential flow model.\n\
By default, preferential flow is enabled if and only if the combined\n\
amount of humus and clay in the top horizon is above 5%.");
    frame.declare_object ("mactrans", Mactrans::component, 
                      "Solute transport model in macropores.");
    frame.set ("mactrans", "default");
  }
} TertiaryOld_syntax;

// tertiary_old.C ends here.
