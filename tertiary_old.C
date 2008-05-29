// biopore_old.C --- Instant movement in vertical geometry.
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
#include "block.h"

struct TertiaryOld : public Tertiary
{
  // Parameters.
  std::auto_ptr<Macro> macro;   // Water transport model in macropores.
  std::auto_ptr<Mactrans> mactrans; // Solute transport model in macropores.

  // Identity.
  bool has_macropores ()
  { return macro.get () && !macro->none (); }

  // Simulation.
  void tick_water (const Geometry&, const Soil&, const SoilWater&,
                   const double dt,
                   Surface& surface,
                   std::vector<double>& S_drain,
                   std::vector<double>& S_matrix, 
                   std::vector<double>& q_tertiary, 
                   Treelog& msg);
  void solute (const Geometry&, const SoilWater&, 
               const std::map<size_t, double>& J_tertiary,
               const double dt, Chemical&, Treelog&);
  void output (Log&) const;
  
  // Create and Destroy.
public:
  bool initialize (const Geometry&, const Soil&, const Scope& parent_scope, 
                   const double pipe_position, Treelog& msg);
  bool check (const Geometry&, Treelog& msg) const;
  TertiaryOld (Block& al);
};


void
TertiaryOld::tick_water (const Geometry& geometry, const Soil& soil,
                         const SoilWater& soil_water,
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
TertiaryOld::initialize (const Geometry& geometry, const Soil& soil,
                         const Scope& scope, const double pipe_position, 
                         Treelog& msg)
{ 
  Treelog::Open nest (msg, component + std::string (": ") + name);

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
    /* Already got them. */;
  else if (soil.humus (0) + soil.clay (0) > 0.05)
    // More than 5% clay (and humus) in first horizon.
    {
      // Find first non-clay layer.
      size_t lay = 1;
      while (lay < cell_size && soil.humus (lay) + soil.clay (lay) > 0.05)
        lay++;

      // Don't go below 1.5 m.
      double height = std::max (geo.zplus (lay-1), -150.0);

      // Don't go below drain pipes.
      if (pipe_position < 0.0)
        height = std::max (height, pipe_position);

      // Add them.
      macro = Macro::create (height);

      msg.debug ("Adding macropores");
    }

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

TertiaryOld::TertiaryOld (Block& al)
  : Tertiary (al),
    macro (al.check ("macro")
	   ? Librarian::build_item<Macro> (al, "macro")
	   : NULL), 
    mactrans  (Librarian::build_item<Mactrans> (al, "mactrans"))
{ }

static struct TertiaryOldSyntax
{
  static Model& make (Block& al)
  { return *new TertiaryOld (al); }

  TertiaryOldSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "\
Tertiary water and solute movement based on the obsolete 'macro'\n\
and 'mactrans' components.  Provided for backward compatibility.");

  syntax.add_object ("macro", Macro::component,
                     Syntax::OptionalState, Syntax::Singleton,
                     "Preferential flow model.\n\
By default, preferential flow is enabled if and only if the combined\n\
amount of humus and clay in the top horizon is above 5%.");
  syntax.add_object ("mactrans", Mactrans::component, 
                     "Solute transport model in macropores.");
  alist.add ("mactrans", Mactrans::default_model ());

    Librarian::add_type (Tertiary::component, "old", alist, syntax, &make);
  }
} TertiaryOld_syntax;

// tertiary_old.C ends here.
