// soil_water1d.C
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


#include "soil_water1d.h"
#include "log.h"
#include "alist.h"
#include "uzmodel.h"
#include "geometry1d.h"
#include "soil.h"
#include "surface.h"
#include "groundwater.h"
#include "submodeler.h"
#include "macro.h"
#include "syntax.h"
#include "mathlib.h"
#include "submodel.h"
#include <sstream>
#include <memory>

void
SoilWater1D::macro_tick (const Geometry1D& geo, 
                         const Soil& soil, Surface& surface, 
                         Treelog& msg)
{
  if (!macro.get ())			// No macropores.
    return;

  // Calculate preferential flow first.
  std::fill (S_p_.begin (), S_p_.end (), 0.0);
  std::fill (q_p_.begin (), q_p_.end (), 0.0);
  macro->tick (geo, soil, 0, soil.size () - 1, surface, h_ice_, h_, Theta_,
	       S_sum_, S_p_, q_p_, msg);
}

void
SoilWater1D::tick (const Geometry1D& geo,
                   const Soil& soil,
                   const SoilHeat& soil_heat, 
                   Surface& surface, Groundwater& groundwater,
                   Treelog& msg)
{
  Treelog::Open nest (msg, "SoilWater1D");
  tick_base (geo.cell_size (), soil, msg);

  // Limit for groundwater table.
  size_t last  = soil.size () - 1;
  if (groundwater.bottom_type () == Groundwater::pressure)
    {
      daisy_assert (soil.size () > 1);
      if (groundwater.table () <= geo.zplus (soil.size () - 2))
	throw ("Groundwater table in or below lowest cell.");
      last = geo.interval_plus (groundwater.table ());
      if (last >=  soil.size () - 1)
	daisy_panic ("Groundwater too low.");
      // Pressure at the last cell is equal to the water above it.
      for (size_t i = last + 1; i < soil.size (); i++)
	{
	  h_old_[i] = groundwater.table () - geo.z (i);
	  h_[i] = groundwater.table () - geo.z (i);
	}
    }

  // Limit for ridging.
  const size_t first = surface.soil_top () ? surface.last_cell () : 0;
  bool ok = true;

  // Calculate matrix flow next.
  try
    {
      ok = top->tick (msg, geo, soil, soil_heat,
                      first, surface,
                      last, groundwater,
                      S_sum_, h_old_, Theta_old_, h_ice_,
                      h_, Theta_, q_);
    }
  catch (const char* error)
    {
      msg.warning (std::string ("UZ problem: ") + error);
      ok = false;
    }
  catch (const std::string& error)
    {
      msg.warning (std::string ("UZ problem: ") + error);
      ok = false;
    }
  if (!ok)
    {
      msg.message ("Using reserve uz model.");
      reserve->tick (msg, geo, soil, soil_heat,
                     first, surface,
                     last, groundwater,
                     S_sum_, h_old_, Theta_old_, h_ice_,
                     h_, Theta_, q_);
    }

  for (size_t i = last + 2; i <= soil.size (); i++)
    {
      q_[i] = q_[i-1];
      q_p_[i] = q_p_[i-1];
    }

  // Update Theta below groundwater table.
  if (groundwater.bottom_type () == Groundwater::pressure)
    {
      for(size_t i = last + 1; i < soil.size (); i++)
	Theta_[i] = soil.Theta (i, h_[i], h_ice_[i]);
    }

  // Update surface and groundwater reservoirs.
  surface.accept_top (q_[0] * dt, geo, 0U, msg);
  groundwater.accept_bottom ((q_[last + 1] + q_p_[last + 1]) * dt,
                             geo, last + 1U);
}

void 
SoilWater1D::output (Log& log) const
{
  output_base (log);
  output_derived (top, "UZtop", log);
}

void
SoilWater1D::initialize (const AttributeList& al,
                         const Geometry1D& geo,
                         const Soil& soil,
                         const Groundwater& groundwater, 
                         Treelog& msg)
{
  Treelog::Open nest (msg, "SoilWater1D");
  const size_t cell_size = geo.cell_size ();

  // Initialize base (requires ice!).
  initialize_base (al, geo, soil, groundwater, msg);

  // Macropores.
  if (al.check ("macro"))
    macro.reset (Librarian<Macro>::build_free (msg, al.alist ("macro"), 
                                               "macro"));
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
      if (groundwater.is_pipe ())
	height = std::max (height, groundwater.pipe_height ());

      // Add them.
      macro = Macro::create (height);

      msg.debug ("Adding macropores");
    }

  // Let 'macro' choose the default method to average K values in 'uz'.
  const bool has_macropores = (macro.get () && !macro->none ());
  top->has_macropores (has_macropores);
}

SoilWater1D::SoilWater1D (Block& al)
  : SoilWater (al),
    top (Librarian<UZmodel>::build_item (al, "UZtop")),
    reserve (Librarian<UZmodel>::build_item (al, "UZreserve")),
    macro (NULL)
{ }

SoilWater1D::~SoilWater1D ()
{ }

void
SoilWater1D::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  load_base (syntax, alist);
  alist.add ("submodel", "SoilWater1D");
  alist.add ("description", "Water content of a vertical soil column.");

  syntax.add ("UZdefault", Librarian<UZmodel>::library (),
	      "Main water transport model in unsaturated zone.");
  alist.add ("UZdefault", UZmodel::default_model ());
  syntax.add ("UZreserve", Librarian<UZmodel>::library (),
	      "Reserve transport model if UZtop fails.");
  alist.add ("UZreserve", UZmodel::reserve_model ());
  syntax.add ("macro", Librarian<Macro>::library (),
	      Syntax::OptionalState, Syntax::Singleton,
	      "Preferential flow model.\n\
By default, preferential flow is enabled if and only if the combined\n\
amount of humus and clay in the top horizon is above 5%.");
}

static Submodel::Register 
soil_water_1d_submodel ("SoilWater1D", SoilWater1D::load_syntax);
