// irrigate.C --- Manage irrigation events.
// 
// Copyright 2010 KU.
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

#include "irrigate.h"
#include "volume.h"
#include "solute.h"
#include "bioclimate.h"
#include "chemistry.h"
#include "frame.h"
#include "check.h"
#include "vcheck.h"
#include "block_submodel.h"
#include "submodeler.h"
#include "librarian.h"
#include "assertion.h"
#include "mathlib.h"

const double 
Irrigation::at_air_temperature = -500.0;

Irrigation::target_t 
Irrigation::Event::symbol2target (const symbol s)
{
  static struct sym_set_t : std::map<symbol, target_t>
  {
    sym_set_t ()
    {
      insert (std::pair<symbol,target_t> ("overhead", overhead));
      insert (std::pair<symbol,target_t> ("surface", surface));
      insert (std::pair<symbol,target_t> ("subsoil", subsoil));
    } 
  } sym_set;
  sym_set_t::const_iterator i = sym_set.find (s);
  daisy_assert (i != sym_set.end ());
  return (*i).second;
}  

void
Irrigation::Event::tick (Chemistry& chemistry, Bioclimate& bioclimate,
                         const double dt, Treelog& msg)
{ 
  const double time_used = std::min (dt, time_left);
  time_left -= time_used;
  const double amount = flux * time_used;
  const double flux_used = amount / dt;
  
  switch (target)
    {
    case overhead:
      if (approximate (temperature, at_air_temperature))
        bioclimate.irrigate_overhead (flux_used);
      else
        bioclimate.irrigate_overhead (flux_used, temperature);
      break;

    case surface:
      if (approximate (temperature, at_air_temperature))
        bioclimate.irrigate_surface (flux_used);
      else
        bioclimate.irrigate_surface (flux_used, temperature);
      break;

    case subsoil:
      bioclimate.irrigate_subsoil (flux_used);
      // TODO: Add to soil
      break;
    }
  
  // TODO: Add solute.
}

bool
Irrigation::Event::done () const
{ return time_left < 1e-9; }

void 
Irrigation::Event::load_syntax (Frame& frame)
{ 
  frame.declare ("time_left", "h", Check::non_negative (), Attribute::State, "\
Time left of this irrigation event.");
  frame.declare ("flux", "mm/h", Check::non_negative (), Attribute::Const, "\
Water applied.");
  frame.declare ("temperature", "dg C", Attribute::OptionalConst, "\
Irrigation temperature. By default, use daily air temperature.\n\
Ignored for subsoil irrigation.");
  frame.declare_object ("solute", Solute::component, 
                        Attribute::State, Attribute::Singleton, "\
Constituents of irrigation water.");
  frame.declare_string ("target", Attribute::Const, "\
Where to apply the irrigation.  \n\
\n\
overhead: Above crop canopy.\n\
surface: On soil surface, below crop canopy.\n\
subsoil: In the soil.  The 'volume' parameter will specify where.");
  static VCheck::Enum target_check ("overhead", "surface", "subsoil");
  frame.set_check ("target", target_check);
  frame.declare_object ("volume", Volume::component, 
                        Attribute::Const, Attribute::Singleton, "\
Soil volume to apply for subsoil irrigation.\n\
Ignored for overhead and surface irrigation.");
}

Irrigation::Event::Event (const BlockSubmodel& al)
  : time_left (al.number ("time_left")),
    flux (al.number ("flux")),
    temperature (al.number ("temperature", at_air_temperature)),
    solute (Librarian::build_item<Solute> (al, "solute")),
    target (symbol2target (al.name ("target"))),
    volume (Librarian::build_item<Volume> (al, "volume"))
{ }

void
Irrigation::tick (Chemistry& chemistry, Bioclimate& bioclimate,
                  const double dt, Treelog& msg)
{
  // Perform events.
  for (size_t e = 0; e < event.size (); e++)
    event[e]->tick (chemistry, bioclimate, dt, msg);

  // Remove all dead events.  There has to be a better way.
  bool removed;
  do
    {
      removed = false;
      for (auto_vector<Event*>::iterator e = event.begin();
	   e != event.end();
	   e++)
	if ((*e)->done ())
	  {
            delete *e;
            event.erase (e); // This invalidates the iterator.
            // Restart the loop.
            removed = true;
            break;
	  }
    }
  while (removed);
}

void 
Irrigation::load_syntax (Frame& frame)
{
  frame.declare_submodule_sequence ("event", Attribute::State, "\
Currently active irrigation events.", Event::load_syntax);
  frame.order ("event");
}

Irrigation::Irrigation (const BlockSubmodel& al)
  : event (map_submodel<Event> (al, "events"))
{ }

// irrigate.C ends here.

