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
#include "timestep.h"
#include "volume.h"
#include "solute.h"
#include "frame.h"
#include "check.h"
#include "block_submodel.h"
#include "assertion.h"

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
Irrigation::Event::load_syntax (Frame& frame)
{ 
  frame.declare_submodule ("duration", Attribute::State, "\
Time left of this irrigation event.", 
                           Timestep::load_syntax);
  frame.declare ("flux", "mm/h", Check::non_negative (), Attribute::Const, "\
Water applied.");
  frame.declare_object ("solute", Solute::component, 
                        Attribute::Const, Attribute::Singleton, "\
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
Soil volume to apply subsoil irrigation.");
}

Irrigation::Event::Event (const BlockSubmodel& al)
  : time_left (0, 0, 0, 0),
    flux (-42.42),
    solute (),
    target (overhead),
    volume ()
{ }

Irrigation::Irrigation (const BlockSubmodel& al)
{ }

// irrigate.C ends here.

