// tertsmall.C --- Small timestep solution for tertiary transport.
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
#include "tertsmall.h"
#include "anystate.h"

// The 'Tertsmall' base class.

Tertsmall&
Tertsmall::none ()
{
  struct None : public Tertsmall
  {
    Anystate get_state () const
    { return Anystate::none (); }
    void set_state (const Anystate&)
    { }
    bool converge (const Anystate&)
    { return true; }
    void matrix_sink (std::vector<double>& S_matrix,
                      std::vector<double>& S_drain) const
    { }
    void find_implicit_water (const Anystate& old_state, 
                              const Geometry& geo, 
                              const Soil& soil,  
                              const SoilHeat& soil_heat, 
                              const std::vector<double>& h,
                              const double dt)
    { return; }
    void update_active (const std::vector<double>& h_matrix)
    { }
  };

  static None none;
  return none;
}

Tertsmall::Tertsmall ()
{ }

Tertsmall::~Tertsmall ()
{ }

// tertsmall.C ends here.

