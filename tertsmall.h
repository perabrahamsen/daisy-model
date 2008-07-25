// tertsmall.h --- Small timestep solution for tertiary transport.
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

#ifndef TERTSMALL_H
#define TERTSMALL_H

#include <boost/noncopyable.hpp>
#include <vector>

class Geometry;
class Soil;
class SoilHeat;
class Anystate;

class Tertsmall : private boost::noncopyable
{
public:
  virtual Anystate get_state () const = 0;
  virtual void set_state (const Anystate&) = 0;
  virtual void matrix_sink (const Geometry& geo, const Soil& soil,  
                            const SoilHeat& soil_heat, 
                            const std::vector<double>& h,
                            std::vector<double>& S_matrix,
                            std::vector<double>& S_drain) const = 0;
  virtual bool find_implicit_water (const Anystate& old_state, 
                                    const Geometry& geo, 
                                    const Soil& soil,  
                                    const SoilHeat& soil_heat, 
                                    const std::vector<double>& h,
                                    const double dt) = 0;
  virtual void update_active (const std::vector<double>& h_matrix) = 0;

  // Create and Destroy.
public:
  static Tertsmall& none ();
  Tertsmall ();
  virtual ~Tertsmall ();
};

#endif // TERTSMALL_H
