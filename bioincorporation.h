// bioincorporation.h --- Biological incorporation of organic matter in soil.
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


#ifndef BIOINCORPORATION_H
#define BIOINCORPORATION_H

#include <vector>

class AttributeList;
class Syntax;
class Log;
class AM;
class Geometry;
class Soil;

class Bioincorporation
{
  // Content.
private:
  struct Implementation;
  Implementation& impl;
  
  // Simulation.
public:
  void tick (const Geometry&, std::vector <AM*>&, double T, double& CO2);
  void output (Log&) const;

  // Create and Destroy.
public:
  void initialize (const Soil&);
  void add_input (const Soil& soil, std::vector<double>& input, double amount);
  AM* create_am (const Geometry&);
  void set_am (AM*);
  static void load_syntax (Syntax&, AttributeList&);
  Bioincorporation (const AttributeList&);
  ~Bioincorporation ();
};

#endif // BIOINCORPORATION_H
