// harvesting.h -- Harvest parameters for the default crop model.
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

#ifndef HARVESTING_H
#define HARVESTING_H

class AttributeList;
class Syntax;
class Log;

#include <vector>
using namespace std;

class Harvesting 
{
  // Parameters.
public:
  const vector<AttributeList*>& Stem; // Stem AM parameters.
  const vector<AttributeList*>& Leaf; // Leaf AM parameters.
  const vector<AttributeList*>& Dead; // Dead AM parameters.
  const vector<AttributeList*>& SOrg; // SOrg AM parameters.
  const vector<AttributeList*>& Root; // Root AM parameters.
  const double EconomicYield_W; // Frac. of economic yield (DM) in storage org.
  const double EconomicYield_N; // Frac. of economic yield (N) in storage org.
  const double DSmax;		// Maximal development stage for which
				// the crop survives harvest.
  const double DSnew;		// Maximal development stage after harvest.

  // Simulation.
public:
  void output (Log& log) const;

  // Create and Destroy.
public:
  static void load_syntax (Syntax& syntax, AttributeList& alist);
  Harvesting (const AttributeList&);
  ~Harvesting ();
};

#endif // HARVESTING_H
