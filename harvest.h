// harvest.h
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


#ifndef HARVEST_H
#define HARVEST_H

#include "chemicals.h"
#include "time.h"
#include "symbol.h"
#include "block.h"

class AttributeList;
class Syntax;
class Log;

class Harvest
{
  // Content:
public:
  const symbol column;
  const Time time;
  const symbol crop;
  const double stem_DM;
  const double stem_N;
  const double stem_C;
  const double dead_DM;
  const double dead_N;
  const double dead_C;
  const double leaf_DM;
  const double leaf_N;
  const double leaf_C;
  const double sorg_DM;
  const double sorg_N;
  const double sorg_C;
  const double water_stress_days;
  const double nitrogen_stress_days;
  const Chemicals chemicals;
  double total_N () const;
  double total_C () const;
  double total_DM () const;

  // Simulation:
public:
  void output (Log&) const;

  // Create and Destroy.
public:
  static void load_syntax (Syntax&, AttributeList&);
  Harvest (Block& al);
  Harvest (symbol col, Time t, symbol crp, 
	   double sDM, double sN, double sC, double dDM, double dN, double dC,
	   double lDM, double lN, double lC, double oDM, double oN, double oC,
           double wsd, double nsd,
	   const Chemicals& chem);
};      

#endif // HARVEST_H
