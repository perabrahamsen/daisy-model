// chemicals.h
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


#ifndef CHEMICALS_H
#define CHEMICALS_H

#include "symbol.h"
#include "syntax.h"
#include <vector>
#include <set>

class Log;
class AttributeList;
class Chemical;

class Chemicals
{ 
  // Content.
public:
  struct Implementation;	// Top secret internal state.
private:
  Implementation& impl;

  // Utilities.
public:
  static const Chemical& lookup (symbol name);
  static void move_fraction (Chemicals& from, Chemicals& to, double fraction);
  static void copy_fraction (const Chemicals& from, Chemicals& to,
			     double fraction);

  // Canopy functions.
public:
  void canopy_update (const Chemicals& canopy_chemicals_in, 
		      double canopy_water_storage,
		      double canopy_water_out,
		      Chemicals& canopy_chemicals_dissipate,
		      Chemicals& canopy_chemicals_out);

  // Simulation
public:
  void output (Log&) const;
  void add (symbol chemical, double amount); // [g/m^2]
  void set_to (symbol chemical, double amount); // [g/m^2]
  double amount (symbol chemical) const; // [g/m^2]
  typedef std::set<symbol> symbol_set;
  void find_missing (const symbol_set& all, symbol_set& missing) const;

  // Create and Destroy.
public:
  void clear ();
  void operator += (const Chemicals&);
  void operator = (const Chemicals&);
  Chemicals (const Chemicals&);
  static void add_syntax (const char* name, Syntax& syntax, 
			  AttributeList& alist,
			  Syntax::category cat, 
			  const char* description);
  Chemicals ();
  Chemicals (const std::vector<AttributeList*>&);
  ~Chemicals ();
};

#endif // CHEMICALS_H
