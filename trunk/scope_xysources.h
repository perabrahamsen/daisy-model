// scope_xysources.h --- A scope based on XY data.
// 
// Copyright 2005 Per Abrahamsen and KVL.
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


#ifndef SCOPE_XYSOURCES_H
#define SCOPE_XYSOURCES_H

#include "scope.h"
#include "memutils.h"
#include <vector>

class Treelog;
class XYSource;
class Units;

class ScopeXYSources : public Scope
{
  // Content.
private:
  auto_vector<XYSource*> source;
  size_t index;
  bool combine_x;
  double range (size_t i) const;
  double domain (size_t i) const;
public:
  bool range_is_x () const
  { return combine_x; }
  symbol range_dimension () const;
  
  // Interface.
public:
  void entries (std::set<symbol>&) const;
  Attribute::type lookup (symbol tag) const;
  bool check (symbol tag) const;
  double number (symbol tag) const;
  symbol dimension (symbol tag) const;
  symbol description (symbol tag) const;

  // Propagate.
  bool load (const Units&, Treelog&);
  void limit_range (double& xmin, double& xmax, 
                    double& ymin, double& ymax) const;
  symbol with ();

  // Loop.
  void first ();
  bool done ();
  void next ();
  double current () const;

  // Create and Destroy.
public:
  ScopeXYSources (const std::vector<XYSource*>& s);
  ~ScopeXYSources ();
};

#endif // SCOPE_XYSOURCES_H
