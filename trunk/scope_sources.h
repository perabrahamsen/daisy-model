// scope_sources.h --- A scope based on a time series.
// 
// Copyright 2005 Per Abrahamsen and KVL.
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


#ifndef SCOPE_SOURCES_H
#define SCOPE_SOURCES_H

#include "scope.h"
#include "time.h"
#include <vector>

class Treelog;
class Source;

class ScopeSources : public Scope
{
  // Content.
private:
  std::vector<symbol> all_numbers_;
  const std::vector<Source*> source;
  std::vector<int> index;
public:
  Time now;

  // Interface.
public:
  const std::vector<symbol>& all_numbers () const;
  bool has_number (symbol tag) const;
  double number (symbol tag) const;
  symbol dimension (symbol tag) const;
  symbol get_description (symbol tag) const;

  // Propagate.
  bool load (Treelog& msg);
  std::string with ();

  // Loop.
  void first ();
  bool done ();
  void next ();

  // Create and Destroy.
public:
  ScopeSources (const std::vector<Source*>& s);
  ~ScopeSources ();
};

#endif // SCOPE_SOURCES_H
