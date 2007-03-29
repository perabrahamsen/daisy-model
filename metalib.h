// metalib.h -- A library of libraries.
// 
// Copyright 2007 Per Abrahamsen and KVL.
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


#ifndef METALIB_H
#define METALIB_H

#include "symbol.h"
#include <memory>
#include <vector>

class Syntax;
class AttributeList;
class Library;

class Metalib
{
  // Content.
  class Implementation;
  std::auto_ptr<Implementation> impl;

  // Use.
public:
  Syntax& syntax () const;
  AttributeList& alist () const;
  bool exist (symbol name) const;
  Library& library (symbol name) const;
  void all (std::vector<symbol>& libraries) const;
  int get_sequence ();
  void clear_all_parsed ();
  void refile_parsed (const std::string& from, const std::string& to);

  // Create and Destroy.
private:
  Metalib (const Metalib&);
public:
  explicit Metalib ();
 ~Metalib ();
};

#endif // METALIB_H
