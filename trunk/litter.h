// litter.h -- Litter lay below permanent vegetation.
// 
// Copyright 2003 Per Abrahamsen and KVL.
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


#ifndef LITTER_H
#define LITTER_H

class AttributeList;
class Syntax;

class Litter
{
  // Content.
public:
  const double vapor_flux_factor;
  const double interception_capacity;
  const double albedo;

  // Create and Destroy.
public:
  static void load_syntax (Syntax&, AttributeList&);
  Litter (const AttributeList&);
  ~Litter ();
};

#endif // LITTER_H
