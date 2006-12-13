// timestep.h -- Time difference.
// 
// Copyright 2006 Per Abrahamsen and KVL.
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

#ifndef TIMESTEP_H
#define TIMESTEP_H

#include "time.h"
#include <memory>

class AttributeList;
class Syntax;
class Block;

class Timestep
{
  // Content.
private:
  struct Implementation;
  const std::auto_ptr<Implementation> impl;

  // Extract elements.
public:
  int years () const;
  int days () const;
  int hours () const;
  int minutes () const;
  int seconds () const;

  // Extract totals.
public:
  double total_hours () const;
    
  // Create.
public:
  static void load_syntax (Syntax&, AttributeList&);
  explicit Timestep (Block&);
  Timestep (int years, int days, int hours, int minutes, int seconds);
  ~Timestep ();
  Timestep (const Timestep&);
private:                    
  const Timestep& operator= (const Timestep&);
};

// Time operations.
void operator+= (Time&, const Timestep&);

Time operator+ (const Time&, const Timestep&);
Time operator- (const Time&, const Timestep&);

Timestep operator- (const Timestep& step);
Timestep operator- (const Time&, const Time&);
Timestep operator+ (const Timestep&, const Timestep&);

#endif // TIMESTEP_H
