// movement.h
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


#ifndef MOVEMENT_H
#define MOVEMENT_H

#include "librarian.h"

class Movement
{
  // Content.
public:
  const symbol name;
  static const char *const description;

  // Simulation.
public:
  virtual void output (Log&) const = 0;

  // Create and Destroy.
protected:
  Movement (Block&);
public:
  virtual ~Movement ();
};

#ifdef FORWARD_TEMPLATES
template<>
Librarian<Movement>::Content* Librarian<Movement>::content;
#endif

static Librarian<Movement> Movement_init ("movement");

#endif // MOVEMENT_H
