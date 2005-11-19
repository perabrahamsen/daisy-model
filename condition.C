// condition.C -- Logic expressions
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


#include "condition.h"

template<>
Librarian<Condition>::Content* Librarian<Condition>::content = NULL;

const char *const Condition::description = "\
A 'condition' component tests the state of the simulation, like\n\
whether the water pressure in a specific depth is above a given\n\
threshold.  Logic conditions like 'and' and 'or' can be used for\n\
testing whether multiple conditions are fulfilled simultaneously.";

void
Condition::tick (const Daisy&, Treelog&)
{ }

const std::string
Condition::timestep ()
{ return "dt"; } 

Condition::Condition (Block& al)
  : name (al.identifier ("type"))
{ }

Condition::~Condition ()
{ }

