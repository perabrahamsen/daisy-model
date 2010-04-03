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

#define BUILD_DLL

#include "condition.h"
#include "block_model.h"
#include "librarian.h"

const char *const Condition::component = "condition";

symbol 
Condition::library_id () const
{
  static const symbol id (component);
  return id;
}

symbol
Condition::timestep ()
{ 
  static const symbol name ("dt");
  return name; 
} 

Condition::Condition (const BlockModel& al)
  : ModelFramed (al)
{ }

void 
Condition::initiate_log (const Daisy&, const Time& previous)
{ }

Condition::Condition (const char *const id)
  : ModelFramed (symbol (id))
{ }

Condition::~Condition ()
{ }

static struct ConditionInit : public DeclareComponent 
{
  ConditionInit ()
    : DeclareComponent (Condition::component, "\
A 'condition' component tests the state of the simulation, like\n\
whether the water pressure in a specific depth is above a given\n\
threshold.  Logic conditions like 'and' and 'or' can be used for\n\
testing whether multiple conditions are fulfilled simultaneously.")
  { }
} Condition_init;

// condition.C ends here.
