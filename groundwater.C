// groundwater.C
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


#include "groundwater.h"
#include "log.h"

EMPTY_TEMPLATE
Librarian<Groundwater>::Content* Librarian<Groundwater>::content = NULL;

const char *const Groundwater::description = "\
The 'groundwater' component is responsible for specifying the\n\
groundwater table at each timestep.";

void
Groundwater::update_water (const Soil&, const SoilHeat&,
			   vector<double>& /* S_sum */,
			   vector<double>& /* S_drain */,
			   vector<double>& /* h */,
			   vector<double>& /* h_ice */,
			   vector<double>& /* Theta */,
			   vector<double>& /* q */,
			   vector<double>& /* q_p */,
			   Treelog&)
{ }

void
Groundwater::output (Log& log) const
{ 
  log.output ("height", table ());
}

void
Groundwater::load_syntax (Syntax& syntax, AttributeList&)
{
  syntax.add ("height", "cm", Syntax::LogOnly,
	      "Groundwater level.  Positive numbers indicate free drainage.");
}

void
Groundwater::initialize (const Time&, const Soil&, Treelog&)
{ }

Groundwater::Groundwater (const AttributeList& al)
  : name (al.name ("type"))
{ }

Groundwater::~Groundwater ()
{ }

