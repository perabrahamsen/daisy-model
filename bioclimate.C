// bioclimate.C
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


#include "bioclimate.h"
#include "weather.h"
#include "mathlib.h"

EMPTY_TEMPLATE
Librarian<Bioclimate>::Content* Librarian<Bioclimate>::content = NULL;

const char *const Bioclimate::description = "\
The 'bioclimate' component is responsible for distributing the water\n\
and energy provided by the weather component among the crops and soil\n\
for a given column.";

double 
Bioclimate::get_evap_interception () const
{ daisy_assert (false); }

double 
Bioclimate::get_intercepted_water () const
{ daisy_assert (false); }

double 
Bioclimate::get_net_throughfall () const
{ daisy_assert (false); }

double 
Bioclimate::get_snow_storage () const
{ daisy_assert (false); }

Bioclimate::Bioclimate (const string& n)
  : name (n)
{ }

Bioclimate::~Bioclimate ()
{ }
