// weather.C
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

#include "weather.h"
#include "block.h"
#include "librarian.h"

const char *const Weather::component = "weather";

symbol
Weather::library_id () const
{
  static const symbol id (component);
  return id;
}

const symbol 
Weather::dry_deposit_unit ()
{
  static const symbol unit ("kg/ha/y");
  return unit;
}

Weather::Weather (Block& al)
  : ModelLogable (al.identifier ("type"))
{ }

Weather::~Weather ()
{ }

static Librarian Weather_init (Weather::component, "\
Meteorological data, as well as the global positioning, are the\n\
responsibility of the 'weather' component, typically be reading the\n\
data from a file.  The meteorological data are common to all columns.");


// weather.C ends here.
