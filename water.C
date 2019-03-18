// water.C --- Water properties.
// 
// Copyright 2009 Per Abrahamsen and KU.
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

#include "water.h"
#include "plf.h"

double 
Water::viscosity (const double T)
{ 
  static struct table : public PLF
  {
    table ()
    {
      add ( 0.0, 1.786e-3);   // [dg C -> Pa s]
      add ( 5.0, 1.519e-3);
      add (10.0, 1.307e-3);
      add (15.0, 1.139e-3);
      add (20.0, 1.002e-3);
      add (25.0, 0.890e-3);
      add (30.0, 0.798e-3);
      add (35.0, 0.719e-3);
      add (40.0, 0.658e-3);
    }
  } table;
  
  return table (T);
}

// water.C ends here.
