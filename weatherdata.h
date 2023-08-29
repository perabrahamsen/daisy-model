// weatherdata.h -- Weather related utilities.
// 
// Copyright 2010 KU
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


#ifndef WEATHERDATA_H
#define WEATHERDATA_H

#include "symbol.h"

class Frame;
class Block;

namespace Weatherdata
{
  // Surface type.
  enum surface_t { reference, field };
  symbol reference_name ();
  symbol field_name ();
  surface_t symbol2surface (const symbol);
  symbol surface2symbol (surface_t);
  
  // Variable symbols.
  symbol GlobRad ();
  symbol AirTemp ();
  symbol T_min ();
  symbol T_max ();
  symbol Precip ();
  symbol RefEvap ();
  symbol VapPres ();
  symbol DiffRad ();
  symbol GHF ();
  symbol CloudinessIndex ();
  symbol RelHum ();
  symbol Wind ();
  symbol CO2 ();
  
  // Stationary symbols.
  symbol Latitude ();
  symbol Longitude ();
  symbol Elevation ();
  symbol TimeZone ();
  symbol ScreenHeight ();
  symbol TAverage ();
  symbol TAmplitude ();
  symbol MaxTDay ();
  symbol NH4WetDep ();
  symbol NH4DryDep ();
  symbol NO3WetDep ();
  symbol NO3DryDep ();
  symbol Deposition ();
  symbol DepDry ();
  symbol DepDryNH4 ();
  symbol DepWetNH4 ();
  symbol PAverage ();
  symbol Timestep ();

  // Symbols representing non-numeric values.
  symbol Station ();
  symbol Note ();
  symbol Surface ();
  symbol PrecipCorrect ();
  symbol PrecipScale ();
  symbol GlobRadScale ();
  symbol TempOffset ();
  symbol Begin ();
  symbol End ();
  
  // Information about a symbol.
  symbol dimension (const symbol);
  symbol description (const symbol);
  double min_value (const symbol);
  double max_value (const symbol);
  symbol meta_key (const symbol);

  // Frame.
  void load_syntax (Frame&);
}

#endif // WEATHERDATA_H
