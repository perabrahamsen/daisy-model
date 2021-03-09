// astronomy.h --- Astronomic utility functions.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
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


#ifndef ASTRONOMY_H
#define ASTRONOMY_H

class Time;

namespace Astronomy
{ 
  double SolarDeclination (const Time&); // [rad]
  double RelativeSunEarthDistance (const Time&);   // []
  double SunsetHourAngle (double Dec, double Lat); // [rad]
  double DailyExtraterrestrialRadiation (const Time&, 
                                         const double latitude); // [W/m2]
  double ExtraterrestrialRadiation (const Time& begin, const Time& end,
				    const double latitude /* [dg N] */,
				    const double longitude  /* [dg E] */,
				    const double timezone); // [W/m2]
  double SinSolarElevationAngle (const Time& time,
                                 const double latitude,
                                 const double longitude,
                                 const double timezone); // []
  double DayLength (const Time& time, const double latitude); // [h]
}

#endif // ASTRONOMY_H
