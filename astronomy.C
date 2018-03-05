// astronomy.C --- Astronomic utility functions.
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

#define BUILD_DLL

#include "astronomy.h"
#include "time.h"
#include "mathlib.h"
#include <algorithm>

double
Astronomy::SolarDeclination (const Time& time) // [rad]
{
  return (0.409 * sin (2.0 * M_PI * time.year_fraction () - 1.39));
}

double
Astronomy::RelativeSunEarthDistance (const Time& time) // []
{
  return (1.0 + 0.033 * cos (2.0 * M_PI * time.year_fraction ()));
}

double
Astronomy::SunsetHourAngle (double Dec, double Lat) // [rad]
{
  return (acos (-tan (Dec) * tan (Lat)));
}

const double SolarConstant = 1366.7; // {W/m2]

double
Astronomy::DailyExtraterrestrialRadiation (const Time& time,
                                           const double latitude)
// [W/m2]
{
  const double Dec = SolarDeclination (time);
  const double Lat = M_PI / 180 * latitude;
  const double x1 = SunsetHourAngle (Dec, Lat) * sin (Lat) * sin (Dec);
  const double x2 = cos (Lat) * cos (Dec) * sin (SunsetHourAngle (Dec, Lat));
  return (SolarConstant * RelativeSunEarthDistance (time) * (x1 + x2) / M_PI);
}

double
Astronomy::ExtraterrestrialRadiation (const Time& time,
                                      const double latitude,
                                      const double longitude,
                                      const double timezone) // [W/m2]
{
  return std::max (SolarConstant * RelativeSunEarthDistance (time) 
                   * SinSolarElevationAngle (time, 
                                             latitude, longitude, timezone),
                   0.0);
}

double
Astronomy::SinSolarElevationAngle (const Time& time,
                                   const double latitude,
                                   const double longitude,
                                   const double timezone) // []
{
  static const double EQT0   = 0.002733;
  static const double EQT1[] = {-7.343,-9.470,-0.3289,-0.1955};
  static const double EQT2[] = {0.5519,-3.020,-0.07581,-0.1245};
  const double Dec = SolarDeclination (time);
  
  const double Lat = M_PI / 180.0 * latitude;
  const double timelag = (timezone - longitude) / 15.0;
  double EQT = EQT0;
  for (unsigned int i = 0; i < 3; i++)
    {
       const double P = 2.0 * M_PI * (i+1) * time.year_fraction ();
       EQT += EQT1[i] * sin(P) + EQT2[i] * cos(P);
    }
  EQT /= 60.0;
  const double SunHourAngle = M_PI / 12.0 
    * (time.day_fraction () * 24.0 + EQT - timelag + 12);
  return (sin(Lat)*sin(Dec) + cos(Lat)*cos(Dec)*cos(SunHourAngle));
}

double
Astronomy::DayLength (const Time& time, const double latitude)
{
  double t = 2 * M_PI * time.year_fraction ();

  const double Dec = (0.3964 - 22.97 * cos (t) + 3.631 * sin (t)
		      - 0.03885 * cos (2 * t)
		      + 0.03838 * sin (2 * t) - 0.15870 * cos (3 * t)
		      + 0.07659 * sin (3 * t) - 0.01021 * cos (4 * t));
  double my_tan 
    = -tan (M_PI / 180.0 * Dec) * tan (M_PI / 180.0 * latitude);
  if (my_tan <= -1.0)
    my_tan = -1.0;
  else if (my_tan >= 1.0)
    my_tan = 1.0;
  t = (24 / M_PI * acos (my_tan));
  const double dl = (t < 0) ? t + 24.0 : t;
  daisy_assert (dl >= 0.0);
  daisy_assert (dl <= 24.0);
  return dl;
}
// astronomy.C ends here.
