// GP2D.h -- Gerwitz and Page model extended for row crops.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
// Copyright 2007 Per Abrahamsen and KVL.
// Copyright 2011 KU.
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

#ifndef GP2D_H
#define GP2D_H

class Treelog;

class GP1D
{
  // Static parameters.
  const double DensRtTip;      // Root density at (pot) pen. depth. [cm/cm^3]
  const double SpRtLength;     // Specific root length [m/g]

  // Helper class.
  class InvW;

  // Calculated parameters.
public:
  double a;                    // Form parameter. [cm^-1]
  double L0;		       // Root density at row at soil surface. [cm/cm^3]
  double d_a;                  // Actual root depth. [cm]
  double kstar;		       // Scale factor due to soil limit. []

  // Use.
public:
  bool set_dynamic (double SoilDepth /* [cm] */, 
                    double CropDepth /* [cm] */,
                    double WRoot /* [g DM/m^2] */, 
                    int debug /* Debug level. */,
                    Treelog&);
public:
  double density (double z /* [cm] */) const;
  
  // Create.
public:
  GP1D (const double DensRtTip /* [cm/cm^3] */,
        const double SpRtLength /* [m/g] */);
};

class GP2D
{
  // Static parameters.
public:
  const double row_position;   // Horizontal position of row crops. [cm]
  const double row_distance;   // Distance betweeen rows. [cm]
  const double DensRtTip;      // Root density at (pot) pen. depth. [cm/cm^3]
  const double SpRtLength;     // Specific root length [m/g]

  // Helper class.
private:
  class InvQ;

  // Calculated parameters.
public:
  double a_z;                  // Form parameter. [cm^-1]
  double a_x;                  // Form parameter. [cm^-1]
  double L00;		       // Root density at row at soil surface. [cm/cm^3]
  double d_a;                  // Actual root depth. [cm]
  double kstar;		       // Scale factor due to soil limit. []

  // Use.
public:
  bool set_dynamic (double SoilDepth /* [cm] */, 
                    double CropDepth /* [cm] */,
                    double CropWidth /* [cm] */,
                    double WRoot /* [g DM/m^2] */, 
                    int debug /* Debug level. */,
                    Treelog&);
public:
  double density (double x /* [cm] */, double z /* [cm] */) const;
  
  // Create.
public:
  GP2D (const double row_position /* [cm] */,
        const double row_distance /* [cm] */,
        const double DensRtTip /* [cm/cm^3] */,
        const double SpRtLength /* [m/g] */);
};

#endif // GP2D_H
