// rainergy.C --- Energy in rain.
// 
// Copyright 2009 Per Abrahamsen and KVL.
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

#include "rainergy.h"
#include "mathlib.h"
#include "librarian.h"
#include "block_model.h"
#include "frame.h"

// The 'rainergy' component.

const char *const Rainergy::component = "rainergy";

symbol 
Rainergy::library_id () const
{
  static const symbol id (component);
  return id;
}

Rainergy::Rainergy ()
{ }

Rainergy::~Rainergy ()
{ }

static struct RainergyInit : public DeclareComponent 
{
  RainergyInit ()
    : DeclareComponent (Rainergy::component, "\
Energy in rain.")
  { }
  void load_frame (Frame& frame) const
  { Model::load_model (frame); }
} Rainergy_init;

// The 'Brown87' model.

struct RainergyBrown87 : public Rainergy
{
  // Simulation.
  double value (const double total_rain /* [mm/h] */, 
                const double direct_rain /* [mm/h] */,
                const double canopy_drip /* [mm/h] */,
                const double canopy_height /* [m] */) const // [J/cm^2/h].
  { 
    const double R = total_rain; // [mm/h]
    const double E = 29.0 * (1.0 - 0.72 * std::exp (-0.05 * R)); // [J/m^2/mm]
    
    const double KE_DT = E * direct_rain; // [J/m^2/h]
    return KE_DT * 1e-4;                  // [J/cm^2/h]
  }
  // Create and Destroy.
  RainergyBrown87 (const BlockModel&)
  { }
  ~RainergyBrown87 ()
  { }
};

static struct RainergyBrown87Syntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new RainergyBrown87 (al); }
  RainergyBrown87Syntax ()
    : DeclareModel (Rainergy::component, "Brown87", "\
Energy as a semi-empirical function of rain intensity.\n\
The energy content in the fraction that hits the canopy is ignored.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.set_strings ("cite", "brown87");
  }
} RainergyBrown87_syntax;

// The 'EUROSEM' model.

struct RainergyEUROSEM : public Rainergy
{
  // Simulation.
  double value (const double total_rain /* [mm/h] */, 
                const double direct_rain /* [mm/h] */,
                const double canopy_drip /* [mm/h] */,
                const double canopy_height /* [m] */) const // [J/cm^2/h].
  { 
    // Intensity.
    const double I = std::max (1.0, total_rain); // [mm/h]

    // Direct rain energy.
    const double DT = direct_rain; // [mm/h]
    const double KE_DT = (8.95 + 8.44 * std::log (I)) * DT; // [J/m^2/h]

    // Canopy drip energy.
    const double LD = canopy_drip; // [mm/h]
    const double PH = canopy_height;        // [m]
    const double KE_LD = (15.8 * std::sqrt (PH) - 5.87) * LD; // [J/m^2/h]
    
    // Total energy.
    const double KE_T = KE_DT + std::max (KE_LD, 0.0); // [J/m^2/h]

    // Convert.
    return KE_T * 1e-4; // [J/cm^2/h]
  }

  // Create and Destroy.
  RainergyEUROSEM (const BlockModel&)
  { }
  ~RainergyEUROSEM ()
  { }
};

static struct RainergyEUROSEMSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new RainergyEUROSEM (al); }
  RainergyEUROSEMSyntax ()
    : DeclareModel (Rainergy::component, "EUROSEM", "\
Kinetic energy model taking vegetation into account.")
  { }
  void load_frame (Frame& frame) const
  { frame.set_strings ("cite", "EUROSEM"); }
} RainergyEUROSEM_syntax;

// rainergy.C ends here.
