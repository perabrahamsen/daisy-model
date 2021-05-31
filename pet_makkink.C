// pet_makkink.C -- Potential evopotranspiration using Makkink's Equation.
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

#include "pet.h"
#include "weather.h"
#include "fao.h"
#include "log.h"
#include "librarian.h"
#include "frame.h"
#include "block_model.h"

// The 'makkink' model.

struct PetMakkinkOld : public Pet
{
  // State.
  double reference_evapotranspiration;
  double potential_evapotranspiration_dry;
  double potential_evapotranspiration_wet;

  // Simulation.
  void tick (const Weather& weather,
	     const double, const double, const double,
	     const Vegetation& crops,
	     const Surface& surface, const Geometry&,
             const Soil&, const SoilHeat&,
	     const SoilWater&, Treelog&)
    {
      reference_evapotranspiration 
	= FAO::Makkink (weather.air_temperature (),
			weather.global_radiation ());
    
      potential_evapotranspiration_dry 
	= reference_to_potential_dry (crops, surface, 
                                      reference_evapotranspiration);
      potential_evapotranspiration_wet 
	= reference_to_potential_wet (crops, surface, 
                                      reference_evapotranspiration);
    }

  void output (Log& log) const
    {
      Pet::output (log);
      output_value (reference_evapotranspiration, 
		    "reference_evapotranspiration", log);
    }

  double dry () const
  { return potential_evapotranspiration_dry; }
  double wet () const
  { return potential_evapotranspiration_wet; }

  // Create.
  PetMakkinkOld (const BlockModel& al)
    : Pet (al)
    { }
};

static struct PetMakkinkOldSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
    { return new PetMakkinkOld (al); }
  PetMakkinkOldSyntax ()
    : DeclareModel (Pet::component, "makkink", 
		    "Potential evopotranspiration using Makkink's Equation (old version).")
  { }
  void load_frame (Frame&) const
  { }
} PetMakkinkOld_syntax;

// The 'Makkink' model.

struct PetMakkink : public Pet
{
  const bool use_daily_temperature;
  const double beta_M0;		// [mm/d]
  const double beta_M1;		// []
  
  // State.
  double reference_evapotranspiration;
  double potential_evapotranspiration_dry;
  double potential_evapotranspiration_wet;

  // Simulation.
  void tick (const Weather& weather,
	     const double, const double, const double,
	     const Vegetation& crops,
	     const Surface& surface, const Geometry&,
             const Soil&, const SoilHeat&,
	     const SoilWater&, Treelog&)
    {
      const double Temp = use_daily_temperature
	? weather.daily_air_temperature ()
	: weather.air_temperature (); // [dg C]
      const double AtmPressure = weather.air_pressure (); // [Pa]
      const double Si_now =		    // [J/m^2/d]
	weather.global_radiation () /* [W/m^2] */
	* 24.0 * 3600.0 /* [s/d] */;
      const double Si_day =		    // [J/m^2/d]
	weather.global_radiation () /* [W/m^2] */
	* 24.0 * 3600.0 /* [s/d] */;
      const double s  		// [Pa/K]
	= FAO::SlopeVapourPressureCurve (Temp);
      const double lambda = FAO::LatentHeatVaporization (Temp); // [J/kg]
      const double gamma	// [Pa/K]
	= FAO::PsychrometricConstant (AtmPressure, Temp);
      const double E_M		// [mm/d]
	= beta_M0 + beta_M1 * (s * Si_day) / (lambda * (s + gamma));
      if (Si_day > 0.0)
	{
	  const double Si_rel = Si_now / Si_day;
	  reference_evapotranspiration = Si_rel * E_M / 24.0 /* [h/d] */;
	}
      else
	reference_evapotranspiration = 0.0;
      
      potential_evapotranspiration_dry 
	= reference_to_potential_dry (crops, surface, 
                                      reference_evapotranspiration);
      potential_evapotranspiration_wet 
	= reference_to_potential_wet (crops, surface, 
                                      reference_evapotranspiration);
    }

  void output (Log& log) const
    {
      Pet::output (log);
      output_value (reference_evapotranspiration, 
		    "reference_evapotranspiration", log);
    }

  double dry () const
  { return potential_evapotranspiration_dry; }
  double wet () const
  { return potential_evapotranspiration_wet; }

  // Create.
  PetMakkink (const BlockModel& al)
    : Pet (al),
      use_daily_temperature (al.flag ("use_daily_temperature")),
      beta_M0 (al.number ("beta_M0")),
      beta_M1 (al.number ("beta_M1"))
    { }
};

static struct PetMakkinkSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
    { return new PetMakkink (al); }
  PetMakkinkSyntax ()
    : DeclareModel (Pet::component, "Makkink", 
		    "Potential evopotranspiration using Makkink's Equation.\n\
E_M = beta_M0 + beta_M1 * (s * Si) / (lambda (s + gamma)) .\n\
\n\
E_M is the reference evapotranspiration [mm/d]. \n\
beta_M0 [mm] and beta_M1 [] are empirical parameters.\n\
Si is global radiation [MJ/m^2/d].\n\
s is the slope of the vapour pressure curve [Pa/K].\n\
lambda is latent heat of vapourization [MJ/kg].\n\
gamma is the psychrometric constant [Pa/K].")
  { }
  void load_frame (Frame& frame) const
  {
    frame.set_strings ("cite", "makkink1957ekzameno");
    frame.declare_boolean ("use_daily_temperature", Attribute::Const, "\
Use daily temperature values even if hourly are available.");
    frame.set ("use_daily_temperature", true);
    frame.declare ("beta_M0", "mm/d", Attribute::Const, "\
Makkink parameter");
    frame.declare ("beta_M1", Attribute::None (), Attribute::Const, "\
Makkink parameter");
  }
} PetMakkink_syntax;

// The 'Makkink57' parameterization.

static struct ChemicalMakkink57Syntax : public DeclareParam
{ 
  ChemicalMakkink57Syntax ()
    : DeclareParam (Pet::component, "Makkink57", "Makkink", "\
Original parameterization.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.set_strings ("cite", "makkink1957ekzameno");
    frame.set ("beta_M0", -0.12);
    frame.set ("beta_M1", 0.61);
  }
} ChemicalMakkink57_syntax;

// The 'AslyngHansen82' parameterization.

static struct ChemicalAslyngHansen82Syntax : public DeclareParam
{ 
  ChemicalAslyngHansen82Syntax ()
    : DeclareParam (Pet::component, "AslyngHansen82", "Makkink", "\
Danish calibration.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.set_strings ("cite", "aslyng1982water", "Hansen205");
    frame.set ("beta_M0", 0.0);
    frame.set ("beta_M1", 0.7);
  }
} ChemicalAslyngHansen82_syntax;

// The 'deBruin87' parameterization.

static struct ChemicaldeBruin87Syntax : public DeclareParam
{ 
  ChemicaldeBruin87Syntax ()
    : DeclareParam (Pet::component, "deBruin87", "Makkink", "\
Dutch calibration.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.set_strings ("cite", "deBruin1987");
    frame.set ("beta_M0", 0.0);
    frame.set ("beta_M1", 0.65);
  }
} ChemicaldeBruin87_syntax;

// pet_makkink.C ends here.
