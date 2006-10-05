// bioclimate_std.C --- The default biclimate model
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
#include "surface.h"
#include "weather.h"
#include "plf.h"
#include "alist.h"
#include "geometry.h"
#include "soil.h"
#include "soil_heat.h"
#include "timestep.h"
#include "syntax.h"
#include "snow.h"
#include "log.h"
#include "mathlib.h"
#include "pet.h"
#include "difrad.h"
#include "svat.h"
#include "vegetation.h"
#include "chemicals.h"
#include "time.h"
#include <sstream>

struct BioclimateStandard : public Bioclimate
{ 
  // Canopy State.
  const long No;		// No of intervals in canopy discretization.
  double LAI_;			// Total LAI of all crops on this column. [0-]
  std::vector<double> Height;	// Height in cm of each endpoint in c.d.
  std::vector<double> PAR_;     // PAR of each interval of c.d.
  double shared_light_fraction_; // Fraction of field with light competition.

  // Canopy Tick.
  void CanopyStructure (const Vegetation&);

  // External water sinks and sources.
  std::auto_ptr<Pet> pet;       // Potential Evapotranspiration model.
  double total_ep;		// Potential evapotranspiration [mm/h]
  double total_ea;		// Actual evapotranspiration [mm/h]

  double irrigation_overhead;	// Irrigation above canopy [mm/h]
  double irrigation_overhead_old;	// Old value for logging.
  double irrigation_overhead_temperature; // Water temperature [dg C]
  double irrigation_surface;	// Irrigation below canopy [mm/h]
  double irrigation_surface_old; // Old value for logging.
  double irrigation_surface_temperature; // Water temperature [dg C]
  double irrigation_subsoil;	// Irrigation incorporated in soil.
  double irrigation_subsoil_old; // Old value for logging.
  double irrigation_subsoil_permanent;	// Irrigation incorporated in soil.

  // Water in snowpack.
  Snow snow;
  double snow_ep;		// Potential snow evaporation [mm/h]
  double snow_ea;		// Actual snow evaporation [mm/h]
  double snow_water_in;		// Water entering snow pack [mm/h]
  double snow_water_in_temperature; // Incoming water temperature [dg C]
  double snow_water_out;	// Water leaving snow pack [mm/h]
  double snow_water_out_temperature; // Temperature of water leaving [dg C]

  // Water intercepted on canopy.
  double canopy_ep;		// Potential canopy evaporation [mm/h]
  double canopy_ea;		// Actual canopy evaporation [mm/h]
  double canopy_water_storage;	// Intercepted water on canopy [mm]
  double canopy_water_temperature; // Temperature of incoming water [dg C]
  double canopy_water_in;	// Water entering canopy [mm/h]
  double canopy_water_out;	// Canopy drip throughfall [mm/h]
  double canopy_water_bypass;	// Water from above bypassing the canopy [mm/h]
  
  // Water intercepted on litter.
  double litter_ep;		// Potential litter evaporation [mm/h]
  double litter_ea;		// Actual litter evaporation [mm/h]
  double litter_water_storage;	// Intercepted water in litter [mm]
  double litter_water_temperature; // Temperature of water in litter [dg C]
  double litter_water_in;	// Water entering litter [mm/h]
  double litter_water_out;	// Water leaving litter [mm/h]

  // Water in pond.
  double pond_ep;		// Potential evaporation from pond [mm/h]
  double pond_ea;		// Actual evaporation from pond [mm/h]

  // Water going through soil surface.
  double soil_ep;		// Potential exfiltration. [mm/h]
  double soil_ea;		// Actual exfiltration. [mm/h]

  // Water transpirated through plant roots.
  std::auto_ptr<SVAT> svat;     // Soil Vegetation Atmosphere model.
  double crop_ep;		// Potential transpiration. [mm/h]
  double crop_ea;		// Actual transpiration. [mm/h]
  double production_stress;	// Stress calculated by SVAT module.

  void WaterDistribution (const Time&,
                          Surface& surface, const Weather& weather, 
			  Vegetation& vegetation, const Movement&,
                          const Geometry&, const Soil& soil,
			  SoilWater& soil_water, const SoilHeat&, Treelog&);

  // Chemicals.
  Chemicals spray_;

  Chemicals snow_chemicals_storage;
  Chemicals snow_chemicals_in;
  Chemicals snow_chemicals_out;
  
  Chemicals canopy_chemicals_storage;
  Chemicals canopy_chemicals_in;
  Chemicals canopy_chemicals_dissipate;
  Chemicals canopy_chemicals_out;

  Chemicals surface_chemicals_in;

  void ChemicalDistribution (Surface& surface, const Vegetation&);

  // Radiation.
  void RadiationDistribution (const Vegetation&);
  std::auto_ptr<Difrad> difrad;  // Diffuse radiation model.
  double difrad0;                // Diffuse radiation above canopy [W/m2]

  // Weather.
  double daily_air_temperature_; // Air temperature in canopy. [dg C]
  double daily_precipitation_; // From weather. [mm]
  double day_length_;		// From weather (does not belong here) [h].
  double daily_global_radiation_; // From weather [W/m2].
  double hourly_global_radiation_; // From weather [W/m2].

  // Simulation
  void tick (const Time&, Surface&, const Weather&, 
	     Vegetation&, const Movement&, const Geometry&,
             const Soil&, SoilWater&, const SoilHeat&, Treelog&);
  void output (Log&) const;

  // Canopy.
  const std::vector<double>& height () const
  { return Height; }
  const std::vector<double>& PAR () const
  { return PAR_; }
  double LAI () const
  { return LAI_; }
  double shared_light_fraction () const
 { return shared_light_fraction_; }

  // Weather.
  double daily_air_temperature () const
  { return daily_air_temperature_; }
  double daily_precipitation () const
  { return daily_precipitation_; }
  double day_length () const
  { return day_length_; }
  double daily_global_radiation () const
  { return daily_global_radiation_; }
  double hourly_global_radiation () const
  { return hourly_global_radiation_; }

  // Manager.
  void irrigate_overhead (double flux, double temp);
  void irrigate_surface (double flux, double temp);
  void irrigate_overhead (double flux);
  void irrigate_surface (double flux);
  void irrigate_subsoil (double flux);
  void set_subsoil_irrigation (double flux);
  void spray (symbol chemical, double amount) // [g/m^2]
  { spray_.add (chemical, amount); }
  void harvest_chemicals (Chemicals& chemicals, double LAI)
  { 
    if (LAI_ > 0.0)
      Chemicals::move_fraction (canopy_chemicals_storage, chemicals,
                                LAI / LAI_);
  }
  
  // Communication with external model.
  double get_evap_interception () const // [mm/h]
  { return canopy_ea; }
  double get_intercepted_water () const // [mm]
  { return canopy_water_storage; }
  double get_net_throughfall () const // [mm/h]
  { return litter_water_out; }
  double get_snow_storage () const // [mm]
  { return snow.storage (); }
  // Create.
  void initialize (const Weather&, Treelog&);
  static void load_syntax (Syntax& syntax, AttributeList& alist);
  BioclimateStandard (Block&);
  ~BioclimateStandard ();
};

void 
BioclimateStandard::initialize (const Weather& weather, Treelog& msg)
{
  if (pet.get ())                      // Explicit.
    return;

  Treelog::Open nest (msg, "Bioclimate: " + name);

  if (!pet.get ())                      // Explicit.
    {
      symbol type;

      if (weather.has_reference_evapotranspiration ())
	type = symbol ("weather");
      else if (weather.has_vapor_pressure () && weather.has_wind ())
	{
	  if (weather.surface () == Weather::field)
	    type = symbol ("PM");
	  else
	    type = symbol ("FAO_PM");    
	}
      else if (weather.has_min_max_temperature ())
	type = symbol ("Hargreaves");
      else
	type = symbol ("makkink");

      msg.debug ("Pet choosen: " + type);

      const Library& library = Librarian<Pet>::library ();
      daisy_assert (library.check (type));
  
      AttributeList alist (library.lookup (type));
      alist.add ("type", type);
      daisy_assert (library.syntax (type).check (alist, msg));
      pet.reset (Librarian<Pet>::build_free (msg, alist, "pet"));
    }

  if (!difrad.get ())                      // Explicit.
    {
      symbol type;

      if (weather.has_diffuse_radiation ())
	type = symbol ("weather");
      else
	type = symbol ("DPF");

      msg.debug ("Difrad choosen: " + type);

      const Library& library = Librarian<Difrad>::library ();
      daisy_assert (library.check (type));
  
      AttributeList alist (library.lookup (type));
      alist.add ("type", type);
      daisy_assert (library.syntax (type).check (alist, msg));
      difrad.reset (Librarian<Difrad>::build_free (msg, alist, "difrad"));
    }
}

void 
BioclimateStandard::load_syntax (Syntax& syntax, AttributeList& alist)
{
  // Canopy structure.
  syntax.add ("NoOfIntervals", Syntax::Integer, Syntax::Const, "\
Number of vertical intervals in which we partition the canopy.");
  alist.add ("NoOfIntervals", 30);

  // External water sources and sinks.
  syntax.add ("pet", Librarian<Pet>::library (), 
              Syntax::OptionalState, Syntax::Singleton, 
              "Potential Evapotranspiration component.\n\
\n\
By default, choose depending on available climate date.\n\
\n\
If reference evaporation is available in the climate data, Daisy will\n\
use these (the weather pet model).\n\
\n\
If vapor pressure and wind are available, it will use Penman-Monteith (PM).\n\
\n\
If the timestep is larger than 12, and daily minimum and maximum\n\
temperature are available,  Samani and Hargreaves (Hargreaves).\n\
\n\
As a last resort,  Makkink (makkink) will be used.");
  syntax.add ("total_ep", "mm/h", Syntax::LogOnly,
              "Potential evapotranspiration.");
  syntax.add ("total_ea", "mm/h", Syntax::LogOnly,
              "Actual evapotranspiration.");
  syntax.add ("irrigation_overhead", "mm/h", Syntax::LogOnly,
              "Irrigation above canopy.");
  syntax.add ("irrigation_overhead_temperature", "dg C", Syntax::LogOnly,
              "Water temperature.");
  syntax.add ("irrigation_surface", "mm/h", Syntax::LogOnly,
              "Irrigation below canopy.");
  syntax.add ("irrigation_surface_temperature", "dg C", Syntax::LogOnly,
              "Water temperature.");
  syntax.add ("irrigation_subsoil", "mm/h", Syntax::LogOnly,
              "Irrigation below soil surface this hour.");
  syntax.add ("irrigation_subsoil_permanent", "mm/h", Syntax::State,
              "Long term irrigation below soil surface.");
  alist.add ("irrigation_subsoil_permanent", 0.0);
  syntax.add ("irrigation_total", "mm/h", Syntax::LogOnly,
              "Total irrigation above of below the soil surface.");

  // Water in snowpack.
  syntax.add_submodule ("Snow", alist, Syntax::State, 
                        "Surface snow pack.",
                        Snow::load_syntax);
  syntax.add ("snow_ep", "mm/h", Syntax::LogOnly,
              "Potential snow evaporation.");
  syntax.add ("snow_ea", "mm/h", Syntax::LogOnly,
              "Actual snow evaporation.");
  syntax.add ("snow_water_in", "mm/h", Syntax::LogOnly,
              "Water entering snow pack.");
  syntax.add ("snow_water_in_temperature", "dg C", Syntax::LogOnly,
              "Temperature of water entering snow pack.");
  syntax.add ("snow_water_out", "mm/h", Syntax::LogOnly,
              "Water leaving snow pack");
  syntax.add ("snow_water_out_temperature", "dg C", Syntax::LogOnly,
              "Temperature of water leaving snow pack.");

  // Water intercepted on canopy.
  syntax.add ("canopy_ep", "mm/h", Syntax::LogOnly,
              "Potential canopy evaporation.");
  syntax.add ("canopy_ea", "mm/h", Syntax::LogOnly,
              "Actual canopy evaporation.");
  syntax.add ("canopy_water_storage", "mm", Syntax::State,
              "Intercepted water on canopy.");
  alist.add ("canopy_water_storage", 0.0);
  syntax.add ("canopy_water_temperature", "dg C", Syntax::LogOnly,
              "Temperature of incoming water.");
  syntax.add ("canopy_water_in", "mm/h", Syntax::LogOnly,
              "Water entering canopy.");
  syntax.add ("canopy_water_out", "mm/h", Syntax::LogOnly,
              "Canopy drip throughfall.");
  syntax.add ("canopy_water_bypass", "mm/h", Syntax::LogOnly,
              "Water from above bypassing the canopy.");
  
  // Water intercepted by litter.
  syntax.add ("litter_ep", "mm/h", Syntax::LogOnly,
              "Potential evaporation litter.");
  syntax.add ("litter_ea", "mm/h", Syntax::LogOnly,
              "Actual litter evaporation.");
  syntax.add ("litter_water_storage", "mm", Syntax::State,
              "Intercepted water on litter.");
  alist.add ("litter_water_storage", 0.0);
  syntax.add ("litter_water_temperature", "dg C", Syntax::LogOnly,
              "Temperature of incoming water.");
  syntax.add ("litter_water_in", "mm/h", Syntax::LogOnly,
              "Water entering litter.");
  syntax.add ("litter_water_out", "mm/h", Syntax::LogOnly,
              "Litter drip throughfall.");
  
  // Water in pond.
  syntax.add ("pond_ep", "mm/h", Syntax::LogOnly,
              "Potential evaporation from pond.");
  syntax.add ("pond_ea", "mm/h", Syntax::LogOnly,
              "Actual evaporation from pond.");

  // Water going through soil surface.
  syntax.add ("svat", Librarian<SVAT>::library (), 
              "Soil Vegetation Atmosphere component.");
  AttributeList svat_alist;
  svat_alist.add ("type", "none");
  alist.add ("svat", svat_alist);
  syntax.add ("soil_ep", "mm/h", Syntax::LogOnly,
              "Potential exfiltration.");
  syntax.add ("soil_ea", "mm/h", Syntax::LogOnly,
              "Actual exfiltration.");

  // Water transpirated through plant roots.
  syntax.add ("crop_ep", "mm/h", Syntax::LogOnly,
              "Potential transpiration.");
  syntax.add ("crop_ea", "mm/h", Syntax::LogOnly,
              "Actual transpiration.");
  syntax.add ("production_stress", Syntax::None (), Syntax::LogOnly,
              "SVAT module induced stress, -1 means use water stress.");

  // Chemicals.
  Chemicals::add_syntax  ("spray", syntax, alist, Syntax::LogOnly,
                          "Chemicals sprayed on field this time step.");

  Chemicals::add_syntax  ("snow_chemicals_storage", 
                          syntax, alist, Syntax::State,
                          "Chemicals stored in the snow pack.");
  Chemicals::add_syntax  ("snow_chemicals_in",
                          syntax, alist, Syntax::LogOnly,
                          "Chemicals entering snow pack.");
  Chemicals::add_syntax  ("snow_chemicals_out",
                          syntax, alist, Syntax::LogOnly,
                          "Chemicals leaking from snow pack.");

  Chemicals::add_syntax  ("canopy_chemicals_storage", 
                          syntax, alist, Syntax::State,
                          "Chemicals stored on canopy.");
  Chemicals::add_syntax  ("canopy_chemicals_in",
                          syntax, alist, Syntax::LogOnly,
                          "Chemicals entering canopy.");
  Chemicals::add_syntax  ("canopy_chemicals_dissipate",
                          syntax, alist, Syntax::LogOnly,
                          "Chemicals dissipating from canopy.");
  Chemicals::add_syntax  ("canopy_chemicals_out",
                          syntax, alist, Syntax::LogOnly,
                          "Chemicals falling through canopy.");

  Chemicals::add_syntax  ("surface_chemicals_in",
                          syntax, alist, Syntax::LogOnly,
                          "Chemicals entering soil surface.");
  //Radiation
  syntax.add ("difrad", Librarian<Pet>::library (), 
              Syntax::OptionalState, Syntax::Singleton, 
              "Diffuse radiation component.\n\
\n\
By default, choose depending on available climate date.\n\
\n\
If diffuse radiation is available in the climate data, Daisy will\n\
use these (the weather difrad model). Otherwise Daisy wil use the DPF model.");
  syntax.add ("difrad0", "W/m^2", Syntax::LogOnly,
              "Diffuse radiation above canopy.");
}

const AttributeList& 
Bioclimate::default_model ()
{
  static AttributeList alist;
  
  if (!alist.check ("type"))
    {
      Syntax dummy;
      BioclimateStandard::load_syntax (dummy, alist);
      alist.add ("type", "default");
    }
  return alist;
}

BioclimateStandard::BioclimateStandard (Block& al)
  : Bioclimate (al),
    No (al.integer ("NoOfIntervals")),
    LAI_ (0.0),
    Height (No + 1),
    PAR_ (No + 1),
    shared_light_fraction_ (1.0),
    pet (al.check ("pet") 
         ? Librarian<Pet>::build_item (al, "pet")
         : NULL),
    total_ep (0.0),
    total_ea (0.0),
    irrigation_overhead (0.0),
    irrigation_overhead_old (0.0),
    irrigation_overhead_temperature (0.0),
    irrigation_surface (0.0),
    irrigation_surface_old (0.0),
    irrigation_surface_temperature (0.0),
    irrigation_subsoil (0.0),
    irrigation_subsoil_old (0.0),
    irrigation_subsoil_permanent (al.number ("irrigation_subsoil_permanent")),
    snow (al.alist ("Snow")),
    snow_ep (0.0),
    snow_ea (0.0),
    snow_water_in (0.0),
    snow_water_in_temperature (0.0),
    snow_water_out (0.0),
    snow_water_out_temperature (0.0),
    canopy_ep (0.0),
    canopy_ea (0.0),
    canopy_water_storage (al.number ("canopy_water_storage")),
    canopy_water_temperature (0.0),
    canopy_water_in (0.0),
    canopy_water_out (0.0),
    canopy_water_bypass (0.0),
    litter_ep (0.0),
    litter_ea (0.0),
    litter_water_storage (al.number ("litter_water_storage")),
    litter_water_temperature (0.0),
    litter_water_in (0.0),
    litter_water_out (0.0),
    pond_ep (0.0),
    pond_ea (0.0),
    soil_ep (0.0),
    soil_ea (0.0),
    svat (Librarian<SVAT>::build_item (al, "svat")),
    crop_ep (0.0),
    crop_ea (0.0),
    production_stress (-1.0),
    spray_ (),
    snow_chemicals_storage (al.alist_sequence ("snow_chemicals_storage")),
    snow_chemicals_in (),
    snow_chemicals_out (),

    canopy_chemicals_storage (al.alist_sequence ("canopy_chemicals_storage")),
    canopy_chemicals_in (),
    canopy_chemicals_dissipate (),
    canopy_chemicals_out (),

    surface_chemicals_in (),
    difrad (al.check ("difrad") 
         ? Librarian<Difrad>::build_item (al, "difrad")
         : NULL),
    difrad0 (0.0),

    // BUG: These should really be part of the state, for checkpoints.
    daily_air_temperature_ (0.0),
    daily_precipitation_ (0.0),
    day_length_ (0.0),
    daily_global_radiation_ (0.0),
    hourly_global_radiation_ (0.0)
{ }

BioclimateStandard::~BioclimateStandard ()
{ }

void
BioclimateStandard::CanopyStructure (const Vegetation& vegetation)
  // Calculate values for the total crop canopy.
{
  shared_light_fraction_ = vegetation.shared_light_fraction ();

  // Update vegetation state.
  const PLF& HvsLAI = vegetation.HvsLAI ();
  LAI_ = vegetation.LAI ();
  
  // Reset PAR intervals.
  std::fill (Height.begin (), Height.end (), 0.0);

  if (LAI_ > 0.0)
    {
      // Count height of each interval.  Interval 0 is the top of the crop
      // and interval "No" is group zero (no bomb intended).
      double dLAI = LAI_ / No;
      for (int i = 0; i <= No; i++)
	Height[i] = HvsLAI ((No - i) * dLAI);

      daisy_assert (!std::isnormal (Height[No]));
      //  daisy_assert (approximate (Height[0], MxH));
      Height[0] = vegetation.height ();
    }
}

void 
BioclimateStandard::RadiationDistribution (const Vegetation& vegetation)
{
  if (!std::isnormal (LAI ()))
    {
      std::fill (&PAR_[0], &PAR_[No+1], 0.0);
      return;
    }

  // Average Canopy Extinction coefficient
  // (how fast the light dim as a  function of LAI passed).
  const double ACExt = vegetation.ACExt ();

  // Average Canopy Reflection coefficient
  const double ACRef =  vegetation.ACRef ();

#if 0
  // Average Radiation Extinction coefficient
  // (like ACExt, but for all radiation, not just light).
  const double ARExt = vegetation.EPext ();
#endif 

  radiation_distribution (No, LAI_, ACRef, hourly_global_radiation (),
                          ACExt, PAR_);
}

void
BioclimateStandard::WaterDistribution (const Time& time, Surface& surface,
				       const Weather& weather, 
				       Vegetation& vegetation,
                                       const Movement& movement,
                                       const Geometry& geo,
				       const Soil& soil, 
				       SoilWater& soil_water,
				       const SoilHeat& soil_heat, Treelog& msg)
{
  // Overview.
  //
  // First we calculate the external water sources (precipitation,
  // irrigation) and sinks (potential evapotranspiration).  Then we
  // update the sources, sinks and storage for each of the following
  // four "containers": The snow pack, water intercepted on the canopy, 
  // pond, and soil surface.  It is assumed that each the flows for
  // each container only depends on the container above, so we
  // calculate the changes from the top and downwards.
  //
  // A final and fifth "container" is the crop transpiration.

  // 1 External water sinks and sources. 

  // 1.1 Evapotranspiration
  daisy_assert (pet.get () != NULL);
  pet->tick (time, 
             weather, vegetation, surface, 
             geo, soil, soil_heat, soil_water, msg);
  total_ep = pet->wet ();
  daisy_assert (total_ep >= 0.0);
  total_ea = 0.0;		// To be calculated.

  // 1.2 Irrigation
  //
  // Already set by manager.

  // 1.3 Weather.
  double rain = weather.rain ();
  double air_temperature = weather.hourly_air_temperature ();

  // 2 Snow Pack

  snow_ep = total_ep - total_ea;
  daisy_assert (snow_ep >= 0.0);
  snow_water_in = rain + irrigation_overhead;
  daisy_assert (snow_water_in >= 0.0);
  if (irrigation_overhead > 0.01)
    snow_water_in_temperature 
      = (irrigation_overhead * irrigation_overhead_temperature
	 + rain * air_temperature) / snow_water_in;
  else
    snow_water_in_temperature = air_temperature;
  snow.tick (msg, movement, soil, soil_water, soil_heat, 
	     weather.hourly_global_radiation (), 0.0,
	     snow_water_in, weather.snow (),
	     surface.ponding (),
	     snow_water_in_temperature, snow_ep);
  snow_ea = snow.evaporation ();
  daisy_assert (snow_ea >= 0.0);
  total_ea += snow_ea;
  daisy_assert (total_ea >= 0.0);
  snow_water_out = snow.percolation ();
  if (snow_water_out < 0.0)
    {
      daisy_assert (snow_water_out + surface.ponding () >= 0.0);
      surface.put_ponding (snow_water_out + surface.ponding ());
      snow_water_out = 0.0;
    }
  snow_water_out_temperature = snow.temperature ();

  // 3 Water intercepted on canopy

  const double canopy_water_capacity = vegetation.interception_capacity ();
  daisy_assert (canopy_water_capacity >= 0.0);
  
  daisy_assert (snow_ea <= total_ep);
  canopy_ep = (total_ep - snow_ea) * vegetation.cover ();
  daisy_assert (canopy_ep >= 0.0);
  if (snow_water_out < 0.0)
    {
      canopy_water_in = 0.0;
      canopy_water_bypass = snow_water_out;
    }
  else
    {
      canopy_water_in = snow_water_out * vegetation.cover ();
      canopy_water_bypass = snow_water_out - canopy_water_in;
    }
  daisy_assert (canopy_water_in >= 0.0);

  if (canopy_water_in > 0.01)
    canopy_water_temperature 
      = (canopy_water_storage * air_temperature 
	 + canopy_water_in * snow_water_out_temperature)
      / (canopy_water_storage + canopy_water_in);
  else
    canopy_water_temperature = air_temperature;

  canopy_ea = std::min (canopy_ep, canopy_water_storage / dt + canopy_water_in);
  daisy_assert (canopy_ea >= 0.0);
  total_ea += canopy_ea;
  daisy_assert (total_ea >= 0.0);
  
  canopy_water_storage += (canopy_water_in - canopy_ea) * dt;
  if (canopy_water_storage < 0.0)
    {
      daisy_assert (canopy_water_storage > -1e-8);
      canopy_water_storage = 0.0;
    }
  
  if (canopy_water_storage > canopy_water_capacity + 1e-8)
    {
      canopy_water_out = canopy_water_storage - canopy_water_capacity;
      canopy_water_storage = canopy_water_capacity;
    }
  else
    {
      canopy_water_out = 0.0;
    }
  daisy_assert (canopy_water_out >= 0.0);

  // 4 Water intercepted by litter

  litter_ep = (total_ep - snow_ea) * (1.0 - vegetation.cover ());
  daisy_assert (snow_ea <= total_ep);
  if (litter_ep < 0.0)
    {
      std::ostringstream tmp;
      tmp << "BUG:\nlitter_ep = " << litter_ep << "\n"
          << "total_ep = " << total_ep << "\n"
          << "snow_ea = " << snow_ea << "\n"
          << "cover  = " << vegetation.cover ();
      msg.error (tmp.str ());
      litter_ep = 0.0;
    }

  litter_water_in 
    = canopy_water_out + canopy_water_bypass + irrigation_surface;

  if (litter_water_in > 0.01)
    litter_water_temperature 
      = (litter_water_storage * air_temperature
         + canopy_water_bypass * snow_water_out_temperature
	 + canopy_water_out * canopy_water_temperature
	 + irrigation_surface * irrigation_surface_temperature)
      / (litter_water_in + litter_water_storage);
  else
    litter_water_temperature = air_temperature;

  daisy_assert (litter_water_storage >= 0.0);
  litter_ea = std::max (std::min (litter_ep,
                        litter_water_storage / dt + litter_water_in),
                   0.0);
  total_ea += litter_ea;
  daisy_assert (total_ea >= 0.0);
  
  const double litter_water_capacity = vegetation.litter_water_capacity ();
  litter_water_storage += (litter_water_in - litter_ea) * dt;
  if (litter_water_storage < 0.0)
    {
      if (litter_water_storage < -1e-8)
        {
          std::ostringstream tmp;
          tmp << "BUG:\n"
              << "litter_water_storage = " << litter_water_storage << "\n"
              << "litter_water_capacity = " << litter_water_capacity << "\n"
              << "litter_water_in = " << litter_water_in << "\n"
              << "litter_ea = " << litter_ea << "\n"
              << "litter_ep = " << litter_ep;
          msg.error (tmp.str ());
        }
      litter_water_out = litter_water_storage;
      litter_water_storage = 0.0;
    }
  else if (litter_water_storage > litter_water_capacity + 1e-8)
    {
      litter_water_out = litter_water_storage - litter_water_capacity;
      litter_water_storage = litter_water_capacity;
    }
  else
    litter_water_out = 0.0;

  // 5 Ponding

  pond_ep = (litter_ep - litter_ea) * vegetation.litter_vapor_flux_factor();
  daisy_assert (litter_ea <= litter_ep);
  if (pond_ep < 0.0)
    {
      std::ostringstream tmp;
      tmp << "BUG:\npond_ep = " << pond_ep << "\n"
          << "litter_ep = " << litter_ep << "\n"
          << "litter_ea = " << litter_ea << "\n"
          << "litter_factor = " << vegetation.litter_vapor_flux_factor ();
      msg.error (tmp.str ());
      pond_ep = 0.0;
    }

  const double soil_T = geo.content_at (soil_heat, &SoilHeat::T, 0.0);
  surface.tick (msg, pond_ep, 
		litter_water_out, litter_water_temperature, 
		geo, soil, soil_water, soil_T);
  pond_ea = surface.evap_pond (msg);
  daisy_assert (pond_ea >= 0.0);
  total_ea += pond_ea;
  daisy_assert (total_ea >= 0.0);

  // 6 Soil

  soil_ep = pond_ep - pond_ea;
  daisy_assert (soil_ep >= 0.0);
  soil_ea = surface.exfiltration ();
  daisy_assert (soil_ea >= 0.0);
  total_ea += soil_ea;
  daisy_assert (total_ea >= 0.0);

  // 7 Transpiration

  // Potential transpiration
  const double potential_crop_transpiration = canopy_ep - canopy_ea;
  const double potential_soil_transpiration 
    = (pond_ep - pond_ea - soil_ea) * vegetation.EpInterchange ();
  crop_ep = bound (0.0, 
		   potential_crop_transpiration + potential_soil_transpiration,
		   std::max (0.0, pet->dry ()));

  // Actual transpiration
  const double day_fraction
    = (daily_global_radiation_ > 0.0)
    ? (weather.hourly_global_radiation () / (24.0 * daily_global_radiation_))
    : 0.0;
  crop_ea = vegetation.transpiration (crop_ep, canopy_ea, geo, soil,
                                      soil_water, 
				      day_fraction, msg);
  daisy_assert (crop_ea >= 0.0);
  total_ea += crop_ea;
  daisy_assert (total_ea >= 0.0);

  // Production stress
  svat->tick (weather, vegetation, surface, soil, soil_heat, soil_water, *pet,
              canopy_ea, snow_ea, pond_ea + litter_ea,
              soil_ea, crop_ea, crop_ep);
  production_stress = svat->production_stress ();
  vegetation.force_production_stress (production_stress);

  // 8 Reset irrigation
  irrigation_overhead_old = irrigation_overhead;
  irrigation_overhead = 0.0;
  irrigation_surface_old = irrigation_surface;
  irrigation_surface = 0.0;
  irrigation_subsoil_old = irrigation_subsoil;
  irrigation_subsoil = 0.0;
  
  // 9 Check
  // Note: total_ea can be larger than total_ep, as PMSW uses a
  // different method for calculating PET.
  daisy_assert (approximate (total_ea,
                             snow_ea + canopy_ea + litter_ea
                             + pond_ea + soil_ea + crop_ea));
}  

void 
BioclimateStandard::tick (const Time& time, 
                          Surface& surface, const Weather& weather,  
			  Vegetation& vegetation, const Movement& movement,
                          const Geometry& geo, const Soil& soil, 
			  SoilWater& soil_water, const SoilHeat& soil_heat,
			  Treelog& msg)
{
  // Keep weather information during time step.
  // Remember this in case the crops should ask.
  hourly_global_radiation_ = weather.hourly_global_radiation ();
  daily_global_radiation_ = weather.daily_global_radiation ();
  daily_air_temperature_ = weather.daily_air_temperature ();
  daily_precipitation_ = weather.daily_precipitation ();
  day_length_ = weather.day_length ();
  
  // Add nitrogen deposit. 
  surface.fertilize (weather.deposit ());

  // Update canopy structure.
  CanopyStructure (vegetation);
 
  // Radiation.
  daisy_assert (difrad.get () != NULL);
  difrad0 = difrad->value (time, weather, msg) * hourly_global_radiation_;
  //daisy_assert (difrad0 >= 0.0);

  // Calculate total canopy, divide it intervalsm, and distribute PAR.
  RadiationDistribution (vegetation);

  // Distribute water among canopy, snow, and soil.
  WaterDistribution (time, surface, weather, vegetation, 
		     movement, geo, soil, soil_water, soil_heat, msg);

  // Let the chemicals follow the water.
  ChemicalDistribution (surface, vegetation);
}

void 
BioclimateStandard::ChemicalDistribution (Surface& surface, 
					  const Vegetation& vegetation)
{
  const double cover = vegetation.cover ();

  // Snow pack
  snow_chemicals_in = spray_;
  snow_chemicals_storage += snow_chemicals_in;
  const double snow_water_storage = snow.storage ();
  const double snow_chemicals_out_fraction
    = (snow_water_storage > 0.01)
    ? snow_water_out / (snow_water_out + snow_water_storage)
    : 1.0;
  snow_chemicals_out.clear ();
  Chemicals::move_fraction (snow_chemicals_storage,
			    snow_chemicals_out, 
			    snow_chemicals_out_fraction);
  
  // Canopy
  canopy_chemicals_in.clear ();
  Chemicals::copy_fraction (snow_chemicals_out, canopy_chemicals_in, cover);
  canopy_chemicals_storage.canopy_update (canopy_chemicals_in, 
					  canopy_water_storage,
					  canopy_water_out,
					  canopy_chemicals_dissipate,
					  canopy_chemicals_out);
  
  // Surface
  surface_chemicals_in.clear ();
  Chemicals::copy_fraction (snow_chemicals_out, surface_chemicals_in, 
			    1.0 - cover);
  surface_chemicals_in += canopy_chemicals_out;
  surface.spray (surface_chemicals_in);

  // Reset spray.
  spray_.clear ();
}

void 
BioclimateStandard::output (Log& log) const
{
  daisy_assert (pet.get () != NULL);
  output_object (pet.get (), "pet", log);
  output_variable (total_ep, log);
  output_variable (total_ea, log);
  output_value (irrigation_overhead_old, "irrigation_overhead", log);
  output_value (irrigation_overhead_temperature, 
                "irrigation_overhead_temperature", log);
  output_value (irrigation_surface_old, "irrigation_surface", log);
  output_value (irrigation_surface_temperature,
                "irrigation_surface_temperature", log);
  output_value (irrigation_subsoil_old + irrigation_subsoil_permanent,
                "irrigation_subsoil", log);
  output_variable (irrigation_subsoil_permanent, log);
  output_value (irrigation_subsoil_old  + irrigation_subsoil_permanent
                + irrigation_surface_old + irrigation_overhead_old, 
                "irrigation_total", log);
  output_submodule (snow, "Snow", log);
  output_variable (snow_ep, log);
  output_variable (snow_ea, log);
  output_variable (snow_water_in, log);
  output_variable (snow_water_in_temperature, log);
  output_variable (snow_water_out, log);
  output_value (snow_water_out_temperature, 
                "snow_water_out_temperature", log);
  output_variable (canopy_ep, log);
  output_variable (canopy_ea, log);
  output_variable (canopy_water_storage, log);
  output_variable (canopy_water_temperature, log);
  output_variable (canopy_water_in, log);
  output_variable (canopy_water_out, log);
  output_variable (canopy_water_bypass, log);
  output_variable (litter_ep, log);
  output_variable (litter_ea, log);
  output_variable (litter_water_storage, log);
  output_variable (litter_water_temperature, log);
  output_variable (litter_water_in, log);
  output_variable (litter_water_out, log);
  output_variable (pond_ep, log);
  output_variable (pond_ea, log);
  output_variable (soil_ep, log);
  output_variable (soil_ea, log);
  output_derived (svat, "svat", log);
  output_variable (crop_ep, log);
  output_variable (crop_ea, log);
  output_variable (production_stress, log);

  // Note: We use snow_chemicals_in instead of spray, since the former
  // is reset after each time step.
  output_submodule (snow_chemicals_in, "spray", log);
  output_submodule (snow_chemicals_storage, "snow_chemicals_storage", log);
  output_submodule (snow_chemicals_in, "snow_chemicals_in", log);
  output_submodule (snow_chemicals_out, "snow_chemicals_out", log);
  output_submodule (canopy_chemicals_storage, "canopy_chemicals_storage", log);
  output_submodule (canopy_chemicals_in, "canopy_chemicals_in", log);
  output_submodule (canopy_chemicals_dissipate, "canopy_chemicals_dissipate",
		    log);
  output_submodule (canopy_chemicals_out, "canopy_chemicals_out", log);
  output_submodule (surface_chemicals_in, "surface_chemicals_in", log);
  //radiation
  daisy_assert (difrad.get () != NULL);
  output_object (difrad.get (), "difrad", log);
  output_variable (difrad0, log);
}

void
BioclimateStandard::irrigate_overhead (double flux, double temp)
{
  double new_top = irrigation_overhead + flux;
  irrigation_overhead_temperature 
    = (temp * flux + irrigation_overhead * irrigation_overhead_temperature) / new_top;
  irrigation_overhead = new_top;
}

void
BioclimateStandard::irrigate_surface (double flux, double temp)
{
  double new_surface = irrigation_surface + flux;
  irrigation_surface_temperature 
    = (temp * flux
       + irrigation_surface * irrigation_surface_temperature) / new_surface;
  irrigation_surface = new_surface;
}

void
BioclimateStandard::irrigate_overhead (double flux)
{ irrigate_overhead (flux, daily_air_temperature ()); }

void
BioclimateStandard::irrigate_surface (double flux)
{ irrigate_surface (flux, daily_air_temperature ()); }

void
BioclimateStandard::irrigate_subsoil (double flux)
{ irrigation_subsoil += flux; }

void
BioclimateStandard::set_subsoil_irrigation (double flux)
{ irrigation_subsoil_permanent = flux; }

static struct BioclimateStandardSyntax
{
  static Bioclimate& make (Block& al)
  { return *new BioclimateStandard (al); }
  
  BioclimateStandardSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
 
    BioclimateStandard::load_syntax (syntax, alist);
    // Add to library.
    Librarian<Bioclimate>::add_type ("default", alist, syntax, &make);
  }
} BioclimateStandard_syntax;

// bioclimate_std.C ends here
