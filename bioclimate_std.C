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
#include "soil.h"
#include "common.h"
#include "syntax.h"
#include "snow.h"
#include "log.h"
#include "mathlib.h"
#include "pet.h"
#include "svat.h"
#include "vegetation.h"
#include "chemicals.h"

struct BioclimateStandard : public Bioclimate
{ 
  // Canopy State.
  const long No;		// No of intervals in canopy discretation.
  double LAI_;			// Total LAI of all crops on this column. [0-]
  vector<double> Height;	// Height in cm of each endpoint in c.d.
  vector<double> PAR_;		// PAR of each interval of c.d.

  // Canopy Tick.
  void CanopyStructure (const Vegetation&);

  // External water sinks and sources.
  Pet& pet;			// Potential Evapotranspiration model.
  double total_ep;		// Potential evapotranspiration [mm/h]
  double total_ea;		// Actual evapotranspiration [mm/h]

  double irrigation_overhead;	// Irrigation above canopy [mm/h]
  double irrigation_overhead_old;	// Old value for logging.
  double irrigation_overhead_temperature; // Water temperature [dg C]
  double irrigation_surface;	// Irrigation below canopy [mm/h]
  double irrigation_surface_old; // Old value for logging.
  double irrigation_surface_temperature; // Water temperature [dg C]

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
  
  // Water in pond.
  double pond_ep;		// Potential evaporation from pond [mm/h]
  double pond_ea;		// Actual evaporation from pond [mm/h]
  double pond_water_in;		// Water entering pond [mm/h]
  double pond_water_in_temperature; // Temperature of water entering [dg C]

  // Water going through soil surface.
  double soil_ep;		// Potential exfiltration. [mm/h]
  double soil_ea;		// Actual exfiltration. [mm/h]

  // Water transpirated through plant roots.
  SVAT& svat;			// Soil Vegetation Atmosphere model.
  double crop_ep;		// Potential transpiration. [mm/h]
  double crop_ea;		// Actual transpiration. [mm/h]
  double production_stress;	// Stress calculated by SVAT module.

  void WaterDistribution (Surface& surface, const Weather& weather, 
			  Vegetation& vegetation, const Soil& soil,
			  SoilWater& soil_water, const SoilHeat&);

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
  void RadiationDistribution (const Weather&, const Vegetation&);
  void IntensityDistribution (double Rad0, double Ext, 
			      vector <double>& Rad) const;

  // Weather.
  double daily_air_temperature_; // Air temperature in canopy. [dg C]
  double day_length_;		// From weather (does not belong here) [h].
  double daily_global_radiation_; // From weather [W/m2].

  // Simulation
  void tick (Surface&, const Weather&, 
	     Vegetation&, const Soil&, SoilWater&, const SoilHeat&);
  void output (Log&) const;

  // Canopy.
  int NumberOfIntervals () const
    { return No; }
  double height (int i) const
    { return Height[i]; }
  double PAR (int i) const
    { return PAR_[i]; }
  double LAI () const
    { return LAI_; }

  // Weather.
  double daily_air_temperature () const
    { return daily_air_temperature_; }
  double day_length () const
    { return day_length_; }
  double daily_global_radiation () const
    { return daily_global_radiation_; }

  // Manager.
  void irrigate_overhead (double flux, double temp);
  void irrigate_surface (double flux, double temp);
  void irrigate_overhead (double flux);
  void irrigate_surface (double flux);
  void spray (const string& chemical, double amount) // [g/m^2]
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
    { return pond_water_in; }
  double get_snow_storage () const // [mm]
    { return snow.storage (); }
  // Create.
  BioclimateStandard (const AttributeList&);
  ~BioclimateStandard ();
};

BioclimateStandard::BioclimateStandard (const AttributeList& al)
  : Bioclimate (al.name ("type")),
    No (al.integer ("NoOfIntervals")),
    LAI_ (0.0),
    Height (al.integer ("NoOfIntervals") + 1),
    PAR_ (al.integer ("NoOfIntervals") + 1),
    pet (Librarian<Pet>::create (al.alist ("pet"))),
    total_ep (0.0),
    total_ea (0.0),
    irrigation_overhead (0.0),
    irrigation_overhead_old (0.0),
    irrigation_overhead_temperature (0.0),
    irrigation_surface (0.0),
    irrigation_surface_old (0.0),
    irrigation_surface_temperature (0.0),
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
    pond_ep (0.0),
    pond_ea (0.0),
    pond_water_in (0.0),
    pond_water_in_temperature (0.0),
    soil_ep (0.0),
    soil_ea (0.0),
    svat (Librarian<SVAT>::create (al.alist ("svat"))),
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
    daily_air_temperature_ (0.0),
    day_length_ (0.0),
    daily_global_radiation_ (0.0)
{ }

BioclimateStandard::~BioclimateStandard ()
{ 
  delete &pet;
  delete &svat;
}

void
BioclimateStandard::CanopyStructure (const Vegetation& vegetation)
  // Calculate values for the total crop canopy.
{

  // Update vegetation state.
  const PLF& HvsLAI = vegetation.HvsLAI ();
  LAI_ = vegetation.LAI ();
  
  // Reset PAR intervals.
  fill (Height.begin (), Height.end (), 0.0);

  if (LAI_ > 0.0)
    {
      // Count height of each interval.  Interval 0 is the top of the crop
      // and interval "No" is group zero (no bomb intended).
      double dLAI = LAI_ / No;
      for (int i = 0; i <= No; i++)
	Height[i] = HvsLAI ((No - i) * dLAI);

      assert (Height[No] == 0.0);
      //  assert (approximate (Height[0], MxH));
      Height[0] = vegetation.height ();
    }
}

void 
BioclimateStandard::RadiationDistribution (const Weather& weather, 
				      const Vegetation& vegetation)
{
  if (LAI () == 0.0)
    {
      fill (&PAR_[0], &PAR_[No+1], 0.0);
      return;
    }

  // Fraction of Photosynthetically Active Radiation in Shortware
  // incoming radiation. 
  static const double PARinSi = 0.50;	

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

  const double PAR0 
    = (1 - ACRef) * PARinSi * weather.hourly_global_radiation ();
  IntensityDistribution (PAR0, ACExt, PAR_);
}

void
BioclimateStandard::IntensityDistribution (const double Rad0,
				      const double Ext,
				      vector <double>& Rad) const
{
  assert (Rad.size () == No + 1);
  const double dLAI = (LAI_ / No);
    
  for (int i = 0; i <= No; i++)
    Rad[i] = Rad0 * exp (- Ext * dLAI * i);
}

void
BioclimateStandard::WaterDistribution (Surface& surface,
				       const Weather& weather, 
				       Vegetation& vegetation,
				       const Soil& soil, 
				       SoilWater& soil_water,
				       const SoilHeat& soil_heat)
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
  pet.tick (weather, vegetation, surface, soil, soil_heat, soil_water);
  total_ep = pet.wet ();
  assert (total_ep >= 0.0);
  total_ea = 0.0;		// To be calculated.

  // 1.2 Irrigation
  //
  // Already set by manager.

  // 1.3 Weather.
  double rain = weather.rain ();
  double air_temperature = weather.hourly_air_temperature ();

  // 2 Snow Pack

  snow_ep = total_ep - total_ea;
  assert (snow_ep >= 0.0);
  snow_water_in = rain + irrigation_overhead;
  assert (snow_water_in >= 0.0);
  if (irrigation_overhead > 0.01)
    snow_water_in_temperature 
      = (irrigation_overhead * irrigation_overhead_temperature
	 + rain * air_temperature) / snow_water_in;
  else
    snow_water_in_temperature = air_temperature;
  snow.tick (soil, soil_water, soil_heat, 
	     weather.hourly_global_radiation (), 0.0,
	     snow_water_in, weather.snow (),
	     surface.ponding (),
	     snow_water_in_temperature, snow_ep);
  snow_ea = snow.evaporation ();
  assert (snow_ea >= 0.0);
  total_ea += snow_ea;
  assert (total_ea >= 0.0);
  snow_water_out = snow.percolation ();
  snow_water_out_temperature = snow.temperature ();

  // 3 Water intercepted on canopy

  const double canopy_water_capacity = vegetation.interception_capacity ();
  assert (canopy_water_capacity >= 0.0);
  canopy_ep = (total_ep - snow_ea) * vegetation.cover ();
  assert (canopy_ep >= 0.0);
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

  assert (canopy_water_in >= 0.0);

  if (canopy_water_in > 0.01)
    canopy_water_temperature 
      = (canopy_water_storage * air_temperature 
	 + canopy_water_in * snow_water_out_temperature)
      / (canopy_water_storage + canopy_water_in);
  else
    canopy_water_temperature = air_temperature;

  canopy_ea = min (canopy_ep, canopy_water_storage / dt + canopy_water_in);
  assert (canopy_ea >= 0.0);
  total_ea += canopy_ea;
  assert (total_ea >= 0.0);
  
  canopy_water_storage += (canopy_water_in - canopy_ea) * dt;
  if (canopy_water_storage < 0.0)
    {
      assert (canopy_water_storage > -1e-8);
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
  assert (canopy_water_out >= 0.0);

  // 4 Ponding

  pond_ep = (total_ep - snow_ea) * (1.0 - vegetation.cover ());
  assert (pond_ep >= 0.0);
  pond_water_in = canopy_water_out + canopy_water_bypass + irrigation_surface;
  if (pond_water_in > 0.01)
    pond_water_in_temperature 
      = (canopy_water_bypass * snow_water_out_temperature
	 + canopy_water_out * canopy_water_temperature
	 + irrigation_surface * irrigation_surface_temperature)
      / pond_water_in;
  else
    pond_water_in_temperature = air_temperature;

  surface.tick (pond_ep, 
		pond_water_in, pond_water_in_temperature, 
		soil, soil_water);
  pond_ea = surface.evap_pond ();
  assert (pond_ea >= 0.0);
  total_ea += pond_ea;
  assert (total_ea >= 0.0);

  // 5 Soil

  soil_ep = pond_ep - pond_ea;
  assert (soil_ep >= 0.0);
  soil_ea = surface.exfiltration ();
  assert (soil_ea >= 0.0);
  total_ea += soil_ea;
  assert (total_ea >= 0.0);

  // 6 Transpiration

  // Potential transpiration
  const double potential_crop_transpiration = canopy_ep - canopy_ea;
  const double potential_soil_transpiration 
    = (pond_ep - pond_ea - soil_ea) * surface.EpInterchange ();
  crop_ep = bound (0.0, 
		   potential_crop_transpiration + potential_soil_transpiration,
		   max (0.0, pet.dry ()));

  // Actual transpiration
  crop_ea = vegetation.transpiration (crop_ep, canopy_ea, soil, soil_water);
  assert (crop_ea >= 0.0);
  total_ea += crop_ea;
  assert (total_ea >= 0.0);

  // Production stress
  svat.tick (weather, vegetation, surface, soil, soil_heat, soil_water, pet,
	     canopy_ea, snow_ea, pond_ea, soil_ea, crop_ea, crop_ep);
  production_stress = svat.production_stress ();
  vegetation.force_production_stress (production_stress);

  // 7 Reset irrigation
  irrigation_overhead_old = irrigation_overhead;
  irrigation_overhead = 0.0;
  irrigation_surface_old = irrigation_surface;
  irrigation_surface = 0.0;

  // 8 Check
  // Note: total_ea can be larger than total_ep, as PMSW uses a
  // different method for calculating PET.
  assert (approximate (total_ea,
		       snow_ea + canopy_ea + pond_ea + soil_ea + crop_ea));
}  

void 
BioclimateStandard::tick (Surface& surface, const Weather& weather, 
			  Vegetation& vegetation, const Soil& soil, 
			  SoilWater& soil_water, const SoilHeat& soil_heat)
{
  // Keep weather information during time step.
  // Remember this in case the crops should ask.
  daily_global_radiation_ = weather.daily_global_radiation ();
  daily_air_temperature_ = weather.daily_air_temperature ();
  day_length_ = weather.day_length ();

  // Add nitrogen deposit. 
  surface.fertilize (weather.deposit ());

  // Update canopy structure.
  CanopyStructure (vegetation);

  // Calculate total canopy, divide it intervalsm, and distribute PAR.
  RadiationDistribution (weather, vegetation);

  // Distribute water among canopy, snow, and soil.
  WaterDistribution (surface, weather, vegetation,
		     soil, soil_water, soil_heat);

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
					  canopy_water_out);
  canopy_chemicals_storage.canopy_dissipate (canopy_chemicals_dissipate);
  canopy_chemicals_storage.canopy_out (canopy_chemicals_out,
				       canopy_water_storage,
				       canopy_water_out);
  canopy_chemicals_storage.cleanup (canopy_chemicals_out);
  
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
  output_derived (pet, "pet", log);
  log.output ("total_ep", total_ep);
  log.output ("total_ea", total_ea);
  log.output ("irrigation_overhead", irrigation_overhead_old);
  log.output ("irrigation_overhead_temperature", 
	      irrigation_overhead_temperature);
  log.output ("irrigation_surface", irrigation_surface_old);
  log.output ("irrigation_surface_temperature",
	      irrigation_surface_temperature);
  output_submodule (snow, "Snow", log);
  log.output ("snow_ep", snow_ep);
  log.output ("snow_ea", snow_ea);
  log.output ("snow_water_in", snow_water_in);
  log.output ("snow_water_in_temperature", 
	      snow_water_in_temperature);
  log.output ("snow_water_out", snow_water_out);
  log.output ("snow_water_out_temperature", 
	      snow_water_out_temperature);
  log.output ("canopy_ep", canopy_ep);
  log.output ("canopy_ea", canopy_ea);
  log.output ("canopy_water_storage", canopy_water_storage);
  log.output ("canopy_water_temperature", canopy_water_temperature);
  log.output ("canopy_water_in", canopy_water_in);
  log.output ("canopy_water_out", canopy_water_out);
  log.output ("canopy_water_bypass", canopy_water_bypass);
  log.output ("pond_ep", pond_ep);
  log.output ("pond_ea", pond_ea);
  log.output ("pond_water_in", pond_water_in);
  log.output ("pond_water_in_temperature", 
	      pond_water_in_temperature);
  log.output ("soil_ep", soil_ep);
  log.output ("soil_ea", soil_ea);
  output_derived (svat, "svat", log);
  log.output ("crop_ep", crop_ep);
  log.output ("crop_ea", crop_ea);
  log.output ("production_stress", production_stress);

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

static struct BioclimateStandardSyntax
{
  static Bioclimate& make (const AttributeList& al)
    { return *new BioclimateStandard (al); }
  
  BioclimateStandardSyntax ()
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
  
      // Canopy structure.
      syntax.add ("NoOfIntervals", Syntax::Integer, Syntax::Const, "\
Number of vertical intervals in which we partition the canopy.");
      alist.add ("NoOfIntervals", 30);

      // External water sources and sinks.
      syntax.add ("pet", Librarian<Pet>::library (), 
		  "Potential Evapotranspiration component.");
      AttributeList& pet_alist = *new AttributeList;
      pet_alist.add ("type", "makkink");
      alist.add ("pet", pet_alist);
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
  
      // Water in pond.
      syntax.add ("pond_ep", "mm/h", Syntax::LogOnly,
		  "Potential evaporation from pond.");
      syntax.add ("pond_ea", "mm/h", Syntax::LogOnly,
		  "Actual evaporation from pond.");
      syntax.add ("pond_water_in", "mm/h", Syntax::LogOnly,
		  "Water entering pond.");
      syntax.add ("pond_water_in_temperature", "dg C", Syntax::LogOnly,
		  "Temperature of water entering pond.");

      // Water going through soil surface.
      syntax.add ("svat", Librarian<SVAT>::library (), 
		  "Soil Vegetation Atmosphere component.");
      AttributeList& svat_alist = *new AttributeList;
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

      // Add to library.
      Librarian<Bioclimate>::add_type ("default", alist, syntax, &make);
    }
} BioclimateStandard_syntax;

// bioclimate_std.C ends here
