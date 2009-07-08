// bioclimate_std.C --- The default biclimate model
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
//
// This file is part of Daisy.
// 
// Daisy is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser P   //ublic License as published by
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

#include "bioclimate.h"
#include "block.h"
#include "surface.h"
#include "weather.h"
#include "plf.h"
#include "frame.h"
#include "geometry.h"
#include "soil.h"
#include "soil_heat.h"
#include "chemistry.h"
#include "chemical.h"
#include "snow.h"
#include "log.h"
#include "mathlib.h"
#include "net_radiation.h"
#include "pet.h"
#include "difrad.h"
#include "raddist.h"
#include "svat.h"
#include "vegetation.h"
#include "time.h"
#include "units.h"
#include "check.h"
#include "fao.h"
#include "librarian.h"
#include "treelog.h"
#include <sstream>

struct BioclimateStandard : public Bioclimate
{ 
  // Canopy State.
  const long No;                // No of intervals in canopy discretization.
  double LAI_;                  // Total LAI of all crops on this column. [0-]
  double cover_;                // Fraction of soil covered by vegetation.
  double sun_LAI_fraction_total_;// Total sun LAI fraction of all crops on this column.
  std::vector<double> Height;   // Height in cm of each endpoint in c.d.
  std::vector<double> total_PAR_;// Total PAR of each interval of c.d. [W/m2]
  std::vector<double> sun_PAR_; //Sunlit fraction of PAR of each interval of c.d.[W/m2]
  std::vector<double> total_NIR_;// Total NIR of each interval of c.d. [W/m2]
  std::vector<double> sun_NIR_; //Sunlit fraction of NIR of each interval of c.d[W/m2].
  std::vector<double> sun_LAI_fraction_;//Fraction of sunlit LAI of each interval of cd
  double shared_light_fraction_; // Fraction of field with light competition.

  // Canopy Tick.
  void CanopyStructure (const Vegetation&);

  // External water sinks and sources.
  std::auto_ptr<NetRadiation> net_radiation;
  std::auto_ptr<Pet> pet;       // Potential Evapotranspiration model.
  double total_ep_;             // Potential evapotranspiration [mm/h]
  double total_ea_;             // Actual evapotranspiration [mm/h]
  double direct_rain_;          // Rain hitting soil directly [mm/h]
  double irrigation_overhead;   // Irrigation above canopy [mm/h]
  double irrigation_overhead_old;       // Old value for logging.
  double irrigation_overhead_temperature; // Water temperature [dg C]
  double irrigation_surface;    // Irrigation below canopy [mm/h]
  double irrigation_surface_old; // Old value for logging.
  double irrigation_surface_temperature; // Water temperature [dg C]
  double irrigation_subsoil;    // Irrigation incorporated in soil.
  double irrigation_subsoil_old; // Old value for logging.
  double irrigation_subsoil_permanent;  // Irrigation incorporated in soil.

  // Water in snowpack.
  Snow snow;
  double snow_ep;               // Potential snow evaporation [mm/h]
  double snow_ea_;              // Actual snow evaporation [mm/h]
  double snow_water_in;         // Water entering snow pack [mm/h]
  double snow_water_in_temperature; // Incoming water temperature [dg C]
  double snow_water_out;        // Water leaving snow pack [mm/h]
  double snow_water_out_temperature; // Temperature of water leaving [dg C]

  // Water intercepted on canopy.
  double canopy_ep;             // Potential canopy evaporation [mm/h]
  double canopy_ea_;            // Actual canopy evaporation [mm/h]
  double canopy_water_storage;  // Intercepted water on canopy [mm]
  double canopy_water_temperature; // Temperature of incoming water [dg C]
  double canopy_water_in;       // Water entering canopy [mm/h]
  double canopy_water_out;      // Canopy drip throughfall [mm/h]
  double canopy_water_bypass;   // Water from above bypassing the canopy [mm/h]
  
  // Water intercepted on litter.
  double litter_ep;             // Potential litter evaporation [mm/h]
  double litter_ea;             // Actual litter evaporation [mm/h]
  double litter_water_storage;  // Intercepted water in litter [mm]
  double litter_water_temperature; // Temperature of water in litter [dg C]
  double litter_water_in;       // Water entering litter [mm/h]
  double litter_water_out;      // Water leaving litter [mm/h]

  // Water in pond.
  double pond_ep;               // Potential evaporation from pond [mm/h]
  double pond_ea_;              // Actual evaporation from pond [mm/h]

  // Water going through soil surface.
  double soil_ep;               // Potential exfiltration. [mm/h]
  double soil_ea_;              // Actual exfiltration. [mm/h]

  // Water transpirated through plant roots.
  const int max_svat_iterations; // Max number of iterations with SVAT model.
  const double max_svat_absolute_difference; // Max difference. [mm/h]
  std::auto_ptr<SVAT> svat;     // Soil Vegetation Atmosphere model.
  double crop_ep_;              // Potential transpiration. [mm/h]
  double crop_ea_soil;          // Crop limited transpiration. [mm/h]
  double crop_ea_svat;          // SVAT limited transpiration. [mm/h]
  double crop_ea_;              // Actual transpiration. [mm/h]
  double production_stress;     // Stress calculated by SVAT module.

  // Bioclimate canopy
  double CanopyTemperature;     // Canopy temperature
  double SunLeafTemperature;    // Sunlit leaf temperature
  double ShadowLeafTemperature; // Shadow leaf temperature
  double wind_speed_field_;     // wind speed at screen height above the canopy [m/s]
  double wind_speed_weather;    // measured wind speed [m/s]

  void WaterDistribution (const Units&, const Time&,
                          Surface& surface, const Weather& weather, 
                          Vegetation& vegetation, const Movement&,
                          const Geometry&, const Soil& soil,
                          const SoilWater& soil_water, const SoilHeat&, 
                          double dt, Treelog&);

  // Radiation.
  static double albedo (const Vegetation& crops, const Surface& surface, 
                        const Geometry&, const Soil&, const SoilWater&);
  std::auto_ptr<Raddist> raddist;// Radiation distribution model.
  const double min_sin_beta_;     // Sinus to lowest sun angle for some models.
  void RadiationDistribution (const Vegetation&, double sin_beta, Treelog&);
  std::auto_ptr<Difrad> difrad;  // Diffuse radiation model.
  double difrad0;                // Diffuse radiation above canopy [W/m2]

  double absorbed_total_PAR_canopy; // Canopy absorbed PAR (sun+shade) [W/m2]
  double absorbed_total_NIR_canopy; // Canopy absorbed NIR (sun+shade) [W/m2]
  double absorbed_total_Long_canopy;// Canopy absorbed Long wave radiation (sun+shade)[W/m2]
  double absorbed_total_PAR_soil;   // Soil absorbed PAR [W/m2]
  double absorbed_total_NIR_soil;   // Soil absorbed NIR [W/m2]
  double absorbed_total_Long_soil;  // Soil absorbed Long wave radiation [W/m2]
  double absorbed_sun_PAR_canopy;   // Canopy absorbed PAR on sunlit leaves [W/m2]
  double absorbed_sun_NIR_canopy;   // Canopy absorbed NIR on sunlit leaves [W/m2]
  double absorbed_sun_Long_canopy;  // Canopy absorbed Long wave radiatio on sunlit leaves [W/m2]
  double absorbed_shadow_PAR_canopy;// Canopy absorbed PAR on shadow leaves [W/m2]
  double absorbed_shadow_NIR_canopy;// Canopy absorbed NIR on shadow leaves [W/m2]
  double absorbed_shadow_Long_canopy;// Canopy absorbed Long wave radiation on shadow leaves [W/m2]
  double rad_abs_soil_;             // Soil absorbed radiation [W/m2]
  double rad_abs_sun_canopy_;       // Canopy absorbed radiatio on sunlit leaves [W/m2]
  double rad_abs_shadow_canopy_;    // Canopy absorbed radiation on shadow leaves [W/m2]
  double incoming_Long_radiation;  // Incoming longwave radiation [W/m2] 
  double incoming_PAR_radiation;   // Incoming PAR radiation [W/m2]
  double incoming_NIR_radiation;   // Incoming NIR radiation [W/m2]
  double incoming_Total_radiation; // Incoming radiation sum of shortwave and 
                                   // longwave [W/m2]

  double sin_beta_;

  // Weather.
  double daily_air_temperature_; // Air temperature in canopy [dg C]
  double daily_precipitation_; // From weather [mm]
  double day_length_;           // From weather (does not belong here) [h]
  double daily_global_radiation_; // From weather [W/m2]
  double global_radiation_; // From weather [W/m2]
  double atmospheric_CO2_;         // From weather [Pa]
  double atmospheric_relative_humidity_; // From weather []

  // Simulation
  void tick (const Units&, const Time&, Surface&, const Weather&, 
             Vegetation&, const Movement&, const Geometry&,
             const Soil&, SoilWater&, const SoilHeat&, Chemistry&, 
             double dt, Treelog&);
  void output (Log&) const;

  // Canopy.
  const std::vector<double>& height () const
  { return Height; }
  const std::vector<double>& PAR () const
  { return total_PAR_; }
  const std::vector<double>& sun_PAR () const
  { return sun_PAR_; }
  const std::vector<double>& NIR () const
  { return total_NIR_; }
  const std::vector<double>& sun_NIR () const
  { return sun_NIR_; }
  const std::vector<double>& sun_LAI_fraction () const
  { return sun_LAI_fraction_; }
  double LAI () const
  { return LAI_; }
  double cover () const
  { return cover_; }
  double sun_LAI_fraction_total () const
  { return sun_LAI_fraction_total_; }
  double wind_speed_field () const 
  { return wind_speed_field_; }
  double rad_abs_soil() const 
  { return rad_abs_soil_; }
  double rad_abs_sun_canopy() const
  { return rad_abs_sun_canopy_; }
  double rad_abs_shadow_canopy() const 
  { return rad_abs_shadow_canopy_; }
  double sin_beta() const 
  { return sin_beta_; }
  double shared_light_fraction () const
  { return shared_light_fraction_; }

  // Weather.
  double daily_air_temperature () const
  { return daily_air_temperature_; }
  double canopy_temperature () const
  { return CanopyTemperature; }
  double sun_leaf_temperature () const
  { return SunLeafTemperature; }
  double shadow_leaf_temperature () const
  { return ShadowLeafTemperature; }
  double daily_precipitation () const
  { return daily_precipitation_; }
  double day_length () const
  { return day_length_; }
  double daily_global_radiation () const
  { return daily_global_radiation_; }
  double global_radiation () const
  { return global_radiation_; }
  double direct_rain () const
  { return direct_rain_; }
  double atmospheric_CO2 () const
  { return atmospheric_CO2_; }
  double atmospheric_relative_humidity () const
  { return atmospheric_relative_humidity_; }

  // Manager.
  void irrigate_overhead (double flux, double temp);
  void irrigate_surface (double flux, double temp);
  void irrigate_overhead (double flux);
  void irrigate_surface (double flux);
  void irrigate_subsoil (double flux);
  void set_subsoil_irrigation (double flux);

  // Communication with svat and external model.
  double total_ep () const // [mm/h]
  { return total_ep_; }
  double total_ea () const // [mm/h]
  { return total_ea_; }
  double snow_ea () const // [mm/h]
  { return snow_ea_; }
  double pond_ea () const // [mm/h]
  { return pond_ea_; }
  double soil_ea () const // [mm/h]
  { return soil_ea_; }
  double soil_surface_ea () const // [mm/h]
  { return soil_ea_ + litter_ea + pond_ea_ + snow_ea_; } 
  double crop_ep () const // [mm/h]
  { return crop_ep_; }
  double crop_ea () const // [mm/h]
  { return crop_ea_; }
  double canopy_ea () const // [mm/h]
  { return canopy_ea_; }
  double min_sin_beta () const // []
  { return min_sin_beta_; }
  double get_intercepted_water () const // [mm]
  { return canopy_water_storage; }
  double get_snow_storage () const // [mm]
  { return snow.storage (); }
  double snow_leak_rate (const double dt) const
  {
    const double snow_new = snow.storage ();
    if (snow_new < 1e-5)
      return 1.0 / dt;
    if (snow_water_out < 1e-8)
      return 0.0;
    const double snow_old = snow_new + snow_water_out * dt;
    
    return snow_water_out / snow_old;
  }
  double canopy_leak_rate (const double dt) const
  {
    if (canopy_water_storage < 1e-5)
      return 1.0 / dt;
    if (canopy_water_out < 1e-8)
      return 0.0;
    const double canopy_water_old
      = canopy_water_storage + canopy_water_out * dt;
    
    return canopy_water_out / canopy_water_old;
  }
  double canopy_leak () const                     // [mm/h]
  { return canopy_water_out; }

  // Create.
  void initialize (Block&, const Weather&);
  BioclimateStandard (Block&);
  void summarize (Treelog& msg) const;
  ~BioclimateStandard ();
};

void 
BioclimateStandard::initialize (Block& block, const Weather& weather)
{
  const Metalib& metalib = block.metalib ();
  Treelog& msg = block.msg ();
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
      pet.reset (Librarian::build_stock<Pet> (metalib, msg, type, "pet"));
    }

  if (!difrad.get ())                      // Explicit.
    {
      symbol type;

      if (weather.has_diffuse_radiation ())
        type = symbol ("weather");
      else
        type = symbol ("DPF");

      msg.debug ("Difrad choosen: " + type);
      difrad.reset (Librarian::build_stock<Difrad> (metalib, msg, type, 
                                                    "difrad"));
    }
}

BioclimateStandard::BioclimateStandard (Block& al)
  : Bioclimate (al),
    No (al.integer ("NoOfIntervals")),
    LAI_ (0.0),
    cover_ (0.0),
    sun_LAI_fraction_total_ (0.0),
    Height (No + 1),
    total_PAR_ (No + 1),
    sun_PAR_ (No + 1),
    total_NIR_ (No + 1),
    sun_NIR_ (No + 1),
    sun_LAI_fraction_ (No),
    shared_light_fraction_ (1.0),
    net_radiation (Librarian::build_item<NetRadiation> (al, "net_radiation")),
    pet (al.check ("pet") 
         ? Librarian::build_item<Pet> (al, "pet")
         : NULL),
    total_ep_ (0.0),
    total_ea_ (0.0),
    direct_rain_ (0.0),
    irrigation_overhead (0.0),
    irrigation_overhead_old (0.0),
    irrigation_overhead_temperature (0.0),
    irrigation_surface (0.0),
    irrigation_surface_old (0.0),
    irrigation_surface_temperature (0.0),
    irrigation_subsoil (0.0),
    irrigation_subsoil_old (0.0),
    irrigation_subsoil_permanent (al.number ("irrigation_subsoil_permanent")),
    snow (al.submodel ("Snow")),
    snow_ep (0.0),
    snow_ea_ (0.0),
    snow_water_in (0.0),
    snow_water_in_temperature (0.0),
    snow_water_out (0.0),
    snow_water_out_temperature (0.0),
    canopy_ep (0.0),
    canopy_ea_ (0.0),
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
    pond_ea_ (0.0),
    soil_ep (0.0),
    soil_ea_ (0.0),
    max_svat_iterations (al.integer ("max_svat_iterations")),
    max_svat_absolute_difference (al.number ("max_svat_absolute_difference")),
    svat (Librarian::build_item<SVAT> (al, "svat")),
    crop_ep_ (0.0),
    crop_ea_soil (0.0),
    crop_ea_svat (0.0),
    crop_ea_ (0.0),
    production_stress (-1.0),
    raddist (Librarian::build_item<Raddist> (al, "raddist")),
    min_sin_beta_ (std::sin (al.number ("min_sun_angle"))),
    difrad (al.check ("difrad") 
            ? Librarian::build_item<Difrad> (al, "difrad")
            : NULL),
    difrad0 (0.0),
    rad_abs_soil_ (0.0),
    rad_abs_sun_canopy_ (0.0),
    rad_abs_shadow_canopy_ (0.0),
    sin_beta_ (0.0),

    // BUG: These should really be part of the state, for checkpoints.
    daily_air_temperature_ (0.0),
    daily_precipitation_ (0.0),
    day_length_ (0.0),
    daily_global_radiation_ (0.0),
    global_radiation_ (0.0),
    atmospheric_CO2_ (-42.42e42),
    atmospheric_relative_humidity_ (-42.42e42)
{ }

void 
BioclimateStandard::summarize (Treelog& msg) const
{
  TREELOG_MODEL (msg);
  svat->summarize (msg);
}

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
  cover_ = vegetation.cover ();
  
  // Reset PAR intervals.
  std::fill (Height.begin (), Height.end (), 0.0);

  if (LAI_ > 0.0)
    {
      // Count height of each interval.  Interval 0 is the top of the crop
      // and interval "No" is group zero (no bomb intended).
      double dLAI = LAI_ / No;
      for (int i = 0; i <= No; i++)
        Height[i] = HvsLAI ((No - i) * dLAI);

      daisy_assert (iszero (Height[No]));
      //  daisy_assert (approximate (Height[0], MxH));
      Height[0] = vegetation.height ();
    }
}

double 
BioclimateStandard::albedo (const Vegetation& crops, const Surface& surface, 
                            const Geometry& geo,
                            const Soil& soil, const SoilWater& soil_water)
{
  const double litter_albedo = crops.litter_albedo ();
  const double surface_albedo = (litter_albedo < 0.0) 
    ? surface.albedo (geo, soil, soil_water)
    : litter_albedo;

  const double LAI = crops.LAI ();
  if (LAI <= 0.0)
    return surface_albedo;

  const double crop_cover = crops.cover ();
  return crops.albedo () * crop_cover
    + surface_albedo * (1.0 - crop_cover);
}

void 
BioclimateStandard::RadiationDistribution (const Vegetation& vegetation, 
                                           const double sin_beta_, Treelog& msg)
{
  TREELOG_MODEL (msg);

  raddist->tick(sun_LAI_fraction_, sun_PAR_, total_PAR_, sun_NIR_, total_NIR_,
                global_radiation (), difrad0, min_sin_beta_, sin_beta_, 
                vegetation, msg);

  //Absorbed PAR in the canopy and the soil:
  incoming_PAR_radiation = total_PAR_[0]; // [W/m2]
  absorbed_total_PAR_canopy = incoming_PAR_radiation - total_PAR_[No];     // [W/m2]
  absorbed_total_PAR_soil = incoming_PAR_radiation - absorbed_total_PAR_canopy;//[W/m2]
  absorbed_sun_PAR_canopy = sun_PAR_[0] - sun_PAR_[No];           // [W/m2]
  absorbed_shadow_PAR_canopy = absorbed_total_PAR_canopy - absorbed_sun_PAR_canopy; // [W/m2]

  //Absorbed NIR in the canopy and the soil:
  incoming_NIR_radiation = total_NIR_[0]; // [W/m2]
  absorbed_total_NIR_canopy = incoming_NIR_radiation - total_NIR_[No];
  absorbed_total_NIR_soil = incoming_NIR_radiation - absorbed_total_NIR_canopy;
  absorbed_sun_NIR_canopy = sun_NIR_[0] - sun_NIR_[No];
  absorbed_shadow_NIR_canopy = absorbed_total_NIR_canopy - absorbed_sun_NIR_canopy;

  //Absorbed Long wave radiation in the canopy and the soil:
  incoming_Long_radiation = net_radiation->incoming_longwave_radiation();

  sun_LAI_fraction_total_ = 0.0;
  for (int i = 0; i <= No - 1; i++)
    {
      sun_LAI_fraction_total_ += sun_LAI_fraction_[i] / No;
    }
  
  absorbed_total_Long_soil = incoming_Long_radiation * (1-cover ());
  absorbed_total_Long_canopy = incoming_Long_radiation * (cover ());
  absorbed_sun_Long_canopy = absorbed_total_Long_canopy * sun_LAI_fraction_total_;
  absorbed_shadow_Long_canopy = absorbed_total_Long_canopy - absorbed_sun_Long_canopy;

  // Variables for SVAT
  rad_abs_soil_ = absorbed_total_Long_soil + absorbed_total_NIR_soil
    + absorbed_total_PAR_soil;
  rad_abs_sun_canopy_ = absorbed_sun_Long_canopy + absorbed_sun_NIR_canopy
    + absorbed_sun_PAR_canopy;
  rad_abs_shadow_canopy_ =  absorbed_shadow_Long_canopy 
    + absorbed_shadow_NIR_canopy + absorbed_shadow_PAR_canopy;

  // Variable for log
  incoming_Total_radiation = incoming_Long_radiation + incoming_PAR_radiation 
    + incoming_NIR_radiation;    // [W/m2]
}

void
BioclimateStandard::WaterDistribution (const Units& units,
                                       const Time& time, Surface& surface,
                                       const Weather& weather, 
                                       Vegetation& vegetation,
                                       const Movement& movement,
                                       const Geometry& geo,
                                       const Soil& soil, 
                                       const SoilWater& soil_water,
                                       const SoilHeat& soil_heat, 
                                       const double dt, Treelog& msg)
{
  TREELOG_MODEL (msg);

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

  // Net Radiation.
  const double Cloudiness = weather.cloudiness ();
  const double AirTemperature = weather.air_temperature ();//[dg C]
  const double VaporPressure = weather.vapor_pressure ();
  const double Si = weather.global_radiation ();
  const double Albedo = albedo (vegetation, surface, geo, soil, soil_water);
  net_radiation->tick (Cloudiness, AirTemperature, VaporPressure, Si, Albedo,
                       msg);
  const double Rn = net_radiation->net_radiation ();

  // 1.1 Evapotranspiration

  daisy_assert (pet.get () != NULL);
  pet->tick (time, 
             weather, Rn, vegetation, surface, 
             geo, soil, soil_heat, soil_water, msg);
  total_ep_ = pet->wet ();
  daisy_assert (total_ep_ >= 0.0);
  total_ea_ = 0.0;              // To be calculated.

  // 1.2 Irrigation
  //
  // Already set by manager.

  // 1.3 Weather.
  double rain = weather.rain ();
  double air_temperature = weather.air_temperature ();

  // 2 Snow Pack

  snow_ep = total_ep_ - total_ea_;
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
             weather.global_radiation (), 0.0,
             snow_water_in, weather.snow (),
             surface.ponding (),
             snow_water_in_temperature, snow_ep, dt);
  snow_ea_ = snow.evaporation ();
  daisy_assert (snow_ea_ >= 0.0);
  total_ea_ += snow_ea_;
  daisy_assert (total_ea_ >= 0.0);
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
  
  daisy_assert (snow_ea_ <= total_ep_);
  canopy_ep = (total_ep_ - snow_ea_) * cover ();
  daisy_assert (canopy_ep >= 0.0);
  if (snow_water_out < 0.0)
    {
      canopy_water_in = 0.0;
      canopy_water_bypass = snow_water_out;
    }
  else
    {
      canopy_water_in = snow_water_out * cover ();
      canopy_water_bypass = snow_water_out - canopy_water_in;
    }
  daisy_assert (canopy_water_in >= 0.0);

  if (canopy_water_in > 0.01)
    canopy_water_temperature 
      = (canopy_water_storage * air_temperature 
         + canopy_water_in * snow_water_out_temperature * dt)
      / (canopy_water_storage + canopy_water_in * dt);
  else
    canopy_water_temperature = air_temperature;

  canopy_ea_ = std::min (canopy_ep, canopy_water_storage / dt + canopy_water_in);
  daisy_assert (canopy_ea_ >= 0.0);
  total_ea_ += canopy_ea_;
  daisy_assert (total_ea_ >= 0.0);
  
  canopy_water_storage += (canopy_water_in - canopy_ea_) * dt;
  if (canopy_water_storage < 0.0)
    {
      daisy_assert (canopy_water_storage > -1e-8);
      canopy_water_storage = 0.0;
    }
  
  if (canopy_water_storage > canopy_water_capacity + 1e-8)
    {
      canopy_water_out = (canopy_water_storage - canopy_water_capacity) / dt;
      canopy_water_storage = canopy_water_capacity;
    }
  else
    {
      canopy_water_out = 0.0;
    }
  daisy_assert (canopy_water_out >= 0.0);

  // 4 Water intercepted by litter

  litter_ep = (total_ep_ - snow_ea_) * (1.0 - cover ());
  daisy_assert (snow_ea_ <= total_ep_);
  if (litter_ep < 0.0)
    {
      std::ostringstream tmp;
      tmp << "BUG:\nlitter_ep = " << litter_ep << "\n"
          << "total_ep = " << total_ep_ << "\n"
          << "snow_ea_ = " << snow_ea_ << "\n"
          << "cover  = " << cover ();
      msg.error (tmp.str ());
      litter_ep = 0.0;
    }

  litter_water_in 
    = canopy_water_out + canopy_water_bypass + irrigation_surface;

  if (litter_water_in > 0.01)
    litter_water_temperature 
      = (litter_water_storage * air_temperature
         + canopy_water_bypass * snow_water_out_temperature * dt
         + canopy_water_out * canopy_water_temperature* dt
         + irrigation_surface * irrigation_surface_temperature * dt)
      / (litter_water_in * dt + litter_water_storage);
  else
    litter_water_temperature = air_temperature;

  daisy_assert (litter_water_storage >= 0.0);
  litter_ea = std::max (std::min (litter_ep,
                                  litter_water_storage / dt + litter_water_in),
                        0.0);
  total_ea_ += litter_ea;
  daisy_assert (total_ea_ >= 0.0);
  
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
      litter_water_out = litter_water_storage / dt;
      litter_water_storage = 0.0;
    }
  else if (litter_water_storage > litter_water_capacity + 1e-8)
    {
      litter_water_out = (litter_water_storage - litter_water_capacity) / dt;
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

  const double soil_T 
    = geo.content_hood (soil_heat, &SoilHeat::T, Geometry::cell_above);
  surface.tick (msg, pond_ep, 
                litter_water_out, litter_water_temperature, 
                geo, soil, soil_water, soil_T, dt);
  pond_ea_ = surface.evap_pond (dt, msg);
  daisy_assert (pond_ea_ >= 0.0);
  total_ea_ += pond_ea_;
  daisy_assert (total_ea_ >= 0.0);

  // 6 Soil

  soil_ep = pond_ep - pond_ea_;
  if (soil_ep < 0.0)
    {
      if (!approximate (pond_ep, pond_ea_))
        {
          std::ostringstream tmp;
          tmp << "BUG:\nsoil_ep = " << soil_ep << "\n"
              << "pond_ep = " << pond_ep << "\n"
              << "pond_ea = " << pond_ea_;
          msg.error (tmp.str ());
        }
      soil_ep = 0.0;
    }
  soil_ea_ = surface.exfiltration (dt);
  daisy_assert (soil_ea_ >= 0.0);
  total_ea_ += soil_ea_;
  daisy_assert (total_ea_ >= 0.0);

  // 7 Transpiration

  // Potential transpiration
  const double potential_crop_transpiration = canopy_ep - canopy_ea_;
  const double potential_soil_transpiration 
    = (pond_ep - pond_ea_ - soil_ea_) * vegetation.EpInterchange ();
  crop_ep_ = bound (0.0, 
                    potential_crop_transpiration + potential_soil_transpiration,
                    std::max (0.0, pet->dry ()));

  // Actual transpiration, based on remaining energy.
  crop_ea_
    = vegetation.transpiration (units, crop_ep_, canopy_ea_, geo, soil,
                                soil_water, dt, msg);
  daisy_assert (crop_ea_ >= 0.0);
  crop_ea_soil = crop_ea_;       // Remember original value.
  
  // Actual transpiration, modified by the SVAT model.

  // Let the SVAT model get the boundary conditions.
  svat->tick (weather, vegetation, geo, soil, soil_heat, soil_water, 
              *this, msg);

  // Our initial guess for transpiration is based on remaining energy.
  double crop_ea_svat_old = crop_ea_soil;
  
  for (int iteration = 0; iteration < max_svat_iterations; iteration++)
    {
      std::ostringstream tmp;
      tmp << "svat iteration " << iteration;
      Treelog::Open nest (msg, tmp.str ());

      // Find stomata conductance based on ABA and crown potential
      // from last attempt at crop transpiration.
      vegetation.find_stomata_conductance (units, time, *this, dt, msg);
      const double gs = vegetation.stomata_conductance ();

      // Find expected transpiration from stomate conductance.
      svat->solve (gs, msg);
      
      const double crop_ea_svat = svat->transpiration ();

      if (std::fabs (crop_ea_svat - crop_ea_svat_old) 
          < max_svat_absolute_difference)
        {
          // Stomate may limit transpiration, not increase it.
          //  daisy_assert (crop_ea_ < crop_ea_soil + 0.01);
          goto success;
        }
      crop_ea_svat_old = crop_ea_svat;

      // Calculate new crop transpiration based on latest SVAT guess.
      crop_ea_
        = vegetation.transpiration (units, crop_ea_svat, canopy_ea_, geo, soil,
                                    soil_water, dt, msg);
      daisy_assert (crop_ea_ >= 0.0);
    }
  msg.error ("SVAT transpiration and stomata conductance"
             " loop did not converge");
 success:;

  // Stress calculated by the SVAT model.
  production_stress = svat->production_stress ();
  vegetation.force_production_stress (production_stress);

  // Total evapotranspiration.
  total_ea_ += crop_ea_;
  daisy_assert (total_ea_ >= 0.0);

  // Direct rain, used for colloid generation
  if (snow_water_out < 0.01)
    direct_rain_ = 0.0;
  else
    // We want to ignore irrigation and melting snow here.
    direct_rain_ = canopy_water_bypass * (rain / snow_water_out);

  // Reset irrigation
  irrigation_overhead_old = irrigation_overhead;
  irrigation_overhead = 0.0;
  irrigation_surface_old = irrigation_surface;
  irrigation_surface = 0.0;
  irrigation_subsoil_old = irrigation_subsoil;
  irrigation_subsoil = 0.0;
  
  // Check
  // Note: total_ea can be larger than total_ep, as PMSW uses a
  // different method for calculating PET.
  daisy_assert (approximate (total_ea_,
                             snow_ea_ + canopy_ea_ + litter_ea
                             + pond_ea_ + soil_ea_ + crop_ea_));
}  

void 
BioclimateStandard::tick (const Units& units, const Time& time, 
                          Surface& surface, const Weather& weather,  
                          Vegetation& vegetation, const Movement& movement,
                          const Geometry& geo, const Soil& soil, 
                          SoilWater& soil_water, const SoilHeat& soil_heat,
                          Chemistry& chemistry,
                          const double dt, Treelog& msg)
{
  // Keep weather information during time step.
  // Remember this in case the crops should ask.
  global_radiation_ = weather.global_radiation ();
  daily_global_radiation_ = weather.daily_global_radiation ();
  daily_air_temperature_ = weather.daily_air_temperature ();
  daily_precipitation_ = weather.daily_precipitation ();
  day_length_ = weather.day_length ();
  atmospheric_CO2_ = weather.CO2 ();
  atmospheric_relative_humidity_ = weather.relative_humidity ();

  // Add deposition. 
  const Unit& u_h = units.get_unit (Units::h ());
  const Unit& u_storage = units.get_unit (IM::storage_unit ());
  const IM im = weather.deposit ().multiply (Scalar (dt, u_h), u_storage);
  chemistry.deposit (im, dt, msg);

  // Update canopy structure.
  CanopyStructure (vegetation);
 
  // Radiation.
  daisy_assert (difrad.get () != NULL);
  difrad0 = difrad->value (time, weather, msg) * global_radiation_;
  //daisy_assert (difrad0 >= 0.0);

  sin_beta_ = weather.sin_solar_elevation_angle (time);
  // Calculate total canopy, divide it into intervals, and distribute PAR.
  RadiationDistribution (vegetation, sin_beta_, msg);

  // Distribute water among canopy, snow, and soil.
  WaterDistribution (units, time, surface, weather, vegetation, 
                     movement, geo, soil, soil_water, soil_heat, dt, msg);

#if 0
  // Calculate temperature of canopy
  //  static const double rho_water = 1.0; // [kg/dm^3]
  // const double AirTemperature = weather.air_temperature ();//[dg C]
  // const double LatentHeatVapor 
  //  = FAO::LatentHeatVaporization (AirTemperature); //[J/kg]
  const double CanopyTranspirationRate = 
    crop_ea_ /*[mm/h]*/* rho_water * LatentHeatVapor / 3600. /*[s/h]*/; //[W/m^2] 

  const double CanopyNetRadiation = net_radiation->net_radiation () 
    * cover ();
  const double SensibleHeatFlux 
    = CanopyNetRadiation - CanopyTranspirationRate;//[W/m^2] 
  
  // const double AirPressure 
  //  = FAO::AtmosphericPressure (weather.elevation ());//[Pa]
  const double rho_a = FAO::AirDensity(AirPressure, AirTemperature);//[kg/m3]
  const double gamma
    = FAO::PsychrometricConstant (AirPressure, AirTemperature);//[Pa/dgC]
  const double epsilon 
    = 0.622; // Ration molecular weight of water vapor / dry air []
  const double c_p 
    = gamma * epsilon * LatentHeatVapor / AirPressure;//[J/kg/dg C]
#endif 

  // Convert wind speed to field conditions.
  const double ScreenHeight = weather.screen_height (); //[m]

  wind_speed_weather =  weather.wind ();//[m/s]

  const double h = Height[0]/100.0; // [m]
  const double h0 = 0.12; // reference height [m]

  if (weather.surface () == Weather::field || h < h0)
    wind_speed_field_ = wind_speed_weather;
  else
    {
      const double u = wind_speed_weather; // wind speed at reference height [m/s]
      const double k = 0.41;  // von Karman constant
      const double d0 = 0.64 * h0; // displacement height (reference) [m]
      const double z0 = 0.13 * h0; // roughness length [m]
      const double u_star = u * k /(log((ScreenHeight - d0)/z0));
      daisy_assert (u_star >= 0.0);
      const double d = 0.64 * h;
      const double z = 0.13 * h;
      // Same height over vegetation.
      const double ScreenHeight1 = ScreenHeight + h - h0;
      wind_speed_field_ = (u_star / k) * log((ScreenHeight1 - d)/z);
    }
  daisy_assert (wind_speed_field_ >= 0.0);

  CanopyTemperature = svat->CanopyTemperature();  //[dg C]
  SunLeafTemperature = svat->SunLeafTemperature();  //[dg C]
  ShadowLeafTemperature = svat->ShadowLeafTemperature();  //[dg C]
}

void 
BioclimateStandard::output (Log& log) const
{
  output_variable (Height, log);
  output_derived (net_radiation, "net_radiation", log);
  output_derived (raddist, "raddist", log);
  daisy_assert (pet.get () != NULL);
  output_object (pet.get (), "pet", log);
  output_value (total_ep_, "total_ep", log);
  output_value (total_ea_, "total_ea", log);
  output_value (direct_rain_, "direct_rain", log);
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
  output_value (snow_ea_, "snow_ea", log);
  output_variable (snow_water_in, log);
  output_variable (snow_water_in_temperature, log);
  output_variable (snow_water_out, log);
  output_value (snow_water_out_temperature, 
                "snow_water_out_temperature", log);
  output_variable (canopy_ep, log);
  output_value (canopy_ea_, "canopy_ea", log);
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
  output_value (pond_ea_, "pond_ea", log);
  output_variable (soil_ep, log);
  output_value (soil_ea_, "soil_ea", log);
  output_derived (svat, "svat", log);
  output_value (crop_ep_, "crop_ep", log);
  output_variable (crop_ea_soil, log);
  output_variable (crop_ea_svat, log);
  output_value (crop_ea_, "crop_ea", log);
  output_variable (production_stress, log);

  output_variable (CanopyTemperature, log);
  output_variable (SunLeafTemperature, log);
  output_variable (ShadowLeafTemperature, log);
  output_value (wind_speed_field_, "wind_speed_field", log);  
  output_variable (wind_speed_weather, log);

  //radiation
  daisy_assert (difrad.get () != NULL);
  output_object (difrad.get (), "difrad", log);
  output_variable (difrad0, log);
  output_value (total_PAR_, "total_PAR", log);
  output_value (sun_PAR_, "sun_PAR", log);
  output_value (total_NIR_, "total_NIR", log);
  output_value (sun_NIR_, "sun_NIR", log);
  output_value (sun_LAI_fraction_, "sun_LAI_fraction", log);
  output_variable (absorbed_total_PAR_canopy, log);
  output_variable (absorbed_total_NIR_canopy, log);
  output_variable (absorbed_total_Long_canopy, log);
  output_variable (absorbed_total_PAR_soil, log);
  output_variable (absorbed_total_NIR_soil, log);
  output_variable (absorbed_total_Long_soil, log);
  output_variable (absorbed_sun_PAR_canopy, log);
  output_variable (absorbed_sun_NIR_canopy, log);
  output_variable (absorbed_sun_Long_canopy, log);
  output_variable (absorbed_shadow_PAR_canopy, log);
  output_variable (absorbed_shadow_NIR_canopy, log);
  output_variable (absorbed_shadow_Long_canopy, log);
  output_variable (incoming_Long_radiation, log);
  output_variable (incoming_PAR_radiation, log);
  output_variable (incoming_NIR_radiation, log);
  output_variable (incoming_Total_radiation, log);
}

void
BioclimateStandard::irrigate_overhead (double flux, double temp)
{
  double new_top = irrigation_overhead + flux;

  irrigation_overhead_temperature 
    = (new_top > 0.01)
    ? (temp * flux
       + irrigation_overhead * irrigation_overhead_temperature) / new_top
    : daily_air_temperature ();
  irrigation_overhead = new_top;
}

void
BioclimateStandard::irrigate_surface (double flux, double temp)
{
  double new_surface = irrigation_surface + flux;
  irrigation_surface_temperature 
    = (new_surface > 0.01) 
    ? (temp * flux
       + irrigation_surface * irrigation_surface_temperature) / new_surface
    : daily_air_temperature ();
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

static struct BioclimateStandardSyntax : DeclareModel
{
  Model* make (Block& al) const
  { return new BioclimateStandard (al); }
  
  BioclimateStandardSyntax ()
    : DeclareModel (Bioclimate::component, "default", "\
The default bioclimate model.")
  { }
  void load_frame (Frame& frame) const
  {
    // Canopy structure.
    frame.declare_integer ("NoOfIntervals", Attribute::Const, "\
Number of vertical intervals in which we partition the canopy.");
    frame.set ("NoOfIntervals", 30);
    frame.declare ("Height", "cm", Attribute::LogOnly, Attribute::CanopyEdges, "\
End points of canopy layers, first entry is top of canopy, last is soil surface.");
    // External water sources and sinks.
    frame.declare_object ("net_radiation", NetRadiation::component,
                          "Net radiation.");
    frame.set ("net_radiation", "brunt");
    frame.declare_object ("pet", Pet::component, 
                          Attribute::OptionalState, Attribute::Singleton, 
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
    frame.declare ("total_ep", "mm/h", Attribute::LogOnly,
                   "Potential evapotranspiration.");
    frame.declare ("total_ea", "mm/h", Attribute::LogOnly,
                   "Actual evapotranspiration.");
    frame.declare ("direct_rain", "mm/h", Attribute::LogOnly,
                   "Rain hitting surface directly.\n\
This includes rain hitting ponded water or litter, but excludes rain\n\
hitting canopy or snow, as well as snow and all forms for irrigation.\n\
The intended use is colloid generation."); 
    frame.declare ("irrigation_overhead", "mm/h", Attribute::LogOnly,
                   "Irrigation above canopy.");
    frame.declare ("irrigation_overhead_temperature", "dg C", Attribute::LogOnly,
                   "Water temperature.");
    frame.declare ("irrigation_surface", "mm/h", Attribute::LogOnly,
                   "Irrigation below canopy.");
    frame.declare ("irrigation_surface_temperature", "dg C", Attribute::LogOnly,
                   "Water temperature.");
    frame.declare ("irrigation_subsoil", "mm/h", Attribute::LogOnly,
                   "Irrigation below soil surface this hour.");
    frame.declare ("irrigation_subsoil_permanent", "mm/h", Attribute::State,
                   "Long term irrigation below soil surface.");
    frame.set ("irrigation_subsoil_permanent", 0.0);
    frame.declare ("irrigation_total", "mm/h", Attribute::LogOnly,
                   "Total irrigation above of below the soil surface.");

    // Water in snowpack.
    frame.declare_submodule ("Snow", Attribute::State, 
                             "Surface snow pack.",
                             Snow::load_syntax);
    frame.declare ("snow_ep", "mm/h", Attribute::LogOnly,
                   "Potential snow evaporation.");
    frame.declare ("snow_ea", "mm/h", Attribute::LogOnly,
                   "Actual snow evaporation.");
    frame.declare ("snow_water_in", "mm/h", Attribute::LogOnly,
                   "Water entering snow pack.");
    frame.declare ("snow_water_in_temperature", "dg C", Attribute::LogOnly,
                   "Temperature of water entering snow pack.");
    frame.declare ("snow_water_out", "mm/h", Attribute::LogOnly,
                   "Water leaving snow pack");
    frame.declare ("snow_water_out_temperature", "dg C", Attribute::LogOnly,
                   "Temperature of water leaving snow pack.");

    // Water intercepted on canopy.
    frame.declare ("canopy_ep", "mm/h", Attribute::LogOnly,
                   "Potential canopy evaporation.");
    frame.declare ("canopy_ea", "mm/h", Attribute::LogOnly,
                   "Actual canopy evaporation.");
    frame.declare ("canopy_water_storage", "mm", Attribute::State,
                   "Intercepted water on canopy.");
    frame.set ("canopy_water_storage", 0.0);
    frame.declare ("canopy_water_temperature", "dg C", Attribute::LogOnly,
                   "Temperature of incoming water.");
    frame.declare ("canopy_water_in", "mm/h", Attribute::LogOnly,
                   "Water entering canopy.");
    frame.declare ("canopy_water_out", "mm/h", Attribute::LogOnly,
                   "Canopy drip throughfall.");
    frame.declare ("canopy_water_bypass", "mm/h", Attribute::LogOnly,
                   "Water from above bypassing the canopy.");

    // Water intercepted by litter.
    frame.declare ("litter_ep", "mm/h", Attribute::LogOnly,
                   "Potential evaporation litter.");
    frame.declare ("litter_ea", "mm/h", Attribute::LogOnly,
                   "Actual litter evaporation.");
    frame.declare ("litter_water_storage", "mm", Attribute::State,
                   "Intercepted water on litter.");
    frame.set ("litter_water_storage", 0.0);
    frame.declare ("litter_water_temperature", "dg C", Attribute::LogOnly,
                   "Temperature of incoming water.");
    frame.declare ("litter_water_in", "mm/h", Attribute::LogOnly,
                   "Water entering litter.");
    frame.declare ("litter_water_out", "mm/h", Attribute::LogOnly,
                   "Litter drip throughfall.");

    // Water in pond.
    frame.declare ("pond_ep", "mm/h", Attribute::LogOnly,
                   "Potential evaporation from pond.");
    frame.declare ("pond_ea", "mm/h", Attribute::LogOnly,
                   "Actual evaporation from pond.");

    // Water going through soil surface.
    frame.declare_integer ("max_svat_iterations", Attribute::Const, "\
Max number of svat iterations before giving up on cobvergence.");
    frame.set ("max_svat_iterations", 100);  
    frame.declare ("max_svat_absolute_difference", "mm/h", Attribute::Const, "\
Maximum absolute difference in svat ea values for convergence.");
    frame.set ("max_svat_absolute_difference", 0.01);

    frame.declare_object ("svat", SVAT::component, 
                          "Soil Vegetation Atmosphere component.");
    frame.set ("svat", "none");
    frame.declare ("soil_ep", "mm/h", Attribute::LogOnly,
                   "Potential exfiltration.");
    frame.declare ("soil_ea", "mm/h", Attribute::LogOnly,
                   "Actual exfiltration.");

    // Water transpirated through plant roots.
    frame.declare ("crop_ep", "mm/h", Attribute::LogOnly,
                   "Potential transpiration.\n\
Transpiration under the assumption that the soil have an unlimited\n\
water supply.  For a fully irrigated crop, this will be equal to the\n\
actual transpiration.");
    frame.declare ("crop_ea_soil", "mm/h", Attribute::LogOnly,
                   "Soil limited transpiration.\n\
The part of the potential transpiration that the soil can supply.");
    frame.declare ("crop_ea_svat", "mm/h", Attribute::LogOnly,
                   "Transpiration suggested by the SVAT module.\n\
Under stressed conditions, the soil, vegetation and atmosphere behave\n\
different than what was assumed when calculating the poterntial\n\
transpiration.");
    frame.declare ("crop_ea", "mm/h", Attribute::LogOnly,
                   "Actual transpiration.\n\
This is the transpiration limited either by what the soil can deliver, or\n\
what the SVAT module requires.");
    frame.declare ("production_stress", Attribute::None (), Attribute::LogOnly,
                   "SVAT module induced stress, -1 means use water stress.");

    // Bioclimate in canopy
    frame.declare ("CanopyTemperature", "dg C", Attribute::LogOnly,
                   "Actual canopy temperature.");
    frame.declare ("SunLeafTemperature", "dg C", Attribute::LogOnly,
                   "Sunlit leaf temperature.");
    frame.declare ("ShadowLeafTemperature", "dg C", Attribute::LogOnly,
                   "Shadow leaf temperature.");
    frame.declare ("wind_speed_field", "m/s", Attribute::LogOnly,
                   "Wind speed in the field at reference height.");
    frame.declare ("wind_speed_weather", "m/s", Attribute::LogOnly,
                   "Measured wind speed.");

    //Radiation
    frame.declare ("min_sun_angle", "rad", Attribute::Const, "\
Minimum sun angle above ground for some 'raddist' and 'svat' models.\n\
\n\
The 'DPF' raddist model will zero radiation if the angle is below this,\n\
and the 'SSOC' svat model will revert to a one leaf description.");
    frame.set ("min_sun_angle", 3.6 * M_PI / 180.0);
    frame.declare_object ("raddist", Raddist::component, 
                          "Radiation distribution model.");
    frame.set ("raddist", "default");
    frame.declare_object ("difrad", Difrad::component, 
                          Attribute::OptionalState, Attribute::Singleton, 
                          "Diffuse radiation component.\n\
\n\
By default, choose depending on available climate date.\n\
\n\
If diffuse radiation is available in the climate data, Daisy will\n\
use these (the weather difrad model). Otherwise Daisy wil use the DPF model.");
    frame.declare ("difrad0", "W/m^2", Attribute::LogOnly,
                   "Diffuse radiation above canopy.");
    frame.declare ("total_PAR", "W/m^2", Attribute::LogOnly, Attribute::CanopyEdges,
                   "Total PAR between canopy layers.");
    frame.declare ("sun_PAR", "W/m^2", Attribute::LogOnly, Attribute::CanopyEdges,
                   "Sun PAR between canopy layers.");
    frame.declare ("total_NIR", "W/m^2", Attribute::LogOnly, Attribute::CanopyEdges,
                   "Total NIR between canopy layers.");
    frame.declare ("sun_NIR", "W/m^2", Attribute::LogOnly, Attribute::CanopyEdges,
                   "Sun NIR between canopy layers.");
    frame.declare ("sun_LAI_fraction", Attribute::Fraction (), Attribute::LogOnly, 
                   Attribute::CanopyCells, "Sunlit LAI in canopy layers.");

    frame.declare ("absorbed_total_PAR_canopy","W/m2", Attribute::LogOnly,
                   "Canopy absorbed PAR (sun+shade)");
    frame.declare ("absorbed_total_NIR_canopy","W/m2", Attribute::LogOnly,
                   "Canopy absorbed NIR (sun+shade)");
    frame.declare ("absorbed_total_Long_canopy","W/m2", Attribute::LogOnly,
                   "Canopy absorbed long wave radiation (sun+shade)");
    frame.declare ("absorbed_total_PAR_soil","W/m2", Attribute::LogOnly,
                   "Soil absorbed PAR (sun+shade)");
    frame.declare ("absorbed_total_NIR_soil","W/m2", Attribute::LogOnly,
                   "Soil absorbed NIR (sun+shade)");
    frame.declare ("absorbed_total_Long_soil","W/m2", Attribute::LogOnly,
                   "Soil absorbed long wave radiation (sun+shade)");
    frame.declare ("absorbed_sun_PAR_canopy","W/m2", Attribute::LogOnly,
                   "Canopy absorbed PAR on sunlit leaves");
    frame.declare ("absorbed_sun_NIR_canopy","W/m2", Attribute::LogOnly,
                   "Canopy absorbed NIR on sunlit leaves");
    frame.declare ("absorbed_sun_Long_canopy","W/m2", Attribute::LogOnly,
                   "Canopy absorbed long wave radiatio on sunlit leaves");
    frame.declare ("absorbed_shadow_PAR_canopy","W/m2", Attribute::LogOnly,
                   "Canopy absorbed PAR on shadow leaves");
    frame.declare ("absorbed_shadow_NIR_canopy","W/m2", Attribute::LogOnly,
                   "Canopy absorbed NIR on shadow leaves");
    frame.declare ("absorbed_shadow_Long_canopy","W/m2", Attribute::LogOnly,
                   "Canopy absorbed long wave radiation on shadow leaves");
    frame.declare ("incoming_Long_radiation","W/m2", Attribute::LogOnly,
                   "Incoming longwave radiation");
    frame.declare ("incoming_PAR_radiation","W/m2", Attribute::LogOnly,
                   "Incoming PAR radiation");
    frame.declare ("incoming_NIR_radiation","W/m2", Attribute::LogOnly,
                   "Incoming NIR radiation");
    frame.declare ("incoming_Total_radiation","W/m2", Attribute::LogOnly,
                   "Incoming radiation, sum of shortwave and longwave");
  }
} BioclimateStandard_syntax;

// bioclimate_std.C ends here
