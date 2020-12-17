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
#include "block_model.h"
#include "surface.h"
#include "weather.h"
#include "plf.h"
#include "frame.h"
#include "geometry.h"
#include "soil.h"
#include "soil_heat.h"
#include "snow.h"
#include "log.h"
#include "mathlib.h"
#include "net_radiation.h"
#include "pet.h"
#include "difrad.h"
#include "raddist.h"
#include "deposition.h"
#include "svat.h"
#include "vegetation.h"
#include "litter.h"
#include "time.h"
#include "check.h"
#include "fao.h"
#include "librarian.h"
#include "treelog_store.h"
#include "resistance.h"
#include "im.h"
#include "soil_water.h"
#include <sstream>

struct BioclimateStandard : public Bioclimate
{ 
  const Metalib& metalib;

  static const double reference_height; // [m]

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
  const std::unique_ptr<NetRadiation> net_radiation;
  std::unique_ptr<Pet> pet;       // Potential Evapotranspiration model.
  double total_ep_;             // Potential evapotranspiration [mm/h]
  double total_ea_;             // Actual evapotranspiration [mm/h]
  double direct_rain_;          // Rain hitting soil directly [mm/h]
  double irrigation_overhead;   // Irrigation above canopy [mm/h]
  double irrigation_overhead_temperature; // Water temperature [dg C]
  double irrigation_surface;    // Irrigation below canopy [mm/h]
  double irrigation_surface_temperature; // Water temperature [dg C]
  double irrigation_subsoil;    // Irrigation incorporated in soil.
  double irrigation_subsoil_permanent;  // Irrigation incorporated in soil.
  double tillage_water;    // Water added to surface due to tillage [mm/h]

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
  double canopy_water_capacity;  // Pot. intercepted water on canopy [mm]
  double canopy_water_storage;  // Intercepted water on canopy [mm]
  double canopy_water_temperature; // Temperature of incoming water [dg C]
  double canopy_water_in;       // Water entering canopy [mm/h]
  double canopy_water_out;      // Canopy drip throughfall [mm/h]
  double canopy_water_bypass;   // Water from above bypassing the canopy [mm/h]
  double canopy_water_below;    // Total water input below canopy [mm/h]

  // Water intercepted on litter.
  double litter_ep;             // Potential litter evaporation [mm/h]
  double litter_ea;             // Actual litter evaporation [mm/h]
  double litter_water_capacity;  // Pot. intercepted water in litter [mm]
  double litter_water_storage;  // Intercepted water in litter [mm]
  double litter_water_temperature; // Temperature of water in litter [dg C]
  double litter_water_in;       // Water entering litter [mm/h]
  double litter_water_out;      // Water leaving litter [mm/h]
  double litter_wash_off; 	// Water hitting but not entering litter [mm/h]

  // Water in pond.
  double pond_ep;               // Potential evaporation from pond [mm/h]
  double pond_ea_;              // Actual evaporation from pond [mm/h]

  // Water going through soil surface.
  double soil_ep;               // Potential exfiltration. [mm/h]
  double soil_ea_;              // Actual exfiltration. [mm/h]

  // Water transpirated through plant roots.
  const int max_svat_iterations; // Max number of iterations with SVAT model.
  const double max_svat_absolute_difference; // Max difference. [mm/h]
  const double maxTdiff;                     // Max temperature diff. [dg C]
  const double maxEdiff;                     // Max pressure diff [Pa]
  const std::unique_ptr<SVAT> svat;     // Soil Vegetation Atmosphere model.
  size_t svat_fail;             // Number of times the svat loop failed.
  size_t svat_total;            // Total number of times the svat loop entered.
  bool svat_failed;		// True iff the SVAT loop failed.
  double crop_ep_;              // Potential transpiration. [mm/h]
  double crop_ea_soil;          // Crop limited transpiration. [mm/h]
  double crop_ea_svat;          // SVAT limited transpiration. [mm/h]
  double crop_ea_;              // Actual transpiration. [mm/h]
  double production_stress;     // Stress calculated by SVAT module.

  // Bioclimate canopy
  double wind_speed_field_;     // wind speed at screen height above the canopy [m/s]
  double wind_speed_weather;    // measured wind speed [m/s]

  void WaterDistribution (const Time&,
                          Surface& surface, const Weather& weather, 
                          Vegetation& vegetation, const Litter& litter, 
                          const Movement&,
                          const Geometry&, const Soil& soil,
                          const SoilWater& soil_water, const SoilHeat&, 
			  const double T_bottom,
                          double dt, Treelog&);

  // Radiation.
  static double find_albedo (const Vegetation& crops, const Litter& litter,
                             const Surface& surface, 
                             const Geometry&, const Soil&, const SoilWater&);
  double albedo;                  // Reflection factor []
  const std::unique_ptr<Raddist> raddist;// Radiation distribution model.
  const double min_sin_beta_;     // Sinus to lowest sun angle for some models.
  void RadiationDistribution (const Vegetation&, double pF, double sin_beta, Treelog&);
  std::unique_ptr<Difrad> difrad;  // Diffuse radiation model.
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
  double atmospheric_O2_;         // From weather [Pa]
  double air_pressure_;         // From weather [Pa]

  // Deposition.
  const std::unique_ptr<Deposition> deposition;  // Deposition model.

  // Initialization.
  const bool fixed_pet;
  const bool fixed_difrad;
  bool has_reference_evapotranspiration;
  bool has_vapor_pressure;
  bool has_wind;
  bool has_min_max_temperature;
  bool has_diffuse_radiation;
  Weatherdata::surface_t old_surface;

  // Simulation
  void tick (const Time&, Surface&, const Weather&, 
             Vegetation&, const Litter& litter, 
             const Movement&, const Geometry&,
             const Soil&, SoilWater&, const SoilHeat&, const double T_bottom,
             double dt, Treelog&);
  void clear ();
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

  // Utils.
  double LAI_sun () const       // [area LEAF/FIELD]
  { return LAI () * sun_LAI_fraction_total (); }
  double LAI_shadow () const    // [area LEAF/FIELD]
  { return LAI () - LAI_sun (); }
  double Collatz_gbw (const double tleaf) const   // [m/s LEAF]
  {
    const double Ptot = air_pressure (); // [Pa]
    const double gbw_molly = 2.0;     // [mol/m^2/s LEAF]
    return Resistance::molly2ms (tleaf, Ptot, gbw_molly); // [m/s LEAF]
  }

  // Weather.
  double daily_rain_temperature () const
  { return std::max (daily_air_temperature_, 0.1); }
  double daily_air_temperature () const
  { return daily_air_temperature_; }
  double canopy_temperature () const
  { return svat->CanopyTemperature (); }
  double canopy_vapour_pressure () const
  { return svat->CanopyVapourPressure (); }
  double sun_leaf_temperature () const
  { return svat->SunLeafTemperature (); }
  double shadow_leaf_temperature () const
  { return svat->ShadowLeafTemperature (); }
  double sun_boundary_layer_water_conductivity () const 
  { 
    double gbw = svat->SunBoundaryLayerWaterConductivity (); 
    if (gbw > 0.0)
      return gbw;
    return Collatz_gbw (sun_leaf_temperature ()) * LAI_sun ();
  }
  double shadow_boundary_layer_water_conductivity () const
  { 
    double gbw = svat->ShadowBoundaryLayerWaterConductivity (); 
    if (gbw > 0.0)
      return gbw;
    return Collatz_gbw (shadow_leaf_temperature ()) * LAI_shadow ();
  }
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
  double atmospheric_O2 () const
  { return atmospheric_O2_; }
  double air_pressure () const
  { return air_pressure_; }

  // Manager.
  void irrigate_overhead (double flux, double temp);
  void irrigate_surface (double flux, double temp);
  void irrigate_overhead (double flux);
  void irrigate_surface (double flux);
  void irrigate_subsoil (double flux);
  void set_subsoil_irrigation (double flux);
  void add_tillage_water (double amount);

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
  double get_litter_water () const // [mm]
  { return litter_water_storage; }
  double get_litter_temperature () const // [dg C]
  { return litter_water_temperature; }
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
    const double epsilon_storage = 1e-8; // [mm]
    const double epsilon_out = 1e-8 / dt; // [mm/h]

    if (canopy_water_out < epsilon_out)
      return 0.0;

    if (canopy_water_storage < epsilon_storage)
      return -1.0;

    // Should be storage at "end of timestep" because of the way the
    // rate is used.
    const double clr = canopy_water_out / canopy_water_storage;
    daisy_assert (clr > 0.0);
    return clr;
  }
  double canopy_leak () const                     // [mm/h]
  { return canopy_water_out; }
  double litter_leak_rate (const double dt) const
  {
    if (litter_water_out < 1e-8)
      return 0.0;
    if (litter_water_storage < 0.01 * litter_water_out * dt)
      return 1.0 / dt;
    const double litter_water_old
      = litter_water_storage + litter_water_out * dt;
    
    return litter_water_out / litter_water_old;
  }
  double litter_wash_off_rate (const double dt) const
  {
    if (litter_wash_off < 1e-8)
      return 0.0;
    if (litter_water_storage < 0.01 * litter_wash_off * dt)
      return 1.0 / dt;
    const double litter_water_old
      = litter_water_storage + litter_wash_off * dt;
    
    return litter_wash_off / litter_water_old;
  }
  const IM& deposit () const
  { return deposition->deposit (); }

  // Create.
  void initialize (const Weather&, Treelog&);
  void reset_weather (const Weather&, Treelog&);
  bool check (const Weather&, Treelog&) const;
  BioclimateStandard (const BlockModel&);
  void summarize (Treelog&) const;
  ~BioclimateStandard ();
};

const double
BioclimateStandard::reference_height = 0.12; // [m]

void 
BioclimateStandard::initialize (const Weather& weather, Treelog& msg)
{
  TREELOG_MODEL (msg);
  reset_weather (weather, msg);
}

void 
BioclimateStandard::reset_weather (const Weather& weather, Treelog& msg)
{
  // Old weather.
  has_reference_evapotranspiration 
    = weather.has_reference_evapotranspiration ();
  has_vapor_pressure = weather.has_vapor_pressure ();
  has_wind = weather.has_wind ();
  has_min_max_temperature = weather.has_min_max_temperature ();
  has_diffuse_radiation = weather.has_diffuse_radiation ();
  old_surface = weather.surface ();
  const double timestep = weather.timestep ();
  
  // Potential evapotranspiration model.
  if (!fixed_pet)                      // Explicit.
    {
      symbol type;

      if (has_reference_evapotranspiration)
        type = "weather";
      else if (has_vapor_pressure && has_wind)
        {
          if (old_surface == Weatherdata::field)
            type = symbol ("PM");
          else if (timestep < 4.0)
            type = symbol ("FAO_PM_hourly");    
	  else
	    type = symbol ("FAO_PM");    
        }
      else if (has_min_max_temperature)
        type = symbol ("Hargreaves");
      else
        type = symbol ("deBruin87");

      msg.message ("Pet choosen: " + type);
      pet.reset (Librarian::build_stock<Pet> (metalib, msg, type, "pet"));
    }

  // Diffuse radiation model.
  if (!fixed_difrad)                      // Explicit.
    {
      symbol type;

      if (has_diffuse_radiation)
        type = symbol ("weather");
      else
        type = symbol ("DPF");

      msg.debug ("Difrad choosen: " + type);
      difrad.reset (Librarian::build_stock<Difrad> (metalib, msg, type, 
                                                    "difrad"));
    }
}

bool 
BioclimateStandard::check (const Weather& weather, Treelog& msg) const
{
  TREELOG_MODEL (msg);
  bool ok = true;
  if (!pet->check (weather, msg))
    ok = false;
  if (!svat->check (weather, msg))
    ok = false;
  if (weather.surface () != Weatherdata::field
      && weather.screen_height () < reference_height)
    {
      std::ostringstream tmp;
      tmp << "Weather ScreenHeight is " << weather.screen_height ()
          << " m, should be > " << reference_height << " m";
      msg.error (tmp.str ());
      ok = false;
    }

  return ok;
}

BioclimateStandard::BioclimateStandard (const BlockModel& al)
  : Bioclimate (al),
    metalib (al.metalib ()),
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
    irrigation_overhead_temperature (0.0),
    irrigation_surface (0.0),
    irrigation_surface_temperature (0.0),
    irrigation_subsoil (0.0),
    irrigation_subsoil_permanent (al.number ("irrigation_subsoil_permanent")),
    tillage_water (0.0),
    snow (al.submodel ("Snow")),
    snow_ep (0.0),
    snow_ea_ (0.0),
    snow_water_in (0.0),
    snow_water_in_temperature (0.0),
    snow_water_out (0.0),
    snow_water_out_temperature (0.0),
    canopy_ep (0.0),
    canopy_ea_ (0.0),
    canopy_water_capacity (0.0),
    canopy_water_storage (al.number ("canopy_water_storage")),
    canopy_water_temperature (0.0),
    canopy_water_in (0.0),
    canopy_water_out (0.0),
    canopy_water_bypass (0.0),
    canopy_water_below (0.0),
    litter_ep (0.0),
    litter_ea (0.0),
    litter_water_capacity (0.0),
    litter_water_storage (al.number ("litter_water_storage")),
    litter_water_temperature (0.0),
    litter_water_in (0.0),
    litter_water_out (0.0),
    litter_wash_off (0.0),
    pond_ep (0.0),
    pond_ea_ (0.0),
    soil_ep (0.0),
    soil_ea_ (0.0),
    max_svat_iterations (al.integer ("max_svat_iterations")),
    max_svat_absolute_difference (al.number ("max_svat_absolute_difference")),
    maxTdiff (al.number ("maxTdiff")),
    maxEdiff (al.number ("maxEdiff")),
    svat (Librarian::build_item<SVAT> (al, "svat")),
    svat_fail (0),
    svat_total (0),
    svat_failed (true),
    crop_ep_ (0.0),
    crop_ea_soil (0.0),
    crop_ea_svat (0.0),
    crop_ea_ (0.0),
    production_stress (-1.0),
    albedo (NAN),
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
    atmospheric_O2_ (-42.42e42),
    air_pressure_ (-42.42e42),

    // Deposition.
    deposition (Librarian::build_item<Deposition> (al, "deposition")),

    // For initialization and weather data shifts.
    fixed_pet (pet.get ()),
    fixed_difrad (difrad.get ()),
    has_reference_evapotranspiration (false),
    has_vapor_pressure (false),
    has_wind (false),
    has_min_max_temperature (false),
    has_diffuse_radiation (false),
    old_surface (Weatherdata::field)
{ }

void 
BioclimateStandard::summarize (Treelog& msg) const
{
  TREELOG_MODEL (msg);
  if (svat_fail > 0)
    {
      TREELOG_MODEL (msg);
      daisy_assert (svat_total > 0);
      std::ostringstream tmp;
      tmp << "Convergence of svat loop failed " << svat_fail 
          << " times out of " << svat_total << "; or "
          << (100.0 * svat_fail / (svat_total + 0.0)) << "%";
      msg.warning (tmp.str ());
      msg.message ("See 'daisy.log' for details");
    }

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
BioclimateStandard::find_albedo (const Vegetation& crops, const Litter& litter,
                                 const Surface& surface, 
                                 const Geometry& geo,
                                 const Soil& soil, const SoilWater& soil_water)
{
  const double surface_albedo = surface.albedo (geo, soil, soil_water);
  const double litter_albedo = litter.albedo ();
  const double litter_cover = litter.cover ();

  // Find albedo below crops.
  const double below_albedo = (litter_albedo < 0.0)
    ? surface_albedo
    : litter_albedo * litter_cover + surface_albedo * (1.0 - litter_cover);

  const double crop_cover = crops.cover ();
  return crops.albedo () * crop_cover
    + below_albedo * (1.0 - crop_cover);
}

void 
BioclimateStandard::RadiationDistribution (const Vegetation& vegetation, const double pF,
                                           const double sin_beta_, Treelog& msg)
{
  TREELOG_MODEL (msg);

  raddist->tick(sun_LAI_fraction_, sun_PAR_, total_PAR_, sun_NIR_, total_NIR_,
                global_radiation (), difrad0, min_sin_beta_, sin_beta_, 
                vegetation, pF, msg);

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
  for (int i = 0; i < No; i++)
    sun_LAI_fraction_total_ += sun_LAI_fraction_[i] / No;
  daisy_assert (sun_LAI_fraction_total_ <= 1.0);
  
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
BioclimateStandard::WaterDistribution (const Time& time, Surface& surface,
                                       const Weather& weather, 
                                       Vegetation& vegetation,
                                       const Litter& litter,
                                       const Movement& movement,
                                       const Geometry& geo,
                                       const Soil& soil, 
                                       const SoilWater& soil_water,
                                       const SoilHeat& soil_heat, 
				       const double T_bottom,
                                       const double dt, Treelog& msg)
{
  TREELOG_MODEL (msg);

  // Overview.
  //
  // First we calculate the external water sources (precipitation,
  // irrigation) and sinks (potential evapotranspiration).  Then we
  // update the sources, sinks and storage for each of the following
  // five "containers": The snow pack, water intercepted on the canopy,
  // litter, pond, and soil surface.  It is assumed that each the flows for
  // each container only depends on the container above, so we
  // calculate the changes from the top and downwards.
  //
  // A final and fifth "container" is the crop transpiration.

  // 1 External water sinks and sources. 

  // Net Radiation.
  const double Cloudiness = weather.cloudiness ();
  const double air_temperature = weather.air_temperature ();//[dg C]
  const double VaporPressure = weather.vapor_pressure ();
  const double Si = weather.global_radiation ();
  albedo = find_albedo (vegetation, litter, surface, geo, soil, soil_water);
  net_radiation->tick (Cloudiness, air_temperature, VaporPressure, Si, albedo,
                       msg);
  const double Rn = net_radiation->net_radiation ();

  // 1.1 Fluxify management operations.

  tillage_water /= dt;

  // 1.2 Weather.
  const double rain = weather.rain ();

  // 1.3 Evapotranspiration

  const double total_input 
    = rain + weather.snow () + irrigation_overhead + irrigation_surface;
  const double free_water = total_input * dt 
    + snow.storage () + surface.ponding_average () 
    + canopy_water_storage + litter_water_storage;

  daisy_assert (pet.get () != NULL);
  pet->tick (time, 
             weather, Rn, vegetation, surface, 
             geo, soil, soil_heat, soil_water, msg);
  if (free_water > 0.01)
    total_ep_ = pet->wet ();
  else
    total_ep_ = pet->dry ();
    
  const double total_ep_dry = pet->dry ();

  daisy_assert (std::isfinite (total_ep_));
  daisy_assert (total_ep_ >= 0.0);
  total_ea_ = 0.0;              // To be calculated.

  // 2 Snow Pack

  const double rain_temperature = std::max (air_temperature, 0.1);
  snow_ep = total_ep_ - total_ea_;
  daisy_assert (snow_ep >= 0.0);
  snow_water_in = rain + irrigation_overhead;
  daisy_assert (snow_water_in >= 0.0);
  if (irrigation_overhead > 0.01)
    snow_water_in_temperature 
      = (irrigation_overhead * irrigation_overhead_temperature
         + rain * rain_temperature) / snow_water_in;
  else if (rain > 0.01)
    snow_water_in_temperature = rain_temperature;
  else
    snow_water_in_temperature = air_temperature;

  snow.tick (msg, movement, soil, soil_water, soil_heat, 
             weather.global_radiation (), 0.0,
             snow_water_in, weather.snow (),
             surface.ponding_average (),
             snow_water_in_temperature, snow_ep, dt);
  snow_ea_ = snow.evaporation ();
  daisy_assert (snow_ea_ >= 0.0);
  total_ea_ += snow_ea_;
  daisy_assert (total_ea_ >= 0.0);
  snow_water_out = snow.percolation ();
  if (snow_water_out < 0.0)
    {
      double adjusted_pond = snow_water_out * dt + surface.ponding_average ();
      if (adjusted_pond < 0.0)
        {
          if (approximate (-snow_water_out * dt, surface.ponding_average ()))
            adjusted_pond = 0.0;
          else
            {
              std::ostringstream tmp;
              tmp << "snow_water_out (" << snow_water_out 
                  << ") * dt (" << dt 
                  << ") + pond (" << surface.ponding_average () 
                  << ") = " << adjusted_pond << ") < 0";
              msg.warning (tmp.str ());
            }
        }
      surface.put_ponding (adjusted_pond);
      snow_water_out = 0.0;
    }
  snow_water_out_temperature = snow.temperature ();
  const double below_snow_ep = total_ep_ - snow_ea_;

  // 3 Water intercepted on canopy

  const double canopy_water_capacity = vegetation.interception_capacity ();
  daisy_assert (canopy_water_capacity >= 0.0);
  
  const double canopy_cover = cover ();
  daisy_assert (snow_ea_ <= total_ep_);
  canopy_ep = below_snow_ep * canopy_cover;
  const double below_canopy_ep = below_snow_ep - canopy_ep;
  const double canopy_ep_dry = total_ep_dry * canopy_cover;
  const double below_canopy_ep_dry = total_ep_dry - canopy_ep_dry;
  if (below_canopy_ep_dry < 0.0 || !std::isfinite (below_canopy_ep_dry))
    {
      std::ostringstream tmp;
      tmp << "below_canopy_ep_dry = " << below_canopy_ep_dry
	  << ", total_ep_dry = " << total_ep_dry 
	  << ", canopy_cover = " << canopy_cover;
      daisy_warning (tmp.str ());
    }

  daisy_assert (canopy_ep >= 0.0);
  if (snow_water_out < 0.0)
    {
      canopy_water_in = 0.0;
      canopy_water_bypass = snow_water_out;
    }
  else
    {
      canopy_water_in = snow_water_out * canopy_cover;
      canopy_water_bypass = snow_water_out - canopy_water_in;
    }
  daisy_assert (canopy_water_in >= 0.0);

  if (canopy_water_in > 0.01)
    canopy_water_temperature 
      = (canopy_water_storage * rain_temperature 
         + canopy_water_in * snow_water_out_temperature * dt)
      / (canopy_water_storage + canopy_water_in * dt);
  else
    canopy_water_temperature = air_temperature;

  canopy_ea_ = std::min (canopy_ep, 
                         canopy_water_storage / dt + canopy_water_in);
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

  const double litter_cover = litter.cover ();
  litter_ep = below_canopy_ep * litter_cover;

  daisy_assert (snow_ea_ <= total_ep_);
  if (litter_ep < 0.0)
    {
      std::ostringstream tmp;
      tmp << "BUG:\nlitter_ep = " << litter_ep << "\n"
          << "total_ep = " << total_ep_ << "\n"
          << "snow_ea_ = " << snow_ea_ << "\n"
          << "cover  = " << canopy_cover;
      msg.error (tmp.str ());
      litter_ep = 0.0;
    }

  canopy_water_below = canopy_water_out + canopy_water_bypass 
    + irrigation_surface + tillage_water; 
  const double canopy_water_below_temperature 
    = canopy_water_below > 0.01
    ? ((canopy_water_bypass * snow_water_out_temperature
        + canopy_water_out * canopy_water_temperature
        + irrigation_surface * irrigation_surface_temperature
        + tillage_water * rain_temperature)
       / canopy_water_below)
    : air_temperature;

  // Fraction of water that hits the litter that is intercepted by it.
  const double litter_water_hit = canopy_water_below * litter_cover;
  const double litter_intercept = litter.intercept ();
  litter_water_in = litter_water_hit * litter_intercept;
  litter_wash_off = litter_water_hit - litter_water_in;
  
  const double litter_water_bypass = canopy_water_below - litter_water_in;
  
  if (litter_water_in > 0.01)
    litter_water_temperature 
      = (litter_water_storage * rain_temperature
         + litter_water_in * dt * canopy_water_below_temperature)
      / (litter_water_in * dt + litter_water_storage);
  else
    litter_water_temperature = air_temperature;

  daisy_assert (litter_water_storage >= 0.0);
  litter_ea = std::max (std::min (litter_ep,
                                  litter_water_storage / dt + litter_water_in),
                        0.0);
  total_ea_ += litter_ea;
  daisy_assert (total_ea_ >= 0.0);
  
  litter_water_capacity = litter.water_capacity ();
  litter_water_storage += (litter_water_in - litter_ea) * dt;
  const double litter_potential_exfiltration = litter.potential_exfiltration ();
  const double litter_overflow
    = (litter_water_storage - litter_water_capacity) / dt;
  const double litter_evacuation
    = litter_water_storage / dt;
  const double litter_potential_down =
    std::min (litter_evacuation,
	      std::max (litter_potential_exfiltration, litter_overflow));
  
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
  else if (litter_potential_down > 0.0)
    {
      litter_water_out = litter_potential_down;
      litter_water_storage -= litter_water_out * dt;
    }
  else
    litter_water_out = 0.0;

  // 5 Ponding

  pond_ep = (litter_ep - litter_ea) * litter.vapor_flux_factor()
    + below_canopy_ep * (1.0 - litter_cover);
  daisy_assert (litter_ea <= litter_ep);
  if (pond_ep < 0.0)
    {
      std::ostringstream tmp;
      tmp << "BUG:\npond_ep = " << pond_ep << "\n"
          << "litter_ep = " << litter_ep << "\n"
          << "litter_ea = " << litter_ea << "\n"
          << "litter_factor = " << litter.vapor_flux_factor ();
      msg.error (tmp.str ());
      pond_ep = 0.0;
    }
  
  const double pond_in = litter_water_out + litter_water_bypass;
  const double pond_in_temperature = (pond_in > 0.01)
    ? ((litter_water_out * litter_water_temperature
        + litter_water_bypass * canopy_water_below_temperature) / pond_in)
    : air_temperature;

  const double soil_T 
    = geo.content_hood (soil_heat, &SoilHeat::T, Geometry::cell_above);
  surface.tick (msg, pond_ep, below_canopy_ep_dry, 
                pond_in, pond_in_temperature, 
                geo, soil, soil_water, soil_T, dt);
  pond_ea_ = surface.evap_pond (dt, msg);
  total_ea_ += pond_ea_;

  // 6 Soil

  soil_ep = bound (0.0, pond_ep - pond_ea_, 
		   std::max (0.0, below_canopy_ep_dry));
  soil_ea_ = surface.exfiltration (dt);
  daisy_assert (soil_ea_ >= 0.0);
  total_ea_ += soil_ea_;
  daisy_assert (total_ea_ >= 0.0);

  // 7 Transpiration

  // Potential transpiration
  const double potential_crop_transpiration = canopy_ep - canopy_ea_;
  const double potential_soil_transpiration 
    = std::max ((soil_ep - soil_ea_) * vegetation.EpInterchange (), 0.0);
  crop_ep_ = bound (0.0, 
                    potential_crop_transpiration,
                    std::max (0.0, canopy_ep_dry))
    + potential_soil_transpiration;

  // Actual transpiration, based on remaining energy.
  crop_ea_
    = vegetation.transpiration (crop_ep_, canopy_ea_, geo, soil,
                                soil_water, dt, msg);
  daisy_assert (crop_ea_ >= 0.0);
  crop_ea_soil = crop_ea_;       // Remember original value.
  
  // Actual transpiration, modified by the SVAT model.

  // Let the SVAT model get the boundary conditions.
  svat->tick (weather, vegetation, geo, soil, soil_heat, T_bottom, soil_water, 
              *this, movement, dt, maxTdiff, maxEdiff, msg);

  // Our initial guess for transpiration is based on remaining energy.
  double crop_ea_svat_old = crop_ea_soil;
  std::ostringstream lout;

  // Use relaxation faction for gs
  double gs_shadow_sum = 0.0;
  double gs_sunlit_sum = 0.0;

  svat_failed = false;
  for (int iteration = 0; iteration < max_svat_iterations; iteration++)
    {
      TreelogStore svat_msg;

      // Old values
      const double old_CanopyTemperature     // [dg C]
        = svat->CanopyTemperature ();
      const double old_SunLeafTemperature  // [dg C]
        = svat->SunLeafTemperature ();
      const double old_ShadowLeafTemperature  // [dg C]
        = svat->ShadowLeafTemperature ();
      const double old_CanopyVapourPressure   // [Pa]
        = svat->CanopyVapourPressure ();
      const double old_SunBoundaryLayerWaterConductivity // [m/s]
        = svat->SunBoundaryLayerWaterConductivity ();
      const double old_ShadowBoundaryLayerWaterConductivity  // [m/s]
        = svat->ShadowBoundaryLayerWaterConductivity ();

      // Find stomata conductance based on ABA and crown potential
      // from last attempt at crop transpiration.
      vegetation.find_stomata_conductance (time, *this, dt, svat_msg);
      const double new_weight = 0.0;
      const double gs_shadow_new = vegetation.shadow_stomata_conductance ();
      gs_shadow_sum += gs_shadow_new;
      const double gs_shadow 
        = (1.0 - new_weight) * gs_shadow_sum / (iteration + 1.0)
        + new_weight * gs_shadow_new;
      const double gs_sunlit_new = vegetation.sunlit_stomata_conductance ();
      gs_sunlit_sum += gs_sunlit_new;
      const double gs_sunlit 
        = (1.0 - new_weight) * gs_sunlit_sum / (iteration + 1.0)
        + new_weight * gs_sunlit_new;

      // Find expected transpiration from stomate conductance.
      const double max_gs = 0.001; // [m/s]
      svat->solve (gs_shadow, gs_sunlit, svat_msg);
      
      const double crop_ea_svat = svat->transpiration ();

      if ((std::fabs (old_CanopyTemperature - svat->CanopyTemperature ())
           < maxTdiff)
          && (std::fabs (old_SunLeafTemperature - svat->SunLeafTemperature ())
              < maxTdiff)
          && (std::fabs (old_ShadowLeafTemperature
                         - svat->ShadowLeafTemperature ())
              < maxTdiff)
          && (std::fabs (old_CanopyVapourPressure 
                         - svat->CanopyVapourPressure ())
              < maxEdiff)
          && (std::fabs (old_SunBoundaryLayerWaterConductivity 
                         - svat->SunBoundaryLayerWaterConductivity ())
              < max_gs)
          && (std::fabs (old_ShadowBoundaryLayerWaterConductivity 
                         - svat->ShadowBoundaryLayerWaterConductivity ())
              < max_gs)
          && (std::fabs (crop_ea_svat - crop_ea_svat_old) 
              < max_svat_absolute_difference)
          && (std::fabs (gs_shadow_new - gs_shadow) < max_gs)
          && (std::fabs (gs_sunlit_new - gs_sunlit) < max_gs))
        {
          // Stomate may limit transpiration, not increase it.
          //  daisy_assert (crop_ea_ < crop_ea_soil + 0.01);
          if (!svat->stable ())
            {
              std::ostringstream tmp;
              tmp << "svat iteration " << iteration;
              Treelog::Open nest (msg, tmp.str ());
              if (svat_fail > 0)
                svat_msg.propagate_debug (msg);
              else
                svat_msg.propagate (msg);
              svat_failed = true;
            }
          goto success;
        }
      lout << "\niteration " << iteration 
           << ", Tc = " << old_CanopyTemperature 
           << ", Tsun = " << old_SunLeafTemperature
           << ", Tshadow = " << old_ShadowLeafTemperature
           << ", ec = " << old_CanopyVapourPressure
           << ", gb_sun = " << old_SunBoundaryLayerWaterConductivity
           << ", gb_shadow = " << old_ShadowBoundaryLayerWaterConductivity
           << ", gs_shadow = " << gs_shadow
           << ", gs_sun = " << gs_sunlit
           << ", ea = " << crop_ea_svat_old;

      crop_ea_svat_old = crop_ea_svat;

      // Calculate new crop transpiration based on latest SVAT guess.
      crop_ea_
        = vegetation.transpiration (crop_ea_svat, canopy_ea_, geo, soil,
                                    soil_water, dt, msg);
      daisy_assert (crop_ea_ >= 0.0);
    }
  msg.error ("SVAT transpiration and stomata conductance"
             " loop did not converge");
  msg.debug (lout.str ());
  svat_failed = true;
 success:;
  if (svat_failed)
    svat_fail++;
  svat_total++;

  // Stress calculated by the SVAT model.
  production_stress = svat->production_stress ();
  vegetation.force_production_stress (production_stress);

  surface.set_svat_temperature (svat->SoilSurfaceTemperature ());
  
  // Total evapotranspiration.
  total_ea_ += crop_ea_;
  daisy_assert (total_ea_ >= 0.0);

  // Direct rain, used for colloid generation
  if (snow_water_out < 0.01)
    direct_rain_ = 0.0;
  else
    // We want to ignore irrigation and melting snow here.
    direct_rain_ = canopy_water_bypass * (rain / snow_water_out);

  // Check
  // Note: total_ea can be larger than total_ep, as PMSW uses a
  // different method for calculating PET.
  daisy_assert (approximate (total_ea_,
                             snow_ea_ + canopy_ea_ + litter_ea
                             + pond_ea_ + soil_ea_ + crop_ea_));
}

void 
BioclimateStandard::tick (const Time& time, 
                          Surface& surface, const Weather& weather,  
                          Vegetation& vegetation, const Litter& litter,
                          const Movement& movement,
                          const Geometry& geo, const Soil& soil, 
                          SoilWater& soil_water, const SoilHeat& soil_heat,
			  const double T_bottom,
                          const double dt, Treelog& msg)
{
  TREELOG_MODEL (msg);

  // Check if weather structure have changed enough to make us switch model.
  if (has_reference_evapotranspiration 
      != weather.has_reference_evapotranspiration ()
      || has_vapor_pressure != weather.has_vapor_pressure ()
      || has_wind != weather.has_wind ()
      || has_min_max_temperature != weather.has_min_max_temperature ()
      || has_diffuse_radiation != weather.has_diffuse_radiation ()
      || old_surface != weather.surface ())
    reset_weather (weather, msg);

  // Keep weather information during time step.
  // Remember this in case the crops should ask.
  global_radiation_ = weather.global_radiation ();
  daily_global_radiation_ = weather.daily_global_radiation ();
  daily_air_temperature_ = weather.daily_air_temperature ();
  daily_precipitation_ = weather.daily_precipitation ();
  day_length_ = weather.day_length ();
  atmospheric_CO2_ = weather.CO2 ();
  atmospheric_O2_ = weather.O2 ();
  air_pressure_ = weather.air_pressure ();

  // Update canopy structure.
  CanopyStructure (vegetation);
 
  // Radiation.
  daisy_assert (difrad.get () != NULL);
  difrad0 = difrad->value (time, weather, msg) * global_radiation_;
  //daisy_assert (difrad0 >= 0.0);

  const double pF 
    = h2pF (geo.content_hood (soil_water, &SoilWater::h, Geometry::cell_above));

  sin_beta_ = weather.sin_solar_elevation_angle (time);
  // Calculate total canopy, divide it into intervals, and distribute PAR.
  RadiationDistribution (vegetation, pF, sin_beta_, msg);

  // Distribute water among canopy, snow, and soil.
  WaterDistribution (time, surface, weather, vegetation, litter,
                     movement, geo, soil, soil_water, soil_heat, T_bottom, 
		     dt, msg);

  // Convert wind speed to field conditions.
  const double ScreenHeight = weather.screen_height (); //[m]

  wind_speed_weather =  weather.wind ();//[m/s]

  const double h = Height[0]/100.0; // [m]
  const double h0 = reference_height; // reference height [m]

  if (weather.surface () == Weatherdata::field || h < h0)
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

  deposition->tick (vegetation, weather, msg);
}

void
BioclimateStandard::clear () 
{
  irrigation_overhead = 0.0;
  irrigation_surface = 0.0;
  irrigation_subsoil = 0.0;
  tillage_water = 0.0;
}

void 
BioclimateStandard::output (Log& log) const
{
  output_variable (Height, log);
  output_variable (albedo, log);
  output_derived (net_radiation, "net_radiation", log);
  output_derived (raddist, "raddist", log);
  daisy_assert (pet.get () != NULL);
  output_object (pet.get (), "pet", log);
  output_value (total_ep_, "total_ep", log);
  output_value (total_ea_, "total_ea", log);
  output_value (direct_rain_, "direct_rain", log);
  output_variable (irrigation_overhead, log);
  output_value (irrigation_overhead_temperature, 
                "irrigation_overhead_temperature", log);
  output_variable (irrigation_surface, log);
  output_value (irrigation_surface_temperature,
                "irrigation_surface_temperature", log);
  output_value (irrigation_subsoil + irrigation_subsoil_permanent,
                "irrigation_subsoil", log);
  output_variable (irrigation_subsoil_permanent, log);
  output_value (irrigation_subsoil  + irrigation_subsoil_permanent
                + irrigation_surface + irrigation_overhead, 
                "irrigation_total", log);
  output_variable (tillage_water, log);
  output_submodule (snow, "Snow", log);
  output_variable (snow_ep, log);
  output_value (snow_ea_, "snow_ea", log);
  output_variable (snow_water_in, log);
  output_variable (snow_water_in_temperature, log);
  output_variable (snow_water_out, log);
  output_value (snow_water_out_temperature, 
                "snow_water_out_temperature", log);
  output_value (cover_, "canopy_cover", log);
  output_variable (canopy_ep, log);
  output_value (canopy_ea_, "canopy_ea", log);
  output_variable (canopy_water_capacity, log);
  output_variable (canopy_water_storage, log);
  output_variable (canopy_water_temperature, log);
  output_variable (canopy_water_in, log);
  output_variable (canopy_water_out, log);
  output_variable (canopy_water_bypass, log);
  output_variable (canopy_water_below, log);
  output_variable (litter_ep, log);
  output_variable (litter_ea, log);
  output_variable (litter_water_capacity, log);
  output_variable (litter_water_storage, log);
  output_variable (litter_water_temperature, log);
  output_variable (litter_water_in, log);
  output_variable (litter_water_out, log);
  output_variable (litter_wash_off, log);
  output_variable (pond_ep, log);
  output_value (pond_ea_, "pond_ea", log);
  output_variable (soil_ep, log);
  output_value (soil_ea_, "soil_ea", log);
  output_value (svat_failed ? 1 : 0, "svat_failed", log);
  output_derived (svat, "svat", log);
  output_value (crop_ep_, "crop_ep", log);
  output_variable (crop_ea_soil, log);
  output_variable (crop_ea_svat, log);
  output_value (crop_ea_, "crop_ea", log);
  output_variable (production_stress, log);

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
  output_value (global_radiation_
		- incoming_PAR_radiation - incoming_NIR_radiation,
		"outgoing_Short_radiation", log);
  // Deposition.
  output_derived (deposition, "deposition", log);
}

void
BioclimateStandard::irrigate_overhead (double flux, double temp)
{
  double new_top = irrigation_overhead + flux;

  irrigation_overhead_temperature 
    = (new_top > 0.01)
    ? (temp * flux
       + irrigation_overhead * irrigation_overhead_temperature) / new_top
    : daily_rain_temperature ();
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
    : daily_rain_temperature ();
  irrigation_surface = new_surface;
}

void
BioclimateStandard::irrigate_overhead (double flux)
{ irrigate_overhead (flux, daily_rain_temperature ()); }

void
BioclimateStandard::irrigate_surface (double flux)
{ irrigate_surface (flux, daily_rain_temperature ()); }

void
BioclimateStandard::irrigate_subsoil (double flux)
{ irrigation_subsoil += flux; }

void
BioclimateStandard::set_subsoil_irrigation (double flux)
{ irrigation_subsoil_permanent = flux; }

void
BioclimateStandard::add_tillage_water (double amount)
{ tillage_water += amount; }

static struct BioclimateStandardSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
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
    frame.declare ("Height", "cm",
                   Attribute::LogOnly, Attribute::CanopyEdges, "\
End points of canopy layers.\n                                  \
First entry is top of canopy, last is soil surface.");
    // External water sources and sinks.
    frame.declare_object ("net_radiation", NetRadiation::component,
                          "Net radiation.");
    frame.set ("net_radiation", "brunt");
    frame.declare_object ("pet", Pet::component, 
                          Attribute::OptionalState, Attribute::Singleton, 
                          "Potential Evapotranspiration component.\n\
\n\
Some pet models provide answers for both dry and wet surface.  For\n\
those, the wet answer will limit total evapotranspiration, while the\n\
dry answer will further limit transpiration.\n\
\n\
The default model depends on available climate date.\n\
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
    frame.declare ("tillage_water", "mm/h", Attribute::LogOnly,
                   "Water added to surface due to tillage operations.");

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
    frame.declare_fraction ("canopy_cover", Attribute::LogOnly,
			    "Fraction of ground covered by canopy.");
    frame.declare ("canopy_ep", "mm/h", Attribute::LogOnly,
                   "Potential canopy evaporation.");
    frame.declare ("canopy_ea", "mm/h", Attribute::LogOnly,
                   "Actual canopy evaporation.");
    frame.declare ("canopy_water_capacity", "mm", Attribute::LogOnly,
                   "Potential intercepted water on canopy.");
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
    frame.declare ("canopy_water_below", "mm/h", Attribute::LogOnly,
                   "Total water input below canopy.");

    // Water intercepted by litter.
    frame.declare ("litter_ep", "mm/h", Attribute::LogOnly,
                   "Potential evaporation litter.");
    frame.declare ("litter_ea", "mm/h", Attribute::LogOnly,
                   "Actual litter evaporation.");
    frame.declare ("litter_water_capacity", "mm", Attribute::LogOnly,
                   "Potential intercepted water on litter.");
    frame.declare ("litter_water_storage", "mm", Attribute::State,
                   "Intercepted water on litter.");
    frame.set ("litter_water_storage", 0.0);
    frame.declare ("litter_water_temperature", "dg C", Attribute::LogOnly,
                   "Temperature of incoming water.");
    frame.declare ("litter_water_in", "mm/h", Attribute::LogOnly,
                   "Water entering litter.");
    frame.declare ("litter_water_out", "mm/h", Attribute::LogOnly,
                   "Litter drip throughfall.");
    frame.declare ("litter_wash_off", "mm/h", Attribute::LogOnly,
                   "Water hitting but not entering litter.");

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
    frame.declare ("maxTdiff", "K", Attribute::Const, "\
Largest temperature difference for convergence.");
    frame.set ("maxTdiff", 0.02);
    frame.declare ("maxEdiff", "Pa", Attribute::Const, "\
Largest humidity difference for convergence.");
    frame.set ("maxEdiff", 1.0);

    frame.declare_object ("svat", SVAT::component, 
                          "Soil Vegetation Atmosphere component.");
    frame.set ("svat", "none");
    frame.declare ("soil_ep", "mm/h", Attribute::LogOnly,
                   "Potential exfiltration.");
    frame.declare ("soil_ea", "mm/h", Attribute::LogOnly,
                   "Actual exfiltration.");

    frame.declare_integer ("svat_failed", Attribute::LogOnly, "\
'0' if the SVAT model found a solution, '1' otherwise.");
    
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
    frame.declare ("albedo", Attribute::None (), Attribute::LogOnly, "\
Reflection factor.");
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

    frame.declare ("absorbed_total_PAR_canopy","W/m^2", Attribute::LogOnly,
                   "Canopy absorbed PAR (sun+shade)");
    frame.declare ("absorbed_total_NIR_canopy","W/m^2", Attribute::LogOnly,
                   "Canopy absorbed NIR (sun+shade)");
    frame.declare ("absorbed_total_Long_canopy","W/m^2", Attribute::LogOnly,
                   "Canopy absorbed long wave radiation (sun+shade)");
    frame.declare ("absorbed_total_PAR_soil","W/m^2", Attribute::LogOnly,
                   "Soil absorbed PAR (sun+shade)");
    frame.declare ("absorbed_total_NIR_soil","W/m^2", Attribute::LogOnly,
                   "Soil absorbed NIR (sun+shade)");
    frame.declare ("absorbed_total_Long_soil","W/m^2", Attribute::LogOnly,
                   "Soil absorbed long wave radiation (sun+shade)");
    frame.declare ("absorbed_sun_PAR_canopy","W/m^2", Attribute::LogOnly,
                   "Canopy absorbed PAR on sunlit leaves");
    frame.declare ("absorbed_sun_NIR_canopy","W/m^2", Attribute::LogOnly,
                   "Canopy absorbed NIR on sunlit leaves");
    frame.declare ("absorbed_sun_Long_canopy","W/m^2", Attribute::LogOnly,
                   "Canopy absorbed long wave radiatio on sunlit leaves");
    frame.declare ("absorbed_shadow_PAR_canopy","W/m^2", Attribute::LogOnly,
                   "Canopy absorbed PAR on shadow leaves");
    frame.declare ("absorbed_shadow_NIR_canopy","W/m^2", Attribute::LogOnly,
                   "Canopy absorbed NIR on shadow leaves");
    frame.declare ("absorbed_shadow_Long_canopy","W/m^2", Attribute::LogOnly,
                   "Canopy absorbed long wave radiation on shadow leaves");
    frame.declare ("incoming_Long_radiation","W/m^2", Attribute::LogOnly,
                   "Incoming longwave radiation");
    frame.declare ("incoming_PAR_radiation","W/m^2", Attribute::LogOnly,
                   "Incoming PAR radiation");
    frame.declare ("incoming_NIR_radiation","W/m^2", Attribute::LogOnly,
                   "Incoming NIR radiation");
    frame.declare ("incoming_Total_radiation","W/m^2", Attribute::LogOnly,
                   "Incoming radiation, sum of shortwave and longwave");
    frame.declare ("outgoing_Short_radiation", "W/m^2", Attribute::LogOnly,
		   "Outgoing shortware radiation.");

    frame.declare_object ("deposition", Deposition::component, 
                          "Deposition model.");
    frame.set ("deposition", "weather");
  }
} BioclimateStandard_syntax;

// bioclimate_std.C ends here
