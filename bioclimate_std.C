// bioclimate_std.C --- The default biclimate model

#include "bioclimate.h"
#include "surface.h"
#include "weather.h"
#include "csmp.h"
#include "alist.h"
#include "soil.h"
#include "common.h"
#include "syntax.h"
#include "snow.h"
#include "log.h"
#include "filter.h"
#include "mathlib.h"
#include "pet.h"
#include "pt.h"
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

  double irrigation_top;	// Irrigation above canopy [mm/h]
  double irrigation_top_old;	// Old value for logging.
  double irrigation_top_temperature; // Water temperature [dg C]
  double irrigation_surface;	// Irrigation below canopy [mm/h]
  double irrigation_surface_old; // Old value for logging.
  double irrigation_surface_temperature; // Water temperature [dg C]

  // Water in snowpack.
  Snow snow;
  double snow_ep;		// Potential snow evaporation [mm/h]
  double snow_ea;		// Actual snow evaporation [mm/h]
  double snow_water_in;		// Water entering snow pack [mm/h]
  double snow_water_in_temperature; // Incomming water temperature [dg C]
  double snow_water_out;	// Water leaving snow pack [mm/h]
  double snow_water_out_temperature; // Temperature of water leaving [dg C]

  // Water intercepted on canopy.
  double canopy_ep;		// Potential canopy evaporation [mm/h]
  double canopy_ea;		// Actual canopy evaporation [mm/h]
  double canopy_water_storage;	// Intercepted water on canopy [mm]
  double canopy_water_temperature; // Temperature of incomming water [dg C]
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
  PT& pt;			// Potential transpiration model.
  double crop_ep;		// Potential transpiration. [mm/h]
  double crop_ea;		// Actual transpiration. [mm/h]

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

  Chemicals surface_chemicals_storage;
  Chemicals surface_chemicals_in;
  Chemicals surface_chemicals_out;

  void ChemicalDistribution (const Vegetation&);

  // Radiation.
  void RadiationDistribution (const Weather&, const Vegetation&);
  void IntensityDistribution (double Rad0, double Ext, 
			      vector <double>& Rad) const;

  // Weather.
  double daily_air_temperature_; // Air temperature in canopy. [dg C]
  double day_length_;		// From weather (does not belong here) [h].
  double daily_global_radiation_; // From weather [W/m2].

  // Simulation
  void tick (Surface&, const Weather&, const Time&, 
	     Vegetation&, const Soil&, SoilWater&, const SoilHeat&);
  void output (Log&, Filter&) const;

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
  void irrigate_top (double flux, double temp);
  void irrigate_surface (double flux, double temp);
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
    irrigation_top (0.0),
    irrigation_top_old (0.0),
    irrigation_top_temperature (0.0),
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
    pt (Librarian<PT>::create (al.alist ("pt"))),
    crop_ep (0.0),
    crop_ea (0.0),
    spray_ (),
    snow_chemicals_storage (al.alist_sequence ("snow_chemicals_storage")),
    snow_chemicals_in (),
    snow_chemicals_out (),

    canopy_chemicals_storage (al.alist_sequence ("canopy_chemicals_storage")),
    canopy_chemicals_in (),
    canopy_chemicals_dissipate (),
    canopy_chemicals_out (),

    surface_chemicals_storage (al.alist_sequence
			       ("surface_chemicals_storage")),
    surface_chemicals_in (),
    surface_chemicals_out (),
    daily_air_temperature_ (0.0),
    day_length_ (0.0),
    daily_global_radiation_ (0.0)
{ }

BioclimateStandard::~BioclimateStandard ()
{ 
  delete &pet;
  delete &pt;
}

void
BioclimateStandard::CanopyStructure (const Vegetation& vegetation)
  // Calculate values for the total crop canopy.
{

  // Update vegetation state.
  const CSMP& HvsLAI = vegetation.HvsLAI ();
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
  // incomming radiation. 
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
  total_ea = 0.0;		// To be calculated.

  // 1.2 Irrigation
  //
  // Already set by manager.

  // 1.3 Weather.
  double rain = weather.rain ();
  double air_temperature = weather.hourly_air_temperature ();

  // 2 Snow Pack

  snow_ep = total_ep - total_ea;
  snow_water_in = rain + irrigation_top;
  if (irrigation_top > 0.01)
    snow_water_in_temperature 
      = (irrigation_top * irrigation_top_temperature
	 + rain * air_temperature) / snow_water_in;
  else
    snow_water_in_temperature = air_temperature;
  snow.tick (soil, soil_water, soil_heat, 
	     weather.hourly_global_radiation (), 0.0,
	     snow_water_in, weather.snow (),
	     snow_water_in_temperature, snow_ep);
  snow_ea = snow.evaporation ();
  total_ea += snow_ea;
  snow_water_out = snow.percolation ();
  snow_water_out_temperature = snow.temperature ();

  // 3 Water intercepted on canopy

  const double canopy_water_capacity = vegetation.interception_capacity ();
  canopy_ep = (total_ep - snow_ea) * vegetation.cover ();
  canopy_water_in = snow_water_out * vegetation.cover ();
  canopy_water_bypass = snow_water_out - canopy_water_in;
  
  if (canopy_water_in > 0.01)
    canopy_water_temperature 
      = (canopy_water_storage * air_temperature 
	 + canopy_water_in * snow_water_out_temperature)
      / (canopy_water_storage + canopy_water_in);
  else
    canopy_water_temperature = air_temperature;

  canopy_ea = min (canopy_ep, canopy_water_storage / dt + canopy_water_in);
  total_ea += canopy_ea;

  canopy_water_storage += (canopy_water_in - canopy_ea) * dt;
  
  if (canopy_water_storage > canopy_water_capacity + 1e-8)
    {
      canopy_water_out = canopy_water_storage - canopy_water_capacity;
      canopy_water_storage = canopy_water_capacity;
    }
  else
    {
      canopy_water_out = 0.0;
    }

  // 4 Ponding

  pond_ep = (total_ep - snow_ea) * (1.0 - vegetation.cover ());
  pond_water_in = canopy_water_out + canopy_water_bypass;
  if (pond_water_in > 0.01)
    pond_water_in_temperature 
      = (canopy_water_bypass * snow_water_out_temperature
	 + canopy_water_out * canopy_water_temperature)
      / pond_water_in;
  else
    pond_water_in_temperature = air_temperature;

  surface.tick (pond_ep, 
		pond_water_in, pond_water_in_temperature, 
		soil, soil_water);
  pond_ea = surface.evap_pond ();
  total_ea += pond_ea;

  // 5 Soil

  soil_ep = pond_ep - pond_ea;
  soil_ea = surface.exfiltration ();
  total_ea += soil_ea;

  // 6 Transpiration

  // Potential transpiration
  pt.tick (weather, vegetation, surface, soil, soil_heat, soil_water, pet,
	   canopy_ea, snow_ea, pond_ea, soil_ea);
  crop_ep = pt.potential_transpiration ();
  assert (crop_ep < total_ep - total_ea + 1e-8);
				
  // Actual transpiration
  crop_ea = vegetation.transpiration (crop_ep, canopy_ea, soil, soil_water) ;
  total_ea += crop_ea;
  
  // 7 Reset irrigation
  irrigation_top_old = irrigation_top;
  irrigation_top = 0.0;
  irrigation_surface_old = irrigation_surface;
  irrigation_surface = 0.0;

  // 8 Check
  assert (total_ea < total_ep + 1e-8);
  assert (approximate (total_ea,
		       snow_ea + canopy_ea + pond_ea + soil_ea + crop_ea));
}  

void 
BioclimateStandard::tick (Surface& surface, const Weather& weather, 
			  const Time&,
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
  ChemicalDistribution (vegetation);
}

void 
BioclimateStandard::ChemicalDistribution (const Vegetation& vegetation)
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
  surface_chemicals_storage += surface_chemicals_in;
  surface_chemicals_out.clear ();

  // Reset spray.
  spray_.clear ();
}

void 
BioclimateStandard::output (Log& log, Filter& filter) const
{
  output_derived (pet, "pet", log, filter);
  log.output ("total_ep", filter, total_ep, true);
  log.output ("total_ea", filter, total_ea, true);
  log.output ("irrigation_top", filter, irrigation_top_old, true);
  log.output ("irrigation_top_temperature", filter,
	      irrigation_top_temperature, true);
  log.output ("irrigation_surface", filter, irrigation_surface_old, true);
  log.output ("irrigation_surface_temperature", filter,
	      irrigation_surface_temperature, true);
  output_submodule (snow, "Snow", log, filter);
  log.output ("snow_ep", filter, snow_ep, true);
  log.output ("snow_ea", filter, snow_ea, true);
  log.output ("snow_water_in", filter, snow_water_in, true);
  log.output ("snow_water_in_temperature", 
	      filter, snow_water_in_temperature, true);
  log.output ("snow_water_out", filter, snow_water_out, true);
  log.output ("snow_water_out_temperature", 
	      filter, snow_water_out_temperature, true);
  log.output ("canopy_ep", filter, canopy_ep, true);
  log.output ("canopy_ea", filter, canopy_ea, true);
  log.output ("canopy_water_storage", filter, canopy_water_storage);
  log.output ("canopy_water_temperature", filter,
	      canopy_water_temperature, true);
  log.output ("canopy_water_in", filter, canopy_water_in, true);
  log.output ("canopy_water_out", filter, canopy_water_out, true);
  log.output ("canopy_water_bypass", filter, canopy_water_bypass, true);
  log.output ("pond_ep", filter, pond_ep, true);
  log.output ("pond_ea", filter, pond_ea, true);
  log.output ("pond_water_in", filter, pond_water_in, true);
  log.output ("pond_water_in_temperature", 
	      filter, pond_water_in_temperature, true);
  log.output ("soil_ep", filter, soil_ep, true);
  log.output ("soil_ea", filter, soil_ea, true);
  output_derived (pt, "pt", log, filter);
  log.output ("crop_ep", filter, crop_ep, true);
  log.output ("crop_ea", filter, crop_ea, true);

  // Note: We use snow_chemicals_in instead of spray, since the former
  // is reset after each time step.
  output_submodule (snow_chemicals_in, "spray", log, filter, true);
  output_submodule (snow_chemicals_storage, "snow_chemicals_storage",
		    log, filter);
  output_submodule (snow_chemicals_in, "snow_chemicals_in",
		    log, filter, true);
  output_submodule (snow_chemicals_out, "snow_chemicals_out",
		    log, filter, true);
  output_submodule (canopy_chemicals_storage, "canopy_chemicals_storage",
		    log, filter);
  output_submodule (canopy_chemicals_in, "canopy_chemicals_in",
		    log, filter, true);
  output_submodule (canopy_chemicals_dissipate, "canopy_chemicals_dissipate",
		    log, filter, true);
  output_submodule (canopy_chemicals_out, "canopy_chemicals_out",
		    log, filter, true);
  output_submodule (surface_chemicals_storage, "surface_chemicals_storage",
		    log, filter);
  output_submodule (surface_chemicals_in, "surface_chemicals_in",
		    log, filter, true);
  output_submodule (surface_chemicals_out, "surface_chemicals_out",
		    log, filter, true);
}

void
BioclimateStandard::irrigate_top (double flux, double temp)
{
  double new_top = irrigation_top + flux;
  irrigation_top_temperature 
    = (temp * flux + irrigation_top * irrigation_top_temperature) / new_top;
  irrigation_top = new_top;
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

#ifdef BORLAND_TEMPLATES
template class add_submodule<Snow>;
#endif BORLAND_TEMPLATES

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
Number of vertical intervals in which we partition the canopy");
      alist.add ("NoOfIntervals", 30);

      // External water sources and sinks.
      syntax.add ("pet", Librarian<Pet>::library (), 
		  "Potential Evapotranspiration component");
      AttributeList& pet_alist = *new AttributeList;
      pet_alist.add ("type", "makkink");
      alist.add ("pet", pet_alist);
      syntax.add ("total_ep", "mm/h", Syntax::LogOnly,
		  "Potential evapotranspiration");
      syntax.add ("total_ea", "mm/h", Syntax::LogOnly,
		  "Actual evapotranspiration");
      syntax.add ("irrigation_top", "mm/h", Syntax::LogOnly,
		  "Irrigation above canopy");
      syntax.add ("irrigation_top_temperature", "dg C", Syntax::LogOnly,
		  "Water temperature");
      syntax.add ("irrigation_surface", "mm/h", Syntax::LogOnly,
		  "Irrigation below canopy");
      syntax.add ("irrigation_surface_temperature", "dg C", Syntax::LogOnly,
		  "Water temperature");

      // Water in snowpack.
      add_submodule<Snow> ("Snow", syntax, alist, Syntax::State, 
			   "Surface snow pack");
      syntax.add ("snow_ep", "mm/h", Syntax::LogOnly,
		  "Potential snow evaporation");
      syntax.add ("snow_ea", "mm/h", Syntax::LogOnly,
		  "Actual snow evaporation");
      syntax.add ("snow_water_in", "mm/h", Syntax::LogOnly,
		  "Water entering snow pack");
      syntax.add ("snow_water_in_temperature", "dg C", Syntax::LogOnly,
		  "Temperature of water entering snow pack");
      syntax.add ("snow_water_out", "mm/h", Syntax::LogOnly,
		  "Water leaving snow pack");
      syntax.add ("snow_water_out_temperature", "dg C", Syntax::LogOnly,
		  "Temperature of water leaving snow pack");

      // Water intercepted on canopy.
      syntax.add ("canopy_ep", "mm/h", Syntax::LogOnly,
		  "Potential canopy evaporation");
      syntax.add ("canopy_ea", "mm/h", Syntax::LogOnly,
		  "Actual canopy evaporation");
      syntax.add ("canopy_water_storage", "mm", Syntax::State,
		  "Intercepted water on canopy");
      alist.add ("canopy_water_storage", 0.0);
      syntax.add ("canopy_water_temperature", "dg C", Syntax::LogOnly,
		  "Temperature of incomming water");
      syntax.add ("canopy_water_in", "mm/h", Syntax::LogOnly,
		  "Water entering canopy");
      syntax.add ("canopy_water_out", "mm/h", Syntax::LogOnly,
		  "Canopy drip throughfall");
      syntax.add ("canopy_water_bypass", "mm/h", Syntax::LogOnly,
		  "Water from above bypassing the canopy");
  
      // Water in pond.
      syntax.add ("pond_ep", "mm/h", Syntax::LogOnly,
		  "Potential evaporation from pond");
      syntax.add ("pond_ea", "mm/h", Syntax::LogOnly,
		  "Actual evaporation from pond");
      syntax.add ("pond_water_in", "mm/h", Syntax::LogOnly,
		  "Water entering pond");
      syntax.add ("pond_water_in_temperature", "dg C", Syntax::LogOnly,
		  "Temperature of water entering pond");

      // Water going through soil surface.
      syntax.add ("pt", Librarian<PT>::library (), 
		  "Potential Transpiration component");
      AttributeList& pt_alist = *new AttributeList;
      pt_alist.add ("type", "default");
      alist.add ("pt", pt_alist);
      syntax.add ("soil_ep", "mm/h", Syntax::LogOnly,
		  "Potential exfiltration.");
      syntax.add ("soil_ea", "mm/h", Syntax::LogOnly,
		  "Actual exfiltration.");

      // Water transpirated through plant roots.
      syntax.add ("crop_ep", "mm/h", Syntax::LogOnly,
		  "Potential transpiration.");
      syntax.add ("crop_ea", "mm/h", Syntax::LogOnly,
		  "Actual transpiration.");

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

      Chemicals::add_syntax  ("surface_chemicals_storage",
			      syntax, alist, Syntax::State,
			      "Chemicals on the soil surface.");
      Chemicals::add_syntax  ("surface_chemicals_in",
			      syntax, alist, Syntax::LogOnly,
			      "Chemicals entering soil surface.");
      Chemicals::add_syntax  ("surface_chemicals_out",
			      syntax, alist, Syntax::LogOnly,
			      "Chemicals entering the soil.");
      // Add to library.
      Librarian<Bioclimate>::add_type ("default", alist, syntax, &make);
    }
} BioclimateStandard_syntax;

// bioclimate_std.C ends here
