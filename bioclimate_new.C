// bioclimate_new.C

#include "bioclimate.h"
#include "surface.h"
#include "weather.h"
#include "crop.h"
#include "csmp.h"
#include "alist.h"
#include "soil.h"
#include "common.h"
#include "syntax.h"
#include "snow.h"
#include "log.h"
#include "filter.h"
#include "mathlib.h"
#include "options.h"

struct BioclimateNew : public Bioclimate
{ 
  // Canopy.
  const long No;		// No of intervals in canopy discretation.
  double MxH;			// Max crop Hieght in canopy [cm].
  double LAI_;			// Total LAI of all crops on this column.
  vector<double> Height;	// Height in cm of each endpoint in c.d.
  vector<double> PAR_;		// PAR of each interval of c.d.
  CSMP LAIvsH;			// LAI below given height [f: cm -> R]
  CSMP HvsLAI;			// Height with LAI below [f: R -> cm]

  // Utilities.
  double CanopySum (const CropList& crops, double (Crop::*fun) () const);
  double CanopyAverage (const CropList& crops, double (Crop::*fun) () const);

  // Tick.
  void CanopyStructure (const CropList&);
  void RadiationDistribution (const Weather&, const CropList&);
  void IntensityDistribution (double Rad0, double Ext, 
				     vector <double>& Rad) const;
  void WaterDistribution (Surface& surface, const Weather& weather, 
			  const CropList& crops, const Soil& soil, 
			  SoilWater& soil_water, const SoilHeat&);
  // Weather.
  double daily_air_temperature_; // Air temperature in canopy.
  double day_length_;		// From weather (does not really belong here).
  double daily_global_radiation_; // From weather.

  // Manager.
  double irrigation;
  double irrigation_temperature;
  Column::irrigation_from irrigation_type;

  // Status.
  double water_temperature;	// Temperature of incomming water.
  double intercepted_water;
  Snow snow;
  
  // Log.
  double PotEvapotranspiration;
  double ActualEvapotranspiration;
  double EvapInterception;
  double net_throughfall;

  // Simulation
  void tick (Surface&, const Weather&, const Time&, 
	     const CropList&, 
	     const Soil&, SoilWater&, const SoilHeat&);
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
  void irrigate (double flux, double temp, 
		 Column::irrigation_from from);
  
  // Communication with external model.
  double get_evap_interception () const // [mm/h]
    { return EvapInterception; }
  double get_intercepted_water () const // [mm]
    { return intercepted_water; }
  double get_net_throughfall () const // [mm/h]
    { return net_throughfall; }
  double get_snow_storage () const // [mm]
    { return snow.get_storage (); }

  // Create.
  BioclimateNew (const AttributeList&);
  ~BioclimateNew ()
    { }
};

BioclimateNew::BioclimateNew (const AttributeList& al)
  : Bioclimate (al.name ("type")),
    No (al.integer ("NoOfIntervals")),
    Height (al.integer ("NoOfIntervals") + 1),
    PAR_ (al.integer ("NoOfIntervals") + 1),
    irrigation (0.0),
    irrigation_temperature (0.0),
    intercepted_water (al.number ("intercepted_water")),
    snow (al.alist ("Snow")),
    PotEvapotranspiration (0.0),
    ActualEvapotranspiration (0.0),
    EvapInterception (0.0),
    net_throughfall (0.0)
{ }

double
BioclimateNew::CanopySum (const CropList& crops, double (Crop::*fun) () const)
{
  if (LAI_ == 0.0)
    return 0.0;

  double value = 0.0;
  
  for (CropList::const_iterator crop = crops.begin();
       crop != crops.end();
       crop++)
    {
      value += ((*crop)->*fun) () * (*crop)->LAI ();
    }
  return value;
}

double
BioclimateNew::CanopyAverage (const CropList& crops,
			      double (Crop::*fun) () const)
{
  if (LAI_ == 0.0)
    return 0.0;

  return CanopySum (crops, fun) / LAI_;
}

void
BioclimateNew::CanopyStructure (const CropList& crops)
  // Calculate values for the total crop canopy.
{

  // Clear old values.
  LAIvsH.clear ();		
  HvsLAI.clear ();
  LAI_ = 0.0;
  MxH = 0.0;
  fill (Height.begin (), Height.end (), 0.0);

  // Calculate LAI and MxH.
  for (CropList::const_iterator crop = crops.begin();
       crop != crops.end();
       crop++)
    {
      if ((*crop)->height () > MxH)
	MxH = (*crop)->height ();
      LAI_ += (*crop)->LAI ();
    }

  // Only calculate LAI distribution, if there is any.
  if (LAI_ == 0.0)
    return;

  // Calculate the total Leaf Area Density as a function of the height
  // above the ground.
  for (CropList::const_iterator crop = crops.begin();
       crop != crops.end();
       crop++)
    {
      if ((*crop)->LAI () > 0.0)
	{
	  (*crop)->CanopyStructure ();
	  LAIvsH += (*crop)->LAIvsH ();
	}
    }
  // There are no leafs below the ground.
  assert (LAIvsH (0.0) == 0.0);
  // All leafs are located below the top of the highest crop.
  assert (approximate (LAI_, LAIvsH (MxH)));

  // Find H as a function of LAI.
  HvsLAI = LAIvsH.inverse ();
  // Check that the end points still match.
  if (!approximate (MxH, HvsLAI (LAI_)))
    CERR << "BUG: MxH = " << MxH 
	 << ", but HvsLAI (LAI) = " << HvsLAI (LAI_) << "\n"; 

  assert (HvsLAI (0.0) == 0.0);
  
  // Count height of each interval.  Interval 0 is the top of the crop
  // and interval "No" is group zero (no bomb intended).
  double dLAI = LAI_ / No;
  for (int i = 0; i <= No; i++)
    Height[i] = HvsLAI ((No - i) * dLAI);

  assert (Height[No] == 0.0);
  //  assert (approximate (Height[0], MxH));
  Height[0] = MxH;
}

void 
BioclimateNew::RadiationDistribution (const Weather& weather, 
				      const CropList& crops)
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
  const double ACExt = CanopyAverage (crops, &Crop::PARext);

  // Average Canopy Reflection coefficient 
  const double ACRef =  CanopyAverage (crops, &Crop::PARref);

#if 0
  // Average Radiation Extinction coefficient
  // (like ACExt, but for all radiation, not just light).
  const double ARExt = CanopyAverage (crops, &Crop::EPext);
#endif 

  const double PAR0 
    = (1 - ACRef) * PARinSi * weather.hourly_global_radiation ();
  IntensityDistribution (PAR0, ACExt, PAR_);
}

void
BioclimateNew::IntensityDistribution (const double Rad0,
				      const double Ext,
				      vector <double>& Rad) const
{
  assert (Rad.size () == No + 1);
  const double dLAI = (LAI_ / No);
    
  for (int i = 0; i <= No; i++)
    Rad[i] = Rad0 * exp (- Ext * dLAI * i);
}

void
BioclimateNew::WaterDistribution (Surface& surface,
				       const Weather& weather, 
				       const CropList& crops,
				       const Soil& soil, 
				       SoilWater& soil_water,
				       const SoilHeat& soil_heat)
{
  static const double dt = 1.0;

  // Calculate total interception.
  const double InterceptionCapacity = CanopySum (crops, &Crop::IntcpCap);
  const double EpExtinction = CanopyAverage (crops, &Crop::EPext);
  double EpFactor = CanopySum (crops, &Crop::EpFac);
  if (LAI_ > 1.0)
    EpFactor /= LAI_;
  else
    EpFactor += (1 - LAI_) * soil.EpFactor ();

  const double ref_evapo = max (0.0, weather.reference_evapotranspiration ());
  
  PotEvapotranspiration = EpFactor * ref_evapo;

  double PotSoilEvaporation = PotEvapotranspiration
    * exp (- EpExtinction * LAI_);
  
  assert (PotSoilEvaporation < 1000.0);
  double PotCanopyEvapotranspiration =
    EpFactor * ref_evapo - PotSoilEvaporation;
  
  double WaterFromAbove = weather.rain ();
  if (irrigation_type == Column::top_irrigation)
    WaterFromAbove += irrigation;

  EvapInterception
    = min (WaterFromAbove + intercepted_water / dt,
	   PotCanopyEvapotranspiration);
  PotCanopyEvapotranspiration -= EvapInterception;

  double Through_fall = WaterFromAbove - EvapInterception
    - min (WaterFromAbove - EvapInterception, 
	   InterceptionCapacity - intercepted_water);

  if (Through_fall < 0.0)
    {
      if (Through_fall < -1e-9)
	CERR << "BUG: Through_fall = " << Through_fall << "\n";
      Through_fall = 0.0;
    }

  intercepted_water += WaterFromAbove - EvapInterception - Through_fall;

  double Total_through_fall = Through_fall;
  
  if (irrigation_type == Column::surface_irrigation)
    Total_through_fall += irrigation;

  double temperature;
  if (Total_through_fall + irrigation > 0.0)
    temperature 
      = (Through_fall * weather.hourly_air_temperature ()
	 + irrigation * irrigation_temperature) / (Through_fall + irrigation);
  else
    temperature = weather.hourly_air_temperature ();

  snow.tick (soil, soil_water, soil_heat, 
	     weather.hourly_global_radiation (), 0.0,
	     Total_through_fall, weather.snow (),
	     temperature, 
	     PotSoilEvaporation + PotCanopyEvapotranspiration);
  assert (PotSoilEvaporation < 1000.0);
  if (snow.evaporation () < PotSoilEvaporation)
    PotSoilEvaporation -= snow.evaporation ();
  else
    {
      PotCanopyEvapotranspiration -= 
	snow.evaporation () - PotSoilEvaporation;
      PotSoilEvaporation = 0;
    }

  net_throughfall = snow.percolation ();

  const double EvapSoilSurface = surface.evaporation (PotSoilEvaporation, 
						      net_throughfall, 
						      temperature,
						      soil, soil_water);

  PotSoilEvaporation -= EvapSoilSurface;

  PotCanopyEvapotranspiration += PotSoilEvaporation * soil.EpInterchange ();

  double TotalCropUptake = 0.0;	// Water uptake by crops.
  
  if (LAI_ > 0.0)
    {
      // Distribute PotCanopyEvapotranspiration on crops.
      const double PotTransPerLAI =  PotCanopyEvapotranspiration / LAI_;
  
      for (CropList::const_iterator crop = crops.begin();
	   crop != crops.end();
	   crop++)
	{
	  TotalCropUptake 
	    += (*crop)->ActualWaterUptake (PotTransPerLAI * (*crop)->LAI (), 
					   soil, soil_water, EvapInterception);
	}
    }
  ActualEvapotranspiration = TotalCropUptake + EvapInterception 
    + EvapSoilSurface + snow.evaporation ();

  irrigation = 0.0;
}

void 
BioclimateNew::tick (Surface& surface, const Weather& weather, 
			  const Time&,
			  const CropList& crops, const Soil& soil, 
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
  CanopyStructure (crops);

  // Calculate total canopy, divide it intervalsm, and distribute PAR.
  RadiationDistribution (weather, crops);

  // Distribute water among canopy, snow, and soil.
  WaterDistribution (surface, weather, crops,
		     soil, soil_water, soil_heat);

}

void 
BioclimateNew::output (Log& log, Filter& filter) const
{
  log.output ("intercepted_water", filter, intercepted_water);
  log.output ("EvapInterception", filter, EvapInterception, true);
  log.output ("net_throughfall", filter, net_throughfall, true);
  log.output ("MxH", filter, MxH, true);
  log.output ("LAI", filter, LAI_, true);
  log.output ("LAIvsH", filter, LAIvsH, true);
  log.output ("HvsLAI", filter, HvsLAI, true);
  log.output ("PotEvapotranspiration", filter,
	      PotEvapotranspiration, true);
  log.output ("ActualEvapotranspiration", filter,
	      ActualEvapotranspiration, true);
  output_submodule (snow, "Snow", log, filter);
}

void
BioclimateNew::irrigate (double flux, double temp, 
		      Column::irrigation_from type)
{
  irrigation = flux;
  irrigation_temperature = temp;
  irrigation_type = type;
}

#ifdef BORLAND_TEMPLATES
template class add_submodule<Snow>;
#endif

static struct BioclimateNewSyntax
{
  static Bioclimate& make (const AttributeList& al)
    { return *new BioclimateNew (al); }
  
  BioclimateNewSyntax ()
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
  
      syntax.add ("NoOfIntervals", Syntax::Integer, Syntax::Const, "\
Number of vertical intervals in which we partition the canopy");
      alist.add ("NoOfIntervals", 30);
      syntax.add ("intercepted_water", "mm", Syntax::State, 
		  "Water intercepted by the canopy");
      alist.add ("intercepted_water", 0.0);
      syntax.add ("EvapInterception", "mm", Syntax::LogOnly,
		  "Intercepted water evaporated from the canopy");
      syntax.add ("net_throughfall", Syntax::Number, Syntax::LogOnly);
      syntax.add ("MxH", "cm", Syntax::LogOnly,
		  "Canopy height");
      syntax.add ("LAI", Syntax::None (), Syntax::LogOnly,
		  "Leaf area index for total canopy");
      syntax.add ("LAIvsH", Syntax::CSMP, Syntax::LogOnly,
		  "Total canopy LAI below given height (cm)");
      syntax.add ("HvsLAI", Syntax::CSMP, Syntax::LogOnly, "\
Height (cm) in which there is a given LAI below in total canopy");
      syntax.add ("PotEvapotranspiration", "mm", Syntax::LogOnly,
		  "Potential evaoptranspiration");
      syntax.add ("ActualEvapotranspiration", "mm", Syntax::LogOnly,
		  "Actual evapotranspiration");
      add_submodule<Snow> ("Snow", syntax, alist, Syntax::State, 
			   "Surface snow pack");

      Librarian<Bioclimate>::add_type ("new", alist, syntax, &make);
    }
} BioclimateNew_syntax;

