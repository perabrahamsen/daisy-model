// bioclimate_std.C

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

class BioclimateStandard : public Bioclimate
{ 
  // Canopy.
public:
  const long No;		// No of intervals in canopy discretation.
  double LAI_;			// Total LAI of all crops on this column.
  vector<double> Height;	// Height in cm of each endpoint in c.d.
  vector<double> PAR_;		// PAR of each interval of c.d.
  void RadiationDistribution (const Weather&, const CropList&);
private:
  void IntensityDistribution (double Rad0, double Ext, 
			      vector <double>& Rad) const;
public:
  void WaterDistribution (Surface& surface, const Weather& weather, 
			  const CropList& crops, const Soil& soil, 
			  SoilWater& soil_water, const SoilHeat&);
  // Weather.
public:
  double temperature;		// Air temperature in canopy.
  double day_length;		// From weather (does not really belong here).
  double daily_radiation;	// From weather.

  // Manager.
  double irrigation;
  double irrigation_temperature;
  Column::irrigation_from irrigation_type;

  // Status.
  double intercepted_water;
  Snow snow;
  
  // Log.
  double PotEvapotranspiration;
  double ActualEvapotranspiration;
  double EvapInterception;

public:
  // Simulation
  void tick (Surface&, const Weather&, const Time&, 
	     const CropList&, 
	     const Soil&, SoilWater&, const SoilHeat&);
  void output (Log&, Filter&) const;

  // Canopy.
public:
  int NumberOfIntervals () const
    { return No; }
  double height (int i) const
    { return Height[i]; }
  double PAR (int i) const
    { return PAR_[i]; }
  double LAI () const
    { return LAI_; }

  // Weather.
public:
  double AirTemperature () const
    { return temperature; }
  double DayLength () const
    { return day_length; }
  double DailyRadiation () const
    { return daily_radiation; }

  // Manager.
public:
  void irrigate (double flux, double temp, 
		 Column::irrigation_from from);
  
  // Create.
public:
  BioclimateStandard (const AttributeList&);
  ~BioclimateStandard ()
    { }
};

BioclimateStandard::BioclimateStandard (const AttributeList& al)
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
    EvapInterception (0.0)
{ }

void 
BioclimateStandard::RadiationDistribution (const Weather& weather, 
					   const CropList& crops)
{
  // Remember this in case the crops should ask.
  daily_radiation = weather.DailyRadiation ();

  // Fraction of Photosynthetically Active Radiation in Shortware
  // incomming radiation. 
  static const double PARinSi = 0.50;	

  double MxH = 0.0;		// Max crop Hieght in canopy [cm].
  double ACExt = 0.0;		// Average Canopy Extinction coefficient
  // (how fast the light dim as a
  //  function of LAI passed).
  double ACRef = 0.0;		// Average Canopy Reflection coefficient 
  double ARExt = 0.0;		// Average Radiation Extinction coefficient
  // (like ACExt, but for all radiation,
  //  not just light).

  // Calculate values for the total crop canopy.
  LAI_ = 0.0;
  for (CropList::const_iterator crop = crops.begin();
       crop != crops.end();
       crop++)
    {
      if ((*crop)->height () > MxH)
	MxH = (*crop)->height ();
      LAI_ += (*crop)->LAI ();
      ACExt += (*crop)->PARext () * (*crop)->LAI ();
      ACRef += (*crop)->PARref () * (*crop)->LAI ();
      ARExt += (*crop)->EPext ()  * (*crop)->LAI ();
    }

  // If we haven't got a canopy, there is nothing more to calculate.
  if (LAI_ == 0.0)
    return;

  // Calculate averages.
  ACExt /= LAI_;
  ACRef /= LAI_;
  // ARExt /= LAI_;

  // Calculate the total Leaf Area Density as a function of the height
  // above the ground.
  CSMP LAIvsH;		
  for (CropList::const_iterator crop = crops.begin();
       crop != crops.end();
       crop++)
    {
      (*crop)->CanopyStructure ();
      if ((*crop)->LAI () > 0.0)
	LAIvsH += (*crop)->LAIvsH ();
    }
  // There are no leafs below the ground.
  assert (LAIvsH (0.0) == 0.0);
  // All leafs are located below the top of the highest crop.
  assert (approximate (LAI_, LAIvsH (MxH)));

  // Find H as a function of LAI.
  CSMP HvsLAI = LAIvsH.inverse ();
  // Check that the end points still match.
  assert (approximate (MxH, HvsLAI (LAI_)));
  assert (HvsLAI (0.0) == 0.0);
  
  // Count height of each interval.  Interval 0 is the top of the crop
  // and interval "No" is group zero (no bomb intended).
  double dLAI = LAI_ / No;
  for (int i = 0; i <= No; i++)
    Height[i] = HvsLAI ((No - i) * dLAI);

  assert (Height[No] == 0.0);
  assert (approximate (Height[0], MxH));
  Height[0] = MxH;

  double PAR0 = (1 - ACRef) * PARinSi * weather.GlobalRadiation ();
  IntensityDistribution (PAR0, ACExt, PAR_);
}

void
BioclimateStandard::IntensityDistribution (const double Rad0,
					   const double Ext,
					   vector <double>& Rad) const
{
  double dLAI = (LAI_ / No);
    
  for (int i = 0; i <= No; i++)
    Rad[i] = Rad0 * exp (- Ext * dLAI * i);
}

void
BioclimateStandard::WaterDistribution (Surface& surface,
				       const Weather& weather, 
				       const CropList& crops,
				       const Soil& soil, 
				       SoilWater& soil_water,
				       const SoilHeat& soil_heat)
{
  static const double dt = 1.0;

  // Calculate total interception.
  double InterceptionCapacity = 0.0;
  double EpExtinction = 0.0;
  double EpFactor = 0.0;

  if (LAI_ > 0.0)
    {
      for (CropList::const_iterator crop = crops.begin();
	   crop != crops.end();
	   crop++)
	{
	  InterceptionCapacity += (*crop)->IntcpCap () * (*crop)->LAI ();
	  EpExtinction += (*crop)->EPext () * (*crop)->LAI ();
	  EpFactor += (*crop)->EpFac () * (*crop)->LAI ();
	}
      EpExtinction /= LAI_;
      if (LAI_ > 1.0)
	EpFactor /= LAI_;
      else
	EpFactor += (1 - LAI_) * soil.EpFactor ();
    }
  else
    EpFactor = soil.EpFactor ();

  const double ref_evapo = max (0.0, weather.ReferenceEvapotranspiration ());
  
  PotEvapotranspiration = EpFactor * ref_evapo;

  double PotSoilEvaporation = PotEvapotranspiration
    * exp (- EpExtinction * LAI_);
  
  assert (PotSoilEvaporation < 1000.0);
  double PotCanopyEvapotranspiration =
    EpFactor * ref_evapo - PotSoilEvaporation;
  
  double WaterFromAbove = weather.Rain ();
  if (irrigation_type == Column::top_irrigation)
    WaterFromAbove += irrigation;

  EvapInterception
    = min (WaterFromAbove + intercepted_water / dt, PotCanopyEvapotranspiration);
  PotCanopyEvapotranspiration -= EvapInterception;

  const double Through_fall = WaterFromAbove - EvapInterception
    - min (WaterFromAbove - EvapInterception, 
	   InterceptionCapacity - intercepted_water);

  intercepted_water += WaterFromAbove - EvapInterception - Through_fall;

  double Total_through_fall = Through_fall;
  
  if (irrigation_type == Column::surface_irrigation)
    Total_through_fall += irrigation;

  double temperature;
  if (Total_through_fall > 0.0)
    temperature 
      = (Through_fall * weather.AirTemperature ()
	 + irrigation * irrigation_temperature) / (Through_fall + irrigation);
  else
    temperature = weather.AirTemperature ();

  snow.tick (soil, soil_water, soil_heat, 
	     weather.GlobalRadiation (), 0.0,
	     Total_through_fall, weather.Snow (),
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
  const double EvapSoilSurface = surface.evaporation (PotSoilEvaporation, 
						      snow.percolation (), 
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
}

void 
BioclimateStandard::tick (Surface& surface, const Weather& weather, 
			  const Time&,
			  const CropList& crops, const Soil& soil, 
			  SoilWater& soil_water, const SoilHeat& soil_heat)
{
  // Keep weather information during time step.
  temperature = weather.AirTemperature ();
  day_length = weather.DayLength ();

  // Add nitrogen deposit. 
  surface.fertilize (weather.Deposit ());

  // Calculate total canopy, divide it intervalsm, and distribute PAR.
  RadiationDistribution (weather, crops);

  // Distribute water among canopy, snow, and soil.
  WaterDistribution (surface, weather, crops,
			  soil, soil_water, soil_heat);

}

void 
BioclimateStandard::output (Log& log, Filter& filter) const
{
  log.output ("intercepted_water", filter, intercepted_water);
  log.output ("EvapInterception", filter, EvapInterception, true);
  log.output ("LAI", filter, LAI_, true);
  log.output ("PotEvapotranspiration", filter,
	      PotEvapotranspiration, true);
  log.output ("ActualEvapotranspiration", filter,
	      ActualEvapotranspiration, true);
  output_submodule (snow, "Snow", log, filter);
}

void
BioclimateStandard::irrigate (double flux, double temp, 
		      Column::irrigation_from type)
{
  irrigation = flux;
  irrigation_temperature = temp;
  irrigation_type = type;
}

#ifdef BORLAND_TEMPLATES
template class add_submodule<Snow>;
#endif

static struct BioclimateStandardSyntax
{
  static Bioclimate& make (const AttributeList& al)
    { return *new BioclimateStandard (al); }
  
  BioclimateStandardSyntax ()
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
  
      syntax.add ("NoOfIntervals", Syntax::Integer, Syntax::Const);
      alist.add ("NoOfIntervals", 30);
      syntax.add ("intercepted_water", Syntax::Number, Syntax::State);
      syntax.add ("EvapInterception", Syntax::Number, Syntax::LogOnly);
      syntax.add ("LAI", Syntax::Number, Syntax::LogOnly);
      syntax.add ("PotEvapotranspiration", Syntax::Number, Syntax::LogOnly);
      syntax.add ("ActualEvapotranspiration", Syntax::Number, Syntax::LogOnly);
      alist.add ("intercepted_water", 0.0);
      add_submodule<Snow> ("Snow", syntax, alist);

      Librarian<Bioclimate>::add_type ("default", alist, syntax, &make);
    }
} BioclimateStandard_syntax;

