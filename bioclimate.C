// bioclimate.C

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
#include "mike_she.h"

class Bioclimate::Implementation
{
  // Canopy.
public:
  const long No;		// No of intervals in canopy discretation.
  double LAI;			// Total LAI of all crops on this column.
  vector<double> Height;	// Height in cm of each endpoint in c.d.
  vector<double> PAR;		// PAR of each interval of c.d.
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
  
  // Construct.
  Implementation (const AttributeList& al);
};

Bioclimate::Implementation::Implementation (const AttributeList& al)
  : No (al.integer ("NoOfIntervals")),
    Height (al.integer ("NoOfIntervals")),
    PAR (al.integer ("NoOfIntervals") + 1),
    irrigation (0.0),
    irrigation_temperature (0.0),
    intercepted_water (al.number ("intercepted_water")),
    snow (al.list ("Snow")),
    PotEvapotranspiration (0.0),
    ActualEvapotranspiration (0.0),
    EvapInterception (0.0)
{ }

void 
Bioclimate::Implementation::RadiationDistribution (const Weather& weather, 
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
  LAI = 0.0;
  for (CropList::const_iterator crop = crops.begin();
       crop != crops.end();
       crop++)
    {
      if ((*crop)->height () > MxH)
	MxH = (*crop)->height ();
      LAI += (*crop)->LAI ();
      ACExt += (*crop)->PARext () * (*crop)->LAI ();
      ACRef += (*crop)->PARref () * (*crop)->LAI ();
      ARExt += (*crop)->EPext ()  * (*crop)->LAI ();
    }

  // If we haven't got a canopy, there is nothing more to calculate.
  if (LAI == 0.0)
    return;

  // Calculate averages.
  ACExt /= LAI;
  ACRef /= LAI;
  // ARExt /= LAI;

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
  assert (fabs (LAI - LAIvsH (MxH)) < LAI / 10000);

  CSMP HvsLAI = LAIvsH.inverse ();

  double dLAI = LAI / No;

  for (int i = 0; i < No; i++)
    {
      Height[i] = HvsLAI ((i + 1) * dLAI);
    }

  double PAR0 = (1 - ACRef) * PARinSi * weather.GlobalRadiation ();
  IntensityDistribution (PAR0, ACExt, PAR);
}

void
Bioclimate::Implementation::IntensityDistribution (const double Rad0,
						   const double Ext,
						   vector <double>& Rad) const
{
  double dLAI = (LAI / No);
    
  for (int i = 0; i <= No; i++)
    Rad[i] = Rad0 * exp (- Ext * dLAI * i);
}

void
Bioclimate::Implementation::WaterDistribution (Surface& surface,
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

  if (LAI > 0.0)
    {
      for (CropList::const_iterator crop = crops.begin();
	   crop != crops.end();
	   crop++)
	{
	  InterceptionCapacity += (*crop)->IntcpCap () * (*crop)->LAI ();
	  EpExtinction += (*crop)->EPext () * (*crop)->LAI ();
	  EpFactor += (*crop)->EpFac () * (*crop)->LAI ();
	}
      EpExtinction /= LAI;
      if (LAI > 1.0)
	EpFactor /= LAI;
      else
	EpFactor += (1 - LAI) * soil.EpFactor ();
    }
  else
    EpFactor = soil.EpFactor ();

  const double ref_evapo = max (0.0, weather.ReferenceEvapotranspiration ());
  
  PotEvapotranspiration = EpFactor * ref_evapo;

  double PotSoilEvaporation = PotEvapotranspiration
    * exp (- EpExtinction * LAI);
  
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
  
  if (LAI > 0.0)
    {
      // Distribute PotCanopyEvapotranspiration on crops.
      const double PotTransPerLAI =  PotCanopyEvapotranspiration / LAI;
  
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

#ifdef MIKE_SHE
  mike_she->put_evap_interception (EvapInterception);
  mike_she->put_intercepted_water (intercepted_water);
  mike_she->put_net_precipitation (Total_through_fall);
#endif
}

void 
Bioclimate::tick (Surface& surface, const Weather& weather, 
		  const Time& time,
		  const CropList& crops, const Soil& soil, 
		  SoilWater& soil_water, const SoilHeat& soil_heat)
{
  // Keep weather information during time step.
  impl.temperature = weather.AirTemperature ();
  impl.day_length = weather.DayLength (time);

  // Add nitrogen deposit. 
  surface.fertilize (weather.Deposit ());

  // Calculate total canopy, divide it intervalsm, and distribute PAR.
  impl.RadiationDistribution (weather, crops);

  // Distribute water among canopy, snow, and soil.
  impl.WaterDistribution (surface, weather, crops,
			  soil, soil_water, soil_heat);

}

void 
Bioclimate::output (Log& log, Filter& filter) const
{
  log.output ("intercepted_water", filter, impl.intercepted_water);
  log.output ("EvapInterception", filter, impl.EvapInterception, true);
  log.output ("LAI", filter, impl.LAI, true);
  log.output ("PotEvapotranspiration", filter,
	      impl.PotEvapotranspiration, true);
  log.output ("ActualEvapotranspiration", filter,
	      impl.ActualEvapotranspiration, true);
  output_submodule (impl.snow, "Snow", log, filter);
}

int
Bioclimate::NumberOfIntervals () const
{
  return impl.No;
}

double
Bioclimate::height (int i) const
{
  return impl.Height[i];
}

double
Bioclimate::PAR (int i) const
{
  return impl.PAR[i];
}

double
Bioclimate::AirTemperature () const
{
  return impl.temperature;
}

double
Bioclimate::DayLength () const
{
  return impl.day_length;
}

double
Bioclimate::DailyRadiation () const
{
  return impl.daily_radiation;
}

void
Bioclimate::irrigate (double flux, double temp, 
		      Column::irrigation_from type)
{
  impl.irrigation = flux;
  impl.irrigation_temperature = temp;
  impl.irrigation_type = type;
}

#ifdef BORLAND_TEMPLATES
template class add_submodule<Snow>;
#endif

void
Bioclimate::load_syntax (Syntax& syntax, AttributeList& alist)
{
  
  syntax.add ("NoOfIntervals", Syntax::Integer, Syntax::Const);
  alist.add ("NoOfIntervals", 30);
  syntax.add ("intercepted_water", Syntax::Number, Syntax::State);
  syntax.add ("EvapInterception", Syntax::Number, Syntax::LogOnly);
  syntax.add ("LAI", Syntax::Number, Syntax::LogOnly);
  syntax.add ("PotEvapotranspiration", Syntax::Number, Syntax::LogOnly);
  syntax.add ("ActualEvapotranspiration", Syntax::Number, Syntax::LogOnly);
  alist.add ("intercepted_water", 0.0);
  add_submodule<Snow> ("Snow", syntax, alist);
}

Bioclimate::Bioclimate (const AttributeList& al)
  : impl (*new Implementation (al))
{ }

Bioclimate::~Bioclimate ()
{ }
