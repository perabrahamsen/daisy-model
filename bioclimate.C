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
#include "soil_water.h"


class Bioclimate::Implementation
{
  // Canopy.
public:
  const long No;		// No of intervals in canopy discretation.
  vector<double> Height;	// Height in cm of each endpoint in c.d.
  vector<double> PAR;		// PAR of each interval of c.d.
  double LAI;			// Total LAI of all crops on this column.
  void RadiationDistribution (const Weather&, const CropList&);
private:
  void IntensityDistribution (double Rad0, double Ext, 
			      vector <double>& Rad) const;
public:
  double PotTransPerLAI;	// Potential evapotranspiration / total LAI.

  // Weather.
public:
  double temperature;		// Air temperature in canopy.
  double day_length;		// From weather (does not really belong here).

  // Manager.
  double irrigation;
  double irrigation_temperature;
  irrigation_from irrigation_type;

  // Status.
  double intercepted_water;
  Snow snow;
  
  // Construct.
  Implementation (const AttributeList& al);
};

Bioclimate::Implementation::Implementation (const AttributeList& al)
  : No (al.integer ("NoOfIntervals")),
    Height (al.integer ("NoOfIntervals")),
    PAR (al.integer ("NoOfIntervals")),
    irrigation (0.0),
    irrigation_temperature (0.0),
    intercepted_water (al.number ("intercepted_water")),
    snow (al.list ("Snow"))
{ }

void 
Bioclimate::Implementation::RadiationDistribution (const Weather& weather, 
						   const CropList& crops)
{
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
  ARExt /= LAI;

  // Calculate the total Leaf Area Density as a function of the height
  // above the ground.
  CSMP LAIvsH;		
  for (CropList::const_iterator crop = crops.begin();
       crop != crops.end();
       crop++)
    {
      (*crop)->CanopyStructure ();
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
    
  for (int i = 0; i < No; i++)
    Rad[i] = Rad0 * exp (- Ext * dLAI * i);
}

void 
Bioclimate::tick (Surface& surface, const Weather& weather, 
		  const CropList& crops, const Soil& soil, 
		  const SoilWater& soil_water)
{
  // Keep weather information during time step.
  impl.temperature = weather.AirTemperature ();
  impl.day_length = weather.DayLength ();

  // Calculate total canopy, divide it intervalsm, and distribute PAR.
  impl.RadiationDistribution (weather, crops);

  // Calculate total interception.
  double InterceptionCapacity = 0.0;
  double EpExtinction = 0.0;
  double EpFactor = 0.0;

  if (impl.LAI > 0.0)
    {
      for (CropList::const_iterator crop = crops.begin();
	   crop != crops.end();
	   crop++)
	{
	  InterceptionCapacity += (*crop)->IntcpCap () * (*crop)->LAI ();
	  EpExtinction += (*crop)->EPext () * (*crop)->LAI ();
	  EpFactor += (*crop)->EpFac () * (*crop)->LAI ();
	}
      EpExtinction /= impl.LAI;
      if (impl.LAI > 1.0)
	EpFactor /= impl.LAI;
      else
	EpFactor += (1 - impl.LAI) * soil.EpFactor ();
    }
  else
    EpFactor = soil.EpFactor ();

  double PotSoilEvaporation = 
      EpFactor
    * weather.ReferenceEvapotranspiration () 
    * exp (- EpExtinction * impl.LAI);
  
  double PotCanopyEvapotranspiration =
    EpFactor * weather.ReferenceEvapotranspiration () - PotSoilEvaporation;
  
  double WaterFromAbove = weather.Rain ();
  if (impl.irrigation_type == top_irrigation)
    WaterFromAbove += impl.irrigation;

  const double Evaporation = min (WaterFromAbove, PotCanopyEvapotranspiration);
  PotCanopyEvapotranspiration -= Evaporation;

  const double Through_fall = WaterFromAbove - Evaporation
    - min (WaterFromAbove - Evaporation, 
	   InterceptionCapacity - impl.intercepted_water);

  impl.intercepted_water += WaterFromAbove - Evaporation - Through_fall;

  double Total_through_fall = Through_fall;
  
  if (impl.irrigation_type == surface_irrigation)
    Total_through_fall += impl.irrigation;

  double temperature;
  if (Total_through_fall > 0.0)
    temperature 
      = (Through_fall * weather.AirTemperature ()
	 + impl.irrigation * impl.irrigation_temperature) / Total_through_fall;
  else
    temperature = weather.AirTemperature ();

  impl.snow.tick (weather.GlobalRadiation (), 0.0,
		  Total_through_fall, weather.Snow (),
		  temperature, 
		  PotSoilEvaporation + PotCanopyEvapotranspiration);
  
  if (impl.snow.evaporation () < PotSoilEvaporation)
    PotSoilEvaporation -= impl.snow.evaporation ();
  else
    {
      PotCanopyEvapotranspiration -= 
	impl.snow.evaporation () - PotSoilEvaporation;
      PotSoilEvaporation = 0;
    }
  PotSoilEvaporation -= 
    surface.evaporation (PotSoilEvaporation, 
			 impl.snow.percolation (),
			 // cm -> mm
			 soil_water.MaxExfiltration (soil) / 10);

  PotCanopyEvapotranspiration += PotSoilEvaporation * soil.EpInterchange ();
  impl.PotTransPerLAI = PotCanopyEvapotranspiration / impl.LAI;
}

int
Bioclimate::NumberOfIntervals(void) const
{
  return impl.No;
}

double
Bioclimate::height(int i) const
{
  return impl.Height[i];
}

double
Bioclimate::PAR(int i) const
{
  return impl.PAR[i];
}

double
Bioclimate::AirTemperature(void) const
{
  return impl.temperature;
}

double
Bioclimate::DayLength(void) const
{
  return impl.day_length;
}

double
Bioclimate::PotTransPerLAI () const
{
  return impl.PotTransPerLAI;
}

void
Bioclimate::Irrigate (double flux, double temp, irrigation_from type)
{
  impl.irrigation = flux;
  impl.irrigation_temperature = temp;
  impl.irrigation_type = type;
}

void
Bioclimate::load_syntax (Syntax& syntax, AttributeList& alist)
{
  
  syntax.add ("NoOfIntervals", Syntax::Integer);
  syntax.add ("intercepted_water", Syntax::Number);
  alist.add ("intercepted_water", 0.0);
  ADD_SUBMODULE (syntax, alist, Snow);
}

Bioclimate::Bioclimate (const AttributeList& al)
  : impl (*new Implementation (al))
{ }

Bioclimate::~Bioclimate ()
{ }
