// bioclimate.C

#include "bioclimate.h"
#include "surface.h"
#include "weather.h"
#include "crop.h"
#include "csmp.h"
#include "alist.h"

#define exception BUG_exception
#include <math.h>
#undef exception

class Bioclimate::Implementation
{
  // Canopy.
public:
  const long No;		// No of intervals in canopy discretation.
  vector<double> Height;	// Height in cm of each endpoint in c.d.
  vector<double> PAR;		// PAR of each interval of c.d.
  double LAI;			// Total LAI of all crops on this column.
  void MainCropGrowthModel (Surface&, const Weather&, const CropList&);
private:
  void IntensityDistribution (double Rad0, double Ext, 
			      vector <double>& Rad) const;
  // Weather.
public:
  double temperature;		// Air temperature in canopy.
  double day_length;		// From weather (does not really belong here).

  // Construct.
  Implementation (int);
};

Bioclimate::Implementation::Implementation (int n)
  : No (n),
    Height (n),
    PAR (n)
{ }

void 
Bioclimate::Implementation::MainCropGrowthModel (Surface& /* face */,
						 const Weather& weather, 
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

#if 0
  IntensityDistribution (PotentialTranspiration (weather),
			 ARExt, PTr);
#endif
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
		  const CropList& crops)
{
  // Calculate total canopy, divide it intervalsm, and distribute PAR.
  impl.MainCropGrowthModel (surface, weather, crops);
  // Keep weather information.
  impl.temperature = weather.AirTemperature ();
  impl.day_length = weather.DayLength ();
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

Bioclimate::Bioclimate (const AttributeList& par, 
			const AttributeList& /* var */)
  : impl (*new Implementation (par.integer ("NoOfIntervals")))
{ }

Bioclimate::~Bioclimate ()
{ }
