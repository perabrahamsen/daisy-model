// bioclimate.C

#include "bioclimate.h"
#include "weather.h"

Librarian<Bioclimate>::Content* Librarian<Bioclimate>::content = NULL;

const char *const Bioclimate::description = "\
The 'bioclimate' component is responsible for distributing the water\n\
and energy provided by the weather component among the crops and soil\n\
for a given column.";

double 
Bioclimate::CanopyResistance (double LAI)
{ 
  assert (LAI > 0.0);
  return (200 / LAI); 
}

double 
Bioclimate::RefCanopyResistance (void)
{ return 70.0; }

double 
Bioclimate::ZeroPlaneDisplacement (double CropHeight)
{ return (0.66 * CropHeight / 100.0); }

double 
Bioclimate::RoughnessHeight_Momentum (double CropHeight)
{ return (0.123 * CropHeight / 100.0); }

double
Bioclimate:: RoughnessHeight_Heat (double CropHeight)
{ return (0.0123 * CropHeight / 100.0); }

double 
Bioclimate::AerodynamicResistance (double CropHeight, double ScreenHeight,
				   double U)
{
  if (U == 0.0)
    U = 0.1;

  assert (U > 0.0);

  double Zom, Zoh;

  if (CropHeight <= 0) {
    Zom = 0.01;
    Zoh = 0.001;
  } else {
    Zom = RoughnessHeight_Momentum (CropHeight);
    Zoh = RoughnessHeight_Heat (CropHeight);
  }
  const double Z = ScreenHeight - ZeroPlaneDisplacement (CropHeight);
  return (log (Z / Zom) * log (Z / Zoh) / (0.41 * 0.41 * U));
}

double 
Bioclimate::RefAerodynamicResistance (double U2)
{ 
  if (U2 == 0.0)
    U2 = 0.1;

  assert (U2 > 0.0);

  return (208 / U2); 
}

double 
Bioclimate::get_evap_interception () const
{ assert (false); return 0.0; }

double 
Bioclimate::get_intercepted_water () const
{ assert (false); return 0.0; }

double 
Bioclimate::get_net_throughfall () const
{ assert (false); return 0.0; }

double 
Bioclimate::get_snow_storage () const
{ assert (false); return 0.0; }

Bioclimate::Bioclimate (const string& n)
  : name (n)
{ }

Bioclimate::~Bioclimate ()
{ }
