// pet_PM.C -- Potential evopotranspiration using Penman-Monteith.

#include "pet.h"
#include "weather.h"
#include "soil.h"
#include "surface.h"
#include "soil_heat.h"
#include "bioclimate.h"
#include "net_radiation.h"
#include "vegetation.h"
#include "log.h"

class PetPM : public Pet
{
public:
  // FAO utilities.
  static double ETaero (double AtmPressure /* [Pa] */,
			double Temp /* [dg C] */,
			double ea /* [Pa] */,
			double ra /* [s/m] */,
			double rc); // [kg/m2/s]
  static double RefETaero (double AtmPressure /* [Pa] */,
			   double Temp /* [dg C] */,
			   double ea /* [Pa] */,
			   double U2);// [kg/m2/s]
  static double ETrad (double AtmPressure /* [Pa] */,
		       double Temp /* [dg C] */,
		       double Rn /* [W/m2] */,
		       double G /* [W/m2] */,
		       double ra /* [s/m] */,
		       double rc); // [kg/m2/s]
  static double PenmanMonteith (double CropHeight /* [m] */,
				double LAI /* [m^2/m^2] */,
				double Rn /* [W/m2] */,
				double G /* [W/m2] */,
				double Temp /* [dg C] */,
				double ea /* [Pa] */,
				double U2 /* [m/s] */,
				double AtmPressure); // [kg/m2/s]
  static double RefPenmanMonteith (double Rn /* [W/m2/] */,
				   double G /* [W/m2] */,
				   double Temp /* [dg C] */,
				   double ea /* [Pa] */,
				   double U2 /* [m/s] */,
				   double AtmPressure); // [kg/m2/s]
  static double RefPenmanMonteithWet (double Rn /* [W/m2] */,
				      double G /* [W/m2] */,
				      double Temp /* [dg C] */,
				      double ea /* [Pa] */,
				      double U2 /* [m/s] */,
				      double AtmPressure); // [kg/m2/s]


  // State.
  double reference_evapotranspiration_wet;
  double reference_evapotranspiration_dry;
  double potential_evapotranspiration_wet;
  double potential_evapotranspiration_dry;

  // Net radiation.
  NetRadiation& net_radiation;

  // Simulation.
  void tick (const Weather& weather, const Vegetation& crops,
	     const Surface& surface, const Soil& soil,
	     const SoilHeat& soil_heat, const SoilWater& soil_water);

  void output (Log& log) const
    {
      Pet::output (log);
      output_derived (net_radiation, "net_radiation", log);
    }

  double wet () const
    {
      return potential_evapotranspiration_wet;
    }

  double dry () const
    {
      return potential_evapotranspiration_dry;
    }

  // Create & Destroy.
  PetPM (const AttributeList& al)
    : Pet (al),
      net_radiation (Librarian<NetRadiation>::create
		     (al.alist ("net_radiation")))
    { }
  ~PetPM ()
    {
      delete &net_radiation;
    }
};

double
PetPM::ETaero (double AtmPressure, double Temp, double ea, double ra,
	       double rc)
{
  const double x1 = Weather::SlopeVapourPressureCurve (Temp) +
    Weather::PsychrometricConstant (AtmPressure, Temp) * (1 + rc / ra);
  const double x2 = Weather::AirDensity (AtmPressure, Temp) * 1.013 /
    Weather::LatentHeatVaporization (Temp);
  const double x3 = (Weather::SaturationVapourPressure (Temp) - ea) / ra;
  return (1.0 / x1) * x2 * x3;
}

double
PetPM::RefETaero (double AtmPressure, double Temp, double ea, double U2)
{
  double x1 = Weather::SlopeVapourPressureCurve (Temp)
    / Weather::PsychrometricConstant (AtmPressure, Temp);
  x1 = 1 / (x1 + 1 + 0.34 * U2);
  const double x2 = 0.9 / (Temp + 273) * U2
    * (Weather::SaturationVapourPressure (Temp) - ea);
  return (x1 * x2) / 86400.0; // [kg/m2/s]
}

double
PetPM::ETrad (double AtmPressure, double Temp, double Rn, double G,
	      double ra, double rc)
{
  double x1 = Weather::SlopeVapourPressureCurve (Temp)
    / Weather::PsychrometricConstant (AtmPressure, Temp);
  x1 /= x1 + 1 + rc / ra;
  const double x2 = (Rn - G) / Weather::LatentHeatVaporization (Temp);
  return (x1 * x2);
}

double
PetPM::PenmanMonteith (double CropHeight, double LAI, double Rn,
		       double G, double Temp, double ea, double U2,
		       double AtmPressure)
{
  const double ra = Bioclimate::AerodynamicResistance (CropHeight, 2.0, U2);
  const double rc = Bioclimate::CanopyResistance (LAI);
  const double E1 = ETrad (AtmPressure, Temp, Rn, G, ra, rc);
  const double E2 = ETaero (AtmPressure, Temp, ea, ra, rc);
  return (E1 + E2);
}

double
PetPM::RefPenmanMonteith (double Rn, double G, double Temp, double ea,
			  double U2, double AtmPressure)
{
  double E3 = 0.03525 * Weather::SlopeVapourPressureCurve (Temp) * (Rn - G) +
    Weather::PsychrometricConstant (AtmPressure, Temp)
    * 0.9 / (Temp + 273) * U2 *
    (Weather::SaturationVapourPressure (Temp) - ea);
  E3 /= Weather::SlopeVapourPressureCurve (Temp) +
    Weather::PsychrometricConstant (AtmPressure, Temp) * (1 + 0.34 * U2);
  return E3 / 86400.0; // [kg/m2/s]
}

double
PetPM::RefPenmanMonteithWet (double Rn, double G, double Temp, double ea,
			     double U2, double AtmPressure)
{
  const double ra = Bioclimate::RefAerodynamicResistance (U2);
  const double rc = 0.0;
  const double E1 = ETrad (AtmPressure, Temp, Rn, G, ra, rc);
  const double E2 = ETaero (AtmPressure, Temp, ea, ra, rc);
  return (E1 + E2);
}

void
PetPM::tick (const Weather& weather, const Vegetation& crops,
	     const Surface& surface, const Soil& soil,
	     const SoilHeat& soil_heat, const SoilWater& soil_water)
{
  // Weather.
  const double Cloudiness = weather.hourly_cloudiness ();
  const double Temp = weather.hourly_air_temperature ();
  const double VaporPressure = weather.vapor_pressure ();
  const double Si = weather.hourly_global_radiation ();
  const double U2 = weather.wind ();
  const double AtmPressure = weather.AtmosphericPressure ();

  // Albedo.
  const double LAI = crops.LAI ();
  double Albedo;
  if (LAI > 0.0)
    {
      const double crop_cover = crops.cover ();
      Albedo = crops.albedo () * crop_cover
	+ surface.albedo (soil, soil_water) * (1.0 - crop_cover);
    }
  else
    Albedo =  surface.albedo (soil, soil_water);

  // Net Radiation.
  net_radiation.tick (Cloudiness, Temp, VaporPressure, Si, Albedo);
  const double Rn = net_radiation.net_radiation ();

  // Ground heat flux.
  const double G = soil_heat.top_flux (soil, soil_water);


  if (LAI > 0.0)
    {
      const double CropHeight = 0.01 * crops.height (); //cm -> m

      // Dry.
      reference_evapotranspiration_dry
	= PenmanMonteith (CropHeight, LAI, Rn, G, Temp, VaporPressure,
			  U2, AtmPressure);
      potential_evapotranspiration_dry
	= max (0.0, reference_evapotranspiration_dry);

      // Wet.
      const double ra
	= Bioclimate::AerodynamicResistance (CropHeight, 2.0, U2);
      const double rc = 0.0;
      const double E1 = ETrad (AtmPressure, Temp, Rn, G, ra, rc);
      const double E2 = ETaero (AtmPressure, Temp, VaporPressure, ra, rc);
      reference_evapotranspiration_wet = E1 + E2;
      potential_evapotranspiration_wet
	= max (0.0, reference_evapotranspiration_wet);

    }
  else
    {
      const double reference_evapotranspiration_dry
	= RefPenmanMonteith (Rn, G, Temp, VaporPressure, U2, AtmPressure);

      potential_evapotranspiration_dry
	= reference_to_potential (crops, surface,
				  reference_evapotranspiration_dry);

      const double reference_evapotranspiration_wet
	= RefPenmanMonteith (Rn, G, Temp, VaporPressure, U2, AtmPressure);
      potential_evapotranspiration_wet
	= reference_to_potential (crops, surface,
				  reference_evapotranspiration_wet);
    }
}

static struct PetPMSyntax
{
  static Pet&
  make (const AttributeList& al)
    { return *new PetPM (al); }
  PetPMSyntax ()
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description",
		 "Potential evopotranspiration using Penman-Monteith.");
      Pet::load_syntax (syntax, alist);
      Syntax Rn_syntax;
      AttributeList Rn_alist;
      Rn_alist.add ("type", "brunt");
      syntax.add ("net_radiation", Librarian<NetRadiation>::library (),
		  "Net radiation.");
      alist.add ("net_radiation", Rn_alist);
      Librarian<Pet>::add_type ("PM", alist, syntax, &make);
    }
} PetPM_syntax;
