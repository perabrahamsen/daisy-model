// snow.C

#include "snow.h"
#include "alist.h"
#include "syntax.h"
#include "log.h"
#include "soil.h"
#include "soil_water.h"
#include "soil_heat.h"
#include "mathlib.h"

struct Snow::Implementation
{ 
  // Flux variables.
  double EvapSnowPack;			// Evaporation from snowpack [mm]
  double q_s;			// Leaking water [mm]
  
  // State variables.
  double Ssnow;			// Snow storage expressed as water [mm]
  double Swater;		// Water in snow storage [mm]
  double age;			// Age since last snow [h]
  double dZs;			// Depth of snow layer [m]

  // Parameters.
  const double mf;		// Snow pack depth melting factor [1/m]
  const double mtprime;		// Air temperature melting factor 
				// [kg/m²/h C]
  const double mrprime;		// Radiation melting factor [kg/J]
  const double m1;		// Radiation melting linear factor [kg/J]
  const double m2;		// Radiation melting exponential factor [1/h]
  const double rho_s;		// Density of newly fallen snow [kg/m³]
  const double f_c;		// Water capacity in snow factor
  const double rho_1;		// Water collapse factor [kg/m³]
  const double rho_2;		// Snow collapse factor [1/m]
  const double Psa;		// Absolute amount of snow required
				// for snow to become new.
  const double fsa;		// Relative amount of snow required
				// for snow to become new.
  const double K_snow_factor;	// Factor related to termal conductivity
				// for snow water mix [W m^4 / Kg²]

  void output (Log& log, Filter& filter) const;
  void tick (const Soil& soil, const SoilWater& soil_water,
	     const SoilHeat& soil_heat,
	     double Si, double q_h, double Prain,
	     double Psnow, double& T, double Epot);
  Implementation (const AttributeList& al);
};

Snow::Implementation::Implementation (const AttributeList& al)
  : EvapSnowPack (0.0),
    q_s (0.0),
    Ssnow (al.number ("Ssnow")),
    Swater (al.number ("Swater")),
    age (al.number ("age")),
    dZs (al.number ("dZs")),
    mf (al.number ("mf")),
    mtprime (al.number ("mtprime")),
    mrprime (al.number ("mrprime")),
    m1 (al.number ("m1")),
    m2 (al.number ("m2")),
    rho_s (al.number ("rho_s")),
    f_c (al.number ("f_c")),
    rho_1 (al.number ("rho_1")),
    rho_2 (al.number ("rho_2")),
    Psa (al.number ("Psa")),
    fsa (al.number ("fsa")),
    K_snow_factor (al.number ("K_snow_factor"))
{ }

void 
Snow::Implementation::output (Log& log, Filter& filter) const
{
  log.output ("EvapSnowPack", filter, EvapSnowPack, true);
  log.output ("q_s", filter, q_s, true);
  log.output ("Ssnow", filter, Ssnow);
  log.output ("Swater", filter, Swater);
  log.output ("age", filter, age);
  log.output ("dZs", filter, dZs);
}

void
Snow::Implementation::tick (const Soil& soil, const SoilWater& soil_water,
			    const SoilHeat& soil_heat,
			    const double Si, const double q_h,
			    const double Prain, const double Psnow,
			    double& T, const double Epot)
{ 
  assert (Si >= 0.0);
  assert (Prain >= 0.0);
  assert (Psnow >= 0.0);
  assert (Epot >= 0.0);
  assert (T > -374 && T <= 100);

  static const double dt = 1.0; // Time step [h].
  static const double f = 1.0;	// Melting factor. [mm H2O / (kg H2O / m²)]
  static const double rho_w = 1.0e3; // Density of water. [kg / m³]
  static const double Lm = 3.34e5; // Snow melting heat factor. [J/kg]

  // Total precpitation. [mm/h]
  const double P = Psnow + Prain;
  
  // Relative amount of snow in percolation.
  const double fs = (P > 0) ? Psnow / P : 0.0;
  assert (fs >= 0.0);

  // Check if snow has become white.
  if (Psnow > Psa && fs > fsa) 
    age = 0;

  // We evaporate as much of the water as we can.  If we can evaporate
  // more than that, evaporate all the snow too.
  if (Epot <= Swater / dt + P)
    EvapSnowPack = Epot;
  else 
    EvapSnowPack = Ssnow / dt + P;

  assert (EvapSnowPack >= 0.0);
  assert (EvapSnowPack <= Epot);
  assert (EvapSnowPack <= (Ssnow / dt + P) * 1.0001);

  // Depth of snow fallen this hour. [m]
  double dZp = 0.0;
  if (Psnow > 0.0)
    {
      // Density of snow-rain mixture. [kg/m³]
      const double rho_p = rho_w + (rho_s - rho_w) * Psnow / P;
      assert (rho_p >= 0.0);
      dZp = P * dt / (f * rho_p);
    }
  assert (dZp >= 0.0);

  // Air temperature melting factor. [kg/J]
  const double mt = mtprime * ((T < 0.0) ? min (1.0, (dZs + dZp) * mf) : 1);
  
  // Radiation melting factor. [kg/J]
  const double mr = mrprime * (1 + m1 * (1 - exp (-m2 * age)));

  // Potential snow melting. [mm/h]
  const double Mprime = (mt * T + mr * Si + q_h / Lm) * f;

  // Minimal possible melting (all water freezes). [mm/h]
  const double M1 = - (Swater/dt + Prain);
  
  // Maximal possible melting (all ice melts). [mm/h]
  const double M2 = (Ssnow - Swater)/dt + Psnow;

  // Actual melting. [mm/h]
  const double M = min (max (M1, Mprime), M2);

  // Evaporation from snow pack. [mm/h]
  double Eprime;
  if (EvapSnowPack <= Ssnow / dt + Prain + M)
    Eprime = EvapSnowPack;
  else
    Eprime = Swater / dt + Prain + M;
  assert (Eprime >= 0.0);
  assert (Eprime <= EvapSnowPack);
  
  // Water storage capacity of snow [mm]
  const double Scapacity = f_c * Ssnow;
  assert (Scapacity >= 0.0);

  // We can now calculate how much water is leaking.
  q_s = max (0.0, Swater + (Prain - EvapSnowPack + M) * dt - Scapacity) / dt;
  assert (q_s >= 0.0);
  
  // New snow pack storage [mm].
  double Ssnow_new = Ssnow + (Psnow + Prain - EvapSnowPack - q_s) * dt;
  if (Ssnow_new < 0.0)
    {
      // cerr << "Lost " << -Ssnow_new << " mm from snow pack.\n";
      Ssnow_new = 0.0;
    }
  
  // New water content in snow pack [mm].
  double Swater_new = Swater + (Prain - Eprime + M - q_s) * dt;
  if (Swater_new < 0.0)
    {
      // cerr << "Lost " << -Swater_new << " mm water from snow pack.\n";
      Swater_new = 0.0;
    }
  if (Swater_new > Ssnow_new)
    {
      // cerr << "Removed " << Swater_new - Ssnow_new << " mm water.\n";
      Swater_new = Ssnow_new;
    }

  // Update the snow height,
  if (Ssnow_new > 0.0)
    {
      if (dZs > 0.0)
	{
	  // Density of collapsing snow pack [kg/m³]
	  assert (Scapacity > 0.0);
	  const double rho_c
	    = max (Ssnow / (f * dZs), 
		   rho_s + rho_1 * Swater_new / Scapacity + rho_2 * Ssnow);
	  assert (rho_c > 0.0);
	  assert (approximate (rho_c, Ssnow / (f * dZs))
		  || approximate (rho_c, rho_s 
				  + rho_1 * Swater_new / Scapacity 
				  + rho_2 * Ssnow));
	  // Size of collapsed snow pack [m]
	  const double dZc = Ssnow / (f * rho_c) + dZp;

	  // Factor in collapsing from passing melting water.
	  dZs = dZc * Ssnow_new / (Ssnow_new + q_s * dt);
	}
      else
	dZs = dZp;
    }
  else
    dZs = 0.0;

  dZs = min (dZs, Ssnow_new / rho_s);

  // Update snow storage.
  Ssnow = Ssnow_new;
  Swater = Ssnow_new;

  // Update snow age.
  if (Ssnow > 0.0)
    age++;

  // Update temperature.
  if (q_s > 1.0e-20)
    // There is water leaking through the snow pack.  
    // Assume it is 0 degrees.
    T = 0.0;
  else if (dZs > 0.01)
    {
      // Information about soil.
      const double Theta = soil_water.Theta (0);
      const double K_soil = soil.K (0, Theta);
      const double Z = soil.z (0);
      const double T_soil = soil_heat.T (0);

      // Density and conductivity of snowpack.
      const double rho = rho_w * Ssnow / dZs; // Bug?
      const double K_snow = K_snow_factor * rho * rho;
      
      T = min ((K_soil / Z * T_soil + K_snow / dZs * T) 
	       / (K_soil / Z + K_snow / dZs),	       
	       0.0);
      assert (T > -100.0 && T < 50.0);
    } 
}
  
void 
Snow::tick (const Soil& soil, const SoilWater& soil_water,
	    const SoilHeat& soil_heat,
	    double Si, double q_h, double Prain,
	    double Psnow, double& T, double Epot)
{
  if (impl.Ssnow > 0.0 || Psnow > 0.0)
    impl.tick (soil, soil_water, soil_heat, Si, q_h, Prain, Psnow, T, Epot);
  else
    {
      impl.EvapSnowPack = 0.0;
      impl.q_s = Prain;
    }
}

void 
Snow::output (Log& log, Filter& filter) const
{
  impl.output (log, filter);
}

double 
Snow::percolation ()
{
  return impl.q_s;
}

double 
Snow::evaporation ()
{
  return impl.EvapSnowPack;
}

double 
Snow::get_storage () const
{
  return impl.Ssnow;
}

void
Snow::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  static const double hours_per_day = 24.0; // [h/d]
  
  syntax.add ("EvapSnowPack", Syntax::Number, Syntax::LogOnly);
  syntax.add ("q_s", Syntax::Number, Syntax::LogOnly);
  syntax.add ("Ssnow", Syntax::Number, Syntax::State);
  alist.add ("Ssnow", 0.0);
  syntax.add ("Swater", Syntax::Number, Syntax::State);
  alist.add ("Swater", 0.0);
  syntax.add ("age", Syntax::Number, Syntax::State);
  alist.add ("age", 0.0);
  syntax.add ("dZs", Syntax::Number, Syntax::State);
  alist.add ("dZs", 0.0);
  syntax.add ("mf", Syntax::Number, Syntax::Const);
  alist.add ("mf", 10.0);
  syntax.add ("mtprime", Syntax::Number, Syntax::Const);
  alist.add ("mtprime", 2.0 / hours_per_day);
  syntax.add ("mrprime", Syntax::Number, Syntax::Const);
  alist.add ("mrprime", 1.5e-7);
  syntax.add ("m1", Syntax::Number, Syntax::Const);
  alist.add ("m1", 2.0);
  syntax.add ("m2", Syntax::Number, Syntax::Const);
  alist.add ("m2", 0.1 / hours_per_day);
  syntax.add ("rho_s", Syntax::Number, Syntax::Const);
  alist.add ("rho_s", 100.0);
  syntax.add ("f_c", Syntax::Number, Syntax::Const);
  alist.add ("f_c", 0.07);
  syntax.add ("rho_1", Syntax::Number, Syntax::Const);
  alist.add ("rho_1", 200.0);
  syntax.add ("rho_2", Syntax::Number, Syntax::Const);
  alist.add ("rho_2", 0.5);
  syntax.add ("Psa", Syntax::Number, Syntax::Const);
  alist.add ("Psa", 200.0);
  syntax.add ("fsa", Syntax::Number, Syntax::Const);
  alist.add ("fsa", 0.5);
  syntax.add ("K_snow_factor", Syntax::Number, Syntax::Const);
  alist.add ("K_snow_factor", 2.86e6);
}
  
Snow::Snow (const AttributeList& al)
  : impl (*new Implementation (al))
{ }

Snow::~Snow ()
{ }
