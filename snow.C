// snow.C

#include "snow.h"
#include "alist.h"
#include "syntax.h"
#include "log.h"
#include "filter.h"

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
  double T;			// Temperature [C]

  // Parameters.
  const double mf;		// Snow pack depth melting factor [1/m]
  const double mtprime;		// Air temperature melting factor 
				// [kg/m^2/h C]
  const double mrprime;		// Radiation melting factor [kg/J]
  const double m1;		// Radiation melting linear factor [kg/J]
  const double m2;		// Radiation melting exponential factor [1/h]
  const double rho_s;		// Density of newly fallen snow [kg/m^3]
  const double f_c;		// Water capacity in snow factor
  const double rho_1;		// Water collapse factor [kg/m^3]
  const double rho_2;		// Snow collapse factor [1/m]
  const double Psa;		// Absolute amount of snow required
				// for snow to become new.
  const double fsa;		// Relative amount of snow required
				// for snow to become new.
  
  void tick (double Si, double q_h, double Prain,
	     double Psnow, double Tair, double Epot);
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
    fsa (al.number ("fsa"))
{ }

void
Snow::Implementation::tick (const double Si, const double q_h,
			    const double Prain, const double Psnow,
			    const double Tair, const double Epot)
{ 
  assert (Si >= 0.0);
  assert (Prain >= 0.0);
  assert (Psnow >= 0.0);
  assert (Epot >= 0.0);
  assert (Tair > -374 && Tair <= 100);

  static const double dt = 1.0; // Time step [h].
  static const double f = 1.0;	// Melting factor. [mm H2O / (kg H2O / m^2)]
  static const double rho_w = 10e3; // Density of water. [kg / m^3]
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
  assert (EvapSnowPack <= Ssnow / dt + P);

  // Depth of snow fallen this hour. [m]
  double dZp = 0.0;
  if (Psnow > 0.0)
    {
      // Density of snow pack. [kg/m^3]
      const double rho_p = rho_w + (rho_s - rho_w) * Psnow / P;
      assert (rho_p >= 0.0);
      dZp = P * dt / (f * rho_p);
    }
  assert (dZp >= 0.0);

  // Air temperature melting factor. [kg/J]
  const double mt = mtprime * ((Tair < 0.0) ? min (1.0, (dZs + dZp) * mf) : 1);
  
  // Radiation melting factor. [kg/J]
  const double mr = mrprime * (1 + m1 * (1 - exp (-m2 * age)));

  // Potential snow melting. [mm/h]
  const double Mprime = (mt * Tair + mr * Si + q_h / Lm) * f;

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
  const double Ssnow_new = Ssnow + (Psnow + Prain - EvapSnowPack - q_s) * dt;
  assert (Ssnow_new >= 0.0);

  // New water content in snow pack [mm].
  const double Swater_new = Swater + (Prain - Eprime + M - q_s) * dt;
  assert (Swater_new >= 0.0);
  assert (Swater_new <= Ssnow_new);

  // Update the snow height,
  if (Ssnow_new > 0.0)
    {
      if (dZs > 0.0)
	{
	  // Density of collapsing snow pack [kg/m^3]
	  assert (Scapacity > 0.0);
	  const double rho_c
	    = max (Ssnow / (f * dZs), 
		   rho_s + rho_1 * Swater_new / Scapacity + rho_2 * Ssnow);
	  assert (rho_c > 0.0);

	  // Size of collapsed snow pack [m]
	  const double dZc = (Ssnow + dZp) / (f * rho_c);

	  // Factor in collapsing from passing melting water.
	  dZs = dZc * Ssnow_new / (Ssnow_new + q_s);
	}
      else
	dZs = dZp;
    }
  else
    dZs = 0.0;

  assert (dZs >= dZp);
  assert (dZs <= Ssnow_new / rho_s);

  // Update snow storage.
  Ssnow = Ssnow_new;
  Swater = Ssnow_new;

  // Update snow age.
  if (Ssnow > 0.0)
    age++;

  // Update temperature.
  T = Tair;			// BUG: This *must* be wrong. Ask SH.
}
  
void 
Snow::tick (double Si, double q_h, double Prain,
	    double Psnow, double Tair, double Epot)
{
  if (impl.Ssnow > 0.0 || Psnow > 0.0)
    impl.tick (Si, q_h, Prain, Psnow, Tair, Epot);
  else
    {
      impl.EvapSnowPack = 0.0;
      impl.q_s = Prain;
      impl.T = Tair;
    }
}

void 
Snow::output (Log& log, const Filter& filter) const
{
  log.output ("EvapSnowPack", filter, impl.EvapSnowPack, true);
  log.output ("q_s", filter, impl.q_s, true);
  log.output ("Ssnow", filter, impl.Ssnow);
  log.output ("Swater", filter, impl.Swater);
  log.output ("age", filter, impl.age);
  log.output ("dZs", filter, impl.dZs);
  log.output ("T", filter, impl.T);
}

double 
Snow::percolation ()
{
  return impl.q_s;
}

double 
Snow::temperature ()
{
  if (impl.Ssnow > 1.0e-6)
    return 0.0;
  else
    return impl.T;
}

double 
Snow::evaporation ()
{
  return impl.EvapSnowPack;
}

void
Snow::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  static const double hours_per_day = 24.0; // [h/d]
  
  syntax.add ("EvapSnowPack", Syntax::Number, Syntax::LogOnly);
  syntax.add ("q_s", Syntax::Number, Syntax::LogOnly);
  syntax.add ("Ssnow", Syntax::Number, Syntax::InOut);
  alist.add ("Ssnow", 0.0);
  syntax.add ("Swater", Syntax::Number, Syntax::InOut);
  alist.add ("Swater", 0.0);
  syntax.add ("age", Syntax::Number, Syntax::InOut);
  alist.add ("age", 0.0);
  syntax.add ("dZs", Syntax::Number, Syntax::InOut);
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
}
  
Snow::Snow (const AttributeList& al)
  : impl (*new Implementation (al))
{ }

Snow::~Snow ()
{ }
