// snow.C

#include "snow.h"
#include "alist.h"
#include "syntax.h"

struct Snow::Implementation
{ 
  // Flux variables.
  double Esnow;			// Evaporation from snowpack [mm]
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
  : Esnow (0.0),
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
  static const double dt = 1.0; // Time step [h].
  static const double f = 1.0;	// Melting factor. [mm H2O / (kg H2O / m^2)]
  static const double rho_w = 10e3; // Density of water. [kg / m^3]
  static const double Lm = 3.34e5; // Snow melting heat factor. [J/kg]

  // Total precpitation. [mm/h]
  const double P = Psnow + Prain;
  
  // Relative amount of snow in percolation.
  const double fs = Psnow / P;
  // Check if snow has become white.
  if (Psnow > Psa && fs > fsa) 
    age = 0;

  // We evaporate as much of the water as we can.  If we can evaporate
  // more than that, evaporate all the snow too.
  if (Epot <= Swater / dt + P)
    Esnow = Epot;
  else 
    Esnow = Ssnow / dt + P;

  // Density of snow pack. [kg/m^3]
  const double rho_p = rho_w + (rho_s + rho_w) * Psnow / P;

  // Depth of snow fallen this hour. [m]
  const double dZp = P * dt / (f * rho_p);

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
  if (Esnow <= Ssnow / dt + Prain + M)
    Eprime = Esnow;
  else
    Eprime = Swater / dt + Prain + M;
  
  // Water storage capacity of snow [mm]
  const double Scapacity = f_c * (Ssnow + (P - Esnow) / dt);

  // We can now calculate how much water is leaking.
  q_s = max (0.0, Swater + (Prain - Esnow + M) * dt - Scapacity) / dt;
  
  // New snow pack storage [mm].
  const double Ssnow_new = Ssnow + (Psnow + Prain - Esnow - q_s) * dt;
  
  // Density of collapsing snow pack [kg/m^3]
  const double rho_c
    = max (Ssnow_new / (f * dZs), 
	   rho_s + rho_1 * Ssnow_new / Scapacity + rho_2 * Ssnow);
  
  // Size of collapsed snow pack [m]
  const double dZs_c = Ssnow / (f * rho_c);

  // Density of snow pack with new content [kg/m^3]
  // BUG: Ask SH what it really should be.
  dZs = dZs_c + (P - Esnow) * rho_s;

  // Update snow storage.
  Ssnow = Ssnow_new;
  Swater += (Prain - Eprime + M - q_s) * dt;

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
  impl.tick (Si, q_h, Prain, Psnow, Tair, Epot);
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
  return impl.Esnow;
}

void
Snow::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  static const double hours_per_day = 24.0; // [h/d]
  
  syntax.add ("Esnow", Syntax::Number, Syntax::LogOnly);
  syntax.add ("q_s", Syntax::Number, Syntax::LogOnly);
  syntax.add ("Ssnow", Syntax::Number);
  alist.add ("Ssnow", 0.0);
  syntax.add ("Swater", Syntax::Number);
  alist.add ("Swater", 0.0);
  syntax.add ("age", Syntax::Number);
  alist.add ("age", 0.0);
  syntax.add ("dZs", Syntax::Number);
  alist.add ("dZs", 0.0);
  syntax.add ("mf", Syntax::Number);
  alist.add ("mf", 10.0);
  syntax.add ("mtprime", Syntax::Number);
  alist.add ("mtprime", 2.0 / hours_per_day);
  syntax.add ("mrprime", Syntax::Number);
  alist.add ("mrprime", 1.5e-7);
  syntax.add ("m1", Syntax::Number);
  alist.add ("m1", 2.0);
  syntax.add ("m2", Syntax::Number);
  alist.add ("m2", 0.1 / hours_per_day);
  syntax.add ("rho_s", Syntax::Number);
  alist.add ("rho_s", 100.0);
  syntax.add ("f_c", Syntax::Number);
  alist.add ("f_c", 0.07);
  syntax.add ("rho_1", Syntax::Number);
  alist.add ("rho_1", 200.0);
  syntax.add ("rho_2", Syntax::Number);
  alist.add ("rho_2", 0.5);
  syntax.add ("Psa", Syntax::Number);
  alist.add ("Psa", 200.0);
  syntax.add ("fsa", Syntax::Number);
  alist.add ("fsa", 0.5);
}
  
Snow::Snow (const AttributeList& al)
  : impl (*new Implementation (al))
{ }

Snow::~Snow ()
{ }
