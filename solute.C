// solute.C

#include "solute.h"
#include "log.h"
#include "filter.h"
#include "syntax.h"
#include "alist.h"
#include "soil.h"
#include "soil_water.h"
#include "mathlib.h"
#include "common.h"

void
Solute::clear ()
{
  fill (S.begin (), S.end (), 0.0);
}

void
Solute::add_to_source (const vector<double>& v)
{
  assert (S.size () == v.size ());
  for (unsigned i = 0; i < S.size (); i++)
    {
      S[i] += v[i];
      // Check for NaN.
      assert (S[i] >= 0.0 || S[i] <= 0.0);
    }
}

void
Solute::add_to_sink (const vector<double>& v)
{
  assert (S.size () == v.size ());
  for (unsigned i = 0; i < S.size (); i++)
    {
      S[i] -= v[i];
      // Check for NaN.
      assert (S[i] >= 0.0 || S[i] <= 0.0);
    }
}

void 
Solute::tick (const Soil& soil, const SoilWater& soil_water, const double J_in)
{
  // Constants.
  const int size = soil.size (); // Number of soil layers.

  for (int i = 0; i < size; i++)
    {
      assert (C_[i] >= 0.0);
      assert (M_[i] >= 0.0);
      if (C_[i] == 0.0)
	assert (M_[i] == 0.0);
      else
#if 0
	assert (abs (M_[i] / C_to_M (soil, soil_water.Theta_old (i), i, C_[i])
		     - 1.0) < 0.001);
#else
      if (!(abs (M_[i] / C_to_M (soil, soil_water.Theta_old (i), i, C_[i])
		     - 1.0) < 0.001))
	cerr << i << ":M [" << M_[i] << "] != C_to_M (C) [" 
	     << C_to_M (soil, soil_water.Theta_old (i), i, C_[i]) << "]\n";
#endif
    }

  // Note: q, D, and alpha depth indexes are all [j-½].

  // Dispersion coefficient [cm²/s]
  vector<double> D (size + 1);

#ifdef CALCULATE_FLUX_FLOW
  vector<double> Jf (size, 0.0);
#endif

  for (int j = 0; j < size; j++)
    {
      const int prev = (j < 1) ? 0 : j - 1;

      // Dispersion length [cm]
      const double lambda = soil.lambda (j);

      // Water flux [cm³ /cm² / h]
      const double q = soil_water.q (j);
      
      // Theta middled in time and space.
      const double Theta
	= (soil_water.Theta (j) + soil_water.Theta (prev)
	   + soil_water.Theta_old (j) + soil_water.Theta_old (prev)) / 4.0;

      // From equation 7-39:
      D[j] = (lambda * abs (-q / Theta)
	      + soil.tortuosity_factor (j, Theta) * diffusion_coefficient ())
	* Theta;
      
      // Check for NaN.
      assert (D[j] >= 0.0 || D[j] <= 0.0);
    }
  D[size] = D[size-1];

  // Weight factor (how important is this flux for the concentration)
  // This is 1 for incomming flux and 0 for outgoing flux.
  vector<double> alpha (size + 1);

  for (int j = 0; j < size + 1; j++)
    {
#if 1
      if (soil_water.q (j) < 0.0)
	alpha[j] = 1.0;
      else
	alpha[j] = 0.0;
#else
      alpha[j] = 0.5;
#endif
    }

  // Find the time step using Courant.
  ddt = 1.0;
  for (int i = 0; i < size; i++)
    ddt = min (ddt, pow (soil.dz (i), 2) / (2 * D[i + 1]));
  
  // Loop through small time steps.
  for (double old_t = 0.0, t = ddt; 
       old_t != t;
       old_t = t, t = min (dt, t + ddt))
    {
      // Parameters for tridiagonal matrix.
      vector<double> a (size);
      vector<double> b (size);
      vector<double> c (size);
      vector<double> d (size);
  
      for (int j = 0; j < size; j++)
	{
	  // dA/dC in the present soil water solute.
	  const double beta = this->beta (soil, soil_water, j, C_[j]);
      
	  const double dz_minus	// Size of layer above current node.
	    = (j < 1) ? - 2.0 * soil.z (j) : soil.z (j-1) - soil.z (j);

	  const double dz_plus	// Size of layer below current node.
	    = (j == size - 1) ? dz_minus : (soil.z (j) - soil.z (j+1));

	  const double dz = soil.dz (j); // Size of current node.
	  const double q_minus = soil_water.q (j); // Flow to above.
	  const double q_plus = soil_water.q (j+1);	// Flow from below.
	  const double alpha_minus = alpha[j]; // Direction above.
	  const double alpha_plus = alpha[j+1]; // Direction below.
	  const double D_minus = D[j]; // Dispertion above.
	  const double D_plus = D[j+1]; // Dispertion below.
	  const double Theta_ratio 
	    = (soil_water.Theta (j) - soil_water.Theta_old (j)) / dt;
	  const double Theta_new // New water content.
	    = soil_water.Theta_old (j) + Theta_ratio * t;
	  const double Theta_old // Old water content.
	    = soil_water.Theta_old (j) + Theta_ratio * old_t;
      
	  // Concentration above and below current node.
	  const double C_minus = (j < 1) ? C_[j] : C_[j-1];
	  const double C_plus = (j == size - 1) ? C_[j] : C_[j+1];
      
	  a[j] = - D_minus / (2.0 * dz_minus * dz) 
	    + (alpha_minus * q_minus) / (2.0 * dz);
	  b[j] = ((Theta_new + beta) / (t - old_t)
		  + D_minus / (2.0 * dz_minus * dz)
		  + D_plus / (2.0 * dz_plus * dz)
		  + ((1 - alpha_minus) * q_minus) / (2.0 * dz)
		  - (alpha_plus * q_plus) / (2.0 * dz));
	  c[j] = - D_plus / (2.0 * dz_plus * dz)
	    - ((1.0 - alpha_plus) * q_plus) / (2.0 * dz);
	  d[j] = ((Theta_old + beta) * C_[j] / (t - old_t)
		  + S[j]
		  + ((D_minus * (C_minus - C_[j])) / (2.0 * dz_minus * dz))
		  - ((D_plus * (C_[j] - C_plus)) / (2.0 * dz_plus * dz))
		  - (q_minus * (alpha_minus * C_minus + (1.0 - alpha_minus) * C_[j])
		     / (2.0 * dz))
		  + (q_plus * (alpha_plus * C_[j] + (1.0 - alpha_plus) * C_plus)
		     / (2.0 * dz)));

	  // Check for NaN.
	  assert (a[j] >= 0.0 || a[j] <= 0.0);
	  assert (b[j] >= 0.0 || b[j] <= 0.0);
	  assert (c[j] >= 0.0 || c[j] <= 0.0);
	  assert (d[j] >= 0.0 || d[j] <= 0.0);
	}
      // Adjust for upper boundary condition.
#if 0
      const double dz_minus = - 2.0 * soil.z (0);
      const double b_in 
	= (D[0] / dz_minus + soil_water.q (0) * alpha[0]) * 0.5
	/ (D[0] / dz_minus + soil_water.q (0) * (1 - alpha[0]));
      const double d_in 
	= (- J_in + (D[0] / dz_minus + soil_water.q (0) * alpha[0]) * 0.5 * C_[0])
	/ (D[0] / dz_minus + soil_water.q (0) * (1 - alpha[0]));
      d[0] -= a[0] * d_in;
      b[0] += a[0] * b_in;
      a[0] = 42.42e42;
#elif 1
      const double dz_x = soil.z (0) - soil.z(1);
      const double Dz = - D[0] / (2 * dz_x);
      const double q0 = soil_water.q (0);
      
      const double d_x = J_in - (1 - alpha[0]) * C_[0] * q0 / 2.0 + Dz * C_[0];
      const double a_x = alpha[0] * q0 + 2 * Dz;
      const double b_x = (1 - alpha[0]) * q0 / 2.0 - Dz;
      d[0] -= a[0] * d_x / a_x;
      b[0] -= a[0] * b_x / a_x;
      a[0] = 42.42e42;
#else
      {
	const int j = 0;
	const double beta = this->beta (soil, soil_water, j, C_[j]);
	const double dz_plus	// Size of layer below current node.
	  = soil.z (j) - soil.z (j+1);
	const double dz = soil.dz (j); // Size of current node.
	const double q_plus = soil_water.q (j+1);	// Flow from below.
	const double alpha_plus = alpha[j+1]; // Direction below.
	const double D_plus = D[j+1]; // Dispertion below.
	const double Theta_ratio 
	  = (soil_water.Theta (j) - soil_water.Theta_old (j)) / dt;
	const double Theta_new // New water content.
	  = soil_water.Theta_old (j) + Theta_ratio * t;
	const double Theta_old // Old water content.
	  = soil_water.Theta_old (j) + Theta_ratio * old_t;

	const double C_plus = (j == size - 1) ? C_[j] : C_[j+1];

	a[0] = 42.42e42;
	b[0] = ((Theta_new + beta) / (t - old_t)
		+ D_plus / (2.0 * dz_plus * dz)
		- (alpha_plus * q_plus) / (2.0 * dz));
	d[0] = ((Theta_old + beta) * C_[j] / (t - old_t)
		+ S[j]
		- J_in / dz
		- ((D_plus * (C_[j] - C_plus)) / (2.0 * dz_plus * dz))
		+ (q_plus * (alpha_plus * C_[j] + (1.0 - alpha_plus) * C_plus) 
		   / (2.0 * dz)));
      }
#endif
      // Adjust for lower boundary condition.
      b[size - 1] += c[size - 1];
      c[size - 1] = -42.42e42;

#ifdef CALCULATE_FLUX_FLOW
      const vector<double> C_old = C_;
#endif

      // Calculate new concentration.
      tridia (0, size, a, b, c, d, C_.begin ());

#ifdef CALCULATE_FLUX_FLOW
      Jf[0] += J_in * (t - old_t);
      for (int i = 0; i < size - 1; i++)
	{
	  const double C_minus = (C_[i] + C_old[i]) / 2.0;
	  const double C_plus = (C_[i+1] + C_old[i+1]) / 2.0;
	  const double dz_plus = soil.z (i) - soil.z (i+1);
	  const double q_plus = soil_water.q (i + 1);
	  const double alpha_plus = alpha[i+1];
	  const double D_plus = D[i+1];
	  Jf[i + 1] += ((D_plus * (C_plus - C_minus) / dz_plus 
			 + q_plus
			 * (alpha_plus * C_minus + (1 - alpha_plus) * C_plus)) 
			* (t - old_t));
	}
#endif

    }
  // Calculate flux with mass conservation.
  J[0] = J_in;
  for (int i = 0; i < size; i++)
    {
      const double M_old = M_[i];
      assert (C_[i] >= 0.0);
      M_[i] = C_to_M (soil, soil_water.Theta (i), i, C_[i]);
      assert (M_[i] >= 0.0);
      J[i + 1] = (((M_[i] - M_old) / dt) - S[i]) * soil.dz (i) + J[i];
    }
#ifdef CALCULATE_FLUX_FLOW
  S = Jf;
#endif

#if 0
  static double t = 0;

  if (abs (J[0]) > 0.1e-7)
    {
      t += dt;
      const double C0 =  J[0] / soil_water.q (0);
      for (int i = 0; i < size; i++)
	{
	  const double v = - soil_water.q (i) / soil_water.Theta (i);
	  const double z = - soil.z (i);
	  const double d = D[i] / soil_water.Theta (i);
	  S[i] = (C0 / 2.0) * (erfc ((z - v * t) / (2.0 * sqrt (d * t)))
			       + exp (v * z / d)
			       * erfc ((z + v * t) / (2.0 * sqrt (d * t))));
	}
    }
  else
    {
      assert (t < 0.1e-7);
    }
#endif
}

bool 
Solute::check (unsigned) const
{
  return true;
}

void
Solute::output (Log& log, const Filter& filter) const
{
  log.output ("C", filter, C_);
  log.output ("M", filter, M_);
  log.output ("S", filter, S, true);
  log.output ("J", filter, J, true);
  log.output ("ddt", filter, ddt, true);
}

void 
Solute::load_syntax (Syntax& syntax, AttributeList&)
{
  syntax.add ("C", Syntax::Number, Syntax::Optional, Syntax::Sequence);
  syntax.add ("M", Syntax::Number, Syntax::Optional, Syntax::Sequence);
  syntax.add ("S", Syntax::Number, Syntax::LogOnly, Syntax::Sequence);
  syntax.add ("J", Syntax::Number, Syntax::LogOnly, Syntax::Sequence);
  syntax.add ("ddt", Syntax::Number, Syntax::LogOnly, Syntax::Singleton);
}

Solute::Solute ()
  : ddt (1.0)
{ }

void 
Solute::add (const Soil& soil, const SoilWater& soil_water, 
	     double amount, double from, double to)
{ 
  soil.add (M_, from, to, amount);
  for (unsigned int i = 0; i < C_.size (); i++)
    C_[i] = M_to_C (soil, soil_water.Theta (i), i, M_[i]);
}

void 
Solute::mix (const Soil& soil, const SoilWater& soil_water, 
	     double from, double to)
{ 
  soil.mix (M_, from, to);
  for (unsigned int i = 0; i < C_.size (); i++)
    C_[i] = M_to_C (soil, soil_water.Theta (i), i, M_[i]);
}

void 
Solute::swap (const Soil& soil, const SoilWater& soil_water,
	      double from, double middle, double to)
{ 
  soil.swap (M_, from, middle, to);
  for (unsigned int i = 0; i < C_.size (); i++)
    C_[i] = M_to_C (soil, soil_water.Theta (i), i, M_[i]);
}

void
Solute::initialize (const Soil& soil, const SoilWater& soil_water,
		    const AttributeList& al)
{
  if (al.check ("C"))
    {
      C_ = al.number_sequence ("C");
      if (C_.size () == 0U)
	C_.push_back (0.0);
      // Fill it up.
      if (C_.size () < soil.size () +0U)
	C_.insert (C_.end (), soil.size () - C_.size (), C_[C_.size () - 1]);
      else if (C_.size () > soil.size () +0U)
	THROW ("To many members of C sequence");
    }
  if (al.check ("M"))
    {
      M_ = al.number_sequence ("M");
      if (M_.size () == 0U)
	M_.push_back (0.0);
      // Fill it up.
      if (M_.size () < soil.size () + 0U)
	M_.insert (M_.end (), soil.size () - M_.size (), M_[M_.size () - 1]);
      else if (M_.size () > soil.size () + 0U)
	THROW ("To many members of M sequence");
    }
  if (!al.check ("C") && !al.check ("M"))
    {
      C_.insert (C_.begin (), soil.size (), 0.0);
      M_.insert (M_.begin (), soil.size (), 0.0);
    }
  else if (!al.check ("C"))
    for (int i = 0; i < soil.size (); i++)
      C_.push_back (M_to_C (soil, soil_water.Theta (i), i, M_[i]));
  else if (!al.check ("M"))
    for (int i = 0; i < soil.size (); i++)
      M_.push_back (C_to_M (soil, soil_water.Theta (i), i, C_[i]));

  for (int i = 0; i < soil.size (); i++)
    {
      if (C_[i] == 0.0)
	{
	  if (M_[i] != 0.0)
	    THROW ("C & M mismatch in solute");
	}
      else
	{
	  if (abs (M_[i] / C_to_M (soil, soil_water.Theta (i), i, C_[i]) 
		   - 1.0) >= 0.001)
	    THROW ("Solute C does not match M");
	}
    }

  S.insert (S.begin (), soil.size (), 0.0);
  J.insert (J.begin (), soil.size () + 1, 0.0);
}

Solute::~Solute ()
{ }
