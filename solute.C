// solute.C

#include "solute.h"
#include "log.h"
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
Solute::tick (const Soil& soil, 
	      const SoilWater& soil_water, 
	      double J_in)
{
#ifdef MIKE_SHE
  S[0] -= J_in / soil.dz (0);
  for (int i = 0; i < soil.size (); i++)
    {
      M_[i] += S[i];
      C_[i] = M_to_C (soil, soil_water.Theta (i), i, M_[i]);
      J[i] = -42.42e42;
    }
#else
  // Remember old values.
  vector<double> C_prev = C_;
  vector<double> M_prev = M_;

  // Constants.
  const int size = soil.size (); // Number of soil layers.

  // Check that incomming C and M makes sense.
  for (int i = 0; i < size; i++)
    {
      assert (C_[i] >= 0.0);
      assert (M_[i] >= 0.0);
      if (C_[i] == 0.0)
	assert (M_[i] == 0.0);
      else
	assert (approximate (M_[i], 
			     C_to_M (soil,
				     soil_water.Theta_old (i), i, C_[i])));
    }

  // Note: q, D, and alpha depth indexes are all [j-½].

  // Dispersion coefficient [cm²/s]
  vector<double> D (size + 1);

  for (int j = 1; j < size; j++)
    {
      // Dispersion length [cm]
      const double lambda = soil.lambda (j);

      // Water flux [cm³ /cm² / h]
      const double q = soil_water.q (j);
      
      // Theta middled in time and space.
      const double Theta = 
	(soil_water.Theta (j) + soil_water.Theta (j-1)
	 + soil_water.Theta_old (j) + soil_water.Theta_old (j-1)) / 4.0;
      // From equation 7-39:
      D[j] = (lambda * fabs (-q / Theta)
	      + soil.tortuosity_factor (j, Theta) * diffusion_coefficient ())
	* Theta;

      // Check for NaN.
      assert (finite (D[j]));
    }
  // Lower boundary.
  {
    // Dispersion length [cm]
    const double lambda = soil.lambda (size-1);

    // Water flux [cm³ /cm² / h]
    const double q = soil_water.q (size);
      
    // Theta middled in time and space.
    const double Theta = 
      (soil_water.Theta (size - 1) + soil_water.Theta_old (size  - 1)) / 2.0;
    // From equation 7-39:
    D[size] = (lambda * fabs (-q / Theta)
	       + soil.tortuosity_factor (size-1, Theta) 
	       * diffusion_coefficient ())
      * Theta;

  }
  // Upper boundary (no dispersion over soil surface).
  D[0] = 0.0;

  // Weight factor (how important is this flux for the concentration)
  // This is 1 for incomming flux and 0 for outgoing flux.
  vector<double> alpha (size + 1);

  for (int j = 0; j < size + 1; j++)
    {
      if (soil_water.q (j) < 0.0)
	alpha[j] = 1.0;
      else
	alpha[j] = 0.0;
    }

  const double dz_top = 0 - soil.z (0);
  // Or: - 2.0 * soil.z (0)
  // Or: soil.z (0) - soil.z(1)

  double C_top = 0.0;
  if (J_in > 0.0)
    {
      cerr << "\nBug: Positive J_in (" << J_in << ")\n";
      J_in = 0.0;
    }
  if (J_in != 0.0)
    {
      assert (J_in < 0.0);
      assert (soil_water.q (0) < 0.0);
      C_top = J_in / soil_water.q (0);
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
  
      // Old absorbed matter.
      vector<double> A (size);
      // Water content at start and end of small timestep.
      vector<double> Theta_old (size);
      vector<double> Theta_new (size);
      for (int j = 0; j < size; j++)
	{
	  const double Theta_ratio 
	    = (soil_water.Theta (j) - soil_water.Theta_old (j)) / dt;
	  Theta_new[j] = soil_water.Theta_old (j) + Theta_ratio * t;
	  Theta_old[j] = soil_water.Theta_old (j) + Theta_ratio * old_t;
	  A[j] = M_[j] - C_[j] * Theta_old[j];
	}

      for (int j = 1; j < size; j++)
	{
	  const double dz_minus	// Size of layer above current node.
	    = soil.z (j-1) - soil.z (j);
	  const double dz_plus	// Size of layer below current node.
	    = (j == size - 1) ? dz_minus : (soil.z (j) - soil.z (j+1));

	  const double dz = soil.dz (j); // Size of current node.
	  double q_minus = soil_water.q (j); // Flow to above.
	  const double q_plus = soil_water.q (j+1);	// Flow from below.
	  const double alpha_minus = alpha[j]; // Direction above.
	  const double alpha_plus = alpha[j+1]; // Direction below.
	  double D_minus = D[j]; // Dispertion above.
	  const double D_plus = D[j+1]; // Dispertion below.

	  // Concentration above and below current node.
	  const double C_minus = C_[j-1];
	  const double C_plus = (j == size - 1) ? C_[j] : C_[j+1];

	  a[j] = - D_minus / (2.0 * dz_minus * dz) 
	    + (alpha_minus * q_minus) / (2.0 * dz);
	  b[j] = (Theta_new[j] / (t - old_t)
		  + D_minus / (2.0 * dz_minus * dz)
		  + D_plus / (2.0 * dz_plus * dz)
		  + ((1 - alpha_minus) * q_minus) / (2.0 * dz)
		  - (alpha_plus * q_plus) / (2.0 * dz));
	  c[j] = - D_plus / (2.0 * dz_plus * dz)
	    - ((1.0 - alpha_plus) * q_plus) / (2.0 * dz);
	  d[j] = (Theta_old[j] * C_[j] / (t - old_t)
		  + S[j]
		  + ((D_minus * (C_minus - C_[j])) / (2.0 * dz_minus * dz))
		  - ((D_plus * (C_[j] - C_plus)) / (2.0 * dz_plus * dz))
		  - (q_minus * (alpha_minus * C_minus + (1.0 - alpha_minus) * C_[j])
		     / (2.0 * dz))
		  + (q_plus * (alpha_plus * C_[j] + (1.0 - alpha_plus) * C_plus)
		     / (2.0 * dz)));

	  // Check for NaN.
	  assert (finite (a[j]));
	  assert (finite (b[j]));
	  assert (finite (c[j]));
	  assert (finite (d[j]));
	}
      // Adjust for upper boundary condition.
      {
	// Size of layer above current node.
	const double dz_minus = dz_top;
	// Size of layer below current node.
	const double dz_plus = soil.z (0) - soil.z (1);

	// Size of current node.
	const double dz = soil.dz (0);
	// Flow to above.
	double q_minus = (J_in == 0.0) ? 0.0 : soil_water.q (0);
	// Flow from below.
	const double q_plus = soil_water.q (1);
	const double alpha_minus = alpha[0]; // Direction above.
	const double alpha_plus = alpha[1]; // Direction below.
	double D_minus = D[0]; // Dispertion above.
	const double D_plus = D[1]; // Dispertion below.

	// Concentration above and below current node.
	const double C_minus = C_top;
	const double C_plus = C_[1];

	a[0] = - D_minus / (2.0 * dz_minus * dz) 
	  + (alpha_minus * q_minus) / (2.0 * dz);
	b[0] = (Theta_new[0] / (t - old_t)
		+ D_minus / (2.0 * dz_minus * dz)
		+ D_plus / (2.0 * dz_plus * dz)
		+ ((1 - alpha_minus) * q_minus) / (2.0 * dz)
		- (alpha_plus * q_plus) / (2.0 * dz));
	c[0] = - D_plus / (2.0 * dz_plus * dz)
	  - ((1.0 - alpha_plus) * q_plus) / (2.0 * dz);
	d[0] = (Theta_old[0] * C_[0] / (t - old_t)
		+ S[0]
		+ ((D_minus * (C_minus - C_[0])) / (2.0 * dz_minus * dz))
		- ((D_plus * (C_[0] - C_plus)) / (2.0 * dz_plus * dz))
		- (q_minus * (alpha_minus * C_minus + (1.0 - alpha_minus) * C_[0])
		   / (2.0 * dz))
		+ (q_plus * (alpha_plus * C_[0] + (1.0 - alpha_plus) * C_plus)
		   / (2.0 * dz)));

	// Check for NaN.
	assert (finite (a[0]));
	assert (finite (b[0]));
	assert (finite (c[0]));
	assert (finite (d[0]));
	if (J_in != 0.0)
	  {
	    const double q0 = soil_water.q (0);
	    assert (J_in < 0.0);
	    assert (q0 < 0.0);
	    // cerr << "J_in == " << J_in << "\n";
	    // cerr << "C_in == " << J_in / q0 << "\n";
	  }
	d[0] -= a[0] * C_top;
      }
      // Adjust for lower boundary condition.
      b[size - 1] += c[size - 1];
      c[size - 1] = -42.42e42;

      // Calculate new concentration.
      tridia (0, size, a, b, c, d, C_.begin ());

      // Update M and C.
      for (int j = 0; j < size; j++)
	{
	  // We use the old absorbed stuff plus the new dissolved stuff.
	  M_[j] = A[j] + Theta_new[j] * C_[j];

	  if (M_[j] < 0.0)
	    { 
	      cerr << "\nBUG: M[" << j << "] = " << M_[j] 
		   << " after transport\n";
	      M_[j] = 0.0;
	    }
	  // We calculate new C by assumining instant absorption.
	  C_[j] = M_to_C (soil, Theta_new[j], j, M_[j]);
	}
    }

  // Calculate flux with mass conservation.

  J[0] = J_in;
  for (int i = 0; i < size; i++)
    {
      assert (M_[i] >= 0.0);
      J[i + 1] = (((M_[i] - M_prev[i]) / dt) - S[i]) * soil.dz (i) + J[i];
    }
#endif
}

bool 
Solute::check (unsigned) const
{
  return true;
}

void
Solute::output (Log& log, Filter& filter) const
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
	  if (fabs (M_[i] / C_to_M (soil, soil_water.Theta (i), i, C_[i]) 
		   - 1.0) >= 0.001)
	    THROW ("Solute C does not match M");
	}
    }

  S.insert (S.begin (), soil.size (), 0.0);
  J.insert (J.begin (), soil.size () + 1, 0.0);
}

Solute::~Solute ()
{ }
