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
    S[i] += v[i];
}

void 
Solute::tick (const Soil& soil, const SoilWater& soil_water, const double J_in)
{
  // Note: q, D, and alpha depth indexes are all [j-1/2].
  
  // Constants.
  const int size = soil.size (); // Number of soil layers.
  const double D_l = diffusion_coefficient (); // in free solution [cm^2 / s]

  // Dispersion coefficient [cm^2 /s]
  vector<double> D (size + 1);

  for (int j = 0; j < size; j++)
    {
      const int prev = (j < 1) ? 0 : j - 1;

      // Dispersion length [m]
      const double lambda = soil.lambda (j);

      // Water flux [m^3 /m^2 / s]
      const double q = soil_water.q (j);
      
      // Porosity []
      const double n = (soil.Theta (j, 0.0) + soil.Theta (prev, 0.0)) / 2.0;

      // Theta middled in time and space.
      const double Theta
	= (soil_water.Theta (j) + soil_water.Theta (prev)
	   + soil_water.Theta_old (j) + soil_water.Theta_old (prev)) / 4.0;

      // Tortuosity factor []
      const double f_l = pow (Theta, 10.0 / 3.0) / (n * n);

      // From equation 7-39:
      D[j] = lambda * abs (-q / Theta) + D_l * f_l;
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


  // Parameters for tridiagonal matrix.
  vector<double> a (size);
  vector<double> b (size);
  vector<double> c (size);
  vector<double> d (size);
  
  for (int j = 0; j < size; j++)
    {
      // dA/dC in the present soil water solute.
      const double beta = this->beta (soil, soil_water, j, C[j]);
      
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
      const double Theta_new = soil_water.Theta (j); // New water content.
      const double Theta_old = soil_water.Theta_old (j); // Old water content.
      
      
      // Concentration above and below current node.
      const double C_minus = (j < 1) ? C[j] : C[j-1];
      const double C_plus = (j == size - 1) ? C[j] : C[j+1];
      
      a[j] = - D_minus / (2.0 * dz_minus * dz) 
	+ (alpha_minus * q_minus) / (2.0 * dz);
      b[j] = ((Theta_new + beta) / dt
	      + D_minus / (2.0 * dz_minus * dz)
	      + D_plus / (2.0 * dz_plus * dz)
	      + ((1 - alpha_minus) * q_minus) / (2.0 * dz)
	      - (alpha_plus * q_plus) / (2.0 * dz));
      c[j] = - D_plus / (2.0 * dz_plus * dz)
	- ((1.0 - alpha_plus) * q_plus) / (2.0 * dz);
      d[j] = ((Theta_old + beta) * C[j] / dt
	      + S[j]
	      + ((D_minus * (C_minus - C[j])) / (2.0 * dz_minus * dz))
	      - ((D_plus * (C[j] - C_plus)) / (2.0 * dz_plus * dz))
	      - (q_minus * (alpha_minus * C_minus + (1.0 - alpha_minus) * C[j])
		 / (2.0 * dz))
	      + (q_plus * (alpha_plus * C[j] + (1.0 - alpha_plus) * C_plus)
		 / (2.0 * dz)));
    }
#if 0
  // Adjust for upper boundary condition.
  const double dz_minus = - 2.0 * soil.z (0);
  const double b_in 
    = (D[0] / dz_minus + soil_water.q (0) * alpha[0]) * 0.5
    / (D[0] / dz_minus + soil_water.q (0) * (1 - alpha[0]));
  const double d_in 
    = (- J_in + (D[0] / dz_minus + soil_water.q (0) * alpha[0]) * 0.5 * C[0])
    / (D[0] / dz_minus + soil_water.q (0) * (1 - alpha[0]));
  d[0] -= a[0] * d_in;
  b[0] += a[0] * b_in;
  a[0] = 42.42e42;
#elif 1
  const double dz_x = - 2.0 * soil.z (0);
  const double d_x = 4 * J_in + 2 * D[0] / dz_x * C[0] 
    - soil_water.q (0) * C[0];
  const double b_x = 2 * (soil_water.q (0) - 2 * D[0] / dz_x);
  const double a_x = soil_water.q (0) + 2 * D[0] / dz_x;
  d[0] -= a[0] * d_x / b_x;
  b[0] -= a[0] * a_x / b_x;
  a[0] = 42.42e42;
#else
  {
    const int j = 0;
    const double beta = this->beta (soil, soil_water, j, C[j]);
    const double dz_plus	// Size of layer below current node.
      = soil.z (j) - soil.z (j+1);
    const double dz = soil.dz (j); // Size of current node.
    const double q_plus = soil_water.q (j+1);	// Flow from below.
    const double alpha_plus = alpha[j+1]; // Direction below.
    const double D_plus = D[j+1]; // Dispertion below.
    const double Theta_new = soil_water.Theta (j); // New water content.
    const double Theta_old = soil_water.Theta_old (j); // Old water content.
    const double C_plus = (j == size - 1) ? C[j] : C[j+1];

    a[0] = 42.42e42;
    b[0] = ((Theta_new + beta) / dt
	    + D_plus / (2.0 * dz_plus * dz)
	    - (alpha_plus * q_plus) / (2.0 * dz));
    d[0] = ((Theta_old + beta) * C[j] / dt
	    + S[j]
	    - J_in / dz
	    - ((D_plus * (C[j] - C_plus)) / (2.0 * dz_plus * dz))
	    + (q_plus * (alpha_plus * C[j] + (1.0 - alpha_plus) * C_plus) 
	       / (2.0 * dz)));
  }
#endif
  // Adjust for lower boundary condition.
  b[size - 1] += c[size - 1];
  c[size - 1] = -42.42e42;

#if 1
  const vector<double> C_old = C;
#endif

  // Calculate new concentration.
  tridia (0, size, a, b, c, d, C.begin ());

  // Calculate flux with mass conservation.
  J[0] = J_in;
  for (int i = 0; i < size; i++)
    {
      const double M_old = M[i];
      M[i] = C_to_M (soil, soil_water, i, C[i]);
      J[i + 1] = (((M[i] - M_old) / dt) - S[i]) * soil.dz (i) + J[i];
    }

#if 1
  S[0] = J_in;
  for (int i = 0; i < size - 1; i++)
    {
      const double C_minus = (C[i] + C_old[i]) / 2.0;
      const double C_plus = (C[i+1] + C_old[i+1]) / 2.0;
      const double dz_plus = soil.z (i) - soil.z (i+1);
      const double q_plus = soil_water.q (i + 1);
      const double alpha_plus = alpha[i+1];
      const double D_plus = D[i+1];
      S[i + 1] = D_plus * (C_plus - C_minus) / dz_plus 
	+ q_plus * (alpha_plus * C_minus + (1 - alpha_plus) * C_plus);
    }
#endif
}

bool 
Solute::check (unsigned n) const
{
  bool ok = true;

  if (C.size () != n)
    {
      cerr << "You have " << n 
	   << " intervals but " << C.size () << " C values\n";
      ok = false;
    }
  if (M.size () != n)
    {
      cerr << "You have " << n 
	   << " intervals but " << M.size () << " M values\n";
      ok = false;
    }
  return ok;
}

void
Solute::output (Log& log, const Filter* filter) const
{
  log.output ("C", filter, C);
  log.output ("M", filter, M);
  log.output ("S", filter, S, true);
  log.output ("J", filter, J, true);
}

void 
Solute::load_syntax (Syntax& syntax, AttributeList&)
{
  syntax.add ("C", Syntax::Number, Syntax::Optional, Syntax::Sequence);
  syntax.add ("M", Syntax::Number, Syntax::Optional, Syntax::Sequence);
  syntax.add ("S", Syntax::Number, Syntax::LogOnly, Syntax::Sequence);
  syntax.add ("J", Syntax::Number, Syntax::LogOnly, Syntax::Sequence);
}

Solute::Solute ()
{ }

void
Solute::initialize (const Soil& soil, const SoilWater& soil_water,
		    const AttributeList& al)
{
  int size = 0;
  
  if (al.check ("C"))
    {
      C = al.number_sequence ("C");
      size = C.size ();
    }
  if (al.check ("M"))
    {
      M = al.number_sequence ("M");
      size = M.size ();
    }
  if (!al.check ("C"))
    for (int i = 0; i < size; i++)
      C.push_back (M_to_C (soil, soil_water, i, M[i]));

  if (!al.check ("M"))
    for (int i = 0; i < size; i++)
      M.push_back (C_to_M (soil, soil_water, i, C[i]));
  
  S.insert (S.begin (), size, 0.0);
  J.insert (J.begin (), size + 1, 0.0);
}

Solute::~Solute ()
{ }
