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
Solute::add_to_sink (const vector<double>& v)
{
  assert (S.size () == v.size ());
  for (unsigned i = 0; i < S.size (); i++)
    S[i] += v[i];
}

void 
Solute::tick (const Soil& soil, const SoilWater& soil_water, double J_in)
{
  // Note: q, D, and alpha depth indexes are all [j-1/2].
  
  // Constants.
  const int size = soil.size (); // Number of soil layers.
  const double D_l = diffusion_coefficient (); // in free solution [m^2 / s]

  // Dispersion coefficient [m^2 /s]
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

      // Tortuosity factor []
      const double f_l = pow (q, 10.0 / 3.0) / (n * n);

      // Theta middled in time and space.
      const double Theta
	= (soil_water.Theta (j) + soil_water.Theta (prev)
	   + soil_water.Theta_old (j) + soil_water.Theta_old (prev)) / 4.0;

      // From equation 7-39:
      D[j] = lambda * abs (q / Theta) + D_l * f_l;
    }
  D[size] = D[size-1];

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

      // Concentration above and below current node.
      const double C_minus = (j < 1) ? C[j] : C[j-1];
      const double C_plus = (j == size - 1) ? C[j] : C[j+1];
      
      a[j] = D[j] / (2.0 * dz_minus * soil.dz (j))
	- (alpha[j] * soil_water.q (j)) / (2.0 * soil.dz (j));
      b[j] = ((soil_water.Theta (j) + beta) / dt
	      + D[j] / (2.0 * dz_minus * soil.dz (j))
	      + D[j+1] / (2.0 * dz_plus * soil.dz (j))
	      - ((1 - alpha[j]) * soil_water.q (j)) / (2.0 * soil.dz (j))
	      + (alpha[j+1] * soil_water.q (j+1)) / (2.0 * soil.dz (j)));
      c[j] = - D[j+1] / dz_plus
	- ((1.0 - alpha[j+1]) * soil_water.q (j+1)) / (2.0 * soil.dz (j));
      d[j] = ((soil_water.Theta_old (j) + beta) * C[j] / dt
	      + S[j]
	      - ((D[j] * (C[j] - C_minus))
		 / (2.0 * dz_minus * soil.dz (j)))
	      + ((D[j+1] * (C_plus - C[j]))
		 / (2.0 * dz_plus * soil.dz (j)))
	      + (soil_water.q (j) 
		 * (alpha[j] * C_minus + (1.0 - alpha[j]) * C[j])
		 / (2.0 * soil.dz (j)))
	      - (soil_water.q (j+1)
		 * (alpha[j+1] * C[j] + (1.0 - alpha[j]) * C_plus)
		 / (2.0 * soil.dz (j))));
    }
  // Adjust for lower boundary condition.
  const double dz_minus = - 2.0 * soil.z (0);
  const double d_in 
    = (J_in + (D[0] / dz_minus
	       - soil_water.q (1) * (1 - alpha[1])) * 0.5 * C[0])
    / (D[0] / dz_minus + soil_water.q (1) * alpha[1]);
  const double b_in 
    = (D[0] / dz_minus - soil_water.q (1) * (1 - alpha[1])) * 0.5
    / (D[0] / dz_minus + soil_water.q (1) * alpha[1]);

  d[0] -= a[0] * d_in;
  b[0] += a[0] * b_in;
  a[0] = 42.42e42;


  // Adjust for upper boundary condition.
  b[size - 1] += c[size - 1];
  c[size - 1] = -42.42e42;

  // Calculate new concentration.
  tridia (0, size, a, b, c, d, C.begin ());

  // Calculate flux with mass conservation.
  J[0] = J_in;
  for (int i = 0; i < size; i++)
    {
      const double M_old = M[i];
      M[i] = C_to_M (soil, soil_water, i, C[i]);
      J[i + 1] = (((M[i] - M_old) / dt) + S[i]) * soil.dz (i) + J[i];
    }
}

bool 
Solute::check (Log&, unsigned n) const
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
  syntax.add ("C", Syntax::Array, Syntax::Optional);
  syntax.add ("M", Syntax::Array, Syntax::Optional);
  syntax.add ("S", Syntax::Array, Syntax::LogOnly);
  syntax.add ("J", Syntax::Array, Syntax::LogOnly);
}

Solute::Solute ()
{ }

void
Solute::initialize (const Soil& soil, const SoilWater soil_water,
		    const AttributeList& al)
{
  int size = 0;
  
  if (al.check ("C"))
    {
      C = al.array ("C");
      size = C.size ();
    }
  if (al.check ("M"))
    {
      M = al.array ("M");
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
