// transport_cd.C --- Using convection-dispersion.

#include "transport.h"
#include "soil.h"
#include "soil_water.h"
#include "solute.h"
#include "log.h"
#include "mathlib.h"

class TransportCD : public Transport
{
  // Log variable.
private:
  vector<double> J;
  double ddt;
  
  // Simulation.
public:
  void tick (const Soil&, const SoilWater&, const Solute&,
	     vector<double>& M, 
	     vector<double>& C,
	     const vector<double>& S,
	     double J_in);
  void output (Log&, Filter&) const;

  // Create.
public:
  TransportCD (const AttributeList& al)
    : Transport (al.name ("type")),
      ddt (dt)
    { }
};

void
TransportCD::output (Log& log, Filter& filter) const
{
  log.output ("J", filter, J, true);
  log.output ("ddt", filter, ddt, true);
}

void 
TransportCD::tick (const Soil& soil, const SoilWater& soil_water,
		   const Solute& solute, 
		   vector<double>& M, 
		   vector<double>& C,
		   const vector<double>& S,
		   double J_in)
{
  // Remember old values.
  vector<double> C_prev = C;
  vector<double> M_prev = M;

  // Constants.
  const unsigned int size = soil.size (); // Number of soil layers.

  // Check that incomming C and M makes sense.
  for (unsigned int i = 0; i < size; i++)
    {
      assert (C[i] >= 0.0);
      assert (M[i] >= 0.0);
      if (C[i] == 0.0)
	assert (M[i] == 0.0);
      else
	assert (approximate (M[i], 
			     solute.C_to_M (soil,
					    soil_water.Theta_old (i),
					    i, C[i])));
    }

  // Note: q, D, and alpha depth indexes are all [j-½].

  // Dispersion coefficient [cm²/s]
  vector<double> D (size + 1);

  for (unsigned int j = 1; j < size; j++)
    {
      // Dispersion length [cm]
      const double lambda = soil.dispersivity (j);

      // Water flux [cm³ /cm² / h]
      const double q = soil_water.q (j);
      
      // Theta middled in time and space.
      const double Theta = 
	(soil_water.Theta (j) + soil_water.Theta (j-1)
	 + soil_water.Theta_old (j) + soil_water.Theta_old (j-1)) / 4.0;
      // From equation 7-39:
      D[j] = (lambda * fabs (-q / Theta)
	      + soil.tortuosity_factor (j, Theta)
	      * solute.diffusion_coefficient ())
	* Theta;

      // Check for NaN.
      assert (finite (D[j]));
    }
  // Lower boundary.
  {
    // Dispersion length [cm]
    const double lambda = soil.dispersivity (size-1);

    // Water flux [cm³ /cm² / h]
    const double q = soil_water.q (size);
      
    // Theta middled in time and space.
    const double Theta = 
      (soil_water.Theta (size - 1) + soil_water.Theta_old (size  - 1)) / 2.0;
    // From equation 7-39:
    D[size] = (lambda * fabs (-q / Theta)
	       + soil.tortuosity_factor (size-1, Theta) 
 	       * solute.diffusion_coefficient ())
      * Theta;

  }
  // Upper boundary (no dispersion over soil surface).
  D[0] = 0.0;

  // Weight factor (how important is this flux for the concentration)
  // This is 1 for incomming flux and 0 for outgoing flux.
  vector<double> alpha (size + 1);

  for (unsigned int j = 0; j < size + 1; j++)
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
  double S_top = 0.0;
  if (J_in > 0.0)
    {
      cerr << "\nBug: Positive J_in (" << J_in << ")\n";
      J_in = 0.0;
    }
  if (J_in != 0.0)
    {
      assert (J_in < 0.0);

      if (soil_water.q (0) < 0.0)
	// Normal condition, stuff is in solute.
	C_top = J_in / soil_water.q (0);
      else			
	// This should only happen if Surface::total_matter_flux.
	S_top = -J_in / soil.dz (0);
    }

  // Find the time step using Courant.
  ddt = 1.0;
  for (unsigned int i = 0; i < size; i++)
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
      for (unsigned int j = 0; j < size; j++)
	{
	  const double Theta_ratio 
	    = (soil_water.Theta (j) - soil_water.Theta_old (j)) / dt;
	  Theta_new[j] = soil_water.Theta_old (j) + Theta_ratio * t;
	  Theta_old[j] = soil_water.Theta_old (j) + Theta_ratio * old_t;
	  A[j] = M[j] - C[j] * Theta_old[j];
	}

      for (unsigned int j = 1; j < size; j++)
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
	  const double C_minus = C[j-1];
	  const double C_plus = (j == size - 1) ? C[j] : C[j+1];

	  a[j] = - D_minus / (2.0 * dz_minus * dz) 
	    + (alpha_minus * q_minus) / (2.0 * dz);
	  b[j] = (Theta_new[j] / (t - old_t)
		  + D_minus / (2.0 * dz_minus * dz)
		  + D_plus / (2.0 * dz_plus * dz)
		  + ((1 - alpha_minus) * q_minus) / (2.0 * dz)
		  - (alpha_plus * q_plus) / (2.0 * dz));
	  c[j] = - D_plus / (2.0 * dz_plus * dz)
	    - ((1.0 - alpha_plus) * q_plus) / (2.0 * dz);
	  d[j] = (Theta_old[j] * C[j] / (t - old_t)
		  + S[j]
		  + ((D_minus * (C_minus - C[j])) / (2.0 * dz_minus * dz))
		  - ((D_plus * (C[j] - C_plus)) / (2.0 * dz_plus * dz))
		  - (q_minus * (alpha_minus * C_minus
				+ (1.0 - alpha_minus) * C[j])
		     / (2.0 * dz))
		  + (q_plus * (alpha_plus * C[j] + (1.0 - alpha_plus) * C_plus)
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
	const double C_plus = C[1];

	a[0] = - D_minus / (2.0 * dz_minus * dz) 
	  + (alpha_minus * q_minus) / (2.0 * dz);
	b[0] = (Theta_new[0] / (t - old_t)
		+ D_minus / (2.0 * dz_minus * dz)
		+ D_plus / (2.0 * dz_plus * dz)
		+ ((1 - alpha_minus) * q_minus) / (2.0 * dz)
		- (alpha_plus * q_plus) / (2.0 * dz));
	c[0] = - D_plus / (2.0 * dz_plus * dz)
	  - ((1.0 - alpha_plus) * q_plus) / (2.0 * dz);
	d[0] = (Theta_old[0] * C[0] / (t - old_t)
		+ S[0] + S_top
		+ ((D_minus * (C_minus - C[0])) / (2.0 * dz_minus * dz))
		- ((D_plus * (C[0] - C_plus)) / (2.0 * dz_plus * dz))
		- (q_minus * (alpha_minus * C_minus
			      + (1.0 - alpha_minus) * C[0])
		   / (2.0 * dz))
		+ (q_plus * (alpha_plus * C[0] + (1.0 - alpha_plus) * C_plus)
		   / (2.0 * dz)));

	// Check for NaN.
	assert (finite (a[0]));
	assert (finite (b[0]));
	assert (finite (c[0]));
	assert (finite (d[0]));
	d[0] -= a[0] * C_top;
      }
      // Adjust for lower boundary condition.
      b[size - 1] += c[size - 1];
      c[size - 1] = -42.42e42;

      // Calculate new concentration.
      tridia (0, size, a, b, c, d, C.begin ());

      // Update M and C.
      for (unsigned int j = 0; j < size; j++)
	{
	  // We use the old absorbed stuff plus the new dissolved stuff.
	  M[j] = A[j] + Theta_new[j] * C[j];

	  if (M[j] < 0.0)
	    { 
	      cerr << "\nBUG: M[" << j << "] = " << M[j] 
		   << " after transport\n";
	      M[j] = 0.0;
	    }
	  // We calculate new C by assumining instant absorption.
	  C[j] = solute.M_to_C (soil, Theta_new[j], j, M[j]);
	}
    }

  // Calculate flux with mass conservation.
  if (size + 1 > J.size ())
    J.insert (J.begin (), size + 1 - J.size (), 0.0);

  J[0] = J_in;
  for (unsigned int i = 0; i < size; i++)
    {
      double S_term = S[i];
      if (i == 0)
	S_term += S_top;
      assert (M[i] >= 0.0);
      J[i + 1] = (((M[i] - M_prev[i]) / dt) - S[i]) * soil.dz (i) + J[i];
    }
}

static struct TransportCDSyntax
{
  static Transport& make (const AttributeList& al)
  {
    return *new TransportCD (al);
  }

  TransportCDSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    syntax.add ("J", Syntax::Number, Syntax::LogOnly, Syntax::Sequence);
    syntax.add ("ddt", Syntax::Number, Syntax::LogOnly, Syntax::Singleton);
    Librarian<Transport>::add_type ("cd", alist, syntax, &make);
  }
} TransportCD_syntax;
