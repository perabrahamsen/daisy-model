// uzmodel.C

#include "uzmodel.h"
#include "soil.h"
#include "mathlib.h"
#include <list.h>

#define exception BUG_exception
#include <math.h>
#undef exception

UZtop::~UZtop ()
{ }

UZbottom::~UZbottom ()
{ }

UZmodel::~UZmodel ()
{ }

bool
UZRichard::richard (const Soil& soil, 
		    int first, const UZtop& top, 
		    int last, const UZbottom& bottom, 
		    const vector<double>& S,
		    const vector<double>& h_old,
		    const vector<double>& Theta_old,
		    vector<double>& h_new,
		    vector<double>& Theta_new) const
{
  // Input variables for solving a tridiagonal matrix.
  const int size = last - first + 1; 
  vector<double> a (size);
  vector<double> b (size);
  vector<double> c (size);
  vector<double> d (size);
  
  // Intermeditate results.
  vector<double> h (size);
  vector<double> h_previous (size);
  vector<double> Theta (size);
  vector<double> Ksum (size);
  vector<double> Kold (size);
  vector<double> K (size + 1);
  vector<double> Kplus (size);

  // Cheat for h bottom.
  K[size] = soil.K (last + 1, 0.0);

  // First guess is the old value.
  copy (h_old.begin () + first, h_old.begin () + last, h.begin ());
  copy (Theta_old.begin () + first, Theta_old.begin () + last, Theta.begin ());

  double time_left = 1.0;	// How much of the large time step left.
  double dt = 1.0;		// We start with small == large time step.
  int number_of_time_step_reductions = 0;
  int iterations_with_this_time_step = 0;

  while (time_left > 0.0)
    {
      // Initialization for each small time step.
      int iterations_used = 0;
      if (dt > time_left)
	dt = time_left;

      for (int i = 0; i < size; i++)
	{
	  Ksum[i] = 0.0;
	  Kold[i] = soil.K (first + i, h[i]);
	}
      h_previous = h;
      
      do
	{
	  iterations_used++;

	  // Calculate parameters.
	  for (int i = 0; i < size; i++)
	    {
	      
	      Ksum[i] += soil.K (first + i, h[i]);
	      K[i] = (Ksum[i] / iterations_used + Kold[i]) / 2;
	    }
	  internode (soil, first, last, K, Kplus);

	  // Calcualte nodes.
	  for (int i = 0; i < size; i++)
	    {
	      const double Cw1 = soil.Cw1 (first + i, h[i]);
	      const double Cw2 = soil.Cw2 (first + i, h[i]);
	      const double dz = soil.dz (first + i);
	      const double z = soil.z (first + i);
	      const double dz_minus = (i == 0 && top.flux_top ())
		? +9999.99 : soil.z (first + i - 1) - z;
	      const double dz_plus = (i == size - 1 && bottom.flux_bottom ()) 
		? -9999.99 : z - soil.z (first + i + 1);
	      
	      if (i == 0)
		{
		  // Calculate upper boundary.
		  if (top.flux_top ())
		    {
		      b[i] = Cw2 + (dt / dz) * (Kplus[i] / dz_plus);
		      d[i] = Theta[i] - Cw1 - dt * S[first + i] 
			+ (dt / dz) * (top.q_top () - Kplus[i]);
		    }
		  else 
		    {
		      double Ksat = soil.K (first + i, 0.0);
		      b[i] = Cw2 
			+ (dt / dz) * (Ksat / dz_minus + Kplus[i] / dz_plus);
		      d[i] = Theta[i] - Cw1 - dt * S[first + i] 
			+ (dt / dz) 
			* (Ksat * (1 + h[i] / dz_minus) - Kplus[i]);
		    }
		  a[i] = 0.0;
		  c[i] = - (dt / dz) * (Kplus[i] / dz_plus);
		}
	      else if (i == size - 1)
		{
		  // Calculate lower boundary.
		  if (bottom.flux_bottom ())
		    {
		      b[i] = Cw2 + (dt / dz) * (Kplus[i - 1] / dz_minus);
		      d[i] = Theta[i] - Cw1 - dt * S[first + i] 
			+ (dt / dz) * (Kplus[i - 1] - bottom.q_bottom ());
		    }
		  else 
		    {
		      d[i] = Theta[i] - Cw1 - dt * S[first + i] 
			+ (dt / dz)
			* (Kplus[i - 1] - Kplus[i] * (1 - h[i + 1] / dz_plus));
		    }
		  a[i] = - (dt / dz) * (Kplus[i - 1] / dz_minus);
		  c[i] = 0.0;
		}
	      else
		// Calculate intermediate nodes.
		{
		  a[i] = - (dt / dz) * (Kplus[i - 1] / dz_minus);
		  b[i] = Cw2 + (dt / dz) * (  Kplus[i - 1] / dz_minus 
					    + Kplus[i] / dz_plus);
		  c[i] = - (dt / dz) * (Kplus[i] / dz_plus);
		  d[i] = Theta[i] - Cw1 - dt * S[first + i] 
		    +  (dt / dz) * (Kplus[i - 1] - Kplus[i] );
		}
	    }
	  tridia (size, a, b, c, d, h.begin ());
	}
      while (   !converges (h_previous, h) 
	     && iterations_used <= max_iterations ());

      if (iterations_used > max_iterations ())
	{
	  number_of_time_step_reductions++;
	  
	  if (number_of_time_step_reductions > max_time_step_reductions ())
	    return false;

	  dt /= time_step_reduction ();
	  h = h_previous;
	}
      else
	{
	  for (int i = 0; i < size; i++)
	    Theta[i] = soil.Theta(first + i, h[i]);
	  
	  time_left -= dt;
	  iterations_with_this_time_step++;

	  if (iterations_with_this_time_step > time_step_reduction ())
	    {
	      number_of_time_step_reductions--;
	      iterations_with_this_time_step = 0;
	      dt *= time_step_reduction ();
	    }
	}
    }

  // Return result.
  copy (h.begin (), h.end (), h_new.begin () + first);
  copy (Theta.begin (), Theta.end (), Theta_new.begin () + first);
  return true;
}

bool
UZRichard::converges (const vector<double>& previous, 
		      const vector<double>& current) const
{
  unsigned int size = previous.size ();
  assert (current.size () == size);

  for (unsigned int i = 0; i < size; i++)
    {
      if (   abs (current[i] - previous[i]) > max_absolute_difference ()
	  && (   previous[i] == 0.0
	      || current[i] == 0.0
	      || (  abs ((current[i] - previous[i]) / previous[i])
		  > max_relative_difference ())))
	return false;
    }
  return true;
}

void 
UZRichard::internode (const Soil& soil, int first, int last,
		      const vector<double>& K, 
		      vector<double>& Kplus) const
{
  int size = last - first;
  for (int i = 0; i < size; i++)
    // Should be a user option. 
    // Kplus[i] = 2 * K[i] * K[i + 1] / (K[i] + K[i + 1]); // Harmonic.
    // Kplus[i] = sqrt (K[i] * K[i + 1]); // Geometric.
    Kplus[i] = 0.5 * (K[i] + K[i + 1]); // Arithmetic.
  
  for (int i = 0; i < size; i++)
    if (!soil.compact (first + i))
      {
	double Ksat = soil.K (first + i, 0.0);
	Kplus[i] = min (Ksat, Kplus[i]);
	if (i > 0)
	  Kplus[i - 1] = min (Ksat, Kplus[i - 1]);
      }
}

int 
UZRichard::max_time_step_reductions () const
{ 
  return 4;
}

int 
UZRichard::time_step_reduction () const
{
  return 4;
}

int 
UZRichard::max_iterations () const
{
  return 25;
}

double  
UZRichard::max_absolute_difference () const
{
  return 0.002;
}

double  
UZRichard::max_relative_difference () const
{
  return 0.001;
}

UZRichard::~UZRichard ()
{ }

