// uzrichard.C

#include "uzrichard.h"
#include "soil.h"
#include "mathlib.h"
#include "alist.h"
#include "syntax.h"

#define exception BUG_exception
#include <math.h>
#undef exception

struct UZRichard::Variables
{
  double q_up;
  double q_down;
  Variables ();
};

UZRichard::Variables::Variables ()
  : q_up (0.0),
    q_down (0.0)
{ }

struct UZRichard::Parameters
{
  int max_time_step_reductions;
  int time_step_reduction;
  int max_iterations;
  double max_absolute_difference;
  double max_relative_difference;
  Parameters (const AttributeList&);
};

UZRichard::Parameters::Parameters (const AttributeList& al)
     : max_time_step_reductions (al.integer ("max_time_step_reductions")),
       time_step_reduction (al.integer ("time_step_reduction")),
       max_iterations (al.integer ("max_iterations")),
       max_absolute_difference (al.number ("max_absolute_difference")),
       max_relative_difference (al.number ("max_relative_difference"))
{ }

bool 
UZRichard::flux_top () const
{
  return true;
}

double 
UZRichard::q () const
{
  return var.q_down;
}
void  
UZRichard::flux_top_on () const
{ }

void  
UZRichard::flux_top_off () const
{ }

bool  
UZRichard::accept_top (double) const
{ 
  return true;
}

bool 
UZRichard::flux_bottom () const
{
  return true;
}

bool  
UZRichard::accept_bottom (double) const
{ 
  return true;
}

bool
UZRichard::richard (const Soil& soil, 
		    int first, const UZtop& top, 
		    int last, const UZbottom& bottom, 
		    const vector<double>& S,
		    const vector<double>& h_old,
		    const vector<double>& Theta_old,
		    vector<double>& h_new,
		    vector<double>& Theta_new,
		    vector<double>& q)
{
  const double large_time_step = 1.0;

  // Input variables for solving a tridiagonal matrix.
  const int size = last - first + 1; 
  vector<double> a (size);
  vector<double> b (size);
  vector<double> c (size);
  vector<double> d (size);
  
  // Intermeditate results.
  vector<double> h (size);
  vector<double> h_previous (size);
  vector<double> h_conv (size);
  vector<double> Theta (size);
  vector<double> Ksum (size);
  vector<double> Kold (size);
  vector<double> K (size + 1);
  vector<double> Kplus (size);

  // For h bottom.
  K[size] = soil.K (last + 1, 0.0);

  // First guess is the old value.
  copy (h_old.begin () + first, h_old.begin () + last + 1, h.begin ());
  copy (Theta_old.begin () + first, Theta_old.begin () + last + 1, Theta.begin ());

  double time_left = large_time_step;	// How much of the large time step left.
  double dt = large_time_step;		// We start with small == large time step.
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
	  h_conv = h;
	  iterations_used++;

	  // Calculate parameters.
	  for (int i = 0; i < size; i++)
	    {
	      
	      Ksum[i] += soil.K (first + i, h[i]);
	      K[i] = (Ksum[i] / iterations_used + Kold[i]) / 2;
	    }
	  if (bottom.flux_bottom ())
	    K[size] = K[size - 1];

	  internode (soil, first, last, K, Kplus);

	  // Calcualte nodes.
	  for (int i = 0; i < size; i++)
	    {
	      const double Cw1 = soil.Cw1 (first + i, h[i]);
	      const double Cw2 = soil.Cw2 (first + i, h[i]);
	      const double dz = soil.dz (first + i);
	      const double z = soil.z (first + i);

	      if (i == 0)
		{
		  // Calculate upper boundary.
		  const double dz_plus = z - soil.z (first + i + 1);

		  if (top.flux_top ())
		    {	
		      b[i] = Cw2 + (dt / dz) * (Kplus[i] / dz_plus);
		      d[i] = Theta[i] - Cw1 - dt * S[first + i] 
			+ (dt / dz) * (top.q () - Kplus[i]);
		    }
		  else 
		    {
		      const double dz_minus = soil.z (first + i - 1) - z;
		      double Ksat = soil.K (first + i, 0.0);
		      b[i] = Cw2 
			+ (dt / dz) * (Ksat / dz_minus + Kplus[i] / dz_plus);
		      d[i] = Theta[i] - Cw1 - dt * S[first + i] 
			+ (dt / dz) 
			* (Ksat * (1 + h[i - 1] / dz_minus) - Kplus[i]);
		    }
		  a[i] = 0.0;
		  c[i] = - (dt / dz) * (Kplus[i] / dz_plus);
		}
	      else if (i == size - 1)
		{
		  // Calculate lower boundary
		  const double dz_minus = soil.z (first + i - 1) - z;

		  if (bottom.flux_bottom ())
		    {
		      double q_bottom = - Kold[i];
		      b[i] = Cw2 + (dt / dz) * (Kplus[i - 1] / dz_minus);
		      d[i] = Theta[i] - Cw1 - dt * S[first + i] 
			+ (dt / dz) * (Kplus[i - 1] + q_bottom);
		    }
		  else 
		    {
		      const double dz_plus = z - soil.z (first + i + 1);

		      b[i] = Cw2 + (dt / dz) * (  Kplus[i - 1] / dz_minus 
						+ Kplus[i] / dz_plus);
		      d[i] = Theta[i] - Cw1 - dt * S[first + i] 
			+ (dt / dz)
			* (Kplus[i - 1] - Kplus[i] * (1 - h[i + 1] / dz_plus));
		    }
		  a[i] = - (dt / dz) * (Kplus[i - 1] / dz_minus);
		  c[i] = 0.0;
		}
	      else
		{
		  // Calculate intermediate nodes.
		  const double dz_minus = soil.z (first + i - 1) - z;
		  const double dz_plus = z - soil.z (first + i + 1);

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
      while (   !converges (h_conv, h) 
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

  // Make it official.
  assert (h_new.size () >= first + size + 0U);
  copy (h.begin (), h.end (), h_new.begin () + first);
  assert (Theta_new.size () >= first + size + 0U);
  copy (Theta.begin (), Theta.end (), Theta_new.begin () + first);

  // Update Theta below groundwater table.
  if (!bottom.flux_bottom ())
    {
      for(unsigned int i = last; i < Theta_new.size (); i++)
	Theta_new[i] = soil.Theta(i, h[i]);
    }

  // Calculate flux.
  if (top.flux_top ())
    {
      // We know flux on upper border, use mass preservation to
      // calculate flux below given the change in water content.
      q[first] = top.q ();
      for (int i = first; i < last; i++)
	q[i + 1] = (((Theta_new[i] - Theta_old[i]) / large_time_step) + S[i])
	  * soil.dz (i) + q[i];
    }
  else
    {
      // Find an unsaturated area.
      int start;
      for (start = last -1; start > 0; start--)
	{
	  if (h_new[start] < 0.0 && h_new[start + 1] < 0.0)
	    break;
	}
      if (start == 0)
	THROW (Numeric ("We couldn't find an unsaturated area."));
      // Use Darcy equation to find flux here.
      q[start + 1] = -Kplus[start] 
	* (  (  (  (h_new[start] +     h_old[start])     / 2 
	   	 - (h_new[start + 1] + h_old[start + 1]) / 2)
	      / (soil.z (start) - soil.z (start + 1)))
	   + 1);
      // Use mass preservation to find flux below and above.
      for (int i = start + 1; i <= last; i++)
	q[i + 1] = (((Theta_new[i] - Theta_old[i]) / large_time_step) + S[i])
	  * soil.dz (i) + q[i];
      for (int i = start; i >= 0; i--)
	q[i] = - (((Theta_new[i + 1] - Theta_old[i + 1]) / large_time_step) + S[i + 1])
	  * soil.dz (i + 1) + q[i + 1];
    }
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
  int size = last - first + 1;
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
  return par.max_time_step_reductions;
}

int 
UZRichard::time_step_reduction () const
{
  return par.time_step_reduction;
}

int 
UZRichard::max_iterations () const
{
  return par.max_iterations;
}

double  
UZRichard::max_absolute_difference () const
{
  return par.max_absolute_difference;
}

double  
UZRichard::max_relative_difference () const
{

  return par.max_relative_difference;
}

void 
UZRichard::tick (const Soil& soil,
		 int first, const UZtop& top, 
		 int last, const UZbottom& bottom, 
		 const vector<double>& S,
		 const vector<double>& h_old,
		 const vector<double>& Theta_old,
		 vector<double>& h,
		 vector<double>& Theta,
		 vector<double>& q)
{
  if (!richard (soil, first, top, last, bottom, 
		S, h_old, Theta_old, h, Theta, q))
    THROW (Runtime ("Richard's Equation doesn't converge."));
    
  if (top.flux_top () && h[first] > 0)
    {
      top.flux_top_off ();
      if (!richard (soil, first, top, last, bottom, 
		    S, h_old, Theta_old, h, Theta, q))
	THROW (Runtime ("Richard's Equation doesn't converge."));
      if (!top.accept_top (q[first]))
	THROW (Numeric ("Couldn't accept top flux"));
    }
  else if (!top.flux_top () && !top.accept_top (q[first]))
    {
      top.flux_top_on ();
      if (!richard (soil, first, top, last, bottom, 
		    S, h_old, Theta_old, h, Theta, q))
	THROW (Runtime ("Richard's Equation doesn't converge."));
      if (h[first] < 0)
	THROW (Numeric ("Couldn't drain top flux"));
    }
  bottom.accept_bottom (q[last + 1]);

  var.q_up = q[first];
  var.q_down = q[last + 1];
}

UZRichard::UZRichard (const AttributeList& al)
  : par (*new Parameters (al)),
    var (*new Variables ())
{ }


UZRichard::~UZRichard ()
{ 
  delete &var;
  delete &par;
}

// Add the UZRichard syntax to the syntax table.
UZmodel* 
UZRichard::make (const AttributeList& al)
{
  return new UZRichard (al);
}

static struct UZRichardSyntax
{
  UZRichardSyntax ();
} UZRichard_syntax;

UZRichardSyntax::UZRichardSyntax ()
{
  Syntax* syntax = new Syntax ();
  AttributeList* alist = new AttributeList ();
  
  syntax->add ("max_time_step_reductions", Syntax::Integer);
  alist->add ("max_time_step_reductions", 4);
  syntax->add ("time_step_reduction", Syntax::Integer);
  alist->add ("time_step_reduction", 4);
  syntax->add ("max_iterations", Syntax::Integer);
  alist->add ("max_iterations", 25);
  syntax->add ("max_absolute_difference", Syntax::Number);
  alist->add ("max_absolute_difference", 0.002);
  syntax->add ("max_relative_difference", Syntax::Number);
  alist->add ("max_relative_difference", 0.001);
  
  UZmodel::add_type ("richards", *alist, *syntax, &UZRichard::make);
}
