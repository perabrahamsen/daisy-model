// uzrichard.C --- Using Richard's Equation to calculate water flow.

#include "uzmodel.h"
#include "soil.h"
#include "mathlib.h"
#include "alist.h"
#include "syntax.h"
#include "common.h"
#include "filter.h"
#include "log.h"

class UZRichard : public UZmodel
{
  // Variables.
private:
  double q_up;
  double q_down;
  int iterations;

  // Parameters.
  int max_time_step_reductions;
  int time_step_reduction;
  int max_iterations;
  double max_absolute_difference;
  double max_relative_difference;

  // UZmodel.
public:
  bool flux_top () const;
  double q () const;
  void flux_top_on ();
  void flux_top_off ();
  bool accept_top (double);
  bool flux_bottom () const;
  bool accept_bottom (double);
  void output (Log&, Filter&) const;

  // Simulate.
private:
  bool richard (const Soil& soil,
		int first, UZtop& top, 
		int last, UZbottom& bottom, 
		const vector<double>& S,
		const vector<double>& h_old,
		const vector<double>& Theta_old,
		vector<double>& h,
		vector<double>& Theta,
		vector<double>& q);
  bool converges (const vector<double>& previous, 
		  const vector<double>& current) const;
  void internode (const Soil& Soil, int first, int last,
		  const vector<double>& K, 
		  vector<double>& Kplus) const;
  void q_darcy (const Soil& soil, 
		int first, int last, 
		const vector<double>& h_previous,
		const vector<double>& h,
		const vector<double>& Theta_previous,
		const vector<double>& Theta,
		const vector<double>& Kplus,
		const vector<double>& S,
		double ddt,
		vector<double>& q);
public:
  void tick (const Soil& soil,
	     int first, UZtop& top, 
	     int last, UZbottom& bottom, 
	     const vector<double>& S,
	     const vector<double>& h_old,
	     const vector<double>& Theta_old,
	     vector<double>& h,
	     vector<double>& Theta,
	     vector<double>& q);

  // Create and Destroy.
public:
  UZRichard (const AttributeList& par);
  ~UZRichard ();
};

bool 
UZRichard::flux_top () const
{
  return true;
}

double 
UZRichard::q () const
{
  return q_down;
}
void  
UZRichard::flux_top_on ()
{ }

void  
UZRichard::flux_top_off ()
{ }

bool  
UZRichard::accept_top (double)
{ 
  return true;
}

bool 
UZRichard::flux_bottom () const
{
  return true;
}

bool  
UZRichard::accept_bottom (double)
{ 
  return true;
}

bool
UZRichard::richard (const Soil& soil, 
		    int first, UZtop& top, 
		    int last, UZbottom& bottom, 
		    const vector<double>& S,
		    const vector<double>& h_old,
		    const vector<double>& Theta_old,
		    vector<double>& h_new,
		    vector<double>& Theta_new,
		    vector<double>& q)
{
  // Input variables for solving a tridiagonal matrix.
  const unsigned int size = last - first + 1; 
  vector<double> a (size);
  vector<double> b (size);
  vector<double> c (size);
  vector<double> d (size);
  
  // Intermeditate results.
  vector<double> h (size);
  vector<double> h_previous (size);
  vector<double> h_conv (size);
  vector<double> Theta_previous (size);
  vector<double> Theta (size);
  vector<double> Ksum (size);
  vector<double> Kold (size);
  vector<double> K (size + 1);
  vector<double> Kplus (size);

  // For h bottom.
  K[size] = soil.K (last + 1, 0.0);

  // Check if we have already switched top once.
  bool switched_top = false;
  // If the original top is a flux top, we need to make sure we don't
  // drain it too fast.
  const bool real_flux_top = top.flux_top ();
  const double real_top_q = real_flux_top ? top.q () : 66.0e66;
  double flux_pond = 0;
  // Keep track of water going to the top.
  double top_water = 0.0;

  // First guess is the old value.
  copy (h_old.begin () + first, h_old.begin () + last + 1, h.begin ());
  copy (Theta_old.begin () + first, Theta_old.begin () + last + 1,
	Theta.begin ());

  double time_left = dt;	// How much of the large time step left.
  double ddt = dt;		// We start with small == large time step.
  int number_of_time_step_reductions = 0;
  int iterations_with_this_time_step = 0;

  while (time_left > 0.0)
    {
      // Initialization for each small time step.
      int iterations_used = 0;
      if (ddt > time_left)
	ddt = time_left;

      for (unsigned int i = 0; i < size; i++)
	{
	  Ksum[i] = 0.0;
	  Kold[i] = soil.K (first + i, h[i]);
	}
      h_previous = h;
      Theta_previous = Theta;

      if (!top.flux_top ())
	h[first] = 0.0; 

      do
	{
	  h_conv = h;
	  iterations_used++;
	  iterations++;

	  // Calculate parameters.
	  for (unsigned int i = 0; i < size; i++)
	    {
	      
	      Ksum[i] += soil.K (first + i, h[i]);
	      K[i] = (Ksum[i] / iterations_used + Kold[i]) / 2;
	    }
	  if (bottom.flux_bottom ())
	    K[size] = K[size - 1];

	  internode (soil, first, last, K, Kplus);

	  // Calcualte nodes.
	  for (unsigned int i = 0; i < size; i++)
	    {
	      const double Cw1 = soil.Cw1 (first + i, h[i]);
	      // const double Cw2 = max (1e-5, soil.Cw2 (first + i, h[i]));
	      const double Cw2 = soil.Cw2 (first + i, h[i]);
	      const double dz = soil.dz (first + i);
	      const double z = soil.z (first + i);

	      if (i == 0)
		{
		  if (top.flux_top ())
		    {	
		      // Calculate upper boundary.
		      const double dz_plus = z - soil.z (first + i + 1);

		      b[i] = Cw2 + (ddt / dz) * (Kplus[i] / dz_plus);
		      d[i] = Theta[i] - Cw1 - ddt * S[first + i] 
			+ (ddt / dz)
			* (- (top.q () + flux_pond / ddt) - Kplus[i]);
		      
		      // Same as pressure boudnary.
		      a[i] = 0.0;
		      c[i] = - (ddt / dz) * (Kplus[i] / dz_plus);
		    }
		}
	      else if (i == 1 && !top.flux_top ())
		{
		  // Calculate upper boundary.
		  const double dz_plus = z - soil.z (first + i + 1);
		  const double dz_minus = soil.z (first + i - 1) - z;
		  b[i] = Cw2 
		    + (ddt / dz) * (Kplus[i - 1] / dz_minus + Kplus[i] / dz_plus);
		  d[i] = Theta[i] - Cw1 - ddt * S[first + i] 
		    + (ddt / dz) 
		    * (Kplus[i - 1] * (1 + h[i - 1] / dz_minus) - Kplus[i]);
		  a[i] = 0.0;
		  c[i] = - (ddt / dz) * (Kplus[i] / dz_plus);
		}
	      else if (i == size - 1)
		{
		  // Calculate lower boundary
		  const double dz_minus = soil.z (first + i - 1) - z;

		  if (bottom.flux_bottom ())
		    {
		      double q_bottom = - Kold[i];
		      b[i] = Cw2 + (ddt / dz) * (Kplus[i - 1] / dz_minus);
		      d[i] = Theta[i] - Cw1 - ddt * S[first + i] 
			+ (ddt / dz) * (Kplus[i - 1] + q_bottom);
		    }
		  else 
		    {
		      const double dz_plus = z - soil.z (first + i + 1);

		      b[i] = Cw2 + (ddt / dz) * (  Kplus[i - 1] / dz_minus 
						+ Kplus[i] / dz_plus);
		      d[i] = Theta[i] - Cw1 - ddt * S[first + i] 
			+ (ddt / dz)
			* (Kplus[i - 1] - Kplus[i] * (1 - h[i + 1] / dz_plus));
		    }
		  a[i] = - (ddt / dz) * (Kplus[i - 1] / dz_minus);
		  c[i] = 0.0;
		}
	      else
		{
		  // Calculate intermediate nodes.
		  const double dz_minus = soil.z (first + i - 1) - z;
		  const double dz_plus = z - soil.z (first + i + 1);

		  a[i] = - (ddt / dz) * (Kplus[i - 1] / dz_minus);
		  b[i] = Cw2 + (ddt / dz) * (  Kplus[i - 1] / dz_minus 
					    + Kplus[i] / dz_plus);
		  c[i] = - (ddt / dz) * (Kplus[i] / dz_plus);
		  d[i] = Theta[i] - Cw1 - ddt * S[first + i] 
		    +  (ddt / dz) * (Kplus[i - 1] - Kplus[i] );
		}
	    }
	  tridia (top.flux_top () ? 0 : 1, 
		  (bottom.flux_bottom () ? size : size - 1), 
		  a, b, c, d, h.begin ());
	}
      while (   !converges (h_conv, h) 
	     && iterations_used <= max_iterations);

      if (iterations_used > max_iterations)
	{
	  number_of_time_step_reductions++;
	  
	  if (number_of_time_step_reductions > max_time_step_reductions)
	    return false;

	  ddt /= time_step_reduction;
	  h = h_previous;
	}
      else
	{
	  // Calculate new water content.
	  for (unsigned int i = 0; i < size; i++)
	    Theta[i] = soil.Theta(first + i, h[i]);

	  bool accepted = true;	// Could the top accept the results?
	  // Amount of water we put into the top this small time step.
	  double delta_top_water = 88.0e88;

	  if (!top.flux_top ())
	    {
	      q_darcy (soil, first, last, h_previous, h, Theta_previous, Theta,
		       Kplus, S, ddt, q);

	      // We take water from flux pond first.
	      delta_top_water = q[first] * ddt + flux_pond;
	      // !! delta_top_water = q[first] * ddt;

	      if (real_flux_top)
		{
		  assert (real_top_q < 0);

		  if (-delta_top_water > - real_top_q * ddt * 1.001)
		  // !! if (-real_top_q * ddt * 1.001 + flux_pond < -delta_top_water)
		    {
		      // We can't retrieve water this fast from the flux top.
		      top.flux_top_on ();
		      accepted = false;
		    }
		  else
		    {
		      const bool ok = top.accept_top (delta_top_water);
		      assert (ok);
		      flux_pond += - (real_top_q - q[first]) * ddt;
		    }
		}
	      else if (!top.accept_top (q[first] * ddt))
		// We don't have more water in the pressure top.
		{
		  if (switched_top)
		    THROW ("Couldn't accept top flux");
		  else 
		    {
		      top.flux_top_on ();
		      accepted = false;
		    }
		}
	    }
	  else if (h[first] <= 0)
	    // We have a flux top, and unsaturated soil.
	    {
	      flux_pond = 0.0;
	      delta_top_water = top.q () * (ddt / time_left);
	      const bool ok = top.accept_top (delta_top_water);
	      assert (ok);
	    }
	  else if (!switched_top)
	    // We have satured soil, make it a pressure top.
	    {
	      top.flux_top_off ();
	      accepted = false;
	    }
	  else
	    THROW ("Couldn't drain top flux");

	  if (accepted)
	    {

#if 0
	      // This code checks that darcy and the mass preservation
	      // code gives the same results.
	      {
		q[first] = delta_top_water / ddt;
		for (int i = first; i < last; i++)
		  {
		    // Mass preservation.
		    q[i + 1] = (((Theta[i - first] - Theta_previous[i - first]) / ddt)
				+ S[i] * ddt) * soil.dz (i) + q[i];
		    if (h[i - first] >= 0.0 && h[i + 1 - first] >= 0.0)
		      continue;
		    const double darcy 
		      = -Kplus[i - first] 
		      * ((  (h[i - first] - h[i + 1 - first])
			  / (soil.z (i) - soil.z (i + 1)))
			 + 1);
		    if (fabs (q[i+1] / darcy - 1.0) > 0.01)
		      CERR << "q[" << i + 1 << "] = " << q[i+1] 
			   << ", darcy = " << darcy << "\n";
		  }
	      }
#endif
	      top_water += delta_top_water;
	      switched_top = false;
	      time_left -= ddt;
	      iterations_with_this_time_step++;

	      if (iterations_with_this_time_step > time_step_reduction)
		{
		  number_of_time_step_reductions--;
		  iterations_with_this_time_step = 0;
		  ddt *= time_step_reduction;
		}
	    }
	  else
	    {
	      switched_top = true;
	      Theta = Theta_previous;
	      h = h_previous;
	    }
	}
    }

  // Make it official.
  assert (h_new.size () >= first + size);
  copy (h.begin (), h.end (), h_new.begin () + first);
  assert (Theta_new.size () >= first + size);
  copy (Theta.begin (), Theta.end (), Theta_new.begin () + first);

  // Update Theta below groundwater table.
  if (!bottom.flux_bottom ())
    {
      for(unsigned int i = last; i < Theta_new.size (); i++)
	Theta_new[i] = soil.Theta (i, h[i]);
    }

#if 0
  q_darcy (soil, first, last, h_old, h_new, Theta_old, Theta_new,
	   Kplus, S, dt, q);
#else
  // We know flux on upper border, use mass preservation to
  // calculate flux below given the change in water content.
  q[first] = top_water / dt;
  for (int i = first; i <= last; i++)
    {
      q[i + 1] = (((Theta_new[i] - Theta_old[i]) / dt) + S[i])
	* soil.dz (i) + q[i];
    }
  return true;
#endif
}

bool
UZRichard::converges (const vector<double>& previous, 
		      const vector<double>& current) const
{
  unsigned int size = previous.size ();
  assert (current.size () == size);

  for (unsigned int i = 0; i < size; i++)
    {
      if (   fabs (current[i] - previous[i]) > max_absolute_difference
	  && (   previous[i] == 0.0
	      || current[i] == 0.0
	      || (  fabs ((current[i] - previous[i]) / previous[i])
		  > max_relative_difference)))
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

void
UZRichard::q_darcy (const Soil& soil, 
		    const int first, const int last, 
		    const vector<double>& /* h_previous */,
		    const vector<double>& h,
		    const vector<double>& Theta_previous,
		    const vector<double>& Theta,
		    const vector<double>& Kplus,
		    const vector<double>& S,
		    const double ddt,
		    vector<double>& q)
{
  // Find an unsaturated area.
  // Start looking 3/4 towards the bottom.
  const double start_pos = (soil.z (first) + soil.z (last) * 3.0) / 4.0;
  int start = soil.interval (start_pos) - 1;
  if (!(start < last - 2))
    THROW ("We need at least 2 numeric nodess below 3/4 depth for \
calculating flow with pressure top.");
  if (!(start > first + 1))
    THROW ("We need at least 1 numeric node above 3/4 depth for \
calculating flow with pressure top.");

  for (; start > 0; start--)
    {
      if (h[start - first] < 0.0 && h[start + 1 - first] < 0.0)
	break;
    }
  if (start == 0)
    THROW ("We couldn't find an unsaturated area.");
  // Use Darcy equation to find flux here.
  q[start + 1] = -Kplus[start - first] 
    * (  (  (h[start - first] - h[start + 1 - first])
	  / (soil.z (start) - soil.z (start + 1)))
       + 1);
  // Use mass preservation to find flux below and above.
  for (int i = start + 1; i <= last; i++)
    {
      q[i + 1] = (((Theta[i - first] - Theta_previous[i-first]) / ddt) + S[i])
	* soil.dz (i) + q[i];
    }
  for (int i = start; i >= first; i--)
    {
      q[i] = - (((Theta[i - first] - Theta_previous[i-first]) / ddt) + S[i])
	* soil.dz (i) + q[i + 1];
    }
}

void 
UZRichard::tick (const Soil& soil,
		 int first, UZtop& top, 
		 int last, UZbottom& bottom, 
		 const vector<double>& S,
		 const vector<double>& h_old,
		 const vector<double>& Theta_old,
		 vector<double>& h,
		 vector<double>& Theta,
		 vector<double>& q)
{
  iterations = 0;
  if (!richard (soil, first, top, last, bottom, 
		S, h_old, Theta_old, h, Theta, q))
    THROW ("Richard's Equation doesn't converge.");
    
  const bool accepted = bottom.accept_bottom (q[last + 1]);
  assert (accepted);

  q_up = q[first];
  q_down = q[last + 1];
}

void
UZRichard::output (Log& log, Filter& filter) const
{
  log.output ("q_up", filter, q_up, true);
  log.output ("q_down", filter, q_down, true);
  log.output ("iterations", filter, iterations, true);
}

UZRichard::UZRichard (const AttributeList& al)
  : UZmodel (al.name ("type")),
    // Variables.
    q_up (0.0),
    q_down (0.0),
    iterations (0),
    // Parameters.
    max_time_step_reductions (al.integer ("max_time_step_reductions")),
    time_step_reduction (al.integer ("time_step_reduction")),
    max_iterations (al.integer ("max_iterations")),
    max_absolute_difference (al.number ("max_absolute_difference")),
    max_relative_difference (al.number ("max_relative_difference"))
{ }

UZRichard::~UZRichard ()
{ }

// Add the UZRichard syntax to the syntax table.
static struct UZRichardSyntax
{
  static UZmodel& make (const AttributeList& al)
    {
      return *new UZRichard (al);
    }

  UZRichardSyntax ()
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();

      syntax.add ("max_time_step_reductions", Syntax::Integer, Syntax::Const);
      alist.add ("max_time_step_reductions", 4);
      syntax.add ("time_step_reduction", Syntax::Integer, Syntax::Const);
      alist.add ("time_step_reduction", 4);
      syntax.add ("max_iterations", Syntax::Integer, Syntax::Const);
      alist.add ("max_iterations", 25);
      syntax.add ("max_absolute_difference", Syntax::Number, Syntax::Const);
      alist.add ("max_absolute_difference", 0.0002);
      syntax.add ("max_relative_difference", Syntax::Number, Syntax::Const);
      alist.add ("max_relative_difference", 0.0001);
      syntax.add ("q_up", Syntax::Number, Syntax::LogOnly);
      syntax.add ("q_down", Syntax::Number, Syntax::LogOnly);
      syntax.add ("iterations", Syntax::Integer, Syntax::LogOnly);

      Librarian<UZmodel>::add_type ("richards", alist, syntax, &make);
    }
} UZRichard_syntax;


