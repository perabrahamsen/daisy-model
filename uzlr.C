// uzlr.C --- using linear reservoirs to calculate water flow.

#include "uzmodel.h"
#include "soil.h"
#include "log.h"
#include "mathlib.h"

class UZlr : public UZmodel
{
  // Parameters.
private:
  const double h_fc;		// Field Capacity. [cm]
  const double z_top;		// Depth of layer with upwrd water movemnt [cm]

  // Variables.
private:
  double q_up;
  double q_down;

  // UZmodel.
public:
  bool flux_top () const
    { return true; };
  double q () const
    { return q_down; }
  void flux_top_on () const
    { }
  void flux_top_off () const
    { }
  bool accept_top (double)
    { return true; };
  bool flux_bottom () const
    { return true; };
  bool accept_bottom (double)
    { return true; };
  void output (Log&) const;

  // Simulate.
public:
  void tick (const Soil& soil,
	     unsigned int first, const UZtop& top, 
	     unsigned int last, const UZbottom& bottom, 
	     const vector<double>& S,
	     const vector<double>& h_old,
	     const vector<double>& Theta_old,
	     const vector<double>& h_ice,
	     vector<double>& h,
	     vector<double>& Theta,
	     vector<double>& q);

  // Create and Destroy.
public:
  UZlr (const AttributeList& par);
  ~UZlr ();
};

void 
UZlr::tick (const Soil& soil,
	    unsigned int first, const UZtop& top, 
	    unsigned int last, const UZbottom& /* bottom */, 
	    const vector<double>& S,
	    const vector<double>& h_old,
	    const vector<double>& Theta_old,
	    const vector<double>& h_ice,
	    vector<double>& h,
	    vector<double>& Theta,
	    vector<double>& q)
{
  // Upper border.
  const double K_sat = soil.K (0, 0.0, h_ice[0]);
  assert (K_sat > 0.0);
  q_up = q[first] = max (top.q (), -K_sat);

  // Use darcy for upward movement in the top.
  const bool use_darcy = (h_old[0] < h_fc) && (q_up > 0.0);
  const int to_darcy = soil.interval (z_top);

  // Intermediate nodes.
  for (int i = first; i <= last; i++)
    {
      const double dz = soil.dz (i);
      const double Theta_new = Theta_old[i] - q[i] * dt / dz - S[i] * dt;
      const double h_new = soil.h (i, Theta_new);
      const double K_new = soil.K (i, h_new, h_ice[i]);

      if (use_darcy && i < to_darcy)
	// Dry earth, near top.  Use darcy to move water up.
	{
	  const double dist = soil.z (i) - soil.z (i+1);
	  q[i+1] = max (K_new * ((h_old[i+1] - h_new) / dist - 1.0), 0.0);
	  Theta[i] = Theta_new + q[i+1] * dt / dz;
	  h[i] = soil.h (i, Theta[i]);
	}
      else if (h_new < h_fc)
	// Dry earth, no water movement.
	{
	  q[i+1] = 0.0;
	  Theta[i] = Theta_new;
	  h[i] = h_new;
	}
      else
	// Gravitational water movement.
	{
	  assert (finite (h_new));
	  const double Theta_sat = soil.Theta (i, 0.0, h_ice[i]);
	  const double Theta_fc = soil.Theta (i, h_fc, h_ice[i]);
	  const double Theta_next = Theta_new - K_new * dt / dz;
	  
	  if (Theta_next < Theta_fc)
	    {
	      q[i+1] = (Theta_fc - Theta_new) * dz / dt;
	      Theta[i] = Theta_fc;
	      h[i] = h_fc;
	    }
	  else if (Theta_next > Theta_sat)
	    {
	      q[i+1] = (Theta_sat - Theta_new) * dz / dt;
	      Theta[i] = Theta_sat;
	      h[i] = 0.0;
	    }
	  else
	    {
	      q[i+1] = -K_new;
	      Theta[i] = Theta_next;
	      h[i] = soil.h (i, Theta[i]);
	    }
	  assert (q[i+1] < 1e-10);
	}
      assert (finite (h[i]));
      assert (finite (Theta[i]));
      assert (finite (q[i+1]));
    }

  // Lower border.
  q_down = q[last + 1];

  // Check mass conservation.
  assert (approximate (soil.total (Theta_old)
		       - q_up * dt + q_down * dt
		       - soil.total (S), 
		       soil.total (Theta)));
}

void
UZlr::output (Log& log) const
{
  log.output ("q_up", q_up);
  log.output ("q_down", q_down);
}

UZlr::UZlr (const AttributeList& al)
  : UZmodel (al.name ("type")),
    h_fc (al.number ("h_fc")),
    z_top (al.number ("z_top")),
    q_up (0.0),
    q_down (0.0)
{ }

UZlr::~UZlr ()
{ }

// Add the UZlr syntax to the syntax table.
static struct UZlrSyntax
{
  static UZmodel& make (const AttributeList& al)
    {
      return *new UZlr (al);
    }

  UZlrSyntax ()
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "\
Use gravitational water movement for wet soil, where h < h_fc.\n\
There are no water movement when h > h_fc, except at the layers down\n\
to z_top, where there can be darcy movement.");
      
      // Variables.
      syntax.add ("q_up", "mm/h", Syntax::LogOnly, 
		  "Flux up through the surface.");
      syntax.add ("q_down", "mm/h", Syntax::LogOnly,
		  "Flux up through the bottom of the last node.");

      // Parameters.
      syntax.add ("h_fc", "cm", Syntax::Const, "Field capacity.");
      alist.add ("h_fc", -100.0);
      syntax.add ("z_top", "cm", Syntax::Const, 
		  "Depth of layer where upward water movement is possible.");
      alist.add ("z_top", -10.0);

      Librarian<UZmodel>::add_type ("lr", alist, syntax, &make);
    }
} UZlr_syntax;


