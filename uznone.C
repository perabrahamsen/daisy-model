// uznone.C --- no water flow.

#include "uzmodel.h"
#include "soil.h"
#include "log.h"
#include "mathlib.h"

class UZNone : public UZmodel
{
  // UZmodel.
public:
  bool flux_top () const
    { return true; }
  double q () const
    { return 0.0; }
  void flux_top_on ()
    { }
  void flux_top_off ()
    { }
  bool accept_top (double)
    { return true; }
  bool flux_bottom () const
    { return true; }
  bool accept_bottom (double)
    { return true; }
  void output (Log&, Filter&) const
    { }

public:
  void tick (const Soil& /* soil */,
	     int first, UZtop& top, 
	     int last, UZbottom& bottom, 
	     const vector<double>& /* S */,
	     const vector<double>& h_old,
	     const vector<double>& Theta_old,
	     vector<double>& h,
	     vector<double>& Theta,
	     vector<double>& q)
    {
      // We need to call this to get stuff incorporated from surface.
      const bool ok = top.accept_top (0.0);
      assert (ok);

      for (int i = first; i <= last; i++)
	{
	  q[i] = 0.0;
	  Theta[i] = Theta_old[i];
	  h[i] = h_old[i];
	}
      q[last + 1] = 0.0;

      const bool accepted = bottom.accept_bottom (q[last + 1]);
      assert (accepted);
    }
  // Create and Destroy.
public:
  UZNone (const AttributeList& al)
    : UZmodel (al.name ("type"))
    { }
  ~UZNone ()
    { }
};

// Add the UZNone syntax to the syntax table.
static struct UZNoneSyntax
{
  static UZmodel& make (const AttributeList& al)
    {
      return *new UZNone (al);
    }

  UZNoneSyntax ()
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      Librarian<UZmodel>::add_type ("none", alist, syntax, &make);
    }
} UZNone_syntax;
