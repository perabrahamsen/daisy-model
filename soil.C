// soil.C

#include "soil.h"
#include "alist.h"
#include "syntax.h"
#include "mathlib.h"
#include <assert.h>
#include <iomanip.h>

double
Soil::MaxRootingDepth () const
{
  return max (-MaxRootingDepth_, z (size () - 1));
}

double 
Soil::EpFactor () const
{
  return EpFactor_;
}

double
Soil::EpInterchange () const
{
  return EpInterchange_;
}

bool 
Soil::check () const
{
  bool ok = Geometry::check ();
  if (horizon_.size () < 1)
    {
      cerr << "You need at least one horizon\n";
      ok = false;
    }
  return ok;
}

void 
Soil::make_table (int i)
{
  cout << "pF   Theta   Cw2           K           (depth " << z (i) << ").\n";
  for (double pF = 0.00; pF <= 5.0; pF += 0.01)
    {
      const double h = pF2h (pF);
      cout << setw (4) << setprecision (3) << pF << " "
	   << setw (6) << setprecision (5) << Theta (i, h) << " "
	   << setw (12) << setprecision (11) << Cw2 (i, h) * 100.0 << " "
	   << setw (12) << setprecision (11) << K (i, h) / 3.6e5 << "\n";
    }
}

void
Soil::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  Geometry::load_syntax (syntax, alist);
  Syntax& layer_syntax = *new Syntax ();
  AttributeList& layer_alist = *new AttributeList ();
  layer_syntax.add ("end", Syntax::Number, Syntax::Const);
  layer_syntax.add ("horizon", Librarian<Horizon>::library (), Syntax::State);
  layer_syntax.order ("end", "horizon");
  syntax.add ("horizons", layer_syntax, Syntax::State, Syntax::Sequence);
  alist.add ("horizons", layer_alist);
  syntax.add ("EpFactor", Syntax::Number, Syntax::Const);
  alist.add ("EpFactor", 0.8);
  syntax.add ("EpInterchange", Syntax::Number, Syntax::Const);
  alist.add ("EpInterchange", 0.6);
  syntax.add ("MaxRootingDepth", Syntax::Number, Syntax::Const);
  alist.add ("MaxRootingDepth", 100.0);
}
  
Soil::Soil (const AttributeList& al)
  : Geometry (al),
    EpFactor_ (al.number ("EpFactor")),
    EpInterchange_ (al.number ("EpInterchange")),
    MaxRootingDepth_ (al.number ("MaxRootingDepth"))
{
  vector<const AttributeList*>::const_iterator layer
    = al.alist_sequence ("horizons").begin ();
  const vector<const AttributeList*>::const_iterator end 
    = al.alist_sequence ("horizons").end ();

  if (layer != end)
    {
      const Horizon* hor 
	= &Librarian<Horizon>::create ((*layer)->alist ("horizon"));
      // double last = 0.0;
      for (unsigned int i = 0; i < size (); i++)
	{
	  double zpls = zplus (i);
	  if (zpls < (*layer)->number ("end"))
	    {
	      layer++;
	      assert (layer != end);
	      hor = &Librarian<Horizon>::create ((*layer)->alist ("horizon"));
	    }
	  horizon_.push_back (hor);
	  // last = zpls;
	}
      horizon_.push_back (hor);
    }
};

Soil::~Soil ()
{ }

