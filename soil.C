// soil.C

#include "soil.h"
#include "alist.h"
#include "syntax.h"
#include "mathlib.h"
#include <assert.h>
#include <iomanip.h>

double
Soil::Cw2 (int i, double h) const
{ 
  const double answer = horizon_[i]->hydraulic.Cw2 (h); 
  if (answer > 0.0)
    return answer;
  // We divide with this.
  return 1.0e-8;
}

double
Soil::MaxRootingDepth () const
{
  return max (-MaxRootingDepth_, z (size () - 1));
}

bool 
Soil::check () const
{
  bool ok = Geometry::check ();
  return ok;
}

bool
Soil::check_alist (const AttributeList& al)
{
  bool ok = Geometry::check_alist (al);

  const vector<AttributeList*>& layers = al.alist_sequence ("horizons");

  if (layers.size () < 1U)
    {
      CERR << "You need at least one horizon\n";
      ok = false;
    }
  double last = 0.0;

  for (unsigned int i = 0; i < layers.size (); i++)
    {
      double end = layers[i]->number ("end");
      if (end >= last)
	{
	  CERR << "Horizon endpoints must be monotonically decreasing\n";
	  ok = false;
	  break;
	}
      last = end;
    }

  if (ok)
    {
      // This check is only meaningful if zplus and layers are ok.
      const vector<double> zplus = al.number_sequence ("zplus");
  
      if (last != zplus[zplus.size() - 1])
	{
	  CERR <<
	    "The last horizon must end the same place as the last interval\n";
	  ok = false;
	}
    }
  return ok;
}  
  

void 
Soil::make_table (int i)
{
  COUT << "pF   Theta   Cw2           K           (depth " << z (i) << ").\n";
  for (double pF = 0.00; pF <= 5.0; pF += 0.01)
    {
      const double h = pF2h (pF);
      COUT << setw (4) << setprecision (3) << pF << " "
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
#if 0
  AttributeList& layer_alist = *new AttributeList ();
#endif
  layer_syntax.add ("end", Syntax::Number, Syntax::Const);
  layer_syntax.add ("horizon", Librarian<Horizon>::library (), Syntax::State);
  layer_syntax.order ("end", "horizon");
  syntax.add ("horizons", layer_syntax, Syntax::State, Syntax::Sequence);
#if 0
  alist.add ("horizons", layer_alist);
#endif
  syntax.add ("MaxRootingDepth", Syntax::Number, Syntax::Const);
  //  alist.add ("MaxRootingDepth", 100.0);
  syntax.add ("dispersivity", Syntax::Number, Syntax::Const);
}
  
Soil::Soil (const AttributeList& al)
  : Geometry (al),
    MaxRootingDepth_ (al.number ("MaxRootingDepth")),
    dispersivity_ (al.number ("dispersivity"))
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

