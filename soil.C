// soil.C

#include "soil.h"
#include "alist.h"
#include "syntax.h"
#include "mathlib.h"
#include "submodel.h"
#include "log.h"
#include <assert.h>
#include <iomanip.h>

struct Soil::Implementation
{
  // Layers.
  struct Layer
  {
    // Content.
    const double end;
    Horizon& horizon;

    // Simulation.
    void output (Log& log) const
    { output_derived (horizon, "horizon", log); }

    // Create and Destroy.
    static void load_syntax (Syntax& syntax, AttributeList& alist)
    { 
      alist.add ("description", "\
A location and content of a soil layer.");
      syntax.add ("end", "cm", Syntax::Const,
		  "End point of this layer (a negative number).");
      syntax.add ("horizon", Librarian<Horizon>::library (), 
		  "Soil properties of this layer.");
      syntax.order ("end", "horizon");
    }
    Layer (const AttributeList& al)
      : end (al.number ("end")),
	horizon (Librarian<Horizon>::create (al.alist ("horizon")))
    { }
    ~Layer ()
    { delete &horizon; }
  };
  const vector<Layer*> layers;

  vector<Horizon*> make_horizons (const Geometry& geometry)
  {
    vector<Horizon*> result;

    vector<Layer*>::const_iterator layer = layers.begin ();
    const vector<Layer*>::const_iterator end = layers.end ();

    assert (layer != end);
    for (unsigned int i = 0; i < geometry.size (); i++)
      {
	if (geometry.zplus (i) < (*layer)->end)
	  {
	    layer++;
	    assert (layer != end);
	  }
	result.push_back (&((*layer)->horizon));
      }
    return result;
  }
  
  // Parameters
  const double MaxRootingDepth;
  const double dispersivity;

  // Create and Destroy.
  Implementation (const AttributeList& al)
    : layers (map_construct<Layer> (al.alist_sequence ("horizons"))),
      MaxRootingDepth (al.number ("MaxRootingDepth")),
      dispersivity (al.number ("dispersivity"))
  { }
  ~Implementation ()
  { sequence_delete (layers.begin (), layers.end ()); }
};

double 
Soil::K (int i, double h, double h_ice) const
{ 
  if (h < h_ice)
    return horizon_[i]->hydraulic.K (h); 
  else
    return horizon_[i]->hydraulic.K (h_ice); 
}

double Soil::Theta (int i, double h, double h_ice) const
{ 
  if (h < h_ice)
    return horizon_[i]->hydraulic.Theta (h);
  else
    return horizon_[i]->hydraulic.Theta (h_ice);
}

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
Soil::dispersivity (int) const
{ return impl.dispersivity; }

void
Soil::output (Log& log) const
{ output_vector (impl.layers, "horizons", log); }

double
Soil::MaxRootingDepth () const
{
  return max (-impl.MaxRootingDepth, z (size () - 1));
}

bool 
Soil::check (ostream& err) const
{
  bool ok = Geometry::check (err);
  return ok;
}

static bool
check_alist (const AttributeList& al, ostream& err)
{
  bool ok = true;

  const vector<AttributeList*>& layers = al.alist_sequence ("horizons");

  if (layers.size () < 1U)
    {
      err << "You need at least one horizon\n";
      ok = false;
    }
  double last = 0.0;

  for (unsigned int i = 0; i < layers.size (); i++)
    {
      double end = layers[i]->number ("end");
      if (end >= last)
	{
	  err << "Horizon endpoints must be monotonically decreasing\n";
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
	  err <<
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
	   << setw (6) << setprecision (5) << Theta (i, h, 0.0) << " "
	   << setw (12) << setprecision (11) << Cw2 (i, h) * 100.0 << " "
	   << setw (12) << setprecision (11) << K (i, h, 0.0) / 3.6e5 << "\n";
    }
}

#ifdef BORLAND_TEMPLATES
 template class add_submodule_sequence<Soil::Implementation::Layer>;
#endif

void
Soil::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  Geometry::load_syntax (syntax, alist);
  syntax.add_check (check_alist);
  alist.add ("submodel", "Soil");
  alist.add ("description", "\
The soil component provides the numeric and physical properties of the soil.");
  add_submodule_sequence<Implementation::Layer> ("horizons", syntax, 
						 Syntax::State, "\
Layered description of the soil properties.");
  syntax.add ("MaxRootingDepth", "cm", Syntax::Const,
	      "Depth at the end of the root zone (a positive number).");
  //  alist.add ("MaxRootingDepth", 100.0);
  syntax.add ("dispersivity", "cm", Syntax::Const, "Dispersion length.");
  alist.add ("dispersivity", 6.0);
}
  
Soil::Soil (const AttributeList& al)
  : Geometry (al),
    impl (*new Implementation (al)),
    horizon_ (impl.make_horizons (*this))
{ };

Soil::~Soil ()
{ delete &impl; }

static Submodel::Register 
soil_submodel ("Soil", Soil::load_syntax);
