// soil.C
// 
// Copyright 1996-2004 Per Abrahamsen and Søren Hansen
// Copyright 2000-2004 KVL.
//
// This file is part of Daisy.
// 
// Daisy is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser Public License as published by
// the Free Software Foundation; either version 2.1 of the License, or
// (at your option) any later version.
// 
// Daisy is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser Public License for more details.
// 
// You should have received a copy of the GNU Lesser Public License
// along with Daisy; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


#include "soil.h"
#include "hydraulic.h"
#include "tortuosity.h"
#include "groundwater.h"
#include "alist.h"
#include "syntax.h"
#include "mathlib.h"
#include "submodel.h"
#include "log.h"
#include "check.h"
#include "vcheck.h"
#include "plf.h"
#include <sstream>
#include "mathlib.h"
#if !(defined (__BORLANDC__) && __BORLANDC__ < 0x0550)
#include <iomanip>
#endif
#include <iostream>

using namespace std;

struct Soil::Implementation
{
  // Layers.
  struct Layer
  {
    // Content.
    const double end;
    auto_ptr<Horizon> horizon;

    // Simulation.
    void output (Log& log) const
    { output_derived (horizon, "horizon", log); }

    // Create and Destroy.
    static void load_syntax (Syntax& syntax, AttributeList& alist)
    { 
      alist.add ("description", "\
A location and content of a soil layer.");
      syntax.add ("end", "cm", Check::negative (), Syntax::Const,
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
    { }
  };
  /* const */ vector<Layer*> layers;
  const int original_layer_size; // Size before adding aquitard, for logging.

  // Parameters
  const bool has_zplus;
  const double MaxRootingDepth;
  const double dispersivity;
  const vector<double> border;

  bool has_attribute (const string& name) const
  { 
    bool missing = false;
    for (unsigned int i = 0; i < layers.size (); i++)
      if (!layers[i]->horizon->has_attribute (name))
	missing = true;
    return !missing;
  }
  
  // Create and Destroy.
  Implementation (const AttributeList& al)
    : layers (map_construct<Layer> (al.alist_sequence ("horizons"))),
      original_layer_size (layers.size ()),
      has_zplus (al.check ("zplus")),
      MaxRootingDepth (al.number ("MaxRootingDepth")),
      dispersivity (al.number ("dispersivity")),
      border (al.number_sequence ("border"))
  { }
  ~Implementation ()
  { sequence_delete (layers.begin (), layers.end ()); }
};

double 
Soil::K (int i, double h, double h_ice, double T) const
{ 
  static struct ViscosityFactor : public PLF
  {
    ViscosityFactor ()
    {
      add ( 0.0, 1002.0 / 1786.0);
      add ( 5.0, 1002.0 / 1519.0);
      add (10.0, 1002.0 / 1307.0);
      add (15.0, 1002.0 / 1139.0);
      add (20.0, 1002.0 / 1002.0);
      add (25.0, 1002.0 /  890.0);
      add (30.0, 1002.0 /  798.0);
      add (35.0, 1002.0 /  719.0);
      add (40.0, 1002.0 /  658.0);
    }
  } viscosity_factor;

  const double T_factor = viscosity_factor (T);
  if (h < h_ice)
    return horizon_[i]->hydraulic->K (h) * T_factor; 
  else
    return horizon_[i]->hydraulic->K (h_ice) * T_factor; 
}

double 
Soil::Cw1 (int i, double h, double h_ice) const
{ return Theta (i, h, h_ice) - Cw2 (i, h) * h; }

double
Soil::Cw2 (int i, double h) const
{ 
  const double answer = horizon_[i]->hydraulic->Cw2 (h); 
  if (answer > 0.0)
    return answer;
  // We divide with this.
  return 1.0e-8;
}

double Soil::Theta (int i, double h, double h_ice) const
{ 
  if (h < h_ice)
    return horizon_[i]->hydraulic->Theta (h);
  else
    return horizon_[i]->hydraulic->Theta (h_ice);
}

double 
Soil::Theta_res (int i) const
{ return horizon_[i]->hydraulic->Theta_res; }

double 
Soil::h (int i, double Theta) const
{ return horizon_[i]->hydraulic->h (Theta); }

double 
Soil::M (int i, double h) const
{ return horizon_[i]->hydraulic->M (h); }

double 
Soil::dispersivity (int) const
{ return impl.dispersivity; }

void
Soil::set_porosity (int i, double Theta)
{ horizon_[i]->hydraulic->set_porosity (Theta); }

double 
Soil::tortuosity_factor (int i, double Theta) const
{ return horizon_[i]->tortuosity->factor (*horizon_[i]->hydraulic, Theta); }

double 
Soil::anisotropy (int i) const
{ return horizon_[i]->anisotropy (); }

double 
Soil::dry_bulk_density (int i) const
{ return horizon_[i]->dry_bulk_density (); }

double 
Soil::clay (int i) const
{ return horizon_[i]->clay (); }

double 
Soil::texture_below (int i, double size) const
{ return horizon_[i]->texture_below (size); }

double 
Soil::humus (int i) const
{ return horizon_[i]->humus (); }

double 
Soil::humus_C (int i) const
{ return horizon_[i]->humus_C (); }

const std::vector<double>& 
Soil::SOM_fractions (int i) const
{ return horizon_[i]->SOM_fractions (); }

const std::vector<double>& 
Soil::SOM_C_per_N (int i) const
{ return horizon_[i]->SOM_C_per_N (); }

double
Soil::C_per_N (int i) const
{ return horizon_[i]->C_per_N (); }

double 
Soil::turnover_factor (int i) const
{ return horizon_[i]->turnover_factor (); }

double 
Soil::heat_conductivity (int i, double Theta, double Ice) const
{ return horizon_[i]->heat_conductivity (Theta, Ice); }

double 
Soil::heat_capacity (int i, double Theta, double Ice) const
{ return horizon_[i]->heat_capacity (Theta, Ice); }

bool
Soil::has_attribute (const string& name) const
{ return impl.has_attribute (name); }

bool 
Soil::has_attribute (int i, const std::string& name) const
{ return horizon_[i]->has_attribute (name); }

double 
Soil::get_attribute (int i, const std::string& name) const
{ return horizon_[i]->get_attribute (name); }

string
Soil::get_dimension (int i, const std::string& name) const
{ return horizon_[i]->get_dimension (name); }

void
Soil::output (Log& log) const
{
  static const symbol horizons_symbol ("horizons");
  if (log.check_interior (horizons_symbol))
    {
      Log::Open open (log, horizons_symbol);
      for (int i = 0; i < impl.original_layer_size; i++)
	{
	  Log::Unnamed unnamed (log);
	  impl.layers[i]->output (log);
	}
    }
}

void 
Soil::nitrification (const size_t i,
                     const double M, const double C, 
                     const double M_left,
                     const double h, const double T,
                     double& NH4, double& N2O, double& NO3) const
{ horizon_[i]->nitrification (M, C, M_left, h,  T, NH4, N2O, NO3); }

double
Soil::MaxRootingDepth () const
{
  return max (-impl.MaxRootingDepth, z (size () - 1));
}

double
Soil::end_of_first_horizon () const
{ 
  daisy_assert (impl.layers.size () > 0);
  return impl.layers[0]->end;
}

bool 
Soil::check (int som_size, Treelog& err) const
{
  bool ok = Geometry::check (err);

  if (som_size >= 0)
    {
      Treelog::Open nest (err, "horizons");
      for (unsigned int i = 0; i < impl.layers.size (); i++)
	{
	  const Horizon& horizon = *impl.layers[i]->horizon;
	  Treelog::Open nest (err, horizon.name);
	  const int f_size = horizon.SOM_fractions ().size ();
	  if (f_size > 0 && f_size != som_size)
	    {
	      Treelog::Open nest (err, "SOM_fractions");
	      std::ostringstream tmp;
	      tmp << "Need " << som_size << " fractions, got " << f_size;
	      err.error (tmp.str ());
	      ok = false;
	    }
	  const int n_size = horizon.SOM_C_per_N ().size ();
	  if (n_size != som_size)
	    {
	      Treelog::Open nest (err, "SOM_C_per_N");
	      std::ostringstream tmp;
	      tmp << "Need " << som_size << " C/N numbers, got " << n_size;
	      err.error (tmp.str ());
	      ok = false;
	    }
	}
    }
  return ok;
}

bool
Soil::check_border (const double border, Treelog& err) const
{
  bool ok = false;
  if (impl.has_zplus)
    {
      for (size_t i = 0; i < size (); i++)
        if (approximate (border, zplus (i)))
          ok = true;
    }
  else
    {
      for (size_t i = 0; i < impl.border.size (); i++)
        if (approximate (border, impl.border[i]))
          ok = true;
    }
  if (!ok)
    {
      std::ostringstream tmp;
      tmp << "No soil border near " << border 
             << " [cm], log results may be inexact";
      err.warning (tmp.str ());
    }
  return ok;
}

static bool
check_alist (const AttributeList& al, Treelog& err)
{
  bool ok = true;

  const vector<AttributeList*>& layers = al.alist_sequence ("horizons");

  if (layers.size () < 1U)
    {
      err.entry ("You need at least one horizon");
      ok = false;
    }
  double last = 0.0;

  for (unsigned int i = 0; i < layers.size (); i++)
    {
      double end = layers[i]->number ("end");
      if (end >= last)
	{
	  err.entry ("Horizon endpoints must be monotonically decreasing");
	  ok = false;
	  break;
	}
      last = end;
    }

  if (ok && al.check ("zplus"))
    {
      // This check is only meaningful if zplus and layers are ok.
      const vector<double> zplus = al.number_sequence ("zplus");
  
      if (last != zplus[zplus.size() - 1])
	{
	  err.entry ("The last horizon must end the same place as the "
		     "last interval");
	  ok = false;
	}
    }
  return ok;
}  

void 
Soil::make_table (int i)
{
  cout << "pF   Theta   Cw2           K           (depth " << z (i) << ").\n";
#if !(defined (__BORLANDC__) && __BORLANDC__ < 0x0550)
  for (double pF = 0.00; pF <= 5.0; pF += 0.01)
    {
      const double h = pF2h (pF);
      cout << setw (4) << setprecision (3) << pF << " "
	   << setw (6) << setprecision (5) << Theta (i, h, 0.0) << " "
	   << setw (12) << setprecision (11) << Cw2 (i, h) * 100.0 << " "
	   << setw (12) << setprecision (11) << K (i, h, 0.0, 20.0) / 3.6e5
	   << "\n";
    }
#endif
}

void
Soil::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  Geometry::load_syntax (syntax, alist);
  syntax.add_check (check_alist);
  alist.add ("submodel", "Soil");
  alist.add ("description", "\
The soil component provides the numeric and physical properties of the soil.");
  syntax.add_submodule_sequence ("horizons", Syntax::State, "\
Layered description of the soil properties.\n\
Some groundwater models, specifically 'pipe', may cause an extra horizon to\n\
be added below the one specified here if you do not also specify 'zplus'.",
				 Implementation::Layer::load_syntax);
  syntax.add ("MaxRootingDepth", "cm", Check::positive (), Syntax::Const,
	      "Depth at the end of the root zone (a positive number).");
  syntax.add ("dispersivity", "cm", Check::positive (), 
	      Syntax::Const, "Dispersion length.");
  alist.add ("dispersivity", 5.0);
  syntax.add ("border", "cm", Check::negative (), 
              Syntax::Const, Syntax::Sequence, "\
List of flux depths where a mass balance should be possible when logging.\n\
This attribute is ignored if zplus is specified explicitly.");
  syntax.add_check ("border", VCheck::decreasing ());
  vector<double> default_borders;
  default_borders.push_back (-100.0);
  alist.add ("border", default_borders);
}
  
Soil::Soil (const AttributeList& al)
  : Geometry (al),
    impl (*new Implementation (al))
{ }

void
Soil::initialize (Groundwater& groundwater, const int som_size, Treelog& msg)
{
  Treelog::Open nest (msg, "Soil");

  // Extra aquitard layer.
  if (!impl.has_zplus && groundwater.is_pipe ())
    {
      // Find parameters.
      const double Z_aquitard = groundwater.Z_aquitard ();
      const double K_aquitard = groundwater.K_aquitard ();
      const double old_end = impl.layers[impl.layers.size () - 1]->end;
      const double Z_horizon
	= (Z_aquitard > 5.0) ? floor (Z_aquitard / 3.0)	: (Z_aquitard / 3.0);
      const double new_end = old_end - Z_horizon;
      groundwater.set_Z_aquitard (Z_aquitard - Z_horizon);

      // Add layer.
      Library& library = Librarian<Horizon>::library ();
      static const symbol aquitard_symbol ("aquitard");
      static const symbol default_symbol ("default");
      if (!library.check (aquitard_symbol))
	{
	  // Create aquitard horizon.
	  AttributeList& alist 
	    = *new AttributeList (library.lookup (default_symbol));
	  alist.add ("clay", 50.0);
	  alist.add ("silt", 20.0);
	  alist.add ("sand", 29.99);
	  alist.add ("humus", 0.01);
	  alist.add ("dry_bulk_density", 2.0);
	  library.add_derived (aquitard_symbol, alist, default_symbol);
	}
      daisy_assert (library.check (aquitard_symbol));
      AttributeList horizon_alist (library.lookup (aquitard_symbol));
      horizon_alist.add ("type", "aquitard");
      AttributeList hydraulic_alist (horizon_alist.alist ("hydraulic"));
      hydraulic_alist.add ("K_sat", K_aquitard);
      horizon_alist.add ("hydraulic", hydraulic_alist);
      daisy_assert (library.syntax (aquitard_symbol).check (horizon_alist,
							    msg));
      Syntax layer_syntax;
      AttributeList layer_alist;
      Implementation::Layer::load_syntax (layer_syntax, layer_alist);
      layer_alist.add ("end", new_end);
      layer_alist.add ("horizon", horizon_alist);
      daisy_assert (layer_syntax.check (layer_alist, msg));
      impl.layers.push_back (new Implementation::Layer (layer_alist));
    }

  const vector<Implementation::Layer*>::const_iterator begin
    = impl.layers.begin ();
  const vector<Implementation::Layer*>::const_iterator end 
    = impl.layers.end ();
  daisy_assert (begin != end);
  vector<Implementation::Layer*>::const_iterator layer;

  // Initialize geometry.
  vector<double> fixed;
  {
    Treelog::Open nest (msg, "Horizons");
    double last = 0.0;
    int next_border = 0;
    for (layer = begin; layer != end; layer++)
      {
        const double current = (*layer)->end;
	daisy_assert (current < last);

	const bool top_soil = (layer == begin);
	(*layer)->horizon->initialize (top_soil, som_size, msg);

        while (next_border < impl.border.size ()
               && current < impl.border[next_border])
          {
            if (last > impl.border[next_border])
              fixed.push_back (impl.border[next_border]);
            next_border++;
          }
      
	last = current;
	fixed.push_back (last);
      }
  }
  initialize_zplus (groundwater, fixed, -impl.MaxRootingDepth, 
		    2 * impl.dispersivity, msg);

  // Initialize horizons.
  layer = begin;
  for (unsigned int i = 0; i < size (); i++)
    {
      if (zplus (i) < (*layer)->end)
	{
	  layer++;
	  daisy_assert (layer != end);
	}
      horizon_.push_back (&*((*layer)->horizon));
    }
}

Soil::~Soil ()
{ delete &impl; }

static Submodel::Register 
soil_submodel ("Soil", Soil::load_syntax);
