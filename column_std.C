// column_std.C -- Full Daisy simulation with organic matter and nitrogen.
// 
// Copyright 1996-2001 Per Abrahamsen and S鷨en Hansen
// Copyright 2000-2001 KVL.
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


#include "column_base.h"
#include "soil_NH4.h"
#include "soil_NO3.h"
#include "organic_matter.h"
#include "nitrification.h"
#include "denitrification.h"
#include "im.h"
#include "am.h"

class ColumnStandard : public ColumnBase
{
  // Content.
private:
  SoilNH4 soil_NH4;
  SoilNO3 soil_NO3;
  OrganicMatter organic_matter;
  Nitrification& nitrification;
  Denitrification denitrification;
  double second_year_utilization_;

  // Log variables.
private:
  double log_seed_N;
  double log_fertilized_NO3;
  double log_fertilized_NH4;
  double log_volatilization;
  double log_fertilized_Org_N;
  double log_fertilized_Org_C;
  double log_fertilized_DM;
  double log_first_year_utilization;
  double seed_N;
  double fertilized_NO3;
  double fertilized_NH4;
  double volatilization;
  double fertilized_Org_N;
  double fertilized_Org_C;
  double fertilized_DM;
  double first_year_utilization;

  // Actions.
public:
  void sow (Treelog&, const AttributeList&);
  void irrigate_overhead (double flux, double temp, const IM&);
  void irrigate_surface (double flux, double temp, const IM&);
  void irrigate_overhead (double flux, const IM&);
  void irrigate_surface (double flux, const IM&);
  void set_subsoil_irrigation (double flux, const IM& sm, 
			       double from, double to);
  void fertilize (const AttributeList&);
  void fertilize (const AttributeList&, double from, double to);
  void clear_second_year_utilization ();

  void add_residuals (vector<AM*>& residuals);
  void mix (Treelog&,
	    const Time&, double from, double to, double penetration = 1.0);
  void swap (Treelog&,
	     const Time&, double from, double middle, double to);

  // Conditions.
public:
  double soil_inorganic_nitrogen (double from, double to) const; // [kg N/ha]
  double second_year_utilization () const;// [kg N/ha]

  // Communication with external model.
public:
  void put_no3_m (const vector<double>& v); // [g/cm^3]
  void get_no3_m (vector<double>& v) const; // [g/cm^3]
  void put_surface_no3 (double no3); // [g/cm^2]
  double get_surface_no3 () const; // [g/cm^2]
  double get_smb_c_at (unsigned int i) const; //[g C/cm設
  double get_co2_production_at (unsigned int i) const; // [g C/cm設

  // Simulation.
public:
  void clear ();
  void tick (Treelog&, const Time&, const Weather*);
  void output_inner (Log&) const;
  bool check_am (const AttributeList& am, Treelog& err) const;
  bool check_inner (Treelog&) const;

  // Create and Destroy.
public:
  Column& clone (symbol name) const
  { 
    AttributeList new_alist (alist);
    // BUG: TODO: Log state of 'this' to new_alist.
    new_alist.add ("type", name.name ());
    return *new ColumnStandard (new_alist); 
  }
  ColumnStandard (const AttributeList& al);
  void initialize (const Time&, Treelog&, const Weather*);
  ~ColumnStandard ();
};

void 
ColumnStandard::sow (Treelog& msg, const AttributeList& al)
{ seed_N += vegetation.sow (msg, al, soil, organic_matter); }

// We need to convert from mm * mg N / liter to g N/m^2.
// mm / liter = 1/m^2
// mg = 1/1000 g
// Thus, we need to divide flux * solute with 1000 to get the surface input.

static const double irrigate_solute_surface_factor = 1.0 / 1000.0;

void 
ColumnStandard::irrigate_overhead (double flux, double temp, const IM& sm)
{
  ColumnBase::irrigate_overhead (flux, temp, sm);
  daisy_assert (flux >= 0.0);
  daisy_assert (sm.NH4 >= 0.0);
  daisy_assert (sm.NO3 >= 0.0);
  surface.fertilize (sm * (flux * irrigate_solute_surface_factor));
}

void 
ColumnStandard::irrigate_surface (double flux, double temp, const IM& sm)
{
  ColumnBase::irrigate_surface (flux, temp, sm);
  daisy_assert (flux >= 0.0);
  daisy_assert (sm.NH4 >= 0.0);
  daisy_assert (sm.NO3 >= 0.0);
  surface.fertilize (sm * (flux * irrigate_solute_surface_factor));
}

void 
ColumnStandard::irrigate_overhead (double flux, const IM& sm)
{
  ColumnBase::irrigate_overhead (flux, sm);
  daisy_assert (flux >= 0.0);
  daisy_assert (sm.NH4 >= 0.0);
  daisy_assert (sm.NO3 >= 0.0);
  surface.fertilize (sm * (flux * irrigate_solute_surface_factor));
}

void 
ColumnStandard::irrigate_surface (double flux, const IM& sm)
{
  ColumnBase::irrigate_surface (flux, sm);
  daisy_assert (flux >= 0.0);
  daisy_assert (sm.NH4 >= 0.0);
  daisy_assert (sm.NO3 >= 0.0);
  surface.fertilize (sm * (flux * irrigate_solute_surface_factor));
}

// We need to convert from mm * mg N / liter to g N/cm^2.
// mm / liter = 1/m^2 = 1/(100^2 cm^2) = 1/10000 1/cm^2 = 1.0e-4 1/cm^2
// mg = 1/1000 g = 1.0e-3 g
// Thus, we need to divide flux * solute with 1.0e-7 to get the surface input.
static const double irrigate_solute_soil_factor = 1.0e-7;

void
ColumnStandard::set_subsoil_irrigation (double flux, const IM& sm, 
					double from, double to)
{
  ColumnBase::set_subsoil_irrigation (flux, sm, from, to);
  daisy_assert (flux >= 0.0);
  daisy_assert (from <= 0.0);
  daisy_assert (to < from);
  soil_NH4.set_external_source (soil, 
				sm.NH4 * (flux * irrigate_solute_soil_factor), 
				from, to);
  soil_NO3.set_external_source (soil, 
				sm.NO3 * (flux * irrigate_solute_soil_factor),
				from, to);
}

void
ColumnStandard::fertilize (const AttributeList& al)
{
  // kg/ha -> g/cm^2
  const double conv = (1000.0 / ((100.0 * 100.0) * (100.0 * 100.0)));

  // Utilization log.
  first_year_utilization += AM::utilized_weight (al);
  second_year_utilization_ += AM::second_year_utilization (al);
  const double lost_NH4 = AM::get_volatilization (al);
  volatilization += lost_NH4;
  

  // Add inorganic matter.
  IM im (al);
  daisy_assert (im.NH4 >= 0.0);
  daisy_assert (im.NO3 >= 0.0);
  surface.fertilize (im);
  fertilized_NO3 += im.NO3 / conv; 
  fertilized_NH4 += im.NH4 / conv + lost_NH4;

  // Add organic matter, if any.
  if (al.name ("syntax") != "mineral")
    {
      AM& am = AM::create (al, soil);
      fertilized_Org_N += am.total_N (soil) / conv; 
      fertilized_Org_C += am.total_C (soil) / conv;
      if (al.check ("weight"))
	fertilized_DM += al.number ("weight");
      organic_matter.add (am);
    }
}

void 
ColumnStandard::fertilize (const AttributeList& al, double from, double to)
{
  daisy_assert (to < from);
  // kg/ha -> g/cm^2
  const double conv = (1000.0 / ((100.0 * 100.0) * (100.0 * 100.0)));

  // Utilization log.
  first_year_utilization += AM::utilized_weight (al);
  second_year_utilization_ += AM::second_year_utilization (al);
  const double lost_NH4 = AM::get_volatilization (al);
  volatilization += lost_NH4;

  // Add inorganic matter.
  IM im (al);
  daisy_assert (im.NH4 >= 0.0);
  daisy_assert (im.NO3 >= 0.0);
  soil_NO3.add_external (soil, soil_water, im.NO3, from, to);
  soil_NH4.add_external (soil, soil_water, im.NH4, from, to);
  fertilized_NO3 += im.NO3 / conv; 
  fertilized_NH4 += im.NH4 / conv + lost_NH4;

  // Add organic matter, if any.
  if (al.name ("syntax") != "mineral")
    {
      AM& am = AM::create (al, soil);
      fertilized_Org_N += am.total_N (soil) / conv; 
      fertilized_Org_C += am.total_C (soil) / conv;
      if (al.check ("weight"))
	fertilized_DM += al.number ("weight");
      am.mix (soil, from, to);
      organic_matter.add (am);
    }
}

void 
ColumnStandard::clear_second_year_utilization ()
{ second_year_utilization_ = 0.0; }

void
ColumnStandard::add_residuals (vector<AM*>& residuals)
{
  // Put the residuals in the soil.
  for (vector<AM*>::iterator residual = residuals.begin ();
       residual != residuals.end ();
       residual++)
    organic_matter.add (*(*residual));
}

void 
ColumnStandard::mix (Treelog& out, const Time& time,
		     double from, double to, double penetration)
{
  ColumnBase::mix (out, time, from, to, penetration);
  soil_NO3.mix (soil, soil_water, from, to);
  soil_NH4.mix (soil, soil_water, from, to);
  organic_matter.mix (soil, soil_water, from, to, penetration, time);
}

void 
ColumnStandard::swap (Treelog& out, 
		      const Time& time, double from, double middle, double to)
{
  ColumnBase::swap (out, time, from, middle, to);
  soil_NO3.swap (soil, soil_water, from, middle, to);
  soil_NH4.swap (soil, soil_water, from, middle, to);
  organic_matter.swap (soil, soil_water, from, middle, to, time);
}

double				// [kg N/ha]
ColumnStandard::soil_inorganic_nitrogen (double from, double to) const
{
  return (soil_NH4.total (soil, from, to) 
	  + soil_NO3.total (soil, from, to)) * 1.0e5; // g N/cm^2 -> kg N/ha
}  

double				// [kg N/ha]
ColumnStandard::second_year_utilization () const
{ return second_year_utilization_; }

void 
ColumnStandard::put_no3_m (const vector<double>& v) // [g/cm^3]
{ soil_NO3.put_M (soil, soil_water, v); }

void 
ColumnStandard::get_no3_m (vector<double>& v) const // [g/cm^3]
{ 
  const unsigned int size = soil.size ();

  v.erase (v.begin (), v.end ());
  for (unsigned int i = 0; i < size; i++)
    v.push_back (soil_NO3.M (i));
}

void 
ColumnStandard::put_surface_no3 (double no3) // [g/cm^2]
{ surface.put_no3 (no3); }

double 
ColumnStandard::get_surface_no3 () const // [g/cm^2]
{ return surface.get_no3 (); }

double 
ColumnStandard::get_smb_c_at (unsigned int i) const //[g C/cm設
{ return organic_matter.get_smb_c_at (i); }

double 
ColumnStandard::get_co2_production_at (unsigned int i) const // [g C/cm設
{ return organic_matter.CO2 (i); }

void
ColumnStandard::clear ()
{ 
  ColumnBase::clear ();

  soil_NO3.clear ();
  soil_NH4.clear ();
}

void
ColumnStandard::tick (Treelog& out, 
		      const Time& time, const Weather* global_weather)
{
  // Base log.
  tick_base (out);

  // Weather.
  if (weather)
    weather->tick (time, out);
  const Weather& my_weather = *(weather ? weather : global_weather);

  // Save logs.
  log_seed_N = seed_N;
  log_fertilized_NO3 = fertilized_NO3;
  log_fertilized_NH4 = fertilized_NH4;
  log_fertilized_Org_N = fertilized_Org_N;
  log_fertilized_Org_C = fertilized_Org_C;
  log_fertilized_DM = fertilized_DM;
  log_first_year_utilization = first_year_utilization;
  log_volatilization = volatilization;
  seed_N = 0.0;
  fertilized_NO3 = 0.0;
  fertilized_NH4 = 0.0;
  fertilized_Org_N = 0.0;
  fertilized_Org_C = 0.0;
  fertilized_DM = 0.0;
  first_year_utilization = 0.0;
  volatilization = 0.0;

  // Early calculation.
  IM soil_top_conc;
  soil_top_conc.NO3 = soil_NO3.C (0) / 10.0; // [g/cm^3] -> [g/cm^2/mm]
  soil_top_conc.NH4 = soil_NH4.C (0) / 10.0; // [g/cm^3] -> [g/cm^2/mm]
  surface.mixture (soil_top_conc);
  surface.mixture (soil_chemicals);
  soil_water.macro_tick (soil, surface, out);

  bioclimate.tick (surface, my_weather, 
		   vegetation, soil, soil_water, soil_heat, out);
  vegetation.tick (time, bioclimate, soil, &organic_matter, 
		   soil_heat, soil_water, &soil_NH4, &soil_NO3, 
		   residuals_DM, residuals_N_top, residuals_C_top, 
		   residuals_N_soil, residuals_C_soil, out);
  organic_matter.tick (soil, soil_water, soil_heat, 
		       soil_NO3, soil_NH4, out);
  nitrification.tick (soil, soil_water, soil_heat, soil_NO3, soil_NH4);
  denitrification.tick (soil, soil_water, soil_heat, soil_NO3, 
			organic_matter);
  groundwater.tick (soil, soil_water, soil_heat, time, out);

  // Transport.
  soil_heat.tick (time, soil, soil_water, surface, my_weather);
  soil_water.tick (soil, soil_heat, surface, groundwater, out);
  soil_chemicals.tick (soil, soil_water, soil_heat, &organic_matter,
		       surface.chemicals_down (), out);
  organic_matter.transport (soil, soil_water, out);
  soil_NO3.tick (soil, soil_water, surface.matter_flux ().NO3, out);
  soil_NH4.tick (soil, soil_water, surface.matter_flux ().NH4, out);
  
  // Once a month we clean up old AM from organic matter.
  if (time.hour () == 13 && time.mday () == 13)
    organic_matter.monthly (soil);
}

void
ColumnStandard::output_inner (Log& log) const
{
  output_submodule (soil_NH4, "SoilNH4", log);
  output_submodule (soil_NO3, "SoilNO3", log);
  static const symbol OrganicMatter_symbol ("OrganicMatter");
  if (log.check_interior (OrganicMatter_symbol))
    {
      Log::Open open (log, OrganicMatter_symbol);
      organic_matter.output (log, soil);
    }
  output_derived (nitrification, "Nitrification", log);
  output_submodule (denitrification, "Denitrification", log);
  output_value (second_year_utilization_, "second_year_utilization", log);
  output_value (log_seed_N, "seed_N", log);
  output_value (log_fertilized_NO3, "fertilized_NO3", log);
  output_value (log_fertilized_NH4, "fertilized_NH4", log);
  output_value (log_volatilization, "volatilization", log);
  output_value (log_fertilized_Org_N, "fertilized_Org_N", log);
  output_value (log_fertilized_Org_C, "fertilized_Org_C", log);
  output_value (log_fertilized_DM, "fertilized_DM", log);
  output_value (log_first_year_utilization, "first_year_utilization", log);
}

bool 
ColumnStandard::check_am (const AttributeList& am, Treelog& err) const 
{ 
  Treelog::Open nest (err, name);
  return organic_matter.check_am (am, err); 
}

bool
ColumnStandard::check_inner (Treelog& err) const
{
  const int n = soil.size ();
  bool ok = true;

  {
    Treelog::Open nest (err, "Soil");
    if (!soil.check (organic_matter.som_pools (), err))
      ok = false;
  }
  {
    Treelog::Open nest (err, "SoilNO3");
    if (!soil_NO3.check (n, err))
      ok = false;
  }
  {
    Treelog::Open nest (err, "SoilNH4");
    if (!soil_NH4.check (n, err))
      ok = false;
  }
  {
    Treelog::Open nest (err, "OrganicMatter");
    if (!organic_matter.check (err))
      ok = false;
  }
  return ok;
}

ColumnStandard::ColumnStandard (const AttributeList& al)
  : ColumnBase (al),
    soil_NH4 (al.alist ("SoilNH4")),
    soil_NO3 (al.alist ("SoilNO3")),
    organic_matter (al.alist ("OrganicMatter")),
    nitrification (Librarian<Nitrification>::create 
		   (al.alist ("Nitrification"))),
    denitrification (al.alist ("Denitrification")),
    second_year_utilization_ (al.number ("second_year_utilization")),
    log_seed_N (0.0),
    log_fertilized_NO3 (0.0),
    log_fertilized_NH4 (0.0),
    log_volatilization (0.0),
    log_fertilized_Org_N (0.0),
    log_fertilized_Org_C (0.0),
    log_fertilized_DM (0.0),
    log_first_year_utilization (0.0),
    seed_N (0.0),
    fertilized_NO3 (0.0),
    fertilized_NH4 (0.0),
    volatilization (0.0),
    fertilized_Org_N (0.0),
    fertilized_Org_C (0.0),
    fertilized_DM (0.0),
    first_year_utilization (0.0)
{ }

void 
ColumnStandard::initialize (const Time& time, Treelog& err, 
			    const Weather* global_weather)
{
  Treelog::Open nest (err, name);
  if (!global_weather && !weather)
    return;
  const double T_avg = weather 
    ? weather->average_temperature ()
    : global_weather->average_temperature ();

  soil.initialize (groundwater, organic_matter.som_pools (), err);
  initialize_common (time, err, global_weather);
  soil_NH4.initialize (alist.alist ("SoilNH4"), soil, soil_water, err);
  soil_NO3.initialize (alist.alist ("SoilNO3"), soil, soil_water, err);
  organic_matter.initialize (alist.alist ("OrganicMatter"), soil, soil_water, 
			     T_avg, err);
  vegetation.initialize (err, soil, organic_matter);
}

ColumnStandard::~ColumnStandard ()
{
  delete &nitrification;
}

static struct ColumnStandardSyntax
{
  static Column& make (const AttributeList& al)
  { return *new ColumnStandard (al); }

  ColumnStandardSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    ColumnBase::load_syntax (syntax, alist);
    syntax.add ("description", Syntax::String, Syntax::OptionalConst,
		"Description of this column."); 
    alist.add ("description", "Hansen et.al. 1990.");
    Librarian<Column>::add_type ("default", alist, syntax, &make);

    syntax.add_submodule ("SoilNH4", alist, Syntax::State,
			  "Ammonium transport and adsorption in soil.",
			  SoilNH4::load_syntax);
    syntax.add_submodule ("SoilNO3", alist, Syntax::State,
			  "Nitrate transport in soil.",
			  SoilNO3::load_syntax);
    syntax.add_submodule ("OrganicMatter", alist, Syntax::State, "\
The organic matter in the soil and on the surface.",
			  OrganicMatter::load_syntax);
    syntax.add ("Nitrification", Librarian<Nitrification>::library (),
		"The soil nitrification process.");
    AttributeList nitrification_alist;
    nitrification_alist.add ("type", "soil");
    nitrification_alist.add ("k_10", 2.08333333333e-7); // 5e-5/24 [1/h]
    nitrification_alist.add ("k", 5.0e-5); // [g N/cm^3]
    nitrification_alist.add ("active_underground", false);
    nitrification_alist.add ("active_groundwater", false);
    nitrification_alist.add ("heat_factor", PLF::empty ());
    nitrification_alist.add ("water_factor", PLF::empty ());
    nitrification_alist.add ("clay_factor", PLF::empty ());

    alist.add ("Nitrification", nitrification_alist);
    syntax.add_submodule ("Denitrification", alist, Syntax::State, "\
The denitrification process.",
			  Denitrification::load_syntax);
    syntax.add ("second_year_utilization", "kg N/ha", Syntax::State,
		"Estimated accumulated second year fertilizer effect.");
    alist.add ("second_year_utilization", 0.0);
    syntax.add ("seed_N", "kg N/ha", Syntax::LogOnly,
		"Amount of nitrogen in seed applied this time step.");
    syntax.add ("fertilized_NO3", "kg N/ha", Syntax::LogOnly,
		"Amount of nitrate applied this time step.");
    syntax.add ("fertilized_NH4", "kg N/ha", Syntax::LogOnly,
		"Amount of ammonium applied this time step.\n\
This included ammonium lost due to volatilization.");
    syntax.add ("fertilized_Org_N", "kg N/ha", Syntax::LogOnly,
		"Amount of organic bound nitrogen applied this time step.");
    syntax.add ("fertilized_Org_C", "kg C/ha", Syntax::LogOnly,
		"Amount of organic bound carbon applied this time step.");
    syntax.add ("fertilized_DM", "ton DM/ha", Syntax::LogOnly,
		"Amount of dry matter applied this time step.");
    syntax.add ("first_year_utilization", "kg N/ha", Syntax::LogOnly,
		"Estimated first year fertilizer effect.");
    syntax.add ("volatilization", "kg N/ha", Syntax::LogOnly,
		"Amount of NH4 volatilization.");
  }
} column_syntax;
