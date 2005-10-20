// column_inorganic.C -- Daisy simulation without organic matter and nitrogen.
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
#include "am.h"
#include "memutils.h"

using namespace std;

class ColumnInorganic : public ColumnBase
{
  // Actions.
public:
  void sow (Treelog& msg, const AttributeList&);
  void fertilize (const AttributeList&);
  void fertilize (const AttributeList&, double from, double to);
  void clear_second_year_utilization ();
  void add_residuals (vector<AM*>& residuals);

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
  void tick (Treelog&, const Time&, const Weather*);
  bool check_am (const AttributeList& am, Treelog& err) const;
  bool check_inner (Treelog&) const;

  // Create and Destroy.
public:
  void initialize (const Time& time, Treelog& err, 
		   const Weather* global_weather)
  {
    soil.initialize (*groundwater, -1, err);
    Treelog::Open nest (err, name);
    if (!initialize_common (time, err, global_weather))
      return;
    vegetation->initialize (time, soil, NULL, err);
  }
  Column& clone (symbol name) const
  { 
    AttributeList new_alist (alist);
    // BUG: TODO: Log state of 'this' to new_alist.
    new_alist.add ("type", name.name ());
    Block block (Librarian<Column>::library ().syntax (name), new_alist);
    return *new ColumnInorganic (block); 
  }
  ColumnInorganic (const Block& al)
    : ColumnBase (al)
  { }
  ~ColumnInorganic ()
  { }
};

void 
ColumnInorganic::sow (Treelog& msg, const AttributeList& al)
{ vegetation->sow (msg, al, soil); }


void
ColumnInorganic::fertilize (const AttributeList&)
{ }

void 
ColumnInorganic::fertilize (const AttributeList&, double, double)
{ }

void 
ColumnInorganic::clear_second_year_utilization ()
{ }

void
ColumnInorganic::add_residuals (vector<AM*>& residuals)
{
  sequence_delete (residuals.begin (), residuals.end ());
}

double
ColumnInorganic::soil_inorganic_nitrogen (double, double) const
{ return 0.0; }

double				// [kg N/ha]
ColumnInorganic::second_year_utilization () const
{ return 0.0; }

void 
ColumnInorganic::put_no3_m (const vector<double>&) // [g/cm^3]
{ daisy_assert (false); }

void 
ColumnInorganic::get_no3_m (vector<double>&) const // [g/cm^3]
{ daisy_assert (false); }

void 
ColumnInorganic::put_surface_no3 (double) // [g/cm^2]
{ daisy_assert (false); }

double 
ColumnInorganic::get_surface_no3 () const // [g/cm^2]
{ daisy_assert (false); }

double 
ColumnInorganic::get_smb_c_at (unsigned int) const //[g C/cm設
{ daisy_assert (false); }

double 
ColumnInorganic::get_co2_production_at (unsigned int) const // [g C/cm設
{ daisy_assert (false); }

void
ColumnInorganic::tick (Treelog& out,
		       const Time& time, const Weather* global_weather)
{
  // Base log.
  tick_base (out);

  // Weather.
  if (weather)
    weather->tick (time, out);
  const Weather& my_weather = *(weather ? weather : global_weather);

  // Early calculation.
  surface.mixture (soil_chemicals);
  soil_water.macro_tick (soil, surface, out);

  bioclimate->tick (time, surface, my_weather, 
                    *vegetation, soil, soil_water, soil_heat, out);
  vegetation->tick (time, *bioclimate, soil, NULL, soil_heat, soil_water,
		   NULL, NULL, 
		   residuals_DM, residuals_N_top, residuals_C_top,
		   residuals_N_soil, residuals_C_soil, out);
  groundwater->tick (soil, soil_water, surface.h (), soil_heat, time, out);

  // Transport.
  soil_heat.tick (time, soil, soil_water, surface, my_weather);
  soil_water.tick (soil, soil_heat,surface, *groundwater, out);
  soil_chemicals.tick (soil, soil_water, soil_heat, NULL, 
		       surface.chemicals_down (), out);
}

bool 
ColumnInorganic::check_am (const AttributeList&, Treelog&) const 
{ daisy_assert (false); }

bool
ColumnInorganic::check_inner (Treelog& err) const
{
  bool ok = true;
  {
    Treelog::Open nest (err, "Soil");
    if (!soil.check (-1, err))
      ok = false;
  }
  return ok;
}
static struct ColumnInorganicSyntax
{
  static Column& make (const Block& al)
  { return *new ColumnInorganic (al); }

  ColumnInorganicSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    ColumnBase::load_syntax (syntax, alist);
    syntax.add ("description", Syntax::String, Syntax::OptionalConst,
		"Description of this column."); 
    alist.add ("description", "Hansen et.al. 1990.\n\
Does not include organic matter or nitrogen.");
    Librarian<Column>::add_type ("inorganic", alist, syntax, &make);
  }
} column_syntax;
