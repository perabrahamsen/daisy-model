// field.C
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
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

#define BUILD_DLL

#include "field.h"
#include "column.h"
#include "log.h"
#include "select.h"
#include "treelog.h"
#include "library.h"
#include "block.h"
#include "memutils.h"
#include "assertion.h"
#include "librarian.h"
#include "frame_model.h"
#include "mathlib.h"
#include "crop.h"
#include "metalib.h"

struct Field::Implementation
{
  // Columns.
  const Library& collib;
  typedef std::vector<Column*> ColumnList;
  ColumnList columns;
  bool total_area_known;        // If logs know total matching area.

  // Restrictions.
  Column* selected;
  void restrict (symbol name);
  void unrestrict ();

  // Actions.
  void sorption_table (const size_t cell, 
                       const double Theta, const double start,
                       const double factor, const int intervals,
                       Treelog& msg) const;
  void sow (const Metalib&, const FrameModel& crop, 
            double row_width, double row_pos, double seed,
            const Time& time, Treelog&);
  void sow (const Metalib&, Crop& crop, 
            double row_width, double row_pos, double seed,
            const Time& time, Treelog&);
  void irrigate (const double duration, const double flux, 
                 const double temp, Irrigation::target_t target,
                 const IM& sm, const boost::shared_ptr<Volume> volume,
                 const bool silence, 
                 Treelog& msg);
  void fertilize (const Metalib&, const FrameModel&, const Volume&, 
                  const Time&, Treelog& msg);
  void fertilize (const Metalib&, const FrameModel&, double from, double to, 
                  const Time&, Treelog& msg);
  void fertilize (const Metalib&, const FrameModel&, 
                  const Time&, Treelog& msg);
  void clear_second_year_utilization ();
  void emerge (symbol crop, Treelog&);
  void harvest (const Time&, symbol name,
		double stub_length, 
		double stem_harvest, 
		double leaf_harvest, 
		double sorg_harvest,
                const bool combine,
		std::vector<const Harvest*>&, Treelog&);
  void pluck (const Time&, symbol name,
              double stem_harvest, 
              double leaf_harvest, 
              double sorg_harvest,
              std::vector<const Harvest*>&, Treelog&);
  void mix (double from, double to, double penetration, double surface_loose,
            const double RR0, const Time&, Treelog&);
  void swap (double from, double middle, double to, 
             const double RR0, const Time&, Treelog&);
  void store_SOM (Treelog& msg);
  void restore_SOM (Treelog& msg);
  void set_porosity (double at, double Theta, Treelog& msg);
  void set_heat_source (double at, double value);
  void spray_overhead (symbol chemical, double amount, Treelog&); // [g/ha]
  void spray_surface (symbol chemical, double amount, Treelog&); // [g/ha]
  void set_surface_detention_capacity (double height); // [mm]
  void remove_solute (symbol chemical);
  double total_solute (symbol chemical) const; // [g/ha]

  // Conditions.
public:
  double daily_air_temperature () const; // [ dg C]
  double daily_precipitation () const; // [mm]
  double daily_global_radiation () const; // [W/m^2]
  double soil_temperature (double height) const; // [ cm -> dg C]
  double soil_water_potential (double height) const; // [cm -> cm]
  double soil_water_content (double from, double to) const; // [cm -> cm]
  double soil_inorganic_nitrogen (double from, double to) const; // [kg N/ha]
  double second_year_utilization () const;// [kg N/ha]
  // Current development stage for the crop named "crop", or
  // Crop::DSremove if no such crop is present.
  double crop_ds (symbol crop) const; 
  // Use defined phenological stage for the crop named "crop", or
  // Crop::DSremove if no such crop is present.
  double crop_stage (symbol crop) const; 
  // Drymatter in shoot [kg/ha], or negative if no such crop is present
  double crop_dm (symbol crop, double height) const; 
  // Drymatter in sorg [kg/ha], or negative if no such crop is present
  double crop_sorg_dm (symbol crop) const; 
  // All names of all crops on selected columns.
  std::string crop_names () const;
  // Simulation.
  void clear ();
  void tick_source (const Scope&, const Time&, Treelog&);
  double suggest_dt (const double weather_dt, const double T_air) const;
  void tick_move (const Metalib& metalib, 
                  const Time&, const Time&, double dt, const Weather*, 
                  const Scope&, Treelog&);

  // Find a specific column.
  Column* find (symbol name) const;

  // Create and destroy.
  bool check (const Weather* global_weather, const Time& from, const Time& to, 
	      const Scope& scope, Treelog&) const;
  bool check_am (const FrameModel& am, Treelog& err) const;
  bool check_z_border (double, Treelog& err) const;
  bool check_x_border (double, Treelog& err) const;
  bool check_y_border (double, Treelog& err) const;
  bool initialize (const Block&, const std::vector<const Scope*> scopes,
                   const Time&, const Weather*, const Scope&);
  Implementation (const Block& parent, const std::string& key);
  void summarize (Treelog& msg) const;
  ~Implementation ();
};

void 
Field::Implementation::restrict (symbol name)
{
  if (selected)
    throw ("Cannot restrict already restricted field");
  selected = find (name);

  if (!selected)
    throw (std::string ("Restricting to non-existing column '") + name + "'");
}

void 
Field::Implementation::unrestrict ()
{
  daisy_safe_assert (selected);
  selected = NULL;
}

void 
Field::Implementation::sorption_table (const size_t cell, 
                                       const double Theta, const double start,
                                       const double factor, const int intervals,
                                       Treelog& msg) const
{
  if (selected)
    {
      Treelog::Open nest (msg, selected->objid);
      selected->sorption_table (cell, Theta, start, factor, intervals, msg);
    }
  else 
    {
      for (ColumnList::const_iterator i = columns.begin ();
	   i != columns.end ();
	   i++)
	{
	  Treelog::Open nest (msg, (*i)->objid);
	  (*i)->sorption_table (cell, Theta, start, factor, intervals, msg);
	}
    }
}

void 
Field::Implementation::sow (const Metalib& metalib, const FrameModel& crop, 
                            const double row_width, const double row_pos,
                            const double seed,
                            const Time& time, Treelog& msg)
{
  if (selected)
    {
      Treelog::Open nest (msg, selected->objid);
      selected->sow (metalib, crop, row_width, row_pos, seed, time, msg);
    }
  else 
    {
      for (ColumnList::iterator i = columns.begin ();
	   i != columns.end ();
	   i++)
	{
	  Treelog::Open nest (msg, (*i)->objid);
	  (*i)->sow (metalib, crop, row_width, row_pos, seed, time, msg);
	}
    }
}

void 
Field::Implementation::sow (const Metalib& metalib, Crop& crop, 
                            const double row_width, const double row_pos,
                            const double seed,
                            const Time& time, Treelog& msg)
{
  if (selected)
    {
      Treelog::Open nest (msg, selected->objid);
      selected->sow (metalib, crop, row_width, row_pos, seed, time, msg);
    }
  else 
    {
      for (ColumnList::iterator i = columns.begin ();
	   i != columns.end ();
	   i++)
	{
	  Treelog::Open nest (msg, (*i)->objid);
	  (*i)->sow (metalib, crop, row_width, row_pos, seed, time, msg);
	}
    }
}

void 
Field::Implementation::irrigate (const double duration, const double flux, 
                                 const double temp, Irrigation::target_t target,
                                 const IM& sm,
                                 const boost::shared_ptr<Volume> volume,
                                 const bool silence, Treelog& msg)
{
  if (selected)
    selected->irrigate (duration, flux, temp, target, sm, volume, silence, msg);
  else for (ColumnList::iterator i = columns.begin ();
	    i != columns.end ();
	    i++)
	 (*i)->irrigate (duration, flux, temp, target, sm, volume,
                         silence, msg);
}

void 
Field::Implementation::fertilize (const Metalib& metalib, const FrameModel& al, 
				  const double from, const double to,
                                  const Time& now, Treelog& msg)
{
  if (selected)
    selected->fertilize (metalib, al, from, to, now, msg);
  else for (ColumnList::iterator i = columns.begin ();
	    i != columns.end ();
	    i++)
	 (*i)->fertilize (metalib, al, from, to, now, msg);
}

void 
Field::Implementation::fertilize (const Metalib& metalib, const FrameModel& al, 
                                  const Volume& volume, const Time& now,
                                  Treelog& msg)
{
  if (selected)
    selected->fertilize (metalib, al, volume, now, msg);
  else for (ColumnList::iterator i = columns.begin ();
	    i != columns.end ();
	    i++)
	 (*i)->fertilize (metalib, al, volume, now, msg);
}

void 
Field::Implementation::fertilize (const Metalib& metalib, const FrameModel& al, 
                                  const Time& now, Treelog& msg)
{
  if (selected)
    {
      selected->fertilize (metalib, al, now, msg);
    }
  else 
    {
      for (ColumnList::iterator i = columns.begin ();
	   i != columns.end ();
	   i++)
	{
	  (*i)->fertilize (metalib, al, now, msg);
	}
    }
}

void 
Field::Implementation::clear_second_year_utilization ()
{
  if (selected)
    selected->clear_second_year_utilization ();
  else for (ColumnList::iterator i = columns.begin ();
	    i != columns.end ();
	    i++)
    (*i)->clear_second_year_utilization ();
}

void
Field::Implementation::emerge (symbol name, Treelog& out)
{
  if (selected)
    {
      Treelog::Open nest (out, selected->objid);
      selected->emerge (name, out);
    }
  else
    {
      for (ColumnList::iterator i = columns.begin ();
	   i != columns.end ();
	   i++)
	{
	  Treelog::Open nest (out, (*i)->objid);
	  (*i)->emerge (name, out);
	}
    }
}

void
Field::Implementation::harvest (const Time& time, 
                                const symbol name,
				const double stub_length, 
				const double stem_harvest, 
				const double leaf_harvest, 
				const double sorg_harvest,
                                const bool combine,
				std::vector<const Harvest*>& total,
				Treelog& out)
{
  if (selected)
    {
      Treelog::Open nest (out, selected->objid);
      selected->harvest (time, name,
			 stub_length,
			 stem_harvest, leaf_harvest, sorg_harvest, combine, 
                         total, out);
    }
  else
    {
      for (ColumnList::iterator i = columns.begin ();
	   i != columns.end ();
	   i++)
	{
	  Treelog::Open nest (out, (*i)->objid);
	  (*i)->harvest (time, name,
			 stub_length,
			 stem_harvest, leaf_harvest, sorg_harvest, combine,
                         total, out);
	}
    }
}

void
Field::Implementation::pluck (const Time& time, 
                              const symbol name,
                              const double stem_harvest, 
                              const double leaf_harvest, 
                              const double sorg_harvest,
                              std::vector<const Harvest*>& total,
                              Treelog& out)
{
  if (selected)
    {
      Treelog::Open nest (out, selected->objid);
      selected->pluck (time, name,
                       stem_harvest, leaf_harvest, sorg_harvest, 
                       total, out);
    }
  else
    {
      for (ColumnList::iterator i = columns.begin ();
	   i != columns.end ();
	   i++)
	{
	  Treelog::Open nest (out, (*i)->objid);
	  (*i)->pluck (time, name,
                       stem_harvest, leaf_harvest, sorg_harvest, 
                       total, out);
	}
    }
}

void 
Field::Implementation::mix (const double from, const double to,
                            const double penetration,
                            const double surface_loose, 
                            const double RR0, const Time& time,
			    Treelog& msg)
{
  if (selected)
    {
      Treelog::Open nest (msg, selected->objid); 
      selected->mix (from, to, penetration, surface_loose, RR0, 
                     time, msg);
    }
  else for (ColumnList::iterator i = columns.begin ();
	    i != columns.end ();
	    i++)
    {
      Treelog::Open nest (msg, (*i)->objid);
      (*i)->mix (from, to, penetration, surface_loose, RR0, time, msg);
    }
}

void 
Field::Implementation::swap (const double from, const double middle, 
                             const double to, 
                             const double RR0, const Time& time, Treelog& msg)
{
  if (selected)
    {
      Treelog::Open nest (msg, selected->objid);
      selected->swap (from, middle, to, RR0, time, msg);
    }
  else for (ColumnList::iterator i = columns.begin ();
	    i != columns.end ();
	    i++)
    {
      Treelog::Open nest (msg, (*i)->objid);
      (*i)->swap (from, middle, to, RR0, time, msg);
    }
}

  void store_SOM (Treelog& msg);
  void restore_SOM (Treelog& msg);

void 
Field::Implementation::store_SOM (Treelog& msg)
{
  if (selected)
    selected->store_SOM (msg);
  else for (ColumnList::iterator i = columns.begin ();
	    i != columns.end ();
	    i++)
         (*i)->store_SOM (msg);
}

void 
Field::Implementation::restore_SOM (Treelog& msg)
{
  if (selected)
    selected->restore_SOM (msg);
  else for (ColumnList::iterator i = columns.begin ();
	    i != columns.end ();
	    i++)
         (*i)->restore_SOM (msg);
}

void 
Field::Implementation::set_porosity (const double at, const double Theta, 
                                     Treelog& msg)
{
  if (selected)
    selected->set_porosity (at, Theta, msg);
  else for (ColumnList::iterator i = columns.begin ();
	    i != columns.end ();
	    i++)
         (*i)->set_porosity (at, Theta, msg);
}

void 
Field::Implementation::spray_overhead (const symbol chemical, 
                                       const double amount,
                                       Treelog& msg) // [g/ha]
{
  if (selected)
    selected->spray_overhead (chemical, amount, msg);
  else for (ColumnList::iterator i = columns.begin ();
	    i != columns.end ();
	    i++)
    (*i)->spray_overhead (chemical, amount, msg);
}

void 
Field::Implementation::spray_surface (const symbol chemical, 
                                       const double amount,
                                       Treelog& msg) // [g/ha]
{
  if (selected)
    selected->spray_surface (chemical, amount, msg);
  else for (ColumnList::iterator i = columns.begin ();
	    i != columns.end ();
	    i++)
    (*i)->spray_surface (chemical, amount, msg);
}

void 
Field::Implementation::set_surface_detention_capacity (double height) // [mm]
{
  if (selected)
    selected->set_surface_detention_capacity (height);
  else for (ColumnList::iterator i = columns.begin ();
	    i != columns.end ();
	    i++)
    (*i)->set_surface_detention_capacity (height);
}

void 
Field::Implementation::remove_solute (const symbol chemical)
{
  if (selected)
    selected->remove_solute (chemical);
  else for (ColumnList::iterator i = columns.begin ();
	    i != columns.end ();
	    i++)
    (*i)->remove_solute (chemical);;
}

double				// [g/ha]
Field::Implementation::total_solute (const symbol chemical) const
{
  double total = 0.0;
  if (selected)
    total = selected->total_solute (chemical);
  else for (auto c : columns)
	 total += c->total_solute (chemical);
  return total;
}

double 
Field::Implementation::daily_air_temperature () const // [ dg C ]
{
  if (selected)
    return selected->daily_air_temperature (); 
  if (columns.size () != 1)
    throw ("Cannot find daily air_temperatur for multiple columns");

  return columns[0]->daily_air_temperature ();
}

double 
Field::Implementation::daily_precipitation () const // [ dg C ]
{
  if (selected)
    return selected->daily_precipitation (); 
  if (columns.size () != 1)
    throw ("Cannot find daily precipitation for multiple columns");

  return columns[0]->daily_precipitation ();
}

double 
Field::Implementation::daily_global_radiation () const // [ dg C ]
{
  if (selected)
    return selected->daily_global_radiation (); 
  if (columns.size () != 1)
    throw ("Cannot find daily global radiation for multiple columns");

  return columns[0]->daily_global_radiation ();
}

double 
Field::Implementation::soil_temperature (double height) const  // [ cm -> dg C]
{ 
  if (selected)
    return selected->soil_temperature (height); 
  if (columns.size () != 1)
    throw ("Cannot take soil temperature of multiple columns");

  return columns[0]->soil_temperature (height);
}

double 
Field::Implementation::soil_water_potential (double height) const // [cm -> cm]
{
  if (selected)
    return selected->soil_water_potential (height); 
  if (columns.size () != 1)
    throw ("Cannot take soil water potential of multiple columns");

  return columns[0]->soil_water_potential (height);
}

double 
Field::Implementation::soil_water_content (double from /* [cm] */,
					   double to /* [cm] */) const // [cm]
{
  if (selected)
    return selected->soil_water_content (from, to); 
  if (columns.size () != 1)
    throw ("Cannot take soil water potential of multiple columns");

  return columns[0]->soil_water_content (from, to);
}

double 
Field::Implementation::soil_inorganic_nitrogen (double from,  // [kg N/ha]
						double to) const
{
  if (selected)
    return selected->soil_inorganic_nitrogen (from, to); 
  if (columns.size () != 1)
    throw ("Cannot find inorganic nitrogen in multiple columns");

  return columns[0]->soil_inorganic_nitrogen (from, to);
}

double 
Field::Implementation::second_year_utilization () const
{
  if (selected)
    return selected->second_year_utilization (); 
  if (columns.size () != 1)
    throw ("Cannot find second year utilization in multiple columns");

  return columns[0]->second_year_utilization ();
}

double 
Field::Implementation::crop_ds (symbol crop) const
{ 
  if (selected)
    return selected->crop_ds (crop);
  
  double DS = 0.0;
  double total_area = 0.0;
  for (ColumnList::const_iterator i = columns.begin ();
       i != columns.end ();
       i++)
    {
      const double area = (*i)->area;
      const double this_DS = (*i)->crop_ds (crop);
      if (!approximate (this_DS, Crop::DSremove))
        {
          total_area += area;
          DS += this_DS;
        }
    }
  if (total_area > 0.0)
    return DS / total_area;

  return Crop::DSremove;
} 

double 
Field::Implementation::crop_stage (symbol crop) const
{ 
  if (selected)
    return selected->crop_stage (crop);
  
  double stage = 0.0;
  double total_area = 0.0;
  for (ColumnList::const_iterator i = columns.begin ();
       i != columns.end ();
       i++)
    {
      const double area = (*i)->area;
      const double this_stage = (*i)->crop_stage (crop);
      if (!approximate (this_stage, Crop::DSremove))
        {
          total_area += area;
          stage += this_stage;
        }
    }
  if (total_area > 0.0)
    return stage / total_area;

  return Crop::DSremove;
} 

double 
Field::Implementation::crop_dm (const symbol crop, const double height) const
{
  if (selected)
    return selected->crop_dm (crop, height);
  
  // We find the total DM for all the columns.
  double DM = 0.0;
  double total_area = 0.0;
  for (ColumnList::const_iterator i = columns.begin ();
       i != columns.end ();
       i++)
    {
      const double area = (*i)->area;
      total_area += area;
      DM += (*i)->crop_dm (crop, height) * area;
    }
  return DM / total_area;
}
  
double 
Field::Implementation::crop_sorg_dm (const symbol crop) const
{
  if (selected)
    return selected->crop_sorg_dm (crop);
  
  // We find the total DM for all the columns.
  double DM = 0.0;
  for (ColumnList::const_iterator i = columns.begin ();
       i != columns.end ();
       i++)
    DM += (*i)->crop_sorg_dm (crop);
  return DM;
}
  
std::string
Field::Implementation::crop_names () const
{
  if (selected)
    return selected->crop_names ();
  
  std::string result = "";
  for (ColumnList::const_iterator i = columns.begin ();
       i != columns.end ();
       i++)
    {
      if (result != "")
	result += ",";
      result += (*i)->crop_names ();
    }
  return result;
}
  
void 
Field::Implementation::clear ()
{
  for (ColumnList::const_iterator i = columns.begin ();
       i != columns.end ();
       i++)
    (*i)->clear ();
}

void 
Field::Implementation::tick_source (const Scope& parent_scope, 
                                    const Time& time_end, Treelog& msg)
{
  if (columns.size () == 1)
    (*(columns.begin ()))->tick_source (parent_scope, time_end, msg);
  else
    for (ColumnList::const_iterator i = columns.begin ();
         i != columns.end ();
         i++)
      {
        Treelog::Open nest (msg, "Column " + (*i)->objid);
        (*i)->tick_source (parent_scope, time_end, msg);
      }
}

double
Field::Implementation::suggest_dt (const double weather_dt,
                                   const double T_air) const
{
  if (columns.size () == 1)
    return (*(columns.begin ()))->suggest_dt (weather_dt, T_air);
  
  double dt = 0.0;
  for (ColumnList::const_iterator i = columns.begin ();
       i != columns.end ();
       i++)
    {
      const double col_dt = (*i)->suggest_dt (weather_dt, T_air);
      if (std::isnormal (col_dt)
          && (!std::isnormal (dt) || dt > col_dt))
        dt = col_dt;
    }
  return dt;
}

void 
Field::Implementation::tick_move (const Metalib& metalib, 
                                  const Time& time, const Time& time_end,
                                  const double dt, 
                                  const Weather* weather, const Scope& scope,
                                  Treelog& msg)
{
  if (columns.size () == 1)
    (*(columns.begin ()))->tick_move (metalib, time, time_end, dt,
                                      weather, scope, msg);
  else
    for (ColumnList::const_iterator i = columns.begin ();
         i != columns.end ();
         i++)
      {
        Treelog::Open nest (msg, "Column " + (*i)->objid);
        (*i)->tick_move (metalib, time, time_end, dt, weather, scope, msg);
      }
}

Column* 
Field::Implementation::find (symbol name) const
{
  for (ColumnList::const_iterator i = columns.begin ();
       i != columns.end ();
       i++)
    { 
      if ((*i)->objid == name)
	return *i;
    }
  return NULL;
}

bool 
Field::Implementation::check (const Weather *const global_weather,
			      const Time& from, const Time& to, 
			      const Scope& scope, Treelog& err) const
{ 
  bool ok = true;
  for (ColumnList::const_iterator i = columns.begin ();
       i != columns.end ();
       i++)
    {
      Treelog::Open nest (err, (*i) ? (*i)->objid.name ().c_str () : "error");
      if ((*i) == NULL || !(*i)->check (global_weather, from, to, scope, err))
	ok = false;
    }
  return ok;
}

bool 
Field::Implementation::check_am (const FrameModel& am, Treelog& err) const
{ 
  Treelog::Open nest (err, am.type_name ());

  bool ok = true;
  for (ColumnList::const_iterator i = columns.begin ();
       i != columns.end ();
       i++)
    {
      Treelog::Open nest (err, (*i)->objid);
      if (!(*i)->check_am (am, err))
	ok = false;
    }
  return ok;
}

bool
Field::Implementation::check_z_border (const double value, Treelog& err) const
{ 
  bool ok = true;
  for (ColumnList::const_iterator i = columns.begin ();
       i != columns.end ();
       i++)
    if (!(*i)->check_z_border (value, err))
      ok = false;
  return ok;
}

bool
Field::Implementation::check_x_border (const double value, Treelog& err) const
{ 
  bool ok = true;
  for (ColumnList::const_iterator i = columns.begin ();
       i != columns.end ();
       i++)
    if (!(*i)->check_x_border (value, err))
      ok = false;
  return ok;
}

bool
Field::Implementation::check_y_border (const double value, Treelog& err) const
{ 
  bool ok = true;
  for (ColumnList::const_iterator i = columns.begin ();
       i != columns.end ();
       i++)
    if (!(*i)->check_y_border (value, err))
      ok = false;
  return ok;
}

bool
Field::Implementation::initialize (const Block& block,
                                   const std::vector<const Scope*> scopes,
                                   const Time& time, const Weather* weather,
				   const Scope& scope)
{
  bool ok = true;
  for (ColumnList::const_iterator i = columns.begin ();
       i != columns.end ();
       i++)
    if (!(*i)->initialize (block.metalib (), scopes, time, weather, scope, block.msg ()))
      ok = false;
  return ok;
}

Field::Implementation::Implementation (const Block& parent, 
				       const std::string& key)
  : collib (parent.metalib ().library (Column::component)),
    columns (Librarian::build_vector<Column> (parent, key)),
    total_area_known (false),
    selected (NULL)
{ }

void
Field::Implementation::summarize (Treelog& msg) const
{ 
  for (ColumnList::const_iterator i = columns.begin ();
       i != columns.end ();
       i++)
    (*i)->summarize (msg);
}


Field::Implementation::~Implementation ()
{
  daisy_safe_assert (selected == NULL);
  sequence_delete (columns.begin (), columns.end ()); 
}

Field::Restrict::Restrict (Field& f, symbol name)
  : field (f)
{ 
  field.impl->restrict (name); 
}

Field::Restrict::~Restrict ()
{ field.impl->unrestrict (); }

void 
Field::sorption_table (const size_t cell, 
                       const double Theta, const double start,
                       const double factor, const int intervals,
                       Treelog& msg) const
{ impl->sorption_table (cell, Theta, start, factor, intervals, msg); }

void 
Field::sow (const Metalib& metalib, const FrameModel& crop,
            const double row_width, const double row_pos, const double seed,
            const Time& time, Treelog& msg)
{ impl->sow (metalib, crop, row_width, row_pos, seed, time, msg); }

void 
Field::sow (const Metalib& metalib, Crop& crop,
            const double row_width, const double row_pos, const double seed,
            const Time& time, Treelog& msg)
{ impl->sow (metalib, crop, row_width, row_pos, seed, time, msg); }

void 
Field::irrigate (const double duration, const double flux, 
                 const double temp, Irrigation::target_t target,
                 const IM& sm, const boost::shared_ptr<Volume> volume,
                 const bool silence, Treelog& msg)
{ impl->irrigate (duration, flux, temp, target, sm, volume, silence, msg); }

void 
Field::fertilize (const Metalib& metalib, const FrameModel& al, 
                  const double from, const double to, 
                  const Time& now, Treelog& msg)
{ impl->fertilize (metalib, al, from, to, now, msg); }

void 
Field::fertilize (const Metalib& metalib, const FrameModel& al, 
                  const Volume& volume, 
                  const Time& now, Treelog& msg)
{ impl->fertilize (metalib, al, volume, now, msg); }

void 
Field::fertilize (const Metalib& metalib, const FrameModel& al, 
                  const Time& now, Treelog& msg)
{ impl->fertilize (metalib, al, now, msg); }

void 
Field::clear_second_year_utilization ()
{ impl->clear_second_year_utilization (); }

void
Field::emerge (symbol name, Treelog& msg)
{ impl->emerge (name, msg); }

void
Field::harvest (const Time& time, const symbol name,
		const double stub_length, 
		const double stem_harvest, 
		const double leaf_harvest, 
		const double sorg_harvest,
                const bool combine,
		std::vector<const Harvest*>& total, Treelog& msg)
{ impl->harvest (time, name, stub_length,
		stem_harvest, leaf_harvest, sorg_harvest, combine, 
                total, msg); }

void
Field::pluck (const Time& time, const symbol name,
              const double stem_harvest, 
              const double leaf_harvest, 
              const double sorg_harvest,
              std::vector<const Harvest*>& total, Treelog& msg)
{ impl->pluck (time, name, stem_harvest, leaf_harvest, sorg_harvest, 
               total, msg); }

void 
Field::mix (const double from, const double to, 
            const double penetration, const double surface_loose, 
            const double RR0, const Time& time, Treelog& msg)
{ impl->mix (from, to, penetration, surface_loose, RR0, time, msg); }

void 
Field::swap (const double from, const double middle, const double to, 
             const double RR0, const Time& time, Treelog& msg)
{ impl->swap (from, middle, to, RR0, time, msg); }

void 
Field::store_SOM (Treelog& msg)
{ impl->store_SOM (msg); }

void 
Field::restore_SOM (Treelog& msg)
{ impl->restore_SOM (msg); }

void 
Field::set_porosity (const double at, const double Theta, Treelog& msg)
{ impl->set_porosity (at, Theta, msg); }

void 
Field::spray_overhead (const symbol chemical, const double amount,
                       Treelog& msg) // [g/ha]
{ impl->spray_overhead (chemical, amount, msg); }

void 
Field::spray_surface (const symbol chemical, const double amount,
                      Treelog& msg) // [g/ha]
{ impl->spray_surface (chemical, amount, msg); }

void 
Field::set_surface_detention_capacity (double height) // [mm]
{ impl->set_surface_detention_capacity (height); }

void 
Field::remove_solute (const symbol chemical)
{ impl->remove_solute (chemical); }

double				// [g/ha]
Field::total_solute (const symbol chemical)
{ return impl->total_solute (chemical); }

double 
Field::daily_air_temperature () const  // [dg C]
{ return impl->daily_air_temperature (); }

double 
Field::daily_precipitation () const  // [dg C]
{ return impl->daily_precipitation (); }

double 
Field::daily_global_radiation () const  // [dg C]
{ return impl->daily_global_radiation (); }

double 
Field::soil_temperature (double height) const  // [cm -> dg C]
{ return impl->soil_temperature (height); }

double 
Field::soil_water_potential (double height) const // [cm -> cm]
{ return impl->soil_water_potential (height); }

double 
Field::soil_water_content (double from, double to) const // [cm]
{ return impl->soil_water_content (from, to); }

double
Field::soil_inorganic_nitrogen (double from, double to) const // [kg N/ha]
{ return impl->soil_inorganic_nitrogen (from, to); }

double
Field::second_year_utilization () const // [kg N/ha]
{ return impl->second_year_utilization (); }

double 
Field::crop_ds (const symbol crop) const
{ return impl->crop_ds (crop); } 

double 
Field::crop_stage (const symbol crop) const
{ return impl->crop_stage (crop); } 

double 
Field::crop_dm (const symbol crop, const double height) const
{ return impl->crop_dm (crop, height); } 

double 
Field::crop_sorg_dm (const symbol crop) const
{ return impl->crop_sorg_dm (crop); } 

std::string
Field::crop_names () const
{ return impl->crop_names (); } 

void
Field::clear ()
{ impl->clear (); }

void
Field::tick_source (const Scope& parent_scope, const Time& time_end,
                    Treelog& msg)
{ impl->tick_source (parent_scope, time_end, msg); }

double
Field::suggest_dt (const double weather_dt, const double T_air) const
{ return impl->suggest_dt (weather_dt, T_air); }

void
Field::tick_move (const Metalib& metalib, 
                  const Time& time, const Time& time_end, const double dt,
                  const Weather* weather, 
                  const Scope& scope, Treelog& msg)
{ impl->tick_move (metalib, time, time_end, dt, weather, scope, msg); }

void 
Field::output (Log& log) const
{
  static const symbol libname (Column::component);

  if (!impl->total_area_known)  // We need to reset after column areas change.
    {
      impl->total_area_known = true;
      // Find total area.
      log.column_clear ();
      for (Implementation::ColumnList::const_iterator i = impl->columns.begin ();
           i != impl->columns.end ();
           i++)
        log.column_add_to_total (**i);
    }

  // For each column.
  for (Implementation::ColumnList::const_iterator i = impl->columns.begin ();
       i != impl->columns.end ();
       i++)
    {
      const Column& column = **i;
      if (log.check_entry (column.objid, libname))
	{
          log.column_select (column); // Relative weight.
	  Log::Entry open_entry (log, column.objid, column.frame (),
				 Column::component);
	  column.output (log);
	}
    }
}

const Column* 
Field::find (symbol name) const
{ return impl->find (name); }

Column* 
Field::find (unsigned int pos) const
{ return impl->columns [pos]; }

unsigned int 
Field::size () const
{ return impl->columns.size (); }

bool 
Field::check (const Weather *const global_weather, 
              const Time& from, const Time& to, 
	      const Scope& scope, Treelog& err) const
{ return impl->check (global_weather, from, to, scope, err); }

bool 
Field::check_am (const FrameModel& am, Treelog& err) const
{ return impl->check_am (am, err); }

bool
Field::check_z_border (const double value, Treelog& err) const
{ return impl->check_z_border (value, err); }

bool
Field::check_x_border (const double value, Treelog& err) const
{ return impl->check_x_border (value, err); }

bool
Field::check_y_border (const double value, Treelog& err) const
{ return impl->check_y_border (value, err); }

bool
Field::initialize (const Block& block, const std::vector<const Scope*> scopes,
                   const Time& time, const Weather* weather, const Scope& scope)
{ return impl->initialize (block, scopes, time, weather, scope); }

Field::Field (const Block& parent, const std::string& key)
  : impl (new Implementation (parent, key))
{ }

void
Field::summarize (Treelog& msg) const
{ impl->summarize (msg); }

Field::~Field ()
{ }

// field.C ends here.
