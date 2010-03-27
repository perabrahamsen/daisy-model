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

  // Restrictions.
  Column* selected;
  void restrict (symbol name);
  void unrestrict ();

  // Actions.
  void sow (const Metalib&, const FrameModel& crop, 
            double row_width, double row_pos, double seed,
            const Time& time, Treelog&);
  void ridge (const FrameSubmodel& ridge);
  void irrigate_overhead (double flux, double temp, const IM&, double dt, 
			  Treelog& msg);
  void irrigate_surface (double flux, double temp, const IM&, double dt, 
			 Treelog& msg);
  void irrigate_overhead (double flux, const IM&, double dt, Treelog& msg);
  void irrigate_surface (double flux, const IM&, double dt, Treelog& msg);
  void irrigate_subsoil (double flux, const IM&, double from, double to, 
                         double dt, Treelog& msg);
  void irrigate_subsoil (double flux, const IM&, const Volume&,
                         double dt, Treelog& msg);
  void fertilize (const Metalib&, const FrameModel&, const Volume&, 
                  const Time&, double dt, Treelog& msg);
  void fertilize (const Metalib&, const FrameModel&, double from, double to, 
                  const Time&, double dt, Treelog& msg);
  void fertilize (const Metalib&, const FrameModel&, 
                  const Time&, double dt, Treelog& msg);
  void clear_second_year_utilization ();
  void emerge (symbol crop, Treelog&);
  void harvest (const Metalib& metalib, 
                const Time&, double dt, symbol name,
		double stub_length, 
		double stem_harvest, 
		double leaf_harvest, 
		double sorg_harvest,
                const bool combine,
		std::vector<const Harvest*>&, Treelog&);
  void pluck (const Metalib& metalib, 
              const Time&, double dt, symbol name,
              double stem_harvest, 
              double leaf_harvest, 
              double sorg_harvest,
              std::vector<const Harvest*>&, Treelog&);
  void mix (const Metalib& metalib, 
            double from, double to, double penetration, 
            const Time&, double dt, Treelog&);
  void swap (const Metalib& metalib, 
             double from, double middle, double to, 
             const Time&, double dt, Treelog&);
  void set_porosity (double at, double Theta, 
                     const double dt, Treelog& msg);
  void set_heat_source (double at, double value);
  void spray (symbol chemical, double amount, double dt, Treelog&); // [g/ha]
  void set_surface_detention_capacity (double height); // [mm]

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
  // Drymatter in shoot [kg/ha], or negative if no such crop is present
  double crop_dm (symbol crop, double height) const; 
  // Drymatter in sorg [kg/ha], or negative if no such crop is present
  double crop_sorg_dm (symbol crop) const; 
  // All names of all crops on selected columns.
  std::string crop_names () const;
  // Simulation.
  void clear ();
  void tick_all (const Metalib& metalib, 
                 const Time&, double dt, const Weather*,
		 const Scope&, Treelog&);
  void tick_one (const Metalib& metalib, 
                 size_t, const Time&, double dt, const Weather*, 
		 const Scope&, Treelog&);

  // Find a specific column.
  double relative_weight (const Column& column, const Select& select) const;
  Column* find (symbol name) const;

  // Create and destroy.
  bool check (const Weather* global_weather, const Time& from, const Time& to, 
	      const Scope& scope, Treelog&) const;
  bool check_am (const FrameModel& am, Treelog& err) const;
  bool check_z_border (double, Treelog& err) const;
  bool check_x_border (double, Treelog& err) const;
  bool check_y_border (double, Treelog& err) const;
  bool initialize (const Block&, const Output&, const Time&, const Weather*, 
		   const Scope&);
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
  daisy_assert (selected);
  selected = NULL;
}

void 
Field::Implementation::sow (const Metalib& metalib, const FrameModel& crop, 
                            const double row_width, const double row_pos,
                            const double seed,
                            const Time& time, Treelog& msg)
{
  if (selected)
    {
      Treelog::Open nest (msg, selected->name);
      selected->sow (metalib, crop, row_width, row_pos, seed, time, msg);
    }
  else 
    {
      for (ColumnList::iterator i = columns.begin ();
	   i != columns.end ();
	   i++)
	{
	  Treelog::Open nest (msg, (*i)->name);
	  (*i)->sow (metalib, crop, row_width, row_pos, seed, time, msg);
	}
    }
}

void 
Field::Implementation::ridge (const FrameSubmodel& ridge)
{
  if (selected)
    selected->ridge (ridge);
  else 
    {
      for (ColumnList::iterator i = columns.begin ();
	   i != columns.end ();
	   i++)
	(*i)->ridge (ridge);
    }
}

void 
Field::Implementation::irrigate_overhead (const double flux, const double temp,
                                          const IM& im, const double dt,
					  Treelog& msg)
{
  if (selected)
    selected->irrigate_overhead (flux, temp, im, dt, msg);
  else for (ColumnList::iterator i = columns.begin ();
	    i != columns.end ();
	    i++)
	 (*i)->irrigate_overhead (flux, temp, im, dt, msg);
}

void 
Field::Implementation::irrigate_surface (const double flux, const double temp,
                                         const IM& im, const double dt,
					 Treelog& msg)
{
  if (selected)
    selected->irrigate_surface (flux, temp, im, dt, msg);
  else for (ColumnList::iterator i = columns.begin ();
	    i != columns.end ();
	    i++)
	 (*i)->irrigate_surface (flux, temp, im, dt, msg);
}

void 
Field::Implementation::irrigate_overhead (const double flux, const IM& im, 
                                          const double dt, Treelog& msg)
{
  if (selected)
    selected->irrigate_overhead (flux, im, dt, msg);
  else for (ColumnList::iterator i = columns.begin ();
	    i != columns.end ();
	    i++)
	 (*i)->irrigate_overhead (flux, im, dt, msg);
}

void 
Field::Implementation::irrigate_surface (const double flux, const IM& im, 
                                         const double dt, Treelog& msg)
{
  if (selected)
    selected->irrigate_surface (flux, im, dt, msg);
  else for (ColumnList::iterator i = columns.begin ();
	    i != columns.end ();
	    i++)
	 (*i)->irrigate_surface (flux, im, dt, msg);
}

void 
Field::Implementation::irrigate_subsoil (const double flux, const IM& im, 
                                         const double from, const double to,
                                         const double dt, Treelog& msg)
{
  if (selected)
    selected->irrigate_subsoil (flux, im, from, to, dt, msg);
  else for (ColumnList::iterator i = columns.begin ();
	    i != columns.end ();
	    i++)
	 (*i)->irrigate_subsoil (flux, im, from, to, dt, msg);
}

void 
Field::Implementation::irrigate_subsoil (const double flux, const IM& im, 
                                         const Volume& volume,
                                         const double dt, Treelog& msg)
{
  if (selected)
    selected->irrigate_subsoil (flux, im, volume, dt, msg);
  else for (ColumnList::iterator i = columns.begin ();
	    i != columns.end ();
	    i++)
	 (*i)->irrigate_subsoil (flux, im, volume, dt, msg);
}

void 
Field::Implementation::fertilize (const Metalib& metalib, const FrameModel& al, 
				  const double from, const double to,
                                  const Time& now, const double dt, 
                                  Treelog& msg)
{
  if (selected)
    selected->fertilize (metalib, al, from, to, now, dt, msg);
  else for (ColumnList::iterator i = columns.begin ();
	    i != columns.end ();
	    i++)
	 (*i)->fertilize (metalib, al, from, to, now, dt, msg);
}

void 
Field::Implementation::fertilize (const Metalib& metalib, const FrameModel& al, 
                                  const Volume& volume,
                                  const Time& now, const double dt,
                                  Treelog& msg)
{
  if (selected)
    selected->fertilize (metalib, al, volume, now, dt, msg);
  else for (ColumnList::iterator i = columns.begin ();
	    i != columns.end ();
	    i++)
	 (*i)->fertilize (metalib, al, volume, now, dt, msg);
}

void 
Field::Implementation::fertilize (const Metalib& metalib, const FrameModel& al, 
                                  const Time& now, const double dt, 
                                  Treelog& msg)
{
  if (selected)
    {
      selected->fertilize (metalib, al, now, dt, msg);
    }
  else 
    {
      for (ColumnList::iterator i = columns.begin ();
	   i != columns.end ();
	   i++)
	{
	  (*i)->fertilize (metalib, al, now, dt, msg);
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
      Treelog::Open nest (out, selected->name);
      selected->emerge (name, out);
    }
  else
    {
      for (ColumnList::iterator i = columns.begin ();
	   i != columns.end ();
	   i++)
	{
	  Treelog::Open nest (out, (*i)->name);
	  (*i)->emerge (name, out);
	}
    }
}

void
Field::Implementation::harvest (const Metalib& metalib, 
                                const Time& time, const double dt,
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
      Treelog::Open nest (out, selected->name);
      selected->harvest (metalib, time, dt, name,
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
	  Treelog::Open nest (out, (*i)->name);
	  (*i)->harvest (metalib, time, dt, name,
			 stub_length,
			 stem_harvest, leaf_harvest, sorg_harvest, combine,
                         total, out);
	}
    }
}

void
Field::Implementation::pluck (const Metalib& metalib, 
                              const Time& time, const double dt,
                              const symbol name,
                              const double stem_harvest, 
                              const double leaf_harvest, 
                              const double sorg_harvest,
                              std::vector<const Harvest*>& total,
                              Treelog& out)
{
  if (selected)
    {
      Treelog::Open nest (out, selected->name);
      selected->pluck (metalib, time, dt, name,
                       stem_harvest, leaf_harvest, sorg_harvest, 
                       total, out);
    }
  else
    {
      for (ColumnList::iterator i = columns.begin ();
	   i != columns.end ();
	   i++)
	{
	  Treelog::Open nest (out, (*i)->name);
	  (*i)->pluck (metalib, time, dt, name,
                       stem_harvest, leaf_harvest, sorg_harvest, 
                       total, out);
	}
    }
}

void 
Field::Implementation::mix (const Metalib& metalib, 
                            const double from, const double to,
                            double penetration, const Time& time,
			    const double dt, Treelog& msg)
{
  if (selected)
    {
      Treelog::Open nest (msg, selected->name); 
      selected->mix (metalib, from, to, penetration, time, dt, msg);
    }
  else for (ColumnList::iterator i = columns.begin ();
	    i != columns.end ();
	    i++)
    {
      Treelog::Open nest (msg, (*i)->name);
      (*i)->mix (metalib, from, to, penetration, time, dt, msg);
    }
}

void 
Field::Implementation::swap (const Metalib& metalib, 
                             const double from, const double middle, 
                             const double to, 
                             const Time& time, const double dt, Treelog& msg)
{
  if (selected)
    {
      Treelog::Open nest (msg, selected->name);
      selected->swap (metalib, from, middle, to, time, dt, msg);
    }
  else for (ColumnList::iterator i = columns.begin ();
	    i != columns.end ();
	    i++)
    {
      Treelog::Open nest (msg, (*i)->name);
      (*i)->swap (metalib, from, middle, to, time, dt, msg);
    }
}

void 
Field::Implementation::set_porosity (const double at, const double Theta, 
                                     const double dt, Treelog& msg)
{
  if (selected)
    selected->set_porosity (at, Theta, dt, msg);
  else for (ColumnList::iterator i = columns.begin ();
	    i != columns.end ();
	    i++)
         (*i)->set_porosity (at, Theta, dt, msg);
}

void 
Field::Implementation::set_heat_source (double at, double value)
{
  if (selected)
    selected->set_heat_source (at, value);
  else for (ColumnList::iterator i = columns.begin ();
	    i != columns.end ();
	    i++)
    (*i)->set_heat_source (at, value);
}

void 
Field::Implementation::spray (const symbol chemical, 
                              const double amount, const double dt,
                              Treelog& msg) // [g/ha]
{
  if (selected)
    selected->spray (chemical, amount, dt, msg);
  else for (ColumnList::iterator i = columns.begin ();
	    i != columns.end ();
	    i++)
    (*i)->spray (chemical, amount, dt, msg);
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
Field::Implementation::tick_all (const Metalib& metalib, 
                                 const Time& time, const double dt, 
                                 const Weather* weather, const Scope& scope,
				 Treelog& msg)
{
  if (columns.size () == 1)
    (*(columns.begin ()))->tick (metalib, time, dt, weather, scope, msg);
  else
    for (ColumnList::const_iterator i = columns.begin ();
         i != columns.end ();
         i++)
      {
        Treelog::Open nest (msg, "Column " + (*i)->name);
        (*i)->tick (metalib, time, dt, weather, scope, msg);
      }
}

void 
Field::Implementation::tick_one (const Metalib& metalib, 
                                 const size_t col,
                                 const Time& time, const double dt, 
                                 const Weather* weather, 
				 const Scope& scope, Treelog& msg)
{
  daisy_assert (columns.size () > 0);
  columns[col]->tick (metalib, time, dt, weather, scope, msg);
}

double 
Field::Implementation::relative_weight (const Column& column, 
                                        const Select& select) const
// Relative area of COLUMN compared to all columns matched by SELECT.
{
  double total_area = 0.0;
  for (Implementation::ColumnList::const_iterator i = columns.begin ();
       i != columns.end ();
       i++)
    {
      const Column& col = **i;
      if (select.valid (collib.ancestors (col.name)))
        total_area += col.area;
    }
  if (total_area > 0.0)
    return column.area / total_area;

  return -42.42e42;
}

Column* 
Field::Implementation::find (symbol name) const
{
  for (ColumnList::const_iterator i = columns.begin ();
       i != columns.end ();
       i++)
    { 
      if ((*i)->name == name)
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
      Treelog::Open nest (err, (*i) ? (*i)->name.name ().c_str () : "error");
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
      Treelog::Open nest (err, (*i)->name);
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
Field::Implementation::initialize (const Block& block, const Output& output,
                                   const Time& time, const Weather* weather,
				   const Scope& scope)
{
  bool ok = true;
  for (ColumnList::const_iterator i = columns.begin ();
       i != columns.end ();
       i++)
    if (!(*i)->initialize (block, output, time, weather, scope))
      ok = false;
  return ok;
}

Field::Implementation::Implementation (const Block& parent, 
				       const std::string& key)
  : collib (parent.metalib ().library (Column::component)),
    columns (Librarian::build_vector<Column> (parent, key)),
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
  daisy_assert (selected == NULL);
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
Field::sow (const Metalib& metalib, const FrameModel& crop,
            const double row_width, const double row_pos, const double seed,
            const Time& time, Treelog& msg)
{ impl->sow (metalib, crop, row_width, row_pos, seed, time, msg); }

void 
Field::ridge (const FrameSubmodel& al)
{ impl->ridge (al); }

void 
Field::irrigate_overhead (double water, double temp, const IM& im, double dt,
			  Treelog& msg)
{ impl->irrigate_overhead (water, temp, im, dt, msg); }

void 
Field::irrigate_surface (double water, double temp, const IM& im, double dt,
			 Treelog& msg)
{ impl->irrigate_surface (water, temp, im, dt, msg); }

void 
Field::irrigate_overhead (double water, const IM& im, double dt, Treelog& msg)
{ impl->irrigate_overhead (water, im, dt, msg); }

void 
Field::irrigate_surface (double water, const IM& im, double dt, Treelog& msg)
{ impl->irrigate_surface (water, im, dt, msg); }

void 
Field::irrigate_subsoil (double water, const IM& im, 
                         double from, double to, double dt, Treelog& msg)
{ impl->irrigate_subsoil (water, im, from, to, dt, msg); }

void 
Field::irrigate_subsoil (double water, const IM& im, 
                         const Volume& volume, double dt, Treelog& msg)
{ impl->irrigate_subsoil (water, im, volume, dt, msg); }

void 
Field::fertilize (const Metalib& metalib, const FrameModel& al, 
                  const double from, const double to, 
                  const Time& now, const double dt, Treelog& msg)
{ impl->fertilize (metalib, al, from, to, now, dt, msg); }

void 
Field::fertilize (const Metalib& metalib, const FrameModel& al, 
                  const Volume& volume, 
                  const Time& now, const double dt, Treelog& msg)
{ impl->fertilize (metalib, al, volume, now, dt, msg); }

void 
Field::fertilize (const Metalib& metalib, const FrameModel& al, 
                  const Time& now, const double dt, Treelog& msg)
{ impl->fertilize (metalib, al, now, dt, msg); }

void 
Field::clear_second_year_utilization ()
{ impl->clear_second_year_utilization (); }

void
Field::emerge (symbol name, Treelog& msg)
{ impl->emerge (name, msg); }

void
Field::harvest (const Metalib& metalib, 
                const Time& time, const double dt, const symbol name,
		const double stub_length, 
		const double stem_harvest, 
		const double leaf_harvest, 
		const double sorg_harvest,
                const bool combine,
		std::vector<const Harvest*>& total, Treelog& msg)
{ impl->harvest (metalib, time, dt, name,
		stub_length,
		stem_harvest, leaf_harvest, sorg_harvest, combine, 
                total, msg); }

void
Field::pluck (const Metalib& metalib, 
              const Time& time, const double dt, const symbol name,
              const double stem_harvest, 
              const double leaf_harvest, 
              const double sorg_harvest,
              std::vector<const Harvest*>& total, Treelog& msg)
{ impl->pluck (metalib, time, dt, name,
               stem_harvest, leaf_harvest, sorg_harvest, 
               total, msg); }

void 
Field::mix (const Metalib& metalib, 
            const double from, const double to, 
            const double penetration, 
            const Time& time, const double dt, Treelog& msg)
{ impl->mix (metalib, from, to, penetration, time, dt, msg); }

void 
Field::swap (const Metalib& metalib, 
             const double from, const double middle, const double to, 
             const Time& time, const double dt, Treelog& msg)
{ impl->swap (metalib, from, middle, to, time, dt, msg); }

void 
Field::set_porosity (const double at, const double Theta, 
                     const double dt, Treelog& msg)
{ impl->set_porosity (at, Theta, dt, msg); }

void 
Field::set_heat_source (double at, double value)
{ impl->set_heat_source (at, value); }

void 
Field::spray (const symbol chemical, 
              const double amount, const double dt,
              Treelog& msg) // [g/ha]
{ impl->spray (chemical, amount, dt, msg); }

void 
Field::set_surface_detention_capacity (double height) // [mm]
{ impl->set_surface_detention_capacity (height); }

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
Field::tick_all (const Metalib& metalib, 
                 const Time& time, const double dt, const Weather* weather, 
                 const Scope& scope, Treelog& msg)
{ impl->tick_all (metalib, time, dt, weather, scope, msg); }

void
Field::tick_one (const Metalib& metalib, 
                 const size_t col,
                 const Time& time, const double dt, const Weather* weather, 
                 const Scope& scope, Treelog& msg)
{ impl->tick_one (metalib, col, time, dt, weather, scope, msg); }

void 
Field::output (Log& log) const
{
  static const symbol libname (Column::component);
  for (Implementation::ColumnList::const_iterator i = impl->columns.begin ();
       i != impl->columns.end ();
       i++)
    {
      const Column& column = **i;
      if (log.check_entry (column.name, libname))
	{
          // Log::Col must be declared before Log::Entry
          Log::Col col (log, column, *this); 
	  Log::Entry open_entry (log, column.name, column.frame (),
				 Column::component);
	  column.output (log);
	}
    }
}

double 
Field::relative_weight (const Column& column, const Select& select) const
{ return impl->relative_weight (column, select); }

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
Field::initialize (const Block& block, const Output& output,
                   const Time& time, const Weather* weather, const Scope& scope)
{ return impl->initialize (block, output, time, weather, scope); }

Field::Field (const Block& parent, const std::string& key)
  : impl (new Implementation (parent, key))
{ }

void
Field::summarize (Treelog& msg) const
{ impl->summarize (msg); }

Field::~Field ()
{ }

// field.C ends here.
