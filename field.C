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


#include "field.h"
#include "column.h"
#include "log.h"
#include "log_clone.h"
#include "treelog.h"

struct Field::Implementation
{
  // Columns.
  typedef vector<Column*> ColumnList;
  ColumnList columns;

  // Restrictions.
  Column* selected;
  void restrict (symbol name);
  void unrestrict ();

  // Actions.
  void sow (Treelog&, const AttributeList& crop);
  void ridge (const AttributeList& ridge);
  void irrigate_overhead (double flux, double temp, const IM&);
  void irrigate_surface (double flux, double temp, const IM&);
  void irrigate_overhead (double flux, const IM&);
  void irrigate_surface (double flux, const IM&);
  void irrigate_subsoil (double flux, const IM&, double from, double to);
  void set_subsoil_irrigation (double flux, const IM& sm, 
			       double from, double to);
  void fertilize (const AttributeList&, double from, double to);  // Organic.
  void fertilize (const AttributeList&);
  void clear_second_year_utilization ();
  void harvest (const Time&, symbol name,
		double stub_length, 
		double stem_harvest, 
		double leaf_harvest, 
		double sorg_harvest,
		vector<const Harvest*>&, Treelog&);
  void mix (Treelog&, const Time&,
	    double from, double to, double penetration);
  void swap (Treelog&, const Time&, double from, double middle, double to);
  void set_porosity (double at, double Theta);
  void set_heat_source (double at, double value);
  void spray (symbol chemical, double amount); // [g/ha]
  void set_surface_detention_capacity (double height); // [mm]

  // Conditions.
public:
  double daily_air_temperature () const; // [ dg C]
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

  // Simulation.
  void clear ();
  void tick (Treelog&, const Time&, const Weather*);
  void output (Log&) const;

  // Find a specific column.
  Column* find (symbol name) const;

  // Changing the field.
  void divide (symbol original, symbol copy, double copy_size, 
	       const Time&, const Weather*);
  void merge (symbol combine, symbol remove);

  // Create and destroy.
  bool check (bool require_weather, const Time& from, const Time& to, 
	      Treelog& err) const;
  bool check_am (const AttributeList& am, Treelog& err) const;
  void initialize (const Time&, Treelog& err, const Weather*);
  Implementation (const vector<AttributeList*>&);
  ~Implementation ();
};

void 
Field::Implementation::restrict (symbol name)
{
  if (selected)
    throw ("Cannot restrict already restricted field");
  selected = find (name);

  if (!selected)
    throw (string ("Restricting to non-existing column '") + name + "'");
}

void 
Field::Implementation::unrestrict ()
{
  daisy_assert (selected);
  selected = NULL;
}

void 
Field::Implementation::sow (Treelog& msg, const AttributeList& crop)
{
  if (selected)
    {
      Treelog::Open nest (msg, selected->name);
      selected->sow (msg, crop);
    }
  else 
    {
      for (ColumnList::iterator i = columns.begin ();
	   i != columns.end ();
	   i++)
	{
	  Treelog::Open nest (msg, (*i)->name);
	  (*i)->sow (msg, crop);
	}
    }
}

void 
Field::Implementation::ridge (const AttributeList& ridge)
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
Field::Implementation::irrigate_overhead (double flux, double temp, const IM& im)
{
  if (selected)
    selected->irrigate_overhead (flux, temp, im);
  else for (ColumnList::iterator i = columns.begin ();
	    i != columns.end ();
	    i++)
    (*i)->irrigate_overhead (flux, temp, im);
}

void 
Field::Implementation::irrigate_surface (double flux, double temp, const IM& im)
{
  if (selected)
    selected->irrigate_surface (flux, temp, im);
  else for (ColumnList::iterator i = columns.begin ();
	    i != columns.end ();
	    i++)
    (*i)->irrigate_surface (flux, temp, im);
}

void 
Field::Implementation::irrigate_overhead (double flux, const IM& im)
{
  if (selected)
    selected->irrigate_overhead (flux, im);
  else for (ColumnList::iterator i = columns.begin ();
	    i != columns.end ();
	    i++)
    (*i)->irrigate_overhead (flux, im);
}

void 
Field::Implementation::irrigate_surface (double flux, const IM& im)
{
  if (selected)
    selected->irrigate_surface (flux, im);
  else for (ColumnList::iterator i = columns.begin ();
	    i != columns.end ();
	    i++)
    (*i)->irrigate_surface (flux, im);
}

void 
Field::Implementation::irrigate_subsoil (double flux, const IM& im, 
                                         double from, double to)
{
  if (selected)
    selected->irrigate_subsoil (flux, im, from, to);
  else for (ColumnList::iterator i = columns.begin ();
	    i != columns.end ();
	    i++)
    (*i)->irrigate_subsoil (flux, im, from, to);
}

void 
Field::Implementation::set_subsoil_irrigation (double flux, const IM& im, 
					       double from, double to)
{
  if (selected)
    selected->set_subsoil_irrigation (flux, im, from, to);
  else for (ColumnList::iterator i = columns.begin ();
	    i != columns.end ();
	    i++)
    (*i)->set_subsoil_irrigation (flux, im, from, to);
}

void 
Field::Implementation::fertilize (const AttributeList& al, 
				  double from, double to)
{
  if (selected)
    selected->fertilize (al, from, to);
  else for (ColumnList::iterator i = columns.begin ();
	    i != columns.end ();
	    i++)
    (*i)->fertilize (al, from, to);
}

void 
Field::Implementation::fertilize (const AttributeList& al)
{
  if (selected)
    {
      selected->fertilize (al);
    }
  else 
    {
      for (ColumnList::iterator i = columns.begin ();
	   i != columns.end ();
	   i++)
	{
	  (*i)->fertilize (al);
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
Field::Implementation::harvest (const Time& time, symbol name,
				double stub_length, 
				double stem_harvest, 
				double leaf_harvest, 
				double sorg_harvest,
				vector<const Harvest*>& total,
				Treelog& out)
{
  if (selected)
    {
      Treelog::Open nest (out, selected->name);
      selected->harvest (time, name,
			 stub_length,
			 stem_harvest, leaf_harvest, sorg_harvest, total, out);
    }
  else
    {
      for (ColumnList::iterator i = columns.begin ();
	   i != columns.end ();
	   i++)
	{
	  Treelog::Open nest (out, (*i)->name);
	  (*i)->harvest (time, name,
			 stub_length,
			 stem_harvest, leaf_harvest, sorg_harvest, total, out);
	}
    }
}

void 
Field::Implementation::mix (Treelog& out, const Time& time,
			    double from, double to, double penetration)
{
  if (selected)
    {
      Treelog::Open nest (out, selected->name); 
      selected->mix (out, time, from, to, penetration);
    }
  else for (ColumnList::iterator i = columns.begin ();
	    i != columns.end ();
	    i++)
    {
      Treelog::Open nest (out, (*i)->name);
      (*i)->mix (out, time, from, to, penetration);
    }
}

void 
Field::Implementation::swap (Treelog& out, const Time& time,
			     double from, double middle, double to)
{
  if (selected)
    {
      Treelog::Open nest (out, selected->name);
      selected->swap (out, time, from, middle, to);
    }
  else for (ColumnList::iterator i = columns.begin ();
	    i != columns.end ();
	    i++)
    {
      Treelog::Open nest (out, (*i)->name);
      (*i)->swap (out, time, from, middle, to);
    }
}

void 
Field::Implementation::set_porosity (double at, double Theta)
{
  if (selected)
    selected->set_porosity (at, Theta);
  else for (ColumnList::iterator i = columns.begin ();
	    i != columns.end ();
	    i++)
    (*i)->set_porosity (at, Theta);
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
Field::Implementation::spray (symbol chemical, double amount) // [g/ha]
{
  if (selected)
    selected->spray (chemical, amount);
  else for (ColumnList::iterator i = columns.begin ();
	    i != columns.end ();
	    i++)
    (*i)->spray (chemical, amount);
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
  if (columns.size () != 1)
    throw ("Cannot find crop development state of multiple columns");

  return columns[0]->crop_ds (crop); 
} 

double 
Field::Implementation::crop_dm (const symbol crop, const double height) const
{
  if (selected)
    return selected->crop_dm (crop, height);
  
  // We find the total DM for all the columns.
  double DM = 0.0;
  for (ColumnList::const_iterator i = columns.begin ();
       i != columns.end ();
       i++)
    DM += (*i)->crop_dm (crop, height);
  return DM;
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
Field::Implementation::tick (Treelog& out, 
			     const Time& time, const Weather* weather)
{
  for (ColumnList::const_iterator i = columns.begin ();
       i != columns.end ();
       i++)
    (*i)->tick (out, time, weather);
}

void 
Field::Implementation::output (Log& log) const
{
  const Library& library = Librarian<Column>::library ();

  for (ColumnList::const_iterator i = columns.begin ();
       i != columns.end ();
       i++)
    {
      if (log.check_entry ((*i)->name, library))
	{
	  Log::Entry open_entry (log, symbol ((*i)->name), (*i)->alist);
	  (*i)->output (log);
	}
    }
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

void 
Field::Implementation::divide (symbol original, symbol copy,
			       double copy_size,
			       const Time& time, const Weather* weather)
{ 
  Column* old = find (original);
  if (!old)
    throw ("Attempt to divide non-existing column"); 
  if (old->size <= copy_size)
    throw ("Attempt to divide column into fraction larger than the original");
  old->size -= copy_size;
  const Library& library = Librarian<Column>::library ();
  const Syntax& syntax = library.syntax (old->alist.identifier ("type"));
  LogClone log_clone ("column", syntax, old->alist);
  old->output (log_clone);
  AttributeList& lib_alist = *new AttributeList ();
  // Remember where we got this object.
  lib_alist.add ("parsed_from_file", "*clone*");
  lib_alist.add ("parsed_sequence", Library::get_sequence ());
  lib_alist.add ("type", original.name ());
  Librarian<Column>::derive_type (copy, lib_alist, original);
  AttributeList copy_alist (log_clone.result ());
  copy_alist.add ("type", copy.name ());
  copy_alist.add ("size", copy_size);
  Column* result = &Librarian<Column>::create (copy_alist);
  result->initialize (time, Treelog::null (), weather);
  columns.push_back (result);
}
  
void
Field::Implementation::merge (symbol /*combine*/,
			      symbol /*remove*/)
{ throw ("Merge is not yet implemented"); } 

bool 
Field::Implementation::check (bool require_weather,
			      const Time& from, const Time& to, 
			      Treelog& err) const
{ 
  bool ok = true;
  for (ColumnList::const_iterator i = columns.begin ();
       i != columns.end ();
       i++)
    {
      Treelog::Open nest (err, (*i) ? (*i)->name.name ().c_str () : "error");
      if ((*i) == NULL || !(*i)->check (require_weather, from, to, err))
	ok = false;
    }
  return ok;
}

bool 
Field::Implementation::check_am (const AttributeList& am, Treelog& err) const
{ 
  Treelog::Open nest (err, am.identifier ("type"));

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

void 
Field::Implementation::initialize (const Time& time, Treelog& err, 
				   const Weather* weather)
{
  for (ColumnList::const_iterator i = columns.begin ();
       i != columns.end ();
       i++)
    (*i)->initialize (time, err, weather);
}

Field::Implementation::Implementation (const vector<AttributeList*>& sequence)
  : columns (map_create<Column> (sequence)),
    selected (NULL)
{ }

Field::Implementation::~Implementation ()
{
  daisy_assert (selected == NULL);
  sequence_delete (columns.begin (), columns.end ()); 
}

Field::Restrict::Restrict (Field& f, symbol name)
  : field (f)
{ 
  field.impl.restrict (name); 
}

Field::Restrict::~Restrict ()
{ field.impl.unrestrict (); }

void 
Field::sow (Treelog& msg, const AttributeList& crop)
{ impl.sow (msg, crop); }

void 
Field::ridge (const AttributeList& al)
{ impl.ridge (al); }

void 
Field::irrigate_overhead (double flux, double temp, const IM& im)
{ impl.irrigate_overhead (flux, temp, im); }

void 
Field::irrigate_surface (double flux, double temp, const IM& im)
{ impl.irrigate_surface (flux, temp, im); }

void 
Field::irrigate_overhead (double flux, const IM& im)
{ impl.irrigate_overhead (flux, im); }

void 
Field::irrigate_surface (double flux, const IM& im)
{ impl.irrigate_surface (flux, im); }

void 
Field::irrigate_subsoil (double flux, const IM& im, double from, double to)
{ impl.irrigate_subsoil (flux, im, from, to); }

void 
Field::set_subsoil_irrigation (double flux, const IM& im, 
			       double from, double to)
{ impl.set_subsoil_irrigation (flux, im, from, to); }

void 
Field::fertilize (const AttributeList& al, double from, double to)
{ impl.fertilize (al, from, to); }

void 
Field::fertilize (const AttributeList& al)
{ impl.fertilize (al); }

void 
Field::clear_second_year_utilization ()
{ impl.clear_second_year_utilization (); }

void
Field::harvest (const Time& time, symbol name,
		double stub_length, 
		double stem_harvest, 
		double leaf_harvest, 
		double sorg_harvest,
		vector<const Harvest*>& total, Treelog& out)
{ impl.harvest (time, name,
		stub_length,
		stem_harvest, leaf_harvest, sorg_harvest, total, out); }

void 
Field::mix (Treelog& out, const Time& time,
	    double from, double to, double penetration)
{ impl.mix (out, time, from, to, penetration); }

void 
Field::swap (Treelog& out, const Time& time, 
	     double from, double middle, double to)
{ impl.swap (out, time, from, middle, to); }

void 
Field::set_porosity (double at, double Theta)
{ impl.set_porosity (at, Theta); }

void 
Field::set_heat_source (double at, double value)
{ impl.set_heat_source (at, value); }

void 
Field::spray (symbol chemical, double amount) // [g/ha]
{ impl.spray (chemical, amount); }

void 
Field::set_surface_detention_capacity (double height) // [mm]
{ impl.set_surface_detention_capacity (height); }

double 
Field::daily_air_temperature () const  // [dg C]
{ return impl.daily_air_temperature (); }

double 
Field::soil_temperature (double height) const  // [cm -> dg C]
{ return impl.soil_temperature (height); }

double 
Field::soil_water_potential (double height) const // [cm -> cm]
{ return impl.soil_water_potential (height); }

double 
Field::soil_water_content (double from, double to) const // [cm]
{ return impl.soil_water_content (from, to); }

double
Field::soil_inorganic_nitrogen (double from, double to) const // [kg N/ha]
{ return impl.soil_inorganic_nitrogen (from, to); }

double
Field::second_year_utilization () const // [kg N/ha]
{ return impl.second_year_utilization (); }

double 
Field::crop_ds (const symbol crop) const
{ return impl.crop_ds (crop); } 

double 
Field::crop_dm (const symbol crop, const double height) const
{ return impl.crop_dm (crop, height); } 

void
Field::clear ()
{ impl.clear (); }

void
Field::tick (Treelog& out, const Time& time, const Weather* weather)
{ impl.tick (out, time, weather); }

void 
Field::output (Log& log) const
{ impl.output (log); }

const Column* 
Field::find (symbol name) const
{ return impl.find (name); }

Column* 
Field::find (unsigned int pos) const
{ return impl.columns [pos]; }

unsigned int 
Field::size () const
{ return impl.columns.size (); }

void 
Field::divide (symbol original, symbol copy, double copy_size,
	       const Time& time, const Weather* weather)
{ impl.divide (original, copy, copy_size, time, weather); }

void 
Field::merge (symbol combine, symbol remove)
{ impl.merge (combine, remove); }

bool 
Field::check (bool require_weather, const Time& from, const Time& to, 
	      Treelog& err) const
{ return impl.check (require_weather, from, to, err); }

bool 
Field::check_am (const AttributeList& am, Treelog& err) const
{ return impl.check_am (am, err); }

void 
Field::initialize (const Time& time, Treelog& err, const Weather* weather)
{ impl.initialize (time, err, weather); }

Field::Field (const vector<AttributeList*>& sequence)
  : impl (*new Implementation (sequence))
{ }

Field::~Field ()
{ delete &impl; }
