// field.C

#include "field.h"
#include "column.h"
#include "log.h"
#include "filter.h"

struct Field::Implementation
{
  // Columns.
  typedef vector<Column*> ColumnList;
  ColumnList columns;

  // Restrictions.
  Column* selected;
  void restrict (const string& name);
  void unrestrict ();

  // Actions.
  void sow (const AttributeList& crop);
  void irrigate_top (double flux, double temp, const IM&);
  void irrigate_surface (double flux, double temp, const IM&);
  void fertilize (const AttributeList&, const Time&, // Organic.
			  double from, double to);
  void fertilize (const AttributeList&, const Time&);
  void fertilize (const IM&, double from, double to); // Mineral.
  void fertilize (const IM&);
  vector<const Harvest*> harvest (const Time&, const string& name,
					  double stub_length, 
					  double stem_harvest, 
					  double leaf_harvest, 
					  double sorg_harvest);
  void mix (const Time&,
		    double from, double to, double penetration);
  void swap (const Time&, double from, double middle, double to);
  void spray (const string& chemical, double amount); // [g/ha]

  // Conditions.
public:
  double soil_temperature (double height) const; // [ cm -> dg C]
  double soil_water_potential (double height) const; // [cm -> cm]
  // Current development stage for the crop named "crop", or
  // Crop::DSremove if no such crop is present.
  double crop_ds (const string& crop) const; 
  // Drymatter in shoot [kg/ha], or negative if no such crop is present
  double crop_dm (const string& crop) const; 

  // Simulation.
  void tick (const Time&, const Weather&);
  void output (Log&, Filter&) const;

  // Find a specific column.
  Column* find (const string& name) const;

  // Create and destroy.
  bool check () const;
  bool check_am (const AttributeList& am) const;
  void initialize (const vector<AttributeList*>&, const Time&, const Weather&);
  Implementation (const vector<AttributeList*>&);
  ~Implementation ();
};

void 
Field::Implementation::restrict (const string& name)
{
  if (selected)
    THROW ("Cannot restrict already restrcited field");
  selected = find (name);

  if (!selected)
    THROW (string ("Restricting to non-existing column `") + name + "'");
}

void 
Field::Implementation::unrestrict ()
{
  assert (selected);
  selected = NULL;
}

void 
Field::Implementation::sow (const AttributeList& crop)
{
  if (selected)
    selected->sow (crop);
  else for (ColumnList::iterator i = columns.begin ();
	    i != columns.end ();
	    i++)
    (*i)->sow (crop);
}

void 
Field::Implementation::irrigate_top (double flux, double temp, const IM& im)
{
  if (selected)
    selected->irrigate_top (flux, temp, im);
  else for (ColumnList::iterator i = columns.begin ();
	    i != columns.end ();
	    i++)
    (*i)->irrigate_top (flux, temp, im);
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
Field::Implementation::fertilize (const AttributeList& al, const Time& time,
				  double from, double to)
{
  if (selected)
    selected->fertilize (al, time, from, to);
  else for (ColumnList::iterator i = columns.begin ();
	    i != columns.end ();
	    i++)
    (*i)->fertilize (al, time, from, to);
}

void 
Field::Implementation::fertilize (const AttributeList& al, const Time& time)
{
  if (selected)
    selected->fertilize (al, time);
  else for (ColumnList::iterator i = columns.begin ();
	    i != columns.end ();
	    i++)
    (*i)->fertilize (al, time);
}

void 
Field::Implementation::fertilize (const IM& im, double from, double to) // Mineral.
{
  if (selected)
    selected->fertilize (im, from, to);
  else for (ColumnList::iterator i = columns.begin ();
	    i != columns.end ();
	    i++)
    (*i)->fertilize (im, from, to);
}

void 
Field::Implementation::fertilize (const IM& im)
{
  if (selected)
    selected->fertilize (im);
  else for (ColumnList::iterator i = columns.begin ();
	    i != columns.end ();
	    i++)
    (*i)->fertilize (im);
}

vector<const Harvest*> 
Field::Implementation::harvest (const Time& time, const string& name,
		double stub_length, 
		double stem_harvest, 
		double leaf_harvest, 
		double sorg_harvest)
{
  if (selected)
    return selected->harvest (time, name,
			      stub_length,
			      stem_harvest, leaf_harvest, sorg_harvest);
  else
    {
      vector<const Harvest*> total;
      for (ColumnList::iterator i = columns.begin ();
	   i != columns.end ();
	   i++)
	{
	  vector<const Harvest*> entry 
	    = (*i)->harvest (time, name,
			     stub_length,
			     stem_harvest, leaf_harvest, sorg_harvest);
	  
	  total.insert (total.end (), entry.begin (), entry.end ());
	}
      return total;
    }
}

void 
Field::Implementation::mix (const Time& time,
	    double from, double to, double penetration)
{
  if (selected)
    selected->mix (time, from, to, penetration);
  else for (ColumnList::iterator i = columns.begin ();
	    i != columns.end ();
	    i++)
    (*i)->mix (time, from, to, penetration);
}

void 
Field::Implementation::swap (const Time& time, double from, double middle, double to)
{
  if (selected)
    selected->swap (time, from, middle, to);
  else for (ColumnList::iterator i = columns.begin ();
	    i != columns.end ();
	    i++)
    (*i)->swap (time, from, middle, to);
}

void 
Field::Implementation::spray (const string& chemical, double amount) // [g/ha]
{
  if (selected)
    selected->spray (chemical, amount);
  else for (ColumnList::iterator i = columns.begin ();
	    i != columns.end ();
	    i++)
    (*i)->spray (chemical, amount);
}


double 
Field::Implementation::soil_temperature (double height) const  // [ cm -> dg C]
{ 
  if (selected)
    return selected->soil_temperature (height); 
  if (columns.size () != 1)
    THROW ("Cannot take soil temperature of multiple columns");

  return columns[0]->soil_temperature (height);
}

double 
Field::Implementation::soil_water_potential (double height) const // [cm -> cm]
{
  if (selected)
    return selected->soil_water_potential (height); 
  if (columns.size () != 1)
    THROW ("Cannot take soil water potential of multiple columns");

  return columns[0]->soil_water_potential (height);
}

double 
Field::Implementation::crop_ds (const string& crop) const
{ 
  if (selected)
    return selected->crop_ds (crop);
  if (columns.size () != 1)
    THROW ("Cannot find crop development state of multiple columns");

  return columns[0]->crop_ds (crop); 
} 

double 
Field::Implementation::crop_dm (const string& crop) const
{
  if (selected)
    return selected->crop_dm (crop);
  
  // We find the total DM for all the columns.
  double DM = 0.0;
  for (ColumnList::const_iterator i = columns.begin ();
       i != columns.end ();
       i++)
    DM += (*i)->crop_dm (crop);
  return DM;
}
  
void 
Field::Implementation::tick (const Time& time, const Weather& weather)
{
  for (ColumnList::const_iterator i = columns.begin ();
       i != columns.end ();
       i++)
    (*i)->tick (time, weather);
}

void 
Field::Implementation::output (Log& log, Filter& filter) const
{
  const Library& library = Librarian<Column>::library ();

  for (ColumnList::const_iterator i = columns.begin ();
       i != columns.end ();
       i++)
    {
      if (filter.check_derived ((*i)->name, library))
	{
	  log.open_entry ((*i)->name);
	  (*i)->output (log, filter.lookup_derived ((*i)->name, library));
	  log.close_entry ();
	}
    }
}

Column* 
Field::Implementation::find (const string& name) const
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
Field::Implementation::check () const
{ 
  bool ok = true;

  for (ColumnList::const_iterator i = columns.begin ();
       i != columns.end ();
       i++)
    if ((*i) == NULL || !(*i)->check ())
      ok = false;

  return ok;
}

bool 
Field::Implementation::check_am (const AttributeList& am) const
{ 
  bool ok = true;

  for (ColumnList::const_iterator i = columns.begin ();
       i != columns.end ();
       i++)
    if (!(*i)->check_am (am))
      ok = false;

  return ok;
}

void 
Field::Implementation::initialize (const vector<AttributeList*>& als,
				   const Time& time, const Weather& weather)
{
  assert (als.size () == columns.size ());
  for (unsigned int i = 0; i < columns.size (); i++)
    columns[i]->initialize (*als[i], time, weather);
}

Field::Implementation::Implementation (const vector<AttributeList*>& sequence)
  : selected (NULL)
{
  for (vector<AttributeList*>::const_iterator i = sequence.begin ();
       i != sequence.end ();
       i++)
    columns.push_back (&Librarian<Column>::create (**i));
}

Field::Implementation::~Implementation ()
{
  assert (selected == NULL);
  sequence_delete (columns.begin (), columns.end ()); 
}

Field::Restrict::Restrict (Field& f, const string& name)
  : field (f)
{ 
  field.impl.restrict (name); 
}

Field::Restrict::~Restrict ()
{ field.impl.unrestrict (); }

void 
Field::sow (const AttributeList& crop)
{ impl.sow (crop); }

void 
Field::irrigate_top (double flux, double temp, const IM& im)
{ impl.irrigate_top (flux, temp, im); }

void 
Field::irrigate_surface (double flux, double temp, const IM& im)
{ impl.irrigate_surface (flux, temp, im); }

void 
Field::fertilize (const AttributeList& al, const Time& time, // Organic.
		  double from, double to)
{ impl.fertilize (al, time, from, to); }

void 
Field::fertilize (const AttributeList& al, const Time& time)
{ impl.fertilize (al, time); }

void 
Field::fertilize (const IM& im, double from, double to) // Mineral.
{ impl.fertilize (im, from, to); }

void 
Field::fertilize (const IM& im)
{ impl.fertilize (im); }

vector<const Harvest*> 
Field::harvest (const Time& time, const string& name,
		double stub_length, 
		double stem_harvest, 
		double leaf_harvest, 
		double sorg_harvest)
{ return impl.harvest (time, name,
		       stub_length,
		       stem_harvest, leaf_harvest, sorg_harvest); }

void 
Field::mix (const Time& time,
	    double from, double to, double penetration)
{ impl.mix (time, from, to, penetration); }

void 
Field::swap (const Time& time, double from, double middle, double to)
{ impl.swap (time, from, middle, to); }

void 
Field::spray (const string& chemical, double amount) // [g/ha]
{ impl.spray (chemical, amount); }

double 
Field::soil_temperature (double height) const  // [ cm -> dg C]
{ return impl.soil_temperature (height); }

double 
Field::soil_water_potential (double height) const // [cm -> cm]
{ return impl.soil_water_potential (height); }

double 
Field::crop_ds (const string& crop) const
{ return impl.crop_ds (crop); } 

double 
Field::crop_dm (const string& crop) const
{ return impl.crop_dm (crop); } 

void
Field::tick (const Time& time, const Weather& weather)
{ impl.tick (time, weather); }

void 
Field::output (Log& log, Filter& filter) const
{ impl.output (log, filter); }

const Column* 
Field::find (const string& name) const
{ return impl.find (name); }

Column* 
Field::find (unsigned int pos) const
{ return impl.columns [pos]; }

unsigned int 
Field::size () const
{ return impl.columns.size (); }

bool 
Field::check () const
{ return impl.check (); }

bool 
Field::check_am (const AttributeList& am) const
{ return impl.check_am (am); }

void 
Field::initialize (const vector<AttributeList*>& als,
		   const Time& time, const Weather& weather)
{ impl.initialize (als, time, weather); }

Field::Field (const vector<AttributeList*>& sequence)
  : impl (*new Implementation (sequence))
{ }

Field::~Field ()
{ }
