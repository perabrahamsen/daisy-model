// column_inorganic.C -- Daisy simulation without organic matter and nitrogen.

#include "column_base.h"
#include "am.h"

class ColumnInorganic : public ColumnBase
{
  // Actions.
public:
  void sow (const AttributeList&);
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
  void tick (const Time&, const Weather*);
  bool check_am (const AttributeList& am, ostream& err) const;

  // Create and Destroy.
public:
  Column& clone (const string& name) const
  { 
    AttributeList new_alist (alist);
    // BUG: TODO: Log state of `this' to new_alist.
    new_alist.add ("type", name);
    return *new ColumnInorganic (new_alist); 
  }
  ColumnInorganic (const AttributeList& al)
    : ColumnBase (al)
  { }
  ~ColumnInorganic ()
  { }
};

void 
ColumnInorganic::sow (const AttributeList& al)
{ vegetation.sow (al, soil); }


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
{ assert (false); }

void 
ColumnInorganic::get_no3_m (vector<double>&) const // [g/cm^3]
{ assert (false); }

void 
ColumnInorganic::put_surface_no3 (double) // [g/cm^2]
{ assert (false); }

double 
ColumnInorganic::get_surface_no3 () const // [g/cm^2]
{
  assert (false); 
  return -42.42e42;
}

double 
ColumnInorganic::get_smb_c_at (unsigned int) const //[g C/cm設
{
  assert (false); 
  return -42.42e42;
}

double 
ColumnInorganic::get_co2_production_at (unsigned int) const // [g C/cm設
{
  assert (false); 
  return -42.42e42;
}

void
ColumnInorganic::tick (const Time& time, const Weather* global_weather)
{
  // Weather.
  if (weather)
    weather->tick (time);
  const Weather& my_weather = *(weather ? weather : global_weather);

  // Remove old source sink terms. 
  soil_water.clear (soil);
  soil_chemicals.clear ();

  // Early calculation.
  surface.mixture (soil_chemicals);
  soil_water.macro_tick (soil, surface);

  bioclimate.tick (surface, my_weather, 
		   vegetation, soil, soil_water, soil_heat);
  vegetation.tick (time, bioclimate, soil, soil_heat, soil_water);
  groundwater.tick (time);

  // Transport.
  soil_heat.tick (time, soil, soil_water, surface, my_weather);
  soil_water.tick (soil, surface, groundwater);
  soil_chemicals.tick (soil, soil_water, soil_heat, NULL, 
		       surface.chemicals_down ());
}

bool 
ColumnInorganic::check_am (const AttributeList&, ostream&) const 
{
  assert (false); 
  return false;
}

static struct ColumnInorganicSyntax
{
  static Column& make (const AttributeList& al)
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
