// column.h

#ifndef COLUMN_H
#define COLUMN_H

#include "librarian.h"

class Library;
class Filter;
class Time;
class Log;
class Weather;
class AttributeList;
class Syntax;
class OrganicMatter;
class IM;
class Crop;
class Harvest;

class Column
{
public:
  string name;

  // Actions.
public:
  enum irrigation_from { top_irrigation, surface_irrigation };
  virtual void sow (const AttributeList& crop) = 0;
  virtual void irrigate (double flux, double temp, 
			 const IM&, irrigation_from) = 0;
  virtual void fertilize (const AttributeList&, const Time&, // Organic.
			  double from, double to) = 0;
  virtual void fertilize (const AttributeList&, const Time&) = 0;
  virtual void fertilize (const IM&, double from, double to) = 0; // Mineral.
  virtual void fertilize (const IM&) = 0;
  virtual vector<const Harvest*> harvest (const Time&, const string& name,
					  double stub_length, 
					  double stem_harvest, 
					  double leaf_harvest, 
					  double sorg_harvest) = 0;
  virtual void mix (const Time&,
		    double from, double to, double penetration = 1.0) = 0;
  virtual void swap (const Time&, double from, double middle, double to) = 0;

  // Conditions.
public:
  virtual double soil_temperature (double height) const = 0; // [ cm -> dg C]
  virtual double soil_water_potential (double height) const = 0; // [cm -> cm]
  // Current development stage for the crop named "crop", or
  // Crop::DSremove if no such crop is present.
  virtual double crop_ds (const string& crop) const = 0; 
  
  // Simulation.
  virtual void tick (const Time&, const Weather&) = 0;

  virtual bool check () const = 0;
  virtual bool check_am (const AttributeList& am) const = 0;
  virtual void output (Log&, Filter&) const = 0;

  // Create and Destroy.
protected:
  Column (const string& name);
public:
  virtual Column& clone (const string& name) const = 0;
  virtual void initialize (const AttributeList&, const Time&, 
			   const Weather&) = 0;

  virtual ~Column ();
};

class ColumnList : public vector <Column*>
{ 
public:
  const Column* find (const string& name) const;
  ColumnList (const vector<AttributeList*>&);
  ~ColumnList ();
};

static Librarian<Column> Column_init ("column");

#endif COLUMN_H
