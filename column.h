// column.h

#ifndef COLUMN_H
#define COLUMN_H

#include <string>
#include <list>
#include <vector>

class Library;
class Filter;
class Time;
class Log;
class Weather;
class Groundwater;
class AttributeList;
class Syntax;
class OrganicMatter;
class InorganicMatter;
class SoluteMatter;
class AOM;

class Column
{
public:
  string name;

  // Actions.
public:
  enum irrigation_from { top_irrigation, surface_irrigation };
  virtual void sow (const AttributeList& crop) = 0;
  virtual void irrigate (double flux, double temp, 
			 const SoluteMatter&, irrigation_from) = 0;
  virtual void fertilize (AOM&, double from, double to) = 0;
  virtual void fertilize (const InorganicMatter&, double from, double to) = 0;
  virtual void mix (double from, double to) = 0;
  virtual void mix_top (double penetration, double to) = 0;
  virtual void swap (double f1, double t1, double f2, double t2) = 0;
  
  // Simulation.
  virtual void tick (const Time&, const Weather&, Groundwater&) = 0;

  virtual bool check () const = 0;
  virtual void output (Log&, const Filter&) const = 0;

  // Library.
public:
  static const Library& library ();
  typedef Column* (*constructor) (const AttributeList&);
  static void add_type (string name, 
			const AttributeList&, const Syntax&,
			constructor);
  static void derive_type (string name, const AttributeList&, string super);
  static Column* create (const AttributeList& al);

  // Create and Destroy.
protected:
  Column (string name);
public:
  virtual ~Column ();
};

class ColumnList : public list <Column*>
{ 
public:
  ColumnList (const vector<const AttributeList*>&);
  ~ColumnList ();
};

// Ensure the Column library is initialized.
// See TC++PL, 2ed, 10.5.1, for an explanation.
static class Column_init
{
  static int count;
public:
  Column_init ();
  ~Column_init ();
} Column_init;

#endif COLUMN_H
