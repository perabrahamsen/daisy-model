// column.h

#ifndef COLUMN_H
#define COLUMN_H

#include <std/string.h>
#include <list.h>

class Library;
class Filter;
class Time;
class Log;
class Weather;
class Groundwater;
class AttributeList;
class Sequence;
class Syntax;

class Column
{
public:
  string name;

  // Actions.
public:
  virtual void sow (const AttributeList& crop, Log&) = 0;

  virtual bool check (Log&) const = 0;
  virtual void output (Log&, const Filter*) const = 0;

  // Simulation.
  virtual void tick (const Time&, const Weather&, const Groundwater&) = 0;

  // Communication with crops.
  virtual double SoilTemperature (double depth) const = 0;
  virtual double MaxRootingDepth () const = 0;

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
  ColumnList (const Sequence&);
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
