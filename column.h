// column.h

#ifndef COLUMN_H
#define COLUMN_H

#include <std/string.h>

class Library;
class Filter;
class Time;
class Log;
class Weather;
class Groundwater;
class AttributeList;
class Syntax;

class Column
{
public:
  string name;

  // Actions.
public:
  virtual void sow (string crop, Log&) = 0;

  virtual bool check (Log&) const = 0;
  virtual void output (Log&, const Filter*) const = 0;

  // Simulation.
  virtual void tick (const Time&, const Weather&, const Groundwater&) = 0;

  // Communication with crops.
  virtual double SoilTemperature (double depth) const = 0;
  virtual double MaxRootingDepth () const = 0;

  // Library.
public:
  static const Library& par_library ();
  static const Library& var_library ();
  typedef Column* (*constructor) (string name, 
				  const AttributeList& par,
				  const AttributeList& var);
  static void add_type (string name, 
			const AttributeList& parList, const Syntax& parSyntax,
			const AttributeList& varList, const Syntax& varSyntax,
			constructor);
  static void derive_type (string name, const AttributeList& par, string super);
  static Column* create (string, const AttributeList& var);

  // Create and Destroy.
protected:
  Column (string name);
public:
  virtual ~Column ();
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
