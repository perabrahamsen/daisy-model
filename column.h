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

class Column
{
public:
  string name;

  // Actions.
public:
  virtual void sow (const Library& croplib, string crop, Log&) = 0;

  virtual bool check (Log&) const = 0;
  virtual void output (Log&, const Filter*) const = 0;

  // Simulation.
  virtual void tick (const Time&, const Weather&, const Groundwater&) = 0;

  // Communication with crops.
  virtual double SoilTemperature (double depth) const = 0;
  virtual double MaxRootingDepth () const = 0;

  // Create and Destroy.
public:
  Column (string name);
  virtual ~Column ();
};

#endif COLUMN_H
