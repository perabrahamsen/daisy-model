// wsource.h -- Selected weather data.
// 
// Copyright 2010 KU
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

#ifndef WSOURCE_H
#define WSOURCE_H

#include "weather.h"
#include "model_derived.h"
#include "scope.h"
#include "symbol.h"
#include <vector>

class BlockModel;
class Time;
class Treelog;

class WSource : public ModelDerived, public Scope, public Weather
{
  // Content.
public:
  static const char *const component;
  symbol library_id () const;
  virtual symbol title () const;

  // Scope.
public:
  int type_size (symbol tag) const = 0; // Don't use default from Scope.
  int value_size (symbol tag) const = 0;
  // symbol name (symbol) const = 0;

  // Timestep.
public:
  virtual const Time& data_begin () const = 0; // Start of first timestep.
  virtual const Time& data_end () const = 0;   // End of last timestep.
  virtual const Time& begin () const = 0;         // Start of timestep.
  virtual const Time& end () const = 0;           // End of timestep.
  virtual double timestep () const = 0;           // Length of timetstep [h]

  // End of timstep.
public:  
  virtual bool end_check (symbol key) const = 0;
  virtual double end_number (symbol key) const = 0;
  virtual symbol end_name (symbol key) const = 0;

  // Number sequences.
  virtual const std::vector<double>& number_sequence (symbol) const = 0;
  virtual const std::vector<double>& end_number_sequence (symbol) const = 0;

  // Meta information.
public:  
  virtual double meta_timestep (symbol key) const = 0;
  virtual bool meta_check (symbol key, symbol meta) const = 0;
  virtual double meta_number (symbol key, symbol meta) const = 0;
  virtual symbol meta_name (symbol key, symbol meta) const = 0;
  virtual bool meta_end_check (symbol key, symbol meta) const = 0;
  virtual double meta_end_number (symbol key, symbol meta) const = 0;
  virtual symbol meta_end_name (symbol key, symbol meta) const = 0; 

  // Simulation.
public:
  virtual void source_tick (Treelog& msg) = 0;
  virtual bool done () const = 0;
  virtual double suggest_dt () const = 0;   // [h]
  virtual void weather_tick (const Time& time, Treelog&) = 0;
  virtual void output (Log&) const = 0;

  // Create and Destroy.
public:
  virtual void source_initialize (Treelog&) = 0;
  virtual bool source_check (Treelog&) const = 0;
  virtual bool weather_check (const Time& from, const Time& to,
                              Treelog&) const = 0;
  virtual void weather_initialize (const Time& time, Treelog& msg) = 0;
  virtual void skip_ahead (const Time& time, Treelog& msg) = 0;
protected:
  explicit WSource (const symbol name);
public:
  ~WSource ();
};

#endif // WSOURCE_H
