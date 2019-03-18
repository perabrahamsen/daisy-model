// wsource_indirect.h -- Base class for wsources based on other wsources.
// 
// Copyright 2011 KU
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

#ifndef WSOURCE_INDIRECT_H
#define WSOURCE_INDIRECT_H

#include "wsource_weather.h"

class WSourceIndirect : public WSourceWeather
{
public:
  std::unique_ptr<WSource> source;

  // Scope interface.
public:
  void entries (std::set<symbol>& e) const;
  Attribute::type lookup (const symbol key) const;
  symbol dimension (const symbol key) const;
  symbol description (const symbol key) const;
  bool check (const symbol key) const;
  double number (const symbol key) const;
  symbol name (const symbol key) const;

  // WSource interface.
public:
  int type_size (symbol tag) const; // Don't use default from Scope.
  int value_size (symbol tag) const;
  bool end_check (symbol key) const;
  double end_number (symbol key) const;
  symbol end_name (symbol key) const;
  const std::vector<double>& number_sequence (symbol) const;
  const std::vector<double>& end_number_sequence (symbol) const;

  double meta_timestep (symbol key) const;
  bool meta_check (symbol key, symbol meta) const;
  double meta_number (symbol key, symbol meta) const;
  symbol meta_name (symbol key, symbol meta) const;
  bool meta_end_check (symbol key, symbol meta) const;
  double meta_end_number (symbol key, symbol meta) const;
  symbol meta_end_name (symbol key, symbol meta) const;

  const Time& data_begin () const; // Start of first timestep.
  const Time& data_end () const;   // End of last timestep.
  const Time& begin () const;
  const Time& end () const;
  double timestep () const;           // Length of timetstep [h]
  void source_tick (Treelog& msg);
  bool done () const;

  void source_initialize (Treelog&);
  void skip_ahead (const Time&, Treelog&);
  bool source_check (Treelog&) const;

  // Create and destroy.
public:
  WSourceIndirect (const BlockModel& al);
  ~WSourceIndirect ();
};

#endif // WSOURCE_INDIRECT_H
