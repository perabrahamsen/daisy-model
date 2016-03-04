// wsource_base.h -- Base classs for wsource parameters.
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

#ifndef WSOURCE_BASE_H
#define WSOURCE_BASE_H

#include "wsource_weather.h"

class WSourceBase : public WSourceWeather
{
private:
  struct Implementation;
  const std::unique_ptr<Implementation> impl;

  // Scope interface.
public:
  void entries (std::set<symbol>& e) const;
  Attribute::type lookup (const symbol key) const;
  symbol dimension (const symbol key) const;
  symbol description (const symbol key) const;
  int type_size (symbol tag) const;
  bool check (const symbol key) const;
  double number (const symbol key) const;
  symbol name (const symbol key) const;
  int value_size (symbol tag) const;

  // WSource interface.
public:
  const Time& data_begin () const; // Start of first timestep.
  const Time& data_end () const;   // End of last timestep.
  const Time& begin () const;
  const Time& end () const;
  const std::vector<double>& number_sequence (symbol) const;
  const std::vector<double>& end_number_sequence (symbol) const;
  double meta_timestep (symbol key) const;
  bool meta_check (symbol key, symbol meta) const;
  double meta_number (symbol key, symbol meta) const;
  symbol meta_name (symbol key, symbol meta) const;
  bool meta_end_check (symbol key, symbol meta) const;
  double meta_end_number (symbol key, symbol meta) const;
  symbol meta_end_name (symbol key, symbol meta) const;

  // Create and destroy.
protected:
  WSourceBase (const BlockModel& al);
  ~WSourceBase ();
};

#endif // WSOURCE_BASE_H
