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

#include "model.h"
#include "scope.h"
#include "symbol.h"

class BlockModel;
class Time;
class Treelog;

class WSource : public Model, public Scope
{
  // Content.
public:
  const symbol name;
  static const char *const component;
  symbol library_id () const;

  // Meta information.
public:  
  // KEY is available in this source.  
  bool has (symbol key) const;
  // KEY is available in this source.  
  double value (symbol key) const;
  // KEY is measured at STATION.
  virtual symbol station (symbol key) const = 0;  
  // KEY is measured at SCREEN HEIGHT.
  virtual double screen_height (symbol key) const = 0; 

  // Timestep.
public:
  virtual const Time& begin () const = 0;         // Start of timestep.
  virtual const Time& end () const = 0;           // End of timestep.
  virtual void tick () = 0;
  
  // Create and Destroy.
public:
  virtual void initialize (Treelog&) = 0;
  using Scope::check;
  virtual bool check (Treelog&) const = 0;
protected:
  explicit WSource ();
public:
  ~WSource ();
};

#endif // WSOURCE_H
