// difrad.h -- Diffuse radiation
// 
// Copyright 2006 Birgitte Gjettermann and KVL
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


#ifndef DIFRAD_H
#define DIFRAD_H

#include "librarian.h"
#include "alist.h"

class Log;
class Weather;
class Time;

class Difrad : public Model
{
  // Content.
public:
  const symbol name;
  static const char *const description;
  static const char *const component;
  const AttributeList alist;	// Remember attributes for checkpoint.

  // Simulation.
public:
  virtual double value (const Time&, const Weather&, Treelog&) = 0;//[]
  virtual void output (Log&) const;

  // Create and Destroy.
  static void load_syntax (Syntax&, AttributeList&);
protected:
  Difrad (Block&);
public:
  ~Difrad ();
};

#endif // DIFRAD_H
