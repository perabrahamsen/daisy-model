// oldunits.h -- Old conversion between different dimensions.
// 
// Copyright 2002 Per Abrahamsen and KVL.
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
//
// This file should only be included as a fallback for the new unit
// interface.

#ifndef OLDUNITS_H
#define OLDUNITS_H

#include "symbol.h"
#include <boost/shared_ptr.hpp>

static class Oldunits
{
  // Content.
private:
  struct Content;
  static Content* content;
  static int count;

  // Symbols.
public:
  static symbol h ();
  static symbol mm ();
  static symbol per_mm ();
  static symbol mm_per_h ();
  static symbol cm ();
  static symbol cm_per_h ();
  static symbol cm2 ();
  static symbol cm3 ();
  static symbol per_h ();
  static symbol ppm ();

  // Conversion.
public:
  struct Convert
  {
    // Use.
    virtual double operator()(double value) const = 0;
    virtual bool valid (double value) const;

    // Create and destroy.
    Convert ();
    virtual ~Convert ();
  };

  // Utilities.
public:
  static void add (const symbol from, const symbol to,
		   double factor, double offset = 0.0);
  static void add (const symbol from, const symbol to, 
                   boost::shared_ptr<Convert>);
  static double convert (const symbol from, const symbol to, double value);
  static bool can_convert (const symbol from, const symbol to);
  static bool can_convert (const symbol from, const symbol to, double value);
  static const Convert& get_convertion (const symbol from, const symbol to);
  static symbol multiply (const symbol, const symbol);

  // Create and destroy.
private:
  Oldunits (const Oldunits&);
  static void standard_conversions ();
public:
  Oldunits ();
  ~Oldunits ();
} oldunits;

#endif // OLDUNITS_H
