// groundwater.h
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
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


#ifndef GROUNDWATER_H
#define GROUNDWATER_H

#include "uzmodel.h"
#include "librarian.h"

class Time;
class Treelog;
class SoilHeat;

class Groundwater : public UZbottom
{
  // Content.
public:
  const symbol name;
  static const char *const description;
  virtual bool is_pipe () const;
  virtual double pipe_height () const;
  virtual double Z_aquitard () const;
  virtual double K_aquitard () const;
  virtual void set_Z_aquitard (double);

  // Simulation.
public:
  virtual void tick (const Time& time, Treelog&) = 0;
  virtual void update_water (const Soil&, const SoilHeat&, UZtop&,
			     vector<double>& S_sum,
			     vector<double>& S_drain,
			     vector<double>& h,
			     const vector<double>& h_ice,
			     vector<double>& Theta,
			     vector<double>& q,
			     const vector<double>& q_p,
			     Treelog& msg);
  virtual void output (Log&) const;

  // Accessors.
public:
  virtual double table () const = 0;

    // Create and Destroy.
public:
  static void load_syntax (Syntax&, AttributeList&);
  virtual void initialize (const Time& time, const Soil&, Treelog&);
protected:
  Groundwater (const AttributeList& al);
public:
  virtual ~Groundwater ();
};

#if !defined (__BORLANDC__)
EMPTY_TEMPLATE
Librarian<Groundwater>::Content* Librarian<Groundwater>::content;
#endif

static Librarian<Groundwater> Groundwater_init ("groundwater");

#endif // GROUNDWATER_H
