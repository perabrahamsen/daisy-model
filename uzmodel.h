// uzmodel.h
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


#ifndef UZMODEL_H
#define UZMODEL_H

#include "librarian.h"

class Soil;
class SoilHeat;
class Library;
class Syntax;
class AttributeList;
class Log;

class UZtop
{
public:
  virtual bool flux_top () const = 0;
  virtual double q () const = 0;
  double h () const
  { return -q () * dt; }
  virtual void flux_top_on () const = 0;
  virtual void flux_top_off () const = 0;
  virtual bool accept_top (Treelog&, double) = 0;
  virtual bool soil_top () const;
  virtual ~UZtop ();
};

class UZbottom
{
public:
  enum bottom_t { pressure, lysimeter, forced_flux, free_drainage };
  virtual bottom_t bottom_type () const = 0;
  virtual double q_bottom () const;
  virtual bool accept_bottom (double) = 0;
  virtual ~UZbottom ();
};

class UZmodel : public UZtop, public UZbottom
{
  // Content.
public: 
  const symbol name;
  static const char *const description;

  // UZtop.
public:
  bool flux_top () const = 0;
  double q () const = 0;
  void flux_top_on () const = 0;
  void flux_top_off () const = 0;
  bool accept_top (Treelog&, double) = 0;

  // UZbottom.
public:
  bottom_t bottom_type () const = 0;
  bool accept_bottom (double) = 0;
  
  // Simulate.
public:
  virtual bool tick (Treelog&, const Soil& soil, const SoilHeat&,
		     unsigned int first, const UZtop& top, 
		     unsigned int last, const UZbottom& bottom, 
		     const vector<double>& S,
		     const vector<double>& h_old,
		     const vector<double>& Theta_old,
		     const vector<double>& h_ice,
		     vector<double>& h,
		     vector<double>& Theta,
		     vector<double>& q) = 0;
  virtual void output (Log&) const = 0;

  // Create and Destroy.
public:
  virtual void has_macropores (bool); // Tell UZ that there is macropores.
protected:
  UZmodel (const AttributeList&);
public:
  virtual ~UZmodel ();
};

#ifdef FORWARD_TEMPLATES
EMPTY_TEMPLATE
Librarian<UZmodel>::Content* Librarian<UZmodel>::content;
#endif

static Librarian<UZmodel> UZmodel_init ("uzmodel");

#endif // UZMODEL_H
