// macro.h --- preferential flow.
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


#ifndef MACRO_H
#define MACRO_H

#include "librarian.h"
#include <vector>

class Soil;
class Log;
class UZtop;

class Macro
{
  // Content.
public:
  const string name;
  static const char *const description;
  virtual bool none () const;

  // Simulation.
public:
  virtual void tick (const Soil& soil, unsigned int first, unsigned int last,
		     UZtop& surface,
		     const vector<double>& h_ice,
		     const vector<double>& h,
		     const vector<double>& Theta,
		     vector<double>& S,
		     vector<double>& S_p,
		     vector<double>& q_p, Treelog&) = 0;
  virtual void output (Log&) const = 0;

  // Create and Destroy.
public:
  static Macro& create (double depth); // Defined in macro_std.C.
protected:
  Macro (const AttributeList& al);
public:
  virtual ~Macro ();
};

#if !defined (__BORLANDC__)
EMPTY_TEMPLATE
Librarian<Macro>::Content* Librarian<Macro>::content;
#endif

static Librarian<Macro> Macro_init ("macro");

#endif // MACRO_H
