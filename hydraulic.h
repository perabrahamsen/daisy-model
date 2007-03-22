// hydraulic.h
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


#ifndef HYDRAULIC_H
#define HYDRAULIC_H

#include "librarian.h"

class Log;
class PLF;
class Treelog;
class Texture;

class Hydraulic : public Model 
{
  // Content.
public:
  const symbol name;
  static const char *const description;
  static const char *const component;
  //
private:
  struct K_at_h;
protected:
  const K_at_h *const K_init;

  // Standard parameters.
public:
  double Theta_sat;
  const double Theta_res;
  /*const*/ double K_sat;
  inline double porosity () const
  { return Theta_sat; }
  virtual void set_porosity (double Theta);

  // Convertion functions.
public:
  virtual double Theta (double h) const = 0;
  virtual double K (double h) const = 0;
  virtual double Cw2 (double h) const = 0;
  virtual double h (double Theta) const = 0;
  virtual double M (double h) const = 0;

  // Simulation.
public:
  virtual void output (Log&) const;

  // Tools for derived classes.
protected:
  void K_to_M (PLF&, int) const;

  // Create and Destroy.
public:
  static void load_Theta_res (Syntax&, AttributeList&);
  static void load_Theta_sat (Syntax&, AttributeList&);
  static void load_K_sat_optional (Syntax&, AttributeList&);
  static void load_K_sat (Syntax&, AttributeList&);
  virtual void initialize (const Texture&, double rho_b, bool top_soil,
			   Treelog&);
  Hydraulic (Block&);
  ~Hydraulic ();
};

static Librarian<Hydraulic> Hydraulic_init;

#endif // HYDRAULIC_H
