// horizon.h
// 
// Copyright 1996-2001, 2003 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001, 2003 KVL.
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


#ifndef HORIZON_H
#define HORIZON_H

#include "librarian.h"
#include <vector>
#include <memory>

class Log;
class Hydraulic;
class Tortuosity;
class Texture;
class Treelog;

class Horizon : public Model 
{
  // Content.
private:
  struct Implementation;
  std::auto_ptr<Implementation> impl;
  double fast_clay;
  double fast_humus;
public:
  const symbol name;
  static const char *const description;
  static const char *const component;

  // Water.
public:
  std::auto_ptr<Hydraulic> hydraulic;
  double anisotropy () const;
  double heat_conductivity (double Theta, double Ice) const; // [erg/cm/h/dg C]
  double heat_capacity (double Theta, double Ice) const; // [erg/cm^3/dg C]
  
  // Texture.
public:
  std::auto_ptr<Tortuosity> tortuosity;
  double dry_bulk_density () const;
  virtual double texture_below (double size /* [um] */) const = 0;
  double clay () const;
  double humus () const;
  double humus_C () const;
  const std::vector<double>& SOM_fractions () const;
  const std::vector<double>& SOM_C_per_N () const;
  double C_per_N () const;
  double turnover_factor () const;
  virtual double quartz () const; // Quartz fraction of minerals [0;1].

  // Chemistry.
public:
  bool has_attribute (const std::string& name) const;
  double get_attribute (const std::string& name) const;
  std::string get_dimension (const std::string& name) const;

  // Simulation.
public:
  void nitrification (const double M, const double C, 
                      const double M_left,
                      const double h, const double T,
                      double& NH4, double& N2O, double& NO3, 
                      const double dt) const;
  void output (Log&) const;

  // Create and Destroy.
private:
  Horizon (const Horizon&);
public:
  static bool check_alist (const AttributeList& al, Treelog& err);
  static void load_syntax (Syntax&, AttributeList&);
  explicit Horizon (Block&);
  virtual void initialize (bool top_soil, int som_size, Treelog&) = 0;
protected:
  void initialize_base (bool top_soil, int som_size, const Texture& texture, 
                        Treelog&);
public:
  ~Horizon ();
};

#ifdef FORWARD_TEMPLATES
template<>
BuildBase* Librarian<Horizon>::content;
#endif

static Librarian<Horizon> Horizon_init;

#endif // HORIZON_H
