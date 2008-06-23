// biopore.h --- A single class of biopores.
// 
// Copyright 2008 Per Abrahamsen and KU.
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


#ifndef BIOPORE_H
#define BIOPORE_H

#include "model.h"
#include "symbol.h"
#include "number.h"
#include <memory>
#include <vector>

class Block;
class AttributeList;
class Geometry;
class Soil;
class SoilWater;
class Log;

class Biopore : public ModelAListed
{
  // Identity.
public:
  static const char *const component;
  symbol library_id () const;

  // Extractable state.
protected:
  class Content
  // This class is intended for derived classes to store state in.
  {
  private:                      // Disable
    Content& operator= (const Content&);
    Content (const Content&);
  public:
    virtual std::auto_ptr<Content> clone () const = 0;
    Content ();
    virtual ~Content ();
  };
public:
  class State
  // This class is the user interface to state management.
  {
    std::auto_ptr<Content> content;
  public:
    const Content& inspect () const;
  private:
    State ();                   // Disable.
  public:
    State& operator= (const State&);
    State (const State&);
    State (std::auto_ptr<Content>);
    ~State ();
  };
  virtual State get_state () const;
  virtual void set_state (const State&);
  virtual bool converge (const State& old); // Are current and old state close?

  // Parameters.
protected:
  std::auto_ptr<Number> density_expr; // Biopore density [cm -> m^-2]
  const double height_start;          // Height biopores start [cm]
  const double height_end;            // Height biopores end [cm]
  const double diameter;              // [cm]

  // Utilities.
protected:
  std::vector<double> density_cell;   // Density based on cell number [m^-2]
  static symbol x_symbol ();

  static double matrix_to_biopore (double K_xx, double M_c, 
                                   double r_c, double h, double h_3);
  
  static double biopore_to_matrix (double R_wall, double M_c, 
                                   double r_c, double h, double h_3);
public:
  // Matrix sink
  virtual double matrix_biopore_matrix (size_t c, const Geometry& geo, 
                                        const Soil& soil, bool active, 
                                        double K_xx, double h) const=0;
  // Matrix sink
  virtual double matrix_biopore_drain (size_t c, const Geometry& geo, 
                                       const Soil& soil, bool active, 
                                       double K_xx, double h) const=0;

  // Interface.
public:
  virtual double air_bottom (size_t c) const = 0; // Lowest point with air [cm]
  double density (size_t c) const                 // [m^-2]
  { return density_cell[c]; }
  
  virtual void add_water (size_t c, double amount /* [cm^3] */) = 0;
  virtual void extract_water (size_t c, const double volume /* [cm^3] */ ,
                              const double Theta /* [cm^3/cm^3] */,
                              const double dt /* [h] */,
                              std::vector<double>& S_drain /* [cm^3/cm^3/h] */,
                              std::vector<double>& S_matrix, Treelog& msg) = 0;
  virtual void release_water (const Geometry& geo, const Soil& soil, 
                              const SoilWater& soil_water,
                              const double dt /* [h] */,
                              std::vector<double>& S_matrix, Treelog& msg) = 0;
  virtual void update_water () = 0;
  virtual void output (Log&) const = 0;

  // Create and Destroy.
protected:
  bool initialize_base (const Geometry&, const Scope&, Treelog&);
public:
  virtual bool initialize (const Geometry&, const Scope&, double pipe_height,
                           Treelog&) = 0;
  bool check_base (const Geometry&, Treelog& msg) const;
  virtual bool check (const Geometry&, Treelog& msg) const = 0;
  static void load_base (Syntax& syntax, AttributeList& alist);
protected:
  explicit Biopore (Block& al);
public:
  virtual ~Biopore ();
};

#endif // BIOPORE_H
