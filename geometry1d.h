// geometry1d.h
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


#ifndef GEOMETRY1D_H
#define GEOMETRY1D_H

#include "geometry.h"
#include "syntax.h"
#include <vector>
#include <string>

class Block;
class AttributeList;
class Treelog;
class Groundwater;

class Geometry1D : public Geometry
{
  // Parameters.
  std::vector<double> zplus_;	// Lower boundary of each interval.
  std::vector<double> z_;       // Center of each interval.
  std::vector<double> dz_;      // Size of each interval.

public:
  // Accessors.
  inline unsigned int size () const
  { return size_; }
  inline double zplus (unsigned int i) const
  { return zplus_[i]; }
  inline double z (unsigned int i) const
  { return z_[i]; }
  inline double dz (unsigned int i) const
  { return dz_[i]; }
  inline double volume (unsigned int i) const
  { return dz_[i] * 1.0 /* [cm] */ * 1.0 /* [cm] */; }
  bool contain_z (size_t i, double z) const;

  unsigned int interval_plus (double z) const;
  unsigned int interval_border (double z) const;

  // Vector operations.
  void mix (std::vector<double>& v, double from, double to) const;
  void mix (std::vector<double>& v, double from, double to, 
            std::vector<double>& change) const;
  void add (std::vector<double>& v,
            double from, double to, double amount) const;
  void add (std::vector<double>& v, const std::vector<double>& density,
	    double amount) const;
  double extract (std::vector<double>& v, double from, double to) const;
  void set (std::vector<double>& v,
            double from, double to, double amount) const;
  void swap (std::vector<double>& v,
             double from, double middle, double to) const;
  void swap (std::vector<double>& v, double from, double middle, double to, 
             std::vector<double>& change) const;
  double total (const std::vector<double>& v) const;
  double total (const std::vector<double>& v, double from, double to) const;

  // Layers -- Support initializing soil arrays layer by layer.
  static void add_layer (Syntax& syntax, Syntax::category, 
                         const std::string& name,
			 const std::string& dimension,
                         const std::string& description);
  void initialize_layer (std::vector<double>& value, 
			 const AttributeList& al, 
			 const std::string& name, Treelog&) const;

  // Creation.
public:
  bool check (Treelog&) const;
  static void load_syntax (Syntax&, AttributeList&);
  Geometry1D (Block&);
  void initialize_zplus (const Groundwater& groundwater,
			 const std::vector<double>& fixed,
			 const double max_rooting_depth,
			 const double max_interval,
			 Treelog& msg);
  virtual ~Geometry1D ();
};

#endif // GEOMETRY1D_H
