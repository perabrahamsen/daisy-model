// geometry.h -- Abstract interface to geometric information.
// 
// Copyright 2006 Per Abrahamsen and KVL.
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


#ifndef GEOMETRY_H
#define GEOMETRY_H

#include "syntax.h"
#include "mathlib.h"
#include <vector>
#include <string>

class Block;
class AttributeList;
class Treelog;
class Groundwater;

class Geometry
{
  // Parameters.
protected:
  unsigned int size_;		// Number of intervals.
public:

#if 1
  virtual double zplus (unsigned int i) const = 0;
  virtual double dz (unsigned int i) const = 0;
  virtual unsigned int interval_plus (double z) const = 0;
  virtual unsigned int interval_border (double z) const = 0;
#endif

  // Accessors.
  inline unsigned int size () const // Number of nodes.
  { return size_; }
  virtual double z (unsigned int i) const = 0; // Node depth [cm]
  virtual double volume (unsigned int i) const = 0; // Node volume [cm^3]
private:
  virtual bool contain_z (size_t i, double z) const = 0; // True iff node i
                                                         // includes depth z
public:
  template<class T>
  double content_at (T& obj, double (T::*content) (int), const double z) const
  {
    double total_volume = 0.0;
    double total_content = 0.0;
  
    for (size_t i = 0; i < this->size (); i++)
      if (this->contain_z (i, z))
        {
          const double volume = this->volume (i);
          total_volume += volume;
          total_content += volume * (obj.*content) (i);
        }
    if (!std::isnormal (total_volume))
      return 0.0;
    
    return total_content / total_volume;
  }

  // Vector operations.
  virtual void mix (std::vector<double>& v, double from, double to) const = 0;
  virtual void mix (std::vector<double>& v, double from, double to, 
                    std::vector<double>& change) const = 0;
  virtual void add (std::vector<double>& v,
                    double from, double to, double amount) const = 0;
  virtual void add (std::vector<double>& v, const std::vector<double>& density,
                    double amount) const = 0;
  virtual double extract (std::vector<double>& v, 
                          double from, double to) const = 0;
  virtual void set (std::vector<double>& v,
                    double from, double to, double amount) const = 0;
  virtual void swap (std::vector<double>& v,
                     double from, double middle, double to) const = 0;
  virtual void swap (std::vector<double>& v, 
                     double from, double middle, double to, 
                     std::vector<double>& change) const = 0;
  virtual double total (const std::vector<double>& v) const = 0;
  virtual double total (const std::vector<double>& v, 
                        double from, double to) const = 0;

  // Layers -- Support initializing soil arrays layer by layer.
  static void add_layer (Syntax& syntax, Syntax::category, 
                         const std::string& name,
			 const std::string& dimension,
                         const std::string& description);
  virtual void initialize_layer (std::vector<double>& value, 
                                 const AttributeList& al, 
                                 const std::string& name, Treelog&) const = 0;

  // Creation.
public:
  virtual bool check (Treelog&) const = 0;
  Geometry (Block&);
  virtual void initialize_zplus (const Groundwater& groundwater,
                                 const std::vector<double>& fixed,
                                 const double max_rooting_depth,
                                 const double max_interval,
                                 Treelog& msg) = 0;
  virtual ~Geometry ();
};

#endif // GEOMETRY_H
