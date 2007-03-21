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

class Surface;
class Groundwater;
class GeometryVert;
class Soil;
class SoilHeat;
class Library;
class Syntax;
class AttributeList;
class Log;

class UZmodel : public Model
{
  // Content.
public: 
  const symbol name;
  static const char *const description;
  static const char *const component;

  // Simulate.
public:
  virtual void tick (Treelog&, const GeometryVert& geo,
                     const Soil& soil, const SoilHeat&,
		     unsigned int first, const Surface& top, 
                     const size_t top_edge,
		     unsigned int last, const Groundwater& bottom, 
		     const std::vector<double>& S,
		     const std::vector<double>& h_old,
		     const std::vector<double>& Theta_old,
		     const std::vector<double>& h_ice,
		     std::vector<double>& h,
		     std::vector<double>& Theta,
                     const size_t q_offset,
		     std::vector<double>& q,
                     double dt) = 0;

  // Create and Destroy.
public:
  static const AttributeList& default_model ();
  static const AttributeList& reserve_model ();
  virtual void has_macropores (bool) = 0; // Tell UZ that there are macropores.
protected:
  UZmodel (Block&);
public:
  ~UZmodel ();
};

#ifdef FORWARD_TEMPLATES
template<>
BuildBase* Librarian<UZmodel>::content;
#endif

static Librarian<UZmodel> UZmodel_init;

#endif // UZMODEL_H
