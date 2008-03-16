// chemistry.h --- Pesticides and other chemicals.
// 
// Copyright 2007 Per Abrahamsen and KVL.
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


#ifndef CHEMISTRY_H
#define CHEMISTRY_H

#include "model.h"
#include "symbol.h"
#include "alist.h"
#include <vector>

class Log;
class Geometry;
class Soil;
class SoilWater;
class SoilHeat;
class OrganicMatter;
class Movement;
class Chemical;
class Volume;
class Treelog;
class Syntax;
class Block;
class Scope;
class IM;

class Chemistry : public ModelAListed
{
  // Content.
public:
  static const char *const component;
  symbol library_id () const;

  // Query.
public:
  bool require (const symbol chem, Treelog&) const;
  virtual bool know (symbol chem) const = 0;
  virtual bool ignored (symbol chem) const = 0;
  virtual Chemical& find (symbol chem) = 0;
  virtual const std::vector<Chemical*>& all () const = 0;

public:
  void deposit (const IM& im, double dt /* [h] */, Treelog&);
  virtual void deposit (symbol chem, double amount /* [g/m^2] */,
			double dt /* [h] */, Treelog&) = 0;
  void spray (const IM& im, double dt /* [h] */, Treelog&);
  virtual void spray (symbol chem, double amount /* [g/m^2] */,
		      double dt /* [h] */, Treelog&) = 0;
  virtual void dissipate (symbol chem, double amount /* [g/m^2] */,
			  double dt /* [h] */, Treelog&) = 0;
  virtual void harvest (double removed, double surface, double dt) = 0;
  virtual void mix (const Geometry&, const Soil&, const SoilWater&, 
                    double from, double to, double penetration, double dt) = 0;
  virtual void swap (const Geometry&, const Soil&, const SoilWater&,
                     double from, double middle, double to, double dt) = 0;
  void incorporate (const Geometry& geo, const IM& im, 
		    const double from, const double to,
		    const double dt, Treelog& msg);
  void incorporate (const Geometry& geo, const IM& im, const Volume&, 
		    const double dt, Treelog& msg);
  virtual void incorporate (const Geometry& geo,
			    const symbol chem, const double amount,
			    const double from, const double to, 
			    const double dt, Treelog& msg) = 0;
  virtual void incorporate (const Geometry& geo,
			    const symbol chem, const double amount,
			    const Volume&, const double dt, Treelog& msg) = 0;

  // Simulation.
public:
  virtual void tick_top (double snow_leak_rate /* [h^-1] */,
                         double cover /* [] */,
                         double canopy_leak_rate /* [h^-1] */,
                         double surface_runoff_rate /* [h^-1] */,
                         double dt /* [h] */,
			 Treelog&) = 0;
  virtual void tick_soil (const Geometry& geo, double ponding /* [mm] */,
                          double R_mixing /* [h/mm] */,
                          const Soil&, const SoilWater&, const SoilHeat&, 
			  Movement&, const OrganicMatter&, Chemistry&, 
			  const bool flux_below, 
                          double dt, const Scope&, Treelog&) = 0;
  virtual void clear () = 0;
  virtual void output (Log&) const;

  // Create & Destroy.
public:
  static const AttributeList& default_model ();
  static const AttributeList& N_model ();
  virtual void initialize (const AttributeList&, const Geometry& geo,
                           const Soil&, const SoilWater&, const SoilHeat&,
			   Treelog&) = 0;
  virtual bool check (const Geometry&,
		      const Soil&, const SoilWater&, const SoilHeat&,
		      const Chemistry&, const Scope&, Treelog&) const = 0;
  static void load_syntax (Syntax&, AttributeList&);
protected:
  explicit Chemistry (Block& al);
public:
  virtual ~Chemistry ();
};

#endif // CHEMISTRY_H
