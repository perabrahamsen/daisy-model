// organic_none.C -- Ignore soil organic matter.
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

#define BUILD_DLL
#include "organic_matter.h"
#include "alist.h"
#include "geometry.h"
#include "librarian.h"

// Convertions

struct OrganicNone : public OrganicMatter
{
  // Content.
  std::vector<bool> active_;

  // Simulation.
  void clear ()
  { }
  void monthly (const Geometry&)
  { }
  const std::vector<bool>& active () const
  { return active_; }
  void tick (const Geometry&,
             const SoilWater&, const SoilHeat&, 
	     SoilNO3&, SoilNH4&, double, Treelog&)
  { }
  void transport (const Soil&, const SoilWater&, Treelog&)
  { }
  const std::vector<DOM*>& fetch_dom () const
  { 
    static std::vector<DOM*> dom; 
    return dom;
  }
  void output (Log&) const
  { }
  double CO2 (size_t) const	// [g C/cm³]
  { return 0.0; }
  double CO2_fast (size_t) const	// [g C/cm³]
  { return 0.0; }
  void mix (const Geometry&, const Soil&, const SoilWater&,
	    double, double, double, const Time&, double)
  { }
  void swap (const Geometry&, const Soil&, const SoilWater&, 
	     double, double, double, const Time&, double)
  { }

  // Communication with external model.
  double get_smb_c_at (size_t) const // [g C/cm³]
  { return 0.0; }

  // Create and Destroy.
  int som_pools () const
  { return 3; }
  bool check (const Soil&, Treelog&) const
  { return true; }
  bool check_am (const AttributeList&, Treelog&) const
  { return true; }
  void add (AM&)
  { }
  void fertilize (const AttributeList&, const Geometry&, double)
  { }
  void fertilize (const AttributeList&, const Geometry&, 
                  double, double, double)
  { }
  AM* find_am (symbol, symbol) const
  { return NULL; }
  void initialize (Block&, const AttributeList&, const Geometry& geo,
                   const Soil&, const SoilWater&, double)
  { active_.insert (active_.end (), geo.cell_size (), false); }
  explicit OrganicNone (Block& al)
    : OrganicMatter (al)
  { }
  ~OrganicNone ()
  { }
};

static struct OrganicNoneSyntax
{
  static Model& make (Block& al)
  { return *new OrganicNone (al); }

  OrganicNoneSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "\
Ignore all soil organic matter dynamics.");
 
    Librarian::add_type (OrganicMatter::component, "none", alist, syntax, &make);
  }
} OrganicNone_syntax;
