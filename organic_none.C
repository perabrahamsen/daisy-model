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
#include "organic.h"
#include "geometry.h"
#include "librarian.h"
#include "frame.h"

// Convertions

struct OrganicNone : public OrganicMatter
{
  // Content.
  std::vector<bool> active_;

  // Simulation.
  void clear ()
  { }
  void monthly (const Metalib&, const Geometry&, Treelog&)
  { }
  const std::vector<bool>& active () const
  { return active_; }
  void tick (const Geometry&, const Soil&, const SoilpH&, 
             const SoilWater&, const SoilHeat&, 
             const std::vector<double>& /* tillage_age */,
	     Chemistry&, double, Treelog&)
  { }
  void transport (const Units&, const Geometry&,
                  const Soil&, const SoilWater&, const SoilHeat&, Treelog&)
  { }
  const std::vector<DOM*>& fetch_dom () const
  { 
    static std::vector<DOM*> dom; 
    return dom;
  }
  void output (Log&) const
  { }
  double top_DM () const
  { return 0.0; }
  double CO2 (size_t) const	// [g C/cm³]
  { return 0.0; }
  double CO2_fast (size_t) const	// [g C/cm³]
  { return 0.0; }
  void mix (const Geometry&, const Soil&, const SoilWater&,
	    double, double, double)
  { }
  void swap (const Geometry&, const Soil&, const SoilWater&, 
	     double, double, double)
  { }
  double AOM_C (const Geometry&, double, double) const
  { return 0.0; }
  void store_SOM ()
  { }
  void restore_SOM ()
  { }
  const std::vector <AM*> get_am () const
  {
    static std::vector <AM*> am;
    return am;
  }
  const std::vector <SMB*> get_smb () const
  {
    static std::vector <SMB*> smb;
    return smb;
  }
  void add_stationary (const std::vector<double>& C,
		       const std::vector<double>& N,
		       const int where, const double dt)
  { }

  // Communication with external model.
  double get_smb_c_at (size_t) const // [g C/cm³]
  { return 0.0; }

  // Create and Destroy.
  int som_pools () const
  { return 3; }
  bool check (const Units&,  const Geometry&,
              const Soil&, const SoilWater&, const SoilHeat&,
	      const Chemistry&, Treelog&) const
  { return true; }
  bool check_am (const FrameModel&, Treelog&) const
  { return true; }
  void add (AM&)
  { }
  void fertilize (const Metalib&, const FrameModel&, const Geometry&,
                  const Time&, Treelog&)
  { }
  void fertilize (const Metalib&, const FrameModel&, const Geometry&, 
                  double, double, const Time&, Treelog&)
  { }
  void fertilize (const Metalib&, const FrameModel&, const Geometry&, 
                  const Volume&, const Time&, Treelog&)
  { }
  AM* find_am (symbol, symbol) const
  { return NULL; }
  void add_to_buffer (const Geometry& geo,
		      const double from /* [cm] */,
		      const double to /* [cm] */,
		      const double C /* [g/cm^2] */,
		      const double N /* [g/cm^2] */)
  { }
  void initialize (const Metalib&, 
                   const Units&, const Frame&, const Geometry& geo,
                   const Soil&, const SoilpH&, 
                   const SoilWater&, const SoilHeat&, 
                   double, Treelog&)
  { active_.insert (active_.end (), geo.cell_size (), false); }
  explicit OrganicNone (const BlockModel& al)
    : OrganicMatter (al)
  { }
  ~OrganicNone ()
  { }
};

static struct OrganicNoneSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new OrganicNone (al); }

  OrganicNoneSyntax ()
    : DeclareModel (OrganicMatter::component, "none", "\
Ignore all soil organic matter dynamics.")
  { }
  void load_frame (Frame&) const
  { }
} OrganicNone_syntax;

// organic_none.C ends here.
