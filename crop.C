// crop.C -- Generic crop interface.
//
// Copyright 1996-2007 KVL, Per Abrahamsen and Søren Hansen
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

#include "crop.h"
#include "om.h"
#include "block_model.h"
#include "mathlib.h"
#include "librarian.h"
#include "vcheck.h"

const double Crop::DSremove = -5001.0;

const char *const Crop::component = "crop";

symbol 
Crop::library_id () const
{
  static const symbol id (component);
  return id;
}

double 
Crop::minimum_light_fraction () const
{ return 0.0; }

double 
Crop::rs_min () const
{ return 100.0; }

double 
Crop::rs_max () const
{ return 1.0e5; }

double 
Crop::shadow_stomata_conductance () const
{ return LAI () / rs_max(); }

double 
Crop::sunlit_stomata_conductance () const
{ return LAI () / rs_max(); }

double 
Crop::leaf_width () const
{ return 3.0; }

double 
Crop::SimLAI () const
{ return LAI (); }

double 
Crop::albedo () const
{ return 0.20; }

const Harvest&
Crop::pluck (const symbol, const Time&, const Geometry&,
             const double, const double, const double,
             std::vector<AM*>& residuals,
             double& residuals_DM,
             double& residuals_N_top,
             double& residuals_C_top,
             std::vector<double>& residuals_N_soil,
             std::vector<double>& residuals_C_soil,
             Treelog&)
{ throw objid + " is unpluckable"; }

void
Crop::kill (const symbol name, const Time& time, const Geometry& geo,
	    std::vector<AM*>& residuals, double& residuals_DM,
	    double& residuals_N_top, double& residuals_C_top,
	    std::vector<double>& residuals_N_soil, 
            std::vector<double>& residuals_C_soil,
	    Treelog& out)
{ 
    harvest (name, time, geo, 
             0.0, 0.0, 0.0, 0.0, true, residuals, 
             residuals_DM, residuals_N_top, residuals_C_top, 
             residuals_N_soil, residuals_C_soil, false, out); 
}

void 
Crop::force_production_stress  (double)
{ }

void 
Crop::find_stomata_conductance (const Time& time, 
                                const Bioclimate&,
                                double dt, Treelog&)
{ }

bool
Crop::ds_remove (const Crop* crop)
{ return approximate (crop->DS (), Crop::DSremove); }

double
Crop::stage () const
{ return DS (); }

const VCheck& 
Crop::check_all ()
{
  static const VCheck::Enum all ("all");
  static const VCheck::InLibrary in_library (component);
  static const VCheck::Any any (all, in_library);
  return any;
}

const VCheck& 
Crop::check_library ()
{
  static const VCheck::InLibrary in_library (component);
  return in_library;
}

const VCheck& 
Crop::check_buildable ()
{
  static const VCheck::Buildable buildable (component);
  return buildable;
}

Crop::Crop (const BlockModel& al)
  : ModelFramed (al)
{ }

Crop::~Crop ()
{ }

static struct CropInit : public DeclareComponent 
{
  CropInit ()
    : DeclareComponent (Crop::component, "\
The 'crop' component simulates a specific crop on the field, typically\n\
averaged over one square meter, not individual plants.  Of particular\n\
interest is water and nitrogen uptake at different depths, and the\n\
vertical leaf area distribution, which are used for competition with\n\
other crops.")
  { }
  void load_frame (Frame& frame) const
  { Model::load_model (frame); }
} Crop_init;

// crop.C ends here.
