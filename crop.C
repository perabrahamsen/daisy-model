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
#include "block.h"
#include "mathlib.h"
#include "librarian.h"

const double Crop::DSremove = -5001.0;

const char *const Crop::component = "crop";

double 
Crop::minimum_light_fraction () const
{ return 0.0; }

#if 0
double 
Crop::water_stress () const
{ daisy_notreached (); }

double 
Crop::nitrogen_stress () const
{ daisy_notreached (); }
#endif

double 
Crop::rs_min () const
{ daisy_notreached (); }

double 
Crop::rs_max () const
{ daisy_notreached (); }

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
{ throw name + " is unpluckable"; }

void
Crop::kill (const symbol name, const Time& time, const Geometry& geo,
	    std::vector<AM*>& residuals, double& residuals_DM,
	    double& residuals_N_top, double& residuals_C_top,
	    std::vector<double>& residuals_N_soil, 
            std::vector<double>& residuals_C_soil,
	    Treelog& out)
{ 
#if defined (_MSC_VER)  
  const Harvest& dummy = // MS Visual C++ 2003 sucks.
#endif 
    harvest (name, time, geo, 
             0.0, 0.0, 0.0, 0.0, true, residuals, 
             residuals_DM, residuals_N_top, residuals_C_top, 
             residuals_N_soil, residuals_C_soil, false, out); 
}

void 
Crop::force_production_stress  (double)
{ }

bool
Crop::ds_remove (const Crop* crop)
{ return approximate (crop->DS (), Crop::DSremove); }

Crop::Crop (Block& al)
  : alist (al.alist ()),
    name (al.identifier ("type"))
{ }

Crop::~Crop ()
{ }

static Librarian Crop_init (Crop::component, "\
The 'crop' component simulates a specific crop on the field, typically\n\
averaged over one square meter, not individual plants.  Of particular\n\
interest is water and nitrogen uptake at different depths, and the\n\
vertical leaf area distribution, which are used for competition with\n\
other crops.");

// crop.C ends here.
