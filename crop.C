 // crop.C

#include "crop.h"
#include "chemicals.h"
#include "om.h"
#include "block.h"
#include "mathlib.h"

const double Crop::DSremove = -5001.0;

template<>
BuildBase* Librarian<Crop>::content = NULL;

const char *const Crop::description = "\
The 'crop' component simulates a specific crop on the field, typically\n\
averaged over one square meter, not individual plants.  Of particular\n\
interest is water and nitrogen uptake at different depths, and the\n\
vertical leaf area distribution, which are used for competition with\n\
other crops.";

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

void
Crop::kill (const symbol name, const Time& time, const Geometry& geo,
	    Bioclimate& bioclimate, std::vector<AM*>& residuals,
	    double& residuals_DM,
	    double& residuals_N_top, double& residuals_C_top,
	    std::vector<double>& residuals_N_soil, 
            std::vector<double>& residuals_C_soil,
	    Treelog& out)
{ 
#if defined (_MSC_VER)  
  const Harvest& dummy = // MS Visual C++ 2003 sucks.
#endif 
    harvest (name, time, geo, bioclimate,
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
