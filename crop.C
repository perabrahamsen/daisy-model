 // crop.C

#include "crop.h"
#include "chemicals.h"
#include "om.h"
// #include "harvest.h"

const double Crop::DSremove = -5001.0;

EMPTY_TEMPLATE
Librarian<Crop>::Content* Librarian<Crop>::content = NULL;

const char *const Crop::description = "\
The 'crop' component simulates a specific crop on the field, typically\n\
averaged over one square meter, not individual plants.  Of particular\n\
interest is water and nitrogen uptake at different depths, and the\n\
vertical leaf area distribution, which are used for competition with\n\
other crops.";

double 
Crop::minimum_light_fraction () const
{ return 0.0; }

#if 0
double 
Crop::water_stress () const
{ daisy_assert (false); }

double 
Crop::nitrogen_stress () const
{ daisy_assert (false); }
#endif

double 
Crop::rs_min () const
{ daisy_assert (false); }

double 
Crop::rs_max () const
{ daisy_assert (false); }

double 
Crop::SimLAI () const
{ return LAI (); }

double 
Crop::albedo () const
{ return 0.20; }

void
Crop::kill (const symbol name, const Time& time, const Geometry& geometry,
	    Bioclimate& bioclimate, vector<AM*>& residuals,
	    double& residuals_DM,
	    double& residuals_N_top, double& residuals_C_top,
	    vector<double>& residuals_N_soil, vector<double>& residuals_C_soil,
	    Treelog& out)
{ 
#if defined (_MSC_VER)  
  const Harvest& dummy = // MS Visual C++ 2003 sucks.
#endif 
    harvest (name, time, geometry, bioclimate,
             0.0, 0.0, 0.0, 0.0, true, residuals, 
             residuals_DM, residuals_N_top, residuals_C_top, 
             residuals_N_soil, residuals_C_soil, out); 
}

void 
Crop::force_production_stress  (double)
{ }

bool
Crop::ds_remove (const Crop* crop)
{ return crop->DS () == Crop::DSremove; }

void
Crop::initialize_organic (Treelog& msg, const Geometry& geometry, 
			  OrganicMatter&)
{ initialize_inorganic (msg, geometry); }

Crop::Crop (const AttributeList& al)
  : alist (al),
    name (al.identifier ("type"))
{ }

Crop::~Crop ()
{ }
