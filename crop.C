 // crop.C

#include "crop.h"
#include "chemicals.h"
#include "om.h"

const double Crop::DSremove = -5001.0;

Librarian<Crop>::Content* Librarian<Crop>::content = NULL;

const char *const Crop::description = "\
The `crop' component simulates a specific crop on the field, typically\n\
averaged over one square meter, not individual plants.  Of particular\n\
interest is water and nitrogen uptake at different depths, and the\n\
vertical leaf area distribution, which are used for competition with\n\
other crops.";

double 
Crop::water_stress () const
{ assert (false); return -42.42e42; }

double 
Crop::nitrogen_stress () const
{ assert (false); return -42.42e42; }

double 
Crop::rs_min () const
{ assert (false); return -42.42e42; }

double 
Crop::rs_max () const
{ assert (false); return -42.42e42; }

double 
Crop::albedo () const
{ return 0.20; }

void
Crop::kill (const string& name, const Time& time, const Geometry& geometry,
	    Bioclimate& bioclimate, vector<AM*>& residuals)
{ 
  harvest (name, time, geometry, bioclimate,
	   0.0, 0.0, 0.0, 0.0, true, residuals); 
}

void 
Crop::force_production_stress  (double)
{ }

bool
Crop::ds_remove (const Crop* crop)
{ return crop->DS () == Crop::DSremove; }

void
Crop::initialize (const Geometry& geometry, const OrganicMatter&)
{ initialize (geometry); }

Crop::Crop (const AttributeList& al)
  : alist (al),
    name (al.name ("type"))
{ }

Crop::~Crop ()
{ }
