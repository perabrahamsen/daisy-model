 // crop.C

#include "crop.h"
#include "chemicals.h"

const double Crop::DSremove = -5001.0;

Librarian<Crop>::Content* Librarian<Crop>::content = NULL;

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
	    OrganicMatter& organic_matter, Bioclimate& bioclimate)
{ 
  Chemicals chemicals;
  harvest (name, time, geometry, organic_matter, bioclimate,
	   0.0, 0.0, 0.0, 0.0, true); 
}

bool
Crop::ds_remove (const Crop* crop)
{ return crop->DS () == Crop::DSremove; }

Crop::Crop (const AttributeList& al)
  : alist (al),
    name (al.name ("type"))
{ }

Crop::~Crop ()
{ }
