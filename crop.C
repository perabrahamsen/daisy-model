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
	    OrganicMatter& organic_matter, Bioclimate& bioclimate)
{ 
  Chemicals chemicals;
  harvest (name, time, geometry, organic_matter, bioclimate,
	   0.0, 0.0, 0.0, 0.0, true); 
}

bool
Crop::ds_remove (const Crop* crop)
{ return crop->DS () == Crop::DSremove; }

const vector<AttributeList*>&
Crop::default_AOM ()
{
  static vector<AttributeList*>* AOM = NULL;

  if (!AOM)
    {
      Syntax om_syntax;
      AttributeList om_alist;
      OM::load_syntax (om_syntax, om_alist);
      AttributeList& AOM1 = *new AttributeList (om_alist);
      AttributeList& AOM2 = *new AttributeList (om_alist);
      AOM1.add ("initial_fraction", 0.80);
      vector<double> CN;
      CN.push_back (90.0);
      AOM1.add ("C_per_N", CN);
      vector<double> efficiency1;
      efficiency1.push_back (0.50);
      efficiency1.push_back (0.50);
      AOM1.add ("efficiency", efficiency1);
      AOM1.add ("turnover_rate", 2.0e-4);
      vector<double> fractions1;
      fractions1.push_back (0.50);
      fractions1.push_back (0.50);
      fractions1.push_back (0.00);
      AOM1.add ("fractions", fractions1);
      vector<double> efficiency2;
      efficiency2.push_back (0.50);
      efficiency2.push_back (0.50);
      AOM2.add ("efficiency", efficiency2);
      AOM2.add ("turnover_rate", 2.0e-3);
      vector<double> fractions2;
      fractions2.push_back (0.00);
      fractions2.push_back (1.00);
      fractions2.push_back (0.00);
      AOM2.add ("fractions", fractions2);
      AOM = new vector<AttributeList*>;
      AOM->push_back (&AOM1);
      AOM->push_back (&AOM2);
    }
  return *AOM;
}


Crop::Crop (const AttributeList& al)
  : alist (al),
    name (al.name ("type"))
{ }

Crop::~Crop ()
{ }
