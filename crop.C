 // crop.C

#include "crop.h"

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

void
Crop::kill (const string& name, const Time& time, const Geometry& geometry,
	    OrganicMatter& organic_matter)
{ harvest (name, time, geometry, organic_matter, 0.0, 0.0, 0.0, 0.0, true); }

bool
Crop::ds_remove (const Crop* crop)
{ return crop->DS () == Crop::DSremove; }

Crop::Crop (const string& n)
  : name (n)
{ }

Crop::~Crop ()
{ }

double CropList::CanopySum (double (Crop::*fun) () const) const
{
  double value = 0.0;

  for (const_iterator crop = begin(); crop != end(); crop++)
    {
      value += ((*crop)->*fun) () * (*crop)->LAI ();
    }
  return value;
}

double CropList::LAI () const
{
  double value = 0.0;

  for (const_iterator crop = begin(); crop != end(); crop++)
    {
      value += (*crop)->LAI ();
    }
  return value;
}

CropList::CropList (const vector<AttributeList*>& sequence)
{
  for (vector<AttributeList*>::const_iterator i = sequence.begin ();
       i != sequence.end ();
       i++)
    push_back (&Librarian<Crop>::create (**i));
}

CropList::~CropList ()
{
  // Borland C++ don't want a const iterator here.
  for (iterator i = begin (); i != end (); i++)
    delete *i;
}
