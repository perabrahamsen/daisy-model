// bioclimate.C

#include "bioclimate.h"

Librarian<Bioclimate>::Content* Librarian<Bioclimate>::content = NULL;

double 
Bioclimate::get_evap_interception () const
{ assert (false); return 0.0; }

double 
Bioclimate::get_intercepted_water () const
{ assert (false); return 0.0; }

double 
Bioclimate::get_net_precipitation () const
{ assert (false); return 0.0; }

double 
Bioclimate::get_snow_height () const
{ assert (false); return 0.0; }

Bioclimate::Bioclimate (const string& n)
  : name (n)
{ }

Bioclimate::~Bioclimate ()
{ }
