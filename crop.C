// crop.C

#include "crop_impl.h"
#include "log.h"

void
Crop::tick (const Wheather& /* wheater */, int /* day */, int /* hour */)
{ 
    cout << "Crop `" << name << "' tick\n"; 
}

void
Crop::output (Log& log, const Filter* filter) const
{
    log.open (name);
    var.output (log, filter);
    log.close ();
}

Crop::Crop (const string n, const AttributeList& pl)
    : 
      par (Parameters::get (n, pl)),
      var (*new Variables (par)),
      name (n)
{ }

Crop::Crop (const string n, const AttributeList& pl, const AttributeList& vl)
    : 
      par (Parameters::get (n, pl)),
      var (*new Variables (vl)),
      name (n)
{ }

Crop::~Crop ()
{ 
    delete &par;
    delete &var;
}
