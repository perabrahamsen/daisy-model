// crop.C

#include "crop_impl.h"

void
Crop::tick (const Wheather& /* wheater */, int /* day */, int /* hour */)
{ 
    cout << "Crop `" << name << "' tick\n"; 
}

Crop::Crop (Log& l, const string n, const ValueList* vl, Column& c)
    : 
      par (*new Parameters (vl)),
      var (*new Variables ()),
      log (l), 
      name (n),
      column (c)
{ }

Crop::~Crop ()
{ 
    delete &par;
    delete &var;
}
