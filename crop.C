// crop.C

#include "daisy.h"

struct Crop::Implementation
{ };

Crop::Crop (Log& l, Column& c)
    : impl (*new Implementation ()),
      log (l), 
      column (c)
{ }

Crop::~Crop ()
{ }

