// input.h

#ifndef INPUT_H
#define INPUT_H

#include "daisy.h"
#include <std/stdexcept.h>

struct Usage : runtime_error
{ 
  const char* what () const;
};

class Input
{
  // Content.
private:
  struct Implementation;
  Implementation& impl;
    
    // Extract.
public:
  const Time& makeTime () const;
  Manager& makeManager () const;
  Weather& makeWeather () const;
  Groundwater& makeGroundwater () const;
  Log& makeLog () const;
  ColumnList& makeColumns () const;
  const Library& makeCrops () const;

  // Create.
public:
  Input (int& argc, char**& argv, ostream&);
};

#endif INPUT_H
