// input.h

#ifndef INPUT_H
#define INPUT_H

#include <std/stdexcept.h>

struct Manager;
struct Weather;
struct Groundwater;
struct Log;
struct ColumnList;
struct Time;
struct ostream;

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
  Time& makeTime () const;
  Manager& makeManager () const;
  Weather& makeWeather () const;
  Groundwater& makeGroundwater () const;
  Log& makeLog () const;
  ColumnList& makeColumns () const;

  // Create.
public:
  Input (int& argc, char**& argv, ostream&);
};

#endif INPUT_H
