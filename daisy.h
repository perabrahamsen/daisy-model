// daisy.h

#ifndef DAISY_H
#define DAISY_H

#include "time.h"

class Manager;
class Weather;
class Groundwater;
class Log;
class Input;
class Condition;
class Filter;
class ColumnList;

class Daisy
{
    // Content.
    struct Implementation;
    Implementation& impl;
public:
    Log& log;
    Time& time;
    Manager& manager;
    Weather& weather;
    Groundwater& groundwater;
    ColumnList& columns;

    // Simulation.
public:
    void run();
    bool match (const Condition*) const;
    void output (Log&, const Filter*) const;
    void output_field (Log&, const Filter*) const;

    // Create and Destroy.
public:
    Daisy (const Input&);
    ~Daisy ();
};

#endif DAISY_H

