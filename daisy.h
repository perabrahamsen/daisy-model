// daisy.h

#ifndef DAISY_H
#define DAISY_H

#include "time.h"

#include <std/typeinfo.h>
#include <std/string.h>
#include <list.h>

class Column;
class Crop;
class Daisy;
class Manager;
class Weather;
class Groundwater;
class Log;
class Input;
class Library;
class Condition;
class Filter;

typedef list <Column*> ColumnList;
typedef list <Crop*> CropList;

#ifdef HANDLE_EXCEPTIONS
#define THROW(x) throw x
#define throw2(x, y) throw (x, y)
#define throw0() throw ()
#else HANDLE_EXCEPTIONS
#define THROW(x) assert ("error" == #x)
#define throw(x)
#define throw2(x, y)
#define throw0()
#endif  HANDLE_EXCEPTIONS

#if 0
#define BUG_DYNAMIC_CAST(T, V) dynamic_cast<T> (V)
#else
#define BUG_DYNAMIC_CAST(T, V) (T) V
#endif

class Daisy
{
    // Content.
    struct Implementation;
    Implementation& impl;
public:
    Log& log;
    Time time;
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

