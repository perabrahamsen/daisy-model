// daisy.h

#ifndef DAISY_H
#define DAISY_H

#include <std/typeinfo.h>
#include <std/string.h>
#include <list.h>

class Column;
class Crop;
class Daisy;
class Manager;
class Wheather;
class Log;
class Input;
class Library;

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
    Manager& manager;
    Wheather& wheather;
    ColumnList& columns;
    const Library& crops;

    // Simulation.
public:
    void run();

    // Create and Destroy.
public:
    Daisy (const Input&);
    ~Daisy ();
};

#endif DAISY_H

