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

