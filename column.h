// column.h

#ifndef COLUMN_H
#define COLUMN_H

#include "daisy.h"

struct ValueList;

class Column
{
    // Content.
    struct Implementation;
    Implementation& impl;
    Log& log;
public:
    string name;
    CropList crops;

    // Simulation.
public:
    void tick (Column* left, Column* rigth, const Wheather& wheater, 
	       int day, int hour);
    void sow (const Library& croplib, string crop);

    // Create and Destroy.
public:
    Column (Log&, string, const ValueList*, const Library&);
    ~Column ();
};

#endif COLUMN_H
