// column.h

#ifndef COLUMN_H
#define COLUMN_H

#include "daisy.h"

struct AttributeList;

class Column
{
    // Content.
    struct Implementation;
    Implementation& impl;
#ifdef COLUMN_INTERNALS
    struct RecCanStr
    {
	vector<double> CanLAD;
	vector<double> CanPAR;
	vector<double> CanPTr;
    } CanStr;
#endif COLUMN_INTERNALS
public:
    string name;
    CropList crops;

    // Simulation.
public:
    void tick (Column* left, Column* rigth, const Wheather& wheater, 
	       int day, int hour);
    void sow (const Library& croplib, string crop, Log&);

    void output (Log&, const Filter*) const;
    void output_crops (Log&, const Filter*) const;

    // Create and Destroy.
public:
    Column (string name, 
	    const AttributeList& paramenters, const AttributeList& variables, 
	    const Library& horizons);
    ~Column ();
};

#endif COLUMN_H
