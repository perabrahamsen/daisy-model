// column.h

#ifndef COLUMN_H
#define COLUMN_H

#include "daisy.h"

struct AttributeList;
struct Time;

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

    // Run.
public:
    virtual void tick (Column* left, Column* rigth, const Bioclimate& wheater, 
		       const Time&);
    void sow (const Library& croplib, string crop, Log&);

    virtual void output (Log&, const Filter*) const;
    void output_crops (Log&, const Filter*) const;
    
#if 0
    // Simulation.
    virtual double PotentialTranspiration (const Bioclimate&);
    virtual double SoilTemperature (double depth);
    virtual double MaxRootingDepth ();
    virtual double EvapInterception ();
#endif

    // Create and Destroy.
public:
    Column (string name, 
	    const AttributeList& paramenters, const AttributeList& variables, 
	    const Library& horizons);
    virtual ~Column ();
};

#endif COLUMN_H
