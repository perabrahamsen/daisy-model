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
    const Bioclimate& bioclimate;
public:
    string name;
    CropList crops;
    struct RecCanStr
    {
	long NoCan;
	double CanLAD[2][30];
	double CanPAR[2][30];
	double CanPTr[2][30];
    } CanStr;

    // Actions.
public:
    void sow (const Library& croplib, string crop, Log&);

    virtual void output (Log&, const Filter*) const;
    void output_crops (Log&, const Filter*) const;
    
    // Simulation.
    virtual double PotentialTranspiration (const Bioclimate&) const;
    virtual double SoilTemperature (double depth) const;
    virtual double MaxRootingDepth () const;
    virtual double EvapInterception () const;
    virtual void SoilColumnDiscretization (long Nz, double *z_b,
					   double *z_n, double *dz) const;
    static void IntensityDistribution (double Rad, double Ext, long NoL,
				       double (*LAD)[30],
				       double (*RadDist)[30]);
    virtual void MainCropGrowthModel ();
    virtual void tick (const Time&);

    // Create and Destroy.
public:
    Column (string name, const Bioclimate&, 
	    const AttributeList& par, const AttributeList& var,
	    const Library&);
    virtual ~Column ();
};

#endif COLUMN_H
