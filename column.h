// column.h

#ifndef COLUMN_H
#define COLUMN_H

#include "daisy.h"
#include <vector.h>

struct AttributeList;
struct Time;

struct ColumnCanopy
{
  const long No;		// No of intervals in canopy discretation.
  vector<double> Height;	// Height in cm of each endpoint in c.d.
  vector<double> PAR;		// PAR of each interval of c.d.
  double LAI;			// Total LAI of all crops on this column.
  ColumnCanopy(int);
};

class Column
{
  // Content.
  struct Implementation;
  Implementation& impl;
  const Bioclimate& bioclimate;
  ColumnCanopy CanStr;
public:
  string name;
  CropList crops;

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
  void IntensityDistribution (double Rad0, double Ext, vector <double>& Rad);
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
