// crop.h

#ifndef CROP_H
#define CROP_H

#include "daisy.h"

struct AttributeList;
struct ColumnCanopy;

class Crop 
{
  // Content.
public:
  const string name;
  struct Parameters;
  struct Variables;
  const Parameters& par;
  Variables& var;
  const Bioclimate& bioclimate;
  const Column* column;

  // Simulation.
public:
  virtual double SoluteUptake (string /* SoluteID */, double /* PotNUpt */,
			       double /* I_Mx */, double /* Rad */);
  virtual double H2OUptake (double PotTransp,
			    double /* RootRad */, double /* h_wp */);
  virtual void Vernalization (double Ta);
  virtual void Emergence ();
  virtual void DevelopmentStage ();
  virtual double CropHeight ();
  virtual void InitialLAI ();
  virtual double CropLAI ();
  virtual void CanopyStructure ();
  virtual void RootPenetration ();
  virtual double RootDensDistPar (double a);
  virtual void RootDensity ();
  virtual void NitContent ();
  virtual void NitrogenUptake (int Hour);
  // Sugar production [gCH2O/m2/h] by canopy photosynthesis.
  virtual double CanopyPhotosynthesis (const ColumnCanopy& CanStr);
  virtual void AssimilatePartitioning (double DS, 
				       double& f_Leaf, double& f_Stem,
				       double& f_Root, double& f_SOrg);
  virtual double MaintenanceRespiration (double r, double Q10,
					 double w, double T);
  virtual void NetProduction ();

public:
  virtual void tick (const Time& time, const ColumnCanopy& CanStr);
  virtual void output (Log&, const Filter*) const;

  // Create and Destroy.
public:
  void set_column (const Column*);
  Crop (const string, const Bioclimate&, const Column*,
	const AttributeList& pl);
  Crop (const string, const Bioclimate&, const Column*,
	const AttributeList& pl, const AttributeList& vl);
  virtual ~Crop ();
};

// Chemical constants affecting the crop.

const double molWeightCH2O = 30.0; // [gCH2O/mol]
const double molWeightCO2 = 44.0; // [gCO2/mol]

#endif CROP_H
