// crop.h

#ifndef CROP_H
#define CROP_H

#include "daisy.h"

struct AttributeList;
struct Bioclimate;
struct CSMP;

class Crop 
{
  // Content.
public:
  const string name;
  struct Parameters;
  struct Variables;
  const Parameters& par;
  Variables& var;

  // Communication with Bioclimate.
public:
  virtual double height ();
  virtual double LAI ();
  virtual const CSMP& LAIvsH ();
  virtual double PARext ();
  virtual double PARref ();
  virtual double EPext ();
  virtual void CanopyStructure ();
  
  // Internal functions.
protected:
  virtual double SoluteUptake (string /* SoluteID */, double /* PotNUpt */,
			       double /* I_Mx */, double /* Rad */);
  virtual double H2OUptake (double PotTransp,
			    double /* RootRad */, double /* h_wp */);
public:				// Used by external development models.
  virtual void Vernalization (double Ta);
protected:
  virtual void Emergence (const Column& column);
  virtual void DevelopmentStage (const Bioclimate&);
  virtual double CropHeight ();
  virtual void InitialLAI ();
  virtual double CropLAI ();
  virtual void RootPenetration (const Column& column);
  virtual double RootDensDistPar (double a);
  virtual void RootDensity ();
  virtual void NitContent ();
  virtual void NitrogenUptake (int Hour);
  // Sugar production [gCH2O/m2/h] by canopy photosynthesis.
  virtual double CanopyPhotosynthesis (const Bioclimate&);
  virtual void AssimilatePartitioning (double DS, 
				       double& f_Leaf, double& f_Stem,
				       double& f_Root, double& f_SOrg);
  virtual double MaintenanceRespiration (double r, double Q10,
					 double w, double T);
  virtual void NetProduction (const Column&, const Bioclimate&);

  // Simulation.
public:
  virtual void tick (const Time& time, const Column&, const Bioclimate&);
  virtual void output (Log&, const Filter*) const;

  // Create and Destroy.
public:
  Crop (const string, const AttributeList& pl);
  Crop (const string, 
	const AttributeList& pl, const AttributeList& vl);
  virtual ~Crop ();
};

// Chemical constants affecting the crop.

const double molWeightCH2O = 30.0; // [gCH2O/mol]
const double molWeightCO2 = 44.0; // [gCO2/mol]

#endif CROP_H
