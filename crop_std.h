// crop_std.h

#ifndef CROP_STD_H
#define CROP_STD_H

#include "crop.h"

class CropStandard : public Crop
{
  // Content.
public:
  struct Parameters;
  struct Variables;
  const Parameters& par;
  Variables& var;

  // Communication with Bioclimate.
public:
  double height () const;
  double LAI () const;
  const CSMP& LAIvsH () const;
  double PARext () const;
  double PARref () const;
  double EPext () const;
  void CanopyStructure ();
  
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
  void tick (const Time& time, const Column&, const Bioclimate&);
  void output (Log&, const Filter*) const;

  // Create and Destroy.
private:
  friend class CropStandardSyntax;
  static Crop* make (string, const AttributeList&, const AttributeList&);
  CropStandard (string name, 
		const AttributeList& par, const AttributeList& var);
public:
  virtual ~CropStandard ();
};

// Chemical constants affecting the crop.

const double molWeightCH2O = 30.0; // [gCH2O/mol]
const double molWeightCO2 = 44.0; // [gCO2/mol]

#endif CROP_STD_H
