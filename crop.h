// crop.h

#ifndef CROP_H
#define CROP_H

#include "daisy.h"

struct AttributeList;

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
    virtual void CanopyStructure (int N, double dH);
    virtual void RootPenetration ();
    virtual double RootDensDistPar (double a);
    virtual void RootDensity ();
    virtual void NitContent ();
    virtual void NitrogenUptake (int Hour);
    virtual double CanopyPhotosynthesis (int No, const double (*RadDist)[30]);
    virtual void AssimilatePartitioning (double DS, 
					 double& f_Leaf, double& f_Stem,
					 double& f_Root, double& f_SOrg);
    virtual double MaintenanceRespiration (double r, double Q10,
					   double w, double T);
    virtual void NetProduction ();

public:
    virtual void tick (const Time& time);
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

#endif CROP_H
