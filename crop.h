// crop.h

#ifndef CROP_H
#define CROP_H

#include "time.h"
#include "librarian.h"
#include <deque>

struct Log;
struct Filter;
struct Time;
struct AttributeList;
struct Bioclimate;
struct CSMP;
struct Library;
struct Syntax;
struct SoilWater;
struct Soil;
struct Geometry;
struct OrganicMatter;
struct SoilHeat;
struct SoilNH4;
struct SoilNO3;
struct Column;
struct Harvest;

class Crop 
{
  // Content.
public:
  const string name;

  // Communication with Bioclimate.
public:
  virtual double water_stress () const;	// [0-1] (1 = full production)
  virtual double nitrogen_stress () const; // [0-1] (0 = no production)
  virtual double rs_min () const; // Minimum trasnpiration resistance.
  virtual double rs_max () const; // Maximum trasnpiration resistance.
  virtual double height () const = 0;
  virtual double LAI () const = 0;
  virtual const CSMP& LAIvsH () const = 0;
  virtual double PARext () const = 0;
  virtual double PARref () const = 0;
  virtual double EPext () const = 0;
  virtual double IntcpCap () const = 0; // Interception Capacity.
  virtual double EpFac () const = 0; // Convertion to potential evapotransp.
  virtual void CanopyStructure () = 0;
  virtual double ActualWaterUptake (double Ept, const Soil&, SoilWater&, 
				    double EvapInterception) = 0;
  
  // Simulation.
public:
  virtual void tick (const Time& time, const Bioclimate&, const Soil&,
		     OrganicMatter&, const SoilHeat&, const SoilWater&,
		     SoilNH4&, SoilNO3&) = 0;
  virtual const Harvest& harvest (const string& column_name,
				  const Time&, const Geometry&, 
				  OrganicMatter&,
				  double stub_length,
				  double stem_harvest,
				  double leaf_harvest, 
				  double sorg_harvest,
				  bool kill_off) = 0;
  void kill (const string&, const Time&, const Geometry&, OrganicMatter&);
  virtual void output (Log&, Filter&) const = 0;
  
  // Queries.
public:
  static bool ds_remove (const Crop*);
  virtual double DS () const = 0; // Development stage, [-1:2] or DSremove.
  static const double DSremove;
  virtual double DM () const = 0; // Shoot dry matter, [kg/ha].

  // Create and Destroy.
public:
  virtual void initialize (const Geometry&) = 0;
protected:
  Crop (const string& );
public:
  virtual ~Crop ();
};

static Librarian<Crop> Crop_init ("crop");

class CropList : public deque <Crop*> 
{ 
public:
  double CanopySum (double (Crop::*fun) () const) const;
  double LAI () const;
  CropList (const vector<AttributeList*>&);
  ~CropList ();
};

#endif CROP_H
