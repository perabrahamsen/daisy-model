// crop.h

#ifndef CROP_H
#define CROP_H

#include "time.h"
#include "librarian.h"
#include <vector>

struct Log;
struct Time;
struct AttributeList;
struct Bioclimate;
struct PLF;
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
struct AM;

class Crop 
{
  // Content.
public:
  const AttributeList alist;	// Remember attributes for checkpoint.
  const string name;
  static const char *const description;

  // Communication with Bioclimate.
public:
  virtual double water_stress () const;	// [0-1] (0 = full production)
  virtual double nitrogen_stress () const; // [0-1] (1 = no production)
  virtual double rs_min () const; // Minimum transpiration resistance.
  virtual double rs_max () const; // Maximum transpiration resistance.
  virtual double height () const = 0;
  virtual double LAI () const = 0;
  virtual const PLF& LAIvsH () const = 0;
  virtual double PARext () const = 0;
  virtual double PARref () const = 0;
  virtual double EPext () const = 0;
  virtual double IntcpCap () const = 0; // Interception Capacity.
  virtual double EpFac () const = 0; // Convertion to potential evapotransp.
  virtual double albedo () const;
  virtual void CanopyStructure () = 0;
  virtual double ActualWaterUptake (double Ept, const Soil&, SoilWater&, 
				    double EvapInterception) = 0;
  virtual void force_production_stress  (double pstress);

  // Simulation.
public:
  virtual void tick (const Time& time, const Bioclimate&, const Soil&,
		     OrganicMatter*, const SoilHeat&, const SoilWater&,
		     SoilNH4*, SoilNO3*) = 0;
  virtual const Harvest& harvest (const string& column_name,
				  const Time&, const Geometry&, 
				  Bioclimate& bioclimate,
				  double stub_length,
				  double stem_harvest,
				  double leaf_harvest, 
				  double sorg_harvest,
				  bool kill_off,
				  vector<AM*>& residuals) = 0;
  void kill (const string&, const Time&, const Geometry&, Bioclimate&,
	     vector<AM*>&);
  virtual void output (Log&) const = 0;
  
  // Queries.
public:
  static bool ds_remove (const Crop*);
  virtual double DS () const = 0; // Development stage, [-1:2] or DSremove.
  static const double DSremove;
  virtual double DM () const = 0; // Shoot dry matter, [kg/ha].

  // Create and Destroy.
public:
  virtual void initialize (const Geometry&, const OrganicMatter&);
  virtual void initialize (const Geometry&) = 0;
protected:
  Crop (const AttributeList& al);
public:
  virtual ~Crop ();
};

static Librarian<Crop> Crop_init ("crop");

#endif // CROP_H
