// crop.h

#ifndef CROP_H
#define CROP_H

#include <string>
#include <list>
#include <vector>
#include "time.h"

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
struct SoilHeat;
struct SoilNH4;
struct SoilNO3;
struct Column;
struct AM;
struct Harvest;

class Crop 
{
  // Content.
public:
  const string name;

  // Communication with Bioclimate.
public:
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
		     const SoilHeat&, const SoilWater&,
		     SoilNH4&, SoilNO3&) = 0;
  virtual const Harvest& harvest (const Time&, Column&, 
				  double stub_length,
				  double stem_harvest,
				  double leaf_harvest, 
				  double sorg_harvest,
				  double dead_harvest,
				  bool kill_off) = 0;
  void kill (const Time&, Column&);
  virtual void output (Log&, const Filter&) const = 0;
  
  // Queries.
public:
  static bool ds_remove (const Crop*);
protected:
  virtual double DS () const = 0;
  static const double DSremove = -5001.0;

  // Library.
public:
  static const Library& library ();
  typedef Crop* (*constructor) (const AttributeList&, int layers);
  static void add_type (string name, 
			const AttributeList&, const Syntax&,
			constructor);
  static void derive_type (string name, const AttributeList& al, string super);
  static Crop* create (const AttributeList&, int layers = -1);

  // Create and Destroy.
protected:
  Crop (const string);
public:
  virtual ~Crop ();
};

class CropList : public list <Crop*> 
{ 
public:
  CropList (const vector<const AttributeList*>&);
  ~CropList ();
};

// Ensure the Crop library is initialized.
// See TC++PL, 2ed, 10.5.1, for an explanation.
static class Crop_init
{
  static int count;
public:
  Crop_init ();
  ~Crop_init ();
} Crop_init;

#endif CROP_H
