// crop.h

#ifndef CROP_H
#define CROP_H

#include <std/string.h>
#include <list.h>

struct Log;
struct Filter;
struct Time;
struct AttributeList;
struct Sequence;
struct Bioclimate;
struct CSMP;
struct Library;
struct Syntax;
struct SoilWater;
struct Soil;
struct SoilHeat;

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
  virtual void tick (const Time& time, const Bioclimate&, const Soil&, const SoilHeat&) = 0;
  virtual void output (Log&, const Filter*) const = 0;

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
  CropList (const Sequence&);
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
