// crop.h

#ifndef CROP_H
#define CROP_H

#include <std/string.h>

struct Log;
struct Filter;
struct Time;
struct Column;
struct AttributeList;
struct Bioclimate;
struct CSMP;
struct Library;
struct Syntax;

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
  virtual void CanopyStructure () = 0;
  
  // Simulation.
public:
  virtual void tick (const Time& time, const Column&, const Bioclimate&) = 0;
  virtual void output (Log&, const Filter*) const = 0;

  // Library.
public:
  static const Library& par_library ();
  static const Library& var_library ();
  typedef Crop* (*constructor) (string name, 
				  const AttributeList& par,
				  const AttributeList& var);
  static void add_type (string name, 
			const AttributeList& parList, const Syntax& parSyntax,
			const AttributeList& varList, const Syntax& varSyntax,
			constructor);
  static void derive_type (string name, const AttributeList& par, string super);
  static Crop* create (string, const AttributeList& var);

  // Create and Destroy.
protected:
  Crop (const string);
public:
  virtual ~Crop ();
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
