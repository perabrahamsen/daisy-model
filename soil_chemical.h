// soil_chemical.h

#ifndef SOIL_CHEMICAL_H
#define SOIL_CHEMICAL_H

#include "solute.h"
#include "csmp.h"
#include <vector>

struct Chemical;
struct Soil;
struct SoilWater;
struct SoilHeat;
struct OrganicMatter;
struct Syntax;
struct AttributeList;

class SoilChemical : public Solute
{
  // Content.
public:
  const Chemical& chemical;
private:
  vector<double> decomposed;
  vector<double> uptaken;
  vector<double> lag;
  static CSMP* no_lag;
  const CSMP& lag_increment;

  // Simulation.
public:
  void uptake (const Soil&, const SoilWater&);
  void decompose (const Soil&, const SoilWater&, const SoilHeat&, 
		  const OrganicMatter&);
  void output (Log&) const;
  
public:
  // Substance specific constants.
  double diffusion_coefficient () const; // in free solu. [cm² / h]

  // Create & Destroy.
public:
  static void load_syntax (Syntax&, AttributeList&);
  void initialize (const AttributeList&, const Soil&, const SoilWater&);
  SoilChemical (const Chemical&, const AttributeList&);	// From parser.
  SoilChemical (const Chemical&); // From influx.
private:
  SoilChemical (const SoilChemical&);
};

#endif SOIL_CHEMICAL_H
