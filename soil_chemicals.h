// soil_chemicals.h

#ifndef SOIL_CHEMICALS_H
#define SOIL_CHEMICALS_H

struct Soil;
struct SoilWater;
struct SoilHeat;
struct OrganicMatter;
struct Chemicals;
struct Log;
struct Syntax;
struct AttributeList;

class SoilChemicals
{
  // Content.
private:
  struct Implementation;
  Implementation& impl;

  // Simulation.
public:
  void tick (const Soil&, const SoilWater&, const SoilHeat&, 
	     const OrganicMatter*, const Chemicals& flux_in);
  void mixture (Chemicals& storage, // [g/m^2]
		Chemicals& up,	// [g/m^2/h]
		double pond,	// [mm]
		double rate) const;	// [h/mm]
  void output (Log&) const;
  void mix (const Soil&, const SoilWater&, double from, double to);
  void swap (const Soil&, const SoilWater&, double from, double middle, double to);

  // Create & Destroy.
public:
  void clear ();
  void initialize (const AttributeList&, const Soil&, const SoilWater&);
  bool check (unsigned n) const;
  static void load_syntax (Syntax&, AttributeList&);
  SoilChemicals (const AttributeList&);
  ~SoilChemicals ();
};

#endif SOIL_CHEMICALS_H
