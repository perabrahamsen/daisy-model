// surface.h

#include "uzmodel.h"

struct AttributeList;
struct Log;
struct Filter;
struct SoilWater;
struct Soil;

class Surface : public UZtop
{
  const double lake;
  double pond;
  bool flux;
  double EvapSoilSurface;
  double Eps;
public:

  // Communication with soil.
  bool flux_top () const;	// From UZtop.
  double q () const;
  void flux_top_on ();
  void flux_top_off ();
  bool accept_top (double);
  double ponding() const;

  void SoilSurfaceConditions (double Theta, double h);

  void output (Log&, const Filter&) const;

  // Communication with bioclimate.
  double evaporation (double PotSoilEvaporation, double Water, 
		      const Soil&, const SoilWater&);

  // Create.
  static void load_syntax (Syntax&, AttributeList&);
  Surface (const AttributeList& par);
};

