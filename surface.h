// surface.h

#include "uzmodel.h"
#include "inorganic_matter.h"

struct AttributeList;
struct Log;
struct Filter;
struct SoilWater;
struct Soil;

class Surface : public UZtop
{
  const double minimal_matter_flux;
  const double lake;
  double pond;
  bool flux;
  double EvapSoilSurface;
  double Eps;
  double T;
  InorganicMatter im;
  InorganicMatter im_flux;

public:

  // Communication with soil.
  bool flux_top () const;	// From UZtop.
  double q () const;
  void flux_top_on ();
  void flux_top_off ();
  bool accept_top (double);
  double ponding() const;
  double temperature () const;

  const InorganicMatter& matter_flux ();

  void SoilSurfaceConditions (double Theta, double h);

  void clear ();

  // Manager.
  void fertilize (const InorganicMatter&);

  void output (Log&, const Filter&) const;

  // Communication with bioclimate.
  double evaporation (double PotSoilEvaporation, double Water, double temp,
		      const Soil&, const SoilWater&);

  // Create.
  static void load_syntax (Syntax&, AttributeList&);
  Surface (const AttributeList& par);
};

