// surface.h

#include "uzmodel.h"

struct AttributeList;

class Surface : public UZtop
{
public:

  // Communication with soil.
  bool flux_top () const;	// From UZtop.
  double q () const;
  void flux_top_on () const;
  void flux_top_off () const;
  bool accept_top (double) const;
  double ponding() const;

  void SoilSurfaceConditions (double Theta, double h);

  // Communication with bioclimate.
  double evaporation (double PotSoilEvaporation, double Water);

  // Create.
  static void load_syntax (Syntax&, AttributeList&);
  Surface (const AttributeList& par);
};

