// surface.h

#include "uzmodel.h"

struct AttributeList;

class Surface : public UZtop
{
public:

  // Communication with soil.
  bool flux_top () const;	// From UZtop.
  double q_top () const;
  void flux_top_on () const;
  void flux_top_off () const;
  bool accept_top (double) const;

  void SoilSurfaceConditions (double Theta, double h);

  // Communication with bioclimate.
  double SurfaceEvaporation (double PotSoilEvaporation, 
			     double Water, double Snow);
  Surface (const AttributeList& par, const AttributeList& var);
};

