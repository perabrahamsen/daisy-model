// surface.h

#include "uzmodel.h"

struct AttributeList;

class Surface : public UZtop
{
public:

  // Communication with soil.
  bool flux_top () const;	// From UZtop.
  double q_top () const;	// From UZtop.
  void SoilSurfaceConditions (double Theta, double h);

  // Communication with bioclimate.
  double SurfaceEvaporation (double PotSoilEvaporation, 
			     double Water, double Snow);
  Surface (const AttributeList& par, const AttributeList& var);
};

