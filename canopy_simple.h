// canopy_simple.h -- Canopy development for simple crop model.

#include "plf.h"

struct Log;
struct AttributeList;
struct Syntax;

class CanopySimple
{
  // Paramaters.
public:
  const double PARref;		// PAR reflectance
  const double PARext;		// PAR extinction coefficient
  const double EPext;		// EP extinction coefficient
  const double IntcpCap;	// 
  const double EpFac;		// 
  const double rs_max;		// max transpiration resistance
  const double rs_min;		// min transpiration resistance

  // Variables.
public:
  double Height;		// Crop height [cm]
  double CAI;	        	// Crop Area Index
  PLF LAIvsH;			// Accumulated Crop Area Index at Height

  // Simulation.
public:
  void output (Log&) const;

  // Create and Destroy.
public:
  static void load_syntax (Syntax& syntax, AttributeList& alist);
  CanopySimple (const AttributeList&);
  ~CanopySimple ();
};
