// canopy_simple.C -- Canopy development for simple crop model.

#include "canopy_simple.h"
#include "submodel.h"
#include "log.h"

void
CanopySimple::output (Log& log) const
{
  log.output ("Height", Height);
  log.output ("CAI", CAI);
  log.output ("LAIvsH", LAIvsH);
}

void 
CanopySimple::load_syntax (Syntax& syntax, AttributeList& alist)
{
  static const PLF empty_plf;

  // Parameters.
  syntax.add ("PARref", Syntax::None (), Syntax::Const,
	      "PAR reflectance.");
  alist.add ("PARref", 0.06);
  syntax.add ("PARext", Syntax::None (), Syntax::Const,
	      "PAR extinction coefficient.");
  alist.add ("PARext", 0.60);
  syntax.add ("EPext", Syntax::None (), Syntax::Const,
	      "EP extinction coefficient.");
  alist.add ("EPext", 0.5);
  syntax.add ("IntcpCap", "mm", Syntax::Const,
	      "Interception capacity.");
  alist.add ("IntcpCap", 0.5);
  syntax.add ("EpFac", Syntax::None (), Syntax::Const,
	      "Potential evapotranspiration factor.");
  alist.add ("EpFac", 1.0);
  syntax.add ("rs_max", "s/m", Syntax::Const,
	      "Maximum transpiration resistance.");
  alist.add ("rs_max", 1.0e5);
  syntax.add ("rs_min", "s/m", Syntax::Const,
	      "Minimum transpiration resistance.");
  alist.add ("rs_min", 30.0);

  // Variables.
  syntax.add ("Height", "cm", Syntax::State, "Crop height.");
  alist.add ("Height", 0.0);
  syntax.add ("CAI", "m^2/m^2", Syntax::State, "Crop Area Index.");
  alist.add ("CAI", 0.0);
  syntax.add ("LAIvsH", "cm", "m^2/m^2", Syntax::State,
	      "Accumulated Leaf Area Index at Height.");
  alist.add ("LAIvsH", empty_plf);
}

CanopySimple::CanopySimple (const AttributeList& vl)
  : PARref (vl.number ("PARref")),
    PARext (vl.number ("PARext")),
    EPext (vl.number ("EPext")),
    IntcpCap (vl.number ("IntcpCap")),
    EpFac (vl.number ("EpFac")),
    rs_max (vl.number ("rs_max")),
    rs_min (vl.number ("rs_min")),
    Height (vl.number ("Height")),
    CAI    (vl.number ("CAI")),
    LAIvsH (vl.plf ("LAIvsH"))
{ }

CanopySimple::~CanopySimple ()
{ }

static Submodel::Register 
soil_submodel ("CanopySimple", CanopySimple::load_syntax);
