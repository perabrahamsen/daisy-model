// canopy_std.C -- Canopy development for standard crop model.

#include "canopy_std.h"
#include "submodel.h"
#include "log.h"
#include "mathlib.h"

double
CanopyStandard::CropHeight (double WStem, double DS)
{
#if 0
  const double H1 = HvsDS (DS) + Offset;
  const double H2 = HvsDS (1.0) * HvsWStem (WStem);
  return min (H1, H2);
#else
  return HvsDS (DS) + Offset;
#endif
}

void
CanopyStandard::InitialCAI (double WLeaf, double DS)
{
  double  CAI2;

//  if (WLeaf >= Canopy.WLfInit)
//    {
//      CAI = Canopy.SpCAI * WLeaf;
//      InitCAI = false;
//    }
//  else
//    {
//#if 0
//      if (DS > Canopy.DSinit)
//	DS = Canopy.DSinit;
//      CAI = 0.5 * (exp (Canopy.InitGrowth * DS) - 1);
//#else
//      CAI = 0.5 * (exp (Canopy.InitGrowth * min (DS, Canopy.DSinit)) - 1);
//#endif
//    }
  const double CAI1 = SpLAI * WLeaf;
  if (DS<0.07)
    {
      CAI2 = 10.;
    }
  else
    {
     CAI2 = max( 0.01, SpLAIfac * SpLAI * WLeaf);
    }
  const double CAI3 = min ( CAI2, 1.0/(1.0+exp(-15.0*(DS-DSLAI05))));
  if (CAI1 >= CAI3)
    {
      CAI = CAI1;
      InitCAI = false;
    }
  else
    CAI = CAI3;
  LeafAI = CAI;
  StemAI = 0.0;
  SOrgAI = 0.0;
}

void
CanopyStandard::CropCAI (double WLeaf, double WSOrg, double WStem, double DS)
{
  LeafAI = SpLAI    * LeafAIMod (DS) * WLeaf;
  SOrgAI = SpSOrgAI * SOrgAIMod (DS) * WSOrg;
  StemAI = SpStemAI * StemAIMod (DS) * WStem;

  CAI = LeafAI + StemPhotEff * StemAI + SOrgPhotEff * SOrgAI;
}

void
CanopyStandard::CanopyStructure (double DS)
{
  // The leaf density is assumed to from the group up to height z0,
  // then increase linearly until height z1, continue at that
  // density until z2, and then decrease linearly until the top of
  // the crop.  The values of z1, z2, and z3 are scaled so the
  // ground height is zero and the top of the crop is 1.

  double z0;			// Height of first leaf [0 - 1].
  double z1;			// Min height of the MaxLAD area [0 - 1].
  double z2;			// Max height of the MaxLAD area [0 - 1].
  double Area;		        // Area spanned by z0, z1, and z2.

  assert (DS > 0.0);
  if (DS <= 1)
    {
      // LAIDist0 is the leaf density at DS = 0, and LAIDist 1
      // is the leaf density at DS = 1.  The leaf density is
      // assumed to develop linearly as a function of DS between
      // DS 0 and DS 1.

      z0 = LAIDist0[0] + (LAIDist1[0] - LAIDist0[0]) * DS;
      z1 = LAIDist0[1] + (LAIDist1[1] - LAIDist0[1]) * DS;
      z2 = LAIDist0[2] + (LAIDist1[2] - LAIDist0[2]) * DS;
      Area = (1.0 + z2 - z1 - z0) / 2.0;
      assert (Area > 0.0);
      assert (Height > 0.0);
      LADm = CAI / (Area * Height);
    }
  else
    {
      assert (DS <= 2);

      z0 = LAIDist1[0];
      z1 = LAIDist1[1];
      z2 = LAIDist1[2];
      double Area = (1.0 + z2 - z1 - z0) / 2.0;
      double MaxLAD = CAI / (Area * Height);

      if (MaxLAD > LADm)
	// After DS = 1 CAI may increase for some time, keeping
	// z0, z1, and z2 constant but adding to MaxLAD.
	LADm = MaxLAD;
      else
	{
	  // Then the crop start eating itself.  This is done by
	  // keeping MaxLAD, (z1 - z0), and z2 constant but
	  // increasing z1 and z0.

	  // Need is the Area we want after moving z1 and z0.
	  double Need = CAI / Height / LADm;

	  if (approximate (Area, Need))
	    Need = Area;

	  assert (Need <= Area);

	  if (Area - Need > z2 - z1)
	    // We have to move z1 beyond z2.
	    {
	      // Let x0 be the height where the canopy starts, x1 the
	      // height where the canopy top, and y1 the canopy at x1.
	      //
	      // We know the area of the triangle:
	      // (1):  Need = (1 - x0) * y1 / 2
	      //
	      // We assume that the slope of the canopy increase and
	      // descrease is unchanged:
	      // (2):   (1 - x1) / y1 =  (1 - z2) / 1
	      // (3):  (x1 - x0) / y1 = (z1 - z0) / 1
	      //
	      // This gives us three equations with three unknown.
	      // We can solve them to get x0, x1, and y1.

	      double x0 = 1.0 - sqrt (2.0 * Need * (z1 - z2 - z0 + 1.0));
	      double x1
		= 1.0 + (z2 - 1) * sqrt (2.0 * Need / (z1 - z2 - z0 + 1.0));
	      double y1 = sqrt (2.0 * Need / (z1 - z2 - z0 + 1.0));

	      // Check the results.
	      assert (approximate (Need, (1.0 - x0) * y1 / 2.0));
	      assert (approximate ((1.0 - x1) / y1, (1.0 - z2)));
	      assert (approximate ((x1 - x0) / y1, (z1 - z0)));

	      // Insert this special distribution, and return.
	      PLF LADvsH;
	      LADvsH.add (x0 * Height, 0.0);
	      LADvsH.add (x1 * Height, y1 * LADm);
	      LADvsH.add (     Height, 0.0);
	      LAIvsH = LADvsH.integrate_stupidly ();
	      return;
	    }
	  // It is enough to z1 closer to z2.
	  z1 += Area - Need;
	  z0 += Area - Need;
	}
    }

  // Create PLF for standard "z0, z1, z2" distribution.
  PLF LADvsH;
  LADvsH.add (z0 * Height, 0.0);
  LADvsH.add (z1 * Height, LADm);
  LADvsH.add (z2 * Height, LADm);
  LADvsH.add (     Height, 0.0);
  LAIvsH = LADvsH.integrate_stupidly ();
  const double CAIm = - log (PARrel) / PARext;
  CAImRat = max (0.0, (CAI - CAIm) / CAIm);
}

void
CanopyStandard::tick (double WLeaf, double WSOrg, double WStem, double DS)
{
  Height = CropHeight (WStem, DS);
  if (InitCAI)
    InitialCAI (WLeaf, DS);
  else
    CropCAI (WLeaf, WSOrg, WStem, DS);
}

void
CanopyStandard::output (Log& log) const
{
  CanopySimple::output (log);
  
  log.output ("InitCAI", InitCAI);
  log.output ("Offset", Offset);
  log.output ("LeafAI", LeafAI);
  log.output ("StemAI", StemAI);
  log.output ("SOrgAI", SOrgAI);
  log.output ("LADm", LADm);
  log.output ("CAImRat", CAImRat);
}

void 
CanopyStandard::load_syntax (Syntax& syntax, AttributeList& alist)
{
  CanopySimple::load_syntax (syntax, alist);
  alist.add ("submodel", "CanopyStandard");

  // Parameters.
  syntax.add ("DSLAI05", Syntax::None (), Syntax::Const,
	      "DS at CAI=0.5; forced development.");
  alist.add ("DSLAI05", 0.15);
  syntax.add ("SpLAI", "(m^2/m^2)/(g DM/m^2)", Syntax::Const,
	      " Specific leaf weight.");
  syntax.add ("LeafAIMod", "DS", Syntax::None (), Syntax::Const,
	      "Specific leaf weight modifier.");
  PLF AIDef;
  AIDef.add (0.00, 1.00);
  AIDef.add (2.00, 1.00);
  alist.add ("LeafAIMod", AIDef);
  syntax.add ("SpLAIfac", Syntax::None (), Syntax::Const,
	      "Factor defining maximum Specific leaf weight.");
  alist.add ("SpLAIfac", 2.0);
  syntax.add ("SpSOrgAI", "(m^2/m^2)/(g DM/m^2)", Syntax::Const,
	      "Specific storage organ weight.");
  alist.add ("SpSOrgAI", 0.0);
  syntax.add ("SOrgAIMod", "DS", Syntax::None (), Syntax::Const,
	      "Specific storage organ weight modifier");
  alist.add ("SOrgAIMod", AIDef);
  syntax.add ("SOrgPhotEff", Syntax::None (), Syntax::Const,
	      "Relative photosynthetic efficiency of storage organ.");
  alist.add ("SOrgPhotEff", 1.0);
  syntax.add ("SpStemAI", "(m^2/m^2)/(g DM/m^2)", Syntax::Const,
	      "Specific stem weight.");
  alist.add ("SpStemAI", 0.0);
  syntax.add ("StemAIMod", "DS", Syntax::None (), Syntax::Const,
	      "Specific stem weight modifier.");
  alist.add ("StemAIMod", AIDef);
  syntax.add ("StemPhotEff", Syntax::None (), Syntax::Const,
	      "Relative photosynthetic efficiency of stem.");
  alist.add ("StemPhotEff", 1.0);
  syntax.add ("HvsDS", Syntax::None (), "cm", Syntax::Const,
	      "Crop height as function of DS.");
  PLF HvsStem;
  HvsStem.add (0.00 , 0.10);
  HvsStem.add (200.0, 1.00);
  syntax.add ("HvsWStem", "g DM/m^2", Syntax::Fraction (), Syntax::Const,
	      "Relative crop height as function of stem weight.");
  alist.add ("HvsWStem", HvsStem);
  syntax.add ("LAIDist0", Syntax::None (), Syntax::Const, 3,
	      "Relative CAI distribution at DS=0.");
  syntax.add ("LAIDist1", Syntax::None (), Syntax::Const, 3,
	      "Relative CAI distribution at DS=1.");
  syntax.add ("PARrel", Syntax::None (), Syntax::Const,
	      "Relative PAR below the syntax.");
  alist.add ("PARrel", 0.05);

  // Variables.
  syntax.add ("InitCAI", Syntax::Boolean, Syntax::State,
	      "Initial CAI development.");
  alist.add ("InitCAI", true);
  syntax.add ("Offset", "cm", Syntax::State, "Extra height after harvest.");
  alist.add ("Offset", 0.0);
  syntax.add ("LeafAI", "m^2/m^2", Syntax::State, "Leaf Area Index.");
  alist.add ("LeafAI", 0.0);
  syntax.add ("StemAI", "m^2/m^2", Syntax::State, "Stem Area Index.");
  alist.add ("StemAI", 0.0);
  syntax.add ("SOrgAI", "m^2/m^2", Syntax::State, "Storage Organ Area Index.");
  alist.add ("SOrgAI", 0.0);
  syntax.add ("LADm", "cm^2/cm^3", Syntax::State,
	      "Maximal Leaf Area Density.");
  alist.add ("LADm", -9999.99);

  // Log Variables.
  syntax.add ("CAImRat", Syntax::None (), Syntax::LogOnly,
	      "(CAIm - CAI) / CAIm.");
}

CanopyStandard::CanopyStandard (const AttributeList& vl)
  : CanopySimple (vl),
    DSLAI05 (vl.number ("DSLAI05")),
    SpLAI (vl.number ("SpLAI")),
    LeafAIMod (vl.plf ("LeafAIMod")),
    SpLAIfac (vl.number ("SpLAIfac")),
    SpSOrgAI (vl.number ("SpSOrgAI")),
    SOrgAIMod (vl.plf ("SOrgAIMod")),
    SOrgPhotEff (vl.number ("SOrgPhotEff")),
    SpStemAI (vl.number ("SpStemAI")),
    StemAIMod (vl.plf ("StemAIMod")),
    StemPhotEff (vl.number ("StemPhotEff")),
    HvsDS (vl.plf ("HvsDS")),
    HvsWStem (vl.plf ("HvsWStem")),
    LAIDist0 (vl.number_sequence ("LAIDist0")),
    LAIDist1 (vl.number_sequence ("LAIDist1")),
    PARrel (vl.number ("PARrel")),
    InitCAI (vl.flag ("InitCAI")),
    Offset (vl.number ("Offset")),
    LeafAI (vl.number ("LeafAI")),
    StemAI (vl.number ("StemAI")),
    SOrgAI (vl.number ("SOrgAI")),
    LADm (vl.number ("LADm")),
    CAImRat (0.0)
{ }

CanopyStandard::~CanopyStandard ()
{ }

static Submodel::Register 
soil_submodel ("CanopyStandard", CanopyStandard::load_syntax);
