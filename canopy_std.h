// canopy_std.h -- Canopy development for standard crop model.

#include "canopy_simple.h"

class CanopyStandard : public CanopySimple
{
  // Paramaters.
private:
  const double DSLAI05;		// DS at CAI=0.5; forced development
  const double SpLAI;		// Specific leaf weight [ (m²/m²) / (g/m²) ]
  const PLF& LeafAIMod;		// Specific leaf area index modifier
  const double SpLAIfac;	// Factor defining max Specific leaf weight
  const double SpSOrgAI;	// Specific storage organ area index
  const PLF& SOrgAIMod;		// Specific storage organ area index modifier
  const double SOrgPhotEff;	// Relative photosynthetic effiency of stor. org.
  const double SpStemAI;	// Specific stem area index
  const PLF& StemAIMod;		// Specific stem area index modifier
  const double StemPhotEff;	// Relative photosynthetic effiency of stem.
public:
  const PLF& HvsDS;		// Crop height as function of DS
  const PLF& HvsWStem;		// Relative Crop height as function of stem weight
private:
  const vector<double>& LAIDist0; // Relative CAI distribution at DS=0
  const vector<double>& LAIDist1; // Relative CAI distribution at DS=1
  const double PARrel;		// Relative PAR below the canopy

  // Variables.
private:
  bool InitCAI;			// Initial CAI development ?
public:
  double Offset;		// Extra height after harvest [cm]
private:
  double LeafAI;		// Leaf Area Index
  double StemAI;		// Stem Area Index
  double SOrgAI;		// Storage organ Area Index
  double LADm;			// Max Leaf Area Density [cm2/cm3]
public:
  double CAImRat;		// (CAIm - CAI) / CAIm []

  // Functions.
public:
  double CropHeight (double Wstem, double DS);
private:
  void InitialCAI (double WLeaf, double DS);
public:
  void CropCAI (double WLeaf, double WSOrg, double WStem, double DS);
  void CanopyStructure (double DS);

  // Simulation.
public:
  void tick (double WLeaf, double WSOrg, double WStem, double DS);
  void output (Log&) const;

  // Create and Destroy.
public:
  static void load_syntax (Syntax& syntax, AttributeList& alist);
  CanopyStandard (const AttributeList&);
  ~CanopyStandard ();
};
