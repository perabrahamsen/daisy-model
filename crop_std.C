// crop_std.C

#include "crop.h"
#include "root_system.h"
#include "canopy_std.h"
#include "log.h"
#include "time.h"
#include "bioclimate.h"
#include "common.h"
#include "plf.h"
#include "soil_water.h"
#include "soil.h"
#include "om.h"
#include "organic_matter.h"
#include "soil_heat.h"
#include "soil_NH4.h"
#include "soil_NO3.h"
#include "am.h"
#include "harvest.h"
#include "mathlib.h"

// Dimensional conversion.
static const double m2_per_cm2 = 0.0001;

// Chemical constants affecting the crop.
const double molWeightCH2O = 30.0; // [gCH2O/mol]
const double molWeightCO2 = 44.0; // [gCO2/mol]

class CropStandard : public Crop
{
  // Content.
public:
  RootSystem& root_system;
  CanopyStandard& canopy;

  struct Parameters;
  struct Variables;
  const Parameters& par;
  Variables& var;

  // Communication with Bioclimate.
public:
  double water_stress () const // [0-1] (0 = full production)
  { return root_system.water_stress; }
  double nitrogen_stress () const // [0-1] (1 = no production)
  { return root_system.nitrogen_stress; }
  double rs_min () const	// Minimum transpiration resistance.
  { return canopy.rs_min; }
  double rs_max () const	// Maximum transpiration resistance.
  { return canopy.rs_max; }
  double height () const	// Crop height [cm]
  { return canopy.Height; }
  double LAI () const
  { return canopy.CAI; }
  const PLF& LAIvsH () const
  { return canopy.LAIvsH; }
  double PARext () const
  { return canopy.PARext; }
  double PARref () const
  { return canopy.PARref; }
  double EPext () const
  { return canopy.EPext; }
  double IntcpCap () const	// Interception Capacity.
  { return canopy.IntcpCap; }
  double EpFac () const		// Convertion to potential evapotransp.
  { return canopy.EpFactor (DS ()); }
  void CanopyStructure ();
  double ActualWaterUptake (double Ept, const Soil&, SoilWater&,
			    double EvapInterception);
  void force_production_stress  (double pstress);

  // Internal functions.
public:				// Used by external development models.
  void Vernalization (double Ta);
protected:
  void Emergence ();
  void DevelopmentStage (const Bioclimate&);
  void NitContent ();
  void NitrogenUptake (int Hour,
		       const Soil& soil,
		       const SoilWater& soil_water,
		       SoilNH4& soil_NH4,
		       SoilNO3& soil_NO3);
  // Sugar production [gCH2O/m2/h] by canopy photosynthesis.
  double CanopyPhotosynthesis (const Bioclimate&);
  double ReMobilization ();
  void AssimilatePartitioning (double DS,
			       double& f_Leaf, double& f_Stem,
			       double& f_Root, double& f_SOrg);
  double MaintenanceRespiration (double r, double w, double T);
  void NetProduction (const Bioclimate&, const Geometry&, const SoilHeat&);
  void NoProduction ();
  double RSR () const;

  // Simulation.
public:
  void tick (const Time& time, const Bioclimate&, const Soil&,
	     OrganicMatter*,
	     const SoilHeat&,
	     const SoilWater&,
	     SoilNH4*,
	     SoilNO3*);
  const Harvest& harvest (const string& column_name,
			  const Time&, const Geometry&,
			  Bioclimate& bioclimate,
			  double stub_length, double stem_harvest,
			  double leaf_harvest, double sorg_harvest,
			  bool kill_off,
			  vector<AM*>& residuals);
  void output (Log&) const;

  double DS () const;
  double DM () const;

  // Create and Destroy.
public:
  void initialize (const Geometry& geometry, const OrganicMatter&);
  void initialize (const Geometry& geometry);
  CropStandard (const AttributeList& vl);
  ~CropStandard ();
};

struct CropStandard::Parameters
{
  const struct DevelPar
  {
    double EmrTSum;		// Soil temp sum at emergence
    const PLF& EmrSMF;         // Soil moisture effect on emergence
    double DS_Emr;		// Development stage (DS) emergence
    double DSRate1;		// Development rate [C-1 or d-1],
    				// the vegetative stage
    double DSRate2;		// Development rate [C-1 or d-1],
				// the reproductive stage
    const PLF& TempEff1;	// Temperature effect, vegetative stage
    const PLF& TempEff2;	// Temperature effect, reproductive stage
    const PLF& PhotEff1;	// Ptotoperiode effect, vegetative stage
    				// defined limit
    double defined_until_ds;	// Model invalid after this DS.
  private:
    friend struct CropStandard::Parameters;
    DevelPar (const AttributeList&);
  } Devel;
  const struct VernalPar {
    bool required;
    double DSLim;		// Max DS without vernalization
    double TaLim;		// Vernalization temp threshold
    double TaSum;		// Vernalization T-sum requirement
    static const AttributeList* none;
  private:
    friend struct CropStandard::Parameters;
    VernalPar (const AttributeList&);
  } Vernal;
  const struct LeafPhotPar {
    double Qeff;		// Quantum efficiency at low light
    double Fm;			// Max assimilation rate
    const PLF& TempEff;	// Temperature effect, photosynthesis
  private:
    friend struct CropStandard::Parameters;
    LeafPhotPar (const AttributeList&);
  } LeafPhot;
  const struct PartitPar {
    const PLF& Root;		// Partitioning functions for root
    const PLF& Leaf;		//   leaf, and stem as function of DS
    const PLF& Stem;
    const PLF& RSR;		// Root/Shoot ratio.
  private:
    friend struct CropStandard::Parameters;
    PartitPar (const AttributeList&);
  } Partit;
  struct ProdPar {
    double CH2OReleaseRate;     // CH2O Release Rate [h-1]
    double E_Root;		// Conversion efficiency, root
    double E_Leaf;		// Conversion efficiency, leaf
    double E_Stem;		// Conversion efficiency, stem
    double E_SOrg;		// Conversion efficiency, stor. org.
    double r_Root;		// Maint. resp. coeff., root
    double r_Leaf;		// Maint. resp. coeff., leaf
    double r_Stem;		// Maint. resp. coeff., stem
    double r_SOrg;		// Maint. resp. coeff., stor. org.
    double ShldResC;		// Capacity of Shielded Reserves
    double ReMobilDS;		// Remobilization, Initial DS
    double ReMobilRt;		// Remobilization, release rate
    double ExfoliationFac;      // Exfoliation factor, 0-1
    double GrowthRateRedFac;    // Growth rate reduction factor, 0-1
    const PLF& LfDR;		// Death rate of Leafs
    const PLF& RtDR;		// Death rate of Roots
    const double Large_RtDR;	// Extra death rate for large root/shoot.
  private:
    friend struct CropStandard::Parameters;
    ProdPar (const AttributeList&);
  } Prod;
  struct CrpNPar {
    double SeedN;		// N-content in seed [ g N/m² ]
    double DS_fixate;		// Fixation of atmospheric N. after this DS
    double DS_cut_fixate;	// Restore fixation this DS after cut.
    double fixate_factor;	// Fraction of N need covered by fixation.
    const PLF& PtLeafCnc;	// Upper limit for N-conc in leaves
    const PLF& CrLeafCnc;	// Critical lim f. N-conc in leaves
    const PLF& NfLeafCnc;	// Non-func lim f. N-conc in leaves
    const PLF& PtStemCnc;	// Upper limit for N-conc in stems
    const PLF& CrStemCnc;	// Critical lim f. N-conc in stems
    const PLF& NfStemCnc;	// Non-func lim f. N-conc in stems
    const PLF& PtRootCnc;	// Upper limit for N-conc in roots
    const PLF& CrRootCnc;	// Critical lim f. N-conc in roots
    const PLF& NfRootCnc;	// Non-func lim f. N-conc in roots
    const PLF& PtSOrgCnc;	// Upper limit for N-conc in stor org
    const PLF& CrSOrgCnc;	// Critical lim f. N-conc in stor org
    const PLF& NfSOrgCnc;	// Non-func lim f. N-conc in stor org
    const PLF& TLLeafEff;	// Translocation effiency, Leaf.
    const PLF& TLRootEff;	// Translocation effiency, Root.
  private:
    friend struct CropStandard::Parameters;
    CrpNPar (const AttributeList&);
  } CrpN;
  struct HarvestPar {
    const vector<AttributeList*>& Stem; // Stem AM parameters.
    const vector<AttributeList*>& Leaf; // Leaf AM parameters.
    const vector<AttributeList*>& Dead; // Dead AM parameters.
    const vector<AttributeList*>& SOrg; // SOrg AM parameters.
    const vector<AttributeList*>& Root; // Root AM parameters.
    const double EconomicYield_W; // Frac. of economic yield (DM) in storage org.
    const double EconomicYield_N; // Frac. of economic yield (N) in storage org.
    const double C_Stem;	// C fraction of total weight.
    const double C_Leaf;	// C fraction of total weight.
    const double C_Dead;	// C fraction of total weight.
    const double C_SOrg;	// C fraction of total weight.
    const double C_Root;	// C fraction of total weight.
    const double DSmax;		// Maximal development stage for which
				// the crop survives harvest.
    const double DSnew;		// Maximal development stage after harvest.
  private:
    friend struct CropStandard::Parameters;
    HarvestPar (const AttributeList&);
  } Harvest;
  const bool enable_water_stress;
  const bool enable_N_stress;
private:
  friend class CropStandard;
  Parameters (const AttributeList&);
public:
  ~Parameters ();
};

const AttributeList* CropStandard::Parameters::VernalPar::none;

struct CropStandard::Variables
{
  void output (Log&) const;
  struct RecPhenology
  {
    void output (Log&) const;
    double DS;	        	// Development Stage
    double Vern;		// Vernalization criterium [C d]
    double partial_day_length;	// Light hours this day until now [0-24 h]
    double day_length;		// Light hours previous day. [0-24 h]
    double partial_soil_temperature; // Accumaleted soil temperature. [°C]
    double soil_temperature;	// Soil temperature previous day. [°C]
    double soil_h;		// Soil potential [cm]
  private:
    friend struct CropStandard::Variables;
    RecPhenology (const Parameters&, const AttributeList&);
  } Phenology;
  struct RecProd
  {
    void output (Log&) const;
    double CH2OPool;            // Carbonhydrate pool [g/m2]
    double WLeaf;		// Leaf dry matter weight [g/m2]
    double WStem;		// Stem dry matter weight [g/m2]
    double WRoot;		// Root dry matter weight [g/m2]
    double WSOrg;		// Storage organ dry matter weight [g/m2]
    double WDead;               // Dead plant material [g/m2]
    double NCrop;		// Nitrogen stored in dry matter [g/m2]
    double NLeaf;		// Leaf nitrogen [g/m2]
    double NStem;		// Stem nitrogen [g/m2]
    double NRoot;		// Root nitrogen [g/m2]
    double NSOrg;		// Storage organ nitrogen [g/m2]
    double NDead;               // N in dead plant material [g/m2]
    double C_AM;                // Added C in plant material [g/m2]
    double N_AM;                // Added N in plant material [g/m2]
    AM* AM_root;		// Dead organic root matter.
    AM* AM_leaf;		// Dead organic leaf matter.

  private:
    friend struct CropStandard::Variables;
    RecProd (const Parameters&, const AttributeList&);
  } Prod;
  struct RecCrpAux
  {
    void output (Log&) const;
    double StemRes;		// Shielded Reserves in Stems
    double PtNCnt;		// Potential Nitrogen Content in Crop [g/m2]
    double CrNCnt;		// Critical Nitrogen Content in Crop [g/m2]
    double NfNCnt;		// Non-func Nitrogen Content in Crop [g/m2]
    double PotTransp;	        // Potential Transpiration [mm/h]
    double PotCanopyAss;	// Potential Canopy Assimilation [g CH2O/m2/h]
    double CanopyAss;	        // Canopy Assimilation [g CH2O/m2/h]
    double NetPhotosynthesis;	// Net Photosynthesis [g CO2/m2/h]
    double RootRespiration;	// Root Respiration [g CO2/m2/h]
    double IncWLeaf;     	// Leaf growth [g DM/m2/d]
    double IncWStem;    	// Stem growth [g DM/m2/d]
    double IncWSOrg;    	// Storage organ growth [g DM/m2/d]
    double IncWRoot;    	// Root growth [g DM/m2/d]
    double DeadWLeaf;		// Leaf DM removed [g DM/m2/d]
    double DeadNLeaf;		// Leaf N removed [g N/m2/d]
    double DeadWRoot;		// Root DM removed [g DM/m2/d]
    double DeadNRoot;		// Root N removed [g N/m2/d]
    double Fixated;		// N fixation from air. [g/m2/h]
    double AccFixated;		// Accumulated N fixation from air. [g/m2]
    double DS_start_fixate;	// Start fixation at this DS.
  private:
    friend struct CropStandard::Variables;
    RecCrpAux (const Parameters&, const AttributeList&);
  } CrpAux;
private:
  friend class CropStandard;
  Variables (const Parameters&, const AttributeList&);
public:
  ~Variables ();
};

CropStandard::Parameters::Parameters (const AttributeList& vl)
  : Devel (vl.alist ("Devel")),
    Vernal (vl.check ("Vernal") ? vl.alist ("Vernal") : *VernalPar::none),
    LeafPhot (vl.alist ("LeafPhot")),
    Partit (vl.alist ("Partit")),
    Prod (vl.alist ("Prod")),
    CrpN (vl.alist ("CrpN")),
    Harvest (vl.alist ("Harvest")),
    enable_water_stress (vl.flag ("enable_water_stress")),
    enable_N_stress (vl.flag ("enable_N_stress"))
{ }

CropStandard::Parameters::DevelPar::DevelPar (const AttributeList& vl)
  : EmrTSum (vl.number ("EmrTSum")),
    EmrSMF (vl.plf ("EmrSMF")),
    DS_Emr (vl.number ("DS_Emr")),
    DSRate1 (vl.number ("DSRate1")),
    DSRate2 (vl.number ("DSRate2")),
    TempEff1 (vl.plf ("TempEff1")),
    TempEff2 (vl.plf ("TempEff2")),
    PhotEff1 (vl.plf ("PhotEff1")),
    defined_until_ds (vl.number ("defined_until_ds"))
{ }

CropStandard::Parameters::VernalPar::VernalPar (const AttributeList& vl)
  : required (vl.check ("required") ? vl.flag ("required") : true),
    DSLim (vl.number ("DSLim")),
    TaLim (vl.number ("TaLim")),
    TaSum (vl.number ("TaSum"))
{ }

CropStandard::Parameters::LeafPhotPar::LeafPhotPar (const AttributeList& vl)
  : Qeff (vl.number ("Qeff")),
    Fm (vl.number ("Fm")),
    TempEff (vl.plf ("TempEff"))
{ }

CropStandard::Parameters::PartitPar::PartitPar (const AttributeList& vl)
  : Root (vl.plf ("Root")),
    Leaf (vl.plf ("Leaf")),
    Stem (vl.plf ("Stem")),
    RSR (vl.plf ("RSR"))
{ }

CropStandard::Parameters::ProdPar::ProdPar (const AttributeList& vl)
  : CH2OReleaseRate (vl.number ("CH2OReleaseRate")),
    E_Root (vl.number ("E_Root")),
    E_Leaf (vl.number ("E_Leaf")),
    E_Stem (vl.number ("E_Stem")),
    E_SOrg (vl.number ("E_SOrg")),
    r_Root (vl.number ("r_Root")),
    r_Leaf (vl.number ("r_Leaf")),
    r_Stem (vl.number ("r_Stem")),
    r_SOrg (vl.number ("r_SOrg")),
    ShldResC (vl.number ("ShldResC")),
    ReMobilDS (vl.number ("ReMobilDS")),
    ReMobilRt (vl.number ("ReMobilRt")),
    ExfoliationFac (vl.number ("ExfoliationFac")),
    GrowthRateRedFac (vl.number ("GrowthRateRedFac")),
    LfDR (vl.plf ("LfDR")),
    RtDR (vl.plf ("RtDR")),
    Large_RtDR (vl.number ("Large_RtDR"))
{ }

CropStandard::Parameters::CrpNPar::CrpNPar (const AttributeList& vl)
  : SeedN (vl.number ("SeedN")),
    DS_fixate (vl.number ("DS_fixate")),
    DS_cut_fixate (vl.number ("DS_cut_fixate")),
    fixate_factor (vl.number ("fixate_factor")),
    PtLeafCnc (vl.plf ("PtLeafCnc")),
    CrLeafCnc (vl.plf ("CrLeafCnc")),
    NfLeafCnc (vl.plf ("NfLeafCnc")),
    PtStemCnc (vl.plf ("PtStemCnc")),
    CrStemCnc (vl.plf ("CrStemCnc")),
    NfStemCnc (vl.plf ("NfStemCnc")),
    PtRootCnc (vl.plf ("PtRootCnc")),
    CrRootCnc (vl.plf ("CrRootCnc")),
    NfRootCnc (vl.plf ("NfRootCnc")),
    PtSOrgCnc (vl.plf ("PtSOrgCnc")),
    CrSOrgCnc (vl.plf ("CrSOrgCnc")),
    NfSOrgCnc (vl.plf ("NfSOrgCnc")),
    TLLeafEff (vl.plf ("TLLeafEff")),
    TLRootEff (vl.plf ("TLRootEff"))
{ }

CropStandard::Parameters::HarvestPar::HarvestPar (const AttributeList& vl)
  : Stem (vl.alist_sequence ("Stem")),
    Leaf (vl.alist_sequence ("Leaf")),
    Dead (vl.alist_sequence ("Dead")),
    SOrg (vl.alist_sequence ("SOrg")),
    Root (vl.alist_sequence ("Root")),
    EconomicYield_W (vl.number ("EconomicYield_W")),
    EconomicYield_N (vl.check ("EconomicYield_N")
                     ? vl.number ("EconomicYield_N")
                     : vl.number ("EconomicYield_W")),
    C_Stem (vl.number ("C_Stem")),
    C_Leaf (vl.number ("C_Leaf")),
    C_Dead (vl.number ("C_Dead")),
    C_SOrg (vl.number ("C_SOrg")),
    C_Root (vl.number ("C_Root")),
    DSmax (vl.number ("DSmax")),
    DSnew (vl.number ("DSnew"))
{ }

CropStandard::Parameters::~Parameters ()
{ }

CropStandard::Variables::Variables (const Parameters& par,
				    const AttributeList& vl)
  : Phenology (par, vl.alist ("Phenology")),
    Prod (par, vl.alist ("Prod")),
    CrpAux (par, vl.alist ("CrpAux"))
{ }

void
CropStandard::Variables::output (Log& log) const
{
  if (log.check ("Phenology"))
    Phenology.output (log);
  if (log.check ("Prod"))
    Prod.output (log);
  if (log.check ("CrpAux"))
    CrpAux.output (log);
}

CropStandard::Variables::RecPhenology::RecPhenology (const Parameters& par,
						     const AttributeList& vl)
  : DS (vl.number ("DS")),
    Vern (vl.check ("Vern") ? vl.number ("Vern") : par.Vernal.TaSum),
    partial_day_length (vl.number ("partial_day_length")),
    day_length (vl.number ("day_length")),
    partial_soil_temperature (vl.number ("partial_soil_temperature")),
    soil_temperature (vl.number ("soil_temperature")),
    soil_h (vl.number ("soil_h"))

{ }

void
CropStandard::Variables::RecPhenology::output (Log& log) const
{
  log.open ("Phenology");
  log.output ("DS", DS);
  log.output ("Vern", Vern);
  log.output ("partial_day_length", partial_day_length);
  log.output ("day_length", day_length);
  log.output ("partial_soil_temperature", partial_soil_temperature);
  log.output ("soil_temperature", soil_temperature);
  log.output ("soil_h", soil_h);
  log.close();
}

CropStandard::Variables::RecProd::RecProd (const Parameters& par,
					   const AttributeList& vl)
  : CH2OPool (vl.number ("CH2OPool")),
    WLeaf (vl.number ("WLeaf")),
    WStem (vl.number ("WStem")),
    WRoot (vl.number ("WRoot")),
    WSOrg (vl.number ("WSOrg")),
    WDead (vl.number ("WDead")),
    NCrop (vl.check ("NCrop") ? vl.number ("NCrop") : par.CrpN.SeedN),
    NLeaf (vl.number ("NLeaf")),
    NStem (vl.number ("NStem")),
    NRoot (vl.number ("NRoot")),
    NSOrg (vl.number ("NSOrg")),
    NDead (vl.number ("NDead")),
    C_AM  (vl.number ("C_AM")),
    N_AM  (vl.number ("N_AM")),
    AM_root (NULL),
    AM_leaf (NULL)
{ }

void
CropStandard::Variables::RecProd::output (Log& log) const
{
  log.open ("Prod");
  log.output ("CH2OPool", CH2OPool);
  log.output ("WLeaf", WLeaf);
  log.output ("WStem", WStem);
  log.output ("WRoot", WRoot);
  log.output ("WSOrg", WSOrg);
  log.output ("WDead", WDead);
  log.output ("NLeaf", NLeaf);
  log.output ("NStem", NStem);
  log.output ("NRoot", NRoot);
  log.output ("NSOrg", NSOrg);
  log.output ("NDead", NDead);
  log.output ("NCrop", NCrop);
  log.output ("C_AM", C_AM);
  log.output ("N_AM", N_AM);
  log.close();
}

CropStandard::Variables::RecCrpAux::RecCrpAux (const Parameters& par,
					       const AttributeList& vl)
  : StemRes (vl.number ("StemRes")),
    PtNCnt (0.0),
    CrNCnt (0.0),
    NfNCnt (0.0),
    PotTransp (vl.number ("PotTransp")),
    PotCanopyAss (vl.number ("PotCanopyAss")),
    CanopyAss (vl.number ("CanopyAss")),
    NetPhotosynthesis (0.0),
    RootRespiration (0.0),
    IncWLeaf (0.0),
    IncWStem (0.0),
    IncWSOrg (0.0),
    IncWRoot (0.0),
    DeadWLeaf (0.0),
    DeadNLeaf (0.0),
    DeadWRoot (0.0),
    DeadNRoot (0.0),
    Fixated (0.0),
    AccFixated (vl.number ("AccFixated")),
    DS_start_fixate (vl.check ("DS_start_fixate")
		     ? vl.number ("DS_start_fixate")
		     : par.CrpN.DS_fixate)
{ }

void
CropStandard::Variables::RecCrpAux::output (Log& log) const
{
  log.open ("CrpAux");
  log.output ("StemRes", StemRes);
  log.output ("PtNCnt", PtNCnt);
  log.output ("CrNCnt", CrNCnt);
  log.output ("NfNCnt", NfNCnt);
  log.output ("PotTransp", PotTransp);
  log.output ("PotCanopyAss", PotCanopyAss);
  log.output ("CanopyAss", CanopyAss);
  log.output ("NetPhotosynthesis", NetPhotosynthesis);
  log.output ("RootRespiration", RootRespiration);
  log.output ("IncWLeaf", IncWLeaf);
  log.output ("IncWStem", IncWStem);
  log.output ("IncWSOrg", IncWSOrg);
  log.output ("IncWRoot", IncWRoot);
  log.output ("DeadWLeaf", DeadWLeaf);
  log.output ("DeadNLeaf", DeadNLeaf);
  log.output ("DeadWRoot", DeadWRoot);
  log.output ("DeadNRoot", DeadNRoot);
  log.output ("Fixated", Fixated);
  log.output ("AccFixated", AccFixated);
  log.output ("DS_start_fixate", DS_start_fixate);
  log.close();
}

CropStandard::Variables::~Variables ()
{ }

void
CropStandard::initialize (const Geometry& geometry,
			  const OrganicMatter& organic_matter)
{
  root_system.initialize (geometry.size ());

  if (var.Phenology.DS >= 0)
    {
      var.Prod.AM_leaf = organic_matter.find_am (name, "dead");
      assert (var.Prod.AM_leaf);
      var.Prod.AM_root = organic_matter.find_am (name, "root");
      assert (var.Prod.AM_root);
      NitContent ();
    }
}

void
CropStandard::initialize (const Geometry& geometry)
{
  root_system.initialize (geometry.size ());

  if (var.Phenology.DS >= 0)
    NitContent ();
}

static struct CropStandardSyntax
{
  static Crop& make (const AttributeList& al)
    { return *new CropStandard (al); }
  CropStandardSyntax ();
} standard_crop_syntax;

#ifdef BORLAND_TEMPLATES
template class add_submodule_sequence<OM>;
template class add_submodule<RootSystem>;
template class add_submodule<CanopyStandard>;
#endif

CropStandardSyntax::CropStandardSyntax ()
{
  static const PLF empty_plf;

  Syntax& Devel = *new Syntax ();
  AttributeList& vDevel = *new AttributeList ();
  Syntax& Vernal = *new Syntax ();
  // WARNING: Don't add an Vernal alist, or the 'Optional' idea is lost.
  Syntax& LeafPhot = *new Syntax ();
  Syntax& Partit = *new Syntax ();
  Syntax& Prod = *new Syntax ();
  AttributeList& vProd = *new AttributeList ();
  Syntax& CrpN = *new Syntax ();
  AttributeList& CrpNList = *new AttributeList ();
  Syntax& Harvest = *new Syntax ();
  AttributeList& HarvestList = *new AttributeList ();
  Syntax& Phenology = *new Syntax ();
  AttributeList& vPhenology = *new AttributeList ();
  Syntax& CrpAux = *new Syntax ();
  AttributeList& vCrpAux = *new AttributeList ();

  // DevelPar
  Devel.add ("EmrTSum", "dg C d", Syntax::Const,
	     "Soil temperature sum at emergence.");
  Devel.add ("EmrSMF", "cm", "d", Syntax::Const,
	     "Soil moisture (h-function) effect on emergense.");
  PLF SMF;
  SMF.add (-1000.0, 0.00);
  SMF.add (-150.0, 1.00);
  SMF.add (-50.00, 1.00);
  SMF.add (-30.00, 0.00);
  vDevel.add("EmrSMF",SMF);
  Devel.add ("DS_Emr", Syntax::None (), Syntax::Const,
	     "Development stage at emergence.");
  Devel.add ("DSRate1", Syntax::None (), Syntax::Const,
	     "Development rate in the vegetative stage.");
  Devel.add ("DSRate2", Syntax::None (), Syntax::Const,
	     "Development rate in the reproductive stage.");
  Devel.add ("TempEff1", "dg C", Syntax::None (), Syntax::Const,
	     "Temperature effect, vegetative stage.");
  Devel.add ("TempEff2", "dg C", Syntax::None (), Syntax::Const,
	     "Temperature effect, reproductive stage.");
  Devel.add ("PhotEff1", "h", Syntax::None (), Syntax::Const,
	     "Photoperiode effect, vegetative stage.");
  Devel.add ("defined_until_ds", Syntax::None (), Syntax::Const,
	     "\
This parameterization is only valid until the specified development state.");
  vDevel.add ("defined_until_ds", 2.0);

  // VernalPar
  Vernal.add ("required", Syntax::Boolean, Syntax::OptionalConst,
	      "True, iff the crop requires vernalization.");
  Vernal.add ("DSLim", Syntax::None (), Syntax::Const,
	      "Development stage at vernalization.");
  Vernal.add ("TaLim", "dg C", Syntax::Const,
	      "Vernalization temperature threshold.");
  Vernal.add ("TaSum", "dg C d", Syntax::Const,
	      "Vernalization temperature-sum requirement.");

  // Initialize "no vernalization"
  AttributeList& noVernal = *new AttributeList ();
  noVernal.add ("required", false);
  noVernal.add ("DSLim", -42.42e42);
  noVernal.add ("TaLim", -42.42e42);
  noVernal.add ("TaSum", -42.42e42);
  CropStandard::Parameters::VernalPar::none = &noVernal;

  // LeafPhotPar
  LeafPhot.add ("Qeff", "(g CO2/m^2/h)/(W/m^2)", Syntax::Const,
		"Quantum efficiency at low light.");
  LeafPhot.add ("Fm", "g CO2/m^2/h", Syntax::Const,
		"Maximum assimilation rate.");
  LeafPhot.add ("TempEff", "dg C", Syntax::None (), Syntax::Const,
		"Temperature effect, photosynthesis.");

  // PartitPar
  Partit.add ("Root", "DS", Syntax::None (), Syntax::Const,
	      "Partitioning functions for root.");
  Partit.add ("Leaf", "DS", Syntax::None (), Syntax::Const,
	      "Partitioning functions for leaves.");
  Partit.add ("Stem", "DS", Syntax::None (), Syntax::Const,
	      "Partitioning functions for stem.");
  Partit.add ("RSR", "DS", Syntax::None (), Syntax::Const,
	      "Root/Shoot ratio as a function of development state.");

  // ProdPar
  Prod.add ("CH2OReleaseRate", Syntax::None (), Syntax::Const,
	    "CH2O Release Rate constant.");
  vProd.add ("CH2OReleaseRate", 0.04);
  Prod.add ("E_Root", Syntax::None (), Syntax::Const,
	    "Conversion efficiency, root.");
  vProd.add ("E_Root", 0.69);
  Prod.add ("E_Leaf", Syntax::None (), Syntax::Const,
	    "Conversion efficiency, leaf.");
  vProd.add ("E_Leaf", 0.68);
  Prod.add ("E_Stem", Syntax::None (), Syntax::Const,
	    "Conversion efficiency, stem.");
  vProd.add ("E_Stem", 0.66);
  Prod.add ("E_SOrg", Syntax::None (), Syntax::Const,
	    "Conversion efficiency, storage organ.");
  Prod.add ("r_Root", Syntax::None (), Syntax::Const,
	    "Maintenance respiration coefficient, root.");
  vProd.add ("r_Root", 0.015);
  Prod.add ("r_Leaf", Syntax::None (), Syntax::Const,
	    "Maintenance respiration coefficient, leaf.");
  Prod.add ("r_Stem", Syntax::None (), Syntax::Const,
	    "Maintenance respiration coefficient, stem.");
  Prod.add ("r_SOrg", Syntax::None (), Syntax::Const,
	    "Maintenance respiration coefficient, storage organ.");
  Prod.add ("ShldResC", Syntax::None (), Syntax::Const,
	    "Capacity of shielded reserves (fraction of stem DM).");
  vProd.add ("ShldResC", 0.0);
  Prod.add ("ReMobilDS", Syntax::None (), Syntax::Const,
	    "Remobilization, Initial DS.");
  vProd.add ("ReMobilDS", 1.20);
  Prod.add ("ReMobilRt", "d^-1", Syntax::Const,
	    "Remobilization, release rate.");
  vProd.add ("ReMobilRt", 0.1);
  Prod.add ("ExfoliationFac", Syntax::None (), Syntax::Const,
	    "Exfoliation factor, 0-1.");
  vProd.add ("ExfoliationFac", 1.0);
  Prod.add ("GrowthRateRedFac", Syntax::None (), Syntax::Const,
	    "Growth rate reduction factor, 0-1.");
  vProd.add ("GrowthRateRedFac", 0.0);
  Prod.add ("LfDR", "DS", " d^-1", Syntax::Const,
	    "Death rate of Leafs.");
  Prod.add ("RtDR", "DS", " d^-1", Syntax::Const,
	    "Death rate of Roots.");
  Prod.add ("Large_RtDR", "d^-1", Syntax::Const,
	    "Extra death rate for large root/shoot.");
  vProd.add ("Large_RtDR", 0.05);

  // CrpNPar
  CrpN.add ("SeedN", "g N/m^2", Syntax::Const,
	    "N-content in seed.");
  CrpN.add ("DS_fixate", Syntax::None (), Syntax::Const,
            "DS at which to start fixation of atmospheric N.");
  CrpNList.add ("DS_fixate", 42000.0);
  CrpN.add ("DS_cut_fixate", Syntax::None (), Syntax::Const,
	    "Restore fixation this DS after cut.");
  CrpNList.add ("DS_cut_fixate", 0.0);
  CrpN.add ("fixate_factor", Syntax::None (), Syntax::Const,
	    "Fraction of needed N fixated by day.");
  CrpNList.add ("fixate_factor", 0.8);
  CrpN.add ("PtLeafCnc", "DS", " g N/g DM", Syntax::Const,
	    "Upper limit for N-concentration in leaves.");
  CrpN.add ("CrLeafCnc", "DS", " g N/g DM", Syntax::Const,
	    "Critical limit for N-concentration in leaves.");
  CrpN.add ("NfLeafCnc", "DS", " g N/g DM", Syntax::Const, "\
Non-functional limit for N-concentration in leaves.");
  CrpN.add ("PtStemCnc", "DS", " g N/g DM", Syntax::Const,
	    "Upper limit for N-concentration in stem.");
  CrpN.add ("CrStemCnc", "DS", " g N/g DM", Syntax::Const,
	    "Critical limit for N-concentration in stem.");
  CrpN.add ("NfStemCnc", "DS", " g N/g DM", Syntax::Const, "\
Non-functional limit for N-concentration in stem.");
  CrpN.add ("PtSOrgCnc", "DS", " g N/g DM", Syntax::Const, "\
Upper limit for N-concentration in storage organ.");
  CrpN.add ("CrSOrgCnc", "DS", " g N/g DM", Syntax::Const, "\
Critical limit for N-concentration in storage organ.");
  CrpN.add ("NfSOrgCnc", "DS", " g N/g DM", Syntax::Const, "\
Non-functional limit for N-concentration in storage organ.");
  CrpN.add ("PtRootCnc", "DS", " g N/g DM", Syntax::Const,
	    "Upper limit for N-concentration in roots.");
  CrpN.add ("CrRootCnc", "DS", " g N/g DM", Syntax::Const,
	    "Critical limit for N-concentration in roots.");
  CrpN.add ("NfRootCnc", "DS", " g N/g DM", Syntax::Const, "\
Non-functional lim for N-concentration in roots.");
  CrpN.add ("TLLeafEff", "DS", Syntax::Fraction (), Syntax::Const,
	    "Translocation effiency, Leaf.");
  PLF TLLeafEff;
  TLLeafEff.add (0.00, 0.90);
  TLLeafEff.add (2.00, 0.90);
  CrpNList.add ("TLLeafEff", TLLeafEff);
  CrpN.add ("TLRootEff", "DS", Syntax::Fraction (), Syntax::Const,
	    "Translocation effiency, Root.");
  PLF TLRootEff;
  TLRootEff.add (0.00, 0.10);
  TLRootEff.add (2.00, 0.10);
  CrpNList.add ("TLRootEff", TLRootEff);

  // HarvestPar
  add_submodule_sequence<OM> ("Stem", Harvest, Syntax::Const,
			      "Stem AM parameters.");
  HarvestList.add ("Stem", AM::default_AOM ());
  add_submodule_sequence<OM> ("Leaf", Harvest, Syntax::Const,
			      "Leaf AM parameters.");
  HarvestList.add ("Leaf", AM::default_AOM ());
  add_submodule_sequence<OM> ("Dead", Harvest, Syntax::Const,
			      "Dead leaves AM parameters.");
  HarvestList.add ("Dead", AM::default_AOM ());
  add_submodule_sequence<OM> ("SOrg", Harvest, Syntax::Const,
			      "Storage organ AM parameters.");
  HarvestList.add ("SOrg", AM::default_AOM ());
  add_submodule_sequence<OM> ("Root", Harvest, Syntax::Const,
			      "Root AM parameters.");
  HarvestList.add ("Root", AM::default_AOM ());
  Harvest.add ("EconomicYield_W", Syntax::None (), Syntax::Const, "\
Valuable fraction of storage organ (DM), e.g. grain or tuber.");
  HarvestList.add ("EconomicYield_W", 1.00);
  Harvest.add ("EconomicYield_N", Syntax::None (), Syntax::OptionalConst,
               "Valuable fraction of storage organ (N).\n\
By default the value for DM is used.");
  Harvest.add ("C_Stem", Syntax::None (), Syntax::Const,
	       "C fraction of total weight.");
  HarvestList.add ("C_Stem", 0.420);
  Harvest.add ("C_Leaf", Syntax::None (), Syntax::Const,
	       "C fraction of total weight.");
  HarvestList.add ("C_Leaf", 0.420);
  Harvest.add ("C_Dead", Syntax::None (), Syntax::Const,
	       "C fraction of total weight.");
  HarvestList.add ("C_Dead", 0.420);
  Harvest.add ("C_SOrg", Syntax::None (), Syntax::Const,
	       "C fraction of total weight.");
  HarvestList.add ("C_SOrg", 0.420);
  Harvest.add ("C_Root", Syntax::None (), Syntax::Const,
	       "C fraction of total weight.");
  HarvestList.add ("C_Root", 0.420);
  Harvest.add ("DSmax", Syntax::None (), Syntax::Const, "\
Maximal development stage for which the crop survives harvest.");
  HarvestList.add ("DSmax", 0.80);
  Harvest.add ("DSnew", Syntax::None (), Syntax::Const,
	       "New development stage after harvest.");
  HarvestList.add ("DSnew", 0.20);

  // Variables.

  // Phenology
  Phenology.add ("DS", Syntax::None (), Syntax::State,
		 "Development Stage.");
  vPhenology.add ("DS", -1.0);
  Phenology.add ("Vern", "dg C d", Syntax::OptionalState,
		 "Vernalization criterium.");
  Phenology.add ("partial_day_length", "h", Syntax::State,
		 "Number of light hours this day, so far.");
  vPhenology.add ("partial_day_length", 0.0);
  Phenology.add ("day_length", "h", Syntax::State,
		 "Number of light hours yesterday.");
  vPhenology.add ("day_length", 0.0);
  Phenology.add ("partial_soil_temperature", "dg C h", Syntax::State,
		 "Soil temperature hours this day, so far.");
  vPhenology.add ("partial_soil_temperature", 0.0);
  Phenology.add ("soil_temperature", "dg C", Syntax::State,
		 "Average soil temperature yesterday.");
  vPhenology.add ("soil_temperature", 0.0);
  Phenology.add ("soil_h", "cm", Syntax::State,
		 "Soil pressure potential.");
  vPhenology.add ("soil_h", -100.0);

  // Prod
  // Warning: Uses same syntax as 'ProdPar'.
  Prod.add ("CH2OPool", "g DM/m^2", Syntax::State, "CH2O Pool.");
  vProd.add ("CH2OPool", 0.001);
  Prod.add ("WLeaf", "g DM/m^2", Syntax::State, "Leaf dry matter weight.");
  vProd.add ("WLeaf", 0.001);
  Prod.add ("WStem", "g DM/m^2", Syntax::State, "Stem dry matter weight.");
  vProd.add ("WStem", 0.000);
  Prod.add ("WRoot", "g DM/m^2", Syntax::State, "Root dry matter weight.");
  vProd.add ("WRoot", 0.001);
  Prod.add ("WSOrg", "g DM/m^2", Syntax::State,
	    "Storage organ dry matter weight.");
  vProd.add ("WSOrg", 0.000);
  Prod.add ("WDead", "g DM/m^2", Syntax::State,
	    "Dead leaves dry matter weight.");
  vProd.add ("WDead", 0.000);
  Prod.add ("NLeaf", "g N/m^2", Syntax::State,
	    "Nitrogen stored in the leaves.");
  vProd.add ("NLeaf", 0.000);
  Prod.add ("NStem", "g N/m^2", Syntax::State,
	    "Nitrogen stored in the stem.");
  vProd.add ("NStem", 0.000);
  Prod.add ("NRoot", "g N/m^2", Syntax::State,
	    "Nitrogen stored in the roots.");
  vProd.add ("NRoot", 0.000);
  Prod.add ("NSOrg", "g N/m^2", Syntax::State,
	    "Nitrogen stored in the storage organ.");
  vProd.add ("NSOrg", 0.000);
  Prod.add ("NDead", "g N/m^2", Syntax::State,
	    "Nitrogen stored in dead leaves.");
  vProd.add ("NDead", 0.000);
  Prod.add ("NCrop", "g N/m^2", Syntax::OptionalState,
	    "Total crop nitrogen content.");
  Prod.add ("C_AM", "g C/m^2", Syntax::State,
	    "Added C in plant material.");
  vProd.add ("C_AM", 0.000);
  Prod.add ("N_AM", "g N/m^2", Syntax::State,
	    "Added N in plant material.");
  vProd.add ("N_AM", 0.000);

  // CrpAux
  CrpAux.add ("StemRes", "g DM/m^2", Syntax::State,
	      "Shielded reserves in stems.");
  vCrpAux.add ("StemRes", 0.0);
  CrpAux.add ("PtNCnt", "g/m^2", Syntax::LogOnly,
	      "Potential nitrogen content in crop.");
  CrpAux.add ("CrNCnt", "g/m^2", Syntax::LogOnly,
	      "Critical nitrogen content in crop.");
  CrpAux.add ("NfNCnt", "g/m^2", Syntax::LogOnly,
	      "Non-functional nitrogen content in crop.");
  CrpAux.add ("PotTransp", "mm/h", Syntax::State,
	      "Potential transpiration.");
  vCrpAux.add ("PotTransp", 0.0);
  CrpAux.add ("PotCanopyAss", "g CH2O/m^2", Syntax::State,
	      "Potential canopy assimilation this day until now.");
  vCrpAux.add ("PotCanopyAss", 0.0);
  CrpAux.add ("CanopyAss", "g CH2O/m^2", Syntax::State,
	      "Canopy assimilation this day until now.");
  vCrpAux.add ("CanopyAss", 0.0);
  CrpAux.add ("NetPhotosynthesis", "g CO2/m^2/h", Syntax::LogOnly,
	      "Net Photosynthesis.");
  CrpAux.add ("RootRespiration", "g CO2/m^2/h", Syntax::LogOnly,
	      "Root Respiration.");
  CrpAux.add ("IncWLeaf", "g DM/m^2/d", Syntax::LogOnly,
	      "Leaf growth.");
  CrpAux.add ("IncWStem", "g DM/m^2/d", Syntax::LogOnly,
	      "Stem growth.");
  CrpAux.add ("IncWSOrg", "g DM/m^2/d", Syntax::LogOnly,
	      "Storage organ growth.");
  CrpAux.add ("IncWRoot", "g DM/m^2/d", Syntax::LogOnly,
	      "Root growth.");
  CrpAux.add ("DeadWLeaf", "g DM/m^2/d", Syntax::LogOnly,
	      "Leaf DM removed.");
  CrpAux.add ("DeadNLeaf", "g N/m2/d", Syntax::LogOnly,
	      "Leaf N removed.");
  CrpAux.add ("DeadWRoot", "g DM/m^2/d", Syntax::LogOnly,
	      "Root DM removed.");
  CrpAux.add ("DeadNRoot", "g N/m2/d", Syntax::LogOnly,
	      "Root N removed.");
  CrpAux.add ("Fixated", "g N/m^2/h", Syntax::LogOnly, 
	      "N fixation from air.");
  CrpAux.add ("AccFixated", "g N/m^2", Syntax::LogOnly, 
	      "Accumuated N fixation from air.");
  vCrpAux.add ("AccFixated", 0.0);
  CrpAux.add ("DS_start_fixate", Syntax::None (), Syntax::OptionalState,
	      "Development stage at which to restart fixation after a cut.");

  // Model.
  Syntax& syntax = *new Syntax ();
  AttributeList& alist = *new AttributeList ();

  syntax.add ("description", Syntax::String, Syntax::OptionalConst,
	      "Description of this parameterization."); 
  alist.add ("description", "Standard Daisy crop model.  Hansen, 1999.");
  syntax.add ("enable_water_stress", Syntax::Boolean, Syntax::Const,
	      "Set this to true to let water stress limit production.");
  alist.add ("enable_water_stress", true);
  syntax.add ("enable_N_stress", Syntax::Boolean, Syntax::Const,
	      "Set this true to let nitrogen stress limit production.");
  alist.add ("enable_N_stress", true);

  // Submodels.
  add_submodule<RootSystem>("Root", syntax, alist,
			    Syntax::State, "Root system.");
  add_submodule<CanopyStandard>("Canopy", syntax, alist,
				Syntax::State, "Canopy.");

  syntax.add ("Devel", Devel, "Crop development.");
  alist.add ("Devel", vDevel);
  syntax.add ("Vernal", Vernal, Syntax::OptionalConst, Syntax::Singleton,
	      "Vernalization.");
  syntax.add ("LeafPhot", LeafPhot, "Leaf photosynthesis.");
  syntax.add ("Partit", Partit, "Assimilate partitioning.");
  syntax.add ("CrpN", CrpN, "Nitrogen content in the crop.");
  alist.add ("CrpN", CrpNList);
  syntax.add ("Harvest", Harvest, "Harvest parameters.");
  alist.add ("Harvest", HarvestList);
  syntax.add ("Phenology", Phenology, "Crop development.");
  alist.add ("Phenology", vPhenology);
  syntax.add ("Prod", Prod, "Crop production.");
  alist.add ("Prod", vProd);
  syntax.add ("CrpAux", CrpAux, "Miscellaneous crop state variables.");
  alist.add ("CrpAux", vCrpAux);

  Librarian<Crop>::add_type ("default", alist, syntax, &make);
}

void
CropStandard::Vernalization (double Ta)
{
  const Parameters::VernalPar& Vernal = par.Vernal;
  double& Vern = var.Phenology.Vern;
  double& DS = var.Phenology.DS;

  Vern -= min (Ta - Vernal.TaLim, 0.0);
  if (DS > Vernal.DSLim)
    DS = Vernal.DSLim;
}

void
CropStandard::Emergence ()
{
  const Parameters::DevelPar& Devel = par.Devel;
  double& DS = var.Phenology.DS;
  const double h = var.Phenology.soil_h;

  DS += var.Phenology.soil_temperature / Devel.EmrTSum * Devel.EmrSMF (h);
  if (DS > 0)
    DS = Devel.DS_Emr;
}

void
CropStandard::DevelopmentStage (const Bioclimate& bioclimate)
{
  const Parameters::DevelPar& Devel = par.Devel;
  Variables::RecPhenology& Phenology = var.Phenology;

  const double Ta = bioclimate.daily_air_temperature ();

  if (Phenology.DS < 1.0)
    {
      // Only increase DS if assimilate production covers leaf respiration.
      if (var.CrpAux.IncWLeaf +  var.CrpAux.DeadWLeaf
	  >  -var.Prod.WLeaf /1000.0) // It lost 0.1% of its leafs to resp.
	Phenology.DS += (Devel.DSRate1
			 * Devel.TempEff1 (Ta)
			 * Devel.PhotEff1 (var.Phenology.day_length + 1.0));
      if (par.Vernal.required && Phenology.Vern < 0)
	Vernalization (Ta);
      if (Phenology.DS >= 1.0)
	COUT << " [" << name << " is flowering]\n";
    }
  else
    {
      Phenology.DS += Devel.DSRate2 * Devel.TempEff2 (Ta);
      if (Phenology.DS > 2)
	{
	  COUT << " [" << name << " is ripe]\n";
	  Phenology.DS = 2.0;
	  NoProduction ();
	}
    }

  assert (Phenology.DS <= Devel.defined_until_ds);
}

void 
CropStandard::CanopyStructure ()
{ canopy.CanopyStructure (var.Phenology.DS); }

double
CropStandard::ActualWaterUptake (double Ept,
				 const Soil& soil, SoilWater& soil_water,
				 const double EvapInterception)
{
  return root_system.water_uptake (Ept, soil, soil_water, EvapInterception);
}

void 
CropStandard::force_production_stress  (double pstress)
{ root_system.production_stress = pstress; }

void
CropStandard::NitContent ()
{
  const Parameters::CrpNPar& CrpN = par.CrpN;
  const double DS = var.Phenology.DS;
  Variables::RecProd& Prod = var.Prod;

  var.CrpAux.PtNCnt = CrpN.PtLeafCnc (DS) * Prod.WLeaf
    + CrpN.PtStemCnc (DS) * Prod.WStem
    + CrpN.PtSOrgCnc (DS) * Prod.WSOrg
    + CrpN.PtRootCnc (DS) * Prod.WRoot;

  var.CrpAux.CrNCnt = CrpN.CrLeafCnc (DS) * Prod.WLeaf
    + CrpN.CrStemCnc (DS) * Prod.WStem
    + CrpN.CrSOrgCnc (DS) * Prod.WSOrg
    + CrpN.CrRootCnc (DS) * Prod.WRoot;

  var.CrpAux.NfNCnt = CrpN.NfLeafCnc (DS) * Prod.WLeaf
    + CrpN.NfStemCnc (DS) * Prod.WStem
    + CrpN.NfSOrgCnc (DS) * Prod.WSOrg
    + CrpN.NfRootCnc (DS) * Prod.WRoot;

  if (Prod.NCrop >= var.CrpAux.CrNCnt)
    {
      const double x = (Prod.NCrop - var.CrpAux.CrNCnt)
        / (var.CrpAux.PtNCnt - var.CrpAux.CrNCnt);
      Prod.NLeaf = ((CrpN.PtLeafCnc (DS) - CrpN.CrLeafCnc (DS)) * x
        + CrpN.CrLeafCnc (DS)) * Prod.WLeaf;
      Prod.NStem = ((CrpN.PtStemCnc (DS) - CrpN.CrStemCnc (DS)) * x
        + CrpN.CrStemCnc (DS)) * Prod.WStem;
      Prod.NSOrg = ((CrpN.PtSOrgCnc (DS) - CrpN.CrSOrgCnc (DS)) * x
        + CrpN.CrSOrgCnc (DS)) * Prod.WSOrg;
      Prod.NRoot = Prod.NCrop - Prod.NLeaf - Prod.NStem - Prod.NSOrg;
    }
  else
    {
      const double x = (Prod.NCrop - var.CrpAux.CrNCnt)
        / (var.CrpAux.NfNCnt - var.CrpAux.CrNCnt);
      Prod.NLeaf = ((CrpN.NfLeafCnc (DS) - CrpN.CrLeafCnc (DS)) * x
        + CrpN.CrLeafCnc (DS)) * Prod.WLeaf;
      Prod.NStem = ((CrpN.NfStemCnc (DS) - CrpN.CrStemCnc (DS)) * x
        + CrpN.CrStemCnc (DS)) * Prod.WStem;
      Prod.NSOrg = ((CrpN.NfSOrgCnc (DS) - CrpN.CrSOrgCnc (DS)) * x
        + CrpN.CrSOrgCnc (DS)) * Prod.WSOrg;
      Prod.NRoot = Prod.NCrop - Prod.NLeaf - Prod.NStem - Prod.NSOrg;
    }
  assert (Prod.NLeaf >= 0.0);
  assert (Prod.NStem >= 0.0);
  assert (Prod.NSOrg >= 0.0);
  assert (Prod.NRoot >= 0.0);
}

void
CropStandard::NitrogenUptake (int Hour,
			      const Soil& soil,
			      const SoilWater& soil_water,
			      SoilNH4& soil_NH4,
			      SoilNO3& soil_NO3)
{
  double& NCrop = var.Prod.NCrop;
  double PotNUpt = (var.CrpAux.PtNCnt - NCrop) / ((Hour == 0) ? 1.0 : (25.0 - Hour));

  const double NUpt = root_system.nitrogen_uptake (soil, soil_water, 
						   soil_NH4, soil_NO3,
						   PotNUpt);
  NCrop += NUpt;
  PotNUpt -= NUpt;

  if (PotNUpt > 0 && var.Phenology.DS > var.CrpAux.DS_start_fixate)
    {
      var.CrpAux.Fixated = par.CrpN.fixate_factor * PotNUpt;
      var.CrpAux.AccFixated += var.CrpAux.Fixated;
      NCrop += var.CrpAux.Fixated;
      // PotNUpt -= var.CrpAux.Fixated;
    }
  else
    var.CrpAux.Fixated = 0.0;

  // Updating the nitrogen stress
  root_system.nitrogen_stress
    = 1.0 - bound (0.0, ((NCrop - var.CrpAux.NfNCnt)
			 / (var.CrpAux.CrNCnt - var.CrpAux.NfNCnt)),
		   1.0);
  
  // Ensure we have enough N for all the crop parts.
  if (!par.enable_N_stress)
    NCrop = max (NCrop, var.CrpAux.CrNCnt);
}

double
CropStandard::CanopyPhotosynthesis (const Bioclimate& bioclimate)
{
  // sugar production [gCH2O/m2/h] by canopy photosynthesis.
  const Parameters::LeafPhotPar& LeafPhot = par.LeafPhot;
  const PLF& LAIvsH = canopy.LAIvsH;
  const double Ta = bioclimate.daily_air_temperature ();
  const double Teff = LeafPhot.TempEff (Ta); // Temperature effect

  // One crop: assert (approximate (canopy.CAI, bioclimate.CAI ()));
  if (!approximate (LAIvsH (canopy.Height), canopy.CAI))
    {
      CERR << "Bug: CAI below top: " << LAIvsH (canopy.Height)
	   << " Total CAI: " << canopy.CAI << "\n";
      CanopyStructure ();
      CERR << "Adjusted: CAI below top: " << LAIvsH (canopy.Height)
	   << " Total CAI: " << canopy.CAI << "\n";
    }

 // CAI below the current leaf layer.
  double prevLA = LAIvsH (bioclimate.height (0));
  // Assimilate produced by canopy photosynthesis
  double Ass = 0.0;
  // Accumulated CAI, for testing purposes.
  double accCAI =0.0;
  // Number of computational intervals in the canopy.
  const int No = bioclimate.NumberOfIntervals ();
  // CAI in each interval.
  const double dCAI = bioclimate.LAI () / No;

  // True, if we haven't reached the top of the crop yet.
  bool top_crop = true;

  for (int i = 0; i < No; i++)
    {
      const double height = bioclimate.height (i+1);
      assert (height < bioclimate.height (i));

      if (top_crop && height <= canopy.Height)
	{
	  // We count day hours at the top of the crop.
	  top_crop = false;
	  if (bioclimate.PAR (i) > 0.5 * 25.0)
	    var.Phenology.partial_day_length += 1.0;
	}
      // Leaf Area index for a given leaf layer
      const double LA = prevLA - LAIvsH (height);
      assert (LA >= 0.0);
      if (LA > 0)
	{
	  prevLA = LAIvsH (height);
	  accCAI += LA;

	  const double dPAR
	    = (bioclimate.PAR (i) - bioclimate.PAR (i + 1)) / dCAI;

	  // Leaf Photosynthesis [gCO2/m2/h]
	  const double F = LeafPhot.Fm *
	    (1.0 - exp (- (LeafPhot.Qeff * dPAR / LeafPhot.Fm)));

	  Ass += LA * F;
	}
    }
  assert (approximate (accCAI, canopy.CAI));

  return (molWeightCH2O / molWeightCO2) * Teff * Ass;
}

double
CropStandard::ReMobilization ()
{
  const Parameters::ProdPar& Prod = par.Prod;
  const double DS = var.Phenology.DS;
  const double WStem = var.Prod.WStem;
  double& StemRes = var.CrpAux.StemRes;

  if (DS < Prod.ReMobilDS)
    {
      StemRes = 0.0;
      return 0.0;
    }
  else if (StemRes < 1.0e-9)
    {
      StemRes = Prod.ShldResC * WStem;
      return 0.0;
    }
  else
    {
      const double ReMobilization = Prod.ReMobilRt / 24. * StemRes;
      StemRes -= ReMobilization;
      return ReMobilization;
    }
}

void
CropStandard::AssimilatePartitioning (double DS,
				      double& f_Leaf, double& f_Stem,
				      double& f_Root, double& f_SOrg)
{
  const Parameters::PartitPar& Partit = par.Partit;

  if (RSR () > Partit.RSR (DS))
    f_Root = 0.0;
  else
    f_Root = Partit.Root (DS);
  f_Leaf = (1 - f_Root) * Partit.Leaf (DS);
  f_Stem = (1 - f_Root) * Partit.Stem (DS);
  f_SOrg = max (0.0, 1 - f_Root - f_Leaf - f_Stem);
  if (f_SOrg < 1e-5)
    {
      f_Root += f_SOrg;
      f_SOrg = 0.0;
    }
}

double
CropStandard::MaintenanceRespiration (double r, double w, double T)
{
  if (w <= 0.0)
    return 0.0;

  return (molWeightCH2O / molWeightCO2)
    * r / 24. * max (0.0, 0.4281 * (exp (0.57 - 0.024 * T + 0.0020 * T * T)
			      - exp (0.57 - 0.042 * T - 0.0051 * T * T))) * w;
}

void
CropStandard::NetProduction (const Bioclimate& bioclimate,
			     const Geometry& geometry,
			     const SoilHeat& soil_heat)
{
  // Remobilization
  const double ReMobil = ReMobilization ();
  var.Prod.CH2OPool += ReMobil;


  const Parameters::ProdPar& pProd = par.Prod;
  const double DS = var.Phenology.DS;
  const double Depth = root_system.Depth;
  Variables::RecProd& vProd = var.Prod;
  Variables::RecCrpAux& CrpAux = var.CrpAux;
  double NetAss = CrpAux.CanopyAss;

  const double AirT = bioclimate.daily_air_temperature ();
  const double SoilT = soil_heat.T (geometry.interval_plus (-Depth / 3));
  double RMLeaf
    = MaintenanceRespiration (pProd.r_Leaf, vProd.WLeaf, AirT);
  const double RMStem
    = MaintenanceRespiration (pProd.r_Stem, vProd.WStem, AirT);
  const double RMSOrg
    = MaintenanceRespiration (pProd.r_SOrg, vProd.WSOrg, AirT);
  const double RMRoot
    = MaintenanceRespiration (pProd.r_Root, vProd.WRoot, SoilT);

  RMLeaf = max (0.0, RMLeaf - CrpAux.PotCanopyAss + CrpAux.CanopyAss);
  const double RM = RMLeaf + RMStem + RMSOrg + RMRoot;
  NetAss -= RM;
  double RootResp = RMRoot;

  if (var.Prod.CH2OPool >= RM)
    {
      var.Prod.CH2OPool -= RM;
      double f_Leaf, f_Stem, f_SOrg, f_Root;
      AssimilatePartitioning (DS, f_Leaf, f_Stem, f_Root, f_SOrg);
      const double AssG = pProd.CH2OReleaseRate * var.Prod.CH2OPool;
      CrpAux.IncWLeaf = pProd.E_Leaf * f_Leaf * AssG;
      CrpAux.IncWStem = pProd.E_Stem * f_Stem * AssG - ReMobil;
      CrpAux.IncWSOrg = pProd.E_SOrg * f_SOrg * AssG;
      CrpAux.IncWRoot = pProd.E_Root * f_Root * AssG;
      var.Prod.CH2OPool -= AssG;
      NetAss -= (1.0 - pProd.E_Leaf) * f_Leaf * AssG
        + (1.0 - pProd.E_Stem) * f_Stem * AssG
        + (1.0 - pProd.E_SOrg) * f_SOrg * AssG
        + (1.0 - pProd.E_Root) * f_Root * AssG;
      RootResp += (1.0 - pProd.E_Root) * f_Root * AssG;
    }
  else
    {
      if (RMLeaf <= var.Prod.CH2OPool)
	{
	  CrpAux.IncWLeaf = 0.0;
	  var.Prod.CH2OPool -= RMLeaf;
	}
      else
	{
	  CrpAux.IncWLeaf = var.Prod.CH2OPool - RMLeaf;
	  var.Prod.CH2OPool = 0.0;
	}
      if (RMSOrg <= var.Prod.CH2OPool)
	{
	  CrpAux.IncWSOrg = 0.0;
	  var.Prod.CH2OPool -= RMSOrg;
	}
      else
	{
	  CrpAux.IncWSOrg = var.Prod.CH2OPool - RMSOrg - ReMobil;
	  var.Prod.CH2OPool = 0.0;
	}
      if (RMStem <= var.Prod.CH2OPool)
	{
	  CrpAux.IncWStem = -ReMobil;
	  var.Prod.CH2OPool -= RMStem;
	}
      else
	{
	  CrpAux.IncWStem = var.Prod.CH2OPool - RMStem - ReMobil;
          if (vProd.WStem + CrpAux.IncWStem + CrpAux.IncWSOrg >= 0.0
	      && vProd.WStem > vProd.WSOrg)
            {
	      CrpAux.IncWStem += CrpAux.IncWSOrg;
	      CrpAux.IncWSOrg  = 0.0;
            }
	  var.Prod.CH2OPool = 0.0;
	}
      if (RMRoot <= var.Prod.CH2OPool)
	{
	  CrpAux.IncWRoot = 0.0;
	  var.Prod.CH2OPool -= RMRoot;
	}
      else
	{
	  CrpAux.IncWRoot = var.Prod.CH2OPool - RMRoot;
	  var.Prod.CH2OPool = 0.0;
	}
      if (var.Prod.CH2OPool > 0.0)
	CERR << "BUG: Extra CH2O: " << var.Prod.CH2OPool << "\n";
    }
  CrpAux.NetPhotosynthesis = molWeightCO2 / molWeightCH2O * NetAss;
  CrpAux.RootRespiration = molWeightCO2 / molWeightCH2O * RootResp;

  // Update dead leafs
  CrpAux.DeadWLeaf = pProd.LfDR (DS) / 24.0 * vProd.WLeaf;
  CrpAux.DeadWLeaf += vProd.WLeaf * 0.333 * canopy.CAImRat / 24.0;
  assert (CrpAux.DeadWLeaf >= 0.0);
  double DdLeafCnc;
  assert (vProd.WLeaf > 0.0);
  if (vProd.NCrop > 1.05 * CrpAux.PtNCnt)
    DdLeafCnc = vProd.NLeaf/vProd.WLeaf;
  else
    DdLeafCnc = (vProd.NLeaf/vProd.WLeaf - par.CrpN.NfLeafCnc (DS))
      * ( 1.0 - par.CrpN.TLLeafEff (DS)) +  par.CrpN.NfLeafCnc (DS);
  assert (DdLeafCnc >= 0.0);
  assert (CrpAux.DeadWLeaf >= 0.0);
  CrpAux.DeadNLeaf = DdLeafCnc * CrpAux.DeadWLeaf;
  assert (CrpAux.DeadNLeaf >= 0.0);
  CrpAux.IncWLeaf -= CrpAux.DeadWLeaf;
  assert (CrpAux.DeadWLeaf >= 0.0);
  vProd.WDead += (1.0 - par.Prod.ExfoliationFac) * CrpAux.DeadWLeaf;
  vProd.NDead += (1.0 - par.Prod.ExfoliationFac) * CrpAux.DeadNLeaf;
  assert (vProd.NDead >= 0.0);

  const double C_foli = par.Harvest.C_Dead *
                        par.Prod.ExfoliationFac * CrpAux.DeadWLeaf;
  const double N_foli = par.Prod.ExfoliationFac * CrpAux.DeadNLeaf;
  assert (N_foli >= 0.0);
  if (C_foli < 1e-50)
    assert (N_foli < 1e-40);
  else
    {
      assert (N_foli > 0.0);
      vProd.AM_leaf->add ( C_foli * m2_per_cm2, N_foli * m2_per_cm2);
      vProd.C_AM += C_foli;
      vProd.N_AM += N_foli;
    }

  // Update dead roots.
  double RtDR = pProd.RtDR (DS);
  if (RSR () > par.Partit.RSR (DS))
    RtDR += pProd.Large_RtDR;

  CrpAux.DeadWRoot = RtDR / 24.0 * vProd.WRoot;
  double DdRootCnc;
  if (vProd.NCrop > 1.05 * CrpAux.PtNCnt)
    DdRootCnc = vProd.NRoot/vProd.WRoot;
  else
    DdRootCnc = (vProd.NRoot/vProd.WRoot - par.CrpN.NfRootCnc (DS))
      * ( 1.0 - par.CrpN.TLRootEff (DS)) +  par.CrpN.NfRootCnc (DS);
  CrpAux.DeadNRoot = DdRootCnc * CrpAux.DeadWRoot;
  CrpAux.IncWRoot -= CrpAux.DeadWRoot;
  const double C_Root = par.Harvest.C_Root * CrpAux.DeadWRoot;
  vProd.AM_root->add (geometry, C_Root * m2_per_cm2,
		      CrpAux.DeadNRoot * m2_per_cm2,
		      root_system.Density);
  assert (C_Root == 0.0 || CrpAux.DeadNRoot > 0.0);
  vProd.C_AM += C_Root;
  vProd.N_AM += CrpAux.DeadNRoot;

  // Update production.
  vProd.NCrop -= (CrpAux.DeadNLeaf + CrpAux.DeadNRoot);
  assert (vProd.NCrop > 0.0);
  vProd.WLeaf += CrpAux.IncWLeaf;
  vProd.WStem += CrpAux.IncWStem;
  vProd.WSOrg += CrpAux.IncWSOrg;
  vProd.WRoot += CrpAux.IncWRoot;
}

void
CropStandard::NoProduction ()
{
  var.CrpAux.IncWLeaf = 0.0;
  var.CrpAux.IncWStem = 0.0;
  var.CrpAux.IncWSOrg = 0.0;
  var.CrpAux.IncWRoot = 0.0;
  var.CrpAux.NetPhotosynthesis = 0.0;
  var.CrpAux.RootRespiration = 0.0;
  var.CrpAux.DeadWLeaf = 0.0;
  var.CrpAux.DeadNLeaf = 0.0;
  var.CrpAux.DeadWRoot = 0.0;
  var.CrpAux.DeadNRoot = 0.0;
}

double
CropStandard::RSR () const
{
  const double shoot = var.Prod.WStem + var.Prod.WSOrg + var.Prod.WLeaf;
  const double root = var.Prod.WRoot;
  if (shoot < 20.0 || root < 20.0)
    return 0.33333;
  return root/shoot;
}

void
CropStandard::tick (const Time& time,
		    const Bioclimate& bioclimate,
		    const Soil& soil,
		    OrganicMatter* organic_matter,
		    const SoilHeat& soil_heat,
		    const SoilWater& soil_water,
		    SoilNH4* soil_NH4,
		    SoilNO3* soil_NO3)
{
  // Update partial_soil_temperature and pressure potential.
  var.Phenology.partial_soil_temperature +=
    soil_heat.T (soil.interval_plus (-root_system.DptEmr));
  var.Phenology.soil_h =
    soil_water.h (soil.interval_plus (-root_system.DptEmr/2.));

  if (time.hour () == 0 && var.Phenology.DS <= 0)
    {
      // Calculate average soil temperature.
      var.Phenology.soil_temperature =
	var.Phenology.partial_soil_temperature / 24.0;
      var.Phenology.partial_soil_temperature = 0.0;

      Emergence ();
      if (var.Phenology.DS >= 0)
	{
	  COUT << " [" << name << " is emerging]\n";

	  canopy.tick (var.Prod.WLeaf, var.Prod.WSOrg,
		       var.Prod.WStem, var.Phenology.DS);
	  NitContent ();
	  root_system.tick (soil, soil_heat, var.Prod.WRoot, 0.0);

	  if (organic_matter)
	    {
	      if (!var.Prod.AM_root)
		var.Prod.AM_root
		  = &AM::create (soil, time, par.Harvest.Root,
				 name, "root", AM::Locked);
	      if (!var.Prod.AM_leaf)
		var.Prod.AM_leaf
		  = &AM::create (soil, time, par.Harvest.Dead,
				 name, "dead", AM::Locked);

	      organic_matter->add (*var.Prod.AM_root);
	      organic_matter->add (*var.Prod.AM_leaf);
	    }
	  else
	    {
	      if (!var.Prod.AM_root)
		var.Prod.AM_root
		  = &AM::create (soil, time, par.Harvest.Root,
				 name, "root", AM::Unlocked);
	      if (!var.Prod.AM_leaf)
		var.Prod.AM_leaf
		  = &AM::create (soil, time, par.Harvest.Dead,
				 name, "dead", AM::Unlocked);
	    }
	}
      return;
    }
  if (var.Phenology.DS <= 0 || var.Phenology.DS >= 2)
    return;

  if (soil_NO3)
    {
      assert (soil_NH4);
      NitrogenUptake (time.hour (),
		      soil, soil_water, *soil_NH4, *soil_NO3);
    }
  else
    {
      assert (!soil_NH4);
      var.Prod.NCrop = var.CrpAux.PtNCnt;
    }  
  const double nitrogen_stress = root_system.nitrogen_stress;
  const double water_stress = root_system.water_stress;

  if (bioclimate.PAR (bioclimate.NumberOfIntervals () - 1) > 0)
    {
      double Ass = CanopyPhotosynthesis (bioclimate);
      var.CrpAux.PotCanopyAss = Ass;
      if (root_system.production_stress >= 0.0)
	Ass *= (1.0 - root_system.production_stress);
      else if (par.enable_water_stress)
	Ass *= (1.0 - water_stress);
      if (par.enable_N_stress)
	Ass *= (1.0 - nitrogen_stress);
      var.CrpAux.CanopyAss = Ass;
      const double ProdLim = (1.0 - par.Prod.GrowthRateRedFac);
      var.Prod.CH2OPool += ProdLim * Ass;
    }
  NetProduction (bioclimate, soil, soil_heat);
  NitContent ();
  if (time.hour () != 0)
    return;

  // Update final daylength.
  var.Phenology.day_length = var.Phenology.partial_day_length;
  var.Phenology.partial_day_length = 0.0;

  canopy.tick (var.Prod.WLeaf, var.Prod.WSOrg,
	       var.Prod.WStem, var.Phenology.DS);
  DevelopmentStage (bioclimate);
  root_system.tick (soil, soil_heat, var.Prod.WRoot, var.CrpAux.IncWRoot);
}

const Harvest&
CropStandard::harvest (const string& column_name,
		       const Time& time,
		       const Geometry& geometry,
		       Bioclimate& bioclimate,
		       double stub_length,
		       double stem_harvest,
		       double leaf_harvest,
		       double sorg_harvest,
		       bool kill_off,
		       vector<AM*>& residuals)
{
  NitContent();
  const Parameters::HarvestPar& Hp = par.Harvest;
  Variables::RecProd& Prod = var.Prod;
  const double DS = var.Phenology.DS;
  const double DSmax = Hp.DSmax;

  const double WStem = Prod.WStem;
  const double WLeaf = Prod.WLeaf;
  const double WSOrg = Prod.WSOrg;
  const double WRoot = Prod.WRoot;
  const double WDead = Prod.WDead;
  // Part of economic yield removed at harvest
  const double WEYRm = Hp.EconomicYield_W * sorg_harvest * WSOrg;
  // Part of economic yield left in the field at harvest
  const double WEYLf = Hp.EconomicYield_W * (1 - sorg_harvest) * WSOrg;
  // Part of non economic yield removed from the field at harvest
  const double WRsRm = (1 - Hp.EconomicYield_W) * sorg_harvest * WSOrg;
  // Part of non economic yield left in the field at harvest
  const double WRsLf = (1 - Hp.EconomicYield_W) * (1 - sorg_harvest) * WSOrg;
  const double NStem = Prod.NStem;
  const double NLeaf = Prod.NLeaf;
  const double NSOrg = Prod.NSOrg;
  const double NRoot = Prod.NRoot;
  const double NDead = Prod.NDead;
  // Part of economic yield removed at harvest
  const double NEYRm = Hp.EconomicYield_N * sorg_harvest * NSOrg;
  // Part of economic yield left in the field at harvest
  const double NEYLf = Hp.EconomicYield_N * (1 - sorg_harvest) * NSOrg;
  // Part of non economic yield removed from the field at harvest
  const double NRsRm = (1 - Hp.EconomicYield_N) * sorg_harvest * NSOrg;
  // Part of non economic yield left in the field at harvest
  const double NRsLf = (1 - Hp.EconomicYield_N) * (1 - sorg_harvest) * NSOrg;
  const double C_Stem = Hp.C_Stem;
  const double C_Dead = Hp.C_Dead;
  const double C_Leaf = Hp.C_Leaf;
  const double C_SOrg = Hp.C_SOrg;
  const double C_Root = Hp.C_Root;

  const vector<AttributeList*>& Stem = Hp.Stem;
  const vector<AttributeList*>& Dead = Hp.Dead;
  const vector<AttributeList*>& Leaf = Hp.Leaf;
  const vector<AttributeList*>& SOrg = Hp.SOrg;

  const vector<double>& density = root_system.Density;
  const double length = height ();

  Chemicals chemicals;

  // Leave stem and leaf below stub alone.

  if (stub_length < length)
    {
      stem_harvest *= (1.0 - stub_length / length);

      const double total_CAI = LAI ();
      if (total_CAI > 0.0)
	{
	  const double stub_CAI = LAIvsH ()(stub_length);
	  leaf_harvest *= (1.0 - stub_CAI / total_CAI);
	  bioclimate.harvest_chemicals (chemicals, total_CAI - stub_CAI);
	}
    }
  else
    {
      stem_harvest = 0.0;
      leaf_harvest = 0.0;
    }

  if (!kill_off && DS < DSmax && stub_length > 0.0)
    {
      // Cut back development stage and production.
      const double DSnew = Hp.DSnew;

      if (DS > DSnew)
	var.Phenology.DS = DSnew;

      // Stop fixation after cut.
      if (DS > var.CrpAux.DS_start_fixate)
	var.CrpAux.DS_start_fixate = par.CrpN.DS_cut_fixate;

      Prod.WStem *= (1.0 - stem_harvest);
      Prod.WDead *= (1.0 - stem_harvest);
      Prod.WLeaf *= (1.0 - leaf_harvest);
      Prod.WSOrg *= (1.0 - sorg_harvest);
      Prod.NCrop -= (  NStem * stem_harvest
		     + NLeaf * leaf_harvest
		     + NSOrg * sorg_harvest);

      if (DS > 0.0)
	{
	  // Adjust canopy for the sake of bioclimate.
	  canopy.Height = min (stub_length, canopy.Height);
	  canopy.Offset
	    = canopy.Height
	    - canopy.HvsDS (var.Phenology.DS) ;
	  assert (approximate (canopy.CropHeight (var.Phenology.DS),
			       canopy.Height));
	  canopy.CropCAI (var.Prod.WLeaf, var.Prod.WSOrg,
			  var.Prod.WStem, var.Phenology.DS);
	  CanopyStructure ();

	  // Residuals left in the field
	  const double C = C_SOrg * WRsRm;
	  const double N = NRsRm;
	  AM& am = AM::create (geometry, time, SOrg, name, "sorg");
	  assert (C == 0.0 || N > 0.0);
	  am.add ( C * m2_per_cm2, N * m2_per_cm2);
	  residuals.push_back (&am);
	  Prod.C_AM += C;
	  Prod.N_AM += N;
	}
    }
  else
    {
      var.Phenology.DS = DSremove;

      // Add crop remains to the soil.
      if (stem_harvest < 1.0 && WStem > 0.0)
	{
          const double C = WStem * C_Stem * (1.0 - stem_harvest);
          const double N = NStem * (1.0 - stem_harvest);
	  AM& am = AM::create (geometry, time, Stem, name, "stem");
	  am.add (C * m2_per_cm2, N * m2_per_cm2);
	  assert (C == 0.0 || N > 0.0);
	  residuals.push_back (&am);
	}
      if (stem_harvest < 1.0 && WDead > 0.0)
	{
          const double C = WDead * C_Dead * (1.0 - stem_harvest);
          const double N = NDead * (1.0 - stem_harvest);
	  if (!var.Prod.AM_leaf)
	    var.Prod.AM_leaf
	      = &AM::create (geometry, time, Dead, name, "dead", AM::Unlocked);
	  var.Prod.AM_leaf->add (C * m2_per_cm2, N * m2_per_cm2);
	  assert (C == 0.0 || N > 0.0);
	}
      if (leaf_harvest < 1.0 && WLeaf > 0.0)
	{
          const double C = WLeaf * C_Leaf * (1.0 - leaf_harvest);
          const double N = NLeaf * (1.0 - leaf_harvest);
	  AM& am = AM::create (geometry, time, Leaf, name, "leaf");
	  assert (C == 0.0 || N > 0.0);
	  am.add ( C * m2_per_cm2, N * m2_per_cm2);
	  residuals.push_back (&am);
	}
      if (sorg_harvest < 1.0 && WSOrg > 0.0)
	{
          const double C = C_SOrg * (WEYLf + WRsLf + WRsRm);
          const double N = NEYLf + NRsLf + NRsRm;
	  AM& am = AM::create (geometry, time, SOrg, name, "sorg");
	  assert (C == 0.0 || N > 0.0);
	  am.add ( C * m2_per_cm2, N * m2_per_cm2);
	  residuals.push_back (&am);
	}

      // Update and unlock locked AMs.
      if (!var.Prod.AM_root)
	var.Prod.AM_root = &AM::create (geometry, time, par.Harvest.Root,
					name, "root", AM::Unlocked);
      if (geometry.total (density) > 0.0)
	var.Prod.AM_root->add (geometry,
			       WRoot * C_Root * m2_per_cm2,
			       NRoot * m2_per_cm2,
			       density);
      else
	var.Prod.AM_root->add (WRoot * C_Root * m2_per_cm2,
			       NRoot * m2_per_cm2);
      assert (WRoot == 0.0 || NRoot > 0.0);
      if (var.Prod.AM_root->locked ())
	var.Prod.AM_root->unlock (); // Stored in organic matter.
      else
	residuals.push_back (var.Prod.AM_root);	// No organic matter.
      var.Prod.AM_root = NULL;

      if (var.Prod.AM_leaf)
	{
	  if (var.Prod.AM_leaf->locked ()) 
	    var.Prod.AM_leaf->unlock (); // Stored in organic matter.
	  else 
	    residuals.push_back (var.Prod.AM_leaf);// No organic matter.
	  var.Prod.AM_leaf = NULL;
	}
    }
  return *new Harvest (column_name, time, name,
		       WStem * stem_harvest, NStem * stem_harvest,
		       WDead * stem_harvest, NDead * stem_harvest,
		       WLeaf * leaf_harvest, NLeaf * leaf_harvest,
		       WEYRm, NEYRm, chemicals);
}

void
CropStandard::output (Log& log) const
{
  output_submodule (root_system, "Root", log);
  output_submodule (canopy, "Canopy", log);
  var.output (log);
}

double
CropStandard::DS () const
{ return var.Phenology.DS; }

double
CropStandard::DM () const	
{ 
  return (var.Prod.WSOrg + var.Prod.WStem + var.Prod.WLeaf + var.Prod.WDead)
    * 10; // [g/m^2 -> kg/ha]
}

CropStandard::CropStandard (const AttributeList& al)
  : Crop (al),
    root_system (*new RootSystem (al.alist ("Root"))),
    canopy (*new CanopyStandard (al.alist ("Canopy"))),
    par (*new Parameters (al)),
    var (*new Variables (par, al))
{ }

CropStandard::~CropStandard ()
{
  delete &root_system;
  delete &canopy;
  delete &var;
  delete &par;
}
