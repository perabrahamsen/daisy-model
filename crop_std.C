// crop_std.C

#include "crop.h"
#include "log.h"
#include "time.h"
#include "csmp.h"
#include "bioclimate.h"
#include "common.h"
#include "csmp.h"
#include "syntax.h"
#include "alist.h"
#include "filter.h"
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

class CropStandard : public Crop
{
  // Content.
public:
  struct Parameters;
  struct Variables;
  const Parameters& par;
  Variables& var;

  // Communication with Bioclimate.
public:
  double height () const;
  double LAI () const;
  const CSMP& LAIvsH () const;
  double PARext () const;
  double PARref () const;
  double EPext () const;
  double IntcpCap () const; // Interception Capacity.
  double EpFac () const; // Convertion to potential evapotransp.
  void CanopyStructure ();
  double ActualWaterUptake (double Ept, const Soil&, SoilWater&,
			    double EvapInterception);
  
  // Internal functions.
protected:
  double PotentialWaterUptake (const double h, 
			       const Soil& soil, const SoilWater& soil_water);
  double SoluteUptake (const Soil&,
		       const SoilWater&,
		       Solute&,
		       double /* PotNUpt */,
		       vector<double>& uptake,
		       double /* I_Mx */, double /* Rad */);

public:				// Used by external development models.
  void Vernalization (double Ta);
protected:
  void Emergence (const Soil&, const SoilHeat&);
  void DevelopmentStage (const Bioclimate&);
  double CropHeight ();
  void InitialLAI ();
  double CropLAI ();
  void RootPenetration (const Soil&, const SoilHeat&);
  double RootDensDistPar (double a);
  void RootDensity (const Soil& soil);
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

  // Simulation.
public:
  void tick (const Time& time, const Bioclimate&, const Soil&,
	     OrganicMatter&,
	     const SoilHeat&,
	     const SoilWater&, 
	     SoilNH4&,
	     SoilNO3&);
  const Harvest& harvest (const string column_name,
			  const Time&, const Geometry&, OrganicMatter&,
			  double stub_length, double stem_harvest,
			  double leaf_harvest, double sorg_harvest,
			  bool kill_off);
  void output (Log&, Filter&) const;

  double DS () const;

  // Create and Destroy.
private:
  friend class CropStandardSyntax;
  static Crop* make (const AttributeList&, int layers);
  CropStandard (const AttributeList& vl, int layers);
public:
  ~CropStandard ();
};

// Chemical constants affecting the crop.

const double molWeightCH2O = 30.0; // [gCH2O/mol]
const double molWeightCO2 = 44.0; // [gCO2/mol]

typedef void (*CropFun)(const Bioclimate&, CropStandard&);

struct CropStandard::Parameters
{ 
  const struct DevelPar
  {
    double EmrTSum;		// Soil temp sum at emergence
    double DS_Emr;		// Development stage (DS) emergence
    double DSRate1;		// Development rate [C-1 or d-1],
    // the vegetative stage
    double DSRate2;		// Development rate [C-1 or d-1],
    // the reproductive stage
    const CSMP& TempEff1;   // Temperature effect, vegetative stage
    const CSMP& TempEff2;   // Temperature effect, reproductive stage
    const CSMP& PhotEff1;   // Ptotoperiode effect, vegetative stage
  private:
    friend struct CropStandard::Parameters;
    DevelPar (const AttributeList&);
  } Devel;
  const struct VernalPar {
    bool required;
    double DSLim1;		// DS at beginning of vernalization
    double DSLim2;		// DS at end of vernalization
    double TaLim;		// Vernalization temp threshold
    double TaSum;		// Vernalization T-sum requirement
  private:
    friend struct CropStandard::Parameters;
    VernalPar (const AttributeList&);
  } Vernal;
  const struct LeafPhotPar {
    double Qeff;		// Quantum efficiency at low light
    double Fm;			// Max assimilation rate
    const CSMP& TempEff;	// Temperature effect, photosynthesis
  private:
    friend struct CropStandard::Parameters;
    LeafPhotPar (const AttributeList&);
  } LeafPhot;
  const struct CanopyPar {
    double InitGrowth;		// Initial growth parameter.
    double DSinit;		// DS at end of initial LAI-Development
    double WLfInit;		// WLeaf at end of initial LAI-Development
    double SpLAI;		// Specific leaf weight [ (m²/m²) / (g/m²) ]
    const CSMP& HvsDS;	// Crop height as function of DS
    const vector<double>& LAIDist0; // Relative LAI distribution at DS=0
    const vector<double>& LAIDist1; // Relative LAI distribution at DS=1
    double PARref;		// PAR reflectance
    double PARext;		// PAR extinction coefficient
    double EPext;		// EP extinction coefficient
  private:
    friend struct CropStandard::Parameters;
    CanopyPar (const AttributeList&);
  } Canopy;
  const struct RootPar {
    double DptEmr;		// Penetration at emergence
    double PenPar1;		// Penetration rate parameter, coefficient
    double PenPar2;		// Penetration rate parameter, threshold
    double MaxPen;		// Max penetration depth
    double SpRtLength;		// Specific root length
    double DensRtTip;		// Root density at (pot) penetration depth
    double Rad;			// Root radius [ cm ]
    double h_wp;		// Matrix potential at wilting
    double MxNH4Up;		// Max NH4 uptake per unit root length
    double MxNO3Up;		// Max NO3 uptake per unit root length
    double Rxylem;		// Transport resistence in xyleme
  private:
    friend struct CropStandard::Parameters;
    RootPar (const AttributeList&);
  } Root;
  const struct PartitPar {
    const CSMP& Root;	// Partitioning functions for root
    const CSMP& Leaf;	//   leaf, and stem as function of DS
    const CSMP& Stem;
  private:
    friend struct CropStandard::Parameters;
    PartitPar (const AttributeList&);
  } Partit;
  struct ProdPar {
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
    const CSMP& LfDR;		// Death rate of Leafs
    const CSMP& RtDR;		// Death rate of Roots
  private:
    friend struct CropStandard::Parameters;
    ProdPar (const AttributeList&);
  } Prod;
  struct CrpNPar {
    double SeedN;		// N-content in seed [ g N/m² ]
    double DS_fixate;		// Fixation of atmospheric N. after this DS
    double DS_cut_fixate;	// Restore fixation this DS after cut. 
    const CSMP& PtLeafCnc;	// Upper limit for N-conc in leaves
    const CSMP& CrLeafCnc;	// Critical lim f. N-conc in leaves
    const CSMP& NfLeafCnc;	// Non-func lim f. N-conc in leaves
    const CSMP& PtStemCnc;	// Upper limit for N-conc in stems
    const CSMP& CrStemCnc;	// Critical lim f. N-conc in stems
    const CSMP& NfStemCnc;	// Non-func lim f. N-conc in stems
    const CSMP& PtRootCnc;	// Upper limit for N-conc in roots
    const CSMP& CrRootCnc;	// Critical lim f. N-conc in roots
    const CSMP& NfRootCnc;	// Non-func lim f. N-conc in roots
    const CSMP& PtSOrgCnc;	// Upper limit for N-conc in stor org
    const CSMP& CrSOrgCnc;	// Critical lim f. N-conc in stor org
    const CSMP& NfSOrgCnc;	// Non-func lim f. N-conc in stor org
    const CSMP& DdRootCnc;	// N-conc in dead root material.
  private:
    friend struct CropStandard::Parameters;
    CrpNPar (const AttributeList&);
  } CrpN;
  struct HarvestPar {
    const double beta;		// The root/top concentration relation
    const double CStraw;	// Normal straw concentration
    const double CSOrg;		// Sorg conc. at the end of the normal range
    const double alpha;		// Rel. inc. in straw conc. above normal range
    const vector<const AttributeList*>& Stem; // Stem AM parameters.
    const vector<const AttributeList*>& Leaf; // Leaf AM parameters.
    const vector<const AttributeList*>& Dead; // Dead AM parameters.
    const vector<const AttributeList*>& SOrg; // SOrg AM parameters.
    const vector<const AttributeList*>& Root; // Root AM parameters.
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
  // Dunno where these belongs...
  double IntcpCap;
  double EpFac;
private:
  friend class CropStandard;
  Parameters (const AttributeList&);
public:
  ~Parameters ();
};

struct CropStandard::Variables
{ 
  void output (Log&, Filter&) const;
  struct RecPhenology
  {
    void output (Log&, Filter&) const;
    double DS;		// Development Stage
    double Vern;		// Vernalization criterium [C d]
  private:
    friend struct CropStandard::Variables;
    RecPhenology (const Parameters&, const AttributeList&);
  } Phenology;
  struct RecCanopy
  {
    void output (Log&, Filter&) const;
    double Height;		// Crop height [cm]
    double LAI;		// Leaf Area Index
    double LADm;		// Max Leaf Area Density [cm2/cm3]
    CSMP LAIvsH;		// Accumulated Leaf Area Index at Height
  private:
    friend struct CropStandard::Variables;
    RecCanopy (const Parameters&, const AttributeList&);
  } Canopy;
  struct RecRootSys
  {
    void output (Log&, Filter&) const;
    double Depth;		// Rooting Depth [cm]
    vector<double> Density;	// Root density [cm/cm3] in soil layers
    vector<double> H2OExtraction; // Extraction of H2O in soil layers
    // [cm³/cm³/h]
    vector<double> NH4Extraction; // Extraction of NH4-N in soil layers
    // [gN/cm³/h]
    vector<double> NO3Extraction; // Extraction of NH4-N in soil layers
    // [gN/cm³/h]
    double h_x;			// Root extraction at surface.
    double water_stress;	// Fraction of requested water we got.
    double Ept;			// Potential evapotranspiration.
  private:
    friend struct CropStandard::Variables;
    RecRootSys (const Parameters&, const AttributeList&, int layers);
  } RootSys;
  struct RecProd
  {
    void output (Log&, Filter&) const;
    double WLeaf;		// Leaf dry matter weight [g/m2]
    double WStem;		// Stem dry matter weight [g/m2]
    double WRoot;		// Root dry matter weight [g/m2]
    double WSOrg;		// Storage organ dry matter weight [g/m2]
    double NCrop;		// Nitrogen stored in dry matter [g/m2]
    AM* AM_root;		// Dead organic root matter.
    AM* AM_leaf;		// Dead organic leaf matter.
    
  private:
    friend struct CropStandard::Variables;
    RecProd (const Parameters&, const AttributeList&);
  } Prod;
  struct RecCrpAux
  {
    void output (Log&, Filter&) const;
    bool InitLAI;		// Initial LAI development ?
    double StemRes;		// Shielded Reserves in Stems
    double PotRtDpt;	// Potential Root Penetration Depth [cm]
    double PtNCnt;		// Potential Nitrogen Content in Crop [g/m2]
    double CrNCnt;		// Critical Nitrogen Content in Crop [g/m2]
    double NfNCnt;		// Non-func Nitrogen Content in Crop [g/m2]
    double PotTransp;	// Potential Transpiration [mm/h]
    double PotCanopyAss;	// Potential Canopy Assimilation [g CH2O/m2/h]
    double CanopyAss;	// Canopy Assimilation [g CH2O/m2/h]
    double IncWLeaf;	// Leaf growth [g DM/m2/d]
    double IncWStem;	// Stem growth [g DM/m2/d]
    double IncWSOrg;	// Storage organ growth [g DM/m2/d]
    double IncWRoot;	// Root growth [g DM/m2/d]
    double DeadWLeaf;		// Leaf DM removed [g DM/m2/d]
    double DeadNLeaf;		// Leaf N removed [g N/m2/d]
    double DeadWRoot;		// Root DM removed [g DM/m2/d]
    double DeadNRoot;		// Root N removed [g N/m2/d]
    double H2OUpt;		// H2O uptake [mm/h]
    double NH4Upt;		// NH4-N uptake [g/m2/h]
    double NO3Upt;		// NO3-N uptake [g/m2/h]
    double Fixated;		// N fixation from air. [g/m2/h]
    double DS_start_fixate;	// Start fixation at this DS.
  private:
    friend struct CropStandard::Variables;
    RecCrpAux (const Parameters&, const AttributeList&);
  } CrpAux;
private:
  friend class CropStandard;
  Variables (const Parameters&, const AttributeList&, int layers);
public:
  ~Variables ();
};

CropStandard::Parameters::Parameters (const AttributeList& vl) 
  : Devel (vl.alist ("Devel")),
    Vernal (vl.alist ("Vernal")),
    LeafPhot (vl.alist ("LeafPhot")),
    Canopy (vl.alist ("Canopy")),
    Root (vl.alist ("Root")),
    Partit (vl.alist ("Partit")),
    Prod (vl.alist ("Prod")),
    CrpN (vl.alist ("CrpN")),
    Harvest (vl.alist ("Harvest")),
    IntcpCap (vl.number ("IntcpCap")),
    EpFac (vl.number ("EpFac"))
{ }

CropStandard::Parameters::DevelPar::DevelPar (const AttributeList& vl)
  : EmrTSum (vl.number ("EmrTSum")),
    DS_Emr (vl.number ("DS_Emr")),
    DSRate1 (vl.number ("DSRate1")),
    DSRate2 (vl.number ("DSRate2")),
    TempEff1 (vl.csmp ("TempEff1")),
    TempEff2 (vl.csmp ("TempEff2")),
    PhotEff1 (vl.csmp ("PhotEff1"))
{ }

CropStandard::Parameters::VernalPar::VernalPar (const AttributeList& vl)
  : required (vl.flag ("required")),
    DSLim1 (vl.number ("DSLim1")),
    DSLim2 (vl.number ("DSLim2")),
    TaLim (vl.number ("TaLim")),
    TaSum (vl.number ("TaSum"))
{ }

CropStandard::Parameters::LeafPhotPar::LeafPhotPar (const AttributeList& vl)
  : Qeff (vl.number ("Qeff")),
    Fm (vl.number ("Fm")),
    TempEff (vl.csmp ("TempEff"))
{ }

CropStandard::Parameters::CanopyPar::CanopyPar (const AttributeList& vl)
  : InitGrowth (vl.number ("InitGrowth")),
    DSinit (vl.number ("DSinit")),
    WLfInit (vl.number ("WLfInit")),
    SpLAI (vl.number ("SpLAI")),
    HvsDS (vl.csmp ("HvsDS")),
    LAIDist0 (vl.number_sequence ("LAIDist0")),
    LAIDist1 (vl.number_sequence ("LAIDist1")),
    PARref (vl.number ("PARref")),
    PARext (vl.number ("PARext")),
    EPext (vl.number ("EPext"))
{ }

CropStandard::Parameters::RootPar::RootPar (const AttributeList& vl)
  : DptEmr (vl.number ("DptEmr")),
    PenPar1 (vl.number ("PenPar1")),
    PenPar2 (vl.number ("PenPar2")),
    MaxPen (vl.number ("MaxPen")),
    SpRtLength (vl.number ("SpRtLength")),
    DensRtTip (vl.number ("DensRtTip")),
    Rad (vl.number ("Rad")),
    h_wp (vl.number ("h_wp")),
    MxNH4Up (vl.number ("MxNH4Up")),
    MxNO3Up (vl.number ("MxNO3Up")),
    Rxylem (vl.number ("Rxylem"))
{ }

CropStandard::Parameters::PartitPar::PartitPar (const AttributeList& vl)
  : Root (vl.csmp ("Root")),
    Leaf (vl.csmp ("Leaf")),
    Stem (vl.csmp ("Stem"))
{ }

CropStandard::Parameters::ProdPar::ProdPar (const AttributeList& vl)
  : E_Root (vl.number ("E_Root")),
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
    LfDR (vl.csmp ("LfDR")),
    RtDR (vl.csmp ("RtDR"))     
{ }

CropStandard::Parameters::CrpNPar::CrpNPar (const AttributeList& vl)
  : SeedN (vl.number ("SeedN")),
    DS_fixate (vl.number ("DS_fixate")),
    DS_cut_fixate (vl.number ("DS_cut_fixate")),
    PtLeafCnc (vl.csmp ("PtLeafCnc")),
    CrLeafCnc (vl.csmp ("CrLeafCnc")),
    NfLeafCnc (vl.csmp ("NfLeafCnc")),
    PtStemCnc (vl.csmp ("PtStemCnc")),
    CrStemCnc (vl.csmp ("CrStemCnc")),
    NfStemCnc (vl.csmp ("NfStemCnc")),
    PtRootCnc (vl.csmp ("PtRootCnc")),
    CrRootCnc (vl.csmp ("CrRootCnc")),
    NfRootCnc (vl.csmp ("NfRootCnc")),
    PtSOrgCnc (vl.csmp ("PtSOrgCnc")),
    CrSOrgCnc (vl.csmp ("CrSOrgCnc")),
    NfSOrgCnc (vl.csmp ("NfSOrgCnc")),
    DdRootCnc (vl.csmp ("DdRootCnc"))
{ }

CropStandard::Parameters::HarvestPar::HarvestPar (const AttributeList& vl)
  : beta (vl.number ("beta")),
    CStraw (vl.number ("CStraw")),
    CSOrg (vl.number ("CSOrg")),
    alpha (vl.number ("alpha")),
    Stem (vl.alist_sequence ("Stem")),
    Leaf (vl.alist_sequence ("Leaf")),
    Dead (vl.alist_sequence ("Dead")),
    SOrg (vl.alist_sequence ("SOrg")),
    Root (vl.alist_sequence ("Root")),
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
				    const AttributeList& vl,
				    int layers)
  : Phenology (par, vl.alist ("Phenology")),
    Canopy (par, vl.alist ("Canopy")),
    RootSys (par, vl.alist ("RootSys"), layers),
    Prod (par, vl.alist ("Prod")),
    CrpAux (par, vl.alist ("CrpAux"))
{ }

void 
CropStandard::Variables::output (Log& log, Filter& filter) const
{
  if (filter.check ("Phenology"))
    Phenology.output (log, filter.lookup ("Phenology"));
  if (filter.check ("Canopy"))
    Canopy.output (log, filter.lookup ("Canopy"));
  if (filter.check ("RootSys"))
    RootSys.output (log, filter.lookup ("RootSys"));
  if (filter.check ("Prod"))
    Prod.output (log, filter.lookup ("Prod"));
  if (filter.check ("CrpAux"))
    CrpAux.output (log, filter.lookup ("CrpAux"));
}

CropStandard::Variables::RecPhenology::RecPhenology (const Parameters& par,
						     const AttributeList& vl)
  : DS (vl.number ("DS")),
    Vern (vl.check ("Vern") ? vl.number ("Vern") : par.Vernal.TaSum)
{ }

void 
CropStandard::Variables::RecPhenology::output (Log& log, Filter& filter) const
{
  log.open ("Phenology");
  log.output ("DS", filter, DS);
  log.output ("Vern", filter, Vern);
  log.close();
}

CropStandard::Variables::RecCanopy::RecCanopy (const Parameters&,
					       const AttributeList& vl)
  : Height (vl.number ("Height")),
    LAI (vl.number ("LAI")),
    LADm (vl.number ("LADm")),
    LAIvsH (vl.csmp ("LAIvsH"))
{ }

void 
CropStandard::Variables::RecCanopy::output (Log& log, Filter& filter) const
{
  log.open ("Canopy");
  log.output ("Height", filter, Height);
  log.output ("LAI", filter, LAI);
  log.output ("LADm", filter, LADm);
  log.output ("LAIvsH", filter, LAIvsH);
  log.close();
}

CropStandard::Variables::RecRootSys::RecRootSys (const Parameters& par,
						 const AttributeList& vl, 
						 int layers)
  : Depth (vl.check ("Depth") ? vl.number ("Depth") : par.Root.DptEmr),
    Density (vl.number_sequence ("Density")),
    H2OExtraction (vl.number_sequence ("H2OExtraction")),
    NH4Extraction (vl.number_sequence ("NH4Extraction")),
    NO3Extraction (vl.number_sequence ("NO3Extraction")),
    h_x (vl.number ("h_x")),
    water_stress (1.0),
    Ept (0.0)
{ 
  if (layers > 0)
    {
      assert (Density.size () == 0);
      Density.insert (Density.begin (), layers, 0.0);
      assert (H2OExtraction.size () == 0);
      H2OExtraction.insert (H2OExtraction.begin (), layers, 0.0);
      assert (NH4Extraction.size () == 0);
      NH4Extraction.insert (NH4Extraction.begin (), layers, 0.0);
      assert (NO3Extraction.size () == 0);
      NO3Extraction.insert (NO3Extraction.begin (), layers, 0.0);
    }
}

void 
CropStandard::Variables::RecRootSys::output (Log& log, Filter& filter) const
{
  log.open ("RootSys");
  log.output ("Depth", filter, Depth);
  log.output ("Density", filter, Density);
  log.output ("H2OExtraction", filter, H2OExtraction);
  log.output ("NH4Extraction", filter, NH4Extraction);
  log.output ("NO3Extraction", filter, NO3Extraction);
  log.output ("h_x", filter, h_x);
  log.output ("water_stress", filter, water_stress, true);
  log.output ("Ept", filter, Ept, true);
  log.close();
}

CropStandard::Variables::RecProd::RecProd (const Parameters& par, 
					   const AttributeList& vl)
  : WLeaf (vl.number ("WLeaf")),
    WStem (vl.number ("WStem")),
    WRoot (vl.number ("WRoot")),
    WSOrg (vl.number ("WSOrg")),
    NCrop (vl.check ("NCrop") ? vl.number ("NCrop") : par.CrpN.SeedN),
    AM_root (NULL),
    AM_leaf (NULL)
{ }

void 
CropStandard::Variables::RecProd::output (Log& log, Filter& filter) const
{
  log.open ("Prod");
  log.output ("WLeaf", filter, WLeaf);
  log.output ("WStem", filter, WStem);
  log.output ("WRoot", filter, WRoot);
  log.output ("WSOrg", filter, WSOrg);
  log.output ("NCrop", filter, NCrop);
  log.close();
}

CropStandard::Variables::RecCrpAux::RecCrpAux (const Parameters& par, 
					       const AttributeList& vl)
  : InitLAI (vl.flag ("InitLAI")),
    StemRes (vl.number ("StemRes")),
    PotRtDpt (  vl.check ("PotRtDpt") 
	      ? vl.number ("PotRtDpt")
	      : par.Root.DptEmr),
    PtNCnt (0.0),
    CrNCnt (0.0),
    NfNCnt (0.0),
    PotTransp (vl.number ("PotTransp")),
    PotCanopyAss (vl.number ("PotCanopyAss")),
    CanopyAss (vl.number ("CanopyAss")),
    IncWLeaf (0.0),
    IncWStem (0.0),
    IncWSOrg (0.0),
    IncWRoot (0.0),
    DeadWLeaf (0.0),
    DeadNLeaf (0.0),
    DeadWRoot (0.0),
    DeadNRoot (0.0),
    H2OUpt (0.0),
    NH4Upt (0.0),
    NO3Upt (0.0),
    Fixated (0.0),
    DS_start_fixate (par.CrpN.DS_fixate)
{ }

void 
CropStandard::Variables::RecCrpAux::output (Log& log, Filter& filter) const
{
  log.open ("CrpAux");
  log.output ("InitLAI", filter, InitLAI);
  log.output ("StemRes", filter, StemRes);
  log.output ("PotRtDpt", filter, PotRtDpt);
  log.output ("PtNCnt", filter, PtNCnt, true);
  log.output ("CrNCnt", filter, CrNCnt, true);
  log.output ("NfNCnt", filter, NfNCnt, true);
  log.output ("PotTransp", filter, PotTransp);
  log.output ("PotCanopyAss", filter, PotCanopyAss);
  log.output ("CanopyAss", filter, CanopyAss);
  log.output ("IncWLeaf", filter, IncWLeaf, true);
  log.output ("IncWStem", filter, IncWStem, true);
  log.output ("IncWSOrg", filter, IncWSOrg, true);
  log.output ("IncWRoot", filter, IncWRoot, true);
  log.output ("DeadWLeaf", filter, DeadWLeaf, true);
  log.output ("DeadNLeaf", filter, DeadNLeaf, true);
  log.output ("DeadWRoot", filter, DeadWRoot, true);
  log.output ("DeadNRoot", filter, DeadNRoot, true);
  log.output ("H2OUpt", filter, H2OUpt, true);
  log.output ("NH4Upt", filter, NH4Upt, true);
  log.output ("NO3Upt", filter, NO3Upt, true);
  log.output ("Fixated", filter, Fixated, true);
  log.output ("DS_start_fixate", filter, DS_start_fixate, true);
  log.close();
}

CropStandard::Variables::~Variables ()
{ }

// Add the Crop syntax to the syntax table.
Crop*
CropStandard::make (const AttributeList& vl, int layers)
{
  return new CropStandard (vl, layers);
}

static struct CropStandardSyntax
{
  CropStandardSyntax ();
} standard_crop_syntax;

#ifdef BORLAND_TEMPLATES
template class add_submodule<OM>;
#endif

CropStandardSyntax::CropStandardSyntax ()
{
  static const vector<double> empty_array;
  static const CSMP empty_csmp;

  Syntax& syntax = *new Syntax ();
  AttributeList& alist = *new AttributeList ();

    // Canopy
  Syntax& Canopy = *new Syntax ();
  syntax.add ("Canopy", Canopy, Syntax::State);

  // CropPar

  // DevelPar
  Syntax& Devel = *new Syntax ();
  syntax.add ("Devel", Devel, Syntax::Const);

  Devel.add ("EmrTSum", Syntax::Number, Syntax::Const);
  Devel.add ("DS_Emr", Syntax::Number, Syntax::Const);
  Devel.add ("DSRate1", Syntax::Number, Syntax::Const);
  Devel.add ("DSRate2", Syntax::Number, Syntax::Const);
  Devel.add ("TempEff1", Syntax::CSMP, Syntax::Const);
  Devel.add ("TempEff2", Syntax::CSMP, Syntax::Const);
  Devel.add ("PhotEff1", Syntax::CSMP, Syntax::Const);
    
  // VernalPar
  Syntax& Vernal = *new Syntax ();
  syntax.add ("Vernal", Vernal, Syntax::Const);

  Vernal.add ("required", Syntax::Boolean, Syntax::Const);
  Vernal.add ("DSLim1", Syntax::Number, Syntax::Const);
  Vernal.add ("DSLim2", Syntax::Number, Syntax::Const);
  Vernal.add ("TaLim", Syntax::Number, Syntax::Const);
  Vernal.add ("TaSum", Syntax::Number, Syntax::Const);

  // LeafPhotPar
  Syntax& LeafPhot = *new Syntax ();
  syntax.add ("LeafPhot", LeafPhot, Syntax::Const);

  LeafPhot.add ("Qeff", Syntax::Number, Syntax::Const);
  LeafPhot.add ("Fm", Syntax::Number, Syntax::Const);
  LeafPhot.add ("TempEff", Syntax::CSMP, Syntax::Const);

  Canopy.add ("InitGrowth", Syntax::Number, Syntax::Const);
  Canopy.add ("DSinit", Syntax::Number, Syntax::Const);
  Canopy.add ("WLfInit", Syntax::Number, Syntax::Const);
  Canopy.add ("SpLAI", Syntax::Number, Syntax::Const);
  Canopy.add ("HvsDS", Syntax::CSMP, Syntax::Const);
  Canopy.add ("LAIDist0", Syntax::Number, Syntax::Const, 3);
  Canopy.add ("LAIDist1", Syntax::Number, Syntax::Const, 3);
  Canopy.add ("PARref", Syntax::Number, Syntax::Const);
  Canopy.add ("PARext", Syntax::Number, Syntax::Const);
  Canopy.add ("EPext", Syntax::Number, Syntax::Const);

  // RootPar
  Syntax& Root = *new Syntax ();
  syntax.add ("Root", Root, Syntax::Const);

  Root.add ("DptEmr", Syntax::Number, Syntax::Const);
  Root.add ("PenPar1", Syntax::Number, Syntax::Const);
  Root.add ("PenPar2", Syntax::Number, Syntax::Const);
  Root.add ("MaxPen", Syntax::Number, Syntax::Const);
  Root.add ("SpRtLength", Syntax::Number, Syntax::Const);
  Root.add ("DensRtTip", Syntax::Number, Syntax::Const);
  Root.add ("Rad", Syntax::Number, Syntax::Const);
  Root.add ("h_wp", Syntax::Number, Syntax::Const);
  Root.add ("MxNH4Up", Syntax::Number, Syntax::Const);
  Root.add ("MxNO3Up", Syntax::Number, Syntax::Const);
  Root.add ("Rxylem", Syntax::Number, Syntax::Const);

    // PartitPar
  Syntax& Partit = *new Syntax ();
  syntax.add ("Partit", Partit, Syntax::Const);

  Partit.add ("Root", Syntax::CSMP, Syntax::Const);
  Partit.add ("Leaf", Syntax::CSMP, Syntax::Const);
  Partit.add ("Stem", Syntax::CSMP, Syntax::Const);

  // ProdPar
  Syntax& Prod = *new Syntax ();
  syntax.add ("Prod", Prod, Syntax::Const);

  Prod.add ("E_Root", Syntax::Number, Syntax::Const);
  Prod.add ("E_Leaf", Syntax::Number, Syntax::Const);
  Prod.add ("E_Stem", Syntax::Number, Syntax::Const);
  Prod.add ("E_SOrg", Syntax::Number, Syntax::Const);
  Prod.add ("r_Root", Syntax::Number, Syntax::Const);
  Prod.add ("r_Leaf", Syntax::Number, Syntax::Const);
  Prod.add ("r_Stem", Syntax::Number, Syntax::Const);
  Prod.add ("r_SOrg", Syntax::Number, Syntax::Const);
  Prod.add ("ShldResC", Syntax::Number, Syntax::Const);
  Prod.add ("ReMobilDS", Syntax::Number, Syntax::Const);
  Prod.add ("ReMobilRt", Syntax::Number, Syntax::Const);
  Prod.add ("LfDR", Syntax::CSMP, Syntax::Const);
  Prod.add ("RtDR", Syntax::CSMP, Syntax::Const);

  // CrpNPar
  Syntax& CrpN = *new Syntax ();
  AttributeList& CrpNList = *new AttributeList ();
  syntax.add ("CrpN", CrpN, Syntax::Const);
  alist.add ("CrpN", CrpNList);

  CrpN.add ("SeedN", Syntax::Number, Syntax::Const);
  CrpN.add ("DS_fixate", Syntax::Number, Syntax::Const);
  CrpNList.add ("DS_fixate", 42000.0);
  CrpN.add ("DS_cut_fixate", Syntax::Number, Syntax::Const);
  CrpNList.add ("DS_cut_fixate", 0.0);
  CrpN.add ("PtLeafCnc", Syntax::CSMP, Syntax::Const);
  CrpN.add ("CrLeafCnc", Syntax::CSMP, Syntax::Const);
  CrpN.add ("NfLeafCnc", Syntax::CSMP, Syntax::Const);
  CrpN.add ("PtStemCnc", Syntax::CSMP, Syntax::Const);
  CrpN.add ("CrStemCnc", Syntax::CSMP, Syntax::Const);
  CrpN.add ("NfStemCnc", Syntax::CSMP, Syntax::Const);
  CrpN.add ("PtRootCnc", Syntax::CSMP, Syntax::Const);
  CrpN.add ("CrRootCnc", Syntax::CSMP, Syntax::Const);
  CrpN.add ("NfRootCnc", Syntax::CSMP, Syntax::Const);
  CrpN.add ("PtSOrgCnc", Syntax::CSMP, Syntax::Const);
  CrpN.add ("CrSOrgCnc", Syntax::CSMP, Syntax::Const);
  CrpN.add ("NfSOrgCnc", Syntax::CSMP, Syntax::Const);
  CrpN.add ("DdRootCnc", Syntax::CSMP, Syntax::Const);

  // HarvestPar
  Syntax& Harvest = *new Syntax ();
  AttributeList& HarvestList = *new AttributeList ();

  syntax.add ("Harvest", Harvest, Syntax::Const);
  alist.add ("Harvest", HarvestList);

  Harvest.add ("beta", Syntax::Number, Syntax::Const);
  Harvest.add ("CStraw", Syntax::Number, Syntax::Const);
  Harvest.add ("CSOrg", Syntax::Number, Syntax::Const);
  Harvest.add ("alpha", Syntax::Number, Syntax::Const);
  add_submodule<OM> ("Stem", Harvest, HarvestList,
		     Syntax::Const, Syntax::Sequence);
  add_submodule<OM> ("Leaf", Harvest, HarvestList,
		     Syntax::Const, Syntax::Sequence);
  add_submodule<OM> ("Dead", Harvest, HarvestList,
		     Syntax::Const, Syntax::Sequence);
  add_submodule<OM> ("SOrg", Harvest, HarvestList,
		     Syntax::Const, Syntax::Sequence);
  add_submodule<OM> ("Root", Harvest, HarvestList,
		     Syntax::Const, Syntax::Sequence);
  Harvest.add ("C_Stem", Syntax::Number, Syntax::Const);
  Harvest.add ("C_Leaf", Syntax::Number, Syntax::Const);
  Harvest.add ("C_Dead", Syntax::Number, Syntax::Const);
  Harvest.add ("C_SOrg", Syntax::Number, Syntax::Const);
  Harvest.add ("C_Root", Syntax::Number, Syntax::Const);
  Harvest.add ("DSmax", Syntax::Number, Syntax::Const);
  HarvestList.add ("DSmax", 0.0);
  Harvest.add ("DSnew", Syntax::Number, Syntax::Const);
  HarvestList.add ("DSnew", 0.0);

   // I don't know where these belong.
  syntax.add ("IntcpCap", Syntax::Number, Syntax::Const);
  syntax.add ("EpFac", Syntax::Number, Syntax::Const);

  // Variables.

  // Phenology
  Syntax& Phenology = *new Syntax ();
  AttributeList& vPhenology = *new AttributeList ();
  syntax.add ("Phenology", Phenology, Syntax::State);
  alist.add ("Phenology", vPhenology);

  Phenology.add ("DS", Syntax::Number, Syntax::State);
  vPhenology.add ("DS", -1.0);
  Phenology.add ("Vern", Syntax::Number, Syntax::Optional);

  // Canopy
  AttributeList& vCanopy = *new AttributeList ();
  alist.add ("Canopy", vCanopy);

  Canopy.add ("Height", Syntax::Number, Syntax::State);
  vCanopy.add ("Height", 0.0);
  Canopy.add ("LAI", Syntax::Number, Syntax::State);
  vCanopy.add ("LAI", 0.0);
  Canopy.add ("LADm", Syntax::Number, Syntax::State);
  vCanopy.add ("LADm", -9999.99);
  Canopy.add ("LAIvsH", Syntax::CSMP, Syntax::State);
  vCanopy.add ("LAIvsH", empty_csmp);

  // RootSys
  Syntax& RootSys = *new Syntax ();
  AttributeList& vRootSys = *new AttributeList ();
  syntax.add ("RootSys", RootSys, Syntax::State);
  alist.add ("RootSys", vRootSys);

  RootSys.add ("Depth", Syntax::Number, Syntax::Optional);
  RootSys.add ("Density", Syntax::Number, Syntax::State, Syntax::Sequence);
  vRootSys.add ("Density", empty_array);
  RootSys.add ("H2OExtraction", Syntax::Number,
	       Syntax::State, Syntax::Sequence);
  vRootSys.add ("H2OExtraction", empty_array);
  RootSys.add ("NH4Extraction", Syntax::Number,
	       Syntax::State, Syntax::Sequence);
  vRootSys.add ("NH4Extraction", empty_array);
  RootSys.add ("NO3Extraction", Syntax::Number,
	       Syntax::State, Syntax::Sequence);
  vRootSys.add ("NO3Extraction", empty_array);
  RootSys.add ("h_x", Syntax::Number, Syntax::State);
  vRootSys.add ("h_x", 0.0);
  RootSys.add ("water_stress", Syntax::Number, Syntax::LogOnly);
  RootSys.add ("Ept", Syntax::Number, Syntax::LogOnly);

  // Prod
  // Warning: Uses same syntax as `ProdPar'.
  AttributeList& vProd = *new AttributeList ();
  alist.add ("Prod", vProd);

  Prod.add ("WLeaf", Syntax::Number, Syntax::State);
  vProd.add ("WLeaf", 0.001);
  Prod.add ("WStem", Syntax::Number, Syntax::State);
  vProd.add ("WStem", 0.000);
  Prod.add ("WRoot", Syntax::Number, Syntax::State);
  vProd.add ("WRoot", 0.001);
  Prod.add ("WSOrg", Syntax::Number, Syntax::State);
  vProd.add ("WSOrg", 0.000);
  Prod.add ("NCrop", Syntax::Number, Syntax::Optional);

  // CrpAux
  Syntax& CrpAux = *new Syntax ();
  AttributeList& vCrpAux = *new AttributeList ();
  syntax.add ("CrpAux", CrpAux, Syntax::State);
  alist.add ("CrpAux", vCrpAux);

  CrpAux.add ("InitLAI", Syntax::Boolean, Syntax::State);
  vCrpAux.add ("InitLAI", true);
  CrpAux.add ("PotRtDpt", Syntax::Number, Syntax::Optional);
  CrpAux.add ("StemRes", Syntax::Number, Syntax::State);
  vCrpAux.add ("StemRes", 0.0);
  CrpAux.add ("PtNCnt", Syntax::Number, Syntax::LogOnly);
  CrpAux.add ("CrNCnt", Syntax::Number, Syntax::LogOnly);
  CrpAux.add ("NfNCnt", Syntax::Number, Syntax::LogOnly);
  CrpAux.add ("PotTransp", Syntax::Number, Syntax::State);
  vCrpAux.add ("PotTransp", 0.0);
  CrpAux.add ("PotCanopyAss", Syntax::Number, Syntax::State);
  vCrpAux.add ("PotCanopyAss", 0.0);
  CrpAux.add ("CanopyAss", Syntax::Number, Syntax::State);
  vCrpAux.add ("CanopyAss", 0.0);
  CrpAux.add ("IncWLeaf", Syntax::Number, Syntax::LogOnly);
  CrpAux.add ("IncWStem", Syntax::Number, Syntax::LogOnly);
  CrpAux.add ("IncWSOrg", Syntax::Number, Syntax::LogOnly);
  CrpAux.add ("IncWRoot", Syntax::Number, Syntax::LogOnly);
  CrpAux.add ("DeadWLeaf", Syntax::Number, Syntax::LogOnly);
  CrpAux.add ("DeadNLeaf", Syntax::Number, Syntax::LogOnly);
  CrpAux.add ("DeadWRoot", Syntax::Number, Syntax::LogOnly);
  CrpAux.add ("DeadNRoot", Syntax::Number, Syntax::LogOnly);
  CrpAux.add ("H2OUpt", Syntax::Number, Syntax::LogOnly);
  CrpAux.add ("NH4Upt", Syntax::Number, Syntax::LogOnly);
  CrpAux.add ("NO3Upt", Syntax::Number, Syntax::LogOnly);
  CrpAux.add ("Fixated", Syntax::Number, Syntax::LogOnly);
  CrpAux.add ("DS_start_fixate", Syntax::Number, Syntax::LogOnly);

  Crop::add_type ("default", alist, syntax, &CropStandard::make);
}

double
CropStandard::height () const
{
  return var.Canopy.Height;
}

double
CropStandard::LAI () const
{
  return var.Canopy.LAI;
}

const CSMP&
CropStandard::LAIvsH () const
{
  return var.Canopy.LAIvsH;
}

double
CropStandard::PARext () const
{
  return par.Canopy.PARext;
}

double
CropStandard::PARref () const
{
  return par.Canopy.PARref;
}

double
CropStandard::EPext () const
{
  return par.Canopy.EPext;
}

double
CropStandard::IntcpCap () const
{
  return par.IntcpCap;
}

double
CropStandard::EpFac () const
{
  return par.EpFac;
}

double
CropStandard::SoluteUptake (const Soil& soil,
			    const SoilWater& soil_water,
			    Solute& solute,
			    double PotNUpt,
			    vector<double>& uptake,
			    double I_max, double r_root)
{
  PotNUpt /= 1.0e4;		// gN/m²/h -> gN/cm²/h
  const vector<double>& root_density = var.RootSys.Density;
  const vector<double>& S = var.RootSys.H2OExtraction;
  const int size = soil.size ();
  vector<double> I_zero (size, 0.0);
  vector<double> B_zero (size, 0.0);
  double U_zero = 0.0;
  double B = 0.0;
  double c_root = 0.0;

  for (int i = 0; i < size; i++)
    {
      const double C_l = solute.C (i);
      const double Theta = soil_water.Theta_old (i);
      const double L = root_density[i];
      if (L > 0 && soil_water.h (i) <= 0.0)
	{
	  const double q_r = S[i] / L;
	  const double D = solute.diffusion_coefficient ()
	    * soil.tortuosity_factor (i, Theta)
	    * Theta;
	  const double alpha = q_r / ( 2 * M_PI * D);
	  const double beta = 1.0 / (r_root * sqrt (M_PI * L));
	  const double beta_squared = beta * beta;
	  if (alpha < 1e-10)
	    {
	      B_zero[i] = 4.0 * M_PI * D 
		/ (beta_squared * log (beta_squared) / (beta_squared - 1.0) - 1.0);
	      I_zero[i] = B_zero[i] * C_l;
	    }
	  else if (alpha == 2.0)
	    {
	      B_zero[i] = q_r * log (beta_squared) 
		/ ((beta_squared - 1.0) - log (beta_squared));
	      I_zero[i] = q_r * (beta_squared - 1.0) * C_l 
		/ ((beta_squared - 1.0) - log (beta_squared));
	    }
	  else
	    {
	      B_zero[i] = q_r * (pow (beta, 2.0 - alpha) - 1.0)
		/ ((beta_squared - 1.0) * (1.0 - 0.5 * alpha)
		   - (pow (beta, 2.0 - alpha) - 1.0));
	      I_zero[i] = q_r * (beta_squared - 1.0) * (1.0 - 0.5 * alpha) * C_l
		/ ((beta_squared - 1.0) * (1.0 - 0.5 * alpha)
		   - (pow (beta, 2.0 - alpha) - 1.0));
	    }
	  assert (finite (I_zero[i]));
	  assert (finite (B_zero[i]));
	  B += L * soil.dz (i) * B_zero[i];
	  U_zero += L * soil.dz (i) * min (I_zero[i], I_max);
	}
    }
  if (U_zero > PotNUpt)
    c_root = (U_zero - PotNUpt) / B;

  for (int i = 0; i < size; i++)
    {
      const double L = root_density[i];
      if (solute.M_left (i) > 1e-8 && L > 0 && soil_water.h (i) <= 0.0)
	uptake[i] = max (0.0, 
			 min (L * (min (I_zero[i], I_max)
				   - B_zero[i] * c_root),
			      solute.M_left (i)));
      else
	uptake[i] = 0.0;
      assert (uptake[i] >= 0.0);
    }
  solute.add_to_sink (uptake);

  // gN/cm³/h -> gN/m²/h
  return soil.total (uptake) * 1.0e4; 
}

void
CropStandard::Vernalization (double Ta)
{
  const Parameters::VernalPar& Vernal = par.Vernal;
  double& Vern = var.Phenology.Vern;
  double& DS = var.Phenology.DS;

  if (DS < Vernal.DSLim1)
    return;
  Vern -= min (Ta - Vernal.TaLim, 0.0);
  if (DS > Vernal.DSLim2)
    DS = Vernal.DSLim2;
}

void 
CropStandard::Emergence (const Soil& soil, const SoilHeat& soil_heat)
{
  const Parameters::DevelPar& Devel = par.Devel;
  const double EmrDpt = par.Root.DptEmr;
  double& DS = var.Phenology.DS;

  DS += soil_heat.T (soil.interval_plus (-EmrDpt)) / Devel.EmrTSum;
  if (DS > 0)
    DS = Devel.DS_Emr;
}

void 
CropStandard::DevelopmentStage (const Bioclimate& bioclimate)
{
  const Parameters::DevelPar& Devel = par.Devel;
  Variables::RecPhenology& Phenology = var.Phenology;

  const double Ta = bioclimate.AirTemperature ();

  if (Phenology.DS < 1)
    {
      Phenology.DS += (Devel.DSRate1
		       * Devel.TempEff1 (Ta)
		       * Devel.PhotEff1 (bioclimate.DayLength ()));
      if (Phenology.Vern < 0)
	Vernalization (Ta);
    }
  else
    {
      Phenology.DS += Devel.DSRate2 * Devel.TempEff2 (Ta);
      if (Phenology.DS > 2)
	Phenology.DS = 2.0;
    }
}
double 
CropStandard::CropHeight ()
{
  const Parameters::CanopyPar& Canopy = par.Canopy;
  double& DS = var.Phenology.DS;

  return Canopy.HvsDS (DS);
}

void 
CropStandard::InitialLAI ()
{
  const Parameters::CanopyPar& Canopy = par.Canopy;
  const double WLeaf = var.Prod.WLeaf;
  double& DS = var.Phenology.DS;
  double& LAI = var.Canopy.LAI;
  bool& InitLAI = var.CrpAux.InitLAI;

  if (WLeaf >= Canopy.WLfInit)
    {
      LAI = Canopy.SpLAI * WLeaf;
      InitLAI = false;
    }
  else
    {
      if (DS > Canopy.DSinit)
	DS = Canopy.DSinit;
      LAI = 0.5 * (exp (Canopy.InitGrowth * DS) - 1);
    }
}

double 
CropStandard::CropLAI ()
{    
  const Parameters::CanopyPar& Canopy = par.Canopy;
  // const double DS = var.Phenology.DS;
  const double WLeaf = var.Prod.WLeaf;

  return Canopy.SpLAI * WLeaf;
}

void 
CropStandard::CanopyStructure ()
{
  const Parameters::CanopyPar& CanopyPar = par.Canopy;
  const double DS = var.Phenology.DS;
  Variables::RecCanopy& Canopy = var.Canopy;
    
  // The leaf density is assumed to from the group up to height z0,
  // then increase linearly until height z1, continue at that
  // density until z2, and then decrease linearly until the top of
  // the crop.  The values of z1, z2, and z3 are scaled so the
  // ground height is zero and the top of the crop is 1.

  double z0;			// Height of first leaf [0 - 1].
  double z1;			// Min height of the MaxLAD area [0 - 1].
  double z2;			// Max height of the MaxLAD area [0 - 1].
  double Area;		// Area spanned by z0, z1, and z2.

  if (DS <= 1)
    {
      // LAIDist0 is the leaf density at DS = 0, and LAIDist 1
      // is the leaf density at DS = 1.  The leaf density is
      // assumed to develop linearly as a function of DS between
      // DS 0 and DS 1.

      z0 = CanopyPar.LAIDist0[0] +
	(CanopyPar.LAIDist1[0] - CanopyPar.LAIDist0[0]) * DS;
      z1 = CanopyPar.LAIDist0[1] +
	(CanopyPar.LAIDist1[1] - CanopyPar.LAIDist0[1]) * DS;
      z2 = CanopyPar.LAIDist0[2] +
	(CanopyPar.LAIDist1[2] - CanopyPar.LAIDist0[2]) * DS;
      Area = (1 + z2 - z1 - z0) / 2;
      Canopy.LADm = Canopy.LAI / (Area * Canopy.Height);
    }
  else
    {
      assert (DS <= 2);

      z0 = CanopyPar.LAIDist1[0];
      z1 = CanopyPar.LAIDist1[1];
      z2 = CanopyPar.LAIDist1[2];
      double Area = (1 + z2 - z1 - z0) / 2;
      double MaxLAD = Canopy.LAI / (Area * Canopy.Height);
	    
      if (MaxLAD > Canopy.LADm)
	// After DS = 1 LAI may increase for some time, keeping
	// z0, z1, and z2 constant but adding to MaxLAD.
	Canopy.LADm = MaxLAD;
      else
	{
	  // Then the crop start eating itself.  This is done by
	  // keeping MaxLAD, (z1 - z0), and z2 constant but
	  // increasing z1 and z0.

	  // Need is the Area we want after moving z1 and z0.
	  double Need = Canopy.LAI / Canopy.Height / Canopy.LADm;

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

	      double x0 = 1 - sqrt (2 * Need * (z1 - z2 - z0 + 1));
	      double x1 = 1 + (z2 - 1) * sqrt (2 * Need / (z1 - z2 - z0 + 1));
	      double y1 = sqrt (2 * Need / (z1 - z2 - z0 + 1));
			    
	      // Check the results.
	      assert (fabs (Need - (1 - x0) * y1 / 2) < 0.0001);
	      assert (fabs ((1 - x1) / y1 - (1 - z2)) < 0.0001);
	      assert (fabs ((x1 - x0) / y1 - (z1 - z0)) < 0.0001);

	      // Insert this special distribution, and return.
	      CSMP LADvsH;
	      LADvsH.add (x0 * Canopy.Height, 0.0);
	      LADvsH.add (x1 * Canopy.Height, 
			  y1 * Canopy.LADm);
	      LADvsH.add (     Canopy.Height, 0.0);
	      Canopy.LAIvsH = LADvsH.integrate_stupidly ();
	      return;
	    }
	  // It is enough to z1 closer to z2.
	  z1 += Area - Need;
	  z0 += Area - Need;
	}
    }
    
  // Create CSMP for standard "z0, z1, z2" distribution.
  CSMP LADvsH;
  LADvsH.add (z0 * Canopy.Height, 0.0);
  LADvsH.add (z1 * Canopy.Height, Canopy.LADm);
  LADvsH.add (z2 * Canopy.Height, Canopy.LADm);
  LADvsH.add (     Canopy.Height, 0.0);
  Canopy.LAIvsH = LADvsH.integrate_stupidly ();
}

double
CropStandard::ActualWaterUptake (double Ept,
				 const Soil& soil, SoilWater& soil_water,
				 const double EvapInterception)
{
  if (Ept < 0)
    {
      cerr << "\nBUG: Negative EPT (" << Ept << ")\n";
      Ept = 0.0;
    }
  assert (EvapInterception >= 0);
  static const double min_step = 1.0;
  const double h_wp = par.Root.h_wp;
  double& h_x = var.RootSys.h_x;
  double total = PotentialWaterUptake (h_x, soil, soil_water);
  double step = min_step;
  var.RootSys.Ept = Ept;

  while (total < Ept && h_x > h_wp)
    {
      const double h_next = max (h_x - step, h_wp);
      const double next = PotentialWaterUptake (h_next, soil, soil_water);
      
      if (next < total)
	// We are past the top of the curve.
	if (step <= min_step)
	  // We cannot go any closer to the top, skip it.
	  {
	    h_x = h_wp;
	    total = PotentialWaterUptake (h_x, soil, soil_water);
	    break;
	  }
	else
	  // Try again a little close.
	  {
	    step /= 2;
	    continue;
	  }
      total = next;
      h_x = h_next;
      step *= 2;
    }
  if (h_x < h_wp)
    h_x = h_wp;

  step = min_step;
  assert (h_x < 0.001);
  while (total > Ept && h_x < 0.0)
    {
      assert (h_x < 0.001);
      const double h_next = min (h_x + step, 0.0);
      const double next = PotentialWaterUptake (h_next, soil, soil_water);

      if (next < Ept)
	// We went too far.
	if (step <= min_step)
	  {
	    // We can't get any closer.
	    assert (next <= total);
	    if (next >= Ept)
	      {
		total = next;
		h_x = h_next;
	      }
	    else 
	      
	    break;
	  }
	else
	  // Try again a little closer.
	  {
	    step /= 2;
	    continue;
	  }
      
      total = next;
      h_x = h_next;
      step *= 2;
    }

  // We need this to make sure H2OExtraction corresponds to `h_x'.
  const double total2 = PotentialWaterUptake (h_x, soil, soil_water);
  assert (total == total2);

  vector<double>& H2OExtraction = var.RootSys.H2OExtraction;
  if (total > Ept)
    {
      assert (h_x < 0.001);
      assert (total > 0);
      const double factor = Ept / total;
      for (unsigned int i = 0; i < soil.size (); i++)
	H2OExtraction[i] *= factor;
      total = Ept;
    }
  // Update soil water sink term.
  soil_water.add_to_sink (H2OExtraction);
  // Update water stress factor
  double& water_stress = var.RootSys.water_stress;
  if (Ept < 0.010)
    water_stress = 1.0;
  else
    water_stress = (total + EvapInterception) / (Ept + EvapInterception);

  var.CrpAux.H2OUpt = total;
  return total;
}

double
CropStandard::PotentialWaterUptake (const double h_x, 
				    const Soil& soil, const SoilWater& soil_water)
{
  const double Rxylem = par.Root.Rxylem;
  const double area = M_PI * par.Root.Rad * par.Root.Rad;
  const vector<double>& L = var.RootSys.Density;
  vector<double>& S = var.RootSys.H2OExtraction;
  double total = 0.0;
  for (unsigned int i = 0; i < soil.size () && L[i] > 0.0; i++)
    {
      const double h = h_x - (1 + Rxylem) * soil.z (i);
      const double uptake = max (L[i] * (soil.Theta (i, h) / soil.Theta (i, 0.0))
				      * (soil.M (i, soil_water.h (i)) - soil.M (i, h))
                                      / (- 0.5 * log (area * L[i])),
                                 0.0);
      assert (L[i] >= 0.0);
      assert (soil.Theta (i, h) > 0.0);
      assert (soil.Theta (i, 0.0) > 0.0);
      assert (soil.M (i, soil_water.h (i)) >= 0.0);
      assert (soil.M (i, h) >= 0.0);
      assert (area * L[i] > 0.0);
      assert ((- 0.5 * log (area * L[i])) != 0.0);
      assert (uptake >= 0.0);
      S[i] = uptake;
      total += uptake * soil.dz (i) * 10; // mm/cm.
    }
  return total;
}

void 
CropStandard::RootPenetration (const Soil& soil, const SoilHeat& soil_heat)
{
  const Parameters::RootPar& Root = par.Root;
  // const double DS = var.Phenology.DS;
  const double IncWRoot = var.CrpAux.IncWRoot;
  double& PotRtDpt = var.CrpAux.PotRtDpt;
  double& Depth = var.RootSys.Depth;
    
  if (IncWRoot <= 0)
    return;

  double Ts = soil_heat.T (soil.interval_plus (-Depth));
  double dp = Root.PenPar1 * max (0.0, Ts - Root.PenPar2);
  PotRtDpt = min (PotRtDpt + dp, Root.MaxPen);
  /*max depth determined by crop*/
  Depth = min (Depth + dp, Root.MaxPen);
  /*max depth determined by crop*/
  Depth = min (Depth, -soil.MaxRootingDepth ()); /*or by soil conditions*/
}

double 
CropStandard::RootDensDistPar (double a)
{
  double x, y, z, x1, y1, z1, x2, y2, z2;

  if (1 + a > exp (1.0))
    {
      x1 = 1.0;
      y1 = exp (x1);
      z1 = 1 + a * x1;
      x2 = 2.0;
      y2 = exp (x2);
      z2 = 1 + a * x2;
      while ((z1 - y1) * (z2 - y2) > 0)
	{
	  x1 = x2;
	  y1 = y2;
	  z1 = z2;
	  x2++;
	  y2 = exp (x2);
	  z2 = 1 + a * x2;
	}
    }
  else if (a >= 0.3)
    {
      x1 = 0.3;
      y1 = exp (x1);
      z1 = 1 + a * x1;
      x2 = 1.0;
      y2 = exp (x2);
      z2 = 1 + a * x2;
    }
  else
    {
      assert (false /* Invalid Root Distribution */);
    }
  x = (y2 * (x2 - 1) - y1 * (x1 - 1)) / (y2 - y1);
  y = exp (x);
  z = 1 + a * x;
  while (fabs (2 * (z - y) / (z + y)) > 1.0e-5)
    {
      if (z - y > 0)
	{
	  x1 = x;
	  y1 = y;
	  z1 = z;
	}
      else
	{
	  x2 = x;
	  y2 = y;
	  z2 = z;
	}
      x = (y2 * (x2 - 1) - y1 * (x1 - 1)) / (y2 - y1);
      y = exp (x);
      z = 1 + a * x;
    }
  return x;
}

void 
CropStandard::RootDensity (const Soil& soil)
{
  const Parameters::RootPar& Root = par.Root;
  const double WRoot = var.Prod.WRoot;
  const double PotRtDpt = var.CrpAux.PotRtDpt;
  Variables::RecRootSys& RootSys = var.RootSys;

  double LengthPrArea
    = max (10 * Root.SpRtLength * WRoot, 3 * PotRtDpt); /*cm/cm2*/
  double a = RootDensDistPar (LengthPrArea / (PotRtDpt * Root.DensRtTip));
  double L0 = Root.DensRtTip * exp (a);
  a /= PotRtDpt;
  if (RootSys.Depth < PotRtDpt)
    {
      double Lz = L0 * exp (-a * RootSys.Depth);
      a =   RootDensDistPar (LengthPrArea / (RootSys.Depth * Lz))
	/ RootSys.Depth;
    }
  
  vector<double>& d = var.RootSys.Density;
  
  unsigned int i = 0;
  for (; i == 0 || -soil.zplus (i-1) < RootSys.Depth; i++)
    d[i] = L0 * exp (a * soil.z (i));
  assert (i < soil.size ());
  for (; i < soil.size (); i++)
    d[i] = 0.0;
}

void 
CropStandard::NitContent ()
{
  const Parameters::CrpNPar& CrpN = par.CrpN;
  const double DS = var.Phenology.DS;
  const Variables::RecProd& Prod = var.Prod;

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
}

void
CropStandard::NitrogenUptake (int Hour, 
			      const Soil& soil,
			      const SoilWater& soil_water,
			      SoilNH4& soil_NH4,
			      SoilNO3& soil_NO3)
{
  const Parameters::RootPar& Root = par.Root;
  Variables::RecCrpAux& CrpAux = var.CrpAux;
  double& NCrop = var.Prod.NCrop;
  Variables::RecRootSys& RootSys = var.RootSys;

  double PotNUpt = (CrpAux.PtNCnt - NCrop) / ((Hour == 0) ? 1 : (25 - Hour));
  if (PotNUpt > 0)
    {
      CrpAux.NH4Upt
	= SoluteUptake (soil, soil_water, soil_NH4, PotNUpt, 
			RootSys.NH4Extraction, Root.MxNH4Up, Root.Rad);
      assert (CrpAux.NH4Upt >= 0.0);

      NCrop += CrpAux.NH4Upt;
      PotNUpt -= CrpAux.NH4Upt;
    }
  else
    CrpAux.NH4Upt = 0.0;
  if (PotNUpt > 0)
    {
      CrpAux.NO3Upt
	= SoluteUptake (soil, soil_water, soil_NO3, PotNUpt,
			RootSys.NO3Extraction, Root.MxNO3Up, Root.Rad); 
      assert (CrpAux.NO3Upt >= 0.0);
      NCrop += CrpAux.NO3Upt;
      PotNUpt -= CrpAux.NO3Upt;
    }
  else
    CrpAux.NO3Upt = 0.0;

  if (PotNUpt > 0 && var.Phenology.DS > var.CrpAux.DS_start_fixate)
    {
      CrpAux.Fixated = 0.8 * PotNUpt;
      NCrop += CrpAux.Fixated;
      PotNUpt -= CrpAux.Fixated;
    }
  else
    CrpAux.Fixated = 0.0;
}

double 
CropStandard::CanopyPhotosynthesis (const Bioclimate& bioclimate)
{
  // sugar production [gCH2O/m2/h] by canopy photosynthesis.
  const Parameters::LeafPhotPar& LeafPhot = par.LeafPhot;
  const CSMP& LAIvsH = var.Canopy.LAIvsH;
  const double Ta = bioclimate.AirTemperature ();
  const double Teff = LeafPhot.TempEff (Ta); // Temperature effect


  double prevLA = 0.0;		// LAI below the current leaf layer.
  double Ass = 0.0;	// Assimilate produced by canopy photosynthesis

  int No = bioclimate.NumberOfIntervals ();
  for (int i = 0; i < No; i++)
    {
      const double height = bioclimate.height (i);
      // Leaf Area index for a given leaf layer
      const double LA = LAIvsH (height) - prevLA;
      prevLA = LAIvsH (height);
      if (LA > 0)
	{
	  const double dPAR
	    = (bioclimate.PAR (i) - bioclimate.PAR (i + 1)) / LA;

	  // Leaf Photosynthesis [gCO2/m2/h]
	  const double F = LeafPhot.Fm * 
	    (1.0 - exp (- (LeafPhot.Qeff * dPAR / LeafPhot.Fm)));

	  Ass += LA * F;
	}
    }
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
      const double ReMobilization = Prod.ReMobilRt * StemRes;
      StemRes = StemRes-ReMobilization;
      return ReMobilization;
    }
}

void 
CropStandard::AssimilatePartitioning (double DS, 
			      double& f_Leaf, double& f_Stem,
			      double& f_Root, double& f_SOrg)
{
  const Parameters::PartitPar& Partit = par.Partit;
    
  f_Root = Partit.Root (DS);
  f_Leaf = (1 - f_Root) * Partit.Leaf (DS);
  f_Stem = (1 - f_Root) * Partit.Stem (DS);
  f_SOrg = max (0.0, 1 - f_Root - f_Leaf - f_Stem);
}

double 
CropStandard::MaintenanceRespiration (double r, double w, double T)
{
  if (w <= 0.0)
    return 0.0;

  return (molWeightCH2O / molWeightCO2) 
    * r * max (0.0, 0.4281 * (exp (0.57 - 0.024 * T + 0.0020 * T * T)
			      - exp (0.57 - 0.042 * T - 0.0051 * T * T))) * w;
}

void 
CropStandard::NetProduction (const Bioclimate& bioclimate,
			     const Geometry& geometry,
			     const SoilHeat& soil_heat)
{
  const Parameters::ProdPar& pProd = par.Prod;
  const double DS = var.Phenology.DS;
  const double Depth = var.RootSys.Depth;
  Variables::RecProd& vProd = var.Prod;
  Variables::RecCrpAux& CrpAux = var.CrpAux;

  const double AirT = bioclimate.AirTemperature ();
  const double SoilT = soil_heat.T (geometry.interval_plus (-Depth / 3));
  double RMLeaf
    = MaintenanceRespiration (pProd.r_Leaf, vProd.WLeaf, AirT);
  const double RMStem
    = MaintenanceRespiration (pProd.r_Stem, vProd.WStem, AirT);
  const double RMSOrg
    = MaintenanceRespiration (pProd.r_SOrg, vProd.WSOrg, AirT);
  const double RMRoot
    = MaintenanceRespiration (pProd.r_Root, vProd.WRoot, SoilT);

  const double ReMobil = ReMobilization ();
  CrpAux.CanopyAss += ReMobil;

  RMLeaf = max (0.0, RMLeaf - CrpAux.PotCanopyAss + CrpAux.CanopyAss);
  const double RM = RMLeaf + RMStem + RMSOrg + RMRoot;
    
  if (CrpAux.CanopyAss >= RM)
    {
      const double AssG = CrpAux.CanopyAss - RM;
      const double Stress 
	= max (0.0, min (1.0, ((vProd.NCrop - CrpAux.NfNCnt) 
			       / (CrpAux.CrNCnt - CrpAux.NfNCnt))));
      double f_Leaf, f_Stem, f_SOrg, f_Root;
      AssimilatePartitioning (DS, f_Leaf, f_Stem, f_Root, f_SOrg);
      CrpAux.IncWLeaf = Stress * pProd.E_Leaf * f_Leaf * AssG;
      CrpAux.IncWStem = Stress * pProd.E_Stem * f_Stem * AssG - ReMobil;
      CrpAux.IncWSOrg = Stress * pProd.E_SOrg * f_SOrg * AssG;
      CrpAux.IncWRoot = Stress * pProd.E_Root * f_Root * AssG;
    }
  else
    {
      double AssG;

      if (RMLeaf <= CrpAux.CanopyAss)
	{
	  CrpAux.IncWLeaf = 0.0;
	  AssG = CrpAux.CanopyAss - RMLeaf;
	}
      else
	{
	  CrpAux.IncWLeaf = CrpAux.CanopyAss - RMLeaf;
	  AssG = 0.0;
	}
      if (RMSOrg <= AssG)
	{
	  CrpAux.IncWSOrg = 0.0;
	  AssG -= RMSOrg;
	}
      else
	{
	  CrpAux.IncWSOrg = AssG - RMSOrg;
	  AssG = 0.0;
	}
      if (RMStem <= AssG)
	{
	  CrpAux.IncWRoot = 0.0;
	  AssG -= RMStem;
	}
      else
	{
	  CrpAux.IncWRoot = AssG - RMRoot;
	  AssG = 0.0;
	}
      if (RMRoot <= AssG)
	{
	  CrpAux.IncWRoot = 0.0;
	  AssG -= RMRoot;
	}
      else
	{
	  CrpAux.IncWRoot = AssG - RMRoot;
	  AssG = 0.0;
	}
      if (AssG > 0.0)
	cerr << "BUG: Extra AssG: " << AssG << "\n";
    }

  // Update dead leafs
  CrpAux.DeadWLeaf = pProd.LfDR (DS) * vProd.WLeaf;
  CrpAux.DeadNLeaf = par.CrpN.NfLeafCnc (DS) * CrpAux.DeadWLeaf;
  CrpAux.IncWLeaf -= CrpAux.DeadWLeaf;
  vProd.AM_leaf->add (par.Harvest.C_Dead * CrpAux.DeadWLeaf, CrpAux.DeadNLeaf);

  // Update dead roots.
  CrpAux.DeadWRoot = pProd.RtDR (DS) * vProd.WRoot;
  CrpAux.DeadNRoot = par.CrpN.DdRootCnc (DS) * CrpAux.DeadWRoot;
  CrpAux.IncWRoot -= CrpAux.DeadWRoot;
  vProd.AM_root->add (geometry, 
		      par.Harvest.C_Root * CrpAux.DeadWRoot, CrpAux.DeadNRoot,
		      var.RootSys.Density);

  // Update production.
  vProd.NCrop -= (CrpAux.DeadNLeaf + CrpAux.DeadNRoot);
  assert (vProd.NCrop > 0.0);
  vProd.WLeaf += CrpAux.IncWLeaf;
  vProd.WStem += CrpAux.IncWStem;
  vProd.WSOrg += CrpAux.IncWSOrg;
  vProd.WRoot += CrpAux.IncWRoot;
}

void 
CropStandard::tick (const Time& time,
		    const Bioclimate& bioclimate,
		    const Soil& soil,
		    OrganicMatter& organic_matter,
		    const SoilHeat& soil_heat,
		    const SoilWater& soil_water, 
		    SoilNH4& soil_NH4,
		    SoilNO3& soil_NO3)
{
  // Clear log.
  fill (var.RootSys.NO3Extraction.begin (), 
	var.RootSys.NO3Extraction.end (),
	0.0);
  fill (var.RootSys.NH4Extraction.begin (), 
	var.RootSys.NH4Extraction.end (),
	0.0);

  if (time.hour () == 0 && var.Phenology.DS <= 0)
    {
      Emergence (soil, soil_heat);
      if (var.Phenology.DS >= 0)
	{
	  InitialLAI ();
	  var.Canopy.Height = CropHeight ();
	  NitContent ();
	  var.CrpAux.PotCanopyAss = 0.0;
	  var.CrpAux.CanopyAss = 0.0;
	  RootDensity (soil);
	  
	  var.Prod.AM_root 
	    = &AM::create (soil, time, par.Harvest.Root,
			   name, "root", AM::Locked);
	  var.Prod.AM_leaf
	    = &AM::create (soil, time, par.Harvest.Dead,
			   name, "dead", AM::Locked);

	  organic_matter.add (*var.Prod.AM_root);
	  organic_matter.add (*var.Prod.AM_leaf);
	}
      return;
    }
  if (var.Phenology.DS <= 0 || var.Phenology.DS >= 2)
    return;
  const double water_stress = var.RootSys.water_stress;
  NitrogenUptake (time.hour (), 
		  soil, soil_water, soil_NH4,soil_NO3);
  if (bioclimate.PAR (bioclimate.NumberOfIntervals () - 1) > 0)
    {
      double Ass = CanopyPhotosynthesis (bioclimate);
      var.CrpAux.PotCanopyAss += Ass;
      var.CrpAux.CanopyAss += water_stress * Ass;
    }
  if (time.hour () != 0)
    return;
  DevelopmentStage (bioclimate);
  var.Canopy.Height = CropHeight ();
  if (var.CrpAux.InitLAI)
    InitialLAI ();
  else
    var.Canopy.LAI = CropLAI ();
  NetProduction (bioclimate, soil, soil_heat);
  RootPenetration (soil, soil_heat);
  RootDensity (soil);
  NitContent ();
  var.CrpAux.PotCanopyAss = 0.0;
  var.CrpAux.CanopyAss = 0.0;
}

const Harvest&
CropStandard::harvest (const string column_name,
		       const Time& time, 
		       const Geometry& geometry, 
		       OrganicMatter& organic_matter,
		       double stub_length, 
		       double stem_harvest, 
		       double leaf_harvest, 
		       double sorg_harvest,
		       bool kill_off)
{
  const Parameters::HarvestPar& Hp = par.Harvest;
  Variables::RecProd& Prod = var.Prod;
  const double DS = var.Phenology.DS;
  const double DSmax = Hp.DSmax;

  const double beta = Hp.beta;
  const double alpha = Hp.alpha;
  double CStraw = Hp.CStraw;
  double CSOrg = Hp.CSOrg;

  const double WStem = Prod.WStem;
  const double WLeaf = Prod.WLeaf;
  const double WSOrg = Prod.WSOrg;
  const double WRoot = Prod.WRoot;
  const double NCrop = Prod.NCrop;

  const double WShoot = WSOrg + WLeaf + WStem;

  const double WCrop = WShoot + WRoot;

  // Calculate NRoot.
  double NRoot = 0.0;
  if (WRoot > 0.0)
    {
      const double C = NCrop / WCrop;
      const double B = WRoot / WCrop;
      const double A = WShoot / WCrop / (beta * beta);

      const double CRoot = (sqrt (B * B + 4.0 * A * C) - B) / (2.0 * A);
      NRoot = CRoot * WRoot;
    }

  // Calculate N in shoot.
  double NSOrg = 0.0;
  double NStem = 0.0;
  double NLeaf = 0.0;

  const double NShoot = NCrop - NRoot;

  if (WSOrg > 0.0)
    {
      const double WStraw = WLeaf + WStem;
      const double CShoot = NShoot / WShoot;
      const double Climit = (CStraw * WStraw + CSOrg * WSOrg);
  
      if (WSOrg == 0)
	CStraw = NShoot / WStraw;
      else if (CShoot <= Climit)
	CSOrg = (NShoot - CStraw * WStraw) / WSOrg;
      else
	{
	  CStraw = ((CStraw * WSOrg + alpha * (NShoot - CSOrg * WSOrg))
		    / (WSOrg + alpha * WStraw));
	  CSOrg = (NShoot - CStraw * WStraw) / WSOrg;
	}
      const double NStraw = WStraw * CStraw;
      NSOrg = WSOrg * CSOrg;

      if (NShoot > 0.0)
	assert (fabs (NShoot / (NStraw + NSOrg) - 1.0) < 0.0001);
  
      if (WStraw > 0.0)
	{
	  NStem = NStraw * (WStem / WStraw);
	  NLeaf = NStraw * (WLeaf / WStraw);
	}
    }
  else if (WShoot > 0.0)
    {
      NStem = NShoot * (WStem / WShoot);
      NLeaf = NShoot * (WLeaf / WShoot);
    }
  else if (NShoot > 0.0 && (DS >= DSmax || kill_off))
    {
      cerr << "\nLost " << NShoot << " N in empty shoot.\n";
    }

  const double C_Stem = Hp.C_Stem;
  const double C_Leaf = Hp.C_Leaf;
  const double C_SOrg = Hp.C_SOrg;
  const double C_Root = Hp.C_Root;

  const vector<const AttributeList*>& Stem = Hp.Stem;
  const vector<const AttributeList*>& Leaf = Hp.Leaf;
  const vector<const AttributeList*>& SOrg = Hp.SOrg;

  const vector<double>& density = var.RootSys.Density;
  const double length = height ();

  if (stub_length < length)
    stem_harvest *= (1.0 - stub_length / length);

  if (!kill_off && DS < DSmax)
    {
      // Cut back development stage and production.
      const double DSnew = Hp.DSnew;

      if (DS > DSnew)
	var.Phenology.DS = DSnew;
      
      // Stop fixation after cut.
      if (DS > var.CrpAux.DS_start_fixate)
	var.CrpAux.DS_start_fixate = par.CrpN.DS_cut_fixate;

      Prod.WStem *= (1.0 - stem_harvest); 
      Prod.WLeaf *= (1.0 - leaf_harvest);
      Prod.WSOrg *= (1.0 - sorg_harvest);
      Prod.NCrop -= (  NStem * stem_harvest
		     + NLeaf * leaf_harvest
		     + NSOrg * sorg_harvest);
    }
  else
    {
      var.Phenology.DS = DSremove;

      // Add crop remains to the soil.
      if (stem_harvest < 1.0 && WStem > 0.0)
	{
	  AM& am = AM::create (geometry, time, Stem, name, "stem");
	  am.add (WStem * C_Stem * (1.0 - stem_harvest), 
		  NStem * (1.0 - stem_harvest));
	  organic_matter.add (am);
	}
      if (leaf_harvest < 1.0 && WLeaf > 0.0)
	{
	  AM& am = AM::create (geometry, time, Leaf, name, "leaf");
	  am.add (WLeaf * C_Leaf * (1.0 - leaf_harvest), 
		  NLeaf * (1.0 - leaf_harvest));
	  organic_matter.add (am);
	}
      if (sorg_harvest < 1.0 && WSOrg > 0.0)
	{
	  AM& am = AM::create (geometry, time, SOrg, name, "sorg");
	  am.add (WSOrg * C_SOrg * (1.0 - sorg_harvest),
		  NSOrg * (1.0 - sorg_harvest));
	  organic_matter.add (am);
	}

      // Update and unlock locked AMs.
      if (var.Prod.AM_root)
	{
	  if (geometry.total (density) > 0.0)
	    var.Prod.AM_root->add (geometry, WRoot * C_Root, NRoot, density);
	  else
	    var.Prod.AM_root->add (WRoot * C_Root, NRoot);
	  var.Prod.AM_root->unlock ();
	  var.Prod.AM_root = NULL;
	}
      else
	assert (WRoot == 0.0);

      if (var.Prod.AM_leaf)
	{
	  var.Prod.AM_leaf->unlock ();
	  var.Prod.AM_leaf = NULL;
	}
    }
  return *new Harvest (column_name, time, name, 
		       WStem * stem_harvest, NStem * stem_harvest,
		       WLeaf * leaf_harvest, NLeaf * leaf_harvest,
		       WSOrg * sorg_harvest, NSOrg * sorg_harvest);
}

void
CropStandard::output (Log& log, Filter& filter) const
{
  var.output (log, filter);
}

double
CropStandard::DS () const
{ return var.Phenology.DS; }

CropStandard::CropStandard (const AttributeList& al, int layers)
  : Crop (al.name ("type")),
    par (*new Parameters (al)),
    var (*new Variables (par, al, layers))
{ }

CropStandard::~CropStandard ()
{ 
  delete &var;
}

