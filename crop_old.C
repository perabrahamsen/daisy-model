// crop_old.C

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
#include <list>

class CropOld : public Crop
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
  void AssimilatePartitioning (double DS, double& f_Leaf, double& f_Root);
  double MaintenanceRespiration (double r, double Q10,
					 double w, double T);
  void NetProduction (const Bioclimate&, const Soil&, const SoilHeat&);

  // Simulation.
public:
  void tick (const Time& time, const Bioclimate&, const Soil&,
	     OrganicMatter&,
	     const SoilHeat&,
	     const SoilWater&, 
	     SoilNH4&,
	     SoilNO3&);
  const Harvest& harvest (const string& column_name,
			  const Time&, const Geometry&, OrganicMatter&,
			  double stub_length, double stem_harvest,
			  double leaf_harvest, double sorg_harvest, 
			  bool kill_off);
  void output (Log&, Filter&) const;

  double DS () const;

  // Create and Destroy.
private:
  friend class CropOldSyntax;
  static Crop* make (const AttributeList&, int layers);
  CropOld (const AttributeList& vl, int layers);
public:
  ~CropOld ();
};

// Chemical constants affecting the crop.

const double molWeightCH2O = 30.0; // [gCH2O/mol]
const double molWeightCO2 = 44.0; // [gCO2/mol]

typedef void (*CropFun)(const Bioclimate&, CropOld&);

struct CropOld::Parameters
{ 
  const struct DevelPar
  {
    double EmrTSum;		// Soil temp sum at emergence
    double DS_Emr;		// Development stage (DS) emergence
    bool DS_reset;		// True for winther crops.
    double DSRate1;		// Development rate [C-1 or d-1],
    // the vegetative stage
    double DSRate2;		// Development rate [C-1 or d-1],
    // the reproductive stage
    const CSMP& TempEff1;   // Temperature effect, vegetative stage
    const CSMP& TempEff2;   // Temperature effect, reproductive stage
    const CSMP& PhotEff1;   // Ptotoperiode effect, vegetative stage
  private:
    friend struct CropOld::Parameters;
    DevelPar (const AttributeList&);
  } Devel;
  const struct VernalPar {
    bool required;
    double DSLim1;		// DS at beginning of vernalization
    double DSLim2;		// DS at end of vernalization
    double TaLim;		// Vernalization temp threshold
    double TaSum;		// Vernalization T-sum requirement
  private:
    friend struct CropOld::Parameters;
    VernalPar (const AttributeList&);
  } Vernal;
  const struct LeafPhotPar {
    double Qeff;		// Quantum efficiency at low light
    double Fm;		// Max assimilation rate
    double TLim1;		// Lowest temp for photosynthesis
    double TLim2;		// Lowest temp for unrestricted phot.
  private:
    friend struct CropOld::Parameters;
    LeafPhotPar (const AttributeList&);
  } LeafPhot;
  const struct CanopyPar {
    double InitGrowth;		// Initial growth parameter.
    double DSinit;		// DS at end of initial LAI-Development
    double WLfInit;		// WLeaf at end of initial LAI-Development
    double DS1;			// DS state at start of leaf recession.
    double alpha;		// Leaf recession parameter.
    double beta;		// Leaf recession parameter.
    double SpLAI;		// Specific leaf weight [ (m²/m²) / (g/m²) ]
    const CSMP& HvsDS;	// Crop height as function of DS
    const vector<double>& LAIDist0; // Relative LAI distribution at DS=0
    const vector<double>& LAIDist1; // Relative LAI distribution at DS=1
    double PARref;		// PAR reflectance
    double PARext;		// PAR extinction coefficient
    double EPext;		// EP extinction coefficient
  private:
    friend struct CropOld::Parameters;
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
    friend struct CropOld::Parameters;
    RootPar (const AttributeList&);
  } Root;
  const struct PartitPar {
    const CSMP& Root;	// Partitioning functions for root
  private:
    friend struct CropOld::Parameters;
    PartitPar (const AttributeList&);
  } Partit;
  struct RespPar {
    const CSMP& E_Root;		// Conversion efficiency, root
    const CSMP& E_Leaf;		// Conversion efficiency, leaf
    const CSMP& r_Root;		// Maint. resp. coeff., root
    const CSMP& r_Leaf;		// Maint. resp. coeff., leaf
    double Q10;		// Maint. resp. Q10-value
  private:
    friend struct CropOld::Parameters;
    RespPar (const AttributeList&);
  } Resp;
  struct CrpNPar {
    double SeedN;		// N-content in seed [ g N/m² ]
    double DS_fixate;		// Fixation of atmospheric N.
    const CSMP& PtLeafCnc;	// Upper limit for N-conc in leaves
    const CSMP& CrLeafCnc;	// Critical lim f. N-conc in leaves
    const CSMP& NfLeafCnc;	// Non-func lim f. N-conc in leaves
    const CSMP& PtRootCnc;	// Upper limit for N-conc in roots
    const CSMP& CrRootCnc;	// Critical lim f. N-conc in roots
    const CSMP& NfRootCnc;	// Non-func lim f. N-conc in roots
  private:
    friend struct CropOld::Parameters;
    CrpNPar (const AttributeList&);
  } CrpN;
  struct HarvestPar {
    const double index;		// Partition of leaf that are storage organ.
    const double beta;		// The root/top concentration relation
    const double CStem;	// Normal straw concentration
    const double CSOrg;		// Sorg conc. at the end of the normal range
    const double alpha;		// Rel. inc. in straw conc. above normal range
    const vector<AttributeList*>& Stem; // Stem AM parameters.
    const vector<AttributeList*>& Leaf; // Leaf AM parameters.
    const vector<AttributeList*>& SOrg; // SOrg AM parameters.
    const vector<AttributeList*>& Root; // Root AM parameters.
    const double C_Stem;	// C fraction of total weight.
    const double C_SOrg;	// C fraction of total weight.
    const double C_Root;	// C fraction of total weight.
    const double DSmax;		// Maximal development stage for which
				// the crop survives harvest.
    const double DSnew;		// Maximal development stage after harvest.
  private:
    friend struct CropOld::Parameters;
    HarvestPar (const AttributeList&);
  } Harvest;
  // Dunno where these belongs...
  double IntcpCap;
  double EpFac;
  bool enable_water_stress;
  bool enable_N_stress;
private:
  friend class CropOld;
  Parameters (const AttributeList&);
public:
  ~Parameters ();
};

struct CropOld::Variables
{ 
  void output (Log&, Filter&) const;
  struct RecPhenology
  {
    void output (Log&, Filter&) const;
    double DS;		// Development Stage
    double Vern;		// Vernalization criterium [C d]
  private:
    friend struct CropOld::Variables;
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
    friend struct CropOld::Variables;
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
    double ws_up;		// Water stress factor
    double ws_down;		// Water stress denominator (dk:tæller)
    double Ept;			// Potential evapotranspiration.
    double transpiration;	// Total water uptake.
  private:
    friend struct CropOld::Variables;
    RecRootSys (const Parameters&, const AttributeList&, int layers);
  } RootSys;
  struct RecProd
  {
    void output (Log&, Filter&) const;
    double WLeaf;		// Leaf dry matter weight [g/m2]
    double WhiteStubble;	// Old dead leftover from harvest.
    double WRoot;		// Root dry matter weight [g/m2]
    double NCrop;		// Nitrogen stored in dry matter [g/m2]
  private:
    friend struct CropOld::Variables;
    RecProd (const Parameters&, const AttributeList&);
  } Prod;
  struct RecCrpAux
  {
    void output (Log&, Filter&) const;
    bool InitLAI;		// Initial LAI development ?
    double PotRtDpt;	// Potential Root Penetration Depth [cm]
    double PtNCnt;		// Potential Nitrogen Content in Crop [g/m2]
    double CrNCnt;		// Critical Nitrogen Content in Crop [g/m2]
    double NfNCnt;		// Non-func Nitrogen Content in Crop [g/m2]
    double PotTransp;	// Potential Transpiration [mm/h]
    double PotCanopyAss;	// Potential Canopy Assimilation [g CH2O/m2/h]
    double CanopyAss;	// Canopy Assimilation [g CH2O/m2/h]
    double LogPotCanopyAss;	// The above is hourly accumulated values 
    double LogCanopyAss;	// over the day.  This is last days total.
    double IncWLeaf;	// Leaf growth [g DM/m2/d]
    double IncWRoot;	// Root growth [g DM/m2/d]
    // double H2OUpt;		// H2O uptake [mm/h]
    double NH4Upt;		// NH4-N uptake [g/m2/h]
    double NO3Upt;		// NO3-N uptake [g/m2/h]
    double Fixated;		// N fixation from air. [g/m2/h]
  private:
    friend struct CropOld::Variables;
    RecCrpAux (const Parameters&, const AttributeList&);
  } CrpAux;
private:
  friend class CropOld;
  Variables (const Parameters&, const AttributeList&, int layers);
public:
  ~Variables ();
};

CropOld::Parameters::Parameters (const AttributeList& vl) 
  : Devel (vl.alist ("Devel")),
    Vernal (vl.alist ("Vernal")),
    LeafPhot (vl.alist ("LeafPhot")),
    Canopy (vl.alist ("Canopy")),
    Root (vl.alist ("Root")),
    Partit (vl.alist ("Partit")),
    Resp (vl.alist ("Resp")),
    CrpN (vl.alist ("CrpN")),
    Harvest (vl.alist ("Harvest")),
    IntcpCap (vl.number ("IntcpCap")),
    EpFac (vl.number ("EpFac")),
    enable_water_stress (vl.flag ("enable_water_stress")),
    enable_N_stress (vl.flag ("enable_N_stress"))
{ }

CropOld::Parameters::DevelPar::DevelPar (const AttributeList& vl)
  : EmrTSum (vl.number ("EmrTSum")),
    DS_Emr (vl.number ("DS_Emr")),
    DS_reset (vl.flag ("DS_reset")),
    DSRate1 (vl.number ("DSRate1")),
    DSRate2 (vl.number ("DSRate2")),
    TempEff1 (vl.csmp ("TempEff1")),
    TempEff2 (vl.csmp ("TempEff2")),
    PhotEff1 (vl.csmp ("PhotEff1"))
{ }

CropOld::Parameters::VernalPar::VernalPar (const AttributeList& vl)
  : required (vl.flag ("required")),
    DSLim1 (vl.number ("DSLim1")),
    DSLim2 (vl.number ("DSLim2")),
    TaLim (vl.number ("TaLim")),
    TaSum (vl.number ("TaSum"))
{ }

CropOld::Parameters::LeafPhotPar::LeafPhotPar (const AttributeList& vl)
  : Qeff (vl.number ("Qeff")),
    Fm (vl.number ("Fm")),
    TLim1 (vl.number ("TLim1")),
    TLim2 (vl.number ("TLim2"))
{ }

CropOld::Parameters::CanopyPar::CanopyPar (const AttributeList& vl)
  : InitGrowth (vl.number ("InitGrowth")),
    DSinit (vl.number ("DSinit")),
    WLfInit (vl.number ("WLfInit")),
    DS1 (vl.number ("DS1")),
    alpha (vl.number ("alpha")),
    beta (vl.number ("beta")),
    SpLAI (vl.number ("SpLAI")),
    HvsDS (vl.csmp ("HvsDS")),
    LAIDist0 (vl.number_sequence ("LAIDist0")),
    LAIDist1 (vl.number_sequence ("LAIDist1")),
    PARref (vl.number ("PARref")),
    PARext (vl.number ("PARext")),
    EPext (vl.number ("EPext"))
{ }

CropOld::Parameters::RootPar::RootPar (const AttributeList& vl)
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

CropOld::Parameters::PartitPar::PartitPar (const AttributeList& vl)
  : Root (vl.csmp ("Root"))
{ }

CropOld::Parameters::RespPar::RespPar (const AttributeList& vl)
  : E_Root (vl.csmp ("E_Root")),
    E_Leaf (vl.csmp ("E_Leaf")),
    r_Root (vl.csmp ("r_Root")),
    r_Leaf (vl.csmp ("r_Leaf")),
    Q10 (vl.number ("Q10"))     
{ }

CropOld::Parameters::CrpNPar::CrpNPar (const AttributeList& vl)
  : SeedN (vl.number ("SeedN")),
    DS_fixate (vl.number ("DS_fixate")),
    PtLeafCnc (vl.csmp ("PtLeafCnc")),
    CrLeafCnc (vl.csmp ("CrLeafCnc")),
    NfLeafCnc (vl.csmp ("NfLeafCnc")),
    PtRootCnc (vl.csmp ("PtRootCnc")),
    CrRootCnc (vl.csmp ("CrRootCnc")),
    NfRootCnc (vl.csmp ("NfRootCnc"))
{ }

CropOld::Parameters::HarvestPar::HarvestPar (const AttributeList& vl)
  : index (vl.number ("index")),
    beta (vl.number ("beta")),
    CStem (vl.number ("CStem")),
    CSOrg (vl.number ("CSOrg")),
    alpha (vl.number ("alpha")),
    Stem (vl.alist_sequence ("Stem")),
    Leaf (vl.alist_sequence ("Leaf")),
    SOrg (vl.alist_sequence ("SOrg")),
    Root (vl.alist_sequence ("Root")),
    C_Stem (vl.number ("C_Stem")),
    C_SOrg (vl.number ("C_SOrg")),
    C_Root (vl.number ("C_Root")),
    DSmax (vl.number ("DSmax")),
    DSnew (vl.number ("DSnew"))
{ }

CropOld::Parameters::~Parameters ()
{ }

CropOld::Variables::Variables (const Parameters& par, 
				    const AttributeList& vl,
				    int layers)
  : Phenology (par, vl.alist ("Phenology")),
    Canopy (par, vl.alist ("Canopy")),
    RootSys (par, vl.alist ("RootSys"), layers),
    Prod (par, vl.alist ("Prod")),
    CrpAux (par, vl.alist ("CrpAux"))
{ }

void 
CropOld::Variables::output (Log& log, Filter& filter) const
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

CropOld::Variables::RecPhenology::RecPhenology (const Parameters& par,
						     const AttributeList& vl)
  : DS (vl.number ("DS")),
    Vern (vl.check ("Vern") ? vl.number ("Vern") : par.Vernal.TaSum)
{ }

void 
CropOld::Variables::RecPhenology::output (Log& log, Filter& filter) const
{
  log.open ("Phenology");
  log.output ("DS", filter, DS);
  log.output ("Vern", filter, Vern);
  log.close();
}

CropOld::Variables::RecCanopy::RecCanopy (const Parameters&,
					       const AttributeList& vl)
  : Height (vl.number ("Height")),
    LAI (vl.number ("LAI")),
    LADm (vl.number ("LADm")),
    LAIvsH (vl.csmp ("LAIvsH"))
{ }

void 
CropOld::Variables::RecCanopy::output (Log& log, Filter& filter) const
{
  log.open ("Canopy");
  log.output ("Height", filter, Height);
  log.output ("LAI", filter, LAI);
  log.output ("LADm", filter, LADm);
  log.output ("LAIvsH", filter, LAIvsH);
  log.close();
}

CropOld::Variables::RecRootSys::RecRootSys (const Parameters& par,
						 const AttributeList& vl, 
						 int layers)
  : Depth (vl.check ("Depth") ? vl.number ("Depth") : par.Root.DptEmr),
    Density (vl.number_sequence ("Density")),
    H2OExtraction (vl.number_sequence ("H2OExtraction")),
    NH4Extraction (vl.number_sequence ("NH4Extraction")),
    NO3Extraction (vl.number_sequence ("NO3Extraction")),
    h_x (vl.number ("h_x")),
    water_stress (1.0),
    ws_up (0.0),
    ws_down (0.0),
    Ept (0.0),
    transpiration (0.0)
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
CropOld::Variables::RecRootSys::output (Log& log, Filter& filter) const
{
  log.open ("RootSys");
  log.output ("Depth", filter, Depth);
  log.output ("Density", filter, Density);
  log.output ("H2OExtraction", filter, H2OExtraction);
  log.output ("NH4Extraction", filter, NH4Extraction);
  log.output ("NO3Extraction", filter, NO3Extraction);
  log.output ("h_x", filter, h_x);
  log.output ("water_stress", filter, water_stress, true);
  log.output ("transpiration", filter, transpiration, true);
  log.output ("Ept", filter, Ept, true);
  log.close();
}

CropOld::Variables::RecProd::RecProd (const Parameters& par, 
					   const AttributeList& vl)
  : WLeaf (vl.number ("WLeaf")),
    WhiteStubble (vl.number ("WhiteStubble")),
    WRoot (vl.number ("WRoot")),
    NCrop (vl.check ("NCrop") ? vl.number ("NCrop") : par.CrpN.SeedN)
{ }

void 
CropOld::Variables::RecProd::output (Log& log, Filter& filter) const
{
  log.open ("Prod");
  log.output ("WLeaf", filter, WLeaf);
  log.output ("WhiteStubble", filter, WhiteStubble);
  log.output ("WRoot", filter, WRoot);
  log.output ("NCrop", filter, NCrop);
  log.close();
}

CropOld::Variables::RecCrpAux::RecCrpAux (const Parameters& par, 
					       const AttributeList& vl)
  : InitLAI (vl.flag ("InitLAI")),
    PotRtDpt (  vl.check ("PotRtDpt") 
	      ? vl.number ("PotRtDpt")
	      : par.Root.DptEmr),
    PtNCnt (0.0),
    CrNCnt (0.0),
    NfNCnt (0.0),
    PotTransp (vl.number ("PotTransp")),
    PotCanopyAss (vl.number ("PotCanopyAss")),
    CanopyAss (vl.number ("CanopyAss")),
    LogPotCanopyAss (0.0),
    LogCanopyAss (0.0),
    IncWLeaf (0.0),
    IncWRoot (0.0),
    // H2OUpt (vl.number ("H2OUpt")),
    NH4Upt (0.0),
    NO3Upt (0.0),
    Fixated (0.0)
{ }

void 
CropOld::Variables::RecCrpAux::output (Log& log, Filter& filter) const
{
  log.open ("CrpAux");
  log.output ("InitLAI", filter, InitLAI);
  log.output ("PotRtDpt", filter, PotRtDpt);
  log.output ("PtNCnt", filter, PtNCnt, true);
  log.output ("CrNCnt", filter, CrNCnt, true);
  log.output ("NfNCnt", filter, NfNCnt, true);
  log.output ("PotTransp", filter, PotTransp);
  log.output ("PotCanopyAss", filter, PotCanopyAss);
  log.output ("CanopyAss", filter, CanopyAss);
  log.output ("LogPotCanopyAss", filter, LogPotCanopyAss, true);
  log.output ("LogCanopyAss", filter, LogCanopyAss, true);
  log.output ("IncWLeaf", filter, IncWLeaf, true);
  log.output ("IncWRoot", filter, IncWRoot, true);
  // log.output ("H2OUpt", filter, H2OUpt);
  log.output ("NH4Upt", filter, NH4Upt, true);
  log.output ("NO3Upt", filter, NO3Upt, true);
  log.output ("Fixated", filter, Fixated, true);
  log.close();
}

CropOld::Variables::~Variables ()
{ }

// Add the Crop syntax to the syntax table.
Crop*
CropOld::make (const AttributeList& vl, int layers)
{
  return new CropOld (vl, layers);
}

static struct CropOldSyntax
{
  CropOldSyntax ();
} old_crop_syntax;

#ifdef BORLAND_TEMPLATES
template class add_submodule<OM>;
#endif

CropOldSyntax::CropOldSyntax ()
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
  Devel.add ("DS_reset", Syntax::Boolean, Syntax::Const);
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
  LeafPhot.add ("TLim1", Syntax::Number, Syntax::Const);
  LeafPhot.add ("TLim2", Syntax::Number, Syntax::Const);

  Canopy.add ("InitGrowth", Syntax::Number, Syntax::Const);
  Canopy.add ("DSinit", Syntax::Number, Syntax::Const);
  Canopy.add ("WLfInit", Syntax::Number, Syntax::Const);
  Canopy.add ("DS1", Syntax::Number, Syntax::Const);
  Canopy.add ("alpha", Syntax::Number, Syntax::Const);
  Canopy.add ("beta", Syntax::Number, Syntax::Const);
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

  // RespPar
  Syntax& Resp = *new Syntax ();
  syntax.add ("Resp", Resp, Syntax::Const);

  Resp.add ("E_Root", Syntax::CSMP, Syntax::Const);
  Resp.add ("E_Leaf", Syntax::CSMP, Syntax::Const);
  Resp.add ("r_Root", Syntax::CSMP, Syntax::Const);
  Resp.add ("r_Leaf", Syntax::CSMP, Syntax::Const);
  Resp.add ("Q10", Syntax::Number, Syntax::Const);

  // CrpNPar
  Syntax& CrpN = *new Syntax ();
  AttributeList& CrpNList = *new AttributeList ();

  syntax.add ("CrpN", CrpN, Syntax::Const);
  alist.add ("CrpN", CrpNList);

  CrpN.add ("SeedN", Syntax::Number, Syntax::Const);
  CrpN.add ("DS_fixate", Syntax::Number, Syntax::Const);
  CrpNList.add ("DS_fixate", 42000.0);
  CrpN.add ("PtLeafCnc", Syntax::CSMP, Syntax::Const);
  CrpN.add ("CrLeafCnc", Syntax::CSMP, Syntax::Const);
  CrpN.add ("NfLeafCnc", Syntax::CSMP, Syntax::Const);
  CrpN.add ("PtRootCnc", Syntax::CSMP, Syntax::Const);
  CrpN.add ("CrRootCnc", Syntax::CSMP, Syntax::Const);
  CrpN.add ("NfRootCnc", Syntax::CSMP, Syntax::Const);

  // HarvestPar
  Syntax& Harvest = *new Syntax ();
  AttributeList& HarvestList = *new AttributeList ();

  syntax.add ("Harvest", Harvest, Syntax::Const);
  alist.add ("Harvest", HarvestList);

  Harvest.add ("index", Syntax::Number, Syntax::Const);
  Harvest.add ("beta", Syntax::Number, Syntax::Const);
  Harvest.add ("CStem", Syntax::Number, Syntax::Const);
  Harvest.add ("CSOrg", Syntax::Number, Syntax::Const);
  Harvest.add ("alpha", Syntax::Number, Syntax::Const);
  add_submodule<OM> ("Stem", Harvest, HarvestList,
		     Syntax::Const, Syntax::Sequence);
  add_submodule<OM> ("Leaf", Harvest, HarvestList,
		     Syntax::Const, Syntax::Sequence);
  add_submodule<OM> ("SOrg", Harvest, HarvestList,
		     Syntax::Const, Syntax::Sequence);
  add_submodule<OM> ("Root", Harvest, HarvestList,
		     Syntax::Const, Syntax::Sequence);
  Harvest.add ("C_Stem", Syntax::Number, Syntax::Const);
  Harvest.add ("C_SOrg", Syntax::Number, Syntax::Const);
  Harvest.add ("C_Root", Syntax::Number, Syntax::Const);
  Harvest.add ("DSmax", Syntax::Number, Syntax::Const);
  HarvestList.add ("DSmax", 0.0);
  Harvest.add ("DSnew", Syntax::Number, Syntax::Const);
  HarvestList.add ("DSnew", 0.0);

   // I don't know where these belong.
  syntax.add ("IntcpCap", Syntax::Number, Syntax::Const);
  syntax.add ("EpFac", Syntax::Number, Syntax::Const);
  syntax.add ("enable_water_stress", Syntax::Boolean, Syntax::Const);
  alist.add ("enable_water_stress", true);
  syntax.add ("enable_N_stress", Syntax::Boolean, Syntax::Const);
  alist.add ("enable_N_stress", true);

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
  RootSys.add ("H2OExtraction", Syntax::Number, Syntax::State, Syntax::Sequence);
  vRootSys.add ("H2OExtraction", empty_array);
  RootSys.add ("NH4Extraction", Syntax::Number, Syntax::State, Syntax::Sequence);
  vRootSys.add ("NH4Extraction", empty_array);
  RootSys.add ("NO3Extraction", Syntax::Number, Syntax::State, Syntax::Sequence);
  vRootSys.add ("NO3Extraction", empty_array);
  RootSys.add ("h_x", Syntax::Number, Syntax::State);
  vRootSys.add ("h_x", 0.0);
  RootSys.add ("water_stress", Syntax::Number, Syntax::LogOnly);
  RootSys.add ("transpiration", Syntax::Number, Syntax::LogOnly);
  RootSys.add ("Ept", Syntax::Number, Syntax::LogOnly);

  // Prod
  Syntax& Prod = *new Syntax ();
  AttributeList& vProd = *new AttributeList ();
  syntax.add ("Prod", Prod, Syntax::State);
  alist.add ("Prod", vProd);

  Prod.add ("WLeaf", Syntax::Number, Syntax::State);
  vProd.add ("WLeaf", 0.001);
  Prod.add ("WhiteStubble", Syntax::Number, Syntax::State);
  vProd.add ("WhiteStubble", 0.0);
  Prod.add ("WRoot", Syntax::Number, Syntax::State);
  vProd.add ("WRoot", 0.001);
  Prod.add ("NCrop", Syntax::Number, Syntax::Optional);

  // CrpAux
  Syntax& CrpAux = *new Syntax ();
  AttributeList& vCrpAux = *new AttributeList ();
  syntax.add ("CrpAux", CrpAux, Syntax::State);
  alist.add ("CrpAux", vCrpAux);

  CrpAux.add ("InitLAI", Syntax::Boolean, Syntax::State);
  vCrpAux.add ("InitLAI", true);
  CrpAux.add ("PotRtDpt", Syntax::Number, Syntax::Optional);
  CrpAux.add ("PtNCnt", Syntax::Number, Syntax::LogOnly);
  CrpAux.add ("CrNCnt", Syntax::Number, Syntax::LogOnly);
  CrpAux.add ("NfNCnt", Syntax::Number, Syntax::LogOnly);
  CrpAux.add ("PotTransp", Syntax::Number, Syntax::State);
  vCrpAux.add ("PotTransp", 0.0);
  CrpAux.add ("PotCanopyAss", Syntax::Number, Syntax::State);
  vCrpAux.add ("PotCanopyAss", 0.0);
  CrpAux.add ("CanopyAss", Syntax::Number, Syntax::State);
  vCrpAux.add ("CanopyAss", 0.0);
  CrpAux.add ("LogPotCanopyAss", Syntax::Number, Syntax::LogOnly);
  CrpAux.add ("LogCanopyAss", Syntax::Number, Syntax::LogOnly);
  CrpAux.add ("IncWLeaf", Syntax::Number, Syntax::LogOnly);
  CrpAux.add ("IncWRoot", Syntax::Number, Syntax::LogOnly);
  // CrpAux.add ("H2OUpt", Syntax::Number, Syntax::State);
  // vCrpAux.add ("H2OUpt", 0.0);
  CrpAux.add ("NH4Upt", Syntax::Number, Syntax::LogOnly);
  CrpAux.add ("NO3Upt", Syntax::Number, Syntax::LogOnly);
  CrpAux.add ("Fixated", Syntax::Number, Syntax::LogOnly);

  Crop::add_type ("old", alist, syntax, &CropOld::make);
}

double
CropOld::height () const
{
  return var.Canopy.Height;
}

double
CropOld::LAI () const
{
  return var.Canopy.LAI;
}

const CSMP&
CropOld::LAIvsH () const
{
  return var.Canopy.LAIvsH;
}

double
CropOld::PARext () const
{
  return par.Canopy.PARext;
}

double
CropOld::PARref () const
{
  return par.Canopy.PARref;
}

double
CropOld::EPext () const
{
  return par.Canopy.EPext;
}

double
CropOld::IntcpCap () const
{
  return par.IntcpCap;
}

double
CropOld::EpFac () const
{
  return par.EpFac;
}

double
CropOld::SoluteUptake (const Soil& soil,
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
CropOld::Vernalization (double Ta)
{
  const Parameters::VernalPar& Vernal = par.Vernal;
  double& Vern = var.Phenology.Vern;
  double& DS = var.Phenology.DS;

  if (DS <= Vernal.DSLim1)
    return;
  Vern -= min (Ta - Vernal.TaLim, 0.0);
  if (DS > Vernal.DSLim2)
    DS = Vernal.DSLim2;
}

void 
CropOld::Emergence (const Soil& soil, const SoilHeat& soil_heat)
{
  const Parameters::DevelPar& Devel = par.Devel;
  const double EmrDpt = par.Root.DptEmr;
  double& DS = var.Phenology.DS;

  DS += soil_heat.T (soil.interval_plus (-EmrDpt)) / Devel.EmrTSum;
  if (DS > 0.0)
    DS = Devel.DS_Emr;
}

void 
CropOld::DevelopmentStage (const Bioclimate& bioclimate)
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
      if (Phenology.DS > 2.0)
	Phenology.DS = 2.0;
    }
}

double 
CropOld::CropHeight ()
{
  const Parameters::CanopyPar& Canopy = par.Canopy;
  double& DS = var.Phenology.DS;

  return Canopy.HvsDS (DS);
}

void 
CropOld::InitialLAI ()
{
  const Parameters::CanopyPar& Canopy = par.Canopy;
  const double WLeaf = var.Prod.WLeaf;
  double& DS = var.Phenology.DS;
  double& LAI = var.Canopy.LAI;
  bool& InitLAI = var.CrpAux.InitLAI;

  if (WLeaf >= Canopy.WLfInit)
    {
      if (DS > Canopy.DSinit)
	DS = Canopy.DSinit;
      CropLAI ();
      InitLAI = false;
    }
  else
    {
      LAI = 0.5 * (exp (Canopy.InitGrowth * DS) - 1);
    }
}

double 
CropOld::CropLAI ()
{    
  const Parameters::CanopyPar& Canopy = par.Canopy;
  const double DS = var.Phenology.DS;
  const double WLeaf = max (0.0, var.Prod.WLeaf - var.Prod.WhiteStubble);
  const double DS1 = Canopy.DS1;

  if (DS < DS1)
    return Canopy.SpLAI * WLeaf;

  const double LAI = Canopy.SpLAI * WLeaf
    * (1.0 - Canopy.alpha * (DS - DS1) / ((DS - DS1) + Canopy.beta));

  return max (0.0, LAI);
}

void 
CropOld::CanopyStructure ()
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
CropOld::ActualWaterUptake (double Ept,
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
		// total = next;
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
  if (Ept >= 0.010)
    {
      assert (var.RootSys.ws_up <= var.RootSys.ws_down);
      assert (total <= Ept);

      var.RootSys.ws_up += (total + EvapInterception);
      var.RootSys.ws_down += (Ept + EvapInterception);
      assert (var.RootSys.ws_up <= var.RootSys.ws_down);
    }
  // Update transpiration.
  double& transpiration = var.RootSys.transpiration;
  transpiration = total;

  return total;
}

double
CropOld::PotentialWaterUptake (const double h_x, 
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
CropOld::RootPenetration (const Soil& soil, const SoilHeat& soil_heat)
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
CropOld::RootDensDistPar (double a)
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
      // z1 = 1 + a * x1;
      x2 = 1.0;
      y2 = exp (x2);
      // z2 = 1 + a * x2;
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
	  // z1 = z;
	}
      else
	{
	  x2 = x;
	  y2 = y;
	  // z2 = z;
	}
      x = (y2 * (x2 - 1) - y1 * (x1 - 1)) / (y2 - y1);
      y = exp (x);
      z = 1 + a * x;
    }
  return x;
}

void 
CropOld::RootDensity (const Soil& soil)
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
CropOld::NitContent ()
{
  const Parameters::CrpNPar& CrpN = par.CrpN;
  const double DS = var.Phenology.DS;
  const Variables::RecProd& Prod = var.Prod;

  var.CrpAux.PtNCnt = CrpN.PtLeafCnc (DS) * Prod.WLeaf
    + CrpN.PtRootCnc (DS) * Prod.WRoot;

  var.CrpAux.CrNCnt = CrpN.CrLeafCnc (DS) * Prod.WLeaf
    + CrpN.CrRootCnc (DS) * Prod.WRoot;

  var.CrpAux.NfNCnt = CrpN.NfLeafCnc (DS) * Prod.WLeaf
    + CrpN.NfRootCnc (DS) * Prod.WRoot;
}

void
CropOld::NitrogenUptake (int Hour, 
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
    {
      CrpAux.NH4Upt = 0.0;
    }

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
    {
      CrpAux.NO3Upt = 0.0;
    }
  if (PotNUpt > 0 && var.Phenology.DS > par.CrpN.DS_fixate)
    {
      CrpAux.Fixated = 0.8 * PotNUpt;
      NCrop += CrpAux.Fixated;
      // PotNUpt -= CrpAux.Fixated;
    }
  else
    CrpAux.Fixated = 0.0;
}

double 
CropOld::CanopyPhotosynthesis (const Bioclimate& bioclimate)
{
  // sugar production [gCH2O/m2/h] by canopy photosynthesis.
  const Parameters::LeafPhotPar& LeafPhot = par.LeafPhot;
  const CSMP& LAIvsH = var.Canopy.LAIvsH;

  double Teff;			// Temperature effect
  double F;			// Leaf Photosynthesis [gCO2/m2/h]
  double prevLA = 0.0;		// LAI below the current leaf layer.
  double Ass = 0.0;		// Assimilate produced by canopy photosynthesis
  double Ta = bioclimate.AirTemperature ();
  
  if (Ta < LeafPhot.TLim1)
    Teff = 0.0;
  else
    {
      if (Ta > LeafPhot.TLim2)
	Teff = 1.0;
      else
	Teff =   (Ta - LeafPhot.TLim1)
	  / (LeafPhot.TLim2 - LeafPhot.TLim1);
    }

  double LA, dPAR;

  int No = bioclimate.NumberOfIntervals ();
  for (int i = 0; i < No; i++)
    {
      const double height = bioclimate.height (i);
      // Leaf Area index for a given leaf layer
      LA = LAIvsH (height) - prevLA;
      prevLA = LAIvsH (height);
      if (LA > 0)
	{
	  dPAR = (bioclimate.PAR (i) - bioclimate.PAR (i + 1)) / LA;

	  F = LeafPhot.Fm * (1.0 
			     - exp (- (LeafPhot.Qeff * dPAR / LeafPhot.Fm)));

	  Ass += LA * F;
	}
    }
  return (molWeightCH2O / molWeightCO2) * Teff * Ass;
}

void 
CropOld::AssimilatePartitioning (double DS, double& f_Leaf, double& f_Root)
{
  const Parameters::PartitPar& Partit = par.Partit;
    
  f_Root = Partit.Root (DS);
  f_Leaf = max (0.0, 1.0 - f_Root);
}

double 
CropOld::MaintenanceRespiration (double r, double /* Q10 */, 
				 double w, double T)
{
  if (w > 0)
    return r * max (0.0, 0.4281 * (exp (0.57 - 0.024 * T + 0.0020 * T * T)
				   - exp (0.57 - 0.042 * T - 0.0051 * T * T)))
      * w;
  // return r * exp ((T - 20.0) / 10.0 * log (Q10)) * w;
  else
    return 0.0;
}

void 
CropOld::NetProduction (const Bioclimate& bioclimate,
			const Soil& soil, const SoilHeat& soil_heat)
{
  const Parameters::RespPar& Resp = par.Resp;
  const double DS = var.Phenology.DS;
  const double Depth = var.RootSys.Depth;
  Variables::RecProd& Prod = var.Prod;
  Variables::RecCrpAux& CrpAux = var.CrpAux;

  const double RMLeaf
    = MaintenanceRespiration (Resp.r_Leaf (DS), Resp.Q10, Prod.WLeaf, 
			      bioclimate.AirTemperature ());
  const double RMRoot
    = MaintenanceRespiration (Resp.r_Root (DS), Resp.Q10, Prod.WRoot, 
			      soil_heat.T (soil.interval_plus (-Depth / 3)));
  double AssG, f_Leaf, f_Root;
    
  AssG = CrpAux.CanopyAss;
  if (par.enable_N_stress)
    AssG *= max (0.0, min (1.0, ((Prod.NCrop - CrpAux.NfNCnt) 
				 / (CrpAux.CrNCnt - CrpAux.NfNCnt))));
  
  AssimilatePartitioning (DS, f_Leaf, f_Root);
  CrpAux.IncWLeaf = Resp.E_Leaf (DS) * (f_Leaf * AssG - RMLeaf);
  if (CrpAux.IncWLeaf < 0.0)
    CrpAux.IncWLeaf = f_Leaf * AssG - RMLeaf;
  CrpAux.IncWRoot = Resp.E_Root (DS) * (f_Root * AssG - RMRoot);
  if (CrpAux.IncWRoot < 0.0)
    CrpAux.IncWRoot = f_Root * AssG - RMRoot;
  Prod.WLeaf += CrpAux.IncWLeaf;
  Prod.WRoot += CrpAux.IncWRoot;
}

void 
CropOld::tick (const Time& time,
	       const Bioclimate& bioclimate,
	       const Soil& soil,
	       OrganicMatter&,
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

  // It was a bad winther.
  if (par.Devel.DS_reset 
      && time.month () == 3
      && time.mday () == 1
      && time.hour () == 6)
    {
      var.Phenology.DS = 0.1;
    }

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
	}
      return;
    }
  if (var.Phenology.DS <= 0 || var.Phenology.DS >= 2)
    return;
  NitrogenUptake (time.hour (), 
		  soil, soil_water, soil_NH4,soil_NO3);
  if (time.hour () != 0)
    return;
  assert (var.RootSys.ws_up <= var.RootSys.ws_down);
  double& water_stress = var.RootSys.water_stress;
  if (var.RootSys.ws_down > 0)
    water_stress = var.RootSys.ws_up / var.RootSys.ws_down;
  else
    water_stress = 1.0;
  assert (water_stress >= 0.0 && water_stress <= 1.0);
  var.RootSys.ws_up = 0.0;
  var.RootSys.ws_down = 0.0;

  const double Ta = bioclimate.AirTemperature ();

  double Teff;

  if (Ta < par.LeafPhot.TLim1)
    Teff = 0.0;
  else
    {
      if (Ta > par.LeafPhot.TLim2)
	Teff = 1.0;
      else
	Teff =   (Ta - par.LeafPhot.TLim1)
	  / (par.LeafPhot.TLim2 - par.LeafPhot.TLim1);
    }

  // Fraction of Photosynthetically Active Radiation in Shortware
  // incomming radiation. 
  const double PARinSi = 0.48;	

  const double beta1 = 0.158;
  const double beta2 = 0.094;

  const double LAI = min (5.0, var.Canopy.LAI);
  const double Si = bioclimate.DailyRadiation () * (24*60*60); // [J/m²/d]
  const double Sad = PARinSi * (1.0 - par.Canopy.PARref)
    * (1.0 - exp (-par.Canopy.PARext * LAI)) * Si;
  const double epsilon = 0.15 - (beta1 - beta2 * LAI / (LAI + 3.0))
    * Sad / (Sad + 7.8e6);
  const double C = 15.7;	// [MJ/kg]
  const double Fcd = epsilon * Sad / C;	// [mg CH2O /m²/d]
  const double Ass = 0.001 * Fcd * Teff; // [g/m²/d]
  
  var.CrpAux.PotCanopyAss += Ass;
  if (Ass <= 0.0)
    /* do nothing */;
  else if (!par.enable_water_stress || var.CrpAux.InitLAI)
    var.CrpAux.CanopyAss += Ass;
  else
    var.CrpAux.CanopyAss += water_stress * Ass;

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
  var.CrpAux.LogPotCanopyAss = var.CrpAux.PotCanopyAss;
  var.CrpAux.LogCanopyAss = var.CrpAux.CanopyAss;
  var.CrpAux.PotCanopyAss = 0.0;
  var.CrpAux.CanopyAss = 0.0;
}

const Harvest&
CropOld::harvest (const string& column_name,
		  const Time& time, const Geometry& geometry, 
		  OrganicMatter& organic_matter,
		  double stub_length,
		  double stem_harvest, double, double sorg_harvest, 
		  bool kill_off)
{
  const Parameters::HarvestPar& Hp = par.Harvest;
  Variables::RecProd& Prod = var.Prod;
  const double DS = var.Phenology.DS;

  const double index = Hp.index;
  const double beta = Hp.beta;
  const double alpha = Hp.alpha;
  double CStem = Hp.CStem;
  double CSOrg = Hp.CSOrg;

  const double WLeaf = Prod.WLeaf;
  const double WRoot = Prod.WRoot;
  const double NCrop = Prod.NCrop;

  const double WStub = (stub_length < height () 
			? WLeaf * stub_length / height ()
			: WLeaf);
			
  const double WSOrg = index * (WLeaf - WStub);
  const double WStraw = WLeaf - WSOrg - WStub;

  const double WAll = WLeaf + WRoot;
  const double C = NCrop / WAll;
  const double B = WRoot / WAll;
  const double A = WLeaf / WAll / (beta * beta);

  const double CRoot = (sqrt (B * B + 4.0 * A * C) - B) / (2.0 * A);
  const double NRoot = CRoot * WRoot;


  const double NLeaf = NCrop - NRoot;
  const double CLeaf = NLeaf / WLeaf;

  const double WStem = WStub + WStraw;
  const double Climit = (CStem * WStem + CSOrg * WSOrg);
  
  if (WSOrg == 0.0)
    CStem = NLeaf / WStem;
  else if (CLeaf <= Climit)
    CSOrg = (NLeaf - CStem * WStem) / WSOrg;
  else
    {
      CStem = ((CStem * WSOrg + alpha * (NLeaf - CSOrg * WSOrg))
		/ (WSOrg + alpha * WStem));
      CSOrg = (NLeaf - CStem * WStem) / WSOrg;
    }
  const double NStem = WStem * CStem;
  const double NSOrg = WSOrg * CSOrg;
  const double NStraw = (WStraw / WStem) * NStem;
  const double NStub = NStem - NStraw;

  if (NLeaf > 0.0)
    assert (fabs (NLeaf / (NStem + NSOrg) - 1.0) < 0.0001);
  
  const double C_Stem = Hp.C_Stem;
  const double C_SOrg = Hp.C_SOrg;
  const double C_Root = Hp.C_Root;

  const vector<AttributeList*>& Stem = Hp.Stem;
  const vector<AttributeList*>& SOrg = Hp.SOrg;
  const vector<AttributeList*>& Root = Hp.Root;

  const vector<double>& density = var.RootSys.Density;

  const double DSmax = Hp.DSmax;

  if (!kill_off && DS < DSmax)
    {
      cout << "(survived)";
      // Cut back development stage and production.
      const double DSnew = Hp.DSnew;

      if (DS > DSnew)
	var.Phenology.DS = DSnew;

      Prod.WLeaf -= (WStraw * stem_harvest + WSOrg * sorg_harvest);
      Prod.WhiteStubble = 0.6 * Prod.WLeaf;
      Prod.NCrop -= (NStraw * stem_harvest + NSOrg * sorg_harvest);
    }
  else
    {
      var.Phenology.DS = DSremove;

      const double WStem = WStraw * (1.0 - stem_harvest) + WStub;
      const double NStem = NStraw * (1.0 - stem_harvest) + NStub;

      const double m2_per_cm2 = 0.0001;
      
      // Add crop remains to the soil.
      if (stem_harvest < 1.0 && WStem > 0.0)
	{
	  AM& am = AM::create (geometry, time, Stem, name, "stem");
	  am.add (WStem * C_Stem * m2_per_cm2,
		  NStem * m2_per_cm2);
	  organic_matter.add (am);
	}
      if (sorg_harvest < 1.0 && WSOrg > 0.0)
	{
	  AM& am = AM::create (geometry, time, SOrg, name, "sorg");
	  am.add (WSOrg * C_SOrg * (1.0 - sorg_harvest) * m2_per_cm2,
		  NSOrg * (1.0 - sorg_harvest) * m2_per_cm2);
	  organic_matter.add (am);
	}
      if (WRoot > 0.0)
	{
	  AM& am = AM::create (geometry, time, Root, name, "root");
	  if (geometry.total (density) > 0.0)
	    am.add (geometry,
		    WRoot * C_Root * m2_per_cm2,
		    NRoot * m2_per_cm2,
		    density);
	  else
	    am.add (WRoot * C_Root * m2_per_cm2,
		    NRoot * m2_per_cm2);
	  organic_matter.add (am);
	}
    }

  return *new Harvest (column_name, time, name, 
		       WStraw * stem_harvest, NStraw * stem_harvest,
		       0.0, 0.0,
		       WSOrg * sorg_harvest, NSOrg * sorg_harvest);
}

void
CropOld::output (Log& log, Filter& filter) const
{
  var.output (log, filter);
}

double
CropOld::DS () const
{ return var.Phenology.DS; }

CropOld::CropOld (const AttributeList& al, int layers)
  : Crop (al.name ("type")),
    par (*new Parameters (al)),
    var (*new Variables (par, al, layers))
{ }

CropOld::~CropOld ()
{ 
  delete &var;
}
