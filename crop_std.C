// crop_std.C

#include "crop.h"
#include "log.h"
#include "time.h"
#include "csmp.h"
#include "bioclimate.h"
#include "common.h"
#include "csmp.h"
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
#include "options.h"

// Dimensional conversion.
static const double m2_per_cm2 = 0.0001;

// Chemical constants affecting the crop.
const double molWeightCH2O = 30.0; // [gCH2O/mol]
const double molWeightCO2 = 44.0; // [gCO2/mol]

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
  double water_stress () const;	// [0-1] (1 = full production)
  double nitrogen_stress () const; // [0-1] (0 = no production)
  double rs_min () const;	// Minimum trasnpiration resistance.
  double rs_max () const;	// Maximum trasnpiration resistance.
  double height () const;	// Crop height [cm]
  double LAI () const;
  const CSMP& LAIvsH () const;
  double PARext () const;
  double PARref () const;
  double EPext () const;
  double IntcpCap () const;	// Interception Capacity.
  double EpFac () const;	// Convertion to potential evapotransp.
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
  void Emergence ();
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
  double RSR () const;

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
  double DM () const;

  // Create and Destroy.
public:
  void initialize (const Geometry& geometry);
  CropStandard (const AttributeList& vl);
  ~CropStandard ();
};

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
    const CSMP& TempEff1;	// Temperature effect, vegetative stage
    const CSMP& TempEff2;	// Temperature effect, reproductive stage
    const CSMP& PhotEff1;	// Ptotoperiode effect, vegetative stage
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
    double IntcpCap;
    double EpFac;
    double rs_max;		// max transpiration resistance
    double rs_min;		// min transpiration resistance
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
    const CSMP& Root;		// Partitioning functions for root
    const CSMP& Leaf;		//   leaf, and stem as function of DS
    const CSMP& Stem;
    const CSMP& RSR;		// Root/Shoot ratio.
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
    const CSMP& TLLeafEff;	// Translocation effiency, Leaf.
    const CSMP& TLRootEff;	// Translocation effiency, Root.
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
  bool enable_water_stress;
  bool enable_N_stress;
private:
  friend class CropStandard;
  Parameters (const AttributeList&);
public:
  ~Parameters ();
};

const AttributeList* CropStandard::Parameters::VernalPar::none;

struct CropStandard::Variables
{ 
  void output (Log&, Filter&) const;
  struct RecPhenology
  {
    void output (Log&, Filter&) const;
    double DS;		// Development Stage
    double Vern;		// Vernalization criterium [C d]
    double partial_day_length;	// Light hours this day until now [0-24 h]
    double day_length;		// Light hours previous day. [0-24 h]
    double partial_soil_temperature; // Accumaleted soil temperature. [°C]
    double soil_temperature;	// Soil temperature previous day. [°C]
  private:
    friend struct CropStandard::Variables;
    RecPhenology (const Parameters&, const AttributeList&);
  } Phenology;
  struct RecCanopy
  {
    void output (Log&, Filter&) const;
    double Height;		// Crop height [cm]
    double Offset;		// Extra height after harvest [cm]
    double LAI;	        	// Leaf Area Index
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
    double nitrogen_stress;	// Fraction of requested nitrogen we got.
    double Ept;			// Potential evapotranspiration.
  private:
    friend struct CropStandard::Variables;
    RecRootSys (const Parameters&, const AttributeList&);
  } RootSys;
  struct RecProd
  {
    void output (Log&, Filter&) const;
    double WLeaf;		// Leaf dry matter weight [g/m2]
    double WStem;		// Stem dry matter weight [g/m2]
    double WRoot;		// Root dry matter weight [g/m2]
    double WSOrg;		// Storage organ dry matter weight [g/m2]
    double NCrop;		// Nitrogen stored in dry matter [g/m2]
    double NLeaf;		// Leaf nitrogen [g/m2]
    double NStem;		// Stem nitrogen [g/m2]
    double NRoot;		// Root nitrogen [g/m2]
    double NSOrg;		// Storage organ nitrogen [g/m2]
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
    double PotRtDpt;	        // Potential Root Penetration Depth [cm]
    double PtNCnt;		// Potential Nitrogen Content in Crop [g/m2]
    double CrNCnt;		// Critical Nitrogen Content in Crop [g/m2]
    double NfNCnt;		// Non-func Nitrogen Content in Crop [g/m2]
    double PotTransp;	        // Potential Transpiration [mm/h]
    double PotCanopyAss;	// Potential Canopy Assimilation [g CH2O/m2/h]
    double CanopyAss;	        // Canopy Assimilation [g CH2O/m2/h]
    double LogPotCanopyAss;	// The above is hourly accumulated values 
    double LogCanopyAss;	// over the day.  This is last days total.
    double IncWLeaf;     	// Leaf growth [g DM/m2/d]
    double IncWStem;    	// Stem growth [g DM/m2/d]
    double IncWSOrg;    	// Storage organ growth [g DM/m2/d]
    double IncWRoot;    	// Root growth [g DM/m2/d]
    double LAImRat;		// (LAIm - LAI) / LAIm []
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
  Variables (const Parameters&, const AttributeList&);
public:
  ~Variables ();
};

CropStandard::Parameters::Parameters (const AttributeList& vl) 
  : Devel (vl.alist ("Devel")),
    Vernal (vl.check ("Vernal") ? vl.alist ("Vernal") : *VernalPar::none),
    LeafPhot (vl.alist ("LeafPhot")),
    Canopy (vl.alist ("Canopy")),
    Root (vl.alist ("Root")),
    Partit (vl.alist ("Partit")),
    Prod (vl.alist ("Prod")),
    CrpN (vl.alist ("CrpN")),
    Harvest (vl.alist ("Harvest")),
    enable_water_stress (vl.flag ("enable_water_stress")),
    enable_N_stress (vl.flag ("enable_N_stress"))
{ }

CropStandard::Parameters::DevelPar::DevelPar (const AttributeList& vl)
  : EmrTSum (vl.number ("EmrTSum")),
    DS_Emr (vl.number ("DS_Emr")),
    DSRate1 (vl.number ("DSRate1")),
    DSRate2 (vl.number ("DSRate2")),
    TempEff1 (vl.csmp ("TempEff1")),
    TempEff2 (vl.csmp ("TempEff2")),
    PhotEff1 (vl.csmp ("PhotEff1")),
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
    EPext (vl.number ("EPext")),
    IntcpCap (vl.number ("IntcpCap")),
    EpFac (vl.number ("EpFac")),
    rs_max (vl.number ("rs_max")),
    rs_min (vl.number ("rs_min"))
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
    Stem (vl.csmp ("Stem")),
    RSR (vl.csmp ("RSR"))
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
    RtDR (vl.csmp ("RtDR")),
    Large_RtDR (vl.number ("Large_RtDR"))
{ }

CropStandard::Parameters::CrpNPar::CrpNPar (const AttributeList& vl)
  : SeedN (vl.number ("SeedN")),
    DS_fixate (vl.number ("DS_fixate")),
    DS_cut_fixate (vl.number ("DS_cut_fixate")),
    fixate_factor (vl.number ("fixate_factor")),
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
    TLLeafEff (vl.csmp ("TLLeafEff")),
    TLRootEff (vl.csmp ("TLRootEff"))
{ }

CropStandard::Parameters::HarvestPar::HarvestPar (const AttributeList& vl)
  : Stem (vl.alist_sequence ("Stem")),
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
				    const AttributeList& vl)
  : Phenology (par, vl.alist ("Phenology")),
    Canopy (par, vl.alist ("Canopy")),
    RootSys (par, vl.alist ("RootSys")),
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
    Vern (vl.check ("Vern") ? vl.number ("Vern") : par.Vernal.TaSum),
    partial_day_length (vl.number ("partial_day_length")),
    day_length (vl.number ("day_length")),
    partial_soil_temperature (vl.number ("partial_soil_temperature")),
    soil_temperature (vl.number ("soil_temperature"))

{ }

void 
CropStandard::Variables::RecPhenology::output (Log& log, Filter& filter) const
{
  log.open ("Phenology");
  log.output ("DS", filter, DS);
  log.output ("Vern", filter, Vern);
  log.output ("partial_day_length", filter, partial_day_length);
  log.output ("day_length", filter, day_length);
  log.output ("partial_soil_temperature", filter, partial_soil_temperature);
  log.output ("soil_temperature", filter, soil_temperature);
  log.close();
}

CropStandard::Variables::RecCanopy::RecCanopy (const Parameters&,
					       const AttributeList& vl)
  : Height (vl.number ("Height")),
    Offset (vl.number ("Offset")),
    LAI (vl.number ("LAI")),
    LADm (vl.number ("LADm")),
    LAIvsH (vl.csmp ("LAIvsH"))
{ }

void 
CropStandard::Variables::RecCanopy::output (Log& log, Filter& filter) const
{
  log.open ("Canopy");
  log.output ("Height", filter, Height);
  log.output ("Offset", filter, Offset);
  log.output ("LAI", filter, LAI);
  log.output ("LADm", filter, LADm);
  log.output ("LAIvsH", filter, LAIvsH);
  log.close();
}

CropStandard::Variables::RecRootSys::RecRootSys (const Parameters& par,
						 const AttributeList& vl)
  : Depth (vl.check ("Depth") ? vl.number ("Depth") : par.Root.DptEmr),
    Density (vl.number_sequence ("Density")),
    H2OExtraction (vl.number_sequence ("H2OExtraction")),
    NH4Extraction (vl.number_sequence ("NH4Extraction")),
    NO3Extraction (vl.number_sequence ("NO3Extraction")),
    h_x (vl.number ("h_x")),
    water_stress (1.0),
    nitrogen_stress (1.0),
    Ept (0.0)
{ }

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
  log.output ("nitrogen_stress", filter, nitrogen_stress, true);
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
    NLeaf (vl.number ("NLeaf")),
    NStem (vl.number ("NStem")),
    NRoot (vl.number ("NRoot")),
    NSOrg (vl.number ("NSOrg")),
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
  log.output ("NLeaf", filter, NLeaf);
  log.output ("NStem", filter, NStem);
  log.output ("NRoot", filter, NRoot);
  log.output ("NSOrg", filter, NSOrg);
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
    LogPotCanopyAss (0.0),
    LogCanopyAss (0.0),
    IncWLeaf (0.0),
    IncWStem (0.0),
    IncWSOrg (0.0),
    IncWRoot (0.0),
    LAImRat (0.0),
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
  log.output ("LogPotCanopyAss", filter, LogPotCanopyAss, true);
  log.output ("LogCanopyAss", filter, LogCanopyAss, true);
  log.output ("IncWLeaf", filter, IncWLeaf, true);
  log.output ("IncWStem", filter, IncWStem, true);
  log.output ("IncWSOrg", filter, IncWSOrg, true);
  log.output ("IncWRoot", filter, IncWRoot, true);
  log.output ("LAImRat", filter, LAImRat, true);
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

void
CropStandard::initialize (const Geometry& geometry)
{
  unsigned int size = geometry.size ();

  // Fill rootsys arrays.
  var.RootSys.Density.insert (var.RootSys.Density.end (),
			      size - var.RootSys.Density.size (), 
			      0.0);
  var.RootSys.H2OExtraction.insert (var.RootSys.H2OExtraction.end (),
				    size - var.RootSys.H2OExtraction.size (),
				    0.0);
  var.RootSys.NH4Extraction.insert (var.RootSys.NH4Extraction.end (),
				    size - var.RootSys.NH4Extraction.size (),
				    0.0);
  var.RootSys.NO3Extraction.insert (var.RootSys.NO3Extraction.end (),
				    size - var.RootSys.NO3Extraction.size (),
				    0.0);
}

static struct CropStandardSyntax
{
  static Crop& make (const AttributeList& al)
    { return *new CropStandard (al); }
  CropStandardSyntax ();
} standard_crop_syntax;

#ifdef BORLAND_TEMPLATES
template class add_submodule_sequence<OM>;
#endif

CropStandardSyntax::CropStandardSyntax ()
{
  static const vector<double> empty_array;
  static const CSMP empty_csmp;

  Syntax& syntax = *new Syntax ();
  AttributeList& alist = *new AttributeList ();

  // DevelPar
  Syntax& Devel = *new Syntax ();
  AttributeList& vDevel = *new AttributeList ();

  Devel.add ("EmrTSum", Syntax::Number, Syntax::Const);
  Devel.add ("DS_Emr", Syntax::Number, Syntax::Const);
  Devel.add ("DSRate1", Syntax::Number, Syntax::Const);
  Devel.add ("DSRate2", Syntax::Number, Syntax::Const);
  Devel.add ("TempEff1", Syntax::CSMP, Syntax::Const);
  Devel.add ("TempEff2", Syntax::CSMP, Syntax::Const);
  Devel.add ("PhotEff1", Syntax::CSMP, Syntax::Const);
  Devel.add ("defined_until_ds", Syntax::Number, Syntax::Const);
  vDevel.add ("defined_until_ds", 2.0);
    
  syntax.add ("Devel", Devel, Syntax::Const);
  alist.add ("Devel", vDevel);

  // VernalPar
  Syntax& Vernal = *new Syntax ();
  // WARNING: Don't add an alist here, or the `Optional' idea is lost.

  Vernal.add ("required", Syntax::Boolean, Syntax::Optional);
  Vernal.add ("DSLim", Syntax::Number, Syntax::Const);
  Vernal.add ("TaLim", Syntax::Number, Syntax::Const);
  Vernal.add ("TaSum", Syntax::Number, Syntax::Const);

  syntax.add ("Vernal", Vernal, Syntax::Optional);

  // Initialize "no vernalization"
  AttributeList& noVernal = *new AttributeList ();
  noVernal.add ("required", false);
  noVernal.add ("DSLim", -42.42e42);
  noVernal.add ("TaLim", -42.42e42);
  noVernal.add ("TaSum", -42.42e42);
  CropStandard::Parameters::VernalPar::none = &noVernal;

  // LeafPhotPar
  Syntax& LeafPhot = *new Syntax ();

  LeafPhot.add ("Qeff", Syntax::Number, Syntax::Const);
  LeafPhot.add ("Fm", Syntax::Number, Syntax::Const);
  LeafPhot.add ("TempEff", Syntax::CSMP, Syntax::Const);

  syntax.add ("LeafPhot", LeafPhot, Syntax::Const);

  // Canopy
  Syntax& Canopy = *new Syntax ();
  AttributeList& vCanopy = *new AttributeList ();

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
  Canopy.add ("IntcpCap", Syntax::Number, Syntax::Const);
  vCanopy.add ("IntcpCap", 0.5);
  Canopy.add ("EpFac", Syntax::Number, Syntax::Const);
  vCanopy.add ("EpFac", 1.0);
  Canopy.add ("rs_max", Syntax::Number, Syntax::Const);
  vCanopy.add ("rs_max", 1.0e5);
  Canopy.add ("rs_min", Syntax::Number, Syntax::Const);
  vCanopy.add ("rs_min", 0.0);

  // RootPar
  Syntax& Root = *new Syntax ();
  AttributeList& vRoot = *new AttributeList ();

  Root.add ("DptEmr", Syntax::Number, Syntax::Const);
  Root.add ("PenPar1", Syntax::Number, Syntax::Const);
  Root.add ("PenPar2", Syntax::Number, Syntax::Const);
  Root.add ("MaxPen", Syntax::Number, Syntax::Const);
  Root.add ("SpRtLength", Syntax::Number, Syntax::Const);
  Root.add ("DensRtTip", Syntax::Number, Syntax::Const);
  vRoot.add ("DensRtTip", 0.1);
  Root.add ("Rad", Syntax::Number, Syntax::Const);
  Root.add ("h_wp", Syntax::Number, Syntax::Const);
  vRoot.add ("h_wp",-15000.0);
  Root.add ("MxNH4Up", Syntax::Number, Syntax::Const);
  vRoot.add ("MxNH4Up", 2.5e-7);
  Root.add ("MxNO3Up", Syntax::Number, Syntax::Const);
  vRoot.add ("MxNO3Up", 2.5e-8);
  Root.add ("Rxylem", Syntax::Number, Syntax::Const);
  vRoot.add ("Rxylem", 10.0);

  syntax.add ("Root", Root, Syntax::Const);
  alist.add ("Root", vRoot);

  // PartitPar
  Syntax& Partit = *new Syntax ();

  Partit.add ("Root", Syntax::CSMP, Syntax::Const);
  Partit.add ("Leaf", Syntax::CSMP, Syntax::Const);
  Partit.add ("Stem", Syntax::CSMP, Syntax::Const);
  Partit.add ("RSR", Syntax::CSMP, Syntax::Const);

  syntax.add ("Partit", Partit, Syntax::Const);

  // ProdPar
  Syntax& Prod = *new Syntax ();
  AttributeList& vProd = *new AttributeList ();

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
  Prod.add ("Large_RtDR", Syntax::Number, Syntax::Const);

  // CrpNPar
  Syntax& CrpN = *new Syntax ();
  AttributeList& CrpNList = *new AttributeList ();

  CrpN.add ("SeedN", Syntax::Number, Syntax::Const);
  CrpN.add ("DS_fixate", Syntax::Number, Syntax::Const);
  CrpNList.add ("DS_fixate", 42000.0);
  CrpN.add ("DS_cut_fixate", Syntax::Number, Syntax::Const);
  CrpNList.add ("DS_cut_fixate", 0.0);
  CrpN.add ("fixate_factor", Syntax::None (), Syntax::Const,
	    "Fraction of needed N fixated by day.");
  CrpNList.add ("fixate_factor", 0.8);
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
  CrpN.add ("TLLeafEff", Syntax::CSMP, Syntax::Const);
  CSMP TLLeafEff;
  TLLeafEff.add (0.00, 0.90);
  TLLeafEff.add (2.00, 0.90);
  CrpNList.add ("TLLeafEff", TLLeafEff);
  CrpN.add ("TLRootEff", Syntax::CSMP, Syntax::Const);
  CSMP TLRootEff;
  TLRootEff.add (0.00, 0.10);
  TLRootEff.add (2.00, 0.10);
  CrpNList.add ("TLRootEff", TLRootEff);

  syntax.add ("CrpN", CrpN, Syntax::Const);
  alist.add ("CrpN", CrpNList);

  // HarvestPar
  Syntax om_syntax;
  AttributeList om_alist;
  OM::load_syntax (om_syntax, om_alist);
  Syntax& Harvest = *new Syntax ();
  AttributeList& HarvestList = *new AttributeList ();
  AttributeList& AOM1 = *new AttributeList (om_alist);
  AttributeList& AOM2 = *new AttributeList (om_alist);
  AOM1.add ("initial_fraction", 0.80);
  vector<double> CN;
  CN.push_back (100.0);
  AOM1.add ("C_per_N", CN);
  vector<double> efficiency1;
  efficiency1.push_back (0.50);
  efficiency1.push_back (0.50);
  AOM1.add ("efficiency", efficiency1);
  AOM1.add ("turnover_rate", 2.917e-4);
  vector<double> fractions1;
  fractions1.push_back (0.50);
  fractions1.push_back (0.50);
  fractions1.push_back (0.00);
  AOM1.add ("fractions", fractions1);
  vector<double> efficiency2;
  efficiency2.push_back (0.50);
  efficiency2.push_back (0.50);
  AOM2.add ("efficiency", efficiency2);
  AOM2.add ("turnover_rate", 2.917e-3);
  vector<double> fractions2;
  fractions2.push_back (0.00);
  fractions2.push_back (1.00);
  fractions2.push_back (0.00);
  AOM2.add ("fractions", fractions2);
  vector<AttributeList*> AOM;
  AOM.push_back (&AOM1);
  AOM.push_back (&AOM2);
  add_submodule_sequence<OM> ("Stem", Harvest, Syntax::Const);
  add_submodule_sequence<OM> ("Leaf", Harvest, Syntax::Const);
  add_submodule_sequence<OM> ("Dead", Harvest, Syntax::Const);
  HarvestList.add ("Dead", AOM);
  add_submodule_sequence<OM> ("SOrg", Harvest, Syntax::Const);
  add_submodule_sequence<OM> ("Root", Harvest, Syntax::Const);
  Harvest.add ("C_Stem", Syntax::Number, Syntax::Const);
  HarvestList.add ("C_Stem", 0.420);
  Harvest.add ("C_Leaf", Syntax::Number, Syntax::Const);
  HarvestList.add ("C_Leaf", 0.420);
  Harvest.add ("C_Dead", Syntax::Number, Syntax::Const);
  HarvestList.add ("C_Dead", 0.420);
  Harvest.add ("C_SOrg", Syntax::Number, Syntax::Const);
  HarvestList.add ("C_SOrg", 0.420);
  Harvest.add ("C_Root", Syntax::Number, Syntax::Const);
  HarvestList.add ("C_Root", 0.420);
  Harvest.add ("DSmax", Syntax::Number, Syntax::Const);
  HarvestList.add ("DSmax", 0.80);
  Harvest.add ("DSnew", Syntax::Number, Syntax::Const);
  HarvestList.add ("DSnew", 0.20);

  syntax.add ("Harvest", Harvest, Syntax::Const);
  alist.add ("Harvest", HarvestList);

  syntax.add ("enable_water_stress", Syntax::Boolean, Syntax::Const);
  alist.add ("enable_water_stress", true);
  syntax.add ("enable_N_stress", Syntax::Boolean, Syntax::Const);
  alist.add ("enable_N_stress", true);

  // Variables.

  // Phenology
  Syntax& Phenology = *new Syntax ();
  AttributeList& vPhenology = *new AttributeList ();

  Phenology.add ("DS", Syntax::Number, Syntax::State);
  vPhenology.add ("DS", -1.0);
  Phenology.add ("Vern", Syntax::Number, Syntax::Optional);
  Phenology.add ("partial_day_length", Syntax::Number, Syntax::State);
  vPhenology.add ("partial_day_length", 0.0);
  Phenology.add ("day_length", Syntax::Number, Syntax::State);
  vPhenology.add ("day_length", 0.0);
  Phenology.add ("partial_soil_temperature", Syntax::Number, Syntax::State);
  vPhenology.add ("partial_soil_temperature", 0.0);
  Phenology.add ("soil_temperature", Syntax::Number, Syntax::State);
  vPhenology.add ("soil_temperature", 0.0);

  syntax.add ("Phenology", Phenology, Syntax::State);
  alist.add ("Phenology", vPhenology);

  // Canopy
  Canopy.add ("Height", Syntax::Number, Syntax::State);
  vCanopy.add ("Height", 0.0);
  Canopy.add ("Offset", Syntax::Number, Syntax::State);
  vCanopy.add ("Offset", 0.0);
  Canopy.add ("LAI", Syntax::Number, Syntax::State);
  vCanopy.add ("LAI", 0.0);
  Canopy.add ("LADm", Syntax::Number, Syntax::State);
  vCanopy.add ("LADm", -9999.99);
  Canopy.add ("LAIvsH", Syntax::CSMP, Syntax::State);
  vCanopy.add ("LAIvsH", empty_csmp);

  syntax.add ("Canopy", Canopy, Syntax::State);
  alist.add ("Canopy", vCanopy);

  // RootSys
  Syntax& RootSys = *new Syntax ();
  AttributeList& vRootSys = *new AttributeList ();

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
  RootSys.add ("nitrogen_stress", Syntax::Number, Syntax::LogOnly);
  RootSys.add ("Ept", Syntax::Number, Syntax::LogOnly);

  syntax.add ("RootSys", RootSys, Syntax::State);
  alist.add ("RootSys", vRootSys);

  // Prod
  // Warning: Uses same syntax as `ProdPar'.
  Prod.add ("WLeaf", Syntax::Number, Syntax::State);
  vProd.add ("WLeaf", 0.001);
  Prod.add ("WStem", Syntax::Number, Syntax::State);
  vProd.add ("WStem", 0.000);
  Prod.add ("WRoot", Syntax::Number, Syntax::State);
  vProd.add ("WRoot", 0.001);
  Prod.add ("WSOrg", Syntax::Number, Syntax::State);
  vProd.add ("WSOrg", 0.000);
  Prod.add ("NLeaf", Syntax::Number, Syntax::State);
  vProd.add ("NLeaf", 0.000);
  Prod.add ("NStem", Syntax::Number, Syntax::State);
  vProd.add ("NStem", 0.000);
  Prod.add ("NRoot", Syntax::Number, Syntax::State);
  vProd.add ("NRoot", 0.000);
  Prod.add ("NSOrg", Syntax::Number, Syntax::State);
  vProd.add ("NSOrg", 0.000);
  Prod.add ("NCrop", Syntax::Number, Syntax::Optional);

  alist.add ("Prod", vProd);
  syntax.add ("Prod", Prod, Syntax::State);

  // CrpAux
  Syntax& CrpAux = *new Syntax ();
  AttributeList& vCrpAux = *new AttributeList ();

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
  CrpAux.add ("LogPotCanopyAss", Syntax::Number, Syntax::LogOnly);
  CrpAux.add ("LogCanopyAss", Syntax::Number, Syntax::LogOnly);
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

  syntax.add ("CrpAux", CrpAux, Syntax::State);
  alist.add ("CrpAux", vCrpAux);

  syntax.add ("description", Syntax::String, Syntax::Optional); 

  Librarian<Crop>::add_type ("default", alist, syntax, &make);
}

double CropStandard::water_stress () const // [0-1] (1 = full production)
{ return var.RootSys.water_stress; }

double CropStandard::nitrogen_stress () const // [0-1] (0 = no production)
{ return var.RootSys.nitrogen_stress; }

double CropStandard::rs_min () const // Minimum trasnpiration resistance.
{ return par.Canopy.rs_min; }

double CropStandard::rs_max () const // Maximum trasnpiration resistance.
{ return par.Canopy.rs_max; }

double CropStandard::height () const // Crop height [cm]
{ return var.Canopy.Height; }

double CropStandard::LAI () const
{ return var.Canopy.LAI; }

const CSMP& CropStandard::LAIvsH () const
{ return var.Canopy.LAIvsH; }

double CropStandard::PARext () const
{ return par.Canopy.PARext; }

double CropStandard::PARref () const
{ return par.Canopy.PARref; }

double CropStandard::EPext () const
{ return par.Canopy.EPext; }

double CropStandard::IntcpCap () const // Interception Capacity.
{ return par.Canopy.IntcpCap; }

double CropStandard::EpFac () const // Convertion to potential evapotransp.
{ return par.Canopy.EpFac; }

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
			      solute.M_left (i) - 1e-8));
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

  Vern -= min (Ta - Vernal.TaLim, 0.0);
  if (DS > Vernal.DSLim)
    DS = Vernal.DSLim;
}

void 
CropStandard::Emergence ()
{
  const Parameters::DevelPar& Devel = par.Devel;
  double& DS = var.Phenology.DS;

  DS += var.Phenology.soil_temperature / Devel.EmrTSum;
  if (DS > 0)
    DS = Devel.DS_Emr;
}

void
CropStandard::DevelopmentStage (const Bioclimate& bioclimate)
{
  const Parameters::DevelPar& Devel = par.Devel;
  Variables::RecPhenology& Phenology = var.Phenology;

  const double Ta = bioclimate.daily_air_temperature ();

  if (Phenology.DS < 1)
    {
      // Only increase DS if assimilate production covers leaf respiration.
      if (var.CrpAux.IncWLeaf +  var.CrpAux.DeadWLeaf 
	  >  -var.Prod.WLeaf /1000.0) // It lost 0.1% of its leafs to resp.
	Phenology.DS += (Devel.DSRate1
			 * Devel.TempEff1 (Ta)
			 * Devel.PhotEff1 (var.Phenology.day_length + 1.0));
      if (par.Vernal.required && Phenology.Vern < 0)
	Vernalization (Ta);
    }
  else
    {
      Phenology.DS += Devel.DSRate2 * Devel.TempEff2 (Ta);
      if (Phenology.DS > 2)
	Phenology.DS = 2.0;
    }

  assert (Phenology.DS <= Devel.defined_until_ds);
}
double 
CropStandard::CropHeight ()
{
  const Parameters::CanopyPar& Canopy = par.Canopy;
  double& DS = var.Phenology.DS;

  return Canopy.HvsDS (DS) + var.Canopy.Offset;
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
#if 0
      if (DS > Canopy.DSinit)
	DS = Canopy.DSinit;
      LAI = 0.5 * (exp (Canopy.InitGrowth * DS) - 1);
#else
      LAI = 0.5 * (exp (Canopy.InitGrowth * min (DS, Canopy.DSinit)) - 1);
#endif
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
  double Area;		        // Area spanned by z0, z1, and z2.

  assert (DS > 0.0);
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
      Area = (1.0 + z2 - z1 - z0) / 2.0;
      assert (Area > 0.0);
      assert (Canopy.Height > 0.0);
      Canopy.LADm = Canopy.LAI / (Area * Canopy.Height);
    }
  else
    {
      assert (DS <= 2);

      z0 = CanopyPar.LAIDist1[0];
      z1 = CanopyPar.LAIDist1[1];
      z2 = CanopyPar.LAIDist1[2];
      double Area = (1.0 + z2 - z1 - z0) / 2.0;
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

	      double x0 = 1.0 - sqrt (2.0 * Need * (z1 - z2 - z0 + 1.0));
	      double x1 
		= 1.0 + (z2 - 1) * sqrt (2.0 * Need / (z1 - z2 - z0 + 1.0));
	      double y1 = sqrt (2.0 * Need / (z1 - z2 - z0 + 1.0));
			    
	      // Check the results.
	      assert (approximate (Need, (1.0 - x0) * y1 / 2.0));
	      assert (approximate ((1.0 - x1) / y1, (1.0 - z2)));
	      assert (approximate ((x1 - x0) / y1, (z1 - z0)));

	      // Insert this special distribution, and return.
	      CSMP LADvsH;
	      LADvsH.add (x0 * Canopy.Height, 0.0);
	      LADvsH.add (x1 * Canopy.Height, y1 * Canopy.LADm);
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
  const double LAIm = - log (0.05) / par.Canopy.PARext;
  var.CrpAux.LAImRat = max (0.0, (Canopy.LAI - LAIm) / LAIm);
}

double
CropStandard::ActualWaterUptake (double Ept,
				 const Soil& soil, SoilWater& soil_water,
				 const double EvapInterception)
{
  if (Ept < 0)
    {
      CERR << "\nBUG: Negative EPT (" << Ept << ")\n";
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
  const double h_wp = par.Root.h_wp;
  const double Rxylem = par.Root.Rxylem;
  const double area = M_PI * par.Root.Rad * par.Root.Rad;
  const vector<double>& L = var.RootSys.Density;
  vector<double>& S = var.RootSys.H2OExtraction;
  double total = 0.0;
  for (unsigned int i = 0; i < soil.size () && L[i] > 0.0; i++)
    {
      if (soil_water.h (i) >= 0.0)
	{
	  S[i] = 0.0;
	  continue;
	}
      const double h = h_x - (1 + Rxylem) * soil.z (i);
      assert (soil_water.Theta_left (i) >= 0.0);
      assert (soil.Theta (i, h_wp) >= soil.Theta_res (i));
      const double max_uptake 
	= (soil_water.Theta_left (i) - soil.Theta (i, h_wp)) / dt;
      const double uptake
	= max (min (2 * M_PI * L[i]
		    * (soil.Theta (i, h) / soil.Theta (i, 0.0))
		    * (soil.M (i, soil_water.h (i)) - soil.M (i, h))
		    / (- 0.5 * log (area * L[i])),
		    max_uptake),
	       0.0);
      assert (soil_water.Theta_left (i) - uptake > soil.Theta_res (i));
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
 //     z1 = 1 + a * x1;
      x2 = 1.0;
      y2 = exp (x2);
 //     z2 = 1 + a * x2;
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
  Variables::RecProd& Prod = var.Prod;
  double x;

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
      x = (Prod.NCrop - var.CrpAux.CrNCnt)
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
      x = (Prod.NCrop - var.CrpAux.CrNCnt)
        / (var.CrpAux.NfNCnt - var.CrpAux.CrNCnt);
      Prod.NLeaf = ((CrpN.NfLeafCnc (DS) - CrpN.CrLeafCnc (DS)) * x
        + CrpN.CrLeafCnc (DS)) * Prod.WLeaf;
      Prod.NStem = ((CrpN.NfStemCnc (DS) - CrpN.CrStemCnc (DS)) * x
        + CrpN.CrStemCnc (DS)) * Prod.WStem;
      Prod.NSOrg = ((CrpN.NfSOrgCnc (DS) - CrpN.CrSOrgCnc (DS)) * x
        + CrpN.CrSOrgCnc (DS)) * Prod.WSOrg;
      Prod.NRoot = Prod.NCrop - Prod.NLeaf - Prod.NStem - Prod.NSOrg;
    }
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
      CrpAux.Fixated = par.CrpN.fixate_factor * PotNUpt;
      NCrop += CrpAux.Fixated;
      // PotNUpt -= CrpAux.Fixated;
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
  const double Ta = bioclimate.daily_air_temperature ();
  const double Teff = LeafPhot.TempEff (Ta); // Temperature effect

  // One crop: assert (approximate (var.Canopy.LAI, bioclimate.LAI ()));
  if (!approximate (LAIvsH (var.Canopy.Height), var.Canopy.LAI))
    {
      CERR << "Bug: LAI below top: " << LAIvsH (var.Canopy.Height)
	   << " Total LAI: " << var.Canopy.LAI << "\n";
      CanopyStructure ();
      CERR << "Adjusted: LAI below top: " << LAIvsH (var.Canopy.Height)
	   << " Total LAI: " << var.Canopy.LAI << "\n";
    }

 // LAI below the current leaf layer.
  double prevLA = LAIvsH (bioclimate.height (0));
  // Assimilate produced by canopy photosynthesis
  double Ass = 0.0;
  // Accumulated LAI, for testing purposes.
  double accLAI =0.0;
  // Number of computational intervals in the canopy.
  const int No = bioclimate.NumberOfIntervals ();
  // LAI in each interval.
  const double dLAI = bioclimate.LAI () / No;

  // True, if we haven't reached the top of the crop yet.
  bool top_crop = true;

  for (int i = 0; i < No; i++)
    {
      const double height = bioclimate.height (i+1);
      assert (height < bioclimate.height (i));

      if (top_crop && height <= var.Canopy.Height)
	{
	  // We count day hours at the top of the crop.
	  top_crop = false;
	  if (bioclimate.PAR (i) > 0.5 * 70.0)
	    var.Phenology.partial_day_length += 1.0;
	}
      // Leaf Area index for a given leaf layer
      const double LA = prevLA - LAIvsH (height);
      assert (LA >= 0.0);
      if (LA > 0)
	{
	  prevLA = LAIvsH (height);
	  accLAI += LA;

	  const double dPAR
	    = (bioclimate.PAR (i) - bioclimate.PAR (i + 1)) / dLAI;

	  // Leaf Photosynthesis [gCO2/m2/h]
	  const double F = LeafPhot.Fm * 
	    (1.0 - exp (- (LeafPhot.Qeff * dPAR / LeafPhot.Fm)));

	  Ass += LA * F;
	}
    }
  assert (approximate (accLAI, var.Canopy.LAI));

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

  const double ReMobil = ReMobilization ();
  CrpAux.CanopyAss += ReMobil;

  RMLeaf = max (0.0, RMLeaf - CrpAux.PotCanopyAss + CrpAux.CanopyAss);
  const double RM = RMLeaf + RMStem + RMSOrg + RMRoot;

  double& Stress = var.RootSys.nitrogen_stress;
    
  if (CrpAux.CanopyAss >= RM)
    {
      const double AssG = CrpAux.CanopyAss - RM;
      if (par.enable_N_stress)
	Stress
	  = max (0.0, min (1.0, ((vProd.NCrop - CrpAux.NfNCnt)
				 / (CrpAux.CrNCnt - CrpAux.NfNCnt))));
      else
	Stress = 1.0;

      double f_Leaf, f_Stem, f_SOrg, f_Root;
      AssimilatePartitioning (DS, f_Leaf, f_Stem, f_Root, f_SOrg);
      CrpAux.IncWLeaf = Stress * pProd.E_Leaf * f_Leaf * AssG;
      CrpAux.IncWStem = Stress * pProd.E_Stem * f_Stem * AssG - ReMobil;
      CrpAux.IncWSOrg = Stress * pProd.E_SOrg * f_SOrg * AssG;
      CrpAux.IncWRoot = Stress * pProd.E_Root * f_Root * AssG;
    }
  else
    {
      Stress = 1.0;
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
	CERR << "BUG: Extra AssG: " << AssG << "\n";
    }

  // Update dead leafs
  CrpAux.DeadWLeaf = pProd.LfDR (DS) * vProd.WLeaf;
  CrpAux.DeadWLeaf += vProd.WLeaf * (1.0 / 3.0) * CrpAux.LAImRat;
  const double DdLeafCnc = (vProd.NLeaf/vProd.WLeaf - par.CrpN.NfLeafCnc (DS))
    * ( 1.0 - par.CrpN.TLLeafEff (DS)) +  par.CrpN.NfLeafCnc (DS);
  CrpAux.DeadNLeaf = DdLeafCnc * CrpAux.DeadWLeaf;
  CrpAux.IncWLeaf -= CrpAux.DeadWLeaf;
  assert (CrpAux.DeadWLeaf >= 0.0);
  vProd.AM_leaf->add (par.Harvest.C_Dead * CrpAux.DeadWLeaf * m2_per_cm2,
		      CrpAux.DeadNLeaf * m2_per_cm2);

  // Update dead roots.
  double RtDR = pProd.RtDR (DS);
  if (RSR () > par.Partit.RSR (DS))
    RtDR += pProd.Large_RtDR;

  CrpAux.DeadWRoot = RtDR * vProd.WRoot;
  const double DdRootCnc = (vProd.NRoot/vProd.WRoot - par.CrpN.NfRootCnc (DS))
    * ( 1.0 - par.CrpN.TLRootEff (DS)) +  par.CrpN.NfRootCnc (DS);
  CrpAux.DeadNRoot = DdRootCnc * CrpAux.DeadWRoot;
  CrpAux.IncWRoot -= CrpAux.DeadWRoot;
  vProd.AM_root->add (geometry,
		      par.Harvest.C_Root * CrpAux.DeadWRoot * m2_per_cm2,
		      CrpAux.DeadNRoot * m2_per_cm2,
		      var.RootSys.Density);

  // Update production.
  vProd.NCrop -= (CrpAux.DeadNLeaf + CrpAux.DeadNRoot);
  assert (vProd.NCrop > 0.0);
  vProd.WLeaf += CrpAux.IncWLeaf;
  vProd.WStem += CrpAux.IncWStem;
  vProd.WSOrg += CrpAux.IncWSOrg;
  vProd.WRoot += CrpAux.IncWRoot;
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

  // Update partial_soil_temperature.
  var.Phenology.partial_soil_temperature += 
    soil_heat.T (soil.interval_plus (-par.Root.DptEmr));

  if (time.hour () == 0 && var.Phenology.DS <= 0)
    {
      // Calculate average soil temperature.
      var.Phenology.soil_temperature =
	var.Phenology.partial_soil_temperature / 24.0;
      var.Phenology.partial_soil_temperature = 0.0;

      Emergence ();
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
      if (par.enable_water_stress)
	var.CrpAux.CanopyAss += water_stress * Ass;
      else
	var.CrpAux.CanopyAss += Ass;
    }
  if (time.hour () != 0)
    return;

  // Update final daylength.
  var.Phenology.day_length = var.Phenology.partial_day_length;
  var.Phenology.partial_day_length = 0.0;

  var.Canopy.Height = CropHeight ();
  if (var.CrpAux.InitLAI)
    InitialLAI ();
  else
    var.Canopy.LAI = CropLAI ();
  NetProduction (bioclimate, soil, soil_heat);
  DevelopmentStage (bioclimate);
  RootPenetration (soil, soil_heat);
  RootDensity (soil);
  NitContent ();

  var.CrpAux.LogPotCanopyAss = var.CrpAux.PotCanopyAss;
  var.CrpAux.LogCanopyAss = var.CrpAux.CanopyAss;
  var.CrpAux.PotCanopyAss = 0.0;
  var.CrpAux.CanopyAss = 0.0;
}

const Harvest&
CropStandard::harvest (const string& column_name,
		       const Time& time,
		       const Geometry& geometry,
		       OrganicMatter& organic_matter,
		       double stub_length,
		       double stem_harvest,
		       double leaf_harvest,
		       double sorg_harvest,
		       bool kill_off)
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
  const double NStem = Prod.NStem;
  const double NLeaf = Prod.NLeaf;
  const double NSOrg = Prod.NSOrg;
  const double NRoot = Prod.NRoot;
  const double C_Stem = Hp.C_Stem;
  const double C_Leaf = Hp.C_Leaf;
  const double C_SOrg = Hp.C_SOrg;
  const double C_Root = Hp.C_Root;

  const vector<AttributeList*>& Stem = Hp.Stem;
  const vector<AttributeList*>& Leaf = Hp.Leaf;
  const vector<AttributeList*>& SOrg = Hp.SOrg;

  const vector<double>& density = var.RootSys.Density;
  const double length = height ();

  // Leave stem and leaf below stub alone.
  if (stub_length < length)
    {
      stem_harvest *= (1.0 - stub_length / length);

      const double total_LAI = LAI ();
      if (total_LAI > 0.0)
	{
	  const double stub_LAI = LAIvsH ()(stub_length);
	  leaf_harvest *= (1.0 - stub_LAI / total_LAI);
	}
    }

  CERR << "Got (stem_harvest " << stem_harvest << ")\n";
  CERR << "Got (leaf_harvest " << leaf_harvest << ")\n";
  CERR << "Got (total_LAI " << LAI () << ")\n";
  CERR << "Got (stub_LAI " << LAIvsH ()(stub_length) << ")\n";

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

      // Adjust canopy for the sake of bioclimate.
      var.Canopy.Height = min (stub_length, var.Canopy.Height);
      var.Canopy.Offset
	= var.Canopy.Height
	- par.Canopy.HvsDS (var.Phenology.DS) ;
      assert (approximate (CropHeight (), var.Canopy.Height));
      var.Canopy.LAI = CropLAI ();
      CanopyStructure ();
    }
  else
    {
      var.Phenology.DS = DSremove;

      // Add crop remains to the soil.
      if (stem_harvest < 1.0 && WStem > 0.0)
	{
	  AM& am = AM::create (geometry, time, Stem, name, "stem");
	  am.add (WStem * C_Stem * (1.0 - stem_harvest) * m2_per_cm2,
		  NStem * (1.0 - stem_harvest) * m2_per_cm2);
	  organic_matter.add (am);
	}
      if (leaf_harvest < 1.0 && WLeaf > 0.0)
	{
	  AM& am = AM::create (geometry, time, Leaf, name, "leaf");
	  am.add (WLeaf * C_Leaf * (1.0 - leaf_harvest) * m2_per_cm2,
		  NLeaf * (1.0 - leaf_harvest) * m2_per_cm2);
	  organic_matter.add (am);
	}
      if (sorg_harvest < 1.0 && WSOrg > 0.0)
	{
	  AM& am = AM::create (geometry, time, SOrg, name, "sorg");
	  am.add (WSOrg * C_SOrg * (1.0 - sorg_harvest) * m2_per_cm2,
		  NSOrg * (1.0 - sorg_harvest) * m2_per_cm2);
	  organic_matter.add (am);
	}

      // Update and unlock locked AMs.
      if (var.Prod.AM_root)
	{
	  if (geometry.total (density) > 0.0)
	    var.Prod.AM_root->add (geometry,
				   WRoot * C_Root * m2_per_cm2,
				   NRoot * m2_per_cm2,
				   density);
	  else
	    var.Prod.AM_root->add (WRoot * C_Root * m2_per_cm2,
				   NRoot * m2_per_cm2);
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

double
CropStandard::DM () const	// [g/m² -> kg/ha]
{ return (var.Prod.WSOrg + var.Prod.WStem + var.Prod.WLeaf) * 10; }

CropStandard::CropStandard (const AttributeList& al)
  : Crop (al.name ("type")),
    par (*new Parameters (al)),
    var (*new Variables (par, al))
{ }

CropStandard::~CropStandard ()
{
  delete &var;
  delete &par;
}

