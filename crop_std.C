// crop_std.C

#include "crop.h"
#include "log.h"
#include "time.h"
#include "column.h"
#include "csmp.h"
#include "bioclimate.h"
#include "common.h"
#include "ftable.h"
#include "csmp.h"
#include "syntax.h"
#include "alist.h"
#include "filter.h"
#include "bioclimate.h"

#include <list.h>

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
  
  // Internal functions.
protected:
 virtual double SoluteUptake (string /* SoluteID */, double /* PotNUpt */,
			       double /* I_Mx */, double /* Rad */);
  virtual double H2OUptake (double PotTransp,
			    double /* RootRad */, double /* h_wp */);
public:				// Used by external development models.
  virtual void Vernalization (double Ta);
protected:
  virtual void Emergence (const Column& column);
  virtual void DevelopmentStage (const Bioclimate&);
  virtual double CropHeight ();
  virtual void InitialLAI ();
  virtual double CropLAI ();
  virtual void RootPenetration (const Column& column);
  virtual double RootDensDistPar (double a);
  virtual void RootDensity ();
  virtual void NitContent ();
  virtual void NitrogenUptake (int Hour);
  // Sugar production [gCH2O/m2/h] by canopy photosynthesis.
  virtual double CanopyPhotosynthesis (const Bioclimate&);
  virtual void AssimilatePartitioning (double DS, 
				       double& f_Leaf, double& f_Stem,
				       double& f_Root, double& f_SOrg);
  virtual double MaintenanceRespiration (double r, double Q10,
					 double w, double T);
  virtual void NetProduction (const Column&, const Bioclimate&);

  // Simulation.
public:
  void tick (const Time& time, const Column&, const Bioclimate&);
  void output (Log&, const Filter*) const;

  // Create and Destroy.
private:
  friend class CropStandardSyntax;
  static Crop* make (const AttributeList&);
  CropStandard (const AttributeList& vl);
public:
  virtual ~CropStandard ();
};

// Chemical constants affecting the crop.

const double molWeightCH2O = 30.0; // [gCH2O/mol]
const double molWeightCO2 = 44.0; // [gCO2/mol]

typedef void (*CropFun)(const Bioclimate&, CropStandard&);

struct CropStandard::Parameters
{ 
  const struct DevelPar
  {
    static dFTable<CropFun> models;
    CropFun Model;		// Phenological development model ID
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
    static dFTable<CropFun> models;
    CropFun Model;		// exponential or parabolic
    double Qeff;		// Quantum efficiency at low light
    double Fm;		// Max assimilation rate
    double TLim1;		// Lowest temp for photosynthesis
    double TLim2;		// Lowest temp for unrestricted phot.
  private:
    friend struct CropStandard::Parameters;
    LeafPhotPar (const AttributeList&);
  } LeafPhot;
  const struct CanopyPar {
    double DSinit;		// DS at end of initial LAI-Development
    double WLfInit;		// WLeaf at end of initial LAI-Development
    double SpLAI;		// Specific leaf weight
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
    double SpRtLength;	// Specific root length
    double DensRtTip;	// Root density at (pot) penetration depth
    double Rad;		// Root radius
    double h_wp;		// Matrix potential at wilting
    double MxNH4Up;		// Max NH4 uptake per unit root length
    double MxNO3Up;		// Max NO3 uptake per unit root length
  private:
    friend struct CropStandard::Parameters;
    RootPar (const AttributeList&);
  } Root;
  const struct PartitPar {
    const CSMP& Root;	// Partitioning functions for root
    const CSMP& Leaf;	//   leaf, and stem as function of DS
    const CSMP& Stem;
    const CSMP& LfDR;	// Death rate of Leafs
    const CSMP& RtDR;	// Death rate of Roots
  private:
    friend struct CropStandard::Parameters;
    PartitPar (const AttributeList&);
  } Partit;
  struct RespPar {
    double E_Root;		// Conversion efficiency, root
    double E_Leaf;		// Conversion efficiency, leaf
    double E_Stem;		// Conversion efficiency, stem
    double E_SOrg;		// Conversion efficiency, stor. org.
    double r_Root;		// Maint. resp. coeff., root
    double r_Leaf;		// Maint. resp. coeff., leaf
    double r_Stem;		// Maint. resp. coeff., stem
    double r_SOrg;		// Maint. resp. coeff., stor. org.
    double Q10;		// Maint. resp. Q10-value
  private:
    friend struct CropStandard::Parameters;
    RespPar (const AttributeList&);
  } Resp;
  struct CrpNPar {
    double SeedN;		// N-content in seed
    const CSMP& PtLeafCnc;	// Upper limit for N-conc in leaves
    const CSMP& CrLeafCnc;	// Critical lim f. N-conc in leaves
    const CSMP& PtStemCnc;	// Upper limit for N-conc in stems
    const CSMP& CrStemCnc;	// Critical lim f. N-conc in stems
    const CSMP& PtRootCnc;	// Upper limit for N-conc in roots
    const CSMP& CrRootCnc;	// Critical lim f. N-conc in roots
    const CSMP& PtSOrgCnc;	// Upper limit for N-conc in stor org
    const CSMP& CrSOrgCnc;	// Critical lim f. N-conc in stor org
  private:
    friend struct CropStandard::Parameters;
    CrpNPar (const AttributeList&);
  } CrpN;
  // Dunno where these belongs...
  double IntcpCap;
  double EpFac;
  static const Parameters& get (const string, const AttributeList&);
private:
  // BUG: We should have a STL map<string, Parameters> instead of
  // having each Parameters object know its own name.
  typedef list<const Parameters*> pList;
  const string name;
  static pList all;
  Parameters (const string, const AttributeList&);
public:
  ~Parameters ();
};

struct CropStandard::Variables
{ 
  void output (Log&, const Filter*) const;
  struct RecPhenology
  {
    void output (Log&, const Filter*) const;
    double DS;		// Development Stage
    double Vern;		// Vernalization criterium [C d]
  private:
    friend struct CropStandard::Variables;
    RecPhenology (const Parameters&, const AttributeList&);
  } Phenology;
  struct RecCanopy
  {
    void output (Log&, const Filter*) const;
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
    void output (Log&, const Filter*) const;
    double Depth;		// Rooting Depth [cm]
    vector<double> Density;	// Root density [cm/cm3] in soil layers
    vector<double> H2OExtraction; // Extraction of H2O in soil layers
    // [cm/h]
    vector<double> NH4Extraction; // Extraction of NH4-N in soil layers
    // [mg/m2/h]
    vector<double> NO3Extraction; // Extraction of NH4-N in soil layers
    // [mg/m2/h]
  private:
    friend struct CropStandard::Variables;
    RecRootSys (const Parameters&, const AttributeList&);
  } RootSys;
  struct RecProd
  {
    void output (Log&, const Filter*) const;
    double WLeaf;		// Leaf dry matter weight [g/m2]
    double WStem;		// Stem dry matter weight [g/m2]
    double WRoot;		// Root dry matter weight [g/m2]
    double WSOrg;		// Storage organ dry matter weight [g/m2]
    double WLDrd;		// Inactive canopy dry matter weight [g/m2]
    double NCrop;		// Nitrogen stored in dry matter [g/m2]
  private:
    friend struct CropStandard::Variables;
    RecProd (const Parameters&, const AttributeList&);
  } Prod;
  struct RecCrpAux
  {
    void output (Log&, const Filter*) const;
    bool InitLAI;		// Initial LAI development ?
    double PotRtDpt;	// Potential Root Penetration Depth [cm]
    double PtNCnt;		// Potential Nitrogen Content in Crop [g/m2]
    double CrNCnt;		// Critical Nitrogen Content in Crop [g/m2]
    double PotTransp;	// Potential Transpiration [mm/h]
    double PotCanopyAss;	// Potential Canopy Assimilation [g CH2O/m2/h]
    double CanopyAss;	// Canopy Assimilation [g CH2O/m2/h]
    double IncWLeaf;	// Leaf growth [g DM/m2/d]
    double IncWStem;	// Stem growth [g DM/m2/d]
    double IncWSOrg;	// Storage organ growth [g DM/m2/d]
    double IncWRoot;	// Root growth [g DM/m2/d]
    double H2OUpt;		// H2O uptake [mm/h]
    double NH4Upt;		// NH4-N uptake [g/m2/h]
    double NO3Upt;		// NO3-N uptake [g/m2/h]
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

static void 
devel_m1 (const Bioclimate& bioclimate, CropStandard& crop)
{
  const CropStandard::Parameters::DevelPar& Devel = crop.par.Devel;
  CropStandard::Variables::RecPhenology& Phenology = crop.var.Phenology;
  double Ta = bioclimate.AirTemperature ();

  if (Phenology.DS < 1)
    {
      Phenology.DS += Devel.DSRate1 * Ta;
      if (Phenology.Vern < 0)
	crop.Vernalization (Ta);
    }
  else
    {
      Phenology.DS += Devel.DSRate2 * Ta;
      if (Phenology.DS > 2)
	Phenology.DS = 2.0;
    }
}

static void 
devel_m2 (const Bioclimate& bioclimate, CropStandard& crop)
{
  const CropStandard::Parameters::DevelPar& Devel = crop.par.Devel;
  CropStandard::Variables::RecPhenology& Phenology = crop.var.Phenology;
  double Ta = bioclimate.AirTemperature ();

  if (Phenology.DS < 1)
    {
      Phenology.DS += Devel.DSRate1 * Devel.TempEff1 (Ta);
      if (Phenology.Vern < 0)
	crop. Vernalization (Ta);
    }
  else
    {
      Phenology.DS += Devel.DSRate2 * Devel.TempEff2 (Ta);
      if (Phenology.DS > 2)
	Phenology.DS = 2.0;
    }
}

static void 
devel_m3 (const Bioclimate& bioclimate, CropStandard& crop)
{
  const CropStandard::Parameters::DevelPar& Devel = crop.par.Devel;
  CropStandard::Variables::RecPhenology& Phenology = crop.var.Phenology;
  double Ta = bioclimate.AirTemperature ();

  if (Phenology.DS < 1)
    {
      Phenology.DS += (Devel.DSRate1
		       * Devel.TempEff1 (Ta)
		       * Devel.PhotEff1 (bioclimate.DayLength ()));
      if (Phenology.Vern < 0)
	crop.Vernalization (Ta);
    }
  else
    {
      Phenology.DS += Devel.DSRate2 * Devel.TempEff2 (Ta);
      if (Phenology.DS > 2)
	Phenology.DS = 2.0;
    }
}

CropStandard::Parameters::pList CropStandard::Parameters::all;

const CropStandard::Parameters& 
CropStandard::Parameters::get (const string n, const AttributeList& vl)
{
  for (pList::iterator i = all.begin (); i != all.end (); i++)
    {
      // BUG: We should test that all the parameters (but not
      // variables) are equal!
      if ((*i)->name == n)
	return *(*i);
    }
  const Parameters* p = new Parameters (n, vl);
  all.push_back (p);
  return *p;
}

CropStandard::Parameters::Parameters (const string n, const AttributeList& vl) 
  : Devel (vl.list ("Devel")),
    Vernal (vl.list ("Vernal")),
    LeafPhot (vl.list ("LeafPhot")),
    Canopy (vl.list ("Canopy")),
    Root (vl.list ("Root")),
    Partit (vl.list ("Partit")),
    Resp (vl.list ("Resp")),
    CrpN (vl.list ("CrpN")),
    IntcpCap (vl.number ("IntcpCap")),
    EpFac (vl.number ("EpFac")),
    name (n)
{ }

CropStandard::Parameters::DevelPar::DevelPar (const AttributeList& vl)
  : Model    (models.lookup (vl.name ("Model"))),
    EmrTSum (vl.number ("EmrTSum")),
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
  : Model (models.lookup (vl.name ("Model"))),
    Qeff (vl.number ("Qeff")),
    Fm (vl.number ("Fm")),
    TLim1 (vl.number ("TLim1")),
    TLim2 (vl.number ("TLim2"))
{ }

CropStandard::Parameters::CanopyPar::CanopyPar (const AttributeList& vl)
  : DSinit (vl.number ("DSinit")),
    WLfInit (vl.number ("WLfInit")),
    SpLAI (vl.number ("SpLAI")),
    HvsDS (vl.csmp ("HvsDS")),
    LAIDist0 (vl.array ("LAIDist0")),
    LAIDist1 (vl.array ("LAIDist1")),
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
    MxNO3Up (vl.number ("MxNO3Up"))
{ }

CropStandard::Parameters::PartitPar::PartitPar (const AttributeList& vl)
  : Root (vl.csmp ("Root")),
    Leaf (vl.csmp ("Leaf")),
    Stem (vl.csmp ("Stem")),
    LfDR (vl.csmp ("LfDR")),
    RtDR (vl.csmp ("RtDR"))
{ }

CropStandard::Parameters::RespPar::RespPar (const AttributeList& vl)
  : E_Root (vl.number ("E_Root")),
    E_Leaf (vl.number ("E_Leaf")),
    E_Stem (vl.number ("E_Stem")),
    E_SOrg (vl.number ("E_SOrg")),
    r_Root (vl.number ("r_Root")),
    r_Leaf (vl.number ("r_Leaf")),
    r_Stem (vl.number ("r_Stem")),
    r_SOrg (vl.number ("r_SOrg")),
    Q10 (vl.number ("Q10"))     
{ }

CropStandard::Parameters::CrpNPar::CrpNPar (const AttributeList& vl)
  : SeedN (vl.number ("SeedN")),
    PtLeafCnc (vl.csmp ("PtLeafCnc")),
    CrLeafCnc (vl.csmp ("CrLeafCnc")),
    PtStemCnc (vl.csmp ("PtStemCnc")),
    CrStemCnc (vl.csmp ("CrStemCnc")),
    PtRootCnc (vl.csmp ("PtRootCnc")),
    CrRootCnc (vl.csmp ("CrRootCnc")),
    PtSOrgCnc (vl.csmp ("PtSOrgCnc")),
    CrSOrgCnc (vl.csmp ("CrSOrgCnc"))
{ }

CropStandard::Parameters::~Parameters ()
{ }

CropStandard::Variables::Variables (const Parameters& par, 
				    const AttributeList& vl)
  : Phenology (par, vl.list ("Phenology")),
    Canopy (par, vl.list ("Canopy")),
    RootSys (par, vl.list ("RootSys")),
    Prod (par, vl.list ("Prod")),
    CrpAux (par, vl.list ("CrpAux"))
{ }

void 
CropStandard::Variables::output (Log& log, const Filter* filter) const
{
  if (filter->check ("Phenology"))
    Phenology.output (log, filter->lookup ("Phenology"));
  if (filter->check ("Canopy"))
    Canopy.output (log, filter->lookup ("Canopy"));
  if (filter->check ("RootSys"))
    RootSys.output (log, filter->lookup ("RootSys"));
  if (filter->check ("Prod"))
    Prod.output (log, filter->lookup ("Prod"));
  if (filter->check ("CrpAux"))
    CrpAux.output (log, filter->lookup ("CrpAux"));
}

CropStandard::Variables::RecPhenology::RecPhenology (const Parameters& par,
						     const AttributeList& vl)
  : DS (vl.number ("DS")),
    Vern (vl.check ("Vern") ? vl.number ("Vern") : par.Vernal.TaSum)
{ }

void 
CropStandard::Variables::RecPhenology::output (Log& log, const Filter* filter) const
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
CropStandard::Variables::RecCanopy::output (Log& log, const Filter* filter) const
{
  log.open ("Canopy");
  log.output ("Height", filter, Height);
  log.output ("LAI", filter, LAI);
  log.output ("LADm", filter, LADm);
  log.output ("LAIvsH", filter, LAIvsH);
  log.close();
}

CropStandard::Variables::RecRootSys::RecRootSys (const Parameters& par,
						 const AttributeList& vl)
  : Depth (vl.check ("Depth") ? vl.number ("Depth") : par.Root.DptEmr),
    Density (vl.array ("Density")),
    H2OExtraction (vl.array ("H2OExtraction")),
    NH4Extraction (vl.array ("NH4Extraction")),
    NO3Extraction (vl.array ("NO3Extraction"))
{ }

void 
CropStandard::Variables::RecRootSys::output (Log& log, const Filter* filter) const
{
  log.open ("RootSys");
  log.output ("Depth", filter, Depth);
  log.output ("Density", filter, Density);
  log.output ("H2OExtraction", filter, H2OExtraction);
  log.output ("NH4Extraction", filter, NH4Extraction);
  log.output ("NO3Extraction", filter, NO3Extraction);
  log.close();
}

CropStandard::Variables::RecProd::RecProd (const Parameters& par, 
					   const AttributeList& vl)
  : WLeaf (vl.number ("WLeaf")),
    WStem (vl.number ("WStem")),
    WRoot (vl.number ("WRoot")),
    WSOrg (vl.number ("WSOrg")),
    WLDrd (vl.number ("WLDrd")),
    NCrop (vl.check ("NCrop") ? vl.number ("NCrop") : par.CrpN.SeedN)
{ }

void 
CropStandard::Variables::RecProd::output (Log& log, const Filter* filter) const
{
  log.open ("Prod");
  log.output ("WLeaf", filter, WLeaf);
  log.output ("WStem", filter, WStem);
  log.output ("WRoot", filter, WRoot);
  log.output ("WSOrg", filter, WSOrg);
  log.output ("WLDrd", filter, WLDrd);
  log.output ("NCrop", filter, NCrop);
  log.close();
}

CropStandard::Variables::RecCrpAux::RecCrpAux (const Parameters& par, 
					       const AttributeList& vl)
  : InitLAI (vl.flag ("InitLAI")),
    PotRtDpt (  vl.check ("PotRtDpt") 
	      ? vl.number ("PotRtDpt")
	      : par.Root.DptEmr),
    PtNCnt (  vl.check ("PtNCnt")
	    ? vl.number ("PtNCnt")
	    : par.CrpN.SeedN),
    PotTransp (vl.number ("PotTransp")),
    PotCanopyAss (vl.number ("PotCanopyAss")),
    CanopyAss (vl.number ("CanopyAss")),
    IncWLeaf (vl.number ("IncWLeaf")),
    IncWStem (vl.number ("IncWStem")),
    IncWSOrg (vl.number ("IncWSOrg")),
    IncWRoot (vl.number ("IncWRoot")),
    H2OUpt (vl.number ("H2OUpt")),
    NH4Upt (vl.number ("NH4Upt")),
    NO3Upt (vl.number ("NO3Upt"))
{ }

void 
CropStandard::Variables::RecCrpAux::output (Log& log, const Filter* filter) const
{
  log.open ("CrpAux");
  log.output ("InitLAI", filter, InitLAI);
  log.output ("PotRtDpt", filter, PotRtDpt);
  log.output ("PtNCnt", filter, PtNCnt);
  log.output ("PotTransp", filter, PotTransp);
  log.output ("PotCanopyAss", filter, PotCanopyAss);
  log.output ("CanopyAss", filter, CanopyAss);
  log.output ("IncWLeaf", filter, IncWLeaf);
  log.output ("IncWStem", filter, IncWStem);
  log.output ("IncWSOrg", filter, IncWSOrg);
  log.output ("IncWRoot", filter, IncWRoot);
  log.output ("H2OUpt", filter, H2OUpt);
  log.output ("NH4Upt", filter, NH4Upt);
  log.output ("NO3Upt", filter, NO3Upt);
  log.close();
}

CropStandard::Variables::~Variables ()
{ }

// BUG: Should not be necessary.
void
NullCropFun(const Bioclimate&, Crop&)
{ }

dFTable<CropFun> CropStandard::Parameters::DevelPar::models;
dFTable<CropFun> CropStandard::Parameters::LeafPhotPar::models;

// Add the Crop syntax to the syntax table.
Crop*
CropStandard::make (const AttributeList& vl)
{
  return new CropStandard (vl);
}

static struct CropStandardSyntax
{
  CropStandardSyntax ();
} standard_crop_syntax;

CropStandardSyntax::CropStandardSyntax ()
{
  static const vector<double> empty_array;
  static const CSMP empty_csmp;

  // CropPar
  Syntax& syntax = *new Syntax ();
  AttributeList& alist = *new AttributeList ();

  // DevelPar
  CropStandard::Parameters::DevelPar::models.add("m1", &devel_m1);
  CropStandard::Parameters::DevelPar::models.add("m2", &devel_m2);
  CropStandard::Parameters::DevelPar::models.add("m3", &devel_m3);
  Syntax& Devel = *new Syntax ();
  syntax.add ("Devel", Devel);

  Devel.add ("Model", &CropStandard::Parameters::DevelPar::models);
  Devel.add ("EmrTSum", Syntax::Number);
  Devel.add ("DS_Emr", Syntax::Number);
  Devel.add ("DSRate1", Syntax::Number);
  Devel.add ("DSRate2", Syntax::Number);
  Devel.add ("TempEff1", Syntax::CSMP);
  Devel.add ("TempEff2", Syntax::CSMP);
  Devel.add ("PhotEff1", Syntax::CSMP);
    
  // VernalPar
  Syntax& Vernal = *new Syntax ();
  syntax.add ("Vernal", Vernal);

  Vernal.add ("required", Syntax::Boolean);
  Vernal.add ("DSLim1", Syntax::Number);
  Vernal.add ("DSLim2", Syntax::Number);
  Vernal.add ("TaLim", Syntax::Number);
  Vernal.add ("TaSum", Syntax::Number);

  // LeafPhotPar
  CropStandard::Parameters::LeafPhotPar::models.add("exponential",
						    &NullCropFun);
  CropStandard::Parameters::LeafPhotPar::models.add("parabolic",
						    &NullCropFun);

  Syntax& LeafPhot = *new Syntax ();
  syntax.add ("LeafPhot", LeafPhot);

  LeafPhot.add ("Model", &CropStandard::Parameters::LeafPhotPar::models);
  LeafPhot.add ("Qeff", Syntax::Number);
  LeafPhot.add ("Fm", Syntax::Number);
  LeafPhot.add ("TLim1", Syntax::Number);
  LeafPhot.add ("TLim2", Syntax::Number);

    // CanopyPar
  Syntax& Canopy = *new Syntax ();
  syntax.add ("Canopy", Canopy);

  Canopy.add ("DSinit", Syntax::Number);
  Canopy.add ("WLfInit", Syntax::Number);
  Canopy.add ("SpLAI", Syntax::Number);
  Canopy.add ("HvsDS", Syntax::CSMP);
  Canopy.add ("LAIDist0", 3);
  Canopy.add ("LAIDist1", 3);
  Canopy.add ("PARref", Syntax::Number);
  Canopy.add ("PARext", Syntax::Number);
  Canopy.add ("EPext", Syntax::Number);

  // RootPar
  Syntax& Root = *new Syntax ();
  syntax.add ("Root", Root);

  Root.add ("DptEmr", Syntax::Number);
  Root.add ("PenPar1", Syntax::Number);
  Root.add ("PenPar2", Syntax::Number);
  Root.add ("MaxPen", Syntax::Number);
  Root.add ("SpRtLength", Syntax::Number);
  Root.add ("DensRtTip", Syntax::Number);
  Root.add ("Rad", Syntax::Number);
  Root.add ("h_wp", Syntax::Number);
  Root.add ("MxNH4Up", Syntax::Number);
  Root.add ("MxNO3Up", Syntax::Number);

    // PartitPar
  Syntax& Partit = *new Syntax ();
  syntax.add ("Partit", Partit);

  Partit.add ("Root", Syntax::CSMP);
  Partit.add ("Leaf", Syntax::CSMP);
  Partit.add ("Stem", Syntax::CSMP);
  Partit.add ("LfDR", Syntax::CSMP);
  Partit.add ("RtDR", Syntax::CSMP);

  // RespPar
  Syntax& Resp = *new Syntax ();
  syntax.add ("Resp", Resp);

  Resp.add ("E_Root", Syntax::Number);
  Resp.add ("E_Leaf", Syntax::Number);
  Resp.add ("E_Stem", Syntax::Number);
  Resp.add ("E_SOrg", Syntax::Number);
  Resp.add ("r_Root", Syntax::Number);
  Resp.add ("r_Leaf", Syntax::Number);
  Resp.add ("r_Stem", Syntax::Number);
  Resp.add ("r_SOrg", Syntax::Number);
  Resp.add ("Q10", Syntax::Number);

  // CrpNPar
  Syntax& CrpN = *new Syntax ();
  syntax.add ("CrpN", CrpN);

  CrpN.add ("SeedN", Syntax::Number);
  CrpN.add ("PtLeafCnc", Syntax::CSMP);
  CrpN.add ("CrLeafCnc", Syntax::CSMP);
  CrpN.add ("PtStemCnc", Syntax::CSMP);
  CrpN.add ("CrStemCnc", Syntax::CSMP);
  CrpN.add ("PtRootCnc", Syntax::CSMP);
  CrpN.add ("CrRootCnc", Syntax::CSMP);
  CrpN.add ("PtSOrgCnc", Syntax::CSMP);
  CrpN.add ("CrSOrgCnc", Syntax::CSMP);

  // I don't know where these belong.
  syntax.add ("IntcpCap", Syntax::Number);
  syntax.add ("EpFac", Syntax::Number);

  // Variables.

  // Phenology
  Syntax& Phenology = *new Syntax ();
  AttributeList& vPhenology = *new AttributeList ();
  syntax.add ("Phenology", Phenology);
  alist.add ("Phenology", vPhenology);

  Phenology.add ("DS", Syntax::Number);
  vPhenology.add ("DS", -1.0);
  Phenology.add ("Vern", Syntax::Number, Syntax::Optional);

  // Canopy
  AttributeList& vCanopy = *new AttributeList ();
  syntax.add ("Canopy", Canopy);
  alist.add ("Canopy", vCanopy);

  Canopy.add ("Height", Syntax::Number);
  vCanopy.add ("Height", 0.0);
  Canopy.add ("LAI", Syntax::Number);
  vCanopy.add ("LAI", 0.0);
  Canopy.add ("LADm", Syntax::Number);
  vCanopy.add ("LADm", -9999.99);
  Canopy.add ("LAIvsH", Syntax::CSMP);
  vCanopy.add ("LAIvsH", &empty_csmp);

    // RootSys
  Syntax& RootSys = *new Syntax ();
  AttributeList& vRootSys = *new AttributeList ();
  syntax.add ("RootSys", RootSys);
  alist.add ("RootSys", vRootSys);

  RootSys.add ("Depth", Syntax::Number, Syntax::Optional);
  RootSys.add ("Density", Syntax::Array);
  vRootSys.add ("Density", empty_array);
  RootSys.add ("H2OExtraction", Syntax::Array);
  vRootSys.add ("H2OExtraction", empty_array);
  RootSys.add ("NH4Extraction", Syntax::Array);
  vRootSys.add ("NH4Extraction", empty_array);
  RootSys.add ("NO3Extraction", Syntax::Array);
  vRootSys.add ("NO3Extraction", empty_array);

  // Prod
  Syntax& Prod = *new Syntax ();
  AttributeList& vProd = *new AttributeList ();
  syntax.add ("Prod", Prod);
  alist.add ("Prod", vProd);

  Prod.add ("WLeaf", Syntax::Number);
  vProd.add ("WLeaf", 0.001);
  Prod.add ("WStem", Syntax::Number);
  vProd.add ("WStem", 0.000);
  Prod.add ("WRoot", Syntax::Number);
  vProd.add ("WRoot", 0.001);
  Prod.add ("WSOrg", Syntax::Number);
  vProd.add ("WSOrg", 0.000);
  Prod.add ("WLDrd", Syntax::Number);
  vProd.add ("WLDrd", 0.000);
  Prod.add ("NCrop", Syntax::Number, Syntax::Optional);

  // CrpAux
  Syntax& CrpAux = *new Syntax ();
  AttributeList& vCrpAux = *new AttributeList ();
  syntax.add ("CrpAux", CrpAux);
  alist.add ("CrpAux", vCrpAux);

  CrpAux.add ("InitLAI", Syntax::Boolean);
  vCrpAux.add ("InitLAI", true);
  CrpAux.add ("PotRtDpt", Syntax::Number, Syntax::Optional);
  CrpAux.add ("PtNCnt", Syntax::Number, Syntax::Optional);
  CrpAux.add ("CrNCnt", Syntax::Number);
  vCrpAux.add ("CrNCnt", 0.0);
  CrpAux.add ("PotTransp", Syntax::Number);
  vCrpAux.add ("PotTransp", 0.0);
  CrpAux.add ("PotCanopyAss", Syntax::Number);
  vCrpAux.add ("PotCanopyAss", 0.0);
  CrpAux.add ("CanopyAss", Syntax::Number);
  vCrpAux.add ("CanopyAss", 0.0);
  CrpAux.add ("IncWLeaf", Syntax::Number);
  vCrpAux.add ("IncWLeaf", 0.0);
  CrpAux.add ("IncWStem", Syntax::Number);
  vCrpAux.add ("IncWStem", 0.0);
  CrpAux.add ("IncWSOrg", Syntax::Number);
  vCrpAux.add ("IncWSOrg", 0.0);
  CrpAux.add ("IncWRoot", Syntax::Number);
  vCrpAux.add ("IncWRoot", 0.0);
  CrpAux.add ("H2OUpt", Syntax::Number);
  vCrpAux.add ("H2OUpt", 0.0);
  CrpAux.add ("NH4Upt", Syntax::Number);
  vCrpAux.add ("NH4Upt", 0.0);
  CrpAux.add ("NO3Upt", Syntax::Number);
  vCrpAux.add ("NO3Upt", 0.0);

  Crop::add_type ("crop", alist, syntax, &CropStandard::make);
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
CropStandard::SoluteUptake (string /* SoluteID */, double PotNUpt,
		    double /* I_Mx */, double /* Rad */)
{
  // const Variables::RecRootSys& RootSys = var.RootSys;

  return PotNUpt;
}

double
CropStandard::H2OUptake (double PotTransp, double /* RootRad */, double /* h_wp */)
{
  // const Variables::RecRootSys& RootSys = var.RootSys;

  return PotTransp;
}

void
CropStandard::Vernalization (double Ta)
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
CropStandard::Emergence (const Column& column)
{
  const Parameters::DevelPar& Devel = par.Devel;
  const double EmrDpt = par.Root.DptEmr;
  double& DS = var.Phenology.DS;

  DS += column.SoilTemperature (EmrDpt) / Devel.EmrTSum;
  if (DS > 0)
    DS = Devel.DS_Emr;
}

void 
CropStandard::DevelopmentStage (const Bioclimate& bioclimate)
{
  const Parameters::DevelPar& Devel = par.Devel;

  Devel.Model (bioclimate, *this);
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
      LAI = 0.5 * (exp (4.8 * DS) - 1);
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

void 
CropStandard::RootPenetration (const Column& column)
{
  const Parameters::RootPar& Root = par.Root;
  // const double DS = var.Phenology.DS;
  const double IncWRoot = var.CrpAux.IncWRoot;
  double& PotRtDpt = var.CrpAux.PotRtDpt;
  double& Depth = var.RootSys.Depth;
    
  if (IncWRoot <= 0)
    return;

  double Ts = column.SoilTemperature (Depth);
  double dp = Root.PenPar1 * max (0.0, Ts - Root.PenPar2);
  PotRtDpt = min (PotRtDpt + dp, Root.MaxPen);
  /*max depth determined by crop*/
  Depth = min (Depth + dp, Root.MaxPen);
  /*max depth determined by crop*/
  Depth = min (Depth, column.MaxRootingDepth ()); /*or by soil conditions*/
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
  while (fabs (2 * (z - y) / (z + y)) > 10e-6)
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
CropStandard::RootDensity ()
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

#if 0
  double z_b[81];
  double z_n[81];
  int Nz;
  double dz[81];

  column.SoilColumnDiscretization (Nz, z_b, z_n, dz);

  RootSys.Density.reserve (20);

  int i;
  for (i = 1; z_b[i] >= RootSys.Depth; i++)
    RootSys.Density[i - 1] = L0 * exp (-a * z_n[i - 1]);
  // RootSys.Nr = i - 1;

  for (int j = i - 1; j <= 19; j++)
    RootSys.Density[j] = 0.0;
#endif
}

void 
CropStandard::NitContent ()
{
  const Parameters::CrpNPar& CrpN = par.CrpN;
  const double DS = var.Phenology.DS;
  const Variables::RecProd& Prod = var.Prod;
  double& PtNCnt = var.CrpAux.PtNCnt;
  double& CrNCnt = var.CrpAux.CrNCnt;

  double NLeaf = CrpN.PtLeafCnc (DS) * Prod.WLeaf;
  double NStem = CrpN.PtStemCnc (DS) * Prod.WStem;
  double NSOrg = CrpN.PtSOrgCnc (DS) * Prod.WSOrg;
  double NRoot = CrpN.PtRootCnc (DS) * Prod.WRoot;
  PtNCnt = NLeaf + NStem + NSOrg + NRoot;

  NLeaf = CrpN.CrLeafCnc (DS) * Prod.WLeaf;
  NStem = CrpN.CrStemCnc (DS) * Prod.WStem;
  NSOrg = CrpN.CrSOrgCnc (DS) * Prod.WSOrg;
  NRoot = CrpN.CrRootCnc (DS) * Prod.WRoot;
  CrNCnt = NLeaf + NStem + NSOrg + NRoot;
}

#if 0
void 
CropStandard::Transpiration (RecRootPar RootPar, double PotTransp,
		     RecRootSys *RootSys, double *Transp,
		     double *WStress)
{
  for (i = 1; i <= 20; i++)
    RootSys.H2OExtraction[i - 1] = 0.0;
  Transp = H2OUptake (PotTransp, RootPar.Rad, RootPar.h_wp);
  if (PotTransp < 0.005)
    *WStress = 1.0;
  else
    *WStress = (*Transp + EvapInterception ()) / (PotTransp + EvapInterception ());
}
#endif

void 
CropStandard::NitrogenUptake (int Hour)
{
  const Parameters::RootPar& Root = par.Root;
  Variables::RecRootSys& RootSys = var.RootSys;
  Variables::RecCrpAux& CrpAux = var.CrpAux;
  double& NCrop = var.Prod.NCrop;

  double PotNUpt = (CrpAux.PtNCnt - NCrop) / ((Hour == 0) ? 1 : (25 - Hour));
  RootSys.NH4Extraction.reserve (20);
  RootSys.NO3Extraction.reserve (20);
  for (int i = 0; i <= 19; i++)
    {
      RootSys.NH4Extraction[i] = 0.0;
      RootSys.NO3Extraction[i] = 0.0;
    }
  if (PotNUpt > 0)
    {
      CrpAux.NH4Upt
	= SoluteUptake ("NH4", PotNUpt, Root.MxNH4Up, Root.Rad);
      NCrop += CrpAux.NH4Upt;
      PotNUpt -= CrpAux.NH4Upt;
    }
  else
    CrpAux.NH4Upt = 0.0;
  if (PotNUpt > 0)
    {
      CrpAux.NO3Upt
	= SoluteUptake ("NO3", PotNUpt, Root.MxNO3Up, Root.Rad); 
      NCrop += CrpAux.NO3Upt;
    }
  else
    CrpAux.NO3Upt = 0.0;
  NCrop = max (NCrop, CrpAux.PtNCnt);
}

double 
CropStandard::CanopyPhotosynthesis (const Bioclimate& bioclimate)
{
  // sugar production [gCH2O/m2/h] by canopy photosynthesis.
  const Parameters::LeafPhotPar& LeafPhot = par.LeafPhot;
  const CSMP& LAIvsH = var.Canopy.LAIvsH;

  double Teff;		// Temperature effect
  double F;			// Leaf Photosynthesis [gCO2/m2/h]
  double LA;			// Leaf Area index for a given leaf layer
  double prevLA = 0.0;	// LAI below the current leaf layer.
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

  int No = bioclimate.NumberOfIntervals ();
  for (int i = 0; i < No; i++)
    {
      double height = bioclimate.height (i);

      F = LeafPhot.Fm * (1 - exp (- (LeafPhot.Qeff *  bioclimate.PAR (i)
				     / LeafPhot.Fm)));
	    
      LA = LAIvsH (height) - prevLA;
      prevLA = LAIvsH (height);
	    
      Ass += LA * F;
    }
  return (molWeightCH2O / molWeightCO2 * Teff * Ass);
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
CropStandard::MaintenanceRespiration (double r, double Q10, double w, double T)
{
  if (w > 0)
    return (molWeightCH2O / molWeightCO2 * r * exp ((T - 20) / 10 * log (Q10)) * w);
  else
    return 0.0;
}

void 
CropStandard::NetProduction (const Column& column, const Bioclimate& bioclimate)
{
  const Parameters::PartitPar& Partit = par.Partit;
  const Parameters::RespPar& Resp = par.Resp;
  const double DS = var.Phenology.DS;
  const double Depth = var.RootSys.Depth;
  Variables::RecProd& Prod = var.Prod;
  Variables::RecCrpAux& CrpAux = var.CrpAux;

  double T = bioclimate.AirTemperature ();
  double RMLeaf
    = MaintenanceRespiration (Resp.r_Leaf, Resp.Q10, Prod.WLeaf, T);
  double RMStem
    = MaintenanceRespiration (Resp.r_Stem, Resp.Q10, Prod.WStem, T);
  double RMSOrg
    = MaintenanceRespiration (Resp.r_SOrg, Resp.Q10, Prod.WSOrg, T);
  T = column.SoilTemperature (Depth / 3);
  double RMRoot
    = MaintenanceRespiration (Resp.r_Root, Resp.Q10, Prod.WRoot, T);
  RMLeaf = max (0.0, RMLeaf - CrpAux.PotCanopyAss + CrpAux.CanopyAss);
  double RM = RMLeaf + RMStem + RMSOrg + RMRoot;
  double AssG, Stress, f_Leaf, f_Stem, f_SOrg, f_Root, DeadRate, DeadLeaf;
    
  if (CrpAux.CanopyAss >= RM)
    {
      AssG = CrpAux.CanopyAss - RM;
      Stress = min (1.0, Prod.NCrop / CrpAux.CrNCnt);
      AssimilatePartitioning (DS, f_Leaf, f_Stem, f_Root, f_SOrg);
      CrpAux.IncWLeaf = Stress * Resp.E_Leaf * f_Leaf * AssG;
      CrpAux.IncWStem = Stress * Resp.E_Stem * f_Stem * AssG;
      CrpAux.IncWSOrg = Stress * Resp.E_SOrg * f_SOrg * AssG;
      CrpAux.IncWRoot = Stress * Resp.E_Root * f_Root * AssG;
    }
  else
    {
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
    }
  DeadRate = Partit.LfDR (DS) * bioclimate.AirTemperature ();
  DeadLeaf = DeadRate * Prod.WLeaf;
  CrpAux.IncWLeaf -= DeadLeaf;
  Prod.WLDrd += DeadLeaf;
  Prod.WLeaf += CrpAux.IncWLeaf;
  Prod.WStem += CrpAux.IncWStem;
  Prod.WSOrg += CrpAux.IncWSOrg;
  Prod.WRoot += CrpAux.IncWRoot;
}

void 
CropStandard::tick (const Time& time, 
	    const Column& column, const Bioclimate& bioclimate)
{
  cout << "Crop `" << name << "' tick\n"; 

  if (time.hour () == 0 && var.Phenology.DS <= 0)
    {
      Emergence (column);
      if (var.Phenology.DS >= 0)
	{
	  InitialLAI ();
	  var.Canopy.Height = CropHeight ();
	}
      return;
    }
  if (var.Phenology.DS <= 0 || var.Phenology.DS >= 2)
    return;
  if (time.hour () == 1)
    {
      NitContent ();
      var.CrpAux.PotCanopyAss = 0.0;
      var.CrpAux.CanopyAss = 0.0;
    }
#if 0
  EPAdsorptionDist (CanStr.CanLAD, CanStr.CanPTr, Canopy,AdsPTr);
  CrpAux.PotTransp = 0.0;
  for (int i = 1; i < Canopy.NoLAD; i++)
    CrpAux.PotTransp = CrpAux.PotTransp + AdsPTr[2,i];
  Transpiration (Root, CrpAux.PotTransp, RootSys, CrpAux.H2OUpt, WStress);
#endif
  double WStress = 1.0;
  NitrogenUptake (time.hour ());
  if (bioclimate.PAR (bioclimate.NumberOfIntervals () - 1) > 0)
    {
      double Ass = CanopyPhotosynthesis (bioclimate);
      var.CrpAux.PotCanopyAss += Ass;
      var.CrpAux.CanopyAss += WStress * Ass;
    }
  if (time.hour () != 0)
    return;
  DevelopmentStage (bioclimate);
  var.Canopy.Height = CropHeight ();
  if (var.CrpAux.InitLAI)
    InitialLAI ();
  else
    var.Canopy.LAI = CropLAI ();
  NetProduction (column, bioclimate);
  RootPenetration (column);
  RootDensity ();
}

void
CropStandard::output (Log& log, const Filter* filter) const
{
  log.open (name);
  var.output (log, filter);
  log.close ();
}

CropStandard::CropStandard (const AttributeList& al)
  : Crop (al.name ("type")),
    par (Parameters::get (al.name ("type"), al)),
    var (*new Variables (par, al))
{ }

CropStandard::~CropStandard ()
{ 
  delete &var;
}
