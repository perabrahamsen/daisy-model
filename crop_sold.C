// crop_old.C
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
//
// This file is part of Daisy.
// 
// Daisy is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser Public License as published by
// the Free Software Foundation; either version 2.1 of the License, or
// (at your option) any later version.
// 
// Daisy is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser Public License for more details.
// 
// You should have received a copy of the GNU Lesser Public License
// along with Daisy; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


#include "crop.h"
#include "log.h"
#include "time.h"
#include "bioclimate.h"
#include "common.h"
#include "plf.h"
#include "soil_water.h"
#include "soil.h"
#include "organic_matter.h"
#include "om.h"
#include "soil_heat.h"
#include "soil_NH4.h"
#include "soil_NO3.h"
#include "am.h"
#include "harvest.h"
#include "mathlib.h"
#include "message.h"

class CropSold : public Crop
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
  const PLF& LAIvsH () const;
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
  void AssimilatePartitioning (double DS, double& f_Leaf, double& f_SOrg,
			       double& f_Root);
  double MaintenanceRespiration (double r, double w, double T);
  void NetProduction (const Bioclimate&, const Soil&, const SoilHeat&);

  // Simulation.
public:
  void tick (const Time& time, const Bioclimate&, const Soil&,
	     OrganicMatter*,
	     const SoilHeat&,
	     const SoilWater&, 
	     SoilNH4*,
	     SoilNO3*);
  const Harvest& harvest (const string& column_name, const Time&,
			  const Geometry& geometry, 
			  Bioclimate& bioclimate,
			  double stub_length, double stem_harvest,
			  double leaf_harvest, double sorg_harvest,
			  bool kill_off,
			  vector<AM*>& residuals);
  void output (Log&) const;

  double DS () const;
  double DM () const;
  double total_N () const;

  // Create and Destroy.
public:
  void initialize (const Geometry& geometry);
  CropSold (const AttributeList& vl);
  ~CropSold ();
};

typedef void (*CropFun)(const Bioclimate&, CropSold&);

struct CropSold::Parameters
{ 
  const struct DevelPar
  {
    double EmrTSum;		// Soil temp sum at emergence
    double DS_Emr;		// Development stage (DS) emergence
    bool DS_reset;		// True for winter crops.
    double DSRate1;		// Development rate [C-1 or d-1],
    // the vegetative stage
    double DSRate2;		// Development rate [C-1 or d-1],
    // the reproductive stage
    const PLF& TempEff1;   // Temperature effect, vegetative stage
    const PLF& TempEff2;   // Temperature effect, reproductive stage
    const PLF& PhotEff1;   // Ptotoperiode effect, vegetative stage
  private:
    friend struct CropSold::Parameters;
    DevelPar (const AttributeList&);
  } Devel;
  const struct VernalPar {
    bool required;
    double DSLim1;		// DS at beginning of vernalization
    double DSLim2;		// DS at end of vernalization
    double TaLim;		// Vernalization temp threshold
    double TaSum;		// Vernalization T-sum requirement
  private:
    friend struct CropSold::Parameters;
    VernalPar (const AttributeList&);
  } Vernal;
  const struct LeafPhotPar {
    double TLim1;		// Lowest temp for photosynthesis
    double TLim2;		// Lowest temp for unrestricted phot.
  private:
    friend struct CropSold::Parameters;
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
    const PLF& HvsDS;	// Crop height as function of DS
    const vector<double>& LAIDist0; // Relative LAI distribution at DS=0
    const vector<double>& LAIDist1; // Relative LAI distribution at DS=1
    double PARref;		// PAR reflectance
    double PARext;		// PAR extinction coefficient
    double EPext;		// EP extinction coefficient
  private:
    friend struct CropSold::Parameters;
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
    double Rxylem;		// Transport resistance in xyleme
  private:
    friend struct CropSold::Parameters;
    RootPar (const AttributeList&);
  } Root;
  const struct PartitPar {
    const PLF& Root;	// Partitioning functions for root
    const PLF& SOrg;	// Partitioning functions for storage organ
  private:
    friend struct CropSold::Parameters;
    PartitPar (const AttributeList&);
  } Partit;
  struct RespPar {
    const PLF& E_Root;		// Conversion efficiency, root
    const PLF& E_SOrg;		// Conversion efficiency, storage organ
    const PLF& E_Leaf;		// Conversion efficiency, leaf
    const PLF& r_Root;		// Maint. resp. coeff., root
    const PLF& r_SOrg;		// Maint. resp. coeff., storage organ
    const PLF& r_Leaf;		// Maint. resp. coeff., leaf
  private:
    friend struct CropSold::Parameters;
    RespPar (const AttributeList&);
  } Resp;
  struct CrpNPar {
    double SeedN;		// N-content in seed [ g N/m² ]
    double DS_fixate;		// Fixation of atmospheric N.
    const PLF& PtLeafCnc;	// Upper limit for N-conc in leaves
    const PLF& CrLeafCnc;	// Critical lim f. N-conc in leaves
    const PLF& NfLeafCnc;	// Non-func lim f. N-conc in leaves
    const PLF& PtSOrgCnc;	// Upper limit for N-conc in storage organ
    const PLF& CrSOrgCnc;	// Critical lim f. N-conc in storage organ
    const PLF& NfSOrgCnc;	// Non-func lim f. N-conc in storage organ
    const PLF& PtRootCnc;	// Upper limit for N-conc in roots
    const PLF& CrRootCnc;	// Critical lim f. N-conc in roots
    const PLF& NfRootCnc;	// Non-func lim f. N-conc in roots
  private:
    friend struct CropSold::Parameters;
    CrpNPar (const AttributeList&);
  } CrpN;
  struct HarvestPar {
    const double beta;		// The root/top concentration relation
    const double At;		// N distribution parameter.
    const double Bt;		// N distribution parameter.
    const vector<AttributeList*>& Leaf; // Leaf AM parameters.
    const vector<AttributeList*>& SOrg; // SOrg AM parameters.
    const vector<AttributeList*>& Root; // Root AM parameters.
    const double C_Leaf;	// C fraction of total weight.
    const double C_SOrg;	// C fraction of total weight.
    const double C_Root;	// C fraction of total weight.
    const double DSmax;		// Maximal development stage for which
				// the crop survives harvest.
    const double DSnew;		// Maximal development stage after harvest.
  private:
    friend struct CropSold::Parameters;
    HarvestPar (const AttributeList&);
  } Harvest;
  // Dunno where these belongs...
  double IntcpCap;
  double EpFac;
private:
  friend class CropSold;
  Parameters (const AttributeList&);
public:
  ~Parameters ();
};

struct CropSold::Variables
{ 
  void output (Log&) const;
  struct RecPhenology
  {
    void output (Log&) const;
    double DS;		// Development Stage
    double Vern;		// Vernalization criterium [C d]
  private:
    friend struct CropSold::Variables;
    RecPhenology (const Parameters&, const AttributeList&);
  } Phenology;
  struct RecCanopy
  {
    void output (Log&) const;
    double Height;		// Crop height [cm]
    double LAI;		// Leaf Area Index
    double LADm;		// Max Leaf Area Density [cm2/cm3]
    PLF LAIvsH;		// Accumulated Leaf Area Index at Height
  private:
    friend struct CropSold::Variables;
    RecCanopy (const Parameters&, const AttributeList&);
  } Canopy;
  struct RecRootSys
  {
    void output (Log&) const;
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
    double ws_up;		// Water stress factor.
    double ws_down;		// Water stress denominator.
    double Ept;			// Potential evapotranspiration.
    double transpiration;	// Total water uptake.
  private:
    friend struct CropSold::Variables;
    RecRootSys (const Parameters&, const AttributeList&);
  } RootSys;
  struct RecProd
  {
    void output (Log&) const;
    double WLeaf;		// Leaf dry matter weight [g/m2]
    double WhiteStubble;	// Old dead leftover from harvest.
    double WSOrg;		// Storage organ dry matter weight [g/m2]
    double WRoot;		// Root dry matter weight [g/m2]
    double NCrop;		// Nitrogen stored in dry matter [g/m2]
  private:
    friend struct CropSold::Variables;
    RecProd (const Parameters&, const AttributeList&);
  } Prod;
  struct RecCrpAux
  {
    void output (Log&) const;
    bool InitLAI;		// Initial LAI development ?
    double PotRtDpt;	// Potential Root Penetration Depth [cm]
    double PtNCnt;		// Potential Nitrogen Content in Crop [g/m2]
    double CrNCnt;		// Critical Nitrogen Content in Crop [g/m2]
    double NfNCnt;		// Non-func Nitrogen Content in Crop [g/m2]
    double PotTransp;	// Potential Transpiration [mm/h]
    double PotCanopyAss;	// Potential Canopy Assimilation [g CH2O/m2]
    double CanopyAss;	// Canopy Assimilation [g CH2O/m2]
    double IncWLeaf;	// Leaf growth [g DM/m2/d]
    double IncWSOrg;	// Storage organ growth [g DM/m2/d]
    double IncWRoot;	// Root growth [g DM/m2/d]
    // double H2OUpt;		// H2O uptake [mm/h]
    double NH4Upt;		// NH4-N uptake [g/m2/h]
    double NO3Upt;		// NO3-N uptake [g/m2/h]
    double Fixated;		// N fixation from air. [g/m2/h]
  private:
    friend struct CropSold::Variables;
    RecCrpAux (const Parameters&, const AttributeList&);
  } CrpAux;
private:
  friend class CropSold;
  Variables (const Parameters&, const AttributeList&);
public:
  ~Variables ();
};

CropSold::Parameters::Parameters (const AttributeList& vl) 
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
    EpFac (vl.number ("EpFac"))
{ }

CropSold::Parameters::DevelPar::DevelPar (const AttributeList& vl)
  : EmrTSum (vl.number ("EmrTSum")),
    DS_Emr (vl.number ("DS_Emr")),
    DS_reset (vl.flag ("DS_reset")),
    DSRate1 (vl.number ("DSRate1")),
    DSRate2 (vl.number ("DSRate2")),
    TempEff1 (vl.plf ("TempEff1")),
    TempEff2 (vl.plf ("TempEff2")),
    PhotEff1 (vl.plf ("PhotEff1"))
{ }

CropSold::Parameters::VernalPar::VernalPar (const AttributeList& vl)
  : required (vl.flag ("required")),
    DSLim1 (vl.number ("DSLim1")),
    DSLim2 (vl.number ("DSLim2")),
    TaLim (vl.number ("TaLim")),
    TaSum (vl.number ("TaSum"))
{ }

CropSold::Parameters::LeafPhotPar::LeafPhotPar (const AttributeList& vl)
  : TLim1 (vl.number ("TLim1")),
    TLim2 (vl.number ("TLim2"))
{ }

CropSold::Parameters::CanopyPar::CanopyPar (const AttributeList& vl)
  : InitGrowth (vl.number ("InitGrowth")),
    DSinit (vl.number ("DSinit")),
    WLfInit (vl.number ("WLfInit")),
    DS1 (vl.number ("DS1")),
    alpha (vl.number ("alpha")),
    beta (vl.number ("beta")),
    SpLAI (vl.number ("SpLAI")),
    HvsDS (vl.plf ("HvsDS")),
    LAIDist0 (vl.number_sequence ("LAIDist0")),
    LAIDist1 (vl.number_sequence ("LAIDist1")),
    PARref (vl.number ("PARref")),
    PARext (vl.number ("PARext")),
    EPext (vl.number ("EPext"))
{ }

CropSold::Parameters::RootPar::RootPar (const AttributeList& vl)
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

CropSold::Parameters::PartitPar::PartitPar (const AttributeList& vl)
  : Root (vl.plf ("Root")),
    SOrg (vl.plf ("SOrg"))
{ }

CropSold::Parameters::RespPar::RespPar (const AttributeList& vl)
  : E_Root (vl.plf ("E_Root")),
    E_SOrg (vl.plf ("E_SOrg")),
    E_Leaf (vl.plf ("E_Leaf")),
    r_Root (vl.plf ("r_Root")),
    r_SOrg (vl.plf ("r_SOrg")),
    r_Leaf (vl.plf ("r_Leaf"))
{ }

CropSold::Parameters::CrpNPar::CrpNPar (const AttributeList& vl)
  : SeedN (vl.number ("SeedN")),
    DS_fixate (vl.number ("DS_fixate")),
    PtLeafCnc (vl.plf ("PtLeafCnc")),
    CrLeafCnc (vl.plf ("CrLeafCnc")),
    NfLeafCnc (vl.plf ("NfLeafCnc")),
    PtSOrgCnc (vl.plf ("PtSOrgCnc")),
    CrSOrgCnc (vl.plf ("CrSOrgCnc")),
    NfSOrgCnc (vl.plf ("NfSOrgCnc")),
    PtRootCnc (vl.plf ("PtRootCnc")),
    CrRootCnc (vl.plf ("CrRootCnc")),
    NfRootCnc (vl.plf ("NfRootCnc"))
{ }

CropSold::Parameters::HarvestPar::HarvestPar (const AttributeList& vl)
  : beta (vl.number ("beta")),
    At (vl.number ("At")),
    Bt (vl.number ("Bt")),
    Leaf (vl.alist_sequence ("Leaf")),
    SOrg (vl.alist_sequence ("SOrg")),
    Root (vl.alist_sequence ("Root")),
    C_Leaf (vl.number ("C_Leaf")),
    C_SOrg (vl.number ("C_SOrg")),
    C_Root (vl.number ("C_Root")),
    DSmax (vl.number ("DSmax")),
    DSnew (vl.number ("DSnew"))
{ }

CropSold::Parameters::~Parameters ()
{ }

CropSold::Variables::Variables (const Parameters& par, 
				    const AttributeList& vl)
  : Phenology (par, vl.alist ("Phenology")),
    Canopy (par, vl.alist ("Canopy")),
    RootSys (par, vl.alist ("RootSys")),
    Prod (par, vl.alist ("Prod")),
    CrpAux (par, vl.alist ("CrpAux"))
{ }

void 
CropSold::Variables::output (Log& log) const
{
  if (log.check ("Phenology"))
    Phenology.output (log);
  if (log.check ("Canopy"))
    Canopy.output (log);
  if (log.check ("RootSys"))
    RootSys.output (log);
  if (log.check ("Prod"))
    Prod.output (log);
  if (log.check ("CrpAux"))
    CrpAux.output (log);
}

CropSold::Variables::RecPhenology::RecPhenology (const Parameters& par,
						     const AttributeList& vl)
  : DS (vl.number ("DS")),
    Vern (vl.check ("Vern") ? vl.number ("Vern") : par.Vernal.TaSum)
{ }

void 
CropSold::Variables::RecPhenology::output (Log& log) const
{
  log.open ("Phenology");
  log.output ("DS", DS);
  log.output ("Vern", Vern);
  log.close();
}

CropSold::Variables::RecCanopy::RecCanopy (const Parameters&,
					       const AttributeList& vl)
  : Height (vl.number ("Height")),
    LAI (vl.number ("LAI")),
    LADm (vl.number ("LADm")),
    LAIvsH (vl.plf ("LAIvsH"))
{ }

void 
CropSold::Variables::RecCanopy::output (Log& log) const
{
  log.open ("Canopy");
  log.output ("Height", Height);
  log.output ("LAI", LAI);
  log.output ("LADm", LADm);
  log.output ("LAIvsH", LAIvsH);
  log.close();
}

CropSold::Variables::RecRootSys::RecRootSys (const Parameters& par,
						 const AttributeList& vl)
  : Depth (vl.check ("Depth") ? vl.number ("Depth") : par.Root.DptEmr),
    Density (vl.number_sequence ("Density")),
    H2OExtraction (vl.number_sequence ("H2OExtraction")),
    NH4Extraction (vl.number_sequence ("NH4Extraction")),
    NO3Extraction (vl.number_sequence ("NO3Extraction")),
    h_x (vl.number ("h_x")),
    water_stress (0.0),
    ws_up (0.0),
    ws_down (0.0),
    Ept (0.0),
    transpiration (0.0)
{ }

void 
CropSold::Variables::RecRootSys::output (Log& log) const
{
  log.open ("RootSys");
  log.output ("Depth", Depth);
  log.output ("Density", Density);
  log.output ("H2OExtraction", H2OExtraction);
  log.output ("NH4Extraction", NH4Extraction);
  log.output ("NO3Extraction", NO3Extraction);
  log.output ("h_x", h_x);
  log.output ("water_stress", water_stress);
  log.output ("transpiration", transpiration);
  log.output ("Ept", Ept);
  log.close();
}

CropSold::Variables::RecProd::RecProd (const Parameters& par, 
					   const AttributeList& vl)
  : WLeaf (vl.number ("WLeaf")),
    WhiteStubble (vl.number ("WhiteStubble")),
    WSOrg (vl.number ("WSOrg")),
    WRoot (vl.number ("WRoot")),
    NCrop (vl.check ("NCrop") ? vl.number ("NCrop") : par.CrpN.SeedN)
{ }

void 
CropSold::Variables::RecProd::output (Log& log) const
{
  log.open ("Prod");
  log.output ("WLeaf", WLeaf);
  log.output ("WhiteStubble", WhiteStubble);
  log.output ("WSOrg", WSOrg);
  log.output ("WRoot", WRoot);
  log.output ("NCrop", NCrop);
  log.close();
}

CropSold::Variables::RecCrpAux::RecCrpAux (const Parameters& par, 
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
    IncWLeaf (0.0),
    IncWSOrg (0.0),
    IncWRoot (0.0),
    // H2OUpt (vl.number ("H2OUpt")),
    NH4Upt (0.0),
    NO3Upt (0.0),
    Fixated (0.0)
{ }

void 
CropSold::Variables::RecCrpAux::output (Log& log) const
{
  log.open ("CrpAux");
  log.output ("InitLAI", InitLAI);
  log.output ("PotRtDpt", PotRtDpt);
  log.output ("PtNCnt", PtNCnt);
  log.output ("PotTransp", PotTransp);
  log.output ("PotCanopyAss", PotCanopyAss);
  log.output ("CanopyAss", CanopyAss);
  log.output ("IncWLeaf", IncWLeaf);
  log.output ("IncWSOrg", IncWSOrg);
  log.output ("IncWRoot", IncWRoot);
  // log.output ("H2OUpt", H2OUpt);
  log.output ("NH4Upt", NH4Upt);
  log.output ("NO3Upt", NO3Upt);
  log.output ("Fixated", Fixated);
  log.close();
}

CropSold::Variables::~Variables ()
{ }

void
CropSold::initialize (const Geometry& geometry)
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

static struct CropSoldSyntax
{
  static Crop& make (const AttributeList& al)
    { return *new CropSold (al); }
  CropSoldSyntax ();
} old_crop_syntax;

CropSoldSyntax::CropSoldSyntax ()
{
  static const vector<double> empty_array;
  static const PLF empty_plf;

  // Submodels.
  Syntax& Canopy = *new Syntax ();
  AttributeList vCanopy;
  Syntax& Devel = *new Syntax ();
  Syntax& Vernal = *new Syntax ();
  Syntax& LeafPhot = *new Syntax ();
  Syntax& Root = *new Syntax ();
  Syntax& Partit = *new Syntax ();
  Syntax& Resp = *new Syntax ();
  Syntax& CrpN = *new Syntax ();
  AttributeList CrpNList;
  Syntax& Harvest = *new Syntax ();
  AttributeList HarvestList;
  Syntax& Phenology = *new Syntax ();
  AttributeList vPhenology;
  Syntax& RootSys = *new Syntax ();
  AttributeList vRootSys;
  Syntax& Prod = *new Syntax ();
  AttributeList vProd;
  Syntax& CrpAux = *new Syntax ();
  AttributeList vCrpAux;

  // DevelPar
  Devel.add ("EmrTSum", "dg C d", Syntax::Const,
	     "Soil temperature sum at emergence.");
  Devel.add ("DS_Emr", Syntax::None (), Syntax::Const,
	     "Development stage at emergence.");
  Devel.add ("DS_reset", Syntax::Boolean, Syntax::Const,
	     "True for winter crops.");
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
    
  // VernalPar
  Vernal.add ("required", Syntax::Boolean, Syntax::Const,
	      "True, iff the crop requires vernalization.");
  Vernal.add ("DSLim1", Syntax::None (), Syntax::Const,
	      "DS at beginning of vernalization.");
  Vernal.add ("DSLim2", Syntax::None (), Syntax::Const,
	      "DS at end of vernalization.");
  Vernal.add ("TaLim", "dg C", Syntax::Const,
	      "Vernalization temperature threshold.");
  Vernal.add ("TaSum", "dg C d", Syntax::Const,
	      "Vernalization temperature-sum requirement.");

  // LeafPhotPar
#if 0
  LeafPhot.add ("Qeff", Syntax::Unknown (), Syntax::Const,
		"Unused, for compatibility only.");
  LeafPhot.add ("Fm", Syntax::Unknown (), Syntax::Const,
		"Unused, for compatibility only.");
#endif
  LeafPhot.add ("TLim1", "dg C", Syntax::Const,
		"Lowest temperature for photosynthesis.");
  LeafPhot.add ("TLim2", "dg C", Syntax::Const,
		"Lowest temperature for unrestricted photosynthesis.");

  // CanopyPar
  Canopy.add ("InitGrowth", Syntax::None (), Syntax::Const,
	      "Initial growth parameter.");
  Canopy.add ("DSinit", Syntax::None (), Syntax::Const,
	      "DS at end of initial LAI-Development.");
  Canopy.add ("WLfInit", "g DM/m^2", Syntax::Const,
	      "WLeaf at end of initial LAI-Development.");
  Canopy.add ("DS1", Syntax::None (), Syntax::Const,
	      "DS state at start of leaf recession.");
  Canopy.add ("alpha", Syntax::None (), Syntax::Const,
	      "Leaf recession parameter.");
  Canopy.add ("beta", Syntax::None (), Syntax::Const,
	      "Leaf recession parameter.");
  Canopy.add ("SpLAI", "(m^2/m^2)/(g/m^2)", Syntax::Const,
	      "Specific leaf weight.");
  Canopy.add ("HvsDS", "DS", "cm", Syntax::Const,
	      "Crop height as function of DS.");
  Canopy.add ("LAIDist0", Syntax::None (), Syntax::Const, 3,
	      "Relative LAI distribution at DS=0.");
  Canopy.add ("LAIDist1", Syntax::None (), Syntax::Const, 3,
	      "Relative LAI distribution at DS=1.");
  Canopy.add ("PARref", Syntax::None (), Syntax::Const,
	      "PAR reflectance.");
  Canopy.add ("PARext", Syntax::None (), Syntax::Const,
	      "PAR extinction coefficient.");
  Canopy.add ("EPext", Syntax::None (), Syntax::Const,
	      "EP extinction coefficient.");

  // RootPar
  Root.add ("DptEmr", "cm", Syntax::Const,
	    "Penetration at emergence.");
  Root.add ("PenPar1", "cm/dg C/d", Syntax::Const,
	    "Penetration rate parameter, coefficient.");
  Root.add ("PenPar2", "dg C", Syntax::Const,
	    "Penetration rate parameter, threshold.");
  Root.add ("MaxPen", "cm", Syntax::Const,
	    "Maximum penetration depth.");
  Root.add ("SpRtLength", "km/g", Syntax::Const,
	    "Specific root length.");
  Root.add ("DensRtTip", "cm/cm^3", Syntax::Const,
	    "Root density at (potential) penetration depth.");
  Root.add ("Rad", "cm", Syntax::Const,
	    "Root radius.");
  Root.add ("h_wp", "cm", Syntax::Const,
	    "Matrix potential at wilting point.");
  Root.add ("MxNH4Up", "g/cm/h", Syntax::Const,
	    "Maximum NH4 uptake per unit root length.");
  Root.add ("MxNO3Up", "g/cm/h", Syntax::Const,
	    "Maximum NO3 uptake per unit root length.");
  Root.add ("Rxylem", Syntax::None (), Syntax::Const,
	    "Transport resistance in xyleme.");

  // PartitPar
  Partit.add ("Root", "DS", Syntax::Unknown (), Syntax::Const,
	      "Partitioning functions for root.");
  Partit.add ("SOrg", "DS", Syntax::Unknown (), Syntax::Const,
	      "Partitioning functions for storage organ.");

  // RespPar
  Resp.add ("E_Root", "DS", Syntax::Unknown (), Syntax::Const,
	    "Conversion efficiency, root.");
  Resp.add ("E_SOrg", "DS", Syntax::Unknown (), Syntax::Const,
	    "Conversion efficiency, storage organ.");
  Resp.add ("E_Leaf", "DS", Syntax::Unknown (), Syntax::Const,
	    "Conversion efficiency, leaf.");
  Resp.add ("r_Root", "DS", Syntax::Unknown (), Syntax::Const,
	    "Maintenance respiration coefficient, root.");
  Resp.add ("r_SOrg", "DS", Syntax::Unknown (), Syntax::Const,
	    "Maintenance respiration coefficient, root.");
  Resp.add ("r_Leaf", "DS", Syntax::Unknown (), Syntax::Const,
	    "Maintenance respiration coefficient, leaf.");

  // CrpNPar
  CrpN.add ("SeedN", "g N/m^2", Syntax::Const,
	    "N-content in seed.");
  CrpN.add ("DS_fixate", Syntax::None (), Syntax::Const,
            "DS at which to start fixation of atmospheric N.");
  CrpNList.add ("DS_fixate", 42000.0);
  CrpN.add ("PtLeafCnc", "DS", " g N/g DM", Syntax::Const,
	    "Upper limit for N-concentration in leaves.");
  CrpN.add ("CrLeafCnc", "DS", " g N/g DM", Syntax::Const,
	    "Critical limit for N-concentration in leaves.");
  CrpN.add ("NfLeafCnc", "DS", " g N/g DM", Syntax::Const, "\
Non-functional limit for N-concentration in leaves.");
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

  // HarvestPar
  Harvest.add ("beta", "sqrt ((g N/m^2)/(g DM/m^2))", Syntax::Const,
	       "N partitioning at harvest parameter.");
  Harvest.add ("At", "(g N/m^2)/(g DM/m^2)", Syntax::Const,
	       "N partitioning at harvest parameter.");
  Harvest.add ("Bt", Syntax::None (), Syntax::Const,
	       "N partitioning at harvest parameter.");
  Harvest.add_submodule_sequence ("Leaf", Syntax::Const, 
				 "Leaf AM parameters.", OM::load_syntax);
  Harvest.add_submodule_sequence ("SOrg", Syntax::Const,
				 "SOrg AM parameters.", OM::load_syntax);
  Harvest.add_submodule_sequence ("Root", Syntax::Const,
				 "Root AM parameters.", OM::load_syntax);
  Harvest.add ("C_Leaf", Syntax::None (), Syntax::Const,
	       "C fraction of total weight.");
  Harvest.add ("C_SOrg", Syntax::None (), Syntax::Const,
	       "C fraction of total weight.");
  Harvest.add ("C_Root", Syntax::None (), Syntax::Const,
	       "C fraction of total weight.");
  Harvest.add ("DSmax", Syntax::None (), Syntax::Const, "\
Maximal development stage for which the crop survives harvest.");
  HarvestList.add ("DSmax", 0.0);
  Harvest.add ("DSnew", Syntax::None (), Syntax::Const,
	       "New development stage after harvest.");
  HarvestList.add ("DSnew", 0.0);

  // Phenology
  Phenology.add ("DS", Syntax::None (), Syntax::State,
		 "Development Stage.");
  vPhenology.add ("DS", -1.0);
  Phenology.add ("Vern", "dg C d", Syntax::OptionalState,
		 "Vernalization criterium.");
 
  // Canopy
  Canopy.add ("Height", "cm", Syntax::State,
	      "Crop height.");
  vCanopy.add ("Height", 0.0);
  Canopy.add ("LAI", "m^2/m^2", Syntax::State,
	      "Leaf Area Index.");
  vCanopy.add ("LAI", 0.0);
  Canopy.add ("LADm", "cm^2/cm^3", Syntax::State,
	      "Maximal Leaf Area Density.");
  vCanopy.add ("LADm", -9999.99);
  Canopy.add ("LAIvsH", "cm", "m^2/m^2", Syntax::State,
	      "Accumulated Leaf Area Index at Height.");
  vCanopy.add ("LAIvsH", empty_plf);

    // RootSys
  RootSys.add ("Depth", "cm", Syntax::OptionalState,
	       "Rooting Depth.");
  RootSys.add ("Density", "cm/cm3", Syntax::State, Syntax::Sequence,
	       "Root density in soil layers.");
  vRootSys.add ("Density", empty_array);
  RootSys.add ("H2OExtraction", "cm^3/cm^3/h", Syntax::State, Syntax::Sequence,
	       "Extraction of H2O in soil layers.");
  vRootSys.add ("H2OExtraction", empty_array);
  RootSys.add ("NH4Extraction", "g N/cm^3/h", Syntax::State, Syntax::Sequence,
	       "Extraction of NH4-N in soil layers.");
  vRootSys.add ("NH4Extraction", empty_array);
  RootSys.add ("NO3Extraction", "g N/cm^3/h", Syntax::State, Syntax::Sequence,
	       "Extraction of NO3-N in soil layers.");
  vRootSys.add ("NO3Extraction", empty_array);
  RootSys.add ("h_x", "cm", Syntax::State,
	       "Root extraction at surface.");
  vRootSys.add ("h_x", 0.0);
  RootSys.add ("water_stress", Syntax::None (), Syntax::LogOnly,
	       "Fraction of requested water we got.");
  RootSys.add ("transpiration", "mm/h", Syntax::LogOnly,
	       "Total water uptake.");
  RootSys.add ("Ept", "mm/h", Syntax::LogOnly,
	       "Potential transpiration.");

  // Prod
  Prod.add ("WLeaf", "g DM/m^2", Syntax::State,
	    "Leaf dry matter weight.");
  vProd.add ("WLeaf", 0.001);
  Prod.add ("WhiteStubble", "g DM/m^2", Syntax::State,
	    "Old dead leftover from harvest.");
  vProd.add ("WhiteStubble", 0.0);
  Prod.add ("WSOrg", "g DM/m^2", Syntax::State,
	    "Storage organ dry matter weight.");
  vProd.add ("WSOrg", 0.0);
  Prod.add ("WRoot", "g DM/m^2", Syntax::State,
	    "Root dry matter weight.");
  vProd.add ("WRoot", 0.001);
  Prod.add ("NCrop", "g N/m^2", Syntax::OptionalState,
	    "Nitrogen stored in dry matter.");

  // CrpAux
  CrpAux.add ("InitLAI", Syntax::Boolean, Syntax::State,
	      "Initial LAI development.");
  vCrpAux.add ("InitLAI", true);
  CrpAux.add ("PotRtDpt", "cm", Syntax::OptionalState,
	      "Potential root penetration depth.");
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
  CrpAux.add ("IncWLeaf", "g DM/m^2/d", Syntax::LogOnly,
	      "Leaf growth.");
  CrpAux.add ("IncWSOrg", "g DM/m^2/d", Syntax::LogOnly,
	      "Stroage organ growth.");
  CrpAux.add ("IncWRoot", "g DM/m^2/d", Syntax::LogOnly,
	      "Root growth.");
  CrpAux.add ("NH4Upt", "g N/m^2/h", Syntax::LogOnly,
	      "NH4-N uptake.");
  CrpAux.add ("NO3Upt", "g N/m^2/h", Syntax::LogOnly,
	      "NO3-N uptake.");
  CrpAux.add ("Fixated", "g N/m^2/h", Syntax::LogOnly,
	      " fixation from air.");

  // Model.
  Syntax& syntax = *new Syntax ();
  AttributeList& alist = *new AttributeList ();
  alist.add ("description", "\
Daisy v1 crop model, for crops with storage organ in the soil.");

  syntax.add ("IntcpCap", "mm", Syntax::Const,
	      "Interception capacity.");
  syntax.add ("EpFac", Syntax::None (), Syntax::Const,
	      "Potential evapotranspiration factor.");

  // Add subcomponents.
  syntax.add ("Devel", Devel, "Crop development.");
  syntax.add ("Vernal", Vernal, "Vernalization.");
  syntax.add ("LeafPhot", LeafPhot, "Leaf photosynthesis.");
  syntax.add ("Root", Root, "The root system.");
  syntax.add ("Partit", Partit, "Assimilate partitioning.");
  syntax.add ("Resp", Resp, "Respiration.");
  syntax.add ("CrpN", CrpN, "Nitrogen content in the crop.");
  alist.add ("CrpN", CrpNList);
  syntax.add ("Harvest", Harvest, "Harvest parameters.");
  alist.add ("Harvest", HarvestList);
  syntax.add ("Phenology", Phenology, "Crop development.");
  alist.add ("Phenology", vPhenology);
  syntax.add ("Canopy", Canopy, "Canopy structure.");
  alist.add ("Canopy", vCanopy);
  syntax.add ("RootSys", RootSys, "Root system state.");
  alist.add ("RootSys", vRootSys);
  syntax.add ("Prod", Prod, "Crop production.");
  alist.add ("Prod", vProd);
  syntax.add ("CrpAux", CrpAux, "Miscellaneous crop state variables..");
  alist.add ("CrpAux", vCrpAux);

  Librarian<Crop>::add_type ("sold", alist, syntax, &make);
}

double
CropSold::height () const
{
  return var.Canopy.Height;
}

double
CropSold::LAI () const
{
  return var.Canopy.LAI;
}

const PLF&
CropSold::LAIvsH () const
{
  return var.Canopy.LAIvsH;
}

double
CropSold::PARext () const
{
  return par.Canopy.PARext;
}

double
CropSold::PARref () const
{
  return par.Canopy.PARref;
}

double
CropSold::EPext () const
{
  return par.Canopy.EPext;
}

double
CropSold::IntcpCap () const
{
  return par.IntcpCap;
}

double
CropSold::EpFac () const
{
  return par.EpFac;
}

double
CropSold::SoluteUptake (const Soil& soil,
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
      if (L > 0  && soil_water.h (i) <= 0.0)
	uptake[i] = bound (0.0, 
			   L * (min (I_zero[i], I_max)
				- B_zero[i] * c_root),
			   max (0.0, solute.M_left (i) - 1e-8));
      else
	uptake[i] = 0.0;
      assert (uptake[i] >= 0.0);
    }
  solute.add_to_sink (uptake);

  // gN/cm³/h -> gN/m²/h
  return soil.total (uptake) * 1.0e4; 
}

void
CropSold::Vernalization (double Ta)
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
CropSold::Emergence (const Soil& soil, const SoilHeat& soil_heat)
{
  const Parameters::DevelPar& Devel = par.Devel;
  const double EmrDpt = par.Root.DptEmr;
  double& DS = var.Phenology.DS;

  DS += soil_heat.T (soil.interval_plus (-EmrDpt)) / Devel.EmrTSum;
  if (DS > 0)
    DS = Devel.DS_Emr;
}

void 
CropSold::DevelopmentStage (const Bioclimate& bioclimate)
{
  const Parameters::DevelPar& Devel = par.Devel;
  Variables::RecPhenology& Phenology = var.Phenology;

  const double Ta = bioclimate.daily_air_temperature ();

  if (Phenology.DS < 1)
    {
      Phenology.DS += (Devel.DSRate1
		       * Devel.TempEff1 (Ta)
		       * Devel.PhotEff1 (bioclimate.day_length ()));
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
CropSold::CropHeight ()
{
  const Parameters::CanopyPar& Canopy = par.Canopy;
  double& DS = var.Phenology.DS;

  return Canopy.HvsDS (DS);
}

void 
CropSold::InitialLAI ()
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
CropSold::CropLAI ()
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
CropSold::CanopyStructure ()
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
	      PLF LADvsH;
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
    
  // Create PLF for old "z0, z1, z2" distribution.
  PLF LADvsH;
  LADvsH.add (z0 * Canopy.Height, 0.0);
  LADvsH.add (z1 * Canopy.Height, Canopy.LADm);
  LADvsH.add (z2 * Canopy.Height, Canopy.LADm);
  LADvsH.add (     Canopy.Height, 0.0);
  Canopy.LAIvsH = LADvsH.integrate_stupidly ();
}

double
CropSold::ActualWaterUptake (double Ept,
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

  // We need this to make sure H2OExtraction corresponds to 'h_x'.
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
  soil_water.root_uptake (H2OExtraction);
  // Update water stress factor
  if (Ept >= 0.010)
    {
      assert (total <= Ept);

      var.RootSys.ws_up += (total + EvapInterception);
      var.RootSys.ws_down += (Ept + EvapInterception);
    }

  // Update transpiration.
  double& transpiration = var.RootSys.transpiration;
  transpiration = total;

  return total;
}

double
CropSold::PotentialWaterUptake (const double h_x, 
				    const Soil& soil, const SoilWater& soil_water)
{
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
      const double uptake
	= max (2 * M_PI * L[i] * (soil_water.Theta (soil, i, h) 
				  / soil_water.Theta (soil, i, 0.0))
	       * (soil.M (i, soil_water.h (i)) - soil.M (i, h))
	       / (- 0.5 * log (area * L[i])),
	       0.0);
      assert (L[i] >= 0.0);
      assert (soil_water.Theta (soil, i, h) > 0.0);
      assert (soil_water.Theta (soil, i, 0.0) > 0.0);
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
CropSold::RootPenetration (const Soil& soil, const SoilHeat& soil_heat)
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
CropSold::RootDensDistPar (double a)
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
CropSold::RootDensity (const Soil& soil)
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
CropSold::NitContent ()
{
  const Parameters::CrpNPar& CrpN = par.CrpN;
  const double DS = var.Phenology.DS;
  const Variables::RecProd& Prod = var.Prod;

  var.CrpAux.PtNCnt = CrpN.PtLeafCnc (DS) * Prod.WLeaf
    + CrpN.PtSOrgCnc (DS) * Prod.WSOrg
    + CrpN.PtRootCnc (DS) * Prod.WRoot;

  var.CrpAux.CrNCnt = CrpN.CrLeafCnc (DS) * Prod.WLeaf
    + CrpN.CrSOrgCnc (DS) * Prod.WSOrg
    + CrpN.CrRootCnc (DS) * Prod.WRoot;

  var.CrpAux.NfNCnt = CrpN.NfLeafCnc (DS) * Prod.WLeaf
    + CrpN.NfSOrgCnc (DS) * Prod.WSOrg
    + CrpN.NfRootCnc (DS) * Prod.WRoot;
}

void
CropSold::NitrogenUptake (int Hour, 
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

  if (PotNUpt > 0 && var.Phenology.DS > par.CrpN.DS_fixate)
    {
      CrpAux.Fixated = 0.8 * PotNUpt;
      NCrop += CrpAux.Fixated;
      // PotNUpt -= CrpAux.Fixated;
    }
  else
    CrpAux.Fixated = 0.0;
}

void 
CropSold::AssimilatePartitioning (double DS, double& f_Leaf,
				  double& f_SOrg, double& f_Root)
{
  const Parameters::PartitPar& Partit = par.Partit;
    
  f_Root = Partit.Root (DS);
  f_SOrg = Partit.SOrg (DS);
  f_Leaf = max (0.0, 1.0 - f_Root - f_SOrg);
}

double 
CropSold::MaintenanceRespiration (double r, double w, double T)
{
  if (w > 0)
    return r * max (0.0, 0.4281 * (exp (0.57 - 0.024 * T + 0.0020 * T * T)
				   - exp (0.57 - 0.042 * T - 0.0051 * T * T)))
      * w;
  else
    return 0.0;
}

void 
CropSold::NetProduction (const Bioclimate& bioclimate,
			     const Soil& soil, const SoilHeat& soil_heat)
{
  const Parameters::RespPar& Resp = par.Resp;
  const double DS = var.Phenology.DS;
  const double Depth = var.RootSys.Depth;
  Variables::RecProd& Prod = var.Prod;
  Variables::RecCrpAux& CrpAux = var.CrpAux;

  const double RMLeaf
    = MaintenanceRespiration (Resp.r_Leaf (DS), Prod.WLeaf, 
			      bioclimate.daily_air_temperature ());
  const double RMSOrg
    = MaintenanceRespiration (Resp.r_SOrg (DS), Prod.WSOrg, 
			      bioclimate.daily_air_temperature ());
  const double RMRoot
    = MaintenanceRespiration (Resp.r_Root (DS), Prod.WRoot, 
			      soil_heat.T (soil.interval_plus (-Depth / 3)));

  double f_Leaf, f_SOrg, f_Root;
  AssimilatePartitioning (DS, f_Leaf, f_SOrg, f_Root);
  assert (approximate (f_Leaf + f_SOrg + f_Root, 1.0));

  const double AssG = CrpAux.CanopyAss 
    * bound (0.0, ((Prod.NCrop - CrpAux.NfNCnt) 
		   / (CrpAux.CrNCnt - CrpAux.NfNCnt)),
	     1.0);
  
  CrpAux.IncWLeaf = Resp.E_Leaf (DS) * (f_Leaf * AssG - RMLeaf);
  if (CrpAux.IncWLeaf < 0.0)
    CrpAux.IncWLeaf = f_Leaf * AssG - RMLeaf;
  CrpAux.IncWSOrg = Resp.E_SOrg (DS) * (f_SOrg * AssG - RMSOrg);
  if (CrpAux.IncWSOrg < 0.0)
    CrpAux.IncWSOrg = f_Root * AssG - RMSOrg;
  CrpAux.IncWRoot = Resp.E_Root (DS) * (f_Root * AssG - RMRoot);
  if (CrpAux.IncWRoot < 0.0)
    CrpAux.IncWRoot = f_Root * AssG - RMRoot;
  Prod.WLeaf += CrpAux.IncWLeaf;
  Prod.WSOrg += CrpAux.IncWSOrg;
  Prod.WRoot += CrpAux.IncWRoot;
}

void 
CropSold::tick (const Time& time,
		const Bioclimate& bioclimate,
		const Soil& soil,
		OrganicMatter*,
		const SoilHeat& soil_heat,
		const SoilWater& soil_water, 
		SoilNH4* soil_NH4,
		SoilNO3* soil_NO3)
{
  // Clear log.
  fill (var.RootSys.NO3Extraction.begin (), 
	var.RootSys.NO3Extraction.end (),
	0.0);
  fill (var.RootSys.NH4Extraction.begin (), 
	var.RootSys.NH4Extraction.end (),
	0.0);

  if (par.Devel.DS_reset 
      && time.month () == 3
      && time.mday () == 1
      && time.hour () == 6)
    {
      // It was a bad winter.
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
  if (time.hour () != 0)
    return;
  double& water_stress = var.RootSys.water_stress;
  if (var.RootSys.ws_down > 0)
    water_stress = 1.0 - var.RootSys.ws_up / var.RootSys.ws_down;
  else
    water_stress = 0.0;
  assert (water_stress >= 0.0 && water_stress <= 1.0);
  var.RootSys.ws_up = 0.0;
  var.RootSys.ws_down = 0.0;

  const double Ta = bioclimate.daily_air_temperature ();

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
  const double Si = bioclimate.daily_global_radiation () * (24*60*60); // [J/m²/d]
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
  else if (var.CrpAux.InitLAI)
    var.CrpAux.CanopyAss += Ass;
  else
    var.CrpAux.CanopyAss += (1.0 - water_stress) * Ass;

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
CropSold::harvest (const string& column_name,
		   const Time& time, const Geometry& geometry,
		   Bioclimate&,
		   double,
		   double, double leaf_harvest, double sorg_harvest, 
		   bool, vector<AM*>& residuals)
{
  const Parameters::HarvestPar& Hp = par.Harvest;
  Variables::RecProd& Prod = var.Prod;

  const double beta = Hp.beta;
  const double At = Hp.At;
  const double Bt = Hp.Bt;

  const double WLeaf = Prod.WLeaf;
  const double WRoot = Prod.WRoot;
  const double WSOrg = Prod.WSOrg;
  const double NCrop = Prod.NCrop;

  const double WCrop = WLeaf + WSOrg + WRoot;
  const double XLeaf = WLeaf / WCrop;
  const double XSOrg = WSOrg / WCrop;
  const double XRoot = WRoot / WCrop;
  
  const double A = (XLeaf  + XSOrg * Bt) / (beta * beta);
  const double B = XRoot;
  const double C = - NCrop / WCrop + XSOrg * At;

  const double CRoot = (sqrt (B * B - 4.0 * A * C) - B) / (2.0 * A);
  const double NRoot = CRoot * WRoot;

  const double CLeaf = pow (CRoot / beta, 2.0);
  const double NLeaf = CLeaf * WLeaf;

  const double CSOrg = At + Bt * CLeaf;
  const double NSOrg = CSOrg * WSOrg;

  assert (NLeaf >= 0.0);
  assert (NSOrg >= 0.0);
  assert (NRoot >= 0.0);
  assert (NCrop >= 0.0);
  assert (approximate (NLeaf + NSOrg + NRoot, NCrop));

  const double C_Leaf = Hp.C_Leaf;
  const double C_SOrg = Hp.C_SOrg;
  const double C_Root = Hp.C_Root;

  const vector<AttributeList*>& Leaf = Hp.Leaf;
  const vector<AttributeList*>& SOrg = Hp.SOrg;
  const vector<AttributeList*>& Root = Hp.Root;

  const vector<double>& density = var.RootSys.Density;

  var.Phenology.DS = DSremove;

  const double m2_per_cm2 = 0.0001;

  // Add crop remains to the soil.
  if (leaf_harvest < 1.0 && WLeaf > 0.0)
    {
      AM& am = AM::create (geometry, time, Leaf, name, "leaf");
      am.add (WLeaf * C_Leaf * (1.0 - leaf_harvest) * m2_per_cm2, 
	      NLeaf * (1.0 - leaf_harvest) * m2_per_cm2);
      residuals.push_back (&am);
    }
  if (sorg_harvest < 1.0 && WSOrg > 0.0)
    {
      AM& am = AM::create (geometry, time, SOrg, name, "sorg");
      am.add (WSOrg * C_SOrg * (1.0 - sorg_harvest) * m2_per_cm2,
	      NSOrg * (1.0 - sorg_harvest) * m2_per_cm2);
      residuals.push_back (&am);
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
      residuals.push_back (&am);
    }
  Chemicals chemicals;
  return *new Harvest (column_name, time, name, 
		       WLeaf * leaf_harvest, NLeaf * leaf_harvest, 0.0,
		       0.0, 0.0, 0.0,
		       0.0, 0.0, 0.0,
		       WSOrg * sorg_harvest, NSOrg * sorg_harvest, 0.0,
		       chemicals);
}

void
CropSold::output (Log& log) const
{
  var.output (log);
}

double
CropSold::DS () const
{ return var.Phenology.DS; }

double
CropSold::DM () const	// [g/m² -> kg/ha]
{ return (var.Prod.WSOrg + var.Prod.WLeaf) * 10; }

double
CropSold::total_N () const
{
  // kg/ha -> g/cm^2
  const double conv = (1000.0 / ((100.0 * 100.0) * (100.0 * 100.0)));
  return var.Prod.NCrop / conv;
}

CropSold::CropSold (const AttributeList& al)
  : Crop (al),
    par (*new Parameters (al)),
    var (*new Variables (par, al))
{ }

CropSold::~CropSold ()
{ 
  delete &var;
}
