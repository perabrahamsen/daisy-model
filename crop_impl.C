// crop_impl.C

#include "crop_impl.h"
#include "syntax.h"
#include "alist.h"
#include "filter.h"
#include "log.h"
#include "bioclimate.h"

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
CropStandard::make (string name, 
		    const AttributeList& par, 
		    const AttributeList& var)
{
  return new CropStandard (name, par, var);
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
  Syntax& parSyntax = *new Syntax ();
  AttributeList& parList = *new AttributeList ();
  {
    // DevelPar
    CropStandard::Parameters::DevelPar::models.add("m1", &devel_m1);
    CropStandard::Parameters::DevelPar::models.add("m2", &devel_m2);
    CropStandard::Parameters::DevelPar::models.add("m3", &devel_m3);
    Syntax& Devel = *new Syntax ();
    parSyntax.add ("Devel", Devel);

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
    parSyntax.add ("Vernal", Vernal);

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
    parSyntax.add ("LeafPhot", LeafPhot);

    LeafPhot.add ("Model", &CropStandard::Parameters::LeafPhotPar::models);
    LeafPhot.add ("Qeff", Syntax::Number);
    LeafPhot.add ("Fm", Syntax::Number);
    LeafPhot.add ("TLim1", Syntax::Number);
    LeafPhot.add ("TLim2", Syntax::Number);

    // CanopyPar
    Syntax& Canopy = *new Syntax ();
    parSyntax.add ("Canopy", Canopy);

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
    parSyntax.add ("Root", Root);

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
    parSyntax.add ("Partit", Partit);

    Partit.add ("Root", Syntax::CSMP);
    Partit.add ("Leaf", Syntax::CSMP);
    Partit.add ("Stem", Syntax::CSMP);
    Partit.add ("LfDR", Syntax::CSMP);
    Partit.add ("RtDR", Syntax::CSMP);

    // RespPar
    Syntax& Resp = *new Syntax ();
    parSyntax.add ("Resp", Resp);

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
    parSyntax.add ("CrpN", CrpN);

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
    parSyntax.add ("IntcpCap", Syntax::Number);
    parSyntax.add ("EpFac", Syntax::Number);
  }

  // Variables
  Syntax& varSyntax = *new Syntax ();
  AttributeList& varList = *new AttributeList ();
  {
    // Phenology
    Syntax& Phenology = *new Syntax ();
    AttributeList& vPhenology = *new AttributeList ();
    varSyntax.add ("Phenology", Phenology);
    varList.add ("Phenology", vPhenology);

    Phenology.add ("DS", Syntax::Number);
    vPhenology.add ("DS", -1.0);
    Phenology.add ("Vern", Syntax::Number, Syntax::Optional);

    // Canopy
    Syntax& Canopy = *new Syntax (); 
    AttributeList& vCanopy = *new AttributeList ();
    varSyntax.add ("Canopy", Canopy);
    varList.add ("Canopy", vCanopy);

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
    varSyntax.add ("RootSys", RootSys);
    varList.add ("RootSys", vRootSys);

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
    varSyntax.add ("Prod", Prod);
    varList.add ("Prod", vProd);

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
    varSyntax.add ("CrpAux", CrpAux);
    varList.add ("CrpAux", vCrpAux);

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
  }
  Crop::add_type ("crop", parList, parSyntax, varList, varSyntax, 
		  &CropStandard::make);
}
