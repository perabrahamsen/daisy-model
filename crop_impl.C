// crop_impl.C

#include "crop_impl.h"
#include "syntax.h"
#include "alist.h"
#include "csmp.h"
#include "filter.h"
#include "log.h"
#include "bioclimate.h"

static void 
devel_m1 (Crop& crop)
{
    const Crop::Parameters::DevelPar& Devel = crop.par.Devel;
    Crop::Variables::RecPhenology& Phenology = crop.var.Phenology;
    double Ta = crop.bioclimate.AirTemperature ();

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
devel_m2 (Crop& crop)
{
    const Crop::Parameters::DevelPar& Devel = crop.par.Devel;
    Crop::Variables::RecPhenology& Phenology = crop.var.Phenology;
    double Ta = crop.bioclimate.AirTemperature ();

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
devel_m3 (Crop& crop)
{
    const Crop::Parameters::DevelPar& Devel = crop.par.Devel;
    Crop::Variables::RecPhenology& Phenology = crop.var.Phenology;
    double Ta = crop.bioclimate.AirTemperature ();

    if (Phenology.DS < 1)
	{
	    Phenology.DS += (Devel.DSRate1
			      * Devel.TempEff1 (Ta)
			      * Devel.PhotEff1 (crop.bioclimate.DayLength ()));
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

Crop::Parameters::pList Crop::Parameters::all;

const Crop::Parameters& 
Crop::Parameters::get (const string n, const AttributeList& vl)
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

Crop::Parameters::Parameters (const string n, const AttributeList& vl) 
    : Devel (vl.list ("Devel")),
      Vernal (vl.list ("Vernal")),
      LeafPhot (vl.list ("LeafPhot")),
      Canopy (vl.list ("Canopy")),
      Root (vl.list ("Root")),
      Partit (vl.list ("Partit")),
      Resp (vl.list ("Resp")),
      CrpN (vl.list ("CrpN")),
      name (n)
{ }

Crop::Parameters::DevelPar::DevelPar (const AttributeList& vl)
    : Model    (models.lookup (vl.name ("Model"))),
      EmrTSum (vl.number ("EmrTSum")),
      DS_Emr (vl.number ("DS_Emr")),
      DSRate1 (vl.number ("DSRate1")),
      DSRate2 (vl.number ("DSRate2")),
      TempEff1 (vl.csmp ("TempEff1")),
      TempEff2 (vl.csmp ("TempEff2")),
      PhotEff1 (vl.csmp ("PhotEff1"))
{ }

Crop::Parameters::VernalPar::VernalPar (const AttributeList& vl)
    : required (vl.flag ("required")),
      DSLim1 (vl.number ("DSLim1")),
      DSLim2 (vl.number ("DSLim2")),
      TaLim (vl.number ("TaLim")),
      TaSum (vl.number ("TaSum"))
{ }

Crop::Parameters::LeafPhotPar::LeafPhotPar (const AttributeList& vl)
    : Model (models.lookup (vl.name ("Model"))),
      Qeff (vl.number ("Qeff")),
      Fm (vl.number ("Fm")),
      TLim1 (vl.number ("TLim1")),
      TLim2 (vl.number ("TLim2"))
{ }

Crop::Parameters::CanopyPar::CanopyPar (const AttributeList& vl)
    : DSinit (vl.number ("DSinit")),
      WLfInit (vl.number ("WLfInit")),
      SpLAI (vl.number ("SpLAI")),
      HvsDS (vl.csmp ("HvsDS")),
      LAIDist0 (vl.array ("LAIDist0")),
      LAIDist1 (vl.array ("LAIDist1")),
      LAIDista (vl.number ("LAIDista")),
      PARref (vl.number ("PARref")),
      PARext (vl.number ("PARext")),
      EPext (vl.number ("EPext"))
{ }

Crop::Parameters::RootPar::RootPar (const AttributeList& vl)
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

Crop::Parameters::PartitPar::PartitPar (const AttributeList& vl)
    : Root (vl.csmp ("Root")),
      Leaf (vl.csmp ("Leaf")),
      Stem (vl.csmp ("Stem")),
      LfDR (vl.csmp ("LfDR")),
      RtDR (vl.csmp ("RtDR"))
{ }

Crop::Parameters::RespPar::RespPar (const AttributeList& vl)
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

Crop::Parameters::CrpNPar::CrpNPar (const AttributeList& vl)
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

Crop::Parameters::~Parameters ()
{ }

Crop::Variables::Variables (const Parameters& par)
    : Phenology (par),
      Canopy (par),
      RootSys (par),
      Prod (par),
      CrpAux (par)
{ }

Crop::Variables::Variables (const AttributeList& vl)
    : Phenology (vl.list ("Phenology")),
      Canopy (vl.list ("Canopy")),
      RootSys (vl.list ("RootSys")),
      Prod (vl.list ("Prod")),
      CrpAux (vl.list ("CrpAux"))
{ }

void 
Crop::Variables::output (Log& log, const Filter* filter) const
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

Crop::Variables::RecPhenology::RecPhenology (const Parameters& par)
    : DS (-1.0),
      Vern (par.Vernal.TaSum)
{ }

Crop::Variables::RecPhenology::RecPhenology (const AttributeList& vl)
    : DS (vl.number ("DS")),
      Vern (vl.number ("Vern"))
{ }

void 
Crop::Variables::RecPhenology::output (Log& log, const Filter* filter) const
{
    log.open ("Phenology");
    log.output ("DS", filter, DS);
    log.output ("Vern", filter, Vern);
    log.close();
}

Crop::Variables::RecCanopy::RecCanopy (const Parameters& /* par */)
    : Height (0.0),
      LAI (0.0),
      LADm (-9999.99), 
      LADDist0 (), 
      LADDist1 ()
{ }

Crop::Variables::RecCanopy::RecCanopy (const AttributeList& vl)
    : Height (vl.number ("Height")),
      LAI (vl.number ("LAI")),
      LADm (vl.number ("LADm")),
      LADDist0 (vl.array ("LADDist0")),
      LADDist1 (vl.array ("LADDist1"))
{ }

void 
Crop::Variables::RecCanopy::output (Log& log, const Filter* filter) const
{
    log.open ("Canopy");
    log.output ("Height", filter, Height);
    log.output ("LAI", filter, LAI);
    log.output ("LADm", filter, LADm);
    log.output ("LADDist0", filter, LADDist0);
    log.output ("LADDist1", filter, LADDist1);
    log.close();
}

Crop::Variables::RecRootSys::RecRootSys (const Parameters& par)
    : Depth (par.Root.DptEmr)
// Density, H2OExtraction, NH4Extraction, NO3Extraction
{ }

Crop::Variables::RecRootSys::RecRootSys (const AttributeList& vl)
    : Depth (vl.number ("Depth")),
      Density (vl.array ("Density")),
      H2OExtraction (vl.array ("H2OExtraction")),
      NH4Extraction (vl.array ("NH4Extraction")),
      NO3Extraction (vl.array ("NO3Extraction"))
{ }

void 
Crop::Variables::RecRootSys::output (Log& log, const Filter* filter) const
{
    log.open ("RootSys");
    log.output ("Depth", filter, Depth);
    log.output ("Density", filter, Density);
    log.output ("H2OExtraction", filter, H2OExtraction);
    log.output ("NH4Extraction", filter, NH4Extraction);
    log.output ("NO3Extraction", filter, NO3Extraction);
    log.close();
}

Crop::Variables::RecProd::RecProd (const Parameters& par)
    : WLeaf (0.001),
      WStem (0.000),
      WRoot (0.001),
      WSOrg (0.000),
      WLDrd (0.000),
      NCrop (par.CrpN.SeedN)
{ }

Crop::Variables::RecProd::RecProd (const AttributeList& vl)
    : WLeaf (vl.number ("WLeaf")),
      WStem (vl.number ("WStem")),
      WRoot (vl.number ("WRoot")),
      WSOrg (vl.number ("WSOrg")),
      WLDrd (vl.number ("WLDrd")),
      NCrop (vl.number ("NCrop"))
{ }

void 
Crop::Variables::RecProd::output (Log& log, const Filter* filter) const
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

Crop::Variables::RecCrpAux::RecCrpAux (const Parameters& par)
    : InitLAI (true),
      PotRtDpt (par.Root.DptEmr),
      PtNCnt (par.CrpN.SeedN),
      // PotTransp, PotCanopyAss
      CanopyAss(0.0),
      IncWLeaf (0.0),
      IncWStem (0.0),
      IncWSOrg (0.0),
      IncWRoot (0.0)
// H2OUpt, NH4Upt, NO3Upt
{ }

Crop::Variables::RecCrpAux::RecCrpAux (const AttributeList& vl)
    : InitLAI (vl.flag ("InitLAI")),
      PotRtDpt (vl.number ("PotRtDpt")),
      PtNCnt (vl.number ("PtNCnt")),
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
Crop::Variables::RecCrpAux::output (Log& log, const Filter* filter) const
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

Crop::Variables::~Variables ()
{ }

dFTable<CropFun> Crop::Parameters::DevelPar::models;
dFTable<CropFun> Crop::Parameters::LeafPhotPar::models;

// Add the Crop syntax to the syntax table.
static struct CropSyntax
{
    void parameters ();
    void variables ();
    CropSyntax ();
} crop_syntax;

CropSyntax::CropSyntax ()
{ 
    parameters ();
    variables ();
}

void CropSyntax::parameters ()
{ 
    // CropPar
    Syntax* par = new Syntax ();
    syntax_table->add ("crop", par);
    
    // DevelPar
    Crop::Parameters::DevelPar::models.add("m1", &devel_m1);
    Crop::Parameters::DevelPar::models.add("m2", &devel_m2);
    Crop::Parameters::DevelPar::models.add("m3", &devel_m3);
    Syntax* Devel = new Syntax ();
    par->add ("Devel", Devel);

    Devel->add ("Model", &Crop::Parameters::DevelPar::models);
    Devel->add ("EmrTSum", Syntax::Number);
    Devel->add ("DS_Emr", Syntax::Number);
    Devel->add ("DSRate1", Syntax::Number);
    Devel->add ("DSRate2", Syntax::Number);
    Devel->add ("TempEff1", Syntax::CSMP);
    Devel->add ("TempEff2", Syntax::CSMP);
    Devel->add ("PhotEff1", Syntax::CSMP);
    
    // VernalPar
    Syntax* Vernal = new Syntax ();
    par->add ("Vernal", Vernal);

    Vernal->add ("required", Syntax::Boolean);
    Vernal->add ("DSLim1", Syntax::Number);
    Vernal->add ("DSLim2", Syntax::Number);
    Vernal->add ("TaLim", Syntax::Number);
    Vernal->add ("TaSum", Syntax::Number);

    // LeafPhotPar
    Crop::Parameters::LeafPhotPar::models.add("exponential", (CropFun) 0);
    Crop::Parameters::LeafPhotPar::models.add("parabolic", (CropFun) 0);

    Syntax* LeafPhot = new Syntax ();
    par->add ("LeafPhot", LeafPhot);

    LeafPhot->add ("Model", &Crop::Parameters::LeafPhotPar::models);
    LeafPhot->add ("Qeff", Syntax::Number);
    LeafPhot->add ("Fm", Syntax::Number);
    LeafPhot->add ("TLim1", Syntax::Number);
    LeafPhot->add ("TLim2", Syntax::Number);

    // CanopyPar
    Syntax* Canopy = new Syntax ();
    par->add ("Canopy", Canopy);

    Canopy->add ("DSinit", Syntax::Number);
    Canopy->add ("WLfInit", Syntax::Number);
    Canopy->add ("SpLAI", Syntax::Number);
    Canopy->add ("HvsDS", Syntax::CSMP);
    Canopy->add ("LAIDist0", 3);
    Canopy->add ("LAIDist1", 3);
    Canopy->add ("LAIDista", Syntax::Number);
    Canopy->add ("PARref", Syntax::Number);
    Canopy->add ("PARext", Syntax::Number);
    Canopy->add ("EPext", Syntax::Number);

    // RootPar
    Syntax* Root = new Syntax ();
    par->add ("Root", Root);

    Root->add ("DptEmr", Syntax::Number);
    Root->add ("PenPar1", Syntax::Number);
    Root->add ("PenPar2", Syntax::Number);
    Root->add ("MaxPen", Syntax::Number);
    Root->add ("SpRtLength", Syntax::Number);
    Root->add ("DensRtTip", Syntax::Number);
    Root->add ("Rad", Syntax::Number);
    Root->add ("h_wp", Syntax::Number);
    Root->add ("MxNH4Up", Syntax::Number);
    Root->add ("MxNO3Up", Syntax::Number);

    // PartitPar
    Syntax* Partit = new Syntax ();
    par->add ("Partit", Partit);

    Partit->add ("Root", Syntax::CSMP);
    Partit->add ("Leaf", Syntax::CSMP);
    Partit->add ("Stem", Syntax::CSMP);
    Partit->add ("LfDR", Syntax::CSMP);
    Partit->add ("RtDR", Syntax::CSMP);

    // RespPar
    Syntax* Resp = new Syntax ();
    par->add ("Resp", Resp);

    Resp->add ("E_Root", Syntax::Number);
    Resp->add ("E_Leaf", Syntax::Number);
    Resp->add ("E_Stem", Syntax::Number);
    Resp->add ("E_SOrg", Syntax::Number);
    Resp->add ("r_Root", Syntax::Number);
    Resp->add ("r_Leaf", Syntax::Number);
    Resp->add ("r_Stem", Syntax::Number);
    Resp->add ("r_SOrg", Syntax::Number);
    Resp->add ("Q10", Syntax::Number);

    // CrpNPar
    Syntax* CrpN = new Syntax ();
    par->add ("CrpN", CrpN);

    CrpN->add ("SeedN", Syntax::Number);
    CrpN->add ("PtLeafCnc", Syntax::CSMP);
    CrpN->add ("CrLeafCnc", Syntax::CSMP);
    CrpN->add ("PtStemCnc", Syntax::CSMP);
    CrpN->add ("CrStemCnc", Syntax::CSMP);
    CrpN->add ("PtRootCnc", Syntax::CSMP);
    CrpN->add ("CrRootCnc", Syntax::CSMP);
    CrpN->add ("PtSOrgCnc", Syntax::CSMP);
    CrpN->add ("CrSOrgCnc", Syntax::CSMP);
}

void CropSyntax::variables ()
{ 
    // CropVar
    Syntax* var = new Syntax ();
    syntax_table->add ("crop/state", var);
    
    // Phenology
    Syntax* Phenology = new Syntax ();
    var->add ("Phenology", Phenology);

    Phenology->add ("DS", Syntax::Number);
    Phenology->add ("Vern", Syntax::Number);

    // Canopy
    Syntax* Canopy = new Syntax ();
    var->add ("Canopy", Canopy);

    Canopy->add ("Height", Syntax::Number);
    Canopy->add ("LAI", Syntax::Number);
    Canopy->add ("LADm", Syntax::Number);
    Canopy->add ("LADDist0", Syntax::Array);
    Canopy->add ("LADDist1", Syntax::Array);

    // RootSys
    Syntax* RootSys = new Syntax ();
    var->add ("RootSys", RootSys);

    RootSys->add ("Depth", Syntax::Number);
    RootSys->add ("Density", Syntax::Array);   
    RootSys->add ("H2OExtraction", Syntax::Array);
    RootSys->add ("NH4Extraction", Syntax::Array);
    RootSys->add ("NO3Extraction", Syntax::Array);

    // Prod
    Syntax* Prod = new Syntax ();
    var->add ("Prod", Prod);

    Prod->add ("WLeaf", Syntax::Number);
    Prod->add ("WStem", Syntax::Number);
    Prod->add ("WRoot", Syntax::Number);
    Prod->add ("WSOrg", Syntax::Number);
    Prod->add ("WLDrd", Syntax::Number);
    Prod->add ("NCrop", Syntax::Number);

    // CrpAux
    Syntax* CrpAux = new Syntax ();
    var->add ("CrpAux", CrpAux);

    CrpAux->add ("InitLAI", Syntax::Boolean);
    CrpAux->add ("PotRtDpt", Syntax::Number);
    CrpAux->add ("PtNCnt", Syntax::Number);
    CrpAux->add ("CrNCnt", Syntax::Number);
    CrpAux->add ("PotTransp", Syntax::Number);
    CrpAux->add ("PotCanopyAss", Syntax::Number);
    CrpAux->add ("CanopyAss", Syntax::Number);
    CrpAux->add ("IncWLeaf", Syntax::Number);
    CrpAux->add ("IncWStem", Syntax::Number);
    CrpAux->add ("IncWSOrg", Syntax::Number);
    CrpAux->add ("IncWRoot", Syntax::Number);
    CrpAux->add ("H2OUpt", Syntax::Number);
    CrpAux->add ("NH4Upt", Syntax::Number);
    CrpAux->add ("NO3Upt", Syntax::Number);
}
