// crop_impl.C

#include "crop_impl.h"
#include "value.h"
#include "syntax.h"

Crop::Parameters::Parameters (const ValueList* vl) 
    : Devel (BUG_DYNAMIC_CAST (const ValueList*, vl->lookup ("Devel"))),
      Vernal (BUG_DYNAMIC_CAST (const ValueList*, vl->lookup ("Vernal"))),
      LeafPhot (BUG_DYNAMIC_CAST (const ValueList*,
				  vl->lookup ("LeafPhot"))),
      Canopy (BUG_DYNAMIC_CAST (const ValueList*, vl->lookup ("Canopy"))),
      Root (BUG_DYNAMIC_CAST (const ValueList*, vl->lookup ("Root"))),
      Partit (BUG_DYNAMIC_CAST (const ValueList*, vl->lookup ("Partit"))),
      Resp (BUG_DYNAMIC_CAST (const ValueList*, vl->lookup ("Resp"))),
      CrpN (BUG_DYNAMIC_CAST (const ValueList*, vl->lookup ("CrpN")))
{ }

Crop::Parameters::~Parameters ()
{ }

Crop::Parameters::DevelPar::DevelPar (const ValueList* vl)
    : Model (models.lookup (vl->lookup ("Model")->name ())),
      EmrTSum (vl->lookup ("EmrTSum")->number ()),
      DS_Emr (vl->lookup ("DS_Emr")->number ()),
      DSRate1 (vl->lookup ("DSRate1")->number ()),
      DSRate2 (vl->lookup ("DSRate2")->number ()),
      TempEff1 (BUG_DYNAMIC_CAST (const ValueCSMP*, vl->lookup ("TempEff1"))),
      TempEff2 (BUG_DYNAMIC_CAST (const ValueCSMP*, vl->lookup ("TempEff2"))),
      PhotEff1 (BUG_DYNAMIC_CAST (const ValueCSMP*, vl->lookup ("PhotEff1")))
{ }

Crop::Parameters::VernalPar::VernalPar (const ValueList* vl)
    : required (vl->lookup ("required")->flag ()),
      DSLim1 (vl->lookup ("DSLim1")->number ()),
      DSLim2 (vl->lookup ("DSLim2")->number ()),
      TaLim (vl->lookup ("TaLim")->number ()),
      TaSum (vl->lookup ("TaSum")->number ())
{ }

Crop::Parameters::LeafPhotPar::LeafPhotPar (const ValueList* vl)
    : Model (models.lookup (vl->lookup ("Model")->name ())),
      Qeff (vl->lookup ("Qeff")->number ()),
      Fm (vl->lookup ("Fm")->number ()),
      TLim1 (vl->lookup ("TLim1")->number ()),
      TLim2 (vl->lookup ("TLim2")->number ())
{ }

Crop::Parameters::CanopyPar::CanopyPar (const ValueList* vl)
    : DSinit (vl->lookup ("DSinit")->number ()),
      WLfInit (vl->lookup ("WLfInit")->number ()),
      SpLAI (vl->lookup ("SpLAI")->number ()),
      HvsDS (BUG_DYNAMIC_CAST (const ValueCSMP*, vl->lookup ("HvsDS"))),
      LAIDista (vl->lookup ("LAIDista")->number ()),
      PARref (vl->lookup ("PARref")->number ()),
      PARext (vl->lookup ("PARext")->number ()),
      EPext (vl->lookup ("EPext")->number ())
{ 
    const ValueArray* a0
	= BUG_DYNAMIC_CAST (const ValueArray*, vl->lookup ("LAIDist0"));
    const ValueArray* a1
	= BUG_DYNAMIC_CAST (const ValueArray*, vl->lookup ("LAIDist1"));

    LAIDist0[0] = (*a0)[0];
    LAIDist0[1] = (*a0)[1];
    LAIDist0[2] = (*a0)[2];
    LAIDist1[0] = (*a1)[0];
    LAIDist1[1] = (*a1)[1];
    LAIDist1[2] = (*a1)[2];
}

Crop::Parameters::RootPar::RootPar (const ValueList* vl)
    : DptEmr (vl->lookup ("DptEmr")->number ()),
      PenPar1 (vl->lookup ("PenPar1")->number ()),
      PenPar2 (vl->lookup ("PenPar2")->number ()),
      MaxPen (vl->lookup ("MaxPen")->number ()),
      SpRtLength (vl->lookup ("SpRtLength")->number ()),
      DensRtTip (vl->lookup ("DensRtTip")->number ()),
      Rad (vl->lookup ("Rad")->number ()),
      h_wp (vl->lookup ("h_wp")->number ()),
      MxNH4Up (vl->lookup ("MxNH4Up")->number ()),
      MxNO3Up (vl->lookup ("MxNO3Up")->number ())
{ }

Crop::Parameters::PartitPar::PartitPar (const ValueList* vl)
    : Root (BUG_DYNAMIC_CAST (const ValueCSMP*, vl->lookup ("Root"))),
      Leaf (BUG_DYNAMIC_CAST (const ValueCSMP*, vl->lookup ("Leaf"))),
      Stem (BUG_DYNAMIC_CAST (const ValueCSMP*, vl->lookup ("Stem"))),
      LfDR (BUG_DYNAMIC_CAST (const ValueCSMP*, vl->lookup ("LfDR")))     
{ }

Crop::Parameters::RespPar::RespPar (const ValueList* vl)
    : E_Root (vl->lookup ("E_Root")->number ()),
      E_Leaf (vl->lookup ("E_Leaf")->number ()),
      E_Stem (vl->lookup ("E_Stem")->number ()),
      E_SOrg (vl->lookup ("E_SOrg")->number ()),
      r_Root (vl->lookup ("r_Root")->number ()),
      r_Leaf (vl->lookup ("r_Leaf")->number ()),
      r_Stem (vl->lookup ("r_Stem")->number ()),
      r_SOrg (vl->lookup ("r_SOrg")->number ()),
      Q10 (vl->lookup ("Q10")->number ())     
{ }

Crop::Parameters::CrpNPar::CrpNPar (const ValueList* vl)
    : SeedN (vl->lookup ("SeedN")->number ()),
      PtLeafCnc (BUG_DYNAMIC_CAST (const ValueCSMP*,
				   vl->lookup ("PtLeafCnc"))),
      CrLeafCnc (BUG_DYNAMIC_CAST (const ValueCSMP*,
				   vl->lookup ("CrLeafCnc"))),
      PtStemCnc (BUG_DYNAMIC_CAST (const ValueCSMP*,
				   vl->lookup ("PtStemCnc"))),
      CrStemCnc (BUG_DYNAMIC_CAST (const ValueCSMP*,
				   vl->lookup ("CrStemCnc"))),
      PtRootCnc (BUG_DYNAMIC_CAST (const ValueCSMP*,
				   vl->lookup ("PtRootCnc"))),
      CrRootCnc (BUG_DYNAMIC_CAST (const ValueCSMP*,
				   vl->lookup ("CrRootCnc"))),
      PtSOrgCnc (BUG_DYNAMIC_CAST (const ValueCSMP*,
				   vl->lookup ("PtSOrgCnc"))),
      CrSOrgCnc (BUG_DYNAMIC_CAST (const ValueCSMP*,
				   vl->lookup ("CrSOrgCnc")))
{ }

Crop::Variables::Variables ()
{ }

Crop::Variables::~Variables ()
{ }

dFTable<CropFun> Crop::Parameters::DevelPar::models;
dFTable<CropFun> Crop::Parameters::LeafPhotPar::models;

// Add the Crop syntax to the syntax table.
static struct CropSyntax
{
    CropSyntax ();
} crop_syntax;

CropSyntax::CropSyntax ()
{ 
    // CropPar
    Syntax* crop = new Syntax ();
    syntax_table->add ("crop", crop);
    
    // DevelPar
    Crop::Parameters::DevelPar::models.add("m1", (CropFun) 0);
    Crop::Parameters::DevelPar::models.add("m2", (CropFun) 0);
    Crop::Parameters::DevelPar::models.add("m3", (CropFun) 0);
    Syntax* Devel = new Syntax ();
    crop->add ("Devel", Devel);

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
    crop->add ("Vernal", Vernal);

    Vernal->add ("required", Syntax::Boolean);
    Vernal->add ("DSLim1", Syntax::Number);
    Vernal->add ("DSLim2", Syntax::Number);
    Vernal->add ("TaLim", Syntax::Number);
    Vernal->add ("TaSum", Syntax::Number);

    // LeafPhotPar
    Crop::Parameters::LeafPhotPar::models.add("exponential", (CropFun) 0);
    Crop::Parameters::LeafPhotPar::models.add("parabolic", (CropFun) 0);

    Syntax* LeafPhot = new Syntax ();
    crop->add ("LeafPhot", LeafPhot);

    LeafPhot->add ("Model", &Crop::Parameters::LeafPhotPar::models);
    LeafPhot->add ("Qeff", Syntax::Number);
    LeafPhot->add ("Fm", Syntax::Number);
    LeafPhot->add ("TLim1", Syntax::Number);
    LeafPhot->add ("TLim2", Syntax::Number);

    // CanopyPar
    Syntax* Canopy = new Syntax ();
    crop->add ("Canopy", Canopy);

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
    crop->add ("Root", Root);

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
    crop->add ("Partit", Partit);

    Partit->add ("Root", Syntax::CSMP);
    Partit->add ("Leaf", Syntax::CSMP);
    Partit->add ("Stem", Syntax::CSMP);
    Partit->add ("LfDR", Syntax::CSMP);

    // RespPar
    Syntax* Resp = new Syntax ();
    crop->add ("Resp", Resp);

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
    crop->add ("CrpN", CrpN);

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
