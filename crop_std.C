// crop_std.C

#include "crop_impl.h"
#include "log.h"
#include "time.h"
#include "column.h"
#include "csmp.h"
#include "bioclimate.h"
#include "common.h"

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

CropStandard::CropStandard (const string n,
			    const AttributeList& pl, const AttributeList& vl)
  : Crop (n),
    par (Parameters::get (n, pl)),
    var (*new Variables (par, vl))
{ }

CropStandard::~CropStandard ()
{ 
  delete &var;
}
