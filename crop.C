// crop.C

#include "crop_impl.h"
#include "log.h"
#include "time.h"
#include "column.h"
#include "csmp.h"
#include "bioclimate.h"
#define exception _BUG_EXCPETION
#include <math.h>
#undef exception 

double
Crop::SoluteUptake (string /* SoluteID */, double PotNUpt,
		    double /* I_Mx */, double /* Rad */)
{
    // const Variables::RecRootSys& RootSys = var.RootSys;

    return PotNUpt;
}

double
Crop::H2OUptake (double PotTransp, double /* RootRad */, double /* h_wp */)
{
    // const Variables::RecRootSys& RootSys = var.RootSys;

    return PotTransp;
}

void
Crop::Vernalization (double Ta)
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
Crop::Emergence ()
{
    const Parameters::DevelPar& Devel = par.Devel;
    const double EmrDpt = par.Root.DptEmr;
    double& DS = var.Phenology.DS;

    DS += column->SoilTemperature (EmrDpt) / Devel.EmrTSum;
    if (DS > 0)
	DS = Devel.DS_Emr;
}

void 
Crop::DevelopmentStage ()
{
    const Parameters::DevelPar& Devel = par.Devel;

    Devel.Model (*this);
}

double 
Crop::CropHeight ()
{
    const Parameters::CanopyPar& Canopy = par.Canopy;
    double& DS = var.Phenology.DS;

    return Canopy.HvsDS (DS);
}

void 
Crop::InitialLAI ()
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
Crop::CropLAI ()
{    
    const Parameters::CanopyPar& Canopy = par.Canopy;
    // const double DS = var.Phenology.DS;
    const double WLeaf = var.Prod.WLeaf;

    return Canopy.SpLAI * WLeaf;
}

void 
Crop::CanopyStructure (const int N, const double dH)
{
    const Parameters::CanopyPar& CanopyPar = par.Canopy;
    const double DS = var.Phenology.DS;
    Variables::RecCanopy& Canopy = var.Canopy;
    
    double z1, z2, z3, z, hz, Ar, MaxLAD;
    double CanopyH = N * dH;

    if (DS <= 1)
	{
	    z1 = CanopyPar.LAIDist0[0] +
		(CanopyPar.LAIDist1[0] - CanopyPar.LAIDist0[0]) * DS;
	    z2 = CanopyPar.LAIDist0[1] +
		(CanopyPar.LAIDist1[1] - CanopyPar.LAIDist0[1]) * DS;
	    z3 = CanopyPar.LAIDist0[2] +
		(CanopyPar.LAIDist1[2] - CanopyPar.LAIDist0[2]) * DS;
	    MaxLAD = 2 * Canopy.LAI / ((z3 + z2 - z1) * Canopy.Height);
	    Canopy.LADm = MaxLAD;
	}
    else if (DS <= 2)
	{
	    MaxLAD = Canopy.LADm;
	    z1 = CanopyPar.LAIDist1[0];
	    z2 = CanopyPar.LAIDist1[1];
	    z3 = CanopyPar.LAIDist1[2];
	    Ar = (z3 + z2 - z1) / 2;
	    z = Canopy.LAI / Canopy.Height / MaxLAD;
	    if (z < Ar)
		{
		    if (z2 - Ar + z >= z1)
			{
			    z2 += z - Ar;
			    z3 += z - Ar;
			}
		    else
			{
			    hz = sqrt (2 * z / (z3 - z2 + z1));
			    z3 = 2 * z / hz;
			    z2 = hz * z1;
			    z1 = z2;
			    MaxLAD = Canopy.LADm * hz;
			}
		}
	}
    assert (Canopy.LADDist0.size () == Canopy.LADDist1.size ());
    if (Canopy.LADDist0.size () < N)
	{
	    Canopy.LADDist0.insert (Canopy.LADDist0.end (),
				    N - Canopy.LADDist0.size (),
				    -99999.99);
	    Canopy.LADDist1.insert (Canopy.LADDist1.end (),
				    N - Canopy.LADDist1.size (),
				    -99999.99);
	}

    for (int i = 1; i <= N; i++) {
	Canopy.LADDist0[i - 1] = dH * i;
	if (Canopy.LADDist0[i - 1] < CanopyH - Canopy.Height)
	    Canopy.LADDist1[i - 1] = 0.0;
	else if (Canopy.LADDist0[i - 1]
		 < CanopyH - (1 - z1) * Canopy.Height)
	    {
		z = (Canopy.LADDist0[i - 1]
		     - CanopyH + Canopy.Height) / Canopy.Height;
		Canopy.LADDist1[i - 1] = MaxLAD * z / z1;
	    }
	else if (Canopy.LADDist0[i - 1]
		 < CanopyH - (1 - z2) * Canopy.Height)
	    Canopy.LADDist1[i - 1] = MaxLAD;
	else if (Canopy.LADDist0[i - 1]
		 < CanopyH - (1 - z3) * Canopy.Height)
	    {
		z = (Canopy.LADDist0[i - 1]
		     - CanopyH + Canopy.Height) / Canopy.Height;
		Canopy.LADDist1[i - 1]
		    = MaxLAD * (1 - (z - z2) / (z3 - z2));
	    }
	else
	    Canopy.LADDist1[i - 1] = 0.0;
    }
    Canopy.LADDist0[N - 1] = CanopyH;
    Canopy.LADDist1[N - 1] = 0.0;
    Ar = Canopy.LADDist0[0] * Canopy.LADDist1[0] / 2;
    for (int i = 2; i <= N; i++)
	Ar += Canopy.LADDist0[0]
	    * (Canopy.LADDist1[i - 2] + Canopy.LADDist1[i - 1]) / 2;
    if (Ar > 0.001)
	z = Canopy.LAI / Ar;
    else
	z = 1.0;
    for (int i = 1; i <= N; i++)
	Canopy.LADDist1[i - 1] *= z;
}

void 
Crop::RootPenetration ()
{
    const Parameters::RootPar& Root = par.Root;
    // const double DS = var.Phenology.DS;
    const double IncWRoot = var.CrpAux.IncWRoot;
    double& PotRtDpt = var.CrpAux.PotRtDpt;
    double& Depth = var.RootSys.Depth;
    
    if (IncWRoot <= 0)
	return;

    double Ts = column->SoilTemperature (Depth);
    double dp = Root.PenPar1 * max (0.0, Ts - Root.PenPar2);
    PotRtDpt = min (PotRtDpt + dp, Root.MaxPen);
    /*max depth determined by crop*/
    Depth = min (Depth + dp, Root.MaxPen);
    /*max depth determined by crop*/
    Depth = min (Depth, column->MaxRootingDepth ()); /*or by soil conditions*/
}

double 
Crop::RootDensDistPar (double a)
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
Crop::RootDensity ()
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

    int Nz;
    double z_b[81];
    double z_n[81];
    double dz[81];

    column->SoilColumnDiscretization (Nz, z_b, z_n, dz);

    RootSys.Density.reserve (20);

    int i;
    for (i = 1; z_b[i] >= RootSys.Depth; i++)
	RootSys.Density[i - 1] = L0 * exp (-a * z_n[i - 1]);
    // RootSys.Nr = i - 1;

    for (int j = i - 1; j <= 19; j++)
	RootSys.Density[j] = 0.0;
}

void 
Crop::NitContent ()
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
Crop::Transpiration (RecRootPar RootPar, double PotTransp,
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
Crop::NitrogenUptake (int Hour)
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
Crop::CanopyPhotosynthesis (const int No, const double (*RadDist)[30])
{
    const Parameters::LeafPhotPar& LeafPhot = par.LeafPhot;
    const vector<double>& LADDist0 = var.Canopy.LADDist0;
    const vector<double>& LADDist1 = var.Canopy.LADDist1;

    double Teff, F, LA;
    double Ass = 0.0;
    double Ta = bioclimate.AirTemperature ();

#if 0
    if (LeafPhot.Model != 1)
	assert (0);
#endif
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
    for (int i = 1; i <= No; i++)
	{
	    if (LADDist0[i - 1] < RadDist[0][i - 1])
		F = 0.0;
	    else
		F = LeafPhot.Fm * (1 - exp (- (LeafPhot.Qeff * RadDist[1]
					       [i - 1] / LeafPhot.Fm)));
	    if (i == 1)
		LA = LADDist0[0] * LADDist1[0] / 2;
	    else
		LA = LADDist0[0] * (LADDist1[i - 2] + LADDist1[i - 1]) / 2;
	    Ass += LA * F;
	}
    return (30.0 / 44 * Teff * Ass);
}

void 
Crop::AssimilatePartitioning (double DS, 
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
Crop::MaintenanceRespiration (double r, double Q10, double w, double T)
{
    if (w > 0)
	return (30.0 / 44 * r * exp ((T - 20) / 10 * log (Q10)) * w);
    else
	return 0.0;
}

void 
Crop::NetProduction ()
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
    T = column->SoilTemperature (Depth / 3);
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
Crop::tick (const Time& time)
{
    cout << "Crop `" << name << "' tick\n"; 

    const Column::RecCanStr& CanStr = column->CanStr;

    if (time.hour () == 0 && var.Phenology.DS <= 0)
	{
	    Emergence ();
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
    if (CanStr.CanPAR[1][0] > 0)
	{
	    double Ass = CanopyPhotosynthesis (CanStr.NoCan, CanStr.CanPAR);
	    var.CrpAux.PotCanopyAss += Ass;
	    var.CrpAux.CanopyAss += WStress * Ass;
	}
    if (time.hour () != 0)
	return;
    DevelopmentStage ();
    var.Canopy.Height = CropHeight ();
    if (var.CrpAux.InitLAI)
	InitialLAI ();
    else
	var.Canopy.LAI = CropLAI ();
    NetProduction ();
    RootPenetration ();
    RootDensity ();
}

void
Crop::output (Log& log, const Filter* filter) const
{
    log.open (name);
    var.output (log, filter);
    log.close ();
}

void 
Crop::set_column (const Column* c)
{
    column = c;
}

Crop::Crop (const string n, const Bioclimate& b, const Column* c,
	    const AttributeList& pl)
    : name (n),
      par (Parameters::get (n, pl)),
      var (*new Variables (par)),
      bioclimate (b),
      column (c)
{ }

Crop::Crop (const string n, const Bioclimate& b, const Column* c,
	    const AttributeList& pl, const AttributeList& vl)
    : name (n),
      par (Parameters::get (n, pl)),
      var (*new Variables (vl)),
      bioclimate (b),
      column (c)
{ }

Crop::~Crop ()
{ 
    delete &var;
}
