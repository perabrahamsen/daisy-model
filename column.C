// column.C

#include "column.h"
#include "crop.h"
#include "syntax.h"
#include "log.h"
#include "filter.h"
#include "library.h"
#include "bioclimate.h"
#include "crop_impl.h"
#include "alist.h"
#include "csmp.h"
#include <iostream.h>

#define exception _BUG_EXCPETION
#include <math.h>
#undef exception

struct AttributeList;

struct Column::Implementation
{ 
    Implementation ();
    ~Implementation ();
};

Column::Implementation::Implementation ()
{ }

Column::Implementation::~Implementation ()
{ }

void
Column::tick (const Time& time)
{
    cout << "Column `" << name << "' tick\n"; 
    MainCropGrowthModel ();
    for (CropList::iterator crop = crops.begin(); crop != crops.end(); crop++)
	 (*crop)->tick (time);
}

void
Column::sow (const Library& croplib, string crop, Log& log)
{
    if (croplib.check (crop))
	{
	    const AttributeList& values = croplib.lookup (crop);

	    if (syntax_table->syntax ("crop")->check (crop, values, log))
		crops.push_back (new Crop (crop, bioclimate, this, values));
	}
    else
	cerr << "Cannot sow unknow crop `" << crop << "'\n";
}

void
Column::output (Log& log, const Filter* filter) const
{
    log.open (name);
    if (filter->check ("crops"))
	output_crops (log, filter->lookup ("crops"));
    log.close ();
}

void
Column::output_crops (Log& log, const Filter* filter) const
{
    log.open ("crops");
    for (CropList::const_iterator crop = crops.begin(); 
	 crop != crops.end();
	 crop++)
	{
	    if (filter->check ((*crop)->name))
		(*crop)->output (log, filter->lookup ((*crop)->name));
	}
    log.close ();
}

double
Column::PotentialTranspiration (const Bioclimate&) const
{
    return 1.0;
}

double
Column::SoilTemperature (double /* depth */) const
{
    return bioclimate.AirTemperature ();
}

double
Column::MaxRootingDepth () const
{
    return 100.0;
}

double
Column::EvapInterception () const
{
    return -9999999.99;
}

void 
Column::SoilColumnDiscretization(long /* Nz */, double* /* z_b */,
				 double* /* z_n */, double*/* dz */) const
{ }

void 
Column::IntensityDistribution (double Rad, double Ext, long NoL,
			       double (*LAD)[30], double (*RadDist)[30])
{
    double dH = LAD[0][0];
    double LAI = LAD[1][0] * dH / 2;
    RadDist[0][0] = LAD[0][0];
    RadDist[1][0] = Rad * exp (-Ext * LAI);
    for (int i = 2; i <= NoL; i++)
	{
	    LAI += (LAD[1][i - 1] + LAD[1][i - 2]) * dH / 2;
	    RadDist[0][i - 1] = LAD[0][i - 1];
	    RadDist[1][i - 1] = Rad * exp (-Ext * LAI);
	}
}

void 
Column::MainCropGrowthModel ()
{
    static const double PARinSi = 0.50;
    static const int LeafLayerNo = 30;

    double MxH = 0.0;
    double TCLAI = 0.0;
    double TCExt = 0.0;
    double TCRef = 0.0;
    double TRExt = 0.0;

    for (CropList::iterator crop = crops.begin(); crop != crops.end(); crop++)
	{
	    if ((*crop)->var.Canopy.Height > MxH)
		MxH = (*crop)->var.Canopy.Height;
	    TCLAI += (*crop)->var.Canopy.LAI;
	    TCExt += (*crop)->par.Canopy.PARext * (*crop)->var.Canopy.LAI;
	    TCRef += (*crop)->par.Canopy.PARref * (*crop)->var.Canopy.LAI;
	    TRExt += (*crop)->par.Canopy.EPext  * (*crop)->var.Canopy.LAI;
	}
    if (TCLAI > 0)
	{
	    TCExt /= TCLAI;
	    TCRef /= TCLAI;
	    TRExt /= TCLAI;
	    CanStr.NoCan = LeafLayerNo;
	    double dH = MxH / CanStr.NoCan;
	    for (CropList::iterator crop = crops.begin();
		 crop != crops.end();
		 crop++)
		{
		    (*crop)->CanopyStructure (CanStr.NoCan, dH);
		}
	    for (int i = 0; i < CanStr.NoCan; i++)
		{
		    CanStr.CanLAD[0][i] = (i + 1) * dH;
		    CanStr.CanLAD[1][i] = 0.0;
		    for (CropList::iterator crop = crops.begin();
			 crop != crops.end();
			 crop++)
			{
			    double H = CanStr.CanLAD[0][i];
			    if ((*crop)->var.Canopy.Height
				> CanStr.CanLAD[0][i])
				{
				    CanStr.CanLAD[1][i]
					+= CSMP::find (
					    (*crop)->var.Canopy.LADDist0,
					    (*crop)->var.Canopy.LADDist1,
					    H);
				}
			}
		}
	    double PAR = (1 - TCRef) * PARinSi * bioclimate.GlobalRadiation ();
	    IntensityDistribution (PAR, TCExt, CanStr.NoCan, CanStr.CanLAD,
				   CanStr.CanPAR);
#if 0
	  PTr = PotentialTranspiration;
	  IntensityDistribution (PTr, TRExt,
				 CanStr.NoCan, CanStr.CanLAD, CanStr.CanPTr);
#endif
	}
    else
	CanStr.NoCan = 0;
}

Column::Column (string n, const Bioclimate& b, 
		const AttributeList& /*par*/, const AttributeList& var, 
		const Library&)
    : impl (*new Implementation ()),
      bioclimate (b),
      name (n)
{ 
    if (var.check ("crops"))
	{
	    crops = var.crops ("crops");
	    for (CropList::iterator crop = crops.begin(); 
		 crop != crops.end(); 
		 crop++)
		{
		    (*crop)->set_column (this);
		}
	}
}

Column::~Column ()
{ 
    for (CropList::iterator crop = crops.begin(); crop != crops.end(); crop++)
	delete *crop;
}

// Add the Column syntax to the syntax table.
static struct ColumnSyntax
{
    ColumnSyntax ();
} column_syntax;

ColumnSyntax::ColumnSyntax ()
{ 
    Syntax* par = new Syntax ();
    syntax_table->add ("column", par);

    Syntax* var = new Syntax ();
    var->add ("crops", Syntax::Crops);
    syntax_table->add ("column/state", var);
}
