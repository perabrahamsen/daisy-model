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

ColumnCanopy::ColumnCanopy (int n)
  : No (n),
    Height (n),
    PAR (n)
{ }

void
Column::tick (const Time& time)
{
  cout << "Column `" << name << "' tick\n"; 
  MainCropGrowthModel ();
  for (CropList::iterator crop = crops.begin(); crop != crops.end(); crop++)
    (*crop)->tick (time, CanStr);
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
Column::IntensityDistribution (const double Rad0, const double Ext,
			       vector <double>& Rad)
{
  double dLAI = (CanStr.LAI / CanStr.No);
    
  for (int i = 0; i < CanStr.No; i++)
    Rad[i] = Rad0 * exp (- Ext * dLAI * i);
}

void 
Column::MainCropGrowthModel ()
{
  // Fraction of Photosynthetically Active Radiation in Shortware
  // incomming radiation. 
  static const double PARinSi = 0.50;	

  double MxH = 0.0;		// Max crop Hieght in canopy [cm].
  double ACExt = 0.0;		// Average Canopy Extinction coefficient
  // (how fast the light dim as a
  //  function of LAI passed).
  double ACRef = 0.0;		// Average Canopy Reflection coefficient 
  double ARExt = 0.0;		// Average Radiation Extinction coefficient
  // (like ACExt, but for all radiation,
  //  not just light).

  // Calculate values for the total crop canopy.
  CanStr.LAI = 0.0;
  for (CropList::iterator crop = crops.begin(); crop != crops.end(); crop++)
    {
      if ((*crop)->var.Canopy.Height > MxH)
	MxH = (*crop)->var.Canopy.Height;
      CanStr.LAI += (*crop)->var.Canopy.LAI;
      ACExt += (*crop)->par.Canopy.PARext * (*crop)->var.Canopy.LAI;
      ACRef += (*crop)->par.Canopy.PARref * (*crop)->var.Canopy.LAI;
      ARExt += (*crop)->par.Canopy.EPext  * (*crop)->var.Canopy.LAI;
    }

  // If we haven't got a canopy, there is nothing more to calculate.
  if (CanStr.LAI == 0.0)
    return;

    // Calculate averages.
  ACExt /= CanStr.LAI;
  ACRef /= CanStr.LAI;
  ARExt /= CanStr.LAI;

  // CanStr.Height;	
  // CanStr.PAR;	

  // Calculate the total Leaf Area Density as a function of the height
  // above the ground.
  CSMP LAIvsH;		
  for (CropList::iterator crop = crops.begin();
       crop != crops.end();
       crop++)
    {
      (*crop)->CanopyStructure ();
      LAIvsH += (*crop)->var.Canopy.LAIvsH;
    }
  // There are no leafs below the ground.
  assert (LAIvsH (0.0) == 0.0);
  // All leafs are located below the top of the highest crop.
  assert (fabs (CanStr.LAI - LAIvsH (MxH)) < CanStr.LAI / 10000);

  CSMP HvsLAI = LAIvsH.inverse ();

  double dLAI = CanStr.LAI / CanStr.No;

  for (int i = 0; i < CanStr.No; i++)
    {
      CanStr.Height[i] = HvsLAI ((i + 1) * dLAI);
    }

  double PAR0 = (1 - ACRef) * PARinSi * bioclimate.GlobalRadiation ();
  IntensityDistribution (PAR0, ACExt, CanStr.PAR);

#if 0
  IntensityDistribution (PotentialTranspiration (bioclimate),
			 ARExt, CanStr.PTr);
#endif
}

Column::Column (string n, const Bioclimate& b, 
		const AttributeList& par, const AttributeList& var, 
		const Library&)
  : impl (*new Implementation ()),
    bioclimate (b),
    CanStr (par.integer ("NoCan")),
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
  par->add ("NoCan", Syntax::Integer);
  syntax_table->add ("column", par);

  Syntax* var = new Syntax ();
  var->add ("crops", Syntax::Crops);
  syntax_table->add ("column/state", var);
}
