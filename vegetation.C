 // vegetation.C

#include "vegetation.h"
#include "crop.h"
#include "csmp.h"
#include "mathlib.h"
#include "harvest.h"
#include "log.h"
#include "submodel.h"
#include <deque>

struct Vegetation::Implementation
{
  // Types.
  typedef deque <Crop*> CropList;
  typedef double (Crop::*CropFun) () const;

  // Crops.
  CropList crops;		// The crops themselves.

  // Canopy structure.
  double LAI;			// Total LAI of all crops on this column [0-]
  double height;		// Max crop height in canopy [cm]
  double cover;			// Fraction of soil covered by crops [0-1]
  CSMP LAIvsH;			// LAI below given height [f: cm -> R]
  CSMP HvsLAI;			// Height with LAI below [f: R -> cm]

  // Radiation.
  double ACExt;			// Canopy extinction coefficient
  // (how fast the light dim as a function of LAI passed).  
  double ACRef;			// Canopy reflection coefficient 
  double ARExt;			// Radiation Extinction coefficient
  // (like ACExt, but for all radiation, not just light).
  double EpFactor;		// Reference to potential evapotranspiration.
  double albedo;		// Another reflection factor.

  // Water.
  double interception_capacity;	// Canopy water storage capacity [mm]

  // Utilities.
  double CanopySum (CropFun fun) const;
  double CanopyAverage (CropFun fun) const;

  // Individual crop queries.
  double DS_by_name (const string& name) const;
  double DM_by_name (const string& name) const;

  // Simulation.
  void tick (const Time& time,
	     const Bioclimate& bioclimate,
	     const Soil& soil,
	     OrganicMatter& organic_matter,
	     const SoilHeat& soil_heat,
	     const SoilWater& soil_water,
	     SoilNH4& soil_NH4,
	     SoilNO3& soil_NO3);
  void reset_canopy_structure ();
  double transpiration (double potential_transpiration,
			double canopy_evaporation,
			const Soil& soil, SoilWater& soil_water);
  void kill_all (const string&, const Time&, const Geometry&, OrganicMatter&,
		 Bioclimate&);
  vector<const Harvest*> harvest (const string& column_name,
				  const string& crop_name,
				  const Time&, const Geometry&, 
				  OrganicMatter&,
				  Bioclimate&,
				  double stub_length,
				  double stem_harvest,
				  double leaf_harvest, 
				  double sorg_harvest);
  void sow (const AttributeList& al, const Geometry&, const OrganicMatter&);
  void output (Log&) const;

  // Create and destroy.
  void initialize (const Geometry& geometry, const OrganicMatter&);
  Implementation (const AttributeList&);
  ~Implementation ();
};

double 
Vegetation::Implementation::CanopySum (CropFun fun) const
{
  double value = 0.0;

  for (CropList::const_iterator crop = crops.begin();
       crop != crops.end(); 
       crop++)
    {
      value += ((*crop)->*fun) () * (*crop)->LAI ();
    }
  return value;
}

double
Vegetation::Implementation::CanopyAverage (CropFun fun) const
{
  assert (LAI > 0.0);
  return CanopySum (fun) / LAI;
}


double 
Vegetation::Implementation::DS_by_name (const string& name) const
{
  for (CropList::const_iterator crop = crops.begin();
       crop != crops.end();
       crop++)
    if ((*crop)->name == name)
      return (*crop)->DS ();
  return Crop::DSremove;
}

double 
Vegetation::Implementation::DM_by_name (const string& name) const
{
  for (CropList::const_iterator crop = crops.begin();
       crop != crops.end();
       crop++)
    if ((*crop)->name == name)
      return (*crop)->DM ();
  return -42.42e42;
}

void 
Vegetation::Implementation::tick (const Time& time,
				  const Bioclimate& bioclimate,
				  const Soil& soil,
				  OrganicMatter& organic_matter,
				  const SoilHeat& soil_heat,
				  const SoilWater& soil_water,
				  SoilNH4& soil_NH4,
				  SoilNO3& soil_NO3)
{
  // Uptake and convertion of matter.
  for (CropList::iterator crop = crops.begin(); 
       crop != crops.end(); 
       crop++)
    {
      (*crop)->tick (time, bioclimate, soil, organic_matter, 
		     soil_heat, soil_water, soil_NH4, soil_NO3);
    }

  // Make sure the crop which took first this time will be last next.
  if (crops.size () > 1U)
    {
      crops.push_back (crops.front ());
      crops.pop_front ();
    }

  // Reset canopy structure.
  reset_canopy_structure ();
}


void 
Vegetation::Implementation::reset_canopy_structure ()
{
  // Reset vegetation state.
  LAI = 0.0;
  height = 0.0;
  LAIvsH.clear ();		

  // Update vegetation state.
  for (CropList::iterator crop = crops.begin(); 
       crop != crops.end(); 
       crop++)
    {
      const double crop_LAI = (*crop)->LAI ();
      if (crop_LAI > 0.0)
	{
	  (*crop)->CanopyStructure ();
	  LAI += crop_LAI;
	  if ((*crop)->height () > height)
	    height = (*crop)->height ();
	  LAIvsH += (*crop)->LAIvsH ();
	}
    }
  
  if (LAI > 0.0)
    {
      // Check that we calculated LAIvsH right.
      assert (LAIvsH (0.0) == 0.0);
      assert (approximate (LAI, LAIvsH (height)));

      // Find H as a function of LAI.
      HvsLAI = LAIvsH.inverse ();
      if (!approximate (height, HvsLAI (LAI), 0.01))
	{
	  CERR << "BUG: Vegetation: height == " << height
	       << ", LAI == " << LAI
	       << ", HvsLAI (LAI) == " << HvsLAI (LAI) << ".\n";
	  
	  for (CropList::iterator crop = crops.begin(); 
	       crop != crops.end(); 
	       crop++)
	    {
	      CERR << (*crop)->name << " has height "
		   << (*crop)->height () << " and LAI "
		   << (*crop)->LAI () << ".\n";
	    }
	  LAI = 0.0;
	  height = 0.0;
	}
      else
	{
	  assert (HvsLAI (0.0) == 0.0);
	  
	  // Other stuff
	  cover =  1.0 - exp (-CanopySum (&Crop::EPext));
	  ACExt = CanopyAverage (&Crop::PARext);
	  ACRef =  CanopyAverage (&Crop::PARref);
	  ARExt = CanopyAverage (&Crop::EPext);
	  EpFactor = CanopyAverage (&Crop::EpFac);
	  albedo = CanopyAverage (&Crop::albedo);
	  interception_capacity = CanopySum (&Crop::IntcpCap);
	  return;
	}
    }
  // No vegetation.
  HvsLAI.clear ();
  cover = 0.0;
  ACExt = 0.0;
  ACRef = 0.0;
  ARExt = 0.0;
  EpFactor = 0.0;
  albedo = 0.0;
  interception_capacity = 0.0;
}

double
Vegetation::Implementation::transpiration (double potential_transpiration,
					   double canopy_evaporation,
					   const Soil& soil, 
					   SoilWater& soil_water)
{
  double value = 0.0;
  
  if (LAI > 0.0)
    {
      // Distribute potential transpiration on crops.
      const double pt_per_LAI = potential_transpiration / LAI;
#if 0
      // Shouldn't we split canopy_evaporation among the different crops?
      const double ce_per_LAI = canopy_evaporation / LAI;
#endif

      for (CropList::iterator crop = crops.begin();
	   crop != crops.end();
	   crop++)
	{
	  value += (*crop)->ActualWaterUptake (pt_per_LAI * (*crop)->LAI (), 
					       soil, soil_water, 
					       canopy_evaporation);
	}
    }
  return value;
}

void
Vegetation::Implementation::kill_all (const string& name, const Time& time, 
				      const Geometry& geometry, 
				      OrganicMatter& organic_matter,
				      Bioclimate& bioclimate)
{
  for (CropList::iterator crop = crops.begin(); 
       crop != crops.end(); 
       crop++)
    {
      (*crop)->kill (name, time, geometry, organic_matter, bioclimate);
      delete *crop;
    }
  crops.erase (crops.begin (), crops.end ());
  assert (crops.size () == 0);
  reset_canopy_structure ();
}


vector<const Harvest*>
Vegetation::Implementation::harvest (const string& column_name,
				     const string& crop_name,
				     const Time& time, 
				     const Geometry& geometry, 
				     OrganicMatter& organic_matter,
				     Bioclimate& bioclimate,
				     double stub_length,
				     double stem_harvest, double leaf_harvest, 
				     double sorg_harvest)
{
  const bool all = (crop_name == "all");
  vector<const Harvest*> harvest;
  
  // Harvest all crops of this type.
  for (CropList::iterator crop = crops.begin();
       crop != crops.end();
       crop++)
    if (all || (*crop)->name == crop_name)
      harvest.push_back (&(*crop)->harvest (column_name, time, 
					    geometry, organic_matter,
					    bioclimate,
					    stub_length, stem_harvest,
					    leaf_harvest, sorg_harvest, 
					    false));

  // Remove all dead crops.  There has to be a better way.
  bool removed;
  do
    {
      removed = false;
      for (CropList::iterator crop = crops.begin();
	   crop != crops.end();
	   crop++)
	if (all || (*crop)->name == crop_name)
	  {
	    if (Crop::ds_remove (*crop))
	      {
		delete *crop;
		crops.erase (crop); // This invalidates the iterator.
		// Restart the loop.
		removed = true;
		break;
	      }
	  }
    }
  while (removed);

  // Notify the vegetation.
  reset_canopy_structure ();

  // Return the result.
  return harvest;
}

void
Vegetation::Implementation::sow (const AttributeList& al,
				 const Geometry& geometry,
				 const OrganicMatter& organic_matter)
{
  Crop& crop = Librarian<Crop>::create (al);
  crop.initialize (geometry, organic_matter);
  crops.push_back (&crop);
}

void
Vegetation::Implementation::output (Log& log) const
{
  output_list (crops, "crops", log, Librarian<Crop>::library ());
  log.output ("LAI", LAI);
  log.output ("height", height);
  log.output ("cover", cover);
  log.output ("LAIvsH", LAIvsH);
  log.output ("HvsLAI", HvsLAI);
  log.output ("ACExt", ACExt);
  log.output ("ACRef", ACRef);
  log.output ("ARExt", ARExt);
  log.output ("ARExt", ARExt);
  log.output ("EpFactor", EpFactor);
  log.output ("albedo", albedo);
  log.output ("interception_capacity", interception_capacity);
}

void
Vegetation::Implementation::initialize (const Geometry& geometry, 
					const OrganicMatter& organic_matter)
{
  for (unsigned int i = 0; i < crops.size (); i++)
    crops[i]->initialize (geometry, organic_matter);
  reset_canopy_structure ();
}

Vegetation::Implementation::Implementation (const AttributeList& al)
  : crops (),			// deque, so we can't use map_create.
    LAI (0.0),
    height (0.0),
    cover (0.0),
    LAIvsH (),
    HvsLAI (),
    ACExt (0.0),
    ACRef (0.0),
    ARExt (0.0),
    EpFactor (0.0),
    albedo (0.0),
    interception_capacity (0.0)
{
  const vector<AttributeList*>& sequence = al.alist_sequence ("crops");
  for (vector<AttributeList*>::const_iterator i = sequence.begin ();
       i != sequence.end ();
       i++)
    {
      crops.push_back (&Librarian<Crop>::create (**i));
    }
}

Vegetation::Implementation::~Implementation ()
{ 
  // Borland C++ don't want a const iterator here.
  for (CropList::iterator i = crops.begin (); i != crops.end (); i++)
    delete *i;
}
double 
Vegetation::LAI () const
{ return impl.LAI; }

double 
Vegetation::height () const
{ return impl.height; }

double 
Vegetation::cover () const
{ return impl.cover; }

const CSMP& 
Vegetation::LAIvsH () const
{ return impl.LAIvsH; }

const CSMP& 
Vegetation::HvsLAI () const
{ return impl.HvsLAI; }

double 
Vegetation::ACExt () const
{ return impl.ACExt; }

double 
Vegetation::ACRef () const
{ return impl.ACRef; }

double 
Vegetation::ARExt () const
{ return impl.ARExt; }

double 
Vegetation::EpFactor () const
{ return impl.EpFactor; }

double 
Vegetation::albedo () const
{ return impl.albedo; }

double 
Vegetation::interception_capacity () const
{ return impl.interception_capacity; }

double 
Vegetation::DS_by_name (const string& name) const
{ return impl.DS_by_name (name); }

double 
Vegetation::DM_by_name (const string& name) const
{ return impl.DM_by_name (name); }

void 
Vegetation::tick (const Time& time,
		  const Bioclimate& bioclimate,
		  const Soil& soil,
		  OrganicMatter& organic_matter,
		  const SoilHeat& soil_heat,
		  const SoilWater& soil_water,
		  SoilNH4& soil_NH4,
		  SoilNO3& soil_NO3)
{ impl.tick (time, bioclimate, soil, organic_matter, 
	     soil_heat, soil_water, soil_NH4, soil_NO3); }

double
Vegetation::transpiration (double potential_transpiration,
			   double canopy_evaporation,
			   const Soil& soil, SoilWater& soil_water)
{ return impl.transpiration (potential_transpiration, canopy_evaporation, 
			     soil, soil_water); }
void
Vegetation::kill_all (const string& name, const Time& time, 
		      const Geometry& geometry, OrganicMatter& organic_matter,
		      Bioclimate& bioclimate)
{ impl.kill_all (name, time, geometry, organic_matter, bioclimate); }

vector<const Harvest*>
Vegetation::harvest (const string& column_name,
		     const string& crop_name,
		     const Time& time, 
		     const Geometry& geometry, 
		     OrganicMatter& organic_matter,
		     Bioclimate& bioclimate,
		     double stub_length,
		     double stem_harvest, double leaf_harvest, 
		     double sorg_harvest)
{ return impl.harvest (column_name, crop_name, time, geometry, organic_matter,
		       bioclimate, stub_length, 
		       stem_harvest, leaf_harvest, sorg_harvest); }

void
Vegetation::sow (const AttributeList& al, const Geometry& geometry,
		 const OrganicMatter& organic_matter)
{ impl.sow (al, geometry, organic_matter); }

void
Vegetation::output (Log& log) const
{ impl.output (log); }

void
Vegetation::initialize (const Geometry& geometry, 
			const OrganicMatter& organic_matter)
{ impl.initialize (geometry, organic_matter); }

void
Vegetation::load_syntax (Syntax& syntax, AttributeList& alist)
{
    alist.add ("submodel", "Vegetation");
    alist.add ("description", "Keep track of all crops on the field.");
    syntax.add ("crops", Librarian<Crop>::library (), 
		Syntax::Sequence,
		"List of crops growing in the field");
    alist.add ("crops", *new vector<AttributeList*>);
    syntax.add ("LAI", "m^2/m^2", Syntax::LogOnly,
		"Total LAI of all crops on this column");
    syntax.add ("height", "cm", Syntax::LogOnly,
		"Max crop height in canopy");
    syntax.add ("cover", "m^2/m^2", Syntax::LogOnly,
		"Fraction of soil covered by crops");
    syntax.add ("LAIvsH", Syntax::CSMP, Syntax::LogOnly,
		"Total canopy LAI below given height (cm)");
    syntax.add ("HvsLAI", Syntax::CSMP, Syntax::LogOnly, "\
Height in which there is a given LAI below in total canopy");
    syntax.add ("ACExt", Syntax::None (), Syntax::LogOnly,
		"Canopy extinction coefficient \
\(how fast the light dim as a function of LAI passed)");
    syntax.add ("ACRef", Syntax::None (), Syntax::LogOnly,
		"Canopy reflection coefficient");
    syntax.add ("ARExt", Syntax::None (), Syntax::LogOnly,
		"Radiation Extinction coefficient \
\(like ACExt, but for all radiation, not just light)");
    syntax.add ("EpFactor", Syntax::None (), Syntax::LogOnly,
		"Reference to potential evapotranspiration");
    syntax.add ("albedo", Syntax::None (), Syntax::LogOnly,
		"Another reflection factor");
    syntax.add ("interception_capacity", "mm", Syntax::LogOnly,
		"Canopy water storage capacity");
}


Vegetation::Vegetation (const AttributeList& al)
  : impl (*new Implementation (al))
{ }

Vegetation::~Vegetation ()
{ delete &impl; }

static Submodel::Register vegetation_submodel ("Vegetation", 
					       Vegetation::load_syntax);
