// vegetation_crops.C

#include "vegetation.h"
#include "crop.h"
#include "organic_matter.h"
#include "soil.h"
#include "plf.h"
#include "mathlib.h"
#include "harvest.h"
#include "log.h"
#include "tmpstream.h"
#include <deque>

struct VegetationCrops : public Vegetation
{
  // Types.
  typedef deque <Crop*> CropList;
  typedef double (Crop::*CropFun) () const;

  // Crops.
  CropList crops;		// The crops themselves.

  // Canopy structure.
  double LAI_;			// Total LAI of all crops on this column [0-]
  double height_;		// Max crop height in canopy [cm]
  double cover_;		// Fraction of soil covered by crops [0-1]
  PLF LAIvsH_;			// LAI below given height [f: cm -> R]
  PLF HvsLAI_;			// Height with LAI below [f: R -> cm]

  // Radiation.
  double ACExt_;		// Canopy extinction coefficient
  // (how fast the light dim as a function of LAI passed).  
  double ACRef_;		// Canopy reflection coefficient 
  double ARExt_;		// Radiation Extinction coefficient
  // (like ACExt, but for all radiation, not just light).
  double EpFactor_;		// Reference to potential evapotranspiration.
  double albedo_;		// Another reflection factor.

  // Water.
  double interception_capacity_;// Canopy water storage capacity [mm]

  // Queries.
  double LAI () const
  { return LAI_; }
  double height () const
  { return height_; }
  double cover () const
  { return cover_; }
  const PLF& LAIvsH () const
  { return LAIvsH_; }
  const PLF& HvsLAI () const
  { return HvsLAI_; }
  double ACExt () const
  { return ACExt_; }
  double ACRef () const
  { return ACRef_; }
  double ARExt () const
  { return ARExt_; }
  double EpFactor () const
  { return EpFactor_; }
  double albedo () const
  { return albedo_; }
  double interception_capacity () const
  { return interception_capacity_; }


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
	     SoilNH4& soil_NH4, SoilNO3& soil_NO3, 
	     double& residuals_DM,
	     double& residuals_N_top, double& residuals_C_top,
	     vector<double>& residuals_N_soil,
	     vector<double>& residuals_C_soil,
	     Treelog&);
  void tick (const Time& time,
	     const Bioclimate& bioclimate,
	     const Soil& soil,
	     const SoilHeat& soil_heat,
	     const SoilWater& soil_water, 
	     double& residuals_DM,
	     double& residuals_N_top, double& residuals_C_top,
	     vector<double>& residuals_N_soil,
	     vector<double>& residuals_C_soil,
	     Treelog&);
  void reset_canopy_structure (Treelog&);
  double transpiration (double potential_transpiration,
			double canopy_evaporation,
			const Soil& soil, SoilWater& soil_water, Treelog&);
  void force_production_stress  (double pstress);
  void kill_all (const string&, const Time&, const Geometry&, 
		 Bioclimate&, vector<AM*>& residuals, 			 
		 double& residuals_DM,
		 double& residuals_N_top, double& residuals_C_top,
		 vector<double>& residuals_N_soil,
		 vector<double>& residuals_C_soil,
		 Treelog&);
  void harvest (const string& column_name, const string& crop_name,
		const Time&, const Geometry&, Bioclimate&,
		double stub_length,
		double stem_harvest, double leaf_harvest, double sorg_harvest,
		vector<const Harvest*>& harvest, vector<AM*>& residuals,
		double& harvest_DM, double& harvest_N, double& harvest_C, 
		double& residuals_DM,
		double& residuals_N_top, double& residuals_C_top,
		vector<double>& residuals_N_soil,
		vector<double>& residuals_C_soil,
		Treelog&);
  double sow (Treelog& msg, 
	      const AttributeList& al, const Geometry&, OrganicMatter&);
  void sow (Treelog& msg, const AttributeList& al, const Geometry&);
  void output (Log&) const;

  // Create and destroy.
  void initialize (Treelog& msg, const Soil& soil, OrganicMatter&);
  VegetationCrops (const AttributeList&);
  ~VegetationCrops ();
};

double 
VegetationCrops::CanopySum (CropFun fun) const
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
VegetationCrops::CanopyAverage (CropFun fun) const
{
  assert (LAI_ > 0.0);
  return CanopySum (fun) / LAI_;
}


double 
VegetationCrops::DS_by_name (const string& name) const
{
  for (CropList::const_iterator crop = crops.begin();
       crop != crops.end();
       crop++)
    if ((*crop)->name == name)
      return (*crop)->DS ();
  return Crop::DSremove;
}

double 
VegetationCrops::DM_by_name (const string& name) const
{
  for (CropList::const_iterator crop = crops.begin();
       crop != crops.end();
       crop++)
    if ((*crop)->name == name)
      return (*crop)->DM ();
  return 0.0;
}

void 
VegetationCrops::tick (const Time& time,
		       const Bioclimate& bioclimate,
		       const Soil& soil,
		       OrganicMatter& organic_matter,
		       const SoilHeat& soil_heat,
		       const SoilWater& soil_water,
		       SoilNH4& soil_NH4,
		       SoilNO3& soil_NO3,
		       double& residuals_DM,
		       double& residuals_N_top, double& residuals_C_top,
		       vector<double>& residuals_N_soil,
		       vector<double>& residuals_C_soil,
		       Treelog& msg)
{
  // Uptake and convertion of matter.
  for (CropList::iterator crop = crops.begin(); 
       crop != crops.end(); 
       crop++)
    {
      (*crop)->tick (time, bioclimate, soil, &organic_matter, 
		     soil_heat, soil_water, &soil_NH4, &soil_NO3, 
		     residuals_DM, residuals_N_top, residuals_C_top,
		     residuals_N_soil, residuals_C_soil, msg);
    }

  // Make sure the crop which took first this time will be last next.
  if (crops.size () > 1U)
    {
      crops.push_back (crops.front ());
      crops.pop_front ();
    }

  // Reset canopy structure.
  reset_canopy_structure (msg);
}

void 
VegetationCrops::tick (const Time& time,
		       const Bioclimate& bioclimate,
		       const Soil& soil,
		       const SoilHeat& soil_heat,
		       const SoilWater& soil_water,
		       double& residuals_DM,
		       double& residuals_N_top, double& residuals_C_top,
		       vector<double>& residuals_N_soil,
		       vector<double>& residuals_C_soil,
		       Treelog& msg)
{
  // Uptake and convertion of matter.
  for (CropList::iterator crop = crops.begin(); 
       crop != crops.end(); 
       crop++)
    {
      (*crop)->tick (time, bioclimate, soil, NULL, 
		     soil_heat, soil_water, NULL, NULL, 
		     residuals_DM, residuals_N_top, residuals_C_top,
		     residuals_N_soil, residuals_C_soil, msg);
    }

  // Make sure the crop which took first this time will be last next.
  if (crops.size () > 1U)
    {
      crops.push_back (crops.front ());
      crops.pop_front ();
    }

  // Reset canopy structure.
  reset_canopy_structure (msg);
}

void 
VegetationCrops::reset_canopy_structure (Treelog& msg)
{
  // Reset vegetation state.
  LAI_ = 0.0;
  height_ = 0.0;
  LAIvsH_.clear ();		

  // Update vegetation state.
  for (CropList::iterator crop = crops.begin(); 
       crop != crops.end(); 
       crop++)
    {
      const double crop_LAI = (*crop)->LAI ();
      if (crop_LAI > 0.0)
	{
	  (*crop)->CanopyStructure ();
	  LAI_ += crop_LAI;
	  if ((*crop)->height () > height_)
	    height_ = (*crop)->height ();
	  LAIvsH_ += (*crop)->LAIvsH ();
	}
    }
  
  if (LAI_ > 0.0)
    {
      // Check that we calculated LAIvsH right.
      assert (LAIvsH_ (0.0) == 0.0);
      assert (approximate (LAI_, LAIvsH_ (height_)));

      // Find H as a function of LAI.
      HvsLAI_ = LAIvsH_.inverse ();
      if (!approximate (height_, HvsLAI_ (LAI_), 0.01))
	{
	  Treelog::Open nest (msg, "Vegetation reset canopy structure");
	  TmpStream tmp;
	  tmp () << "BUG: Vegetation: height == " << height_
		 << ", LAI == " << LAI_
		 << ", HvsLAI (LAI) == " << HvsLAI_ (LAI_);
	  msg.error (tmp.str ());
	  for (CropList::iterator crop = crops.begin(); 
	       crop != crops.end(); 
	       crop++)
	    {
	      TmpStream tmp;
	      tmp () << (*crop)->name << " has height "
		     << (*crop)->height () << " and LAI "
		     << (*crop)->LAI ();
	      msg.error (tmp.str ());
	    }
	  LAI_ = 0.0;
	  height_ = 0.0;
	}
      else
	{
	  assert (HvsLAI_ (0.0) == 0.0);
	  
	  // Other stuff
	  cover_ =  1.0 - exp (-CanopySum (&Crop::EPext));
	  ACExt_ = CanopyAverage (&Crop::PARext);
	  ACRef_ =  CanopyAverage (&Crop::PARref);
	  ARExt_ = CanopyAverage (&Crop::EPext);
	  EpFactor_ = CanopyAverage (&Crop::EpFac);
	  albedo_ = CanopyAverage (&Crop::albedo);
	  interception_capacity_ = CanopySum (&Crop::IntcpCap);
	  return;
	}
    }
  // No vegetation.
  HvsLAI_.clear ();
  cover_ = 0.0;
  ACExt_ = 0.0;
  ACRef_ = 0.0;
  ARExt_ = 0.0;
  EpFactor_ = 0.0;
  albedo_ = 0.0;
  interception_capacity_ = 0.0;
}

double
VegetationCrops::transpiration (double potential_transpiration,
				double canopy_evaporation,
				const Soil& soil, 
				SoilWater& soil_water, Treelog& msg)
{
  double value = 0.0;
  
  if (LAI_ > 0.0)
    {
      // Distribute potential transpiration on crops.
      const double pt_per_LAI = potential_transpiration / LAI_;
#if 0
      // Shouldn't we split canopy_evaporation among the different crops?
      const double ce_per_LAI = canopy_evaporation / LAI_;
#endif

      for (CropList::iterator crop = crops.begin();
	   crop != crops.end();
	   crop++)
	{
	  value += (*crop)->ActualWaterUptake (pt_per_LAI * (*crop)->LAI (), 
					       soil, soil_water, 
					       canopy_evaporation, msg);
	}
    }
  return value;
}

void
VegetationCrops::force_production_stress (double pstress)
{
  for (CropList::iterator crop = crops.begin(); 
       crop != crops.end(); 
       crop++)
    {
      (*crop)->force_production_stress (pstress);
    }
}

void
VegetationCrops::kill_all (const string& name, const Time& time, 
			   const Geometry& geometry, 
			   Bioclimate& bioclimate, vector<AM*>& residuals,
			   double& residuals_DM,
			   double& residuals_N_top, double& residuals_C_top,
			   vector<double>& residuals_N_soil,
			   vector<double>& residuals_C_soil,
			   Treelog& msg)
{
  for (CropList::iterator crop = crops.begin(); 
       crop != crops.end(); 
       crop++)
    {
      (*crop)->kill (name, time, geometry, bioclimate, residuals, 
		     residuals_DM, residuals_N_top, residuals_C_top,
		     residuals_N_soil, residuals_C_soil, msg);
      delete *crop;
    }
  crops.erase (crops.begin (), crops.end ());
  assert (crops.size () == 0);
  reset_canopy_structure (msg);
}

void
VegetationCrops::harvest (const string& column_name,
			  const string& crop_name,
			  const Time& time, 
			  const Geometry& geometry, 
			  Bioclimate& bioclimate,
			  double stub_length,
			  double stem_harvest, double leaf_harvest, 
			  double sorg_harvest, 
			  vector<const Harvest*>& harvest,
			  vector<AM*>& residuals,
			  double& harvest_DM, 
			  double& harvest_N, double& harvest_C,
			  double& residuals_DM, 
			  double& residuals_N_top, double& residuals_C_top,
			  vector<double>& residuals_N_soil,
			  vector<double>& residuals_C_soil,
			  Treelog& msg)
{
  const bool all = (crop_name == "all");

  // Harvest all crops of this type.
  for (CropList::iterator crop = crops.begin();
       crop != crops.end();
       crop++)
    if (all || (*crop)->name == crop_name)
      {
	const Harvest& mine = 
	  (*crop)->harvest (column_name, time, 
			    geometry, 
			    bioclimate,
			    stub_length, stem_harvest,
			    leaf_harvest, sorg_harvest, 
			    false, residuals, 
			    residuals_DM, residuals_N_top, residuals_C_top,
			    residuals_N_soil, residuals_C_soil, msg);

	harvest_DM += mine.total_DM ();
	harvest_N += mine.total_N ();
	harvest_C += mine.total_C ();

	harvest.push_back (&mine);
      }

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
  reset_canopy_structure (msg);
}

double
VegetationCrops::sow (Treelog& msg, const AttributeList& al,
		      const Geometry& geometry,
		      OrganicMatter& organic_matter)
{
  Crop& crop = Librarian<Crop>::create (al);
  crop.initialize (msg, geometry, organic_matter);
  crops.push_back (&crop);
  return crop.total_N ();
}

void
VegetationCrops::sow (Treelog& msg, const AttributeList& al,
		      const Geometry& geometry)
{
  Crop& crop = Librarian<Crop>::create (al);
  crop.initialize (msg, geometry);
  crops.push_back (&crop);
}

void
VegetationCrops::output (Log& log) const
{
  Vegetation::output (log);
  output_list (crops, "crops", log, Librarian<Crop>::library ());
}

void
VegetationCrops::initialize (Treelog& msg, const Soil& soil, 
			     OrganicMatter& organic_matter)
{
  for (unsigned int i = 0; i < crops.size (); i++)
    crops[i]->initialize (msg, soil, organic_matter);
  reset_canopy_structure (msg);
}

VegetationCrops::VegetationCrops (const AttributeList& al)
  : Vegetation (al),
    crops (),			// deque, so we can't use map_create.
    LAI_ (0.0),
    height_ (0.0),
    cover_ (0.0),
    LAIvsH_ (),
    HvsLAI_ (),
    ACExt_ (0.0),
    ACRef_ (0.0),
    ARExt_ (0.0),
    EpFactor_ (0.0),
    albedo_ (0.0),
    interception_capacity_ (0.0)
{
  const vector<AttributeList*>& sequence = al.alist_sequence ("crops");
  for (vector<AttributeList*>::const_iterator i = sequence.begin ();
       i != sequence.end ();
       i++)
    {
      crops.push_back (&Librarian<Crop>::create (**i));
    }
}

VegetationCrops::~VegetationCrops ()
{ 
  // Borland C++ don't want a const iterator here.
  for (CropList::iterator i = crops.begin (); i != crops.end (); i++)
    delete *i;
}

static struct
VegetationCropsSyntax
{
  static Vegetation& make (const AttributeList& al)
  { return *new VegetationCrops (al); }

  VegetationCropsSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    Vegetation::load_syntax (syntax, alist);
    alist.add ("description", "Keep track of all crops on the field.");
    syntax.add ("crops", Librarian<Crop>::library (), 
		Syntax::Sequence,
		"List of crops growing in the field");
    alist.add ("crops", vector<AttributeList*> ());
    Librarian<Vegetation>::add_type ("crops", alist, syntax, &make);
  }
} VegetationCrops_syntax;
