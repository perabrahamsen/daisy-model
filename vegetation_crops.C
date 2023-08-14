// vegetation_crops.C
// 
// Copyright 1996-2003 Per Abrahamsen and Søren Hansen
// Copyright 2000-2003 KVL.
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

#define BUILD_DLL
#include "vegetation.h"
#include "crop.h"
#include "organic.h"
#include "geometry.h"
#include "soil.h"
#include "plf.h"
#include "mathlib.h"
#include "harvest.h"
#include "log.h"
#include "librarian.h"
#include "metalib.h"
#include "treelog.h"
#include "frame_submodel.h"
#include "library.h"
#include <sstream>
#include <deque>

struct VegetationCrops : public Vegetation
{
  const Metalib& metalib;
  const Library& library;

  // Types.
  typedef std::deque <Crop*> CropList;
  typedef double (Crop::*CropFun) () const;

  // Crops.
  CropList crops;		// The crops themselves.

  // Harvest.
  std::vector<const Harvest*> my_harvest;

  // Nitrogen.
  double N_;                    // [kg N/ha]
  double N_fixated_;             // [kg N/ha/h]

  // Forced LAI
  class ForcedLAI
  {
    /* const */ std::vector<int> years;
    /* const */ std::vector<PLF> LAIvsDAY;

    // use.
  public:
    double operator() (int year, int yday);
    
    // Create;
    static void load_syntax (Frame&);
    ForcedLAI (const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>& als);
  } forced_LAI;

  // Canopy structure.
  double shared_light_fraction_; // Light not reserved a specific crop.
  double LAI_;			// Total LAI of all crops on this column [0-]
  double height_;		// Max crop height in canopy [cm]
  double leaf_width_;           // Leaf width in canopy [cm]
  double cover_;		// Fraction of soil covered by crops [0-1]
  PLF LAIvsH_;			// LAI below given height [f: cm -> R]
  PLF HvsLAI_;			// Height with LAI below [f: R -> cm]

  // Radiation.
  double ACExt_PAR_;		// Canopy extinction coefficient of PAR
  // (how fast the light dim as a function of LAI passed).  
  double ACRef_PAR_;		// Canopy reflection coefficient of PAR
  double ACExt_NIR_;		// Canopy extinction coefficient of NIR
  // (how fast the light dim as a function of LAI passed).  
  double ACRef_NIR_;		// Canopy reflection coefficient of NIR
  double ARExt_;		// Radiation Extinction coefficient
  // (like ACExt, but for all radiation, not just light).
  double EpFactorDry_;		// Reference to potential evapotranspiration.
  double EpFactorWet_;		// Reference to potential evapotranspiration.
  double albedo_;		// Another reflection factor.

  // Water.
  double interception_capacity_;// Canopy water storage capacity [mm]
  double shadow_stomata_conductance_;  // Canopy stomata conductance. [m/s]
  double sunlit_stomata_conductance_;  // Canopy stomata conductance. [m/s]

  // Queries.
  double shared_light_fraction () const
  { return shared_light_fraction_; }
  double rs_min () const	// Minimum transpiration resistance.
  { 
    return CanopyHarmonic (&Crop::rs_min); 
  }
  double rs_max () const	// Maximum transpiration resistance.
  { 
    return CanopyHarmonic (&Crop::rs_max) ; 
  }
  double shadow_stomata_conductance () const	// Stomata conductance [m/s]
  { return shadow_stomata_conductance_; }
  double sunlit_stomata_conductance () const	// Stomata conductance [m/s]
  { return sunlit_stomata_conductance_; }
  double N () const
  { return N_; }
  double N_fixated () const
  { return N_fixated_; }
  double LAI () const
  { return LAI_; }
  double height () const
  { return height_; }
  double leaf_width () const
  { return leaf_width_; }
  double cover () const
  { return cover_; }
  const PLF& LAIvsH () const
  { return LAIvsH_; }
  const PLF& HvsLAI () const
  { return HvsLAI_; }
  double ACExt_PAR () const
  { return ACExt_PAR_; }
  double ACRef_PAR () const
  { return ACRef_PAR_; }
  double ACExt_NIR () const
  { return ACExt_NIR_; }
  double ACRef_NIR () const
  { return ACRef_NIR_; }
  double ARExt () const
  { return ARExt_; }
  double EpFactorDry () const
  { return EpFactorDry_; }
  double EpFactorWet () const
  { return EpFactorWet_; }
  double albedo () const
  { return albedo_; }
  double interception_capacity () const
  { return interception_capacity_; }


  // Utilities.
  double CanopySum (CropFun fun) const;
  double CanopyAverage (CropFun fun) const;
  double CanopyHarmonic (CropFun fun) const;

  // Individual crop queries.
  double DS_by_name (symbol name) const;
  double stage_by_name (symbol name) const;
  double DM_by_name (symbol name, double height) const;
  double SOrg_DM_by_name (symbol name) const;
  std::string crop_names () const;

  std::vector<double> total_root_density;
  const std::vector<double>& effective_root_density () const;
  const std::vector<double>& effective_root_density (symbol crop) const;

  // Simulation.
  void reset_canopy_structure (Treelog&);
  double transpiration (double potential_transpiration,
			double canopy_evaporation,
                        const Geometry& geo,
			const Soil& soil, const SoilWater& soil_water, 
			double dt, Treelog&);
  void find_stomata_conductance (const Time& time, 
                                 const Bioclimate&, double dt, Treelog&);
  void tick (const Scope&, const Time& time, const Bioclimate&, 
             const Geometry& geo, const Soil&, const SoilHeat&,
             SoilWater&, Chemistry&, OrganicMatter&,
             double& residuals_DM,
             double& residuals_N_top, double& residuals_C_top,
             std::vector<double>& residuals_N_soil,
             std::vector<double>& residuals_C_soil,
             double dt,
             Treelog&);
  void clear ()
  {
    my_harvest.clear ();
  }
  void force_production_stress  (double pstress);
  void kill_all (symbol, const Time&, const Geometry&,
                 std::vector<AM*>& residuals, 			 
		 double& residuals_DM,
		 double& residuals_N_top, double& residuals_C_top,
		 std::vector<double>& residuals_N_soil,
		 std::vector<double>& residuals_C_soil,
		 Treelog&);
  void emerge (symbol crop_name, Treelog&);
  void harvest (symbol column_name, symbol crop_name,
		const Time&, const Geometry&,
		double stub_length,
		double stem_harvest, double leaf_harvest, double sorg_harvest,
		std::vector<const Harvest*>& harvest, double& min_height,
		double& yield_DM, double& yield_N,
		double& harvest_DM, double& harvest_N, double& harvest_C, 
                std::vector<AM*>& residuals,
		double& residuals_DM,
		double& residuals_N_top, double& residuals_C_top,
		std::vector<double>& residuals_N_soil,
		std::vector<double>& residuals_C_soil,
                const bool combine,
		Treelog&);
  void pluck (symbol column_name,
              symbol crop_name,
              const Time&, const Geometry&, 
              double stem_harvest,
              double leaf_harvest, 
              double sorg_harvest,
              std::vector<const Harvest*>& harvest,
	      double& yield_DM, double& yield_N,
              double& harvest_DM, 
              double& harvest_N, double& harvest_C,
              std::vector<AM*>& residuals,
              double& residuals_DM,
              double& residuals_N_top,
              double& residuals_C_top,
              std::vector<double>& residuals_N_soil,
              std::vector<double>& residuals_C_soil,
              Treelog&);
  void cleanup_canopy (symbol crop_name, Treelog&);
  void sow (const Scope&, const FrameModel& al, 
            const double row_width /* [cm] */,
            const double row_pos /* [cm] */,
            const double seed /* [kg w.w./ha] */,
            const Geometry&, const Soil&, OrganicMatter&, 
            double& seed_N /* kg/ha */, double& seed_C /* kg/ha */,
            const Time&, Treelog&);
  void sow (const Scope&, Crop& crop, 
            const double row_width /* [cm] */,
            const double row_pos /* [cm] */,
            const double seed /* [kg w.w./ha] */,
            const Geometry&, const Soil&, OrganicMatter&, 
            double& seed_N /* kg/ha */, double& seed_C /* kg/ha */,
            const Time&, Treelog&);
  void output (Log&) const;

  // Create and destroy.
  void initialize (const Scope&, const Time&, const Geometry& geo,
                   const Soil& soil, OrganicMatter&,
                   Treelog& msg);
  bool check (const Scope&, const Geometry&, Treelog& msg) const;
  static CropList build_crops (const BlockModel& block, const std::string& key);
  VegetationCrops (const BlockModel&);
  ~VegetationCrops ();
};

double 
VegetationCrops::ForcedLAI::operator() (int year, int yday)
{
  for (unsigned int i = 0; i < years.size (); i++)
    {
      if (years[i] == year)
	{
	  if (yday < LAIvsDAY[i].x (0))
	    return -1.0;

	  if (yday > LAIvsDAY[i].x (LAIvsDAY[i].size () - 1))
	    return -1.0;

	  return LAIvsDAY[i](yday);
	}
    }
  return -1.0;
}
    
void VegetationCrops::ForcedLAI::load_syntax (Frame& frame)
{
  frame.declare_integer ("year", Attribute::Const, "\
Year for which to use forced LAI.");
  frame.declare ("LAIvsDAY", "m^2/m^2", "yday", Attribute::Const, 
		"LAI as a function of Julian day.\n\
\n\
The simulated LAI will be used before the first day you specify and\n\
after the last specified day.  Simulated LAI will also be used\n\
whenever 'LAIvsDAY' becomes negative.");
  frame.order ("year", "LAIvsDAY");
}

VegetationCrops::ForcedLAI::ForcedLAI
/**/ (const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>& als)
{
  for (unsigned int i = 0; i < als.size (); i++)
    {
      years.push_back (als[i]->integer ("year"));
      LAIvsDAY.push_back (als[i]->plf ("LAIvsDAY"));
    }
}

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
  daisy_assert (LAI_ > 0.0);
  return CanopySum (fun) / LAI_;
}

double 
VegetationCrops::CanopyHarmonic (CropFun fun) const
{
  double value = 0.0;

  for (CropList::const_iterator crop = crops.begin();
       crop != crops.end(); 
       crop++)
    {
      value += 1.0 / (((*crop)->*fun) () * (*crop)->LAI ());
    }
  return 1.0 / (value * LAI_);
}

double 
VegetationCrops::DS_by_name (symbol name) const
{
  if (name == Vegetation::all_crops ())
    {
      double DS = -1.0;
      for (CropList::const_iterator crop = crops.begin ();
           crop != crops.end ();
           crop++)
        DS = std::max (DS, (*crop)->DS ());

      return DS;
    }

  for (CropList::const_iterator crop = crops.begin ();
       crop != crops.end ();
       crop++)
    if (library.is_derived_from ((*crop)->objid,  name))
      return (*crop)->DS ();
  return Crop::DSremove;
}

double 
VegetationCrops::stage_by_name (symbol name) const
{
  if (name == Vegetation::all_crops ())
    {
      double stage = Crop::DSremove;
      for (CropList::const_iterator crop = crops.begin ();
           crop != crops.end ();
           crop++)
        stage = std::max (stage, (*crop)->stage ());

      return stage;
    }

  for (CropList::const_iterator crop = crops.begin ();
       crop != crops.end ();
       crop++)
    if (library.is_derived_from ((*crop)->objid,  name))
      return (*crop)->stage ();
  return Crop::DSremove;
}

double 
VegetationCrops::DM_by_name (symbol name, double height) const
{
  if (name == Vegetation::all_crops ())
    {
      double sum = 0.0;

      for (CropList::const_iterator crop = crops.begin();
	   crop != crops.end();
	   crop++)
	sum += (*crop)->DM (height);

      daisy_assert (std::isfinite (sum));
      return sum;
    }
  double sum = 0.0;
  for (CropList::const_iterator crop = crops.begin();
       crop != crops.end();
       crop++)
    if (library.is_derived_from ((*crop)->objid, name))
      sum += (*crop)->DM (height);
  daisy_assert (std::isfinite (sum));
  return sum;
}

double 
VegetationCrops::SOrg_DM_by_name (symbol name) const
{
  if (name == Vegetation::all_crops ())
    {
      double sum = 0.0;

      for (CropList::const_iterator crop = crops.begin();
	   crop != crops.end();
	   crop++)
	sum += (*crop)->SOrg_DM ();

      return sum;
    }
  
  double sum = 0.0;
  for (CropList::const_iterator crop = crops.begin();
       crop != crops.end();
       crop++)
    if (library.is_derived_from ((*crop)->objid, name))
      sum += (*crop)->SOrg_DM ();
  return sum;
}

std::string
VegetationCrops::crop_names () const
{ 
  std::string result;
  for (CropList::const_iterator crop = crops.begin();
       crop != crops.end();
       crop++)
    {
      if (result != "")
	result += ",";
      result += (*crop)->objid.name ();
    }
  return result;
}

const std::vector<double>& 
VegetationCrops::effective_root_density () const
{ return total_root_density; }

const std::vector<double>& 
VegetationCrops::effective_root_density (symbol name) const
{ 
  for (CropList::const_iterator crop = crops.begin();
       crop != crops.end();
       crop++)
    if ((*crop)->objid == name)
      return (*crop)->effective_root_density ();

  static const std::vector<double> empty;
  return empty;
}

void 
VegetationCrops::find_stomata_conductance (const Time& time, 
                                           const Bioclimate& bioclimate,
                                           double dt, Treelog& msg)
{
  shadow_stomata_conductance_ = 0.0;
  sunlit_stomata_conductance_ = 0.0;

  if (LAI () < 1e-9)
    return;

  for (CropList::const_iterator crop = crops.begin();
       crop != crops.end();
       crop++)
    {
      (*crop)->find_stomata_conductance (time, bioclimate, dt, msg);
      shadow_stomata_conductance_ += (*crop)->shadow_stomata_conductance ();
      sunlit_stomata_conductance_ += (*crop)->sunlit_stomata_conductance ();
    }
}

void 
VegetationCrops::tick (const Scope& scope, 
                       const Time& time, const Bioclimate& bioclimate, 
                       const Geometry& geo, const Soil& soil,
		       const SoilHeat& soil_heat,
		       SoilWater& soil_water, Chemistry& chemistry,
		       OrganicMatter& organic_matter,
		       double& residuals_DM,
		       double& residuals_N_top, double& residuals_C_top,
		       std::vector<double>& residuals_N_soil,
		       std::vector<double>& residuals_C_soil,
		       const double dt, Treelog& msg)
{
  // Forced LAI_
  double ForcedLAI = forced_LAI (time.year (), time.yday ());
  double SimLAI = 0.0;
  if (ForcedLAI >= 0)
    {
      for (CropList::iterator crop = crops.begin(); 
	   crop != crops.end(); 
	   crop++)
	{
	  const double MyLAI = (*crop)->SimLAI ();
	  if (MyLAI > 0.0)
	    SimLAI += MyLAI;
	}
      
      if (SimLAI < 1e-10)
	ForcedLAI = -1.0;
    }
  
  // Uptake and convertion of matter.
  for (CropList::iterator crop = crops.begin(); 
       crop != crops.end(); 
       crop++)
    {
      // Relative forced LAI.
      const double MyLAI = (*crop)->SimLAI ();
      const bool use_force = (ForcedLAI >= 0.0 && MyLAI > 0.0);
      const double my_force = use_force ? (MyLAI / SimLAI) * ForcedLAI : -1.0;
      
      // Tick.
      (*crop)->tick (scope, time, bioclimate, my_force, geo, soil, soil_heat, 
                     soil_water, chemistry, organic_matter, 
                     residuals_DM, residuals_N_top, residuals_C_top,
		     residuals_N_soil, residuals_C_soil, dt, msg);
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
  // Clear roots.
  total_root_density.clear ();

  // Shared light.
  shared_light_fraction_= 1.0;
      
  // Reset vegetation state.
  N_ = 0.0;
  N_fixated_ = 0.0;
  LAI_ = 0.0;
  height_ = 0.0;
  LAIvsH_.clear ();		

  // Update vegetation state.
  for (CropList::iterator crop = crops.begin(); 
       crop != crops.end(); 
       crop++)
    {
      // Nitrogen.
      N_ += (*crop)->total_N ();
      N_fixated_ += (*crop)->N_fixated ();

      // Canopy.
      shared_light_fraction_ -= (*crop)->minimum_light_fraction ();
      const double crop_LAI = (*crop)->LAI ();
      if (crop_LAI > 0.0)
	{
	  (*crop)->CanopyStructure ();
	  LAI_ += crop_LAI;
	  if ((*crop)->height () > height_)
	    height_ = (*crop)->height ();
	  LAIvsH_ += (*crop)->LAIvsH ();
	}

      // Roots.
      const std::vector<double>& root_density = (*crop)->effective_root_density ();
      for (size_t i = 0; i < root_density.size (); i++)
	if (total_root_density.size () > i)
	  total_root_density[i] += root_density[i];
	else
	  total_root_density.push_back (root_density[i]);
    }
  
  if (shared_light_fraction_ < -1e-20)
    throw ("Sum of minumum light fraction greater than 1");

  if (LAI_ > 0.0)
    {
      // Check that we calculated LAIvsH right.
      daisy_assert (iszero (LAIvsH_ (0.0)));
      daisy_approximate (LAI_, LAIvsH_ (height_));

      // Find H as a function of LAI.
      HvsLAI_ = LAIvsH_.inverse ();
      if (!approximate (height_, HvsLAI_ (LAI_), 0.01))
	{
	  Treelog::Open nest (msg, "Vegetation reset canopy structure");
	  std::ostringstream tmp;
	  tmp << "BUG: Vegetation: height == " << height_
		 << ", LAI == " << LAI_
		 << ", HvsLAI (LAI) == " << HvsLAI_ (LAI_);
	  msg.error (tmp.str ());
	  for (CropList::iterator crop = crops.begin(); 
	       crop != crops.end(); 
	       crop++)
	    {
	      std::ostringstream tmp;
	      tmp << (*crop)->objid << " has height "
		     << (*crop)->height () << " and LAI "
		     << (*crop)->LAI ();
	      msg.error (tmp.str ());
	    }
	  LAI_ = 0.0;
	  height_ = 0.0;
	}
      else
	{
	  daisy_assert (iszero (HvsLAI_ (0.0)));
	  
	  // Other stuff
          leaf_width_ = CanopyAverage (&Crop::leaf_width);
	  cover_ =  1.0 - exp (-CanopySum (&Crop::EPext));
          daisy_assert (cover_ <= 1.0);
          daisy_assert (cover_ >= 0.0);
	  ACExt_PAR_ = CanopyAverage (&Crop::PARext);
	  ACRef_PAR_ =  CanopyAverage (&Crop::PARref);
          ACExt_NIR_ = CanopyAverage (&Crop::NIRext);
	  ACRef_NIR_ =  CanopyAverage (&Crop::NIRref);
	  ARExt_ = CanopyAverage (&Crop::EPext);
	  EpFactorDry_ = CanopyAverage (&Crop::EpFacDry);
	  EpFactorWet_ = CanopyAverage (&Crop::EpFacWet);
	  albedo_ = CanopyAverage (&Crop::albedo);
	  interception_capacity_ = CanopySum (&Crop::IntcpCap);
	  return;
	}
    }
  // No vegetation.
  HvsLAI_.clear ();
  leaf_width_ = 0.0;
  cover_ = 0.0;
  ACExt_PAR_ = 0.0;
  ACRef_PAR_ = 0.0;
  ACExt_NIR_ = 0.0;
  ACRef_NIR_ = 0.0;
  ARExt_ = 0.0;
  EpFactorDry_ = 0.0;
  EpFactorWet_ = 0.0;
  albedo_ = 0.0;
  interception_capacity_ = 0.0;
}

double
VegetationCrops::transpiration (const double potential_transpiration,
				const double canopy_evaporation,
                                const Geometry& geo,
				const Soil& soil, 
				const SoilWater& soil_water, 
                                const double dt, Treelog& msg)
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
					       geo, soil, soil_water, 
					       canopy_evaporation, 
					       dt, msg);
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
VegetationCrops::kill_all (symbol name, const Time& time, 
			   const Geometry& geo, 
			   std::vector<AM*>& residuals,
			   double& residuals_DM,
			   double& residuals_N_top, double& residuals_C_top,
			   std::vector<double>& residuals_N_soil,
			   std::vector<double>& residuals_C_soil,
			   Treelog& msg)
{
  for (CropList::iterator crop = crops.begin(); 
       crop != crops.end(); 
       crop++)
    {
      (*crop)->kill (name, time, geo, residuals, 
		     residuals_DM, residuals_N_top, residuals_C_top,
		     residuals_N_soil, residuals_C_soil, msg);
      delete *crop;
    }
  crops.erase (crops.begin (), crops.end ());
  daisy_assert (crops.size () == 0);
  reset_canopy_structure (msg);
}

void
VegetationCrops::emerge (const symbol crop_name, Treelog&)
{
  const bool all = (crop_name == Vegetation::all_crops ());

  for (CropList::iterator crop = crops.begin();
       crop != crops.end();
       crop++)
    if (all || library.is_derived_from ((*crop)->objid, crop_name))
      (*crop)->emerge ();
}


void
VegetationCrops::harvest (const symbol column_name,
			  const symbol crop_name,
			  const Time& time, 
			  const Geometry& geo, 
			  double stub_length,
			  double stem_harvest, double leaf_harvest, 
			  double sorg_harvest, 
			  std::vector<const Harvest*>& harvest,
                          double& min_height,
			  double& yield_DM, double& yield_N,
			  double& harvest_DM, 
			  double& harvest_N, double& harvest_C,
			  std::vector<AM*>& residuals,
			  double& residuals_DM, 
			  double& residuals_N_top, double& residuals_C_top,
			  std::vector<double>& residuals_N_soil,
			  std::vector<double>& residuals_C_soil,
                          const bool combine,
			  Treelog& msg)
{
  const bool all = (crop_name == Vegetation::all_crops ());

  // Harvest all crops of this type.
  for (CropList::iterator crop = crops.begin();
       crop != crops.end();
       crop++)
    if (all || library.is_derived_from ((*crop)->objid, crop_name))
      {
        const double old_crop_C = (*crop)->total_C ();
        const double old_residuals_C_top = residuals_C_top;
        const double old_residuals_C_soil
          = geo.total_surface (residuals_C_soil) * 10000;
        const double sorg_height = (*crop)->sorg_height ();
        const bool root_fruit = (sorg_height < 0.0);
        min_height = std::min (min_height, sorg_height);
	const Harvest& mine = 
	  (*crop)->harvest (column_name, time, 
			    geo, 
			    stub_length, stem_harvest,
			    leaf_harvest, sorg_harvest, 
			    root_fruit, residuals, 
			    residuals_DM, residuals_N_top, residuals_C_top,
			    residuals_N_soil, residuals_C_soil, combine, msg);
	yield_DM += mine.sorg_DM;
	yield_N += mine.sorg_N;
	harvest_DM += mine.total_DM ();
	harvest_N += mine.total_N ();
	harvest_C += mine.total_C ();

	harvest.push_back (&mine);
	my_harvest.push_back (&mine);

        const double new_crop_C = Crop::ds_remove (*crop) 
          ? 0.0 
          : (*crop)->total_C ();
        const double balance = (new_crop_C - old_crop_C)
          + ((residuals_C_top + geo.total_surface (residuals_C_soil) * 10000)
             - (old_residuals_C_top + old_residuals_C_soil)) * 10
          + mine.total_C () * 10;
        if (false && fabs (balance) > 0.001 /* 1 [g/ha] */)
          {
            std::ostringstream tmp;
            tmp << "delta Crop = " << new_crop_C - old_crop_C;
            tmp << "\ntop residuals = "
                << (residuals_C_top - old_residuals_C_top) * 10
                << "\nsoil residuals = "
                << (geo.total_surface (residuals_C_soil) * 10000
                    - old_residuals_C_soil) * 10
                << "\nharvest = " << mine.total_C () * 10
                << "\nbalance = " << balance;
	    tmp << "\nold_crop_C = " << old_crop_C
		<< "\n new_crop_C = " << new_crop_C
		<< "\n old_residuals_C_top = " << old_residuals_C_top
		<< "\n residuals_C_top = " << residuals_C_top
		<< "\n old_residuals_C_soil = " << old_residuals_C_soil
		<< "\n residuals_C_soil = " << geo.total_surface (residuals_C_soil) * 10000;
            msg.error (tmp.str ());
          }
      }

  // Remove dead crops and reset canopy structure.
  cleanup_canopy (crop_name, msg);
}

void 
VegetationCrops::pluck (symbol column_name,
                        symbol crop_name,
                        const Time& time, const Geometry& geo, 
                        double stem_harvest,
                        double leaf_harvest, 
                        double sorg_harvest,
                        std::vector<const Harvest*>& harvest,
			double& yield_DM, double& yield_N,
                        double& harvest_DM, 
                        double& harvest_N, double& harvest_C,
                        std::vector<AM*>& residuals,
                        double& residuals_DM, 
                        double& residuals_N_top, double& residuals_C_top,
                        std::vector<double>& residuals_N_soil,
                        std::vector<double>& residuals_C_soil,
                        Treelog& msg)
{
  const bool all = (crop_name == Vegetation::all_crops ());
  
  // Harvest all crops of this type.
  for (CropList::iterator crop = crops.begin();
       crop != crops.end();
       crop++)
    if (all || library.is_derived_from ((*crop)->objid, crop_name))
      {
        const double old_crop_C = (*crop)->total_C ();
        const double old_residuals_C_top = residuals_C_top;
        const double old_residuals_C_soil
          = geo.total_surface (residuals_C_soil) * 10000;
        const double old_residuals_C 
          = old_residuals_C_top + old_residuals_C_soil;
	const Harvest& mine 
          = (*crop)->pluck (column_name, time, geo, 
                            stem_harvest, leaf_harvest, sorg_harvest, 
                            residuals, 
                            residuals_DM, residuals_N_top, residuals_C_top,
                            residuals_N_soil, residuals_C_soil, msg);
	yield_DM += mine.sorg_DM;
	yield_N += mine.sorg_N;
	harvest_DM += mine.total_DM ();
	harvest_N += mine.total_N ();
	harvest_C += mine.total_C ();

	harvest.push_back (&mine);
	my_harvest.push_back (&mine);

        const double new_crop_C = Crop::ds_remove (*crop) 
          ? 0.0 
          : (*crop)->total_C ();
        const double new_residuals_C 
          = (residuals_C_top + geo.total_surface (residuals_C_soil) * 10000);
        if (!balance (old_crop_C + 10 * old_residuals_C,
                      new_crop_C + 10 * new_residuals_C,
                      -10.0 * mine.total_C ()))
          {
            std::ostringstream tmp;
            tmp << "delta Crop (" << new_crop_C - old_crop_C
                << ") != harvest (" << mine.total_C () * 10 
                << ") + delta residuals (" 
                << 10 * (new_residuals_C - old_residuals_C) << ")";
            msg.error (tmp.str ());
          }
      }

  // Remove dead crops and reset canopy structure.
  cleanup_canopy (crop_name, msg);
}

void
VegetationCrops::cleanup_canopy (const symbol crop_name, Treelog& msg)
{
  const bool all = (crop_name == Vegetation::all_crops ());

  // Remove all dead crops.  There has to be a better way.
  bool removed;
  do
    {
      removed = false;
      for (CropList::iterator crop = crops.begin();
	   crop != crops.end();
	   crop++)
	if (all || library.is_derived_from ((*crop)->objid, crop_name))
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

void
VegetationCrops::sow (const Scope& scope, const FrameModel& al,
                      const double row_width,
                      const double row_pos,
                      const double seed,
		      const Geometry& geo, const Soil& soil, 
		      OrganicMatter& organic_matter, 
                      double& seed_N, double& seed_C, const Time& time, 
                      Treelog& msg)
{
  Crop *const crop = Librarian::build_frame<Crop> (metalib, msg, al, "sow");
  if (!crop)
    {
      msg.error ("Sowing failed");
      return;
    }
  sow (scope, *crop, row_width, row_pos, seed, geo, soil, organic_matter,
       seed_N, seed_C, time, msg);
}

void
VegetationCrops::sow (const Scope& scope, Crop& crop,
                      const double row_width,
                      const double row_pos,
                      const double seed,
		      const Geometry& geo, const Soil& soil, 
		      OrganicMatter& organic_matter, 
                      double& seed_N, double& seed_C, const Time& time, 
                      Treelog& msg)
{
  const symbol name = crop.objid;
  for (CropList::iterator i = crops.begin();
       i != crops.end();
       i++)
    if ((*i)->objid == name)
      msg.error ("There is already an " + name + " on the field.\n\
If you want two " + name + " you should rename one of them");
  crop.initialize (scope, geo, soil, row_width, row_pos, seed, organic_matter,
		   time, msg);
  if (!crop.check (scope, geo, msg))
    {
      msg.error ("Sow failed");
      return;
    }
  crops.push_back (&crop);
  seed_N += crop.total_N ();
  seed_C += crop.total_C ();
  reset_canopy_structure (msg);
}

void
VegetationCrops::output (Log& log) const
{
  Vegetation::output (log);
  output_list (crops, "crops", log, Crop::component);

  static const symbol harvest_symbol ("harvest");
  static const symbol croplib (Crop::component);
  if (log.check_interior (harvest_symbol))
    {
      Log::Open open (log, harvest_symbol);
      for (const auto i : my_harvest)
        {
          const symbol crop = (*i).crop;
          if (!log.check_entry (crop, croplib))
            continue;

          Log::Shallow named (log, crop, croplib);
          (*i).output (log);
        }
    }
}

void
VegetationCrops::initialize (const Scope& scope, const Time& time,
			     const Geometry& geo, const Soil& soil, 
			     OrganicMatter& organic_matter,
                             Treelog& msg)
{
  for (unsigned int i = 0; i < crops.size (); i++)
    crops[i]->initialize (scope, geo, soil, organic_matter, time, msg);

  reset_canopy_structure (msg);
}

bool 
VegetationCrops::check (const Scope& scope, const Geometry& geo,
                        Treelog& msg) const
{
  bool ok = true;
  for (size_t i = 0; i < crops.size (); i++)
    {
      Treelog::Open nest (msg, "crop:' " + crops[i]->objid + "'");
      crops[i]->check (scope, geo, msg);
    }
  return ok;
}

VegetationCrops::CropList
VegetationCrops::build_crops (const BlockModel& al, const std::string& key)
{
  const std::vector<Crop*> v = Librarian::build_vector<Crop> (al, key);
  return CropList (v.begin (), v.end ());
}

VegetationCrops::VegetationCrops (const BlockModel& al)
  : Vegetation (al),
    metalib (al.metalib ()),
    library (metalib.library (Crop::component)),
    crops (build_crops (al, "crops")),
    N_ (0.0),
    N_fixated_ (0.0),
    // deque, so we can't use build_vector.
    forced_LAI (al.submodel_sequence ("ForcedLAI")),
    shared_light_fraction_ (1.0),
    LAI_ (0.0),
    height_ (0.0),
    leaf_width_ (0.0),
    cover_ (0.0),
    LAIvsH_ (),
    HvsLAI_ (),
    ACExt_PAR_ (0.0),
    ACRef_PAR_ (0.0),
    ACExt_NIR_ (0.0),
    ACRef_NIR_ (0.0),
    ARExt_ (0.0),
    EpFactorDry_ (0.0),
    EpFactorWet_ (0.0),
    albedo_ (0.0),
    interception_capacity_ (0.0),
    shadow_stomata_conductance_ (0.0),
    sunlit_stomata_conductance_ (0.0)
{ }

VegetationCrops::~VegetationCrops ()
{ 
  // Borland C++ don't want a const iterator here.
  for (CropList::iterator i = crops.begin (); i != crops.end (); i++)
    delete *i;
}

static struct VegetationCropsSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new VegetationCrops (al); }

  VegetationCropsSyntax ()
    : DeclareModel (Vegetation::component, "crops", "Keep track of all crops on the field.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_submodule_sequence("ForcedLAI", Attribute::Const, "\
By default, the total LAI for the vegetation will be the sum of the\n\
simulated LAI for the individual crops.  However, you can force the\n\
model to use a different values for LAI by setting this attribute.\n\
The specified LAI will be distributed among the crops on the field\n\
corresponding to their simulated LAI.\n\
\n\
'ForcedLAI' can be useful if you have measured the total LAI on the\n\
field, and want to force the model to confirm to the measurements.  \n\
\n\
'ForcedDAY' will not affect the LAI for crops that have not yet\n\
emerged.  If no crops have emerged on the field, it will be ignored.",
                                  VegetationCrops::ForcedLAI::load_syntax);
    frame.set_empty ("ForcedLAI");
    frame.declare_object ("crops", Crop::component, 
                       Attribute::State, Attribute::Variable,
                       "List of crops growing in the field");
    frame.set_empty ("crops");
    frame.declare_submodule_sequence ("harvest", Attribute::LogOnly, "\
Harvest current timestep.", Harvest::load_syntax);
  }
} VegetationCrops_syntax;

// vegetation_crops.C ends here.
