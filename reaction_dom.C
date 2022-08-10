// reaction_dom.C -- DOM turnover
// 
// Copyright 2020 Per Abrahamsen and KU.
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

#include "reaction.h"
#include "geometry.h"
#include "soil.h"
#include "soil_water.h"
#include "soil_heat.h"
#include "chemistry.h"
#include "chemical.h"
#include "organic.h"
#include "log.h"
#include "assertion.h"
#include "librarian.h"
#include "frame.h"
#include "treelog.h"
#include "rate.h"
#include "abiotic.h"
#include "block_model.h"
#include "check.h"
#include "mathlib.h"
#include <sstream>

struct ReactionDOM : public Reaction
{
  // Parameters.
  const symbol DOC_name;	// Name of chemical representing DOC.
  const symbol DON_name;	// Name of chemical representing DON.
  const double turnover_rate; // [h^-1]
  const double max_N_depletion_rate; // [h^-1]
  const int where;
  const double efficiency; // []
  const double C_per_N_goal;	// [g C/g M]
  
  // Log variables.
  std::vector<double> NH4;
  std::vector<double> NO3;
  std::vector<double> CO2;
  std::vector<double> DOC;
  std::vector<double> DON;
  std::vector<double> SOC;
  std::vector<double> SON;
  std::vector<double> h_factor_;
  std::vector<double> T_factor_;
  std::vector<double> N_factor_;

  // Output.
  void output (Log& log) const;

  // Simulation.
  void tick_soil (const Geometry& geo,
                  const Soil& soil, const SoilWater& soil_water, 
                  const SoilHeat& soil_heat,
                  OrganicMatter& organic_matter, Chemistry& chemistry,
		  const double dt, Treelog& msg);

  // Create.
  bool check (const Geometry&, 
              const Soil& soil, const SoilWater& soil_water, 
	      const SoilHeat& soil_heat,
	      const OrganicMatter&, const Chemistry&, Treelog& msg) const;
  void initialize (const Geometry&, 
                   const Soil&, const SoilWater&, const SoilHeat&, 
                   const OrganicMatter&, const Surface&, Treelog&);
  explicit ReactionDOM (const BlockModel& al);
};

void
ReactionDOM::output (Log& log) const
{
  output_variable (NH4, log);
  output_variable (NO3, log);
  output_variable (CO2, log);
  output_variable (DOC, log);
  output_variable (DON, log);
  output_variable (SOC, log);
  output_variable (SON, log);
  output_value (h_factor_, "h_factor", log);
  output_value (T_factor_, "T_factor", log);
  output_value (N_factor_, "N_factor", log);
}

void 
ReactionDOM::tick_soil (const Geometry& geo,
			const Soil& soil, const SoilWater& soil_water,
			const SoilHeat& soil_heat,
			OrganicMatter& organic, 
			Chemistry& chemistry, 
			const double dt, Treelog& msg)
{
  // Code.
  const size_t cell_size = geo.cell_size ();
  const std::vector<bool> active = organic.active (); 
  Chemical& soil_NO3 = chemistry.find (Chemical::NO3 ());
  Chemical& soil_NH4 = chemistry.find (Chemical::NH4 ());
  Chemical& soil_DOC = chemistry.find (DOC_name);
  Chemical& soil_DON = chemistry.find (DON_name);

  daisy_assert (NH4.size () == cell_size);
  daisy_assert (NO3.size () == cell_size);
  daisy_assert (CO2.size () == cell_size);
  daisy_assert (DOC.size () == cell_size);
  daisy_assert (DON.size () == cell_size);
  daisy_assert (SOC.size () == cell_size);
  daisy_assert (SON.size () == cell_size);

  for (size_t i = 0; i < cell_size; i++)
    {
      // Skip inactive soil.
      if (!active[i])
	{
	  NH4[i] = NO3[i] = CO2[i] = DOC[i] = DON[i] = SOC[i] = SON[i] = 0.0;
	  continue;
	}

      // Abiotic factors.
      const double h = soil_water.h (i);	// [cm]
      const double h_factor = Abiotic::f_h (h); // []
      const double T = soil_heat.T (i);		// [dg C]
      const double T_factor = Abiotic::f_T0 (T); // []

      // Potential turnover rate, assuming unlimited N.
      const double rate_pot = turnover_rate * h_factor * T_factor; // [h^-1]
      const double DON_C = soil_DOC.M_primary (i); // [g N/cm^3]
      const double DON_N = soil_DON.M_primary (i); // [g N/cm^3]
      const double C_gen_pot = DON_C * rate_pot;   // [g C/cm^3/h]
      const double N_gen_pot =  DON_N * rate_pot;  // [g N/cm^3/h]
      const double NH4_avail			   // [g N/cm^3/h]
	= soil_NH4.M_primary (i) * max_N_depletion_rate;
      const double NO3_avail	// [g N/cm^3/h]
	= soil_NO3.M_primary (i) * max_N_depletion_rate;
      const double N_avail = NH4_avail + NO3_avail; // [g N/cm^3/h]
      daisy_assert (N_avail >= 0.0);
      const double N_need // [g N/cm^3/h]
	// = [g C/cm^3/h] * [] / [g C/g N]
	= (C_per_N_goal > 0.0)
	? std::max (0.0, (C_gen_pot * efficiency / C_per_N_goal - N_gen_pot))
	: 0.0;
      daisy_assert (N_need >= 0.0);
      const double N_factor
	= (N_need > N_avail)
	? (N_avail / N_need )
	: 1.0;
      daisy_assert (N_factor >= 0.0);
      daisy_assert (N_factor <= 1.0);

      // N limited turnover.
      const double rate = rate_pot * N_factor; // [h^-1]
      daisy_assert (rate >= 0.0);
      const double C_gen = DON_C * rate;       // [g C/cm^3/h]
      const double N_gen =  DON_N * rate;      // [g N/cm^3/h]
      const double SOC_gen = C_gen * efficiency; // [g C/cm^3/h]
      const double SON_gen =			 // [g N/cm^3/h]
	(C_per_N_goal > 0.0)
	? (SOC_gen / C_per_N_goal)
	: N_gen; 
      daisy_assert (SON_gen >= 0.0);
      const double N_consume = SON_gen - N_gen;	 // [g N/cm^3/h]
      
      // Update source/sink.
      if (N_consume > NH4_avail)
	{ 
	  NH4[i] = -NH4_avail;		 // [g N/cm^3/h]
	  NO3[i] = -N_consume + NH4_avail; // [g N/cm^3/h]
	}
      else
	{
	  NH4[i] = -N_consume;	// [g N/cm^3/h]
	  NO3[i] = 0.0;		// [g N/cm^3/h]
	}

      daisy_assert (-NH4[i] <= 1.01 * NH4_avail);
      daisy_assert (-NO3[i] <= 1.01 * NO3_avail
		    || approximate (N_consume, NH4_avail));
      daisy_approximate (-N_consume,  NH4[i] + NO3[i]);
      
      CO2[i] = C_gen - SOC_gen;	// [g C/cm^3/h]
      DOC[i] = C_gen;		// [g C/cm^3/h]
      DON[i] = N_gen;		// [g N/cm^3/h]
      SOC[i] = SOC_gen;		// [g C/cm^3/h]
      SON[i] = SON_gen;		// [g N/cm^3/h]

      daisy_approximate (NH4[i] + NO3[i] + SON[i], DON[i]);
      daisy_approximate (CO2[i] + SOC[i], DOC[i]);
      daisy_assert (SOC[i] >= 0.0);
      daisy_assert (SON[i] >= 0.0);
      daisy_assert (DOC[i] >= 0.0);
      daisy_assert (DON[i] >= 0.0);

      h_factor_[i] = h_factor;
      T_factor_[i] = T_factor;
      N_factor_[i] = N_factor;
    }
  
  // Make it official.
#if 1
  soil_NH4.add_to_transform_source (NH4);
  soil_NO3.add_to_transform_source (NO3);
  soil_DOC.add_to_transform_sink (DOC);
  soil_DON.add_to_transform_sink (DON);
  organic.add_stationary (SOC, SON, where, dt);
#endif
}

bool 
ReactionDOM::check (const Geometry&,
		    const Soil&, const SoilWater&, const SoilHeat&,
		    const OrganicMatter& organic,
		    const Chemistry& chemistry, Treelog& msg) const
{ 
  bool ok = true;
  if (!chemistry.know (Chemical::NO3 ()))
    {
      msg.error ("DOM requires NO3 to be tracked");
      ok = false;
    }
  if (!chemistry.know (Chemical::NH4 ()))
    {
      msg.error ("DOM requires NH4 to be tracked");
      ok = false;
    }
  if (!chemistry.know (DOC_name))
    {
      msg.error ("DOM requires DOC to be tracked");
      ok = false;
    }
  if (!chemistry.know (DON_name))
    {
      msg.error ("DOM requires DON to be tracked");
      ok = false;
    }
  const std::vector <SMB*>& smb = organic.get_smb ();
  if (where > smb.size ())
    {
      msg.error ("'where' too large (not enough SMB pools)");
      ok = false;
    }
  
  return ok;
}

void
ReactionDOM::initialize (const Geometry& geo,
			 const Soil&, 
			 const SoilWater&, const SoilHeat&,
			 const OrganicMatter&, const Surface&, Treelog&)
{
  const size_t cell_size = geo.cell_size ();

  NH4 = std::vector<double> (cell_size, 0.0);
  NO3 = std::vector<double> (cell_size, 0.0);
  CO2 = std::vector<double> (cell_size, 0.0);
  DOC = std::vector<double> (cell_size, 0.0);
  DON = std::vector<double> (cell_size, 0.0);
  SOC = std::vector<double> (cell_size, 0.0);
  SON = std::vector<double> (cell_size, 0.0);
  h_factor_ = std::vector<double> (cell_size, 1.0);
  T_factor_ = std::vector<double> (cell_size, 1.0);
  N_factor_ = std::vector<double> (cell_size, 1.0);
}

ReactionDOM::ReactionDOM (const BlockModel& al)
  : Reaction (al),
    DOC_name (al.name ("DOC_name")),
    DON_name (al.name ("DON_name")),
    turnover_rate (Rate::value (al, "turnover")),
    max_N_depletion_rate (Rate::value (al, "max_N_depletion")),
    where (al.integer ("where")),
    efficiency (al.number ("efficiency")),
    C_per_N_goal (al.number ("C_per_N_goal"))
{ }

static struct ReactionDOMSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ReactionDOM (al); }
  ReactionDOMSyntax ()
    : DeclareModel (Reaction::component, "DOM_turnover", "\
Turnover of dissolved organic matter.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.set_strings ("cite", "garnier2003modelling");
    frame.declare_string ("DOC_name", Attribute::Const, "\
Name of compound representing dissolved organic carbon.");
    frame.set ("DOC_name", Chemical::DOC ());
    frame.declare_string ("DON_name", Attribute::Const, "\
Name of compound representing dissolved organic nitrogen.");
    frame.set ("DON_name", Chemical::DON ());
    Rate::declare (frame, "turnover", "Turnover of DOM.\n\
Garnier et al. (2003) table 1 (T=15 degC) 1.49*10/15) = 0.99  [d^-1]");
    Rate::set_rate (frame, "turnover", 0.04125 /* [h^-1] */);
    Rate::declare (frame, "max_N_depletion", "Max depeletion of N.");
    Rate::set_rate (frame, "max_N_depletion", 0.1 /* [h^-1] */);
    frame.declare_integer ("where", Attribute::Const, "\
Target for DOM turnover.\n\
Should be a number between 0 and the number of SMB pools.\n\
0 means the SMB1, 1 means SMB2.\n\
If there are 2 SMB pools, 2 will mean soil buffer.");
    frame.set ("where", 1);
    frame.declare_fraction ("efficiency", Attribute::Const, "\
Fraction of carbon not lost as CO2 during turnover.");
  frame.set_cited ("efficiency", 0.62, "Table 1", "garnier2003modelling");
    frame.declare ("C_per_N_goal", "g C/g N", Check::non_negative (),
		   Attribute::Const, "\
Target C/N for DOM turnover.\n\
If non-negative, mineral N will be added or removed to achieve the goal.");
  frame.set_cited ("C_per_N_goal", 10.0, "Table 2", "garnier2003modelling");

    frame.declare ("NH4", "g N/cm^3/h",
		   Attribute::LogOnly, Attribute::SoilCells, 
		   "Rate of ammonium generated.");
    frame.declare ("NO3", "g N/cm^3/h",
		   Attribute::LogOnly, Attribute::SoilCells, 
		   "Rate of nitrate generated.");
    frame.declare ("CO2", "g C/cm^3/h",
		   Attribute::LogOnly, Attribute::SoilCells, 
		   "Rate of carbon dioxide generated.");
    frame.declare ("DOC", "g C/cm^3/h",
		   Attribute::LogOnly, Attribute::SoilCells, 
		   "Rate of dissolved organic C consumed.");
    frame.declare ("DON", "g N/cm^3/h",
		   Attribute::LogOnly, Attribute::SoilCells, 
		   "Rate of dissolved organic N consumed.");
    frame.declare ("SOC", "g C/cm^3/h",
		   Attribute::LogOnly, Attribute::SoilCells, 
		   "Rate of dissolved organic C immobilized.");
    frame.declare ("SON", "g N/cm^3/h",
		   Attribute::LogOnly, Attribute::SoilCells, 
		   "Rate of dissolved organic N immobilized.");
    frame.declare ("h_factor", Attribute::None (),
		   Attribute::LogOnly, Attribute::SoilCells, 
		   "Soil water potential effect on turnover rate.");
    frame.declare ("T_factor", Attribute::None (),
		   Attribute::LogOnly, Attribute::SoilCells, 
		   "Soil temperature effect on turnover rate.");
    frame.declare ("N_factor", Attribute::None (),
		   Attribute::LogOnly, Attribute::SoilCells, 
		   "Soil nitrogen effect on turnover rate.");
  }
} ReactionDOM_syntax;

// reaction_dom.C ends here.
