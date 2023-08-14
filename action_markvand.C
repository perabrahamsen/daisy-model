// action_markvand.C  -- Emulate part of the MARKVAND irrigation system.
// 
// Copyright 2005 Per Abrahamsen and KVL.
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
#include "action.h"
#include "block_model.h"
#include "daisy.h"
#include "field.h"
#include "crop.h"
#include "im.h"
#include "fao.h"
#include "log.h"
#include "mathlib.h"
#include "check.h"
#include "vcheck.h"
#include "assertion.h"
#include "memutils.h"
#include "librarian.h"
#include "vegetation.h"
#include "treelog.h"
#include "submodeler.h"
#include "frame_submodel.h"
#include "units.h"
#include <vector>
#include <memory>
#include <sstream>
#include <map>

// MV_Soil

struct MV_Soil : public Model
{
  // Content.
  static const char *const description;
  static const char *const component;
  symbol library_id () const;

  // Parameters.
  const double z_o;		// Depth of top soil. [mm]
  const double z_xJ;		// Max rooting depth. [mm]
  const double Theta_fo;	// Field capacity, topsoil. []
  const double Theta_wo;	// Wielding point, topsoil. []
  const double Theta_Fo;	// Holding capaicty, topsoil. []
  const double Theta_fu;	// Field capacity, subsoil. []
  const double Theta_wu;	// Wielting point, subsoil. []
  const double Theta_Fu;	// Holding capaicty, subsoil. []
  const double max_capacity;	// Holding capacity of max root zone. [mm]
  const double C_e;		// Capacity of evaporation reservoir. [mm]
  const double c_e;		// Basic evaporation factor.
  const double c_T;		// Transpiration constant. [mm]
  const double k_qr;		// Drainage constant root zone.
  const double k_qb;		// Drainage constant subsone.

  // Root zone capacity.
  double C_r (const double z_r) const // [mm]
  {
    if (z_r < z_o)
      return std::max (C_e, Theta_Fo * z_r);
    return std::max (C_e, Theta_Fo * z_o - Theta_Fu * (z_r - z_o));
  }
  // Subzone capacity.
  double C_b (const double z_r) const // [mm]
  { return max_capacity - C_r (z_r); }

  // Create and Destroy.
  MV_Soil (const BlockModel& al)
    : z_o (al.number ("z_o")),
      z_xJ (al.number ("z_xJ")),
      Theta_fo (al.number ("Theta_fo")),
      Theta_wo (al.number ("Theta_wo")),
      Theta_Fo (Theta_fo - Theta_wo),
      Theta_fu (al.number ("Theta_fu")),
      Theta_wu (al.number ("Theta_wu")),
      Theta_Fu (Theta_fu - Theta_wu),
      max_capacity (Theta_Fo * z_o + Theta_Fu * (z_xJ - z_o)),
      C_e (al.number ("C_e")),
      c_e (al.number ("c_e")),
      c_T (al.number ("c_T")),
      k_qr (al.number ("k_qr")),
      k_qb (al.number ("k_qb"))
  { }
  ~MV_Soil ()
  { }
};

static struct MV_SoilInit : public DeclareComponent
{
  MV_SoilInit () 
    : DeclareComponent (MV_Soil::component, MV_Soil::description)
  { }
} Soil_init;


const char *const MV_Soil::description = "\
Description of a soil for use by the MARKVAND model.";

const char *const MV_Soil::component = "MV_Soil";

symbol
MV_Soil::library_id () const
{
  static const symbol id (component);
  return id;
}

static struct MV_SoilSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new MV_Soil (al); }
  MV_SoilSyntax ()
    : DeclareModel (MV_Soil::component, "default", 
                    "Standard MARKVAND soil model.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare ("z_o", "mm", Check::positive (), Attribute::Const,
                "Depth of top soil.");
    frame.declare ("z_xJ", "mm", Check::positive (), Attribute::Const,
                "Max rooting depth.");
    frame.declare ("Theta_fo", Attribute::Fraction (), Check::positive (), 
                Attribute::Const, "Field capacity, topsoil.");
    frame.declare ("Theta_wo", Attribute::Fraction (), Check::positive (), 
                Attribute::Const,
                "Wilting point, topsoil.");
    frame.declare ("Theta_fu", Attribute::Fraction (), Check::positive (), 
                Attribute::Const,
                "Field capacity, subsoil.");
    frame.declare ("Theta_wu", Attribute::Fraction (), Check::positive (), 
                Attribute::Const,
                "Wilting point, subsoil.");
    frame.declare ("C_e", "mm", Check::non_negative (), Attribute::Const,
                "Capacity of evaporation reservoir.");
    frame.declare ("c_e", Attribute::Fraction (), Check::non_negative (), 
                Attribute::Const,
                "Basic evaporation factor.");
    frame.declare ("c_T", "mm", Attribute::Const,
                "Transpiration constant.");
    frame.declare ("k_qr", Attribute::None (), Attribute::Const,
                "Drainage constant root zone.");
    frame.declare ("k_qb", Attribute::None (), Attribute::Const,
                "Drainage constant subsone.");
  }
} MV_Soil_syntax;

// MV_Crop

struct MV_Crop : public Model
{
  // Content.
  const symbol name;
  static const char *const description;
  static const char *const component;
  symbol library_id () const;

  // Parameters.
  const std::vector<double> S_F; // Temperature sum for each phase.
  const std::vector<double> A_F; // Deficit trigger for each phase.
  const double L_gv; // Green leaf area index at emergence / growth start.
  const double L_ge; // Green leaf area index at the time
                     // where growth rate become exponential.
  const double L_gx;		// Maximum green leaf area index.
  const double L_gm;		// Green leaf area index at maturity.
  const double L_ym;		// Yellow leaf area index at maturity.
  const double S_Le; // Temperature sum when green LAI growth turn exponential.
  const double S_Lx;		// Temperature sum maximum green LAI.
  const double S_Lr;	// Temperature sum for start of yellow leaves.
  const double S_Lm;		// Temperature sum at maturity.
  const double z_0;	// Root depth before emergence (growth start).
  const double z_v;	    // Root depth at emergence (growth start).
  const double z_xA;		// Maximum root depth for this crop.
  const double z_m;		// Root depth at maturity.
  const double c_r;		// Root penetration rate.
  
  // Phases.
  static std::vector<double> accumulated (const std::vector<double>& numbers)
  {
    std::vector<double> result;
    double sum = 0.0;
    for (size_t i = 0; i < numbers.size (); i++)
      {
	sum += numbers[i];
	result.push_back (sum);
      }
    return result;
  }
  size_t phase (const double T_sum) const
  { 
    size_t i = 0; 
    while (i < S_F.size () && S_F[i] < T_sum)
      i++;
    return i;
  }
  double A (const double T_sum) const
  {
    const size_t i = phase (T_sum);
    return (A_F.size () > i) ? A_F[i] : 1.0;
  }

  // Simulation.
  double L_g (const double T_sum) const
  { 
    if (T_sum < 0)
      return L_gv;
    if (T_sum < S_Le)
      return L_gv + (L_ge - L_gv) * T_sum / S_Le;
    if (T_sum < S_Lx)
      return L_ge + (L_gx - L_ge) 
	* ((exp (2.4 * (T_sum - S_Le) / (S_Lx - S_Le)) - 1.0) / 10.0);
    if (T_sum < S_Lr)
      return L_gx;
    if (T_sum < S_Lm)
      return L_gx - (L_gx - L_gm) * (T_sum - S_Lr) / (S_Lm - S_Lr);
    return L_gm;
  }
  double L_y (const double T_sum) const
  {
    if (T_sum < S_Lr)
      return 0.0;
    if (T_sum < S_Lm)
      return L_ym * (T_sum - S_Lr) / (S_Lm - S_Lr);
    return L_ym;
  }
  double z_r (const double z_x, const double T_sum, const double dt) const
  {
    if (T_sum < 0.0)
      return z_0;
    if (T_sum < S_Lm)
      return std::min (z_x, std::max (z_v, c_r * dt));
    return z_m;
  }

  // Create and Destroy.
  MV_Crop (const BlockModel& al)
    : name (al.type_name ()),
      S_F (accumulated (al.number_sequence ("S_F"))),
      A_F (al.number_sequence ("A_F")),
      L_gv (al.number ("L_gv")),
      L_ge (al.number ("L_ge")),
      L_gx (al.number ("L_gx")),
      L_gm (al.number ("L_gm")),
      L_ym (al.number ("L_ym")),
      S_Le (al.number ("S_Le")),
      S_Lx (al.number ("S_Lx")),
      S_Lr (al.number ("S_Lr")),
      S_Lm (al.number ("S_Lm")),
      z_0 (al.number ("z_0")),
      z_v (al.number ("z_v")),
      z_xA (al.number ("z_xA")),
      z_m (al.number ("z_m")),
      c_r (al.number ("c_r"))
  { }
  ~MV_Crop ()
  { }
};

static struct MV_CropInit : public DeclareComponent
{
  MV_CropInit () 
    : DeclareComponent (MV_Crop::component, MV_Crop::description)
  { }
} Crop_init;

const char *const MV_Crop::description = "\
Description of a crop for use by the MARKVAND model.";

const char *const MV_Crop::component = "MV_Crop";

symbol
MV_Crop::library_id () const
{
  static const symbol id (component);
  return id;
}

static struct MV_CropSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new MV_Crop (al); }

  static bool check_alist (const Metalib&, const Frame& al, Treelog& msg)
  {
    bool ok = true;
    if (al.number_sequence ("S_F").size ()
        != al.number_sequence ("A_F").size ())
      {
        msg.error ("'S_F' and 'A_F' should be the same length");
        ok = false;
      }
    return ok;
  }

  MV_CropSyntax ()
    : DeclareModel (MV_Crop::component, "default", "\
Standard MARKVAND crop model.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.add_check (check_alist);
    frame.declare ("S_F", "dg C d", Check::non_negative (), 
                   Attribute::Const, Attribute::Variable,
                   "Temperature sum for each phase.");
    frame.set_check ("S_F", VCheck::min_size_1 ());
    frame.declare ("A_F", Attribute::Fraction (), Attribute::Const, Attribute::Variable,
                "Allowable water deficit for each phase before irrigation.");
    frame.set_check ("A_F", VCheck::min_size_1 ());
    frame.declare ("L_gv", Attribute::None (), Check::non_negative (), Attribute::Const,
                "Green leaf area index at emergence / growth start.");
    frame.declare ("L_ge", Attribute::None (), Check::non_negative (),
                Attribute::Const, "\
Green leaf area index at the time where growth rate become exponential.");
    frame.declare ("L_gx", Attribute::None (), Check::non_negative (), Attribute::Const,
                "Maximum green leaf area index.");
    frame.declare ("L_gm", Attribute::None (), Check::non_negative (), Attribute::Const,
                "Green leaf area index at maturity.");
    frame.declare ("L_ym", Attribute::None (), Check::non_negative (), Attribute::Const,
                "Yellow leaf area index at maturity.");
    frame.declare ("S_Le", "dg C d", Check::non_negative (), Attribute::Const,
                "Temperature sum when green LAI growth turn exponential.");
    frame.declare ("S_Lx", "dg C d", Check::non_negative (), Attribute::Const,
                "Temperature sum maximum green LAI.");
    frame.declare ("S_Lr", "dg C d", Check::non_negative (), Attribute::Const,
                "Temperature sum for start of yellow leaves.");
    frame.declare ("S_Lm", "dg C d", Check::non_negative (), Attribute::Const,
                "Temperature sum at maturity.");
    frame.declare ("z_0", "mm", Check::non_negative (), Attribute::Const,
                "Root depth before emergence (growth start).");
    frame.declare ("z_v", "mm", Check::non_negative (), Attribute::Const,
                "Root depth at emergence (growth start).");
    frame.declare ("z_xA", "mm", Check::non_negative (), Attribute::Const,
                "Maximum root depth for this crop.");
    frame.declare ("z_m", "mm", Check::non_negative (), Attribute::Const,
                "Root depth at maturity.");
    frame.declare ("c_r", "mm/d", Check::non_negative (), Attribute::Const,
                "Root penetration rate.");
  }
} MV_Crop_syntax;

struct ActionMarkvand : public Action
{
  const double flux;         // Application speed [mm/h]

  // Soil & Crop.
  const std::unique_ptr<MV_Soil> soil;
  const struct crop_map_t : public std::map<symbol, const MV_Crop*>
  {
    static void load_syntax (Frame&);
    crop_map_t (const BlockModel&, const std::string& key);
    ~crop_map_t ();
  } crop_map;

  // Time.
  double T_sum;		  // Temperature sum since emergence. [dg C d]
  double dt;			// Days since emergence. [d]

  // Reservoirs.
  double V_I;	     // Amount of water intercepter by leaves. [mm]
  double V_r;	   // Amount of available water in root zone. [mm]
  double V_e; // Amount of available water in top soil reservoir. [mm]
  double C_u; // Capacity of water in upper root zone. [mm]
  double V_u;  // Amount of available water in upper root zone. [mm]
  double V_b; // Amount of water between current and max root depth. [mm]

  // Solute.
  const IM solute;

  // Simulation.
  const MV_Crop* get_crop (Daisy& daisy) const;
  void doIt (Daisy& daisy, const Scope&, Treelog& out);
  bool done (const Daisy&, const Scope&, Treelog&) const
  { return false; }
  void output (Log&) const;

  void tick (const Daisy&, const Scope&, Treelog&)
  { }
  void initialize (const Daisy&, const Scope&, Treelog&)
  { }
  bool check (const Daisy&, const Scope&, Treelog& err) const
  { return true; }

  // Create and destroy.
  ActionMarkvand (const BlockModel& al);
  ~ActionMarkvand ();
};

void 
ActionMarkvand::crop_map_t::load_syntax (Frame& frame)
{ 
  frame.declare_string ("Daisy", Attribute::Const, 
	      "Name of Daisy crop.");
  frame.declare_object ("MARKVAND", MV_Crop::component, 
                     Attribute::Const, Attribute::Singleton,
                     "MARKVAND crop description.");
  frame.order ("Daisy", "MARKVAND");
}

ActionMarkvand::crop_map_t::crop_map_t (const BlockModel& al, const std::string& key)
{
  const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>& alists = al.submodel_sequence (key);
  for (size_t i = 0; i < alists.size (); i++)
    {
      BlockSubmodel nest (al, key, i);
      (*this)[alists[i]->name ("Daisy")] 
        = Librarian::build_item<MV_Crop> (nest, "MARKVAND");
    }
}

ActionMarkvand::crop_map_t::~crop_map_t ()
{ map_delete (begin (), end ()); }

const MV_Crop*
ActionMarkvand::get_crop (Daisy& daisy) const
{
  const std::string crop_name = daisy.field ().crop_names ();
  const crop_map_t::const_iterator entry = crop_map.find (crop_name);
  return (entry != crop_map.end ()) ? entry->second : NULL;
}

void 
ActionMarkvand::doIt (Daisy& daisy, const Scope&, Treelog& msg)
{
  // Default values.
  const double default_LAI = 3.0;
  const double default_A = 0.6;

  // Daily occurence.
  if (daisy.time ().hour () != 8)
    return;
  
  // Emergence and harvest.
  const bool has_crop
    = daisy.field ().crop_dm (Vegetation::all_crops (), 0.1) > 0.0; 
  if (T_sum < 0.0)
    {
      if (has_crop
	  && daisy.time ().month () >= 3
	  && daisy.time ().month () < 8)
        {
	  const MV_Crop *const crop = get_crop (daisy);
          T_sum = 0.0;
	  dt = 0.0;
	  if (crop)
	    msg.message ("Starting MARKVAND irrigation for " 
			 + crop->name.name () + ".");
	  else
	    msg.message ("Starting MARKVAND irrigation for unknown crop "
                         + daisy.field ().crop_names () + ".");
	  const double z_x = crop 
	    ? std::min (crop->z_xA, soil->z_xJ)
	    : soil->z_xJ;
	  const double z_r = crop
	    ? crop->z_r (z_x, T_sum, dt)
	    : soil->z_xJ;
	  V_r = soil->C_r (z_r);
	  V_b = soil->C_b (z_r);
	  V_e = soil->C_e;
        }
      return;
    }
  if (!has_crop)
    {
      T_sum = dt = V_e = V_r = V_b = -42.42e42;
      V_I = C_u = V_u = 0.0;
      msg.message ("Stoping MARKVAND irrigation.");
      return;
    }

  // Weather data.
  const double air_temperature = daisy.field ().daily_air_temperature ();
  const double global_radiation = daisy.field ().daily_global_radiation ();
  const double P = daisy.field ().daily_precipitation ();
  const double reference_evapotranspiration 
    = FAO::Makkink (air_temperature, global_radiation) * 24.0;

  // Old root zone capacities.
  const MV_Crop *const crop = get_crop (daisy);
  const double z_x = crop 	// Max rooting depth. [mm]
    ? std::min (crop->z_xA, soil->z_xJ)
    : soil->z_xJ;
  const double z_r_old = crop	// Effective rooting depth. [mm]
    ? crop->z_r (z_x, T_sum, dt) 
    : z_x;
  const double C_r_old = soil->C_r (z_r_old);
  const double C_b_old = soil->C_b (z_r_old);

  // Irrigation.
  const double A = crop ? crop->A (T_sum) : default_A;
  const double A_u = (C_u > 0.0) ? 1.0 - V_u / C_u : 1.0;
  const double A_r = 1.0 - V_r / C_r_old;
  const double I_E = 0.9;	// Irrigation efficiency.
  const double I = (A_u > A && A_r > A) 
    ? (C_r_old - V_r) / I_E
    : 0.0;
  if (I > 0.0)
    {
      std::ostringstream tmp;
      tmp << "MARKVAND Irrigating " << I << " mm";
      msg.message (tmp.str ());
      daisy.field ().irrigate (I/flux, flux, Irrigation::at_air_temperature,
                             Irrigation::overhead, solute, 
                             boost::shared_ptr<Volume> (), false, msg);
    }

  // Update temperature sum and time.
  const double T_b = 0.0;       // Base temperature. [dg C]
  if (air_temperature > T_b)
    T_sum += air_temperature - T_b;
  dt += 1.0;

  // Leaves.
  const double L_g = crop ? crop->L_g (T_sum) : default_LAI; // Green LAI. []
  const double L_y = crop ? crop->L_y (T_sum) : 0.0; // Yellow LAI. []
  const double L = L_g + L_y;	// Total LAI. []
  const double k_p = 0.6;	// Extinction coefficency. []
  const double cover = exp (-k_p * L); // []
  const double green_cover = exp (-k_p * L_g); // []

  // Potential evapotranspiration.
  const double E_p = reference_evapotranspiration;
  const double E_pe = E_p * cover; // Soil surface E_p. [mm]
  const double E_pc = E_p - E_pe; // Crop E_p. [mm]
  const double E_pcg = E_p - (1.0 - green_cover); // Green leaves E_p [mm]
  const double E_pcy = E_pc - E_pcg; // Yellow leaves E_p [mm]

  // Root zone capacities.
  const double z_r = crop	// Effective rooting depth. [mm]
    ? crop->z_r (z_x, T_sum, dt) 
    : z_x;
  const double C_r = soil->C_r (z_r); // Root zone capacity.
  const double C_b = soil->C_b (z_r); // Capacity below root zone.

  // Zone adjustments.
  {
    const double V_r_new = V_r + (C_r - C_r_old) *
      ((C_r <= C_r_old) 
       ? V_r / C_r_old		// Shrinking root zone.
       : V_b / C_b_old);		// Growing root zone.
    V_b -= V_r_new - V_r;
    V_r = V_r_new;
    const double C_u_new = std::min (C_r, C_u);
    V_u = (C_u_new > 0.0) ? V_u - (C_u - C_u_new) * V_u / C_u : 0.0;
    C_u = C_u_new;
  }

  // Interception and fallthrough.
  const double c_i = 0.5; // Relative capacity. [mm]
  const double C_I = c_i * L;	// Absolute capacity. [mm]
  const double V_I_new = std::min (C_I, V_I + P + I);
  const double P_I = P + I - (V_I_new - V_I); // Fallthrough. [mm]
  V_I = V_I_new;
  V_e += P_I;
  V_r += P_I;

  // Actual evaporation.
  double E_a = 0.0;
  
  // Soil surface evaporation.
  {
    const double E_ae = (V_e < E_pe)
      ? E_pe
      : ((E_pe <= V_r + V_b)
	 ? soil->c_e * E_pe * (V_r + V_b) / (C_r + C_b)
	 : 0.0);
    E_a += E_ae;
    V_e = std::min (soil->C_e, std::max (V_e - E_ae, 0.0));
    const double V_r_new = std::max (V_r - E_ae, 0.0);
    V_b = std::max (V_b - E_ae + V_r - V_r_new, 0.0);
    V_r = V_r_new;
    V_u = std::max (V_u + P_I - E_ae, 0.0);
    C_u = std::min (C_r, C_u + std::max (P_I - E_ae, 0.0));
  }

  // Intercaption evaporation.
  const double E_aIy = (L > 0.0) ? std::min (V_I * L_y / L, E_pcy) : 0.0;
  const double E_aIg = (L > 0.0) ? std::min (V_I * L_g / L, E_pcg) : 0.0;
  const double E_aI = E_aIg + E_aIy;
  E_a += E_aI;
  V_I -= E_aI;

  // Transpiration.
  const double E_pT = E_pcg - E_aIg;
  if (E_pT > 0.0)
    {
      daisy_assert (C_r > 0.0);
      const double E_aTr 
        = E_pT * ((C_r > V_r + 1e-10) 
                  ? (1.0 - pow ((C_r - V_r) / C_r, soil->c_T / E_pT))
                  : 1.0);
      const double E_aTu = (C_u > 0.0)
	? E_pT * ((C_u > V_u + 1e-10) 
                  ? (1.0 - pow ((C_u - V_u) / C_u, soil->c_T / E_pT))
                  : 1.0)
	: 0.0;
      const double E_aT = std::min (V_r, std::max (E_aTr, E_aTu));
      E_a += E_aT;

      if (E_aT > E_aTu)
	C_u = V_u = 0.0;
      else if (E_aT > V_u)
	V_u = 0.0;
      else
	V_u -= E_aT;

      V_r -= E_aT;
    }

  // Drainage.
  const double D_r = (V_r > C_r)
    ? (soil->k_qr + (1.0 - soil->k_qr) * (z_x - z_r) / z_x) * (V_r - C_r) 
    : 0.0;
  const double D_b = (V_b > C_b)
    ? (soil->k_qb + (1.0 - soil->k_qb) * (z_r / z_x)) * (V_b - C_b) 
    : 0.0;
  V_r -= D_r;
  V_b += D_r - D_b;

#if 0
  std::ostringstream debug;
  debug << "T_sum = " << T_sum 
        << ", phase = " << (crop ? crop->phase (T_sum) : 999)
        << ", A = " << A << ", A_u = " << A_u 
        << " and A_r = " << A_r << "\n";

  debug << "P_I = " << P_I << ", E_a = " << E_a << ", D_r = " << D_r
        << ", V_r = " << V_r 
        << ", V_e = " << V_e 
        << ", V_u = " << V_u
        << ", V_b = " << V_b 
        << ", V_I = " << V_I;
  msg.message (debug.str ());
#endif
}

void
ActionMarkvand::output (Log& log) const
{
  output_variable (T_sum, log);
  output_variable (dt, log);
  output_variable (V_I, log);
  if (V_r >= 0.0)
    output_variable (V_r, log);
  if (V_e >= 0.0)
    output_variable (V_e, log);
  output_variable (C_u, log);
  output_variable (V_u, log);
  if (V_b >= 0.0)
    output_variable (V_b, log);
}

ActionMarkvand::ActionMarkvand (const BlockModel& al)
  : Action (al),
    flux (al.number ("flux")),
    soil (Librarian::build_item<MV_Soil> (al, "soil")),
    crop_map (al, "map"),
    T_sum (al.number ("T_sum", -1.0)),
    dt (al.number ("dt", -42.42e42)),
    V_I (al.number ("V_I")),
    V_r (al.number ("V_r", -42.42e42)),
    V_e (al.number ("V_e", -42.42e42)),
    C_u (al.number ("C_u")),
    V_u (al.number ("V_u")),
    V_b (al.number ("V_b", -42.42e42)),
    solute (al, "solute")
{ }

ActionMarkvand::~ActionMarkvand ()
{ }

static struct ActionMarkvandSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ActionMarkvand (al); }
  static bool check_alist (const Metalib&, const Frame&, Treelog&)
  {
    bool ok = true;
    return ok;
  }
  ActionMarkvandSyntax ()
    : DeclareModel (Action::component, "markvand", "\
Irrigate the field according to MARKVAND scheduling.")
  { }
  static void load_ppm (Frame& frame)
  { IM::add_syntax (frame, Attribute::Const, Units::ppm ()); }
  void load_frame (Frame& frame) const
  { 
    frame.add_check (check_alist);	
    frame.declare ("flux", "mm/h", Check::positive (), Attribute::Const,
                   "Water application speed.");
    frame.set ("flux", 2.0);
    frame.declare_object ("soil", MV_Soil::component, Attribute::Const, 
                       Attribute::Singleton,
                       "Soil type to schedule irrigation on.");
    frame.declare_submodule_sequence ("map", Attribute::Const, "\
Map of Daisy crop names into MARKVAND crop descriptions.",
				   &ActionMarkvand::crop_map_t::load_syntax);
    frame.declare ("T_sum", "dg C d", Attribute::OptionalState, 
                "Temperature sum since emergence.");
    frame.declare ("dt", "d", Attribute::OptionalState, 
                "Days since emergence.");
    frame.declare ("V_I", "mm", Attribute::OptionalState, 
                "Amount of water intercepter by leaves.");
    frame.set ("V_I", 0.0);
    frame.declare ("V_r", "mm", Attribute::OptionalState, 
                "Amount of available water in root zone.\n\
By default, the reservoir will be full at plant emergence.");
    frame.declare ("V_e", "mm", Attribute::OptionalState, 
                "Amount of available water in top soil reservoir.\n\
This is the water that can be extracted by soil evaporation.\n\
Included in 'V_r'.\n\
By default, the reservoir will be full at plant emergence.");
    frame.declare ("C_u", "mm", Attribute::OptionalState, 
                "Capacity of available water in upper root zone.");
    frame.set ("C_u", 0.0);
    frame.declare ("V_u", "mm", Attribute::OptionalState, 
                "Amount of available water in upper root zone.\n\
Included in 'V_r'.");
    frame.set ("V_u", 0.0);
    frame.declare ("V_b", "mm", Attribute::OptionalState, 
                "Amount of water between current and max root depth.\n\
By default, the reservoir will be full at plant emergence.");
    frame.declare_submodule_sequence ("solute", Attribute::Const, "\
Solutes in irrigation water.", load_ppm);
    frame.set_empty ("solute");
  }
} ActionMarkvand_syntax;

// action_markvand.C ends here.
