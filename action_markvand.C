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

#include "action.h"
#include "block.h"
#include "daisy.h"
#include "field.h"
#include "crop.h"
#include "im.h"
#include "fao.h"
#include "log.h"
#include "symbol.h"
#include "mathlib.h"
#include "check.h"
#include "vcheck.h"
#include "memutils.h"
#include <vector>
#include <memory>
#include <sstream>

// MV_Soil

struct MV_Soil
{
  // Content.
  const symbol name;
  static const char *const description;

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
  MV_Soil (Block& al)
    : name (al.identifier ("type")),
      z_o (al.number ("z_o")),
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

template<>
Librarian<MV_Soil>::Content* Librarian<MV_Soil>::content = NULL;

static Librarian<MV_Soil> MV_Soil_init ("MV_Soil");


const char *const MV_Soil::description = "\
Description of a soil for use by the MARKVAND model.";

static struct MV_SoilSyntax
{
  static MV_Soil&
  make (Block& al)
  { return *new MV_Soil (al); }
  MV_SoilSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Standard MARKVAND soil model.");
    syntax.add ("z_o", "mm", Check::positive (), Syntax::Const,
                "Depth of top soil.");
    syntax.add ("z_xJ", "mm", Check::positive (), Syntax::Const,
                "Max rooting depth.");
    syntax.add ("Theta_fo", Syntax::Fraction (), Check::positive (), 
                Syntax::Const, "Field capacity, topsoil.");
    syntax.add ("Theta_wo", Syntax::Fraction (), Check::positive (), 
                Syntax::Const,
                "Wielding point, topsoil.");
    syntax.add ("Theta_fu", Syntax::Fraction (), Check::positive (), 
                Syntax::Const,
                "Field capacity, subsoil.");
    syntax.add ("Theta_wu", Syntax::Fraction (), Check::positive (), 
                Syntax::Const,
                "Wielting point, subsoil.");
    syntax.add ("C_e", "mm", Check::non_negative (), Syntax::Const,
                "Capacity of evaporation reservoir.");
    syntax.add ("c_e", Syntax::Fraction (), Check::non_negative (), 
                Syntax::Const,
                "Basic evaporation factor.");
    syntax.add ("c_T", "mm", Syntax::Const,
                "Transpiration constant.");
    syntax.add ("k_qr", Syntax::None (), Syntax::Const,
                "Drainage constant root zone.");
    syntax.add ("k_qb", Syntax::None (), Syntax::Const,
                "Drainage constant subsone.");
    Librarian<MV_Soil>::add_type ("default", alist, syntax, &make);
  }
} MV_Soil_syntax;

// MV_Crop

struct MV_Crop
{
  // Content.
  const symbol name;
  static const char *const description;

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
  const double A (const double T_sum) const
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
  MV_Crop (Block& al)
    : name (al.identifier ("type")),
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

template<>
Librarian<MV_Crop>::Content* Librarian<MV_Crop>::content = NULL;

static Librarian<MV_Crop> MV_Crop_init ("MV_Crop");

const char *const MV_Crop::description = "\
Description of a crop for use by the MARKVAND model.";

static struct MV_CropSyntax
{
  static MV_Crop& make (Block& al)
  { return *new MV_Crop (al); }

  static bool check_alist (const AttributeList& al, Treelog& msg)
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
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    syntax.add_check (check_alist);
    alist.add ("description", "Standard MARKVAND crop model.");
    syntax.add ("S_F", "dg C d", Check::non_negative (), 
                Syntax::Const, Syntax::Sequence,
                "Temperature sum for each phase.");
    syntax.add_check ("S_F", VCheck::min_size_1 ());
    syntax.add ("A_F", Syntax::Fraction (), Syntax::Const, Syntax::Sequence,
                "Allowable water deficit for each phase before irrigation.");
    syntax.add_check ("A_F", VCheck::min_size_1 ());
    syntax.add ("L_gv", Syntax::None (), Check::non_negative (), Syntax::Const,
                "Green leaf area index at emergence / growth start.");
    syntax.add ("L_ge", Syntax::None (), Check::non_negative (),
                Syntax::Const, "\
Green leaf area index at the time where growth rate become exponential.");
    syntax.add ("L_gx", Syntax::None (), Check::non_negative (), Syntax::Const,
                "Maximum green leaf area index.");
    syntax.add ("L_gm", Syntax::None (), Check::non_negative (), Syntax::Const,
                "Green leaf area index at maturity.");
    syntax.add ("L_ym", Syntax::None (), Check::non_negative (), Syntax::Const,
                "Yellow leaf area index at maturity.");
    syntax.add ("S_Le", "dg C d", Check::non_negative (), Syntax::Const,
                "Temperature sum when green LAI growth turn exponential.");
    syntax.add ("S_Lx", "dg C d", Check::non_negative (), Syntax::Const,
                "Temperature sum maximum green LAI.");
    syntax.add ("S_Lr", "dg C d", Check::non_negative (), Syntax::Const,
                "Temperature sum for start of yellow leaves.");
    syntax.add ("S_Lm", "dg C d", Check::non_negative (), Syntax::Const,
                "Temperature sum at maturity.");
    syntax.add ("z_0", "mm", Check::non_negative (), Syntax::Const,
                "Root depth before emergence (growth start).");
    syntax.add ("z_v", "mm", Check::non_negative (), Syntax::Const,
                "Root depth at emergence (growth start).");
    syntax.add ("z_xA", "mm", Check::non_negative (), Syntax::Const,
                "Maximum root depth for this crop.");
    syntax.add ("z_m", "mm", Check::non_negative (), Syntax::Const,
                "Root depth at maturity.");
    syntax.add ("c_r", "mm/d", Check::non_negative (), Syntax::Const,
                "Root penetration rate.");
    Librarian<MV_Crop>::add_type ("default", alist, syntax, &make);
  }
} MV_Crop_syntax;

struct ActionMarkvand : public Action
{
  // Soil & Crop.
  const std::auto_ptr<MV_Soil> soil;
  const struct crop_map_t : public std::map<std::string, const MV_Crop*>
  {
    static void load_syntax (Syntax& syntax, AttributeList&);
    crop_map_t (Block&, const std::string& key);
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

  // Simulation.
  const MV_Crop* get_crop (Daisy& daisy) const;
  void doIt (Daisy& daisy, Treelog& out);
  bool done (const Daisy&, Treelog&) const
  { return false; }
  void output (Log&) const;

  // Create and destroy.
  ActionMarkvand (Block& al);
  ~ActionMarkvand ();
};

void 
ActionMarkvand::crop_map_t::load_syntax (Syntax& syntax, AttributeList&)
{ 
  syntax.add ("Daisy", Syntax::String, Syntax::Const, 
	      "Name of Daisy crop.");
  syntax.add ("MARKVAND", Librarian<MV_Crop>::library (), 
	      Syntax::Const, Syntax::Singleton,
	      "MARKVAND crop description.");
  syntax.order ("Daisy", "MARKVAND");
}

ActionMarkvand::crop_map_t::crop_map_t (Block& al, const std::string& key)
{
  const Syntax& syntax = al.syntax ().syntax (key);
  const std::vector<AttributeList*>& alists = al.alist_sequence (key);
  for (size_t i = 0; i < alists.size (); i++)
    {
      Block nest (al, syntax, *alists[i], sequence_id (key, i));
      (*this)[alists[i]->name ("Daisy")] 
	= Librarian<MV_Crop>::build_item (nest, "MARKVAND");
    }
}

ActionMarkvand::crop_map_t::~crop_map_t ()
{ map_delete (begin (), end ()); }

const MV_Crop*
ActionMarkvand::get_crop (Daisy& daisy) const
{
  const std::string crop_name = daisy.field.crop_names ();
  const crop_map_t::const_iterator entry = crop_map.find (crop_name);
  return (entry != crop_map.end ()) ? entry->second : NULL;
}

void 
ActionMarkvand::doIt (Daisy& daisy, Treelog& out)
{
  // Default values.
  const double default_LAI = 3.0;
  const double default_A = 0.6;

  // Daily occurence.
  if (daisy.time.hour () != 8)
    return;
  
  // Emergence and harvest.
  static const symbol all_symbol ("all");
  const bool has_crop = daisy.field.crop_dm (all_symbol, 0.1) > 0.0; 
  if (T_sum < 0.0)
    {
      if (has_crop)
        {
	  const MV_Crop *const crop = get_crop (daisy);
          T_sum = 0.0;
	  dt = 0.0;
	  if (crop)
	    out.message ("Starting MARKVAND irrigation for " 
			 + crop->name.name () + ".");
	  else
	    out.message ("Starting MARKVAND irrigation for unknown crop "
                         + daisy.field.crop_names () + ".");
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
      out.message ("Stoping MARKVAND irrigation.");
      return;
    }

  // Weather data.
  const double air_temperature = daisy.field.daily_air_temperature ();
  const double global_radiation = daisy.field.daily_global_radiation ();
  const double P = daisy.field.daily_precipitation ();
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
      out.message (tmp.str ());
      IM im;
      daisy.field.irrigate_overhead (I, im, daisy.dt);
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
  const double E_pcy = E_pc - E_pcy; // Yellow leaves E_p [mm]

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
  out.message (debug.str ());
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

ActionMarkvand::ActionMarkvand (Block& al)
  : Action (al),
    soil (Librarian<MV_Soil>::build_item (al, "soil")),
    crop_map (al, "map"),
    T_sum (al.number ("T_sum", -1.0)),
    dt (al.number ("dt", -42.42e42)),
    V_I (al.number ("V_I")),
    V_r (al.number ("V_r", -42.42e42)),
    V_e (al.number ("V_e", -42.42e42)),
    C_u (al.number ("C_u")),
    V_u (al.number ("V_u")),
    V_b (al.number ("V_b", -42.42e42))
{ }

ActionMarkvand::~ActionMarkvand ()
{ }

static struct ActionMarkvandSyntax
{
  static Action& make (Block& al)
  { return *new ActionMarkvand (al); }
  static bool check_alist (const AttributeList&, Treelog&)
  {
    bool ok = true;
    return ok;
  }
  ActionMarkvandSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    syntax.add_check (check_alist);	
    syntax.add ("soil", Librarian<MV_Soil>::library (), Syntax::Const, 
                Syntax::Singleton,
                "Soil type to schedule irrigation on.");
    syntax.add_submodule_sequence ("map", Syntax::Const, "\
Map of Daisy crop names into MARKVAND crop descriptions.",
				   &ActionMarkvand::crop_map_t::load_syntax);
    syntax.add ("T_sum", "dg C d", Syntax::OptionalState, 
                "Temperature sum since emergence.");
    syntax.add ("dt", "d", Syntax::OptionalState, 
                "Days since emergence.");
    syntax.add ("V_I", "mm", Syntax::OptionalState, 
                "Amount of water intercepter by leaves.");
    alist.add ("V_I", 0.0);
    syntax.add ("V_r", "mm", Syntax::OptionalState, 
                "Amount of available water in root zone.\n\
By default, the reservoir will be full at plant emergence.");
    syntax.add ("V_e", "mm", Syntax::OptionalState, 
                "Amount of available water in top soil reservoir.\n\
This is the water that can be extracted by soil evaporation.\n\
Included in 'V_r'.\n\
By default, the reservoir will be full at plant emergence.");
    syntax.add ("C_u", "mm", Syntax::OptionalState, 
                "Capacity of available water in upper root zone.");
    alist.add ("C_u", 0.0);
    syntax.add ("V_u", "mm", Syntax::OptionalState, 
                "Amount of available water in upper root zone.\n\
Included in 'V_r'.");
    alist.add ("V_u", 0.0);
    syntax.add ("V_b", "mm", Syntax::OptionalState, 
                "Amount of water between current and max root depth.\n\
By default, the reservoir will be full at plant emergence.");
    alist.add ("description", "\
Irrigate the field according to MARKVAND scheduling.");
    Librarian<Action>::add_type ("markvand", alist, syntax, &make);
  }
} ActionMarkvand_syntax;

