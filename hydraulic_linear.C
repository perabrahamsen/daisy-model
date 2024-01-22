// hydraulic_linear.C -- Hysteresis based on linear transform.
// 
// Copyright 2020 KU
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

#include "hydraulic.h"
#include "block_model.h"
#include "librarian.h"
#include "frame.h"
#include "assertion.h"
#include "log.h"
#include <sstream>

class HydraulicLinear : public Hydraulic
{
  // Parameters.
  const std::unique_ptr<Hydraulic> wet;
  const std::unique_ptr<Hydraulic> dry;
  const bool debug;
  
  // State variables
  bool is_wetting;
  double a;
  double b;
  double c;
  double d;
  const Hydraulic* hyd;

  void tick (const double dt /* [h] */, const double rain /* [mm/h] */,
	     const double ice /* */, Treelog& msg);
  void hysteresis (const double dt /* [h] */,
		   const double h_old /* [cm] */,
		   const double h /* [cm] */,
		   const double T);
  void output (Log&) const;

  double Theta (double h) const;
  double KT (double h, double T) const;
  double Cw2 (double h) const;
  double h (double Theta) const;
  double M (double h) const;


  // Create and Destroy.
  void initialize (const Texture&, double rho_b, bool top_soil,
		   double CEC, double center_z, Treelog&);
public:
  HydraulicLinear (const BlockModel&);
  virtual ~HydraulicLinear ();
};

void
HydraulicLinear::tick (const double dt /* [h] */,
		       const double rain /* [mm/h] */,
		       const double ice /* */, Treelog& msg)
{
  wet->tick (dt, rain, ice, msg);
  dry->tick (dt, rain, ice, msg);
}

void
HydraulicLinear::hysteresis (const double /* [h] */,
			     const double h_old /* [cm] */,
			     const double h /* [cm] */,
			     const double T /* [dg C] */)
{
  if (is_wetting && h < h_old)
    {
      // Switch to drying.
      daisy_assert (hyd == wet.get ());

      // Find turnover point (h_tp, Theta_tp, K_tp).
      const double h_tp = h_old;
      const double Theta_tp = Theta (h_tp);
      const double K_tp = KT (h_tp, T);

      // Turnover values in dry curve (h_tp, Theta_dry, K_dry)
      const double Theta_dry = dry->Theta (h_tp);
      const double K_dry = dry->KT (h_tp, T);

      // Dry extreme is (h=-inf, Theta=Theta_res, K=0).
      const double Theta_res = dry->Theta_res;
      daisy_assert (Theta_dry > Theta_res);
      
      // Scale retention curve.
      // a Theta_dry (h) + b = Theta_tp
      // a Theta_res + b = Theta_res
      a = (Theta_tp - Theta_res) / (Theta_dry - Theta_res);
      b = Theta_tp - (a * Theta_dry);
      daisy_approximate (0.01 + Theta_res - a * Theta_res, 0.01 + b);

      // Scale dry conductivity curve-
      // c K_dry + d = K_tp
      // c K (-inf) + d = 0
      c = K_tp / K_dry;
      d = 0.0;

      if (debug)
	{
	  std::ostringstream tmp;
	  tmp << "Swicthing to drying at (h, Theta, K) = (" << h_tp << ", "
	      << Theta_tp << ", " << K_tp << "); Theta* (h) = "
	      << a << " Theta (h) + " << b << "; K* (h) = " << c
	      << " K (h) + " << d;
	  Assertion::message (tmp.str ());
	}
      
      // Make it official.
      hyd = dry.get ();
      is_wetting = false;

      // Check turnover point unchanged.
      daisy_approximate (Theta (h_tp), Theta_tp);
      daisy_approximate (KT (h_tp, T), K_tp);
    }
  else if (!is_wetting && h > h_old && h < 0.0)
    {
      // Switch to wetting.
      daisy_assert (hyd == dry.get ());

      // Find turnover point (h_tp, Theta_tp, K_tp).
      const double h_tp = h_old;
      const double Theta_tp = Theta (h_tp);
      const double K_tp = KT (h_tp, T);

      // Turnover values in wet curve (h_tp, Theta_wet, K_wet)
      const double Theta_wet = wet->Theta (h_tp);
      const double K_wet = wet->KT (h_tp, T);

      // Wet extreme is (h=0, Theta=Theta_sat, K=K_sat).
      const double Theta_sat = wet->Theta_sat;
      const double K_sat = wet->K_sat;
      daisy_assert (Theta_wet <= Theta_sat);
      daisy_assert (K_wet <= K_sat);

      // Scale retention curve.
      // a Theta_wet (h) + b = Theta_tp
      // a Theta_sat + b = Theta_sat
      a = (Theta_tp - Theta_sat) / (Theta_wet - Theta_sat);
      b = Theta_tp - (a * Theta_wet);
      daisy_approximate (b, Theta_sat - a * Theta_sat);

      // Scale conductivity curve.
      // c K_wet (h) + d = K_tp
      // c K_sat + d = K_sat
      c = (K_tp - K_sat) / (K_wet - K_sat);
      d = K_tp - (c * K_wet);
      daisy_approximate (d, K_sat - c * K_sat);

      if (debug)
	{
	  std::ostringstream tmp;
	  tmp << "Swicthing to wetting at (h, Theta, K) = (" << h_tp << ", "
	      << Theta_tp << ", " << K_tp << "); Theta* (h)= "
	      << a << " Theta (h) + " << b << "; K* (h) = " << c
	      << " K (h) + " << d;
	  Assertion::message (tmp.str ());
	}

      // Make it official.
      hyd = wet.get ();
      is_wetting = true;
    }
}

void
HydraulicLinear::output (Log& log) const
{
  output_object (wet, "wet", log);
  output_object (dry, "dry", log);
  output_variable (is_wetting, log);
  output_variable (a, log);
  output_variable (b, log);
  output_variable (c, log);
  output_variable (d, log);
}

double 
HydraulicLinear::Theta (const double h) const
{ return a * hyd->Theta (h) + b; }

double 
HydraulicLinear::KT (const double h, const double T) const
{ return c * hyd->KT (h, T) + d; }

double 
HydraulicLinear::Cw2 (const double h) const
{ return hyd->Cw2 (h) + a; }

double 
HydraulicLinear::h (const double Theta) const
{ return hyd->h ((Theta - b) / a); }

double 
HydraulicLinear::M (double h) const
{ return c * hyd->M (h); }

void
HydraulicLinear::initialize (const Texture& texture,
			     double rho_b, bool top_soil,
			     double CEC, double center_z, Treelog& msg)
{
  Assertion::message ("InitLin");
  wet->initialize (texture, rho_b, top_soil, CEC, center_z, msg);
  dry->initialize (texture, rho_b, top_soil, CEC, center_z, msg);
  Theta_sat = std::max (wet->Theta_sat, dry->Theta_sat);
  Theta_res = std::min (wet->Theta_res, dry->Theta_res);
}

HydraulicLinear::HydraulicLinear (const BlockModel& al)
  : Hydraulic (al),
    wet (Librarian::build_item<Hydraulic> (al, "wet")),
    dry (Librarian::build_item<Hydraulic> (al, "dry")),
    debug (al.flag ("debug")),
    is_wetting (al.flag ("is_wetting")),
    a (al.number ("a")),
    b (al.number ("b")),
    c (al.number ("c")),
    d (al.number ("d")),
    hyd (is_wetting ? wet.get () : dry.get ())
{ }
    
HydraulicLinear::~HydraulicLinear ()
{ }

static struct HydraulicLinearSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new HydraulicLinear (al); }
  HydraulicLinearSyntax ()
    : DeclareModel (Hydraulic::component, "linear",
		    "Hysteresis using linear transformation.\n\
\n\
Theta* (h) = a Theta (h) + b\n\
\n\
K* (h) = c K (h) + d\n\
\n\
where Theta and K are defined by either 'wet' or 'dry'.\n\
\n\
'a', 'b', 'c', and 'd' are recalculated whenever the node switches between\n\
wetting and drying.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.declare_object ("wet", Hydraulic::component, 
                          "The hydraulic propeties of wetting soil.");
    frame.declare_object ("dry", Hydraulic::component, 
                          "The hydraulic propeties of drying soil.");
    frame.declare_boolean ("debug", Attribute::Const, "\
Let us know when we switch between drying and wetting.");
    frame.set ("debug", false);
    frame.declare_boolean ("is_wetting", Attribute::State, "\
True iff 'a', 'b', 'c', and 'd' are applied to the 'wet' model.");
    frame.declare ("a", Attribute::None (), Attribute::State, "\
Factor multiplied with water content (Theta).");
    frame.declare ("b", "cm^3/cm^3", Attribute::State, "\
Offset added to water content (Theta).");
    frame.declare ("c", Attribute::None (), Attribute::State, "\
Factor multiplied with hydraulic conductivity (K).");
    frame.declare ("d", "cm/h", Attribute::State, "\
Offset added to hydraulic conductivity (K).");
  }
} hydraulicLinear_syntax;

// hydraulic_linear.C ends here.
