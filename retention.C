// retention.C --- Retention curve.
// 
// Copyright 2020 KU.
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

#include "retention.h"
#include "block_model.h"
#include "mathlib.h"
#include "librarian.h"
#include "check.h"
#include "intrinsics.h"
#include "library.h"
#include "frame_model.h"
#include "treelog.h"
#include "mathlib.h"
#include <sstream>

// The 'retention' component.

const char *const Retention::component = "retention";

symbol 
Retention::library_id () const
{
  static const symbol id (component);
  return id;
}

Retention::Retention ()
{ }

Retention::~Retention ()
{ }


// The 'PASTIS' model.

static struct RetentionInit : public DeclareComponent 
{
  RetentionInit ()
    : DeclareComponent (Retention::component, "\
Specify a retention or a halftime.")
  { }
} Retention_init;

// The 'PASTIS' model.

struct RetentionPASTIS : public Retention
{
  double Theta_res;
  double h_min;
  double Theta_sat;
  
  double h (const double Theta) const // []->[cm]
  {
    if (Theta < Theta_res)
      return h_min;
    if (Theta > Theta_sat)
      return 0.0;

    return -std::pow (-h_min,
		      1.0-((Theta - Theta_res)
			   / (2.0 * Theta_sat / 3.0 - Theta_res)));
  }

  void initialize (const double Theta_res_, const double h_res_,
		   const double Theta_sat_, Treelog&)
  {
    Theta_res = Theta_res_;
    h_min = h_res_;
    Theta_sat = Theta_sat_;
  }
  RetentionPASTIS (const BlockModel&)
  { }
  ~RetentionPASTIS ()
  { }
};

static struct RetentionPASTISSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new RetentionPASTIS (al); }
  RetentionPASTISSyntax ()
    : DeclareModel (Retention::component, "PASTIS", "\
Retention curve of Mulch used in PASTIS.\n\
h (Theta) = -(-h_min)^(1-((Theta - Theta_res) / (2/3 Theta_sat - Theta_res)))")
  { }
  void load_frame (Frame&) const
  { }
} RetentionPASTIS_syntax;

// The 'Exponential' model.

struct RetentionExponential : public Retention
{
  double k; 			// [cm^-1]
  double Theta_res;		// []
  double h_min;			// [cm]
  double Theta_sat;		// []
  
  double h (const double Theta) const // []->[cm]
  {
    
    if (Theta <= Theta_res)
      return h_min;
    if (Theta >= Theta_sat)
      return 0.0;

    return std::log ((Theta - Theta_res) / (Theta_sat - Theta_res)) / k;
  }

  double Theta (const double h) // [cm]->[]
  { return std::exp (k * h) * (Theta_sat - Theta_res) + Theta_res;  }
  
  void initialize (const double Theta_res_, const double h_res_,
		   const double Theta_sat_, Treelog& msg)
  {
    if (Theta_res < 0.0)
      Theta_res = Theta_res_;
    if (h_min > 0.0)
      h_min = h_res_;
    if (Theta_sat < 0.0)
      Theta_sat = Theta_sat_;

    std::ostringstream tmp;
    tmp << "pF\th\tTheta";
    for (double pF = -1.0; pF < 10.5; pF += 1.0)
      {
	const double h1 = pF2h (pF);
	const double Theta1 = Theta (h1);
	const double h2 = h (Theta1);
	tmp << "\n" << pF << "\t" << h2 << "\t" << Theta1;
      }
    msg.debug (tmp.str ());
  }
  RetentionExponential (const BlockModel& al)
    : k (al.number ("k")),
      Theta_res (al.number ("Theta_res", -1.0)),
      h_min (al.number ("h_min", 1.0)),
      Theta_sat (al.number ("Theta_sat", -1.0))
  { }
  ~RetentionExponential ()
  { }
};

static struct RetentionExponentialSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new RetentionExponential (al); }
  RetentionExponentialSyntax ()
    : DeclareModel (Retention::component, "exp", "\
Retention curve of Mulch used in Exponential.\n\
Theta (h) = exp (k h) (Theta_sat - Theta_res) + Theta_res\n\
h (Theta) = ln ((Theta - Theta_res) / (Theta_sat - Theta_res)) / k")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare ("k", "cm^-1", Check::non_negative (),
		   Attribute::Const, "\
Theta (h) = exp (k h) (Theta_sat - Theta_res) + Theta_res");
    frame.declare_fraction ("Theta_res", Attribute::OptionalConst, "\
Residual water.");
    frame.declare ("h_min", "cm", Attribute::OptionalConst, "\
Pressure below residual water.");
    frame.declare_fraction ("Theta_sat", Attribute::OptionalConst, "\
Residual water.");
  }
} RetentionExponential_syntax;

// retention.C ends here.
