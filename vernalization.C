// vernalization.C -- Default crop vernalization submodel.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
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

#include "vernalization.h"
#include "librarian.h"
#include "log.h"
#include "frame.h"
#include "block_model.h"

// The 'vernalization' component.

const char *const Vernalization::component = "vernalization";

symbol
Vernalization::library_id () const
{
  static const symbol id (component);
  return id;
}

Vernalization::Vernalization (const BlockModel& al)
  : ModelDerived (al.type_name ())
{ }

Vernalization::~Vernalization ()
{ }

static struct VernalizationInit : public DeclareComponent 
{
  VernalizationInit ()
    : DeclareComponent (Vernalization::component, "\
Requirement for a cold period before flowering.")
  { }
} Vernalization_init;
// The 'default' vernalization model.

struct VernalizationStandard : public Vernalization
{
  // Parameters.
  const double DSLim;		// Max DS without vernalization
  const double TaLim;		// Vernalization temp threshold

  // State.
  double TaSum;		// Vernalization T-sum requirement

  // Simulation.
  void operator () (double Ta, double& DS)
  {
    if (TaSum < 0)
      {
        TaSum -= std::min (Ta - TaLim, 0.0);
        if (DS > DSLim)
          DS = DSLim;
      }
  }

  void output (Log& log) const
  { output_variable (TaSum, log); }

  // Create and Destroy.
  VernalizationStandard (const BlockModel& al)
    : Vernalization (al),
      DSLim (al.number ("DSLim")),
      TaLim (al.number ("TaLim")),
      TaSum (al.number ("TaSum"))
  { }
};

static struct VernalizationStandardSyntax : public DeclareModel
{
  // We can't use "used_to_be_a_submodel" since the default model is "none".
  Model* make (const BlockModel& al) const
  { return new VernalizationStandard (al); }
  VernalizationStandardSyntax ()
    : DeclareModel (Vernalization::component, "default", "\
Temperature sum dependent vernalization.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare ("DSLim", "DS", Attribute::Const,
               "Development stage at vernalization.");
    frame.declare ("TaLim", "dg C", Attribute::Const,
               "Vernalization temperature threshold.");
    frame.declare ("TaSum", "dg C d", Attribute::State,
               "Vernalization temperature-sum requirement.");
  }
} standard_vernalization_syntax;

// The 'none' vernalization model.

struct VernalizationNone : public Vernalization
{
  // Simulation.
  void operator () (double, double&)
  { return; }

  void output (Log&) const
  { }

  // Create and Destroy.
  VernalizationNone (const BlockModel& al)
    : Vernalization (al)
  { }
};

static struct VernalizationNoneSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new VernalizationNone (al); }
  VernalizationNoneSyntax ()
    : DeclareModel (Vernalization::component, "none", "\
No vernalization.")
  { }
  void load_frame (Frame&) const
  { }
} none_vernalization_syntax;

// vernalization.C ends here.
