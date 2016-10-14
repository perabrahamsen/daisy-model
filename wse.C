// wse.C -- Water Stress Effect on yield.
// 
// Copyright 2004 Per Abrahamsen, Søren Hansen and KVL.
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

#include "wse.h"
#include "block_model.h"
#include "program.h"
#include "mathlib.h"
#include "librarian.h"
#include "treelog.h"
#include "frame.h"
#include <sstream>
#include <memory>

const char *const WSE::component = "wse";

symbol
WSE::library_id () const
{
  static const symbol id (component);
  return id;
}

WSE::WSE ()
{ }

WSE::~WSE ()
{ }

struct WSE_full : public WSE
{
  double factor (const double water_stress) const
  { return 1.0 - water_stress; }

  WSE_full ()
  { }
  WSE_full (const BlockModel&)
  { }
  ~WSE_full ()
  { }
};

std::unique_ptr<WSE> 
WSE::create_full ()
{
  std::unique_ptr<WSE> full (new WSE_full ());
  return full;
}

static struct WSE_fullSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new WSE_full (al); }
  WSE_fullSyntax ()
    : DeclareModel (WSE::component, "full", "\
Water stress has full effect on crop growth.\n\
This means that if there is 50% water stress, assimilate production\n\
will be cut into half.")
  { }
  void load_frame (Frame& frame) const
  {

  }
} WSE_full_syntax;

struct WSE_partial : public WSE
{
  const double y_half;

  double factor (const double water_stress) const
  { 
    if (approximate (y_half, 0.5))
      return 1.0 - water_stress;
    const double divisor = (1.0 - 2 * y_half) * water_stress + y_half;
    const double factor 
      = 1.0 - ((fabs (divisor) < 1e-10) 
               ? 0.0 
               : y_half / divisor);
    const double effect = 1.0 + factor * (1.0 - y_half) / (2.0 * y_half - 1.0);
    return bound (0.0, effect, 1.0);
  }

  WSE_partial (const BlockModel& al)
    : y_half (al.number ("y_half"))
  { }
  ~WSE_partial ()
  { }
};

static struct WSE_partialSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new WSE_partial (al); }
  WSE_partialSyntax ()
    : DeclareModel (WSE::component, "partial", "\
Water stress has partial effect on crop growth.\n\
\n\
With this model, there will be full production when there is enough\n\
available soil water to cover the potential evapotranspiration, and no\n\
production when there is no soil water available.  In between production\n\
is controled by the 'y_half' parameter.\n\
\n\
See SH:REFERENCE for more explanation.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_fraction ("y_half", Attribute::Const, "\
Effect on assimilate production of water stress.\n\
This parameter specifies the effect on assimilate production\n(\
compared to potential) when the amount of available soil water is\n\
enough to cover exactly half the potential evapotranspiration.");
  }
} WSE_partial_syntax;

struct WSE_none : public WSE
{
  double factor (const double) const
  { return 1.0; }

  WSE_none ()
  { }
  WSE_none (const BlockModel&)
  { }
  ~WSE_none ()
  { }
};

std::unique_ptr<WSE> 
WSE::create_none ()
{
  std::unique_ptr<WSE> none (new WSE_none ());
  return none;
}

static struct WSE_noneSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new WSE_none (al); }
  WSE_noneSyntax ()
    : DeclareModel (WSE::component, "none", 
               "Water stress has no effect on plant growth.")
  { }
  void load_frame (Frame& frame) const
  {

  }
} WSE_none_syntax;


struct ProgramWSE_table : public Program
{
  const std::unique_ptr<WSE> wse;
  const int intervals;

  bool run (Treelog& msg)
  {
    std::ostringstream tmp;
    tmp << "stress\teffect\n";
    for (int i = 0; i <= intervals; i++)
      {
        const double ws = (i + 0.0) / (intervals + 0.0);
        const double e = wse->factor (ws);
        tmp << ws << "\t" << e << "\n";
      }
    msg.message (tmp.str ());
    return true;
  }

  // Create and Destroy.
  void initialize (Block&)
  { };
  bool check (Treelog&)
  { return true; }
  ProgramWSE_table (const BlockModel& al)
    : Program (al),
      wse (Librarian::build_item<WSE> (al, "wse")),
      intervals (al.integer ("intervals"))
  { }
  ~ProgramWSE_table ()
  { }
};

static struct ProgramWSE_tableSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ProgramWSE_table (al); }
  ProgramWSE_tableSyntax ()
    : DeclareModel (Program::component, "wse", "Generate a table of the water stress effect.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_object ("wse", WSE::component, 
                       Attribute::Const, Attribute::Singleton, "\
The water stress effect to show in the table.");
    frame.declare_integer ("intervals", Attribute::Const, "\
Number of intervals in the table.");
    frame.set ("intervals", 10);
    frame.order ("wse");
  }
} ProgramWSE_table_syntax;

static struct WSEInit : public DeclareComponent 
{
  WSEInit ()
    : DeclareComponent (WSE::component, "\
The water stress effect on crop growth.")
  { }
} WSE_init;

// wse.C ends here.
