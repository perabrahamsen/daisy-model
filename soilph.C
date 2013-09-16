// soilph.C --- pH og soil.
// 
// Copyright 2013 KU.
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

#include "soilph.h"
#include "log.h"
#include "block_model.h"
#include "submodeler.h"
#include "librarian.h"
#include "check.h"
#include "vcheck.h"
#include "plf.h"
#include "memutils.h"
#include "assertion.h"
#include "geometry.h"

// The 'soilph' component.

const char *const SoilpH::component = "soilph";

symbol
SoilpH::library_id () const
{
  static const symbol id (component);
  return id;
}

SoilpH::SoilpH (const BlockModel& al)
  : ModelDerived (al.type_name ())
{ }

SoilpH::~SoilpH ()
{ }

static struct SoilpHInit : public DeclareComponent 
{
  void load_frame (Frame& frame) const
  {
    Model::load_model (frame);
    frame.declare ("pH", "pH", Check::none (), 
                   Attribute::LogOnly, Attribute::SoilCells, "\
Current pH of soil.");
  }
  SoilpHInit ()
    : DeclareComponent (SoilpH::component, "\
pH of soil.")
  { }
} SoilpH_init;

// The 'neutral' model.

struct SoilpHNeutral : public SoilpH
{
  double pH (size_t) const
  { return 7.0; }
  void tick (const Geometry&, const Time&, Treelog&)
  { }
  void output (Log&) const
  { }
  void initialize (const Geometry&, const Time&, Treelog&)
  { }
  SoilpHNeutral (const BlockModel& al)
    : SoilpH (al)
  { }
};

static struct SoilpHNeutralSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new SoilpHNeutral (al); }
  SoilpHNeutralSyntax ()
    : DeclareModel (SoilpH::component, "neutral", "\
A soil with a constant pH of 7.")
  { }
  void load_frame (Frame& frame) const
  { }
} SoilpHNeutral_syntax;

// The 'year' model.

struct SoilpHYear : public SoilpH
{
  struct pHyear
  {
    // Content.
    const int year;
    const PLF pH_depth;

    // Create and Destroy.
    static void load_syntax (Frame& frame)
    { 
      frame.declare_integer ("year", Attribute::Const,
                             "Calendar year.");
      frame.declare ("pH_depth", "cm", "pH", Attribute::Const,
                     "pH as a function of depth.");
      frame.order ("year", "pH_depth");
    }
    pHyear (const Block& al)
      : year (al.integer ("year")),
	pH_depth (al.plf ("pH_depth"))
    { }
    ~pHyear ()
    { }
  };
  auto_vector<const pHyear*> pH_year;
  std::vector<double> pH_index;

  double pH (size_t c) const
  { 
    daisy_assert (c < pH_index.size ());
    return pH_index[c];
  }
  
  void tick (const Geometry& geo, const Time& time, Treelog&)
  {
    const size_t cell_size = geo.cell_size ();
    daisy_assert (pH_year.size () > 0);

    // Make room.
    while (pH_index.size () < cell_size)
      pH_index.push_back (7.0);

    // Now.
    const int year = time.year ();

    // Find interval.
    const pHyear* before = NULL;
    const pHyear* after = NULL;
    for (int i = 0; i < pH_year.size (); i++)
      {
        if (pH_year[i]->year <= year)
          before = pH_year[i];
        else if (after == NULL && pH_year[i]->year >= year)
          after = pH_year[i];
      }
    daisy_assert (before || after);
    if (!before)
      before = after;
    else if (!after)
      after = before;

    if (before == after)
      // Exact match.
      for (size_t c = 0; c < cell_size; c++)
        pH_index[c] = before->pH_depth (geo.cell_z (c));
    else
      // Interpolate.
      {
        daisy_assert (before->year < after->year);
        const double x0 = before->year;
        const double x1 = after->year;
        const double dx = (year - x0) / (x1 - x0);
        for (size_t c = 0; c < cell_size; c++)
          {
            const double z = geo.cell_z (c);
            const double y0 = before->pH_depth (z);
            const double y1 = after->pH_depth (z);
            // Linear interpolation.
            pH_index[c] = y0 + (y1 - y0) * dx;
          }
      }
  }
  void initialize (const Geometry& geo, const Time& time, Treelog& msg)
  { tick (geo, time, msg); }

  void output (Log& log) const
  { output_value (pH_index, "pH", log); }
  SoilpHYear (const BlockModel& al)
    : SoilpH (al),
      pH_year (map_submodel_const<pHyear> (al, "pH_depth_year"))
  { }
};

static struct SoilpHYearSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new SoilpHYear (al); }
  SoilpHYearSyntax ()
    : DeclareModel (SoilpH::component, "year", "\
A soil with a constant pH of 7.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.declare_submodule_sequence ("pH_depth_year", Attribute::Const, "\
pH as a function of depth and calendar year.\n                          \
If specified, Daisy will interpolate between depth and between years.\n\
By default, a pH of 7 is assumed.", SoilpHYear::pHyear::load_syntax);
    frame.set_check ("pH_depth_year", VCheck::min_size_1 ());
  }
} SoilpHYear_syntax;

// soilph.C ends here.
