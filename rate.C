// rate.C --- Find a rate [h^-1]
// 
// Copyright 2018 KU.
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

#include "rate.h"
#include "block_model.h"
#include "mathlib.h"
#include "librarian.h"
#include "check.h"
#include "intrinsics.h"
#include "library.h"
#include "frame_model.h"

// The 'rate' component.

const char *const Rate::component = "rate";

symbol 
Rate::library_id () const
{
  static const symbol id (component);
  return id;
}

Rate::Rate ()
{ }

Rate::~Rate ()
{ }

static struct RateInit : public DeclareComponent 
{
  RateInit ()
    : DeclareComponent (Rate::component, "\
Specify a rate or a halftime.")
  { }
} Rate_init;

void
Rate::declare (Frame& frame, const symbol name, const symbol description)
{
  frame.declare_object (name, component, description);
}

void
Rate::set_rate (Frame& frame, const symbol name, const double value)
{
  const symbol model = "rate";
  const Intrinsics& intrinsics = Librarian::intrinsics ();
  intrinsics.instantiate (component, model);
  const FrameModel& old = intrinsics.library (component).model (model);
  boost::shared_ptr<FrameModel> child (&old.clone ());
  child->set ("rate", value);
  frame.set (name, child);
}

void
Rate::set_halftime (Frame& frame, const symbol name, const double value)
{
  const symbol model = "halftime";
  const Intrinsics& intrinsics = Librarian::intrinsics ();
  intrinsics.instantiate (component, model);
  const FrameModel& old = intrinsics.library (component).model (model);
  boost::shared_ptr<FrameModel> child (&old.clone ());
  child->set ("halftime", value);
  frame.set (name, child);
}

double
Rate::value (const BlockModel& al, const symbol name)
{
  const std::unique_ptr<Rate> model (Librarian::build_item<Rate> (al, name));
  const double result = model->find_rate ();
  return result;
}

// The 'rate' model.

struct RateRate : public Rate
{
  const double rate;		// [h^-1]

  double find_rate () const
  { return rate; }

  RateRate (const BlockModel& al)
    : rate (al.number ("rate"))
  { }
  ~RateRate ()
  { }
};

static struct RateRateSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new RateRate (al); }
  RateRateSyntax ()
    : DeclareModel (Rate::component, "rate", "\
Specify rate directly.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare ("rate", "h^-1", Attribute::Const, "\
Rate to use.");
    frame.order ("rate");
  }
} RateRate_syntax;

// The zero paramerization.

static struct RateZeroSyntax : public DeclareParam
{ 
  RateZeroSyntax ()
    : DeclareParam (Rate::component, "zero", "rate", "\
A rate of zero.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.set ("rate", 0.0);
  }
} RateZero_syntax;


// halftime model.

struct RateHalftime : public Rate
{
  const double rate;		// [h^-1]

  double find_rate () const
  { return rate; }

  RateHalftime (const BlockModel& al)
    : rate (halftime_to_rate (al.number ("halftime")))
  { }
  ~RateHalftime ()
  { }
};

static struct RateHalftimeSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new RateHalftime (al); }
  RateHalftimeSyntax ()
    : DeclareModel (Rate::component, "halftime", "\
A rate specified through the equivalent halftime (rate = ln 2 / halftime).")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare ("halftime", "h", Check::positive (), Attribute::Const, "\
Halftime of the rate to use.");
    frame.order ("halftime");
  }
} RateHalftime_syntax;

// rate.C ends here.
