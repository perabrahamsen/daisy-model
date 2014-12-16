// deposition.C --- Deposition of inorganic material from atmosphere.
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

#include "deposition.h"
#include "log.h"
#include "weather.h"
#include "block_model.h"
#include "librarian.h"

// The 'deposition' component.

const char *const Deposition::component = "deposition";

symbol
Deposition::library_id () const
{
  static const symbol id (component);
  return id;
}

const IM& 
Deposition::deposit () const
{ return my_deposit; }

void 
Deposition::output (Log& log) const
{ output_submodule (deposit (), "deposit", log); }

Deposition::Deposition (const BlockModel& al)
  : ModelDerived (al.type_name ()),
    my_deposit (al, "deposit")    // For the units...
{ }

Deposition::~Deposition ()
{ }

static struct DepositionInit : public DeclareComponent 
{
  static void load_flux (Frame& frame)
  { IM::add_syntax (frame, Attribute::LogOnly, IM::flux_unit ()); }
  void load_frame (Frame& frame) const
  {
    Model::load_model (frame);
    frame.declare_submodule_sequence ("deposit", Attribute::LogOnly, "\
Total atmospheric deposition.", load_flux);
  }
  DepositionInit ()
    : DeclareComponent (Deposition::component, "\
Deposition of inorganic material from atmosphere.")
  { }
} Deposition_init;

// The 'weather' model.

struct DepositionWeather : public Deposition
{
  void tick (const Vegetation&, const Weather& weather, Treelog&)
  { my_deposit = weather.deposit (); }

  DepositionWeather (const BlockModel& al)
    : Deposition (al)
  { }
};

static struct DepositionWeatherSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new DepositionWeather (al); }
  DepositionWeatherSyntax ()
    : DeclareModel (Deposition::component, "weather", "\
Rely solely on weather model for deposition.")
  { }
  void load_frame (Frame& frame) const
  { }
} DepositionWeather_syntax;

// The 'gundersen' model.

struct DepositionGundersen : public Deposition
{
  void tick (const Vegetation&, const Weather& weather, Treelog&)
  { my_deposit = weather.deposit (); }

  DepositionGundersen (const BlockModel& al)
    : Deposition (al)
  { }
};

static struct DepositionGundersenSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new DepositionGundersen (al); }
  DepositionGundersenSyntax ()
    : DeclareModel (Deposition::component, "Gundersen_et_al", "\
Currently identical to weather. Should take vegetation height into account.")
  { }
  void load_frame (Frame& frame) const
  { }
} DepositionGundersen_syntax;

// deposition.C ends here.
