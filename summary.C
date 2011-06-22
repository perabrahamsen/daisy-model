// summary.C
// 
// Copyright 2003 Per Abrahamsen and KVL.
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

#include "summary.h"
#include "block_model.h"
#include "librarian.h"

const char *const Summary::component = "summary";

symbol
Summary::library_id () const
{ return component; }

void 
Summary::find_scopes (std::vector<const Scope*>&) const
{ }

void 
Summary::tick (const Time&)
{ }

bool
Summary::check (Treelog&) const
{ return true; }

Summary::Summary (const BlockModel& al)
  : objid (al.check ("title") ? al.name ("title") : al.type_name ())
{ }

Summary::~Summary ()
{ }

static struct SummaryInit : public DeclareComponent 
{
  SummaryInit ()
    : DeclareComponent (Summary::component, "\
Summary reports for log parameterizations.")
  { }
  void load_frame (Frame& frame) const
  { 
    Model::load_model (frame); 
    frame.declare_string ("title", Attribute::OptionalConst,
		  "Title of this summary.\n\
By default, use the name of the parameterization.");
  }
} Summary_init;

