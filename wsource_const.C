// wsource_const.C -- Weather data that never changes.
// 
// Copyright 2010 KU
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

#include "wsource_base.h"
#include "librarian.h"
#include "treelog.h"
#include "time.h"
#include "frame.h"

struct WSourceConst : public WSourceBase
{
  const double timestep_hours;        // [h]
  double timestep () const
  { return timestep_hours; }
  bool end_check (symbol key) const
  { return check (key); }
  double end_number (symbol key) const
  { return number (key); }
  symbol end_name (symbol key) const
  { return name (key); }

  void source_tick (Treelog&)
  { }
  bool done () const
  { return true; }
  void source_initialize (Treelog&)
  { }
  bool source_check (Treelog& msg) const
  { 
    Treelog::Open nest (msg, __FUNCTION__);
    bool ok = true;
    if (timestep_hours < 0)
      {
        msg.error ("Period ends before it starts");
        ok = false;
      }
    return ok; 
  }
  WSourceConst (const BlockModel& al)
    : WSourceBase (al),
      timestep_hours (Time::fraction_hours_between (begin (), end ()))
  { }
  ~WSourceConst ()
  { }
};

static struct WSourceConstSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new WSourceConst (al); }
  WSourceConstSyntax ()
    : DeclareModel (WSource::component, "const", "base", "\
Weather that does not change during the simulation.")
  { }
  void load_frame (Frame&) const
  { }
} WSourceConst_syntax;

// wsource_const.C ends here.
