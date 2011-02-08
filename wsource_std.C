// wsource_std.C -- Standard weather data file.
// 
// Copyright 2011 KU
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

#include "wsource_table.h"
#include "librarian.h"
#include "treelog.h"
#include "timestep.h"
#include "block_model.h"
#include "submodeler.h"

struct WSourceStandard : public WSourceTable
{
  bool top_level;

  bool weather_initialize (const Time& time, Treelog& msg)
  {
    bool ok = true;
    top_level = true;
    if (!WSourceTable::weather_initialize (time, msg))
      ok = false;
    top_level = false;
    return ok;
  }

  bool source_check (Treelog& msg) const
  { 
    bool ok = true;
    if (!top_level)
      {
        msg.error ("\
The '" + objid + "' model cannot be used here.  Try 'table' instead");
        ok = false;
      }
    if (!WSourceTable::source_check (msg))
      ok = false;
    return ok; 
  }
  

  WSourceStandard (const BlockModel& al)
    : WSourceTable (al),
      top_level (false)
  { }
  ~WSourceStandard ()
  { }
};

static struct WSourceStandardSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new WSourceStandard (al); }
  WSourceStandardSyntax ()
    : DeclareModel (WSource::component, "default", "table",
                    "Read weather data from specific file.")
  { }
  static bool check_alist (const Metalib&, const Frame& al, Treelog& msg)
  { 
    bool ok = true;
    return ok;
  }
  void load_frame (Frame& frame) const
  { 
    frame.add_check (check_alist);
    frame.order ("file");
  }
} WSourceStandard_syntax;

// wsource_std.C ends here.
