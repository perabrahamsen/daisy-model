// wsource_time.C -- Weather data with a time offset.
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

#include "wsource_indirect.h"
#include "librarian.h"
#include "treelog.h"
#include "timestep.h"
#include "block_model.h"
#include "submodeler.h"

struct WSourceTime : public WSourceIndirect
{
  const Timestep offset;
  Time my_data_begin;
  Time my_data_end;
  Time my_begin;
  Time my_end;
  
  const Time& data_begin () const
  { return my_data_begin; }
  const Time& data_end () const
  { return my_data_end; }
  const Time& begin () const
  { return my_begin; }
  const Time& end () const
  { return my_end; }

  void source_tick (Treelog& msg)
  {
    TREELOG_MODEL (msg);

    source->source_tick (msg);
    my_begin = source->begin () + offset;
    my_end = source->end () + offset;
  }    
  void source_initialize (Treelog& msg)
  {
    TREELOG_MODEL (msg);

    source->source_initialize (msg);
    my_data_begin = source->data_begin () + offset;
    my_data_end = source->data_end () + offset;
    my_begin = source->begin () + offset;
    my_end = source->end () + offset;
  }
  WSourceTime (const BlockModel& al)
    : WSourceIndirect (al),
      offset (al.check ("offset")
              ? submodel_value<Timestep> (al, "offset")
              : (Time (al.integer ("to"), 1, 1, 0) 
                 - Time (al.integer ("from"), 1, 1, 0)))
  { }
  ~WSourceTime ()
  { }
};

static struct WSourceTimeSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new WSourceTime (al); }
  WSourceTimeSyntax ()
    : DeclareModel (WSource::component, "time", "indirect",
                    "Change time period for weather data.\n\
\n\
You can either specify 'offset' directly, or indirectly through 'from'\n\
and 'to'.  If you chose the later, offset will be calculated January\n\
the first in the year specified with 'from' will be mapped to January\n\
the first in the year specified with 'to'.  Leap years mean that other\n\
dates may not be mapped exactly to corresponding dates in other years.")
  { }
  static bool check_alist (const Metalib&, const Frame& al, Treelog& msg)
  { 
    bool ok = true;
    
    if (al.check ("offset") == (al.check ("from") || al.check ("to")))
      {
        msg.error ("Must specify either 'offset' or 'from'/'to', but not both");
        ok = false;
      }
    if (al.check ("to") != al.check ("from"))
      {
        msg.error ("Must specify both 'from' or 'to', or neither one");
        
      }
    return ok;
  }
  void load_frame (Frame& frame) const
  { 
    frame.add_check (check_alist);
    frame.declare_submodule ("offset", Attribute::OptionalState, "\
Time offset.  Positive mean move time ahead.", Timestep::load_syntax);
    frame.declare_integer ("from", Attribute::OptionalState, "\
Year in weather data.");
    frame.set_check ("from", VCheck::valid_year ());
    frame.declare_integer ("to", Attribute::OptionalState, "\
Year in simulation.");
    frame.set_check ("to", VCheck::valid_year ());
  }
} WSourceTime_syntax;

// wsource_time.C ends here.
