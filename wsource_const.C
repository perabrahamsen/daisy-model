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
  bool is_done;
  double timestep () const
  { return timestep_hours; }
  bool end_check (symbol key) const
  { return check (key); }
  double end_number (symbol key) const
  { return number (key); }
  symbol end_name (symbol key) const
  { return name (key); }

  void source_tick (Treelog&)
  { is_done = true; }
  bool done () const
  { return is_done; }
  void source_initialize (Treelog&)
  { }
  void skip_ahead (const Time&, Treelog&)
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
      timestep_hours (Time::fraction_hours_between (begin (), end ())),
      is_done (false)
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

static struct WSourceNoneSyntax : public DeclareParam
{ 
  WSourceNoneSyntax ()
    : DeclareParam (WSource::component, "none", "const", "\
No weather.\n\
No precipitation or global radiation, temperature is 10 dg Celcius.\n\
The static information is taken from a climate station operated\n\
by Copenhagen University, and located in Taastrup, Denmark.\n\
You can overwrite any parameters to simulate an experiment under \n\
controled, constant weather conditions.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.set ("Station", "Taastrup");
    frame.set ("Elevation", 30.0);
    frame.set ("Longitude", 12.0);
    frame.set ("Latitude", 56.0);
    frame.set ("TimeZone", 15.0);
    frame.set ("Surface", "reference");
    frame.set ("ScreenHeight", 2.0);
    frame.set ("TAverage", 7.8);
    frame.set ("TAmplitude", 8.5);
    frame.set ("MaxTDay", 209.0);
    frame.set ("Precip", 0.0);
    frame.set ("GlobRad", 0.0);
    frame.set ("AirTemp", 10.0);
  }
} WSourceNone_syntax;

static struct WSourceMissingSyntax : public DeclareParam
{ 
  WSourceMissingSyntax ()
    : DeclareParam (WSource::component, "null", "const", "\
Missing weather data.\n\
Use for indicating no weather data available.")
  { }
  void load_frame (Frame&) const
  { }
} WSourceMissing_syntax;

// wsource_const.C ends here.
