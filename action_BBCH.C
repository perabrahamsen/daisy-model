// action_BBCH.C -- Management based on BBCH scale read from file.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
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

#include "action.h"
#include "daisy.h"
#include "librarian.h"
#include "lexer_data.h"
#include "block_model.h"
#include "time.h"
#include "assertion.h"
#include "vcheck.h"
#include "path.h"
#include "mathlib.h"
#include "condition.h"
#include "log.h"
#include <istream>
#include <map>

struct ActionBBCH : public Action
{
  const Path& path;
  const symbol filename;
  const int from;
  const int to;
  const bool from_start;
  const bool to_start;
  const symbol crop;
  std::unique_ptr<Action> action;
  std::unique_ptr<Condition> condition;

  std::unique_ptr<std::istream> in_file;
  std::unique_ptr<LexerData> in_data;
  Time begin;
  Time end;
 
  symbol rest_of_line ()
  {
    std::string tmp;
    while (in_data->good () && in_data->peek () != '\n')
      tmp += int2char (in_data->get ());
    return tmp;
  }

  Time find_next (const Time& now, const int want, const bool start,
		  Treelog& msg)
  {
    if (!in_data.get ())
      {
	daisy_assert (!in_file.get ());
	in_file = path.open_file (filename);
	in_data.reset (new LexerData (filename, *in_file, msg));
	in_data->skip ("BBCH,startDate,endDate,scale,Crop");
      }
    while (in_data->good ())
      {
	in_data->skip_line ();
	in_data->skip ("\n");
	if (in_data->get_cardinal () != want)
	  continue;
	Time time_begin;
	Time time_end;
	in_data->skip (",");
	in_data->read_date (time_begin);
	time_begin.tick_hour (8);
	in_data->skip (" 00:00:00,");
	if (in_data->peek () == '-')
	  {
	    in_data->skip ("-");
	    in_data->get_cardinal ();
	  }
	else
	  {
	    in_data->read_date (time_end);
	    time_end.tick_hour (8);
	    in_data->skip (" 00:00:00");
	  }
	const Time next = (start || time_end == Time::null ())
	  ? time_begin
	  : time_end;
	if (next < now)
	  continue;
	in_data->skip (",BBCH,");
	const symbol what = rest_of_line ();
	if (what == crop)
	  return next;
      }
    throw "File error";
  }

  void find_interval (const Time& now, Treelog& msg)
  {
    begin = find_next (now, from, from_start, msg);
    end = find_next (now, to, to_start, msg);
  }
  void doIt (Daisy& daisy, const Scope& scope, Treelog& msg)
  {
    if (begin == Time::null ())
      find_interval (daisy.time (), msg);

    if (daisy.time () < begin)
      return;

    if (!condition->match (daisy, scope, msg)
	&& daisy.time () < end)
      return;

    action->doIt (daisy, scope, msg);
    find_interval (daisy.time (), msg);
  }

  void tick (const Daisy& daisy, const Scope& scope, Treelog& msg)
  {
    action->tick (daisy, scope, msg);
    condition->tick (daisy, scope, msg);
  }

  bool done (const Daisy&, const Scope&, Treelog&) const
  { return false; }
  
  void output (Log& log) const
  {
    output_object (action, "do", log);
    output_object (condition, "when", log);
  }

  void initialize (const Daisy& daisy, const Scope& scope, Treelog& msg)
  {
    action->initialize (daisy, scope, msg);
    condition->initialize (daisy, scope, msg);
  }

  bool check (const Daisy& daisy, const Scope& scope, Treelog& msg) const
  {
    bool ok = true; 
    if (!action->check (daisy, scope, msg))
      ok = false;
    if (!condition->check (daisy, scope, msg))
      ok = false;
    return ok;
  }

  ActionBBCH (const BlockModel& al)
    : Action (al),
      path (al.path ()),
      filename (al.name ("file")),
      from (al.integer ("from")),
      to (al.integer ("to")),
      from_start (al.flag ("from_start")),
      to_start (al.flag ("to_start")),
      crop (al.name ("crop")),
      action (Librarian::build_item<Action> (al, "do")),
      condition (Librarian::build_item<Condition> (al, "when"))
  { }
};

static struct ActionBBCHSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ActionBBCH (al); }
  ActionBBCHSyntax ()
    : DeclareModel  (Action::component, "BBCH", "\
Management according to external BBCH scale.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_string ("file", Attribute::Const, "\
Name of file where data is found.");
    frame.declare_integer ("from", Attribute::Const, "\
BBCH at beginning of interval.");
    frame.declare_integer ("to", Attribute::Const, "\
BBCH at end of interval.");
    frame.declare_boolean ("from_start", Attribute::Const, "\
If true, the interval begins when the crop enters the specified BBCH.\n\
Otherwise, the interval begins when the crop leaves the specified BBCH.");
    frame.set ("from_start", true);
    frame.declare_boolean ("to_start", Attribute::Const, "\
If true, the interval ends when the crop enters the specified BBCH.\n\
Otherwise, the interval ends when the crop leaves the specified BBCH.");
    frame.set ("to_start", false);
    frame.declare_string ("crop", Attribute::Const, "\
Name of crop to use BBCH number for.");
    frame.declare_object ("do", Action::component, "\
Action to perform.");
    frame.declare_object ("when", Condition::component, 
			  "When to perform the action.\n\
It will be performed the first time the condition is true within\n\
the specified BBCH interval, or at the end of the interval, whichever\n\
comes first. Use 'true' to perform the action at the start of the interval,\n\
or 'false' to perform it at the end.");
  }
} ActionBBCH_syntax;

// action_BBCH.C ends here.
