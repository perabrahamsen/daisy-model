// condition_BBCH.C -- Wait for BBCH number.
// 
// Copyright 2017 KU
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

#include "condition.h"
#include "block_model.h"
#include "librarian.h"
#include "frame.h"
#include "lexer_data.h"
#include "time.h"
#include "mathlib.h"
#include "path.h"
#include "daisy.h"
#include "treelog.h"
#include <istream>

struct ConditionBBCH : public Condition
{
  const Path& path;
  const symbol filename;
  const int bbch;
  const bool start;
  const symbol crop;

  std::unique_ptr<std::istream> in_file;
  std::unique_ptr<LexerData> in_data;
  Time next;

  symbol rest_of_line ()
  {
    std::string tmp;
    while (in_data->good () && in_data->peek () != '\n')
      tmp += int2char (in_data->get ());
    return tmp;
  }

  void find_next (const Time& now, Treelog& msg)
  {
    if (!in_data.get ())
      {
	daisy_assert (!in_file.get ());
	in_file = path.open_file (filename);
	in_data.reset (new LexerData (filename, *in_file, msg));
	in_data->skip ("BBCH,startDate,endDate,scale,Crop");
	msg.message ("Opened BBCH file");
      }
    while (in_data->good ())
      {
	in_data->skip_line ();
	in_data->skip ("\n");
	if (in_data->get_cardinal () != bbch)
	  continue;
	Time time_begin;
	Time time_end;
	in_data->skip (",");
	in_data->read_date (time_begin);
	in_data->skip (" 00:00:00,");
	if (in_data->peek () == '-')
	  {
	    in_data->skip ("-");
	    in_data->get_cardinal ();
	  }
	else
	  {
	    in_data->read_date (time_end);
	    in_data->skip (" 00:00:00");
	  }
	next = (start || time_end == Time::null ())
	  ? time_begin
	  : time_end;
	if (next < now)
	  continue;
	in_data->skip (",BBCH,");
	const symbol what = rest_of_line ();
	if (what == crop)
	  return;
      }
    throw "File error";
  }

  bool match (const Daisy& daisy, const Scope&, Treelog&) const
  {
    daisy_assert (next != Time::null ());
    return daisy.time () == next;
  }
  void output (Log&) const
  { }
  void tick (const Daisy& daisy, const Scope&, Treelog& msg)
  {
    if (next == Time::null () || daisy.time () > next)
      {
	find_next (daisy.time (), msg);
	msg.message ("Next time: " + next.print ());
      }
  }
  void initialize (const Daisy&, const Scope&, Treelog&)
  { }
  bool check (const Daisy&, const Scope&, Treelog&) const
  { return true; }

  ConditionBBCH (const BlockModel& al)
    : Condition (al),
      path (al.path ()),
      filename (al.name ("file")),
      bbch (al.integer ("bbch")),
      start (al.flag ("start")),
      crop (al.name ("crop")),
      next (Time::null ())
  { }
};

static struct ConditionBBCHSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ConditionBBCH (al); }
  ConditionBBCHSyntax ()
    : DeclareModel (Condition::component, "BBCH_after", "\
True after specified BBCH.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_string ("file", Attribute::Const, "\
Name of file where data is found.");
    frame.declare_integer ("bbch", Attribute::Const, 
			   "BBCH to test for.");
    frame.declare_boolean ("start", Attribute::Const, "\
If true, the interval begins when the crop enters the specified BBCH.\n\
Otherwise, the interval begins when the crop leaves the specified BBCH.");
    frame.declare_string ("crop", Attribute::Const, "\
Name of crop to use BBCH number for.");
  }
} ConditionBBCH_syntax;

// condition_BBCH.C ends here.
