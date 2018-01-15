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
#include <istream>
#include <map>

struct ActionBBCH : public Action
{
  const Path& path;
  const symbol filename;
  const std::vector<int> interval;
  const int from;
  const int to;
  const symbol crop;
  std::unique_ptr<Action> action;

  struct When
  {
    // Enum in a namespace.
    enum when_t { first, last };
  private:
    when_t value;
    static when_t symbol2when (symbol s)
    {
      static struct sym_set_t : std::map<symbol, when_t>
      {
	sym_set_t ()
	{
	  insert (std::pair<symbol,when_t> ("first", first));
	  insert (std::pair<symbol,when_t> ("last", last));
	} 
      } sym_set;
      sym_set_t::const_iterator i = sym_set.find (s);
      daisy_assert (i != sym_set.end ());
      return (*i).second;
    }  
  public:
    operator when_t () const
    { return value; }
    When (when_t v)
      : value (v)
    { }
    When (symbol s)
      : value (symbol2when (s))
    { }
  };
  const When when;
  
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
      }
    const int want = (when == When::first) ? from : to;
    while (in_data->good ())
      {
	in_data->skip_line ();
	in_data->skip ("\n");
	if (in_data->get_cardinal () != want)
	  continue;
	in_data->skip (",");
	in_data->read_date (next);
	in_data->skip (" 00:00:00,");
	if (in_data->peek () == '-')
	  {
	    in_data->skip ("-");
	    in_data->get_cardinal ();
	  }
	else
	  {
	    Time time_end;
	    in_data->read_date (time_end);
	    in_data->skip (" 00:00:00");
	    if (when == When::last)
	      next = time_end;
	  }
	next.tick_hour (8);
	if (next < now)
	  continue;
	in_data->skip (",BBCH,");
	const symbol what = rest_of_line ();
	if (what == crop)
	  return;
      }
    throw "File error";
  }

  void doIt (Daisy& daisy, const Scope& scope, Treelog& msg)
  {
    if (next == Time::null ())
      find_next (daisy.time (), msg);
    if (daisy.time () < next)
      return;
      
    action->doIt (daisy, scope, msg);
    find_next (daisy.time (), msg);
  }

  void tick (const Daisy&, const Scope&, Treelog&)
  { }

  bool done (const Daisy&, const Scope&, Treelog&) const
  { return false; }
  
  void initialize (const Daisy&, const Scope&, Treelog&)
  { }

  bool check (const Daisy&, const Scope&, Treelog&) const
  { return true; }

  ActionBBCH (const BlockModel& al)
    : Action (al),
      path (al.path ()),
      filename (al.name ("file")),
      interval (al.integer_sequence ("interval")),
      from (interval[0]),
      to (interval[1]),
      crop (al.name ("crop")),
      action (Librarian::build_item<Action> (al, "do")),
      when (al.name ("when")),
      next (Time::null ())
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
    frame.declare_integer ("interval", Attribute::Const, 2, "\
Start and end BBCH for action.");
    frame.declare_string ("crop", Attribute::Const, "\
Name of crop to use BBCH number for.");
    frame.declare_object ("do", Action::component, "\
Action to perform.");
    frame.declare_string ("when", Attribute::Const, "\
When to do the action.\n\
\n\
first: First day within the specified interval.\n\
\n\
last: Last day within the specified interval");
    static VCheck::Enum when_check ("first", "last");
    frame.set_check ("when", when_check);
  }
} ActionBBCH_syntax;

// action_BBCH.C ends here.
