// log_bbch.C -- Read BBCH numbers from external file.
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
#include "log.h"
#include "daisy.h"
#include "assertion.h"
#include "path.h"
#include "treelog.h"
#include "block_model.h"
#include "librarian.h"
#include "lexer_data.h"
#include "mathlib.h"
#include <fstream>
#include <sstream>

// A 'dummy' log that doesn't descent.

struct LogDummy : public Log
{
 // Filter function.
  bool check_leaf (symbol) const
  { return false; }
  bool check_interior (symbol) const
  { return false; }
  bool check_derived (symbol, symbol, const symbol) const
  { return false; }

  void done (const std::vector<Time::component_t>& time_columns,
	     const Time&, const double, Treelog&)
  { daisy_notreached (); }

  bool initial_match (const Daisy&, const Time& previous, Treelog&)
  { return false; }
  void initial_done (const std::vector<Time::component_t>& time_columns,
		     const Time&, Treelog&)
  { daisy_notreached (); }

  // Normal items.
  void open (symbol)
  { daisy_notreached (); }
  void close ()
  { daisy_notreached (); }

  // Unnamed items.
  void open_unnamed ()
  { daisy_notreached (); }
  void close_unnamed ()
  { daisy_notreached (); }

  // Derived items.
  void open_derived (symbol, symbol, const symbol)
  { daisy_notreached (); }
  void close_derived ()
  { daisy_notreached (); }

  // Derived items with their own alist
  void open_object (symbol, symbol, const Frame&, const symbol)
  { daisy_notreached (); }
  void close_object ()
  { daisy_notreached (); }

  // Derived items in a list.
  void open_entry (symbol, const Frame&, const symbol)
  { daisy_notreached (); }
  void close_entry ()
  { daisy_notreached (); }

  // Named derived items in a list.
  void open_named_entry (symbol, symbol, const Frame&)
  { daisy_notreached (); }
  void close_named_entry ()
  { daisy_notreached (); }

  // Named object
  void open_shallow (symbol, const symbol)
  { daisy_notreached (); }
  void close_shallow ()
  { daisy_notreached (); }

  void output_entry (symbol, bool)
  { }
  void output_entry (symbol, double)
  { }
  void output_entry (symbol, int)
  { }
  void output_entry (symbol, symbol)
  { }
  void output_entry (symbol, const std::vector<double>&)
  { }
  void output_entry (symbol, const PLF&)
  { }

  LogDummy (const BlockModel& al)
    : Log (al)
  { }

  ~LogDummy ()
  { }
};

// The 'BBCH' log model.

struct LogBBCH : public LogDummy, 
		 public Scope
{
  void find_scopes (std::vector<const Scope*>& scopes) const
  { scopes.push_back (this); }

  // Content.
  const Path& path;
  const symbol file;
  const symbol crop;
  
  Time now;
  std::unique_ptr<std::istream> in;
  std::unique_ptr<LexerData> lex;
  int bbch_read;
  int bbch_use;
  Time time_begin;
  Time time_end;
  
  symbol rest_of_line ()
  {
    std::string tmp;
    while (lex->good () && lex->peek () != '\n')
      tmp += int2char (lex->get ());
    return tmp;
  }


  // Checking to see if we should log this time step.
  bool match (const Daisy& daisy, Treelog& msg)
  {
    now = daisy.time ();

    while (lex->good () && (time_end == Time::null () || time_end < now))
      {
	lex->skip_line ();
	lex->skip ("\n");
	bbch_read = lex->get_cardinal ();
	lex->skip (",");
	lex->read_date (time_begin);
	lex->skip (" 00:00:00,");
	if (bbch_read == 92)
	  {
	    // End date is (sometimes) junk for bbch 92.
	    time_end == Time::null ();
	    while (lex->good () && lex->peek () != ',')
	      lex->get ();
	  }
	else
	  {
	    lex->read_date (time_end);
	    lex->skip (" 00:00:00");
	  }
	lex->skip (",BBCH,");
	const symbol what = rest_of_line ();
	
	if (what != crop)
	  time_begin = Time::null ();
      }
    if (!lex->good ())
      bbch_use = -99;
    else if (now >= time_begin)
      bbch_use = bbch_read;
    else
      bbch_use = bbch_read - 1;
    return false;
  }

  // Scope
  static symbol bbch_name ()
  {
    static const symbol name = "BBCH";
    return name;
  }
  symbol title () const
  { return objid; }
  // void tick (const Scope&, Treelog&);
  void entries (std::set<symbol>& entries) const
  { entries.insert (bbch_name ()); }
  Attribute::type lookup (symbol tag) const
  { return (tag == bbch_name ()) ? Attribute::Number : Attribute::Error; }
  int type_size (symbol tag) const
  { return Attribute::Singleton; }
  int value_size (symbol tag) const
  { return Attribute::Singleton; }
  bool check (symbol tag) const
  { return tag == bbch_name (); }
  double number (symbol) const
  { return bbch_use; }
  symbol dimension (symbol) const
  { return Attribute::None (); }
  symbol name (symbol) const
  { daisy_notreached (); }
  symbol description (symbol) const
  {
    static const symbol desc = "\
BBCH number read from file.";
    return desc;
  }

  // Create and Destroy.
  void initialize (const symbol, const symbol, Treelog& msg)
  { 
    in = path.open_file (file);
    lex = std::make_unique<LexerData> (file, *in, msg);
    lex->skip ("BBCH,startDate,endDate,scale,Crop");
    msg.message ("Opened BBCH file");
  }

  bool check (const Border&, Treelog& msg) const
  { 
    TREELOG_MODEL (msg);
    bool ok = true;
    if (!lex->good ())
      ok = false;
    return ok; 
  }

  LogBBCH (const BlockModel& al)
    : LogDummy (al),
      path (al.path ()),
      file (al.name ("file")),
      crop (al.name ("crop")),
      bbch_read (-99),
      bbch_use (-99),
      time_begin (Time::null ()),
      time_end (Time::null ())
  { }
  ~LogBBCH ()
  { }
};

static struct LogBBCHSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new LogBBCH (al); }

  LogBBCHSyntax ()
    : DeclareModel (Log::component, "BBCH", "Read BBCH stages from file.")
  { }
  void load_frame (Frame& frame) const
  {  
    frame.declare_string ("file", Attribute::Const,
			  "Name of file with BBCH data.");
    frame.declare_string ("crop", Attribute::Const,
			  "Name of crop in BBCH file to use.");
  }
} LogBBCH_syntax;

// log_bbch.C ends here.
