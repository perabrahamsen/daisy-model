// depth.C --- Depth as a function of time.
// 
// Copyright 2005 Per Abrahamsen and KVL.
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

#include "depth.h"
#include "block.h"
#include "alist.h"
#include "time.h"
#include "plf.h"
#include "lexer_data.h"
#include "output.h"
#include "number.h"
#include "treelog.h"
#include "units.h"
#include "check.h"
#include "vcheck.h"
#include "assertion.h"
#include "librarian.h"
#include "mathlib.h"
#include "path.h"
#include "frame.h"
#include <string>
#include <sstream>

// depth component.

const char *const Depth::component = "depth";

symbol
Depth::library_id () const
{
  static const symbol id (component);
  return id;
}

Depth::Depth (Block& al)
  : name (al.name ("type"))
{ }

Depth::Depth (const symbol n)
  : name (n)
{ }

Depth::~Depth ()
{ }

// const model.

struct DepthConst : public Depth
{
  const double value;
  
  void tick (const Units&, const Time&, const Scope&, Treelog&)
  { }
  double operator()() const
  { return value; }
  void initialize (const Units&, const Scope&, Treelog&)
  { }
  virtual bool check (const Units&, const Scope&, Treelog&) const
  { return true; }
  DepthConst (Block& al)
    : Depth (al),
      value (al.number ("value"))
  { }
  DepthConst (const double height)
    : Depth (symbol ("create")),
      value (height)
  { }
  ~DepthConst ()
  { }
};

Depth*
Depth::create (const double height)
{ return new DepthConst (height); }

static struct DepthConstSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new DepthConst (al); }
  DepthConstSyntax ()
    : DeclareModel (Depth::component, "const", "Constant depth.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.add ("value", "cm", Check::non_positive (), Value::Const, 
                "Constant depth.");
    frame.order ("value");
  }
} DepthConst_syntax;

// extern model.

struct DepthExtern : public Depth
{
  // Content.
  const std::auto_ptr<Number> expr;
  double value;

  void tick (const Units& units, const Time&, const Scope& scope, Treelog& msg)
  { 
    if (!expr->tick_value (units, value, Units::cm (), scope, msg))
      if (!approximate (value, 42.0))
	{
	  msg.error ("External depth not found");
	  value = 42.0;
	}
  }

  double operator()() const
  { return value; }
    
  void initialize (const Units& units, const Scope& scope, Treelog& msg)
  { expr->initialize (units, scope, msg); }

  virtual bool check (const Units& units, const Scope& scope,
                      Treelog& msg) const
  { 
    
    bool ok = true;
    if (!expr->check_dim (units, scope, Units::cm (), msg))
      ok = false;
    return ok;
  }
  DepthExtern (Block& al)
    : Depth (al),
      expr (Librarian::build_item<Number> (al, "value")),
      value (al.number ("initial_value", -42.42e42))
  { }
  ~DepthExtern ()
  { }
};

static struct DepthExternSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new DepthExtern (al); }
  DepthExternSyntax ()
    : DeclareModel (Depth::component, "extern", "\
Look up depth in an scope.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.add_object ("value", Number::component, 
                       Value::Const, Value::Singleton, "\
Expression that evaluates to a depth.");
    frame.add ("initial_value", "cm", Check::none (), Value::OptionalConst,
		"Initial depth.");

  }
} DepthExtern_syntax;

// PLF model.

struct DepthPLF : public Depth
{
  const Time start;
  PLF value;
  double current_value;

  void  tick (const Units&, const Time& time, const Scope&, Treelog&)
  { current_value = value (Time::hours_between (start, time)); }

  double operator()() const
  { return current_value; }

  
  void initialize (const Units&, const Scope&, Treelog&)
  { }
  virtual bool check (const Units&, const Scope&, Treelog&) const
  { return true; }
  static PLF convert_to_plf (const std::vector<const AttributeList*>& table)
  {
    daisy_assert (table.size () > 0);
    const Time start (table[0]->alist ("time"));
    PLF result;
    for (size_t i = 0; i < table.size (); i++)
      {
        const Time now (table[i]->alist ("time"));
        const double value = table[i]->number ("value");
        result.add (Time::hours_between (start, now), value);
      }
    return result;
  }
  DepthPLF (Block& al)
    : Depth (al),
      start (al.alist_sequence ("table")[0]->alist ("time")),
      value (convert_to_plf (al.alist_sequence ("table"))),
      current_value (-42.42e42)
  { }
  ~DepthPLF ()
  { }
};

// GCC 2.95 can't link if this class is nested.
static const class CheckTable : public VCheck
{
  void check (const Metalib&, const Syntax&, const AttributeList& alist,
              const symbol key) const throw (std::string)
  {
    daisy_assert (alist.check (key));
        
    const std::vector<const AttributeList*>& table 
      = alist.alist_sequence (key); 
    if (table.size () < 2)
      throw std::string ("You must list at least two entries");
    Time last (table[0]->alist ("time"));
    for (size_t i = 1; i < table.size (); i++)
      {
        Time next ((table[i]->alist ("time")));
        if (next <= last)
          throw std::string ("Time must be increasing");
        last = next;
      }
  }
} check_table;

static struct DepthPLFSyntax : public DeclareModel
{
  static void entry_syntax (Frame& frame)
  {
    frame.add_submodule ("time", Value::Const, "Time.",
                          Time::load_syntax);
    frame.add ("value", "cm", Check::non_positive (), Value::Const, 
                "Depth.");
    frame.order ("time", "value");
  }
  Model* make (Block& al) const
  { return new DepthPLF (al); }
  DepthPLFSyntax ()
    : DeclareModel (Depth::component, "PLF", "Linear interpolation of depth.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.add_submodule_sequence ("table", Value::Const, 
                          "Height as a function of time.\n\
This is a list where each element has the form (TIME VALUE).\n\
The TIME entries must be increasing cronologically.  The corresponding\n\
VALUE represents the value at that time.  In order to find\n\
the depth for other times, linear interpolation between the entries\n\
in the list will be used.", entry_syntax);
    frame.add_check ("table", check_table);
    frame.order ("table");
  }
} DepthPLF_syntax;

// file model.

namespace State 
{
  enum type { uninitialized, ok, error = -1 }; 
}

struct DepthFile : public Depth
{
  Path& path;
  const symbol file;
  State::type state;
  Time start;
  PLF value;
  double current_value;

  void tick (const Units&, const Time& time, const Scope&, Treelog&)
  { 
    daisy_assert (state == State::ok);
    current_value = value (Time::hours_between (start, time)); 
  }
  double operator()() const
  { return current_value; }
  bool read_date (LexerData& lex, Time& time)
  {
      int year;
      int month;
      int day;

      year = lex.get_cardinal ();
      if (year < 0 || year > 9999)
	lex.error ("Bad year");
      if (year < 100)
	year += 1900;
      lex.skip_space ();
      month = lex.get_cardinal ();
      if (month < 1 || month > 12)
	lex.error ("Bad month");
      lex.skip_space ();
      day = lex.get_cardinal ();
      if (day < 1 || day > 31)
	lex.error ("Bad day");

      if (!Time::valid (year, month, day, 23))
	{
	  lex.error ("Bad date");
          return false;
        }
      time = Time (year, month, day, 23);
      return true;
  }
  void initialize (const Units&, const Scope&, Treelog& msg)
  { 
    daisy_assert (state == State::uninitialized);
    std::auto_ptr<std::istream> input_stream = path.open_file (file.name ());
    LexerData lex (file.name (), *input_stream, msg);
    lex.skip_space ();
    if (lex.peek () == '#')
      {
        lex.skip_line ();
        lex.next_line ();
      }

    // Start.
    read_date (lex, start);
    lex.skip_space ();
    value.add (0, lex.get_number ());
    lex.next_line ();

    int lines = 1;
    Time last = start;
    while (lex.good ())
      {
        Time next (1,1,1,1);
        if (!read_date (lex, next))
          lex.skip_line ();
        else
          {
            lex.skip_space ();
            const double height = lex.get_number ();
            if (next <= last)
              lex.error ("Time should be increasing");
            else if (height >= 0.0)
              lex.error ("Height should be negative");
            else
              {
                value.add (Time::hours_between (start, next), height);
                last = next;
                lines++;
              }
          }
        lex.next_line ();
      }
    lex.eof ();
    if (lex.error_count > 0)
      state = State::error;
    else if (lines < 2)
      {
        msg.error ("There should be at least two entries");
        state = State::error;
      }
    else 
      state = State::ok;
  }
  virtual bool check (const Units&, const Scope&, Treelog&) const
  { return state == State::ok; }
  DepthFile (Block& al)
    : Depth (al),
      path (al.path ()),
      file (al.name ("file")),
      state (State::uninitialized),
      start (1, 1, 1, 1),
      current_value (-42.42e42)
  { }
  ~DepthFile ()
  { }
};

static struct DepthFileSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new DepthFile (al); }

  DepthFileSyntax ()
    : DeclareModel (Depth::component, "file", "Linear interpolation of depth read from file.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.add ("file", Value::String, Value::Const,
                "Name of file to read data from.\n\
The format of each line in the file is 'YEAR MONTH DAY HEIGHT',\n\
where HEIGHT should in cm above ground (i.e. a negative number).\n\
Linear interpolation is used between the datapoints.");
    frame.order ("file");
  }
} DepthFile_syntax;

static struct DepthInit : public DeclareComponent 
{
  DepthInit ()
    : DeclareComponent (Depth::component, "\
Find the depth of two numbers.")
  { }
} Depth_init;

// depth.C ends here.
