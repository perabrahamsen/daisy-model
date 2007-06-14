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
#include "syntax.h"
#include "time.h"
#include "plf.h"
#include "lexer_data.h"
#include "treelog.h"
#include "check.h"
#include "vcheck.h"
#include "assertion.h"
#include "librarian.h"
#include <string>

// depth component.

const char *const Depth::component = "depth";

Depth::Depth (Block& al)
  : name (al.identifier ("type"))
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
  
  double operator()(const Time&) const
  { return value; }

  void initialize (Treelog&)
  { }
  virtual bool check (Treelog&) const
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

static struct DepthConstSyntax
{
  static Model& make (Block& al)
  { return *new DepthConst (al); }
  DepthConstSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Constant depth.");
    syntax.add ("value", "cm", Check::non_positive (), Syntax::Const, 
                "Constant depth.");
    syntax.order ("value");
    Librarian::add_type (Depth::component, "const", alist, syntax, &make);
  }
} DepthConst_syntax;

// PLF model.

struct DepthPLF : public Depth
{
  const Time start;
  PLF value;
  
  double operator()(const Time& time) const
  { return value (Time::hours_between (start, time)); }
  
  void initialize (Treelog&)
  { }
  virtual bool check (Treelog&) const
  { return true; }
  static PLF convert_to_plf (const std::vector<AttributeList*>& table)
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
      value (convert_to_plf (al.alist_sequence ("table")))
  { }
  ~DepthPLF ()
  { }
};

// GCC 2.95 can't link if this class is nested.
static const class CheckTable : public VCheck
{
  void check (const Syntax&, const AttributeList& alist,
              const std::string& key) const throw (std::string)
  {
    daisy_assert (alist.check (key));
        
    const std::vector<AttributeList*>& table = alist.alist_sequence (key); 
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

static struct DepthPLFSyntax
{
  static void entry_syntax (Syntax& syntax, AttributeList& alist)
  {
    syntax.add_submodule ("time", alist, Syntax::Const, "Time.",
                          Time::load_syntax);
    syntax.add ("value", "cm", Check::non_positive (), Syntax::Const, 
                "Depth.");
    syntax.order ("time", "value");
  }
  static Model& make (Block& al)
  { return *new DepthPLF (al); }
  DepthPLFSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Linear interpolation of depth.");
    syntax.add_submodule_sequence ("table", Syntax::Const, 
                          "Height as a function of time.\n\
This is a list where each element has the form (TIME VALUE).\n\
The TIME entries must be increasing cronologically.  The corresponding\n\
VALUE represents the value at that time.  In order to find\n\
the depth for other times, linear interpolation between the entries\n\
in the list will be used.", entry_syntax);
    syntax.add_check ("table", check_table);
    syntax.order ("table");
    Librarian::add_type (Depth::component, "PLF", alist, syntax, &make);
  }
} DepthPLF_syntax;

// file model.

namespace State 
{
  enum type { uninitialized, ok, error = -1 }; 
}

struct DepthFile : public Depth
{
  const std::string file;
  State::type state;
  Time start;
  PLF value;
  
  double operator()(const Time& time) const
  { 
    daisy_assert (state == State::ok);
    return value (Time::hours_between (start, time)); 
  }
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
  void initialize (Treelog& msg)
  { 
    daisy_assert (state == State::uninitialized);
    LexerData lex (file, msg);
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
  virtual bool check (Treelog&) const
  { return state == State::ok; }
  DepthFile (Block& al)
    : Depth (al),
      file (al.name ("file")),
      state (State::uninitialized),
      start (1, 1, 1, 1)
  { }
  ~DepthFile ()
  { }
};

static struct DepthFileSyntax
{
  static Model& make (Block& al)
  { return *new DepthFile (al); }

  DepthFileSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Linear interpolation of depth read from file.");
    syntax.add ("file", Syntax::String, Syntax::Const,
                "Name of file to read data from.\n\
The format of each line in the file is 'YEAR MONTH DAY HEIGHT',\n\
where HEIGHT should in cm above ground (i.e. a negative number).\n\
Linear interpolation is used between the datapoints.");
    syntax.order ("file");
    Librarian::add_type (Depth::component, "file", alist, syntax, &make);
  }
} DepthFile_syntax;

static Librarian Depth_init (Depth::component, "\
Find the depth of two numbers.");

// depth.C ends here.
