// rootdens_cpedata.C -- Read Agrovand data.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
// Copyright 2007, 2008, 2009 Per Abrahamsen and KVL.
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

#include "program.h"
#include "lexer_table.h"
#include "librarian.h"
#include "assertion.h"
#include "time.h"
#include "timestep.h"
#include "mathlib.h"
#include <sstream>

namespace 
{
struct Handle
{
  // Enum in a namespace.
  enum handle_t { average, difference, sum };
private:
  handle_t value;
  static handle_t symbol2handle (symbol s);
public:
  operator handle_t () const
  { return value; }
  Handle (handle_t v)
    : value (v)
  { }
  Handle (symbol s)
    : value (symbol2handle (s))
  { }
};

Handle::handle_t
Handle::symbol2handle (symbol s)
{
  static struct sym_set_t : std::map<symbol, handle_t>
  {
    sym_set_t ()
    {
      insert (std::pair<symbol,handle_t> ("average", average));
      insert (std::pair<symbol,handle_t> ("difference", difference));
      insert (std::pair<symbol,handle_t> ("sum", sum));
    } 
  } sym_set;
  sym_set_t::const_iterator i = sym_set.find (s);
  daisy_assert (i != sym_set.end ());
  return (*i).second;
}  
}

struct ProgramCPEData : public Program
{
  const Time origin;
  const symbol day_tag;
  const symbol value_tag;
  const symbol weight_tag;
  const double factor;
  const Handle handle;
  LexerTable lex;
  int day_c;
  int value_c;
  int weight_c;

  // Use.
  bool run (Treelog& msg)
  {
    Time start = origin;
    start.tick_day (-1);
    daisy_assert (day_c >= 0);
    daisy_assert (value_c >= 0);

    std::ostringstream tmp;
    double last_value = 0.0;
    double printed_value = 0.0;
    double sum_value = 0.0;
    double sum_weight = 0.0;
    Time next = start;
    next.tick_hour (1);

    while (lex.good ())
      {
        // Read entries.
        std::vector<std::string> entries;
        if (!lex.get_entries (entries))
          {
            msg.warning ("bad line");
            continue;
          }

        daisy_assert (day_c < entries.size ());
        const std::string d = entries[day_c];
        if (lex.is_missing (d))
          {
            msg.warning ("missing day");
            continue;
          }
        const double day = lex.convert_to_double (d);

        daisy_assert (value_c < entries.size ());
        const std::string val = entries[value_c];
        if (lex.is_missing (val))
          continue;
        const double next_value = lex.convert_to_double (val);

        double weight_value = 1.0;
        if (weight_c >= 0)
          {
            daisy_assert (weight_c < entries.size ());
            const std::string weight = entries[weight_c];
            if (lex.is_missing (weight))
              {
                msg.warning ("missing weight");
                continue;
              }
            weight_value = lex.convert_to_double (weight);
          }

        const Timestep diff = next - start;
        double next_hour = diff.total_hours () / 24.0;

        if (day > next_hour)
          {
            double value;
            switch (handle)
              { 
              case Handle::difference:
                value = last_value - printed_value;
                printed_value = last_value;
                break;
              case Handle::average:
                if (sum_weight > 0)
                  value = sum_value / sum_weight;
                else
                  {
                    daisy_assert (iszero (sum_value));
                    value = 0.0;
                  }
                sum_value = sum_weight = 0.0;
                break;
              case Handle::sum:
                value = sum_value;
                sum_value = 0.0;
                break;
              default:
                daisy_notreached ();
              }
            tmp << next.year () 
                << "\t" << next.month ()
                << "\t" << next.mday ()
                << "\t" << next.hour ()
                << "\t" << value * factor
                << "\n";
            
            while (day > next_hour)
              {
                next.tick_hour ();
                const Timestep diff = next - start;
                next_hour = diff.total_hours () / 24.0;
              }
          }
        last_value = next_value;
        sum_value += next_value * weight_value;
        sum_weight += weight_value;
      }
    msg.message (tmp.str ());
    return true;
  }

  // Create and Destroy.
  void initialize (Metalib&, Block& al)
  { 
    if (lex.read_header (al.msg ()))
      {
        day_c = lex.find_tag (day_tag);
        value_c = lex.find_tag (value_tag);
      }
  }

  bool check (Treelog& msg)
  {
    bool ok = true;
    if (value_tag == day_tag)
      {
        msg.error ("Value and day tags must differ.");
        ok = false;
      }
    if (day_c == -42)
      {
        msg.error ("Read failed");
        ok = false;
      }
    else
      {
        if (day_c < 0)
          {
            msg.error ("'" + day_tag + "' not found");
            ok = false;
          }
        if (value_c < 0)
          {
            msg.error ("'" + value_tag + "' not found");
            ok = false;
          }
      }
  return ok; 
  }

  ProgramCPEData (const BlockModel& al)
    : Program (al),
      origin (al.submodel ("origin")),
      day_tag (al.name ("day")),
      value_tag (al.name ("value")),
      weight_tag (al.name ("weight", Attribute::None ())),
      factor (al.number ("factor")),
      handle (al.name ("handle")),
      lex (al),
      day_c (-42),
      value_c (-42),
      weight_c (-42)
  { }
  ~ProgramCPEData ()
  { }
};

static struct ProgramCPEDataSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ProgramCPEData (al); }
  ProgramCPEDataSyntax ()
    : DeclareModel (Program::component, "cpedata", "\
Manipulate data from Agrovand.")
  { }
  void load_frame (Frame& frame) const
  { 
    LexerTable::load_syntax (frame);
    frame.declare_submodule ("origin", Attribute::Const, "Day 1.", 
                             Time::load_syntax);
    frame.declare_string ("day", Attribute::Const, "\
Tag used for day.");
    frame.declare_string ("value", Attribute::Const, "\
Tag used for value.");
    frame.declare_string ("weight", Attribute::OptionalConst, "\
Tag used for weight.\n\
If you specify this, 'value' will be given this weight.");
    frame.declare ("factor", Attribute::None (), Attribute::Const, "\
Multiply printed value with this number.");
    frame.set ("factor", 1.0);
    frame.declare_string ("handle", Attribute::Const, "\
This option determine how the specified variable should be handled.\n\
\n\
average: print mean of values within timestep.\n\
\n\
difference: print difference between last value in timestep, and the\n\
last value in the previous timestep.\n\
\n\
sum: print sum of all values within current timestep.");
    static VCheck::Enum handle_check ("average", "difference", "sum");
    frame.set_check ("handle", handle_check);
  }
} ProgramCPEData_syntax;

// rootdens_cpedata.C ends here.
