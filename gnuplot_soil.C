// gnuplot_time.C -- Plot soil content at specific time.
// 
// Copyright 2005 and 2010 Per Abrahamsen and KVL.
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
#include "gnuplot_base.h"
#include "block_model.h"
#include "lexer_table.h"
#include "treelog.h"
#include "mathlib.h"
#include "memutils.h"
#include "librarian.h"
#include "frame.h"
#include <sstream>

struct GnuplotSoil : public GnuplotBase
{
  // Ranges.
  std::auto_ptr<Time> at;
  const double width;
  const double depth;

  // Data.
  LexerTable lex;

  // Use.
  bool initialize (const Units& units, Treelog& msg);
  bool plot (std::ostream& out, Treelog& msg);
  
  // Create and Destroy.
  explicit GnuplotSoil (const BlockModel& al);
  ~GnuplotSoil ();
};

bool
GnuplotSoil::initialize (const Units& units, Treelog& msg)
{ 
  bool ok = true;
  return ok;
}

bool
GnuplotSoil::load (Treelog& msg)
{
  // Read header.
  if (!lex.read_header (msg))
    return false;

  if (!lex.good ())
    return false;

  // Tag.
  const int tag_c = lex.find_tag (tag);
  if (tag_c < 0)
    {
      lex.error ("Tag '" + tag + "' not found");
      return false;
    }

  // Read dimensions.
  symbol original (lex.dimension (tag_c));
  if (accumulate ())
    {
      const symbol accumulated = Units::multiply (original, timestep);
      if (accumulated != Attribute::Unknown ())
        original = accumulated;
    }
  if (original != Attribute::Unknown () && dimension_ == Attribute::Unknown ())
    dimension_ = original;
  else if (!has_factor && !units.can_convert (original, dimension_))
    {
      std::ostringstream tmp;
      tmp << "Cannot convert from [" << original << "] to [" << dimension_ 
          << "]";
      lex.error (tmp.str ());
      return false;
    }

  // Read data.
  Time last_time (9999, 12, 31, 23);
  std::vector<double> vals;

  while (lex.good ())
    {
      // Read entries.
      std::vector<std::string> entries;
      Time time (9999, 1, 1, 0);
      if (!read_entry (entries, time))
        continue;

      // Extract value.
      const std::string value = entries[tag_c];

      // Skip missing values.
      if (lex.is_missing (value))
        continue;
        
      // Convert it.
      double val = lex.convert_to_double (value);
      if (has_factor)
        val *= factor;
      else if (units.can_convert (original, dimension_, val))
        val = units.convert (original, dimension_, val);
      else 
        {
          static bool has_warned = false;
          if (!has_warned)
            {
              std::ostringstream tmp;
              tmp << "Cannot convert " << val << " from [" << original 
                  << "] to [" << dimension_ << "]";
              lex.debug (tmp.str ());
              has_warned = true;
            }
          // Treat as missing value.
          continue;
        }

      // Store it.
      if (time != last_time)
	{
          if (vals.size () > 0)
            add_entry (last_time, vals);
	  last_time = time;
	}
      vals.push_back (val);
    }
  if (vals.size () > 0)
    add_entry (last_time, vals);

  // Done.
  return true;
}

bool
GnuplotSoil::plot (std::ostream& out, Treelog& msg)
{ 
  // Read data.
  

  // Header.
  plot_header (out);
  out << "\
set pm3d map\n\
set size ratio -1\n";

  // Legend.
  if (legend != "auto")
    out << "set key " << legend_table[legend] << "\n";

  // Range
  out << "set xrange [0:" << width << "]\n";
  out << "set yrange [" << depth << ":0]\n";
  
  // Extra.
  for (size_t i = 0; i < extra.size (); i++)
    out << extra[i].name () << "\n";

  // Plot.
  out << "splot '-' using 1:2:3\n";
  
  // Data.
  for (size_t i = 0; i < source.size (); i++)
    {
      if (source[i]->value ().size () < 1)
        continue;
      const bool use_ebars = source[i]->with () == "errorbars";
      const size_t size = source[i]->time ().size ();
      daisy_assert (size == source[i]->value ().size ());
      bool begun = (begin.get () == NULL);
      double last = 0.0;
      const bool accumulate = source[i]->accumulate ();
      for (size_t j = 0; j < size; j++)
        {
          // 
          const Time time = source[i]->time ()[j];
          if (!begun)
            {
              if (time >= *begin)
                // Begin logging.
                begun = true;
              else 
                // To early, skip entry.
                continue;
            }
          else if (end.get () && time > *end)
            // Finished.
            break;

          const double value = source[i]->value ()[j];
          if (accumulate)
            last += value;
          else
            last = value;
          out << time.year () << "-" << time.month () << "-" << time.mday ()
              << "T" << time.hour () << "\t" << last;
	  if (use_ebars)
	    out << "\t" << source[i]->ebar ()[j];
	  out << "\n";
        }
      out << "e\n";
    }

  // The end.
  if (interactive ())
    out << "pause mouse\n";

  return true;
}

GnuplotSoil::GnuplotSoil (const BlockModel& al)
  : GnuplotBase (al),
    begin (al.check ("begin") 
	   ? new Time (al.submodel ("begin")) 
	   : NULL),
    end (al.check ("end")
	 ? new Time (al.submodel ("end")) 
	 : NULL),
    ymin_flag (al.check ("ymin")),
    ymin (al.number ("ymin", 42.42e42)),
    ymax_flag (al.check ("ymax")),
    ymax (al.number ("ymax", 42.42e42)),
    y2min_flag (al.check ("y2min")),
    y2min (al.number ("y2min", 42.42e42)),
    y2max_flag (al.check ("y2max")),
    y2max (al.number ("y2max", 42.42e42)),
    source (Librarian::build_vector<Source> (al, "source"))
{ }

GnuplotSoil::~GnuplotSoil ()
{ 
  sequence_delete (source.begin (), source.end ()); 
}

static struct GnuplotSoilSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new GnuplotSoil (al); }
  GnuplotSoilSyntax ()
    : DeclareModel (Gnuplot::component, "time", "common",
               "Generate a gnuplot graph with times series.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_submodule ("begin", Attribute::OptionalConst,
			  "First date at x-axis.", Time::load_syntax);
    frame.declare_submodule ("end", Attribute::OptionalConst,
			  "Last date at x-axis.", Time::load_syntax);
    frame.declare ("ymin", Attribute::User (), Attribute::OptionalConst, "\
Fixed lowest value on left y-axis.\n\
By default determine this from the data.");
    frame.declare ("ymax", Attribute::User (), Attribute::OptionalConst, "\
Fixed highest value on right y-axis.\n\
By default determine this from the data.");
    frame.declare ("y2min", Attribute::User (), Attribute::OptionalConst, "\
Fixed lowest value on left y-axis.\n\
By default determine this from the data.");
    frame.declare ("y2max", Attribute::User (), Attribute::OptionalConst, "\
Fixed highest value on right y-axis.\n\
By default determine this from the data.");
                
    frame.declare_object ("source", Source::component, Attribute::State, 
                       Attribute::Variable, "\
Time series to plot.");
  }
} GnuplotSoil_syntax;
