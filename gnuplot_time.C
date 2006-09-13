// gnuplot_time.C -- 2D plot with Daisy time on the X axes. 
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

#include "gnuplot_base.h"
#include "source.h"
#include "treelog.h"
#include "mathlib.h"
#include "memutils.h"
#include <sstream>

struct GnuplotTime : public GnuplotBase
{
  // Ranges.
  std::auto_ptr<Time> begin;
  std::auto_ptr<Time> end;
  const bool ymin_flag;
  const double ymin;
  const bool ymax_flag;
  const double ymax;
  const bool y2min_flag;
  const double y2min;
  const bool y2max_flag;
  const double y2max;

  // Source.
  const std::vector<Source*> source;

  // Use.
  static std::string timeform (const Time& time);
  bool initialize (Treelog& msg);
  bool plot (std::ostream& out, Treelog& msg);
  
  // Create and Destroy.
  explicit GnuplotTime (Block& al);
  ~GnuplotTime ();
};

std::string
GnuplotTime::timeform (const Time& time)
{
  std::ostringstream tmp;
  tmp << "\"" << time.year () << "-" << time.month () << "-" << time.mday ()
      << "T" << time.hour () << "\"";
  return tmp.str ();
}

bool
GnuplotTime::initialize (Treelog& msg)
{ 
  bool ok = true;
  for (size_t i = 0; i < source.size(); i++)
    {
      std::ostringstream tmp;
      tmp << name << "[" << i << "]: " << source[i]->name 
          << " '" << source[i]->title () << "'";
      Treelog::Open nest (msg, tmp.str ());
      if (!source[i]->load (msg))
        ok = false;
      else if (source[i]->value ().size () < 1)
        msg.error ("No data in plot, ignoring");
    }
  return ok;
}

bool
GnuplotTime::plot (std::ostream& out, Treelog& msg)
{ 
  // Header.
  plot_header (out);
  out << "\
set xtics nomirror autofreq\n\
set ytics nomirror\n\
set xdata time\n\
set timefmt \"%Y-%m-%dT%H\"\n\
set style data lines\n";

  // Removed: set format x "%m-%y"

  // Dimensions.
  std::vector<symbol> dims;
  std::vector<int> axis;
  for (size_t i = 0; i < source.size (); i++)
    {
      if (source[i]->value ().size () < 1)
        {
          axis.push_back (-42);
          continue;
        }

      const symbol dim = source[i]->dimension ();
      
      for (size_t j = 0; j < dims.size (); j++)
        if (dim == dims[j])
          {
            axis.push_back (j);
            goto cont2;
          }
      axis.push_back (dims.size ());
      dims.push_back (dim);
    cont2: ;
    }
  switch (dims.size ())
    {
    case 2:
      out << "set y2tics\n";
      out << "set y2label " << quote (dims[1]) << "\n";
      out << "set ylabel " << quote (dims[0]) << "\n";
      break;
    case 1:
      out << "unset y2tics\n";
      out << "unset y2label\n";
      out << "set ylabel " << quote (dims[0]) << "\n";
      break;
    default:
      msg.error ("Can only plot one or two units at a time");
      return false;
    }

  // Legend.
  if (legend == "auto")
    {
      // Find ranges.
      Time soft_begin (9999, 12, 31, 23);
      Time soft_end (1,1,1,0);
      double soft_ymin = 1e99;
      double soft_ymax = -soft_ymin;
      double soft_y2min = soft_ymin;
      double soft_y2max = soft_ymax;

      for (size_t i = 0; i < source.size (); i++)
        if (source[i]->value ().size () < 1)
          /**/;
        else if (axis[i] == 0)
	  source[i]->limit (soft_begin, soft_end, soft_ymin, soft_ymax);
	else
	  source[i]->limit (soft_begin, soft_end, soft_y2min, soft_y2max);

      if (begin.get ())
	soft_begin = *begin;
      if (end.get ())
	soft_end = *end;
      if (ymin_flag)
	soft_ymin = ymin;
      if (ymax_flag)
	soft_ymax = ymax;
      if (y2min_flag)
	soft_y2min = y2min;
      if (y2max_flag)
	soft_y2max = y2max;

      // Find distances.
      double nw = 1.0;
      double ne = 1.0;
      double sw = 1.0;
      double se = 1.0;
      for (size_t i = 0; i < source.size (); i++)
	if (source[i]->value ().size () < 1)
          /**/;
        else if (axis[i] == 0)
	  source[i]->distance (soft_begin, soft_end, soft_ymin, soft_ymax,
			       nw, ne, sw, se);
	else
	  source[i]->distance (soft_begin, soft_end, soft_y2min, soft_y2max,
			       nw, ne, sw, se);

      // Choose closest.
      const double max_distance = std::max (std::max (nw, ne), 
					    std::max (sw, se));
      if (max_distance < 0.05)
	legend = "outside";
      else if (approximate (max_distance, ne))
	legend = "ne";
      else if (approximate (max_distance, nw))
	legend = "nw";
      else if (approximate (max_distance, se))
	legend = "se";
      else
	{
	  daisy_assert (approximate (max_distance, sw));
	  legend = "sw";
	}
    }
  out << "set key " << legend_table[legend] << "\n";

  // X Range
  out << "set xrange [";
  if (begin.get ())
    out << timeform (*begin);
  else
    out << "*";
  out << ":";
  if (end.get ())
    out << timeform (*end);
  else
    out << "*";
  out << "]\n";
  out << "unset x2range\n";

  // Y range
  out << "set yrange [";
  if (ymin_flag)
    out << ymin;
  else
    out << "*";
  out << ":";
  if (ymax_flag)
    out << ymax;
  else
    out << "*";
  out << "]\n";
  if (dims.size () == 2)
    {
      out << "set y2range [";
      if (y2min_flag)
        out << y2min;
      else
        out << "*";
      out << ":";
      if (y2max_flag)
        out << y2max;
      else
        out << "*";
      out << "]\n";
    }
  else
    out << "unset y2range\n";

  // Extra.
  for (size_t i = 0; i < extra.size (); i++)
    out << extra[i].name () << "\n";

  // Plot.
  out << "plot ";
  int points = 0;
  int lines = 0;
  bool first = true;
  daisy_assert (axis.size () == source.size ());
  for (size_t i = 0; i < source.size (); i++)
    {
      if (source[i]->value ().size () < 1)
        continue;
      const std::string with = source[i]->with ();
      if (first)
        first = false;
      else
        out << ", ";
      out << "'-' using 1:2";
      if (with == "errorbars")
	out << ":3";
      out << " title " << quote (source[i]->title ());
      if (axis[i] == 1)
	out << " axes x1y2";
      else
	daisy_assert (axis[i] == 0);
      out << " with ";	
      const int style = source[i]->style ();
      out << with;
      if (with == "points" || with == "errorbars")
	out << " " << (style < 0 ? ++points : ((style == 0) ? points : style));
      else if (with == "lines")
	out << " " << (style < 0 ? ++lines :  ((style == 0) ? lines : style));
      else 
	{
	  if (style >= 0)
	    out << " " << style;
	}
    }
  out << "\n";
  
  // Data.
  for (size_t i = 0; i < source.size (); i++)
    {
      if (source[i]->value ().size () < 1)
        continue;
      const bool use_ebars = source[i]->with () == "errorbars";
      const size_t size = source[i]->time ().size ();
      daisy_assert (size == source[i]->value ().size ());
      for (size_t j = 0; j < size; j++)
        {
          const Time time = source[i]->time ()[j];
          out << time.year () << "-" << time.month () << "-" << time.mday ()
              << "T" << time.hour () << "\t" << source[i]->value ()[j];
	  if (use_ebars)
	    out << "\t" << source[i]->ebar ()[j];
	  out << "\n";
        }
      out << "e\n";
    }

  // The end.
  if (device == "screen")
    out << "pause mouse\n";

  return true;
}

GnuplotTime::GnuplotTime (Block& al)
  : GnuplotBase (al),
    begin (al.check ("begin") 
	   ? new Time (al.alist ("begin")) 
	   : NULL),
    end (al.check ("end")
	 ? new Time (al.alist ("end")) 
	 : NULL),
    ymin_flag (al.check ("ymin")),
    ymin (al.number ("ymin", 42.42e42)),
    ymax_flag (al.check ("ymax")),
    ymax (al.number ("ymax", 42.42e42)),
    y2min_flag (al.check ("y2min")),
    y2min (al.number ("y2min", 42.42e42)),
    y2max_flag (al.check ("y2max")),
    y2max (al.number ("y2max", 42.42e42)),
    source (Librarian<Source>::build_vector (al, "source"))
{ }

GnuplotTime::~GnuplotTime ()
{ 
  sequence_delete (source.begin (), source.end ()); 
}

static struct GnuplotTimeSyntax
{
  static Gnuplot& make (Block& al)
  { return *new GnuplotTime (al); }
  GnuplotTimeSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    GnuplotBase::load_syntax (syntax, alist);

    alist.add ("description",
               "Generate a gnuplot graph with times series."); 

    syntax.add_submodule ("begin", alist, Syntax::OptionalConst,
			  "First date at x-axis.", Time::load_syntax);
    syntax.add_submodule ("end", alist, Syntax::OptionalConst,
			  "Last date at x-axis.", Time::load_syntax);
    syntax.add ("ymin", Syntax::User (), Syntax::OptionalConst, "\
Fixed lowest value on left y-axis.\n\
By default determine this from the data.");
    syntax.add ("ymax", Syntax::User (), Syntax::OptionalConst, "\
Fixed highest value on right y-axis.\n\
By default determine this from the data.");
    syntax.add ("y2min", Syntax::User (), Syntax::OptionalConst, "\
Fixed lowest value on left y-axis.\n\
By default determine this from the data.");
    syntax.add ("y2max", Syntax::User (), Syntax::OptionalConst, "\
Fixed highest value on right y-axis.\n\
By default determine this from the data.");
                
    syntax.add ("source", Librarian<Source>::library (), Syntax::State, 
		Syntax::Sequence, "\
Time series to plot.");
    Librarian<Gnuplot>::add_type ("time", alist, syntax, &make);
  }
} GnuplotTime_syntax;
