// groundwater_file.C
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
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


#include "groundwater.h"
#include "block.h"
#include "alist.h"
#include "lexer_data.h"
#include "time.h"
#include <fstream>

using namespace std;

class GroundwaterFile : public Groundwater
{
  // Data.
private:
  Time previous_time;
  Time next_time;
  double previous_depth;
  double next_depth;
  double depth;

  // File.
  const string file_name;
  LexerData* lex;
  
  // Groundwater.
public:
  bottom_t bottom_type () const;
  double q_bottom () const
  { daisy_notreached (); }

  // Simulation.
public:
  void tick (const Geometry&,
             const Soil&, SoilWater&, double, const SoilHeat&,
	     const Time& time, Treelog& msg)
  { tick (time, msg); }
  void tick (const Time&, Treelog&);
  double table () const;

  // Create and Destroy.
public:
  void initialize (const Output&, const Geometry&, const Time& time, Treelog&);
  GroundwaterFile (Block&);
  ~GroundwaterFile ();
};

Groundwater::bottom_t
GroundwaterFile::bottom_type () const
{
  if (depth > 0)	     // Positive numbers indicate flux bottom.
    return free_drainage;
  else
    return pressure;
}

void
GroundwaterFile::tick (const Time& time, Treelog&)
{
  daisy_assert (lex);
  while (next_time < time)
    {
      if (!lex->good ())
	throw ("groundwater file read error");

      // Remember old value.
      previous_time = next_time;
      previous_depth = next_depth;

      // Read in new value.
      int year;
      int month;
      int day;

      year = lex->get_cardinal ();
      if (year < 0 || year > 9999)
	lex->error ("Bad year");
      if (year < 100)
	year += 1900;
      lex->skip_space ();
      month = lex->get_cardinal ();
      if (month < 1 || month > 12)
	lex->error ("Bad month");
      lex->skip_space ();
      day = lex->get_cardinal ();
      if (day < 1 || day > 31)
	lex->error ("Bad day");
      lex->skip_space ();
      next_depth = lex->get_number ();
      if (next_depth > 0.0)
	lex->error ("positive depth");
      if (!Time::valid (year, month, day, 23))
	{
	  lex->error ("Bad date");
	  lex->skip_line ();
	  lex->next_line ();
	  next_time = previous_time;
	  next_depth = previous_depth;
	  continue;
	}

      lex->next_line ();
      next_time = Time (year, month, day, 23);
    }
  // We should be somewhere in the interval.
  daisy_assert (previous_time < time || time == previous_time);
  daisy_assert (time < next_time  || time == next_time);

  // Interpolate depth values.
  const double total_interval = Time::days_between (previous_time, next_time);
  const double covered_interval = Time::days_between (previous_time, time);
  const double covered_fraction = covered_interval / total_interval;
  const double total_change = next_depth - previous_depth;
  depth = previous_depth + covered_fraction * total_change;
}

double
GroundwaterFile::table () const
{
  return depth;
}

void
GroundwaterFile::initialize (const Output&, const Geometry&, const Time& time,
			     Treelog& err)
{
  daisy_assert (lex == NULL);
  lex = new LexerData (file_name, err);
  tick (time, err); 
}

GroundwaterFile::GroundwaterFile (Block& al)
  : Groundwater (al),
    previous_time (42, 1, 1, 0),
    next_time (42, 1, 1, 0),
    previous_depth (-42.42e42),
    next_depth (-42.42e42),
    depth (-42.42e42),
    file_name (al.name ("file")),
    lex (NULL)
{ }

GroundwaterFile::~GroundwaterFile ()
{ 
  delete lex;
}

static struct GroundwaterFileSyntax
{
  static Groundwater& make (Block& al)
    { return *new GroundwaterFile (al); }
  GroundwaterFileSyntax ()
    { 
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "Read groundwater table from a file.");
      Groundwater::load_syntax (syntax, alist);
      syntax.add ("file", Syntax::String, Syntax::Const,
		  "Name of file to read data from.\n\
The format of each line in the file is 'YEAR MONTH DAY HEIGHT',\n\
where HEIGHT should in cm above ground (i.e. a negative number).\n\
Linear interpolation is used between the datapoints.");
      syntax.order ("file");
      Librarian<Groundwater>::add_type ("file", alist, syntax, &make);
    }
} GroundwaterFile_syntax;


