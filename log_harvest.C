// log_harvest.C
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


#include "log.h"
#include "daisy.h"
#include "harvest.h"
#include "version.h"
#include <fstream>
#include <time.h>

struct LogHarvest : public Log
{
  // Filter function.
  bool check_member (const string&) const
  { return false; }
  bool check_derived (const string&, const string&, const Library&) const
  { return false; }

  // Content.
  unsigned int last_size;
  string file;			// Filename.
  ofstream out;			// Output stream.
  bool print_header;		// Set if header should be printed.
  bool print_tags;		// Set if tags should be printed.
  bool print_dimension;		// Set if dimensions should be printed.
  const bool print_N;		// Set if nitrogen content should be printed.
  const bool print_C;		// Set if carbon content should be printed.

  // Checking to see if we should log this time step.
  bool match (const Daisy& daisy, Treelog&)
  {
    if (print_header)
      {
	print_dlf_header (out, daisy.alist);
	print_header = false;
      }
    if (print_tags)
      {
	out << "year\tmonth\tday\tcolumn\tcrop"
	    << "\tstem_DM\tdead_DM\tleaf_DM\tsorg_DM";
	if (print_N)
	  out << "\tstem_N\tdead_N\tleaf_N\tsorg_N";
	if (print_C)
	  out << "\tstem_C\tdead_C\tleaf_C\tsorg_C";
	out << "\n";
	print_tags = false;
      }
    if (print_dimension)
      {
	out << "\t\t\t\t"
	    << "\tt/ha\tt/ha\tt/ha\tt/ha";
	if (print_N)
	  out << "\tkg/ha\tkg/ha\tkg/ha\tkg/ha";
	if (print_C)
	  out << "\tkg/ha\tkg/ha\tkg/ha\tkg/ha";
	out << "\n";
	print_dimension = false;
      }
    for (; last_size < daisy.harvest.size (); last_size++)
      {
	const Harvest& harvest = *(daisy.harvest[last_size]);
	out << harvest.time.year ()
	    << "\t" << harvest.time.month ()
	    << "\t" << harvest.time.mday ()
	    << "\t" << harvest.column
	    << "\t" << harvest.crop
	    << "\t" << harvest.stem_DM * 0.01
	    << "\t" << harvest.dead_DM * 0.01
	    << "\t" << harvest.leaf_DM * 0.01
	    << "\t" << harvest.sorg_DM * 0.01;
	if (print_N)
	  out << "\t" << harvest.stem_N * 10.0
	      << "\t" << harvest.dead_N * 10.0
	      << "\t" << harvest.leaf_N * 10.0
	      << "\t" << harvest.sorg_N * 10.0;
	if (print_C)
	  out << "\t" << harvest.stem_C * 10.0
	      << "\t" << harvest.dead_C * 10.0
	      << "\t" << harvest.leaf_C * 10.0
	      << "\t" << harvest.sorg_C * 10.0;
	out << "\n";
	out.flush ();
      }
    return false;
  }

  void done ()
  { daisy_assert (false); }

  // Normal items.
  void open (const string&)
  { daisy_assert (false); }
  void close ()
  { daisy_assert (false); }

  // Unnamed items.
  void open_unnamed ()
  { daisy_assert (false); }
  void close_unnamed ()
  { daisy_assert (false); }

  // Derived items.
  void open_derived (const string&, const string&)
  { daisy_assert (false); }
  void close_derived ()
  { daisy_assert (false); }

  // Derived items in a list.
  void open_entry (const string&, const AttributeList&)
  { daisy_assert (false); }
  void close_entry ()
  { daisy_assert (false); }

  // Named derived items in a list.
  void open_named_entry (const string&, const string&, const AttributeList&)
  { daisy_assert (false); }
  void close_named_entry ()
  { daisy_assert (false); }

  void output (const string&, const Time&)
  { }
  void output (const string&, const bool)
  { }
  void output (const string&, const double)
  { }
  void output (const string&, const int)
  { }
  void output (const string&, const string&)
  { }
  void output (const string&, const vector<double>&)
  { }
  void output (const string&, const PLF&)
  { }

  // Create and Destroy.
  bool check (const Syntax&, Treelog&) const
  { return true; }

  LogHarvest (const AttributeList& al)
    : Log (al),
      last_size (0),
      file (al.name ("where")),
#ifdef BORLAND_PERMISSIONS
      out (file.c_str (), ios::out|ios::trunc, 0666),
#else
      out (file.c_str ()),
#endif
      print_header (al.flag ("print_header")),
      print_tags (al.flag ("print_tags")),
      print_dimension (al.flag ("print_dimension")),
      print_N (al.flag ("print_N")),
      print_C (al.flag ("print_C"))
  {
    // Header.
    if (print_header)
      {
	out << "dlf-0.0 -- harvest\n\n";
	out << "VERSION: " << version  << "\n";
	out << "FILE: " << file  << "\n";
	time_t now = time (NULL);
	out << "RUN: " << ctime (&now) << "\n";
      }
    out.flush ();
  }

  ~LogHarvest ()
  {
    if (!out.good ())
      throw (string ("Problems writing to '") + file + "'");
  }
};

static struct LogHarvestSyntax
{
  static Log& make (const AttributeList& al)
  { return *new LogHarvest (al); }

  LogHarvestSyntax ()
  {  
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Create a log of all harvests.");
    syntax.add ("where", Syntax::String, Syntax::Const,
		"Name of the log file to create.");
    alist.add ("where", "harvest.dlf");
    syntax.add ("print_header", Syntax::Boolean, Syntax::Const,
		"Print header section of the file.");
    alist.add ("print_header", true);
    syntax.add ("print_tags", Syntax::Boolean, Syntax::Const,
		"Print a tag line in the file.");
    alist.add ("print_tags", true);
    syntax.add ("print_dimension", Syntax::Boolean, Syntax::Const,
		"Print a line with units after the tag line.");
    syntax.add ("print_N", Syntax::Boolean, Syntax::Const,
		"Print nitrogen content of harvest.");
    alist.add ("print_N", true);
    syntax.add ("print_C", Syntax::Boolean, Syntax::Const,
		"Print carbon content of harvest.");
    alist.add ("print_C", false);
    alist.add ("print_dimension", true);
    alist.add ("flush", true);

    Librarian<Log>::add_type ("harvest", alist, syntax, &make); 
  }
} LogHarvest_syntax;

