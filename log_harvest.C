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
#include "dlf.h"
#include "vcheck.h"
#include "version.h"
#include "assertion.h"
#include <sstream>
#include <fstream>
#include <time.h>

using namespace std;

struct LogHarvest : public Log
{
  // Filter function.
  bool check_leaf (symbol) const
  { return false; }
  bool check_interior (symbol) const
  { return false; }
  bool check_derived (symbol, symbol, const char*) const
  { return false; }

  // Content.
  unsigned int last_size;
  const string file;            // Filename.
  ofstream out;			// Output stream.
  DLF print_header;		// How much header should be printed?
  bool print_tags;		// Set if tags should be printed.
  bool print_dimension;		// Set if dimensions should be printed.
  const bool print_N;		// Set if nitrogen content should be printed.
  const bool print_C;		// Set if carbon content should be printed.

  // Checking to see if we should log this time step.
  bool match (const Daisy& daisy, Treelog&)
  {
    print_header.finish (out, daisy);

    if (print_tags)
      {
	out << "year\tmonth\tday\tcolumn\tcrop"
	    << "\tstem_DM\tdead_DM\tleaf_DM\tsorg_DM";
	if (print_N)
	  out << "\tstem_N\tdead_N\tleaf_N\tsorg_N";
	if (print_C)
	  out << "\tstem_C\tdead_C\tleaf_C\tsorg_C";
	out << "\tWStress\tNStress\n";
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
	out << "\td\td\n";
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
	out << "\t" << harvest.water_stress_days
            << "\t" << harvest.nitrogen_stress_days
            << "\n";
	out.flush ();
      }
    return false;
  }

  void done (const Time&, double)
  { daisy_notreached (); }

  bool initial_match (const Daisy&, Treelog&)
  { return false; }
  void initial_done (const Time&, double)
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
  void open_derived (symbol, symbol)
  { daisy_notreached (); }
  void close_derived ()
  { daisy_notreached (); }

  // Derived items with their own alist
  void open_object (symbol, symbol, const AttributeList&)
  { daisy_notreached (); }
  void close_object ()
  { daisy_notreached (); }

  // Derived items in a list.
  void open_entry (symbol, const AttributeList&)
  { daisy_notreached (); }
  void close_entry ()
  { daisy_notreached (); }

  // Named derived items in a list.
  void open_named_entry (symbol, symbol, const AttributeList&)
  { daisy_notreached (); }
  void close_named_entry ()
  { daisy_notreached (); }

  void output_entry (symbol, bool)
  { }
  void output_entry (symbol, double)
  { }
  void output_entry (symbol, int)
  { }
  void output_entry (symbol, symbol)
  { }
  void output_entry (symbol, const vector<double>&)
  { }
  void output_entry (symbol, const PLF&)
  { }

  // Create and Destroy.
  void initialize (Treelog&)
  { 
    out.open (file.c_str ()); 

    // Header.
    print_header.start (out, name, file, "");
    out.flush ();
  }

  bool check (const Border&, Treelog& msg) const
  { 
    Treelog::Open nest (msg, name);
    bool ok = true;
    if (!out.good ())
      {
	std::ostringstream tmp;
	tmp << "Write error for '" << file << "'";
	msg.error (tmp.str ());
	ok = false;
      }
    return ok; 
  }

  LogHarvest (Block& al)
    : Log (al),
      last_size (0),
      file (al.name ("where")),
      print_header (al.name ("print_header")),
      print_tags (al.flag ("print_tags")),
      print_dimension (al.flag ("print_dimension")),
      print_N (al.flag ("print_N")),
      print_C (al.flag ("print_C"))
  { }

  ~LogHarvest ()
  {
    if (!out.good ())
      throw (string ("Problems writing to '") + file + "'");
  }
};

static struct LogHarvestSyntax
{
  static Model& make (Block& al)
  { return *new LogHarvest (al); }

  LogHarvestSyntax ()
  {  
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Create a log of all harvests.");
    syntax.add ("where", Syntax::String, Syntax::Const,
		"Name of the log file to create.");
    alist.add ("where", "harvest.dlf");
    syntax.add ("print_header", Syntax::String, Syntax::Const,
                "If this is set to 'false', no header is printed.\n\
If this is set to 'true', a full header is printer.\n\
If this is set to 'fixed', a small fixed size header is printed.");
    static VCheck::Enum check_header ("false", "true", "fixed");
    syntax.add_check ("print_header", check_header);
    alist.add ("print_header", "true");
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

    BuildBase::add_type (Log::component, "harvest", alist, syntax, &make); 
  }
} LogHarvest_syntax;

