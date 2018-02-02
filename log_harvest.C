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

#define BUILD_DLL
#include "log.h"
#include "daisy.h"
#include "harvest.h"
#include "dlf.h"
#include "version.h"
#include "assertion.h"
#include "librarian.h"
#include "treelog.h"
#include "frame.h"
#include "format.h"
#include <sstream>
#include <fstream>
#include <time.h>

struct LogHarvest : public Log
{
  // Filter function.
  bool check_leaf (symbol) const
  { return false; }
  bool check_interior (symbol) const
  { return false; }
  bool check_derived (symbol, symbol, const symbol) const
  { return false; }

  // Content.
  unsigned int last_size;
  const symbol file;            // Filename.
  std::ofstream out;			// Output stream.
  DLF print_header;		// How much header should be printed?
  bool print_tags;		// Set if tags should be printed.
  bool print_dimension;		// Set if dimensions should be printed.
  const bool print_N;		// Set if nitrogen content should be printed.
  const bool print_C;		// Set if carbon content should be printed.

  // Checking to see if we should log this time step.
  bool match (const Daisy& daisy, Treelog&)
  {
    print_header.finish (out, metalib (), daisy.frame ());

    if (print_tags)
      {
	out << "year\tmonth\tday\tcolumn\tcrop"
	    << "\tstem_DM\tdead_DM\tleaf_DM\tsorg_DM";
	if (print_N)
	  out << "\tstem_N\tdead_N\tleaf_N\tsorg_N";
	if (print_C)
	  out << "\tstem_C\tdead_C\tleaf_C\tsorg_C";
	out << "\tWStress\tNStress\tWP_ET\tHI\n";
	print_tags = false;
      }
    if (print_dimension)
      {
	out << "\t\t\t\t"
	    << "\tMg DM/ha\tMg DM/ha\tMg DM/ha\tMg DM/ha";
	if (print_N)
	  out << "\tkg N/ha\tkg N/ha\tkg N/ha\tkg N/ha";
	if (print_C)
	  out << "\tkg C/ha\tkg C/ha\tkg C/ha\tkg C/ha";
	out << "\td\td\tkg/m^3\t\n";
	print_dimension = false;
      }
    for (; last_size < daisy.harvest ().size (); last_size++)
      {
	const Harvest& harvest = *(daisy.harvest ()[last_size]);
	out << harvest.harvest_time.year ()
	    << "\t" << harvest.harvest_time.month ()
	    << "\t" << harvest.harvest_time.mday ()
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
            << "\t" << harvest.water_productivity
            << "\t" << harvest.harvest_index
            << "\n";
	out.flush ();
      }
    return false;
  }

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

  // Create and Destroy.
  void initialize (const symbol log_dir, const symbol suffix, Treelog&)
  { 
    const std::string fn = log_dir.name () + file.name () + suffix.name ();
    out.open (fn.c_str ()); 

    // Header.
    print_header.start (out, objid, file, "");
    out.flush ();
  }

  bool check (const Border&, Treelog& msg) const
  { 
    TREELOG_MODEL (msg);
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

  LogHarvest (const BlockModel& al)
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
      Assertion::error  ("Problems writing to '" + file + "'");
  }
};

static struct LogHarvestSyntax : public DeclareModel
{
  static void entry (Format& format, const symbol name, const symbol dim,
                     const symbol description)
  {
    Format::Item item (format, name);  
    format.special ("nbsp");
    format.text ("[");
    format.bold (dim);
    format.text ("]");
    format.hard_linebreak ();
    format.text (description);
    format.soft_linebreak ();
  }
  static void document_entries (Format& format, const Metalib&, 
                         Treelog& msg, const symbol name)
  {
    if (name != "harvest")
      return;
    
    format.bold ("Table columns (common):");
    {
      Format::List dummy (format);
      entry (format, "stem_DM", "Mg DM/ha", "\
Stem dry matter removed by harvest.");
      entry (format, "dead_DM", "Mg DM/ha", "\
Yeallow leaves dry matter removed by harvest.");
      entry (format, "leaf_DM", "Mg DM/ha", "\
Green leaves dry matter removed by harvest.");
      entry (format, "sorg_DM", "Mg DM/ha", "\
Storage organ (grains or tuber) dry matter removed by harvest.\n\
For some crops, only the economicly important part of the storage organ\n\
is counted.");
    }
    format.soft_linebreak ();

    format.bold ("Table columns (if print_N is set):");
    {
      Format::List dummy (format);
      entry (format, "stem_N", "kg N/ha", "\
Stem nitrogen removed by harvest.");
      entry (format, "dead_N", "kg N/ha", "\
Yeallow leaves nitrogen removed by harvest.");
      entry (format, "leaf_N", "kg N/ha", "\
Green leaves nitrogen removed by harvest.");
      entry (format, "sorg_N", "kg N/ha", "\
Storage organ (grains or tuber) nitrogen removed by harvest.");
    }
    format.soft_linebreak ();
      
    format.bold ("Table columns (if print_C is set):");
    {
      Format::List dummy (format);
      entry (format, "stem_C", "kg C/ha", "\
Stem carbon removed by harvest.");
      entry (format, "dead_C", "kg C/ha", "\
Yeallow leaves carbon removed by harvest.");
      entry (format, "leaf_C", "kg C/ha", "\
Green leaves carbon removed by harvest.");
      entry (format, "sorg_C", "kg C/ha", "\
Storage organ (grains or tuber) carbon removed by harvest.");
    }
    format.soft_linebreak ();

    format.bold ("Table columns (common):");
    {
      Format::List dummy (format);
      
      entry (format, "WStress", "d", "\
Number of days worth of production lost due to water stress.\n\
\n\
This is just a rough measure. It is calculated by weighting the water\n\
stress variation over the day with the amount of global radiation\n\
received in the same period. So water stress at noon with high\n\
radiation counts a lot more than water stress near sunset with low\n\
radiation. All days are counted the same, so water stress a day with\n\
low radiation is counted the same as water stress a day with high\n\
radiation.");
      entry (format, "NStress", "d", "\
Number of days worth of production lost due to nitrogen stress.\n\
\n\
This is just a rough measure. It is calculated by weighting the nitrogen\n\
stress variation over the day with the amount of global radiation\n\
received in the same period. So nitrogen stress at noon with high\n\
radiation counts a lot more than nitrogen stress near sunset with low\n\
radiation. All days are counted the same, so nitrogen stress a day with\n\
low radiation is counted the same as nitrogen stress a day with high\n\
radiation.");
      entry (format, "WP_ET", "kg/m^3", "\
A measure for water usage efficiency.\n\
Specifically the economic yield (storage organ) divided by the total\n\
evapotranspiration in the period from emergence to maturity or\n\
harvest.");
    }
  }

  Model* make (const BlockModel& al) const
  { return new LogHarvest (al); }

  LogHarvestSyntax ()
    : DeclareModel (Log::component, "harvest", "Create a log of all harvests.")
  { }
  void load_frame (Frame& frame) const
  {  
    frame.declare_string ("where", Attribute::Const,
		"Name of the log file to create.");
    frame.set ("where", "harvest.dlf");
    DLF::add_syntax (frame, "print_header");
    frame.declare_boolean ("print_tags", Attribute::Const,
		"Print a tag line in the file.");
    frame.set ("print_tags", true);
    frame.declare_boolean ("print_dimension", Attribute::Const,
		"Print a line with units after the tag line.");
    frame.declare_boolean ("print_N", Attribute::Const,
		"Print nitrogen content of harvest.");
    frame.set ("print_N", true);
    frame.declare_boolean ("print_C", Attribute::Const,
		"Print carbon content of harvest.");
    frame.set ("print_C", false);
    frame.set ("print_dimension", true);
    Librarian::add_doc_fun (Log::component, document_entries);
  }
} LogHarvest_syntax;

