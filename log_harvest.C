// log_harvest.C

#include "log.h"
#include "daisy.h"
#include "harvest.h"
#include "version.h"
#include "message.h"
#include <fstream.h>
#include <time.h>

struct LogHarvest : public Log
{
  // Filter function.
  bool check (const string&) const
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

  // Checking to see if we should log this time step.
  bool match (const Daisy& daisy)
    {
      if (print_header)
	{
	  print_dlf_header (out, daisy.alist);
	  print_header = false;
	}
      if (print_tags)
	{
	  out << "year\tmonth\tday\tcolumn\tcrop\t"
	      << "stem_DM\tdead_DM\tleaf_DM\tsorg_DM\t"
	      << "stem_N\tdead_N\tleaf_N\tsorg_N\t"
	      << "stem_C\tdead_C\tleaf_C\tsorg_C\n";
	  print_tags = false;
	}
      if (print_dimension)
	{
	  out << "\t\t\t\t\t"
	      << "t/ha\tt/ha\tt/ha\tt/ha\t"
	      << "kg/ha\tkg/ha\tkg/ha\tkg/ha\t"
	      << "kg/ha\tkg/ha\tkg/ha\tkg/ha\n";
	  print_dimension = false;
	}
      for (; last_size < daisy.harvest.size (); last_size++)
	{
	  const Harvest& harvest = *(daisy.harvest[last_size]);
	  out << harvest.time.year () << "\t"
	      << harvest.time.month () << "\t"
	      << harvest.time.mday () << "\t"
	      << harvest.column << "\t"
	      << harvest.crop << "\t"
	      << harvest.stem_DM * 0.01 << "\t"
	      << harvest.dead_DM * 0.01 << "\t"
	      << harvest.leaf_DM * 0.01 << "\t"
	      << harvest.sorg_DM * 0.01 << "\t"
	      << harvest.stem_N * 10.0 << "\t"
	      << harvest.dead_N * 10.0 << "\t"
	      << harvest.leaf_N * 10.0 << "\t"
	      << harvest.sorg_N * 10.0 << "\t"
	      << harvest.stem_C * 10.0 << "\t"
	      << harvest.dead_C * 10.0 << "\t"
	      << harvest.leaf_C * 10.0 << "\t"
	      << harvest.sorg_C * 10.0 << "\n";
	  out.flush ();
	}
      return false;
    }

  void done ()
    { assert (false); }

  // Open normal items.
  void open (const string&)
    { assert (false); }
  void close ()
    { assert (false); }

  // Ignore unnamed items.
  void open_unnamed ()
    { assert (false); }
  void close_unnamed ()
    { assert (false); }

  // Open derived items two steps a time.
  void open_derived (const string&, const string&)
    { assert (false); }
  void close_derived ()
    { assert (false); }

  // Open derived items in list normally.
  void open_entry (const string&, const AttributeList&)
    { assert (false); }
  void close_entry ()
    { assert (false); }

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
      print_dimension (al.flag ("print_dimension"))
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
	CERR << "Problems writing to '" << file << "'\n";
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
      alist.add ("print_dimension", true);
      alist.add ("flush", true);

      Librarian<Log>::add_type ("harvest", alist, syntax, &make); 
    }
} LogHarvest_syntax;

