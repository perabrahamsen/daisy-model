// log_harvest.C

#include "log.h"
#include "daisy.h"
#include "harvest.h"
#include <fstream.h>

struct LogHarvest : public Log, public Filter
{
  // Filter functions.
  bool check (const string&, bool) const
    { return false; }
  bool check (const Library&, int) const
    { return false; }
  bool check (const Syntax&, int) const
    { return false; }
  bool check (Syntax::type, int) const
    { return false; }

  Filter& lookup (const string&) const
    { 
      // Bug: We should get rid of the filter all together.
      return const_cast<LogHarvest&> (*this); 
    }

  // Content.
  unsigned int last_size;
  string file;			// Filename.
  ofstream out;			// Output stream.

  // Checking to see if we should log this time step.
  Filter& match (const Daisy& daisy)
    {
      for (; last_size < daisy.harvest.size (); last_size++)
	{
	  const Harvest& harvest = *(daisy.harvest[last_size]);
	  out << harvest.time.year () << "\t"
	      << harvest.time.month () << "\t"
	      << harvest.time.mday () << "\t"
	      << harvest.column << "\t"
	      << harvest.crop << "\t"
	      << harvest.stem_DM * 0.01 << "\t"
	      << harvest.leaf_DM * 0.01 << "\t"
	      << harvest.sorg_DM * 0.01 << "\t"
	      << harvest.stem_N * 10.0 << "\t"
	      << harvest.leaf_N * 10.0 << "\t"
	      << harvest.sorg_N * 10.0 << "\n";
	  out.flush ();
	}
      return *this;
    }

  void done ()
    { }

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

  void output (const string&, Filter&, const Time&, bool)
    { }
  void output (const string&, Filter&, const bool, bool)
    { }
  void output (const string&, Filter&, const double, bool)
    { }
  void output (const string&, Filter&, const int, bool)
    { }
  void output (const string&, Filter&, const string&, bool)
    { }
  void output (const string&, Filter&, const vector<double>&, bool)
    { }
  void output (const string&, Filter&, const CSMP&, bool)
    { }

  // Create and Destroy.
  bool check (const Syntax&) const
    { return true; }

  LogHarvest (const AttributeList& al)
    : Log (),
      last_size (0),
      file (al.name ("where")),
#ifdef BORLAND_PERMISSIONS
      out (file.c_str (), ios::out|ios::trunc, 0666)
#else
      out (file.c_str ())
#endif
    {
      // Tags.
      if (al.flag ("print_tags"))
	out << "year\tmonth\tday\tcolumn\tcrop\t"
	    << "stem_DM\tleaf_DM\tsorg_DM\tstem_N\tleaf_N\tsorg_N\n";
      // Dimensions.
      if (al.flag ("print_dimension"))
	out << "\t\t\t\t\t"
	    << "t/ha\t/ha\tt/ha\tkg/ha\tkg/ha\tkg/ha\n";
      out.flush ();
    }

  ~LogHarvest ()
    {
      if (!out.good ())
	CERR << "Problems writing to `" << file << "'\n";
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
      alist.add ("where", "harvest.tab");
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

