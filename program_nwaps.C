// program_nwaps.C -- Collect output from spawned Daisy programs.
// 
// Copyright 2023 Per Abrahamsen and KU.
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
#include "block_model.h"
#include "treelog.h"
#include "librarian.h"
#include "symbol.h"
#include "assertion.h"
#include "lexer_data.h"
#include "mathlib.h"

#include <filesystem>
#include <fstream>
#include <numeric>
#include <sstream>

struct ProgramNwaps : public Program
{
  // Content.
  const symbol parent_directory;
  std::vector<symbol> directory;  
  std::vector<symbol> file;
  const std::vector<symbol> scenario;
  const symbol scn_sep;
  const symbol input_suffix;
  const symbol output_prefix;
  const symbol output_suffix;
  const symbol summary_prefix;
  const symbol success_file;
  const symbol failure_file;
  const std::vector<symbol> missing;
  const std::vector<double> fractiles;
  
  // Running.
  bool first;
  
  bool split (const std::string& file, const std::string& sep,
	      std::vector<std::string>&  result)
  {
    std::string name = file;

    while (name.length () > 0)
      {
	const int found = name.find (sep);
	if (found == std::string::npos)
	  {
	    result.push_back (name);
	    return true;
	  }
	result.push_back (name.substr (0, found));
	name = name.substr (found + sep.length ());
      }
    return false;
  }

  std::string get_entry (LexerData& lex)
  {
    std::string result;
    while (lex.peek () != '\t' && lex.peek () != '\n')
      result += int2char (lex.get ());
    return result;
  }
  void get_entries (LexerData& lex, std::vector<std::string>& entries)
  {
    while (lex.good ())
      {
	entries.push_back (get_entry (lex));
	if (lex.peek () == '\n')
	  break;
	lex.skip ("\t");
      }
    lex.skip ("\n");
  }

  void put_entry (std::ostream& out, const std::string s)
  {
    if (first)
      first = false;
    else
      out << ",";
    out << s;
  }

  void put_line (std::ostream& out)
  {
    out << "\n";
    first = true;
  }

  static double count (const std::vector<double>& v)
  { return v.size (); }

  static double average (const std::vector<double>& v)
  {
    if (v.size () < 1)
      return NAN;

    const double N = count (v);
    const double sum = std::accumulate (v.begin (), v.end (), 0.0);
    const double avg = sum / N;
    return avg;
  }

  static double stdev (const std::vector<double>& v)
  {
    if (v.size () < 2)
      return 0.0;

    const double N = count (v);
    const double avg = average (v);
    const double sqrdiff = std::accumulate (v.begin (), v.end (), 0.0,
					    [avg](double old, double val)
					    { return old + sqr (val-avg); });
    const double sd = std::sqrt (sqrdiff / (N - 1.0));
    return sd;
  }

  static double sterr (const std::vector<double>& v)
  {
    if (v.size () < 2)
      return 0.0;
    const double N = count (v);
    const double sd = stdev (v);
    const double se = sd / std::sqrt (N);
    return se;
  }

  typedef double (*sum_fun_t) (const std::vector<double>&);
  void summary_line (std::ostream& sum,
		     const std::string& what,
		     const std::vector<std::string>& scn,
		     const std::vector<std::vector<double>>& table,
		     sum_fun_t fun) const
  {
    sum << what;
    for (auto s: scn)
      sum << "," << s;
    for (auto d: table)
      sum << "," << fun (d);
    sum << "\n";
  }
  void fractile_line (std::ostream& sum,
		      const double fractile,
		      const std::vector<std::string>& scn,
		      const std::vector<std::vector<double>>& table) const
  {
    sum << fractile * 100 << "%";
    for (auto s: scn)
      sum << "," << s;
    for (auto d: table)
      {
	double val = NAN;
	if (d.size () > 0)
	  {	  
	    std::sort (d.begin (), d.end ());
	    const int index = 0.5 + fractile * (d.size () - 1);
	    daisy_assert (index >= 0);
	    daisy_assert (index < d.size ());
	    val = d[index];
	  }
	sum << "," << val;
      }
    sum << "\n";
  }

  // Use.
  bool run (Treelog& msg)
  {
    TREELOG_MODEL (msg);
    // Fill directory.
    if (directory.size () == 0)
      {
	for (auto& entry
	       : std::filesystem::directory_iterator (parent_directory.name ()))
	  {
	    if (!entry.is_directory ())
	      continue;

	    // const std::string file = entry.path ().filename ();
	    const std::string file = entry.path ().filename ().string ();

	    if (scenario.size () > 1)
	      {
		std::vector<std::string> scns;
		if (!split (file, scn_sep.name (), scns)
		    || scns.size () != scenario.size ())
		  {
		    msg.message ("Ignored, '" + file + "' name no match");
		    continue;
		  }
	      }
	    const std::string failure
	      = entry.path ().string () + "/" + failure_file.name ();
	    if (std::filesystem::exists (failure))
	      {
		msg.message ("Ignored, '" + file + "' failed spawn");
		continue;
	      }
	    const std::string success
	      = entry.path ().string () + "/" + success_file.name ();
	    if (!std::filesystem::exists (success))
	      {
		msg.message ("Ignored, '" + file + "' not a spawn success");
		continue;
	      }
	    directory.push_back (file);
	  }
	std::ostringstream tmp;
	tmp << "Found " <<  directory.size () << " scenario directories.";
	msg.message (tmp.str ());
      }
    if (directory.size () == 0)
      {
	msg.message ("Nowhere to look.");
	return true;
      }

    // Fill files.
    if (file.size () == 0)
      {
	const std::string dir = parent_directory + "/" + directory[0].name ();
	msg.message ("Looking for files in " + dir);

	for (auto& entry : std::filesystem::directory_iterator (dir))
	  {
	    const std::string name = entry.path ().filename ().string ();
	    if (!entry.is_regular_file ())
	      {
		msg.message ("Ignoring " + name);
		continue;
	      }
	    const int found = name.find (input_suffix.name ());
	    if (found == std::string::npos
		|| found + input_suffix.name ().length () != name.length ())
	      {
		msg.message ("'" + name + "' is not a '"
			     + input_suffix + "' file");
		continue;
	      }
	    const std::string base = name.substr (0, found);
	    file.push_back (base);
	  }	    
	std::ostringstream tmp;
	tmp << "Found " <<  file.size () << " data files.";
	msg.message (tmp.str ());
      }
    if (file.size () == 0)
      {
	msg.message ("Nothing to do.");
	return true;
      }

    for (auto f: file)
      {
	Treelog::Open nest (msg, f);
	msg.message ("Collecting...");
	const std::string out_name = output_prefix + f + output_suffix;
	std::ofstream out (out_name);
	if (!out.good ())
	  {
	    msg.error ("Write failure for '" + out_name + "', skipping");
	    continue;
	  }

	const std::string sum_name = summary_prefix + f + output_suffix;
	std::ofstream sum (sum_name);
	if (!sum.good ())
	  {
	    msg.error ("Write failure for '" + sum_name + "', skipping");
	    continue;
	  }

	bool first_line = true;
	for (auto d: directory)
	  {
	    Treelog::Open nest (msg, d);
	    std::vector<std::string> scn;
	    (void) split (d.name (), scn_sep.name (), scn);
	    const std::string name
	      = parent_directory + "/" + d + "/" + f + input_suffix;
	    std::ifstream in (name.c_str ());
	    if (!in.good ())
	      {
		msg.warning ("Problems opening " + name + ", skipping");
		continue;
	      }
	    LexerData lex (f + input_suffix, in, msg);
	    // Skip header.
	    while (lex.good () && lex.peek () != '-')
	      {
		lex.skip_line ();
		lex.skip ("\n");
	      }
	    lex.skip_hyphens ();

	    // Read tags.
	    std::vector<std::string> tags;
	    get_entries (lex, tags);

	    // Read dims.
	    std::vector<std::string> dims;
	    get_entries (lex, dims);

	    if (tags.size () != dims.size ())
	      {
		lex.warning ("Mismatched tag and unit lines");
		continue;
	      }

	    std::vector<std::string> cols;
	    for (int i = 0; i < dims.size (); i++)
	      if (dims[i].length () > 0)
		cols.push_back (tags[i] + " [" + dims[i] + "]");
	      else
		cols.push_back (tags[i]);
	      
	    
	    std::vector<std::vector<double>> table (tags.size ());

	    // Tag line
	    if (first_line)
	      {
		for (auto s: scenario)
		  put_entry (out, s.name ());
	    
		for (auto c: cols)
		  put_entry (out, c);

		put_line (out);
	      }

	    // Data.
	    while (lex.good ())
	      {
		std::vector<std::string> data;
		std::vector<double> numbers;
		get_entries (lex, data);
		for (auto s: scn)
		  put_entry (out, s);
		for (auto d: data)
		  {
		    put_entry (out, d);

		    double val = NAN;
		    if (std::find (missing.begin (), missing.end (), d)
			== missing.end ())
		      {
			char end;
			std::istringstream in (d);
			
			in >> val >> end;
			if (!in.eof ())
			  val = NAN;
		      }
		    numbers.push_back (val);
		  }
		put_line (out);

		if (table.size () != numbers.size ())
		  lex.warning ("Mismatched tags and data lines");
		else for (int i = 0; i < tags.size (); i++)
		       if (std::isfinite (numbers[i]))
			 table[i].push_back (numbers[i]);
	      }
	    lex.eof ();

	    // Make summaries.
	    if (first_line)
	      {
		first_line = false;

		sum << "What";
		for (auto s: scenario)
		  sum << "," << s;
		for (auto c: cols)
		  sum << "," << c;
		sum << "\n";
	      }

	    // Count.
	    summary_line (sum, "N", scn, table, count);
	    summary_line (sum, "Average", scn, table, average);
	    summary_line (sum, "STDEV", scn, table, stdev);
	    summary_line (sum, "STERR", scn, table, sterr);
	    for (auto f: fractiles)
	      fractile_line (sum, f, scn, table);
	  }
      }
    return true;
  }

  // Create and Destroy.
  void initialize (Block&)
  { }
  bool check (Treelog&)
  { return true; }

  ProgramNwaps (const BlockModel& al)
    : Program (al),
      parent_directory (al.name ("parent_directory")),
      directory (al.name_sequence ("directory")),
      file (al.name_sequence ("file")),
      scenario (al.name_sequence ("scenario")),
      scn_sep (al.name ("scn_sep")),
      input_suffix (al.name ("input_suffix")),
      output_prefix (al.name ("output_prefix")),
      output_suffix (al.name ("output_suffix")),
      summary_prefix (al.name ("summary_prefix")),
      success_file (al.name ("success_file")),
      failure_file (al.name ("failure_file")),
      missing (al.name_sequence ("missing")),
      fractiles (al.number_sequence ("fractiles"))
  { }
  ~ProgramNwaps ()
  {  }
};

static struct ProgramNwapsSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ProgramNwaps (al); }
  ProgramNwapsSyntax ()
    : DeclareModel (Program::component, "nwaps", "\
Nwaps collects results from multiple directories.")
  { }

  static bool check_alist (const Metalib& metalib, const Frame& al,
			   Treelog& msg)
  {
    bool ok = true;

    return ok;
  }
  
  void load_frame (Frame& frame) const
  {
    frame.add_check (check_alist);
    frame.declare_string ("parent_directory", Attribute::Const, "\
Directory to look for subfolders in.");
    frame.set ("parent_directory", ".");
    frame.declare_string ("directory", Attribute::Const, Attribute::Variable, "\
Directories to look for data in. If empty, use all applicable.");
    frame.set_empty ("directory");
    frame.declare_string ("file", Attribute::Const, Attribute::Variable, "\
Files to look for data in. If empty, use all applicable.");
    frame.set_empty ("file");
    frame.declare_string ("scenario", Attribute::Const, Attribute::Variable, "\
Names of scenarios.");
    frame.set_strings ("scenario", "Scenario");
    frame.declare_string ("scn_sep", Attribute::Const, "\
String seperating scenarios in directory names.");
    frame.set ("scn_sep", "-");
    frame.declare_string ("input_suffix", Attribute::Const, "\
Look for files with this suffix.");
    frame.set ("input_suffix", ".dlf");
    frame.declare_string ("output_prefix", Attribute::Const, "\
Prepend this to output file names.");
    frame.set ("output_prefix", "out_");
    frame.declare_string ("output_suffix", Attribute::Const, "\
Append this to output file names.");
    frame.set ("output_suffix", ".csv");
    frame.declare_string ("summary_prefix", Attribute::Const, "\
Prepend this to summary file names.");
    frame.set ("summary_prefix", "sum_");
    frame.declare_string ("success_file", Attribute::Const, "\
Only combine directories containing this file.");
    frame.set ("success_file", "SUCCESS");
    frame.declare_string ("failure_file", Attribute::Const, "\
Don't combine directories containing this file.");
    frame.set ("failure_file", "FAILURE");
    frame.declare_string ("missing", Attribute::Const, Attribute::Variable, "\
List of strings indicating missing values.");
    frame.set_strings ("missing", "", "00.00");
    frame.declare_fraction ("fractiles",
			    Attribute::Const, Attribute::Variable, "\
Fractiles to include in summary.");
    const std::vector<double> fractiles {0.0, 0.1, 0.5, 0.9, 1.0};
    frame.set ("fractiles", fractiles);
  }
} ProgramNwaps_syntax;

// program_nwaps.C ends here.
