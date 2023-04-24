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

struct ProgramNwaps : public Program
{
  // Content.
  const symbol parent_directory;
  std::vector<symbol> directory;  
  std::vector<symbol> file;
  const std::vector<symbol> scenario;
  const symbol scn_sep;
  const bool combine_units;
  const symbol input_suffix;
  const symbol output_prefix;
  const symbol output_suffix;
  const symbol summary_prefix;
  const symbol success_file;
  const symbol failure_file;

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

  // Use.
  bool run (Treelog& msg)
  {
    // Fill directory.
    if (directory.size () == 0)
      {
	for (auto& entry
	       : std::filesystem::directory_iterator (parent_directory.name ()))
	  {
	    if (!entry.is_directory ())
	      continue;

	    const std::string file = entry.path ().filename ();

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
	      = entry.path () + "/" + failure_file.name ();
	    if (std::filesystem::exists (failure))
	      {
		msg.message ("Ignored, '" + file + "' failed spawn");
		continue;
	      }
	    const std::string success
	      = entry.path () + "/" + success_file.name ();
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
	    const std::string name = entry.path ().filename ();
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
	bool first_line = true;
	const std::string out_name = output_prefix + f + output_suffix;
	std::ofstream out (out_name);

	if (!out.good ())
	  {
	    msg.error ("Write failure for '" + out_name + "', skipping");
	    continue;
	  }
	for (auto d: directory)
	  {
	    std::vector<std::string> scn;
	    (void) split (d.name (), scn_sep.name (), scn);
	    const std::string name
	      = parent_directory + "/" + d + "/" + f + input_suffix;
	    msg.message ("Reading " + name);
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
		msg.warning ("Mismatched tag and unit lines");
		continue;
	      }

	    // Tag line
	    if (first_line)
	      {
		first_line = false;
		
		for (auto s: scenario)
		  put_entry (out, s.name ());
	    
		for (int i = 0; i < dims.size (); i++)
		  if (dims[i].length () > 0)
		    put_entry (out, tags[i] + " [" + dims[i] + "]");
		  else
		    put_entry (out, tags[i]);
		put_line (out);
	      }

	    // Data.
	    while (lex.good ())
	      {
		std::vector<std::string> data;
		get_entries (lex, data);
		for (auto s: scn)
		  put_entry (out, s);
		for (auto d: data)
		  put_entry (out, d);
		put_line (out);
	      }
	    lex.eof ();
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
      combine_units (al.flag ("combine_units")),
      input_suffix (al.name ("input_suffix")),
      output_prefix (al.name ("output_prefix")),
      output_suffix (al.name ("output_suffix")),
      summary_prefix (al.name ("summary_prefix")),
      success_file (al.name ("success_file")),
      failure_file (al.name ("failure_file"))
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
Nwaps collect results from multiple directories.")
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
    frame.declare_boolean ("combine_units", Attribute::Const, "\
Combine name and unit rows in Daisy log files.");
    frame.set ("combine_units", true);
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
  }
} ProgramNwaps_syntax;

// program_nwaps.C ends here.
