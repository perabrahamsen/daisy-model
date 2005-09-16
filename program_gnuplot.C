// program_gnuplot.C -- gnuplot interface 
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


#include "program.h"
#include "path.h"
#include "treelog.h"
#include "tmpstream.h"
#include "lexer_data.h"
#include "mathlib.h"
#include <string>

struct ProgramGnuplot : public Program
{
  // Content.
  const std::string program;
  const std::string tmpdir;

  // Source.
  struct Source
  {
    const std::string filename;
    const std::string tag;
    
    std::string field_sep;

    // Run.
    static int find_tag (std::map<std::string,int>& tag_pos,
                         const std::string& tag, 
			 int default_value);
    std::string get_entry (LexerData& lex) const;
    std::vector<std::string> get_entries (LexerData& lex) const;
    bool load (Treelog& msg);

    // Create and Destroy.
    static void load_syntax (Syntax& syntax, AttributeList&);
    Source (const AttributeList& al);
    ~Source ();
  };
  /* const */ std::vector<Source*> source;

  // Use.
  void run (Treelog& msg);
  
  // Create and Destroy.
  void initialize (const Syntax*, const AttributeList*, Treelog&)
  { }
  bool check (Treelog&)
  { return true; }
  ProgramGnuplot (const AttributeList& al)
    : Program (al),
      program (al.name ("program")),
      tmpdir (al.name ("directory")),
      source (map_construct<Source> (al.alist_sequence ("source")))
  { }
  ~ProgramGnuplot ()
  { sequence_delete (source.begin (), source.end ()); }
};


int
ProgramGnuplot::Source::find_tag (std::map<std::string,int>& tag_pos,
                                  const std::string& tag, 
				  const int default_value)
{
  if (tag_pos.find (tag) == tag_pos.end ())
    return default_value;
  return tag_pos[tag];
}

std::string
ProgramGnuplot::Source::get_entry (LexerData& lex) const
{
  std::string tmp_term;  // Data storage.
  const char* field_term;

  switch (field_sep.size ())
    { 
    case 0:
      // Whitespace
      field_term = " \t\n";
      break;
    case 1:
      // Single character field seperator.
      tmp_term = field_sep + "\n";
      field_term = tmp_term.c_str ();
      break;
    default:
      // Multi-character field seperator.
      daisy_assert (false);
    }

  // Find it.
  std::string entry = "";
  while (lex.good ())
    {
      int c = lex.peek ();
      if (strchr (field_term, c))
	break;
      entry += int2char (lex.get ());
    }
  return entry;
}

std::vector<std::string>
ProgramGnuplot::Source::get_entries (LexerData& lex) const
{
  std::vector<std::string> entries;

  while (lex.good () && lex.peek () != '\n')
    {
      if (field_sep == "")
	lex.skip_space ();
      else
	lex.skip(field_sep.c_str ());
      entries.push_back (get_entry (lex));
    }
  lex.next_line ();
  return entries;
}

bool
ProgramGnuplot::Source::load (Treelog& msg)
{
  LexerData lex (filename, msg);

  // Open errors?
  if (!lex.good ())
    return false;

  // Read first line.
  const std::string type = lex.get_word ();
  if (type == "dwf-0.0")
    {
      field_sep = "";
    }
  else if (type == "dlf-0.0" || type == "ddf-0.0")
    {
      field_sep = "\t";
    }
  else
    lex.error ("Unknown file type '" + type + "'");
  lex.skip_line ();
  lex.next_line ();

  // Skip keywords.
  while (lex.good () && lex.peek () != '-')
    {
      lex.skip_line ();
      lex.next_line ();
    }

  // Skip hyphens.
  lex.skip_hyphens ();

  // Read tags.
  std::map<std::string,int> tag_pos;
  const std::vector<std::string> tag_names = get_entries ();
  for (int count = 0; count < tag_names.size (); count++)
    {
      const std::string candidate = tag_names[count];
      if (tag_pos.find (candidate) == tag_pos.end ())
        tag_pos[candidate] = count;
      else
	lex.warning ("Duplicate tag: " + candidate);
    }

  const int tag_c = find_tag (tag_pos, tag, -1);
  const int year_c = find_tag (tag_pos, "year", -1);
  const int month_c = find_tag (tag_pos, "month", -1);
  const int mday_c = find_tag (tag_pos, "mday", -1);
  const int hour_c = find_tag (tag_pos, "hour", -1);
  if (tag_c < 0)
    {
      lex.error ("Tag '" + tag + "' not found");
      return false;
    }

  // Read dimensions.
  dim_names = get_entries (lex);
  if (dim_names.size () != tag_names.size ())
    if (dim_names.size () > tag_c)
      lex.warning ("Number of dimensions does not match number of tags");
    else
      {
	lex.error ("No dimension for '" + tag + "' found");
	return false;
      }


  // Read data.
#if 0
  while (lex.good ())
    {
      const std::string entries = 
      double year = 1000;
      double 
      for (int count == 0; lex.good (); count ++)
        {
          double number = 
        }
    }
#endif

  // Done.
  return true;
}

void 
ProgramGnuplot::Source::load_syntax (Syntax& syntax, AttributeList&)
{
  syntax.add ("file", Syntax::String, Syntax::Const, "\
Name of Daisy log file where data is found.");
  syntax.add ("tag", Syntax::String, Syntax::Const, "\
Name of column in Daisy log file where data is found.");
}

ProgramGnuplot::Source::Source (const AttributeList& al)
  : filename (al.name ("file")),
    tag (al.name ("tag")),
    field_sep ("\t")
{ }

ProgramGnuplot::Source::~Source ()
{ }

void 
ProgramGnuplot::run (Treelog& msg)
{ 
  for (size_t i = 0; i < source.size(); i++)
    {
      TmpStream tmp;
      tmp () << name << "[" << i << "]: " << source[i]->tag;
      Treelog::Open nest (msg, tmp.str ());
      if (!source[i]->load (msg))
        return;
    }
}

static struct ProgramGnuplotSyntax
{
  static Program&
  make (const AttributeList& al)
  { return *new ProgramGnuplot (al); }
  ProgramGnuplotSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description",
               "Generate plot from Daisy log files with gnuplot.\n\
UNDER CONSTRUCTION, DO NOT USE!"); 
    syntax.add ("program", Syntax::String, Syntax::Const, "\
File name of the gnuplot executable.");
    syntax.add ("directory", Syntax::String, Syntax::Const, "\
Temporary directory to run gnuplot in.");
    syntax.add_submodule_sequence ("source", Syntax::State, "\
Fertilizer application by date.", ProgramGnuplot::Source::load_syntax);
    Librarian<Program>::add_type ("gnuplot", alist, syntax, &make);
  }
} ProgramGnuplot_syntax;
