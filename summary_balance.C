// summary_balance.C --- Create a balance for a log model.
// 
// Copyright 2003, 2004 Per Abrahamsen and KVL.
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

#include "summary.h"
#include "block_model.h"
#include "fetch_pretty.h"
#include "select.h"
#include "treelog.h"
#include "memutils.h"
#include "librarian.h"
#include "frame.h"
#include <sstream>
#include <algorithm>
#include <fstream>
#include <string>

struct SummaryBalance : public Summary
{
  static const char *const default_description;
  const symbol description;
  const symbol file;

  // Content.
  const int precision;
  const bool require_top;
  const std::vector<symbol> input;
  const std::vector<symbol> output;
  const std::vector<symbol> content;
  const class ConstructFetch : public std::vector<FetchPretty*> 
  {
    void add (const std::vector<symbol>& a)
    {
      for (size_t i = 0; i < a.size (); i++)
        push_back (new FetchPretty (a[i]));
    }
  public:
    ConstructFetch (const std::vector<symbol>& a, 
                    const std::vector<symbol>& b, 
                    const std::vector<symbol>& c)
    { add (a); add (b); add (c); }
    ~ConstructFetch ()
    { sequence_delete (begin (), end ()); }
  } 
  fetch;

  // Create and Destroy.
  void clear ();
  void initialize (std::vector<Select*>&, Treelog&);
  explicit SummaryBalance (const BlockModel&);
  bool in_list (size_t i, const std::vector<symbol>& names) const;
  double find_total (const std::vector<symbol>& names, 
                     int& max_digits) const;
  symbol print_entries (std::ostream& out, const std::vector<symbol>& names, 
                        int max_size, int width) const;
  void print_balance (std::ostream& out,
                      const symbol title, double total, symbol dim,
                      int dim_size, int max_size,
                      int width) const;
  void summarize (Treelog&) const;
};

const char *const SummaryBalance::default_description  = "\
A summary model providing a balance for a log parameterization.";

void
SummaryBalance::clear ()
{ FetchPretty::clear (fetch); }

void
SummaryBalance::initialize (std::vector<Select*>& select, Treelog& msg)
{ 
  TREELOG_MODEL (msg);
  FetchPretty::initialize (fetch, select, msg);
}

SummaryBalance::SummaryBalance (const BlockModel& al)
  : Summary (al),
    description (al.frame ().description ()),
    file (al.name ("where", "")),
    precision (al.integer ("precision")),
    require_top (al.flag ("require_top")),
    input (al.name_sequence ("input")),
    output (al.name_sequence ("output")),
    content (al.name_sequence ("content")),
    fetch (input, output, content)
{ }

bool
SummaryBalance::in_list (size_t i, const std::vector<symbol>& names) const
{ return find (names.begin (), names.end (), fetch[i]->tag) != names.end (); }

double
SummaryBalance::find_total (const std::vector<symbol>& names, 
                            int& max_digits) const
{
  double total = 0.0;
  for (size_t i = 0; i < fetch.size (); i++)
    if (in_list (i, names))
      max_digits = std::max (max_digits, fetch[i]->value_size (total));
  return total;
}

symbol
SummaryBalance::print_entries (std::ostream& out, const std::vector<symbol>& names, 
                               const int max_size, const int width) const
{
  symbol dim = Attribute::User ();
  for (unsigned int i = 0; i < fetch.size (); i++)
    {
      if (!in_list (i, names))
        continue;
          
      if (dim == Attribute::User ())
        dim = fetch[i]->dimension ();
      else if (fetch[i]->dimension () != dim)
        dim = Attribute::Unknown ();

      out << std::string (max_size - fetch[i]->name_size (), ' ');
      fetch[i]->summarize (out, width);
    }
  return dim;
}

void
SummaryBalance::print_balance (std::ostream& out,
                               const symbol title, const double total, 
                               const symbol dim,
                               const int /* dim_size */, const int max_size,
                               const int width) const
{
  out << std::string (max_size + 3, ' ') << std::string (width, '-') << "\n"
      << std::string (max_size - title.name ().size (), ' ') << title << " = ";
  out.width (width);
  out << total;
  if (dim != Attribute::Unknown ())
    out << " [" << dim << "]";
  out << "\n";
}

void 
SummaryBalance::summarize (Treelog& msg) const
{
  TREELOG_MODEL (msg);

  // We write the summary to a string at first.
  std::ostringstream tmp;
  tmp.precision (precision);
  tmp.flags (std::ios::right | std::ios::fixed);
  if (description.name () != default_description)
    tmp << description << "\n\n";

  // Find width of tags.
  const std::string total_title = "Balance (= In - Out - Increase)";
  const std::string content_title = "Total increase in content";
  size_t max_size = std::max (total_title.size (), content_title.size ());
  for (unsigned int i = 0; i < fetch.size (); i++)
    max_size = std::max (max_size, fetch[i]->name_size ());

  // Find width and total values
  int max_digits = 0;
  const double total_input = find_total (input, max_digits);
  const double total_output = find_total (output, max_digits);
  const double total_content = find_total (content, max_digits);
  const double total = total_input - total_output - total_content;
  max_digits = std::max (max_digits, FetchPretty::width (total_input));
  max_digits = std::max (max_digits, FetchPretty::width (total_output));
  max_digits = std::max (max_digits, FetchPretty::width (total_content));
  max_digits = std::max (max_digits, FetchPretty::width (total));

  // Find total width.
  const int width = max_digits + (precision > 0 ? 1 : 0) + precision;

  // Find width of dimensions.
  size_t dim_size = 0;
  for (unsigned int i = 0; i < fetch.size (); i++)
    dim_size = std::max (dim_size, fetch[i]->dimension ().name ().size ());

  // Print all entries.
  symbol shared_dim = Attribute::User ();
  if (input.size () > 0)
    {
      const symbol dim = print_entries (tmp, input, max_size, width);
      print_balance (tmp, "Total input", total_input, dim,
                     dim_size, max_size, width);
      shared_dim = dim;
      tmp << "\n";
    }

  if (output.size () > 0)
    {
      const symbol dim = print_entries (tmp, output, max_size, width);
      print_balance (tmp, "Total output", total_output, dim,
                     dim_size, max_size, width);
      if (shared_dim == Attribute::User ()) 
        shared_dim = dim;
      else if (dim != shared_dim)
        shared_dim = Attribute::Unknown ();
      tmp << "\n";
    }

  if (content.size () > 0)
    {
      const symbol dim = print_entries (tmp, content, max_size, width);
      print_balance (tmp, content_title, total_content, dim,
                     dim_size, max_size, width);
      if (shared_dim == Attribute::User ()) 
        shared_dim = dim;
      else if (dim != shared_dim)
        shared_dim = Attribute::Unknown ();
      tmp << "\n";
    }

  print_balance (tmp, total_title, total, 
                 shared_dim, dim_size, max_size, width);
  tmp << std::string (max_size + 3, ' ') << std::string (width, '=');

  // Where?
  if (file == "")
    msg.message (tmp.str ());
  else
    { 
      std::ofstream out (file.name ().c_str ());
      out << tmp.str ();
      if (! out.good ())
        msg.error ("Could not write to '" + file + "'");
    } 
}

static struct SummaryBalanceSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new SummaryBalance (al); }
  SummaryBalanceSyntax ()
    : DeclareModel (Summary::component, "balance",
                    SummaryBalance::default_description)
  { }
  void load_frame (Frame& frame) const
    {
      frame.declare_string ("where", Attribute::OptionalConst,
                  "File name to store the summary.\n\
By default, the summary will be stored in daisy.log and the screen.");
      frame.declare_integer ("precision", Attribute::Const,
		  "Number of digits to print after decimal point.");
      frame.set ("precision", 2);
      frame.declare_boolean ("require_top", Attribute::Const, "\
If the balance only hold true when logging the top of the soil, i.e. the\n\
`from' parameter of the log model is 0, this flag should be set.");
      frame.set ("require_top", false);
      frame.declare_string ("input", Attribute::Const, Attribute::Variable,
                  "Tags of columns in log file representing inputs.");
      frame.declare_string ("output", Attribute::Const, Attribute::Variable,
                  "Tags of columns in log file representing outputs.");
      frame.declare_string ("content", Attribute::Const, Attribute::Variable,
                  "Tags of columns in log file representing content.");
    }
} SummaryBalance_syntax;

// summary_balance.C ends here.
