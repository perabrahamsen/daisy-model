// summary_simple.C
// 
// Copyright 2003 Per Abrahamsen and KVL.
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
#include "fetch_pretty.h"
#include "select.h"
#include "treelog.h"
#include "memutils.h"
#include "submodeler.h"
#include "librarian.h"
#include "frame.h"
#include "block_model.h"
#include <sstream>
#include <fstream>
#include <string>

struct SummarySimple : public Summary
{
  static const symbol default_description;
  const symbol description;
  const symbol file;
  const bool print_sum;
  const symbol sum_name;
  const symbol period;

  // Content.
  const int precision;
  const std::vector<FetchPretty*> fetch;

  // Create and Destroy.
  void clear ();
  void initialize (std::vector<Select*>&, Treelog&);
  explicit SummarySimple (const BlockModel&);
  ~SummarySimple ();
  void summarize (Treelog&) const;
};

const symbol SummarySimple::default_description ("\
A simple log file summary model.");

void
SummarySimple::clear ()
{ FetchPretty::clear (fetch); }

void
SummarySimple::initialize (std::vector<Select*>& select, Treelog& msg)
{ 
  TREELOG_MODEL (msg);
  FetchPretty::initialize (fetch, select, msg);
}

SummarySimple::SummarySimple (const BlockModel& al)
  : Summary (al),
    description (al.frame ().description ()),
    file (al.name ("where", "")),
    print_sum (al.flag ("print_sum")),
    sum_name (al.name ("sum_name")),
    period (al.check ("period") ? al.name ("period") : symbol ("")),
    precision (al.integer ("precision")),
    fetch (map_construct<FetchPretty> (al.submodel_sequence ("fetch")))
{ }

SummarySimple::~SummarySimple ()
{ sequence_delete (fetch.begin (), fetch.end ()); }

void 
SummarySimple::summarize (Treelog& msg) const
{
  TREELOG_MODEL (msg);
  std::ostringstream tmp;
  tmp.precision (precision);
  tmp.flags (std::ios::right | std::ios::fixed);
  if (description != default_description)
    tmp << description << "\n\n";

  double total = 0.0;
  const int sum_size = sum_name.name ().size ();
  size_t max_size = print_sum ? sum_size : 0;
  for (unsigned int i = 0; i < fetch.size (); i++)
    max_size = std::max (max_size, fetch[i]->name_size ());
  int max_digits = 0;
  for (unsigned int i = 0; i < fetch.size (); i++)
    max_digits = std::max (max_digits, fetch[i]->value_size (total));
  max_digits = std::max (max_digits, FetchPretty::width (total));
  const int width = max_digits + (precision > 0 ? 1 : 0) + precision;
  size_t dim_size = 0;
  for (unsigned int i = 0; i < fetch.size (); i++)
    dim_size = std::max (dim_size, fetch[i]->dimension ().name ().size ());
  symbol last_dim;
  bool same_dim = true;
  for (unsigned int i = 0; i < fetch.size (); i++)
    {
      if (i == 0)
	last_dim = fetch[i]->dimension ();
      else if (same_dim && fetch[i]->dimension () != last_dim)
	same_dim = false;
	
      tmp << std::string (max_size - fetch[i]->name_size (), ' ');
      fetch[i]->summarize (tmp, width);
    }
  if (print_sum)
  {
    tmp << std::string (max_size + 3 + width + 3 + dim_size, '-') << "\n"
	   << std::string (max_size - sum_size, ' ') << sum_name << " = ";
    tmp.width (width);
    tmp << total;
    if (same_dim)
      tmp << " [" << last_dim << "]";
    tmp << "\n" << std::string (max_size + 3, ' ') << std::string (width, '=');
  }
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

static struct SummarySimpleSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new SummarySimple (al); }
  SummarySimpleSyntax ()
    : DeclareModel (Summary::component, "simple",
                    SummarySimple::default_description)
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_string ("where", Attribute::OptionalConst,
                "File name to store the summary.\n\
By default, the summary will be stored in daisy.log and the screen.");
    frame.declare_boolean ("print_sum", Attribute::Const, 
                "Print sum of all the summary lines.");
    frame.set ("print_sum", true);
    frame.declare_string ("sum_name", Attribute::Const,
                "Name of the sum of all the entries.");
    frame.set ("sum_name", "Sum");	
    frame.declare_string ("period", Attribute::OptionalConst, "\
Set this to 'y', 'm', 'w', 'd' or 'h' to get fluxes per time period\n\
instead of total amount.");
    frame.declare_submodule_sequence ("fetch", Attribute::Const, "\
List of columns to fetch for the summary.", FetchPretty::load_syntax);
    frame.declare_integer ("precision", Attribute::Const,
                "Number of digits to print after decimal point.");
    frame.set ("precision", 2);
  }
} SummarySimple_syntax;

// summary_simple.C ends here.
