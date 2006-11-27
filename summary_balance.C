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


#include "summary.h"
#include "block.h"
#include "alist.h"
#include "fetch.h"
#include "select.h"
#include "treelog.h"
#include "memutils.h"
#include <sstream>
#include <algorithm>
#include <fstream>
#include <string>
using namespace std;

struct SummaryBalance : public Summary
{
  static const char *const default_description;
  const symbol description;
  const string file;
  const symbol title;
  const symbol period;

  // Content.
  const int precision;
  const bool require_top;
  const vector<symbol> input;
  const vector<symbol> output;
  const vector<symbol> content;
  const class ConstructFetch : public vector<Fetch*> 
  {
    void add (const vector<symbol>& a)
    {
      for (size_t i = 0; i < a.size (); i++)
        push_back (new Fetch (a[i]));
    }
  public:
    ConstructFetch (const vector<symbol>& a, 
                    const vector<symbol>& b, 
                    const vector<symbol>& c)
    { add (a); add (b); add (c); }
    ~ConstructFetch ()
    { sequence_delete (begin (), end ()); }
  } 
  fetch;

  // Create and Destroy.
  void clear ();
  void initialize (vector<Select*>&, Treelog&);
  explicit SummaryBalance (Block&);
  bool in_list (size_t i, const vector<symbol>& names) const;
  double find_total (const vector<symbol>& names, 
                     int& max_digits, int hours) const;
  string print_entries (ostream& out, const vector<symbol>& names, 
                        int max_size, int width, int hours) const;
  void print_balance (ostream& out,
                      const string& title, double total, const string& dim,
                      int dim_size, int max_size,
                      int width) const;
  void summarize (int hours, Treelog&) const;
};

const char *const SummaryBalance::default_description  = "\
A summary model providing a balance for a log parameterization.";

void
SummaryBalance::clear ()
{ Fetch::clear (fetch); }

void
SummaryBalance::initialize (vector<Select*>& select, Treelog& msg)
{ 
  Treelog::Open nest (msg, name);
  Fetch::initialize (fetch, select, msg);
}

SummaryBalance::SummaryBalance (Block& al)
  : Summary (al),
    description (al.identifier ("description")),
    file (al.name ("where", "")),
    title (al.check ("title") ? al.identifier ("title") : name),
    period (al.check ("period") ? al.identifier ("period") : symbol ("")),
    precision (al.integer ("precision")),
    require_top (al.flag ("require_top")),
    input (al.identifier_sequence ("input")),
    output (al.identifier_sequence ("output")),
    content (al.identifier_sequence ("content")),
    fetch (input, output, content)
{ }

bool
SummaryBalance::in_list (size_t i, const vector<symbol>& names) const
{ return find (names.begin (), names.end (), fetch[i]->tag) != names.end (); }

double
SummaryBalance::find_total (const vector<symbol>& names, 
                            int& max_digits, int hours) const
{
  double total = 0.0;
  for (size_t i = 0; i < fetch.size (); i++)
    if (in_list (i, names))
      max_digits = max (max_digits, 
			fetch[i]->value_size (total, period, hours));
  return total;
}

string
SummaryBalance::print_entries (ostream& out, const vector<symbol>& names, 
                               const int max_size, const int width, 
                               const int hours) const
{
  string dim = Syntax::User ();
  for (unsigned int i = 0; i < fetch.size (); i++)
    {
      if (!in_list (i, names))
        continue;
          
      if (dim == Syntax::User ())
        dim = fetch[i]->dimension (period);
      else if (fetch[i]->dimension (period) != dim)
        dim = Syntax::Unknown ();

      out << string (max_size - fetch[i]->name_size (), ' ');
      fetch[i]->summarize (out, width, period, hours);
    }
  return dim;
}

void
SummaryBalance::print_balance (ostream& out,
                               const string& title, const double total, 
                               const string& dim,
                               const int /* dim_size */, const int max_size,
                               const int width) const
{
  out << string (max_size + 3, ' ') << string (width, '-') << "\n"
      << string (max_size - title.size (), ' ') << title << " = ";
  out.width (width);
  out << total;
  if (dim != Syntax::Unknown ())
    out << " [" << dim << "]";
  out << "\n";
}

void 
SummaryBalance::summarize (const int hours, Treelog& msg) const
{
  Treelog::Open nest (msg, title);

  // We write the summary to a string at first.
  std::ostringstream tmp;
  tmp.precision (precision);
  tmp.flags (ios::right | ios::fixed);
  if (description.name () != default_description)
    tmp << description << "\n\n";

  // Find width of tags.
  const string total_title = "Balance (= In - Out - Increase)";
  const string content_title = "Total increase in content";
  size_t max_size = max(total_title.size (), content_title.size ());
  for (unsigned int i = 0; i < fetch.size (); i++)
    max_size = max (max_size, fetch[i]->name_size ());

  // Find width and total values
  int max_digits = 0;
  const double total_input = find_total (input, max_digits, hours);
  const double total_output = find_total (output, max_digits, hours);
  const double total_content = find_total (content, max_digits, hours);
  const double total = total_input - total_output - total_content;
  max_digits = max (max_digits, Fetch::width (total));

  // Find total width.
  const int width = max_digits + (precision > 0 ? 1 : 0) + precision;

  // Find width of dimensions.
  size_t dim_size = 0;
  for (unsigned int i = 0; i < fetch.size (); i++)
    dim_size = max (dim_size, fetch[i]->dimension (period).size ());

  // Print all entries.
  string shared_dim = Syntax::User ();
  if (input.size () > 0)
    {
      const string dim = print_entries (tmp, input, max_size, width, hours);
      print_balance (tmp, "Total input", total_input, dim,
                     dim_size, max_size, width);
      shared_dim = dim;
      tmp << "\n";
    }

  if (output.size () > 0)
    {
      const string dim = print_entries (tmp, output, 
                                        max_size, width, hours);
      print_balance (tmp, "Total output", total_output, dim,
                     dim_size, max_size, width);
      if (shared_dim == Syntax::User ()) 
        shared_dim = dim;
      else if (dim != shared_dim)
        shared_dim = Syntax::Unknown ();
      tmp << "\n";
    }

  if (content.size () > 0)
    {
      const string dim = print_entries (tmp, content, 
                                        max_size, width, hours);
      print_balance (tmp, content_title, total_content, dim,
                     dim_size, max_size, width);
      if (shared_dim == Syntax::User ()) 
        shared_dim = dim;
      else if (dim != shared_dim)
        shared_dim = Syntax::Unknown ();
      tmp << "\n";
    }

  print_balance (tmp, total_title, total, 
                 shared_dim, dim_size, max_size, width);
  tmp << string (max_size + 3, ' ') << string (width, '=');

  // Where?
  if (file == "")
    msg.message (tmp.str ());
  else
    { 
      ofstream out (file.c_str ());
      out << tmp.str ();
      if (! out.good ())
        msg.error ("Could not write to '" + file + "'");
    } 
}

static struct SummaryBalanceSyntax
{
  static Summary& make (Block& al)
  { return *new SummaryBalance (al); }
  SummaryBalanceSyntax ()
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      syntax.add ("description", Syntax::String, Syntax::Const,
		  "Description of this summary format.");
      alist.add ("description", SummaryBalance::default_description);
      syntax.add ("where", Syntax::String, Syntax::OptionalConst,
                  "File name to store the summary.\n\
By default, the summary will be stored in daisy.log and the screen.");
      syntax.add ("title", Syntax::String, Syntax::OptionalConst,
		  "Title of this summary.\n\
By default, use the name of the parameterization.");
      syntax.add ("period", Syntax::String, Syntax::OptionalConst, "\
Set this to 'y', 'm', 'w', 'd' or 'h' to get fluxes per time period\n\
instead of total amount.");
      syntax.add ("precision", Syntax::Integer, Syntax::Const,
		  "Number of digits to print after decimal point.");
      alist.add ("precision", 2);
      syntax.add ("require_top", Syntax::Boolean, Syntax::Const, "\
If the balance only hold true when logging the top of the soil, i.e. the\n\
`to' parameter of the log model is 0, this flag should be set.");
      alist.add ("require_top", false);
      syntax.add ("input", Syntax::String, Syntax::Const, Syntax::Sequence,
                  "Tags of columns in log file representing inputs.");
      syntax.add ("output", Syntax::String, Syntax::Const, Syntax::Sequence,
                  "Tags of columns in log file representing outputs.");
      syntax.add ("content", Syntax::String, Syntax::Const, Syntax::Sequence,
                  "Tags of columns in log file representing content.");
      Librarian<Summary>::add_type ("balance", alist, syntax, &make);
    }
} SummaryBalance_syntax;
