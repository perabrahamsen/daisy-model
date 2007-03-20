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


#include "summary.h"
#include "alist.h"
#include "fetch.h"
#include "select.h"
#include "treelog.h"
#include "memutils.h"
#include "submodeler.h"
#include <sstream>
#include <fstream>
#include <string>
using namespace std;

struct SummarySimple : public Summary
{
  static const symbol default_description;
  const symbol description;
  const string file;
  const symbol title;
  const bool print_sum;
  const symbol sum_name;
  const symbol period;

  // Content.
  const int precision;
  const vector<Fetch*> fetch;

  // Create and Destroy.
  void clear ();
  void initialize (vector<Select*>&, Treelog&);
  explicit SummarySimple (Block&);
  ~SummarySimple ();
  void summarize (int hours, Treelog&) const;
};

const symbol SummarySimple::default_description ("\
A simple log file summary model.");

void
SummarySimple::clear ()
{ Fetch::clear (fetch); }

void
SummarySimple::initialize (vector<Select*>& select, Treelog& msg)
{ 
  Treelog::Open nest (msg, name);
  Fetch::initialize (fetch, select, msg);
}

SummarySimple::SummarySimple (Block& al)
  : Summary (al),
    description (al.identifier ("description")),
    file (al.name ("where", "")),
    title (al.check ("title") ? al.identifier ("title") : name),
    print_sum (al.flag ("print_sum")),
    sum_name (al.identifier ("sum_name")),
    period (al.check ("period") ? al.identifier ("period") : symbol ("")),
    precision (al.integer ("precision")),
    fetch (map_construct<Fetch> (al.alist_sequence ("fetch")))
{ }

SummarySimple::~SummarySimple ()
{ sequence_delete (fetch.begin (), fetch.end ()); }

void 
SummarySimple::summarize (const int hours, Treelog& msg) const
{
  Treelog::Open nest (msg, title);
  std::ostringstream tmp;
  tmp.precision (precision);
  tmp.flags (ios::right | ios::fixed);
  if (description != default_description)
    tmp << description << "\n\n";

  double total = 0.0;
  const int sum_size = sum_name.name ().size ();
  size_t max_size = print_sum ? sum_size : 0;
  for (unsigned int i = 0; i < fetch.size (); i++)
    max_size = max (max_size, fetch[i]->name_size ());
  int max_digits = 0;
  for (unsigned int i = 0; i < fetch.size (); i++)
    max_digits = max (max_digits, fetch[i]->value_size (total, period, hours));
  max_digits = max (max_digits, Fetch::width (total));
  const int width = max_digits + (precision > 0 ? 1 : 0) + precision;
  size_t dim_size = 0;
  for (unsigned int i = 0; i < fetch.size (); i++)
    dim_size = max (dim_size, fetch[i]->dimension (period).size ());
  string last_dim;
  bool same_dim = true;
  for (unsigned int i = 0; i < fetch.size (); i++)
    {
      if (i == 0)
	last_dim = fetch[i]->dimension (period);
      else if (same_dim && fetch[i]->dimension (period) != last_dim)
	same_dim = false;
	
      tmp << string (max_size - fetch[i]->name_size (), ' ');
      fetch[i]->summarize (tmp, width, period, hours);
    }
  if (print_sum)
  {
    tmp << string (max_size + 3 + width + 3 + dim_size, '-') << "\n"
	   << string (max_size - sum_size, ' ') << sum_name << " = ";
    tmp.width (width);
    tmp << total;
    if (same_dim)
      tmp << " [" << last_dim << "]";
    tmp << "\n" << string (max_size + 3, ' ') << string (width, '=');
  }
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

static struct SummarySimpleSyntax
{
  static Model& make (Block& al)
  { return *new SummarySimple (al); }
  SummarySimpleSyntax ()
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      syntax.add ("description", Syntax::String, Syntax::Const,
		  "Description of this summary format.");
      alist.add ("description", SummarySimple::default_description);
      syntax.add ("where", Syntax::String, Syntax::OptionalConst,
                  "File name to store the summary.\n\
By default, the summary will be stored in daisy.log and the screen.");
      syntax.add ("title", Syntax::String, Syntax::OptionalConst,
		  "Title of this summary.\n\
By default, use the name of the parameterization.");
      syntax.add ("print_sum", Syntax::Boolean, Syntax::Const, 
		  "Print sum of all the summary lines.");
      alist.add ("print_sum", true);
      syntax.add ("sum_name", Syntax::String, Syntax::Const,
		  "Name of the sum of all the entries.");
      alist.add ("sum_name", "Sum");	
      syntax.add ("period", Syntax::String, Syntax::OptionalConst, "\
Set this to 'y', 'm', 'w', 'd' or 'h' to get fluxes per time period\n\
instead of total amount.");
      syntax.add_submodule_sequence ("fetch", Syntax::Const, "\
List of columns to fetch for the summary.", Fetch::load_syntax);
      syntax.add ("precision", Syntax::Integer, Syntax::Const,
		  "Number of digits to print after decimal point.");
      alist.add ("precision", 2);
      Librarian<Summary>::add_type ("simple", alist, syntax, &make);
    }
} SummarySimple_syntax;
