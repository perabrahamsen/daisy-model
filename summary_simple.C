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
#include "destination.h"
#include "select.h"
#include "treelog.h"
#include "tmpstream.h"
#include "mathlib.h"

#include <string>
using namespace std;

struct SummarySimple : public Summary
{
  static const symbol default_description;
  const symbol description;
  const symbol title;
  const symbol sum_name;
  const symbol period;

  // Content.
  const int precision;
  static int width (double value);
  static double period_factor (symbol period, int hours);
    
  // Fetchers.
  struct Fetch : public Destination
  {
    // Content.
    const symbol tag;
    const double factor;
    const symbol name;
    string select_dimension;

    // State.
    enum { NewContent, Content, Flux, Error } type;
    double initial;
    double last;
    double sum;

    // Destination
    void error ()
    { type = Error; }
    void missing ()
    { 
      switch (type)
	{
	case NewContent:
	case Content:
	  type = Error;
	  break;
	case Flux:
	  break;
	case Error:
	  break;
	}
    }
    void add (const vector<double>&)
    { type = Error; }
    void add (const double value)
    { 
      switch (type)
	{
	case NewContent:
	  type = Content;
	  initial = last = value;
	  break;
	case Content:
	  last = value;
	case Flux:
	  sum += value;
	  break;
	case Error:
	  break;
	}
    }
    void add (const symbol)
    { type = Error; }

    // Create and Destroy.
    void clear ()
    { 
      if (type == Content)
	type = NewContent;
      else if (type == Flux)
	sum = 0; 
    }
    static void load_syntax (Syntax& syntax, AttributeList& alist)
    { 
      alist.add ("description", "A summary file line.");
      syntax.add ("tag", Syntax::String, Syntax::Const, "\
The tag of a column in the log file to summarize in this line.");
      syntax.add ("factor", Syntax::None (), Syntax::Const, "\
Factor to multiply with to get the sum.\n\
Typically 1.0 to add this line, or -1.0 to subtract it.");
      alist.add ("factor", 1.0);
      syntax.add ("name", Syntax::String, Syntax::OptionalConst, "\
Name to use for this line.  By default use the tag.");
      syntax.order ("tag");
    }
    Fetch (const AttributeList& al)
      : tag (al.identifier ("tag")),
	factor (al.number ("factor")),
	name (al.check ("name") ? al.identifier ("name") : tag),
	type (Error),
	initial (-42.42e42),
	last (-42.42e42),
	sum (-42.42e42)
    { }
    const string dimension (const symbol period)
    {
      if (type != Flux)
	return select_dimension;

      const int size = select_dimension.size ();
      if (size > 1)
	{
	  const char last = select_dimension[size - 1];
	  const char second_last = select_dimension[size - 2];
	  if (second_last == '/'
	      && (last == 'h' || last == 'd' || last == 'w' 
		  || last == 'm' || last == 'y'))
	    {
	      const string strip = select_dimension.substr (0, size - 2);
	      if (period.name () != "")
		return strip + "/" + period;
	      else
		return strip;
	    }
	}
      if (period.name () == "h")
	return select_dimension;
      else if (period.name () == "")
	return select_dimension + "h";
      else
	return select_dimension + "h/" + period;
    }
    int name_size ()
    { 
      if (type == Content)
	return name.name ().size () + 6;
      else
	return name.name ().size ();
    }
    int value_size (double& total, const symbol period, const int hours)
    {
      double value = 0.0;
      switch (type)
	{
	case Error:
	case NewContent:
	  break;
	case Content:
	  value = (last - initial) * factor;
	  break;
	case Flux:
	  value = sum * factor * period_factor (period, hours);
	  break;
	}
      total += value;
      return width (value);
    }
    void summarize (ostream& out, const int width, 
		    const symbol period, const int hours)
    {
      if (type == Content)
	out << "delta ";
      out << name << " = ";
      out.width (width);
      switch (type)
	{
	case NewContent:
	  out << "no data";
	  break;
	case Content:
	  {
	    const double value = (last - initial) * factor;
	    out << value;
	  }
	  break;
	case Flux:
	  {
	    const double value = sum * factor * period_factor (period, hours);
	    out << value;
	  }
	  break;
	case Error:
	  out << "bogus data";
	  break;
	}
      out << " " << "[" << dimension (period) << "]\n";
    }
  };
  const vector<Fetch*> fetch;

  // Create and Destroy.
  void clear ();
  void initialize (vector<Select*>&, Treelog&);
  explicit SummarySimple (const AttributeList&);
  void summarize (int hours, Treelog&);
};

const symbol SummarySimple::default_description ("\
A simple log file summary model.");

int 
SummarySimple::width (const double value)
{
  if (!isnormal (value))
    return 0;

  const int absolute = double2int (floor (log10 (fabs (value)))) + 1;
  if (value < 0)
    return absolute + 1;
  else
    return absolute;
}

double
SummarySimple::period_factor (const symbol period, const int hours)
{ 
  if (period.name () == "")
    return 1.0;
  if (period.name () == "y")
    return 365.2425 * 24.0 / hours;
  if (period.name () == "m")
    return 30.0 * 24.0 / hours;
  if (period.name () == "w")
    return 7.0 * 24.0 / hours;
  if (period.name () == "d")
    return 24.0 / hours;
  if (period.name () == "h")
    return 1.0 / hours;
  return -42.42e42;
}
    
void
SummarySimple::clear ()
{ 
  for (unsigned int i = 0; i != fetch.size (); i++)
    fetch[i]->clear ();
}

void
SummarySimple::initialize (vector<Select*>& select, Treelog& msg)
{ 
  Treelog::Open nest (msg, name);

  for (unsigned int i = 0; i != fetch.size (); i++)
    {
      Treelog::Open nest (msg, fetch[i]->tag);
      bool found = false;
      
      for (unsigned int j = 0; j != select.size (); j++)
	{
	  Treelog::Open nest (msg, select[j]->tag ());
	  
	  if (fetch[i]->tag == select[j]->tag ())
	    if (found)
	      msg.warning ("Duplicate tag ignored");
	    else
	      {	
		select[j]->add_dest (fetch[i]);
		fetch[i]->select_dimension = select[j]->dimension ();
		fetch[i]->type = (select[j]->flux && !select[j]->accumulate)
		  ? Fetch::Flux 
		  : Fetch::NewContent;
		found = true;
	      }
	}
      if (!found)
	msg.warning ("No tag found");
    }
}

SummarySimple::SummarySimple (const AttributeList& al)
  : Summary (al),
    description (al.identifier ("description")),
    title (al.check ("title") ? al.identifier ("title") : name),
    sum_name (al.identifier ("sum_name")),
    period (al.check ("period") ? al.identifier ("period") : symbol ("")),
    precision (al.integer ("precision")),
    fetch (map_construct<Fetch> (al.alist_sequence ("fetch")))
{ }

void 
SummarySimple::summarize (const int hours, Treelog& msg)
{
  Treelog::Open nest (msg, title);
  TmpStream tmp;
  tmp ().precision (precision);
  tmp ().flags (ios::right | ios::fixed);
  if (description != default_description)
    tmp () << description << "\n\n";

  double total = 0.0;
  const int sum_size = sum_name.name ().size ();
  int max_size = sum_size;
  for (unsigned int i = 0; i < fetch.size (); i++)
    max_size = max (max_size, fetch[i]->name_size ());
  int max_digits = 0;
  for (unsigned int i = 0; i < fetch.size (); i++)
    max_digits = max (max_digits, fetch[i]->value_size (total, period, hours));
  max_digits = max (max_digits, width (total));
  const double width = max_digits + (precision > 0 ? 1 : 0) + precision;
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
	
      tmp () << string (max_size - fetch[i]->name_size (), ' ');
      fetch[i]->summarize (tmp (), width, period, hours);
    }
  tmp () << string (max_size + 3 + width + 3 + dim_size, '-') << "\n"
	 << string (max_size - sum_size, ' ') << sum_name << " = ";
  tmp ().width (width);
  tmp () << total;
  if (same_dim)
    tmp () << " [" << last_dim << "]";
  tmp () << "\n" << string (max_size + 3, ' ') << string (width, '=');
  msg.message (tmp.str ());
}

static struct SummarySimpleSyntax
{
  static Summary& make (const AttributeList& al)
  { return *new SummarySimple (al); }
  SummarySimpleSyntax ()
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      syntax.add ("description", Syntax::String, Syntax::Const,
		  "Description of this summary format.");
      alist.add ("description", SummarySimple::default_description);
      syntax.add ("title", Syntax::String, Syntax::OptionalConst,
		  "Title of this summary.\n\
By default, use the name of the parameterization.");
      syntax.add ("sum_name", Syntax::String, Syntax::Const,
		  "Name of the sum of all the entries.");
      alist.add ("sum_name", "Sum");	
      syntax.add ("period", Syntax::String, Syntax::OptionalConst, "\
Set this to 'y', 'm', 'w', 'd' or 'h' to get fluxes per time period\n\
instead of total amount.");
      syntax.add_submodule_sequence ("fetch", Syntax::Const, "\
List of columns to fetch for the summary.",
				     SummarySimple::Fetch::load_syntax);
      syntax.add ("precision", Syntax::Integer, Syntax::Const,
		  "Number of digits to print after decimal point.");
      alist.add ("precision", 2);
	
      Librarian<Summary>::add_type ("simple", alist, syntax, &make);
    }
} SummarySimple_syntax;
