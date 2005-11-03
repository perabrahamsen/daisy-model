// source_combine.C -- Combine data sources for gnuplot interface 
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

#include "source.h"
#include "number.h"
#include "scope.h"
#include "vcheck.h"
#include "memutils.h"
#include <sstream>

// ScopeScource
class ScopeSources : public Scope
{
  // Content.
private:
  const std::vector<Source*> source;
  std::vector<size_t> index;
public:
  Time now;

  // Interface.
public:
  bool has_number (const std::string& tag) const
  {
    for (size_t i = 0; i < source.size (); i++)
      if (source[i]->title () == tag 
	  && index[i] < source[i]->value ().size ()
          && source[i]->time ().at (index[i]) == now)
	return true;
    
    return false;
  }
  double number (const std::string& tag) const
  {
    for (size_t i = 0; i < source.size (); i++)
      if (source[i]->title () == tag)
	{
	  daisy_assert (index[i] < source[i]->value ().size ());
          daisy_assert (source[i]->time ().at (index[i]) == now);
	  return source[i]->value ().at (index[i]);
	}
    daisy_assert (false);
  }
  const std::string& dimension (const std::string& tag) const
  {
    for (size_t i = 0; i < source.size (); i++)
      if (source[i]->title () == tag)
	return source[i]->dimension ();
    daisy_assert (false);
  }

  // Propagate.
  bool load (Treelog& msg)
  {
    daisy_assert (index.size () == source.size ());

    // Propagate.
    bool ok = true;
    for (size_t i = 0; i < source.size (); i++)
      {
	std::ostringstream tmp;
	tmp << "[" << i << "] " << source[i]->title ();
	Treelog::Open nest (msg, tmp.str ());
	if (!source[i]->load (msg))
          ok = false;
      }
    return ok;
  }

  std::string with ()
  {
    if (source.size () > 0)
      return source[0]->with ();
    
    // No sources => no dates => no data.
    return "lines";
  }

  // Loop.
  void first ()
  { 
    fill (index.begin (), index.end (), 0); 
    now = Time (1, 1, 1, 0);
    next ();
  }

  bool done ()
  {
    daisy_assert (index.size () == source.size ());
    for (size_t i = 0; i < index.size (); i++)
      if (index[i] < source[i]->value ().size ())
        return false;

    return true;
  }
      
  void next ()
  {
    daisy_assert (index.size () == source.size ());
    for (size_t i = 0; i < index.size (); i++)
      if (index[i] < source[i]->time ().size ()
          && source[i]->time ().at (index[i]) <= now)
        index[i]++;
    
    now = Time (9999, 12, 31, 23);
    
    for (size_t i = 0; i < index.size (); i++)
      if (index[i] < source[i]->time ().size ()
          && source[i]->time ().at (index[i]) <= now)
        now = source[i]->time ().at (index[i]);
  }

  // Create and Destroy.
public:
  ScopeSources (const std::vector<Source*>& s)
    : source (s),
      index (s.size (), 0),
      now (1, 1, 1, 0)
  { }
  ~ScopeSources ()
  { sequence_delete (source.begin (), source.end ()); }
};

struct SourceCombine : public Source
{
  // Content.
  ScopeSources scope;
  const std::auto_ptr<Number> expr;
  const std::string title_;
  std::string dimension_;
  std::string with_;
  const int style_;
  std::vector<Time> times;
  std::vector<double> values;

  // Interface.
public:
  const std::string& with () const
  { return with_; }
  int style () const 
  { return style_; }
  const std::vector<Time>& time () const
  { return times; }
  const std::vector<double>& value () const
  { return values; }
  const std::vector<double>& ebar () const
  { daisy_assert (false); }
  const std::string& title () const
  { return title_; }
  const std::string& dimension () const 
  { return dimension_; }

  // Read. 
public:
 bool load (Treelog& msg);

  // Create and Destroy.
public:
  explicit SourceCombine (Block& al);
  ~SourceCombine ()
  { }
};

bool
SourceCombine::load (Treelog& msg)
{
  // Propagate.
  scope.load (msg);

  // Scope
  if (!expr->check (scope, msg))
    return false;

  // Extract.
  dimension_ = expr->dimension (scope);
  if (with_ == "")
    with_ = scope.with ();

  // Read data.
  for (scope.first (); !scope.done (); scope.next ())
    {
      if (!expr->missing (scope))
        {
          times.push_back (scope.now);
          values.push_back (expr->value (scope));
        }
    }
  daisy_assert (times.size () == values.size ());

  // Done.
  return true;
}

SourceCombine::SourceCombine (Block& al)
  : Source (al),
    scope (Librarian<Source>::build_vector (al, "source")),
    expr (Librarian<Number>::build_item (al, "expr")),
    title_ (al.name ("title", expr->title ())),
    dimension_ ("UNINITIALIZED"),
    with_ (al.name ("with", "")),
    style_ (al.integer ("style", -1))
{ }

static struct SourceCombineSyntax
{
  static Source& make (Block& al)
  { return *new SourceCombine (al); }

  SourceCombineSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    Source::load_syntax (syntax, alist);
    alist.add ("description", 
	       "Combine data from multiple sources with a single expression.");
    syntax.add ("source", Librarian<Number>::library (), 
		Syntax::Const, Syntax::Sequence, "\
List of sources for data.  The style information for the sources is\n\
ignored, but the dates, title and value is used as specified by\n\
'expr' to calculate the combines date and value pairs.");
    syntax.add ("expr", Librarian<Number>::library (), 
		Syntax::Const, Syntax::Singleton, "\
Expression for calculating the value for this source for each row.\n\
A row is any date found in any of the member of 'source'.  The\n\
expression may refer to the value of each source by its title.");
    syntax.add ("title", Syntax::String, Syntax::OptionalConst, "\
Name of data legend in plot, by default the name of the 'expr' object.");
  syntax.add ("with", Syntax::String, Syntax::OptionalConst, "\
Specify 'points' to plot each point individually, or 'lines' to draw\n\
lines between them.  By default, let the first source decide.");
  static VCheck::Enum with ("lines", "points");
  syntax.add_check ("with", with);
  syntax.add ("style", Syntax::Integer, Syntax::OptionalConst, "\
Style to use for this dataset.  By default, gnuplot will use style 1\n\
for the first source to plot with lines, style 2 for the second, and\n\
so forth until it runs out of styles and has to start over.  Points\n\
work similar, but with its own style counter.  For color plots, points\n\
and lines with the same style number also have the same color.");
    
    Librarian<Source>::add_type ("combine", alist, syntax, &make);
  }
} SourceCombine_syntax;

// source_std.C ends here.
