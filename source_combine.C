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
#include "scope.h"

// ScopeScource
class ScopeSources : public Scope
{
  // Content.
private:
  const std::vector<Source*> source;
  const vector<size_t> index;
  
  // Interface.
public:
  bool has_number (const std::string& tag) const
  {
    for (size_t i = 0; i < source.size (); i++)
      if (source.title () == tag 
	  && index[i] < source.value ().size ())
	return true;
    
    return false;
  }
  double number (const std::string& tag) const
  {
    for (size_t i = 0; i < source.size (); i++)
      if (source.title () == tag)
	{
	  daisy_assert (index[i] < source.value ().size ());
	  return source.value ().at (index[i]);
	}
    daisy_assert (false);
  }
  const std::string& dimension (const std::string& tag) const
  {
    for (size_t i = 0; i < source.size (); i++)
      if (source.title () == tag)
	return source.dimension ();
    daisy_assert (false);
  }

  // Propagate.
  bool load (Treelog& msg)
  {
    // Propagate.
    for (size_t i = 0; i < source.size (); i++)
      {
	index.push_back (0);
	std::ostringstream tmp;
	tmp << "[" << i << "] " << source[i]->title ();
	Treelog::Open nest (msg, tmp.str ());
	source[i]->load (msg);
      }
    daisy_assert (index.size () == source.size ());
  }
    

  // Create and Destroy.
public:
  ScopeSources (const std::vector<Source*>& s)
    : source (s)
  { }
  ~ScopeSources ()
  { sequence_delete (source.begin (), source.end ()); }
};

struct SourceCombine : public SourceFile
{
  // Content.
  ScopeSources scope;
  const std::auto_ptr<Number> expr;
  const std::string title_;
  std::string dimension_;

  // Interface.
public:
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
  for (size_t i = 0; i < source.size (); i++)
    {
      std::ostringstream tmp;
      tmp << "[" << i << "] " << source[i]->title ();
      Treelog::Open nest (msg, tmp.str ());
      source[i]->load (msg);
    }

  // Scope
  if (!expr->check (scope, msg))
    {
      lex.error ("Bad expression");
      return false;
    }
  dimension_ = expr->dimension (scope);

  // Read data.
  while (!scope.done ())
    {
      
    }
  // Done.
  return true;
}

SourceCombine::SourceCombine (Block& al)
  : SourceFile (al),
    scope (Librarian<Source>::build_vector (al, "source")),
    expr (Librarian<Number>::build_item (al, "expr")),
    title_ (al.name ("title", expr->title ())),
    dimension_ ("UNINITIALIZED")
{ }

SourceCombine::~SourceCombine ()
{ }

static struct SourceCombineSyntax
{
  static Source& make (Block& al)
  { return *new SourceCombine (al); }

  SourceCombineSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    SourceFile::load_syntax (syntax, alist);
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
    
    Librarian<Source>::add_type ("combine", alist, syntax, &make);
  }
} SourceCombine_syntax;

// source_std.C ends here.
