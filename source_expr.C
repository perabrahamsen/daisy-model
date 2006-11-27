// source_expr.C -- Data source for gnuplot interface 
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

#include "source_file.h"
#include "scope_table.h"
#include "number.h"
#include "alist.h"

struct SourceExpr : public SourceFile
{
  // Content.
  const std::auto_ptr<Number> expr;
  const std::string title_;
  symbol dimension_;

  // Interface.
public:
  const std::string& title () const
  { return title_; }
  symbol dimension () const 
  { return dimension_; }

  // Read. 
public:
 bool load (Treelog& msg);

  // Create and Destroy.
public:
  explicit SourceExpr (Block& al);
  ~SourceExpr ();
};

bool
SourceExpr::load (Treelog& msg)
{
  // Lex it.
  if (!read_header (msg))
    return false;

  // Scope
  ScopeTable scope (lex);
  if (!expr->initialize (msg) || !expr->check (scope, msg))
    {
      lex.error ("Bad expression");
      return false;
    }
  expr->tick (scope, msg);
  dimension_ = expr->dimension (scope);

  // Read data.
  Time last_time (9999, 12, 31, 23);
  std::vector<double> vals;
  while (lex.good ())
    {
      // Read entries.
      Time time (9999, 1, 1, 0);
      std::vector<std::string> entries;
      if (!read_entry (entries, time))
        continue;

      // Set it.
      scope.set (entries);

      // Missing value.
      if (expr->missing (scope))
	continue;

      // Store it.
      if (time != last_time)
	{
          if (vals.size () > 0)
            add_entry (last_time, vals);
	  last_time = time;
	}
      vals.push_back (expr->value (scope));
    }
  if (vals.size () > 0)
    add_entry (last_time, vals);

  // Done.
  return true;
}

SourceExpr::SourceExpr (Block& al)
  : SourceFile (al),
    expr (Librarian<Number>::build_item (al, "expr")),
    title_ (al.name ("title", expr->title ())),
    dimension_ ("UNINITIALIZED")
{ }

SourceExpr::~SourceExpr ()
{ }


static struct SourceExprSyntax
{
  static Source& make (Block& al)
  { return *new SourceExpr (al); }

  SourceExprSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    SourceFile::load_style (syntax, alist, "\
By default the name of the 'expr' object.");
    alist.add ("description", 
	       "Read a daisy log, weather or data file.\n\
Calculate a single value for each time step, based on the value\n\
in the various columns.");
    syntax.add ("expr", Librarian<Number>::library (), 
		Syntax::Const, Syntax::Singleton, "\
Expression for calculating the value for this source for each row.\n\
The expression can refer to the value in a specific column by the tag\n\
for that column.");

    Librarian<Source>::add_type ("arithmetic", alist, syntax, &make);
  }
} SourceExpr_syntax;

// source_expr.C ends here.
