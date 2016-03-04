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

#define BUILD_DLL
#include "source_file.h"
#include "scope_table.h"
#include "boolean.h"
#include "number.h"
#include "librarian.h"
#include "frame.h"
#include "units.h"

struct SourceExpr : public SourceFile
{
  const Units& units;

  // Content.
  const std::unique_ptr<Number> expr;
  const std::unique_ptr<Boolean> valid;
  const symbol title_;
  symbol dimension_;

  // Interface.
public:
  symbol title () const
  { return title_; }
  symbol dimension () const 
  { return dimension_; }

  // Read. 
public:
 bool load (Treelog& msg);

  // Create and Destroy.
public:
  explicit SourceExpr (const BlockModel& al);
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
  if (!expr->initialize (units, scope, msg) || !expr->check (units, scope, msg))
    {
      lex.error ("Bad expression");
      return false;
    }
  expr->tick (units, scope, msg);
  dimension_ = expr->dimension (scope);
  if (accumulate ())
    {
      const symbol accumulated = Units::multiply (dimension_, timestep);
      if (accumulated != Attribute::Unknown ())
        dimension_ = accumulated;
    }

  if (!valid->initialize (units, scope, msg)
      || !valid->check (units, scope, msg))
    {
      lex.error ("Invalid expression");
      return false;
    }
  valid->tick (units, scope, msg);


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

      // Missing or invalid value.
      if (expr->missing (scope))
	continue;
      if (valid->missing (scope))
	continue;
      if (!valid->value (scope))
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

SourceExpr::SourceExpr (const BlockModel& al)
  : SourceFile (al),
    units (al.units ()),
    expr (Librarian::build_item<Number> (al, "expr")),
    valid (Librarian::build_item<Boolean> (al, "valid")),
    title_ (al.name ("title", expr->title ())),
    dimension_ ("UNINITIALIZED")
{ }

SourceExpr::~SourceExpr ()
{ }


static struct SourceExprSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new SourceExpr (al); }

  SourceExprSyntax ()
    : DeclareModel (Source::component, "arithmetic", 
	       "Read a daisy log, weather or data file.\n\
Calculate a single value for each time step, based on the value\n\
in the various columns.")
  { }
  void load_frame (Frame& frame) const
  { 
    SourceFile::load_style (frame, "\
By default the name of the 'expr' object.");
    frame.declare_object ("expr", Number::component, 
                          Attribute::Const, Attribute::Singleton, "\
Expression for calculating the value for this source for each row.\n\
The expression can refer to the value in a specific column by the tag\n\
for that column.");
    frame.declare_object ("valid", Boolean::component, 
                          Attribute::Const, Attribute::Singleton, "\
Ignore entries if this boolean expression is false.");
    frame.set ("valid", "true");
  }
} SourceExpr_syntax;

// source_expr.C ends here.
