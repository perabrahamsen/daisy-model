// program_extract.C -- Extract data from table.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
// Copyright 2007, 2008, 2009 Per Abrahamsen and KVL.
// Copyright 2015 Per Abrahamsen and KU.
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

#include "program.h"
#include "lexer_table.h"
#include "scope_table.h"
#include "number.h"
#include "boolean.h"
#include "librarian.h"
#include "memutils.h"
#include "assertion.h"
#include "vcheck.h"
#include "mathlib.h"
#include <sstream>

// The 'listsum' Component.

class Listsum : public Model
{
  // Content.
public:
  static const char *const component;
  symbol library_id () const;

  // Simulation.
public:
  virtual void add (double item) = 0;
  virtual double result () const = 0;
  virtual bool valid () const
  { return true; }

  // Create and Destroy.
protected:
  Listsum ();
public:
  ~Listsum ();
};

const char *const Listsum::component = "listsum";

symbol 
Listsum::library_id () const
{
  static const symbol id (component);
  return id;
}

Listsum::Listsum ()
{ }

Listsum::~Listsum ()
{ }

static struct ListsumInit : public DeclareComponent 
{
  ListsumInit ()
    : DeclareComponent (Listsum::component, "\
Condense a list of numbers to a single number.")
  { }
} Listsum_init;

// The 'sum' model.

struct ListsumSum : public Listsum
{
  double sum;

  // Simulation.
  void add (double value)
  { sum += value; }
  double result () const
  { return sum; }

  // Create and Destroy.
  ListsumSum (const BlockModel&)
    : sum (0.0)
  { }
  ~ListsumSum ()
  { }
};

static struct ListsumSumSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ListsumSum (al); }
  ListsumSumSyntax ()
    : DeclareModel (Listsum::component, "sum", "\
The sum of all members of a list.")
  { }
  void load_frame (Frame& frame) const
  { }
} ListsumSum_syntax;

// The 'max' model.

struct ListsumMax : public Listsum
{
  double max;

  // Simulation.
  void add (double value)
  {
    if (value > max)
      max = value; 
  }
  double result () const
  { return max; }

  // Create and Destroy.
  ListsumMax (const BlockModel&)
    : max (0.0)
  { }
  ~ListsumMax ()
  { }
};

static struct ListsumMaxSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ListsumMax (al); }
  ListsumMaxSyntax ()
    : DeclareModel (Listsum::component, "max", "\
The maximum of all members of a list.")
  { }
  void load_frame (Frame& frame) const
  { }
} ListsumMax_syntax;

// The 'min' model.

struct ListsumMin : public Listsum
{
  double min;

  // Simulation.
  void add (double value)
  { 
    if (!std::isfinite (min) || value < min)
      min = value;
    }
  double result () const
  { return min; }

  // Create and Destroy.
  ListsumMin (const BlockModel&)
    : min (NAN)
  { }
  ~ListsumMin ()
  { }
};

static struct ListsumMinSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ListsumMin (al); }
  ListsumMinSyntax ()
    : DeclareModel (Listsum::component, "min", "\
The minimum of all members of a list.")
  { }
  void load_frame (Frame& frame) const
  { }
} ListsumMin_syntax;

// The 'count' model.

struct ListsumCount : public Listsum
{
  double count;

  // Simulation.
  void add (double)
  { count += 1.0; }
  double result () const
  { return count; }

  // Create and Destroy.
  ListsumCount (const BlockModel&)
    : count (0.0)
  { }
  ~ListsumCount ()
  { }
};

static struct ListsumCountSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ListsumCount (al); }
  ListsumCountSyntax ()
    : DeclareModel (Listsum::component, "count", "\
Count all members of a list.")
  { }
  void load_frame (Frame& frame) const
  { }
} ListsumCount_syntax;

// The 'arithmetic' model.

struct ListsumArithmetic : public Listsum
{
  double sum;
  double count;

  // Simulation.
  void add (double value)
  {
    count  += 1.0; 
    sum += value;
  }
  double result () const
  { return sum / count; }
  bool valid () const 
  { return count > 0.1; }

  // Create and Destroy.
  ListsumArithmetic (const BlockModel&)
    : sum (0.0),
      count (0.0)
  { }
  ~ListsumArithmetic ()
  { }
};

static struct ListsumArithmeticSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ListsumArithmetic (al); }
  ListsumArithmeticSyntax ()
    : DeclareModel (Listsum::component, "arithmetic", "\
Arithmetic average of all members of a list.")
  { }
  void load_frame (Frame& frame) const
  { }
} ListsumArithmetic_syntax;

// The 'extract' program.

struct ProgramExtract : public Program
{
  // Data.
  const Metalib& metalib;
  const Units& units;
  LexerTable lex;
  const std::unique_ptr<Number> expr;
  const std::unique_ptr<Boolean> valid;
  const symbol handle;
  const std::vector<symbol> columns;
  const std::vector<symbol> rows;
  const symbol column_separator;
  const symbol row_separator;

  // Use.
  bool run (Treelog& msg)
  {
    // Lex it.
    if (!lex.read_header (msg))
      return false;
    
    bool ok = true;
    std::vector<int> column_c;
    for (size_t i = 0; i < columns.size (); i++)
      {
	const int c = lex.find_tag (columns[i]);
	column_c.push_back (c);
	if (c < 0)
	  {
	    lex.error ("Can't find column '" + columns[i] + "'");
	    ok = false;
	  }
      }
    std::vector<int> row_c;
    for (size_t i = 0; i < rows.size (); i++)
      {
	const int c = lex.find_tag (rows[i]);
	row_c.push_back (c);
	if (c < 0)
	  {
	    lex.error ("Can't find row '" + rows[i] + "'");
	    ok = false;
	  }
      }
    if (!ok)
      return false;

    // Scope
    ScopeTable scope (lex);
    if (!expr->initialize (units, scope, msg)
	|| !expr->check (units, scope, msg))
      {
	lex.error ("Bad expression");
	ok = false;
      }
    expr->tick (units, scope, msg);

    if (!valid->initialize (units, scope, msg)
	|| !valid->check (units, scope, msg))
      {
	lex.error ("Bad 'valid' expression");
	ok = false;
      }
    valid->tick (units, scope, msg);

    if (!ok)
      return false;

    // All handles.
    typedef auto_map<std::string, Listsum*> handlemap_t;
    handlemap_t handles;

    typedef std::set<std::string> sset;
    sset column_names;
    sset row_names;

    // Read data.
    while (lex.good ())
      {
        // Read entries.
        std::vector<std::string> entries;
        if (!lex.get_entries (entries))
	  continue;

	// Set it.
	scope.set (entries);
      
	// Missing value.
	if (expr->missing (scope))
	  continue;
	if (valid->missing (scope))
	  continue;
	if (!valid->value (scope))
	  continue;
	
	const double value = expr->value (scope);

	std::string col = "";
	for (size_t i = 0; i < column_c.size (); i++)
	  {
	    const int c = column_c[i];
	    daisy_assert (c >= 0);
	    if (i > 0)
	      col += column_separator.name ();
	    col += entries[c];
	  }
	column_names.insert (col);

	std::string row = "";
	for (size_t i = 0; i < row_c.size (); i++)
	  {
	    const int c = row_c[i];
	    daisy_assert (c >= 0);
	    if (i > 0)
	      row += row_separator.name ();
	    row += entries[c];
	  }
	row_names.insert (row);
	
	std::string cell = col + "@" + row;

	const handlemap_t::iterator i = handles.find (cell);
	Listsum* handler = NULL;
	if (i == handles.end ())
	  {
	    handler = Librarian::build_stock<Listsum> (metalib, msg,
						       handle, __FUNCTION__);
	    handles[cell] = handler;
	  }
	else
	  handler = (*i).second;
	if (!handler)
	  return false;
	
	// Store it.
	handler->add (value);
      }
    std::ostringstream out;
    out << "There are " << column_names.size () << "x" << row_names.size () 
	<< " = " << handles.size () << " answers\n";

    for (sset::const_iterator c = column_names.begin ();
	 c != column_names.end ();
	 c++)
      out << "\t" << *c;

    for (sset::const_iterator r = row_names.begin ();
	 r != row_names.end ();
	 r++)
      {
	out << "\n" << *r;
	for (sset::const_iterator c = column_names.begin ();
	     c != column_names.end ();
	     c++)
	  {
	    out << "\t";
	    const std::string e = *c + "@" + *r;
	    const handlemap_t::iterator i = handles.find (e);
	    if (i == handles.end ())
	      out << "nan";
	    else
	      out << (*i).second->result ();
	  }
      }

    msg.message (out.str ());

    
    return true;
  }
  
  // Create and Destroy.
  void initialize (Block&)
  { }
  
  bool check (Treelog&)
  { return true; }
  
  ProgramExtract (const BlockModel& al)
    : Program (al),
      metalib (al.metalib ()),
      units (al.units ()),
      lex (al),
      expr (Librarian::build_item<Number> (al, "expr")),
      valid (Librarian::build_item<Boolean> (al, "valid")),
      handle (al.name ("handle")),
      columns (al.name_sequence ("columns")),
      rows (al.name_sequence ("rows")),
      column_separator (al.name ("column_separator")),
      row_separator (al.name ("row_separator"))
  { }
  ~ProgramExtract ()
  { }
};

static struct ProgramExtractSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ProgramExtract (al); }
  ProgramExtractSyntax ()
    : DeclareModel (Program::component, "extract", "\
Extract data from a data file.")
  { }
  void load_frame (Frame& frame) const
  { 
    LexerTable::load_syntax (frame);
    frame.declare_object ("expr", Number::component, 
                       Attribute::Const, Attribute::Singleton, "\
Expression to extract.\n\
The expression can refer to the value in a specific column by the tag\n\
for that column.");
    frame.declare_object ("valid", Boolean::component, 
                          Attribute::Const, Attribute::Singleton, "\
Ignore entries if this boolean expression is false.");
    frame.set ("valid", "true");
    frame.declare_string ("handle", Attribute::Const, "\
How to handle extracted data.");
    static const VCheck::Buildable is_handle (Listsum::component);
    frame.set_check ("handle", is_handle);
    frame.declare_string ("columns", Attribute::Const, Attribute::Variable, "\
List of entries to show as columns in the table.\n\
Each combination of values will be one column.");
    frame.declare_string ("rows", Attribute::Const, Attribute::Variable, "\
List of entries to show as columns in the table.\n\
Each combination of values will be one row.");
    frame.declare_string ("column_separator", Attribute::Const, "\
String to seperate values within a column descriptor.");
    frame.set ("column_separator", "+");
    frame.declare_string ("row_separator", Attribute::Const, "\
String to seperate values within a row descriptor.");
    frame.set ("row_separator", "+");
  }
} ProgramExtract_syntax;

// program_extract.C ends here.
