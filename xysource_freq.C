// xysource_freq.h -- Frequency table source for gnuplot interface 
// 
// Copyright 2005 and 2015 Per Abrahamsen and KVL.
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
#include "xysource.h"
#include "lexer_table.h"
#include "scope_table.h"
#include "number.h"
#include "boolean.h"
#include "gnuplot_utils.h"
#include "vcheck.h"
#include "assertion.h"
#include "librarian.h"
#include "frame.h"

class XYSourceFreq : public XYSource
{
  // Content.
  LexerTable lex;
  const symbol with_;
  const int style_;
  std::vector<double> sorts;
  std::vector<double> plots;
  const std::unique_ptr<Number> sort_expr;
  const std::unique_ptr<Number> plot_expr;
  const std::unique_ptr<Boolean> valid;
  const symbol title_;
  const symbol percent;
  const bool use_sort_value;
  const bool plot_on_x;
  symbol sort_dim;
  symbol plot_dim;

  // Interface.
public:
  symbol title () const
  { return title_; }
  const std::vector<double>& x () const
  { 
    if (plot_on_x)
      return plots;
    else 
      return sorts; 
  }
  const std::vector<double>& y () const
  { 
    if (plot_on_x)
      return sorts;
    else 
      return plots; 
  }
  symbol with () const
  { return with_; }
  int style () const 
  { return style_; }
  symbol x_dimension () const 
  { 
    if (plot_on_x)
      return plot_dim;
    else if (use_sort_value)
      return sort_dim;
    else
      return percent;
  }
  symbol y_dimension () const 
  {
    if (!plot_on_x)
      return plot_dim;
    else if (use_sort_value)
      return sort_dim;
    else
      return percent;
  }

  // Read.
private:
  bool read_header (Treelog&);
public:
  bool load (const Units&, Treelog& msg);

  // Create.
public:
  explicit XYSourceFreq (const BlockModel&);
private:
  XYSourceFreq (const XYSourceFreq&);
  XYSourceFreq& operator= (const XYSourceFreq&);
public:
  ~XYSourceFreq ();
};

bool
XYSourceFreq::read_header (Treelog& msg)
{
  // Read header.
  if (!lex.read_header (msg))
    return false;

  return lex.good ();
}

struct SortPlot
{
  double sort;
  double plot;
  bool operator< (const SortPlot& other) const
  { return sort < other.sort; }
  SortPlot (const double s, const double p)
    : sort (s),
      plot (p)
  { }
};

bool
XYSourceFreq::load (const Units& units, Treelog& msg)
{
  // Lex it.
  if (!read_header (msg))
    return false;

  // Scope
  ScopeTable scope (lex);
  {
    bool ok = true;
    if (!sort_expr->initialize (units, scope, msg)
        || !sort_expr->check (units, scope, msg))
      {
        lex.error ("Bad 'sort' expression");
        ok = false;
      }
    sort_expr->tick (units, scope, msg);
    sort_dim = sort_expr->dimension (scope);
    
    if (!plot_expr->initialize (units, scope, msg)
        || !plot_expr->check (units, scope, msg))
      {
        lex.error ("Bad 'plot' expression");
        ok = false;
      }
    plot_expr->tick (units, scope, msg);
    plot_dim = plot_expr->dimension (scope);

    if (!valid->initialize (units, scope, msg)
        || !valid->check (units, scope, msg))
      {
        lex.error ("Bad 'valid' expression");
        ok = false;
      }
    valid->tick (units, scope, msg);

    if (!ok)
      return false;
  }

  // Read data.
  std::vector<SortPlot> values;

  while (lex.good ())
    {
      // Read entries.
      std::vector<std::string> entries;
      if (!lex.get_entries (entries))
        continue;

      // Set it.
      scope.set (entries);
      
      // Missing value.
      if (sort_expr->missing (scope) || plot_expr->missing (scope))
	continue;
      if (valid->missing (scope))
	continue;
      if (!valid->value (scope))
        continue;
      
      // Store it.
      values.push_back (SortPlot (sort_expr->value (scope),
				  plot_expr->value (scope)));
    }
  std::sort (values.begin (), values.end ());
  const double count = values.size ();

  if (count < 1.0)
    {
      msg.error ("No data");
      return false;
    }
  for (size_t i = 0; i < values.size (); i++)
    {
      const double c = i + 1.0;
      if (use_sort_value)
	sorts.push_back (values[i].sort);
      else
	sorts.push_back (100.0 * c / count);
      plots.push_back (values[i].plot);
    }
  daisy_assert (sorts.size () == values.size ());
  daisy_assert (sorts.size () == plots.size ());

  // Done.
  return true;
}

XYSourceFreq::XYSourceFreq (const BlockModel& al)
  : XYSource (al),
    lex (al),
    with_ (al.name ("with", "lines")),
    style_ (al.integer ("style", -1)),
    sort_expr (Librarian::build_item<Number> (al, "sort")),
    plot_expr (al.check ("plot")
	       ? Librarian::build_item<Number> (al, "plot")
	       :  Librarian::build_item<Number> (al, "sort")),
    valid (Librarian::build_item<Boolean> (al, "valid")),
    title_ (al.name ("title", plot_expr->title ())),
    percent (al.name ("percent")),
    use_sort_value (al.flag ("use_sort_value")),
    plot_on_x (al.flag ("plot_on_x")),
    sort_dim ("UNINITIALIZED"),
    plot_dim ("UNINITIALIZED")
{ }

XYSourceFreq::~XYSourceFreq ()
{ }

static struct XYSourceFreqSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new XYSourceFreq (al); }

  XYSourceFreqSyntax ()
    : DeclareModel (XYSource::component, "frequency", 
	       "Read a daisy log, weather or data file.\n\
Plot the frequency of a specific column.")
  { }
  void load_frame (Frame& frame) const
  { 
    LexerTable::load_syntax (frame);
    GnuplotUtil::load_style (frame, "\
By default, data will be drawn with lines.", "\
By default the name of the tag.");
    frame.declare_object ("sort", Number::component, 
                       Attribute::Const, Attribute::Singleton, "\
Expression for calculating the sorting value for this source for each row.\n\
The expression can refer to the value in a specific column by the tag\n\
for that column.");
    frame.declare_object ("plot", Number::component, 
			  Attribute::OptionalConst, Attribute::Singleton, "\
Expression for calculating the plot value for this source for each row.\n\
By default, this will be identical to 'sort'.");
    frame.declare_object ("valid", Boolean::component, 
                          Attribute::Const, Attribute::Singleton, "\
Ignore entries if this boolean expression is false.");
    frame.set ("valid", "true");
    frame.declare_string ("percent", Attribute::Const, "\
Symbol to use for percent.");
    frame.set ("percent", "%");
    frame.declare_boolean ("use_sort_value", Attribute::Const, "\
Show 'sort' value instead of accumulated frequency.");
    frame.set ("use_sort_value", false);
    frame.declare_boolean ("plot_on_x", Attribute::Const, "\
Show plot values on x axis.");
    frame.set ("plot_on_x", true);
  }
} XYSourceFreq_syntax;

// xysource_freq.C ends here.
