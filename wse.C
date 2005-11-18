// wse.C -- Water Stress Effect on yield.
// 
// Copyright 2004 Per Abrahamsen, Søren Hansen and KVL.
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


#include "wse.h"
#include "program.h"
#include <sstream>
#include "mathlib.h"
#include <memory>

template<>
Librarian<WSE>::Content* Librarian<WSE>::content = NULL;

const char *const WSE::description = "\
The water stress effect on crop growth.";

WSE::WSE (Block& al)
  : name (al.identifier ("type"))
{ }

WSE::~WSE ()
{ }

struct WSE_full : public WSE
{
  double factor (const double water_stress) const
  { return 1.0 - water_stress; }

  WSE_full (Block& al)
    : WSE (al)
  { }
  ~WSE_full ()
  { }
};


static struct WSE_fullSyntax
{
  static WSE& make (Block& al)
  { return *new WSE_full (al); }
  WSE_fullSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", "\
Water stress has full effect on crop growth.\n\
This means that if there is 50% water stress, assimilate production\n\
will be cut into half."); 
    Librarian<WSE>::add_type ("full", alist, syntax, &make);
  }
} WSE_full_syntax;

const AttributeList& 
WSE::default_model ()
{
  static AttributeList alist;
  
  if (!alist.check ("type"))
    alist.add ("type", "full");

  return alist;
}

struct WSE_partial : public WSE
{
  const double y_half;

  double factor (const double water_stress) const
  { 
    if (approximate (y_half, 0.5))
      return 1.0 - water_stress;
    const double divisor = (1.0 - 2 * y_half) * water_stress + y_half;
    const double factor 
      = 1.0 - ((fabs (divisor) < 1e-10) 
               ? 0.0 
               : y_half / divisor);
    return 1.0 + factor * (1.0 - y_half) / (2.0 * y_half - 1.0); 
  }

  WSE_partial (Block& al)
    : WSE (al),
      y_half (al.number ("y_half"))
  { }
  ~WSE_partial ()
  { }
};

static struct WSE_partialSyntax
{
  static WSE& make (Block& al)
  { return *new WSE_partial (al); }
  WSE_partialSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "\
Water stress has partial effect on crop growth.\n\
\n\
With this model, there will be full production when there is enough\n\
available soil water to cover the potential evapotranspiration, and no\n\
production when there is no soil water available.  In between production\n\
is controled by the 'y_half' parameter.\n\
\n\
See SH:REFERENCE for more explanation.");
    syntax.add_fraction ("y_half", Syntax::Const, "\
Effect on assimilate production of water stress.\n\
This parameter specifies the effect on assimilate production\n(\
compared to potential) when the amount of available soil water is\n\
enough to cover exactly half the potential evapotranspiration.");
    Librarian<WSE>::add_type ("partial", alist, syntax, &make);
  }
} WSE_partial_syntax;

struct ProgramWSE_table : public Program
{
  const std::auto_ptr<WSE> wse;
  const int intervals;

  void run (Treelog& msg)
  {
    std::ostringstream tmp;
    tmp << "stress\teffect\n";
    for (int i = 0; i <= intervals; i++)
      {
        const double ws = (i + 0.0) / (intervals + 0.0);
        const double e = wse->factor (ws);
        tmp << ws << "\t" << e << "\n";
      }
    msg.message (tmp.str ());
  }

  // Create and Destroy.
  void initialize (const Syntax*, const AttributeList*, Treelog&)
  { };
  bool check (Treelog&)
  { return true; }
  ProgramWSE_table (Block& al)
    : Program (al),
      wse (Librarian<WSE>::build_item (al, "wse")),
      intervals (al.integer ("intervals"))
  { }
  ~ProgramWSE_table ()
  { }
};

static struct ProgramWSE_tableSyntax
{
  static Program&
  make (Block& al)
  { return *new ProgramWSE_table (al); }
  ProgramWSE_tableSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Generate a table of the water stress effect.");
    syntax.add ("wse", Librarian<WSE>::library (), 
                Syntax::Const, Syntax::Singleton, "\
The water stress effect to show in the table.");
    syntax.add ("intervals", Syntax::Integer, Syntax::Const, "\
Number of intervals in the table.");
    alist.add ("intervals", 10);
    syntax.order ("wse");
    Librarian<Program>::add_type ("wse", alist, syntax, &make);
  }
} ProgramWSE_table_syntax;
