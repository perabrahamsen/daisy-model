// pedotransfer_linear.C -- Two soil chemicals reching for linear.
// 
// Copyright 2002 Per Abrahamsen and KVL.
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


#include "pedo.h"
#include "soil.h"

struct PedotransferLinear : public Pedotransfer
{
  // Parameters.
  typedef map<string, double, less<string>/**/> factor_type;
  /* const */ factor_type factors;
  const double offset;

  // Simulation.
  void set (const Soil&, vector<double>&) const;

  // Create.
  bool check (const Soil&, Treelog& err) const;
  void initialize (const Soil&);
  PedotransferLinear (const AttributeList& al);
};

void 
PedotransferLinear::set (const Soil& soil, vector<double>& array) const
{
  array.insert (array.end (), soil.size () - array.size (), 0.0);

  for (unsigned int i = 0; i < soil.size (); i++)
    {
      double entry = offset;
      for (factor_type::const_iterator f = factors.begin ();
	   f != factors.end ();
	   f++)
	{
	  const string& name = (*f).first;
	  if (soil.has_attribute (name))
	    {
	      const double factor = (*f).second;
	      entry += soil.get_attribute (i, name) * factor;
	    }
	  else 
	    entry = -42.42e42;	// We get an error on 'check' later. 
	}
      array[i] = entry;
    }  
}

bool
PedotransferLinear::check (const Soil& soil, Treelog& err) const
{ 
  bool ok = true;

  for (factor_type::const_iterator i = factors.begin ();
       i != factors.end ();
       i++)
    {
      const string& name = (*i).first;
      if (!soil.has_attribute (name))
	{
	  err.entry (string ("Required attribute '") 
		     + name + "' is missing from the soil");
	  ok = false;
	}
    }
  return ok;
}

PedotransferLinear::PedotransferLinear (const AttributeList& al)
  : Pedotransfer (al),
    offset (al.number ("offset"))
{
  const vector<AttributeList*>& alists = al.alist_sequence ("factors");
  for (unsigned int i = 0; i < alists.size (); i++)
    factors[alists[i]->name ("name")] = alists[i]->number ("value");
}

static struct PedotransferLinearSyntax
{
  static Pedotransfer& make (const AttributeList& al)
  { return *new PedotransferLinear (al); }
  static bool check_alist (const AttributeList&, Treelog&)
  {
    bool ok = true;
    return ok;
  }
  PedotransferLinearSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    syntax.add_check (check_alist);
    AttributeList& alist = *new AttributeList ();
    Pedotransfer::load_syntax (syntax, alist);

    alist.add ("description", 
	       "A linear combination of soil attributes.");
    Syntax& attSyntax = *new Syntax ();
    attSyntax.add ("name", Syntax::String, Syntax::Const,
		   "Name of soil attribute.");
    attSyntax.add ("value", Syntax::Unknown (), Syntax::Const,
		   "Factor to multiply soil attribute with.");
    attSyntax.order ("name", "value");
    syntax.add ("factors", attSyntax, Syntax::Const, Syntax::Sequence,
		"Soil attribute factors.");
    syntax.add ("offset", Syntax::Unknown (), Syntax::Const,
		"Fixed number to add to the result");

    Librarian<Pedotransfer>::add_type ("linear", alist, syntax, &make);
  }
} PedotransferLinear_syntax;
