// uznone.C --- no water flow.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
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


#include "uzmodel.h"
#include "soil.h"
#include "log.h"
#include "mathlib.h"

class UZNone : public UZmodel
{
  // UZmodel.
public:
  bool flux_top () const
    { return true; }
  double q () const
    { return 0.0; }
  void flux_top_on () const
    { }
  void flux_top_off () const
    { }
  bool accept_top (Treelog&, double)
    { return true; }
  bool flux_bottom () const
    { return true; }
  bool accept_bottom (double)
    { return true; }
  void output (Log&) const
    { }

public:
  bool tick (Treelog&, const Soil& /* soil */, const SoilHeat&,
	     unsigned int first, const UZtop& /* top */, 
	     unsigned int last, const UZbottom& /* bottom */, 
	     const vector<double>& /* S */,
	     const vector<double>& h_old,
	     const vector<double>& Theta_old,
	     const vector<double>& /* h_ice */,
	     vector<double>& h,
	     vector<double>& Theta,
	     vector<double>& q)
    {
      for (int i = first; i <= last; i++)
	{
	  q[i] = 0.0;
	  Theta[i] = Theta_old[i];
	  h[i] = h_old[i];
	}
      q[last + 1] = 0.0;
      return true;
    }
  // Create and Destroy.
public:
  UZNone (const AttributeList& al)
    : UZmodel (al)
    { }
  ~UZNone ()
    { }
};

// Add the UZNone syntax to the syntax table.
static struct UZNoneSyntax
{
  static UZmodel& make (const AttributeList& al)
    {
      return *new UZNone (al);
    }

  UZNoneSyntax ()
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "No water movement, and no sink.");
      Librarian<UZmodel>::add_type ("none", alist, syntax, &make);
    }
} UZNone_syntax;
