// clayom_old.C -- Traditional clay funtion.
// 
// Copyright 2002 KVL and Per Abrahamsen.
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


#include "clayom.h"
#include "block.h"
#include "alist.h"
#include "plf.h"
#include "smb.h"
#include "soil.h"

using namespace std;

class ClayOMOld : public ClayOM
{
  // Content.
  PLF factor_;

  // Simulation.
public:
  void set_rates (const Soil&, const vector<SMB*>&) const;
  double factor (const double clay) const ;
  bool smb_use_clay (unsigned int pool) const ;
  bool som_use_clay (unsigned int pool) const ;

  // Create and Destroy.
  bool check (const vector<SMB*>& smb, Treelog& err) const;
  ClayOMOld (Block& al);
  ~ClayOMOld ();
};

void
ClayOMOld::set_rates (const Soil& soil, const vector<SMB*>& smb) const
{ 
  for (int pool = 0; pool < smb.size (); pool++)
    {
      daisy_assert (smb[pool]->clay_maintenance.size () == 0);
      smb[pool]->clay_maintenance.insert (smb[pool]->clay_maintenance.end (),
					  soil.size (), 
					  smb[pool]->maintenance);
      daisy_assert (smb[pool]->clay_turnover.size () == 0);
      smb[pool]->clay_turnover.insert (smb[pool]->clay_turnover.end (), 
				       soil.size (), 
				       smb[pool]->turnover_rate);
    }
}

double
ClayOMOld::factor (const double clay) const 
{ return factor_ (clay); }

bool 
ClayOMOld::smb_use_clay (unsigned int pool) const
{ return pool == 0; }

bool 
ClayOMOld::som_use_clay (unsigned int /* pool */) const
{ return true; }

// Create and Destroy.

bool 
ClayOMOld::check (const vector<SMB*>&, Treelog&) const
{ return true; }

ClayOMOld::ClayOMOld (Block& al)
  : ClayOM (al),
    factor_ (al.plf ("factor"))
{ }

ClayOMOld::~ClayOMOld ()
{ }

static struct ClayOMOldSyntax
{
  static Model& make (Block& al)
  { return *new ClayOMOld (al); }

  ClayOMOldSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", 
	       "Traditional clay influence on organic matter.");

    syntax.add ("factor", Syntax::Fraction (), Syntax::None (),
		Syntax::Const, "\
Function of clay content, multiplied to the maintenance and turnover rates\n\
of SMB1 and all SOM pools.");
    PLF factor;
    factor.add (0.00, 1.0);
    factor.add (0.25, 0.5);
    factor.add (1.00, 0.5);
    alist.add ("factor", factor);

    Librarian<ClayOM>::add_type ("old", alist, syntax, &make);
  }
} ClayOMOld_syntax;
