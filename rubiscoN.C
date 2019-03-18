// rubiscoN.C  -- Rubisco N content of leaves.
// 
// Copyright 2006, Birgitte Gjettermann, Per Abrahamsen and KVL.
// Copyright 2017 KU.
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

#include "rubiscoN.h"
#include "block_model.h"
#include "librarian.h"
#include "check.h"

// The 'rubiscoN' component.

const char *const RubiscoN::component = "rubiscoN";

symbol
RubiscoN::library_id () const
{
  static const symbol id (component);
  return id;
}

RubiscoN::RubiscoN ()
{ }

RubiscoN::~RubiscoN ()
{ }

static struct RubiscoNInit : public DeclareComponent 
{
  RubiscoNInit ()
    : DeclareComponent (RubiscoN::component, "\
Find total amount of Rubisco-N in leaves.")
  { }
} RubiscoN_init;

// The 'default' model.

struct RubiscoNStandard : public RubiscoN
{
  // Parameters.
private:
  const double fraction;	// Fraction of RuBisCo N in canopy.
  const double offset;		// Subtract this amount of N per LAI. [g/m^2]
  const bool subtract_Nf;	// Subtract non-functional N.
  const bool subtract_Pt;	// Subtract luxury N.
  
  // Simulation.
  double value (const double LAI, const double Act, 
		const double Nf, const double Cr, const double Pt)
  {
    double N = Act;

    if (subtract_Pt && Act > Cr)
      N = Cr;

    if (subtract_Nf)
      N -= Nf;

    N -= offset * LAI;

    N *= fraction;
    
    return N;
  }

  // Create.
public:
  RubiscoNStandard (const BlockModel& al)
    : fraction (al.number ("fraction")),
      offset (al.number ("offset")),
      subtract_Nf (al.flag ("subtract_Nf")),
      subtract_Pt (al.flag ("subtract_Pt"))
  { }
  ~RubiscoNStandard ()
  { }
};

static struct RubiscoNStandardSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new RubiscoNStandard (al); }

  RubiscoNStandardSyntax ()
    : DeclareModel (RubiscoN::component, "default", 
		    "Standard estimation of RuBisCo N.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_fraction ("fraction", Attribute::Const, "\
Fraction of remaining N in leaves that RuBisCo-N.\n\
First structural and luxury N may be subtracted, as per the\n\
other parameters.");
    frame.set_cited ("fraction", 0.75, "Equation 6", "boegh2002");
    frame.declare ("offset", "g N/m^2", Check::none (), Attribute::Const, "\
Subtract this amount of N per LAI.");
    frame.set ("offset", 0.0);
    frame.declare_boolean ("subtract_Nf", Attribute::Const, "\
Subtract N corresponding to the non functional concentration in leafs.");
    frame.set ("subtract_Nf", false);
    frame.declare_boolean ("subtract_Pt", Attribute::Const, "\
Subtract N above the critical concentration in leafs.");
    frame.set ("subtract_Pt", false);

  }
} rubiscoNStandard_syntax;

// rubiscoN.C ends here.
