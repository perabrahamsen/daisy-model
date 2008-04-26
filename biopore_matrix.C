// biopore_matrix.C --- Static vertical biopores with a capacity.
// 
// Copyright 2008 Per Abrahamsen and KU.
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

#include "biopore.h"
#include "imvec.h"
#include "block.h"
#include "vcheck.h"
#include "librarian.h"
#include "submodeler.h"

// The 'matrix' model.

struct BioporeMatrix : public Biopore
{
  // Parameters.
  /* const */ std::vector<double> xplus; // [cm]
  const double diameter;        // [mm]
  
  // State.
  std::vector<double> h_bottom; // [cm]
  std::auto_ptr<IMvec> solute;  // [g/cm^3]

  // Create and Destroy.
  BioporeMatrix (Block& al);
};

BioporeMatrix::BioporeMatrix (Block& al)
  : Biopore (al),
    xplus (al.check ("xplus") 
           ? al.number_sequence ("xplus") 
           : std::vector<double> ()),
    diameter (al.number ("diameter")),
    h_bottom (al.check ("h_bottom") 
              ? al.number_sequence ("h_bottom") 
              : std::vector<double> ()),
    solute (al.check ("solute")
            ? new IMvec (al, "solute")
            : NULL)
{ }

static struct BioporeMatrixSyntax
{
  static Model& make (Block& al)
  { return *new BioporeMatrix (al); }

  BioporeMatrixSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Biopores that ends in the matrix.");
    Biopore::load_base (syntax, alist);

    syntax.add ("xplus", "cm", Syntax::OptionalConst, Syntax::Sequence,
		"Right side of each biopore interval.\n\
Water and chemical content is tracked individually for each interval.\n\
By default, use intervals as specified by the geometry.");
    syntax.add_check ("xplus", VCheck::increasing ());
    syntax.add ("diameter", "cm", Syntax::Const, "Biopore diameter.");
    syntax.add ("h_bottom", "cm", Syntax::OptionalConst, Syntax::Sequence,
		"Pressure at the bottom of the biopores in each interval.");

    static const symbol C_unit ("g/cm^3");
    IMvec::add_syntax (syntax, alist, Syntax::OptionalState, "solute", C_unit,
                       "Chemical concentration in biopore intervals.");

    Librarian::add_type (Biopore::component, "matrix", alist, syntax, &make);
  }
} BioporeMatrix_syntax;

// biopore_std.C ends here.
