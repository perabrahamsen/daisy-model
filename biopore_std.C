// biopore_std.C --- Static vertical biopores with a capacity.
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

// The 'default' model.

struct BioporeStandard : public Biopore
{
  // Parameters.
  /* const */ std::vector<double> xplus; // [cm]
  const double diameter;        // [mm]
  
  // State.
  std::vector<double> h_bottom; // [cm]
  std::auto_ptr<IMvec> solute;  // [g/cm^3]

  // Create and Destroy.
  BioporeStandard (Block& al);
};

BioporeStandard::BioporeStandard (Block& al)
  : xplus (al.check ("xplus") 
           ? al.number_sequence ("xplus") 
           : std::vector<double> ()),
    diameter (al.number ("diameter")),
    h_bottom (al.check ("h_bottom") 
              ? al.number_sequence ("h_bottom") 
              : std::vector<double> ()),
    solute (al.check ("solute")
            ? Librarian::build_item<IMvec> (al, "solute")
            : NULL)
{ }

// biopore_std.C ends here.
