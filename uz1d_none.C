// uz1d_none.C --- No flow.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
// Copyright 2006 Per Abrahamsen and KVL.
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
#include "uz1d.h"
#include "librarian.h"
#include <sstream>

struct UZ1DNone : public UZ1D
{
  // Parameters.

  // Interface.
  void tick (SMM1D&, double /* gravity */, double /* dt */, Treelog&)
  { }

  // Create and Destroy.
  UZ1DNone (const BlockModel& al)
    : UZ1D (al)
  { }
  ~UZ1DNone ()
  { }
};

static struct UZ1DNoneSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new UZ1DNone (al); }
  UZ1DNoneSyntax ()
    : DeclareModel (UZ1D::component, "none", "Disable transport")
  { }
  void load_frame (Frame&) const
  { }
} UZ1DNone_syntax;

// uz1d_none.C ends here.
