// select_index.C --- Select a state variable.
// 
// Copyright 1996-2002 Per Abrahamsen and Søren Hansen
// Copyright 2000-2002 KVL.
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

#include "select_value.h"
#include "block_model.h"
#include "librarian.h"
#include "frame.h"
#include "vcheck.h"
#include <sstream>

struct SelectIndex : public SelectValue
{
  // Index.
  const int index;

  // Output routines.
  void output_array (const std::vector<double>& array)
  { 
    if (index < array.size ())
      // Indexes outside the array is treated like missing values.
      add_result (array[index]); 
  }

  // Create and Destroy.
  SelectIndex (const BlockModel& al)
    : SelectValue (al),
      index (al.integer ("index"))
  { }
};

static struct SelectIndexSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new SelectIndex (al); }
  SelectIndexSyntax ()
    : DeclareModel (Select::component, "index", "value", "\
Extract content at specified array index.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.declare_integer ("index", Attribute::Const,
                           "Specify array index to select.");
    frame.set_check ("index", VCheck::non_negative ());
  }
} SelectIndex_syntax;

// select_index.C ends here.
