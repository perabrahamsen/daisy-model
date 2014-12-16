// point.C -- A point in 2D soil space.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
// Copyright 2012 KU.
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

#include "point.h"
#include "block.h"
#include "frame.h"
#include "check.h"

// The 'XYPoint' Submodel

void
XYPoint::load_syntax (Frame& frame)
{ 
  frame.declare ("x", Attribute::Unknown (), Attribute::Const, "X-Coordinate.");
  frame.declare ("y", Attribute::Unknown (), Attribute::Const, "Y-Coordinate.");
  frame.order ("x", "y");
}

XYPoint::XYPoint (const Block& al)
  : x (al.number ("x")),
    y (al.number ("y"))
{ }

XYPoint::~XYPoint ()
{ }

// The 'ZXPoint' Submodel

void
ZXPoint::load_syntax (Frame& frame)
{ 
  frame.declare ("z", "cm", Check::non_positive (), Attribute::Const, 
                 "Vertical position.");
  frame.declare ("x", "cm", Check::non_negative (), Attribute::Const,
                 "Horizontal position.");
  frame.order ("z", "x");
}

ZXPoint::ZXPoint (const Block& al)
  : z (al.number ("z")),
    x (al.number ("x"))
{ }

ZXPoint::~ZXPoint ()
{ }

// point.C ends here.

