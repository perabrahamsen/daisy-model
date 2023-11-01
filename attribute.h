// attribute.h -- Type system for attributes.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
// Copyright 2008 Per Abrahamsen and KVL.
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


#ifndef ATTRIBUTE_H
#define ATTRIBUTE_H

#include "symbol.h"

#ifdef __unix
#define EXPORT /* Nothing */
#elif defined (BUILD_DLL)
/* DLL export */
#define EXPORT __declspec(dllexport)
#else
/* EXE import */
#define EXPORT __declspec(dllimport)
#endif

namespace Attribute
{ 
  // A attribute entry has an associated size.  If the size is a positive
  // integer, the attribute entry specifies an array of that size.  The
  // default size 'Singleton' indicating that the attribute entry match a
  // single item of the specified type, while the 'Variable' used for
  // entries that contain an array of unspecified length. 
  const int Singleton = -117;	
  const int Variable = -3210;
  const int CanopyCells = -3211;
  const int CanopyEdges = -3212;
  const int SoilCells = -3213;
  const int SoilEdges = -3214;
  const int Unspecified = -666;
  symbol size_name (int);

  // Size is not known at compile time.
  inline bool flexible_size (const int size)
  { return size == Variable || size == CanopyCells || size == CanopyEdges
      || size == SoilCells || size == SoilEdges; }

  // A attribute may have a dimension associated.
  symbol Unknown ();
  EXPORT symbol None ();
  symbol Fraction ();
  symbol User ();

  // Each attribute entry should have an associated type.
  enum type 
  { Number, Submodel, PLF, Boolean, String,
    Integer, Model, Scalar, Function, Reference, Error };
  EXPORT symbol type_name (type);
  type type_number (symbol name);
    
  // The requirements with regard to input and output varies with each
  // attribute entry.
  enum category
  {
    // This is a parameter, i.e. its attribute doesn't change during the
    // compilation, and it cannot be written to the log.
    Const,
    // This a state variable, it must be provided at initialization
    // and can be written to the log.
    State,
    // This is a state variable that can be computed from other
    // parameters or state variables, and therefore does not need to
    // be specified before the simulation starts. 
    OptionalState, 
    // This is a parameter that can be computer from other parameters,
    // and therefore does not need to be specified before 
    // the simulation starts. 
    OptionalConst, 
    // This is a variable that is only computed for logging purposes
    // and not a part of the simulation state. 
    LogOnly
  };
  symbol category_name (category);
  int category_number (symbol name);
}

#endif // ATTRIBUTE_H
