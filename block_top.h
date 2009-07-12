// block_top.h -- Frame with context.
// 
// Copyright 2005, 2009 Per Abrahamsen and KVL.
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


#ifndef BLOCK_TOP_H
#define BLOCK_TOP_H

#include "block.h"

#ifdef __unix
#define EXPORT /* Nothing */
#elif defined (BUILD_DLL)
/* DLL export */
#define EXPORT __declspec(dllexport)
#else
/* EXE import */
#define EXPORT __declspec(dllimport)
#endif

class EXPORT BlockTop : public Block
{
  const Metalib& metalib_;
  const Frame& frame_;
  Treelog& msg_;
  const Metalib& metalib () const;
  const Frame& frame () const;
  Treelog& msg () const;
  const Frame& find_frame (const symbol key) const;
public:
  BlockTop (const Metalib&, Treelog& msg, const Frame&);
  ~BlockTop ();
};

#endif // BLOCK_TOP_H
