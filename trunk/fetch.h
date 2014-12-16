// fetch.h --- Fetch data from a log model to a summary model.
// 
// Copyright 2003-2004 Per Abrahamsen and KVL
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


#ifndef FETCH_H
#define FETCH_H

#include "destination.h"
#include "symbol.h"
#include <vector>
#include <iosfwd>

class Frame;
class FrameSubmodel;
class Treelog;
class Select;

class Fetch : public Destination
{
  // Content.
public:
  const symbol tag;
public:
  symbol select_dimension;

  // State.
public:
  enum { NewContent, Content, Flux, Error } type;
protected:
  double initial;
  double last;
  double sum;

  // Destination
private:
  void missing ();
  void add (const std::vector<double>&);
  void add (const double value);
  void add (const symbol);

  // Use.
public:
  symbol dimension () const;

  // Create and Destroy.
public:
  static void clear (const std::vector<Fetch*>& fetch);
protected:
  void clear ();
public:
  static void initialize (const std::vector<Fetch*>& fetch,
                          std::vector<Select*>& select, Treelog& msg);
protected:
  void initialize (std::vector<Select*>& select, Treelog& msg);
public:
  static void load_syntax (Frame&);
  explicit Fetch (const FrameSubmodel& al);
  explicit Fetch (const symbol key);
};

#endif // FETCH_H
