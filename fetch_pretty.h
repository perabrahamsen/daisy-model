// fetch_pretty.h --- Fetch data from a log model to a summary model.
// 
// Copyright 2003-2004 Per Abrahamsen and KVL
// Copyright 2009 University of Copenhagen
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


#ifndef FETCH_PRETTY_H
#define FETCH_PRETTY_H

#include "fetch.h"

class FetchPretty : public Fetch
{
  // Content.
private:
  const symbol name;
  const double factor;
  const bool add_delta;

  // Use.
public:
  static int width (double value);
  size_t name_size () const;
  int value_size (double& total) const;
  void summarize (std::ostream& out, const int width) const;

  // Create and Destroy.
private:
  using Fetch::clear;
  using Fetch::initialize;
public:
  static void clear (const std::vector<FetchPretty*>& fetch);
  static void initialize (const std::vector<FetchPretty*>& fetch,
                          std::vector<Select*>& select, Treelog& msg);
  static void load_syntax (Frame&);
  explicit FetchPretty (const FrameSubmodel& al);
  explicit FetchPretty (const symbol key);
};

#endif // FETCH_PRETTY_H
