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

class Syntax;
class AttributeList;
class Treelog;
class Select;

class Fetch : public Destination
{
  // Content.
public:
  const symbol tag;
private:
  const double factor;
  const symbol name;
  const bool add_delta;
public:
  std::string select_dimension;

  // State.
public:
  enum { NewContent, Content, Flux, Error } type;
private:
  double initial;
  double last;
  double sum;

  // Destination
private:
  void error ();
  void missing ();
  void add (const std::vector<double>&);
  void add (const double value);
  void add (const symbol);

  // Use.
private:
  static double period_factor (symbol period, int hours);
public:
  static int width (double value);
  const std::string dimension (const symbol period) const;
  size_t name_size () const;
  int value_size (double& total, const symbol period, const int hours) const;
  void summarize (std::ostream& out, const int width, 
                  const symbol period, const int hours) const;

  // Create and Destroy.
public:
  static void clear (const std::vector<Fetch*>& fetch);
private:
  void clear ();
public:
  static void initialize (const std::vector<Fetch*>& fetch,
                          std::vector<Select*>& select, Treelog& msg);
  static void load_syntax (Syntax& syntax, AttributeList& alist);
  explicit Fetch (const AttributeList& al);
  explicit Fetch (const symbol key);
};

#endif // FETCH_H
