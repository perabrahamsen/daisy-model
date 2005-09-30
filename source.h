// source.h -- Data source for gnuplot interface 
// 
// Copyright 2005 Per Abrahamsen and KVL.
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

#ifndef SOURCE_H
#define SOURCE_H

#include "librarian.h"
#include "time.h"

class Source
{
  // Content.
public:
  const symbol name;
  static const char *const description;

  // Interface.
public:
  virtual const std::string& title () const = 0;
  virtual const std::string& dimension () const = 0;
  virtual const std::string& with () const = 0;
  virtual int style () const = 0;
  virtual const std::vector<Time>& time () const = 0;
  virtual const std::vector<double>& value () const = 0;
  virtual bool load (Treelog& msg) = 0;

  // Utilities.
public:
  void limit (Time& begin, Time& end, double& ymin, double& ymax) const;
  void distance (Time begin, Time end, double ymin, double ymax,
		 double& nw, double& ne, double& sw, double& se) const;
  
  // Create and Destroy.
public:
  static void load_syntax (Syntax& syntax, AttributeList&);
protected:
  explicit Source (const AttributeList& al);
public:
  virtual ~Source ();
};

#ifdef FORWARD_TEMPLATES
template<>
Librarian<Source>::Content* Librarian<Source>::content;
#endif

static Librarian<Source> Source_init ("source");

#endif // SOURCE_H
