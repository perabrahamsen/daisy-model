// program.h --- Program to run.
// 
// Copyright Per Abrahamsen and KVL.
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


#ifndef PROGRAM_H
#define PROGRAM_H

#include "librarian.h"

class Treelog;

class Program
{
  // Content.
public:
  const AttributeList& alist;  	// Remember attributes for checkpoint.
  const symbol name;
  static const char *const description;

  // Simulation.
public:
  virtual void run (Treelog&) = 0;

  // Create and Destroy.
public:
  virtual void initialize (const Syntax* glob_syn,
                           const AttributeList* glob_al,
                           Treelog& err) = 0;
  virtual bool check (Treelog& err) = 0;
protected:
  Program (const AttributeList& al);
public:
  virtual ~Program ();
};

#ifdef FORWARD_TEMPLATES
EMPTY_TEMPLATE
Librarian<Program>::Content* Librarian<Program>::content;
#endif

static Librarian<Program> Program_init ("program");

#endif // PROGRAM_H
