// solver_none.C -- "Solve" matrix equation Ax=b by doing nothing.
// 
// Copyright 2007 Per Abrahamsen and KVL.
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

// Uncomment for fast code that does not catches bugs.
#define BOOST_UBLAS_NDEBUG
#define NDEBUG

#include "solver.h"
#include "librarian.h"
#include "frame.h"

struct SolverNone : public Solver
{ 
  void solve (Matrix&, const Vector&, Vector&) const
  { }
  SolverNone (const BlockModel& al)
    : Solver (al)
  { }
};

static struct SolverNoneSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new SolverNone (al); }
  SolverNoneSyntax ()
    : DeclareModel (Solver::component, "none", "Don't solve the equation.")
  { }
  void load_frame (Frame& frame) const
  {
  }
} SolverNone_syntax;

// solver_none.C ends here.
