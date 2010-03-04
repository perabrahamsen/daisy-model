// solver_cxspare.C -- Solve matrix equation Ax=b using CXSparse library.
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
#include "ublas_cxsparse.h"
#include "frame.h"

#define MEMCHECK(foo) if (!(foo)) throw "CXSparse: Bad matrix: " #foo ;

struct SolverCXSparse : public Solver
{ 
  void solve (Matrix& A, const Vector& b, Vector& x) const // Solve Ax=b
  {
    // declare variable (T=double, size_type=unsigned int)
    CS::cxsparse_type_traits<double, size_t>::lu_type     cs_lu;

    // decompose
    cs_lu = CS::cs_ul_decompose (A, 1.0);

    // check
    MEMCHECK (cs_lu.first);
    MEMCHECK (cs_lu.first->q);
    MEMCHECK (cs_lu.second)
    MEMCHECK (cs_lu.second->U);
    MEMCHECK (cs_lu.second->L);
    MEMCHECK (cs_lu.second->pinv);

    // solve
    x = b;
    CS::cs_ul_solve (cs_lu, x);

    // free
    CS::cs_lu_free (cs_lu);
  }
  SolverCXSparse (const BlockModel& al)
    : Solver (al)
  { }
};

static struct SolverCXSparseSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new SolverCXSparse (al); }
  SolverCXSparseSyntax ()
    : DeclareModel (Solver::component, "cxsparse", "\
Solve equation using CXSparse library described in:\n\
\n\
Direct Methods for Sparse Linear Systems, T. A. Davis, SIAM,\n\
Philadelphia, Sept. 2006. Part of the SIAM Book Series on the\n\
Fundamentals of Algorithms.\n\
\n\
The uBLAS interface was provided by Gunter Winkler <guwi17@gmx.de>.")
  { }
  void load_frame (Frame& frame) const
  {
  }
} SolverCXSparse_syntax;

// solver_cxsparse.C ends here.
