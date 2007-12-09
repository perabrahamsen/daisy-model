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
#include "syntax.h"
#include "alist.h"
#include "librarian.h"
#include "ublas_cxsparse.h"

struct SolverCXSparse : public Solver
{ 
  void solve (Matrix& A, const Vector& b, Vector& x) const // Solve Ax=b
  {
    // declare variable (T=double, size_type=unsigned int)
    CS::cxsparse_type_traits<double, unsigned int>::lu_type     cs_lu;

    // decompose
    cs_lu = CS::cs_ul_decompose (A, 1.0);

    // solve
    x = b;
    CS::cs_ul_solve (cs_lu, x);

    // free
    CS::cs_lu_free (cs_lu);
  }
  SolverCXSparse (Block& al)
    : Solver (al)
  { }
};

static struct SolverCXSparseSyntax
{
  static Model& make (Block& al)
  { return *new SolverCXSparse (al); }
  SolverCXSparseSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Solve equation using CXSparse library.");
    Librarian::add_type (Solver::component, "cxsparse", alist, syntax, &make);
  }
} SolverCXSparse_syntax;

const AttributeList&
Solver::default_model ()
{
  static AttributeList alist;
  if (!alist.check ("type"))
    alist.add ("type", "cxsparse");

  return alist;
}

// solver_cxsparse.C ends here.
