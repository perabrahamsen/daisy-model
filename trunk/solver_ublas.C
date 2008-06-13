// solver_ublas.C -- Solve matrix equation Ax=b using ublas.
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
#include "assertion.h"
#include "syntax.h"
#include "alist.h"
#include "librarian.h"

#include <boost/numeric/ublas/triangular.hpp>
#include <boost/numeric/ublas/vector_proxy.hpp>
#include <boost/numeric/ublas/lu.hpp>

struct SolverUBLAS : public Solver
{ 
  void solve (Matrix& A, const Vector& b, Vector& x) const // Solve Ax=b
  {
    namespace ublas = boost::numeric::ublas;

    const size_t size = b.size ();
    ublas::permutation_matrix<double> piv (size);
    const bool singular = ublas::lu_factorize(A, piv);
    daisy_assert (!singular);
    x = b; 			// x should contain b as a start.
    ublas::lu_substitute (A, piv, x);
  }
  SolverUBLAS (Block& al)
    : Solver (al)
  { }
};

static struct SolverUBLASSyntax
{
  static Model& make (Block& al)
  { return *new SolverUBLAS (al); }
  SolverUBLASSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Solve equation using UBLAS lu functions.");
    Librarian::add_type (Solver::component, "ublas", alist, syntax, &make);
  }
} SolverUBLAS_syntax;

// solver_ublas.C ends here.
