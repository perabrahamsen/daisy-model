// solver.C -- Solve matrix equation Ax=b.
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

#include "solver.h"
#include "block.h"
#include "librarian.h"

const char *const Solver::component = "solver";

Solver::Matrix::Matrix (const size_t size)
  : SMatrix_type (size, size)
{
#ifdef USE_DENSE_MATRIX
  *this = ublas::zero_matrix<double> (size, size); 
#endif // USE_DENSE_MATRIX
}

Solver::Solver (Block& al)
  : name (al.identifier ("type"))
{ }

Solver::~Solver ()
{ }

static Librarian Solver_init (Solver::component, "\
A way to solve the matrix equation 'A x = b'.");

// solver.C ends here.
