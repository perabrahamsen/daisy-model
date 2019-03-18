// solver.h -- Solve matrix equation Ax=b.
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

#ifndef SOLVER_H
#define SOLVER_H

#include "model.h"
#include "symbol.h"

#include <boost/numeric/ublas/vector.hpp>
// #include <boost/numeric/ublas/vector_of_vector.hpp>
#include "fixed_vector_of_vector.hpp"

#ifdef USE_DENSE_MATRIX
#include <boost/numeric/ublas/matrix.hpp>
typedef boost::numeric::ublas::matrix<double> SMatrix_type;
#else  // compressed matrix
#include <boost/numeric/ublas/matrix_sparse.hpp>
typedef boost::numeric::ublas::compressed_matrix<double> SMatrix_type;
#endif // compressed matrix

class BlockModel;

class Solver : public Model 
{
  // Identity.
public:
  const symbol name;
  static const char *const component;
  symbol library_id () const;

  // Types.
public:
  typedef boost::numeric::ublas::generalized_vector_of_vector<double, boost::numeric::ublas::row_major, boost::numeric::ublas::vector<boost::numeric::ublas::mapped_vector<double> > > build_matrix;
  typedef boost::numeric::ublas::vector<double> Vector;
  struct Matrix : public SMatrix_type
  {
    using SMatrix_type::operator=;
    void resize (size_t size);
    Matrix (size_t size);
  };
  
  // Use.
public:
  virtual void solve (Matrix& A, const Vector& b, Vector& x) const = 0;

  // Create and Destroy.
private:
  Solver ();			// Disable
public:
  Solver (const BlockModel& al);
  ~Solver ();
};

#endif // SOLVER_H
