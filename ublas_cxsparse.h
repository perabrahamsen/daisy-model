// Copyright 2007 Gunter Winkler <guwi17@gmx.de>
// Distributed under the Boost Software License, Version 1.0. (See
// accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

extern "C" {
#include "cs.h"
}

namespace CS {

  namespace ublas = ::boost::numeric::ublas;


  /* some type mappings */
  template <class VALUE_TYPE, class INDEX_TYPE>
  struct cxsparse_type_traits
  {
    typedef VALUE_TYPE value_type;
    typedef INDEX_TYPE index_type;
    //  typedef error_unknown_combination_of_VALUE_TYPE_and_INDEX_TYPE  matrix_type;
  };

  template <>
  struct cxsparse_type_traits<double, int>
  {
    typedef double  value_type;
    typedef int     index_type;
    typedef cs_di_sparse  matrix_type;
    typedef cs_di_symbolic symbolic_type;
    typedef cs_di_numeric  numeric_type;
    typedef cs_di_dmperm_results dmperm_results;
    typedef std::pair<symbolic_type*, numeric_type*> lu_type;
  };

  template <>
  struct cxsparse_type_traits<double, unsigned int>
  {
    typedef double  value_type;
    typedef int     index_type; // CXSparse always uses signed integers
    typedef cs_di_sparse  matrix_type;
    typedef cs_di_symbolic symbolic_type;
    typedef cs_di_numeric  numeric_type;
    typedef cs_di_dmperm_results dmperm_results;
    typedef std::pair<symbolic_type*, numeric_type*> lu_type;
  };

  template <>
  struct cxsparse_type_traits<double, long>
  {
    typedef double  value_type;
    typedef long     index_type;
    typedef cs_dl_sparse  matrix_type;
    typedef cs_dl_symbolic symbolic_type;
    typedef cs_dl_numeric  numeric_type;
    typedef cs_dl_dmperm_results dmperm_results;
    typedef std::pair<symbolic_type*, numeric_type*> lu_type;
  };

  template <>
  struct cxsparse_type_traits<double, unsigned long int>
  {
    typedef double  value_type;
    typedef long    index_type; // CXSparse always uses signed integers
    typedef cs_dl_sparse   matrix_type;
    typedef cs_dl_symbolic symbolic_type;
    typedef cs_dl_numeric  numeric_type;
    typedef cs_dl_dmperm_results dmperm_results;
    typedef std::pair<symbolic_type*, numeric_type*> lu_type;
  };


  cxsparse_type_traits<double,int>::symbolic_type
  * cs_sqr_ex(int order, const cxsparse_type_traits<double,int>::matrix_type *A, int qr)
  {
    return cs_di_sqr(order, A, qr);
  }

  cxsparse_type_traits<double,long int>::symbolic_type
  * cs_sqr_ex(int order, const cxsparse_type_traits<double,long int>::matrix_type *A, int qr)
  {
    return cs_dl_sqr(order, A, qr);
  }

  cxsparse_type_traits<double,int>::numeric_type
  * cs_lu_ex (const cxsparse_type_traits<double,int>::matrix_type *A, 
              const cxsparse_type_traits<double,int>::symbolic_type *S, 
              double tol) 
  {
    return cs_di_lu(A,S,tol);
  }

  cxsparse_type_traits<double,long int>::numeric_type
  * cs_lu_ex (const cxsparse_type_traits<double,long int>::matrix_type *A, 
              const cxsparse_type_traits<double,long int>::symbolic_type *S, 
              double tol) 
  {
    return cs_dl_lu(A,S,tol);
  }

  template <class T, class IA, class TA, class Z, class D>
  typename cxsparse_type_traits<T, typename IA::value_type>::matrix_type
  cs_init_matrix(const ublas::compressed_matrix<T, ublas::basic_column_major<Z,D>, 0, IA, TA> & A)
  {
    // assure that you have called A.complete_index1_data(); 
    typedef typename cxsparse_type_traits<T, typename IA::value_type>::index_type I;
    typename cxsparse_type_traits<T, typename IA::value_type>::matrix_type result;
    result.nzmax = A.nnz_capacity();
    result.m = A.size1();
    result.n = A.size2();
    result.p = (I*) (&(A.index1_data()[0]));
    result.i = (I*) (&(A.index2_data()[0]));
    result.x = const_cast<T*> (&(A.value_data()[0]));
    result.nz= -1;
    return result;
  }

  template <class T, class IA, class TA, class Z, class D>
  typename cxsparse_type_traits<T, typename IA::value_type>::matrix_type
  cs_init_matrix_transpose(const ublas::compressed_matrix<T, ublas::basic_row_major<Z,D>, 0, IA, TA> & A)
  {
    // assure that you have called A.complete_index1_data(); 
    typedef typename cxsparse_type_traits<T, typename IA::value_type>::index_type I;
    typename cxsparse_type_traits<T, typename IA::value_type>::matrix_type result;
    result.nzmax = A.nnz_capacity();
    result.m = A.size1();
    result.n = A.size2();
    result.p = (I*) (&(A.index1_data()[0]));
    result.i = (I*) (&(A.index2_data()[0]));
    result.x = const_cast<T*> (&(A.value_data()[0]));
    result.nz= -1;
    return result;
  }

  template <class T, class IA, class TA, class Z, class D>
  typename cxsparse_type_traits<T, typename IA::value_type>::lu_type
  cs_lu_decompose(const ublas::compressed_matrix<T, ublas::basic_column_major<Z,D>, 0, IA, TA> & A, const double tol = 1.0)
  {
    typename cxsparse_type_traits<T, typename IA::value_type>::matrix_type cs_mat = cs_init_matrix(A);

    typename cxsparse_type_traits<T, typename IA::value_type>::symbolic_type *symbolic = 0;
    typename cxsparse_type_traits<T, typename IA::value_type>::numeric_type *numeric = 0;

    symbolic = cs_sqr_ex(2, &cs_mat, 0);
    numeric  = cs_lu_ex(&cs_mat, symbolic, tol);

    typename cxsparse_type_traits<T, typename IA::value_type>::lu_type result(symbolic, numeric);
    return result;
  }

  template <class T, class IA, class TA, class Z, class D>
  typename cxsparse_type_traits<T, typename IA::value_type>::lu_type
  cs_ul_decompose(const ublas::compressed_matrix<T, ublas::basic_row_major<Z,D>, 0, IA, TA> & A, const double tol = 1.0)
  {
    typename cxsparse_type_traits<T, typename IA::value_type>::matrix_type cs_mat = cs_init_matrix_transpose(A);

    typename cxsparse_type_traits<T, typename IA::value_type>::symbolic_type *symbolic = 0;
    typename cxsparse_type_traits<T, typename IA::value_type>::numeric_type *numeric = 0;

    symbolic = cs_sqr_ex(2, &cs_mat, 0);
    numeric  = cs_lu_ex(&cs_mat, symbolic, tol);

    typename cxsparse_type_traits<T, typename IA::value_type>::lu_type result(symbolic, numeric);
    return result;
  }

  template <class E>
  typename cxsparse_type_traits<typename E::value_type, typename E::size_type>::lu_type
  cs_ul_decompose(const ublas::matrix_expression<E> & AE, const double tol = 1.0)
  {
    ublas::compressed_matrix<typename E::value_type, 
      ublas::basic_row_major<typename E::size_type, typename E::difference_type>, 
      0,
      ublas::unbounded_array<typename E::size_type>,
      ublas::unbounded_array<typename E::value_type> > temp(AE());
    return cs_ul_decompose(temp);
  }

  void cs_lu_solve(const std::pair<cs_dl_symbolic*, cs_dl_numeric*>& lu, ublas::vector<double>& b)
  {
    long n = b.size();
    ublas::vector<double> x(n);
    cs_dl_ipvec( lu.second->pinv, &(b[0]), &(x[0]), n);
    cs_dl_lsolve( lu.second->L, &(x[0]) );
    cs_dl_usolve( lu.second->U, &(x[0]) );
    cs_dl_ipvec( lu.first->q, &(x[0]), &(b[0]), n);
  }

  void cs_lu_solve(const std::pair<cs_di_symbolic*, cs_di_numeric*>& lu, ublas::vector<double>& b)
  {
    int n = b.size();
    ublas::vector<double> x(n);
    cs_di_ipvec( lu.second->pinv, &(b[0]), &(x[0]), n);
    cs_di_lsolve( lu.second->L, &(x[0]) );
    cs_di_usolve( lu.second->U, &(x[0]) );
    cs_di_ipvec( lu.first->q, &(x[0]), &(b[0]), n);
  }

  void cs_ul_solve(const std::pair<cs_dl_symbolic*, cs_dl_numeric*>& lu, ublas::vector<double>& b)
  {
    long n = b.size();
    ublas::vector<double> x(n);
    cs_dl_pvec( lu.first->q, &(b[0]), &(x[0]), n);
    cs_dl_utsolve( lu.second->U, &(x[0]) );
    cs_dl_ltsolve( lu.second->L, &(x[0]) );
    cs_dl_pvec( lu.second->pinv, &(x[0]), &(b[0]), n);
  }

  void cs_ul_solve(const std::pair<cs_di_symbolic*, cs_di_numeric*>& lu, ublas::vector<double>& b)
  {
    int n = b.size();
    ublas::vector<double> x(n);
    cs_di_pvec( lu.first->q, &(b[0]), &(x[0]), n);
    cs_di_utsolve( lu.second->U, &(x[0]) );
    cs_di_ltsolve( lu.second->L, &(x[0]) );
    cs_di_pvec( lu.second->pinv, &(x[0]), &(b[0]), n);
  }

  void cs_lu_free(std::pair<cs_di_symbolic*, cs_di_numeric*>& lu)
  {
    cs_di_sfree(lu.first);
    cs_di_nfree(lu.second);
  }

  void cs_lu_free(std::pair<cs_dl_symbolic*, cs_dl_numeric*>& lu)
  {
    cs_dl_sfree(lu.first);
    cs_dl_nfree(lu.second);
  }

}
