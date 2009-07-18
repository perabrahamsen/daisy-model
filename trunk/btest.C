// btest.C -- Test linking with boost/ublas


#include <boost/numeric/ublas/vector_proxy.hpp>
#include <boost/numeric/ublas/banded.hpp>
#include <boost/numeric/ublas/io.hpp>
#include <boost/numeric/ublas/triangular.hpp>
#include <boost/numeric/ublas/matrix.hpp>
#include <boost/numeric/ublas/vector.hpp>
#include <boost/numeric/ublas/lu.hpp>




namespace ublas = boost::numeric::ublas;


int main ()


{
  
  const int cell_size = 2;
  
  //Initialize A-matrix
  ublas::matrix<double> A (cell_size, cell_size);  
  A (0, 0) = 1;
  A (1, 1) = 2;
  
  A (0, 1) = 0;
  A (1, 0) = 0;

  //Initialize b-vector
  ublas::vector<double> b (cell_size);  
  b (0) = 1;
  b (1) = 1;

  // Solve Ax=b (maybe)
  ublas::permutation_matrix<double> piv (cell_size);
  const bool singular = ublas::lu_factorize(A, piv);
  if (singular)
    std::cout << "singular!" << std::endl;
 
  ublas::lu_substitute (A, piv, b); // b now contains solution 

  std::cout << A << std::endl;
  std::cout << piv << std::endl;  
  std::cout << b << std::endl;
  return 0;
}




#if 0
{
  ublas::banded_matrix<double> m (3, 3, 0, 0);
  for (int i = 0; i < m.size1 (); ++ i)
    m (i, i) = i;
  std::cout << m << std::endl;
  return 0;
}
#endif 
