// btest.C -- Test linking with boost/ublas


#include <boost/numeric/ublas/banded.hpp>
#include <boost/numeric/ublas/io.hpp>

namespace ublas = boost::numeric::ublas;

int main ()
{
  ublas::banded_matrix<double> m (3, 3, 0, 0);
  for (int i = 0; i < m.size1 (); ++ i)
    m (i, i) = i;
  std::cout << m << std::endl;
  return 0;
}
