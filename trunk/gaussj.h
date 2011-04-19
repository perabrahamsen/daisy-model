// gaussj.h

#ifndef GAUSSJ_H
#define GAUSSJ_H

#include <vector>

class GaussJordan
{
  const unsigned int size;
  std::vector<double> matrix;
  std::vector<double> value;
  std::vector<double> result_;

private:
  void uptriangle ();
  void process_column (int k);
  void find_pivot (int k);
  int find_nonzero (int k);
  void swap_rows (int k, int j);

public:
  void set_value (int row, double);
  double get_value (int row) const;
  void set_entry (int row, int column, double);
  double get_entry (int row, int column) const;
  inline double operator() (int row, int column) const
  { return get_entry (row, column); }
  void solve ();
  double result (int row) const;
  GaussJordan (int s);
};

#endif // GAUSSJ_H
