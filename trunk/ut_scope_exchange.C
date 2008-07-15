#define BUILD_DLL

#include "scope_exchange.h"
#include "symbol.h"
#include "mathlib.h"

#include <iostream>

#define UTEST(x) \
try { if (!(x)) std::cerr << __FILE__ << __LINE__ \
                          << ":" "Test '" #x "' failed\n"; } \
catch (...) { std::cerr << __FILE__ << __LINE__ \
                        << ":" << "Test '" #x "' throwed an exception\n"; }

static void utest ()
{
  const symbol x_symbol = symbol ("x");
  const symbol x_dim = symbol ("x dim");
  const symbol x_desc = symbol ("x desc");
  const symbol y_symbol = symbol ("y");

  ScopeExchange scope;
  scope.add_item (new ExchangeNumber (x_symbol, 
                                      x_dim.name ().c_str (), 
                                      x_desc.name ().c_str ()));
  scope.add_item (new ExchangeNumber (y_symbol, 42.0, "y dim", "y desc"));
  scope.done ();
  UTEST (scope.all_numbers ().size () == 2);
  UTEST (scope.has_number (y_symbol) == true);
  UTEST (approximate (scope.number (y_symbol), 42.0));
  UTEST (scope.has_number (x_symbol) == false);
  scope.set_number (x_symbol, 43.0);
  UTEST (scope.has_number (x_symbol) == true);
  UTEST (approximate (scope.number (x_symbol), 43.0));
  UTEST (scope.dimension (x_symbol) == x_dim);
  UTEST (scope.get_description (x_symbol) == x_desc);
  UTEST (scope.has_number (symbol ("no such number")) == false);
  UTEST (scope.has_identifier (symbol ("no such identifier")) == false);
  UTEST (scope.has_identifier (x_symbol) == false);
}

int main ()
{
  std::cout << "Starting unit test.\n";
  utest ();
  std::cout << "Unit test done.\n";
  return EXIT_SUCCESS;
}
