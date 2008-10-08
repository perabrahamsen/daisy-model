#define BUILD_DLL
#include "scope_exchange.h"
#include <gtest/gtest.h>

TEST (ScopeExchange, All)
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
  EXPECT_EQ (scope.all_numbers ().size (), 2);
  EXPECT_EQ (scope.has_number (y_symbol), true);
  EXPECT_EQ (scope.number (y_symbol), 42.0);
  EXPECT_EQ (scope.has_number (x_symbol), false);
  scope.set_number (x_symbol, 43.0);
  EXPECT_EQ (scope.has_number (x_symbol), true);
  EXPECT_EQ (scope.number (x_symbol), 43.0);
  EXPECT_EQ (scope.dimension (x_symbol), x_dim);
  EXPECT_EQ (scope.get_description (x_symbol), x_desc);
  EXPECT_EQ (scope.has_number (symbol ("no such number")), false);
  EXPECT_EQ (scope.has_name (symbol ("no such identifier")), false);
  EXPECT_EQ (scope.has_name (x_symbol), false);
}
