#define BUILD_DLL
#include "scope_exchange.h"
#include <gtest/gtest.h>

TEST (ScopeExchange, All)
{
  const symbol x_symbol = symbol ("x");
  const symbol x_dim = symbol ("x dim");
  const symbol x_desc = symbol ("x desc");
  const symbol y_symbol = symbol ("y");
  const symbol n_symbol = symbol ("n");

  ScopeExchange scope ("test");
  scope.add_item (new ExchangeNumber (x_symbol, 
                                      x_dim.name ().c_str (), 
                                      x_desc.name ().c_str ()));
  scope.add_item (new ExchangeNumber (y_symbol, 42.0, "y dim", "y desc"));
  scope.done ();
  std::vector<symbol> entries;
  scope.entries (entries);
  EXPECT_EQ (entries.size (), 2);
  EXPECT_EQ (scope.lookup (y_symbol), Value::Number);
  EXPECT_TRUE (scope.check (y_symbol));
  EXPECT_EQ (scope.number (y_symbol), 42.0);
  EXPECT_EQ (scope.lookup (x_symbol), Value::Number);
  EXPECT_FALSE (scope.check (x_symbol));
  scope.add (x_symbol, 43.0);
  EXPECT_TRUE (scope.check (x_symbol));
  EXPECT_EQ (scope.number (x_symbol), 43.0);
  EXPECT_EQ (scope.dimension (x_symbol), x_dim);
  EXPECT_EQ (scope.description (x_symbol), x_desc);
  EXPECT_EQ (scope.lookup ("no such number"), Value::Error);
  EXPECT_FALSE (scope.check ("no such number"));
}
