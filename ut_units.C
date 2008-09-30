#define BUILD_DLL

#include "units.h"
#include "unit.h"
#include "metalib.h"
#include "treelog.h"
#include "assertion.h"

#include <gtest/gtest.h>

struct UnitsTest : public testing::Test
{
  const Assertion::Register shut_up;
  const Metalib metalib;
  const Units& units;
  
  UnitsTest ()
    : shut_up (Treelog::null ()),
      metalib (Units::load_syntax),
      units (metalib.units ())
  { }
};
  
TEST_F (UnitsTest, MassAreaFlux2HeightFlux)
{
  EXPECT_TRUE (units.has_unit (Units::mm_per_h ()));
  EXPECT_TRUE (units.has_unit (Unit::mass_per_area_per_time ()));
  ASSERT_TRUE (units.can_convert (Unit::mass_per_area_per_time (),
                                  Units::mm_per_h ()));
  EXPECT_DOUBLE_EQ (units.convert (Unit::mass_per_area_per_time (),
                                   Units::mm_per_h (),
                                   1.0),
                    3600.0);
  const symbol svat_ssoc_transp ("kg m^-2 s^-1");
  EXPECT_TRUE (units.has_unit (svat_ssoc_transp));
  ASSERT_TRUE (units.can_convert (svat_ssoc_transp,
                                  Units::mm_per_h ()));
  EXPECT_DOUBLE_EQ (units.convert (svat_ssoc_transp, Units::mm_per_h (), 1.0),
                    3600.0);
}

TEST_F (UnitsTest, Kelvin2Celcius)
{
  EXPECT_TRUE (units.has_unit ("K"));
  EXPECT_TRUE (units.has_unit ("dg C"));
  ASSERT_TRUE (units.can_convert ("K", "dg C"));
  EXPECT_NEAR (units.convert ("K", "dg C", 0.0), -273.15, 0.01);
}

TEST_F (UnitsTest, Kelvin2Fahrenheit)
{
  EXPECT_TRUE (units.has_unit ("K"));
  EXPECT_TRUE (units.has_unit ("dg F"));
  ASSERT_TRUE (units.can_convert ("K", "dg F"));
  EXPECT_NEAR (units.convert ("K", "dg F", 0.0), -459.67, 0.01);
}
