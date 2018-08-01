/* Copyright (c) 2012-2018 Big Ladder Software LLC. All rights reserved.
 * See the LICENSE file for additional terms and conditions. */

#include "fixtures/bestest-fixture.hpp"
#include "fixtures/typical-fixture.hpp"

#include "Errors.hpp"

using namespace Kiva;

std::string dbl_to_string(double dbl) {
  std::ostringstream strs;
  strs << dbl;
  std::string str = strs.str();
  return str;
}

TEST_F(GC10aFixture, GC10a) {
  double analyticalQ = 2432.597;
  // double trnsysQ = 2427;
  // double fluentQ = 2425;
  // double matlabQ = 2432;

  EXPECT_NEAR(calcQ(), analyticalQ, analyticalQ * 0.03);

  // kiva: 2480.2
}

TEST_F(BESTESTFixture, GC30a) {
  fnd.deepGroundDepth = 30.0;
  fnd.farFieldWidth = 20.0;

  double trnsysQ = 2642;
  double fluentQ = 2585;
  double matlabQ = 2695;

  double average = (trnsysQ + fluentQ + matlabQ) / 3.0;

  EXPECT_NEAR(calcQ(), average, average * 0.05);

  // average: 2640.7
  // kiva: 2665.0
}

TEST_F(BESTESTFixture, GC30b) {
  fnd.deepGroundDepth = 15.0;
  fnd.farFieldWidth = 15.0;
  fnd.interiorConvectiveCoefficient = 100;
  fnd.exteriorConvectiveCoefficient = 100;

  double trnsysQ = 2533;
  double fluentQ = 2504;
  double matlabQ = 2570;

  double average = (trnsysQ + fluentQ + matlabQ) / 3.0;

  EXPECT_NEAR(calcQ(), average, average * 0.05);

  // average: 2535.7
  // kiva: 2576.8
}

TEST_F(BESTESTFixture, GC30c) {
  fnd.deepGroundDepth = 15.0;
  fnd.farFieldWidth = 8.0;
  fnd.interiorConvectiveCoefficient = 7.95;

  double trnsysQ = 2137;
  double fluentQ = 2123;
  double matlabQ = 2154;

  double average = (trnsysQ + fluentQ + matlabQ) / 3.0;

  EXPECT_NEAR(calcQ(), average, average * 0.05);

  // average: 2138
  // kiva: 2229.3
}

TEST_F(BESTESTFixture, GC60b) {
  fnd.deepGroundDepth = 15.0;
  fnd.farFieldWidth = 15.0;
  fnd.interiorConvectiveCoefficient = 7.95;
  fnd.exteriorConvectiveCoefficient = 100;

  double trnsysQ = 2113;
  double fluentQ = 2104;
  double matlabQ = 2128;

  double average = (trnsysQ + fluentQ + matlabQ) / 3.0;

  EXPECT_NEAR(calcQ(), average, average * 0.05);

  // average: 2115
  // kiva: 2208.7
}

TEST_F(BESTESTFixture, GC65b) {
  fnd.deepGroundDepth = 15.0;
  fnd.farFieldWidth = 15.0;
  fnd.interiorConvectiveCoefficient = 7.95;
  fnd.exteriorConvectiveCoefficient = 11.95;

  double trnsysQ = 1994;
  double fluentQ = 1991;
  double matlabQ = 2004;

  double average = (trnsysQ + fluentQ + matlabQ) / 3.0;

  EXPECT_NEAR(calcQ(), average, average * 0.05);

  // average: 1996.3
  // kiva: 2073.3
}

// Not an actual BESTEST Case, though recommended by TRNSYS report
TEST_F(BESTESTFixture, 1D) {
  fnd.exposedFraction = 0.0;
  fnd.deepGroundDepth = 1.0;
  fnd.useDetailedExposedPerimeter = false;

  double area = 144; // m2
  double expectedQ = fnd.soil.conductivity / fnd.deepGroundDepth * area *
                     (bcs.indoorTemp - bcs.deepGroundTemperature);
  EXPECT_NEAR(calcQ(), expectedQ, 1.0);
}

/*
TEST_F( TypicalFixture, Slab)
{
  bcs.localWindSpeed = 0;
  bcs.outdoorTemp = 283.15;
  bcs.indoorTemp = 303.15;

  outputMap[Surface::ST_SLAB_CORE] = {
    GroundOutput::OT_RATE
  };

  EXPECT_NEAR(1.0, 1.0, 1.0);
}*/

TEST_F(GC10aFixture, calculateADI) {
  fnd.numericalScheme = Foundation::NS_ADI;
  bool fullyear = false;
  //  fullyear = true;
  if (fullyear) {
    double surface_avg = calculate(8760);
    Kiva::showMessage(MSG_INFO, dbl_to_string(surface_avg));
    EXPECT_NEAR(surface_avg, 2888.473, 0.01);
  } else {
    double surface_avg = calculate();
    Kiva::showMessage(MSG_INFO, dbl_to_string(surface_avg));
    EXPECT_NEAR(surface_avg, 2607.32, 0.01);
  }
}

TEST_F(GC10aFixture, calculateImplicit) {
  fnd.numericalScheme = Foundation::NS_IMPLICIT;

  double surface_avg = calculate();
  Kiva::showMessage(MSG_INFO, dbl_to_string(surface_avg));
  EXPECT_NEAR(surface_avg, 2601.25, 0.01);
}

TEST_F(GC10aFixture, calculateCrankN) {
  fnd.numericalScheme = Foundation::NS_CRANK_NICOLSON;

  double surface_avg = calculate();
  Kiva::showMessage(MSG_INFO, dbl_to_string(surface_avg));
  EXPECT_NEAR(surface_avg, 2600.87, 0.01);
}

TEST_F(GC10aFixture, calculateADE) {
  fnd.numericalScheme = Foundation::NS_ADE;

  double surface_avg = calculate();
  Kiva::showMessage(MSG_INFO, dbl_to_string(surface_avg));
  EXPECT_NEAR(surface_avg, 2615.19, 0.01);
}

TEST_F(GC10aFixture, GC10a_calculateSteadyState) {
  fnd.numericalScheme = Foundation::NS_STEADY_STATE;

  double surface_avg = calculate();
  Kiva::showMessage(MSG_INFO, dbl_to_string(surface_avg));
  EXPECT_NEAR(surface_avg, 3107.57, 0.01);
}

// Google Test main
int main(int argc, char **argv) {
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
