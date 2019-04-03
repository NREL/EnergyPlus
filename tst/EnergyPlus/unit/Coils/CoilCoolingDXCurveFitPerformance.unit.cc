
// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/Coils/CoilCoolingDX.hh>
#include "Coils/CoilCoolingDXFixture.hh"

using namespace EnergyPlus;

TEST_F( CoilCoolingDXTest, CoilCoolingDXCurveFitPerformanceInput )
{
    std::string idf_objects = this->getPerformanceObjectString("coilPerformance", false, 2);
    EXPECT_TRUE(process_idf( idf_objects, false ));
    CoilCoolingDXCurveFitPerformance thisPerf("coilPerformance");
    EXPECT_EQ("COILPERFORMANCE", thisPerf.name);
    EXPECT_EQ("BASEOPERATINGMODE", thisPerf.normalMode.name);
    EXPECT_FALSE(thisPerf.hasAlternateMode);
}

TEST_F( CoilCoolingDXTest, CoilCoolingDXCurveFitPerformanceInputAlternateMode )
{
    std::string idf_objects = this->getPerformanceObjectString("coilPerformance", true, 2);
    EXPECT_TRUE(process_idf( idf_objects, false ));
    CoilCoolingDXCurveFitPerformance thisPerf("coilPerformance");
    EXPECT_EQ("COILPERFORMANCE", thisPerf.name);
    EXPECT_EQ("BASEOPERATINGMODE", thisPerf.normalMode.name);
    EXPECT_TRUE(thisPerf.hasAlternateMode);
}
