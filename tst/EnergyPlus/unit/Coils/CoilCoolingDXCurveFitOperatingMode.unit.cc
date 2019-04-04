
// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/Coils/CoilCoolingDX.hh>
#include "Coils/CoilCoolingDXFixture.hh"

using namespace EnergyPlus;

TEST_F( CoilCoolingDXTest, CoilCoolingDXCurveFitModeInput )
{
    std::string idf_objects = this->getModeObjectString("mode1", 2);
    EXPECT_TRUE(process_idf( idf_objects, false ));
    CoilCoolingDXCurveFitOperatingMode thisMode("mode1");
    EXPECT_EQ("MODE1", thisMode.name);
    EXPECT_EQ("MODE1SPEED1", thisMode.speeds[0].name);
}
