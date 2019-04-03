
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
    EXPECT_EQ("SPEED1", thisMode.speeds[0].name);
}


TEST_F( EnergyPlusFixture, CoilCoolingDXCurveFitOperatingModeCorrectSpeed )
{
    std::string const idf_objects = delimited_string( {
        "Coil:Cooling:DX:CurveFit:OperatingMode, ",
        " OperatingMode1Name,                    ",
        " 12000,                                 ",
        " 100,                                   ",
        " 200,                                   ",
        " 2.5,                                   ",
        " 0.5,                                   ",
        " 100,                                   ",
        " 300,                                   ",
        " Yes,                                   ",
        " Evaporative,                           ",
        " 200,                                   ",
        " DiscreteStagedContinuousOrNotBacon,    ",
        " 5,                                     ",
        " OperatingSpeed1,                       ",
        " OperatingSpeed2;                       "
    } );



}