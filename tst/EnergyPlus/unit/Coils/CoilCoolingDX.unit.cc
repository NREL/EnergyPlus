
// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/Coils/CoilCoolingDX.hh>

#include "Coils/CoilCoolingDXFixture.hh"

using namespace EnergyPlus;

TEST_F( CoilCoolingDXTest, CoilCoolingDXInput )
{
    std::string idf_objects = this->getCoilObjectString("coolingCoil", false, 2);
    EXPECT_TRUE(process_idf( idf_objects, false ));
    CoilCoolingDX thisCoil("coolingCoil");
    EXPECT_EQ("COOLINGCOIL", thisCoil.name);
    EXPECT_EQ("PERFORMANCEOBJECTNAME", thisCoil.performance.name);
}
