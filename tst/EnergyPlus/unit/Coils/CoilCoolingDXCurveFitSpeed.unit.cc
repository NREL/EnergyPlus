
// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/Coils/CoilCoolingDXCurveFitOperatingMode.hh>
#include <EnergyPlus/Coils/CoilCoolingDXCurveFitSpeed.hh>
#include "DataLoopNode.hh"

#include "Coils/CoilCoolingDXFixture.hh"

using namespace EnergyPlus;


TEST_F( CoilCoolingDXTest, CoilCoolingDXCurveFitSpeedInput )
{
    std::string idf_objects = this->getSpeedObjectString("speed1");
    EXPECT_TRUE(process_idf( idf_objects, false ));
    CoilCoolingDXCurveFitSpeed thisSpeed("speed1");
    EXPECT_EQ("SPEED1", thisSpeed.name);
}

TEST_F( CoilCoolingDXTest, CoilCoolingDXCurveFitSpeedTest )
{
    std::string idf_objects = this->getSpeedObjectString("speed1");
    EXPECT_TRUE(process_idf( idf_objects, false ));
    CoilCoolingDXCurveFitSpeed thisSpeed("speed1");
    EXPECT_EQ("SPEED1", thisSpeed.name);

    CoilCoolingDXCurveFitOperatingMode thisMode;
    thisMode.ratedGrossTotalCap = 12000;
    thisMode.ratedEvapAirFlowRate = 100;
    thisMode.ratedCondAirFlowRate = 200;
    thisSpeed.parentMode = &thisMode;

    DataLoopNode::NodeData inletNode;
    inletNode.Temp = 20.0;
    inletNode.HumRat = 0.008;
    inletNode.Enthalpy = 40000.0;
    DataLoopNode::NodeData outletNode;

    thisSpeed.PLR = 1.0;
    thisSpeed.CondInletTemp = 35.0;
    thisSpeed.ambPressure = 101325.0;
    thisSpeed.AirFF = 1.0;
    thisSpeed.rated_total_capacity = 3000.0;
    thisSpeed.RatedAirMassFlowRate = 1.0;
    thisSpeed.RatedSHR = 0.75;
    thisSpeed.RatedCBF = 0.09;
    thisSpeed.RatedEIR = 0.30;
    thisSpeed.AirMassFlow = 1.0;
    thisSpeed.mySizeFlag = false; // disable this so that we don't actually resize the speed
    int fanOpMode = 0;
    Real64 condInletTemp = 24;
    thisSpeed.CalcSpeedOutput(inletNode, outletNode, thisSpeed.PLR, fanOpMode, condInletTemp);

    EXPECT_NEAR( outletNode.Temp, 17.791, 0.001 );
    EXPECT_NEAR( outletNode.HumRat, 0.00754, 0.0001 );
    EXPECT_NEAR( outletNode.Enthalpy, 37000.0, 0.1 );
    EXPECT_NEAR( thisSpeed.FullLoadPower, 900.0, 0.1 );
    EXPECT_NEAR( thisSpeed.RTF, 1.0, 0.01 );

}
