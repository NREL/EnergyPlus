// EnergyPlus::IndoorIceRink Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/IndoorIceRink.hh>
#include <EnergyPlus/UtilityRoutines.hh>

#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/DataSurfaceLists.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/Plant/PlantManager.hh>
#include <EnergyPlus/PlantUtilities.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SizingManager.hh>
#include <EnergyPlus/SurfaceGeometry.hh>

using namespace EnergyPlus;
using namespace ObjexxFCL;
using namespace DataGlobals;
using namespace EnergyPlus::IceRink;
using namespace EnergyPlus::DataPlant;
using namespace EnergyPlus::DataHeatBalance;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::DataLoopNode;
using namespace EnergyPlus::DataSurfaces;
using namespace EnergyPlus::DataSurfaceLists;
using namespace EnergyPlus::HeatBalanceManager;
using namespace EnergyPlus::ScheduleManager;
using namespace EnergyPlus::SurfaceGeometry;

TEST_F(EnergyPlusFixture, IndoorIceRink_DRinkEff)
{
    Real64 Temperature;
    Real64 RefrigMassFlow;
    Real64 TubeLength;
    Real64 TubeDiameter;
    int NumCircs;
    int SysNum;
    Real64 Effectiveness;

    PlantLoop.allocate(1);
    DRink.allocate(1);
    DRink(1).RefrigLoopNum = 1;
    PlantLoop(1).FluidName = "NH3";
    PlantLoop(1).FluidIndex = 1;

    // Set values of items that will stay constant for all calls to the HX Effectiveness function

    RefrigMassFlow = 0.1;
    TubeLength = 10;
    TubeDiameter = 0.1;
    SysNum = 1;
    NumCircs = 1;

    // Test 1: Cooling for Direct Refrigeration System
    Effectiveness = 0.0;
    Temperature = -10;
    // DRink(SysNum).CRefrigLoopNum = 1;

    Effectiveness = CalcEffectivenessDRink(SysNum, Temperature, RefrigMassFlow, NumCircs, TubeLength, TubeDiameter);
    EXPECT_NEAR(Effectiveness, 0.733, 0.001);
}

TEST_F(EnergyPlusFixture, IndoorIceRink_IRinkEff)
{
    Real64 Temperature;
    Real64 RefrigMassFlow;
    Real64 TubeLength;
    Real64 TubeDiameter;
    int SysNum;
    int NumCircs;
    Real64 Effectiveness;
    int RefrigType;
    Real64 Concentration;

    // Set values of items that will stay constant for all calls to the HX Effectiveness function

    RefrigMassFlow = 0.1;
    TubeLength = 10;
    TubeDiameter = 0.1;
    SysNum = 1;
    NumCircs = 1;

    // Test 1: Cooling for Indirect Refrigeration System with Calcium Chloride solution
    Effectiveness = 0.0;
    Temperature = -9.5;
    RefrigType = 1;
    Concentration = 25.00;

    Effectiveness = CalcEffectivenessIRink(SysNum, Temperature, RefrigMassFlow, NumCircs, TubeLength, TubeDiameter, RefrigType, Concentration);
    EXPECT_NEAR(Effectiveness, 0.1919, 0.01);

    // Test 2: Cooling for Indirect Refrigeration System with Ethylene Glycol solution
    Effectiveness = 0.0;
    Temperature = -9.5;
    RefrigType = 2;
    Concentration = 25.00;

    Effectiveness = CalcEffectivenessIRink(SysNum, Temperature, RefrigMassFlow, NumCircs, TubeLength, TubeDiameter, RefrigType, Concentration);
    EXPECT_NEAR(Effectiveness, 0.1303, 0.01);
}

TEST_F(EnergyPlusFixture, Resurfacing)
{
    Real64 HeatLoad;
    int SysNum;
    int SystemType;
    int MachineNum;
    DRink.allocate(1);
    IRink.allocate(1);
    Resurfacer.allocate(1);
    Schedule.allocate(1);

    // Set values of items that will stay constant
    SysNum = 1;
    MachineNum = 1;

    Schedule(1).CurrentValue = 3;
    Resurfacer(MachineNum).InitWaterTemp = 10.00;
    Resurfacer(MachineNum).TankCapacity = 3.00;
    Resurfacer(MachineNum).ResurfacingSchedPtr = 1;
    Resurfacer(MachineNum).ResurfacingWaterTemp = 15.00;
    Resurfacer(MachineNum).NoOfResurfEvents = 3;
    Resurfacer(MachineNum).GlycolIndex = 1;

    DRink(SysNum).IceSetptTemp = -3.00;
    DRink(SysNum).LengthRink = 60.00;
    DRink(SysNum).WidthRink = 30.00;
    DRink(SysNum).DepthRink = 0.1;

    IRink(SysNum).IceSetptTemp = -3.00;
    IRink(SysNum).LengthRink = 60.00;
    IRink(SysNum).WidthRink = 30.00;
    IRink(SysNum).DepthRink = 0.1;

    // Test 1: Resurfacing for DRink
    SystemType = 1;
    IceRinkResurfacer(HeatLoad, SysNum, SystemType, MachineNum);
    EXPECT_NEAR(HeatLoad, 3625796.78, 0.1);

    // Test 2: Resurfacing for IRink
    SystemType = 2;
    IceRinkResurfacer(HeatLoad, SysNum, SystemType, MachineNum);
    EXPECT_NEAR(HeatLoad, 3625796.78, 0.1);
}

TEST_F(EnergyPlusFixture, RinkFreezing)
{
    Real64 FreezingLoad;
    int SysNum;
    int SystemType;

    SysNum = 1;
    DRink.allocate(1);
    IRink.allocate(1);

    // Set values of items that will stay constant
    DRink(SysNum).LengthRink = 60.00;
    DRink(SysNum).WidthRink = 30.00;
    DRink(SysNum).IceThickness = 0.02;
    DRink(SysNum).IceSetptTemp = -3.00;
    DRink(SysNum).FloodWaterTemp = 15.00;
    DRink(SysNum).WaterIndex = 1;

    IRink(SysNum).LengthRink = 60.00;
    IRink(SysNum).WidthRink = 30.00;
    IRink(SysNum).IceThickness = 0.02;
    IRink(SysNum).IceSetptTemp = -3.00;
    IRink(SysNum).FloodWaterTemp = 15.00;
    IRink(SysNum).WaterIndex = 1;

    // Test 1: Freezing of DRink
    SystemType = 1;
    IceRinkFreezing(FreezingLoad, SysNum, SystemType);
    EXPECT_NEAR(FreezingLoad, 14482318172.4, 0.001);

    // Test 2: Freezing of IRink
    SystemType = 2;
    IceRinkFreezing(FreezingLoad, SysNum, SystemType);
    EXPECT_NEAR(FreezingLoad, 14482318172.4, 0.1);
}
