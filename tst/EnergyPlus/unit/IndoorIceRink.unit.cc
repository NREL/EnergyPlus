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

class IndoorIceRinkTest : public EnergyPlusFixture
{
public:
    int SysNum;
    int SystemType;
    Real64 ExpectedResult1;
    Real64 ExpectedResult2;
    Real64 ExpectedResult3;
    Real64 const CpWater = 4180.0;  // For estimating the expected result (J/kg.C)
    Real64 const RhoWater = 1000.0; // For estimating the expected result (kg/m3)

protected:
    virtual void SetUp()
    {
        EnergyPlusFixture::SetUp(); // Sets up the base fixture first.

        DRink.allocate(1);
        IRink.allocate(1);
        FinalZoneSizing.allocate(1);
        ZoneEqSizing.allocate(1);
        Zone.allocate(1);
        CurZoneEqNum = 1;
        ZoneEqSizing(CurZoneEqNum).SizingMethod.allocate(25);
        ZoneSizingRunDone = true;

        CurSysNum = 0;
        SysNum = 1;
        SystemType = DirectSystem;
        // HydronicRadiantSysNumericFields.allocate(1);
        // HydronicRadiantSysNumericFields(RadSysNum).FieldNames.allocate(15);
        DRink(SysNum).NumCircuits.allocate(1);
        // IRink(SysNum).NumCircuits.allocate(1);

        // Set up plant loop
        TotNumLoops = 1;
        PlantLoop.allocate(TotNumLoops);
        PlantSizData.allocate(TotNumLoops);
        NumPltSizInput = TotNumLoops;

        for (int loopindex = 1; loopindex <= TotNumLoops; ++loopindex) {
            auto &loop(PlantLoop(loopindex));
            loop.LoopSide.allocate(2);
            auto &loopside(PlantLoop(loopindex).LoopSide(1));
            loopside.TotalBranches = 1;
            loopside.Branch.allocate(1);
            auto &loopsidebranch(PlantLoop(loopindex).LoopSide(1).Branch(1));
            loopsidebranch.TotalComponents = 1;
            loopsidebranch.Comp.allocate(1);
        }
        PlantLoop(1).Name = "Refrigerant Loop";
        PlantLoop(1).FluidName = "AMMONIA";
        PlantLoop(1).FluidIndex = -2;

        PlantSizData(1).PlantLoopName = "Refrigerant Loop";
        PlantSizData(1).ExitTemp = 80.0;
        PlantSizData(1).DeltaT = 10.0;

        ExpectedResult1 = 0.0;
        ExpectedResult2 = 0.0;
        ExpectedResult3 = 0.0;
    }

    virtual void TearDown()
    {
        EnergyPlusFixture::TearDown(); // Remember to tear down the base fixture after cleaning up derived fixture!
    }
};

TEST_F(EnergyPlusFixture, IndoorIceRink_DirectSysHXEffectTest)
{
    Real64 Temperature;
    Real64 RefrigMassFlow;
    Real64 TubeLength;
    Real64 TubeDiameter;
    int SysNum;
    Real64 HXEffectFuncResult;

    // Set values of items that will stay constant for all calls to the HX Effectiveness function

    RefrigMassFlow = 0.1;
    TubeLength = 10;
    TubeDiameter = 0.1;
    SysNum = 1;
    // PlantLoop(1).FluidName = "WATER";

    // Test 1: Cooling for Direct Refrigeration System
    HXEffectFuncResult = 0.0;
    Temperature = -10;
    // DRink(SysNum).CRefrigLoopNum = 1;

    HXEffectFuncResult = CalcDRinkHXEffectTerm(Temperature, SysNum, RefrigMassFlow, TubeLength, TubeDiameter);
    EXPECT_NEAR(HXEffectFuncResult, 321.781, 0.001);
}

TEST_F(EnergyPlusFixture, IndoorIceRink_IndirectSysHXEffectTest)
{
    Real64 Temperature;
    Real64 RefrigMassFlow;
    Real64 TubeLength;
    Real64 TubeDiameter;
    int SysNum;
    Real64 HXEffectFuncResult;
    int RefrigType;
    Real64 Concentration;

    // Set values of items that will stay constant for all calls to the HX Effectiveness function

    RefrigMassFlow = 0.1;
    TubeLength = 10;
    TubeDiameter = 0.1;
    SysNum = 1;

    // Test 1: Cooling for Indirect Refrigeration System with Calcium Chloride solution
    HXEffectFuncResult = 0.0;
    Temperature = -9.5;
    RefrigType = 1;
    Concentration = 25.00;

    HXEffectFuncResult = CalcIRinkHXEffectTerm(Temperature, SysNum, RefrigMassFlow, TubeLength, TubeDiameter, RefrigType, Concentration);
    EXPECT_NEAR(HXEffectFuncResult, 54.471, 0.01);

    // Test 2: Cooling for Indirect Refrigeration System with Ethylene Glycol solution
    HXEffectFuncResult = 0.0;
    Temperature = -9.5;
    RefrigType = 2;
    Concentration = 25.00;
    
    HXEffectFuncResult = CalcIRinkHXEffectTerm(Temperature, SysNum, RefrigMassFlow, TubeLength, TubeDiameter, RefrigType, Concentration);
    EXPECT_NEAR(HXEffectFuncResult, 48.7575, 0.01);
}

TEST_F(EnergyPlusFixture, IndoorIceRink_BOTC)
{
    // Set values of items that will stay constant for all calls to the BOTC function
    int SysNum = 1;
    Real64 Result;
    // Test 1: BOTC test for direct refrigeration type ice rink
    /*DRink.allocate(1);
    Node.allocate(1);
    
    DRink(SysNum).RefrigerantName = "NH3";
    DRink(SysNum).ColdRefrigInNode = 1;
    DRink(SysNum).RefIndex = 1;
    DRink(SysNum).TubeDiameter = 1.0;
    DRink(SysNum).TubeLength = 200.0;
    DRink(SysNum).RefOutBOTCtrlTemp = -5.0;
    Node(1).Temp = 2.0;
    Result = BOTC(DirectSystem, SysNum);

    EXPECT_NEAR(Result, 20.0, 0.1);*/

    // Test 2: BOTC test for indirect refrigeration type ice rink
    Node.allocate(1);
    Node(1).Temp = 2.0;

    IRink.allocate(1);
    int SystemType = IndirectSystem;
    IRink(SysNum).RefrigerantName = "EG";
    IRink(SysNum).ColdRefrigInNode = 1;
    IRink(SysNum).RefIndex = 1;
    IRink(SysNum).TubeDiameter = 1;
    IRink(SysNum).TubeLength = 1000.0;
    IRink(SysNum).RefOutBOTCtrlTemp = -5.0;
    IRink(SysNum).Concentration = 10.00;
    IRink(SysNum).RefrigType = 2;
    Result = BOTC(IndirectSystem, SysNum);

    EXPECT_NEAR(Result, 10.0, 0.1);
}

TEST_F(EnergyPlusFixture, IndoorIceRink_Resurfacer)
{
    Real64 ResurfacerTank_capacity;      
    Real64 ResurfacingHWTemperature; 
    Real64 IceSurfaceTemperature;    
    Real64 InitResurfWaterTemp;
    int SysNum(1);
    int ResurfacerIndex(1);
    DRink.allocate(1);
    IRink.allocate(1);
    RefrigSysTypes.allocate(1);

    // Dimensions of Direct refrigeration type rink
    DRink(SysNum).LengthRink = 61;
    DRink(SysNum).WidthRink = 26;
    DRink(SysNum).DepthRink = 1.3;
    // Dimensions of Indirect refrigeration type rink
    IRink(SysNum).LengthRink = 61;
    IRink(SysNum).WidthRink = 26;
    IRink(SysNum).DepthRink = 1.3;

    Resurfacer.allocate(1);
    Resurfacer(ResurfacerIndex).GlycolIndex = 1;
    Real64 FunctResult;

    // Set values of items that will stay constant for all calls to the resurfacer function
    ResurfacerTank_capacity = 2;
    ResurfacingHWTemperature = 15;
    IceSurfaceTemperature = -3.00;
    InitResurfWaterTemp = 10.00;
    
    // Test 1: Test for direct refrigeration type ice rink
    RefrigSysTypes(SysNum).SystemType = 1;
    FunctResult = IceRinkResurfacer(ResurfacerTank_capacity,
                                    ResurfacingHWTemperature,
                                    IceSurfaceTemperature,
                                    InitResurfWaterTemp,
                                    ResurfacerIndex,
                                    SysNum);
    EXPECT_NEAR(FunctResult, 824493.416, 0.001);

    // Test 2: Test for indirect refrigeration type ice rink
    RefrigSysTypes(SysNum).SystemType = 2;
    FunctResult =
        IceRinkResurfacer(ResurfacerTank_capacity, 
                          ResurfacingHWTemperature, 
                          IceSurfaceTemperature, 
                          InitResurfWaterTemp, 
                          ResurfacerIndex, 
                          SysNum);
    EXPECT_NEAR(FunctResult, 824493.416, 0.001);


}
