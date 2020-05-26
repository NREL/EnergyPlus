// EnergyPlus, Copyright (c) 1996-2020, The Board of Trustees of the University of Illinois,
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy), Oak Ridge
// National Laboratory, managed by UT-Battelle, Alliance for Sustainable Energy, LLC, and other
// contributors. All rights reserved.
//
// NOTICE: This Software was developed under funding from the U.S. Department of Energy and the
// U.S. Government consequently retains certain rights. As such, the U.S. Government has been
// granted for itself and others acting on its behalf a paid-up, nonexclusive, irrevocable,
// worldwide license in the Software to reproduce, distribute copies to the public, prepare
// derivative works, and perform publicly and display publicly, and to permit others to do so.
//
// Redistribution and use in source and binary forms, with or without modification, are permitted
// provided that the following conditions are met:
//
// (1) Redistributions of source code must retain the above copyright notice, this list of
//     conditions and the following disclaimer.
//
// (2) Redistributions in binary form must reproduce the above copyright notice, this list of
//     conditions and the following disclaimer in the documentation and/or other materials
//     provided with the distribution.
//
// (3) Neither the name of the University of California, Lawrence Berkeley National Laboratory,
//     the University of Illinois, U.S. Dept. of Energy nor the names of its contributors may be
//     used to endorse or promote products derived from this software without specific prior
//     written permission.
//
// (4) Use of EnergyPlus(TM) Name. If Licensee (i) distributes the software in stand-alone form
//     without changes from the version obtained under this License, or (ii) Licensee makes a
//     reference solely to the software portion of its product, Licensee must refer to the
//     software as "EnergyPlus version X" software, where "X" is the version number Licensee
//     obtained under this License and may not use a different name for the software. Except as
//     specifically required in this Section (4), Licensee shall not use in a company name, a
//     product name, in advertising, publicity, or other promotional activities any name, trade
//     name, trademark, logo, or other designation of "EnergyPlus", "E+", "e+" or confusingly
//     similar designation, without the U.S. Department of Energy's prior written consent.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
// AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
// OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.

// EnergyPlus::FaultManager unit tests
// Fouling Air Filter

// Google test headers
#include <gtest/gtest.h>

// C++ Headers
#include <cmath>
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/FaultsManager.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/HVACControllers.hh>
#include <EnergyPlus/MixedAir.hh>
#include <EnergyPlus/OutputFiles.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SetPointManager.hh>
#include <EnergyPlus/WaterCoils.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace CurveManager;
using namespace DataLoopNode;
using namespace Fans;
using namespace FaultsManager;
using namespace EnergyPlus::ScheduleManager;

namespace EnergyPlus {

TEST_F(EnergyPlusFixture, FaultsManager_FaultFoulingAirFilters_CheckFaultyAirFilterFanCurve)
{
    // PURPOSE OF THIS SUBROUTINE:
    //     To check whether the fan curve specified in the FaultModel:Fouling:AirFilter object
    //     covers the rated operational point of the corresponding fan
    //     Return true if the curve covers the fan rated operational point

    int CurveNum;
    int FanNum;
    bool TestRestult;

    // Allocate
    NumCurves = 1;
    PerfCurve.allocate(NumCurves);

    state.fans.NumFans = 2;
    Fan.allocate(state.fans.NumFans);
    FaultsFouledAirFilters.allocate(state.fans.NumFans);

    // Inputs: fan curve
    CurveNum = 1;
    PerfCurve(CurveNum).CurveType = Cubic;
    PerfCurve(CurveNum).ObjectType = "Curve:Cubic";
    PerfCurve(CurveNum).InterpolationType = EvaluateCurveToLimits;
    PerfCurve(CurveNum).Coeff1 = 1151.1;
    PerfCurve(CurveNum).Coeff2 = 13.509;
    PerfCurve(CurveNum).Coeff3 = -0.9105;
    PerfCurve(CurveNum).Coeff4 = -0.0129;
    PerfCurve(CurveNum).Coeff5 = 0.0;
    PerfCurve(CurveNum).Coeff6 = 0.0;
    PerfCurve(CurveNum).Var1Min = 7.0;
    PerfCurve(CurveNum).Var1Max = 21.0;

    // Inputs:
    FanNum = 1;
    Fan(FanNum).FanName = "Fan_1";
    Fan(FanNum).FanType = "Fan:VariableVolume";
    Fan(FanNum).MaxAirFlowRate = 18.194;
    Fan(FanNum).DeltaPress = 1017.59;
    FaultsFouledAirFilters(FanNum).FaultyAirFilterFanName = "Fan_1";
    FaultsFouledAirFilters(FanNum).FaultyAirFilterFanCurvePtr = CurveNum;

    FanNum = 2;
    Fan(FanNum).FanName = "Fan_2";
    Fan(FanNum).FanType = "Fan:VariableVolume";
    Fan(FanNum).MaxAirFlowRate = 18.194;
    Fan(FanNum).DeltaPress = 1017.59 * 1.2;
    FaultsFouledAirFilters(FanNum).FaultyAirFilterFanName = "Fan_2";
    FaultsFouledAirFilters(FanNum).FaultyAirFilterFanCurvePtr = CurveNum;

    // Run and Check
    // (1)The rated operational point of Fan_1 falls on the fan curve
    FanNum = 1;
    TestRestult = FaultsFouledAirFilters(FanNum).CheckFaultyAirFilterFanCurve(state);
    EXPECT_TRUE(TestRestult);
    // (2)The rated operational point of Fan_2 does not fall on the fan curve
    FanNum = 2;
    TestRestult = FaultsFouledAirFilters(FanNum).CheckFaultyAirFilterFanCurve(state);
    EXPECT_FALSE(TestRestult);

    // Clean up
    PerfCurve.deallocate();
    Fan.deallocate();
}

TEST_F(EnergyPlusFixture, FaultsManager_FaultFoulingAirFilters_CalFaultyFanAirFlowReduction)
{
    // PURPOSE OF THIS SUBROUTINE:
    //     Calculate the decrease of the fan air flow rate, given the fan curve
    //     and the increase of fan pressure rise due to fouling air filters

    int CurveNum;
    int FanNum;
    double FanDesignFlowRateDec;
    double FanFaultyDeltaPressInc = 0.10; // Increase by 10%

    // Allocate
    NumCurves = 1;
    PerfCurve.allocate(NumCurves);

    state.fans.NumFans = 1;
    Fan.allocate(state.fans.NumFans);

    // Inputs: fan curve
    CurveNum = 1;
    PerfCurve(CurveNum).CurveType = Cubic;
    PerfCurve(CurveNum).ObjectType = "Curve:Cubic";
    PerfCurve(CurveNum).InterpolationType = EvaluateCurveToLimits;
    PerfCurve(CurveNum).Coeff1 = 1151.1;
    PerfCurve(CurveNum).Coeff2 = 13.509;
    PerfCurve(CurveNum).Coeff3 = -0.9105;
    PerfCurve(CurveNum).Coeff4 = -0.0129;
    PerfCurve(CurveNum).Coeff5 = 0.0;
    PerfCurve(CurveNum).Coeff6 = 0.0;
    PerfCurve(CurveNum).Var1Min = 7.0;
    PerfCurve(CurveNum).Var1Max = 21.0;

    // Inputs: fans
    FanNum = 1;
    Fan(FanNum).FanName = "Fan_1";
    Fan(FanNum).FanType = "Fan:VariableVolume";
    Fan(FanNum).MaxAirFlowRate = 18.194;
    Fan(FanNum).DeltaPress = 1017.59;

    // Run and Check
    FanDesignFlowRateDec = CalFaultyFanAirFlowReduction(
        Fan(FanNum).FanName, Fan(FanNum).MaxAirFlowRate, Fan(FanNum).DeltaPress, FanFaultyDeltaPressInc * Fan(FanNum).DeltaPress, CurveNum);

    EXPECT_NEAR(3.845, FanDesignFlowRateDec, 0.005);

    // Clean up
    PerfCurve.deallocate();
    Fan.deallocate();
}

TEST_F(EnergyPlusFixture, FaultsManager_TemperatureSensorOffset_CoilSAT)
{
    // PURPOSE OF THIS SUBROUTINE:
    //     Test the assignment of coil supply air temperature sensor offset fault information
    //     to the corresponding coil controller

    std::string const idf_objects = delimited_string({
        "                                                              ",
        "FaultModel:TemperatureSensorOffset:CoilSupplyAir,             ",
        "   Fault_SAT_CoolCoil1,!- Name                                ",
        "   ,                   !- Availability Schedule Name          ",
        "   ,                   !- Severity Schedule Name              ",
        "   Coil:Cooling:Water, !- Coil Object Type                    ",
        "   Chilled Water Coil, !- Coil Object Name                    ",
        "   CW Coil Controller, !- Water Coil Controller Name          ",
        "   2.0;                !- Reference Sensor Offset {deltaC}    ",
        "                                                              ",
        "Coil:Cooling:Water,                                           ",
        "   Chilled Water Coil, !- Name                                ",
        "   AvailSched,         !- Availability Schedule Name          ",
        "   autosize,           !- Design Water Flow Rate {m3/s}       ",
        "   autosize,           !- Design Air Flow Rate {m3/s}         ",
        "   autosize,           !- Design Inlet Water Temperature {C}  ",
        "   autosize,           !- Design Inlet Air Temperature {C}    ",
        "   autosize,           !- Design Outlet Air Temperature {C}   ",
        "   autosize,           !- Design Inlet Air Humidity Ratio {-} ",
        "   autosize,           !- Design Outlet Air Humidity Ratio {-}",
        "   Water Inlet Node,   !- Water Inlet Node Name               ",
        "   Water Outlet Node,  !- Water Outlet Node Name              ",
        "   Air Inlet Node,     !- Air Inlet Node Name                 ",
        "   Air Outlet Node,    !- Air Outlet Node Name                ",
        "   SimpleAnalysis,     !- Type of Analysis                    ",
        "   CrossFlow;          !- Heat Exchanger Configuration        ",
        "                                                              ",
        "Controller:WaterCoil,                                         ",
        "   CW Coil Controller, !- Name                                ",
        "   HumidityRatio,      !- Control Variable                    ",
        "   Reverse,            !- Action                              ",
        "   FLOW,               !- Actuator Variable                   ",
        "   Air Outlet Node,    !- Sensor Node Name                    ",
        "   Water Inlet Node,   !- Actuator Node Name                  ",
        "   autosize,           !- Controller Convergence Tolerance {C}",
        "   autosize,           !- Maximum Actuated Flow {m3/s}        ",
        "   0.0;                !- Minimum Actuated Flow {m3/s}        ",
        "                                                              ",
        "SetpointManager:Scheduled,                                    ",
        "   HumRatSPManager,    !- Name                                ",
        "   HumidityRatio,      !- Control Variable                    ",
        "   HumRatioSched,      !- Schedule Name                       ",
        "   Air Outlet Node;    !- Setpoint Node or NodeList Name      ",
        "                                                              ",
        "Schedule:Compact,                                             ",
        "   HumRatioSched,      !- Name                                ",
        "   Any Number,         !- Schedule Type Limits Name           ",
        "   Through: 12/31,     !- Field 1                             ",
        "   For: AllDays,       !- Field 2                             ",
        "   Until: 24:00, 0.015;!- Field 3                             ",
        "Schedule:Compact,                                             ",
        "   AvailSched,         !- Name                                ",
        "   Fraction,           !- Schedule Type Limits Name           ",
        "   Through: 12/31,     !- Field 1                             ",
        "   For: AllDays,       !- Field 2                             ",
        "   Until: 24:00, 1.0;  !- Field 3                             ",
        "                                                              ",
        "AirLoopHVAC:ControllerList,                                   ",
        "   CW Coil Controller, !- Name                                ",
        "   Controller:WaterCoil,!- Controller 1 Object Type           ",
        "   CW Coil Controller; !- Controller 1 Name                   ",
    });

    // Process inputs
    ASSERT_TRUE(process_idf(idf_objects));

    // Readin inputs
    SetPointManager::GetSetPointManagerInputs(state);
    HVACControllers::GetControllerInput(state);

    // Run
    CheckAndReadFaults(state);

    // Check
    EXPECT_EQ(2.0, FaultsCoilSATSensor(1).Offset);
    EXPECT_EQ("COIL:COOLING:WATER", FaultsCoilSATSensor(1).CoilType);
    EXPECT_TRUE(HVACControllers::ControllerProps(1).FaultyCoilSATFlag);
    EXPECT_EQ(1, HVACControllers::ControllerProps(1).FaultyCoilSATIndex);
}

TEST_F(EnergyPlusFixture, FaultsManager_FaultChillerSWTSensor_CalFaultChillerSWT)
{
    // PURPOSE OF THIS SUBROUTINE:
    // To check CalFaultChillerSWT which calculates the mass flow rate and supply water temperature of a chiller with faulty SWT sensor.

    bool FlagVariableFlow;         // True if chiller is variable flow and false if it is constant flow
    Real64 FaultyChillerSWTOffset; // Faulty chiller SWT sensor offset
    Real64 Cp = 4500;              // Local fluid specific heat
    Real64 EvapInletTemp = 12;     // Chiller evaporator inlet water temperature
    Real64 EvapOutletTemp = 7;     // Chiller evaporator outlet water temperature, fault free
    Real64 EvapMassFlowRate = 40;  // Chiller mass flow rate, fault free
    Real64 QEvaporator = 900000;   // Chiller evaporator heat transfer rate, fault free
    FaultPropertiesChillerSWT FaultChiller;

    // 1) offset is 0C
    FlagVariableFlow = false;
    Real64 EvapOutletTemp_1 = EvapOutletTemp;     // Chiller evaporator outlet water temperature
    Real64 EvapMassFlowRate_1 = EvapMassFlowRate; // Chiller mass flow rate
    Real64 QEvaporator_1 = QEvaporator;           // Chiller evaporator heat transfer rate
    FaultyChillerSWTOffset = 0;
    FaultChiller.CalFaultChillerSWT(FlagVariableFlow, FaultyChillerSWTOffset, Cp, EvapInletTemp, EvapOutletTemp_1, EvapMassFlowRate_1, QEvaporator_1);
    EXPECT_EQ(1, EvapOutletTemp_1 / EvapOutletTemp);
    EXPECT_EQ(1, QEvaporator_1 / QEvaporator);

    // 2) offset is 2C
    Real64 EvapOutletTemp_2 = EvapOutletTemp;     // Chiller evaporator outlet water temperature
    Real64 EvapMassFlowRate_2 = EvapMassFlowRate; // Chiller mass flow rate
    Real64 QEvaporator_2 = QEvaporator;           // Chiller evaporator heat transfer rate
    FaultyChillerSWTOffset = 2;
    FaultChiller.CalFaultChillerSWT(FlagVariableFlow, FaultyChillerSWTOffset, Cp, EvapInletTemp, EvapOutletTemp_2, EvapMassFlowRate_2, QEvaporator_2);
    EXPECT_NEAR(0.714, EvapOutletTemp_2 / EvapOutletTemp, 0.001);
    EXPECT_NEAR(1.400, QEvaporator_2 / QEvaporator, 0.001);

    // 3) offset is -2C
    Real64 EvapOutletTemp_3 = EvapOutletTemp;     // Chiller evaporator outlet water temperature
    Real64 EvapMassFlowRate_3 = EvapMassFlowRate; // Chiller mass flow rate
    Real64 QEvaporator_3 = QEvaporator;           // Chiller evaporator heat transfer rate
    FaultyChillerSWTOffset = -2;
    FaultChiller.CalFaultChillerSWT(FlagVariableFlow, FaultyChillerSWTOffset, Cp, EvapInletTemp, EvapOutletTemp_3, EvapMassFlowRate_3, QEvaporator_3);
    EXPECT_NEAR(1.285, EvapOutletTemp_3 / EvapOutletTemp, 0.001);
    EXPECT_NEAR(0.600, QEvaporator_3 / QEvaporator, 0.001);
}

TEST_F(EnergyPlusFixture, FaultsManager_CalFaultOffsetAct)
{
    // PURPOSE OF THIS SUBROUTINE:
    // To check CalFaultOffsetAct which calculates the dynamic fault offset based on the fault availability schedule and severity schedule.

    Real64 OffsetAct;
    FaultProperties Fault;

    Fault.AvaiSchedPtr = -1;
    Fault.SeveritySchedPtr = -1;
    Fault.Offset = 10;

    // Run and Check
    OffsetAct = Fault.CalFaultOffsetAct();
    EXPECT_EQ(10, OffsetAct);
}

TEST_F(EnergyPlusFixture, FaultsManager_EconomizerFaultGetInput)
{
    // PURPOSE OF THIS SUBROUTINE:
    // checks GetOAControllerInputs also fills economizer fault info to OA controller

    std::string const idf_objects = delimited_string({

        "  Controller:OutdoorAir,",
        "    VAV_1_OA_Controller,     !- Name",
        "    VAV_1_OARelief Node,     !- Relief Air Outlet Node Name",
        "    VAV_1 Supply Equipment Inlet Node,  !- Return Air Node Name",
        "    VAV_1_OA-VAV_1_CoolCNode,!- Mixed Air Node Name",
        "    VAV_1_OAInlet Node,      !- Actuator Node Name",
        "    AUTOSIZE,                !- Minimum Outdoor Air Flow Rate {m3/s}",
        "    AUTOSIZE,                !- Maximum Outdoor Air Flow Rate {m3/s}",
        "    DifferentialDryBulb,     !- Economizer Control Type",
        "    ModulateFlow,            !- Economizer Control Action Type",
        "    28.0,                    !- Economizer Maximum Limit Dry-Bulb Temperature {C}",
        "    64000.0,                 !- Economizer Maximum Limit Enthalpy {J/kg}",
        "    ,                        !- Economizer Maximum Limit Dewpoint Temperature {C}",
        "    ,                        !- Electronic Enthalpy Limit Curve Name",
        "    -100.0,                  !- Economizer Minimum Limit Dry-Bulb Temperature {C}",
        "    NoLockout,               !- Lockout Type",
        "    FixedMinimum,            !- Minimum Limit Type",
        "    MinOA_MotorizedDamper_Sched;  !- Minimum Outdoor Air Schedule Name",

        "  Controller:OutdoorAir,",
        "    VAV_2_OA_Controller,     !- Name",
        "    VAV_2_OARelief Node,     !- Relief Air Outlet Node Name",
        "    VAV_2 Supply Equipment Inlet Node,  !- Return Air Node Name",
        "    VAV_2_OA-VAV_2_CoolCNode,!- Mixed Air Node Name",
        "    VAV_2_OAInlet Node,      !- Actuator Node Name",
        "    AUTOSIZE,                !- Minimum Outdoor Air Flow Rate {m3/s}",
        "    AUTOSIZE,                !- Maximum Outdoor Air Flow Rate {m3/s}",
        "    DifferentialDryBulb,     !- Economizer Control Type",
        "    ModulateFlow,            !- Economizer Control Action Type",
        "    28.0,                    !- Economizer Maximum Limit Dry-Bulb Temperature {C}",
        "    64000.0,                 !- Economizer Maximum Limit Enthalpy {J/kg}",
        "    ,                        !- Economizer Maximum Limit Dewpoint Temperature {C}",
        "    ,                        !- Electronic Enthalpy Limit Curve Name",
        "    -100.0,                  !- Economizer Minimum Limit Dry-Bulb Temperature {C}",
        "    NoLockout,               !- Lockout Type",
        "    FixedMinimum,            !- Minimum Limit Type",
        "    MinOA_MotorizedDamper_Sched;  !- Minimum Outdoor Air Schedule Name",

        "  Schedule:Compact,",
        "    MinOA_MotorizedDamper_Sched,  !- Name",
        "    Fraction,                !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 07:00,0.0,        !- Field 3",
        "    Until: 22:00,1.0,        !- Field 4",
        "    Until: 24:00,0.0;        !- Field 5",

        "  Schedule:Compact,",
        "    ALWAYS_ON,               !- Name",
        "    On/Off,                  !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,1;          !- Field 3",

        "  Schedule:Compact,",
        "    OATSeveritySch,          !- Name",
        "    On/Off,                  !- Schedule Type Limits Name",
        "    Through: 6/30,           !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00,0,          !- Field 3",
        "    Through: 12/31,          !- Field 5",
        "    For: AllDays,            !- Field 6",
        "    Until: 24:00,1;          !- Field 7",

        "  FaultModel:TemperatureSensorOffset:OutdoorAir,",
        "    OATFault,                !- Name",
        "    ALWAYS_ON,               !- Availability Schedule Name",
        "    OATSeveritySch,          !- Severity Schedule Name",
        "    Controller:OutdoorAir,   !- Controller Object Type",
        "    VAV_1_OA_Controller,     !- Controller Object Name",
        "    2.0;                     !- Temperature Sensor Offset {deltaC}",

        "  FaultModel:HumiditySensorOffset:OutdoorAir,",
        "    OAWFault,                !- Name",
        "    ALWAYS_ON,               !- Availability Schedule Name",
        "    ,                        !- Severity Schedule Name",
        "    Controller:OutdoorAir,   !- Controller Object Type",
        "    VAV_1_OA_Controller,     !- Controller Object Name",
        "    -0.002;                  !- Humidity Sensor Offset {kgWater/kgDryAir}",

        "  FaultModel:EnthalpySensorOffset:OutdoorAir,",
        "    OAHFault,                !- Name",
        "    ALWAYS_ON,               !- Availability Schedule Name",
        "    ,                        !- Severity Schedule Name",
        "    Controller:OutdoorAir,   !- Controller Object Type",
        "    VAV_1_OA_Controller,     !- Controller Object Name",
        "    5000;                    !- Enthalpy Sensor Offset {J/kg}",

        "  FaultModel:TemperatureSensorOffset:ReturnAir,",
        "    RATFault,                !- Name",
        "    ,                        !- Availability Schedule Name",
        "    ,                        !- Severity Schedule Name",
        "    Controller:OutdoorAir,   !- Controller Object Type",
        "    VAV_2_OA_Controller,     !- Controller Object Name",
        "    -2.0;                    !- Temperature Sensor Offset {deltaC}",

        "  FaultModel:EnthalpySensorOffset:ReturnAir,",
        "    RAHFault,                !- Name",
        "    ,                        !- Availability Schedule Name",
        "    ,                        !- Severity Schedule Name",
        "    Controller:OutdoorAir,   !- Controller Object Type",
        "    VAV_2_OA_Controller,     !- Controller Object Name",
        "    -2000;                   !- Enthalpy Sensor Offset {J/kg}",
    });

    // Process inputs
    ASSERT_TRUE(process_idf(idf_objects));

    ScheduleManager::ProcessScheduleInput(state.outputFiles); // read schedules

    MixedAir::GetOAControllerInputs(state);

    // there are two OA controller objects
    EXPECT_EQ(MixedAir::NumOAControllers, 2);
    // there are five economizer faults
    EXPECT_EQ(FaultsManager::NumFaultyEconomizer, 5);

    // there are three economizer faults in the 1st OA controller
    EXPECT_EQ(MixedAir::OAController(1).NumFaultyEconomizer, 3);
    EXPECT_EQ(MixedAir::OAController(1).EconmizerFaultNum(1), 1);
    EXPECT_EQ(MixedAir::OAController(1).EconmizerFaultNum(2), 2);
    EXPECT_EQ(MixedAir::OAController(1).EconmizerFaultNum(3), 3);

    // there are two economizer faults in the 2nd OA controller
    EXPECT_EQ(MixedAir::OAController(2).NumFaultyEconomizer, 2);
    EXPECT_EQ(MixedAir::OAController(2).EconmizerFaultNum(1), 4);
    EXPECT_EQ(MixedAir::OAController(2).EconmizerFaultNum(2), 5);
}


TEST_F(EnergyPlusFixture, FaultsManager_FoulingCoil_CoilNotFound)
{
    // Test that an error is raised when coil not found
    std::string const idf_objects = delimited_string({

        "Schedule:Compact,                                             ",
        "   AvailSched,         !- Name                                ",
        "   ,                   !- Schedule Type Limits Name           ",
        "   Through: 12/31,     !- Field 1                             ",
        "   For: AllDays,       !- Field 2                             ",
        "   Until: 24:00, 1.0;  !- Field 3                             ",

        "FaultModel:Fouling:Coil,",
        "  FouledHeatingCoil,       !- Name",
        "  Non Existent Cooling Coil, !- Coil Name",
        "  ,                        !- Availability Schedule Name",
        "  ,                        !- Severity Schedule Name",
        "  FouledUARated,           !- Fouling Input Method",
        "  3.32;                    !- UAFouled {W/K}",

    });

    // Process inputs
    ASSERT_TRUE(process_idf(idf_objects));

    ASSERT_THROW(FaultsManager::CheckAndReadFaults(state), std::runtime_error);

    std::string const error_string = delimited_string({
        "   ** Severe  ** FaultModel:Fouling:Coil = \"FOULEDHEATINGCOIL\". Referenced Coil named \"NON EXISTENT COOLING COIL\" was not found.",
        "   **  Fatal  ** CheckAndReadFaults: Errors found in getting FaultModel input data. Preceding condition(s) cause termination.",
        "   ...Summary of Errors that led to program termination:",
        "   ..... Reference severe error count=1",
        "   ..... Last severe error=FaultModel:Fouling:Coil = \"FOULEDHEATINGCOIL\". Referenced Coil named \"NON EXISTENT COOLING COIL\" was not found.",
    });

    EXPECT_TRUE(compare_err_stream(error_string, true));
}

TEST_F(EnergyPlusFixture, FaultsManager_FoulingCoil_BadCoilType)
{
    // Test that an error is raised if the coil is found, but it's not one of the supported types
    // Note JM 2020-02-04: As of today, only Simple Heating / Cooling water coils are supported

    std::string const idf_objects = delimited_string({

        "Schedule:Compact,                                             ",
        "   AvailSched,         !- Name                                ",
        "   ,                   !- Schedule Type Limits Name           ",
        "   Through: 12/31,     !- Field 1                             ",
        "   For: AllDays,       !- Field 2                             ",
        "   Until: 24:00, 1.0;  !- Field 3                             ",

        "  Coil:Cooling:Water:DetailedGeometry,",
        "    Detailed Pre Cooling Coil, !- Name",
        "    ,                        !- Availability Schedule Name",
        "    autosize,                !- Maximum Water Flow Rate {m3/s}",
        "    autosize,                !- Tube Outside Surface Area {m2}",
        "    autosize,                !- Total Tube Inside Area {m2}",
        "    autosize,                !- Fin Surface Area {m2}",
        "    autosize,                !- Minimum Airflow Area {m2}",
        "    autosize,                !- Coil Depth {m}",
        "    autosize,                !- Fin Diameter {m}",
        "    ,                        !- Fin Thickness {m}",
        "    ,                        !- Tube Inside Diameter {m}",
        "    ,                        !- Tube Outside Diameter {m}",
        "    ,                        !- Tube Thermal Conductivity {W/m-K}",
        "    ,                        !- Fin Thermal Conductivity {W/m-K}",
        "    ,                        !- Fin Spacing {m}",
        "    ,                        !- Tube Depth Spacing {m}",
        "    ,                        !- Number of Tube Rows",
        "    autosize,                !- Number of Tubes per Row",
        "    Main Cooling Coil 1 Water Inlet Node,  !- Water Inlet Node Name",
        "    Main Cooling Coil 1 Water Outlet Node,  !- Water Outlet Node Name",
        "    Main Cooling Coil 1 Inlet Node,  !- Air Inlet Node Name",
        "    Main Cooling Coil 1 Outlet Node;  !- Air Outlet Node Name",

        "FaultModel:Fouling:Coil,",
        "  FouledHeatingCoil,       !- Name",
        "  Detailed Pre Cooling Coil, !- Coil Name",
        "  ,                        !- Availability Schedule Name",
        "  ,                        !- Severity Schedule Name",
        "  FouledUARated,           !- Fouling Input Method",
        "  3.32;                    !- UAFouled {W/K}",

    });

    // Process inputs
    ASSERT_TRUE(process_idf(idf_objects));

    ASSERT_THROW(FaultsManager::CheckAndReadFaults(state), std::runtime_error);

    std::string const error_string = delimited_string({
        "   ** Severe  ** FaultModel:Fouling:Coil = \"FOULEDHEATINGCOIL\" invalid Coil Name = \"DETAILED PRE COOLING COIL\".",
        "   **   ~~~   ** Coil was found but it is not one of the supported types (\"Coil:Cooling:Water\" or \"Coil:Heating:Water\").",
        "   **  Fatal  ** CheckAndReadFaults: Errors found in getting FaultModel input data. Preceding condition(s) cause termination.",
        "   ...Summary of Errors that led to program termination:",
        "   ..... Reference severe error count=1",
        "   ..... Last severe error=FaultModel:Fouling:Coil = \"FOULEDHEATINGCOIL\" invalid Coil Name = \"DETAILED PRE COOLING COIL\".",
    });

    EXPECT_TRUE(compare_err_stream(error_string, true));
}

TEST_F(EnergyPlusFixture, FaultsManager_FoulingCoil_AssignmentAndCalc)
{
    // Test for #6313. Ensure Faults and coils are correctly linked, fault input is properly read, and CalFaultyCoilFoulingFactor correctly works
    std::string const idf_objects = delimited_string({

        "ScheduleTypeLimits,",
        "  Fraction,                !- Name",
        "  0,                       !- Lower Limit Value",
        "  1,                       !- Upper Limit Value",
        "  Continuous;              !- Numeric Type",

        "Schedule:Compact,",
        "  AvailSched,              !- Name",
        "  Fraction,                !- Schedule Type Limits Name",
        "  Through: 12/31,          !- Field 1",
        "  For: AllDays,            !- Field 2",
        "  Until: 24:00,1.00;       !- Field 3",

        "Schedule:Compact,",
        "  SeveritySched,           !- Name",
        "  Fraction,                !- Schedule Type Limits Name",
        "  Through: 12/31,          !- Field 1",
        "  For: AllDays,            !- Field 2",
        "  Until: 24:00,0.75;       !- Field 3",

        "Coil:Heating:Water,",
        "  AHU HW Heating Coil,     !- Name",
        "  AvailSched,              !- Availability Schedule Name",
        "  6.64,                    !- U-Factor Times Area Value {W/K}",
        "  0.000010,                !- Maximum Water Flow Rate {m3/s}",
        "  AHU HW Heating Coil Water Inlet Node,  !- Water Inlet Node Name",
        "  AHU HW Heating Coil Water Outlet Node,  !- Water Outlet Node Name",
        "  Air Loop Referenz AHU Cooling Coil Air Outlet Node,  !- Air Inlet Node Name",
        "  AHU HW Heating Coil Air Outlet Node,  !- Air Outlet Node Name",
        "  UFactorTimesAreaAndDesignWaterFlowRate,  !- Performance Input Method",
        "  438.32,                  !- Rated Capacity {W}",
        "  80,                      !- Rated Inlet Water Temperature {C}",
        "  16,                      !- Rated Inlet Air Temperature {C}",
        "  70,                      !- Rated Outlet Water Temperature {C}",
        "  35,                      !- Rated Outlet Air Temperature {C}",
        "  0.50;                    !- Rated Ratio for Air and Water Convection",

        "FaultModel:Fouling:Coil,",
        "  FouledHeatingCoil,       !- Name",
        "  AHU HW Heating Coil,     !- Coil Name",
        "  ,                        !- Availability Schedule Name",
        "  SeveritySched,           !- Severity Schedule Name",
        "  FouledUARated,           !- Fouling Input Method",
        "  3.32;                    !- UAFouled {W/K}",

        "Coil:Cooling:Water,",
        "   AHU CHW Cooling Coil,   !- Name",
        "   AvailSched,             !- Availability Schedule Name",
        "   autosize,               !- Design Water Flow Rate {m3/s}",
        "   autosize,               !- Design Air Flow Rate {m3/s}",
        "   autosize,               !- Design Inlet Water Temperature {C}",
        "   autosize,               !- Design Inlet Air Temperature {C}",
        "   autosize,               !- Design Outlet Air Temperature {C}",
        "   autosize,               !- Design Inlet Air Humidity Ratio {-}",
        "   autosize,               !- Design Outlet Air Humidity Ratio {-}",
        "   Water Inlet Node,       !- Water Inlet Node Name",
        "   Water Outlet Node,      !- Water Outlet Node Name",
        "   Air Inlet Node,         !- Air Inlet Node Name",
        "   Air Outlet Node,        !- Air Outlet Node Name",
        "   SimpleAnalysis,         !- Type of Analysis",
        "   CrossFlow;              !- Heat Exchanger Configuration",

        "FaultModel:Fouling:Coil,",
        "  FouledCoolingCoil,       !- Name",
        "  AHU CHW Cooling Coil,    !- Coil Name",
        "  AvailSched,              !- Availability Schedule Name",
        "  SeveritySched,           !- Severity Schedule Name",
        "  FoulingFactor,           !- Fouling Input Method",
        "  ,                        !- UAFouled {W/K}",
        // Note: don't mind these values, there are plain bogus/unresearched
        "  0.0005,                  !- Water Side Fouling Factor, m2-K/W",
        "  0.0001,                  !- Air Side Fouling Factor, m2-K/W",
        "  100.0,                   !- Outside Coil Surface Area, m2",
        "  0.1;                     !- Inside to Outside Coil Surface Area Ratio",

        "Coil:Cooling:Water,",
        "   AHU CHW Coil With no fault, !- Name",
        "   AvailSched,             !- Availability Schedule Name",
        "   autosize,               !- Design Water Flow Rate {m3/s}",
        "   autosize,               !- Design Air Flow Rate {m3/s}",
        "   autosize,               !- Design Inlet Water Temperature {C}",
        "   autosize,               !- Design Inlet Air Temperature {C}",
        "   autosize,               !- Design Outlet Air Temperature {C}",
        "   autosize,               !- Design Inlet Air Humidity Ratio {-}",
        "   autosize,               !- Design Outlet Air Humidity Ratio {-}",
        "   Water 2 Inlet Node,     !- Water Inlet Node Name",
        "   Water 2 Outlet Node,    !- Water Outlet Node Name",
        "   Air 2 Inlet Node,       !- Air Inlet Node Name",
        "   Air 2 Outlet Node,      !- Air Outlet Node Name",
        "   SimpleAnalysis,         !- Type of Analysis",
        "   CrossFlow;              !- Heat Exchanger Configuration",
    });

    // Process inputs
    ASSERT_TRUE(process_idf(idf_objects));

    DataHVACGlobals::TimeStepSys = 1;
    DataGlobals::NumOfTimeStepInHour = 4;
    DataGlobals::MinutesPerTimeStep = 60 / DataGlobals::NumOfTimeStepInHour;

    ScheduleManager::ProcessScheduleInput(state.outputFiles);  // read schedule data
    int avaiSchedIndex = ScheduleManager::GetScheduleIndex("AVAILSCHED");
    EXPECT_EQ(1, avaiSchedIndex);
    int severitySchedIndex = ScheduleManager::GetScheduleIndex("SEVERITYSCHED");
    EXPECT_EQ(2, severitySchedIndex);


    // Readin inputs
    //SetPointManager::GetSetPointManagerInputs();
    //HVACControllers::GetControllerInput();

    // Run
    ASSERT_NO_THROW(FaultsManager::CheckAndReadFaults(state));

    // Read schedule values
    DataGlobals::TimeStep = 1;
    DataGlobals::HourOfDay = 1;
    DataEnvironment::DayOfWeek = 1;
    DataEnvironment::DayOfYear_Schedule = 1;
    ScheduleManager::UpdateScheduleValues();

    EXPECT_EQ(2, FaultsManager::NumFouledCoil);
    // This should also have called WaterCoil::GetWaterCoilInput
    EXPECT_EQ(3, WaterCoils::NumWaterCoils);


    // Check that fault association actually happened
    {
        int CoilNum = 1;
        int FaultIndex = 1;
        EXPECT_EQ("AHU HW HEATING COIL", WaterCoils::WaterCoil(CoilNum).Name);
        EXPECT_NEAR(6.64, WaterCoils::WaterCoil(CoilNum).UACoil, 0.0001);
        EXPECT_EQ(WaterCoils::WaterCoil_SimpleHeating, WaterCoils::WaterCoil(CoilNum).WaterCoilType_Num);

        EXPECT_EQ(CoilNum, FaultsManager::FouledCoils(FaultIndex).FouledCoilNum);
        EXPECT_EQ(WaterCoils::WaterCoil_SimpleHeating, FaultsManager::FouledCoils(FaultIndex).FouledCoiledType);

        EXPECT_TRUE(WaterCoils::WaterCoil(CoilNum).FaultyCoilFoulingFlag);
        EXPECT_EQ(FaultIndex, WaterCoils::WaterCoil(CoilNum).FaultyCoilFoulingIndex);

        // Doesn't have an Availability Schedule
        EXPECT_EQ(-1, FaultsManager::FouledCoils(FaultIndex).AvaiSchedPtr);
        // Has a Severity Schedule
        EXPECT_EQ("SEVERITYSCHED", FaultsManager::FouledCoils(FaultIndex).SeveritySchedule);
        EXPECT_EQ(severitySchedIndex, FaultsManager::FouledCoils(FaultIndex).SeveritySchedPtr);

        EXPECT_EQ(FaultsManager::iFouledCoil_UARated, FaultsManager::FouledCoils(FaultIndex).FoulingInputMethod);
        EXPECT_NEAR(3.32, FaultsManager::FouledCoils(FaultIndex).UAFouled, 0.0001);

        // Check calculation
        // Expected FaultFrac * (1/UAfouled - 1 / UACoilTotal)
        // Real64 expectedFoulingFactor = 0.75 * ((1.0 / 3.32) - (1.0 / 6.64));
        // EXPECT_NEAR(expectedFoulingFactor, FaultsManager::FouledCoils(FaultIndex).CalFaultyCoilFoulingFactor(), 0.0001);
    }

    // Cooling Coil, method is "FoulingFactor"
    {
        int CoilNum = 2;
        int FaultIndex = 2;
        EXPECT_EQ("AHU CHW COOLING COIL", WaterCoils::WaterCoil(CoilNum).Name);
        EXPECT_EQ(WaterCoils::WaterCoil_Cooling, WaterCoils::WaterCoil(CoilNum).WaterCoilType_Num);

        EXPECT_EQ(CoilNum, FaultsManager::FouledCoils(FaultIndex).FouledCoilNum);
        EXPECT_EQ(WaterCoils::WaterCoil_Cooling, FaultsManager::FouledCoils(FaultIndex).FouledCoiledType);

        EXPECT_TRUE(WaterCoils::WaterCoil(CoilNum).FaultyCoilFoulingFlag);
        EXPECT_EQ(FaultIndex, WaterCoils::WaterCoil(CoilNum).FaultyCoilFoulingIndex);

        // Has an Availabity Schedule
        EXPECT_EQ("AVAILSCHED", FaultsManager::FouledCoils(FaultIndex).AvaiSchedule);
        EXPECT_EQ(avaiSchedIndex, FaultsManager::FouledCoils(FaultIndex).AvaiSchedPtr);
        // Has a Severity Schedule
        EXPECT_EQ("SEVERITYSCHED", FaultsManager::FouledCoils(FaultIndex).SeveritySchedule);
        EXPECT_EQ(severitySchedIndex, FaultsManager::FouledCoils(FaultIndex).SeveritySchedPtr);

        EXPECT_EQ(FaultsManager::iFouledCoil_FoulingFactor, FaultsManager::FouledCoils(FaultIndex).FoulingInputMethod);
        EXPECT_NEAR(0.0005, FaultsManager::FouledCoils(FaultIndex).Rfw, 0.0001);
        EXPECT_NEAR(0.0001, FaultsManager::FouledCoils(FaultIndex).Rfa, 0.0001);
        EXPECT_NEAR(100.0, FaultsManager::FouledCoils(FaultIndex).Aout, 0.01);
        EXPECT_NEAR(0.1, FaultsManager::FouledCoils(FaultIndex).Aratio, 0.0001);

        // Check calculation
        //Real64 waterTerm = 0.0005 / (100.0*0.1); // Rf_water/A_water = Rfw / (Aout * Aratio)
        //Real64 airTerm = 0.0001 / 100.0;         // Rf_air/A_air = Rfa / Aout
        // Expected FaultFrac * (waterTerm + airTerm)
        // Real64 expectedFoulingFactor = 0.75 * (waterTerm + airTerm);
        // EXPECT_NEAR(expectedFoulingFactor, FaultsManager::FouledCoils(FaultIndex).CalFaultyCoilFoulingFactor(), 0.0001);
    }

    // No association if not meant!
    {
        int CoilNum = 3;
        EXPECT_EQ("AHU CHW COIL WITH NO FAULT", WaterCoils::WaterCoil(CoilNum).Name);
        EXPECT_EQ(WaterCoils::WaterCoil_Cooling, WaterCoils::WaterCoil(CoilNum).WaterCoilType_Num);

        EXPECT_FALSE(WaterCoils::WaterCoil(CoilNum).FaultyCoilFoulingFlag);
        EXPECT_EQ(0, WaterCoils::WaterCoil(CoilNum).FaultyCoilFoulingIndex);
    }

}

} // namespace EnergyPlus
