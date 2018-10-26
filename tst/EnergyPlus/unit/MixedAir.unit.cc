// EnergyPlus, Copyright (c) 1996-2018, The Board of Trustees of the University of Illinois,
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

// EnergyPlus::MixedAir Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataContaminantBalance.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataZoneControls.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/Humidifiers.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/MixedAir.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SizingManager.hh>

#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/OutAirNodeManager.hh>

using namespace EnergyPlus::MixedAir;
using namespace EnergyPlus::DataContaminantBalance;
using namespace EnergyPlus::DataAirLoop;
using namespace EnergyPlus::DataAirSystems;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::DataHeatBalance;
using namespace EnergyPlus::ScheduleManager;
using namespace EnergyPlus::DataEnvironment;
using namespace EnergyPlus::DataZoneEquipment;
using namespace EnergyPlus::DataLoopNode;
using namespace EnergyPlus::DataZoneEnergyDemands;
using namespace EnergyPlus::DataZoneControls;
using namespace EnergyPlus::HeatBalanceManager;
using namespace EnergyPlus::Humidifiers;
using namespace EnergyPlus::SizingManager;

namespace EnergyPlus {

TEST_F(EnergyPlusFixture, MixedAir_ProcessOAControllerTest)
{
    std::string const idf_objects = delimited_string({
        "Version,8.3;",
        "  OutdoorAir:Node,",
        "    Outside Air Inlet Node 1; !- Name",
        "  Controller:OutdoorAir,",
        "    OA Controller 1,         !- Name",
        "    Relief Air Outlet Node 1, !- Relief Air Outlet Node Name",
        "    VAV Sys 1 Inlet Node,    !- Return Air Node Name",
        "    Mixed Air Node 1,        !- Mixed Air Node Name",
        "    Outside Air Inlet Node 1, !- Actuator Node Name",
        "    autosize,                !- Minimum Outdoor Air Flow Rate {m3/s}",
        "    autosize,                !- Maximum Outdoor Air Flow Rate {m3/s}",
        "    NoEconomizer,            !- Economizer Control Type",
        "    ModulateFlow,            !- Economizer Control Action Type",
        "    ,                        !- Economizer Maximum Limit Dry-Bulb Temperature {C}",
        "    ,                        !- Economizer Maximum Limit Enthalpy {J/kg}",
        "    ,                        !- Economizer Maximum Limit Dewpoint Temperature {C}",
        "    ,                        !- Electronic Enthalpy Limit Curve Name",
        "    ,                        !- Economizer Minimum Limit Dry-Bulb Temperature {C}",
        "    NoLockout,               !- Lockout Type",
        "    ProportionalMinimum;     !- Minimum Limit Type",
        "  Controller:OutdoorAir,",
        "    OA Controller 2,         !- Name",
        "    Relief Air Outlet Node 2, !- Relief Air Outlet Node Name",
        "    VAV Sys 2 Inlet Node,    !- Return Air Node Name",
        "    Mixed Air Node 2,        !- Mixed Air Node Name",
        "    Outside Air Inlet Node 2, !- Actuator Node Name",
        "    autosize,                !- Minimum Outdoor Air Flow Rate {m3/s}",
        "    autosize,                !- Maximum Outdoor Air Flow Rate {m3/s}",
        "    NoEconomizer,            !- Economizer Control Type",
        "    ModulateFlow,            !- Economizer Control Action Type",
        "    ,                        !- Economizer Maximum Limit Dry-Bulb Temperature {C}",
        "    ,                        !- Economizer Maximum Limit Enthalpy {J/kg}",
        "    ,                        !- Economizer Maximum Limit Dewpoint Temperature {C}",
        "    ,                        !- Electronic Enthalpy Limit Curve Name",
        "    ,                        !- Economizer Minimum Limit Dry-Bulb Temperature {C}",
        "    NoLockout,               !- Lockout Type",
        "    ProportionalMinimum;     !- Minimum Limit Type",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    bool ErrorsFound(false); // If errors detected in input
    int ControllerNum(0);    // Controller number
    int NumArg(0);
    int NumNums(0);
    int NumAlphas(0);
    int IOStat(0);
    std::string const CurrentModuleObject = CurrentModuleObjects(CMO_OAController);

    inputProcessor->getObjectDefMaxArgs(CurrentModuleObjects(CMO_OAController), NumArg, NumAlphas, NumNums);

    Array1D<Real64> NumArray(NumNums, 0.0);
    Array1D_string AlphArray(NumAlphas);
    Array1D_string cAlphaFields(NumAlphas);
    Array1D_string cNumericFields(NumNums);
    Array1D_bool lAlphaBlanks(NumAlphas, true);
    Array1D_bool lNumericBlanks(NumNums, true);

    NumOAControllers = inputProcessor->getNumObjectsFound(CurrentModuleObject);
    OAController.allocate(NumOAControllers);

    ControllerNum = 1;

    inputProcessor->getObjectItem(CurrentModuleObject,
                                  ControllerNum,
                                  AlphArray,
                                  NumAlphas,
                                  NumArray,
                                  NumNums,
                                  IOStat,
                                  lNumericBlanks,
                                  lAlphaBlanks,
                                  cAlphaFields,
                                  cNumericFields);

    ProcessOAControllerInputs(CurrentModuleObject,
                              ControllerNum,
                              AlphArray,
                              NumAlphas,
                              NumArray,
                              NumNums,
                              lNumericBlanks,
                              lAlphaBlanks,
                              cAlphaFields,
                              cNumericFields,
                              ErrorsFound);

    EXPECT_FALSE(ErrorsFound);
    EXPECT_EQ(2, OAController(1).OANode);
    EXPECT_TRUE(OutAirNodeManager::CheckOutAirNodeNumber(OAController(1).OANode));

    ControllerNum = 2;
    inputProcessor->getObjectItem(CurrentModuleObject,
                                  ControllerNum,
                                  AlphArray,
                                  NumAlphas,
                                  NumArray,
                                  NumNums,
                                  IOStat,
                                  lNumericBlanks,
                                  lAlphaBlanks,
                                  cAlphaFields,
                                  cNumericFields);

    ErrorsFound = false;
    ProcessOAControllerInputs(CurrentModuleObject,
                              ControllerNum,
                              AlphArray,
                              NumAlphas,
                              NumArray,
                              NumNums,
                              lNumericBlanks,
                              lAlphaBlanks,
                              cAlphaFields,
                              cNumericFields,
                              ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    EXPECT_EQ(6, OAController(2).OANode);
    EXPECT_FALSE(OutAirNodeManager::CheckOutAirNodeNumber(OAController(2).OANode));
}

TEST_F(EnergyPlusFixture, MixedAir_HXBypassOptionTest)
{
    std::string const idf_objects = delimited_string(
        {"Version,8.3;",
         "  OutdoorAir:Node,",
         "    Outside Air Inlet Node 1; !- Name",
         "  Controller:OutdoorAir,",
         "    OA Controller 1,         !- Name",
         "    Relief Air Outlet Node 1, !- Relief Air Outlet Node Name",
         "    VAV Sys 1 Inlet Node,    !- Return Air Node Name",
         "    Mixed Air Node 1,        !- Mixed Air Node Name",
         "    Outside Air Inlet Node 1, !- Actuator Node Name",
         "    0.2,                !- Minimum Outdoor Air Flow Rate {m3/s}",
         "    1.0,                !- Maximum Outdoor Air Flow Rate {m3/s}",
         "    DifferentialDryBulb,     !- Economizer Control Type", // Economizer should open for this one, so OA flow should be > min OA
         "    ModulateFlow,            !- Economizer Control Action Type",
         "    ,                        !- Economizer Maximum Limit Dry-Bulb Temperature {C}",
         "    ,                        !- Economizer Maximum Limit Enthalpy {J/kg}",
         "    ,                        !- Economizer Maximum Limit Dewpoint Temperature {C}",
         "    ,                        !- Electronic Enthalpy Limit Curve Name",
         "    ,                        !- Economizer Minimum Limit Dry-Bulb Temperature {C}",
         "    NoLockout,               !- Lockout Type", // No lockout
         "    ProportionalMinimum,     !- Minimum Limit Type",
         "    ,                        !- Minimum Outdoor Air Schedule Name",
         "    ,                        !- Minimum Fraction of Outdoor Air Schedule Name",
         "    ,                        !- Maximum Fraction of Outdoor Air Schedule Name",
         "    ,                        !- Mechanical Ventilation Controller Name",
         "    ,                        !- Time of Day Economizer Control Schedule Name",
         "    No,                      !- High Humidity Control",
         "    ,                        !- Humidistat Control Zone Name",
         "    ,                        !- High Humidity Outdoor Air Flow Ratio",
         "    Yes,                     !- Control High Indoor Humidity Based on Outdoor Humidity Ratio",
         "    BypassWhenWithinEconomizerLimits;  !- Heat Recovery Bypass Control Type", // HX bypass should be true

         "  OutdoorAir:Mixer,",
         "    OA Mixer 1,                !- Name",
         "    Mixed Air Node 1,          !- Mixed Air Node Name",
         "    Outside Air Inlet Node 1, !- Outdoor Air Stream Node Name",
         "    Relief Air Outlet Node 1,  !- Relief Air Stream Node Name",
         "    VAV Sys 1 Inlet Node;     !- Return Air Stream Node Name",

         " AirLoopHVAC:ControllerList,",
         "    OA Sys 1 controller,     !- Name",
         "    Controller:OutdoorAir,   !- Controller 1 Object Type",
         "    OA Controller 1;         !- Controller 1 Name",

         " AirLoopHVAC:OutdoorAirSystem:EquipmentList,",
         "    OA Sys 1 Equipment list, !- Name",
         "    OutdoorAir:Mixer,        !- Component 1 Object Type",
         "    OA Mixer 1;                !- Component 1 Name",

         " AirLoopHVAC:OutdoorAirSystem,",
         "    OA Sys 1, !- Name",
         "    OA Sys 1 controller,     !- Controller List Name",
         "    OA Sys 1 Equipment list; !- Outdoor Air Equipment List Name",

         "  Controller:OutdoorAir,",
         "    OA Controller 2,         !- Name",
         "    Relief Air Outlet Node 2, !- Relief Air Outlet Node Name",
         "    VAV Sys 2 Inlet Node,    !- Return Air Node Name",
         "    Mixed Air Node 2,        !- Mixed Air Node Name",
         "    Outside Air Inlet Node 2, !- Actuator Node Name",
         "    0.2,                !- Minimum Outdoor Air Flow Rate {m3/s}",
         "    1.0,                !- Maximum Outdoor Air Flow Rate {m3/s}",
         "    DifferentialDryBulb,     !- Economizer Control Type", // Economizer should be locked out for this one, so OA flow should = min OA
         "    ModulateFlow,            !- Economizer Control Action Type",
         "    ,                        !- Economizer Maximum Limit Dry-Bulb Temperature {C}",
         "    ,                        !- Economizer Maximum Limit Enthalpy {J/kg}",
         "    ,                        !- Economizer Maximum Limit Dewpoint Temperature {C}",
         "    ,                        !- Electronic Enthalpy Limit Curve Name",
         "    ,                        !- Economizer Minimum Limit Dry-Bulb Temperature {C}",
         "    LockoutWithHeating,               !- Lockout Type", // Lockout with heating is on
         "    ProportionalMinimum,     !- Minimum Limit Type",
         "    ,                        !- Minimum Outdoor Air Schedule Name",
         "    ,                        !- Minimum Fraction of Outdoor Air Schedule Name",
         "    ,                        !- Maximum Fraction of Outdoor Air Schedule Name",
         "    ,                        !- Mechanical Ventilation Controller Name",
         "    ,                        !- Time of Day Economizer Control Schedule Name",
         "    No,                      !- High Humidity Control",
         "    ,                        !- Humidistat Control Zone Name",
         "    ,                        !- High Humidity Outdoor Air Flow Ratio",
         "    Yes,                     !- Control High Indoor Humidity Based on Outdoor Humidity Ratio",
         "    BypassWhenWithinEconomizerLimits;  !- Heat Recovery Bypass Control Type", // HX bypass should be false because economizer is locked out

         "  OutdoorAir:Mixer,",
         "    OA Mixer 2,                !- Name",
         "    Mixed Air Node 2,          !- Mixed Air Node Name",
         "    Outside Air Inlet Node 2, !- Outdoor Air Stream Node Name",
         "    Relief Air Outlet Node 2,  !- Relief Air Stream Node Name",
         "    VAV Sys 2 Inlet Node;     !- Return Air Stream Node Name",

         " AirLoopHVAC:ControllerList,",
         "    OA Sys 2 controller,     !- Name",
         "    Controller:OutdoorAir,   !- Controller 1 Object Type",
         "    OA Controller 2;         !- Controller 1 Name",

         " AirLoopHVAC:OutdoorAirSystem:EquipmentList,",
         "    OA Sys 2 Equipment list, !- Name",
         "    OutdoorAir:Mixer,        !- Component 1 Object Type",
         "    OA Mixer 2;                !- Component 1 Name",

         " AirLoopHVAC:OutdoorAirSystem,",
         "    OA Sys 2, !- Name",
         "    OA Sys 2 controller,     !- Controller List Name",
         "    OA Sys 2 Equipment list; !- Outdoor Air Equipment List Name",

         "  Controller:OutdoorAir,",
         "    OA Controller 3,         !- Name",
         "    Relief Air Outlet Node 3, !- Relief Air Outlet Node Name",
         "    VAV Sys 3 Inlet Node,    !- Return Air Node Name",
         "    Mixed Air Node 3,        !- Mixed Air Node Name",
         "    Outside Air Inlet Node 3, !- Actuator Node Name",
         "    0.2,                !- Minimum Outdoor Air Flow Rate {m3/s}",
         "    1.0,                !- Maximum Outdoor Air Flow Rate {m3/s}",
         "    DifferentialDryBulb,     !- Economizer Control Type", // Economizer should open for this one, so OA flow should be > min OA
         "    ModulateFlow,            !- Economizer Control Action Type",
         "    ,                        !- Economizer Maximum Limit Dry-Bulb Temperature {C}",
         "    ,                        !- Economizer Maximum Limit Enthalpy {J/kg}",
         "    ,                        !- Economizer Maximum Limit Dewpoint Temperature {C}",
         "    ,                        !- Electronic Enthalpy Limit Curve Name",
         "    ,                        !- Economizer Minimum Limit Dry-Bulb Temperature {C}",
         "    NoLockout,               !- Lockout Type",
         "    ProportionalMinimum,     !- Minimum Limit Type",
         "    ,                        !- Minimum Outdoor Air Schedule Name",
         "    ,                        !- Minimum Fraction of Outdoor Air Schedule Name",
         "    ,                        !- Maximum Fraction of Outdoor Air Schedule Name",
         "    ,                        !- Mechanical Ventilation Controller Name",
         "    ,                        !- Time of Day Economizer Control Schedule Name",
         "    No,                      !- High Humidity Control",
         "    ,                        !- Humidistat Control Zone Name",
         "    ,                        !- High Humidity Outdoor Air Flow Ratio",
         "    Yes,                     !- Control High Indoor Humidity Based on Outdoor Humidity Ratio",
         "    BypassWhenOAFlowGreaterThanMinimum;  !- Heat Recovery Bypass Control Type", // HX bypass should be true because economizer has opened up

         "  OutdoorAir:Mixer,",
         "    OA Mixer 3,                !- Name",
         "    Mixed Air Node 3,          !- Mixed Air Node Name",
         "    Outside Air Inlet Node 3, !- Outdoor Air Stream Node Name",
         "    Relief Air Outlet Node 3,  !- Relief Air Stream Node Name",
         "    VAV Sys 3 Inlet Node;     !- Return Air Stream Node Name",

         " AirLoopHVAC:ControllerList,",
         "    OA Sys 3 controller,     !- Name",
         "    Controller:OutdoorAir,   !- Controller 1 Object Type",
         "    OA Controller 3;         !- Controller 1 Name",

         " AirLoopHVAC:OutdoorAirSystem:EquipmentList,",
         "    OA Sys 3 Equipment list, !- Name",
         "    OutdoorAir:Mixer,        !- Component 1 Object Type",
         "    OA Mixer 3;                !- Component 1 Name",

         " AirLoopHVAC:OutdoorAirSystem,",
         "    OA Sys 3, !- Name",
         "    OA Sys 3 controller,     !- Controller List Name",
         "    OA Sys 3 Equipment list; !- Outdoor Air Equipment List Name",

         "  Controller:OutdoorAir,",
         "    OA Controller 4,         !- Name",
         "    Relief Air Outlet Node 4, !- Relief Air Outlet Node Name",
         "    VAV Sys 4 Inlet Node,    !- Return Air Node Name",
         "    Mixed Air Node 4,        !- Mixed Air Node Name",
         "    Outside Air Inlet Node 4, !- Actuator Node Name",
         "    0.2,                !- Minimum Outdoor Air Flow Rate {m3/s}",
         "    1.0,                !- Maximum Outdoor Air Flow Rate {m3/s}",
         "    DifferentialDryBulb,     !- Economizer Control Type", // Economizer should not open for this one - lowered the outdoor dry bulb temp for
                                                                    // Case 4
         "    ModulateFlow,            !- Economizer Control Action Type",
         "    ,                        !- Economizer Maximum Limit Dry-Bulb Temperature {C}",
         "    ,                        !- Economizer Maximum Limit Enthalpy {J/kg}",
         "    ,                        !- Economizer Maximum Limit Dewpoint Temperature {C}",
         "    ,                        !- Electronic Enthalpy Limit Curve Name",
         "    ,                        !- Economizer Minimum Limit Dry-Bulb Temperature {C}",
         "    NoLockout,               !- Lockout Type",
         "    ProportionalMinimum,     !- Minimum Limit Type",
         "    ,                        !- Minimum Outdoor Air Schedule Name",
         "    ,                        !- Minimum Fraction of Outdoor Air Schedule Name",
         "    ,                        !- Maximum Fraction of Outdoor Air Schedule Name",
         "    ,                        !- Mechanical Ventilation Controller Name",
         "    ,                        !- Time of Day Economizer Control Schedule Name",
         "    No,                      !- High Humidity Control",
         "    ,                        !- Humidistat Control Zone Name",
         "    ,                        !- High Humidity Outdoor Air Flow Ratio",
         "    Yes,                     !- Control High Indoor Humidity Based on Outdoor Humidity Ratio",
         "    BypassWhenOAFlowGreaterThanMinimum;  !- Heat Recovery Bypass Control Type", // HX bypass should be true because economizer has opened up

         "  OutdoorAir:Mixer,",
         "    OA Mixer 4,                !- Name",
         "    Mixed Air Node 4,          !- Mixed Air Node Name",
         "    Outside Air Inlet Node 4, !- Outdoor Air Stream Node Name",
         "    Relief Air Outlet Node 4,  !- Relief Air Stream Node Name",
         "    VAV Sys 4 Inlet Node;     !- Return Air Stream Node Name",

         " AirLoopHVAC:ControllerList,",
         "    OA Sys 4 controller,     !- Name",
         "    Controller:OutdoorAir,   !- Controller 1 Object Type",
         "    OA Controller 4;         !- Controller 1 Name",

         " AirLoopHVAC:OutdoorAirSystem:EquipmentList,",
         "    OA Sys 4 Equipment list, !- Name",
         "    OutdoorAir:Mixer,        !- Component 1 Object Type",
         "    OA Mixer 4;                !- Component 1 Name",

         " AirLoopHVAC:OutdoorAirSystem,",
         "    OA Sys 4, !- Name",
         "    OA Sys 4 controller,     !- Controller List Name",
         "    OA Sys 4 Equipment list; !- Outdoor Air Equipment List Name",

         "  Controller:OutdoorAir,",
         "    OA Controller 5,         !- Name",
         "    Relief Air Outlet Node 5, !- Relief Air Outlet Node Name",
         "    VAV Sys 5 Inlet Node,    !- Return Air Node Name",
         "    Mixed Air Node 5,        !- Mixed Air Node Name",
         "    Outside Air Inlet Node 5, !- Actuator Node Name",
         "    0.2,                !- Minimum Outdoor Air Flow Rate {m3/s}",
         "    1.0,                !- Maximum Outdoor Air Flow Rate {m3/s}",
         "    DifferentialDryBulb,     !- Economizer Control Type", // Economizer should not open for this one - lowered the outdoor dry bulb temp for
                                                                    // Case 4
         "    ModulateFlow,            !- Economizer Control Action Type",
         "    ,                        !- Economizer Maximum Limit Dry-Bulb Temperature {C}",
         "    ,                        !- Economizer Maximum Limit Enthalpy {J/kg}",
         "    ,                        !- Economizer Maximum Limit Dewpoint Temperature {C}",
         "    ,                        !- Electronic Enthalpy Limit Curve Name",
         "    ,                        !- Economizer Minimum Limit Dry-Bulb Temperature {C}",
         "    NoLockout,               !- Lockout Type",
         "    ProportionalMinimum,     !- Minimum Limit Type",
         "    ,                        !- Minimum Outdoor Air Schedule Name",
         "    ,                        !- Minimum Fraction of Outdoor Air Schedule Name",
         "    ,                        !- Maximum Fraction of Outdoor Air Schedule Name",
         "    ,                        !- Mechanical Ventilation Controller Name",
         "    ,                        !- Time of Day Economizer Control Schedule Name",
         "    No,                      !- High Humidity Control",
         "    ,                        !- Humidistat Control Zone Name",
         "    ,                        !- High Humidity Outdoor Air Flow Ratio",
         "    No,                      !- Control High Indoor Humidity Based on Outdoor Humidity Ratio",
         "    BypassWhenOAFlowGreaterThanMinimum;  !- Heat Recovery Bypass Control Type", // HX bypass should be true because economizer has opened up

         "  OutdoorAir:Mixer,",
         "    OA Mixer 5,                !- Name",
         "    Mixed Air Node 5,          !- Mixed Air Node Name",
         "    OA Sys 5 HC Outlet Node,   !- Outdoor Air Stream Node Name",
         "    Relief Air Outlet Node 5,  !- Relief Air Stream Node Name",
         "    VAV Sys 5 Inlet Node;     !- Return Air Stream Node Name",

         " AirLoopHVAC:ControllerList,",
         "    OA Sys 5 controller,     !- Name",
         "    Controller:OutdoorAir,   !- Controller 1 Object Type",
         "    OA Controller 5;         !- Controller 1 Name",

         " AirLoopHVAC:OutdoorAirSystem:EquipmentList,",
         "    OA Sys 5 Equipment list, !- Name",
         "    Coil:Heating:Electric,    !- Component 1 Object Type",
         "    OA Sys 5 Heating Coil,    !- Component 1 Name",
         "    OutdoorAir:Mixer,         !- Component 2 Object Type",
         "    OA Mixer 5;               !- Component 2 Name",

         " AirLoopHVAC:OutdoorAirSystem,",
         "    OA Sys 5, !- Name",
         "    OA Sys 5 controller,      !- Controller List Name",
         "    OA Sys 5 Equipment list;  !- Outdoor Air Equipment List Name",

         " Coil:Heating:Electric,",
         "    OA Sys 5 Heating Coil,    !- Name",
         "    ,                         !- Availability Schedule Name",
         "    1,                        !- Efficiency",
         "    2500,                     !- Nominal Capacity{ W }",
         "    Outside Air Inlet Node 5, !- Air Inlet Node Name",
         "    OA Sys 5 HC Outlet Node,  !- Air Outlet Node Name",
         "    OA Sys 5 HC Outlet Node;  !- Temperature Setpoint Node Name"

        });

    ASSERT_TRUE(process_idf(idf_objects));
    GetOAControllerInputs();
    EXPECT_EQ(2, OAController(1).OANode);
    EXPECT_TRUE(OutAirNodeManager::CheckOutAirNodeNumber(OAController(1).OANode));

    EXPECT_EQ(6, OAController(2).OANode);
    EXPECT_FALSE(OutAirNodeManager::CheckOutAirNodeNumber(OAController(2).OANode));

    int OAControllerNum;
    int AirLoopNum;

    DataHVACGlobals::NumPrimaryAirSys = 5; // will be reset in DataHVACGlobals::clear_state(); in EnergyPlusFixture
    AirLoopControlInfo.allocate(5);        // will be deallocated by MixedAir::clear_state(); in EnergyPlusFixture
    AirLoopFlow.allocate(5);               // will be deallocated by MixedAir::clear_state(); in EnergyPlusFixture
    PrimaryAirSystem.allocate(5);          // will be deallocated by DataAirSystems::clear_state(); in EnergyPlusFixture
    Node.allocate(21);                     // will be deallocated by DataLoopNode::clear_state(); in EnergyPlusFixture

    StdBaroPress = StdPressureSeaLevel;
    StdRhoAir = Psychrometrics::PsyRhoAirFnPbTdbW(StdBaroPress, 20.0, 0.0);

    // Initialize common AirLoop data
    for (AirLoopNum = 1; AirLoopNum <= 5; ++AirLoopNum) {
        AirLoopControlInfo(AirLoopNum).OASysNum = AirLoopNum;
        AirLoopControlInfo(AirLoopNum).EconoLockout = false;
        AirLoopControlInfo(AirLoopNum).NightVent = false;
        AirLoopControlInfo(AirLoopNum).FanOpMode = DataHVACGlobals::ContFanCycCoil;
        AirLoopControlInfo(AirLoopNum).LoopFlowRateSet = false;
        AirLoopControlInfo(AirLoopNum).CheckHeatRecoveryBypassStatus = true;
        AirLoopControlInfo(AirLoopNum).OASysComponentsSimulated = true;
        AirLoopControlInfo(AirLoopNum).EconomizerFlowLocked = false;
        AirLoopControlInfo(AirLoopNum).HeatRecoveryBypass = false;
        AirLoopControlInfo(AirLoopNum).HeatRecoveryResimFlag = false; // Need this to avoid resetting hxbypass, saying this has already been simulated
        AirLoopFlow(AirLoopNum).DesSupply = 1.0 * StdRhoAir;
        PrimaryAirSystem(AirLoopNum).NumBranches = 1;
        PrimaryAirSystem(AirLoopNum).Branch.allocate(1);
        PrimaryAirSystem(AirLoopNum).Branch(1).TotalComponents = 1;
        PrimaryAirSystem(AirLoopNum).Branch(1).Comp.allocate(1);
    }
    PrimaryAirSystem(1).Branch(1).Comp(1).Name = "OA Sys 1";
    PrimaryAirSystem(1).Branch(1).Comp(1).TypeOf = "AirLoopHVAC:OutdoorAirSystem";
    PrimaryAirSystem(2).Branch(1).Comp(1).Name = "OA Sys 2";
    PrimaryAirSystem(2).Branch(1).Comp(1).TypeOf = "AirLoopHVAC:OutdoorAirSystem";
    PrimaryAirSystem(3).Branch(1).Comp(1).Name = "OA Sys 3";
    PrimaryAirSystem(3).Branch(1).Comp(1).TypeOf = "AirLoopHVAC:OutdoorAirSystem";
    PrimaryAirSystem(4).Branch(1).Comp(1).Name = "OA Sys 4";
    PrimaryAirSystem(4).Branch(1).Comp(1).TypeOf = "AirLoopHVAC:OutdoorAirSystem";

    // Initialize common OA controller and node data
    for (OAControllerNum = 1; OAControllerNum <= 5; ++OAControllerNum) {
        OAController(OAControllerNum).MinOAMassFlowRate = OAController(OAControllerNum).MinOA * StdRhoAir;
        OAController(OAControllerNum).MaxOAMassFlowRate = OAController(OAControllerNum).MaxOA * StdRhoAir;
        if (OAControllerNum == 5) {
            OAController(OAControllerNum).InletNode = 18;
        } else {
            OAController(OAControllerNum).InletNode = OAController(OAControllerNum).OANode;
        }
        OAController(OAControllerNum).RetTemp = 24.0;
        OAController(OAControllerNum).InletTemp = 20.0; // This is the same as the outdoor air dry bulb for these tests
        OAController(OAControllerNum).OATemp = 20.0;
        OAController(OAControllerNum).MixSetTemp = 22.0;
        OAController(OAControllerNum).ExhMassFlow = 0.0;
        // OAController( OAControllerNum ).InletEnth = needs to be initialized if an enthalpy economizer is tested
        // OAController( OAControllerNum ).RetEnth = needs to be initialized if an enthalpy economizer is tested
        OAController(OAControllerNum).MixMassFlow = 0.5;                                    // Note this is 50% of design flow set above
        Node(OAControllerNum * 4).MassFlowRate = OAController(OAControllerNum).MixMassFlow; // Return air nodes
        Node(OAControllerNum + ((OAControllerNum - 1) * 3)).MassFlowRateMaxAvail = OAController(OAControllerNum).MixMassFlow; // Mixed air nodes
        Node(OAControllerNum * 4).Temp = OAController(OAControllerNum).RetTemp;                                               // Return air nodes
        Node(OAControllerNum * 4).Enthalpy = Psychrometrics::PsyHFnTdbW(OAController(OAControllerNum).RetTemp, 0.0); // Return air nodes, dry air
        Node(OAControllerNum * 4 - 3).TempSetPoint = OAController(OAControllerNum).MixSetTemp;                       // Mixed air nodes
        if (OAControllerNum == 5) Node(18).TempSetPoint = OAController(OAControllerNum).MixSetTemp + 1.0;            // Mixed air nodes
        Node(OAControllerNum * 4 - 2).Temp = OAController(OAControllerNum).OATemp; // OA inlet (actuated) air nodes, dry air
        Node(OAControllerNum * 4 - 2).Enthalpy = Psychrometrics::PsyHFnTdbW(OAController(OAControllerNum).InletTemp, 0.0);
        ; // OA inlet (actuated) air nodes, dry air
    }

    Real64 expectedOAflow(0.0);
    Real64 expectedMinOAflow(0.0);

    // Case 1 - economizer active, NoLockout, BypassWhenWithinEconomizerLimits
    // economizer should open to meet the mixed air setpoint assuming dry air to make it simple, HXbypass true
    //   OAFlow = MixFlow*(MixTemp - RetTemp)/(InletTemp - RetTemp)
    AirLoopNum = 1;
    OAControllerNum = 1;
    AirLoopControlInfo(AirLoopNum).HeatingActiveFlag = true;
    // setup OA system and initialize nodes
    //		ManageOutsideAirSystem( "OA Sys 1", true, AirLoopNum, OAControllerNum );
    OAController(OAControllerNum).CalcOAController(AirLoopNum, true);

    expectedMinOAflow =
        0.2 * StdRhoAir * OAController(OAControllerNum).MixMassFlow / AirLoopFlow(AirLoopNum).DesSupply; // For Proportional minimum input
    expectedOAflow = OAController(OAControllerNum).MixMassFlow * (OAController(OAControllerNum).MixSetTemp - OAController(OAControllerNum).RetTemp) /
                     (OAController(OAControllerNum).InletTemp - OAController(OAControllerNum).RetTemp);
    EXPECT_NEAR(expectedOAflow, OAController(OAControllerNum).OAMassFlow, 0.00001);
    EXPECT_NEAR(OAController(OAControllerNum).OAMassFlow / OAController(OAControllerNum).MixMassFlow, AirLoopFlow(AirLoopNum).OAFrac, 0.00001);
    EXPECT_EQ(expectedMinOAflow, AirLoopFlow(AirLoopNum).MinOutAir);
    EXPECT_EQ(expectedMinOAflow / OAController(OAControllerNum).MixMassFlow, AirLoopFlow(AirLoopNum).OAMinFrac);
    EXPECT_TRUE(AirLoopControlInfo(AirLoopNum).HeatRecoveryBypass);
    EXPECT_EQ(1, OAController(OAControllerNum).HeatRecoveryBypassStatus);

    // Case 2 - economizer active, LockoutWithHeating, BypassWhenWithinEconomizerLimits
    // economizer should not be locked out, OA flow at minimum, HXbypass true
    AirLoopNum = 2;
    OAControllerNum = 2;
    AirLoopControlInfo(AirLoopNum).HeatingActiveFlag = true;
    OAController(OAControllerNum).InletTemp = 0.0; // This is the same as the outdoor air dry bulb for these tests
    OAController(OAControllerNum).OATemp = 0.0;
    Node(OAControllerNum * 4 - 2).Temp = OAController(OAControllerNum).OATemp; // OA inlet (actuated) air nodes, dry air

    OAController(OAControllerNum).CalcOAController(AirLoopNum, true);

    expectedMinOAflow =
        0.2 * StdRhoAir * OAController(OAControllerNum).MixMassFlow / AirLoopFlow(AirLoopNum).DesSupply; // For Proportional minimum input
    expectedOAflow = expectedMinOAflow;
    EXPECT_NEAR(expectedOAflow, OAController(OAControllerNum).OAMassFlow, 0.00001);
    EXPECT_NEAR(OAController(OAControllerNum).OAMassFlow / OAController(OAControllerNum).MixMassFlow, AirLoopFlow(AirLoopNum).OAFrac, 0.00001);
    EXPECT_EQ(expectedMinOAflow, AirLoopFlow(AirLoopNum).MinOutAir);
    EXPECT_EQ(expectedMinOAflow / OAController(OAControllerNum).MixMassFlow, AirLoopFlow(AirLoopNum).OAMinFrac);
    EXPECT_FALSE(AirLoopControlInfo(AirLoopNum).HeatRecoveryBypass);
    EXPECT_EQ(0, OAController(OAControllerNum).HeatRecoveryBypassStatus);

    // Case 3 - economizer active, NoLockout, BypassWhenOAFlowGreaterThanMinimum (should be same result as Case 1)
    // economizer should open to meet the mixed air setpoint assuming dry air to make it simple, HXbypass true
    //   OAFlow = MixFlow*(MixTemp - RetTemp)/(InletTemp - RetTemp)
    AirLoopNum = 3;
    OAControllerNum = 3;
    AirLoopControlInfo(AirLoopNum).HeatingActiveFlag = true;
    OAController(OAControllerNum).InletTemp = 20.0; // This is the same as the outdoor air dry bulb for these tests
    OAController(OAControllerNum).OATemp = 20.0;
    Node(OAControllerNum * 4 - 2).Temp = OAController(OAControllerNum).OATemp; // OA inlet (actuated) air nodes, dry air
    OAController(OAControllerNum).CalcOAController(AirLoopNum, true);

    expectedMinOAflow =
        0.2 * StdRhoAir * OAController(OAControllerNum).MixMassFlow / AirLoopFlow(AirLoopNum).DesSupply; // For Proportional minimum input
    expectedOAflow = OAController(OAControllerNum).MixMassFlow * (OAController(OAControllerNum).MixSetTemp - OAController(OAControllerNum).RetTemp) /
                     (OAController(OAControllerNum).InletTemp - OAController(OAControllerNum).RetTemp);
    EXPECT_NEAR(expectedOAflow, OAController(OAControllerNum).OAMassFlow, 0.00001);
    EXPECT_NEAR(OAController(OAControllerNum).OAMassFlow / OAController(OAControllerNum).MixMassFlow, AirLoopFlow(AirLoopNum).OAFrac, 0.00001);
    EXPECT_EQ(expectedMinOAflow, AirLoopFlow(AirLoopNum).MinOutAir);
    EXPECT_EQ(expectedMinOAflow / OAController(OAControllerNum).MixMassFlow, AirLoopFlow(AirLoopNum).OAMinFrac);
    EXPECT_TRUE(AirLoopControlInfo(AirLoopNum).HeatRecoveryBypass);
    EXPECT_EQ(1, OAController(OAControllerNum).HeatRecoveryBypassStatus);

    // Case 4 - economizer active, NoLockout, BypassWhenOAFlowGreaterThanMinimum
    // economizer should be at minimum due to cold outdoor temp, OA flow at minimum, HXbypass false
    AirLoopNum = 4;
    OAControllerNum = 4;
    AirLoopControlInfo(AirLoopNum).HeatingActiveFlag = true;
    OAController(OAControllerNum).InletTemp = 0.0; // This is the same as the outdoor air dry bulb for these tests
    OAController(OAControllerNum).OATemp = 0.0;
    Node(OAControllerNum * 4 - 2).Temp = OAController(OAControllerNum).OATemp; // OA inlet (actuated) air nodes, dry air

    OAController(OAControllerNum).CalcOAController(AirLoopNum, true);

    expectedMinOAflow =
        0.2 * StdRhoAir * OAController(OAControllerNum).MixMassFlow / AirLoopFlow(AirLoopNum).DesSupply; // For Proportional minimum input
    expectedOAflow = expectedMinOAflow;
    EXPECT_NEAR(expectedOAflow, OAController(OAControllerNum).OAMassFlow, 0.00001);
    EXPECT_NEAR(OAController(OAControllerNum).OAMassFlow / OAController(OAControllerNum).MixMassFlow, AirLoopFlow(AirLoopNum).OAFrac, 0.00001);
    EXPECT_EQ(expectedMinOAflow, AirLoopFlow(AirLoopNum).MinOutAir);
    EXPECT_EQ(expectedMinOAflow / OAController(OAControllerNum).MixMassFlow, AirLoopFlow(AirLoopNum).OAMinFrac);
    EXPECT_FALSE(AirLoopControlInfo(AirLoopNum).HeatRecoveryBypass);
    EXPECT_EQ(0, OAController(OAControllerNum).HeatRecoveryBypassStatus);

    // Case 5 - heating coil in outside air stream upstream of mixer #5697
    // economizer active, NoLockout, BypassWhenOAFlowGreaterThanMinimum
    // economizer should open to meet mixed air set point temperature, HXbypass true
    AirLoopNum = 5;
    OAControllerNum = 5;
    AirLoopControlInfo(AirLoopNum).HeatingActiveFlag = false;
    OAController(OAControllerNum).InletTemp = 20.0; // This is the same as the outdoor air dry bulb for these tests
    OAController(OAControllerNum).OATemp = 20.0;
    Node(OAControllerNum * 4 - 3).MassFlowRate = OAController(OAControllerNum).MixMassFlow; // set the mixed air node mass flow rate
    Node(OAControllerNum * 4 - 2).Temp = OAController(OAControllerNum).OATemp;              // OA inlet (actuated) air nodes, dry air

    OAController(OAControllerNum).CalcOAController(AirLoopNum, true);

    expectedMinOAflow =
        0.2 * StdRhoAir * OAController(OAControllerNum).MixMassFlow / AirLoopFlow(AirLoopNum).DesSupply; // For Proportional minimum input
    expectedOAflow = expectedMinOAflow;
    EXPECT_GT(OAController(OAControllerNum).OAMassFlow, expectedOAflow);
    EXPECT_NEAR(OAController(OAControllerNum).OAMassFlow / OAController(OAControllerNum).MixMassFlow, AirLoopFlow(AirLoopNum).OAFrac, 0.00001);
    EXPECT_NEAR(OAController(OAControllerNum).OAMassFlow, 0.145329, 0.000001);
    EXPECT_EQ(expectedMinOAflow, AirLoopFlow(AirLoopNum).MinOutAir);
    EXPECT_EQ(expectedMinOAflow / OAController(OAControllerNum).MixMassFlow, AirLoopFlow(AirLoopNum).OAMinFrac);
    EXPECT_FALSE(AirLoopControlInfo(AirLoopNum).HeatRecoveryBypass);
    EXPECT_EQ(0, OAController(OAControllerNum).HeatRecoveryBypassStatus);
}

TEST_F(EnergyPlusFixture, CO2ControlDesignOccupancyTest)
{
    Contaminant.CO2Simulation = true;
    Contaminant.CO2OutdoorSchedPtr = 1;

    std::string const idf_objects = delimited_string({
        "Version,8.3;",
        "  OutdoorAir:Node,",
        "    Outside Air Inlet Node; !- Name",
        "  Schedule:Constant,",
        "    VentSchedule, !- Name",
        "     , !- Schedule Type Limits Name",
        "     1; !- Hourly value",
        "  Schedule:Constant,",
        "    ZoneADEffSch, !- Name",
        "     , !- Schedule Type Limits Name",
        "     1; !- Hourly value",
        "  Schedule:Constant,",
        "    OAFractionSched, !- Name",
        "     , !- Schedule Type Limits Name",
        "     1; !- Hourly value",
        "  Schedule:Constant,",
        "    CO2AvailSchedule, !- Name",
        "     , !- Schedule Type Limits Name",
        "     1.0; !- Hourly value",
        "  Controller:OutdoorAir,",
        "    OA Controller 1, !- Name",
        "    Relief Air Outlet Node, !- Relief Air Outlet Node Name",
        "    Outdoor Air Mixer Inlet Node, !- Return Air Node Name",
        "    Mixed Air Node, !- Mixed Air Node Name",
        "    Outside Air Inlet Node, !- Actuator Node Name",
        "    0.0, !- Minimum Outdoor Air Flow Rate{ m3 / s }",
        "    1.7, !- Maximum Outdoor Air Flow Rate{ m3 / s }",
        "    NoEconomizer, !- Economizer Control Type",
        "    ModulateFlow, !- Economizer Control Action Type",
        "    , !- Economizer Maximum Limit Dry - Bulb Temperature{ C }",
        "    , !- Economizer Maximum Limit Enthalpy{ J / kg }",
        "    , !- Economizer Maximum Limit Dewpoint Temperature{ C }",
        "    , !- Electronic Enthalpy Limit Curve Name",
        "    , !- Economizer Minimum Limit Dry - Bulb Temperature{ C }",
        "    NoLockout, !- Lockout Type",
        "    FixedMinimum, !- Minimum Limit Type",
        "    OAFractionSched, !- Minimum Outdoor Air Schedule Name",
        "    , !- Minimum Fraction of Outdoor Air Schedule Name",
        "    , !- Maximum Fraction of Outdoor Air Schedule Name",
        "    DCVObject;               !- Mechanical Ventilation Controller Name",
        "  Controller:MechanicalVentilation,",
        "    DCVObject, !- Name",
        "    VentSchedule, !- Availability Schedule Name",
        "    Yes, !- Demand Controlled Ventilation",
        "    ProportionalControlBasedonDesignOccupancy, !- System Outdoor Air Method",
        "     , !- Zone Maximum Outdoor Air Fraction{ dimensionless }",
        "    West Zone, !- Zone 1 Name",
        "    CM DSOA West Zone, !- Design Specification Outdoor Air Object Name 1",
        "    CM DSZAD West Zone; !- Design Specification Zone Air Distribution Object Name 1",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    AirLoopControlInfo.allocate(1);
    AirLoopControlInfo(1).LoopFlowRateSet = true;
    OARequirements.allocate(1);
    OARequirements(1).Name = "CM DSOA WEST ZONE";
    OARequirements(1).OAFlowMethod = OAFlowSum;
    OARequirements(1).OAFlowPerPerson = 0.003149;
    OARequirements(1).OAFlowPerArea = 0.000407;

    ZoneAirDistribution.allocate(1);
    ZoneAirDistribution(1).Name = "CM DSZAD WEST ZONE";
    ZoneAirDistribution(1).ZoneADEffSchPtr = 4;

    Zone.allocate(1);
    Zone(1).Name = "WEST ZONE";
    Zone(1).FloorArea = 10.0;
    Zone(1).ZoneContamControllerSchedIndex = 4;

    AirLoopFlow.allocate(1);
    AirLoopFlow(1).OAFrac = 0.01;    // DataAirLoop variable (AirloopHVAC)
    AirLoopFlow(1).OAMinFrac = 0.01; // DataAirLoop variable (AirloopHVAC)

    GetOAControllerInputs();

    EXPECT_EQ(7, VentilationMechanical(1).SystemOAMethod);
    EXPECT_TRUE(OutAirNodeManager::CheckOutAirNodeNumber(OAController(1).OANode));
    EXPECT_NEAR(0.00314899, VentilationMechanical(1).ZoneOAPeopleRate(1), 0.00001);
    EXPECT_NEAR(0.000407, VentilationMechanical(1).ZoneOAAreaRate(1), 0.00001);

    StdRhoAir = 1.2;
    OAController(1).MixMassFlow = 1.7 * StdRhoAir;
    OAController(1).MaxOAMassFlowRate = 1.7 * StdRhoAir;
    AirLoopFlow(1).DesSupply = 1.7;
    VentilationMechanical(1).SchPtr = 1;
    Schedule(1).CurrentValue = 1.0;

    VentilationMechanical(1).ZoneADEffSchPtr(1) = 2;
    Schedule(2).CurrentValue = 1.0;
    TotPeople = 1;
    People.allocate(1);
    People(1).Name = "WestPeople";
    People(1).ZonePtr = 1;
    People(1).NumberOfPeople = 3;
    Zone(1).TotOccupants = 3;
    Schedule(4).CurrentValue = 1.0;
    ZoneCO2GainFromPeople.allocate(1);
    ZoneCO2GainFromPeople(1) = 3.82E-8;
    OutdoorCO2 = 400;
    ZoneAirCO2.allocate(1);
    ZoneAirCO2(1) = 600.0;
    ZoneEquipConfig.allocate(1);
    ZoneEquipConfig(1).NumInletNodes = 1;
    ZoneEquipConfig(1).AirDistUnitCool.allocate(1);
    ZoneEquipConfig(1).AirDistUnitCool(1).InNode = 10;
    ZoneEquipConfig(1).InletNode.allocate(1);
    ZoneEquipConfig(1).InletNode(1) = 10;
    Node.allocate(10);
    Node(10).Temp = 13.00;
    Node(10).HumRat = 0.008;
    Node(10).MassFlowRate = 1.7 * StdRhoAir;
    OutBaroPress = 101325;
    ZoneSysEnergyDemand.allocate(1);

    OAController(1).CalcOAController(1, true);

    EXPECT_NEAR(0.0194359, OAController(1).OAMassFlow, 0.00001);
    EXPECT_NEAR(0.009527, OAController(1).MinOAFracLimit, 0.00001);

    OARequirements(1).OAFlowMethod = 9;
    VentilationMechanical(1).ZoneOAFlowMethod(1) = OARequirements(1).OAFlowMethod;
    DataAirLoop::NumOASystems = 1;

    OutsideAirSys.allocate(1);
    OutsideAirSys(1).Name = "AIRLOOP OASYSTEM";
    OutsideAirSys(1).NumControllers = 1;
    OutsideAirSys(1).ControllerName.allocate(1);
    OutsideAirSys(1).ControllerName(1) = "OA CONTROLLER 1";
    OutsideAirSys(1).ComponentType.allocate(1);
    OutsideAirSys(1).ComponentType(1) = "OutdoorAir:Mixer";
    OutsideAirSys(1).ComponentName.allocate(1);
    OutsideAirSys(1).ComponentName(1) = "OAMixer";
    OAMixer.allocate(1);
    OAMixer(1).Name = "OAMixer";
    OAMixer(1).InletNode = 2;

    DataHVACGlobals::NumPrimaryAirSys = 1;
    PrimaryAirSystem.allocate(1);
    PrimaryAirSystem(1).Name = "PrimaryAirLoop";
    PrimaryAirSystem(1).NumBranches = 1;
    PrimaryAirSystem(1).Branch.allocate(1);
    PrimaryAirSystem(1).Branch(1).TotalComponents = 1;
    PrimaryAirSystem(1).Branch(1).Comp.allocate(1);
    PrimaryAirSystem(1).Branch(1).Comp(1).Name = OutsideAirSys(1).Name;
    PrimaryAirSystem(1).Branch(1).Comp(1).TypeOf = "AirLoopHVAC:OutdoorAirSystem";

    AirLoopZoneInfo.allocate(1);
    AirLoopZoneInfo(1).NumZones = 1;
    AirLoopZoneInfo(1).ActualZoneNumber.allocate(1);
    AirLoopZoneInfo(1).ActualZoneNumber(1) = 1;

    InitOAController(1, true, 1);
    EXPECT_EQ("ProportionalControlBasedOnDesignOccupancy", DataSizing::cOAFlowMethodTypes(VentilationMechanical(1).ZoneOAFlowMethod(1)));

    OutsideAirSys.deallocate();
    OAMixer.deallocate();
    AirLoopZoneInfo.deallocate();
    PrimaryAirSystem.deallocate();
    ZoneAirCO2.deallocate();
    ZoneCO2GainFromPeople.deallocate();
}

TEST_F(EnergyPlusFixture, MissingDesignOccupancyTest)
{

    bool ErrorsFound(false);

    std::string const idf_objects = delimited_string({
        "Version,8.3;",

        "Zone,",
        "  WEST ZONE,              !- Name",
        "  0,                      !- Direction of Relative North{ deg }",
        "  0,                      !- X Origin{ m }",
        "  0,                      !- Y Origin{ m }",
        "  0,                      !- Z Origin{ m }",
        "  1,                      !- Type",
        "  1,                      !- Multiplier",
        "  autocalculate,          !- Ceiling Height{ m }",
        "  autocalculate;          !- Volume{ m3 }",

        "Sizing:Zone,",
        "  WEST ZONE,              !- Zone or ZoneList Name",
        "  SupplyAirTemperature,   !- Zone Cooling Design Supply Air Temperature Input Method",
        "  14,                     !- Zone Cooling Design Supply Air Temperature{ C }",
        "  ,                       !- Zone Cooling Design Supply Air Temperature Difference{ deltaC }",
        "  SupplyAirTemperature,   !- Zone Heating Design Supply Air Temperature Input Method",
        "  40,                     !- Zone Heating Design Supply Air Temperature{ C }",
        "  ,                       !- Zone Heating Design Supply Air Temperature Difference{ deltaC }",
        "  0.0085,                 !- Zone Cooling Design Supply Air Humidity Ratio{ kgWater / kgDryAir }",
        "  0.008,                  !- Zone Heating Design Supply Air Humidity Ratio{ kgWater / kgDryAir }",
        "  ,                       !- Design Specification Outdoor Air Object Name",
        "  ,                       !- Zone Heating Sizing Factor",
        "  ,                       !- Zone Cooling Sizing Factor",
        "  DesignDay,              !- Cooling Design Air Flow Method",
        "  0,                      !- Cooling Design Air Flow Rate{ m3 / s }",
        "  0.000762,               !- Cooling Minimum Air Flow per Zone Floor Area{ m3 / s - m2 }",
        "  0,                      !- Cooling Minimum Air Flow{ m3 / s }",
        "  0,                      !- Cooling Minimum Air Flow Fraction",
        "  DesignDay,              !- Heating Design Air Flow Method",
        "  0,                      !- Heating Design Air Flow Rate{ m3 / s }",
        "  0.002032,               !- Heating Maximum Air Flow per Zone Floor Area{ m3 / s - m2 }",
        "  0.1415762,              !- Heating Maximum Air Flow{ m3 / s }",
        "  0.3,                    !- Heating Maximum Air Flow Fraction",
        "  West Zone Design Spec Zone Air Dist; !- Design Specification Zone Air Distribution Object Name",

        "  OutdoorAir:Node,",
        "    Outside Air Inlet Node; !- Name",
        "  Schedule:Constant,",
        "    VentSchedule,           !- Name",
        "     ,                      !- Schedule Type Limits Name",
        "     1;                     !- Hourly value",
        "  Schedule:Constant,",
        "    ZoneADEffSch,           !- Name",
        "     ,                      !- Schedule Type Limits Name",
        "     1;                     !- Hourly value",
        "  Schedule:Constant,",
        "    OAFractionSched,        !- Name",
        "     ,                      !- Schedule Type Limits Name",
        "     1;                     !- Hourly value",
        "  Schedule:Constant,",
        "    CO2AvailSchedule,       !- Name",
        "     ,                      !- Schedule Type Limits Name",
        "     1.0;                   !- Hourly value",

        "  Controller:OutdoorAir,",
        "    OA Controller 1,        !- Name",
        "    Relief Air Outlet Node, !- Relief Air Outlet Node Name",
        "    Outdoor Air Mixer Inlet Node, !- Return Air Node Name",
        "    Mixed Air Node,         !- Mixed Air Node Name",
        "    Outside Air Inlet Node, !- Actuator Node Name",
        "    0.0,                    !- Minimum Outdoor Air Flow Rate{ m3 / s }",
        "    1.7,                    !- Maximum Outdoor Air Flow Rate{ m3 / s }",
        "    NoEconomizer,           !- Economizer Control Type",
        "    ModulateFlow,           !- Economizer Control Action Type",
        "    ,                       !- Economizer Maximum Limit Dry - Bulb Temperature{ C }",
        "    ,                       !- Economizer Maximum Limit Enthalpy{ J / kg }",
        "    ,                       !- Economizer Maximum Limit Dewpoint Temperature{ C }",
        "    ,                       !- Electronic Enthalpy Limit Curve Name",
        "    ,                       !- Economizer Minimum Limit Dry - Bulb Temperature{ C }",
        "    NoLockout,              !- Lockout Type",
        "    FixedMinimum,           !- Minimum Limit Type",
        "    OAFractionSched,        !- Minimum Outdoor Air Schedule Name",
        "    ,                       !- Minimum Fraction of Outdoor Air Schedule Name",
        "    ,                       !- Maximum Fraction of Outdoor Air Schedule Name",
        "    DCVObject;              !- Mechanical Ventilation Controller Name",

        "  Controller:MechanicalVentilation,",
        "    DCVObject, !- Name",
        "    VentSchedule, !- Availability Schedule Name",
        "    Yes, !- Demand Controlled Ventilation",
        "    ZoneSum, !- System Outdoor Air Method",
        "     , !- Zone Maximum Outdoor Air Fraction{ dimensionless }",
        "    WEST ZONE, !- Zone 1 Name",
        "    , !- Design Specification Outdoor Air Object Name 1",
        "    West Zone Design Spec Zone Air Dist; !- Design Specification Zone Air Distribution Object Name 1",

        "DesignSpecification:ZoneAirDistribution,",
        "  West Zone Design Spec Zone Air Dist, !- Name",
        "  1, !- Zone Air Distribution Effectiveness in Cooling Mode{ dimensionless }",
        "  1; !- Zone Air Distribution Effectiveness in Heating Mode{ dimensionless }",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    AirLoopControlInfo.allocate(1);
    AirLoopControlInfo(1).LoopFlowRateSet = true;
    OARequirements.allocate(1);
    ZoneAirDistribution.allocate(1);
    ZoneAirDistribution(1).Name = "CM DSZAD WEST ZONE";
    ZoneAirDistribution(1).ZoneADEffSchPtr = 4;

    AirLoopFlow.allocate(1);
    AirLoopFlow(1).OAFrac = 0.01;    // DataAirLoop variable (AirloopHVAC)
    AirLoopFlow(1).OAMinFrac = 0.01; // DataAirLoop variable (AirloopHVAC)

    GetZoneData(ErrorsFound);  // read zone data
    EXPECT_FALSE(ErrorsFound); // expect no errors
    GetZoneAirDistribution();
    GetZoneSizingInput();
    DataGlobals::DoZoneSizing = true;
    GetOAControllerInputs();

    EXPECT_EQ(0.00944, VentilationMechanical(1).ZoneOAPeopleRate(1));
    EXPECT_EQ(0.00, VentilationMechanical(1).ZoneOAAreaRate(1));
    EXPECT_EQ(0.00, VentilationMechanical(1).ZoneOAFlowRate(1));
    EXPECT_EQ(0.00, VentilationMechanical(1).ZoneOAACHRate(1));
}

TEST_F(EnergyPlusFixture, MixedAir_TestHXinOASystem)
{
    std::string const idf_objects = delimited_string({
        "Version,8.3;",

        "  OutdoorAir:Node,",
        "    Outside Air Inlet Node;  !- Name",

        "  Controller:OutdoorAir,",
        "    OA Controller 1,         !- Name",
        "    Relief Air Outlet Node,  !- Relief Air Outlet Node Name",
        "    Air Loop Inlet Node,     !- Return Air Node Name",
        "    Mixed Air Node,          !- Mixed Air Node Name",
        "    Outside Air Inlet Node,  !- Actuator Node Name",
        "    1.0,                     !- Minimum Outdoor Air Flow Rate {m3/s}",
        "    1.0,                     !- Maximum Outdoor Air Flow Rate {m3/s}",
        "    NoEconomizer,            !- Economizer Control Type", // Economizer should open for this one, so OA flow should be > min OA
        "    ModulateFlow,            !- Economizer Control Action Type",
        "    ,                        !- Economizer Maximum Limit Dry-Bulb Temperature {C}",
        "    ,                        !- Economizer Maximum Limit Enthalpy {J/kg}",
        "    ,                        !- Economizer Maximum Limit Dewpoint Temperature {C}",
        "    ,                        !- Electronic Enthalpy Limit Curve Name",
        "    ,                        !- Economizer Minimum Limit Dry-Bulb Temperature {C}",
        "    NoLockout,               !- Lockout Type", // No lockout
        "    ProportionalMinimum,     !- Minimum Limit Type",
        "    ,                        !- Minimum Outdoor Air Schedule Name",
        "    ,                        !- Minimum Fraction of Outdoor Air Schedule Name",
        "    ,                        !- Maximum Fraction of Outdoor Air Schedule Name",
        "    ,                        !- Mechanical Ventilation Controller Name",
        "    ,                        !- Time of Day Economizer Control Schedule Name",
        "    No,                      !- High Humidity Control",
        "    ,                        !- Humidistat Control Zone Name",
        "    ,                        !- High Humidity Outdoor Air Flow Ratio",
        "    No;                      !- Control High Indoor Humidity Based on Outdoor Humidity Ratio",

        "  HeatExchanger:AirToAir:FlatPlate,",
        "    OA Heat Exchanger 1,     !- Name",
        "    ,                        !- Availability Schedule Name",
        "    ParallelFlow,            !- Flow Arrangement Type",
        "    No,                      !- Economizer Lockout",
        "    1,                       !- Ratio of Supply to Secondary hA Values",
        "    1.0,                     !- Nominal Supply Air Flow Rate{ m3 / s }",
        "    5,                       !- Nominal Supply Air Inlet Temperature{ C }",
        "    10,                      !- Nominal Supply Air Outlet Temperature{ C }",
        "    1.0,                     !- Nominal Secondary Air Flow Rate{ m3 / s }",
        "    20,                      !- Nominal Secondary Air Inlet Temperature{ C }",
        "    0,                       !- Nominal Electric Power{ W }",
        "    Heat Exchanger Outlet Node2, !- Supply Air Inlet Node Name",
        "    Heat Exchanger Outlet Node,  !- Supply Air Outlet Node Name",
        "    Relief Air Outlet Node,      !- Secondary Air Inlet Node Name",
        "    Heat Exchanger Secondary Outlet Node;  !- Secondary Air Outlet Node Name",

        "  HeatExchanger:AirToAir:FlatPlate,",
        "    OA Heat Exchanger 2,     !- Name",
        "    ,                        !- Availability Schedule Name",
        "    ParallelFlow,            !- Flow Arrangement Type",
        "    No,                      !- Economizer Lockout",
        "    1,                       !- Ratio of Supply to Secondary hA Values",
        "    1.0,                     !- Nominal Supply Air Flow Rate{ m3 / s }",
        "    5,                       !- Nominal Supply Air Inlet Temperature{ C }",
        "    10,                      !- Nominal Supply Air Outlet Temperature{ C }",
        "    1.0,                     !- Nominal Secondary Air Flow Rate{ m3 / s }",
        "    20,                      !- Nominal Secondary Air Inlet Temperature{ C }",
        "    0,                       !- Nominal Electric Power{ W }",
        "    Outside Air Inlet Node,  !- Supply Air Inlet Node Name",
        "    Heat Exchanger Outlet Node2,           !- Supply Air Outlet Node Name",
        "    Heat Exchanger Secondary Outlet Node,  !- Secondary Air Inlet Node Name",
        "    Heat Exchanger Secondary Outlet Node2; !- Secondary Air Outlet Node Name",

        "  OutdoorAir:Mixer,",
        "    OA Mixer,                !- Name",
        "    Mixed Air Node,          !- Mixed Air Node Name",
        "    Heat Exchanger Outlet Node, !- Outdoor Air Stream Node Name",
        "    Relief Air Outlet Node,  !- Relief Air Stream Node Name",
        "    Air Loop Inlet Node;     !- Return Air Stream Node Name",

        " AirLoopHVAC:ControllerList,",
        "    OA Sys 1 controller,     !- Name",
        "    Controller:OutdoorAir,   !- Controller 1 Object Type",
        "    OA Controller 1;         !- Controller 1 Name",

        " AirLoopHVAC:OutdoorAirSystem:EquipmentList,",
        "    OA Sys 1 Equipment list, !- Name",
        "    HeatExchanger:AirToAir:FlatPlate, !- Component 1 Object Type",
        "    OA Heat Exchanger 2,     !- Component 1 Name",
        "    HeatExchanger:AirToAir:FlatPlate, !- Component 1 Object Type",
        "    OA Heat Exchanger 1,     !- Component 1 Name",
        "    OutdoorAir:Mixer,        !- Component 2 Object Type",
        "    OA Mixer;                !- Component 2 Name",

        " AirLoopHVAC:OutdoorAirSystem,",
        "    OA Sys 1, !- Name",
        "    OA Sys 1 controller,     !- Controller List Name",
        "    OA Sys 1 Equipment list; !- Outdoor Air Equipment List Name",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    GetOASysInputFlag = true;
    DataGlobals::BeginEnvrnFlag = true;
    int AirloopNum = 1;
    int OASysNum = 1;
    int OAControllerNum = 1;
    PrimaryAirSystem.allocate(AirloopNum);
    PrimaryAirSystem(AirloopNum).Name = "Airloop 1";
    AirLoopControlInfo.allocate(AirloopNum); // will be deallocated by MixedAir::clear_state(); in EnergyPlusFixture
    AirLoopFlow.allocate(AirloopNum);        // will be deallocated by MixedAir::clear_state(); in EnergyPlusFixture
    DataEnvironment::StdRhoAir = 1.2;
    DataEnvironment::OutBaroPress = 101250.0;
    AirLoopFlow(AirloopNum).DesSupply = 1.0 * DataEnvironment::StdRhoAir;

    // setup OA system and initialize nodes
    ManageOutsideAirSystem("OA Sys 1", true, AirloopNum, OASysNum);

    // reset nodes to common property
    for (int i = 1; i <= DataLoopNode::NumOfNodes; ++i) {
        Node(i).Temp = 20.0;
        Node(i).HumRat = 0.01;
        Node(i).Enthalpy = 45478.0;
        Node(i).MassFlowRate = AirLoopFlow(AirloopNum).DesSupply;
        Node(i).MassFlowRateMaxAvail = AirLoopFlow(AirloopNum).DesSupply;
        Node(i).Press = 101250.0;
    }

    // simulate OA system, common node property is propogated
    ManageOutsideAirSystem("OA Sys 1", true, AirloopNum, OASysNum);

    // change node property at OA inlet and mixer inlet
    Node(2).Temp = 18.0; // reset temps at HX
    Node(5).Temp = 24.0;

    // simulate OA system
    ManageOutsideAirSystem("OA Sys 1", true, AirloopNum, OASysNum);

    int mixedAirNode = OAController(OAControllerNum).MixNode;
    int mixerIntletNode = OAController(OAControllerNum).InletNode;
    EXPECT_EQ(Node(mixedAirNode).Temp, Node(mixerIntletNode).Temp);
}

TEST_F(EnergyPlusFixture, MixedAir_HumidifierOnOASystemTest)
{
    std::string const idf_objects = delimited_string({
        "Version,8.4;",

        "AirLoopHVAC:OutdoorAirSystem,",
        "    DOAS OA System,          !- Name",
        "    DOAS OA System Controllers,  !- Controller List Name",
        "    DOAS OA System Equipment;!- Outdoor Air Equipment List Name",

        "AirLoopHVAC:OutdoorAirSystem:EquipmentList,",
        "    DOAS OA System Equipment,!- Name",
        "    Humidifier:Steam:Electric,        !- Component 1 Object Type",
        "    DOAS OA Humidifier,      !- Component 1 Name",
        "    OutdoorAir:Mixer,        !- Component 2 Object Type",
        "    DOAS OA Mixing Box;      !- Component 2 Name",

        "  Humidifier:Steam:Electric,",
        "    DOAS OA Humidifier,      !- Name",
        "    FanAvailSched,           !- Availability Schedule Name",
        "    8.0E-006,                !- Rated Capacity {m3/s}",
        "    21000.0,                 !- Rated Power {W}",
        "    0,                       !- Rated Fan Power {W}",
        "    0,                       !- Standby Power {W}",
        "    DOAS Outdoor Air Inlet,  !- Air Inlet Node Name",
        "    DOAS Humidifier Air Outlet, !- Air Outlet Node Name",
        "    ;                        !- Water Storage Tank Name",

        "Schedule:Compact,",
        "    FanAvailSched,           !- Name",
        "    Fraction,                !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: Alldays,            !- Field 2",
        "    Until: 24:00, 1.0;       !- Field 3",

        "ScheduleTypeLimits,",
        "    Fraction,                !- Name",
        "    0.0,                     !- Lower Limit Value",
        "    1.0,                     !- Upper Limit Value",
        "    CONTINUOUS;              !- Numeric Type",

        "  SetpointManager:Scheduled,",
        "    Main Humidifier setpoint Mgr,  !- Name",
        "    MinimumHumidityRatio,          !- Control Variable",
        "    Humidifier Setpoint Schedule,  !- Schedule Name",
        "    DOAS Humidifier Air Outlet;    !- Setpoint Node or NodeList Name",

        "  Schedule:Compact,",
        "    Humidifier Setpoint Schedule,  !- Name",
        "    HumidityRatio,           !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: Alldays,            !- Field 2",
        "    Until: 24:00,0.005;      !- Field 3",

        "  ScheduleTypeLimits,",
        "    HumidityRatio,           !- Name",
        "    0.0001,                  !- Lower Limit Value",
        "    0.0120,                  !- Upper Limit Value",
        "    CONTINUOUS;              !- Numeric Type",

        "OutdoorAir:NodeList,",
        "    DOAS Outdoor Air Inlet;  !- Node or NodeList Name 1",

        "Controller:OutdoorAir,",
        "    DOAS OA Controller,      !- Name",
        "    DOAS Relief Air Outlet,  !- Relief Air Outlet Node Name",
        "    DOAS Air Loop Inlet,     !- Return Air Node Name",
        "    DOAS Mixed Air Outlet,   !- Mixed Air Node Name",
        "    DOAS Outdoor Air Inlet,  !- Actuator Node Name",
        "    1.0,                     !- Minimum Outdoor Air Flow Rate {m3/s}",
        "    1.0,                     !- Maximum Outdoor Air Flow Rate {m3/s}",
        "    NoEconomizer,            !- Economizer Control Type",
        "    ModulateFlow,            !- Economizer Control Action Type",
        "    ,                        !- Economizer Maximum Limit Dry-Bulb Temperature {C}",
        "    ,                        !- Economizer Maximum Limit Enthalpy {J/kg}",
        "    ,                        !- Economizer Maximum Limit Dewpoint Temperature {C}",
        "    ,                        !- Electronic Enthalpy Limit Curve Name",
        "    12.2,                    !- Economizer Minimum Limit Dry-Bulb Temperature {C}",
        "    NoLockout,               !- Lockout Type",
        "    ProportionalMinimum,     !- Minimum Limit Type",
        "    ,                        !- Minimum Outdoor Air Schedule Name",
        "    ,                        !- Minimum Fraction of Outdoor Air Schedule Name",
        "    ,                        !- Maximum Fraction of Outdoor Air Schedule Name",
        "    ,                        !- Mechanical Ventilation Controller Name",
        "    ,                        !- Time of Day Economizer Control Schedule Name",
        "    No,                      !- High Humidity Control",
        "    ,                        !- Humidistat Control Zone Name",
        "    ,                        !- High Humidity Outdoor Air Flow Ratio",
        "    No;                      !- Control High Indoor Humidity Based on Outdoor Humidity Ratio",

        "OutdoorAir:Mixer,",
        "    DOAS OA Mixing Box,      !- Name",
        "    DOAS Mixed Air Outlet,   !- Mixed Air Node Name",
        "    DOAS Humidifier Air Outlet, !- Outdoor Air Stream Node Name",
        "    DOAS Relief Air Outlet,  !- Relief Air Stream Node Name",
        "    DOAS Air Loop Inlet;     !- Return Air Stream Node Name",

        "AirLoopHVAC:ControllerList,",
        "    DOAS OA System Controllers,  !- Name",
        "    Controller:OutdoorAir,   !- Controller 1 Object Type",
        "    DOAS OA Controller;      !- Controller 1 Name",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    DataGlobals::NumOfTimeStepInHour = 1;
    DataGlobals::MinutesPerTimeStep = 60 / DataGlobals::NumOfTimeStepInHour;
    DataGlobals::TimeStep = 1;
    DataGlobals::HourOfDay = 1;
    DataEnvironment::DayOfWeek = 1;
    DataEnvironment::DayOfYear_Schedule = 1;
    ScheduleManager::UpdateScheduleValues();

    GetOASysInputFlag = true;
    DataGlobals::BeginEnvrnFlag = true;
    int AirloopNum = 1;
    int OASysNum = 1;
    int HumNum(1);
    int AirInNode(0);
    int AirOutNode(0);
    Real64 WaterConsumptionRate(0.0); // water use rate of the humidifier
    Real64 ElecPowerInput(0.0);       // electric use rate of the humidifier

    PrimaryAirSystem.allocate(AirloopNum);
    PrimaryAirSystem(AirloopNum).Name = "Airloop 1";
    AirLoopControlInfo.allocate(AirloopNum);
    AirLoopFlow.allocate(AirloopNum);
    AirLoopFlow(AirloopNum).DesSupply = 1.0 * DataEnvironment::StdRhoAir;
    DataEnvironment::StdRhoAir = 1.2;
    DataEnvironment::OutBaroPress = 101250.0;
    DataSizing::SysSizingRunDone = false;
    DataSizing::CurSysNum = 1;

    GetOutsideAirSysInputs();
    EXPECT_EQ(1, NumOASystems);
    EXPECT_EQ("DOAS OA SYSTEM", OutsideAirSys(OASysNum).Name);

    // setup OA system and initialize nodes
    ManageOutsideAirSystem(OutsideAirSys(OASysNum).Name, true, AirloopNum, OASysNum);
    // reset nodes to common property
    for (int i = 1; i <= DataLoopNode::NumOfNodes; ++i) {
        Node(i).Temp = 20.0;
        Node(i).HumRat = 0.0005;
        Node(i).Enthalpy = Psychrometrics::PsyHFnTdbW(DataLoopNode::Node(i).Temp, DataLoopNode::Node(i).HumRat);
        Node(i).MassFlowRate = AirLoopFlow(AirloopNum).DesSupply;
        Node(i).MassFlowRateMaxAvail = AirLoopFlow(AirloopNum).DesSupply;
        Node(i).Press = 101250.0;
    }
    // simulate OA system, common node properties are propagated
    ManageOutsideAirSystem(OutsideAirSys(OASysNum).Name, true, AirloopNum, OASysNum);
    // humidifier water and electric use rate are zero (no Hum Rat setpoint applied)
    EXPECT_EQ(0.0, Humidifiers::Humidifier(HumNum).WaterAdd);
    EXPECT_EQ(0.0, Humidifiers::Humidifier(HumNum).ElecUseRate);

    // Add humidity ratio setpoint to the humidifier air outlet node
    Node(2).HumRatMin = 0.005; // humidity ratio setpoint value
    // simulate OA system
    ManageOutsideAirSystem(OutsideAirSys(OASysNum).Name, true, AirloopNum, OASysNum);
    // get humidifier's air inlet and outlet node number
    AirInNode = Humidifiers::Humidifier(HumNum).AirInNode;
    AirOutNode = Humidifiers::Humidifier(HumNum).AirOutNode;
    // Calculate expected humidifier water consumption rate
    WaterConsumptionRate = AirLoopFlow(AirloopNum).DesSupply * (0.005 - 0.0005);
    // Calculate humidifier electric use rate (fan electric power and standby electric power are zero)
    ElecPowerInput = (WaterConsumptionRate / Humidifiers::Humidifier(HumNum).NomCap) * Humidifiers::Humidifier(HumNum).NomPower;
    // Confirm humidifier water consumption calculation
    EXPECT_EQ(WaterConsumptionRate, Humidifiers::Humidifier(HumNum).WaterAdd);
    // confirm that electric energy is used by the humidifier
    EXPECT_EQ(ElecPowerInput, Humidifiers::Humidifier(HumNum).ElecUseRate);
}

TEST_F(EnergyPlusFixture, FreezingCheckTest)
{
    std::string const idf_objects = delimited_string({
        "Version,8.3;",
        "  OutdoorAir:Node,",
        "    Outside Air Inlet Node 1; !- Name",
        "  Schedule:Constant,",
        "    OAFractionSched, !- Name",
        "     , !- Schedule Type Limits Name",
        "     1.0; !- Hourly value",
        "  Controller:OutdoorAir,",
        "    OA Controller 1,         !- Name",
        "    Relief Air Outlet Node 1, !- Relief Air Outlet Node Name",
        "    Outdoor Air Mixer Inlet Node,    !- Return Air Node Name",
        "    Mixed Air Node,        !- Mixed Air Node Name",
        "    Outside Air Inlet Node, !- Actuator Node Name",
        "    0.2,                !- Minimum Outdoor Air Flow Rate {m3/s}",
        "    1.0,                !- Maximum Outdoor Air Flow Rate {m3/s}",
        "    NoEconomizer,     !- Economizer Control Type", // Economizer should open for this one, so OA flow should be > min OA
        "    ModulateFlow,            !- Economizer Control Action Type",
        "    ,                        !- Economizer Maximum Limit Dry-Bulb Temperature {C}",
        "    ,                        !- Economizer Maximum Limit Enthalpy {J/kg}",
        "    ,                        !- Economizer Maximum Limit Dewpoint Temperature {C}",
        "    ,                        !- Electronic Enthalpy Limit Curve Name",
        "    ,                        !- Economizer Minimum Limit Dry-Bulb Temperature {C}",
        "    NoLockout,               !- Lockout Type", // No lockout
        "    ProportionalMinimum,     !- Minimum Limit Type",
        "    OAFractionSched;                        !- Minimum Outdoor Air Schedule Name",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    GetOAControllerInputs();

    AirLoopControlInfo.allocate(1); // will be deallocated by MixedAir::clear_state(); in EnergyPlusFixture
    AirLoopFlow.allocate(1);        // will be deallocated by MixedAir::clear_state(); in EnergyPlusFixture
    Node.allocate(5);               // will be deallocated by DataLoopNode::clear_state(); in EnergyPlusFixture

    int OAControllerNum = 1;
    int AirLoopNum = 1;

    AirLoopControlInfo(AirLoopNum).EconoLockout = false;
    AirLoopControlInfo(AirLoopNum).NightVent = false;
    AirLoopControlInfo(AirLoopNum).FanOpMode = DataHVACGlobals::CycFanCycCoil;
    AirLoopControlInfo(AirLoopNum).LoopFlowRateSet = false;
    AirLoopControlInfo(AirLoopNum).CheckHeatRecoveryBypassStatus = true;
    AirLoopControlInfo(AirLoopNum).OASysComponentsSimulated = true;
    AirLoopControlInfo(AirLoopNum).EconomizerFlowLocked = false;
    AirLoopControlInfo(AirLoopNum).HeatRecoveryBypass = false;
    AirLoopControlInfo(AirLoopNum).HeatRecoveryResimFlag = false; // Need this to avoid resetting hxbypass, saying this has already been simulated
    AirLoopFlow(AirLoopNum).DesSupply = 1.0;

    StdBaroPress = StdPressureSeaLevel;
    StdRhoAir = Psychrometrics::PsyRhoAirFnPbTdbW(StdBaroPress, 20.0, 0.0);

    // Initialize common OA controller and node data
    OAController(OAControllerNum).MinOAMassFlowRate = OAController(OAControllerNum).MinOA * StdRhoAir;
    OAController(OAControllerNum).MaxOAMassFlowRate = OAController(OAControllerNum).MaxOA * StdRhoAir;
    OAController(OAControllerNum).InletNode = OAController(OAControllerNum).OANode;
    OAController(OAControllerNum).RetTemp = 24.0;
    OAController(OAControllerNum).InletTemp = 5.0; // This is the same as the outdoor air dry bulb for these tests
    OAController(OAControllerNum).OATemp = 5.0;
    OAController(OAControllerNum).MixSetTemp = 22.0;
    OAController(OAControllerNum).ExhMassFlow = 0.0;
    // OAController( OAControllerNum ).InletEnth = needs to be initialized if an enthalpy economizer is tested
    // OAController( OAControllerNum ).RetEnth = needs to be initialized if an enthalpy economizer is tested
    OAController(OAControllerNum).MixMassFlow = 0.5;                                    // Note this is 50% of design flow set above
    Node(OAControllerNum * 4).MassFlowRate = OAController(OAControllerNum).MixMassFlow; // Return air nodes
    Node(OAControllerNum * 4).Temp = OAController(OAControllerNum).RetTemp;             // Return air nodes
    Node(OAControllerNum * 4).Enthalpy = Psychrometrics::PsyHFnTdbW(OAController(OAControllerNum).RetTemp, 0.0); // Return air nodes, dry air
    Node(OAControllerNum * 4 - 3).TempSetPoint = OAController(OAControllerNum).MixSetTemp;                       // Mixed air nodes
    Node(OAControllerNum * 4 - 2).Enthalpy = Psychrometrics::PsyHFnTdbW(OAController(OAControllerNum).InletTemp, 0.0);
    ; // OA inlet (actuated) air nodes, dry air

    OAController(1).CoolCoilFreezeCheck = true;
    Schedule(1).CurrentValue = 1.0;

    OAController(OAControllerNum).CalcOAController(AirLoopNum, true);

    EXPECT_NEAR(0.2408617, OAController(1).OAFractionRpt, 0.00001);
}

TEST_F(EnergyPlusFixture, MixedAir_ControllerTypeTest)
{

    int OAControllerNum = 1;

    OAController.allocate(OAControllerNum);
    Node.allocate(4);

    OAController(OAControllerNum).ControllerType_Num = ControllerOutsideAir;
    OAController(OAControllerNum).OANode = 1;
    OAController(OAControllerNum).InletNode = 2;
    OAController(OAControllerNum).RelNode = 3;
    OAController(OAControllerNum).RetNode = 4;
    OAController(OAControllerNum).OAMassFlow = 0.1;
    OAController(OAControllerNum).ExhMassFlow = 0.0;
    OAController(OAControllerNum).RelMassFlow = max(OAController(OAControllerNum).OAMassFlow - OAController(OAControllerNum).ExhMassFlow, 0.0);
    Node(OAController(OAControllerNum).RetNode).MassFlowRate = 0.2;
    Node(OAController(OAControllerNum).RelNode).MassFlowRate = OAController(OAControllerNum).RelMassFlow;
    Node(OAController(OAControllerNum).InletNode).CO2 = 600.0;
    Node(OAController(OAControllerNum).RelNode).CO2 = 500.0;
    OutdoorCO2 = 400.0;

    Node(OAController(OAControllerNum).InletNode).GenContam = 0.5;
    Node(OAController(OAControllerNum).RelNode).GenContam = 0.3;
    OutdoorGC = 0.1;

    Contaminant.CO2Simulation = true;
    Contaminant.GenericContamSimulation = true;

    OAController(OAControllerNum).UpdateOAController();
    // Expect no value changes of relief node due to no actions.
    EXPECT_NEAR(500.0, Node(OAController(OAControllerNum).RelNode).CO2, 0.00001);
    EXPECT_NEAR(0.3, Node(OAController(OAControllerNum).RelNode).GenContam, 0.00001);

    Contaminant.CO2Simulation = false;
    Contaminant.GenericContamSimulation = false;
    OAController.deallocate();
    Node.deallocate();
}

TEST_F(EnergyPlusFixture, MixedAir_MissingHIghRHControlInputTest)
{
    std::string const idf_objects = delimited_string({
        "Version,8.3;",

        "  OutdoorAir:Node,",
        "    Outside Air Inlet Node 1; !- Name",

        "  Schedule:Constant,",
        "    Min OA Sched,             !- Name",
        "     Any Number,              !- Schedule Type Limits Name",
        "     0.5;                     !- Value",

        "  ScheduleTypeLimits,",
        "    Any Number;              !- Name",

        "  Controller:OutdoorAir,",
        "    OA Controller 1,          !- Name",
        "    Relief Air Outlet Node 1, !- Relief Air Outlet Node Name",
        "    VAV Sys 1 Inlet Node,     !- Return Air Node Name",
        "    Mixed Air Node 1,         !- Mixed Air Node Name",
        "    Outside Air Inlet Node 1, !- Actuator Node Name",
        "    autosize,                 !- Minimum Outdoor Air Flow Rate {m3/s}",
        "    autosize,                 !- Maximum Outdoor Air Flow Rate {m3/s}",
        "    FixedDryBulb,             !- Economizer Control Type",
        "    ModulateFlow,             !- Economizer Control Action Type",
        "    ,                         !- Economizer Maximum Limit Dry-Bulb Temperature {C}",
        "    ,                         !- Economizer Maximum Limit Enthalpy {J/kg}",
        "    ,                         !- Economizer Maximum Limit Dewpoint Temperature {C}",
        "    ,                         !- Electronic Enthalpy Limit Curve Name",
        "    ,                         !- Economizer Minimum Limit Dry-Bulb Temperature {C}",
        "    NoLockout,                !- Lockout Type",
        "    ProportionalMinimum,      !- Minimum Limit Type",
        "    Min OA Sched,             !- Minimum Outdoor Air Schedule Name",
        "    ,                         !- Minimum Fraction of Outdoor Air Schedule Name",
        "    ,                         !- Maximum Fraction of Outdoor Air Schedule Name",
        "    ,                         !- Mechanical Ventilation Controller Name",
        "    ,                         !- Time of Day Economizer Control Schedule Name",
        "    Yes,                      !- High Humidity Control",
        "    Zone1,                    !- Humidistat Control Zone Name",
        "    1;                        !- High Humidity Outdoor Air Flow Ratio",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    compare_err_stream(""); // just for debugging

    bool ErrorsFound(false); // If errors detected in input
    int ControllerNum(0);    // Controller number
    int NumArg(0);
    int NumNums(0);
    int NumAlphas(0);
    int IOStat(0);
    std::string const CurrentModuleObject = CurrentModuleObjects(CMO_OAController);

    inputProcessor->getObjectDefMaxArgs(CurrentModuleObjects(CMO_OAController), NumArg, NumAlphas, NumNums);

    Array1D<Real64> NumArray(NumNums, 0.0);
    Array1D_string AlphArray(NumAlphas);
    Array1D_string cAlphaFields(NumAlphas);
    Array1D_string cNumericFields(NumNums);
    Array1D_bool lAlphaBlanks(NumAlphas, true);
    Array1D_bool lNumericBlanks(NumNums, true);

    NumOAControllers = inputProcessor->getNumObjectsFound(CurrentModuleObject);
    OAController.allocate(NumOAControllers);

    ControllerNum = 1;
    Zone.allocate(1);
    Zone(1).Name = "ZONE1";
    NumOfZones = 1;
    ZoneEquipConfig.allocate(1);
    ZoneEquipConfig(1).ActualZoneNum = 1;
    ZoneEquipConfig(1).ZoneNode = 2;
    ZoneEquipConfig(1).NumInletNodes = 1;
    ZoneEquipConfig(1).InletNodeAirLoopNum.allocate(1);
    ZoneEquipConfig(1).InletNodeAirLoopNum(1) = 1;
    PrimaryAirSystem.allocate(1);
    PrimaryAirSystem(1).NumBranches = 1;
    PrimaryAirSystem(1).Branch.allocate(1);
    PrimaryAirSystem(1).Branch(1).Comp.allocate(1);
    PrimaryAirSystem(1).Branch(1).TotalComponents = 1;
    PrimaryAirSystem(1).Branch(1).Comp(1).Name = "OASysName";
    PrimaryAirSystem(1).Branch(1).Comp(1).TypeOf = "AirLoopHVAC:OutdoorAirSystem";
    OutsideAirSys.allocate(1);
    OutsideAirSys(1).Name = "OASysName";
    OutsideAirSys(1).NumControllers = 1;
    OutsideAirSys(1).ControllerType.allocate(1);
    OutsideAirSys(1).ControllerType(1) = "Controller:OutdoorAir";
    OutsideAirSys(1).ControllerName.allocate(1);
    OutsideAirSys(1).ControllerName(1) = "OA Controller 1";
    DataAirLoop::NumOASystems = 1;
    HumidityControlZone.allocate(1);
    HumidityControlZone(1).ActualZoneNum = 1;
    NumHumidityControlZones = 1;

    inputProcessor->getObjectItem(CurrentModuleObject,
                                  ControllerNum,
                                  AlphArray,
                                  NumAlphas,
                                  NumArray,
                                  NumNums,
                                  IOStat,
                                  lNumericBlanks,
                                  lAlphaBlanks,
                                  cAlphaFields,
                                  cNumericFields);

    ProcessOAControllerInputs(CurrentModuleObject,
                              ControllerNum,
                              AlphArray,
                              NumAlphas,
                              NumArray,
                              NumNums,
                              lNumericBlanks,
                              lAlphaBlanks,
                              cAlphaFields,
                              cNumericFields,
                              ErrorsFound);
    // compare_err_stream( "" ); // just for debugging

    EXPECT_FALSE(ErrorsFound);
    EXPECT_FALSE(OAController(ControllerNum)
                     .ModifyDuringHighOAMoisture); // missing input defaults "Control High Indoor Humidity Based on Outdoor Humidity Ratio" to false

    std::string const error_string = delimited_string({
        "   ** Warning ** Controller:OutdoorAir \"OA CONTROLLER 1\", missing field value",
        "   **   ~~~   ** ...Control High Indoor Humidity Based on Outdoor Humidity Ratio will default to Yes when High Humidity Control= \"Yes\"",
    });
    EXPECT_TRUE(compare_err_stream(error_string, true));
}

TEST_F(EnergyPlusFixture, OAControllerMixedAirSPTest)
{

    std::string const idf_objects = delimited_string({

        "  OutdoorAir:Node,",
        "    Outside Air Inlet Node;  !- Name",

        "  Schedule:Constant,",
        "    OAFractionSched,         !- Name",
        "     ,                       !- Schedule Type Limits Name",
        "     1;                      !- Hourly value",

        "  Controller:OutdoorAir,",
        "    OA Controller 1,         !- Name",
        "    Relief Air Outlet Node,  !- Relief Air Outlet Node Name",
        "    Outdoor Air Mixer Inlet Node, !- Return Air Node Name",
        "    Mixed Air Node,          !- Mixed Air Node Name",
        "    Outside Air Inlet Node,  !- Actuator Node Name",
        "    0.0,                     !- Minimum Outdoor Air Flow Rate{ m3 / s }",
        "    1.7,                     !- Maximum Outdoor Air Flow Rate{ m3 / s }",
        "    NoEconomizer,            !- Economizer Control Type",
        "    ModulateFlow,            !- Economizer Control Action Type",
        "    ,                        !- Economizer Maximum Limit Dry - Bulb Temperature{ C }",
        "    ,                        !- Economizer Maximum Limit Enthalpy{ J / kg }",
        "    ,                        !- Economizer Maximum Limit Dewpoint Temperature{ C }",
        "    ,                        !- Electronic Enthalpy Limit Curve Name",
        "    12.5,                    !- Economizer Minimum Limit Dry - Bulb Temperature{ C }",
        "    NoLockout,               !- Lockout Type",
        "    FixedMinimum,            !- Minimum Limit Type",
        "    OAFractionSched,         !- Minimum Outdoor Air Schedule Name",
        "    ,                        !- Minimum Fraction of Outdoor Air Schedule Name",
        "    ,                        !- Maximum Fraction of Outdoor Air Schedule Name",
        "    ;                        !- Mechanical Ventilation Controller Name",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    AirLoopFlow.allocate(1);
    AirLoopControlInfo.allocate(1);
    AirLoopControlInfo(1).LoopFlowRateSet = true;

    GetOAControllerInputs();

    StdRhoAir = 1.2;
    OAController(1).MixMassFlow = 1.7 * StdRhoAir;
    OAController(1).MaxOAMassFlowRate = 1.7 * StdRhoAir;
    AirLoopFlow(1).DesSupply = 1.7 * StdRhoAir;
    AirLoopFlow(1).ReqSupplyFrac = 1.0;
    Node(OAController(1).MixNode).MassFlowRateMaxAvail = AirLoopFlow(1).DesSupply; // set max avail or controller will shut down
    Node(OAController(1).RetNode).MassFlowRate =
        AirLoopFlow(1).DesSupply; // set return flow for mixing calculation (i.e., mix flow = return flow + exhaust flow [0])
    NumOASystems = 1;
    OutsideAirSys.allocate(1);
    OutsideAirSys(1).Name = "AIRLOOP OASYSTEM";
    OutsideAirSys(1).NumControllers = 1;
    OutsideAirSys(1).ControllerName.allocate(1);
    OutsideAirSys(1).ControllerName(1) = "OA CONTROLLER 1";
    OutsideAirSys(1).ComponentType.allocate(1);
    OutsideAirSys(1).ComponentType(1) = "OutdoorAir:Mixer";
    OutsideAirSys(1).ComponentName.allocate(1);
    OutsideAirSys(1).ComponentName(1) = "OAMixer";
    OAMixer.allocate(1);
    OAMixer(1).Name = "OAMixer";
    OAMixer(1).InletNode = 2;

    DataHVACGlobals::NumPrimaryAirSys = 1;
    PrimaryAirSystem.allocate(1);
    PrimaryAirSystem(1).Name = "PrimaryAirLoop";
    PrimaryAirSystem(1).NumBranches = 1;
    PrimaryAirSystem(1).Branch.allocate(1);
    PrimaryAirSystem(1).Branch(1).TotalComponents = 1;
    PrimaryAirSystem(1).Branch(1).Comp.allocate(1);
    PrimaryAirSystem(1).Branch(1).Comp(1).Name = OutsideAirSys(1).Name;
    PrimaryAirSystem(1).Branch(1).Comp(1).TypeOf = "AirLoopHVAC:OutdoorAirSystem";

    // mixed node temperature set point has not yet been set, expect OA controller mixed temp SP to be equal to low temp limit
    InitOAController(1, true, 1);
    EXPECT_EQ(OAController(1).MixSetTemp, OAController(1).TempLowLim);

    // expect OA controller mixed node temp SP to be equal to 0.5 C.
    Node(1).TempSetPoint = 0.5;
    InitOAController(1, true, 1);
    EXPECT_EQ(OAController(1).MixSetTemp, 0.5);

    // expect OA controller mixed node temp SP to be less than 0 and equal to -5.0 C.
    Node(1).TempSetPoint = -5.0;
    InitOAController(1, true, 1);
    EXPECT_EQ(OAController(1).MixSetTemp, -5.0);
}

TEST_F(EnergyPlusFixture, MixedAir_MiscGetsPart1)
{
    std::string const idf_objects = delimited_string({
        "Version,8.3;",

        "  OutdoorAir:Node,",
        "    Outside Air Inlet Node;  !- Name",

        "  Controller:OutdoorAir,",
        "    OA Controller 1,         !- Name",
        "    Relief Air Outlet Node,  !- Relief Air Outlet Node Name",
        "    Air Loop Inlet Node,     !- Return Air Node Name",
        "    Mixed Air Node,          !- Mixed Air Node Name",
        "    Outside Air Inlet Node,  !- Actuator Node Name",
        "    1.0,                     !- Minimum Outdoor Air Flow Rate {m3/s}",
        "    1.0,                     !- Maximum Outdoor Air Flow Rate {m3/s}",
        "    NoEconomizer,            !- Economizer Control Type", // Economizer should open for this one, so OA flow should be > min OA
        "    ModulateFlow,            !- Economizer Control Action Type",
        "    ,                        !- Economizer Maximum Limit Dry-Bulb Temperature {C}",
        "    ,                        !- Economizer Maximum Limit Enthalpy {J/kg}",
        "    ,                        !- Economizer Maximum Limit Dewpoint Temperature {C}",
        "    ,                        !- Electronic Enthalpy Limit Curve Name",
        "    ,                        !- Economizer Minimum Limit Dry-Bulb Temperature {C}",
        "    NoLockout,               !- Lockout Type", // No lockout
        "    ProportionalMinimum,     !- Minimum Limit Type",
        "    ,                        !- Minimum Outdoor Air Schedule Name",
        "    ,                        !- Minimum Fraction of Outdoor Air Schedule Name",
        "    ,                        !- Maximum Fraction of Outdoor Air Schedule Name",
        "    ,                        !- Mechanical Ventilation Controller Name",
        "    ,                        !- Time of Day Economizer Control Schedule Name",
        "    No,                      !- High Humidity Control",
        "    ,                        !- Humidistat Control Zone Name",
        "    ,                        !- High Humidity Outdoor Air Flow Ratio",
        "    No;                      !- Control High Indoor Humidity Based on Outdoor Humidity Ratio",

        "  HeatExchanger:AirToAir:FlatPlate,",
        "    OA Heat Exchanger 1,     !- Name",
        "    ,                        !- Availability Schedule Name",
        "    ParallelFlow,            !- Flow Arrangement Type",
        "    No,                      !- Economizer Lockout",
        "    1,                       !- Ratio of Supply to Secondary hA Values",
        "    1.0,                     !- Nominal Supply Air Flow Rate{ m3 / s }",
        "    5,                       !- Nominal Supply Air Inlet Temperature{ C }",
        "    10,                      !- Nominal Supply Air Outlet Temperature{ C }",
        "    1.0,                     !- Nominal Secondary Air Flow Rate{ m3 / s }",
        "    20,                      !- Nominal Secondary Air Inlet Temperature{ C }",
        "    0,                       !- Nominal Electric Power{ W }",
        "    Heat Exchanger Outlet Node2, !- Supply Air Inlet Node Name",
        "    Heat Exchanger Outlet Node,  !- Supply Air Outlet Node Name",
        "    Relief Air Outlet Node,      !- Secondary Air Inlet Node Name",
        "    Heat Exchanger Secondary Outlet Node;  !- Secondary Air Outlet Node Name",

        "  HeatExchanger:AirToAir:FlatPlate,",
        "    OA Heat Exchanger 2,     !- Name",
        "    ,                        !- Availability Schedule Name",
        "    ParallelFlow,            !- Flow Arrangement Type",
        "    No,                      !- Economizer Lockout",
        "    1,                       !- Ratio of Supply to Secondary hA Values",
        "    1.0,                     !- Nominal Supply Air Flow Rate{ m3 / s }",
        "    5,                       !- Nominal Supply Air Inlet Temperature{ C }",
        "    10,                      !- Nominal Supply Air Outlet Temperature{ C }",
        "    1.0,                     !- Nominal Secondary Air Flow Rate{ m3 / s }",
        "    20,                      !- Nominal Secondary Air Inlet Temperature{ C }",
        "    0,                       !- Nominal Electric Power{ W }",
        "    Outside Air Inlet Node,  !- Supply Air Inlet Node Name",
        "    Heat Exchanger Outlet Node2,           !- Supply Air Outlet Node Name",
        "    Heat Exchanger Secondary Outlet Node,  !- Secondary Air Inlet Node Name",
        "    Heat Exchanger Secondary Outlet Node2; !- Secondary Air Outlet Node Name",

        "  OutdoorAir:Mixer,",
        "    OA Mixer,                !- Name",
        "    Mixed Air Node,          !- Mixed Air Node Name",
        "    Heat Exchanger Outlet Node, !- Outdoor Air Stream Node Name",
        "    Relief Air Outlet Node,  !- Relief Air Stream Node Name",
        "    Air Loop Inlet Node;     !- Return Air Stream Node Name",

        " AirLoopHVAC:ControllerList,",
        "    OA Sys 1 controller,     !- Name",
        "    Controller:OutdoorAir,   !- Controller 1 Object Type",
        "    OA Controller 1;         !- Controller 1 Name",

        " AirLoopHVAC:OutdoorAirSystem:EquipmentList,",
        "    OA Sys 1 Equipment list, !- Name",
        "    HeatExchanger:AirToAir:FlatPlate, !- Component 1 Object Type",
        "    OA Heat Exchanger 2,     !- Component 1 Name",
        "    HeatExchanger:AirToAir:FlatPlate, !- Component 1 Object Type",
        "    OA Heat Exchanger 1,     !- Component 1 Name",
        "    OutdoorAir:Mixer,        !- Component 2 Object Type",
        "    OA Mixer;                !- Component 2 Name",

        " AirLoopHVAC:OutdoorAirSystem,",
        "    OA Sys 1, !- Name",
        "    OA Sys 1 controller,     !- Controller List Name",
        "    OA Sys 1 Equipment list; !- Outdoor Air Equipment List Name",
    });

    ASSERT_TRUE(process_idf(idf_objects));
    GetOAControllerInputs();

    EXPECT_EQ(1, GetNumOAMixers());
    EXPECT_EQ(1, GetNumOAControllers());
    EXPECT_EQ(3, GetOAMixerReliefNodeNumber(1));
    EXPECT_EQ(1, GetOAMixerIndex("OA Mixer"));
}

TEST_F(EnergyPlusFixture, MixedAir_MiscGetsPart2)
{
    std::string const idf_objects = delimited_string({

        "Version,8.4;",

        "SimulationControl,",
        "    Yes,                     !- Do Zone Sizing Calculation",
        "    Yes,                     !- Do System Sizing Calculation",
        "    Yes,                     !- Do Plant Sizing Calculation",
        "    Yes,                     !- Run Simulation for Sizing Periods",
        "    No;                      !- Run Simulation for Weather File Run Periods",

        "Building,",
        "    Fan Coil with DOAS,      !- Name",
        "    30.,                     !- North Axis {deg}",
        "    City,                    !- Terrain",
        "    0.04,                    !- Loads Convergence Tolerance Value",
        "    0.4,                     !- Temperature Convergence Tolerance Value {deltaC}",
        "    FullExterior,            !- Solar Distribution",
        "    25,                      !- Maximum Number of Warmup Days",
        "    6;                       !- Minimum Number of Warmup Days",

        "Timestep,4;",

        "Site:Location,",
        "    CHICAGO_IL_USA TMY2-94846,  !- Name",
        "    41.78,                   !- Latitude {deg}",
        "    -87.75,                  !- Longitude {deg}",
        "    -6.00,                   !- Time Zone {hr}",
        "    190.00;                  !- Elevation {m}",

        "! CHICAGO_IL_USA Annual Cooling 1% Design Conditions, MaxDB=  31.5�C MCWB=  23.0�C",
        "SizingPeriod:DesignDay,",
        "    CHICAGO_IL_USA Annual Cooling 1% Design Conditions DB/MCWB,  !- Name",
        "    7,                       !- Month",
        "    21,                      !- Day of Month",
        "    SummerDesignDay,         !- Day Type",
        "    31.5,                    !- Maximum Dry-Bulb Temperature {C}",
        "    10.7,                    !- Daily Dry-Bulb Temperature Range {deltaC}",
        "    ,                        !- Dry-Bulb Temperature Range Modifier Type",
        "    ,                        !- Dry-Bulb Temperature Range Modifier Day Schedule Name",
        "    Wetbulb,                 !- Humidity Condition Type",
        "    23.0,                    !- Wetbulb or DewPoint at Maximum Dry-Bulb {C}",
        "    ,                        !- Humidity Condition Day Schedule Name",
        "    ,                        !- Humidity Ratio at Maximum Dry-Bulb {kgWater/kgDryAir}",
        "    ,                        !- Enthalpy at Maximum Dry-Bulb {J/kg}",
        "    ,                        !- Daily Wet-Bulb Temperature Range {deltaC}",
        "    99063.,                  !- Barometric Pressure {Pa}",
        "    5.3,                     !- Wind Speed {m/s}",
        "    230,                     !- Wind Direction {deg}",
        "    No,                      !- Rain Indicator",
        "    No,                      !- Snow Indicator",
        "    No,                      !- Daylight Saving Time Indicator",
        "    ASHRAEClearSky,          !- Solar Model Indicator",
        "    ,                        !- Beam Solar Day Schedule Name",
        "    ,                        !- Diffuse Solar Day Schedule Name",
        "    ,                        !- ASHRAE Clear Sky Optical Depth for Beam Irradiance (taub) {dimensionless}",
        "    ,                        !- ASHRAE Clear Sky Optical Depth for Diffuse Irradiance (taud) {dimensionless}",
        "    1.0;                     !- Sky Clearness",

        "! CHICAGO_IL_USA Annual Heating 99% Design Conditions DB, MaxDB= -17.3�C",
        "SizingPeriod:DesignDay,",
        "    CHICAGO_IL_USA Annual Heating 99% Design Conditions DB,  !- Name",
        "    1,                       !- Month",
        "    21,                      !- Day of Month",
        "    WinterDesignDay,         !- Day Type",
        "    -17.3,                   !- Maximum Dry-Bulb Temperature {C}",
        "    0.0,                     !- Daily Dry-Bulb Temperature Range {deltaC}",
        "    ,                        !- Dry-Bulb Temperature Range Modifier Type",
        "    ,                        !- Dry-Bulb Temperature Range Modifier Day Schedule Name",
        "    Wetbulb,                 !- Humidity Condition Type",
        "    -17.3,                   !- Wetbulb or DewPoint at Maximum Dry-Bulb {C}",
        "    ,                        !- Humidity Condition Day Schedule Name",
        "    ,                        !- Humidity Ratio at Maximum Dry-Bulb {kgWater/kgDryAir}",
        "    ,                        !- Enthalpy at Maximum Dry-Bulb {J/kg}",
        "    ,                        !- Daily Wet-Bulb Temperature Range {deltaC}",
        "    99063.,                  !- Barometric Pressure {Pa}",
        "    4.9,                     !- Wind Speed {m/s}",
        "    270,                     !- Wind Direction {deg}",
        "    No,                      !- Rain Indicator",
        "    No,                      !- Snow Indicator",
        "    No,                      !- Daylight Saving Time Indicator",
        "    ASHRAEClearSky,          !- Solar Model Indicator",
        "    ,                        !- Beam Solar Day Schedule Name",
        "    ,                        !- Diffuse Solar Day Schedule Name",
        "    ,                        !- ASHRAE Clear Sky Optical Depth for Beam Irradiance (taub) {dimensionless}",
        "    ,                        !- ASHRAE Clear Sky Optical Depth for Diffuse Irradiance (taud) {dimensionless}",
        "    0.0;                     !- Sky Clearness",

        "Site:GroundTemperature:BuildingSurface,21.5,21.4,21.5,21.5,22.0,22.9,23.0,23.1,23.1,22.2,21.7,21.6;",

        "ScheduleTypeLimits,",
        "    Any Number;              !- Name",

        "ScheduleTypeLimits,",
        "    Fraction,                !- Name",
        "    0.0,                     !- Lower Limit Value",
        "    1.0,                     !- Upper Limit Value",
        "    CONTINUOUS;              !- Numeric Type",

        "ScheduleTypeLimits,",
        "    Temperature,             !- Name",
        "    -60,                     !- Lower Limit Value",
        "    200,                     !- Upper Limit Value",
        "    CONTINUOUS,              !- Numeric Type",
        "    Temperature;             !- Unit Type",

        "ScheduleTypeLimits,",
        "    Control Type,            !- Name",
        "    0,                       !- Lower Limit Value",
        "    4,                       !- Upper Limit Value",
        "    DISCRETE;                !- Numeric Type",

        "ScheduleTypeLimits,",
        "    On/Off,                  !- Name",
        "    0,                       !- Lower Limit Value",
        "    1,                       !- Upper Limit Value",
        "    DISCRETE;                !- Numeric Type",

        "ScheduleTypeLimits,",
        "    FlowRate,                !- Name",
        "    0.0,                     !- Lower Limit Value",
        "    10,                      !- Upper Limit Value",
        "    CONTINUOUS;              !- Numeric Type",

        "ScheduleTypeLimits,",
        "    HVACTemplate Any Number; !- Name",

        "!-   ===========  ALL OBJECTS IN CLASS: SCHEDULE:COMPACT ===========",

        "Schedule:Compact,",
        "    OCCUPY-1,                !- Name",
        "    Fraction,                !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: WeekDays SummerDesignDay CustomDay1 CustomDay2,  !- Field 2",
        "    Until: 8:00, 0.0,        !- Field 4",
        "    Until: 11:00, 1.00,      !- Field 6",
        "    Until: 12:00, 0.80,      !- Field 8",
        "    Until: 13:00, 0.40,      !- Field 10",
        "    Until: 14:00, 0.80,      !- Field 12",
        "    Until: 18:00, 1.00,      !- Field 14",
        "    Until: 19:00, 0.50,      !- Field 16",
        "    Until: 21:00, 0.10,      !- Field 18",
        "    Until: 24:00, 0.0,       !- Field 20",
        "    For: Weekends WinterDesignDay Holiday,  !- Field 21",
        "    Until: 24:00, 0.0;       !- Field 23",

        "Schedule:Compact,",
        "    LIGHTS-1,                !- Name",
        "    Fraction,                !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: WeekDays SummerDesignDay CustomDay1 CustomDay2,  !- Field 2",
        "    Until: 8:00, 0.05,       !- Field 4",
        "    Until: 9:00, 0.9,        !- Field 6",
        "    Until: 10:00, 0.95,      !- Field 8",
        "    Until: 11:00, 1.00,      !- Field 10",
        "    Until: 12:00, 0.95,      !- Field 12",
        "    Until: 13:00, 0.8,       !- Field 14",
        "    Until: 14:00, 0.9,       !- Field 16",
        "    Until: 18:00, 1.00,      !- Field 18",
        "    Until: 19:00, 0.60,      !- Field 20",
        "    Until: 21:00, 0.40,      !- Field 22",
        "    Until: 24:00, 0.05,      !- Field 24",
        "    For: Weekends WinterDesignDay Holiday,  !- Field 25",
        "    Until: 24:00, 0.05;      !- Field 27",

        "Schedule:Compact,",
        "    EQUIP-1,                 !- Name",
        "    Fraction,                !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: WeekDays SummerDesignDay CustomDay1 CustomDay2,  !- Field 2",
        "    Until: 8:00, 0.02,       !- Field 4",
        "    Until: 9:00, 0.4,        !- Field 6",
        "    Until: 14:00, 0.9,       !- Field 8",
        "    Until: 15:00, 0.8,       !- Field 10",
        "    Until: 16:00, 0.7,       !- Field 12",
        "    Until: 18:00, 0.5,       !- Field 14",
        "    Until: 21:00, 0.3,       !- Field 16",
        "    Until: 24:00, 0.02,      !- Field 18",
        "    For: Weekends WinterDesignDay Holiday,  !- Field 19",
        "    Until: 24:00, 0.02;      !- Field 21",

        "Schedule:Compact,",
        "    INFIL-SCH,               !- Name",
        "    Fraction,                !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: WeekDays CustomDay1 CustomDay2,  !- Field 2",
        "    Until: 7:00, 1.0,        !- Field 4",
        "    Until: 21:00, 0.0,       !- Field 6",
        "    Until: 24:00, 1.0,       !- Field 8",
        "    For: Weekends Holiday,   !- Field 9",
        "    Until: 24:00, 1.0,       !- Field 11",
        "    For: SummerDesignDay,    !- Field 12",
        "    Until: 24:00, 1.0,       !- Field 14",
        "    For: WinterDesignDay,    !- Field 15",
        "    Until: 24:00, 1.0;       !- Field 17",

        "Schedule:Compact,",
        "    ActSchd,                 !- Name",
        "    Any Number,              !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00, 117.239997864;",
        "                             !- Field 4",

        "Schedule:Compact,",
        "    ShadeTransSch,           !- Name",
        "    Fraction,                !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00, 0.0;       !- Field 4",

        "! For heating, recover 2 hrs early",
        "Schedule:Compact,",
        "    Htg-SetP-Sch,            !- Name",
        "    Temperature,             !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: WeekDays CustomDay1 CustomDay2,  !- Field 2",
        "    Until: 6:00, 13.0,       !- Field 4",
        "    Until: 7:00, 18.0,       !- Field 6",
        "    Until: 21:00, 23.0,      !- Field 8",
        "    Until: 24:00, 13.0,      !- Field 10",
        "    For: WeekEnds Holiday,   !- Field 11",
        "    Until: 24:00, 13.0,      !- Field 13",
        "    For: SummerDesignDay,    !- Field 14",
        "    Until: 24:00, 13.0,      !- Field 16",
        "    For: WinterDesignDay,    !- Field 17",
        "    Until: 24:00, 23.0;      !- Field 19",

        "! For cooling, recover 1 hr early",
        "Schedule:Compact,",
        "    Clg-SetP-Sch,            !- Name",
        "    Temperature,             !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: WeekDays CustomDay1 CustomDay2,  !- Field 2",
        "    Until: 7:00, 32.0,       !- Field 4",
        "    Until: 21:00, 24.0,      !- Field 6",
        "    Until: 24:00, 32.0,      !- Field 8",
        "    For: WeekEnds Holiday,   !- Field 9",
        "    Until: 24:00, 32.0,      !- Field 11",
        "    For: SummerDesignDay,    !- Field 12",
        "    Until: 24:00, 24.0,      !- Field 14",
        "    For: WinterDesignDay,    !- Field 15",
        "    Until: 24:00, 32.0;      !- Field 17",

        "Schedule:Compact,",
        "    FanAvailSched,           !- Name",
        "    Fraction,                !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: WeekDays CustomDay1 CustomDay2,  !- Field 2",
        "    Until: 7:00, 0.0,        !- Field 4",
        "    Until: 21:00, 1.0,       !- Field 6",
        "    Until: 24:00, 0.0,       !- Field 8",
        "    For: Weekends Holiday,   !- Field 9",
        "    Until: 24:00, 0.0,       !- Field 11",
        "    For: SummerDesignDay,    !- Field 12",
        "    Until: 24:00, 1.0,       !- Field 14",
        "    For: WinterDesignDay,    !- Field 15",
        "    Until: 24:00, 1.0;       !- Field 17",

        "Schedule:Compact,",
        "    HVACTemplate-Always 1,   !- Name",
        "    HVACTemplate Any Number, !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00, 1;         !- Field 4",

        "Schedule:Compact,",
        "    HVACTemplate-Always 4,   !- Name",
        "    HVACTemplate Any Number, !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00, 4;         !- Field 4",

        "Schedule:Compact,",
        "    HVACTemplate-Always 12.2,!- Name",
        "    HVACTemplate Any Number, !- Schedule Type Limits Name",
        "    Through: 12/31,          !- Field 1",
        "    For: AllDays,            !- Field 2",
        "    Until: 24:00, 12.2;      !- Field 4",

        "Material,",
        "    WD10,                    !- Name",
        "    MediumSmooth,            !- Roughness",
        "    0.667,                   !- Thickness {m}",
        "    0.115,                   !- Conductivity {W/m-K}",
        "    513,                     !- Density {kg/m3}",
        "    1381,                    !- Specific Heat {J/kg-K}",
        "    0.9,                     !- Thermal Absorptance",
        "    0.78,                    !- Solar Absorptance",
        "    0.78;                    !- Visible Absorptance",

        "Material,",
        "    RG01,                    !- Name",
        "    Rough,                   !- Roughness",
        "    1.2700000E-02,           !- Thickness {m}",
        "    1.442000,                !- Conductivity {W/m-K}",
        "    881.0000,                !- Density {kg/m3}",
        "    1674.000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.6500000,               !- Solar Absorptance",
        "    0.6500000;               !- Visible Absorptance",

        "Material,",
        "    BR01,                    !- Name",
        "    VeryRough,               !- Roughness",
        "    9.4999997E-03,           !- Thickness {m}",
        "    0.1620000,               !- Conductivity {W/m-K}",
        "    1121.000,                !- Density {kg/m3}",
        "    1464.000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.7000000,               !- Solar Absorptance",
        "    0.7000000;               !- Visible Absorptance",

        "Material,",
        "    IN46,                    !- Name",
        "    VeryRough,               !- Roughness",
        "    7.6200001E-02,           !- Thickness {m}",
        "    2.3000000E-02,           !- Conductivity {W/m-K}",
        "    24.00000,                !- Density {kg/m3}",
        "    1590.000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.5000000,               !- Solar Absorptance",
        "    0.5000000;               !- Visible Absorptance",

        "Material,",
        "    WD01,                    !- Name",
        "    MediumSmooth,            !- Roughness",
        "    1.9099999E-02,           !- Thickness {m}",
        "    0.1150000,               !- Conductivity {W/m-K}",
        "    513.0000,                !- Density {kg/m3}",
        "    1381.000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.7800000,               !- Solar Absorptance",
        "    0.7800000;               !- Visible Absorptance",

        "Material,",
        "    PW03,                    !- Name",
        "    MediumSmooth,            !- Roughness",
        "    1.2700000E-02,           !- Thickness {m}",
        "    0.1150000,               !- Conductivity {W/m-K}",
        "    545.0000,                !- Density {kg/m3}",
        "    1213.000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.7800000,               !- Solar Absorptance",
        "    0.7800000;               !- Visible Absorptance",

        "Material,",
        "    IN02,                    !- Name",
        "    Rough,                   !- Roughness",
        "    9.0099998E-02,           !- Thickness {m}",
        "    4.3000001E-02,           !- Conductivity {W/m-K}",
        "    10.00000,                !- Density {kg/m3}",
        "    837.0000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.7500000,               !- Solar Absorptance",
        "    0.7500000;               !- Visible Absorptance",

        "Material,",
        "    GP01,                    !- Name",
        "    MediumSmooth,            !- Roughness",
        "    1.2700000E-02,           !- Thickness {m}",
        "    0.1600000,               !- Conductivity {W/m-K}",
        "    801.0000,                !- Density {kg/m3}",
        "    837.0000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.7500000,               !- Solar Absorptance",
        "    0.7500000;               !- Visible Absorptance",

        "Material,",
        "    GP02,                    !- Name",
        "    MediumSmooth,            !- Roughness",
        "    1.5900001E-02,           !- Thickness {m}",
        "    0.1600000,               !- Conductivity {W/m-K}",
        "    801.0000,                !- Density {kg/m3}",
        "    837.0000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.7500000,               !- Solar Absorptance",
        "    0.7500000;               !- Visible Absorptance",

        "Material,",
        "    CC03,                    !- Name",
        "    MediumRough,             !- Roughness",
        "    0.1016000,               !- Thickness {m}",
        "    1.310000,                !- Conductivity {W/m-K}",
        "    2243.000,                !- Density {kg/m3}",
        "    837.0000,                !- Specific Heat {J/kg-K}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.6500000,               !- Solar Absorptance",
        "    0.6500000;               !- Visible Absorptance",

        "Material:NoMass,",
        "    CP01,                    !- Name",
        "    Rough,                   !- Roughness",
        "    0.3670000,               !- Thermal Resistance {m2-K/W}",
        "    0.9000000,               !- Thermal Absorptance",
        "    0.7500000,               !- Solar Absorptance",
        "    0.7500000;               !- Visible Absorptance",

        "Material:NoMass,",
        "    MAT-CLNG-1,              !- Name",
        "    Rough,                   !- Roughness",
        "    0.652259290,             !- Thermal Resistance {m2-K/W}",
        "    0.65,                    !- Thermal Absorptance",
        "    0.65,                    !- Solar Absorptance",
        "    0.65;                    !- Visible Absorptance",

        "Material:AirGap,",
        "    AL21,                    !- Name",
        "    0.1570000;               !- Thermal Resistance {m2-K/W}",

        "Material:AirGap,",
        "    AL23,                    !- Name",
        "    0.1530000;               !- Thermal Resistance {m2-K/W}",

        "WindowMaterial:Glazing,",
        "    CLEAR 3MM,               !- Name",
        "    SpectralAverage,         !- Optical Data Type",
        "    ,                        !- Window Glass Spectral Data Set Name",
        "    0.003,                   !- Thickness {m}",
        "    0.837,                   !- Solar Transmittance at Normal Incidence",
        "    0.075,                   !- Front Side Solar Reflectance at Normal Incidence",
        "    0.075,                   !- Back Side Solar Reflectance at Normal Incidence",
        "    0.898,                   !- Visible Transmittance at Normal Incidence",
        "    0.081,                   !- Front Side Visible Reflectance at Normal Incidence",
        "    0.081,                   !- Back Side Visible Reflectance at Normal Incidence",
        "    0.0,                     !- Infrared Transmittance at Normal Incidence",
        "    0.84,                    !- Front Side Infrared Hemispherical Emissivity",
        "    0.84,                    !- Back Side Infrared Hemispherical Emissivity",
        "    0.9;                     !- Conductivity {W/m-K}",

        "WindowMaterial:Glazing,",
        "    GREY 3MM,                !- Name",
        "    SpectralAverage,         !- Optical Data Type",
        "    ,                        !- Window Glass Spectral Data Set Name",
        "    0.003,                   !- Thickness {m}",
        "    0.626,                   !- Solar Transmittance at Normal Incidence",
        "    0.061,                   !- Front Side Solar Reflectance at Normal Incidence",
        "    0.061,                   !- Back Side Solar Reflectance at Normal Incidence",
        "    0.611,                   !- Visible Transmittance at Normal Incidence",
        "    0.061,                   !- Front Side Visible Reflectance at Normal Incidence",
        "    0.061,                   !- Back Side Visible Reflectance at Normal Incidence",
        "    0.0,                     !- Infrared Transmittance at Normal Incidence",
        "    0.84,                    !- Front Side Infrared Hemispherical Emissivity",
        "    0.84,                    !- Back Side Infrared Hemispherical Emissivity",
        "    0.9;                     !- Conductivity {W/m-K}",

        "WindowMaterial:Glazing,",
        "    CLEAR 6MM,               !- Name",
        "    SpectralAverage,         !- Optical Data Type",
        "    ,                        !- Window Glass Spectral Data Set Name",
        "    0.006,                   !- Thickness {m}",
        "    0.775,                   !- Solar Transmittance at Normal Incidence",
        "    0.071,                   !- Front Side Solar Reflectance at Normal Incidence",
        "    0.071,                   !- Back Side Solar Reflectance at Normal Incidence",
        "    0.881,                   !- Visible Transmittance at Normal Incidence",
        "    0.080,                   !- Front Side Visible Reflectance at Normal Incidence",
        "    0.080,                   !- Back Side Visible Reflectance at Normal Incidence",
        "    0.0,                     !- Infrared Transmittance at Normal Incidence",
        "    0.84,                    !- Front Side Infrared Hemispherical Emissivity",
        "    0.84,                    !- Back Side Infrared Hemispherical Emissivity",
        "    0.9;                     !- Conductivity {W/m-K}",

        "WindowMaterial:Glazing,",
        "    LoE CLEAR 6MM,           !- Name",
        "    SpectralAverage,         !- Optical Data Type",
        "    ,                        !- Window Glass Spectral Data Set Name",
        "    0.006,                   !- Thickness {m}",
        "    0.600,                   !- Solar Transmittance at Normal Incidence",
        "    0.170,                   !- Front Side Solar Reflectance at Normal Incidence",
        "    0.220,                   !- Back Side Solar Reflectance at Normal Incidence",
        "    0.840,                   !- Visible Transmittance at Normal Incidence",
        "    0.055,                   !- Front Side Visible Reflectance at Normal Incidence",
        "    0.078,                   !- Back Side Visible Reflectance at Normal Incidence",
        "    0.0,                     !- Infrared Transmittance at Normal Incidence",
        "    0.84,                    !- Front Side Infrared Hemispherical Emissivity",
        "    0.10,                    !- Back Side Infrared Hemispherical Emissivity",
        "    0.9;                     !- Conductivity {W/m-K}",

        "WindowMaterial:Gas,",
        "    AIR 6MM,                 !- Name",
        "    Air,                     !- Gas Type",
        "    0.0063;                  !- Thickness {m}",

        "WindowMaterial:Gas,",
        "    AIR 13MM,                !- Name",
        "    Air,                     !- Gas Type",
        "    0.0127;                  !- Thickness {m}",

        "WindowMaterial:Gas,",
        "    ARGON 13MM,              !- Name",
        "    Argon,                   !- Gas Type",
        "    0.0127;                  !- Thickness {m}",

        "Construction,",
        "    ROOF-1,                  !- Name",
        "    RG01,                    !- Outside Layer",
        "    BR01,                    !- Layer 2",
        "    IN46,                    !- Layer 3",
        "    WD01;                    !- Layer 4",

        "Construction,",
        "    WALL-1,                  !- Name",
        "    WD01,                    !- Outside Layer",
        "    PW03,                    !- Layer 2",
        "    IN02,                    !- Layer 3",
        "    GP01;                    !- Layer 4",

        "Construction,",
        "    CLNG-1,                  !- Name",
        "    MAT-CLNG-1;              !- Outside Layer",

        "Construction,",
        "    FLOOR-SLAB-1,            !- Name",
        "    CC03;                    !- Outside Layer",

        "Construction,",
        "    INT-WALL-1,              !- Name",
        "    GP02,                    !- Outside Layer",
        "    AL21,                    !- Layer 2",
        "    GP02;                    !- Layer 3",

        "Construction,",
        "    Dbl Clr 3mm/13mm Air,    !- Name",
        "    CLEAR 3MM,               !- Outside Layer",
        "    AIR 13MM,                !- Layer 2",
        "    CLEAR 3MM;               !- Layer 3",

        "Construction,",
        "    Sgl Grey 3mm,            !- Name",
        "    GREY 3MM;                !- Outside Layer",

        "GlobalGeometryRules,",
        "    UpperLeftCorner,         !- Starting Vertex Position",
        "    CounterClockWise,        !- Vertex Entry Direction",
        "    relative;                !- Coordinate System",

        "Zone,",
        "    PLENUM-1,                !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0, 0, 0,                            !- X,Y,Z  {m}",
        "    1,                       !- Type",
        "    1,                       !- Multiplier",
        "    0.609600067,             !- Ceiling Height {m}",
        "    283.2;                   !- Volume {m3}",

        "Zone,",
        "    SPACE1-1,                !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0, 0, 0,                            !- X,Y,Z  {m}",
        "    1,                       !- Type",
        "    1,                       !- Multiplier",
        "    2.438400269,             !- Ceiling Height {m}",
        "    239.247360229;           !- Volume {m3}",

        "Zone,",
        "    SPACE2-1,                !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0, 0, 0,                            !- X,Y,Z  {m}",
        "    1,                       !- Type",
        "    1,                       !- Multiplier",
        "    2.438400269,             !- Ceiling Height {m}",
        "    103.311355591;           !- Volume {m3}",

        "Zone,",
        "    SPACE3-1,                !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0, 0, 0,                            !- X,Y,Z  {m}",
        "    1,                       !- Type",
        "    1,                       !- Multiplier",
        "    2.438400269,             !- Ceiling Height {m}",
        "    239.247360229;           !- Volume {m3}",

        "Zone,",
        "    SPACE4-1,                !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0, 0, 0,                            !- X,Y,Z  {m}",
        "    1,                       !- Type",
        "    1,                       !- Multiplier",
        "    2.438400269,             !- Ceiling Height {m}",
        "    103.311355591;           !- Volume {m3}",

        "Zone,",
        "    SPACE5-1,                !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0, 0, 0,                            !- X,Y,Z  {m}",
        "    1,                       !- Type",
        "    1,                       !- Multiplier",
        "    2.438400269,             !- Ceiling Height {m}",
        "    447.682556152;           !- Volume {m3}",

        "BuildingSurface:Detailed,",
        "    WALL-1PF,                !- Name",
        "    WALL,                    !- Surface Type",
        "    WALL-1,                  !- Construction Name",
        "    PLENUM-1,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.50000,                 !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0.0, 0.0, 3.0,                      !- X,Y,Z  1 {m}",
        "    0.0, 0.0, 2.4,                      !- X,Y,Z  2 {m}",
        "    30.5, 0.0, 2.4,                     !- X,Y,Z  3 {m}",
        "    30.5, 0.0, 3.0;                     !- X,Y,Z  4 {m}",

        "BuildingSurface:Detailed,",
        "    WALL-1PR,                !- Name",
        "    WALL,                    !- Surface Type",
        "    WALL-1,                  !- Construction Name",
        "    PLENUM-1,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.50000,                 !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    30.5, 0.0, 3.0,                     !- X,Y,Z  1 {m}",
        "    30.5, 0.0, 2.4,                     !- X,Y,Z  2 {m}",
        "    30.5, 15.2, 2.4,                    !- X,Y,Z  3 {m}",
        "    30.5, 15.2, 3.0;                    !- X,Y,Z  4 {m}",

        "BuildingSurface:Detailed,",
        "    WALL-1PB,                !- Name",
        "    WALL,                    !- Surface Type",
        "    WALL-1,                  !- Construction Name",
        "    PLENUM-1,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.50000,                 !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    30.5, 15.2, 3.0,                    !- X,Y,Z  1 {m}",
        "    30.5, 15.2, 2.4,                    !- X,Y,Z  2 {m}",
        "    0.0, 15.2, 2.4,                     !- X,Y,Z  3 {m}",
        "    0.0, 15.2, 3.0;                     !- X,Y,Z  4 {m}",

        "BuildingSurface:Detailed,",
        "    WALL-1PL,                !- Name",
        "    WALL,                    !- Surface Type",
        "    WALL-1,                  !- Construction Name",
        "    PLENUM-1,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.50000,                 !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0.0, 15.2, 3.0,                     !- X,Y,Z  1 {m}",
        "    0.0, 15.2, 2.4,                     !- X,Y,Z  2 {m}",
        "    0.0, 0.0, 2.4,                      !- X,Y,Z  3 {m}",
        "    0.0, 0.0, 3.0;                      !- X,Y,Z  4 {m}",

        "BuildingSurface:Detailed,",
        "    TOP-1,                   !- Name",
        "    ROOF,                    !- Surface Type",
        "    ROOF-1,                  !- Construction Name",
        "    PLENUM-1,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.00000,                 !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0.0, 15.2, 3.0,                     !- X,Y,Z  1 {m}",
        "    0.0, 0.0, 3.0,                      !- X,Y,Z  2 {m}",
        "    30.5, 0.0, 3.0,                     !- X,Y,Z  3 {m}",
        "    30.5, 15.2, 3.0;                    !- X,Y,Z  4 {m}",

        "BuildingSurface:Detailed,",
        "    C1-1P,                   !- Name",
        "    FLOOR,                   !- Surface Type",
        "    CLNG-1,                  !- Construction Name",
        "    PLENUM-1,                !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    C1-1,                    !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.0,                     !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    26.8, 3.7, 2.4,                     !- X,Y,Z  1 {m}",
        "    30.5, 0.0, 2.4,                     !- X,Y,Z  2 {m}",
        "    0.0, 0.0, 2.4,                      !- X,Y,Z  3 {m}",
        "    3.7, 3.7, 2.4;                      !- X,Y,Z  4 {m}",

        "BuildingSurface:Detailed,",
        "    C2-1P,                   !- Name",
        "    FLOOR,                   !- Surface Type",
        "    CLNG-1,                  !- Construction Name",
        "    PLENUM-1,                !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    C2-1,                    !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.0,                     !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    26.8, 11.6, 2.4,                    !- X,Y,Z  1 {m}",
        "    30.5, 15.2, 2.4,                    !- X,Y,Z  2 {m}",
        "    30.5, 0.0, 2.4,                     !- X,Y,Z  3 {m}",
        "    26.8, 3.7, 2.4;                     !- X,Y,Z  4 {m}",

        "BuildingSurface:Detailed,",
        "    C3-1P,                   !- Name",
        "    FLOOR,                   !- Surface Type",
        "    CLNG-1,                  !- Construction Name",
        "    PLENUM-1,                !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    C3-1,                    !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.0,                     !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    26.8, 11.6, 2.4,                    !- X,Y,Z  1 {m}",
        "    3.7, 11.6, 2.4,                     !- X,Y,Z  2 {m}",
        "    0.0, 15.2, 2.4,                     !- X,Y,Z  3 {m}",
        "    30.5, 15.2, 2.4;                    !- X,Y,Z  4 {m}",

        "BuildingSurface:Detailed,",
        "    C4-1P,                   !- Name",
        "    FLOOR,                   !- Surface Type",
        "    CLNG-1,                  !- Construction Name",
        "    PLENUM-1,                !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    C4-1,                    !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.0,                     !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    3.7, 3.7, 2.4,                      !- X,Y,Z  1 {m}",
        "    0.0, 0.0, 2.4,                      !- X,Y,Z  2 {m}",
        "    0.0, 15.2, 2.4,                     !- X,Y,Z  3 {m}",
        "    3.7, 11.6, 2.4;                     !- X,Y,Z  4 {m}",

        "BuildingSurface:Detailed,",
        "    C5-1P,                   !- Name",
        "    FLOOR,                   !- Surface Type",
        "    CLNG-1,                  !- Construction Name",
        "    PLENUM-1,                !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    C5-1,                    !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.0,                     !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    26.8, 11.6, 2.4,                    !- X,Y,Z  1 {m}",
        "    26.8, 3.7, 2.4,                     !- X,Y,Z  2 {m}",
        "    3.7, 3.7, 2.4,                      !- X,Y,Z  3 {m}",
        "    3.7, 11.6, 2.4;                     !- X,Y,Z  4 {m}",

        "BuildingSurface:Detailed,",
        "    FRONT-1,                 !- Name",
        "    WALL,                    !- Surface Type",
        "    WALL-1,                  !- Construction Name",
        "    SPACE1-1,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.50000,                 !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0.0, 0.0, 2.4,                      !- X,Y,Z  1 {m}",
        "    0.0, 0.0, 0.0,                      !- X,Y,Z  2 {m}",
        "    30.5, 0.0, 0.0,                     !- X,Y,Z  3 {m}",
        "    30.5, 0.0, 2.4;                     !- X,Y,Z  4 {m}",

        "BuildingSurface:Detailed,",
        "    C1-1,                    !- Name",
        "    CEILING,                 !- Surface Type",
        "    CLNG-1,                  !- Construction Name",
        "    SPACE1-1,                !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    C1-1P,                   !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.0,                     !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    3.7, 3.7, 2.4,                      !- X,Y,Z  1 {m}",
        "    0.0, 0.0, 2.4,                      !- X,Y,Z  2 {m}",
        "    30.5, 0.0, 2.4,                     !- X,Y,Z  3 {m}",
        "    26.8, 3.7, 2.4;                     !- X,Y,Z  4 {m}",

        "BuildingSurface:Detailed,",
        "    F1-1,                    !- Name",
        "    FLOOR,                   !- Surface Type",
        "    FLOOR-SLAB-1,            !- Construction Name",
        "    SPACE1-1,                !- Zone Name",
        "    Ground,                  !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.0,                     !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    26.8, 3.7, 0.0,                     !- X,Y,Z  1 {m}",
        "    30.5, 0.0, 0.0,                     !- X,Y,Z  2 {m}",
        "    0.0, 0.0, 0.0,                      !- X,Y,Z  3 {m}",
        "    3.7, 3.7, 0.0;                      !- X,Y,Z  4 {m}",

        "BuildingSurface:Detailed,",
        "    SB12,                    !- Name",
        "    WALL,                    !- Surface Type",
        "    INT-WALL-1,              !- Construction Name",
        "    SPACE1-1,                !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    SB21,                    !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.0,                     !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    30.5, 0.0, 2.4,                     !- X,Y,Z  1 {m}",
        "    30.5, 0.0, 0.0,                     !- X,Y,Z  2 {m}",
        "    26.8, 3.7, 0.0,                     !- X,Y,Z  3 {m}",
        "    26.8, 3.7, 2.4;                     !- X,Y,Z  4 {m}",

        "BuildingSurface:Detailed,",
        "    SB14,                    !- Name",
        "    WALL,                    !- Surface Type",
        "    INT-WALL-1,              !- Construction Name",
        "    SPACE1-1,                !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    SB41,                    !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.0,                     !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    3.7, 3.7, 2.4,                      !- X,Y,Z  1 {m}",
        "    3.7, 3.7, 0.0,                      !- X,Y,Z  2 {m}",
        "    0.0, 0.0, 0.0,                      !- X,Y,Z  3 {m}",
        "    0.0, 0.0, 2.4;                      !- X,Y,Z  4 {m}",

        "BuildingSurface:Detailed,",
        "    SB15,                    !- Name",
        "    WALL,                    !- Surface Type",
        "    INT-WALL-1,              !- Construction Name",
        "    SPACE1-1,                !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    SB51,                    !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.0,                     !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    26.8, 3.7, 2.4,                     !- X,Y,Z  1 {m}",
        "    26.8, 3.7, 0.0,                     !- X,Y,Z  2 {m}",
        "    3.7, 3.7, 0.0,                      !- X,Y,Z  3 {m}",
        "    3.7, 3.7, 2.4;                      !- X,Y,Z  4 {m}",

        "BuildingSurface:Detailed,",
        "    RIGHT-1,                 !- Name",
        "    WALL,                    !- Surface Type",
        "    WALL-1,                  !- Construction Name",
        "    SPACE2-1,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.50000,                 !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    30.5, 0.0, 2.4,                     !- X,Y,Z  1 {m}",
        "    30.5, 0.0, 0.0,                     !- X,Y,Z  2 {m}",
        "    30.5, 15.2, 0.0,                    !- X,Y,Z  3 {m}",
        "    30.5, 15.2, 2.4;                    !- X,Y,Z  4 {m}",

        "BuildingSurface:Detailed,",
        "    C2-1,                    !- Name",
        "    CEILING,                 !- Surface Type",
        "    CLNG-1,                  !- Construction Name",
        "    SPACE2-1,                !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    C2-1P,                   !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.0,                     !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    26.8, 3.7, 2.4,                     !- X,Y,Z  1 {m}",
        "    30.5, 0.0, 2.4,                     !- X,Y,Z  2 {m}",
        "    30.5, 15.2, 2.4,                    !- X,Y,Z  3 {m}",
        "    26.8, 11.6, 2.4;                    !- X,Y,Z  4 {m}",

        "BuildingSurface:Detailed,",
        "    F2-1,                    !- Name",
        "    FLOOR,                   !- Surface Type",
        "    FLOOR-SLAB-1,            !- Construction Name",
        "    SPACE2-1,                !- Zone Name",
        "    Ground,                  !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.0,                     !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    26.8, 11.6, 0.0,                    !- X,Y,Z  1 {m}",
        "    30.5, 15.2, 0.0,                    !- X,Y,Z  2 {m}",
        "    30.5, 0.0, 0.0,                     !- X,Y,Z  3 {m}",
        "    26.8, 3.7, 0.0;                     !- X,Y,Z  4 {m}",

        "BuildingSurface:Detailed,",
        "    SB21,                    !- Name",
        "    WALL,                    !- Surface Type",
        "    INT-WALL-1,              !- Construction Name",
        "    SPACE2-1,                !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    SB12,                    !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.0,                     !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    26.8, 3.7, 2.4,                     !- X,Y,Z  1 {m}",
        "    26.8, 3.7, 0.0,                     !- X,Y,Z  2 {m}",
        "    30.5, 0.0, 0.0,                     !- X,Y,Z  3 {m}",
        "    30.5, 0.0, 2.4;                     !- X,Y,Z  4 {m}",

        "BuildingSurface:Detailed,",
        "    SB23,                    !- Name",
        "    WALL,                    !- Surface Type",
        "    INT-WALL-1,              !- Construction Name",
        "    SPACE2-1,                !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    SB32,                    !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.0,                     !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    30.5, 15.2, 2.4,                    !- X,Y,Z  1 {m}",
        "    30.5, 15.2, 0.0,                    !- X,Y,Z  2 {m}",
        "    26.8, 11.6, 0.0,                    !- X,Y,Z  3 {m}",
        "    26.8, 11.6, 2.4;                    !- X,Y,Z  4 {m}",

        "BuildingSurface:Detailed,",
        "    SB25,                    !- Name",
        "    WALL,                    !- Surface Type",
        "    INT-WALL-1,              !- Construction Name",
        "    SPACE2-1,                !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    SB52,                    !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.0,                     !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    26.8, 11.6, 2.4,                    !- X,Y,Z  1 {m}",
        "    26.8, 11.6, 0.0,                    !- X,Y,Z  2 {m}",
        "    26.8, 3.7, 0.0,                     !- X,Y,Z  3 {m}",
        "    26.8, 3.7, 2.4;                     !- X,Y,Z  4 {m}",

        "BuildingSurface:Detailed,",
        "    BACK-1,                  !- Name",
        "    WALL,                    !- Surface Type",
        "    WALL-1,                  !- Construction Name",
        "    SPACE3-1,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.50000,                 !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    30.5, 15.2, 2.4,                    !- X,Y,Z  1 {m}",
        "    30.5, 15.2, 0.0,                    !- X,Y,Z  2 {m}",
        "    0.0, 15.2, 0.0,                     !- X,Y,Z  3 {m}",
        "    0.0, 15.2, 2.4;                     !- X,Y,Z  4 {m}",

        "BuildingSurface:Detailed,",
        "    C3-1,                    !- Name",
        "    CEILING,                 !- Surface Type",
        "    CLNG-1,                  !- Construction Name",
        "    SPACE3-1,                !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    C3-1P,                   !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.0,                     !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    30.5, 15.2, 2.4,                    !- X,Y,Z  1 {m}",
        "    0.0, 15.2, 2.4,                     !- X,Y,Z  2 {m}",
        "    3.7, 11.6, 2.4,                     !- X,Y,Z  3 {m}",
        "    26.8, 11.6, 2.4;                    !- X,Y,Z  4 {m}",

        "BuildingSurface:Detailed,",
        "    F3-1,                    !- Name",
        "    FLOOR,                   !- Surface Type",
        "    FLOOR-SLAB-1,            !- Construction Name",
        "    SPACE3-1,                !- Zone Name",
        "    Ground,                  !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.0,                     !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    26.8, 11.6, 0.0,                    !- X,Y,Z  1 {m}",
        "    3.7, 11.6, 0.0,                     !- X,Y,Z  2 {m}",
        "    0.0, 15.2, 0.0,                     !- X,Y,Z  3 {m}",
        "    30.5, 15.2, 0.0;                    !- X,Y,Z  4 {m}",

        "BuildingSurface:Detailed,",
        "    SB32,                    !- Name",
        "    WALL,                    !- Surface Type",
        "    INT-WALL-1,              !- Construction Name",
        "    SPACE3-1,                !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    SB23,                    !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.0,                     !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    26.8, 11.6, 2.4,                    !- X,Y,Z  1 {m}",
        "    26.8, 11.6, 0.0,                    !- X,Y,Z  2 {m}",
        "    30.5, 15.2, 0.0,                    !- X,Y,Z  3 {m}",
        "    30.5, 15.2, 2.4;                    !- X,Y,Z  4 {m}",

        "BuildingSurface:Detailed,",
        "    SB34,                    !- Name",
        "    WALL,                    !- Surface Type",
        "    INT-WALL-1,              !- Construction Name",
        "    SPACE3-1,                !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    SB43,                    !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.0,                     !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0.0, 15.2, 2.4,                     !- X,Y,Z  1 {m}",
        "    0.0, 15.2, 0.0,                     !- X,Y,Z  2 {m}",
        "    3.7, 11.6, 0.0,                     !- X,Y,Z  3 {m}",
        "    3.7, 11.6, 2.4;                     !- X,Y,Z  4 {m}",

        "BuildingSurface:Detailed,",
        "    SB35,                    !- Name",
        "    WALL,                    !- Surface Type",
        "    INT-WALL-1,              !- Construction Name",
        "    SPACE3-1,                !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    SB53,                    !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.0,                     !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    3.7, 11.6, 2.4,                     !- X,Y,Z  1 {m}",
        "    3.7, 11.6, 0.0,                     !- X,Y,Z  2 {m}",
        "    26.8, 11.6, 0.0,                    !- X,Y,Z  3 {m}",
        "    26.8, 11.6, 2.4;                    !- X,Y,Z  4 {m}",

        "BuildingSurface:Detailed,",
        "    LEFT-1,                  !- Name",
        "    WALL,                    !- Surface Type",
        "    WALL-1,                  !- Construction Name",
        "    SPACE4-1,                !- Zone Name",
        "    Outdoors,                !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    SunExposed,              !- Sun Exposure",
        "    WindExposed,             !- Wind Exposure",
        "    0.50000,                 !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0.0, 15.2, 2.4,                     !- X,Y,Z  1 {m}",
        "    0.0, 15.2, 0.0,                     !- X,Y,Z  2 {m}",
        "    0.0, 0.0, 0.0,                      !- X,Y,Z  3 {m}",
        "    0.0, 0.0, 2.4;                      !- X,Y,Z  4 {m}",

        "BuildingSurface:Detailed,",
        "    C4-1,                    !- Name",
        "    CEILING,                 !- Surface Type",
        "    CLNG-1,                  !- Construction Name",
        "    SPACE4-1,                !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    C4-1P,                   !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.0,                     !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    3.7, 11.6, 2.4,                     !- X,Y,Z  1 {m}",
        "    0.0, 15.2, 2.4,                     !- X,Y,Z  2 {m}",
        "    0.0, 0.0, 2.4,                      !- X,Y,Z  3 {m}",
        "    3.7, 3.7, 2.4;                      !- X,Y,Z  4 {m}",

        "BuildingSurface:Detailed,",
        "    F4-1,                    !- Name",
        "    FLOOR,                   !- Surface Type",
        "    FLOOR-SLAB-1,            !- Construction Name",
        "    SPACE4-1,                !- Zone Name",
        "    Ground,                  !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.0,                     !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    3.7, 3.7, 0.0,                      !- X,Y,Z  1 {m}",
        "    0.0, 0.0, 0.0,                      !- X,Y,Z  2 {m}",
        "    0.0, 15.2, 0.0,                     !- X,Y,Z  3 {m}",
        "    3.7, 11.6, 0.0;                     !- X,Y,Z  4 {m}",

        "BuildingSurface:Detailed,",
        "    SB41,                    !- Name",
        "    WALL,                    !- Surface Type",
        "    INT-WALL-1,              !- Construction Name",
        "    SPACE4-1,                !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    SB14,                    !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.0,                     !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    0.0, 0.0, 2.4,                      !- X,Y,Z  1 {m}",
        "    0.0, 0.0, 0.0,                      !- X,Y,Z  2 {m}",
        "    3.7, 3.7, 0.0,                      !- X,Y,Z  3 {m}",
        "    3.7, 3.7, 2.4;                      !- X,Y,Z  4 {m}",

        "BuildingSurface:Detailed,",
        "    SB43,                    !- Name",
        "    WALL,                    !- Surface Type",
        "    INT-WALL-1,              !- Construction Name",
        "    SPACE4-1,                !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    SB34,                    !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.0,                     !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    3.7, 11.6, 2.4,                     !- X,Y,Z  1 {m}",
        "    3.7, 11.6, 0.0,                     !- X,Y,Z  2 {m}",
        "    0.0, 15.2, 0.0,                     !- X,Y,Z  3 {m}",
        "    0.0, 15.2, 2.4;                     !- X,Y,Z  4 {m}",

        "BuildingSurface:Detailed,",
        "    SB45,                    !- Name",
        "    WALL,                    !- Surface Type",
        "    INT-WALL-1,              !- Construction Name",
        "    SPACE4-1,                !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    SB54,                    !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.0,                     !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    3.7, 3.7, 2.4,                      !- X,Y,Z  1 {m}",
        "    3.7, 3.7, 0.0,                      !- X,Y,Z  2 {m}",
        "    3.7, 11.6, 0.0,                     !- X,Y,Z  3 {m}",
        "    3.7, 11.6, 2.4;                     !- X,Y,Z  4 {m}",

        "BuildingSurface:Detailed,",
        "    C5-1,                    !- Name",
        "    CEILING,                 !- Surface Type",
        "    CLNG-1,                  !- Construction Name",
        "    SPACE5-1,                !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    C5-1P,                   !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.0,                     !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    3.7, 11.6, 2.4,                     !- X,Y,Z  1 {m}",
        "    3.7, 3.7, 2.4,                      !- X,Y,Z  2 {m}",
        "    26.8, 3.7, 2.4,                     !- X,Y,Z  3 {m}",
        "    26.8, 11.6, 2.4;                    !- X,Y,Z  4 {m}",

        "BuildingSurface:Detailed,",
        "    F5-1,                    !- Name",
        "    FLOOR,                   !- Surface Type",
        "    FLOOR-SLAB-1,            !- Construction Name",
        "    SPACE5-1,                !- Zone Name",
        "    Ground,                  !- Outside Boundary Condition",
        "    ,                        !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.0,                     !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    26.8, 11.6, 0.0,                    !- X,Y,Z  1 {m}",
        "    26.8, 3.7, 0.0,                     !- X,Y,Z  2 {m}",
        "    3.7, 3.7, 0.0,                      !- X,Y,Z  3 {m}",
        "    3.7, 11.6, 0.0;                     !- X,Y,Z  4 {m}",

        "BuildingSurface:Detailed,",
        "    SB51,                    !- Name",
        "    WALL,                    !- Surface Type",
        "    INT-WALL-1,              !- Construction Name",
        "    SPACE5-1,                !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    SB15,                    !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.0,                     !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    3.7, 3.7, 2.4,                      !- X,Y,Z  1 {m}",
        "    3.7, 3.7, 0.0,                      !- X,Y,Z  2 {m}",
        "    26.8, 3.7, 0.0,                     !- X,Y,Z  3 {m}",
        "    26.8, 3.7, 2.4;                     !- X,Y,Z  4 {m}",

        "BuildingSurface:Detailed,",
        "    SB52,                    !- Name",
        "    WALL,                    !- Surface Type",
        "    INT-WALL-1,              !- Construction Name",
        "    SPACE5-1,                !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    SB25,                    !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.0,                     !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    26.8, 3.7, 2.4,                     !- X,Y,Z  1 {m}",
        "    26.8, 3.7, 0.0,                     !- X,Y,Z  2 {m}",
        "    26.8, 11.6, 0.0,                    !- X,Y,Z  3 {m}",
        "    26.8, 11.6, 2.4;                    !- X,Y,Z  4 {m}",

        "BuildingSurface:Detailed,",
        "    SB53,                    !- Name",
        "    WALL,                    !- Surface Type",
        "    INT-WALL-1,              !- Construction Name",
        "    SPACE5-1,                !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    SB35,                    !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.0,                     !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    26.8, 11.6, 2.4,                    !- X,Y,Z  1 {m}",
        "    26.8, 11.6, 0.0,                    !- X,Y,Z  2 {m}",
        "    3.7, 11.6, 0.0,                     !- X,Y,Z  3 {m}",
        "    3.7, 11.6, 2.4;                     !- X,Y,Z  4 {m}",

        "BuildingSurface:Detailed,",
        "    SB54,                    !- Name",
        "    WALL,                    !- Surface Type",
        "    INT-WALL-1,              !- Construction Name",
        "    SPACE5-1,                !- Zone Name",
        "    Surface,                 !- Outside Boundary Condition",
        "    SB45,                    !- Outside Boundary Condition Object",
        "    NoSun,                   !- Sun Exposure",
        "    NoWind,                  !- Wind Exposure",
        "    0.0,                     !- View Factor to Ground",
        "    4,                       !- Number of Vertices",
        "    3.7, 11.6, 2.4,                     !- X,Y,Z  1 {m}",
        "    3.7, 11.6, 0.0,                     !- X,Y,Z  2 {m}",
        "    3.7, 3.7, 0.0,                      !- X,Y,Z  3 {m}",
        "    3.7, 3.7, 2.4;                      !- X,Y,Z  4 {m}",

        "FenestrationSurface:Detailed,",
        "    WF-1,                    !- Name",
        "    WINDOW,                  !- Surface Type",
        "    Dbl Clr 3mm/13mm Air,    !- Construction Name",
        "    FRONT-1,                 !- Building Surface Name",
        "    ,                        !- Outside Boundary Condition Object",
        "    0.50000,                 !- View Factor to Ground",
        "    ,                        !- Frame and Divider Name",
        "    1,                       !- Multiplier",
        "    4,                       !- Number of Vertices",
        "    3.0, 0.0, 2.1,                      !- X,Y,Z  1 {m}",
        "    3.0, 0.0, 0.9,                      !- X,Y,Z  2 {m}",
        "    16.8, 0.0, 0.9,                     !- X,Y,Z  3 {m}",
        "    16.8, 0.0, 2.1;                     !- X,Y,Z  4 {m}",

        "FenestrationSurface:Detailed,",
        "    DF-1,                    !- Name",
        "    GLASSDOOR,               !- Surface Type",
        "    Sgl Grey 3mm,            !- Construction Name",
        "    FRONT-1,                 !- Building Surface Name",
        "    ,                        !- Outside Boundary Condition Object",
        "    0.50000,                 !- View Factor to Ground",
        "    ,                        !- Frame and Divider Name",
        "    1,                       !- Multiplier",
        "    4,                       !- Number of Vertices",
        "    21.3, 0.0, 2.1,                     !- X,Y,Z  1 {m}",
        "    21.3, 0.0, 0.0,                     !- X,Y,Z  2 {m}",
        "    23.8, 0.0, 0.0,                     !- X,Y,Z  3 {m}",
        "    23.8, 0.0, 2.1;                     !- X,Y,Z  4 {m}",

        "FenestrationSurface:Detailed,",
        "    WR-1,                    !- Name",
        "    WINDOW,                  !- Surface Type",
        "    Dbl Clr 3mm/13mm Air,    !- Construction Name",
        "    RIGHT-1,                 !- Building Surface Name",
        "    ,                        !- Outside Boundary Condition Object",
        "    0.50000,                 !- View Factor to Ground",
        "    ,                        !- Frame and Divider Name",
        "    1,                       !- Multiplier",
        "    4,                       !- Number of Vertices",
        "    30.5, 3.8, 2.1,                     !- X,Y,Z  1 {m}",
        "    30.5, 3.8, 0.9,                     !- X,Y,Z  2 {m}",
        "    30.5, 11.4, 0.9,                    !- X,Y,Z  3 {m}",
        "    30.5, 11.4, 2.1;                    !- X,Y,Z  4 {m}",

        "FenestrationSurface:Detailed,",
        "    WB-1,                    !- Name",
        "    WINDOW,                  !- Surface Type",
        "    Dbl Clr 3mm/13mm Air,    !- Construction Name",
        "    BACK-1,                  !- Building Surface Name",
        "    ,                        !- Outside Boundary Condition Object",
        "    0.50000,                 !- View Factor to Ground",
        "    ,                        !- Frame and Divider Name",
        "    1,                       !- Multiplier",
        "    4,                       !- Number of Vertices",
        "    27.4, 15.2, 2.1,                    !- X,Y,Z  1 {m}",
        "    27.4, 15.2, 0.9,                    !- X,Y,Z  2 {m}",
        "    13.7, 15.2, 0.9,                    !- X,Y,Z  3 {m}",
        "    13.7, 15.2, 2.1;                    !- X,Y,Z  4 {m}",

        "FenestrationSurface:Detailed,",
        "    DB-1,                    !- Name",
        "    GLASSDOOR,               !- Surface Type",
        "    Sgl Grey 3mm,            !- Construction Name",
        "    BACK-1,                  !- Building Surface Name",
        "    ,                        !- Outside Boundary Condition Object",
        "    0.50000,                 !- View Factor to Ground",
        "    ,                        !- Frame and Divider Name",
        "    1,                       !- Multiplier",
        "    4,                       !- Number of Vertices",
        "    9.1, 15.2, 2.1,                     !- X,Y,Z  1 {m}",
        "    9.1, 15.2, 0.0,                     !- X,Y,Z  2 {m}",
        "    7.0, 15.2, 0.0,                     !- X,Y,Z  3 {m}",
        "    7.0, 15.2, 2.1;                     !- X,Y,Z  4 {m}",

        "FenestrationSurface:Detailed,",
        "    WL-1,                    !- Name",
        "    WINDOW,                  !- Surface Type",
        "    Dbl Clr 3mm/13mm Air,    !- Construction Name",
        "    LEFT-1,                  !- Building Surface Name",
        "    ,                        !- Outside Boundary Condition Object",
        "    0.50000,                 !- View Factor to Ground",
        "    ,                        !- Frame and Divider Name",
        "    1,                       !- Multiplier",
        "    4,                       !- Number of Vertices",
        "    0.0, 11.4, 2.1,                     !- X,Y,Z  1 {m}",
        "    0.0, 11.4, 0.9,                     !- X,Y,Z  2 {m}",
        "    0.0, 3.8, 0.9,                      !- X,Y,Z  3 {m}",
        "    0.0, 3.8, 2.1;                      !- X,Y,Z  4 {m}",

        "Shading:Zone:Detailed,",
        "    Main South Overhang,     !- Name",
        "    FRONT-1,                 !- Base Surface Name",
        "    ShadeTransSch,           !- Transmittance Schedule Name",
        "    4,                       !- Number of Vertices",
        "    0.0, -1.3, 2.2,                     !- X,Y,Z  1 {m}",
        "    0.0, 0.0, 2.2,                      !- X,Y,Z  2 {m}",
        "    19.8, 0.0, 2.2,                     !- X,Y,Z  3 {m}",
        "    19.8, -1.3, 2.2;                    !- X,Y,Z  4 {m}",

        "Shading:Zone:Detailed,",
        "    South Door Overhang,     !- Name",
        "    FRONT-1,                 !- Base Surface Name",
        "    ShadeTransSch,           !- Transmittance Schedule Name",
        "    4,                       !- Number of Vertices",
        "    21.0, -2.0, 2.6,                    !- X,Y,Z  1 {m}",
        "    21.0, 0.0, 2.6,                     !- X,Y,Z  2 {m}",
        "    24.1, 0.0, 2.6,                     !- X,Y,Z  3 {m}",
        "    24.1, -2.0, 2.6;                    !- X,Y,Z  4 {m}",

        "People,",
        "    SPACE1-1 People 1,       !- Name",
        "    SPACE1-1,                !- Zone or ZoneList Name",
        "    OCCUPY-1,                !- Number of People Schedule Name",
        "    people,                  !- Number of People Calculation Method",
        "    11,                      !- Number of People",
        "    ,                        !- People per Zone Floor Area {person/m2}",
        "    ,                        !- Zone Floor Area per Person {m2/person}",
        "    0.3,                     !- Fraction Radiant",
        "    ,                        !- Sensible Heat Fraction",
        "    ActSchd;                 !- Activity Level Schedule Name",

        "People,",
        "    SPACE2-1 People 1,       !- Name",
        "    SPACE2-1,                !- Zone or ZoneList Name",
        "    OCCUPY-1,                !- Number of People Schedule Name",
        "    people,                  !- Number of People Calculation Method",
        "    5,                       !- Number of People",
        "    ,                        !- People per Zone Floor Area {person/m2}",
        "    ,                        !- Zone Floor Area per Person {m2/person}",
        "    0.3,                     !- Fraction Radiant",
        "    ,                        !- Sensible Heat Fraction",
        "    ActSchd;                 !- Activity Level Schedule Name",

        "People,",
        "    SPACE3-1 People 1,       !- Name",
        "    SPACE3-1,                !- Zone or ZoneList Name",
        "    OCCUPY-1,                !- Number of People Schedule Name",
        "    people,                  !- Number of People Calculation Method",
        "    11,                      !- Number of People",
        "    ,                        !- People per Zone Floor Area {person/m2}",
        "    ,                        !- Zone Floor Area per Person {m2/person}",
        "    0.3,                     !- Fraction Radiant",
        "    ,                        !- Sensible Heat Fraction",
        "    ActSchd;                 !- Activity Level Schedule Name",

        "People,",
        "    SPACE4-1 People 1,       !- Name",
        "    SPACE4-1,                !- Zone or ZoneList Name",
        "    OCCUPY-1,                !- Number of People Schedule Name",
        "    people,                  !- Number of People Calculation Method",
        "    5,                       !- Number of People",
        "    ,                        !- People per Zone Floor Area {person/m2}",
        "    ,                        !- Zone Floor Area per Person {m2/person}",
        "    0.3,                     !- Fraction Radiant",
        "    ,                        !- Sensible Heat Fraction",
        "    ActSchd;                 !- Activity Level Schedule Name",

        "People,",
        "    SPACE5-1 People 1,       !- Name",
        "    SPACE5-1,                !- Zone or ZoneList Name",
        "    OCCUPY-1,                !- Number of People Schedule Name",
        "    people,                  !- Number of People Calculation Method",
        "    20,                      !- Number of People",
        "    ,                        !- People per Zone Floor Area {person/m2}",
        "    ,                        !- Zone Floor Area per Person {m2/person}",
        "    0.3,                     !- Fraction Radiant",
        "    ,                        !- Sensible Heat Fraction",
        "    ActSchd;                 !- Activity Level Schedule Name",

        "Lights,",
        "    SPACE1-1 Lights 1,       !- Name",
        "    SPACE1-1,                !- Zone or ZoneList Name",
        "    LIGHTS-1,                !- Schedule Name",
        "    LightingLevel,           !- Design Level Calculation Method",
        "    1584,                    !- Lighting Level {W}",
        "    ,                        !- Watts per Zone Floor Area {W/m2}",
        "    ,                        !- Watts per Person {W/person}",
        "    0,                       !- Return Air Fraction",
        "    0.59,                    !- Fraction Radiant",
        "    0.2,                     !- Fraction Visible",
        "    0,                       !- Fraction Replaceable",
        "    GeneralLights;           !- End-Use Subcategory",

        "Lights,",
        "    SPACE2-1 Lights 1,       !- Name",
        "    SPACE2-1,                !- Zone or ZoneList Name",
        "    LIGHTS-1,                !- Schedule Name",
        "    LightingLevel,           !- Design Level Calculation Method",
        "    684,                     !- Lighting Level {W}",
        "    ,                        !- Watts per Zone Floor Area {W/m2}",
        "    ,                        !- Watts per Person {W/person}",
        "    0,                       !- Return Air Fraction",
        "    0.59,                    !- Fraction Radiant",
        "    0.2,                     !- Fraction Visible",
        "    0,                       !- Fraction Replaceable",
        "    GeneralLights;           !- End-Use Subcategory",

        "Lights,",
        "    SPACE3-1 Lights 1,       !- Name",
        "    SPACE3-1,                !- Zone or ZoneList Name",
        "    LIGHTS-1,                !- Schedule Name",
        "    LightingLevel,           !- Design Level Calculation Method",
        "    1584,                    !- Lighting Level {W}",
        "    ,                        !- Watts per Zone Floor Area {W/m2}",
        "    ,                        !- Watts per Person {W/person}",
        "    0,                       !- Return Air Fraction",
        "    0.59,                    !- Fraction Radiant",
        "    0.2,                     !- Fraction Visible",
        "    0,                       !- Fraction Replaceable",
        "    GeneralLights;           !- End-Use Subcategory",

        "Lights,",
        "    SPACE4-1 Lights 1,       !- Name",
        "    SPACE4-1,                !- Zone or ZoneList Name",
        "    LIGHTS-1,                !- Schedule Name",
        "    LightingLevel,           !- Design Level Calculation Method",
        "    684,                     !- Lighting Level {W}",
        "    ,                        !- Watts per Zone Floor Area {W/m2}",
        "    ,                        !- Watts per Person {W/person}",
        "    0,                       !- Return Air Fraction",
        "    0.59,                    !- Fraction Radiant",
        "    0.2,                     !- Fraction Visible",
        "    0,                       !- Fraction Replaceable",
        "    GeneralLights;           !- End-Use Subcategory",

        "Lights,",
        "    SPACE5-1 Lights 1,       !- Name",
        "    SPACE5-1,                !- Zone or ZoneList Name",
        "    LIGHTS-1,                !- Schedule Name",
        "    LightingLevel,           !- Design Level Calculation Method",
        "    2964,                    !- Lighting Level {W}",
        "    ,                        !- Watts per Zone Floor Area {W/m2}",
        "    ,                        !- Watts per Person {W/person}",
        "    0,                       !- Return Air Fraction",
        "    0.59,                    !- Fraction Radiant",
        "    0.2,                     !- Fraction Visible",
        "    0,                       !- Fraction Replaceable",
        "    GeneralLights;           !- End-Use Subcategory",

        "ElectricEquipment,",
        "    SPACE1-1 ElecEq 1,       !- Name",
        "    SPACE1-1,                !- Zone or ZoneList Name",
        "    EQUIP-1,                 !- Schedule Name",
        "    EquipmentLevel,          !- Design Level Calculation Method",
        "    1056,                    !- Design Level {W}",
        "    ,                        !- Watts per Zone Floor Area {W/m2}",
        "    ,                        !- Watts per Person {W/person}",
        "    0,                       !- Fraction Latent",
        "    0.3,                     !- Fraction Radiant",
        "    0;                       !- Fraction Lost",

        "ElectricEquipment,",
        "    SPACE2-1 ElecEq 1,       !- Name",
        "    SPACE2-1,                !- Zone or ZoneList Name",
        "    EQUIP-1,                 !- Schedule Name",
        "    EquipmentLevel,          !- Design Level Calculation Method",
        "    456,                     !- Design Level {W}",
        "    ,                        !- Watts per Zone Floor Area {W/m2}",
        "    ,                        !- Watts per Person {W/person}",
        "    0,                       !- Fraction Latent",
        "    0.3,                     !- Fraction Radiant",
        "    0;                       !- Fraction Lost",

        "ElectricEquipment,",
        "    SPACE3-1 ElecEq 1,       !- Name",
        "    SPACE3-1,                !- Zone or ZoneList Name",
        "    EQUIP-1,                 !- Schedule Name",
        "    EquipmentLevel,          !- Design Level Calculation Method",
        "    1056,                    !- Design Level {W}",
        "    ,                        !- Watts per Zone Floor Area {W/m2}",
        "    ,                        !- Watts per Person {W/person}",
        "    0,                       !- Fraction Latent",
        "    0.3,                     !- Fraction Radiant",
        "    0;                       !- Fraction Lost",

        "ElectricEquipment,",
        "    SPACE4-1 ElecEq 1,       !- Name",
        "    SPACE4-1,                !- Zone or ZoneList Name",
        "    EQUIP-1,                 !- Schedule Name",
        "    EquipmentLevel,          !- Design Level Calculation Method",
        "    456,                     !- Design Level {W}",
        "    ,                        !- Watts per Zone Floor Area {W/m2}",
        "    ,                        !- Watts per Person {W/person}",
        "    0,                       !- Fraction Latent",
        "    0.3,                     !- Fraction Radiant",
        "    0;                       !- Fraction Lost",

        "ElectricEquipment,",
        "    SPACE5-1 ElecEq 1,       !- Name",
        "    SPACE5-1,                !- Zone or ZoneList Name",
        "    EQUIP-1,                 !- Schedule Name",
        "    EquipmentLevel,          !- Design Level Calculation Method",
        "    1976,                    !- Design Level {W}",
        "    ,                        !- Watts per Zone Floor Area {W/m2}",
        "    ,                        !- Watts per Person {W/person}",
        "    0,                       !- Fraction Latent",
        "    0.3,                     !- Fraction Radiant",
        "    0;                       !- Fraction Lost",

        "ZoneInfiltration:DesignFlowRate,",
        "    SPACE1-1 Infil 1,        !- Name",
        "    SPACE1-1,                !- Zone or ZoneList Name",
        "    INFIL-SCH,               !- Schedule Name",
        "    flow/zone,               !- Design Flow Rate Calculation Method",
        "    0.0167,                  !- Design Flow Rate {m3/s}",
        "    ,                        !- Flow per Zone Floor Area {m3/s-m2}",
        "    ,                        !- Flow per Exterior Surface Area {m3/s-m2}",
        "    ,                        !- Air Changes per Hour {1/hr}",
        "    0,                       !- Constant Term Coefficient",
        "    0,                       !- Temperature Term Coefficient",
        "    0.2237,                  !- Velocity Term Coefficient",
        "    0;                       !- Velocity Squared Term Coefficient",

        "ZoneInfiltration:DesignFlowRate,",
        "    SPACE2-1 Infil 1,        !- Name",
        "    SPACE2-1,                !- Zone or ZoneList Name",
        "    INFIL-SCH,               !- Schedule Name",
        "    flow/zone,               !- Design Flow Rate Calculation Method",
        "    0.00717,                 !- Design Flow Rate {m3/s}",
        "    ,                        !- Flow per Zone Floor Area {m3/s-m2}",
        "    ,                        !- Flow per Exterior Surface Area {m3/s-m2}",
        "    ,                        !- Air Changes per Hour {1/hr}",
        "    0,                       !- Constant Term Coefficient",
        "    0,                       !- Temperature Term Coefficient",
        "    0.2237,                  !- Velocity Term Coefficient",
        "    0;                       !- Velocity Squared Term Coefficient",

        "ZoneInfiltration:DesignFlowRate,",
        "    SPACE3-1 Infil 1,        !- Name",
        "    SPACE3-1,                !- Zone or ZoneList Name",
        "    INFIL-SCH,               !- Schedule Name",
        "    flow/zone,               !- Design Flow Rate Calculation Method",
        "    0.0167,                  !- Design Flow Rate {m3/s}",
        "    ,                        !- Flow per Zone Floor Area {m3/s-m2}",
        "    ,                        !- Flow per Exterior Surface Area {m3/s-m2}",
        "    ,                        !- Air Changes per Hour {1/hr}",
        "    0,                       !- Constant Term Coefficient",
        "    0,                       !- Temperature Term Coefficient",
        "    0.2237,                  !- Velocity Term Coefficient",
        "    0;                       !- Velocity Squared Term Coefficient",

        "ZoneInfiltration:DesignFlowRate,",
        "    SPACE4-1 Infil 1,        !- Name",
        "    SPACE4-1,                !- Zone or ZoneList Name",
        "    INFIL-SCH,               !- Schedule Name",
        "    flow/zone,               !- Design Flow Rate Calculation Method",
        "    0.00717,                 !- Design Flow Rate {m3/s}",
        "    ,                        !- Flow per Zone Floor Area {m3/s-m2}",
        "    ,                        !- Flow per Exterior Surface Area {m3/s-m2}",
        "    ,                        !- Air Changes per Hour {1/hr}",
        "    0,                       !- Constant Term Coefficient",
        "    0,                       !- Temperature Term Coefficient",
        "    0.2237,                  !- Velocity Term Coefficient",
        "    0;                       !- Velocity Squared Term Coefficient",

        "ZoneInfiltration:DesignFlowRate,",
        "    SPACE5-1 Infil 1,        !- Name",
        "    SPACE5-1,                !- Zone or ZoneList Name",
        "    INFIL-SCH,               !- Schedule Name",
        "    flow/zone,               !- Design Flow Rate Calculation Method",
        "    0.031089,                !- Design Flow Rate {m3/s}",
        "    ,                        !- Flow per Zone Floor Area {m3/s-m2}",
        "    ,                        !- Flow per Exterior Surface Area {m3/s-m2}",
        "    ,                        !- Air Changes per Hour {1/hr}",
        "    0,                       !- Constant Term Coefficient",
        "    0,                       !- Temperature Term Coefficient",
        "    0.2237,                  !- Velocity Term Coefficient",
        "    0;                       !- Velocity Squared Term Coefficient",

        "DesignSpecification:OutdoorAir,",
        "    SZ DSOA SPACE1-1,        !- Name",
        "    flow/person,             !- Outdoor Air Method",
        "    0.00944,                 !- Outdoor Air Flow per Person {m3/s-person}",
        "    0.0,                     !- Outdoor Air Flow per Zone Floor Area {m3/s-m2}",
        "    0.0;                     !- Outdoor Air Flow per Zone {m3/s}",

        "DesignSpecification:OutdoorAir,",
        "    SZ DSOA SPACE2-1,        !- Name",
        "    flow/person,             !- Outdoor Air Method",
        "    0.00944,                 !- Outdoor Air Flow per Person {m3/s-person}",
        "    0.0,                     !- Outdoor Air Flow per Zone Floor Area {m3/s-m2}",
        "    0.0;                     !- Outdoor Air Flow per Zone {m3/s}",

        "DesignSpecification:OutdoorAir,",
        "    SZ DSOA SPACE3-1,        !- Name",
        "    flow/person,             !- Outdoor Air Method",
        "    0.00944,                 !- Outdoor Air Flow per Person {m3/s-person}",
        "    0.0,                     !- Outdoor Air Flow per Zone Floor Area {m3/s-m2}",
        "    0.0;                     !- Outdoor Air Flow per Zone {m3/s}",

        "DesignSpecification:OutdoorAir,",
        "    SZ DSOA SPACE4-1,        !- Name",
        "    flow/person,             !- Outdoor Air Method",
        "    0.00944,                 !- Outdoor Air Flow per Person {m3/s-person}",
        "    0.0,                     !- Outdoor Air Flow per Zone Floor Area {m3/s-m2}",
        "    0.0;                     !- Outdoor Air Flow per Zone {m3/s}",

        "DesignSpecification:OutdoorAir,",
        "    SZ DSOA SPACE5-1,        !- Name",
        "    flow/person,             !- Outdoor Air Method",
        "    0.00944,                 !- Outdoor Air Flow per Person {m3/s-person}",
        "    0.0,                     !- Outdoor Air Flow per Zone Floor Area {m3/s-m2}",
        "    0.0;                     !- Outdoor Air Flow per Zone {m3/s}",

        "Sizing:Zone,",
        "    SPACE1-1,                !- Zone or ZoneList Name",
        "    SupplyAirTemperature,    !- Zone Cooling Design Supply Air Temperature Input Method",
        "    12.5,                    !- Zone Cooling Design Supply Air Temperature {C}",
        "    11.11,                   !- Zone Cooling Design Supply Air Temperature Difference {deltaC}",
        "    SupplyAirTemperature,    !- Zone Heating Design Supply Air Temperature Input Method",
        "    50,                      !- Zone Heating Design Supply Air Temperature {C}",
        "    ,                        !- Zone Heating Design Supply Air Temperature Difference {deltaC}",
        "    0.008,                   !- Zone Cooling Design Supply Air Humidity Ratio {kgWater/kgDryAir}",
        "    0.008,                   !- Zone Heating Design Supply Air Humidity Ratio {kgWater/kgDryAir}",
        "    SZ DSOA SPACE1-1,        !- Design Specification Outdoor Air Object Name",
        "    ,                        !- Zone Heating Sizing Factor",
        "    ,                        !- Zone Cooling Sizing Factor",
        "    DesignDay,               !- Cooling Design Air Flow Method",
        "    0,                       !- Cooling Design Air Flow Rate {m3/s}",
        "    ,                        !- Cooling Minimum Air Flow per Zone Floor Area {m3/s-m2}",
        "    ,                        !- Cooling Minimum Air Flow {m3/s}",
        "    0,                       !- Cooling Minimum Air Flow Fraction",
        "    DesignDay,               !- Heating Design Air Flow Method",
        "    0,                       !- Heating Design Air Flow Rate {m3/s}",
        "    ,                        !- Heating Maximum Air Flow per Zone Floor Area {m3/s-m2}",
        "    ,                        !- Heating Maximum Air Flow {m3/s}",
        "    0,                       !- Heating Maximum Air Flow Fraction",
        "    ,                        !- Design Specification Zone Air Distribution Object Name",
        "    Yes,                     !- Account for Dedicated Outside Air System",
        "    ColdSupplyAir,           !- Dedicated Outside Air System Control Strategy",
        "    12.8,                    !- Dedicated Outside Air Low Setpoint for Design",
        "    15.6;                    !- Dedicated Outside Air High Setpoint for Design",

        "Sizing:Zone,",
        "    SPACE2-1,                !- Zone or ZoneList Name",
        "    SupplyAirTemperature,    !- Zone Cooling Design Supply Air Temperature Input Method",
        "    12.5,                    !- Zone Cooling Design Supply Air Temperature {C}",
        "    11.11,                   !- Zone Cooling Design Supply Air Temperature Difference {deltaC}",
        "    SupplyAirTemperature,    !- Zone Heating Design Supply Air Temperature Input Method",
        "    50,                      !- Zone Heating Design Supply Air Temperature {C}",
        "    ,                        !- Zone Heating Design Supply Air Temperature Difference {deltaC}",
        "    0.008,                   !- Zone Cooling Design Supply Air Humidity Ratio {kgWater/kgDryAir}",
        "    0.008,                   !- Zone Heating Design Supply Air Humidity Ratio {kgWater/kgDryAir}",
        "    SZ DSOA SPACE2-1,        !- Design Specification Outdoor Air Object Name",
        "    ,                        !- Zone Heating Sizing Factor",
        "    ,                        !- Zone Cooling Sizing Factor",
        "    DesignDay,               !- Cooling Design Air Flow Method",
        "    0,                       !- Cooling Design Air Flow Rate {m3/s}",
        "    ,                        !- Cooling Minimum Air Flow per Zone Floor Area {m3/s-m2}",
        "    ,                        !- Cooling Minimum Air Flow {m3/s}",
        "    0,                       !- Cooling Minimum Air Flow Fraction",
        "    DesignDay,               !- Heating Design Air Flow Method",
        "    0,                       !- Heating Design Air Flow Rate {m3/s}",
        "    ,                        !- Heating Maximum Air Flow per Zone Floor Area {m3/s-m2}",
        "    ,                        !- Heating Maximum Air Flow {m3/s}",
        "    0,                       !- Heating Maximum Air Flow Fraction",
        "    ,                        !- Design Specification Zone Air Distribution Object Name",
        "    Yes,                     !- Account for Dedicated Outside Air System",
        "    ColdSupplyAir,           !- Dedicated Outside Air System Control Strategy",
        "    12.8,                    !- Dedicated Outside Air Low Setpoint for Design",
        "    15.6;                    !- Dedicated Outside Air High Setpoint for Design",

        "Sizing:Zone,",
        "    SPACE3-1,                !- Zone or ZoneList Name",
        "    SupplyAirTemperature,    !- Zone Cooling Design Supply Air Temperature Input Method",
        "    12.5,                    !- Zone Cooling Design Supply Air Temperature {C}",
        "    11.11,                   !- Zone Cooling Design Supply Air Temperature Difference {deltaC}",
        "    SupplyAirTemperature,    !- Zone Heating Design Supply Air Temperature Input Method",
        "    50,                      !- Zone Heating Design Supply Air Temperature {C}",
        "    ,                        !- Zone Heating Design Supply Air Temperature Difference {deltaC}",
        "    0.008,                   !- Zone Cooling Design Supply Air Humidity Ratio {kgWater/kgDryAir}",
        "    0.008,                   !- Zone Heating Design Supply Air Humidity Ratio {kgWater/kgDryAir}",
        "    SZ DSOA SPACE3-1,        !- Design Specification Outdoor Air Object Name",
        "    ,                        !- Zone Heating Sizing Factor",
        "    ,                        !- Zone Cooling Sizing Factor",
        "    DesignDay,               !- Cooling Design Air Flow Method",
        "    0,                       !- Cooling Design Air Flow Rate {m3/s}",
        "    ,                        !- Cooling Minimum Air Flow per Zone Floor Area {m3/s-m2}",
        "    ,                        !- Cooling Minimum Air Flow {m3/s}",
        "    0,                       !- Cooling Minimum Air Flow Fraction",
        "    DesignDay,               !- Heating Design Air Flow Method",
        "    0,                       !- Heating Design Air Flow Rate {m3/s}",
        "    ,                        !- Heating Maximum Air Flow per Zone Floor Area {m3/s-m2}",
        "    ,                        !- Heating Maximum Air Flow {m3/s}",
        "    0,                       !- Heating Maximum Air Flow Fraction",
        "    ,                        !- Design Specification Zone Air Distribution Object Name",
        "    Yes,                     !- Account for Dedicated Outside Air System",
        "    ColdSupplyAir,           !- Dedicated Outside Air System Control Strategy",
        "    12.8,                    !- Dedicated Outside Air Low Setpoint for Design",
        "    15.6;                    !- Dedicated Outside Air High Setpoint for Design",

        "Sizing:Zone,",
        "    SPACE4-1,                !- Zone or ZoneList Name",
        "    SupplyAirTemperature,    !- Zone Cooling Design Supply Air Temperature Input Method",
        "    12.5,                    !- Zone Cooling Design Supply Air Temperature {C}",
        "    11.11,                   !- Zone Cooling Design Supply Air Temperature Difference {deltaC}",
        "    SupplyAirTemperature,    !- Zone Heating Design Supply Air Temperature Input Method",
        "    50,                      !- Zone Heating Design Supply Air Temperature {C}",
        "    ,                        !- Zone Heating Design Supply Air Temperature Difference {deltaC}",
        "    0.008,                   !- Zone Cooling Design Supply Air Humidity Ratio {kgWater/kgDryAir}",
        "    0.008,                   !- Zone Heating Design Supply Air Humidity Ratio {kgWater/kgDryAir}",
        "    SZ DSOA SPACE4-1,        !- Design Specification Outdoor Air Object Name",
        "    ,                        !- Zone Heating Sizing Factor",
        "    ,                        !- Zone Cooling Sizing Factor",
        "    DesignDay,               !- Cooling Design Air Flow Method",
        "    0,                       !- Cooling Design Air Flow Rate {m3/s}",
        "    ,                        !- Cooling Minimum Air Flow per Zone Floor Area {m3/s-m2}",
        "    ,                        !- Cooling Minimum Air Flow {m3/s}",
        "    0,                       !- Cooling Minimum Air Flow Fraction",
        "    DesignDay,               !- Heating Design Air Flow Method",
        "    0,                       !- Heating Design Air Flow Rate {m3/s}",
        "    ,                        !- Heating Maximum Air Flow per Zone Floor Area {m3/s-m2}",
        "    ,                        !- Heating Maximum Air Flow {m3/s}",
        "    0,                       !- Heating Maximum Air Flow Fraction",
        "    ,                        !- Design Specification Zone Air Distribution Object Name",
        "    Yes,                     !- Account for Dedicated Outside Air System",
        "    ColdSupplyAir,           !- Dedicated Outside Air System Control Strategy",
        "    12.8,                    !- Dedicated Outside Air Low Setpoint for Design",
        "    15.6;                    !- Dedicated Outside Air High Setpoint for Design",

        "Sizing:Zone,",
        "    SPACE5-1,                !- Zone or ZoneList Name",
        "    SupplyAirTemperature,    !- Zone Cooling Design Supply Air Temperature Input Method",
        "    12.5,                    !- Zone Cooling Design Supply Air Temperature {C}",
        "    11.11,                   !- Zone Cooling Design Supply Air Temperature Difference {deltaC}",
        "    SupplyAirTemperature,    !- Zone Heating Design Supply Air Temperature Input Method",
        "    50,                      !- Zone Heating Design Supply Air Temperature {C}",
        "    ,                        !- Zone Heating Design Supply Air Temperature Difference {deltaC}",
        "    0.008,                   !- Zone Cooling Design Supply Air Humidity Ratio {kgWater/kgDryAir}",
        "    0.008,                   !- Zone Heating Design Supply Air Humidity Ratio {kgWater/kgDryAir}",
        "    SZ DSOA SPACE5-1,        !- Design Specification Outdoor Air Object Name",
        "    ,                        !- Zone Heating Sizing Factor",
        "    ,                        !- Zone Cooling Sizing Factor",
        "    DesignDay,               !- Cooling Design Air Flow Method",
        "    0,                       !- Cooling Design Air Flow Rate {m3/s}",
        "    ,                        !- Cooling Minimum Air Flow per Zone Floor Area {m3/s-m2}",
        "    ,                        !- Cooling Minimum Air Flow {m3/s}",
        "    0,                       !- Cooling Minimum Air Flow Fraction",
        "    DesignDay,               !- Heating Design Air Flow Method",
        "    0,                       !- Heating Design Air Flow Rate {m3/s}",
        "    ,                        !- Heating Maximum Air Flow per Zone Floor Area {m3/s-m2}",
        "    ,                        !- Heating Maximum Air Flow {m3/s}",
        "    0,                       !- Heating Maximum Air Flow Fraction",
        "    ,                        !- Design Specification Zone Air Distribution Object Name",
        "    Yes,                     !- Account for Dedicated Outside Air System",
        "    ColdSupplyAir,           !- Dedicated Outside Air System Control Strategy",
        "    12.8,                    !- Dedicated Outside Air Low Setpoint for Design",
        "    15.6;                    !- Dedicated Outside Air High Setpoint for Design",

        "Sizing:System,",
        "    DOAS,                    !- AirLoop Name",
        "    VentilationRequirement,  !- Type of Load to Size On",
        "    autosize,                !- Design Outdoor Air Flow Rate {m3/s}",
        "    1.0,                     !- Central Heating Maximum System Air Flow Ratio",
        "    2,                       !- Preheat Design Temperature {C}",
        "    0.008,                   !- Preheat Design Humidity Ratio {kgWater/kgDryAir}",
        "    11,                      !- Precool Design Temperature {C}",
        "    0.008,                   !- Precool Design Humidity Ratio {kgWater/kgDryAir}",
        "    12.8,                    !- Central Cooling Design Supply Air Temperature {C}",
        "    12.2,                    !- Central Heating Design Supply Air Temperature {C}",
        "    NonCoincident,           !- Type of Zone Sum to Use",
        "    Yes,                     !- 100% Outdoor Air in Cooling",
        "    Yes,                     !- 100% Outdoor Air in Heating",
        "    0.00924,                 !- Central Cooling Design Supply Air Humidity Ratio {kgWater/kgDryAir}",
        "    0.003,                   !- Central Heating Design Supply Air Humidity Ratio {kgWater/kgDryAir}",
        "    DesignDay,               !- Cooling Supply Air Flow Rate Method",
        "    0,                       !- Cooling Supply Air Flow Rate {m3/s}",
        "    ,                        !- Cooling Supply Air Flow Rate Per Floor Area {m3/s-m2}",
        "    ,                        !- Cooling Fraction of Autosized Cooling Supply Air Flow Rate",
        "    ,                        !- Cooling Supply Air Flow Rate Per Unit Cooling Capacity {m3/s-W}",
        "    DesignDay,               !- Heating Supply Air Flow Rate Method",
        "    0,                       !- Heating Supply Air Flow Rate {m3/s}",
        "    ,                        !- Heating Supply Air Flow Rate Per Floor Area {m3/s-m2}",
        "    ,                        !- Heating Fraction of Autosized Heating Supply Air Flow Rate",
        "    ,                        !- Heating Fraction of Autosized Cooling Supply Air Flow Rate",
        "    ,                        !- Heating Supply Air Flow Rate Per Unit Heating Capacity {m3/s-W}",
        "    ZoneSum,                 !- System Outdoor Air Method",
        "    1.0,                     !- Zone Maximum Outdoor Air Fraction {dimensionless}",
        "    CoolingDesignCapacity,   !- Cooling Design Capacity Method",
        "    autosize,                !- Cooling Design Capacity {W}",
        "    ,                        !- Cooling Design Capacity Per Floor Area {W/m2}",
        "    ,                        !- Fraction of Autosized Cooling Design Capacity",
        "    HeatingDesignCapacity,   !- Heating Design Capacity Method",
        "    autosize,                !- Heating Design Capacity {W}",
        "    ,                        !- Heating Design Capacity Per Floor Area {W/m2}",
        "    ,                        !- Fraction of Autosized Heating Design Capacity",
        "    OnOff;                   !- Central Cooling Capacity Control Method",

        "Sizing:Plant,",
        "    Hot Water Loop Hot Water Loop,  !- Plant or Condenser Loop Name",
        "    Heating,                 !- Loop Type",
        "    82,                      !- Design Loop Exit Temperature {C}",
        "    11.0;                    !- Loop Design Temperature Difference {deltaC}",

        "Sizing:Plant,",
        "    Chilled Water Loop Chilled Water Loop,  !- Plant or Condenser Loop Name",
        "    Cooling,                 !- Loop Type",
        "    7.22,                    !- Design Loop Exit Temperature {C}",
        "    6.67;                    !- Loop Design Temperature Difference {deltaC}",

        "ZoneControl:Thermostat,",
        "    SPACE1-1 Thermostat,     !- Name",
        "    SPACE1-1,                !- Zone or ZoneList Name",
        "    HVACTemplate-Always 4,   !- Control Type Schedule Name",
        "    ThermostatSetpoint:DualSetpoint,  !- Control 1 Object Type",
        "    All Zones Dual SP Control;  !- Control 1 Name",

        "ZoneControl:Thermostat,",
        "    SPACE2-1 Thermostat,     !- Name",
        "    SPACE2-1,                !- Zone or ZoneList Name",
        "    HVACTemplate-Always 4,   !- Control Type Schedule Name",
        "    ThermostatSetpoint:DualSetpoint,  !- Control 1 Object Type",
        "    All Zones Dual SP Control;  !- Control 1 Name",

        "ZoneControl:Thermostat,",
        "    SPACE3-1 Thermostat,     !- Name",
        "    SPACE3-1,                !- Zone or ZoneList Name",
        "    HVACTemplate-Always 4,   !- Control Type Schedule Name",
        "    ThermostatSetpoint:DualSetpoint,  !- Control 1 Object Type",
        "    All Zones Dual SP Control;  !- Control 1 Name",

        "ZoneControl:Thermostat,",
        "    SPACE4-1 Thermostat,     !- Name",
        "    SPACE4-1,                !- Zone or ZoneList Name",
        "    HVACTemplate-Always 4,   !- Control Type Schedule Name",
        "    ThermostatSetpoint:DualSetpoint,  !- Control 1 Object Type",
        "    All Zones Dual SP Control;  !- Control 1 Name",

        "ZoneControl:Thermostat,",
        "    SPACE5-1 Thermostat,     !- Name",
        "    SPACE5-1,                !- Zone or ZoneList Name",
        "    HVACTemplate-Always 4,   !- Control Type Schedule Name",
        "    ThermostatSetpoint:DualSetpoint,  !- Control 1 Object Type",
        "    All Zones Dual SP Control;  !- Control 1 Name",

        "ThermostatSetpoint:DualSetpoint,",
        "    All Zones Dual SP Control,  !- Name",
        "    Htg-SetP-Sch,            !- Heating Setpoint Temperature Schedule Name",
        "    Clg-SetP-Sch;            !- Cooling Setpoint Temperature Schedule Name",

        "ZoneHVAC:FourPipeFanCoil,",
        "    SPACE1-1 Fan Coil,       !- Name",
        "    FanAvailSched,           !- Availability Schedule Name",
        "    CyclingFan,              !- Capacity Control Method",
        "    autosize,                !- Maximum Supply Air Flow Rate {m3/s}",
        "    0.33,                    !- Low Speed Supply Air Flow Ratio",
        "    0.66,                    !- Medium Speed Supply Air Flow Ratio",
        "    0.0,                     !- Maximum Outdoor Air Flow Rate {m3/s}",
        "    ,                        !- Outdoor Air Schedule Name",
        "    SPACE1-1 Fan Coil Return,!- Air Inlet Node Name",
        "    SPACE1-1 Supply Inlet,   !- Air Outlet Node Name",
        "    OutdoorAir:Mixer,        !- Outdoor Air Mixer Object Type",
        "    SPACE1-1 OA Mixing Box,  !- Outdoor Air Mixer Name",
        "    Fan:OnOff,               !- Supply Air Fan Object Type",
        "    SPACE1-1 Supply Fan,     !- Supply Air Fan Name",
        "    Coil:Cooling:Water,      !- Cooling Coil Object Type",
        "    SPACE1-1 Cooling Coil,   !- Cooling Coil Name",
        "    autosize,                !- Maximum Cold Water Flow Rate {m3/s}",
        "    0,                       !- Minimum Cold Water Flow Rate {m3/s}",
        "    0.001,                   !- Cooling Convergence Tolerance",
        "    Coil:Heating:Water,      !- Heating Coil Object Type",
        "    SPACE1-1 Heating Coil,   !- Heating Coil Name",
        "    autosize,                !- Maximum Hot Water Flow Rate {m3/s}",
        "    0,                       !- Minimum Hot Water Flow Rate {m3/s}",
        "    0.001;                   !- Heating Convergence Tolerance",

        "ZoneHVAC:FourPipeFanCoil,",
        "    SPACE2-1 Fan Coil,       !- Name",
        "    FanAvailSched,           !- Availability Schedule Name",
        "    CyclingFan,              !- Capacity Control Method",
        "    autosize,                !- Maximum Supply Air Flow Rate {m3/s}",
        "    0.33,                    !- Low Speed Supply Air Flow Ratio",
        "    0.66,                    !- Medium Speed Supply Air Flow Ratio",
        "    0.0,                     !- Maximum Outdoor Air Flow Rate {m3/s}",
        "    ,                        !- Outdoor Air Schedule Name",
        "    SPACE2-1 Fan Coil Return,!- Air Inlet Node Name",
        "    SPACE2-1 Supply Inlet,   !- Air Outlet Node Name",
        "    OutdoorAir:Mixer,        !- Outdoor Air Mixer Object Type",
        "    SPACE2-1 OA Mixing Box,  !- Outdoor Air Mixer Name",
        "    Fan:OnOff,               !- Supply Air Fan Object Type",
        "    SPACE2-1 Supply Fan,     !- Supply Air Fan Name",
        "    Coil:Cooling:Water,      !- Cooling Coil Object Type",
        "    SPACE2-1 Cooling Coil,   !- Cooling Coil Name",
        "    autosize,                !- Maximum Cold Water Flow Rate {m3/s}",
        "    0,                       !- Minimum Cold Water Flow Rate {m3/s}",
        "    0.001,                   !- Cooling Convergence Tolerance",
        "    Coil:Heating:Water,      !- Heating Coil Object Type",
        "    SPACE2-1 Heating Coil,   !- Heating Coil Name",
        "    autosize,                !- Maximum Hot Water Flow Rate {m3/s}",
        "    0,                       !- Minimum Hot Water Flow Rate {m3/s}",
        "    0.001;                   !- Heating Convergence Tolerance",

        "ZoneHVAC:FourPipeFanCoil,",
        "    SPACE3-1 Fan Coil,       !- Name",
        "    FanAvailSched,           !- Availability Schedule Name",
        "    CyclingFan,              !- Capacity Control Method",
        "    autosize,                !- Maximum Supply Air Flow Rate {m3/s}",
        "    0.33,                    !- Low Speed Supply Air Flow Ratio",
        "    0.66,                    !- Medium Speed Supply Air Flow Ratio",
        "    0.0,                     !- Maximum Outdoor Air Flow Rate {m3/s}",
        "    ,                        !- Outdoor Air Schedule Name",
        "    SPACE3-1 Fan Coil Return,!- Air Inlet Node Name",
        "    SPACE3-1 Supply Inlet,   !- Air Outlet Node Name",
        "    OutdoorAir:Mixer,        !- Outdoor Air Mixer Object Type",
        "    SPACE3-1 OA Mixing Box,  !- Outdoor Air Mixer Name",
        "    Fan:OnOff,               !- Supply Air Fan Object Type",
        "    SPACE3-1 Supply Fan,     !- Supply Air Fan Name",
        "    Coil:Cooling:Water,      !- Cooling Coil Object Type",
        "    SPACE3-1 Cooling Coil,   !- Cooling Coil Name",
        "    autosize,                !- Maximum Cold Water Flow Rate {m3/s}",
        "    0,                       !- Minimum Cold Water Flow Rate {m3/s}",
        "    0.001,                   !- Cooling Convergence Tolerance",
        "    Coil:Heating:Water,      !- Heating Coil Object Type",
        "    SPACE3-1 Heating Coil,   !- Heating Coil Name",
        "    autosize,                !- Maximum Hot Water Flow Rate {m3/s}",
        "    0,                       !- Minimum Hot Water Flow Rate {m3/s}",
        "    0.001;                   !- Heating Convergence Tolerance",

        "ZoneHVAC:FourPipeFanCoil,",
        "    SPACE4-1 Fan Coil,       !- Name",
        "    FanAvailSched,           !- Availability Schedule Name",
        "    CyclingFan,              !- Capacity Control Method",
        "    autosize,                !- Maximum Supply Air Flow Rate {m3/s}",
        "    0.33,                    !- Low Speed Supply Air Flow Ratio",
        "    0.66,                    !- Medium Speed Supply Air Flow Ratio",
        "    0.0,                     !- Maximum Outdoor Air Flow Rate {m3/s}",
        "    ,                        !- Outdoor Air Schedule Name",
        "    SPACE4-1 Fan Coil Return,!- Air Inlet Node Name",
        "    SPACE4-1 Supply Inlet,   !- Air Outlet Node Name",
        "    OutdoorAir:Mixer,        !- Outdoor Air Mixer Object Type",
        "    SPACE4-1 OA Mixing Box,  !- Outdoor Air Mixer Name",
        "    Fan:OnOff,               !- Supply Air Fan Object Type",
        "    SPACE4-1 Supply Fan,     !- Supply Air Fan Name",
        "    Coil:Cooling:Water,      !- Cooling Coil Object Type",
        "    SPACE4-1 Cooling Coil,   !- Cooling Coil Name",
        "    autosize,                !- Maximum Cold Water Flow Rate {m3/s}",
        "    0,                       !- Minimum Cold Water Flow Rate {m3/s}",
        "    0.001,                   !- Cooling Convergence Tolerance",
        "    Coil:Heating:Water,      !- Heating Coil Object Type",
        "    SPACE4-1 Heating Coil,   !- Heating Coil Name",
        "    autosize,                !- Maximum Hot Water Flow Rate {m3/s}",
        "    0,                       !- Minimum Hot Water Flow Rate {m3/s}",
        "    0.001;                   !- Heating Convergence Tolerance",

        "ZoneHVAC:FourPipeFanCoil,",
        "    SPACE5-1 Fan Coil,       !- Name",
        "    FanAvailSched,           !- Availability Schedule Name",
        "    CyclingFan,              !- Capacity Control Method",
        "    autosize,                !- Maximum Supply Air Flow Rate {m3/s}",
        "    0.33,                    !- Low Speed Supply Air Flow Ratio",
        "    0.66,                    !- Medium Speed Supply Air Flow Ratio",
        "    0.0,                     !- Maximum Outdoor Air Flow Rate {m3/s}",
        "    ,                        !- Outdoor Air Schedule Name",
        "    SPACE5-1 Fan Coil Return,!- Air Inlet Node Name",
        "    SPACE5-1 Supply Inlet,   !- Air Outlet Node Name",
        "    OutdoorAir:Mixer,        !- Outdoor Air Mixer Object Type",
        "    SPACE5-1 OA Mixing Box,  !- Outdoor Air Mixer Name",
        "    Fan:OnOff,               !- Supply Air Fan Object Type",
        "    SPACE5-1 Supply Fan,     !- Supply Air Fan Name",
        "    Coil:Cooling:Water,      !- Cooling Coil Object Type",
        "    SPACE5-1 Cooling Coil,   !- Cooling Coil Name",
        "    autosize,                !- Maximum Cold Water Flow Rate {m3/s}",
        "    0,                       !- Minimum Cold Water Flow Rate {m3/s}",
        "    0.001,                   !- Cooling Convergence Tolerance",
        "    Coil:Heating:Water,      !- Heating Coil Object Type",
        "    SPACE5-1 Heating Coil,   !- Heating Coil Name",
        "    autosize,                !- Maximum Hot Water Flow Rate {m3/s}",
        "    0,                       !- Minimum Hot Water Flow Rate {m3/s}",
        "    0.001;                   !- Heating Convergence Tolerance",

        "AirTerminal:SingleDuct:VAV:NoReheat,",
        "    SPACE1-1 DOAS Air Terminal,  !- Name",
        "    HVACTemplate-Always 1,   !- Availability Schedule Name",
        "    SPACE1-1 DOAS Supply Inlet,  !- Air Outlet Node Name",
        "    SPACE1-1 Zone Equip Inlet,  !- Air Inlet Node Name",
        "    autosize,                !- Maximum Air Flow Rate {m3/s}",
        "    Constant,                !- Zone Minimum Air Flow Input Method",
        "    1.0;                     !- Constant Minimum Air Flow Fraction",

        "AirTerminal:SingleDuct:VAV:NoReheat,",
        "    SPACE2-1 DOAS Air Terminal,  !- Name",
        "    HVACTemplate-Always 1,   !- Availability Schedule Name",
        "    SPACE2-1 DOAS Supply Inlet,  !- Air Outlet Node Name",
        "    SPACE2-1 Zone Equip Inlet,  !- Air Inlet Node Name",
        "    autosize,                !- Maximum Air Flow Rate {m3/s}",
        "    Constant,                !- Zone Minimum Air Flow Input Method",
        "    1.0;                     !- Constant Minimum Air Flow Fraction",

        "AirTerminal:SingleDuct:VAV:NoReheat,",
        "    SPACE3-1 DOAS Air Terminal,  !- Name",
        "    HVACTemplate-Always 1,   !- Availability Schedule Name",
        "    SPACE3-1 DOAS Supply Inlet,  !- Air Outlet Node Name",
        "    SPACE3-1 Zone Equip Inlet,  !- Air Inlet Node Name",
        "    autosize,                !- Maximum Air Flow Rate {m3/s}",
        "    Constant,                !- Zone Minimum Air Flow Input Method",
        "    1.0;                     !- Constant Minimum Air Flow Fraction",

        "AirTerminal:SingleDuct:VAV:NoReheat,",
        "    SPACE4-1 DOAS Air Terminal,  !- Name",
        "    HVACTemplate-Always 1,   !- Availability Schedule Name",
        "    SPACE4-1 DOAS Supply Inlet,  !- Air Outlet Node Name",
        "    SPACE4-1 Zone Equip Inlet,  !- Air Inlet Node Name",
        "    autosize,                !- Maximum Air Flow Rate {m3/s}",
        "    Constant,                !- Zone Minimum Air Flow Input Method",
        "    1.0;                     !- Constant Minimum Air Flow Fraction",

        "AirTerminal:SingleDuct:VAV:NoReheat,",
        "    SPACE5-1 DOAS Air Terminal,  !- Name",
        "    HVACTemplate-Always 1,   !- Availability Schedule Name",
        "    SPACE5-1 DOAS Supply Inlet,  !- Air Outlet Node Name",
        "    SPACE5-1 Zone Equip Inlet,  !- Air Inlet Node Name",
        "    autosize,                !- Maximum Air Flow Rate {m3/s}",
        "    Constant,                !- Zone Minimum Air Flow Input Method",
        "    1.0;                     !- Constant Minimum Air Flow Fraction",

        "ZoneHVAC:AirDistributionUnit,",
        "    SPACE1-1 DOAS ATU,       !- Name",
        "    SPACE1-1 DOAS Supply Inlet,  !- Air Distribution Unit Outlet Node Name",
        "    AirTerminal:SingleDuct:VAV:NoReheat,  !- Air Terminal Object Type",
        "    SPACE1-1 DOAS Air Terminal;  !- Air Terminal Name",

        "ZoneHVAC:AirDistributionUnit,",
        "    SPACE2-1 DOAS ATU,       !- Name",
        "    SPACE2-1 DOAS Supply Inlet,  !- Air Distribution Unit Outlet Node Name",
        "    AirTerminal:SingleDuct:VAV:NoReheat,  !- Air Terminal Object Type",
        "    SPACE2-1 DOAS Air Terminal;  !- Air Terminal Name",

        "ZoneHVAC:AirDistributionUnit,",
        "    SPACE3-1 DOAS ATU,       !- Name",
        "    SPACE3-1 DOAS Supply Inlet,  !- Air Distribution Unit Outlet Node Name",
        "    AirTerminal:SingleDuct:VAV:NoReheat,  !- Air Terminal Object Type",
        "    SPACE3-1 DOAS Air Terminal;  !- Air Terminal Name",

        "ZoneHVAC:AirDistributionUnit,",
        "    SPACE4-1 DOAS ATU,       !- Name",
        "    SPACE4-1 DOAS Supply Inlet,  !- Air Distribution Unit Outlet Node Name",
        "    AirTerminal:SingleDuct:VAV:NoReheat,  !- Air Terminal Object Type",
        "    SPACE4-1 DOAS Air Terminal;  !- Air Terminal Name",

        "ZoneHVAC:AirDistributionUnit,",
        "    SPACE5-1 DOAS ATU,       !- Name",
        "    SPACE5-1 DOAS Supply Inlet,  !- Air Distribution Unit Outlet Node Name",
        "    AirTerminal:SingleDuct:VAV:NoReheat,  !- Air Terminal Object Type",
        "    SPACE5-1 DOAS Air Terminal;  !- Air Terminal Name",

        "ZoneHVAC:EquipmentList,",
        "    SPACE1-1 Equipment,      !- Name",
        "    SequentialLoad,          !- Load Distribution Scheme",
        "    ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 1 Object Type",
        "    SPACE1-1 DOAS ATU,       !- Zone Equipment 1 Name",
        "    1,                       !- Zone Equipment 1 Cooling Sequence",
        "    1,                       !- Zone Equipment 1 Heating or No-Load Sequence",
        "    ZoneHVAC:FourPipeFanCoil,!- Zone Equipment 2 Object Type",
        "    SPACE1-1 Fan Coil,       !- Zone Equipment 2 Name",
        "    2,                       !- Zone Equipment 2 Cooling Sequence",
        "    2;                       !- Zone Equipment 2 Heating or No-Load Sequence",

        "ZoneHVAC:EquipmentList,",
        "    SPACE2-1 Equipment,      !- Name",
        "    SequentialLoad,          !- Load Distribution Scheme",
        "    ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 1 Object Type",
        "    SPACE2-1 DOAS ATU,       !- Zone Equipment 1 Name",
        "    1,                       !- Zone Equipment 1 Cooling Sequence",
        "    1,                       !- Zone Equipment 1 Heating or No-Load Sequence",
        "    ZoneHVAC:FourPipeFanCoil,!- Zone Equipment 2 Object Type",
        "    SPACE2-1 Fan Coil,       !- Zone Equipment 2 Name",
        "    2,                       !- Zone Equipment 2 Cooling Sequence",
        "    2;                       !- Zone Equipment 2 Heating or No-Load Sequence",

        "ZoneHVAC:EquipmentList,",
        "    SPACE3-1 Equipment,      !- Name",
        "    SequentialLoad,          !- Load Distribution Scheme",
        "    ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 1 Object Type",
        "    SPACE3-1 DOAS ATU,       !- Zone Equipment 1 Name",
        "    1,                       !- Zone Equipment 1 Cooling Sequence",
        "    1,                       !- Zone Equipment 1 Heating or No-Load Sequence",
        "    ZoneHVAC:FourPipeFanCoil,!- Zone Equipment 2 Object Type",
        "    SPACE3-1 Fan Coil,       !- Zone Equipment 2 Name",
        "    2,                       !- Zone Equipment 2 Cooling Sequence",
        "    2;                       !- Zone Equipment 2 Heating or No-Load Sequence",

        "ZoneHVAC:EquipmentList,",
        "    SPACE4-1 Equipment,      !- Name",
        "    SequentialLoad,          !- Load Distribution Scheme",
        "    ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 1 Object Type",
        "    SPACE4-1 DOAS ATU,       !- Zone Equipment 1 Name",
        "    1,                       !- Zone Equipment 1 Cooling Sequence",
        "    1,                       !- Zone Equipment 1 Heating or No-Load Sequence",
        "    ZoneHVAC:FourPipeFanCoil,!- Zone Equipment 2 Object Type",
        "    SPACE4-1 Fan Coil,       !- Zone Equipment 2 Name",
        "    2,                       !- Zone Equipment 2 Cooling Sequence",
        "    2;                       !- Zone Equipment 2 Heating or No-Load Sequence",

        "ZoneHVAC:EquipmentList,",
        "    SPACE5-1 Equipment,      !- Name",
        "    SequentialLoad,          !- Load Distribution Scheme",
        "    ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 1 Object Type",
        "    SPACE5-1 DOAS ATU,       !- Zone Equipment 1 Name",
        "    1,                       !- Zone Equipment 1 Cooling Sequence",
        "    1,                       !- Zone Equipment 1 Heating or No-Load Sequence",
        "    ZoneHVAC:FourPipeFanCoil,!- Zone Equipment 2 Object Type",
        "    SPACE5-1 Fan Coil,       !- Zone Equipment 2 Name",
        "    2,                       !- Zone Equipment 2 Cooling Sequence",
        "    2;                       !- Zone Equipment 2 Heating or No-Load Sequence",

        "ZoneHVAC:EquipmentConnections,",
        "    SPACE1-1,                !- Zone Name",
        "    SPACE1-1 Equipment,      !- Zone Conditioning Equipment List Name",
        "    SPACE1-1 Inlets,         !- Zone Air Inlet Node or NodeList Name",
        "    SPACE1-1 Fan Coil Return,!- Zone Air Exhaust Node or NodeList Name",
        "    SPACE1-1 Zone Air Node,  !- Zone Air Node Name",
        "    SPACE1-1 Return Outlet;  !- Zone Return Air Node Name",

        "ZoneHVAC:EquipmentConnections,",
        "    SPACE2-1,                !- Zone Name",
        "    SPACE2-1 Equipment,      !- Zone Conditioning Equipment List Name",
        "    SPACE2-1 Inlets,         !- Zone Air Inlet Node or NodeList Name",
        "    SPACE2-1 Fan Coil Return,!- Zone Air Exhaust Node or NodeList Name",
        "    SPACE2-1 Zone Air Node,  !- Zone Air Node Name",
        "    SPACE2-1 Return Outlet;  !- Zone Return Air Node Name",

        "ZoneHVAC:EquipmentConnections,",
        "    SPACE3-1,                !- Zone Name",
        "    SPACE3-1 Equipment,      !- Zone Conditioning Equipment List Name",
        "    SPACE3-1 Inlets,         !- Zone Air Inlet Node or NodeList Name",
        "    SPACE3-1 Fan Coil Return,!- Zone Air Exhaust Node or NodeList Name",
        "    SPACE3-1 Zone Air Node,  !- Zone Air Node Name",
        "    SPACE3-1 Return Outlet;  !- Zone Return Air Node Name",

        "ZoneHVAC:EquipmentConnections,",
        "    SPACE4-1,                !- Zone Name",
        "    SPACE4-1 Equipment,      !- Zone Conditioning Equipment List Name",
        "    SPACE4-1 Inlets,         !- Zone Air Inlet Node or NodeList Name",
        "    SPACE4-1 Fan Coil Return,!- Zone Air Exhaust Node or NodeList Name",
        "    SPACE4-1 Zone Air Node,  !- Zone Air Node Name",
        "    SPACE4-1 Return Outlet;  !- Zone Return Air Node Name",

        "ZoneHVAC:EquipmentConnections,",
        "    SPACE5-1,                !- Zone Name",
        "    SPACE5-1 Equipment,      !- Zone Conditioning Equipment List Name",
        "    SPACE5-1 Inlets,         !- Zone Air Inlet Node or NodeList Name",
        "    SPACE5-1 Fan Coil Return,!- Zone Air Exhaust Node or NodeList Name",
        "    SPACE5-1 Zone Air Node,  !- Zone Air Node Name",
        "    SPACE5-1 Return Outlet;  !- Zone Return Air Node Name",

        "Fan:VariableVolume,",
        "    DOAS Supply Fan,         !- Name",
        "    FanAvailSched,                !- Availability Schedule Name",
        "    0.7,                     !- Fan Total Efficiency",
        "    1000,                    !- Pressure Rise {Pa}",
        "    autosize,                !- Maximum Flow Rate {m3/s}",
        "    Fraction,                !- Fan Power Minimum Flow Rate Input Method",
        "    0.0,                     !- Fan Power Minimum Flow Fraction",
        "    ,                        !- Fan Power Minimum Air Flow Rate {m3/s}",
        "    0.9,                     !- Motor Efficiency",
        "    1,                       !- Motor In Airstream Fraction",
        "    0.0015302446,            !- Fan Power Coefficient 1",
        "    0.0052080574,            !- Fan Power Coefficient 2",
        "    1.1086242,               !- Fan Power Coefficient 3",
        "    -0.11635563,             !- Fan Power Coefficient 4",
        "    0,                       !- Fan Power Coefficient 5",
        "    DOAS Heating Coil Outlet,!- Air Inlet Node Name",
        "    DOAS Supply Fan Outlet;  !- Air Outlet Node Name",

        "Fan:OnOff,",
        "    SPACE1-1 Supply Fan,     !- Name",
        "    FanAvailSched,           !- Availability Schedule Name",
        "    0.7,                     !- Fan Total Efficiency",
        "    75,                      !- Pressure Rise {Pa}",
        "    autosize,                !- Maximum Flow Rate {m3/s}",
        "    0.9,                     !- Motor Efficiency",
        "    1,                       !- Motor In Airstream Fraction",
        "    SPACE1-1 Mixed Air Outlet,  !- Air Inlet Node Name",
        "    SPACE1-1 Supply Fan Outlet;  !- Air Outlet Node Name",

        "Fan:OnOff,",
        "    SPACE2-1 Supply Fan,     !- Name",
        "    FanAvailSched,           !- Availability Schedule Name",
        "    0.7,                     !- Fan Total Efficiency",
        "    75,                      !- Pressure Rise {Pa}",
        "    autosize,                !- Maximum Flow Rate {m3/s}",
        "    0.9,                     !- Motor Efficiency",
        "    1,                       !- Motor In Airstream Fraction",
        "    SPACE2-1 Mixed Air Outlet,  !- Air Inlet Node Name",
        "    SPACE2-1 Supply Fan Outlet;  !- Air Outlet Node Name",

        "Fan:OnOff,",
        "    SPACE3-1 Supply Fan,     !- Name",
        "    FanAvailSched,           !- Availability Schedule Name",
        "    0.7,                     !- Fan Total Efficiency",
        "    75,                      !- Pressure Rise {Pa}",
        "    autosize,                !- Maximum Flow Rate {m3/s}",
        "    0.9,                     !- Motor Efficiency",
        "    1,                       !- Motor In Airstream Fraction",
        "    SPACE3-1 Mixed Air Outlet,  !- Air Inlet Node Name",
        "    SPACE3-1 Supply Fan Outlet;  !- Air Outlet Node Name",

        "Fan:OnOff,",
        "    SPACE4-1 Supply Fan,     !- Name",
        "    FanAvailSched,           !- Availability Schedule Name",
        "    0.7,                     !- Fan Total Efficiency",
        "    75,                      !- Pressure Rise {Pa}",
        "    autosize,                !- Maximum Flow Rate {m3/s}",
        "    0.9,                     !- Motor Efficiency",
        "    1,                       !- Motor In Airstream Fraction",
        "    SPACE4-1 Mixed Air Outlet,  !- Air Inlet Node Name",
        "    SPACE4-1 Supply Fan Outlet;  !- Air Outlet Node Name",

        "Fan:OnOff,",
        "    SPACE5-1 Supply Fan,     !- Name",
        "    FanAvailSched,           !- Availability Schedule Name",
        "    0.7,                     !- Fan Total Efficiency",
        "    75,                      !- Pressure Rise {Pa}",
        "    autosize,                !- Maximum Flow Rate {m3/s}",
        "    0.9,                     !- Motor Efficiency",
        "    1,                       !- Motor In Airstream Fraction",
        "    SPACE5-1 Mixed Air Outlet,  !- Air Inlet Node Name",
        "    SPACE5-1 Supply Fan Outlet;  !- Air Outlet Node Name",

        "Coil:Cooling:Water,",
        "    SPACE1-1 Cooling Coil,   !- Name",
        "    HVACTemplate-Always 1,   !- Availability Schedule Name",
        "    autosize,                !- Design Water Flow Rate {m3/s}",
        "    autosize,                !- Design Air Flow Rate {m3/s}",
        "    autosize,                !- Design Inlet Water Temperature {C}",
        "    autosize,                !- Design Inlet Air Temperature {C}",
        "    autosize,                !- Design Outlet Air Temperature {C}",
        "    autosize,                !- Design Inlet Air Humidity Ratio {kgWater/kgDryAir}",
        "    autosize,                !- Design Outlet Air Humidity Ratio {kgWater/kgDryAir}",
        "    SPACE1-1 Cooling Coil ChW Inlet,  !- Water Inlet Node Name",
        "    SPACE1-1 Cooling Coil ChW Outlet,  !- Water Outlet Node Name",
        "    SPACE1-1 Supply Fan Outlet,  !- Air Inlet Node Name",
        "    SPACE1-1 Cooling Coil Outlet,  !- Air Outlet Node Name",
        "    DetailedAnalysis,        !- Type of Analysis",
        "    CrossFlow;               !- Heat Exchanger Configuration",

        "Coil:Cooling:Water,",
        "    SPACE2-1 Cooling Coil,   !- Name",
        "    HVACTemplate-Always 1,   !- Availability Schedule Name",
        "    autosize,                !- Design Water Flow Rate {m3/s}",
        "    autosize,                !- Design Air Flow Rate {m3/s}",
        "    autosize,                !- Design Inlet Water Temperature {C}",
        "    autosize,                !- Design Inlet Air Temperature {C}",
        "    autosize,                !- Design Outlet Air Temperature {C}",
        "    autosize,                !- Design Inlet Air Humidity Ratio {kgWater/kgDryAir}",
        "    autosize,                !- Design Outlet Air Humidity Ratio {kgWater/kgDryAir}",
        "    SPACE2-1 Cooling Coil ChW Inlet,  !- Water Inlet Node Name",
        "    SPACE2-1 Cooling Coil ChW Outlet,  !- Water Outlet Node Name",
        "    SPACE2-1 Supply Fan Outlet,  !- Air Inlet Node Name",
        "    SPACE2-1 Cooling Coil Outlet,  !- Air Outlet Node Name",
        "    DetailedAnalysis,        !- Type of Analysis",
        "    CrossFlow;               !- Heat Exchanger Configuration",

        "Coil:Cooling:Water,",
        "    SPACE3-1 Cooling Coil,   !- Name",
        "    HVACTemplate-Always 1,   !- Availability Schedule Name",
        "    autosize,                !- Design Water Flow Rate {m3/s}",
        "    autosize,                !- Design Air Flow Rate {m3/s}",
        "    autosize,                !- Design Inlet Water Temperature {C}",
        "    autosize,                !- Design Inlet Air Temperature {C}",
        "    autosize,                !- Design Outlet Air Temperature {C}",
        "    autosize,                !- Design Inlet Air Humidity Ratio {kgWater/kgDryAir}",
        "    autosize,                !- Design Outlet Air Humidity Ratio {kgWater/kgDryAir}",
        "    SPACE3-1 Cooling Coil ChW Inlet,  !- Water Inlet Node Name",
        "    SPACE3-1 Cooling Coil ChW Outlet,  !- Water Outlet Node Name",
        "    SPACE3-1 Supply Fan Outlet,  !- Air Inlet Node Name",
        "    SPACE3-1 Cooling Coil Outlet,  !- Air Outlet Node Name",
        "    DetailedAnalysis,        !- Type of Analysis",
        "    CrossFlow;               !- Heat Exchanger Configuration",

        "Coil:Cooling:Water,",
        "    SPACE4-1 Cooling Coil,   !- Name",
        "    HVACTemplate-Always 1,   !- Availability Schedule Name",
        "    autosize,                !- Design Water Flow Rate {m3/s}",
        "    autosize,                !- Design Air Flow Rate {m3/s}",
        "    autosize,                !- Design Inlet Water Temperature {C}",
        "    autosize,                !- Design Inlet Air Temperature {C}",
        "    autosize,                !- Design Outlet Air Temperature {C}",
        "    autosize,                !- Design Inlet Air Humidity Ratio {kgWater/kgDryAir}",
        "    autosize,                !- Design Outlet Air Humidity Ratio {kgWater/kgDryAir}",
        "    SPACE4-1 Cooling Coil ChW Inlet,  !- Water Inlet Node Name",
        "    SPACE4-1 Cooling Coil ChW Outlet,  !- Water Outlet Node Name",
        "    SPACE4-1 Supply Fan Outlet,  !- Air Inlet Node Name",
        "    SPACE4-1 Cooling Coil Outlet,  !- Air Outlet Node Name",
        "    DetailedAnalysis,        !- Type of Analysis",
        "    CrossFlow;               !- Heat Exchanger Configuration",

        "Coil:Cooling:Water,",
        "    SPACE5-1 Cooling Coil,   !- Name",
        "    HVACTemplate-Always 1,   !- Availability Schedule Name",
        "    autosize,                !- Design Water Flow Rate {m3/s}",
        "    autosize,                !- Design Air Flow Rate {m3/s}",
        "    autosize,                !- Design Inlet Water Temperature {C}",
        "    autosize,                !- Design Inlet Air Temperature {C}",
        "    autosize,                !- Design Outlet Air Temperature {C}",
        "    autosize,                !- Design Inlet Air Humidity Ratio {kgWater/kgDryAir}",
        "    autosize,                !- Design Outlet Air Humidity Ratio {kgWater/kgDryAir}",
        "    SPACE5-1 Cooling Coil ChW Inlet,  !- Water Inlet Node Name",
        "    SPACE5-1 Cooling Coil ChW Outlet,  !- Water Outlet Node Name",
        "    SPACE5-1 Supply Fan Outlet,  !- Air Inlet Node Name",
        "    SPACE5-1 Cooling Coil Outlet,  !- Air Outlet Node Name",
        "    DetailedAnalysis,        !- Type of Analysis",
        "    CrossFlow;               !- Heat Exchanger Configuration",

        "Coil:Cooling:Water,",
        "    DOAS Cooling Coil,       !- Name",
        "    HVACTemplate-Always 1,   !- Availability Schedule Name",
        "    autosize,                !- Design Water Flow Rate {m3/s}",
        "    autosize,                !- Design Air Flow Rate {m3/s}",
        "    autosize,                !- Design Inlet Water Temperature {C}",
        "    autosize,                !- Design Inlet Air Temperature {C}",
        "    autosize,                !- Design Outlet Air Temperature {C}",
        "    autosize,                !- Design Inlet Air Humidity Ratio {kgWater/kgDryAir}",
        "    autosize,                !- Design Outlet Air Humidity Ratio {kgWater/kgDryAir}",
        "    DOAS Cooling Coil ChW Inlet,  !- Water Inlet Node Name",
        "    DOAS Cooling Coil ChW Outlet,  !- Water Outlet Node Name",
        "    DOAS Heat Recovery Supply Outlet,   !- Air Inlet Node Name",
        "    DOAS Cooling Coil Outlet,!- Air Outlet Node Name",
        "    DetailedAnalysis,        !- Type of Analysis",
        "    CrossFlow;               !- Heat Exchanger Configuration",

        "Coil:Heating:Water,",
        "    SPACE1-1 Heating Coil,   !- Name",
        "    HVACTemplate-Always 1,   !- Availability Schedule Name",
        "    autosize,                !- U-Factor Times Area Value {W/K}",
        "    autosize,                !- Maximum Water Flow Rate {m3/s}",
        "    SPACE1-1 Heating Coil HW Inlet,  !- Water Inlet Node Name",
        "    SPACE1-1 Heating Coil HW Outlet,  !- Water Outlet Node Name",
        "    SPACE1-1 Cooling Coil Outlet,  !- Air Inlet Node Name",
        "    SPACE1-1 Supply Inlet,   !- Air Outlet Node Name",
        "    UFactorTimesAreaAndDesignWaterFlowRate,  !- Performance Input Method",
        "    autosize,                !- Rated Capacity {W}",
        "    82.2,                    !- Rated Inlet Water Temperature {C}",
        "    16.6,                    !- Rated Inlet Air Temperature {C}",
        "    71.1,                    !- Rated Outlet Water Temperature {C}",
        "    32.2,                    !- Rated Outlet Air Temperature {C}",
        "    0.5;                     !- Rated Ratio for Air and Water Convection",

        "Coil:Heating:Water,",
        "    SPACE2-1 Heating Coil,   !- Name",
        "    HVACTemplate-Always 1,   !- Availability Schedule Name",
        "    autosize,                !- U-Factor Times Area Value {W/K}",
        "    autosize,                !- Maximum Water Flow Rate {m3/s}",
        "    SPACE2-1 Heating Coil HW Inlet,  !- Water Inlet Node Name",
        "    SPACE2-1 Heating Coil HW Outlet,  !- Water Outlet Node Name",
        "    SPACE2-1 Cooling Coil Outlet,  !- Air Inlet Node Name",
        "    SPACE2-1 Supply Inlet,   !- Air Outlet Node Name",
        "    UFactorTimesAreaAndDesignWaterFlowRate,  !- Performance Input Method",
        "    autosize,                !- Rated Capacity {W}",
        "    82.2,                    !- Rated Inlet Water Temperature {C}",
        "    16.6,                    !- Rated Inlet Air Temperature {C}",
        "    71.1,                    !- Rated Outlet Water Temperature {C}",
        "    32.2,                    !- Rated Outlet Air Temperature {C}",
        "    0.5;                     !- Rated Ratio for Air and Water Convection",

        "Coil:Heating:Water,",
        "    SPACE3-1 Heating Coil,   !- Name",
        "    HVACTemplate-Always 1,   !- Availability Schedule Name",
        "    autosize,                !- U-Factor Times Area Value {W/K}",
        "    autosize,                !- Maximum Water Flow Rate {m3/s}",
        "    SPACE3-1 Heating Coil HW Inlet,  !- Water Inlet Node Name",
        "    SPACE3-1 Heating Coil HW Outlet,  !- Water Outlet Node Name",
        "    SPACE3-1 Cooling Coil Outlet,  !- Air Inlet Node Name",
        "    SPACE3-1 Supply Inlet,   !- Air Outlet Node Name",
        "    UFactorTimesAreaAndDesignWaterFlowRate,  !- Performance Input Method",
        "    autosize,                !- Rated Capacity {W}",
        "    82.2,                    !- Rated Inlet Water Temperature {C}",
        "    16.6,                    !- Rated Inlet Air Temperature {C}",
        "    71.1,                    !- Rated Outlet Water Temperature {C}",
        "    32.2,                    !- Rated Outlet Air Temperature {C}",
        "    0.5;                     !- Rated Ratio for Air and Water Convection",

        "Coil:Heating:Water,",
        "    SPACE4-1 Heating Coil,   !- Name",
        "    HVACTemplate-Always 1,   !- Availability Schedule Name",
        "    autosize,                !- U-Factor Times Area Value {W/K}",
        "    autosize,                !- Maximum Water Flow Rate {m3/s}",
        "    SPACE4-1 Heating Coil HW Inlet,  !- Water Inlet Node Name",
        "    SPACE4-1 Heating Coil HW Outlet,  !- Water Outlet Node Name",
        "    SPACE4-1 Cooling Coil Outlet,  !- Air Inlet Node Name",
        "    SPACE4-1 Supply Inlet,   !- Air Outlet Node Name",
        "    UFactorTimesAreaAndDesignWaterFlowRate,  !- Performance Input Method",
        "    autosize,                !- Rated Capacity {W}",
        "    82.2,                    !- Rated Inlet Water Temperature {C}",
        "    16.6,                    !- Rated Inlet Air Temperature {C}",
        "    71.1,                    !- Rated Outlet Water Temperature {C}",
        "    32.2,                    !- Rated Outlet Air Temperature {C}",
        "    0.5;                     !- Rated Ratio for Air and Water Convection",

        "Coil:Heating:Water,",
        "    SPACE5-1 Heating Coil,   !- Name",
        "    HVACTemplate-Always 1,   !- Availability Schedule Name",
        "    autosize,                !- U-Factor Times Area Value {W/K}",
        "    autosize,                !- Maximum Water Flow Rate {m3/s}",
        "    SPACE5-1 Heating Coil HW Inlet,  !- Water Inlet Node Name",
        "    SPACE5-1 Heating Coil HW Outlet,  !- Water Outlet Node Name",
        "    SPACE5-1 Cooling Coil Outlet,  !- Air Inlet Node Name",
        "    SPACE5-1 Supply Inlet,   !- Air Outlet Node Name",
        "    UFactorTimesAreaAndDesignWaterFlowRate,  !- Performance Input Method",
        "    autosize,                !- Rated Capacity {W}",
        "    82.2,                    !- Rated Inlet Water Temperature {C}",
        "    16.6,                    !- Rated Inlet Air Temperature {C}",
        "    71.1,                    !- Rated Outlet Water Temperature {C}",
        "    32.2,                    !- Rated Outlet Air Temperature {C}",
        "    0.5;                     !- Rated Ratio for Air and Water Convection",

        "Coil:Heating:Water,",
        "    DOAS Heating Coil,       !- Name",
        "    HVACTemplate-Always 1,   !- Availability Schedule Name",
        "    autosize,                !- U-Factor Times Area Value {W/K}",
        "    autosize,                !- Maximum Water Flow Rate {m3/s}",
        "    DOAS Heating Coil HW Inlet,  !- Water Inlet Node Name",
        "    DOAS Heating Coil HW Outlet,  !- Water Outlet Node Name",
        "    DOAS Cooling Coil Outlet,!- Air Inlet Node Name",
        "    DOAS Heating Coil Outlet,!- Air Outlet Node Name",
        "    UFactorTimesAreaAndDesignWaterFlowRate,  !- Performance Input Method",
        "    autosize,                !- Rated Capacity {W}",
        "    82.2,                    !- Rated Inlet Water Temperature {C}",
        "    16.6,                    !- Rated Inlet Air Temperature {C}",
        "    71.1,                    !- Rated Outlet Water Temperature {C}",
        "    32.2,                    !- Rated Outlet Air Temperature {C}",
        "    0.5;                     !- Rated Ratio for Air and Water Convection",

        "HeatExchanger:AirToAir:SensibleAndLatent,",
        "    DOAS Heat Recovery,      !- Name",
        "    HVACTemplate-Always 1,   !- Availability Schedule Name",
        "    autosize,                !- Nominal Supply Air Flow Rate {m3/s}",
        "    0.7,                     !- Sensible Effectiveness at 100% Heating Air Flow {dimensionless}",
        "    0.65,                    !- Latent Effectiveness at 100% Heating Air Flow {dimensionless}",
        "    0.750000,                !- Sensible Effectiveness at 75% Heating Air Flow {dimensionless}",
        "    0.700000,                !- Latent Effectiveness at 75% Heating Air Flow {dimensionless}",
        "    0.7,                     !- Sensible Effectiveness at 100% Cooling Air Flow {dimensionless}",
        "    0.65,                    !- Latent Effectiveness at 100% Cooling Air Flow {dimensionless}",
        "    0.750000,                !- Sensible Effectiveness at 75% Cooling Air Flow {dimensionless}",
        "    0.700000,                !- Latent Effectiveness at 75% Cooling Air Flow {dimensionless}",
        "    DOAS Mixed Air Outlet,   !- Supply Air Inlet Node Name",
        "    DOAS Heat Recovery Supply Outlet,  !- Supply Air Outlet Node Name",
        "    DOAS Relief Air Outlet,  !- Exhaust Air Inlet Node Name",
        "    DOAS Heat Recovery Relief Outlet,  !- Exhaust Air Outlet Node Name",
        "    0,                       !- Nominal Electric Power {W}",
        "    Yes,                     !- Supply Air Outlet Temperature Control",
        "    Plate,                   !- Heat Exchanger Type",
        "    MinimumExhaustTemperature,  !- Frost Control Type",
        "    1.7,                     !- Threshold Temperature {C}",
        "    0.083,                   !- Initial Defrost Time Fraction {dimensionless}",
        "    0.012,                   !- Rate of Defrost Time Fraction Increase {1/K}",
        "    Yes;                     !- Economizer Lockout",

        "Controller:WaterCoil,",
        "    DOAS Cooling Coil Controller,  !- Name",
        "    Temperature,             !- Control Variable",
        "    Reverse,                 !- Action",
        "    Flow,                    !- Actuator Variable",
        "    DOAS Cooling Coil Outlet,!- Sensor Node Name",
        "    DOAS Cooling Coil ChW Inlet,  !- Actuator Node Name",
        "    autosize,                !- Controller Convergence Tolerance {deltaC}",
        "    autosize,                !- Maximum Actuated Flow {m3/s}",
        "    0;                       !- Minimum Actuated Flow {m3/s}",

        "Controller:WaterCoil,",
        "    DOAS Heating Coil Controller,  !- Name",
        "    Temperature,             !- Control Variable",
        "    Normal,                  !- Action",
        "    Flow,                    !- Actuator Variable",
        "    DOAS Heating Coil Outlet,!- Sensor Node Name",
        "    DOAS Heating Coil HW Inlet,  !- Actuator Node Name",
        "    autosize,                !- Controller Convergence Tolerance {deltaC}",
        "    autosize,                !- Maximum Actuated Flow {m3/s}",
        "    0;                       !- Minimum Actuated Flow {m3/s}",

        "Controller:OutdoorAir,",
        "    DOAS OA Controller,      !- Name",
        "    DOAS Relief Air Outlet,  !- Relief Air Outlet Node Name",
        "    DOAS Air Loop Inlet,     !- Return Air Node Name",
        "    DOAS Mixed Air Outlet,   !- Mixed Air Node Name",
        "    DOAS Outdoor Air Inlet,  !- Actuator Node Name",
        "    autosize,                !- Minimum Outdoor Air Flow Rate {m3/s}",
        "    autosize,                !- Maximum Outdoor Air Flow Rate {m3/s}",
        "    DifferentialEnthalpy,    !- Economizer Control Type",
        "    MinimumFlowWithBypass,   !- Economizer Control Action Type",
        "    ,                        !- Economizer Maximum Limit Dry-Bulb Temperature {C}",
        "    ,                        !- Economizer Maximum Limit Enthalpy {J/kg}",
        "    ,                        !- Economizer Maximum Limit Dewpoint Temperature {C}",
        "    ,                        !- Electronic Enthalpy Limit Curve Name",
        "    12.2,                    !- Economizer Minimum Limit Dry-Bulb Temperature {C}",
        "    NoLockout,               !- Lockout Type",
        "    ProportionalMinimum;     !- Minimum Limit Type",

        "AirLoopHVAC:ControllerList,",
        "    DOAS Controllers,        !- Name",
        "    Controller:WaterCoil,    !- Controller 1 Object Type",
        "    DOAS Cooling Coil Controller,  !- Controller 1 Name",
        "    Controller:WaterCoil,    !- Controller 2 Object Type",
        "    DOAS Heating Coil Controller;  !- Controller 2 Name",

        "AirLoopHVAC:ControllerList,",
        "    DOAS OA System Controllers,  !- Name",
        "    Controller:OutdoorAir,   !- Controller 1 Object Type",
        "    DOAS OA Controller;      !- Controller 1 Name",

        "AirLoopHVAC,",
        "    DOAS,                    !- Name",
        "    DOAS Controllers,        !- Controller List Name",
        "    DOAS Availability Managers,  !- Availability Manager List Name",
        "    autosize,                !- Design Supply Air Flow Rate {m3/s}",
        "    DOAS Branches,           !- Branch List Name",
        "    ,                        !- Connector List Name",
        "    DOAS Air Loop Inlet,     !- Supply Side Inlet Node Name",
        "    DOAS Return Air Outlet,  !- Demand Side Outlet Node Name",
        "    DOAS Supply Path Inlet,  !- Demand Side Inlet Node Names",
        "    DOAS Supply Fan Outlet;  !- Supply Side Outlet Node Names",

        "AirLoopHVAC:OutdoorAirSystem:EquipmentList,",
        "    DOAS OA System Equipment,!- Name",
        "    OutdoorAir:Mixer,        !- Component 2 Object Type",
        "    DOAS OA Mixing Box;      !- Component 2 Name",

        "AirLoopHVAC:OutdoorAirSystem,",
        "    DOAS OA System,          !- Name",
        "    DOAS OA System Controllers,  !- Controller List Name",
        "    DOAS OA System Equipment,!- Outdoor Air Equipment List Name",
        "    DOAS Availability Managers;  !- Availability Manager List Name",

        "OutdoorAir:Mixer,",
        "    SPACE1-1 OA Mixing Box,  !- Name",
        "    SPACE1-1 Mixed Air Outlet,  !- Mixed Air Node Name",
        "    SPACE1-1 Outside Air Inlet,  !- Outdoor Air Stream Node Name",
        "    SPACE1-1 Relief Air Outlet,  !- Relief Air Stream Node Name",
        "    SPACE1-1 Fan Coil Return;!- Return Air Stream Node Name",

        "OutdoorAir:Mixer,",
        "    SPACE2-1 OA Mixing Box,  !- Name",
        "    SPACE2-1 Mixed Air Outlet,  !- Mixed Air Node Name",
        "    SPACE2-1 Outside Air Inlet,  !- Outdoor Air Stream Node Name",
        "    SPACE2-1 Relief Air Outlet,  !- Relief Air Stream Node Name",
        "    SPACE2-1 Fan Coil Return;!- Return Air Stream Node Name",

        "OutdoorAir:Mixer,",
        "    SPACE3-1 OA Mixing Box,  !- Name",
        "    SPACE3-1 Mixed Air Outlet,  !- Mixed Air Node Name",
        "    SPACE3-1 Outside Air Inlet,  !- Outdoor Air Stream Node Name",
        "    SPACE3-1 Relief Air Outlet,  !- Relief Air Stream Node Name",
        "    SPACE3-1 Fan Coil Return;!- Return Air Stream Node Name",

        "OutdoorAir:Mixer,",
        "    SPACE4-1 OA Mixing Box,  !- Name",
        "    SPACE4-1 Mixed Air Outlet,  !- Mixed Air Node Name",
        "    SPACE4-1 Outside Air Inlet,  !- Outdoor Air Stream Node Name",
        "    SPACE4-1 Relief Air Outlet,  !- Relief Air Stream Node Name",
        "    SPACE4-1 Fan Coil Return;!- Return Air Stream Node Name",

        "OutdoorAir:Mixer,",
        "    SPACE5-1 OA Mixing Box,  !- Name",
        "    SPACE5-1 Mixed Air Outlet,  !- Mixed Air Node Name",
        "    SPACE5-1 Outside Air Inlet,  !- Outdoor Air Stream Node Name",
        "    SPACE5-1 Relief Air Outlet,  !- Relief Air Stream Node Name",
        "    SPACE5-1 Fan Coil Return;!- Return Air Stream Node Name",

        "OutdoorAir:Mixer,",
        "    DOAS OA Mixing Box,      !- Name",
        "    DOAS Mixed Air Outlet,   !- Mixed Air Node Name",
        "    DOAS Outdoor Air Inlet,  !- Outdoor Air Stream Node Name",
        "    DOAS Relief Air Outlet,  !- Relief Air Stream Node Name",
        "    DOAS Air Loop Inlet;     !- Return Air Stream Node Name",

        "AirLoopHVAC:ZoneSplitter,",
        "    DOAS Zone Splitter,      !- Name",
        "    DOAS Supply Path Inlet,  !- Inlet Node Name",
        "    SPACE1-1 Zone Equip Inlet,  !- Outlet 1 Node Name",
        "    SPACE2-1 Zone Equip Inlet,  !- Outlet 2 Node Name",
        "    SPACE3-1 Zone Equip Inlet,  !- Outlet 3 Node Name",
        "    SPACE4-1 Zone Equip Inlet,  !- Outlet 4 Node Name",
        "    SPACE5-1 Zone Equip Inlet;  !- Outlet 5 Node Name",

        "AirLoopHVAC:SupplyPath,",
        "    DOAS Supply Path,        !- Name",
        "    DOAS Supply Path Inlet,  !- Supply Air Path Inlet Node Name",
        "    AirLoopHVAC:ZoneSplitter,!- Component 1 Object Type",
        "    DOAS Zone Splitter;      !- Component 1 Name",

        "AirLoopHVAC:ZoneMixer,",
        "    DOAS Zone Mixer,         !- Name",
        "    DOAS Return Air Outlet,  !- Outlet Node Name",
        "    SPACE1-1 Return Outlet,  !- Inlet 1 Node Name",
        "    SPACE2-1 Return Outlet,  !- Inlet 2 Node Name",
        "    SPACE3-1 Return Outlet,  !- Inlet 3 Node Name",
        "    SPACE4-1 Return Outlet,  !- Inlet 4 Node Name",
        "    SPACE5-1 Return Outlet;  !- Inlet 5 Node Name",

        "AirLoopHVAC:ReturnPath,",
        "    DOAS Return Path,        !- Name",
        "    DOAS Return Air Outlet,  !- Return Air Path Outlet Node Name",
        "    AirLoopHVAC:ZoneMixer,   !- Component 1 Object Type",
        "    DOAS Zone Mixer;         !- Component 1 Name",

        "Branch,",
        "    SPACE1-1 Cooling Coil ChW Branch,  !- Name",
        "    ,                        !- Pressure Drop Curve Name",
        "    Coil:Cooling:Water,      !- Component 1 Object Type",
        "    SPACE1-1 Cooling Coil,   !- Component 1 Name",
        "    SPACE1-1 Cooling Coil ChW Inlet,  !- Component 1 Inlet Node Name",
        "    SPACE1-1 Cooling Coil ChW Outlet;  !- Component 1 Outlet Node Name",

        "Branch,",
        "    SPACE1-1 Heating Coil HW Branch,  !- Name",
        "    ,                        !- Pressure Drop Curve Name",
        "    Coil:Heating:Water,      !- Component 1 Object Type",
        "    SPACE1-1 Heating Coil,   !- Component 1 Name",
        "    SPACE1-1 Heating Coil HW Inlet,  !- Component 1 Inlet Node Name",
        "    SPACE1-1 Heating Coil HW Outlet;  !- Component 1 Outlet Node Name",

        "Branch,",
        "    SPACE2-1 Cooling Coil ChW Branch,  !- Name",
        "    ,                        !- Pressure Drop Curve Name",
        "    Coil:Cooling:Water,      !- Component 1 Object Type",
        "    SPACE2-1 Cooling Coil,   !- Component 1 Name",
        "    SPACE2-1 Cooling Coil ChW Inlet,  !- Component 1 Inlet Node Name",
        "    SPACE2-1 Cooling Coil ChW Outlet;  !- Component 1 Outlet Node Name",

        "Branch,",
        "    SPACE2-1 Heating Coil HW Branch,  !- Name",
        "    ,                        !- Pressure Drop Curve Name",
        "    Coil:Heating:Water,      !- Component 1 Object Type",
        "    SPACE2-1 Heating Coil,   !- Component 1 Name",
        "    SPACE2-1 Heating Coil HW Inlet,  !- Component 1 Inlet Node Name",
        "    SPACE2-1 Heating Coil HW Outlet;  !- Component 1 Outlet Node Name",

        "Branch,",
        "    SPACE3-1 Cooling Coil ChW Branch,  !- Name",
        "    ,                        !- Pressure Drop Curve Name",
        "    Coil:Cooling:Water,      !- Component 1 Object Type",
        "    SPACE3-1 Cooling Coil,   !- Component 1 Name",
        "    SPACE3-1 Cooling Coil ChW Inlet,  !- Component 1 Inlet Node Name",
        "    SPACE3-1 Cooling Coil ChW Outlet;  !- Component 1 Outlet Node Name",

        "Branch,",
        "    SPACE3-1 Heating Coil HW Branch,  !- Name",
        "    ,                        !- Pressure Drop Curve Name",
        "    Coil:Heating:Water,      !- Component 1 Object Type",
        "    SPACE3-1 Heating Coil,   !- Component 1 Name",
        "    SPACE3-1 Heating Coil HW Inlet,  !- Component 1 Inlet Node Name",
        "    SPACE3-1 Heating Coil HW Outlet;  !- Component 1 Outlet Node Name",

        "Branch,",
        "    SPACE4-1 Cooling Coil ChW Branch,  !- Name",
        "    ,                        !- Pressure Drop Curve Name",
        "    Coil:Cooling:Water,      !- Component 1 Object Type",
        "    SPACE4-1 Cooling Coil,   !- Component 1 Name",
        "    SPACE4-1 Cooling Coil ChW Inlet,  !- Component 1 Inlet Node Name",
        "    SPACE4-1 Cooling Coil ChW Outlet;  !- Component 1 Outlet Node Name",

        "Branch,",
        "    SPACE4-1 Heating Coil HW Branch,  !- Name",
        "    ,                        !- Pressure Drop Curve Name",
        "    Coil:Heating:Water,      !- Component 1 Object Type",
        "    SPACE4-1 Heating Coil,   !- Component 1 Name",
        "    SPACE4-1 Heating Coil HW Inlet,  !- Component 1 Inlet Node Name",
        "    SPACE4-1 Heating Coil HW Outlet;  !- Component 1 Outlet Node Name",

        "Branch,",
        "    SPACE5-1 Cooling Coil ChW Branch,  !- Name",
        "    ,                        !- Pressure Drop Curve Name",
        "    Coil:Cooling:Water,      !- Component 1 Object Type",
        "    SPACE5-1 Cooling Coil,   !- Component 1 Name",
        "    SPACE5-1 Cooling Coil ChW Inlet,  !- Component 1 Inlet Node Name",
        "    SPACE5-1 Cooling Coil ChW Outlet;  !- Component 1 Outlet Node Name",

        "Branch,",
        "    SPACE5-1 Heating Coil HW Branch,  !- Name",
        "    ,                        !- Pressure Drop Curve Name",
        "    Coil:Heating:Water,      !- Component 1 Object Type",
        "    SPACE5-1 Heating Coil,   !- Component 1 Name",
        "    SPACE5-1 Heating Coil HW Inlet,  !- Component 1 Inlet Node Name",
        "    SPACE5-1 Heating Coil HW Outlet;  !- Component 1 Outlet Node Name",

        "Branch,",
        "    DOAS Main Branch,        !- Name",
        "    ,                        !- Pressure Drop Curve Name",
        "    AirLoopHVAC:OutdoorAirSystem,  !- Component 1 Object Type",
        "    DOAS OA System,          !- Component 1 Name",
        "    DOAS Air Loop Inlet,     !- Component 1 Inlet Node Name",
        "    DOAS Mixed Air Outlet,   !- Component 1 Outlet Node Name",
        "    HeatExchanger:AirToAir:SensibleAndLatent,  !- Component 1 Object Type",
        "    DOAS Heat Recovery,      !- Component 2 Name",
        "    DOAS Mixed Air Outlet,   !- Component 2 Inlet Node Name",
        "    DOAS Heat Recovery Supply Outlet,  !- Component 2 Outlet Node Name",
        "    Coil:Cooling:Water,      !- Component 3 Object Type",
        "    DOAS Cooling Coil,       !- Component 3 Name",
        "    DOAS Heat Recovery Supply Outlet,   !- Component 3 Inlet Node Name",
        "    DOAS Cooling Coil Outlet,!- Component 3 Outlet Node Name",
        "    Coil:Heating:Water,      !- Component 4 Object Type",
        "    DOAS Heating Coil,       !- Component 4 Name",
        "    DOAS Cooling Coil Outlet,!- Component 4 Inlet Node Name",
        "    DOAS Heating Coil Outlet,!- Component 4 Outlet Node Name",
        "    Fan:VariableVolume,      !- Component 5 Object Type",
        "    DOAS Supply Fan,         !- Component 5 Name",
        "    DOAS Heating Coil Outlet,!- Component 5 Inlet Node Name",
        "    DOAS Supply Fan Outlet;  !- Component 5 Outlet Node Name",

        "Branch,",
        "    DOAS Cooling Coil ChW Branch,  !- Name",
        "    ,                        !- Pressure Drop Curve Name",
        "    Coil:Cooling:Water,      !- Component 1 Object Type",
        "    DOAS Cooling Coil,       !- Component 1 Name",
        "    DOAS Cooling Coil ChW Inlet,  !- Component 1 Inlet Node Name",
        "    DOAS Cooling Coil ChW Outlet;  !- Component 1 Outlet Node Name",

        "Branch,",
        "    DOAS Heating Coil HW Branch,  !- Name",
        "    ,                        !- Pressure Drop Curve Name",
        "    Coil:Heating:Water,      !- Component 1 Object Type",
        "    DOAS Heating Coil,       !- Component 1 Name",
        "    DOAS Heating Coil HW Inlet,  !- Component 1 Inlet Node Name",
        "    DOAS Heating Coil HW Outlet;  !- Component 1 Outlet Node Name",

        "Branch,",
        "    Main Boiler HW Branch,   !- Name",
        "    ,                        !- Pressure Drop Curve Name",
        "    DistrictHeating,         !- Component 1 Object Type",
        "    Purchased Heating,         !- Component 1 Name",
        "    Purchased Heat Inlet Node, !- Component 1 Inlet Node Name",
        "    Purchased Heat Outlet Node;   !- Component 1 Outlet Node Name",

        "Branch,",
        "    Main Chiller ChW Branch, !- Name",
        "    ,                        !- Pressure Drop Curve Name",
        "    DistrictCooling,         !- Component 1 Object Type",
        "    Purchased Cooling,            !- Component 1 Name",
        "    Purchased Cooling Inlet Node,  !- Component 1 Inlet Node Name",
        "    Purchased Cooling Outlet Node; !- Component 1 Outlet Node Name",

        "Branch,",
        "    Hot Water Loop HW Supply Bypass Branch,  !- Name",
        "    ,                        !- Pressure Drop Curve Name",
        "    Pipe:Adiabatic,          !- Component 1 Object Type",
        "    Hot Water Loop HW Supply Side Bypass Pipe,  !- Component 1 Name",
        "    Hot Water Loop HW Supply Bypass Inlet,  !- Component 1 Inlet Node Name",
        "    Hot Water Loop HW Supply Bypass Outlet;  !- Component 1 Outlet Node Name",

        "Branch,",
        "    Hot Water Loop HW Supply Inlet Branch,  !- Name",
        "    ,                        !- Pressure Drop Curve Name",
        "    Pump:ConstantSpeed,      !- Component 1 Object Type",
        "    Hot Water Loop HW Supply Pump,  !- Component 1 Name",
        "    Hot Water Loop HW Supply Inlet,  !- Component 1 Inlet Node Name",
        "    Hot Water Loop HW Pump Outlet;  !- Component 1 Outlet Node Name",

        "Branch,",
        "    Hot Water Loop HW Supply Outlet Branch,  !- Name",
        "    ,                        !- Pressure Drop Curve Name",
        "    Pipe:Adiabatic,          !- Component 1 Object Type",
        "    Hot Water Loop HW Supply Outlet Pipe,  !- Component 1 Name",
        "    Hot Water Loop HW Supply Outlet Pipe Inlet,  !- Component 1 Inlet Node Name",
        "    Hot Water Loop HW Supply Outlet;  !- Component 1 Outlet Node Name",

        "Branch,",
        "    Hot Water Loop HW Demand Inlet Branch,  !- Name",
        "    ,                        !- Pressure Drop Curve Name",
        "    Pipe:Adiabatic,          !- Component 1 Object Type",
        "    Hot Water Loop HW Demand Inlet Pipe,  !- Component 1 Name",
        "    Hot Water Loop HW Demand Inlet,  !- Component 1 Inlet Node Name",
        "    Hot Water Loop HW Demand Inlet Pipe Outlet;  !- Component 1 Outlet Node Name",

        "Branch,",
        "    Hot Water Loop HW Demand Bypass Branch,  !- Name",
        "    ,                        !- Pressure Drop Curve Name",
        "    Pipe:Adiabatic,          !- Component 1 Object Type",
        "    Hot Water Loop HW Demand Side Bypass Pipe,  !- Component 1 Name",
        "    Hot Water Loop HW Demand Bypass Inlet,  !- Component 1 Inlet Node Name",
        "    Hot Water Loop HW Demand Bypass Outlet;  !- Component 1 Outlet Node Name",

        "Branch,",
        "    Hot Water Loop HW Demand Outlet Branch,  !- Name",
        "    ,                        !- Pressure Drop Curve Name",
        "    Pipe:Adiabatic,          !- Component 1 Object Type",
        "    Hot Water Loop HW Demand Outlet Pipe,  !- Component 1 Name",
        "    Hot Water Loop HW Demand Outlet Pipe Inlet,  !- Component 1 Inlet Node Name",
        "    Hot Water Loop HW Demand Outlet;  !- Component 1 Outlet Node Name",

        "Branch,",
        "    Chilled Water Loop ChW Supply Bypass Branch,  !- Name",
        "    ,                        !- Pressure Drop Curve Name",
        "    Pipe:Adiabatic,          !- Component 1 Object Type",
        "    Chilled Water Loop ChW Supply Side Bypass Pipe,  !- Component 1 Name",
        "    Chilled Water Loop ChW Supply Bypass Inlet,  !- Component 1 Inlet Node Name",
        "    Chilled Water Loop ChW Supply Bypass Outlet;  !- Component 1 Outlet Node Name",

        "Branch,",
        "    Chilled Water Loop ChW Supply Inlet Branch,  !- Name",
        "    ,                        !- Pressure Drop Curve Name",
        "    Pump:ConstantSpeed,      !- Component 1 Object Type",
        "    Chilled Water Loop ChW Supply Pump,  !- Component 1 Name",
        "    Chilled Water Loop ChW Supply Inlet,  !- Component 1 Inlet Node Name",
        "    Chilled Water Loop ChW Pump Outlet;  !- Component 1 Outlet Node Name",

        "Branch,",
        "    Chilled Water Loop ChW Supply Outlet Branch,  !- Name",
        "    ,                        !- Pressure Drop Curve Name",
        "    Pipe:Adiabatic,          !- Component 1 Object Type",
        "    Chilled Water Loop ChW Supply Outlet Pipe,  !- Component 1 Name",
        "    Chilled Water Loop ChW Supply Outlet Pipe Inlet,  !- Component 1 Inlet Node Name",
        "    Chilled Water Loop ChW Supply Outlet;  !- Component 1 Outlet Node Name",

        "Branch,",
        "    Chilled Water Loop ChW Demand Inlet Branch,  !- Name",
        "    ,                        !- Pressure Drop Curve Name",
        "    Pipe:Adiabatic,          !- Component 1 Object Type",
        "    Chilled Water Loop ChW Demand Inlet Pipe,  !- Component 1 Name",
        "    Chilled Water Loop ChW Demand Inlet,  !- Component 1 Inlet Node Name",
        "    Chilled Water Loop ChW Demand Inlet Pipe Outlet;  !- Component 1 Outlet Node Name",

        "Branch,",
        "    Chilled Water Loop ChW Demand Bypass Branch,  !- Name",
        "    ,                        !- Pressure Drop Curve Name",
        "    Pipe:Adiabatic,          !- Component 1 Object Type",
        "    Chilled Water Loop ChW Demand Side Bypass Pipe,  !- Component 1 Name",
        "    Chilled Water Loop ChW Demand Bypass Inlet,  !- Component 1 Inlet Node Name",
        "    Chilled Water Loop ChW Demand Bypass Outlet;  !- Component 1 Outlet Node Name",

        "Branch,",
        "    Chilled Water Loop ChW Demand Outlet Branch,  !- Name",
        "    ,                        !- Pressure Drop Curve Name",
        "    Pipe:Adiabatic,          !- Component 1 Object Type",
        "    Chilled Water Loop ChW Demand Outlet Pipe,  !- Component 1 Name",
        "    Chilled Water Loop ChW Demand Outlet Pipe Inlet,  !- Component 1 Inlet Node Name",
        "    Chilled Water Loop ChW Demand Outlet;  !- Component 1 Outlet Node Name",

        "BranchList,",
        "    DOAS Branches,           !- Name",
        "    DOAS Main Branch;        !- Branch 1 Name",

        "BranchList,",
        "    Hot Water Loop HW Supply Side Branches,  !- Name",
        "    Hot Water Loop HW Supply Inlet Branch,  !- Branch 1 Name",
        "    Main Boiler HW Branch,   !- Branch 2 Name",
        "    Hot Water Loop HW Supply Bypass Branch,  !- Branch 3 Name",
        "    Hot Water Loop HW Supply Outlet Branch;  !- Branch 4 Name",

        "BranchList,",
        "    Hot Water Loop HW Demand Side Branches,  !- Name",
        "    Hot Water Loop HW Demand Inlet Branch,  !- Branch 1 Name",
        "    SPACE1-1 Heating Coil HW Branch,  !- Branch 2 Name",
        "    SPACE2-1 Heating Coil HW Branch,  !- Branch 3 Name",
        "    SPACE3-1 Heating Coil HW Branch,  !- Branch 4 Name",
        "    SPACE4-1 Heating Coil HW Branch,  !- Branch 5 Name",
        "    SPACE5-1 Heating Coil HW Branch,  !- Branch 6 Name",
        "    DOAS Heating Coil HW Branch,  !- Branch 7 Name",
        "    Hot Water Loop HW Demand Bypass Branch,  !- Branch 8 Name",
        "    Hot Water Loop HW Demand Outlet Branch;  !- Branch 9 Name",

        "BranchList,",
        "    Chilled Water Loop ChW Supply Side Branches,  !- Name",
        "    Chilled Water Loop ChW Supply Inlet Branch,  !- Branch 1 Name",
        "    Main Chiller ChW Branch, !- Branch 2 Name",
        "    Chilled Water Loop ChW Supply Bypass Branch,  !- Branch 3 Name",
        "    Chilled Water Loop ChW Supply Outlet Branch;  !- Branch 4 Name",

        "BranchList,",
        "    Chilled Water Loop ChW Demand Side Branches,  !- Name",
        "    Chilled Water Loop ChW Demand Inlet Branch,  !- Branch 1 Name",
        "    SPACE1-1 Cooling Coil ChW Branch,  !- Branch 2 Name",
        "    SPACE2-1 Cooling Coil ChW Branch,  !- Branch 3 Name",
        "    SPACE3-1 Cooling Coil ChW Branch,  !- Branch 4 Name",
        "    SPACE4-1 Cooling Coil ChW Branch,  !- Branch 5 Name",
        "    SPACE5-1 Cooling Coil ChW Branch,  !- Branch 6 Name",
        "    DOAS Cooling Coil ChW Branch,  !- Branch 7 Name",
        "    Chilled Water Loop ChW Demand Bypass Branch,  !- Branch 8 Name",
        "    Chilled Water Loop ChW Demand Outlet Branch;  !- Branch 9 Name",

        "Connector:Splitter,",
        "    Hot Water Loop HW Supply Splitter,  !- Name",
        "    Hot Water Loop HW Supply Inlet Branch,  !- Inlet Branch Name",
        "    Main Boiler HW Branch,   !- Outlet Branch 1 Name",
        "    Hot Water Loop HW Supply Bypass Branch;  !- Outlet Branch 2 Name",

        "Connector:Splitter,",
        "    Hot Water Loop HW Demand Splitter,  !- Name",
        "    Hot Water Loop HW Demand Inlet Branch,  !- Inlet Branch Name",
        "    Hot Water Loop HW Demand Bypass Branch,  !- Outlet Branch 1 Name",
        "    SPACE1-1 Heating Coil HW Branch,  !- Outlet Branch 2 Name",
        "    SPACE2-1 Heating Coil HW Branch,  !- Outlet Branch 3 Name",
        "    SPACE3-1 Heating Coil HW Branch,  !- Outlet Branch 4 Name",
        "    SPACE4-1 Heating Coil HW Branch,  !- Outlet Branch 5 Name",
        "    SPACE5-1 Heating Coil HW Branch,  !- Outlet Branch 6 Name",
        "    DOAS Heating Coil HW Branch;  !- Outlet Branch 7 Name",

        "Connector:Splitter,",
        "    Chilled Water Loop ChW Supply Splitter,  !- Name",
        "    Chilled Water Loop ChW Supply Inlet Branch,  !- Inlet Branch Name",
        "    Chilled Water Loop ChW Supply Bypass Branch,  !- Outlet Branch 1 Name",
        "    Main Chiller ChW Branch; !- Outlet Branch 2 Name",

        "Connector:Splitter,",
        "    Chilled Water Loop ChW Demand Splitter,  !- Name",
        "    Chilled Water Loop ChW Demand Inlet Branch,  !- Inlet Branch Name",
        "    Chilled Water Loop ChW Demand Bypass Branch,  !- Outlet Branch 1 Name",
        "    SPACE1-1 Cooling Coil ChW Branch,  !- Outlet Branch 2 Name",
        "    SPACE2-1 Cooling Coil ChW Branch,  !- Outlet Branch 3 Name",
        "    SPACE3-1 Cooling Coil ChW Branch,  !- Outlet Branch 4 Name",
        "    SPACE4-1 Cooling Coil ChW Branch,  !- Outlet Branch 5 Name",
        "    SPACE5-1 Cooling Coil ChW Branch,  !- Outlet Branch 6 Name",
        "    DOAS Cooling Coil ChW Branch;  !- Outlet Branch 7 Name",

        "Connector:Mixer,",
        "    Hot Water Loop HW Supply Mixer,  !- Name",
        "    Hot Water Loop HW Supply Outlet Branch,  !- Outlet Branch Name",
        "    Main Boiler HW Branch,   !- Inlet Branch 1 Name",
        "    Hot Water Loop HW Supply Bypass Branch;  !- Inlet Branch 2 Name",

        "Connector:Mixer,",
        "    Hot Water Loop HW Demand Mixer,  !- Name",
        "    Hot Water Loop HW Demand Outlet Branch,  !- Outlet Branch Name",
        "    Hot Water Loop HW Demand Bypass Branch,  !- Inlet Branch 1 Name",
        "    SPACE1-1 Heating Coil HW Branch,  !- Inlet Branch 2 Name",
        "    SPACE2-1 Heating Coil HW Branch,  !- Inlet Branch 3 Name",
        "    SPACE3-1 Heating Coil HW Branch,  !- Inlet Branch 4 Name",
        "    SPACE4-1 Heating Coil HW Branch,  !- Inlet Branch 5 Name",
        "    SPACE5-1 Heating Coil HW Branch,  !- Inlet Branch 6 Name",
        "    DOAS Heating Coil HW Branch;  !- Inlet Branch 7 Name",

        "Connector:Mixer,",
        "    Chilled Water Loop ChW Supply Mixer,  !- Name",
        "    Chilled Water Loop ChW Supply Outlet Branch,  !- Outlet Branch Name",
        "    Chilled Water Loop ChW Supply Bypass Branch,  !- Inlet Branch 1 Name",
        "    Main Chiller ChW Branch; !- Inlet Branch 2 Name",

        "Connector:Mixer,",
        "    Chilled Water Loop ChW Demand Mixer,  !- Name",
        "    Chilled Water Loop ChW Demand Outlet Branch,  !- Outlet Branch Name",
        "    Chilled Water Loop ChW Demand Bypass Branch,  !- Inlet Branch 1 Name",
        "    SPACE1-1 Cooling Coil ChW Branch,  !- Inlet Branch 2 Name",
        "    SPACE2-1 Cooling Coil ChW Branch,  !- Inlet Branch 3 Name",
        "    SPACE3-1 Cooling Coil ChW Branch,  !- Inlet Branch 4 Name",
        "    SPACE4-1 Cooling Coil ChW Branch,  !- Inlet Branch 5 Name",
        "    SPACE5-1 Cooling Coil ChW Branch,  !- Inlet Branch 6 Name",
        "    DOAS Cooling Coil ChW Branch;  !- Inlet Branch 7 Name",

        "ConnectorList,",
        "    Hot Water Loop HW Supply Side Connectors,  !- Name",
        "    Connector:Splitter,      !- Connector 1 Object Type",
        "    Hot Water Loop HW Supply Splitter,  !- Connector 1 Name",
        "    Connector:Mixer,         !- Connector 2 Object Type",
        "    Hot Water Loop HW Supply Mixer;  !- Connector 2 Name",

        "ConnectorList,",
        "    Hot Water Loop HW Demand Side Connectors,  !- Name",
        "    Connector:Splitter,      !- Connector 1 Object Type",
        "    Hot Water Loop HW Demand Splitter,  !- Connector 1 Name",
        "    Connector:Mixer,         !- Connector 2 Object Type",
        "    Hot Water Loop HW Demand Mixer;  !- Connector 2 Name",

        "ConnectorList,",
        "    Chilled Water Loop ChW Supply Side Connectors,  !- Name",
        "    Connector:Splitter,      !- Connector 1 Object Type",
        "    Chilled Water Loop ChW Supply Splitter,  !- Connector 1 Name",
        "    Connector:Mixer,         !- Connector 2 Object Type",
        "    Chilled Water Loop ChW Supply Mixer;  !- Connector 2 Name",

        "ConnectorList,",
        "    Chilled Water Loop ChW Demand Side Connectors,  !- Name",
        "    Connector:Splitter,      !- Connector 1 Object Type",
        "    Chilled Water Loop ChW Demand Splitter,  !- Connector 1 Name",
        "    Connector:Mixer,         !- Connector 2 Object Type",
        "    Chilled Water Loop ChW Demand Mixer;  !- Connector 2 Name",

        "NodeList,",
        "    SPACE1-1 Inlets,         !- Name",
        "    SPACE1-1 Supply Inlet,   !- Node 1 Name",
        "    SPACE1-1 DOAS Supply Inlet;  !- Node 2 Name",

        "NodeList,",
        "    SPACE2-1 Inlets,         !- Name",
        "    SPACE2-1 Supply Inlet,   !- Node 1 Name",
        "    SPACE2-1 DOAS Supply Inlet;  !- Node 2 Name",

        "NodeList,",
        "    SPACE3-1 Inlets,         !- Name",
        "    SPACE3-1 Supply Inlet,   !- Node 1 Name",
        "    SPACE3-1 DOAS Supply Inlet;  !- Node 2 Name",

        "NodeList,",
        "    SPACE4-1 Inlets,         !- Name",
        "    SPACE4-1 Supply Inlet,   !- Node 1 Name",
        "    SPACE4-1 DOAS Supply Inlet;  !- Node 2 Name",

        "NodeList,",
        "    SPACE5-1 Inlets,         !- Name",
        "    SPACE5-1 Supply Inlet,   !- Node 1 Name",
        "    SPACE5-1 DOAS Supply Inlet;  !- Node 2 Name",

        "NodeList,",
        "    DOAS Cooling Setpoint Nodes,  !- Name",
        "    DOAS Cooling Coil Outlet;!- Node 1 Name",

        "NodeList,",
        "    Hot Water Loop HW Supply Setpoint Nodes,  !- Name",
        "    Purchased Heat Outlet Node,   !- Node 1 Name",
        "    Hot Water Loop HW Supply Outlet;  !- Node 2 Name",

        "NodeList,",
        "    Chilled Water Loop ChW Supply Setpoint Nodes,  !- Name",
        "    Purchased Cooling Outlet Node, !- Node 1 Name",
        "    Chilled Water Loop ChW Supply Outlet;  !- Node 2 Name",

        "OutdoorAir:NodeList,",
        "    SPACE1-1 Outside Air Inlet;  !- Node or NodeList Name 1",

        "OutdoorAir:NodeList,",
        "    SPACE2-1 Outside Air Inlet;  !- Node or NodeList Name 1",

        "OutdoorAir:NodeList,",
        "    SPACE3-1 Outside Air Inlet;  !- Node or NodeList Name 1",

        "OutdoorAir:NodeList,",
        "    SPACE4-1 Outside Air Inlet;  !- Node or NodeList Name 1",

        "OutdoorAir:NodeList,",
        "    SPACE5-1 Outside Air Inlet;  !- Node or NodeList Name 1",

        "OutdoorAir:NodeList,",
        "    DOAS Outdoor Air Inlet;  !- Node or NodeList Name 1",

        "Pipe:Adiabatic,",
        "    Hot Water Loop HW Supply Side Bypass Pipe,  !- Name",
        "    Hot Water Loop HW Supply Bypass Inlet,  !- Inlet Node Name",
        "    Hot Water Loop HW Supply Bypass Outlet;  !- Outlet Node Name",

        "Pipe:Adiabatic,",
        "    Hot Water Loop HW Supply Outlet Pipe,  !- Name",
        "    Hot Water Loop HW Supply Outlet Pipe Inlet,  !- Inlet Node Name",
        "    Hot Water Loop HW Supply Outlet;  !- Outlet Node Name",

        "Pipe:Adiabatic,",
        "    Hot Water Loop HW Demand Inlet Pipe,  !- Name",
        "    Hot Water Loop HW Demand Inlet,  !- Inlet Node Name",
        "    Hot Water Loop HW Demand Inlet Pipe Outlet;  !- Outlet Node Name",

        "Pipe:Adiabatic,",
        "    Hot Water Loop HW Demand Side Bypass Pipe,  !- Name",
        "    Hot Water Loop HW Demand Bypass Inlet,  !- Inlet Node Name",
        "    Hot Water Loop HW Demand Bypass Outlet;  !- Outlet Node Name",

        "Pipe:Adiabatic,",
        "    Hot Water Loop HW Demand Outlet Pipe,  !- Name",
        "    Hot Water Loop HW Demand Outlet Pipe Inlet,  !- Inlet Node Name",
        "    Hot Water Loop HW Demand Outlet;  !- Outlet Node Name",

        "Pipe:Adiabatic,",
        "    Chilled Water Loop ChW Supply Side Bypass Pipe,  !- Name",
        "    Chilled Water Loop ChW Supply Bypass Inlet,  !- Inlet Node Name",
        "    Chilled Water Loop ChW Supply Bypass Outlet;  !- Outlet Node Name",

        "Pipe:Adiabatic,",
        "    Chilled Water Loop ChW Supply Outlet Pipe,  !- Name",
        "    Chilled Water Loop ChW Supply Outlet Pipe Inlet,  !- Inlet Node Name",
        "    Chilled Water Loop ChW Supply Outlet;  !- Outlet Node Name",

        "Pipe:Adiabatic,",
        "    Chilled Water Loop ChW Demand Inlet Pipe,  !- Name",
        "    Chilled Water Loop ChW Demand Inlet,  !- Inlet Node Name",
        "    Chilled Water Loop ChW Demand Inlet Pipe Outlet;  !- Outlet Node Name",

        "Pipe:Adiabatic,",
        "    Chilled Water Loop ChW Demand Side Bypass Pipe,  !- Name",
        "    Chilled Water Loop ChW Demand Bypass Inlet,  !- Inlet Node Name",
        "    Chilled Water Loop ChW Demand Bypass Outlet;  !- Outlet Node Name",

        "Pipe:Adiabatic,",
        "    Chilled Water Loop ChW Demand Outlet Pipe,  !- Name",
        "    Chilled Water Loop ChW Demand Outlet Pipe Inlet,  !- Inlet Node Name",
        "    Chilled Water Loop ChW Demand Outlet;  !- Outlet Node Name",

        "Pump:ConstantSpeed,",
        "    Hot Water Loop HW Supply Pump,  !- Name",
        "    Hot Water Loop HW Supply Inlet,  !- Inlet Node Name",
        "    Hot Water Loop HW Pump Outlet,  !- Outlet Node Name",
        "    autosize,                !- Rated Flow Rate {m3/s}",
        "    179352,                  !- Rated Pump Head {Pa}",
        "    autosize,                !- Rated Power Consumption {W}",
        "    0.9,                     !- Motor Efficiency",
        "    0,                       !- Fraction of Motor Inefficiencies to Fluid Stream",
        "    INTERMITTENT;            !- Pump Control Type",

        "Pump:ConstantSpeed,",
        "    Chilled Water Loop ChW Supply Pump,  !- Name",
        "    Chilled Water Loop ChW Supply Inlet,  !- Inlet Node Name",
        "    Chilled Water Loop ChW Pump Outlet,  !- Outlet Node Name",
        "    autosize,                !- Rated Flow Rate {m3/s}",
        "    179352,                  !- Rated Pump Head {Pa}",
        "    autosize,                !- Rated Power Consumption {W}",
        "    0.9,                     !- Motor Efficiency",
        "    0,                       !- Fraction of Motor Inefficiencies to Fluid Stream",
        "    INTERMITTENT;            !- Pump Control Type",

        "  DistrictHeating,",
        "    Purchased Heating,          !- Name",
        "    Purchased Heat Inlet Node,  !- Hot Water Inlet Node Name",
        "    Purchased Heat Outlet Node, !- Hot Water Outlet Node Name",
        "    35000;                      !- Nominal Capacity {W}",

        "  DistrictCooling,",
        "    Purchased Cooling,       !- Name",
        "    Purchased Cooling Inlet Node,  !- Chilled Water Inlet Node Name",
        "    Purchased Cooling Outlet Node,  !- Chilled Water Outlet Node Name",
        "    35000;                   !- Nominal Capacity {W}",

        "PlantLoop,",
        "    Hot Water Loop Hot Water Loop,  !- Name",
        "    Water,                   !- Fluid Type",
        "    ,                        !- User Defined Fluid Type",
        "    Hot Water Loop Operation,!- Plant Equipment Operation Scheme Name",
        "    Hot Water Loop HW Supply Outlet,  !- Loop Temperature Setpoint Node Name",
        "    100,                     !- Maximum Loop Temperature {C}",
        "    10,                      !- Minimum Loop Temperature {C}",
        "    autosize,                !- Maximum Loop Flow Rate {m3/s}",
        "    0,                       !- Minimum Loop Flow Rate {m3/s}",
        "    autosize,                !- Plant Loop Volume {m3}",
        "    Hot Water Loop HW Supply Inlet,  !- Plant Side Inlet Node Name",
        "    Hot Water Loop HW Supply Outlet,  !- Plant Side Outlet Node Name",
        "    Hot Water Loop HW Supply Side Branches,  !- Plant Side Branch List Name",
        "    Hot Water Loop HW Supply Side Connectors,  !- Plant Side Connector List Name",
        "    Hot Water Loop HW Demand Inlet,  !- Demand Side Inlet Node Name",
        "    Hot Water Loop HW Demand Outlet,  !- Demand Side Outlet Node Name",
        "    Hot Water Loop HW Demand Side Branches,  !- Demand Side Branch List Name",
        "    Hot Water Loop HW Demand Side Connectors,  !- Demand Side Connector List Name",
        "    SequentialLoad,          !- Load Distribution Scheme",
        "    ,                        !- Availability Manager List Name",
        "    SingleSetpoint,          !- Plant Loop Demand Calculation Scheme",
        "    ,                        !- Common Pipe Simulation",
        "    ,                        !- Pressure Simulation Type",
        "    2.0;                     !- Loop Circulation Time {minutes}",

        "PlantLoop,",
        "    Chilled Water Loop Chilled Water Loop,  !- Name",
        "    Water,                   !- Fluid Type",
        "    ,                        !- User Defined Fluid Type",
        "    Chilled Water Loop Chiller Operation,  !- Plant Equipment Operation Scheme Name",
        "    Chilled Water Loop ChW Supply Outlet,  !- Loop Temperature Setpoint Node Name",
        "    98,                      !- Maximum Loop Temperature {C}",
        "    1,                       !- Minimum Loop Temperature {C}",
        "    autosize,                !- Maximum Loop Flow Rate {m3/s}",
        "    0,                       !- Minimum Loop Flow Rate {m3/s}",
        "    autosize,                !- Plant Loop Volume {m3}",
        "    Chilled Water Loop ChW Supply Inlet,  !- Plant Side Inlet Node Name",
        "    Chilled Water Loop ChW Supply Outlet,  !- Plant Side Outlet Node Name",
        "    Chilled Water Loop ChW Supply Side Branches,  !- Plant Side Branch List Name",
        "    Chilled Water Loop ChW Supply Side Connectors,  !- Plant Side Connector List Name",
        "    Chilled Water Loop ChW Demand Inlet,  !- Demand Side Inlet Node Name",
        "    Chilled Water Loop ChW Demand Outlet,  !- Demand Side Outlet Node Name",
        "    Chilled Water Loop ChW Demand Side Branches,  !- Demand Side Branch List Name",
        "    Chilled Water Loop ChW Demand Side Connectors,  !- Demand Side Connector List Name",
        "    SequentialLoad,          !- Load Distribution Scheme",
        "    ,                        !- Availability Manager List Name",
        "    SingleSetpoint,          !- Plant Loop Demand Calculation Scheme",
        "    None,                    !- Common Pipe Simulation",
        "    ,                        !- Pressure Simulation Type",
        "    2.0;                     !- Loop Circulation Time {minutes}",

        "PlantEquipmentList,",
        "    Hot Water Loop All Equipment,           !- Name",
        "    DistrictHeating,          !- Equipment 1 Object Type",
        "    Purchased Heating;        !- Equipment 1 Name",

        "PlantEquipmentList,",
        "    Chilled Water Loop All Chillers,  !- Name",
        "    DistrictCooling,    !- Equipment 1 Object Type",
        "    Purchased Cooling;            !- Equipment 1 Name",

        "PlantEquipmentOperation:CoolingLoad,",
        "    Chilled Water Loop Chiller Operation All Hours,  !- Name",
        "    0,                       !- Load Range 1 Lower Limit {W}",
        "    1000000000000000,        !- Load Range 1 Upper Limit {W}",
        "    Chilled Water Loop All Chillers;  !- Range 1 Equipment List Name",

        "PlantEquipmentOperation:HeatingLoad,",
        "    Hot Water Loop Operation All Hours,  !- Name",
        "    0,                       !- Load Range 1 Lower Limit {W}",
        "    1000000000000000,        !- Load Range 1 Upper Limit {W}",
        "    Hot Water Loop All Equipment;  !- Range 1 Equipment List Name",

        "PlantEquipmentOperationSchemes,",
        "    Hot Water Loop Operation,!- Name",
        "    PlantEquipmentOperation:HeatingLoad,  !- Control Scheme 1 Object Type",
        "    Hot Water Loop Operation All Hours,  !- Control Scheme 1 Name",
        "    HVACTemplate-Always 1;   !- Control Scheme 1 Schedule Name",

        "PlantEquipmentOperationSchemes,",
        "    Chilled Water Loop Chiller Operation,  !- Name",
        "    PlantEquipmentOperation:CoolingLoad,  !- Control Scheme 1 Object Type",
        "    Chilled Water Loop Chiller Operation All Hours,  !- Control Scheme 1 Name",
        "    HVACTemplate-Always 1;   !- Control Scheme 1 Schedule Name",

        "AvailabilityManager:Scheduled,",
        "    DOAS Availability,       !- Name",
        "    FanAvailSched;                !- Schedule Name",

        "AvailabilityManagerAssignmentList,",
        "    DOAS Availability Managers,  !- Name",
        "    AvailabilityManager:Scheduled,  !- Availability Manager 1 Object Type",
        "    DOAS Availability;       !- Availability Manager 1 Name",

        "SetpointManager:Scheduled,",
        "    DOAS Heating Supply Air Temp Manager,",
        "    Temperature,",
        "    HVACTemplate-Always 12.2,",
        "    DOAS Supply Path Inlet;",

        "SetpointManager:OutdoorAirReset,",
        "    DOAS Cooling Supply Air Temp Manager, !- Name",
        "    Temperature,  !- Control Variable",
        "    18.3,                    !- Setpoint at Outdoor Low Temperature {C}",
        "    4.4,                    !- Outdoor Low Temperature (C)",
        "    12.8,                    !- Setpoint at Outdoor High Temperature (C)",
        "    26.7,                    !- Outdoor High Temperature (C)",
        "    DOAS Supply Fan Outlet;  !- Setpoint Node or NodeList Name",

        "SetpointManager:OutdoorAirReset,",
        "    Hot Water Loop HW Temp Manager,  !- Name",
        "    Temperature,             !- Control Variable",
        "    82.2,                    !- Setpoint at Outdoor Low Temperature {C}",
        "    -6.7,                    !- Outdoor Low Temperature {C}",
        "    65.6,                    !- Setpoint at Outdoor High Temperature {C}",
        "    10,                      !- Outdoor High Temperature {C}",
        "    Hot Water Loop HW Supply Setpoint Nodes;  !- Setpoint Node or NodeList Name",

        "SetpointManager:OutdoorAirReset,",
        "    Chilled Water Loop ChW Temp Manager,  !- Name",
        "    Temperature,             !- Control Variable",
        "    12.2,                    !- Setpoint at Outdoor Low Temperature {C}",
        "    15.6,                    !- Outdoor Low Temperature {C}",
        "    6.7,                     !- Setpoint at Outdoor High Temperature {C}",
        "    26.7,                    !- Outdoor High Temperature {C}",
        "    Chilled Water Loop ChW Supply Setpoint Nodes;  !- Setpoint Node or NodeList Name",

        "SetpointManager:MixedAir,",
        "    DOAS Cooling Coil Air Temp Manager,  !- Name",
        "    Temperature,             !- Control Variable",
        "    DOAS Supply Fan Outlet,  !- Reference Setpoint Node Name",
        "    DOAS Heating Coil Outlet,!- Fan Inlet Node Name",
        "    DOAS Supply Fan Outlet,  !- Fan Outlet Node Name",
        "    DOAS Cooling Setpoint Nodes;  !- Setpoint Node or NodeList Name",

        "SetpointManager:MixedAir,",
        "    DOAS Heating Coil Air Temp Manager,  !- Name",
        "    Temperature,             !- Control Variable",
        "    DOAS Supply Path Inlet,  !- Reference Setpoint Node Name",
        "    DOAS Heating Coil Outlet,!- Fan Inlet Node Name",
        "    DOAS Supply Fan Outlet,  !- Fan Outlet Node Name",
        "    DOAS Heating Coil Outlet;!- Setpoint Node or NodeList Name",

        "SetpointManager:MixedAir,",
        "    DOAS Heat Recovery Air Temp Manager,  !- Name",
        "    Temperature,             !- Control Variable",
        "    DOAS Supply Path Inlet,  !- Reference Setpoint Node Name",
        "    DOAS Heating Coil Outlet,!- Fan Inlet Node Name",
        "    DOAS Supply Fan Outlet,  !- Fan Outlet Node Name",
        "    DOAS Heat Recovery Supply Outlet;  !- Setpoint Node or NodeList Name",

        "SetpointManager:MixedAir,",
        "    DOAS Heat Recovery Economizer Temp Manager,  !- Name",
        "    Temperature,             !- Control Variable",
        "    DOAS Supply Fan Outlet,  !- Reference Setpoint Node Name",
        "    DOAS Heating Coil Outlet,!- Fan Inlet Node Name",
        "    DOAS Supply Fan Outlet,  !- Fan Outlet Node Name",
        "    DOAS Mixed Air Outlet;   !- Setpoint Node or NodeList Name",

    });

    ASSERT_TRUE(process_idf(idf_objects));
    GetOAControllerInputs();

    EXPECT_EQ(6, GetNumOAMixers());
    EXPECT_EQ(1, GetNumOAControllers());
    EXPECT_EQ(18, GetOAMixerReliefNodeNumber(1));

    // indexes can be found in  OAMixer array for these feild names
    EXPECT_EQ(1, GetOAMixerIndex("SPACE1-1 OA Mixing Box"));
    EXPECT_EQ(2, GetOAMixerIndex("SPACE2-1 OA Mixing Box"));
    EXPECT_EQ(3, GetOAMixerIndex("SPACE3-1 OA Mixing Box"));
    EXPECT_EQ(4, GetOAMixerIndex("SPACE4-1 OA Mixing Box"));
    EXPECT_EQ(5, GetOAMixerIndex("SPACE5-1 OA Mixing Box"));
    EXPECT_EQ(6, GetOAMixerIndex("DOAS OA Mixing Box"));
}

TEST_F(EnergyPlusFixture, MechVentController_IAQPTests)
{
    Contaminant.CO2Simulation = true;
    Contaminant.GenericContamSimulation = true;

    std::string const idf_objects = delimited_string({"Version,8.6;",
                                                      "  Controller:MechanicalVentilation,",
                                                      "    DCVObject, !- Name",
                                                      "    , !- Availability Schedule Name",
                                                      "    , !- Demand Controlled Ventilation",
                                                      "    IndoorAirQualityProcedure, !- System Outdoor Air Method",
                                                      "     , !- Zone Maximum Outdoor Air Fraction{ dimensionless }",
                                                      "    Zone 1, !- Zone 1 Name",
                                                      "    , !- Design Specification Outdoor Air Object Name 1",
                                                      "    , !- Design Specification Zone Air Distribution Object Name 1",
                                                      "    Zone 2, !- Zone 1 Name",
                                                      "    , !- Design Specification Outdoor Air Object Name 1",
                                                      "    ; !- Design Specification Zone Air Distribution Object Name 1",
                                                      "    Zone, Zone 1;",
                                                      "    Zone, Zone 2;"});

    ASSERT_TRUE(process_idf(idf_objects));

    bool ErrorsFound(false);
    GetZoneData(ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    int NumZones(2);
    Real64 SysMassFlow(0.0); // System supply mass flow rate [kg/s]
    Real64 OAMassFlow(0.0);  // OA mass flow rate [kg/s]
    DataContaminantBalance::ZoneSysContDemand.allocate(NumZones);
    DataContaminantBalance::ZoneSysContDemand(1).OutputRequiredToCO2SP = 0.2;
    DataContaminantBalance::ZoneSysContDemand(2).OutputRequiredToCO2SP = 0.3;
    DataContaminantBalance::ZoneSysContDemand(1).OutputRequiredToGCSP = 1.0;
    DataContaminantBalance::ZoneSysContDemand(2).OutputRequiredToGCSP = 0.5;

    GetOAControllerInputs();

    // Case 1 - System OA method = IndoorAirQualityProcedure, SOAM_IAQP, controls to OutputRequiredToCO2SP
    OAMassFlow = 0.0;
    EXPECT_EQ(SOAM_IAQP, VentilationMechanical(1).SystemOAMethod);
    VentilationMechanical(1).CalcMechVentController(SysMassFlow, OAMassFlow);
    EXPECT_EQ(0.5, OAMassFlow);

    // Case 2 - System OA method = IndoorAirQualityProcedureGenericContaminant, SOAM_IAQPGC, controls to OutputRequiredToGCSP
    OAMassFlow = 0.0;
    VentilationMechanical(1).SystemOAMethod = SOAM_IAQPGC;
    VentilationMechanical(1).CalcMechVentController(SysMassFlow, OAMassFlow);
    EXPECT_EQ(1.5, OAMassFlow);

    // Case 3 - System OA method = IndoorAirQualityProcedureCombined, SOAM_IAQPCOM, controls to greater of total OutputRequiredToCO2SP and
    // OutputRequiredToGCSP
    OAMassFlow = 0.0;
    VentilationMechanical(1).SystemOAMethod = SOAM_IAQPCOM;
    VentilationMechanical(1).CalcMechVentController(SysMassFlow, OAMassFlow);
    EXPECT_EQ(1.5, OAMassFlow);

    // Case 4 - System OA method = IndoorAirQualityProcedureCombined, SOAM_IAQPCOM, set zone OA schedules to alwaysoff
    ScheduleManager::Schedule.allocate(1);
    ScheduleManager::Schedule(1).CurrentValue = 0.0;
    VentilationMechanical(1).ZoneOASchPtr(1) = 1;
    VentilationMechanical(1).ZoneOASchPtr(2) = 1;

    OAMassFlow = 0.0;
    VentilationMechanical(1).SystemOAMethod = SOAM_IAQPCOM;
    VentilationMechanical(1).CalcMechVentController(SysMassFlow, OAMassFlow);
    EXPECT_EQ(0.0, OAMassFlow);

    DataContaminantBalance::ZoneSysContDemand.deallocate();
    ScheduleManager::Schedule.deallocate();
}

TEST_F(EnergyPlusFixture, MechVentController_ZoneSumTests)
{
    Contaminant.CO2Simulation = true;
    Contaminant.CO2OutdoorSchedPtr = 1;

    std::string const idf_objects = delimited_string({"Version,8.6;",
                                                      "  Controller:MechanicalVentilation,",
                                                      "    DCVObject, !- Name",
                                                      "    , !- Availability Schedule Name",
                                                      "    Yes, !- Demand Controlled Ventilation",
                                                      "    ZoneSum, !- System Outdoor Air Method",
                                                      "     , !- Zone Maximum Outdoor Air Fraction{ dimensionless }",
                                                      "    Zone 1, !- Zone 1 Name",
                                                      "    Zone 1 DSOA, !- Design Specification Outdoor Air Object Name 1",
                                                      "    , !- Design Specification Zone Air Distribution Object Name 1",
                                                      "    Zone 2, !- Zone 1 Name",
                                                      "    Zone 2 DSOA, !- Design Specification Outdoor Air Object Name 1",
                                                      "    , !- Design Specification Zone Air Distribution Object Name 1",
                                                      "    Zone 3, !- Zone 1 Name",
                                                      "    Zone 3 DSOA, !- Design Specification Outdoor Air Object Name 1",
                                                      "    , !- Design Specification Zone Air Distribution Object Name 1",
                                                      "    Zone 4, !- Zone 1 Name",
                                                      "    Zone 4 DSOA, !- Design Specification Outdoor Air Object Name 1",
                                                      "    , !- Design Specification Zone Air Distribution Object Name 1",
                                                      "    Zone 5, !- Zone 1 Name",
                                                      "    Zone 5 DSOA, !- Design Specification Outdoor Air Object Name 1",
                                                      "    , !- Design Specification Zone Air Distribution Object Name 1",
                                                      "    Zone 6, !- Zone 1 Name",
                                                      "    Zone 6 DSOA, !- Design Specification Outdoor Air Object Name 1",
                                                      "    ; !- Design Specification Zone Air Distribution Object Name 1",
                                                      "DesignSpecification:OutdoorAir,",
                                                      "    Zone 1 DSOA,             !- Name",
                                                      "    flow/person,             !- Outdoor Air Method",
                                                      "    0.1,                     !- Outdoor Air Flow per Person {m3/s-person}",
                                                      "    0.0,                     !- Outdoor Air Flow per Zone Floor Area {m3/s-m2}",
                                                      "    0.0,                     !- Outdoor Air Flow per Zone {m3/s}",
                                                      "    0.0,                     !- Outdoor Air Flow Air Changes per Hour {1/hr}",
                                                      "    Zone 1 OA Schedule;      !- Outdoor Air Schedule Name",
                                                      "DesignSpecification:OutdoorAir,",
                                                      "    Zone 2 DSOA,             !- Name",
                                                      "    flow/area,               !- Outdoor Air Method",
                                                      "    0.0,                     !- Outdoor Air Flow per Person {m3/s-person}",
                                                      "    1.0,                     !- Outdoor Air Flow per Zone Floor Area {m3/s-m2}",
                                                      "    0.0,                     !- Outdoor Air Flow per Zone {m3/s}",
                                                      "    0.0,                     !- Outdoor Air Flow Air Changes per Hour {1/hr}",
                                                      "    Zone 2 OA Schedule;      !- Outdoor Air Schedule Name",
                                                      "DesignSpecification:OutdoorAir,",
                                                      "    Zone 3 DSOA,             !- Name",
                                                      "    flow/zone,               !- Outdoor Air Method",
                                                      "    0.0,                     !- Outdoor Air Flow per Person {m3/s-person}",
                                                      "    0.0,                     !- Outdoor Air Flow per Zone Floor Area {m3/s-m2}",
                                                      "    3.0,                     !- Outdoor Air Flow per Zone {m3/s}",
                                                      "    0.0,                     !- Outdoor Air Flow Air Changes per Hour {1/hr}",
                                                      "    Zone 3 OA Schedule;      !- Outdoor Air Schedule Name",
                                                      "DesignSpecification:OutdoorAir,",
                                                      "    Zone 4 DSOA,             !- Name",
                                                      "    AirChanges/Hour,         !- Outdoor Air Method",
                                                      "    0.0,                     !- Outdoor Air Flow per Person {m3/s-person}",
                                                      "    0.0,                     !- Outdoor Air Flow per Zone Floor Area {m3/s-m2}",
                                                      "    0.0,                     !- Outdoor Air Flow per Zone {m3/s}",
                                                      "    5.0,                     !- Outdoor Air Flow Air Changes per Hour {1/hr}",
                                                      "    Zone 4 OA Schedule;      !- Outdoor Air Schedule Name",
                                                      "DesignSpecification:OutdoorAir,",
                                                      "    Zone 5 DSOA,             !- Name",
                                                      "    Sum,                     !- Outdoor Air Method",
                                                      "    0.2,                     !- Outdoor Air Flow per Person {m3/s-person}",
                                                      "    2.0,                     !- Outdoor Air Flow per Zone Floor Area {m3/s-m2}",
                                                      "    5.0,                     !- Outdoor Air Flow per Zone {m3/s}",
                                                      "    4.0,                     !- Outdoor Air Flow Air Changes per Hour {1/hr}",
                                                      "    Zone 5 OA Schedule;      !- Outdoor Air Schedule Name",
                                                      "DesignSpecification:OutdoorAir,",
                                                      "    Zone 6 DSOA,             !- Name",
                                                      "    Maximum,                 !- Outdoor Air Method",
                                                      "    0.3,                     !- Outdoor Air Flow per Person {m3/s-person}",
                                                      "    1.0,                     !- Outdoor Air Flow per Zone Floor Area {m3/s-m2}",
                                                      "    1.0,                     !- Outdoor Air Flow per Zone {m3/s}",
                                                      "    0.1,                     !- Outdoor Air Flow Air Changes per Hour {1/hr}",
                                                      "    Zone 6 OA Schedule;      !- Outdoor Air Schedule Name",
                                                      "Schedule:Constant, Zone 1 OA Schedule, , 0.1;",
                                                      "Schedule:Constant, Zone 2 OA Schedule, , 0.2;",
                                                      "Schedule:Constant, Zone 3 OA Schedule, , 0.3;",
                                                      "Schedule:Constant, Zone 4 OA Schedule, , 0.4;",
                                                      "Schedule:Constant, Zone 5 OA Schedule, , 0.5;",
                                                      "Schedule:Constant, Zone 6 OA Schedule, , 0.6;",
                                                      "Zone,",
                                                      "    Zone 1,                  !- Name",
                                                      "    0,                       !- Direction of Relative North {deg}",
                                                      "    0, 0, 0,                 !- X,Y,Z  {m}",
                                                      "    1,                       !- Type",
                                                      "    1,                       !- Multiplier",
                                                      "    ,                        !- Ceiling Height {m}",
                                                      "    ,                        !- Volume {m3}",
                                                      "    100;                     !- Floor Area {m2}",
                                                      "Zone,",
                                                      "    Zone 2,                  !- Name",
                                                      "    0,                       !- Direction of Relative North {deg}",
                                                      "    0, 0, 0,                 !- X,Y,Z  {m}",
                                                      "    1,                       !- Type",
                                                      "    1,                       !- Multiplier",
                                                      "    ,                        !- Ceiling Height {m}",
                                                      "    ,                        !- Volume {m3}",
                                                      "    200;                     !- Floor Area {m2}",
                                                      "Zone,",
                                                      "    Zone 3,                  !- Name",
                                                      "    0,                       !- Direction of Relative North {deg}",
                                                      "    0, 0, 0,                 !- X,Y,Z  {m}",
                                                      "    1,                       !- Type",
                                                      "    1,                       !- Multiplier",
                                                      "    ,                        !- Ceiling Height {m}",
                                                      "    ,                        !- Volume {m3}",
                                                      "    300;                     !- Floor Area {m2}",
                                                      "Zone,",
                                                      "    Zone 4,                  !- Name",
                                                      "    0,                       !- Direction of Relative North {deg}",
                                                      "    0, 0, 0,                 !- X,Y,Z  {m}",
                                                      "    1,                       !- Type",
                                                      "    1,                       !- Multiplier",
                                                      "    ,                        !- Ceiling Height {m}",
                                                      "    3600,                    !- Volume {m3}",
                                                      "    400;                     !- Floor Area {m2}",
                                                      "Zone,",
                                                      "    Zone 5,                  !- Name",
                                                      "    0,                       !- Direction of Relative North {deg}",
                                                      "    0, 0, 0,                 !- X,Y,Z  {m}",
                                                      "    1,                       !- Type",
                                                      "    1,                       !- Multiplier",
                                                      "    ,                        !- Ceiling Height {m}",
                                                      "    7200,                    !- Volume {m3}",
                                                      "    100;                     !- Floor Area {m2}",
                                                      "Zone,",
                                                      "    Zone 6,                  !- Name",
                                                      "    0,                       !- Direction of Relative North {deg}",
                                                      "    0, 0, 0,                 !- X,Y,Z  {m}",
                                                      "    1,                       !- Type",
                                                      "    5,                       !- Multiplier",
                                                      "    ,                        !- Ceiling Height {m}",
                                                      "    3600,                    !- Volume {m3}",
                                                      "    600;                     !- Floor Area {m2}"});

    ASSERT_TRUE(process_idf(idf_objects));

    bool ErrorsFound(false);
    GetZoneData(ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    // Initialize schedule values
    DataGlobals::NumOfTimeStepInHour = 1;
    DataGlobals::MinutesPerTimeStep = 60 / DataGlobals::NumOfTimeStepInHour;
    DataGlobals::TimeStep = 1;
    DataGlobals::HourOfDay = 1;
    DataEnvironment::DayOfWeek = 1;
    DataEnvironment::DayOfYear_Schedule = 100;
    ScheduleManager::UpdateScheduleValues();

    // Initialize zone areas and volumes - too many other things need to be set up to do these in the normal routines
    int NumZones(6);
    for (int index = 1; index <= NumZones; ++index) {
        DataHeatBalance::Zone(index).FloorArea = DataHeatBalance::Zone(index).UserEnteredFloorArea;
    }

    Real64 SysMassFlow(0.0);          // System supply mass flow rate [kg/s]
    Real64 OAMassFlow(0.0);           // OA mass flow rate [kg/s]
    DataEnvironment::StdRhoAir = 1.0; // For convenience so mass flow returned will equal volume flows input

    DataHeatBalance::ZoneIntGain.allocate(NumZones);
    DataHeatBalance::ZoneIntGain(1).NOFOCC = 10;
    DataHeatBalance::ZoneIntGain(2).NOFOCC = 2;
    DataHeatBalance::ZoneIntGain(3).NOFOCC = 3;
    DataHeatBalance::ZoneIntGain(4).NOFOCC = 4;
    DataHeatBalance::ZoneIntGain(5).NOFOCC = 20;
    DataHeatBalance::ZoneIntGain(6).NOFOCC = 6;

    SizingManager::GetOARequirements();
    GetOAControllerInputs();
    EXPECT_EQ(SOAM_ZoneSum, VentilationMechanical(1).SystemOAMethod);

    // Summary of inputs and expected OA flow rate for each zone, StdRho = 1, so mass flow = volume flow for these tests
    // Zone 1 - flow/person, 0.1 m3/s/person, 10 persons, OA=1 m3/s
    // Zone 2 - flow/area, 1.0 m3/s-m2, area 200 m2, OA=200 m3/s
    // Zone 3 - flow/zone, 3.0 m3/s-zone, OA=3.0 m3/s
    // Zone 4 - AirChanges/Hour, 5.0 ACH, volume 3600 m3, OA=5 m3/s (ACH/3600=air change/sec)
    // Zone 5 - Sum, 0.2 m3/s/person, 20 persons [4], 2 m3/s-m2, area 100 [200], 5 m3/s-zone [5], 4 ACH, volume 7200 m3 [8], OA=4+200+5+8=217 m3/s
    // Zone 6 - Maximum, 0.3 m3/s/person, 6 persons [1.8], 1 m3/s-m2, area 600 [600], 1 m3/s-zone [1], 0.1 ACH, volume 3600 m3 [0.1],
    // OA=max(1.8+600+1+0.1=600 m3/s

    // Apply schedules and zone multipliers
    // Zone 1 - schedule = 0.1, multiplier = 1.0, OA=1*0.1*1  =   0.1 m3/s
    // Zone 2 - schedule = 0.2, multiplier = 1.0, OA=200*0.2*1=  40.0 m3/s
    // Zone 3 - schedule = 0.3, multiplier = 1.0, OA=3*0.3*1  =   0.9 m3/s
    // Zone 4 - schedule = 0.4, multiplier = 1.0, OA=5*0.4*1  =   2.0 m3/s
    // Zone 5 - schedule = 0.5, multiplier = 1.0, OA=217*0.5*1= 108.5 m3/s
    // Zone 6 - schedule = 0.6, multiplier = 5.0, OA=600*0.6*5=1800.0 m3/s
    // Total for all zones = 1951.5 m3/s

    // Case 1 - All zones as initially set up
    OAMassFlow = 0.0;
    VentilationMechanical(1).CalcMechVentController(SysMassFlow, OAMassFlow);
    EXPECT_NEAR(1951.5, OAMassFlow, 0.00001);

    // Case 2 - Turn off Zone 4-6
    OAMassFlow = 0.0;
    ScheduleManager::Schedule(4).CurrentValue = 0.0;
    ScheduleManager::Schedule(5).CurrentValue = 0.0;
    ScheduleManager::Schedule(6).CurrentValue = 0.0;
    VentilationMechanical(1).CalcMechVentController(SysMassFlow, OAMassFlow);
    EXPECT_NEAR(41.0, OAMassFlow, 0.00001);

    // Case 3 - Turn off remaining zones
    OAMassFlow = 0.0;
    ScheduleManager::Schedule(1).CurrentValue = 0.0;
    ScheduleManager::Schedule(2).CurrentValue = 0.0;
    ScheduleManager::Schedule(3).CurrentValue = 0.0;
    VentilationMechanical(1).CalcMechVentController(SysMassFlow, OAMassFlow);
    EXPECT_EQ(0.0, OAMassFlow);

    DataHeatBalance::ZoneIntGain.deallocate();
}

TEST_F(EnergyPlusFixture, CO2ControlDesignOARateTest)
{
    // Test a new feature: Proportional Demand Control Ventilation (DCV) Enhancements
    Contaminant.CO2Simulation = true;
    Contaminant.CO2OutdoorSchedPtr = 1;

    std::string const idf_objects = delimited_string({
        "Version,8.3;",
        "  OutdoorAir:Node,",
        "    Outside Air Inlet Node; !- Name",
        "  Schedule:Constant,",
        "    VentSchedule, !- Name",
        "     , !- Schedule Type Limits Name",
        "     1; !- Hourly value",
        "  Schedule:Constant,",
        "    ZoneADEffSch, !- Name",
        "     , !- Schedule Type Limits Name",
        "     1; !- Hourly value",
        "  Schedule:Constant,",
        "    OAFractionSched, !- Name",
        "     , !- Schedule Type Limits Name",
        "     1; !- Hourly value",
        "  Schedule:Constant,",
        "    CO2AvailSchedule, !- Name",
        "     , !- Schedule Type Limits Name",
        "     1.0; !- Hourly value",
        "  Schedule:Constant,",
        "    CO2SetpointSchedule, !- Name",
        "     , !- Schedule Type Limits Name",
        "     900.0; !- Hourly value",
        "  Schedule:Constant,",
        "    CO2MinSchedule, !- Name",
        "     , !- Schedule Type Limits Name",
        "     300.0; !- Hourly value",
        "  Schedule:Constant,",
        "    CO2MaxSchedule, !- Name",
        "     , !- Schedule Type Limits Name",
        "     900.0; !- Hourly value",
        "  Schedule:Constant,",
        "    Minimum Outdoor Air Flow Rate Schedule, !- Name",
        "     , !- Schedule Type Limits Name",
        "     0.001; !- Hourly value",
        "  Controller:OutdoorAir,",
        "    OA Controller 1, !- Name",
        "    Relief Air Outlet Node, !- Relief Air Outlet Node Name",
        "    Outdoor Air Mixer Inlet Node, !- Return Air Node Name",
        "    Mixed Air Node, !- Mixed Air Node Name",
        "    Outside Air Inlet Node, !- Actuator Node Name",
        "    0.0, !- Minimum Outdoor Air Flow Rate{ m3 / s }",
        "    1.7, !- Maximum Outdoor Air Flow Rate{ m3 / s }",
        "    NoEconomizer, !- Economizer Control Type",
        "    ModulateFlow, !- Economizer Control Action Type",
        "    , !- Economizer Maximum Limit Dry - Bulb Temperature{ C }",
        "    , !- Economizer Maximum Limit Enthalpy{ J / kg }",
        "    , !- Economizer Maximum Limit Dewpoint Temperature{ C }",
        "    , !- Electronic Enthalpy Limit Curve Name",
        "    , !- Economizer Minimum Limit Dry - Bulb Temperature{ C }",
        "    NoLockout, !- Lockout Type",
        "    FixedMinimum, !- Minimum Limit Type",
        "    OAFractionSched, !- Minimum Outdoor Air Schedule Name",
        "    , !- Minimum Fraction of Outdoor Air Schedule Name",
        "    , !- Maximum Fraction of Outdoor Air Schedule Name",
        "    DCVObject;               !- Mechanical Ventilation Controller Name",
        "  Controller:MechanicalVentilation,",
        "    DCVObject, !- Name",
        "    VentSchedule, !- Availability Schedule Name",
        "    Yes, !- Demand Controlled Ventilation",
        "    ProportionalControlBasedOnDesignOARate, !- System Outdoor Air Method",
        "     , !- Zone Maximum Outdoor Air Fraction{ dimensionless }",
        "    West Zone, !- Zone 1 Name",
        "    CM DSOA West Zone, !- Design Specification Outdoor Air Object Name 1",
        "    CM DSZAD West Zone; !- Design Specification Zone Air Distribution Object Name 1",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    ContaminantControlledZone.allocate(1);
    ContaminantControlledZone(1).AvaiSchedPtr = 4;
    ContaminantControlledZone(1).SPSchedIndex = 5;
    ContaminantControlledZone(1).ZoneMinCO2SchedIndex = 6;
    ContaminantControlledZone(1).ZoneMaxCO2SchedIndex = 7;

    AirLoopControlInfo.allocate(1);
    AirLoopControlInfo(1).LoopFlowRateSet = true;
    OARequirements.allocate(1);
    OARequirements(1).Name = "CM DSOA WEST ZONE";
    OARequirements(1).OAFlowMethod = OAFlowSum;
    OARequirements(1).OAFlowPerPerson = 0.003149;
    OARequirements(1).OAFlowPerArea = 0.000407;
    OARequirements(1).OAPropCtlMinRateSchPtr = 8;

    ZoneAirDistribution.allocate(1);
    ZoneAirDistribution(1).Name = "CM DSZAD WEST ZONE";
    ZoneAirDistribution(1).ZoneADEffSchPtr = 4;

    Zone.allocate(1);
    Zone(1).Name = "WEST ZONE";
    Zone(1).FloorArea = 10.0;
    Zone(1).ZoneContamControllerSchedIndex = 4;

    AirLoopFlow.allocate(1);
    AirLoopFlow(1).OAFrac = 0.01;    // DataAirLoop variable (AirloopHVAC)
    AirLoopFlow(1).OAMinFrac = 0.01; // DataAirLoop variable (AirloopHVAC)

    GetOAControllerInputs();

    EXPECT_EQ(8, VentilationMechanical(1).SystemOAMethod);
    EXPECT_TRUE(OutAirNodeManager::CheckOutAirNodeNumber(OAController(1).OANode));
    EXPECT_NEAR(0.00314899, VentilationMechanical(1).ZoneOAPeopleRate(1), 0.00001);
    EXPECT_NEAR(0.000407, VentilationMechanical(1).ZoneOAAreaRate(1), 0.00001);

    StdRhoAir = 1.2;
    OAController(1).MixMassFlow = 1.7 * StdRhoAir;
    OAController(1).MaxOAMassFlowRate = 1.7 * StdRhoAir;
    AirLoopFlow(1).DesSupply = 1.7;
    VentilationMechanical(1).SchPtr = 1;
    Schedule(1).CurrentValue = 1.0;

    VentilationMechanical(1).ZoneADEffSchPtr(1) = 2;
    Schedule(2).CurrentValue = 1.0;
    TotPeople = 1;
    People.allocate(1);
    People(1).Name = "WestPeople";
    People(1).ZonePtr = 1;
    People(1).NumberOfPeople = 3;
    Zone(1).TotOccupants = 3;
    Schedule(3).CurrentValue = 0.1;
    Schedule(4).CurrentValue = 1.0;
    ZoneCO2GainFromPeople.allocate(1);
    ZoneCO2GainFromPeople(1) = 3.82E-8;
    OutdoorCO2 = 400;
    ZoneAirCO2.allocate(1);
    ZoneAirCO2(1) = 600.0;
    ZoneEquipConfig.allocate(1);
    ZoneEquipConfig(1).NumInletNodes = 1;
    ZoneEquipConfig(1).AirDistUnitCool.allocate(1);
    ZoneEquipConfig(1).AirDistUnitCool(1).InNode = 10;
    ZoneEquipConfig(1).InletNode.allocate(1);
    ZoneEquipConfig(1).InletNode(1) = 10;
    Node.allocate(10);
    Node(10).Temp = 13.00;
    Node(10).HumRat = 0.008;
    Node(10).MassFlowRate = 1.7 * StdRhoAir;
    OutBaroPress = 101325;
    ZoneSysEnergyDemand.allocate(1);
    ZoneIntGain.allocate(1);
    ZoneIntGain(1).NOFOCC = 0.1;
    Schedule(5).CurrentValue = 900.0;
    Schedule(6).CurrentValue = 300.0;
    Schedule(7).CurrentValue = 900.0;
    Zone(1).ZoneMinCO2SchedIndex = 6;
    Zone(1).ZoneMaxCO2SchedIndex = 7;
    Schedule(8).CurrentValue = 0.01;

    OAController(1).CalcOAController(1, true);

    EXPECT_NEAR(0.003183055786, OAController(1).OAMassFlow, 0.00001);
    EXPECT_NEAR(0.001560321463, OAController(1).MinOAFracLimit, 0.00001);

    AirLoopControlInfo.deallocate();
    OARequirements.deallocate();
    ZoneAirDistribution.deallocate();
    Zone.deallocate();
    AirLoopFlow.deallocate();
    People.deallocate();
    ZoneAirCO2.deallocate();
    ZoneEquipConfig.deallocate();
    Node.deallocate();
    ZoneSysEnergyDemand.deallocate();
    ZoneCO2GainFromPeople.deallocate();
    ContaminantControlledZone.deallocate();
    ZoneIntGain.deallocate();
}

TEST_F(EnergyPlusFixture, MixedAir_OAControllerOrderInControllersListTest)
{
    std::string const idf_objects = delimited_string({
        "  Version,8.8;",

        "  AvailabilityManagerAssignmentList,",
        "    VAV Sys 1 Avail List,    !- Name",
        "    AvailabilityManager:Scheduled,  !- Availability Manager 1 Object Type",
        "    VAV Sys 1 Avail;         !- Availability Manager 1 Name",

        "  AvailabilityManager:Scheduled,",
        "    VAV Sys 1 Avail,         !- Name",
        "    FanAvailSched;           !- Schedule Name",

        "  BranchList,",
        "    VAV Sys 1 Branches,      !- Name",
        "    VAV Sys 1 Main Branch;   !- Branch 1 Name",

        "  Branch,",
        "    VAV Sys 1 Main Branch,   !- Name",
        "    ,                        !- Pressure Drop Curve Name",
        "    AirLoopHVAC:OutdoorAirSystem,  !- Component 1 Object Type",
        "    OA Sys 1,                !- Component 1 Name",
        "    VAV Sys 1 Inlet Node,    !- Component 1 Inlet Node Name",
        "    Mixed Air Node 1,        !- Component 1 Outlet Node Name",
        "    Coil:Cooling:Water,      !- Component 2 Object Type",
        "    Main Cooling Coil 1,     !- Component 2 Name",
        "    Mixed Air Node 1,        !- Component 2 Inlet Node Name",
        "    Main Cooling Coil 1 Outlet Node,  !- Component 2 Outlet Node Name",
        "    Coil:Heating:Water,      !- Component 3 Object Type",
        "    Main Heating Coil 1,     !- Component 3 Name",
        "    Main Cooling Coil 1 Outlet Node,  !- Component 3 Inlet Node Name",
        "    Main Heating Coil 1 Outlet Node,  !- Component 3 Outlet Node Name",
        "    Fan:VariableVolume,      !- Component 4 Object Type",
        "    Supply Fan 1,            !- Component 4 Name",
        "    Main Heating Coil 1 Outlet Node,  !- Component 4 Inlet Node Name",
        "    VAV Sys 1 Outlet Node;   !- Component 4 Outlet Node Name",

        "  AirLoopHVAC:OutdoorAirSystem,",
        "    OA Sys 1,                !- Name",
        "    OA Sys 1 Controllers,    !- Controller List Name",
        "    OA Sys 1 Equipment,      !- Outdoor Air Equipment List Name",
        "    VAV Sys 1 Avail List;    !- Availability Manager List Name",

        "  AirLoopHVAC:ControllerList,",
        "    OA Sys 1 Controllers,    !- Name",
        "    Controller:WaterCoil,    !- Controller 1 Object Type",
        "    OA CC Controller 1,      !- Controller 1 Name",
        "    Controller:OutdoorAir,   !- Controller 2 Object Type",
        "    OA Controller 1,         !- Controller 2 Name",
        "    Controller:WaterCoil,    !- Controller 3 Object Type",
        "    OA HC Controller 1;      !- Controller 3 Name",

        "  AirLoopHVAC:OutdoorAirSystem:EquipmentList,",
        "    OA Sys 1 Equipment,      !- Name",
        "    Coil:Heating:Water,      !- Component 1 Object Type",
        "    OA Heating Coil 1,       !- Component 1 Name",
        "    Coil:Cooling:Water,      !- Component 2 Object Type",
        "    OA Cooling Coil 1,       !- Component 2 Name",
        "    OutdoorAir:Mixer,        !- Component 3 Object Type",
        "    OA Mixing Box 1;         !- Component 3 Name",

        "  Coil:Heating:Water,",
        "    OA Heating Coil 1,       !- Name",
        "    CoolingCoilAvailSched,   !- Availability Schedule Name",
        "    autosize,                !- U-Factor Times Area Value {W/K}",
        "    autosize,                !- Maximum Water Flow Rate {m3/s}",
        "    OA Heating Coil 1 Water Inlet Node,  !- Water Inlet Node Name",
        "    OA Heating Coil 1 Water Outlet Node,  !- Water Outlet Node Name",
        "    Outside Air Inlet Node 1,!- Air Inlet Node Name",
        "    OA Heating Coil 1 Air Outlet Node,  !- Air Outlet Node Name",
        "    UFactorTimesAreaAndDesignWaterFlowRate,  !- Performance Input Method",
        "    autosize,                !- Rated Capacity {W}",
        "    82.2,                    !- Rated Inlet Water Temperature {C}",
        "    16.6,                    !- Rated Inlet Air Temperature {C}",
        "    71.1,                    !- Rated Outlet Water Temperature {C}",
        "    32.2,                    !- Rated Outlet Air Temperature {C}",
        "    ;                        !- Rated Ratio for Air and Water Convection",

        "  Coil:Cooling:Water,",
        "    OA Cooling Coil 1,       !- Name",
        "    CoolingCoilAvailSched,   !- Availability Schedule Name",
        "    autosize,                !- Design Water Flow Rate {m3/s}",
        "    autosize,                !- Design Air Flow Rate {m3/s}",
        "    autosize,                !- Design Inlet Water Temperature {C}",
        "    autosize,                !- Design Inlet Air Temperature {C}",
        "    autosize,                !- Design Outlet Air Temperature {C}",
        "    autosize,                !- Design Inlet Air Humidity Ratio {kgWater/kgDryAir}",
        "    autosize,                !- Design Outlet Air Humidity Ratio {kgWater/kgDryAir}",
        "    OA Cooling Coil 1 Water Inlet Node,  !- Water Inlet Node Name",
        "    OA Cooling Coil 1 Water Outlet Node,  !- Water Outlet Node Name",
        "    OA Heating Coil 1 Air Outlet Node,  !- Air Inlet Node Name",
        "    OA Mixing Box 1 Inlet Node,  !- Air Outlet Node Name",
        "    SimpleAnalysis,          !- Type of Analysis",
        "    CrossFlow;               !- Heat Exchanger Configuration",

        "  OutdoorAir:Mixer,",
        "    OA Mixing Box 1,         !- Name",
        "    Mixed Air Node 1,        !- Mixed Air Node Name",
        "    OA Mixing Box 1 Inlet Node,  !- Outdoor Air Stream Node Name",
        "    Relief Air Outlet Node 1,!- Relief Air Stream Node Name",
        "    VAV Sys 1 Inlet Node;    !- Return Air Stream Node Name",

        "  Coil:Cooling:Water,",
        "    Main Cooling Coil 1,     !- Name",
        "    CoolingCoilAvailSched,   !- Availability Schedule Name",
        "    autosize,                !- Design Water Flow Rate {m3/s}",
        "    autosize,                !- Design Air Flow Rate {m3/s}",
        "    autosize,                !- Design Inlet Water Temperature {C}",
        "    autosize,                !- Design Inlet Air Temperature {C}",
        "    autosize,                !- Design Outlet Air Temperature {C}",
        "    autosize,                !- Design Inlet Air Humidity Ratio {kgWater/kgDryAir}",
        "    autosize,                !- Design Outlet Air Humidity Ratio {kgWater/kgDryAir}",
        "    Main Cooling Coil 1 Water Inlet Node,  !- Water Inlet Node Name",
        "    Main Cooling Coil 1 Water Outlet Node,  !- Water Outlet Node Name",
        "    Mixed Air Node 1,        !- Air Inlet Node Name",
        "    Main Cooling Coil 1 Outlet Node,  !- Air Outlet Node Name",
        "    SimpleAnalysis,          !- Type of Analysis",
        "    CrossFlow;               !- Heat Exchanger Configuration",

        "  Coil:Heating:Water,",
        "    Main Heating Coil 1,     !- Name",
        "    ReheatCoilAvailSched,    !- Availability Schedule Name",
        "    autosize,                !- U-Factor Times Area Value {W/K}",
        "    autosize,                !- Maximum Water Flow Rate {m3/s}",
        "    Main Heating Coil 1 Water Inlet Node,  !- Water Inlet Node Name",
        "    Main Heating Coil 1 Water Outlet Node,  !- Water Outlet Node Name",
        "    Main Cooling Coil 1 Outlet Node,  !- Air Inlet Node Name",
        "    Main Heating Coil 1 Outlet Node,  !- Air Outlet Node Name",
        "    UFactorTimesAreaAndDesignWaterFlowRate,  !- Performance Input Method",
        "    autosize,                !- Rated Capacity {W}",
        "    82.2,                    !- Rated Inlet Water Temperature {C}",
        "    16.6,                    !- Rated Inlet Air Temperature {C}",
        "    71.1,                    !- Rated Outlet Water Temperature {C}",
        "    32.2,                    !- Rated Outlet Air Temperature {C}",
        "    ;                        !- Rated Ratio for Air and Water Convection",

        "  Fan:VariableVolume,",
        "    Supply Fan 1,            !- Name",
        "    FanAvailSched,           !- Availability Schedule Name",
        "    0.7,                     !- Fan Total Efficiency",
        "    600.0,                   !- Pressure Rise {Pa}",
        "    autosize,                !- Maximum Flow Rate {m3/s}",
        "    Fraction,                !- Fan Power Minimum Flow Rate Input Method",
        "    0.25,                    !- Fan Power Minimum Flow Fraction",
        "    ,                        !- Fan Power Minimum Air Flow Rate {m3/s}",
        "    0.9,                     !- Motor Efficiency",
        "    1.0,                     !- Motor In Airstream Fraction",
        "    0.35071223,              !- Fan Power Coefficient 1",
        "    0.30850535,              !- Fan Power Coefficient 2",
        "    -0.54137364,             !- Fan Power Coefficient 3",
        "    0.87198823,              !- Fan Power Coefficient 4",
        "    0.000,                   !- Fan Power Coefficient 5",
        "    Main Heating Coil 1 Outlet Node,  !- Air Inlet Node Name",
        "    VAV Sys 1 Outlet Node;   !- Air Outlet Node Name",

        "  Controller:WaterCoil,",
        "    OA HC Controller 1,      !- Name",
        "    Temperature,             !- Control Variable",
        "    Normal,                  !- Action",
        "    FLOW,                    !- Actuator Variable",
        "    OA Heating Coil 1 Air Outlet Node,  !- Sensor Node Name",
        "    OA Heating Coil 1 Water Inlet Node,  !- Actuator Node Name",
        "    0.002,                   !- Controller Convergence Tolerance {deltaC}",
        "    autosize,                !- Maximum Actuated Flow {m3/s}",
        "    0.0;                     !- Minimum Actuated Flow {m3/s}",

        "  Controller:WaterCoil,",
        "    OA CC Controller 1,      !- Name",
        "    Temperature,             !- Control Variable",
        "    Reverse,                 !- Action",
        "    FLOW,                    !- Actuator Variable",
        "    OA Mixing Box 1 Inlet Node,  !- Sensor Node Name",
        "    OA Cooling Coil 1 Water Inlet Node,  !- Actuator Node Name",
        "    0.002,                   !- Controller Convergence Tolerance {deltaC}",
        "    autosize,                !- Maximum Actuated Flow {m3/s}",
        "    0.0;                     !- Minimum Actuated Flow {m3/s}",

        "  Controller:WaterCoil,",
        "    Central Cooling Coil Controller 1,  !- Name",
        "    Temperature,             !- Control Variable",
        "    Reverse,                 !- Action",
        "    FLOW,                    !- Actuator Variable",
        "    Main Cooling Coil 1 Outlet Node,  !- Sensor Node Name",
        "    Main Cooling Coil 1 Water Inlet Node,  !- Actuator Node Name",
        "    0.002,                   !- Controller Convergence Tolerance {deltaC}",
        "    autosize,                !- Maximum Actuated Flow {m3/s}",
        "    0.0;                     !- Minimum Actuated Flow {m3/s}",

        "  Controller:OutdoorAir,",
        "    OA Controller 1,         !- Name",
        "    Relief Air Outlet Node 1,!- Relief Air Outlet Node Name",
        "    VAV Sys 1 Inlet Node,    !- Return Air Node Name",
        "    Mixed Air Node 1,        !- Mixed Air Node Name",
        "    Outside Air Inlet Node 1,!- Actuator Node Name",
        "    autosize,                !- Minimum Outdoor Air Flow Rate {m3/s}",
        "    autosize,                !- Maximum Outdoor Air Flow Rate {m3/s}",
        "    NoEconomizer,            !- Economizer Control Type",
        "    ModulateFlow,            !- Economizer Control Action Type",
        "    19.,                     !- Economizer Maximum Limit Dry-Bulb Temperature {C}",
        "    ,                        !- Economizer Maximum Limit Enthalpy {J/kg}",
        "    ,                        !- Economizer Maximum Limit Dewpoint Temperature {C}",
        "    ,                        !- Electronic Enthalpy Limit Curve Name",
        "    4.6,                     !- Economizer Minimum Limit Dry-Bulb Temperature {C}",
        "    NoLockout,               !- Lockout Type",
        "    FixedMinimum,            !- Minimum Limit Type",
        "    ;                        !- Minimum Outdoor Air Schedule Name",

        "  Controller:WaterCoil,",
        "    Central Heating Coil Controller 1,  !- Name",
        "    Temperature,             !- Control Variable",
        "    Normal,                  !- Action",
        "    FLOW,                    !- Actuator Variable",
        "    Main Heating Coil 1 Outlet Node,  !- Sensor Node Name",
        "    Main Heating Coil 1 Water Inlet Node,  !- Actuator Node Name",
        "    0.002,                   !- Controller Convergence Tolerance {deltaC}",
        "    autosize,                !- Maximum Actuated Flow {m3/s}",
        "    0.0;                     !- Minimum Actuated Flow {m3/s}",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    GetOAControllerInputs();

    GetOutsideAirSysInputs();

    auto &CurrentOASystem(DataAirLoop::OutsideAirSys[0]);

    EXPECT_EQ(CurrentOASystem.NumControllers, 3);
    EXPECT_EQ(CurrentOASystem.ControllerType(1), "CONTROLLER:WATERCOIL");
    EXPECT_EQ(CurrentOASystem.ControllerName(1), "OA CC CONTROLLER 1");
    EXPECT_EQ(CurrentOASystem.ControllerType(2), "CONTROLLER:OUTDOORAIR");
    EXPECT_EQ(CurrentOASystem.ControllerName(2), "OA CONTROLLER 1");
    EXPECT_EQ(CurrentOASystem.ControllerType(3), "CONTROLLER:WATERCOIL");
    EXPECT_EQ(CurrentOASystem.ControllerName(3), "OA HC CONTROLLER 1");
    EXPECT_EQ(CurrentOASystem.OAControllerName, "OA CONTROLLER 1");
    EXPECT_EQ(CurrentOASystem.OAControllerIndex, 0);
    int AirLoopNum = 0;
    bool FirstHVACIteration = true;
    // sim OAController with OAControllerIndex = 0 for the first time only
    SimOAController(CurrentOASystem.OAControllerName, CurrentOASystem.OAControllerIndex, FirstHVACIteration, AirLoopNum);
    // OAControllerIndex is set during first time InitOAController run
    EXPECT_EQ(CurrentOASystem.OAControllerIndex, 1);
}

TEST_F(EnergyPlusFixture, OAController_ProportionalMinimum_HXBypassTest)
{
    std::string const idf_objects = delimited_string({
        "Version,8.9;",
        "  OutdoorAir:Node,",
        "    Outside Air Inlet Node;  !- Name",

        "  Controller:OutdoorAir,",
        "    OA Controller,           !- Name",
        "    Relief Air Outlet Node,  !- Relief Air Outlet Node Name",
        "    VAV Sys Inlet Node,      !- Return Air Node Name",
        "    Mixed Air Node,          !- Mixed Air Node Name",
        "    Outside Air Inlet Node,  !- Actuator Node Name",
        "    0.2,                     !- Minimum Outdoor Air Flow Rate {m3/s}",
        "    1.0,                     !- Maximum Outdoor Air Flow Rate {m3/s}",
        "    DifferentialDryBulb,     !- Economizer Control Type",
        "    ModulateFlow,            !- Economizer Control Action Type",
        "    ,                        !- Economizer Maximum Limit Dry-Bulb Temperature {C}",
        "    ,                        !- Economizer Maximum Limit Enthalpy {J/kg}",
        "    ,                        !- Economizer Maximum Limit Dewpoint Temperature {C}",
        "    ,                        !- Electronic Enthalpy Limit Curve Name",
        "    ,                        !- Economizer Minimum Limit Dry-Bulb Temperature {C}",
        "    NoLockout,               !- Lockout Type",
        "    ProportionalMinimum,     !- Minimum Limit Type",
        "    ,                        !- Minimum Outdoor Air Schedule Name",
        "    ,                        !- Minimum Fraction of Outdoor Air Schedule Name",
        "    ,                        !- Maximum Fraction of Outdoor Air Schedule Name",
        "    ,                        !- Mechanical Ventilation Controller Name",
        "    ,                        !- Time of Day Economizer Control Schedule Name",
        "    No,                      !- High Humidity Control",
        "    ,                        !- Humidistat Control Zone Name",
        "    ,                        !- High Humidity Outdoor Air Flow Ratio",
        "    Yes,                     !- Control High Indoor Humidity Based on Outdoor Humidity Ratio",
        "    BypassWhenOAFlowGreaterThanMinimum;  !- Heat Recovery Bypass Control Type",

        "  OutdoorAir:Mixer,",
        "    OA Mixer,                !- Name",
        "    Mixed Air Node,          !- Mixed Air Node Name",
        "    Outside Air Inlet Node,  !- Outdoor Air Stream Node Name",
        "    Relief Air Outlet Node,  !- Relief Air Stream Node Name",
        "    VAV Sys Inlet Node;      !- Return Air Stream Node Name",

        " AirLoopHVAC:ControllerList,",
        "    OA Sys Controller,       !- Name",
        "    Controller:OutdoorAir,   !- Controller 1 Object Type",
        "    OA Controller;           !- Controller 1 Name",

        " AirLoopHVAC:OutdoorAirSystem:EquipmentList,",
        "    OA Sys Equipment list,   !- Name",
        "    OutdoorAir:Mixer,        !- Component 1 Object Type",
        "    OA Mixer;                !- Component 1 Name",

        " AirLoopHVAC:OutdoorAirSystem,",
        "    OA Sys,                  !- Name",
        "    OA Sys controller,       !- Controller List Name",
        "    OA Sys Equipment list;   !- Outdoor Air Equipment List Name",

    });

    ASSERT_TRUE(process_idf(idf_objects));
    GetOAControllerInputs();
    EXPECT_EQ(2, OAController(1).OANode);
    EXPECT_TRUE(OutAirNodeManager::CheckOutAirNodeNumber(OAController(1).OANode));

    int OAControllerNum(1);
    int AirLoopNum(1);

    DataHVACGlobals::NumPrimaryAirSys = 1;
    StdBaroPress = StdPressureSeaLevel;
    // assume dry air (zero humidity ratio)
    StdRhoAir = Psychrometrics::PsyRhoAirFnPbTdbW(StdBaroPress, 20.0, 0.0);

    AirLoopFlow.allocate(1);
    PrimaryAirSystem.allocate(1);
    AirLoopControlInfo.allocate(1);

    auto &curAirLoopFlow(AirLoopFlow(AirLoopNum));
    auto &curOACntrl(OAController(OAControllerNum));
    auto &AirLoopCntrlInfo(AirLoopControlInfo(AirLoopNum));
    auto &PrimaryAirSys(PrimaryAirSystem(AirLoopNum));

    PrimaryAirSys.NumBranches = 1;
    PrimaryAirSys.Branch.allocate(1);
    PrimaryAirSys.Branch(1).TotalComponents = 1;
    PrimaryAirSys.Branch(1).Comp.allocate(1);
    PrimaryAirSys.Branch(1).Comp(1).Name = "OA Sys";
    PrimaryAirSys.Branch(1).Comp(1).TypeOf = "AirLoopHVAC:OutdoorAirSystem";

    Real64 DesignSupplyAirMassFlow = 1.0 * StdRhoAir;
    Real64 MixedAirMassFlow = 0.50 * DesignSupplyAirMassFlow;

    curAirLoopFlow.DesSupply = DesignSupplyAirMassFlow;

    // Initialize common AirLoop data
    AirLoopCntrlInfo.OASysNum = AirLoopNum;
    AirLoopCntrlInfo.EconoLockout = false;
    AirLoopCntrlInfo.NightVent = false;
    AirLoopCntrlInfo.FanOpMode = DataHVACGlobals::ContFanCycCoil;
    AirLoopCntrlInfo.LoopFlowRateSet = false;
    AirLoopCntrlInfo.CheckHeatRecoveryBypassStatus = true;
    AirLoopCntrlInfo.OASysComponentsSimulated = true;
    AirLoopCntrlInfo.EconomizerFlowLocked = false;
    AirLoopCntrlInfo.HeatRecoveryBypass = false;
    AirLoopCntrlInfo.HeatRecoveryResimFlag = false;
    AirLoopCntrlInfo.HeatingActiveFlag = true;

    // Initialize OA controller and node data
    curOACntrl.MinOAMassFlowRate = curOACntrl.MinOA * StdRhoAir;
    curOACntrl.MaxOAMassFlowRate = curOACntrl.MaxOA * StdRhoAir;
    curOACntrl.InletNode = curOACntrl.OANode;
    curOACntrl.RetTemp = 24.0;
    curOACntrl.OATemp = 20.0;
    curOACntrl.InletTemp = curOACntrl.OATemp;
    curOACntrl.MixSetTemp = 22.0;
    curOACntrl.ExhMassFlow = 0.0;
    curOACntrl.MixMassFlow = MixedAirMassFlow;

    // Initialize air node data
    Node(curOACntrl.MixNode).MassFlowRate = curOACntrl.MixMassFlow;
    Node(curOACntrl.MixNode).MassFlowRateMaxAvail = curOACntrl.MixMassFlow;
    Node(curOACntrl.RetNode).Temp = curOACntrl.RetTemp;
    Node(curOACntrl.RetNode).Enthalpy = Psychrometrics::PsyHFnTdbW(curOACntrl.RetTemp, 0.0);
    Node(curOACntrl.MixNode).TempSetPoint = curOACntrl.MixSetTemp;
    Node(curOACntrl.OANode).Temp = curOACntrl.OATemp;
    Node(curOACntrl.OANode).Enthalpy = Psychrometrics::PsyHFnTdbW(curOACntrl.InletTemp, 0.0);

    Real64 OAMassFlowActual(0.0);
    Real64 OAMassFlowAMin(0.0);
    Real64 OutAirMassFlowFracMin(0.0);
    Real64 OutAirMassFlowFracActual(0.0);

    // check OA controller inputs
    EXPECT_EQ(curOACntrl.Lockout, NoLockoutPossible); // NoLockout (ecoomizer always active)
    EXPECT_EQ(curOACntrl.HeatRecoveryBypassControlType, DataHVACGlobals::BypassWhenOAFlowGreaterThanMinimum);
    EXPECT_FALSE(curOACntrl.FixedMin); // Economizer Minimum Limit Type = ProportionalMinimum
    EXPECT_EQ(curOACntrl.MinOA, 0.2);  // OA min vol flow rate

    // calc minimum OA mass flow fraction
    OutAirMassFlowFracMin = curOACntrl.MinOA * StdRhoAir / DesignSupplyAirMassFlow;
    // calc minimum OA mass flow for ProportionalMinimum
    OAMassFlowAMin = OutAirMassFlowFracMin * MixedAirMassFlow;
    // calc actual OA mass flow fraction
    OutAirMassFlowFracActual = (curOACntrl.MixSetTemp - curOACntrl.RetTemp) / (curOACntrl.InletTemp - curOACntrl.RetTemp);
    // calc actual OA mass flow
    OAMassFlowActual = OutAirMassFlowFracActual * MixedAirMassFlow;

    // run OA controller and OA economizer
    curOACntrl.CalcOAController(AirLoopNum, true);

    // check min OA flow and fraction
    EXPECT_EQ(OAMassFlowAMin, curAirLoopFlow.MinOutAir);
    EXPECT_EQ(OutAirMassFlowFracMin, curAirLoopFlow.OAMinFrac);
    // check actual OA flow and fraction
    EXPECT_NEAR(OAMassFlowActual, curOACntrl.OAMassFlow, 0.00000001);
    EXPECT_NEAR(OutAirMassFlowFracActual, curAirLoopFlow.OAFrac, 0.00000001);
    // check HX bypass status
    EXPECT_GT(OAMassFlowActual, OAMassFlowAMin);
    EXPECT_EQ(1, curOACntrl.HeatRecoveryBypassStatus);
    EXPECT_TRUE(AirLoopControlInfo(AirLoopNum).HeatRecoveryBypass);
}

TEST_F(EnergyPlusFixture, OAController_FixedMinimum_MinimumLimitTypeTest)
{
    std::string const idf_objects = delimited_string({
        "Version,8.9;",
        "  OutdoorAir:Node,",
        "    Outside Air Inlet Node;  !- Name",

        "  Controller:OutdoorAir,",
        "    OA Controller,           !- Name",
        "    Relief Air Outlet Node,  !- Relief Air Outlet Node Name",
        "    VAV Sys Inlet Node,      !- Return Air Node Name",
        "    Mixed Air Node,          !- Mixed Air Node Name",
        "    Outside Air Inlet Node,  !- Actuator Node Name",
        "    0.2,                     !- Minimum Outdoor Air Flow Rate {m3/s}",
        "    1.0,                     !- Maximum Outdoor Air Flow Rate {m3/s}",
        "    DifferentialDryBulb,     !- Economizer Control Type",
        "    ModulateFlow,            !- Economizer Control Action Type",
        "    ,                        !- Economizer Maximum Limit Dry-Bulb Temperature {C}",
        "    ,                        !- Economizer Maximum Limit Enthalpy {J/kg}",
        "    ,                        !- Economizer Maximum Limit Dewpoint Temperature {C}",
        "    ,                        !- Electronic Enthalpy Limit Curve Name",
        "    ,                        !- Economizer Minimum Limit Dry-Bulb Temperature {C}",
        "    NoLockout,               !- Lockout Type",
        "    FixedMinimum,            !- Minimum Limit Type",
        "    ,                        !- Minimum Outdoor Air Schedule Name",
        "    ,                        !- Minimum Fraction of Outdoor Air Schedule Name",
        "    ,                        !- Maximum Fraction of Outdoor Air Schedule Name",
        "    ,                        !- Mechanical Ventilation Controller Name",
        "    ,                        !- Time of Day Economizer Control Schedule Name",
        "    No,                      !- High Humidity Control",
        "    ,                        !- Humidistat Control Zone Name",
        "    ,                        !- High Humidity Outdoor Air Flow Ratio",
        "    Yes,                     !- Control High Indoor Humidity Based on Outdoor Humidity Ratio",
        "    BypassWhenOAFlowGreaterThanMinimum;  !- Heat Recovery Bypass Control Type",

        "  HeatExchanger:AirToAir:SensibleAndLatent,",
        "    OA Heat Recovery,        !- Name",
        "    ,                        !- Availability Schedule Name",
        "    AUTOSIZE,                !- Nominal Supply Air Flow Rate {m3/s}",
        "    0.70,                    !- Sensible Effectiveness at 100% Heating Air Flow {dimensionless}",
        "    0.60,                    !- Latent Effectiveness at 100% Heating Air Flow {dimensionless}",
        "    0.70,                    !- Sensible Effectiveness at 75% Heating Air Flow {dimensionless}",
        "    0.60,                    !- Latent Effectiveness at 75% Heating Air Flow {dimensionless}",
        "    0.75,                    !- Sensible Effectiveness at 100% Cooling Air Flow {dimensionless}",
        "    0.60,                    !- Latent Effectiveness at 100% Cooling Air Flow {dimensionless}",
        "    0.75,                    !- Sensible Effectiveness at 75% Cooling Air Flow {dimensionless}",
        "    0.60,                    !- Latent Effectiveness at 75% Cooling Air Flow {dimensionless}",
        "    Outside Air Inlet Node,  !- Supply Air Inlet Node Name",
        "    OA HR Outlet Node,       !- Supply Air Outlet Node Name",
        "    Relief Air Outlet Node,  !- Exhaust Air Inlet Node Name",
        "    HR Exhaust Air Outlet Node,  !- Exhaust Air Outlet Node Name",
        "    1500.0,                  !- Nominal Electric Power {W}",
        "    Yes,                     !- Supply Air Outlet Temperature Control",
        "    Rotary,                  !- Heat Exchanger Type",
        "    ExhaustOnly,             !- Frost Control Type",
        "    -23.3,                   !- Threshold Temperature {C}",
        "    0.167,                   !- Initial Defrost Time Fraction {dimensionless}",
        "    1.44;                    !- Rate of Defrost Time Fraction Increase {1/K}",

        "  OutdoorAir:Mixer,",
        "    OA Mixer,                !- Name",
        "    Mixed Air Node,          !- Mixed Air Node Name",
        "    OA HR Outlet Node,       !- Outdoor Air Stream Node Name",
        "    Relief Air Outlet Node,  !- Relief Air Stream Node Name",
        "    VAV Sys Inlet Node;      !- Return Air Stream Node Name",

        " AirLoopHVAC:ControllerList,",
        "    OA Sys Controller,       !- Name",
        "    Controller:OutdoorAir,   !- Controller 1 Object Type",
        "    OA Controller;           !- Controller 1 Name",

        " AirLoopHVAC:OutdoorAirSystem:EquipmentList,",
        "    OA Sys Equipment list,   !- Name",
        "    HeatExchanger:AirToAir:SensibleAndLatent,  !- Component 1 Object Type",
        "    OA Heat Recovery,        !- Component 1 Name",
        "    OutdoorAir:Mixer,        !- Component 1 Object Type",
        "    OA Mixer;                !- Component 1 Name",

        " AirLoopHVAC:OutdoorAirSystem,",
        "    OA Sys,                  !- Name",
        "    OA Sys controller,       !- Controller List Name",
        "    OA Sys Equipment list;   !- Outdoor Air Equipment List Name",

    });

    ASSERT_TRUE(process_idf(idf_objects));
    GetOutsideAirSysInputs();
    EXPECT_EQ(1, NumOASystems);
    EXPECT_EQ("OA SYS", OutsideAirSys(1).Name);

    EXPECT_EQ(2, OutsideAirSys(1).NumComponents);
    EXPECT_EQ("OA HEAT RECOVERY", OutsideAirSys(1).ComponentName(1));
    EXPECT_EQ("OA MIXER", OutsideAirSys(1).ComponentName(2));

    GetOAControllerInputs();
    EXPECT_EQ(5, OAController(1).OANode);
    EXPECT_TRUE(OutAirNodeManager::CheckOutAirNodeNumber(OAController(1).OANode));

    int OAControllerNum(1);
    int AirLoopNum(1);

    DataHVACGlobals::NumPrimaryAirSys = 1;
    StdBaroPress = StdPressureSeaLevel;
    // assume dry air (zero humidity ratio)
    StdRhoAir = Psychrometrics::PsyRhoAirFnPbTdbW(StdBaroPress, 20.0, 0.0);

    AirLoopFlow.allocate(1);
    PrimaryAirSystem.allocate(1);
    AirLoopControlInfo.allocate(1);

    auto &curAirLoopFlow(AirLoopFlow(AirLoopNum));
    auto &curOACntrl(OAController(OAControllerNum));
    auto &AirLoopCntrlInfo(AirLoopControlInfo(AirLoopNum));
    auto &PrimaryAirSys(PrimaryAirSystem(AirLoopNum));

    PrimaryAirSys.NumBranches = 1;
    PrimaryAirSys.Branch.allocate(1);
    PrimaryAirSys.Branch(1).TotalComponents = 1;
    PrimaryAirSys.Branch(1).Comp.allocate(1);
    PrimaryAirSys.Branch(1).Comp(1).Name = "OA Sys";
    PrimaryAirSys.Branch(1).Comp(1).TypeOf = "AirLoopHVAC:OutdoorAirSystem";

    Real64 DesignSupplyAirMassFlow = 1.0 * StdRhoAir;
    Real64 MixedAirMassFlow = 0.50 * DesignSupplyAirMassFlow;

    curAirLoopFlow.DesSupply = DesignSupplyAirMassFlow;

    // Initialize common AirLoop data
    AirLoopCntrlInfo.OASysNum = AirLoopNum;
    AirLoopCntrlInfo.EconoLockout = false;
    AirLoopCntrlInfo.NightVent = false;
    AirLoopCntrlInfo.FanOpMode = DataHVACGlobals::ContFanCycCoil;
    AirLoopCntrlInfo.LoopFlowRateSet = false;
    AirLoopCntrlInfo.CheckHeatRecoveryBypassStatus = true;
    AirLoopCntrlInfo.OASysComponentsSimulated = true;
    AirLoopCntrlInfo.EconomizerFlowLocked = false;
    AirLoopCntrlInfo.HeatRecoveryBypass = false;
    AirLoopCntrlInfo.HeatRecoveryResimFlag = false;
    AirLoopCntrlInfo.HeatingActiveFlag = true;

    // Initialize OA controller and node data
    curOACntrl.MinOAMassFlowRate = curOACntrl.MinOA * StdRhoAir;
    curOACntrl.MaxOAMassFlowRate = curOACntrl.MaxOA * StdRhoAir;
    curOACntrl.InletNode = curOACntrl.OANode;
    curOACntrl.RetTemp = 24.0;
    curOACntrl.OATemp = 20.0;
    curOACntrl.InletTemp = curOACntrl.OATemp;
    curOACntrl.MixSetTemp = 22.0;
    curOACntrl.ExhMassFlow = 0.0;
    curOACntrl.MixMassFlow = MixedAirMassFlow;

    // Initialize air node data
    Node(curOACntrl.MixNode).MassFlowRate = curOACntrl.MixMassFlow;
    Node(curOACntrl.MixNode).MassFlowRateMaxAvail = curOACntrl.MixMassFlow;
    Node(curOACntrl.RetNode).Temp = curOACntrl.RetTemp;
    Node(curOACntrl.RetNode).Enthalpy = Psychrometrics::PsyHFnTdbW(curOACntrl.RetTemp, 0.0);
    Node(curOACntrl.MixNode).TempSetPoint = curOACntrl.MixSetTemp;
    Node(curOACntrl.OANode).Temp = curOACntrl.OATemp;
    Node(curOACntrl.OANode).Enthalpy = Psychrometrics::PsyHFnTdbW(curOACntrl.InletTemp, 0.0);

    Real64 OAMassFlowActual(0.0);
    Real64 OAMassFlowAMin(0.0);
    Real64 OutAirMassFlowFracMin(0.0);
    Real64 OutAirMassFlowFracActual(0.0);

    // check OA controller inputs
    EXPECT_EQ(curOACntrl.MinOA, 0.2);                 // user specified minimum OA vol flow rate
    EXPECT_TRUE(curOACntrl.FixedMin);                 // Economizer Minimum Limit Type = FixedMinimum
    EXPECT_EQ(curOACntrl.Lockout, NoLockoutPossible); // NoLockout (ecoomizer always active)
    EXPECT_EQ(curOACntrl.HeatRecoveryBypassControlType, DataHVACGlobals::BypassWhenOAFlowGreaterThanMinimum);

    // calc minimum OA mass flow for FixedMinimum
    OAMassFlowAMin = curOACntrl.MinOA * StdRhoAir;
    // calc minimum OA mass flow fraction
    OutAirMassFlowFracMin = OAMassFlowAMin / DesignSupplyAirMassFlow;

    // calc actual OA mass flow fraction
    OutAirMassFlowFracActual = (curOACntrl.MixSetTemp - curOACntrl.RetTemp) / (curOACntrl.InletTemp - curOACntrl.RetTemp);
    // calc actual OA mass flow
    OAMassFlowActual = OutAirMassFlowFracActual * MixedAirMassFlow;

    // run OA controller and OA economizer
    curOACntrl.CalcOAController(AirLoopNum, true);

    // check min OA flow and fraction
    EXPECT_EQ(OAMassFlowAMin, curAirLoopFlow.MinOutAir);
    EXPECT_EQ(OutAirMassFlowFracMin, curAirLoopFlow.OAMinFrac);
    // check actual OA flow and fraction
    EXPECT_NEAR(OAMassFlowActual, curOACntrl.OAMassFlow, 0.00000001);
    EXPECT_NEAR(OutAirMassFlowFracActual, curAirLoopFlow.OAFrac, 0.00000001);
    // check HX bypass status
    EXPECT_GT(OAMassFlowActual, OAMassFlowAMin);
    EXPECT_EQ(1, curOACntrl.HeatRecoveryBypassStatus);
    EXPECT_TRUE(AirLoopControlInfo(AirLoopNum).HeatRecoveryBypass);
}

} // namespace EnergyPlus
