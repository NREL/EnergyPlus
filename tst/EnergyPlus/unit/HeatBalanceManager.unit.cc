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

// EnergyPlus::HeatBalanceManager Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <DataAirLoop.hh>
#include <DataAirSystems.hh>
#include <DataDaylighting.hh>
#include <DataEnvironment.hh>
#include <DataHVACGlobals.hh>
#include <DataHeatBalFanSys.hh>
#include <DataLoopNode.hh>
#include <DataSurfaces.hh>
#include <DataZoneEquipment.hh>
#include <ElectricPowerServiceManager.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <HeatBalanceAirManager.hh>
#include <OutAirNodeManager.hh>
#include <OutputProcessor.hh>
#include <ScheduleManager.hh>
#include <SimulationManager.hh>
#include <UtilityRoutines.hh>
#include <WeatherManager.hh>
#include <ZoneEquipmentManager.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus::HeatBalanceManager;
using namespace EnergyPlus::DataHeatBalance;
using namespace EnergyPlus::DataIPShortCuts;
using namespace EnergyPlus::DataGlobals;
using namespace EnergyPlus::ZoneEquipmentManager;
using namespace EnergyPlus::HeatBalanceAirManager;
using namespace EnergyPlus::ScheduleManager;
using namespace EnergyPlus::DataHeatBalFanSys;
using namespace EnergyPlus::DataZoneEquipment;
using namespace EnergyPlus::DataLoopNode;
using namespace EnergyPlus::DataAirLoop;
using namespace EnergyPlus::DataAirSystems;
using namespace EnergyPlus::DataHVACGlobals;

namespace EnergyPlus {

TEST_F(EnergyPlusFixture, HeatBalanceManager_ZoneAirBalance_OutdoorAir)
{
    std::string const idf_objects = delimited_string({
        "ZoneAirBalance:OutdoorAir,\n",
        "    LIVING ZONE Balance 1,   !- Name\n",
        "    LIVING ZONE,             !- Zone Name\n",
        "    Quadrature,              !- Air Balance Method\n",
        "    0.01,                    !- Induced Outdoor Air Due to Unbalanced Duct Leakage {m3/s}\n",
        "    INF-SCHED;               !- Induced Outdoor Air Schedule Name",
        "ZoneAirBalance:OutdoorAir,\n",
        "    LIVING ZONE Balance 2,   !- Name\n",
        "    LIVING ZONE,             !- Zone Name\n",
        "    Quadrature,              !- Air Balance Method\n",
        "    0.01,                    !- Induced Outdoor Air Due to Unbalanced Duct Leakage {m3/s}\n",
        "    INF-SCHED2;              !- Induced Outdoor Air Schedule Name",
        "Zone,",
        "LIVING ZONE,             !- Name",
        "0,                       !- Direction of Relative North {deg}",
        "0,                       !- X Origin {m}",
        "0,                       !- Y Origin {m}",
        "0,                       !- Z Origin {m}",
        "1,                       !- Type",
        "1,                       !- Multiplier",
        "autocalculate,           !- Ceiling Height {m}",
        "autocalculate;           !- Volume {m3}",

    });
    ASSERT_TRUE(process_idf(idf_objects));
    bool ErrorsFound = false;
    auto numZones = inputProcessor->getNumObjectsFound("Zone");
    ZoneReOrder.allocate(numZones);
    GetZoneData(ErrorsFound);
    GetAirFlowFlag(ErrorsFound);
    EXPECT_TRUE(ErrorsFound);
}

TEST_F(EnergyPlusFixture, HeatBalanceManager_WindowMaterial_Gap_Duplicate_Names)
{
    std::string const idf_objects = delimited_string({
        "Version,8.6;",
        "  WindowMaterial:Gap,",
        "    Gap_1_Layer,             !- Name",
        "    0.0127,                  !- Thickness {m}",
        "    Gas_1_W_0_0127,          !- Gas (or Gas Mixture)",
        "    101325.0000;             !- Pressure {Pa}",
        "  WindowGap:DeflectionState,",
        "    DeflectionState_813_Measured_Gap_1,  !- Name",
        "    0.0120;                  !- Deflected Thickness {m}",
        "  WindowMaterial:Gap,",
        "    Gap_6_Layer,             !- Name",
        "    0.0060,                  !- Thickness {m}",
        "    Gap_6_W_0_0060,          !- Gas (or Gas Mixture)",
        "    101300.0000,             !- Pressure {Pa}",
        "    DeflectionState_813_Measured_Gap_1;  !- Deflection State",
        "  WindowMaterial:Gap,",
        "    Gap_1_Layer,             !- Name",
        "    0.0100,                  !- Thickness {m}",
        "    Gas_1_W_0_0100,          !- Gas (or Gas Mixture)",
        "    101325.0000;             !- Pressure {Pa}",
    });

    ASSERT_FALSE(process_idf(idf_objects, false)); // expect errors
    std::string const error_string = delimited_string({
        "   ** Severe  ** Duplicate name found. name: \"Gap_1_Layer\". Overwriting existing object.",
    });
    EXPECT_TRUE(compare_err_stream(error_string, true));

    bool ErrorsFound(false);

    GetMaterialData(ErrorsFound);

    EXPECT_FALSE(ErrorsFound);
}

TEST_F(EnergyPlusFixture, HeatBalanceManager_WindowMaterial_Gap_Duplicate_Names_2)
{
    std::string const idf_objects = delimited_string({
        "Version,8.6;",
        "  WindowGap:DeflectionState,",
        "    DeflectionState_813_Measured_Gap_1,  !- Name",
        "    0.0120;                  !- Deflected Thickness {m}",
        "  WindowMaterial:Gap,",
        "    Gap_6_Layer,             !- Name",
        "    0.0060,                  !- Thickness {m}",
        "    Gap_6_W_0_0060,          !- Gas (or Gas Mixture)",
        "    101300.0000,             !- Pressure {Pa}",
        "    DeflectionState_813_Measured_Gap_1;  !- Deflection State",
        "  WindowMaterial:Gap,",
        "    Gap_1_Layer,             !- Name",
        "    0.0127,                  !- Thickness {m}",
        "    Gas_1_W_0_0127,          !- Gas (or Gas Mixture)",
        "    101325.0000;             !- Pressure {Pa}",
        "  WindowMaterial:Gap,",
        "    Gap_1_Layer,             !- Name",
        "    0.0100,                  !- Thickness {m}",
        "    Gas_1_W_0_0100,          !- Gas (or Gas Mixture)",
        "    101325.0000;             !- Pressure {Pa}",
    });

    ASSERT_FALSE(process_idf(idf_objects, false)); // expect errors
    std::string const error_string = delimited_string({
        "   ** Severe  ** Duplicate name found. name: \"Gap_1_Layer\". Overwriting existing object.",
    });
    EXPECT_TRUE(compare_err_stream(error_string, true));

    bool ErrorsFound(false);

    GetMaterialData(ErrorsFound);

    EXPECT_FALSE(ErrorsFound);
}

TEST_F(EnergyPlusFixture, HeatBalanceManager_ProcessZoneData)
{
    // Test input processing of Zone object
    //	Zone,
    //		ZONE ONE, !- Name
    //		0, !- Direction of Relative North{ deg }
    //		0, 0, 0, !- X, Y, Z{ m }
    //		1, !- Type
    //		1, !- Multiplier
    //		autocalculate, !- Ceiling Height{ m }
    //		autocalculate, !- Volume{ m3 }
    //		, !- Floor Area{ m2 }
    //		AdaptiveConvectionAlgorithm;  !- Zone Inside Convection Algorithm

    bool ErrorsFound(false); // If errors detected in input
    int ZoneNum(0);          // Zone number
    int NumAlphas(2);
    int NumNumbers(9);

    cCurrentModuleObject = "Zone";
    NumOfZones = 2;
    Zone.allocate(NumOfZones);

    // Set up a Zone object
    NumAlphas = 2;
    NumNumbers = 9;
    lNumericFieldBlanks.allocate(NumNumbers);
    lAlphaFieldBlanks.allocate(NumAlphas);
    cAlphaFieldNames.allocate(NumAlphas);
    cNumericFieldNames.allocate(NumNumbers);
    cAlphaArgs.allocate(NumAlphas);
    rNumericArgs.allocate(NumNumbers);
    lNumericFieldBlanks = false;
    lAlphaFieldBlanks = false;
    cAlphaFieldNames = " ";
    cNumericFieldNames = " ";
    cAlphaArgs = " ";
    rNumericArgs = 0.0;

    ZoneNum = 1;
    cAlphaArgs(1) = "Zone One";                    // Name
    rNumericArgs(1) = 0.0;                         // Direction of Relative North[deg]
    rNumericArgs(2) = 0.0;                         // X [m]
    rNumericArgs(3) = 0.0;                         // Y [m]
    rNumericArgs(4) = 0.0;                         // Z [m]
    rNumericArgs(5) = 0.0;                         // Type
    rNumericArgs(6) = 0.0;                         // Multiplier
    lNumericFieldBlanks(7) = true;                 // Ceiling Height{ m }
    lNumericFieldBlanks(8) = true;                 // Volume{ m3 }
    lNumericFieldBlanks(9) = true;                 // Floor Area{ m2 }
    cAlphaArgs(2) = "ADAPTIVECONVECTIONALGORITHM"; // Zone Inside Convection Algorithm - Must be UPPERCASE by this point

    ErrorsFound = false;
    ProcessZoneData(cCurrentModuleObject,
                    ZoneNum,
                    cAlphaArgs,
                    NumAlphas,
                    rNumericArgs,
                    NumNumbers,
                    lNumericFieldBlanks,
                    lAlphaFieldBlanks,
                    cAlphaFieldNames,
                    cNumericFieldNames,
                    ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    ZoneNum = 2;
    cAlphaArgs(1) = "Zone Two";      // Name
    cAlphaArgs(2) = "InvalidChoice"; // Zone Inside Convection Algorithm - Must be UPPERCASE by this point
    ErrorsFound = false;
    ProcessZoneData(cCurrentModuleObject,
                    ZoneNum,
                    cAlphaArgs,
                    NumAlphas,
                    rNumericArgs,
                    NumNumbers,
                    lNumericFieldBlanks,
                    lAlphaFieldBlanks,
                    cAlphaFieldNames,
                    cNumericFieldNames,
                    ErrorsFound);
    EXPECT_TRUE(ErrorsFound);

    ZoneNum = 2;
    cAlphaArgs(1) = "Zone Two"; // Name
    cAlphaArgs(2) = "TARP";     // Zone Inside Convection Algorithm - Must be UPPERCASE by this point
    ErrorsFound = false;
    ProcessZoneData(cCurrentModuleObject,
                    ZoneNum,
                    cAlphaArgs,
                    NumAlphas,
                    rNumericArgs,
                    NumNumbers,
                    lNumericFieldBlanks,
                    lAlphaFieldBlanks,
                    cAlphaFieldNames,
                    cNumericFieldNames,
                    ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    EXPECT_EQ("Zone One", Zone(1).Name);
    EXPECT_EQ(AdaptiveConvectionAlgorithm, Zone(1).InsideConvectionAlgo);
    EXPECT_EQ("Zone Two", Zone(2).Name);
    EXPECT_EQ(ASHRAETARP, Zone(2).InsideConvectionAlgo);
}

TEST_F(EnergyPlusFixture, HeatBalanceManager_GetWindowConstructData)
{
    // Test get input for window construction object
    // Construction,
    //	 WINDOWWBLIND, !- Name
    //	 GLASS,        !- Outside Layer
    //	 AIRGAP,       !- Layer 2
    //	 GLASS;        !- Layer 3

    std::string const idf_objects = delimited_string({
        "Version,8.3;",
        "Construction,",
        " WINDOWWBLIND, !- Name",
        " GLASS,        !- Outside Layer",
        " AIRGAP,       !- Layer 2",
        " GLASS;        !- Layer 3",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    bool ErrorsFound(false); // If errors detected in input

    TotMaterials = 3;
    Material.allocate(TotMaterials);
    Material(1).Name = "GLASS";
    Material(2).Name = "AIRGAP";
    Material(3).Name = "GLASS";

    // Material layer group index
    Material(1).Group = 3; // WindowGlass
    Material(2).Group = 4; // WindowGas
    Material(3).Group = 3; // WindowGlass

    NominalRforNominalUCalculation.allocate(1);
    NominalRforNominalUCalculation(1) = 0.0;
    NominalR.allocate(TotMaterials);
    NominalR(1) = 0.4; // Set these explicity for each material layer to avoid random failures of check for NominalRforNominalUCalculation == 0.0 at
                       // end of GetConstructData
    NominalR(2) = 0.4;
    NominalR(3) = 0.4;

    // call to get valid window material types
    ErrorsFound = false;
    GetConstructData(ErrorsFound); // returns ErrorsFound as false since all layers are valid
    EXPECT_FALSE(ErrorsFound);

    // Clear shared arrays that were allocated in GetConstructData
    Construct.deallocate();

    // call to get invalid window material type
    //		Material( 2 ).Group = 16; // BlindEquivalentLayer, this layer is invalid in plain windows
    //		ErrorsFound = false;
    //		GetConstructData( ErrorsFound ); // returns ErrorsFound as true since layer 2 is invalid
    //		EXPECT_TRUE( ErrorsFound );
}

TEST_F(EnergyPlusFixture, HeatBalanceManager_ZoneAirMassFlowConservationData1)
{
    // Test get input for ZoneAirMassFlowConservation object

    std::string const idf_objects = delimited_string({
        "Version,8.3;",
        "Building,",
        "My Building, !- Name",
        "30., !- North Axis{ deg }",
        "City, !- Terrain",
        "0.04, !- Loads Convergence Tolerance Value",
        "0.4, !- Temperature Convergence Tolerance Value{ deltaC }",
        "FullExterior, !- Solar Distribution",
        "25, !- Maximum Number of Warmup Days",
        "6;                       !- Minimum Number of Warmup Days",
        "ZoneAirMassFlowConservation,",
        "Yes, !- Adjust Zone Mixing For Zone Air Mass Flow Balance",
        "AddInfiltrationFlow, !- Infiltration Balancing Method",
        "MixingSourceZonesOnly; !- Infiltration Balancing Zones",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    bool ErrorsFound(false); // If errors detected in input

    // call to process input
    ErrorsFound = false;
    GetProjectControlData(ErrorsFound); // returns ErrorsFound false, ZoneAirMassFlowConservation never sets it
    EXPECT_FALSE(ErrorsFound);
    EXPECT_TRUE(ZoneAirMassFlow.EnforceZoneMassBalance);
    EXPECT_TRUE(ZoneAirMassFlow.BalanceMixing);
    EXPECT_EQ(ZoneAirMassFlow.InfiltrationTreatment, AddInfiltrationFlow);
    EXPECT_EQ(ZoneAirMassFlow.InfiltrationZoneType, MixingSourceZonesOnly);
}

TEST_F(EnergyPlusFixture, HeatBalanceManager_ZoneAirMassFlowConservationData2)
{
    // Test get input for ZoneAirMassFlowConservation object

    std::string const idf_objects = delimited_string({"Version,8.3;",
                                                      "Building,",
                                                      "My Building, !- Name",
                                                      "30., !- North Axis{ deg }",
                                                      "City, !- Terrain",
                                                      "0.04, !- Loads Convergence Tolerance Value",
                                                      "0.4, !- Temperature Convergence Tolerance Value{ deltaC }",
                                                      "FullExterior, !- Solar Distribution",
                                                      "25, !- Maximum Number of Warmup Days",
                                                      "6;                       !- Minimum Number of Warmup Days",
                                                      "ZoneAirMassFlowConservation,",
                                                      "No, !- Adjust Zone Mixing For Zone Air Mass Flow Balance",
                                                      "AdjustInfiltrationFlow, !- Infiltration Balancing Method",
                                                      "AllZones;                !- Infiltration Balancing Zones",
                                                      "Zone, Zone 1;",
                                                      "Zone, Zone 2;",
                                                      "ZoneMixing,",
                                                      "Zone 2 Zone Mixing, !- Name",
                                                      "Zone 2, !- Zone Name",
                                                      "Always1, !- Schedule Name",
                                                      "Flow/Zone, !- Design Flow Rate Calculation Method",
                                                      "0.07, !- Design Flow Rate{ m3 / s }",
                                                      ", !- Flow Rate per Zone Floor Area{ m3 / s - m2 }",
                                                      ", !- Flow Rate per Person{ m3 / s - person }",
                                                      ", !- Air Changes per Hour{ 1 / hr }",
                                                      "Zone 1, !- Source Zone Name",
                                                      "0.0;                     !- Delta Temperature{ deltaC }",
                                                      "ZoneInfiltration:DesignFlowRate,",
                                                      "Zone 1 Infil 1, !- Name",
                                                      "Zone 1, !- Zone or ZoneList Name",
                                                      "Always1, !- Schedule Name",
                                                      "flow/zone, !- Design Flow Rate Calculation Method",
                                                      "0.032, !- Design Flow Rate{ m3 / s }",
                                                      ", !- Flow per Zone Floor Area{ m3 / s - m2 }",
                                                      ", !- Flow per Exterior Surface Area{ m3 / s - m2 }",
                                                      ", !- Air Changes per Hour{ 1 / hr }",
                                                      "1, !- Constant Term Coefficient",
                                                      "0, !- Temperature Term Coefficient",
                                                      "0, !- Velocity Term Coefficient",
                                                      "0; !- Velocity Squared Term Coefficient",
                                                      "Schedule:Constant,Always1,,1.0;"

    });

    ASSERT_TRUE(process_idf(idf_objects));

    bool ErrorsFound(false); // If errors detected in input

    // call to process input
    ProcessScheduleInput();
    ErrorsFound = false;
    GetProjectControlData(ErrorsFound); // returns ErrorsFound false, ZoneAirMassFlowConservation never sets it
    EXPECT_FALSE(ErrorsFound);
    EXPECT_TRUE(ZoneAirMassFlow.EnforceZoneMassBalance);
    EXPECT_FALSE(ZoneAirMassFlow.BalanceMixing);
    EXPECT_EQ(ZoneAirMassFlow.InfiltrationTreatment, AdjustInfiltrationFlow);
    EXPECT_EQ(ZoneAirMassFlow.InfiltrationZoneType, AllZones);

    // setup mixing and infiltration objects
    NumOfZones = 2;
    ZoneReOrder.allocate(NumOfZones);
    ErrorsFound = false;
    GetZoneData(ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    AllocateHeatBalArrays();
    ErrorsFound = false;
    GetSimpleAirModelInputs(ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    SetZoneMassConservationFlag();
    // setup zone equipment configuration
    ZoneEquipConfig.allocate(NumOfZones);

    ZoneEquipConfig(1).ZoneName = "Zone 1";
    ZoneEquipConfig(1).ActualZoneNum = 1;
    ZoneEquipConfig(1).NumInletNodes = 1;
    ZoneEquipConfig(1).InletNode.allocate(1);
    ZoneEquipConfig(1).NumExhaustNodes = 1;
    ZoneEquipConfig(1).ExhaustNode.allocate(1);
    ZoneEquipConfig(1).ZoneNode = 1;
    ZoneEquipConfig(1).InletNode(1) = 2;
    ZoneEquipConfig(1).ExhaustNode(1) = 3;
    ZoneEquipConfig(1).NumReturnNodes = 1;
    ZoneEquipConfig(1).ReturnNode.allocate(1);
    ZoneEquipConfig(1).ReturnNode(1) = 4;
    ZoneEquipConfig(1).IsControlled = true;
    ZoneEquipConfig(1).ReturnFlowSchedPtrNum = ScheduleAlwaysOn;
    ZoneEquipConfig(1).InletNodeAirLoopNum.allocate(1);
    ZoneEquipConfig(1).InletNodeADUNum.allocate(1);
    ZoneEquipConfig(1).AirDistUnitCool.allocate(1);
    ZoneEquipConfig(1).AirDistUnitHeat.allocate(1);
    ZoneEquipConfig(1).InletNodeAirLoopNum(1) = 1;
    ZoneEquipConfig(1).InletNodeADUNum(1) = 0;
    ZoneEquipConfig(1).AirDistUnitCool(1).InNode = 2;
    ZoneEquipConfig(1).ReturnNodeAirLoopNum.allocate(1);
    ZoneEquipConfig(1).ReturnNodeInletNum.allocate(1);
    ZoneEquipConfig(1).ReturnNodeAirLoopNum(1) = 1;
    ZoneEquipConfig(1).ReturnNodeInletNum(1) = 1;

    ZoneEquipConfig(2).ZoneName = "Zone 2";
    ZoneEquipConfig(2).ActualZoneNum = 2;
    ZoneEquipConfig(2).NumExhaustNodes = 1;
    ZoneEquipConfig(2).ExhaustNode.allocate(1);
    ZoneEquipConfig(2).NumInletNodes = 1;
    ZoneEquipConfig(2).InletNode.allocate(1);
    ZoneEquipConfig(2).ZoneNode = 5;
    ZoneEquipConfig(2).InletNode(1) = 6;
    ZoneEquipConfig(2).ExhaustNode(1) = 7;
    ZoneEquipConfig(2).NumReturnNodes = 1;
    ZoneEquipConfig(2).ReturnNode.allocate(1);
    ZoneEquipConfig(2).ReturnNode(1) = 8;
    ZoneEquipConfig(2).IsControlled = true;
    ZoneEquipConfig(2).ReturnFlowSchedPtrNum = ScheduleAlwaysOn;
    ZoneEquipConfig(2).InletNodeAirLoopNum.allocate(1);
    ZoneEquipConfig(2).InletNodeADUNum.allocate(1);
    ZoneEquipConfig(2).AirDistUnitCool.allocate(1);
    ZoneEquipConfig(2).AirDistUnitHeat.allocate(1);
    ZoneEquipConfig(2).InletNodeAirLoopNum(1) = 1;
    ZoneEquipConfig(2).InletNodeADUNum(1) = 0;
    ZoneEquipConfig(2).AirDistUnitCool(1).InNode = 6;
    ZoneEquipConfig(2).ReturnNodeAirLoopNum.allocate(1);
    ZoneEquipConfig(2).ReturnNodeInletNum.allocate(1);
    ZoneEquipConfig(2).ReturnNodeAirLoopNum(1) = 1;
    ZoneEquipConfig(2).ReturnNodeInletNum(1) = 1;

    ZoneEquipInputsFilled = true;
    NumPrimaryAirSys = 1;
    AirLoopFlow.allocate(1);
    PrimaryAirSystem.allocate(1);
    PrimaryAirSystem(1).OASysExists = true;
    Node.allocate(8);

    Node(1).MassFlowRate = 0.0; // Zone 1 zone node
    Node(2).MassFlowRate = 1.0; // Zone 1 inlet node
    Node(3).MassFlowRate = 2.0; // Zone 1 exhaust node
    Node(4).MassFlowRate = 9.0; // Zone 1 return node
    ZoneEquipConfig(1).ZoneExh = 2.0;

    Node(5).MassFlowRate = 0.0; // Zone 2 zone node
    Node(6).MassFlowRate = 2.0; // Zone 2 inlet node
    Node(7).MassFlowRate = 0.0; // Zone 2 exhaust node
    Node(8).MassFlowRate = 8.0; // Zone 2 return node
    ZoneEquipConfig(2).ZoneExh = 0.0;
    AirLoopFlow(1).MaxOutAir = Node(2).MassFlowRate + Node(6).MassFlowRate;
    Infiltration(1).MassFlowRate = 0.5;
    Mixing(1).MixingMassFlowRate = 0.1;

    // call zone air mass balance
    CalcZoneMassBalance();
    EXPECT_EQ(Node(4).MassFlowRate, 0.0);         // Zone 1 return node (max(0.0, 1-2)
    EXPECT_EQ(Infiltration(1).MassFlowRate, 1.0); // Zone 1 infiltration flow rate (2 - 1)
    EXPECT_EQ(Mixing(1).MixingMassFlowRate, 0.1); // Zone 1 to Zone 2 mixing flow rate (unchanged)
    EXPECT_EQ(Node(8).MassFlowRate,
              2.0); // Zone 2 return node (should be 2 now, because this has zone mass conservation active, so return should equal supply)

    ZoneReOrder.deallocate();
    ZoneEquipConfig.deallocate();
    Node.deallocate();
    PrimaryAirSystem.deallocate();
    AirLoopFlow.deallocate();
    NumPrimaryAirSys = 0;
}

TEST_F(EnergyPlusFixture, HeatBalanceManager_ZoneAirMassFlowConservationData3)
{
    // Test get input for ZoneAirMassFlowConservation object

    std::string const idf_objects = delimited_string({"Version,8.3;",
                                                      "Building,",
                                                      "My Building, !- Name",
                                                      "30., !- North Axis{ deg }",
                                                      "City, !- Terrain",
                                                      "0.04, !- Loads Convergence Tolerance Value",
                                                      "0.4, !- Temperature Convergence Tolerance Value{ deltaC }",
                                                      "FullExterior, !- Solar Distribution",
                                                      "25, !- Maximum Number of Warmup Days",
                                                      "6;                       !- Minimum Number of Warmup Days",
                                                      "ZoneAirMassFlowConservation,",
                                                      "No, !- Adjust Zone Mixing For Zone Air Mass Flow Balance",
                                                      "None, !- Infiltration Balancing Method",
                                                      ";                !- Infiltration Balancing Zones"});

    ASSERT_TRUE(process_idf(idf_objects));

    bool ErrorsFound(false); // If errors detected in input

    // call to process input
    ErrorsFound = false;
    GetProjectControlData(ErrorsFound); // returns ErrorsFound false, ZoneAirMassFlowConservation never sets it
    EXPECT_FALSE(ErrorsFound);
    EXPECT_FALSE(ZoneAirMassFlow.EnforceZoneMassBalance);
    EXPECT_FALSE(ZoneAirMassFlow.BalanceMixing);
    EXPECT_EQ(ZoneAirMassFlow.InfiltrationTreatment, NoInfiltrationFlow);
    EXPECT_EQ(ZoneAirMassFlow.InfiltrationZoneType, 0);
}

TEST_F(EnergyPlusFixture, HeatBalanceManager_ZoneAirMassFlowConservationReportVariableTest)
{
    // Test get output variables for ZoneAirMassFlowConservation object #5637

    std::string const idf_objects = delimited_string({
        "Version,8.5;",
        "Building,",
        "My Building, !- Name",
        "30., !- North Axis{ deg }",
        "City, !- Terrain",
        "0.04, !- Loads Convergence Tolerance Value",
        "0.4, !- Temperature Convergence Tolerance Value{ deltaC }",
        "FullExterior, !- Solar Distribution",
        "25, !- Maximum Number of Warmup Days",
        "6;                       !- Minimum Number of Warmup Days",
        "ZoneAirMassFlowConservation,",
        "Yes, !- Adjust Zone Mixing For Zone Air Mass Flow Balance",
        "AdjustInfiltrationFlow, !- Infiltration Balancing Method",
        "AllZones;                !- Infiltration Balancing Zones",

        "  Zone,",
        "    WEST ZONE,               !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1,                       !- Multiplier",
        "    autocalculate,           !- Ceiling Height {m}",
        "    autocalculate;           !- Volume {m3}",

        "  Zone,",
        "    EAST ZONE,               !- Name",
        "    0,                       !- Direction of Relative North {deg}",
        "    0,                       !- X Origin {m}",
        "    0,                       !- Y Origin {m}",
        "    0,                       !- Z Origin {m}",
        "    1,                       !- Type",
        "    1,                       !- Multiplier",
        "    autocalculate,           !- Ceiling Height {m}",
        "    autocalculate;           !- Volume {m3}",
        " Output:Variable,",
        "   *, !- Key Value",
        "   Zone Air Mass Balance Exhaust Mass Flow Rate, !- Variable Name",
        "   hourly;                  !- Reporting Frequency",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    bool ErrorsFound(false); // If errors detected in input

    // call to process input
    ErrorsFound = false;
    GetProjectControlData(ErrorsFound); // returns ErrorsFound false, ZoneAirMassFlowConservation never sets it
    EXPECT_FALSE(ErrorsFound);
    NumOfZones = 2;
    ZoneReOrder.allocate(NumOfZones);
    ErrorsFound = false;
    GetZoneData(ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    ErrorsFound = false;
    GetSimpleAirModelInputs(ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    // first 2 have indexes swapped now since they are in lexicigraphical order now according to the new input processor
    EXPECT_EQ("WEST ZONE:Zone Air Mass Balance Exhaust Mass Flow Rate", OutputProcessor::RVariableTypes(1).VarName);
    EXPECT_EQ("EAST ZONE:Zone Air Mass Balance Exhaust Mass Flow Rate", OutputProcessor::RVariableTypes(2).VarName);
    EXPECT_EQ(1, OutputProcessor::RVariableTypes(1).ReportID);
    EXPECT_EQ(2, OutputProcessor::RVariableTypes(2).ReportID);
}

TEST_F(EnergyPlusFixture, HeatBalanceManager_GetMaterialRoofVegetation)
{
    std::string const idf_objects = delimited_string({
        "  Version,8.6;",

        "  Material:RoofVegetation,",
        "    ThickSoil,               !- Name",
        "    0.5,                     !- Height of Plants {m}",
        "    5,                       !- Leaf Area Index {dimensionless}",
        "    0.2,                     !- Leaf Reflectivity {dimensionless}",
        "    0.95,                    !- Leaf Emissivity",
        "    180,                     !- Minimum Stomatal Resistance {s/m}",
        "    EcoRoofSoil,             !- Soil Layer Name",
        "    MediumSmooth,            !- Roughness",
        "    0.36,                    !- Thickness {m}",
        "    0.4,                     !- Conductivity of Dry Soil {W/m-K}",
        "    641,                     !- Density of Dry Soil {kg/m3}",
        "    1100,                    !- Specific Heat of Dry Soil {J/kg-K}",
        "    0.95,                    !- Thermal Absorptance",
        "    0.8,                     !- Solar Absorptance",
        "    0.7,                     !- Visible Absorptance",
        "    0.4,                     !- Saturation Volumetric Moisture Content of the Soil Layer",
        "    0.01,                    !- Residual Volumetric Moisture Content of the Soil Layer",
        "    0.45,                    !- Initial Volumetric Moisture Content of the Soil Layer",
        "    Advanced;                !- Moisture Diffusion Calculation Method",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    bool ErrorsFound(false);
    GetMaterialData(ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    // check the "Material:RoofVegetation" names
    EXPECT_EQ(Material(1).Name, "THICKSOIL");
    // check maximum (saturated) moisture content
    EXPECT_EQ(0.4, Material(1).Porosity);
    // check initial moisture Content was reset
    EXPECT_EQ(0.4, Material(1).InitMoisture); // reset from 0.45 to 0.4 during get input
}

TEST_F(EnergyPlusFixture, HeatBalanceManager_WarmUpConvergenceSmallLoadTest)
{

    WarmupFlag = false;
    DayOfSim = 7;
    MinNumberOfWarmupDays = 25;
    NumOfZones = 1;
    WarmupConvergenceValues.allocate(NumOfZones);
    TempConvergTol = 0.01;
    LoadsConvergTol = 0.01;
    MaxTempPrevDay.allocate(NumOfZones);
    MaxTempPrevDay(1) = 23.0;
    MaxTempZone.allocate(NumOfZones);
    MaxTempZone(1) = 23.0;
    MinTempPrevDay.allocate(NumOfZones);
    MinTempPrevDay(1) = 23.0;
    MinTempZone.allocate(NumOfZones);
    MinTempZone(1) = 23.0;
    MaxHeatLoadZone.allocate(NumOfZones);
    MaxHeatLoadPrevDay.allocate(NumOfZones);
    WarmupConvergenceValues(1).TestMaxHeatLoadValue = 0.0;
    MaxCoolLoadZone.allocate(NumOfZones);
    MaxCoolLoadPrevDay.allocate(NumOfZones);
    WarmupConvergenceValues(1).TestMaxCoolLoadValue = 0.0;

    // Test 1: All Maxs both less than MinLoad (100.0)
    MaxHeatLoadZone(1) = 50.0;
    MaxHeatLoadPrevDay(1) = 90.0;
    MaxCoolLoadZone(1) = 50.0;
    MaxCoolLoadPrevDay(1) = 90.0;
    CheckWarmupConvergence();
    EXPECT_EQ(WarmupConvergenceValues(1).PassFlag(3), 2);
    EXPECT_EQ(WarmupConvergenceValues(1).PassFlag(4), 2);
    EXPECT_NEAR(WarmupConvergenceValues(1).TestMaxHeatLoadValue, 0.0, 0.0001);
    EXPECT_NEAR(WarmupConvergenceValues(1).TestMaxCoolLoadValue, 0.0, 0.0001);

    // Test 2: Max Previous Day both less than MinLoad
    MaxHeatLoadZone(1) = 100.5;
    MaxHeatLoadPrevDay(1) = 90.0;
    MaxCoolLoadZone(1) = 100.5;
    MaxCoolLoadPrevDay(1) = 90.0;
    CheckWarmupConvergence();
    EXPECT_EQ(WarmupConvergenceValues(1).PassFlag(3), 2);
    EXPECT_EQ(WarmupConvergenceValues(1).PassFlag(4), 2);
    EXPECT_NEAR(WarmupConvergenceValues(1).TestMaxHeatLoadValue, 0.005, 0.0001);
    EXPECT_NEAR(WarmupConvergenceValues(1).TestMaxCoolLoadValue, 0.005, 0.0001);

    // Test 3: Max Current Day both less than MinLoad
    MaxHeatLoadZone(1) = 90.0;
    MaxHeatLoadPrevDay(1) = 100.5;
    MaxCoolLoadZone(1) = 90.0;
    MaxCoolLoadPrevDay(1) = 100.5;
    CheckWarmupConvergence();
    EXPECT_EQ(WarmupConvergenceValues(1).PassFlag(3), 2);
    EXPECT_EQ(WarmupConvergenceValues(1).PassFlag(4), 2);
    EXPECT_NEAR(WarmupConvergenceValues(1).TestMaxHeatLoadValue, 0.005, 0.0001);
    EXPECT_NEAR(WarmupConvergenceValues(1).TestMaxCoolLoadValue, 0.005, 0.0001);

    // Test 4: Everything greater than MinLoad (pass convergence test)
    MaxHeatLoadZone(1) = 201.0;
    MaxHeatLoadPrevDay(1) = 200.0;
    MaxCoolLoadZone(1) = 201.0;
    MaxCoolLoadPrevDay(1) = 200.0;
    CheckWarmupConvergence();
    EXPECT_EQ(WarmupConvergenceValues(1).PassFlag(3), 2);
    EXPECT_EQ(WarmupConvergenceValues(1).PassFlag(4), 2);
    EXPECT_NEAR(WarmupConvergenceValues(1).TestMaxHeatLoadValue, 0.005, 0.0001);
    EXPECT_NEAR(WarmupConvergenceValues(1).TestMaxCoolLoadValue, 0.005, 0.0001);

    // Test 5: Everything greater than MinLoad (fail convergence test)
    MaxHeatLoadZone(1) = 210.0;
    MaxHeatLoadPrevDay(1) = 200.0;
    MaxCoolLoadZone(1) = 210.0;
    MaxCoolLoadPrevDay(1) = 200.0;
    CheckWarmupConvergence();
    EXPECT_EQ(WarmupConvergenceValues(1).PassFlag(3), 1);
    EXPECT_EQ(WarmupConvergenceValues(1).PassFlag(4), 1);
    EXPECT_NEAR(WarmupConvergenceValues(1).TestMaxHeatLoadValue, 0.05, 0.005);
    EXPECT_NEAR(WarmupConvergenceValues(1).TestMaxCoolLoadValue, 0.05, 0.005);
}

TEST_F(EnergyPlusFixture, HeatBalanceManager_TestZonePropertyLocalEnv)
{

    std::string const idf_objects =
        delimited_string({"  Version,9.0;",

                          "  Building,",
                          "    House with Local Air Nodes,  !- Name",
                          "    0,                       !- North Axis {deg}",
                          "    Suburbs,                 !- Terrain",
                          "    0.001,                   !- Loads Convergence Tolerance Value",
                          "    0.0050000,               !- Temperature Convergence Tolerance Value {deltaC}",
                          "    FullInteriorAndExterior, !- Solar Distribution",
                          "    25,                      !- Maximum Number of Warmup Days",
                          "    6;                       !- Minimum Number of Warmup Days",

                          "  Timestep,6;",

                          "  SurfaceConvectionAlgorithm:Inside,TARP;",

                          "  SurfaceConvectionAlgorithm:Outside,DOE-2;",

                          "  HeatBalanceAlgorithm,ConductionTransferFunction;",

                          "  Output:DebuggingData,0,0;",

                          "  SimulationControl,",
                          "    No,                      !- Do Zone Sizing Calculation",
                          "    No,                      !- Do System Sizing Calculation",
                          "    No,                      !- Do Plant Sizing Calculation",
                          "    Yes,                     !- Run Simulation for Sizing Periods",
                          "    Yes;                     !- Run Simulation for Weather File Run Periods",

                          "  RunPeriod,",
                          "    ,                        !- Name",
                          "    1,                       !- Begin Month",
                          "    14,                      !- Begin Day of Month",
                          "    ,                        !- Begin Year",
                          "    1,                       !- End Month",
                          "    14,                      !- End Day of Month",
                          "    ,                        !- End Year",
                          "    Tuesday,                 !- Day of Week for Start Day",
                          "    Yes,                     !- Use Weather File Holidays and Special Days",
                          "    Yes,                     !- Use Weather File Daylight Saving Period",
                          "    No,                      !- Apply Weekend Holiday Rule",
                          "    Yes,                     !- Use Weather File Rain Indicators",
                          "    Yes;                     !- Use Weather File Snow Indicators",

                          "  RunPeriod,",
                          "    ,                        !- Name",
                          "    7,                       !- Begin Month",
                          "    7,                       !- Begin Day of Month",
                          "    ,                        !- Begin Year",
                          "    7,                       !- End Month",
                          "    7,                       !- End Day of Month",
                          "    ,                        !- End Year",
                          "    Tuesday,                 !- Day of Week for Start Day",
                          "    Yes,                     !- Use Weather File Holidays and Special Days",
                          "    Yes,                     !- Use Weather File Daylight Saving Period",
                          "    No,                      !- Apply Weekend Holiday Rule",
                          "    Yes,                     !- Use Weather File Rain Indicators",
                          "    No;                      !- Use Weather File Snow Indicators",

                          "  Site:Location,",
                          "    CHICAGO_IL_USA TMY2-94846,  !- Name",
                          "    41.78,                   !- Latitude {deg}",
                          "    -87.75,                  !- Longitude {deg}",
                          "    -6.00,                   !- Time Zone {hr}",
                          "    190.00;                  !- Elevation {m}",

                          "  SizingPeriod:DesignDay,",
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

                          "  SizingPeriod:DesignDay,",
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

                          "  Site:GroundTemperature:BuildingSurface,20.03,20.03,20.13,20.30,20.43,20.52,20.62,20.77,20.78,20.55,20.44,20.20;",

                          "  Material,",
                          "    A1 - 1 IN STUCCO,        !- Name",
                          "    Smooth,                  !- Roughness",
                          "    2.5389841E-02,           !- Thickness {m}",
                          "    0.6918309,               !- Conductivity {W/m-K}",
                          "    1858.142,                !- Density {kg/m3}",
                          "    836.8000,                !- Specific Heat {J/kg-K}",
                          "    0.9000000,               !- Thermal Absorptance",
                          "    0.9200000,               !- Solar Absorptance",
                          "    0.9200000;               !- Visible Absorptance",

                          "  Material,",
                          "    CB11,                    !- Name",
                          "    MediumRough,             !- Roughness",
                          "    0.2032000,               !- Thickness {m}",
                          "    1.048000,                !- Conductivity {W/m-K}",
                          "    1105.000,                !- Density {kg/m3}",
                          "    837.0000,                !- Specific Heat {J/kg-K}",
                          "    0.9000000,               !- Thermal Absorptance",
                          "    0.2000000,               !- Solar Absorptance",
                          "    0.2000000;               !- Visible Absorptance",

                          "  Material,",
                          "    GP01,                    !- Name",
                          "    MediumSmooth,            !- Roughness",
                          "    1.2700000E-02,           !- Thickness {m}",
                          "    0.1600000,               !- Conductivity {W/m-K}",
                          "    801.0000,                !- Density {kg/m3}",
                          "    837.0000,                !- Specific Heat {J/kg-K}",
                          "    0.9000000,               !- Thermal Absorptance",
                          "    0.7500000,               !- Solar Absorptance",
                          "    0.7500000;               !- Visible Absorptance",

                          "  Material,",
                          "    IN02,                    !- Name",
                          "    Rough,                   !- Roughness",
                          "    9.0099998E-02,           !- Thickness {m}",
                          "    4.3000001E-02,           !- Conductivity {W/m-K}",
                          "    10.00000,                !- Density {kg/m3}",
                          "    837.0000,                !- Specific Heat {J/kg-K}",
                          "    0.9000000,               !- Thermal Absorptance",
                          "    0.7500000,               !- Solar Absorptance",
                          "    0.7500000;               !- Visible Absorptance",

                          "  Material,",
                          "    IN05,                    !- Name",
                          "    Rough,                   !- Roughness",
                          "    0.2458000,               !- Thickness {m}",
                          "    4.3000001E-02,           !- Conductivity {W/m-K}",
                          "    10.00000,                !- Density {kg/m3}",
                          "    837.0000,                !- Specific Heat {J/kg-K}",
                          "    0.9000000,               !- Thermal Absorptance",
                          "    0.7500000,               !- Solar Absorptance",
                          "    0.7500000;               !- Visible Absorptance",

                          "  Material,",
                          "    PW03,                    !- Name",
                          "    MediumSmooth,            !- Roughness",
                          "    1.2700000E-02,           !- Thickness {m}",
                          "    0.1150000,               !- Conductivity {W/m-K}",
                          "    545.0000,                !- Density {kg/m3}",
                          "    1213.000,                !- Specific Heat {J/kg-K}",
                          "    0.9000000,               !- Thermal Absorptance",
                          "    0.7800000,               !- Solar Absorptance",
                          "    0.7800000;               !- Visible Absorptance",

                          "  Material,",
                          "    CC03,                    !- Name",
                          "    MediumRough,             !- Roughness",
                          "    0.1016000,               !- Thickness {m}",
                          "    1.310000,                !- Conductivity {W/m-K}",
                          "    2243.000,                !- Density {kg/m3}",
                          "    837.0000,                !- Specific Heat {J/kg-K}",
                          "    0.9000000,               !- Thermal Absorptance",
                          "    0.6500000,               !- Solar Absorptance",
                          "    0.6500000;               !- Visible Absorptance",

                          "  Material,",
                          "    HF-A3,                   !- Name",
                          "    Smooth,                  !- Roughness",
                          "    1.5000000E-03,           !- Thickness {m}",
                          "    44.96960,                !- Conductivity {W/m-K}",
                          "    7689.000,                !- Density {kg/m3}",
                          "    418.0000,                !- Specific Heat {J/kg-K}",
                          "    0.9000000,               !- Thermal Absorptance",
                          "    0.2000000,               !- Solar Absorptance",
                          "    0.2000000;               !- Visible Absorptance",

                          "  Material:NoMass,",
                          "    AR02,                    !- Name",
                          "    VeryRough,               !- Roughness",
                          "    7.8000002E-02,           !- Thermal Resistance {m2-K/W}",
                          "    0.9000000,               !- Thermal Absorptance",
                          "    0.7000000,               !- Solar Absorptance",
                          "    0.7000000;               !- Visible Absorptance",

                          "  Material:NoMass,",
                          "    CP02,                    !- Name",
                          "    Rough,                   !- Roughness",
                          "    0.2170000,               !- Thermal Resistance {m2-K/W}",
                          "    0.9000000,               !- Thermal Absorptance",
                          "    0.7500000,               !- Solar Absorptance",
                          "    0.7500000;               !- Visible Absorptance",

                          "  Construction,",
                          "    EXTWALL:LIVING,          !- Name",
                          "    A1 - 1 IN STUCCO,        !- Outside Layer",
                          "    GP01;                    !- Layer 3",

                          "  Construction,",
                          "    FLOOR:LIVING,            !- Name",
                          "    CC03,                    !- Outside Layer",
                          "    CP02;                    !- Layer 2",

                          "  Construction,",
                          "    ROOF,                    !- Name",
                          "    AR02,                    !- Outside Layer",
                          "    PW03;                    !- Layer 2",

                          "  Zone,",
                          "    LIVING ZONE,             !- Name",
                          "    0,                       !- Direction of Relative North {deg}",
                          "    0,                       !- X Origin {m}",
                          "    0,                       !- Y Origin {m}",
                          "    0,                       !- Z Origin {m}",
                          "    1,                       !- Type",
                          "    1,                       !- Multiplier",
                          "    autocalculate,           !- Ceiling Height {m}",
                          "    autocalculate;           !- Volume {m3}",

                          "  GlobalGeometryRules,",
                          "    UpperLeftCorner,         !- Starting Vertex Position",
                          "    CounterClockWise,        !- Vertex Entry Direction",
                          "    World;                   !- Coordinate System",

                          "  BuildingSurface:Detailed,",
                          "    Living:North,            !- Name",
                          "    Wall,                    !- Surface Type",
                          "    EXTWALL:LIVING,          !- Construction Name",
                          "    LIVING ZONE,             !- Zone Name",
                          "    Outdoors,                !- Outside Boundary Condition",
                          "    ,                        !- Outside Boundary Condition Object",
                          "    SunExposed,              !- Sun Exposure",
                          "    WindExposed,             !- Wind Exposure",
                          "    0.5000000,               !- View Factor to Ground",
                          "    4,                       !- Number of Vertices",
                          "    1,1,1,  !- X,Y,Z ==> Vertex 1 {m}",
                          "    1,1,0,  !- X,Y,Z ==> Vertex 2 {m}",
                          "    0,1,0,  !- X,Y,Z ==> Vertex 3 {m}",
                          "    0,1,1;  !- X,Y,Z ==> Vertex 4 {m}",

                          "  BuildingSurface:Detailed,",
                          "    Living:East,             !- Name",
                          "    Wall,                    !- Surface Type",
                          "    EXTWALL:LIVING,          !- Construction Name",
                          "    LIVING ZONE,             !- Zone Name",
                          "    Outdoors,                !- Outside Boundary Condition",
                          "    ,                        !- Outside Boundary Condition Object",
                          "    SunExposed,              !- Sun Exposure",
                          "    WindExposed,             !- Wind Exposure",
                          "    0.5000000,               !- View Factor to Ground",
                          "    4,                       !- Number of Vertices",
                          "    1,0,1,  !- X,Y,Z ==> Vertex 1 {m}",
                          "    1,0,0,  !- X,Y,Z ==> Vertex 2 {m}",
                          "    1,1,0,  !- X,Y,Z ==> Vertex 3 {m}",
                          "    1,1,1;  !- X,Y,Z ==> Vertex 4 {m}",

                          "  BuildingSurface:Detailed,",
                          "    Living:South,            !- Name",
                          "    Wall,                    !- Surface Type",
                          "    EXTWALL:LIVING,          !- Construction Name",
                          "    LIVING ZONE,             !- Zone Name",
                          "    Outdoors,                !- Outside Boundary Condition",
                          "    ,                        !- Outside Boundary Condition Object",
                          "    SunExposed,              !- Sun Exposure",
                          "    WindExposed,             !- Wind Exposure",
                          "    0.5000000,               !- View Factor to Ground",
                          "    4,                       !- Number of Vertices",
                          "    0,0,1,  !- X,Y,Z ==> Vertex 1 {m}",
                          "    0,0,0,  !- X,Y,Z ==> Vertex 2 {m}",
                          "    1,0,0,  !- X,Y,Z ==> Vertex 3 {m}",
                          "    1,0,1;  !- X,Y,Z ==> Vertex 4 {m}",

                          "  BuildingSurface:Detailed,",
                          "    Living:West,             !- Name",
                          "    Wall,                    !- Surface Type",
                          "    EXTWALL:LIVING,          !- Construction Name",
                          "    LIVING ZONE,             !- Zone Name",
                          "    Outdoors,                !- Outside Boundary Condition",
                          "    ,                        !- Outside Boundary Condition Object",
                          "    SunExposed,              !- Sun Exposure",
                          "    WindExposed,             !- Wind Exposure",
                          "    0.5000000,               !- View Factor to Ground",
                          "    4,                       !- Number of Vertices",
                          "    0,1,1,  !- X,Y,Z ==> Vertex 1 {m}",
                          "    0,1,0,  !- X,Y,Z ==> Vertex 2 {m}",
                          "    0,0,0,  !- X,Y,Z ==> Vertex 3 {m}",
                          "    0,0,1;  !- X,Y,Z ==> Vertex 4 {m}",

                          "  BuildingSurface:Detailed,",
                          "    Living:Floor,            !- Name",
                          "    FLOOR,                   !- Surface Type",
                          "    FLOOR:LIVING,            !- Construction Name",
                          "    LIVING ZONE,             !- Zone Name",
                          "    Surface,                 !- Outside Boundary Condition",
                          "    Living:Floor,            !- Outside Boundary Condition Object",
                          "    NoSun,                   !- Sun Exposure",
                          "    NoWind,                  !- Wind Exposure",
                          "    0,                       !- View Factor to Ground",
                          "    4,                       !- Number of Vertices",
                          "    0,0,0,  !- X,Y,Z ==> Vertex 1 {m}",
                          "    0,1,0,  !- X,Y,Z ==> Vertex 2 {m}",
                          "    1,1,0,  !- X,Y,Z ==> Vertex 3 {m}",
                          "    1,0,0;  !- X,Y,Z ==> Vertex 4 {m}",

                          "  BuildingSurface:Detailed,",
                          "    Living:Ceiling,          !- Name",
                          "    ROOF,                 !- Surface Type",
                          "    ROOF,          !- Construction Name",
                          "    LIVING ZONE,             !- Zone Name",
                          "    Outdoors,                !- Outside Boundary Condition",
                          "    ,                        !- Outside Boundary Condition Object",
                          "    SunExposed,              !- Sun Exposure",
                          "    WindExposed,             !- Wind Exposure",
                          "    0,                       !- View Factor to Ground",
                          "    4,                       !- Number of Vertices",
                          "    0,1,1,  !- X,Y,Z ==> Vertex 1 {m}",
                          "    0,0,1,  !- X,Y,Z ==> Vertex 2 {m}",
                          "    1,0,1,  !- X,Y,Z ==> Vertex 3 {m}",
                          "    1,1,1;  !- X,Y,Z ==> Vertex 4 {m}",

                          "  ZoneProperty:LocalEnvironment,",
                          "    LocEnv:LIVING ZONE,           !- Name",
                          "    LIVING ZONE,                  !- Exterior Surface Name",
                          "    OutdoorAirNode:0001;          !- Outdoor Air Node Name",

                          "  OutdoorAir:Node,",
                          "    OutdoorAirNode:0001,          !- Name",
                          "    ,                             !- Height Above Ground",
                          "    OutdoorAirNodeDryBulb:0001,   !- Drybulb Temperature Schedule Name",
                          "    OutdoorAirNodeWetBulb:0001,   !- Wetbulb Schedule Name",
                          "    OutdoorAirNodeWindSpeed:0001, !- Wind Speed Schedule Name",
                          "    OutdoorAirNodeWindDir:0001;   !- Wind Direction Schedule Name",

                          "  ScheduleTypeLimits,",
                          "    Any Number;                   !- Name",

                          "  Schedule:Compact,",
                          "    OutdoorAirNodeDryBulb:0001,   !- Name",
                          "    Any Number,                   !- Schedule Type Limits Name",
                          "    Through: 12/31,               !- Field 1",
                          "    For: AllDays,                 !- Field 2",
                          "    Until: 24:00, 15.0;           !- Field 3",

                          "  Schedule:Compact,",
                          "    OutdoorAirNodeWetBulb:0001,   !- Name",
                          "    Any Number,                   !- Schedule Type Limits Name",
                          "    Through: 12/31,               !- Field 1",
                          "    For: AllDays,                 !- Field 2",
                          "    Until: 24:00, 12.0;           !- Field 3",

                          "  Schedule:Compact,",
                          "    OutdoorAirNodeWindSpeed:0001, !- Name",
                          "    Any Number,                   !- Schedule Type Limits Name",
                          "    Through: 12/31,               !- Field 1",
                          "    For: AllDays,                 !- Field 2",
                          "    Until: 24:00, 1.23;           !- Field 3",

                          "  Schedule:Compact,",
                          "    OutdoorAirNodeWindDir:0001,   !- Name",
                          "    Any Number,                   !- Schedule Type Limits Name",
                          "    Through: 12/31,               !- Field 1",
                          "    For: AllDays,                 !- Field 2",
                          "    Until: 24:00, 90;             !- Field 3"});

    ASSERT_TRUE(process_idf(idf_objects));
    bool ErrorsFound = false;

    ScheduleManager::ProcessScheduleInput();

    HeatBalanceManager::GetProjectControlData(ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    HeatBalanceManager::GetZoneData(ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    HeatBalanceManager::GetMaterialData(ErrorsFound);
    EXPECT_FALSE(ErrorsFound);
    HeatBalanceManager::GetConstructData(ErrorsFound);
    EXPECT_FALSE(ErrorsFound);

    EXPECT_TRUE(DataGlobals::AnyLocalEnvironmentsInModel);

    DataZoneEquipment::ZoneEquipConfig.allocate(1);
    DataZoneEquipment::ZoneEquipConfig(1).ZoneName = "LIVING ZONE";
    DataZoneEquipment::ZoneEquipConfig(1).ActualZoneNum = 1;
    std::vector<int> controlledZoneEquipConfigNums;
    controlledZoneEquipConfigNums.push_back(1);
    DataHeatBalance::Zone(1).IsControlled = true;

    DataZoneEquipment::ZoneEquipConfig(1).NumInletNodes = 2;
    DataZoneEquipment::ZoneEquipConfig(1).InletNode.allocate(2);
    DataZoneEquipment::ZoneEquipConfig(1).InletNode(1) = 1;
    DataZoneEquipment::ZoneEquipConfig(1).InletNode(2) = 2;
    DataZoneEquipment::ZoneEquipConfig(1).NumExhaustNodes = 1;
    DataZoneEquipment::ZoneEquipConfig(1).ExhaustNode.allocate(1);
    DataZoneEquipment::ZoneEquipConfig(1).ExhaustNode(1) = 3;
    DataZoneEquipment::ZoneEquipConfig(1).NumReturnNodes = 1;
    DataZoneEquipment::ZoneEquipConfig(1).ReturnNode.allocate(1);
    DataZoneEquipment::ZoneEquipConfig(1).ReturnNode(1) = 4;

    DataHeatBalance::TempEffBulkAir.allocate(6);

    DataHeatBalance::HConvIn.allocate(6);
    DataHeatBalance::HConvIn(1) = 0.5;
    DataHeatBalance::HConvIn(2) = 0.5;
    DataHeatBalance::HConvIn(3) = 0.5;
    DataHeatBalance::HConvIn(4) = 0.5;
    DataHeatBalance::HConvIn(5) = 0.5;
    DataHeatBalance::HConvIn(6) = 0.5;

    DataGlobals::KickOffSimulation = true;
    DataHeatBalFanSys::ZoneLatentGain.allocate(1);
    DataGlobals::TimeStepZoneSec = 900;
    DataHeatBalance::ZoneWinHeatGain.allocate(1);
    DataHeatBalance::ZoneWinHeatGainRep.allocate(1);
    DataHeatBalance::ZoneWinHeatGainRepEnergy.allocate(1);

    // Set up
    OutAirNodeManager::GetOutAirNodesInput();
    DataEnvironment::OutBaroPress = 101325;
    ScheduleManager::Schedule(1).CurrentValue = 25.0;
    ScheduleManager::Schedule(2).CurrentValue = 20.0;
    ScheduleManager::Schedule(3).CurrentValue = 1.5;
    ScheduleManager::Schedule(4).CurrentValue = 90.0;

    OutAirNodeManager::InitOutAirNodes();

    // Test if local nodes data correctly overwritten
    EXPECT_EQ(25.0, DataLoopNode::Node(1).OutAirDryBulb);
    EXPECT_EQ(20.0, DataLoopNode::Node(1).OutAirWetBulb);
    EXPECT_EQ(1.5, DataLoopNode::Node(1).OutAirWindSpeed);
    EXPECT_EQ(90.0, DataLoopNode::Node(1).OutAirWindDir);
    EXPECT_DOUBLE_EQ(0.012611481326656135, DataLoopNode::Node(1).HumRat);
    EXPECT_DOUBLE_EQ(57247.660939392081, DataLoopNode::Node(1).Enthalpy);

    InitHeatBalance();

    // Test if local value correctly overwritten
    EXPECT_EQ(25.0, Zone(1).OutDryBulbTemp);
    EXPECT_EQ(20.0, Zone(1).OutWetBulbTemp);
    EXPECT_EQ(1.5, Zone(1).WindSpeed);
    EXPECT_EQ(90.0, Zone(1).WindDir);
}

TEST_F(EnergyPlusFixture, HeatBalanceManager_HVACSystemRootFindingAlgorithmInputTest)
{
    // Test eio output for HVACSystemRootFindingAlgorithm

    std::string const idf_objects = delimited_string({
        "Version,9.0;",
        "Building,",
        "My Building, !- Name",
        "30., !- North Axis{ deg }",
        "City, !- Terrain",
        "0.04, !- Loads Convergence Tolerance Value",
        "0.4, !- Temperature Convergence Tolerance Value{ deltaC }",
        "FullExterior, !- Solar Distribution",
        "25, !- Maximum Number of Warmup Days",
        "6;                       !- Minimum Number of Warmup Days",
        "ZoneAirMassFlowConservation,",
        "No, !- Adjust Zone Mixing For Zone Air Mass Flow Balance",
        "None, !- Infiltration Balancing Method",
        ";                !- Infiltration Balancing Zones",
        " HVACSystemRootFindingAlgorithm,",
        " RegulaFalsiThenBisection,!- Algorithm",
        " 5;                       !- Number of Iterations Before Algorithm Switch",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    bool ErrorsFound(false); // If errors detected in input
    ErrorsFound = false;
    GetProjectControlData(ErrorsFound); // returns ErrorsFound false
    EXPECT_FALSE(ErrorsFound);
    EXPECT_EQ(DataHVACGlobals::HVACSystemRootFinding.Algorithm, "REGULAFALSITHENBISECTION");
}

TEST_F(EnergyPlusFixture, HeatBalanceManager_HVACSystemRootFindingAlgorithmNoInputTest)
{
    // Test that root solver algorithm is RegulaFalsi when no HVACSystemRootFindingAlgorithm object exists

    std::string const idf_objects = delimited_string({
        "Version,9.0;",
        "Building,",
        "My Building, !- Name",
        "30., !- North Axis{ deg }",
        "City, !- Terrain",
        "0.04, !- Loads Convergence Tolerance Value",
        "0.4, !- Temperature Convergence Tolerance Value{ deltaC }",
        "FullExterior, !- Solar Distribution",
        "25, !- Maximum Number of Warmup Days",
        "6;                       !- Minimum Number of Warmup Days",
        "ZoneAirMassFlowConservation,",
        "No, !- Adjust Zone Mixing For Zone Air Mass Flow Balance",
        "None, !- Infiltration Balancing Method",
        ";                !- Infiltration Balancing Zones",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    bool ErrorsFound(false); // If errors detected in input
    ErrorsFound = false;
    GetProjectControlData(ErrorsFound); // returns ErrorsFound false
    EXPECT_FALSE(ErrorsFound);
    EXPECT_EQ(DataHVACGlobals::HVACSystemRootFinding.Algorithm, "RegulaFalsi");
}

TEST_F(EnergyPlusFixture, HeatBalanceManager_EMSConstructionTest)
{

    DataIPShortCuts::lAlphaFieldBlanks = true;

    std::string const idf_objects = delimited_string({
        "Version,9.0;",
        "  SimulationControl,",
        "    No,                      !- Do Zone Sizing Calculation",
        "    No,                      !- Do System Sizing Calculation",
        "    No,                      !- Do Plant Sizing Calculation",
        "    Yes,                     !- Run Simulation for Sizing Periods",
        "    No;                      !- Run Simulation for Weather File Run Periods",

        "  SizingPeriod:DesignDay,",
        "    SunnyWinterDay,  !- Name",
        "    1,                       !- Month",
        "    21,                      !- Day of Month",
        "    WinterDesignDay,         !- Day Type",
        "    5.0,                    !- Maximum Dry-Bulb Temperature {C}",
        "    0.0,                    !- Daily Dry-Bulb Temperature Range {deltaC}",
        "    ,                        !- Dry-Bulb Temperature Range Modifier Type",
        "    ,                        !- Dry-Bulb Temperature Range Modifier Day Schedule Name",
        "    Wetbulb,                 !- Humidity Condition Type",
        "    4.0,                    !- Wetbulb or DewPoint at Maximum Dry-Bulb {C}",
        "    ,                        !- Humidity Condition Day Schedule Name",
        "    ,                        !- Humidity Ratio at Maximum Dry-Bulb {kgWater/kgDryAir}",
        "    ,                        !- Enthalpy at Maximum Dry-Bulb {J/kg}",
        "    ,                        !- Daily Wet-Bulb Temperature Range {deltaC}",
        "    83411.,                  !- Barometric Pressure {Pa}",
        "    4,                       !- Wind Speed {m/s}",
        "    120,                     !- Wind Direction {deg}",
        "    No,                      !- Rain Indicator",
        "    No,                      !- Snow Indicator",
        "    No,                      !- Daylight Saving Time Indicator",
        "    ASHRAEClearSky,          !- Solar Model Indicator",
        "    ,                        !- Beam Solar Day Schedule Name",
        "    ,                        !- Diffuse Solar Day Schedule Name",
        "    ,                        !- ASHRAE Clear Sky Optical Depth for Beam Irradiance (taub) {dimensionless}",
        "    ,                        !- ASHRAE Clear Sky Optical Depth for Diffuse Irradiance (taud) {dimensionless}",
        "    1.00;                    !- Sky Clearness",

        "  Site:Location,",
        "    Denver Stapleton Intl Arpt CO USA WMO=724690,  !- Name",
        "    39.77,                   !- Latitude {deg}",
        "    -104.87,                 !- Longitude {deg}",
        "    -7.00,                   !- Time Zone {hr}",
        "    1611.00;                 !- Elevation {m}",
        "Material,",
        "  Concrete Block,          !- Name",
        "  MediumRough,             !- Roughness",
        "  0.1014984,               !- Thickness {m}",
        "  0.3805070,               !- Conductivity {W/m-K}",
        "  608.7016,                !- Density {kg/m3}",
        "  836.8000;                !- Specific Heat {J/kg-K}",
        "Construction,",
        "  WallConstruction,        !- Name",
        "  Concrete Block;          !- Outside Layer",
        "  WindowMaterial:Glazing,",
        "    ELECTRO GLASS DARK STATE,!- Name",
        "    SpectralAverage,         !- Optical Data Type",
        "    ,                        !- Window Glass Spectral Data Set Name",
        "    0.006,                   !- Thickness {m}",
        "    0.111,                   !- Solar Transmittance at Normal Incidence",
        "    0.179,                   !- Front Side Solar Reflectance at Normal Incidence",
        "    0.179,                   !- Back Side Solar Reflectance at Normal Incidence",
        "    0.128,                   !- Visible Transmittance at Normal Incidence",
        "    0.081,                   !- Front Side Visible Reflectance at Normal Incidence",
        "    0.081,                   !- Back Side Visible Reflectance at Normal Incidence",
        "    0.0,                     !- Infrared Transmittance at Normal Incidence",
        "    0.0001,                    !- Front Side Infrared Hemispherical Emissivity",
        "    0.0001,                    !- Back Side Infrared Hemispherical Emissivity",
        "    0.9;                     !- Conductivity {W/m-K}",
        "  WindowMaterial:Glazing,",
        "    ELECTRO GLASS LIGHT STATE,!- Name",
        "    SpectralAverage,         !- Optical Data Type",
        "    ,                        !- Window Glass Spectral Data Set Name",
        "    0.006,                   !- Thickness {m}",
        "    0.9,                   !- Solar Transmittance at Normal Incidence",
        "    0.1,                   !- Front Side Solar Reflectance at Normal Incidence",
        "    0.1,                   !- Back Side Solar Reflectance at Normal Incidence",
        "    0.9,                   !- Visible Transmittance at Normal Incidence",
        "    0.1,                   !- Front Side Visible Reflectance at Normal Incidence",
        "    0.1,                   !- Back Side Visible Reflectance at Normal Incidence",
        "    0.0,                     !- Infrared Transmittance at Normal Incidence",
        "    0.0001,                    !- Front Side Infrared Hemispherical Emissivity",
        "    0.0001,                    !- Back Side Infrared Hemispherical Emissivity",
        "    0.9;                     !- Conductivity {W/m-K}",
        "Construction,",
        "  WindowConstruction1,      !- Name",
        "  ELECTRO GLASS LIGHT STATE;          !- Outside Layer",
        "Construction,",
        "  WindowConstruction2,      !- Name",
        "  ELECTRO GLASS DARK STATE;          !- Outside Layer",
        "FenestrationSurface:Detailed,",
        "  FenestrationSurface,     !- Name",
        "  Window,                  !- Surface Type",
        "  WindowConstruction1,      !- Construction Name",
        "  Wall,                    !- Building Surface Name",
        "  ,                        !- Outside Boundary Condition Object",
        "  0.5000000,               !- View Factor to Ground",
        "  ,                        !- Frame and Divider Name",
        "  1.0,                     !- Multiplier",
        "  4,                       !- Number of Vertices",
        "  0.200000,0.000000,9.900000,  !- X,Y,Z ==> Vertex 1 {m}",
        "  0.200000,0.000000,0.1000000,  !- X,Y,Z ==> Vertex 2 {m}",
        "  9.900000,0.000000,0.1000000,  !- X,Y,Z ==> Vertex 3 {m}",
        "  9.900000,0.000000,9.900000;  !- X,Y,Z ==> Vertex 4 {m}",
        "BuildingSurface:Detailed,"
        "  Wall,                    !- Name",
        "  Wall,                    !- Surface Type",
        "  WallConstruction,        !- Construction Name",
        "  Zone,                    !- Zone Name",
        "  Outdoors,                !- Outside Boundary Condition",
        "  ,                        !- Outside Boundary Condition Object",
        "  SunExposed,              !- Sun Exposure",
        "  WindExposed,             !- Wind Exposure",
        "  0.5000000,               !- View Factor to Ground",
        "  4,                       !- Number of Vertices",
        "  0.000000,0.000000,10.00000,  !- X,Y,Z ==> Vertex 1 {m}",
        "  0.000000,0.000000,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "  10.00000,0.000000,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "  10.00000,0.000000,10.00000;  !- X,Y,Z ==> Vertex 4 {m}",
        "BuildingSurface:Detailed,"
        "  Floor,                   !- Name",
        "  Floor,                   !- Surface Type",
        "  WallConstruction,        !- Construction Name",
        "  Zone,                    !- Zone Name",
        "  Outdoors,                !- Outside Boundary Condition",
        "  ,                        !- Outside Boundary Condition Object",
        "  NoSun,                   !- Sun Exposure",
        "  NoWind,                  !- Wind Exposure",
        "  1.0,                     !- View Factor to Ground",
        "  4,                       !- Number of Vertices",
        "  0.000000,0.000000,0,  !- X,Y,Z ==> Vertex 1 {m}",
        "  0.000000,10.000000,0,  !- X,Y,Z ==> Vertex 2 {m}",
        "  10.00000,10.000000,0,  !- X,Y,Z ==> Vertex 3 {m}",
        "  10.00000,0.000000,0;  !- X,Y,Z ==> Vertex 4 {m}",
        "Zone,"
        "  Zone,                    !- Name",
        "  0,                       !- Direction of Relative North {deg}",
        "  6.000000,                !- X Origin {m}",
        "  6.000000,                !- Y Origin {m}",
        "  0,                       !- Z Origin {m}",
        "  1,                       !- Type",
        "  1,                       !- Multiplier",
        "  autocalculate,           !- Ceiling Height {m}",
        "  autocalculate;           !- Volume {m3}",
        "  Daylighting:Controls,",
        "    Daylighting Control,!- Name",
        "    Zone,          !- Zone Name",
        "    SplitFlux,               !- Daylighting Method",
        "    ,                        !- Availability Schedule Name",
        "    Continuous,              !- Lighting Control Type",
        "    0.3,                     !- Minimum Input Power Fraction for Continuous or ContinuousOff Dimming Control",
        "    0.2,                     !- Minimum Light Output Fraction for Continuous or ContinuousOff Dimming Control",
        "    1,                       !- Number of Stepped Control Steps",
        "    1,                       !- Probability Lighting will be Reset When Needed in Manual Stepped Control",
        "    ,                        !- Glare Calculation Daylighting Reference Point Name",
        "    ,                        !- Glare Calculation Azimuth Angle of View Direction Clockwise from Zone y-Axis {deg}",
        "    22,                      !- Maximum Allowable Discomfort Glare Index",
        "    ,                        !- DElight Gridding Resolution {m2}",
        "    Reference Point 1,  !- Daylighting Reference Point 1 Name",
        "    1,                       !- Fraction of Zone Controlled by Reference Point 1",
        "    500;                     !- Illuminance Setpoint at Reference Point 1 {lux}",
        "",
        "  Daylighting:ReferencePoint,",
        "    Reference Point 1,  !- Name",
        "    Zone,          !- Zone Name",
        "    12,                      !- X-Coordinate of Reference Point {m}",
        "    2.5,                     !- Y-Coordinate of Reference Point {m}",
        "    0.8;                     !- Z-Coordinate of Reference Point {m}",
        "  ShadowCalculation,",
        "    TimestepFrequency,       !- Calculation Method",
        "    30,                      !- Calculation Frequency",
        "    15000;                   !- Maximum Figures in Shadow Overlap Calculations",
        "EnergyManagementSystem:ConstructionIndexVariable, Win_1, WINDOWCONSTRUCTION1;",
        "EnergyManagementSystem:ConstructionIndexVariable, Win_2, WINDOWCONSTRUCTION2;",
        "  EnergyManagementSystem:Actuator,",
        "    Win1_Construct,          !- Name",
        "    FenestrationSurface,  !- Actuated Component Unique Name",
        "    Surface,                 !- Actuated Component Type",
        "    Construction State;      !- Actuated Component Control Type",
        "",
        "  EnergyManagementSystem:ProgramCallingManager,",
        "    Window Switcher,  !- Name",
        "    BeginTimestepBeforePredictor,  !- EnergyPlus Model Calling Point",
        "    ZN_1_wall_south_Window_1_Control;  !- Program Name 1",
        "",
        "  EnergyManagementSystem:Program,",
        "    ZN_1_wall_south_Window_1_Control,  !- Name",
        "    IF Hour > 12,    !- Program Line 1",
        "    Set Win1_Construct = Win_2,  !- Program Line 2",
        "    ELSE,                    !- <none>",
        "    SET Win1_Construct = Win_1,  !- <none>",
        "    ENDIF;                   !- <none>",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    OutputProcessor::TimeValue.allocate(2);
    SimulationManager::ManageSimulation();
    DataGlobals::DayOfSim = 2; // avoid array bounds problem in RecKeepHeatBalance
    WeatherManager::Envrn = 1;

    // Test 1 - Set time of day to morning - should use high transmittance window
    DataGlobals::TimeStep = 1;
    DataGlobals::HourOfDay = 11;
    DataGlobals::CurrentTime = 11.0;
    WeatherManager::SetCurrentWeather();
    HeatBalanceManager::ManageHeatBalance();
    // For now, must call this twice in order to hit the BeginTimeStepBeforePredictor EMS calling point
    HeatBalanceManager::ManageHeatBalance();
    // Find the fenestration surface
    int winSurfNum = UtilityRoutines::FindItemInList("FENESTRATIONSURFACE", DataSurfaces::Surface);
    int win1ConstNum = UtilityRoutines::FindItemInList("WINDOWCONSTRUCTION1", DataHeatBalance::Construct);
    EXPECT_EQ(DataSurfaces::Surface(winSurfNum).Construction, win1ConstNum);
    Real64 transSol = DataSurfaces::WinSysSolTransmittance(winSurfNum);
    EXPECT_GT(transSol, 0.8);
    Real64 refPtIllum = DataDaylighting::ZoneDaylight(1).DaylIllumAtRefPt(1);
    EXPECT_GT(refPtIllum, 3000.0);

    // Test 2 - Set time of day to afternoon - should use low transmittance window
    DataGlobals::TimeStep = 1;
    DataGlobals::HourOfDay = 14;
    DataGlobals::CurrentTime = 14.0;
    WeatherManager::SetCurrentWeather();
    HeatBalanceManager::ManageHeatBalance();
    // For now, must call this twice in order to hit the BeginTimeStepBeforePredictor EMS calling point
    HeatBalanceManager::ManageHeatBalance();
    int win2ConstNum = UtilityRoutines::FindItemInList("WINDOWCONSTRUCTION2", DataHeatBalance::Construct);
    EXPECT_EQ(DataSurfaces::Surface(winSurfNum).Construction, win2ConstNum);
    transSol = DataSurfaces::WinSysSolTransmittance(winSurfNum);
    EXPECT_LT(transSol, 0.2);
    refPtIllum = DataDaylighting::ZoneDaylight(1).DaylIllumAtRefPt(1);
    EXPECT_LT(refPtIllum, 1000.0);
}

} // namespace EnergyPlus
