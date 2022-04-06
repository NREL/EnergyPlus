// EnergyPlus, Copyright (c) 1996-2022, The Board of Trustees of the University of Illinois,
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

// C++ Headers
#include <algorithm>
#include <cmath>
#include <memory>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <AirflowNetwork/Elements.hpp>
#include <EnergyPlus/AirLoopHVACDOAS.hh>
#include <EnergyPlus/Autosizing/Base.hh>
#include <EnergyPlus/BranchInputManager.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataContaminantBalance.hh>
#include <EnergyPlus/DataConvergParams.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobalConstants.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataPrecisionGlobals.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataSystemVariables.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/DesiccantDehumidifiers.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/EvaporativeCoolers.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/Furnaces.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/GeneralRoutines.hh>
#include <EnergyPlus/HVACControllers.hh>
#include <EnergyPlus/HVACDXHeatPumpSystem.hh>
#include <EnergyPlus/HVACDuct.hh>
#include <EnergyPlus/HVACFan.hh>
#include <EnergyPlus/HVACHXAssistedCoolingCoil.hh>
#include <EnergyPlus/HVACInterfaceManager.hh>
#include <EnergyPlus/HVACMultiSpeedHeatPump.hh>
#include <EnergyPlus/HVACUnitaryBypassVAV.hh>
#include <EnergyPlus/HVACVariableRefrigerantFlow.hh>
#include <EnergyPlus/HeatRecovery.hh>
#include <EnergyPlus/HeatingCoils.hh>
#include <EnergyPlus/Humidifiers.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/MixedAir.hh>
#include <EnergyPlus/MixerComponent.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutAirNodeManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/OutputReportPredefined.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/SimAirServingZones.hh>
#include <EnergyPlus/SizingManager.hh>
#include <EnergyPlus/SplitterComponent.hh>
#include <EnergyPlus/SteamCoils.hh>
#include <EnergyPlus/SystemAvailabilityManager.hh>
#include <EnergyPlus/UnitarySystem.hh>
#include <EnergyPlus/UserDefinedComponents.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WaterCoils.hh>
#include <EnergyPlus/ZonePlenum.hh>

namespace EnergyPlus::SimAirServingZones {

// MODULE INFORMATION
//       AUTHOR:  Russ Taylor, Dan Fisher, Fred Buhl
//       DATE WRITTEN:  Oct 1997
//       MODIFIED:  Dec 1997 Fred Buhl; Richard Liesen  Apr 1998,
//                  Dec 1999 Fred Buhl
//                  22Aug2010 Craig Wray - added Fan:ComponentModel
//       RE-ENGINEERED:  This is new code, not reengineered

// PURPOSE OF THIS MODULE:
// Contains the data and code for simulating the HVAC forced
// air systems.

// METHODOLOGY EMPLOYED:
// Successive iteration forward from the return air inlet
// to the supply air outlets.

using namespace DataLoopNode;
using namespace DataAirLoop;
using namespace DataHVACGlobals;
using namespace DataSizing;
using namespace DataZoneEquipment;
using namespace DataAirSystems;

void ManageAirLoops(EnergyPlusData &state,
                    bool const FirstHVACIteration, // TRUE if first full HVAC iteration in an HVAC timestep
                    bool &SimAir,                  // TRUE means air loops must be (re)simulated
                    bool &SimZoneEquipment         // TRUE means zone equipment must be (re) simulated
)
{

    // SUBROUTINE INFORMATION
    //             AUTHOR:  Russ Taylor, Dan Fisher, Fred Buhl
    //       DATE WRITTEN:  Oct 1997
    //           MODIFIED:  Dec 1997 Fred Buhl
    //      RE-ENGINEERED:  This is new code, not reengineered

    // PURPOSE OF THIS SUBROUTINE:
    // This is the manager subroutine for the air loop simulation.
    // Called from SimSelectedEquipment, which is called from SimHVAC,
    // which is called from ManageHVAC, the top level system/plant driver.
    // The subroutine performs the usual manager functions: it calls the
    // Get, Init, Sim, Update, and Report routines.

    auto &AirLoopControlInfo(state.dataAirLoop->AirLoopControlInfo);

    using MixedAir::ManageOutsideAirSystem;

    if (state.dataSimAirServingZones->GetAirLoopInputFlag) { // First time subroutine has been entered
        GetAirPathData(state);                               // Get air loop descriptions from input file
        state.dataSimAirServingZones->GetAirLoopInputFlag = false;
    }

    // Initialize air loop related parameters
    InitAirLoops(state, FirstHVACIteration);

    // Call the AirLoop Simulation
    if (state.dataGlobal->SysSizingCalc) {
        SizeAirLoops(state);
    } else if (!state.dataGlobal->SysSizingCalc) {
        SimAirLoops(state, FirstHVACIteration, SimZoneEquipment);
    }

    // This flag could be used to resimulate only the air loops that needed additional iterations.
    // This flag would have to be moved inside SimAirLoops to gain this flexibility.
    SimAir = std::any_of(
        AirLoopControlInfo.begin(), AirLoopControlInfo.end(), [](DataAirLoop::AirLoopControlData const &e) { return e.ResimAirLoopFlag; });
}

// Get Input Section of the Module
//******************************************************************************

void GetAirPathData(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION
    //             AUTHOR:  Fred Buhl
    //       DATE WRITTEN:  Jan 1998
    //           MODIFIED: Richard Liesen April 1998, Fred Buhl Dec 1999
    //      RE-ENGINEERED:  This is new code, not reengineered

    // PURPOSE OF THIS SUBROUTINE:
    // Input all the data needed to simulate the air loops in the problem.

    // METHODOLOGY EMPLOYED:
    // Use the various "Get" routines from the InputProcessor module to
    // obtain input data and store it in the data structures defined in MODULE SimAirServingZones

    // REFERENCES: This gets the following object:
    // AirLoopHVAC,
    //        \min-fields 10
    //        \memo Defines a central forced air system
    //    A1, \field Name
    //        \required-field
    //        \type alpha
    //        \reference AirPrimaryLoops
    //    A2, \field Controller List Name
    //        \note Enter the name of an AirLoopHVAC:ControllerList object.
    //        \type object-list
    //        \object-list ControllerLists
    //    A3, \field Availability Manager List Name
    //        \note Enter the name of an AvailabilityManagerAssignmentList object.
    //        \type object-list
    //        \object-list SystemAvailabilityManagerLists
    //    N1, \field Design Primary Air Flow Rate
    //        \default 0
    //        \units m3/s
    //        \autosizable
    //    A4, \field BranchList Name
    //        \note Name of a BranchList containing all the branches in this air loop
    //        \required-field
    //        \type object-list
    //        \object-list BranchLists
    //    A5, \field ConnectorList Name
    //        \note Name of a Connector List containing all the splitters and mixers in the loop
    //        \type object-list
    //        \object-list ConnectorLists
    //    A6, \field Supply Side Inlet Node Name
    //        \note Name of inlet node where return air enters the supply side of the air loop
    //        \required-field
    //    A7, \field Demand Side Outlet Node Name
    //        \note Name of outlet node where return air leaves the demand side and enters the supply side.
    //        \required-field
    //    A8, \field Demand Side Inlet Node Names
    //        \note Name of a Node or NodeList containing the inlet node(s) supplying air to zone equipment.
    //        \required-field
    //    A9; \field Supply Side Outlet Node Names
    //        \note Name of a Node or NodeList containing the outlet node(s) supplying air to the demand side.
    //        \required-field

    // Using/Aliasing
    using BranchInputManager::GetBranchData;
    using BranchInputManager::GetBranchList;
    using BranchInputManager::GetLoopMixer;
    using BranchInputManager::GetLoopSplitter;
    using BranchInputManager::GetNumSplitterMixerInConntrList;
    using BranchInputManager::NumBranchesInBranchList;
    using BranchInputManager::NumCompsInBranch;
    using HVACControllers::CheckCoilWaterInletNode;
    using HVACControllers::GetControllerActuatorNodeNum;
    using MixedAir::FindOAMixerMatchForOASystem;
    using MixedAir::GetNumOASystems;
    using MixedAir::GetOACompListNumber;
    using MixedAir::GetOACompName;
    using MixedAir::GetOACompType;
    using MixedAir::GetOACompTypeNum;
    using MixedAir::GetOAMixerInletNodeNumber;
    using MixedAir::GetOASysControllerListIndex;
    using MixedAir::GetOASysNumCoolingCoils;
    using MixedAir::GetOASysNumHeatingCoils;
    using MixedAir::GetOASysNumHXs;
    using MixedAir::GetOASysNumSimpControllers;
    using MixedAir::GetOASystemNumber;
    using NodeInputManager::GetNodeNums;
    using NodeInputManager::GetOnlySingleNode;
    using SystemAvailabilityManager::GetAirLoopAvailabilityManager;
    using WaterCoils::GetCoilWaterInletNode;

    // SUBROUTINE PARAMETER DEFINITIONS:
    constexpr const char *RoutineName("GetAirPathData: ");

    auto &PrimaryAirSystems(state.dataAirSystemsData->PrimaryAirSystems);
    auto &OutsideAirSys(state.dataAirLoop->OutsideAirSys);
    auto &AirToZoneNodeInfo(state.dataAirLoop->AirToZoneNodeInfo);
    auto &AirLoopControlInfo(state.dataAirLoop->AirLoopControlInfo);

    // SUBROUTINE LOCAL VARIABLE DEFINITIONS
    int NumNumbers;                // number of numbers returned by GetObjectItem
    Array1D<Real64> Numbers;       // numbers (REAL(r64)s) returned by GetObjectItem
    Array1D_string cNumericFields; // Numeric field names
    Array1D_bool lNumericBlanks;   // Logical array, numeric field input BLANK = .TRUE.
    int NumAlphas;                 // number of strings returned by GetObjectItem
    int NumParams;
    int MaxNumbers;
    int MaxAlphas;
    Array1D_string Alphas;           // alpha strings returned by GetObjectItem
    Array1D_string cAlphaFields;     // Alpha field names
    Array1D_bool lAlphaBlanks;       // Logical array, alpha field input BLANK = .TRUE.
    std::string CurrentModuleObject; // Object type for getting and error messages
    int NumNodes;                    // number of nodes returned by GetNodeNums
    Array1D_int NodeNums;            // node numbers returned by GetNodeNums
    int NodeNum;                     // a node number
    int AirSysNum;                   // an air system (air loop) number
    int OANum;                       // outside air system index
    int NumInList;
    int OAMixNum;                     // outside air mixer index
    int IOStat;                       // status number returned by GetObjectItem
    int NumControllers;               // number of controllers
    int ControllerListNum;            // Controller List index
    int ControllerNum;                // Controller index
    int I;                            // do loop index
    int BranchNum;                    // branch index
    int CompNum;                      // component index
    int NumCompsOnBranch;             // Number of components on a branch
    int OutBranchNum;                 // outlet branch index
    int InBranchNum;                  // inlet branch index
    std::string ControllerName;       // controller name
    std::string ControllerType;       // controller type
    std::string BranchListName;       // Name of a Branch List object
    std::string ControllerListName;   // Name of a controller list object
    std::string AvailManagerListName; // Name of an availability manager list object
    std::string ConnectorListName;    // Name of a connector list object
    Array1D_string BranchNames;       // Branch names from GetBranchList call
    Array1D_string CompTypes;         // Component types from GetBranchList call
    Array1D_string CompNames;         // Component names from GetBranchList call
    Array1D_string InletNodeNames;    // Component inlet node names from GetBranchData call
    Array1D_string OutletNodeNames;   // Component outlet node names from GetBranchData call
    Array1D_string NodeNames;         // Outlet node names from GetLoopSplitter call
    Array1D_int NodeNumbers;          // Outlet node numbers from GetLoopSplitter call
    Array1D_int InletNodeNumbers;     // Component inlet node numbers from GetBranchData call
    Array1D_int OutletNodeNumbers;    // Component outlet node numbers from GetBranchData call
    DataBranchAirLoopPlant::PressureCurveType PressCurveType;
    int PressCurveIndex;
    bool ErrorsFound(false); // TRUE if errors detected in input
    Array1D_bool PackagedUnit;
    int test;
    int count;
    bool ErrInList;
    int ConListNum(0);          // index of a Connector List object in the input
    bool SplitterExists(false); // TRUE if there is a slitter in a primary air system
    bool MixerExists(false);    // TRUE if there is a mixer in a primary air system
    bool errFlag;
    bool IsNotOK;
    /////////// hoisted into namespace
    // static int TestUniqueNodesNum( 0 );
    ///////////////////////////
    int NumOASysSimpControllers; // number of simple controllers in the OA Sys of an air primary system
    int NumOASysControllers;     // total number of controllers in the OA Sys
    int OASysContListNum;        // index of the controller list of the OA Sys
    int OASysControllerNum;      // index of OA Sys simple controller in the air primary system controller lists
    bool NodeNotFound;           // true if matching actuator node not found
    CompType CompType_Num;       // numeric equivalent for component type
    std::string CompType;        // component type
    int WaterCoilNodeNum;        // numeric equivalent for water coil node number
    int ActuatorNodeNum;         // numeric equivalent for controller actuator node number
    Array1D_string MatchNodeName(3);

    struct AirUniqueNodes
    {
        // Members
        std::string NodeName;
        std::string AirLoopName;
        std::string FieldName;
        bool NodeNameUsed;

        // Default Constructor
        AirUniqueNodes() : NodeNameUsed(false)
        {
        }
    };

    // Object Data
    Array1D<AirUniqueNodes> TestUniqueNodes;

    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, "AirLoopHVAC", NumParams, MaxAlphas, MaxNumbers);
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, "ConnectorList", NumParams, NumAlphas, NumNumbers);
    MaxAlphas = max(MaxAlphas, NumAlphas);
    MaxNumbers = max(MaxNumbers, NumNumbers);
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, "AirLoopHVAC:ControllerList", NumParams, NumAlphas, NumNumbers);
    MaxAlphas = max(MaxAlphas, NumAlphas);
    MaxNumbers = max(MaxNumbers, NumNumbers);

    Numbers.allocate(MaxNumbers);
    cNumericFields.allocate(MaxNumbers);
    lNumericBlanks.allocate(MaxNumbers);
    Alphas.allocate(MaxAlphas);
    cAlphaFields.allocate(MaxAlphas);
    lAlphaBlanks.allocate(MaxAlphas);

    // Initialize some local arrays
    Numbers = 0.0;
    cNumericFields = "";
    lNumericBlanks = true;
    Alphas = "";
    cAlphaFields = "";
    lAlphaBlanks = true;

    state.dataSimAirServingZones->NumOfTimeStepInDay = state.dataGlobal->NumOfTimeStepInHour * 24;

    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, "NodeList", NumParams, NumAlphas, NumNumbers);
    NodeNums.dimension(NumParams, 0);

    auto &NumPrimaryAirSys = state.dataHVACGlobal->NumPrimaryAirSys;

    // Find number of primary air systems
    NumPrimaryAirSys = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "AirLoopHVAC");
    TestUniqueNodes.allocate(NumPrimaryAirSys * 4); // used to look at specific nodes that must be unique, fields A6-A9

    PrimaryAirSystems.allocate(NumPrimaryAirSys);                  // allocate the primary air sys data array
    AirToZoneNodeInfo.allocate(NumPrimaryAirSys);                  // allocate the array that stores the air sys / zone equp connection data
    state.dataAirLoop->AirLoopZoneInfo.allocate(NumPrimaryAirSys); // allocate array that has cleaner list of zones attached to air loop
    state.dataAirLoop->AirToOANodeInfo.allocate(NumPrimaryAirSys); // allocate the array that stores the OA node connections (reporting)
    PackagedUnit.allocate(NumPrimaryAirSys);
    AirLoopControlInfo.allocate(NumPrimaryAirSys);
    state.dataAirLoop->AirLoopFlow.allocate(NumPrimaryAirSys);
    state.dataConvergeParams->AirLoopConvergence.allocate(NumPrimaryAirSys);
    state.dataSize->UnitarySysEqSizing.allocate(NumPrimaryAirSys);
    if (state.dataAirflowNetwork->SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlMultiADS ||
        state.dataAirflowNetwork->SimulateAirflowNetwork == AirflowNetwork::AirflowNetworkControlSimpleADS) {
        state.dataAirLoop->AirLoopAFNInfo.allocate(NumPrimaryAirSys);
    }

    state.dataHVACGlobal->GetAirPathDataDone = true; // used by UnitarySystem::getUnitarySystemInputData to determine if airloops are setup yet
    if (NumPrimaryAirSys <= 0) {
        TestUniqueNodes.deallocate();
        NodeNums.deallocate();
        return;
    }

    // Loop through the primary air systems and obtain the data for each system
    for (AirSysNum = 1; AirSysNum <= NumPrimaryAirSys; ++AirSysNum) {
        NumOASysControllers = 0;
        NumOASysSimpControllers = 0;
        OASysContListNum = 0;
        PackagedUnit(AirSysNum) = false;
        PrimaryAirSystems(AirSysNum).OASysExists = false; // init Outside Air system connection data to none
        PrimaryAirSystems(AirSysNum).isAllOA = false;
        PrimaryAirSystems(AirSysNum).OASysInletNodeNum = 0;
        PrimaryAirSystems(AirSysNum).OASysOutletNodeNum = 0;
        PrimaryAirSystems(AirSysNum).NumOAHeatCoils = 0;
        PrimaryAirSystems(AirSysNum).NumOACoolCoils = 0;
        AirLoopControlInfo(AirSysNum).FanOpMode = DataHVACGlobals::ContFanCycCoil; // initialize to constant fan mode for all air loops
        state.dataAirLoop->AirLoopFlow(AirSysNum).FanPLR = 1.0;                    // initialize to 1 for all air loops

        CurrentModuleObject = "AirLoopHVAC";

        state.dataInputProcessing->inputProcessor->getObjectItem(state,
                                                                 CurrentModuleObject,
                                                                 AirSysNum,
                                                                 Alphas,
                                                                 NumAlphas,
                                                                 Numbers,
                                                                 NumNumbers,
                                                                 IOStat,
                                                                 lNumericBlanks,
                                                                 lAlphaBlanks,
                                                                 cAlphaFields,
                                                                 cNumericFields); // get all the input data for the air system

        // Assign the air system data to the simulation variables.
        // Data needed to simulate the system goes into PrimaryAirSystem.
        // Data connecting the air system to the zone equipment goes into AirToZoneNodeInfo (in DataLoopNode).
        UtilityRoutines::IsNameEmpty(state, Alphas(1), CurrentModuleObject, ErrorsFound);
        PrimaryAirSystems(AirSysNum).Name = Alphas(1);
        AirToZoneNodeInfo(AirSysNum).AirLoopName = Alphas(1);
        if (NumAlphas < 9) {
            ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alphas(1) + "\", insufficient information.");
            ShowContinueError(state, "...Have supplied less than 9 alpha fields.");
            ErrorsFound = true;
            continue;
        }
        if (NumNumbers < 1) {
            ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alphas(1) + "\", insufficient information.");
            ShowContinueError(state, "...Have supplied less than 1 numeric field.");
            ErrorsFound = true;
            continue;
        }
        PrimaryAirSystems(AirSysNum).DesignVolFlowRate = Numbers(1);
        if (!lNumericBlanks(2)) {
            PrimaryAirSystems(AirSysNum).DesignReturnFlowFraction = Numbers(2);
        }
        // Only allow one return air node (at the loop level)
        AirToZoneNodeInfo(AirSysNum).NumReturnNodes = 1;
        // Allocate the return air node arrays
        AirToZoneNodeInfo(AirSysNum).AirLoopReturnNodeNum.allocate(AirToZoneNodeInfo(AirSysNum).NumReturnNodes);
        AirToZoneNodeInfo(AirSysNum).ZoneEquipReturnNodeNum.allocate(AirToZoneNodeInfo(AirSysNum).NumReturnNodes);
        // fill the return air node arrays with node numbers
        AirToZoneNodeInfo(AirSysNum).AirLoopReturnNodeNum(1) = GetOnlySingleNode(state,
                                                                                 Alphas(6),
                                                                                 ErrorsFound,
                                                                                 DataLoopNode::ConnectionObjectType::AirLoopHVAC,
                                                                                 Alphas(1),
                                                                                 DataLoopNode::NodeFluidType::Air,
                                                                                 DataLoopNode::ConnectionType::Inlet,
                                                                                 NodeInputManager::CompFluidStream::Primary,
                                                                                 ObjectIsParent);
        if (!lAlphaBlanks(7)) {
            AirToZoneNodeInfo(AirSysNum).ZoneEquipReturnNodeNum(1) = GetOnlySingleNode(state,
                                                                                       Alphas(7),
                                                                                       ErrorsFound,
                                                                                       DataLoopNode::ConnectionObjectType::AirLoopHVAC,
                                                                                       Alphas(1),
                                                                                       DataLoopNode::NodeFluidType::Air,
                                                                                       DataLoopNode::ConnectionType::Outlet,
                                                                                       NodeInputManager::CompFluidStream::Primary,
                                                                                       ObjectIsParent);
        } else {
            // If no return path, set this to zero to trigger special handling when calling UpdateHVACInterface
            AirToZoneNodeInfo(AirSysNum).ZoneEquipReturnNodeNum(1) = 0;
        }

        // work on unique nodes
        test =
            UtilityRoutines::FindItemInList(Alphas(6), TestUniqueNodes, &AirUniqueNodes::NodeName, state.dataSimAirServingZones->TestUniqueNodesNum);
        if (test == 0) {
            ++state.dataSimAirServingZones->TestUniqueNodesNum;
            TestUniqueNodes(state.dataSimAirServingZones->TestUniqueNodesNum).NodeName = Alphas(6);
            TestUniqueNodes(state.dataSimAirServingZones->TestUniqueNodesNum).AirLoopName = Alphas(1);
            TestUniqueNodes(state.dataSimAirServingZones->TestUniqueNodesNum).FieldName = cAlphaFields(6);
            TestUniqueNodes(state.dataSimAirServingZones->TestUniqueNodesNum).NodeNameUsed = true;
        } else {
            ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alphas(1) + "\", duplicate node name.");
            ShowContinueError(state, "...used for " + cAlphaFields(6) + "=\"" + Alphas(6) + "\"");
            ShowContinueError(state,
                              "...first used in " + CurrentModuleObject + "=\"" + TestUniqueNodes(test).AirLoopName + "\" for " +
                                  TestUniqueNodes(test).FieldName);
            ErrorsFound = true;
        }
        if (!lAlphaBlanks(7)) {
            test = UtilityRoutines::FindItemInList(
                Alphas(7), TestUniqueNodes, &AirUniqueNodes::NodeName, state.dataSimAirServingZones->TestUniqueNodesNum);
            if (test == 0) {
                ++state.dataSimAirServingZones->TestUniqueNodesNum;
                TestUniqueNodes(state.dataSimAirServingZones->TestUniqueNodesNum).NodeName = Alphas(7);
                TestUniqueNodes(state.dataSimAirServingZones->TestUniqueNodesNum).AirLoopName = Alphas(1);
                TestUniqueNodes(state.dataSimAirServingZones->TestUniqueNodesNum).FieldName = cAlphaFields(7);
                TestUniqueNodes(state.dataSimAirServingZones->TestUniqueNodesNum).NodeNameUsed = true;
            } else {
                ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alphas(1) + "\", duplicate node name.");
                ShowContinueError(state, "...used for " + cAlphaFields(7) + "=\"" + Alphas(7) + "\"");
                ShowContinueError(state,
                                  "...first used in " + CurrentModuleObject + "=\"" + TestUniqueNodes(test).AirLoopName + "\" for " +
                                      TestUniqueNodes(test).FieldName);
                ErrorsFound = true;
            }
        }
        test =
            UtilityRoutines::FindItemInList(Alphas(8), TestUniqueNodes, &AirUniqueNodes::NodeName, state.dataSimAirServingZones->TestUniqueNodesNum);
        if (test == 0) {
            ++state.dataSimAirServingZones->TestUniqueNodesNum;
            TestUniqueNodes(state.dataSimAirServingZones->TestUniqueNodesNum).NodeName = Alphas(8);
            TestUniqueNodes(state.dataSimAirServingZones->TestUniqueNodesNum).AirLoopName = Alphas(1);
            TestUniqueNodes(state.dataSimAirServingZones->TestUniqueNodesNum).FieldName = cAlphaFields(8);
            TestUniqueNodes(state.dataSimAirServingZones->TestUniqueNodesNum).NodeNameUsed = true;
        } else {
            ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alphas(1) + "\", duplicate node name/list.");
            ShowContinueError(state, "...used for " + cAlphaFields(8) + "=\"" + Alphas(8) + "\"");
            ShowContinueError(state,
                              "...first used in " + CurrentModuleObject + "=\"" + TestUniqueNodes(test).AirLoopName + "\" for " +
                                  TestUniqueNodes(test).FieldName);
            ErrorsFound = true;
        }
        test =
            UtilityRoutines::FindItemInList(Alphas(9), TestUniqueNodes, &AirUniqueNodes::NodeName, state.dataSimAirServingZones->TestUniqueNodesNum);
        if (test == 0) {
            ++state.dataSimAirServingZones->TestUniqueNodesNum;
            TestUniqueNodes(state.dataSimAirServingZones->TestUniqueNodesNum).NodeName = Alphas(9);
            TestUniqueNodes(state.dataSimAirServingZones->TestUniqueNodesNum).AirLoopName = Alphas(1);
            TestUniqueNodes(state.dataSimAirServingZones->TestUniqueNodesNum).FieldName = cAlphaFields(9);
            TestUniqueNodes(state.dataSimAirServingZones->TestUniqueNodesNum).NodeNameUsed = true;
        } else {
            ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alphas(1) + "\", duplicate node name/list.");
            ShowContinueError(state, "...used for " + cAlphaFields(9) + "=\"" + Alphas(9) + "\"");
            ShowContinueError(state,
                              "...first used in " + CurrentModuleObject + "=\"" + TestUniqueNodes(test).AirLoopName + "\" for " +
                                  TestUniqueNodes(test).FieldName);
            ErrorsFound = true;
        }
        // this test depends on the controlled zone input having been "gotten"
        test = 0;
        for (count = 1; count <= state.dataZoneEquip->NumReturnAirPaths; ++count) {
            if (state.dataZoneEquip->ReturnAirPath(count).OutletNodeNum == AirToZoneNodeInfo(AirSysNum).ZoneEquipReturnNodeNum(1)) {
                test = state.dataZoneEquip->ReturnAirPath(count).OutletNodeNum;
                break;
            }
        }
        if ((test == 0) && (AirToZoneNodeInfo(AirSysNum).NumReturnNodes > 0)) { // there, see if it's in the controlled zone info
            for (count = 1; count <= state.dataGlobal->NumOfZones; ++count) {
                for (int retNode = 1; retNode <= state.dataZoneEquip->ZoneEquipConfig(count).NumReturnNodes; ++retNode) {
                    if (state.dataZoneEquip->ZoneEquipConfig(count).ReturnNode(retNode) != AirToZoneNodeInfo(AirSysNum).ZoneEquipReturnNodeNum(1))
                        continue;
                    test = count;
                    break;
                }
                if (test == count) break;
            }
        }
        if ((test == 0) && (AirToZoneNodeInfo(AirSysNum).NumReturnNodes > 0) && !lAlphaBlanks(7)) {
            ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alphas(1) + "\", invalid.");
            ShowContinueError(state, cAlphaFields(7) + " (Return Air Path or ZoneHVAC:EquipmentConnections) not valid = \"" + Alphas(7) + "\".");
            ErrorsFound = true;
        }
        // Get the supply nodes
        ErrInList = false;
        GetNodeNums(state,
                    Alphas(8),
                    NumNodes,
                    NodeNums,
                    ErrInList,
                    DataLoopNode::NodeFluidType::Air,
                    DataLoopNode::ConnectionObjectType::AirLoopHVAC,
                    PrimaryAirSystems(AirSysNum).Name,
                    DataLoopNode::ConnectionType::Inlet,
                    NodeInputManager::CompFluidStream::Primary,
                    ObjectIsParent,
                    _,
                    cAlphaFields(8));
        if (ErrInList) {
            ErrorsFound = true;
        }
        // Allow at most 3 supply nodes (for a 3 deck system)
        if (NumNodes > 3) {
            ShowSevereError(state,
                            std::string{RoutineName} + CurrentModuleObject + "=\"" + PrimaryAirSystems(AirSysNum).Name + "\", too many nodes.");
            ShowContinueError(state, "Only 1st 3 Nodes will be used from " + cAlphaFields(8) + "=\"" + Alphas(8) + "\".");
            ErrorsFound = true;
        }
        if (NumNodes == 0) {
            ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + PrimaryAirSystems(AirSysNum).Name + "\", too few nodes.");
            ShowContinueError(state, "There must be at least 1 supply node in the system.");
            ErrorsFound = true;
        }
        AirToZoneNodeInfo(AirSysNum).NumSupplyNodes = NumNodes;
        // Allocate the supply node arrays in AirToZoneNodeInfo
        AirToZoneNodeInfo(AirSysNum).ZoneEquipSupplyNodeNum.allocate(AirToZoneNodeInfo(AirSysNum).NumSupplyNodes);
        AirToZoneNodeInfo(AirSysNum).AirLoopSupplyNodeNum.allocate(AirToZoneNodeInfo(AirSysNum).NumSupplyNodes);
        AirToZoneNodeInfo(AirSysNum).SupplyDuctType.allocate(AirToZoneNodeInfo(AirSysNum).NumSupplyNodes);
        // Fill the supply node arrays with node numbers
        for (I = 1; I <= AirToZoneNodeInfo(AirSysNum).NumSupplyNodes; ++I) {
            AirToZoneNodeInfo(AirSysNum).ZoneEquipSupplyNodeNum(I) = NodeNums(I);
            AirToZoneNodeInfo(AirSysNum).SupplyDuctType(I) = 0;
        }
        ErrInList = false;
        GetNodeNums(state,
                    Alphas(9),
                    NumNodes,
                    NodeNums,
                    ErrInList,
                    DataLoopNode::NodeFluidType::Air,
                    DataLoopNode::ConnectionObjectType::AirLoopHVAC,
                    PrimaryAirSystems(AirSysNum).Name,
                    DataLoopNode::ConnectionType::Outlet,
                    NodeInputManager::CompFluidStream::Primary,
                    ObjectIsParent,
                    _,
                    cAlphaFields(9));
        if (ErrInList) {
            ErrorsFound = true;
        }
        if (NumNodes != AirToZoneNodeInfo(AirSysNum).NumSupplyNodes) {
            ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + "=\"" + Alphas(1) + "\", node mismatch.");
            ShowContinueError(state,
                              format("...number of air system exit nodes [{}] must match number of zone equip inlet nodes [{}].",
                                     NumNodes,
                                     AirToZoneNodeInfo(AirSysNum).NumSupplyNodes));
            ErrorsFound = true;
        }
        for (I = 1; I <= AirToZoneNodeInfo(AirSysNum).NumSupplyNodes; ++I) {
            AirToZoneNodeInfo(AirSysNum).AirLoopSupplyNodeNum(I) = NodeNums(I);
        }
        AirToZoneNodeInfo(AirSysNum).NumZonesCooled = 0;
        AirToZoneNodeInfo(AirSysNum).NumZonesHeated = 0;
        // Branch, Controller, Availability Manager and Connector List Names to access later
        ControllerListName = Alphas(2);
        BranchListName = Alphas(4);
        AvailManagerListName = Alphas(3);
        ConnectorListName = Alphas(5);
        PrimaryAirSystems(AirSysNum).NumBranches = NumBranchesInBranchList(state, BranchListName);
        if (PrimaryAirSystems(AirSysNum).NumBranches == 0) {
            ShowSevereError(
                state, std::string{RoutineName} + CurrentModuleObject + "=\"" + PrimaryAirSystems(AirSysNum).Name + "\", insufficient information.");
            ShowContinueError(state, "...there must be at least 1 branch specified.");
            ErrorsFound = true;
        }
        BranchNames.allocate(PrimaryAirSystems(AirSysNum).NumBranches);
        BranchNames = "";
        // get the branch lists
        GetBranchList(state, PrimaryAirSystems(AirSysNum).Name, BranchListName, PrimaryAirSystems(AirSysNum).NumBranches, BranchNames, "Air");
        PrimaryAirSystems(AirSysNum).Branch.allocate(PrimaryAirSystems(AirSysNum).NumBranches);
        // Cycle through all of the branches and set up the branch data
        for (BranchNum = 1; BranchNum <= PrimaryAirSystems(AirSysNum).NumBranches; ++BranchNum) {
            PrimaryAirSystems(AirSysNum).Branch(BranchNum).Name = BranchNames(BranchNum);
            NumCompsOnBranch = NumCompsInBranch(state, BranchNames(BranchNum));
            if (NumCompsOnBranch <= 0) {
                ShowSevereError(state,
                                std::string{RoutineName} + CurrentModuleObject + "=\"" + PrimaryAirSystems(AirSysNum).Name +
                                    "\", insufficient information.");
                ShowContinueError(state, "...Branch=\"" + BranchNames(BranchNum) + "\", no components on branch.");
                ErrorsFound = true;
                continue;
            }
            CompTypes.allocate(NumCompsOnBranch);
            CompNames.allocate(NumCompsOnBranch);
            InletNodeNames.allocate(NumCompsOnBranch);
            InletNodeNumbers.dimension(NumCompsOnBranch, 0);
            OutletNodeNames.allocate(NumCompsOnBranch);
            OutletNodeNumbers.dimension(NumCompsOnBranch, 0);

            GetBranchData(state,
                          PrimaryAirSystems(AirSysNum).Name,
                          BranchNames(BranchNum),
                          PressCurveType,
                          PressCurveIndex,
                          NumCompsOnBranch,
                          CompTypes,
                          CompNames,
                          InletNodeNames,
                          InletNodeNumbers,
                          OutletNodeNames,
                          OutletNodeNumbers,
                          ErrorsFound); // Placeholders for plant branch pressure data (not used in air loops)
            PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp.allocate(NumCompsOnBranch);
            PrimaryAirSystems(AirSysNum).Branch(BranchNum).TotalComponents = NumCompsOnBranch;

            PrimaryAirSystems(AirSysNum).Branch(BranchNum).TotalNodes = NumCompsOnBranch + 1;
            PrimaryAirSystems(AirSysNum).Branch(BranchNum).NodeNum.allocate(NumCompsOnBranch + 1);
            PrimaryAirSystems(AirSysNum).Branch(BranchNum).NodeNum(1) = InletNodeNumbers(1);
            PrimaryAirSystems(AirSysNum).Branch(BranchNum).DuctType = Main;

            // If first node is an outdoor air node, then consider this to have a simple OA system (many places check for this)
            if (OutAirNodeManager::CheckOutAirNodeNumber(state, InletNodeNumbers(1))) {
                PrimaryAirSystems(AirSysNum).OASysExists = true;
                PrimaryAirSystems(AirSysNum).isAllOA = true;
                PrimaryAirSystems(AirSysNum).OASysInletNodeNum = InletNodeNumbers(1);
                PrimaryAirSystems(AirSysNum).OASysOutletNodeNum = InletNodeNumbers(1);
                PrimaryAirSystems(AirSysNum).OAMixOAInNodeNum = InletNodeNumbers(1);
                state.dataAirLoop->AirToOANodeInfo(AirSysNum).OASysExists = true;
                state.dataAirLoop->AirToOANodeInfo(AirSysNum).OASysInletNodeNum = InletNodeNumbers(1);
                state.dataAirLoop->AirToOANodeInfo(AirSysNum).OASysOutletNodeNum = InletNodeNumbers(1);
            }
            for (CompNum = 1; CompNum <= PrimaryAirSystems(AirSysNum).Branch(BranchNum).TotalComponents; ++CompNum) {

                PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).TypeOf = CompTypes(CompNum);
                PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).Name = CompNames(CompNum);
                PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).CompIndex = 0;
                PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).NodeNameIn = InletNodeNames(CompNum);
                PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).NodeNumIn = InletNodeNumbers(CompNum);
                PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).NodeNameOut = OutletNodeNames(CompNum);
                PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).NodeNumOut = OutletNodeNumbers(CompNum);
                PrimaryAirSystems(AirSysNum).Branch(BranchNum).NodeNum(CompNum + 1) = OutletNodeNumbers(CompNum);

                // Check for Outside Air system; if there, store its connection node numbers to primary air system
                if (UtilityRoutines::SameString(CompTypes(CompNum), "AirLoopHVAC:OutdoorAirSystem")) {
                    if (PrimaryAirSystems(AirSysNum).OASysExists) {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" + PrimaryAirSystems(AirSysNum).Name +
                                            "\", too many outdoor air systems.");
                        ShowContinueError(state, "Only one AirLoopHVAC:OutdoorAirSystem allowed.");
                        ErrorsFound = true;
                        continue;
                    }
                    PrimaryAirSystems(AirSysNum).OASysExists = true;
                    PrimaryAirSystems(AirSysNum).OASysInletNodeNum = InletNodeNumbers(CompNum);
                    PrimaryAirSystems(AirSysNum).OASysOutletNodeNum = OutletNodeNumbers(CompNum);
                    state.dataAirLoop->AirToOANodeInfo(AirSysNum).OASysExists = true;
                    state.dataAirLoop->AirToOANodeInfo(AirSysNum).OASysInletNodeNum = InletNodeNumbers(CompNum);
                    state.dataAirLoop->AirToOANodeInfo(AirSysNum).OASysOutletNodeNum = OutletNodeNumbers(CompNum);
                    OANum = GetOASystemNumber(state, CompNames(CompNum));
                    if (OANum > 0) {
                        NumOASysSimpControllers = GetOASysNumSimpControllers(state, OANum);
                        PrimaryAirSystems(AirSysNum).NumOAHeatCoils = GetOASysNumHeatingCoils(state, OANum);
                        PrimaryAirSystems(AirSysNum).NumOACoolCoils = GetOASysNumCoolingCoils(state, OANum);
                        PrimaryAirSystems(AirSysNum).NumOAHXs = GetOASysNumHXs(state, OANum);
                        OASysContListNum = GetOASysControllerListIndex(state, OANum);
                        OAMixNum = FindOAMixerMatchForOASystem(state, OANum);
                        if (OAMixNum > 0) {
                            PrimaryAirSystems(AirSysNum).OAMixOAInNodeNum = GetOAMixerInletNodeNumber(state, OAMixNum);
                        } else {
                            ShowSevereError(state,
                                            std::string{RoutineName} + CurrentModuleObject + "=\"" + PrimaryAirSystems(AirSysNum).Name +
                                                "\", item not found.");
                            ShowContinueError(state, "OutdoorAir:Mixer for AirLoopHVAC:OutdoorAirSystem=\"" + CompNames(CompNum) + "\" not found.");
                            ErrorsFound = true;
                        }
                    } else {
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" + PrimaryAirSystems(AirSysNum).Name +
                                            "\", item not found.");
                        ShowContinueError(state, "AirLoopHVAC:OutdoorAirSystem=\"" + CompNames(CompNum) + "\" not found.");
                        ShowContinueError(state, "  referenced in Branch=\"" + PrimaryAirSystems(AirSysNum).Branch(BranchNum).Name + "\".");
                        ErrorsFound = true;
                    }
                }
                {
                    auto const componentType(uppercased(CompTypes(CompNum)));
                    if (componentType == "COILSYSTEM:COOLING:DX") {
                        PackagedUnit(AirSysNum) = true;
                    } else if (componentType == "COILSYSTEM:HEATING:DX") {
                        PackagedUnit(AirSysNum) = true;
                    } else if (componentType == "COILSYSTEM:COOLING:WATER") {
                        PackagedUnit(AirSysNum) = true;
                    } else if (componentType == "AIRLOOPHVAC:UNITARYSYSTEM") {
                        PackagedUnit(AirSysNum) = true;
                    } else if (componentType == "AIRLOOPHVAC:UNITARY:FURNACE:HEATONLY") {
                        PackagedUnit(AirSysNum) = true;
                    } else if (componentType == "AIRLOOPHVAC:UNITARY:FURNACE:HEATCOOL") {
                        PackagedUnit(AirSysNum) = true;
                    } else if (componentType == "AIRLOOPHVAC:UNITARYHEATONLY") {
                        PackagedUnit(AirSysNum) = true;
                    } else if (componentType == "AIRLOOPHVAC:UNITARYHEATCOOL") {
                        PackagedUnit(AirSysNum) = true;
                    } else if (componentType == "AIRLOOPHVAC:UNITARYHEATPUMP:AIRTOAIR") {
                        PackagedUnit(AirSysNum) = true;
                    } else if (componentType == "AIRLOOPHVAC:UNITARYHEATPUMP:WATERTOAIR") {
                        PackagedUnit(AirSysNum) = true;
                    } else if (componentType == "AIRLOOPHVAC:UNITARYHEATCOOL:VAVCHANGEOVERBYPASS") {
                        PackagedUnit(AirSysNum) = true;
                    } else if (componentType == "AIRLOOPHVAC:UNITARYHEATPUMP:AIRTOAIR:MULTISPEED") {
                        PackagedUnit(AirSysNum) = true;
                    }
                }

            } // end of component loop

            PrimaryAirSystems(AirSysNum).Branch(BranchNum).ControlType = "";
            PrimaryAirSystems(AirSysNum).Branch(BranchNum).NodeNumIn = InletNodeNumbers(1);
            PrimaryAirSystems(AirSysNum).Branch(BranchNum).NodeNumOut = OutletNodeNumbers(NumCompsOnBranch);

            CompTypes.deallocate();
            CompNames.deallocate();
            InletNodeNames.deallocate();
            InletNodeNumbers.deallocate();
            OutletNodeNames.deallocate();
            OutletNodeNumbers.deallocate();

        } // end of branch loop

        BranchNames.deallocate();

        // find and store the primary air system outlet branch reference numbers
        PrimaryAirSystems(AirSysNum).NumOutletBranches = AirToZoneNodeInfo(AirSysNum).NumSupplyNodes;
        for (OutBranchNum = 1; OutBranchNum <= 3; ++OutBranchNum) {
            PrimaryAirSystems(AirSysNum).OutletBranchNum(OutBranchNum) = 0;
            if (OutBranchNum > PrimaryAirSystems(AirSysNum).NumOutletBranches) break;
            MatchNodeName(OutBranchNum) = state.dataLoopNodes->NodeID(AirToZoneNodeInfo(AirSysNum).AirLoopSupplyNodeNum(OutBranchNum));
            for (BranchNum = 1; BranchNum <= PrimaryAirSystems(AirSysNum).NumBranches; ++BranchNum) {
                if (AirToZoneNodeInfo(AirSysNum).AirLoopSupplyNodeNum(OutBranchNum) == PrimaryAirSystems(AirSysNum).Branch(BranchNum).NodeNumOut) {
                    PrimaryAirSystems(AirSysNum).OutletBranchNum(OutBranchNum) = BranchNum;
                }
            }
        }
        //  Check for errors
        for (OutBranchNum = 1; OutBranchNum <= PrimaryAirSystems(AirSysNum).NumOutletBranches; ++OutBranchNum) {
            if (PrimaryAirSystems(AirSysNum).OutletBranchNum(OutBranchNum) != 0) continue;
            ShowSevereError(state,
                            std::string{RoutineName} + CurrentModuleObject + "=\"" + PrimaryAirSystems(AirSysNum).Name + "\", branch in error.");
            ShowContinueError(state, "Probable missing or misspelled node referenced in the branch(es):");
            for (BranchNum = 1; BranchNum <= PrimaryAirSystems(AirSysNum).NumBranches; ++BranchNum) {
                ShowContinueError(state, "Possible Error in Branch Object=\"" + PrimaryAirSystems(AirSysNum).Branch(BranchNum).Name + "\".");
            }
            ShowContinueError(state, "...looking to match to Node=\"" + MatchNodeName(OutBranchNum) + "\".");
            ErrorsFound = true;
        }

        // find and store the primary air system inlet branch numbers
        PrimaryAirSystems(AirSysNum).NumInletBranches = AirToZoneNodeInfo(AirSysNum).NumReturnNodes;
        for (InBranchNum = 1; InBranchNum <= PrimaryAirSystems(AirSysNum).NumInletBranches; ++InBranchNum) {
            PrimaryAirSystems(AirSysNum).InletBranchNum(InBranchNum) = 0;
            for (BranchNum = 1; BranchNum <= PrimaryAirSystems(AirSysNum).NumBranches; ++BranchNum) {
                if (AirToZoneNodeInfo(AirSysNum).AirLoopReturnNodeNum(InBranchNum) == PrimaryAirSystems(AirSysNum).Branch(BranchNum).NodeNumIn) {
                    PrimaryAirSystems(AirSysNum).InletBranchNum(InBranchNum) = BranchNum;
                }
            }
            if (PrimaryAirSystems(AirSysNum).InletBranchNum(InBranchNum) == 0) {
                ShowSevereError(
                    state, std::string{RoutineName} + CurrentModuleObject + "=\"" + PrimaryAirSystems(AirSysNum).Name + "\", connection to zone.");
                ShowContinueError(state, "No Connection found for Return Air from Zone");
                ShowContinueError(state,
                                  "Expected node name =\"" +
                                      state.dataLoopNodes->NodeID(AirToZoneNodeInfo(AirSysNum).AirLoopReturnNodeNum(InBranchNum)) + "\".");
                ErrorsFound = true;
            }
        }

        // Check to see if a spliter and/or mixer exist
        SplitterExists = false;
        MixerExists = false;

        if (ConnectorListName != std::string()) {
            ConListNum = state.dataInputProcessing->inputProcessor->getObjectItemNum(state, "ConnectorList", ConnectorListName);
            if (ConListNum > 0) {
                state.dataInputProcessing->inputProcessor->getObjectItem(
                    state, "ConnectorList", ConListNum, Alphas, NumAlphas, Numbers, NumNumbers, IOStat);
                if ((UtilityRoutines::SameString(Alphas(2), "Connector:Splitter")) ||
                    (UtilityRoutines::SameString(Alphas(4), "Connector:Splitter"))) {
                    SplitterExists = true;
                }
                if ((UtilityRoutines::SameString(Alphas(2), "Connector:Mixer")) || (UtilityRoutines::SameString(Alphas(4), "Connector:Mixer"))) {
                    MixerExists = true;
                }
            } else {
                ShowSevereError(
                    state, std::string{RoutineName} + CurrentModuleObject + "=\"" + PrimaryAirSystems(AirSysNum).Name + "\", connector list object.");
                ShowContinueError(state, "ConnectorList object=\"" + ConnectorListName + "\" not found in input.");
            }
            errFlag = false;
            GetNumSplitterMixerInConntrList(
                state, "AirLoop", ConnectorListName, state.dataLoopNodes->NumofSplitters, state.dataLoopNodes->NumofMixers, errFlag);
            if (errFlag) {
            }
        }

        // If there is a SPLITTER, get its data
        if (SplitterExists) {
            state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, "Connector:Splitter", NumParams, NumAlphas, NumNodes);
            NodeNames.allocate(NumAlphas);
            NodeNumbers.allocate(NumAlphas);
            GetLoopSplitter(state,
                            PrimaryAirSystems(AirSysNum).Name,
                            ConnectorListName,
                            PrimaryAirSystems(AirSysNum).Splitter.Name,
                            PrimaryAirSystems(AirSysNum).Splitter.Exists,
                            PrimaryAirSystems(AirSysNum).Splitter.NodeNameIn,
                            PrimaryAirSystems(AirSysNum).Splitter.NodeNumIn,
                            PrimaryAirSystems(AirSysNum).Splitter.TotalOutletNodes,
                            NodeNames,
                            NodeNumbers,
                            ErrorsFound);

            PrimaryAirSystems(AirSysNum).Splitter.NodeNameOut.allocate(PrimaryAirSystems(AirSysNum).Splitter.TotalOutletNodes);
            PrimaryAirSystems(AirSysNum).Splitter.NodeNumOut.allocate(PrimaryAirSystems(AirSysNum).Splitter.TotalOutletNodes);
            PrimaryAirSystems(AirSysNum).Splitter.BranchNumOut.allocate(PrimaryAirSystems(AirSysNum).Splitter.TotalOutletNodes);

            for (NodeNum = 1; NodeNum <= PrimaryAirSystems(AirSysNum).Splitter.TotalOutletNodes; ++NodeNum) {

                PrimaryAirSystems(AirSysNum).Splitter.NodeNameOut(NodeNum) = NodeNames(NodeNum);
                PrimaryAirSystems(AirSysNum).Splitter.NodeNumOut(NodeNum) = NodeNumbers(NodeNum);

                PrimaryAirSystems(AirSysNum).Splitter.BranchNumOut(NodeNum) = 0;
                for (BranchNum = 1; BranchNum <= PrimaryAirSystems(AirSysNum).NumBranches; ++BranchNum) {

                    if (PrimaryAirSystems(AirSysNum).Branch(BranchNum).NodeNumIn == PrimaryAirSystems(AirSysNum).Splitter.NodeNumOut(NodeNum)) {
                        PrimaryAirSystems(AirSysNum).Splitter.BranchNumOut(NodeNum) = BranchNum;
                        break;
                    }
                }
            }

            PrimaryAirSystems(AirSysNum).Splitter.BranchNumIn = 0;
            for (BranchNum = 1; BranchNum <= PrimaryAirSystems(AirSysNum).NumBranches; ++BranchNum) {

                if (PrimaryAirSystems(AirSysNum).Branch(BranchNum).NodeNumOut == PrimaryAirSystems(AirSysNum).Splitter.NodeNumIn) {
                    PrimaryAirSystems(AirSysNum).Splitter.BranchNumIn = BranchNum;
                    break;
                }
            }

            if (allocated(NodeNames)) {
                NodeNames.deallocate();
                NodeNumbers.deallocate();
            }

        } else {
            PrimaryAirSystems(AirSysNum).Splitter.Exists = false;
            PrimaryAirSystems(AirSysNum).Splitter.NodeNumIn = 0;
            PrimaryAirSystems(AirSysNum).Splitter.BranchNumIn = 0;
            PrimaryAirSystems(AirSysNum).Splitter.NodeNameIn = "";
            PrimaryAirSystems(AirSysNum).Splitter.TotalOutletNodes = 0;
            PrimaryAirSystems(AirSysNum).Splitter.NodeNumOut.allocate(0);
            PrimaryAirSystems(AirSysNum).Splitter.BranchNumOut.allocate(0);
            PrimaryAirSystems(AirSysNum).Splitter.NodeNameOut.allocate(0);
        }

        // If there is a MIXER, get its data
        if (MixerExists) {
            state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, "Connector:Mixer", NumParams, NumAlphas, NumNodes);
            NodeNames.allocate(NumAlphas);
            NodeNumbers.allocate(NumAlphas);
            GetLoopMixer(state,
                         PrimaryAirSystems(AirSysNum).Name,
                         ConnectorListName,
                         PrimaryAirSystems(AirSysNum).Mixer.Name,
                         PrimaryAirSystems(AirSysNum).Mixer.Exists,
                         PrimaryAirSystems(AirSysNum).Mixer.NodeNameOut,
                         PrimaryAirSystems(AirSysNum).Mixer.NodeNumOut,
                         PrimaryAirSystems(AirSysNum).Mixer.TotalInletNodes,
                         NodeNames,
                         NodeNumbers,
                         ErrorsFound);

            PrimaryAirSystems(AirSysNum).Mixer.NodeNameIn.allocate(PrimaryAirSystems(AirSysNum).Mixer.TotalInletNodes);
            PrimaryAirSystems(AirSysNum).Mixer.NodeNumIn.allocate(PrimaryAirSystems(AirSysNum).Mixer.TotalInletNodes);
            PrimaryAirSystems(AirSysNum).Mixer.BranchNumIn.allocate(PrimaryAirSystems(AirSysNum).Mixer.TotalInletNodes);

            for (NodeNum = 1; NodeNum <= PrimaryAirSystems(AirSysNum).Mixer.TotalInletNodes; ++NodeNum) {

                PrimaryAirSystems(AirSysNum).Mixer.NodeNameIn(NodeNum) = NodeNames(NodeNum);
                PrimaryAirSystems(AirSysNum).Mixer.NodeNumIn(NodeNum) = NodeNumbers(NodeNum);

                PrimaryAirSystems(AirSysNum).Mixer.BranchNumIn(NodeNum) = 0;
                for (BranchNum = 1; BranchNum <= PrimaryAirSystems(AirSysNum).NumBranches; ++BranchNum) {

                    if (PrimaryAirSystems(AirSysNum).Branch(BranchNum).NodeNumIn == PrimaryAirSystems(AirSysNum).Mixer.NodeNumIn(NodeNum)) {
                        PrimaryAirSystems(AirSysNum).Mixer.BranchNumIn(NodeNum) = BranchNum;
                        break;
                    }
                }
            }

            PrimaryAirSystems(AirSysNum).Mixer.BranchNumOut = 0;
            for (BranchNum = 1; BranchNum <= PrimaryAirSystems(AirSysNum).NumBranches; ++BranchNum) {

                if (PrimaryAirSystems(AirSysNum).Branch(BranchNum).NodeNumIn == PrimaryAirSystems(AirSysNum).Mixer.NodeNumOut) {
                    PrimaryAirSystems(AirSysNum).Mixer.BranchNumOut = BranchNum;
                    break;
                }
            }

            if (allocated(NodeNames)) {
                NodeNames.deallocate();
                NodeNumbers.deallocate();
            }

        } else {
            PrimaryAirSystems(AirSysNum).Mixer.Exists = false;
            PrimaryAirSystems(AirSysNum).Mixer.NodeNumOut = 0;
            PrimaryAirSystems(AirSysNum).Mixer.BranchNumOut = 0;
            PrimaryAirSystems(AirSysNum).Mixer.NodeNameOut = "";
            PrimaryAirSystems(AirSysNum).Mixer.TotalInletNodes = 0;
            PrimaryAirSystems(AirSysNum).Mixer.NodeNumIn.allocate(0);
            PrimaryAirSystems(AirSysNum).Mixer.BranchNumIn.allocate(0);
            PrimaryAirSystems(AirSysNum).Mixer.NodeNameIn.allocate(0);
        }

        NumControllers = 0;
        if (ControllerListName != std::string()) { // If not blank, then must be there and valid
            // Loop through the controller lists until you find the one attached to this primary air system
            ControllerListNum = state.dataInputProcessing->inputProcessor->getObjectItemNum(state, "AirLoopHVAC:ControllerList", ControllerListName);
            if (ControllerListNum > 0) {
                state.dataInputProcessing->inputProcessor->getObjectItem(
                    state, "AirLoopHVAC:ControllerList", ControllerListNum, Alphas, NumAlphas, Numbers, NumNumbers, IOStat);
                // Check the current controller list and if it matches input names
                NumControllers = (NumAlphas - 1) / 2; // Subtract off the controller list name first
                // store all the controller data
                PrimaryAirSystems(AirSysNum).NumControllers = NumControllers + NumOASysSimpControllers;
                PrimaryAirSystems(AirSysNum).ControllerName.allocate(NumControllers + NumOASysSimpControllers);
                PrimaryAirSystems(AirSysNum).ControllerType.allocate(NumControllers + NumOASysSimpControllers);
                PrimaryAirSystems(AirSysNum).ControllerIndex.allocate(NumControllers + NumOASysSimpControllers);
                PrimaryAirSystems(AirSysNum).ControllerIndex = 0;
                PrimaryAirSystems(AirSysNum).ControlConverged.allocate(NumControllers + NumOASysSimpControllers);
                PrimaryAirSystems(AirSysNum).CanBeLockedOutByEcono.allocate(NumControllers + NumOASysSimpControllers);
                for (ControllerNum = NumOASysSimpControllers + 1; ControllerNum <= NumOASysSimpControllers + NumControllers; ++ControllerNum) {
                    ControllerName = Alphas((ControllerNum - NumOASysSimpControllers) * 2 + 1);
                    ControllerType = Alphas((ControllerNum - NumOASysSimpControllers) * 2);
                    PrimaryAirSystems(AirSysNum).ControllerName(ControllerNum) = ControllerName;
                    PrimaryAirSystems(AirSysNum).ControllerType(ControllerNum) = ControllerType;
                    IsNotOK = false;
                    ValidateComponent(state, ControllerType, ControllerName, IsNotOK, CurrentModuleObject);
                    if (IsNotOK) {
                        ShowContinueError(state,
                                          std::string{RoutineName} + CurrentModuleObject + "=\"" + PrimaryAirSystems(AirSysNum).Name +
                                              "\", for ControllerList=\"" + ControllerListName + "\".");
                        ErrorsFound = true;
                    }
                    PrimaryAirSystems(AirSysNum).ControlConverged(ControllerNum) = false;
                    PrimaryAirSystems(AirSysNum).CanBeLockedOutByEcono(ControllerNum) = false;
                } // End of ControllerListNum Loop
            } else {
                ShowSevereError(state,
                                std::string{RoutineName} + CurrentModuleObject + "=\"" + PrimaryAirSystems(AirSysNum).Name +
                                    "\", controller list object.");
                ShowContinueError(state, "ControllerList object=\"" + ControllerListName + "\" not found in input.");
                ErrorsFound = true;
            }
        }
        if (NumOASysSimpControllers > 0) {
            state.dataInputProcessing->inputProcessor->getObjectItem(
                state, "AirLoopHVAC:ControllerList", OASysContListNum, Alphas, NumAlphas, Numbers, NumNumbers, IOStat);
            // allocate air primary system controller lists if not already done
            if (NumControllers == 0) {
                PrimaryAirSystems(AirSysNum).NumControllers = NumOASysSimpControllers;
                PrimaryAirSystems(AirSysNum).ControllerName.allocate(NumOASysSimpControllers);
                PrimaryAirSystems(AirSysNum).ControllerType.allocate(NumOASysSimpControllers);
                PrimaryAirSystems(AirSysNum).ControllerIndex.allocate(NumOASysSimpControllers);
                PrimaryAirSystems(AirSysNum).ControllerIndex = 0;
                PrimaryAirSystems(AirSysNum).ControlConverged.allocate(NumOASysSimpControllers);
                PrimaryAirSystems(AirSysNum).CanBeLockedOutByEcono.allocate(NumOASysSimpControllers);
                PrimaryAirSystems(AirSysNum).ControlConverged = false;
                PrimaryAirSystems(AirSysNum).CanBeLockedOutByEcono = false;
            }
            // loop over the OA Sys controllers and move them up to the primary air system controller lists
            OASysControllerNum = 0;
            NumOASysControllers = (NumAlphas - 1) / 2;
            for (ControllerNum = 1; ControllerNum <= NumOASysControllers; ++ControllerNum) {
                ControllerName = Alphas(ControllerNum * 2 + 1);
                ControllerType = Alphas(ControllerNum * 2);
                if (!UtilityRoutines::SameString(ControllerType, "Controller:OutdoorAir")) {
                    ++OASysControllerNum;
                    PrimaryAirSystems(AirSysNum).ControllerName(OASysControllerNum) = ControllerName;
                    PrimaryAirSystems(AirSysNum).ControllerType(OASysControllerNum) = ControllerType;
                    PrimaryAirSystems(AirSysNum).ControlConverged(OASysControllerNum) = false;
                    PrimaryAirSystems(AirSysNum).CanBeLockedOutByEcono(OASysControllerNum) = true;
                    GetControllerActuatorNodeNum(state, ControllerName, ActuatorNodeNum, errFlag);

                    bool nonLockoutCoilFound = false;
                    WaterCoilNodeNum = -1;
                    // added to fix bug issue #5695, if HW coil on outdoor air system, don't lock out during economizing
                    if (OANum > 0) {
                        for (int OACompNum = 1; OACompNum <= OutsideAirSys(OANum).NumComponents; ++OACompNum) {
                            CompType = OutsideAirSys(OANum).ComponentType(OACompNum);
                            if (UtilityRoutines::SameString(CompType, "Coil:Heating:Water")) {
                                WaterCoilNodeNum = GetCoilWaterInletNode(state, CompType, OutsideAirSys(OANum).ComponentName(OACompNum), ErrorsFound);
                                if (WaterCoilNodeNum == ActuatorNodeNum) nonLockoutCoilFound = true;
                                break;
                            }
                        }
                    }
                    if (!nonLockoutCoilFound) {
                        //         Coil controllers can be entered either in the air loop controller list or the
                        //         OA system controller list. The CanBeLockedOutByEcono should only be set for OA coils
                        //         First get the OA controller actuator node and then compare to the air loop coil water inlet node
                        //         If these node numbers match, the coil is in the main air loop and the lockout flag should be reset to FALSE
                        for (BranchNum = 1; BranchNum <= PrimaryAirSystems(AirSysNum).NumBranches; ++BranchNum) {
                            for (CompNum = 1; CompNum <= PrimaryAirSystems(AirSysNum).Branch(BranchNum).TotalComponents; ++CompNum) {
                                if (UtilityRoutines::SameString(PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).TypeOf,
                                                                "AirloopHVAC:OutdoorAirSystem"))
                                    continue;
                                CompType = PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).TypeOf;
                                if (UtilityRoutines::SameString(CompType, "Coil:Cooling:Water:DetailedGeometry") ||
                                    UtilityRoutines::SameString(CompType, "Coil:Heating:Water") ||
                                    UtilityRoutines::SameString(CompType, "Coil:Cooling:Water")) {
                                    WaterCoilNodeNum = GetCoilWaterInletNode(
                                        state, CompType, PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).Name, ErrorsFound);
                                    if (WaterCoilNodeNum == ActuatorNodeNum) {
                                        nonLockoutCoilFound = true;
                                        break;
                                    }
                                }
                            }
                        }
                    }
                    if (nonLockoutCoilFound) {
                        PrimaryAirSystems(AirSysNum).CanBeLockedOutByEcono(OASysControllerNum) = false;
                    }
                }
            }
        }
        if (NumControllers + NumOASysSimpControllers == 0) {
            if (!PackagedUnit(AirSysNum)) {
                ShowWarningError(
                    state, std::string{RoutineName} + CurrentModuleObject + "=\"" + PrimaryAirSystems(AirSysNum).Name + "\" has no Controllers.");
            }
            PrimaryAirSystems(AirSysNum).NumControllers = 0;
            PrimaryAirSystems(AirSysNum).ControllerName.allocate(0);
            PrimaryAirSystems(AirSysNum).ControllerType.allocate(0);
            PrimaryAirSystems(AirSysNum).ControlConverged.allocate(0);
            PrimaryAirSystems(AirSysNum).CanBeLockedOutByEcono.allocate(0);
        }

        errFlag = false;
        GetAirLoopAvailabilityManager(state, AvailManagerListName, AirSysNum, NumPrimaryAirSys, errFlag);

        if (errFlag) {
            ShowContinueError(state, "Occurs in " + CurrentModuleObject + " = " + PrimaryAirSystems(AirSysNum).Name);
            ErrorsFound = true;
        }

    } // End Air Loop

    Numbers.deallocate();
    cNumericFields.deallocate();
    lNumericBlanks.deallocate();
    Alphas.deallocate();
    cAlphaFields.deallocate();
    lAlphaBlanks.deallocate();

    TestUniqueNodes.deallocate();
    for (AirSysNum = 1; AirSysNum <= NumPrimaryAirSys; ++AirSysNum) {
        for (BranchNum = 1; BranchNum <= PrimaryAirSystems(AirSysNum).NumBranches; ++BranchNum) {
            for (CompNum = 1; CompNum <= PrimaryAirSystems(AirSysNum).Branch(BranchNum).TotalComponents; ++CompNum) {

                {
                    auto const componentType(uppercased(PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).TypeOf));

                    if (componentType == "AIRLOOPHVAC:OUTDOORAIRSYSTEM") {
                        PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).CompType_Num = CompType::OAMixer_Num;

                        // Fan Types for the air sys simulation
                    } else if (componentType == "FAN:CONSTANTVOLUME") {
                        PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).CompType_Num = CompType::Fan_Simple_CV;

                    } else if (componentType == "FAN:VARIABLEVOLUME") {
                        PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).CompType_Num = CompType::Fan_Simple_VAV;

                    } else if (componentType == "FAN:SYSTEMMODEL") {
                        PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).CompType_Num = CompType::Fan_System_Object;
                        // Construct fan object
                        if (HVACFan::getFanObjectVectorIndex(state, PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).Name, false) < 0) {
                            state.dataHVACFan->fanObjs.emplace_back(
                                new HVACFan::FanSystem(state, PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).Name));
                        }
                        PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).CompIndex =
                            HVACFan::getFanObjectVectorIndex(state, PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).Name) +
                            1; // + 1 for shift from zero-based vector to 1-based compIndex
                        state.dataHVACFan
                            ->fanObjs[HVACFan::getFanObjectVectorIndex(state, PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).Name)]
                            ->AirPathFlag = true;
                    } else if (componentType == "FAN:COMPONENTMODEL") {
                        PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).CompType_Num = CompType::Fan_ComponentModel;

                        // Coil Types for the air sys simulation
                        //        HX Assisted coils are not allowed on a branch at this time
                        //        CASE('COILSYSTEM:COOLING:DX:HEATEXCHANGERASSISTED')
                        //          PrimaryAirSystem(AirSysNum)%Branch(BranchNum)%Comp(CompNum)%CompType_Num=DXCoil_CoolingHXAsst
                    } else if (componentType == "COILSYSTEM:COOLING:WATER:HEATEXCHANGERASSISTED") {
                        PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).CompType_Num = CompType::WaterCoil_CoolingHXAsst;
                    } else if (componentType == "COIL:HEATING:WATER") {
                        PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).CompType_Num = CompType::WaterCoil_SimpleHeat;
                    } else if (componentType == "COIL:HEATING:STEAM") {
                        PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).CompType_Num = CompType::SteamCoil_AirHeat;
                    } else if (componentType == "COIL:COOLING:WATER:DETAILEDGEOMETRY") {
                        PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).CompType_Num = CompType::WaterCoil_DetailedCool;
                    } else if (componentType == "COIL:COOLING:WATER") {
                        PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).CompType_Num = CompType::WaterCoil_Cooling;
                    } else if (componentType == "COIL:HEATING:ELECTRIC") {
                        PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).CompType_Num = CompType::Coil_ElectricHeat;
                    } else if (componentType == "COIL:HEATING:FUEL") {
                        PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).CompType_Num = CompType::Coil_GasHeat;

                        // Heat reclaim
                    } else if (componentType == "COIL:HEATING:DESUPERHEATER") {
                        PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).CompType_Num = CompType::Coil_DeSuperHeat;

                    } else if (componentType == "COILSYSTEM:COOLING:DX") {
                        PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).CompType_Num = CompType::DXSystem;
                    } else if (componentType == "COILSYSTEM:HEATING:DX") {
                        PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).CompType_Num = CompType::DXHeatPumpSystem;
                    } else if (componentType == "COIL:USERDEFINED") {
                        PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).CompType_Num = CompType::CoilUserDefined;
                    } else if (componentType == "AIRLOOPHVAC:UNITARYSYSTEM") {
                        PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).CompType_Num = CompType::UnitarySystemModel;
                        UnitarySystems::UnitarySys thisSys;
                        PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).compPointer =
                            thisSys.factory(state,
                                            DataHVACGlobals::UnitarySys_AnyCoilType,
                                            PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).Name,
                                            false,
                                            0);
                    } else if (componentType == "COILSYSTEM:COOLING:WATER") {
                        PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).CompType_Num = CompType::CoilSystemWater;
                        UnitarySystems::UnitarySys thisSys;
                        PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).compPointer =
                            thisSys.factory(state,
                                            DataHVACGlobals::UnitarySys_AnyCoilType,
                                            PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).Name,
                                            false,
                                            0);
                    } else if (componentType == "AIRLOOPHVAC:UNITARY:FURNACE:HEATONLY") {
                        PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).CompType_Num = CompType::Furnace_UnitarySys_HeatOnly;
                    } else if (componentType == "AIRLOOPHVAC:UNITARY:FURNACE:HEATCOOL") {
                        PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).CompType_Num = CompType::Furnace_UnitarySys_HeatCool;
                    } else if (componentType == "AIRLOOPHVAC:UNITARYHEATONLY") {
                        PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).CompType_Num = CompType::Furnace_UnitarySys_HeatOnly;
                    } else if (componentType == "AIRLOOPHVAC:UNITARYHEATCOOL") {
                        PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).CompType_Num = CompType::Furnace_UnitarySys_HeatCool;
                    } else if (componentType == "AIRLOOPHVAC:UNITARYHEATPUMP:AIRTOAIR") {
                        PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).CompType_Num = CompType::Furnace_UnitarySys_HeatCool;
                    } else if (componentType == "AIRLOOPHVAC:UNITARYHEATPUMP:WATERTOAIR") {
                        PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).CompType_Num = CompType::Furnace_UnitarySys_HeatCool;

                    } else if (componentType == "AIRLOOPHVAC:UNITARYHEATCOOL:VAVCHANGEOVERBYPASS") {
                        PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).CompType_Num = CompType::UnitarySystem_BypassVAVSys;

                        // Humidifier Types for the air system simulation
                    } else if (componentType == "HUMIDIFIER:STEAM:ELECTRIC") {
                        PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).CompType_Num = CompType::Humidifier;

                    } else if (componentType == "HUMIDIFIER:STEAM:GAS") {
                        PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).CompType_Num = CompType::Humidifier;

                        // Evap Cooler Types for the air system simulation
                    } else if (componentType == "EVAPORATIVECOOLER:DIRECT:CELDEKPAD") {
                        PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).CompType_Num = CompType::EvapCooler;
                    } else if (componentType == "EVAPORATIVECOOLER:INDIRECT:CELDEKPAD") {
                        PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).CompType_Num = CompType::EvapCooler;
                    } else if (componentType == "EVAPORATIVECOOLER:INDIRECT:WETCOIL") {
                        PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).CompType_Num = CompType::EvapCooler;
                    } else if (componentType == "EVAPORATIVECOOLER:INDIRECT:RESEARCHSPECIAL") {
                        PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).CompType_Num = CompType::EvapCooler;
                    } else if (componentType == "EVAPORATIVECOOLER:DIRECT:RESEARCHSPECIAL") {
                        PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).CompType_Num = CompType::EvapCooler;

                        // Desiccant Dehumidifier Types for the air system simulation
                    } else if (componentType == "DEHUMIDIFIER:DESICCANT:NOFANS") {
                        PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).CompType_Num = CompType::Desiccant;
                    } else if (componentType == "DEHUMIDIFIER:DESICCANT:SYSTEM") {
                        PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).CompType_Num = CompType::Desiccant;

                        // Heat recovery
                    } else if (componentType == "HEATEXCHANGER:AIRTOAIR:FLATPLATE") {
                        PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).CompType_Num = CompType::HeatXchngr;

                    } else if (componentType == "HEATEXCHANGER:AIRTOAIR:SENSIBLEANDLATENT") {
                        PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).CompType_Num = CompType::HeatXchngr;

                    } else if (componentType == "HEATEXCHANGER:DESICCANT:BALANCEDFLOW") {
                        PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).CompType_Num = CompType::HeatXchngr;

                        // Ducts
                    } else if (componentType == "DUCT") {
                        PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).CompType_Num = CompType::Duct;

                    } else if (componentType == "AIRLOOPHVAC:UNITARYHEATPUMP:AIRTOAIR:MULTISPEED") {
                        PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).CompType_Num = CompType::UnitarySystem_MSHeatPump;

                    } else if (componentType == "ZONEHVAC:TERMINALUNIT:VARIABLEREFRIGERANTFLOW") {
                        PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).CompType_Num = CompType::ZoneVRFasAirLoopEquip;

                    } else if (componentType == "FAN:ONOFF" || componentType == "COIL:COOLING:DX:SINGLESPEED" ||
                               componentType == "COIL:HEATING:DX:SINGLESPEED" || componentType == "COIL:COOLING:DX:TWOSTAGEWITHHUMIDITYCONTROLMODE" ||
                               componentType == "COIL:COOLING:DX:MULTISPEED" || componentType == "COIL:HEATING:DX:MULTISPEED") {
                        ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + " = \"" + PrimaryAirSystems(AirSysNum).Name + "\".");
                        ShowContinueError(state,
                                          "..Invalid Air Loop Component Type = \"" +
                                              PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).TypeOf + "\".");
                        ShowContinueError(
                            state, "..Air Loop Component Name = \"" + PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).Name + "\".");
                        ShowContinueError(state, "..reference Branch = \"" + PrimaryAirSystems(AirSysNum).Branch(BranchNum).Name + "\".");
                        ShowContinueError(state,
                                          "...This component may only be referenced by a parent component such as "
                                          "AirLoopHVAC:Unitary:Furnace:HeatCool or similar.");
                        ErrorsFound = true;

                    } else {
                        ShowSevereError(state, std::string{RoutineName} + CurrentModuleObject + " = \"" + PrimaryAirSystems(AirSysNum).Name + "\".");
                        ShowContinueError(state,
                                          "..Invalid Air Loop Component Type = \"" +
                                              PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).TypeOf + "\".");
                        ShowContinueError(
                            state, "..Air Loop Component Name = \"" + PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).Name + "\".");
                        ShowContinueError(state, "..reference Branch = \"" + PrimaryAirSystems(AirSysNum).Branch(BranchNum).Name + "\".");
                        ErrorsFound = true;
                    }
                }
            }
        }
    }

    // check that actuator nodes are matched by a water coil inlet node

    for (AirSysNum = 1; AirSysNum <= NumPrimaryAirSys; ++AirSysNum) {
        for (BranchNum = 1; BranchNum <= PrimaryAirSystems(AirSysNum).NumBranches; ++BranchNum) {
            for (CompNum = 1; CompNum <= PrimaryAirSystems(AirSysNum).Branch(BranchNum).TotalComponents; ++CompNum) {
                CompType_Num = PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).CompType_Num;
                if (CompType_Num == CompType::WaterCoil_DetailedCool || CompType_Num == CompType::WaterCoil_SimpleHeat ||
                    CompType_Num == CompType::WaterCoil_Cooling) {
                    WaterCoilNodeNum = GetCoilWaterInletNode(state,
                                                             PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).TypeOf,
                                                             PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).Name,
                                                             ErrorsFound);
                    CheckCoilWaterInletNode(state, WaterCoilNodeNum, NodeNotFound);
                    if (NodeNotFound) {
                        ErrorsFound = true;
                        ShowSevereError(state,
                                        std::string{RoutineName} + CurrentModuleObject + "=\"" +
                                            PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).Name + "\", invalid actuator.");
                        ShowContinueError(state,
                                          "...this coil requires a water coil controller and the inlet node of a water coil must also be an "
                                          "actuator node of a water coil controller.");
                    }
                }
            }
        }
    }

    OANum = GetNumOASystems(state);
    for (int OASysNum = 1; OASysNum <= OANum; ++OASysNum) {
        NumInList = GetOACompListNumber(state, OASysNum);
        for (int OACompNum = 1; OACompNum <= NumInList; ++OACompNum) {
            CompType_Num = GetOACompTypeNum(state, OASysNum, OACompNum);
            if (CompType_Num == CompType::WaterCoil_DetailedCool || CompType_Num == CompType::WaterCoil_SimpleHeat ||
                CompType_Num == CompType::WaterCoil_Cooling) {
                WaterCoilNodeNum =
                    GetCoilWaterInletNode(state, GetOACompType(state, OASysNum, OACompNum), GetOACompName(state, OASysNum, OACompNum), ErrorsFound);
                CheckCoilWaterInletNode(state, WaterCoilNodeNum, NodeNotFound);
                UnitarySystems::isWaterCoilHeatRecoveryType(state, WaterCoilNodeNum, NodeNotFound);
                if (NodeNotFound) {
                    ErrorsFound = true;
                    ShowSevereError(state,
                                    std::string{RoutineName} + CurrentModuleObject + "=\"" + GetOACompName(state, OASysNum, OACompNum) +
                                        "\", invalid actuator.");
                    ShowContinueError(state,
                                      "...this coil requires a water coil controller and the inlet node of a water coil must also be an actuator "
                                      "node of a water coil controller.");
                }
            }
        }
    }

    if (ErrorsFound) {
        ShowFatalError(state, format("{}Errors found retrieving input for {}.", RoutineName, CurrentModuleObject));
    }

    for (AirSysNum = 1; AirSysNum <= NumPrimaryAirSys; ++AirSysNum) {
        SetupOutputVariable(state,
                            "Air System Simulation Cycle On Off Status",
                            OutputProcessor::Unit::None,
                            state.dataAirLoop->PriAirSysAvailMgr(AirSysNum).AvailStatus,
                            OutputProcessor::SOVTimeStepType::HVAC,
                            OutputProcessor::SOVStoreType::Average,
                            PrimaryAirSystems(AirSysNum).Name);
    }

    if (state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, "AirLoopHVAC:DedicatedOutdoorAirSystem") > 0) {
        if (state.dataAirLoopHVACDOAS->GetInputOnceFlag) {
            AirLoopHVACDOAS::getAirLoopHVACDOASInput(state);
            state.dataAirLoopHVACDOAS->GetInputOnceFlag = false;
        }
    }
}

// End of Get Input subroutines for the Module
//******************************************************************************

// Beginning Initialization Section of the Module
//******************************************************************************

void InitAirLoops(EnergyPlusData &state, bool const FirstHVACIteration) // TRUE if first full HVAC iteration in an HVAC timestep
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Richard J. Liesen
    //       DATE WRITTEN   April 1998
    //       MODIFIED       Dec 1999 Fred Buhl
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Initializes the primary air system simulation

    // METHODOLOGY EMPLOYED:
    // (1) For the first simulation in an HVAC timestep, the air system is initialized to
    //     design air flow rates.
    // (2) For subsequent simulations, air flow data is set by the zone equipment inlet
    //     nodes and the return air node.
    // (3) Other air system node data such as temperatures and humidity ratios are only
    //     initialized at the start of an environment (run period or design day).

    // Using/Aliasing
    using Fans::GetFanIndex;
    using General::FindNumberInList;
    using Psychrometrics::PsyHFnTdbW;
    using Psychrometrics::PsyRhoAirFnPbTdbW;
    using SplitterComponent::SplitterConditions;
    // using ZonePlenum::ZoneSupPlenCond;
    using ZonePlenum::ZoneSupplyPlenumConditions;

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int NumAllSupAirPathNodes; // total number of nodes in a supply air path including duplicates
    int NumSupAirPathNodes;    // total number of nodes in a supply air path
    int NumSupAirPathOutNodes; // total number of outlet nodes in a supply air path
    int NumSupAirPathIntNodes; // total number of intermediate nodes in a supply air path
    int NodeIndex;             // DO loop index for nodes on branch
    int SupNodeIndex;          // do loop index of a supply air path node
    int SupNodeIndex2;         // 2nd do loop index of a supply air path node
    int SupAirPathNodeNum;     // index of a supply air path node
    int SupAirPathOutNodeNum;  // index of a supply air path outlet node
    int AirLoopNum;            // DO loop counter for air systems
    int BranchNum;             // DO loop counter for branches
    int OutBranchNum;          // reference number of an outlet branch
    int InBranchNum;           // reference number of an inlet branch
    // unused  INTEGER :: InletBranchNum   ! Branch reference number of splitter inlet branch
    int NodeNum;                  // a node number
    int OutNum;                   // DO loop index for outlet branches
    int InNum;                    // DO loop index for inlet branches
    int CompNum;                  // DO loop index for  branch components
    int ZoneSideNodeNum;          // a Zone Equipment inlet node number
    int BranchNodeIndex;          // DO loop index for nodes on a branch
    int NodeNumOut;               // node number of a branch outlet node
    int NodeNumIn;                // node number of a splitter inlet node or a branch inlet node
    int SplitterOutNum;           // DO loop index of splitter outlets
    int PlenumOutNum;             // DO loop index of supply plenum outlets
    Real64 MassFlowSaved;         // mass flow rate for a node saved from previous call
    Real64 MassFlowSet;           // desired mass flow rate for a node
    int SupAirPath;               // supply air path do loop index
    int SupAirPathNum;            // specific supply air path index
    int SplitterNum;              // Zone equip splitter index
    int PlenumNum;                // supply plenum index
    int CtrlZoneNum;              // Controlled zone index
    int ZoneInNum;                // zone inlet index
    int ZoneInSysIndex;           // index into CoolCtrlZoneNums or HeatCtrlZoneNums
    int NumComponentsInSys;       // total number of components in the primary air system
    int NumComponentsOnBranch;    // total number of components in the primary air system
    bool FoundSupPathZoneConnect; // true if there is a valid connection between the supply air path and a zone terminal unit inlet
    CompType CompTypeNum;         // component_type number for components on branches
    int SupFanIndex;
    int RetFanIndex;
    bool FoundOASys;

    // Dimension the local subcomponent arrays

    auto &NumPrimaryAirSys = state.dataHVACGlobal->NumPrimaryAirSys;

    bool ErrorsFound;
    Array1D_int tmpNodeARR;
    int nodeLoop;
    int ZoneNum;

    ErrorsFound = false;
    state.dataHVACGlobal->AirLoopInit = true;

    auto &PrimaryAirSystems(state.dataAirSystemsData->PrimaryAirSystems);
    auto &AirToZoneNodeInfo(state.dataAirLoop->AirToZoneNodeInfo);
    auto &AirLoopControlInfo(state.dataAirLoop->AirLoopControlInfo);

    // Do the one time initializations
    if (state.dataSimAirServingZones->InitAirLoopsOneTimeFlag) {

        // Figure out what zones are served by each primary air system (air loop) and
        // store the results in AirToZoneNodeInfo()%CoolCtrlZoneNums and AirToZoneNodeInfo()%HeatCtrlZoneNums

        // Allocate scratch arrays for storing controlled zone numbers for each air loop.
        state.dataSimAirServingZones->CtrlZoneNumsCool.allocate(state.dataGlobal->NumOfZones);
        state.dataSimAirServingZones->CtrlZoneNumsHeat.allocate(state.dataGlobal->NumOfZones);
        state.dataSimAirServingZones->ZoneInletNodesCool.allocate(state.dataGlobal->NumOfZones);
        state.dataSimAirServingZones->ZoneInletNodesHeat.allocate(state.dataGlobal->NumOfZones);
        state.dataSimAirServingZones->TermInletNodesCool.allocate(state.dataGlobal->NumOfZones);
        state.dataSimAirServingZones->TermInletNodesHeat.allocate(state.dataGlobal->NumOfZones);
        state.dataSimAirServingZones->TermUnitSizingNumsCool.allocate(state.dataGlobal->NumOfZones);
        state.dataSimAirServingZones->TermUnitSizingNumsHeat.allocate(state.dataGlobal->NumOfZones);

        state.dataSimAirServingZones->MassFlowSetToler = DataConvergParams::HVACFlowRateToler * 0.00001;

        for (SupAirPath = 1; SupAirPath <= state.dataZoneEquip->NumSupplyAirPaths; ++SupAirPath) {

            NumAllSupAirPathNodes = 0;
            SupAirPathNodeNum = 0;
            SupAirPathOutNodeNum = 0;
            NumSupAirPathOutNodes = 0;
            NumSupAirPathNodes = 0;
            NumSupAirPathIntNodes = 0;

            // each supply air path may have up to one splitter and one plenum.  Check for all combinations count
            // all nodes (including duplicates)
            for (CompNum = 1; CompNum <= state.dataZoneEquip->SupplyAirPath(SupAirPath).NumOfComponents; ++CompNum) {
                if (UtilityRoutines::SameString(state.dataZoneEquip->SupplyAirPath(SupAirPath).ComponentType(CompNum), "AirLoopHVAC:ZoneSplitter")) {
                    SplitterNum = UtilityRoutines::FindItemInList(state.dataZoneEquip->SupplyAirPath(SupAirPath).ComponentName(CompNum),
                                                                  state.dataSplitterComponent->SplitterCond,
                                                                  &SplitterConditions::SplitterName);
                    if (SplitterNum == 0) {
                        ShowSevereError(
                            state, "AirLoopHVAC:ZoneSplitter not found=" + state.dataZoneEquip->SupplyAirPath(SupAirPath).ComponentName(CompNum));
                        ShowContinueError(state, "Occurs in AirLoopHVAC:SupplyPath=" + state.dataZoneEquip->SupplyAirPath(SupAirPath).Name);
                        ErrorsFound = true;
                    }
                    state.dataZoneEquip->SupplyAirPath(SupAirPath).SplitterIndex(CompNum) = SplitterNum;
                    NumAllSupAirPathNodes += state.dataSplitterComponent->SplitterCond(SplitterNum).NumOutletNodes + 1;
                } else if (UtilityRoutines::SameString(state.dataZoneEquip->SupplyAirPath(SupAirPath).ComponentType(CompNum),
                                                       "AirLoopHVAC:SupplyPlenum")) {
                    PlenumNum = UtilityRoutines::FindItemInList(state.dataZoneEquip->SupplyAirPath(SupAirPath).ComponentName(CompNum),
                                                                state.dataZonePlenum->ZoneSupPlenCond,
                                                                &ZoneSupplyPlenumConditions::ZonePlenumName);
                    if (PlenumNum == 0) {
                        ShowSevereError(
                            state, "AirLoopHVAC:SupplyPlenum not found=" + state.dataZoneEquip->SupplyAirPath(SupAirPath).ComponentName(CompNum));
                        ShowContinueError(state, "Occurs in AirLoopHVAC:SupplyPath=" + state.dataZoneEquip->SupplyAirPath(SupAirPath).Name);
                        ErrorsFound = true;
                    }
                    state.dataZoneEquip->SupplyAirPath(SupAirPath).PlenumIndex(CompNum) = PlenumNum;
                    NumAllSupAirPathNodes += state.dataZonePlenum->ZoneSupPlenCond(PlenumNum).NumOutletNodes + 1;
                }
            }
            state.dataSimAirServingZones->SupNode.allocate(NumAllSupAirPathNodes);
            state.dataSimAirServingZones->SupNodeType.allocate(NumAllSupAirPathNodes);

            // figure out the order of the splitter and plenum in the path, by flagging the first node of the component
            // as either a 'pathinlet' or a 'compinlet'
            for (CompNum = 1; CompNum <= state.dataZoneEquip->SupplyAirPath(SupAirPath).NumOfComponents; ++CompNum) {
                SplitterNum = state.dataZoneEquip->SupplyAirPath(SupAirPath).SplitterIndex(CompNum);
                PlenumNum = state.dataZoneEquip->SupplyAirPath(SupAirPath).PlenumIndex(CompNum);
                if (SplitterNum > 0) {
                    ++SupAirPathNodeNum;
                    state.dataSimAirServingZones->SupNode(SupAirPathNodeNum) = state.dataSplitterComponent->SplitterCond(SplitterNum).InletNode;
                    if (CompNum == 1) {
                        state.dataSimAirServingZones->SupNodeType(SupAirPathNodeNum) = DataZoneEquipment::AirNodeType::PathInlet;
                    } else {
                        state.dataSimAirServingZones->SupNodeType(SupAirPathNodeNum) = DataZoneEquipment::AirNodeType::CompInlet;
                    }
                    for (SplitterOutNum = 1; SplitterOutNum <= state.dataSplitterComponent->SplitterCond(SplitterNum).NumOutletNodes;
                         ++SplitterOutNum) {
                        ++SupAirPathNodeNum;
                        state.dataSimAirServingZones->SupNode(SupAirPathNodeNum) =
                            state.dataSplitterComponent->SplitterCond(SplitterNum).OutletNode(SplitterOutNum);
                        state.dataSimAirServingZones->SupNodeType(SupAirPathNodeNum) = DataZoneEquipment::AirNodeType::Invalid;
                    }
                } else if (PlenumNum > 0) {
                    ++SupAirPathNodeNum;
                    state.dataSimAirServingZones->SupNode(SupAirPathNodeNum) = state.dataZonePlenum->ZoneSupPlenCond(PlenumNum).InletNode;
                    if (CompNum == 1) {
                        state.dataSimAirServingZones->SupNodeType(SupAirPathNodeNum) = DataZoneEquipment::AirNodeType::PathInlet;
                    } else {
                        state.dataSimAirServingZones->SupNodeType(SupAirPathNodeNum) = DataZoneEquipment::AirNodeType::CompInlet;
                    }
                    for (PlenumOutNum = 1; PlenumOutNum <= state.dataZonePlenum->ZoneSupPlenCond(PlenumNum).NumOutletNodes; ++PlenumOutNum) {
                        ++SupAirPathNodeNum;
                        state.dataSimAirServingZones->SupNode(SupAirPathNodeNum) =
                            state.dataZonePlenum->ZoneSupPlenCond(PlenumNum).OutletNode(PlenumOutNum);
                        state.dataSimAirServingZones->SupNodeType(SupAirPathNodeNum) = DataZoneEquipment::AirNodeType::Invalid;
                    }
                }
            }

            // find the nodes that connect a splitter and a plenum
            for (SupNodeIndex = 1; SupNodeIndex <= NumAllSupAirPathNodes; ++SupNodeIndex) {
                if (state.dataSimAirServingZones->SupNodeType(SupNodeIndex) == DataZoneEquipment::AirNodeType::Invalid) {
                    for (SupNodeIndex2 = SupNodeIndex + 1; SupNodeIndex2 <= NumAllSupAirPathNodes; ++SupNodeIndex2) {
                        if ((state.dataSimAirServingZones->SupNode(SupNodeIndex) == state.dataSimAirServingZones->SupNode(SupNodeIndex2)) &&
                            (state.dataSimAirServingZones->SupNodeType(SupNodeIndex2) == DataZoneEquipment::AirNodeType::CompInlet)) {
                            state.dataSimAirServingZones->SupNodeType(SupNodeIndex) = DataZoneEquipment::AirNodeType::Intermediate;
                            break;
                        }
                    }
                }
            }

            //  the rest of the nodes are outlet nodes and count the duplicated intermediate nodes
            for (SupNodeIndex = 1; SupNodeIndex <= NumAllSupAirPathNodes; ++SupNodeIndex) {
                if (state.dataSimAirServingZones->SupNodeType(SupNodeIndex) == DataZoneEquipment::AirNodeType::Invalid) {
                    ++NumSupAirPathOutNodes;
                    state.dataSimAirServingZones->SupNodeType(SupNodeIndex) = DataZoneEquipment::AirNodeType::Outlet;
                } else if (state.dataSimAirServingZones->SupNodeType(SupNodeIndex) == DataZoneEquipment::AirNodeType::Intermediate) {
                    ++NumSupAirPathIntNodes;
                }
            }

            //  eliminate the duplicates to find the number of nodes in the supply air path
            NumSupAirPathNodes = NumAllSupAirPathNodes - NumSupAirPathIntNodes;
            SupAirPathNodeNum = 0;
            state.dataZoneEquip->SupplyAirPath(SupAirPath).OutletNode.allocate(NumSupAirPathOutNodes);
            state.dataZoneEquip->SupplyAirPath(SupAirPath).Node.allocate(NumSupAirPathNodes);
            state.dataZoneEquip->SupplyAirPath(SupAirPath).NodeType.allocate(NumSupAirPathNodes);
            state.dataZoneEquip->SupplyAirPath(SupAirPath).NumNodes = NumSupAirPathNodes;
            state.dataZoneEquip->SupplyAirPath(SupAirPath).NumOutletNodes = NumSupAirPathOutNodes;

            // transfer data from the local SupNode array to the SupplyAirPath data structure
            for (SupNodeIndex = 1; SupNodeIndex <= NumAllSupAirPathNodes; ++SupNodeIndex) {
                if (state.dataSimAirServingZones->SupNodeType(SupNodeIndex) == DataZoneEquipment::AirNodeType::PathInlet ||
                    state.dataSimAirServingZones->SupNodeType(SupNodeIndex) == DataZoneEquipment::AirNodeType::Intermediate ||
                    state.dataSimAirServingZones->SupNodeType(SupNodeIndex) == DataZoneEquipment::AirNodeType::Outlet) {
                    ++SupAirPathNodeNum;
                    // map the local node numbers to the HVAC (global) node numbers
                    state.dataZoneEquip->SupplyAirPath(SupAirPath).Node(SupAirPathNodeNum) = state.dataSimAirServingZones->SupNode(SupNodeIndex);
                    state.dataZoneEquip->SupplyAirPath(SupAirPath).NodeType(SupAirPathNodeNum) =
                        state.dataSimAirServingZones->SupNodeType(SupNodeIndex);
                }
                if (state.dataSimAirServingZones->SupNodeType(SupNodeIndex) == DataZoneEquipment::AirNodeType::Outlet) {
                    ++SupAirPathOutNodeNum;
                    // map the outlet node number to the HVAC (global) node number
                    state.dataZoneEquip->SupplyAirPath(SupAirPath).OutletNode(SupAirPathOutNodeNum) =
                        state.dataSimAirServingZones->SupNode(SupNodeIndex);
                }
            }
            state.dataSimAirServingZones->SupNode.deallocate();
            state.dataSimAirServingZones->SupNodeType.deallocate();
        }

        // Now loop over the air loops
        for (AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum) {

            state.dataSimAirServingZones->CtrlZoneNumsCool = 0;
            state.dataSimAirServingZones->CtrlZoneNumsHeat = 0;
            state.dataSimAirServingZones->ZoneInletNodesCool = 0;
            state.dataSimAirServingZones->ZoneInletNodesHeat = 0;
            int NumZonesCool = 0;
            int NumZonesHeat = 0;
            NumComponentsInSys = 0;

            // count the number of components in this primary air system
            for (BranchNum = 1; BranchNum <= PrimaryAirSystems(AirLoopNum).NumBranches; ++BranchNum) {
                NumComponentsInSys += PrimaryAirSystems(AirLoopNum).Branch(BranchNum).TotalComponents;
            }
            // set the Simple flag
            if (PrimaryAirSystems(AirLoopNum).NumBranches == 1 && NumComponentsInSys == 1) {
                AirLoopControlInfo(AirLoopNum).Simple = true;
            }

            // loop over the air loop's output nodes
            for (OutNum = 1; OutNum <= AirToZoneNodeInfo(AirLoopNum).NumSupplyNodes; ++OutNum) {
                ZoneSideNodeNum = AirToZoneNodeInfo(AirLoopNum).ZoneEquipSupplyNodeNum(OutNum);
                // find the corresponding branch number
                OutBranchNum = PrimaryAirSystems(AirLoopNum).OutletBranchNum(OutNum);
                // find the supply air path corresponding to each air loop outlet node
                SupAirPathNum = 0;
                // loop over the air loop's output nodes
                for (SupAirPath = 1; SupAirPath <= state.dataZoneEquip->NumSupplyAirPaths; ++SupAirPath) {
                    if (ZoneSideNodeNum == state.dataZoneEquip->SupplyAirPath(SupAirPath).InletNodeNum) {
                        SupAirPathNum = SupAirPath;
                        break;
                    }
                }
                if (SupAirPathNum > 0) {
                    NumSupAirPathOutNodes = state.dataZoneEquip->SupplyAirPath(SupAirPathNum).NumOutletNodes;
                } else {
                    NumSupAirPathOutNodes = 0;
                }

                // Now Loop over the Supply Air Path outlet nodes and find out which zone and which air terminal
                // unit on that zone is connected to that supply air path.

                for (SupAirPathOutNodeNum = 1; SupAirPathOutNodeNum <= NumSupAirPathOutNodes; ++SupAirPathOutNodeNum) {
                    FoundSupPathZoneConnect = false;
                    // loop over all controlled zones.
                    for (CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {
                        if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).IsControlled) continue;
                        // Loop over the air distribution unit inlets for each controlled zone.
                        // Look for a match between the zone splitter outlet node and the air distribution unit inlet node.
                        // When match found save the controlled zone number in CtrlZoneNumsCool or CtrlZoneNumsHeat
                        for (ZoneInNum = 1; ZoneInNum <= state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).NumInletNodes; ++ZoneInNum) {
                            NumComponentsOnBranch = PrimaryAirSystems(AirLoopNum).Branch(OutBranchNum).TotalComponents;

                            // BEGIN COOLING: Check for a match between the cooling air distribution unit inlet
                            // and the supply air path outlet
                            if (state.dataZoneEquip->SupplyAirPath(SupAirPathNum).OutletNode(SupAirPathOutNodeNum) ==
                                state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).AirDistUnitCool(ZoneInNum).InNode) {
                                ++NumZonesCool;
                                // Set Duct Type for branch for dual duct
                                if (NumZonesCool == 1 && OutBranchNum > 1) {
                                    PrimaryAirSystems(AirLoopNum).Branch(OutBranchNum).DuctType = Cooling;
                                }
                                if (NumZonesCool == 1) {
                                    AirToZoneNodeInfo(AirLoopNum).SupplyDuctType(OutNum) = Cooling;
                                }
                                state.dataSimAirServingZones->CtrlZoneNumsCool(NumZonesCool) = CtrlZoneNum;
                                state.dataSimAirServingZones->ZoneInletNodesCool(NumZonesCool) =
                                    state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).InletNode(ZoneInNum);
                                state.dataSimAirServingZones->TermInletNodesCool(NumZonesCool) =
                                    state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).AirDistUnitCool(ZoneInNum).InNode;
                                state.dataSimAirServingZones->TermUnitSizingNumsCool(NumZonesCool) =
                                    state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).AirDistUnitCool(ZoneInNum).TermUnitSizingIndex;
                                if (AirLoopNum > 0) {
                                    if (PrimaryAirSystems(AirLoopNum).OASysExists) {
                                        state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).ZoneHasAirLoopWithOASys = true;
                                    }
                                }
                                state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).InletNodeAirLoopNum(ZoneInNum) = AirLoopNum;
                                FoundSupPathZoneConnect = true;

                                // set the supply air path
                                state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).AirDistUnitCool(ZoneInNum).SupplyAirPathExists = true;

                                // Once a match is found between a supply air path outlet node and an air distribution inlet
                                // node, we go on to the next supply air path outlet.  Therefore, *both* the air distribution
                                // unit loop and the controlled zone loop may be exited.
                                goto ControlledZoneLoop_exit;
                            } // end check for cooling air distribution units

                            // END COOLING: end check for match between supply air path outlet and cooling air
                            // distribution inlet

                            // BEGIN HEATING: If we don't get a match, check for a heating match
                            if (state.dataZoneEquip->SupplyAirPath(SupAirPathNum).OutletNode(SupAirPathOutNodeNum) ==
                                state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).AirDistUnitHeat(ZoneInNum).InNode) {
                                ++NumZonesHeat;
                                // Set Duct Type for branch for dual duct
                                if (NumZonesHeat == 1 && OutBranchNum > 1) {
                                    PrimaryAirSystems(AirLoopNum).Branch(OutBranchNum).DuctType = Heating;
                                }
                                if (NumZonesHeat == 1) {
                                    AirToZoneNodeInfo(AirLoopNum).SupplyDuctType(OutNum) = Heating;
                                }
                                state.dataSimAirServingZones->CtrlZoneNumsHeat(NumZonesHeat) = CtrlZoneNum;
                                state.dataSimAirServingZones->ZoneInletNodesHeat(NumZonesHeat) =
                                    state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).InletNode(ZoneInNum);
                                state.dataSimAirServingZones->TermInletNodesHeat(NumZonesHeat) =
                                    state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).AirDistUnitHeat(ZoneInNum).InNode;
                                state.dataSimAirServingZones->TermUnitSizingNumsHeat(NumZonesHeat) =
                                    state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).AirDistUnitHeat(ZoneInNum).TermUnitSizingIndex;
                                if (state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).InletNodeAirLoopNum(ZoneInNum) == 0)
                                    state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).InletNodeAirLoopNum(ZoneInNum) = AirLoopNum;
                                FoundSupPathZoneConnect = true;

                                // Set the supply air path flag
                                state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).AirDistUnitHeat(ZoneInNum).SupplyAirPathExists = true;

                                // Once a match is found between a supply air path outlet node and an air distribution inlet
                                // node, we go on to the next supply air path outlet.  Therefore, *both* the air distribution
                                // unit loop and the controlled zone loop may be exited.
                                goto ControlledZoneLoop_exit;
                            } // end check for heatingair distribution units
                        }
                    }
                ControlledZoneLoop_exit:;

                    // If the supply air path is not connected to either a heating or a cooling air distribution
                    // unit...we have a problem!
                    if (!FoundSupPathZoneConnect) {
                        ShowSevereError(
                            state,
                            "Node " +
                                state.dataLoopNodes->NodeID(state.dataZoneEquip->SupplyAirPath(SupAirPathNum).OutletNode(SupAirPathOutNodeNum)) +
                                " connects to no component");
                        ShowContinueError(state, "Occurs in Supply Air Path=" + state.dataZoneEquip->SupplyAirPath(SupAirPathNum).Name);
                        ShowContinueError(state, "Check the connection to a ZoneHVAC:EquipmentConnections object");
                        ShowContinueError(state, "Check if this component is missing from the Supply Air Path");
                        ErrorsFound = true;
                    }
                }

                // What if there is no supply air path & the air loop outlet is just hooked directly to
                // an air distribution unit of a single zone? In this case look for a match between
                // ZoneSideNodeNum and a zone's air distribution unit inlets.
                if (SupAirPathNum == 0) {

                    for (CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {
                        if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).IsControlled) continue;
                        // Loop over the air distribution unit inlets for each controlled zone.
                        // Look for a match between the zone equip inlet node and the air distribution unit inlet node.
                        // When match found save the controlled zone number in CtrlZoneNumsCool or CtrlZoneNumsHeat
                        for (ZoneInNum = 1; ZoneInNum <= state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).NumInletNodes; ++ZoneInNum) {

                            // set supply air path flag
                            state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).AirDistUnitCool(ZoneInNum).SupplyAirPathExists = false;

                            if (ZoneSideNodeNum == state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).AirDistUnitCool(ZoneInNum).InNode) {
                                ++NumZonesCool;
                                // Set Duct Type for branch for dual duct
                                if (NumZonesCool == 1 && OutBranchNum > 1) {
                                    PrimaryAirSystems(AirLoopNum).Branch(OutBranchNum).DuctType = Cooling;
                                }
                                state.dataSimAirServingZones->CtrlZoneNumsCool(NumZonesCool) = CtrlZoneNum;
                                state.dataSimAirServingZones->ZoneInletNodesCool(NumZonesCool) =
                                    state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).InletNode(ZoneInNum);
                                state.dataSimAirServingZones->TermInletNodesCool(NumZonesCool) =
                                    state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).AirDistUnitCool(ZoneInNum).InNode;
                                state.dataSimAirServingZones->TermUnitSizingNumsCool(NumZonesCool) =
                                    state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).AirDistUnitCool(ZoneInNum).TermUnitSizingIndex;
                                if (state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).InletNodeAirLoopNum(ZoneInNum) == 0)
                                    state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).InletNodeAirLoopNum(ZoneInNum) = AirLoopNum;
                                goto ControlledZoneLoop2_exit;
                            }

                            if (ZoneSideNodeNum == state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).AirDistUnitHeat(ZoneInNum).InNode) {
                                ++NumZonesHeat;
                                // Set Duct Type for branch for dual duct
                                if (NumZonesHeat == 1 && OutBranchNum > 1) {
                                    PrimaryAirSystems(AirLoopNum).Branch(OutBranchNum).DuctType = Heating;
                                }
                                state.dataSimAirServingZones->CtrlZoneNumsHeat(NumZonesHeat) = CtrlZoneNum;
                                state.dataSimAirServingZones->ZoneInletNodesHeat(NumZonesHeat) =
                                    state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).InletNode(ZoneInNum);
                                state.dataSimAirServingZones->TermInletNodesHeat(NumZonesHeat) =
                                    state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).AirDistUnitHeat(ZoneInNum).InNode;
                                state.dataSimAirServingZones->TermUnitSizingNumsHeat(NumZonesHeat) =
                                    state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).AirDistUnitHeat(ZoneInNum).TermUnitSizingIndex;
                                if (state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).InletNodeAirLoopNum(ZoneInNum) == 0)
                                    state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).InletNodeAirLoopNum(ZoneInNum) = AirLoopNum;
                                goto ControlledZoneLoop2_exit;
                            }
                        }
                    }
                ControlledZoneLoop2_exit:;
                } // End of no supply air path case
            }

            // we now know the number of heated and cooled zones served by this primary air system.
            // Allocate the subarrays in AirToZoneNodeInfo
            AirToZoneNodeInfo(AirLoopNum).CoolCtrlZoneNums.allocate(NumZonesCool);
            AirToZoneNodeInfo(AirLoopNum).HeatCtrlZoneNums.allocate(NumZonesHeat);
            AirToZoneNodeInfo(AirLoopNum).CoolZoneInletNodes.allocate(NumZonesCool);
            AirToZoneNodeInfo(AirLoopNum).HeatZoneInletNodes.allocate(NumZonesHeat);
            AirToZoneNodeInfo(AirLoopNum).TermUnitCoolInletNodes.allocate(NumZonesCool);
            AirToZoneNodeInfo(AirLoopNum).TermUnitHeatInletNodes.allocate(NumZonesHeat);
            AirToZoneNodeInfo(AirLoopNum).TermUnitCoolSizingIndex.allocate(NumZonesCool);
            AirToZoneNodeInfo(AirLoopNum).TermUnitHeatSizingIndex.allocate(NumZonesHeat);
            // Move the controlled zone numbers from the scratch arrays into AirToZoneNodeInfo
            for (ZoneInSysIndex = 1; ZoneInSysIndex <= NumZonesCool; ++ZoneInSysIndex) {
                AirToZoneNodeInfo(AirLoopNum).CoolCtrlZoneNums(ZoneInSysIndex) = state.dataSimAirServingZones->CtrlZoneNumsCool(ZoneInSysIndex);
                AirToZoneNodeInfo(AirLoopNum).CoolZoneInletNodes(ZoneInSysIndex) = state.dataSimAirServingZones->ZoneInletNodesCool(ZoneInSysIndex);
                AirToZoneNodeInfo(AirLoopNum).TermUnitCoolInletNodes(ZoneInSysIndex) =
                    state.dataSimAirServingZones->TermInletNodesCool(ZoneInSysIndex);
                AirToZoneNodeInfo(AirLoopNum).TermUnitCoolSizingIndex(ZoneInSysIndex) =
                    state.dataSimAirServingZones->TermUnitSizingNumsCool(ZoneInSysIndex);
            }

            for (ZoneInSysIndex = 1; ZoneInSysIndex <= NumZonesHeat; ++ZoneInSysIndex) {
                AirToZoneNodeInfo(AirLoopNum).HeatCtrlZoneNums(ZoneInSysIndex) = state.dataSimAirServingZones->CtrlZoneNumsHeat(ZoneInSysIndex);
                AirToZoneNodeInfo(AirLoopNum).HeatZoneInletNodes(ZoneInSysIndex) = state.dataSimAirServingZones->ZoneInletNodesHeat(ZoneInSysIndex);
                AirToZoneNodeInfo(AirLoopNum).TermUnitHeatInletNodes(ZoneInSysIndex) =
                    state.dataSimAirServingZones->TermInletNodesHeat(ZoneInSysIndex);
                AirToZoneNodeInfo(AirLoopNum).TermUnitHeatSizingIndex(ZoneInSysIndex) =
                    state.dataSimAirServingZones->TermUnitSizingNumsHeat(ZoneInSysIndex);
            }

            AirToZoneNodeInfo(AirLoopNum).NumZonesCooled = NumZonesCool;
            AirToZoneNodeInfo(AirLoopNum).NumZonesHeated = NumZonesHeat;

            if ((NumZonesCool + NumZonesHeat) == 0) {
                ShowSevereError(state, "An outlet node in AirLoopHVAC=\"" + PrimaryAirSystems(AirLoopNum).Name + "\" is not connected to any zone");
                ShowContinueError(state,
                                  "Could not match ZoneEquipGroup Inlet Node=\"" + state.dataLoopNodes->NodeID(ZoneSideNodeNum) +
                                      "\" to any Supply Air Path or controlled zone");
                ErrorsFound = true;
            }

            // now fill the return air bypass information needed by the RAB setpoint manager
            if (PrimaryAirSystems(AirLoopNum).Splitter.Exists && PrimaryAirSystems(AirLoopNum).Mixer.Exists) {
                PrimaryAirSystems(AirLoopNum).RABExists = true;
                for (BranchNum = 1; BranchNum <= PrimaryAirSystems(AirLoopNum).NumBranches; ++BranchNum) {
                    // find the RAB branch; its inlet is a splitter outlet and it outlet is a mixer inlet
                    if ((PrimaryAirSystems(AirLoopNum).Branch(BranchNum).NodeNumIn == PrimaryAirSystems(AirLoopNum).Splitter.NodeNumOut(1) ||
                         PrimaryAirSystems(AirLoopNum).Branch(BranchNum).NodeNumIn == PrimaryAirSystems(AirLoopNum).Splitter.NodeNumOut(2)) &&
                        (PrimaryAirSystems(AirLoopNum).Branch(BranchNum).NodeNumOut == PrimaryAirSystems(AirLoopNum).Mixer.NodeNumIn(1) ||
                         PrimaryAirSystems(AirLoopNum).Branch(BranchNum).NodeNumOut == PrimaryAirSystems(AirLoopNum).Mixer.NodeNumIn(2)) &&
                        (PrimaryAirSystems(AirLoopNum).Branch(BranchNum).TotalComponents == 1) &&
                        (UtilityRoutines::SameString(PrimaryAirSystems(AirLoopNum).Branch(BranchNum).Comp(1).TypeOf, "Duct"))) {
                        // set the RAB splitter outlet node and the RAB mixer inlet node
                        PrimaryAirSystems(AirLoopNum).RABSplitOutNode = PrimaryAirSystems(AirLoopNum).Branch(BranchNum).NodeNumIn;
                        PrimaryAirSystems(AirLoopNum).RABMixInNode = PrimaryAirSystems(AirLoopNum).Branch(BranchNum).NodeNumOut;
                        // set the other nodes
                        if (PrimaryAirSystems(AirLoopNum).Splitter.NodeNumOut(1) == PrimaryAirSystems(AirLoopNum).RABSplitOutNode) {
                            PrimaryAirSystems(AirLoopNum).OtherSplitOutNode = PrimaryAirSystems(AirLoopNum).Splitter.NodeNumOut(2);
                        } else {
                            PrimaryAirSystems(AirLoopNum).OtherSplitOutNode = PrimaryAirSystems(AirLoopNum).Splitter.NodeNumOut(1);
                        }
                        if (PrimaryAirSystems(AirLoopNum).Mixer.NodeNumIn(1) == PrimaryAirSystems(AirLoopNum).RABMixInNode) {
                            PrimaryAirSystems(AirLoopNum).SupMixInNode = PrimaryAirSystems(AirLoopNum).Mixer.NodeNumIn(2);
                        } else {
                            PrimaryAirSystems(AirLoopNum).SupMixInNode = PrimaryAirSystems(AirLoopNum).Mixer.NodeNumIn(1);
                        }
                        // set the duct type
                        PrimaryAirSystems(AirLoopNum).Branch(BranchNum).DuctType = RAB;
                    }
                }
                PrimaryAirSystems(AirLoopNum).MixOutNode = PrimaryAirSystems(AirLoopNum).Mixer.NodeNumOut;
            }
        }

        // now fill out AirLoopZoneInfo for cleaner struct of zones attached to air loop, moved from MixedAir to here for use with Std. 62.1
        int MaxNumAirLoopZones = 0;
        for (int NumofAirLoop = 1; NumofAirLoop <= NumPrimaryAirSys; ++NumofAirLoop) {
            int NumAirLoopZones = AirToZoneNodeInfo(NumofAirLoop).NumZonesCooled + AirToZoneNodeInfo(NumofAirLoop).NumZonesHeated;
            // NumZonesCooled + NumZonesHeated must be > 0 or Fatal error is issued in SimAirServingZones
            MaxNumAirLoopZones = max(MaxNumAirLoopZones, NumAirLoopZones); // Max number of zones on any air loop being simulated
        }
        // Find the zones attached to each air loop
        for (int NumofAirLoop = 1; NumofAirLoop <= NumPrimaryAirSys; ++NumofAirLoop) {
            state.dataAirLoop->AirLoopZoneInfo(NumofAirLoop).Zone.allocate(MaxNumAirLoopZones);
            state.dataAirLoop->AirLoopZoneInfo(NumofAirLoop).ActualZoneNumber.allocate(MaxNumAirLoopZones);
            int NumAirLoopCooledZones = AirToZoneNodeInfo(NumofAirLoop).NumZonesCooled;
            int AirLoopZones = NumAirLoopCooledZones;
            int NumAirLoopHeatedZones = AirToZoneNodeInfo(NumofAirLoop).NumZonesHeated;
            // Store cooling zone numbers in AirLoopZoneInfo data structure
            for (int NumAirLoopCooledZonesTemp = 1; NumAirLoopCooledZonesTemp <= NumAirLoopCooledZones; ++NumAirLoopCooledZonesTemp) {
                state.dataAirLoop->AirLoopZoneInfo(NumofAirLoop).Zone(NumAirLoopCooledZonesTemp) =
                    AirToZoneNodeInfo(NumofAirLoop).CoolCtrlZoneNums(NumAirLoopCooledZonesTemp);
                state.dataAirLoop->AirLoopZoneInfo(NumofAirLoop).ActualZoneNumber(NumAirLoopCooledZonesTemp) =
                    state.dataZoneEquip->ZoneEquipConfig(AirToZoneNodeInfo(NumofAirLoop).CoolCtrlZoneNums(NumAirLoopCooledZonesTemp)).ActualZoneNum;
            }
            // Store heating zone numbers in AirLoopZoneInfo data structure
            // Only store zone numbers that aren't already defined as cooling zones above
            for (int NumAirLoopHeatedZonesTemp = 1; NumAirLoopHeatedZonesTemp <= NumAirLoopHeatedZones; ++NumAirLoopHeatedZonesTemp) {
                ZoneNum = AirToZoneNodeInfo(NumofAirLoop).HeatCtrlZoneNums(NumAirLoopHeatedZonesTemp);
                bool CommonZone = false;
                for (int NumAirLoopCooledZonesTemp = 1; NumAirLoopCooledZonesTemp <= NumAirLoopCooledZones; ++NumAirLoopCooledZonesTemp) {
                    if (ZoneNum != AirToZoneNodeInfo(NumofAirLoop).CoolCtrlZoneNums(NumAirLoopCooledZonesTemp)) continue;
                    CommonZone = true;
                }
                if (!CommonZone) {
                    ++AirLoopZones;
                    state.dataAirLoop->AirLoopZoneInfo(NumofAirLoop).Zone(AirLoopZones) = ZoneNum;
                    state.dataAirLoop->AirLoopZoneInfo(NumofAirLoop).ActualZoneNumber(AirLoopZones) =
                        state.dataZoneEquip->ZoneEquipConfig(ZoneNum).ActualZoneNum;
                }
            }
            state.dataAirLoop->AirLoopZoneInfo(NumofAirLoop).NumZones = AirLoopZones;
        }

        // now register zone inlet nodes as critical demand nodes in the convergence tracking
        state.dataConvergeParams->ZoneInletConvergence.allocate(state.dataGlobal->NumOfZones);
        for (ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
            if (state.dataZoneEquip->ZoneEquipConfig(ZoneNum).NumInletNodes > 0) {
                state.dataConvergeParams->ZoneInletConvergence(ZoneNum).NumInletNodes = state.dataZoneEquip->ZoneEquipConfig(ZoneNum).NumInletNodes;
                state.dataConvergeParams->ZoneInletConvergence(ZoneNum).InletNode.allocate(
                    state.dataZoneEquip->ZoneEquipConfig(ZoneNum).NumInletNodes);
                for (nodeLoop = 1; nodeLoop <= state.dataZoneEquip->ZoneEquipConfig(ZoneNum).NumInletNodes; ++nodeLoop) {
                    state.dataConvergeParams->ZoneInletConvergence(ZoneNum).InletNode(nodeLoop).NodeNum =
                        state.dataZoneEquip->ZoneEquipConfig(ZoneNum).InletNode(nodeLoop);
                }
            }
        }

        // now connect return nodes with airloops and corresponding inlet nodes
        ConnectReturnNodes(state);

        state.dataSimAirServingZones->InitAirLoopsOneTimeFlag = false;

        state.dataSimAirServingZones->CtrlZoneNumsCool.deallocate();
        state.dataSimAirServingZones->CtrlZoneNumsHeat.deallocate();
        state.dataSimAirServingZones->ZoneInletNodesCool.deallocate();
        state.dataSimAirServingZones->ZoneInletNodesHeat.deallocate();
        state.dataSimAirServingZones->TermInletNodesCool.deallocate();
        state.dataSimAirServingZones->TermInletNodesHeat.deallocate();
        state.dataSimAirServingZones->TermUnitSizingNumsCool.deallocate();
        state.dataSimAirServingZones->TermUnitSizingNumsHeat.deallocate();

        if (ErrorsFound) {
            ShowFatalError(state, "Preceding errors cause termination");
        }

        for (AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum) {

            SupFanIndex = 0;
            RetFanIndex = 0;
            FoundOASys = false;
            PrimaryAirSystems(AirLoopNum).FanDesCoolLoad = 0.0;
            FanModelType supFanModelType = FanModelType::Invalid;
            FanModelType retFanModelType = FanModelType::Invalid;

            bool FoundCentralCoolCoil = false;
            for (BranchNum = 1; BranchNum <= PrimaryAirSystems(AirLoopNum).NumBranches; ++BranchNum) {

                for (CompNum = 1; CompNum <= PrimaryAirSystems(AirLoopNum).Branch(BranchNum).TotalComponents; ++CompNum) {
                    CompTypeNum = PrimaryAirSystems(AirLoopNum).Branch(BranchNum).Comp(CompNum).CompType_Num;
                    if (PrimaryAirSystems(AirLoopNum).Branch(BranchNum).Comp(CompNum).CompType_Num == CompType::OAMixer_Num) {
                        FoundOASys = true;
                    }
                    if (CompTypeNum == CompType::WaterCoil_SimpleCool || CompTypeNum == CompType::WaterCoil_Cooling ||
                        CompTypeNum == CompType::WaterCoil_DetailedCool || CompTypeNum == CompType::WaterCoil_CoolingHXAsst ||
                        CompTypeNum == CompType::DXSystem) {
                        FoundCentralCoolCoil = true;
                    }
                    if (CompTypeNum == CompType::Fan_Simple_CV || CompTypeNum == CompType::Fan_Simple_VAV ||
                        CompTypeNum == CompType::Fan_ComponentModel) {
                        if (PrimaryAirSystems(AirLoopNum).OASysExists && !PrimaryAirSystems(AirLoopNum).isAllOA) {
                            if (FoundOASys) {
                                if (PrimaryAirSystems(AirLoopNum).Branch(BranchNum).DuctType != 3) {
                                    GetFanIndex(state, PrimaryAirSystems(AirLoopNum).Branch(BranchNum).Comp(CompNum).Name, SupFanIndex, ErrorsFound);
                                    supFanModelType = StructArrayLegacyFanModels;
                                    goto EndOfAirLoop;
                                }
                            } else {
                                GetFanIndex(state, PrimaryAirSystems(AirLoopNum).Branch(BranchNum).Comp(CompNum).Name, RetFanIndex, ErrorsFound);
                                retFanModelType = StructArrayLegacyFanModels;
                            }
                        } else {
                            GetFanIndex(state, PrimaryAirSystems(AirLoopNum).Branch(BranchNum).Comp(CompNum).Name, SupFanIndex, ErrorsFound);
                            supFanModelType = StructArrayLegacyFanModels;
                            goto EndOfAirLoop;
                        }
                    }
                    if (CompTypeNum == CompType::Fan_System_Object) {
                        if (PrimaryAirSystems(AirLoopNum).OASysExists && !PrimaryAirSystems(AirLoopNum).isAllOA) {
                            if (FoundOASys) {
                                if (PrimaryAirSystems(AirLoopNum).Branch(BranchNum).DuctType != 3) {
                                    SupFanIndex =
                                        HVACFan::getFanObjectVectorIndex(state, PrimaryAirSystems(AirLoopNum).Branch(BranchNum).Comp(CompNum).Name);
                                    supFanModelType = ObjectVectorOOFanSystemModel;
                                    goto EndOfAirLoop;
                                }
                            } else {
                                RetFanIndex =
                                    HVACFan::getFanObjectVectorIndex(state, PrimaryAirSystems(AirLoopNum).Branch(BranchNum).Comp(CompNum).Name);
                                retFanModelType = ObjectVectorOOFanSystemModel;
                            }
                        } else {
                            SupFanIndex = HVACFan::getFanObjectVectorIndex(state, PrimaryAirSystems(AirLoopNum).Branch(BranchNum).Comp(CompNum).Name);
                            supFanModelType = ObjectVectorOOFanSystemModel;
                            goto EndOfAirLoop;
                        }
                    }

                } // end of component loop

            } // end of Branch loop
        EndOfAirLoop:;

            if (supFanModelType == StructArrayLegacyFanModels) {
                PrimaryAirSystems(AirLoopNum).SupFanNum = SupFanIndex;
                PrimaryAirSystems(AirLoopNum).supFanModelType = StructArrayLegacyFanModels;
            } else if (supFanModelType == ObjectVectorOOFanSystemModel) {
                PrimaryAirSystems(AirLoopNum).supFanVecIndex = SupFanIndex;
                PrimaryAirSystems(AirLoopNum).supFanModelType = ObjectVectorOOFanSystemModel;
            }
            if (FoundCentralCoolCoil) { // parent systems with fan will need to set the fan placement
                PrimaryAirSystems(AirLoopNum).supFanLocation = FanPlacement::DrawThru;
            } else {
                PrimaryAirSystems(AirLoopNum).supFanLocation = FanPlacement::BlowThru;
            }

            if (retFanModelType == StructArrayLegacyFanModels) {
                PrimaryAirSystems(AirLoopNum).retFanModelType = StructArrayLegacyFanModels;
                PrimaryAirSystems(AirLoopNum).RetFanNum = RetFanIndex;
            } else if (retFanModelType == ObjectVectorOOFanSystemModel) {
                PrimaryAirSystems(AirLoopNum).retFanModelType = ObjectVectorOOFanSystemModel;
                PrimaryAirSystems(AirLoopNum).retFanVecIndex = RetFanIndex;
            }
        }
        // Check whether there are Central Heating Coils in the Primary Air System
        for (AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum) {
            bool FoundCentralHeatCoil = false;
            for (BranchNum = 1; !FoundCentralHeatCoil && BranchNum <= PrimaryAirSystems(AirLoopNum).NumBranches; ++BranchNum) {
                for (CompNum = 1; !FoundCentralHeatCoil && CompNum <= PrimaryAirSystems(AirLoopNum).Branch(BranchNum).TotalComponents; ++CompNum) {
                    CompTypeNum = PrimaryAirSystems(AirLoopNum).Branch(BranchNum).Comp(CompNum).CompType_Num;
                    if (CompTypeNum == CompType::WaterCoil_SimpleHeat || CompTypeNum == CompType::Coil_ElectricHeat ||
                        CompTypeNum == CompType::Coil_GasHeat || CompTypeNum == CompType::SteamCoil_AirHeat ||
                        CompTypeNum == CompType::Coil_DeSuperHeat || CompTypeNum == CompType::DXHeatPumpSystem ||
                        CompTypeNum == CompType::Furnace_UnitarySys_HeatOnly || CompTypeNum == CompType::Furnace_UnitarySys_HeatCool ||
                        CompTypeNum == CompType::UnitarySystem_BypassVAVSys || CompTypeNum == CompType::UnitarySystem_MSHeatPump ||
                        CompTypeNum == CompType::CoilUserDefined) {
                        FoundCentralHeatCoil = true;
                    } else if (CompTypeNum == CompType::UnitarySystemModel) {
                        // mine HeatCoilExists from UnitarySystem
                        std::string CompName = PrimaryAirSystems(AirLoopNum).Branch(BranchNum).Comp(CompNum).Name;
                        bool CoolingCoilExists = false;
                        bool HeatingCoilExists = false;
                        UnitarySystems::UnitarySys::getUnitarySysHeatCoolCoil(state, CompName, CoolingCoilExists, HeatingCoilExists, 0);
                        if (HeatingCoilExists) FoundCentralHeatCoil = true;
                    }
                } // end of component loop
            }     // end of Branch loop
            PrimaryAirSystems(AirLoopNum).CentralHeatCoilExists = FoundCentralHeatCoil;
        } // end of AirLoop loop

        // Check whether there are Central Cooling Coils in the Primary Air System
        for (AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum) {
            bool FoundCentralCoolCoil = false;
            for (BranchNum = 1; !FoundCentralCoolCoil && BranchNum <= PrimaryAirSystems(AirLoopNum).NumBranches; ++BranchNum) {
                for (CompNum = 1; !FoundCentralCoolCoil && CompNum <= PrimaryAirSystems(AirLoopNum).Branch(BranchNum).TotalComponents; ++CompNum) {
                    CompTypeNum = PrimaryAirSystems(AirLoopNum).Branch(BranchNum).Comp(CompNum).CompType_Num;
                    if (CompTypeNum == CompType::WaterCoil_SimpleCool || CompTypeNum == CompType::WaterCoil_Cooling ||
                        CompTypeNum == CompType::WaterCoil_DetailedCool || CompTypeNum == CompType::WaterCoil_CoolingHXAsst ||
                        CompTypeNum == CompType::DXCoil_CoolingHXAsst || CompTypeNum == CompType::DXSystem ||
                        CompTypeNum == CompType::Furnace_UnitarySys_HeatCool || CompTypeNum == CompType::UnitarySystem_BypassVAVSys ||
                        CompTypeNum == CompType::UnitarySystem_MSHeatPump || CompTypeNum == CompType::CoilUserDefined) {
                        FoundCentralCoolCoil = true;
                    } else if (CompTypeNum == CompType::UnitarySystemModel) {
                        // mine CoolHeat coil exists from UnitarySys
                        std::string CompName = PrimaryAirSystems(AirLoopNum).Branch(BranchNum).Comp(CompNum).Name;
                        bool CoolingCoilExists = false;
                        bool HeatingCoilExists = false;
                        UnitarySystems::UnitarySys::getUnitarySysHeatCoolCoil(state, CompName, CoolingCoilExists, HeatingCoilExists, 0);
                        if (CoolingCoilExists) FoundCentralCoolCoil = true;
                    }
                } // end of component loop
            }     // end of Branch loop
            PrimaryAirSystems(AirLoopNum).CentralCoolCoilExists = FoundCentralCoolCoil;
        } // end of AirLoop loop

    } // one time flag

    // Size the air loop branch air flows
    if (!state.dataGlobal->SysSizingCalc && state.dataSimAirServingZones->InitAirLoopsBranchSizingFlag) {

        for (AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum) {

            for (BranchNum = 1; BranchNum <= PrimaryAirSystems(AirLoopNum).NumBranches; ++BranchNum) {
                SizeAirLoopBranches(state, AirLoopNum, BranchNum);
            }
        }

        state.dataSimAirServingZones->InitAirLoopsBranchSizingFlag = false;

        // calculate the ratio of air loop design flow to the sum of the zone design flows
        for (AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum) {
            state.dataSimAirServingZones->SumZoneDesFlow = 0.0;
            state.dataAirLoop->AirLoopFlow(AirLoopNum).DesSupply = PrimaryAirSystems(AirLoopNum).DesignVolFlowRate * state.dataEnvrn->StdRhoAir;
            state.dataAirLoop->AirLoopFlow(AirLoopNum).DesReturnFrac = PrimaryAirSystems(AirLoopNum).DesignReturnFlowFraction;
            for (ZoneInSysIndex = 1; ZoneInSysIndex <= AirToZoneNodeInfo(AirLoopNum).NumZonesCooled; ++ZoneInSysIndex) {
                state.dataSimAirServingZones->TUInNode = AirToZoneNodeInfo(AirLoopNum).TermUnitCoolInletNodes(ZoneInSysIndex);
                state.dataSimAirServingZones->SumZoneDesFlow += state.dataLoopNodes->Node(state.dataSimAirServingZones->TUInNode).MassFlowRateMax;
            }
            if (state.dataSimAirServingZones->SumZoneDesFlow > VerySmallMassFlow) {
                state.dataAirLoop->AirLoopFlow(AirLoopNum).SysToZoneDesFlowRatio =
                    state.dataAirLoop->AirLoopFlow(AirLoopNum).DesSupply / state.dataSimAirServingZones->SumZoneDesFlow;
            } else {
                state.dataAirLoop->AirLoopFlow(AirLoopNum).SysToZoneDesFlowRatio = 1.0;
            }
        }

        for (int ZoneNum = 1; ZoneNum <= state.dataGlobal->NumOfZones; ++ZoneNum) {
            if (!state.dataZoneEquip->ZoneEquipConfig(ZoneNum).IsControlled) continue;
            // sets design supply air flow rate in the ZoneEquipConfig struct for use with zone air mass balance
            for (int returnNum = 1; returnNum <= state.dataZoneEquip->ZoneEquipConfig(ZoneNum).NumReturnNodes; ++returnNum) {
                int airLoop = state.dataZoneEquip->ZoneEquipConfig(ZoneNum).ReturnNodeAirLoopNum(returnNum);
                if (airLoop > 0) {
                    state.dataZoneEquip->ZoneEquipConfig(ZoneNum).AirLoopDesSupply = state.dataAirLoop->AirLoopFlow(airLoop).DesSupply;
                }
            }
        }
    }

    // Do the Begin Environment initializations
    if (state.dataGlobal->BeginEnvrnFlag && FirstHVACIteration && state.dataSimAirServingZones->MyEnvrnFlag) {

        if (NumPrimaryAirSys > 0) {
            for (auto &e : state.dataAirLoop->PriAirSysAvailMgr) {
                e.AvailStatus = NoAction;
                e.StartTime = 0;
                e.StopTime = 0;
            }
        }

        for (AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum) { // Start looping through all of the air loops...

            for (BranchNum = 1; BranchNum <= PrimaryAirSystems(AirLoopNum).NumBranches; ++BranchNum) { // loop over all branches in system
                for (NodeIndex = 1; NodeIndex <= PrimaryAirSystems(AirLoopNum).Branch(BranchNum).TotalNodes;
                     ++NodeIndex) { // loop over alll nodes on branch

                    NodeNum = PrimaryAirSystems(AirLoopNum).Branch(BranchNum).NodeNum(NodeIndex);

                    // Initialize the nodes to a standard set of initial conditions that will
                    //  change after the first iteration to a system value
                    state.dataLoopNodes->Node(NodeNum).Temp = 20.0;
                    state.dataLoopNodes->Node(NodeNum).HumRat = state.dataEnvrn->OutHumRat;
                    state.dataLoopNodes->Node(NodeNum).Enthalpy =
                        PsyHFnTdbW(state.dataLoopNodes->Node(NodeNum).Temp, state.dataLoopNodes->Node(NodeNum).HumRat);
                    // set the node mass flow rates to the airloop max mass flow rate
                    state.dataLoopNodes->Node(NodeNum).MassFlowRate = state.dataAirLoop->AirLoopFlow(AirLoopNum).DesSupply;
                    state.dataLoopNodes->Node(NodeNum).MassFlowRateMax = state.dataAirLoop->AirLoopFlow(AirLoopNum).DesSupply;
                    state.dataLoopNodes->Node(NodeNum).MassFlowRateMaxAvail = state.dataAirLoop->AirLoopFlow(AirLoopNum).DesSupply;
                    state.dataLoopNodes->Node(NodeNum).MassFlowRateMin = 0.0;
                    state.dataLoopNodes->Node(NodeNum).MassFlowRateSetPoint = 0.0;
                    state.dataLoopNodes->Node(NodeNum).MassFlowRateMinAvail = 0.0;
                    state.dataLoopNodes->Node(NodeNum).Press = state.dataEnvrn->StdBaroPress;
                    state.dataLoopNodes->Node(NodeNum).Quality = 0.0;
                    if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                        state.dataLoopNodes->Node(NodeNum).CO2 = state.dataContaminantBalance->OutdoorCO2;
                    }
                    if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                        state.dataLoopNodes->Node(NodeNum).GenContam = state.dataContaminantBalance->OutdoorGC;
                    }

                } // end of loop over nodes on each branch

            } // end of loop through branches in system

        } // end of loop over primary air systems
        state.dataSimAirServingZones->MyEnvrnFlag = false;

    } // End the environment initializations

    if (!state.dataGlobal->BeginEnvrnFlag) {
        state.dataSimAirServingZones->MyEnvrnFlag = true;
    }

    // Do the Begin Day initializations
    if (state.dataGlobal->BeginDayFlag) {
    }

    // There are no hourly initializations done in the heat balance

    // Do the following initializations (every time step).

    for (AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum) {
        // zero all MassFlowRateSetPoints
        for (BranchNum = 1; BranchNum <= PrimaryAirSystems(AirLoopNum).NumBranches; ++BranchNum) { // loop over all branches in system
            if (PrimaryAirSystems(AirLoopNum).Branch(BranchNum).DuctType == RAB) continue;
            for (NodeIndex = 1; NodeIndex <= PrimaryAirSystems(AirLoopNum).Branch(BranchNum).TotalNodes;
                 ++NodeIndex) { // loop over alll nodes on branch
                NodeNum = PrimaryAirSystems(AirLoopNum).Branch(BranchNum).NodeNum(NodeIndex);
                state.dataLoopNodes->Node(NodeNum).MassFlowRateSetPoint = 0.0;
                // Reset MassFlowRateMaxAvail at start of each HVAC simulation
                if (FirstHVACIteration) {
                    state.dataLoopNodes->Node(NodeNum).MassFlowRateMaxAvail = state.dataLoopNodes->Node(NodeNum).MassFlowRateMax;
                    state.dataLoopNodes->Node(NodeNum).MassFlowRateMinAvail = state.dataLoopNodes->Node(NodeNum).MassFlowRateMin;
                }
            }
        }

        // set the required flow (from zone equipment) at system outlet nodes
        for (OutNum = 1; OutNum <= PrimaryAirSystems(AirLoopNum).NumOutletBranches; ++OutNum) {
            OutBranchNum = PrimaryAirSystems(AirLoopNum).OutletBranchNum(OutNum);
            NodeNumOut = PrimaryAirSystems(AirLoopNum).Branch(OutBranchNum).NodeNumOut;
            ZoneSideNodeNum = AirToZoneNodeInfo(AirLoopNum).ZoneEquipSupplyNodeNum(OutNum);

            if (!FirstHVACIteration) {
                MassFlowSet = state.dataLoopNodes->Node(ZoneSideNodeNum).MassFlowRate;
            } else { // first time through in each HVAC timestep, use loop design mass flow rates for required mass flows
                MassFlowSet = state.dataAirLoop->AirLoopFlow(AirLoopNum).DesSupply;
            }
            // Need to make sure that flows are greater than zero
            if (MassFlowSet >= 0.0) {
                state.dataLoopNodes->Node(NodeNumOut).MassFlowRateSetPoint = MassFlowSet;
            } else if (MassFlowSet < 0.0) {
                state.dataLoopNodes->Node(NodeNumOut).MassFlowRateSetPoint = 0.0;
            }

            if (state.dataLoopNodes->Node(NodeNumOut).MassFlowRateSetPoint < state.dataSimAirServingZones->MassFlowSetToler) {
                state.dataLoopNodes->Node(NodeNumOut).MassFlowRateSetPoint = 0.0;
            }

            // Pass the required mass flow upstream to the start of each outlet branch
            for (BranchNodeIndex = PrimaryAirSystems(AirLoopNum).Branch(OutBranchNum).TotalNodes - 1; BranchNodeIndex >= 1; --BranchNodeIndex) {
                NodeNum = PrimaryAirSystems(AirLoopNum).Branch(OutBranchNum).NodeNum(BranchNodeIndex);
                if (PrimaryAirSystems(AirLoopNum).OASysExists && (NodeNum == PrimaryAirSystems(AirLoopNum).OASysInletNodeNum)) {
                    // need to modify if OA relief and supply not balanced because of exhaust fans
                    state.dataSimAirServingZones->OAReliefDiff =
                        state.dataLoopNodes->Node(PrimaryAirSystems(AirLoopNum).OASysOutletNodeNum).MassFlowRate -
                        state.dataLoopNodes->Node(NodeNum).MassFlowRate;
                    if (state.dataSimAirServingZones->OAReliefDiff > 0.0) {
                        state.dataLoopNodes->Node(NodeNum).MassFlowRateSetPoint =
                            state.dataLoopNodes->Node(NodeNumOut).MassFlowRateSetPoint - state.dataSimAirServingZones->OAReliefDiff;
                    } else {
                        state.dataLoopNodes->Node(NodeNum).MassFlowRateSetPoint = state.dataLoopNodes->Node(NodeNumOut).MassFlowRateSetPoint;
                    }
                } else {
                    state.dataLoopNodes->Node(NodeNum).MassFlowRateSetPoint = state.dataLoopNodes->Node(NodeNumOut).MassFlowRateSetPoint;
                }
            } // end loop over branch nodes

        } // end loop over outlet branches

        // [DC/LBNL] Initialize flag for current air loop
        AirLoopControlInfo(AirLoopNum).NewFlowRateFlag = false;

        // start each HVAC simulation at design air flow rate
        if (FirstHVACIteration) {
            // At each new HVAC iteration reset air loop converged flag to avoid attempting a warm restart
            // in SimAirLoop
            for (auto &e : AirLoopControlInfo)
                e.ConvergedFlag = false;

            for (InNum = 1; InNum <= PrimaryAirSystems(AirLoopNum).NumInletBranches; ++InNum) {
                InBranchNum = PrimaryAirSystems(AirLoopNum).InletBranchNum(InNum);
                if (InBranchNum == 0) {
                    ShowFatalError(state, "Missing Inlet Branch on Primary Air System=" + PrimaryAirSystems(AirLoopNum).Name);
                }
                NodeNumIn = PrimaryAirSystems(AirLoopNum).Branch(InBranchNum).NodeNumIn;

                // [DC/LBNL] Save previous mass flow rate
                MassFlowSaved = state.dataLoopNodes->Node(NodeNumIn).MassFlowRate;

                state.dataLoopNodes->Node(NodeNumIn).MassFlowRate = state.dataAirLoop->AirLoopFlow(AirLoopNum).DesSupply;

                // [DC/LBNL] Detect if air mass flow rate has changed since last air loop simulation
                if (state.dataLoopNodes->Node(NodeNumIn).MassFlowRate != MassFlowSaved) {
                    AirLoopControlInfo(AirLoopNum).NewFlowRateFlag = true;
                }

            } // end loop over inlet branches
            AirLoopControlInfo(AirLoopNum).EconoLockout = false;
        }
        // if a flow rate is specified for the loop use it here
        if (AirLoopControlInfo(AirLoopNum).LoopFlowRateSet && !FirstHVACIteration) {
            for (InNum = 1; InNum <= PrimaryAirSystems(AirLoopNum).NumInletBranches; ++InNum) {
                InBranchNum = PrimaryAirSystems(AirLoopNum).InletBranchNum(InNum);
                NodeNumIn = PrimaryAirSystems(AirLoopNum).Branch(InBranchNum).NodeNumIn;
                state.dataLoopNodes->Node(NodeNumIn).MassFlowRate =
                    state.dataAirLoop->AirLoopFlow(AirLoopNum).DesSupply * state.dataAirLoop->AirLoopFlow(AirLoopNum).ReqSupplyFrac -
                    (state.dataAirLoop->AirLoopFlow(AirLoopNum).SupFlow - state.dataAirLoop->AirLoopFlow(AirLoopNum).SysRetFlow);
            }
        }

    } // end loop over primary air systems
}

void ConnectReturnNodes(EnergyPlusData &state)
{
    // This initializes ZoneEquipConfig.ReturnNodeInletNum and ReturnNodeAirLoopNum
    // Search all return paths to match return nodes with the airloop they are connected to and find the corresponding zone inlet node (same zone,
    // same airloop)

    auto &AirToZoneNodeInfo(state.dataAirLoop->AirToZoneNodeInfo);
    auto &NumPrimaryAirSys = state.dataHVACGlobal->NumPrimaryAirSys;

    if (!state.dataZoneEquip->ZoneEquipInputsFilled) return;

    bool returnPathFound = false;
    // Loop over all controlled zones
    for (int ctrlZoneNum = 1; ctrlZoneNum <= state.dataGlobal->NumOfZones; ++ctrlZoneNum) {
        auto &thisZoneEquip(state.dataZoneEquip->ZoneEquipConfig(ctrlZoneNum));
        if (!thisZoneEquip.IsControlled) continue;
        // Loop over each return node for this zone
        for (int zoneOutNum = 1; zoneOutNum <= thisZoneEquip.NumReturnNodes; ++zoneOutNum) {
            returnPathFound = false;
            int airLoopNum = 0;
            int thisReturnNode = thisZoneEquip.ReturnNode(zoneOutNum);
            // Loop over all return paths
            for (int retPathNum = 1; retPathNum <= state.dataZoneEquip->NumReturnAirPaths; ++retPathNum) {
                auto const &thisRetPath(state.dataZoneEquip->ReturnAirPath(retPathNum));
                // Find which airloop this return path is on
                for (int sysNum = 1; sysNum <= NumPrimaryAirSys; ++sysNum) {
                    if (AirToZoneNodeInfo(sysNum).NumReturnNodes > 0) {
                        if (thisRetPath.OutletNodeNum == AirToZoneNodeInfo(sysNum).ZoneEquipReturnNodeNum(1)) {
                            airLoopNum = sysNum;
                            break;
                        }
                    }
                }
                // Loop over components in return path and each component's inlet nodes
                for (int compNum = 1; compNum <= thisRetPath.NumOfComponents; ++compNum) {
                    DataZoneEquipment::AirLoopHVACZone compType = thisRetPath.ComponentTypeEnum(compNum);
                    if (compType == DataZoneEquipment::AirLoopHVACZone::Mixer) {
                        auto const &thisMixer(state.dataMixerComponent->MixerCond(thisRetPath.ComponentIndex(compNum)));
                        for (int inNode = 1; inNode <= thisMixer.NumInletNodes; ++inNode) {
                            if (thisReturnNode == thisMixer.InletNode(inNode)) {
                                thisZoneEquip.ReturnNodeAirLoopNum(zoneOutNum) = airLoopNum; // set the return node airloop num
                                returnPathFound = true;
                                break; // leave component inlet node loop
                            }
                        }
                    } else if (compType == DataZoneEquipment::AirLoopHVACZone::ReturnPlenum) {
                        auto const &thisPlenum(state.dataZonePlenum->ZoneRetPlenCond(thisRetPath.ComponentIndex(compNum)));
                        for (int inNode = 1; inNode <= thisPlenum.NumInletNodes; ++inNode) {
                            if (thisReturnNode == thisPlenum.InletNode(inNode)) {
                                thisZoneEquip.ReturnNodeAirLoopNum(zoneOutNum) = airLoopNum; // set the return node airloop num
                                returnPathFound = true;
                                break; // leave component inlet node loop
                            }
                        }
                    }
                    if (returnPathFound) break; // leave return path component loop
                }
                if (returnPathFound) break; // leave return path loop
            }

            if (airLoopNum > 0) {
                // Find matching inlet node connected to the same air loop
                for (int inletNum = 1; inletNum <= thisZoneEquip.NumInletNodes; ++inletNum) {
                    if (thisZoneEquip.InletNodeAirLoopNum(inletNum) == airLoopNum) {
                        thisZoneEquip.ReturnNodeInletNum(zoneOutNum) = inletNum;
                        break;
                    }
                }
            }
        } // return nodes loop
    }     // controlled zones loop

    // Check for any air loops that may be connected directly to a zone return node
    for (int airLoopNum = 1; airLoopNum <= NumPrimaryAirSys; ++airLoopNum) {
        bool returnFound = false;
        if (AirToZoneNodeInfo(airLoopNum).NumReturnNodes > 0) {
            int zeqReturnNodeNum = AirToZoneNodeInfo(airLoopNum).ZoneEquipReturnNodeNum(1);
            if (zeqReturnNodeNum > 0) {
                for (int ctrlZoneNum = 1; ctrlZoneNum <= state.dataGlobal->NumOfZones; ++ctrlZoneNum) {
                    auto &thisZoneEquip(state.dataZoneEquip->ZoneEquipConfig(ctrlZoneNum));
                    if (!thisZoneEquip.IsControlled) continue;
                    for (int zoneOutNum = 1; zoneOutNum <= thisZoneEquip.NumReturnNodes; ++zoneOutNum) {
                        if (thisZoneEquip.ReturnNode(zoneOutNum) == zeqReturnNodeNum) {
                            thisZoneEquip.ReturnNodeAirLoopNum(zoneOutNum) = airLoopNum;
                            returnFound = true;
                            // Find matching inlet node connected to the same air loop
                            for (int inletNum = 1; inletNum <= thisZoneEquip.NumInletNodes; ++inletNum) {
                                if (thisZoneEquip.InletNodeAirLoopNum(inletNum) == airLoopNum) {
                                    thisZoneEquip.ReturnNodeInletNum(zoneOutNum) = inletNum;
                                    break;
                                }
                            }
                            break; // leave zone return node loop
                        }
                        if (returnFound) break; // leave controlled zone loop
                    }
                }
            }
        }
    }
}

void SimAirLoops(EnergyPlusData &state, bool const FirstHVACIteration, bool &SimZoneEquipment)
{

    // SUBROUTINE INFORMATION
    //             AUTHOR:  Russ Taylor, Dan Fisher, Fred Buhl
    //       DATE WRITTEN:  Oct 1997
    //           MODIFIED:  Dec 1997 Fred Buhl
    //           MODIFIED:  Apr 1998 Richard Liesen
    //           MODIFIED:  Dec 1999 Fred Buhl
    //           MODIFIED:  Feb 2006 Dimitri Curtil (LBNL)
    //                      - Moved air loop simulation to SimAirLoop() routine.
    //      RE-ENGINEERED:  This is new code, not reengineered

    // PURPOSE OF THIS SUBROUTINE:
    // This is the driver subroutine for the air loop simulation. It simulates
    // each primary air system in the problem and passes the outlet node conditions
    // on to the attached Zone Equipment inlet nodes.

    // METHODOLOGY EMPLOYED:
    // For each primary air system:
    // (1) each component in the system is simulated in natural order, beginning at
    //     the return air inlet and progressing to the supply air outlets. Node data
    //     is passed in the same direction.
    // (2) The controllers and their actions are simulated.
    // (3) Steps 2 and 3 are repeated until the control criteria are satisfied.
    // (4) A mass balance check is performed; if it fails, mass balance is imposed
    //     and steps 1, 2, and 3 are repeated. At the end we should have a correct,
    //     self consistent primary air system simulation.

    // REFERENCES: None

    // Using/Aliasing
    using General::GetPreviousHVACTime;
    using HVACInterfaceManager::UpdateHVACInterface;

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:
    // TRUE if first full HVAC iteration in an HVAC timestep
    // TRUE if Zone Equipment needs to be resimulated.

    // SUBROUTINE LOCAL VARIABLE DEFINITIONS
    Real64 rxTime;
    // Primary Air Sys DO loop index
    int AirLoopNum;
    // Max number of iterations performed by controllers on each air loop
    int AirLoopIterMax;
    // Aggregated number of iterations across all controllers on each air loop
    int AirLoopIterTot;
    // Total number of times SimAirLoopComponents() has been invoked to simulate each air loop
    int AirLoopNumCalls;
    // Primary air system outlet DO loop index
    int AirSysOutNum;
    // DO loop index; there are 2 passes - the 2nd is done only if mass balance fails
    int AirLoopPass;
    // Flag set by ResolveSysFlow; if TRUE, mass balance failed and there must be a second pass
    bool SysReSim;
    DataConvergParams::CalledFrom CalledFrom;

    auto &AirToZoneNodeInfo(state.dataAirLoop->AirToZoneNodeInfo);
    auto &AirLoopControlInfo(state.dataAirLoop->AirLoopControlInfo);

    // Set up output variables
    if (!state.dataSimAirServingZones->OutputSetupFlag) {
        SetupOutputVariable(state,
                            "Air System Simulation Maximum Iteration Count",
                            OutputProcessor::Unit::None,
                            state.dataSimAirServingZones->salIterMax,
                            OutputProcessor::SOVTimeStepType::HVAC,
                            OutputProcessor::SOVStoreType::Summed,
                            "SimAir");
        SetupOutputVariable(state,
                            "Air System Simulation Iteration Count",
                            OutputProcessor::Unit::None,
                            state.dataSimAirServingZones->salIterTot,
                            OutputProcessor::SOVTimeStepType::HVAC,
                            OutputProcessor::SOVStoreType::Summed,
                            "SimAir");
        SetupOutputVariable(state,
                            "Air System Component Model Simulation Calls",
                            OutputProcessor::Unit::None,
                            state.dataSimAirServingZones->NumCallsTot,
                            OutputProcessor::SOVTimeStepType::HVAC,
                            OutputProcessor::SOVStoreType::Summed,
                            "SimAir");
        state.dataSimAirServingZones->OutputSetupFlag = true;
    }

    // BUG: IterMax should not be aggregated as a Sum output variable
    //      We need a new aggregation scheme to track the max value across HVAC steps
    //      instead of summing it up.
    state.dataSimAirServingZones->salIterMax = 0;

    // Reset counters to capture statistics for the current zone time step
    // Aggregate statistics over all HVAC time steps, even the rejected ones, to properly
    // reflect the numerical work. The condition to detect a new HVAC time step is essentially
    // based on the time stamp at the beginning of the current HVAC step (expressed in seconds).
    if (FirstHVACIteration) {
        rxTime = GetPreviousHVACTime(state);
        if (state.dataSimAirServingZones->SavedPreviousHVACTime != rxTime) {
            state.dataSimAirServingZones->SavedPreviousHVACTime = rxTime;
            state.dataSimAirServingZones->salIterTot = 0;
            state.dataSimAirServingZones->NumCallsTot = 0;
        }
    }

    // Loop over all the primary air loop; simulate their components (equipment)
    // and controllers
    for (AirLoopNum = 1; AirLoopNum <= state.dataHVACGlobal->NumPrimaryAirSys; ++AirLoopNum) { // NumPrimaryAirSys is the number of primary air loops

        // Check to see if System Availability Managers are asking for fans to cycle on or shut off
        // and set fan on/off flags accordingly.
        state.dataHVACGlobal->TurnFansOn = false;
        state.dataHVACGlobal->TurnFansOff = false;
        state.dataHVACGlobal->NightVentOn = false;
        if (state.dataAirLoop->PriAirSysAvailMgr(AirLoopNum).AvailStatus == CycleOn) {
            state.dataHVACGlobal->TurnFansOn = true;
        }
        if (state.dataAirLoop->PriAirSysAvailMgr(AirLoopNum).AvailStatus == ForceOff) {
            state.dataHVACGlobal->TurnFansOff = true;
        }
        if (AirLoopControlInfo(AirLoopNum).NightVent) {
            state.dataHVACGlobal->NightVentOn = true;
        }

        //   Set current system number for sizing routines
        state.dataSize->CurSysNum = AirLoopNum;

        // 2 passes; 1 usually suffices; 2 is done if ResolveSysFlow detects a failure of mass balance
        for (AirLoopPass = 1; AirLoopPass <= 2; ++AirLoopPass) {

            SysReSim = false;
            AirLoopControlInfo(AirLoopNum).AirLoopPass = AirLoopPass; // save for use without passing as argument

            // Simulate controllers on air loop with current air mass flow rates
            SimAirLoop(state, FirstHVACIteration, AirLoopNum, AirLoopPass, AirLoopIterMax, AirLoopIterTot, AirLoopNumCalls);

            // Update tracker for maximum number of iterations needed by any controller on all air loops
            state.dataSimAirServingZones->salIterMax = max(state.dataSimAirServingZones->salIterMax, AirLoopIterMax);
            // Update tracker for aggregated number of iterations needed by all controllers on all air loops
            state.dataSimAirServingZones->salIterTot += AirLoopIterTot;
            // Update tracker for total number of times SimAirLoopComponents() has been invoked across all air loops
            state.dataSimAirServingZones->NumCallsTot += AirLoopNumCalls;

            // At the end of the first pass, check whether a second pass is needed or not
            if (AirLoopPass == 1) {
                // If simple system, skip second pass
                if (AirLoopControlInfo(AirLoopNum).Simple) break;
                ResolveSysFlow(state, AirLoopNum, SysReSim);
                // If mass balance OK, skip second pass
                if (!SysReSim) break;
            }
        }

        // Air system side has been simulated, now transfer conditions across to
        // the zone equipment side, looping through all supply air paths for this
        // air loop.
        for (AirSysOutNum = 1; AirSysOutNum <= AirToZoneNodeInfo(AirLoopNum).NumSupplyNodes; ++AirSysOutNum) {
            if (AirSysOutNum == 1) CalledFrom = DataConvergParams::CalledFrom::AirSystemSupplySideDeck1;
            if (AirSysOutNum == 2) CalledFrom = DataConvergParams::CalledFrom::AirSystemSupplySideDeck2;
            UpdateHVACInterface(state,
                                AirLoopNum,
                                CalledFrom,
                                AirToZoneNodeInfo(AirLoopNum).AirLoopSupplyNodeNum(AirSysOutNum),
                                AirToZoneNodeInfo(AirLoopNum).ZoneEquipSupplyNodeNum(AirSysOutNum),
                                SimZoneEquipment);
        } // ...end of DO loop over supply air paths for this air loop.

    } // End of Air Loop iteration

    if (state.dataAirLoopHVACDOAS->airloopDOAS.size() > 0) {
        int index;
        Real64 OAMassFLowrate = 0.0;
        for (std::size_t loop = 0; loop < state.dataAirLoopHVACDOAS->airloopDOAS.size(); ++loop) {
            auto &thisAirLoopDOASObjec = state.dataAirLoopHVACDOAS->airloopDOAS[loop]; // <- regular reference variable, not a pointer
            if (thisAirLoopDOASObjec.m_AirLoopDOASNum > -1) {
                index = thisAirLoopDOASObjec.m_AirLoopDOASNum;
            } else {
                index = -1;
            }
            thisAirLoopDOASObjec.SimAirLoopHVACDOAS(state, FirstHVACIteration, index);
            OAMassFLowrate += thisAirLoopDOASObjec.SumMassFlowRate;
        }

        if (OAMassFLowrate > 0.0) {
            for (AirLoopNum = 1; AirLoopNum <= state.dataHVACGlobal->NumPrimaryAirSys;
                 ++AirLoopNum) { // NumPrimaryAirSys is the number of primary air loops
                state.dataHVACGlobal->TurnFansOn = false;
                state.dataHVACGlobal->TurnFansOff = false;
                state.dataHVACGlobal->NightVentOn = false;
                if (state.dataAirLoop->PriAirSysAvailMgr(AirLoopNum).AvailStatus == CycleOn) {
                    state.dataHVACGlobal->TurnFansOn = true;
                }
                if (state.dataAirLoop->PriAirSysAvailMgr(AirLoopNum).AvailStatus == ForceOff) {
                    state.dataHVACGlobal->TurnFansOff = true;
                }
                if (AirLoopControlInfo(AirLoopNum).NightVent) {
                    state.dataHVACGlobal->NightVentOn = true;
                }

                //   Set current system number for sizing routines
                state.dataSize->CurSysNum = AirLoopNum;

                // 2 passes; 1 usually suffices; 2 is done if ResolveSysFlow detects a failure of mass balance
                for (AirLoopPass = 1; AirLoopPass <= 2; ++AirLoopPass) {

                    SysReSim = false;
                    AirLoopControlInfo(AirLoopNum).AirLoopPass = AirLoopPass; // save for use without passing as argument

                    // Simulate controllers on air loop with current air mass flow rates
                    SimAirLoop(state, FirstHVACIteration, AirLoopNum, AirLoopPass, AirLoopIterMax, AirLoopIterTot, AirLoopNumCalls);

                    // Update tracker for maximum number of iterations needed by any controller on all air loops
                    state.dataSimAirServingZones->salIterMax = max(state.dataSimAirServingZones->salIterMax, AirLoopIterMax);
                    // Update tracker for aggregated number of iterations needed by all controllers on all air loops
                    state.dataSimAirServingZones->salIterTot += AirLoopIterTot;
                    // Update tracker for total number of times SimAirLoopComponents() has been invoked across all air loops
                    state.dataSimAirServingZones->NumCallsTot += AirLoopNumCalls;

                    // At the end of the first pass, check whether a second pass is needed or not
                    if (AirLoopPass == 1) {
                        // If simple system, skip second pass
                        if (AirLoopControlInfo(AirLoopNum).Simple) break;
                        ResolveSysFlow(state, AirLoopNum, SysReSim);
                        // If mass balance OK, skip second pass
                        if (!SysReSim) break;
                    }
                }

                // Air system side has been simulated, now transfer conditions across to
                // the zone equipment side, looping through all supply air paths for this
                // air loop.
                for (AirSysOutNum = 1; AirSysOutNum <= AirToZoneNodeInfo(AirLoopNum).NumSupplyNodes; ++AirSysOutNum) {
                    if (AirSysOutNum == 1) CalledFrom = DataConvergParams::CalledFrom::AirSystemSupplySideDeck1;
                    if (AirSysOutNum == 2) CalledFrom = DataConvergParams::CalledFrom::AirSystemSupplySideDeck2;
                    UpdateHVACInterface(state,
                                        AirLoopNum,
                                        CalledFrom,
                                        AirToZoneNodeInfo(AirLoopNum).AirLoopSupplyNodeNum(AirSysOutNum),
                                        AirToZoneNodeInfo(AirLoopNum).ZoneEquipSupplyNodeNum(AirSysOutNum),
                                        SimZoneEquipment);
                } // ...end of DO loop over supply air paths for this air loop.

            } // End of Air Loop iteration
            // check convergence at the mixer outlet or at the AirLoopDOAS outlet
            AirLoopHVACDOAS::CheckConvergence(state);
        }
    }
    // Reset current system number for sizing routines
    state.dataSize->CurSysNum = 0;
}

void SimAirLoop(EnergyPlusData &state,
                bool const FirstHVACIteration,
                int const AirLoopNum,
                int const AirLoopPass,
                int &AirLoopIterMax,
                int &AirLoopIterTot,
                int &AirLoopNumCalls)
{

    // SUBROUTINE INFORMATION
    //             AUTHOR:  Dimitri Curtil (LBNL)
    //       DATE WRITTEN:  March 2006
    //                      - Fine-tuned outer loop over controllers.
    //                      - Added convergence tracing for air loop controllers.
    //                      - Added mechanism for speculative warm restart after first iteration.
    //      RE-ENGINEERED:  This is new code based on the code that used to be part
    //                      of SimAirLoops().

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine simulates the desired air loop by solving for all the
    // controllers on the air loop in the order they are specified.

    // METHODOLOGY EMPLOYED:
    // To speed up the simulation, we introduced the possiblity to perform the controller
    // simulation on each air loop using a warm restart from the solution obtained
    // at the previous HVAC step iteration. This is only attempted if the air mass flow
    // rate(s) for the air system have not changed since the last iteration.
    // Of course if the warm restart fails, then we perform a normal simulation from
    // a cold start. We refer to this scheme as speculative warm restart.

    // REFERENCES: None

    // Using/Aliasing
    using namespace DataHVACControllers;
    using namespace DataSystemVariables;
    using General::CreateSysTimeIntervalString;
    using HVACControllers::TraceAirLoopControllers;
    using HVACControllers::TrackAirLoopControllers;

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:
    // TRUE if first full HVAC iteration in an HVAC timestep
    // Index of the air loop to simulate
    // There are 2 passes - the 2nd is done only if mass balance fails
    // Max number of iterations performed by controllers across this air loop
    // Aggregated number of iterations across all controllers on this air loop
    // Total number of times SimAirLoopComponents() has been invoked to simulate this air loop

    // SUBROUTINE LOCAL VARIABLE DEFINITIONS

    auto &PrimaryAirSystems(state.dataAirSystemsData->PrimaryAirSystems);
    auto &AirLoopControlInfo(state.dataAirLoop->AirLoopControlInfo);

    // Reset air loop trackers to zero
    AirLoopIterMax = 0;
    AirLoopIterTot = 0;
    AirLoopNumCalls = 0;

    // Perform air loop simulation to satisfy convergence for all controllers
    // If first HVAC iteration or new air flow we force a cold restart.
    // Otherwise we attempt a speculative warm restart.
    // TODO: Improve detection of when air flow rate has changed since last air loop simulation
    // TODO: Detect whether warm restart is supported on air loop on very first air loop
    //       simulation only instead of at each HVAC iteration as done now.
    // Only enabled if there are controllers on the air loop
    // Check that the speculative warm restart feature is allowed
    // Never done at first HVAC iteration
    // Never done during sizing
    // Next condition is true whenever the final check for the air loop was converged
    // at the previous SimAirLoop call
    // Next conditions should detect when air mass flow rates have changed
    state.dataSimAirServingZones->DoWarmRestartFlagSAL =
        PrimaryAirSystems(AirLoopNum).NumControllers > 0 && AirLoopControlInfo(AirLoopNum).AllowWarmRestartFlag && !FirstHVACIteration &&
        !state.dataGlobal->SysSizingCalc && AirLoopControlInfo(AirLoopNum).ConvergedFlag && !AirLoopControlInfo(AirLoopNum).LoopFlowRateSet &&
        !AirLoopControlInfo(AirLoopNum).NewFlowRateFlag;

    if (!state.dataSimAirServingZones->DoWarmRestartFlagSAL) {
        // Solve controllers with cold start using default initial values
        SolveAirLoopControllers(state,
                                FirstHVACIteration,
                                AirLoopNum,
                                state.dataSimAirServingZones->AirLoopConvergedFlagSAL,
                                state.dataSimAirServingZones->IterMaxSAL2,
                                state.dataSimAirServingZones->IterTotSAL2,
                                state.dataSimAirServingZones->NumCallsSAL2);

        // Update air loop trackers
        state.dataSimAirServingZones->WarmRestartStatusSAL = ControllerWarmRestart::None;
        AirLoopNumCalls += state.dataSimAirServingZones->NumCallsSAL2;
        AirLoopIterMax = max(AirLoopIterMax, state.dataSimAirServingZones->IterMaxSAL2);
        AirLoopIterTot += state.dataSimAirServingZones->IterTotSAL2;
    } else {
        // First try with speculative warm restart using previous solution
        ReSolveAirLoopControllers(state,
                                  FirstHVACIteration,
                                  AirLoopNum,
                                  state.dataSimAirServingZones->AirLoopConvergedFlagSAL,
                                  state.dataSimAirServingZones->IterMaxSAL2,
                                  state.dataSimAirServingZones->IterTotSAL2,
                                  state.dataSimAirServingZones->NumCallsSAL2);

        // Update air loop trackers
        state.dataSimAirServingZones->WarmRestartStatusSAL = ControllerWarmRestart::Success;
        AirLoopNumCalls += state.dataSimAirServingZones->NumCallsSAL2;
        AirLoopIterMax = max(AirLoopIterMax, state.dataSimAirServingZones->IterMaxSAL2);
        AirLoopIterTot += state.dataSimAirServingZones->IterTotSAL2;

        // Retry with cold start using default initial values if speculative warm restart did not work
        if (!state.dataSimAirServingZones->AirLoopConvergedFlagSAL) {
            SolveAirLoopControllers(state,
                                    FirstHVACIteration,
                                    AirLoopNum,
                                    state.dataSimAirServingZones->AirLoopConvergedFlagSAL,
                                    state.dataSimAirServingZones->IterMaxSAL2,
                                    state.dataSimAirServingZones->IterTotSAL2,
                                    state.dataSimAirServingZones->NumCallsSAL2);

            // Update air loop trackers
            state.dataSimAirServingZones->WarmRestartStatusSAL = DataHVACControllers::ControllerWarmRestart::Fail;
            AirLoopNumCalls += state.dataSimAirServingZones->NumCallsSAL2;
            AirLoopIterMax = max(AirLoopIterMax, state.dataSimAirServingZones->IterMaxSAL2);
            AirLoopIterTot += state.dataSimAirServingZones->IterTotSAL2;
        }
    }

    // Updates air loop statistics
    // To enable runtime statistics tracking for each air loop, define the environment variable
    // TRACK_AIRLOOP=YES or TRACK_AIRLOOP=Y
    if (state.dataSysVars->TrackAirLoopEnvFlag) {
        TrackAirLoopControllers(
            state, AirLoopNum, state.dataSimAirServingZones->WarmRestartStatusSAL, AirLoopIterMax, AirLoopIterTot, AirLoopNumCalls);
    }

    // Generate trace for all controllers on this air loop
    // To enable generating a trace file with the converged solution for all controllers on each air loop,
    // define the environment variable TRACE_AIRLOOP=YES or TRACE_AIRLOOP=Y.
    if (state.dataSysVars->TraceAirLoopEnvFlag) {
        TraceAirLoopControllers(
            state, FirstHVACIteration, AirLoopNum, AirLoopPass, state.dataSimAirServingZones->AirLoopConvergedFlagSAL, AirLoopNumCalls);
    }

    // When there is more than 1 controller on an air loop, each controller sensing
    // different nodes with potentially different setpoints, it is likely that
    // AirLoopConvergedFlag will be false as the individual setpoints will not
    // be satisfied once all the controllers have been simulated. Typically, this could
    // happen if
    // If this is the case then we do not want to try a warm restart as it is very
    // unlikely to succeed.
    AirLoopControlInfo(AirLoopNum).ConvergedFlag = state.dataSimAirServingZones->AirLoopConvergedFlagSAL;
}

void SolveAirLoopControllers(
    EnergyPlusData &state, bool const FirstHVACIteration, int const AirLoopNum, bool &AirLoopConvergedFlag, int &IterMax, int &IterTot, int &NumCalls)
{

    // SUBROUTINE INFORMATION
    //             AUTHOR:  Dimitri Curtil (LBNL)
    //       DATE WRITTEN:  Feb 2006
    //           MODIFIED:
    //      RE-ENGINEERED:  This is reengineered code that used to be in SimAirLoops()

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine solves for the controllers on the specfied air loop assuming a cold start.

    // METHODOLOGY EMPLOYED:
    // For the specified primary air system:
    // (1) each component in the system is simulated in natural order, beginning at
    //     the return air inlet and progressing to the supply air outlets. Node data
    //     is passed in the same direction.
    // (2) The controllers and their actions are simulated.
    // (3) Steps 2 and 3 are repeated until the control criteria are satisfied.

    // REFERENCES: None

    // Using/Aliasing
    using namespace DataHVACControllers;
    using General::CreateSysTimeIntervalString;
    using HVACControllers::ManageControllers;

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:
    // TRUE if first full HVAC iteration in an HVAC timestep
    // DO loop index; there are 2 passes the 2nd is done only if mass balance fails
    // Index of the air loop being simulated
    // TRUE when primary air system & controllers simulation has converged;
    // Max number of iterations performed by controllers across this air loop
    // Aggregated number of iterations across all controllers on this air loop
    // Total number of times SimAirLoopComponents() has been invoked

    // SUBROUTINE PARAMETER DEFINITIONS:
    // Maximum iterations of an air system/controllers simulation sequence
    int constexpr MaxIter(50);

    // INTERFACE BLOCK DEFINITIONS: None

    // DERIVED TYPE DEFINITIONS: None

    // SUBROUTINE LOCAL VARIABLE DEFINITIONS
    // TRUE if controller supports speculative warm restart
    bool AllowWarmRestartFlag;
    // TRUE when controller has converged
    bool ControllerConvergedFlag;
    // TRUE when air loop has been evaluated with latest actuated variables
    bool IsUpToDateFlag;

    auto &PrimaryAirSystems(state.dataAirSystemsData->PrimaryAirSystems);
    auto &AirLoopControlInfo(state.dataAirLoop->AirLoopControlInfo);

    // To track number of calls to SimAirLoopComponents() for each air loop
    // Represents the most computationally expensive operation in the iteration.
    // Best metric to use to assess the runtime performance of air loop simulation
    NumCalls = 0;
    IterMax = 0;
    IterTot = 0;

    AirLoopConvergedFlag = true;
    state.dataSimAirServingZones->BypassOAControllerSALC = true; // don't simulate OA contollers at this time (see SolveWaterCoilController)
    IsUpToDateFlag = false;
    PrimaryAirSystems(AirLoopNum).ControlConverged = false;

    AllowWarmRestartFlag = true;
    AirLoopControlInfo(AirLoopNum).AllowWarmRestartFlag = true;

    if (PrimaryAirSystems(AirLoopNum).SizeAirloopCoil) { // one time flag to initialize controller index and size coils if needed
        // Loop through the controllers first to set the controller index in the PrimaryAirSystem array.
        // Need to actaully simulate controller to get controller index.
        for (int AirLoopControlNum = 1; AirLoopControlNum <= PrimaryAirSystems(AirLoopNum).NumControllers; ++AirLoopControlNum) {
            PrimaryAirSystems(AirLoopNum).ControllerIndex(AirLoopControlNum) =
                HVACControllers::GetControllerIndex(state, PrimaryAirSystems(AirLoopNum).ControllerName(AirLoopControlNum));
            state.dataHVACControllers->ControllerProps(PrimaryAirSystems(AirLoopNum).ControllerIndex(AirLoopControlNum)).AirLoopControllerIndex =
                AirLoopControlNum;
        }
        // When using controllers, size air loop coils so ControllerProps (e.g., Min/Max Actuated) can be set
        if (PrimaryAirSystems(AirLoopNum).NumControllers > 0) SimAirLoopComponents(state, AirLoopNum, FirstHVACIteration);
        PrimaryAirSystems(AirLoopNum).SizeAirloopCoil = false;
    }

    // This call to ManageControllers reinitializes the controllers actuated variables to zero
    // E.g., actuator inlet water flow
    for (int AirLoopControlNum = 1; AirLoopControlNum <= PrimaryAirSystems(AirLoopNum).NumControllers; ++AirLoopControlNum) {

        // BypassOAController is true here since we do not want to simulate the controller if it has already been simulated in the OA system
        // ControllerConvergedFlag is returned true here for water coils in OA system
        ManageControllers(state,
                          PrimaryAirSystems(AirLoopNum).ControllerName(AirLoopControlNum),
                          PrimaryAirSystems(AirLoopNum).ControllerIndex(AirLoopControlNum),
                          FirstHVACIteration,
                          AirLoopNum,
                          ControllerOperation::ColdStart,
                          ControllerConvergedFlag,
                          IsUpToDateFlag,
                          state.dataSimAirServingZones->BypassOAControllerSALC,
                          AllowWarmRestartFlag);
        // Detect whether the speculative warm restart feature is supported by each controller
        // on this air loop.
        AirLoopControlInfo(AirLoopNum).AllowWarmRestartFlag = AirLoopControlInfo(AirLoopNum).AllowWarmRestartFlag && AllowWarmRestartFlag;
    }

    // Evaluate air loop components with new actuated variables
    ++NumCalls;
    SimAirLoopComponents(state, AirLoopNum, FirstHVACIteration);
    IsUpToDateFlag = true;

    // Loop over the air sys controllers until convergence or MaxIter iterations
    for (int AirLoopControlNum = 1; AirLoopControlNum <= PrimaryAirSystems(AirLoopNum).NumControllers; ++AirLoopControlNum) {

        state.dataSimAirServingZones->IterSALC = 0;
        ControllerConvergedFlag = false;
        // if the controller can be locked out by the economizer operation and the economizer is active, leave the controller inactive
        if (AirLoopControlInfo(AirLoopNum).EconoActive) {
            // nesting this next if to try and speed this up. If economizer is not active, it doesn't matter if CanBeLockedOutByEcono = true
            if (PrimaryAirSystems(AirLoopNum).CanBeLockedOutByEcono(AirLoopControlNum)) {
                ControllerConvergedFlag = true;
                continue;
            }
        }

        // For each controller in sequence, iterate until convergence
        while (!ControllerConvergedFlag) {

            ++state.dataSimAirServingZones->IterSALC;

            ManageControllers(state,
                              PrimaryAirSystems(AirLoopNum).ControllerName(AirLoopControlNum),
                              PrimaryAirSystems(AirLoopNum).ControllerIndex(AirLoopControlNum),
                              FirstHVACIteration,
                              AirLoopNum,
                              ControllerOperation::Iterate,
                              ControllerConvergedFlag,
                              IsUpToDateFlag,
                              state.dataSimAirServingZones->BypassOAControllerSALC);

            PrimaryAirSystems(AirLoopNum).ControlConverged(AirLoopControlNum) = ControllerConvergedFlag;

            if (!ControllerConvergedFlag) {
                // Only check abnormal termination if not yet converged
                // The iteration counter has been exceeded.
                if (state.dataSimAirServingZones->IterSALC > MaxIter) {
                    // Indicate that this air loop is not converged
                    AirLoopConvergedFlag = false;

                    // The warning message will be suppressed during the warm up days.
                    if (!state.dataGlobal->WarmupFlag) {
                        ++state.dataSimAirServingZones->ErrCountSALC;
                        if (state.dataSimAirServingZones->ErrCountSALC < 15) {
                            state.dataSimAirServingZones->ErrEnvironmentName = state.dataEnvrn->EnvironmentName;
                            const auto CharErrOut = fmt::to_string(MaxIter);
                            ShowWarningError(state,
                                             "SolveAirLoopControllers: Maximum iterations (" + CharErrOut + ") exceeded for " +
                                                 PrimaryAirSystems(AirLoopNum).Name + ", " +
                                                 PrimaryAirSystems(AirLoopNum).ControllerName(AirLoopControlNum) + ", at " +
                                                 state.dataEnvrn->EnvironmentName + ", " + state.dataEnvrn->CurMnDy + ' ' +
                                                 CreateSysTimeIntervalString(state));
                        } else {
                            if (state.dataEnvrn->EnvironmentName != state.dataSimAirServingZones->ErrEnvironmentName) {
                                state.dataSimAirServingZones->MaxErrCountSALC = 0;
                                state.dataSimAirServingZones->ErrEnvironmentName = state.dataEnvrn->EnvironmentName;
                            }
                            ShowRecurringWarningErrorAtEnd(state,
                                                           "SolveAirLoopControllers: Exceeding Maximum iterations for " +
                                                               PrimaryAirSystems(AirLoopNum).Name + " during " + state.dataEnvrn->EnvironmentName +
                                                               " continues",
                                                           state.dataSimAirServingZones->MaxErrCountSALC);
                        }
                    }

                    // It is necessary to execute this statement anytime, even if the warning message is suppressed.
                    // To continue the simulation it must be able to goto the Exit statement
                    break; // It will not converge this time
                }

                // Re-evaluate air loop components with new actuated variables
                ++NumCalls;
                // this call to SimAirLoopComponents will simulate the OA system and set the PrimaryAirSystem( AirLoopNum ).ControlConverged(
                // AirLoopControlNum ) flag for controllers of water coils in the OA system for controllers not in the OA system, this flag is set
                // above in this function
                SimAirLoopComponents(state, AirLoopNum, FirstHVACIteration);
                // pass convergence flag from OA system water coils (i.e., SolveWaterCoilController) back to this loop
                // for future reference, the PrimaryAirSystem().ControlConverged flag is set while managing OA system water coils.
                // If convergence is not achieved with OA system water coils, suspect how this flag is passed back here or why OA system coils do
                // not converge
                ControllerConvergedFlag = PrimaryAirSystems(AirLoopNum).ControlConverged(AirLoopControlNum);
                IsUpToDateFlag = true;
            }

        } // End of the Convergence Iteration

        // Update tracker for max iteration counter across all controllers on this air loops
        IterMax = max(IterMax, state.dataSimAirServingZones->IterSALC);
        // Update tracker for aggregated counter of air loop inner iterations across controllers
        // on this air loop
        IterTot += state.dataSimAirServingZones->IterSALC;

    } // End of controller loop

    // Once the controllers are converged then need to simulate the components once
    // more to ensure that they are simulated with the latest values.
    if (!IsUpToDateFlag || !AirLoopConvergedFlag) {
        ++NumCalls;
        SimAirLoopComponents(state, AirLoopNum, FirstHVACIteration);
        IsUpToDateFlag = true;
    }

    // Check that all active controllers are still convergence
    for (int AirLoopControlNum = 1; AirLoopControlNum <= PrimaryAirSystems(AirLoopNum).NumControllers; ++AirLoopControlNum) {

        ControllerConvergedFlag = false;

        ManageControllers(state,
                          PrimaryAirSystems(AirLoopNum).ControllerName(AirLoopControlNum),
                          PrimaryAirSystems(AirLoopNum).ControllerIndex(AirLoopControlNum),
                          FirstHVACIteration,
                          AirLoopNum,
                          ControllerOperation::End,
                          ControllerConvergedFlag,
                          IsUpToDateFlag,
                          state.dataSimAirServingZones->BypassOAControllerSALC);

        PrimaryAirSystems(AirLoopNum).ControlConverged(AirLoopControlNum) = ControllerConvergedFlag;

        AirLoopConvergedFlag = AirLoopConvergedFlag && ControllerConvergedFlag;
    }
}

void SolveWaterCoilController(EnergyPlusData &state,
                              bool const FirstHVACIteration,
                              int const AirLoopNum,
                              std::string const &CompName,
                              int &CompIndex,
                              std::string const &ControllerName,
                              int ControllerIndex,
                              bool const HXAssistedWaterCoil)
{

    // SUBROUTINE INFORMATION
    //       AUTHOR:  Richard Raustad (FSEC)
    //       DATE WRITTEN:  July 2017

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine solves for the controllers in the specfied air loop OA system.

    // METHODOLOGY EMPLOYED:
    // For the specified primary air system:
    // (1) the specific component in the OA system is simulated
    // (2) The controllers and their actions are simulated
    // (3) Steps 2 and 3 are repeated until the control criteria are satisfied
    // (4) convergence is passed back to SolveAirLoopControllers via PrimaryAirSystem( AirLoopNum ).ControlConverged( ControllerIndex )

    // REFERENCES: None

    // Using/Aliasing
    using namespace DataHVACControllers;
    using General::CreateSysTimeIntervalString;
    using HVACControllers::ManageControllers;
    using HVACHXAssistedCoolingCoil::SimHXAssistedCoolingCoil;
    using WaterCoils::SimulateWaterCoilComponents;

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:
    // TRUE if first full HVAC iteration in an HVAC timestep
    // DO loop index; there are 2 passes the 2nd is done only if mass balance fails
    // Index of the air loop being simulated
    // TRUE when primary air system & controllers simulation has converged;
    // Max number of iterations performed by controllers across this air loop
    // Aggregated number of iterations across all controllers on this air loop
    // Total number of times SimAirLoopComponents() has been invoked

    // SUBROUTINE PARAMETER DEFINITIONS:
    // Maximum iterations of an air system/controllers simulation sequence
    constexpr int MaxIter(50);

    // SUBROUTINE LOCAL VARIABLE DEFINITIONS
    // TRUE if controller supports speculative warm restart
    bool AllowWarmRestartFlag;
    // TRUE when controller has converged
    bool ControllerConvergedFlag;
    // TRUE when air loop has been evaluated with latest actuated variables
    bool IsUpToDateFlag;

    // A character string equivalent of ErrCount

    auto &PrimaryAirSystems(state.dataAirSystemsData->PrimaryAirSystems);
    auto &AirLoopControlInfo(state.dataAirLoop->AirLoopControlInfo);

    bool AirLoopCheck = false;
    if (AirLoopNum > 0) {
        AirLoopCheck = true;
    }
    state.dataSimAirServingZones->BypassOAControllerSWCC = false; // simulate OA water coil controllers
    if (AirLoopCheck) {
        state.dataSimAirServingZones->AirLoopPassSWCC = AirLoopControlInfo(AirLoopNum).AirLoopPass;
    }
    IsUpToDateFlag = false;
    if (AirLoopCheck) {
        PrimaryAirSystems(AirLoopNum).ControlConverged = false;
    }

    AllowWarmRestartFlag = true;
    if (AirLoopCheck) {
        AirLoopControlInfo(AirLoopNum).AllowWarmRestartFlag = true;
    }

    // This call to ManageControllers reinitializes the controllers actuated variables to zero

    // BypassOAController is false here since we want to simulate the controller
    ManageControllers(state,
                      ControllerName,
                      ControllerIndex,
                      FirstHVACIteration,
                      AirLoopNum,
                      ControllerOperation::ColdStart,
                      ControllerConvergedFlag,
                      IsUpToDateFlag,
                      state.dataSimAirServingZones->BypassOAControllerSWCC,
                      AllowWarmRestartFlag);

    // Detect whether the speculative warm restart feature is supported by each controller on this air loop.
    if (AirLoopCheck) {
        AirLoopControlInfo(AirLoopNum).AllowWarmRestartFlag = AirLoopControlInfo(AirLoopNum).AllowWarmRestartFlag && AllowWarmRestartFlag;
    }

    // Evaluate water coils with new actuated variables
    if (HXAssistedWaterCoil) {
        SimHXAssistedCoolingCoil(state, CompName, FirstHVACIteration, CompressorOperation::On, 0.0, CompIndex, ContFanCycCoil);
    } else {
        SimulateWaterCoilComponents(state, CompName, FirstHVACIteration, CompIndex);
    }
    IsUpToDateFlag = true;

    // Loop over the air sys controllers until convergence or MaxIter iterations
    state.dataSimAirServingZones->IterSWCC = 0;
    ControllerConvergedFlag = false;
    // if the controller can be locked out by the economizer operation and the economizer is active, leave the controller inactive
    if (AirLoopCheck) {
        if (AirLoopControlInfo(AirLoopNum).EconoActive) {
            if (PrimaryAirSystems(AirLoopNum)
                    .CanBeLockedOutByEcono(state.dataHVACControllers->ControllerProps(ControllerIndex).AirLoopControllerIndex)) {
                ControllerConvergedFlag = true;
            }
        }
    }

    // For this controller, iterate until convergence
    while (!ControllerConvergedFlag) {

        ++state.dataSimAirServingZones->IterSWCC;

        ManageControllers(state,
                          ControllerName,
                          ControllerIndex,
                          FirstHVACIteration,
                          AirLoopNum,
                          ControllerOperation::Iterate,
                          ControllerConvergedFlag,
                          IsUpToDateFlag,
                          state.dataSimAirServingZones->BypassOAControllerSWCC);

        if (AirLoopCheck) {
            PrimaryAirSystems(AirLoopNum).ControlConverged(state.dataHVACControllers->ControllerProps(ControllerIndex).AirLoopControllerIndex) =
                ControllerConvergedFlag;
        }

        if (!ControllerConvergedFlag) {
            // Only check abnormal termination if not yet converged
            // The iteration counter has been exceeded.
            if (state.dataSimAirServingZones->IterSWCC > MaxIter) {

                // The warning message will be suppressed during the warm up days.
                if (!state.dataGlobal->WarmupFlag) {
                    ++state.dataSimAirServingZones->ErrCountSWCC;
                    if (state.dataSimAirServingZones->ErrCountSWCC < 15) {
                        state.dataSimAirServingZones->ErrEnvironmentNameSolveWaterCoilController = state.dataEnvrn->EnvironmentName;
                        const auto CharErrOut = fmt::to_string(MaxIter);
                        ShowWarningError(state,
                                         "SolveAirLoopControllers: Maximum iterations (" + CharErrOut + ") exceeded for " +
                                             PrimaryAirSystems(AirLoopNum).Name + ":" + ControllerName + ", at " + state.dataEnvrn->EnvironmentName +
                                             ", " + state.dataEnvrn->CurMnDy + ' ' + CreateSysTimeIntervalString(state));
                    } else {
                        if (state.dataEnvrn->EnvironmentName != state.dataSimAirServingZones->ErrEnvironmentNameSolveWaterCoilController) {
                            state.dataSimAirServingZones->MaxErrCountSWCC = 0;
                            state.dataSimAirServingZones->ErrEnvironmentNameSolveWaterCoilController = state.dataEnvrn->EnvironmentName;
                        }
                        ShowRecurringWarningErrorAtEnd(state,
                                                       "SolveAirLoopControllers: Exceeding Maximum iterations for " +
                                                           PrimaryAirSystems(AirLoopNum).Name + " during " + state.dataEnvrn->EnvironmentName +
                                                           " continues",
                                                       state.dataSimAirServingZones->MaxErrCountSWCC);
                    }
                }

                // It is necessary to execute this statement anytime, even if the warning message is suppressed.
                // To continue the simulation it must be able to goto the Exit statement
                break; // It will not converge this time
            }

            // Re-evaluate air loop components with new actuated variables
            if (HXAssistedWaterCoil) {
                SimHXAssistedCoolingCoil(state, CompName, FirstHVACIteration, CompressorOperation::On, 0.0, CompIndex, ContFanCycCoil);
            } else {
                SimulateWaterCoilComponents(state, CompName, FirstHVACIteration, CompIndex);
            }
            IsUpToDateFlag = true;
        }

    } // End of the Convergence Iteration

    IsUpToDateFlag = true;

    // Check that this controller is still converged

    ControllerConvergedFlag = false;

    ManageControllers(state,
                      ControllerName,
                      ControllerIndex,
                      FirstHVACIteration,
                      AirLoopNum,
                      ControllerOperation::End,
                      ControllerConvergedFlag,
                      IsUpToDateFlag,
                      state.dataSimAirServingZones->BypassOAControllerSWCC);

    // pass convergence of OA system water coils back to SolveAirLoopControllers via PrimaryAirSystem().ControlConverged flag
    if (AirLoopCheck) {
        PrimaryAirSystems(AirLoopNum).ControlConverged(state.dataHVACControllers->ControllerProps(ControllerIndex).AirLoopControllerIndex) =
            ControllerConvergedFlag;
        AirLoopControlInfo(AirLoopNum).ConvergedFlag = AirLoopControlInfo(AirLoopNum).ConvergedFlag && ControllerConvergedFlag;
    }
}

void ReSolveAirLoopControllers(
    EnergyPlusData &state, bool const FirstHVACIteration, int const AirLoopNum, bool &AirLoopConvergedFlag, int &IterMax, int &IterTot, int &NumCalls)
{

    // SUBROUTINE INFORMATION
    //             AUTHOR:  Dimitri Curtil (LBNL)
    //       DATE WRITTEN:  Feb 2006
    //           MODIFIED:
    //      RE-ENGINEERED:  This is new code

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine solves for the controllers on the specfied air loop by reusing
    // the solution from the previous HVAC iteration.
    // It is used in the context of the optimization technique referred to as
    // speculative warm restart.

    // METHODOLOGY EMPLOYED:
    // For the specified primary air system:
    // (1) each component in the system is simulated in natural order, beginning at
    //     the return air inlet and progressing to the supply air outlets. Node data
    //     is passed in the same direction.
    // (2) The controllers and their actions are simulated.

    // REFERENCES: None

    // Using/Aliasing
    using namespace DataHVACControllers;
    using HVACControllers::ManageControllers;

    // Locals
    // SUBROUTINE ARGUMENT DEFINITIONS:
    // TRUE if first full HVAC iteration in an HVAC timestep
    // DO loop index; there are 2 passes the 2nd is done only if mass balance fails
    // TRUE when primary air system & controllers simulation has converged;
    // Max number of iterations performed by controllers across all air loops
    // Aggregated number of iterations across all air loops
    // Total number of times SimAirLoopComponents() has been invoked

    // SUBROUTINE LOCAL VARIABLE DEFINITIONS
    // Controller DO loop index
    int AirLoopControlNum;
    // TRUE when controller has converged
    bool ControllerConvergedFlag;
    // TRUE when air loop needs to be refreshed.
    // Note that it is not used by ManageControllers() in the WARM_RESTART mode.
    bool IsUpToDateFlag;

    auto &PrimaryAirSystems(state.dataAirSystemsData->PrimaryAirSystems);

    // To track number of calls to SimAirLoopComponents() for each air loop
    // Represents the most computationally expensive operation in the iteration.
    // Best metric to use to assess the runtime performance of air loop simulation
    NumCalls = 0;
    IterMax = 0;
    IterTot = 0;

    AirLoopConvergedFlag = true;
    state.dataSimAirServingZones->BypassOAControllerRSALC =
        false; // not exactly sure of this but it seems all controllers need to be simulated -- don't bypass
    IsUpToDateFlag = false;
    PrimaryAirSystems(AirLoopNum).ControlConverged = false;

    // This call to ManageControllers reinitializes the controllers actuated variables to zero
    // E.g., actuator inlet water flow
    for (AirLoopControlNum = 1; AirLoopControlNum <= PrimaryAirSystems(AirLoopNum).NumControllers; ++AirLoopControlNum) {

        // BypassOAController is false here since we want to simulate the controller during ReSolveAirLoopControllers calls ?
        ManageControllers(state,
                          PrimaryAirSystems(AirLoopNum).ControllerName(AirLoopControlNum),
                          PrimaryAirSystems(AirLoopNum).ControllerIndex(AirLoopControlNum),
                          FirstHVACIteration,
                          AirLoopNum,
                          ControllerOperation::WarmRestart,
                          ControllerConvergedFlag,
                          IsUpToDateFlag,
                          state.dataSimAirServingZones->BypassOAControllerRSALC);
    }

    // Evaluate air loop components with new actuated variables
    ++NumCalls;
    SimAirLoopComponents(state, AirLoopNum, FirstHVACIteration);
    IsUpToDateFlag = true;

    // Check that all active controllers are still convergence
    // Check that actuated variables are within min/max constraints
    for (AirLoopControlNum = 1; AirLoopControlNum <= PrimaryAirSystems(AirLoopNum).NumControllers; ++AirLoopControlNum) {

        ControllerConvergedFlag = false;

        ManageControllers(state,
                          PrimaryAirSystems(AirLoopNum).ControllerName(AirLoopControlNum),
                          PrimaryAirSystems(AirLoopNum).ControllerIndex(AirLoopControlNum),
                          FirstHVACIteration,
                          AirLoopNum,
                          ControllerOperation::End,
                          ControllerConvergedFlag,
                          IsUpToDateFlag,
                          state.dataSimAirServingZones->BypassOAControllerRSALC);

        PrimaryAirSystems(AirLoopNum).ControlConverged(AirLoopControlNum) = ControllerConvergedFlag;

        AirLoopConvergedFlag = AirLoopConvergedFlag && ControllerConvergedFlag;

        // Update tracker for max iteration counter across all controllers on all air loops
        IterMax = max(IterMax, 0);
        // Update tracker for aggregated counter of air loop inner iterations across all controllers
        IterTot += 0;

    } // end of controller loop
}

void SimAirLoopComponents(EnergyPlusData &state,
                          int const AirLoopNum,         // Index of the air loop being currently simulated
                          bool const FirstHVACIteration // TRUE if first full HVAC iteration in an HVAC timestep
)
{
    // SUBROUTINE INFORMATION
    //             AUTHOR:  Dimitri Curtil (LBNL)
    //       DATE WRITTEN:  Feb 2006
    //           MODIFIED:
    //      RE-ENGINEERED:

    // PURPOSE OF THIS SUBROUTINE:
    // This simulates all components on a particular air loop in the primary air system.
    // This code used to appear in different places in SimAirLoops(). Now consolidated
    // into one subroutine called many times.

    // METHODOLOGY EMPLOYED:
    // For each branch in the air loop:
    // (1) update branch connection with (BeforeBranchSim)
    // (2) simulate each component
    // (3) update branch connection with (AfterBranchSim) to enforce continuity through splitter
    // Sets current branch number to CurBranchNum defined in MODULE DataSizing
    // Sets duct type of current branch to CurDuctType defined in MODULE DataSizing
    // Upon exiting, resets both counters to 0.

    // SUBROUTINE LOCAL VARIABLE DEFINITIONS: None
    int BranchNum; // Branch DO loop index
    int CompNum;   // Component DO loop index
    // std::string CompType; // Component type
    // std::string CompName; // Component name
    CompType CompType_Num; // Numeric equivalent for CompType

    auto &PrimaryAirSystems(state.dataAirSystemsData->PrimaryAirSystems);

    for (BranchNum = 1; BranchNum <= PrimaryAirSystems(AirLoopNum).NumBranches; ++BranchNum) { // loop over all branches in air system

        UpdateBranchConnections(state, AirLoopNum, BranchNum, BeforeBranchSim);

        state.dataSize->CurBranchNum = BranchNum;
        state.dataSize->CurDuctType = PrimaryAirSystems(AirLoopNum).Branch(BranchNum).DuctType;

        // Loop over components in branch
        for (CompNum = 1; CompNum <= PrimaryAirSystems(AirLoopNum).Branch(BranchNum).TotalComponents; ++CompNum) {
            // CompType = PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).TypeOf;
            // CompName = PrimaryAirSystem( AirLoopNum ).Branch( BranchNum ).Comp( CompNum ).Name;
            CompType_Num = PrimaryAirSystems(AirLoopNum).Branch(BranchNum).Comp(CompNum).CompType_Num;

            // Simulate each component on PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Name
            SimAirLoopComponent(state,
                                PrimaryAirSystems(AirLoopNum).Branch(BranchNum).Comp(CompNum).Name,
                                CompType_Num,
                                FirstHVACIteration,
                                AirLoopNum,
                                PrimaryAirSystems(AirLoopNum).Branch(BranchNum).Comp(CompNum).CompIndex,
                                PrimaryAirSystems(AirLoopNum).Branch(BranchNum).Comp(CompNum).compPointer,
                                AirLoopNum,
                                BranchNum,
                                CompNum);
        } // End of component loop

        // Enforce continuity through the splitter
        UpdateBranchConnections(state, AirLoopNum, BranchNum, AfterBranchSim);

    } // End of branch loop

    state.dataSize->CurBranchNum = 0;
    state.dataSize->CurDuctType = 0;
}

void SimAirLoopComponent(EnergyPlusData &state,
                         std::string const &CompName,   // the component Name
                         CompType const CompType_Num,   // numeric equivalent for component type
                         bool const FirstHVACIteration, // TRUE if first full HVAC iteration in an HVAC timestep
                         int const AirLoopNum,          // Primary air loop number
                         int &CompIndex,                // numeric pointer for CompType/CompName -- passed back from other routines
                         HVACSystemData *CompPointer,   // equipment actual pointer
                         int const airLoopNum,          // index to AirloopHVAC
                         int const branchNum,           // index to AirloopHVAC branch
                         int const compNum              // index to AirloopHVAC branch component
)
{

    // SUBROUTINE INFORMATION
    //             AUTHOR:  Russ Taylor, Dan Fisher, Fred Buhl
    //       DATE WRITTEN:  Oct 1997
    //           MODIFIED:  Dec 1997 Fred Buhl, Richard Raustad,FSEC Sept 2003
    //      RE-ENGINEERED:  This is new code, not reengineered

    // PURPOSE OF THIS SUBROUTINE:
    // Calls the individual air loop component simulation routines

    // METHODOLOGY EMPLOYED: None

    // REFERENCES: None

    // USE Statements
    // Using/Aliasing
    using DesiccantDehumidifiers::SimDesiccantDehumidifier;
    using EvaporativeCoolers::SimEvapCooler;
    using Fans::SimulateFanComponents;
    using Furnaces::SimFurnace;
    using HeatingCoils::SimulateHeatingCoilComponents;
    using HeatRecovery::SimHeatRecovery;
    using Humidifiers::SimHumidifier;
    using HVACDuct::SimDuct;
    using HVACDXHeatPumpSystem::SimDXHeatPumpSystem;
    using HVACHXAssistedCoolingCoil::SimHXAssistedCoolingCoil;
    using HVACMultiSpeedHeatPump::SimMSHeatPump;
    using HVACUnitaryBypassVAV::SimUnitaryBypassVAV;
    using MixedAir::ManageOutsideAirSystem;
    using SteamCoils::SimulateSteamCoilComponents;
    using UserDefinedComponents::SimCoilUserDefined;
    using WaterCoils::SimulateWaterCoilComponents;

    // SUBROUTINE LOCAL VARIABLE DEFINITIONS:
    Real64 QActual;
    int OAUnitNum = 0;           // used only for UnitarySystem call
    Real64 OAUCoilOutTemp = 0.0; // used only for UnitarySystem call
    bool ZoneEquipFlag = false;  // used only for UnitarySystem call
    bool CoolingActive = false;
    bool HeatingActive = false;

    auto &AirLoopControlInfo(state.dataAirLoop->AirLoopControlInfo);

    switch (CompType_Num) {
    case CompType::OAMixer_Num: { // 'OUTSIDE AIR SYSTEM'
        ManageOutsideAirSystem(state, CompName, FirstHVACIteration, AirLoopNum, CompIndex);
        // Fan Types for the air sys simulation
    } break;
    case CompType::Fan_Simple_CV: { // 'Fan:ConstantVolume'
        Fans::SimulateFanComponents(state, CompName, FirstHVACIteration, CompIndex);
    } break;
    case CompType::Fan_Simple_VAV: { // 'Fan:VariableVolume'
        Fans::SimulateFanComponents(state, CompName, FirstHVACIteration, CompIndex);
    } break;
    case CompType::Fan_System_Object: {                                        // "Fan:SystemModel" new for V8.6
        if (CompIndex == 0) {                                                  // 0 means has not been filled because of 1-based arrays in old fortran
            CompIndex = HVACFan::getFanObjectVectorIndex(state, CompName) + 1; // + 1 for shift from zero-based vector to 1-based compIndex
        }
        // if the fan is here, it can't (yet) really be cycling fan operation, set this ugly global in the event that there are dx coils
        // involved but the fan should really run like constant volume and not cycle with compressor
        state.dataHVACGlobal->OnOffFanPartLoadFraction = 1.0;
        state.dataHVACFan->fanObjs[CompIndex - 1]->simulate(state, _, _, _, _); // vector is 0 based, but CompIndex is 1 based so shift
    } break;
    case CompType::Fan_ComponentModel: { // 'Fan:ComponentModel'
        Fans::SimulateFanComponents(state, CompName, FirstHVACIteration, CompIndex);

        // Coil Types for the air sys simulation
        //  Currently no control for HX Assisted coils
        //  CASE(DXCoil_CoolingHXAsst)  ! 'CoilSystem:Cooling:DX:HeatExchangerAssisted'
        //    CALL SimHXAssistedCoolingCoil(CompName,FirstHVACIteration,CoilOn,0.0,CompIndex,ContFanCycCoil)
    } break;
    case CompType::WaterCoil_CoolingHXAsst: { // 'CoilSystem:Cooling:Water:HeatExchangerAssisted'
        SimHXAssistedCoolingCoil(state,
                                 CompName,
                                 FirstHVACIteration,
                                 CompressorOperation::On,
                                 DataPrecisionGlobals::constant_zero,
                                 CompIndex,
                                 ContFanCycCoil,
                                 _,
                                 _,
                                 _,
                                 QActual);
        if (QActual > 0.0) CoolingActive = true; // determine if coil is ON
    } break;
    case CompType::WaterCoil_SimpleHeat: { // 'Coil:Heating:Water'
        SimulateWaterCoilComponents(state, CompName, FirstHVACIteration, CompIndex, QActual);
        if (QActual > 0.0) HeatingActive = true; // determine if coil is ON
    } break;
    case CompType::SteamCoil_AirHeat: { // 'Coil:Heating:Steam'
        SimulateSteamCoilComponents(state, CompName, FirstHVACIteration, CompIndex, DataPrecisionGlobals::constant_zero, QActual);
        if (QActual > 0.0) HeatingActive = true; // determine if coil is ON
    } break;
    case CompType::WaterCoil_DetailedCool: { // 'Coil:Cooling:Water:DetailedGeometry'
        SimulateWaterCoilComponents(state, CompName, FirstHVACIteration, CompIndex, QActual);
        if (QActual > 0.0) CoolingActive = true; // determine if coil is ON
    } break;
    case CompType::WaterCoil_Cooling: { // 'Coil:Cooling:Water'
        SimulateWaterCoilComponents(state, CompName, FirstHVACIteration, CompIndex, QActual);
        if (QActual > 0.0) CoolingActive = true; // determine if coil is ON
        // stand-alone coils are temperature controlled (do not pass QCoilReq in argument list, QCoilReq overrides temp SP)
    } break;
    case CompType::Coil_ElectricHeat: { // 'Coil:Heating:Electric'
        SimulateHeatingCoilComponents(state, CompName, FirstHVACIteration, _, CompIndex, QActual);
        if (QActual > 0.0) HeatingActive = true; // determine if coil is ON
        // stand-alone coils are temperature controlled (do not pass QCoilReq in argument list, QCoilReq overrides temp SP)
    } break;
    case CompType::Coil_GasHeat: { // 'Coil:Heating:Fuel'
        SimulateHeatingCoilComponents(state, CompName, FirstHVACIteration, _, CompIndex, QActual);
        if (QActual > 0.0) HeatingActive = true; // determine if coil is ON
        // stand-alone coils are temperature controlled (do not pass QCoilReq in argument list, QCoilReq overrides temp SP)
    } break;
    case CompType::Coil_DeSuperHeat: { // 'Coil:Heating:Desuperheater' - heat reclaim
        SimulateHeatingCoilComponents(state, CompName, FirstHVACIteration, _, CompIndex, QActual);
        if (QActual > 0.0) HeatingActive = true; // determine if coil is ON
    } break;
    case CompType::DXSystem: { // CoilSystem:Cooling:DX  old 'AirLoopHVAC:UnitaryCoolOnly'
        if (CompPointer == nullptr) {
            UnitarySystems::UnitarySys thisSys;
            CompPointer = thisSys.factory(state, DataHVACGlobals::UnitarySys_AnyCoilType, CompName, false, 0);
            // temporary fix for saving pointer, eventually apply to UnitarySystem 25 lines down
            state.dataAirSystemsData->PrimaryAirSystems(airLoopNum).Branch(branchNum).Comp(compNum).compPointer = CompPointer;
        }
        Real64 sensOut = 0.0;
        Real64 latOut = 0.0;
        CompPointer->simulate(state,
                              CompName,
                              FirstHVACIteration,
                              AirLoopNum,
                              CompIndex,
                              HeatingActive,
                              CoolingActive,
                              OAUnitNum,
                              OAUCoilOutTemp,
                              ZoneEquipFlag,
                              sensOut,
                              latOut);

    } break;
    case CompType::DXHeatPumpSystem: { // 'CoilSystem:Heating:DX'
        SimDXHeatPumpSystem(state, CompName, FirstHVACIteration, AirLoopNum, CompIndex, _, _, QActual);
        if (QActual > 0.0) HeatingActive = true; // determine if coil is ON
    } break;
    case CompType::CoilUserDefined: { // Coil:UserDefined
        SimCoilUserDefined(state, CompName, CompIndex, AirLoopNum, HeatingActive, CoolingActive);
    } break;
    case CompType::UnitarySystemModel: { // 'AirLoopHVAC:UnitarySystem'
        Real64 sensOut = 0.0;
        Real64 latOut = 0.0;
        CompPointer->simulate(state,
                              CompName,
                              FirstHVACIteration,
                              AirLoopNum,
                              CompIndex,
                              HeatingActive,
                              CoolingActive,
                              OAUnitNum,
                              OAUCoilOutTemp,
                              ZoneEquipFlag,
                              sensOut,
                              latOut);
    } break;
    case CompType::CoilSystemWater: { // 'CoilSystemCooling:Water'
        if (CompPointer == nullptr) {
            UnitarySystems::UnitarySys thisSys;
            CompPointer = thisSys.factory(state, DataHVACGlobals::UnitarySys_AnyCoilType, CompName, false, 0);
            // temporary fix for saving pointer, eventually apply to UnitarySystem 16 lines above
            state.dataAirSystemsData->PrimaryAirSystems(airLoopNum).Branch(branchNum).Comp(compNum).compPointer = CompPointer;
        }
        Real64 sensOut = 0.0;
        Real64 latOut = 0.0;
        CompPointer->simulate(state,
                              CompName,
                              FirstHVACIteration,
                              AirLoopNum,
                              CompIndex,
                              HeatingActive,
                              CoolingActive,
                              OAUnitNum,
                              OAUCoilOutTemp,
                              ZoneEquipFlag,
                              sensOut,
                              latOut);
    } break;
    case CompType::Furnace_UnitarySys_HeatOnly:
    case CompType::Furnace_UnitarySys_HeatCool: {
        // 'AirLoopHVAC:Unitary:Furnace:HeatOnly', 'AirLoopHVAC:Unitary:Furnace:HeatCool',
        // 'AirLoopHVAC:UnitaryHeatOnly', 'AirLoopHVAC:UnitaryHeatCool'
        // 'AirLoopHVAC:UnitaryHeatPump:AirToAir', 'AirLoopHVAC:UnitaryHeatPump:WaterToAir'
        SimFurnace(state, CompName, FirstHVACIteration, AirLoopNum, CompIndex);
    } break;
    case CompType::UnitarySystem_BypassVAVSys: { // 'AirLoopHVAC:UnitaryHeatCool:VAVChangeoverBypass'
        SimUnitaryBypassVAV(state, CompName, FirstHVACIteration, AirLoopNum, CompIndex);
    } break;
    case CompType::UnitarySystem_MSHeatPump: { // 'AirLoopHVAC:UnitaryHeatPump:AirToAir:Multispeed'
        SimMSHeatPump(state, CompName, FirstHVACIteration, AirLoopNum, CompIndex);
        // Humidifier Types for the air system simulation
    } break;
    case CompType::Humidifier: { // 'Humidifier:Steam:Electric' and 'Humidifier:Steam:Gas'
        SimHumidifier(state, CompName, FirstHVACIteration, CompIndex);
        // Evap Cooler Types for the air system simulation
    } break;
    case CompType::EvapCooler: { // 'EvaporativeCooler:Direct:CelDekPad', 'EvaporativeCooler:Indirect:CelDekPad'
        // 'EvaporativeCooler:Indirect:WetCoil', 'EvaporativeCooler:Indirect:ResearchSpecial'
        SimEvapCooler(state, CompName, CompIndex, state.dataAirLoop->AirLoopFlow(AirLoopNum).FanPLR);
        // Desiccant Dehumidifier Types for the air system simulation
    } break;
    case CompType::Desiccant: { // 'Dehumidifier:Desiccant:NoFans', 'Dehumidifier:Desiccant:System'
        SimDesiccantDehumidifier(state, CompName, FirstHVACIteration, CompIndex);
        // Heat recovery
    } break;
    case CompType::HeatXchngr: { // 'HeatExchanger:AirToAir:FlatPlate', 'HeatExchanger:AirToAir:SensibleAndLatent'
        // 'HeatExchanger:Desiccant:BalancedFlow'
        SimHeatRecovery(state,
                        CompName,
                        FirstHVACIteration,
                        CompIndex,
                        AirLoopControlInfo(AirLoopNum).FanOpMode,
                        state.dataAirLoop->AirLoopFlow(AirLoopNum).FanPLR,
                        _,
                        _,
                        _,
                        AirLoopControlInfo(AirLoopNum).EconoActive,
                        AirLoopControlInfo(AirLoopNum).HighHumCtrlActive);

        // Ducts
    } break;
    case CompType::ZoneVRFasAirLoopEquip: { // 'ZoneHVAC:TerminalUnit:VariableRefrigerantFlow'
        int ControlledZoneNum = 0;
        int constexpr OAUnitNumLocal = 0;
        Real64 constexpr OAUCoilOutTempLocal = 0.0;
        bool constexpr ZoneEquipment = false;
        Real64 sysOut = 0.0;
        Real64 latOut = 0.0;
        HVACVariableRefrigerantFlow::SimulateVRF(state,
                                                 CompName,
                                                 FirstHVACIteration,
                                                 ControlledZoneNum,
                                                 CompIndex,
                                                 HeatingActive,
                                                 CoolingActive,
                                                 OAUnitNumLocal,
                                                 OAUCoilOutTempLocal,
                                                 ZoneEquipment,
                                                 sysOut,
                                                 latOut);

    } break;
    case CompType::Duct: { // 'Duct'
        SimDuct(state, CompName, FirstHVACIteration, CompIndex);
    } break;
    default:
        break;
    }

    // Set AirLoopControlInfo flag to identify coil operation for "Air Loop Coils"
    // Any coil operation from multiple coils causes flag to be TRUE
    // Flag is reset at beginning of each iteration (Subroutine SimHVAC)
    AirLoopControlInfo(AirLoopNum).CoolingActiveFlag = AirLoopControlInfo(AirLoopNum).CoolingActiveFlag || CoolingActive;
    AirLoopControlInfo(AirLoopNum).HeatingActiveFlag = AirLoopControlInfo(AirLoopNum).HeatingActiveFlag || HeatingActive;
}

void UpdateBranchConnections(EnergyPlusData &state,
                             int const AirLoopNum, // primary air system number
                             int const BranchNum,  // branch reference number
                             int const Update      // 1=BeforeBranchSim; 2=AfterBranchSim
)
{

    // SUBROUTINE INFORMATION
    //             AUTHOR:  Fred Buhl
    //       DATE WRITTEN:  Nov 1999
    //           MODIFIED:
    //      RE-ENGINEERED:  This is new code, not reengineered

    // PURPOSE OF THIS SUBROUTINE:
    // This routine passes node data from a branch exit node through a
    // splitter.

    // METHODOLOGY EMPLOYED:
    // Temperature, humidity ratio, and enthalpy are passed through from
    // the inlet to the outlets. The mass flow is divided among the outlets
    // according to the required mass flows established by the zone equipment
    // simulation. The required mass flows are were stored in the node data
    // as MassFlowRateSetPoints in the InitAirLoops routine.

    // Using/Aliasing
    using Psychrometrics::PsyTdbFnHW;

    int OutletNum;                  // splitter outlet DO loop index
    int InletNum;                   // mixer inlet DO loop index
    int InletNodeNum;               // node number of splitter inlet node
    int OutletNodeNum;              // node number of a splitter outlet node
    int RABNodeNum;                 // splitter outlet RAB node
    int NonRABNodeNum;              // splitter outlet nonRAB node
    Real64 MassFlowRateSetSum;      // sum of mass flow rate setpoints for splitter outlet nodes
    Real64 MassFlowRateOut;         // outlet mass flow rate of mixer
    Real64 MassFlowRateMinAvailOut; // outlet minimum available mass flow rate
    Real64 OutletHumRat;            // outlet humidity ratio of mixer
    Real64 OutletEnthalpy;          // outlet enthalpy of mixer
    Real64 OutletPress;
    Real64 OutletCO2; // outlet CO2 of mixer
    Real64 OutletGC;  // outlet generic contaminant of mixer
    MassFlowRateSetSum = 0.0;
    MassFlowRateOut = 0.0;
    MassFlowRateMinAvailOut = 0.0;
    OutletHumRat = 0.0;
    OutletEnthalpy = 0.0;
    OutletPress = 0.0;
    RABNodeNum = 0;
    NonRABNodeNum = 0;
    OutletCO2 = 0.0;
    OutletGC = 0.0;

    auto &PrimaryAirSystems(state.dataAirSystemsData->PrimaryAirSystems);
    auto &AirLoopControlInfo(state.dataAirLoop->AirLoopControlInfo);

    if (PrimaryAirSystems(AirLoopNum).Splitter.Exists && Update == AfterBranchSim) {
        // if we are at an inlet branch, pass data through the splitter
        if (PrimaryAirSystems(AirLoopNum).Splitter.BranchNumIn == BranchNum) {
            InletNodeNum = PrimaryAirSystems(AirLoopNum).Splitter.NodeNumIn;
            // Pass node data through the splitter
            for (OutletNum = 1; OutletNum <= PrimaryAirSystems(AirLoopNum).Splitter.TotalOutletNodes; ++OutletNum) {
                OutletNodeNum = PrimaryAirSystems(AirLoopNum).Splitter.NodeNumOut(OutletNum);
                state.dataLoopNodes->Node(OutletNodeNum).Temp = state.dataLoopNodes->Node(InletNodeNum).Temp;
                state.dataLoopNodes->Node(OutletNodeNum).HumRat = state.dataLoopNodes->Node(InletNodeNum).HumRat;
                state.dataLoopNodes->Node(OutletNodeNum).Enthalpy = state.dataLoopNodes->Node(InletNodeNum).Enthalpy;
                state.dataLoopNodes->Node(OutletNodeNum).Press = state.dataLoopNodes->Node(InletNodeNum).Press;
                MassFlowRateSetSum +=
                    min(state.dataLoopNodes->Node(OutletNodeNum).MassFlowRateSetPoint, state.dataLoopNodes->Node(OutletNodeNum).MassFlowRateMaxAvail);
                if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                    state.dataLoopNodes->Node(OutletNodeNum).CO2 = state.dataLoopNodes->Node(InletNodeNum).CO2;
                }
                if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                    state.dataLoopNodes->Node(OutletNodeNum).GenContam = state.dataLoopNodes->Node(InletNodeNum).GenContam;
                }
            }
            if (!PrimaryAirSystems(AirLoopNum).RABExists) {
                // set the outlet mass flows
                for (OutletNum = 1; OutletNum <= PrimaryAirSystems(AirLoopNum).Splitter.TotalOutletNodes; ++OutletNum) {
                    OutletNodeNum = PrimaryAirSystems(AirLoopNum).Splitter.NodeNumOut(OutletNum);
                    if (MassFlowRateSetSum < SmallMassFlow || state.dataLoopNodes->Node(InletNodeNum).MassFlowRate < SmallMassFlow) {
                        state.dataLoopNodes->Node(OutletNodeNum).MassFlowRate = 0.0;
                    } else {
                        state.dataLoopNodes->Node(OutletNodeNum).MassFlowRate = state.dataLoopNodes->Node(InletNodeNum).MassFlowRate *
                                                                                (min(state.dataLoopNodes->Node(OutletNodeNum).MassFlowRateSetPoint,
                                                                                     state.dataLoopNodes->Node(OutletNodeNum).MassFlowRateMaxAvail) /
                                                                                 MassFlowRateSetSum);
                    }
                    state.dataLoopNodes->Node(OutletNodeNum).MassFlowRateMaxAvail = state.dataLoopNodes->Node(InletNodeNum).MassFlowRateMaxAvail;
                    state.dataLoopNodes->Node(OutletNodeNum).MassFlowRateMinAvail = 0.0;
                }
            } else { // set the RAB flow rates
                RABNodeNum = PrimaryAirSystems(AirLoopNum).RABSplitOutNode;
                NonRABNodeNum = PrimaryAirSystems(AirLoopNum).OtherSplitOutNode;
                if (AirLoopControlInfo(AirLoopNum).EconoActive) {
                    state.dataLoopNodes->Node(RABNodeNum).MassFlowRate = 0.0;
                    state.dataLoopNodes->Node(NonRABNodeNum).MassFlowRate = state.dataLoopNodes->Node(InletNodeNum).MassFlowRate;
                } else {
                    state.dataLoopNodes->Node(RABNodeNum).MassFlowRate = state.dataLoopNodes->Node(RABNodeNum).MassFlowRateSetPoint;
                    state.dataLoopNodes->Node(NonRABNodeNum).MassFlowRate =
                        state.dataLoopNodes->Node(InletNodeNum).MassFlowRate - state.dataLoopNodes->Node(RABNodeNum).MassFlowRate;
                    if (state.dataLoopNodes->Node(NonRABNodeNum).MassFlowRate <= state.dataAirLoop->AirLoopFlow(AirLoopNum).MinOutAir) {
                        state.dataLoopNodes->Node(NonRABNodeNum).MassFlowRate =
                            min(state.dataAirLoop->AirLoopFlow(AirLoopNum).MinOutAir, state.dataLoopNodes->Node(InletNodeNum).MassFlowRate);
                        state.dataLoopNodes->Node(RABNodeNum).MassFlowRate =
                            state.dataLoopNodes->Node(InletNodeNum).MassFlowRate - state.dataLoopNodes->Node(NonRABNodeNum).MassFlowRate;
                    }
                }
            }
        }
    }

    if (PrimaryAirSystems(AirLoopNum).Mixer.Exists && Update == BeforeBranchSim) {
        // if we are at a mixer outlet branch, calculate the outlet branch conditions
        if (PrimaryAirSystems(AirLoopNum).Mixer.BranchNumOut == BranchNum) {
            OutletNodeNum = PrimaryAirSystems(AirLoopNum).Mixer.NodeNumOut;
            // get the outlet mass flow rate and the outlet minavail mass flow rate
            for (InletNum = 1; InletNum <= PrimaryAirSystems(AirLoopNum).Mixer.TotalInletNodes; ++InletNum) {
                InletNodeNum = PrimaryAirSystems(AirLoopNum).Mixer.NodeNumIn(InletNum);
                MassFlowRateOut += state.dataLoopNodes->Node(InletNodeNum).MassFlowRate;
                MassFlowRateMinAvailOut += state.dataLoopNodes->Node(InletNodeNum).MassFlowRateMinAvail;
            }
            // set the outlet mass flow
            state.dataLoopNodes->Node(OutletNodeNum).MassFlowRate = MassFlowRateOut;
            state.dataLoopNodes->Node(OutletNodeNum).MassFlowRateMinAvail = MassFlowRateMinAvailOut;
            state.dataLoopNodes->Node(OutletNodeNum).MassFlowRateMaxAvail = state.dataLoopNodes->Node(OutletNodeNum).MassFlowRateMax;
            // calculate the outlet humidity ratio and enthalpy and pressure
            if (MassFlowRateOut > 0.0) {
                for (InletNum = 1; InletNum <= PrimaryAirSystems(AirLoopNum).Mixer.TotalInletNodes; ++InletNum) {
                    InletNodeNum = PrimaryAirSystems(AirLoopNum).Mixer.NodeNumIn(InletNum);
                    OutletHumRat +=
                        (state.dataLoopNodes->Node(InletNodeNum).MassFlowRate * state.dataLoopNodes->Node(InletNodeNum).HumRat) / MassFlowRateOut;
                    OutletEnthalpy +=
                        (state.dataLoopNodes->Node(InletNodeNum).MassFlowRate * state.dataLoopNodes->Node(InletNodeNum).Enthalpy) / MassFlowRateOut;
                    OutletPress +=
                        (state.dataLoopNodes->Node(InletNodeNum).MassFlowRate * state.dataLoopNodes->Node(InletNodeNum).Press) / MassFlowRateOut;
                    if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                        OutletCO2 +=
                            (state.dataLoopNodes->Node(InletNodeNum).MassFlowRate * state.dataLoopNodes->Node(InletNodeNum).CO2) / MassFlowRateOut;
                    }
                    if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                        OutletGC += (state.dataLoopNodes->Node(InletNodeNum).MassFlowRate * state.dataLoopNodes->Node(InletNodeNum).GenContam) /
                                    MassFlowRateOut;
                    }
                }
            } else {
                InletNodeNum = PrimaryAirSystems(AirLoopNum).Mixer.NodeNumIn(1);
                OutletHumRat = state.dataLoopNodes->Node(InletNodeNum).HumRat;
                OutletEnthalpy = state.dataLoopNodes->Node(InletNodeNum).Enthalpy;
                OutletPress = state.dataLoopNodes->Node(InletNodeNum).Press;
                if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                    OutletCO2 = state.dataLoopNodes->Node(InletNodeNum).CO2;
                }
                if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                    OutletGC = state.dataLoopNodes->Node(InletNodeNum).GenContam;
                }
            }
            state.dataLoopNodes->Node(OutletNodeNum).HumRat = OutletHumRat;
            state.dataLoopNodes->Node(OutletNodeNum).Enthalpy = OutletEnthalpy;
            state.dataLoopNodes->Node(OutletNodeNum).Press = OutletPress;
            // calculate the outlet temperature
            state.dataLoopNodes->Node(OutletNodeNum).Temp = PsyTdbFnHW(OutletEnthalpy, OutletHumRat);
            if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                state.dataLoopNodes->Node(OutletNodeNum).CO2 = OutletCO2;
            }
            if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                state.dataLoopNodes->Node(OutletNodeNum).GenContam = OutletGC;
            }
        }
    }
}

void ResolveSysFlow(EnergyPlusData &state,
                    int const SysNum, // the primary air system number
                    bool &SysReSim    // Set to TRUE if mass balance fails and resimulation is needed
)
{

    // SUBROUTINE INFORMATION
    //             AUTHOR:  Fred Buhl
    //       DATE WRITTEN:  Dec 1999
    //           MODIFIED:
    //      RE-ENGINEERED:  This is new code, not reengineered

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutines checks for mass flow balance in all air system branches
    // and across all connections. If there is a failure of mass flow
    // balance, mass flows are imposed to achieve mass flow balance and
    // the resimulate flag SysReSim is set to true.

    // METHODOLOGY EMPLOYED:
    // Node()%MassFlowRateMaxAvail for every node is set to the minimum
    // Node()%MassFlowRateMaxAvail on each branch.  Mass balance is imposed
    // at the branch connections. System inlet mass flows are forced to
    // be less than or equal to the resulting inlet MassFlowRateMaxAvails.

    int BranchNum;                 // branch DO loop index
    int NodeIndex;                 // node on branch DO loop index
    Real64 MassFlowRateOutSum;     // sum of splitter outlet mass flow rates (imposed)
    Real64 BranchMassFlowMaxAvail; // branch level maximum flow rate possible
    int OutletNum;                 // splitter outlet DO loop index
    int OutletNodeNum;             // a splitter outlet node number
    int InletNodeNum;              // splitter inlet node number
    int NodeNum;                   // a node number
    int NodeNumNext;               // node number of next node on a branch
    int InNodeNum;                 // air system inlet node
    int InBranchNum;               // air system inlet branch number
    int InBranchIndex;             // air sys inlet branch DO loop index

    auto &PrimaryAirSystems(state.dataAirSystemsData->PrimaryAirSystems);
    auto &AirLoopControlInfo(state.dataAirLoop->AirLoopControlInfo);

    // Find the minimum MassFlowMaxAvail for each branch in the system and store it on the branch inlet node.
    // Check for mass flow conservation on each branch. Set SysReSim to TRUE is mass flow not conserved.
    for (BranchNum = 1; BranchNum <= PrimaryAirSystems(SysNum).NumBranches; ++BranchNum) { // loop over branches in system
        // Initialize branch max avail mass flow to max avail mass flow at outlet node
        BranchMassFlowMaxAvail = state.dataLoopNodes->Node(PrimaryAirSystems(SysNum).Branch(BranchNum).NodeNumOut).MassFlowRateMaxAvail;
        for (NodeIndex = 1; NodeIndex <= PrimaryAirSystems(SysNum).Branch(BranchNum).TotalNodes; ++NodeIndex) { // loop over nodes on branch
            // Get the new smallest max avail mass flow
            NodeNum = PrimaryAirSystems(SysNum).Branch(BranchNum).NodeNum(NodeIndex);
            BranchMassFlowMaxAvail = min(BranchMassFlowMaxAvail, state.dataLoopNodes->Node(NodeNum).MassFlowRateMaxAvail);
            // Check for mass flow conservation on the branch
            if (NodeIndex < PrimaryAirSystems(SysNum).Branch(BranchNum).TotalNodes) {
                // Set ReSim flag to TRUE if mass flow not conserved on this branch
                NodeNumNext = PrimaryAirSystems(SysNum).Branch(BranchNum).NodeNum(NodeIndex + 1);
                if (NodeNum == PrimaryAirSystems(SysNum).OASysInletNodeNum) continue; // don't enforce mass balance across OA Sys
                // Changeover bypass system connected to a plenum or mixer will need to include the bypass flow rate
                if (std::abs(state.dataLoopNodes->Node(NodeNum).MassFlowRate - state.dataLoopNodes->Node(NodeNumNext).MassFlowRate -
                             state.dataAirLoop->AirLoopFlow(SysNum).BypassMassFlow) > SmallMassFlow)
                    SysReSim = true;
            }
        } // end node loop
        // Store the minimum MassFlowMaxAvail for this branch on the branch inlet node (AirloopHVAC supply inlet node)
        state.dataLoopNodes->Node(PrimaryAirSystems(SysNum).Branch(BranchNum).NodeNumIn).MassFlowRateMaxAvail = BranchMassFlowMaxAvail;
    } // end branch loop
    // force resimulation for fan-cycling, nonsimple systems
    if (!AirLoopControlInfo(SysNum).Simple && AirLoopControlInfo(SysNum).CyclingFan) {
        SysReSim = true;
    }

    // If mass flow conserved on each branch, check for mass balance across splitter
    if (!SysReSim && PrimaryAirSystems(SysNum).Splitter.Exists) {
        MassFlowRateOutSum = 0.0;
        InletNodeNum = PrimaryAirSystems(SysNum).Splitter.NodeNumIn;
        // Get sum of splitter outlet mass flows
        for (OutletNum = 1; OutletNum <= PrimaryAirSystems(SysNum).Splitter.TotalOutletNodes; ++OutletNum) {
            OutletNodeNum = PrimaryAirSystems(SysNum).Splitter.NodeNumOut(OutletNum);
            MassFlowRateOutSum += state.dataLoopNodes->Node(OutletNodeNum).MassFlowRate;
        }
        // Check whether sum of splitter outlet mass flows equals splitter inlet flow.
        if (std::abs(MassFlowRateOutSum - state.dataLoopNodes->Node(InletNodeNum).MassFlowRate) > SmallMassFlow) SysReSim = true;
    }

    //// Resimulate if the zone air mass flow conservation convergence critreon is not met
    if (state.dataHVACGlobal->ZoneMassBalanceHVACReSim) SysReSim = true;

    // If mass balance failed, resimulation is needed. Impose a mass balance for the new simulation.
    if (SysReSim) {
        // Set the MassFlowRateMaxAvail on each node to the minimum MassFlowRateMaxAvail for the branch.
        for (BranchNum = 1; BranchNum <= PrimaryAirSystems(SysNum).NumBranches; ++BranchNum) {                      // loop over branches in system
            for (NodeIndex = 2; NodeIndex <= PrimaryAirSystems(SysNum).Branch(BranchNum).TotalNodes; ++NodeIndex) { // loop over nodes on branch
                NodeNum = PrimaryAirSystems(SysNum).Branch(BranchNum).NodeNum(NodeIndex);
                state.dataLoopNodes->Node(NodeNum).MassFlowRateMaxAvail =
                    state.dataLoopNodes->Node(PrimaryAirSystems(SysNum).Branch(BranchNum).NodeNumIn).MassFlowRateMaxAvail;
            }
        }

        // Impose mass balance at splitter
        if (PrimaryAirSystems(SysNum).Splitter.Exists) {
            InBranchNum = PrimaryAirSystems(SysNum).Splitter.BranchNumIn;
            MassFlowRateOutSum = 0.0;
            InletNodeNum = PrimaryAirSystems(SysNum).Splitter.NodeNumIn;
            for (OutletNum = 1; OutletNum <= PrimaryAirSystems(SysNum).Splitter.TotalOutletNodes; ++OutletNum) {
                OutletNodeNum = PrimaryAirSystems(SysNum).Splitter.NodeNumOut(OutletNum);
                MassFlowRateOutSum +=
                    min(state.dataLoopNodes->Node(OutletNodeNum).MassFlowRateMaxAvail, state.dataLoopNodes->Node(OutletNodeNum).MassFlowRateSetPoint);
            }
            // set the splitter inlet Max Avail mass flow rate
            if (state.dataLoopNodes->Node(InletNodeNum).MassFlowRateMaxAvail > MassFlowRateOutSum + SmallMassFlow) {
                state.dataLoopNodes->Node(InletNodeNum).MassFlowRateMaxAvail = MassFlowRateOutSum;
            }
            // Pass the splitter inlet Max Avail mass flow rate upstream to the mixed air node
            for (NodeIndex = PrimaryAirSystems(SysNum).Branch(InBranchNum).TotalNodes - 1; NodeIndex >= 1; --NodeIndex) {
                NodeNum = PrimaryAirSystems(SysNum).Branch(InBranchNum).NodeNum(NodeIndex);
                state.dataLoopNodes->Node(NodeNum).MassFlowRateMaxAvail = state.dataLoopNodes->Node(InletNodeNum).MassFlowRateMaxAvail;
                if (NodeNum == PrimaryAirSystems(SysNum).OASysOutletNodeNum) break;
            }
        }

        // Make sure air system inlet nodes have flow consistent with MassFlowRateMaxAvail
        for (InBranchIndex = 1; InBranchIndex <= PrimaryAirSystems(SysNum).NumInletBranches; ++InBranchIndex) {
            InBranchNum = PrimaryAirSystems(SysNum).InletBranchNum(InBranchIndex);
            InNodeNum = PrimaryAirSystems(SysNum).Branch(InBranchNum).NodeNumIn;
            state.dataLoopNodes->Node(InNodeNum).MassFlowRate =
                min(state.dataLoopNodes->Node(InNodeNum).MassFlowRate, state.dataLoopNodes->Node(InNodeNum).MassFlowRateMaxAvail);
        }
    }
}

void SizeAirLoops(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   February 2001
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Will perform central air system sizing simulations. Right now just
    // initializes system sizing arrays. Calculations based on System Sizing
    // input and the Zone Sizing simulations are done in UpdateSysSizing.

    // METHODOLOGY EMPLOYED:
    // Will run purchased hot and chilled water type simulations to determine
    // central plant flow rates. Right now just uses one time flag to call
    // SetUpSysSizingArrays.

    if (state.dataSimAirServingZones->SizeAirLoopsOneTimeFlag) {
        SetUpSysSizingArrays(state);
        state.dataSimAirServingZones->SizeAirLoopsOneTimeFlag = false;
    }
}

void SizeAirLoopBranches(EnergyPlusData &state, int const AirLoopNum, int const BranchNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   September 2001
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine is for sizing air loop branches for which flow rates have not been
    // specified in the input.

    // METHODOLOGY EMPLOYED:
    // Obtains flow rates from the zone or system sizing arrays.

    // Using/Aliasing
    using namespace DataSizing;
    using HVACHXAssistedCoolingCoil::GetHXCoilType;
    using HVACHXAssistedCoolingCoil::GetHXDXCoilName;
    using WaterCoils::SetCoilDesFlow;

    std::string CompType; // Component type
    std::string CompName; // Component name
    std::string CoilName;
    std::string CoilType;
    std::string ScalableSM;                    // scalable sizing methods label for reporting
    SimAirServingZones::CompType CompType_Num; // Numeric equivalent for CompType
    int CompNum;
    bool ErrorsFound;

    auto &FinalSysSizing(state.dataSize->FinalSysSizing);
    auto &PrimaryAirSystems(state.dataAirSystemsData->PrimaryAirSystems);

    ErrorsFound = false;

    if (BranchNum == 1) {

        if (PrimaryAirSystems(AirLoopNum).DesignVolFlowRate == AutoSize) {
            CheckSysSizing(state, "AirLoopHVAC", PrimaryAirSystems(AirLoopNum).Name);
            PrimaryAirSystems(AirLoopNum).DesignVolFlowRate = FinalSysSizing(AirLoopNum).DesMainVolFlow;

            {
                auto const SELECT_CASE_var(FinalSysSizing(AirLoopNum).ScaleCoolSAFMethod);
                if (SELECT_CASE_var == FlowPerFloorArea) {
                    ScalableSM = "User-Specified(scaled by flow / area) ";
                } else if (SELECT_CASE_var == FractionOfAutosizedCoolingAirflow) {
                    ScalableSM = "User-Specified(scaled by fractional multiplier) ";
                } else if (SELECT_CASE_var == FlowPerCoolingCapacity) {
                    ScalableSM = "User-Specified(scaled by flow / capacity) ";
                } else {
                    ScalableSM = "Design ";
                }
            }
            BaseSizer::reportSizerOutput(state,
                                         "AirLoopHVAC",
                                         PrimaryAirSystems(AirLoopNum).Name,
                                         ScalableSM + "Supply Air Flow Rate [m3/s]",
                                         PrimaryAirSystems(AirLoopNum).DesignVolFlowRate);
            // Initialize MaxOutAir for DOAS loops with no actual OASys, systems with an OA controller will overwrite this is CalcOAController
            if (PrimaryAirSystems(AirLoopNum).isAllOA)
                state.dataAirLoop->AirLoopFlow(AirLoopNum).MaxOutAir = PrimaryAirSystems(AirLoopNum).DesignVolFlowRate * state.dataEnvrn->StdRhoAir;
        }

        if (allocated(FinalSysSizing) && FinalSysSizing(AirLoopNum).SysAirMinFlowRatWasAutoSized) {
            BaseSizer::reportSizerOutput(state,
                                         "AirLoopHVAC",
                                         PrimaryAirSystems(AirLoopNum).Name,
                                         "Central Heating Maximum System Air Flow Ratio",
                                         FinalSysSizing(AirLoopNum).SysAirMinFlowRat);
        }

        if (PrimaryAirSystems(AirLoopNum).DesignVolFlowRate < SmallAirVolFlow) {
            ShowSevereError(state,
                            format("SizeAirLoopBranches: AirLoopHVAC {} has air flow less than {:.4R} m3/s.",
                                   PrimaryAirSystems(AirLoopNum).Name,
                                   SmallAirVolFlow));
            ShowContinueError(state,
                              format("Primary air system volumetric flow rate = {:.4R} m3/s.", PrimaryAirSystems(AirLoopNum).DesignVolFlowRate));
            ShowContinueError(state, "Check flow rate inputs for components in this air loop and,");
            ShowContinueError(state, "if autosized, check Sizing:Zone and Sizing:System objects and related inputs.");
            ShowFatalError(state, "Previous condition causes termination.");
        }
    }

    // Loop over components in branch; pass the design air flow rate to the coil components that don't have
    // design air flow as an input
    for (CompNum = 1; CompNum <= PrimaryAirSystems(AirLoopNum).Branch(BranchNum).TotalComponents; ++CompNum) {
        CompType = PrimaryAirSystems(AirLoopNum).Branch(BranchNum).Comp(CompNum).TypeOf;
        CompName = PrimaryAirSystems(AirLoopNum).Branch(BranchNum).Comp(CompNum).Name;
        CompType_Num = PrimaryAirSystems(AirLoopNum).Branch(BranchNum).Comp(CompNum).CompType_Num;
        if (CompType_Num == CompType::WaterCoil_DetailedCool || CompType_Num == CompType::WaterCoil_SimpleHeat ||
            CompType_Num == CompType::WaterCoil_CoolingHXAsst) {
            if (CompType_Num == CompType::WaterCoil_CoolingHXAsst) {
                CoilName = GetHXDXCoilName(state, CompType, CompName, ErrorsFound);
                CoilType = GetHXCoilType(state, CompType, CompName, ErrorsFound);
            } else {
                CoilName = CompName;
                CoilType = CompType;
            }
            SetCoilDesFlow(state, CoilType, CoilName, PrimaryAirSystems(AirLoopNum).DesignVolFlowRate, ErrorsFound);
        }
    } // End of component loop
    if (ErrorsFound) {
        ShowFatalError(state, "Preceding sizing errors cause program termination");
    }
}

void SetUpSysSizingArrays(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   February 2001
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Allocate and fill the SysSizing data array.

    // METHODOLOGY EMPLOYED:
    // Uses data from System Sizing input and the system to zone connection data
    // calculated in InitAirLoops and stored in AirToZoneNodeInfo in DataLoopNode..

    bool ErrorsFound(false); // Set to true if errors in input, fatal at end of routine

    auto &CalcSysSizing(state.dataSize->CalcSysSizing);
    auto &SysSizing(state.dataSize->SysSizing);
    auto &FinalSysSizing(state.dataSize->FinalSysSizing);
    auto &NumAirTerminalUnits(state.dataSize->NumAirTerminalUnits);
    auto &SysSizInput(state.dataSize->SysSizInput);
    auto &PrimaryAirSystems(state.dataAirSystemsData->PrimaryAirSystems);
    auto &SysSizPeakDDNum(state.dataSize->SysSizPeakDDNum);
    auto &NumPrimaryAirSys(state.dataHVACGlobal->NumPrimaryAirSys);
    // have moved a large number of std 62.1 variables to DataSizing.hh so they can be used outside of this routine

    // allocate arrays used to store values for standard 62.1 tabular report
    if (!allocated(state.dataSize->VpzClgByZone)) {
        state.dataSize->VdzClgByZone.dimension(NumAirTerminalUnits, 0.0);
        state.dataSize->VdzMinClgByZone.dimension(NumAirTerminalUnits, 0.0);
        state.dataSize->VdzHtgByZone.dimension(NumAirTerminalUnits, 0.0);
        state.dataSize->VdzMinHtgByZone.dimension(NumAirTerminalUnits, 0.0);
        state.dataSize->ZdzClgByZone.dimension(NumAirTerminalUnits, 0.0);
        state.dataSize->ZdzHtgByZone.dimension(NumAirTerminalUnits, 0.0);
        state.dataSize->VpzClgByZone.dimension(NumAirTerminalUnits, 0.0);
        state.dataSize->VpzMinClgByZone.dimension(NumAirTerminalUnits, 0.0);
        state.dataSize->VpzHtgByZone.dimension(NumAirTerminalUnits, 0.0);
        state.dataSize->VpzMinHtgByZone.dimension(NumAirTerminalUnits, 0.0);
        state.dataSize->VpzClgSumBySys.dimension(NumPrimaryAirSys, 0.0);
        state.dataSize->VpzHtgSumBySys.dimension(NumPrimaryAirSys, 0.0);
        state.dataSize->VbzByZone.dimension(NumAirTerminalUnits, 0.0);
        state.dataSize->PzSumBySys.dimension(NumPrimaryAirSys, 0.0);
        state.dataSize->PsBySys.dimension(NumPrimaryAirSys, 0.0);
        state.dataSize->DBySys.dimension(NumPrimaryAirSys, 0.0);
        state.dataSize->SumRpxPzBySys.dimension(NumPrimaryAirSys, 0.0);
        state.dataSize->SumRaxAzBySys.dimension(NumPrimaryAirSys, 0.0);
        state.dataSize->PeakPsOccurrenceDateTimeStringBySys.dimension(NumPrimaryAirSys, "");
        state.dataSize->PeakPsOccurrenceEnvironmentStringBySys.dimension(NumPrimaryAirSys, "");
        state.dataSize->VouBySys.dimension(NumPrimaryAirSys, 0.0);
        state.dataSize->VpsClgBySys.dimension(NumPrimaryAirSys, 0.0);
        state.dataSize->VpsHtgBySys.dimension(NumPrimaryAirSys, 0.0);
    }

    for (int SysSizIndex = 1; SysSizIndex <= state.dataSize->NumSysSizInput; ++SysSizIndex) {
        int PrimAirIndex = UtilityRoutines::FindItemInList(SysSizInput(SysSizIndex).AirPriLoopName, PrimaryAirSystems);
        if (PrimAirIndex == 0) {
            ShowSevereError(state, "Sizing:System: " + SysSizInput(SysSizIndex).AirPriLoopName + " references unknown AirLoopHVAC");
            ErrorsFound = true;
        } else {
            SysSizInput(SysSizIndex).AirLoopNum = PrimAirIndex;
        }
    }
    if (ErrorsFound) {
        ShowFatalError(state, "Errors found in Sizing:System input");
    }

    SysSizing.allocate(state.dataEnvrn->TotDesDays + state.dataEnvrn->TotRunDesPersDays, NumPrimaryAirSys);
    FinalSysSizing.allocate(NumPrimaryAirSys);
    CalcSysSizing.allocate(NumPrimaryAirSys);
    SysSizPeakDDNum.allocate(NumPrimaryAirSys);

    for (int DesDayEnvrnNum = 1; DesDayEnvrnNum <= state.dataEnvrn->TotDesDays + state.dataEnvrn->TotRunDesPersDays; ++DesDayEnvrnNum) {
        for (int AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum) {

            SysSizing(DesDayEnvrnNum, AirLoopNum).AirPriLoopName = PrimaryAirSystems(AirLoopNum).Name;
            int SysSizNum = UtilityRoutines::FindItemInList(
                SysSizing(DesDayEnvrnNum, AirLoopNum).AirPriLoopName, SysSizInput, &SystemSizingInputData::AirPriLoopName);
            if (SysSizNum > 0) { // move data from system sizing input
                SysSizing(DesDayEnvrnNum, AirLoopNum).LoadSizeType = SysSizInput(SysSizNum).LoadSizeType;
                SysSizing(DesDayEnvrnNum, AirLoopNum).CoolingPeakLoadType = SysSizInput(SysSizNum).CoolingPeakLoadType;
                SysSizing(DesDayEnvrnNum, AirLoopNum).CoolCapControl = SysSizInput(SysSizNum).CoolCapControl;
                SysSizing(DesDayEnvrnNum, AirLoopNum).DesOutAirVolFlow = SysSizInput(SysSizNum).DesOutAirVolFlow;
                SysSizing(DesDayEnvrnNum, AirLoopNum).SysAirMinFlowRat = SysSizInput(SysSizNum).SysAirMinFlowRat;
                SysSizing(DesDayEnvrnNum, AirLoopNum).SysAirMinFlowRatWasAutoSized = SysSizInput(SysSizNum).SysAirMinFlowRatWasAutoSized;
                SysSizing(DesDayEnvrnNum, AirLoopNum).PreheatTemp = SysSizInput(SysSizNum).PreheatTemp;
                SysSizing(DesDayEnvrnNum, AirLoopNum).PreheatHumRat = SysSizInput(SysSizNum).PreheatHumRat;
                SysSizing(DesDayEnvrnNum, AirLoopNum).PrecoolTemp = SysSizInput(SysSizNum).PrecoolTemp;
                SysSizing(DesDayEnvrnNum, AirLoopNum).PrecoolHumRat = SysSizInput(SysSizNum).PrecoolHumRat;
                SysSizing(DesDayEnvrnNum, AirLoopNum).CoolSupTemp = SysSizInput(SysSizNum).CoolSupTemp;
                SysSizing(DesDayEnvrnNum, AirLoopNum).HeatSupTemp = SysSizInput(SysSizNum).HeatSupTemp;
                SysSizing(DesDayEnvrnNum, AirLoopNum).CoolSupHumRat = SysSizInput(SysSizNum).CoolSupHumRat;
                SysSizing(DesDayEnvrnNum, AirLoopNum).HeatSupHumRat = SysSizInput(SysSizNum).HeatSupHumRat;
                SysSizing(DesDayEnvrnNum, AirLoopNum).SizingOption = SysSizInput(SysSizNum).SizingOption;
                if (PrimaryAirSystems(AirLoopNum).isAllOA) {
                    SysSizing(DesDayEnvrnNum, AirLoopNum).CoolOAOption = AllOA;
                    SysSizing(DesDayEnvrnNum, AirLoopNum).HeatOAOption = AllOA;
                } else {
                    SysSizing(DesDayEnvrnNum, AirLoopNum).CoolOAOption = SysSizInput(SysSizNum).CoolOAOption;
                    SysSizing(DesDayEnvrnNum, AirLoopNum).HeatOAOption = SysSizInput(SysSizNum).HeatOAOption;
                }
                SysSizing(DesDayEnvrnNum, AirLoopNum).CoolAirDesMethod = SysSizInput(SysSizNum).CoolAirDesMethod;
                SysSizing(DesDayEnvrnNum, AirLoopNum).HeatAirDesMethod = SysSizInput(SysSizNum).HeatAirDesMethod;
                SysSizing(DesDayEnvrnNum, AirLoopNum).ScaleCoolSAFMethod = SysSizInput(SysSizNum).ScaleCoolSAFMethod;
                SysSizing(DesDayEnvrnNum, AirLoopNum).ScaleHeatSAFMethod = SysSizInput(SysSizNum).ScaleHeatSAFMethod;
                SysSizing(DesDayEnvrnNum, AirLoopNum).CoolingCapMethod = SysSizInput(SysSizNum).CoolingCapMethod;
                SysSizing(DesDayEnvrnNum, AirLoopNum).HeatingCapMethod = SysSizInput(SysSizNum).HeatingCapMethod;
                SysSizing(DesDayEnvrnNum, AirLoopNum).InpDesCoolAirFlow = SysSizInput(SysSizNum).DesCoolAirFlow;
                SysSizing(DesDayEnvrnNum, AirLoopNum).InpDesHeatAirFlow = SysSizInput(SysSizNum).DesHeatAirFlow;
                SysSizing(DesDayEnvrnNum, AirLoopNum).MaxZoneOAFraction = SysSizInput(SysSizNum).MaxZoneOAFraction;
                SysSizing(DesDayEnvrnNum, AirLoopNum).OAAutoSized = SysSizInput(SysSizNum).OAAutoSized;

            } else { // Set missing inputs to the first
                SysSizing(DesDayEnvrnNum, AirLoopNum).LoadSizeType = SysSizInput(1).LoadSizeType;
                SysSizing(DesDayEnvrnNum, AirLoopNum).CoolingPeakLoadType = SysSizInput(1).CoolingPeakLoadType;
                SysSizing(DesDayEnvrnNum, AirLoopNum).CoolCapControl = SysSizInput(1).CoolCapControl;
                SysSizing(DesDayEnvrnNum, AirLoopNum).DesOutAirVolFlow = SysSizInput(1).DesOutAirVolFlow;
                SysSizing(DesDayEnvrnNum, AirLoopNum).SysAirMinFlowRat = SysSizInput(1).SysAirMinFlowRat;
                SysSizing(DesDayEnvrnNum, AirLoopNum).SysAirMinFlowRatWasAutoSized = SysSizInput(1).SysAirMinFlowRatWasAutoSized;
                SysSizing(DesDayEnvrnNum, AirLoopNum).PreheatTemp = SysSizInput(1).PreheatTemp;
                SysSizing(DesDayEnvrnNum, AirLoopNum).PreheatHumRat = SysSizInput(1).PreheatHumRat;
                SysSizing(DesDayEnvrnNum, AirLoopNum).PrecoolTemp = SysSizInput(1).PrecoolTemp;
                SysSizing(DesDayEnvrnNum, AirLoopNum).PrecoolHumRat = SysSizInput(1).PrecoolHumRat;
                SysSizing(DesDayEnvrnNum, AirLoopNum).CoolSupTemp = SysSizInput(1).CoolSupTemp;
                SysSizing(DesDayEnvrnNum, AirLoopNum).HeatSupTemp = SysSizInput(1).HeatSupTemp;
                SysSizing(DesDayEnvrnNum, AirLoopNum).CoolSupHumRat = SysSizInput(1).CoolSupHumRat;
                SysSizing(DesDayEnvrnNum, AirLoopNum).HeatSupHumRat = SysSizInput(1).HeatSupHumRat;
                SysSizing(DesDayEnvrnNum, AirLoopNum).SizingOption = SysSizInput(1).SizingOption;
                if (PrimaryAirSystems(AirLoopNum).isAllOA) {
                    SysSizing(DesDayEnvrnNum, AirLoopNum).CoolOAOption = AllOA;
                    SysSizing(DesDayEnvrnNum, AirLoopNum).HeatOAOption = AllOA;
                } else {
                    SysSizing(DesDayEnvrnNum, AirLoopNum).CoolOAOption = SysSizInput(1).CoolOAOption;
                    SysSizing(DesDayEnvrnNum, AirLoopNum).HeatOAOption = SysSizInput(1).HeatOAOption;
                }
                SysSizing(DesDayEnvrnNum, AirLoopNum).CoolAirDesMethod = SysSizInput(1).CoolAirDesMethod;
                SysSizing(DesDayEnvrnNum, AirLoopNum).HeatAirDesMethod = SysSizInput(1).HeatAirDesMethod;
                SysSizing(DesDayEnvrnNum, AirLoopNum).ScaleCoolSAFMethod = SysSizInput(1).ScaleCoolSAFMethod;
                SysSizing(DesDayEnvrnNum, AirLoopNum).ScaleHeatSAFMethod = SysSizInput(1).ScaleHeatSAFMethod;
                SysSizing(DesDayEnvrnNum, AirLoopNum).CoolingCapMethod = SysSizInput(1).CoolingCapMethod;
                SysSizing(DesDayEnvrnNum, AirLoopNum).HeatingCapMethod = SysSizInput(1).HeatingCapMethod;
                SysSizing(DesDayEnvrnNum, AirLoopNum).InpDesCoolAirFlow = SysSizInput(1).DesCoolAirFlow;
                SysSizing(DesDayEnvrnNum, AirLoopNum).InpDesHeatAirFlow = SysSizInput(1).DesHeatAirFlow;
                SysSizing(DesDayEnvrnNum, AirLoopNum).MaxZoneOAFraction = SysSizInput(1).MaxZoneOAFraction;
                SysSizing(DesDayEnvrnNum, AirLoopNum).OAAutoSized = SysSizInput(1).OAAutoSized;
            }
            SysSizing(DesDayEnvrnNum, AirLoopNum).HeatFlowSeq.dimension(state.dataSimAirServingZones->NumOfTimeStepInDay, 0.0);
            SysSizing(DesDayEnvrnNum, AirLoopNum).SumZoneHeatLoadSeq.dimension(state.dataSimAirServingZones->NumOfTimeStepInDay, 0.0);
            SysSizing(DesDayEnvrnNum, AirLoopNum).CoolFlowSeq.dimension(state.dataSimAirServingZones->NumOfTimeStepInDay, 0.0);
            SysSizing(DesDayEnvrnNum, AirLoopNum).SumZoneCoolLoadSeq.dimension(state.dataSimAirServingZones->NumOfTimeStepInDay, 0.0);
            SysSizing(DesDayEnvrnNum, AirLoopNum).CoolZoneAvgTempSeq.dimension(state.dataSimAirServingZones->NumOfTimeStepInDay, 0.0);
            SysSizing(DesDayEnvrnNum, AirLoopNum).HeatZoneAvgTempSeq.dimension(state.dataSimAirServingZones->NumOfTimeStepInDay, 0.0);
            SysSizing(DesDayEnvrnNum, AirLoopNum).SensCoolCapSeq.dimension(state.dataSimAirServingZones->NumOfTimeStepInDay, 0.0);
            SysSizing(DesDayEnvrnNum, AirLoopNum).TotCoolCapSeq.dimension(state.dataSimAirServingZones->NumOfTimeStepInDay, 0.0);
            SysSizing(DesDayEnvrnNum, AirLoopNum).HeatCapSeq.dimension(state.dataSimAirServingZones->NumOfTimeStepInDay, 0.0);
            SysSizing(DesDayEnvrnNum, AirLoopNum).PreheatCapSeq.dimension(state.dataSimAirServingZones->NumOfTimeStepInDay, 0.0);
            SysSizing(DesDayEnvrnNum, AirLoopNum).SysCoolRetTempSeq.dimension(state.dataSimAirServingZones->NumOfTimeStepInDay, 0.0);
            SysSizing(DesDayEnvrnNum, AirLoopNum).SysCoolRetHumRatSeq.dimension(state.dataSimAirServingZones->NumOfTimeStepInDay, 0.0);
            SysSizing(DesDayEnvrnNum, AirLoopNum).SysHeatRetTempSeq.dimension(state.dataSimAirServingZones->NumOfTimeStepInDay, 0.0);
            SysSizing(DesDayEnvrnNum, AirLoopNum).SysHeatRetHumRatSeq.dimension(state.dataSimAirServingZones->NumOfTimeStepInDay, 0.0);
            SysSizing(DesDayEnvrnNum, AirLoopNum).SysCoolOutTempSeq.dimension(state.dataSimAirServingZones->NumOfTimeStepInDay, 0.0);
            SysSizing(DesDayEnvrnNum, AirLoopNum).SysCoolOutHumRatSeq.dimension(state.dataSimAirServingZones->NumOfTimeStepInDay, 0.0);
            SysSizing(DesDayEnvrnNum, AirLoopNum).SysHeatOutTempSeq.dimension(state.dataSimAirServingZones->NumOfTimeStepInDay, 0.0);
            SysSizing(DesDayEnvrnNum, AirLoopNum).SysHeatOutHumRatSeq.dimension(state.dataSimAirServingZones->NumOfTimeStepInDay, 0.0);
            SysSizing(DesDayEnvrnNum, AirLoopNum).SysDOASHeatAddSeq.dimension(state.dataSimAirServingZones->NumOfTimeStepInDay, 0.0);
            SysSizing(DesDayEnvrnNum, AirLoopNum).SysDOASLatAddSeq.dimension(state.dataSimAirServingZones->NumOfTimeStepInDay, 0.0);
        } // end the primary air system loop
    }

    for (int AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum) {

        FinalSysSizing(AirLoopNum).AirPriLoopName = PrimaryAirSystems(AirLoopNum).Name;
        CalcSysSizing(AirLoopNum).AirPriLoopName = PrimaryAirSystems(AirLoopNum).Name;
        int SysSizNum =
            UtilityRoutines::FindItemInList(FinalSysSizing(AirLoopNum).AirPriLoopName, SysSizInput, &SystemSizingInputData::AirPriLoopName);
        if (SysSizNum > 0) { // move data from system sizing input
            FinalSysSizing(AirLoopNum).LoadSizeType = SysSizInput(SysSizNum).LoadSizeType;
            FinalSysSizing(AirLoopNum).CoolingPeakLoadType = SysSizInput(SysSizNum).CoolingPeakLoadType;
            FinalSysSizing(AirLoopNum).CoolCapControl = SysSizInput(SysSizNum).CoolCapControl;
            FinalSysSizing(AirLoopNum).DesOutAirVolFlow = SysSizInput(SysSizNum).DesOutAirVolFlow;
            FinalSysSizing(AirLoopNum).SysAirMinFlowRat = SysSizInput(SysSizNum).SysAirMinFlowRat;
            FinalSysSizing(AirLoopNum).SysAirMinFlowRatWasAutoSized = SysSizInput(SysSizNum).SysAirMinFlowRatWasAutoSized;
            FinalSysSizing(AirLoopNum).PreheatTemp = SysSizInput(SysSizNum).PreheatTemp;
            FinalSysSizing(AirLoopNum).PreheatHumRat = SysSizInput(SysSizNum).PreheatHumRat;
            FinalSysSizing(AirLoopNum).PrecoolTemp = SysSizInput(SysSizNum).PrecoolTemp;
            FinalSysSizing(AirLoopNum).PrecoolHumRat = SysSizInput(SysSizNum).PrecoolHumRat;
            FinalSysSizing(AirLoopNum).CoolSupTemp = SysSizInput(SysSizNum).CoolSupTemp;
            FinalSysSizing(AirLoopNum).HeatSupTemp = SysSizInput(SysSizNum).HeatSupTemp;
            FinalSysSizing(AirLoopNum).CoolSupHumRat = SysSizInput(SysSizNum).CoolSupHumRat;
            FinalSysSizing(AirLoopNum).HeatSupHumRat = SysSizInput(SysSizNum).HeatSupHumRat;
            FinalSysSizing(AirLoopNum).SizingOption = SysSizInput(SysSizNum).SizingOption;
            if (PrimaryAirSystems(AirLoopNum).isAllOA) {
                FinalSysSizing(AirLoopNum).CoolOAOption = AllOA;
                FinalSysSizing(AirLoopNum).HeatOAOption = AllOA;
            } else {
                FinalSysSizing(AirLoopNum).CoolOAOption = SysSizInput(SysSizNum).CoolOAOption;
                FinalSysSizing(AirLoopNum).HeatOAOption = SysSizInput(SysSizNum).HeatOAOption;
            }
            FinalSysSizing(AirLoopNum).CoolAirDesMethod = SysSizInput(SysSizNum).CoolAirDesMethod;
            FinalSysSizing(AirLoopNum).HeatAirDesMethod = SysSizInput(SysSizNum).HeatAirDesMethod;
            FinalSysSizing(AirLoopNum).ScaleCoolSAFMethod = SysSizInput(SysSizNum).ScaleCoolSAFMethod;
            FinalSysSizing(AirLoopNum).ScaleHeatSAFMethod = SysSizInput(SysSizNum).ScaleHeatSAFMethod;
            FinalSysSizing(AirLoopNum).CoolingCapMethod = SysSizInput(SysSizNum).CoolingCapMethod;
            FinalSysSizing(AirLoopNum).HeatingCapMethod = SysSizInput(SysSizNum).HeatingCapMethod;

            FinalSysSizing(AirLoopNum).ScaledCoolingCapacity = SysSizInput(SysSizNum).ScaledCoolingCapacity;
            FinalSysSizing(AirLoopNum).ScaledHeatingCapacity = SysSizInput(SysSizNum).ScaledHeatingCapacity;

            FinalSysSizing(AirLoopNum).InpDesCoolAirFlow = SysSizInput(SysSizNum).DesCoolAirFlow;
            FinalSysSizing(AirLoopNum).InpDesHeatAirFlow = SysSizInput(SysSizNum).DesHeatAirFlow;
            FinalSysSizing(AirLoopNum).SystemOAMethod = SysSizInput(SysSizNum).SystemOAMethod;
            FinalSysSizing(AirLoopNum).MaxZoneOAFraction = SysSizInput(SysSizNum).MaxZoneOAFraction;
            FinalSysSizing(AirLoopNum).OAAutoSized = SysSizInput(SysSizNum).OAAutoSized;

            FinalSysSizing(AirLoopNum).FlowPerFloorAreaCooled = SysSizInput(SysSizNum).FlowPerFloorAreaCooled;
            FinalSysSizing(AirLoopNum).FlowPerFloorAreaHeated = SysSizInput(SysSizNum).FlowPerFloorAreaHeated;
            FinalSysSizing(AirLoopNum).FractionOfAutosizedCoolingAirflow = SysSizInput(SysSizNum).FractionOfAutosizedCoolingAirflow;
            FinalSysSizing(AirLoopNum).FractionOfAutosizedHeatingAirflow = SysSizInput(SysSizNum).FractionOfAutosizedHeatingAirflow;
            FinalSysSizing(AirLoopNum).FlowPerCoolingCapacity = SysSizInput(SysSizNum).FlowPerCoolingCapacity;
            FinalSysSizing(AirLoopNum).FlowPerHeatingCapacity = SysSizInput(SysSizNum).FlowPerHeatingCapacity;

            CalcSysSizing(AirLoopNum).LoadSizeType = SysSizInput(SysSizNum).LoadSizeType;
            CalcSysSizing(AirLoopNum).CoolingPeakLoadType = SysSizInput(SysSizNum).CoolingPeakLoadType;
            CalcSysSizing(AirLoopNum).CoolCapControl = SysSizInput(SysSizNum).CoolCapControl;
            CalcSysSizing(AirLoopNum).DesOutAirVolFlow = SysSizInput(SysSizNum).DesOutAirVolFlow;
            CalcSysSizing(AirLoopNum).SysAirMinFlowRat = SysSizInput(SysSizNum).SysAirMinFlowRat;
            CalcSysSizing(AirLoopNum).SysAirMinFlowRatWasAutoSized = SysSizInput(SysSizNum).SysAirMinFlowRatWasAutoSized;
            CalcSysSizing(AirLoopNum).PreheatTemp = SysSizInput(SysSizNum).PreheatTemp;
            CalcSysSizing(AirLoopNum).PreheatHumRat = SysSizInput(SysSizNum).PreheatHumRat;
            CalcSysSizing(AirLoopNum).PrecoolTemp = SysSizInput(SysSizNum).PrecoolTemp;
            CalcSysSizing(AirLoopNum).PrecoolHumRat = SysSizInput(SysSizNum).PrecoolHumRat;
            CalcSysSizing(AirLoopNum).CoolSupTemp = SysSizInput(SysSizNum).CoolSupTemp;
            CalcSysSizing(AirLoopNum).HeatSupTemp = SysSizInput(SysSizNum).HeatSupTemp;
            CalcSysSizing(AirLoopNum).CoolSupHumRat = SysSizInput(SysSizNum).CoolSupHumRat;
            CalcSysSizing(AirLoopNum).HeatSupHumRat = SysSizInput(SysSizNum).HeatSupHumRat;
            CalcSysSizing(AirLoopNum).SizingOption = SysSizInput(SysSizNum).SizingOption;
            if (PrimaryAirSystems(AirLoopNum).isAllOA) {
                CalcSysSizing(AirLoopNum).CoolOAOption = AllOA;
                CalcSysSizing(AirLoopNum).HeatOAOption = AllOA;
            } else {
                CalcSysSizing(AirLoopNum).CoolOAOption = SysSizInput(SysSizNum).CoolOAOption;
                CalcSysSizing(AirLoopNum).HeatOAOption = SysSizInput(SysSizNum).HeatOAOption;
            }
            CalcSysSizing(AirLoopNum).CoolAirDesMethod = SysSizInput(SysSizNum).CoolAirDesMethod;
            CalcSysSizing(AirLoopNum).HeatAirDesMethod = SysSizInput(SysSizNum).HeatAirDesMethod;
            CalcSysSizing(AirLoopNum).ScaleCoolSAFMethod = SysSizInput(SysSizNum).ScaleCoolSAFMethod;
            CalcSysSizing(AirLoopNum).ScaleHeatSAFMethod = SysSizInput(SysSizNum).ScaleHeatSAFMethod;
            CalcSysSizing(AirLoopNum).CoolingCapMethod = SysSizInput(SysSizNum).CoolingCapMethod;
            CalcSysSizing(AirLoopNum).HeatingCapMethod = SysSizInput(SysSizNum).HeatingCapMethod;
            CalcSysSizing(AirLoopNum).ScaledCoolingCapacity = SysSizInput(SysSizNum).ScaledCoolingCapacity;
            CalcSysSizing(AirLoopNum).ScaledHeatingCapacity = SysSizInput(SysSizNum).ScaledHeatingCapacity;

            CalcSysSizing(AirLoopNum).InpDesCoolAirFlow = SysSizInput(SysSizNum).DesCoolAirFlow;
            CalcSysSizing(AirLoopNum).InpDesHeatAirFlow = SysSizInput(SysSizNum).DesHeatAirFlow;
            CalcSysSizing(AirLoopNum).SystemOAMethod = SysSizInput(SysSizNum).SystemOAMethod;
            CalcSysSizing(AirLoopNum).MaxZoneOAFraction = SysSizInput(SysSizNum).MaxZoneOAFraction;
            CalcSysSizing(AirLoopNum).OAAutoSized = SysSizInput(SysSizNum).OAAutoSized;
            CalcSysSizing(AirLoopNum).FlowPerFloorAreaCooled = SysSizInput(SysSizNum).FlowPerFloorAreaCooled;
            CalcSysSizing(AirLoopNum).FlowPerFloorAreaHeated = SysSizInput(SysSizNum).FlowPerFloorAreaHeated;
            CalcSysSizing(AirLoopNum).FractionOfAutosizedCoolingAirflow = SysSizInput(SysSizNum).FractionOfAutosizedCoolingAirflow;
            CalcSysSizing(AirLoopNum).FractionOfAutosizedHeatingAirflow = SysSizInput(SysSizNum).FractionOfAutosizedHeatingAirflow;
            CalcSysSizing(AirLoopNum).FlowPerCoolingCapacity = SysSizInput(SysSizNum).FlowPerCoolingCapacity;
            CalcSysSizing(AirLoopNum).FlowPerHeatingCapacity = SysSizInput(SysSizNum).FlowPerHeatingCapacity;

        } else { // Set missing inputs to the first
            ShowWarningError(state,
                             "SetUpSysSizingArrays: Sizing for System (HVACAirLoop)=\"" + FinalSysSizing(AirLoopNum).AirPriLoopName +
                                 "\" will use Sizing:System specifications listed for System=\"" + SysSizInput(1).AirPriLoopName + "\".");
            FinalSysSizing(AirLoopNum).LoadSizeType = SysSizInput(1).LoadSizeType;
            FinalSysSizing(AirLoopNum).CoolingPeakLoadType = SysSizInput(1).CoolingPeakLoadType;
            FinalSysSizing(AirLoopNum).CoolCapControl = SysSizInput(1).CoolCapControl;
            FinalSysSizing(AirLoopNum).DesOutAirVolFlow = SysSizInput(1).DesOutAirVolFlow;
            FinalSysSizing(AirLoopNum).SysAirMinFlowRat = SysSizInput(1).SysAirMinFlowRat;
            FinalSysSizing(AirLoopNum).SysAirMinFlowRatWasAutoSized = SysSizInput(1).SysAirMinFlowRatWasAutoSized;
            FinalSysSizing(AirLoopNum).PreheatTemp = SysSizInput(1).PreheatTemp;
            FinalSysSizing(AirLoopNum).PreheatHumRat = SysSizInput(1).PreheatHumRat;
            FinalSysSizing(AirLoopNum).PrecoolTemp = SysSizInput(1).PrecoolTemp;
            FinalSysSizing(AirLoopNum).PrecoolHumRat = SysSizInput(1).PrecoolHumRat;
            FinalSysSizing(AirLoopNum).CoolSupTemp = SysSizInput(1).CoolSupTemp;
            FinalSysSizing(AirLoopNum).HeatSupTemp = SysSizInput(1).HeatSupTemp;
            FinalSysSizing(AirLoopNum).CoolSupHumRat = SysSizInput(1).CoolSupHumRat;
            FinalSysSizing(AirLoopNum).HeatSupHumRat = SysSizInput(1).HeatSupHumRat;
            FinalSysSizing(AirLoopNum).SizingOption = SysSizInput(1).SizingOption;
            if (PrimaryAirSystems(AirLoopNum).isAllOA) {
                FinalSysSizing(AirLoopNum).CoolOAOption = AllOA;
                FinalSysSizing(AirLoopNum).HeatOAOption = AllOA;
            } else {
                FinalSysSizing(AirLoopNum).CoolOAOption = SysSizInput(1).CoolOAOption;
                FinalSysSizing(AirLoopNum).HeatOAOption = SysSizInput(1).HeatOAOption;
            }
            FinalSysSizing(AirLoopNum).CoolAirDesMethod = SysSizInput(1).CoolAirDesMethod;
            FinalSysSizing(AirLoopNum).HeatAirDesMethod = SysSizInput(1).HeatAirDesMethod;
            FinalSysSizing(AirLoopNum).ScaleCoolSAFMethod = SysSizInput(1).ScaleCoolSAFMethod;
            FinalSysSizing(AirLoopNum).ScaleHeatSAFMethod = SysSizInput(1).ScaleHeatSAFMethod;
            FinalSysSizing(AirLoopNum).CoolingCapMethod = SysSizInput(1).CoolingCapMethod;
            FinalSysSizing(AirLoopNum).HeatingCapMethod = SysSizInput(1).HeatingCapMethod;
            FinalSysSizing(AirLoopNum).ScaledCoolingCapacity = SysSizInput(1).ScaledCoolingCapacity;
            FinalSysSizing(AirLoopNum).ScaledHeatingCapacity = SysSizInput(1).ScaledHeatingCapacity;

            FinalSysSizing(AirLoopNum).InpDesCoolAirFlow = SysSizInput(1).DesCoolAirFlow;
            FinalSysSizing(AirLoopNum).InpDesHeatAirFlow = SysSizInput(1).DesHeatAirFlow;
            FinalSysSizing(AirLoopNum).SystemOAMethod = SysSizInput(1).SystemOAMethod;
            FinalSysSizing(AirLoopNum).MaxZoneOAFraction = SysSizInput(1).MaxZoneOAFraction;
            FinalSysSizing(AirLoopNum).OAAutoSized = SysSizInput(1).OAAutoSized;

            FinalSysSizing(AirLoopNum).FlowPerFloorAreaCooled = SysSizInput(1).FlowPerFloorAreaCooled;
            FinalSysSizing(AirLoopNum).FlowPerFloorAreaHeated = SysSizInput(1).FlowPerFloorAreaHeated;
            FinalSysSizing(AirLoopNum).FractionOfAutosizedCoolingAirflow = SysSizInput(1).FractionOfAutosizedCoolingAirflow;
            FinalSysSizing(AirLoopNum).FractionOfAutosizedHeatingAirflow = SysSizInput(1).FractionOfAutosizedHeatingAirflow;
            FinalSysSizing(AirLoopNum).FlowPerCoolingCapacity = SysSizInput(1).FlowPerCoolingCapacity;
            FinalSysSizing(AirLoopNum).FlowPerHeatingCapacity = SysSizInput(1).FlowPerHeatingCapacity;

            CalcSysSizing(AirLoopNum).LoadSizeType = SysSizInput(1).LoadSizeType;
            CalcSysSizing(AirLoopNum).CoolingPeakLoadType = SysSizInput(1).CoolingPeakLoadType;
            CalcSysSizing(AirLoopNum).CoolCapControl = SysSizInput(1).CoolCapControl;
            CalcSysSizing(AirLoopNum).DesOutAirVolFlow = SysSizInput(1).DesOutAirVolFlow;
            CalcSysSizing(AirLoopNum).SysAirMinFlowRat = SysSizInput(1).SysAirMinFlowRat;
            CalcSysSizing(AirLoopNum).SysAirMinFlowRatWasAutoSized = SysSizInput(1).SysAirMinFlowRatWasAutoSized;
            CalcSysSizing(AirLoopNum).PreheatTemp = SysSizInput(1).PreheatTemp;
            CalcSysSizing(AirLoopNum).PreheatHumRat = SysSizInput(1).PreheatHumRat;
            CalcSysSizing(AirLoopNum).PrecoolTemp = SysSizInput(1).PrecoolTemp;
            CalcSysSizing(AirLoopNum).PrecoolHumRat = SysSizInput(1).PrecoolHumRat;
            CalcSysSizing(AirLoopNum).CoolSupTemp = SysSizInput(1).CoolSupTemp;
            CalcSysSizing(AirLoopNum).HeatSupTemp = SysSizInput(1).HeatSupTemp;
            CalcSysSizing(AirLoopNum).CoolSupHumRat = SysSizInput(1).CoolSupHumRat;
            CalcSysSizing(AirLoopNum).HeatSupHumRat = SysSizInput(1).HeatSupHumRat;
            CalcSysSizing(AirLoopNum).SizingOption = SysSizInput(1).SizingOption;
            if (PrimaryAirSystems(AirLoopNum).isAllOA) {
                CalcSysSizing(AirLoopNum).CoolOAOption = AllOA;
                CalcSysSizing(AirLoopNum).HeatOAOption = AllOA;
            } else {
                CalcSysSizing(AirLoopNum).CoolOAOption = SysSizInput(1).CoolOAOption;
                CalcSysSizing(AirLoopNum).HeatOAOption = SysSizInput(1).HeatOAOption;
            }
            CalcSysSizing(AirLoopNum).CoolAirDesMethod = SysSizInput(1).CoolAirDesMethod;
            CalcSysSizing(AirLoopNum).HeatAirDesMethod = SysSizInput(1).HeatAirDesMethod;
            CalcSysSizing(AirLoopNum).ScaleCoolSAFMethod = SysSizInput(1).ScaleCoolSAFMethod;
            CalcSysSizing(AirLoopNum).ScaleHeatSAFMethod = SysSizInput(1).ScaleHeatSAFMethod;
            CalcSysSizing(AirLoopNum).CoolingCapMethod = SysSizInput(1).CoolingCapMethod;
            CalcSysSizing(AirLoopNum).HeatingCapMethod = SysSizInput(1).HeatingCapMethod;
            CalcSysSizing(AirLoopNum).ScaledCoolingCapacity = SysSizInput(1).ScaledCoolingCapacity;
            CalcSysSizing(AirLoopNum).ScaledHeatingCapacity = SysSizInput(1).ScaledHeatingCapacity;
            CalcSysSizing(AirLoopNum).InpDesCoolAirFlow = SysSizInput(1).DesCoolAirFlow;
            CalcSysSizing(AirLoopNum).InpDesHeatAirFlow = SysSizInput(1).DesHeatAirFlow;
            CalcSysSizing(AirLoopNum).SystemOAMethod = SysSizInput(1).SystemOAMethod;
            CalcSysSizing(AirLoopNum).MaxZoneOAFraction = SysSizInput(1).MaxZoneOAFraction;
            CalcSysSizing(AirLoopNum).OAAutoSized = SysSizInput(1).OAAutoSized;

            CalcSysSizing(AirLoopNum).FlowPerFloorAreaCooled = SysSizInput(1).FlowPerFloorAreaCooled;
            CalcSysSizing(AirLoopNum).FlowPerFloorAreaHeated = SysSizInput(1).FlowPerFloorAreaHeated;
            CalcSysSizing(AirLoopNum).FractionOfAutosizedCoolingAirflow = SysSizInput(1).FractionOfAutosizedCoolingAirflow;
            CalcSysSizing(AirLoopNum).FractionOfAutosizedHeatingAirflow = SysSizInput(1).FractionOfAutosizedHeatingAirflow;
            CalcSysSizing(AirLoopNum).FlowPerCoolingCapacity = SysSizInput(1).FlowPerCoolingCapacity;
            CalcSysSizing(AirLoopNum).FlowPerHeatingCapacity = SysSizInput(1).FlowPerHeatingCapacity;
        }
        FinalSysSizing(AirLoopNum).HeatFlowSeq.allocate(state.dataSimAirServingZones->NumOfTimeStepInDay);
        FinalSysSizing(AirLoopNum).SumZoneHeatLoadSeq.allocate(state.dataSimAirServingZones->NumOfTimeStepInDay);
        FinalSysSizing(AirLoopNum).CoolFlowSeq.allocate(state.dataSimAirServingZones->NumOfTimeStepInDay);
        FinalSysSizing(AirLoopNum).SumZoneCoolLoadSeq.allocate(state.dataSimAirServingZones->NumOfTimeStepInDay);
        FinalSysSizing(AirLoopNum).CoolZoneAvgTempSeq.allocate(state.dataSimAirServingZones->NumOfTimeStepInDay);
        FinalSysSizing(AirLoopNum).HeatZoneAvgTempSeq.allocate(state.dataSimAirServingZones->NumOfTimeStepInDay);
        FinalSysSizing(AirLoopNum).SensCoolCapSeq.allocate(state.dataSimAirServingZones->NumOfTimeStepInDay);
        FinalSysSizing(AirLoopNum).TotCoolCapSeq.allocate(state.dataSimAirServingZones->NumOfTimeStepInDay);
        FinalSysSizing(AirLoopNum).HeatCapSeq.allocate(state.dataSimAirServingZones->NumOfTimeStepInDay);
        FinalSysSizing(AirLoopNum).PreheatCapSeq.allocate(state.dataSimAirServingZones->NumOfTimeStepInDay);
        FinalSysSizing(AirLoopNum).SysCoolRetTempSeq.allocate(state.dataSimAirServingZones->NumOfTimeStepInDay);
        FinalSysSizing(AirLoopNum).SysCoolRetHumRatSeq.allocate(state.dataSimAirServingZones->NumOfTimeStepInDay);
        FinalSysSizing(AirLoopNum).SysHeatRetTempSeq.allocate(state.dataSimAirServingZones->NumOfTimeStepInDay);
        FinalSysSizing(AirLoopNum).SysHeatRetHumRatSeq.allocate(state.dataSimAirServingZones->NumOfTimeStepInDay);
        FinalSysSizing(AirLoopNum).SysCoolOutTempSeq.allocate(state.dataSimAirServingZones->NumOfTimeStepInDay);
        FinalSysSizing(AirLoopNum).SysCoolOutHumRatSeq.allocate(state.dataSimAirServingZones->NumOfTimeStepInDay);
        FinalSysSizing(AirLoopNum).SysHeatOutTempSeq.allocate(state.dataSimAirServingZones->NumOfTimeStepInDay);
        FinalSysSizing(AirLoopNum).SysHeatOutHumRatSeq.allocate(state.dataSimAirServingZones->NumOfTimeStepInDay);
        FinalSysSizing(AirLoopNum).SysDOASHeatAddSeq.allocate(state.dataSimAirServingZones->NumOfTimeStepInDay);
        FinalSysSizing(AirLoopNum).SysDOASLatAddSeq.allocate(state.dataSimAirServingZones->NumOfTimeStepInDay);
        FinalSysSizing(AirLoopNum).HeatFlowSeq = 0.0;
        FinalSysSizing(AirLoopNum).SumZoneHeatLoadSeq = 0.0;
        FinalSysSizing(AirLoopNum).CoolFlowSeq = 0.0;
        FinalSysSizing(AirLoopNum).SumZoneCoolLoadSeq = 0.0;
        FinalSysSizing(AirLoopNum).CoolZoneAvgTempSeq = 0.0;
        FinalSysSizing(AirLoopNum).HeatZoneAvgTempSeq = 0.0;
        FinalSysSizing(AirLoopNum).SensCoolCapSeq = 0.0;
        FinalSysSizing(AirLoopNum).TotCoolCapSeq = 0.0;
        FinalSysSizing(AirLoopNum).HeatCapSeq = 0.0;
        FinalSysSizing(AirLoopNum).PreheatCapSeq = 0.0;
        FinalSysSizing(AirLoopNum).SysCoolRetTempSeq = 0.0;
        FinalSysSizing(AirLoopNum).SysCoolRetHumRatSeq = 0.0;
        FinalSysSizing(AirLoopNum).SysHeatRetTempSeq = 0.0;
        FinalSysSizing(AirLoopNum).SysHeatRetHumRatSeq = 0.0;
        FinalSysSizing(AirLoopNum).SysCoolOutTempSeq = 0.0;
        FinalSysSizing(AirLoopNum).SysCoolOutHumRatSeq = 0.0;
        FinalSysSizing(AirLoopNum).SysHeatOutTempSeq = 0.0;
        FinalSysSizing(AirLoopNum).SysHeatOutHumRatSeq = 0.0;
        FinalSysSizing(AirLoopNum).SysDOASHeatAddSeq = 0.0;
        FinalSysSizing(AirLoopNum).SysDOASLatAddSeq = 0.0;
        FinalSysSizing(AirLoopNum).FloorAreaOnAirLoopCooled = 0.0;
        FinalSysSizing(AirLoopNum).FloorAreaOnAirLoopHeated = 0.0;
        CalcSysSizing(AirLoopNum).HeatFlowSeq.allocate(state.dataSimAirServingZones->NumOfTimeStepInDay);
        CalcSysSizing(AirLoopNum).SumZoneHeatLoadSeq.allocate(state.dataSimAirServingZones->NumOfTimeStepInDay);
        CalcSysSizing(AirLoopNum).CoolFlowSeq.allocate(state.dataSimAirServingZones->NumOfTimeStepInDay);
        CalcSysSizing(AirLoopNum).SumZoneCoolLoadSeq.allocate(state.dataSimAirServingZones->NumOfTimeStepInDay);
        CalcSysSizing(AirLoopNum).CoolZoneAvgTempSeq.allocate(state.dataSimAirServingZones->NumOfTimeStepInDay);
        CalcSysSizing(AirLoopNum).HeatZoneAvgTempSeq.allocate(state.dataSimAirServingZones->NumOfTimeStepInDay);
        CalcSysSizing(AirLoopNum).SensCoolCapSeq.allocate(state.dataSimAirServingZones->NumOfTimeStepInDay);
        CalcSysSizing(AirLoopNum).TotCoolCapSeq.allocate(state.dataSimAirServingZones->NumOfTimeStepInDay);
        CalcSysSizing(AirLoopNum).HeatCapSeq.allocate(state.dataSimAirServingZones->NumOfTimeStepInDay);
        CalcSysSizing(AirLoopNum).PreheatCapSeq.allocate(state.dataSimAirServingZones->NumOfTimeStepInDay);
        CalcSysSizing(AirLoopNum).SysCoolRetTempSeq.allocate(state.dataSimAirServingZones->NumOfTimeStepInDay);
        CalcSysSizing(AirLoopNum).SysCoolRetHumRatSeq.allocate(state.dataSimAirServingZones->NumOfTimeStepInDay);
        CalcSysSizing(AirLoopNum).SysHeatRetTempSeq.allocate(state.dataSimAirServingZones->NumOfTimeStepInDay);
        CalcSysSizing(AirLoopNum).SysHeatRetHumRatSeq.allocate(state.dataSimAirServingZones->NumOfTimeStepInDay);
        CalcSysSizing(AirLoopNum).SysCoolOutTempSeq.allocate(state.dataSimAirServingZones->NumOfTimeStepInDay);
        CalcSysSizing(AirLoopNum).SysCoolOutHumRatSeq.allocate(state.dataSimAirServingZones->NumOfTimeStepInDay);
        CalcSysSizing(AirLoopNum).SysHeatOutTempSeq.allocate(state.dataSimAirServingZones->NumOfTimeStepInDay);
        CalcSysSizing(AirLoopNum).SysHeatOutHumRatSeq.allocate(state.dataSimAirServingZones->NumOfTimeStepInDay);
        CalcSysSizing(AirLoopNum).SysDOASHeatAddSeq.allocate(state.dataSimAirServingZones->NumOfTimeStepInDay);
        CalcSysSizing(AirLoopNum).SysDOASLatAddSeq.allocate(state.dataSimAirServingZones->NumOfTimeStepInDay);
        CalcSysSizing(AirLoopNum).HeatFlowSeq = 0.0;
        CalcSysSizing(AirLoopNum).SumZoneHeatLoadSeq = 0.0;
        CalcSysSizing(AirLoopNum).CoolFlowSeq = 0.0;
        CalcSysSizing(AirLoopNum).SumZoneCoolLoadSeq = 0.0;
        CalcSysSizing(AirLoopNum).CoolZoneAvgTempSeq = 0.0;
        CalcSysSizing(AirLoopNum).HeatZoneAvgTempSeq = 0.0;
        CalcSysSizing(AirLoopNum).SensCoolCapSeq = 0.0;
        CalcSysSizing(AirLoopNum).TotCoolCapSeq = 0.0;
        CalcSysSizing(AirLoopNum).HeatCapSeq = 0.0;
        CalcSysSizing(AirLoopNum).PreheatCapSeq = 0.0;
        CalcSysSizing(AirLoopNum).SysCoolRetTempSeq = 0.0;
        CalcSysSizing(AirLoopNum).SysCoolRetHumRatSeq = 0.0;
        CalcSysSizing(AirLoopNum).SysHeatRetTempSeq = 0.0;
        CalcSysSizing(AirLoopNum).SysHeatRetHumRatSeq = 0.0;
        CalcSysSizing(AirLoopNum).SysCoolOutTempSeq = 0.0;
        CalcSysSizing(AirLoopNum).SysCoolOutHumRatSeq = 0.0;
        CalcSysSizing(AirLoopNum).SysHeatOutTempSeq = 0.0;
        CalcSysSizing(AirLoopNum).SysHeatOutHumRatSeq = 0.0;
        CalcSysSizing(AirLoopNum).SysDOASHeatAddSeq = 0.0;
        CalcSysSizing(AirLoopNum).SysDOASLatAddSeq = 0.0;
        CalcSysSizing(AirLoopNum).FloorAreaOnAirLoopCooled = 0.0;
        CalcSysSizing(AirLoopNum).FloorAreaOnAirLoopHeated = 0.0;

        SysSizPeakDDNum(AirLoopNum).TimeStepAtSensCoolPk.allocate(state.dataEnvrn->TotDesDays + state.dataEnvrn->TotRunDesPersDays);
        SysSizPeakDDNum(AirLoopNum).TimeStepAtTotCoolPk.allocate(state.dataEnvrn->TotDesDays + state.dataEnvrn->TotRunDesPersDays);
        SysSizPeakDDNum(AirLoopNum).TimeStepAtCoolFlowPk.allocate(state.dataEnvrn->TotDesDays + state.dataEnvrn->TotRunDesPersDays);
        SysSizPeakDDNum(AirLoopNum).TimeStepAtHeatPk.allocate(state.dataEnvrn->TotDesDays + state.dataEnvrn->TotRunDesPersDays);
        SysSizPeakDDNum(AirLoopNum).TimeStepAtSensCoolPk = 0;
        SysSizPeakDDNum(AirLoopNum).TimeStepAtTotCoolPk = 0;
        SysSizPeakDDNum(AirLoopNum).TimeStepAtCoolFlowPk = 0;
        SysSizPeakDDNum(AirLoopNum).TimeStepAtHeatPk = 0;

        if (state.dataGlobal->AnyEnergyManagementSystemInModel) {

            SetupEMSInternalVariable(state,
                                     "Intermediate Air System Main Supply Volume Flow Rate",
                                     FinalSysSizing(AirLoopNum).AirPriLoopName,
                                     "[m3/s]",
                                     FinalSysSizing(AirLoopNum).DesMainVolFlow);
            SetupEMSActuator(state,
                             "Sizing:System",
                             FinalSysSizing(AirLoopNum).AirPriLoopName,
                             "Main Supply Volume Flow Rate",
                             "[m3/s]",
                             FinalSysSizing(AirLoopNum).EMSOverrideDesMainVolFlowOn,
                             FinalSysSizing(AirLoopNum).EMSValueDesMainVolFlow);

            SetupEMSInternalVariable(state,
                                     "Intermediate Air System Coincident Peak Cooling Mass Flow Rate",
                                     FinalSysSizing(AirLoopNum).AirPriLoopName,
                                     "[kg/s]",
                                     FinalSysSizing(AirLoopNum).CoinCoolMassFlow);
            SetupEMSActuator(state,
                             "Sizing:System",
                             FinalSysSizing(AirLoopNum).AirPriLoopName,
                             "Main Supply Coincident Peak Cooling Mass Flow Rate",
                             "[kg/s]",
                             FinalSysSizing(AirLoopNum).EMSOverrideCoinCoolMassFlowOn,
                             FinalSysSizing(AirLoopNum).EMSValueCoinCoolMassFlow);

            SetupEMSInternalVariable(state,
                                     "Intermediate Air System Coincident Peak Heating Mass Flow Rate",
                                     FinalSysSizing(AirLoopNum).AirPriLoopName,
                                     "[kg/s]",
                                     FinalSysSizing(AirLoopNum).CoinHeatMassFlow);
            SetupEMSActuator(state,
                             "Sizing:System",
                             FinalSysSizing(AirLoopNum).AirPriLoopName,
                             "Main Supply Coincident Peak Heating Mass Flow Rate",
                             "[kg/s]",
                             FinalSysSizing(AirLoopNum).EMSOverrideCoinHeatMassFlowOn,
                             FinalSysSizing(AirLoopNum).EMSValueCoinHeatMassFlow);

            SetupEMSInternalVariable(state,
                                     "Intermediate Air System Noncoincident Peak Cooling Mass Flow Rate",
                                     FinalSysSizing(AirLoopNum).AirPriLoopName,
                                     "[kg/s]",
                                     FinalSysSizing(AirLoopNum).NonCoinCoolMassFlow);
            SetupEMSActuator(state,
                             "Sizing:System",
                             FinalSysSizing(AirLoopNum).AirPriLoopName,
                             "Main Supply Noncoincident Peak Cooling Mass Flow Rate",
                             "[kg/s]",
                             FinalSysSizing(AirLoopNum).EMSOverrideNonCoinCoolMassFlowOn,
                             FinalSysSizing(AirLoopNum).EMSValueNonCoinCoolMassFlow);
            SetupEMSInternalVariable(state,
                                     "Intermediate Air System Noncoincident Peak Heating Mass Flow Rate",
                                     FinalSysSizing(AirLoopNum).AirPriLoopName,
                                     "[kg/s]",
                                     FinalSysSizing(AirLoopNum).NonCoinHeatMassFlow);
            SetupEMSActuator(state,
                             "Sizing:System",
                             FinalSysSizing(AirLoopNum).AirPriLoopName,
                             "Main Supply Noncoincident Peak Heating Mass Flow Rate",
                             "[kg/s]",
                             FinalSysSizing(AirLoopNum).EMSOverrideNonCoinHeatMassFlowOn,
                             FinalSysSizing(AirLoopNum).EMSValueNonCoinHeatMassFlow);

            SetupEMSInternalVariable(state,
                                     "Intermediate Air System Heating Volume Flow Rate",
                                     FinalSysSizing(AirLoopNum).AirPriLoopName,
                                     "[m3/s]",
                                     FinalSysSizing(AirLoopNum).DesHeatVolFlow);
            SetupEMSActuator(state,
                             "Sizing:System",
                             FinalSysSizing(AirLoopNum).AirPriLoopName,
                             "Main Heating Volume Flow Rate",
                             "[m3/s]",
                             FinalSysSizing(AirLoopNum).EMSOverrideDesHeatVolFlowOn,
                             FinalSysSizing(AirLoopNum).EMSValueDesHeatVolFlow);

            SetupEMSInternalVariable(state,
                                     "Intermediate Air System Cooling Volume Flow Rate",
                                     FinalSysSizing(AirLoopNum).AirPriLoopName,
                                     "[m3/s]",
                                     FinalSysSizing(AirLoopNum).DesCoolVolFlow);
            SetupEMSActuator(state,
                             "Sizing:System",
                             FinalSysSizing(AirLoopNum).AirPriLoopName,
                             "Main Cooling Volume Flow Rate",
                             "[m3/s]",
                             FinalSysSizing(AirLoopNum).EMSOverrideDesCoolVolFlowOn,
                             FinalSysSizing(AirLoopNum).EMSValueDesCoolVolFlow);
            // internal variables useful for sizing air system component models
            SetupEMSInternalVariable(state,
                                     "Air System Cooling Design Sensible Capacity",
                                     FinalSysSizing(AirLoopNum).AirPriLoopName,
                                     "[W]",
                                     FinalSysSizing(AirLoopNum).SensCoolCap);
            SetupEMSInternalVariable(state,
                                     "Air System Cooling Design Total Capacity",
                                     FinalSysSizing(AirLoopNum).AirPriLoopName,
                                     "[W]",
                                     FinalSysSizing(AirLoopNum).TotCoolCap);
            SetupEMSInternalVariable(state,
                                     "Air System Heating Design Sensible Capacity",
                                     FinalSysSizing(AirLoopNum).AirPriLoopName,
                                     "[W]",
                                     FinalSysSizing(AirLoopNum).HeatCap);
            SetupEMSInternalVariable(state,
                                     "Air System Preheating Design Sensible Capacity",
                                     FinalSysSizing(AirLoopNum).AirPriLoopName,
                                     "[W]",
                                     FinalSysSizing(AirLoopNum).PreheatCap);

            SetupEMSInternalVariable(state,
                                     "Air System Outdoor Air Design Volume Flow Rate",
                                     FinalSysSizing(AirLoopNum).AirPriLoopName,
                                     "[m3/s]",
                                     FinalSysSizing(AirLoopNum).DesOutAirVolFlow);

            SetupEMSInternalVariable(state,
                                     "Air System Cooling Design Mixed Air Temperature",
                                     FinalSysSizing(AirLoopNum).AirPriLoopName,
                                     "[C]",
                                     FinalSysSizing(AirLoopNum).MixTempAtCoolPeak);
            SetupEMSInternalVariable(state,
                                     "Air System Cooling Design Mixed Air Humidity Ratio",
                                     FinalSysSizing(AirLoopNum).AirPriLoopName,
                                     "[kgWater/kgDryAir]",
                                     FinalSysSizing(AirLoopNum).MixHumRatAtCoolPeak);
            SetupEMSInternalVariable(state,
                                     "Air System Cooling Design Return Air Temperature",
                                     FinalSysSizing(AirLoopNum).AirPriLoopName,
                                     "[C]",
                                     FinalSysSizing(AirLoopNum).RetTempAtCoolPeak);
            SetupEMSInternalVariable(state,
                                     "Air System Cooling Design Return Air Humidity Ratio",
                                     FinalSysSizing(AirLoopNum).AirPriLoopName,
                                     "[kgWater/kgDryAir]",
                                     FinalSysSizing(AirLoopNum).RetHumRatAtCoolPeak);
            SetupEMSInternalVariable(state,
                                     "Air System Cooling Design Outdoor Air Temperature",
                                     FinalSysSizing(AirLoopNum).AirPriLoopName,
                                     "[C]",
                                     FinalSysSizing(AirLoopNum).OutTempAtCoolPeak);
            SetupEMSInternalVariable(state,
                                     "Air System Cooling Design Outdoor Air Humidity Ratio",
                                     FinalSysSizing(AirLoopNum).AirPriLoopName,
                                     "[kgWater/kgDryAir]",
                                     FinalSysSizing(AirLoopNum).OutHumRatAtCoolPeak);

            SetupEMSInternalVariable(state,
                                     "Air System Heating Design Mixed Air Temperature",
                                     FinalSysSizing(AirLoopNum).AirPriLoopName,
                                     "[C]",
                                     FinalSysSizing(AirLoopNum).HeatMixTemp);
            SetupEMSInternalVariable(state,
                                     "Air System Heating Design Mixed Air Humidity Ratio",
                                     FinalSysSizing(AirLoopNum).AirPriLoopName,
                                     "[kgWater/kgDryAir]",
                                     FinalSysSizing(AirLoopNum).HeatMixHumRat);
            SetupEMSInternalVariable(state,
                                     "Air System Heating Design Return Air Temperature",
                                     FinalSysSizing(AirLoopNum).AirPriLoopName,
                                     "[C]",
                                     FinalSysSizing(AirLoopNum).HeatRetTemp);
            SetupEMSInternalVariable(state,
                                     "Air System Heating Design Return Air Humidity Ratio",
                                     FinalSysSizing(AirLoopNum).AirPriLoopName,
                                     "[kgWater/kgDryAir]",
                                     FinalSysSizing(AirLoopNum).HeatRetHumRat);
            SetupEMSInternalVariable(state,
                                     "Air System Heating Design Outdoor Air Temperature",
                                     FinalSysSizing(AirLoopNum).AirPriLoopName,
                                     "[C]",
                                     FinalSysSizing(AirLoopNum).HeatOutTemp);
            SetupEMSInternalVariable(state,
                                     "Air System Heating Design Outdoor Air Humidity Ratio",
                                     FinalSysSizing(AirLoopNum).AirPriLoopName,
                                     "[kgWater/kgDryAir]",
                                     FinalSysSizing(AirLoopNum).HeatOutHumRat);
        }

    } // end the primary air system loop
}

void SizeSysOutdoorAir(EnergyPlusData &state)
{

    using namespace OutputReportPredefined;

    Real64 MinOAFlow;                // design minimum outside air flow for a system
    Real64 ZoneOAFracCooling;        // zone OA fraction for cooling design air flow
    Real64 ZoneOAFracHeating;        // zone OA fraction for heating design air flow
    Real64 ZoneSA;                   // Zone supply air flow rate
    Real64 ZonePA;                   // Zone primary air flow rate
    Real64 ClgSupplyAirAdjustFactor; // temporary variable
    Real64 HtgSupplyAirAdjustFactor; // temporary variable
    Real64 SysOAUnc;                 // uncorrected system OA summing up people and area based OA for all zones for VRP
    Real64 ZoneOAUnc;                // uncorrected zone OA summing up people and area based OA for each zone

    auto &CalcSysSizing(state.dataSize->CalcSysSizing);
    auto &TermUnitSizing(state.dataSize->TermUnitSizing);
    auto &FinalSysSizing(state.dataSize->FinalSysSizing);
    auto &TermUnitFinalZoneSizing(state.dataSize->TermUnitFinalZoneSizing);
    auto &SysSizInput(state.dataSize->SysSizInput);
    auto &AirToZoneNodeInfo(state.dataAirLoop->AirToZoneNodeInfo);

    // begin system OA calcs, this is the first pass, std 62.1 calcs are redone after adjustments and zone units are set up

    // call refactored routine for Pz, Ps and D
    SizingManager::DetermineSystemPopulationDiversity(state);

    // If the system design minimum outside air flow rate is autosized, calculate it from the zone data
    // Note that all TermUnitFinalZoneSizing values have already been scaled by air terminal sizing factors
    for (int AirLoopNum = 1; AirLoopNum <= state.dataHVACGlobal->NumPrimaryAirSys; ++AirLoopNum) {
        MinOAFlow = 0.0;
        SysOAUnc = 0.0;
        ClgSupplyAirAdjustFactor = 1.0;
        HtgSupplyAirAdjustFactor = 1.0;
        int SysSizNum =
            UtilityRoutines::FindItemInList(FinalSysSizing(AirLoopNum).AirPriLoopName, SysSizInput, &SystemSizingInputData::AirPriLoopName);
        if (SysSizNum == 0) SysSizNum = 1; // use first when none applicable
        if (FinalSysSizing(AirLoopNum).OAAutoSized) {
            int NumZonesCooled = AirToZoneNodeInfo(AirLoopNum).NumZonesCooled;

            // people related code removed, see SizingManager::DetermineSystemPopulationDiversity

            for (int ZonesCooledNum = 1; ZonesCooledNum <= NumZonesCooled; ++ZonesCooledNum) {
                int TermUnitSizingIndex = AirToZoneNodeInfo(AirLoopNum).TermUnitCoolSizingIndex(ZonesCooledNum);
                if (TermUnitSizingIndex == 0) {
                    ShowSevereError(state,
                                    "SetUpSysSizingArray: TermUnitSizingIndex = 0 for AirLoop=" + AirToZoneNodeInfo(AirLoopNum).AirLoopName +
                                        ", Zone =" + state.dataHeatBal->Zone(AirToZoneNodeInfo(AirLoopNum).CoolCtrlZoneNums(ZonesCooledNum)).Name);
                    ShowFatalError(state, "This is a defect. Please report this issue.");
                }
                if (SysSizNum > 0) {
                    ZoneOAUnc = TermUnitFinalZoneSizing(TermUnitSizingIndex).TotalOAFromPeople +
                                TermUnitFinalZoneSizing(TermUnitSizingIndex)
                                    .TotalOAFromArea; // should not have diversity at this point (no should have diversity in Vou if VRP)
                    if (SysSizInput(SysSizNum).SystemOAMethod == SOAM_ZoneSum) { // ZoneSum Method
                        SysOAUnc += ZoneOAUnc;
                    } else if (SysSizInput(SysSizNum).SystemOAMethod == SOAM_VRP ||
                               SysSizInput(SysSizNum).SystemOAMethod == SOAM_SP) { // Ventilation Rate Procedure
                        SysOAUnc += TermUnitFinalZoneSizing(TermUnitSizingIndex).TotalOAFromPeople * state.dataSize->DBySys(AirLoopNum) +
                                    TermUnitFinalZoneSizing(TermUnitSizingIndex).TotalOAFromArea; // apply D to people term
                    }
                    state.dataSize->SumRpxPzBySys(AirLoopNum) += TermUnitFinalZoneSizing(TermUnitSizingIndex).TotalOAFromPeople;
                    state.dataSize->SumRaxAzBySys(AirLoopNum) += TermUnitFinalZoneSizing(TermUnitSizingIndex).TotalOAFromArea;

                    // save for Standard 62 tabular report
                    state.dataSize->VbzByZone(TermUnitSizingIndex) = ZoneOAUnc; // fixed now, previously RHS already had Ez factored in.
                    // Save Std 62.1 cooling ventilation required by zone
                    if (TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneADEffCooling > 0.0) {
                        TermUnitFinalZoneSizing(TermUnitSizingIndex).VozClgByZone =
                            ZoneOAUnc / TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneADEffCooling;
                    } else {
                        TermUnitFinalZoneSizing(TermUnitSizingIndex).VozClgByZone = ZoneOAUnc;
                    }

                    if (SysSizInput(SysSizNum).SystemOAMethod == SOAM_ZoneSum) { // ZoneSum Method
                        MinOAFlow += TermUnitFinalZoneSizing(TermUnitSizingIndex).MinOA;
                        if (TermUnitFinalZoneSizing(TermUnitSizingIndex).DesCoolVolFlow > 0.0) {
                            ZoneOAFracCooling = TermUnitFinalZoneSizing(TermUnitSizingIndex).VozClgByZone /
                                                TermUnitFinalZoneSizing(TermUnitSizingIndex)
                                                    .DesCoolVolFlow; // calculate anyway for use with zone OA max fraction below
                        } else {
                            ZoneOAFracCooling = 0.0;
                        }
                    } else if (SysSizInput(SysSizNum).SystemOAMethod == SOAM_VRP ||
                               SysSizInput(SysSizNum).SystemOAMethod == SOAM_SP) { // Ventilation Rate Procedure
                        // CR 8872 - check to see if uncorrected OA is calculated to be greater than 0
                        if (!(ZoneOAUnc > 0.0)) {
                            ShowSevereError(
                                state, "Sizing:System - The system outdoor air method is set to VRP in " + FinalSysSizing(AirLoopNum).AirPriLoopName);
                            ShowContinueError(state,
                                              "But zone \"" + TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneName +
                                                  "\" associated with system does not have OA flow/person");
                            ShowContinueError(state,
                                              "or flow/area values specified in DesignSpecification:OutdoorAir object associated with the zone");
                        }

                        // Save Std 62.1 cooling ventilation required by zone
                        MinOAFlow += TermUnitFinalZoneSizing(TermUnitSizingIndex).VozClgByZone; // Don't include D

                        if (TermUnitFinalZoneSizing(TermUnitSizingIndex).DesCoolVolFlow > 0.0) {
                            if (TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneSecondaryRecirculation > 0.0 ||
                                TermUnitFinalZoneSizing(TermUnitSizingIndex).DesCoolVolFlowMin <= 0) {
                                // multi-path system or VAV Minimum not defined
                                ZoneOAFracCooling = TermUnitFinalZoneSizing(TermUnitSizingIndex).VozClgByZone /
                                                    TermUnitFinalZoneSizing(TermUnitSizingIndex)
                                                        .DesCoolVolFlow; // this should be based on final atu flows, not sizing design

                            } else {
                                // Single path; Use VAV Minimum as the Vpz in the Zp = Voz / Vpz equations
                                ZoneOAFracCooling = TermUnitFinalZoneSizing(TermUnitSizingIndex).VozClgByZone /
                                                    TermUnitFinalZoneSizing(TermUnitSizingIndex)
                                                        .DesCoolVolFlowMin; // this should be based on final atu flows, not sizing design
                            }
                        } else {
                            ZoneOAFracCooling = 0.0;
                        }
                    } else { // error
                    }
                } else { // ZoneSum Method
                    MinOAFlow += TermUnitFinalZoneSizing(TermUnitSizingIndex).MinOA;
                    ZoneOAFracCooling = 0.0;
                }

                // Calc maximum zone OA fraction and supply air adjustment factor based on
                // user entered max allowed OA fraction
                if (FinalSysSizing(AirLoopNum).MaxZoneOAFraction > 0 && ZoneOAFracCooling > FinalSysSizing(AirLoopNum).MaxZoneOAFraction) {
                    if (FinalSysSizing(AirLoopNum).CoolAirDesMethod == FromDDCalc) { // DesignDay Method
                        ClgSupplyAirAdjustFactor = ZoneOAFracCooling / FinalSysSizing(AirLoopNum).MaxZoneOAFraction;
                        if (TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneSecondaryRecirculation > 0.0 ||
                            TermUnitFinalZoneSizing(TermUnitSizingIndex).DesCoolVolFlowMin <= 0) {
                            // multi-path system or VAV Minimum not defined
                            TermUnitFinalZoneSizing(TermUnitSizingIndex).DesCoolVolFlow *= ClgSupplyAirAdjustFactor;
                        } else {
                            // Single path; Use VAV Minimum as the Vpz in the Zp = Voz / Vpz equations
                            TermUnitFinalZoneSizing(TermUnitSizingIndex).DesCoolVolFlowMin *=
                                ClgSupplyAirAdjustFactor; // from code inspection value set here is used above, before being set.

                            // Don't allow the design cooling airflow to be less than the VAV minimum airflow
                            TermUnitFinalZoneSizing(TermUnitSizingIndex).DesCoolVolFlow =
                                max(TermUnitFinalZoneSizing(TermUnitSizingIndex).DesCoolVolFlow,
                                    TermUnitFinalZoneSizing(TermUnitSizingIndex).DesCoolVolFlowMin);
                        }
                        // Don't allow the design terminal airflow to be less than the design cooling airflow
                        TermUnitSizing(TermUnitSizingIndex).AirVolFlow =
                            max(TermUnitSizing(TermUnitSizingIndex).AirVolFlow, TermUnitFinalZoneSizing(TermUnitSizingIndex).DesCoolVolFlow);
                        ZoneOAFracCooling = FinalSysSizing(AirLoopNum).MaxZoneOAFraction;
                    } else {
                        ClgSupplyAirAdjustFactor = 1.0;
                    }
                } else {
                    ClgSupplyAirAdjustFactor = 1.0;
                }

                ZoneSA = 0.0;
                ZonePA = 0.0;
                state.dataSimAirServingZones->EpSSOA = 1.0;
                if (TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneSecondaryRecirculation > 0.0) { // multi-path system
                    // Vpz: "Primary" supply air from main air handler served by an oa mixer
                    ZonePA = TermUnitFinalZoneSizing(TermUnitSizingIndex).DesCoolVolFlow;
                    // Vdz: "Discharge" supply air delivered to zone by terminal unit
                    ZoneSA = max(TermUnitSizing(TermUnitSizingIndex).AirVolFlow, ZonePA);

                    // For re-circulation systems, Vpz used to determine Zpz is the design terminal airflow
                    // Std 62.1-2010, section 6.2.5.1: "Vpz (used to determin Zpz) is the primary airflow rate
                    // rate to the ventilation zone from the air handler, including outdoor air and recirculated air.
                    // MJW - Not sure this is correct, seems like it should be ZonePA - above comments contradict each other
                    state.dataSize->VpzMinClgByZone(TermUnitSizingIndex) = ZoneSA;

                } else { // single path system
                    // Vdz: "Discharge" supply air delivered to zone by terminal unit
                    ZonePA = TermUnitFinalZoneSizing(TermUnitSizingIndex).DesCoolVolFlow;
                    // Vpz: "Primary" supply air from main air handler served by an oa mixer
                    ZoneSA = TermUnitFinalZoneSizing(TermUnitSizingIndex).DesCoolVolFlow;

                    // Save VpzMin in case this is a single path VAV system.
                    // Std 62.1-2010, section 6.2.5.1: "For VAV-system design purposes, Vpz is the lowest zone primary
                    // airflow value expected at the design condition analyzed."
                    state.dataSize->VpzMinClgByZone(TermUnitSizingIndex) =
                        TermUnitFinalZoneSizing(TermUnitSizingIndex).DesCoolVolFlowMin; // this may be getting used before it gets filled ??

                    // In case for some reason the VAV minimum has not been defined, use the design primary airflow
                    if (TermUnitFinalZoneSizing(TermUnitSizingIndex).DesCoolVolFlowMin <= 0)
                        state.dataSize->VpzMinClgByZone(TermUnitSizingIndex) = ZonePA;
                }

                // save zone discharge supply airflow
                state.dataSize->VdzClgByZone(TermUnitSizingIndex) = ZoneSA;

                // save Vpz zone primary airflow for standard 62.1 report
                state.dataSize->VpzClgByZone(TermUnitSizingIndex) = ZonePA;
                state.dataSize->VpzClgSumBySys(AirLoopNum) += ZonePA;

                // Fraction of required zone ventilation to minimum primary airflow expected at condition analyzed
                TermUnitFinalZoneSizing(TermUnitSizingIndex).ZpzClgByZone = 0.0;
                if (state.dataSize->VpzMinClgByZone(TermUnitSizingIndex) > 0) {
                    TermUnitFinalZoneSizing(TermUnitSizingIndex).ZpzClgByZone =
                        min(1.0, TermUnitFinalZoneSizing(TermUnitSizingIndex).VozClgByZone / state.dataSize->VpzMinClgByZone(TermUnitSizingIndex));
                }

                // calc zone primary air fraction
                if (ZoneSA > 0.0) state.dataSimAirServingZones->EpSSOA = ZonePA / ZoneSA;
                if (state.dataSimAirServingZones->EpSSOA > 1.0) state.dataSimAirServingZones->EpSSOA = 1.0;
                TermUnitFinalZoneSizing(TermUnitSizingIndex).ZonePrimaryAirFraction = state.dataSimAirServingZones->EpSSOA;
                TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneOAFracCooling = ZoneOAFracCooling;

                // determined cooled zone floor area in an airloop
                FinalSysSizing(AirLoopNum).FloorAreaOnAirLoopCooled += TermUnitFinalZoneSizing(TermUnitSizingIndex).TotalZoneFloorArea;

                TermUnitFinalZoneSizing(TermUnitSizingIndex).SupplyAirAdjustFactor = max(ClgSupplyAirAdjustFactor, HtgSupplyAirAdjustFactor);
            }

            int NumZonesHeated = AirToZoneNodeInfo(AirLoopNum).NumZonesHeated;
            if (NumZonesHeated > 0) {
                for (int ZonesHeatedNum = 1; ZonesHeatedNum <= NumZonesHeated; ++ZonesHeatedNum) {
                    int TermUnitSizingIndex = AirToZoneNodeInfo(AirLoopNum).TermUnitHeatSizingIndex(ZonesHeatedNum);
                    if (TermUnitSizingIndex == 0) {
                        ShowSevereError(
                            state,
                            "SetUpSysSizingArray: TermUnitSizingIndex = 0 for AirLoop=" + AirToZoneNodeInfo(AirLoopNum).AirLoopName +
                                ", Zone =" + state.dataHeatBal->Zone(AirToZoneNodeInfo(AirLoopNum).HeatCtrlZoneNums(ZonesHeatedNum)).Name);
                        ShowFatalError(state, "This is a defect. Please report this issue.");
                    }
                    int MatchingCooledZoneNum =
                        General::FindNumberInList(TermUnitSizingIndex, AirToZoneNodeInfo(AirLoopNum).TermUnitCoolSizingIndex, NumZonesCooled);
                    if (MatchingCooledZoneNum == 0) {
                        if (SysSizNum > 0) {
                            ZoneOAUnc = TermUnitFinalZoneSizing(TermUnitSizingIndex).TotalOAFromPeople +
                                        TermUnitFinalZoneSizing(TermUnitSizingIndex).TotalOAFromArea; // should not have diversity at this point
                            if (SysSizInput(SysSizNum).SystemOAMethod == SOAM_ZoneSum) {              // ZoneSum Method
                                SysOAUnc += ZoneOAUnc;
                            } else if (SysSizInput(SysSizNum).SystemOAMethod == SOAM_VRP ||
                                       SysSizInput(SysSizNum).SystemOAMethod == SOAM_SP) { // Ventilation Rate and Simplified Procedure
                                SysOAUnc += TermUnitFinalZoneSizing(TermUnitSizingIndex).TotalOAFromPeople * state.dataSize->DBySys(AirLoopNum) +
                                            TermUnitFinalZoneSizing(TermUnitSizingIndex).TotalOAFromArea; // apply D to people term
                            }
                            state.dataSize->SumRpxPzBySys(AirLoopNum) += TermUnitFinalZoneSizing(TermUnitSizingIndex).TotalOAFromPeople;
                            state.dataSize->SumRaxAzBySys(AirLoopNum) += TermUnitFinalZoneSizing(TermUnitSizingIndex).TotalOAFromArea;
                            // save for Standard 62 tabular report
                            state.dataSize->VbzByZone(TermUnitSizingIndex) = ZoneOAUnc; // fixed now, previously RHS already had Ez factored in.
                            // Save Std 62.1 heating ventilation required by zone
                            if (TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneADEffHeating > 0.0) {
                                TermUnitFinalZoneSizing(TermUnitSizingIndex).VozHtgByZone =
                                    ZoneOAUnc / TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneADEffHeating;
                            } else {
                                TermUnitFinalZoneSizing(TermUnitSizingIndex).VozHtgByZone = ZoneOAUnc;
                            }

                            if (SysSizInput(SysSizNum).SystemOAMethod == SOAM_ZoneSum) { // ZoneSum Method
                                MinOAFlow += TermUnitFinalZoneSizing(TermUnitSizingIndex).MinOA;
                                if (TermUnitFinalZoneSizing(TermUnitSizingIndex).DesHeatVolFlow > 0.0) {
                                    ZoneOAFracHeating = TermUnitFinalZoneSizing(TermUnitSizingIndex).VozHtgByZone /
                                                        TermUnitFinalZoneSizing(TermUnitSizingIndex)
                                                            .DesHeatVolFlow; // calculate anyway for use with zone OA max fraction below
                                } else {
                                    ZoneOAFracHeating = 0.0;
                                }

                            } else if (SysSizInput(SysSizNum).SystemOAMethod == SOAM_VRP ||
                                       SysSizInput(SysSizNum).SystemOAMethod == SOAM_SP) { // Ventilation Rate and Simplified Procedure
                                // CR 8872 - check to see if uncorrected OA is calculated to be greater than 0
                                if (!(ZoneOAUnc > 0.0)) {
                                    ShowSevereError(state,
                                                    "Sizing:System - The system outdoor air method is set to VRP in " +
                                                        FinalSysSizing(AirLoopNum).AirPriLoopName);
                                    ShowContinueError(state,
                                                      "But zone \"" + TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneName +
                                                          "\" associated with system does not have OA flow/person");
                                    ShowContinueError(
                                        state, "or flow/area values specified in DesignSpecification:OutdoorAir object associated with the zone");
                                }

                                // Save Std 62.1 heating ventilation required by zone
                                MinOAFlow += TermUnitFinalZoneSizing(TermUnitSizingIndex).VozHtgByZone; // Don't include D

                                if (TermUnitFinalZoneSizing(TermUnitSizingIndex).DesHeatVolFlow > 0.0) {
                                    if (TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneSecondaryRecirculation > 0.0) { // multi-path system
                                        // multi-path system
                                        ZoneOAFracHeating = TermUnitFinalZoneSizing(TermUnitSizingIndex).VozHtgByZone /
                                                            TermUnitSizing(TermUnitSizingIndex).AirVolFlow;
                                    } else {
                                        // Single path system
                                        ZoneOAFracHeating = TermUnitFinalZoneSizing(TermUnitSizingIndex).VozHtgByZone /
                                                            TermUnitFinalZoneSizing(TermUnitSizingIndex).DesHeatVolFlow;
                                    }
                                } else {
                                    ZoneOAFracHeating = 0.0;
                                }
                            } else { // would be error
                            }
                        } else { // ZoneSum Method
                            MinOAFlow += TermUnitFinalZoneSizing(TermUnitSizingIndex).MinOA;
                            ZoneOAFracHeating = 0.0;
                        }
                    } else { // matching cooled zone > 0
                        //?? so what happens if zone is both heated and cooled this makes little sense?  Don't want to double count in
                        // MinOAFlow
                        // but still need to do std62.1 heating calcs ??
                        ZoneOAFracHeating = 0.0;
                    }

                    // Calc maximum zone OA fraction and supply air adjustment factor based
                    // on user entered max allowed OA fraction
                    if (FinalSysSizing(AirLoopNum).MaxZoneOAFraction > 0 && ZoneOAFracHeating > FinalSysSizing(AirLoopNum).MaxZoneOAFraction) {
                        if (FinalSysSizing(AirLoopNum).CoolAirDesMethod == FromDDCalc) { // DesignDay Method
                            HtgSupplyAirAdjustFactor = ZoneOAFracHeating / FinalSysSizing(AirLoopNum).MaxZoneOAFraction;
                            if (TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneSecondaryRecirculation > 0.0 ||
                                TermUnitFinalZoneSizing(TermUnitSizingIndex).DesCoolVolFlowMin <= 0) {
                                // multi-path system or VAV Heating airflow max not defined
                                TermUnitSizing(TermUnitSizingIndex).AirVolFlow *= HtgSupplyAirAdjustFactor;
                            } else {
                                // Single path; Use VAV Heating airflow max as the Vpz in the Zp = Voz / Vpz equations
                                TermUnitFinalZoneSizing(TermUnitSizingIndex).DesHeatVolFlow *= HtgSupplyAirAdjustFactor;
                                // Don't allow the design terminal airflow to be less than the design heating airflow
                                TermUnitSizing(TermUnitSizingIndex).AirVolFlow =
                                    max(TermUnitSizing(TermUnitSizingIndex).AirVolFlow, TermUnitFinalZoneSizing(TermUnitSizingIndex).DesHeatVolFlow);
                            }
                            ZoneOAFracHeating = FinalSysSizing(AirLoopNum).MaxZoneOAFraction;
                        } else {
                            HtgSupplyAirAdjustFactor = 1.0;
                        }
                    } else {
                        HtgSupplyAirAdjustFactor = 1.0;
                    }

                    ZoneSA = 0.0;
                    ZonePA = 0.0;
                    state.dataSimAirServingZones->EpSSOA = 1.0;
                    if (TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneSecondaryRecirculation > 0.0) { // multi-path system
                        // Vpz: "Primary" supply air from main air handler served by an oa mixer
                        ZonePA = TermUnitFinalZoneSizing(TermUnitSizingIndex).DesHeatVolFlow;
                        // Vdz: "Discharge" supply air delivered to zone by terminal unit
                        ZoneSA = max(TermUnitSizing(TermUnitSizingIndex).AirVolFlow, ZonePA);

                        // For re-circulation systems, Vpz used to determine Zpz is the design terminal airflow
                        // Std 62.1-2010, section 6.2.5.1: "Vpz (used to determin Zpz) is the primary airflow rate
                        // rate to the ventilation zone from the air handler, including outdoor air and recirculated air.
                        state.dataSize->VpzMinHtgByZone(TermUnitSizingIndex) = ZoneSA;

                    } else { // single path system

                        ZonePA = TermUnitFinalZoneSizing(TermUnitSizingIndex).DesHeatVolFlow;
                        ZoneSA = TermUnitFinalZoneSizing(TermUnitSizingIndex).DesHeatVolFlow;

                        // We do not use the cooling VAV min for heating because the VAV-box heating maximum may be larger.
                        state.dataSize->VpzMinHtgByZone(TermUnitSizingIndex) = ZoneSA;
                    }

                    // save Vdz zone discharge supply airflow for standard 62.1 report
                    state.dataSize->VdzHtgByZone(TermUnitSizingIndex) = ZoneSA;

                    // save Vpz zone primary airflow for standard 62.1 report
                    state.dataSize->VpzHtgByZone(TermUnitSizingIndex) = ZonePA;
                    state.dataSize->VpzHtgSumBySys(AirLoopNum) += ZonePA;

                    // Fraction of required zone ventilation to minimum primary airflow expected at condition analyzed
                    TermUnitFinalZoneSizing(TermUnitSizingIndex).ZpzHtgByZone = 0.0;
                    if (state.dataSize->VpzMinHtgByZone(TermUnitSizingIndex) > 0) {
                        TermUnitFinalZoneSizing(TermUnitSizingIndex).ZpzHtgByZone = min(
                            1.0, TermUnitFinalZoneSizing(TermUnitSizingIndex).VozHtgByZone / state.dataSize->VpzMinHtgByZone(TermUnitSizingIndex));
                    }

                    // calc zone primary air fraction
                    if (ZoneSA > 0.0) state.dataSimAirServingZones->EpSSOA = ZonePA / ZoneSA;
                    if (state.dataSimAirServingZones->EpSSOA > 1.0) state.dataSimAirServingZones->EpSSOA = 1.0;
                    TermUnitFinalZoneSizing(TermUnitSizingIndex).ZonePrimaryAirFractionHtg = state.dataSimAirServingZones->EpSSOA;
                    TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneOAFracHeating = ZoneOAFracHeating;

                    // determined heated zone floor area in an airloop
                    FinalSysSizing(AirLoopNum).FloorAreaOnAirLoopHeated += TermUnitFinalZoneSizing(TermUnitSizingIndex).TotalZoneFloorArea;

                    TermUnitFinalZoneSizing(TermUnitSizingIndex).SupplyAirAdjustFactor = max(ClgSupplyAirAdjustFactor, HtgSupplyAirAdjustFactor);

                } // end for loop of heated zones

            } else { // getting heating flow based values for Std 62.1 report for single path systems
                ZoneOAFracHeating = 0.0;
                for (int ZonesHeatedNum = 1; ZonesHeatedNum <= NumZonesCooled; ++ZonesHeatedNum) {
                    int TermUnitSizingIndex = AirToZoneNodeInfo(AirLoopNum).TermUnitCoolSizingIndex(ZonesHeatedNum);
                    if (TermUnitSizingIndex == 0) {
                        ShowSevereError(
                            state,
                            "SetUpSysSizingArray: TermUnitSizingIndex = 0 for AirLoop=" + AirToZoneNodeInfo(AirLoopNum).AirLoopName +
                                ", Zone =" + state.dataHeatBal->Zone(AirToZoneNodeInfo(AirLoopNum).CoolCtrlZoneNums(ZonesHeatedNum)).Name);
                        ShowFatalError(state, "This is a defect. Please report this issue.");
                    }

                    // Save Std 62.1 heating ventilation required by zone
                    if (TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneADEffHeating > 0.0) {
                        TermUnitFinalZoneSizing(TermUnitSizingIndex).VozHtgByZone =
                            state.dataSize->VbzByZone(TermUnitSizingIndex) / TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneADEffHeating;
                    } else {
                        TermUnitFinalZoneSizing(TermUnitSizingIndex).VozHtgByZone = state.dataSize->VbzByZone(TermUnitSizingIndex);
                    }

                    if (TermUnitFinalZoneSizing(TermUnitSizingIndex).DesHeatVolFlow > 0.0) {
                        if (TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneSecondaryRecirculation > 0.0) { // multi-path system
                            // multi-path system
                            if (TermUnitSizing(TermUnitSizingIndex).AirVolFlow != 0) {
                                ZoneOAFracHeating =
                                    TermUnitFinalZoneSizing(TermUnitSizingIndex).VozHtgByZone / TermUnitSizing(TermUnitSizingIndex).AirVolFlow;
                            }
                        } else {
                            // Single path system
                            ZoneOAFracHeating = TermUnitFinalZoneSizing(TermUnitSizingIndex).VozHtgByZone /
                                                TermUnitFinalZoneSizing(TermUnitSizingIndex).DesHeatVolFlow;
                        }
                    } else {
                        ZoneOAFracHeating = 0.0;
                    }

                    // Calc maximum zone OA fraction and supply air adjustment factor based
                    // on user entered max allowed OA fraction - a TRACE feature
                    if (FinalSysSizing(AirLoopNum).MaxZoneOAFraction > 0 && ZoneOAFracHeating > FinalSysSizing(AirLoopNum).MaxZoneOAFraction) {
                        if (FinalSysSizing(AirLoopNum).HeatAirDesMethod == FromDDCalc) { // DesignDay Method
                            HtgSupplyAirAdjustFactor = ZoneOAFracHeating / FinalSysSizing(AirLoopNum).MaxZoneOAFraction;
                            if (TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneSecondaryRecirculation > 0.0 ||
                                TermUnitFinalZoneSizing(TermUnitSizingIndex).DesCoolVolFlowMin <= 0) {
                                // multi-path system or VAV Heating airflow max not defined
                                TermUnitSizing(TermUnitSizingIndex).AirVolFlow *= HtgSupplyAirAdjustFactor;
                            } else {
                                // Single path; Use VAV Heating airflow max as the Vpz in the Zp = Voz / Vpz equations
                                TermUnitFinalZoneSizing(TermUnitSizingIndex).DesHeatVolFlow *= HtgSupplyAirAdjustFactor;
                                // Don't allow the design terminal airflow to be less than the design heating airflow
                                TermUnitSizing(TermUnitSizingIndex).AirVolFlow =
                                    max(TermUnitSizing(TermUnitSizingIndex).AirVolFlow, TermUnitFinalZoneSizing(TermUnitSizingIndex).DesHeatVolFlow);
                            }
                            ZoneOAFracHeating = FinalSysSizing(AirLoopNum).MaxZoneOAFraction;
                        }
                    }
                    ZonePA = TermUnitFinalZoneSizing(TermUnitSizingIndex).DesHeatVolFlow;
                    ZoneSA = TermUnitFinalZoneSizing(TermUnitSizingIndex).DesHeatVolFlow;
                    // save Vdz zone discharge airflow for standard 62.1 report
                    state.dataSize->VdzHtgByZone(TermUnitSizingIndex) = ZoneSA;
                    // save Vpz zone primary airflow for standard 62.1 report
                    state.dataSize->VpzHtgByZone(TermUnitSizingIndex) = ZonePA;
                    state.dataSize->VpzHtgSumBySys(AirLoopNum) += ZonePA;

                    // We do not use the cooling VAV min for heating because the VAV-box heating maximum may be larger.
                    state.dataSize->VpzMinHtgByZone(TermUnitSizingIndex) = ZoneSA;

                    // Fraction of required zone ventilation to minimum primary airflow expected at condition analyzed
                    TermUnitFinalZoneSizing(TermUnitSizingIndex).ZpzHtgByZone = 0.0;
                    if (state.dataSize->VpzMinHtgByZone(TermUnitSizingIndex) > 0) {
                        TermUnitFinalZoneSizing(TermUnitSizingIndex).ZpzHtgByZone =
                            TermUnitFinalZoneSizing(TermUnitSizingIndex).VozHtgByZone / state.dataSize->VpzMinHtgByZone(TermUnitSizingIndex);
                    }

                    // calc zone primary air fraction
                    state.dataSimAirServingZones->EpSSOA = 1.0;
                    if (ZoneSA > 0.0) state.dataSimAirServingZones->EpSSOA = ZonePA / ZoneSA;
                    if (state.dataSimAirServingZones->EpSSOA > 1.0) state.dataSimAirServingZones->EpSSOA = 1.0;
                    TermUnitFinalZoneSizing(TermUnitSizingIndex).ZonePrimaryAirFractionHtg = state.dataSimAirServingZones->EpSSOA;
                    TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneOAFracHeating = ZoneOAFracHeating;
                    TermUnitFinalZoneSizing(TermUnitSizingIndex).SupplyAirAdjustFactor = max(ClgSupplyAirAdjustFactor, HtgSupplyAirAdjustFactor);

                } // end for loop over cooled zones (for htg calcs though)
                // CalcZoneSizing( CurOverallSimDay, CtrlZoneNum ).SupplyAirAdjustFactor = TermUnitFinalZoneSizing( TermUnitSizingIndex
                // ).SupplyAirAdjustFactor;
                FinalSysSizing(AirLoopNum).FloorAreaOnAirLoopHeated = FinalSysSizing(AirLoopNum).FloorAreaOnAirLoopCooled;
            }

            FinalSysSizing(AirLoopNum).SysUncOA = SysOAUnc;
            CalcSysSizing(AirLoopNum).SysUncOA = SysOAUnc;
            state.dataSize->VouBySys(AirLoopNum) = SysOAUnc;

            FinalSysSizing(AirLoopNum).DesOutAirVolFlow = MinOAFlow;
            CalcSysSizing(AirLoopNum).DesOutAirVolFlow = MinOAFlow;

            for (int DesDayEnvrnNum = 1; DesDayEnvrnNum <= state.dataEnvrn->TotDesDays + state.dataEnvrn->TotRunDesPersDays; ++DesDayEnvrnNum) {
                state.dataSize->SysSizing(DesDayEnvrnNum, AirLoopNum).DesOutAirVolFlow = FinalSysSizing(AirLoopNum).DesOutAirVolFlow;
            }
        }
    }

    // END SYSTEM OA CALCS

    // have moved std 62.1 table report writing to ManageSystemVentilationAdjustments in SizingManager
}

void UpdateSysSizing(EnergyPlusData &state, DataGlobalConstants::CallIndicator const CallIndicator)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   February 2001
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Update the result variables of the zone sizing calculation

    // METHODOLOGY EMPLOYED:
    // CallIndicator = 1 (BeginDay) zero the result arrays
    // CallIndicator = 2 (DuringDay) fill arrays, averaging over 1 zone time step
    // CallIndicator = 3 (EndDay) calculate daily maxima
    // CallIndicator = 5 (EndSysSizingCalc) write out results

    // Using/Aliasing
    using EMSManager::ManageEMS;
    using General::FindNumberInList;
    using Psychrometrics::PsyCpAirFnW;
    using Psychrometrics::PsyHFnTdbW;
    using Psychrometrics::PsyRhoAirFnPbTdbW;
    using namespace OutputReportPredefined;
    using namespace DataSizing;

    // Locals
    int numOfTimeStepInDay; // number of zone time steps in a day

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int AirLoopNum;                // primary air system index
    int TimeStepInDay;             // zone time step in day
    int TimeStepIndex;             // zone time step index
    int I;                         // write statement index
    int J;                         // write statement index
    Real64 SysCoolRetTemp;         // system cooling return temperature for a time step [C]
    Real64 SysHeatRetTemp;         // system heating return temperature for a time step [C]
    Real64 RhoAir;                 // density of air kg/m3
    Real64 OutAirFrac;             // outside air fraction
    Real64 SysCoolMixTemp;         // system cooling mixed air temperature [C]
    Real64 SysHeatMixTemp;         // system heating mixed air temperature [C]
    Real64 SysSensCoolCap;         // system sensible cooling capacity [W]
    Real64 SysTotCoolCap;          // system total cooling capacity [W]
    Real64 SysCoolZoneAvgTemp;     // system cooling zone average temperature [C]
    Real64 SysHeatZoneAvgTemp;     // system heating zone average temperature [C]
    Real64 SysHeatCap;             // system heating capacity [W]
    int HourCounter;               // Hour Counter
    int TimeStepCounter;           // Time Step Counter
    int Minutes;                   // Current Minutes Counter
    int HourPrint;                 // Hour to print (timestamp)
    int DDNum;                     // design day index
    int CoolDDNum;                 // design day index of a peak cooling day
    int HeatDDNum;                 // design day index of a peak cooling day
    int CoolTimeStepNum;           // time step index (in day) of a cooling peak
    int HeatTimeStepNum;           // time step index (in day) of a cooling peak
    Real64 OutAirTemp;             // outside air temperature
    Real64 OutAirHumRat;           // outside air humifity ratio
    Real64 SysCoolMixHumRat;       // system cooling mixed air humidity ratio [kg water/kg dry air]
    Real64 SysCoolRetHumRat;       // system coolingreturn air humifity ratio [kg water/kg dry air]
    Real64 SysHeatMixHumRat;       // system heating mixed air humidity ratio [kg water/kg dry air]
    Real64 SysHeatRetHumRat;       // system heatingreturn air humifity ratio [kg water/kg dry air]
    Real64 SysCoolOutTemp;         // system cooling outside air temperature [C]
    Real64 SysCoolOutHumRat;       // system cooling outside air humidity ratio [kg water/kg dry air]
    Real64 SysHeatOutTemp;         // system heating outside air temperature [C]
    Real64 SysHeatOutHumRat;       // system heating outside air humidity ratio [kg water/kg dry air]
    Real64 SysDOASHeatAdd;         // system DOAS heat addition rate [W]
    Real64 SysDOASLatAdd;          // system DOAS latent heat addition rate [W]
    Real64 SysCoolSizingRat;       // ratio of user input design flow for cooling divided by calculated design cooling flow
    Real64 SysHeatSizingRat;       // ratio of user input design flow for heating divided by calculated design heating flow
    Real64 ZoneOARatio;            // ratio of zone OA flow to zone design cooling or heating flow
    Real64 RetTempRise;            // difference between zone return temperature and zone temperature [delta K]
    Real64 SysCoolingEv;           // System level ventilation effectiveness for cooling mode
    Real64 SysHeatingEv;           // System level ventilation effectiveness for heating mode
    Real64 SysHtgPeakAirflow;      // Peak heating airflow
    int MatchingCooledZoneNum;     // temporary variable
    Real64 termunitsizingtempfrac; // 1.0/(1.0+termunitsizing(ctrlzone)%inducrat)
    Real64 termunitsizingtemp;     // (1.0+termunitsizing(ctrlzone)%inducrat)
    Real64 VozClg(0.0);            // corrected (for ventilation efficiency) zone outside air flow rate [m3/s]

    auto &FinalSysSizing(state.dataSize->FinalSysSizing);
    auto &TermUnitSizing(state.dataSize->TermUnitSizing);
    auto &CalcSysSizing(state.dataSize->CalcSysSizing);
    auto &TermUnitFinalZoneSizing(state.dataSize->TermUnitFinalZoneSizing);
    auto &NumAirTerminalUnits(state.dataSize->NumAirTerminalUnits);
    auto &ZoneSizing(state.dataSize->ZoneSizing);
    auto &CurOverallSimDay(state.dataSize->CurOverallSimDay);
    auto &SysSizing(state.dataSize->SysSizing);
    auto &SizingFileColSep(state.dataSize->SizingFileColSep);
    auto &CalcZoneSizing(state.dataSize->CalcZoneSizing);
    auto &AirToZoneNodeInfo(state.dataAirLoop->AirToZoneNodeInfo);
    auto &SysSizPeakDDNum(state.dataSize->SysSizPeakDDNum);
    auto &NumPrimaryAirSys(state.dataHVACGlobal->NumPrimaryAirSys);

    numOfTimeStepInDay = state.dataGlobal->NumOfTimeStepInHour * 24;

    // allocate scratch arrays
    if (!allocated(state.dataSize->SensCoolCapTemp)) {
        state.dataSize->SensCoolCapTemp.dimension(NumPrimaryAirSys, 0.0);
        state.dataSize->TotCoolCapTemp.dimension(NumPrimaryAirSys, 0.0);
    }

    // allocate arrays used to store values for standard 62.1 tabular report
    if (!allocated(state.dataSize->FaByZoneCool)) {
        state.dataSize->FaByZoneCool.dimension(NumAirTerminalUnits, 0.0);
        state.dataSize->FaByZoneHeat.dimension(NumAirTerminalUnits, 0.0);
        state.dataSize->FbByZoneCool.dimension(NumAirTerminalUnits, 0.0);
        state.dataSize->FbByZoneHeat.dimension(NumAirTerminalUnits, 0.0);
        state.dataSize->FcByZoneCool.dimension(NumAirTerminalUnits, 0.0);
        state.dataSize->FcByZoneHeat.dimension(NumAirTerminalUnits, 0.0);
        state.dataSimAirServingZones->EvBySysCool.dimension(NumPrimaryAirSys, 1.0);
        state.dataSimAirServingZones->EvBySysHeat.dimension(NumPrimaryAirSys, 1.0);
        state.dataSize->XsBySysCool.dimension(NumPrimaryAirSys, 1.0);
        state.dataSize->XsBySysHeat.dimension(NumPrimaryAirSys, 1.0);
        state.dataSize->EvzByZoneCool.dimension(NumAirTerminalUnits, 1.0);
        state.dataSize->EvzByZoneCoolPrev.dimension(NumAirTerminalUnits, 1.0);
        state.dataSize->EvzByZoneHeat.dimension(NumAirTerminalUnits, 1.0);
        state.dataSize->EvzByZoneHeatPrev.dimension(NumAirTerminalUnits, 1.0);
        state.dataSize->EvzMinBySysCool.dimension(NumPrimaryAirSys, 1.0);
        state.dataSize->EvzMinBySysHeat.dimension(NumPrimaryAirSys, 1.0);
        state.dataSize->VotClgBySys.dimension(NumPrimaryAirSys, 0.0);
        state.dataSize->VotHtgBySys.dimension(NumPrimaryAirSys, 0.0);
        state.dataSize->VozSumClgBySys.dimension(NumPrimaryAirSys, 0.0);
        state.dataSize->VozSumHtgBySys.dimension(NumPrimaryAirSys, 0.0);
    }

    switch (CallIndicator) {
    case DataGlobalConstants::CallIndicator::BeginDay: {
        // Correct the zone return temperature in ZoneSizing for the case of induction units. The calc in
        // ZoneEquipmentManager assumes all the air entering the zone goes into the return node.
        for (int CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {
            if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).IsControlled) continue;
            // Use first non-zero airdistunit for now
            int TermUnitSizingIndex = 0;
            for (int InletNode = 1; InletNode <= state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).NumInletNodes; ++InletNode) {
                TermUnitSizingIndex = state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).AirDistUnitCool(InletNode).TermUnitSizingIndex;
                if (TermUnitSizingIndex == 0) continue;
                termunitsizingtemp = (1.0 + TermUnitSizing(TermUnitSizingIndex).InducRat);
                termunitsizingtempfrac = (1.0 / termunitsizingtemp);
                if (TermUnitSizingIndex > 0) break;
            }
            if (TermUnitSizingIndex == 0) continue; // Skip this if there are no terminal units
            RetTempRise =
                ZoneSizing(CurOverallSimDay, CtrlZoneNum).ZoneRetTempAtCoolPeak - ZoneSizing(CurOverallSimDay, CtrlZoneNum).ZoneTempAtCoolPeak;
            if (RetTempRise > 0.01) {
                // avoid possible compiler bug
                //          ZoneSizing(CtrlZoneNum,CurOverallSimDay)%ZoneRetTempAtCoolPeak = &
                //            ZoneSizing(CtrlZoneNum,CurOverallSimDay)%ZoneTempAtCoolPeak + RetTempRise * &
                //           (1.0d0/(1.0d0+TermUnitSizing(CtrlZoneNum)%InducRat))
                ZoneSizing(CurOverallSimDay, CtrlZoneNum).ZoneRetTempAtCoolPeak =
                    ZoneSizing(CurOverallSimDay, CtrlZoneNum).ZoneTempAtCoolPeak + RetTempRise * termunitsizingtempfrac;
            }
            RetTempRise =
                ZoneSizing(CurOverallSimDay, CtrlZoneNum).ZoneRetTempAtHeatPeak - ZoneSizing(CurOverallSimDay, CtrlZoneNum).ZoneTempAtHeatPeak;
            if (RetTempRise > 0.01) {
                // avoid possible compiler bug
                //          ZoneSizing(CtrlZoneNum,CurOverallSimDay)%ZoneRetTempAtHeatPeak = &
                //            ZoneSizing(CtrlZoneNum,CurOverallSimDay)%ZoneTempAtHeatPeak + RetTempRise * &
                //            (1./(1.+TermUnitSizing(CtrlZoneNum)%InducRat))
                ZoneSizing(CurOverallSimDay, CtrlZoneNum).ZoneRetTempAtHeatPeak =
                    ZoneSizing(CurOverallSimDay, CtrlZoneNum).ZoneTempAtHeatPeak + RetTempRise * termunitsizingtempfrac;
            }
        }

        for (AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum) { // start of begin day loop over primary air systems

            int NumZonesCooled = AirToZoneNodeInfo(AirLoopNum).NumZonesCooled;
            int NumZonesHeated = AirToZoneNodeInfo(AirLoopNum).NumZonesHeated;
            SysSizing(CurOverallSimDay, AirLoopNum).CoolDesDay = state.dataEnvrn->EnvironmentName;
            SysSizing(CurOverallSimDay, AirLoopNum).HeatDesDay = state.dataEnvrn->EnvironmentName;
            state.dataSize->SensCoolCapTemp(AirLoopNum) = 0.0;
            state.dataSize->TotCoolCapTemp(AirLoopNum) = 0.0;

            for (int ZonesCooledNum = 1; ZonesCooledNum <= NumZonesCooled; ++ZonesCooledNum) { // loop over cooled zones
                int CtrlZoneNum = AirToZoneNodeInfo(AirLoopNum).CoolCtrlZoneNums(ZonesCooledNum);
                int TermUnitSizingIndex = AirToZoneNodeInfo(AirLoopNum).TermUnitCoolSizingIndex(ZonesCooledNum);
                Real64 adjCoolMassFlow = TermUnitSizing(TermUnitSizingIndex)
                                             .applyTermUnitSizingCoolFlow(ZoneSizing(CurOverallSimDay, CtrlZoneNum).DesCoolMassFlow,
                                                                          ZoneSizing(CurOverallSimDay, CtrlZoneNum).DesCoolMassFlowNoOA);
                SysSizing(CurOverallSimDay, AirLoopNum).NonCoinCoolMassFlow += adjCoolMassFlow / (1.0 + TermUnitSizing(TermUnitSizingIndex).InducRat);
            } // end of loop over cooled zones

            if (NumZonesHeated > 0) {                                                              // if there are zones supplied with central hot air
                for (int ZonesHeatedNum = 1; ZonesHeatedNum <= NumZonesHeated; ++ZonesHeatedNum) { // loop over heated zones
                    int CtrlZoneNum = AirToZoneNodeInfo(AirLoopNum).HeatCtrlZoneNums(ZonesHeatedNum);
                    int TermUnitSizingIndex = AirToZoneNodeInfo(AirLoopNum).TermUnitHeatSizingIndex(ZonesHeatedNum);
                    Real64 adjHeatMassFlow = TermUnitSizing(TermUnitSizingIndex)
                                                 .applyTermUnitSizingHeatFlow(ZoneSizing(CurOverallSimDay, CtrlZoneNum).DesHeatMassFlow,
                                                                              ZoneSizing(CurOverallSimDay, CtrlZoneNum).DesHeatMassFlowNoOA);
                    SysSizing(CurOverallSimDay, AirLoopNum).NonCoinHeatMassFlow +=
                        adjHeatMassFlow / (1.0 + TermUnitSizing(TermUnitSizingIndex).InducRat);
                }                                                                                  // end of loop over heated zones
            } else {                                                                               // otherwise use cool supply zones
                for (int ZonesCooledNum = 1; ZonesCooledNum <= NumZonesCooled; ++ZonesCooledNum) { // loop over cooled zones
                    int CtrlZoneNum = AirToZoneNodeInfo(AirLoopNum).CoolCtrlZoneNums(ZonesCooledNum);
                    int TermUnitSizingIndex = AirToZoneNodeInfo(AirLoopNum).TermUnitCoolSizingIndex(ZonesCooledNum);
                    Real64 adjHeatMassFlow = TermUnitSizing(TermUnitSizingIndex)
                                                 .applyTermUnitSizingHeatFlow(ZoneSizing(CurOverallSimDay, CtrlZoneNum).DesHeatMassFlow,
                                                                              ZoneSizing(CurOverallSimDay, CtrlZoneNum).DesHeatMassFlowNoOA);
                    SysSizing(CurOverallSimDay, AirLoopNum).NonCoinHeatMassFlow +=
                        adjHeatMassFlow / (1.0 + TermUnitSizing(TermUnitSizingIndex).InducRat);
                } // end of loop over cooled zones
            }     // End of heat / cool zone if - else

        } // End of begin day loop over primary air systems
    } break;
    case DataGlobalConstants::CallIndicator::DuringDay: {
        TimeStepInDay = (state.dataGlobal->HourOfDay - 1) * state.dataGlobal->NumOfTimeStepInHour +
                        state.dataGlobal->TimeStep; // calculate current zone time step index

        // Correct the zone return temperature in ZoneSizing for the case of induction units. The calc in
        // ZoneEquipmentManager assumes all the air entering the zone goes into the return node.
        for (int CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {
            if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).IsControlled) continue;
            // Use first non-zero airdistunit for now, if there is one
            termunitsizingtempfrac = 1.0;
            int TermUnitSizingIndex = 0;
            for (int InletNode = 1; InletNode <= state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).NumInletNodes; ++InletNode) {
                TermUnitSizingIndex = state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).AirDistUnitCool(InletNode).TermUnitSizingIndex;
                if (TermUnitSizingIndex == 0) continue;
                termunitsizingtemp = (1.0 + TermUnitSizing(TermUnitSizingIndex).InducRat);
                termunitsizingtempfrac = (1.0 / termunitsizingtemp);
                if (TermUnitSizingIndex > 0) break;
            }
            if (TermUnitSizingIndex == 0) continue; // Skip this if there are no terminal units
            RetTempRise = ZoneSizing(CurOverallSimDay, CtrlZoneNum).CoolZoneRetTempSeq(TimeStepInDay) -
                          ZoneSizing(CurOverallSimDay, CtrlZoneNum).CoolZoneTempSeq(TimeStepInDay);
            if (RetTempRise > 0.01) {
                // avoid possible compiler bug
                //          ZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolZoneRetTempSeq(TimeStepInDay) = &
                //            ZoneSizing(CtrlZoneNum,CurOverallSimDay)%CoolZoneTempSeq(TimeStepInDay) + RetTempRise * &
                //           (1.0d0/(1.0d0+TermUnitSizing(CtrlZoneNum)%InducRat))
                ZoneSizing(CurOverallSimDay, CtrlZoneNum).CoolZoneRetTempSeq(TimeStepInDay) =
                    ZoneSizing(CurOverallSimDay, CtrlZoneNum).CoolZoneTempSeq(TimeStepInDay) + RetTempRise * termunitsizingtempfrac;
            }
            RetTempRise = ZoneSizing(CurOverallSimDay, CtrlZoneNum).HeatZoneRetTempSeq(TimeStepInDay) -
                          ZoneSizing(CurOverallSimDay, CtrlZoneNum).HeatZoneTempSeq(TimeStepInDay);
            if (RetTempRise > 0.01) {
                ZoneSizing(CurOverallSimDay, CtrlZoneNum).HeatZoneRetTempSeq(TimeStepInDay) =
                    ZoneSizing(CurOverallSimDay, CtrlZoneNum).HeatZoneTempSeq(TimeStepInDay) + RetTempRise * termunitsizingtempfrac;
            }
        }

        for (AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum) { // start of zone time step loop over primary air systems

            int NumZonesCooled = AirToZoneNodeInfo(AirLoopNum).NumZonesCooled;
            int NumZonesHeated = AirToZoneNodeInfo(AirLoopNum).NumZonesHeated;

            SysCoolRetTemp = 0.0;
            OutAirFrac = 0.0;
            SysCoolMixTemp = 0.0;
            SysSensCoolCap = 0.0;
            SysCoolRetHumRat = 0.0;
            SysCoolMixHumRat = 0.0;
            SysCoolZoneAvgTemp = 0.0;
            SysHeatZoneAvgTemp = 0.0;
            SysTotCoolCap = 0.0;
            SysDOASHeatAdd = 0.0;
            SysDOASLatAdd = 0.0;

            for (int ZonesCooledNum = 1; ZonesCooledNum <= NumZonesCooled; ++ZonesCooledNum) { // loop over zones cooled by central system
                int CtrlZoneNum = AirToZoneNodeInfo(AirLoopNum).CoolCtrlZoneNums(ZonesCooledNum);
                int TermUnitSizingIndex = AirToZoneNodeInfo(AirLoopNum).TermUnitCoolSizingIndex(ZonesCooledNum);
                // sum up the system mass flow rate for this time step
                Real64 adjCoolFlowSeq = TermUnitSizing(TermUnitSizingIndex)
                                            .applyTermUnitSizingCoolFlow(ZoneSizing(CurOverallSimDay, CtrlZoneNum).CoolFlowSeq(TimeStepInDay),
                                                                         ZoneSizing(CurOverallSimDay, CtrlZoneNum).CoolFlowSeqNoOA(TimeStepInDay));
                SysSizing(CurOverallSimDay, AirLoopNum).CoolFlowSeq(TimeStepInDay) +=
                    adjCoolFlowSeq / (1.0 + TermUnitSizing(TermUnitSizingIndex).InducRat);
                // sum up the zone cooling load to be met by this system for this time step
                SysSizing(CurOverallSimDay, AirLoopNum).SumZoneCoolLoadSeq(TimeStepInDay) +=
                    ZoneSizing(CurOverallSimDay, CtrlZoneNum).CoolLoadSeq(TimeStepInDay);
                // calculate the return air temperature for this time step
                SysCoolRetTemp += ZoneSizing(CurOverallSimDay, CtrlZoneNum).CoolZoneRetTempSeq(TimeStepInDay) *
                                  ZoneSizing(CurOverallSimDay, CtrlZoneNum).CoolFlowSeq(TimeStepInDay) /
                                  (1.0 + TermUnitSizing(TermUnitSizingIndex).InducRat);
                SysCoolRetHumRat += ZoneSizing(CurOverallSimDay, CtrlZoneNum).CoolZoneHumRatSeq(TimeStepInDay) *
                                    ZoneSizing(CurOverallSimDay, CtrlZoneNum).CoolFlowSeq(TimeStepInDay) /
                                    (1.0 + TermUnitSizing(TermUnitSizingIndex).InducRat);
                SysCoolZoneAvgTemp += ZoneSizing(CurOverallSimDay, CtrlZoneNum).CoolZoneTempSeq(TimeStepInDay) *
                                      ZoneSizing(CurOverallSimDay, CtrlZoneNum).CoolFlowSeq(TimeStepInDay) /
                                      (1.0 + TermUnitSizing(TermUnitSizingIndex).InducRat);
                SysDOASHeatAdd += ZoneSizing(CurOverallSimDay, CtrlZoneNum).DOASHeatAddSeq(TimeStepInDay) *
                                  ZoneSizing(CurOverallSimDay, CtrlZoneNum).CoolFlowSeq(TimeStepInDay) /
                                  (1.0 + TermUnitSizing(TermUnitSizingIndex).InducRat);
                SysDOASLatAdd += ZoneSizing(CurOverallSimDay, CtrlZoneNum).DOASLatAddSeq(TimeStepInDay) *
                                 ZoneSizing(CurOverallSimDay, CtrlZoneNum).CoolFlowSeq(TimeStepInDay) /
                                 (1.0 + TermUnitSizing(TermUnitSizingIndex).InducRat);
            } // end of loop over zones cooled by central system
            // check that there is system mass flow
            if (SysSizing(CurOverallSimDay, AirLoopNum).CoolFlowSeq(TimeStepInDay) > 0.0) {
                // complete return air temp calc
                SysCoolRetTemp /= SysSizing(CurOverallSimDay, AirLoopNum).CoolFlowSeq(TimeStepInDay);
                SysCoolRetHumRat /= SysSizing(CurOverallSimDay, AirLoopNum).CoolFlowSeq(TimeStepInDay);
                SysCoolZoneAvgTemp /= SysSizing(CurOverallSimDay, AirLoopNum).CoolFlowSeq(TimeStepInDay);
                SysSizing(CurOverallSimDay, AirLoopNum).SysCoolRetTempSeq(TimeStepInDay) = SysCoolRetTemp;
                SysSizing(CurOverallSimDay, AirLoopNum).SysCoolRetHumRatSeq(TimeStepInDay) = SysCoolRetHumRat;
                SysSizing(CurOverallSimDay, AirLoopNum).CoolZoneAvgTempSeq(TimeStepInDay) = SysCoolZoneAvgTemp;
                // calculate the outside air fraction for this time step
                RhoAir = state.dataEnvrn->StdRhoAir;
                if (SysSizing(CurOverallSimDay, AirLoopNum).CoolOAOption == MinOA) {
                    OutAirFrac = RhoAir * SysSizing(CurOverallSimDay, AirLoopNum).DesOutAirVolFlow /
                                 SysSizing(CurOverallSimDay, AirLoopNum).CoolFlowSeq(TimeStepInDay);
                    OutAirFrac = min(1.0, max(0.0, OutAirFrac));
                } else {
                    OutAirFrac = 1.0;
                }
                // now calculate the mixed air temperature
                SysCoolMixTemp = state.dataEnvrn->OutDryBulbTemp * OutAirFrac + SysCoolRetTemp * (1.0 - OutAirFrac);
                SysCoolMixHumRat = state.dataEnvrn->OutHumRat * OutAirFrac + SysCoolRetHumRat * (1.0 - OutAirFrac);
                SysSizing(CurOverallSimDay, AirLoopNum).SysCoolOutTempSeq(TimeStepInDay) = state.dataEnvrn->OutDryBulbTemp;
                SysSizing(CurOverallSimDay, AirLoopNum).SysCoolOutHumRatSeq(TimeStepInDay) = state.dataEnvrn->OutHumRat;
                // From the mixed air temp, system design supply air temp, and the mass flow rate
                // calculate the system sensible cooling capacity
                SysSensCoolCap = PsyCpAirFnW(DataPrecisionGlobals::constant_zero) *
                                 SysSizing(CurOverallSimDay, AirLoopNum).CoolFlowSeq(TimeStepInDay) *
                                 (SysCoolMixTemp - SysSizing(CurOverallSimDay, AirLoopNum).CoolSupTemp);
                SysSensCoolCap = max(0.0, SysSensCoolCap);
                // calculate the system total cooling capacity
                SysTotCoolCap = SysSizing(CurOverallSimDay, AirLoopNum).CoolFlowSeq(TimeStepInDay) *
                                (PsyHFnTdbW(SysCoolMixTemp, SysCoolMixHumRat) - PsyHFnTdbW(SysSizing(CurOverallSimDay, AirLoopNum).CoolSupTemp,
                                                                                           SysSizing(CurOverallSimDay, AirLoopNum).CoolSupHumRat));
                SysTotCoolCap = max(0.0, SysTotCoolCap);
                // Save the sens cool cap for this time step
                SysSizing(CurOverallSimDay, AirLoopNum).SensCoolCapSeq(TimeStepInDay) = SysSensCoolCap;
                // Save the tot cool cap for this time step
                SysSizing(CurOverallSimDay, AirLoopNum).TotCoolCapSeq(TimeStepInDay) = SysTotCoolCap;
                // Save the DOAS flows
                SysSizing(CurOverallSimDay, AirLoopNum).SysDOASHeatAddSeq(TimeStepInDay) = SysDOASHeatAdd;
                SysSizing(CurOverallSimDay, AirLoopNum).SysDOASLatAddSeq(TimeStepInDay) = SysDOASLatAdd;
            } // end of system mass flow check

            // get the maximum system sensible cooling capacity
            if (SysSensCoolCap > state.dataSize->SensCoolCapTemp(AirLoopNum)) {
                SysSizPeakDDNum(AirLoopNum).TimeStepAtSensCoolPk(CurOverallSimDay) = TimeStepInDay;
                state.dataSize->SensCoolCapTemp(AirLoopNum) = SysSensCoolCap;
                if (SysSizing(CurOverallSimDay, AirLoopNum).CoolingPeakLoadType == SensibleCoolingLoad) {
                    SysSizing(CurOverallSimDay, AirLoopNum).SensCoolCap = SysSensCoolCap;
                    SysSizing(CurOverallSimDay, AirLoopNum).TotCoolCap = SysTotCoolCap;
                    SysSizing(CurOverallSimDay, AirLoopNum).MixTempAtCoolPeak = SysCoolMixTemp;
                    SysSizing(CurOverallSimDay, AirLoopNum).MixHumRatAtCoolPeak = SysCoolMixHumRat;
                    SysSizing(CurOverallSimDay, AirLoopNum).RetTempAtCoolPeak = SysCoolRetTemp;
                    SysSizing(CurOverallSimDay, AirLoopNum).RetHumRatAtCoolPeak = SysCoolRetHumRat;
                    SysSizing(CurOverallSimDay, AirLoopNum).OutTempAtCoolPeak = state.dataEnvrn->OutDryBulbTemp;
                    SysSizing(CurOverallSimDay, AirLoopNum).OutHumRatAtCoolPeak = state.dataEnvrn->OutHumRat;
                    SysSizing(CurOverallSimDay, AirLoopNum).MassFlowAtCoolPeak = SysSizing(CurOverallSimDay, AirLoopNum).CoolFlowSeq(TimeStepInDay);
                }
            }
            // get the maximum system total cooling capacity
            if (SysTotCoolCap > state.dataSize->TotCoolCapTemp(AirLoopNum)) {
                SysSizPeakDDNum(AirLoopNum).TimeStepAtTotCoolPk(CurOverallSimDay) = TimeStepInDay;
                state.dataSize->TotCoolCapTemp(AirLoopNum) = SysTotCoolCap;
                if (SysSizing(CurOverallSimDay, AirLoopNum).CoolingPeakLoadType == TotalCoolingLoad) {
                    SysSizing(CurOverallSimDay, AirLoopNum).SensCoolCap = SysSensCoolCap;
                    SysSizing(CurOverallSimDay, AirLoopNum).TotCoolCap = SysTotCoolCap;
                    SysSizing(CurOverallSimDay, AirLoopNum).MixTempAtCoolPeak = SysCoolMixTemp;
                    SysSizing(CurOverallSimDay, AirLoopNum).MixHumRatAtCoolPeak = SysCoolMixHumRat;
                    SysSizing(CurOverallSimDay, AirLoopNum).RetTempAtCoolPeak = SysCoolRetTemp;
                    SysSizing(CurOverallSimDay, AirLoopNum).RetHumRatAtCoolPeak = SysCoolRetHumRat;
                    SysSizing(CurOverallSimDay, AirLoopNum).OutTempAtCoolPeak = state.dataEnvrn->OutDryBulbTemp;
                    SysSizing(CurOverallSimDay, AirLoopNum).OutHumRatAtCoolPeak = state.dataEnvrn->OutHumRat;
                    SysSizing(CurOverallSimDay, AirLoopNum).MassFlowAtCoolPeak = SysSizing(CurOverallSimDay, AirLoopNum).CoolFlowSeq(TimeStepInDay);
                }
                SysSizing(CurOverallSimDay, AirLoopNum).SysCoolCoinSpaceSens = 0.0;
                for (int zonesCoolLoop = 1; zonesCoolLoop <= AirToZoneNodeInfo(AirLoopNum).NumZonesCooled; ++zonesCoolLoop) {
                    int zoneNum = AirToZoneNodeInfo(AirLoopNum).CoolCtrlZoneNums(zonesCoolLoop);
                    SysSizing(CurOverallSimDay, AirLoopNum).SysCoolCoinSpaceSens +=
                        CalcZoneSizing(CurOverallSimDay, zoneNum).CoolLoadSeq(TimeStepInDay);
                }
            }
            // get the maximum cooling mass flow rate
            if (SysSizing(CurOverallSimDay, AirLoopNum).CoolFlowSeq(TimeStepInDay) > SysSizing(CurOverallSimDay, AirLoopNum).CoinCoolMassFlow) {
                SysSizing(CurOverallSimDay, AirLoopNum).CoinCoolMassFlow = SysSizing(CurOverallSimDay, AirLoopNum).CoolFlowSeq(TimeStepInDay);
                SysSizPeakDDNum(AirLoopNum).TimeStepAtCoolFlowPk(CurOverallSimDay) = TimeStepInDay;
            }
            SysHeatRetTemp = 0.0;
            OutAirFrac = 0.0;
            SysHeatMixTemp = 0.0;
            SysHeatCap = 0.0;
            SysHeatRetHumRat = 0.0;
            SysHeatMixHumRat = 0.0;

            if (NumZonesHeated > 0) { // IF there are centrally heated zones

                for (int ZonesHeatedNum = 1; ZonesHeatedNum <= NumZonesHeated; ++ZonesHeatedNum) { // loop over the heated zones
                    int CtrlZoneNum = AirToZoneNodeInfo(AirLoopNum).HeatCtrlZoneNums(ZonesHeatedNum);
                    int TermUnitSizingIndex = AirToZoneNodeInfo(AirLoopNum).TermUnitHeatSizingIndex(ZonesHeatedNum);
                    // sum up the heating mass flow rate for this time step
                    Real64 adjHeatFlowSeq =
                        TermUnitSizing(TermUnitSizingIndex)
                            .applyTermUnitSizingHeatFlow(ZoneSizing(CurOverallSimDay, CtrlZoneNum).HeatFlowSeq(TimeStepInDay),
                                                         ZoneSizing(CurOverallSimDay, CtrlZoneNum).HeatFlowSeqNoOA(TimeStepInDay));
                    SysSizing(CurOverallSimDay, AirLoopNum).HeatFlowSeq(TimeStepInDay) +=
                        adjHeatFlowSeq / (1.0 + TermUnitSizing(TermUnitSizingIndex).InducRat);
                    // sum up the zone heating load to be met by this system for this time step
                    SysSizing(CurOverallSimDay, AirLoopNum).SumZoneHeatLoadSeq(TimeStepInDay) +=
                        ZoneSizing(CurOverallSimDay, CtrlZoneNum).HeatLoadSeq(TimeStepInDay) / (1.0 + TermUnitSizing(TermUnitSizingIndex).InducRat);
                    // calculate the return air temperature for this time step
                    SysHeatRetTemp += ZoneSizing(CurOverallSimDay, CtrlZoneNum).HeatZoneRetTempSeq(TimeStepInDay) *
                                      ZoneSizing(CurOverallSimDay, CtrlZoneNum).HeatFlowSeq(TimeStepInDay) /
                                      (1.0 + TermUnitSizing(TermUnitSizingIndex).InducRat);
                    SysHeatRetHumRat += ZoneSizing(CurOverallSimDay, CtrlZoneNum).HeatZoneHumRatSeq(TimeStepInDay) *
                                        ZoneSizing(CurOverallSimDay, CtrlZoneNum).HeatFlowSeq(TimeStepInDay) /
                                        (1.0 + TermUnitSizing(TermUnitSizingIndex).InducRat);
                    SysHeatZoneAvgTemp += ZoneSizing(CurOverallSimDay, CtrlZoneNum).HeatZoneTempSeq(TimeStepInDay) *
                                          ZoneSizing(CurOverallSimDay, CtrlZoneNum).HeatFlowSeq(TimeStepInDay) /
                                          (1.0 + TermUnitSizing(TermUnitSizingIndex).InducRat);
                } // end heated zones loop
                // check that the system flow rate is nonzero
                if (SysSizing(CurOverallSimDay, AirLoopNum).HeatFlowSeq(TimeStepInDay) > 0.0) {
                    // complete return air temp calc
                    SysHeatRetTemp /= SysSizing(CurOverallSimDay, AirLoopNum).HeatFlowSeq(TimeStepInDay);
                    SysHeatRetHumRat /= SysSizing(CurOverallSimDay, AirLoopNum).HeatFlowSeq(TimeStepInDay);
                    SysHeatZoneAvgTemp /= SysSizing(CurOverallSimDay, AirLoopNum).HeatFlowSeq(TimeStepInDay);
                    SysSizing(CurOverallSimDay, AirLoopNum).SysHeatRetTempSeq(TimeStepInDay) = SysHeatRetTemp;
                    SysSizing(CurOverallSimDay, AirLoopNum).SysHeatRetHumRatSeq(TimeStepInDay) = SysHeatRetHumRat;
                    SysSizing(CurOverallSimDay, AirLoopNum).HeatZoneAvgTempSeq(TimeStepInDay) = SysHeatZoneAvgTemp;
                    // calculate the outside air fraction for this time step
                    RhoAir = state.dataEnvrn->StdRhoAir;
                    if (SysSizing(CurOverallSimDay, AirLoopNum).HeatOAOption == MinOA) {
                        OutAirFrac = RhoAir * SysSizing(CurOverallSimDay, AirLoopNum).DesOutAirVolFlow /
                                     SysSizing(CurOverallSimDay, AirLoopNum).HeatFlowSeq(TimeStepInDay);
                        OutAirFrac = min(1.0, max(0.0, OutAirFrac));
                    } else {
                        OutAirFrac = 1.0;
                    }
                    // calculate the mixed air temperature
                    SysHeatMixTemp = state.dataEnvrn->OutDryBulbTemp * OutAirFrac + SysHeatRetTemp * (1.0 - OutAirFrac);
                    SysHeatMixHumRat = state.dataEnvrn->OutHumRat * OutAirFrac + SysHeatRetHumRat * (1.0 - OutAirFrac);
                    SysSizing(CurOverallSimDay, AirLoopNum).SysHeatOutTempSeq(TimeStepInDay) = state.dataEnvrn->OutDryBulbTemp;
                    SysSizing(CurOverallSimDay, AirLoopNum).SysHeatOutHumRatSeq(TimeStepInDay) = state.dataEnvrn->OutHumRat;
                    // From the mixed air temp, heating supply air temp, and mass flow rate calculate the system heating capacity
                    SysHeatCap = PsyCpAirFnW(DataPrecisionGlobals::constant_zero) *
                                 SysSizing(CurOverallSimDay, AirLoopNum).HeatFlowSeq(TimeStepInDay) *
                                 (SysSizing(CurOverallSimDay, AirLoopNum).HeatSupTemp - SysHeatMixTemp);
                    SysHeatCap = max(0.0, SysHeatCap);
                    // save the system heating capacity for the time step
                    SysSizing(CurOverallSimDay, AirLoopNum).HeatCapSeq(TimeStepInDay) = SysHeatCap;
                } // end system flow rate IF

                // Get the maximum system heating capacity
                if (SysHeatCap > SysSizing(CurOverallSimDay, AirLoopNum).HeatCap) {
                    SysSizPeakDDNum(AirLoopNum).TimeStepAtHeatPk(CurOverallSimDay) = TimeStepInDay;
                    SysSizing(CurOverallSimDay, AirLoopNum).HeatCap = SysHeatCap;
                    SysSizing(CurOverallSimDay, AirLoopNum).HeatMixTemp = SysHeatMixTemp;
                    SysSizing(CurOverallSimDay, AirLoopNum).HeatMixHumRat = SysHeatMixHumRat;
                    SysSizing(CurOverallSimDay, AirLoopNum).HeatRetTemp = SysHeatRetTemp;
                    SysSizing(CurOverallSimDay, AirLoopNum).HeatRetHumRat = SysHeatRetHumRat;
                    SysSizing(CurOverallSimDay, AirLoopNum).HeatOutTemp = state.dataEnvrn->OutDryBulbTemp;
                    SysSizing(CurOverallSimDay, AirLoopNum).HeatOutHumRat = state.dataEnvrn->OutHumRat;
                    // save time of system coincident heating coil peak
                    SysSizing(CurOverallSimDay, AirLoopNum).SysHeatCoilTimeStepPk = TimeStepInDay;
                    SysSizing(CurOverallSimDay, AirLoopNum).SysHeatCoinSpaceSens = 0.0;
                    if (AirToZoneNodeInfo(AirLoopNum).NumZonesHeated > 0) {
                        for (int zonesHeatLoop = 1; zonesHeatLoop <= AirToZoneNodeInfo(AirLoopNum).NumZonesHeated; ++zonesHeatLoop) {
                            int zoneNum = AirToZoneNodeInfo(AirLoopNum).HeatCtrlZoneNums(zonesHeatLoop);
                            SysSizing(CurOverallSimDay, AirLoopNum).SysHeatCoinSpaceSens +=
                                CalcZoneSizing(CurOverallSimDay, zoneNum).HeatLoadSeq(TimeStepInDay);
                        }
                    }
                }
                //! save time of system coincident heating airflow peak
                if (SysSizing(CurOverallSimDay, AirLoopNum).HeatFlowSeq(TimeStepInDay) > SysSizing(CurOverallSimDay, AirLoopNum).CoinHeatMassFlow) {
                    SysSizing(CurOverallSimDay, AirLoopNum).SysHeatAirTimeStepPk = TimeStepInDay;
                }

                // Get the maximum system heating flow rate
                SysSizing(CurOverallSimDay, AirLoopNum).CoinHeatMassFlow =
                    max(SysSizing(CurOverallSimDay, AirLoopNum).CoinHeatMassFlow, SysSizing(CurOverallSimDay, AirLoopNum).HeatFlowSeq(TimeStepInDay));

            } else { // No centrally heated zones: use cooled zones

                for (int ZonesCooledNum = 1; ZonesCooledNum <= NumZonesCooled; ++ZonesCooledNum) { // loop over the cooled zones
                    int CtrlZoneNum = AirToZoneNodeInfo(AirLoopNum).CoolCtrlZoneNums(ZonesCooledNum);
                    int TermUnitSizingIndex = AirToZoneNodeInfo(AirLoopNum).TermUnitCoolSizingIndex(ZonesCooledNum);
                    // sum up the heating mass flow rate for this time step
                    Real64 adjHeatFlowSeq =
                        TermUnitSizing(TermUnitSizingIndex)
                            .applyTermUnitSizingHeatFlow(ZoneSizing(CurOverallSimDay, CtrlZoneNum).HeatFlowSeq(TimeStepInDay),
                                                         ZoneSizing(CurOverallSimDay, CtrlZoneNum).HeatFlowSeqNoOA(TimeStepInDay));
                    SysSizing(CurOverallSimDay, AirLoopNum).HeatFlowSeq(TimeStepInDay) +=
                        adjHeatFlowSeq / (1.0 + TermUnitSizing(TermUnitSizingIndex).InducRat);
                    // sum up the zone heating load to be met by this system for this time step
                    SysSizing(CurOverallSimDay, AirLoopNum).SumZoneHeatLoadSeq(TimeStepInDay) +=
                        ZoneSizing(CurOverallSimDay, CtrlZoneNum).HeatLoadSeq(TimeStepInDay) / (1.0 + TermUnitSizing(TermUnitSizingIndex).InducRat);
                    // calculate the return air temperature for this time step
                    SysHeatRetTemp += ZoneSizing(CurOverallSimDay, CtrlZoneNum).HeatZoneRetTempSeq(TimeStepInDay) *
                                      ZoneSizing(CurOverallSimDay, CtrlZoneNum).HeatFlowSeq(TimeStepInDay) /
                                      (1.0 + TermUnitSizing(TermUnitSizingIndex).InducRat);
                    SysHeatRetHumRat += ZoneSizing(CurOverallSimDay, CtrlZoneNum).HeatZoneHumRatSeq(TimeStepInDay) *
                                        ZoneSizing(CurOverallSimDay, CtrlZoneNum).HeatFlowSeq(TimeStepInDay) /
                                        (1.0 + TermUnitSizing(TermUnitSizingIndex).InducRat);
                    SysHeatZoneAvgTemp += ZoneSizing(CurOverallSimDay, CtrlZoneNum).HeatZoneTempSeq(TimeStepInDay) *
                                          ZoneSizing(CurOverallSimDay, CtrlZoneNum).HeatFlowSeq(TimeStepInDay) /
                                          (1.0 + TermUnitSizing(TermUnitSizingIndex).InducRat);
                } // end of cooled zones loop

                if (SysSizing(CurOverallSimDay, AirLoopNum).HeatFlowSeq(TimeStepInDay) > 0.0) {
                    // complete return air temp calc
                    SysHeatRetTemp /= SysSizing(CurOverallSimDay, AirLoopNum).HeatFlowSeq(TimeStepInDay);
                    SysHeatRetHumRat /= SysSizing(CurOverallSimDay, AirLoopNum).HeatFlowSeq(TimeStepInDay);
                    SysHeatZoneAvgTemp /= SysSizing(CurOverallSimDay, AirLoopNum).HeatFlowSeq(TimeStepInDay);
                    SysSizing(CurOverallSimDay, AirLoopNum).SysHeatRetTempSeq(TimeStepInDay) = SysHeatRetTemp;
                    SysSizing(CurOverallSimDay, AirLoopNum).SysHeatRetHumRatSeq(TimeStepInDay) = SysHeatRetHumRat;
                    SysSizing(CurOverallSimDay, AirLoopNum).HeatZoneAvgTempSeq(TimeStepInDay) = SysHeatZoneAvgTemp;
                    // calculate the outside air fraction for this time step
                    RhoAir = state.dataEnvrn->StdRhoAir;
                    if (SysSizing(CurOverallSimDay, AirLoopNum).HeatOAOption == MinOA) {
                        OutAirFrac = RhoAir * SysSizing(CurOverallSimDay, AirLoopNum).DesOutAirVolFlow /
                                     SysSizing(CurOverallSimDay, AirLoopNum).HeatFlowSeq(TimeStepInDay);
                        OutAirFrac = min(1.0, max(0.0, OutAirFrac));
                    } else {
                        OutAirFrac = 1.0;
                    }
                    // calculate the mixed air temperature
                    SysHeatMixTemp = state.dataEnvrn->OutDryBulbTemp * OutAirFrac + SysHeatRetTemp * (1.0 - OutAirFrac);
                    SysHeatMixHumRat = state.dataEnvrn->OutHumRat * OutAirFrac + SysHeatRetHumRat * (1.0 - OutAirFrac);
                    SysSizing(CurOverallSimDay, AirLoopNum).SysHeatOutTempSeq(TimeStepInDay) = state.dataEnvrn->OutDryBulbTemp;
                    SysSizing(CurOverallSimDay, AirLoopNum).SysHeatOutHumRatSeq(TimeStepInDay) = state.dataEnvrn->OutHumRat;
                    // From the mixed air temp, heating supply air temp, and mass flow rate calculate the system heating capacity
                    SysHeatCap = PsyCpAirFnW(DataPrecisionGlobals::constant_zero) *
                                 SysSizing(CurOverallSimDay, AirLoopNum).HeatFlowSeq(TimeStepInDay) *
                                 (SysSizing(CurOverallSimDay, AirLoopNum).HeatSupTemp - SysHeatMixTemp);
                    SysHeatCap = max(0.0, SysHeatCap);
                    // save the system heating capacity for the time step
                    SysSizing(CurOverallSimDay, AirLoopNum).HeatCapSeq(TimeStepInDay) = SysHeatCap;
                } // end system flow rate IF

                // Get the maximum system heating capacity
                if (SysHeatCap > SysSizing(CurOverallSimDay, AirLoopNum).HeatCap) {
                    SysSizPeakDDNum(AirLoopNum).TimeStepAtHeatPk(CurOverallSimDay) = TimeStepInDay;
                    SysSizing(CurOverallSimDay, AirLoopNum).HeatCap = SysHeatCap;
                    SysSizing(CurOverallSimDay, AirLoopNum).HeatMixTemp = SysHeatMixTemp;
                    SysSizing(CurOverallSimDay, AirLoopNum).HeatMixHumRat = SysHeatMixHumRat;
                    SysSizing(CurOverallSimDay, AirLoopNum).HeatRetTemp = SysHeatRetTemp;
                    SysSizing(CurOverallSimDay, AirLoopNum).HeatRetHumRat = SysHeatRetHumRat;
                    SysSizing(CurOverallSimDay, AirLoopNum).HeatOutTemp = state.dataEnvrn->OutDryBulbTemp;
                    SysSizing(CurOverallSimDay, AirLoopNum).HeatOutHumRat = state.dataEnvrn->OutHumRat;
                    // save time of system coincident heating coil peak
                    SysSizing(CurOverallSimDay, AirLoopNum).SysHeatCoilTimeStepPk = TimeStepInDay;

                    SysSizing(CurOverallSimDay, AirLoopNum).SysHeatCoinSpaceSens = 0.0;
                    for (int zonesCoolLoop = 1; zonesCoolLoop <= AirToZoneNodeInfo(AirLoopNum).NumZonesCooled; ++zonesCoolLoop) {
                        int zoneNum = AirToZoneNodeInfo(AirLoopNum).CoolCtrlZoneNums(zonesCoolLoop);
                        SysSizing(CurOverallSimDay, AirLoopNum).SysHeatCoinSpaceSens +=
                            CalcZoneSizing(CurOverallSimDay, zoneNum).HeatLoadSeq(TimeStepInDay);
                    }
                } // Get the maximum system heating flow rate
                // save time of system coincident heating airflow peak
                if (SysSizing(CurOverallSimDay, AirLoopNum).HeatFlowSeq(TimeStepInDay) > SysSizing(CurOverallSimDay, AirLoopNum).CoinHeatMassFlow) {
                    SysSizing(CurOverallSimDay, AirLoopNum).SysHeatAirTimeStepPk = TimeStepInDay;
                }

                SysSizing(CurOverallSimDay, AirLoopNum).CoinHeatMassFlow =
                    max(SysSizing(CurOverallSimDay, AirLoopNum).CoinHeatMassFlow, SysSizing(CurOverallSimDay, AirLoopNum).HeatFlowSeq(TimeStepInDay));
            }

        } // end of loop over primary air systems
    } break;
    case DataGlobalConstants::CallIndicator::EndDay: {
        // the entire set of std. 62.1 code here seems misplaced, should have been placed in EndSysSizCalc block
        // Get design flows
        SysCoolingEv = 1.0;
        SysHeatingEv = 1.0;
        for (AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum) {

            int NumZonesCooled = AirToZoneNodeInfo(AirLoopNum).NumZonesCooled;
            int NumZonesHeated = AirToZoneNodeInfo(AirLoopNum).NumZonesHeated;

            {
                auto const SELECT_CASE_var1(SysSizing(CurOverallSimDay, AirLoopNum).SizingOption);
                if (SELECT_CASE_var1 == Coincident) {
                    if (FinalSysSizing(AirLoopNum).SystemOAMethod == SOAM_ZoneSum) {
                        SysSizing(CurOverallSimDay, AirLoopNum).DesCoolVolFlow =
                            SysSizing(CurOverallSimDay, AirLoopNum).CoinCoolMassFlow / state.dataEnvrn->StdRhoAir;
                        SysSizing(CurOverallSimDay, AirLoopNum).DesHeatVolFlow =
                            SysSizing(CurOverallSimDay, AirLoopNum).CoinHeatMassFlow / state.dataEnvrn->StdRhoAir;
                        state.dataSize->VotClgBySys(AirLoopNum) = FinalSysSizing(AirLoopNum).SysUncOA;
                        state.dataSize->VotHtgBySys(AirLoopNum) = FinalSysSizing(AirLoopNum).SysUncOA;
                        for (int ZonesCooledNum = 1; ZonesCooledNum <= NumZonesCooled; ++ZonesCooledNum) {
                            int TermUnitSizingIndex = AirToZoneNodeInfo(AirLoopNum).TermUnitCoolSizingIndex(ZonesCooledNum);
                            if (TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneADEffCooling < state.dataSize->EvzMinBySysCool(AirLoopNum))
                                state.dataSize->EvzMinBySysCool(AirLoopNum) = TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneADEffCooling;
                            if (TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneADEffHeating < state.dataSize->EvzMinBySysHeat(AirLoopNum))
                                state.dataSize->EvzMinBySysHeat(AirLoopNum) = TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneADEffHeating;
                        }
                        for (int ZonesHeatedNum = 1; ZonesHeatedNum <= NumZonesHeated; ++ZonesHeatedNum) {
                            int TermUnitSizingIndex = AirToZoneNodeInfo(AirLoopNum).TermUnitHeatSizingIndex(ZonesHeatedNum);
                            if (TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneADEffCooling < state.dataSize->EvzMinBySysCool(AirLoopNum))
                                state.dataSize->EvzMinBySysCool(AirLoopNum) = TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneADEffCooling;
                            if (TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneADEffHeating < state.dataSize->EvzMinBySysHeat(AirLoopNum))
                                state.dataSize->EvzMinBySysHeat(AirLoopNum) = TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneADEffHeating;
                        }
                        if (SysSizing(CurOverallSimDay, AirLoopNum).DesCoolVolFlow > 0) {
                            state.dataSize->XsBySysCool(AirLoopNum) =
                                min(1.0, FinalSysSizing(AirLoopNum).SysUncOA / SysSizing(CurOverallSimDay, AirLoopNum).DesCoolVolFlow);
                        } else {
                            state.dataSize->XsBySysCool(AirLoopNum) = 0.0;
                        }
                        if (SysSizing(CurOverallSimDay, AirLoopNum).DesHeatVolFlow > 0) {
                            state.dataSize->XsBySysHeat(AirLoopNum) =
                                min(1.0, FinalSysSizing(AirLoopNum).SysUncOA / SysSizing(CurOverallSimDay, AirLoopNum).DesHeatVolFlow);
                        } else {
                            state.dataSize->XsBySysHeat(AirLoopNum) = 0.0;
                        }
                    } else if (FinalSysSizing(AirLoopNum).SystemOAMethod == SOAM_VRP ||
                               FinalSysSizing(AirLoopNum).SystemOAMethod == SOAM_SP) { // Ventilation Rate and Simplified Procedure
                        // cooling
                        SysSizing(CurOverallSimDay, AirLoopNum).DesCoolVolFlow =
                            SysSizing(CurOverallSimDay, AirLoopNum).CoinCoolMassFlow / state.dataEnvrn->StdRhoAir;
                        if (SysSizing(CurOverallSimDay, AirLoopNum).DesCoolVolFlow > 0) {
                            OutAirFrac =
                                SysSizing(CurOverallSimDay, AirLoopNum).DesOutAirVolFlow / SysSizing(CurOverallSimDay, AirLoopNum).DesCoolVolFlow;
                        } else {
                            OutAirFrac = 0.0;
                        }
                        OutAirFrac = min(1.0, max(0.0, OutAirFrac));
                        if (SysSizing(CurOverallSimDay, AirLoopNum).DesCoolVolFlow > 0) {
                            state.dataSimAirServingZones->Xs =
                                min(1.0, FinalSysSizing(AirLoopNum).SysUncOA / SysSizing(CurOverallSimDay, AirLoopNum).DesCoolVolFlow);
                        } else {
                            state.dataSimAirServingZones->Xs = 0.0;
                        }
                        if (FinalSysSizing(AirLoopNum).OAAutoSized && SysSizing(CurOverallSimDay, AirLoopNum).DesCoolVolFlow > 0) {
                            int numZonesCooled = AirToZoneNodeInfo(AirLoopNum).NumZonesCooled;
                            state.dataSimAirServingZones->MinCoolingEvz = 1.0;
                            state.dataSize->VozSumClgBySys(AirLoopNum) = 0.0;
                            for (int ZonesCooledNum = 1; ZonesCooledNum <= numZonesCooled; ++ZonesCooledNum) {
                                int TermUnitSizingIndex = AirToZoneNodeInfo(AirLoopNum).TermUnitCoolSizingIndex(ZonesCooledNum);

                                // Zone air secondary recirculation fraction
                                state.dataSimAirServingZones->Er = TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneSecondaryRecirculation;
                                state.dataSimAirServingZones->Ep = TermUnitFinalZoneSizing(TermUnitSizingIndex).ZonePrimaryAirFraction;
                                state.dataSimAirServingZones->ZoneOAFrac = TermUnitFinalZoneSizing(TermUnitSizingIndex).ZpzClgByZone;
                                state.dataSimAirServingZones->ZoneEz = TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneADEffCooling;
                                VozClg = TermUnitFinalZoneSizing(TermUnitSizingIndex).VozClgByZone;
                                if (FinalSysSizing(AirLoopNum).SystemOAMethod == SOAM_SP) { // 62.1 simplified procedure
                                    if (state.dataSize->DBySys(AirLoopNum) < 0.60) {
                                        state.dataSize->EvzByZoneCool(TermUnitSizingIndex) = 0.88 * state.dataSize->DBySys(AirLoopNum) + 0.22;
                                    } else {
                                        state.dataSize->EvzByZoneCool(TermUnitSizingIndex) = 0.75;
                                    }
                                    state.dataSimAirServingZones->MinCoolingEvz = state.dataSize->EvzByZoneCool(TermUnitSizingIndex);
                                } else {
                                    if (state.dataSimAirServingZones->Er > 0.0) {
                                        // multi-path ventilation system using VRP
                                        state.dataSimAirServingZones->Fa =
                                            state.dataSimAirServingZones->Ep +
                                            (1.0 - state.dataSimAirServingZones->Ep) * state.dataSimAirServingZones->Er;
                                        state.dataSimAirServingZones->Fb = state.dataSimAirServingZones->Ep;
                                        state.dataSimAirServingZones->Fc = 1.0 - (1.0 - state.dataSimAirServingZones->ZoneEz) *
                                                                                     (1.0 - state.dataSimAirServingZones->Er) *
                                                                                     (1.0 - state.dataSimAirServingZones->Ep);
                                        // save Fa Fb and Fc for standard 62.1 report
                                        state.dataSize->FaByZoneCool(TermUnitSizingIndex) = state.dataSimAirServingZones->Fa;
                                        state.dataSize->FbByZoneCool(TermUnitSizingIndex) = state.dataSimAirServingZones->Fb;
                                        state.dataSize->FcByZoneCool(TermUnitSizingIndex) = state.dataSimAirServingZones->Fc;

                                        // Calc zone ventilation efficiency
                                        if (state.dataSimAirServingZones->Fa > 0.0) {
                                            SysCoolingEv = 1.0 +
                                                           state.dataSimAirServingZones->Xs * state.dataSimAirServingZones->Fb /
                                                               state.dataSimAirServingZones->Fa -
                                                           state.dataSimAirServingZones->ZoneOAFrac * state.dataSimAirServingZones->Ep *
                                                               state.dataSimAirServingZones->Fc / state.dataSimAirServingZones->Fa;
                                        } else {
                                            SysCoolingEv = 1.0;
                                        }

                                    } else {
                                        // single-path ventilation system
                                        SysCoolingEv = 1.0 + state.dataSimAirServingZones->Xs - state.dataSimAirServingZones->ZoneOAFrac;
                                        // Apply ventilation efficiency limit; reset SysCoolingEv if necessary
                                        LimitZoneVentEff(state, state.dataSimAirServingZones->Xs, VozClg, TermUnitSizingIndex, SysCoolingEv);
                                    }
                                    if (SysCoolingEv < state.dataSimAirServingZones->MinCoolingEvz)
                                        state.dataSimAirServingZones->MinCoolingEvz = SysCoolingEv;
                                    state.dataSize->EvzByZoneCoolPrev(TermUnitSizingIndex) =
                                        state.dataSize->EvzByZoneCool(TermUnitSizingIndex); // Save previous EvzByZoneCool
                                    state.dataSize->EvzByZoneCool(TermUnitSizingIndex) = SysCoolingEv;
                                }
                                state.dataSize->VozSumClgBySys(AirLoopNum) += TermUnitFinalZoneSizing(TermUnitSizingIndex).VozClgByZone;
                            }

                            if (state.dataSimAirServingZones->MinCoolingEvz > 0) {
                                // (However, I don't think people diversity can be done correctly in E+ Sizing so assuming D=1 in this
                                // equation
                                // Vou = Diversity*(Rp*Pz) + Ra*Az
                                state.dataSimAirServingZones->Vou = FinalSysSizing(AirLoopNum).SysUncOA;
                                state.dataSimAirServingZones->Vot = state.dataSimAirServingZones->Vou / state.dataSimAirServingZones->MinCoolingEvz;
                                if (state.dataSimAirServingZones->Vot > state.dataSize->VotClgBySys(AirLoopNum)) {
                                    // This might be the cooling design day so only update if Vot is larger than the previous
                                    state.dataSize->VotClgBySys(AirLoopNum) = state.dataSimAirServingZones->Vot;
                                    state.dataSize->XsBySysCool(AirLoopNum) = state.dataSimAirServingZones->Xs;
                                    state.dataSize->EvzMinBySysCool(AirLoopNum) = state.dataSimAirServingZones->MinCoolingEvz;
                                } else {
                                    // Restore EvzByZoneCool() since it was reset by the current (but not highest Vot) design day
                                    for (int ZonesCooledNum = 1; ZonesCooledNum <= numZonesCooled; ++ZonesCooledNum) {
                                        int TermUnitSizingIndex = AirToZoneNodeInfo(AirLoopNum).TermUnitCoolSizingIndex(ZonesCooledNum);
                                        state.dataSize->EvzByZoneCool(TermUnitSizingIndex) = state.dataSize->EvzByZoneCoolPrev(TermUnitSizingIndex);
                                    }
                                }
                            }
                        }

                        // heating
                        SysSizing(CurOverallSimDay, AirLoopNum).DesHeatVolFlow =
                            SysSizing(CurOverallSimDay, AirLoopNum).CoinHeatMassFlow / state.dataEnvrn->StdRhoAir;
                        if (SysSizing(CurOverallSimDay, AirLoopNum).DesHeatVolFlow > 0) {
                            OutAirFrac =
                                SysSizing(CurOverallSimDay, AirLoopNum).DesOutAirVolFlow / SysSizing(CurOverallSimDay, AirLoopNum).DesHeatVolFlow;
                        } else {
                            OutAirFrac = 0.0;
                        }
                        OutAirFrac = min(1.0, max(0.0, OutAirFrac));

                        // This is a bit of a cludge. If the design zone heating airflows were increased due to
                        // the MaxZoneOaFraction, then the SysSizing(AirLoopNum,CurOverallSimDay)%DesHeatVolFlow
                        // variable will be out of sync with the
                        if (FinalSysSizing(AirLoopNum).MaxZoneOAFraction > 0 && FinalSysSizing(AirLoopNum).HeatAirDesMethod == FromDDCalc) {
                            SysHtgPeakAirflow = 0.0;
                            if (NumZonesHeated > 0) {
                                for (int ZonesHeatedNum = 1; ZonesHeatedNum <= NumZonesHeated; ++ZonesHeatedNum) {
                                    int TermUnitSizingIndex = AirToZoneNodeInfo(AirLoopNum).TermUnitHeatSizingIndex(ZonesHeatedNum);
                                    SysHtgPeakAirflow += TermUnitFinalZoneSizing(TermUnitSizingIndex).DesHeatVolFlow;
                                }
                            } else {
                                for (int ZonesHeatedNum = 1; ZonesHeatedNum <= NumZonesCooled; ++ZonesHeatedNum) {
                                    int TermUnitSizingIndex = AirToZoneNodeInfo(AirLoopNum).TermUnitCoolSizingIndex(ZonesHeatedNum);
                                    SysHtgPeakAirflow += TermUnitFinalZoneSizing(TermUnitSizingIndex).DesHeatVolFlow;
                                }
                            }
                        } else {
                            SysHtgPeakAirflow = SysSizing(CurOverallSimDay, AirLoopNum).DesHeatVolFlow;
                        }

                        if (SysSizing(CurOverallSimDay, AirLoopNum).DesHeatVolFlow > 0) {
                            // SysSizing(AirLoopNum,CurOverallSimDay)%DesHeatVolFlow may be out of sync with
                            // FinalZoneSizing(CtrlZoneNum)%DesHeatVolFlow
                            state.dataSimAirServingZones->Xs = min(
                                1.0,
                                FinalSysSizing(AirLoopNum).SysUncOA / max(SysSizing(CurOverallSimDay, AirLoopNum).DesHeatVolFlow, SysHtgPeakAirflow));
                        } else {
                            state.dataSimAirServingZones->Xs = 0.0;
                        }

                        if (FinalSysSizing(AirLoopNum).OAAutoSized && SysSizing(CurOverallSimDay, AirLoopNum).DesHeatVolFlow > 0) {
                            int numZonesHeated = AirToZoneNodeInfo(AirLoopNum).NumZonesHeated;
                            state.dataSimAirServingZones->MinHeatingEvz = 1.0;
                            state.dataSize->VozSumHtgBySys(AirLoopNum) = 0.0;
                            if (numZonesHeated > 0) {
                                for (int ZonesHeatedNum = 1; ZonesHeatedNum <= numZonesHeated; ++ZonesHeatedNum) {
                                    int TermUnitSizingIndex = AirToZoneNodeInfo(AirLoopNum).TermUnitHeatSizingIndex(ZonesHeatedNum);
                                    MatchingCooledZoneNum =
                                        FindNumberInList(TermUnitSizingIndex, AirToZoneNodeInfo(AirLoopNum).TermUnitCoolSizingIndex, NumZonesCooled);
                                    if (MatchingCooledZoneNum == 0) {
                                        // Zone air secondary recirculation fraction
                                        state.dataSimAirServingZones->Er = TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneSecondaryRecirculation;
                                        state.dataSimAirServingZones->Ep = TermUnitFinalZoneSizing(TermUnitSizingIndex).ZonePrimaryAirFractionHtg;
                                        state.dataSimAirServingZones->ZoneOAFrac = TermUnitFinalZoneSizing(TermUnitSizingIndex).ZpzHtgByZone;
                                        state.dataSimAirServingZones->ZoneEz = TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneADEffHeating;
                                        if (FinalSysSizing(AirLoopNum).SystemOAMethod == SOAM_SP) { // 62.1 simplified procedure
                                            if (state.dataSize->DBySys(AirLoopNum) < 0.60) {
                                                state.dataSize->EvzByZoneHeat(TermUnitSizingIndex) = 0.88 * state.dataSize->DBySys(AirLoopNum) + 0.22;
                                            } else {
                                                state.dataSize->EvzByZoneHeat(TermUnitSizingIndex) = 0.75;
                                            }
                                            state.dataSimAirServingZones->MinHeatingEvz = state.dataSize->EvzByZoneHeat(TermUnitSizingIndex);
                                        } else {
                                            if (state.dataSimAirServingZones->Er > 0.0) {
                                                // multi-path ventilation system using VRP
                                                state.dataSimAirServingZones->Fa =
                                                    state.dataSimAirServingZones->Ep +
                                                    (1.0 - state.dataSimAirServingZones->Ep) * state.dataSimAirServingZones->Er;
                                                state.dataSimAirServingZones->Fb = state.dataSimAirServingZones->Ep;
                                                state.dataSimAirServingZones->Fc = 1.0 - (1.0 - state.dataSimAirServingZones->ZoneEz) *
                                                                                             (1.0 - state.dataSimAirServingZones->Er) *
                                                                                             (1.0 - state.dataSimAirServingZones->Ep);
                                                // save Fa Fb and Fc for standard 62.1 report
                                                state.dataSize->FaByZoneHeat(TermUnitSizingIndex) = state.dataSimAirServingZones->Fa;
                                                state.dataSize->FbByZoneHeat(TermUnitSizingIndex) = state.dataSimAirServingZones->Fb;
                                                state.dataSize->FcByZoneHeat(TermUnitSizingIndex) = state.dataSimAirServingZones->Fc;

                                                // Calc zone ventilation efficiency
                                                if (state.dataSimAirServingZones->Fa > 0.0) {
                                                    SysHeatingEv = 1.0 +
                                                                   state.dataSimAirServingZones->Xs * state.dataSimAirServingZones->Fb /
                                                                       state.dataSimAirServingZones->Fa -
                                                                   state.dataSimAirServingZones->ZoneOAFrac * state.dataSimAirServingZones->Ep *
                                                                       state.dataSimAirServingZones->Fc / state.dataSimAirServingZones->Fa;
                                                } else {
                                                    SysHeatingEv = 1.0;
                                                }
                                            } else {
                                                // single-path ventilation system
                                                SysHeatingEv = 1.0 + state.dataSimAirServingZones->Xs - state.dataSimAirServingZones->ZoneOAFrac;
                                            }
                                            if (SysHeatingEv < state.dataSimAirServingZones->MinHeatingEvz)
                                                state.dataSimAirServingZones->MinHeatingEvz = SysHeatingEv;
                                            state.dataSize->EvzByZoneHeatPrev(TermUnitSizingIndex) =
                                                state.dataSize->EvzByZoneHeat(TermUnitSizingIndex); // Save previous EvzByZoneHeat
                                            state.dataSize->EvzByZoneHeat(TermUnitSizingIndex) = SysHeatingEv;
                                        }
                                        state.dataSize->VozSumHtgBySys(AirLoopNum) += TermUnitFinalZoneSizing(TermUnitSizingIndex).VozHtgByZone;
                                    }
                                }
                            } else {
                                for (int ZonesHeatedNum = 1; ZonesHeatedNum <= NumZonesCooled; ++ZonesHeatedNum) {
                                    int TermUnitSizingIndex = AirToZoneNodeInfo(AirLoopNum).TermUnitCoolSizingIndex(ZonesHeatedNum);
                                    // Zone air secondary recirculation fraction
                                    state.dataSimAirServingZones->Er = TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneSecondaryRecirculation;
                                    state.dataSimAirServingZones->Ep = TermUnitFinalZoneSizing(TermUnitSizingIndex).ZonePrimaryAirFractionHtg;
                                    state.dataSimAirServingZones->ZoneOAFrac = TermUnitFinalZoneSizing(TermUnitSizingIndex).ZpzHtgByZone;
                                    state.dataSimAirServingZones->ZoneEz = TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneADEffHeating;
                                    if (FinalSysSizing(AirLoopNum).SystemOAMethod == SOAM_SP) { // 62.1 simplified procedure
                                        if (state.dataSize->DBySys(AirLoopNum) < 0.60) {
                                            state.dataSize->EvzByZoneHeat(TermUnitSizingIndex) = 0.88 * state.dataSize->DBySys(AirLoopNum) + 0.22;
                                        } else {
                                            state.dataSize->EvzByZoneHeat(TermUnitSizingIndex) = 0.75;
                                        }
                                        state.dataSimAirServingZones->MinHeatingEvz = state.dataSize->EvzByZoneHeat(TermUnitSizingIndex);
                                    } else {
                                        if (state.dataSimAirServingZones->Er > 0.0) {
                                            // multi-path ventilation system using VRP
                                            state.dataSimAirServingZones->Fa =
                                                state.dataSimAirServingZones->Ep +
                                                (1.0 - state.dataSimAirServingZones->Ep) * state.dataSimAirServingZones->Er;
                                            state.dataSimAirServingZones->Fb = state.dataSimAirServingZones->Ep;
                                            state.dataSimAirServingZones->Fc = 1.0 - (1.0 - state.dataSimAirServingZones->ZoneEz) *
                                                                                         (1.0 - state.dataSimAirServingZones->Er) *
                                                                                         (1.0 - state.dataSimAirServingZones->Ep);
                                            // save Fa Fb and Fc for standard 62.1 report
                                            state.dataSize->FaByZoneHeat(TermUnitSizingIndex) = state.dataSimAirServingZones->Fa;
                                            state.dataSize->FbByZoneHeat(TermUnitSizingIndex) = state.dataSimAirServingZones->Fb;
                                            state.dataSize->FcByZoneHeat(TermUnitSizingIndex) = state.dataSimAirServingZones->Fc;

                                            // Calc zone ventilation efficiency
                                            if (state.dataSimAirServingZones->Fa > 0.0) {
                                                SysHeatingEv = 1.0 +
                                                               state.dataSimAirServingZones->Xs * state.dataSimAirServingZones->Fb /
                                                                   state.dataSimAirServingZones->Fa -
                                                               state.dataSimAirServingZones->ZoneOAFrac * state.dataSimAirServingZones->Ep *
                                                                   state.dataSimAirServingZones->Fc / state.dataSimAirServingZones->Fa;
                                            } else {
                                                SysHeatingEv = 1.0;
                                            }
                                        } else {
                                            // single-path ventilation system
                                            SysHeatingEv = 1.0 + state.dataSimAirServingZones->Xs - state.dataSimAirServingZones->ZoneOAFrac;
                                        }
                                        if (SysHeatingEv < state.dataSimAirServingZones->MinHeatingEvz)
                                            state.dataSimAirServingZones->MinHeatingEvz = SysHeatingEv;
                                        state.dataSize->EvzByZoneHeatPrev(TermUnitSizingIndex) =
                                            state.dataSize->EvzByZoneHeat(TermUnitSizingIndex); // Save previous EvzByZoneHeat
                                        state.dataSize->EvzByZoneHeat(TermUnitSizingIndex) = SysHeatingEv;
                                        state.dataSize->VozSumHtgBySys(AirLoopNum) += TermUnitFinalZoneSizing(TermUnitSizingIndex).VozHtgByZone;
                                    }
                                }
                            }

                            if (state.dataSimAirServingZones->MinHeatingEvz > 0) {
                                // Std 62.1-2010, section 6.2.5.4: Eq. 6.6
                                // (However, I don't think people diversity can be done correctly in E+ Sizing so assuming D=1 in this
                                // equation
                                // Vou = Diversity*(Rp*Pz) + Ra*Az
                                state.dataSimAirServingZones->Vou = FinalSysSizing(AirLoopNum).SysUncOA;
                                state.dataSimAirServingZones->Vot = state.dataSimAirServingZones->Vou / state.dataSimAirServingZones->MinHeatingEvz;
                                if (state.dataSimAirServingZones->Vot > state.dataSize->VotHtgBySys(AirLoopNum)) {
                                    // This might be the cooling design day so only update if Vot is larger than the previous
                                    state.dataSize->VotHtgBySys(AirLoopNum) = state.dataSimAirServingZones->Vot;
                                    state.dataSize->XsBySysHeat(AirLoopNum) = state.dataSimAirServingZones->Xs;
                                    state.dataSize->EvzMinBySysHeat(AirLoopNum) = state.dataSimAirServingZones->MinHeatingEvz;
                                } else {
                                    // Restore EvzByZoneHeat() since it was reset by the current (but not highest Vot) design day
                                    // This kludge is probably because inside EndDay block and code gets called for each design day.
                                    if (numZonesHeated > 0) {
                                        for (int ZonesHeatedNum = 1; ZonesHeatedNum <= numZonesHeated; ++ZonesHeatedNum) {
                                            int TermUnitSizingIndex = AirToZoneNodeInfo(AirLoopNum).TermUnitHeatSizingIndex(ZonesHeatedNum);
                                            state.dataSize->EvzByZoneHeat(TermUnitSizingIndex) =
                                                state.dataSize->EvzByZoneHeatPrev(TermUnitSizingIndex);
                                        }
                                    } else {
                                        for (int ZonesHeatedNum = 1; ZonesHeatedNum <= NumZonesCooled; ++ZonesHeatedNum) {
                                            int TermUnitSizingIndex = AirToZoneNodeInfo(AirLoopNum).TermUnitCoolSizingIndex(ZonesHeatedNum);
                                            state.dataSize->EvzByZoneHeat(TermUnitSizingIndex) =
                                                state.dataSize->EvzByZoneHeatPrev(TermUnitSizingIndex);
                                        }
                                    }
                                }
                            }
                        }
                    } else { // error
                    }
                    SysSizing(CurOverallSimDay, AirLoopNum).DesMainVolFlow =
                        max(SysSizing(CurOverallSimDay, AirLoopNum).DesCoolVolFlow, SysSizing(CurOverallSimDay, AirLoopNum).DesHeatVolFlow);
                    // this should also be as least as big as is needed for Vot
                } else if (SELECT_CASE_var1 == NonCoincident) {
                    if (FinalSysSizing(AirLoopNum).SystemOAMethod == SOAM_ZoneSum) {
                        SysSizing(CurOverallSimDay, AirLoopNum).DesCoolVolFlow =
                            SysSizing(CurOverallSimDay, AirLoopNum).NonCoinCoolMassFlow / state.dataEnvrn->StdRhoAir;
                        SysSizing(CurOverallSimDay, AirLoopNum).DesHeatVolFlow =
                            SysSizing(CurOverallSimDay, AirLoopNum).NonCoinHeatMassFlow / state.dataEnvrn->StdRhoAir;
                        state.dataSize->VotClgBySys(AirLoopNum) = FinalSysSizing(AirLoopNum).SysUncOA;
                        state.dataSize->VotHtgBySys(AirLoopNum) = FinalSysSizing(AirLoopNum).SysUncOA;
                        for (int ZonesCooledNum = 1; ZonesCooledNum <= NumZonesCooled; ++ZonesCooledNum) {
                            int TermUnitSizingIndex = AirToZoneNodeInfo(AirLoopNum).TermUnitCoolSizingIndex(ZonesCooledNum);
                            if (TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneADEffCooling < state.dataSize->EvzMinBySysCool(AirLoopNum))
                                state.dataSize->EvzMinBySysCool(AirLoopNum) = TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneADEffCooling;
                            if (TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneADEffHeating < state.dataSize->EvzMinBySysHeat(AirLoopNum))
                                state.dataSize->EvzMinBySysHeat(AirLoopNum) = TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneADEffHeating;
                        }
                        for (int ZonesHeatedNum = 1; ZonesHeatedNum <= NumZonesHeated; ++ZonesHeatedNum) {
                            int TermUnitSizingIndex = AirToZoneNodeInfo(AirLoopNum).TermUnitHeatSizingIndex(ZonesHeatedNum);
                            if (TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneADEffCooling < state.dataSize->EvzMinBySysCool(AirLoopNum))
                                state.dataSize->EvzMinBySysCool(AirLoopNum) = TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneADEffCooling;
                            if (TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneADEffHeating < state.dataSize->EvzMinBySysHeat(AirLoopNum))
                                state.dataSize->EvzMinBySysHeat(AirLoopNum) = TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneADEffHeating;
                        }
                        if (SysSizing(CurOverallSimDay, AirLoopNum).DesCoolVolFlow > 0) {
                            state.dataSize->XsBySysCool(AirLoopNum) =
                                min(1.0, FinalSysSizing(AirLoopNum).SysUncOA / SysSizing(CurOverallSimDay, AirLoopNum).DesCoolVolFlow);
                        } else {
                            state.dataSize->XsBySysCool(AirLoopNum) = 0.0;
                        }
                        if (SysSizing(CurOverallSimDay, AirLoopNum).DesHeatVolFlow > 0) {
                            state.dataSize->XsBySysHeat(AirLoopNum) =
                                min(1.0, FinalSysSizing(AirLoopNum).SysUncOA / SysSizing(CurOverallSimDay, AirLoopNum).DesHeatVolFlow);
                        } else {
                            state.dataSize->XsBySysHeat(AirLoopNum) = 0.0;
                        }
                    } else if (FinalSysSizing(AirLoopNum).SystemOAMethod == SOAM_VRP ||
                               FinalSysSizing(AirLoopNum).SystemOAMethod == SOAM_SP) { // Ventilation Rate and Simplified Procedure
                        // cooling
                        SysSizing(CurOverallSimDay, AirLoopNum).DesCoolVolFlow =
                            SysSizing(CurOverallSimDay, AirLoopNum).NonCoinCoolMassFlow / state.dataEnvrn->StdRhoAir;
                        if (SysSizing(CurOverallSimDay, AirLoopNum).DesCoolVolFlow > 0) {
                            OutAirFrac =
                                SysSizing(CurOverallSimDay, AirLoopNum).DesOutAirVolFlow / SysSizing(CurOverallSimDay, AirLoopNum).DesCoolVolFlow;
                        } else {
                            OutAirFrac = 0.0;
                        }
                        OutAirFrac = min(1.0, max(0.0, OutAirFrac));

                        if (SysSizing(CurOverallSimDay, AirLoopNum).DesCoolVolFlow > 0) {
                            state.dataSimAirServingZones->Xs =
                                min(1.0, FinalSysSizing(AirLoopNum).SysUncOA / SysSizing(CurOverallSimDay, AirLoopNum).DesCoolVolFlow);
                        } else {
                            state.dataSimAirServingZones->Xs = 0.0;
                        }
                        if (FinalSysSizing(AirLoopNum).OAAutoSized && SysSizing(CurOverallSimDay, AirLoopNum).DesCoolVolFlow > 0) {
                            int numZonesCooled = AirToZoneNodeInfo(AirLoopNum).NumZonesCooled;
                            state.dataSimAirServingZones->MinCoolingEvz = 1.0;
                            state.dataSize->VozSumClgBySys(AirLoopNum) = 0.0;
                            for (int ZonesCooledNum = 1; ZonesCooledNum <= numZonesCooled; ++ZonesCooledNum) {
                                int TermUnitSizingIndex = AirToZoneNodeInfo(AirLoopNum).TermUnitCoolSizingIndex(ZonesCooledNum);

                                // Zone air secondary recirculation fraction
                                state.dataSimAirServingZones->Er = TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneSecondaryRecirculation;
                                state.dataSimAirServingZones->Ep = TermUnitFinalZoneSizing(TermUnitSizingIndex).ZonePrimaryAirFraction;
                                state.dataSimAirServingZones->ZoneOAFrac = TermUnitFinalZoneSizing(TermUnitSizingIndex).ZpzClgByZone;
                                state.dataSimAirServingZones->ZoneEz = TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneADEffCooling;
                                VozClg = TermUnitFinalZoneSizing(TermUnitSizingIndex).VozClgByZone;
                                if (FinalSysSizing(AirLoopNum).SystemOAMethod == SOAM_SP) { // 62.1 simplified procedure
                                    if (state.dataSize->DBySys(AirLoopNum) < 0.60) {
                                        state.dataSize->EvzByZoneCool(TermUnitSizingIndex) = 0.88 * state.dataSize->DBySys(AirLoopNum) + 0.22;
                                    } else {
                                        state.dataSize->EvzByZoneCool(TermUnitSizingIndex) = 0.75;
                                    }
                                    state.dataSimAirServingZones->MinCoolingEvz = state.dataSize->EvzByZoneCool(TermUnitSizingIndex);
                                } else {
                                    if (state.dataSimAirServingZones->Er > 0.0) {
                                        // multi-path ventilation system using VRP
                                        state.dataSimAirServingZones->Fa =
                                            state.dataSimAirServingZones->Ep +
                                            (1.0 - state.dataSimAirServingZones->Ep) * state.dataSimAirServingZones->Er;
                                        state.dataSimAirServingZones->Fb = state.dataSimAirServingZones->Ep;
                                        state.dataSimAirServingZones->Fc = 1.0 - (1.0 - state.dataSimAirServingZones->ZoneEz) *
                                                                                     (1.0 - state.dataSimAirServingZones->Er) *
                                                                                     (1.0 - state.dataSimAirServingZones->Ep);
                                        // save Fa Fb and Fc for standard 62.1 report
                                        state.dataSize->FaByZoneCool(TermUnitSizingIndex) = state.dataSimAirServingZones->Fa;
                                        state.dataSize->FbByZoneCool(TermUnitSizingIndex) = state.dataSimAirServingZones->Fb;
                                        state.dataSize->FcByZoneCool(TermUnitSizingIndex) = state.dataSimAirServingZones->Fc;

                                        // Calc zone ventilation efficiency
                                        if (state.dataSimAirServingZones->Fa > 0.0) {
                                            SysCoolingEv = 1.0 +
                                                           state.dataSimAirServingZones->Xs * state.dataSimAirServingZones->Fb /
                                                               state.dataSimAirServingZones->Fa -
                                                           state.dataSimAirServingZones->ZoneOAFrac * state.dataSimAirServingZones->Ep *
                                                               state.dataSimAirServingZones->Fc / state.dataSimAirServingZones->Fa;
                                        } else {
                                            SysCoolingEv = 1.0;
                                        }
                                    } else {
                                        // single-path ventilation system
                                        SysCoolingEv = 1.0 + state.dataSimAirServingZones->Xs - state.dataSimAirServingZones->ZoneOAFrac;
                                        // Apply ventilation efficiency limit; reset SysCoolingEv if necessary
                                        LimitZoneVentEff(state, state.dataSimAirServingZones->Xs, VozClg, TermUnitSizingIndex, SysCoolingEv);
                                    }
                                    if (SysCoolingEv < state.dataSimAirServingZones->MinCoolingEvz)
                                        state.dataSimAirServingZones->MinCoolingEvz = SysCoolingEv;
                                    state.dataSize->EvzByZoneCoolPrev(TermUnitSizingIndex) = state.dataSize->EvzByZoneCool(TermUnitSizingIndex);
                                    state.dataSize->EvzByZoneCool(TermUnitSizingIndex) = SysCoolingEv;
                                    state.dataSize->VozSumClgBySys(AirLoopNum) += TermUnitFinalZoneSizing(TermUnitSizingIndex).VozClgByZone;
                                }
                                state.dataSize->VozSumClgBySys(AirLoopNum) += TermUnitFinalZoneSizing(TermUnitSizingIndex).VozClgByZone;
                            }

                            if (state.dataSimAirServingZones->MinCoolingEvz > 0) {
                                // Std 62.1-2010, section 6.2.5.4: Eq. 6.6
                                // (However, I don't think people diversity can be done correctly in E+ Sizing so assuming D=1 in this
                                // equation
                                // Vou = Diversity*(Rp*Pz) + Ra*Az
                                state.dataSimAirServingZones->Vou = FinalSysSizing(AirLoopNum).SysUncOA;
                                state.dataSimAirServingZones->Vot = state.dataSimAirServingZones->Vou / state.dataSimAirServingZones->MinCoolingEvz;
                                if (state.dataSimAirServingZones->Vot > state.dataSize->VotClgBySys(AirLoopNum)) {
                                    // This might be the cooling design day so only update if Vot is larger than the previous
                                    state.dataSize->VotClgBySys(AirLoopNum) = state.dataSimAirServingZones->Vot;
                                    state.dataSize->XsBySysCool(AirLoopNum) = state.dataSimAirServingZones->Xs;
                                    state.dataSize->EvzMinBySysCool(AirLoopNum) = state.dataSimAirServingZones->MinCoolingEvz;
                                } else {
                                    // Restore EvzByZoneCool() since it was reset by the current (but not highest Vot) design day
                                    for (int ZonesCooledNum = 1; ZonesCooledNum <= numZonesCooled; ++ZonesCooledNum) {
                                        int TermUnitSizingIndex = AirToZoneNodeInfo(AirLoopNum).TermUnitCoolSizingIndex(ZonesCooledNum);
                                        state.dataSize->EvzByZoneCool(TermUnitSizingIndex) = state.dataSize->EvzByZoneCoolPrev(TermUnitSizingIndex);
                                    }
                                }
                            }
                        }

                        // heating
                        SysSizing(CurOverallSimDay, AirLoopNum).DesHeatVolFlow =
                            SysSizing(CurOverallSimDay, AirLoopNum).NonCoinHeatMassFlow / state.dataEnvrn->StdRhoAir;
                        if (SysSizing(CurOverallSimDay, AirLoopNum).DesHeatVolFlow > 0) {
                            OutAirFrac =
                                SysSizing(CurOverallSimDay, AirLoopNum).DesOutAirVolFlow / SysSizing(CurOverallSimDay, AirLoopNum).DesHeatVolFlow;
                        } else {
                            OutAirFrac = 0.0;
                        }
                        OutAirFrac = min(1.0, max(0.0, OutAirFrac));

                        if (SysSizing(CurOverallSimDay, AirLoopNum).DesHeatVolFlow > 0) {
                            state.dataSimAirServingZones->Xs =
                                min(1.0, FinalSysSizing(AirLoopNum).SysUncOA / SysSizing(CurOverallSimDay, AirLoopNum).DesHeatVolFlow);
                        } else {
                            state.dataSimAirServingZones->Xs = 0.0;
                        }
                        if (FinalSysSizing(AirLoopNum).OAAutoSized && SysSizing(CurOverallSimDay, AirLoopNum).DesHeatVolFlow > 0) {
                            int numZonesHeated = AirToZoneNodeInfo(AirLoopNum).NumZonesHeated;
                            state.dataSimAirServingZones->MinHeatingEvz = 1.0;
                            state.dataSize->VozSumHtgBySys(AirLoopNum) = 0.0;
                            if (numZonesHeated > 0) {
                                for (int ZonesHeatedNum = 1; ZonesHeatedNum <= numZonesHeated; ++ZonesHeatedNum) {
                                    int TermUnitSizingIndex = AirToZoneNodeInfo(AirLoopNum).TermUnitHeatSizingIndex(ZonesHeatedNum);
                                    MatchingCooledZoneNum =
                                        FindNumberInList(TermUnitSizingIndex, AirToZoneNodeInfo(AirLoopNum).TermUnitCoolSizingIndex, NumZonesCooled);
                                    if (MatchingCooledZoneNum == 0) {
                                        // Zone air secondary recirculation fraction
                                        state.dataSimAirServingZones->Er = TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneSecondaryRecirculation;
                                        state.dataSimAirServingZones->Ep = TermUnitFinalZoneSizing(TermUnitSizingIndex).ZonePrimaryAirFractionHtg;
                                        state.dataSimAirServingZones->ZoneOAFrac = TermUnitFinalZoneSizing(TermUnitSizingIndex).ZpzHtgByZone;
                                        state.dataSimAirServingZones->ZoneEz = TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneADEffHeating;
                                        if (FinalSysSizing(AirLoopNum).SystemOAMethod == SOAM_SP) { // 62.1 simplified procedure
                                            if (state.dataSize->DBySys(AirLoopNum) < 0.60) {
                                                state.dataSize->EvzByZoneHeat(TermUnitSizingIndex) = 0.88 * state.dataSize->DBySys(AirLoopNum) + 0.22;
                                            } else {
                                                state.dataSize->EvzByZoneHeat(TermUnitSizingIndex) = 0.75;
                                            }
                                            state.dataSimAirServingZones->MinHeatingEvz = state.dataSize->EvzByZoneHeat(TermUnitSizingIndex);
                                        } else {
                                            if (state.dataSimAirServingZones->Er > 0.0) {
                                                // multi-path ventilation system using VRP
                                                state.dataSimAirServingZones->Fa =
                                                    state.dataSimAirServingZones->Ep +
                                                    (1.0 - state.dataSimAirServingZones->Ep) * state.dataSimAirServingZones->Er;
                                                state.dataSimAirServingZones->Fb = state.dataSimAirServingZones->Ep;
                                                state.dataSimAirServingZones->Fc = 1.0 - (1.0 - state.dataSimAirServingZones->ZoneEz) *
                                                                                             (1.0 - state.dataSimAirServingZones->Er) *
                                                                                             (1.0 - state.dataSimAirServingZones->Ep);

                                                // Calc zone ventilation efficiency
                                                if (state.dataSimAirServingZones->Fa > 0.0) {
                                                    SysHeatingEv = 1.0 +
                                                                   state.dataSimAirServingZones->Xs * state.dataSimAirServingZones->Fb /
                                                                       state.dataSimAirServingZones->Fa -
                                                                   state.dataSimAirServingZones->ZoneOAFrac * state.dataSimAirServingZones->Ep *
                                                                       state.dataSimAirServingZones->Fc / state.dataSimAirServingZones->Fa;
                                                } else {
                                                    SysHeatingEv = 1.0;
                                                }
                                            } else {
                                                // single-path ventilation system
                                                SysHeatingEv = 1.0 + state.dataSimAirServingZones->Xs - state.dataSimAirServingZones->ZoneOAFrac;
                                            }
                                        }
                                    }
                                    if (FinalSysSizing(AirLoopNum).SystemOAMethod == SOAM_SP) { // 62.1 simplified procedure
                                        state.dataSize->VozSumHtgBySys(AirLoopNum) += TermUnitFinalZoneSizing(TermUnitSizingIndex).VozHtgByZone;
                                    } else {
                                        if (SysHeatingEv < state.dataSimAirServingZones->MinHeatingEvz)
                                            state.dataSimAirServingZones->MinHeatingEvz = SysHeatingEv;
                                        state.dataSize->EvzByZoneHeatPrev(TermUnitSizingIndex) =
                                            state.dataSize->EvzByZoneHeat(TermUnitSizingIndex); // Save previous EvzByZoneHeat
                                        state.dataSize->EvzByZoneHeat(TermUnitSizingIndex) = SysHeatingEv;
                                        state.dataSize->VozSumHtgBySys(AirLoopNum) += TermUnitFinalZoneSizing(TermUnitSizingIndex).VozHtgByZone;
                                    }
                                }
                            } else {
                                int numZonesCooled = AirToZoneNodeInfo(AirLoopNum).NumZonesCooled;
                                for (int ZonesHeatedNum = 1; ZonesHeatedNum <= numZonesCooled; ++ZonesHeatedNum) {
                                    int TermUnitSizingIndex = AirToZoneNodeInfo(AirLoopNum).TermUnitCoolSizingIndex(ZonesHeatedNum);
                                    // Zone air secondary recirculation fraction
                                    state.dataSimAirServingZones->Er = TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneSecondaryRecirculation;
                                    state.dataSimAirServingZones->Ep = TermUnitFinalZoneSizing(TermUnitSizingIndex).ZonePrimaryAirFractionHtg;
                                    state.dataSimAirServingZones->ZoneOAFrac = TermUnitFinalZoneSizing(TermUnitSizingIndex).ZpzHtgByZone;
                                    state.dataSimAirServingZones->ZoneEz = TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneADEffHeating;
                                    if (FinalSysSizing(AirLoopNum).SystemOAMethod == SOAM_SP) { // 62.1 simplified procedure
                                        if (state.dataSize->DBySys(AirLoopNum) < 0.60) {
                                            state.dataSize->EvzByZoneCool(TermUnitSizingIndex) = 0.88 * state.dataSize->DBySys(AirLoopNum) + 0.22;
                                        } else {
                                            state.dataSize->EvzByZoneCool(TermUnitSizingIndex) = 0.75;
                                        }
                                        state.dataSimAirServingZones->MinCoolingEvz = state.dataSize->EvzByZoneCool(TermUnitSizingIndex);
                                    } else {
                                        if (state.dataSimAirServingZones->Er > 0.0) {
                                            // multi-path ventilation system using VRP
                                            state.dataSimAirServingZones->Fa =
                                                state.dataSimAirServingZones->Ep +
                                                (1.0 - state.dataSimAirServingZones->Ep) * state.dataSimAirServingZones->Er;
                                            state.dataSimAirServingZones->Fb = state.dataSimAirServingZones->Ep;
                                            state.dataSimAirServingZones->Fc = 1.0 - (1.0 - state.dataSimAirServingZones->ZoneEz) *
                                                                                         (1.0 - state.dataSimAirServingZones->Er) *
                                                                                         (1.0 - state.dataSimAirServingZones->Ep);

                                            // Calc zone ventilation efficiency
                                            if (state.dataSimAirServingZones->Fa > 0.0) {
                                                SysHeatingEv = 1.0 +
                                                               state.dataSimAirServingZones->Xs * state.dataSimAirServingZones->Fb /
                                                                   state.dataSimAirServingZones->Fa -
                                                               state.dataSimAirServingZones->ZoneOAFrac * state.dataSimAirServingZones->Ep *
                                                                   state.dataSimAirServingZones->Fc / state.dataSimAirServingZones->Fa;
                                            } else {
                                                SysHeatingEv = 1.0;
                                            }
                                        } else {
                                            // single-path ventilation system
                                            SysHeatingEv = 1.0 + state.dataSimAirServingZones->Xs - state.dataSimAirServingZones->ZoneOAFrac;
                                        }
                                        if (SysHeatingEv < state.dataSimAirServingZones->MinHeatingEvz)
                                            state.dataSimAirServingZones->MinHeatingEvz = SysHeatingEv;
                                        state.dataSize->EvzByZoneHeatPrev(TermUnitSizingIndex) =
                                            state.dataSize->EvzByZoneHeat(TermUnitSizingIndex); // Save previous EvzByZoneHeat
                                        state.dataSize->EvzByZoneHeat(TermUnitSizingIndex) = SysHeatingEv;
                                    }
                                    state.dataSize->VozSumHtgBySys(AirLoopNum) += TermUnitFinalZoneSizing(TermUnitSizingIndex).VozHtgByZone;
                                }
                            }

                            if (state.dataSimAirServingZones->MinHeatingEvz > 0) {
                                // Std 62.1-2010, section 6.2.5.4: Eq. 6.6
                                // (However, I don't think people diversity can be done correctly in E+ Sizing so assuming D=1 in this
                                // equation
                                // Vou = Diversity*(Rp*Pz) + Ra*Az
                                state.dataSimAirServingZones->Vou = FinalSysSizing(AirLoopNum).SysUncOA;
                                state.dataSimAirServingZones->Vot = state.dataSimAirServingZones->Vou / state.dataSimAirServingZones->MinHeatingEvz;
                                if (state.dataSimAirServingZones->Vot > state.dataSize->VotHtgBySys(AirLoopNum)) {
                                    // This might be the cooling design day so only update if Vot is larger than the previous
                                    state.dataSize->VotHtgBySys(AirLoopNum) = state.dataSimAirServingZones->Vot;
                                    state.dataSize->XsBySysHeat(AirLoopNum) = state.dataSimAirServingZones->Xs;
                                    state.dataSize->EvzMinBySysHeat(AirLoopNum) = state.dataSimAirServingZones->MinHeatingEvz;
                                } else {
                                    // Restore EvzByZoneHeat() since it was just reset by the current (but not highest Vot) design day
                                    // This kludge is probably because inside EndDay block and code gets called for each design day.
                                    if (numZonesHeated > 0) {
                                        for (int ZonesHeatedNum = 1; ZonesHeatedNum <= numZonesHeated; ++ZonesHeatedNum) {
                                            int TermUnitSizingIndex = AirToZoneNodeInfo(AirLoopNum).TermUnitHeatSizingIndex(ZonesHeatedNum);
                                            state.dataSize->EvzByZoneHeat(TermUnitSizingIndex) =
                                                state.dataSize->EvzByZoneHeatPrev(TermUnitSizingIndex);
                                        }
                                    } else {
                                        for (int ZonesHeatedNum = 1; ZonesHeatedNum <= NumZonesCooled; ++ZonesHeatedNum) {
                                            int TermUnitSizingIndex = AirToZoneNodeInfo(AirLoopNum).TermUnitCoolSizingIndex(ZonesHeatedNum);
                                            state.dataSize->EvzByZoneHeat(TermUnitSizingIndex) =
                                                state.dataSize->EvzByZoneHeatPrev(TermUnitSizingIndex);
                                        }
                                    }
                                }
                            }
                        }
                    } else { // error
                    }

                    SysSizing(CurOverallSimDay, AirLoopNum).DesMainVolFlow =
                        max(SysSizing(CurOverallSimDay, AirLoopNum).DesCoolVolFlow, SysSizing(CurOverallSimDay, AirLoopNum).DesHeatVolFlow);
                    // this should also be as least as big as is needed for Vot
                }
            }

            // If the ventilation was autosized using the ASHRAE VRP method, then the design zone and system ventilation values
            // must be based on the larger of the cooling or heating OA
            if (FinalSysSizing(AirLoopNum).OAAutoSized &&
                (FinalSysSizing(AirLoopNum).SystemOAMethod == SOAM_VRP || FinalSysSizing(AirLoopNum).SystemOAMethod == SOAM_SP)) {
                Real64 VotMax = max(state.dataSize->VotClgBySys(AirLoopNum), state.dataSize->VotHtgBySys(AirLoopNum));

                // Reset the system level ventilation to the larger of the system-level cooling or heating Vot
                FinalSysSizing(AirLoopNum).DesOutAirVolFlow = VotMax;
                CalcSysSizing(AirLoopNum).DesOutAirVolFlow = VotMax;

                // Reset the zone level ventilation to the larger of the zone-level cooling or heating Voz
                // Loop through cooled zones and heated zones - ok if there's overlap
                for (int zoneNum = 1; zoneNum <= NumZonesCooled; ++zoneNum) {
                    int TermUnitSizingIndex = AirToZoneNodeInfo(AirLoopNum).TermUnitCoolSizingIndex(zoneNum);
                    Real64 VozMax =
                        max(TermUnitFinalZoneSizing(TermUnitSizingIndex).VozClgByZone, TermUnitFinalZoneSizing(TermUnitSizingIndex).VozHtgByZone);
                    TermUnitFinalZoneSizing(TermUnitSizingIndex).MinOA = VozMax;
                }
                for (int zoneNum = 1; zoneNum <= NumZonesHeated; ++zoneNum) {
                    int TermUnitSizingIndex = AirToZoneNodeInfo(AirLoopNum).TermUnitHeatSizingIndex(zoneNum);
                    Real64 VozMax =
                        max(TermUnitFinalZoneSizing(TermUnitSizingIndex).VozClgByZone, TermUnitFinalZoneSizing(TermUnitSizingIndex).VozHtgByZone);
                    TermUnitFinalZoneSizing(TermUnitSizingIndex).MinOA = VozMax;
                }
            }
        }
    } break;
    case DataGlobalConstants::CallIndicator::EndSysSizingCalc: {
        // Correct the zone return temperature in FinalZoneSizing for the case of induction units. The calc in
        // ZoneEquipmentManager assumes all the air entering the zone goes into the return node.
        for (int CtrlZoneNum = 1; CtrlZoneNum <= state.dataGlobal->NumOfZones; ++CtrlZoneNum) {
            if (!state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).IsControlled) continue;
            // Use first non-zero airdistunit for now, if there is one
            termunitsizingtempfrac = 1.0;
            int TermUnitSizingIndex = 0;
            for (int InletNode = 1; InletNode <= state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).NumInletNodes; ++InletNode) {
                TermUnitSizingIndex = state.dataZoneEquip->ZoneEquipConfig(CtrlZoneNum).AirDistUnitCool(InletNode).TermUnitSizingIndex;
                if (TermUnitSizingIndex == 0) continue;
                termunitsizingtemp = (1.0 + TermUnitSizing(TermUnitSizingIndex).InducRat);
                termunitsizingtempfrac = (1.0 / termunitsizingtemp);
                if (TermUnitSizingIndex > 0) break;
            }
            if (TermUnitSizingIndex == 0) continue; // Skip this if there are no terminal units
            RetTempRise =
                TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneRetTempAtCoolPeak - TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneTempAtCoolPeak;
            if (RetTempRise > 0.01) {
                // avoid possible compiler bug
                //          FinalZoneSizing(CtrlZoneNum)%ZoneRetTempAtCoolPeak = &
                //            FinalZoneSizing(CtrlZoneNum)%ZoneTempAtCoolPeak + RetTempRise * &
                //           (1.0d0/(1.0d0+TermUnitSizing(CtrlZoneNum)%InducRat))
                TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneRetTempAtCoolPeak =
                    TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneTempAtCoolPeak + RetTempRise * termunitsizingtempfrac;
            }
            RetTempRise =
                TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneRetTempAtHeatPeak - TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneTempAtHeatPeak;
            if (RetTempRise > 0.01) {
                // avoid possible compiler bug
                //          FinalZoneSizing(CtrlZoneNum)%ZoneRetTempAtHeatPeak = &
                //            FinalZoneSizing(CtrlZoneNum)%ZoneTempAtHeatPeak + RetTempRise * &
                //            (1.0d0/(1.0d0+TermUnitSizing(CtrlZoneNum)%InducRat))
                TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneRetTempAtHeatPeak =
                    TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneTempAtHeatPeak + RetTempRise * termunitsizingtempfrac;
            }
            for (TimeStepIndex = 1; TimeStepIndex <= numOfTimeStepInDay; ++TimeStepIndex) {
                RetTempRise = TermUnitFinalZoneSizing(TermUnitSizingIndex).CoolZoneRetTempSeq(TimeStepIndex) -
                              TermUnitFinalZoneSizing(TermUnitSizingIndex).CoolZoneTempSeq(TimeStepIndex);
                if (RetTempRise > 0.01) {
                    // avoid possible compiler bug
                    //            FinalZoneSizing(CtrlZoneNum)%CoolZoneRetTempSeq(TimeStepIndex) = &
                    //              FinalZoneSizing(CtrlZoneNum)%CoolZoneTempSeq(TimeStepIndex) + RetTempRise * &
                    //             (1.0d0/(1.0d0+TermUnitSizing(CtrlZoneNum)%InducRat))
                    TermUnitFinalZoneSizing(TermUnitSizingIndex).CoolZoneRetTempSeq(TimeStepIndex) =
                        TermUnitFinalZoneSizing(TermUnitSizingIndex).CoolZoneTempSeq(TimeStepIndex) + RetTempRise * termunitsizingtempfrac;
                }
                RetTempRise = TermUnitFinalZoneSizing(TermUnitSizingIndex).HeatZoneRetTempSeq(TimeStepIndex) -
                              TermUnitFinalZoneSizing(TermUnitSizingIndex).HeatZoneTempSeq(TimeStepIndex);
                if (RetTempRise > 0.01) {
                    // avoid possible compiler bug
                    //            FinalZoneSizing(CtrlZoneNum)%HeatZoneRetTempSeq(TimeStepIndex) = &
                    //              FinalZoneSizing(CtrlZoneNum)%HeatZoneTempSeq(TimeStepIndex) + RetTempRise * &
                    //             (1.0d0/(1.0d0+TermUnitSizing(CtrlZoneNum)%InducRat))
                    TermUnitFinalZoneSizing(TermUnitSizingIndex).HeatZoneRetTempSeq(TimeStepIndex) =
                        TermUnitFinalZoneSizing(TermUnitSizingIndex).HeatZoneTempSeq(TimeStepIndex) + RetTempRise * termunitsizingtempfrac;
                }
            }
        }

        // Get final design flows
        for (AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum) {
            state.dataSize->SensCoolCapTemp(AirLoopNum) = 0.0;
            state.dataSize->TotCoolCapTemp(AirLoopNum) = 0.0;

            // For coincident sizing, loop over design days and pick out the largest central heating amd
            // cooling flow rates and associated data

            for (DDNum = 1; DDNum <= state.dataEnvrn->TotDesDays + state.dataEnvrn->TotRunDesPersDays; ++DDNum) {

                if (SysSizing(DDNum, AirLoopNum).SensCoolCap > state.dataSize->SensCoolCapTemp(AirLoopNum)) {
                    SysSizPeakDDNum(AirLoopNum).SensCoolPeakDD = DDNum;
                    SysSizPeakDDNum(AirLoopNum).cSensCoolPeakDDDate = state.dataSize->DesDayWeath(DDNum).DateString;
                    state.dataSize->SensCoolCapTemp(AirLoopNum) = SysSizing(DDNum, AirLoopNum).SensCoolCap;
                    if (SysSizing(DDNum, AirLoopNum).CoolingPeakLoadType == SensibleCoolingLoad) {
                        CalcSysSizing(AirLoopNum).DesCoolVolFlow = SysSizing(DDNum, AirLoopNum).DesCoolVolFlow;
                        CalcSysSizing(AirLoopNum).CoolDesDay = SysSizing(DDNum, AirLoopNum).CoolDesDay;
                        // CalcSysSizing( AirLoopNum ).CoinCoolMassFlow = SysSizing( DDNum, AirLoopNum ).CoinCoolMassFlow;
                        CalcSysSizing(AirLoopNum).MassFlowAtCoolPeak = SysSizing(DDNum, AirLoopNum).MassFlowAtCoolPeak;
                        CalcSysSizing(AirLoopNum).SensCoolCap = SysSizing(DDNum, AirLoopNum).SensCoolCap;
                        CalcSysSizing(AirLoopNum).TotCoolCap = SysSizing(DDNum, AirLoopNum).TotCoolCap;
                        CalcSysSizing(AirLoopNum).CoolFlowSeq = SysSizing(DDNum, AirLoopNum).CoolFlowSeq;
                        CalcSysSizing(AirLoopNum).SumZoneCoolLoadSeq = SysSizing(DDNum, AirLoopNum).SumZoneCoolLoadSeq;
                        CalcSysSizing(AirLoopNum).CoolZoneAvgTempSeq = SysSizing(DDNum, AirLoopNum).CoolZoneAvgTempSeq;
                        CalcSysSizing(AirLoopNum).SensCoolCapSeq = SysSizing(DDNum, AirLoopNum).SensCoolCapSeq;
                        CalcSysSizing(AirLoopNum).TotCoolCapSeq = SysSizing(DDNum, AirLoopNum).TotCoolCapSeq;
                        CalcSysSizing(AirLoopNum).MixTempAtCoolPeak = SysSizing(DDNum, AirLoopNum).MixTempAtCoolPeak;
                        CalcSysSizing(AirLoopNum).RetTempAtCoolPeak = SysSizing(DDNum, AirLoopNum).RetTempAtCoolPeak;
                        CalcSysSizing(AirLoopNum).MixHumRatAtCoolPeak = SysSizing(DDNum, AirLoopNum).MixHumRatAtCoolPeak;
                        CalcSysSizing(AirLoopNum).RetHumRatAtCoolPeak = SysSizing(DDNum, AirLoopNum).RetHumRatAtCoolPeak;
                        CalcSysSizing(AirLoopNum).OutTempAtCoolPeak = SysSizing(DDNum, AirLoopNum).OutTempAtCoolPeak;
                        CalcSysSizing(AirLoopNum).OutHumRatAtCoolPeak = SysSizing(DDNum, AirLoopNum).OutHumRatAtCoolPeak;
                        CalcSysSizing(AirLoopNum).SysCoolRetTempSeq = SysSizing(DDNum, AirLoopNum).SysCoolRetTempSeq;
                        CalcSysSizing(AirLoopNum).SysCoolRetHumRatSeq = SysSizing(DDNum, AirLoopNum).SysCoolRetHumRatSeq;
                        CalcSysSizing(AirLoopNum).SysCoolOutTempSeq = SysSizing(DDNum, AirLoopNum).SysCoolOutTempSeq;
                        CalcSysSizing(AirLoopNum).SysCoolOutHumRatSeq = SysSizing(DDNum, AirLoopNum).SysCoolOutHumRatSeq;
                        CalcSysSizing(AirLoopNum).SysDOASHeatAddSeq = SysSizing(DDNum, AirLoopNum).SysDOASHeatAddSeq;
                        CalcSysSizing(AirLoopNum).SysDOASLatAddSeq = SysSizing(DDNum, AirLoopNum).SysDOASLatAddSeq;
                        CalcSysSizing(AirLoopNum).SysCoolCoinSpaceSens = SysSizing(DDNum, AirLoopNum).SysCoolCoinSpaceSens;
                    }
                }

                if (SysSizing(DDNum, AirLoopNum).TotCoolCap > state.dataSize->TotCoolCapTemp(AirLoopNum)) {
                    SysSizPeakDDNum(AirLoopNum).TotCoolPeakDD = DDNum;
                    SysSizPeakDDNum(AirLoopNum).cTotCoolPeakDDDate = state.dataSize->DesDayWeath(DDNum).DateString;
                    state.dataSize->TotCoolCapTemp(AirLoopNum) = SysSizing(DDNum, AirLoopNum).TotCoolCap;
                    if (SysSizing(DDNum, AirLoopNum).CoolingPeakLoadType == TotalCoolingLoad) {
                        CalcSysSizing(AirLoopNum).DesCoolVolFlow = SysSizing(DDNum, AirLoopNum).DesCoolVolFlow;
                        CalcSysSizing(AirLoopNum).CoolDesDay = SysSizing(DDNum, AirLoopNum).CoolDesDay;
                        // CalcSysSizing( AirLoopNum ).CoinCoolMassFlow = SysSizing( DDNum, AirLoopNum ).CoinCoolMassFlow;
                        CalcSysSizing(AirLoopNum).MassFlowAtCoolPeak = SysSizing(DDNum, AirLoopNum).MassFlowAtCoolPeak;
                        CalcSysSizing(AirLoopNum).SensCoolCap = SysSizing(DDNum, AirLoopNum).SensCoolCap;
                        CalcSysSizing(AirLoopNum).TotCoolCap = SysSizing(DDNum, AirLoopNum).TotCoolCap;
                        CalcSysSizing(AirLoopNum).CoolFlowSeq = SysSizing(DDNum, AirLoopNum).CoolFlowSeq;
                        CalcSysSizing(AirLoopNum).SumZoneCoolLoadSeq = SysSizing(DDNum, AirLoopNum).SumZoneCoolLoadSeq;
                        CalcSysSizing(AirLoopNum).CoolZoneAvgTempSeq = SysSizing(DDNum, AirLoopNum).CoolZoneAvgTempSeq;
                        CalcSysSizing(AirLoopNum).SensCoolCapSeq = SysSizing(DDNum, AirLoopNum).SensCoolCapSeq;
                        CalcSysSizing(AirLoopNum).TotCoolCapSeq = SysSizing(DDNum, AirLoopNum).TotCoolCapSeq;
                        CalcSysSizing(AirLoopNum).MixTempAtCoolPeak = SysSizing(DDNum, AirLoopNum).MixTempAtCoolPeak;
                        CalcSysSizing(AirLoopNum).RetTempAtCoolPeak = SysSizing(DDNum, AirLoopNum).RetTempAtCoolPeak;
                        CalcSysSizing(AirLoopNum).MixHumRatAtCoolPeak = SysSizing(DDNum, AirLoopNum).MixHumRatAtCoolPeak;
                        CalcSysSizing(AirLoopNum).RetHumRatAtCoolPeak = SysSizing(DDNum, AirLoopNum).RetHumRatAtCoolPeak;
                        CalcSysSizing(AirLoopNum).OutTempAtCoolPeak = SysSizing(DDNum, AirLoopNum).OutTempAtCoolPeak;
                        CalcSysSizing(AirLoopNum).OutHumRatAtCoolPeak = SysSizing(DDNum, AirLoopNum).OutHumRatAtCoolPeak;
                        CalcSysSizing(AirLoopNum).SysCoolRetTempSeq = SysSizing(DDNum, AirLoopNum).SysCoolRetTempSeq;
                        CalcSysSizing(AirLoopNum).SysCoolRetHumRatSeq = SysSizing(DDNum, AirLoopNum).SysCoolRetHumRatSeq;
                        CalcSysSizing(AirLoopNum).SysCoolOutTempSeq = SysSizing(DDNum, AirLoopNum).SysCoolOutTempSeq;
                        CalcSysSizing(AirLoopNum).SysCoolOutHumRatSeq = SysSizing(DDNum, AirLoopNum).SysCoolOutHumRatSeq;
                        CalcSysSizing(AirLoopNum).SysDOASHeatAddSeq = SysSizing(DDNum, AirLoopNum).SysDOASHeatAddSeq;
                        CalcSysSizing(AirLoopNum).SysDOASLatAddSeq = SysSizing(DDNum, AirLoopNum).SysDOASLatAddSeq;
                    }
                    CalcSysSizing(AirLoopNum).SysCoolCoinSpaceSens = SysSizing(DDNum, AirLoopNum).SysCoolCoinSpaceSens;
                }

                if (SysSizing(DDNum, AirLoopNum).CoinCoolMassFlow > CalcSysSizing(AirLoopNum).CoinCoolMassFlow) {
                    CalcSysSizing(AirLoopNum).CoinCoolMassFlow = SysSizing(DDNum, AirLoopNum).CoinCoolMassFlow;
                    SysSizPeakDDNum(AirLoopNum).CoolFlowPeakDD = DDNum;
                    SysSizPeakDDNum(AirLoopNum).cCoolFlowPeakDDDate = state.dataSize->DesDayWeath(DDNum).DateString;
                }

                if (SysSizing(DDNum, AirLoopNum).HeatCap > CalcSysSizing(AirLoopNum).HeatCap) {
                    SysSizPeakDDNum(AirLoopNum).HeatPeakDD = DDNum;
                    SysSizPeakDDNum(AirLoopNum).cHeatPeakDDDate = state.dataSize->DesDayWeath(DDNum).DateString;
                    CalcSysSizing(AirLoopNum).DesHeatVolFlow = SysSizing(DDNum, AirLoopNum).DesHeatVolFlow;
                    CalcSysSizing(AirLoopNum).HeatDesDay = SysSizing(DDNum, AirLoopNum).HeatDesDay;
                    CalcSysSizing(AirLoopNum).CoinHeatMassFlow = SysSizing(DDNum, AirLoopNum).CoinHeatMassFlow;
                    CalcSysSizing(AirLoopNum).HeatCap = SysSizing(DDNum, AirLoopNum).HeatCap;
                    CalcSysSizing(AirLoopNum).PreheatCap = SysSizing(DDNum, AirLoopNum).PreheatCap;
                    CalcSysSizing(AirLoopNum).HeatFlowSeq = SysSizing(DDNum, AirLoopNum).HeatFlowSeq;
                    CalcSysSizing(AirLoopNum).SumZoneHeatLoadSeq = SysSizing(DDNum, AirLoopNum).SumZoneHeatLoadSeq;
                    CalcSysSizing(AirLoopNum).HeatCapSeq = SysSizing(DDNum, AirLoopNum).HeatCapSeq;
                    CalcSysSizing(AirLoopNum).HeatZoneAvgTempSeq = SysSizing(DDNum, AirLoopNum).HeatZoneAvgTempSeq;
                    CalcSysSizing(AirLoopNum).PreheatCapSeq = SysSizing(DDNum, AirLoopNum).PreheatCapSeq;
                    CalcSysSizing(AirLoopNum).HeatMixTemp = SysSizing(DDNum, AirLoopNum).HeatMixTemp;
                    CalcSysSizing(AirLoopNum).HeatRetTemp = SysSizing(DDNum, AirLoopNum).HeatRetTemp;
                    CalcSysSizing(AirLoopNum).HeatMixHumRat = SysSizing(DDNum, AirLoopNum).HeatMixHumRat;
                    CalcSysSizing(AirLoopNum).HeatRetHumRat = SysSizing(DDNum, AirLoopNum).HeatRetHumRat;
                    CalcSysSizing(AirLoopNum).HeatOutTemp = SysSizing(DDNum, AirLoopNum).HeatOutTemp;
                    CalcSysSizing(AirLoopNum).HeatOutHumRat = SysSizing(DDNum, AirLoopNum).HeatOutHumRat;
                    CalcSysSizing(AirLoopNum).SysHeatRetTempSeq = SysSizing(DDNum, AirLoopNum).SysHeatRetTempSeq;
                    CalcSysSizing(AirLoopNum).SysHeatRetHumRatSeq = SysSizing(DDNum, AirLoopNum).SysHeatRetHumRatSeq;
                    CalcSysSizing(AirLoopNum).SysHeatOutTempSeq = SysSizing(DDNum, AirLoopNum).SysHeatOutTempSeq;
                    CalcSysSizing(AirLoopNum).SysHeatOutHumRatSeq = SysSizing(DDNum, AirLoopNum).SysHeatOutHumRatSeq;

                    CalcSysSizing(AirLoopNum).SysHeatCoilTimeStepPk = SysSizing(DDNum, AirLoopNum).SysHeatCoilTimeStepPk;

                    CalcSysSizing(AirLoopNum).SysHeatAirTimeStepPk = SysSizing(DDNum, AirLoopNum).SysHeatAirTimeStepPk;
                    CalcSysSizing(AirLoopNum).HeatDDNum = DDNum;
                    CalcSysSizing(AirLoopNum).SysHeatCoinSpaceSens = SysSizing(DDNum, AirLoopNum).SysHeatCoinSpaceSens;
                }
            }

            CalcSysSizing(AirLoopNum).DesMainVolFlow = max(CalcSysSizing(AirLoopNum).DesCoolVolFlow, CalcSysSizing(AirLoopNum).DesHeatVolFlow);

            // For noncoincident sizing, find the max heat and cool mass flow for each zone over all the
            // design days. Then calculate the associated heating and cooling capacities.

            int NumZonesCooled = AirToZoneNodeInfo(AirLoopNum).NumZonesCooled;
            int NumZonesHeated = AirToZoneNodeInfo(AirLoopNum).NumZonesHeated;
            SysCoolRetTemp = 0.0;
            OutAirFrac = 0.0;
            SysCoolMixTemp = 0.0;
            SysSensCoolCap = 0.0;
            SysTotCoolCap = 0.0;
            CoolTimeStepNum = 0;
            CoolDDNum = 0;
            OutAirTemp = 0.0;
            OutAirHumRat = 0.0;
            SysCoolMixHumRat = 0.0;
            SysCoolRetHumRat = 0.0;
            SysCoolOutTemp = 0.0;
            SysCoolOutHumRat = 0.0;

            for (int ZonesCooledNum = 1; ZonesCooledNum <= NumZonesCooled; ++ZonesCooledNum) { // loop over cooled zones
                int TermUnitSizingIndex = AirToZoneNodeInfo(AirLoopNum).TermUnitCoolSizingIndex(ZonesCooledNum);
                // save the system cooling supply air temp
                TermUnitFinalZoneSizing(TermUnitSizingIndex).DesCoolCoilInTempTU = CalcSysSizing(AirLoopNum).CoolSupTemp;
                // save the system cooling supply air hum rat
                TermUnitFinalZoneSizing(TermUnitSizingIndex).DesCoolCoilInHumRatTU = CalcSysSizing(AirLoopNum).CoolSupHumRat;
                if (TermUnitFinalZoneSizing(TermUnitSizingIndex).DesCoolMassFlow <= 0.0) continue;
                Real64 coolMassFlow = TermUnitFinalZoneSizing(TermUnitSizingIndex)
                                          .DesCoolMassFlow; // already scaled for term unit sizing in UpdateTermUnitFinalZoneSizing
                CalcSysSizing(AirLoopNum).NonCoinCoolMassFlow += coolMassFlow / (1.0 + TermUnitSizing(TermUnitSizingIndex).InducRat);
                SysCoolRetTemp += TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneRetTempAtCoolPeak * coolMassFlow /
                                  (1.0 + TermUnitSizing(TermUnitSizingIndex).InducRat);
                SysCoolRetHumRat += TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneHumRatAtCoolPeak * coolMassFlow /
                                    (1.0 + TermUnitSizing(TermUnitSizingIndex).InducRat);
                CoolDDNum = TermUnitFinalZoneSizing(TermUnitSizingIndex).CoolDDNum;
                CoolTimeStepNum = TermUnitFinalZoneSizing(TermUnitSizingIndex).TimeStepNumAtCoolMax;
                OutAirTemp += state.dataSize->DesDayWeath(CoolDDNum).Temp(CoolTimeStepNum) * coolMassFlow /
                              (1.0 + TermUnitSizing(TermUnitSizingIndex).InducRat);
                OutAirHumRat += state.dataSize->DesDayWeath(CoolDDNum).HumRat(CoolTimeStepNum) * coolMassFlow /
                                (1.0 + TermUnitSizing(TermUnitSizingIndex).InducRat);
            }
            if (CalcSysSizing(AirLoopNum).NonCoinCoolMassFlow > 0.0) {
                SysCoolRetTemp /= CalcSysSizing(AirLoopNum).NonCoinCoolMassFlow;
                SysCoolRetHumRat /= CalcSysSizing(AirLoopNum).NonCoinCoolMassFlow;
                OutAirTemp /= CalcSysSizing(AirLoopNum).NonCoinCoolMassFlow;
                OutAirHumRat /= CalcSysSizing(AirLoopNum).NonCoinCoolMassFlow;
                SysCoolOutTemp = OutAirTemp;
                SysCoolOutHumRat = OutAirHumRat;
                RhoAir = state.dataEnvrn->StdRhoAir;
                if (CalcSysSizing(AirLoopNum).CoolOAOption == MinOA) {
                    OutAirFrac = RhoAir * CalcSysSizing(AirLoopNum).DesOutAirVolFlow / CalcSysSizing(AirLoopNum).NonCoinCoolMassFlow;
                    OutAirFrac = min(1.0, max(0.0, OutAirFrac));
                } else {
                    OutAirFrac = 1.0;
                }
                SysCoolMixTemp = OutAirTemp * OutAirFrac + SysCoolRetTemp * (1.0 - OutAirFrac);
                SysCoolMixHumRat = OutAirHumRat * OutAirFrac + SysCoolRetHumRat * (1.0 - OutAirFrac);
                SysSensCoolCap = PsyCpAirFnW(DataPrecisionGlobals::constant_zero) * CalcSysSizing(AirLoopNum).NonCoinCoolMassFlow *
                                 (SysCoolMixTemp - CalcSysSizing(AirLoopNum).CoolSupTemp);
                SysSensCoolCap = max(0.0, SysSensCoolCap);
                SysTotCoolCap = CalcSysSizing(AirLoopNum).NonCoinCoolMassFlow *
                                (PsyHFnTdbW(SysCoolMixTemp, SysCoolMixHumRat) -
                                 PsyHFnTdbW(CalcSysSizing(AirLoopNum).CoolSupTemp, CalcSysSizing(AirLoopNum).CoolSupHumRat));
                SysTotCoolCap = max(0.0, SysTotCoolCap);
            }

            SysHeatRetTemp = 0.0;
            OutAirFrac = 0.0;
            SysHeatMixTemp = 0.0;
            SysHeatCap = 0.0;
            HeatTimeStepNum = 0;
            HeatDDNum = 0;
            OutAirTemp = 0.0;
            OutAirHumRat = 0.0;
            SysHeatMixHumRat = 0.0;
            SysHeatRetHumRat = 0.0;
            SysHeatOutTemp = 0.0;
            SysHeatOutHumRat = 0.0;

            if (NumZonesHeated > 0) { // IF there are centrally heated zones

                for (int ZonesHeatedNum = 1; ZonesHeatedNum <= NumZonesHeated; ++ZonesHeatedNum) { // loop over the heated zones
                    int TermUnitSizingIndex = AirToZoneNodeInfo(AirLoopNum).TermUnitHeatSizingIndex(ZonesHeatedNum);
                    // save the system heating supply air temp
                    TermUnitFinalZoneSizing(TermUnitSizingIndex).DesHeatCoilInTempTU = CalcSysSizing(AirLoopNum).HeatSupTemp;
                    // save the system heating supply air hum rat
                    TermUnitFinalZoneSizing(TermUnitSizingIndex).DesHeatCoilInHumRatTU = CalcSysSizing(AirLoopNum).HeatSupHumRat;
                    if (TermUnitFinalZoneSizing(TermUnitSizingIndex).DesHeatMassFlow <= 0.0) continue;
                    Real64 heatMassFlow = TermUnitFinalZoneSizing(TermUnitSizingIndex)
                                              .DesHeatMassFlow; // already scaled for term unit sizing in UpdateTermUnitFinalZoneSizing
                    CalcSysSizing(AirLoopNum).NonCoinHeatMassFlow += heatMassFlow / (1.0 + TermUnitSizing(TermUnitSizingIndex).InducRat);
                    SysHeatRetTemp += TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneRetTempAtHeatPeak * heatMassFlow /
                                      (1.0 + TermUnitSizing(TermUnitSizingIndex).InducRat);
                    SysHeatRetHumRat += TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneHumRatAtHeatPeak * heatMassFlow /
                                        (1.0 + TermUnitSizing(TermUnitSizingIndex).InducRat);
                    HeatDDNum = TermUnitFinalZoneSizing(TermUnitSizingIndex).HeatDDNum;
                    HeatTimeStepNum = TermUnitFinalZoneSizing(TermUnitSizingIndex).TimeStepNumAtHeatMax;
                    OutAirTemp += state.dataSize->DesDayWeath(HeatDDNum).Temp(HeatTimeStepNum) * heatMassFlow /
                                  (1.0 + TermUnitSizing(TermUnitSizingIndex).InducRat);
                    OutAirHumRat += state.dataSize->DesDayWeath(HeatDDNum).HumRat(HeatTimeStepNum) * heatMassFlow /
                                    (1.0 + TermUnitSizing(TermUnitSizingIndex).InducRat);
                }
                if (CalcSysSizing(AirLoopNum).NonCoinHeatMassFlow > 0.0) {
                    SysHeatRetTemp /= CalcSysSizing(AirLoopNum).NonCoinHeatMassFlow;
                    SysHeatRetHumRat /= CalcSysSizing(AirLoopNum).NonCoinHeatMassFlow;
                    OutAirTemp /= CalcSysSizing(AirLoopNum).NonCoinHeatMassFlow;
                    OutAirHumRat /= CalcSysSizing(AirLoopNum).NonCoinHeatMassFlow;
                    SysHeatOutTemp = OutAirTemp;
                    SysHeatOutHumRat = OutAirHumRat;
                    RhoAir = state.dataEnvrn->StdRhoAir;
                    if (CalcSysSizing(AirLoopNum).HeatOAOption == MinOA) {
                        OutAirFrac = RhoAir * CalcSysSizing(AirLoopNum).DesOutAirVolFlow / CalcSysSizing(AirLoopNum).NonCoinHeatMassFlow;
                        OutAirFrac = min(1.0, max(0.0, OutAirFrac));
                    } else {
                        OutAirFrac = 1.0;
                    }
                    SysHeatMixTemp = OutAirTemp * OutAirFrac + SysHeatRetTemp * (1.0 - OutAirFrac);
                    SysHeatMixHumRat = OutAirHumRat * OutAirFrac + SysHeatRetHumRat * (1.0 - OutAirFrac);
                    SysHeatCap = PsyCpAirFnW(DataPrecisionGlobals::constant_zero) * CalcSysSizing(AirLoopNum).NonCoinHeatMassFlow *
                                 (CalcSysSizing(AirLoopNum).HeatSupTemp - SysHeatMixTemp);
                    SysHeatCap = max(0.0, SysHeatCap);
                }

            } else { // No centrally heated zones: use cooled zones

                for (int ZonesCooledNum = 1; ZonesCooledNum <= NumZonesCooled; ++ZonesCooledNum) { // loop over the cooled zones
                    int TermUnitSizingIndex = AirToZoneNodeInfo(AirLoopNum).TermUnitCoolSizingIndex(ZonesCooledNum);
                    // save the system heating supply air temp
                    TermUnitFinalZoneSizing(TermUnitSizingIndex).DesHeatCoilInTempTU = CalcSysSizing(AirLoopNum).HeatSupTemp;
                    // save the system heating supply air hum rat
                    TermUnitFinalZoneSizing(TermUnitSizingIndex).DesHeatCoilInHumRatTU = CalcSysSizing(AirLoopNum).HeatSupHumRat;
                    if (TermUnitFinalZoneSizing(TermUnitSizingIndex).DesHeatMassFlow <= 0.0) continue;
                    Real64 heatMassFlow = TermUnitFinalZoneSizing(TermUnitSizingIndex)
                                              .DesHeatMassFlow; // already scaled for term unit sizing in UpdateTermUnitFinalZoneSizing
                    CalcSysSizing(AirLoopNum).NonCoinHeatMassFlow += heatMassFlow / (1.0 + TermUnitSizing(TermUnitSizingIndex).InducRat);
                    SysHeatRetTemp += TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneRetTempAtHeatPeak * heatMassFlow /
                                      (1.0 + TermUnitSizing(TermUnitSizingIndex).InducRat);
                    SysHeatRetHumRat += TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneHumRatAtHeatPeak * heatMassFlow /
                                        (1.0 + TermUnitSizing(TermUnitSizingIndex).InducRat);
                    HeatDDNum = TermUnitFinalZoneSizing(TermUnitSizingIndex).HeatDDNum;
                    HeatTimeStepNum = TermUnitFinalZoneSizing(TermUnitSizingIndex).TimeStepNumAtHeatMax;
                    OutAirTemp += state.dataSize->DesDayWeath(HeatDDNum).Temp(HeatTimeStepNum) * heatMassFlow /
                                  (1.0 + TermUnitSizing(TermUnitSizingIndex).InducRat);
                    OutAirHumRat += state.dataSize->DesDayWeath(HeatDDNum).HumRat(HeatTimeStepNum) * heatMassFlow /
                                    (1.0 + TermUnitSizing(TermUnitSizingIndex).InducRat);
                }
                if (CalcSysSizing(AirLoopNum).NonCoinHeatMassFlow > 0.0) {
                    SysHeatRetTemp /= CalcSysSizing(AirLoopNum).NonCoinHeatMassFlow;
                    SysHeatRetHumRat /= CalcSysSizing(AirLoopNum).NonCoinHeatMassFlow;
                    OutAirTemp /= CalcSysSizing(AirLoopNum).NonCoinHeatMassFlow;
                    OutAirHumRat /= CalcSysSizing(AirLoopNum).NonCoinHeatMassFlow;
                    SysHeatOutTemp = OutAirTemp;
                    SysHeatOutHumRat = OutAirHumRat;
                    RhoAir = state.dataEnvrn->StdRhoAir;
                    if (CalcSysSizing(AirLoopNum).HeatOAOption == MinOA) {
                        OutAirFrac = RhoAir * CalcSysSizing(AirLoopNum).DesOutAirVolFlow / CalcSysSizing(AirLoopNum).NonCoinHeatMassFlow;
                        OutAirFrac = min(1.0, max(0.0, OutAirFrac));
                    } else {
                        OutAirFrac = 1.0;
                    }
                    SysHeatMixTemp = OutAirTemp * OutAirFrac + SysHeatRetTemp * (1.0 - OutAirFrac);
                    SysHeatMixHumRat = OutAirHumRat * OutAirFrac + SysHeatRetHumRat * (1.0 - OutAirFrac);
                    SysHeatCap = PsyCpAirFnW(DataPrecisionGlobals::constant_zero) * CalcSysSizing(AirLoopNum).NonCoinHeatMassFlow *
                                 (CalcSysSizing(AirLoopNum).HeatSupTemp - SysHeatMixTemp);
                    SysHeatCap = max(0.0, SysHeatCap);
                }
            }

            // move the noncoincident results into the system sizing array
            if (CalcSysSizing(AirLoopNum).SizingOption == NonCoincident) {
                // But first check to see if the noncoincident result is actually bigger than the coincident (for 100% outside air)
                if (!(FinalSysSizing(AirLoopNum).CoolOAOption == 1 && SysSensCoolCap <= 0.0)) { // CoolOAOption = Yes 100% OA
                    CalcSysSizing(AirLoopNum).SensCoolCap = SysSensCoolCap;
                    CalcSysSizing(AirLoopNum).TotCoolCap = SysTotCoolCap;
                    CalcSysSizing(AirLoopNum).MixTempAtCoolPeak = SysCoolMixTemp;
                    CalcSysSizing(AirLoopNum).RetTempAtCoolPeak = SysCoolRetTemp;
                    CalcSysSizing(AirLoopNum).MixHumRatAtCoolPeak = SysCoolMixHumRat;
                    CalcSysSizing(AirLoopNum).RetHumRatAtCoolPeak = SysCoolRetHumRat;
                    CalcSysSizing(AirLoopNum).OutTempAtCoolPeak = SysCoolOutTemp;
                    CalcSysSizing(AirLoopNum).OutHumRatAtCoolPeak = SysCoolOutHumRat;
                }
                // check to see if the noncoincident result is actually bigger than the coincident (for 100% outside air)
                // why is this < 0.0 ? SysHeatCap cannot be < 0 ?? this code will always get executed
                if (!(FinalSysSizing(AirLoopNum).HeatOAOption == 1 && SysHeatCap < 0.0)) { // HeatOAOption = Yes 100% OA
                    CalcSysSizing(AirLoopNum).HeatCap = SysHeatCap;
                    CalcSysSizing(AirLoopNum).HeatMixTemp = SysHeatMixTemp;
                    CalcSysSizing(AirLoopNum).HeatRetTemp = SysHeatRetTemp;
                    CalcSysSizing(AirLoopNum).HeatMixHumRat = SysHeatMixHumRat;
                    CalcSysSizing(AirLoopNum).HeatRetHumRat = SysHeatRetHumRat;
                    CalcSysSizing(AirLoopNum).HeatOutTemp = SysHeatOutTemp;
                    CalcSysSizing(AirLoopNum).HeatOutHumRat = SysHeatOutHumRat;
                }
                CalcSysSizing(AirLoopNum).DesCoolVolFlow = CalcSysSizing(AirLoopNum).NonCoinCoolMassFlow / state.dataEnvrn->StdRhoAir;
                CalcSysSizing(AirLoopNum).DesHeatVolFlow = CalcSysSizing(AirLoopNum).NonCoinHeatMassFlow / state.dataEnvrn->StdRhoAir;
                CalcSysSizing(AirLoopNum).DesMainVolFlow = max(CalcSysSizing(AirLoopNum).DesCoolVolFlow, CalcSysSizing(AirLoopNum).DesHeatVolFlow);
            }
        }

        // Move final system design data (calculated from zone data) to user design array
        for (std::size_t i = 0; i < FinalSysSizing.size(); ++i) {
            auto &z(FinalSysSizing[i]);
            auto &c(CalcSysSizing[i]);
            z.CoolDesDay = c.CoolDesDay;
            z.HeatDesDay = c.HeatDesDay;
            z.CoinCoolMassFlow = c.CoinCoolMassFlow;
            z.CoinHeatMassFlow = c.CoinHeatMassFlow;
            z.NonCoinCoolMassFlow = c.NonCoinCoolMassFlow;
            z.NonCoinHeatMassFlow = c.NonCoinHeatMassFlow;
            z.DesMainVolFlow = c.DesMainVolFlow;
            z.DesHeatVolFlow = c.DesHeatVolFlow;
            z.DesCoolVolFlow = c.DesCoolVolFlow;
            z.MassFlowAtCoolPeak = c.MassFlowAtCoolPeak;
            z.SensCoolCap = c.SensCoolCap;
            z.TotCoolCap = c.TotCoolCap;
            z.HeatCap = c.HeatCap;
            z.PreheatCap = c.PreheatCap;
            z.MixTempAtCoolPeak = c.MixTempAtCoolPeak;
            z.MixHumRatAtCoolPeak = c.MixHumRatAtCoolPeak;
            z.RetTempAtCoolPeak = c.RetTempAtCoolPeak;
            z.RetHumRatAtCoolPeak = c.RetHumRatAtCoolPeak;
            z.OutTempAtCoolPeak = c.OutTempAtCoolPeak;
            z.OutHumRatAtCoolPeak = c.OutHumRatAtCoolPeak;
            z.HeatMixTemp = c.HeatMixTemp;
            z.HeatMixHumRat = c.HeatMixHumRat;
            z.HeatRetTemp = c.HeatRetTemp;
            z.HeatRetHumRat = c.HeatRetHumRat;
            z.HeatOutTemp = c.HeatOutTemp;
            z.HeatOutHumRat = c.HeatOutHumRat;
            z.SysHeatCoilTimeStepPk = c.SysHeatCoilTimeStepPk;
            z.SysHeatAirTimeStepPk = c.SysHeatAirTimeStepPk;
            z.HeatDDNum = c.HeatDDNum;
            z.SysCoolCoinSpaceSens = c.SysCoolCoinSpaceSens;
            z.SysHeatCoinSpaceSens = c.SysHeatCoinSpaceSens;
        }

        for (AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum) {
            for (TimeStepIndex = 1; TimeStepIndex <= numOfTimeStepInDay; ++TimeStepIndex) {
                FinalSysSizing(AirLoopNum).HeatFlowSeq(TimeStepIndex) = CalcSysSizing(AirLoopNum).HeatFlowSeq(TimeStepIndex);
                FinalSysSizing(AirLoopNum).CoolFlowSeq(TimeStepIndex) = CalcSysSizing(AirLoopNum).CoolFlowSeq(TimeStepIndex);
                FinalSysSizing(AirLoopNum).SumZoneCoolLoadSeq(TimeStepIndex) = CalcSysSizing(AirLoopNum).SumZoneCoolLoadSeq(TimeStepIndex);
                FinalSysSizing(AirLoopNum).SumZoneHeatLoadSeq(TimeStepIndex) = CalcSysSizing(AirLoopNum).SumZoneHeatLoadSeq(TimeStepIndex);
                FinalSysSizing(AirLoopNum).CoolZoneAvgTempSeq(TimeStepIndex) = CalcSysSizing(AirLoopNum).CoolZoneAvgTempSeq(TimeStepIndex);
                FinalSysSizing(AirLoopNum).HeatZoneAvgTempSeq(TimeStepIndex) = CalcSysSizing(AirLoopNum).HeatZoneAvgTempSeq(TimeStepIndex);
                FinalSysSizing(AirLoopNum).SensCoolCapSeq(TimeStepIndex) = CalcSysSizing(AirLoopNum).SensCoolCapSeq(TimeStepIndex);
                FinalSysSizing(AirLoopNum).TotCoolCapSeq(TimeStepIndex) = CalcSysSizing(AirLoopNum).TotCoolCapSeq(TimeStepIndex);
                FinalSysSizing(AirLoopNum).HeatCapSeq(TimeStepIndex) = CalcSysSizing(AirLoopNum).HeatCapSeq(TimeStepIndex);
                FinalSysSizing(AirLoopNum).PreheatCapSeq(TimeStepIndex) = CalcSysSizing(AirLoopNum).PreheatCapSeq(TimeStepIndex);
                FinalSysSizing(AirLoopNum).SysCoolRetTempSeq(TimeStepIndex) = CalcSysSizing(AirLoopNum).SysCoolRetTempSeq(TimeStepIndex);
                FinalSysSizing(AirLoopNum).SysCoolRetHumRatSeq(TimeStepIndex) = CalcSysSizing(AirLoopNum).SysCoolRetHumRatSeq(TimeStepIndex);
                FinalSysSizing(AirLoopNum).SysHeatRetTempSeq(TimeStepIndex) = CalcSysSizing(AirLoopNum).SysHeatRetTempSeq(TimeStepIndex);
                FinalSysSizing(AirLoopNum).SysHeatRetHumRatSeq(TimeStepIndex) = CalcSysSizing(AirLoopNum).SysHeatRetHumRatSeq(TimeStepIndex);
                FinalSysSizing(AirLoopNum).SysCoolOutTempSeq(TimeStepIndex) = CalcSysSizing(AirLoopNum).SysCoolOutTempSeq(TimeStepIndex);
                FinalSysSizing(AirLoopNum).SysCoolOutHumRatSeq(TimeStepIndex) = CalcSysSizing(AirLoopNum).SysCoolOutHumRatSeq(TimeStepIndex);
                FinalSysSizing(AirLoopNum).SysHeatOutTempSeq(TimeStepIndex) = CalcSysSizing(AirLoopNum).SysHeatOutTempSeq(TimeStepIndex);
                FinalSysSizing(AirLoopNum).SysHeatOutHumRatSeq(TimeStepIndex) = CalcSysSizing(AirLoopNum).SysHeatOutHumRatSeq(TimeStepIndex);
                FinalSysSizing(AirLoopNum).SysDOASHeatAddSeq(TimeStepIndex) = CalcSysSizing(AirLoopNum).SysDOASHeatAddSeq(TimeStepIndex);
                FinalSysSizing(AirLoopNum).SysDOASLatAddSeq(TimeStepIndex) = CalcSysSizing(AirLoopNum).SysDOASLatAddSeq(TimeStepIndex);
            }
        }

        // Check for user input design system flow rates. Set the sizing ratios.
        for (AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum) {

            // adjust system sizing flow rates for scalable flows
            UpdateSysSizingForScalableInputs(state, AirLoopNum);

            int NumZonesCooled = AirToZoneNodeInfo(AirLoopNum).NumZonesCooled;
            int NumZonesHeated = AirToZoneNodeInfo(AirLoopNum).NumZonesHeated;
            RhoAir = state.dataEnvrn->StdRhoAir;
            SysCoolSizingRat = 0.0;
            if (CalcSysSizing(AirLoopNum).InpDesCoolAirFlow > 0.0 && CalcSysSizing(AirLoopNum).DesCoolVolFlow > 0.0 &&
                (CalcSysSizing(AirLoopNum).CoolAirDesMethod == InpDesAirFlow || CalcSysSizing(AirLoopNum).ScaleCoolSAFMethod == FlowPerFloorArea ||
                 CalcSysSizing(AirLoopNum).ScaleCoolSAFMethod == FractionOfAutosizedCoolingAirflow ||
                 CalcSysSizing(AirLoopNum).ScaleCoolSAFMethod == FlowPerCoolingCapacity)) {
                SysCoolSizingRat = CalcSysSizing(AirLoopNum).InpDesCoolAirFlow / CalcSysSizing(AirLoopNum).DesCoolVolFlow;
            } else {
                SysCoolSizingRat = 1.0;
            }

            SysHeatSizingRat = 0.0;
            if (CalcSysSizing(AirLoopNum).InpDesHeatAirFlow > 0.0 && CalcSysSizing(AirLoopNum).DesHeatVolFlow > 0.0 &&
                (CalcSysSizing(AirLoopNum).HeatAirDesMethod == InpDesAirFlow || CalcSysSizing(AirLoopNum).ScaleHeatSAFMethod == FlowPerFloorArea ||
                 CalcSysSizing(AirLoopNum).ScaleHeatSAFMethod == FractionOfAutosizedHeatingAirflow ||
                 CalcSysSizing(AirLoopNum).ScaleHeatSAFMethod == FractionOfAutosizedCoolingAirflow ||
                 CalcSysSizing(AirLoopNum).ScaleHeatSAFMethod == FlowPerHeatingCapacity)) {
                SysHeatSizingRat = CalcSysSizing(AirLoopNum).InpDesHeatAirFlow / CalcSysSizing(AirLoopNum).DesHeatVolFlow;
            } else {
                SysHeatSizingRat = 1.0;
            }

            if (CalcSysSizing(AirLoopNum).LoadSizeType == Ventilation && SysCoolSizingRat == 1.0) {
                if (CalcSysSizing(AirLoopNum).DesCoolVolFlow > 0.0) {
                    SysCoolSizingRat = CalcSysSizing(AirLoopNum).DesOutAirVolFlow / CalcSysSizing(AirLoopNum).DesCoolVolFlow;
                    state.dataSize->VotClgBySys(AirLoopNum) = FinalSysSizing(AirLoopNum).DesOutAirVolFlow;
                } else {
                    SysCoolSizingRat = 1.0;
                }
            }
            if (CalcSysSizing(AirLoopNum).LoadSizeType == Ventilation && SysHeatSizingRat == 1.0) {
                if (CalcSysSizing(AirLoopNum).DesHeatVolFlow > 0.0) {
                    SysHeatSizingRat = CalcSysSizing(AirLoopNum).DesOutAirVolFlow / CalcSysSizing(AirLoopNum).DesHeatVolFlow;
                    state.dataSize->VotHtgBySys(AirLoopNum) = FinalSysSizing(AirLoopNum).DesOutAirVolFlow;
                } else {
                    SysHeatSizingRat = 1.0;
                }
            }

            // Calculate the new user modified system design quantities
            if (std::abs(SysCoolSizingRat - 1.0) > 0.00001) {

                FinalSysSizing(AirLoopNum).CoinCoolMassFlow = SysCoolSizingRat * CalcSysSizing(AirLoopNum).CoinCoolMassFlow;
                FinalSysSizing(AirLoopNum).NonCoinCoolMassFlow = SysCoolSizingRat * CalcSysSizing(AirLoopNum).NonCoinCoolMassFlow;
                FinalSysSizing(AirLoopNum).DesCoolVolFlow = SysCoolSizingRat * CalcSysSizing(AirLoopNum).DesCoolVolFlow;
                FinalSysSizing(AirLoopNum).MassFlowAtCoolPeak = SysCoolSizingRat * CalcSysSizing(AirLoopNum).MassFlowAtCoolPeak;

                if (FinalSysSizing(AirLoopNum).DesCoolVolFlow > 0.0) {

                    for (TimeStepIndex = 1; TimeStepIndex <= numOfTimeStepInDay; ++TimeStepIndex) {

                        if (CalcSysSizing(AirLoopNum).CoolFlowSeq(TimeStepIndex) > 0.0) {

                            FinalSysSizing(AirLoopNum).CoolFlowSeq(TimeStepIndex) =
                                SysCoolSizingRat * CalcSysSizing(AirLoopNum).CoolFlowSeq(TimeStepIndex);
                            if (FinalSysSizing(AirLoopNum).CoolOAOption == MinOA) {
                                OutAirFrac =
                                    RhoAir * FinalSysSizing(AirLoopNum).DesOutAirVolFlow / FinalSysSizing(AirLoopNum).CoolFlowSeq(TimeStepIndex);
                                OutAirFrac = min(1.0, max(0.0, OutAirFrac));
                            } else {
                                OutAirFrac = 1.0;
                            }
                            SysCoolMixTemp = FinalSysSizing(AirLoopNum).SysCoolOutTempSeq(TimeStepIndex) * OutAirFrac +
                                             FinalSysSizing(AirLoopNum).SysCoolRetTempSeq(TimeStepIndex) * (1.0 - OutAirFrac);
                            SysCoolMixHumRat = FinalSysSizing(AirLoopNum).SysCoolOutHumRatSeq(TimeStepIndex) * OutAirFrac +
                                               FinalSysSizing(AirLoopNum).SysCoolRetHumRatSeq(TimeStepIndex) * (1.0 - OutAirFrac);
                            SysSensCoolCap = PsyCpAirFnW(DataPrecisionGlobals::constant_zero) *
                                             FinalSysSizing(AirLoopNum).CoolFlowSeq(TimeStepIndex) *
                                             (SysCoolMixTemp - FinalSysSizing(AirLoopNum).CoolSupTemp);
                            SysSensCoolCap = max(0.0, SysSensCoolCap);
                            SysTotCoolCap = FinalSysSizing(AirLoopNum).CoolFlowSeq(TimeStepIndex) *
                                            (PsyHFnTdbW(SysCoolMixTemp, SysCoolMixHumRat) -
                                             PsyHFnTdbW(FinalSysSizing(AirLoopNum).CoolSupTemp, FinalSysSizing(AirLoopNum).CoolSupHumRat));
                            SysTotCoolCap = max(0.0, SysTotCoolCap);
                            FinalSysSizing(AirLoopNum).SensCoolCapSeq(TimeStepIndex) = SysSensCoolCap;
                            FinalSysSizing(AirLoopNum).TotCoolCapSeq(TimeStepIndex) = SysTotCoolCap;
                        }
                    }

                    if (FinalSysSizing(AirLoopNum).CoolOAOption == MinOA) {
                        OutAirFrac = FinalSysSizing(AirLoopNum).DesOutAirVolFlow / FinalSysSizing(AirLoopNum).DesCoolVolFlow;
                        OutAirFrac = min(1.0, max(0.0, OutAirFrac));
                    } else {
                        OutAirFrac = 1.0;
                    }
                    FinalSysSizing(AirLoopNum).MixTempAtCoolPeak =
                        FinalSysSizing(AirLoopNum).OutTempAtCoolPeak * OutAirFrac + FinalSysSizing(AirLoopNum).RetTempAtCoolPeak * (1.0 - OutAirFrac);
                    FinalSysSizing(AirLoopNum).MixHumRatAtCoolPeak = FinalSysSizing(AirLoopNum).OutHumRatAtCoolPeak * OutAirFrac +
                                                                     FinalSysSizing(AirLoopNum).RetHumRatAtCoolPeak * (1.0 - OutAirFrac);
                    FinalSysSizing(AirLoopNum).SensCoolCap = PsyCpAirFnW(DataPrecisionGlobals::constant_zero) * RhoAir *
                                                             FinalSysSizing(AirLoopNum).DesCoolVolFlow *
                                                             (FinalSysSizing(AirLoopNum).MixTempAtCoolPeak - FinalSysSizing(AirLoopNum).CoolSupTemp);
                    FinalSysSizing(AirLoopNum).SensCoolCap = max(0.0, FinalSysSizing(AirLoopNum).SensCoolCap);
                    FinalSysSizing(AirLoopNum).TotCoolCap =
                        RhoAir * FinalSysSizing(AirLoopNum).DesCoolVolFlow *
                        (PsyHFnTdbW(FinalSysSizing(AirLoopNum).MixTempAtCoolPeak, FinalSysSizing(AirLoopNum).MixHumRatAtCoolPeak) -
                         PsyHFnTdbW(FinalSysSizing(AirLoopNum).CoolSupTemp, FinalSysSizing(AirLoopNum).CoolSupHumRat));
                    FinalSysSizing(AirLoopNum).TotCoolCap = max(0.0, FinalSysSizing(AirLoopNum).TotCoolCap);
                }

                // take account of the user input system flow rates and alter the zone flow rates to match
                for (int ZonesCooledNum = 1; ZonesCooledNum <= NumZonesCooled; ++ZonesCooledNum) {
                    int TermUnitSizingIndex = AirToZoneNodeInfo(AirLoopNum).TermUnitCoolSizingIndex(ZonesCooledNum);
                    if ((SysCoolSizingRat != 1.0) && (FinalSysSizing(AirLoopNum).LoadSizeType == Ventilation) &&
                        (TermUnitFinalZoneSizing(TermUnitSizingIndex).MinOA > 0.0)) {
                        // size on ventilation load
                        if (TermUnitFinalZoneSizing(TermUnitSizingIndex).MinOA > 0.0) {
                            ZoneOARatio =
                                TermUnitFinalZoneSizing(TermUnitSizingIndex).MinOA /
                                max(TermUnitFinalZoneSizing(TermUnitSizingIndex).DesCoolVolFlow, TermUnitFinalZoneSizing(TermUnitSizingIndex).MinOA);
                            ZoneOARatio *= (1.0 + TermUnitSizing(TermUnitSizingIndex).InducRat);
                        } else {
                            ZoneOARatio = 0.0;
                        }
                        TermUnitFinalZoneSizing(TermUnitSizingIndex).scaleZoneCooling(ZoneOARatio);
                    } else if ((SysCoolSizingRat > 1.0) || (SysCoolSizingRat < 1.0 && FinalSysSizing(AirLoopNum).SizingOption == NonCoincident)) {
                        // size on user input system design flows
                        TermUnitFinalZoneSizing(TermUnitSizingIndex).scaleZoneCooling(SysCoolSizingRat);
                    }
                }
            }

            if (std::abs(SysHeatSizingRat - 1.0) > 0.00001) {

                FinalSysSizing(AirLoopNum).CoinHeatMassFlow = SysHeatSizingRat * CalcSysSizing(AirLoopNum).CoinHeatMassFlow;
                FinalSysSizing(AirLoopNum).NonCoinHeatMassFlow = SysHeatSizingRat * CalcSysSizing(AirLoopNum).NonCoinHeatMassFlow;
                FinalSysSizing(AirLoopNum).DesHeatVolFlow = SysHeatSizingRat * CalcSysSizing(AirLoopNum).DesHeatVolFlow;

                if (FinalSysSizing(AirLoopNum).DesHeatVolFlow > 0.0) {

                    for (TimeStepIndex = 1; TimeStepIndex <= numOfTimeStepInDay; ++TimeStepIndex) {

                        if (CalcSysSizing(AirLoopNum).HeatFlowSeq(TimeStepIndex) > 0.0) {

                            FinalSysSizing(AirLoopNum).HeatFlowSeq(TimeStepIndex) =
                                SysHeatSizingRat * CalcSysSizing(AirLoopNum).HeatFlowSeq(TimeStepIndex);
                            if (FinalSysSizing(AirLoopNum).HeatOAOption == MinOA) {
                                OutAirFrac =
                                    RhoAir * FinalSysSizing(AirLoopNum).DesOutAirVolFlow / FinalSysSizing(AirLoopNum).HeatFlowSeq(TimeStepIndex);
                                OutAirFrac = min(1.0, max(0.0, OutAirFrac));
                            } else {
                                OutAirFrac = 1.0;
                            }
                            SysHeatMixTemp = FinalSysSizing(AirLoopNum).SysHeatOutTempSeq(TimeStepIndex) * OutAirFrac +
                                             FinalSysSizing(AirLoopNum).SysHeatRetTempSeq(TimeStepIndex) * (1.0 - OutAirFrac);
                            SysHeatMixHumRat = FinalSysSizing(AirLoopNum).SysHeatOutHumRatSeq(TimeStepIndex) * OutAirFrac +
                                               FinalSysSizing(AirLoopNum).SysHeatRetHumRatSeq(TimeStepIndex) * (1.0 - OutAirFrac);
                            SysHeatCap = PsyCpAirFnW(DataPrecisionGlobals::constant_zero) * FinalSysSizing(AirLoopNum).HeatFlowSeq(TimeStepIndex) *
                                         (FinalSysSizing(AirLoopNum).HeatSupTemp - SysHeatMixTemp);
                            SysHeatCap = max(0.0, SysHeatCap);
                            FinalSysSizing(AirLoopNum).HeatCapSeq(TimeStepIndex) = SysHeatCap;
                        }
                    }

                    if (FinalSysSizing(AirLoopNum).HeatOAOption == MinOA) {
                        OutAirFrac = FinalSysSizing(AirLoopNum).DesOutAirVolFlow / FinalSysSizing(AirLoopNum).DesHeatVolFlow;
                        OutAirFrac = min(1.0, max(0.0, OutAirFrac));
                    } else {
                        OutAirFrac = 1.0;
                    }
                    FinalSysSizing(AirLoopNum).HeatMixTemp =
                        FinalSysSizing(AirLoopNum).HeatOutTemp * OutAirFrac + FinalSysSizing(AirLoopNum).HeatRetTemp * (1.0 - OutAirFrac);
                    FinalSysSizing(AirLoopNum).HeatMixHumRat =
                        FinalSysSizing(AirLoopNum).HeatOutHumRat * OutAirFrac + FinalSysSizing(AirLoopNum).HeatRetHumRat * (1.0 - OutAirFrac);
                    FinalSysSizing(AirLoopNum).HeatCap = PsyCpAirFnW(DataPrecisionGlobals::constant_zero) * RhoAir *
                                                         FinalSysSizing(AirLoopNum).DesHeatVolFlow *
                                                         (FinalSysSizing(AirLoopNum).HeatSupTemp - FinalSysSizing(AirLoopNum).HeatMixTemp);
                    FinalSysSizing(AirLoopNum).HeatCap = max(0.0, FinalSysSizing(AirLoopNum).HeatCap);
                }
                // take account of the user input system flow rates and alter the zone flow rates to match (for terminal unit sizing)
                if (NumZonesHeated > 0) {                                                              // IF there are centrally heated zones
                    for (int ZonesHeatedNum = 1; ZonesHeatedNum <= NumZonesHeated; ++ZonesHeatedNum) { // loop over the heated zones
                        int TermUnitSizingIndex = AirToZoneNodeInfo(AirLoopNum).TermUnitHeatSizingIndex(ZonesHeatedNum);
                        if ((SysHeatSizingRat != 1.0) && (FinalSysSizing(AirLoopNum).LoadSizeType == Ventilation) &&
                            (TermUnitFinalZoneSizing(TermUnitSizingIndex).MinOA > 0.0)) {
                            // size on ventilation load
                            if (TermUnitFinalZoneSizing(TermUnitSizingIndex).MinOA > 0.0) {
                                ZoneOARatio = TermUnitFinalZoneSizing(TermUnitSizingIndex).MinOA /
                                              max(TermUnitFinalZoneSizing(TermUnitSizingIndex).DesHeatVolFlow,
                                                  TermUnitFinalZoneSizing(TermUnitSizingIndex).MinOA);
                                ZoneOARatio *= (1.0 + TermUnitSizing(TermUnitSizingIndex).InducRat);
                            } else {
                                ZoneOARatio = 0.0;
                            }
                            TermUnitFinalZoneSizing(TermUnitSizingIndex).scaleZoneHeating(ZoneOARatio);
                        } else if ((SysHeatSizingRat > 1.0) || (SysHeatSizingRat < 1.0 && FinalSysSizing(AirLoopNum).SizingOption == NonCoincident)) {
                            // size on user input system design flows
                            TermUnitFinalZoneSizing(TermUnitSizingIndex).scaleZoneHeating(SysHeatSizingRat);
                        }
                    }
                } else {                                                                               // No centrally heated zones: use cooled zones
                    for (int ZonesCooledNum = 1; ZonesCooledNum <= NumZonesCooled; ++ZonesCooledNum) { // loop over the cooled zones
                        int TermUnitSizingIndex = AirToZoneNodeInfo(AirLoopNum).TermUnitCoolSizingIndex(ZonesCooledNum);
                        if ((SysHeatSizingRat != 1.0) && (FinalSysSizing(AirLoopNum).LoadSizeType == Ventilation) &&
                            (TermUnitFinalZoneSizing(TermUnitSizingIndex).MinOA <= 0.0)) {
                            ShowWarningError(state,
                                             "FinalSystemSizing: AirLoop=\"" + AirToZoneNodeInfo(AirLoopNum).AirLoopName +
                                                 "\", Requested sizing on Ventilation,");
                            ShowContinueError(
                                state, "but Zone has no design OA Flow. Zone=\"" + TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneName + "\".");
                        }
                        if ((SysHeatSizingRat != 1.0) && (FinalSysSizing(AirLoopNum).LoadSizeType == Ventilation) &&
                            (TermUnitFinalZoneSizing(TermUnitSizingIndex).MinOA > 0.0)) {
                            // size on ventilation load
                            if (TermUnitFinalZoneSizing(TermUnitSizingIndex).MinOA > 0.0) {
                                ZoneOARatio = TermUnitFinalZoneSizing(TermUnitSizingIndex).MinOA /
                                              max(TermUnitFinalZoneSizing(TermUnitSizingIndex).DesHeatVolFlow,
                                                  TermUnitFinalZoneSizing(TermUnitSizingIndex).MinOA);
                                ZoneOARatio *= (1.0 + TermUnitSizing(TermUnitSizingIndex).InducRat);
                            } else {
                                ZoneOARatio = 0.0;
                            }
                            TermUnitFinalZoneSizing(TermUnitSizingIndex).scaleZoneHeating(ZoneOARatio);
                        } else if ((SysHeatSizingRat != 1.0) && (FinalSysSizing(AirLoopNum).LoadSizeType == Ventilation) &&
                                   (TermUnitFinalZoneSizing(TermUnitSizingIndex).MinOA > 0.0)) {
                            // size on user input system design flows
                            TermUnitFinalZoneSizing(TermUnitSizingIndex).scaleZoneHeating(SysHeatSizingRat);
                        }
                    }
                }
            }

            FinalSysSizing(AirLoopNum).DesMainVolFlow = max(FinalSysSizing(AirLoopNum).DesCoolVolFlow, FinalSysSizing(AirLoopNum).DesHeatVolFlow);

            // loop over the zones cooled by this system and sum up the min cooling flow rates to get the
            // min system cooling flow rate
            for (int ZonesCooledNum = 1; ZonesCooledNum <= NumZonesCooled; ++ZonesCooledNum) {
                int TermUnitSizingIndex = AirToZoneNodeInfo(AirLoopNum).TermUnitCoolSizingIndex(ZonesCooledNum);
                FinalSysSizing(AirLoopNum).DesCoolVolFlowMin += TermUnitFinalZoneSizing(TermUnitSizingIndex).DesCoolVolFlowMin;
            }
            if (FinalSysSizing(AirLoopNum).DesCoolVolFlowMin <= 0.0) {
                FinalSysSizing(AirLoopNum).DesCoolVolFlowMin = FinalSysSizing(AirLoopNum).DesOutAirVolFlow;
            }
        }

        // Specify the heating supply air Temp/HumRat for different system configurations
        for (AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum) {

            int NumZonesHeated = AirToZoneNodeInfo(AirLoopNum).NumZonesHeated;

            if (NumZonesHeated > 0) { // IF there are centrally heated zones
                for (int ZonesHeatedNum = 1; ZonesHeatedNum <= NumZonesHeated; ++ZonesHeatedNum) {
                    int TermUnitSizingIndex = AirToZoneNodeInfo(AirLoopNum).TermUnitHeatSizingIndex(ZonesHeatedNum);

                    TermUnitFinalZoneSizing(TermUnitSizingIndex).DesHeatCoilInTempTU = GetHeatingSATempForSizing(state, AirLoopNum);
                    TermUnitFinalZoneSizing(TermUnitSizingIndex).DesHeatCoilInHumRatTU = GetHeatingSATempHumRatForSizing(state, AirLoopNum);
                }
            }
        }

        // EMS calling point to customize system sizing results
        bool anyEMSRan;
        ManageEMS(state, EMSManager::EMSCallFrom::SystemSizing, anyEMSRan, ObjexxFCL::Optional_int_const());

        // EMS override point
        if (state.dataGlobal->AnyEnergyManagementSystemInModel) {
            for (AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum) {
                if (FinalSysSizing(AirLoopNum).EMSOverrideCoinCoolMassFlowOn)
                    FinalSysSizing(AirLoopNum).CoinCoolMassFlow = FinalSysSizing(AirLoopNum).EMSValueCoinCoolMassFlow;
                if (FinalSysSizing(AirLoopNum).EMSOverrideCoinHeatMassFlowOn)
                    FinalSysSizing(AirLoopNum).CoinHeatMassFlow = FinalSysSizing(AirLoopNum).EMSValueCoinHeatMassFlow;
                if (FinalSysSizing(AirLoopNum).EMSOverrideNonCoinCoolMassFlowOn)
                    FinalSysSizing(AirLoopNum).NonCoinCoolMassFlow = FinalSysSizing(AirLoopNum).EMSValueNonCoinCoolMassFlow;
                if (FinalSysSizing(AirLoopNum).EMSOverrideNonCoinHeatMassFlowOn)
                    FinalSysSizing(AirLoopNum).NonCoinHeatMassFlow = FinalSysSizing(AirLoopNum).EMSValueNonCoinHeatMassFlow;
                if (FinalSysSizing(AirLoopNum).EMSOverrideDesMainVolFlowOn)
                    FinalSysSizing(AirLoopNum).DesMainVolFlow = FinalSysSizing(AirLoopNum).EMSValueDesMainVolFlow;
                if (FinalSysSizing(AirLoopNum).EMSOverrideDesHeatVolFlowOn)
                    FinalSysSizing(AirLoopNum).DesHeatVolFlow = FinalSysSizing(AirLoopNum).EMSValueDesHeatVolFlow;
                if (FinalSysSizing(AirLoopNum).EMSOverrideDesCoolVolFlowOn)
                    FinalSysSizing(AirLoopNum).DesCoolVolFlow = FinalSysSizing(AirLoopNum).EMSValueDesCoolVolFlow;

            } // over NumPrimaryAirSys
        }

        // determine if main design is from cooling or heating
        for (AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum) {
            if (FinalSysSizing(AirLoopNum).DesMainVolFlow == FinalSysSizing(AirLoopNum).DesCoolVolFlow) {
                FinalSysSizing(AirLoopNum).sysSizeCoolingDominant = true;
            } else if (FinalSysSizing(AirLoopNum).DesMainVolFlow == FinalSysSizing(AirLoopNum).DesHeatVolFlow) {
                FinalSysSizing(AirLoopNum).sysSizeHeatingDominant = true;
            }
        }

        // write out the sys design calc results

        print(state.files.ssz, "Time");
        // static ObjexxFCL::gio::Fmt SSizeFmt11("(A1,A,A,A1,A,A,A1,A,A,A1,A,A)");
        // for ( I = 1; I <= NumPrimaryAirSys; ++I ) {
        //     { IOFlags flags; flags.ADVANCE( "No" ); ObjexxFCL::gio::write( OutputFileSysSizing, SSizeFmt11, flags ) << SizingFileColSep <<
        // CalcSysSizing( I ).AirPriLoopName << ":Des Heat Mass Flow [kg/s]" << SizingFileColSep << CalcSysSizing( I ).AirPriLoopName <<
        // ":Des Cool Mass Flow [kg/s]" << SizingFileColSep << CalcSysSizing( I ).AirPriLoopName << ":Des Heat Cap [W]" <<
        // SizingFileColSep << CalcSysSizing( I ).AirPriLoopName << ":Des Sens Cool Cap [W]"; }
        // }
        for (I = 1; I <= NumPrimaryAirSys; ++I) {
            for (J = 1; J <= state.dataEnvrn->TotDesDays + state.dataEnvrn->TotRunDesPersDays; ++J) {
                constexpr const char *SSizeFmt12("{}{}{}{:2}{}{}{}{}{:2}{}{}{}{}{:2}{}{}{}{}{:2}{}{}{}{}{:2}{}");
                print(state.files.ssz,
                      SSizeFmt12,
                      SizingFileColSep,
                      CalcSysSizing(I).AirPriLoopName,
                      ":DesPer",
                      J,
                      ":Des Heat Mass Flow [kg/s]",
                      SizingFileColSep,
                      CalcSysSizing(I).AirPriLoopName,
                      ":DesPer",
                      J,
                      ":Des Heat Cap [W]",
                      SizingFileColSep,
                      CalcSysSizing(I).AirPriLoopName,
                      ":DesPer",
                      J,
                      ":Des Cool Mass Flow [kg/s]",
                      SizingFileColSep,
                      CalcSysSizing(I).AirPriLoopName,
                      ":DesPer",
                      J,
                      ":Des Sens Cool Cap [W]",
                      SizingFileColSep,
                      CalcSysSizing(I).AirPriLoopName,
                      ":DesPer",
                      J,
                      ":Des Tot Cool Cap [W]");
            }
        }
        print(state.files.ssz, "\n");
        //      HourFrac = 0.0
        Minutes = 0;
        TimeStepIndex = 0;
        for (HourCounter = 1; HourCounter <= 24; ++HourCounter) {
            for (TimeStepCounter = 1; TimeStepCounter <= state.dataGlobal->NumOfTimeStepInHour; ++TimeStepCounter) {
                ++TimeStepIndex;
                Minutes += state.dataGlobal->MinutesPerTimeStep;
                if (Minutes == 60) {
                    Minutes = 0;
                    HourPrint = HourCounter;
                } else {
                    HourPrint = HourCounter - 1;
                }
                constexpr const char *SSizeFmt20("{:02}:{:02}:00");
                print(state.files.ssz, SSizeFmt20, HourPrint, Minutes);
                for (I = 1; I <= NumPrimaryAirSys; ++I) {
                    for (J = 1; J <= state.dataEnvrn->TotDesDays + state.dataEnvrn->TotRunDesPersDays; ++J) {
                        constexpr const char *SSizeFmt22("{}{:12.6E}{}{:12.6E}{}{:12.6E}{}{:12.6E}{}{:12.6E}");

                        print(state.files.ssz,
                              SSizeFmt22,
                              SizingFileColSep,
                              SysSizing(J, I).HeatFlowSeq(TimeStepIndex),
                              SizingFileColSep,
                              SysSizing(J, I).HeatCapSeq(TimeStepIndex),
                              SizingFileColSep,
                              SysSizing(J, I).CoolFlowSeq(TimeStepIndex),
                              SizingFileColSep,
                              SysSizing(J, I).SensCoolCapSeq(TimeStepIndex),
                              SizingFileColSep,
                              SysSizing(J, I).TotCoolCapSeq(TimeStepIndex));
                    }
                }
                print(state.files.ssz, "\n");
            }
        }

        constexpr const char *SSizeFmt31("{}{:12.6E}{}{:12.6E}{}{:12.6E}{}{:12.6E}");
        print(state.files.ssz, "Coinc Peak   ");
        for (I = 1; I <= NumPrimaryAirSys; ++I) {
            print(state.files.ssz,
                  SSizeFmt31,
                  SizingFileColSep,
                  CalcSysSizing(I).CoinHeatMassFlow,
                  SizingFileColSep,
                  CalcSysSizing(I).CoinCoolMassFlow,
                  SizingFileColSep,
                  CalcSysSizing(I).HeatCap,
                  SizingFileColSep,
                  CalcSysSizing(I).SensCoolCap);
        }
        print(state.files.ssz, "\n");

        print(state.files.ssz, "NonCoinc Peak");
        for (I = 1; I <= NumPrimaryAirSys; ++I) {
            print(state.files.ssz,
                  SSizeFmt31,
                  SizingFileColSep,
                  CalcSysSizing(I).NonCoinHeatMassFlow,
                  SizingFileColSep,
                  CalcSysSizing(I).NonCoinCoolMassFlow,
                  SizingFileColSep,
                  CalcSysSizing(I).HeatCap,
                  SizingFileColSep,
                  CalcSysSizing(I).SensCoolCap);
        }
        print(state.files.ssz, "\n");
        // have moved a big section to later in calling order, write predefined standard 62.1 report data
    } break;
    default:
        break;
    }
}

void UpdateSysSizingForScalableInputs(EnergyPlusData &state, int const AirLoopNum)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Bereket Nigusse
    //       DATE WRITTEN   Auguts 2014
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Modifies the design sizing flow rates for system scalable sizing method

    // Using/Aliasing
    using Psychrometrics::PsyCpAirFnW;
    using Psychrometrics::PsyHFnTdbW;

    // SUBROUTINE PARAMETER DEFINITIONS:

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Real64 TempSize;           // autosized value
    Real64 CoilInTemp;         // entering coil air temperature [C]
    Real64 CoilInHumRat;       // entering coil air humidity ratio [kg/kg]
    Real64 CoilInEnth;         // entering coil air enthalpy [J/kg]
    Real64 CoilOutTemp;        // coil outlet air temperature [C]
    Real64 CoilOutHumRat;      // coil outlet air humidity ratio [kg/kg]
    Real64 CoilOutEnth;        // coil outlet air enthalpy [J/kg]
    Real64 OutAirFrac;         // outdoor air fraction [-]
    Real64 CpAirStd;           // specific heat of air at standard condition
    Real64 FractionOfAutosize; // user specified autosized fraction for capacity and supply air flow
    Real64 AutosizedCapacity;  // autosized heating and cooling capacity

    auto &FinalSysSizing(state.dataSize->FinalSysSizing);
    auto &CalcSysSizing(state.dataSize->CalcSysSizing);
    auto &PrimaryAirSystems(state.dataAirSystemsData->PrimaryAirSystems);

    state.dataSize->DataFracOfAutosizedCoolingCapacity = 1.0;
    state.dataSize->DataFracOfAutosizedHeatingCapacity = 1.0;

    if (AirLoopNum > 0) {

        TempSize = 0.0;
        FractionOfAutosize = 1.0;

        // scalable sizing option for cooling supply air flow rate
        {
            auto const SELECT_CASE_var(FinalSysSizing(AirLoopNum).ScaleCoolSAFMethod);
            if (SELECT_CASE_var == FlowPerFloorArea) {
                TempSize = FinalSysSizing(AirLoopNum).FlowPerFloorAreaCooled * FinalSysSizing(AirLoopNum).FloorAreaOnAirLoopCooled;
                CalcSysSizing(AirLoopNum).InpDesCoolAirFlow = TempSize;
                FinalSysSizing(AirLoopNum).InpDesCoolAirFlow = TempSize;
            } else if (SELECT_CASE_var == FractionOfAutosizedCoolingAirflow) {
                FractionOfAutosize = FinalSysSizing(AirLoopNum).FractionOfAutosizedCoolingAirflow;
                CalcSysSizing(AirLoopNum).InpDesCoolAirFlow = CalcSysSizing(AirLoopNum).DesCoolVolFlow * FractionOfAutosize;
                FinalSysSizing(AirLoopNum).InpDesCoolAirFlow = FinalSysSizing(AirLoopNum).DesCoolVolFlow * FractionOfAutosize;
            } else if (SELECT_CASE_var == FlowPerCoolingCapacity) {
                if (FinalSysSizing(AirLoopNum).CoolingCapMethod == FractionOfAutosizedCoolingCapacity) {
                    FractionOfAutosize = FinalSysSizing(AirLoopNum).ScaledCoolingCapacity;
                    if (PrimaryAirSystems(AirLoopNum).NumOACoolCoils == 0) { // there is no precooling of the OA stream
                        CoilInTemp = FinalSysSizing(AirLoopNum).MixTempAtCoolPeak;
                        CoilInHumRat = FinalSysSizing(AirLoopNum).MixHumRatAtCoolPeak;
                    } else { // there is precooling of OA stream
                        if (FinalSysSizing(AirLoopNum).DesCoolVolFlow > 0.0) {
                            OutAirFrac = FinalSysSizing(AirLoopNum).DesOutAirVolFlow / FinalSysSizing(AirLoopNum).DesCoolVolFlow;
                        } else {
                            OutAirFrac = 1.0;
                        }
                        OutAirFrac = min(1.0, max(0.0, OutAirFrac));
                        CoilInTemp =
                            OutAirFrac * FinalSysSizing(AirLoopNum).PrecoolTemp + (1.0 - OutAirFrac) * FinalSysSizing(AirLoopNum).RetTempAtCoolPeak;
                        CoilInHumRat = OutAirFrac * FinalSysSizing(AirLoopNum).PrecoolHumRat +
                                       (1.0 - OutAirFrac) * FinalSysSizing(AirLoopNum).RetHumRatAtCoolPeak;
                    }
                    CoilOutTemp = FinalSysSizing(AirLoopNum).CoolSupTemp;
                    CoilOutHumRat = FinalSysSizing(AirLoopNum).CoolSupHumRat;
                    CoilInEnth = PsyHFnTdbW(CoilInTemp, CoilInHumRat);
                    CoilOutEnth = PsyHFnTdbW(CoilOutTemp, CoilOutHumRat);
                    AutosizedCapacity = state.dataEnvrn->StdRhoAir * FinalSysSizing(AirLoopNum).DesCoolVolFlow * (CoilInEnth - CoilOutEnth);
                    TempSize = FinalSysSizing(AirLoopNum).FlowPerCoolingCapacity * AutosizedCapacity * FractionOfAutosize;
                } else if (FinalSysSizing(AirLoopNum).CoolingCapMethod == CoolingDesignCapacity) {
                    if (FinalSysSizing(AirLoopNum).ScaledCoolingCapacity == DataSizing::AutoSize) {
                        if (PrimaryAirSystems(AirLoopNum).NumOACoolCoils == 0) { // there is no precooling of the OA stream
                            CoilInTemp = FinalSysSizing(AirLoopNum).MixTempAtCoolPeak;
                            CoilInHumRat = FinalSysSizing(AirLoopNum).MixHumRatAtCoolPeak;
                        } else { // there is precooling of OA stream
                            if (FinalSysSizing(AirLoopNum).DesCoolVolFlow > 0.0) {
                                OutAirFrac = FinalSysSizing(AirLoopNum).DesOutAirVolFlow / FinalSysSizing(AirLoopNum).DesCoolVolFlow;
                            } else {
                                OutAirFrac = 1.0;
                            }
                            OutAirFrac = min(1.0, max(0.0, OutAirFrac));
                            CoilInTemp = OutAirFrac * FinalSysSizing(AirLoopNum).PrecoolTemp +
                                         (1.0 - OutAirFrac) * FinalSysSizing(AirLoopNum).RetTempAtCoolPeak;
                            CoilInHumRat = OutAirFrac * FinalSysSizing(AirLoopNum).PrecoolHumRat +
                                           (1.0 - OutAirFrac) * FinalSysSizing(AirLoopNum).RetHumRatAtCoolPeak;
                        }
                        CoilOutTemp = FinalSysSizing(AirLoopNum).CoolSupTemp;
                        CoilOutHumRat = FinalSysSizing(AirLoopNum).CoolSupHumRat;
                        CoilInEnth = PsyHFnTdbW(CoilInTemp, CoilInHumRat);
                        CoilOutEnth = PsyHFnTdbW(CoilOutTemp, CoilOutHumRat);
                        AutosizedCapacity = state.dataEnvrn->StdRhoAir * FinalSysSizing(AirLoopNum).DesCoolVolFlow * (CoilInEnth - CoilOutEnth);
                        TempSize = FinalSysSizing(AirLoopNum).FlowPerCoolingCapacity * AutosizedCapacity * FractionOfAutosize;
                    } else {
                        TempSize = FinalSysSizing(AirLoopNum).FlowPerCoolingCapacity * FinalSysSizing(AirLoopNum).ScaledCoolingCapacity;
                    }
                } else if (FinalSysSizing(AirLoopNum).CoolingCapMethod == CapacityPerFloorArea) {
                    TempSize = FinalSysSizing(AirLoopNum).FlowPerCoolingCapacity * FinalSysSizing(AirLoopNum).ScaledCoolingCapacity *
                               FinalSysSizing(AirLoopNum).FloorAreaOnAirLoopCooled;
                }
                CalcSysSizing(AirLoopNum).InpDesCoolAirFlow = TempSize;
                FinalSysSizing(AirLoopNum).InpDesCoolAirFlow = TempSize;
            }
        }

        // scalable sizing option for heating supply air flow rate
        {
            auto const SELECT_CASE_var(FinalSysSizing(AirLoopNum).ScaleHeatSAFMethod);
            if (SELECT_CASE_var == FlowPerFloorArea) {
                TempSize = FinalSysSizing(AirLoopNum).FlowPerFloorAreaHeated * FinalSysSizing(AirLoopNum).FloorAreaOnAirLoopHeated;
                CalcSysSizing(AirLoopNum).InpDesHeatAirFlow = TempSize;
                FinalSysSizing(AirLoopNum).InpDesHeatAirFlow = TempSize;
            } else if (SELECT_CASE_var == FractionOfAutosizedHeatingAirflow) {
                FractionOfAutosize = FinalSysSizing(AirLoopNum).FractionOfAutosizedHeatingAirflow;
                CalcSysSizing(AirLoopNum).InpDesHeatAirFlow = CalcSysSizing(AirLoopNum).DesHeatVolFlow * FractionOfAutosize;
                FinalSysSizing(AirLoopNum).InpDesHeatAirFlow = FinalSysSizing(AirLoopNum).DesHeatVolFlow * FractionOfAutosize;
            } else if (SELECT_CASE_var == FractionOfAutosizedCoolingAirflow) {
                FractionOfAutosize = FinalSysSizing(AirLoopNum).FractionOfAutosizedCoolingAirflow;
                CalcSysSizing(AirLoopNum).InpDesHeatAirFlow = CalcSysSizing(AirLoopNum).DesHeatVolFlow * FractionOfAutosize;
                FinalSysSizing(AirLoopNum).InpDesHeatAirFlow = FinalSysSizing(AirLoopNum).DesHeatVolFlow * FractionOfAutosize;
            } else if (SELECT_CASE_var == FlowPerHeatingCapacity) {
                if (FinalSysSizing(AirLoopNum).HeatingCapMethod == FractionOfAutosizedHeatingCapacity) {
                    FractionOfAutosize = FinalSysSizing(AirLoopNum).ScaledHeatingCapacity;
                    if (FinalSysSizing(AirLoopNum).HeatOAOption == MinOA) {
                        if (FinalSysSizing(AirLoopNum).DesHeatVolFlow > 0.0) {
                            OutAirFrac = FinalSysSizing(AirLoopNum).DesOutAirVolFlow / FinalSysSizing(AirLoopNum).DesHeatVolFlow;
                        } else {
                            OutAirFrac = 1.0;
                        }
                        OutAirFrac = std::min(1.0, std::max(0.0, OutAirFrac));
                    } else {
                        OutAirFrac = 1.0;
                    }
                    if (state.dataSize->CurOASysNum == 0 && PrimaryAirSystems(AirLoopNum).NumOAHeatCoils > 0) {
                        CoilInTemp =
                            OutAirFrac * FinalSysSizing(AirLoopNum).PreheatTemp + (1.0 - OutAirFrac) * FinalSysSizing(AirLoopNum).HeatRetTemp;
                    } else {
                        CoilInTemp =
                            OutAirFrac * FinalSysSizing(AirLoopNum).HeatOutTemp + (1.0 - OutAirFrac) * FinalSysSizing(AirLoopNum).HeatRetTemp;
                    }
                    CoilOutTemp = FinalSysSizing(AirLoopNum).HeatSupTemp;
                    CpAirStd = PsyCpAirFnW(DataPrecisionGlobals::constant_zero);
                    AutosizedCapacity =
                        state.dataEnvrn->StdRhoAir * FinalSysSizing(AirLoopNum).DesHeatVolFlow * CpAirStd * (CoilOutTemp - CoilInTemp);
                    TempSize = FinalSysSizing(AirLoopNum).FlowPerHeatingCapacity * AutosizedCapacity * FractionOfAutosize;
                } else if (FinalSysSizing(AirLoopNum).HeatingCapMethod == HeatingDesignCapacity) {
                    if (FinalSysSizing(AirLoopNum).ScaledHeatingCapacity == DataSizing::AutoSize) {
                        if (FinalSysSizing(AirLoopNum).HeatOAOption == MinOA) {
                            if (FinalSysSizing(AirLoopNum).DesHeatVolFlow > 0.0) {
                                OutAirFrac = FinalSysSizing(AirLoopNum).DesOutAirVolFlow / FinalSysSizing(AirLoopNum).DesHeatVolFlow;
                            } else {
                                OutAirFrac = 1.0;
                            }
                            OutAirFrac = std::min(1.0, std::max(0.0, OutAirFrac));
                        } else {
                            OutAirFrac = 1.0;
                        }
                        if (state.dataSize->CurOASysNum == 0 && PrimaryAirSystems(AirLoopNum).NumOAHeatCoils > 0) {
                            CoilInTemp =
                                OutAirFrac * FinalSysSizing(AirLoopNum).PreheatTemp + (1.0 - OutAirFrac) * FinalSysSizing(AirLoopNum).HeatRetTemp;
                        } else {
                            CoilInTemp =
                                OutAirFrac * FinalSysSizing(AirLoopNum).HeatOutTemp + (1.0 - OutAirFrac) * FinalSysSizing(AirLoopNum).HeatRetTemp;
                        }
                        CoilOutTemp = FinalSysSizing(AirLoopNum).HeatSupTemp;
                        CpAirStd = PsyCpAirFnW(DataPrecisionGlobals::constant_zero);
                        AutosizedCapacity =
                            state.dataEnvrn->StdRhoAir * FinalSysSizing(AirLoopNum).DesHeatVolFlow * CpAirStd * (CoilOutTemp - CoilInTemp);
                        TempSize = FinalSysSizing(AirLoopNum).FlowPerHeatingCapacity * AutosizedCapacity * FractionOfAutosize;
                    } else {
                        TempSize = FinalSysSizing(AirLoopNum).FlowPerHeatingCapacity * FinalSysSizing(AirLoopNum).ScaledHeatingCapacity;
                    }
                } else if (FinalSysSizing(AirLoopNum).HeatingCapMethod == CapacityPerFloorArea) {
                    TempSize = FinalSysSizing(AirLoopNum).FlowPerHeatingCapacity * FinalSysSizing(AirLoopNum).ScaledHeatingCapacity *
                               FinalSysSizing(AirLoopNum).FloorAreaOnAirLoopCooled;
                }
                CalcSysSizing(AirLoopNum).InpDesHeatAirFlow = TempSize;
                FinalSysSizing(AirLoopNum).InpDesHeatAirFlow = TempSize;
            }
        }

        // save the total cooling capacity sizing data for scalable sizing
        {
            auto const SELECT_CASE_var(FinalSysSizing(AirLoopNum).CoolingCapMethod);
            if (SELECT_CASE_var == CoolingDesignCapacity) {
                if (CalcSysSizing(AirLoopNum).ScaledCoolingCapacity > 0.0) {
                    CalcSysSizing(AirLoopNum).CoolingTotalCapacity = CalcSysSizing(AirLoopNum).ScaledCoolingCapacity;
                    FinalSysSizing(AirLoopNum).CoolingTotalCapacity = CalcSysSizing(AirLoopNum).ScaledCoolingCapacity;
                } else {
                    FinalSysSizing(AirLoopNum).CoolingTotalCapacity = 0.0; // autosized, set to zero initially
                }
            } else if (SELECT_CASE_var == CapacityPerFloorArea) {
                FinalSysSizing(AirLoopNum).CoolingTotalCapacity =
                    CalcSysSizing(AirLoopNum).ScaledCoolingCapacity * FinalSysSizing(AirLoopNum).FloorAreaOnAirLoopCooled;
            } else if (SELECT_CASE_var == FractionOfAutosizedCoolingCapacity) {
                CalcSysSizing(AirLoopNum).FractionOfAutosizedCoolingCapacity = CalcSysSizing(AirLoopNum).ScaledCoolingCapacity;
                FinalSysSizing(AirLoopNum).FractionOfAutosizedCoolingCapacity = CalcSysSizing(AirLoopNum).ScaledCoolingCapacity;
            }
        }

        // save the total heating capacity sizing data for scalable sizing
        {
            auto const SELECT_CASE_var(FinalSysSizing(AirLoopNum).HeatingCapMethod);
            if (SELECT_CASE_var == HeatingDesignCapacity) {
                if (CalcSysSizing(AirLoopNum).ScaledHeatingCapacity > 0.0) {
                    FinalSysSizing(AirLoopNum).HeatingTotalCapacity = CalcSysSizing(AirLoopNum).ScaledHeatingCapacity;
                } else {
                    FinalSysSizing(AirLoopNum).HeatingTotalCapacity = 0.0; // autosized, set to zero initially
                }
            } else if (SELECT_CASE_var == CapacityPerFloorArea) {
                // even for heating capacity we use cooled zones floor area ( *.FloorAreaOnAirLoopCooled ) served by the airloop
                FinalSysSizing(AirLoopNum).HeatingTotalCapacity =
                    CalcSysSizing(AirLoopNum).ScaledHeatingCapacity * FinalSysSizing(AirLoopNum).FloorAreaOnAirLoopCooled;
            } else if (SELECT_CASE_var == FractionOfAutosizedHeatingCapacity) {
                FinalSysSizing(AirLoopNum).FractionOfAutosizedHeatingCapacity = CalcSysSizing(AirLoopNum).ScaledHeatingCapacity;
            }
        }
    }
}

Real64 GetHeatingSATempForSizing(EnergyPlusData &state, int const IndexAirLoop // air loop index
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl, Rongpeng Zhang
    //       DATE WRITTEN   October 2015
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine get the proper reheat coil inlet temperature for sizing, depending on
    // the system configurations:
    // (1) Central heating coils exist
    // (2) No central heating coils, but preheating coils or OA heat-exchangers exist
    // (3) No central heating coils; No preheating coils or OA heat-exchangers

    // Using/Aliasing
    using namespace DataSizing;
    using Psychrometrics::PsyHFnTdbW;
    using Psychrometrics::PsyTdbFnHW;

    // Locals
    Real64 ReheatCoilInTempForSizing;     // Dry bulb temperature of the reheat coil inlet air [C]
    Real64 ReheatCoilInHumRatForSizing;   // Humidity ratio of the reheat coil inlet air [kg/kg]
    Real64 ReheatCoilInEnthalpyForSizing; // Enthalpy of the reheat coil inlet air [J/kg]
    Real64 OutAirFrac;

    auto &CalcSysSizing(state.dataSize->CalcSysSizing);
    auto &FinalSysSizing(state.dataSize->FinalSysSizing);
    auto &PrimaryAirSystems(state.dataAirSystemsData->PrimaryAirSystems);

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

    if (PrimaryAirSystems(IndexAirLoop).CentralHeatCoilExists) {
        // Case: Central heating coils exist

        ReheatCoilInTempForSizing = CalcSysSizing(IndexAirLoop).HeatSupTemp;

    } else if ((PrimaryAirSystems(IndexAirLoop).NumOAHeatCoils > 0) || (PrimaryAirSystems(IndexAirLoop).NumOAHXs)) {
        // Case: No central heating coils, but preheating coils or OA heat-exchangers exist

        if (FinalSysSizing(IndexAirLoop).DesHeatVolFlow > 0) {
            OutAirFrac = FinalSysSizing(IndexAirLoop).DesOutAirVolFlow / FinalSysSizing(IndexAirLoop).DesHeatVolFlow;
            OutAirFrac = min(1.0, max(0.0, OutAirFrac));
        } else {
            OutAirFrac = 0.0;
        }

        // Mixed air humidity ratio and enthalpy
        ReheatCoilInHumRatForSizing =
            OutAirFrac * FinalSysSizing(IndexAirLoop).PreheatHumRat + (1 - OutAirFrac) * FinalSysSizing(IndexAirLoop).HeatRetHumRat;
        ReheatCoilInEnthalpyForSizing =
            OutAirFrac * PsyHFnTdbW(FinalSysSizing(IndexAirLoop).PreheatTemp, FinalSysSizing(IndexAirLoop).PreheatHumRat) +
            (1 - OutAirFrac) * PsyHFnTdbW(FinalSysSizing(IndexAirLoop).HeatRetTemp, FinalSysSizing(IndexAirLoop).HeatRetHumRat);

        // Mixed air dry bulb temperature
        ReheatCoilInTempForSizing = PsyTdbFnHW(ReheatCoilInEnthalpyForSizing, ReheatCoilInHumRatForSizing);

    } else {
        // Case: No central heating coils; No preheating coils or OA heat-exchangers

        ReheatCoilInTempForSizing = FinalSysSizing(IndexAirLoop).HeatMixTemp;
    }

    return ReheatCoilInTempForSizing;
}

Real64 GetHeatingSATempHumRatForSizing(EnergyPlusData &state, int const IndexAirLoop // air loop index
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl, Rongpeng Zhang
    //       DATE WRITTEN   October 2015
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine get the proper reheat coil inlet humidity ratio for sizing, depending on
    // the system configurations:
    // (1) Central heating coils exist
    // (2) No central heating coils, but preheating coils or OA heat-exchangers exist
    // (3) No central heating coils; No preheating coils or OA heat-exchangers

    // Using/Aliasing
    using namespace DataSizing;

    // Locals
    Real64 ReheatCoilInHumRatForSizing;
    Real64 OutAirFrac;

    auto &FinalSysSizing(state.dataSize->FinalSysSizing);
    auto &PrimaryAirSystems(state.dataAirSystemsData->PrimaryAirSystems);

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

    if (PrimaryAirSystems(IndexAirLoop).CentralHeatCoilExists) {
        // Case: Central heating coils exist

        ReheatCoilInHumRatForSizing = state.dataSize->CalcSysSizing(IndexAirLoop).HeatSupHumRat;

    } else if ((PrimaryAirSystems(IndexAirLoop).NumOAHeatCoils > 0) || (PrimaryAirSystems(IndexAirLoop).NumOAHXs)) {
        // Case: No central heating coils, but preheating coils or OA heat-exchangers exist

        if (FinalSysSizing(IndexAirLoop).DesHeatVolFlow > 0) {
            OutAirFrac = FinalSysSizing(IndexAirLoop).DesOutAirVolFlow / FinalSysSizing(IndexAirLoop).DesHeatVolFlow;
            OutAirFrac = min(1.0, max(0.0, OutAirFrac));
        } else {
            OutAirFrac = 0.0;
        }

        ReheatCoilInHumRatForSizing =
            OutAirFrac * FinalSysSizing(IndexAirLoop).PreheatHumRat + (1 - OutAirFrac) * FinalSysSizing(IndexAirLoop).HeatRetHumRat;

    } else {
        // Case: No central heating coils; No preheating coils or OA heat-exchangers

        ReheatCoilInHumRatForSizing = FinalSysSizing(IndexAirLoop).HeatMixHumRat;
    }

    return ReheatCoilInHumRatForSizing;
}

void CheckWaterCoilIsOnAirLoop(EnergyPlusData &state,
                               SimAirServingZones::CompType const CompTypeNum,
                               std::string const &CompType,
                               std::string const &CompName,
                               bool &WaterCoilOnAirLoop)
{
    // PURPOSE OF THIS FUNCTION:
    // This function returns true if a water coil that has water controller is either on
    // primary air or outdoor air system branch. Searches for water coil name and type
    // that match components list in primary air and outside air systems.

    // Return value
    bool CheckWaterCoilIsOnAirLoop(false);

    CheckWaterCoilIsOnAirLoop = CheckWaterCoilOnPrimaryAirLoopBranch(state, CompTypeNum, CompName);
    if (!CheckWaterCoilIsOnAirLoop) {
        CheckWaterCoilIsOnAirLoop = CheckWaterCoilOnOASystem(state, CompTypeNum, CompName);
    }

    if (!CheckWaterCoilIsOnAirLoop) {
        CheckWaterCoilIsOnAirLoop = CheckWaterCoilSystemOnAirLoopOrOASystem(state, CompTypeNum, CompName);
    }
    if (!CheckWaterCoilIsOnAirLoop) {
        ShowSevereError(state, "CheckWaterCoilIsOnAirLoop: = " + CompType + " = " + CompName + ".");
        ShowContinueError(state,
                          "The water coil or coil system is neither on primary air branch nor on outdoor air system hence does not require "
                          "'Controller:WaterCoil' object.");
    }
    WaterCoilOnAirLoop = CheckWaterCoilIsOnAirLoop;
}

bool CheckWaterCoilOnPrimaryAirLoopBranch(EnergyPlusData &state, SimAirServingZones::CompType const CompTypeNum, std::string const &CompName)
{
    // PURPOSE OF THIS FUNCTION:
    // This function returns true if a water coil that has water controller is on
    // primary air loop branch. Searches for water coil name and type that match
    // components list in primary air systems.

    auto &PrimaryAirSystems(state.dataAirSystemsData->PrimaryAirSystems);

    if (state.dataSimAirServingZones->GetAirLoopInputFlag) { // First time subroutine has been entered
        GetAirPathData(state);                               // Get air loop descriptions from input file
        state.dataSimAirServingZones->GetAirLoopInputFlag = false;
    }

    if (state.dataHVACGlobal->NumPrimaryAirSys > 0) {
        for (int AirSysNum = 1; AirSysNum <= state.dataHVACGlobal->NumPrimaryAirSys; ++AirSysNum) {
            for (int BranchNum = 1; BranchNum <= PrimaryAirSystems(AirSysNum).NumBranches; ++BranchNum) {
                for (int CompNum = 1; CompNum <= PrimaryAirSystems(AirSysNum).Branch(BranchNum).TotalComponents; ++CompNum) {
                    if ((CompTypeNum == PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).CompType_Num) &&
                        UtilityRoutines::SameString(CompName, PrimaryAirSystems(AirSysNum).Branch(BranchNum).Comp(CompNum).Name)) {
                        return true;
                    }
                }
            }
        }
    }
    return false;
}

bool CheckWaterCoilOnOASystem(EnergyPlusData &state, SimAirServingZones::CompType const CompTypeNum, std::string const &CompName)
{
    // PURPOSE OF THIS FUNCTION:
    // This function returns true if a water coil that has water controller is on
    // outdoor air system. Searches for water coil name and type that match
    // components list on outside air systems.

    // USE STATEMENTS:
    using MixedAir::GetNumOASystems;
    using MixedAir::GetOutsideAirSysInputs;

    auto &OutsideAirSys(state.dataAirLoop->OutsideAirSys);

    if (state.dataMixedAir->GetOASysInputFlag) {
        GetOutsideAirSysInputs(state);
        state.dataMixedAir->GetOASysInputFlag = false;
    }
    int NumOASys = GetNumOASystems(state);
    if (NumOASys > 0) {
        for (int OASysNum = 1; OASysNum <= NumOASys; ++OASysNum) {
            for (int OACompNum = 1; OACompNum <= OutsideAirSys(OASysNum).NumComponents; ++OACompNum) {
                if ((CompTypeNum == OutsideAirSys(OASysNum).ComponentTypeEnum(OACompNum)) &&
                    (UtilityRoutines::SameString(CompName, OutsideAirSys(OASysNum).ComponentName(OACompNum)))) {
                    return true;
                }
            }
        }
    }
    return false;
}

bool CheckWaterCoilSystemOnAirLoopOrOASystem(EnergyPlusData &state, SimAirServingZones::CompType const CompTypeNum, std::string const &CompName)
{
    // PURPOSE OF THIS FUNCTION:
    // This function returns true if a water coil which is part of CoilSystem:Cooling:Water:HeatExchangerAssisted
    // and that has water controller is on primary air loop branch or outdoor air system. Searches for water coilsystem
    // type and name that match components list in primary air loop or outside air systems.

    // USE STATEMENTS:
    using HVACHXAssistedCoolingCoil::GetHXAssistedCoolingCoilInput;

    // Return value
    bool CheckWaterCoilSystemIsOnAirLoopOASystem(false);

    if (state.dataHVACAssistedCC->GetCoilsInputFlag) {
        // Get the HXAssistedCoolingCoil input
        GetHXAssistedCoolingCoilInput(state);
        state.dataHVACAssistedCC->GetCoilsInputFlag = false;
    }

    bool WaterCoilIsOnWaterCoilSystem = false;
    std::string CoilSystemName = CompName;
    CompType CoilSystemTypeNum = CompTypeNum;

    if (state.dataHVACAssistedCC->TotalNumHXAssistedCoils > 0) {
        // check if the water coil is placed on 'CoilSystem:Cooling:Water:HeatExchangerAssisted' object
        for (int HXASSCoilNum = 1; HXASSCoilNum <= state.dataHVACAssistedCC->TotalNumHXAssistedCoils; ++HXASSCoilNum) {
            std::string CompType = state.dataHVACAssistedCC->HXAssistedCoil(HXASSCoilNum).CoolingCoilType;
            if ((UtilityRoutines::SameString(CompType, "Coil:Cooling:Water") ||
                 UtilityRoutines::SameString(CompType, "Coil:Cooling:Water:DetailedGeometry")) &&
                UtilityRoutines::SameString(CompName, state.dataHVACAssistedCC->HXAssistedCoil(HXASSCoilNum).CoolingCoilName)) {
                CoilSystemName = state.dataHVACAssistedCC->HXAssistedCoil(HXASSCoilNum).Name;
                CoilSystemTypeNum = SimAirServingZones::CompType::WaterCoil_CoolingHXAsst;
                WaterCoilIsOnWaterCoilSystem = true;
                break;
            }
        }
    }

    // check if the CoilSystem object that contains the water coil is placed on air loop branch or OA system
    if (WaterCoilIsOnWaterCoilSystem) {
        CheckWaterCoilSystemIsOnAirLoopOASystem = CheckWaterCoilOnPrimaryAirLoopBranch(state, CoilSystemTypeNum, CoilSystemName);
        if (!CheckWaterCoilSystemIsOnAirLoopOASystem) {
            CheckWaterCoilSystemIsOnAirLoopOASystem = CheckWaterCoilOnOASystem(state, CoilSystemTypeNum, CoilSystemName);
        }
    }
    return CheckWaterCoilSystemIsOnAirLoopOASystem;
}
// namespace SimAirServingZones

// End Algorithm Section of the Module
// *****************************************************************************

// Beginning of Reporting subroutines for the SimAir Module
// *****************************************************************************

//        End of Reporting subroutines for the SimAir Module
// *****************************************************************************

//        Utility Subroutines for the SimAir Module
// *****************************************************************************

void LimitZoneVentEff(EnergyPlusData &state,
                      Real64 Xs,               // ratio of uncorrected system outdoor air flow rate to the design system supply flow rate
                      Real64 Voz,              // corrected (divided by distribution efficiency) zone outside air flow rate [m3/s]
                      int TermUnitSizingIndex, // terminal unit sizing index
                      Real64 &SystemCoolingEv  // system ventilation efficiency
)
{
    // FUNCTION INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   November 2015
    //       MODIFIED
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // Check that system ventilation eff is not less than input minimum system ventilation efficiency.
    // If it is, back calculate and reset ZpzClgByZone and DesCoolVolFlowMin and system ventilation efficiency
    // Also increase DesCoolVolFlow if needed to match the new DesCoolVolFlowMin
    // Why does this look only at cooling?  Shouldn't heating also be checked?

    // METHODOLOGY EMPLOYED:
    // Ventilation Rate Procedure for single pass system

    // Using/Aliasing
    using namespace OutputReportPredefined;

    auto &TermUnitFinalZoneSizing(state.dataSize->TermUnitFinalZoneSizing);

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    Real64 ZoneOAFrac(0.0);  // ratio of Voz to available zone supply air flow
    Real64 AvailSAFlow(0.0); // available zone supply air flow [m3/s]

    if (SystemCoolingEv < TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneVentilationEff) {
        // reset ZoneOAFrac
        ZoneOAFrac = 1.0 + Xs - TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneVentilationEff;
        // reset AvailSAFlow (which in this case is minimum cooling supply air flow rate)
        AvailSAFlow = Voz / ZoneOAFrac;
        // save ZoneOAFrac
        TermUnitFinalZoneSizing(TermUnitSizingIndex).ZpzClgByZone = ZoneOAFrac;
        // save new (increased) minimum flow rate
        TermUnitFinalZoneSizing(TermUnitSizingIndex).DesCoolVolFlowMin = AvailSAFlow;
        // make sure the max flow rate is >= the new minimum flow rate
        TermUnitFinalZoneSizing(TermUnitSizingIndex).DesCoolVolFlow = max(AvailSAFlow, TermUnitFinalZoneSizing(TermUnitSizingIndex).DesCoolVolFlow);
        // set the system ventilation efficiency to the user specified minimum
        SystemCoolingEv = TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneVentilationEff;

        // Vpz: "Primary" supply air from main air handler served by an oa mixer
        Real64 VpzClgByZone = TermUnitFinalZoneSizing(TermUnitSizingIndex).DesCoolVolFlow;

        // Vdz: "Discharge" supply air delivered to zone by terminal unit
        Real64 VdzClgByZone = 0.0;
        // Taken from similar section in SetUpSysSizingArrays
        if (TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneSecondaryRecirculation > 0.0) { // multi-path system
            VdzClgByZone = max(state.dataSize->TermUnitSizing(TermUnitSizingIndex).AirVolFlow, VpzClgByZone);
        } else { // single path system
            VdzClgByZone = TermUnitFinalZoneSizing(TermUnitSizingIndex).DesCoolVolFlow;
        }

        // Update VRP table entries:
        PreDefTableEntry(
            state, state.dataOutRptPredefined->pdchS62zcdVpz, TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneName, VpzClgByZone, 4); // Vpz
        PreDefTableEntry(
            state, state.dataOutRptPredefined->pdchS62zcdVdz, TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneName, VdzClgByZone, 4); // Vdz
        PreDefTableEntry(state,
                         state.dataOutRptPredefined->pdchS62zcdVpzmin,
                         TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneName,
                         TermUnitFinalZoneSizing(TermUnitSizingIndex).DesCoolVolFlowMin,
                         4); // Vpz-min
        PreDefTableEntry(state,
                         state.dataOutRptPredefined->pdchS62zcdZpz,
                         TermUnitFinalZoneSizing(TermUnitSizingIndex).ZoneName,
                         TermUnitFinalZoneSizing(TermUnitSizingIndex).ZpzClgByZone,
                         3); // Zpz = Voz/Vpz        }
    }
}

//        End of Utility subroutines for the SimAir Module
// *****************************************************************************

} // namespace EnergyPlus::SimAirServingZones
