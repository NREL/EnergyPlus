// EnergyPlus, Copyright (c) 1996-2021, The Board of Trustees of the University of Illinois,
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
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataContaminantBalance.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataErrorTracking.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus::NodeInputManager {

// MODULE INFORMATION:
//       AUTHOR         Linda K. Lawrie
//       DATE WRITTEN   September 1999
//       MODIFIED       na
//       RE-ENGINEERED  na

// PURPOSE OF THIS MODULE:
// To provide utilities for reading and assigning indices for the
// nodes in the HVAC loops.

using namespace DataLoopNode;
using namespace BranchNodeConnections;

constexpr const char *fluidNameSteam("STEAM");

void GetNodeNums(EnergyPlusData &state,
                 std::string const &Name,                                   // Name for which to obtain information
                 int &NumNodes,                                             // Number of nodes accompanying this Name
                 Array1D_int &NodeNumbers,                                  // Node Numbers accompanying this Name
                 bool &ErrorsFound,                                         // True when errors are found...
                 DataLoopNode::NodeFluidType NodeFluidType,                 // Fluidtype for checking/setting node FluidType
                 std::string const &NodeObjectType,                         // Node Object Type (i.e. "Chiller:Electric")
                 std::string const &NodeObjectName,                         // Node Object Name (i.e. "MyChiller")
                 DataLoopNode::NodeConnectionType const NodeConnectionType, // Node Connection Type (see DataLoopNode)
                 compFluidStream const NodeFluidStream,                     // Which Fluid Stream (1,2,3,...)
                 bool const ObjectIsParent,                                 // True/False
                 Optional_bool_const IncrementFluidStream,                  // True/False
                 Optional_string_const InputFieldName                       // Input Field Name
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   September 1999
    //       MODIFIED       February 2004, Fluid Type checking/setting
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine calls the Node Manager to determine if the
    // entered name has already been assigned and if it is a list
    // or if it is a single node.  If it has not been assigned, then
    // it is a single node and will need to be entered in the Node
    // data structure.

    // SUBROUTINE PARAMETER DEFINITIONS:
    static std::string const RoutineName("GetNodeNums: ");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int ThisOne; // Indicator for this Name
    std::string ConnectionType;
    int Loop;
    NodeInputManager::compFluidStream FluidStreamNum; // Fluid stream number passed to RegisterNodeConnection

    if (state.dataNodeInputMgr->GetNodeInputFlag) {
        GetNodeListsInput(state, ErrorsFound);
        state.dataNodeInputMgr->GetNodeInputFlag = false;
    }

    if (NodeFluidType != DataLoopNode::NodeFluidType::Air && NodeFluidType != DataLoopNode::NodeFluidType::Water &&
        NodeFluidType != DataLoopNode::NodeFluidType::Electric && NodeFluidType != DataLoopNode::NodeFluidType::Steam &&
        NodeFluidType != DataLoopNode::NodeFluidType::blank) {
        ShowSevereError(state, RoutineName + NodeObjectType + "=\"" + NodeObjectName + "\", invalid fluid type.");
        ShowContinueError(state, format("..Invalid FluidType={}", NodeFluidType));
        ErrorsFound = true;
        ShowFatalError(state, "Preceding issue causes termination.");
    }

    if (not_blank(Name)) {
        ThisOne = UtilityRoutines::FindItemInList(Name, state.dataNodeInputMgr->NodeLists);
        if (ThisOne != 0) {
            NumNodes = state.dataNodeInputMgr->NodeLists(ThisOne).NumOfNodesInList;
            NodeNumbers({1, NumNodes}) = state.dataNodeInputMgr->NodeLists(ThisOne).NodeNumbers({1, NumNodes});
            for (Loop = 1; Loop <= NumNodes; ++Loop) {
                if (NodeFluidType != DataLoopNode::NodeFluidType::blank &&
                    state.dataLoopNodes->Node(NodeNumbers(Loop)).FluidType != DataLoopNode::NodeFluidType::blank) {
                    if (state.dataLoopNodes->Node(NodeNumbers(Loop)).FluidType != NodeFluidType) {
                        ShowSevereError(state, RoutineName + NodeObjectType + "=\"" + NodeObjectName + "\", invalid data.");
                        if (present(InputFieldName)) ShowContinueError(state, "...Ref field=" + InputFieldName);
                        ShowContinueError(
                            state, "Existing Fluid type for node, incorrect for request. Node=" + state.dataLoopNodes->NodeID(NodeNumbers(Loop)));
                        ShowContinueError(
                            state,
                            "Existing Fluid type=" +
                                format("{}", DataLoopNode::ValidNodeFluidTypes(state.dataLoopNodes->Node(NodeNumbers(Loop)).FluidType)) +
                                ", Requested Fluid Type=" + format("{}", DataLoopNode::ValidNodeFluidTypes(NodeFluidType)));
                        ErrorsFound = true;
                    }
                }
                if (state.dataLoopNodes->Node(NodeNumbers(Loop)).FluidType == DataLoopNode::NodeFluidType::blank) {
                    state.dataLoopNodes->Node(NodeNumbers(Loop)).FluidType = NodeFluidType;
                }
                ++state.dataNodeInputMgr->NodeRef(NodeNumbers(Loop));
            }
        } else {
            ThisOne = AssignNodeNumber(state, Name, NodeFluidType, ErrorsFound);
            NumNodes = 1;
            NodeNumbers(1) = ThisOne;
        }
    } else {
        NumNodes = 0;
        NodeNumbers(1) = 0;
    }

    // Most calls to this routine use a fixed fluid stream number for all nodes, this is the default
    FluidStreamNum = NodeFluidStream;
    for (Loop = 1; Loop <= NumNodes; ++Loop) {
        if (static_cast<int>(NodeConnectionType) >= 1 && static_cast<int>(NodeConnectionType) <= NumValidConnectionTypes) {
            ConnectionType = DataLoopNode::ValidConnectionTypes(NodeConnectionType);
        } else {
            ConnectionType = format("{}-unknown", NodeConnectionType);
        }
        // If requested, assign NodeFluidStream to the first node and increment the fluid stream number
        // for each remaining node in the list
        if (present(IncrementFluidStream)) {
            if (IncrementFluidStream) FluidStreamNum = static_cast<NodeInputManager::compFluidStream>(static_cast<int>(NodeFluidStream) + (Loop - 1));
        }
        RegisterNodeConnection(state,
                               NodeNumbers(Loop),
                               state.dataLoopNodes->NodeID(NodeNumbers(Loop)),
                               NodeObjectType,
                               NodeObjectName,
                               ConnectionType,
                               FluidStreamNum,
                               ObjectIsParent,
                               ErrorsFound,
                               InputFieldName);
    }
}

void SetupNodeVarsForReporting(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   September
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine is called when the indicated number of
    // Nodes have been found (TOTAL NODE NUMBER) or when HVAC warmup is
    // complete, whichever condition is reached first.

    auto &Node(state.dataLoopNodes->Node);
    auto &NodeID(state.dataLoopNodes->NodeID);

    if (!state.dataNodeInputMgr->NodeVarsSetup) {
        if (!state.dataErrTracking->AbortProcessing) {
            state.dataLoopNodes->MoreNodeInfo.allocate(state.dataNodeInputMgr->NumOfUniqueNodeNames);
            for (int NumNode = 1; NumNode <= state.dataNodeInputMgr->NumOfUniqueNodeNames; ++NumNode) {
                // Setup Report variables for the Nodes for HVAC Reporting, CurrentModuleObject='Node Name'
                SetupOutputVariable(state,
                                    "System Node Temperature",
                                    OutputProcessor::Unit::C,
                                    state.dataLoopNodes->Node(NumNode).Temp,
                                    "System",
                                    "Average",
                                    NodeID(NumNode));
                SetupOutputVariable(state,
                                    "System Node Mass Flow Rate",
                                    OutputProcessor::Unit::kg_s,
                                    Node(NumNode).MassFlowRate,
                                    "System",
                                    "Average",
                                    NodeID(NumNode));
                SetupOutputVariable(state,
                                    "System Node Humidity Ratio",
                                    OutputProcessor::Unit::kgWater_kgDryAir,
                                    Node(NumNode).HumRat,
                                    "System",
                                    "Average",
                                    NodeID(NumNode));
                SetupOutputVariable(state,
                                    "System Node Setpoint Temperature",
                                    OutputProcessor::Unit::C,
                                    Node(NumNode).TempSetPoint,
                                    "System",
                                    "Average",
                                    NodeID(NumNode));
                SetupOutputVariable(state,
                                    "System Node Setpoint High Temperature",
                                    OutputProcessor::Unit::C,
                                    Node(NumNode).TempSetPointHi,
                                    "System",
                                    "Average",
                                    NodeID(NumNode));
                SetupOutputVariable(state,
                                    "System Node Setpoint Low Temperature",
                                    OutputProcessor::Unit::C,
                                    Node(NumNode).TempSetPointLo,
                                    "System",
                                    "Average",
                                    NodeID(NumNode));
                SetupOutputVariable(state,
                                    "System Node Setpoint Humidity Ratio",
                                    OutputProcessor::Unit::kgWater_kgDryAir,
                                    Node(NumNode).HumRatSetPoint,
                                    "System",
                                    "Average",
                                    NodeID(NumNode));
                SetupOutputVariable(state,
                                    "System Node Setpoint Minimum Humidity Ratio",
                                    OutputProcessor::Unit::kgWater_kgDryAir,
                                    Node(NumNode).HumRatMin,
                                    "System",
                                    "Average",
                                    NodeID(NumNode));
                SetupOutputVariable(state,
                                    "System Node Setpoint Maximum Humidity Ratio",
                                    OutputProcessor::Unit::kgWater_kgDryAir,
                                    Node(NumNode).HumRatMax,
                                    "System",
                                    "Average",
                                    NodeID(NumNode));
                SetupOutputVariable(state,
                                    "System Node Relative Humidity",
                                    OutputProcessor::Unit::Perc,
                                    state.dataLoopNodes->MoreNodeInfo(NumNode).RelHumidity,
                                    "System",
                                    "Average",
                                    NodeID(NumNode));
                SetupOutputVariable(
                    state, "System Node Pressure", OutputProcessor::Unit::Pa, Node(NumNode).Press, "System", "Average", NodeID(NumNode));
                SetupOutputVariable(state,
                                    "System Node Standard Density Volume Flow Rate",
                                    OutputProcessor::Unit::m3_s,
                                    state.dataLoopNodes->MoreNodeInfo(NumNode).VolFlowRateStdRho,
                                    "System",
                                    "Average",
                                    NodeID(NumNode));
                if (Node(NumNode).FluidType == DataLoopNode::NodeFluidType::Air ||
                    Node(NumNode).FluidType == DataLoopNode::NodeFluidType::Water) { // setup volume flow rate report for actual/current density
                    SetupOutputVariable(state,
                                        "System Node Current Density Volume Flow Rate",
                                        OutputProcessor::Unit::m3_s,
                                        state.dataLoopNodes->MoreNodeInfo(NumNode).VolFlowRateCrntRho,
                                        "System",
                                        "Average",
                                        NodeID(NumNode));
                    SetupOutputVariable(state,
                                        "System Node Current Density",
                                        OutputProcessor::Unit::kg_m3,
                                        state.dataLoopNodes->MoreNodeInfo(NumNode).Density,
                                        "System",
                                        "Average",
                                        NodeID(NumNode));
                    SetupOutputVariable(state,
                                        "System Node Specific Heat",
                                        OutputProcessor::Unit::J_kgK,
                                        state.dataLoopNodes->MoreNodeInfo(NumNode).SpecificHeat,
                                        "System",
                                        "Average",
                                        NodeID(NumNode));
                }

                SetupOutputVariable(state,
                                    "System Node Enthalpy",
                                    OutputProcessor::Unit::J_kg,
                                    state.dataLoopNodes->MoreNodeInfo(NumNode).ReportEnthalpy,
                                    "System",
                                    "Average",
                                    NodeID(NumNode));
                SetupOutputVariable(state,
                                    "System Node Wetbulb Temperature",
                                    OutputProcessor::Unit::C,
                                    state.dataLoopNodes->MoreNodeInfo(NumNode).WetBulbTemp,
                                    "System",
                                    "Average",
                                    NodeID(NumNode));
                SetupOutputVariable(state,
                                    "System Node Dewpoint Temperature",
                                    OutputProcessor::Unit::C,
                                    state.dataLoopNodes->MoreNodeInfo(NumNode).AirDewPointTemp,
                                    "System",
                                    "Average",
                                    NodeID(NumNode));
                SetupOutputVariable(
                    state, "System Node Wind Speed", OutputProcessor::Unit::m_s, Node(NumNode).OutAirWindSpeed, "System", "Average", NodeID(NumNode));
                SetupOutputVariable(state,
                                    "System Node Wind Direction",
                                    OutputProcessor::Unit::deg,
                                    Node(NumNode).OutAirWindDir,
                                    "System",
                                    "Average",
                                    NodeID(NumNode));
                SetupOutputVariable(
                    state, "System Node Quality", OutputProcessor::Unit::None, Node(NumNode).Quality, "System", "Average", NodeID(NumNode));
                SetupOutputVariable(
                    state, "System Node Height", OutputProcessor::Unit::m, Node(NumNode).Height, "System", "Average", NodeID(NumNode));
                if (state.dataGlobal->DisplayAdvancedReportVariables) {
                    SetupOutputVariable(state,
                                        "System Node Minimum Temperature",
                                        OutputProcessor::Unit::C,
                                        Node(NumNode).TempMin,
                                        "System",
                                        "Average",
                                        NodeID(NumNode));
                    SetupOutputVariable(state,
                                        "System Node Maximum Temperature",
                                        OutputProcessor::Unit::C,
                                        Node(NumNode).TempMax,
                                        "System",
                                        "Average",
                                        NodeID(NumNode));
                    SetupOutputVariable(state,
                                        "System Node Minimum Limit Mass Flow Rate",
                                        OutputProcessor::Unit::kg_s,
                                        Node(NumNode).MassFlowRateMin,
                                        "System",
                                        "Average",
                                        NodeID(NumNode));
                    SetupOutputVariable(state,
                                        "System Node Maximum Limit Mass Flow Rate",
                                        OutputProcessor::Unit::kg_s,
                                        Node(NumNode).MassFlowRateMax,
                                        "System",
                                        "Average",
                                        NodeID(NumNode));
                    SetupOutputVariable(state,
                                        "System Node Minimum Available Mass Flow Rate",
                                        OutputProcessor::Unit::kg_s,
                                        Node(NumNode).MassFlowRateMinAvail,
                                        "System",
                                        "Average",
                                        NodeID(NumNode));
                    SetupOutputVariable(state,
                                        "System Node Maximum Available Mass Flow Rate",
                                        OutputProcessor::Unit::kg_s,
                                        Node(NumNode).MassFlowRateMaxAvail,
                                        "System",
                                        "Average",
                                        NodeID(NumNode));
                    SetupOutputVariable(state,
                                        "System Node Setpoint Mass Flow Rate",
                                        OutputProcessor::Unit::kg_s,
                                        Node(NumNode).MassFlowRateSetPoint,
                                        "System",
                                        "Average",
                                        NodeID(NumNode));
                    SetupOutputVariable(state,
                                        "System Node Requested Mass Flow Rate",
                                        OutputProcessor::Unit::kg_s,
                                        Node(NumNode).MassFlowRateRequest,
                                        "System",
                                        "Average",
                                        NodeID(NumNode));
                    SetupOutputVariable(state,
                                        "System Node Last Timestep Temperature",
                                        OutputProcessor::Unit::C,
                                        Node(NumNode).TempLastTimestep,
                                        "System",
                                        "Average",
                                        NodeID(NumNode));
                    SetupOutputVariable(state,
                                        "System Node Last Timestep Enthalpy",
                                        OutputProcessor::Unit::J_kg,
                                        Node(NumNode).EnthalpyLastTimestep,
                                        "System",
                                        "Average",
                                        NodeID(NumNode));
                }
                if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                    SetupOutputVariable(
                        state, "System Node CO2 Concentration", OutputProcessor::Unit::ppm, Node(NumNode).CO2, "System", "Average", NodeID(NumNode));
                }
                if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                    SetupOutputVariable(state,
                                        "System Node Generic Air Contaminant Concentration",
                                        OutputProcessor::Unit::ppm,
                                        Node(NumNode).GenContam,
                                        "System",
                                        "Average",
                                        NodeID(NumNode));
                }
            }
        }
        state.dataNodeInputMgr->NodeVarsSetup = true;

        print(state.files.bnd, "{}\n", "! This file shows details about the branches, nodes, and other");
        print(state.files.bnd, "{}\n", "! elements of the flow connections.");
        print(state.files.bnd, "{}\n", "! This file is intended for use in \"debugging\" potential problems");
        print(state.files.bnd, "{}\n", "! that may also be detected by the program, but may be more easily");
        print(state.files.bnd, "{}\n", "! identified by \"eye\".");
        print(state.files.bnd, "{}\n", "! This file is also intended to support software which draws a");
        print(state.files.bnd, "{}\n", "! schematic diagram of the HVAC system.");
        print(state.files.bnd, "{}\n", "! ===============================================================");
        // Show the node names on the Branch-Node Details file
        static constexpr auto Format_700("! #Nodes,<Number of Unique Nodes>");
        print(state.files.bnd, "{}\n", Format_700);
        print(state.files.bnd, " #Nodes,{}\n", state.dataNodeInputMgr->NumOfUniqueNodeNames);
        if (state.dataNodeInputMgr->NumOfUniqueNodeNames > 0) {
            static constexpr auto Format_702("! <Node>,<NodeNumber>,<Node Name>,<Node Fluid Type>,<# Times Node Referenced After Definition>");
            print(state.files.bnd, "{}\n", Format_702);
        }
        int Count0 = 0;
        for (int NumNode = 1; NumNode <= state.dataNodeInputMgr->NumOfUniqueNodeNames; ++NumNode) {
            print(state.files.bnd,
                  " Node,{},{},{},{}\n",
                  NumNode,
                  NodeID(NumNode),
                  DataLoopNode::ValidNodeFluidTypes(Node(NumNode).FluidType),
                  state.dataNodeInputMgr->NodeRef(NumNode));
            if (state.dataNodeInputMgr->NodeRef(NumNode) == 0) ++Count0;
        }
        // Show suspicious node names on the Branch-Node Details file
        if (Count0 > 0) {
            print(state.files.bnd, "{}\n", "! ===============================================================");
            print(state.files.bnd, "{}\n", "! Suspicious nodes have 0 references.  It is normal for some nodes, however.");
            print(state.files.bnd, "{}\n", "! Listing nodes with 0 references (culled from previous list):");
            static constexpr auto Format_703(
                "! <Suspicious Node>,<NodeNumber>,<Node Name>,<Node Fluid Type>,<# Times Node Referenced After Definition>");
            print(state.files.bnd, "{}\n", Format_703);
            for (int NumNode = 1; NumNode <= state.dataNodeInputMgr->NumOfUniqueNodeNames; ++NumNode) {
                if (state.dataNodeInputMgr->NodeRef(NumNode) > 0) continue;
                print(state.files.bnd,
                      " Suspicious Node,{},{},{},{}\n",
                      NumNode,
                      NodeID(NumNode),
                      DataLoopNode::ValidNodeFluidTypes(Node(NumNode).FluidType),
                      state.dataNodeInputMgr->NodeRef(NumNode));
            }
        }
    }
}

void GetNodeListsInput(EnergyPlusData &state, bool &ErrorsFound) // Set to true when requested Node List not found, unchanged otherwise
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   September 1999
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine gets the Node Lists from the IDF and fills the
    // Node List Data Structure.

    // SUBROUTINE PARAMETER DEFINITIONS:
    static std::string const RoutineName("GetNodeListsInput: ");
    static std::string const CurrentModuleObject("NodeList");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int Loop;       // Loop Variable
    int Loop1;      // Loop Variable
    int Loop2;      // Loop Variable
    int NumAlphas;  // Number of alphas in IDF item
    int NumNumbers; // Number of numerics in IDF item
    int IOStatus;   // IOStatus for IDF item (not checked)
    int NCount;     // Actual number of node lists
    bool flagError; // true when error node list name should be output
    Array1D_string cAlphas;
    Array1D<Real64> rNumbers;

    bool localErrorsFound(false);
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, CurrentModuleObject, NCount, NumAlphas, NumNumbers);
    cAlphas.allocate(NumAlphas);
    rNumbers.allocate(NumNumbers);
    state.dataNodeInputMgr->NumOfNodeLists = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
    state.dataNodeInputMgr->NodeLists.allocate(state.dataNodeInputMgr->NumOfNodeLists);
    for (int i = 1; i <= state.dataNodeInputMgr->NumOfNodeLists; ++i) {
        state.dataNodeInputMgr->NodeLists(i).Name.clear();
        state.dataNodeInputMgr->NodeLists(i).NumOfNodesInList = 0;
    }

    NCount = 0;
    for (Loop = 1; Loop <= state.dataNodeInputMgr->NumOfNodeLists; ++Loop) {
        state.dataInputProcessing->inputProcessor->getObjectItem(
            state, CurrentModuleObject, Loop, cAlphas, NumAlphas, rNumbers, NumNumbers, IOStatus);
        if (UtilityRoutines::IsNameEmpty(state, cAlphas(1), CurrentModuleObject, localErrorsFound)) continue;

        ++NCount;
        state.dataNodeInputMgr->NodeLists(NCount).Name = cAlphas(1);
        state.dataNodeInputMgr->NodeLists(NCount).NodeNames.allocate(NumAlphas - 1);
        state.dataNodeInputMgr->NodeLists(NCount).NodeNames = "";
        state.dataNodeInputMgr->NodeLists(NCount).NodeNumbers.allocate(NumAlphas - 1);
        state.dataNodeInputMgr->NodeLists(NCount).NodeNumbers = 0;
        state.dataNodeInputMgr->NodeLists(NCount).NumOfNodesInList = NumAlphas - 1;
        if (NumAlphas <= 1) {
            if (NumAlphas == 1) {
                ShowSevereError(state, RoutineName + CurrentModuleObject + "=\"" + cAlphas(1) + "\" does not have any nodes.");
            } else {
                ShowSevereError(state, RoutineName + CurrentModuleObject + "=<blank> does not have any nodes or nodelist name.");
            }
            localErrorsFound = true;
            continue;
        }
        //  Put all in, then determine unique
        for (Loop1 = 1; Loop1 <= NumAlphas - 1; ++Loop1) {
            state.dataNodeInputMgr->NodeLists(NCount).NodeNames(Loop1) = cAlphas(Loop1 + 1);
            if (cAlphas(Loop1 + 1).empty()) {
                ShowWarningError(state, RoutineName + CurrentModuleObject + "=\"" + cAlphas(1) + "\", blank node name in list.");
                --state.dataNodeInputMgr->NodeLists(NCount).NumOfNodesInList;
                if (state.dataNodeInputMgr->NodeLists(NCount).NumOfNodesInList <= 0) {
                    ShowSevereError(state, RoutineName + CurrentModuleObject + "=\"" + cAlphas(1) + "\" does not have any nodes.");
                    localErrorsFound = true;
                    break;
                }
                continue;
            }
            state.dataNodeInputMgr->NodeLists(NCount).NodeNumbers(Loop1) = AssignNodeNumber(
                state, state.dataNodeInputMgr->NodeLists(NCount).NodeNames(Loop1), DataLoopNode::NodeFluidType::blank, localErrorsFound);
            if (UtilityRoutines::SameString(state.dataNodeInputMgr->NodeLists(NCount).NodeNames(Loop1),
                                            state.dataNodeInputMgr->NodeLists(NCount).Name)) {
                ShowSevereError(state, RoutineName + CurrentModuleObject + "=\"" + cAlphas(1) + "\", invalid node name in list.");
                ShowContinueError(state, format("... Node {} Name=\"{}\", duplicates NodeList Name.", Loop1, cAlphas(Loop1 + 1)));
                localErrorsFound = true;
            }
        }
        // Error on any duplicates
        flagError = true;
        for (Loop1 = 1; Loop1 <= state.dataNodeInputMgr->NodeLists(NCount).NumOfNodesInList; ++Loop1) {
            for (Loop2 = Loop1 + 1; Loop2 <= state.dataNodeInputMgr->NodeLists(NCount).NumOfNodesInList; ++Loop2) {
                if (state.dataNodeInputMgr->NodeLists(NCount).NodeNumbers(Loop1) != state.dataNodeInputMgr->NodeLists(NCount).NodeNumbers(Loop2))
                    continue;
                if (flagError) { // only list nodelist name once
                    ShowSevereError(state, RoutineName + CurrentModuleObject + "=\"" + cAlphas(1) + "\" has duplicate nodes:");
                    flagError = false;
                }
                ShowContinueError(state,
                                  format("...list item={}, \"{}\", duplicate list item={}, \"{}\".",
                                         Loop1,
                                         state.dataLoopNodes->NodeID(state.dataNodeInputMgr->NodeLists(NCount).NodeNumbers(Loop1)),
                                         Loop2,
                                         state.dataLoopNodes->NodeID(state.dataNodeInputMgr->NodeLists(NCount).NodeNumbers(Loop2))));
                localErrorsFound = true;
            }
        }
    }

    for (Loop = 1; Loop <= state.dataNodeInputMgr->NumOfNodeLists; ++Loop) {
        for (Loop2 = 1; Loop2 <= state.dataNodeInputMgr->NodeLists(Loop).NumOfNodesInList; ++Loop2) {
            for (Loop1 = 1; Loop1 <= state.dataNodeInputMgr->NumOfNodeLists; ++Loop1) {
                if (Loop == Loop1) continue; // within a nodelist have already checked to see if node name duplicates nodelist name
                if (!UtilityRoutines::SameString(state.dataNodeInputMgr->NodeLists(Loop).NodeNames(Loop2),
                                                 state.dataNodeInputMgr->NodeLists(Loop1).Name))
                    continue;
                ShowSevereError(state,
                                RoutineName + CurrentModuleObject + "=\"" + state.dataNodeInputMgr->NodeLists(Loop1).Name +
                                    "\", invalid node name in list.");
                ShowContinueError(
                    state,
                    format("... Node {} Name=\"{}\", duplicates NodeList Name.", Loop2, state.dataNodeInputMgr->NodeLists(Loop).NodeNames(Loop2)));
                ShowContinueError(state, "... NodeList=\"" + state.dataNodeInputMgr->NodeLists(Loop1).Name + "\", is duplicated.");
                ShowContinueError(state, "... Items in NodeLists must not be the name of another NodeList.");
                localErrorsFound = true;
            }
        }
    }

    cAlphas.deallocate();
    rNumbers.deallocate();

    if (localErrorsFound) {
        ShowFatalError(state, RoutineName + CurrentModuleObject + ": Error getting input - causes termination.");
        ErrorsFound = true;
    }
}

int AssignNodeNumber(EnergyPlusData &state,
                     std::string const &Name,                         // Name for assignment
                     DataLoopNode::NodeFluidType const NodeFluidType, // must be valid
                     bool &ErrorsFound)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   September 1999
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function assigns a node number to this name.

    // METHODOLOGY EMPLOYED:
    // Look to see if a name has already been entered.  Use the index of
    // the array as the node number, if there.

    // Return value
    int AssignNodeNumber;

    if (NodeFluidType != DataLoopNode::NodeFluidType::Air && NodeFluidType != DataLoopNode::NodeFluidType::Water &&
        NodeFluidType != DataLoopNode::NodeFluidType::Electric && NodeFluidType != DataLoopNode::NodeFluidType::Steam &&
        NodeFluidType != DataLoopNode::NodeFluidType::blank) {
        ShowSevereError(state, format("AssignNodeNumber: Invalid FluidType={}", NodeFluidType));
        ErrorsFound = true;
        ShowFatalError(state, "AssignNodeNumber: Preceding issue causes termination.");
    }

    int NumNode = 0;
    if (state.dataNodeInputMgr->NumOfUniqueNodeNames > 0) {
        NumNode = UtilityRoutines::FindItemInList(
            Name, state.dataLoopNodes->NodeID({1, state.dataNodeInputMgr->NumOfUniqueNodeNames}), state.dataNodeInputMgr->NumOfUniqueNodeNames);
        if (NumNode > 0) {
            AssignNodeNumber = NumNode;
            ++state.dataNodeInputMgr->NodeRef(NumNode);
            if (NodeFluidType != DataLoopNode::NodeFluidType::blank) {
                if (state.dataLoopNodes->Node(NumNode).FluidType != NodeFluidType &&
                    state.dataLoopNodes->Node(NumNode).FluidType != DataLoopNode::NodeFluidType::blank) {
                    ShowSevereError(state, "Existing Fluid type for node, incorrect for request. Node=" + state.dataLoopNodes->NodeID(NumNode));
                    ShowContinueError(
                        state,
                        "Existing Fluid type=" + format("{}", DataLoopNode::ValidNodeFluidTypes(state.dataLoopNodes->Node(NumNode).FluidType)) +
                            ", Requested Fluid Type=" + format("{}", DataLoopNode::ValidNodeFluidTypes(NodeFluidType)));
                    ErrorsFound = true;
                }
            }
            if (state.dataLoopNodes->Node(NumNode).FluidType == DataLoopNode::NodeFluidType::blank) {
                state.dataLoopNodes->Node(NumNode).FluidType = NodeFluidType;
            }
        } else {
            ++state.dataNodeInputMgr->NumOfUniqueNodeNames;
            state.dataLoopNodes->NumOfNodes = state.dataNodeInputMgr->NumOfUniqueNodeNames;

            state.dataLoopNodes->Node.redimension(state.dataLoopNodes->NumOfNodes);
            state.dataLoopNodes->NodeID.redimension({0, state.dataLoopNodes->NumOfNodes});
            state.dataNodeInputMgr->NodeRef.redimension(state.dataLoopNodes->NumOfNodes);
            state.dataLoopNodes->MarkedNode.redimension(state.dataLoopNodes->NumOfNodes);
            state.dataLoopNodes->NodeSetpointCheck.redimension(state.dataLoopNodes->NumOfNodes);
            // Set new item in Node
            state.dataLoopNodes->Node(state.dataLoopNodes->NumOfNodes).FluidType = NodeFluidType;
            state.dataNodeInputMgr->NodeRef(state.dataLoopNodes->NumOfNodes) = 0;
            state.dataLoopNodes->NodeID(state.dataNodeInputMgr->NumOfUniqueNodeNames) = Name;

            AssignNodeNumber = state.dataNodeInputMgr->NumOfUniqueNodeNames;
        }
    } else {
        state.dataLoopNodes->Node.allocate(1);
        state.dataLoopNodes->Node(1).FluidType = NodeFluidType;
        // Allocate takes care of defining
        state.dataLoopNodes->NumOfNodes = 1;
        state.dataLoopNodes->NodeID.allocate({0, 1});
        state.dataNodeInputMgr->NodeRef.allocate(1);
        state.dataLoopNodes->MarkedNode.allocate(1);
        state.dataLoopNodes->NodeSetpointCheck.allocate(1);

        state.dataNodeInputMgr->NumOfUniqueNodeNames = 1;
        state.dataLoopNodes->NodeID(0) = "Undefined";
        state.dataLoopNodes->NodeID(state.dataNodeInputMgr->NumOfUniqueNodeNames) = Name;
        AssignNodeNumber = 1;
        state.dataNodeInputMgr->NodeRef(1) = 0;
    }

    return AssignNodeNumber;
}

int GetOnlySingleNode(EnergyPlusData &state,
                      std::string const &NodeName,
                      bool &errFlag,
                      std::string const &NodeObjectType,                         // Node Object Type (i.e. "Chiller:Electric")
                      std::string const &NodeObjectName,                         // Node Object Name (i.e. "MyChiller")
                      DataLoopNode::NodeFluidType const NodeFluidType,           // Fluidtype for checking/setting node FluidType
                      DataLoopNode::NodeConnectionType const NodeConnectionType, // Node Connection Type (see DataLoopNode)
                      compFluidStream const NodeFluidStream,                     // Which Fluid Stream
                      bool const ObjectIsParent,                                 // True/False
                      Optional_string_const InputFieldName                       // Input Field Name
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda K. Lawrie; adapted from GasAbsorptionChiller;Jason Glazer
    //       DATE WRITTEN   December 2001
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function gets a single node (or error message results) using the
    // node id from the input file.

    // Return value
    int GetSingleNodeResult;

    // FUNCTION PARAMETER DEFINITIONS:
    static std::string const RoutineName("GetOnlySingleNode: ");

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int NumNodes;

    DataLoopNode::NodeFluidType FluidType;
    std::string ConnectionType;

    int NumParams;
    int NumAlphas;
    int NumNums;

    if (state.dataNodeInputMgr->GetOnlySingleNodeFirstTime) {
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, "NodeList", NumParams, NumAlphas, NumNums);
        state.dataNodeInputMgr->GetOnlySingleNodeNodeNums.dimension(NumParams, 0);
        state.dataNodeInputMgr->GetOnlySingleNodeFirstTime = false;
    }

    FluidType = NodeFluidType;

    GetNodeNums(state,
                NodeName,
                NumNodes,
                state.dataNodeInputMgr->GetOnlySingleNodeNodeNums,
                errFlag,
                FluidType,
                NodeObjectType,
                NodeObjectName,
                NodeConnectionType,
                NodeFluidStream,
                ObjectIsParent,
                _,
                InputFieldName);

    if (NumNodes > 1) {
        ShowSevereError(state, RoutineName + NodeObjectType + "=\"" + NodeObjectName + "\", invalid data.");
        if (present(InputFieldName)) ShowContinueError(state, "...Ref field=" + InputFieldName);
        ShowContinueError(state, "Only 1st Node used from NodeList=\"" + NodeName + "\".");
        ShowContinueError(state, "...a Nodelist may not be valid in this context.");
        errFlag = true;
    } else if (NumNodes == 0) {
        state.dataNodeInputMgr->GetOnlySingleNodeNodeNums(1) = 0;
    }
    if (NumNodes > 0) {
        if (static_cast<int>(NodeConnectionType) >= 1 && static_cast<int>(NodeConnectionType) <= NumValidConnectionTypes) {
            ConnectionType = DataLoopNode::ValidConnectionTypes(NodeConnectionType);
        } else {
            ConnectionType = format("{}-unknown", NodeConnectionType);
        }
        //    CALL RegisterNodeConnection(NodeNums(1),NodeID(NodeNums(1)),NodeObjectType,NodeObjectName,  &
        //                                  ConnectionType,NodeFluidStream,ObjectIsParent,errFlag)
    }

    GetSingleNodeResult = state.dataNodeInputMgr->GetOnlySingleNodeNodeNums(1);

    return GetSingleNodeResult;
}

void InitUniqueNodeCheck(EnergyPlusData &state, std::string const &ContextName)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   November 2002
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine begins a process of checking for unique node names
    // in a sequence of nodes.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    bool errFlag(false);

    // Begin set up of Uniqueness context

    if (state.dataNodeInputMgr->GetNodeInputFlag) {
        GetNodeListsInput(state, errFlag);
        state.dataNodeInputMgr->GetNodeInputFlag = false;
    }

    if (!state.dataNodeInputMgr->CurCheckContextName.empty()) {
        ShowFatalError(state,
                       "Init Uniqueness called for \"" + ContextName + ", but checks for \"" + state.dataNodeInputMgr->CurCheckContextName +
                           "\" was already in progress.");
    }
    if (ContextName == std::string()) {
        ShowFatalError(state, "Init Uniqueness called with Blank Context Name");
    }
    if (allocated(state.dataNodeInputMgr->UniqueNodeNames)) {
        state.dataNodeInputMgr->UniqueNodeNames.deallocate();
    }

    state.dataNodeInputMgr->NumCheckNodes = 0;
    state.dataNodeInputMgr->MaxCheckNodes = 100;
    state.dataNodeInputMgr->UniqueNodeNames.allocate(state.dataNodeInputMgr->MaxCheckNodes);
    state.dataNodeInputMgr->CurCheckContextName = ContextName;
}

void CheckUniqueNodes(EnergyPlusData &state,
                      std::string const &NodeTypes,
                      std::string const &CheckType,
                      bool &ErrorsFound,
                      Optional_string_const CheckName,
                      Optional_int_const CheckNumber,
                      Optional_string_const ObjectName)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   November 2002
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine checks the appropriate input argument for uniqueness.
    // Call CheckUniqueNodes(NodeTypes,CheckType,ErrorsFound,CheckName,CheckNumber)
    // NodeTypes - used in error message (if any produced)
    // CheckType - "NodeName' or 'NodeNumber' (only 1 can be input per time)
    // ErrorsFound - true if error found by routine
    // CheckName - NodeName entered
    // CheckNumber - Node Number entered
    // only 1 of CheckName or CheckNumber need be entered.
    // ObjectName - "Name" field of object (i.e., CurCheckContextName)

    // METHODOLOGY EMPLOYED:
    // checks the current list of items for this (again)

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int Found;

    {
        auto const nodeType(CheckType);

        if (nodeType == "NodeName") {
            if (!present(CheckName)) {
                ShowFatalError(state, "Routine CheckUniqueNodes called with Nodetypes=NodeName, but did not include CheckName argument.");
            }
            if (!CheckName().empty()) {
                Found = UtilityRoutines::FindItemInList(CheckName, state.dataNodeInputMgr->UniqueNodeNames, state.dataNodeInputMgr->NumCheckNodes);
                if (Found != 0) {
                    ShowSevereError(state, state.dataNodeInputMgr->CurCheckContextName + "=\"" + ObjectName + "\", duplicate node names found.");
                    ShowContinueError(state, "...for Node Type(s)=" + NodeTypes + ", duplicate node name=\"" + CheckName + "\".");
                    ShowContinueError(state, "...Nodes must be unique across instances of this object.");
                    //          CALL ShowSevereError(state, 'Node Types='//TRIM(NodeTypes)//', Non Unique Name found='//TRIM(CheckName))
                    //          CALL ShowContinueError(state, 'Context='//TRIM(CurCheckContextName))
                    ErrorsFound = true;
                } else {
                    ++state.dataNodeInputMgr->NumCheckNodes;
                    if (state.dataNodeInputMgr->NumCheckNodes > state.dataNodeInputMgr->MaxCheckNodes) {
                        state.dataNodeInputMgr->UniqueNodeNames.redimension(state.dataNodeInputMgr->MaxCheckNodes += 100);
                    }
                    state.dataNodeInputMgr->UniqueNodeNames(state.dataNodeInputMgr->NumCheckNodes) = CheckName;
                }
            }

        } else if (nodeType == "NodeNumber") {
            if (!present(CheckNumber)) {
                ShowFatalError(state, "Routine CheckUniqueNodes called with Nodetypes=NodeNumber, but did not include CheckNumber argument.");
            }
            if (CheckNumber != 0) {
                Found = UtilityRoutines::FindItemInList(
                    state.dataLoopNodes->NodeID(CheckNumber), state.dataNodeInputMgr->UniqueNodeNames, state.dataNodeInputMgr->NumCheckNodes);
                if (Found != 0) {
                    ShowSevereError(state, state.dataNodeInputMgr->CurCheckContextName + "=\"" + ObjectName + "\", duplicate node names found.");
                    ShowContinueError(
                        state, "...for Node Type(s)=" + NodeTypes + ", duplicate node name=\"" + state.dataLoopNodes->NodeID(CheckNumber) + "\".");
                    ShowContinueError(state, "...Nodes must be unique across instances of this object.");
                    ErrorsFound = true;
                } else {
                    ++state.dataNodeInputMgr->NumCheckNodes;
                    if (state.dataNodeInputMgr->NumCheckNodes > state.dataNodeInputMgr->MaxCheckNodes) {
                        state.dataNodeInputMgr->UniqueNodeNames.redimension(state.dataNodeInputMgr->MaxCheckNodes += 100);
                    }
                    state.dataNodeInputMgr->UniqueNodeNames(state.dataNodeInputMgr->NumCheckNodes) = state.dataLoopNodes->NodeID(CheckNumber);
                }
            }

        } else {
            ShowFatalError(state, "CheckUniqueNodes called with invalid Check Type=" + CheckType);
            ErrorsFound = true;
        }
    }
}

void EndUniqueNodeCheck(EnergyPlusData &state, std::string const &ContextName)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   November 2002
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine marks the end of a unique node check.

    if (state.dataNodeInputMgr->CurCheckContextName != ContextName) {
        ShowFatalError(state,
                       "End Uniqueness called for \"" + ContextName + ", but checks for \"" + state.dataNodeInputMgr->CurCheckContextName +
                           "\" was in progress.");
    }
    if (ContextName == std::string()) {
        ShowFatalError(state, "End Uniqueness called with Blank Context Name");
    }
    state.dataNodeInputMgr->CurCheckContextName = std::string();
    if (allocated(state.dataNodeInputMgr->UniqueNodeNames)) {
        state.dataNodeInputMgr->UniqueNodeNames.deallocate();
    }
}

void CalcMoreNodeInfo(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   January 2004
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Calculate additional node information for reporting

    // METHODOLOGY EMPLOYED:
    // Input is the existing node data plus environment variables. Output is
    // stored in MoreNodeInfo.

    // Using/Aliasing
    using FluidProperties::GetDensityGlycol;
    using FluidProperties::GetSatDensityRefrig;
    using FluidProperties::GetSatEnthalpyRefrig;
    using FluidProperties::GetSpecificHeatGlycol;
    using OutputProcessor::ReqReportVariables;
    using Psychrometrics::CPCW;
    using Psychrometrics::PsyCpAirFnW;
    using Psychrometrics::PsyHFnTdbW;
    using Psychrometrics::PsyRhFnTdbWPb;
    using Psychrometrics::PsyRhoAirFnPbTdbW;
    using Psychrometrics::PsyTdpFnWPb;
    using Psychrometrics::PsyTwbFnTdbWPb;
    using Psychrometrics::RhoH2O;
    using ScheduleManager::GetCurrentScheduleValue;

    // SUBROUTINE PARAMETER DEFINITIONS:
    static std::string const RoutineName("CalcMoreNodeInfo");
    static std::string const NodeReportingCalc("NodeReportingCalc:");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int iNode; // node loop index
    int iReq;  // requested report variables loop index

    auto &RhoAirStdInit = state.dataNodeInputMgr->RhoAirStdInit;
    auto &RhoWaterStdInit = state.dataNodeInputMgr->RhoWaterStdInit;
    auto &NodeWetBulbSchedPtr = state.dataNodeInputMgr->NodeWetBulbSchedPtr;
    auto &NodeRelHumidityRepReq = state.dataNodeInputMgr->NodeRelHumidityRepReq;
    auto &NodeRelHumiditySchedPtr = state.dataNodeInputMgr->NodeRelHumiditySchedPtr;
    auto &NodeDewPointRepReq = state.dataNodeInputMgr->NodeDewPointRepReq;
    auto &NodeDewPointSchedPtr = state.dataNodeInputMgr->NodeDewPointSchedPtr;
    auto &NodeSpecificHeatRepReq = state.dataNodeInputMgr->NodeSpecificHeatRepReq;
    auto &NodeSpecificHeatSchedPtr = state.dataNodeInputMgr->NodeSpecificHeatSchedPtr;
    auto &nodeReportingStrings = state.dataNodeInputMgr->nodeReportingStrings;
    auto &nodeFluidNames = state.dataNodeInputMgr->nodeFluidNames;
    bool ReportWetBulb;
    bool ReportRelHumidity;
    bool ReportDewPoint;
    bool ReportSpecificHeat;
    Real64 SteamDensity;
    Real64 EnthSteamInDry;
    Real64 RhoAirCurrent; // temporary value for current air density f(baro, db , W)
    Real64 rho;
    Real64 Cp;
    Real64 rhoStd;

    if (state.dataNodeInputMgr->CalcMoreNodeInfoMyOneTimeFlag) {
        RhoAirStdInit = state.dataEnvrn->StdRhoAir;
        RhoWaterStdInit = RhoH2O(DataGlobalConstants::InitConvTemp);
        state.dataNodeInputMgr->NodeWetBulbRepReq.allocate(state.dataLoopNodes->NumOfNodes);
        NodeWetBulbSchedPtr.allocate(state.dataLoopNodes->NumOfNodes);
        NodeRelHumidityRepReq.allocate(state.dataLoopNodes->NumOfNodes);
        NodeRelHumiditySchedPtr.allocate(state.dataLoopNodes->NumOfNodes);
        NodeDewPointRepReq.allocate(state.dataLoopNodes->NumOfNodes);
        NodeDewPointSchedPtr.allocate(state.dataLoopNodes->NumOfNodes);
        NodeSpecificHeatRepReq.allocate(state.dataLoopNodes->NumOfNodes);
        NodeSpecificHeatSchedPtr.allocate(state.dataLoopNodes->NumOfNodes);
        nodeReportingStrings.reserve(state.dataLoopNodes->NumOfNodes);
        nodeFluidNames.reserve(state.dataLoopNodes->NumOfNodes);
        state.dataNodeInputMgr->NodeWetBulbRepReq = false;
        NodeWetBulbSchedPtr = 0;
        NodeRelHumidityRepReq = false;
        NodeRelHumiditySchedPtr = 0;
        NodeDewPointRepReq = false;
        NodeDewPointSchedPtr = 0;
        NodeSpecificHeatRepReq = false;
        NodeSpecificHeatSchedPtr = 0;

        for (iNode = 1; iNode <= state.dataLoopNodes->NumOfNodes; ++iNode) {
            nodeReportingStrings.push_back(std::string(NodeReportingCalc + state.dataLoopNodes->NodeID(iNode)));
            nodeFluidNames.push_back(FluidProperties::GetGlycolNameByIndex(state, state.dataLoopNodes->Node(iNode).FluidIndex));
            for (iReq = 1; iReq <= state.dataOutputProcessor->NumOfReqVariables; ++iReq) {
                if (UtilityRoutines::SameString(state.dataOutputProcessor->ReqRepVars(iReq).Key, state.dataLoopNodes->NodeID(iNode)) ||
                    state.dataOutputProcessor->ReqRepVars(iReq).Key.empty()) {
                    if (UtilityRoutines::SameString(state.dataOutputProcessor->ReqRepVars(iReq).VarName, "System Node Wetbulb Temperature")) {
                        state.dataNodeInputMgr->NodeWetBulbRepReq(iNode) = true;
                        NodeWetBulbSchedPtr(iNode) = state.dataOutputProcessor->ReqRepVars(iReq).SchedPtr;
                    } else if (UtilityRoutines::SameString(state.dataOutputProcessor->ReqRepVars(iReq).VarName, "System Node Relative Humidity")) {
                        NodeRelHumidityRepReq(iNode) = true;
                        NodeRelHumiditySchedPtr(iNode) = state.dataOutputProcessor->ReqRepVars(iReq).SchedPtr;
                    } else if (UtilityRoutines::SameString(state.dataOutputProcessor->ReqRepVars(iReq).VarName, "System Node Dewpoint Temperature")) {
                        NodeDewPointRepReq(iNode) = true;
                        NodeDewPointSchedPtr(iNode) = state.dataOutputProcessor->ReqRepVars(iReq).SchedPtr;
                    } else if (UtilityRoutines::SameString(state.dataOutputProcessor->ReqRepVars(iReq).VarName, "System Node Specific Heat")) {
                        NodeSpecificHeatRepReq(iNode) = true;
                        NodeSpecificHeatSchedPtr(iNode) = state.dataOutputProcessor->ReqRepVars(iReq).SchedPtr;
                    }
                }
            }
            if (EMSManager::CheckIfNodeMoreInfoSensedByEMS(state, iNode, "System Node Wetbulb Temperature")) {
                state.dataNodeInputMgr->NodeWetBulbRepReq(iNode) = true;
                NodeWetBulbSchedPtr(iNode) = 0;
            }
            if (EMSManager::CheckIfNodeMoreInfoSensedByEMS(state, iNode, "System Node Relative Humidity")) {
                NodeRelHumidityRepReq(iNode) = true;
                NodeRelHumiditySchedPtr(iNode) = 0;
            }
            if (EMSManager::CheckIfNodeMoreInfoSensedByEMS(state, iNode, "System Node Dewpoint Temperature")) {
                NodeDewPointRepReq(iNode) = true;
                NodeDewPointSchedPtr(iNode) = 0;
            }
            if (EMSManager::CheckIfNodeMoreInfoSensedByEMS(state, iNode, "System Node Specific Heat")) {
                NodeSpecificHeatRepReq(iNode) = true;
                NodeSpecificHeatSchedPtr(iNode) = 0;
            }
        }
        state.dataNodeInputMgr->CalcMoreNodeInfoMyOneTimeFlag = false;
    }

    for (iNode = 1; iNode <= state.dataLoopNodes->NumOfNodes; ++iNode) {
        ReportWetBulb = false;
        ReportRelHumidity = false;
        ReportDewPoint = false;
        ReportSpecificHeat = false;
        if (state.dataNodeInputMgr->NodeWetBulbRepReq(iNode) && NodeWetBulbSchedPtr(iNode) > 0) {
            ReportWetBulb = (GetCurrentScheduleValue(state, NodeWetBulbSchedPtr(iNode)) > 0.0);
        } else if (state.dataNodeInputMgr->NodeWetBulbRepReq(iNode) && NodeWetBulbSchedPtr(iNode) == 0) {
            ReportWetBulb = true;
        } else if (state.dataLoopNodes->Node(iNode).SPMNodeWetBulbRepReq) {
            ReportWetBulb = true;
        }
        if (NodeRelHumidityRepReq(iNode) && NodeRelHumiditySchedPtr(iNode) > 0) {
            ReportRelHumidity = (GetCurrentScheduleValue(state, NodeRelHumiditySchedPtr(iNode)) > 0.0);
        } else if (NodeRelHumidityRepReq(iNode) && NodeRelHumiditySchedPtr(iNode) == 0) {
            ReportRelHumidity = true;
        }
        if (NodeDewPointRepReq(iNode) && NodeDewPointSchedPtr(iNode) > 0) {
            ReportDewPoint = (GetCurrentScheduleValue(state, NodeDewPointSchedPtr(iNode)) > 0.0);
        } else if (NodeDewPointRepReq(iNode) && NodeDewPointSchedPtr(iNode) == 0) {
            ReportDewPoint = true;
        }
        if (NodeSpecificHeatRepReq(iNode) && NodeSpecificHeatSchedPtr(iNode) > 0) {
            ReportSpecificHeat = (GetCurrentScheduleValue(state, NodeSpecificHeatSchedPtr(iNode)) > 0.0);
        } else if (NodeSpecificHeatRepReq(iNode) && NodeSpecificHeatSchedPtr(iNode) == 0) {
            ReportSpecificHeat = true;
        }
        // calculate the volume flow rate
        if (state.dataLoopNodes->Node(iNode).FluidType == DataLoopNode::NodeFluidType::Air) {
            state.dataLoopNodes->MoreNodeInfo(iNode).VolFlowRateStdRho = state.dataLoopNodes->Node(iNode).MassFlowRate / RhoAirStdInit;
            // if Node%Press was reliable could be used here.
            RhoAirCurrent = PsyRhoAirFnPbTdbW(
                state, state.dataEnvrn->OutBaroPress, state.dataLoopNodes->Node(iNode).Temp, state.dataLoopNodes->Node(iNode).HumRat);
            state.dataLoopNodes->MoreNodeInfo(iNode).Density = RhoAirCurrent;
            if (RhoAirCurrent != 0.0)
                state.dataLoopNodes->MoreNodeInfo(iNode).VolFlowRateCrntRho = state.dataLoopNodes->Node(iNode).MassFlowRate / RhoAirCurrent;
            state.dataLoopNodes->MoreNodeInfo(iNode).ReportEnthalpy =
                PsyHFnTdbW(state.dataLoopNodes->Node(iNode).Temp, state.dataLoopNodes->Node(iNode).HumRat);
            if (ReportWetBulb) {
                // if Node%Press was reliable could be used here.
                state.dataLoopNodes->MoreNodeInfo(iNode).WetBulbTemp = PsyTwbFnTdbWPb(state,
                                                                                      state.dataLoopNodes->Node(iNode).Temp,
                                                                                      state.dataLoopNodes->Node(iNode).HumRat,
                                                                                      state.dataEnvrn->OutBaroPress,
                                                                                      nodeReportingStrings[iNode - 1]);
            } else {
                state.dataLoopNodes->MoreNodeInfo(iNode).WetBulbTemp = 0.0;
            }
            if (ReportDewPoint) {
                state.dataLoopNodes->MoreNodeInfo(iNode).AirDewPointTemp =
                    PsyTdpFnWPb(state, state.dataLoopNodes->Node(iNode).HumRat, state.dataEnvrn->OutBaroPress);
            } else {
                state.dataLoopNodes->MoreNodeInfo(iNode).AirDewPointTemp = 0.0;
            }
            if (ReportRelHumidity) {
                // if Node%Press was reliable could be used here.
                // following routines don't issue psych errors and may be more reliable.
                state.dataLoopNodes->MoreNodeInfo(iNode).RelHumidity = 100.0 * PsyRhFnTdbWPb(state,
                                                                                             state.dataLoopNodes->Node(iNode).Temp,
                                                                                             state.dataLoopNodes->Node(iNode).HumRat,
                                                                                             state.dataEnvrn->OutBaroPress,
                                                                                             nodeReportingStrings[iNode - 1]);
            } else {
                state.dataLoopNodes->MoreNodeInfo(iNode).RelHumidity = 0.0;
            }
            if (ReportSpecificHeat) { // only call psych routine if needed.
                state.dataLoopNodes->MoreNodeInfo(iNode).SpecificHeat = PsyCpAirFnW(state.dataLoopNodes->Node(iNode).HumRat);
            } else {
                state.dataLoopNodes->MoreNodeInfo(iNode).SpecificHeat = 0.0;
            }
        } else if (state.dataLoopNodes->Node(iNode).FluidType == DataLoopNode::NodeFluidType::Water) {

            if (!((state.dataLoopNodes->Node(iNode).FluidIndex > 0) &&
                  (state.dataLoopNodes->Node(iNode).FluidIndex <= state.dataFluidProps->NumOfGlycols))) {
                rho = RhoWaterStdInit;
                rhoStd = RhoWaterStdInit;
                Cp = CPCW(state.dataLoopNodes->Node(iNode).Temp);
            } else {
                Cp = GetSpecificHeatGlycol(state,
                                           nodeFluidNames[iNode - 1],
                                           state.dataLoopNodes->Node(iNode).Temp,
                                           state.dataLoopNodes->Node(iNode).FluidIndex,
                                           nodeReportingStrings[iNode - 1]);
                rhoStd = GetDensityGlycol(state,
                                          nodeFluidNames[iNode - 1],
                                          DataGlobalConstants::InitConvTemp,
                                          state.dataLoopNodes->Node(iNode).FluidIndex,
                                          nodeReportingStrings[iNode - 1]);
                rho = GetDensityGlycol(state,
                                       nodeFluidNames[iNode - 1],
                                       state.dataLoopNodes->Node(iNode).Temp,
                                       state.dataLoopNodes->Node(iNode).FluidIndex,
                                       nodeReportingStrings[iNode - 1]);
            }

            state.dataLoopNodes->MoreNodeInfo(iNode).VolFlowRateStdRho = state.dataLoopNodes->Node(iNode).MassFlowRate / rhoStd;
            state.dataLoopNodes->MoreNodeInfo(iNode).VolFlowRateCrntRho = state.dataLoopNodes->Node(iNode).MassFlowRate / rho;
            state.dataLoopNodes->MoreNodeInfo(iNode).Density = rho;
            state.dataLoopNodes->MoreNodeInfo(iNode).ReportEnthalpy = Cp * state.dataLoopNodes->Node(iNode).Temp;
            state.dataLoopNodes->MoreNodeInfo(iNode).SpecificHeat = Cp; // always fill since cp already always being calculated anyway
            state.dataLoopNodes->MoreNodeInfo(iNode).WetBulbTemp = 0.0;
            state.dataLoopNodes->MoreNodeInfo(iNode).RelHumidity = 100.0;
        } else if (state.dataLoopNodes->Node(iNode).FluidType == DataLoopNode::NodeFluidType::Steam) {
            if (state.dataLoopNodes->Node(iNode).Quality == 1.0) {
                SteamDensity = GetSatDensityRefrig(state,
                                                   fluidNameSteam,
                                                   state.dataLoopNodes->Node(iNode).Temp,
                                                   state.dataLoopNodes->Node(iNode).Quality,
                                                   state.dataLoopNodes->Node(iNode).FluidIndex,
                                                   RoutineName);
                EnthSteamInDry = GetSatEnthalpyRefrig(state,
                                                      fluidNameSteam,
                                                      state.dataLoopNodes->Node(iNode).Temp,
                                                      state.dataLoopNodes->Node(iNode).Quality,
                                                      state.dataLoopNodes->Node(iNode).FluidIndex,
                                                      RoutineName);
                state.dataLoopNodes->MoreNodeInfo(iNode).VolFlowRateStdRho = state.dataLoopNodes->Node(iNode).MassFlowRate / SteamDensity;
                state.dataLoopNodes->MoreNodeInfo(iNode).ReportEnthalpy = EnthSteamInDry;
                state.dataLoopNodes->MoreNodeInfo(iNode).WetBulbTemp = 0.0;
                state.dataLoopNodes->MoreNodeInfo(iNode).RelHumidity = 0.0;
            } else if (state.dataLoopNodes->Node(iNode).Quality == 0.0) { // The node has condensate water through it
                state.dataLoopNodes->MoreNodeInfo(iNode).VolFlowRateStdRho = state.dataLoopNodes->Node(iNode).MassFlowRate / RhoWaterStdInit;
                state.dataLoopNodes->MoreNodeInfo(iNode).ReportEnthalpy =
                    CPCW(state.dataLoopNodes->Node(iNode).Temp) * state.dataLoopNodes->Node(iNode).Temp;
                state.dataLoopNodes->MoreNodeInfo(iNode).WetBulbTemp = 0.0;
                state.dataLoopNodes->MoreNodeInfo(iNode).RelHumidity = 0.0;
            }
        } else if (state.dataLoopNodes->Node(iNode).FluidType == DataLoopNode::NodeFluidType::Electric) {
            state.dataLoopNodes->MoreNodeInfo(iNode).VolFlowRateStdRho = 0.0;
            state.dataLoopNodes->MoreNodeInfo(iNode).ReportEnthalpy = 0.0;
            state.dataLoopNodes->MoreNodeInfo(iNode).WetBulbTemp = 0.0;
            state.dataLoopNodes->MoreNodeInfo(iNode).RelHumidity = 0.0;
            state.dataLoopNodes->MoreNodeInfo(iNode).SpecificHeat = 0.0;
        } else {
            state.dataLoopNodes->MoreNodeInfo(iNode).VolFlowRateStdRho = state.dataLoopNodes->Node(iNode).MassFlowRate / RhoAirStdInit;
            if (state.dataLoopNodes->Node(iNode).HumRat > 0.0) {
                state.dataLoopNodes->MoreNodeInfo(iNode).ReportEnthalpy =
                    PsyHFnTdbW(state.dataLoopNodes->Node(iNode).Temp, state.dataLoopNodes->Node(iNode).HumRat);
                if (ReportWetBulb) {
                    state.dataLoopNodes->MoreNodeInfo(iNode).WetBulbTemp = PsyTwbFnTdbWPb(
                        state, state.dataLoopNodes->Node(iNode).Temp, state.dataLoopNodes->Node(iNode).HumRat, state.dataEnvrn->StdBaroPress);
                } else {
                    state.dataLoopNodes->MoreNodeInfo(iNode).WetBulbTemp = 0.0;
                }
                if (ReportSpecificHeat) {
                    state.dataLoopNodes->MoreNodeInfo(iNode).SpecificHeat = PsyCpAirFnW(state.dataLoopNodes->Node(iNode).HumRat);
                } else {
                    state.dataLoopNodes->MoreNodeInfo(iNode).SpecificHeat = 0.0;
                }
            } else {
                state.dataLoopNodes->MoreNodeInfo(iNode).ReportEnthalpy =
                    CPCW(state.dataLoopNodes->Node(iNode).Temp) * state.dataLoopNodes->Node(iNode).Temp;
                state.dataLoopNodes->MoreNodeInfo(iNode).WetBulbTemp = 0.0;
                state.dataLoopNodes->MoreNodeInfo(iNode).SpecificHeat = 0.0;
            }
        }
    }
}

void MarkNode(EnergyPlusData &state,
              int const NodeNumber, // Node Number to be marked
              std::string const &ObjectType,
              std::string const &ObjectName,
              std::string const &FieldName)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   March 2004
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine marks a node -- this node needs to exist in more than one object.

    state.dataLoopNodes->MarkedNode(NodeNumber).IsMarked = true;
    state.dataLoopNodes->MarkedNode(NodeNumber).ObjectType = ObjectType;
    state.dataLoopNodes->MarkedNode(NodeNumber).ObjectName = ObjectName;
    state.dataLoopNodes->MarkedNode(NodeNumber).FieldName = FieldName;
}

void CheckMarkedNodes(EnergyPlusData &state, bool &ErrorsFound)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   March 2004
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine checks "marked" nodes.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int NodeNum;

    for (NodeNum = 1; NodeNum <= state.dataLoopNodes->NumOfNodes; ++NodeNum) {
        if (state.dataLoopNodes->MarkedNode(NodeNum).IsMarked) {
            if (state.dataNodeInputMgr->NodeRef(NodeNum) == 0) {
                ShowSevereError(state, "Node=\"" + state.dataLoopNodes->NodeID(NodeNum) + "\" did not find reference by another object.");
                ShowContinueError(state,
                                  "Object=\"" + state.dataLoopNodes->MarkedNode(NodeNum).ObjectType + "\", Name=\"" +
                                      state.dataLoopNodes->MarkedNode(NodeNum).ObjectName + "\", Field=[" +
                                      state.dataLoopNodes->MarkedNode(NodeNum).FieldName + ']');
                ErrorsFound = true;
            }
        }
    }
}

} // namespace EnergyPlus::NodeInputManager
