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

namespace EnergyPlus {

namespace NodeInputManager {

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

    // Data
    // MODULE PARAMETER DEFINITIONS
    static std::string const BlankString;
    static std::string const fluidNameSteam("STEAM");

    // DERIVED TYPE DEFINITIONS

    // INTERFACE BLOCK SPECIFICATIONS
    // na

    // MODULE VARIABLE DECLARATIONS:

    int NumOfNodeLists(0);       // Total number of Node Lists in IDF
    int NumOfUniqueNodeNames(0); // Number of Unique Node Names (current)
    // The following is a module level flag because there are several possible "entries" into
    // this module that may need to get the Node Inputs.
    bool GetNodeInputFlag(true);     // Flag to Get Node Input(s)
    Array1D_string TmpNodeID;        // Used to "reallocate" name arrays
    Array1D_int NodeRef;             // Number of times a Node is "referenced"
    std::string CurCheckContextName; // Used in Uniqueness checks
    Array1D_string UniqueNodeNames;  // used in uniqueness checks
    int NumCheckNodes(0);            // Num of Unique nodes in check
    int MaxCheckNodes(0);            // Current "max" unique nodes in check
    bool NodeVarsSetup(false);       // Setup indicator of node vars for reporting (also that all nodes have been entered)
    Array1D_bool NodeWetBulbRepReq;

    // Object Data
    Array1D<NodeListDef> NodeLists; // Node Lists
    namespace {
        bool CalcMoreNodeInfoMyOneTimeFlag(true); // one time flag
        Array1D_int GetOnlySingleNodeNodeNums;
        bool GetOnlySingleNodeFirstTime(true);
    } // namespace
    // MODULE SUBROUTINES:
    //*************************************************************************

    // Functions

    // Clears the global data in NodeInputManager.
    // Needed for unit tests, should not be normally called.
    void clear_state()
    {
        CalcMoreNodeInfoMyOneTimeFlag = true;
        NumOfNodeLists = 0;
        NumOfUniqueNodeNames = 0;
        GetNodeInputFlag = true;
        TmpNodeID.deallocate();
        NodeRef.deallocate();
        CurCheckContextName = std::string();
        UniqueNodeNames.deallocate();
        NumCheckNodes = 0;
        MaxCheckNodes = 0;
        NodeVarsSetup = false;
        NodeLists.deallocate();
        GetOnlySingleNodeNodeNums.deallocate();
        GetOnlySingleNodeFirstTime = true;
        NodeWetBulbRepReq.deallocate();
    }

    void GetNodeNums(EnergyPlusData &state,
                     std::string const &Name,                  // Name for which to obtain information
                     int &NumNodes,                            // Number of nodes accompanying this Name
                     Array1D_int &NodeNumbers,                 // Node Numbers accompanying this Name
                     bool &ErrorsFound,                        // True when errors are found...
                     int const NodeFluidType,                  // Fluidtype for checking/setting node FluidType
                     std::string const &NodeObjectType,        // Node Object Type (i.e. "Chiller:Electric")
                     std::string const &NodeObjectName,        // Node Object Name (i.e. "MyChiller")
                     int const NodeConnectionType,             // Node Connection Type (see DataLoopNode)
                     int const NodeFluidStream,                // Which Fluid Stream (1,2,3,...)
                     bool const ObjectIsParent,                // True/False
                     Optional_bool_const IncrementFluidStream, // True/False
                     Optional_string_const InputFieldName      // Input Field Name
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

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Argument array dimensioning

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("GetNodeNums: ");

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int ThisOne; // Indicator for this Name
        //  CHARACTER(len=20) :: CaseNodeFluidType
        std::string ConnectionType;
        int Loop;
        int FluidStreamNum; // Fluid stream number passed to RegisterNodeConnection

        if (GetNodeInputFlag) {
            GetNodeListsInput(state, ErrorsFound);
            GetNodeInputFlag = false;
        }

        if (NodeFluidType != NodeType_Air && NodeFluidType != NodeType_Water && NodeFluidType != NodeType_Electric &&
            NodeFluidType != NodeType_Steam && NodeFluidType != NodeType_Unknown) {
            ShowSevereError(state, RoutineName + NodeObjectType + "=\"" + NodeObjectName + "\", invalid fluid type.");
            ShowContinueError(state, format("..Invalid FluidType={}", NodeFluidType));
            ErrorsFound = true;
            ShowFatalError(state, "Preceding issue causes termination.");
        }

        if (not_blank(Name)) {
            ThisOne = UtilityRoutines::FindItemInList(Name, NodeLists);
            if (ThisOne != 0) {
                NumNodes = NodeLists(ThisOne).NumOfNodesInList;
                NodeNumbers({1, NumNodes}) = NodeLists(ThisOne).NodeNumbers({1, NumNodes});
                for (Loop = 1; Loop <= NumNodes; ++Loop) {
                    if (NodeFluidType != NodeType_Unknown && Node(NodeNumbers(Loop)).FluidType != NodeType_Unknown) {
                        if (Node(NodeNumbers(Loop)).FluidType != NodeFluidType) {
                            ShowSevereError(state, RoutineName + NodeObjectType + "=\"" + NodeObjectName + "\", invalid data.");
                            if (present(InputFieldName)) ShowContinueError(state, "...Ref field=" + InputFieldName);
                            ShowContinueError(state, "Existing Fluid type for node, incorrect for request. Node=" + NodeID(NodeNumbers(Loop)));
                            ShowContinueError(state, "Existing Fluid type=" + ValidNodeFluidTypes(Node(NodeNumbers(Loop)).FluidType) +
                                              ", Requested Fluid Type=" + ValidNodeFluidTypes(NodeFluidType));
                            ErrorsFound = true;
                        }
                    }
                    if (Node(NodeNumbers(Loop)).FluidType == NodeType_Unknown) {
                        Node(NodeNumbers(Loop)).FluidType = NodeFluidType;
                    }
                    ++NodeRef(NodeNumbers(Loop));
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
            if (NodeConnectionType >= 1 && NodeConnectionType <= NumValidConnectionTypes) {
                ConnectionType = ValidConnectionTypes(NodeConnectionType);
            } else {
                ConnectionType = format("{}-unknown", NodeConnectionType);
            }
            // If requested, assign NodeFluidStream to the first node and increment the fluid stream number
            // for each remaining node in the list
            if (present(IncrementFluidStream)) {
                if (IncrementFluidStream) FluidStreamNum = NodeFluidStream + (Loop - 1);
            }
            RegisterNodeConnection(state, NodeNumbers(Loop),
                                   NodeID(NodeNumbers(Loop)),
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

        if (!NodeVarsSetup) {
            if (!state.dataErrTracking->AbortProcessing) {
                MoreNodeInfo.allocate(NumOfUniqueNodeNames);
                for (int NumNode = 1; NumNode <= NumOfUniqueNodeNames; ++NumNode) {
                    // Setup Report variables for the Nodes for HVAC Reporting, CurrentModuleObject='Node Name'
                    SetupOutputVariable(state,
                        "System Node Temperature", OutputProcessor::Unit::C, Node(NumNode).Temp, "System", "Average", NodeID(NumNode));
                    SetupOutputVariable(state,
                        "System Node Mass Flow Rate", OutputProcessor::Unit::kg_s, Node(NumNode).MassFlowRate, "System", "Average", NodeID(NumNode));
                    SetupOutputVariable(state, "System Node Humidity Ratio",
                                        OutputProcessor::Unit::kgWater_kgDryAir,
                                        Node(NumNode).HumRat,
                                        "System",
                                        "Average",
                                        NodeID(NumNode));
                    SetupOutputVariable(state, "System Node Setpoint Temperature",
                                        OutputProcessor::Unit::C,
                                        Node(NumNode).TempSetPoint,
                                        "System",
                                        "Average",
                                        NodeID(NumNode));
                    SetupOutputVariable(state, "System Node Setpoint High Temperature",
                                        OutputProcessor::Unit::C,
                                        Node(NumNode).TempSetPointHi,
                                        "System",
                                        "Average",
                                        NodeID(NumNode));
                    SetupOutputVariable(state, "System Node Setpoint Low Temperature",
                                        OutputProcessor::Unit::C,
                                        Node(NumNode).TempSetPointLo,
                                        "System",
                                        "Average",
                                        NodeID(NumNode));
                    SetupOutputVariable(state, "System Node Setpoint Humidity Ratio",
                                        OutputProcessor::Unit::kgWater_kgDryAir,
                                        Node(NumNode).HumRatSetPoint,
                                        "System",
                                        "Average",
                                        NodeID(NumNode));
                    SetupOutputVariable(state, "System Node Setpoint Minimum Humidity Ratio",
                                        OutputProcessor::Unit::kgWater_kgDryAir,
                                        Node(NumNode).HumRatMin,
                                        "System",
                                        "Average",
                                        NodeID(NumNode));
                    SetupOutputVariable(state, "System Node Setpoint Maximum Humidity Ratio",
                                        OutputProcessor::Unit::kgWater_kgDryAir,
                                        Node(NumNode).HumRatMax,
                                        "System",
                                        "Average",
                                        NodeID(NumNode));
                    SetupOutputVariable(state, "System Node Relative Humidity",
                                        OutputProcessor::Unit::Perc,
                                        MoreNodeInfo(NumNode).RelHumidity,
                                        "System",
                                        "Average",
                                        NodeID(NumNode));
                    SetupOutputVariable(state, "System Node Pressure", OutputProcessor::Unit::Pa, Node(NumNode).Press, "System", "Average", NodeID(NumNode));
                    SetupOutputVariable(state, "System Node Standard Density Volume Flow Rate",
                                        OutputProcessor::Unit::m3_s,
                                        MoreNodeInfo(NumNode).VolFlowRateStdRho,
                                        "System",
                                        "Average",
                                        NodeID(NumNode));
                    if (Node(NumNode).FluidType == NodeType_Air ||
                        Node(NumNode).FluidType == NodeType_Water) { // setup volume flow rate report for actual/current density
                        SetupOutputVariable(state, "System Node Current Density Volume Flow Rate",
                                            OutputProcessor::Unit::m3_s,
                                            MoreNodeInfo(NumNode).VolFlowRateCrntRho,
                                            "System",
                                            "Average",
                                            NodeID(NumNode));
                        SetupOutputVariable(state, "System Node Current Density",
                                            OutputProcessor::Unit::kg_m3,
                                            MoreNodeInfo(NumNode).Density,
                                            "System",
                                            "Average",
                                            NodeID(NumNode));
                        SetupOutputVariable(state, "System Node Specific Heat",
                                            OutputProcessor::Unit::J_kgK,
                                            MoreNodeInfo(NumNode).SpecificHeat,
                                            "System",
                                            "Average",
                                            NodeID(NumNode));
                    }

                    SetupOutputVariable(state, "System Node Enthalpy",
                                        OutputProcessor::Unit::J_kg,
                                        MoreNodeInfo(NumNode).ReportEnthalpy,
                                        "System",
                                        "Average",
                                        NodeID(NumNode));
                    SetupOutputVariable(state, "System Node Wetbulb Temperature",
                                        OutputProcessor::Unit::C,
                                        MoreNodeInfo(NumNode).WetBulbTemp,
                                        "System",
                                        "Average",
                                        NodeID(NumNode));
                    SetupOutputVariable(state, "System Node Dewpoint Temperature",
                                        OutputProcessor::Unit::C,
                                        MoreNodeInfo(NumNode).AirDewPointTemp,
                                        "System",
                                        "Average",
                                        NodeID(NumNode));
                    SetupOutputVariable(state,
                        "System Node Wind Speed", OutputProcessor::Unit::m_s, Node(NumNode).OutAirWindSpeed, "System", "Average", NodeID(NumNode));
                    SetupOutputVariable(state,
                        "System Node Wind Direction", OutputProcessor::Unit::deg, Node(NumNode).OutAirWindDir, "System", "Average", NodeID(NumNode));
                    SetupOutputVariable(state,
                        "System Node Quality", OutputProcessor::Unit::None, Node(NumNode).Quality, "System", "Average", NodeID(NumNode));
                    SetupOutputVariable(state, "System Node Height", OutputProcessor::Unit::m, Node(NumNode).Height, "System", "Average", NodeID(NumNode));
                    if (state.dataGlobal->DisplayAdvancedReportVariables) {
                        SetupOutputVariable(state,
                            "System Node Minimum Temperature", OutputProcessor::Unit::C, Node(NumNode).TempMin, "System", "Average", NodeID(NumNode));
                        SetupOutputVariable(state,
                            "System Node Maximum Temperature", OutputProcessor::Unit::C, Node(NumNode).TempMax, "System", "Average", NodeID(NumNode));
                        SetupOutputVariable(state, "System Node Minimum Limit Mass Flow Rate",
                                            OutputProcessor::Unit::kg_s,
                                            Node(NumNode).MassFlowRateMin,
                                            "System",
                                            "Average",
                                            NodeID(NumNode));
                        SetupOutputVariable(state, "System Node Maximum Limit Mass Flow Rate",
                                            OutputProcessor::Unit::kg_s,
                                            Node(NumNode).MassFlowRateMax,
                                            "System",
                                            "Average",
                                            NodeID(NumNode));
                        SetupOutputVariable(state, "System Node Minimum Available Mass Flow Rate",
                                            OutputProcessor::Unit::kg_s,
                                            Node(NumNode).MassFlowRateMinAvail,
                                            "System",
                                            "Average",
                                            NodeID(NumNode));
                        SetupOutputVariable(state, "System Node Maximum Available Mass Flow Rate",
                                            OutputProcessor::Unit::kg_s,
                                            Node(NumNode).MassFlowRateMaxAvail,
                                            "System",
                                            "Average",
                                            NodeID(NumNode));
                        SetupOutputVariable(state, "System Node Setpoint Mass Flow Rate",
                                            OutputProcessor::Unit::kg_s,
                                            Node(NumNode).MassFlowRateSetPoint,
                                            "System",
                                            "Average",
                                            NodeID(NumNode));
                        SetupOutputVariable(state, "System Node Requested Mass Flow Rate",
                                            OutputProcessor::Unit::kg_s,
                                            Node(NumNode).MassFlowRateRequest,
                                            "System",
                                            "Average",
                                            NodeID(NumNode));
                        SetupOutputVariable(state, "System Node Last Timestep Temperature",
                                            OutputProcessor::Unit::C,
                                            Node(NumNode).TempLastTimestep,
                                            "System",
                                            "Average",
                                            NodeID(NumNode));
                        SetupOutputVariable(state, "System Node Last Timestep Enthalpy",
                                            OutputProcessor::Unit::J_kg,
                                            Node(NumNode).EnthalpyLastTimestep,
                                            "System",
                                            "Average",
                                            NodeID(NumNode));
                    }
                    if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                        SetupOutputVariable(state,
                            "System Node CO2 Concentration", OutputProcessor::Unit::ppm, Node(NumNode).CO2, "System", "Average", NodeID(NumNode));
                    }
                    if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                        SetupOutputVariable(state, "System Node Generic Air Contaminant Concentration",
                                            OutputProcessor::Unit::ppm,
                                            Node(NumNode).GenContam,
                                            "System",
                                            "Average",
                                            NodeID(NumNode));
                    }
                }
            }
            NodeVarsSetup = true;

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
            print(state.files.bnd, " #Nodes,{}\n", NumOfUniqueNodeNames);
            if (NumOfUniqueNodeNames > 0) {
                static constexpr auto Format_702(
                    "! <Node>,<NodeNumber>,<Node Name>,<Node Fluid Type>,<# Times Node Referenced After Definition>");
                print(state.files.bnd, "{}\n", Format_702);
            }
            int Count0 = 0;
            for (int NumNode = 1; NumNode <= NumOfUniqueNodeNames; ++NumNode) {
               print(state.files.bnd, " Node,{},{},{},{}\n", NumNode, NodeID(NumNode), ValidNodeFluidTypes(Node(NumNode).FluidType) ,NodeRef(NumNode));
                if (NodeRef(NumNode) == 0) ++Count0;
            }
            // Show suspicious node names on the Branch-Node Details file
            if (Count0 > 0) {
                print(state.files.bnd, "{}\n", "! ===============================================================");
                print(state.files.bnd, "{}\n", "! Suspicious nodes have 0 references.  It is normal for some nodes, however.");
                print(state.files.bnd, "{}\n", "! Listing nodes with 0 references (culled from previous list):");
                static constexpr auto Format_703(
                    "! <Suspicious Node>,<NodeNumber>,<Node Name>,<Node Fluid Type>,<# Times Node Referenced After Definition>");
                print(state.files.bnd, "{}\n", Format_703);
                for (int NumNode = 1; NumNode <= NumOfUniqueNodeNames; ++NumNode) {
                    if (NodeRef(NumNode) > 0) continue;
                    print(state.files.bnd, " Suspicious Node,{},{},{},{}\n", NumNode, NodeID(NumNode),ValidNodeFluidTypes(Node(NumNode).FluidType) ,  NodeRef(NumNode));
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

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        static std::string const RoutineName("GetNodeListsInput: ");
        static std::string const CurrentModuleObject("NodeList");

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

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
        inputProcessor->getObjectDefMaxArgs(state, CurrentModuleObject, NCount, NumAlphas, NumNumbers);
        cAlphas.allocate(NumAlphas);
        rNumbers.allocate(NumNumbers);
        NumOfNodeLists = inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
        NodeLists.allocate(NumOfNodeLists);
        for (int i = 1; i <= NumOfNodeLists; ++i) {
            NodeLists(i).Name.clear();
            NodeLists(i).NumOfNodesInList = 0;
        }

        NCount = 0;
        for (Loop = 1; Loop <= NumOfNodeLists; ++Loop) {
            inputProcessor->getObjectItem(state, CurrentModuleObject, Loop, cAlphas, NumAlphas, rNumbers, NumNumbers, IOStatus);
            if (UtilityRoutines::IsNameEmpty(state, cAlphas(1), CurrentModuleObject, localErrorsFound)) continue;

            ++NCount;
            NodeLists(NCount).Name = cAlphas(1);
            NodeLists(NCount).NodeNames.allocate(NumAlphas - 1);
            NodeLists(NCount).NodeNames = "";
            NodeLists(NCount).NodeNumbers.allocate(NumAlphas - 1);
            NodeLists(NCount).NodeNumbers = 0;
            NodeLists(NCount).NumOfNodesInList = NumAlphas - 1;
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
                NodeLists(NCount).NodeNames(Loop1) = cAlphas(Loop1 + 1);
                if (cAlphas(Loop1 + 1).empty()) {
                    ShowWarningError(state, RoutineName + CurrentModuleObject + "=\"" + cAlphas(1) + "\", blank node name in list.");
                    --NodeLists(NCount).NumOfNodesInList;
                    if (NodeLists(NCount).NumOfNodesInList <= 0) {
                        ShowSevereError(state, RoutineName + CurrentModuleObject + "=\"" + cAlphas(1) + "\" does not have any nodes.");
                        localErrorsFound = true;
                        break;
                    }
                    continue;
                }
                NodeLists(NCount).NodeNumbers(Loop1) = AssignNodeNumber(state, NodeLists(NCount).NodeNames(Loop1), NodeType_Unknown, localErrorsFound);
                if (UtilityRoutines::SameString(NodeLists(NCount).NodeNames(Loop1), NodeLists(NCount).Name)) {
                    ShowSevereError(state, RoutineName + CurrentModuleObject + "=\"" + cAlphas(1) + "\", invalid node name in list.");
                    ShowContinueError(state, format("... Node {} Name=\"{}\", duplicates NodeList Name.", Loop1, cAlphas(Loop1 + 1)));
                    localErrorsFound = true;
                }
            }
            // Error on any duplicates
            flagError = true;
            for (Loop1 = 1; Loop1 <= NodeLists(NCount).NumOfNodesInList; ++Loop1) {
                for (Loop2 = Loop1 + 1; Loop2 <= NodeLists(NCount).NumOfNodesInList; ++Loop2) {
                    if (NodeLists(NCount).NodeNumbers(Loop1) != NodeLists(NCount).NodeNumbers(Loop2)) continue;
                    if (flagError) { // only list nodelist name once
                        ShowSevereError(state, RoutineName + CurrentModuleObject + "=\"" + cAlphas(1) + "\" has duplicate nodes:");
                        flagError = false;
                    }
                    ShowContinueError(state,
                                      format("...list item={}, \"{}\", duplicate list item={}, \"{}\".",
                                             Loop1,
                                             NodeID(NodeLists(NCount).NodeNumbers(Loop1)),
                                             Loop2,
                                             NodeID(NodeLists(NCount).NodeNumbers(Loop2))));
                    localErrorsFound = true;
                }
            }
        }

        for (Loop = 1; Loop <= NumOfNodeLists; ++Loop) {
            for (Loop2 = 1; Loop2 <= NodeLists(Loop).NumOfNodesInList; ++Loop2) {
                for (Loop1 = 1; Loop1 <= NumOfNodeLists; ++Loop1) {
                    if (Loop == Loop1) continue; // within a nodelist have already checked to see if node name duplicates nodelist name
                    if (!UtilityRoutines::SameString(NodeLists(Loop).NodeNames(Loop2), NodeLists(Loop1).Name)) continue;
                    ShowSevereError(state, RoutineName + CurrentModuleObject + "=\"" + NodeLists(Loop1).Name + "\", invalid node name in list.");
                    ShowContinueError(state, format("... Node {} Name=\"{}\", duplicates NodeList Name.", Loop2, NodeLists(Loop).NodeNames(Loop2)));
                    ShowContinueError(state, "... NodeList=\"" + NodeLists(Loop1).Name + "\", is duplicated.");
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

    int AssignNodeNumber(EnergyPlusData &state, std::string const &Name, // Name for assignment
                         int const NodeFluidType, // must be valid
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

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Return value
        int AssignNodeNumber;

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:

        if (NodeFluidType != NodeType_Air && NodeFluidType != NodeType_Water && NodeFluidType != NodeType_Electric &&
            NodeFluidType != NodeType_Steam && NodeFluidType != NodeType_Unknown) {
            ShowSevereError(state, format("AssignNodeNumber: Invalid FluidType={}", NodeFluidType));
            ErrorsFound = true;
            ShowFatalError(state, "AssignNodeNumber: Preceding issue causes termination.");
        }

        int NumNode = 0;
        if (NumOfUniqueNodeNames > 0) {
            NumNode = UtilityRoutines::FindItemInList(Name, NodeID({1, NumOfUniqueNodeNames}), NumOfUniqueNodeNames);
            if (NumNode > 0) {
                AssignNodeNumber = NumNode;
                ++NodeRef(NumNode);
                if (NodeFluidType != NodeType_Unknown) {
                    if (Node(NumNode).FluidType != NodeFluidType && Node(NumNode).FluidType != NodeType_Unknown) {
                        ShowSevereError(state, "Existing Fluid type for node, incorrect for request. Node=" + NodeID(NumNode));
                        ShowContinueError(state, "Existing Fluid type=" + ValidNodeFluidTypes(Node(NumNode).FluidType) +
                                          ", Requested Fluid Type=" + ValidNodeFluidTypes(NodeFluidType));
                        ErrorsFound = true;
                    }
                }
                if (Node(NumNode).FluidType == NodeType_Unknown) {
                    Node(NumNode).FluidType = NodeFluidType;
                }
            } else {
                ++NumOfUniqueNodeNames;
                NumOfNodes = NumOfUniqueNodeNames;

                Node.redimension(NumOfNodes);
                NodeID.redimension({0, NumOfNodes});
                NodeRef.redimension(NumOfNodes);
                MarkedNode.redimension(NumOfNodes);
                NodeSetpointCheck.redimension(NumOfNodes);
                // Set new item in Node
                Node(NumOfNodes).FluidType = NodeFluidType;
                NodeRef(NumOfNodes) = 0;
                NodeID(NumOfUniqueNodeNames) = Name;

                AssignNodeNumber = NumOfUniqueNodeNames;
            }
        } else {
            Node.allocate(1);
            Node(1).FluidType = NodeFluidType;
            // Allocate takes care of defining
            NumOfNodes = 1;
            NodeID.allocate({0, 1});
            NodeRef.allocate(1);
            MarkedNode.allocate(1);
            NodeSetpointCheck.allocate(1);

            NumOfUniqueNodeNames = 1;
            NodeID(0) = "Undefined";
            NodeID(NumOfUniqueNodeNames) = Name;
            AssignNodeNumber = 1;
            NodeRef(1) = 0;
        }

        return AssignNodeNumber;
    }

    int GetOnlySingleNode(EnergyPlusData &state,
                          std::string const &NodeName,
                          bool &errFlag,
                          std::string const &NodeObjectType,   // Node Object Type (i.e. "Chiller:Electric")
                          std::string const &NodeObjectName,   // Node Object Name (i.e. "MyChiller")
                          int const NodeFluidType,             // Fluidtype for checking/setting node FluidType
                          int const NodeConnectionType,        // Node Connection Type (see DataLoopNode)
                          int const NodeFluidStream,           // Which Fluid Stream (1,2,3,...)
                          bool const ObjectIsParent,           // True/False
                          Optional_string_const InputFieldName // Input Field Name
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

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Return value
        int GetSingleNodeResult;

        // Locals
        // FUNCTION ARGUMENT DEFINITIONS:

        // FUNCTION PARAMETER DEFINITIONS:
        static std::string const RoutineName("GetOnlySingleNode: ");

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:
        int NumNodes;

        int FluidType;
        std::string ConnectionType;

        int NumParams;
        int NumAlphas;
        int NumNums;

        if (GetOnlySingleNodeFirstTime) {
            inputProcessor->getObjectDefMaxArgs(state, "NodeList", NumParams, NumAlphas, NumNums);
            GetOnlySingleNodeNodeNums.dimension(NumParams, 0);
            GetOnlySingleNodeFirstTime = false;
        }

        FluidType = NodeFluidType;

        GetNodeNums(state,
                    NodeName,
                    NumNodes,
                    GetOnlySingleNodeNodeNums,
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
            GetOnlySingleNodeNodeNums(1) = 0;
        }
        if (NumNodes > 0) {
            if (NodeConnectionType >= 1 && NodeConnectionType <= NumValidConnectionTypes) {
                ConnectionType = ValidConnectionTypes(NodeConnectionType);
            } else {
                ConnectionType = format("{}-unknown", NodeConnectionType);
            }
            //    CALL RegisterNodeConnection(NodeNums(1),NodeID(NodeNums(1)),NodeObjectType,NodeObjectName,  &
            //                                  ConnectionType,NodeFluidStream,ObjectIsParent,errFlag)
        }

        GetSingleNodeResult = GetOnlySingleNodeNodeNums(1);

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

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        bool errFlag(false);

        // Begin set up of Uniqueness context

        if (GetNodeInputFlag) {
            GetNodeListsInput(state, errFlag);
            GetNodeInputFlag = false;
        }

        if (!CurCheckContextName.empty()) {
            ShowFatalError(state, "Init Uniqueness called for \"" + ContextName + ", but checks for \"" + CurCheckContextName +
                           "\" was already in progress.");
        }
        if (ContextName == BlankString) {
            ShowFatalError(state, "Init Uniqueness called with Blank Context Name");
        }
        if (allocated(UniqueNodeNames)) {
            UniqueNodeNames.deallocate();
        }

        NumCheckNodes = 0;
        MaxCheckNodes = 100;
        UniqueNodeNames.allocate(MaxCheckNodes);
        CurCheckContextName = ContextName;
    }

    void CheckUniqueNodes(EnergyPlusData &state, std::string const &NodeTypes,
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

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int Found;

        {
            auto const nodeType(CheckType);

            if (nodeType == "NodeName") {
                if (!present(CheckName)) {
                    ShowFatalError(state, "Routine CheckUniqueNodes called with Nodetypes=NodeName, but did not include CheckName argument.");
                }
                if (!CheckName().empty()) {
                    Found = UtilityRoutines::FindItemInList(CheckName, UniqueNodeNames, NumCheckNodes);
                    if (Found != 0) {
                        ShowSevereError(state, CurCheckContextName + "=\"" + ObjectName + "\", duplicate node names found.");
                        ShowContinueError(state, "...for Node Type(s)=" + NodeTypes + ", duplicate node name=\"" + CheckName + "\".");
                        ShowContinueError(state, "...Nodes must be unique across instances of this object.");
                        //          CALL ShowSevereError(state, 'Node Types='//TRIM(NodeTypes)//', Non Unique Name found='//TRIM(CheckName))
                        //          CALL ShowContinueError(state, 'Context='//TRIM(CurCheckContextName))
                        ErrorsFound = true;
                    } else {
                        ++NumCheckNodes;
                        if (NumCheckNodes > MaxCheckNodes) {
                            UniqueNodeNames.redimension(MaxCheckNodes += 100);
                        }
                        UniqueNodeNames(NumCheckNodes) = CheckName;
                    }
                }

            } else if (nodeType == "NodeNumber") {
                if (!present(CheckNumber)) {
                    ShowFatalError(state, "Routine CheckUniqueNodes called with Nodetypes=NodeNumber, but did not include CheckNumber argument.");
                }
                if (CheckNumber != 0) {
                    Found = UtilityRoutines::FindItemInList(NodeID(CheckNumber), UniqueNodeNames, NumCheckNodes);
                    if (Found != 0) {
                        ShowSevereError(state, CurCheckContextName + "=\"" + ObjectName + "\", duplicate node names found.");
                        ShowContinueError(state, "...for Node Type(s)=" + NodeTypes + ", duplicate node name=\"" + NodeID(CheckNumber) + "\".");
                        ShowContinueError(state, "...Nodes must be unique across instances of this object.");
                        //          CALL ShowSevereError(state, 'Node Types='//TRIM(NodeTypes)//', Non Unique Name found='//TRIM(NodeID(CheckNumber)))
                        //          CALL ShowContinueError(state, 'Context='//TRIM(CurCheckContextName))
                        ErrorsFound = true;
                    } else {
                        ++NumCheckNodes;
                        if (NumCheckNodes > MaxCheckNodes) {
                            UniqueNodeNames.redimension(MaxCheckNodes += 100);
                        }
                        UniqueNodeNames(NumCheckNodes) = NodeID(CheckNumber);
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

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        // na

        if (CurCheckContextName != ContextName) {
            ShowFatalError(state, "End Uniqueness called for \"" + ContextName + ", but checks for \"" + CurCheckContextName + "\" was in progress.");
        }
        if (ContextName == BlankString) {
            ShowFatalError(state, "End Uniqueness called with Blank Context Name");
        }
        CurCheckContextName = BlankString;
        if (allocated(UniqueNodeNames)) {
            UniqueNodeNames.deallocate();
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
        using FluidProperties::GetGlycolNameByIndex;
        using FluidProperties::GetSatDensityRefrig;
        using FluidProperties::GetSatEnthalpyRefrig;
        using FluidProperties::GetSpecificHeatGlycol;
        using FluidProperties::NumOfGlycols;
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

        static Real64 RhoAirStdInit;
        static Real64 RhoWaterStdInit;
        static Array1D_int NodeWetBulbSchedPtr;
        static Array1D_bool NodeRelHumidityRepReq;
        static Array1D_int NodeRelHumiditySchedPtr;
        static Array1D_bool NodeDewPointRepReq;
        static Array1D_int NodeDewPointSchedPtr;
        static Array1D_bool NodeSpecificHeatRepReq;
        static Array1D_int NodeSpecificHeatSchedPtr;
        static std::vector<std::string> nodeReportingStrings;
        static std::vector<std::string> nodeFluidNames;
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

        if (CalcMoreNodeInfoMyOneTimeFlag) {
            RhoAirStdInit = state.dataEnvrn->StdRhoAir;
            RhoWaterStdInit = RhoH2O(DataGlobalConstants::InitConvTemp);
            NodeWetBulbRepReq.allocate(NumOfNodes);
            NodeWetBulbSchedPtr.allocate(NumOfNodes);
            NodeRelHumidityRepReq.allocate(NumOfNodes);
            NodeRelHumiditySchedPtr.allocate(NumOfNodes);
            NodeDewPointRepReq.allocate(NumOfNodes);
            NodeDewPointSchedPtr.allocate(NumOfNodes);
            NodeSpecificHeatRepReq.allocate(NumOfNodes);
            NodeSpecificHeatSchedPtr.allocate(NumOfNodes);
            nodeReportingStrings.reserve(NumOfNodes);
            nodeFluidNames.reserve(NumOfNodes);
            NodeWetBulbRepReq = false;
            NodeWetBulbSchedPtr = 0;
            NodeRelHumidityRepReq = false;
            NodeRelHumiditySchedPtr = 0;
            NodeDewPointRepReq = false;
            NodeDewPointSchedPtr = 0;
            NodeSpecificHeatRepReq = false;
            NodeSpecificHeatSchedPtr = 0;

            for (iNode = 1; iNode <= NumOfNodes; ++iNode) {
                nodeReportingStrings.push_back(std::string(NodeReportingCalc + NodeID(iNode)));
                nodeFluidNames.push_back(GetGlycolNameByIndex(Node(iNode).FluidIndex));
                for (iReq = 1; iReq <= state.dataOutputProcessor->NumOfReqVariables; ++iReq) {
                    if (UtilityRoutines::SameString(state.dataOutputProcessor->ReqRepVars(iReq).Key, NodeID(iNode)) || state.dataOutputProcessor->ReqRepVars(iReq).Key.empty()) {
                        if (UtilityRoutines::SameString(state.dataOutputProcessor->ReqRepVars(iReq).VarName, "System Node Wetbulb Temperature")) {
                            NodeWetBulbRepReq(iNode) = true;
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
                    NodeWetBulbRepReq(iNode) = true;
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
            CalcMoreNodeInfoMyOneTimeFlag = false;
        }

        for (iNode = 1; iNode <= NumOfNodes; ++iNode) {
            ReportWetBulb = false;
            ReportRelHumidity = false;
            ReportDewPoint = false;
            ReportSpecificHeat = false;
            if (NodeWetBulbRepReq(iNode) && NodeWetBulbSchedPtr(iNode) > 0) {
                ReportWetBulb = (GetCurrentScheduleValue(state, NodeWetBulbSchedPtr(iNode)) > 0.0);
            } else if (NodeWetBulbRepReq(iNode) && NodeWetBulbSchedPtr(iNode) == 0) {
                ReportWetBulb = true;
            } else if (Node(iNode).SPMNodeWetBulbRepReq) {
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
            if (Node(iNode).FluidType == NodeType_Air) {
                MoreNodeInfo(iNode).VolFlowRateStdRho = Node(iNode).MassFlowRate / RhoAirStdInit;
                // if Node%Press was reliable could be used here.
                RhoAirCurrent = PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, Node(iNode).Temp, Node(iNode).HumRat);
                MoreNodeInfo(iNode).Density = RhoAirCurrent;
                if (RhoAirCurrent != 0.0) MoreNodeInfo(iNode).VolFlowRateCrntRho = Node(iNode).MassFlowRate / RhoAirCurrent;
                MoreNodeInfo(iNode).ReportEnthalpy = PsyHFnTdbW(Node(iNode).Temp, Node(iNode).HumRat);
                if (ReportWetBulb) {
                    // if Node%Press was reliable could be used here.
                    MoreNodeInfo(iNode).WetBulbTemp =
                        PsyTwbFnTdbWPb(state, Node(iNode).Temp, Node(iNode).HumRat, state.dataEnvrn->OutBaroPress, nodeReportingStrings[iNode - 1]);
                } else {
                    MoreNodeInfo(iNode).WetBulbTemp = 0.0;
                }
                if (ReportDewPoint) {
                    MoreNodeInfo(iNode).AirDewPointTemp = PsyTdpFnWPb(state, Node(iNode).HumRat, state.dataEnvrn->OutBaroPress);
                } else {
                    MoreNodeInfo(iNode).AirDewPointTemp = 0.0;
                }
                if (ReportRelHumidity) {
                    // if Node%Press was reliable could be used here.
                    // following routines don't issue psych errors and may be more reliable.
                    MoreNodeInfo(iNode).RelHumidity =
                        100.0 * PsyRhFnTdbWPb(state, Node(iNode).Temp, Node(iNode).HumRat, state.dataEnvrn->OutBaroPress, nodeReportingStrings[iNode - 1]);
                } else {
                    MoreNodeInfo(iNode).RelHumidity = 0.0;
                }
                if (ReportSpecificHeat) { // only call psych routine if needed.
                    MoreNodeInfo(iNode).SpecificHeat = PsyCpAirFnW(Node(iNode).HumRat);
                } else {
                    MoreNodeInfo(iNode).SpecificHeat = 0.0;
                }
            } else if (Node(iNode).FluidType == NodeType_Water) {

                if (!((Node(iNode).FluidIndex > 0) && (Node(iNode).FluidIndex <= NumOfGlycols))) {
                    rho = RhoWaterStdInit;
                    rhoStd = RhoWaterStdInit;
                    Cp = CPCW(Node(iNode).Temp);
                } else {
                    Cp = GetSpecificHeatGlycol(state, nodeFluidNames[iNode - 1], Node(iNode).Temp, Node(iNode).FluidIndex, nodeReportingStrings[iNode - 1]);
                    rhoStd = GetDensityGlycol(
                        state, nodeFluidNames[iNode - 1], DataGlobalConstants::InitConvTemp, Node(iNode).FluidIndex, nodeReportingStrings[iNode - 1]);
                    rho = GetDensityGlycol(state, nodeFluidNames[iNode - 1], Node(iNode).Temp, Node(iNode).FluidIndex, nodeReportingStrings[iNode - 1]);
                }

                MoreNodeInfo(iNode).VolFlowRateStdRho = Node(iNode).MassFlowRate / rhoStd;
                MoreNodeInfo(iNode).VolFlowRateCrntRho = Node(iNode).MassFlowRate / rho;
                MoreNodeInfo(iNode).Density = rho;
                MoreNodeInfo(iNode).ReportEnthalpy = Cp * Node(iNode).Temp;
                MoreNodeInfo(iNode).SpecificHeat = Cp; // always fill since cp already always being calculated anyway
                MoreNodeInfo(iNode).WetBulbTemp = 0.0;
                MoreNodeInfo(iNode).RelHumidity = 100.0;
            } else if (Node(iNode).FluidType == NodeType_Steam) {
                if (Node(iNode).Quality == 1.0) {
                    SteamDensity = GetSatDensityRefrig(state, fluidNameSteam, Node(iNode).Temp, Node(iNode).Quality, Node(iNode).FluidIndex, RoutineName);
                    EnthSteamInDry = GetSatEnthalpyRefrig(state, fluidNameSteam, Node(iNode).Temp, Node(iNode).Quality, Node(iNode).FluidIndex, RoutineName);
                    MoreNodeInfo(iNode).VolFlowRateStdRho = Node(iNode).MassFlowRate / SteamDensity;
                    MoreNodeInfo(iNode).ReportEnthalpy = EnthSteamInDry;
                    MoreNodeInfo(iNode).WetBulbTemp = 0.0;
                    MoreNodeInfo(iNode).RelHumidity = 0.0;
                } else if (Node(iNode).Quality == 0.0) { // The node has condensate water through it
                    MoreNodeInfo(iNode).VolFlowRateStdRho = Node(iNode).MassFlowRate / RhoWaterStdInit;
                    MoreNodeInfo(iNode).ReportEnthalpy = CPCW(Node(iNode).Temp) * Node(iNode).Temp;
                    MoreNodeInfo(iNode).WetBulbTemp = 0.0;
                    MoreNodeInfo(iNode).RelHumidity = 0.0;
                }
            } else if (Node(iNode).FluidType == NodeType_Electric) {
                MoreNodeInfo(iNode).VolFlowRateStdRho = 0.0;
                MoreNodeInfo(iNode).ReportEnthalpy = 0.0;
                MoreNodeInfo(iNode).WetBulbTemp = 0.0;
                MoreNodeInfo(iNode).RelHumidity = 0.0;
                MoreNodeInfo(iNode).SpecificHeat = 0.0;
            } else {
                MoreNodeInfo(iNode).VolFlowRateStdRho = Node(iNode).MassFlowRate / RhoAirStdInit;
                if (Node(iNode).HumRat > 0.0) {
                    MoreNodeInfo(iNode).ReportEnthalpy = PsyHFnTdbW(Node(iNode).Temp, Node(iNode).HumRat);
                    if (ReportWetBulb) {
                        MoreNodeInfo(iNode).WetBulbTemp = PsyTwbFnTdbWPb(state, Node(iNode).Temp, Node(iNode).HumRat, state.dataEnvrn->StdBaroPress);
                    } else {
                        MoreNodeInfo(iNode).WetBulbTemp = 0.0;
                    }
                    if (ReportSpecificHeat) {
                        MoreNodeInfo(iNode).SpecificHeat = PsyCpAirFnW(Node(iNode).HumRat);
                    } else {
                        MoreNodeInfo(iNode).SpecificHeat = 0.0;
                    }
                } else {
                    MoreNodeInfo(iNode).ReportEnthalpy = CPCW(Node(iNode).Temp) * Node(iNode).Temp;
                    MoreNodeInfo(iNode).WetBulbTemp = 0.0;
                    MoreNodeInfo(iNode).SpecificHeat = 0.0;
                }
            }
        }
    }

    void MarkNode(int const NodeNumber, // Node Number to be marked
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

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        // na

        MarkedNode(NodeNumber).IsMarked = true;
        MarkedNode(NodeNumber).ObjectType = ObjectType;
        MarkedNode(NodeNumber).ObjectName = ObjectName;
        MarkedNode(NodeNumber).FieldName = FieldName;
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

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // na

        // USE STATEMENTS:
        // na

        // Locals
        // SUBROUTINE ARGUMENT DEFINITIONS:

        // SUBROUTINE PARAMETER DEFINITIONS:
        // na

        // INTERFACE BLOCK SPECIFICATIONS
        // na

        // DERIVED TYPE DEFINITIONS
        // na

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        int NodeNum;

        for (NodeNum = 1; NodeNum <= NumOfNodes; ++NodeNum) {
            if (MarkedNode(NodeNum).IsMarked) {
                if (NodeRef(NodeNum) == 0) {
                    ShowSevereError(state, "Node=\"" + NodeID(NodeNum) + "\" did not find reference by another object.");
                    ShowContinueError(state, "Object=\"" + MarkedNode(NodeNum).ObjectType + "\", Name=\"" + MarkedNode(NodeNum).ObjectName + "\", Field=[" +
                                      MarkedNode(NodeNum).FieldName + ']');
                    ErrorsFound = true;
                }
            }
        }
    }

} // namespace NodeInputManager

} // namespace EnergyPlus
