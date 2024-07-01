// EnergyPlus, Copyright (c) 1996-2024, The Board of Trustees of the University of Illinois,
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

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/member.functions.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataBranchNodeConnections.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus::BranchNodeConnections {

// Module containing the routines dealing with the Branch/Node Connections (CompSets, etc)

// MODULE INFORMATION:
//       AUTHOR         Linda Lawrie
//       DATE WRITTEN   May 2005

// PURPOSE OF THIS MODULE:
// This module encapsulates the connection data necessary for some of the checks
// needed in the branch-node data

// Using/Aliasing
using namespace DataBranchNodeConnections;

static constexpr std::string_view undefined("UNDEFINED");

void RegisterNodeConnection(EnergyPlusData &state,
                            int const NodeNumber,                                // Number for this Node
                            std::string_view const NodeName,                     // Name of this Node
                            Node::ConnObjType const ObjectType, // Type of object this Node is connected to (e.g. Chiller:Electric)
                            std::string_view const ObjectName,                   // Name of object this Node is connected to (e.g. MyChiller)
                            Node::ConnType const ConnectionType,   // Connection Type for this Node (must be valid)
                            Node::CompFluidStream const FluidStream, // Count on Fluid Streams
                            bool const IsParent,                                 // True when node is a parent node
                            bool &errFlag,                                       // Will be True if errors already detected or if errors found here
                            std::string_view const InputFieldName                // Input Field Name
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   February 2004
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine registers a node connection in the Node Connection data structure.  This
    // structure is intended to help with HVAC diagramming as well as validation of nodes.

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName = "RegisterNodeConnection: ";

    bool ErrorsFoundHere = false;

    if ((ObjectType == Node::ConnObjType::Invalid) || (ObjectType == Node::ConnObjType::Num)) {
        ShowSevereError(state, "Developer Error: Invalid ObjectType");
        ShowContinueError(state, format("Occurs for Node={}, ObjectName={}", std::string{NodeName}, std::string{ObjectName}));
        ErrorsFoundHere = true;
    }

    std::string_view const objTypeStr = Node::connObjTypeNames[(int)ObjectType];
    std::string_view const conTypeStr = Node::connTypeNames[(int)ConnectionType];

    if ((ConnectionType == Node::ConnType::Invalid) || (ConnectionType == Node::ConnType::Num)) {
        ShowSevereError(state, format("{}{}{}", RoutineName, "Invalid ConnectionType=", ConnectionType));
        ShowContinueError(state, format("Occurs for Node={}, ObjectType={}, ObjectName={}", NodeName, objTypeStr, ObjectName));
        ErrorsFoundHere = true;
    }

    bool MakeNew = true;
    for (int Count = 1; Count <= state.dataBranchNodeConnections->NumNodeConnections; ++Count) {
        if (state.dataBranchNodeConnections->NodeConnections(Count).NodeNum != NodeNumber) continue;
        if (state.dataBranchNodeConnections->NodeConnections(Count).connObjType != ObjectType) continue;
        if (!Util::SameString(state.dataBranchNodeConnections->NodeConnections(Count).ObjectName, ObjectName)) continue;
        if (state.dataBranchNodeConnections->NodeConnections(Count).connType != ConnectionType) continue;
        if (state.dataBranchNodeConnections->NodeConnections(Count).FluidStream != FluidStream) continue;
        if ((state.dataBranchNodeConnections->NodeConnections(Count).ObjectIsParent && !IsParent) ||
            (!state.dataBranchNodeConnections->NodeConnections(Count).ObjectIsParent && IsParent)) {
            ShowSevereError(state, format("{}{}", RoutineName, "Node registered for both Parent and \"not\" Parent"));
            ShowContinueError(state, format("{}{}{}{}{}{}", "Occurs for Node=", NodeName, ", ObjectType=", ObjectType, ", ObjectName=", ObjectName));
            ErrorsFoundHere = true;
        }
        MakeNew = false;
    }
    if (MakeNew) {
        int constexpr NodeConnectionAlloc = 1000;
        ++state.dataBranchNodeConnections->NumNodeConnections;
        if (state.dataBranchNodeConnections->NumNodeConnections > 1 &&
            state.dataBranchNodeConnections->NumNodeConnections > state.dataBranchNodeConnections->MaxNodeConnections) {
            state.dataBranchNodeConnections->NodeConnections.resize(state.dataBranchNodeConnections->MaxNodeConnections += NodeConnectionAlloc);
        } else if (state.dataBranchNodeConnections->NumNodeConnections == 1) {
            state.dataBranchNodeConnections->NodeConnections.allocate(NodeConnectionAlloc);
            state.dataBranchNodeConnections->MaxNodeConnections = NodeConnectionAlloc;
        }

        state.dataBranchNodeConnections->NodeConnections(state.dataBranchNodeConnections->NumNodeConnections).NodeNum = NodeNumber;
        state.dataBranchNodeConnections->NodeConnections(state.dataBranchNodeConnections->NumNodeConnections).NodeName = NodeName;
        state.dataBranchNodeConnections->NodeConnections(state.dataBranchNodeConnections->NumNodeConnections).connObjType = ObjectType;
        state.dataBranchNodeConnections->NodeConnections(state.dataBranchNodeConnections->NumNodeConnections).ObjectName = ObjectName;
        state.dataBranchNodeConnections->NodeConnections(state.dataBranchNodeConnections->NumNodeConnections).connType = ConnectionType;
        state.dataBranchNodeConnections->NodeConnections(state.dataBranchNodeConnections->NumNodeConnections).FluidStream = FluidStream;
        state.dataBranchNodeConnections->NodeConnections(state.dataBranchNodeConnections->NumNodeConnections).ObjectIsParent = IsParent;
    }

    if (has_prefixi(objTypeStr, "AirTerminal:")) {
        if (!InputFieldName.empty()) {
            ++state.dataBranchNodeConnections->NumAirTerminalNodes;
            int constexpr EqNodeConnectionAlloc = 100;
            if (state.dataBranchNodeConnections->NumAirTerminalNodes > 1 &&
                state.dataBranchNodeConnections->NumAirTerminalNodes > state.dataBranchNodeConnections->MaxAirTerminalNodes) {
                state.dataBranchNodeConnections->AirTerminalNodeConnections.resize(state.dataBranchNodeConnections->MaxAirTerminalNodes +=
                                                                                   EqNodeConnectionAlloc);
            } else if (state.dataBranchNodeConnections->NumAirTerminalNodes == 1) {
                state.dataBranchNodeConnections->AirTerminalNodeConnections.allocate(EqNodeConnectionAlloc);
                state.dataBranchNodeConnections->MaxAirTerminalNodes = EqNodeConnectionAlloc;
            }

            // Check out AirTerminal inlet/outlet nodes
            bool Found = Util::FindItemInList(NodeName,
                                              state.dataBranchNodeConnections->AirTerminalNodeConnections,
                                              &EqNodeConnectionDef::NodeName,
                                              state.dataBranchNodeConnections->NumAirTerminalNodes - 1);
            if (Found != 0) { // Nodename already used
                ShowSevereError(state, fmt::format("{}{}=\"{}\" node name duplicated", RoutineName, ObjectType, ObjectName));
                ShowContinueError(state, format("NodeName=\"{}\", entered as type={}", NodeName, conTypeStr));
                ShowContinueError(state, fmt::format("In Field={}", InputFieldName));
                ShowContinueError(state,
                                  format("NodeName=\"{}\", entered as type={}", NodeName, Node::connTypeNamesUC[(int)ConnectionType]));
                ShowContinueError(state, format("In Field={}", InputFieldName));
                ShowContinueError(
                    state,
                    format("Already used in {}=\"{}\".", objTypeStr, state.dataBranchNodeConnections->AirTerminalNodeConnections(Found).ObjectName));
                ShowContinueError(
                    state,
                    format(" as type={}, In Field={}",
                           Node::connTypeNamesUC[(int)state.dataBranchNodeConnections->AirTerminalNodeConnections(Found).connType],
                           state.dataBranchNodeConnections->AirTerminalNodeConnections(Found).InputFieldName));
                ErrorsFoundHere = true;
            } else {
                state.dataBranchNodeConnections->AirTerminalNodeConnections(state.dataBranchNodeConnections->NumAirTerminalNodes).NodeName =
                    NodeName;
                state.dataBranchNodeConnections->AirTerminalNodeConnections(state.dataBranchNodeConnections->NumAirTerminalNodes).connObjType =
                    ObjectType;
                state.dataBranchNodeConnections->AirTerminalNodeConnections(state.dataBranchNodeConnections->NumAirTerminalNodes).ObjectName =
                    ObjectName;
                state.dataBranchNodeConnections->AirTerminalNodeConnections(state.dataBranchNodeConnections->NumAirTerminalNodes).connType =
                    ConnectionType;
                state.dataBranchNodeConnections->AirTerminalNodeConnections(state.dataBranchNodeConnections->NumAirTerminalNodes).InputFieldName =
                    InputFieldName;
            }
        } else {
            ShowSevereError(state, fmt::format("{}{} , Developer Error: Input Field Name not included.", RoutineName, objTypeStr));
            ShowContinueError(state, "Node names not checked for duplication.");
        }
    }

    if (ErrorsFoundHere) {
        errFlag = true;
    }
}

void OverrideNodeConnectionType(
    EnergyPlusData &state,
    int const NodeNumber,                                // Number for this Node
    std::string const &NodeName,                         // Name of this Node
    Node::ConnObjType const ObjectType, // Type of object this Node is connected to (e.g. Chiller:Electric)
    std::string const &ObjectName,                       // Name of object this Node is connected to (e.g. MyChiller)
    Node::ConnType const ConnectionType,   // Connection Type for this Node (must be valid)
    Node::CompFluidStream const FluidStream, // Count on Fluid Streams
    bool const IsParent,                                 // True when node is a parent node
    bool &errFlag                                        // Will be True if errors already detected or if errors found here
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         M. J. Witte
    //       DATE WRITTEN   June 2016

    // PURPOSE:
    // This subroutine modifies an existing node connection in the Node Connection data structure.  This
    // structure is intended to help with HVAC diagramming as well as validation of nodes. This function
    // is a based on RegisterNodeConnection.

    static constexpr std::string_view RoutineName("ModifyNodeConnectionType: ");

    if ((ConnectionType == Node::ConnType::Invalid) || (ConnectionType == Node::ConnType::Num)) {
        ShowSevereError(state, format("{}{}{}", RoutineName, "Invalid ConnectionType=", ConnectionType));
        ShowContinueError(
            state,
            format("Occurs for Node={}, ObjectType={}, ObjectName={}", NodeName, Node::connTypeNames[(int)ObjectType], ObjectName));
        errFlag = true;
    }

    int Found = 0;
    for (int Count = 1; Count <= state.dataBranchNodeConnections->NumNodeConnections; ++Count) {
        if (state.dataBranchNodeConnections->NodeConnections(Count).NodeNum != NodeNumber) continue;
        if (state.dataBranchNodeConnections->NodeConnections(Count).connObjType != ObjectType) continue;
        if (!Util::SameString(state.dataBranchNodeConnections->NodeConnections(Count).ObjectName, ObjectName)) continue;
        if (state.dataBranchNodeConnections->NodeConnections(Count).FluidStream != FluidStream) continue;
        if ((state.dataBranchNodeConnections->NodeConnections(Count).ObjectIsParent != IsParent)) continue;
        Found = Count;
        break;
    }

    if (Found > 0) {
        state.dataBranchNodeConnections->NodeConnections(Found).connType = ConnectionType;
    } else {
        ShowSevereError(state, format("{}{}", RoutineName, "Existing node connection not found."));
        ShowContinueError(
            state,
            format("Occurs for Node={}, ObjectType={}, ObjectName={}", NodeName, Node::connTypeNames[(int)ObjectType], ObjectName));
        errFlag = true;
    }
}

void CheckNodeConnections(EnergyPlusData &state, bool &ErrorsFound)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   March 2004
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine processes the node connection data structure looking at:
    // 1.  In the NodeConnections list, for any node which appears as a sensor or an
    // actuator, the same node must also appear in the connections list at least once
    // as a node type which is not sensor or actuator or outsideair.
    // 2.  In the NodeConnections list, for any node which appears as a setpoint, the
    // same node must also appear in the connections list at least once as a node type
    // which is not a setpoint or outsideair.
    // 3.  Every ZoneInlet must appear as an outlet from something, otherwise it will
    // do nothing.
    // 4.  Every ZoneExhaust must appear as an inlet to something,
    // otherwise it will do nothing.
    // 5.  Every inlet node should match either an Outlet, ZoneReturn, ZoneExhaust, ReliefAir,
    // or OutsideAir node.
    //  With the current data structure, when checking inlets:
    //    a)  If an InletNode's object is AirLoopHVAC, CondenserLoop, or PlantLoop, then skip the test.
    //    b)  If an InletNode's object is not one of the above types, it is valid if the
    //        same node name appears as an INLET to an AirLoopHVAC, CondenserLoop, or PlantLoop.
    // 6.  Any given node can only be an inlet once in the list of Non-Parent Node Connections
    // 7.  Any given node can only be an outlet once in the list of Non-Parent Node Connections
    // 8.  non-parent outlet nodes -- must never be an outlet more than once
    // 9.  nodes of type OutsideAirReference must be registered as Node::NodeConnectionType::OutsideAir
    // 10. fluid streams cannot have multiple inlet/outlet nodes on same component
    // 11. zone nodes may not be used as anything else except as a setpoint, sensor or actuator node

    // METHODOLOGY EMPLOYED:
    // Needs description, as appropriate.

    // Using/Aliasing

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    bool IsValid;
    bool IsInlet;
    bool IsOutlet;
    bool MatchedAtLeastOne;
    int ErrorCounter;
    Array1D_int FluidStreamInletCount;
    Array1D_int FluidStreamOutletCount;
    Array1D_int NodeObjects;
    Array1D_bool FluidStreamCounts;

    ErrorCounter = 0;

    //  Check 1 -- check sensor and actuator nodes
    for (int Loop1 = 1; Loop1 <= state.dataBranchNodeConnections->NumNodeConnections; ++Loop1) {
        if (state.dataBranchNodeConnections->NodeConnections(Loop1).connType != Node::ConnType::Sensor) continue;
        IsValid = false;
        for (int Loop2 = 1; Loop2 <= state.dataBranchNodeConnections->NumNodeConnections; ++Loop2) {
            if (Loop1 == Loop2) continue;
            if (state.dataBranchNodeConnections->NodeConnections(Loop1).NodeNum !=
                state.dataBranchNodeConnections->NodeConnections(Loop2).NodeNum)
                continue;
            if ((state.dataBranchNodeConnections->NodeConnections(Loop2).connType == Node::ConnType::Actuator) ||
                (state.dataBranchNodeConnections->NodeConnections(Loop2).connType == Node::ConnType::Sensor)) {
                continue;
            }

            IsValid = true;
        }
        if (!IsValid) {
            ShowSevereError(state,
                            format("Node Connection Error, Node=\"{}\", Sensor node did not find a matching node of appropriate type (other than "
                                   "Actuator or Sensor).",
                                   state.dataBranchNodeConnections->NodeConnections(Loop1).NodeName));

            ShowContinueError(state,
                              format("Reference Object={}, Name={}",
                                     Node::connObjTypeNames[(int)state.dataBranchNodeConnections->NodeConnections(Loop1).connObjType],
                                     state.dataBranchNodeConnections->NodeConnections(Loop1).ObjectName));
            ++ErrorCounter;
            ErrorsFound = true;
        }
    }

    for (int Loop1 = 1; Loop1 <= state.dataBranchNodeConnections->NumNodeConnections; ++Loop1) {
        if (state.dataBranchNodeConnections->NodeConnections(Loop1).connType != Node::ConnType::Actuator) continue;
        IsValid = false;
        for (int Loop2 = 1; Loop2 <= state.dataBranchNodeConnections->NumNodeConnections; ++Loop2) {
            if (Loop1 == Loop2) continue;
            if (state.dataBranchNodeConnections->NodeConnections(Loop1).NodeNum !=
                state.dataBranchNodeConnections->NodeConnections(Loop2).NodeNum)
                continue;

            if ((state.dataBranchNodeConnections->NodeConnections(Loop2).connType == Node::ConnType::Actuator) ||
                (state.dataBranchNodeConnections->NodeConnections(Loop2).connType == Node::ConnType::Sensor) ||
                (state.dataBranchNodeConnections->NodeConnections(Loop2).connType == Node::ConnType::OutsideAir)) {
                continue;
            }

            IsValid = true;
        }
        if (!IsValid) {
            ShowSevereError(state,
                            format("Node Connection Error, Node=\"{}\", Actuator node did not find a matching node of appropriate type (other than "
                                   "Actuator, Sensor, OutsideAir).",
                                   state.dataBranchNodeConnections->NodeConnections(Loop1).NodeName));

            ShowContinueError(state,
                              format("Reference Object={}, Name={}",
                                     Node::connObjTypeNames[(int)state.dataBranchNodeConnections->NodeConnections(Loop1).connObjType],
                                     state.dataBranchNodeConnections->NodeConnections(Loop1).ObjectName));
            ++ErrorCounter;
            ErrorsFound = true;
        }
    }

    // Check 2 -- setpoint nodes
    // Check 2a -- setpoint node must also be an inlet or an outlet (CR8212)
    for (int Loop1 = 1; Loop1 <= state.dataBranchNodeConnections->NumNodeConnections; ++Loop1) {
        if (state.dataBranchNodeConnections->NodeConnections(Loop1).connType != Node::ConnType::SetPoint) continue;
        IsValid = false;
        IsInlet = false;
        IsOutlet = false;
        for (int Loop2 = 1; Loop2 <= state.dataBranchNodeConnections->NumNodeConnections; ++Loop2) {
            if (Loop1 == Loop2) continue;
            if (state.dataBranchNodeConnections->NodeConnections(Loop1).NodeNum !=
                state.dataBranchNodeConnections->NodeConnections(Loop2).NodeNum)
                continue;
            if ((state.dataBranchNodeConnections->NodeConnections(Loop2).connType == Node::ConnType::SetPoint) ||
                (state.dataBranchNodeConnections->NodeConnections(Loop2).connType == Node::ConnType::OutsideAir)) {
                continue;
            }

            if (state.dataBranchNodeConnections->NodeConnections(Loop2).connType == Node::ConnType::Inlet) {
                IsInlet = true;
            } else if (state.dataBranchNodeConnections->NodeConnections(Loop2).connType == Node::ConnType::Outlet) {
                IsOutlet = true;
            }
            IsValid = true;
        }
        if (!IsValid) {
            ShowSevereError(state,
                            format("Node Connection Error, Node=\"{}\", Setpoint node did not find a matching node of appropriate type (other than "
                                   "Setpoint, OutsideAir).",
                                   state.dataBranchNodeConnections->NodeConnections(Loop1).NodeName));

            ShowContinueError(state,
                              format("Reference Object={}, Name={}",
                                     Node::connObjTypeNames[(int)state.dataBranchNodeConnections->NodeConnections(Loop1).connObjType],
                                     state.dataBranchNodeConnections->NodeConnections(Loop1).ObjectName));
            ++ErrorCounter;
            ErrorsFound = true;
        }
        if (!IsInlet && !IsOutlet) {
            ShowSevereError(state,
                            format("Node Connection Error, Node=\"{}\", Setpoint node did not find a matching node of type Inlet or Outlet.",
                                   state.dataBranchNodeConnections->NodeConnections(Loop1).NodeName));
            ShowContinueError(state, "It appears this node is not part of the HVAC system.");

            ShowContinueError(state,
                              format("Reference Object={}, Name={}",
                                     Node::connObjTypeNames[(int)state.dataBranchNodeConnections->NodeConnections(Loop1).connObjType],
                                     state.dataBranchNodeConnections->NodeConnections(Loop1).ObjectName));
            ++ErrorCounter;
        }
    }

    // Check 2a -- setpoint node must also be an inlet or an outlet (CR8212)

    // Check 3 -- zone inlet nodes -- must be an outlet somewhere
    for (int Loop1 = 1; Loop1 <= state.dataBranchNodeConnections->NumNodeConnections; ++Loop1) {
        if (state.dataBranchNodeConnections->NodeConnections(Loop1).connType != Node::ConnType::ZoneInlet) continue;
        IsValid = false;
        for (int Loop2 = 1; Loop2 <= state.dataBranchNodeConnections->NumNodeConnections; ++Loop2) {
            if (Loop1 == Loop2) continue;
            if (state.dataBranchNodeConnections->NodeConnections(Loop1).NodeNum !=
                state.dataBranchNodeConnections->NodeConnections(Loop2).NodeNum)
                continue;
            if (state.dataBranchNodeConnections->NodeConnections(Loop2).connType != Node::ConnType::Outlet) continue;
            IsValid = true;
        }
        if (!IsValid) {
            ShowSevereError(state,
                            format("Node Connection Error, Node=\"{}\", ZoneInlet node did not find an outlet node.",
                                   state.dataBranchNodeConnections->NodeConnections(Loop1).NodeName));

            ShowContinueError(state,
                              format("Reference Object={}, Name={}",
                                     Node::connObjTypeNames[(int)state.dataBranchNodeConnections->NodeConnections(Loop1).connObjType],
                                     state.dataBranchNodeConnections->NodeConnections(Loop1).ObjectName));
            ++ErrorCounter;
        }
    }

    // Check 4 -- zone exhaust nodes -- must be an inlet somewhere
    for (int Loop1 = 1; Loop1 <= state.dataBranchNodeConnections->NumNodeConnections; ++Loop1) {
        if (state.dataBranchNodeConnections->NodeConnections(Loop1).connType != Node::ConnType::ZoneExhaust) continue;
        IsValid = false;
        for (int Loop2 = 1; Loop2 <= state.dataBranchNodeConnections->NumNodeConnections; ++Loop2) {
            if (Loop1 == Loop2) continue;
            if (state.dataBranchNodeConnections->NodeConnections(Loop1).NodeNum !=
                state.dataBranchNodeConnections->NodeConnections(Loop2).NodeNum)
                continue;
            if (state.dataBranchNodeConnections->NodeConnections(Loop2).connType != Node::ConnType::Inlet) continue;
            IsValid = true;
        }
        if (!IsValid) {
            ShowSevereError(state,
                            format("Node Connection Error, Node=\"{}\", ZoneExhaust node did not find a matching inlet node.",
                                   state.dataBranchNodeConnections->NodeConnections(Loop1).NodeName));

            ShowContinueError(state,
                              format("Reference Object={}, Name={}",
                                     Node::connObjTypeNames[(int)state.dataBranchNodeConnections->NodeConnections(Loop1).connObjType],
                                     state.dataBranchNodeConnections->NodeConnections(Loop1).ObjectName));
            ++ErrorCounter;
        }
    }

    // Check 5 -- return plenum induced air outlet nodes -- must be an inlet somewhere
    for (int Loop1 = 1; Loop1 <= state.dataBranchNodeConnections->NumNodeConnections; ++Loop1) {
        if (state.dataBranchNodeConnections->NodeConnections(Loop1).connType != Node::ConnType::InducedAir) continue;
        IsValid = false;
        for (int Loop2 = 1; Loop2 <= state.dataBranchNodeConnections->NumNodeConnections; ++Loop2) {
            if (Loop1 == Loop2) continue;
            if (state.dataBranchNodeConnections->NodeConnections(Loop1).NodeNum !=
                state.dataBranchNodeConnections->NodeConnections(Loop2).NodeNum)
                continue;
            if (state.dataBranchNodeConnections->NodeConnections(Loop2).connType != Node::ConnType::Inlet) continue;
            IsValid = true;
        }
        if (!IsValid) {
            ShowSevereError(state,
                            format("Node Connection Error, Node=\"{}\", Return plenum induced air outlet node did not find a matching inlet node.",
                                   state.dataBranchNodeConnections->NodeConnections(Loop1).NodeName));

            ShowContinueError(state,
                              format("Reference Object={}, Name={}",
                                     Node::connObjTypeNames[(int)state.dataBranchNodeConnections->NodeConnections(Loop1).connObjType],
                                     state.dataBranchNodeConnections->NodeConnections(Loop1).ObjectName));
            ++ErrorCounter;
            ErrorsFound = true;
        }
    }

    // Check 6 -- every inlet should have a matching outlet, zonereturn, zoneexhaust, induced air, reliefair or outsideair
    //    a)  If an InletNode's object is AirLoopHVAC, CondenserLoop, or PlantLoop, then skip the test.
    //    b)  If an InletNode's object is not one of the above types, it is valid if the
    //        same node name appears as an INLET to an AirLoopHVAC, CondenserLoop, or PlantLoop.
    for (int Loop1 = 1; Loop1 <= state.dataBranchNodeConnections->NumNodeConnections; ++Loop1) {
        if (state.dataBranchNodeConnections->NodeConnections(Loop1).connType != Node::ConnType::Inlet) continue;
        if (state.dataBranchNodeConnections->NodeConnections(Loop1).connObjType == Node::ConnObjType::AirLoopHVAC ||
            state.dataBranchNodeConnections->NodeConnections(Loop1).connObjType == Node::ConnObjType::CondenserLoop ||
            state.dataBranchNodeConnections->NodeConnections(Loop1).connObjType == Node::ConnObjType::PlantLoop)
            continue;
        IsValid = false;
        MatchedAtLeastOne = false;
        for (int Loop2 = 1; Loop2 <= state.dataBranchNodeConnections->NumNodeConnections; ++Loop2) {
            if (Loop1 == Loop2) continue;
            if (state.dataBranchNodeConnections->NodeConnections(Loop1).NodeNum !=
                state.dataBranchNodeConnections->NodeConnections(Loop2).NodeNum)
                continue;

            if ((state.dataBranchNodeConnections->NodeConnections(Loop2).connType == Node::ConnType::Outlet) ||
                (state.dataBranchNodeConnections->NodeConnections(Loop2).connType == Node::ConnType::ZoneReturn) ||
                (state.dataBranchNodeConnections->NodeConnections(Loop2).connType == Node::ConnType::ZoneExhaust) ||
                (state.dataBranchNodeConnections->NodeConnections(Loop2).connType == Node::ConnType::InducedAir) ||
                (state.dataBranchNodeConnections->NodeConnections(Loop2).connType == Node::ConnType::ReliefAir) ||
                (state.dataBranchNodeConnections->NodeConnections(Loop2).connType == Node::ConnType::OutsideAir)) {
                MatchedAtLeastOne = true;
                continue;
            }

            if (state.dataBranchNodeConnections->NodeConnections(Loop2).connType == Node::ConnType::Inlet &&
                (state.dataBranchNodeConnections->NodeConnections(Loop2).connObjType == Node::ConnObjType::AirLoopHVAC ||
                 state.dataBranchNodeConnections->NodeConnections(Loop2).connObjType == Node::ConnObjType::CondenserLoop ||
                 state.dataBranchNodeConnections->NodeConnections(Loop2).connObjType == Node::ConnObjType::PlantLoop)) {
                MatchedAtLeastOne = true;
                continue;
            }
            IsValid = false;
        }
        if (!IsValid && !MatchedAtLeastOne) {
            ShowSevereError(state,
                            format("{}{}{}",
                                   "Node Connection Error, Node=\"",
                                   state.dataBranchNodeConnections->NodeConnections(Loop1).NodeName,
                                   R"(", Inlet node did not find an appropriate matching "outlet" node.)"));
            ShowContinueError(state, "If this is an outdoor air inlet node, it must be listed in an OutdoorAir:Node or OutdoorAir:NodeList object.");

            ShowContinueError(state,
                              format("Reference Object={}, Name={}",
                                     Node::connObjTypeNames[(int)state.dataBranchNodeConnections->NodeConnections(Loop1).connObjType],
                                     state.dataBranchNodeConnections->NodeConnections(Loop1).ObjectName));
            ++ErrorCounter;
        }
    }

    // Check 7 -- non-parent inlet nodes -- must never be an inlet more than once
    for (int Loop1 = 1; Loop1 <= state.dataBranchNodeConnections->NumNodeConnections; ++Loop1) {
        // Only non-parent node connections
        if (state.dataBranchNodeConnections->NodeConnections(Loop1).ObjectIsParent) continue;
        if (state.dataBranchNodeConnections->NodeConnections(Loop1).connType != Node::ConnType::Inlet) continue;
        for (int Loop2 = Loop1; Loop2 <= state.dataBranchNodeConnections->NumNodeConnections; ++Loop2) {
            if (Loop1 == Loop2) continue;
            if (state.dataBranchNodeConnections->NodeConnections(Loop2).ObjectIsParent) continue;
            if (state.dataBranchNodeConnections->NodeConnections(Loop2).connType != Node::ConnType::Inlet) continue;
            if (state.dataBranchNodeConnections->NodeConnections(Loop2).NodeNum ==
                state.dataBranchNodeConnections->NodeConnections(Loop1).NodeNum) {
                ShowSevereError(state,
                                format("Node Connection Error, Node=\"{}\", The same node appears as a non-parent Inlet node more than once.",
                                       state.dataBranchNodeConnections->NodeConnections(Loop1).NodeName));

                ShowContinueError(
                    state,
                    format("Reference Object={}, Name={}",
                           Node::connObjTypeNames[(int)state.dataBranchNodeConnections->NodeConnections(Loop1).connObjType],
                           state.dataBranchNodeConnections->NodeConnections(Loop1).ObjectName));

                ShowContinueError(
                    state,
                    format("Reference Object={}, Name={}",
                           Node::connObjTypeNames[(int)state.dataBranchNodeConnections->NodeConnections(Loop2).connObjType],
                           state.dataBranchNodeConnections->NodeConnections(Loop2).ObjectName));
                ++ErrorCounter;
                break;
            }
        }
    }

    // Check 8 -- non-parent outlet nodes -- must never be an outlet more than once
    for (int Loop1 = 1; Loop1 <= state.dataBranchNodeConnections->NumNodeConnections; ++Loop1) {
        // Only non-parent node connections
        if (state.dataBranchNodeConnections->NodeConnections(Loop1).ObjectIsParent) continue;
        if (state.dataBranchNodeConnections->NodeConnections(Loop1).connType != Node::ConnType::Outlet) continue;
        IsValid = true;
        for (int Loop2 = Loop1; Loop2 <= state.dataBranchNodeConnections->NumNodeConnections; ++Loop2) {
            if (Loop1 == Loop2) continue;
            if (state.dataBranchNodeConnections->NodeConnections(Loop2).ObjectIsParent) continue;
            if (state.dataBranchNodeConnections->NodeConnections(Loop2).connType != Node::ConnType::Outlet) continue;
            if (state.dataBranchNodeConnections->NodeConnections(Loop2).NodeNum ==
                state.dataBranchNodeConnections->NodeConnections(Loop1).NodeNum) {
                // Skip if one of the
                ShowSevereError(state,
                                format("Node Connection Error, Node=\"{}\", The same node appears as a non-parent Outlet node more than once.",
                                       state.dataBranchNodeConnections->NodeConnections(Loop1).NodeName));

                ShowContinueError(
                    state,
                    format("Reference Object={}, Name={}",
                           Node::connObjTypeNames[(int)state.dataBranchNodeConnections->NodeConnections(Loop1).connObjType],
                           state.dataBranchNodeConnections->NodeConnections(Loop1).ObjectName));

                ShowContinueError(
                    state,
                    format("Reference Object={}, Name={}",
                           Node::connObjTypeNames[(int)state.dataBranchNodeConnections->NodeConnections(Loop2).connObjType],
                           state.dataBranchNodeConnections->NodeConnections(Loop2).ObjectName));

                ++ErrorCounter;
                break;
            }
        }
    }

    // Check 9 -- nodes of type OutsideAirReference must be registered as Node::NodeConnectionType::OutsideAir
    for (int Loop1 = 1; Loop1 <= state.dataBranchNodeConnections->NumNodeConnections; ++Loop1) {
        if (state.dataBranchNodeConnections->NodeConnections(Loop1).connType != Node::ConnType::OutsideAirReference) continue;
        IsValid = false;
        for (int Loop2 = 1; Loop2 <= state.dataBranchNodeConnections->NumNodeConnections; ++Loop2) {
            if (Loop1 == Loop2) continue;
            if (state.dataBranchNodeConnections->NodeConnections(Loop1).NodeNum !=
                state.dataBranchNodeConnections->NodeConnections(Loop2).NodeNum)
                continue;
            if (state.dataBranchNodeConnections->NodeConnections(Loop2).connType != Node::ConnType::OutsideAir) continue;
            IsValid = true;
            break;
        }
        if (!IsValid) {
            ShowSevereError(state,
                            format("{}{}{}",
                                   "Node Connection Error, Node=\"",
                                   state.dataBranchNodeConnections->NodeConnections(Loop1).NodeName,
                                   R"(", Outdoor Air Reference did not find an appropriate "outdoor air" node.)"));
            ShowContinueError(state, "This node must be listed in an OutdoorAir:Node or OutdoorAir:NodeList object in order to set its conditions.");

            ShowContinueError(state,
                              format("Reference Object={}, Name={}",
                                     Node::connObjTypeNames[(int)state.dataBranchNodeConnections->NodeConnections(Loop1).connObjType],
                                     state.dataBranchNodeConnections->NodeConnections(Loop1).ObjectName));

            ++ErrorCounter;
        }
    }

    // Check 10 -- fluid streams cannot have multiple inlet/outlet nodes on same component
    //  can have multiple inlets with one outlet or vice versa but cannot have multiple both inlet and outlet
    if (state.dataBranchNodeConnections->NumNodeConnections > 0) {
        int MaxFluidStream = (int)maxval(state.dataBranchNodeConnections->NodeConnections, &NodeConnectionDef::FluidStream);
        FluidStreamInletCount.allocate(MaxFluidStream);
        FluidStreamOutletCount.allocate(MaxFluidStream);
        FluidStreamCounts.allocate(MaxFluidStream);
        NodeObjects.allocate(state.dataBranchNodeConnections->NumNodeConnections + 1);
        FluidStreamInletCount = 0;
        FluidStreamOutletCount = 0;
        NodeObjects = 0;
        FluidStreamCounts = false;
        // Following code relies on node connections for single object type/name being grouped together
        int Object = 1;
        int EndConnect = 0;
        int NumObjects = 2;
        NodeObjects(1) = 1;
        while (Object < state.dataBranchNodeConnections->NumNodeConnections) {
            if (state.dataBranchNodeConnections->NodeConnections(Object).connObjType !=
                    state.dataBranchNodeConnections->NodeConnections(Object + 1).connObjType ||
                state.dataBranchNodeConnections->NodeConnections(Object).ObjectName !=
                    state.dataBranchNodeConnections->NodeConnections(Object + 1).ObjectName) {
                EndConnect = Object + 1;
                NodeObjects(NumObjects) = EndConnect;
                // if (Object + 1 < state.dataBranchNodeConnections->NumOfNodeConnections) ++NumObjects;
                ++NumObjects;
            }
            ++Object;
        }
        NodeObjects(NumObjects) = state.dataBranchNodeConnections->NumNodeConnections + 1;
        // NodeObjects now contains each consecutive object...
        for (Object = 1; Object <= NumObjects - 1; ++Object) {
            IsValid = true;
            FluidStreamInletCount = 0;
            FluidStreamOutletCount = 0;
            FluidStreamCounts = false;
            int Loop1 = NodeObjects(Object);
            if (state.dataBranchNodeConnections->NumNodeConnections < 2) continue;
            if (state.dataBranchNodeConnections->NodeConnections(Loop1).ObjectIsParent) continue;
            if (state.dataBranchNodeConnections->NodeConnections(Loop1).connType == Node::ConnType::Inlet) {
                ++FluidStreamInletCount((int)state.dataBranchNodeConnections->NodeConnections(Loop1).FluidStream);
            } else if (state.dataBranchNodeConnections->NodeConnections(Loop1).connType == Node::ConnType::Outlet) {
                ++FluidStreamOutletCount((int)state.dataBranchNodeConnections->NodeConnections(Loop1).FluidStream);
            }
            for (int Loop2 = Loop1 + 1; Loop2 <= NodeObjects(Object + 1) - 1; ++Loop2) {
                if (state.dataBranchNodeConnections->NodeConnections(Loop2).ObjectIsParent) continue;
                if (state.dataBranchNodeConnections->NodeConnections(Loop2).connType == Node::ConnType::Inlet) {
                    ++FluidStreamInletCount((int)state.dataBranchNodeConnections->NodeConnections(Loop2).FluidStream);
                } else if (state.dataBranchNodeConnections->NodeConnections(Loop2).connType == Node::ConnType::Outlet) {
                    ++FluidStreamOutletCount((int)state.dataBranchNodeConnections->NodeConnections(Loop2).FluidStream);
                }
            }
            for (int Loop2 = 1; Loop2 <= MaxFluidStream; ++Loop2) {
                if (FluidStreamInletCount(Loop2) > 1 && FluidStreamOutletCount(Loop2) > 1) {
                    IsValid = false;
                    FluidStreamCounts(Loop2) = true;
                }
            }
            if (!IsValid) {

                ShowSevereError(
                    state,
                    format("(Developer) Node Connection Error, Object={}:{}",
                           Node::connObjTypeNames[(int)state.dataBranchNodeConnections->NodeConnections(Loop1).connObjType],
                           state.dataBranchNodeConnections->NodeConnections(Loop1).ObjectName));

                ShowContinueError(state, "Object has multiple connections on both inlet and outlet fluid streams.");
                for (int Loop2 = 1; Loop2 <= MaxFluidStream; ++Loop2) {
                    if (FluidStreamCounts(Loop2)) ShowContinueError(state, format("...occurs in Fluid Stream [{}].", Loop2));
                }
                ++ErrorCounter;
                ErrorsFound = true;
            }
        }
        FluidStreamInletCount.deallocate();
        FluidStreamOutletCount.deallocate();
        FluidStreamCounts.deallocate();
        NodeObjects.deallocate();
    }

    // Check 11 - zone nodes may not be used as anything else except as a setpoint, sensor or actuator node
    for (int Loop1 = 1; Loop1 <= state.dataBranchNodeConnections->NumNodeConnections; ++Loop1) {
        if (state.dataBranchNodeConnections->NodeConnections(Loop1).connType != Node::ConnType::ZoneNode) continue;
        IsValid = true;
        for (int Loop2 = Loop1; Loop2 <= state.dataBranchNodeConnections->NumNodeConnections; ++Loop2) {
            if (Loop1 == Loop2) continue;
            if (state.dataBranchNodeConnections->NodeConnections(Loop1).NodeName ==
                state.dataBranchNodeConnections->NodeConnections(Loop2).NodeName) {

                if ((state.dataBranchNodeConnections->NodeConnections(Loop2).connType == Node::ConnType::Actuator) ||
                    (state.dataBranchNodeConnections->NodeConnections(Loop2).connType == Node::ConnType::Sensor) ||
                    (state.dataBranchNodeConnections->NodeConnections(Loop2).connType == Node::ConnType::SetPoint)) {
                    continue;
                }

                ShowSevereError(state,
                                format("Node Connection Error, Node Name=\"{}\", The same zone node appears more than once.",
                                       state.dataBranchNodeConnections->NodeConnections(Loop1).NodeName));

                ShowContinueError(
                    state,
                    format("Reference Object={}, Object Name={}",
                           Node::connObjTypeNames[(int)state.dataBranchNodeConnections->NodeConnections(Loop1).connObjType],
                           state.dataBranchNodeConnections->NodeConnections(Loop1).ObjectName));

                ShowContinueError(
                    state,
                    format("Reference Object={}, Object Name={}",
                           Node::connObjTypeNames[(int)state.dataBranchNodeConnections->NodeConnections(Loop2).connObjType],
                           state.dataBranchNodeConnections->NodeConnections(Loop2).ObjectName));

                ++ErrorCounter;
                ErrorsFound = true;
            }
        }
    }

    state.dataBranchNodeConnections->NumNodeConnectionErrors += ErrorCounter;
}

bool IsParentObject(EnergyPlusData &state, Node::ConnObjType const ComponentType, std::string const &ComponentName)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   May 2005

    // PURPOSE OF THIS FUNCTION:
    // This routine determines if a component name is a parent node.

    // METHODOLOGY EMPLOYED:
    // Traverses CompSet structure.

    // Return value
    bool IsParent = false; // True if this combination is a parent

    for (int Loop = 1; Loop <= state.dataBranchNodeConnections->NumNodeConnections; ++Loop) {
        if (state.dataBranchNodeConnections->NodeConnections(Loop).connObjType == ComponentType &&
            state.dataBranchNodeConnections->NodeConnections(Loop).ObjectName == ComponentName) {
            if (state.dataBranchNodeConnections->NodeConnections(Loop).ObjectIsParent) {
                IsParent = true;
            }
            break;
        }
    }
    if (!IsParent) {
        IsParent = IsParentObjectCompSet(state, ComponentType, ComponentName);
    }

    return IsParent;
}

int WhichParentSet(EnergyPlusData &state, Node::ConnObjType const ComponentType, std::string const &ComponentName)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   May 2005

    // PURPOSE OF THIS FUNCTION:
    // This routine determines which parent node list (number) for a given component name
    // and type.

    // Return value
    int WhichOne = 0;

    for (int Loop = 1; Loop <= state.dataBranchNodeConnections->NumActualParents; ++Loop) {
        if (state.dataBranchNodeConnections->ParentNodeList(Loop).ComponentType == ComponentType &&
            state.dataBranchNodeConnections->ParentNodeList(Loop).ComponentName == ComponentName) {
            WhichOne = Loop;
            break;
        }
    }

    return WhichOne;
}

void GetParentData(EnergyPlusData &state,
                   Node::ConnObjType const ComponentType,
                   std::string const &ComponentName,
                   std::string &InNodeName,
                   int &InNodeNum,
                   std::string &OutNodeName,
                   int &OutNodeNum,
                   bool &ErrorsFound)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   May 2005

    // PURPOSE OF THIS SUBROUTINE:
    // This routine gets node data for a given Parent Component Type and Name Name.

    // METHODOLOGY EMPLOYED:
    // Traverses CompSet structure.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    bool ErrInObject = false;

    InNodeName = std::string();
    InNodeNum = 0;
    OutNodeName = std::string();
    OutNodeNum = 0;
    ErrInObject = false;

    int Which = WhichParentSet(state, ComponentType, ComponentName);
    if (Which != 0) {
        InNodeName = state.dataBranchNodeConnections->ParentNodeList(Which).InNodeName;
        OutNodeName = state.dataBranchNodeConnections->ParentNodeList(Which).OutNodeName;
        // Get Node Numbers
        InNodeNum = Node::GetNodeIndex(state, InNodeName);
        OutNodeNum = Node::GetNodeIndex(state, OutNodeName);
    } else if (IsParentObjectCompSet(state, ComponentType, ComponentName)) {
        Which = WhichCompSet(state, ComponentType, ComponentName);
        if (Which != 0) {
            InNodeName = state.dataBranchNodeConnections->CompSets(Which).InNodeName;
            OutNodeName = state.dataBranchNodeConnections->CompSets(Which).OutNodeName;
            InNodeNum = Node::GetNodeIndex(state, InNodeName);
            OutNodeNum = Node::GetNodeIndex(state, OutNodeName);
        } else {
            ErrInObject = true;
            ShowWarningError(state,
                             format("GetParentData: Component Type={}, Component Name={} not found.",
                                    Node::connObjTypeNames[(int)ComponentType],
                                    ComponentName));
        }
    } else {
        ErrInObject = true;
        ShowWarningError(state,
                         format("GetParentData: Component Type={}, Component Name={} not found.",
                                Node::connObjTypeNames[(int)ComponentType],
                                ComponentName));
    }

    if (ErrInObject) ErrorsFound = true;
}

bool IsParentObjectCompSet(EnergyPlusData &state, Node::ConnObjType const ComponentType, std::string const &ComponentName)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   May 2005

    // PURPOSE OF THIS FUNCTION:
    // This routine determines if a component name is a parent node.

    // METHODOLOGY EMPLOYED:
    // Traverses CompSet structure.

    // Return value
    bool IsParent = false; // True if this combination is a parent

    for (int Loop = 1; Loop <= state.dataBranchNodeConnections->NumCompSets; ++Loop) {
        if (state.dataBranchNodeConnections->CompSets(Loop).ParentObjectType == ComponentType &&
            state.dataBranchNodeConnections->CompSets(Loop).ParentCName == ComponentName) {
            IsParent = true;
            break;
        }
    }

    return IsParent;
}

int WhichCompSet(EnergyPlusData &state, Node::ConnObjType const ComponentType, std::string const &ComponentName)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   May 2005

    // PURPOSE OF THIS FUNCTION:
    // This routine determines which comp set (number) for a given component name
    // and type.

    // METHODOLOGY EMPLOYED:
    // Traverses CompSet structure.

    // Return value
    int WhichOne = 0;

    for (int Loop = 1; Loop <= state.dataBranchNodeConnections->NumCompSets; ++Loop) {
        if (state.dataBranchNodeConnections->CompSets(Loop).ComponentObjectType == ComponentType &&
            state.dataBranchNodeConnections->CompSets(Loop).CName == ComponentName) {
            WhichOne = Loop;
            break;
        }
    }

    return WhichOne;
}

int GetNumChildren(EnergyPlusData &state, Node::ConnObjType const ComponentType, std::string const &ComponentName)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   May 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This routine counts the number of children for a parent Component Set.

    // METHODOLOGY EMPLOYED:
    // Traverses CompSet structure.

    // Return value
    int NumChildren;

    NumChildren = 0;
    if (IsParentObject(state, ComponentType, ComponentName)) {
        for (int Loop = 1; Loop <= state.dataBranchNodeConnections->NumCompSets; ++Loop) {
            if (state.dataBranchNodeConnections->CompSets(Loop).ParentObjectType == ComponentType &&
                state.dataBranchNodeConnections->CompSets(Loop).ParentCName == ComponentName) {
                ++NumChildren;
            }
        }
    }

    return NumChildren;
}

void GetComponentData(EnergyPlusData &state,
                      Node::ConnObjType const ComponentType,
                      std::string const &ComponentName,
                      bool &IsParent, // true or false
                      int &NumInlets,
                      Array1D_string &InNodeNames,
                      Array1D_int &InNodeNums,
                      Array1D<Node::CompFluidStream> &InletFluidStreams,
                      int &NumOutlets,
                      Array1D_string &OutNodeNames,
                      Array1D_int &OutNodeNums,
                      Array1D<Node::CompFluidStream> &OutletFluidStreams)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   May 2005

    // PURPOSE OF THIS SUBROUTINE:
    // This routine gets data for a given Component Type and Name Name.

    // METHODOLOGY EMPLOYED:
    // Traverses CompSet structure.

    if (allocated(InNodeNames)) InNodeNames.deallocate();
    if (allocated(InNodeNums)) InNodeNums.deallocate();
    if (allocated(InletFluidStreams)) InletFluidStreams.deallocate();
    if (allocated(OutNodeNames)) OutNodeNames.deallocate();
    if (allocated(OutNodeNums)) OutNodeNums.deallocate();
    if (allocated(OutletFluidStreams)) OutletFluidStreams.deallocate();

    NumInlets = 0;
    NumOutlets = 0;

    IsParent = false;
    for (int Which = 1; Which <= state.dataBranchNodeConnections->NumNodeConnections; ++Which) {
        if (state.dataBranchNodeConnections->NodeConnections(Which).connObjType != ComponentType ||
            state.dataBranchNodeConnections->NodeConnections(Which).ObjectName != ComponentName)
            continue;
        if (state.dataBranchNodeConnections->NodeConnections(Which).ObjectIsParent) IsParent = true;
        if (state.dataBranchNodeConnections->NodeConnections(Which).connType == Node::ConnType::Inlet) {
            ++NumInlets;
        } else if (state.dataBranchNodeConnections->NodeConnections(Which).connType == Node::ConnType::Outlet) {
            ++NumOutlets;
        }
    }

    InNodeNames.allocate(NumInlets);
    InNodeNums.allocate(NumInlets);
    InletFluidStreams.allocate(NumInlets);
    OutNodeNames.allocate(NumOutlets);
    OutNodeNums.allocate(NumOutlets);
    OutletFluidStreams.allocate(NumOutlets);

    InNodeNames = std::string();
    InNodeNums = 0;
    InletFluidStreams = Node::CompFluidStream::Invalid;
    OutNodeNames = std::string();
    OutNodeNums = 0;
    OutletFluidStreams = Node::CompFluidStream::Invalid;
    NumInlets = 0;
    NumOutlets = 0;

    for (int Which = 1; Which <= state.dataBranchNodeConnections->NumNodeConnections; ++Which) {
        if (state.dataBranchNodeConnections->NodeConnections(Which).connObjType != ComponentType ||
            state.dataBranchNodeConnections->NodeConnections(Which).ObjectName != ComponentName)
            continue;
        if (state.dataBranchNodeConnections->NodeConnections(Which).connType == Node::ConnType::Inlet) {
            ++NumInlets;
            InNodeNames(NumInlets) = state.dataBranchNodeConnections->NodeConnections(Which).NodeName;
            InNodeNums(NumInlets) = state.dataBranchNodeConnections->NodeConnections(Which).NodeNum;
            InletFluidStreams(NumInlets) = state.dataBranchNodeConnections->NodeConnections(Which).FluidStream;
        } else if (state.dataBranchNodeConnections->NodeConnections(Which).connType == Node::ConnType::Outlet) {
            ++NumOutlets;
            OutNodeNames(NumOutlets) = state.dataBranchNodeConnections->NodeConnections(Which).NodeName;
            OutNodeNums(NumOutlets) = state.dataBranchNodeConnections->NodeConnections(Which).NodeNum;
            OutletFluidStreams(NumOutlets) = state.dataBranchNodeConnections->NodeConnections(Which).FluidStream;
        }
    }
}

void GetChildrenData(EnergyPlusData &state,
                     Node::ConnObjType const ComponentType,
                     std::string const &ComponentName,
                     int &NumChildren,
                     EPVector<Node::ConnObjType> &ChildrenCType,
                     Array1D_string &ChildrenCName,
                     Array1D_string &InNodeName,
                     Array1D_int &InNodeNum,
                     Array1D_string &OutNodeName,
                     Array1D_int &OutNodeNum,
                     bool &ErrorsFound)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   May 2005

    // PURPOSE OF THIS SUBROUTINE:
    // This routine gets children data for given parent node.

    // METHODOLOGY EMPLOYED:
    // Traverses CompSet structure.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    EPVector<Node::ConnObjType> ChildCType;
    Array1D_string ChildCName;
    Array1D_string ChildInNodeName;
    Array1D_string ChildOutNodeName;
    Array1D_int ChildInNodeNum;
    Array1D_int ChildOutNodeNum;
    bool ErrInObject;

    std::fill(ChildrenCType.begin(), ChildrenCType.end(), Node::ConnObjType::Invalid);
    ChildrenCName = std::string();
    InNodeName = std::string();
    InNodeNum = 0;
    OutNodeName = std::string();
    OutNodeNum = 0;
    ErrInObject = false;

    if (IsParentObject(state, ComponentType, ComponentName)) {
        NumChildren = GetNumChildren(state, ComponentType, ComponentName);
        if (NumChildren == 0) {
            ShowWarningError(state,
                             format("GetChildrenData: Parent Node has no children, node={}:{}.",
                                    Node::connObjTypeNames[(int)ComponentType],
                                    ComponentName));
        } else {
            int ParentInNodeNum;
            int ParentOutNodeNum;
            std::string ParentInNodeName;
            std::string ParentOutNodeName;
            GetParentData(
                state, ComponentType, ComponentName, ParentInNodeName, ParentInNodeNum, ParentOutNodeName, ParentOutNodeNum, ErrInObject);
            ChildCType.clear();
            ChildCType.allocate(NumChildren);
            ChildCName.allocate(NumChildren);
            ChildInNodeName.allocate(NumChildren);
            ChildOutNodeName.allocate(NumChildren);
            ChildInNodeNum.allocate(NumChildren);
            ChildOutNodeNum.allocate(NumChildren);
            ChildCName = std::string();
            ChildInNodeName = std::string();
            ChildOutNodeName = std::string();
            ChildInNodeNum = 0;
            ChildOutNodeNum = 0;
            int CountNum = 0;
            for (int Loop = 1; Loop <= state.dataBranchNodeConnections->NumCompSets; ++Loop) {
                if (state.dataBranchNodeConnections->CompSets(Loop).ParentObjectType == ComponentType &&
                    state.dataBranchNodeConnections->CompSets(Loop).ParentCName == ComponentName) {
                    ++CountNum;
                    ChildCType(CountNum) = state.dataBranchNodeConnections->CompSets(Loop).ComponentObjectType;
                    ChildCName(CountNum) = state.dataBranchNodeConnections->CompSets(Loop).CName;
                    ChildInNodeName(CountNum) = state.dataBranchNodeConnections->CompSets(Loop).InNodeName;
                    ChildOutNodeName(CountNum) = state.dataBranchNodeConnections->CompSets(Loop).OutNodeName;
                    // Get Node Numbers
                    ChildInNodeNum(CountNum) = Node::GetNodeIndex(state, ChildInNodeName(CountNum));
                    ChildOutNodeNum(CountNum) = Node::GetNodeIndex(state, ChildOutNodeName(CountNum));
                }
            }
            if (CountNum != NumChildren) {
                ShowSevereError(state, "GetChildrenData: Counted nodes not equal to GetNumChildren count");
                ErrInObject = true;
            } else {
                // Children arrays built.  Now "sort" for flow connection order(?)
                std::string MatchNodeName = ParentInNodeName;
                CountNum = 0;
                int CountMatchLoop = 0;
                while (CountMatchLoop < NumChildren) {
                    ++CountMatchLoop;
                    //          Matched=.FALSE.
                    for (int Loop = 1; Loop <= NumChildren; ++Loop) {
                        if (ChildInNodeName(Loop) == MatchNodeName) {
                            ++CountNum;
                            ChildrenCType(CountNum) = ChildCType(Loop);
                            ChildrenCName(CountNum) = ChildCName(Loop);
                            InNodeName(CountNum) = ChildInNodeName(Loop);
                            InNodeNum(CountNum) = ChildInNodeNum(Loop);
                            OutNodeName(CountNum) = ChildOutNodeName(Loop);
                            OutNodeNum(CountNum) = ChildOutNodeNum(Loop);
                            ChildInNodeName(Loop).clear(); // So it won't match anymore
                            //              Matched=.TRUE.
                            MatchNodeName = ChildOutNodeName(Loop);
                            break;
                        }
                    }
                }
                if (MatchNodeName != ParentOutNodeName) {
                    for (int Loop = 1; Loop <= NumChildren; ++Loop) {
                        if (ChildInNodeName(Loop).empty()) continue;
                        if (ChildOutNodeName(Loop) == ParentOutNodeName) break;
                        break;
                    }
                }
                for (int Loop = 1; Loop <= NumChildren; ++Loop) {
                    if (ChildInNodeName(Loop).empty()) continue;
                    ++CountNum;
                    ChildrenCType(CountNum) = ChildCType(Loop);
                    ChildrenCName(CountNum) = ChildCName(Loop);
                    InNodeName(CountNum) = ChildInNodeName(Loop);
                    InNodeNum(CountNum) = ChildInNodeNum(Loop);
                    OutNodeName(CountNum) = ChildOutNodeName(Loop);
                    OutNodeNum(CountNum) = ChildOutNodeNum(Loop);
                }
                ChildCType.deallocate();
                ChildCName.deallocate();
                ChildInNodeName.deallocate();
                ChildOutNodeName.deallocate();
                ChildInNodeNum.deallocate();
                ChildOutNodeNum.deallocate();
            }
        }
    } else {
        ShowWarningError(state,
                         format("GetChildrenData: Requested Children Data for non Parent Node={}:{}.",
                                Node::connObjTypeNames[(int)ComponentType],
                                ComponentName));
        ErrInObject = true;
    }

    if (ErrInObject) ErrorsFound = true;
}

void SetUpCompSets(EnergyPlusData &state,
                   std::string_view ParentType,       // Parent Object Type
                   std::string_view ParentName,       // Parent Object Name
                   std::string_view CompType,         // Component Type
                   std::string_view CompName,         // Component Name
                   std::string_view InNode,        // Inlet Node Name
                   std::string_view OutNode,       // Outlet Node Name
                   std::string_view const Description // Description
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   November 2001

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine sets up "Component Sets" as input in the branch
    // lists.  These can be used later to verify that the proper names and
    // inlet/outlet nodes have been input.  This routine assumes that identical
    // "CompSets" cannot be used in multiple places and issues a warning if they are.

    std::string ParentTypeUC = Util::makeUPPER(ParentType);
    std::string CompTypeUC = Util::makeUPPER(CompType);
    // TODO: Refactor this away by passing in enums
    Node::ConnObjType ParentTypeEnum =
            static_cast<Node::ConnObjType>(getEnumValue(Node::connObjTypeNamesUC, ParentTypeUC));
    assert(ParentTypeEnum != Node::ConnObjType::Invalid);

    Node::ConnObjType ComponentTypeEnum =
            static_cast<Node::ConnObjType>(getEnumValue(Node::connObjTypeNamesUC, CompTypeUC));
    assert(ComponentTypeEnum != Node::ConnObjType::Invalid);

    int Found = 0;

    // See if Component-Nodes set is already there - should be unique
    // Try to fill in blanks (passed in as undefined
    for (int Count = 1; Count <= state.dataBranchNodeConnections->NumCompSets; ++Count) {
        if (CompName != state.dataBranchNodeConnections->CompSets(Count).CName) continue;
        if (ComponentTypeEnum != Node::ConnObjType::Undefined) {
            if (ComponentTypeEnum != state.dataBranchNodeConnections->CompSets(Count).ComponentObjectType) continue;
        }
        // Component name matches, component type matches or is undefined
        if (InNode != undefined) {
            if (state.dataBranchNodeConnections->CompSets(Count).InNodeName != undefined) {
                if (InNode != state.dataBranchNodeConnections->CompSets(Count).InNodeName) continue;
            } else {
                state.dataBranchNodeConnections->CompSets(Count).InNodeName = InNode;
            }
        }
        if (OutNode != undefined) {
            if (state.dataBranchNodeConnections->CompSets(Count).OutNodeName != undefined) {
                if (OutNode != state.dataBranchNodeConnections->CompSets(Count).OutNodeName) continue;
            } else {
                state.dataBranchNodeConnections->CompSets(Count).OutNodeName = OutNode;
            }
        }
        //  See if something undefined and set here
        if (state.dataBranchNodeConnections->CompSets(Count).ParentObjectType == Node::ConnObjType::Undefined &&
            state.dataBranchNodeConnections->CompSets(Count).ParentCName == undefined) {
            // Assume this is a further definition for this compset
            state.dataBranchNodeConnections->CompSets(Count).ParentObjectType = ParentTypeEnum;
            state.dataBranchNodeConnections->CompSets(Count).ParentCName = ParentName;
            if (!Description.empty()) {
                state.dataBranchNodeConnections->CompSets(Count).Description = Description;
            }
            Found = Count;
            break;
        }
    }
    if (Found == 0) {
        for (int Count = 1; Count <= state.dataBranchNodeConnections->NumCompSets; ++Count) {
            Found = 0;
            // Test if inlet node has been used before as an inlet node
            // If the matching node name does not belong to the parent object, then error
            // For example a fan may share the same inlet node as the furnace object which is its parent
            if (InNode != state.dataBranchNodeConnections->CompSets(Count).InNodeName) {
                continue;
                // If parent type is undefined then no error
            } else if ((ParentTypeEnum == Node::ConnObjType::Undefined) ||
                       (state.dataBranchNodeConnections->CompSets(Count).ParentObjectType == Node::ConnObjType::Undefined)) {
                // If node name is undefined then no error
            } else if (InNode != undefined) {
                // If the matching node name does not belong to the parent or child object, then error
                // For example a fan may share the same inlet node as the furnace object which is its parent
                if ((ParentTypeEnum == state.dataBranchNodeConnections->CompSets(Count).ComponentObjectType) &&
                    (ParentName == state.dataBranchNodeConnections->CompSets(Count).CName)) {
                    // OK - The duplicate inlet node belongs to this component's parent
                } else if ((ComponentTypeEnum == state.dataBranchNodeConnections->CompSets(Count).ParentObjectType) &&
                           (CompName == state.dataBranchNodeConnections->CompSets(Count).ParentCName)) {
                    // OK - The duplicate inlet node belongs to a child of this component
                } else {
                    // Due to possibility of grandparents or more, if the matching node name
                    // belongs to a component that appears as a parent, then OK
                    int Found2 = 0;
                    for (int Count2 = 1; Count2 <= state.dataBranchNodeConnections->NumCompSets; ++Count2) {
                        if ((state.dataBranchNodeConnections->CompSets(Count).ComponentObjectType ==
                             state.dataBranchNodeConnections->CompSets(Count2).ParentObjectType) &&
                            (state.dataBranchNodeConnections->CompSets(Count).CName == state.dataBranchNodeConnections->CompSets(Count2).ParentCName))
                            Found2 = 1;
                        if ((ComponentTypeEnum == state.dataBranchNodeConnections->CompSets(Count2).ParentObjectType) &&
                            (CompName == state.dataBranchNodeConnections->CompSets(Count2).ParentCName))
                            Found2 = 1;
                    }
                    if (Found2 == 0) {
                        ShowWarningError(state, format("Node used as an inlet more than once: {}", InNode));
                        ShowContinueError(
                            state,
                            format("  Used by: {}, name={}",
                                   Node::connObjTypeNames[(int)state.dataBranchNodeConnections->CompSets(Count).ParentObjectType],
                                   state.dataBranchNodeConnections->CompSets(Count).ParentCName));
                        ShowContinueError(
                            state,
                            format("  as inlet for: {}, name={}",
                                   Node::connObjTypeNames[(int)state.dataBranchNodeConnections->CompSets(Count).ComponentObjectType],
                                   state.dataBranchNodeConnections->CompSets(Count).CName));
                        ShowContinueError(state, format("{}{}{}", "  and  by     : ", ParentTypeUC + ", name=", ParentName));
                        ShowContinueError(state, format("{}{}{}", "  as inlet for: ", CompTypeUC + ", name=", CompName));
                    }
                }
            }
            // Test if outlet node has been used before as an outlet node
            // If the matching node name does not belong to the parent or child object, then error
            // For example a fan may share the same outlet node as the furnace object which is its parent
            if (OutNode != state.dataBranchNodeConnections->CompSets(Count).OutNodeName) {
                continue;
                // If parent type is undefined then no error
            } else if ((ParentTypeEnum == Node::ConnObjType::Undefined) ||
                       (state.dataBranchNodeConnections->CompSets(Count).ParentObjectType == Node::ConnObjType::Undefined)) {
                // If node name is undefined then no error
            } else if (OutNode != undefined) {
                if ((ParentTypeEnum == state.dataBranchNodeConnections->CompSets(Count).ComponentObjectType) &&
                    (ParentName == state.dataBranchNodeConnections->CompSets(Count).CName)) {
                    // OK - The duplicate outlet node belongs to this component's parent
                } else if ((ComponentTypeEnum == state.dataBranchNodeConnections->CompSets(Count).ParentObjectType) &&
                           (CompName == state.dataBranchNodeConnections->CompSets(Count).ParentCName)) {
                    // OK - The duplicate outlet node belongs to a child of this component
                } else {
                    // Due to possibility of grandparents or more, if the matching node name
                    // belongs to a component that appears as a parent, then OK
                    int Found2 = 0;
                    for (int Count2 = 1; Count2 <= state.dataBranchNodeConnections->NumCompSets; ++Count2) {
                        if ((state.dataBranchNodeConnections->CompSets(Count).ComponentObjectType ==
                             state.dataBranchNodeConnections->CompSets(Count2).ParentObjectType) &&
                            (state.dataBranchNodeConnections->CompSets(Count).CName == state.dataBranchNodeConnections->CompSets(Count2).ParentCName))
                            Found2 = 1;
                        if ((ComponentTypeEnum == state.dataBranchNodeConnections->CompSets(Count2).ParentObjectType) &&
                            (CompName == state.dataBranchNodeConnections->CompSets(Count2).ParentCName))
                            Found2 = 1;
                    }
                    // This rule is violated by dual duct units, so let it pass
                    if (Found2 == 0) {
                        std::string_view const CType =
                            Node::connObjTypeNames[(int)state.dataBranchNodeConnections->CompSets(Count).ComponentObjectType];
                        if ((!has_prefixi(CType, "AirTerminal:DualDuct:")) && (!has_prefixi(CompTypeUC, "AirTerminal:DualDuct:"))) {
                            ShowWarningError(state, format("Node used as an outlet more than once: {}", OutNode));
                            ShowContinueError(
                                state,
                                format("  Used by: {}, name={}",
                                       Node::connObjTypeNames[(int)state.dataBranchNodeConnections->CompSets(Count).ParentObjectType],
                                       state.dataBranchNodeConnections->CompSets(Count).ParentCName));
                            ShowContinueError(
                                state,
                                format(
                                    "  as outlet for: {}, name={}",
                                    Node::connObjTypeNames[(int)state.dataBranchNodeConnections->CompSets(Count).ComponentObjectType],
                                    state.dataBranchNodeConnections->CompSets(Count).CName));
                            ShowContinueError(state, format("{}{}{}", "  and  by     : ", ParentTypeUC + ", name=", ParentName));
                            ShowContinueError(state, format("{}{}{}", "  as outlet for: ", CompTypeUC + ", name=", CompName));
                        }
                    }
                }
            }
            if (ComponentTypeEnum != state.dataBranchNodeConnections->CompSets(Count).ComponentObjectType &&
                ComponentTypeEnum != Node::ConnObjType::Undefined)
                continue;
            if (CompName != state.dataBranchNodeConnections->CompSets(Count).CName) continue;
            Found = Count;
            break;
        }
    }
    if (Found == 0) {
        state.dataBranchNodeConnections->CompSets.resize(++state.dataBranchNodeConnections->NumCompSets);
        state.dataBranchNodeConnections->CompSets(state.dataBranchNodeConnections->NumCompSets).ParentObjectType = ParentTypeEnum;

        state.dataBranchNodeConnections->CompSets(state.dataBranchNodeConnections->NumCompSets).ParentCName = ParentName;
        state.dataBranchNodeConnections->CompSets(state.dataBranchNodeConnections->NumCompSets).ComponentObjectType = ComponentTypeEnum;
        state.dataBranchNodeConnections->CompSets(state.dataBranchNodeConnections->NumCompSets).CName = CompName;
        state.dataBranchNodeConnections->CompSets(state.dataBranchNodeConnections->NumCompSets).InNodeName =

            Util::makeUPPER(InNode); // TODO: Fix this....
        state.dataBranchNodeConnections->CompSets(state.dataBranchNodeConnections->NumCompSets).OutNodeName =
            Util::makeUPPER(OutNode); // TODO: Fix this....

        if (!Description.empty()) {
            state.dataBranchNodeConnections->CompSets(state.dataBranchNodeConnections->NumCompSets).Description = Description;
        } else {
            state.dataBranchNodeConnections->CompSets(state.dataBranchNodeConnections->NumCompSets).Description = undefined;
        }
    }
}

void TestInletOutletNodes(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   November 2001

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine tests the branches to see if a duplicate inlet node
    // exists under a different name in the sequence; likewise for outlet.

    Array1D_bool AlreadyNoted;

    // Test component sets created by branches
    AlreadyNoted.dimension(state.dataBranchNodeConnections->NumCompSets, false);
    for (int Count = 1; Count <= state.dataBranchNodeConnections->NumCompSets; ++Count) {
        for (int Other = 1; Other <= state.dataBranchNodeConnections->NumCompSets; ++Other) {
            if (Count == Other) continue;
            if (state.dataBranchNodeConnections->CompSets(Count).InNodeName != state.dataBranchNodeConnections->CompSets(Other).InNodeName)
                continue;
            if (AlreadyNoted(Count)) continue;
            //  All other values must match
            if (state.dataBranchNodeConnections->CompSets(Count).ComponentObjectType !=
                    state.dataBranchNodeConnections->CompSets(Other).ComponentObjectType ||
                state.dataBranchNodeConnections->CompSets(Count).CName != state.dataBranchNodeConnections->CompSets(Other).CName ||
                state.dataBranchNodeConnections->CompSets(Count).OutNodeName != state.dataBranchNodeConnections->CompSets(Other).OutNodeName) {
                AlreadyNoted(Other) = true;
                ShowWarningError(state,
                                 format("Node used as an inlet more than once: {}", state.dataBranchNodeConnections->CompSets(Count).InNodeName));
                ShowContinueError(
                    state,
                    format("  Used by: {}, name={}",
                           Node::connObjTypeNames[(int)state.dataBranchNodeConnections->CompSets(Count).ParentObjectType],
                           state.dataBranchNodeConnections->CompSets(Count).ParentCName));
                ShowContinueError(
                    state,
                    format("  as inlet for: {}, name={}",
                           Node::connObjTypeNames[(int)state.dataBranchNodeConnections->CompSets(Other).ComponentObjectType],
                           state.dataBranchNodeConnections->CompSets(Other).CName));
                ShowContinueError(
                    state,
                    format("  and by: {}, name={}",
                           Node::connObjTypeNames[(int)state.dataBranchNodeConnections->CompSets(Other).ParentObjectType],
                           state.dataBranchNodeConnections->CompSets(Other).ParentCName));
                ShowContinueError(
                    state,
                    format("  as inlet for: {}, name={}",
                           Node::connObjTypeNames[(int)state.dataBranchNodeConnections->CompSets(Count).ComponentObjectType],
                           state.dataBranchNodeConnections->CompSets(Count).CName));
            }
        }
    }

    AlreadyNoted = false;
    for (int Count = 1; Count <= state.dataBranchNodeConnections->NumCompSets; ++Count) {
        for (int Other = 1; Other <= state.dataBranchNodeConnections->NumCompSets; ++Other) {
            if (Count == Other) continue;
            if (state.dataBranchNodeConnections->CompSets(Count).OutNodeName != state.dataBranchNodeConnections->CompSets(Other).OutNodeName)
                continue;
            if (AlreadyNoted(Count)) continue;
            //  All other values must match
            if (state.dataBranchNodeConnections->CompSets(Count).ComponentObjectType !=
                    state.dataBranchNodeConnections->CompSets(Other).ComponentObjectType ||
                state.dataBranchNodeConnections->CompSets(Count).CName != state.dataBranchNodeConnections->CompSets(Other).CName ||
                state.dataBranchNodeConnections->CompSets(Count).InNodeName != state.dataBranchNodeConnections->CompSets(Other).InNodeName) {
                AlreadyNoted(Other) = true;
                ShowWarningError(
                    state, format("Node used as an outlet more than once: {}", state.dataBranchNodeConnections->CompSets(Count).OutNodeName));
                ShowContinueError(
                    state,
                    format("  Used by: {}, name={}",
                           Node::connObjTypeNames[(int)state.dataBranchNodeConnections->CompSets(Count).ParentObjectType],
                           state.dataBranchNodeConnections->CompSets(Count).ParentCName));
                ShowContinueError(
                    state,
                    format("  as outlet for: {}, name={}",
                           Node::connObjTypeNames[(int)state.dataBranchNodeConnections->CompSets(Other).ComponentObjectType],
                           state.dataBranchNodeConnections->CompSets(Other).CName));
                ShowContinueError(
                    state,
                    format("  and by: {}, name={}",
                           Node::connObjTypeNames[(int)state.dataBranchNodeConnections->CompSets(Other).ParentObjectType],
                           state.dataBranchNodeConnections->CompSets(Other).ParentCName));
                ShowContinueError(
                    state,
                    format("  as outlet for: {}, name={}",
                           Node::connObjTypeNames[(int)state.dataBranchNodeConnections->CompSets(Count).ComponentObjectType],
                           state.dataBranchNodeConnections->CompSets(Count).CName));
            }
        }
    }

    AlreadyNoted.deallocate();
}

void TestCompSet(EnergyPlusData &state,
                 std::string_view const CompType, // Component Type
                 std::string_view CompName,       // Component Name
                 std::string const &InNode,    // Inlet Node Name
                 std::string const &OutNode,   // Outlet Node Name
                 std::string const &Description   // Description of Node Pair (for warning message)
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   November 2001

    // PURPOSE OF THIS SUBROUTINE:
    // Register a child component in the CompSets data structure.
    // NOTE:  This function was originally designed to test the stored "Component Sets" to
    // see if there was one of this combination in there.  Thus the name "TestCompSet".
    // However, this was based on a false assumption that input would always be gotten
    // first for the parent object, then for the child object.  But this is often not the
    // case.  Ultimately, the name of this function should be changed or it should be merged
    // into SetUpCompSets.
    // Until then, this function does the following:
    //   a)  Search CompSets for this combination of component type, component name,
    //       inlet node and outlet node.  If component type/name match and the existing
    //       node names are UNDEFINED, this compset is assumed to be a match.
    //   b)  If found, fill in any missing data such as node names or node description
    //   c)  If not found, call SetUpCompSets (with parent type and name UNDEFINED)
    //       to add a new item in the CompSets array

    std::string CompTypeUC = Util::makeUPPER(CompType);

    // TODO: Refactor this away by passing in enums
    Node::ConnObjType ComponentTypeEnum =
            static_cast<Node::ConnObjType>(getEnumValue(Node::connObjTypeNamesUC, CompTypeUC));
    assert(ComponentTypeEnum != Node::ConnObjType::Invalid);

    // See if Already there
    int Found = 0;
    for (int Count = 1; Count <= state.dataBranchNodeConnections->NumCompSets; ++Count) {
        if ((ComponentTypeEnum != state.dataBranchNodeConnections->CompSets(Count).ComponentObjectType) &&
            (state.dataBranchNodeConnections->CompSets(Count).ComponentObjectType != Node::ConnObjType::Undefined))
            continue;
        if (CompName != state.dataBranchNodeConnections->CompSets(Count).CName) continue;
        if ((InNode != state.dataBranchNodeConnections->CompSets(Count).InNodeName) &&
            (state.dataBranchNodeConnections->CompSets(Count).InNodeName != undefined) && (InNode != undefined))
            continue;
        if ((OutNode != state.dataBranchNodeConnections->CompSets(Count).OutNodeName) &&
            (state.dataBranchNodeConnections->CompSets(Count).OutNodeName != undefined) && (OutNode != undefined))
            continue;

        Found = Count;
        break;
    }

    if (Found == 0) {
        SetUpCompSets(state, undefined, undefined, CompType, CompName, InNode, OutNode, Description);
    } else {
        // Fill in node names and component type for previously undefined values:
        //   If the parent object did not specify a component type or inlet or outlet node, then that value
        //   is UNDEFINED in CompSets.  When a component calls TestCompSet, the comp type and inlet and
        //   outlet nodes are known, so they can be filled in for future reference.
        if (state.dataBranchNodeConnections->CompSets(Found).ComponentObjectType == Node::ConnObjType::Undefined) {
            state.dataBranchNodeConnections->CompSets(Found).ComponentObjectType = ComponentTypeEnum;
        }
        if (state.dataBranchNodeConnections->CompSets(Found).InNodeName == undefined)
            state.dataBranchNodeConnections->CompSets(Found).InNodeName = InNode;
        if (state.dataBranchNodeConnections->CompSets(Found).OutNodeName == undefined)
            state.dataBranchNodeConnections->CompSets(Found).OutNodeName = OutNode;
        if (state.dataBranchNodeConnections->CompSets(Found).Description == undefined)
            state.dataBranchNodeConnections->CompSets(Found).Description = Description;
    }
}

void TestCompSetInletOutletNodes(EnergyPlusData &state, bool &ErrorsFound)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   March 2008
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine tests the comp sets to see if a duplicate comp name
    // exists under a different set of inlet/outlet nodes.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Array1D_bool AlreadyNoted;

    // Test component sets created by branches
    AlreadyNoted.dimension(state.dataBranchNodeConnections->NumCompSets, false);
    for (int Count = 1; Count <= state.dataBranchNodeConnections->NumCompSets; ++Count) {
        for (int Other = 1; Other <= state.dataBranchNodeConnections->NumCompSets; ++Other) {
            if (Count == Other) continue;
            if (state.dataBranchNodeConnections->CompSets(Count).ComponentObjectType ==
                Node::ConnObjType ::SolarCollectorUnglazedTranspired)
                continue;
            if (state.dataBranchNodeConnections->CompSets(Count).ComponentObjectType !=
                    state.dataBranchNodeConnections->CompSets(Other).ComponentObjectType ||
                state.dataBranchNodeConnections->CompSets(Count).CName != state.dataBranchNodeConnections->CompSets(Other).CName)
                continue;
            if (state.dataBranchNodeConnections->CompSets(Count).Description != state.dataBranchNodeConnections->CompSets(Other).Description) {
                if (state.dataBranchNodeConnections->CompSets(Count).Description != undefined &&
                    state.dataBranchNodeConnections->CompSets(Other).Description != undefined)
                    continue;
            }
            if (state.dataBranchNodeConnections->CompSets(Count).InNodeName == state.dataBranchNodeConnections->CompSets(Other).InNodeName)
                continue;
            if (state.dataBranchNodeConnections->CompSets(Count).OutNodeName == state.dataBranchNodeConnections->CompSets(Other).OutNodeName)
                continue;
            if (AlreadyNoted(Count)) continue;
            //  All other values must match
            AlreadyNoted(Other) = true;
            ShowSevereError(state, "Same component name and type has differing Node Names.");
            ShowContinueError(
                state,
                format("  Component: {}, name={}",
                       Node::connObjTypeNames[(int)state.dataBranchNodeConnections->CompSets(Count).ComponentObjectType],
                       state.dataBranchNodeConnections->CompSets(Count).CName));
            ShowContinueError(state,
                              format("   Nodes, inlet: {}, outlet: {}",
                                     state.dataBranchNodeConnections->CompSets(Count).InNodeName,
                                     state.dataBranchNodeConnections->CompSets(Count).OutNodeName));
            ShowContinueError(state,
                              format(" & Nodes, inlet: {}, outlet: {}",
                                     state.dataBranchNodeConnections->CompSets(Other).InNodeName,
                                     state.dataBranchNodeConnections->CompSets(Other).OutNodeName));
            ShowContinueError(state,
                              format("   Node Types:   {} & {}",
                                     state.dataBranchNodeConnections->CompSets(Count).Description,
                                     state.dataBranchNodeConnections->CompSets(Other).Description));
            ErrorsFound = true;
        }
    }

    AlreadyNoted.deallocate();
}

void GetNodeConnectionType(EnergyPlusData &state, int const NodeNumber, EPVector<Node::ConnType> &NodeConnectType, bool &errFlag)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Lixing Gu
    //       DATE WRITTEN   Jan 2007

    // PURPOSE OF THIS FUNCTION:
    // This function provides a connection type with given node number

    Array1D_int ListArray;
    Array1D_string ConnectionTypes(15);

    for (int nodetype = 1; nodetype < (int)Node::ConnType::Num; ++nodetype) {
        ConnectionTypes(nodetype) = Node::connTypeNames[nodetype];
    }

    if (allocated(NodeConnectType)) NodeConnectType.deallocate();

    int NumInList;
    FindAllNodeNumbersInList(
        NodeNumber, state.dataBranchNodeConnections->NodeConnections, state.dataBranchNodeConnections->NumNodeConnections, NumInList, ListArray);

    NodeConnectType.allocate(NumInList);

    if (NumInList > 0) {
        for (int NodeConnectIndex = 1; NodeConnectIndex <= NumInList; ++NodeConnectIndex) {
            NodeConnectType(NodeConnectIndex) = state.dataBranchNodeConnections->NodeConnections(ListArray(NodeConnectIndex)).connType;
        }
    } else {
        if (NodeNumber > 0) {
            ShowWarningError(state, format("Node not found = {}.", state.dataLoopNodes->nodes(NodeNumber)->Name));
        } else {
            ShowWarningError(state, "Invalid node number passed = 0.");
        }
        errFlag = true;
    }
}

void FindAllNodeNumbersInList(int const WhichNumber,
                              EPVector<DataBranchNodeConnections::NodeConnectionDef> const &NodeConnections,
                              int const NumItems,
                              int &CountOfItems,            // Number of items found
                              Array1D_int &AllNumbersInList // Index array to all numbers found
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         R. Raustad
    //       DATE WRITTEN   January 2007

    // PURPOSE OF THIS FUNCTION:
    // This function looks up a number(integer) in a similar list of
    // items and returns the index of the item in the list, if
    // found.

    CountOfItems = 0;

    if (allocated(AllNumbersInList)) AllNumbersInList.deallocate();

    for (int Count = 1; Count <= NumItems; ++Count) {
        if (WhichNumber == NodeConnections(Count).NodeNum) {
            ++CountOfItems;
        }
    }

    if (CountOfItems > 0) {

        AllNumbersInList.dimension(CountOfItems, 0);
        CountOfItems = 0;

        for (int Count = 1; Count <= NumItems; ++Count) {
            if (WhichNumber == NodeConnections(Count).NodeNum) {
                ++CountOfItems;
                AllNumbersInList(CountOfItems) = Count;
            }
        }
    }
}

} // namespace EnergyPlus::BranchNodeConnections
