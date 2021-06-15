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

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/member.functions.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataBranchNodeConnections.hh>
#include <EnergyPlus/DataLoopNode.hh>
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
using namespace DataLoopNode;
using namespace DataBranchNodeConnections;

void RegisterNodeConnection(EnergyPlusData &state,
                            int const NodeNumber,                // Number for this Node
                            std::string_view NodeName,         // Name of this Node
                            std::string_view ObjectType,       // Type of object this Node is connected to (e.g. Chiller:Electric)
                            std::string_view ObjectName,       // Name of object this Node is connected to (e.g. MyChiller)
                            std::string_view ConnectionType,   // Connection Type for this Node (must be valid)
                            int const FluidStream,               // Count on Fluid Streams
                            bool const IsParent,                 // True when node is a parent node
                            bool &errFlag,                       // Will be True if errors already detected or if errors found here
                            Optional_string_const InputFieldName // Input Field Name
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

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    bool ErrorsFoundHere;
    int Count;
    bool MakeNew;
    int Found;

    ErrorsFoundHere = false;
    if (!IsValidConnectionType(ConnectionType)) {
        ShowSevereError(state, format("{}{}{}", RoutineName, "Invalid ConnectionType=", ConnectionType));
        ShowContinueError(state, "Occurs for Node=" + std::string{NodeName} + ", ObjectType=" + std::string{ObjectType} + ", ObjectName=" + std::string{ObjectName});
        ErrorsFoundHere = true;
    }

    MakeNew = true;
    for (Count = 1; Count <= state.dataBranchNodeConnections->NumOfNodeConnections; ++Count) {
        if (state.dataBranchNodeConnections->NodeConnections(Count).NodeNumber != NodeNumber) continue;
        if (!UtilityRoutines::SameString(state.dataBranchNodeConnections->NodeConnections(Count).ObjectType, ObjectType)) continue;
        if (!UtilityRoutines::SameString(state.dataBranchNodeConnections->NodeConnections(Count).ObjectName, ObjectName)) continue;
        if (!UtilityRoutines::SameString(state.dataBranchNodeConnections->NodeConnections(Count).ConnectionType, ConnectionType)) continue;
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
        ++state.dataBranchNodeConnections->NumOfNodeConnections;
        if (state.dataBranchNodeConnections->NumOfNodeConnections > 1 &&
            state.dataBranchNodeConnections->NumOfNodeConnections > state.dataBranchNodeConnections->MaxNumOfNodeConnections) {
            state.dataBranchNodeConnections->NodeConnections.redimension(state.dataBranchNodeConnections->MaxNumOfNodeConnections +=
                                                                         state.dataBranchNodeConnections->NodeConnectionAlloc);
        } else if (state.dataBranchNodeConnections->NumOfNodeConnections == 1) {
            state.dataBranchNodeConnections->NodeConnections.allocate(state.dataBranchNodeConnections->NodeConnectionAlloc);
            state.dataBranchNodeConnections->MaxNumOfNodeConnections = state.dataBranchNodeConnections->NodeConnectionAlloc;
        }

        state.dataBranchNodeConnections->NodeConnections(state.dataBranchNodeConnections->NumOfNodeConnections).NodeNumber = NodeNumber;
        state.dataBranchNodeConnections->NodeConnections(state.dataBranchNodeConnections->NumOfNodeConnections).NodeName = NodeName;
        state.dataBranchNodeConnections->NodeConnections(state.dataBranchNodeConnections->NumOfNodeConnections).ObjectType =
            UtilityRoutines::MakeUPPERCase(ObjectType);
        state.dataBranchNodeConnections->NodeConnections(state.dataBranchNodeConnections->NumOfNodeConnections).ObjectName = ObjectName;
        state.dataBranchNodeConnections->NodeConnections(state.dataBranchNodeConnections->NumOfNodeConnections).ConnectionType = ConnectionType;
        state.dataBranchNodeConnections->NodeConnections(state.dataBranchNodeConnections->NumOfNodeConnections).FluidStream = FluidStream;
        state.dataBranchNodeConnections->NodeConnections(state.dataBranchNodeConnections->NumOfNodeConnections).ObjectIsParent = IsParent;
    }

    if (has_prefixi(ObjectType, "AirTerminal:")) {
        if (present(InputFieldName)) {
            ++state.dataBranchNodeConnections->NumOfAirTerminalNodes;
            if (state.dataBranchNodeConnections->NumOfAirTerminalNodes > 1 &&
                state.dataBranchNodeConnections->NumOfAirTerminalNodes > state.dataBranchNodeConnections->MaxNumOfAirTerminalNodes) {
                state.dataBranchNodeConnections->AirTerminalNodeConnections.redimension(state.dataBranchNodeConnections->MaxNumOfAirTerminalNodes +=
                                                                                        state.dataBranchNodeConnections->EqNodeConnectionAlloc);
            } else if (state.dataBranchNodeConnections->NumOfAirTerminalNodes == 1) {
                state.dataBranchNodeConnections->AirTerminalNodeConnections.allocate(state.dataBranchNodeConnections->EqNodeConnectionAlloc);
                state.dataBranchNodeConnections->MaxNumOfAirTerminalNodes = state.dataBranchNodeConnections->EqNodeConnectionAlloc;
            }

            // Check out AirTerminal inlet/outlet nodes
            Found = UtilityRoutines::FindItemInList(NodeName,
                                                    state.dataBranchNodeConnections->AirTerminalNodeConnections,
                                                    &EqNodeConnectionDef::NodeName,
                                                    state.dataBranchNodeConnections->NumOfAirTerminalNodes - 1);
            if (Found != 0) { // Nodename already used
                ShowSevereError(state, fmt::format("{}{}=\"{}\" node name duplicated", RoutineName, ObjectType, ObjectName));
                ShowContinueError(state, "NodeName=\"" + std::string{NodeName} + "\", entered as type=" + std::string{ConnectionType});
                ShowContinueError(state, "In Field=" + InputFieldName());
                ShowContinueError(state,
                                  "Already used in " + state.dataBranchNodeConnections->AirTerminalNodeConnections(Found).ObjectType + "=\"" +
                                      state.dataBranchNodeConnections->AirTerminalNodeConnections(Found).ObjectName + "\".");
                ShowContinueError(state,
                                  " as type=" + state.dataBranchNodeConnections->AirTerminalNodeConnections(Found).ConnectionType +
                                      ", In Field=" + state.dataBranchNodeConnections->AirTerminalNodeConnections(Found).InputFieldName);
                ErrorsFoundHere = true;
            } else {
                state.dataBranchNodeConnections->AirTerminalNodeConnections(state.dataBranchNodeConnections->NumOfAirTerminalNodes).NodeName =
                    NodeName;
                state.dataBranchNodeConnections->AirTerminalNodeConnections(state.dataBranchNodeConnections->NumOfAirTerminalNodes).ObjectType =
                    ObjectType;
                state.dataBranchNodeConnections->AirTerminalNodeConnections(state.dataBranchNodeConnections->NumOfAirTerminalNodes).ObjectName =
                    ObjectName;
                state.dataBranchNodeConnections->AirTerminalNodeConnections(state.dataBranchNodeConnections->NumOfAirTerminalNodes).ConnectionType =
                    ConnectionType;
                state.dataBranchNodeConnections->AirTerminalNodeConnections(state.dataBranchNodeConnections->NumOfAirTerminalNodes).InputFieldName =
                    InputFieldName;
            }
        } else {
            ShowSevereError(state, fmt::format("{}{} , Developer Error: Input Field Name not included.", RoutineName, ObjectType));
            ShowContinueError(state, "Node names not checked for duplication.");
        }
    }

    if (ErrorsFoundHere) {
        errFlag = true;
    }
}

void OverrideNodeConnectionType(EnergyPlusData &state,
                                int const NodeNumber,              // Number for this Node
                                std::string const &NodeName,       // Name of this Node
                                std::string const &ObjectType,     // Type of object this Node is connected to (e.g. Chiller:Electric)
                                std::string const &ObjectName,     // Name of object this Node is connected to (e.g. MyChiller)
                                std::string const &ConnectionType, // Connection Type for this Node (must be valid)
                                int const FluidStream,             // Count on Fluid Streams
                                bool const IsParent,               // True when node is a parent node
                                bool &errFlag                      // Will be True if errors already detected or if errors found here
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

    if (!IsValidConnectionType(ConnectionType)) {
        ShowSevereError(state, format("{}{}{}", RoutineName, "Invalid ConnectionType=", ConnectionType));
        ShowContinueError(state, "Occurs for Node=" + NodeName + ", ObjectType=" + ObjectType + ", ObjectName=" + ObjectName);
        errFlag = true;
    }

    int Found = 0;
    for (int Count = 1; Count <= state.dataBranchNodeConnections->NumOfNodeConnections; ++Count) {
        if (state.dataBranchNodeConnections->NodeConnections(Count).NodeNumber != NodeNumber) continue;
        if (!UtilityRoutines::SameString(state.dataBranchNodeConnections->NodeConnections(Count).ObjectType, ObjectType)) continue;
        if (!UtilityRoutines::SameString(state.dataBranchNodeConnections->NodeConnections(Count).ObjectName, ObjectName)) continue;
        if (state.dataBranchNodeConnections->NodeConnections(Count).FluidStream != FluidStream) continue;
        if ((state.dataBranchNodeConnections->NodeConnections(Count).ObjectIsParent != IsParent)) continue;
        Found = Count;
        break;
    }

    if (Found > 0) {
        state.dataBranchNodeConnections->NodeConnections(Found).ConnectionType = ConnectionType;
    } else {
        ShowSevereError(state, format("{}{}", RoutineName, "Existing node connection not found."));
        ShowContinueError(state, "Occurs for Node=" + NodeName + ", ObjectType=" + ObjectType + ", ObjectName=" + ObjectName);
        errFlag = true;
    }
}

bool IsValidConnectionType(std::string_view ConnectionType)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   August 2003
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function determines if a connection type is valid.

    // Return value
    bool IsValid;

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int Count;

    IsValid = false;
    for (Count = 1; Count <= NumValidConnectionTypes; ++Count) {
        if (ConnectionType != DataLoopNode::ValidConnectionTypes(static_cast<DataLoopNode::NodeConnectionType>(Count))) continue;
        IsValid = true;
        break;
    }

    return IsValid;
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
    // 9.  nodes of type OutsideAirReference must be registered as DataLoopNode::NodeConnectionType::OutsideAir
    // 10. fluid streams cannot have multiple inlet/outlet nodes on same component
    // 11. zone nodes may not be used as anything else except as a setpoint, sensor or actuator node

    // METHODOLOGY EMPLOYED:
    // Needs description, as appropriate.

    // Using/Aliasing

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int Loop1;
    int Loop2;
    bool IsValid;
    bool IsInlet;
    bool IsOutlet;
    bool MatchedAtLeastOne;
    int ErrorCounter;
    int Object;
    int StartConnect;
    int EndConnect;
    Array1D_int FluidStreamInletCount;
    Array1D_int FluidStreamOutletCount;
    Array1D_int NodeObjects;
    Array1D_bool FluidStreamCounts;
    int NumObjects;
    int MaxFluidStream;

    ErrorCounter = 0;

    auto &ValidConnectionTypes(DataLoopNode::ValidConnectionTypes);

    //  Check 1 -- check sensor and actuator nodes
    for (Loop1 = 1; Loop1 <= state.dataBranchNodeConnections->NumOfNodeConnections; ++Loop1) {
        if (state.dataBranchNodeConnections->NodeConnections(Loop1).ConnectionType != ValidConnectionTypes(DataLoopNode::NodeConnectionType::Sensor))
            continue;
        IsValid = false;
        for (Loop2 = 1; Loop2 <= state.dataBranchNodeConnections->NumOfNodeConnections; ++Loop2) {
            if (Loop1 == Loop2) continue;
            if (state.dataBranchNodeConnections->NodeConnections(Loop1).NodeNumber !=
                state.dataBranchNodeConnections->NodeConnections(Loop2).NodeNumber)
                continue;
            if (state.dataBranchNodeConnections->NodeConnections(Loop2).ConnectionType ==
                ValidConnectionTypes(DataLoopNode::NodeConnectionType::Actuator))
                continue;
            if (state.dataBranchNodeConnections->NodeConnections(Loop2).ConnectionType ==
                ValidConnectionTypes(DataLoopNode::NodeConnectionType::Sensor))
                continue;
            IsValid = true;
        }
        if (!IsValid) {
            ShowSevereError(state,
                            "Node Connection Error, Node=\"" + state.dataBranchNodeConnections->NodeConnections(Loop1).NodeName +
                                "\", Sensor node did not find a matching node of appropriate type (other than Actuator or Sensor).");
            ShowContinueError(state,
                              "Reference Object=" + state.dataBranchNodeConnections->NodeConnections(Loop1).ObjectType +
                                  ", Name=" + state.dataBranchNodeConnections->NodeConnections(Loop1).ObjectName);
            ++ErrorCounter;
            ErrorsFound = true;
        }
    }

    for (Loop1 = 1; Loop1 <= state.dataBranchNodeConnections->NumOfNodeConnections; ++Loop1) {
        if (state.dataBranchNodeConnections->NodeConnections(Loop1).ConnectionType !=
            ValidConnectionTypes(DataLoopNode::NodeConnectionType::Actuator))
            continue;
        IsValid = false;
        for (Loop2 = 1; Loop2 <= state.dataBranchNodeConnections->NumOfNodeConnections; ++Loop2) {
            if (Loop1 == Loop2) continue;
            if (state.dataBranchNodeConnections->NodeConnections(Loop1).NodeNumber !=
                state.dataBranchNodeConnections->NodeConnections(Loop2).NodeNumber)
                continue;
            if (state.dataBranchNodeConnections->NodeConnections(Loop2).ConnectionType ==
                ValidConnectionTypes(DataLoopNode::NodeConnectionType::Actuator))
                continue;
            if (state.dataBranchNodeConnections->NodeConnections(Loop2).ConnectionType ==
                ValidConnectionTypes(DataLoopNode::NodeConnectionType::Sensor))
                continue;
            if (state.dataBranchNodeConnections->NodeConnections(Loop2).ConnectionType ==
                ValidConnectionTypes(DataLoopNode::NodeConnectionType::OutsideAir))
                continue;
            IsValid = true;
        }
        if (!IsValid) {
            ShowSevereError(state,
                            "Node Connection Error, Node=\"" + state.dataBranchNodeConnections->NodeConnections(Loop1).NodeName +
                                "\", Actuator node did not find a matching node of appropriate type (other than Actuator, Sensor, OutsideAir).");
            ShowContinueError(state,
                              "Reference Object=" + state.dataBranchNodeConnections->NodeConnections(Loop1).ObjectType +
                                  ", Name=" + state.dataBranchNodeConnections->NodeConnections(Loop1).ObjectName);
            ++ErrorCounter;
            ErrorsFound = true;
        }
    }

    // Check 2 -- setpoint nodes
    // Check 2a -- setpoint node must also be an inlet or an outlet (CR8212)
    for (Loop1 = 1; Loop1 <= state.dataBranchNodeConnections->NumOfNodeConnections; ++Loop1) {
        if (state.dataBranchNodeConnections->NodeConnections(Loop1).ConnectionType !=
            ValidConnectionTypes(DataLoopNode::NodeConnectionType::SetPoint))
            continue;
        IsValid = false;
        IsInlet = false;
        IsOutlet = false;
        for (Loop2 = 1; Loop2 <= state.dataBranchNodeConnections->NumOfNodeConnections; ++Loop2) {
            if (Loop1 == Loop2) continue;
            if (state.dataBranchNodeConnections->NodeConnections(Loop1).NodeNumber !=
                state.dataBranchNodeConnections->NodeConnections(Loop2).NodeNumber)
                continue;
            if (state.dataBranchNodeConnections->NodeConnections(Loop2).ConnectionType ==
                ValidConnectionTypes(DataLoopNode::NodeConnectionType::SetPoint))
                continue;
            if (state.dataBranchNodeConnections->NodeConnections(Loop2).ConnectionType ==
                ValidConnectionTypes(DataLoopNode::NodeConnectionType::OutsideAir))
                continue;
            if (state.dataBranchNodeConnections->NodeConnections(Loop2).ConnectionType ==
                ValidConnectionTypes(DataLoopNode::NodeConnectionType::Inlet))
                IsInlet = true;
            if (state.dataBranchNodeConnections->NodeConnections(Loop2).ConnectionType ==
                ValidConnectionTypes(DataLoopNode::NodeConnectionType::Outlet))
                IsOutlet = true;
            IsValid = true;
        }
        if (!IsValid) {
            ShowSevereError(state,
                            "Node Connection Error, Node=\"" + state.dataBranchNodeConnections->NodeConnections(Loop1).NodeName +
                                "\", Setpoint node did not find a matching node of appropriate type (other than Setpoint, OutsideAir).");
            ShowContinueError(state,
                              "Reference Object=" + state.dataBranchNodeConnections->NodeConnections(Loop1).ObjectType +
                                  ", Name=" + state.dataBranchNodeConnections->NodeConnections(Loop1).ObjectName);
            ++ErrorCounter;
            ErrorsFound = true;
        }
        if (!IsInlet && !IsOutlet) {
            ShowSevereError(state,
                            "Node Connection Error, Node=\"" + state.dataBranchNodeConnections->NodeConnections(Loop1).NodeName +
                                "\", Setpoint node did not find a matching node of type Inlet or Outlet.");
            ShowContinueError(state, "It appears this node is not part of the HVAC system.");
            ShowContinueError(state,
                              "Reference Object=" + state.dataBranchNodeConnections->NodeConnections(Loop1).ObjectType +
                                  ", Name=" + state.dataBranchNodeConnections->NodeConnections(Loop1).ObjectName);
            ++ErrorCounter;
            //      ErrorsFound=.TRUE.
        }
    }

    // Check 2a -- setpoint node must also be an inlet or an outlet (CR8212)

    // Check 3 -- zone inlet nodes -- must be an outlet somewhere
    for (Loop1 = 1; Loop1 <= state.dataBranchNodeConnections->NumOfNodeConnections; ++Loop1) {
        if (state.dataBranchNodeConnections->NodeConnections(Loop1).ConnectionType !=
            ValidConnectionTypes(DataLoopNode::NodeConnectionType::ZoneInlet))
            continue;
        IsValid = false;
        for (Loop2 = 1; Loop2 <= state.dataBranchNodeConnections->NumOfNodeConnections; ++Loop2) {
            if (Loop1 == Loop2) continue;
            if (state.dataBranchNodeConnections->NodeConnections(Loop1).NodeNumber !=
                state.dataBranchNodeConnections->NodeConnections(Loop2).NodeNumber)
                continue;
            if (state.dataBranchNodeConnections->NodeConnections(Loop2).ConnectionType !=
                ValidConnectionTypes(DataLoopNode::NodeConnectionType::Outlet))
                continue;
            IsValid = true;
        }
        if (!IsValid) {
            ShowSevereError(state,
                            "Node Connection Error, Node=\"" + state.dataBranchNodeConnections->NodeConnections(Loop1).NodeName +
                                "\", ZoneInlet node did not find an outlet node.");
            ShowContinueError(state,
                              "Reference Object=" + state.dataBranchNodeConnections->NodeConnections(Loop1).ObjectType +
                                  ", Name=" + state.dataBranchNodeConnections->NodeConnections(Loop1).ObjectName);
            ++ErrorCounter;
            //      ErrorsFound=.TRUE.
        }
    }

    // Check 4 -- zone exhaust nodes -- must be an inlet somewhere
    for (Loop1 = 1; Loop1 <= state.dataBranchNodeConnections->NumOfNodeConnections; ++Loop1) {
        if (state.dataBranchNodeConnections->NodeConnections(Loop1).ConnectionType !=
            ValidConnectionTypes(DataLoopNode::NodeConnectionType::ZoneExhaust))
            continue;
        IsValid = false;
        for (Loop2 = 1; Loop2 <= state.dataBranchNodeConnections->NumOfNodeConnections; ++Loop2) {
            if (Loop1 == Loop2) continue;
            if (state.dataBranchNodeConnections->NodeConnections(Loop1).NodeNumber !=
                state.dataBranchNodeConnections->NodeConnections(Loop2).NodeNumber)
                continue;
            if (state.dataBranchNodeConnections->NodeConnections(Loop2).ConnectionType !=
                ValidConnectionTypes(DataLoopNode::NodeConnectionType::Inlet))
                continue;
            IsValid = true;
        }
        if (!IsValid) {
            ShowSevereError(state,
                            "Node Connection Error, Node=\"" + state.dataBranchNodeConnections->NodeConnections(Loop1).NodeName +
                                "\", ZoneExhaust node did not find a matching inlet node.");
            ShowContinueError(state,
                              "Reference Object=" + state.dataBranchNodeConnections->NodeConnections(Loop1).ObjectType +
                                  ", Name=" + state.dataBranchNodeConnections->NodeConnections(Loop1).ObjectName);
            ++ErrorCounter;
            //      ErrorsFound=.TRUE.
        }
    }

    // Check 5 -- return plenum induced air outlet nodes -- must be an inlet somewhere
    for (Loop1 = 1; Loop1 <= state.dataBranchNodeConnections->NumOfNodeConnections; ++Loop1) {
        if (state.dataBranchNodeConnections->NodeConnections(Loop1).ConnectionType !=
            ValidConnectionTypes(DataLoopNode::NodeConnectionType::InducedAir))
            continue;
        IsValid = false;
        for (Loop2 = 1; Loop2 <= state.dataBranchNodeConnections->NumOfNodeConnections; ++Loop2) {
            if (Loop1 == Loop2) continue;
            if (state.dataBranchNodeConnections->NodeConnections(Loop1).NodeNumber !=
                state.dataBranchNodeConnections->NodeConnections(Loop2).NodeNumber)
                continue;
            if (state.dataBranchNodeConnections->NodeConnections(Loop2).ConnectionType !=
                ValidConnectionTypes(DataLoopNode::NodeConnectionType::Inlet))
                continue;
            IsValid = true;
        }
        if (!IsValid) {
            ShowSevereError(state,
                            "Node Connection Error, Node=\"" + state.dataBranchNodeConnections->NodeConnections(Loop1).NodeName +
                                "\", Return plenum induced air outlet node did not find a matching inlet node.");
            ShowContinueError(state,
                              "Reference Object=" + state.dataBranchNodeConnections->NodeConnections(Loop1).ObjectType +
                                  ", Name=" + state.dataBranchNodeConnections->NodeConnections(Loop1).ObjectName);
            ++ErrorCounter;
            ErrorsFound = true;
        }
    }

    // Check 6 -- every inlet should have a matching outlet, zonereturn, zoneexhaust, induced air, reliefair or outsideair
    //    a)  If an InletNode's object is AirLoopHVAC, CondenserLoop, or PlantLoop, then skip the test.
    //    b)  If an InletNode's object is not one of the above types, it is valid if the
    //        same node name appears as an INLET to an AirLoopHVAC, CondenserLoop, or PlantLoop.
    for (Loop1 = 1; Loop1 <= state.dataBranchNodeConnections->NumOfNodeConnections; ++Loop1) {
        if (state.dataBranchNodeConnections->NodeConnections(Loop1).ConnectionType != ValidConnectionTypes(DataLoopNode::NodeConnectionType::Inlet))
            continue;
        if (state.dataBranchNodeConnections->NodeConnections(Loop1).ObjectType == "AIRLOOPHVAC" ||
            state.dataBranchNodeConnections->NodeConnections(Loop1).ObjectType == "CONDENSERLOOP" ||
            state.dataBranchNodeConnections->NodeConnections(Loop1).ObjectType == "PLANTLOOP")
            continue;
        IsValid = false;
        MatchedAtLeastOne = false;
        for (Loop2 = 1; Loop2 <= state.dataBranchNodeConnections->NumOfNodeConnections; ++Loop2) {
            if (Loop1 == Loop2) continue;
            if (state.dataBranchNodeConnections->NodeConnections(Loop1).NodeNumber !=
                state.dataBranchNodeConnections->NodeConnections(Loop2).NodeNumber)
                continue;
            if (state.dataBranchNodeConnections->NodeConnections(Loop2).ConnectionType ==
                    ValidConnectionTypes(DataLoopNode::NodeConnectionType::Outlet) ||
                state.dataBranchNodeConnections->NodeConnections(Loop2).ConnectionType ==
                    ValidConnectionTypes(DataLoopNode::NodeConnectionType::ZoneReturn) ||
                state.dataBranchNodeConnections->NodeConnections(Loop2).ConnectionType ==
                    ValidConnectionTypes(DataLoopNode::NodeConnectionType::ZoneExhaust) ||
                state.dataBranchNodeConnections->NodeConnections(Loop2).ConnectionType ==
                    ValidConnectionTypes(DataLoopNode::NodeConnectionType::InducedAir) ||
                state.dataBranchNodeConnections->NodeConnections(Loop2).ConnectionType ==
                    ValidConnectionTypes(DataLoopNode::NodeConnectionType::ReliefAir) ||
                state.dataBranchNodeConnections->NodeConnections(Loop2).ConnectionType ==
                    ValidConnectionTypes(DataLoopNode::NodeConnectionType::OutsideAir)) {
                MatchedAtLeastOne = true;
                continue;
            }
            if (state.dataBranchNodeConnections->NodeConnections(Loop2).ConnectionType ==
                    ValidConnectionTypes(DataLoopNode::NodeConnectionType::Inlet) &&
                (state.dataBranchNodeConnections->NodeConnections(Loop2).ObjectType == "AIRLOOPHVAC" ||
                 state.dataBranchNodeConnections->NodeConnections(Loop2).ObjectType == "CONDENSERLOOP" ||
                 state.dataBranchNodeConnections->NodeConnections(Loop2).ObjectType == "PLANTLOOP")) {
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
                              "Reference Object=" + state.dataBranchNodeConnections->NodeConnections(Loop1).ObjectType +
                                  ", Name=" + state.dataBranchNodeConnections->NodeConnections(Loop1).ObjectName);
            ++ErrorCounter;
            //      ErrorsFound=.TRUE.
        }
    }

    // Check 7 -- non-parent inlet nodes -- must never be an inlet more than once
    for (Loop1 = 1; Loop1 <= state.dataBranchNodeConnections->NumOfNodeConnections; ++Loop1) {
        // Only non-parent node connections
        if (state.dataBranchNodeConnections->NodeConnections(Loop1).ObjectIsParent) continue;
        if (state.dataBranchNodeConnections->NodeConnections(Loop1).ConnectionType != ValidConnectionTypes(DataLoopNode::NodeConnectionType::Inlet))
            continue;
        for (Loop2 = Loop1; Loop2 <= state.dataBranchNodeConnections->NumOfNodeConnections; ++Loop2) {
            if (Loop1 == Loop2) continue;
            if (state.dataBranchNodeConnections->NodeConnections(Loop2).ObjectIsParent) continue;
            if (state.dataBranchNodeConnections->NodeConnections(Loop2).ConnectionType !=
                ValidConnectionTypes(DataLoopNode::NodeConnectionType::Inlet))
                continue;
            if (state.dataBranchNodeConnections->NodeConnections(Loop2).NodeNumber ==
                state.dataBranchNodeConnections->NodeConnections(Loop1).NodeNumber) {
                ShowSevereError(state,
                                "Node Connection Error, Node=\"" + state.dataBranchNodeConnections->NodeConnections(Loop1).NodeName +
                                    "\", The same node appears as a non-parent Inlet node more than once.");
                ShowContinueError(state,
                                  "Reference Object=" + state.dataBranchNodeConnections->NodeConnections(Loop1).ObjectType +
                                      ", Name=" + state.dataBranchNodeConnections->NodeConnections(Loop1).ObjectName);
                ShowContinueError(state,
                                  "Reference Object=" + state.dataBranchNodeConnections->NodeConnections(Loop2).ObjectType +
                                      ", Name=" + state.dataBranchNodeConnections->NodeConnections(Loop2).ObjectName);
                ++ErrorCounter;
                //        ErrorsFound=.TRUE.
                break;
            }
        }
    }

    // Check 8 -- non-parent outlet nodes -- must never be an outlet more than once
    for (Loop1 = 1; Loop1 <= state.dataBranchNodeConnections->NumOfNodeConnections; ++Loop1) {
        // Only non-parent node connections
        if (state.dataBranchNodeConnections->NodeConnections(Loop1).ObjectIsParent) continue;
        if (state.dataBranchNodeConnections->NodeConnections(Loop1).ConnectionType != ValidConnectionTypes(DataLoopNode::NodeConnectionType::Outlet))
            continue;
        IsValid = true;
        for (Loop2 = Loop1; Loop2 <= state.dataBranchNodeConnections->NumOfNodeConnections; ++Loop2) {
            if (Loop1 == Loop2) continue;
            if (state.dataBranchNodeConnections->NodeConnections(Loop2).ObjectIsParent) continue;
            if (state.dataBranchNodeConnections->NodeConnections(Loop2).ConnectionType !=
                ValidConnectionTypes(DataLoopNode::NodeConnectionType::Outlet))
                continue;
            if (state.dataBranchNodeConnections->NodeConnections(Loop2).NodeNumber ==
                state.dataBranchNodeConnections->NodeConnections(Loop1).NodeNumber) {
                // Skip if one of the
                ShowSevereError(state,
                                "Node Connection Error, Node=\"" + state.dataBranchNodeConnections->NodeConnections(Loop1).NodeName +
                                    "\", The same node appears as a non-parent Outlet node more than once.");
                ShowContinueError(state,
                                  "Reference Object=" + state.dataBranchNodeConnections->NodeConnections(Loop1).ObjectType +
                                      ", Name=" + state.dataBranchNodeConnections->NodeConnections(Loop1).ObjectName);
                ShowContinueError(state,
                                  "Reference Object=" + state.dataBranchNodeConnections->NodeConnections(Loop2).ObjectType +
                                      ", Name=" + state.dataBranchNodeConnections->NodeConnections(Loop2).ObjectName);
                ++ErrorCounter;
                //        ErrorsFound=.TRUE.
                break;
            }
        }
    }

    // Check 9 -- nodes of type OutsideAirReference must be registered as DataLoopNode::NodeConnectionType::OutsideAir
    for (Loop1 = 1; Loop1 <= state.dataBranchNodeConnections->NumOfNodeConnections; ++Loop1) {
        if (state.dataBranchNodeConnections->NodeConnections(Loop1).ConnectionType !=
            ValidConnectionTypes(DataLoopNode::NodeConnectionType::OutsideAirReference))
            continue;
        IsValid = false;
        for (Loop2 = 1; Loop2 <= state.dataBranchNodeConnections->NumOfNodeConnections; ++Loop2) {
            if (Loop1 == Loop2) continue;
            if (state.dataBranchNodeConnections->NodeConnections(Loop1).NodeNumber !=
                state.dataBranchNodeConnections->NodeConnections(Loop2).NodeNumber)
                continue;
            if (state.dataBranchNodeConnections->NodeConnections(Loop2).ConnectionType !=
                ValidConnectionTypes(DataLoopNode::NodeConnectionType::OutsideAir))
                continue;
            IsValid = true;
            break;
        }
        if (!IsValid) {
            ShowSevereError(state,
                            format("{}{}{}",
                                   "Node Connection Error, Node=\"",
                                   state.dataBranchNodeConnections->NodeConnections(Loop1).NodeName,
                                   "\", Outdoor Air Reference did not find an appropriate \"outdoor air\" node."));
            ShowContinueError(state, "This node must be listed in an OutdoorAir:Node or OutdoorAir:NodeList object in order to set its conditions.");
            ShowContinueError(state,
                              "Reference Object=" + state.dataBranchNodeConnections->NodeConnections(Loop1).ObjectType +
                                  ", Name=" + state.dataBranchNodeConnections->NodeConnections(Loop1).ObjectName);
            ++ErrorCounter;
            //      ErrorsFound=.TRUE.
        }
    }

    // Check 10 -- fluid streams cannot have multiple inlet/outlet nodes on same component
    //  can have multiple inlets with one outlet or vice versa but cannot have multiple both inlet and outlet
    if (state.dataBranchNodeConnections->NumOfNodeConnections > 0) {
        MaxFluidStream = maxval(state.dataBranchNodeConnections->NodeConnections, &NodeConnectionDef::FluidStream);
        FluidStreamInletCount.allocate(MaxFluidStream);
        FluidStreamOutletCount.allocate(MaxFluidStream);
        FluidStreamCounts.allocate(MaxFluidStream);
        NodeObjects.allocate(state.dataBranchNodeConnections->NumOfNodeConnections);
        FluidStreamInletCount = 0;
        FluidStreamOutletCount = 0;
        NodeObjects = 0;
        FluidStreamCounts = false;
        // Following code relies on node connections for single object type/name being grouped together
        Object = 1;
        StartConnect = 1;
        EndConnect = 0;
        NumObjects = 2;
        NodeObjects(1) = 1;
        while (Object < state.dataBranchNodeConnections->NumOfNodeConnections) {
            if (state.dataBranchNodeConnections->NodeConnections(Object).ObjectType !=
                    state.dataBranchNodeConnections->NodeConnections(Object + 1).ObjectType ||
                state.dataBranchNodeConnections->NodeConnections(Object).ObjectName !=
                    state.dataBranchNodeConnections->NodeConnections(Object + 1).ObjectName) {
                EndConnect = Object + 1;
                NodeObjects(NumObjects) = EndConnect;
                if (Object + 1 < state.dataBranchNodeConnections->NumOfNodeConnections) ++NumObjects;
            }
            ++Object;
        }
        // NodeObjects now contains each consecutive object...
        for (Object = 1; Object <= NumObjects - 1; ++Object) {
            IsValid = true;
            FluidStreamInletCount = 0;
            FluidStreamOutletCount = 0;
            FluidStreamCounts = false;
            Loop1 = NodeObjects(Object);
            if (state.dataBranchNodeConnections->NumOfNodeConnections < 2) continue;
            if (state.dataBranchNodeConnections->NodeConnections(Loop1).ObjectIsParent) continue;
            if (state.dataBranchNodeConnections->NodeConnections(Loop1).ConnectionType ==
                ValidConnectionTypes(DataLoopNode::NodeConnectionType::Inlet))
                ++FluidStreamInletCount(state.dataBranchNodeConnections->NodeConnections(Loop1).FluidStream);
            if (state.dataBranchNodeConnections->NodeConnections(Loop1).ConnectionType ==
                ValidConnectionTypes(DataLoopNode::NodeConnectionType::Outlet))
                ++FluidStreamOutletCount(state.dataBranchNodeConnections->NodeConnections(Loop1).FluidStream);
            for (Loop2 = Loop1 + 1; Loop2 <= NodeObjects(Object + 1) - 1; ++Loop2) {
                if (state.dataBranchNodeConnections->NodeConnections(Loop2).ObjectIsParent) continue;
                if (state.dataBranchNodeConnections->NodeConnections(Loop2).ConnectionType ==
                    ValidConnectionTypes(DataLoopNode::NodeConnectionType::Inlet))
                    ++FluidStreamInletCount(state.dataBranchNodeConnections->NodeConnections(Loop2).FluidStream);
                if (state.dataBranchNodeConnections->NodeConnections(Loop2).ConnectionType ==
                    ValidConnectionTypes(DataLoopNode::NodeConnectionType::Outlet))
                    ++FluidStreamOutletCount(state.dataBranchNodeConnections->NodeConnections(Loop2).FluidStream);
            }
            for (Loop2 = 1; Loop2 <= MaxFluidStream; ++Loop2) {
                if (FluidStreamInletCount(Loop2) > 1 && FluidStreamOutletCount(Loop2) > 1) {
                    IsValid = false;
                    FluidStreamCounts(Loop2) = true;
                }
            }
            if (!IsValid) {
                ShowSevereError(state,
                                "(Developer) Node Connection Error, Object=" + state.dataBranchNodeConnections->NodeConnections(Loop1).ObjectType +
                                    ':' + state.dataBranchNodeConnections->NodeConnections(Loop1).ObjectName);
                ShowContinueError(state, "Object has multiple connections on both inlet and outlet fluid streams.");
                for (Loop2 = 1; Loop2 <= MaxFluidStream; ++Loop2) {
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
    for (Loop1 = 1; Loop1 <= state.dataBranchNodeConnections->NumOfNodeConnections; ++Loop1) {
        if (state.dataBranchNodeConnections->NodeConnections(Loop1).ConnectionType !=
            ValidConnectionTypes(DataLoopNode::NodeConnectionType::ZoneNode))
            continue;
        IsValid = true;
        for (Loop2 = Loop1; Loop2 <= state.dataBranchNodeConnections->NumOfNodeConnections; ++Loop2) {
            if (Loop1 == Loop2) continue;
            if (state.dataBranchNodeConnections->NodeConnections(Loop1).NodeName ==
                state.dataBranchNodeConnections->NodeConnections(Loop2).NodeName) {
                if (state.dataBranchNodeConnections->NodeConnections(Loop2).ConnectionType ==
                    ValidConnectionTypes(DataLoopNode::NodeConnectionType::Sensor))
                    continue;
                if (state.dataBranchNodeConnections->NodeConnections(Loop2).ConnectionType ==
                    ValidConnectionTypes(DataLoopNode::NodeConnectionType::Actuator))
                    continue;
                if (state.dataBranchNodeConnections->NodeConnections(Loop2).ConnectionType ==
                    ValidConnectionTypes(DataLoopNode::NodeConnectionType::SetPoint))
                    continue;
                ShowSevereError(state,
                                "Node Connection Error, Node Name=\"" + state.dataBranchNodeConnections->NodeConnections(Loop1).NodeName +
                                    "\", The same zone node appears more than once.");
                ShowContinueError(state,
                                  "Reference Object=" + state.dataBranchNodeConnections->NodeConnections(Loop1).ObjectType +
                                      ", Object Name=" + state.dataBranchNodeConnections->NodeConnections(Loop1).ObjectName);
                ShowContinueError(state,
                                  "Reference Object=" + state.dataBranchNodeConnections->NodeConnections(Loop2).ObjectType +
                                      ", Object Name=" + state.dataBranchNodeConnections->NodeConnections(Loop2).ObjectName);
                ++ErrorCounter;
                ErrorsFound = true;
            }
        }
    }

    state.dataBranchNodeConnections->NumNodeConnectionErrors += ErrorCounter;
}

bool IsParentObject(EnergyPlusData &state, std::string const &ComponentType, std::string const &ComponentName)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   May 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This routine determines if a component name is a parent node.

    // METHODOLOGY EMPLOYED:
    // Traverses CompSet structure.

    // Return value
    bool IsParent; // True if this combination is a parent

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int Loop;

    IsParent = false;
    for (Loop = 1; Loop <= state.dataBranchNodeConnections->NumOfNodeConnections; ++Loop) {
        if (state.dataBranchNodeConnections->NodeConnections(Loop).ObjectType == ComponentType &&
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

int WhichParentSet(EnergyPlusData &state, std::string const &ComponentType, std::string const &ComponentName)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   May 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This routine determines which parent node list (number) for a given component name
    // and type.

    // Return value
    int WhichOne;

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int Loop;

    WhichOne = 0;
    for (Loop = 1; Loop <= state.dataBranchNodeConnections->NumOfActualParents; ++Loop) {
        if (state.dataBranchNodeConnections->ParentNodeList(Loop).CType == ComponentType &&
            state.dataBranchNodeConnections->ParentNodeList(Loop).CName == ComponentName) {
            WhichOne = Loop;
            break;
        }
    }

    return WhichOne;
}

void GetParentData(EnergyPlusData &state,
                   std::string const &ComponentType,
                   std::string const &ComponentName,
                   std::string &InletNodeName,
                   int &InletNodeNum,
                   std::string &OutletNodeName,
                   int &OutletNodeNum,
                   bool &ErrorsFound)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   May 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This routine gets node data for a given Parent Component Type and Name Name.

    // METHODOLOGY EMPLOYED:
    // Traverses CompSet structure.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    bool ErrInObject;
    int Which;

    InletNodeName = std::string();
    InletNodeNum = 0;
    OutletNodeName = std::string();
    OutletNodeNum = 0;
    ErrInObject = false;

    Which = WhichParentSet(state, ComponentType, ComponentName);
    if (Which != 0) {
        InletNodeName = state.dataBranchNodeConnections->ParentNodeList(Which).InletNodeName;
        OutletNodeName = state.dataBranchNodeConnections->ParentNodeList(Which).OutletNodeName;
        // Get Node Numbers
        InletNodeNum = UtilityRoutines::FindItemInList(
            InletNodeName, state.dataLoopNodes->NodeID({1, state.dataLoopNodes->NumOfNodes}), state.dataLoopNodes->NumOfNodes);
        OutletNodeNum = UtilityRoutines::FindItemInList(
            OutletNodeName, state.dataLoopNodes->NodeID({1, state.dataLoopNodes->NumOfNodes}), state.dataLoopNodes->NumOfNodes);
    } else if (IsParentObjectCompSet(state, ComponentType, ComponentName)) {
        Which = WhichCompSet(state, ComponentType, ComponentName);
        if (Which != 0) {
            InletNodeName = state.dataBranchNodeConnections->CompSets(Which).InletNodeName;
            OutletNodeName = state.dataBranchNodeConnections->CompSets(Which).OutletNodeName;
            InletNodeNum = UtilityRoutines::FindItemInList(
                InletNodeName, state.dataLoopNodes->NodeID({1, state.dataLoopNodes->NumOfNodes}), state.dataLoopNodes->NumOfNodes);
            OutletNodeNum = UtilityRoutines::FindItemInList(
                OutletNodeName, state.dataLoopNodes->NodeID({1, state.dataLoopNodes->NumOfNodes}), state.dataLoopNodes->NumOfNodes);
        } else {
            ErrInObject = true;
            ShowWarningError(state, "GetParentData: Component Type=" + ComponentType + ", Component Name=" + ComponentName + " not found.");
        }
    } else {
        ErrInObject = true;
        ShowWarningError(state, "GetParentData: Component Type=" + ComponentType + ", Component Name=" + ComponentName + " not found.");
    }

    if (ErrInObject) ErrorsFound = true;
}

bool IsParentObjectCompSet(EnergyPlusData &state, std::string const &ComponentType, std::string const &ComponentName)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   May 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This routine determines if a component name is a parent node.

    // METHODOLOGY EMPLOYED:
    // Traverses CompSet structure.

    // Return value
    bool IsParent; // True if this combination is a parent

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int Loop;

    IsParent = false;
    for (Loop = 1; Loop <= state.dataBranchNodeConnections->NumCompSets; ++Loop) {
        if (state.dataBranchNodeConnections->CompSets(Loop).ParentCType == ComponentType &&
            state.dataBranchNodeConnections->CompSets(Loop).ParentCName == ComponentName) {
            IsParent = true;
            break;
        }
    }

    return IsParent;
}

int WhichCompSet(EnergyPlusData &state, std::string const &ComponentType, std::string const &ComponentName)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   May 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This routine determines which comp set (number) for a given component name
    // and type.

    // METHODOLOGY EMPLOYED:
    // Traverses CompSet structure.

    // Return value
    int WhichOne;

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int Loop;

    WhichOne = 0;
    for (Loop = 1; Loop <= state.dataBranchNodeConnections->NumCompSets; ++Loop) {
        if (state.dataBranchNodeConnections->CompSets(Loop).CType == ComponentType &&
            state.dataBranchNodeConnections->CompSets(Loop).CName == ComponentName) {
            WhichOne = Loop;
            break;
        }
    }

    return WhichOne;
}

int GetNumChildren(EnergyPlusData &state, std::string const &ComponentType, std::string const &ComponentName)
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

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int Loop;

    NumChildren = 0;
    if (IsParentObject(state, ComponentType, ComponentName)) {
        for (Loop = 1; Loop <= state.dataBranchNodeConnections->NumCompSets; ++Loop) {
            if (state.dataBranchNodeConnections->CompSets(Loop).ParentCType == ComponentType &&
                state.dataBranchNodeConnections->CompSets(Loop).ParentCName == ComponentName) {
                ++NumChildren;
            }
        }
    }

    return NumChildren;
}

void GetComponentData(EnergyPlusData &state,
                      std::string const &ComponentType,
                      std::string const &ComponentName,
                      bool &IsParent, // true or false
                      int &NumInlets,
                      Array1D_string &InletNodeNames,
                      Array1D_int &InletNodeNums,
                      Array1D_int &InletFluidStreams,
                      int &NumOutlets,
                      Array1D_string &OutletNodeNames,
                      Array1D_int &OutletNodeNums,
                      Array1D_int &OutletFluidStreams,
                      bool &ErrorsFound // set to true if errors found, unchanged otherwise
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   May 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This routine gets data for a given Component Type and Name Name.

    // METHODOLOGY EMPLOYED:
    // Traverses CompSet structure.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    bool ErrInObject;
    int Which;

    if (allocated(InletNodeNames)) InletNodeNames.deallocate();
    if (allocated(InletNodeNums)) InletNodeNums.deallocate();
    if (allocated(InletFluidStreams)) InletFluidStreams.deallocate();
    if (allocated(OutletNodeNames)) OutletNodeNames.deallocate();
    if (allocated(OutletNodeNums)) OutletNodeNums.deallocate();
    if (allocated(OutletFluidStreams)) OutletFluidStreams.deallocate();

    NumInlets = 0;
    NumOutlets = 0;

    //  FoundObject=.FALSE.
    IsParent = false;
    for (Which = 1; Which <= state.dataBranchNodeConnections->NumOfNodeConnections; ++Which) {
        if (state.dataBranchNodeConnections->NodeConnections(Which).ObjectType != ComponentType ||
            state.dataBranchNodeConnections->NodeConnections(Which).ObjectName != ComponentName)
            continue;
        //    FoundObject=.TRUE.
        if (state.dataBranchNodeConnections->NodeConnections(Which).ObjectIsParent) IsParent = true;
        if (UtilityRoutines::SameString(state.dataBranchNodeConnections->NodeConnections(Which).ConnectionType, "Inlet")) ++NumInlets;
        if (UtilityRoutines::SameString(state.dataBranchNodeConnections->NodeConnections(Which).ConnectionType, "Outlet")) ++NumOutlets;
    }

    InletNodeNames.allocate(NumInlets);
    InletNodeNums.allocate(NumInlets);
    InletFluidStreams.allocate(NumInlets);
    OutletNodeNames.allocate(NumOutlets);
    OutletNodeNums.allocate(NumOutlets);
    OutletFluidStreams.allocate(NumOutlets);

    InletNodeNames = std::string();
    InletNodeNums = 0;
    InletFluidStreams = 0;
    OutletNodeNames = std::string();
    OutletNodeNums = 0;
    OutletFluidStreams = 0;
    NumInlets = 0;
    NumOutlets = 0;
    ErrInObject = false;

    for (Which = 1; Which <= state.dataBranchNodeConnections->NumOfNodeConnections; ++Which) {
        if (state.dataBranchNodeConnections->NodeConnections(Which).ObjectType != ComponentType ||
            state.dataBranchNodeConnections->NodeConnections(Which).ObjectName != ComponentName)
            continue;
        if (UtilityRoutines::SameString(state.dataBranchNodeConnections->NodeConnections(Which).ConnectionType, "Inlet")) {
            ++NumInlets;
            InletNodeNames(NumInlets) = state.dataBranchNodeConnections->NodeConnections(Which).NodeName;
            InletNodeNums(NumInlets) = state.dataBranchNodeConnections->NodeConnections(Which).NodeNumber;
            InletFluidStreams(NumInlets) = state.dataBranchNodeConnections->NodeConnections(Which).FluidStream;
        }
        if (UtilityRoutines::SameString(state.dataBranchNodeConnections->NodeConnections(Which).ConnectionType, "Outlet")) {
            ++NumOutlets;
            OutletNodeNames(NumOutlets) = state.dataBranchNodeConnections->NodeConnections(Which).NodeName;
            OutletNodeNums(NumOutlets) = state.dataBranchNodeConnections->NodeConnections(Which).NodeNumber;
            OutletFluidStreams(NumOutlets) = state.dataBranchNodeConnections->NodeConnections(Which).FluidStream;
        }
    }
    if (ErrInObject) {
        ShowWarningError(state, "GetComponentData: Component Type=" + ComponentType + ", Component Name=" + ComponentName + " not found.");
    }

    if (ErrInObject) ErrorsFound = true;
}

void GetChildrenData(EnergyPlusData &state,
                     std::string const &ComponentType,
                     std::string const &ComponentName,
                     int &NumChildren,
                     Array1D_string &ChildrenCType,
                     Array1D_string &ChildrenCName,
                     Array1D_string &InletNodeName,
                     Array1D_int &InletNodeNum,
                     Array1D_string &OutletNodeName,
                     Array1D_int &OutletNodeNum,
                     bool &ErrorsFound)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   May 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This routine gets children data for given parent node.

    // METHODOLOGY EMPLOYED:
    // Traverses CompSet structure.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Array1D_string ChildCType;
    Array1D_string ChildCName;
    Array1D_string ChildInNodeName;
    Array1D_string ChildOutNodeName;
    Array1D_int ChildInNodeNum;
    Array1D_int ChildOutNodeNum;
    int Loop;
    int CountNum;
    bool ErrInObject;
    std::string MatchNodeName;
    std::string ParentInletNodeName;
    std::string ParentOutletNodeName;
    int ParentInletNodeNum;
    int ParentOutletNodeNum;
    // unused1109  LOGICAL Matched
    int CountMatchLoop;

    ChildrenCType = std::string();
    ChildrenCName = std::string();
    InletNodeName = std::string();
    InletNodeNum = 0;
    OutletNodeName = std::string();
    OutletNodeNum = 0;
    ErrInObject = false;

    if (IsParentObject(state, ComponentType, ComponentName)) {
        NumChildren = GetNumChildren(state, ComponentType, ComponentName);
        if (NumChildren == 0) {
            ShowWarningError(state, "GetChildrenData: Parent Node has no children, node=" + ComponentType + ':' + ComponentName);
        } else {
            GetParentData(
                state, ComponentType, ComponentName, ParentInletNodeName, ParentInletNodeNum, ParentOutletNodeName, ParentOutletNodeNum, ErrInObject);
            ChildCType.allocate(NumChildren);
            ChildCName.allocate(NumChildren);
            ChildInNodeName.allocate(NumChildren);
            ChildOutNodeName.allocate(NumChildren);
            ChildInNodeNum.allocate(NumChildren);
            ChildOutNodeNum.allocate(NumChildren);
            ChildCType = std::string();
            ChildCName = std::string();
            ChildInNodeName = std::string();
            ChildOutNodeName = std::string();
            ChildInNodeNum = 0;
            ChildOutNodeNum = 0;
            CountNum = 0;
            for (Loop = 1; Loop <= state.dataBranchNodeConnections->NumCompSets; ++Loop) {
                if (state.dataBranchNodeConnections->CompSets(Loop).ParentCType == ComponentType &&
                    state.dataBranchNodeConnections->CompSets(Loop).ParentCName == ComponentName) {
                    ++CountNum;
                    ChildCType(CountNum) = state.dataBranchNodeConnections->CompSets(Loop).CType;
                    ChildCName(CountNum) = state.dataBranchNodeConnections->CompSets(Loop).CName;
                    ChildInNodeName(CountNum) = state.dataBranchNodeConnections->CompSets(Loop).InletNodeName;
                    ChildOutNodeName(CountNum) = state.dataBranchNodeConnections->CompSets(Loop).OutletNodeName;
                    // Get Node Numbers
                    ChildInNodeNum(CountNum) = UtilityRoutines::FindItemInList(ChildInNodeName(CountNum),
                                                                               state.dataLoopNodes->NodeID({1, state.dataLoopNodes->NumOfNodes}),
                                                                               state.dataLoopNodes->NumOfNodes);
                    ChildOutNodeNum(CountNum) = UtilityRoutines::FindItemInList(ChildOutNodeName(CountNum),
                                                                                state.dataLoopNodes->NodeID({1, state.dataLoopNodes->NumOfNodes}),
                                                                                state.dataLoopNodes->NumOfNodes);
                }
            }
            if (CountNum != NumChildren) {
                ShowSevereError(state, "GetChildrenData: Counted nodes not equal to GetNumChildren count");
                ErrInObject = true;
            } else {
                // Children arrays built.  Now "sort" for flow connection order(?)
                MatchNodeName = ParentInletNodeName;
                CountNum = 0;
                CountMatchLoop = 0;
                while (CountMatchLoop < NumChildren) {
                    ++CountMatchLoop;
                    //          Matched=.FALSE.
                    for (Loop = 1; Loop <= NumChildren; ++Loop) {
                        if (ChildInNodeName(Loop) == MatchNodeName) {
                            ++CountNum;
                            ChildrenCType(CountNum) = ChildCType(Loop);
                            ChildrenCName(CountNum) = ChildCName(Loop);
                            InletNodeName(CountNum) = ChildInNodeName(Loop);
                            InletNodeNum(CountNum) = ChildInNodeNum(Loop);
                            OutletNodeName(CountNum) = ChildOutNodeName(Loop);
                            OutletNodeNum(CountNum) = ChildOutNodeNum(Loop);
                            ChildInNodeName(Loop).clear(); // So it won't match anymore
                            //              Matched=.TRUE.
                            MatchNodeName = ChildOutNodeName(Loop);
                            break;
                        }
                    }
                }
                if (MatchNodeName != ParentOutletNodeName) {
                    for (Loop = 1; Loop <= NumChildren; ++Loop) {
                        if (ChildInNodeName(Loop).empty()) continue;
                        if (ChildOutNodeName(Loop) == ParentOutletNodeName) break;
                        break;
                    }
                }
                for (Loop = 1; Loop <= NumChildren; ++Loop) {
                    if (ChildInNodeName(Loop).empty()) continue;
                    ++CountNum;
                    ChildrenCType(CountNum) = ChildCType(Loop);
                    ChildrenCName(CountNum) = ChildCName(Loop);
                    InletNodeName(CountNum) = ChildInNodeName(Loop);
                    InletNodeNum(CountNum) = ChildInNodeNum(Loop);
                    OutletNodeName(CountNum) = ChildOutNodeName(Loop);
                    OutletNodeNum(CountNum) = ChildOutNodeNum(Loop);
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
        ShowSevereError(state, "GetChildrenData: Requested Children Data for non Parent Node=" + ComponentType + ':' + ComponentName);
        ErrInObject = true;
    }

    if (ErrInObject) ErrorsFound = true;
}

void SetUpCompSets(EnergyPlusData &state,
                   std::string_view ParentType,    // Parent Object Type
                   std::string_view ParentName,    // Parent Object Name
                   std::string_view CompType,      // Component Type
                   std::string_view CompName,      // Component Name
                   std::string_view InletNode,     // Inlet Node Name
                   std::string_view OutletNode,    // Outlet Node Name
                   Optional_string_const Description // Description
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   November 2001
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine sets up "Component Sets" as input in the branch
    // lists.  These can be used later to verify that the proper names and
    // inlet/outlet nodes have been input.  This routine assumes that identical
    // "CompSets" cannot be used in multiple places and issues a warning if they are.
    // This subroutine also

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    std::string CompTypeUC;   // Component type in upper case
    std::string ParentTypeUC; // Parent component type in upper case
    int Count;
    int Count2;
    int Found;
    int Found2;

    // Object Data

    ParentTypeUC = UtilityRoutines::MakeUPPERCase(ParentType);
    CompTypeUC = UtilityRoutines::MakeUPPERCase(CompType);
    Found = 0;

    // See if Component-Nodes set is already there - should be unique
    // Try to fill in blanks (passed in as undefined
    for (Count = 1; Count <= state.dataBranchNodeConnections->NumCompSets; ++Count) {
        if (CompName != state.dataBranchNodeConnections->CompSets(Count).CName) continue;
        if (CompTypeUC != "UNDEFINED") {
            if (CompTypeUC != state.dataBranchNodeConnections->CompSets(Count).CType) continue;
        }
        // Component name matches, component type matches or is undefined
        if (InletNode != "UNDEFINED") {
            if (state.dataBranchNodeConnections->CompSets(Count).InletNodeName != "UNDEFINED") {
                if (InletNode != state.dataBranchNodeConnections->CompSets(Count).InletNodeName) continue;
            } else {
                state.dataBranchNodeConnections->CompSets(Count).InletNodeName = InletNode;
            }
        }
        if (OutletNode != "UNDEFINED") {
            if (state.dataBranchNodeConnections->CompSets(Count).OutletNodeName != "UNDEFINED") {
                if (OutletNode != state.dataBranchNodeConnections->CompSets(Count).OutletNodeName) continue;
            } else {
                state.dataBranchNodeConnections->CompSets(Count).OutletNodeName = OutletNode;
            }
        }
        //  See if something undefined and set here
        if (state.dataBranchNodeConnections->CompSets(Count).ParentCType == "UNDEFINED" &&
            state.dataBranchNodeConnections->CompSets(Count).ParentCName == "UNDEFINED") {
            // Assume this is a further definition for this compset
            state.dataBranchNodeConnections->CompSets(Count).ParentCType = ParentTypeUC;
            state.dataBranchNodeConnections->CompSets(Count).ParentCName = ParentName;
            if (present(Description)) state.dataBranchNodeConnections->CompSets(Count).Description = Description;
            Found = Count;
            break;
        }
    }
    if (Found == 0) {
        for (Count = 1; Count <= state.dataBranchNodeConnections->NumCompSets; ++Count) {
            Found = 0;
            // Test if inlet node has been used before as an inlet node
            // If the matching node name does not belong to the parent object, then error
            // For example a fan may share the same inlet node as the furnace object which is its parent
            if (InletNode != state.dataBranchNodeConnections->CompSets(Count).InletNodeName) {
                continue;
                // If parent type is "UNDEFINED" then no error
            } else if ((ParentTypeUC == "UNDEFINED") || (state.dataBranchNodeConnections->CompSets(Count).ParentCType == "UNDEFINED")) {
                // If node name is "UNDEFINED" then no error
            } else if (InletNode != "UNDEFINED") {
                // If the matching node name does not belong to the parent or child object, then error
                // For example a fan may share the same inlet node as the furnace object which is its parent
                if ((ParentTypeUC == state.dataBranchNodeConnections->CompSets(Count).CType) &&
                    (ParentName == state.dataBranchNodeConnections->CompSets(Count).CName)) {
                    // OK - The duplicate inlet node belongs to this component's parent
                } else if ((CompTypeUC == state.dataBranchNodeConnections->CompSets(Count).ParentCType) &&
                           (CompName == state.dataBranchNodeConnections->CompSets(Count).ParentCName)) {
                    // OK - The duplicate inlet node belongs to a child of this component
                } else {
                    // Due to possibility of grandparents or more, if the matching node name
                    // belongs to a component that appears as a parent, then OK
                    Found2 = 0;
                    for (Count2 = 1; Count2 <= state.dataBranchNodeConnections->NumCompSets; ++Count2) {
                        if ((state.dataBranchNodeConnections->CompSets(Count).CType ==
                             state.dataBranchNodeConnections->CompSets(Count2).ParentCType) &&
                            (state.dataBranchNodeConnections->CompSets(Count).CName == state.dataBranchNodeConnections->CompSets(Count2).ParentCName))
                            Found2 = 1;
                        if ((CompTypeUC == state.dataBranchNodeConnections->CompSets(Count2).ParentCType) &&
                            (CompName == state.dataBranchNodeConnections->CompSets(Count2).ParentCName))
                            Found2 = 1;
                    }
                    if (Found2 == 0) {
                        ShowWarningError(state, "Node used as an inlet more than once: " + std::string{InletNode});
                        ShowContinueError(state,
                                          "  Used by     : " + state.dataBranchNodeConnections->CompSets(Count).ParentCType +
                                              ", name=" + state.dataBranchNodeConnections->CompSets(Count).ParentCName);
                        ShowContinueError(state,
                                          "  as inlet for: " + state.dataBranchNodeConnections->CompSets(Count).CType +
                                              ", name=" + state.dataBranchNodeConnections->CompSets(Count).CName);
                        ShowContinueError(state, format("{}{}{}", "  and  by     : ", ParentTypeUC + ", name=", ParentName));
                        ShowContinueError(state, format("{}{}{}", "  as inlet for: ", CompTypeUC + ", name=", CompName));
                    }
                }
            }
            // Test if outlet node has been used before as an outlet node
            // If the matching node name does not belong to the parent or child object, then error
            // For example a fan may share the same outlet node as the furnace object which is its parent
            if (OutletNode != state.dataBranchNodeConnections->CompSets(Count).OutletNodeName) {
                continue;
                // If parent type is "UNDEFINED" then no error
            } else if ((ParentTypeUC == "UNDEFINED") || (state.dataBranchNodeConnections->CompSets(Count).ParentCType == "UNDEFINED")) {
                // If node name is "UNDEFINED" then no error
            } else if (OutletNode != "UNDEFINED") {
                if ((ParentTypeUC == state.dataBranchNodeConnections->CompSets(Count).CType) &&
                    (ParentName == state.dataBranchNodeConnections->CompSets(Count).CName)) {
                    // OK - The duplicate outlet node belongs to this component's parent
                } else if ((CompTypeUC == state.dataBranchNodeConnections->CompSets(Count).ParentCType) &&
                           (CompName == state.dataBranchNodeConnections->CompSets(Count).ParentCName)) {
                    // OK - The duplicate outlet node belongs to a child of this component
                } else {
                    // Due to possibility of grandparents or more, if the matching node name
                    // belongs to a component that appears as a parent, then OK
                    Found2 = 0;
                    for (Count2 = 1; Count2 <= state.dataBranchNodeConnections->NumCompSets; ++Count2) {
                        if ((state.dataBranchNodeConnections->CompSets(Count).CType ==
                             state.dataBranchNodeConnections->CompSets(Count2).ParentCType) &&
                            (state.dataBranchNodeConnections->CompSets(Count).CName == state.dataBranchNodeConnections->CompSets(Count2).ParentCName))
                            Found2 = 1;
                        if ((CompTypeUC == state.dataBranchNodeConnections->CompSets(Count2).ParentCType) &&
                            (CompName == state.dataBranchNodeConnections->CompSets(Count2).ParentCName))
                            Found2 = 1;
                    }
                    // This rule is violated by dual duct units, so let it pass
                    if ((Found2 == 0) && (!has_prefixi(state.dataBranchNodeConnections->CompSets(Count).CType, "AirTerminal:DualDuct:")) &&
                        (!has_prefixi(CompTypeUC, "AirTerminal:DualDuct:"))) {
                        ShowWarningError(state, "Node used as an outlet more than once: " + std::string{OutletNode});
                        ShowContinueError(state,
                                          "  Used by     : " + state.dataBranchNodeConnections->CompSets(Count).ParentCType +
                                              ", name=" + state.dataBranchNodeConnections->CompSets(Count).ParentCName);
                        ShowContinueError(state,
                                          "  as outlet for: " + state.dataBranchNodeConnections->CompSets(Count).CType +
                                              ", name=" + state.dataBranchNodeConnections->CompSets(Count).CName);
                        ShowContinueError(state, format("{}{}{}", "  and  by     : ", ParentTypeUC + ", name=", ParentName));
                        ShowContinueError(state, format("{}{}{}", "  as outlet for: ", CompTypeUC + ", name=", CompName));
                    }
                }
            }
            if (CompTypeUC != state.dataBranchNodeConnections->CompSets(Count).CType && CompTypeUC != "UNDEFINED") continue;
            if (CompName != state.dataBranchNodeConnections->CompSets(Count).CName) continue;
            Found = Count;
            break;
        }
    }
    if (Found == 0) {
        state.dataBranchNodeConnections->CompSets.redimension(++state.dataBranchNodeConnections->NumCompSets);
        state.dataBranchNodeConnections->CompSets(state.dataBranchNodeConnections->NumCompSets).ParentCType = ParentTypeUC;
        state.dataBranchNodeConnections->CompSets(state.dataBranchNodeConnections->NumCompSets).ParentCName = ParentName;
        state.dataBranchNodeConnections->CompSets(state.dataBranchNodeConnections->NumCompSets).CType = CompTypeUC;
        state.dataBranchNodeConnections->CompSets(state.dataBranchNodeConnections->NumCompSets).CName = CompName;
        state.dataBranchNodeConnections->CompSets(state.dataBranchNodeConnections->NumCompSets).InletNodeName =
            UtilityRoutines::MakeUPPERCase(InletNode); // TODO: Fix this....
        state.dataBranchNodeConnections->CompSets(state.dataBranchNodeConnections->NumCompSets).OutletNodeName =
            UtilityRoutines::MakeUPPERCase(OutletNode); // TODO: Fix this....
        if (present(Description)) {
            state.dataBranchNodeConnections->CompSets(state.dataBranchNodeConnections->NumCompSets).Description = Description;
        } else {
            state.dataBranchNodeConnections->CompSets(state.dataBranchNodeConnections->NumCompSets).Description = "UNDEFINED";
        }
    }
}

void TestInletOutletNodes(EnergyPlusData &state, [[maybe_unused]] bool &ErrorsFound)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   November 2001
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine tests the branches to see if a duplicate inlet node
    // exists under a different name in the sequence; likewise for outlet.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int Count;
    int Other;
    Array1D_bool AlreadyNoted;

    // Test component sets created by branches
    AlreadyNoted.dimension(state.dataBranchNodeConnections->NumCompSets, false);
    for (Count = 1; Count <= state.dataBranchNodeConnections->NumCompSets; ++Count) {
        for (Other = 1; Other <= state.dataBranchNodeConnections->NumCompSets; ++Other) {
            if (Count == Other) continue;
            if (state.dataBranchNodeConnections->CompSets(Count).InletNodeName != state.dataBranchNodeConnections->CompSets(Other).InletNodeName)
                continue;
            if (AlreadyNoted(Count)) continue;
            //  All other values must match
            if (state.dataBranchNodeConnections->CompSets(Count).CType != state.dataBranchNodeConnections->CompSets(Other).CType ||
                state.dataBranchNodeConnections->CompSets(Count).CName != state.dataBranchNodeConnections->CompSets(Other).CName ||
                state.dataBranchNodeConnections->CompSets(Count).OutletNodeName != state.dataBranchNodeConnections->CompSets(Other).OutletNodeName) {
                AlreadyNoted(Other) = true;
                ShowWarningError(state, "Node used as an inlet more than once: " + state.dataBranchNodeConnections->CompSets(Count).InletNodeName);
                ShowContinueError(state,
                                  "  Used by     : " + state.dataBranchNodeConnections->CompSets(Count).ParentCType +
                                      ", name=" + state.dataBranchNodeConnections->CompSets(Count).ParentCName);
                ShowContinueError(state,
                                  "  as inlet for: " + state.dataBranchNodeConnections->CompSets(Count).CType +
                                      ", name=" + state.dataBranchNodeConnections->CompSets(Count).CName);
                ShowContinueError(state,
                                  "  and  by     : " + state.dataBranchNodeConnections->CompSets(Other).ParentCType +
                                      ", name=" + state.dataBranchNodeConnections->CompSets(Other).ParentCName);
                ShowContinueError(state,
                                  "  as inlet for: " + state.dataBranchNodeConnections->CompSets(Other).CType +
                                      ", name=" + state.dataBranchNodeConnections->CompSets(Other).CName);
                //        ErrorsFound=.TRUE.
            }
        }
    }

    AlreadyNoted = false;
    for (Count = 1; Count <= state.dataBranchNodeConnections->NumCompSets; ++Count) {
        for (Other = 1; Other <= state.dataBranchNodeConnections->NumCompSets; ++Other) {
            if (Count == Other) continue;
            if (state.dataBranchNodeConnections->CompSets(Count).OutletNodeName != state.dataBranchNodeConnections->CompSets(Other).OutletNodeName)
                continue;
            if (AlreadyNoted(Count)) continue;
            //  All other values must match
            if (state.dataBranchNodeConnections->CompSets(Count).CType != state.dataBranchNodeConnections->CompSets(Other).CType ||
                state.dataBranchNodeConnections->CompSets(Count).CName != state.dataBranchNodeConnections->CompSets(Other).CName ||
                state.dataBranchNodeConnections->CompSets(Count).InletNodeName != state.dataBranchNodeConnections->CompSets(Other).InletNodeName) {
                AlreadyNoted(Other) = true;
                ShowWarningError(state, "Node used as an outlet more than once: " + state.dataBranchNodeConnections->CompSets(Count).OutletNodeName);
                ShowContinueError(state,
                                  "  Used by      : " + state.dataBranchNodeConnections->CompSets(Count).ParentCType +
                                      ", name=" + state.dataBranchNodeConnections->CompSets(Count).ParentCName);
                ShowContinueError(state,
                                  "  as outlet for: " + state.dataBranchNodeConnections->CompSets(Count).CType +
                                      ", name=" + state.dataBranchNodeConnections->CompSets(Count).CName);
                ShowContinueError(state,
                                  "  and  by      : " + state.dataBranchNodeConnections->CompSets(Other).ParentCType +
                                      ", name=" + state.dataBranchNodeConnections->CompSets(Other).ParentCName);
                ShowContinueError(state,
                                  "  as outlet for: " + state.dataBranchNodeConnections->CompSets(Other).CType +
                                      ", name=" + state.dataBranchNodeConnections->CompSets(Other).CName);
                //        ErrorsFound=.TRUE.
            }
        }
    }

    AlreadyNoted.deallocate();
}

void TestCompSet(EnergyPlusData &state,
                 std::string const &CompType,   // Component Type
                 std::string_view CompName,   // Component Name
                 std::string const &InletNode,  // Inlet Node Name
                 std::string const &OutletNode, // Outlet Node Name
                 std::string const &Description // Description of Node Pair (for warning message)
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   November 2001
    //       MODIFIED       na
    //       RE-ENGINEERED  na

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

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int Count;
    int Found;
    std::string CompTypeUC; // Component type in upper case

    CompTypeUC = UtilityRoutines::MakeUPPERCase(CompType);

    // See if Already there
    Found = 0;
    for (Count = 1; Count <= state.dataBranchNodeConnections->NumCompSets; ++Count) {
        if ((CompTypeUC != state.dataBranchNodeConnections->CompSets(Count).CType) &&
            (state.dataBranchNodeConnections->CompSets(Count).CType != "UNDEFINED"))
            continue;
        if (CompName != state.dataBranchNodeConnections->CompSets(Count).CName) continue;
        if ((InletNode != state.dataBranchNodeConnections->CompSets(Count).InletNodeName) &&
            (state.dataBranchNodeConnections->CompSets(Count).InletNodeName != "UNDEFINED") && (InletNode != "UNDEFINED"))
            continue;
        if ((OutletNode != state.dataBranchNodeConnections->CompSets(Count).OutletNodeName) &&
            (state.dataBranchNodeConnections->CompSets(Count).OutletNodeName != "UNDEFINED") && (OutletNode != "UNDEFINED"))
            continue;

        Found = Count;
        break;
    }

    if (Found == 0) {
        SetUpCompSets(state, "UNDEFINED", "UNDEFINED", CompType, CompName, InletNode, OutletNode, Description);
    } else {
        // Fill in node names and component type for previously undefined values:
        //   If the parent object did not specify a component type or inlet or outlet node, then that value
        //   is UNDEFINED in CompSets.  When a component calls TestCompSet, the comp type and inlet and
        //   outlet nodes are known, so they can be filled in for future reference.
        if (state.dataBranchNodeConnections->CompSets(Found).CType == "UNDEFINED")
            state.dataBranchNodeConnections->CompSets(Found).CType = CompTypeUC;
        if (state.dataBranchNodeConnections->CompSets(Found).InletNodeName == "UNDEFINED")
            state.dataBranchNodeConnections->CompSets(Found).InletNodeName = InletNode;
        if (state.dataBranchNodeConnections->CompSets(Found).OutletNodeName == "UNDEFINED")
            state.dataBranchNodeConnections->CompSets(Found).OutletNodeName = OutletNode;
        if (state.dataBranchNodeConnections->CompSets(Found).Description == "UNDEFINED")
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
    int Count;
    int Other;
    Array1D_bool AlreadyNoted;

    // Test component sets created by branches
    AlreadyNoted.dimension(state.dataBranchNodeConnections->NumCompSets, false);
    for (Count = 1; Count <= state.dataBranchNodeConnections->NumCompSets; ++Count) {
        for (Other = 1; Other <= state.dataBranchNodeConnections->NumCompSets; ++Other) {
            if (Count == Other) continue;
            if (state.dataBranchNodeConnections->CompSets(Count).CType == "SOLARCOLLECTOR:UNGLAZEDTRANSPIRED") continue;
            if (state.dataBranchNodeConnections->CompSets(Count).CType != state.dataBranchNodeConnections->CompSets(Other).CType ||
                state.dataBranchNodeConnections->CompSets(Count).CName != state.dataBranchNodeConnections->CompSets(Other).CName)
                continue;
            if (state.dataBranchNodeConnections->CompSets(Count).Description != state.dataBranchNodeConnections->CompSets(Other).Description) {
                if (state.dataBranchNodeConnections->CompSets(Count).Description != "UNDEFINED" &&
                    state.dataBranchNodeConnections->CompSets(Other).Description != "UNDEFINED")
                    continue;
            }
            if (state.dataBranchNodeConnections->CompSets(Count).InletNodeName == state.dataBranchNodeConnections->CompSets(Other).InletNodeName)
                continue;
            if (state.dataBranchNodeConnections->CompSets(Count).OutletNodeName == state.dataBranchNodeConnections->CompSets(Other).OutletNodeName)
                continue;
            if (AlreadyNoted(Count)) continue;
            //  All other values must match
            AlreadyNoted(Other) = true;
            ShowSevereError(state, "Same component name and type has differing Node Names.");
            ShowContinueError(state,
                              "   Component:    " + state.dataBranchNodeConnections->CompSets(Count).CType +
                                  ", name=" + state.dataBranchNodeConnections->CompSets(Count).CName);
            ShowContinueError(state,
                              "   Nodes, inlet: " + state.dataBranchNodeConnections->CompSets(Count).InletNodeName +
                                  ", outlet: " + state.dataBranchNodeConnections->CompSets(Count).OutletNodeName);
            ShowContinueError(state,
                              " & Nodes, inlet: " + state.dataBranchNodeConnections->CompSets(Other).InletNodeName +
                                  ", outlet: " + state.dataBranchNodeConnections->CompSets(Other).OutletNodeName);
            ShowContinueError(state,
                              "   Node Types:   " + state.dataBranchNodeConnections->CompSets(Count).Description + " & " +
                                  state.dataBranchNodeConnections->CompSets(Other).Description);
            ErrorsFound = true;
        }
    }

    AlreadyNoted.deallocate();
}

void GetNodeConnectionType(EnergyPlusData &state, int const NodeNumber, Array1D_int &NodeConnectType, bool &errFlag)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Lixing Gu
    //       DATE WRITTEN   Jan 2007
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function provides a connection type with given node number

    // FUNCTION LOCAL VARIABLE DECLARATIONS:
    int NodeConnectIndex;
    int NumInList;
    Array1D_int ListArray;
    Array1D_string ConnectionTypes(15);

    for (int nodetype = 1; nodetype <= NumValidConnectionTypes; ++nodetype) {
        ConnectionTypes(nodetype) = ValidConnectionTypes(static_cast<DataLoopNode::NodeConnectionType>(nodetype));
    }

    if (allocated(NodeConnectType)) NodeConnectType.deallocate();

    FindAllNodeNumbersInList(
        NodeNumber, state.dataBranchNodeConnections->NodeConnections, state.dataBranchNodeConnections->NumOfNodeConnections, NumInList, ListArray);

    NodeConnectType.allocate(NumInList);

    if (NumInList > 0) {
        for (NodeConnectIndex = 1; NodeConnectIndex <= NumInList; ++NodeConnectIndex) {
            NodeConnectType(NodeConnectIndex) =
                UtilityRoutines::FindItemInList(state.dataBranchNodeConnections->NodeConnections(ListArray(NodeConnectIndex)).ConnectionType,
                                                ConnectionTypes,
                                                NumValidConnectionTypes);
        }
    } else {
        if (NodeNumber > 0) {
            ShowWarningError(state, "Node not found = " + state.dataLoopNodes->NodeID(NodeNumber) + '.');
        } else {
            ShowWarningError(state, "Invalid node number passed = 0.");
        }
        errFlag = true;
    }
}

void FindAllNodeNumbersInList(int const WhichNumber,
                              Array1D<DataBranchNodeConnections::NodeConnectionDef> const &NodeConnections,
                              int const NumItems,
                              int &CountOfItems,            // Number of items found
                              Array1D_int &AllNumbersInList // Index array to all numbers found
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         R. Raustad
    //       DATE WRITTEN   January 2007
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function looks up a number(integer) in a similar list of
    // items and returns the index of the item in the list, if
    // found.

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int Count; // Counter for DO loops

    CountOfItems = 0;

    if (allocated(AllNumbersInList)) AllNumbersInList.deallocate();

    for (Count = 1; Count <= NumItems; ++Count) {
        if (WhichNumber == NodeConnections(Count).NodeNumber) {
            ++CountOfItems;
        }
    }

    if (CountOfItems > 0) {

        AllNumbersInList.dimension(CountOfItems, 0);
        CountOfItems = 0;

        for (Count = 1; Count <= NumItems; ++Count) {
            if (WhichNumber == NodeConnections(Count).NodeNumber) {
                ++CountOfItems;
                AllNumbersInList(CountOfItems) = Count;
            }
        }
    }
}

} // namespace EnergyPlus::BranchNodeConnections
