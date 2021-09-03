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

#ifndef NodeInputManager_hh_INCLUDED
#define NodeInputManager_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

namespace NodeInputManager {

    // Using/Aliasing
    using DataLoopNode::MarkedNodeData;
    using DataLoopNode::NodeData;

    // For GetOnlySingleNode(), GetNodeNums(), etc
    enum class compFluidStream
    {
        Unassigned = -1,
        Primary = 1,
        Secondary = 2,
        Tertiary = 3
    };

    struct NodeListDef // Derived Type for Node Lists
    {
        // Members
        std::string Name;         // Name of this Node List
        int NumOfNodesInList;     // Number of Nodes in this Node List
        Array1D_string NodeNames; // List of Names in this Node List
        Array1D_int NodeNumbers;  // Number of each Node (ref NodeNames) in this Node List

        // Default Constructor
        NodeListDef() : NumOfNodesInList(0)
        {
        }
    };

    void GetNodeNums(EnergyPlusData &state,
                     std::string const &Name,                                   // Name for which to obtain information
                     int &NumNodes,                                             // Number of nodes accompanying this Name
                     Array1D_int &NodeNumbers,                                  // Node Numbers accompanying this Name
                     bool &ErrorsFound,                                         // True when errors are found...
                     DataLoopNode::NodeFluidType const NodeFluidType,           // Fluidtype for checking/setting node FluidType
                     std::string const &NodeObjectType,                         // Node Object Type (i.e. "Chiller:Electric")
                     std::string const &NodeObjectName,                         // Node Object Name (i.e. "MyChiller")
                     DataLoopNode::NodeConnectionType const NodeConnectionType, // Node Connection Type (see DataLoopNode)
                     compFluidStream const NodeFluidStream,                     // Which Fluid Stream (1,2,3,...)
                     bool const ObjectIsParent,                                 // True/False
                     Optional_bool_const IncrementFluidStream = _,              // True/False
                     Optional_string_const InputFieldName = _                   // Input Field Name
    );

    void SetupNodeVarsForReporting(EnergyPlusData &state);

    void GetNodeListsInput(EnergyPlusData &state, bool &ErrorsFound); // Set to true when requested Node List not found, unchanged otherwise

    int AssignNodeNumber(EnergyPlusData &state,
                         std::string const &Name,                         // Name for assignment
                         DataLoopNode::NodeFluidType const NodeFluidType, // must be valid
                         bool &ErrorsFound);

    int GetOnlySingleNode(EnergyPlusData &state,
                          std::string const &NodeName,
                          bool &errFlag,
                          std::string const &NodeObjectType,                         // Node Object Type (i.e. "Chiller:Electric")
                          std::string const &NodeObjectName,                         // Node Object Name (i.e. "MyChiller")
                          DataLoopNode::NodeFluidType const NodeFluidType,           // Fluidtype for checking/setting node FluidType
                          DataLoopNode::NodeConnectionType const NodeConnectionType, // Node Connection Type (see DataLoopNode)
                          compFluidStream const NodeFluidStream,                     // Which Fluid Stream (1,2,3,...)
                          bool const ObjectIsParent,                                 // True/False
                          Optional_string_const InputFieldName = _                   // Input Field Name
    );

    void InitUniqueNodeCheck(EnergyPlusData &state, std::string const &ContextName);

    void CheckUniqueNodes(EnergyPlusData &state,
                          std::string const &NodeTypes,
                          std::string const &CheckType,
                          bool &ErrorsFound,
                          Optional_string_const CheckName = _,
                          Optional_int_const CheckNumber = _,
                          Optional_string_const ObjectName = _);

    void EndUniqueNodeCheck(EnergyPlusData &state, std::string const &ContextName);

    void CalcMoreNodeInfo(EnergyPlusData &state);

    void MarkNode(EnergyPlusData &state,
                  int const NodeNumber, // Node Number to be marked
                  std::string const &ObjectType,
                  std::string const &ObjectName,
                  std::string const &FieldName);

    void CheckMarkedNodes(EnergyPlusData &state, bool &ErrorsFound);

} // namespace NodeInputManager

struct NodeInputManagerData : BaseGlobalStruct
{

    int NumOfNodeLists = 0;       // Total number of Node Lists in IDF
    int NumOfUniqueNodeNames = 0; // Number of Unique Node Names (current)
    // The following is a module level flag because there are several possible "entries" into
    // this module that may need to get the Node Inputs.
    bool GetNodeInputFlag = true;    // Flag to Get Node Input(s)
    Array1D_int NodeRef;             // Number of times a Node is "referenced"
    std::string CurCheckContextName; // Used in Uniqueness checks
    Array1D_string UniqueNodeNames;  // used in uniqueness checks
    int NumCheckNodes = 0;           // Num of Unique nodes in check
    int MaxCheckNodes = 0;           // Current "max" unique nodes in check
    bool NodeVarsSetup = false;      // Setup indicator of node vars for reporting (also that all nodes have been entered)
    Array1D_bool NodeWetBulbRepReq;
    bool CalcMoreNodeInfoMyOneTimeFlag = true; // one time flag
    Array1D_int GetOnlySingleNodeNodeNums;
    bool GetOnlySingleNodeFirstTime = true;

    // Object Data
    Array1D<NodeInputManager::NodeListDef> NodeLists; // Node Lists

    Real64 RhoAirStdInit;
    Real64 RhoWaterStdInit;
    Array1D_int NodeWetBulbSchedPtr;
    Array1D_bool NodeRelHumidityRepReq;
    Array1D_int NodeRelHumiditySchedPtr;
    Array1D_bool NodeDewPointRepReq;
    Array1D_int NodeDewPointSchedPtr;
    Array1D_bool NodeSpecificHeatRepReq;
    Array1D_int NodeSpecificHeatSchedPtr;
    std::vector<std::string> nodeReportingStrings;
    std::vector<std::string> nodeFluidNames;

    void clear_state() override
    {
        this->CalcMoreNodeInfoMyOneTimeFlag = true;
        this->NumOfNodeLists = 0;
        this->NumOfUniqueNodeNames = 0;
        this->GetNodeInputFlag = true;
        this->NodeRef.deallocate();
        this->CurCheckContextName = std::string();
        this->UniqueNodeNames.deallocate();
        this->NumCheckNodes = 0;
        this->MaxCheckNodes = 0;
        this->NodeVarsSetup = false;
        this->NodeLists.deallocate();
        this->GetOnlySingleNodeNodeNums.deallocate();
        this->GetOnlySingleNodeFirstTime = true;
        this->NodeWetBulbRepReq.deallocate();
    }
};

} // namespace EnergyPlus

#endif
