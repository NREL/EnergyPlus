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

#ifndef DataLoopNode_hh_INCLUDED
#define DataLoopNode_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

namespace DataLoopNode {

    // Using/Aliasing

    // Data
    // MODULE PARAMETER DEFINITIONS:

    enum class NodeFluidType
    {
        blank,
        Air,
        Water,
        Steam,
        Electric
    };

    enum class NodeConnectionType
    {
        blank,
        Inlet,
        Outlet,
        Internal,
        ZoneNode,
        Sensor,
        Actuator,
        OutsideAir,
        ReliefAir,
        ZoneInlet,
        ZoneReturn,
        ZoneExhaust,
        SetPoint,
        Electric,
        OutsideAirReference,
        InducedAir
    };

    constexpr int NumValidConnectionTypes(15);

    constexpr Real64 SensedLoadFlagValue(-999.0);
    constexpr Real64 SensedNodeFlagValue(-999.0);

    // Valid IsParent Types for Node Connections
    constexpr bool ObjectIsParent(true);
    constexpr bool ObjectIsNotParent(false);
    constexpr bool IncrementFluidStreamYes(true);

    constexpr const char *ValidNodeFluidTypes(NodeFluidType const NodeFluidType) // Valid Fluid Types for Nodes
    {
        switch (NodeFluidType) {
        case NodeFluidType::blank:
            return "blank";

        case NodeFluidType::Air:
            return "Air";

        case NodeFluidType::Water:
            return "Water";

        case NodeFluidType::Steam:
            return "Steam";

        case NodeFluidType::Electric:
            return "Electric";

        default:
            return "blank";
        }
    }

    constexpr const char *ValidConnectionTypes(NodeConnectionType const NodeConnectionType) // Valid Connection Types for Nodes
    {

        switch (NodeConnectionType) {
        case NodeConnectionType::Inlet:
            return "Inlet";

        case NodeConnectionType::Outlet:
            return "Outlet";

        case NodeConnectionType::Internal:
            return "Internal";

        case NodeConnectionType::ZoneNode:
            return "ZoneNode";

        case NodeConnectionType::Sensor:
            return "Sensor";

        case NodeConnectionType::Actuator:
            return "Actuator";

        case NodeConnectionType::OutsideAir:
            return "OutdoorAir";

        case NodeConnectionType::ReliefAir:
            return "ReliefAir";

        case NodeConnectionType::ZoneInlet:
            return "ZoneInlet";

        case NodeConnectionType::ZoneReturn:
            return "ZoneReturn";

        case NodeConnectionType::ZoneExhaust:
            return "ZoneExhaust";

        case NodeConnectionType::SetPoint:
            return "Setpoint";

        case NodeConnectionType::Electric:
            return "Electric";

        case NodeConnectionType::OutsideAirReference:
            return "OutsideAirReference";

        case NodeConnectionType::InducedAir:
            return "InducedAir";

        default:
            return "blank";
        }
    }

    // Types
    struct NodeData
    {
        // Members
        NodeFluidType FluidType;     // must be one of the valid parameters
        int FluidIndex;              // For Fluid Properties
        Real64 Temp;                 // {C}
        Real64 TempMin;              // {C}
        Real64 TempMax;              // {C}
        Real64 TempSetPoint;         // {C}
        Real64 TempLastTimestep;     // [C}
        Real64 MassFlowRateRequest;  // {kg/s}
        Real64 MassFlowRate;         // {kg/s}
        Real64 MassFlowRateMin;      // {kg/s}
        Real64 MassFlowRateMax;      // {kg/s}
        Real64 MassFlowRateMinAvail; // {kg/s}
        Real64 MassFlowRateMaxAvail; // {kg/s}
        Real64 MassFlowRateSetPoint; // {kg/s}
        Real64 Quality;              // {0.0-1.0 vapor fraction/percent}
        Real64 Press;                // {Pa}
        Real64 Enthalpy;             // {J/kg}
        Real64 EnthalpyLastTimestep; // {J/kg}
        Real64 HumRat;               // {}
        Real64 HumRatMin;            // {}
        Real64 HumRatMax;            // {}
        Real64 HumRatSetPoint;       // {}
        Real64 TempSetPointHi;       // {C}
        Real64 TempSetPointLo;       // {C}
        Real64 Height;               // {m}

        //  Following are for Outdoor Air Nodes Scheduled Properties
        bool IsLocalNode;
        int OutAirDryBulbSchedNum;
        int OutAirWetBulbSchedNum;
        int OutAirWindSpeedSchedNum;
        int OutAirWindDirSchedNum;

        //  Following are for Outdoor Air Nodes "read only"
        Real64 OutAirDryBulb;              // {C}
        bool EMSOverrideOutAirDryBulb;     // if true, the EMS is calling to override outdoor air node drybulb setting
        Real64 EMSValueForOutAirDryBulb;   // value EMS is directing to use for outdoor air node's drybulb {C}
        Real64 OutAirWetBulb;              // {C}
        bool EMSOverrideOutAirWetBulb;     // if true, the EMS is calling to override outdoor air node wetbulb setting
        Real64 EMSValueForOutAirWetBulb;   // value EMS is directing to use for outdoor air node's wetbulb {C}
        Real64 OutAirWindSpeed;            // {m/s}
        bool EMSOverrideOutAirWindSpeed;   // if true, the EMS is calling to override outdoor air node wind speed setting
        Real64 EMSValueForOutAirWindSpeed; // value EMS is directing to use for outdoor air node's drybulb {m/s}
        Real64 OutAirWindDir;              // {degree}
        bool EMSOverrideOutAirWindDir;     // if true, the EMS is calling to override outdoor air node wind direction setting
        Real64 EMSValueForOutAirWindDir;   // value EMS is directing to use for outdoor air node's wind directio {degree}
        // Contaminant
        Real64 CO2;                // {ppm}
        Real64 CO2SetPoint;        // {ppm}
        Real64 GenContam;          // {ppm}
        Real64 GenContamSetPoint;  // {ppm}
        bool SPMNodeWetBulbRepReq; // Set to true when node has SPM which follows wetbulb

        // error message flag
        bool plantNodeErrorMsgIssued;

        // Default Constructor
        NodeData()
            : FluidType(NodeFluidType::blank), FluidIndex(0), Temp(0.0), TempMin(0.0), TempMax(0.0), TempSetPoint(SensedNodeFlagValue),
              TempLastTimestep(0.0), MassFlowRateRequest(0.0), MassFlowRate(0.0), MassFlowRateMin(0.0), MassFlowRateMax(SensedNodeFlagValue),
              MassFlowRateMinAvail(0.0), MassFlowRateMaxAvail(0.0), MassFlowRateSetPoint(0.0), Quality(0.0), Press(0.0), Enthalpy(0.0),
              EnthalpyLastTimestep(0.0), HumRat(0.0), HumRatMin(SensedNodeFlagValue), HumRatMax(SensedNodeFlagValue),
              HumRatSetPoint(SensedNodeFlagValue), TempSetPointHi(SensedNodeFlagValue), TempSetPointLo(SensedNodeFlagValue), Height(-1.0),
              IsLocalNode(false), OutAirDryBulbSchedNum(0), OutAirWetBulbSchedNum(0), OutAirWindSpeedSchedNum(0), OutAirWindDirSchedNum(0),
              OutAirDryBulb(0.0), EMSOverrideOutAirDryBulb(false), EMSValueForOutAirDryBulb(0.0), OutAirWetBulb(0.0), EMSOverrideOutAirWetBulb(false),
              EMSValueForOutAirWetBulb(0.0), OutAirWindSpeed(0.0), EMSOverrideOutAirWindSpeed(false), EMSValueForOutAirWindSpeed(0.0),
              OutAirWindDir(0.0), EMSOverrideOutAirWindDir(false), EMSValueForOutAirWindDir(0.0), CO2(0.0), CO2SetPoint(0.0), GenContam(0.0),
              GenContamSetPoint(0.0), SPMNodeWetBulbRepReq(false), plantNodeErrorMsgIssued(false)
        {
        }

        // Member Constructor
        NodeData(NodeFluidType const FluidType,     // must be one of the valid parameters
                 int const FluidIndex,              // For Fluid Properties
                 Real64 const Temp,                 // {C}
                 Real64 const TempMin,              // {C}
                 Real64 const TempMax,              // {C}
                 Real64 const TempSetPoint,         // {C}
                 Real64 const TempLastTimestep,     // [C}
                 Real64 const MassFlowRateRequest,  // {kg/s}
                 Real64 const MassFlowRate,         // {kg/s}
                 Real64 const MassFlowRateMin,      // {kg/s}
                 Real64 const MassFlowRateMax,      // {kg/s}
                 Real64 const MassFlowRateMinAvail, // {kg/s}
                 Real64 const MassFlowRateMaxAvail, // {kg/s}
                 Real64 const MassFlowRateSetPoint, // {kg/s}
                 Real64 const Quality,              // {0.0-1.0 vapor fraction/percent}
                 Real64 const Press,                // {Pa}
                 Real64 const Enthalpy,             // {J/kg}
                 Real64 const EnthalpyLastTimestep, // {J/kg}
                 Real64 const HumRat,               // {}
                 Real64 const HumRatMin,            // {}
                 Real64 const HumRatMax,            // {}
                 Real64 const HumRatSetPoint,       // {}
                 Real64 const TempSetPointHi,       // {C}
                 Real64 const TempSetPointLo,       // {C}
                 Real64 const Height,               // {m}
                 bool const IsLocalNode,
                 int const OutAirDryBulbSchedNum,         // schedule value in {C}
                 int const OutAirWetBulbSchedNum,         // schedule value in {C}
                 int const OutAirWindSpeedSchedNum,       // schedule value in {m/s}
                 int const OutAirWindDirSchedNum,         // schedule value in {degree}
                 Real64 const OutAirDryBulb,              // {C}
                 bool const EMSOverrideOutAirDryBulb,     // if true, the EMS is calling to override outdoor air node drybulb setting
                 Real64 const EMSValueForOutAirDryBulb,   // value EMS is directing to use for outdoor air node's drybulb {C}
                 Real64 const OutAirWetBulb,              // {C}
                 bool const EMSOverrideOutAirWetBulb,     // if true, the EMS is calling to override outdoor air node wetbulb setting
                 Real64 const EMSValueForOutAirWetBulb,   // value EMS is directing to use for outdoor air node's wetbulb {C}
                 Real64 const OutAirWindSpeed,            // {m/s}
                 bool const EMSOverrideOutAirWindSpeed,   // if true, the EMS is calling to override outdoor air node wind speed setting
                 Real64 const EMSValueForOutAirWindSpeed, // value EMS is directing to use for outdoor air node's drybulb {m/s}
                 Real64 const OutAirWindDir,              // {degree}
                 bool const EMSOverrideOutAirWindDir,     // if true, the EMS is calling to override outdoor air node wind direction setting
                 Real64 const EMSValueForOutAirWindDir,   // value EMS is directing to use for outdoor air node's wind directio {degree}
                 Real64 const CO2,                        // {ppm}
                 Real64 const CO2SetPoint,                // {ppm}
                 Real64 const GenContam,                  // {ppm}
                 Real64 const GenContamSetPoint,          // {ppm}
                 bool const SPMNodeWetBulbRepReq,         // Set to true when node has SPM which follows wetbulb
                 bool const plantNodeErrorMsgIssued)
            : FluidType(FluidType), FluidIndex(FluidIndex), Temp(Temp), TempMin(TempMin), TempMax(TempMax), TempSetPoint(TempSetPoint),
              TempLastTimestep(TempLastTimestep), MassFlowRateRequest(MassFlowRateRequest), MassFlowRate(MassFlowRate),
              MassFlowRateMin(MassFlowRateMin), MassFlowRateMax(MassFlowRateMax), MassFlowRateMinAvail(MassFlowRateMinAvail),
              MassFlowRateMaxAvail(MassFlowRateMaxAvail), MassFlowRateSetPoint(MassFlowRateSetPoint), Quality(Quality), Press(Press),
              Enthalpy(Enthalpy), EnthalpyLastTimestep(EnthalpyLastTimestep), HumRat(HumRat), HumRatMin(HumRatMin), HumRatMax(HumRatMax),
              HumRatSetPoint(HumRatSetPoint), TempSetPointHi(TempSetPointHi), TempSetPointLo(TempSetPointLo), Height(Height),
              IsLocalNode(IsLocalNode), OutAirDryBulbSchedNum(OutAirDryBulbSchedNum), OutAirWetBulbSchedNum(OutAirWetBulbSchedNum),
              OutAirWindSpeedSchedNum(OutAirWindSpeedSchedNum), OutAirWindDirSchedNum(OutAirWindDirSchedNum), OutAirDryBulb(OutAirDryBulb),
              EMSOverrideOutAirDryBulb(EMSOverrideOutAirDryBulb), EMSValueForOutAirDryBulb(EMSValueForOutAirDryBulb), OutAirWetBulb(OutAirWetBulb),
              EMSOverrideOutAirWetBulb(EMSOverrideOutAirWetBulb), EMSValueForOutAirWetBulb(EMSValueForOutAirWetBulb),
              OutAirWindSpeed(OutAirWindSpeed), EMSOverrideOutAirWindSpeed(EMSOverrideOutAirWindSpeed),
              EMSValueForOutAirWindSpeed(EMSValueForOutAirWindSpeed), OutAirWindDir(OutAirWindDir),
              EMSOverrideOutAirWindDir(EMSOverrideOutAirWindDir), EMSValueForOutAirWindDir(EMSValueForOutAirWindDir), CO2(CO2),
              CO2SetPoint(CO2SetPoint), GenContam(GenContam), GenContamSetPoint(GenContamSetPoint), SPMNodeWetBulbRepReq(SPMNodeWetBulbRepReq),
              plantNodeErrorMsgIssued(plantNodeErrorMsgIssued)
        {
        }
    };

    struct MoreNodeData
    {
        // Members
        Real64 RelHumidity;        // {%}
        Real64 ReportEnthalpy;     // specific enthalpy calculated at the HVAC timestep [J/kg]
        Real64 VolFlowRateStdRho;  // volume flow rate at standard density [m3/s]
        Real64 VolFlowRateCrntRho; // volume flow rate at current density, only used for air nodes [m3/s]
        Real64 WetBulbTemp;        // wetbulb temperature [C]
        Real64 Density;            // reported density at current temperature [kg/m3]
        Real64 AirDewPointTemp;    // reported system node dewpoint temperature [C]
        Real64 SpecificHeat;       // reported node specific heat [J/kg-C]

        // Default Constructor
        MoreNodeData()
            : RelHumidity(0.0), ReportEnthalpy(0.0), VolFlowRateStdRho(0.0), VolFlowRateCrntRho(0.0), WetBulbTemp(0.0), Density(0.0),
              AirDewPointTemp(0.0), SpecificHeat(0.0)
        {
        }
    };

    struct MarkedNodeData
    {
        // Members
        bool IsMarked;          // true if this is a marked node
        std::string ObjectType; // Object Type that needs it "marked"
        std::string ObjectName; // Object Name that needs it "marked"
        std::string FieldName;  // FieldName that needs it "marked"

        // Default Constructor
        MarkedNodeData() : IsMarked(false)
        {
        }
    };

    // A struct to defer checking whether a node did correctly get a setpoint via the API / PythonPlugin
    struct NodeSetpointCheckData
    {
        bool needsSetpointChecking;
        bool checkTemperatureSetPoint;
        bool checkTemperatureMinSetPoint;
        bool checkTemperatureMaxSetPoint;
        bool checkHumidityRatioSetPoint;
        bool checkHumidityRatioMinSetPoint;
        bool checkHumidityRatioMaxSetPoint;
        bool checkMassFlowRateSetPoint;
        bool checkMassFlowRateMinSetPoint;
        bool checkMassFlowRateMaxSetPoint;

        NodeSetpointCheckData()
            : needsSetpointChecking(false), checkTemperatureSetPoint(false), checkTemperatureMinSetPoint(false), checkTemperatureMaxSetPoint(false),
              checkHumidityRatioSetPoint(false), checkHumidityRatioMinSetPoint(false), checkHumidityRatioMaxSetPoint(false),
              checkMassFlowRateSetPoint(false), checkMassFlowRateMinSetPoint(false), checkMassFlowRateMaxSetPoint(false)
        {
        }
    };
} // namespace DataLoopNode

struct LoopNodeData : BaseGlobalStruct
{

    int NumOfNodes = 0;
    int NumofSplitters = 0;
    int NumofMixers = 0;
    Array1D_string NodeID;
    Array1D<DataLoopNode::NodeData> Node; // dim to num nodes in SimHVAC
    DataLoopNode::NodeData DefaultNodeValues = {
        DataLoopNode::NodeFluidType::blank,
        0,
        0.0,
        0.0,
        0.0,
        DataLoopNode::SensedNodeFlagValue,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        DataLoopNode::SensedNodeFlagValue,
        DataLoopNode::SensedNodeFlagValue,
        DataLoopNode::SensedNodeFlagValue,
        DataLoopNode::SensedNodeFlagValue,
        DataLoopNode::SensedNodeFlagValue,
        -1.0,
        false,
        0,
        0,
        0,
        0,
        0.0,
        false,
        0.0,
        0.0,
        false,
        0.0,
        0.0,
        false,
        0.0,
        0.0,
        false,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        false,
        false}; // Autodesk:Note If intent is default construction drop initializer to elim bug exposure | FluidType |
                // FluidIndex | Temp {C} | TempMin {C} | TempMax {C} | TempSetPoint {C} | TempLastTimeStep {C} |
                // MassFlowRateRequest {kg/s} | MassFlowRate {kg/s} | MassFlowRateMin {kg/s} | MassFlowRateMax {kg/s}
                // //Autodesk:Note SensedNodeFlagValue is default initializer | MassFlowRateMinAvail {kg/s} |
                // MassFlowRateMaxAvail {kg/s} | MassFlowRateSetPoint {kg/s} | Quality {0.0-1.0 vapor fraction/percent} | Press
                // {Pa}   REAL(r64)     :: | Enthalpy {J/kg} | EnthalpyLastTimeStep {J/kg} | HumRat {} | HumRatMin {} |
                // HumRatMax {} | HumRatSetPoint {} | TempSetPointHi {C} | TempSetPointLo {C} | Height {m} | OutAirDryBulb {C}
                // | EMSOverrideOutAirDryBulb | EMSValueForOutAirDryBulb {C} | OutAirWetBulb {C} | EMSOverrideOutAirWetBulb |
                // EMSValueForOutAirWetBulb {C} | CO2 {ppm} | CO2 setpoint {ppm} | Generic contaminant {ppm} | Generic
                // contaminant setpoint {ppm} | Set to true when node has SPM which follows wetbulb
    Array1D<DataLoopNode::MoreNodeData> MoreNodeInfo;
    Array1D<DataLoopNode::MarkedNodeData> MarkedNode;
    Array1D<DataLoopNode::NodeSetpointCheckData> NodeSetpointCheck;

    void clear_state() override
    {
        this->NumOfNodes = 0;
        this->NumofSplitters = 0;
        this->NumofMixers = 0;
        this->NodeID.deallocate();
        this->Node.deallocate();
        this->DefaultNodeValues = DataLoopNode::NodeData(DataLoopNode::NodeFluidType::blank,
                                                         0,
                                                         0.0,
                                                         0.0,
                                                         0.0,
                                                         DataLoopNode::SensedNodeFlagValue,
                                                         0.0,
                                                         0.0,
                                                         0.0,
                                                         0.0,
                                                         0.0,
                                                         0.0,
                                                         0.0,
                                                         0.0,
                                                         0.0,
                                                         0.0,
                                                         0.0,
                                                         0.0,
                                                         0.0,
                                                         DataLoopNode::SensedNodeFlagValue,
                                                         DataLoopNode::SensedNodeFlagValue,
                                                         DataLoopNode::SensedNodeFlagValue,
                                                         DataLoopNode::SensedNodeFlagValue,
                                                         DataLoopNode::SensedNodeFlagValue,
                                                         -1.0,
                                                         false,
                                                         0,
                                                         0,
                                                         0,
                                                         0,
                                                         0.0,
                                                         false,
                                                         0.0,
                                                         0.0,
                                                         false,
                                                         0.0,
                                                         0.0,
                                                         false,
                                                         0.0,
                                                         0.0,
                                                         false,
                                                         0.0,
                                                         0.0,
                                                         0.0,
                                                         0.0,
                                                         0.0,
                                                         false,
                                                         false);
        this->MoreNodeInfo.deallocate();
        this->MarkedNode.deallocate();
        this->NodeSetpointCheck.deallocate();
    }
};

} // namespace EnergyPlus

#endif
