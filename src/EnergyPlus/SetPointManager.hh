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

#ifndef SetPointManager_hh_INCLUDED
#define SetPointManager_hh_INCLUDED

#include <map>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/EnergyPlus.hh>
#include <EnergyPlus/Plant/Enums.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

namespace SetPointManager {

    enum class CtrlNodeType : int
    {
        control,
        reference
    };

    enum class SupplyFlowTempStrategy
    {
        MaxTemp,
        MinTemp,
        Unknown
    };
    enum class ControlStrategy
    {
        TempFirst,
        FlowFirst,
        Unknown
    };
    enum class ReferenceTempType
    {
        WetBulb,
        DryBulb,
        Unknown
    };
    enum class ReferenceGroundTempObjectType
    {
        BuildingSurface,
        Shallow,
        Deep,
        FCFactorMethod,
        Unknown
    };

    enum class iCtrlVarType
    {
        Unknown,
        Temp,
        MaxTemp,
        MinTemp,
        HumRat,
        MaxHumRat,
        MinHumRat,
        MassFlow,
        MaxMassFlow,
        MinMassFlow
    };
    int constexpr NumValidCtrlTypes = 9;
    inline const char *controlTypeName(iCtrlVarType cvt)
    {
        switch (cvt) {
        case iCtrlVarType::Temp:
            return "Temperature";
        case iCtrlVarType::MaxTemp:
            return "MaximumTemperature";
        case iCtrlVarType::MinTemp:
            return "MinimumTemperature";
        case iCtrlVarType::HumRat:
            return "HumidityRatio";
        case iCtrlVarType::MaxHumRat:
            return "MaximumHumidityRatio";
        case iCtrlVarType::MinHumRat:
            return "MinimumHumidityRatio";
        case iCtrlVarType::MassFlow:
            return "MassFlowRate";
        case iCtrlVarType::MaxMassFlow:
            return "MaximumMassFlowRate";
        case iCtrlVarType::MinMassFlow:
            return "MinimumMassFlowRate";
        case iCtrlVarType::Unknown:
            return "*UNKNOWN*";
        }
        return "*UNKNOWN*"; // not sure how we would get here, the switch block cases are exhaustive
    }

    enum class SetPointManagerType
    {
        Scheduled,
        ScheduledDual,
        OutsideAir,
        SZReheat,
        SZHeating,
        SZCooling,
        SZMinHum,
        SZMaxHum,
        MixedAir,
        OutsideAirPretreat,
        Warmest,
        Coldest,
        WarmestTempFlow,
        RAB,
        MZCoolingAverage,
        MZHeatingAverage,
        MZMinHumAverage,
        MZMaxHumAverage,
        MZMinHum,
        MZMaxHum,
        FollowOATemp,
        FollowSysNodeTemp,
        GroundTemp,
        CondEntReset,
        IdealCondEntReset,
        SZOneStageCooling,
        SZOneStageHeating,
        ReturnWaterResetChW,
        ReturnWaterResetHW,
        TESScheduled,
        Unknown
    };
    int constexpr NumValidSPMTypes = 30;
    inline const char *managerTypeName(SetPointManagerType t)
    {
        switch (t) {
        case SetPointManagerType::Scheduled:
            return "SetpointManager:Scheduled";
        case SetPointManagerType::ScheduledDual:
            return "SetpointManager:Scheduled:DualSetpoint";
        case SetPointManagerType::OutsideAir:
            return "SetpointManager:OutdoorAirReset";
        case SetPointManagerType::SZReheat:
            return "SetpointManager:SingleZone:Reheat";
        case SetPointManagerType::SZHeating:
            return "SetpointManager:SingleZone:Heating";
        case SetPointManagerType::SZCooling:
            return "SetpointManager:SingleZone:Cooling";
        case SetPointManagerType::SZMinHum:
            return "SetpointManager:SingleZone:Humidity:Minimum";
        case SetPointManagerType::SZMaxHum:
            return "SetpointManager:SingleZone:Humidity:Maximum";
        case SetPointManagerType::MixedAir:
            return "SetpointManager:MixedAir";
        case SetPointManagerType::OutsideAirPretreat:
            return "SetpointManager:OutdoorAirPretreat";
        case SetPointManagerType::Warmest:
            return "SetpointManager:Warmest";
        case SetPointManagerType::Coldest:
            return "SetpointManager:Coldest";
        case SetPointManagerType::WarmestTempFlow:
            return "SetpointManager:WarmestTemperatureFlow";
        case SetPointManagerType::RAB:
            return "SetpointManager:ReturnAirBypassFlow";
        case SetPointManagerType::MZCoolingAverage:
            return "SetpointManager:MultiZone:Cooling:Average";
        case SetPointManagerType::MZHeatingAverage:
            return "SetpointManager:MultiZone:Heating:Average";
        case SetPointManagerType::MZMinHumAverage:
            return "SetpointManager:MultiZone:MinimumHumidity:Average";
        case SetPointManagerType::MZMaxHumAverage:
            return "SetpointManager:MultiZone:MaximumHumidity:Average";
        case SetPointManagerType::MZMinHum:
            return "SetpointManager:MultiZone:Humidity:Minimum";
        case SetPointManagerType::MZMaxHum:
            return "SetpointManager:MultiZone:Humidity:Maximum";
        case SetPointManagerType::FollowOATemp:
            return "SetpointManager:FollowOutdoorAirTemperature";
        case SetPointManagerType::FollowSysNodeTemp:
            return "SetpointManager:FollowSystemNodeTemperature";
        case SetPointManagerType::GroundTemp:
            return "SetpointManager:FollowGroundTemperature";
        case SetPointManagerType::CondEntReset:
            return "SetpointManager:CondenserEnteringReset";
        case SetPointManagerType::IdealCondEntReset:
            return "SetpointManager:CondenserEnteringReset:Ideal";
        case SetPointManagerType::SZOneStageCooling:
            return "SetpointManager:SingleZone:OneStageCooling";
        case SetPointManagerType::SZOneStageHeating:
            return "SetpointManager:SingleZone:OneStageHeating";
        case SetPointManagerType::ReturnWaterResetChW:
            return "SetpointManager:ReturnTemperature:ChilledWater";
        case SetPointManagerType::ReturnWaterResetHW:
            return "SetpointManager:ReturnTemperature:HotWater";
        case SetPointManagerType::TESScheduled:
            return "SetpointManager:ScheduledTES";
        case SetPointManagerType::Unknown:
            return "*UNKNOWN*";
        }
        return "*UNKNOWN*"; // not sure how we would get here, the switch block cases are exhaustive
    }

    struct SPBase
    {
        std::string Name;
        iCtrlVarType CtrlTypeMode; // set to iCtrlVarType::*
        std::string CtrlVarType;
        SPBase() : CtrlTypeMode(iCtrlVarType::Unknown)
        {
        }
    };

    struct DataSetPointManager : SPBase // Derived type for all Setpoint Managers
    {
        // Members
        SetPointManagerType SPMType; // integer representing type of setpoint manager
        int SPMIndex;                // index to specific set point manager
        int NumCtrlNodes;            // number of control nodes
        Array1D_int CtrlNodes;       // index to control node
        int AirLoopNum;              // index to air loop
        std::string AirLoopName;     // name of air loop
        int RefNode;                 // index to reference node

        // Default Constructor
        DataSetPointManager() : SPMType(SetPointManagerType::Unknown), SPMIndex(0), NumCtrlNodes(0), AirLoopNum(0), RefNode(0)
        {
        }
    };

    struct DefineScheduledSetPointManager : SPBase // Derived type for Scheduled Setpoint Manager data
    {
        // Members
        std::string Sched;
        int SchedPtr;
        int NumCtrlNodes;
        std::string CtrlNodeListName;
        Array1D_int CtrlNodes;
        Real64 SetPt;

        // Default Constructor
        DefineScheduledSetPointManager() : SchedPtr(0), NumCtrlNodes(0), SetPt(0.0)
        {
        }

        void calculate(EnergyPlusData &state);
    };

    struct DefineSchedDualSetPointManager : SPBase // Derived type for Scheduled Dual Setpoint Manager
    {
        // Members
        std::string SchedHi;
        std::string SchedLo;
        int SchedPtrHi;
        int SchedPtrLo;
        int NumCtrlNodes;
        std::string CtrlNodeListName;
        Array1D_int CtrlNodes;
        Real64 SetPtHi;
        Real64 SetPtLo;

        // Default Constructor
        DefineSchedDualSetPointManager() : SchedPtrHi(0), SchedPtrLo(0), NumCtrlNodes(0), SetPtHi(0.0), SetPtLo(0.0)
        {
        }

        void calculate(EnergyPlusData &state);
    };

    struct DefineOutsideAirSetPointManager : SPBase // Derived type for Outside Air Setpoint Manager Data
    {
        // Members
        Real64 OutLowSetPt1;  // 1st setpoint at outside low
        Real64 OutLow1;       // 1st Outside low
        Real64 OutHighSetPt1; // 1st setpoint at outside high
        Real64 OutHigh1;      // 1st Outside high
        std::string Sched;    // Optional schedule
        int SchedPtr;         // Schedule index
        Real64 OutLowSetPt2;  // 2nd setpoint at outside low (optional)
        Real64 OutLow2;       // 2nd Outside low (optional)
        Real64 OutHighSetPt2; // 2nd setpoint at outside high (optional)
        Real64 OutHigh2;      // 2nd Outside high (optional)
        int NumCtrlNodes;
        std::string CtrlNodeListName;
        Array1D_int CtrlNodes;
        Real64 SetPt; // current setpoint value

        // Default Constructor
        DefineOutsideAirSetPointManager()
            : OutLowSetPt1(0.0), OutLow1(0.0), OutHighSetPt1(0.0), OutHigh1(0.0), SchedPtr(0), OutLowSetPt2(0.0), OutLow2(0.0), OutHighSetPt2(0.0),
              OutHigh2(0.0), NumCtrlNodes(0), SetPt(0.0)

        {
        }

        void calculate(EnergyPlusData &state);

        Real64 CalcSetPoint(Real64 OutLowTemp, Real64 OutHighTemp, Real64 OutDryBulbTemp, Real64 SetTempAtOutLow, Real64 SetTempAtOutHigh);
    };

    struct DefineSZReheatSetPointManager : SPBase // Derived type for the Single Zone Reheat Setpoint Manager data
    {
        // Members
        std::string ControlZoneName; // name of the control zone (zone with main thermostat)
        int ControlZoneNum;          // number (index into Zone array) of control zone
        int ZoneNodeNum;             // zone node number
        int ZoneInletNodeNum;        // inlet node number for the SZRH air
        Real64 MinSetTemp;           // minimum supply air setpoint temperature
        Real64 MaxSetTemp;           // maximum supply air setpoint temperature
        int MixedAirNode;            // mixed air node number
        int FanNodeIn;               // fan inlet node number
        int FanNodeOut;              // fan outlet node number
        int AirLoopNum;              // air loop index of air loop associated with this setpoint manager
        int OAInNode;                // outside airstream inlet node to the OA mixer
        int RetNode;                 // return node inlet to OA mixer
        int LoopInNode;              // Primary Air System inlet node
        int NumCtrlNodes;
        Array1D_int CtrlNodes; // node numbers of nodes where setpoint is to be set
        Real64 SetPt;          // the setpoint

        // Default Constructor
        DefineSZReheatSetPointManager()
            : ControlZoneNum(0), ZoneNodeNum(0), ZoneInletNodeNum(0), MinSetTemp(0.0), MaxSetTemp(0.0), MixedAirNode(0), FanNodeIn(0), FanNodeOut(0),
              AirLoopNum(0), OAInNode(0), RetNode(0), LoopInNode(0), NumCtrlNodes(0), SetPt(0.0)
        {
        }

        void calculate(EnergyPlusData &state);
    };

    struct DefineSZHeatingSetPointManager : SPBase // Derived type for the Single Zone Heating Setpoint Manager data
    {
        // Members
        std::string ControlZoneName; // name of the control zone (zone with main thermostat)
        int ControlZoneNum;          // number (index into Zone array) of control zone
        int ZoneNodeNum;             // zone node number
        int ZoneInletNodeNum;        // inlet node number for the supply air
        Real64 MinSetTemp;           // minimum supply air setpoint temperature
        Real64 MaxSetTemp;           // maximum supply air setpoint temperature
        int NumCtrlNodes;
        Array1D_int CtrlNodes; // node numbers of nodes where setpoint is to be set
        Real64 SetPt;          // the setpoint

        // Default Constructor
        DefineSZHeatingSetPointManager()
            : ControlZoneNum(0), ZoneNodeNum(0), ZoneInletNodeNum(0), MinSetTemp(0.0), MaxSetTemp(0.0), NumCtrlNodes(0), SetPt(0.0)
        {
        }

        void calculate(EnergyPlusData &state);
    };

    struct DefineSZCoolingSetPointManager : SPBase // Derived type for the Single Zone Cooling Setpoint Manager data
    {
        // Members
        std::string ControlZoneName; // name of the control zone (zone with main thermostat)
        int ControlZoneNum;          // number (index into Zone array) of control zone
        int ZoneNodeNum;             // zone node number
        int ZoneInletNodeNum;        // inlet node number for the supply air
        Real64 MinSetTemp;           // minimum supply air setpoint temperature
        Real64 MaxSetTemp;           // maximum supply air setpoint temperature
        int NumCtrlNodes;
        Array1D_int CtrlNodes; // node numbers of nodes where setpoint is to be set
        Real64 SetPt;          // the setpoint

        // Default Constructor
        DefineSZCoolingSetPointManager()
            : ControlZoneNum(0), ZoneNodeNum(0), ZoneInletNodeNum(0), MinSetTemp(0.0), MaxSetTemp(0.0), NumCtrlNodes(0), SetPt(0.0)
        {
        }

        void calculate(EnergyPlusData &state);
    };

    struct DefineSZMinHumSetPointManager : SPBase // Derived Type for Single Zone Minimum Humidity Setpoint Manager data
    {
        // Members
        int NumZones;            // number of zones whose humidity is being controlled
        int NumCtrlNodes;        // number of nodes whose humidity ratio is being set
        Array1D_int ZoneNodes;   // zone node numbers of zones being controlled
        Array1D_int ZoneNum;     // actual zone number ( index into Zone array)
        Array1D_int CtrlZoneNum; // index into ZoneEquipConfig
        Array1D_int CtrlNodes;   // nodes where humidity ratio is being set
        Real64 SetPt;            // the setpoint

        // Default Constructor
        DefineSZMinHumSetPointManager() : NumZones(0), NumCtrlNodes(0), SetPt(0.0)
        {
        }

        void calculate(EnergyPlusData &state);
    };

    struct DefineSZMaxHumSetPointManager : SPBase // Derived Type for Single Zone Maximum Humidity Setpoint Manager data
    {
        // Members
        int NumZones;            // number of zones whose humidity is being controlled
        int NumCtrlNodes;        // number of nodes whose humidity ratio is being set
        Array1D_int ZoneNodes;   // zone node numbers of zones being controlled
        Array1D_int ZoneNum;     // actual zone number (index into Zone array)
        Array1D_int CtrlZoneNum; // index into ZoneEquipConfig
        Array1D_int CtrlNodes;   // nodes where humidity ratio is being set
        Real64 SetPt;            // the setpoint

        // Default Constructor
        DefineSZMaxHumSetPointManager() : NumZones(0), NumCtrlNodes(0), SetPt(0.0)
        {
        }

        void calculate(EnergyPlusData &state);
    };

    struct DefineMixedAirSetPointManager : SPBase
    {
        // Members
        int RefNode;               // reference node number
        int FanInNode;             // supply fan inlet node number
        int FanOutNode;            // Supplt fan outlet node number
        int NumCtrlNodes;          // number of nodes whose humidity ratio is being set
        Array1D_int CtrlNodes;     // node numbers of nodes where setpoint is to be set
        Real64 SetPt;              // the setpoint
        bool MySetPointCheckFlag;  // used for mixed air SPM test for missing SP
        bool FreezeCheckEnable;    // Enable freezing check
        int CoolCoilInNode;        // Cooling coil inlet node number
        int CoolCoilOutNode;       // Cooling coil outlet node number
        Real64 MinCoolCoilOutTemp; // The minimum temperature at cooling coil outlet node

        // Default Constructor
        DefineMixedAirSetPointManager()
            : RefNode(0), FanInNode(0), FanOutNode(0), NumCtrlNodes(0), SetPt(0.0), MySetPointCheckFlag(true), FreezeCheckEnable(false),
              CoolCoilInNode(0), CoolCoilOutNode(0), MinCoolCoilOutTemp(7.2)

        {
        }

        void calculate(EnergyPlusData &state);
    };

    struct DefineOAPretreatSetPointManager : SPBase
    {
        // Members
        int RefNode;              // reference node number
        int MixedOutNode;         // mixed air outlet node number
        int OAInNode;             // outside air inlet node number
        int ReturnInNode;         // return air inlet node number
        Real64 MinSetTemp;        // minimum supply air setpoint temperature [C]
        Real64 MaxSetTemp;        // maximum supply air setpoint temperature [C]
        Real64 MinSetHumRat;      // minimum supply air setpoint humidity ratio [kg/kg]
        Real64 MaxSetHumRat;      // maximum supply air setpoint humidity ratio [kg/kg]
        int NumCtrlNodes;         // number of nodes whose humidity ratio is being set
        Array1D_int CtrlNodes;    // node numbers of nodes where setpoint is to be set
        Real64 SetPt;             // the setpoint
        bool MySetPointCheckFlag; // used for DOAS SPM test for missing SP

        // Default Constructor
        DefineOAPretreatSetPointManager()
            : RefNode(0), MixedOutNode(0), OAInNode(0), ReturnInNode(0), MinSetTemp(0.0), MaxSetTemp(0.0), MinSetHumRat(0.0), MaxSetHumRat(0.0),
              NumCtrlNodes(0), SetPt(0.0), MySetPointCheckFlag(true)
        {
        }

        void calculate(EnergyPlusData &state);
    };

    struct DefineWarmestSetPointManager : SPBase
    {
        // Members
        std::string AirLoopName;         // name of air loop that will use "warmest zone" strategy
        int AirLoopNum;                  // index of named air loop
        Real64 MinSetTemp;               // minimum supply air setpoint temperature
        Real64 MaxSetTemp;               // maximum supply air setpoint temperature
        SupplyFlowTempStrategy Strategy; // supply flow and temperature set strategy
        // 1 = MaxTemp
        int NumCtrlNodes;      // number of nodes whose temperature is being set
        Array1D_int CtrlNodes; // nodes where temperature is being set
        Real64 SetPt;          // the setpoint

        // Default Constructor
        DefineWarmestSetPointManager()
            : AirLoopNum(0), MinSetTemp(0.0), MaxSetTemp(0.0), Strategy(SupplyFlowTempStrategy::Unknown), NumCtrlNodes(0), SetPt(0.0)
        {
        }

        void calculate(EnergyPlusData &state);
    };

    struct DefineColdestSetPointManager : SPBase
    {
        // Members
        std::string AirLoopName;         // name of air loop that will use "coldest zone" strategy
        int AirLoopNum;                  // index of named air loop
        Real64 MinSetTemp;               // minimum supply air setpoint temperature
        Real64 MaxSetTemp;               // maximum supply air setpoint temperature
        SupplyFlowTempStrategy Strategy; // supply flow and temperature set strategy
        // 2 = MinTemp
        int NumCtrlNodes;      // number of nodes whose temperature is being set
        Array1D_int CtrlNodes; // nodes where temperature is being set
        Real64 SetPt;          // the setpoint

        // Default Constructor
        DefineColdestSetPointManager()
            : AirLoopNum(0), MinSetTemp(0.0), MaxSetTemp(0.0), Strategy(SupplyFlowTempStrategy::Unknown), NumCtrlNodes(0), SetPt(0.0)
        {
        }

        void calculate(EnergyPlusData &state);
    };

    struct DefWarmestSetPtManagerTempFlow : SPBase
    {
        // Members
        std::string AirLoopName;  // name of air loop that will use "warmest zone" strategy
        int AirLoopNum;           // index of named air loop
        Real64 MinSetTemp;        // minimum supply air setpoint temperature
        Real64 MaxSetTemp;        // maximum supply air setpoint temperature
        ControlStrategy Strategy; // supply flow and temperature set strategy
        // 1 = TempFirst, 2 = FlowFirst
        int NumCtrlNodes;      // number of nodes whose temperature is being set
        Array1D_int CtrlNodes; // nodes where temperature is being set
        Real64 SetPt;          // the setpoint
        Real64 MinTurndown;    // minimum fractional flow rate
        Real64 Turndown;       // fractional flow rate
        int CritZoneNum;
        bool SimReady;

        // Default Constructor
        DefWarmestSetPtManagerTempFlow()
            : AirLoopNum(0), MinSetTemp(0.0), MaxSetTemp(0.0), Strategy(ControlStrategy::Unknown), NumCtrlNodes(0), SetPt(0.0), MinTurndown(0.0),
              Turndown(0.0), CritZoneNum(0), SimReady(false)
        {
        }

        void calculate(EnergyPlusData &state);
    };

    struct DefRABFlowSetPointManager : SPBase
    {
        // Members
        int NumCtrlNodes;        // number of nodes whose temperature is being set
        Array1D_int CtrlNodes;   // nodes where temperature is being set
        std::string AirLoopName; // name of air loop that will use "warmest zone" strategy
        int AirLoopNum;          // index of named air loop
        std::string Sched;       // name of a schedule of supply air setpoint temperatures
        int SchedPtr;            // index of the above schedule
        Real64 FlowSetPt;        // mass flow rate setpoint (kg/s)
        int RABMixInNode;
        int SupMixInNode;
        int MixOutNode;
        int RABSplitOutNode;
        int SysOutNode;
        int AllSetPtMgrIndex; // index of RAB SP manager in AllSetPtMgr structure

        // Default Constructor
        DefRABFlowSetPointManager()
            : NumCtrlNodes(0), AirLoopNum(0), SchedPtr(0), FlowSetPt(0.0), RABMixInNode(0), SupMixInNode(0), MixOutNode(0), RABSplitOutNode(0),
              SysOutNode(0), AllSetPtMgrIndex(0)
        {
        }

        void calculate(EnergyPlusData &state);
    };

    struct DefMultiZoneAverageCoolingSetPointManager : SPBase // derived type for SetpointManager:Multizone:Cooling:Average data
    {
        // Members
        std::string AirLoopName; // name of air loop that will use "MultiZone:Cooling:Average" strategy
        int AirLoopNum;          // index of named air loop
        Real64 MinSetTemp;       // minimum supply air setpoint temperature [C]
        Real64 MaxSetTemp;       // maximum supply air setpoint temperature [C]
        int NumCtrlNodes;        // number of nodes whose temperature is being set
        Array1D_int CtrlNodes;   // nodes where temperature is being set
        Real64 SetPt;            // the temperature setpoint [C]

        // Default Constructor
        DefMultiZoneAverageCoolingSetPointManager() : AirLoopNum(0), MinSetTemp(0.0), MaxSetTemp(0.0), NumCtrlNodes(0), SetPt(0.0)
        {
        }

        void calculate(EnergyPlusData &state);
    };

    struct DefMultiZoneAverageHeatingSetPointManager : SPBase // derived type for SetpointManager:Multizone:Heating:Average data
    {
        // Members
        std::string AirLoopName; // name of air loop that will use "MultiZone:Heating:Average" strategy
        int AirLoopNum;          // index of named air loop
        Real64 MinSetTemp;       // minimum supply air setpoint temperature [C]
        Real64 MaxSetTemp;       // maximum supply air setpoint temperature [C]
        int NumCtrlNodes;        // number of nodes whose temperature is being set
        Array1D_int CtrlNodes;   // nodes where temperature is being set
        Real64 SetPt;            // the temperature setpoint [C]

        // Default Constructor
        DefMultiZoneAverageHeatingSetPointManager() : AirLoopNum(0), MinSetTemp(0.0), MaxSetTemp(0.0), NumCtrlNodes(0), SetPt(0.0)
        {
        }

        void calculate(EnergyPlusData &state);
    };

    struct DefMultiZoneAverageMinHumSetPointManager : SPBase // derived type for SetpointManager:MultiZone:MinimumHumidity:Average data
    {
        // Members
        std::string AirLoopName; // name of air loop using MultiZone:MinimumHumidity:Average strategy
        int AirLoopNum;          // index of named air loop
        Real64 MinSetHum;        // minimum supply air humidity ratio [kg/kg]
        Real64 MaxSetHum;        // maximum supply air humidity ratio [kg/kg]
        int NumCtrlNodes;        // number of nodes whose humidity ratio is being set
        Array1D_int CtrlNodes;   // nodes where humidity ratio is being set
        Real64 SetPt;            // the humidity ratio setpoint [kg/kg]

        // Default Constructor
        DefMultiZoneAverageMinHumSetPointManager() : AirLoopNum(0), MinSetHum(0.0), MaxSetHum(0.0), NumCtrlNodes(0), SetPt(0.0)
        {
        }

        void calculate(EnergyPlusData &state);
    };

    struct DefMultiZoneAverageMaxHumSetPointManager : SPBase // derived type for SetpointManager:MultiZone:MaximumHumidity:Average data
    {
        // Members
        std::string AirLoopName; // name of air loop using MultiZone:MaximumHumidity:Average strategy
        int AirLoopNum;          // index of named air loop
        Real64 MinSetHum;        // minimum supply air humidity ratio [kg/kg]
        Real64 MaxSetHum;        // maximum supply air humidity ratio [kg/kg]
        int NumCtrlNodes;        // number of nodes whose humidity ratio is being set
        Array1D_int CtrlNodes;   // nodes where humidity ratio is being set
        Real64 SetPt;            // the humidity ratio setpoint [kg/kg]

        // Default Constructor
        DefMultiZoneAverageMaxHumSetPointManager() : AirLoopNum(0), MinSetHum(0.0), MaxSetHum(0.0), NumCtrlNodes(0), SetPt(0.0)
        {
        }

        void calculate(EnergyPlusData &state);
    };

    struct DefMultiZoneMinHumSetPointManager : SPBase // derived type for SetpointManager:MultiZone:Humidity:Minimum data
    {
        // Members
        std::string AirLoopName; // name of air loop using SetpointManager:MultiZone:Humidity:Minimum
        int AirLoopNum;          // index of named air loop
        Real64 MinSetHum;        // minimum supply air humidity ratio [kg/kg]
        Real64 MaxSetHum;        // maximum supply air humidity ratio [kg/kg]
        int NumCtrlNodes;        // number of nodes whose humidity ratio is being set
        Array1D_int CtrlNodes;   // nodes where humidity ratio is being set
        Real64 SetPt;            // the humidity ratio setpoint [kg/kg]

        // Default Constructor
        DefMultiZoneMinHumSetPointManager() : AirLoopNum(0), MinSetHum(0.0), MaxSetHum(0.0), NumCtrlNodes(0), SetPt(0.0)
        {
        }

        void calculate(EnergyPlusData &state);
    };

    struct DefMultiZoneMaxHumSetPointManager : SPBase // derived type for SetpointManager:MultiZone:Humidity:Maximum data
    {
        // Members
        std::string AirLoopName; // name of air loop using SetpointManager:MultiZone:Humidity:Maximum
        int AirLoopNum;          // index of named air loop
        Real64 MinSetHum;        // minimum supply air humidity ratio [kg/kg]
        Real64 MaxSetHum;        // maximum supply air humidity ratio [kg/kg]
        int NumCtrlNodes;        // number of nodes whose humidity ratio is being set
        Array1D_int CtrlNodes;   // nodes where humidity ratio is being set
        Real64 SetPt;            // the humidity ratio setpoint [kg/kg]

        // Default Constructor
        DefMultiZoneMaxHumSetPointManager() : AirLoopNum(0), MinSetHum(0.0), MaxSetHum(0.0), NumCtrlNodes(0), SetPt(0.0)
        {
        }

        void calculate(EnergyPlusData &state);
    };

    struct DefineFollowOATempSetPointManager : SPBase
    {
        // Members
        std::string RefTempType;       // Reference Temperature type (choice OutdoorAirWetBulb/OutdoorAirDryBulb)
        ReferenceTempType RefTypeMode; // set to iRefTempType_WetBulb or iRefTempType_DryBulb
        Real64 Offset;                 // Offset temperature difference
        Real64 MinSetTemp;             // Minimum supply air setpoint temperature
        Real64 MaxSetTemp;             // Maximum supply air setpoint temperature
        int NumCtrlNodes;              // number of nodes whose temperature is being set
        Array1D_int CtrlNodes;         // nodes where temperature is being set
        Real64 SetPt;                  // the setpoint

        // Default Constructor
        DefineFollowOATempSetPointManager()
            : RefTypeMode(ReferenceTempType::Unknown), Offset(0.0), MinSetTemp(0.0), MaxSetTemp(0.0), NumCtrlNodes(0), SetPt(0.0)
        {
        }

        void calculate(EnergyPlusData &state);
    };

    struct DefineFollowSysNodeTempSetPointManager : SPBase
    {
        // Members
        int RefNodeNum;                // reference node number
        std::string RefTempType;       // Reference Temperature type (choice OutdoorAirWetBulb/OutdoorAirDryBulb)
        ReferenceTempType RefTypeMode; // set to iRefTempType_WetBulb or iRefTempType_DryBulb
        Real64 Offset;                 // Offset temperature difference
        Real64 MinSetTemp;             // Minimum supply air setpoint temperature
        Real64 MaxSetTemp;             // Maximum supply air setpoint temperature
        int NumCtrlNodes;              // number of nodes whose temperature is being set
        Array1D_int CtrlNodes;         // nodes where temperature is being set
        Real64 SetPt;                  // the setpoint

        // Default Constructor
        DefineFollowSysNodeTempSetPointManager()
            : RefNodeNum(0), RefTypeMode(ReferenceTempType::Unknown), Offset(0.0), MinSetTemp(0.0), MaxSetTemp(0.0), NumCtrlNodes(0), SetPt(0.0)
        {
        }

        void calculate(EnergyPlusData &state);
    };

    struct DefineGroundTempSetPointManager : SPBase
    {
        // Members
        std::string RefGroundTempObjType; // Reference Temperature type (Available choices are listed below)
        // Site:GroundTemperature:BuildingSurface
        // Site:GroundTemperature:Shallow
        // Site:GroundTemperature:Deep
        // Site:GroundTemperature:FCfactorMethod
        ReferenceGroundTempObjectType RefTypeMode; // set to iRefGroundTempObjType_xxxx based on RefGroundTempObjType
        Real64 Offset;                             // Offset temperature difference
        Real64 MinSetTemp;                         // Minimum supply air setpoint temperature
        Real64 MaxSetTemp;                         // Maximum supply air setpoint temperature
        int NumCtrlNodes;                          // number of nodes whose temperature is being set
        Array1D_int CtrlNodes;                     // nodes where temperature is being set
        Real64 SetPt;                              // the setpoint

        // Default Constructor
        DefineGroundTempSetPointManager()
            : RefTypeMode(ReferenceGroundTempObjectType::Unknown), Offset(0.0), MinSetTemp(0.0), MaxSetTemp(0.0), NumCtrlNodes(0), SetPt(0.0)
        {
        }

        void calculate(EnergyPlusData &state);
    };

    struct DefineCondEntSetPointManager : SPBase // derived type for SetpointManager:CondenserEnteringReset data
    {
        // Members
        std::string CondEntTempSched;   // Optional schedule
        int CondEntTempSchedPtr;        // default condenser entering water temperature schedule Index
        Real64 TowerDsnInletAirWetBulb; // cooling tower design inlet air wetbulb temperature
        int MinTwrWbCurve;              // minimum design wetbulb temperature curve name
        int MinOaWbCurve;               // minimum outside air wetbulb temperature curve name
        int OptCondEntCurve;            // optimized condenser entering water temperature curve name
        Real64 MinimumLiftTD;           // minimum lift
        Real64 MaxCondEntTemp;          // maximum condenser entering water temp
        int NumCtrlNodes;               // number of nodes whose temperature is being set
        std::string CtrlNodeListName;
        Array1D_int CtrlNodes;      // nodes where temperature is being set
        Real64 SetPt;               // the temperature set point [C]
        int ChillerIndexPlantSide;  // plant side chiller index
        int ChillerIndexDemandSide; // demand side chiller index
        int BranchIndexPlantSide;   // plant side branch index
        int BranchIndexDemandSide;  // demand side branch index
        int LoopIndexPlantSide;     // plant side loop index
        int LoopIndexDemandSide;    // deand side loop index
        int TypeNum;                // chiller type number

        // Default Constructor
        DefineCondEntSetPointManager()
            : CondEntTempSchedPtr(0), TowerDsnInletAirWetBulb(0.0), MinTwrWbCurve(0), MinOaWbCurve(0), OptCondEntCurve(0), MinimumLiftTD(0.0),
              MaxCondEntTemp(0.0), NumCtrlNodes(0), SetPt(0.0), ChillerIndexPlantSide(0), ChillerIndexDemandSide(0), BranchIndexPlantSide(0),
              BranchIndexDemandSide(0), LoopIndexPlantSide(0), LoopIndexDemandSide(0), TypeNum(0)
        {
        }

        void calculate(EnergyPlusData &state);
    };

    struct DefineIdealCondEntSetPointManager : SPBase // derived type for SetpointManager:CondenserEnteringReset:Ideal data
    {
        // Members
        // type of variable to be set
        Real64 MinimumLiftTD;  // minimum lift
        Real64 MaxCondEntTemp; // maximum condenser entering water temp
        int NumCtrlNodes;      // number of nodes whose temperature is being set
        std::string CtrlNodeListName;
        Array1D_int CtrlNodes;                                 // nodes where temperature is being set
        Real64 SetPt;                                          // the temperature set point [C]
        int ChillerIndexPlantSide;                             // plant side chiller index
        int BranchIndexPlantSide;                              // plant side branch index
        int LoopIndexPlantSide;                                // plant side loop index
        OutputProcessor::VariableType ChllrVarType;            // report variable type
        int ChllrVarIndex;                                     // report variable index
        OutputProcessor::VariableType ChlPumpVarType;          // report variable type
        int ChlPumpVarIndex;                                   // report variable index
        Array1D<OutputProcessor::VariableType> ClTowerVarType; // report variable type
        Array1D_int ClTowerVarIndex;                           // report variable index
        OutputProcessor::VariableType CndPumpVarType;          // report variable type
        int CndPumpVarIndex;                                   // report variable index
        int TypeNum;                                           // chiller type number
        Array1D_int TowerNum;                                  // cooling tower number
        int CondLoopNum;                                       // condenser loop number
        Array1D_int CondTowerBranchNum;                        // condenser branch number
        int numTowers;                                         // number of towers to query
        int CondPumpNum;                                       // condenser pump number
        int CondPumpBranchNum;                                 // condenser branch number for pump
        int ChilledPumpNum;                                    // chilled water pump number
        int ChilledPumpBranchNum;                              // chilled water branch number for pump
        bool SetupIdealCondEntSetPtVars;                       // flag for initialization of meters and such

        // Default Constructor
        DefineIdealCondEntSetPointManager()
            : MinimumLiftTD(0.0), MaxCondEntTemp(0.0), NumCtrlNodes(0), SetPt(0.0), ChillerIndexPlantSide(0), BranchIndexPlantSide(0),
              LoopIndexPlantSide(0), ChllrVarType(OutputProcessor::VariableType::NotFound), ChllrVarIndex(0),
              ChlPumpVarType(OutputProcessor::VariableType::NotFound), ChlPumpVarIndex(0), CndPumpVarType(OutputProcessor::VariableType::NotFound),
              CndPumpVarIndex(0), TypeNum(0), CondLoopNum(0), numTowers(0), CondPumpNum(0), CondPumpBranchNum(0), ChilledPumpNum(0),
              ChilledPumpBranchNum(0), SetupIdealCondEntSetPtVars(true)
        {
        }

        void calculate(EnergyPlusData &state);

        void SetupMeteredVarsForSetPt(EnergyPlusData &state);

        Real64 calculateCurrentEnergyUsage(EnergyPlusData &state);

        void setupSetPointAndFlags(Real64 &TotEnergy,
                                   Real64 &TotEnergyPre,
                                   Real64 &CondWaterSetPoint,
                                   Real64 &CondTempLimit,
                                   bool &RunOptCondEntTemp,
                                   bool &RunSubOptCondEntTemp,
                                   bool &RunFinalOptCondEntTemp) const;
    };

    struct DefineSZOneStageCoolinggSetPointManager : SPBase // Derived type for the Single Zone One Stage Cooling Setpoint Manager data
    {
        // Members
        std::string ControlZoneName; // name of the control zone (zone with main thermostat)
        int ControlZoneNum;          // number (index into Zone array) of control zone
        int ZoneNodeNum;             // zone node number
        Real64 CoolingOnTemp;        // minimum supply air setpoint temperature
        Real64 CoolingOffTemp;       // maximum supply air setpoint temperature
        int NumCtrlNodes;
        Array1D_int CtrlNodes; // node numbers of nodes where setpoint is to be set
        Real64 SetPt;          // the setpoint

        // Default Constructor
        DefineSZOneStageCoolinggSetPointManager()
            : ControlZoneNum(0), ZoneNodeNum(0), CoolingOnTemp(0.0), CoolingOffTemp(0.0), NumCtrlNodes(0), SetPt(0.0)
        {
        }

        void calculate(EnergyPlusData &state);
    };

    struct DefineSZOneStageHeatingSetPointManager : SPBase // Derived type for the Single Zone One Stage Heating Setpoint Manager data
    {
        // Members
        std::string ControlZoneName; // name of the control zone (zone with main thermostat)
        int ControlZoneNum;          // number (index into Zone array) of control zone
        int ZoneNodeNum;             // zone node number
        Real64 HeatingOnTemp;        // minimum supply air setpoint temperature
        Real64 HeatingOffTemp;       // maximum supply air setpoint temperature
        int NumCtrlNodes;
        Array1D_int CtrlNodes; // node numbers of nodes where setpoint is to be set
        Real64 SetPt;          // the setpoint

        // Default Constructor
        DefineSZOneStageHeatingSetPointManager()
            : ControlZoneNum(0), ZoneNodeNum(0), HeatingOnTemp(0.0), HeatingOffTemp(0.0), NumCtrlNodes(0), SetPt(0.0)
        {
        }

        void calculate(EnergyPlusData &state);
    };

    struct DefineReturnWaterChWSetPointManager : SPBase // derived type for SetpointManager:SupplyResetForReturnTemperature:ChilledWater data
    {
        // Members
        int returnNodeIndex;                    // node ID for the plant supply-side return node
        int supplyNodeIndex;                    // node ID for the plant supply-side supply node
        Real64 minimumChilledWaterSetpoint;     // the minimum reset temperature for the chilled water setpoint
        Real64 maximumChilledWaterSetpoint;     // the maximum reset temperature for the chilled water setpoint
        int returnTemperatureScheduleIndex;     // the index in Schedules array for the scheduled return temperature; zero if not used
        Real64 returnTemperatureConstantTarget; // the constant value used as the return temperature target; used if schedule index is zero
        Real64 currentSupplySetPt;              // the current supply setpoint temperature
        int plantLoopIndex;                     // the index for the plant loop for this manager, zero if not initialized
        int plantSetpointNodeIndex;             // the index for the node where the plant setpoint is set, need to look up after Plant is established
        bool useReturnTempSetpoint;             // only true if the target return temperature should be looked up as the Node(returnNode).TempSetPoint

        // Default Constructor
        DefineReturnWaterChWSetPointManager()
            : returnNodeIndex(0), supplyNodeIndex(0), minimumChilledWaterSetpoint(0.0), maximumChilledWaterSetpoint(0.0),
              returnTemperatureScheduleIndex(0), returnTemperatureConstantTarget(0.0), currentSupplySetPt(0.0), plantLoopIndex(0),
              plantSetpointNodeIndex(0), useReturnTempSetpoint(false)
        {
        }

        // Calculation method
        void calculate(EnergyPlusData &state, DataLoopNode::NodeData &returnNode, DataLoopNode::NodeData &supplyNode);
    };

    struct DefineReturnWaterHWSetPointManager : SPBase // derived type for SetpointManager:SupplyResetForReturnTemperature:HotWater data
    {
        // Members
        int returnNodeIndex;                    // node ID for the plant supply-side return node
        int supplyNodeIndex;                    // node ID for the plant supply-side supply node
        Real64 maximumHotWaterSetpoint;         // the maximum reset temperature for the hot water setpoint
        Real64 minimumHotWaterSetpoint;         // the minimum reset temperature for the hot water setpoint
        int returnTemperatureScheduleIndex;     // the index in Schedules array for the scheduled return temperature; zero if not used
        Real64 returnTemperatureConstantTarget; // the constant value used as the return temperature target; used if schedule index is zero
        Real64 currentSupplySetPt;              // the current supply setpoint temperature
        int plantLoopIndex;                     // the index for the plant loop for this manager, zero if not initialized
        int plantSetpointNodeIndex;             // the index for the node where the plant setpoint is set, need to look up after Plant is established
        bool useReturnTempSetpoint;             // only true if the target return temperature should be looked up as the Node(returnNode).TempSetPoint

        // Default Constructor
        DefineReturnWaterHWSetPointManager()
            : returnNodeIndex(0), supplyNodeIndex(0), maximumHotWaterSetpoint(0.0), minimumHotWaterSetpoint(0.0), returnTemperatureScheduleIndex(0),
              returnTemperatureConstantTarget(0.0), currentSupplySetPt(0.0), plantLoopIndex(0), plantSetpointNodeIndex(0),
              useReturnTempSetpoint(false)
        {
        }

        // Calculation method
        void calculate(EnergyPlusData &state, DataLoopNode::NodeData &returnNode, DataLoopNode::NodeData &supplyNode);
    };

    struct DefineScheduledTESSetPointManager : SPBase // Derived type for Scheduled TES Setpoint Manager data
    {
        // Members
        int SchedPtr;
        int SchedPtrCharge;
        int CtrlNodeNum;
        Real64 NonChargeCHWTemp;
        Real64 ChargeCHWTemp;
        DataPlant::iCtrlType CompOpType;
        Real64 SetPt;

        // Default Constructor
        DefineScheduledTESSetPointManager()
            : SchedPtr(0), SchedPtrCharge(0), CtrlNodeNum(0), NonChargeCHWTemp(0.0), ChargeCHWTemp(0.0), CompOpType(DataPlant::iCtrlType::Unassigned),
              SetPt(0.0)
        {
        }

        // Calculation method
        void calculate(EnergyPlusData &state);
    };

    void ManageSetPoints(EnergyPlusData &state);

    void GetSetPointManagerInputs(EnergyPlusData &state); // wrapper for GetInput to accomodate unit testing

    void GetSetPointManagerInputData(EnergyPlusData &state, bool &ErrorsFound);

    void VerifySetPointManagers(EnergyPlusData &state, bool &ErrorsFound); // flag to denote node conflicts in input. !unused1208

    void InitSetPointManagers(EnergyPlusData &state);

    void SimSetPointManagers(EnergyPlusData &state);

    void UpdateSetPointManagers(EnergyPlusData &state);

    void UpdateMixedAirSetPoints(EnergyPlusData &state);

    void UpdateOAPretreatSetPoints(EnergyPlusData &state);

    int getSPMBasedOnNode(EnergyPlusData &state, int NodeNum, iCtrlVarType SetPtType, SetPointManagerType SMPType, CtrlNodeType ctrlOrRefNode);

    bool IsNodeOnSetPtManager(EnergyPlusData &state, int NodeNum, iCtrlVarType SetPtType);

    bool NodeHasSPMCtrlVarType(EnergyPlusData &state, int NodeNum, iCtrlVarType iCtrlVarType);

    void ResetHumidityRatioCtrlVarType(EnergyPlusData &state, int NodeNum);

    void CheckIfAnyIdealCondEntSetPoint(EnergyPlusData &state);

    iCtrlVarType GetHumidityRatioVariableType(EnergyPlusData &state, int CntrlNodeNum);

    void SetUpNewScheduledTESSetPtMgr(EnergyPlusData &state,
                                      int SchedPtr,
                                      int SchedPtrCharge,
                                      Real64 NonChargeCHWTemp,
                                      Real64 ChargeCHWTemp,
                                      DataPlant::iCtrlType const &CompOpType,
                                      int ControlNodeNum);

    bool GetCoilFreezingCheckFlag(EnergyPlusData &state, int MixedAirSPMNum);

    int GetMixedAirNumWithCoilFreezingCheck(EnergyPlusData &state, int MixedAirNode);

} // namespace SetPointManager

struct SetPointManagerData : BaseGlobalStruct
{
    // MODULE VARIABLE DECLARATIONS:
    int NumAllSetPtMgrs = 0;                 // Number of all Setpoint Managers found in input
    int NumSchSetPtMgrs = 0;                 // Number of Scheduled Setpoint Managers found in input
    int NumDualSchSetPtMgrs = 0;             // Number of Scheduled Dual Setpoint Managers found in input
    int NumOutAirSetPtMgrs = 0;              // Number of Outside Air Setpoint Managers found in input
    int NumSZRhSetPtMgrs = 0;                // number of single zone reheat setpoint managers
    int NumSZHtSetPtMgrs = 0;                // number of single zone heating setpoint managers
    int NumSZClSetPtMgrs = 0;                // number of single zone cooling setpoint managers
    int NumSZMinHumSetPtMgrs = 0;            // number of Single Zone Minimum Humidity Setpoint Managers
    int NumSZMaxHumSetPtMgrs = 0;            // number of Single Zone Maximum Humidity Setpoint Managers
    int NumMixedAirSetPtMgrs = 0;            // number of mixed air setpoint managers
    int NumOAPretreatSetPtMgrs = 0;          // number of outside air pretreat setpoint managers
    int NumWarmestSetPtMgrs = 0;             // number of Warmest setpoint managers
    int NumColdestSetPtMgrs = 0;             // number of Coldest setpoint managers
    int NumWarmestSetPtMgrsTempFlow = 0;     // number of Warmest Temp Flow setpoint managers
    int NumRABFlowSetPtMgrs = 0;             // number of return air bypass temperature-based flow setpoint manager
    int NumMZClgAverageSetPtMgrs = 0;        // number of Multizone:Cooling:Average setpoint managers
    int NumMZHtgAverageSetPtMgrs = 0;        // number of Multizone:Heating:Average setpoint managers
    int NumMZAverageMinHumSetPtMgrs = 0;     // number of MultiZone:MinimumHumidity:Average setpoint managers
    int NumMZAverageMaxHumSetPtMgrs = 0;     // number of MultiZone:MaximumHumidity:Average setpoint managers
    int NumMZMinHumSetPtMgrs = 0;            // number of MultiZone:Humidity:Minimum setpoint managers
    int NumMZMaxHumSetPtMgrs = 0;            // number of MultiZone:Humidity:Maximum setpoint managers
    int NumFollowOATempSetPtMgrs = 0;        // number of SetpointManager:FollowOutdoorAirTemperature setpoint managers
    int NumFollowSysNodeTempSetPtMgrs = 0;   // number of SetpointManager:FollowSystemNodeTemperature setpoint managers
    int NumGroundTempSetPtMgrs = 0;          // number of SetpointManager:FollowGroundTemperature setpoint managers
    int NumCondEntSetPtMgrs = 0;             // number of Condenser Entering Reset setpoint managers
    int NumIdealCondEntSetPtMgrs = 0;        // number of Ideal Condenser Entering Temperature setpoint managers
    int NumSZOneStageCoolingSetPtMgrs = 0;   // number of single zone one stage cooling setpoint managers
    int NumSZOneStageHeatingSetPtMgrs = 0;   // number of singel zone one stage heating setpoint managers
    int NumReturnWaterResetChWSetPtMgrs = 0; // number of return water reset setpoint managers
    int NumReturnWaterResetHWSetPtMgrs = 0;  // number of hot-water return water reset setpoint managers
    int NumSchTESSetPtMgrs = 0;              // number of TES scheduled setpoint managers (created internally, not by user input)

    Real64 TSupNoHC = 0.0;     // supply temperature with no heating or cooling
    Real64 ExtrRateNoHC = 0.0; // the heating (>0) or cooling (<0) that can be done by supply air at TSupNoHC [W]

    int GetSetPointManagerInputMaxNumAlphas = 0;  // argument for call to GetObjectDefMaxArgs
    int GetSetPointManagerInputMaxNumNumbers = 0; // argument for call to GetObjectDefMaxArgs
    int InitSetPointManagerTypeNum = 0;
    int InitSetPointManagerNumChiller = 0;
    int InitSetPointManagerTypeOf_Num = 0;

    bool ManagerOn = false;
    bool GetInputFlag = true; // First time, input is "gotten"

    bool InitSetPointManagersOneTimeFlag = true;
    bool InitSetPointManagersOneTimeFlag2 = true;
    Real64 DCESPMDsn_EntCondTemp = 0.0;
    Real64 DCESPMDsn_MinCondSetpt = 0.0;
    Real64 DCESPMCur_MinLiftTD = 0.0;
    Real64 DCESPMDesign_Load_Sum = 0.0;
    Real64 DCESPMActual_Load_Sum = 0.0;
    Real64 DCESPMWeighted_Actual_Load_Sum = 0.0;
    Real64 DCESPMWeighted_Design_Load_Sum = 0.0;
    Real64 DCESPMWeighted_Ratio = 0.0;
    Real64 DCESPMMin_DesignWB = 0.0;
    Real64 DCESPMMin_ActualWb = 0.0;
    Real64 DCESPMOpt_CondEntTemp = 0.0;
    Real64 DCESPMDesignClgCapacity_Watts = 0.0;
    Real64 DCESPMCurrentLoad_Watts = 0.0;
    Real64 DCESPMCondInletTemp = 0.0;
    Real64 DCESPMEvapOutletTemp = 0.0;
    bool NoSurfaceGroundTempObjWarning = true; // This will cause a warning to be issued if no "surface" ground
    // temperature object was input.
    bool NoShallowGroundTempObjWarning = true; // This will cause a warning to be issued if no "shallow" ground
    // temperature object was input.
    bool NoDeepGroundTempObjWarning = true; // This will cause a warning to be issued if no "deep" ground
    // temperature object was input.
    bool NoFCGroundTempObjWarning = true; // This will cause a warning to be issued if no ground
    // temperature object was input for FC Factor method
    bool InitSetPointManagersMyEnvrnFlag = true; // flag for init once at start of environment
    bool RunSubOptCondEntTemp = false;
    bool RunFinalOptCondEntTemp = false;

    // Object Data
    Array1D<SetPointManager::DataSetPointManager> AllSetPtMgr;                                    // Array for all Setpoint Manager data(warnings)
    Array1D<SetPointManager::DefineScheduledSetPointManager> SchSetPtMgr;                         // Array for Scheduled Setpoint Manager data
    Array1D<SetPointManager::DefineSchedDualSetPointManager> DualSchSetPtMgr;                     // Dual Scheduled Setpoint Manager data
    Array1D<SetPointManager::DefineOutsideAirSetPointManager> OutAirSetPtMgr;                     // Array for Outside Air Setpoint Manager data
    Array1D<SetPointManager::DefineSZReheatSetPointManager> SingZoneRhSetPtMgr;                   // Array for SZRH Set Pt Mgr
    Array1D<SetPointManager::DefineSZHeatingSetPointManager> SingZoneHtSetPtMgr;                  // Array for SZ Heating Set Pt Mgr
    Array1D<SetPointManager::DefineSZCoolingSetPointManager> SingZoneClSetPtMgr;                  // Array for SZ Cooling Set Pt Mgr
    Array1D<SetPointManager::DefineSZMinHumSetPointManager> SZMinHumSetPtMgr;                     // Array for SZ Min Hum Set Pt Mgr
    Array1D<SetPointManager::DefineSZMaxHumSetPointManager> SZMaxHumSetPtMgr;                     // Array for SZ Max Hum Set Pt Mgr
    Array1D<SetPointManager::DefineMixedAirSetPointManager> MixedAirSetPtMgr;                     // Array for Mixed Air Set Pt Mgr
    Array1D<SetPointManager::DefineOAPretreatSetPointManager> OAPretreatSetPtMgr;                 // Array for OA Pretreat Set Pt Mgr
    Array1D<SetPointManager::DefineWarmestSetPointManager> WarmestSetPtMgr;                       // Array for Warmest Set Pt Mgr
    Array1D<SetPointManager::DefineColdestSetPointManager> ColdestSetPtMgr;                       // Array for Coldest Set Pt Mgr
    Array1D<SetPointManager::DefWarmestSetPtManagerTempFlow> WarmestSetPtMgrTempFlow;             // Array for Warmest Set Pt Mgr
    Array1D<SetPointManager::DefRABFlowSetPointManager> RABFlowSetPtMgr;                          // Array for return air bypass
    Array1D<SetPointManager::DefMultiZoneAverageCoolingSetPointManager> MZAverageCoolingSetPtMgr; // Array for MultiZone
    Array1D<SetPointManager::DefMultiZoneAverageHeatingSetPointManager> MZAverageHeatingSetPtMgr; // Array for MultiZone
    Array1D<SetPointManager::DefMultiZoneAverageMinHumSetPointManager> MZAverageMinHumSetPtMgr;   // Array for MultiZone
    Array1D<SetPointManager::DefMultiZoneAverageMaxHumSetPointManager> MZAverageMaxHumSetPtMgr;   // Array for MultiZone
    Array1D<SetPointManager::DefMultiZoneMinHumSetPointManager> MZMinHumSetPtMgr;                 // Multizone min humidity rat Set Pt Mgr
    Array1D<SetPointManager::DefMultiZoneMaxHumSetPointManager> MZMaxHumSetPtMgr;                 // Multizone max humidity rat Set Pt Mgr
    Array1D<SetPointManager::DefineFollowOATempSetPointManager> FollowOATempSetPtMgr;             // Array for Follow Outdoor Air
    Array1D<SetPointManager::DefineFollowSysNodeTempSetPointManager> FollowSysNodeTempSetPtMgr;   // Array for Follow System
    Array1D<SetPointManager::DefineGroundTempSetPointManager> GroundTempSetPtMgr;                 // Array for Ground Temp Setpoint
    Array1D<SetPointManager::DefineCondEntSetPointManager> CondEntSetPtMgr;                       // Condenser Entering Water Set Pt Mgr
    Array1D<SetPointManager::DefineIdealCondEntSetPointManager> IdealCondEntSetPtMgr;             // Ideal Condenser Entering Set Pt Mgr
    Array1D<SetPointManager::DefineSZOneStageCoolinggSetPointManager> SZOneStageCoolingSetPtMgr;  // single zone 1 stage cool
    Array1D<SetPointManager::DefineSZOneStageHeatingSetPointManager> SZOneStageHeatingSetPtMgr;   // single zone 1 stage heat
    Array1D<SetPointManager::DefineReturnWaterChWSetPointManager> ReturnWaterResetChWSetPtMgr;    // return water reset
    Array1D<SetPointManager::DefineReturnWaterHWSetPointManager> ReturnWaterResetHWSetPtMgr;      // hot-water return water reset
    Array1D<SetPointManager::DefineScheduledTESSetPointManager> SchTESSetPtMgr;                   // Array for TES Scheduled Setpoint Manager data

    Real64 CondWaterSetPoint = 0; // Condenser entering water temperature setpoint this timestep, C
    Real64 EvapOutletTemp = 0;    // Evaporator water outlet temperature (C)
    Real64 CondTempLimit = 0;     // Condenser entering water temperature setpoint lower limit
    Real64 CurLoad = 0;           // Current cooling load, W
    Real64 TotEnergy = 0;         // Total energy consumptions at this time step
    Real64 TotEnergyPre = 0;      // Total energy consumptions at the previous time step

    void clear_state() override
    {

        NumAllSetPtMgrs = 0;                 // Number of all Setpoint Managers found in input
        NumSchSetPtMgrs = 0;                 // Number of Scheduled Setpoint Managers found in input
        NumDualSchSetPtMgrs = 0;             // Number of Scheduled Dual Setpoint Managers found in input
        NumOutAirSetPtMgrs = 0;              // Number of Outside Air Setpoint Managers found in input
        NumSZRhSetPtMgrs = 0;                // number of single zone reheat setpoint managers
        NumSZHtSetPtMgrs = 0;                // number of single zone heating setpoint managers
        NumSZClSetPtMgrs = 0;                // number of single zone cooling setpoint managers
        NumSZMinHumSetPtMgrs = 0;            // number of Single Zone Minimum Humidity Setpoint Managers
        NumSZMaxHumSetPtMgrs = 0;            // number of Single Zone Maximum Humidity Setpoint Managers
        NumMixedAirSetPtMgrs = 0;            // number of mixed air setpoint managers
        NumOAPretreatSetPtMgrs = 0;          // number of outside air pretreat setpoint managers
        NumWarmestSetPtMgrs = 0;             // number of Warmest setpoint managers
        NumColdestSetPtMgrs = 0;             // number of Coldest setpoint managers
        NumWarmestSetPtMgrsTempFlow = 0;     // number of Warmest Temp Flow setpoint managers
        NumRABFlowSetPtMgrs = 0;             // number of return air bypass temperature-based flow setpoint manager
        NumMZClgAverageSetPtMgrs = 0;        // number of Multizone:Cooling:Average setpoint managers
        NumMZHtgAverageSetPtMgrs = 0;        // number of Multizone:Heating:Average setpoint managers
        NumMZAverageMinHumSetPtMgrs = 0;     // number of MultiZone:MinimumHumidity:Average setpoint managers
        NumMZAverageMaxHumSetPtMgrs = 0;     // number of MultiZone:MaximumHumidity:Average setpoint managers
        NumMZMinHumSetPtMgrs = 0;            // number of MultiZone:Humidity:Minimum setpoint managers
        NumMZMaxHumSetPtMgrs = 0;            // number of MultiZone:Humidity:Maximum setpoint managers
        NumFollowOATempSetPtMgrs = 0;        // number of SetpointManager:FollowOutdoorAirTemperature setpoint managers
        NumFollowSysNodeTempSetPtMgrs = 0;   // number of SetpointManager:FollowSystemNodeTemperature setpoint managers
        NumGroundTempSetPtMgrs = 0;          // number of SetpointManager:FollowGroundTemperature setpoint managers
        NumCondEntSetPtMgrs = 0;             // number of Condenser Entering Reset setpoint managers
        NumIdealCondEntSetPtMgrs = 0;        // number of Ideal Condenser Entering Temperature setpoint managers
        NumSZOneStageCoolingSetPtMgrs = 0;   // number of single zone one stage cooling setpoint managers
        NumSZOneStageHeatingSetPtMgrs = 0;   // number of singel zone one stage heating setpoint managers
        NumReturnWaterResetChWSetPtMgrs = 0; // number of return water reset setpoint managers
        NumReturnWaterResetHWSetPtMgrs = 0;  // number of hot-water return water reset setpoint managers
        NumSchTESSetPtMgrs = 0;              // number of TES Scheduled setpoint Managers

        DCESPMDsn_EntCondTemp = 0.0;
        DCESPMDsn_MinCondSetpt = 0.0;
        DCESPMCur_MinLiftTD = 0.0;
        DCESPMDesign_Load_Sum = 0.0;
        DCESPMActual_Load_Sum = 0.0;
        DCESPMWeighted_Actual_Load_Sum = 0.0;
        DCESPMWeighted_Design_Load_Sum = 0.0;
        DCESPMWeighted_Ratio = 0.0;
        DCESPMMin_DesignWB = 0.0;
        DCESPMMin_ActualWb = 0.0;
        DCESPMOpt_CondEntTemp = 0.0;
        DCESPMDesignClgCapacity_Watts = 0.0;
        DCESPMCurrentLoad_Watts = 0.0;
        DCESPMCondInletTemp = 0.0;
        DCESPMEvapOutletTemp = 0.0;

        TSupNoHC = 0.0;     // supply temperature with no heating or cooling
        ExtrRateNoHC = 0.0; // the heating (>0) or cooling (<0) that can be done by supply air at TSupNoHC [W]

        GetSetPointManagerInputMaxNumAlphas = 0;  // argument for call to GetObjectDefMaxArgs
        GetSetPointManagerInputMaxNumNumbers = 0; // argument for call to GetObjectDefMaxArgs
        InitSetPointManagerTypeNum = 0;
        InitSetPointManagerNumChiller = 0;
        InitSetPointManagerTypeOf_Num = 0;

        ManagerOn = false;
        GetInputFlag = true; // First time, input is "gotten"
        // Object Data
        InitSetPointManagersOneTimeFlag = true;
        InitSetPointManagersOneTimeFlag2 = true;
        AllSetPtMgr.deallocate();                 // Array for all Setpoint Manager data(warnings)
        SchSetPtMgr.deallocate();                 // Array for Scheduled Setpoint Manager data
        DualSchSetPtMgr.deallocate();             // Dual Scheduled Setpoint Manager data
        OutAirSetPtMgr.deallocate();              // Array for Outside Air Setpoint Manager data
        SingZoneRhSetPtMgr.deallocate();          // Array for SZRH Set Pt Mgr
        SingZoneHtSetPtMgr.deallocate();          // Array for SZ Heating Set Pt Mgr
        SingZoneClSetPtMgr.deallocate();          // Array for SZ Cooling Set Pt Mgr
        SZMinHumSetPtMgr.deallocate();            // Array for SZ Min Hum Set Pt Mgr
        SZMaxHumSetPtMgr.deallocate();            // Array for SZ Max Hum Set Pt Mgr
        MixedAirSetPtMgr.deallocate();            // Array for Mixed Air Set Pt Mgr
        OAPretreatSetPtMgr.deallocate();          // Array for OA Pretreat Set Pt Mgr
        WarmestSetPtMgr.deallocate();             // Array for Warmest Set Pt Mgr
        ColdestSetPtMgr.deallocate();             // Array for Coldest Set Pt Mgr
        WarmestSetPtMgrTempFlow.deallocate();     // Array for Warmest Set Pt Mgr
        RABFlowSetPtMgr.deallocate();             // Array for return air bypass
        MZAverageCoolingSetPtMgr.deallocate();    // Array for MultiZone
        MZAverageHeatingSetPtMgr.deallocate();    // Array for MultiZone
        MZAverageMinHumSetPtMgr.deallocate();     // Array for MultiZone
        MZAverageMaxHumSetPtMgr.deallocate();     // Array for MultiZone
        MZMinHumSetPtMgr.deallocate();            // Multizone min humidity rat Set Pt Mgr
        MZMaxHumSetPtMgr.deallocate();            // Multizone max humidity rat Set Pt Mgr
        FollowOATempSetPtMgr.deallocate();        // Array for Follow Outdoor Air
        FollowSysNodeTempSetPtMgr.deallocate();   // Array for Follow System
        GroundTempSetPtMgr.deallocate();          // Array for Ground Temp Setpoint
        CondEntSetPtMgr.deallocate();             // Condenser Entering Water Set Pt Mgr
        IdealCondEntSetPtMgr.deallocate();        // Ideal Condenser Entering Set Pt Mgr
        SZOneStageCoolingSetPtMgr.deallocate();   // single zone 1 stage cool
        SZOneStageHeatingSetPtMgr.deallocate();   // single zone 1 stage heat
        ReturnWaterResetChWSetPtMgr.deallocate(); // return water reset
        ReturnWaterResetHWSetPtMgr.deallocate();  // hot-water return water reset
        SchTESSetPtMgr.deallocate();              // TES Scheduled setpoint Managers

        NoSurfaceGroundTempObjWarning = true;
        NoShallowGroundTempObjWarning = true;
        NoDeepGroundTempObjWarning = true;
        NoFCGroundTempObjWarning = true;
        InitSetPointManagersMyEnvrnFlag = true;
        RunSubOptCondEntTemp = false;
        RunFinalOptCondEntTemp = false;
        CondWaterSetPoint = 0;
        EvapOutletTemp = 0;
        CondTempLimit = 0;
        CurLoad = 0;
        TotEnergy = 0;
        TotEnergyPre = 0;
    }
};

} // namespace EnergyPlus

#endif
