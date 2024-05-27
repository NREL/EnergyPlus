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

#ifndef SetPointManager_hh_INCLUDED
#define SetPointManager_hh_INCLUDED

#include <map>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/EnergyPlus.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/Plant/Enums.hh>
#include <EnergyPlus/Plant/PlantLocation.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

namespace SetPointManager {

    enum class SupplyFlowTempStrategy
    {
        Invalid = -1,
        MaxTemp,
        MinTemp,
        Num
    };
        
    enum class ControlStrategy
    {
        Invalid = -1,
        TempFirst,
        FlowFirst,
        Num
    };

    enum class AirTempType
    {
        Invalid = -1,
        WetBulb,
        DryBulb,
        Num
    };

    enum class ReturnTempType
    {
        Invalid = -1,
        Scheduled,
        Constant,
        Setpoint,
        Num
    };
        
    enum class SPMType
    {
        Invalid = -1,
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
        ReturnAirBalance,
        MZCoolingAverage,
        MZHeatingAverage,
        MZMinHumAverage,
        MZMaxHumAverage,
        MZMinHum,
        MZMaxHum,
        FollowOutsideAirTemp,
        FollowSystemNodeTemp,
        FollowGroundTemp,
        CondenserEnteringTemp,
        IdealCondenserEnteringTemp,
        SZOneStageCooling,
        SZOneStageHeating,
        ChilledWaterReturnTemp,
        HotWaterReturnTemp,
        TESScheduled,
        SystemNodeTemp,
        SystemNodeHum,
        Num
    };

    struct SPMBase
    {
        std::string Name;
        SPMType type = SPMType::Invalid;

        HVAC::CtrlVarType ctrlVar = HVAC::CtrlVarType::Invalid; // set to CtrlVarType::*

        int numCtrlNodes = 0;        // number of control nodes
        Array1D_int ctrlNodes;       // index to control node

        std::string AirLoopName;     // name of air loop
        int AirLoopNum = 0;          // index to air loop
        int RefNodeNum = 0;          // index to reference node

        Real64 MinSetTemp = 0.0;     // minimum supply air setpoint temperature
        Real64 MaxSetTemp = 0.0;     // maximum supply air setpoint temperature

        Real64 MinSetHum = 0.0;      // minimum supply air setpoint humidity ratio [kg/kg]
        Real64 MaxSetHum = 0.0;      // maximum supply air setpoint humidity ratio [kg/kg]
        Real64 SetPt = 0.0;          // the setpoint

        virtual ~SPMBase() = default;

        virtual void calculate(EnergyPlusData &state) = 0;

    };

    struct SPMScheduled : SPMBase // Derived type for Scheduled Setpoint Manager data
    {
        // Members
        int SchedPtr = 0;

        void calculate(EnergyPlusData &state);
    };

    struct SPMScheduledDual : SPMBase // Derived type for Scheduled Dual Setpoint Manager
    {
        // Members
        int SchedPtrHi = 0;
        int SchedPtrLo = 0;
        Real64 SetPtHi = 0.0;
        Real64 SetPtLo = 0.0;

        void calculate(EnergyPlusData &state);
    };

    struct SPMOutsideAir : SPMBase // Derived type for Outside Air Setpoint Manager Data
    {
        // Members
        Real64 OutLowSetPt1 = 0.0;         // 1st setpoint at outside low
        Real64 OutLow1 = 0.0;              // 1st Outside low
        Real64 OutHighSetPt1 = 0.0;        // 1st setpoint at outside high
        Real64 OutHigh1 = 0.0;             // 1st Outside high
        int SchedPtr = 0;                  // Schedule index
        int invalidSchedValErrorIndex = 0; // index for recurring error when schedule is not 1 or 2
        int setPtErrorCount = 0;           // countfor recurring error when schedule is not 1 or 2
        Real64 OutLowSetPt2 = 0.0;         // 2nd setpoint at outside low (optional)
        Real64 OutLow2 = 0.0;              // 2nd Outside low (optional)
        Real64 OutHighSetPt2 = 0.0;        // 2nd setpoint at outside high (optional)
        Real64 OutHigh2 = 0.0;             // 2nd Outside high (optional)

        void calculate(EnergyPlusData &state);
    };

    struct SPMSingleZoneReheat : SPMBase // Derived type for the Single Zone Reheat Setpoint Manager data
    {
        // Members
        std::string ControlZoneName; // name of the control zone (zone with main thermostat)
        int ControlZoneNum = 0;          // number (index into Zone array) of control zone
        int ZoneNodeNum = 0;             // zone node number
        int ZoneInletNodeNum = 0;        // inlet node number for the SZRH air
        int MixedAirNodeNum = 0;            // mixed air node number
        int FanInNodeNum = 0;               // fan inlet node number
        int FanOutNodeNum = 0;              // fan outlet node number
        int OAInNodeNum = 0;                // outside airstream inlet node to the OA mixer
        int RetNodeNum = 0;                 // return node inlet to OA mixer
        int LoopInNodeNum = 0;              // Primary Air System inlet node

        void calculate(EnergyPlusData &state);
    };

    // The one can be used for both SZ Heating and SZCooling SPMs
    struct SPMSingleZoneTemp : SPMBase 
    {
        // Members
        std::string ControlZoneName; // name of the control zone (zone with main thermostat)
        int ControlZoneNum = 0;          // number (index into Zone array) of control zone
        int ZoneNodeNum = 0;             // zone node number
        int ZoneInletNodeNum = 0;        // inlet node number for the supply air

        void calculate(EnergyPlusData &state);
    };

    // Can be used for both SZMinHum and SZMaxHum SPMs
    struct SPMSingleZoneHum : SPMBase // Derived Type for Single Zone Minimum Humidity Setpoint Manager data
    {
        // Members
        int ZoneNodeNum = 0;   // zone node numbers of zones being controlled
        int CtrlZoneNum = 0; // index into ZoneEquipConfig

        void calculate(EnergyPlusData &state);
    };

    struct SPMMixedAir : SPMBase

    {
        // Members
        int FanInNodeNum = 0;               // supply fan inlet node number
        int FanOutNodeNum = 0;              // Supplt fan outlet node number
        bool MySetPointCheckFlag = true; // used for mixed air SPM test for missing SP
        bool FreezeCheckEnable = true;   // Enable freezing check
        int CoolCoilInNodeNum = 0;          // Cooling coil inlet node number
        int CoolCoilOutNodeNum = 0;         // Cooling coil outlet node number
        Real64 MinCoolCoilOutTemp = 7.2; // The minimum temperature at cooling coil outlet node

        void calculate(EnergyPlusData &state);
    };

    struct SPMOutsideAirPretreat : SPMBase
    {
        // Members
        int MixedOutNodeNum = 0;           // mixed air outlet node number
        int OAInNodeNum = 0;               // outside air inlet node number
        int ReturnInNodeNum = 0;           // return air inlet node number
        bool MySetPointCheckFlag = true; // used for DOAS SPM test for missing SP

        void calculate(EnergyPlusData &state);
    };

    // Works for both coldest and warmest SPMs
    struct SPMTempest : SPMBase
    {
        // Members
        SupplyFlowTempStrategy Strategy = SupplyFlowTempStrategy::Invalid; // supply flow and temperature set strategy

        void calculate(EnergyPlusData &state);
    };

    struct SPMWarmestTempFlow : SPMBase
    {
        // Members
        ControlStrategy Strategy = ControlStrategy::Invalid; // supply flow and temperature set strategy
        Real64 MinTurndown = 0.0;    // minimum fractional flow rate
        Real64 Turndown = 0.0;       // fractional flow rate
        int CritZoneNum = 0;
        bool SimReady = false;

        void calculate(EnergyPlusData &state);
    };

    struct SPMReturnAirBalanceFlow : SPMBase
    {
        // Members
        int SchedPtr = 0;            // index of the above schedule
        Real64 FlowSetPt = 0;        // mass flow rate setpoint (kg/s)
        int RABMixInNodeNum = 0;
        int SupMixInNodeNum = 0;
        int MixOutNodeNum = 0;
        int RABSplitOutNodeNum = 0;
        int SysOutNodeNum = 0;

        void calculate(EnergyPlusData &state);
    };

    struct SPMMultiZoneTemp : SPMBase // derived type for SetpointManager:Multizone:Cooling:Average data
    {
        void calculate(EnergyPlusData &state);
    };

    struct SPMMultiZoneHum : SPMBase // derived type for SetpointManager:MultiZone:MinimumHumidity:Average data
    {
        void calculate(EnergyPlusData &state);
    };

    struct SPMFollowOutsideAirTemp : SPMBase
    {
        // Members
        AirTempType RefTempType = AirTempType::Invalid; // set to iRefTempType_WetBulb or iRefTempType_DryBulb
        Real64 Offset = 0.0;                 // Offset temperature difference

        void calculate(EnergyPlusData &state);
    };

    struct SPMFollowSysNodeTemp : SPMBase
    {
        // Members
        AirTempType RefTempType = AirTempType::Invalid; // set to iRefTempType_WetBulb or iRefTempType_DryBulb
        Real64 Offset = 0.0;                 // Offset temperature difference

        void calculate(EnergyPlusData &state);
    };

    struct SPMFollowGroundTemp : SPMBase
    {
        // Members
        // Site:GroundTemperature:BuildingSurface
        // Site:GroundTemperature:Shallow
        // Site:GroundTemperature:Deep
        // Site:GroundTemperature:FCfactorMethod
        DataEnvironment::GroundTempType RefTempType = DataEnvironment::GroundTempType::Invalid; 
        Real64 Offset = 0.0;                             // Offset temperature difference

        void calculate(EnergyPlusData &state);
    };

    struct SPMCondenserEnteringTemp : SPMBase // derived type for SetpointManager:CondenserEnteringReset data
    {
        // Members
        int CondenserEnteringTempSchedPtr = 0;        // default condenser entering water temperature schedule Index
        Real64 TowerDesignInletAirWetBulbTemp = 0; // cooling tower design inlet air wetbulb temperature
        int MinTowerDesignWetBulbCurveNum = 0;              // minimum design wetbulb temperature curve name
        int MinOAWetBulbCurveNum = 0;               // minimum outside air wetbulb temperature curve name
        int OptCondenserEnteringTempCurveNum = 0;            // optimized condenser entering water temperature curve name
        Real64 MinimumLift = 0.0;           // minimum lift
        Real64 MaxCondenserEnteringTemp = 0.0;          // maximum condenser entering water temp
        PlantLocation plantPloc;          // plant side chiller index
        PlantLocation demandPloc; 
        DataPlant::PlantEquipmentType ChillerType; // chiller type number

        void calculate(EnergyPlusData &state);
    };

    struct SPMVar
    {
        OutputProcessor::VariableType Type = OutputProcessor::VariableType::Invalid;
        int Num = 0;
    };
            
    struct SPMIdealCondenserEnteringTemp : SPMBase // derived type for SetpointManager:CondenserEnteringReset:Ideal data
    {
        // Members
        Real64 MinimumLift = 0.0;  // minimum lift
        Real64 MaxCondenserEnteringTemp = 0.0; // maximum condenser entering water temp
        PlantLocation ChillerPloc;                           // plant side chiller index
        SPMVar ChillerVar;
        SPMVar ChilledWaterPumpVar;
        Array1D<SPMVar> TowerVars; 
        SPMVar CondenserPumpVar;
        DataPlant::PlantEquipmentType ChillerType = DataPlant::PlantEquipmentType::Invalid;             // chiller type number
        Array1D<PlantLocation> TowerPlocs;
        int NumTowers = 0;                                     // number of towers to query
        PlantLocation CondenserPumpPloc;                              // condenser pump number
        PlantLocation ChilledWaterPumpPloc;                           // chilled water pump number
        bool SetupIdealCondEntSetPtVars = true;                // flag for initialization of meters and such

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

    struct SPMSingleZoneOneStageCooling : SPMBase // Derived type for the Single Zone One Stage Cooling Setpoint Manager data
    {
        // Members
        int ControlZoneNum = 0;          // number (index into Zone array) of control zone
        int ZoneNodeNum = 0;             // zone node number
        Real64 CoolingOnSetPt = 0.0;        // minimum supply air setpoint temperature
        Real64 CoolingOffSetPt = 0.0;       // maximum supply air setpoint temperature

        void calculate(EnergyPlusData &state);
    };

    struct SPMSingleZoneOneStageHeating : SPMBase // Derived type for the Single Zone One Stage Cooling Setpoint Manager data
    {
        // Members
        int ControlZoneNum = 0;          // number (index into Zone array) of control zone
        int ZoneNodeNum = 0;             // zone node number
        Real64 HeatingOnSetPt = 0.0;        // minimum supply air setpoint temperature
        Real64 HeatingOffSetPt = 0.0;       // maximum supply air setpoint temperature

        void calculate(EnergyPlusData &state);
    };


    struct SPMReturnWaterTemp : SPMBase // derived type for SetpointManager:SupplyResetForReturnTemperature:ChilledWater data
    {
        // Members
        int returnNodeNum = 0;                    // node ID for the plant supply-side return node
        int supplyNodeNum = 0;                    // node ID for the plant supply-side supply node
        int returnTempSchedNum = 0;     // the index in Schedules array for the scheduled return temperature; zero if not used
        Real64 returnTempConstantTarget = 0.0; // the constant value used as the return temperature target; used if schedule index is zero
        Real64 currentSupplySetPt = 0.0;              // the current supply setpoint temperature
        int plantLoopNum = 0;                     // the index for the plant loop for this manager, zero if not initialized
        int plantSetPtNodeNum = 0;             // the index for the node where the plant setpoint is set, need to look up after Plant is established
        ReturnTempType returnTempType = ReturnTempType::Invalid;

        // Calculation method
        void calculate(EnergyPlusData &state);
        // void calculate(EnergyPlusData &state, DataLoopNode::NodeData &returnNode, DataLoopNode::NodeData &supplyNode);
    };

    struct SPMTESScheduled : SPMBase // Derived type for Scheduled TES Setpoint Manager data
    {
        // Members
        int SchedPtr = 0;
        int SchedPtrCharge = 0;
        int CtrlNodeNum = 0;
        Real64 NonChargeCHWTemp = 0.0;
        Real64 ChargeCHWTemp = 0.0;
        DataPlant::CtrlType CompOpType = DataPlant::CtrlType::Invalid;

        // Calculation method
        void calculate(EnergyPlusData &state);
    };

    struct SPMSystemNode : SPMBase // Derived type for System Node Reset Setpoint Manager Data
    {
        // Members
        Real64 LowRefSetPt = 0.0;  // Setpoint at Low Reference Temperature or Humidity Ratio (i.e., Maximum Temperature/Humidity Ratio Setpoint)
        Real64 HighRefSetPt = 0.0; // Setpoint at High Reference Temperature or Humidity Ratio (i.e., Maximum Temperature/Humidity Ratio Setpoint)
        Real64 LowRef = 0.0;      // Low Reference Temperature or Humidity Ratio
        Real64 HighRef = 0.0;     // High Reference Temperature or Humidity Ratio

        void calculate(EnergyPlusData &state);
    };

    int GetSetPointManagerIndex(EnergyPlusData &state, std::string const &Name);
            
    void ManageSetPoints(EnergyPlusData &state);

    void GetSetPointManagerInputs(EnergyPlusData &state); // wrapper for GetInput to accomodate unit testing

    void GetSetPointManagerInputData(EnergyPlusData &state, bool &ErrorsFound);

    void VerifySetPointManagers(EnergyPlusData &state, bool &ErrorsFound); // flag to denote node conflicts in input. !unused1208

    void InitSetPointManagers(EnergyPlusData &state);

    void SimSetPointManagers(EnergyPlusData &state);

    void UpdateSetPointManagers(EnergyPlusData &state);

    void UpdateMixedAirSetPoints(EnergyPlusData &state);

    void UpdateOAPretreatSetPoints(EnergyPlusData &state);

    int GetSetPointManagerIndexByNode(EnergyPlusData &state, int NodeNum, HVAC::CtrlVarType ctrlVar, SPMType spmType, bool isRefNode);

    bool IsNodeOnSetPtManager(EnergyPlusData &state, int NodeNum, HVAC::CtrlVarType ctrlVar);

    bool NodeHasSPMCtrlVarType(EnergyPlusData &state, int NodeNum, HVAC::CtrlVarType ctrlVar);

    void ResetHumidityRatioCtrlVarType(EnergyPlusData &state, int NodeNum);

    void CheckIfAnyIdealCondEntSetPoint(EnergyPlusData &state);

    HVAC::CtrlVarType GetHumidityRatioVariableType(EnergyPlusData &state, int CtrlNodeNum);

    void SetUpNewScheduledTESSetPtMgr(EnergyPlusData &state,
                                      int SchedPtr,
                                      int SchedPtrCharge,
                                      Real64 NonChargeCHWTemp,
                                      Real64 ChargeCHWTemp,
                                      DataPlant::CtrlType CompOpType,
                                      int ControlNodeNum);

    bool GetCoilFreezingCheckFlag(EnergyPlusData &state, int MixedAirSPMNum);

    int GetMixedAirNumWithCoilFreezingCheck(EnergyPlusData &state, int MixedAirNode);

    Real64 interpSetPoint(Real64 LowVal, Real64 HighVal, Real64 RefVal, Real64 SetptAtLowVal, Real64 SetptAtHighVal);
} // namespace SetPointManager

struct SetPointManagerData : BaseGlobalStruct
{
    int GetSetPointManagerInputMaxNumAlphas = 0;  // argument for call to GetObjectDefMaxArgs
    int GetSetPointManagerInputMaxNumNumbers = 0; // argument for call to GetObjectDefMaxArgs

    bool ManagerOn = false;
    bool GetInputFlag = true; // First time, input is "gotten"

    bool InitSetPointManagersOneTimeFlag = true;
    bool InitSetPointManagersOneTimeFlag2 = true;
    // This will cause a warning to be issued if no "surface" ground temperature object was input.

    std::array<bool, (int)DataEnvironment::GroundTempType::Num> NoGroundTempObjWarning = {true, true, true, true}; 
    bool InitSetPointManagersMyEnvrnFlag = true; // flag for init once at start of environment

    bool RunSubOptCondEntTemp = false;
    bool RunFinalOptCondEntTemp = false;

    // Object Data
    Array1D<SetPointManager::SPMBase *> spms;
    std::map<std::string, int> spmMap;
        
    // Real64 CondWaterSetPoint = 0; // Condenser entering water temperature setpoint this timestep, C

        // Real64 EvapOutletTemp = 0;    // Evaporator water outlet temperature (C)
        // Real64 CondTempLimit = 0;     // Condenser entering water temperature setpoint lower limit
        // Real64 CurLoad = 0;           // Current cooling load, W
        // Real64 TotEnergy = 0;         // Total energy consumptions at this time step
        // Real64 TotEnergyPre = 0;      // Total energy consumptions at the previous time step

    Real64 ActualLoadSum = 0.0;
    Real64 DesignLoadSum = 0.0;
    Real64 WeightedActualLoadSum = 0.0;
    Real64 WeightedDesignLoadSum = 0.0;

    void clear_state() override
    {
        GetSetPointManagerInputMaxNumAlphas = 0;  // argument for call to GetObjectDefMaxArgs
        GetSetPointManagerInputMaxNumNumbers = 0; // argument for call to GetObjectDefMaxArgs

        ManagerOn = false;
        GetInputFlag = true; // First time, input is "gotten"
        // Object Data
        InitSetPointManagersOneTimeFlag = true;
        InitSetPointManagersOneTimeFlag2 = true;

        for (int iSPM = 1; iSPM <= (int)spms.size(); ++iSPM) {
            delete spms[iSPM];
        }
        spms.deallocate();
        spmMap.clear();

        NoGroundTempObjWarning = {true, true, true, true};
        InitSetPointManagersMyEnvrnFlag = true;
        RunSubOptCondEntTemp = false;
        RunFinalOptCondEntTemp = false;

        // CondWaterSetPoint = 0;
        // EvapOutletTemp = 0;
        // CondTempLimit = 0;
        // CurLoad = 0;
        // TotEnergy = 0;
        // TotEnergyPre = 0;
    }
};

} // namespace EnergyPlus

#endif
