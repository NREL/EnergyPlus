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

#ifndef SingleDuct_hh_INCLUDED
#define SingleDuct_hh_INCLUDED

#include <unordered_map>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

namespace SingleDuct {

    enum class Action
    {
        Normal,
        ReverseAction,
        ReverseActionWithLimits,
        HeatingActionNotUsed
    };
    enum class SysType
    {
        SingleDuctVAVReheat,
        SingleDuctConstVolReheat,
        SingleDuctConstVolNoReheat,
        SingleDuctVAVNoReheat,
        SingleDuctVAVReheatVSFan,
        SingleDuctCBVAVReheat,
        SingleDuctCBVAVNoReheat,
        Unknown
    };
    enum class HeatingCoilType : int
    {
        None,
        Gas,
        Electric,
        SimpleHeating,
        SteamAirHeating
    };
    enum class MinFlowFraction
    {
        Constant,
        Scheduled,
        Fixed,
        MinFracNotUsed
    };

    struct SingleDuctAirTerminalFlowConditions
    {
        // Members
        Real64 AirMassFlowRate;         // MassFlow through the Sys being Simulated [kg/Sec]
        Real64 AirMassFlowRateMaxAvail; // MassFlow through the Sys being Simulated [kg/Sec]
        Real64 AirMassFlowRateMinAvail; // MassFlow through the Sys being Simulated [kg/Sec]
        Real64 AirTemp;                 // (C)
        Real64 AirHumRat;               // (Kg/Kg)
        Real64 AirEnthalpy;             // (J/Kg)

        // Default Constructor
        SingleDuctAirTerminalFlowConditions()
            : AirMassFlowRate(0.0), AirMassFlowRateMaxAvail(0.0), AirMassFlowRateMinAvail(0.0), AirTemp(0.0), AirHumRat(0.0), AirEnthalpy(0.0)
        {
        }
    };

    struct SingleDuctAirTerminal
    {
        // Members
        int SysNum;                     // index to single duct air terminal unit
        std::string SysName;            // Name of the Sys
        std::string SysType;            // Type of Sys ie. VAV, Mixing, Inducing, etc.
        enum SysType SysType_Num;       // Numeric Equivalent for System type
        std::string Schedule;           // Sys Operation Schedule
        int SchedPtr;                   // Pointer to the correct schedule
        std::string ReheatComp;         // Type of the Reheat Coil Object
        HeatingCoilType ReheatComp_Num; // Numeric Equivalent in this module for Coil type
        int ReheatComp_Index;           // Returned Index number from other routines
        std::string ReheatName;         // name of reheat coil
        int ReheatComp_PlantType;       // typeOf_ number for plant type of heating coil
        std::string FanType;            // Type of the Fan Object
        int Fan_Num;                    // Numeric Equivalent in this module for fan type
        int Fan_Index;                  // Returned Index number from other routines
        int ControlCompTypeNum;
        int CompErrIndex;
        std::string FanName;                  // name of fan
        Real64 MaxAirVolFlowRate;             // Max Specified Volume Flow Rate of Sys (cooling max) [m3/sec]
        Real64 AirMassFlowRateMax;            // Max Specified Mass Flow Rate of Sys (cooling max) [kg/sec]
        Real64 MaxHeatAirVolFlowRate;         // Max specified volume flow rate of unit at max heating [m3/s]
        Real64 HeatAirMassFlowRateMax;        // Max Specified Mass Flow Rate of unit at max heating [kg/sec]
        MinFlowFraction ZoneMinAirFracMethod; // parameter for what method is used for min flow fraction
        Real64 ZoneMinAirFracDes;             // Fraction of supply air used as design minimum flow
        Real64 ZoneMinAirFrac;                // Fraction of supply air used as current minimum flow
        Real64 ZoneMinAirFracReport;          // Fraction of supply air used as minimum flow for reporting (zero if terminal unit flow is zero)
        Real64 ZoneFixedMinAir;               // Absolute minimum supply air flow
        int ZoneMinAirFracSchPtr;             // pointer to the schedule for min flow fraction
        bool ConstantMinAirFracSetByUser;     // record if user left field blank for constant min fraction.
        bool FixedMinAirSetByUser;            // record if user left field blank for constant min fraction.
        Real64 DesignMinAirFrac;              // store user entered constant min flow fract for design
        Real64 DesignFixedMinAir;             // store user entered constant min flow for design
        int InletNodeNum;                     // terminal unit inlet node number; damper inlet node number
        int OutletNodeNum;                    // damper outlet node number for VAV; unused by CV; coil air inlet node for VAV
        // fan outlet node, coil inlet node for VAV VS Fan
        int ReheatControlNode;        // hot water inlet node for heating coil
        int ReheatCoilOutletNode;     // outlet node for heating coil
        Real64 ReheatCoilMaxCapacity; // heating coil capacity, W
        int ReheatAirOutletNode;      // terminal unit outlet node; heating coil air outlet node
        Real64 MaxReheatWaterVolFlow; // m3/s
        Real64 MaxReheatSteamVolFlow; // m3/s
        Real64 MaxReheatWaterFlow;    // kg/s
        Real64 MaxReheatSteamFlow;    // kg/s
        Real64 MinReheatWaterVolFlow; // m3/s
        Real64 MinReheatSteamVolFlow; // m3/s
        Real64 MinReheatWaterFlow;    // kg/s
        Real64 MinReheatSteamFlow;    // kg/s
        Real64 ControllerOffset;
        Real64 MaxReheatTemp; // C
        bool MaxReheatTempSetByUser;
        Action DamperHeatingAction;
        Real64 DamperPosition;
        int ADUNum;                           // index of corresponding air distribution unit
        int FluidIndex;                       // Refrigerant index
        int ErrCount1;                        // iteration limit exceeded in Hot Water Flow Calc
        int ErrCount1c;                       // iteration limit exceeded in Hot Water Flow Calc - continue
        int ErrCount2;                        // bad iterations limits in hot water flow calc
        Real64 ZoneFloorArea;                 // Zone floor area
        int CtrlZoneNum;                      // Pointer to CtrlZone data structure
        int CtrlZoneInNodeIndex;              // which controlled zone inlet node number corresponds with this unit
        int ActualZoneNum;                    // Pointer to Zone data Structure
        Real64 MaxAirVolFlowRateDuringReheat; // Maximum vol flow during reheat
        Real64 MaxAirVolFractionDuringReheat; // Maximum vol flow fraction during reheat
        Real64 AirMassFlowDuringReheatMax;    // Maximum mass flow during reheat
        int ZoneOutdoorAirMethod;             // Outdoor air method
        Real64 OutdoorAirFlowRate;            // report variable for TU outdoor air flow rate
        bool NoOAFlowInputFromUser;           // avoids OA calculation if no input specified by user
        int OARequirementsPtr;                // - Index to DesignSpecification:OutdoorAir object
        int AirLoopNum;
        int HWLoopNum;                // plant topology, loop number
        int HWLoopSide;               // plant topology, loop side number
        int HWBranchIndex;            // plant topology, Branch number
        int HWCompIndex;              // plant topology, Component number
        std::string ZoneHVACUnitType; // type of Zone HVAC unit for air terminal mixer units
        std::string ZoneHVACUnitName; // name of Zone HVAC unit for air terminal mixer units
        int SecInNode;                // zone or zone unit air node number
        // warning variables
        int IterationLimit;                  // Used for RegulaFalsi error -1
        int IterationFailed;                 // Used for RegulaFalsi error -2
        int OAPerPersonMode;                 // mode for how per person rates are determined, DCV or design.
        bool EMSOverrideAirFlow;             // if true, EMS is calling to override flow rate
        Real64 EMSMassFlowRateValue;         // value EMS is directing to use for flow rate [kg/s]
        int ZoneTurndownMinAirFracSchPtr;    // pointer to the schedule for turndown minimum airflow fraction
        Real64 ZoneTurndownMinAirFrac;       // turndown minimum airflow fraction value, multiplier of zone design minimum air flow
        bool ZoneTurndownMinAirFracSchExist; // if true, if zone turndown min air frac schedule exist
        bool MyEnvrnFlag;
        bool MySizeFlag;
        bool GetGasElecHeatCoilCap; // Gets autosized value of coil capacity
        bool PlantLoopScanFlag;     // plant loop scan flag, false if scanned
        Real64 MassFlow1;           // previous value of the terminal unit mass flow rate
        Real64 MassFlow2;           // previous value of the previous value of the mass flow rate
        Real64 MassFlow3;
        Real64 MassFlowDiff;
        SingleDuctAirTerminalFlowConditions sd_airterminalInlet;
        SingleDuctAirTerminalFlowConditions sd_airterminalOutlet;

        // Default Constructor
        SingleDuctAirTerminal()
            : SysNum(-1), SysType_Num(SysType::Unknown), SchedPtr(0), ReheatComp_Num(HeatingCoilType::None), ReheatComp_Index(0),
              ReheatComp_PlantType(0), Fan_Num(0), Fan_Index(0), ControlCompTypeNum(0), CompErrIndex(0), MaxAirVolFlowRate(0.0),
              AirMassFlowRateMax(0.0), MaxHeatAirVolFlowRate(0.0), HeatAirMassFlowRateMax(0.0), ZoneMinAirFracMethod(MinFlowFraction::Constant),
              ZoneMinAirFracDes(0.0), ZoneMinAirFrac(0.0), ZoneMinAirFracReport(0.0), ZoneFixedMinAir(0.0), ZoneMinAirFracSchPtr(0),
              ConstantMinAirFracSetByUser(false), FixedMinAirSetByUser(false), DesignMinAirFrac(0.0), DesignFixedMinAir(0.0), InletNodeNum(0),
              OutletNodeNum(0), ReheatControlNode(0), ReheatCoilOutletNode(0), ReheatCoilMaxCapacity(0.0), ReheatAirOutletNode(0),
              MaxReheatWaterVolFlow(0.0), MaxReheatSteamVolFlow(0.0), MaxReheatWaterFlow(0.0), MaxReheatSteamFlow(0.0), MinReheatWaterVolFlow(0.0),
              MinReheatSteamVolFlow(0.0), MinReheatWaterFlow(0.0), MinReheatSteamFlow(0.0), ControllerOffset(0.0), MaxReheatTemp(0.0),
              MaxReheatTempSetByUser(false), DamperHeatingAction(Action::HeatingActionNotUsed), DamperPosition(0.0), ADUNum(0), FluidIndex(0),
              ErrCount1(0), ErrCount1c(0), ErrCount2(0), ZoneFloorArea(0.0), CtrlZoneNum(0), CtrlZoneInNodeIndex(0), ActualZoneNum(0),
              MaxAirVolFlowRateDuringReheat(0.0), MaxAirVolFractionDuringReheat(0.0), AirMassFlowDuringReheatMax(0.0), ZoneOutdoorAirMethod(0),
              OutdoorAirFlowRate(0.0), NoOAFlowInputFromUser(true), OARequirementsPtr(0), AirLoopNum(0), HWLoopNum(0), HWLoopSide(0),
              HWBranchIndex(0), HWCompIndex(0), SecInNode(0), IterationLimit(0), IterationFailed(0), OAPerPersonMode(0), EMSOverrideAirFlow(false),
              EMSMassFlowRateValue(0.0), ZoneTurndownMinAirFracSchPtr(0), ZoneTurndownMinAirFrac(1.0), ZoneTurndownMinAirFracSchExist(false),
              MyEnvrnFlag(true), MySizeFlag(true), GetGasElecHeatCoilCap(true), PlantLoopScanFlag(true), MassFlow1(0.0), MassFlow2(0.0),
              MassFlow3(0.0), MassFlowDiff(0.0)
        {
        }

        void InitSys(EnergyPlusData &state, bool FirstHVACIteration);

        void SizeSys(EnergyPlusData &state);

        void SimVAV(EnergyPlusData &state, bool FirstHVACIteration, int ZoneNum, int ZoneNodeNum);

        void CalcOAMassFlow(EnergyPlusData &state, Real64 &SAMassFlow, Real64 &AirLoopOAFrac) const;

        void SimCBVAV(EnergyPlusData &state, bool FirstHVACIteration, int ZoneNum, int ZoneNodeNum);

        void SimVAVVS(EnergyPlusData &state, bool FirstHVACIteration, int ZoneNum, int ZoneNodeNum);

        void SimConstVol(EnergyPlusData &state, bool FirstHVACIteration, int ZoneNum, int ZoneNodeNum);

        void CalcVAVVS(EnergyPlusData &state,
                       bool FirstHVACIteration,
                       int ZoneNode,
                       Real64 HWFlow,
                       Real64 HCoilReq,
                       int FanType,
                       Real64 AirFlow,
                       int FanOn,
                       Real64 &LoadMet);

        static Real64 VAVVSCoolingResidual(EnergyPlusData &state, Real64 SupplyAirMassFlow, Array1D<Real64> const &Par);

        static Real64 VAVVSHWNoFanResidual(EnergyPlusData &state, Real64 HWMassFlow, Array1D<Real64> const &Par);

        static Real64 VAVVSHWFanOnResidual(EnergyPlusData &state, Real64 SupplyAirMassFlow, Array1D<Real64> const &Par);

        static Real64 VAVVSHCFanOnResidual(EnergyPlusData &state, Real64 HeatingFrac, Array1D<Real64> const &Par);

        void SimConstVolNoReheat(EnergyPlusData &state);

        void CalcOutdoorAirVolumeFlowRate(EnergyPlusData &state);

        void UpdateSys(EnergyPlusData &state) const;

        void ReportSys(EnergyPlusData &state);
    };

    struct AirTerminalMixerData
    {
        // Members
        // Input data
        std::string Name;             // name of unit
        int MixerType;                // type of inlet mixer, 1 = inlet side, 2 = supply side
        int ZoneHVACUnitType;         // type of Zone HVAC unit. ZoneHVAC:WaterToAirHeatPump =1, ZoneHVAC:FourPipeFanCoil = 2
        std::string ZoneHVACUnitName; // name of Zone HVAC unit
        int SecInNode;                // secondary air inlet node number
        int PriInNode;                // primary air inlet node number
        int MixedAirOutNode;          // mixed air outlet node number
        int ZoneInletNode;            // zone inlet node that ultimately receives air from this mixer
        Real64 ZoneAirTemp;           // zone air in temp
        Real64 ZoneAirHumRat;         // zone air in hum rat
        Real64 ZoneAirEnthalpy;       // zone air in enthalpy
        Real64 ZoneAirPressure;       // zone air in pressure
        Real64 ZoneAirMassFlowRate;   // zone air in mass flow rate
        Real64 DOASTemp;              // DOAS air in temp
        Real64 DOASHumRat;            // DOAS air in hum rat
        Real64 DOASEnthalpy;          // DOAS air in enthalpy
        Real64 DOASPressure;          // DOAS air in pressure
        Real64 DOASMassFlowRate;      // DOAS air in mass flow rate
        Real64 MixedAirTemp;          // mixed air in temp
        Real64 MixedAirHumRat;        // mixed air in hum rat
        Real64 MixedAirEnthalpy;      // mixed air in enthalpy
        Real64 MixedAirPressure;      // mixed air in pressure
        Real64 MixedAirMassFlowRate;  // mixed air in mass flow rate
        Real64 MassFlowRateMaxAvail;  // maximum air mass flow rate allowed through component
        int ADUNum;                   // index of Air Distribution Unit
        int TermUnitSizingIndex;      // Pointer to TermUnitSizing and TermUnitFinalZoneSizing data for this terminal unit
        bool OneTimeInitFlag;         // true if one-time inits should be done
        bool OneTimeInitFlag2;        // true if more one-time inits should be done
        int ZoneEqNum;
        int CtrlZoneInNodeIndex; // which controlled zone inlet node number corresponds with this unit
        int ZoneNum;
        bool NoOAFlowInputFromUser;     // avoids OA calculation if no input specified by user
        int OARequirementsPtr;          // - Index to DesignSpecification:OutdoorAir object
        int AirLoopNum;                 // System sizing adjustments
        Real64 DesignPrimaryAirVolRate; // System sizing adjustments, filled from design OA spec using sizing mode flags.
        int OAPerPersonMode;            // mode for how per person rates are determined, DCV or design.
        bool printWarning;              // flag to print warnings only once
        // Default Constructor
        AirTerminalMixerData()
            : MixerType(0), ZoneHVACUnitType(0), SecInNode(0), PriInNode(0), MixedAirOutNode(0), ZoneInletNode(0), ZoneAirTemp(0.0),
              ZoneAirHumRat(0.0), ZoneAirEnthalpy(0.0), ZoneAirPressure(0.0), ZoneAirMassFlowRate(0.0), DOASTemp(0.0), DOASHumRat(0.0),
              DOASEnthalpy(0.0), DOASPressure(0.0), DOASMassFlowRate(0.0), MixedAirTemp(0.0), MixedAirHumRat(0.0), MixedAirEnthalpy(0.0),
              MixedAirPressure(0.0), MixedAirMassFlowRate(0.0), MassFlowRateMaxAvail(0.0), ADUNum(0), TermUnitSizingIndex(0), OneTimeInitFlag(true),
              OneTimeInitFlag2(true), ZoneEqNum(0), CtrlZoneInNodeIndex(0), ZoneNum(0), NoOAFlowInputFromUser(true), OARequirementsPtr(0),
              AirLoopNum(0), DesignPrimaryAirVolRate(0.0), OAPerPersonMode(0), printWarning(true)
        {
        }

        void InitATMixer(EnergyPlusData &state, bool FirstHVACIteration);
    };

    void
    SimulateSingleDuct(EnergyPlusData &state, std::string_view CompName, bool FirstHVACIteration, int ZoneNum, int ZoneNodeNum, int &CompIndex);

    void GetSysInput(EnergyPlusData &state);

    void GetHVACSingleDuctSysIndex(EnergyPlusData &state,
                                   std::string const &SDSName,
                                   int &SDSIndex,
                                   bool &ErrorsFound,
                                   Optional_string_const ThisObjectType = _,
                                   Optional_int DamperInletNode = _, // Damper inlet node number
                                   Optional_int DamperOutletNode = _ // Damper outlet node number
    );

    void SimATMixer(EnergyPlusData &state, std::string const &SysName, bool FirstHVACIteration, int &SysIndex);

    void GetATMixers(EnergyPlusData &state);

    void CalcATMixer(EnergyPlusData &state, int SysNum);

    void UpdateATMixer(EnergyPlusData &state, int SysNum);

    void GetATMixer(EnergyPlusData &state,
                    std::string const &ZoneEquipName, // zone unit name name
                    std::string &ATMixerName,         // air terminal mixer name
                    int &ATMixerNum,                  // air terminal mixer index
                    int &ATMixerType,                 // air teminal mixer type
                    int &ATMixerPriNode,              // air terminal mixer primary air node number
                    int &ATMixerSecNode,              // air terminal mixer secondary air node number
                    int &ATMixerOutNode,              // air terminal mixer outlet air node number
                    int const &ZoneEquipOutletNode    // zone equipment outlet node (used with inlet side mixers)
    );

    void SetATMixerPriFlow(EnergyPlusData &state,
                           int ATMixerNum,                               // Air terminal mixer index
                           Optional<Real64 const> PriAirMassFlowRate = _ // Air terminal mixer primary air mass flow rate [kg/s]
    );

    void setATMixerSizingProperties(EnergyPlusData &state,
                                    int const &inletATMixerIndex, // index to ATMixer at inlet of zone equipment
                                    int const &controlledZoneNum, // controlled zone number
                                    int const &curZoneEqNum       // current zone equipment being simulated
    );

} // namespace SingleDuct

struct SingleDuctData : BaseGlobalStruct
{

    Array1D<SingleDuct::AirTerminalMixerData> SysATMixer;
    Array1D<SingleDuct::SingleDuctAirTerminal> sd_airterminal;
    std::unordered_map<std::string, std::string> SysUniqueNames;
    Array1D_bool CheckEquipName;
    int NumATMixers = 0;
    int NumConstVolSys = 0;
    int NumSDAirTerminal = 0;              // The Number of single duct air terminals found in the Input
    bool GetInputFlag = true;              // Flag set to make sure you get input once
    bool GetATMixerFlag = true;            // Flag set to make sure you get input once
    bool InitSysFlag = true;               // Flag set to make sure you do begin simulation initializaztions once
    bool InitATMixerFlag = true;           // Flag set to make sure you do begin simulation initializaztions once for mixer
    bool ZoneEquipmentListChecked = false; // True after the Zone Equipment List has been checked for items

    int SysNumGSI = 0;   // The Sys that you are currently loading input into
    int SysIndexGSI = 0; // The Sys that you are currently loading input into
    int NumVAVSysGSI = 0;
    int NumNoRHVAVSysGSI = 0;
    int NumVAVVSGSI = 0;
    int NumCBVAVSysGSI = 0;
    int NumNoRHCBVAVSysGSI = 0;
    int NumAlphasGSI = 0;
    int NumNumsGSI = 0;
    int NumCVNoReheatSysGSI = 0;
    int MaxNumsGSI = 0;   // Maximum number of numeric input fields
    int MaxAlphasGSI = 0; // Maximum number of alpha input fields
    int TotalArgsGSI = 0; // Total number of alpha and numeric arguments  = max for a
    Real64 CoilInTempSS = 0.0;
    Real64 DesCoilLoadSS = 0.0;
    Real64 DesZoneHeatLoadSS = 0.0;
    Real64 ZoneDesTempSS = 0.0;
    Real64 ZoneDesHumRatSS = 0.0;
    int CoilWaterInletNodeSS = 0;
    int CoilWaterOutletNodeSS = 0;
    int CoilSteamInletNodeSS = 0;
    int CoilSteamOutletNodeSS = 0;
    int DummyWaterIndexSS = 1;
    Real64 UserInputMaxHeatAirVolFlowRateSS = 0.0; // user input for MaxHeatAirVolFlowRate
    Real64 MinAirMassFlowRevActSVAV = 0.0;         // minimum air mass flow rate used in "reverse action" air mass flow rate calculation
    Real64 MaxAirMassFlowRevActSVAV = 0.0;         // maximum air mass flow rate used in "reverse action" air mass flow rate calculation
    Real64 ZoneTempSCBVAV = 0.0;                   // zone air temperature [C]
    Real64 MaxHeatTempSCBVAV = 0.0;                // maximum supply air temperature [C]
    Real64 MassFlowReqSCBVAV = 0.0;                // air mass flow rate required to meet the coil heating load [W]
    Real64 MassFlowActualSCBVAV = 0.0;             // air mass flow rate actually used [W]
    Real64 QZoneMaxSCBVAV = 0.0;                   // maximum zone heat addition rate given constraints of MaxHeatTemp and max        /
                                                   // available air mass flow rate [W]
    Real64 MinMassAirFlowSCBVAV = 0.0;             // the air flow rate during heating for normal acting damper
    Real64 QZoneMax2SCBVAV = 0.0;                  // temporary variable
    Real64 QZoneMax3SCBVAV = 0.0;                  // temporary variable
    Real64 TAirMaxSCV = 0.0;                       // Maximum zone supply air temperature [C]
    Real64 QMaxSCV = 0.0;                          // Maximum heat addition rate imposed by the max zone supply air temperature [W]
    Real64 ZoneTempSCV = 0.0;                      // Zone temperature [C]
    Real64 QMax2SCV = 0.0;
    int SysNumSATM = 0;
    Real64 PriMassFlowRateCATM = 0.0;
    Real64 PriEnthalpyCATM = 0.0;
    Real64 PriHumRatCATM = 0.0;
    Real64 PriTempCATM = 0.0;

    Real64 SecAirMassFlowRateCATM = 0.0;
    Real64 SecAirEnthalpyCATM = 0.0;
    Real64 SecAirHumRatCATM = 0.0;
    Real64 SecAirTempCATM = 0.0;

    Real64 MixedAirMassFlowRateCATM = 0.0;
    Real64 MixedAirEnthalpyCATM = 0.0;
    Real64 MixedAirHumRatCATM = 0.0;
    Real64 MixedAirTempCATM = 0.0;

    Real64 ZoneTempSDAT = 0.0;                      // zone air temperature [C]
    Real64 MaxHeatTempSDAT = 0.0;                   // maximum supply air temperature [C]
    Real64 MaxDeviceAirMassFlowReheatSDAT = 0.0;    // air mass flow rate required to meet the coil heating load [W]
    Real64 MassFlowReqToLimitLeavingTempSDAT = 0.0; // air mass flow rate actually used [W]
    Real64 QZoneMaxRHTempLimitSDAT = 0.0;           // maximum zone heat addition rate given constraints of MaxHeatTemp and max
                                                    // available air mass flow rate [W]
    Real64 MinMassAirFlowSDAT = 0.0;                // the air flow rate during heating for normal acting damper
    Real64 QZoneMax2SDAT = 0.0;                     // temporary variable

    void clear_state() override
    {
        this->SysATMixer.deallocate();
        this->sd_airterminal.deallocate();
        this->SysUniqueNames.clear();
        this->CheckEquipName.deallocate();
        this->NumATMixers = 0;
        this->NumConstVolSys = 0;
        this->NumSDAirTerminal = 0;
        this->GetInputFlag = true;
        this->GetATMixerFlag = true;
        this->InitSysFlag = true;
        this->InitATMixerFlag = true;
        this->ZoneEquipmentListChecked = false;

        this->SysNumGSI = 0;   // The Sys that you are currently loading input into
        this->SysIndexGSI = 0; // The Sys that you are currently loading input into
        this->NumVAVSysGSI = 0;
        this->NumNoRHVAVSysGSI = 0;
        this->NumVAVVSGSI = 0;
        this->NumCBVAVSysGSI = 0;
        this->NumNoRHCBVAVSysGSI = 0;
        this->NumAlphasGSI = 0;
        this->NumNumsGSI = 0;
        this->NumCVNoReheatSysGSI = 0;
        this->MaxNumsGSI = 0;   // Maximum number of numeric input fields
        this->MaxAlphasGSI = 0; // Maximum number of alpha input fields
        this->TotalArgsGSI = 0; // Total number of alpha and numeric arguments  = max for a
        this->CoilInTempSS = 0.0;
        this->DesCoilLoadSS = 0.0;
        this->DesZoneHeatLoadSS = 0.0;
        this->ZoneDesTempSS = 0.0;
        this->ZoneDesHumRatSS = 0.0;
        this->CoilWaterInletNodeSS = 0;
        this->CoilWaterOutletNodeSS = 0;
        this->CoilSteamInletNodeSS = 0;
        this->CoilSteamOutletNodeSS = 0;
        this->DummyWaterIndexSS = 1;
        this->UserInputMaxHeatAirVolFlowRateSS = 0.0; // user input for MaxHeatAirVolFlowRate
        this->MinAirMassFlowRevActSVAV = 0.0;         // minimum air mass flow rate used in "reverse action" air mass flow rate calculation
        this->MaxAirMassFlowRevActSVAV = 0.0;         // maximum air mass flow rate used in "reverse action" air mass flow rate calculation
        this->ZoneTempSCBVAV = 0.0;                   // zone air temperature [C]
        this->MaxHeatTempSCBVAV = 0.0;                // maximum supply air temperature [C]
        this->MassFlowReqSCBVAV = 0.0;                // air mass flow rate required to meet the coil heating load [W]
        this->MassFlowActualSCBVAV = 0.0;             // air mass flow rate actually used [W]
        this->QZoneMaxSCBVAV = 0.0;                   // maximum zone heat addition rate given constraints of MaxHeatTemp and max        /
        this->MinMassAirFlowSCBVAV = 0.0;             // the air flow rate during heating for normal acting damper
        this->QZoneMax2SCBVAV = 0.0;                  // temporary variable
        this->QZoneMax3SCBVAV = 0.0;                  // temporary variable
        this->TAirMaxSCV = 0.0;                       // Maximum zone supply air temperature [C]
        this->QMaxSCV = 0.0;                          // Maximum heat addition rate imposed by the max zone supply air temperature [W]
        this->ZoneTempSCV = 0.0;                      // Zone temperature [C]
        this->QMax2SCV = 0.0;
        this->SysNumSATM = 0;
        this->PriMassFlowRateCATM = 0.0;
        this->PriEnthalpyCATM = 0.0;
        this->PriHumRatCATM = 0.0;
        this->PriTempCATM = 0.0;
        this->SecAirMassFlowRateCATM = 0.0;
        this->SecAirEnthalpyCATM = 0.0;
        this->SecAirHumRatCATM = 0.0;
        this->SecAirTempCATM = 0.0;
        this->MixedAirMassFlowRateCATM = 0.0;
        this->MixedAirEnthalpyCATM = 0.0;
        this->MixedAirHumRatCATM = 0.0;
        this->MixedAirTempCATM = 0.0;

        this->ZoneTempSDAT = 0.0;                      // zone air temperature [C]
        this->MaxHeatTempSDAT = 0.0;                   // maximum supply air temperature [C]
        this->MaxDeviceAirMassFlowReheatSDAT = 0.0;    // air mass flow rate required to meet the coil heating load [W]
        this->MassFlowReqToLimitLeavingTempSDAT = 0.0; // air mass flow rate actually used [W]
        this->QZoneMaxRHTempLimitSDAT = 0.0;           // maximum zone heat addition rate given constraints of MaxHeatTemp and max
        this->MinMassAirFlowSDAT = 0.0;                // the air flow rate during heating for normal acting damper
        this->QZoneMax2SDAT = 0.0;                     // temporary variable
    }
};

} // namespace EnergyPlus

#endif
