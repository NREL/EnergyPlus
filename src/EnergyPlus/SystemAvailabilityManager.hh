// EnergyPlus, Copyright (c) 1996-2018, The Board of Trustees of the University of Illinois,
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

#ifndef SystemAvailabilityManager_hh_INCLUDED
#define SystemAvailabilityManager_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <DataGlobals.hh>
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace SystemAvailabilityManager {

    // Using/Aliasing

    // Data
    // MODULE PARAMETER DEFINITIONS
    extern int const MaxDayTypes;
    extern int const StayOff;
    extern int const CycleOnAny;
    extern int const CycleOnControlZone;
    extern int const ZoneFansOnly;

    // Cycling Run Time Control Type
    extern int const FixedRunTime;
    extern int const Thermostat;
    extern int const ThermostatWithMinimumRunTime;

    // Optimum start parameter definations
    extern int const ControlZone;
    extern int const MaximumOfZoneList;

    extern int const ConstantTemperatureGradient;
    extern int const AdaptiveTemperatureGradient;
    extern int const AdaptiveASHRAE;
    extern int const ConstantStartTime;

    // Hybrid Ventilation parameters
    extern int const HybridVentMode_No;       // No hybrid ventilation control
    extern int const HybridVentMode_Temp;     // Temperature control
    extern int const HybridVentMode_Enth;     // Enthalpy control
    extern int const HybridVentMode_DewPoint; // Dew point control
    extern int const HybridVentMode_OA;       // Outdoor air control
    extern int const HybridVentMode_OperT80;  // Operative temperature control with 80% acceptability limits
    extern int const HybridVentMode_OperT90;  // Operative temperature control with 90% acceptability limits
    extern int const HybridVentMode_CO2;      // CO2 control

    extern int const HybridVentCtrl_NoAction; // No hybrid ventilation control
    extern int const HybridVentCtrl_Open;     // Open windows or doors
    extern int const HybridVentCtrl_Close;    // Close windows or doors

    extern int const NumValidSysAvailManagerTypes;
    extern Array1D_string const cValidSysAvailManagerTypes;
    extern int const SysAvailMgr_Scheduled;
    extern int const SysAvailMgr_ScheduledOn;
    extern int const SysAvailMgr_ScheduledOff;
    extern int const SysAvailMgr_NightCycle;
    extern int const SysAvailMgr_DiffThermo;
    extern int const SysAvailMgr_HiTempTOff;
    extern int const SysAvailMgr_HiTempTOn;
    extern int const SysAvailMgr_LoTempTOff;
    extern int const SysAvailMgr_LoTempTOn;
    extern int const SysAvailMgr_NightVent;
    extern int const SysAvailMgr_HybridVent;

    extern int const SysAvailMgr_OptimumStart;
    extern Array1D_int const ValidSysAvailManagerTypes;
    // DERIVED TYPE DEFINITIONS

    // Not used yet

    // MODULE VARIABLE DECLARATIONS

    extern int NumSchedSysAvailMgrs;
    extern int NumSchedOnSysAvailMgrs;
    extern int NumSchedOffSysAvailMgrs;
    extern int NumNCycSysAvailMgrs;
    extern int NumDiffTSysAvailMgrs;
    extern int NumHiTurnOffSysAvailMgrs;
    extern int NumHiTurnOnSysAvailMgrs;
    extern int NumLoTurnOffSysAvailMgrs;
    extern int NumLoTurnOnSysAvailMgrs;
    extern int NumNVentSysAvailMgrs;
    extern int NumAvailManagerLists;
    extern bool GetAvailListsInput;
    extern bool GetAvailMgrInputFlag; // First time, input is "gotten"
    extern bool GetHybridInputFlag;   // Flag set to make sure you get input once
    extern int NumOptStartSysAvailMgrs;

    // SUBROUTINE SPECIFICATIONS FOR MODULE

    // Types

    struct DefineSchedSysAvailManager // Derived type for Scheduled Sys Avail Managers
    {
        // Members
        std::string Name; // Name of the manager object
        int MgrType;      // Integer equivalent of availability manager type
        int SchedPtr;     // Schedule pointer
        int AvailStatus;  // reports status of availability manager

        // Default Constructor
        DefineSchedSysAvailManager() : MgrType(0), SchedPtr(0), AvailStatus(0)
        {
        }
    };

    struct DefineSchedOnSysAvailManager // Derived type for Scheduled On Sys Avail Managers
    {
        // Members
        std::string Name; // Name of the manager object
        int MgrType;      // Integer equivalent of availability manager type
        int SchedPtr;     // Schedule pointer
        int AvailStatus;  // reports status of availability manager

        // Default Constructor
        DefineSchedOnSysAvailManager() : MgrType(0), SchedPtr(0), AvailStatus(0)
        {
        }
    };

    struct DefineSchedOffSysAvailManager // Derived type for Scheduled Off Sys Avail Managers
    {
        // Members
        std::string Name; // Name of the manager object
        int MgrType;      // Integer equivalent of availability manager type
        int SchedPtr;     // Schedule pointer
        int AvailStatus;  // reports status of availability manager

        // Default Constructor
        DefineSchedOffSysAvailManager() : MgrType(0), SchedPtr(0), AvailStatus(0)
        {
        }
    };

    struct DefineNightCycSysAvailManager // Derived type for Night Cycle Sys Avail Managers
    {
        // Members
        std::string Name;     // Name of the manager object
        int MgrType;          // Integer equivalent of availability manager type
        int SchedPtr;         // Applicability schedule pointer
        std::string FanSched; // Fan schedule name
        int FanSchedPtr;      // Fan schedule pointer
        int CtrlType;         // type of control: Stay Off, Cycle On Any,
        //   Cycle On Control Zone, or Cycle On Any - Zone Fans Only
        int CycRunTimeCntrlType;           // Cycling Run Time Control Type
        Real64 TempTolRange;               // range in degrees C of thermostat tolerance
        int CyclingTimeSteps;              // period (in Loads time steps) system will cycle on.
        int AvailStatus;                   // reports status of availability manager
        int PriorAvailStatus;              // prior status of availability manager
        std::string CtrlZoneListName;      // controlled zone or zonelist name
        int NumOfCtrlZones;                // number of controlled zones
        Array1D_int CtrlZonePtrs;          // pointers to controlled zone(s)
        std::string CoolingZoneListName;   // coolin zone or zonelist name
        int NumOfCoolingZones;             // number of cooling zones
        Array1D_int CoolingZonePtrs;       // pointers to cooling zone(s)
        std::string HeatingZoneListName;   // heatig zone or zonelist name
        int NumOfHeatingZones;             // number of heatig zones
        Array1D_int HeatingZonePtrs;       // pointers to heating zone(s)
        std::string HeatZnFanZoneListName; // heating zone fans only zone or zonelist name
        int NumOfHeatZnFanZones;           // number of heating zone fans only zones
        Array1D_int HeatZnFanZonePtrs;     // pointers to heating zone fans only zone(s)

        // Default Constructor
        DefineNightCycSysAvailManager()
            : MgrType(0), SchedPtr(0), FanSchedPtr(0), CtrlType(0), CycRunTimeCntrlType(0), TempTolRange(1.0), CyclingTimeSteps(1), AvailStatus(0),
              PriorAvailStatus(0), NumOfCtrlZones(0), NumOfCoolingZones(0), NumOfHeatingZones(0), NumOfHeatZnFanZones(0)
        {
        }
    };

    struct DefineOptStartSysAvailManager // Derived type for Optimal Start Sys Avail Managers
    {
        // Members
        std::string Name;         // Name of the manager object
        int MgrType;              // Integer equivalent of availability manager type
        bool isSimulated;         // true after availability manager is simulated
        int SchedPtr;             // Applicability schedule pointer
        std::string FanSched;     // Fan schedule name
        int FanSchedPtr;          // Fan schedule pointer
        int CtrlType;             // Type of control: Stay Off, ControlZone, MaximumofZoneList
        std::string CtrlZoneName; // Name of the control zone
        int ZoneNum;              // zone number of control zone
        int ControlledZoneNum;    // controlled zone number of control zone
        std::string ZoneListName; // Zone List name
        int NumOfZones;           // Number of zones in the list
        Array1D_int ZonePtrs;     // Pointers to zones in the list
        Real64 MaxOptStartTime;   // Maximum value of start time in hours
        int CtrlAlgType;          // Control algorithm: ConstantTemperatureGradient,
        // AdaptiveTemperatureGradient, AdaptiveASHRAE, ConstantStartTime
        Real64 ConstTGradCool;    // Constant temperature gradient in cooling mode, unit: degC per hour
        Real64 ConstTGradHeat;    // Constant temperature gradient in heating mode, unit: degC per hour
        Real64 InitTGradCool;     // Initial value for temperature gradient in cooling mode, unit: degC per hour
        Real64 InitTGradHeat;     // Initial value for temperature gradient in heating mode, unit: degC per hour
        Real64 AdaptiveTGradCool; // Calculated adaptive temperature gradient in cooling mode, unit: degC per hour
        Real64 AdaptiveTGradHeat; // Calculated adaptive temperature gradient in heating mode, unit: degC per hour
        Real64 ConstStartTime;    // Constant start time in hours
        int NumPreDays;           // Number of previous days for adaptive control
        int AvailStatus;          // reports status of availability manager
        Real64 NumHoursBeforeOccupancy;
        Real64 TempDiffHi;   // temperature difference for cooling mode
        Real64 TempDiffLo;   // temperature difference for heating mode
        int ATGWCZoneNumLo;  // zone index for worst case heating zone
        int ATGWCZoneNumHi;  // zone index for worst case cooling zone
        bool CycleOnFlag;    // Tracks when air loop has cycled on
        bool ATGUpdateFlag1; // updates
        bool ATGUpdateFlag2;
        bool FirstTimeATGFlag;
        bool OverNightStartFlag; // Flag to indicate the optimum start starts before mid night.
        bool OSReportVarFlag;
        Array1D<Real64> AdaTempGradTrdHeat; // Heating temp gradient for previous days
        Array1D<Real64> AdaTempGradTrdCool; // Cooling temp gradient for previous days
        Real64 AdaTempGradHeat;
        Real64 AdaTempGradCool;
        Real64 ATGUpdateTime1;
        Real64 ATGUpdateTime2;
        Real64 ATGUpdateTemp1;
        Real64 ATGUpdateTemp2;

        // Default Constructor
        DefineOptStartSysAvailManager()
            : MgrType(0), isSimulated(false), SchedPtr(0), FanSchedPtr(0), CtrlType(0), ZoneNum(0), ControlledZoneNum(0), NumOfZones(0),
              MaxOptStartTime(6.0), CtrlAlgType(0), ConstTGradCool(1.0), ConstTGradHeat(1.0), InitTGradCool(1.0), InitTGradHeat(1.0),
              AdaptiveTGradCool(1.0), AdaptiveTGradHeat(1.0), ConstStartTime(2.0), NumPreDays(1), AvailStatus(0), NumHoursBeforeOccupancy(0.0),
              TempDiffHi(0.0), TempDiffLo(0.0), ATGWCZoneNumLo(0), ATGWCZoneNumHi(0), CycleOnFlag(false), ATGUpdateFlag1(false),
              ATGUpdateFlag2(false), FirstTimeATGFlag(true), OverNightStartFlag(false), OSReportVarFlag(false), AdaTempGradHeat(0.0),
              AdaTempGradCool(0.0), ATGUpdateTime1(0.0), ATGUpdateTime2(0.0), ATGUpdateTemp1(0.0), ATGUpdateTemp2(0.0)
        {
        }

        void SetOptStartFlag(int const AirLoopNum);
    };

    struct DefineASHRAEAdaptiveOptimumStartCoeffs // Derived type for Differential Thermostat Sys Avail Managers
    {
        // Members
        std::string Name; // Name of the object
        Real64 Coeff1;    // 1st Coefficient of the equation
        Real64 Coeff2;    // 2nd Coefficient of the equation
        Real64 Coeff3;    // 3rd Coefficient of the equation
        Real64 Coeff4;    // 4th Coefficient of the equation

        // Default Constructor
        DefineASHRAEAdaptiveOptimumStartCoeffs() : Coeff1(0.0), Coeff2(0.0), Coeff3(0.0), Coeff4(0.0)
        {
        }
    };

    struct DefineDiffTSysAvailManager // Derived type for Differential Thermostat Sys Avail Managers
    {
        // Members
        std::string Name;   // Name of the manager object
        int MgrType;        // Integer equivalent of availability manager type
        int HotNode;        // "Hot" sensor node
        int ColdNode;       // "Cold" sensor node
        Real64 TempDiffOn;  // Temperature difference for turn on (delta C)
        Real64 TempDiffOff; // Temperature difference for turn off (delta C)
        int AvailStatus;    // reports status of availability manager

        // Default Constructor
        DefineDiffTSysAvailManager() : MgrType(0), HotNode(0), ColdNode(0), TempDiffOn(0.0), TempDiffOff(0.0), AvailStatus(0)
        {
        }
    };

    struct DefineHiLoSysAvailManager // Derived type for High/Low Temperature On/Off Sys Avail Managers
    {
        // Members
        std::string Name; // Name of the manager object
        int MgrType;      // Integer equivalent of availability manager type
        int Node;         // Sensor node
        Real64 Temp;      // Temperature for on/off (C)
        int SchedPtr;     // Applicability schedule pointer
        int AvailStatus;  // reports status of availability manager

        // Default Constructor
        DefineHiLoSysAvailManager() : MgrType(0), Node(0), Temp(0.0), SchedPtr(0), AvailStatus(0)
        {
        }
    };

    struct DefineNightVentSysAvailManager
    {
        // Members
        std::string Name;          // Name of the manager object
        int MgrType;               // Integer equivalent of availability manager type
        int SchedPtr;              // Applicability schedule pointer
        std::string FanSched;      // Fan schedule name
        int FanSchedPtr;           // Fan schedule pointer
        std::string VentTempSched; // Ventilation temperature schedule
        int VentTempSchedPtr;      // Ventilation temperature schedule pointer
        Real64 VentDelT;           // Ventilation delta T [deltaC]
        Real64 VentTempLowLim;     // ventilation temperature low limit
        std::string CtrlZoneName;  // Name of the control zone
        int ZoneNum;               // zome number of control zone
        int ControlledZoneNum;     // controlled zone number of control zone
        Real64 VentFlowFrac;       // the night venting flow fraction
        int AvailStatus;           // reports status of availability manager

        // Default Constructor
        DefineNightVentSysAvailManager()
            : MgrType(0), SchedPtr(0), FanSchedPtr(0), VentTempSchedPtr(0), VentDelT(0.0), VentTempLowLim(0.0), ZoneNum(0), ControlledZoneNum(0),
              VentFlowFrac(0.0), AvailStatus(0)
        {
        }
    };

    struct DefineHybridVentSysAvailManager
    {
        // Members
        std::string Name;            // Name of the object
        int MgrType;                 // Integer equivalent of availability manager type
        std::string AirLoopName;     // Name of HVAC Air Loop
        int AirLoopNum;              // HVAC Air Loop number
        std::string ControlZoneName; // Controlled zone name
        int NodeNumOfControlledZone; // Controlled zone node number
        int ActualZoneNum;           // Actual zone number
        int ControlledZoneNum;       // Controlled zone number
        int ControlModeSchedPtr;     // Ventilation control mode schedule pointer
        int ControlMode;             // hybrid ventilation control mode
        int VentilationCtrl;         // Ventilation control type: Noaction, Close, Open
        Real64 MinOutdoorTemp;       // Minimum Outdoor Temperature [C]
        Real64 MaxOutdoorTemp;       // Maximum Outdoor Temperature [C]
        Real64 MinOutdoorEnth;       // Minimum Outdoor Enthalpy [J/kg]
        Real64 MaxOutdoorEnth;       // Maximum Outdoor Enthalpy [J/kg]
        Real64 MinOutdoorDewPoint;   // Minimum Outdoor Dew point temperature [C]
        Real64 MaxOutdoorDewPoint;   // Maximum Outdoor Dew Point Temperature [C]
        Real64 MaxWindSpeed;         // Maximum Wind speed [m/s]
        bool UseRainIndicator;       // Use WeatherFile Rain Indicators
        std::string MinOASched;      // Minimum Outdoor Ventilation Air Schedule Name
        int MinOASchedPtr;           // Minimum Outdoor Ventilation Air Schedule pointer
        int DewPointNoRHErrCount;    // Dewpoint control mode error count without a humidistat
        int DewPointNoRHErrIndex;    // Dewpoint control mode error index without a humidistat
        int DewPointErrCount;        // Dewpoint control mode error count without a valid humidistat
        int DewPointErrIndex;        // Dewpoint control mode error index without a valid humidistat
        int SingleHCErrCount;        // Temperature and enthalpy control mode error count
        // with a singleHeatingCooling setpoint
        int SingleHCErrIndex; // Temperature and enthalpy control mode error index
        // with a singleHeatingCooling setpoint
        int OpeningFactorFWS;                 // Opening factor modifier as a function of wind speed
        int ANControlTypeSchedPtr;            // AirflowNetwork control type schedule pointer
        int SimpleControlTypeSchedPtr;        // Simple airflow object control type schedule pointer
        int VentilationPtr;                   // Ventilation object name pointer
        int AvailStatus;                      // reports status of availability manager
        std::string VentilationName;          // Ventilation object name
        bool HybridVentMgrConnectedToAirLoop; // Flag to check whether hybrid ventilation
        // manager is connected to air loop
        bool SimHybridVentSysAvailMgr; // Set to false when a zone has two hybrid ventilation
        // managers, one with air loop and one without
        Real64 OperativeTemp;    // Zone air operative temperature [C]
        Real64 CO2;              // Zone air CO2 [ppm]
        Real64 MinOperTime;      // Minimum HVAC Operation Time [minutes]
        Real64 MinVentTime;      // Minimum Ventilation Time [minutes]
        Real64 TimeOperDuration; // Time duration with continuous HVAC operation [minutes]
        Real64 TimeVentDuration; // Time duration with continuous ventilation [minutes]
        Real64 minAdaTem;        // minimum adaptive temperature for adaptive temperature control [C]
        Real64 maxAdaTem;        // maximum adaptive temperature for adaptive temperature control [C]

        // Default Constructor
        DefineHybridVentSysAvailManager()
            : MgrType(0), AirLoopNum(0), NodeNumOfControlledZone(0), ActualZoneNum(0), ControlledZoneNum(0), ControlModeSchedPtr(0), ControlMode(0),
              VentilationCtrl(0), MinOutdoorTemp(-100.0), MaxOutdoorTemp(100.0), MinOutdoorEnth(0.1), MaxOutdoorEnth(300000.0),
              MinOutdoorDewPoint(-100.0), MaxOutdoorDewPoint(100.0), MaxWindSpeed(0.0), UseRainIndicator(true), MinOASchedPtr(0),
              DewPointNoRHErrCount(0), DewPointNoRHErrIndex(0), DewPointErrCount(0), DewPointErrIndex(0), SingleHCErrCount(0), SingleHCErrIndex(0),
              OpeningFactorFWS(0), ANControlTypeSchedPtr(0), SimpleControlTypeSchedPtr(0), VentilationPtr(0), AvailStatus(0),
              HybridVentMgrConnectedToAirLoop(true), SimHybridVentSysAvailMgr(false), OperativeTemp(0.0), CO2(0.0), MinOperTime(0.0),
              MinVentTime(0.0), TimeOperDuration(0.0), TimeVentDuration(0.0), minAdaTem(0.0), maxAdaTem(0.0)
        {
        }
    };

    struct SysAvailManagerList
    {
        // Members
        std::string Name; // Availability Manager List Name
        int NumItems;
        Array1D_string AvailManagerName;
        Array1D_string cAvailManagerType;
        Array1D_int AvailManagerType;

        // Default Constructor
        SysAvailManagerList() : NumItems(0)
        {
        }
    };

    // Object Data
    extern Array1D<DefineSchedSysAvailManager> SchedSysAvailMgrData;
    extern Array1D<DefineSchedOnSysAvailManager> SchedOnSysAvailMgrData;
    extern Array1D<DefineSchedOffSysAvailManager> SchedOffSysAvailMgrData;
    extern Array1D<DefineNightCycSysAvailManager> NCycSysAvailMgrData;
    extern Array1D<DefineDiffTSysAvailManager> DiffTSysAvailMgrData;
    extern Array1D<DefineHiLoSysAvailManager> HiTurnOffSysAvailMgrData;
    extern Array1D<DefineHiLoSysAvailManager> HiTurnOnSysAvailMgrData;
    extern Array1D<DefineHiLoSysAvailManager> LoTurnOffSysAvailMgrData;
    extern Array1D<DefineHiLoSysAvailManager> LoTurnOnSysAvailMgrData;
    extern Array1D<DefineNightVentSysAvailManager> NVentSysAvailMgrData;
    extern Array1D<DefineHybridVentSysAvailManager> HybridVentSysAvailMgrData;
    extern Array1D<SysAvailManagerList> SysAvailMgrListData;
    extern Array1D<DefineOptStartSysAvailManager> OptStartSysAvailMgrData;
    extern Array1D<DefineASHRAEAdaptiveOptimumStartCoeffs> ASHRAEOptSCoeffCooling;
    extern Array1D<DefineASHRAEAdaptiveOptimumStartCoeffs> ASHRAEOptSCoeffHeating;

    // Functions
    void clear_state();

    void ManageSystemAvailability();

    void GetSysAvailManagerInputs();

    void GetSysAvailManagerListInputs();

    void GetPlantAvailabilityManager(std::string const &AvailabilityListName, // name that should be an Availability Manager List Name
                                     int const Loop,                          // which loop this is
                                     int const NumPlantLoops,                 // Total number of plant loops
                                     bool &ErrorsFound                        // true if certain errors are detected here
    );

    void GetAirLoopAvailabilityManager(std::string const &AvailabilityListName, // name that should be an Availability Manager List Name
                                       int const Loop,                          // which loop this is
                                       int const NumAirLoops,                   // Total number of air loops
                                       bool &ErrorsFound                        // true if certain errors are detected here
    );

    void GetZoneEqAvailabilityManager(int const ZoneEquipType, // Type of ZoneHVAC:* component
                                      int const CompNum,       // Index of a particular ZoneHVAC:* component
                                      bool &ErrorsFound        // true if certain errors are detected here
    );

    void InitSysAvailManagers();

    void SimSysAvailManager(int const SysAvailType,
                            std::string const &SysAvailName,
                            int &SysAvailNum,
                            int const PriAirSysNum, // Primary Air System index. If being called for a ZoneHVAC:* component
                            int const PreviousStatus,
                            int &AvailStatus,
                            Optional_int_const ZoneEquipType = _, // Type of ZoneHVAC:* equipment component
                            Optional_int_const CompNum = _        // Index of ZoneHVAC:* equipment component
    );

    void CalcSchedSysAvailMgr(int const SysAvailNum, // number of the current scheduled system availability manager
                              int &AvailStatus       // System status indicator
    );

    void CalcSchedOnSysAvailMgr(int const SysAvailNum, // number of the current scheduled on system availability manager
                                int &AvailStatus       // System status indicator
    );

    void CalcSchedOffSysAvailMgr(int const SysAvailNum, // number of the current scheduled off system availability manager
                                 int &AvailStatus       // System status indicator
    );

    void CalcNCycSysAvailMgr(int const SysAvailNum,                // number of the current scheduled system availability manager
                             int const PriAirSysNum,               // number of the primary air system affected by this Avail. Manager
                             int &AvailStatus,                     // System status indicator
                             Optional_int_const ZoneEquipType = _, // Type of ZoneHVAC equipment component
                             Optional_int_const CompNum = _        // Index of ZoneHVAC equipment component
    );

    bool CoolingZoneOutOfTolerance(Array1D_int const ZonePtrList, // list of controlled zone pointers
                                   int const NumZones,            // number of zones in list
                                   Real64 const TempTolerance     // temperature tolerance
    );

    bool HeatingZoneOutOfTolerance(Array1D_int const ZonePtrList, // list of controlled zone pointers
                                   int const NumZones,            // number of zones in list
                                   Real64 const TempTolerance     // temperature tolerance
    );

    void CalcOptStartSysAvailMgr(int const SysAvailNum,                // number of the current scheduled system availability manager
                                 int const PriAirSysNum,               // number of the primary air system affected by this Avail. Manager
                                 int &AvailStatus,                     // System status indicator
                                 Optional_int_const ZoneEquipType = _, // Type of ZoneHVAC equipment component
                                 Optional_int_const CompNum = _        // Index of ZoneHVAC equipment component
    );

    void CalcNVentSysAvailMgr(int const SysAvailNum,               // number of the current scheduled system availability manager
                              int const PriAirSysNum,              // number of the primary air system affected by this Avail. Manager
                              int &AvailStatus,                    // System status indicator
                              Optional_int_const ZoneEquipType = _ // Type of zone equipment component
    );

    void CalcDiffTSysAvailMgr(int const SysAvailNum,    // Number of the current scheduled system availability manager
                              int const PreviousStatus, // System status for the previous timestep
                              int &AvailStatus          // System status indicator
    );

    void CalcHiTurnOffSysAvailMgr(int const SysAvailNum, // Number of the current scheduled system availability manager
                                  int &AvailStatus       // System status indicator
    );

    void CalcHiTurnOnSysAvailMgr(int const SysAvailNum, // Number of the current scheduled system availability manager
                                 int &AvailStatus       // System status indicator
    );

    void CalcLoTurnOffSysAvailMgr(int const SysAvailNum, // Number of the current scheduled system availability manager
                                  int &AvailStatus       // System status indicator
    );

    void CalcLoTurnOnSysAvailMgr(int const SysAvailNum, // Number of the current scheduled system availability manager
                                 int &AvailStatus       // System status indicator
    );

    int ValidateAndSetSysAvailabilityManagerType(std::string const &AvailMgrName); // name to validate

    void ManageHybridVentilation();

    void GetHybridVentilationInputs();

    void InitHybridVentSysAvailMgr();

    void CalcHybridVentSysAvailMgr(int const SysAvailNum,              // number of the current scheduled system availability manager
                                   Optional_int_const PriAirSysNum = _ // number of the primary air system affected by this Avail. Manager
    );

    bool GetHybridVentilationControlStatus(int const ZoneNum); // Index of zone

} // namespace SystemAvailabilityManager

} // namespace EnergyPlus

#endif
