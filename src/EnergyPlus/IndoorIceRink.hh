// EnergyPlus, Copyright (c) 1996-2019, The Board of Trustees of the University of Illinois,
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

#ifndef IndoorIceRink_hh_INCLUDED
#define IndoorIceRink_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <DataGlobals.hh>
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace IceRink {

    // Using/Aliasing

    // Data
    // MODULE PARAMETER DEFINITIONS:
    // na

    // MODULE VARIABLE DECLARATIONS:
    // Standard, run-of-the-mill variables...

    extern int const DirectSystem;
    extern int const IndirectSystem;
    // Control types:
    extern int const SurfaceTempControl;     // Controls system using ice surface temperature
    extern int const BrineOutletTempControl; // Controls system using brine outlet temperature
    // MODULE VARIABLE DECLARATIONS:
    extern int NumIceRinks; // Number of Ice Rinks
    extern Array1D_bool CheckEquipName;
    extern int NumOfDirectSystem;    // Number of direct refrigeration systems
    extern int NumOfIndirectSystem;  // Number of direct refrigeration systems
    extern int TotalNumRefrigSystem; // Total number of refrigeration systems
    // Operating Modes:
    extern int OperatingMode; // Variable that keeps track of the operating mode of the refrigeration system
    extern int CoolingMode;   // Indicates that the refrigeration system is in cooling mode
    extern int NotOperating;  // Indicates that the refrigeration system is not operating

    extern Array1D<Real64> QRadSysSrcAvg; // Average source over the time step for a particular radiant surface
    // Record keeping variables used to calculate QRadSysSrcAvg locally
    extern Array1D<Real64> LastQRadSysSrc;     // Need to keep the last value in case we are still iterating
    extern Array1D<Real64> LastSysTimeElapsed; // Need to keep the last value in case we are still iterating
    extern Array1D<Real64> LastTimeStepSys;    // Need to keep the last value in case we are still iterating
    // Autosizing variables
    extern Array1D_bool MySizeFlagDirSys;
    extern Array1D_bool MySizeFlagIndirSys;

    // DERIVED TYPE DEFINITIONS:

    // MODULE VARIABLE DECLARATIONS:

    // SUBROUTINE SPECIFICATIONS FOR MODULE IndoorIceRink
    // Type:

    struct DirectRefrigSysData
    {
        // Members
        // Input data
        std::string Name;                // name of direct refrigeration system
        std::string SchedName;           // availability schedule
        int SchedPtr;                    // index to schedule
        std::string ZoneName;            // Name of zone the system is serving
        int ZonePtr;                     // Point to this zone in the Zone derived type
        int NumOfSurfaces;               // Number of surfaces included in this radiant system (coordinated control)
        Array1D_int SurfacePtr2;         // Pointer to the surface(s) in the Surface derived type
        int SurfacePtr;                  // Pointer to the surface in the Surface derived type, for ice rink it is the floor
        std::string SurfaceName;         // Name of surface that are the ice rink system  is embedded.
        Array1D<Real64> SurfaceFlowFrac; // Fraction of flow/pipe length for a particular surface
        Array1D<Real64> NumCircuits;     // Number of fluid circuits in the surface
        Real64 LengthRink;               // Length of the ice rink
        Real64 WidthRink;                // Width of the ice rink
        Real64 DepthRink;                // Depth of the ice rink (NOTE: Total depth, including the concrete and soil layer beneath the ice rink
        Real64 TotalSurfaceArea;         // Total surface area for all surfaces that are part of this radiant system
        Real64 TubeDiameter;             // tube diameter for embedded tubing
        Real64 TubeLength;               // tube length embedded in radiant surface
        int ControlType;                 // Control type for the system (Ice Surface Temperature Control OR Brine Outlet Temperature Control)
        Real64 CondDewPtDeltaT;          // Diff between surface temperature and dew point for cond. shut-off
        Real64 CondCausedTimeOff;        // Amount of time condensation did or could have turned system off
        bool CondCausedShutDown;         // .TRUE. when condensation predicted at surface
        int NumCircs;                    // Number of fuild circuits
        Real64 RefrigVolFlowMaxCool;     // maximum refrigerant flow rate for cooling, m3/s
        Real64 RefrigFlowMaxCool;        // maximum refrigerant flow rate for cooling, kg/s
        bool CoolingSystem;              // .TRUE. when the system is able to cool (parameters are valid)
        int ColdRefrigInNode;            // cold refrigerant inlet node
        int ColdRefrigOutNode;           // cold refrigerant outlet node
        Real64 ColdThrottlRange;         // Throttling range for cooling [C]
        std::string ColdSetptSched;      // Schedule name for the zone setpoint temperature
        int ColdSetptSchedPtr;           // Schedule index for the zone setpoint temperature
        int CRefrigLoopNum;
        int CRefrigLoopSide;
        int CRefrigBranchNum;
        int CRefrigCompNum;
        bool HeatingSystem;        // .TRUE. when the system is able to cool (parameters are valid)
        int HotBrineInNode;        // cold water inlet node
        int HotBrineOutNode;       // cold water outlet node
        Real64 HotThrottlRange;    // Throttling range for cooling [C]
        std::string HotSetptSched; // Schedule name for the zone setpoint temperature
        int HotSetptSchedPtr;      // Schedule index for the zone setpoint temperature
        int HBLoopNum;
        int HBLoopSide;
        int HBBranchNum;
        int HBCompNum;
        std::string ResurfacingWaterSupplyName; // Name of resurfacing water source
        std::string ResurfacingWaterSchedName;  // Name of resurfacing water supply schedule
        int ResurfacingWaterSchedPtr;           // Index to schedule for resurfacing water water
        int GlycolIndex;                        // Index to Glycol Properties
        Real64 GlycolConc;                      // Concentration of the glycol.
        int CondErrIndex;                       // Error index for recurring warning messages
        int CondCtrlType;                       // Condensation control type (initialize to simple off)
        int NumCircCalcMethod;                  // Calculation method for number of circuits per surface; 1=1 per surface, 2=use cicuit length
        Real64 CircLength;                      // Circuit length {m}
        Real64 MaxNumOfPeople;                  // Number of people in the pool as defined by user input
        std::string PeopleSchedName;            // Name of people schedule
        int PeopleSchedPtr;                     // People schedule index
        std::string PeopleHeatGainSchedName;    // Name of people heat gain schedule
        int PeopleHeatGainSchedPtr;             // People heat gain schedule index
        Real64 PeopleHeatGain;                  // Current heat gain from people
        Real64 BrineMass;                       // Mass of brine in the indirect refrigeration system
        std::string ActivityFactorSchedName;    // Activity factor schedule name
        int ActivityFactorSchedPtr;             // Activity factor schedule pointer
        Real64 CurActivityFactor;               // Current activity factor value
        Real64 CurSetPtTemp;                    // Current ice rink floor setpoint temperature
        std::string SetPtTempSchedName;         // Schedule name for ice rink floor setpoint temperature
        int SetPtTempSchedPtr;                  // Schedule pointer for ice rink floor setpoint temperature

        // Report data
        Real64 RefrigInletTemp;    // Refrigerant inlet temperature, in C
        Real64 RefrigOutletTemp;   // Refrigerant outlet temperature, in C
        Real64 MiscEquipEnergy;    // energy for miscellaneous pool equipment in Joules
        Real64 MiscEquipPower;     // power for miscellaneous pool equipment in Watts
        Real64 RefrigMassFlowRate; // Brine mass flow rate
        Real64 CoolPower;          // cooling sent to panel in Watts
        Real64 CoolEnergy;         // cooling sent to panel in Joules
        int OutRangeLoErrorCount;  // recurring errors for crazy results too low fluid temperature
        int OutRangeHiErrorCount;
        int CoolingCapMethod;         // - Method for Low Temp Radiant system cooling capacity scaledsizing calculation (CoolingDesignCapacity,
                                      // CapacityPerFloorArea, FracOfAutosizedCoolingCapacity)
        Real64 ScaledCoolingCapacity; // -  Low Temp Radiant system scaled maximum cooling capacity {W} or scalable variable of zone HVAC equipment,
                                      // {-}, or {W/m2}

        // Default Constructor
        DirectRefrigSysData()
            : SchedPtr(0), ZonePtr(0), NumOfSurfaces(0), TotalSurfaceArea(0.0), TubeDiameter(0.0), TubeLength(0.0), NumCircs(0), ControlType(0),
              RefrigVolFlowMaxCool(0.0), RefrigFlowMaxCool(0.0), CoolingSystem(false), ColdRefrigInNode(0), ColdRefrigOutNode(0),
              ColdThrottlRange(0.0), ColdSetptSchedPtr(0), CRefrigLoopNum(0), CRefrigLoopSide(0), CRefrigBranchNum(0), CRefrigCompNum(0),
              GlycolIndex(0), CondErrIndex(0), CondCtrlType(1), NumCircCalcMethod(0), CircLength(0.0), RefrigInletTemp(0.0), RefrigOutletTemp(0.0),
              RefrigMassFlowRate(0.0), CoolPower(0.0), CoolEnergy(0.0), OutRangeHiErrorCount(0), OutRangeLoErrorCount(0), CoolingCapMethod(0),
              ScaledCoolingCapacity(0.0), CondCausedShutDown(false), CondDewPtDeltaT(1.0), CondCausedTimeOff(0.0), ActivityFactorSchedPtr(0),
              CurActivityFactor(0.0), CurSetPtTemp(0.0), SetPtTempSchedPtr(0)
        {
        }
    };
    struct IndirectRefrigSysData
    {
        // Members
        // Input data
        std::string Name;                // name of direct refrigeration system
        std::string SchedName;           // availability schedule
        int SchedPtr;                    // index to schedule
        std::string ZoneName;            // Name of zone the system is serving
        int ZonePtr;                     // Point to this zone in the Zone derived type
        int NumOfSurfaces;               // Number of surfaces included in this radiant system (coordinated control)
        Array1D_int SurfacePtr2;         // Pointer to the surface(s) in the Surface derived type
        int SurfacePtr;                  // Pointer to the surface in the Surface derived type, for ice rink it is the floor
        std::string SurfaceName;         // Name of surface that are the ice rink system  is embedded.
        Array1D<Real64> SurfaceFlowFrac; // Fraction of flow/pipe length for a particular surface
        Array1D<Real64> NumCircuits;     // Number of fluid circuits in the surface
        Real64 LengthRink;               // Length of the ice rink
        Real64 WidthRink;                // Width of the ice rink
        Real64 DepthRink;                // Depth of the ice rink (NOTE: Total depth, including the concrete and soil layer beneath the ice rink
        Real64 TotalSurfaceArea;         // Total surface area for all surfaces that are part of this radiant system
        Real64 TubeDiameter;             // tube diameter for embedded tubing
        Real64 TubeLength;               // tube length embedded in radiant surface
        int NumCircs;                    // Number of fuild circuits
        int ControlType;                 // Control type for the system (Ice Surface Temperature Control OR Brine Outlet Temperature Control)
        Real64 CondDewPtDeltaT;          // Diff between surface temperature and dew point for cond. shut-off
        Real64 RefrigVolFlowMaxCool;     // maximum refrigerant flow rate for cooling, m3/s
        Real64 BrineFlowMaxCool;         // maximum brine flow rate for cooling, kg/s
        bool CoolingSystem;              // .TRUE. when the system is able to cool (parameters are valid)
        int ColdBrineInNode;             // cold refrigerant inlet node
        int ColdBrineOutNode;            // cold refrigerant outlet node
        Real64 ColdThrottlRange;         // Throttling range for cooling [C]
        std::string ColdSetptSched;      // Schedule name for the zone setpoint temperature
        int ColdSetptSchedPtr;           // Schedule index for the zone setpoint temperature
        int CBLoopNum;
        int CBLoopSide;
        int CBBranchNum;
        int CBCompNum;
        bool HeatingSystem;        // .TRUE. when the system is able to cool (parameters are valid)
        int HotBrineInNode;        // cold water inlet node
        int HotBrineOutNode;       // cold water outlet node
        Real64 HotThrottlRange;    // Throttling range for cooling [C]
        std::string HotSetptSched; // Schedule name for the zone setpoint temperature
        int HotSetptSchedPtr;      // Schedule index for the zone setpoint temperature
        int HBLoopNum;
        int HBLoopSide;
        int HBBranchNum;
        int HBCompNum;
        std::string ResurfacingWaterSupplyName; // Name of resurfacing water source
        std::string ResurfacingWaterSchedName;  // Name of resurfacing water supply schedule
        int ResurfacingWaterSchedPtr;           // Index to schedule for resurfacing water water
        int GlycolIndex;                        // Index to Glycol Properties
        Real64 GlycolConc;                      // Concentration of the glycol.
        int CondErrIndex;                       // Error index for recurring warning messages
        int CondCtrlType;                       // Condensation control type (initialize to simple off)
        int NumCircCalcMethod;                  // Calculation method for number of circuits per surface; 1=1 per surface, 2=use cicuit length
        Real64 CircLength;                      // Circuit length {m}
        Real64 MaxNumOfPeople;                  // Number of people in the pool as defined by user input
        std::string PeopleSchedName;            // Name of people schedule
        int PeopleSchedPtr;                     // People schedule index
        std::string PeopleHeatGainSchedName;    // Name of people heat gain schedule
        int PeopleHeatGainSchedPtr;             // People heat gain schedule index
        Real64 PeopleHeatGain;                  // Current heat gain from people
        Real64 BrineMass;                       // Mass of brine in the indirect refrigeration system
        bool CondCausedShutDown;                // .TRUE. when condensation predicted at surface
        std::string ActivityFactorSchedName;    // Activity factor schedule name
        int ActivityFactorSchedPtr;             // Activity factor schedule pointer
        Real64 CurActivityFactor;               // Current activity factor value
        Real64 CurSetPtTemp;                    // Current ice rink floor setpoint temperature
        std::string SetPtTempSchedName;         // Schedule name for ice rink floor setpoint temperature
        int SetPtTempSchedPtr;                  // Schedule pointer for ice rink floor setpoint temperature

        // Report data
        Real64 BrineInletTemp;    // Refrigerant inlet temperature, in C
        Real64 BrineOutletTemp;   // Refrigerant outlet temperature, in C
        Real64 MiscEquipEnergy;   // energy for miscellaneous pool equipment in Joules
        Real64 MiscEquipPower;    // power for miscellaneous pool equipment in Watts
        Real64 BrineMassFlowRate; // Brine mass flow rate
        Real64 CoolPower;         // cooling sent to panel in Watts
        Real64 CoolEnergy;        // cooling sent to panel in Joules
        int OutRangeLoErrorCount; // recurring errors for crazy results too low fluid temperature
        int OutRangeHiErrorCount;
        int CoolingCapMethod;         // - Method for Low Temp Radiant system cooling capacity scaledsizing calculation (CoolingDesignCapacity,
                                      // CapacityPerFloorArea, FracOfAutosizedCoolingCapacity)
        Real64 ScaledCoolingCapacity; // -  Low Temp Radiant system scaled maximum cooling capacity {W} or scalable variable of zone HVAC equipment,
                                      // {-}, or {W/m2}

        // Default Constructor
        IndirectRefrigSysData()
            : SchedPtr(0), ZonePtr(0), NumOfSurfaces(0), TotalSurfaceArea(0.0), TubeDiameter(0.0), TubeLength(0.0), NumCircs(0), ControlType(0),
              RefrigVolFlowMaxCool(0.0), BrineFlowMaxCool(0.0), CoolingSystem(false), ColdBrineInNode(0), ColdBrineOutNode(0), ColdThrottlRange(0.0),
              ColdSetptSchedPtr(0), CBLoopNum(0), CBLoopSide(0), CBBranchNum(0), CBCompNum(0), GlycolIndex(0), CondErrIndex(0), CondCtrlType(1),
              NumCircCalcMethod(0), CircLength(0.0), BrineInletTemp(0.0), BrineOutletTemp(0.0), BrineMassFlowRate(0.0), CoolPower(0.0),
              CoolEnergy(0.0), OutRangeHiErrorCount(0), OutRangeLoErrorCount(0), CoolingCapMethod(0), ScaledCoolingCapacity(0.0),
              CondCausedShutDown(false), CondDewPtDeltaT(1.0), ActivityFactorSchedPtr(0), CurActivityFactor(0.0), CurSetPtTemp(0.0),
              SetPtTempSchedPtr(0)
        {
        }
    };

    struct RefrigSysTypeData
    {
        // Members
        // This type used to track different components/types for efficiency
        std::string Name; // name of refrigeartion system
        int SystemType;   // Type of System (see System Types in Parameters)
        int CompIndex;    // Index in specific system types

        // Default Constructor
        RefrigSysTypeData() : SystemType(0), CompIndex(0)
        {
        }
    };

    struct ResurfacerData
    {
        // Members
        int GlycolIndex;
        int ResurfacingSchedPtr;
        Real64 ResurfacingWaterTemp;
        // Report Data
        Real64 QResurfacing;
        Real64 EHeatingWater;
        Real64 QHumidity;
        // Default Constructor
        ResurfacerData() : GlycolIndex(0), ResurfacingSchedPtr(0), ResurfacingWaterTemp(0.0), QResurfacing(0.0), EHeatingWater(0.0), QHumidity(0.0)
        {
        }
    };

    struct DirectRefrigSysNumericFieldData
    {
        // Members
        Array1D_string FieldNames;

        // Default Constructor
        DirectRefrigSysNumericFieldData()
        {
        }
    };

    struct IndirectRefrigSysNumericFieldData
    {
        // Members
        Array1D_string FieldNames;

        // Default Constructor
        IndirectRefrigSysNumericFieldData()
        {
        }
    };
    // Object Data
    extern Array1D<DirectRefrigSysData> DRink;
    extern Array1D<IndirectRefrigSysData> IRink;
    extern Array1D<RefrigSysTypeData> RadSysTypes;
    extern Array1D<ResurfacerData> Resurfacer;
    extern Array1D<DirectRefrigSysNumericFieldData> DirectRefrigSysNumericFields;
    extern Array1D<IndirectRefrigSysNumericFieldData> IndirectRefrigSysNumericFields;
    // Functions

    void clear_state();

    void SimIndoorIceRink(bool const FirstHVACIteration,
                          std::string const &CompName, // name of the refrigeration system
                          int &CompIndex,              // Index to the refrigeration system
                          int SysNum,
                          Real64 &LoadMet); // Index to the refrigeration system);
    void GetIndoorIceRink();

    void InitIndoorIceRink(bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
                           bool &InitErrorsFound,         // TRUE if some error is found in the initialization
                           int const SystemType,          // Type of refrigeration system: Direct or Indirect
                           int const SysNum);             // Index to the refrigeration system

    Real64 IceRinkResurfacer(Real64 ResurfacerTank_capacity,  // Resurfacing machine tank capacity
                             Real64 ResurfacingHWTemperature, // Temperature of flood water
                             Real64 IceSurfaceTemperature,    // Temperature of ice rink surface
                             Real64 InitResurfWaterTemp,      // Initial temperature of resurfacing water
                             int const ResurfacerIndex);

    Real64 SizeRefrigSysTubeLength(int const SystemType,  // Tye of refrigeration system, direct or indirect
                                   int const RefrigSysNum // index of the refrigeration system
    );

    void CalcDirectIndoorIceRinkComps(int const SysNum, // Index number for the direct refrigeration system
                                      Real64 &LoadMet);

    void CalcIndirectIndoorIceRinkSys();

    void CalcIndirectIndoorIceRinkComps(int const SysNum, // Index number for the indirect refrigeration system
                                        Real64 &LoadMet);

    Real64 STC(int const SystemType, int const SysNum);

    Real64 BOTC(int const SystemType, int const SysNum);

    Real64 CalcDRinkHXEffectTerm(Real64 const Temperature,    // Temperature of refrigerant entering the radiant system, in C
                                 int const SysNum,            // Index to the refrigeration system
                                 Real64 const RefrigMassFlow, // Mass flow rate of refrigerant in direct refrigeration system, kg/s
                                 Real64 TubeLength,
                                 Real64 TubeDiameter);

    Real64 CalcIRinkHXEffectTerm(Real64 const Temperature,    // Temperature of the refrigerant entering the radiant system
                                 int const SysNum,            // Index to the refrigeration system
                                 Real64 const RefrigMassFlow, // Mass flow rate of refrigerant in direct refrigeration system, kg/s
                                 Real64 TubeLength,           // Total length of the piping used in the radiant system
                                 Real64 TubeDiameter,         // Inner diameter of the piping used in the radiant system
                                 int const RefrigType, // Refrigerant used in the radiant system: Ethylene Glycol(EG) or Cslcium Chloride(CaCl2)
                                 Real64 Concentration  // Concentration of the brine(refrigerant) in the radiant system (allowed range 10% to 30%)
    );

    Real64 SumHATsurf(int const ZoneNum);

    void UpdateRadSysSourceValAvg(bool &LowTempRadSysOn);

    void UpdateIndoorIceRink(int const SysNum,    // index to the refrigeration system
                             int const SystemType // Type of refrigeration system: Direct or indirect
    );
    void ReportIndoorIceRink(int const SysNum,    // Index to the refrigeration system
                             int const SystemType // Type of refrigeration system: Direct or indirect
    );
} // namespace IceRink

} // namespace EnergyPlus

#endif
