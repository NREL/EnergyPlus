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

#ifndef Furnaces_hh_INCLUDED
#define Furnaces_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <DataGlobals.hh>
#include <EnergyPlus.hh>
#include <VariableSpeedCoils.hh>

namespace EnergyPlus {

namespace Furnaces {

    // Using/Aliasing
    using VariableSpeedCoils::MaxSpedLevels;

    // Data
    // MODULE PARAMETER DEFINITIONS
    // na

    // Last mode of operation
    extern int const CoolingMode; // last compressor operating mode was in cooling
    extern int const HeatingMode; // last compressor operating mode was in heating
    // Airflow control for contant fan mode
    extern int const UseCompressorOnFlow;  // set compressor OFF air flow rate equal to compressor ON air flow rate
    extern int const UseCompressorOffFlow; // set compressor OFF air flow rate equal to user defined value
    // Compressor operation
    extern int const On;  // normal compressor operation
    extern int const Off; // signal DXCoil that compressor shouldn't run

    // Dehumidification control modes (DehumidControlMode)
    extern int const DehumidControl_None;
    extern int const DehumidControl_Multimode;
    extern int const DehumidControl_CoolReheat;

    // DERIVED TYPE DEFINITIONS

    // MODULE VARIABLE DECLARATIONS:
    extern int NumFurnaces; // The number of furnaces found in the input data file
    extern Array1D_bool MySizeFlag;
    extern Array1D_bool CheckEquipName;
    extern Real64 ModifiedHeatCoilLoad; // used to adjust heating coil capacity if outlet temp > DesignMaxOutletTemp,
    // used for Coil:Gas:Heating and Coil:Electric:Heating coils only.
    extern Real64 OnOffAirFlowRatioSave;        // Saves the OnOffAirFlowRatio calculated in RegulaFalsi CALLs.
    extern Real64 OnOffFanPartLoadFractionSave; // Global part-load fraction passed to fan object
    extern Real64 CompOnMassFlow;               // Supply air mass flow rate w/ compressor ON [kg/s]
    extern Real64 CompOffMassFlow;              // Supply air mass flow rate w/ compressor OFF [kg/s]
    extern Real64 CompOnFlowRatio;              // fan flow ratio when coil on
    extern Real64 CompOffFlowRatio;             // fan flow ratio when coil off
    extern Real64 FanSpeedRatio;                // ratio of air flow ratio passed to fan object
    extern Real64 CoolHeatPLRRat;               // ratio of cooling to heating PLR, used for cycling fan RH control
    extern bool HeatingLoad;
    extern bool CoolingLoad;
    extern bool EconomizerFlag;             // holds air loop economizer status
    extern int AirLoopPass;                 // Number of air loop pass
    extern bool HPDehumidificationLoadFlag; // true if there is dehumidification load (heat pumps only)
    extern Real64 TempSteamIn;              // steam coil steam inlet temperature
    // starting add variables for variable speed water source heat pump
    extern Real64 SaveCompressorPLR;        // holds compressor PLR from active DX coil
    extern std::string CurrentModuleObject; // Object type for getting and error messages
    // ending varibles for variable speed water source heat pump

    // Subroutine Specifications for the Module
    // Driver/Manager Routines

    // Get Input routines for module

    // Initialization routines for module

    // Calculate routines to check convergence

    // Supporting routines for module

    // modules for variable speed heat pump

    // Reporting routines for module

    // Types

    struct FurnaceEquipConditions
    {
        // Members
        std::string Name;                   // Name of the Furnace
        int FurnaceType_Num;                // Numeric Equivalent for Furnace Type
        int FurnaceIndex;                   // Index to furnace
        int SchedPtr;                       // Index to furnace operating schedule
        int FanSchedPtr;                    // Index to fan operating mode schedule
        int FanAvailSchedPtr;               // Index to fan availability schedule
        int ControlZoneNum;                 // Index to controlled zone
        int ZoneSequenceCoolingNum;         // Index to cooling sequence/priority for this zone
        int ZoneSequenceHeatingNum;         // Index to heating sequence/priority for this zone
        int CoolingCoilType_Num;            // Numeric Equivalent for Cooling Coil Type
        int CoolingCoilIndex;               // Index to cooling coil
        int ActualDXCoilIndexForHXAssisted; // Index to DX cooling coil when HX assisted
        bool CoolingCoilUpstream;           // Indicates if cooling coil is upstream of heating coil
        int HeatingCoilType_Num;            // Numeric Equivalent for Heating Coil Type
        int HeatingCoilIndex;               // Index to heating coil
        int ReheatingCoilType_Num;          // Numeric Equivalent for Reheat Coil Type
        int ReheatingCoilIndex;             // Index to reheat coil
        std::string HeatingCoilName;        // name of heating coil
        std::string HeatingCoilType;        // type of heating coil
        int CoilControlNode;                // control node for hot water and steam heating coils
        int HWCoilAirInletNode;             // air inlet node number of HW coil for PTAC, PTHP, HeatCool, HeatOnly
        int HWCoilAirOutletNode;            // air outlet node number of HW coil for PTAC, PTHP, HeatCool, HeatOnly
        int SuppCoilAirInletNode;           // air inlet node number of HW coil for HeatCool Reheat Coil
        int SuppCoilAirOutletNode;          // air outlet node number of HW coil for HeatCool Reheat Coil
        int SuppHeatCoilType_Num;           // Numeric Equivalent for Supplemental Heat Coil Type
        int SuppHeatCoilIndex;              // Index to supplemental heater
        int SuppCoilControlNode;            // control node for steam and hot water heating coil
        std::string SuppHeatCoilName;       // name of supplemental heating coil
        std::string SuppHeatCoilType;       // type of supplemental heating coil
        int FanType_Num;                    // Integer equivalent of fan type (1=OnOff, 2 = ConstVolume)
        int FanIndex;                       // Index to fan object
        int FurnaceInletNodeNum;            // Furnace inlet node number
        int FurnaceOutletNodeNum;           // Furnace inlet node number
        int OpMode;                         // operation mode: 1 = cycling fan, cycling coils
        //                 2 = continuous fan, cycling coils
        int LastMode;                       // last mode of operation, coolingmode or heatingmode
        int AirFlowControl;                 // fan control mode, UseCompressorOnFlow or UseCompressorOffFlow
        int FanPlace;                       // fan placement; 1=blow through, 2=draw through
        int NodeNumOfControlledZone;        // Node number of controlled zone air node
        int WatertoAirHPType;               // Type of water to air heat pump model used
        Real64 CoolingConvergenceTolerance; // Convergence tolerance for cooling,
        //   ratio (CoolingCoilLoad - FurnaceCoolingOutput)/CoolingCoilLoad
        Real64 HeatingConvergenceTolerance; // Convergence tolerance for heating,
        //   ratio (HeatingCoilLoad - HeatPumpheatingOutput)/HeatingCoilLoad
        Real64 DesignHeatingCapacity;                   // Nominal Capacity of Heating Coil [W]
        Real64 DesignCoolingCapacity;                   // Nominal Capacity of Cooling Coil [W]
        Real64 CoolingCoilSensDemand;                   // Sensible demand on Cooling Coil [W]
        Real64 HeatingCoilSensDemand;                   // Sensible demand on Heating Coil [W]
        Real64 CoolingCoilLatentDemand;                 // Latent demand on Cooling Coil [W]
        Real64 DesignSuppHeatingCapacity;               // Nominal Capacity of Supplemental Heating Coil [W]
        Real64 DesignFanVolFlowRate;                    // Vol Flow through the Furnace being Simulated [m**3/Sec]
        bool DesignFanVolFlowRateEMSOverrideOn;         // if true, then EMS is calling to override autosize fan flow
        Real64 DesignFanVolFlowRateEMSOverrideValue;    // EMS value for override of fan flow rate autosize [m3/s]
        Real64 DesignMassFlowRate;                      // Design mass flow rate through furnace [kg/s]
        Real64 MaxCoolAirVolFlow;                       // supply air volumetric flow rate during cooling operation [m3/s]
        bool MaxCoolAirVolFlowEMSOverrideOn;            // if true, EMS is calling to override autosize flow during cooling
        Real64 MaxCoolAirVolFlowEMSOverrideValue;       // EMS value for override of flow during cooling [m3/s]
        Real64 MaxHeatAirVolFlow;                       // supply air volumetric flow rate during cooling operation [m3/s]
        bool MaxHeatAirVolFlowEMSOverrideOn;            // if true, EMS is calling to override autosize flow during heating
        Real64 MaxHeatAirVolFlowEMSOverrideValue;       // EMS value for override of flow during heating operation [m3/s]
        Real64 MaxNoCoolHeatAirVolFlow;                 // supply air volumetric flow rate when no cooling or heating [m3/s]
        bool MaxNoCoolHeatAirVolFlowEMSOverrideOn;      // if true, EMS is calling to override autosize no heatcool rate
        Real64 MaxNoCoolHeatAirVolFlowEMSOverrideValue; // EMS value for override of flow during no heat cool [m3/s]
        Real64 MaxCoolAirMassFlow;                      // supply air mass flow rate during cooling operation [kg/s]
        Real64 MaxHeatAirMassFlow;                      // supply air mass flow rate during heating operation [kg/s]
        Real64 MaxNoCoolHeatAirMassFlow;                // supply air mass flow rate when no cooling or heating [kg/s]
        Real64 MaxHeatCoilFluidFlow;                    // water or steam mass flow rate for heating coil [kg/s]
        Real64 MaxSuppCoilFluidFlow;                    // water or steam mass flow rate for supplemental heating coil [kg/s]
        Real64 ControlZoneMassFlowFrac;                 // Fraction of furnace flow to control zone
        Real64 DesignMaxOutletTemp;                     // Maximum supply air temperature from furnace heater [C]
        Real64 MdotFurnace;                             // Mass flow rate through furnace [kg/s]
        Real64 FanPartLoadRatio;                        // Part load ratio of furnace fan (mdot actual/mdot design)
        Real64 CompPartLoadRatio;                       // Part load ratio of furnace compressor (load / steady-state output)
        Real64 WSHPRuntimeFrac;                         // Runtime fraction of water source heat pump
        Real64 CoolPartLoadRatio;                       // Cooling part load ratio
        Real64 HeatPartLoadRatio;                       // Heating part load ratio
        Real64 MinOATCompressorCooling;                 // Minimum outdoor operating temperature for heat pump compressor
        Real64 MinOATCompressorHeating;                 // Minimum outdoor operating temperature for heat pump compressor
        Real64 MaxOATSuppHeat;                          // Maximum outdoor dry-bulb temperature for
        int CondenserNodeNum;                           // Node number of outdoor condenser/compressor
        Real64 MaxONOFFCyclesperHour;                   // Maximum ON/OFF Cycling Rate [cycles/hr]
        Real64 HPTimeConstant;                          // Heat Pump Time Constant [s]
        Real64 OnCyclePowerFraction;                    // Fraction of on-cycle power use [~]
        // supplemental heating coil operation
        Real64 FanDelayTime; // Fan delay time, time delay for the HP's fan to
        // shut off after compressor cycle off  [s]
        bool Humidistat;                        // Humidistat control (heatcool units only and not heatpump)
        bool InitHeatPump;                      // Heat pump initialization flag (for error reporting)
        int DehumidControlType_Num;             // 0 = None, 1=MultiMode, 2=CoolReheat
        int LatentMaxIterIndex;                 // Index to recurring warning message
        int LatentRegulaFalsiFailedIndex;       // Index to recurring warning message
        int LatentRegulaFalsiFailedIndex2;      // Index to recurring warning message
        int SensibleMaxIterIndex;               // Index to recurring warning message
        int SensibleRegulaFalsiFailedIndex;     // Index to recurring warning message
        int WSHPHeatMaxIterIndex;               // Index to recurring warning message
        int WSHPHeatRegulaFalsiFailedIndex;     // Index to recurring warning message
        int DXHeatingMaxIterIndex;              // Index to recurring warning message
        int DXHeatingRegulaFalsiFailedIndex;    // Index to recurring warning messages
        int HeatingMaxIterIndex;                // Index to recurring warning message
        int HeatingMaxIterIndex2;               // Index to recurring warning message
        int HeatingRegulaFalsiFailedIndex;      // Index to recurring warning messages
        Real64 ActualFanVolFlowRate;            // Volumetric flow rate from fan object
        Real64 HeatingSpeedRatio;               // Fan speed ratio in heating mode
        Real64 CoolingSpeedRatio;               // Fan speed ratio in cooling mode
        Real64 NoHeatCoolSpeedRatio;            // Fan speed ratio when no cooling or heating
        int ZoneInletNode;                      // Zone inlet node number in the controlled zone
        Real64 SenLoadLoss;                     // Air distribution system sensible loss
        Real64 LatLoadLoss;                     // Air distribution system latent loss
        Real64 SensibleLoadMet;                 // System sensible load
        Real64 LatentLoadMet;                   // System latent load
        Real64 DehumidInducedHeatingDemandRate; // Additional heating demand on supplemental heater
        // when heat pumps operate on dehumidification mode
        int CoilOutletNode;                   // outlet node for hot water and steam heating coil
        int LoopNum;                          // plant loop index for water and steam heating coil
        int LoopSide;                         // plant loop side  index for water and steam heating coil
        int BranchNum;                        // plant loop branch index for water and steam heating coil
        int CompNum;                          // plant loop component index for water and steam heating coil
        int SuppCoilOutletNode;               // outlet node for hot water and steam supplemental heating coil
        int LoopNumSupp;                      // plant loop index for water and steam supplemental heating coil
        int LoopSideSupp;                     // plant loop side  index for  water and steam supplemental heating coil
        int BranchNumSupp;                    // plant loop branch index for water and steam supplemental heating coil
        int CompNumSupp;                      // plant loop component index for water and steam supplemental heating coil
        int HotWaterCoilMaxIterIndex;         // Index to recurring warning message
        int HotWaterCoilMaxIterIndex2;        // Index to recurring warning message
        bool EMSOverrideSensZoneLoadRequest;  // if true, then EMS is calling to override zone load
        Real64 EMSSensibleZoneLoadValue;      // Value EMS is directing to use
        bool EMSOverrideMoistZoneLoadRequest; // if true, then EMS is calling to override zone load
        Real64 EMSMoistureZoneLoadValue;      // Value EMS is directing to use
        // starting added varibles for variable speed water source heat pump, Bo Shen, ORNL, March 2012
        int HeatCoolMode;                    // System operating mode (0 = floating, 1 = cooling, 2 = heating)
        int NumOfSpeedCooling;               // The number of speeds for cooling
        int NumOfSpeedHeating;               // The number of speeds for heating
        Real64 IdleSpeedRatio;               // idle air fan ratio
        Real64 IdleVolumeAirRate;            // idle air flow rate
        Real64 IdleMassFlowRate;             // idle air flow rate
        Real64 FanVolFlow;                   // fan volumetric flow rate
        bool CheckFanFlow;                   // Supply airflow check
        Array1D<Real64> HeatVolumeFlowRate;  // Supply air volume flow rate during heating operation
        Array1D<Real64> HeatMassFlowRate;    // Supply air mass flow rate during heating operation
        Array1D<Real64> CoolVolumeFlowRate;  // Supply air volume flow rate during cooling operation
        Array1D<Real64> CoolMassFlowRate;    // Supply air mass flow rate during cooling operation
        Array1D<Real64> MSHeatingSpeedRatio; // Fan speed ratio in heating mode
        Array1D<Real64> MSCoolingSpeedRatio; // Fan speed ratio in cooling mode
        bool bIsIHP;
        int CompSpeedNum;
        Real64 CompSpeedRatio;
        int ErrIndexCyc;
        int ErrIndexVar;
        // end of the additional variables for variable speed water source heat pump
        int WaterCyclingMode; // Heat Pump Coil water flow mode; See definitions in DataHVACGlobals,
        // 1=water cycling, 2=water constant, 3=water constant on demand (old mode)
        int iterationCounter;       // track time step iterations
        Array1D<int> iterationMode; // keep track of previous iteration mode (i.e., cooling or heating)
        bool FirstPass;             // used to determine when first call is made

        // Default Constructor
        FurnaceEquipConditions()
            : FurnaceType_Num(0), FurnaceIndex(0), SchedPtr(0), FanSchedPtr(0), FanAvailSchedPtr(0), ControlZoneNum(0), ZoneSequenceCoolingNum(0),
              ZoneSequenceHeatingNum(0), CoolingCoilType_Num(0), CoolingCoilIndex(0), ActualDXCoilIndexForHXAssisted(0), CoolingCoilUpstream(true),
              HeatingCoilType_Num(0), HeatingCoilIndex(0), ReheatingCoilType_Num(0), ReheatingCoilIndex(0), CoilControlNode(0), HWCoilAirInletNode(0),
              HWCoilAirOutletNode(0), SuppCoilAirInletNode(0), SuppCoilAirOutletNode(0), SuppHeatCoilType_Num(0), SuppHeatCoilIndex(0),
              SuppCoilControlNode(0), FanType_Num(0), FanIndex(0), FurnaceInletNodeNum(0), FurnaceOutletNodeNum(0), OpMode(0), LastMode(0),
              AirFlowControl(0), FanPlace(0), NodeNumOfControlledZone(0), WatertoAirHPType(0), CoolingConvergenceTolerance(0.0),
              HeatingConvergenceTolerance(0.0), DesignHeatingCapacity(0.0), DesignCoolingCapacity(0.0), CoolingCoilSensDemand(0.0),
              HeatingCoilSensDemand(0.0), CoolingCoilLatentDemand(0.0), DesignSuppHeatingCapacity(0.0), DesignFanVolFlowRate(0.0),
              DesignFanVolFlowRateEMSOverrideOn(false), DesignFanVolFlowRateEMSOverrideValue(0.0), DesignMassFlowRate(0.0), MaxCoolAirVolFlow(0.0),
              MaxCoolAirVolFlowEMSOverrideOn(false), MaxCoolAirVolFlowEMSOverrideValue(0.0), MaxHeatAirVolFlow(0.0),
              MaxHeatAirVolFlowEMSOverrideOn(false), MaxHeatAirVolFlowEMSOverrideValue(0.0), MaxNoCoolHeatAirVolFlow(0.0),
              MaxNoCoolHeatAirVolFlowEMSOverrideOn(false), MaxNoCoolHeatAirVolFlowEMSOverrideValue(0.0), MaxCoolAirMassFlow(0.0),
              MaxHeatAirMassFlow(0.0), MaxNoCoolHeatAirMassFlow(0.0), MaxHeatCoilFluidFlow(0.0), MaxSuppCoilFluidFlow(0.0),
              ControlZoneMassFlowFrac(0.0), DesignMaxOutletTemp(9999.0), MdotFurnace(0.0), FanPartLoadRatio(0.0), CompPartLoadRatio(0.0),
              WSHPRuntimeFrac(0.0), CoolPartLoadRatio(0.0), HeatPartLoadRatio(0.0), MinOATCompressorCooling(0.0), MinOATCompressorHeating(0.0),
              MaxOATSuppHeat(0.0), CondenserNodeNum(0), MaxONOFFCyclesperHour(0.0), HPTimeConstant(0.0), OnCyclePowerFraction(0.0), FanDelayTime(0.0),
              Humidistat(false), InitHeatPump(false), DehumidControlType_Num(0), LatentMaxIterIndex(0), LatentRegulaFalsiFailedIndex(0),
              LatentRegulaFalsiFailedIndex2(0), SensibleMaxIterIndex(0), SensibleRegulaFalsiFailedIndex(0), WSHPHeatMaxIterIndex(0),
              WSHPHeatRegulaFalsiFailedIndex(0), DXHeatingMaxIterIndex(0), DXHeatingRegulaFalsiFailedIndex(0), HeatingMaxIterIndex(0),
              HeatingMaxIterIndex2(0), HeatingRegulaFalsiFailedIndex(0), ActualFanVolFlowRate(0.0), HeatingSpeedRatio(1.0), CoolingSpeedRatio(1.0),
              NoHeatCoolSpeedRatio(1.0), ZoneInletNode(0), SenLoadLoss(0.0), LatLoadLoss(0.0), SensibleLoadMet(0.0), LatentLoadMet(0.0),
              DehumidInducedHeatingDemandRate(0.0), CoilOutletNode(0), LoopNum(0), LoopSide(0), BranchNum(0), CompNum(0), SuppCoilOutletNode(0),
              LoopNumSupp(0), LoopSideSupp(0), BranchNumSupp(0), CompNumSupp(0), HotWaterCoilMaxIterIndex(0), HotWaterCoilMaxIterIndex2(0),
              EMSOverrideSensZoneLoadRequest(false), EMSSensibleZoneLoadValue(0.0), EMSOverrideMoistZoneLoadRequest(false),
              EMSMoistureZoneLoadValue(0.0), HeatCoolMode(0), NumOfSpeedCooling(0), NumOfSpeedHeating(0), IdleSpeedRatio(0.0), IdleVolumeAirRate(0.0),
              IdleMassFlowRate(0.0), FanVolFlow(0.0), CheckFanFlow(true), HeatVolumeFlowRate(MaxSpedLevels, 0.0),
              HeatMassFlowRate(MaxSpedLevels, 0.0), CoolVolumeFlowRate(MaxSpedLevels, 0.0), CoolMassFlowRate(MaxSpedLevels, 0.0),
              MSHeatingSpeedRatio(MaxSpedLevels, 0.0), MSCoolingSpeedRatio(MaxSpedLevels, 0.0), bIsIHP(false), CompSpeedNum(0), CompSpeedRatio(0.0),
              ErrIndexCyc(0), ErrIndexVar(0), WaterCyclingMode(0), iterationCounter(0), iterationMode(0), FirstPass(true)
        {
        }
    };

    // Object Data
    extern Array1D<FurnaceEquipConditions> Furnace;

    // Functions

    void clear_state();

    void SimFurnace(std::string const &FurnaceName,
                    bool const FirstHVACIteration,
                    int const AirLoopNum, // Primary air loop number
                    int &CompIndex        // Pointer to which furnace
    );

    // Get Input Section of the Module
    //******************************************************************************

    void GetFurnaceInput();

    // End of Get Input subroutines for this Module
    //******************************************************************************

    // Beginning Initialization Section of the Module
    //******************************************************************************

    void InitFurnace(int const FurnaceNum,         // index to Furnace
                     int const AirLoopNum,         // index to air loop
                     Real64 &OnOffAirFlowRatio,    // ratio of on to off air mass flow rate
                     int &OpMode,                  // fan operating mode
                     Real64 &ZoneLoad,             // zone sensible load to be met (modified here as needed) (W)
                     Real64 &MoistureLoad,         // zone moisture load (W)
                     bool const FirstHVACIteration // TRUE if first HVAC iteration
    );

    void SetOnOffMassFlowRate(int const FurnaceNum,      // index to furnace
                              int const AirLoopNum,      // index to air loop !unused1208
                              Real64 &OnOffAirFlowRatio, // ratio of coil on to coil off air flow rate
                              int const OpMode,          // fan operating mode
                              Real64 const ZoneLoad,     // sensible load to be met (W) !unused1208
                              Real64 const MoistureLoad, // moisture load to be met (W)
                              Real64 const PartLoadRatio // coil part-load ratio
    );

    void SizeFurnace(int const FurnaceNum, bool const FirstHVACIteration);

    // End Initialization Section of the Module
    //******************************************************************************

    // Beginning of Update subroutines for the Furnace Module
    // *****************************************************************************

    void CalcNewZoneHeatOnlyFlowRates(int const FurnaceNum,          // Index to furnace
                                      bool const FirstHVACIteration, // Iteration flag
                                      Real64 const ZoneLoad,         // load to be met by furnace (W)
                                      Real64 &HeatCoilLoad,          // actual load passed to heating coil (W)
                                      Real64 &OnOffAirFlowRatio      // ratio of coil on to coil off air flow rate
    );

    void CalcNewZoneHeatCoolFlowRates(int const FurnaceNum,
                                      bool const FirstHVACIteration,
                                      int const CompOp,          // compressor operation flag (1=On, 0=Off)
                                      Real64 const ZoneLoad,     // the control zone load (watts)
                                      Real64 const MoistureLoad, // the control zone latent load (watts)
                                      Real64 &HeatCoilLoad,      // Heating load to be met by heating coil ( excluding heat pump DX coil)
                                      Real64 &ReheatCoilLoad,    // Heating load to be met by reheat coil using hstat (excluding HP DX coil)
                                      Real64 &OnOffAirFlowRatio, // Ratio of compressor ON air flow to AVERAGE air flow over time step
                                      bool &HXUnitOn             // flag to control HX based on zone moisture load
    );

    void CalcWaterToAirHeatPump(int const AirLoopNum,          // index to air loop
                                int const FurnaceNum,          // index to Furnace
                                bool const FirstHVACIteration, // TRUE on first HVAC iteration
                                int const CompOp,              // compressor operation flag (1=On, 0=Off)
                                Real64 const ZoneLoad,         // the control zone load (watts)
                                Real64 const MoistureLoad      // the control zone latent load (watts)
    );

    void CalcFurnaceOutput(int const FurnaceNum,
                           bool const FirstHVACIteration,
                           int const FanOpMode,            // Cycling fan or constant fan
                           int const CompOp,               // Compressor on/off; 1=on, 0=off
                           Real64 const CoolPartLoadRatio, // DX cooling coil part load ratio
                           Real64 const HeatPartLoadRatio, // DX heating coil part load ratio (0 for other heating coil types)
                           Real64 const HeatCoilLoad,      // Heating coil load for gas heater
                           Real64 const ReheatCoilLoad,    // Reheating coil load for gas heater
                           Real64 &SensibleLoadMet,        // Sensible cooling load met (furnace outlet with respect to control zone temp)
                           Real64 &LatentLoadMet,          // Latent cooling load met (furnace outlet with respect to control zone humidity ratio)
                           Real64 &OnOffAirFlowRatio,      // Ratio of compressor ON mass flow rate to AVERAGE
                           bool const HXUnitOn,            // flag to enable HX based on zone moisture load
                           Optional<Real64 const> CoolingHeatingPLRRat = _ // cooling PLR to heating PLR ratio, used for cycling fan RH control
    );

    //        End of Update subroutines for the Furnace Module
    // *****************************************************************************

    Real64 CalcFurnaceResidual(Real64 const PartLoadRatio, // DX cooling coil part load ratio
                               Array1<Real64> const &Par   // Function parameters
    );

    Real64 CalcWaterToAirResidual(Real64 const PartLoadRatio, // DX cooling coil part load ratio
                                  Array1<Real64> const &Par   // Function parameters
    );

    void SetAverageAirFlow(int const FurnaceNum,       // Unit index
                           Real64 const PartLoadRatio, // unit part load ratio
                           Real64 &OnOffAirFlowRatio   // ratio of compressor ON airflow to AVERAGE airflow over timestep
    );

    void HeatPumpRunFrac(int const FurnaceNum, // Furnace Index Number
                         Real64 const PLR,     // part load ratio
                         bool &errFlag,        // part load factor out of range flag
                         Real64 &RuntimeFrac   // the required run time fraction to meet part load
    );

    // Beginning of Reporting subroutines for the Furnace Module
    // *****************************************************************************

    void ReportFurnace(int const FurnaceNum, // Furnace Index Number
                       int const AirLoopNum  // index to air loop
    );

    void CalcNonDXHeatingCoils(int const FurnaceNum,           // Furnace Index
                               bool const SuppHeatingCoilFlag, // .TRUE. if supplemental heating coil
                               bool const FirstHVACIteration,  // flag for first HVAC iteration in the time step
                               Real64 const QCoilLoad,         // load met by unit (watts)
                               int const FanMode,              // fan operation mode
                               Real64 &HeatCoilLoadmet         // Heating Load Met
    );

    Real64 HotWaterCoilResidual(Real64 const HWFlow,      // hot water flow rate in kg/s
                                Array1<Real64> const &Par // Par(5) is the requested coil load
    );

    //        End of Reporting subroutines for the Furnace Module

    //******************************************************************************

    void SimVariableSpeedHP(int const FurnaceNum,          // number of the current engine driven Heat Pump being simulated
                            bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
                            int const AirLoopNum,          // index to air loop
                            Real64 const QZnReq,           // required zone load
                            Real64 const QLatReq,          // required latent load
                            Real64 &OnOffAirFlowRatio      // ratio of compressor ON airflow to AVERAGE airflow over timestep
    );

    //******************************************************************************

    void ControlVSHPOutput(int const FurnaceNum,          // Unit index of engine driven heat pump
                           bool const FirstHVACIteration, // flag for 1st HVAC iteration in the time step
                           int const CompOp,              // compressor operation; 1=on, 0=off
                           int const OpMode,              // operating mode: CycFanCycCoil | ContFanCycCoil
                           Real64 &QZnReq,                // cooling or heating output needed by zone [W]
                           Real64 &QLatReq,               // latent cooling output needed by zone [W]
                           int const ZoneNum,             // Index to zone number
                           int &SpeedNum,                 // Speed number
                           Real64 &SpeedRatio,            // unit speed ratio for DX coils
                           Real64 &PartLoadFrac,          // unit part load fraction
                           Real64 &OnOffAirFlowRatio,     // ratio of compressor ON airflow to AVERAGE airflow over timestep
                           Real64 &SupHeaterLoad          // Supplemental heater load [W]
    );

    //******************************************************************************

    void CalcVarSpeedHeatPump(int const FurnaceNum,          // Variable speed heat pump number
                              bool const FirstHVACIteration, // Flag for 1st HVAC iteration
                              int const CompOp,              // Compressor on/off; 1=on, 0=off
                              int const SpeedNum,            // Speed number
                              Real64 const SpeedRatio,       // Compressor speed ratio
                              Real64 const PartLoadFrac,     // Compressor part load fraction
                              Real64 &SensibleLoadMet,       // Sensible cooling load met (furnace outlet with respect to control zone temp)
                              Real64 &LatentLoadMet,         // Latent cooling load met (furnace outlet with respect to control zone humidity ratio)
                              Real64 const QZnReq,           // Zone load (W)
                              Real64 const QLatReq,          // Zone latent load []
                              Real64 &OnOffAirFlowRatio,     // Ratio of compressor ON airflow to AVERAGE airflow over timestep
                              Real64 &SupHeaterLoad          // supplemental heater load (W)
    );

    //******************************************************************************

    Real64 VSHPCyclingResidual(Real64 const PartLoadFrac, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                               Array1<Real64> const &Par  // par(1) = FurnaceNum
    );

    //******************************************************************************

    Real64 VSHPSpeedResidual(Real64 const SpeedRatio,  // compressor cycling ratio (1.0 is continuous, 0.0 is off)
                             Array1<Real64> const &Par // par(1) = MSHPNum
    );

    void SetVSHPAirFlow(int const FurnaceNum,                 // Unit index
                        Real64 const PartLoadRatio,           // unit part load ratio
                        Real64 &OnOffAirFlowRatio,            // ratio of compressor ON airflow to average airflow over timestep
                        Optional_int_const SpeedNum = _,      // Speed number
                        Optional<Real64 const> SpeedRatio = _ // Speed ratio
    );

    void SetOnOffMassFlowRateVSCoil(int const FurnaceNum,          // index to furnace
                                    int const ZoneNum,             // index to zone
                                    bool const FirstHVACIteration, // Flag for 1st HVAC iteration
                                    int const AirLoopNum,          // index to air loop !unused1208
                                    Real64 &OnOffAirFlowRatio,     // ratio of coil on to coil off air flow rate
                                    int const OpMode,              // fan operating mode
                                    Real64 const QZnReq,           // sensible load to be met (W) !unused1208
                                    Real64 const MoistureLoad,     // moisture load to be met (W)
                                    Real64 &PartLoadRatio          // coil part-load ratio
    );

    void SetMinOATCompressor(int const FurnaceNum,                   // index to furnace
                             std::string const FurnaceName,          // name of furnace
                             std::string const cCurrentModuleObject, // type of furnace
                             std::string const CoolingCoilType,      // type of cooling coil
                             std::string const CoolingCoilName,      // name of cooling coil
                             std::string const HeatingCoilType,      // type of heating coil
                             std::string const HeatingCoilName,      // name of heating coil
                             bool &ErrorsFound                       // GetInput logical that errors were found
    );

} // namespace Furnaces

} // namespace EnergyPlus

#endif
