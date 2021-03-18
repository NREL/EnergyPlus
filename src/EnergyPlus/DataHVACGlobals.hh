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

#ifndef DataHVACGlobals_hh_INCLUDED
#define DataHVACGlobals_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

namespace DataHVACGlobals {

    // Using/Aliasing

    // Data
    // -only module should be available to other modules and routines.
    // Thus, all variables in this module must be PUBLIC.
    enum class HVACSystemRootSolverAlgorithm : int
    {
        RegulaFalsi = 0,
        Bisection,
        RegulaFalsiThenBisection,
        BisectionThenRegulaFalsi,
        Alternation
    };

    // MODULE PARAMETER DEFINITIONS:

    Real64 constexpr SmallHumRatDiff(1.0E-7);
    Real64 constexpr SmallTempDiff(1.0E-5);
    Real64 constexpr SmallMassFlow(0.001);
    Real64 constexpr VerySmallMassFlow(1.0E-30);
    Real64 constexpr SmallLoad(1.0);
    Real64 constexpr TempControlTol(0.1); // temperature control tolerance for packaged equip. [deg C]
    Real64 constexpr SmallAirVolFlow(0.001);
    Real64 constexpr SmallWaterVolFlow(1.0E-9);
    Real64 constexpr BlankNumeric(-99999.0);      // indicates numeric input field was blank
    Real64 constexpr RetTempMax(60.0);            // maximum return air temperature [deg C]
    Real64 constexpr RetTempMin(-30.0);           // minimum return air temperature [deg C]
    Real64 constexpr DesCoilHWInletTempMin(46.0); // minimum heating water coil water inlet temp for UA sizing only. [deg C]

    int constexpr NumOfSizingTypes(35); // request sizing for cooling air flow rate

    
    // Sizing types
    int constexpr CoolingAirflowSizing(1);                // request sizing for cooling air flow rate
    int constexpr CoolingWaterDesWaterInletTempSizing(6); // request sizing for cooling water coil inlet water temp
    int constexpr HeatingAirflowSizing(14);               // request sizing for heating air flow rate
    int constexpr SystemAirflowSizing(16);                // request sizing for system air flow rate
    int constexpr CoolingCapacitySizing(17);              // request sizing for cooling capacity
    int constexpr HeatingCapacitySizing(18);              // request sizing for heating capacity
    int constexpr SystemCapacitySizing(21);               // request sizing for system capacity
    int constexpr AutoCalculateSizing(25);                // identifies an autocalulate input

    // Condenser Type (using same numbering scheme as for chillers)
    int constexpr AirCooled(1);   // Air-cooled condenser
    int constexpr WaterCooled(2); // Water-cooled condenser
    int constexpr EvapCooled(3);  // Evaporatively-cooled condenser
    int constexpr WaterHeater(4); // Condenser heats water (e.g., in water heater tank)

    // The following parameters are used for system availability status
    int constexpr NoAction(0);
    int constexpr ForceOff(1);
    int constexpr CycleOn(2);
    int constexpr CycleOnZoneFansOnly(3);
    // The following parameters describe the setpoint types in TempControlType(ActualZoneNum)
    int constexpr SingleHeatingSetPoint(1);
    int constexpr SingleCoolingSetPoint(2);
    int constexpr SingleHeatCoolSetPoint(3);
    int constexpr DualSetPointWithDeadBand(4);
    // parameters describing air duct type
    int constexpr Main(1);
    int constexpr Cooling(2);
    int constexpr Heating(3);
    int constexpr Other(4);
    int constexpr RAB(5);
    // parameters describing fan types
    int constexpr NumAllFanTypes(6);

    // fan types
    int constexpr FanType_SimpleConstVolume(1);
    int constexpr FanType_SimpleVAV(2);
    int constexpr FanType_SimpleOnOff(3);
    int constexpr FanType_ZoneExhaust(4);
    int constexpr FanType_ComponentModel(5);
    int constexpr FanType_SystemModelObject(6);

    // Fan Minimum Flow Fraction Input Method
    int constexpr MinFrac(1);
    int constexpr FixedMin(2);
    // Fan mode
    int constexpr CycFanCycCoil(1); // Cycling fan, cycling coil = 1
    int constexpr ContFanCycCoil(2); // Continuous fan, cycling coil = 2
    // Fan placement
    int constexpr BlowThru(1); // fan before coil
    int constexpr DrawThru(2); // fan after coil
    // OA Controller Heat Recovery Bypass Control Types
    int constexpr BypassWhenWithinEconomizerLimits(0); // heat recovery controlled by economizer limits
    int constexpr BypassWhenOAFlowGreaterThanMinimum(1); // heat recovery ON at minimum OA in economizer mode

    extern Array1D_string const cFanTypes;

// parameters describing unitary systems
    int constexpr NumUnitarySystemTypes(7);
    // Furnace/Unitary System Types
    int constexpr Furnace_HeatOnly(1);
    int constexpr Furnace_HeatCool(2);
    int constexpr UnitarySys_HeatOnly(3);
    int constexpr UnitarySys_HeatCool(4);
    int constexpr UnitarySys_HeatPump_AirToAir(5);
    int constexpr UnitarySys_HeatPump_WaterToAir(6);
    int constexpr UnitarySys_AnyCoilType(7);

    extern Array1D_string const cFurnaceTypes;

 // parameters describing coil types
    int constexpr NumAllCoilTypes(37);
    int constexpr CoilDX_CoolingSingleSpeed(1);
    int constexpr CoilDX_HeatingEmpirical(2);
    int constexpr CoilDX_CoolingTwoSpeed(3);
    int constexpr CoilDX_CoolingHXAssisted(4);
    int constexpr CoilDX_CoolingTwoStageWHumControl(5);
    int constexpr CoilDX_HeatPumpWaterHeaterPumped(6);
    int constexpr CoilDX_HeatPumpWaterHeaterWrapped(7);
    int constexpr CoilDX_MultiSpeedCooling(8);
    int constexpr CoilDX_MultiSpeedHeating(9);
    int constexpr Coil_HeatingGasOrOtherFuel(10);
    int constexpr Coil_HeatingGas_MultiStage(11);
    int constexpr Coil_HeatingElectric(12);
    int constexpr Coil_HeatingElectric_MultiStage(13);
    int constexpr Coil_HeatingDesuperheater(14);
    int constexpr Coil_CoolingWater(15);
    int constexpr Coil_CoolingWaterDetailed(16);
    int constexpr Coil_HeatingWater(17);
    int constexpr Coil_HeatingSteam(18);
    int constexpr CoilWater_CoolingHXAssisted(19);
    int constexpr Coil_CoolingWaterToAirHP(20);
    int constexpr Coil_HeatingWaterToAirHP(21);
    int constexpr Coil_CoolingWaterToAirHPSimple(22);
    int constexpr Coil_HeatingWaterToAirHPSimple(23);
    int constexpr CoilVRF_Cooling(24);
    int constexpr CoilVRF_Heating(25);
    int constexpr Coil_UserDefined(26);
    int constexpr CoilDX_PackagedThermalStorageCooling(27);
    int constexpr Coil_CoolingWaterToAirHPVSEquationFit(28);
    int constexpr Coil_HeatingWaterToAirHPVSEquationFit(29);
    int constexpr Coil_CoolingAirToAirVariableSpeed(30);
    int constexpr Coil_HeatingAirToAirVariableSpeed(31);
    int constexpr CoilDX_HeatPumpWaterHeaterVariableSpeed(32);
    int constexpr CoilVRF_FluidTCtrl_Cooling(33);
    int constexpr CoilVRF_FluidTCtrl_Heating(34);
    int constexpr CoilDX_Cooling(35);
    //    int const CoilDX_SubcoolReheat(36);
    int constexpr CoilDX_CurveFit_Speed(37);

    int constexpr coilNormalMode = 0;        // Normal operation mode
    int constexpr coilEnhancedMode = 1;      // Enhanced operation mode
    int constexpr coilSubcoolReheatMode = 2; // SubcoolReheat operation mode

    extern Array1D_string const cAllCoilTypes;
    extern Array1D_string const cCoolingCoilTypes;
    extern Array1D_string const cHeatingCoilTypes;

    // Water to air HP coil types
    int constexpr WatertoAir_Simple(1);
    int constexpr WatertoAir_ParEst(2);
    int constexpr WatertoAir_VarSpeedEquationFit(3);
    int constexpr WatertoAir_VarSpeedLooUpTable(4);

    // Water to Air HP Water Flow Mode
    int constexpr WaterCycling(1); // water flow cycles with compressor
    int constexpr WaterConstant(2); // water flow is constant
    int constexpr
        WaterConstantOnDemand(3); // water flow is constant whenever the coil is operational - this is the only method used in EP V7.2 and earlier

    // parameters describing coil performance types
    int const CoilPerfDX_CoolBypassEmpirical(100);

    // Airflow per total capacity range (Regular DX coils)
    Real64 constexpr MaxRatedVolFlowPerRatedTotCap1(0.00006041); // m3/s per watt = 450 cfm/ton
    Real64 constexpr MinRatedVolFlowPerRatedTotCap1(0.00004027); // m3/s per watt = 300 cfm/ton
    Real64 constexpr MaxHeatVolFlowPerRatedTotCap1(0.00008056);  // m3/s per watt = 600 cfm/ton
    Real64 constexpr MaxCoolVolFlowPerRatedTotCap1(0.00006713);  // m3/s per watt = 500 cfm/ton
    Real64 constexpr MinOperVolFlowPerRatedTotCap1(0.00002684);  // m3/s per watt = 200 cfm/ton

    // 100% DOAS DX coils Airflow per total capacity ratio
    Real64 constexpr MaxRatedVolFlowPerRatedTotCap2(0.00003355); // m3/s per watt = 250 cfm/ton
    Real64 constexpr MinRatedVolFlowPerRatedTotCap2(0.00001677); // m3/s per watt = 125 cfm/ton
    Real64 constexpr MaxHeatVolFlowPerRatedTotCap2(0.00004026);  // m3/s per watt = 300 cfm/ton
    Real64 constexpr MaxCoolVolFlowPerRatedTotCap2(0.00004026);  // m3/s per watt = 300 cfm/ton
    Real64 constexpr MinOperVolFlowPerRatedTotCap2(0.00001342);  // m3/s per watt = 100 cfm/ton

    extern Array1D<Real64> MaxRatedVolFlowPerRatedTotCap;
    extern Array1D<Real64> MinRatedVolFlowPerRatedTotCap;
    extern Array1D<Real64> MaxHeatVolFlowPerRatedTotCap;
    extern Array1D<Real64> MaxCoolVolFlowPerRatedTotCap;
    extern Array1D<Real64> MinOperVolFlowPerRatedTotCap;

    // dx coil type (DXCT)
    int constexpr RegularDXCoil(1); // Regular DX coils or mixed air dx coils
    int constexpr DOASDXCoil(2);    // 100% DOAS DX coils
     
    extern int DXCT;                // dx coil type: regular DX coil ==1, 100% DOAS DX coil = 2

    // Parameters describing Heat Exchanger types
    int constexpr NumHXTypes(3);

    int constexpr HX_AIRTOAIR_FLATPLATE(1);
    int constexpr HX_AIRTOAIR_GENERIC(2);
    int constexpr HX_DESICCANT_BALANCED(3);

    extern Array1D_string const cHXTypes;

    // Parameters describing air terminal mixers
    int constexpr NumATMixerTypes(2);
    int constexpr No_ATMixer(0);
    int constexpr ATMixer_InletSide(1);
    int constexpr ATMixer_SupplySide(2);

    extern Array1D_string const cATMixerTypes;
    bool constexpr ATMixerExists(true);

    // Parameters describing variable refrigerant flow terminal unit types
    int constexpr NumVRFTUTypes(1);
    int constexpr VRFTUType_ConstVolume(1);

    extern Array1D_string const cVRFTUTypes;

    // VRF Heating Performance Curve Temperature Type
    int constexpr NumVRFHeatingPerformanceOATTypes(2);
    int constexpr WetBulbIndicator(1);
    int constexpr DryBulbIndicator(2);

    extern Array1D_string const cVRFHeatingPerformanceOATTypes;

    // parameter concerning the amount of change in zone temperature is needed
    // for oscillation of zone temperature to be detected.
    Real64 constexpr OscillateMagnitude(0.15);

    // Parameters for HVACSystemRootFindingAlgorithm
    int constexpr Bisection(2);

    // MODULE VARIABLE DECLARATIONS:

    extern bool FirstTimeStepSysFlag; // Set to true at the start of each sub-time step

    extern Real64 TimeStepSys;                    // System Time Increment - the adaptive time step used by the HVAC simulation (hours)
    extern Real64 SysTimeElapsed;                 // elapsed system time in zone timestep (hours)
    extern Real64 FracTimeStepZone;               // System time step divided by the zone time step
    extern bool ShortenTimeStepSys;               // Logical flag that triggers shortening of system time step
    extern int NumOfSysTimeSteps;                 // for current zone time step, number of system timesteps inside  it
    extern int NumOfSysTimeStepsLastZoneTimeStep; // previous zone time step, num of system timesteps inside
    extern int LimitNumSysSteps;

    extern bool UseZoneTimeStepHistory;     // triggers use of zone time step history, else system time step history, for ZTM1, ZTMx
    extern int NumPlantLoops;               // Number of plant loops specified in simulation
    extern int NumCondLoops;                // Number of condenser plant loops specified in simulation
    extern int NumElecCircuits;             // Number of electric circuits specified in simulation
    extern int NumGasMeters;                // Number of gas meters specified in simulation
    extern int NumPrimaryAirSys;            // Number of primary HVAC air systems
    extern Real64 OnOffFanPartLoadFraction; // fan part-load fraction (Fan:OnOff)
    extern Real64 DXCoilTotalCapacity;      // DX coil total cooling capacity (eio report var for HPWHs)
    extern Real64 DXElecCoolingPower;       // Electric power consumed by DX cooling coil last DX simulation
    extern Real64 DXElecHeatingPower;       // Electric power consumed by DX heating coil last DX simulation
    extern Real64 ElecHeatingCoilPower;     // Electric power consumed by electric heating coil
    extern Real64 SuppHeatingCoilPower;     // Electric power consumed by electric supplemental heating coil
    extern Real64 AirToAirHXElecPower;      // Electric power consumed by Heat Exchanger:Air To Air (Generic or Flat Plate)
    // from last simulation in HeatRecovery.cc
    extern Real64 UnbalExhMassFlow;      // unbalanced zone exhaust from a zone equip component [kg/s]
    extern Real64 BalancedExhMassFlow;   // balanced zone exhaust (declared as so by user)  [kg/s]
    extern Real64 PlenumInducedMassFlow; // secondary air mass flow rate induced from a return plenum [kg/s]
    extern bool TurnFansOn;              // If true overrides fan schedule and cycles fans on
    extern bool TurnZoneFansOnlyOn; // If true overrides zone fan schedule and cycles fans on (currently used only by parallel powered induction unit)
    extern bool TurnFansOff;        // If True overides fan schedule and TurnFansOn and forces fans off
    extern bool ZoneCompTurnFansOn; // If true overrides fan schedule and cycles fans on
    extern bool ZoneCompTurnFansOff; // If True overides fan schedule and TurnFansOn and forces fans off
    extern bool SetPointErrorFlag;   // True if any needed setpoints not set; if true, program terminates
    extern bool DoSetPointTest;      // True one time only for sensed node setpoint test
    extern bool NightVentOn;         // set TRUE in SimAirServingZone if night ventilation is happening

    extern int NumTempContComps;
    extern Real64 HPWHInletDBTemp;     // Used by curve objects when calculating DX coil performance for HEAT PUMP:WATER HEATER
    extern Real64 HPWHInletWBTemp;     // Used by curve objects when calculating DX coil performance for HEAT PUMP:WATER HEATER
    extern Real64 HPWHCrankcaseDBTemp; // Used for HEAT PUMP:WATER HEATER crankcase heater ambient temperature calculations
    extern bool AirLoopInit;           // flag for whether InitAirLoops has been called
    extern bool AirLoopsSimOnce;       // True means that the air loops have been simulated once in this environment
    extern bool GetAirPathDataDone;    // True means that air loops inputs have been processed

    // Hybrid ventilation control part
    extern int NumHybridVentSysAvailMgrs;                  // Number of hybrid ventilation control
    extern Array1D_int HybridVentSysAvailAirLoopNum;       // Airloop number in hybrid vent availability manager
    extern Array1D_int HybridVentSysAvailVentCtrl;         // Ventilation control action in hybrid vent availability manager
    extern Array1D_int HybridVentSysAvailActualZoneNum;    // Actual zone num in hybrid vent availability manager
    extern Array1D_int HybridVentSysAvailANCtrlStatus;     // AN control status in hybrid vent availability manager
    extern Array1D_int HybridVentSysAvailMaster;           // Master object name: Ventilation for simple; Zone name for AN
    extern Array1D<Real64> HybridVentSysAvailWindModifier; // Wind modifier for AirflowNetwork
    // For multispeed heat pump only
    extern Real64 MSHPMassFlowRateLow;     // Mass flow rate at low speed
    extern Real64 MSHPMassFlowRateHigh;    // Mass flow rate at high speed
    extern Real64 MSHPWasteHeat;           // Waste heat
    extern Real64 PreviousTimeStep;        // The time step length at the previous time step
    extern bool ShortenTimeStepSysRoomAir; // Logical flag that triggers shortening of system time step

    extern Real64 deviationFromSetPtThresholdHtg; // heating threshold for reporting setpoint deviation
    extern Real64 deviationFromSetPtThresholdClg; // cooling threshold for reporting setpoint deviation

    extern bool SimAirLoopsFlag;               // True when the air loops need to be (re)simulated
    extern bool SimElecCircuitsFlag;           // True when electic circuits need to be (re)simulated
    extern bool SimPlantLoopsFlag;             // True when the main plant loops need to be (re)simulated
    extern bool SimZoneEquipmentFlag;          // True when zone equipment components need to be (re)simulated
    extern bool SimNonZoneEquipmentFlag;       // True when non-zone equipment components need to be (re)simulated
    extern bool ZoneMassBalanceHVACReSim;      // True when zone air mass flow balance and air loop needs (re)simulated
    extern int MinAirLoopIterationsAfterFirst; // minimum number of HVAC iterations after FirstHVACIteration (must be at least 2 for sequenced loads
                                               // to operate on air loops)

    int constexpr NumZoneHVACTerminalTypes = 38;
    extern Array1D_string const ccZoneHVACTerminalTypes;
    extern Array1D_string const ZoneHVACTerminalTypes;
    int constexpr ZoneEquipTypeOf_VariableRefrigerantFlow(1);
    int constexpr ZoneEquipTypeOf_EnergyRecoveryVentilator(2);
    int constexpr ZoneEquipTypeOf_FourPipeFanCoil(3);
    int constexpr ZoneEquipTypeOf_OutdoorAirUnit(4);
    int constexpr ZoneEquipTypeOf_PackagedTerminalAirConditioner(5);
    int constexpr ZoneEquipTypeOf_PackagedTerminalHeatPump(6);
    int constexpr ZoneEquipTypeOf_UnitHeater(7);
    int constexpr ZoneEquipTypeOf_UnitVentilator(8);
    int constexpr ZoneEquipTypeOf_VentilatedSlab(9);
    int constexpr ZoneEquipTypeOf_WaterToAirHeatPump(10);
    int constexpr ZoneEquipTypeOf_WindowAirConditioner(11);
    int constexpr ZoneEquipTypeOf_BaseboardRadiantConvectiveElectric(12);
    int constexpr ZoneEquipTypeOf_BaseboardRadiantConvectiveWater(13);
    int constexpr ZoneEquipTypeOf_BaseboardRadiantConvectiveSteam(14);
    int constexpr ZoneEquipTypeOf_BaseboardConvectiveElectric(15);
    int constexpr ZoneEquipTypeOf_BaseboardConvectiveWater(16);
    int constexpr ZoneEquipTypeOf_HighTemperatureRadiant(17);
    int constexpr ZoneEquipTypeOf_DehumidifierDX(18);
    int constexpr ZoneEquipTypeOf_IdealLoadsAirSystem(19);
    int constexpr ZoneEquipTypeOf_RefrigerationChillerSet(20);
    int constexpr ZoneEquipTypeOf_HybridUnitaryAirConditioners(21);
    int constexpr ZoneEquipTypeOf_FanZoneExhaust(22);
    int constexpr ZoneEquipTypeOf_WaterHeaterHeatPump(23);
    int constexpr ZoneEquipTypeOf_AirTerminalDualDuctConstantVolume(24);
    int constexpr ZoneEquipTypeOf_AirTerminalDualDuctVAV(25);
    int constexpr ZoneEquipTypeOf_AirTerminalSingleDuctConstantVolumeReheat(26);
    int constexpr ZoneEquipTypeOf_AirTerminalSingleDuctConstantVolumeNoReheat(27);
    int constexpr ZoneEquipTypeOf_AirTerminalSingleDuctVAVReheat(28);
    int constexpr ZoneEquipTypeOf_AirTerminalSingleDuctVAVNoReheat(29);
    int constexpr ZoneEquipTypeOf_AirTerminalSingleDuctSeriesPIUReheat(30);
    int constexpr ZoneEquipTypeOf_AirTerminalSingleDuctParallelPIUReheat(31);
    int constexpr ZoneEquipTypeOf_AirTerminalSingleDuctCAVFourPipeInduction(32);
    int constexpr ZoneEquipTypeOf_AirTerminalSingleDuctVAVReheatVariableSpeedFan(33);
    int constexpr ZoneEquipTypeOf_AirTerminalSingleDuctVAVHeatAndCoolReheat(34);
    int constexpr ZoneEquipTypeOf_AirTerminalSingleDuctVAVHeatAndCoolNoReheat(35);
    int constexpr ZoneEquipTypeOf_AirTerminalSingleDuctConstantVolumeCooledBeam(36);
    int constexpr ZoneEquipTypeOf_AirTerminalDualDuctVAVOutdoorAir(37);
    int constexpr ZoneEquipTypeOf_AirLoopHVACReturnAir(38);

    struct ComponentSetPtData
    {
        // Members
        std::string EquipmentType;
        std::string EquipmentName;
        int NodeNumIn;
        int NodeNumOut;
        Real64 EquipDemand;
        Real64 DesignFlowRate;
        std::string HeatOrCool;
        int OpType;

        // Default Constructor
        ComponentSetPtData() : NodeNumIn(0), NodeNumOut(0), EquipDemand(0.0), DesignFlowRate(0.0), OpType(0)
        {
        }
    };

    struct DefineZoneCompAvailMgrs
    {
        // Members
        int NumAvailManagers;             // number of availability managers for this system
        int AvailStatus;                  // system availability status
        int StartTime;                    // cycle on time (in SimTimeSteps)
        int StopTime;                     // cycle off time (in SimTimeSteps)
        std::string AvailManagerListName; // name of each availability manager
        Array1D_string AvailManagerName;  // name of each availability manager
        Array1D_int AvailManagerType;     // type of availability manager
        Array1D_int AvailManagerNum;      // index for availability manager
        int ZoneNum;                      // cycle off time (in SimTimeSteps)
        bool Input;                       // starts off as true to initialize zone equipment availability manager data
        int Count;                        // initialize twice to ensure zone equipment availability manager list name has been read in

        // Default Constructor
        DefineZoneCompAvailMgrs() : NumAvailManagers(0), AvailStatus(0), StartTime(0), StopTime(0), ZoneNum(0), Input(true), Count(0)
        {
        }
    };

    struct ZoneCompTypeData
    {
        // Members
        Array1D<DefineZoneCompAvailMgrs> ZoneCompAvailMgrs;
        int TotalNumComp; // total number of components of a zone equip type

        // Default Constructor
        ZoneCompTypeData() : TotalNumComp(0)
        {
        }
    };

    struct OptStartDataType
    {
        // Members
        Array1D_int ActualZoneNum;
        Array1D<Real64> OccStartTime;
        Array1D_bool OptStartFlag;

        // Default Constructor
        OptStartDataType()
        {
        }
    };

    struct HVACSystemRootFindingAlgorithm
    {
        // Members
        std::string Algorithm;                              // Choice of algorithm
        int NumOfIter;                                      // Number of Iteration Before Algorith Switch
        HVACSystemRootSolverAlgorithm HVACSystemRootSolver; // 1 RegulaFalsi; 2 Bisection; 3 BisectionThenRegulaFalsi; 4 RegulaFalsiThenBisection; 5
                                                            // Alternation Default Constructor
        HVACSystemRootFindingAlgorithm() : NumOfIter(5), HVACSystemRootSolver(HVACSystemRootSolverAlgorithm::RegulaFalsi)
        {
        }
    };

    // Object Data
    extern Array1D<ZoneCompTypeData> ZoneComp;
    extern OptStartDataType OptStartData; // For optimum start
    extern Array1D<ComponentSetPtData> CompSetPtEquip;
    extern HVACSystemRootFindingAlgorithm HVACSystemRootFinding;

    // Clears the global data in DataHVACGlobals.
    // Needed for unit tests, should not be normally called.
    void clear_state();

} // namespace DataHVACGlobals

struct HVACGlobalsData : BaseGlobalStruct {

    void clear_state() override
    {

    }
};

} // namespace EnergyPlus

#endif
