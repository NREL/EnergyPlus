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
    int constexpr CycFanCycCoil(1);  // Cycling fan, cycling coil = 1
    int constexpr ContFanCycCoil(2); // Continuous fan, cycling coil = 2
    // Fan placement
    int constexpr BlowThru(1); // fan before coil
    int constexpr DrawThru(2); // fan after coil
    // OA Controller Heat Recovery Bypass Control Types
    int constexpr BypassWhenWithinEconomizerLimits(0);   // heat recovery controlled by economizer limits
    int constexpr BypassWhenOAFlowGreaterThanMinimum(1); // heat recovery ON at minimum OA in economizer mode

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

    // Water to air HP coil types
    int constexpr WatertoAir_Simple(1);
    int constexpr WatertoAir_ParEst(2);
    int constexpr WatertoAir_VarSpeedEquationFit(3);
    int constexpr WatertoAir_VarSpeedLooUpTable(4);

    // Water to Air HP Water Flow Mode
    int constexpr WaterCycling(1);  // water flow cycles with compressor
    int constexpr WaterConstant(2); // water flow is constant
    int constexpr WaterConstantOnDemand(
        3); // water flow is constant whenever the coil is operational - this is the only method used in EP V7.2 and earlier

    // parameters describing coil performance types
    int constexpr CoilPerfDX_CoolBypassEmpirical(100);

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

    // dx coil type (DXCT)
    int constexpr RegularDXCoil(1); // Regular DX coils or mixed air dx coils
    int constexpr DOASDXCoil(2);    // 100% DOAS DX coils

    // Parameters describing Heat Exchanger types
    int constexpr NumHXTypes(3);

    int constexpr HX_AIRTOAIR_FLATPLATE(1);
    int constexpr HX_AIRTOAIR_GENERIC(2);
    int constexpr HX_DESICCANT_BALANCED(3);

    // Parameters describing air terminal mixers
    int constexpr NumATMixerTypes(2);
    int constexpr No_ATMixer(0);
    int constexpr ATMixer_InletSide(1);
    int constexpr ATMixer_SupplySide(2);

    bool constexpr ATMixerExists(true);

    // Parameters describing variable refrigerant flow terminal unit types
    int constexpr NumVRFTUTypes(1);
    int constexpr VRFTUType_ConstVolume(1);

    // VRF Heating Performance Curve Temperature Type
    int constexpr NumVRFHeatingPerformanceOATTypes(2);
    int constexpr WetBulbIndicator(1);
    int constexpr DryBulbIndicator(2);

    // parameter concerning the amount of change in zone temperature is needed
    // for oscillation of zone temperature to be detected.
    Real64 constexpr OscillateMagnitude(0.15);

    // Parameters for HVACSystemRootFindingAlgorithm
    int constexpr Bisection(2);

    int constexpr NumZoneHVACTerminalTypes = 38;
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

    extern Array1D_string const cFanTypes;
    extern Array1D_string const cAllCoilTypes;
    extern Array1D_string const cCoolingCoilTypes;
    extern Array1D_string const cHeatingCoilTypes;
    extern Array1D_string const ccZoneHVACTerminalTypes;
    extern Array1D_string const ZoneHVACTerminalTypes;
    extern Array1D_string const cATMixerTypes;
    extern Array1D_string const cVRFTUTypes;
    extern Array1D_string const cVRFHeatingPerformanceOATTypes;
    extern Array1D_string const cHXTypes;
    extern Array1D_string const cFurnaceTypes;

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
        OptStartDataType() = default;
    };

} // namespace DataHVACGlobals

struct HVACGlobalsData : BaseGlobalStruct
{
    // Object Data
    Array1D<DataHVACGlobals::ZoneCompTypeData> ZoneComp;
    DataHVACGlobals::OptStartDataType OptStartData; // For optimum start
    Array1D<DataHVACGlobals::ComponentSetPtData> CompSetPtEquip;

    // Hybrid ventilation control part
    int NumHybridVentSysAvailMgrs = 0;              // Number of hybrid ventilation control
    Array1D_int HybridVentSysAvailAirLoopNum;       // Airloop number in hybrid vent availability manager
    Array1D_int HybridVentSysAvailVentCtrl;         // Ventilation control action in hybrid vent availability manager
    Array1D_int HybridVentSysAvailActualZoneNum;    // Actual zone num in hybrid vent availability manager
    Array1D_int HybridVentSysAvailANCtrlStatus;     // AN control status in hybrid vent availability manager
    Array1D_int HybridVentSysAvailMaster;           // Master object name: Ventilation for simple; Zone name for AN
    Array1D<Real64> HybridVentSysAvailWindModifier; // Wind modifier for AirflowNetwork
    // For multispeed heat pump only
    Real64 MSHPMassFlowRateLow = 0.0;       // Mass flow rate at low speed
    Real64 MSHPMassFlowRateHigh = 0.0;      // Mass flow rate at high speed
    Real64 MSHPWasteHeat = 0.0;             // Waste heat
    Real64 PreviousTimeStep = 0.0;          // The time step length at the previous time step
    bool ShortenTimeStepSysRoomAir = false; // Logical flag that triggers shortening of system time step

    Real64 deviationFromSetPtThresholdHtg = -0.2; // heating threshold for reporting setpoint deviation
    Real64 deviationFromSetPtThresholdClg = 0.2;  // cooling threshold for reporting setpoint deviation

    bool SimAirLoopsFlag;                   // True when the air loops need to be (re)simulated
    bool SimElecCircuitsFlag;               // True when electic circuits need to be (re)simulated
    bool SimPlantLoopsFlag;                 // True when the main plant loops need to be (re)simulated
    bool SimZoneEquipmentFlag;              // True when zone equipment components need to be (re)simulated
    bool SimNonZoneEquipmentFlag;           // True when non-zone equipment components need to be (re)simulated
    bool ZoneMassBalanceHVACReSim;          // True when zone air mass flow balance and air loop needs (re)simulated
    int MinAirLoopIterationsAfterFirst = 1; // minimum number of HVAC iterations after FirstHVACIteration

    Array1D<Real64> MaxRatedVolFlowPerRatedTotCap =
        Array1D<Real64>(2, {DataHVACGlobals::MaxRatedVolFlowPerRatedTotCap1, DataHVACGlobals::MaxRatedVolFlowPerRatedTotCap2});
    Array1D<Real64> MinRatedVolFlowPerRatedTotCap =
        Array1D<Real64>(2, {DataHVACGlobals::MinRatedVolFlowPerRatedTotCap1, DataHVACGlobals::MinRatedVolFlowPerRatedTotCap2});
    Array1D<Real64> MaxHeatVolFlowPerRatedTotCap =
        Array1D<Real64>(2, {DataHVACGlobals::MaxHeatVolFlowPerRatedTotCap1, DataHVACGlobals::MaxHeatVolFlowPerRatedTotCap2});
    Array1D<Real64> MaxCoolVolFlowPerRatedTotCap =
        Array1D<Real64>(2, {DataHVACGlobals::MaxCoolVolFlowPerRatedTotCap1, DataHVACGlobals::MaxCoolVolFlowPerRatedTotCap2});
    Array1D<Real64> MinOperVolFlowPerRatedTotCap =
        Array1D<Real64>(2, {DataHVACGlobals::MinOperVolFlowPerRatedTotCap1, DataHVACGlobals::MinOperVolFlowPerRatedTotCap2});

    int DXCT = 1;                      // dx coil type: regular DX coil ==1, 100% DOAS DX coil = 2
    bool FirstTimeStepSysFlag = false; // Set to true at the start of each sub-time step

    Real64 TimeStepSys = 0.0;                  // System Time Increment - the adaptive time step used by the HVAC simulation (hours)
    Real64 SysTimeElapsed = 0.0;               // elapsed system time in zone timestep (hours)
    Real64 FracTimeStepZone = 0.0;             // System time step divided by the zone time step
    bool ShortenTimeStepSys = false;           // Logical flag that triggers shortening of system time step
    int NumOfSysTimeSteps = 1;                 // for current zone time step, number of system timesteps inside  it
    int NumOfSysTimeStepsLastZoneTimeStep = 1; // previous zone time step, num of system timesteps inside
    int LimitNumSysSteps = 0;

    bool UseZoneTimeStepHistory = true;    // triggers use of zone time step history, else system time step history, for ZTM1, ZTMx
    int NumPlantLoops = 0;                 // Number of plant loops specified in simulation
    int NumCondLoops = 0;                  // Number of condenser plant loops specified in simulation
    int NumElecCircuits = 0;               // Number of electric circuits specified in simulation
    int NumGasMeters = 0;                  // Number of gas meters specified in simulation
    int NumPrimaryAirSys = 0;              // Number of primary HVAC air systems
    Real64 OnOffFanPartLoadFraction = 1.0; // fan part-load fraction (Fan:OnOff)
    Real64 DXCoilTotalCapacity = 0.0;      // DX coil total cooling capacity (eio report var for HPWHs)
    Real64 DXElecCoolingPower = 0.0;       // Electric power consumed by DX cooling coil last DX simulation
    Real64 DXElecHeatingPower = 0.0;       // Electric power consumed by DX heating coil last DX simulation
    Real64 ElecHeatingCoilPower = 0.0;     // Electric power consumed by electric heating coil
    Real64 SuppHeatingCoilPower = 0.0;     // Electric power consumed by electric supplemental heating coil
    Real64 AirToAirHXElecPower = 0.0;      // Electric power consumed by Heat Exchanger:Air To Air (Generic or Flat Plate)
    // from last simulation in HeatRecovery.cc
    Real64 UnbalExhMassFlow = 0.0;      // unbalanced zone exhaust from a zone equip component [kg/s]
    Real64 BalancedExhMassFlow = 0.0;   // balanced zone exhaust (declared as so by user)  [kg/s]
    Real64 PlenumInducedMassFlow = 0.0; // secondary air mass flow rate induced from a return plenum [kg/s]
    bool TurnFansOn = false;            // If true overrides fan schedule and cycles fans on
    bool TurnZoneFansOnlyOn =
        false;                // If true overrides zone fan schedule and cycles fans on (currently used only by parallel powered induction unit)
    bool TurnFansOff = false; // If True overides fan schedule and TurnFansOn and forces fans off
    bool ZoneCompTurnFansOn = false;  // If true overrides fan schedule and cycles fans on
    bool ZoneCompTurnFansOff = false; // If True overides fan schedule and TurnFansOn and forces fans off
    bool SetPointErrorFlag = false;   // True if any needed setpoints not set; if true, program terminates
    bool DoSetPointTest = false;      // True one time only for sensed node setpoint test
    bool NightVentOn = false;         // set TRUE in SimAirServingZone if night ventilation is happening

    int NumTempContComps = 0;
    Real64 HPWHInletDBTemp = 0.0;     // Used by curve objects when calculating DX coil performance for HEAT PUMP:WATER HEATER
    Real64 HPWHInletWBTemp = 0.0;     // Used by curve objects when calculating DX coil performance for HEAT PUMP:WATER HEATER
    Real64 HPWHCrankcaseDBTemp = 0.0; // Used for HEAT PUMP:WATER HEATER crankcase heater ambient temperature calculations
    bool AirLoopInit = false;         // flag for whether InitAirLoops has been called
    bool AirLoopsSimOnce = false;     // True means that the air loops have been simulated once in this environment
    bool GetAirPathDataDone = false;  // True means that air loops inputs have been processed
    bool StandardRatingsMyOneTimeFlag = true;
    bool StandardRatingsMyCoolOneTimeFlag = true;
    bool StandardRatingsMyCoolOneTimeFlag2 = true;
    bool StandardRatingsMyHeatOneTimeFlag = true;

    void clear_state() override
    {
        this->ZoneComp.deallocate();
        this->CompSetPtEquip.deallocate();
        this->OptStartData = DataHVACGlobals::OptStartDataType();
        // unit test ZoneTempPredictorCorrector_ReportingTest fails without this next line. Next 2 lines are just to be thorough.
        this->OptStartData.OptStartFlag.deallocate();
        this->OptStartData.ActualZoneNum.deallocate();
        this->OptStartData.OccStartTime.deallocate();

        this->NumHybridVentSysAvailMgrs = 0;
        this->HybridVentSysAvailAirLoopNum.deallocate();
        this->HybridVentSysAvailVentCtrl.deallocate();
        this->HybridVentSysAvailActualZoneNum.deallocate();
        this->HybridVentSysAvailANCtrlStatus.deallocate();
        this->HybridVentSysAvailMaster.deallocate();
        this->HybridVentSysAvailWindModifier.deallocate();
        this->MSHPMassFlowRateLow = 0.0;
        this->MSHPMassFlowRateHigh = 0.0;
        this->MSHPWasteHeat = 0.0;
        this->PreviousTimeStep = 0.0;
        this->ShortenTimeStepSysRoomAir = false;
        this->deviationFromSetPtThresholdHtg = -0.2;
        this->deviationFromSetPtThresholdClg = 0.2;
        this->SimAirLoopsFlag = true;
        this->SimElecCircuitsFlag = true;
        this->SimPlantLoopsFlag = true;
        this->SimZoneEquipmentFlag = true;
        this->SimNonZoneEquipmentFlag = true;
        this->ZoneMassBalanceHVACReSim = true;
        this->MinAirLoopIterationsAfterFirst = 1;

        this->MaxRatedVolFlowPerRatedTotCap =
            Array1D<Real64>(2, {DataHVACGlobals::MaxRatedVolFlowPerRatedTotCap1, DataHVACGlobals::MaxRatedVolFlowPerRatedTotCap2});
        this->MinRatedVolFlowPerRatedTotCap =
            Array1D<Real64>(2, {DataHVACGlobals::MinRatedVolFlowPerRatedTotCap1, DataHVACGlobals::MinRatedVolFlowPerRatedTotCap2});
        this->MaxHeatVolFlowPerRatedTotCap =
            Array1D<Real64>(2, {DataHVACGlobals::MaxHeatVolFlowPerRatedTotCap1, DataHVACGlobals::MaxHeatVolFlowPerRatedTotCap2});
        this->MaxCoolVolFlowPerRatedTotCap =
            Array1D<Real64>(2, {DataHVACGlobals::MaxCoolVolFlowPerRatedTotCap1, DataHVACGlobals::MaxCoolVolFlowPerRatedTotCap2});
        this->MinOperVolFlowPerRatedTotCap =
            Array1D<Real64>(2, {DataHVACGlobals::MinOperVolFlowPerRatedTotCap1, DataHVACGlobals::MinOperVolFlowPerRatedTotCap2});

        this->DXCT = 1;                              // dx coil type: regular DX coil ==1, 100% DOAS DX coil = 2
        this->FirstTimeStepSysFlag = false;          // Set to true at the start of each sub-time step
        this->TimeStepSys = 0.0;                     // System Time Increment - the adaptive time step used by the HVAC simulation (hours)
        this->SysTimeElapsed = 0.0;                  // elapsed system time in zone timestep (hours)
        this->FracTimeStepZone = 0.0;                // System time step divided by the zone time step
        this->ShortenTimeStepSys = false;            // Logical flag that triggers shortening of system time step
        this->NumOfSysTimeSteps = 1;                 // for current zone time step, number of system timesteps inside  it
        this->NumOfSysTimeStepsLastZoneTimeStep = 1; // previous zone time step, num of system timesteps inside
        this->LimitNumSysSteps = 0;
        this->UseZoneTimeStepHistory = true;  // triggers use of zone time step history, else system time step history, for ZTM1, ZTMx
        this->NumPlantLoops = 0;              // Number of plant loops specified in simulation
        this->NumCondLoops = 0;               // Number of condenser plant loops specified in simulation
        this->NumElecCircuits = 0;            // Number of electric circuits specified in simulation
        this->NumGasMeters = 0;               // Number of gas meters specified in simulation
        this->NumPrimaryAirSys = 0;           // Number of primary HVAC air systems
        this->OnOffFanPartLoadFraction = 1.0; // fan part-load fraction (Fan:OnOff)
        this->DXCoilTotalCapacity = 0.0;      // DX coil total cooling capacity (eio report var for HPWHs)
        this->DXElecCoolingPower = 0.0;       // Electric power consumed by DX cooling coil last DX simulation
        this->DXElecHeatingPower = 0.0;       // Electric power consumed by DX heating coil last DX simulation
        this->ElecHeatingCoilPower = 0.0;     // Electric power consumed by electric heating coil
        this->SuppHeatingCoilPower = 0.0;     // Electric power consumed by electric supplemental heating coil
        this->AirToAirHXElecPower = 0.0;      // Electric power consumed by Heat Exchanger:Air To Air (Generic or Flat Plate)
        this->UnbalExhMassFlow = 0.0;         // unbalanced zone exhaust from a zone equip component [kg/s]
        this->BalancedExhMassFlow = 0.0;      // balanced zone exhaust (declared as so by user)  [kg/s]
        this->PlenumInducedMassFlow = 0.0;    // secondary air mass flow rate induced from a return plenum [kg/s]
        this->TurnFansOn = false;             // If true overrides fan schedule and cycles fans on
        this->TurnZoneFansOnlyOn =
            false;                 // If true overrides zone fan schedule and cycles fans on (currently used only by parallel powered induction unit)
        this->TurnFansOff = false; // If True overides fan schedule and TurnFansOn and forces fans off
        this->ZoneCompTurnFansOn = false;  // If true overrides fan schedule and cycles fans on
        this->ZoneCompTurnFansOff = false; // If True overides fan schedule and TurnFansOn and forces fans off
        this->SetPointErrorFlag = false;   // True if any needed setpoints not set; if true, program terminates
        this->DoSetPointTest = false;      // True one time only for sensed node setpoint test
        this->NightVentOn = false;         // set TRUE in SimAirServingZone if night ventilation is happening
        this->NumTempContComps = 0;
        this->HPWHInletDBTemp = 0.0;      // Used by curve objects when calculating DX coil performance for HEAT PUMP:WATER HEATER
        this->HPWHInletWBTemp = 0.0;      // Used by curve objects when calculating DX coil performance for HEAT PUMP:WATER HEATER
        this->HPWHCrankcaseDBTemp = 0.0;  // Used for HEAT PUMP:WATER HEATER crankcase heater ambient temperature calculations
        this->AirLoopInit = false;        // flag for whether InitAirLoops has been called
        this->AirLoopsSimOnce = false;    // True means that the air loops have been simulated once in this environment
        this->GetAirPathDataDone = false; // True means that air loops inputs have been processed
        this->StandardRatingsMyOneTimeFlag = true;
        this->StandardRatingsMyCoolOneTimeFlag = true;
        this->StandardRatingsMyCoolOneTimeFlag2 = true;
        this->StandardRatingsMyHeatOneTimeFlag = true;
    }
};

} // namespace EnergyPlus

#endif
