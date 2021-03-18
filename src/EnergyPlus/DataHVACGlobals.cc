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

// EnergyPlus Headers
#include <EnergyPlus/DataHVACGlobals.hh>

namespace EnergyPlus {

namespace DataHVACGlobals {

    // MODULE INFORMATION:
    //       MODIFIED       Craig Wray 22Aug2010 Added Fan Component Model

    // PURPOSE OF THIS MODULE:
    // This data-only module is a repository for HVAC variables which are considered
    // to be "global" in nature in EnergyPlus.
    
    Array1D_string const
        cFanTypes(NumAllFanTypes,
                  {"Fan:ConstantVolume", "Fan:VariableVolume", "Fan:OnOff", "Fan:ZoneExhaust", "Fan:ComponentModel", "Fan:SystemModel"});

    
    Array1D_string const cFurnaceTypes(NumUnitarySystemTypes,
                                       {"AirLoopHVAC:Unitary:Furnace:HeatOnly",
                                        "AirLoopHVAC:Unitary:Furnace:HeatCool",
                                        "AirLoopHVAC:UnitaryHeatOnly",
                                        "AirLoopHVAC:UnitaryHeatCool",
                                        "AirLoopHVAC:UnitaryHeatPump:AirToAir",
                                        "AirLoopHVAC:UnitaryHeatPump:WaterToAir",
                                        "AirLoopHVAC:UnitarySystem"});


    Array1D_string const cAllCoilTypes(NumAllCoilTypes,
                                       {"Coil:Cooling:DX:SingleSpeed",
                                        "Coil:Heating:DX:SingleSpeed",
                                        "Coil:Cooling:DX:TwoSpeed",
                                        "CoilSystem:Cooling:DX:HeatExchangerAssisted",
                                        "Coil:Cooling:DX:TwoStageWithHumidityControlMode",
                                        "Coil:WaterHeating:AirToWaterHeatPump:Pumped",
                                        "Coil:WaterHeating:AirToWaterHeatPump:Wrapped",
                                        "Coil:Cooling:DX:MultiSpeed",
                                        "Coil:Heating:DX:MultiSpeed",
                                        "Coil:Heating:Fuel",
                                        "Coil:Heating:Gas:MultiStage",
                                        "Coil:Heating:Electric",
                                        "Coil:Heating:Electric:MultiStage",
                                        "Coil:Heating:Desuperheater",
                                        "Coil:Cooling:Water",
                                        "Coil:Cooling:Water:DetailedGeometry",
                                        "Coil:Heating:Water",
                                        "Coil:Heating:Steam",
                                        "CoilSystem:Cooling:Water:HeatExchangerAssisted",
                                        "Coil:Cooling:WaterToAirHeatPump:ParameterEstimation",
                                        "Coil:Heating:WaterToAirHeatPump:ParameterEstimation",
                                        "Coil:Cooling:WaterToAirHeatPump:EquationFit",
                                        "Coil:Heating:WaterToAirHeatPump:EquationFit",
                                        "Coil:Cooling:DX:VariableRefrigerantFlow",
                                        "Coil:Heating:DX:VariableRefrigerantFlow",
                                        "Coil:UserDefined",
                                        "Coil:Cooling:DX:SingleSpeed:ThermalStorage",
                                        "Coil:Cooling:WaterToAirHeatPump:VariableSpeedEquationFit",
                                        "Coil:Heating:WaterToAirHeatPump:VariableSpeedEquationFit",
                                        "Coil:Cooling:DX:VariableSpeed",
                                        "Coil:Heating:DX:VariableSpeed",
                                        "Coil:WaterHeating:AirToWaterHeatPump:VariableSpeed",
                                        "Coil:Cooling:DX:VariableRefrigerantFlow:FluidTemperatureControl",
                                        "Coil:Heating:DX:VariableRefrigerantFlow:FluidTemperatureControl",
                                        "Coil:Cooling:DX",
                                        "Coil:Cooling:DX:SubcoolReheat",
                                        "Coil:Cooling:DX:CurveFit:Speed"});

    Array1D_string const cCoolingCoilTypes(NumAllCoilTypes,
                                           {"Coil:Cooling:DX:SingleSpeed",
                                            "",
                                            "Coil:Cooling:DX:TwoSpeed",
                                            "CoilSystem:Cooling:DX:HeatExchangerAssisted",
                                            "Coil:Cooling:DX:TwoStageWithHumidityControlMode",
                                            "",
                                            "",
                                            "Coil:Cooling:DX:MultiSpeed",
                                            "",
                                            "",
                                            "",
                                            "",
                                            "",
                                            "",
                                            "Coil:Cooling:Water",
                                            "Coil:Cooling:Water:DetailedGeometry",
                                            "",
                                            "",
                                            "CoilSystem:Cooling:Water:HeatExchangerAssisted",
                                            "Coil:Cooling:WaterToAirHeatPump:ParameterEstimation",
                                            "",
                                            "Coil:Cooling:WaterToAirHeatPump:EquationFit",
                                            "",
                                            "Coil:Cooling:DX:VariableRefrigerantFlow",
                                            "",
                                            "",
                                            "Coil:Cooling:DX:SingleSpeed:ThermalStorage",
                                            "Coil:Cooling:WaterToAirHeatPump:VariableSpeedEquationFit",
                                            "",
                                            "Coil:Cooling:DX:VariableSpeed",
                                            "",
                                            "",
                                            "Coil:Cooling:DX:VariableRefrigerantFlow:FluidTemperatureControl",
                                            "",
                                            "Coil:Cooling:DX",
                                            "Coil:Cooling:DX:SubcoolReheat",
                                            "Coil:Cooling:DX:CurveFit:Speed"});

    Array1D_string const cHeatingCoilTypes(NumAllCoilTypes,
                                           {"",
                                            "Coil:Heating:DX:SingleSpeed",
                                            "",
                                            "",
                                            "",
                                            "Coil:WaterHeating:AirToWaterHeatPump:Pumped",
                                            "Coil:WaterHeating:AirToWaterHeatPump:Wrapped",
                                            "",
                                            "Coil:Heating:DX:MultiSpeed",
                                            "Coil:Heating:Fuel",
                                            "Coil:Heating:Gas:MultiStage",
                                            "Coil:Heating:Electric",
                                            "Coil:Heating:Electric:MultiStage",
                                            "Coil:Heating:Desuperheater",
                                            "",
                                            "",
                                            "Coil:Heating:Water",
                                            "Coil:Heating:Steam",
                                            "",
                                            "",
                                            "Coil:Heating:WaterToAirHeatPump:ParameterEstimation",
                                            "",
                                            "Coil:Heating:WaterToAirHeatPump:EquationFit",
                                            "",
                                            "Coil:Heating:DX:VariableRefrigerantFlow",
                                            "",
                                            "",
                                            "",
                                            "Coil:Heating:WaterToAirHeatPump:VariableSpeedEquationFit",
                                            "",
                                            "Coil:Heating:DX:VariableSpeed",
                                            "Coil:WaterHeating:AirToWaterHeatPump:VariableSpeed",
                                            "",
                                            "Coil:Heating:DX:VariableRefrigerantFlow:FluidTemperatureControl",
                                            "",
                                            "",
                                            ""});

    Array1D<Real64> MaxRatedVolFlowPerRatedTotCap(2, {MaxRatedVolFlowPerRatedTotCap1, MaxRatedVolFlowPerRatedTotCap2});
    Array1D<Real64> MinRatedVolFlowPerRatedTotCap(2, {MinRatedVolFlowPerRatedTotCap1, MinRatedVolFlowPerRatedTotCap2});
    Array1D<Real64> MaxHeatVolFlowPerRatedTotCap(2, {MaxHeatVolFlowPerRatedTotCap1, MaxHeatVolFlowPerRatedTotCap2});
    Array1D<Real64> MaxCoolVolFlowPerRatedTotCap(2, {MaxCoolVolFlowPerRatedTotCap1, MaxCoolVolFlowPerRatedTotCap2});
    Array1D<Real64> MinOperVolFlowPerRatedTotCap(2, {MinOperVolFlowPerRatedTotCap1, MinOperVolFlowPerRatedTotCap2});

    int DXCT(1);                // dx coil type: regular DX coil ==1, 100% DOAS DX coil = 2
    
    Array1D_string const
        cHXTypes(NumHXTypes,
                 {"HeatExchanger:AirToAir:FlatPlate", "HeatExchanger:AirToAir:SensibleAndLatent", "HeatExchanger:Desiccant:BalancedFlow"});

    Array1D_string const cATMixerTypes(NumATMixerTypes, {"AirTerminal:SingleDuct:InletSideMixer", "AirTerminal:SingleDuct:SupplySideMixer"});

    Array1D_string const cVRFTUTypes(NumVRFTUTypes, std::string("ZoneHVAC:TerminalUnit:VariableRefrigerantFlow"));

    Array1D_string const cVRFHeatingPerformanceOATTypes(NumVRFHeatingPerformanceOATTypes, {"WetBulbTemperature", "DryBulbTemperature"});

    bool FirstTimeStepSysFlag(false); // Set to true at the start of each sub-time step

    Real64 TimeStepSys(0.0);                  // System Time Increment - the adaptive time step used by the HVAC simulation (hours)
    Real64 SysTimeElapsed(0.0);               // elapsed system time in zone timestep (hours)
    Real64 FracTimeStepZone(0.0);             // System time step divided by the zone time step
    bool ShortenTimeStepSys(false);           // Logical flag that triggers shortening of system time step
    int NumOfSysTimeSteps(1);                 // for current zone time step, number of system timesteps inside  it
    int NumOfSysTimeStepsLastZoneTimeStep(1); // previous zone time step, num of system timesteps inside
    int LimitNumSysSteps(0);

    bool UseZoneTimeStepHistory(true);    // triggers use of zone time step history, else system time step history, for ZTM1, ZTMx
    int NumPlantLoops(0);                 // Number of plant loops specified in simulation
    int NumCondLoops(0);                  // Number of condenser plant loops specified in simulation
    int NumElecCircuits(0);               // Number of electric circuits specified in simulation
    int NumGasMeters(0);                  // Number of gas meters specified in simulation
    int NumPrimaryAirSys(0);              // Number of primary HVAC air systems
    Real64 OnOffFanPartLoadFraction(1.0); // fan part-load fraction (Fan:OnOff)
    Real64 DXCoilTotalCapacity(0.0);      // DX coil total cooling capacity (eio report var for HPWHs)
    Real64 DXElecCoolingPower(0.0);       // Electric power consumed by DX cooling coil last DX simulation
    Real64 DXElecHeatingPower(0.0);       // Electric power consumed by DX heating coil last DX simulation
    Real64 ElecHeatingCoilPower(0.0);     // Electric power consumed by electric heating coil
    Real64 SuppHeatingCoilPower(0.0);     // Electric power consumed by electric supplemental heating coil
    Real64 AirToAirHXElecPower(0.0);      // Electric power consumed by Heat Exchanger:Air To Air (Generic or Flat Plate)
    // from last simulation in HeatRecovery.cc
    Real64 UnbalExhMassFlow(0.0);      // unbalanced zone exhaust from a zone equip component [kg/s]
    Real64 BalancedExhMassFlow(0.0);   // balanced zone exhaust (declared as so by user)  [kg/s]
    Real64 PlenumInducedMassFlow(0.0); // secondary air mass flow rate induced from a return plenum [kg/s]
    bool TurnFansOn(false);            // If true overrides fan schedule and cycles fans on
    bool TurnZoneFansOnlyOn(false); // If true overrides zone fan schedule and cycles fans on (currently used only by parallel powered induction unit)
    bool TurnFansOff(false);        // If True overides fan schedule and TurnFansOn and forces fans off
    bool ZoneCompTurnFansOn(false); // If true overrides fan schedule and cycles fans on
    bool ZoneCompTurnFansOff(false); // If True overides fan schedule and TurnFansOn and forces fans off
    bool SetPointErrorFlag(false);   // True if any needed setpoints not set; if true, program terminates
    bool DoSetPointTest(false);      // True one time only for sensed node setpoint test
    bool NightVentOn(false);         // set TRUE in SimAirServingZone if night ventilation is happening

    int NumTempContComps(0);
    Real64 HPWHInletDBTemp(0.0);     // Used by curve objects when calculating DX coil performance for HEAT PUMP:WATER HEATER
    Real64 HPWHInletWBTemp(0.0);     // Used by curve objects when calculating DX coil performance for HEAT PUMP:WATER HEATER
    Real64 HPWHCrankcaseDBTemp(0.0); // Used for HEAT PUMP:WATER HEATER crankcase heater ambient temperature calculations
    bool AirLoopInit(false);         // flag for whether InitAirLoops has been called
    bool AirLoopsSimOnce(false);     // True means that the air loops have been simulated once in this environment
    bool GetAirPathDataDone(false);  // True means that air loops inputs have been processed

    // Hybrid ventilation control part
    int NumHybridVentSysAvailMgrs(0);               // Number of hybrid ventilation control
    Array1D_int HybridVentSysAvailAirLoopNum;       // Airloop number in hybrid vent availability manager
    Array1D_int HybridVentSysAvailVentCtrl;         // Ventilation control action in hybrid vent availability manager
    Array1D_int HybridVentSysAvailActualZoneNum;    // Actual zone num in hybrid vent availability manager
    Array1D_int HybridVentSysAvailANCtrlStatus;     // AN control status in hybrid vent availability manager
    Array1D_int HybridVentSysAvailMaster;           // Master object name: Ventilation for simple; Zone name for AN
    Array1D<Real64> HybridVentSysAvailWindModifier; // Wind modifier for AirflowNetwork
    // For multispeed heat pump only
    Real64 MSHPMassFlowRateLow(0.0);       // Mass flow rate at low speed
    Real64 MSHPMassFlowRateHigh(0.0);      // Mass flow rate at high speed
    Real64 MSHPWasteHeat(0.0);             // Waste heat
    Real64 PreviousTimeStep(0.0);          // The time step length at the previous time step
    bool ShortenTimeStepSysRoomAir(false); // Logical flag that triggers shortening of system time step

    Real64 deviationFromSetPtThresholdHtg(-0.2); // heating threshold for reporting setpoint deviation
    Real64 deviationFromSetPtThresholdClg(0.2);  // cooling threshold for reporting setpoint deviation

    bool SimAirLoopsFlag;                  // True when the air loops need to be (re)simulated
    bool SimElecCircuitsFlag;              // True when electic circuits need to be (re)simulated
    bool SimPlantLoopsFlag;                // True when the main plant loops need to be (re)simulated
    bool SimZoneEquipmentFlag;             // True when zone equipment components need to be (re)simulated
    bool SimNonZoneEquipmentFlag;          // True when non-zone equipment components need to be (re)simulated
    bool ZoneMassBalanceHVACReSim;         // True when zone air mass flow balance and air loop needs (re)simulated
    int MinAirLoopIterationsAfterFirst(1); // minimum number of HVAC iterations after FirstHVACIteration

    Array1D_string const ZoneHVACTerminalTypes(NumZoneHVACTerminalTypes,
                                               {"ZONEHVAC:TERMINALUNIT:VARIABLEREFRIGERANTFLOW",
                                                "ZONEHVAC:ENERGYRECOVERYVENTILATOR",
                                                "ZONEHVAC:FOURPIPEFANCOIL",
                                                "ZONEHVAC:OUTDOORAIRUNIT",
                                                "ZONEHVAC:PACKAGEDTERMINALAIRCONDITIONER",
                                                "ZONEHVAC:PACKAGEDTERMINALHEATPUMP",
                                                "ZONEHVAC:UNITHEATER",
                                                "ZONEHVAC:UNITVENTILATOR",
                                                "ZONEHVAC:VENTILATEDSLAB",
                                                "ZONEHVAC:WATERTOAIRHEATPUMP",
                                                "ZONEHVAC:WINDOWAIRCONDITIONER",
                                                "ZONEHVAC:BASEBOARD:RADIANTCONVECTIVE:ELECTRIC",
                                                "ZONEHVAC:BASEBOARD:RADIANTCONVECTIVE:WATER",
                                                "ZONEHVAC:BASEBOARD:RADIANTCONVECTIVE:STEAM",
                                                "ZONEHVAC:BASEBOARD:CONVECTIVE:ELECTRIC",
                                                "ZONEHVAC:BASEBOARD:CONVECTIVE:WATER",
                                                "ZONEHVAC:HIGHTEMPERATURERADIANT",
                                                "ZONEHVAC:DEHUMIDIFIER:DX",
                                                "ZONEHVAC:IDEALLOADSAIRSYSTEM",
                                                "ZONEHVAC:REFRIGERATIONCHILLERSET",
                                                "ZONEHVAC:HYBRIDUNITARYHVAC",
                                                "FAN:ZONEEXHAUST",
                                                "WATERHEATER:HEATPUMP",
                                                "AIRTERMINAL:DUALDUCT:CONSTANTVOLUME",
                                                "AIRTERMINAL:DUALDUCT:VAV",
                                                "AIRTERMINAL:SINGLEDUCT:CONSTANTVOLUME:REHEAT",
                                                "AIRTERMINAL:SINGLEDUCT:CONSTANTVOLUME:NOREHEAT",
                                                "AIRTERMINAL:SINGLEDUCT:VAV:REHEAT",
                                                "AIRTERMINAL:SINGLEDUCT:VAV:NOREHEAT",
                                                "AIRTERMINAL:SINGLEDUCT:SERIESPIU:REHEAT",
                                                "AIRTERMINAL:SINGLEDUCT:PARALLELPIU:REHEAT",
                                                "AIRTERMINAL:SINGLEDUCT:CONSTANTVOLUME:FOURPIPEINDUCTION",
                                                "AIRTERMINAL:SINGLEDUCT:VAV:REHEAT:VARIABLESPEEDFAN",
                                                "AIRTERMINAL:SINGLEDUCT:VAV:HEATANDCOOL:REHEAT",
                                                "AIRTERMINAL:SINGLEDUCT:VAV:HEATANDCOOL:NOREHEAT",
                                                "AIRTERMINAL:SINGLEDUCT:CONSTANTVOLUME:COOLEDBEAM",
                                                "AIRTERMINAL:DUALDUCT:VAV:OUTDOORAIR",
                                                "AIRLOOPHVACRETURNAIR"});

    Array1D_string const ccZoneHVACTerminalTypes(NumZoneHVACTerminalTypes,
                                                 {"ZoneHVAC:TerminalUnit:VariableRefrigerantFlow",
                                                  "ZoneHVAC:EnergyRecoveryVentilator",
                                                  "ZoneHVAC:FourPipeFanCoil",
                                                  "ZoneHVAC:OutdoorAirUnit",
                                                  "ZoneHVAC:PackagedTerminalAirConditioner",
                                                  "ZoneHVAC:PackagedTerminalHeatPump",
                                                  "ZoneHVAC:UnitHeater",
                                                  "ZoneHVAC:UnitVentilator",
                                                  "ZoneHVAC:VentilatedSlab",
                                                  "ZoneHVAC:WaterToAirHeatPump",
                                                  "ZoneHVAC:WindowAirConditioner",
                                                  "ZoneHVAC:Baseboard:RadiantConvective:Electric",
                                                  "ZoneHVAC:Baseboard:RadiantConvective:Water",
                                                  "ZoneHVAC:Baseboard:RadiantConvective:Steam",
                                                  "ZoneHVAC:Baseboard:Convective:Electric",
                                                  "ZoneHVAC:Baseboard:Convective:Water",
                                                  "ZoneHVAC:HighTemperatureRadiant",
                                                  "ZoneHVAC:Dehumidifier:DX",
                                                  "ZoneHVAC:IdealLoadsAirSystem",
                                                  "ZoneHVAC:RefrigerationChillerSet",
                                                  "ZoneHVAC:HybridUnitaryHVAC",
                                                  "Fan:ZoneExhaust",
                                                  "WaterHeater:HeatPump",
                                                  "AirTerminal:DualDuct:ConstantVolume",
                                                  "AirTerminal:DualDuct:VAV",
                                                  "AirTerminal:SingleDuct:ConstantVolume:Reheat",
                                                  "AirTerminal:SingleDuct:ConstantVolume:NoReheat",
                                                  "AirTerminal:SingleDuct:VAV:Reheat",
                                                  "AirTerminal:SingleDuct:VAV:NoReheat",
                                                  "AirTerminal:SingleDuct:SeriesPIU:Reheat",
                                                  "AirTerminal:SingleDuct:ParallelPIU:Reheat",
                                                  "AirTerminal:SingleDuct:ConstantVolume:FourPipeInduction",
                                                  "AirTerminal:SingleDuct:VAV:Reheat:VariableSpeedFan",
                                                  "AirTerminal:SingleDuct:VAV:HeatAndCool:Reheat",
                                                  "AirTerminal:SingleDuct:VAV:HeatAndCool:NoReheat",
                                                  "AirTerminal:SingleDuct:ConstantVolume:CooledBeam",
                                                  "AirTerminal:DualDuct:VAV:OutdoorAir",
                                                  "AirLoopHVACReturnAir"});

    // Object Data
    Array1D<ZoneCompTypeData> ZoneComp;
    OptStartDataType OptStartData; // For optimum start
    Array1D<ComponentSetPtData> CompSetPtEquip;
    HVACSystemRootFindingAlgorithm HVACSystemRootFinding;

    // Clears the global data in DataHVACGlobals.
    // Needed for unit tests, should not be normally called.
    void clear_state()
    {
        DXCT = 1;
        FirstTimeStepSysFlag = false;
        TimeStepSys = 0.0;
        SysTimeElapsed = 0.0;
        FracTimeStepZone = 0.0;
        ShortenTimeStepSys = false;
        NumOfSysTimeSteps = 1;
        NumOfSysTimeStepsLastZoneTimeStep = 1;
        LimitNumSysSteps = 0;
        UseZoneTimeStepHistory = true;
        NumPlantLoops = 0;
        NumCondLoops = 0;
        NumElecCircuits = 0;
        NumGasMeters = 0;
        NumPrimaryAirSys = 0;
        OnOffFanPartLoadFraction = 1.0;
        DXCoilTotalCapacity = 0.0;
        DXElecCoolingPower = 0.0;
        DXElecHeatingPower = 0.0;
        ElecHeatingCoilPower = 0.0;
        SuppHeatingCoilPower = 0.0;
        AirToAirHXElecPower = 0.0;
        UnbalExhMassFlow = 0.0;
        BalancedExhMassFlow = 0.0;
        PlenumInducedMassFlow = 0.0;
        TurnFansOn = false;
        TurnZoneFansOnlyOn = false;
        TurnFansOff = false;
        ZoneCompTurnFansOn = false;
        ZoneCompTurnFansOff = false;
        SetPointErrorFlag = false;
        DoSetPointTest = false;
        NightVentOn = false;
        NumTempContComps = 0;
        HPWHInletDBTemp = 0.0;
        HPWHInletWBTemp = 0.0;
        HPWHCrankcaseDBTemp = 0.0;
        AirLoopInit = false;
        AirLoopsSimOnce = false;
        GetAirPathDataDone = false;
        NumHybridVentSysAvailMgrs = 0;
        HybridVentSysAvailAirLoopNum.deallocate();
        HybridVentSysAvailVentCtrl.deallocate();
        HybridVentSysAvailActualZoneNum.deallocate();
        HybridVentSysAvailANCtrlStatus.deallocate();
        HybridVentSysAvailMaster.deallocate();
        HybridVentSysAvailWindModifier.deallocate();
        MSHPMassFlowRateLow = 0.0;
        MSHPMassFlowRateHigh = 0.0;
        MSHPWasteHeat = 0.0;
        PreviousTimeStep = 0.0;
        ShortenTimeStepSysRoomAir = false;
        deviationFromSetPtThresholdHtg = -0.2;
        deviationFromSetPtThresholdClg = 0.2;
        SimAirLoopsFlag = true;
        SimElecCircuitsFlag = true;
        SimPlantLoopsFlag = true;
        SimZoneEquipmentFlag = true;
        SimNonZoneEquipmentFlag = true;
        ZoneMassBalanceHVACReSim = true;
        MinAirLoopIterationsAfterFirst = 1;
        ZoneComp.deallocate();
        CompSetPtEquip.deallocate();
        OptStartData = OptStartDataType();
        // unit test ZoneTempPredictorCorrector_ReportingTest fails without this next line. Next 2 lines are just to be thorough.
        OptStartData.OptStartFlag.deallocate();
        OptStartData.ActualZoneNum.deallocate();
        OptStartData.OccStartTime.deallocate();
    }

} // namespace DataHVACGlobals

} // namespace EnergyPlus
