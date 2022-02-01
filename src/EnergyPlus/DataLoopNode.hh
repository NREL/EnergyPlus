// EnergyPlus, Copyright (c) 1996-2022, The Board of Trustees of the University of Illinois,
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

    enum class NodeFluidType
    {
        Invalid = -1,
        Blank, // TODO: remove, should be same as Invalid
        Air,
        Water,
        Steam,
        Electric, // TODO: Electric node "fluid" type?
        Num
    };

    enum class ConnectionType
    {
        Invalid = -1,
        Blank, // TODO: remove, should be same as Invalid
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
        InducedAir,
        Num
    };

    constexpr Real64 SensedLoadFlagValue(-999.0);
    constexpr Real64 SensedNodeFlagValue(-999.0);

    // Valid IsParent Types for Node Connections
    constexpr bool ObjectIsParent(true);
    constexpr bool ObjectIsNotParent(false);
    constexpr bool IncrementFluidStreamYes(true);

    // Valid Fluid Types for Nodes
    constexpr static std::array<std::string_view, static_cast<int>(NodeFluidType::Num)> NodeFluidTypeNames = {
        "blank", "Air", "Water", "Steam", "Electric"};

    constexpr static std::array<std::string_view, static_cast<int>(NodeFluidType::Num)> NodeFluidTypeNamesUC = {
        "BLANK", "AIR", "WATER", "STEAM", "ELECTRIC"};

    constexpr static std::array<std::string_view, static_cast<int>(ConnectionType::Num)> ConnectionTypeNames = {"blank",
                                                                                                                "Inlet",
                                                                                                                "Outlet",
                                                                                                                "Internal",
                                                                                                                "ZoneNode",
                                                                                                                "Sensor",
                                                                                                                "Actuator",
                                                                                                                "OutdoorAir",
                                                                                                                "ReliefAir",
                                                                                                                "ZoneInlet",
                                                                                                                "ZoneReturn",
                                                                                                                "ZoneExhaust",
                                                                                                                "Setpoint",
                                                                                                                "Electric",
                                                                                                                "OutsideAirReference",
                                                                                                                "InducedAir"};

    constexpr static std::array<std::string_view, static_cast<int>(ConnectionType::Num)> ConnectionTypeNamesUC = {"BLANK",
                                                                                                                  "INLET",
                                                                                                                  "OUTLET",
                                                                                                                  "INTERNAL",
                                                                                                                  "ZONENODE",
                                                                                                                  "SENSOR",
                                                                                                                  "ACTUATOR",
                                                                                                                  "OUTDOORAIR",
                                                                                                                  "RELIEFAIR",
                                                                                                                  "ZONEINLET",
                                                                                                                  "ZONERETURN",
                                                                                                                  "ZONEEXHAUST",
                                                                                                                  "SETPOINT",
                                                                                                                  "ELECTRIC",
                                                                                                                  "OUTSIDEAIRREFERENCE",
                                                                                                                  "INDUCEDAIR"};

    enum class ConnectionObjectType
    {
        Invalid = -1,
        AirConditionerVariableRefrigerantFlow,
        AirLoopHVAC,
        AirLoopHVACMixer,
        AirLoopHVACReturnPath,
        AirLoopHVACReturnPlenum,
        AirLoopHVACSupplyPath,
        AirLoopHVACSupplyPlenum,
        AirLoopHVACUnitaryFurnaceHeatCool,
        AirLoopHVACUnitaryFurnaceHeatOnly,
        AirLoopHVACUnitaryHeatCool,
        AirLoopHVACUnitaryHeatCoolVAVChangeoverBypass,
        AirLoopHVACUnitaryHeatOnly,
        AirLoopHVACUnitaryHeatPumpAirToAir,
        AirLoopHVACUnitaryHeatPumpAirToAirMultiSpeed,
        AirLoopHVACUnitaryHeatPumpWaterToAir,
        AirLoopHVACUnitarySystem,
        AirLoopHVACZoneMixer,
        AirLoopHVACZoneSplitter,
        AirTerminalDualDuctConstantVolume,
        AirTerminalDualDuctVAV,
        AirTerminalDualDuctVAVOutdoorAir,
        AirTerminalSingleDuctConstantVolumeCooledBeam,
        AirTerminalSingleDuctConstantVolumeFourPipeBeam,
        AirTerminalSingleDuctConstantVolumeFourPipeInduction,
        AirTerminalSingleDuctConstantVolumeNoReheat,
        AirTerminalSingleDuctConstantVolumeReheat,
        AirTerminalSingleDuctMixer,
        AirTerminalSingleDuctParallelPIUReheat,
        AirTerminalSingleDuctSeriesPIUReheat,
        AirTerminalSingleDuctUserDefined,
        AirTerminalSingleDuctVAVHeatAndCoolNoReheat,
        AirTerminalSingleDuctVAVHeatAndCoolReheat,
        AirTerminalSingleDuctVAVNoReheat,
        AirTerminalSingleDuctVAVReheat,
        AirTerminalSingleDuctVAVReheatVariableSpeedFan,
        AvailabilityManagerDifferentialThermostat,
        AvailabilityManagerHighTemperatureTurnOff,
        AvailabilityManagerHighTemperatureTurnOn,
        AvailabilityManagerLowTemperatureTurnOff,
        AvailabilityManagerLowTemperatureTurnOn,
        BoilerHotWater,
        BoilerSteam,
        Branch,
        CentralHeatPumpSystem,
        ChillerAbsorption,
        ChillerAbsorptionIndirect,
        ChillerCombustionTurbine,
        ChillerConstantCOP,
        ChillerElectric,
        ChillerElectricEIR,
        ChillerElectricReformulatedEIR,
        ChillerEngineDriven,
        ChillerHeaterAbsorptionDirectFired,
        ChillerHeaterAbsorptionDoubleEffect,
        CoilCoolingDX,
        CoilCoolingDXCurveFitSpeed,
        CoilCoolingDXMultiSpeed,
        CoilCoolingDXSingleSpeed,
        CoilCoolingDXSingleSpeedThermalStorage,
        CoilCoolingDXSubcoolReheat,
        CoilCoolingDXTwoSpeed,
        CoilCoolingDXTwoStageWithHumidityControlMode,
        CoilCoolingDXVariableRefrigerantFlow,
        CoilCoolingDXVariableRefrigerantFlowFluidTemperatureControl,
        CoilCoolingDXVariableSpeed,
        CoilCoolingWater,
        CoilCoolingWaterDetailedGeometry,
        CoilCoolingWaterToAirHeatPumpEquationFit,
        CoilCoolingWaterToAirHeatPumpParameterEstimation,
        CoilCoolingWaterToAirHeatPumpVariableSpeedEquationFit,
        CoilHeatingDXMultiSpeed,
        CoilHeatingDXSingleSpeed,
        CoilHeatingDXVariableRefrigerantFlow,
        CoilHeatingDXVariableRefrigerantFlowFluidTemperatureControl,
        CoilHeatingDXVariableSpeed,
        CoilHeatingDesuperheater,
        CoilHeatingElectric,
        CoilHeatingElectricMultiStage,
        CoilHeatingFuel,
        CoilHeatingGasMultiStage,
        CoilHeatingSteam,
        CoilHeatingWater,
        CoilHeatingWaterToAirHeatPumpEquationFit,
        CoilHeatingWaterToAirHeatPumpParameterEstimation,
        CoilHeatingWaterToAirHeatPumpVariableSpeedEquationFit,
        CoilUserDefined,
        CoilWaterHeatingAirToWaterHeatPumpPumped,
        CoilWaterHeatingAirToWaterHeatPumpVariableSpeed,
        CoilWaterHeatingAirToWaterHeatPumpWrapped,
        CoilWaterHeatingDesuperheater,
        CoilSystemCoolingDX,
        CoilSystemCoolingDXHeatExchangerAssisted,
        CoilSystemCoolingWater,
        CoilSystemCoolingWaterHeatExchangerAssisted,
        CoilSystemIntegratedHeatPumpAirSource,
        Condenser,
        CondenserLoop,
        ConnectorMixer,
        ConnectorSplitter,
        ControllerOutdoorAir,
        ControllerWaterCoil,
        CoolingTowerSingleSpeed,
        CoolingTowerTwoSpeed,
        CoolingTowerVariableSpeed,
        CoolingTowerVariableSpeedMerkel,
        DehumidifierDesiccantNoFans,
        DehumidifierDesiccantSystem,
        DistrictCooling,
        DistrictHeating,
        Duct,
        ElectricEquipmentITEAirCooled,
        EvaporativeCoolerDirectCelDekPad,
        EvaporativeCoolerDirectResearchSpecial,
        EvaporativeCoolerIndirectCelDekPad,
        EvaporativeCoolerIndirectResearchSpecial,
        EvaporativeCoolerIndirectWetCoil,
        EvaporativeFluidCoolerSingleSpeed,
        EvaporativeFluidCoolerTwoSpeed,
        FanComponentModel,
        FanConstantVolume,
        FanOnOff,
        FanSystemModel,
        FanVariableVolume,
        FanZoneExhaust,
        FluidCoolerSingleSpeed,
        FluidCoolerTwoSpeed,
        GeneratorCombustionTurbine,
        GeneratorFuelCellAirSupply,
        GeneratorFuelCellExhaustGasToWaterHeatExchanger,
        GeneratorFuelCellPowerModule,
        GeneratorFuelCellStackCooler,
        GeneratorFuelCellWaterSupply,
        GeneratorFuelSupply,
        GeneratorInternalCombustionEngine,
        GeneratorMicroCHP,
        GeneratorMicroTurbine,
        GroundHeatExchangerHorizontalTrench,
        GroundHeatExchangerPond,
        GroundHeatExchangerSlinky,
        GroundHeatExchangerSurface,
        GroundHeatExchangerSystem,
        HeaderedPumpsConstantSpeed,
        HeaderedPumpsVariableSpeed,
        HeatExchangerAirToAirFlatPlate,
        HeatExchangerAirToAirSensibleAndLatent,
        HeatExchangerDesiccantBalancedFlow,
        HeatExchangerFluidToFluid,
        HeatPumpPlantLoopEIRCooling,
        HeatPumpPlantLoopEIRHeating,
        HeatPumpWaterToWaterEquationFitCooling,
        HeatPumpWaterToWaterEquationFitHeating,
        HeatPumpWaterToWaterParameterEstimationCooling,
        HeatPumpWaterToWaterParameterEstimationHeating,
        HumidifierSteamElectric,
        HumidifierSteamGas,
        Lights,
        LoadProfilePlant,
        OutdoorAirMixer,
        OutdoorAirNode,
        OutdoorAirNodeList,
        PipeAdiabatic,
        PipeAdiabaticSteam,
        PipeIndoor,
        PipeOutdoor,
        PipeUnderground,
        PipingSystemUndergroundPipeCircuit,
        PlantComponentTemperatureSource,
        PlantComponentUserDefined,
        PlantEquipmentOperationComponentSetpoint,
        PlantEquipmentOperationOutdoorDewpointDifference,
        PlantEquipmentOperationOutdoorDrybulbDifference,
        PlantEquipmentOperationOutdoorWetbulbDifference,
        PlantEquipmentOperationThermalEnergyStorage,
        PlantLoop,
        PumpConstantSpeed,
        PumpConstantVolume,
        PumpVariableSpeed,
        PumpVariableSpeedCondensate,
        RefrigerationCompressorRack,
        RefrigerationCondenserAirCooled,
        RefrigerationCondenserEvaporativeCooled,
        RefrigerationCondenserWaterCooled,
        RefrigerationGasCoolerAirCooled,
        SetpointManagerColdest,
        SetpointManagerCondenserEnteringReset,
        SetpointManagerCondenserEnteringResetIdeal,
        SetpointManagerFollowGroundTemperature,
        SetpointManagerFollowOutdoorAirTemperature,
        SetpointManagerFollowSystemNodeTemperature,
        SetpointManagerMixedAir,
        SetpointManagerMultiZoneCoolingAverage,
        SetpointManagerMultiZoneHeatingAverage,
        SetpointManagerMultiZoneHumidityMaximum,
        SetpointManagerMultiZoneHumidityMinimum,
        SetpointManagerMultiZoneMaximumHumidityAverage,
        SetpointManagerMultiZoneMinimumHumidityAverage,
        SetpointManagerOutdoorAirPretreat,
        SetpointManagerOutdoorAirReset,
        SetpointManagerReturnTemperatureChilledWater,
        SetpointManagerReturnTemperatureHotWater,
        SetpointManagerScheduled,
        SetpointManagerScheduledDualSetpoint,
        SetpointManagerSingleZoneCooling,
        SetpointManagerSingleZoneHeating,
        SetpointManagerSingleZoneHumidityMaximum,
        SetpointManagerSingleZoneHumidityMinimum,
        SetpointManagerSingleZoneOneStageCooling,
        SetpointManagerSingleZoneOneStageHeating,
        SetpointManagerSingleZoneReheat,
        SetpointManagerWarmest,
        SetpointManagerWarmestTemperatureFlow,
        SolarCollectorFlatPlatePhotovoltaicThermal,
        SolarCollectorFlatPlateWater,
        SolarCollectorIntegralCollectorStorage,
        SolarCollectorUnglazedTranspired,
        SurfacePropertyLocalEnvironment,
        SwimmingPoolIndoor,
        TemperingValve,
        ThermalStorageChilledWaterMixed,
        ThermalStorageChilledWaterStratified,
        ThermalStorageIceDetailed,
        ThermalStorageIceSimple,
        WaterHeaterHeatPump,
        WaterHeaterHeatPumpPumpedCondenser,
        WaterHeaterHeatPumpWrappedCondenser,
        WaterHeaterMixed,
        WaterHeaterStratified,
        WaterUseConnections,
        ZoneHVACAirDistributionUnit,
        ZoneHVACBaseboardConvectiveElectric,
        ZoneHVACBaseboardConvectiveWater,
        ZoneHVACBaseboardRadiantConvectiveElectric,
        ZoneHVACBaseboardRadiantConvectiveSteam,
        ZoneHVACBaseboardRadiantConvectiveWater,
        ZoneHVACCoolingPanelRadiantConvectiveWater,
        ZoneHVACDehumidifierDX,
        ZoneHVACEnergyRecoveryVentilator,
        ZoneHVACEquipmentConnections,
        ZoneHVACEvaporativeCoolerUnit,
        ZoneHVACExhaustControl,
        ZoneHVACForcedAirUserDefined,
        ZoneHVACFourPipeFanCoil,
        ZoneHVACHighTemperatureRadiant,
        ZoneHVACHybridUnitaryHVAC,
        ZoneHVACIdealLoadsAirSystem,
        ZoneHVACLowTemperatureRadiantConstantFlow,
        ZoneHVACLowTemperatureRadiantVariableFlow,
        ZoneHVACOutdoorAirUnit,
        ZoneHVACPackagedTerminalAirConditioner,
        ZoneHVACPackagedTerminalHeatPump,
        ZoneHVACRefrigerationChillerSet,
        ZoneHVACTerminalUnitVariableRefrigerantFlow,
        ZoneHVACUnitHeater,
        ZoneHVACUnitVentilator,
        ZoneHVACVentilatedSlab,
        ZoneHVACWaterToAirHeatPump,
        ZoneHVACWindowAirConditioner,
        ZonePropertyLocalEnvironment,
        Num,
    };

    constexpr static std::array<std::string_view, static_cast<int>(ConnectionObjectType::Num)> ConnectionObjectTypeNames = {
        "AirConditioner:VariableRefrigerantFlow",
        "AirLoopHVAC",
        "AirLoopHVAC:Mixer",
        "AirLoopHVAC:ReturnPath",
        "AirLoopHVAC:ReturnPlenum",
        "AirLoopHVAC:SupplyPath",
        "AirLoopHVAC:SupplyPlenum",
        "AirLoopHVAC:Unitary:Furnace:HeatCool",
        "AirLoopHVAC:Unitary:Furnace:HeatOnly",
        "AirLoopHVAC:UnitaryHeatCool",
        "AirLoopHVAC:UnitaryHeatCool:VAVChangeoverBypass",
        "AirLoopHVAC:UnitaryHeatOnly",
        "AirLoopHVAC:UnitaryHeatPump:AirToAir",
        "AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed",
        "AirLoopHVAC:UnitaryHeatPump:WaterToAir",
        "AirLoopHVAC:UnitarySystem",
        "AirLoopHVAC:ZoneMixer",
        "AirLoopHVAC:ZoneSplitter",
        "AirTerminal:DualDuct:ConstantVolume",
        "AirTerminal:DualDuct:VAV",
        "AirTerminal:DualDuct:VAV:OutdoorAir",
        "AirTerminal:SingleDuct:ConstantVolume:CooledBeam",
        "AirTerminal:SingleDuct:ConstantVolume:FourPipeBeam",
        "AirTerminal:SingleDuct:ConstantVolume:FourPipeInduction",
        "AirTerminal:SingleDuct:ConstantVolume:NoReheat",
        "AirTerminal:SingleDuct:ConstantVolume:Reheat",
        "AirTerminal:SingleDuct:Mixer",
        "AirTerminal:SingleDuct:ParallelPIU:Reheat",
        "AirTerminal:SingleDuct:SeriesPIU:Reheat",
        "AirTerminal:SingleDuct:UserDefined",
        "AirTerminal:SingleDuct:VAV:HeatAndCool:NoReheat",
        "AirTerminal:SingleDuct:VAV:HeatAndCool:Reheat",
        "AirTerminal:SingleDuct:VAV:NoReheat",
        "AirTerminal:SingleDuct:VAV:Reheat",
        "AirTerminal:SingleDuct:VAV:Reheat:VariableSpeedFan",
        "AvailabilityManager:DifferentialThermostat",
        "AvailabilityManager:HighTemperatureTurnOff",
        "AvailabilityManager:HighTemperatureTurnOn",
        "AvailabilityManager:LowTemperatureTurnOff",
        "AvailabilityManager:LowTemperatureTurnOn",
        "Boiler:HotWater",
        "Boiler:Steam",
        "Branch",
        "CentralHeatPumpSystem",
        "Chiller:Absorption",
        "Chiller:Absorption:Indirect",
        "Chiller:CombustionTurbine",
        "Chiller:ConstantCOP",
        "Chiller:Electric",
        "Chiller:Electric:EIR",
        "Chiller:Electric:ReformulatedEIR",
        "Chiller:EngineDriven",
        "ChillerHeater:Absorption:DirectFired",
        "ChillerHeater:Absorption:DoubleEffect",
        "Coil:Cooling:DX",
        "Coil:Cooling:DX:CurveFit:Speed",
        "Coil:Cooling:DX:MultiSpeed",
        "Coil:Cooling:DX:SingleSpeed",
        "Coil:Cooling:DX:SingleSpeed:ThermalStorage",
        "Coil:Cooling:DX:SubcoolReheat",
        "Coil:Cooling:DX:TwoSpeed",
        "Coil:Cooling:DX:TwoStageWithHumidityControlMode",
        "Coil:Cooling:DX:VariableRefrigerantFlow",
        "Coil:Cooling:DX:VariableRefrigerantFlow:FluidTemperatureControl",
        "Coil:Cooling:DX:VariableSpeed",
        "Coil:Cooling:Water",
        "Coil:Cooling:Water:DetailedGeometry",
        "Coil:Cooling:WaterToAirHeatPump:EquationFit",
        "Coil:Cooling:WaterToAirHeatPump:ParameterEstimation",
        "Coil:Cooling:WaterToAirHeatPump:VariableSpeedEquationFit",
        "Coil:Heating:DX:MultiSpeed",
        "Coil:Heating:DX:SingleSpeed",
        "Coil:Heating:DX:VariableRefrigerantFlow",
        "Coil:Heating:DX:VariableRefrigerantFlow:FluidTemperatureControl",
        "Coil:Heating:DX:VariableSpeed",
        "Coil:Heating:Desuperheater",
        "Coil:Heating:Electric",
        "Coil:Heating:Electric:MultiStage",
        "Coil:Heating:Fuel",
        "Coil:Heating:Gas:MultiStage",
        "Coil:Heating:Steam",
        "Coil:Heating:Water",
        "Coil:Heating:WaterToAirHeatPump:EquationFit",
        "Coil:Heating:WaterToAirHeatPump:ParameterEstimation",
        "Coil:Heating:WaterToAirHeatPump:VariableSpeedEquationFit",
        "Coil:UserDefined",
        "Coil:WaterHeating:AirToWaterHeatPump:Pumped",
        "Coil:WaterHeating:AirToWaterHeatPump:VariableSpeed",
        "Coil:WaterHeating:AirToWaterHeatPump:Wrapped",
        "Coil:WaterHeating:Desuperheater",
        "CoilSystem:Cooling:DX",
        "CoilSystem:Cooling:DX:HeatExchangerAssisted",
        "CoilSystem:Cooling:Water",
        "CoilSystem:Cooling:Water:HeatExchangerAssisted",
        "CoilSystem:IntegratedHeatPump:AirSource",
        "Condenser",
        "CondenserLoop",
        "Connector:Mixer",
        "Connector:Splitter",
        "Controller:OutdoorAir",
        "Controller:WaterCoil",
        "CoolingTower:SingleSpeed",
        "CoolingTower:TwoSpeed",
        "CoolingTower:VariableSpeed",
        "CoolingTower:VariableSpeed:Merkel",
        "Dehumidifier:Desiccant:NoFans",
        "Dehumidifier:Desiccant:System",
        "DistrictCooling",
        "DistrictHeating",
        "Duct",
        "ElectricEquipment:ITE:AirCooled",
        "EvaporativeCooler:Direct:CelDekPad",
        "EvaporativeCooler:Direct:ResearchSpecial",
        "EvaporativeCooler:Indirect:CelDekPad",
        "EvaporativeCooler:Indirect:ResearchSpecial",
        "EvaporativeCooler:Indirect:WetCoil",
        "EvaporativeFluidCooler:SingleSpeed",
        "EvaporativeFluidCooler:TwoSpeed",
        "Fan:ComponentModel",
        "Fan:ConstantVolume",
        "Fan:OnOff",
        "Fan:SystemModel",
        "Fan:VariableVolume",
        "Fan:ZoneExhaust",
        "FluidCooler:SingleSpeed",
        "FluidCooler:TwoSpeed",
        "Generator:CombustionTurbine",
        "Generator:FuelCell:AirSupply",
        "Generator:FuelCell:ExhaustGasToWaterHeatExchanger",
        "Generator:FuelCell:PowerModule",
        "Generator:FuelCell:StackCooler",
        "Generator:FuelCell:WaterSupply",
        "Generator:FuelSupply",
        "Generator:InternalCombustionEngine",
        "Generator:MicroCHP",
        "Generator:MicroTurbine",
        "GroundHeatExchanger:HorizontalTrench",
        "GroundHeatExchanger:Pond",
        "GroundHeatExchanger:Slinky",
        "GroundHeatExchanger:Surface",
        "GroundHeatExchanger:System",
        "HeaderedPumps:ConstantSpeed",
        "HeaderedPumps:VariableSpeed",
        "HeatExchanger:AirToAir:FlatPlate",
        "HeatExchanger:AirToAir:SensibleAndLatent",
        "HeatExchanger:Desiccant:BalancedFlow",
        "HeatExchanger:FluidToFluid",
        "HeatPump:PlantLoop:EIR:Cooling",
        "HeatPump:PlantLoop:EIR:Heating",
        "HeatPump:WaterToWater:EquationFit:Cooling",
        "HeatPump:WaterToWater:EquationFit:Heating",
        "HeatPump:WaterToWater:ParameterEstimation:Cooling",
        "HeatPump:WaterToWater:ParameterEstimation:Heating",
        "Humidifier:Steam:Electric",
        "Humidifier:Steam:Gas",
        "Lights",
        "LoadProfile:Plant",
        "OutdoorAir:Mixer",
        "OutdoorAir:Node",
        "OutdoorAir:NodeList",
        "Pipe:Adiabatic",
        "Pipe:Adiabatic:Steam",
        "Pipe:Indoor",
        "Pipe:Outdoor",
        "Pipe:Underground",
        "PipingSystem:Underground:PipeCircuit",
        "PlantComponent:TemperatureSource",
        "PlantComponent:UserDefined",
        "PlantEquipmentOperation:ComponentSetpoint",
        "PlantEquipmentOperation:OutdoorDewpointDifference",
        "PlantEquipmentOperation:OutdoorDrybulbDifference",
        "PlantEquipmentOperation:OutdoorWetbulbDifference",
        "PlantEquipmentOperation:ThermalEnergyStorage",
        "PlantLoop",
        "Pump:ConstantSpeed",
        "Pump:ConstantVolume",
        "Pump:VariableSpeed",
        "Pump:VariableSpeed:Condensate",
        "Refrigeration:CompressorRack",
        "Refrigeration:Condenser:AirCooled",
        "Refrigeration:Condenser:EvaporativeCooled",
        "Refrigeration:Condenser:WaterCooled",
        "Refrigeration:GasCooler:AirCooled",
        "SetpointManager:Coldest",
        "SetpointManager:CondenserEnteringReset",
        "SetpointManager:CondenserEnteringReset:Ideal",
        "SetpointManager:FollowGroundTemperature",
        "SetpointManager:FollowOutdoorAirTemperature",
        "SetpointManager:FollowSystemNodeTemperature",
        "SetpointManager:MixedAir",
        "SetpointManager:MultiZone:Cooling:Average",
        "SetpointManager:MultiZone:Heating:Average",
        "SetpointManager:MultiZone:Humidity:Maximum",
        "SetpointManager:MultiZone:Humidity:Minimum",
        "SetpointManager:MultiZone:MaximumHumidity:Average",
        "SetpointManager:MultiZone:MinimumHumidity:Average",
        "SetpointManager:OutdoorAirPretreat",
        "SetpointManager:OutdoorAirReset",
        "SetpointManager:ReturnTemperature:ChilledWater",
        "SetpointManager:ReturnTemperature:HotWater",
        "SetpointManager:Scheduled",
        "SetpointManager:Scheduled:DualSetpoint",
        "SetpointManager:SingleZone:Cooling",
        "SetpointManager:SingleZone:Heating",
        "SetpointManager:SingleZone:Humidity:Maximum",
        "SetpointManager:SingleZone:Humidity:Minimum",
        "SetpointManager:SingleZone:OneStageCooling",
        "SetpointManager:SingleZone:OneStageHeating",
        "SetpointManager:SingleZone:Reheat",
        "SetpointManager:Warmest",
        "SetpointManager:WarmestTemperatureFlow",
        "SolarCollector:FlatPlate:PhotovoltaicThermal",
        "SolarCollector:FlatPlate:Water",
        "SolarCollector:IntegralCollectorStorage",
        "SolarCollector:UnglazedTranspired",
        "SurfaceProperty:LocalEnvironment",
        "SwimmingPool:Indoor",
        "TemperingValve",
        "ThermalStorage:ChilledWater:Mixed",
        "ThermalStorage:ChilledWater:Stratified",
        "ThermalStorage:Ice:Detailed",
        "ThermalStorage:Ice:Simple",
        "WaterHeater:HeatPump",
        "WaterHeater:HeatPump:PumpedCondenser",
        "WaterHeater:HeatPump:WrappedCondenser",
        "WaterHeater:Mixed",
        "WaterHeater:Stratified",
        "WaterUse:Connections",
        "ZoneHVAC:AirDistributionUnit",
        "ZoneHVAC:Baseboard:Convective:Electric",
        "ZoneHVAC:Baseboard:Convective:Water",
        "ZoneHVAC:Baseboard:RadiantConvective:Electric",
        "ZoneHVAC:Baseboard:RadiantConvective:Steam",
        "ZoneHVAC:Baseboard:RadiantConvective:Water",
        "ZoneHVAC:CoolingPanel:RadiantConvective:Water",
        "ZoneHVAC:Dehumidifier:DX",
        "ZoneHVAC:EnergyRecoveryVentilator",
        "ZoneHVAC:EquipmentConnections",
        "ZoneHVAC:EvaporativeCoolerUnit",
        "ZoneHVAC:ForcedAir:UserDefined",
        "ZoneHVAC:FourPipeFanCoil",
        "ZoneHVAC:HighTemperatureRadiant",
        "ZoneHVAC:HybridUnitaryHVAC",
        "ZoneHVAC:IdealLoadsAirSystem",
        "ZoneHVAC:LowTemperatureRadiant:ConstantFlow",
        "ZoneHVAC:LowTemperatureRadiant:VariableFlow",
        "ZoneHVAC:OutdoorAirUnit",
        "ZoneHVAC:PackagedTerminalAirConditioner",
        "ZoneHVAC:PackagedTerminalHeatPump",
        "ZoneHVAC:RefrigerationChillerSet",
        "ZoneHVAC:TerminalUnit:VariableRefrigerantFlow",
        "ZoneHVAC:UnitHeater",
        "ZoneHVAC:UnitVentilator",
        "ZoneHVAC:VentilatedSlab",
        "ZoneHVAC:WaterToAirHeatPump",
        "ZoneHVAC:WindowAirConditioner",
        "ZoneProperty:LocalEnvironment",
    };

    constexpr static std::array<std::string_view, static_cast<int>(ConnectionObjectType::Num)> ConnectionObjectTypeNamesUC = {
        "AIRCONDITIONER:VARIABLEREFRIGERANTFLOW",
        "AIRLOOPHVAC",
        "AIRLOOPHVAC:MIXER",
        "AIRLOOPHVAC:RETURNPATH",
        "AIRLOOPHVAC:RETURNPLENUM",
        "AIRLOOPHVAC:SUPPLYPATH",
        "AIRLOOPHVAC:SUPPLYPLENUM",
        "AIRLOOPHVAC:UNITARY:FURNACE:HEATCOOL",
        "AIRLOOPHVAC:UNITARY:FURNACE:HEATONLY",
        "AIRLOOPHVAC:UNITARYHEATCOOL",
        "AIRLOOPHVAC:UNITARYHEATCOOL:VAVCHANGEOVERBYPASS",
        "AIRLOOPHVAC:UNITARYHEATONLY",
        "AIRLOOPHVAC:UNITARYHEATPUMP:AIRTOAIR",
        "AIRLOOPHVAC:UNITARYHEATPUMP:AIRTOAIR:MULTISPEED",
        "AIRLOOPHVAC:UNITARYHEATPUMP:WATERTOAIR",
        "AIRLOOPHVAC:UNITARYSYSTEM",
        "AIRLOOPHVAC:ZONEMIXER",
        "AIRLOOPHVAC:ZONESPLITTER",
        "AIRTERMINAL:DUALDUCT:CONSTANTVOLUME",
        "AIRTERMINAL:DUALDUCT:VAV",
        "AIRTERMINAL:DUALDUCT:VAV:OUTDOORAIR",
        "AIRTERMINAL:SINGLEDUCT:CONSTANTVOLUME:COOLEDBEAM",
        "AIRTERMINAL:SINGLEDUCT:CONSTANTVOLUME:FOURPIPEBEAM",
        "AIRTERMINAL:SINGLEDUCT:CONSTANTVOLUME:FOURPIPEINDUCTION",
        "AIRTERMINAL:SINGLEDUCT:CONSTANTVOLUME:NOREHEAT",
        "AIRTERMINAL:SINGLEDUCT:CONSTANTVOLUME:REHEAT",
        "AIRTERMINAL:SINGLEDUCT:MIXER",
        "AIRTERMINAL:SINGLEDUCT:PARALLELPIU:REHEAT",
        "AIRTERMINAL:SINGLEDUCT:SERIESPIU:REHEAT",
        "AIRTERMINAL:SINGLEDUCT:USERDEFINED",
        "AIRTERMINAL:SINGLEDUCT:VAV:HEATANDCOOL:NOREHEAT",
        "AIRTERMINAL:SINGLEDUCT:VAV:HEATANDCOOL:REHEAT",
        "AIRTERMINAL:SINGLEDUCT:VAV:NOREHEAT",
        "AIRTERMINAL:SINGLEDUCT:VAV:REHEAT",
        "AIRTERMINAL:SINGLEDUCT:VAV:REHEAT:VARIABLESPEEDFAN",
        "AVAILABILITYMANAGER:DIFFERENTIALTHERMOSTAT",
        "AVAILABILITYMANAGER:HIGHTEMPERATURETURNOFF",
        "AVAILABILITYMANAGER:HIGHTEMPERATURETURNON",
        "AVAILABILITYMANAGER:LOWTEMPERATURETURNOFF",
        "AVAILABILITYMANAGER:LOWTEMPERATURETURNON",
        "BOILER:HOTWATER",
        "BOILER:STEAM",
        "BRANCH",
        "CENTRALHEATPUMPSYSTEM",
        "CHILLER:ABSORPTION",
        "CHILLER:ABSORPTION:INDIRECT",
        "CHILLER:COMBUSTIONTURBINE",
        "CHILLER:CONSTANTCOP",
        "CHILLER:ELECTRIC",
        "CHILLER:ELECTRIC:EIR",
        "CHILLER:ELECTRIC:REFORMULATEDEIR",
        "CHILLER:ENGINEDRIVEN",
        "CHILLERHEATER:ABSORPTION:DIRECTFIRED",
        "CHILLERHEATER:ABSORPTION:DOUBLEEFFECT",
        "COIL:COOLING:DX",
        "COIL:COOLING:DX:CURVEFIT:SPEED",
        "COIL:COOLING:DX:MULTISPEED",
        "COIL:COOLING:DX:SINGLESPEED",
        "COIL:COOLING:DX:SINGLESPEED:THERMALSTORAGE",
        "COIL:COOLING:DX:SUBCOOLREHEAT",
        "COIL:COOLING:DX:TWOSPEED",
        "COIL:COOLING:DX:TWOSTAGEWITHHUMIDITYCONTROLMODE",
        "COIL:COOLING:DX:VARIABLEREFRIGERANTFLOW",
        "COIL:COOLING:DX:VARIABLEREFRIGERANTFLOW:FLUIDTEMPERATURECONTROL",
        "COIL:COOLING:DX:VARIABLESPEED",
        "COIL:COOLING:WATER",
        "COIL:COOLING:WATER:DETAILEDGEOMETRY",
        "COIL:COOLING:WATERTOAIRHEATPUMP:EQUATIONFIT",
        "COIL:COOLING:WATERTOAIRHEATPUMP:PARAMETERESTIMATION",
        "COIL:COOLING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT",
        "COIL:HEATING:DX:MULTISPEED",
        "COIL:HEATING:DX:SINGLESPEED",
        "COIL:HEATING:DX:VARIABLEREFRIGERANTFLOW",
        "COIL:HEATING:DX:VARIABLEREFRIGERANTFLOW:FLUIDTEMPERATURECONTROL",
        "COIL:HEATING:DX:VARIABLESPEED",
        "COIL:HEATING:DESUPERHEATER",
        "COIL:HEATING:ELECTRIC",
        "COIL:HEATING:ELECTRIC:MULTISTAGE",
        "COIL:HEATING:FUEL",
        "COIL:HEATING:GAS:MULTISTAGE",
        "COIL:HEATING:STEAM",
        "COIL:HEATING:WATER",
        "COIL:HEATING:WATERTOAIRHEATPUMP:EQUATIONFIT",
        "COIL:HEATING:WATERTOAIRHEATPUMP:PARAMETERESTIMATION",
        "COIL:HEATING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT",
        "COIL:USERDEFINED",
        "COIL:WATERHEATING:AIRTOWATERHEATPUMP:PUMPED",
        "COIL:WATERHEATING:AIRTOWATERHEATPUMP:VARIABLESPEED",
        "COIL:WATERHEATING:AIRTOWATERHEATPUMP:WRAPPED",
        "COIL:WATERHEATING:DESUPERHEATER",
        "COILSYSTEM:COOLING:DX",
        "COILSYSTEM:COOLING:DX:HEATEXCHANGERASSISTED",
        "COILSYSTEM:COOLING:WATER",
        "COILSYSTEM:COOLING:WATER:HEATEXCHANGERASSISTED",
        "COILSYSTEM:INTEGRATEDHEATPUMP:AIRSOURCE",
        "CONDENSER",
        "CONDENSERLOOP",
        "CONNECTOR:MIXER",
        "CONNECTOR:SPLITTER",
        "CONTROLLER:OUTDOORAIR",
        "CONTROLLER:WATERCOIL",
        "COOLINGTOWER:SINGLESPEED",
        "COOLINGTOWER:TWOSPEED",
        "COOLINGTOWER:VARIABLESPEED",
        "COOLINGTOWER:VARIABLESPEED:MERKEL",
        "DEHUMIDIFIER:DESICCANT:NOFANS",
        "DEHUMIDIFIER:DESICCANT:SYSTEM",
        "DISTRICTCOOLING",
        "DISTRICTHEATING",
        "DUCT",
        "ELECTRICEQUIPMENT:ITE:AIRCOOLED",
        "EVAPORATIVECOOLER:DIRECT:CELDEKPAD",
        "EVAPORATIVECOOLER:DIRECT:RESEARCHSPECIAL",
        "EVAPORATIVECOOLER:INDIRECT:CELDEKPAD",
        "EVAPORATIVECOOLER:INDIRECT:RESEARCHSPECIAL",
        "EVAPORATIVECOOLER:INDIRECT:WETCOIL",
        "EVAPORATIVEFLUIDCOOLER:SINGLESPEED",
        "EVAPORATIVEFLUIDCOOLER:TWOSPEED",
        "FAN:COMPONENTMODEL",
        "FAN:CONSTANTVOLUME",
        "FAN:ONOFF",
        "FAN:SYSTEMMODEL",
        "FAN:VARIABLEVOLUME",
        "FAN:ZONEEXHAUST",
        "FLUIDCOOLER:SINGLESPEED",
        "FLUIDCOOLER:TWOSPEED",
        "GENERATOR:COMBUSTIONTURBINE",
        "GENERATOR:FUELCELL:AIRSUPPLY",
        "GENERATOR:FUELCELL:EXHAUSTGASTOWATERHEATEXCHANGER",
        "GENERATOR:FUELCELL:POWERMODULE",
        "GENERATOR:FUELCELL:STACKCOOLER",
        "GENERATOR:FUELCELL:WATERSUPPLY",
        "GENERATOR:FUELSUPPLY",
        "GENERATOR:INTERNALCOMBUSTIONENGINE",
        "GENERATOR:MICROCHP",
        "GENERATOR:MICROTURBINE",
        "GROUNDHEATEXCHANGER:HORIZONTALTRENCH",
        "GROUNDHEATEXCHANGER:POND",
        "GROUNDHEATEXCHANGER:SLINKY",
        "GROUNDHEATEXCHANGER:SURFACE",
        "GROUNDHEATEXCHANGER:SYSTEM",
        "HEADEREDPUMPS:CONSTANTSPEED",
        "HEADEREDPUMPS:VARIABLESPEED",
        "HEATEXCHANGER:AIRTOAIR:FLATPLATE",
        "HEATEXCHANGER:AIRTOAIR:SENSIBLEANDLATENT",
        "HEATEXCHANGER:DESICCANT:BALANCEDFLOW",
        "HEATEXCHANGER:FLUIDTOFLUID",
        "HEATPUMP:PLANTLOOP:EIR:COOLING",
        "HEATPUMP:PLANTLOOP:EIR:HEATING",
        "HEATPUMP:WATERTOWATER:EQUATIONFIT:COOLING",
        "HEATPUMP:WATERTOWATER:EQUATIONFIT:HEATING",
        "HEATPUMP:WATERTOWATER:PARAMETERESTIMATION:COOLING",
        "HEATPUMP:WATERTOWATER:PARAMETERESTIMATION:HEATING",
        "HUMIDIFIER:STEAM:ELECTRIC",
        "HUMIDIFIER:STEAM:GAS",
        "LIGHTS",
        "LOADPROFILE:PLANT",
        "OUTDOORAIR:MIXER",
        "OUTDOORAIR:NODE",
        "OUTDOORAIR:NODELIST",
        "PIPE:ADIABATIC",
        "PIPE:ADIABATIC:STEAM",
        "PIPE:INDOOR",
        "PIPE:OUTDOOR",
        "PIPE:UNDERGROUND",
        "PIPINGSYSTEM:UNDERGROUND:PIPECIRCUIT",
        "PLANTCOMPONENT:TEMPERATURESOURCE",
        "PLANTCOMPONENT:USERDEFINED",
        "PLANTEQUIPMENTOPERATION:COMPONENTSETPOINT",
        "PLANTEQUIPMENTOPERATION:OUTDOORDEWPOINTDIFFERENCE",
        "PLANTEQUIPMENTOPERATION:OUTDOORDRYBULBDIFFERENCE",
        "PLANTEQUIPMENTOPERATION:OUTDOORWETBULBDIFFERENCE",
        "PLANTEQUIPMENTOPERATION:THERMALENERGYSTORAGE",
        "PLANTLOOP",
        "PUMP:CONSTANTSPEED",
        "PUMP:CONSTANTVOLUME",
        "PUMP:VARIABLESPEED",
        "PUMP:VARIABLESPEED:CONDENSATE",
        "REFRIGERATION:COMPRESSORRACK",
        "REFRIGERATION:CONDENSER:AIRCOOLED",
        "REFRIGERATION:CONDENSER:EVAPORATIVECOOLED",
        "REFRIGERATION:CONDENSER:WATERCOOLED",
        "REFRIGERATION:GASCOOLER:AIRCOOLED",
        "SETPOINTMANAGER:COLDEST",
        "SETPOINTMANAGER:CONDENSERENTERINGRESET",
        "SETPOINTMANAGER:CONDENSERENTERINGRESET:IDEAL",
        "SETPOINTMANAGER:FOLLOWGROUNDTEMPERATURE",
        "SETPOINTMANAGER:FOLLOWOUTDOORAIRTEMPERATURE",
        "SETPOINTMANAGER:FOLLOWSYSTEMNODETEMPERATURE",
        "SETPOINTMANAGER:MIXEDAIR",
        "SETPOINTMANAGER:MULTIZONE:COOLING:AVERAGE",
        "SETPOINTMANAGER:MULTIZONE:HEATING:AVERAGE",
        "SETPOINTMANAGER:MULTIZONE:HUMIDITY:MAXIMUM",
        "SETPOINTMANAGER:MULTIZONE:HUMIDITY:MINIMUM",
        "SETPOINTMANAGER:MULTIZONE:MAXIMUMHUMIDITY:AVERAGE",
        "SETPOINTMANAGER:MULTIZONE:MINIMUMHUMIDITY:AVERAGE",
        "SETPOINTMANAGER:OUTDOORAIRPRETREAT",
        "SETPOINTMANAGER:OUTDOORAIRRESET",
        "SETPOINTMANAGER:RETURNTEMPERATURE:CHILLEDWATER",
        "SETPOINTMANAGER:RETURNTEMPERATURE:HOTWATER",
        "SETPOINTMANAGER:SCHEDULED",
        "SETPOINTMANAGER:SCHEDULED:DUALSETPOINT",
        "SETPOINTMANAGER:SINGLEZONE:COOLING",
        "SETPOINTMANAGER:SINGLEZONE:HEATING",
        "SETPOINTMANAGER:SINGLEZONE:HUMIDITY:MAXIMUM",
        "SETPOINTMANAGER:SINGLEZONE:HUMIDITY:MINIMUM",
        "SETPOINTMANAGER:SINGLEZONE:ONESTAGECOOLING",
        "SETPOINTMANAGER:SINGLEZONE:ONESTAGEHEATING",
        "SETPOINTMANAGER:SINGLEZONE:REHEAT",
        "SETPOINTMANAGER:WARMEST",
        "SETPOINTMANAGER:WARMESTTEMPERATUREFLOW",
        "SOLARCOLLECTOR:FLATPLATE:PHOTOVOLTAICTHERMAL",
        "SOLARCOLLECTOR:FLATPLATE:WATER",
        "SOLARCOLLECTOR:INTEGRALCOLLECTORSTORAGE",
        "SOLARCOLLECTOR:UNGLAZEDTRANSPIRED",
        "SURFACEPROPERTY:LOCALENVIRONMENT",
        "SWIMMINGPOOL:INDOOR",
        "TEMPERINGVALVE",
        "THERMALSTORAGE:CHILLEDWATER:MIXED",
        "THERMALSTORAGE:CHILLEDWATER:STRATIFIED",
        "THERMALSTORAGE:ICE:DETAILED",
        "THERMALSTORAGE:ICE:SIMPLE",
        "WATERHEATER:HEATPUMP",
        "WATERHEATER:HEATPUMP:PUMPEDCONDENSER",
        "WATERHEATER:HEATPUMP:WRAPPEDCONDENSER",
        "WATERHEATER:MIXED",
        "WATERHEATER:STRATIFIED",
        "WATERUSE:CONNECTIONS",
        "ZONEHVAC:AIRDISTRIBUTIONUNIT",
        "ZONEHVAC:BASEBOARD:CONVECTIVE:ELECTRIC",
        "ZONEHVAC:BASEBOARD:CONVECTIVE:WATER",
        "ZONEHVAC:BASEBOARD:RADIANTCONVECTIVE:ELECTRIC",
        "ZONEHVAC:BASEBOARD:RADIANTCONVECTIVE:STEAM",
        "ZONEHVAC:BASEBOARD:RADIANTCONVECTIVE:WATER",
        "ZONEHVAC:COOLINGPANEL:RADIANTCONVECTIVE:WATER",
        "ZONEHVAC:DEHUMIDIFIER:DX",
        "ZONEHVAC:ENERGYRECOVERYVENTILATOR",
        "ZONEHVAC:EQUIPMENTCONNECTIONS",
        "ZONEHVAC:EVAPORATIVECOOLERUNIT",
        "ZONEHVAC:FORCEDAIR:USERDEFINED",
        "ZONEHVAC:FOURPIPEFANCOIL",
        "ZONEHVAC:HIGHTEMPERATURERADIANT",
        "ZONEHVAC:HYBRIDUNITARYHVAC",
        "ZONEHVAC:IDEALLOADSAIRSYSTEM",
        "ZONEHVAC:LOWTEMPERATURERADIANT:CONSTANTFLOW",
        "ZONEHVAC:LOWTEMPERATURERADIANT:VARIABLEFLOW",
        "ZONEHVAC:OUTDOORAIRUNIT",
        "ZONEHVAC:PACKAGEDTERMINALAIRCONDITIONER",
        "ZONEHVAC:PACKAGEDTERMINALHEATPUMP",
        "ZONEHVAC:REFRIGERATIONCHILLERSET",
        "ZONEHVAC:TERMINALUNIT:VARIABLEREFRIGERANTFLOW",
        "ZONEHVAC:UNITHEATER",
        "ZONEHVAC:UNITVENTILATOR",
        "ZONEHVAC:VENTILATEDSLAB",
        "ZONEHVAC:WATERTOAIRHEATPUMP",
        "ZONEHVAC:WINDOWAIRCONDITIONER",
        "ZONEPROPERTY:LOCALENVIRONMENT",
    };

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
            : FluidType(NodeFluidType::Blank), FluidIndex(0), Temp(0.0), TempMin(0.0), TempMax(0.0), TempSetPoint(SensedNodeFlagValue),
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
        bool IsMarked;                   // true if this is a marked node
        ConnectionObjectType ObjectType; // Object Type that needs it "marked"
        std::string ObjectName;          // Object Name that needs it "marked"
        std::string FieldName;           // FieldName that needs it "marked"

        // Default Constructor
        MarkedNodeData() : IsMarked(false), ObjectType(ConnectionObjectType::Invalid)
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
        DataLoopNode::NodeFluidType::Blank,
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
        this->DefaultNodeValues = DataLoopNode::NodeData(DataLoopNode::NodeFluidType::Blank,
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
