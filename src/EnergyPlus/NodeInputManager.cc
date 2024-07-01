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

// C++ Headers
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataContaminantBalance.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataErrorTracking.hh>
#include <EnergyPlus/EMSManager.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus::Node {

// MODULE INFORMATION:
//       AUTHOR         Linda K. Lawrie
//       DATE WRITTEN   September 1999

// PURPOSE OF THIS MODULE:
// To provide utilities for reading and assigning indices for the
// nodes in the HVAC loops.

using namespace BranchNodeConnections;

constexpr std::string_view fluidNameSteam = "STEAM";

constexpr std::array<std::string_view, (int)FluidType::Num> fluidTypeNames = {
    "blank", "Air", "Water", "Steam", "Electric"
};

constexpr std::array<std::string_view, (int)FluidType::Num> fluidTypeNamesUC = {
    "BLANK", "AIR", "WATER", "STEAM", "ELECTRIC"
};

constexpr std::array<std::string_view, (int)ConnType::Num> connTypeNames = {
    "blank",
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
    "InducedAir"
};
        
constexpr std::array<std::string_view, (int)ConnType::Num> connTypeNamesUC = {
    "BLANK",
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
    "INDUCEDAIR"
};
        
constexpr std::array<std::string_view, (int)Node::ConnObjType::Num> connObjTypeNames = {
    "Undefined",
    "AirConditioner:VariableRefrigerantFlow",
    "AirLoopHVAC",
    "AirLoopHVAC:DedicatedOutdoorAirSystem",
    "AirLoopHVAC:ExhaustSystem",
    "AirLoopHVAC:Mixer",
    "AirLoopHVAC:OutdoorAirSystem",
    "AirLoopHVAC:ReturnPath",
    "AirLoopHVAC:ReturnPlenum",
    "AirLoopHVAC:Splitter",
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
    "AirTerminal:DualDuct:ConstantVolume:Cool",
    "AirTerminal:DualDuct:ConstantVolume:Heat",
    "AirTerminal:DualDuct:VAV",
    "AirTerminal:DualDuct:VAV:Cool",
    "AirTerminal:DualDuct:VAV:Heat",
    "AirTerminal:DualDuct:VAV:OutdoorAir",
    "AirTerminal:DualDuct:VAV:OutdoorAir:OutdoorAir",
    "AirTerminal:DualDuct:VAV:OutdoorAir:RecirculatedAir",
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
    "Chiller:Electric:ASHRAE205",
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
    "CoilSystem:Heating:DX",
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
    "DistrictHeating:Water",
    "DistrictHeating:Steam",
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
    "HeatPump:AirToWater:FuelFired:Cooling",
    "HeatPump:AirToWater:FuelFired:Heating",
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
    "PlantEquipmentOperation:ChillerHeaterChangeover",
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
    "SetpointManager:SystemNodeReset:Temperature",
    "SetpointManager:SystemNodeReset:Humidity",
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
    "ZoneHVAC:ExhaustControl",
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
    "SpaceHVAC:EquipmentConnections",
    "SpaceHVAC:ZoneEquipmentSplitter",
    "SpaceHVAC:ZoneEquipmentMixer"};

constexpr std::array<std::string_view, static_cast<int>(Node::ConnObjType::Num)> connObjTypeNamesUC = {
    "UNDEFINED",
    "AIRCONDITIONER:VARIABLEREFRIGERANTFLOW",
    "AIRLOOPHVAC",
    "AIRLOOPHVAC:DEDICATEDOUTDOORAIRSYSTEM",
    "AIRLOOPHVAC:EXHAUSTSYSTEM",
    "AIRLOOPHVAC:MIXER",
    "AIRLOOPHVAC:OUTDOORAIRSYSTEM",
    "AIRLOOPHVAC:RETURNPATH",
    "AIRLOOPHVAC:RETURNPLENUM",
    "AIALOOPHVAC:SPLITTER",
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
    "AIRTERMINAL:DUALDUCT:CONSTANTVOLUME:COOL",
    "AIRTERMINAL:DUALDUCT:CONSTANTVOLUME:HEAT",
    "AIRTERMINAL:DUALDUCT:VAV",
    "AIRTERMINAL:DUALDUCT:VAV:COOL",
    "AIRTERMINAL:DUALDUCT:VAV:HEAT",
    "AIRTERMINAL:DUALDUCT:VAV:OUTDOORAIR",
    "AIRTERMINAL:DUALDUCT:VAV:OUTDOORAIR:OUTDOORAIR",
    "AIRTERMINAL:DUALDUCT:VAV:OUTDOORAIR:RECIRCULATEDAIR",
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
    "CHILLER:ELECTRIC:ASHRAE205",
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
    "COILSYSTEM:HEATING:DX",
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
    "DISTRICTHEATING:WATER",
    "DISTRICTHEATING:STEAM",
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
    "HEATPUMP:AIRTOWATER:FUELFIRED:COOLING",
    "HEATPUMP:AIRTOWATER:FUELFIRED:HEATING",
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
    "PLANTEQUIPMENTOPERATION:CHILLERHEATERCHANGEOVER",
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
    "SETPOINTMANAGER:SYSTEMNODERESET:TEMPERATURE",
    "SETPOINTMANAGER:SYSTEMNODERESET:HUMIDITY",
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
    "ZONEHVAC:EXHAUSTCONTROL",
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
    "SPACEHVAC:EQUIPMENTCONNECTIONS",
    "SPACEHVAC:ZONEEQUIPMENTSPLITTER",
    "SPACEHVAC:ZONEEQUIPMENTMIXER"
};
        
int GetNodeIndex(EnergyPlusData &state,
                 std::string const &name)
{
    auto found = state.dataLoopNodes->nodeMap.find(name);
    return (found != state.dataLoopNodes->nodeMap.end()) ? found->second : 0;
}
        
void GetNodeNums(EnergyPlusData &state,
                 std::string const &Name,                                 // Name for which to obtain information
                 int &NumNodes,                                           // Number of nodes accompanying this Name
                 Array1D_int &NodeNumbers,                                // Node Numbers accompanying this Name
                 bool &ErrorsFound,                                       // True when errors are found...
                 FluidType nodeFluidType,               // Fluidtype for checking/setting node FluidType
                 ConnObjType const NodeObjectType, // Node Object Type (i.e. "Chiller:Electric")
                 std::string const &NodeObjectName,                       // Node Object Name (i.e. "MyChiller")
                 ConnType const nodeConnectionType,   // Node Connection Type (see DataLoopNode)
                 CompFluidStream const NodeFluidStream,                   // Which Fluid Stream (1,2,3,...)
                 bool const ObjectIsParent,                               // True/False
                 bool const IncrementFluidStream,                         // True/False
                 std::string_view const InputFieldName                    // Input Field Name
)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   September 1999
    //       MODIFIED       February 2004, Fluid Type checking/setting

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine calls the Node Manager to determine if the
    // entered name has already been assigned and if it is a list
    // or if it is a single node.  If it has not been assigned, then
    // it is a single node and will need to be entered in the Node
    // data structure.

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("GetNodeNums: ");

    auto &dln = state.dataLoopNodes;
    
    std::string_view const objTypeStr = Node::connObjTypeNames[static_cast<int>(NodeObjectType)];

    if (state.dataNodeInputMgr->GetNodeInputFlag) {
        Node::GetNodeListsInput(state, ErrorsFound);
        state.dataNodeInputMgr->GetNodeInputFlag = false;
    }

    if (nodeFluidType != Node::FluidType::Air && nodeFluidType != Node::FluidType::Water &&
        nodeFluidType != Node::FluidType::Electric && nodeFluidType != Node::FluidType::Steam &&
        nodeFluidType != Node::FluidType::Blank) {
        ShowSevereError(state, format("{}{}=\"{}=\", invalid fluid type.", RoutineName, objTypeStr, NodeObjectName));
        ShowContinueError(state, format("..Invalid FluidType={}", nodeFluidType));
        ErrorsFound = true;
        ShowFatalError(state, "Preceding issue causes termination.");
    }

    if (!Name.empty()) {
        int ThisOne = Util::FindItemInList(Name, state.dataNodeInputMgr->NodeLists);
        if (ThisOne != 0) {
            NumNodes = state.dataNodeInputMgr->NodeLists(ThisOne).NumNodes;
            NodeNumbers({1, NumNodes}) = state.dataNodeInputMgr->NodeLists(ThisOne).NodeNums({1, NumNodes});
            for (int Loop = 1; Loop <= NumNodes; ++Loop) {
                auto *node = dln->nodes(NodeNumbers(Loop));
                    
                if (nodeFluidType != Node::FluidType::Blank &&
                    node->fluidType != Node::FluidType::Blank) {
                    if (node->fluidType != nodeFluidType) {
                        ShowSevereError(state, format("{}{}=\"{}=\", invalid data.", RoutineName, objTypeStr, NodeObjectName));
                        if (!InputFieldName.empty()) {
                            ShowContinueError(state, fmt::format("...Ref field={}", InputFieldName));
                        }
                        ShowContinueError(
                            state,
                            format("Existing Fluid type for node, incorrect for request. Node={}", node->Name));
                        ShowContinueError(
                            state,
                            format("Existing Fluid type={}, Requested Fluid Type={}",
                                   Node::fluidTypeNames[(int)node->fluidType],
                                   Node::fluidTypeNames[(int)nodeFluidType]));
                        ErrorsFound = true;
                    }
                }
                if (node->fluidType == Node::FluidType::Blank) {
                    node->fluidType = nodeFluidType;
                }
                ++state.dataNodeInputMgr->NodeRef(NodeNumbers(Loop));
            }
        } else {
            ThisOne = AssignNodeNumber(state, Name, nodeFluidType, ErrorsFound);
            NumNodes = 1;
            NodeNumbers(1) = ThisOne;
        }
    } else {
        NumNodes = 0;
        NodeNumbers(1) = 0;
    }

    // Most calls to this routine use a fixed fluid stream number for all nodes, this is the default
    Node::CompFluidStream FluidStreamNum = NodeFluidStream;
    for (int Loop = 1; Loop <= NumNodes; ++Loop) {
        // If requested, assign NodeFluidStream to the first node and increment the fluid stream number
        // for each remaining node in the list
        if (IncrementFluidStream) {
            FluidStreamNum = static_cast<Node::CompFluidStream>(static_cast<int>(NodeFluidStream) + (Loop - 1));
        }

        RegisterNodeConnection(state,
                               NodeNumbers(Loop),
                               dln->nodes(NodeNumbers(Loop))->Name,
                               NodeObjectType,
                               NodeObjectName,
                               nodeConnectionType,
                               FluidStreamNum,
                               ObjectIsParent,
                               ErrorsFound,
                               InputFieldName);
    }
}

void SetupNodeVarsForReporting(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   September

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine is called when the indicated number of
    // Nodes have been found (TOTAL NODE NUMBER) or when HVAC warmup is
    // complete, whichever condition is reached first.

    auto &dln = state.dataLoopNodes;
    if (!state.dataNodeInputMgr->NodeVarsSetup) {
        if (!state.dataErrTracking->AbortProcessing) {
            for (int NumNode = 1; NumNode <= state.dataNodeInputMgr->NumOfUniqueNodeNames; ++NumNode) {
                auto *node = dln->nodes(NumNode);
                // Setup Report variables for the Nodes for HVAC Reporting, CurrentModuleObject='Node Name'
                SetupOutputVariable(state,
                                    "System Node Temperature",
                                    Constant::Units::C,
                                    node->Temp,
                                    OutputProcessor::TimeStepType::System,
                                    OutputProcessor::StoreType::Average,
                                    node->Name);
                SetupOutputVariable(state,
                                    "System Node Mass Flow Rate",
                                    Constant::Units::kg_s,
                                    node->MassFlowRate,
                                    OutputProcessor::TimeStepType::System,
                                    OutputProcessor::StoreType::Average,
                                    node->Name);
                SetupOutputVariable(state,
                                    "System Node Humidity Ratio",
                                    Constant::Units::kgWater_kgDryAir,
                                    node->HumRat,
                                    OutputProcessor::TimeStepType::System,
                                    OutputProcessor::StoreType::Average,
                                    node->Name);
                SetupOutputVariable(state,
                                    "System Node Setpoint Temperature",
                                    Constant::Units::C,
                                    node->TempSetPoint,
                                    OutputProcessor::TimeStepType::System,
                                    OutputProcessor::StoreType::Average,
                                    node->Name);
                SetupOutputVariable(state,
                                    "System Node Setpoint High Temperature",
                                    Constant::Units::C,
                                    node->TempSetPointHi,
                                    OutputProcessor::TimeStepType::System,
                                    OutputProcessor::StoreType::Average,
                                    node->Name);
                SetupOutputVariable(state,
                                    "System Node Setpoint Low Temperature",
                                    Constant::Units::C,
                                    node->TempSetPointLo,
                                    OutputProcessor::TimeStepType::System,
                                    OutputProcessor::StoreType::Average,
                                    node->Name);
                SetupOutputVariable(state,
                                    "System Node Setpoint Humidity Ratio",
                                    Constant::Units::kgWater_kgDryAir,
                                    node->HumRatSetPoint,
                                    OutputProcessor::TimeStepType::System,
                                    OutputProcessor::StoreType::Average,
                                    node->Name);
                SetupOutputVariable(state,
                                    "System Node Setpoint Minimum Humidity Ratio",
                                    Constant::Units::kgWater_kgDryAir,
                                    node->HumRatMin,
                                    OutputProcessor::TimeStepType::System,
                                    OutputProcessor::StoreType::Average,
                                    node->Name);
                SetupOutputVariable(state,
                                    "System Node Setpoint Maximum Humidity Ratio",
                                    Constant::Units::kgWater_kgDryAir,
                                    node->HumRatMax,
                                    OutputProcessor::TimeStepType::System,
                                    OutputProcessor::StoreType::Average,
                                    node->Name);
                SetupOutputVariable(state,
                                    "System Node Relative Humidity",
                                    Constant::Units::Perc,
                                    node->RelHumidity,
                                    OutputProcessor::TimeStepType::System,
                                    OutputProcessor::StoreType::Average,
                                    node->Name);
                SetupOutputVariable(state,
                                    "System Node Pressure",
                                    Constant::Units::Pa,
                                    node->Press,
                                    OutputProcessor::TimeStepType::System,
                                    OutputProcessor::StoreType::Average,
                                    node->Name);
                SetupOutputVariable(state,
                                    "System Node Standard Density Volume Flow Rate",
                                    Constant::Units::m3_s,
                                    node->VolFlowRateStdRho,
                                    OutputProcessor::TimeStepType::System,
                                    OutputProcessor::StoreType::Average,
                                    node->Name);
                if (node->fluidType == FluidType::Air ||
                    node->fluidType == FluidType::Water) { // setup volume flow rate report for actual/current density
                    SetupOutputVariable(state,
                                        "System Node Current Density Volume Flow Rate",
                                        Constant::Units::m3_s,
                                        node->VolFlowRateCrntRho,
                                        OutputProcessor::TimeStepType::System,
                                        OutputProcessor::StoreType::Average,
                                        node->Name);
                    SetupOutputVariable(state,
                                        "System Node Current Density",
                                        Constant::Units::kg_m3,
                                        node->Density,
                                        OutputProcessor::TimeStepType::System,
                                        OutputProcessor::StoreType::Average,
                                        node->Name);
                    SetupOutputVariable(state,
                                        "System Node Specific Heat",
                                        Constant::Units::J_kgK,
                                        node->SpecificHeat,
                                        OutputProcessor::TimeStepType::System,
                                        OutputProcessor::StoreType::Average,
                                        node->Name);
                }

                SetupOutputVariable(state,
                                    "System Node Enthalpy",
                                    Constant::Units::J_kg,
                                    node->ReportEnthalpy,
                                    OutputProcessor::TimeStepType::System,
                                    OutputProcessor::StoreType::Average,
                                    node->Name);
                SetupOutputVariable(state,
                                    "System Node Wetbulb Temperature",
                                    Constant::Units::C,
                                    node->WetBulbTemp,
                                    OutputProcessor::TimeStepType::System,
                                    OutputProcessor::StoreType::Average,
                                    node->Name);
                SetupOutputVariable(state,
                                    "System Node Dewpoint Temperature",
                                    Constant::Units::C,
                                    node->AirDewPointTemp,
                                    OutputProcessor::TimeStepType::System,
                                    OutputProcessor::StoreType::Average,
                                    node->Name);
                SetupOutputVariable(state,
                                    "System Node Wind Speed",
                                    Constant::Units::m_s,
                                    node->OutAirWindSpeed,
                                    OutputProcessor::TimeStepType::System,
                                    OutputProcessor::StoreType::Average,
                                    node->Name);
                SetupOutputVariable(state,
                                    "System Node Wind Direction",
                                    Constant::Units::deg,
                                    node->OutAirWindDir,
                                    OutputProcessor::TimeStepType::System,
                                    OutputProcessor::StoreType::Average,
                                    node->Name);
                SetupOutputVariable(state,
                                    "System Node Quality",
                                    Constant::Units::None,
                                    node->Quality,
                                    OutputProcessor::TimeStepType::System,
                                    OutputProcessor::StoreType::Average,
                                    node->Name);
                SetupOutputVariable(state,
                                    "System Node Height",
                                    Constant::Units::m,
                                    node->Height,
                                    OutputProcessor::TimeStepType::System,
                                    OutputProcessor::StoreType::Average,
                                    node->Name);
                if (state.dataGlobal->DisplayAdvancedReportVariables) {
                    SetupOutputVariable(state,
                                        "System Node Minimum Temperature",
                                        Constant::Units::C,
                                        node->TempMin,
                                        OutputProcessor::TimeStepType::System,
                                        OutputProcessor::StoreType::Average,
                                        node->Name);
                    SetupOutputVariable(state,
                                        "System Node Maximum Temperature",
                                        Constant::Units::C,
                                        node->TempMax,
                                        OutputProcessor::TimeStepType::System,
                                        OutputProcessor::StoreType::Average,
                                        node->Name);
                    SetupOutputVariable(state,
                                        "System Node Minimum Limit Mass Flow Rate",
                                        Constant::Units::kg_s,
                                        node->MassFlowRateMin,
                                        OutputProcessor::TimeStepType::System,
                                        OutputProcessor::StoreType::Average,
                                        node->Name);
                    SetupOutputVariable(state,
                                        "System Node Maximum Limit Mass Flow Rate",
                                        Constant::Units::kg_s,
                                        node->MassFlowRateMax,
                                        OutputProcessor::TimeStepType::System,
                                        OutputProcessor::StoreType::Average,
                                        node->Name);
                    SetupOutputVariable(state,
                                        "System Node Minimum Available Mass Flow Rate",
                                        Constant::Units::kg_s,
                                        node->MassFlowRateMinAvail,
                                        OutputProcessor::TimeStepType::System,
                                        OutputProcessor::StoreType::Average,
                                        node->Name);
                    SetupOutputVariable(state,
                                        "System Node Maximum Available Mass Flow Rate",
                                        Constant::Units::kg_s,
                                        node->MassFlowRateMaxAvail,
                                        OutputProcessor::TimeStepType::System,
                                        OutputProcessor::StoreType::Average,
                                        node->Name);
                    SetupOutputVariable(state,
                                        "System Node Setpoint Mass Flow Rate",
                                        Constant::Units::kg_s,
                                        node->MassFlowRateSetPoint,
                                        OutputProcessor::TimeStepType::System,
                                        OutputProcessor::StoreType::Average,
                                        node->Name);
                    SetupOutputVariable(state,
                                        "System Node Requested Mass Flow Rate",
                                        Constant::Units::kg_s,
                                        node->MassFlowRateRequest,
                                        OutputProcessor::TimeStepType::System,
                                        OutputProcessor::StoreType::Average,
                                        node->Name);
                    SetupOutputVariable(state,
                                        "System Node Last Timestep Temperature",
                                        Constant::Units::C,
                                        node->TempLastTimestep,
                                        OutputProcessor::TimeStepType::System,
                                        OutputProcessor::StoreType::Average,
                                        node->Name);
                    SetupOutputVariable(state,
                                        "System Node Last Timestep Enthalpy",
                                        Constant::Units::J_kg,
                                        node->EnthalpyLastTimestep,
                                        OutputProcessor::TimeStepType::System,
                                        OutputProcessor::StoreType::Average,
                                        node->Name);
                }
                if (state.dataContaminantBalance->Contaminant.CO2Simulation) {
                    SetupOutputVariable(state,
                                        "System Node CO2 Concentration",
                                        Constant::Units::ppm,
                                        node->CO2,
                                        OutputProcessor::TimeStepType::System,
                                        OutputProcessor::StoreType::Average,
                                        node->Name);
                }
                if (state.dataContaminantBalance->Contaminant.GenericContamSimulation) {
                    SetupOutputVariable(state,
                                        "System Node Generic Air Contaminant Concentration",
                                        Constant::Units::ppm,
                                        node->GenContam,
                                        OutputProcessor::TimeStepType::System,
                                        OutputProcessor::StoreType::Average,
                                        node->Name);
                }
            }
        }
        state.dataNodeInputMgr->NodeVarsSetup = true;

        print(state.files.bnd, "{}\n", "! This file shows details about the branches, nodes, and other");
        print(state.files.bnd, "{}\n", "! elements of the flow connections.");
        print(state.files.bnd, "{}\n", "! This file is intended for use in \"debugging\" potential problems");
        print(state.files.bnd, "{}\n", "! that may also be detected by the program, but may be more easily");
        print(state.files.bnd, "{}\n", "! identified by \"eye\".");
        print(state.files.bnd, "{}\n", "! This file is also intended to support software which draws a");
        print(state.files.bnd, "{}\n", "! schematic diagram of the HVAC system.");
        print(state.files.bnd, "{}\n", "! ===============================================================");
        // Show the node names on the Branch-Node Details file
        static constexpr std::string_view Format_700("! #Nodes,<Number of Unique Nodes>");
        print(state.files.bnd, "{}\n", Format_700);
        print(state.files.bnd, " #Nodes,{}\n", state.dataNodeInputMgr->NumOfUniqueNodeNames);
        if (state.dataNodeInputMgr->NumOfUniqueNodeNames > 0) {
            static constexpr std::string_view Format_702(
                "! <Node>,<NodeNumber>,<Node Name>,<Node Fluid Type>,<# Times Node Referenced After Definition>");
            print(state.files.bnd, "{}\n", Format_702);
        }
        int Count0 = 0;
        for (int NumNode = 1; NumNode <= state.dataNodeInputMgr->NumOfUniqueNodeNames; ++NumNode) {
            auto const *node = dln->nodes(NumNode);
            print(state.files.bnd,
                  " Node,{},{},{},{}\n",
                  NumNode,
                  node->Name,
                  fluidTypeNames[(int)node->fluidType],
                  state.dataNodeInputMgr->NodeRef(NumNode));
            if (state.dataNodeInputMgr->NodeRef(NumNode) == 0) ++Count0;
        }
        // Show suspicious node names on the Branch-Node Details file
        if (Count0 > 0) {
            print(state.files.bnd, "{}\n", "! ===============================================================");
            print(state.files.bnd, "{}\n", "! Suspicious nodes have 0 references.  It is normal for some nodes, however.");
            print(state.files.bnd, "{}\n", "! Listing nodes with 0 references (culled from previous list):");
            static constexpr std::string_view Format_703(
                "! <Suspicious Node>,<NodeNumber>,<Node Name>,<Node Fluid Type>,<# Times Node Referenced After Definition>");
            print(state.files.bnd, "{}\n", Format_703);
            for (int NumNode = 1; NumNode <= state.dataNodeInputMgr->NumOfUniqueNodeNames; ++NumNode) {
                auto const *node = dln->nodes(NumNode);
                if (state.dataNodeInputMgr->NodeRef(NumNode) > 0) continue;
                print(state.files.bnd,
                      " Suspicious Node,{},{},{},{}\n",
                      NumNode,
                      node->Name,
                      fluidTypeNames[(int)node->fluidType],
                      state.dataNodeInputMgr->NodeRef(NumNode));
            }
        }
    }
}

void GetNodeListsInput(EnergyPlusData &state, bool &ErrorsFound) // Set to true when requested Node List not found, unchanged otherwise
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   September 1999
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine gets the Node Lists from the IDF and fills the
    // Node List Data Structure.

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("GetNodeListsInput: ");
    static std::string const CurrentModuleObject("NodeList");

    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int NumAlphas;  // Number of alphas in IDF item
    int NumNumbers; // Number of numerics in IDF item
    int IOStatus;   // IOStatus for IDF item (not checked)
    int NCount;     // Actual number of node lists
    bool flagError; // true when error node list name should be output
    Array1D_string cAlphas;
    Array1D<Real64> rNumbers;

    auto &dln = state.dataLoopNodes;
    
    bool localErrorsFound(false);
    state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, CurrentModuleObject, NCount, NumAlphas, NumNumbers);
    cAlphas.allocate(NumAlphas);
    rNumbers.allocate(NumNumbers);
    state.dataNodeInputMgr->NumOfNodeLists = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, CurrentModuleObject);
    state.dataNodeInputMgr->NodeLists.allocate(state.dataNodeInputMgr->NumOfNodeLists);
    for (int i = 1; i <= state.dataNodeInputMgr->NumOfNodeLists; ++i) {
        state.dataNodeInputMgr->NodeLists(i).Name.clear();
        state.dataNodeInputMgr->NodeLists(i).NumNodes = 0;
    }

    NCount = 0;
    for (int Loop = 1; Loop <= state.dataNodeInputMgr->NumOfNodeLists; ++Loop) {
        state.dataInputProcessing->inputProcessor->getObjectItem(
            state, CurrentModuleObject, Loop, cAlphas, NumAlphas, rNumbers, NumNumbers, IOStatus);
        if (Util::IsNameEmpty(state, cAlphas(1), CurrentModuleObject, localErrorsFound)) continue;

        ++NCount;
        state.dataNodeInputMgr->NodeLists(NCount).Name = cAlphas(1);
        state.dataNodeInputMgr->NodeLists(NCount).NodeNames.allocate(NumAlphas - 1);
        state.dataNodeInputMgr->NodeLists(NCount).NodeNames = "";
        state.dataNodeInputMgr->NodeLists(NCount).NodeNums.allocate(NumAlphas - 1);
        state.dataNodeInputMgr->NodeLists(NCount).NodeNums = 0;
        state.dataNodeInputMgr->NodeLists(NCount).NumNodes = NumAlphas - 1;
        if (NumAlphas <= 1) {
            if (NumAlphas == 1) {
                ShowSevereError(state, format("{}{}=\"{}\" does not have any nodes.", RoutineName, CurrentModuleObject, cAlphas(1)));
            } else {
                ShowSevereError(state, format("{}{}=<blank> does not have any nodes or nodelist name.", RoutineName, CurrentModuleObject));
            }
            localErrorsFound = true;
            continue;
        }
        //  Put all in, then determine unique
        for (int Loop1 = 1; Loop1 <= NumAlphas - 1; ++Loop1) {
            state.dataNodeInputMgr->NodeLists(NCount).NodeNames(Loop1) = cAlphas(Loop1 + 1);
            if (cAlphas(Loop1 + 1).empty()) {
                ShowWarningError(state, format("{}{}=\"{}\", blank node name in list.", RoutineName, CurrentModuleObject, cAlphas(1)));
                --state.dataNodeInputMgr->NodeLists(NCount).NumNodes;
                if (state.dataNodeInputMgr->NodeLists(NCount).NumNodes <= 0) {
                    ShowSevereError(state, format("{}{}=\"{}\" does not have any nodes.", RoutineName, CurrentModuleObject, cAlphas(1)));
                    localErrorsFound = true;
                    break;
                }
                continue;
            }
            state.dataNodeInputMgr->NodeLists(NCount).NodeNums(Loop1) = AssignNodeNumber(
                state, state.dataNodeInputMgr->NodeLists(NCount).NodeNames(Loop1), FluidType::Blank, localErrorsFound);
            if (Util::SameString(state.dataNodeInputMgr->NodeLists(NCount).NodeNames(Loop1), state.dataNodeInputMgr->NodeLists(NCount).Name)) {
                ShowSevereError(state, format("{}{}=\"{}\", invalid node name in list.", RoutineName, CurrentModuleObject, cAlphas(1)));
                ShowContinueError(state, format("... Node {} Name=\"{}\", duplicates NodeList Name.", Loop1, cAlphas(Loop1 + 1)));
                localErrorsFound = true;
            }
        }
        // Error on any duplicates
        flagError = true;
        for (int Loop1 = 1; Loop1 <= state.dataNodeInputMgr->NodeLists(NCount).NumNodes; ++Loop1) {
            int nodeNum1 = state.dataNodeInputMgr->NodeLists(NCount).NodeNums(Loop1);
            auto const *node1 = dln->nodes(nodeNum1);
            for (int Loop2 = Loop1 + 1; Loop2 <= state.dataNodeInputMgr->NodeLists(NCount).NumNodes; ++Loop2) {
                int nodeNum2 = state.dataNodeInputMgr->NodeLists(NCount).NodeNums(Loop2);
                if (nodeNum1 != nodeNum2)
                    continue;
                auto const *node2 = dln->nodes(nodeNum2);
                if (flagError) { // only list nodelist name once
                    ShowSevereError(state, format("{}{}=\"{}\" has duplicate nodes:", RoutineName, CurrentModuleObject, cAlphas(1)));
                    flagError = false;
                }
                ShowContinueError(state,
                                  format("...list item={}, \"{}\", duplicate list item={}, \"{}\".",
                                         Loop1,
                                         node1->Name,
                                         Loop2,
                                         node2->Name));
                localErrorsFound = true;
            }
        }
    }

    for (int Loop = 1; Loop <= state.dataNodeInputMgr->NumOfNodeLists; ++Loop) {
        for (int Loop2 = 1; Loop2 <= state.dataNodeInputMgr->NodeLists(Loop).NumNodes; ++Loop2) {
            for (int Loop1 = 1; Loop1 <= state.dataNodeInputMgr->NumOfNodeLists; ++Loop1) {
                if (Loop == Loop1) continue; // within a nodelist have already checked to see if node name duplicates nodelist name
                if (!Util::SameString(state.dataNodeInputMgr->NodeLists(Loop).NodeNames(Loop2), state.dataNodeInputMgr->NodeLists(Loop1).Name))
                    continue;
                ShowSevereError(
                    state,
                    format(
                        "{}{}=\"{}\", invalid node name in list.", RoutineName, CurrentModuleObject, state.dataNodeInputMgr->NodeLists(Loop1).Name));
                ShowContinueError(
                    state,
                    format("... Node {} Name=\"{}\", duplicates NodeList Name.", Loop2, state.dataNodeInputMgr->NodeLists(Loop).NodeNames(Loop2)));
                ShowContinueError(state, format("... NodeList=\"{}\", is duplicated.", state.dataNodeInputMgr->NodeLists(Loop1).Name));
                ShowContinueError(state, "... Items in NodeLists must not be the name of another NodeList.");
                localErrorsFound = true;
            }
        }
    }

    cAlphas.deallocate();
    rNumbers.deallocate();

    if (localErrorsFound) {
        ShowFatalError(state, format("{}{}: Error getting input - causes termination.", RoutineName, CurrentModuleObject));
        ErrorsFound = true;
    }
}

int AssignNodeNumber(EnergyPlusData &state,
                     std::string const &Name,                         // Name for assignment
                     FluidType const nodeFluidType, // must be valid
                     bool &ErrorsFound)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda K. Lawrie
    //       DATE WRITTEN   September 1999
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS FUNCTION:
    // This function assigns a node number to this name.

    // METHODOLOGY EMPLOYED:
    // Look to see if a name has already been entered.  Use the index of
    // the array as the node number, if there.

    // Return value
    auto &dln = state.dataLoopNodes;
    
    if (nodeFluidType != FluidType::Air && nodeFluidType != FluidType::Water &&
        nodeFluidType != FluidType::Electric && nodeFluidType != FluidType::Steam &&
        nodeFluidType != FluidType::Blank) {
        ShowSevereError(state, format("AssignNodeNumber: Invalid FluidType={}", nodeFluidType));
        ErrorsFound = true;
        ShowFatalError(state, "AssignNodeNumber: Preceding issue causes termination.");
    }

    if (auto found  = dln->nodeMap.find(Name); found != dln->nodeMap.end()) {
        auto *node = dln->nodes(found->second);
        ++state.dataNodeInputMgr->NodeRef(found->second);
        if (nodeFluidType != FluidType::Blank) {
            if (node->fluidType != nodeFluidType && node->fluidType != FluidType::Blank) {
                ShowSevereError(state, format("Existing Fluid type for node, incorrect for request. Node={}", node->Name));
                ShowContinueError(state, format("Existing Fluid type={}, Requested Fluid Type={}",
                                                fluidTypeNames[(int)node->fluidType], fluidTypeNames[(int)nodeFluidType]));
                ErrorsFound = true;
            }
        }
        if (node->fluidType == Node::FluidType::Blank) {
            node->fluidType = nodeFluidType;
        }
        return found->second;
    } else {
        auto *node = new NodeData;
        node->Name = Name;
        node->fluidType = nodeFluidType;
        
        dln->nodes.push_back(node);
        dln->nodeMap.insert_or_assign(Name, dln->nodes.size());

        return dln->nodes.size();
    }
}

int GetSingleNode(EnergyPlusData &state,
                      std::string const &NodeName,
                      bool &errFlag,
                      Node::ConnObjType const NodeObjectType, // Node Object Type (i.e. "Chiller:Electric")
                      std::string const &NodeObjectName,                       // Node Object Name (i.e. "MyChiller")
                      Node::FluidType const nodeFluidType,         // Fluidtype for checking/setting node FluidType
                      Node::ConnType const nodeConnectionType,   // Node Connection Type (see DataLoopNode)
                      CompFluidStream const NodeFluidStream,                   // Which Fluid Stream
                      bool const ObjectIsParent,                               // True/False
                      std::string_view const InputFieldName                    // Input Field Name
)
{

    // FUNCTION INFORMATION:
    //       AUTHOR         Linda K. Lawrie; adapted from GasAbsorptionChiller;Jason Glazer
    //       DATE WRITTEN   December 2001

    // PURPOSE OF THIS FUNCTION:
    // This function gets a single node (or error message results) using the
    // node id from the input file.

    static constexpr std::string_view RoutineName("GetSingleNode: ");

    int NumNodes;
    int NumParams;
    int NumAlphas;
    int NumNums;

    std::string_view const objTypeStr = Node::connObjTypeNames[(int)NodeObjectType];

    if (state.dataNodeInputMgr->GetSingleNodeFirstTime) {
        state.dataInputProcessing->inputProcessor->getObjectDefMaxArgs(state, "NodeList", NumParams, NumAlphas, NumNums);
        state.dataNodeInputMgr->GetSingleNodeNodeNums.dimension(NumParams, 0);
        state.dataNodeInputMgr->GetSingleNodeFirstTime = false;
    }

    GetNodeNums(state,
                NodeName,
                NumNodes,
                state.dataNodeInputMgr->GetSingleNodeNodeNums,
                errFlag,
                nodeFluidType,
                NodeObjectType,
                NodeObjectName,
                nodeConnectionType,
                NodeFluidStream,
                ObjectIsParent,
                false,
                InputFieldName);

    if (NumNodes > 1) {
        ShowSevereError(state, format("{}{}=\"{}=\", invalid data.", RoutineName, objTypeStr, NodeObjectName));
        if (!InputFieldName.empty()) {
            ShowContinueError(state, fmt::format("...Ref field={}", InputFieldName));
        }
        ShowContinueError(state, format("Only 1st Node used from NodeList=\"{}\".", NodeName));
        ShowContinueError(state, "...a Nodelist may not be valid in this context.");
        errFlag = true;
    } else if (NumNodes == 0) {
        state.dataNodeInputMgr->GetSingleNodeNodeNums(1) = 0;
    }

    return state.dataNodeInputMgr->GetSingleNodeNodeNums(1);
}

void InitUniqueNodeCheck(EnergyPlusData &state, std::string const &ContextName)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   November 2002
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine begins a process of checking for unique node names
    // in a sequence of nodes.
#ifdef GET_OUT
    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    bool errFlag(false);

    // Begin set up of Uniqueness context

    if (state.dataNodeInputMgr->GetNodeInputFlag) {
        GetNodeListsInput(state, errFlag);
        state.dataNodeInputMgr->GetNodeInputFlag = false;
    }

    if (!state.dataNodeInputMgr->CurCheckContextName.empty()) {
        ShowFatalError(state,
                       format("Init Uniqueness called for \"{}, but checks for \"{}\" was already in progress.",
                              ContextName,
                              state.dataNodeInputMgr->CurCheckContextName));
    }
    if (ContextName.empty()) {
        ShowFatalError(state, "Init Uniqueness called with Blank Context Name");
    }
    state.dataNodeInputMgr->NumCheckNodes = 0;
    state.dataNodeInputMgr->MaxCheckNodes = 100;
    state.dataNodeInputMgr->CurCheckContextName = ContextName;
#endif // GET_OUT
}

void CheckUniqueNodeNames(
    EnergyPlusData &state, std::string const &NodeTypes, bool &ErrorsFound, std::string const &CheckName, std::string const &ObjectName)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   November 2002
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine checks the appropriate input argument for uniqueness.
    // Call CheckUniqueNodes(NodeTypes,CheckType,ErrorsFound,CheckName,CheckNumber)
    // NodeTypes - used in error message (if any produced)
    // ErrorsFound - true if error found by routine
    // CheckName - NodeName entered
    // ObjectName - "Name" field of object (i.e., CurCheckContextName)

    // METHODOLOGY EMPLOYED:
    // checks the current list of items for this (again)
#ifdef GET_OUT
    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int Found;

    if (!CheckName.empty()) {
        Found = Util::FindItemInList(CheckName, state.dataNodeInputMgr->UniqueNodeNames, state.dataNodeInputMgr->NumCheckNodes);
        if (Found != 0) {
            ShowSevereError(state, format("{}=\"{}\", duplicate node names found.", state.dataNodeInputMgr->CurCheckContextName, ObjectName));
            ShowContinueError(state, format("...for Node Type(s)={}, duplicate node name=\"{}\".", NodeTypes, CheckName));
            ShowContinueError(state, "...Nodes must be unique across instances of this object.");
            //          CALL ShowSevereError(state, 'Node Types='//TRIM(NodeTypes)//', Non Unique Name found='//TRIM(CheckName))
            //          CALL ShowContinueError(state, 'Context='//TRIM(CurCheckContextName))
            ErrorsFound = true;
        } else {
            ++state.dataNodeInputMgr->NumCheckNodes;
            if (state.dataNodeInputMgr->NumCheckNodes > state.dataNodeInputMgr->MaxCheckNodes) {
                state.dataNodeInputMgr->UniqueNodeNames.redimension(state.dataNodeInputMgr->MaxCheckNodes += 100);
            }
            state.dataNodeInputMgr->UniqueNodeNames(state.dataNodeInputMgr->NumCheckNodes) = CheckName;
        }
    }
#endif // GET_OUT    
}

void CheckUniqueNodeNumbers(
    EnergyPlusData &state, std::string const &NodeTypes, bool &ErrorsFound, int const CheckNumber, std::string const &ObjectName)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   November 2002
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine checks the appropriate input argument for uniqueness.
    // Call CheckUniqueNodes(NodeTypes,CheckType,ErrorsFound,CheckName,CheckNumber)
    // NodeTypes - used in error message (if any produced)
    // ErrorsFound - true if error found by routine
    // CheckNumber - Node Number entered
    // ObjectName - "Name" field of object (i.e., CurCheckContextName)

    // METHODOLOGY EMPLOYED:
    // checks the current list of items for this (again)
#ifdef GET_OUT
    // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    int Found;

    if (CheckNumber != 0) {
        Found = Util::FindItemInList(
            dln->NodeID(CheckNumber), state.dataNodeInputMgr->UniqueNodeNames, state.dataNodeInputMgr->NumCheckNodes);
        if (Found != 0) {
            ShowSevereError(state, format("{}=\"{}\", duplicate node names found.", state.dataNodeInputMgr->CurCheckContextName, ObjectName));
            ShowContinueError(state,
                              format("...for Node Type(s)={}, duplicate node name=\"{}\".", NodeTypes, dln->NodeID(CheckNumber)));
            ShowContinueError(state, "...Nodes must be unique across instances of this object.");
            ErrorsFound = true;
        } else {
            ++state.dataNodeInputMgr->NumCheckNodes;
            if (state.dataNodeInputMgr->NumCheckNodes > state.dataNodeInputMgr->MaxCheckNodes) {
                state.dataNodeInputMgr->UniqueNodeNames.redimension(state.dataNodeInputMgr->MaxCheckNodes += 100);
            }
            state.dataNodeInputMgr->UniqueNodeNames(state.dataNodeInputMgr->NumCheckNodes) = dln->NodeID(CheckNumber);
        }
    }
#endif // GET_OUT
}

void EndUniqueNodeCheck(EnergyPlusData &state, std::string const &ContextName)
{
#ifdef GET_OUT
    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   November 2002
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine marks the end of a unique node check.

    if (state.dataNodeInputMgr->CurCheckContextName != ContextName) {
        ShowFatalError(state,
                       format("End Uniqueness called for \"{}, but checks for \"{}\" was in progress.",
                              ContextName,
                              state.dataNodeInputMgr->CurCheckContextName));
    }
    if (ContextName.empty()) {
        ShowFatalError(state, "End Uniqueness called with Blank Context Name");
    }
    state.dataNodeInputMgr->CurCheckContextName = std::string();
    if (allocated(state.dataNodeInputMgr->UniqueNodeNames)) {
        state.dataNodeInputMgr->UniqueNodeNames.deallocate();
    }
#endif // GET_OUT
}

void CalcMoreNodeInfo(EnergyPlusData &state)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Fred Buhl
    //       DATE WRITTEN   January 2004
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // Calculate additional node information for reporting

    // METHODOLOGY EMPLOYED:
    // Input is the existing node data plus environment variables. Output is
    // stored in MoreNodeInfo.

    // Using/Aliasing
    using FluidProperties::GetDensityGlycol;
    using FluidProperties::GetSatDensityRefrig;
    using FluidProperties::GetSatEnthalpyRefrig;
    using FluidProperties::GetSpecificHeatGlycol;
    using Psychrometrics::CPCW;
    using Psychrometrics::PsyCpAirFnW;
    using Psychrometrics::PsyHFnTdbW;
    using Psychrometrics::PsyRhFnTdbWPb;
    using Psychrometrics::PsyRhoAirFnPbTdbW;
    using Psychrometrics::PsyTdpFnWPb;
    using Psychrometrics::PsyTwbFnTdbWPb;
    using Psychrometrics::RhoH2O;
    using ScheduleManager::GetCurrentScheduleValue;

    // SUBROUTINE PARAMETER DEFINITIONS:
    static constexpr std::string_view RoutineName("CalcMoreNodeInfo");
    static std::string const NodeReportingCalc("NodeReportingCalc:");

    auto &RhoAirStdInit = state.dataNodeInputMgr->RhoAirStdInit;
    auto &RhoWaterStdInit = state.dataNodeInputMgr->RhoWaterStdInit;
    auto &NodeWetBulbSchedPtr = state.dataNodeInputMgr->NodeWetBulbSchedPtr;
    auto &NodeRelHumidityRepReq = state.dataNodeInputMgr->NodeRelHumidityRepReq;
    auto &NodeRelHumiditySchedPtr = state.dataNodeInputMgr->NodeRelHumiditySchedPtr;
    auto &NodeDewPointRepReq = state.dataNodeInputMgr->NodeDewPointRepReq;
    auto &NodeDewPointSchedPtr = state.dataNodeInputMgr->NodeDewPointSchedPtr;
    auto &NodeSpecificHeatRepReq = state.dataNodeInputMgr->NodeSpecificHeatRepReq;
    auto &NodeSpecificHeatSchedPtr = state.dataNodeInputMgr->NodeSpecificHeatSchedPtr;
    auto &nodeReportingStrings = state.dataNodeInputMgr->nodeReportingStrings;
    auto &nodeFluidNames = state.dataNodeInputMgr->nodeFluidNames;
    bool ReportWetBulb;
    bool ReportRelHumidity;
    bool ReportDewPoint;
    bool ReportSpecificHeat;
    Real64 SteamDensity;
    Real64 EnthSteamInDry;
    Real64 RhoAirCurrent; // temporary value for current air density f(baro, db , W)
    Real64 rho;
    Real64 Cp;
    Real64 rhoStd;

    auto &dln = state.dataLoopNodes;
    if (state.dataNodeInputMgr->CalcMoreNodeInfoMyOneTimeFlag) {
        RhoAirStdInit = state.dataEnvrn->StdRhoAir;
        RhoWaterStdInit = RhoH2O(Constant::InitConvTemp);
        state.dataNodeInputMgr->NodeWetBulbRepReq.allocate(dln->nodes.size());
        NodeWetBulbSchedPtr.allocate(dln->nodes.size());
        NodeRelHumidityRepReq.allocate(dln->nodes.size());
        NodeRelHumiditySchedPtr.allocate(dln->nodes.size());
        NodeDewPointRepReq.allocate(dln->nodes.size());
        NodeDewPointSchedPtr.allocate(dln->nodes.size());
        NodeSpecificHeatRepReq.allocate(dln->nodes.size());
        NodeSpecificHeatSchedPtr.allocate(dln->nodes.size());
        nodeReportingStrings.reserve(dln->nodes.size());
        nodeFluidNames.reserve(dln->nodes.size());
        state.dataNodeInputMgr->NodeWetBulbRepReq = false;
        NodeWetBulbSchedPtr = 0;
        NodeRelHumidityRepReq = false;
        NodeRelHumiditySchedPtr = 0;
        NodeDewPointRepReq = false;
        NodeDewPointSchedPtr = 0;
        NodeSpecificHeatRepReq = false;
        NodeSpecificHeatSchedPtr = 0;

        for (int iNode = 1; iNode <= dln->nodes.isize(); ++iNode) {
            auto const *node = dln->nodes(iNode);
            nodeReportingStrings.push_back(std::string(NodeReportingCalc + node->Name));
            nodeFluidNames.push_back(FluidProperties::GetGlycolNameByIndex(state, node->FluidIndex));

            for (auto const *reqVar : state.dataOutputProcessor->reqVars) {
                if (Util::SameString(reqVar->key, node->Name) || reqVar->key.empty()) {
                    if (Util::SameString(reqVar->name, "System Node Wetbulb Temperature")) {
                        state.dataNodeInputMgr->NodeWetBulbRepReq(iNode) = true;
                        NodeWetBulbSchedPtr(iNode) = reqVar->SchedPtr;
                    } else if (Util::SameString(reqVar->name, "System Node Relative Humidity")) {
                        NodeRelHumidityRepReq(iNode) = true;
                        NodeRelHumiditySchedPtr(iNode) = reqVar->SchedPtr;
                    } else if (Util::SameString(reqVar->name, "System Node Dewpoint Temperature")) {
                        NodeDewPointRepReq(iNode) = true;
                        NodeDewPointSchedPtr(iNode) = reqVar->SchedPtr;
                    } else if (Util::SameString(reqVar->name, "System Node Specific Heat")) {
                        NodeSpecificHeatRepReq(iNode) = true;
                        NodeSpecificHeatSchedPtr(iNode) = reqVar->SchedPtr;
                    }
                }
            }
            if (EMSManager::CheckIfNodeMoreInfoSensedByEMS(state, iNode, "System Node Wetbulb Temperature")) {
                state.dataNodeInputMgr->NodeWetBulbRepReq(iNode) = true;
                NodeWetBulbSchedPtr(iNode) = 0;
            }
            if (EMSManager::CheckIfNodeMoreInfoSensedByEMS(state, iNode, "System Node Relative Humidity")) {
                NodeRelHumidityRepReq(iNode) = true;
                NodeRelHumiditySchedPtr(iNode) = 0;
            }
            if (EMSManager::CheckIfNodeMoreInfoSensedByEMS(state, iNode, "System Node Dewpoint Temperature")) {
                NodeDewPointRepReq(iNode) = true;
                NodeDewPointSchedPtr(iNode) = 0;
            }
            if (EMSManager::CheckIfNodeMoreInfoSensedByEMS(state, iNode, "System Node Specific Heat")) {
                NodeSpecificHeatRepReq(iNode) = true;
                NodeSpecificHeatSchedPtr(iNode) = 0;
            }
        }
        state.dataNodeInputMgr->CalcMoreNodeInfoMyOneTimeFlag = false;
    }

    for (int iNode = 1; iNode <= dln->nodes.isize(); ++iNode) {
        auto *node = dln->nodes(iNode);
        ReportWetBulb = false;
        ReportRelHumidity = false;
        ReportDewPoint = false;
        ReportSpecificHeat = false;
        if (state.dataNodeInputMgr->NodeWetBulbRepReq(iNode) && NodeWetBulbSchedPtr(iNode) > 0) {
            ReportWetBulb = (GetCurrentScheduleValue(state, NodeWetBulbSchedPtr(iNode)) > 0.0);
        } else if (state.dataNodeInputMgr->NodeWetBulbRepReq(iNode) && NodeWetBulbSchedPtr(iNode) == 0) {
            ReportWetBulb = true;
        } else if (node->SPMNodeWetBulbRepReq) {
            ReportWetBulb = true;
        }
        if (NodeRelHumidityRepReq(iNode) && NodeRelHumiditySchedPtr(iNode) > 0) {
            ReportRelHumidity = (GetCurrentScheduleValue(state, NodeRelHumiditySchedPtr(iNode)) > 0.0);
        } else if (NodeRelHumidityRepReq(iNode) && NodeRelHumiditySchedPtr(iNode) == 0) {
            ReportRelHumidity = true;
        }
        if (NodeDewPointRepReq(iNode) && NodeDewPointSchedPtr(iNode) > 0) {
            ReportDewPoint = (GetCurrentScheduleValue(state, NodeDewPointSchedPtr(iNode)) > 0.0);
        } else if (NodeDewPointRepReq(iNode) && NodeDewPointSchedPtr(iNode) == 0) {
            ReportDewPoint = true;
        }
        if (NodeSpecificHeatRepReq(iNode) && NodeSpecificHeatSchedPtr(iNode) > 0) {
            ReportSpecificHeat = (GetCurrentScheduleValue(state, NodeSpecificHeatSchedPtr(iNode)) > 0.0);
        } else if (NodeSpecificHeatRepReq(iNode) && NodeSpecificHeatSchedPtr(iNode) == 0) {
            ReportSpecificHeat = true;
        }
        // calculate the volume flow rate
        if (node->fluidType == FluidType::Air) {
            node->VolFlowRateStdRho = node->MassFlowRate / RhoAirStdInit;
            // if Node%Press was reliable could be used here.
            RhoAirCurrent = PsyRhoAirFnPbTdbW(state, state.dataEnvrn->OutBaroPress, node->Temp, node->HumRat);
            node->Density = RhoAirCurrent;
            if (RhoAirCurrent != 0.0)
                node->VolFlowRateCrntRho = node->MassFlowRate / RhoAirCurrent;
            node->ReportEnthalpy = PsyHFnTdbW(node->Temp, node->HumRat);
            if (ReportWetBulb) {
                // if Node%Press was reliable could be used here.
                node->WetBulbTemp = PsyTwbFnTdbWPb(state, node->Temp, node->HumRat, state.dataEnvrn->OutBaroPress, nodeReportingStrings[iNode - 1]);
            } else {
                node->WetBulbTemp = 0.0;
            }
            if (ReportDewPoint) {
                node->AirDewPointTemp = PsyTdpFnWPb(state, node->HumRat, state.dataEnvrn->OutBaroPress);
            } else {
                node->AirDewPointTemp = 0.0;
            }
            if (ReportRelHumidity) {
                // if Node%Press was reliable could be used here.
                // following routines don't issue psych errors and may be more reliable.
                node->RelHumidity = 100.0 * PsyRhFnTdbWPb(state, node->Temp, node->HumRat, state.dataEnvrn->OutBaroPress, nodeReportingStrings[iNode - 1]);
            } else {
                node->RelHumidity = 0.0;
            }
            if (ReportSpecificHeat) { // only call psych routine if needed.
                node->SpecificHeat = PsyCpAirFnW(node->HumRat);
            } else {
                node->SpecificHeat = 0.0;
            }
        } else if (node->fluidType == FluidType::Water) {

            if (!((node->FluidIndex > 0) && (node->FluidIndex <= state.dataFluidProps->NumOfGlycols))) {
                rho = RhoWaterStdInit;
                rhoStd = RhoWaterStdInit;
                Cp = CPCW(node->Temp);
            } else {
                Cp = GetSpecificHeatGlycol(state, nodeFluidNames[iNode - 1], node->Temp, node->FluidIndex, nodeReportingStrings[iNode - 1]);
                rhoStd = GetDensityGlycol(state, nodeFluidNames[iNode - 1], Constant::InitConvTemp, node->FluidIndex, nodeReportingStrings[iNode - 1]);
                rho = GetDensityGlycol(state, nodeFluidNames[iNode - 1], node->Temp, node->FluidIndex, nodeReportingStrings[iNode - 1]);
            }

            node->VolFlowRateStdRho = node->MassFlowRate / rhoStd;
            node->VolFlowRateCrntRho = node->MassFlowRate / rho;
            node->Density = rho;
            node->ReportEnthalpy = Cp * node->Temp;
            node->SpecificHeat = Cp; // always fill since cp already always being calculated anyway
            node->WetBulbTemp = 0.0;
            node->RelHumidity = 100.0;
        } else if (node->fluidType == Node::FluidType::Steam) {
            if (node->Quality == 1.0) {
                SteamDensity = GetSatDensityRefrig(state, fluidNameSteam, node->Temp, node->Quality, node->FluidIndex, RoutineName);
                EnthSteamInDry = GetSatEnthalpyRefrig(state, fluidNameSteam, node->Temp, node->Quality, node->FluidIndex, RoutineName);
                node->VolFlowRateStdRho = node->MassFlowRate / SteamDensity;
                node->ReportEnthalpy = EnthSteamInDry;
                node->WetBulbTemp = 0.0;
                node->RelHumidity = 0.0;
            } else if (node->Quality == 0.0) { // The node has condensate water through it
                node->VolFlowRateStdRho = node->MassFlowRate / RhoWaterStdInit;
                node->ReportEnthalpy = CPCW(node->Temp) * node->Temp;
                node->WetBulbTemp = 0.0;
                node->RelHumidity = 0.0;
            }
        } else if (node->fluidType == FluidType::Electric) {
            node->VolFlowRateStdRho = 0.0;
            node->ReportEnthalpy = 0.0;
            node->WetBulbTemp = 0.0;
            node->RelHumidity = 0.0;
            node->SpecificHeat = 0.0;
        } else {
            node->VolFlowRateStdRho = node->MassFlowRate / RhoAirStdInit;
            if (node->HumRat > 0.0) {
                node->ReportEnthalpy = PsyHFnTdbW(node->Temp, node->HumRat);
                if (ReportWetBulb) {
                    node->WetBulbTemp = PsyTwbFnTdbWPb(state, node->Temp, node->HumRat, state.dataEnvrn->StdBaroPress);
                } else {
                    node->WetBulbTemp = 0.0;
                }
                if (ReportSpecificHeat) {
                    node->SpecificHeat = PsyCpAirFnW(node->HumRat);
                } else {
                    node->SpecificHeat = 0.0;
                }
            } else {
                node->ReportEnthalpy = CPCW(node->Temp) * node->Temp;
                node->WetBulbTemp = 0.0;
                node->SpecificHeat = 0.0;
            }
        }
    }
}

void MarkNode(EnergyPlusData &state,
              int const NodeNum, // Node Number to be marked
              ConnObjType const ObjectType,
              std::string const &ObjectName,
              std::string const &FieldName)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   March 2004
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine marks a node -- this node needs to exist in more than one object.

    auto &dln = state.dataLoopNodes;
    auto *node = dln->nodes(NodeNum);
        
    node->IsMarked = true;
    node->connObjType = ObjectType;
    node->ObjectName = ObjectName;
    node->FieldName = FieldName;
}

void CheckMarkedNodes(EnergyPlusData &state, bool &ErrorsFound)
{

    // SUBROUTINE INFORMATION:
    //       AUTHOR         Linda Lawrie
    //       DATE WRITTEN   March 2004
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    // PURPOSE OF THIS SUBROUTINE:
    // This subroutine checks "marked" nodes.
    auto &dln = state.dataLoopNodes;
        
    for (int NodeNum = 1; NodeNum <= dln->nodes.isize(); ++NodeNum) {
        auto const *node = dln->nodes(NodeNum);
            
        if (node->IsMarked) {
            if (state.dataNodeInputMgr->NodeRef(NodeNum) == 0) {
                ShowSevereError(state, format("Node=\"{}\" did not find reference by another object.", node->Name));
                ShowContinueError(state,
                                  format(R"(Object="{}", Name="{}", Field=[{}])",
                                         connObjTypeNames[(int)node->connObjType],
                                         node->ObjectName,
                                         node->FieldName));
                ErrorsFound = true;
            }
        }
    }
}

} // namespace EnergyPlus::Node
