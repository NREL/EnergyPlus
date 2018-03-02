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

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>

// EnergyPlus Headers
#include <DataPlant.hh>
#include <BranchInputManager.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <General.hh>
#include <Plant/Operation/PlantConvergencePoint.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace DataPlant {

	// MODULE INFORMATION:
	//       AUTHOR         Plant code authors?
	//       DATE WRITTEN
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This data-only module contains the structures for various parts of the Plant and
	// Condenser Loops.

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataGlobals::DoingSizing;
	using DataGlobals::OutputFileDebug;
	using DataLoopNode::SensedNodeFlagValue;
	using DataLoopNode::NodeID;

	int const LoadRangeBasedMin( 0 );
	int const LoadRangeBasedMax( 2 );

	// SimFlagCriteriaTypes for use in performing interconnect re-sim checks
	int const CriteriaType_MassFlowRate( 1 );
	int const CriteriaType_Temperature( 2 );
	int const CriteriaType_HeatTransferRate( 3 );

	// Criteria percentage limits for determining re-simulation of connected loop sides
	Real64 const CriteriaDelta_MassFlowRate( 0.001 );
	Real64 const CriteriaDelta_Temperature( 0.010 );
	Real64 const CriteriaDelta_HeatTransferRate( 0.100 );

	int const FreeCoolControlMode_WetBulb( 1 ); // HeatExchanger:Hydronic model control type mode, outdoor wetbulb sensor
	int const FreeCoolControlMode_DryBulb( 2 ); // HeatExchanger:Hydronic model control type mode, outdoor drybulb sensor
	int const FreeCoolControlMode_Loop( 3 ); // HeatExchanger:Hydronic model control type mode, loop setpoint sensor

	// Parameters for use in Loop Demand Calculation Schemes
	int const SingleSetPoint( 1 ); // Uses a single temp setpoint to calculate loop demand
	int const DualSetPointDeadBand( 2 ); // Uses a dual temp setpoint with a deadband between the high
	//  and the low to calculate loop demand
	// Parameters for loop setpoint reference
	int const Air( 1 );
	int const Ground( 2 );

	// Parameters for common pipe
	int const CommonPipe_No( 0 );
	int const CommonPipe_Single( 1 );
	int const CommonPipe_TwoWay( 2 );

	// Parameters for loop side location
	int const DemandSupply_No( 0 );
	int const DemandSide( 1 );
	int const SupplySide( 2 );

	Array1D_string const cLoopSideLocations( {0,2}, { "DemandSupply_No", "DemandSide", "SupplySide" } );
	// Parameters for economizer
	int const Integrated( 1 );
	int const NonIntegrated( 2 );
	int const None( 3 );

	// Parameters for tolerance
	Real64 const LoopDemandTol( 0.1 ); // minimum significant loop cooling or heating demand
	Real64 const DeltaTempTol( 0.0001 ); // minimum significant loop temperature difference

	// Parameters for Component/Equipment Types  (ref: TypeOf in CompData)
	int const LoopType_Plant( 1 );
	int const LoopType_Condenser( 2 );
	int const LoopType_Both( 3 );

	// Parameters for FlowLock standardization
	int const FlowPumpQuery( -1 ); // Used to ask the pumps for their min/max avail based on no constraints
	int const FlowUnlocked( 0 ); // components request flow
	int const FlowLocked( 1 ); // components take their inlet flow

	Array1D_string const cLoopTypes( {0,3}, { "None", "Plant", "Condenser", "Both Plant/Condenser" } );

	// Pressure Routine Call Enumeration
	int const PressureCall_Init( -1 );
	int const PressureCall_Calc( -2 );
	int const PressureCall_Update( -3 );

	// Pressure Simulation Types
	int const Press_NoPressure( 1 ); // Nothing for that particular loop
	int const Press_PumpPowerCorrection( 2 ); // Only updating the pump power
	int const Press_FlowCorrection( 3 ); // Update pump flow rate based on pump curve
	int const Press_FlowSimulation( 4 ); // Full pressure network simulation
	Array1D_string const PressureSimType( 4, { "NONE", "PUMPPOWERCORRECTION", "LOOPFLOWCORRECTION", "PRESSURESIMULATION" } );
	// Parameters for Component/Equipment Types  (ref: TypeOf in CompData)

	int const NumSimPlantEquipTypes( 94 );
	Array1D_string const SimPlantEquipTypes( NumSimPlantEquipTypes, { "BOILER:HOTWATER", "BOILER:STEAM", "CHILLER:ABSORPTION", "CHILLER:ABSORPTION:INDIRECT", "CHILLER:COMBUSTIONTURBINE", "CHILLER:CONSTANTCOP", "CHILLERHEATER:ABSORPTION:DIRECTFIRED", "CHILLER:ELECTRIC", "CHILLER:ELECTRIC:EIR", "CHILLER:ELECTRIC:REFORMULATEDEIR", "CHILLER:ENGINEDRIVEN", "COOLINGTOWER:SINGLESPEED", "COOLINGTOWER:TWOSPEED", "COOLINGTOWER:VARIABLESPEED", "GENERATOR:FUELCELL:EXHAUSTGASTOWATERHEATEXCHANGER", "WATERHEATER:HEATPUMP:PUMPEDCONDENSER", "HEATPUMP:WATERTOWATER:EQUATIONFIT:COOLING", "HEATPUMP:WATERTOWATER:EQUATIONFIT:HEATING", "HEATPUMP:WATERTOWATER:PARAMETERESTIMATION:COOLING", "HEATPUMP:WATERTOWATER:PARAMETERESTIMATION:HEATING", "PIPE:ADIABATIC", "PIPE:ADIABATIC:STEAM", "PIPE:OUTDOOR", "PIPE:INDOOR", "PIPE:UNDERGROUND", "DISTRICTCOOLING", "DISTRICTHEATING", "THERMALSTORAGE:ICE:DETAILED", "THERMALSTORAGE:ICE:SIMPLE", "TEMPERINGVALVE", "WATERHEATER:MIXED", "WATERHEATER:STRATIFIED", "PUMP:VARIABLESPEED", "PUMP:CONSTANTSPEED", "PUMP:VARIABLESPEED:CONDENSATE", "HEADEREDPUMPS:VARIABLESPEED", "HEADEREDPUMPS:CONSTANTSPEED", "WATERUSE:CONNECTIONS", "COIL:COOLING:WATER", "COIL:COOLING:WATER:DETAILEDGEOMETRY", "COIL:HEATING:WATER", "COIL:HEATING:STEAM", "SOLARCOLLECTOR:FLATPLATE:WATER", "LOADPROFILE:PLANT", "GROUNDHEATEXCHANGER:SYSTEM", "GROUNDHEATEXCHANGER:SURFACE", "GROUNDHEATEXCHANGER:POND", "GENERATOR:MICROTURBINE", "GENERATOR:INTERNALCOMBUSTIONENGINE", "GENERATOR:COMBUSTIONTURBINE", "GENERATOR:MICROCHP", "GENERATOR:FUELCELL:STACKCOOLER", "FLUIDCOOLER:SINGLESPEED", "FLUIDCOOLER:TWOSPEED", "EVAPORATIVEFLUIDCOOLER:SINGLESPEED", "EVAPORATIVEFLUIDCOOLER:TWOSPEED", "THERMALSTORAGE:CHILLEDWATER:MIXED", "THERMALSTORAGE:CHILLEDWATER:STRATIFIED", "SOLARCOLLECTOR:FLATPLATE:PHOTOVOLTAICTHERMAL", "ZONEHVAC:BASEBOARD:CONVECTIVE:WATER", "ZONEHVAC:BASEBOARD:RADIANTCONVECTIVE:STEAM", "ZONEHVAC:BASEBOARD:RADIANTCONVECTIVE:WATER", "ZONEHVAC:LOWTEMPERATURERADIANT:VARIABLEFLOW", "ZONEHVAC:LOWTEMPERATURERADIANT:CONSTANTFLOW", "AIRTERMINAL:SINGLEDUCT:CONSTANTVOLUME:COOLEDBEAM", "COIL:HEATING:WATERTOAIRHEATPUMP:EQUATIONFIT", "COIL:COOLING:WATERTOAIRHEATPUMP:EQUATIONFIT", "COIL:HEATING:WATERTOAIRHEATPUMP:PARAMETERESTIMATION", "COIL:COOLING:WATERTOAIRHEATPUMP:PARAMETERESTIMATION", "REFRIGERATION:CONDENSER:WATERCOOLED", "REFRIGERATION:COMPRESSORRACK", "AIRLOOPHVAC:UNITARYHEATPUMP:AIRTOAIR:MULTISPEED", "CHILLERHEATER:ABSORPTION:DOUBLEEFFECT", "PIPINGSYSTEM:UNDERGROUND:PIPECIRCUIT", "SOLARCOLLECTOR:INTEGRALCOLLECTORSTORAGE", "COIL:HEATING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT", "COIL:COOLING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT", "PLANTCOMPONENT:USERDEFINED", "COIL:USERDEFINED", "ZONEHVAC:FORCEDAIR:USERDEFINED", "AIRTERMINAL:SINGLEDUCT:USERDEFINED", "AIRCONDITIONER:VARIABLEREFRIGERANTFLOW", "GROUNDHEATEXCHANGER:HORIZONTALTRENCH", "HEATEXCHANGER:FLUIDTOFLUID", "PLANTCOMPONENT:TEMPERATURESOURCE", "CENTRALHEATPUMPSYSTEM", "AIRLOOPHVAC:UNITARYSYSTEM", "COIL:COOLING:DX:SINGLESPEED:THERMALSTORAGE", "COOLINGTOWER:VARIABLESPEED:MERKEL", "SWIMMINGPOOL:INDOOR", "GROUNDHEATEXCHANGER:SLINKY", "WATERHEATER:HEATPUMP:WRAPPEDCONDENSER", "AIRTERMINAL:SINGLEDUCT:CONSTANTVOLUME:FOURPIPEBEAM", "ZONEHVAC:COOLINGPANEL:RADIANTCONVECTIVE:WATER"} ); // 01 | 02 | 03 | 04 | 05 | 06 | 07 | 08 | 09 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25 | 26 | 27 | 28 | 29 | 30 | 31 | 32 | 33 | 34 | 35 | 36 | 37 | 38 ! demand side component | 39 ! demand side component | 40 ! demand side component | 41 ! demand side component | 42 ! demand side component | 43 ! demand side component | 44 ! demand side component' | 45 | 46 | 47 | 48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 | 58 | 59 | 60 | 61 | 62 | 63 | 64 | 65 | 66 | 67 | 68 | 69 | 70 | 71 | 72 | 73 | 74 | 75 | 76 | 77 | 78 | 79 | 80 | 81 | 82 | 83 | 84 | 85 | 86 | 87 | 88 | 89 | 90 | 91 | 92

	Array1D_string const ccSimPlantEquipTypes( NumSimPlantEquipTypes, { "Boiler:HotWater", "Boiler:Steam", "Chiller:Absorption", "Chiller:Absorption:Indirect", "Chiller:CombustionTurbine", "Chiller:ConstantCOP", "ChillerHeater:Absorption:DirectFired", "Chiller:Electric", "Chiller:Electric:EIR", "Chiller:Electric:ReformulatedEIR", "Chiller:EngineDriven", "CoolingTower:SingleSpeed", "CoolingTower:TwoSpeed", "CoolingTower:VariableSpeed", "Generator:Fuelcell:ExhaustGastoWaterHeatExchanger", "WaterHeater:HeatPump:PumpedCondenser", "Heatpump:WatertoWater:Equationfit:Cooling", "Heatpump:WatertoWater:Equationfit:Heating", "Heatpump:WatertoWater:ParameterEstimation:Cooling", "Heatpump:WatertoWater:ParameterEstimation:Heating", "Pipe:Adiabatic", "Pipe:Adiabatic:Steam", "Pipe:Outdoor", "Pipe:Indoor", "Pipe:Underground", "DistrictCooling", "DistrictHeating", "ThermalStorage:Ice:Detailed", "ThermalStorage:Ice:Simple", "TemperingValve", "WaterHeater:Mixed", "WaterHeater:Stratified", "Pump:VariableSpeed", "Pump:ConstantSpeed", "Pump:VariableSpeed:Condensate", "HeaderedPumps:VariableSpeed", "HeaderedPumps:ConstantSpeed", "WaterUse:Connections", "Coil:Cooling:Water", "Coil:Cooling:Water:DetailedGeometry", "Coil:Heating:Water", "Coil:Heating:Steam", "Solarcollector:Flatplate:Water", "LoadProfile:Plant", "GroundHeatExchanger:System", "GroundHeatExchanger:Surface", "GroundHeatExchanger:Pond", "Generator:Microturbine", "Generator:InternalCombustionEngine", "Generator:CombustionTurbine", "Generator:Microchp", "Generator:Fuelcell:StackCooler", "FluidCooler:SingleSpeed", "FluidCooler:TwoSpeed", "EvaporativeFluidCooler:SingleSpeed", "EvaporativeFluidCooler:TwoSpeed", "ThermalStorage:ChilledWater:Mixed", "ThermalStorage:ChilledWater:Stratified", "SolarCollector:FlatPlate:PhotovoltaicThermal", "ZoneHVAC:Baseboard:Convective:Water", "ZoneHVAC:Baseboard:RadiantConvective:Steam", "ZoneHVAC:Baseboard:RadiantConvective:Water", "ZoneHVAC:LowTemperatureRadiant:VariableFlow", "ZoneHVAC:LowTemperatureRadiant:ConstantFlow", "AirTerminal:SingleDuct:ConstantVolume:CooledBeam", "Coil:Heating:WaterToAirHeatPump:EquationFit", "Coil:Cooling:WaterToAirHeatPump:EquationFit", "Coil:Heating:WaterToAirHeatPump:ParameterEstimation", "Coil:Cooling:WaterToAirHeatPump:ParameterEstimation", "Refrigeration:Condenser:WaterCooled", "Refrigeration:CompressorRack", "AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed", "ChillerHeater:Absorption:DoubleEffect", "PipingSystem:Underground:PipeCircuit", "SolarCollector:IntegralCollectorStorage", "Coil:Heating:WaterToAirHeatPump:VariableSpeedEquationFit", "Coil:Cooling:WaterToAirHeatPump:VariableSpeedEquationFit", "PlantComponent:UserDefined", "Coil:UserDefined", "ZoneHVAC:ForcedAir:UserDefined", "AirTerminal:SingleDuct:UserDefined", "AirConditioner:VariableRefrigerantFlow", "GroundHeatExchanger:HorizontalTrench", "HeatExchanger:FluidToFluid", "PlantComponent:TemperatureSource", "CentralHeatPumpSystem", "AirloopHVAC:UnitarySystem", "Coil:Cooling:DX:SingleSpeed:ThermalStorage", "CoolingTower:VariableSpeed:Merkel", "SwimmingPool:Indoor", "GroundHeatExchanger:Slinky", "WaterHeater:HeatPump:WrappedCondenser", "AirTerminal:SingleDuct:ConstantVolume:FourPipeBeam", "ZoneHVAC:CoolingPanel:RadiantConvective:Water" } ); // 01 | 02 | 03 | 04 | 05 | 06 | 07 | 08 | 09 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25 | 26 | 27 | 28 | 29 | 30 | 31 | 32 | 33 | 34 | 35 | 36 | 37 | 38 ! demand side component | 39 Demand Side Component | 40 Demand Side Component | 41 Demand Side Component | 42 Demand Side Component | 43 Demand Side Component | 44 Demand Side Component' | 45 | 46 | 47 | 48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 | 58 | 59 | 60 | 61 | 62 | 63 | 64 | 65 | 66 | 67 | 68 | 69 | 70 | 71 | 72 | 73 | 74 | 75 | 76 | 77 | 78 | 79 | 80 | 81 | 82 | 83 | 84 | 85 | 86 | 87 | 88 | 89 | 90 | 91 | 92

	Array1D_int const ValidLoopEquipTypes( NumSimPlantEquipTypes, { LoopType_Plant, LoopType_Plant, LoopType_Plant, LoopType_Plant, LoopType_Plant, LoopType_Plant, LoopType_Plant, LoopType_Plant, LoopType_Plant, LoopType_Plant, LoopType_Plant, LoopType_Both, LoopType_Both, LoopType_Both, LoopType_Plant, LoopType_Plant, LoopType_Plant, LoopType_Plant, LoopType_Plant, LoopType_Plant, LoopType_Both, LoopType_Both, LoopType_Both, LoopType_Both, LoopType_Both, LoopType_Both, LoopType_Both, LoopType_Plant, LoopType_Plant, LoopType_Both, LoopType_Both, LoopType_Both, LoopType_Both, LoopType_Both, LoopType_Both, LoopType_Both, LoopType_Both, LoopType_Plant, LoopType_Plant, LoopType_Plant, LoopType_Plant, LoopType_Plant, LoopType_Plant, LoopType_Both, LoopType_Both, LoopType_Both, LoopType_Both, LoopType_Plant, LoopType_Plant, LoopType_Plant, LoopType_Plant, LoopType_Plant, LoopType_Both, LoopType_Both, LoopType_Both, LoopType_Both, LoopType_Both, LoopType_Both, LoopType_Both, LoopType_Plant, LoopType_Plant, LoopType_Plant, LoopType_Plant, LoopType_Plant, LoopType_Both, LoopType_Both, LoopType_Both, LoopType_Both, LoopType_Both, LoopType_Both, LoopType_Both, LoopType_Plant, LoopType_Plant, LoopType_Both, LoopType_Both, LoopType_Both, LoopType_Both, LoopType_Both, LoopType_Both, LoopType_Both, LoopType_Both, LoopType_Both, LoopType_Both, LoopType_Both, LoopType_Both, LoopType_Plant, LoopType_Plant, LoopType_Both, LoopType_Both, LoopType_Both, LoopType_Both, LoopType_Plant , LoopType_Plant, LoopType_Plant } ); // 01  BOILER:HOTWATER | 02  BOILER:STEAM | 03  CHILLER:ABSORPTION | 04  CHILLER:ABSORPTION:INDIRECT | 05  CHILLER:COMBUSTIONTURBINE | 06  CHILLER:CONSTANTCOP | 07  CHILLERHEATER:ABSORPTION:DIRECTFIRED | 08  CHILLER:ELECTRIC | 09  CHILLER:ELECTRIC:EIR | 10  CHILLER:ELECTRIC:REFORMULATEDEIR | 11  CHILLER:ENGINEDRIVEN | 12  COOLINGTOWER:SINGLESPEED | 13  COOLINGTOWER:TWOSPEED | 14  COOLINGTOWER:VARIABLESPEED | 15  GENERATOR:FUELCELL:EXHAUSTGASTOWATERHEATEXCHANGER | 16  WATERHEATER:HEATPUMP:PUMPEDCONDENSER | 17  HEATPUMP:WATERTOWATER:EQUATIONFIT:COOLING | 18  HEATPUMP:WATERTOWATER:EQUATIONFIT:HEATING | 19  HEATPUMP:WATERTOWATER:PARAMETERESTIMATION:COOLING | 20  HEATPUMP:WATERTOWATER:PARAMETERESTIMATION:HEATING | 21  PIPE:ADIABATIC | 22  PIPE:ADIABATIC:STEAM | 23  PIPE:OUTDOOR | 24  PIPE:INDOOR | 25  PIPE:UNDERGROUND | 26  DISTRICTCOOLING | 27  DISTRICTHEATING | 28  THERMALSTORAGE:ICE:DETAILED | 29  THERMALSTORAGE:ICE:SIMPLE | 30  TEMPERINGVALVE | 31  WATERHEATER:MIXED | 32  WATERHEATER:STRATIFIED | 33  PUMP:VARIABLESPEED | 34  PUMP:CONSTANTSPEED | 35  PUMP:VARIABLESPEED:CONDENSATE | 36  HEADEREDPUMPS:VARIABLESPEED | 37  HEADEREDPUMPS:CONSTANTSPEED | 38  WATERUSE:CONNECTIONS | 39  COIL:COOLING:WATER | 40  COIL:COOLING:WATER:DETAILEDGEOMETRY | 41  COIL:HEATING:WATER | 42  COIL:HEATING:STEAM | 43  SOLARCOLLECTOR:FLATPLATE:WATER | 44  LOADPROFILE:PLANT | 45  GROUNDHEATEXCHANGER:SYSTEM | 46  GROUNDHEATEXCHANGER:SURFACE | 47  GROUNDHEATEXCHANGER:POND | 48  GENERATOR:MICROTURBINE | 49  GENERATOR:INTERNALCOMBUSTIONENGINE | 50  GENERATOR:COMBUSTIONTURBINE | 51  GENERATOR:MICROCHP | 52  GENERATOR:FUELCELL:STACKCOOLER | 53  FLUIDCOOLER:SINGLESPEED | 54  FLUIDCOOLER:TWOSPEED | 55  EVAPORATIVEFLUIDCOOLER:SINGLESPEED | 56  EVAPORATIVEFLUIDCOOLER:TWOSPEED | 57  THERMALSTORAGE:CHILLEDWATER:MIXED | 58  THERMALSTORAGE:CHILLEDWATER:STRATIFIED | 59  SOLARCOLLECTOR:FLATPLATE:PHOTOVOLTAICTHERMAL | 60  ZONEHVAC:BASEBOARD:CONVECTIVE:WATER | 61  ZONEHVAC:BASEBOARD:RADIANTCONVECTIVE:STEAM | 62  ZONEHVAC:BASEBOARD:RADIANTCONVECTIVE:WATER | 63  ZONEHVAC:LOWTEMPERATURERADIANT:VARIABLEFLOW | 64  ZONEHVAC:LOWTEMPERATURERADIANT:CONSTANTFLOW | 65 AirTerminal:SingleDuct:ConstantVolume:CooledBeam | 66  Coil:Heating:WaterToAirHeatPump:EquationFit | 67  Coil:Cooling:WaterTOAIRHeatPump:EquationFit | 68  Coil:Heating:WaterTOAIRHeatPump:ParameterEstimation | 69  Coil:Cooling:WaterTOAIRHeatPump:ParameterEstimation | 70  Refrigeration:Condenser:WaterCooled | 71  Refrigeration:CompressorRack | 72  AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed | 73  CHILLERHEATER:ABSORPTION:DOUBLEEFFECT | 74  PipingSystem:Underground:PipeCircuit | 75  SolarCollector:IntegralCollectorStorage | 76  Coil:Heating:WaterToAirHeatPump:VariableSpeedEquationFit | 77  Coil:Cooling:WaterTOAIRHeatPump:VariableSpeedEquationFit | 78  PlantComponent:UserDefined | 79  Coil:UserDefined | 80  ZoneHVAC:ForcedAir:UserDefined | 81  AirTerminal:SingleDuct:UserDefined | 82  AirConditioner:VariableRefrigerantFlow | 83  GroundHeatExchanger:HorizontalTrench | 84  HeatExchanger:FluidToFluid | 85  PlantComponent:TemperatureSource | 86  PlantCentralGroundSourceWrapper | 87  AirloopHVAC:UnitarySystem | 88  Coil:Cooling:DX:SingleSpeed:ThermalStorage | 89  CoolingTower:VariableSpeed:Merkel | 90 SwimmingPool:Indoor | 91 GroundHeatExchanger:Slinky | 92 WaterHeater:HeatPump:WrappedCondenser

	int const TypeOf_Other( -1 );
	int const TypeOf_Boiler_Simple( 1 );
	int const TypeOf_Boiler_Steam( 2 );
	int const TypeOf_Chiller_Absorption( 3 ); // older BLAST absorption chiller
	int const TypeOf_Chiller_Indirect_Absorption( 4 ); // revised absorption chiller
	int const TypeOf_Chiller_CombTurbine( 5 );
	int const TypeOf_Chiller_ConstCOP( 6 );
	int const TypeOf_Chiller_DFAbsorption( 7 );
	int const TypeOf_Chiller_Electric( 8 ); // basic BLAST Chiller
	int const TypeOf_Chiller_ElectricEIR( 9 );
	int const TypeOf_Chiller_ElectricReformEIR( 10 );
	int const TypeOf_Chiller_EngineDriven( 11 );
	int const TypeOf_CoolingTower_SingleSpd( 12 );
	int const TypeOf_CoolingTower_TwoSpd( 13 );
	int const TypeOf_CoolingTower_VarSpd( 14 );
	int const TypeOf_Generator_FCExhaust( 15 );
	int const TypeOf_HeatPumpWtrHeaterPumped( 16 );
	int const TypeOf_HeatPumpWtrHeaterWrapped( 92 );
	int const TypeOf_HPWaterEFCooling( 17 );
	int const TypeOf_HPWaterEFHeating( 18 );
	int const TypeOf_HPWaterPECooling( 19 );
	int const TypeOf_HPWaterPEHeating( 20 );
	int const TypeOf_Pipe( 21 );
	int const TypeOf_PipeSteam( 22 );
	int const TypeOf_PipeExterior( 23 );
	int const TypeOf_PipeInterior( 24 );
	int const TypeOf_PipeUnderground( 25 );
	int const TypeOf_PurchChilledWater( 26 );
	int const TypeOf_PurchHotWater( 27 );
	int const TypeOf_TS_IceDetailed( 28 );
	int const TypeOf_TS_IceSimple( 29 );
	int const TypeOf_ValveTempering( 30 );
	int const TypeOf_WtrHeaterMixed( 31 );
	int const TypeOf_WtrHeaterStratified( 32 );
	int const TypeOf_PumpVariableSpeed( 33 );
	int const TypeOf_PumpConstantSpeed( 34 );
	int const TypeOf_PumpCondensate( 35 );
	int const TypeOf_PumpBankVariableSpeed( 36 );
	int const TypeOf_PumpBankConstantSpeed( 37 );
	int const TypeOf_WaterUseConnection( 38 );
	int const TypeOf_CoilWaterCooling( 39 ); // demand side component
	int const TypeOf_CoilWaterDetailedFlatCooling( 40 ); // demand side component
	int const TypeOf_CoilWaterSimpleHeating( 41 ); // demand side component
	int const TypeOf_CoilSteamAirHeating( 42 ); // demand side component
	int const TypeOf_SolarCollectorFlatPlate( 43 ); // demand side component
	int const TypeOf_PlantLoadProfile( 44 ); // demand side component
	int const TypeOf_GrndHtExchgSystem( 45 );
	int const TypeOf_GrndHtExchgSurface( 46 );
	int const TypeOf_GrndHtExchgPond( 47 );
	int const TypeOf_Generator_MicroTurbine( 48 ); // newer FSEC turbine
	int const TypeOf_Generator_ICEngine( 49 );
	int const TypeOf_Generator_CTurbine( 50 ); // older BLAST turbine
	int const TypeOf_Generator_MicroCHP( 51 );
	int const TypeOf_Generator_FCStackCooler( 52 );
	int const TypeOf_FluidCooler_SingleSpd( 53 );
	int const TypeOf_FluidCooler_TwoSpd( 54 );
	int const TypeOf_EvapFluidCooler_SingleSpd( 55 );
	int const TypeOf_EvapFluidCooler_TwoSpd( 56 );
	int const TypeOf_ChilledWaterTankMixed( 57 );
	int const TypeOf_ChilledWaterTankStratified( 58 );
	int const TypeOf_PVTSolarCollectorFlatPlate( 59 );
	int const TypeOf_Baseboard_Conv_Water( 60 );
	int const TypeOf_Baseboard_Rad_Conv_Steam( 61 );
	int const TypeOf_Baseboard_Rad_Conv_Water( 62 );
	int const TypeOf_LowTempRadiant_VarFlow( 63 );
	int const TypeOf_LowTempRadiant_ConstFlow( 64 );
	int const TypeOf_CooledBeamAirTerminal( 65 );
	int const TypeOf_CoilWAHPHeatingEquationFit( 66 );
	int const TypeOf_CoilWAHPCoolingEquationFit( 67 );
	int const TypeOf_CoilWAHPHeatingParamEst( 68 );
	int const TypeOf_CoilWAHPCoolingParamEst( 69 );
	int const TypeOf_RefrigSystemWaterCondenser( 70 );
	int const TypeOf_RefrigerationWaterCoolRack( 71 );
	int const TypeOf_MultiSpeedHeatPumpRecovery( 72 );
	int const TypeOf_Chiller_ExhFiredAbsorption( 73 );
	int const TypeOf_PipingSystemPipeCircuit( 74 );
	int const TypeOf_SolarCollectorICS( 75 );
	int const TypeOf_CoilVSWAHPHeatingEquationFit( 76 );
	int const TypeOf_CoilVSWAHPCoolingEquationFit( 77 );
	int const TypeOf_PlantComponentUserDefined( 78 );
	int const TypeOf_CoilUserDefined( 79 );
	int const TypeOf_ZoneHVACAirUserDefined( 80 );
	int const TypeOf_AirTerminalUserDefined( 81 );
	int const TypeOf_HeatPumpVRF( 82 );
	int const TypeOf_GrndHtExchgHorizTrench( 83 );
	int const TypeOf_FluidToFluidPlantHtExchg( 84 );
	int const TypeOf_WaterSource( 85 );
	int const TypeOf_CentralGroundSourceHeatPump( 86 );
	int const TypeOf_UnitarySystemRecovery( 87 );
	int const TypeOf_PackagedTESCoolingCoil( 88 );
	int const TypeOf_CoolingTower_VarSpdMerkel( 89 );
	int const TypeOf_SwimmingPool_Indoor( 90 );
	int const TypeOf_GrndHtExchgSlinky( 91 );
	// int const TypeOf_HPWrappedCondenser ?? 92 ??
	int const TypeOf_FourPipeBeamAirTerminal( 93 );
	int const TypeOf_CoolingPanel_Simple( 94 );

	// Parameters for General Equipment Types
	int const NumGeneralEquipTypes( 23 );
	Array1D_string const GeneralEquipTypes( NumGeneralEquipTypes, { "BOILER", "CHILLER", "COOLINGTOWER", "GENERATOR", "HEATEXCHANGER", "HEATPUMP", "PIPE", "PUMP", "DISTRICT", "THERMALSTORAGE", "TEMPERINGVALVE", "WATERHEATER", "WATERUSE", "DEMANDCOIL", "SOLARCOLLECTOR", "LOADPROFILE", "FLUIDCOOLER", "EVAPORATIVEFLUIDCOOLER", "GROUNDHEATEXCHANGER", "ZONEHVACDEMAND", "REFRIGERATION", "PLANTCOMPONENT", "CENTRALHEATPUMPSYSTEM" } );

	int const GenEquipTypes_Boiler( 1 );
	int const GenEquipTypes_Chiller( 2 );
	int const GenEquipTypes_CoolingTower( 3 );
	int const GenEquipTypes_Generator( 4 );
	int const GenEquipTypes_HeatExchanger( 5 );
	int const GenEquipTypes_HeatPump( 6 );
	int const GenEquipTypes_Pipe( 7 );
	int const GenEquipTypes_Pump( 8 );
	int const GenEquipTypes_Purchased( 9 );
	int const GenEquipTypes_ThermalStorage( 10 );
	int const GenEquipTypes_Valve( 11 );
	int const GenEquipTypes_WaterThermalTank( 12 );
	int const GenEquipTypes_WaterUse( 13 );
	int const GenEquipTypes_DemandCoil( 14 );
	int const GenEquipTypes_SolarCollector( 15 );
	int const GenEquipTypes_LoadProfile( 16 );
	int const GenEquipTypes_FluidCooler( 17 );
	int const GenEquipTypes_EvapFluidCooler( 18 );
	int const GenEquipTypes_GroundHeatExchanger( 19 );
	int const GenEquipTypes_ZoneHVACDemand( 20 );
	int const GenEquipTypes_Refrigeration( 21 );
	int const GenEquipTypes_PlantComponent( 22 );
	int const GenEquipTypes_CentralHeatPumpSystem( 23 );

	Array1D< Real64 > const ConvergenceHistoryARR( DataPlant::NumConvergenceHistoryTerms, { 0.0, -1.0, -2.0, -3.0, -4.0 } );
	Real64 const sum_ConvergenceHistoryARR( sum( ConvergenceHistoryARR ) );
	Real64 const square_sum_ConvergenceHistoryARR( pow_2( sum_ConvergenceHistoryARR ) );
	Real64 const sum_square_ConvergenceHistoryARR( sum( pow( ConvergenceHistoryARR, 2 ) ) );

	// The same as TYPE DefinePriAirSysAvailMgrs in DataAirLoop.cc.  A common definition would be nicer.

	// The next three types (all starting with RepReport) are the "shadow"
	// derived types for the ventilation reports.  It keeps the node and
	// other connection information and adds variables for the ventilation
	// reports.  This is the cleanest way to do this and not impact other
	// data structures.  The actual derived types are defined (as allocatable)
	// below with the rest of the declarations.

	//MODULE VARIABLE DECLARATIONS:

	int NumPipes( 0 ); // Total number of pipes
	int NumPlantPipes( 0 ); // Total number of plant pipes
	int NumCondPipes( 0 ); // Total number of condenser pipes
	int TotNumLoops( 0 ); // number of plant and condenser loops
	int TotNumHalfLoops( 0 ); // number of half loops (2 * TotNumLoops)
	bool PlantFirstSizeCompleted( false );
	bool PlantFirstSizesOkayToFinalize( false ); // true if plant sizing is finishing and can save results
	bool PlantReSizingCompleted( false );
	bool PlantFirstSizesOkayToReport( false );
	bool PlantFinalSizesOkayToReport( false );
	bool AnyEMSPlantOpSchemesInModel( false );

	Array1D_int EconBranchNum; // Branch num on which economizer is placed
	Array1D_int EconCompNum; // Component num of economizer in the economizer branch

	Array1D_bool LoadChangeDownStream; // sim control flag.

	int PlantManageSubIterations( 0 ); // tracks plant iterations to characterize solver
	int PlantManageHalfLoopCalls( 0 ); // tracks number of half loop calls

	// these variables are arrays, allocated for the number of those particular loopsides, containing data for the vent reports
	// they are operated on like normal in almost all cases currently, except in the routine which actually mines data and sets them up
	// in that routine in SystemReports.cc, a POINTER is used to iterate over the different array variables below
	// this is why the TARGET attribute is applied to them here
	// further info can be found in SystemReports

	// Routines within this module

	// Object Data
	Array1D< PlantLoopData > PlantLoop;
	Array1D< PlantAvailMgrData > PlantAvailMgr;
	Array1D< ReportVars > PlantReport;
	Array1D< ReportLoopData > VentRepPlantSupplySide;
	Array1D< ReportLoopData > VentRepPlantDemandSide;
	Array1D< ReportLoopData > VentRepCondSupplySide;
	Array1D< ReportLoopData > VentRepCondDemandSide;
	Array1D< PlantCallingOrderInfoStruct > PlantCallingOrderInfo;

	// Clears the global data in DataPlant.
	// Needed for unit tests, should not be normally called.
	void
	clear_state()
	{
		NumPipes = 0;
		NumPlantPipes = 0;
		NumCondPipes = 0;
		TotNumLoops = 0;
		TotNumHalfLoops = 0;
		PlantFirstSizeCompleted = false;
		PlantFirstSizesOkayToFinalize = false;
		PlantReSizingCompleted = false;
		PlantFirstSizesOkayToReport = false;
		PlantFinalSizesOkayToReport = false;
		AnyEMSPlantOpSchemesInModel = false;
		EconBranchNum.deallocate();
		EconCompNum.deallocate();
		LoadChangeDownStream.deallocate();
		PlantManageSubIterations = 0;
		PlantManageHalfLoopCalls = 0;
		PlantLoop.deallocate();
		PlantAvailMgr.deallocate();
		PlantReport.deallocate();
		VentRepPlantSupplySide.deallocate();
		VentRepPlantDemandSide.deallocate();
		VentRepCondSupplySide.deallocate();
		VentRepCondDemandSide.deallocate();
		PlantCallingOrderInfo.deallocate();

	}

} // DataPlant

} // EnergyPlus
