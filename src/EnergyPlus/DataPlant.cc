// ObjexxFCL Headers
#include <ObjexxFCL/FArray.functions.hh>

// EnergyPlus Headers
#include <DataPlant.hh>
#include <BranchInputManager.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <General.hh>
#include <InputProcessor.hh>
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

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES: none

	// OTHER NOTES: none

	// USE STATEMENTS:
	// Use statements for data only modules (only modules that should be used here and sparingly)
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataGlobals::DoingSizing;
	using DataGlobals::OutputFileDebug;
	using DataLoopNode::SensedNodeFlagValue;
	using DataLoopNode::NodeID;

	// Data
	//MODULE PARAMETER DEFINITIONS:
	// Parameters for use in Load Distribution Schemes


	// Parameters for scheme types
	// Used in TYPE(OperationData)%OpSchemeType
	// As in PlantLoop(:)%OpScheme(:)%OpSchemeType
	// Also in PlantLoop()LoopSide()Branch()Comp()%CurOpSchemeType
	int const UnknownStatusOpSchemeType( -2 );
	int const NoControlOpSchemeType( -1 ); // Scheme Type placeholder for items such as pipes
	int const LoadRBOpSchemeType( 0 ); // Scheme Type for Load Range Based Operation (Deprecated)
	int const HeatingRBOpSchemeType( 1 ); // Scheme Type for Heating Load Range Based Operation
	int const CoolingRBOpSchemeType( 2 ); // Scheme Type for Cooling  Load Range Based Operation
	int const WetBulbRBOpSchemeType( 3 ); // Scheme Type for Wet bulb range based Operation
	int const DryBulbRBOpSchemeType( 4 ); // Scheme Type for Dry bulb range based Operation
	int const DewPointRBOpSchemeType( 5 ); // Scheme Type for Dewpoint range based Operation
	int const RelHumRBOpSchemeType( 6 ); // Scheme Type for relative humidity range based Operation
	int const DryBulbTDBOpSchemeType( 7 ); // Scheme Type for relative humidity range based Operation
	int const WetBulbTDBOpSchemeType( 8 ); // Scheme Type for Wet bulb range based Operation
	int const DewPointTDBOpSchemeType( 9 ); // Scheme Type for Wet bulb range based Operation
	int const CompSetPtBasedSchemeType( 10 ); // *Sankar Temp Based Control
	int const UncontrolledOpSchemeType( 11 ); // Scheme Type for Uncontrolled Operation
	int const EMSOpSchemeType( 12 ); // Scheme Type for EMS based operation user Define scheme
	int const PumpOpSchemeType( 13 ); // Not really an OpScheme, just a placeholder
	int const DemandOpSchemeType( 14 ); // Plcaeholder for demand side equipment such as coils
	int const FreeRejectionOpSchemeType( 15 ); // Scheme Type for waterside economizers and the like
	int const WSEconOpSchemeType( 16 ); // Scheme Type for waterside economizers and the like
	// this may be changed later...

	// These are useful for SELECT CASE statements rather than listing all of the individual types listed above
	int const LoadRangeBasedMin( 0 );
	int const LoadRangeBasedMax( 2 );
	int const TempRangeBasedMin( 3 );
	int const TempRangeBasedMax( 6 );
	int const DeltaTempRangeBasedMin( 7 );
	int const DeltaTempRangeBasedMax( 9 );

	// SimFlagCriteriaTypes for use in performing interconnect re-sim checks
	int const CriteriaType_MassFlowRate( 1 );
	int const CriteriaType_Temperature( 2 );
	int const CriteriaType_HeatTransferRate( 3 );

	// Criteria percentage limits for determining re-simulation of connected loop sides
	Real64 const CriteriaDelta_MassFlowRate( 0.001 );
	Real64 const CriteriaDelta_Temperature( 0.010 );
	Real64 const CriteriaDelta_HeatTransferRate( 0.100 );

	// Parameters for loop flow request priority,
	//     used in logic to deal with Node%MassFlowRequest for determining overall loop flow rate
	int const LoopFlowStatus_Unknown( 21 ); // component's status is not yet set
	int const LoopFlowStatus_NeedyAndTurnsLoopOn( 22 ); // component is a "winner" for loop flow requests
	// active valve inside component that modulates flow
	//  gets the loop going under most conditions
	int const LoopFlowStatus_NeedyIfLoopOn( 23 ); // component is a "winner" for loop flow requests
	// but doesn't normally get the loop going to start with
	//  once loop is going, may increase needs, non-zero minimums
	int const LoopFlowStatus_TakesWhatGets( 24 ); // component is a "loser" for loop flow requests,
	// but if the loop is on it
	// it does make flow requests (for s/m resolution)

	//Parameters for component character wrt how load gets met (or not)
	//  used in %HowLoadServed to facilitate load dispatch logic
	int const HowMet_Unknown( 50 ); // not yet set
	int const HowMet_NoneDemand( 51 ); // does not meet a load, demand component
	int const HowMet_PassiveCap( 52 ); // Passive machine, does what conditions allow but
	int const HowMet_ByNominalCap( 53 ); // MaxLoad, MinLoad, OptLoad should work
	int const HowMet_ByNominalCapLowOutLimit( 54 ); // MaxLoad, MinLoad, OptLoad but with low limit temp on outlet
	int const HowMet_ByNominalCapHiOutLimit( 55 ); // MaxLoad, MinLoad, OptLoad but with high limit temp on outlet
	int const HowMet_ByNominalCapFreeCoolCntrl( 56 ); // HowMet_ByNominalCap with free cool shutdown
	int const HowMet_ByNominalCapLowOutLimitFreeCoolCntrl( 57 ); // HowMet_ByNominalCapLowOutLimit with free cool shutdown

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
	int const LoopNode( 3 );

	// Parameters for common pipe
	int const CommonPipe_No( 0 );
	int const CommonPipe_Single( 1 );
	int const CommonPipe_TwoWay( 2 );

	// Parameters for loop side location
	int const DemandSupply_No( 0 );
	int const DemandSide( 1 );
	int const SupplySide( 2 );
	int const DemandSupply_Yes( 3 ); // DSU

	FArray1D_string const cLoopSideLocations( {0,3}, { "DemandSupply_No", "DemandSide", "SupplySide", "DemandSupply_Yes" } );
	// Parameters for economizer
	int const Integrated( 1 );
	int const NonIntegrated( 2 );
	int const None( 3 );

	// Parameters for tolerance
	Real64 const LoopDemandTol( 0.1 ); // minimum significant loop cooling or heating demand
	Real64 const DeltaTempTol( 0.0001 ); // minimum significant loop temperature difference

	// Parameters for Component/Equipment Types  (ref: TypeOf in CompData)
	int const LoopType_NoLoop( 0 );
	int const LoopType_Plant( 1 );
	int const LoopType_Condenser( 2 );
	int const LoopType_Both( 3 );

	// Parameters for calls to simflag routines
	int const PlantSupply( -1 );
	int const PlantDemand( -2 );
	int const CondSupply( -3 );
	int const CondDemand( -4 );

	// Parameters for FlowLock standardization
	int const FlowPumpQuery( -1 ); // Used to ask the pumps for their min/max avail based on no constraints
	int const FlowUnlocked( 0 ); // components request flow
	int const FlowLocked( 1 ); // components take their inlet flow

	FArray1D_string const cLoopTypes( {0,3}, { "None", "Plant", "Condenser", "Both Plant/Condenser" } );

	// Pressure Routine Call Enumeration
	int const PressureCall_Init( -1 );
	int const PressureCall_Calc( -2 );
	int const PressureCall_Update( -3 );

	// Pressure Simulation Types
	int const Press_NoPressure( 1 ); // Nothing for that particular loop
	int const Press_PumpPowerCorrection( 2 ); // Only updating the pump power
	int const Press_FlowCorrection( 3 ); // Update pump flow rate based on pump curve
	int const Press_FlowSimulation( 4 ); // Full pressure network simulation
	FArray1D_string const PressureSimType(
		4, {
			"NONE",
			"PUMPPOWERCORRECTION",
			"LOOPFLOWCORRECTION",
			"PRESSURESIMULATION"
		}
	);
	
	// Parameters for Component/Equipment Types  (ref: TypeOf in CompData)
	int const NumSimPlantEquipTypes( 91 );
	FArray1D_string const SimPlantEquipTypes(
		NumSimPlantEquipTypes, {
			"BOILER:HOTWATER", // 01 
			"BOILER:STEAM", // 02 
			"CHILLER:ABSORPTION", // 03 
			"CHILLER:ABSORPTION:INDIRECT", // 04 
			"CHILLER:COMBUSTIONTURBINE", // 05 
			"CHILLER:CONSTANTCOP", // 06 
			"CHILLERHEATER:ABSORPTION:DIRECTFIRED", // 07 
			"CHILLER:ELECTRIC", // 08 
			"CHILLER:ELECTRIC:EIR", // 09 
			"CHILLER:ELECTRIC:REFORMULATEDEIR", // 10 
			"CHILLER:ENGINEDRIVEN", // 11 
			"COOLINGTOWER:SINGLESPEED", // 12 
			"COOLINGTOWER:TWOSPEED", // 13 
			"COOLINGTOWER:VARIABLESPEED", // 14 
			"GENERATOR:FUELCELL:EXHAUSTGASTOWATERHEATEXCHANGER", // 15 
			"WATERHEATER:HEATPUMP", // 16 
			"HEATPUMP:WATERTOWATER:EQUATIONFIT:COOLING", // 17 
			"HEATPUMP:WATERTOWATER:EQUATIONFIT:HEATING", // 18 
			"HEATPUMP:WATERTOWATER:PARAMETERESTIMATION:COOLING", // 19 
			"HEATPUMP:WATERTOWATER:PARAMETERESTIMATION:HEATING", // 20 
			"PIPE:ADIABATIC", // 21 
			"PIPE:ADIABATIC:STEAM", // 22 
			"PIPE:OUTDOOR", // 23 
			"PIPE:INDOOR", // 24 
			"PIPE:UNDERGROUND", // 25 
			"DISTRICTCOOLING", // 26 
			"DISTRICTHEATING", // 27 
			"THERMALSTORAGE:ICE:DETAILED", // 28 
			"THERMALSTORAGE:ICE:SIMPLE", // 29 
			"TEMPERINGVALVE", // 30 
			"WATERHEATER:MIXED", // 31 
			"WATERHEATER:STRATIFIED", // 32 
			"PUMP:VARIABLESPEED", // 33 
			"PUMP:CONSTANTSPEED", // 34 
			"PUMP:VARIABLESPEED:CONDENSATE", // 35 
			"HEADEREDPUMPS:VARIABLESPEED", // 36 
			"HEADEREDPUMPS:CONSTANTSPEED", // 37 
			"WATERUSE:CONNECTIONS", // 38 ! demand side component 
			"COIL:COOLING:WATER", // 39 ! demand side component 
			"COIL:COOLING:WATER:DETAILEDGEOMETRY", // 40 ! demand side component 
			"COIL:HEATING:WATER", // 41 ! demand side component 
			"COIL:HEATING:STEAM", // 42 ! demand side component 
			"SOLARCOLLECTOR:FLATPLATE:WATER", // 43 ! demand side component 
			"LOADPROFILE:PLANT", // 44 ! demand side component' 
			"GROUNDHEATEXCHANGER:VERTICAL", // 45 
			"GROUNDHEATEXCHANGER:SURFACE", // 46 
			"GROUNDHEATEXCHANGER:POND", // 47 
			"GENERATOR:MICROTURBINE", // 48 
			"GENERATOR:INTERNALCOMBUSTIONENGINE", // 49 
			"GENERATOR:COMBUSTIONTURBINE", // 50 
			"GENERATOR:MICROCHP", // 51 
			"GENERATOR:FUELCELL:STACKCOOLER", // 52 
			"FLUIDCOOLER:SINGLESPEED", // 53 
			"FLUIDCOOLER:TWOSPEED", // 54 
			"EVAPORATIVEFLUIDCOOLER:SINGLESPEED", // 55 
			"EVAPORATIVEFLUIDCOOLER:TWOSPEED", // 56 
			"THERMALSTORAGE:CHILLEDWATER:MIXED", // 57 
			"THERMALSTORAGE:CHILLEDWATER:STRATIFIED", // 58 
			"SOLARCOLLECTOR:FLATPLATE:PHOTOVOLTAICTHERMAL", // 59 
			"ZONEHVAC:BASEBOARD:CONVECTIVE:WATER", // 60 
			"ZONEHVAC:BASEBOARD:RADIANTCONVECTIVE:STEAM", // 61 
			"ZONEHVAC:BASEBOARD:RADIANTCONVECTIVE:WATER", // 62 
			"ZONEHVAC:LOWTEMPERATURERADIANT:VARIABLEFLOW", // 63 
			"ZONEHVAC:LOWTEMPERATURERADIANT:CONSTANTFLOW", // 64 
			"AIRTERMINAL:SINGLEDUCT:CONSTANTVOLUME:COOLEDBEAM", // 65 
			"COIL:HEATING:WATERTOAIRHEATPUMP:EQUATIONFIT", // 66 
			"COIL:COOLING:WATERTOAIRHEATPUMP:EQUATIONFIT", // 67 
			"COIL:HEATING:WATERTOAIRHEATPUMP:PARAMETERESTIMATION", // 68 
			"COIL:COOLING:WATERTOAIRHEATPUMP:PARAMETERESTIMATION", // 69 
			"REFRIGERATION:CONDENSER:WATERCOOLED", // 70 
			"REFRIGERATION:COMPRESSORRACK", // 71 
			"AIRLOOPHVAC:UNITARYHEATPUMP:AIRTOAIR:MULTISPEED", // 72 
			"CHILLERHEATER:ABSORPTION:DOUBLEEFFECT", // 73 
			"PIPINGSYSTEM:UNDERGROUND:PIPECIRCUIT", // 74 
			"SOLARCOLLECTOR:INTEGRALCOLLECTORSTORAGE", // 75 
			"COIL:HEATING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT", // 76 
			"COIL:COOLING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT", // 77 
			"PLANTCOMPONENT:USERDEFINED", // 78 
			"COIL:USERDEFINED", // 79 
			"ZONEHVAC:FORCEDAIR:USERDEFINED", // 80 
			"AIRTERMINAL:SINGLEDUCT:USERDEFINED", // 81 
			"AIRCONDITIONER:VARIABLEREFRIGERANTFLOW", // 82 
			"GROUNDHEATEXCHANGER:HORIZONTALTRENCH", // 83 
			"HEATEXCHANGER:FLUIDTOFLUID", // 84 
			"PLANTCOMPONENT:TEMPERATURESOURCE", // 85 
			"CENTRALHEATPUMPSYSTEM", // 86 
			"AIRLOOPHVAC:UNITARYSYSTEM", // 87 
			"COIL:COOLING:DX:SINGLESPEED:THERMALSTORAGE", // 88 
			"COOLINGTOWER:VARIABLESPEED:MERKEL", // 89 
			"SWIMMINGPOOL:INDOOR", // 90
			"ZONEHVAC:COOLINGPANEL:RADIANTCONVECTIVE:WATER" // 91
		}
	); 

	FArray1D_string const ccSimPlantEquipTypes( 
		NumSimPlantEquipTypes, {
			"Boiler:HotWater", // 01 
			"Boiler:Steam", // 02 
			"Chiller:Absorption", // 03 
			"Chiller:Absorption:Indirect", // 04 
			"Chiller:CombustionTurbine", // 05 
			"Chiller:ConstantCOP", // 06 
			"ChillerHeater:Absorption:DirectFired", // 07 
			"Chiller:Electric", // 08 
			"Chiller:Electric:EIR", // 09 
			"Chiller:Electric:ReformulatedEIR", // 10 
			"Chiller:EngineDriven", // 11 
			"CoolingTower:SingleSpeed", // 12 
			"CoolingTower:TwoSpeed", // 13 
			"CoolingTower:VariableSpeed", // 14 
			"Generator:Fuelcell:ExhaustGastoWaterHeatExchanger", // 15 
			"WaterHeater:Heatpump", // 16 
			"Heatpump:WatertoWater:Equationfit:Cooling", // 17 
			"Heatpump:WatertoWater:Equationfit:Heating", // 18 
			"Heatpump:WatertoWater:ParameterEstimation:Cooling", // 19 
			"Heatpump:WatertoWater:ParameterEstimation:Heating", // 20 
			"Pipe:Adiabatic", // 21 
			"Pipe:Adiabatic:Steam", // 22 
			"Pipe:Outdoor", // 23 
			"Pipe:Indoor", // 24 
			"Pipe:Underground", // 25 
			"DistrictCooling", // 26 
			"DistrictHeating", // 27 
			"ThermalStorage:Ice:Detailed", // 28 
			"ThermalStorage:Ice:Simple", // 29 
			"TemperingValve", // 30 
			"WaterHeater:Mixed", // 31 
			"WaterHeater:Stratified", // 32 
			"Pump:VariableSpeed", // 33 
			"Pump:ConstantSpeed", // 34 
			"Pump:VariableSpeed:Condensate", // 35 
			"HeaderedPumps:VariableSpeed", // 36 
			"HeaderedPumps:ConstantSpeed", // 37 
			"WaterUse:Connections", // 38 ! demand side component 
			"Coil:Cooling:Water", // 39 ! demand side component 
			"Coil:Cooling:Water:DetailedGeometry", // 40 ! demand side component 
			"Coil:Heating:Water", // 41 ! demand side component 
			"Coil:Heating:Steam", // 42 ! demand side component 
			"Solarcollector:Flatplate:Water", // 43 ! demand side component 
			"LoadProfile:Plant", // 44 ! demand side component' 
			"GroundHeatExchanger:Vertical", // 45 
			"GroundHeatExchanger:Surface", // 46 
			"GroundHeatExchanger:Pond", // 47 
			"Generator:Microturbine", // 48 
			"Generator:InternalCombustionEngine", // 49 
			"Generator:CombustionTurbine", // 50 
			"Generator:Microchp", // 51 
			"Generator:Fuelcell:StackCooler", // 52 
			"FluidCooler:SingleSpeed", // 53 
			"FluidCooler:TwoSpeed", // 54 
			"EvaporativeFluidCooler:SingleSpeed", // 55 
			"EvaporativeFluidCooler:TwoSpeed", // 56 
			"ThermalStorage:ChilledWater:Mixed", // 57 
			"ThermalStorage:ChilledWater:Stratified", // 58 
			"SolarCollector:FlatPlate:PhotovoltaicThermal", // 59 
			"ZoneHVAC:Baseboard:Convective:Water", // 60 
			"ZoneHVAC:Baseboard:RadiantConvective:Steam", // 61 
			"ZoneHVAC:Baseboard:RadiantConvective:Water", // 62 
			"ZoneHVAC:LowTemperatureRadiant:VariableFlow", // 63 
			"ZoneHVAC:LowTemperatureRadiant:ConstantFlow", // 64 
			"AirTerminal:SingleDuct:ConstantVolume:CooledBeam", // 65 
			"Coil:Heating:WaterToAirHeatPump:EquationFit", // 66 
			"Coil:Cooling:WaterToAirHeatPump:EquationFit", // 67 
			"Coil:Heating:WaterToAirHeatPump:ParameterEstimation", // 68 
			"Coil:Cooling:WaterToAirHeatPump:ParameterEstimation", // 69 
			"Refrigeration:Condenser:WaterCooled", // 70 
			"Refrigeration:CompressorRack", // 71 
			"AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed", // 72 
			"ChillerHeater:Absorption:DoubleEffect", // 73 
			"PipingSystem:Underground:PipeCircuit", // 74 
			"SolarCollector:IntegralCollectorStorage", // 75 
			"Coil:Heating:WaterToAirHeatPump:VariableSpeedEquationFit", // 76 
			"Coil:Cooling:WaterToAirHeatPump:VariableSpeedEquationFit", // 77 
			"PlantComponent:UserDefined", // 78 
			"Coil:UserDefined", // 79 
			"ZoneHVAC:ForcedAir:UserDefined", // 80 
			"AirTerminal:SingleDuct:UserDefined", // 81 
			"AirConditioner:VariableRefrigerantFlow", // 82 
			"GroundHeatExchanger:HorizontalTrench", // 83 
			"HeatExchanger:FluidToFluid", // 84 
			"PlantComponent:TemperatureSource", // 85 
			"CentralHeatPumpSystem", // 86 
			"AirloopHVAC:UnitarySystem", // 87 
			"Coil:Cooling:DX:SingleSpeed:ThermalStorage", // 88 
			"CoolingTower:VariableSpeed:Merkel", // 89 
			"SwimmingPool:Indoor", // 90
			"ZoneHVAC:CoolingPanel:RadiantConvective:Water" // 91
		}
	);


	FArray1D_int const ValidLoopEquipTypes(
		NumSimPlantEquipTypes, {
			LoopType_Plant, // 01  BOILER:HOTWATER 
			LoopType_Plant, // 02  BOILER:STEAM 
			LoopType_Plant, // 03  CHILLER:ABSORPTION 
			LoopType_Plant, // 04  CHILLER:ABSORPTION:INDIRECT 
			LoopType_Plant, // 05  CHILLER:COMBUSTIONTURBINE 
			LoopType_Plant, // 06  CHILLER:CONSTANTCOP 
			LoopType_Plant, // 07  CHILLERHEATER:ABSORPTION:DIRECTFIRED 
			LoopType_Plant, // 08  CHILLER:ELECTRIC 
			LoopType_Plant, // 09  CHILLER:ELECTRIC:EIR 
			LoopType_Plant, // 10  CHILLER:ELECTRIC:REFORMULATEDEIR 
			LoopType_Plant, // 11  CHILLER:ENGINEDRIVEN 
			LoopType_Both, // 12  COOLINGTOWER:SINGLESPEED 
			LoopType_Both, // 13  COOLINGTOWER:TWOSPEED 
			LoopType_Both, // 14  COOLINGTOWER:VARIABLESPEED 
			LoopType_Plant, // 15  GENERATOR:FUELCELL:EXHAUSTGASTOWATERHEATEXCHANGER 
			LoopType_Plant, // 16  WATERHEATER:HEATPUMP 
			LoopType_Plant, // 17  HEATPUMP:WATERTOWATER:EQUATIONFIT:COOLING 
			LoopType_Plant, // 18  HEATPUMP:WATERTOWATER:EQUATIONFIT:HEATING 
			LoopType_Plant, // 19  HEATPUMP:WATERTOWATER:PARAMETERESTIMATION:COOLING 
			LoopType_Plant, // 20  HEATPUMP:WATERTOWATER:PARAMETERESTIMATION:HEATING 
			LoopType_Both, // 21  PIPE:ADIABATIC 
			LoopType_Both, // 22  PIPE:ADIABATIC:STEAM 
			LoopType_Both, // 23  PIPE:OUTDOOR 
			LoopType_Both, // 24  PIPE:INDOOR 
			LoopType_Both, // 25  PIPE:UNDERGROUND 
			LoopType_Both, // 26  DISTRICTCOOLING 
			LoopType_Both, // 27  DISTRICTHEATING 
			LoopType_Plant, // 28  THERMALSTORAGE:ICE:DETAILED 
			LoopType_Plant, // 29  THERMALSTORAGE:ICE:SIMPLE 
			LoopType_Both, // 30  TEMPERINGVALVE 
			LoopType_Both, // 31  WATERHEATER:MIXED 
			LoopType_Both, // 32  WATERHEATER:STRATIFIED 
			LoopType_Both, // 33  PUMP:VARIABLESPEED 
			LoopType_Both, // 34  PUMP:CONSTANTSPEED 
			LoopType_Both, // 35  PUMP:VARIABLESPEED:CONDENSATE 
			LoopType_Both, // 36  HEADEREDPUMPS:VARIABLESPEED 
			LoopType_Both, // 37  HEADEREDPUMPS:CONSTANTSPEED 
			LoopType_Plant, // 38  WATERUSE:CONNECTIONS 
			LoopType_Plant, // 39  COIL:COOLING:WATER 
			LoopType_Plant, // 40  COIL:COOLING:WATER:DETAILEDGEOMETRY 
			LoopType_Plant, // 41  COIL:HEATING:WATER 
			LoopType_Plant, // 42  COIL:HEATING:STEAM 
			LoopType_Plant, // 43  SOLARCOLLECTOR:FLATPLATE:WATER 
			LoopType_Both, // 44  LOADPROFILE:PLANT 
			LoopType_Both, // 45  GROUNDHEATEXCHANGER:VERTICAL 
			LoopType_Both, // 46  GROUNDHEATEXCHANGER:SURFACE 
			LoopType_Both, // 47  GROUNDHEATEXCHANGER:POND 
			LoopType_Plant, // 48  GENERATOR:MICROTURBINE 
			LoopType_Plant, // 49  GENERATOR:INTERNALCOMBUSTIONENGINE 
			LoopType_Plant, // 50  GENERATOR:COMBUSTIONTURBINE 
			LoopType_Plant, // 51  GENERATOR:MICROCHP 
			LoopType_Plant, // 52  GENERATOR:FUELCELL:STACKCOOLER 
			LoopType_Both, // 53  FLUIDCOOLER:SINGLESPEED 
			LoopType_Both, // 54  FLUIDCOOLER:TWOSPEED 
			LoopType_Both, // 55  EVAPORATIVEFLUIDCOOLER:SINGLESPEED 
			LoopType_Both, // 56  EVAPORATIVEFLUIDCOOLER:TWOSPEED 
			LoopType_Both, // 57  THERMALSTORAGE:CHILLEDWATER:MIXED 
			LoopType_Both, // 58  THERMALSTORAGE:CHILLEDWATER:STRATIFIED 
			LoopType_Both, // 59  SOLARCOLLECTOR:FLATPLATE:PHOTOVOLTAICTHERMAL 
			LoopType_Plant, // 60  ZONEHVAC:BASEBOARD:CONVECTIVE:WATER 
			LoopType_Plant, // 61  ZONEHVAC:BASEBOARD:RADIANTCONVECTIVE:STEAM 
			LoopType_Plant, // 62  ZONEHVAC:BASEBOARD:RADIANTCONVECTIVE:WATER 
			LoopType_Plant, // 63  ZONEHVAC:LOWTEMPERATURERADIANT:VARIABLEFLOW 
			LoopType_Plant, // 64  ZONEHVAC:LOWTEMPERATURERADIANT:CONSTANTFLOW 
			LoopType_Both, // 65  AirTerminal:SingleDuct:ConstantVolume:CooledBeam 
			LoopType_Both, // 66  Coil:Heating:WaterToAirHeatPump:EquationFit 
			LoopType_Both, // 67  Coil:Cooling:WaterTOAIRHeatPump:EquationFit 
			LoopType_Both, // 68  Coil:Heating:WaterTOAIRHeatPump:ParameterEstimation 
			LoopType_Both, // 69  Coil:Cooling:WaterTOAIRHeatPump:ParameterEstimation 
			LoopType_Both, // 70  Refrigeration:Condenser:WaterCooled 
			LoopType_Both, // 71  Refrigeration:CompressorRack 
			LoopType_Plant, // 72  AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed 
			LoopType_Plant, // 73  CHILLERHEATER:ABSORPTION:DOUBLEEFFECT 
			LoopType_Both, // 74  PipingSystem:Underground:PipeCircuit 
			LoopType_Both, // 75  SolarCollector:IntegralCollectorStorage 
			LoopType_Both, // 76  Coil:Heating:WaterToAirHeatPump:VariableSpeedEquationFit 
			LoopType_Both, // 77  Coil:Cooling:WaterTOAIRHeatPump:VariableSpeedEquationFit 
			LoopType_Both, // 78  PlantComponent:UserDefined 
			LoopType_Both, // 79  Coil:UserDefined 
			LoopType_Both, // 80  ZoneHVAC:ForcedAir:UserDefined 
			LoopType_Both, // 81  AirTerminal:SingleDuct:UserDefined 
			LoopType_Both, // 82  AirConditioner:VariableRefrigerantFlow 
			LoopType_Both, // 83  GroundHeatExchanger:HorizontalTrench 
			LoopType_Both, // 84  HeatExchanger:FluidToFluid 
			LoopType_Both, // 85  PlantComponent:TemperatureSource 
			LoopType_Plant, // 86  PlantCentralGroundSourceWrapper 
			LoopType_Plant, // 87  AirloopHVAC:UnitarySystem 
			LoopType_Both, // 88  Coil:Cooling:DX:SingleSpeed:ThermalStorage 
			LoopType_Both, // 89  CoolingTower:VariableSpeed:Merkel 
			LoopType_Both, // 90  SwimmingPool:Indoor
			LoopType_Plant // 91  ZoneHVAC:CoolingPanel:RadiantConvective:Water
		}
	); 

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
	int const TypeOf_HeatPumpWtrHeater( 16 );
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
	int const TypeOf_GrndHtExchgVertical( 45 );
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
	int const TypeOf_CoolingPanel_Simple( 91 );
	
	// Parameters for General Equipment Types
	int const NumGeneralEquipTypes( 23 );
	FArray1D_string const GeneralEquipTypes(
		NumGeneralEquipTypes, {
			"BOILER",
			"CHILLER",
			"COOLINGTOWER",
			"GENERATOR",
			"HEATEXCHANGER",
			"HEATPUMP",
			"PIPE",
			"PUMP",
			"DISTRICT",
			"THERMALSTORAGE",
			"TEMPERINGVALVE",
			"WATERHEATER",
			"WATERUSE",
			"DEMANDCOIL",
			"SOLARCOLLECTOR",
			"LOADPROFILE",
			"FLUIDCOOLER",
			"EVAPORATIVEFLUIDCOOLER",
			"GROUNDHEATEXCHANGER",
			"ZONEHVACDEMAND",
			"REFRIGERATION",
			"PLANTCOMPONENT",
			"CENTRALHEATPUMPSYSTEM"
		}
	);

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

	FArray1D_string const OpSchemeTypes(
		{0,12}, {
			"Load Range Based Operation",
			"PLANTEQUIPMENTOPERATION:HEATINGLOAD",
			"PLANTEQUIPMENTOPERATION:COOLINGLOAD",
			"PLANTEQUIPMENTOPERATION:OUTDOORWETBULB",
			"PLANTEQUIPMENTOPERATION:OUTDOORDRYBULB",
			"PLANTEQUIPMENTOPERATION:OUTDOORDEWPOINT",
			"PLANTEQUIPMENTOPERATION:OUTDOORRELATIVEHUMIDITY",
			"PLANTEQUIPMENTOPERATION:OUTDOORDRYBULBDIFFERENCE",
			"PLANTEQUIPMENTOPERATION:OUTDOORWETBULBDIFFERENCE",
			"PLANTEQUIPMENTOPERATION:OUTDOORDEWPOINTDIFFERENCE",
			"PLANTEQUIPMENTOPERATION:COMPONENTSETPOINT",
			"PLANTEQUIPMENTOPERATION:USERDEFINED",
			"PLANTEQUIPMENTOPERATION:UNCONTROLLED"
		}
	);

	// DERIVED TYPE DEFINITIONS:

	int const NumConvergenceHistoryTerms( 5 );
	FArray1D< Real64 > const ConvergenceHistoryARR( NumConvergenceHistoryTerms, { 0.0, -1.0, -2.0, -3.0, -4.0 } );
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
	Real64 EconLoadMet( 0.0 ); // Load met by Economizer
	int TotNumLoops( 0 ); // number of plant and condenser loops
	int TotNumHalfLoops( 0 ); // number of half loops (2 * TotNumLoops)
	bool PlantSizeNotComplete( true );
	bool PlantSizesOkayToFinalize( false ); // true if plant sizing is finishing and can save results
	bool AnyEMSPlantOpSchemesInModel( false );

	FArray1D_int EconBranchNum; // Branch num on which economizer is placed
	FArray1D_int EconCompNum; // Component num of economizer in the economizer branch

	FArray1D_bool CheckLoopEcon; // Flag for initializations
	FArray1D_bool EconOn; // Flag specifying if economizer is ON

	FArray1D_bool SimSupplySide; // DSU, sim ctrl flag for plant supply sides
	FArray1D_bool SimDemandSide; // DSU, sim ctrl flag for plant supply sides

	FArray1D_bool LoadChangeDownStream; // sim control flag.

	int PlantManageSubIterations( 0 ); // tracks plant iterations to characterize solver
	int PlantManageHalfLoopCalls( 0 ); // tracks number of half loop calls

	// these variables are arrays, allocated for the number of those particular loopsides, containing data for the vent reports
	// they are operated on like normal in almost all cases currently, except in the routine which actually mines data and sets them up
	// in that routine in SystemReports.cc, a POINTER is used to iterate over the different array variables below
	// this is why the TARGET attribute is applied to them here
	// further info can be found in SystemReports

	// Routines within this module

	// Object Data
	FArray1D< PipeData > Pipe;
	FArray1D< PlantLoopData > PlantLoop;
	FArray1D< PlantAvailMgrData > PlantAvailMgr;
	FArray1D< ReportVars > PlantReport;
	FArray1D< ReportLoopData > VentRepPlantSupplySide;
	FArray1D< ReportLoopData > VentRepPlantDemandSide;
	FArray1D< ReportLoopData > VentRepCondSupplySide;
	FArray1D< ReportLoopData > VentRepCondDemandSide;
	FArray1D< PlantCallingOrderInfoStruct > PlantCallingOrderInfo;

	// Functions

	void
	ScanPlantLoopsForObject(
		std::string const & CompName,
		int const CompType,
		int & LoopNum,
		int & LoopSideNum,
		int & BranchNum,
		int & CompNum,
		Optional< Real64 const > LowLimitTemp,
		Optional< Real64 const > HighLimitTemp,
		Optional_int CountMatchPlantLoops,
		Optional_int_const InletNodeNumber,
		Optional_int_const SingleLoopSearch,
		Optional_bool errFlag
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   November 2009
		//       MODIFIED       B. Griffith, changes to help with single component one multiple plant loops
		//       RE-ENGINEERED  na
		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine scans the plant loop structure trying to find the component by type then name.
		// If there are more than one match, it counts them up and returns count using an optional output arg
		// If the option input declaring the component inlet's node name, then the matching is more specific.
		// An optional input, lowlimittemp, can be passed in to be used in the PlantCondLoopOperation routines
		//  when distributing loads to components
		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.
		// REFERENCES:
		// na
		// Using/Aliasing
		using namespace DataGlobals;
		using InputProcessor::SameString;
		using General::RoundSigDigits;
		using BranchInputManager::AuditBranches;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int LoopCtr;
		int LoopSideCtr;
		int BranchCtr;
		int CompCtr;
		bool FoundComponent;
		int FoundCount;
		bool FoundCompName;
		int StartingLoopNum;
		int EndingLoopNum;
		//  logical :: printsteps

		FoundCount = 0;

		FoundComponent = false;
		FoundCompName = false;
		StartingLoopNum = 1;
		EndingLoopNum = TotNumLoops;
		if ( present( SingleLoopSearch ) ) {
			StartingLoopNum = SingleLoopSearch;
			EndingLoopNum = SingleLoopSearch;
		}

		PlantLoops: for ( LoopCtr = StartingLoopNum; LoopCtr <= EndingLoopNum; ++LoopCtr ) {
			auto & this_loop( PlantLoop( LoopCtr ) );
			for ( LoopSideCtr = 1; LoopSideCtr <= 2; ++LoopSideCtr ) {
				auto & this_loop_side( this_loop.LoopSide( LoopSideCtr ) );
				for ( BranchCtr = 1; BranchCtr <= this_loop_side.TotalBranches; ++BranchCtr ) {
					auto & this_branch( this_loop_side.Branch( BranchCtr ) );
					for ( CompCtr = 1; CompCtr <= this_branch.TotalComponents; ++CompCtr ) {
						auto & this_component( this_branch.Comp( CompCtr ) );
						if ( this_component.TypeOf_Num == CompType ) {
							if ( SameString( CompName, this_component.Name ) ) {
								FoundCompName = true;
								if ( present( InletNodeNumber ) ) {
									if ( InletNodeNumber > 0 ) {
										// check if inlet nodes agree
										if ( InletNodeNumber == this_component.NodeNumIn ) {
											FoundComponent = true;
											++FoundCount;
											LoopNum = LoopCtr;
											LoopSideNum = LoopSideCtr;
											BranchNum = BranchCtr;
											CompNum = CompCtr;
										}
									}
								} else {
									FoundComponent = true;
									++FoundCount;
									LoopNum = LoopCtr;
									LoopSideNum = LoopSideCtr;
									BranchNum = BranchCtr;
									CompNum = CompCtr;
								}
								if ( present( LowLimitTemp ) ) {
									this_component.MinOutletTemp = LowLimitTemp;
								}
								if ( present( HighLimitTemp ) ) {
									this_component.MaxOutletTemp = HighLimitTemp;
								}
							}
						}
					}
				}
			}
			PlantLoops_loop: ;
		}
		PlantLoops_exit: ;

		if ( ! FoundComponent ) {
			if ( CompType >= 1 && CompType <= NumSimPlantEquipTypes ) {
				if ( ! present( SingleLoopSearch ) ) {
					ShowSevereError( "Plant Component " + ccSimPlantEquipTypes( CompType ) + " called \"" + CompName + "\" was not found on any plant loops." );
					AuditBranches( true, ccSimPlantEquipTypes( CompType ), CompName );
				} else {
					ShowSevereError( "Plant Component " + ccSimPlantEquipTypes( CompType ) + " called \"" + CompName + "\" was not found on plant loop=\"" + PlantLoop( SingleLoopSearch ).Name + "\"." );
				}
				if ( present( InletNodeNumber ) ) {
					if ( FoundCompName ) {
						ShowContinueError( "Looking for matching inlet Node=\"" + NodeID( InletNodeNumber ) + "\"." );
					}
				}
				if ( present( SingleLoopSearch ) ) {
					ShowContinueError( "Look at Operation Scheme=\"" + PlantLoop( SingleLoopSearch ).OperationScheme + "\"." );
					ShowContinueError( "Look at Branches and Components on the Loop." );
					ShowBranchesOnLoop( SingleLoopSearch );
				}
				if ( present( errFlag ) ) errFlag = true;
			} else {
				ShowSevereError( "ScanPlantLoopsForObject: Invalid CompType passed [" + RoundSigDigits( CompType ) + "], Name=" + CompName );
				ShowContinueError( "Valid CompTypes are in the range [1 - " + RoundSigDigits( NumSimPlantEquipTypes ) + "]." );
				ShowFatalError( "Previous error causes program termination" );
			}
		}

		if ( present( CountMatchPlantLoops ) ) {
			CountMatchPlantLoops = FoundCount;
		}

	}

	void
	ScanPlantLoopsForNodeNum(
		std::string const & CallerName, // really used for error messages
		int const NodeNum, // index in Node structure of node to be scanned
		int & LoopNum, // return value for plant loop
		int & LoopSideNum, // return value for plant loop side
		int & BranchNum,
		Optional_int CompNum
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   Feb. 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Get routine to return plant loop index and plant loop side
		// based on node number.  for one time init routines only.

		// METHODOLOGY EMPLOYED:
		// Loop thru plant data structure and find matching node.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataGlobals;
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int LoopCtr;
		int LoopSideCtr;
		int BranchCtr;
		int CompCtr;
		bool FoundNode;
		int inFoundCount;
		int outFoundCount;

		inFoundCount = 0;
		outFoundCount = 0;
		if ( present( CompNum ) ) {
			CompNum = 0;
		}
		FoundNode = false;

		PlantLoops: for ( LoopCtr = 1; LoopCtr <= TotNumLoops; ++LoopCtr ) {
			auto & this_loop( PlantLoop( LoopCtr ) );
			for ( LoopSideCtr = 1; LoopSideCtr <= 2; ++LoopSideCtr ) {
				auto & this_loop_side( this_loop.LoopSide( LoopSideCtr ) );
				for ( BranchCtr = 1; BranchCtr <= this_loop_side.TotalBranches; ++BranchCtr ) {
					auto & this_branch( this_loop_side.Branch( BranchCtr ) );
					for ( CompCtr = 1; CompCtr <= this_branch.TotalComponents; ++CompCtr ) {
						auto & this_comp( this_branch.Comp( CompCtr ) );
						if ( NodeNum == this_comp.NodeNumIn ) {
							FoundNode = true;
							++inFoundCount;
							LoopNum = LoopCtr;
							LoopSideNum = LoopSideCtr;
							BranchNum = BranchCtr;
							if ( present( CompNum ) ) {
								CompNum = CompCtr;
							}
						}

						if ( NodeNum == this_comp.NodeNumOut ) {
							++outFoundCount;
							LoopNum = LoopCtr;
							LoopSideNum = LoopSideCtr;
							BranchNum = BranchCtr;
						}

					}
				}
			}
			PlantLoops_loop: ;
		}
		PlantLoops_exit: ;

		if ( ! FoundNode ) {
			ShowSevereError( "ScanPlantLoopsForNodeNum: Plant Node was not found as inlet node (for component) on any plant loops" );
			ShowContinueError( "Node Name=\"" + NodeID( NodeNum ) + "\"" );
			if ( ! DoingSizing ) {
				ShowContinueError( "called by " + CallerName );
			} else {
				ShowContinueError( "during sizing: called by " + CallerName );
			}
			if ( outFoundCount > 0 ) ShowContinueError( "Node was found as outlet node (for component) " + RoundSigDigits( outFoundCount ) + " time(s)." );
			ShowContinueError( "Possible error in Branch inputs.  For more information, look for other error messages related to this node name." );
			// fatal?
		}

	}

	bool
	AnyPlantLoopSidesNeedSim()
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   November 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na
		// PURPOSE OF THIS FUNCTION:
		// This subroutine scans the plant LoopSide simflags and returns if any of them are still true
		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.
		// REFERENCES:
		// na
		// USE STATEMENTS:
		// na

		// Return value
		bool AnyPlantLoopSidesNeedSim;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:
		// na

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int LoopCtr;
		int LoopSideCtr;

		//Assume that there aren't any
		AnyPlantLoopSidesNeedSim = false;

		//Then check if there are any
		for ( LoopCtr = 1; LoopCtr <= TotNumLoops; ++LoopCtr ) {
			for ( LoopSideCtr = 1; LoopSideCtr <= 2; ++LoopSideCtr ) {
				if ( PlantLoop( LoopCtr ).LoopSide( LoopSideCtr ).SimLoopSideNeeded ) {
					AnyPlantLoopSidesNeedSim = true;
					return AnyPlantLoopSidesNeedSim;
				}
			}
		}

		return AnyPlantLoopSidesNeedSim;
	}

	void
	SetAllPlantSimFlagsToValue( bool const Value )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   November 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  B. Griffith Feb 2009 DSU3
		// PURPOSE OF THIS SUBROUTINE:
		// Quickly sets all sim flags of a certain type (loop type/side) to a value
		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int LoopCtr;

		//Loop over all loops
		for ( LoopCtr = 1; LoopCtr <= TotNumLoops; ++LoopCtr ) {
			auto & this_loop( PlantLoop( LoopCtr ) );
			this_loop.LoopSide( DemandSide ).SimLoopSideNeeded = Value;
			this_loop.LoopSide( SupplySide ).SimLoopSideNeeded = Value;
		}

	}

	int
	GetLoopSidePumpIndex(
		int const LoopNum,
		int const LoopSideNum,
		int const BranchNum,
		int const CompNum
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   April 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na
		// PURPOSE OF THIS FUNCTION:
		// This subroutine scans the plant LoopSide pumps data structure, and returns the index or zero
		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.
		// REFERENCES:
		// na
		// USE STATEMENTS:
		// na

		// Return value
		int GetLoopSidePumpIndex;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int PumpCtr;

		// Assume it isn't found
		GetLoopSidePumpIndex = 0;

		// If there aren't any pumps on this loop side then just exit
		if ( ! allocated( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Pumps ) ) {
			return GetLoopSidePumpIndex;
		}

		// set up a nice reference
		auto & this_loop_side( PlantLoop( LoopNum ).LoopSide( LoopSideNum ) );

		// We can also make use of the TypeOfs to exit early
		if ( this_loop_side.Branch( BranchNum ).Comp( CompNum ).GeneralEquipType != GenEquipTypes_Pump ) return GetLoopSidePumpIndex;

		// Loop across all the loops on this loop/LoopSide, and check the branch/comp location
		for ( PumpCtr = 1; PumpCtr <= this_loop_side.TotalPumps; ++PumpCtr ) {
			if ( ( this_loop_side.Pumps( PumpCtr ).BranchNum == BranchNum ) && ( this_loop_side.Pumps( PumpCtr ).CompNum == CompNum ) ) {
				GetLoopSidePumpIndex = PumpCtr;
				break;
			}
		}

		return GetLoopSidePumpIndex;

	}

	void
	ShowBranchesOnLoop( int const LoopNum ) // Loop number of loop
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   November 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine will display (with continue error messages) the branch/component
		// structure of the given loop.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		std::string DemandSupply;
		int LSN; // LoopSide counter
		int BrN; // Branch counter
		int CpN; // Component (on branch) counter

		for ( LSN = DemandSide; LSN <= SupplySide; ++LSN ) {
			if ( LSN == DemandSide ) {
				DemandSupply = "Demand";
			} else if ( LSN == SupplySide ) {
				DemandSupply = "Supply";
			} else {
				DemandSupply = "Unknown";
			}
			ShowContinueError( DemandSupply + " Branches:" );
			for ( BrN = 1; BrN <= PlantLoop( LoopNum ).LoopSide( LSN ).TotalBranches; ++BrN ) {
				ShowContinueError( "  " + PlantLoop( LoopNum ).LoopSide( LSN ).Branch( BrN ).Name );
				ShowContinueError( "    Components on Branch:" );
				for ( CpN = 1; CpN <= PlantLoop( LoopNum ).LoopSide( LSN ).Branch( BrN ).TotalComponents; ++CpN ) {
					ShowContinueError( "      " + PlantLoop( LoopNum ).LoopSide( LSN ).Branch( BrN ).Comp( CpN ).TypeOf + ':' + PlantLoop( LoopNum ).LoopSide( LSN ).Branch( BrN ).Comp( CpN ).Name );
				}
			}
		}

	}

	int
	MyPlantSizingIndex(
		std::string const & CompType, // component description
		std::string const & CompName, // user name of component
		int const NodeNumIn, // component water inlet node
		int const NodeNumOut, // component water outlet node
		bool & ErrorsFound, // set to true if there's an error
		Optional_bool_const SupressErrors // used for WSHP's where condenser loop may not be on a plant loop
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   July 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Identify the correct Plant Sizing object for demand-side components such as heating and
		// cooling coils.

		// METHODOLOGY EMPLOYED:
		// This function searches all plant loops for a component whose input and
		// output nodes match the desired input & output nodes. This plant loop index is then used
		// to search the Plant Sizing array for the matching Plant Sizing object.

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using DataSizing::NumPltSizInput;
		using DataSizing::PlantSizData;
		//  USE DataPlant, ONLY: PlantLoop, ScanPlantLoopsForNodeNum

		// Return value
		int MyPltSizNum; // returned plant sizing index

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:

		int MyPltLoopNum;
		int PlantLoopNum;
		int DummyLoopSideNum;
		int DummyBranchNum;
		bool PrintErrorFlag;

		MyPltLoopNum = 0;
		MyPltSizNum = 0;
		ErrorsFound = false;
		if ( present( SupressErrors ) ) {
			PrintErrorFlag = SupressErrors;
		} else {
			PrintErrorFlag = true;
		}

		ScanPlantLoopsForNodeNum( "MyPlantSizingIndex", NodeNumIn, PlantLoopNum, DummyLoopSideNum, DummyBranchNum );

		if ( PlantLoopNum > 0 ) {
			MyPltLoopNum = PlantLoopNum;
		} else {
			MyPltLoopNum = 0;
		}

		if ( MyPltLoopNum > 0 ) {
			if ( NumPltSizInput > 0 ) {
				MyPltSizNum = FindItemInList( PlantLoop( MyPltLoopNum ).Name, PlantSizData.PlantLoopName(), NumPltSizInput );
			}
			if ( MyPltSizNum == 0 ) {
				if ( PrintErrorFlag ) {
					ShowSevereError( "MyPlantSizingIndex: Could not find " + PlantLoop( MyPltLoopNum ).Name + " in Sizing:Plant objects." );
					ShowContinueError( "...reference Component Type=\"" + CompType + "\", Name=\"" + CompName + "\"." );
				}
				ErrorsFound = true;
			}
		} else {
			if ( PrintErrorFlag ) {
				ShowWarningError( "MyPlantSizingIndex: Could not find " + CompType + " with name " + CompName + " on any plant loop" );
			}
			ErrorsFound = true;
		}

		return MyPltSizNum;

	}

	//     NOTICE

	//     Copyright � 1996-2014 The Board of Trustees of the University of Illinois
	//     and The Regents of the University of California through Ernest Orlando Lawrence
	//     Berkeley National Laboratory.  All rights reserved.

	//     Portions of the EnergyPlus software package have been developed and copyrighted
	//     by other individuals, companies and institutions.  These portions have been
	//     incorporated into the EnergyPlus software package under license.   For a complete
	//     list of contributors, see "Notice" located in main.cc.

	//     NOTICE: The U.S. Government is granted for itself and others acting on its
	//     behalf a paid-up, nonexclusive, irrevocable, worldwide license in this data to
	//     reproduce, prepare derivative works, and perform publicly and display publicly.
	//     Beginning five (5) years after permission to assert copyright is granted,
	//     subject to two possible five year renewals, the U.S. Government is granted for
	//     itself and others acting on its behalf a paid-up, non-exclusive, irrevocable
	//     worldwide license in this data to reproduce, prepare derivative works,
	//     distribute copies to the public, perform publicly and display publicly, and to
	//     permit others to do so.

	//     TRADEMARKS: EnergyPlus is a trademark of the US Department of Energy.

} // DataPlant

} // EnergyPlus
