#ifndef DataPlant_hh_INCLUDED
#define DataPlant_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>
#include <DataLoopNode.hh>

namespace EnergyPlus {

namespace DataPlant {

	// Using/Aliasing
	using DataLoopNode::SensedNodeFlagValue;

	// Data
	//MODULE PARAMETER DEFINITIONS:
	// Parameters for use in Load Distribution Schemes
	//extern int const OptimalLoading; // Optimal Load Distribution Scheme
	//extern int const SequentialLoading; // Seqential Load Distribution Scheme
	//extern int const UniformLoading; // Uniform Load Distribution Scheme
	//extern int const UniformPLRLoading; // Uniform PLR Load Distribution Scheme
	//extern int const SequentialUniformPLRLoading; // Sequential Uniform PLR Load Distribution Scheme

	int const OptimalLoading( 1 ); // Optimal Load Distribution Scheme
	int const SequentialLoading( 2 ); // Sequential Load Distribution Scheme
	int const UniformLoading( 3 ); // Uniform Load Distribution Scheme
	int const UniformPLRLoading( 4 ); // Uniform PLR Load Distribution Scheme
	int const SequentialUniformPLRLoading( 5 ); // Sequential Uniform PLR Load Distribution Scheme

	// Parameters for scheme types
	// Used in TYPE(OperationData)%OpSchemeType
	// As in PlantLoop(:)%OpScheme(:)%OpSchemeType
	// Also in PlantLoop()LoopSide()Branch()Comp()%CurOpSchemeType
	// this may be changed later...
	enum OpSchemeType { // Changed to enum: Better semantic fit and allows use in switch statements: Suggest this migration throughout EnergyPlus (and probably C++11 enum "class")
		UnknownStatusOpSchemeType = -2,
		NoControlOpSchemeType = -1, // Scheme Type placeholder for items such as pipes
		LoadRBOpSchemeType = 0, // Scheme Type for Load Range Based Operation (Deprecated)
		HeatingRBOpSchemeType = 1, // Scheme Type for Heating Load Range Based Operation
		CoolingRBOpSchemeType = 2, // Scheme Type for Cooling  Load Range Based Operation
		WetBulbRBOpSchemeType = 3, // Scheme Type for Wet bulb range based Operation
		DryBulbRBOpSchemeType = 4, // Scheme Type for Dry bulb range based Operation
		DewPointRBOpSchemeType = 5, // Scheme Type for Dewpoint range based Operation
		RelHumRBOpSchemeType = 6, // Scheme Type for relative humidity range based Operation
		DryBulbTDBOpSchemeType = 7, // Scheme Type for relative humidity range based Operation
		WetBulbTDBOpSchemeType = 8, // Scheme Type for Wet bulb range based Operation
		DewPointTDBOpSchemeType = 9, // Scheme Type for Wet bulb range based Operation
		CompSetPtBasedSchemeType = 10, // *Sankar Temp Based Control
		UncontrolledOpSchemeType = 11, // Scheme Type for Uncontrolled Operation
		EMSOpSchemeType = 12, // Scheme Type for EMS based operation user Define scheme
		PumpOpSchemeType = 13, // Not really an OpScheme, just a placeholder
		DemandOpSchemeType = 14, // Plcaeholder for demand side equipment such as coils
		FreeRejectionOpSchemeType = 15, // Scheme Type for waterside economizers and the like
		WSEconOpSchemeType = 16 // Scheme Type for waterside economizers and the like
	};

	// These are useful for SELECT CASE statements rather than listing all of the individual types listed above
	extern int const LoadRangeBasedMin;
	extern int const LoadRangeBasedMax;
	extern int const TempRangeBasedMin;
	extern int const TempRangeBasedMax;
	extern int const DeltaTempRangeBasedMin;
	extern int const DeltaTempRangeBasedMax;

	// SimFlagCriteriaTypes for use in performing interconnect re-sim checks
	extern int const CriteriaType_MassFlowRate;
	extern int const CriteriaType_Temperature;
	extern int const CriteriaType_HeatTransferRate;

	// Criteria percentage limits for determining re-simulation of connected loop sides
	extern Real64 const CriteriaDelta_MassFlowRate;
	extern Real64 const CriteriaDelta_Temperature;
	extern Real64 const CriteriaDelta_HeatTransferRate;

	// Parameters for loop flow request priority,
	//     used in logic to deal with Node%MassFlowRequest for determining overall loop flow rate
	extern int const LoopFlowStatus_Unknown; // component's status is not yet set
	extern int const LoopFlowStatus_NeedyAndTurnsLoopOn; // component is a "winner" for loop flow requests
	// active valve inside component that modulates flow
	//  gets the loop going under most conditions
	extern int const LoopFlowStatus_NeedyIfLoopOn; // component is a "winner" for loop flow requests
	// but doesn't normally get the loop going to start with
	//  once loop is going, may increase needs, non-zero minimums
	extern int const LoopFlowStatus_TakesWhatGets; // component is a "loser" for loop flow requests,
	// but if the loop is on it
	// it does make flow requests (for s/m resolution)

	//Parameters for component character wrt how load gets met (or not)
	//  used in %HowLoadServed to facilitate load dispatch logic
	extern int const HowMet_Unknown; // not yet set
	extern int const HowMet_NoneDemand; // does not meet a load, demand component
	extern int const HowMet_PassiveCap; // Passive machine, does what conditions allow but
	extern int const HowMet_ByNominalCap; // MaxLoad, MinLoad, OptLoad should work
	extern int const HowMet_ByNominalCapLowOutLimit; // MaxLoad, MinLoad, OptLoad but with low limit temp on outlet
	extern int const HowMet_ByNominalCapHiOutLimit; // MaxLoad, MinLoad, OptLoad but with high limit temp on outlet
	extern int const HowMet_ByNominalCapFreeCoolCntrl; // HowMet_ByNominalCap with free cool shutdown
	extern int const HowMet_ByNominalCapLowOutLimitFreeCoolCntrl; // HowMet_ByNominalCapLowOutLimit with free cool shutdown

	extern int const FreeCoolControlMode_WetBulb; // HeatExchanger:Hydronic model control type mode, outdoor wetbulb sensor
	extern int const FreeCoolControlMode_DryBulb; // HeatExchanger:Hydronic model control type mode, outdoor drybulb sensor
	extern int const FreeCoolControlMode_Loop; // HeatExchanger:Hydronic model control type mode, loop setpoint sensor

	// Parameters for use in Loop Demand Calculation Schemes
	extern int const SingleSetPoint; // Uses a single temp setpoint to calculate loop demand
	extern int const DualSetPointDeadBand; // Uses a dual temp setpoint with a deadband between the high
	//  and the low to calculate loop demand
	// Parameters for loop setpoint reference
	extern int const Air;
	extern int const Ground;
	extern int const LoopNode;

	// Parameters for common pipe
	extern int const CommonPipe_No;
	extern int const CommonPipe_Single;
	extern int const CommonPipe_TwoWay;

	// Parameters for loop side location
	extern int const DemandSupply_No;
	extern int const DemandSide;
	extern int const SupplySide;
	extern int const DemandSupply_Yes; // DSU

	extern Array1D_string const cLoopSideLocations;
	// Parameters for economizer
	extern int const Integrated;
	extern int const NonIntegrated;
	extern int const None;

	// Parameters for tolerance
	extern Real64 const LoopDemandTol; // minimum significant loop cooling or heating demand
	extern Real64 const DeltaTempTol; // minimum significant loop temperature difference

	// Parameters for Component/Equipment Types  (ref: TypeOf in CompData)
	extern int const LoopType_NoLoop;
	extern int const LoopType_Plant;
	extern int const LoopType_Condenser;
	extern int const LoopType_Both;

	// Parameters for calls to simflag routines
	extern int const PlantSupply;
	extern int const PlantDemand;
	extern int const CondSupply;
	extern int const CondDemand;

	// Parameters for FlowLock standardization
	extern int const FlowPumpQuery; // Used to ask the pumps for their min/max avail based on no constraints
	extern int const FlowUnlocked; // components request flow
	extern int const FlowLocked; // components take their inlet flow

	extern Array1D_string const cLoopTypes;

	// Pressure Routine Call Enumeration
	extern int const PressureCall_Init;
	extern int const PressureCall_Calc;
	extern int const PressureCall_Update;

	// Pressure Simulation Types
	extern int const Press_NoPressure; // Nothing for that particular loop
	extern int const Press_PumpPowerCorrection; // Only updating the pump power
	extern int const Press_FlowCorrection; // Update pump flow rate based on pump curve
	extern int const Press_FlowSimulation; // Full pressure network simulation
	extern Array1D_string const PressureSimType;
	// Parameters for Component/Equipment Types  (ref: TypeOf in CompData)
	extern int const NumSimPlantEquipTypes;
	extern Array1D_string const SimPlantEquipTypes; // 01 | 02 | 03 | 04 | 05 | 06 | 07 | 08 | 09 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25 | 26 | 27 | 28 | 29 | 30 | 31 | 32 | 33 | 34 | 35 | 36 | 37 | 38 ! demand side component | 39 ! demand side component | 40 ! demand side component | 41 ! demand side component | 42 ! demand side component | 43 ! demand side component | 44 ! demand side component' | 45 | 46 | 47 | 48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 | 58 | 59 | 60 | 61 | 62 | 63 | 64 | 65 | 66 | 67 | 68 | 69 | 70 | 71 | 72 | 73 | 74 | 75 | 76 | 77 | 78 | 79 | 80 | 81 | 82 | 83 | 84 | 85 | 86 | 87 | 88 | 89

	extern Array1D_string const ccSimPlantEquipTypes; // 01 | 02 | 03 | 04 | 05 | 06 | 07 | 08 | 09 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25 | 26 | 27 | 28 | 29 | 30 | 31 | 32 | 33 | 34 | 35 | 36 | 37 | 38 ! demand side component | 39 Demand Side Component | 40 Demand Side Component | 41 Demand Side Component | 42 Demand Side Component | 43 Demand Side Component | 44 Demand Side Component' | 45 | 46 | 47 | 48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 | 58 | 59 | 60 | 61 | 62 | 63 | 64 | 65 | 66 | 67 | 68 | 69 | 70 | 71 | 72 | 73 | 74 | 75 | 76 | 77 | 78 | 79 | 80 | 81 | 82 | 83 | 84 | 85 | 86 | 87 | 88 | 89

	extern Array1D_int const ValidLoopEquipTypes; // 01  BOILER:HOTWATER | 02  BOILER:STEAM | 03  CHILLER:ABSORPTION | 04  CHILLER:ABSORPTION:INDIRECT | 05  CHILLER:COMBUSTIONTURBINE | 06  CHILLER:CONSTANTCOP | 07  CHILLERHEATER:ABSORPTION:DIRECTFIRED | 08  CHILLER:ELECTRIC | 09  CHILLER:ELECTRIC:EIR | 10  CHILLER:ELECTRIC:REFORMULATEDEIR | 11  CHILLER:ENGINEDRIVEN | 12  COOLINGTOWER:SINGLESPEED | 13  COOLINGTOWER:TWOSPEED | 14  COOLINGTOWER:VARIABLESPEED | 15  GENERATOR:FUELCELL:EXHAUSTGASTOWATERHEATEXCHANGER | 16  WATERHEATER:HEATPUMP:PUMPEDCONDENSER | 17  HEATPUMP:WATERTOWATER:EQUATIONFIT:COOLING | 18  HEATPUMP:WATERTOWATER:EQUATIONFIT:HEATING | 19  HEATPUMP:WATERTOWATER:PARAMETERESTIMATION:COOLING | 20  HEATPUMP:WATERTOWATER:PARAMETERESTIMATION:HEATING | 21  PIPE:ADIABATIC | 22  PIPE:ADIABATIC:STEAM | 23  PIPE:OUTDOOR | 24  PIPE:INDOOR | 25  PIPE:UNDERGROUND | 26  DISTRICTCOOLING | 27  DISTRICTHEATING | 28  THERMALSTORAGE:ICE:DETAILED | 29  THERMALSTORAGE:ICE:SIMPLE | 30  TEMPERINGVALVE | 31  WATERHEATER:MIXED | 32  WATERHEATER:STRATIFIED | 33  PUMP:VARIABLESPEED | 34  PUMP:CONSTANTSPEED | 35  PUMP:VARIABLESPEED:CONDENSATE | 36  HEADEREDPUMPS:VARIABLESPEED | 37  HEADEREDPUMPS:CONSTANTSPEED | 38  WATERUSE:CONNECTIONS | 39  COIL:COOLING:WATER | 40  COIL:COOLING:WATER:DETAILEDGEOMETRY | 41  COIL:HEATING:WATER | 42  COIL:HEATING:STEAM | 43  SOLARCOLLECTOR:FLATPLATE:WATER | 44  LOADPROFILE:PLANT | 45  GROUNDHEATEXCHANGER:VERTICAL | 46  GROUNDHEATEXCHANGER:SURFACE | 47  GROUNDHEATEXCHANGER:POND | 48  GENERATOR:MICROTURBINE | 49  GENERATOR:INTERNALCOMBUSTIONENGINE | 50  GENERATOR:COMBUSTIONTURBINE | 51  GENERATOR:MICROCHP | 52  GENERATOR:FUELCELL:STACKCOOLER | 53  FLUIDCOOLER:SINGLESPEED | 54  FLUIDCOOLER:TWOSPEED | 55  EVAPORATIVEFLUIDCOOLER:SINGLESPEED | 56  EVAPORATIVEFLUIDCOOLER:TWOSPEED | 57  THERMALSTORAGE:CHILLEDWATER:MIXED | 58  THERMALSTORAGE:CHILLEDWATER:STRATIFIED | 59  SOLARCOLLECTOR:FLATPLATE:PHOTOVOLTAICTHERMAL | 60  ZONEHVAC:BASEBOARD:CONVECTIVE:WATER | 61  ZONEHVAC:BASEBOARD:RADIANTCONVECTIVE:STEAM | 62  ZONEHVAC:BASEBOARD:RADIANTCONVECTIVE:WATER | 63  ZONEHVAC:LOWTEMPERATURERADIANT:VARIABLEFLOW | 64  ZONEHVAC:LOWTEMPERATURERADIANT:CONSTANTFLOW | 65 AirTerminal:SingleDuct:ConstantVolume:CooledBeam | 66  Coil:Heating:WaterToAirHeatPump:EquationFit | 67  Coil:Cooling:WaterTOAIRHeatPump:EquationFit | 68  Coil:Heating:WaterTOAIRHeatPump:ParameterEstimation | 69  Coil:Cooling:WaterTOAIRHeatPump:ParameterEstimation | 70  Refrigeration:Condenser:WaterCooled | 71  Refrigeration:CompressorRack | 72  AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed | 73  CHILLERHEATER:ABSORPTION:DOUBLEEFFECT | 74  PipingSystem:Underground:PipeCircuit | 75  SolarCollector:IntegralCollectorStorage | 76  Coil:Heating:WaterToAirHeatPump:VariableSpeedEquationFit | 77  Coil:Cooling:WaterTOAIRHeatPump:VariableSpeedEquationFit | 78  PlantComponent:UserDefined | 79  Coil:UserDefined | 80  ZoneHVAC:ForcedAir:UserDefined | 81  AirTerminal:SingleDuct:UserDefined | 82  AirConditioner:VariableRefrigerantFlow | 83  GroundHeatExchanger:HorizontalTrench | 84  HeatExchanger:FluidToFluid | 85  PlantComponent:TemperatureSource | 86  PlantCentralGroundSourceWrapper | 87  AirloopHVAC:UnitarySystem | 88  Coil:Cooling:DX:SingleSpeed:ThermalStorage | 89  CoolingTower:VariableSpeed:Merkel

	extern int const TypeOf_Other;
	extern int const TypeOf_Boiler_Simple;
	extern int const TypeOf_Boiler_Steam;
	extern int const TypeOf_Chiller_Absorption; // older BLAST absorption chiller
	extern int const TypeOf_Chiller_Indirect_Absorption; // revised absorption chiller
	extern int const TypeOf_Chiller_CombTurbine;
	extern int const TypeOf_Chiller_ConstCOP;
	extern int const TypeOf_Chiller_DFAbsorption;
	extern int const TypeOf_Chiller_Electric; // basic BLAST Chiller
	extern int const TypeOf_Chiller_ElectricEIR;
	extern int const TypeOf_Chiller_ElectricReformEIR;
	extern int const TypeOf_Chiller_EngineDriven;
	extern int const TypeOf_CoolingTower_SingleSpd;
	extern int const TypeOf_CoolingTower_TwoSpd;
	extern int const TypeOf_CoolingTower_VarSpd;
	extern int const TypeOf_Generator_FCExhaust;
	extern int const TypeOf_HeatPumpWtrHeaterPumped;
	extern int const TypeOf_HeatPumpWtrHeaterWrapped;
	extern int const TypeOf_HPWaterEFCooling;
	extern int const TypeOf_HPWaterEFHeating;
	extern int const TypeOf_HPWaterPECooling;
	extern int const TypeOf_HPWaterPEHeating;
	extern int const TypeOf_Pipe;
	extern int const TypeOf_PipeSteam;
	extern int const TypeOf_PipeExterior;
	extern int const TypeOf_PipeInterior;
	extern int const TypeOf_PipeUnderground;
	extern int const TypeOf_PurchChilledWater;
	extern int const TypeOf_PurchHotWater;
	extern int const TypeOf_TS_IceDetailed;
	extern int const TypeOf_TS_IceSimple;
	extern int const TypeOf_ValveTempering;
	extern int const TypeOf_WtrHeaterMixed;
	extern int const TypeOf_WtrHeaterStratified;
	extern int const TypeOf_PumpVariableSpeed;
	extern int const TypeOf_PumpConstantSpeed;
	extern int const TypeOf_PumpCondensate;
	extern int const TypeOf_PumpBankVariableSpeed;
	extern int const TypeOf_PumpBankConstantSpeed;
	extern int const TypeOf_WaterUseConnection;
	extern int const TypeOf_CoilWaterCooling; // demand side component
	extern int const TypeOf_CoilWaterDetailedFlatCooling; // demand side component
	extern int const TypeOf_CoilWaterSimpleHeating; // demand side component
	extern int const TypeOf_CoilSteamAirHeating; // demand side component
	extern int const TypeOf_SolarCollectorFlatPlate; // demand side component
	extern int const TypeOf_PlantLoadProfile; // demand side component
	extern int const TypeOf_GrndHtExchgVertical;
	extern int const TypeOf_GrndHtExchgSurface;
	extern int const TypeOf_GrndHtExchgPond;
	extern int const TypeOf_Generator_MicroTurbine; // newer FSEC turbine
	extern int const TypeOf_Generator_ICEngine;
	extern int const TypeOf_Generator_CTurbine; // older BLAST turbine
	extern int const TypeOf_Generator_MicroCHP;
	extern int const TypeOf_Generator_FCStackCooler;
	extern int const TypeOf_FluidCooler_SingleSpd;
	extern int const TypeOf_FluidCooler_TwoSpd;
	extern int const TypeOf_EvapFluidCooler_SingleSpd;
	extern int const TypeOf_EvapFluidCooler_TwoSpd;
	extern int const TypeOf_ChilledWaterTankMixed;
	extern int const TypeOf_ChilledWaterTankStratified;
	extern int const TypeOf_PVTSolarCollectorFlatPlate;
	extern int const TypeOf_Baseboard_Conv_Water;
	extern int const TypeOf_Baseboard_Rad_Conv_Steam;
	extern int const TypeOf_Baseboard_Rad_Conv_Water;
	extern int const TypeOf_LowTempRadiant_VarFlow;
	extern int const TypeOf_LowTempRadiant_ConstFlow;
	extern int const TypeOf_CooledBeamAirTerminal;
	extern int const TypeOf_CoilWAHPHeatingEquationFit;
	extern int const TypeOf_CoilWAHPCoolingEquationFit;
	extern int const TypeOf_CoilWAHPHeatingParamEst;
	extern int const TypeOf_CoilWAHPCoolingParamEst;
	extern int const TypeOf_RefrigSystemWaterCondenser;
	extern int const TypeOf_RefrigerationWaterCoolRack;
	extern int const TypeOf_MultiSpeedHeatPumpRecovery;
	extern int const TypeOf_Chiller_ExhFiredAbsorption;
	extern int const TypeOf_PipingSystemPipeCircuit;
	extern int const TypeOf_SolarCollectorICS;
	extern int const TypeOf_CoilVSWAHPHeatingEquationFit;
	extern int const TypeOf_CoilVSWAHPCoolingEquationFit;
	extern int const TypeOf_PlantComponentUserDefined;
	extern int const TypeOf_CoilUserDefined;
	extern int const TypeOf_ZoneHVACAirUserDefined;
	extern int const TypeOf_AirTerminalUserDefined;
	extern int const TypeOf_HeatPumpVRF;
	extern int const TypeOf_GrndHtExchgHorizTrench;
	extern int const TypeOf_FluidToFluidPlantHtExchg;
	extern int const TypeOf_WaterSource;
	extern int const TypeOf_CentralGroundSourceHeatPump;
	extern int const TypeOf_UnitarySystemRecovery;
	extern int const TypeOf_PackagedTESCoolingCoil;
	extern int const TypeOf_CoolingTower_VarSpdMerkel;
	extern int const TypeOf_SwimmingPool_Indoor;
	extern int const TypeOf_GrndHtExchgSlinky;
	extern int const TypeOf_FourPipeBeamAirTerminal;

	// Parameters for General Equipment Types
	extern int const NumGeneralEquipTypes;
	extern Array1D_string const GeneralEquipTypes;

	extern int const GenEquipTypes_Boiler;
	extern int const GenEquipTypes_Chiller;
	extern int const GenEquipTypes_CoolingTower;
	extern int const GenEquipTypes_Generator;
	extern int const GenEquipTypes_HeatExchanger;
	extern int const GenEquipTypes_HeatPump;
	extern int const GenEquipTypes_Pipe;
	extern int const GenEquipTypes_Pump;
	extern int const GenEquipTypes_Purchased;
	extern int const GenEquipTypes_ThermalStorage;
	extern int const GenEquipTypes_Valve;
	extern int const GenEquipTypes_WaterThermalTank;
	extern int const GenEquipTypes_WaterUse;
	extern int const GenEquipTypes_DemandCoil;
	extern int const GenEquipTypes_SolarCollector;
	extern int const GenEquipTypes_LoadProfile;
	extern int const GenEquipTypes_FluidCooler;
	extern int const GenEquipTypes_EvapFluidCooler;
	extern int const GenEquipTypes_GroundHeatExchanger;
	extern int const GenEquipTypes_ZoneHVACDemand;
	extern int const GenEquipTypes_Refrigeration;
	extern int const GenEquipTypes_PlantComponent;
	extern int const GenEquipTypes_CentralHeatPumpSystem;

	extern Array1D_string const OpSchemeTypes; // long since Deprecated, remove?

	// DERIVED TYPE DEFINITIONS:

	// two-way common pipe variables
	//TYPE TwoWayCommonPipeData
	//  LOGICAL   :: SetpointOnSecondarySide  ! true if control point is demand inlet, otherwise supply inlet
	//  REAL(r64) :: CurSecCPLegFlow    !Mass flow rate in primary common pipe leg
	//  REAL(r64) :: CurPriCPLegFlow    !Mass flow rate in secondary common pipe leg
	//  REAL(r64) :: CurSectoPriFlow    !Secondary side to Primary side Mass flow rate
	//  REAL(r64) :: CurPritoSecFlow    !Primary side to Secondary side Mass flow rate
	//  REAL(r64) :: CurSecOutTemp      !Secondary outlet temperature
	//  REAL(r64) :: CurPriOutTemp      !Primary outlet temperature
	//  REAL(r64) :: CurPriInTemp       !Primary inlet temperature
	//  REAL(r64) :: CurSecInTemp       !Secondary inlet temperature
	//  !new/missing?
	//  REAL(r64) :: mdotPriRequest     ! primary total flow request
	//END TYPE TwoWayCommonPipeData

	extern int const NumConvergenceHistoryTerms;
	extern Array1D< Real64 > const ConvergenceHistoryARR;
	extern Real64 const sum_ConvergenceHistoryARR;
	extern Real64 const square_sum_ConvergenceHistoryARR;
	extern Real64 const sum_square_ConvergenceHistoryARR;

	// The same as TYPE DefinePriAirSysAvailMgrs in DataAirLoop.cc.  A common definition would be nicer.

	// The next three types (all starting with RepReport) are the "shadow"
	// derived types for the ventilation reports.  It keeps the node and
	// other connection information and adds variables for the ventilation
	// reports.  This is the cleanest way to do this and not impact other
	// data structures.  The actual derived types are defined (as allocatable)
	// below with the rest of the declarations.

	//MODULE VARIABLE DECLARATIONS:

	extern int NumPipes; // Total number of pipes
	extern int NumPlantPipes; // Total number of plant pipes
	extern int NumCondPipes; // Total number of condenser pipes
	extern Real64 EconLoadMet; // Load met by Economizer
	extern int TotNumLoops; // number of plant and condenser loops
	extern int TotNumHalfLoops; // number of half loops (2 * TotNumLoops)
	extern bool PlantFirstSizeCompleted; //true if first-pass sizing is still going on and not finished
	extern bool PlantFirstSizesOkayToFinalize; // true if first-pass plant sizing is finish and can save results for simulation
	extern bool PlantFirstSizesOkayToReport; // true if initial first pass size can be reported
	extern bool PlantFinalSizesOkayToReport; // true if plant sizing is really all done and final results reported
	extern bool PlantReSizingCompleted;

	extern bool AnyEMSPlantOpSchemesInModel;

	extern Array1D_int EconBranchNum; // Branch num on which economizer is placed
	extern Array1D_int EconCompNum; // Component num of economizer in the economizer branch

	extern Array1D_bool CheckLoopEcon; // Flag for initializations
	extern Array1D_bool EconOn; // Flag specifying if economizer is ON

	extern Array1D_bool SimSupplySide; // DSU, sim ctrl flag for plant supply sides
	extern Array1D_bool SimDemandSide; // DSU, sim ctrl flag for plant supply sides

	extern Array1D_bool LoadChangeDownStream; // sim control flag.

	extern int PlantManageSubIterations; // tracks plant iterations to characterize solver
	extern int PlantManageHalfLoopCalls; // tracks number of half loop calls

	// two-way common pipe variables
	//REAL(r64),SAVE,ALLOCATABLE,DIMENSION(:)    :: CurSecCPLegFlow    !Mass flow rate in primary common pipe leg
	//REAL(r64),SAVE,ALLOCATABLE,DIMENSION(:)    :: CurPriCPLegFlow    !Mass flow rate in secondary common pipe leg
	//REAL(r64),SAVE,ALLOCATABLE,DIMENSION(:)    :: CurSectoPriFlow    !Secondary side to Primary side Mass flow rate
	//REAL(r64),SAVE,ALLOCATABLE,DIMENSION(:)    :: CurPritoSecFlow    !Primary side to Secondary side Mass flow rate
	//REAL(r64),SAVE,ALLOCATABLE,DIMENSION(:)    :: CurSecOutTemp      !Secondary outlet temperature
	//REAL(r64),SAVE,ALLOCATABLE,DIMENSION(:)    :: CurPriOutTemp      !Primary outlet temperature
	//REAL(r64),SAVE,ALLOCATABLE,DIMENSION(:)    :: CurPriInTemp       !Primary inlet temperature
	//REAL(r64),SAVE,ALLOCATABLE,DIMENSION(:)    :: CurSecInTemp       !Secondary inlet temperature

	// these variables are arrays, allocated for the number of those particular loopsides, containing data for the vent reports
	// they are operated on like normal in almost all cases currently, except in the routine which actually mines data and sets them up
	// in that routine in SystemReports.cc, a POINTER is used to iterate over the different array variables below
	// this is why the TARGET attribute is applied to them here
	// further info can be found in SystemReports

	// Routines within this module

	// Types

	struct SplitterData
	{
		// Members
		bool Exists; // True if there is a splitter (only 1 allowed per loop)
		std::string Name; // Name of the splitter
		int NodeNumIn; // Node number for the inlet to the splitter
		int BranchNumIn; // Reference number for branch connected to splitter inlet
		int LevelIn; // Branch Level of splitter inlet in the loop topology
		int LevelOut; // Branch Level of splitter outlet in the loop topology
		int CorrMixIndex; // Index of Mixer corresponding to this Splitter
		std::string NodeNameIn; // Node name for the inlet to the splitter
		int TotalOutletNodes; // Number of outlet nodes for the splitter
		Array1D_int NodeNumOut; // Node number for the outlet to the splitter
		Array1D_int BranchNumOut; // Reference number for branch connected to splitter outlet
		Array1D_string NodeNameOut; // Node name for the outlet to the splitter

		// Default Constructor
		SplitterData() :
			Exists( false ),
			NodeNumIn( 0 ),
			BranchNumIn( 0 ),
			LevelIn( 0 ),
			LevelOut( 0 ),
			CorrMixIndex( 0 ),
			TotalOutletNodes( 0 )
		{}

		// Member Constructor
		SplitterData(
			bool const Exists, // True if there is a splitter (only 1 allowed per loop)
			std::string const & Name, // Name of the splitter
			int const NodeNumIn, // Node number for the inlet to the splitter
			int const BranchNumIn, // Reference number for branch connected to splitter inlet
			int const LevelIn, // Branch Level of splitter inlet in the loop topology
			int const LevelOut, // Branch Level of splitter outlet in the loop topology
			int const CorrMixIndex, // Index of Mixer corresponding to this Splitter
			std::string const & NodeNameIn, // Node name for the inlet to the splitter
			int const TotalOutletNodes, // Number of outlet nodes for the splitter
			Array1_int const & NodeNumOut, // Node number for the outlet to the splitter
			Array1_int const & BranchNumOut, // Reference number for branch connected to splitter outlet
			Array1_string const & NodeNameOut // Node name for the outlet to the splitter
		) :
			Exists( Exists ),
			Name( Name ),
			NodeNumIn( NodeNumIn ),
			BranchNumIn( BranchNumIn ),
			LevelIn( LevelIn ),
			LevelOut( LevelOut ),
			CorrMixIndex( CorrMixIndex ),
			NodeNameIn( NodeNameIn ),
			TotalOutletNodes( TotalOutletNodes ),
			NodeNumOut( NodeNumOut ),
			BranchNumOut( BranchNumOut ),
			NodeNameOut( NodeNameOut )
		{}

	};

	struct MixerData
	{
		// Members
		bool Exists; // True if there is a mixer (only 1 allowed per loop)
		std::string Name; // Name of the mixer
		int NodeNumOut; // Node number for the outlet to the mixer
		int BranchNumOut; // Reference number for branch connected to splitter inlet
		int LevelIn; // Branch Level of mixer inlet in the loop topology
		int LevelOut; // Branch Level of mixer outlet in the loop topology
		int CorrSplitIndex; // Index of Splitter corresponding to this mixer
		std::string NodeNameOut; // Node name for the outlet to the mixer
		int TotalInletNodes; // Number of inlet nodes for the mixer
		Array1D_int NodeNumIn; // Node number for the inlet to the mixer
		Array1D_int BranchNumIn; // Reference number for branch connected to splitter outlet
		Array1D_string NodeNameIn; // Node name for the inlet to the mixer

		// Default Constructor
		MixerData() :
			Exists( false ),
			NodeNumOut( 0 ),
			BranchNumOut( 0 ),
			LevelIn( 0 ),
			LevelOut( 0 ),
			CorrSplitIndex( 0 ),
			TotalInletNodes( 0 )
		{}

		// Member Constructor
		MixerData(
			bool const Exists, // True if there is a mixer (only 1 allowed per loop)
			std::string const & Name, // Name of the mixer
			int const NodeNumOut, // Node number for the outlet to the mixer
			int const BranchNumOut, // Reference number for branch connected to splitter inlet
			int const LevelIn, // Branch Level of mixer inlet in the loop topology
			int const LevelOut, // Branch Level of mixer outlet in the loop topology
			int const CorrSplitIndex, // Index of Splitter corresponding to this mixer
			std::string const & NodeNameOut, // Node name for the outlet to the mixer
			int const TotalInletNodes, // Number of inlet nodes for the mixer
			Array1_int const & NodeNumIn, // Node number for the inlet to the mixer
			Array1_int const & BranchNumIn, // Reference number for branch connected to splitter outlet
			Array1_string const & NodeNameIn // Node name for the inlet to the mixer
		) :
			Exists( Exists ),
			Name( Name ),
			NodeNumOut( NodeNumOut ),
			BranchNumOut( BranchNumOut ),
			LevelIn( LevelIn ),
			LevelOut( LevelOut ),
			CorrSplitIndex( CorrSplitIndex ),
			NodeNameOut( NodeNameOut ),
			TotalInletNodes( TotalInletNodes ),
			NodeNumIn( NodeNumIn ),
			BranchNumIn( BranchNumIn ),
			NodeNameIn( NodeNameIn )
		{}

	};

	struct LoopSidePumpInformation
	{
		// Members
		std::string PumpName;
		int PumpTypeOf;
		int BranchNum;
		int CompNum;
		int PumpOutletNode;
		Real64 PumpFlowRate;
		Real64 CurrentMinAvail;
		Real64 CurrentMaxAvail;
		Real64 PumpHeatToFluid;

		// Default Constructor
		LoopSidePumpInformation() :
			PumpTypeOf( 0 ),
			BranchNum( 0 ),
			CompNum( 0 ),
			PumpOutletNode( 0 ),
			PumpFlowRate( 0.0 ),
			CurrentMinAvail( 0.0 ),
			CurrentMaxAvail( 0.0 ),
			PumpHeatToFluid( 0.0 )
		{}

		// Member Constructor
		LoopSidePumpInformation(
			std::string const & PumpName,
			int const PumpTypeOf,
			int const BranchNum,
			int const CompNum,
			int const PumpOutletNode,
			Real64 const PumpFlowRate,
			Real64 const CurrentMinAvail,
			Real64 const CurrentMaxAvail,
			Real64 const PumpHeatToFluid
		) :
			PumpName( PumpName ),
			PumpTypeOf( PumpTypeOf ),
			BranchNum( BranchNum ),
			CompNum( CompNum ),
			PumpOutletNode( PumpOutletNode ),
			PumpFlowRate( PumpFlowRate ),
			CurrentMinAvail( CurrentMinAvail ),
			CurrentMaxAvail( CurrentMaxAvail ),
			PumpHeatToFluid( PumpHeatToFluid )
		{}

	};

	struct EquipListPtrData
	{
		// Members
		int ListPtr; // points to List on OpScheme on plant loop:
		// PlantLoop(LoopNum)%OpScheme(Optschemeptr)%List(ListPtr)...
		int CompPtr; // points to this component on List on OpScheme on plant loop:
		// PlantLoop(LoopNum)%OpScheme(Optschemeptr)%List(ListPtr)%Comp(compPtr)

		// Default Constructor
		EquipListPtrData() :
			ListPtr( 0 ),
			CompPtr( 0 )
		{}

		// Member Constructor
		EquipListPtrData(
			int const ListPtr, // points to List on OpScheme on plant loop:
			int const CompPtr // points to this component on List on OpScheme on plant loop:
		) :
			ListPtr( ListPtr ),
			CompPtr( CompPtr )
		{}

	};

	struct OpSchemePtrData
	{
		// Members
		int OpSchemePtr; // DSU points to OpScheme on plant loop:
		// PlantLoop(LoopNum)%OpScheme(Optschemeptr)...
		int NumEquipLists; // DSU ALLOCATABLE to the schedule (for valid schedules)
		Array1D< EquipListPtrData > EquipList; // DSU Component  list

		// Default Constructor
		OpSchemePtrData() :
			OpSchemePtr( 0 ),
			NumEquipLists( 0 )
		{}

		// Member Constructor
		OpSchemePtrData(
			int const OpSchemePtr, // DSU points to OpScheme on plant loop:
			int const NumEquipLists, // DSU ALLOCATABLE to the schedule (for valid schedules)
			Array1< EquipListPtrData > const & EquipList // DSU Component  list
		) :
			OpSchemePtr( OpSchemePtr ),
			NumEquipLists( NumEquipLists ),
			EquipList( EquipList )
		{}

	};

	struct CompData
	{
		// Members
		std::string TypeOf; // The 'keyWord' identifying  component type
		int TypeOf_Num; // Reference the "TypeOf" parameters in DataPlant
		int GeneralEquipType; // General Equipment Type (e.g. Chillers, Pumps, etc)
		std::string Name; // Component name
		int CompNum; // Component ID number
		int FlowCtrl; // flow control for splitter/mixer (ACTIVE/PASSIVE/BYPASS)
		int FlowPriority; // status for overall loop flow determination
		bool ON; // TRUE = designated component or operation scheme available
		bool Available; // TRUE = designated component or operation scheme available
		std::string NodeNameIn; // Component inlet node name
		std::string NodeNameOut; // Component outlet node name
		int NodeNumIn; // Component inlet node number
		int NodeNumOut; // Component outlet node number
		Real64 MyLoad; // Distributed Load
		Real64 MaxLoad; // Maximum load
		Real64 MinLoad; // Minimum Load
		Real64 OptLoad; // Optimal Load
		Real64 SizFac; // Sizing Fraction
		int CurOpSchemeType; // updated pointer to
		// Plant()%OpScheme(CurOpSchemeType)...
		int NumOpSchemes; // number of schemes held in the pointer array
		int CurCompLevelOpNum; // pointer to the OpScheme array defined next
		// PlantLoop()%LoopSide()%Branch()%Comp()%OpScheme(curOpSchemePtr)
		Array1D< OpSchemePtrData > OpScheme; // Pointers to component on lists
		Real64 EquipDemand; // Component load request based on inlet temp and outlet SP
		bool EMSLoadOverrideOn; // EMS is calling to override load dispatched to component
		Real64 EMSLoadOverrideValue; // EMS value to use for load when overridden [W] always positive.
		int HowLoadServed; // nature of component in terms of how it can meet load
		Real64 MinOutletTemp; // Component exit lower limit temperature
		Real64 MaxOutletTemp; // Component exit upper limit temperature
		bool FreeCoolCntrlShutDown; // true if component was shut down because of free cooling
		Real64 FreeCoolCntrlMinCntrlTemp; // current control temp value for free cooling controls
		int FreeCoolCntrlMode; // type of sensor used for free cooling controls
		int FreeCoolCntrlNodeNum; // chiller condenser inlet node number for free cooling controls
		int IndexInLoopSidePumps; // If I'm a pump, this tells my index in PL(:)%LS(:)%Pumps
		Real64 TempDesCondIn;
		Real64 TempDesEvapOut;

		// Default Constructor
		CompData() :
			TypeOf_Num( 0 ),
			GeneralEquipType( 0 ),
			CompNum( 0 ),
			FlowCtrl( 0 ),
			FlowPriority( LoopFlowStatus_Unknown ),
			ON( false ),
			Available( false ),
			NodeNumIn( 0 ),
			NodeNumOut( 0 ),
			MyLoad( 0.0 ),
			MaxLoad( 0.0 ),
			MinLoad( 0.0 ),
			OptLoad( 0.0 ),
			SizFac( 0.0 ),
			CurOpSchemeType( UnknownStatusOpSchemeType ),
			NumOpSchemes( 0 ),
			CurCompLevelOpNum( 0 ),
			EquipDemand( 0.0 ),
			EMSLoadOverrideOn( false ),
			EMSLoadOverrideValue( 0.0 ),
			HowLoadServed( HowMet_Unknown ),
			MinOutletTemp( 0.0 ),
			MaxOutletTemp( 0.0 ),
			FreeCoolCntrlShutDown( false ),
			FreeCoolCntrlMinCntrlTemp( 0.0 ),
			FreeCoolCntrlMode( 0 ),
			FreeCoolCntrlNodeNum( 0 ),
			IndexInLoopSidePumps( 0 ),
			TempDesCondIn( 0.0 ),
			TempDesEvapOut( 0.0 )
		{}

		// Member Constructor
		CompData(
			std::string const & TypeOf, // The 'keyWord' identifying  component type
			int const TypeOf_Num, // Reference the "TypeOf" parameters in DataPlant
			int const GeneralEquipType, // General Equipment Type (e.g. Chillers, Pumps, etc)
			std::string const & Name, // Component name
			int const CompNum, // Component ID number
			int const FlowCtrl, // flow control for splitter/mixer (ACTIVE/PASSIVE/BYPASS)
			int const FlowPriority, // status for overall loop flow determination
			bool const ON, // TRUE = designated component or operation scheme available
			bool const Available, // TRUE = designated component or operation scheme available
			std::string const & NodeNameIn, // Component inlet node name
			std::string const & NodeNameOut, // Component outlet node name
			int const NodeNumIn, // Component inlet node number
			int const NodeNumOut, // Component outlet node number
			Real64 const MyLoad, // Distributed Load
			Real64 const MaxLoad, // Maximum load
			Real64 const MinLoad, // Minimum Load
			Real64 const OptLoad, // Optimal Load
			Real64 const SizFac, // Sizing Fraction
			int const CurOpSchemeType, // updated pointer to
			int const NumOpSchemes, // number of schemes held in the pointer array
			int const CurCompLevelOpNum, // pointer to the OpScheme array defined next
			Array1< OpSchemePtrData > const & OpScheme, // Pointers to component on lists
			Real64 const EquipDemand, // Component load request based on inlet temp and outlet SP
			bool const EMSLoadOverrideOn, // EMS is calling to override load dispatched to component
			Real64 const EMSLoadOverrideValue, // EMS value to use for load when overridden [W] always positive.
			int const HowLoadServed, // nature of component in terms of how it can meet load
			Real64 const MinOutletTemp, // Component exit lower limit temperature
			Real64 const MaxOutletTemp, // Component exit upper limit temperature
			bool const FreeCoolCntrlShutDown, // true if component was shut down because of free cooling
			Real64 const FreeCoolCntrlMinCntrlTemp, // current control temp value for free cooling controls
			int const FreeCoolCntrlMode, // type of sensor used for free cooling controls
			int const FreeCoolCntrlNodeNum, // chiller condenser inlet node number for free cooling controls
			int const IndexInLoopSidePumps, // If I'm a pump, this tells my index in PL(:)%LS(:)%Pumps
			Real64 const TempDesCondIn,
			Real64 const TempDesEvapOut
		) :
			TypeOf( TypeOf ),
			TypeOf_Num( TypeOf_Num ),
			GeneralEquipType( GeneralEquipType ),
			Name( Name ),
			CompNum( CompNum ),
			FlowCtrl( FlowCtrl ),
			FlowPriority( FlowPriority ),
			ON( ON ),
			Available( Available ),
			NodeNameIn( NodeNameIn ),
			NodeNameOut( NodeNameOut ),
			NodeNumIn( NodeNumIn ),
			NodeNumOut( NodeNumOut ),
			MyLoad( MyLoad ),
			MaxLoad( MaxLoad ),
			MinLoad( MinLoad ),
			OptLoad( OptLoad ),
			SizFac( SizFac ),
			CurOpSchemeType( CurOpSchemeType ),
			NumOpSchemes( NumOpSchemes ),
			CurCompLevelOpNum( CurCompLevelOpNum ),
			OpScheme( OpScheme ),
			EquipDemand( EquipDemand ),
			EMSLoadOverrideOn( EMSLoadOverrideOn ),
			EMSLoadOverrideValue( EMSLoadOverrideValue ),
			HowLoadServed( HowLoadServed ),
			MinOutletTemp( MinOutletTemp ),
			MaxOutletTemp( MaxOutletTemp ),
			FreeCoolCntrlShutDown( FreeCoolCntrlShutDown ),
			FreeCoolCntrlMinCntrlTemp( FreeCoolCntrlMinCntrlTemp ),
			FreeCoolCntrlMode( FreeCoolCntrlMode ),
			FreeCoolCntrlNodeNum( FreeCoolCntrlNodeNum ),
			IndexInLoopSidePumps( IndexInLoopSidePumps ),
			TempDesCondIn( TempDesCondIn ),
			TempDesEvapOut( TempDesEvapOut )
		{}

	};

	struct BranchData
	{
		// Members
		std::string Name; // Name of the branch
		int ControlType;
		Real64 MinVolFlowRate;
		Real64 MaxVolFlowRate;
		Real64 RequestedMassFlow;
		bool HasConstantSpeedBranchPump; // true if branch has a constant speed branch pump
		Real64 ConstantSpeedBranchMassFlow; // nominal flow rate if constant speed branch pump on
		int BranchLevel;
		int FlowErrCount; // For recurring error counting
		int FlowErrIndex; // For recurring error index
		int TotalComponents; // Total number of components on the branch
		int NodeNumIn; // Component inlet node number
		int NodeNumOut; // Component outlet node number
		bool IsBypass;
		int PumpIndex;
		Real64 PumpSizFac;
		bool EMSCtrlOverrideOn; // if true, EMS is calling to override branch operation avail
		Real64 EMSCtrlOverrideValue; // value set by EMS system for branch override controls
		Array1D< CompData > Comp; // Component type list
		bool HasPressureComponents;
		Real64 PressureDrop;
		int PressureCurveType; // Either none, pressure curve, or generic curve
		int PressureCurveIndex; // Curve: index for pressure drop calculations
		Real64 PressureEffectiveK;

		// Default Constructor
		BranchData() :
			ControlType( 0 ),
			MinVolFlowRate( 0.0 ),
			MaxVolFlowRate( 0.0 ),
			RequestedMassFlow( 0.0 ),
			HasConstantSpeedBranchPump( false ),
			ConstantSpeedBranchMassFlow( 0.0 ),
			BranchLevel( 0 ),
			FlowErrCount( 0 ),
			FlowErrIndex( 0 ),
			TotalComponents( 0 ),
			NodeNumIn( 0 ),
			NodeNumOut( 0 ),
			IsBypass( false ),
			PumpIndex( 0 ),
			PumpSizFac( 1.0 ),
			EMSCtrlOverrideOn( false ),
			EMSCtrlOverrideValue( 0.0 ),
			HasPressureComponents( false ),
			PressureDrop( 0.0 ),
			PressureCurveType( 0 ),
			PressureCurveIndex( 0 ),
			PressureEffectiveK( 0.0 )
		{}

		// Member Constructor
		BranchData(
			std::string const & Name, // Name of the branch
			int const ControlType,
			Real64 const MinVolFlowRate,
			Real64 const MaxVolFlowRate,
			Real64 const RequestedMassFlow,
			bool const HasConstantSpeedBranchPump, // true if branch has a constant speed branch pump
			Real64 const ConstantSpeedBranchMassFlow, // nominal flow rate if constant speed branch pump on
			int const BranchLevel,
			int const FlowErrCount, // For recurring error counting
			int const FlowErrIndex, // For recurring error index
			int const TotalComponents, // Total number of components on the branch
			int const NodeNumIn, // Component inlet node number
			int const NodeNumOut, // Component outlet node number
			bool const IsBypass,
			int const PumpIndex,
			Real64 const PumpSizFac,
			bool const EMSCtrlOverrideOn, // if true, EMS is calling to override branch operation avail
			Real64 const EMSCtrlOverrideValue, // value set by EMS system for branch override controls
			Array1< CompData > const & Comp, // Component type list
			bool const HasPressureComponents,
			Real64 const PressureDrop,
			int const PressureCurveType, // Either none, pressure curve, or generic curve
			int const PressureCurveIndex, // Curve: index for pressure drop calculations
			Real64 const PressureEffectiveK
		) :
			Name( Name ),
			ControlType( ControlType ),
			MinVolFlowRate( MinVolFlowRate ),
			MaxVolFlowRate( MaxVolFlowRate ),
			RequestedMassFlow( RequestedMassFlow ),
			HasConstantSpeedBranchPump( HasConstantSpeedBranchPump ),
			ConstantSpeedBranchMassFlow( ConstantSpeedBranchMassFlow ),
			BranchLevel( BranchLevel ),
			FlowErrCount( FlowErrCount ),
			FlowErrIndex( FlowErrIndex ),
			TotalComponents( TotalComponents ),
			NodeNumIn( NodeNumIn ),
			NodeNumOut( NodeNumOut ),
			IsBypass( IsBypass ),
			PumpIndex( PumpIndex ),
			PumpSizFac( PumpSizFac ),
			EMSCtrlOverrideOn( EMSCtrlOverrideOn ),
			EMSCtrlOverrideValue( EMSCtrlOverrideValue ),
			Comp( Comp ),
			HasPressureComponents( HasPressureComponents ),
			PressureDrop( PressureDrop ),
			PressureCurveType( PressureCurveType ),
			PressureCurveIndex( PressureCurveIndex ),
			PressureEffectiveK( PressureEffectiveK )
		{}

		// Max abs of Comp array MyLoad values //Autodesk:Tuned For replacement of any( abs( Comp.MyLoad() > SmallLoad ) usage
		Real64
		max_abs_Comp_MyLoad() const
		{
			Real64 load( 0.0 ); // Return value
			for ( int i = Comp.l(), e = Comp.u(); i <= e; ++i ) {
				load = max( load, abs( Comp( i ).MyLoad ) );
			}
			return load;
		}

	};

	struct EquipListCompData
	{
		// Members
		std::string Name; // The name of each item in the list
		std::string TypeOf; // The name of each item in the list
		int TypeOf_Num;
		std::string CtrlType; // CoolingOp, HeatingOp, DualOp
		int CtrlTypeNum; // CoolingOp, HeatingOp, DualOp
		int LoopNumPtr; // pointer to the comp location in the data structure
		int LoopSideNumPtr; // pointer to the comp location in the data structure
		int BranchNumPtr; // pointer to the comp location in the data structure
		int CompNumPtr; // pointer to the comp location in the data structure
		Real64 SetPointFlowRate; // COMP SETPOINT CTRL ONLY--load calculation comp flow rate
		std::string DemandNodeName; // COMP SETPOINT CTRL ONLY--The name of each item in the list
		int DemandNodeNum; // COMP SETPOINT CTRL ONLY--The 'keyWord' identifying each item in list
		std::string SetPointNodeName; // COMP SETPOINT CTRL ONLY--The name of each item in the list
		int SetPointNodeNum; // COMP SETPOINT CTRL ONLY--The 'keyWord' identifying each item in list
		Real64 EMSIntVarRemainingLoadValue; // EMS internal variable remaining load, neg cooling [W]
		Real64 EMSActuatorDispatchedLoadValue; // EMS actuator for dispatched load, neg= cooling [W]

		// Default Constructor
		EquipListCompData() :
			TypeOf_Num( 0 ),
			SetPointFlowRate( 0.0 ),
			EMSIntVarRemainingLoadValue( 0.0 ),
			EMSActuatorDispatchedLoadValue( 0.0 )
		{}

		// Member Constructor
		EquipListCompData(
			std::string const & Name, // The name of each item in the list
			std::string const & TypeOf, // The name of each item in the list
			int const TypeOf_Num,
			std::string const & CtrlType, // CoolingOp, HeatingOp, DualOp
			int const CtrlTypeNum, // CoolingOp, HeatingOp, DualOp
			int const LoopNumPtr, // pointer to the comp location in the data structure
			int const LoopSideNumPtr, // pointer to the comp location in the data structure
			int const BranchNumPtr, // pointer to the comp location in the data structure
			int const CompNumPtr, // pointer to the comp location in the data structure
			Real64 const SetPointFlowRate, // COMP SETPOINT CTRL ONLY--load calculation comp flow rate
			std::string const & DemandNodeName, // COMP SETPOINT CTRL ONLY--The name of each item in the list
			int const DemandNodeNum, // COMP SETPOINT CTRL ONLY--The 'keyWord' identifying each item in list
			std::string const & SetPointNodeName, // COMP SETPOINT CTRL ONLY--The name of each item in the list
			int const SetPointNodeNum, // COMP SETPOINT CTRL ONLY--The 'keyWord' identifying each item in list
			Real64 const EMSIntVarRemainingLoadValue, // EMS internal variable remaining load, neg cooling [W]
			Real64 const EMSActuatorDispatchedLoadValue // EMS actuator for dispatched load, neg= cooling [W]
		) :
			Name( Name ),
			TypeOf( TypeOf ),
			TypeOf_Num( TypeOf_Num ),
			CtrlType( CtrlType ),
			CtrlTypeNum( CtrlTypeNum ),
			LoopNumPtr( LoopNumPtr ),
			LoopSideNumPtr( LoopSideNumPtr ),
			BranchNumPtr( BranchNumPtr ),
			CompNumPtr( CompNumPtr ),
			SetPointFlowRate( SetPointFlowRate ),
			DemandNodeName( DemandNodeName ),
			DemandNodeNum( DemandNodeNum ),
			SetPointNodeName( SetPointNodeName ),
			SetPointNodeNum( SetPointNodeNum ),
			EMSIntVarRemainingLoadValue( EMSIntVarRemainingLoadValue ),
			EMSActuatorDispatchedLoadValue( EMSActuatorDispatchedLoadValue )
		{}

	};

	struct EquipOpList // DSU
	{
		// Members
		std::string Name; // The name of each item in the list
		Real64 RangeUpperLimit; // for range based controls
		Real64 RangeLowerLimit; // for range based controls
		int NumComps; // ALLOCATABLE to the schedule (for valid schedules)
		Array1D< EquipListCompData > Comp; // Component type list

		// Default Constructor
		EquipOpList() :
			RangeUpperLimit( 0.0 ),
			RangeLowerLimit( 0.0 ),
			NumComps( 0 )
		{}

		// Member Constructor
		EquipOpList(
			std::string const & Name, // The name of each item in the list
			Real64 const RangeUpperLimit, // for range based controls
			Real64 const RangeLowerLimit, // for range based controls
			int const NumComps, // ALLOCATABLE to the schedule (for valid schedules)
			Array1< EquipListCompData > const & Comp // Component type list
		) :
			Name( Name ),
			RangeUpperLimit( RangeUpperLimit ),
			RangeLowerLimit( RangeLowerLimit ),
			NumComps( NumComps ),
			Comp( Comp )
		{}

	};

	struct OperationData // DSU
	{
		// Members
		std::string Name; // The name of each item in the list
		std::string TypeOf; // The 'keyWord' identifying each item in the list
		int OpSchemeType; // Op scheme type (from keyword)
		std::string Sched; // The name of the schedule associated with the list
		int SchedPtr; // ALLOCATABLE to the schedule (for valid schedules)
		bool Available; // TRUE = designated component or operation scheme available
		int NumEquipLists; // number of equipment lists
		int CurListPtr; // points to the current equipment list
		Array1D< EquipOpList > EquipList; // Component type list
		int EquipListNumForLastStage; // points to the equipment list with the highest upper limit
		std::string ReferenceNodeName; // DELTA CTRL ONLY--for calculation of delta Temp
		int ReferenceNodeNumber; // DELTA CTRL ONLY--for calculation of delta Temp
		int ErlSimProgramMngr; // EMS:ProgramManager to always run when this model is called
		int ErlInitProgramMngr; // EMS:ProgramManager to run when this model is initialized and setup
		Real64 EMSIntVarLoopDemandRate; // EMS internal variable for loop-level demand rate, neg cooling [W]
		bool MyEnvrnFlag;

		// Default Constructor
		OperationData() :
			OpSchemeType( 0 ),
			SchedPtr( 0 ),
			Available( false ),
			NumEquipLists( 0 ),
			CurListPtr( 0 ),
			EquipListNumForLastStage( 0 ),
			ErlSimProgramMngr( 0 ),
			ErlInitProgramMngr( 0 ),
			EMSIntVarLoopDemandRate( 0.0 ),
			MyEnvrnFlag( true )
		{}

		// Member Constructor
		OperationData(
			std::string const & Name, // The name of each item in the list
			std::string const & TypeOf, // The 'keyWord' identifying each item in the list
			int const OpSchemeType, // Op scheme type (from keyword)
			std::string const & Sched, // The name of the schedule associated with the list
			int const SchedPtr, // ALLOCATABLE to the schedule (for valid schedules)
			bool const Available, // TRUE = designated component or operation scheme available
			int const NumEquipLists, // number of equipment lists
			int const CurListPtr, // points to the current equipment list
			Array1< EquipOpList > const & EquipList, // Component type list
			int const EquipListNumForLastStage, // points to the equipment list with the highest upper limit
			std::string const & ReferenceNodeName, // DELTA CTRL ONLY--for calculation of delta Temp
			int const ReferenceNodeNumber, // DELTA CTRL ONLY--for calculation of delta Temp
			int const ErlSimProgramMngr, // EMS:ProgramManager to always run when this model is called
			int const ErlInitProgramMngr, // EMS:ProgramManager to run when this model is initialized and setup
			Real64 const EMSIntVarLoopDemandRate, // EMS internal variable for loop-level demand rate, neg cooling [W]
			bool const MyEnvrnFlag
		) :
			Name( Name ),
			TypeOf( TypeOf ),
			OpSchemeType( OpSchemeType ),
			Sched( Sched ),
			SchedPtr( SchedPtr ),
			Available( Available ),
			NumEquipLists( NumEquipLists ),
			CurListPtr( CurListPtr ),
			EquipList( EquipList ),
			EquipListNumForLastStage( EquipListNumForLastStage ),
			ReferenceNodeName( ReferenceNodeName ),
			ReferenceNodeNumber( ReferenceNodeNumber ),
			ErlSimProgramMngr( ErlSimProgramMngr ),
			ErlInitProgramMngr( ErlInitProgramMngr ),
			EMSIntVarLoopDemandRate( EMSIntVarLoopDemandRate ),
			MyEnvrnFlag( MyEnvrnFlag )
		{}

	};

	struct ConnectedLoopData // DSU
	{
		// Members
		int LoopNum; // plant loop index pointer, for the other loop
		int LoopSideNum; // plant loop side number, for the other loop
		int ConnectorTypeOf_Num; // plant equipment type doing the connecting
		bool LoopDemandsOnRemote; // true if this loop puts demand on connected loop

		// Default Constructor
		ConnectedLoopData() :
			LoopNum( 0 ),
			LoopSideNum( 0 ),
			ConnectorTypeOf_Num( 0 ),
			LoopDemandsOnRemote( false )
		{}

		// Member Constructor
		ConnectedLoopData(
			int const LoopNum, // plant loop index pointer, for the other loop
			int const LoopSideNum, // plant loop side number, for the other loop
			int const ConnectorTypeOf_Num, // plant equipment type doing the connecting
			bool const LoopDemandsOnRemote // true if this loop puts demand on connected loop
		) :
			LoopNum( LoopNum ),
			LoopSideNum( LoopSideNum ),
			ConnectorTypeOf_Num( ConnectorTypeOf_Num ),
			LoopDemandsOnRemote( LoopDemandsOnRemote )
		{}

	};

	struct PlantLocation
	{
		// Members
		int LoopNum;
		int LoopSideNum;
		int BranchNum;
		int CompNum;

		// Default Constructor
		PlantLocation()
		{}

		// Member Constructor
		PlantLocation(
			int const LoopNum,
			int const LoopSideNum,
			int const BranchNum,
			int const CompNum
		) :
			LoopNum( LoopNum ),
			LoopSideNum( LoopSideNum ),
			BranchNum( BranchNum ),
			CompNum( CompNum )
		{}

	};

	struct PlantConvergencePoint
	{
		// Members
		Array1D< Real64 > MassFlowRateHistory;
		Array1D< Real64 > TemperatureHistory;

		// Default Constructor
		PlantConvergencePoint() :
			MassFlowRateHistory( NumConvergenceHistoryTerms ),
			TemperatureHistory( NumConvergenceHistoryTerms )
		{}

		// Member Constructor
		PlantConvergencePoint(
			Array1< Real64 > const & MassFlowRateHistory,
			Array1< Real64 > const & TemperatureHistory
		) :
			MassFlowRateHistory( NumConvergenceHistoryTerms, MassFlowRateHistory ),
			TemperatureHistory( NumConvergenceHistoryTerms, TemperatureHistory )
		{}

		// Scalar Constructor
		PlantConvergencePoint(
			Real64 const MassFlowRateValue,
			Real64 const TemperatureValue
		) :
			MassFlowRateHistory( NumConvergenceHistoryTerms, MassFlowRateValue ),
			TemperatureHistory( NumConvergenceHistoryTerms, TemperatureValue )
		{}

	};

	struct HalfLoopData
	{
		// Members
		bool SimLoopSideNeeded; // Determine whether or not to re-simulate this plant LoopSide
		bool SimZoneEquipNeeded; // Plant requests resimulate zone HVAC equipment
		bool SimAirLoopsNeeded; // Plant requests resimulate air loop HVAC equipment
		bool SimNonZoneEquipNeeded; // Plant requests resimulate non zone Equip
		bool SimElectLoadCentrNeeded; // Plant requests resimulate generators
		bool OncePerTimeStepOperations;
		Real64 TimeElapsed; // store time for dynamic updates for last time
		Real64 FlowRequest; // Flow request in the half loop
		Real64 FlowRequestTemperature; // Average Flow request outlet Temp in the half loop
		// It's necessary to hold the values here since AIR and GROUND SPs aren't associated with either a node or a SP manager
		Real64 TempSetPoint; // Loop temperature setpoint
		Real64 TempSetPointHi; // High Loop temperature setpoint
		Real64 TempSetPointLo; // Low Loop temperature setpoint
		Real64 TempInterfaceTankOutlet; // Used by interface manager in common pipe simulation
		// This is the temperature at the loop outlet linterface
		// with half-loop capacitance and pump heat accounted for.
		Real64 LastTempInterfaceTankOutlet;
		std::string BranchList; // Branch list name for the half loop
		std::string ConnectList; // Connector list name for the half loop
		int TotalBranches; // Total number of branches on the half loop
		int NodeNumIn; // Node number for the inlet to this loop
		std::string NodeNameIn; // Node name for the inlet to this loop
		int NodeNumOut; // Node number for the outlet to this loop
		std::string NodeNameOut; // Node name for the outlet to this loop
		int NumSplitters; // Number of splitters in the half loop
		int NumMixers; // Number of mixers in the half loop
		bool SplitterExists; // Logical Flag indication splitter exists in the half loop
		bool MixerExists; // Logical Flag indication mixer exists in the half loop
		int TotalPumps; // total number of pumps on the half loop
		bool BranchPumpsExist; // logical flag indication branch pumps exist on half loop
		Array1D< LoopSidePumpInformation > Pumps;
		Real64 TotalPumpHeat; // [W] total heat addition by the pumps to place in "tank"
		bool BypassExists;
		bool InletNodeSetPt;
		bool OutletNodeSetPt;
		bool EMSCtrl;
		Real64 EMSValue;
		bool FlowRestrictionFlag; // Max available flow at the outlet of the half loop
		// is less than max available flow at inlet
		int FlowLock; // DSU
		int TotalConnected; // total number of other loops connected to this loop side
		Array1D< ConnectedLoopData > Connected; // DSU Other loops connected to this Loop side
		Array1D< BranchData > Branch; // Branch data
		Array1D< SplitterData > Splitter; // Data for splitter on branch (if any)
		Array1D< MixerData > Mixer; // Data for splitter on branch (if any)
		bool HasPressureComponents;
		bool HasParallelPressComps;
		Real64 PressureDrop;
		Real64 PressureEffectiveK;
		int errCount_LoadWasntDist;
		int errIndex_LoadWasntDist;
		int errCount_LoadRemains;
		int errIndex_LoadRemains;
		Real64 LoopSideInlet_TankTemp;
		PlantConvergencePoint InletNode;
		PlantConvergencePoint OutletNode;

		// Default Constructor
		HalfLoopData() :
			SimLoopSideNeeded( true ),
			SimZoneEquipNeeded( true ),
			SimAirLoopsNeeded( true ),
			SimNonZoneEquipNeeded( true ),
			SimElectLoadCentrNeeded( true ),
			OncePerTimeStepOperations( true ),
			TimeElapsed( 0.0 ),
			FlowRequest( 0.0 ),
			FlowRequestTemperature( 0.0 ),
			TempSetPoint( SensedNodeFlagValue ),
			TempSetPointHi( SensedNodeFlagValue ),
			TempSetPointLo( SensedNodeFlagValue ),
			TempInterfaceTankOutlet( 0.0 ),
			LastTempInterfaceTankOutlet( 0.0 ),
			TotalBranches( 0 ),
			NodeNumIn( 0 ),
			NodeNumOut( 0 ),
			NumSplitters( 0 ),
			NumMixers( 0 ),
			SplitterExists( false ),
			MixerExists( false ),
			TotalPumps( 0 ),
			BranchPumpsExist( false ),
			TotalPumpHeat( 0.0 ),
			BypassExists( false ),
			InletNodeSetPt( false ),
			OutletNodeSetPt( false ),
			EMSCtrl( false ),
			FlowRestrictionFlag( false ),
			FlowLock( 0 ),
			TotalConnected( 0 ),
			HasPressureComponents( false ),
			HasParallelPressComps( false ),
			PressureDrop( 0.0 ),
			PressureEffectiveK( 0.0 ),
			errCount_LoadWasntDist( 0 ),
			errIndex_LoadWasntDist( 0 ),
			errCount_LoadRemains( 0 ),
			errIndex_LoadRemains( 0 ),
			LoopSideInlet_TankTemp( 0.0 ),
			InletNode( 0.0, 0.0 ),
			OutletNode( 0.0, 0.0 )
		{}

		// Member Constructor
		HalfLoopData(
			bool const SimLoopSideNeeded, // Determine whether or not to re-simulate this plant LoopSide
			bool const SimZoneEquipNeeded, // Plant requests resimulate zone HVAC equipment
			bool const SimAirLoopsNeeded, // Plant requests resimulate air loop HVAC equipment
			bool const SimNonZoneEquipNeeded, // Plant requests resimulate non zone Equip
			bool const SimElectLoadCentrNeeded, // Plant requests resimulate generators
			bool const OncePerTimeStepOperations,
			Real64 const TimeElapsed, // store time for dynamic updates for last time
			Real64 const FlowRequest, // Flow request in the half loop
			Real64 const FlowRequestTemperature, // Average Flow request outlet Temp in the half loop
			Real64 const TempSetPoint, // Loop temperature setpoint
			Real64 const TempSetPointHi, // High Loop temperature setpoint
			Real64 const TempSetPointLo, // Low Loop temperature setpoint
			Real64 const TempInterfaceTankOutlet, // Used by interface manager in common pipe simulation
			Real64 const LastTempInterfaceTankOutlet,
			std::string const & BranchList, // Branch list name for the half loop
			std::string const & ConnectList, // Connector list name for the half loop
			int const TotalBranches, // Total number of branches on the half loop
			int const NodeNumIn, // Node number for the inlet to this loop
			std::string const & NodeNameIn, // Node name for the inlet to this loop
			int const NodeNumOut, // Node number for the outlet to this loop
			std::string const & NodeNameOut, // Node name for the outlet to this loop
			int const NumSplitters, // Number of splitters in the half loop
			int const NumMixers, // Number of mixers in the half loop
			bool const SplitterExists, // Logical Flag indication splitter exists in the half loop
			bool const MixerExists, // Logical Flag indication mixer exists in the half loop
			int const TotalPumps, // total number of pumps on the half loop
			bool const BranchPumpsExist, // logical flag indication branch pumps exist on half loop
			Array1< LoopSidePumpInformation > const & Pumps,
			Real64 const TotalPumpHeat, // [W] total heat addition by the pumps to place in "tank"
			bool const BypassExists,
			bool const InletNodeSetPt,
			bool const OutletNodeSetPt,
			bool const EMSCtrl,
			Real64 const EMSValue,
			bool const FlowRestrictionFlag, // Max available flow at the outlet of the half loop
			int const FlowLock, // DSU
			int const TotalConnected, // total number of other loops connected to this loop side
			Array1< ConnectedLoopData > const & Connected, // DSU Other loops connected to this Loop side
			Array1< BranchData > const & Branch, // Branch data
			Array1< SplitterData > const & Splitter, // Data for splitter on branch (if any)
			Array1< MixerData > const & Mixer, // Data for splitter on branch (if any)
			bool const HasPressureComponents,
			bool const HasParallelPressComps,
			Real64 const PressureDrop,
			Real64 const PressureEffectiveK,
			int const errCount_LoadWasntDist,
			int const errIndex_LoadWasntDist,
			int const errCount_LoadRemains,
			int const errIndex_LoadRemains,
			Real64 const LoopSideInlet_TankTemp,
			PlantConvergencePoint const & InletNode,
			PlantConvergencePoint const & OutletNode
		) :
			SimLoopSideNeeded( SimLoopSideNeeded ),
			SimZoneEquipNeeded( SimZoneEquipNeeded ),
			SimAirLoopsNeeded( SimAirLoopsNeeded ),
			SimNonZoneEquipNeeded( SimNonZoneEquipNeeded ),
			SimElectLoadCentrNeeded( SimElectLoadCentrNeeded ),
			OncePerTimeStepOperations( OncePerTimeStepOperations ),
			TimeElapsed( TimeElapsed ),
			FlowRequest( FlowRequest ),
			FlowRequestTemperature( FlowRequestTemperature ),
			TempSetPoint( TempSetPoint ),
			TempSetPointHi( TempSetPointHi ),
			TempSetPointLo( TempSetPointLo ),
			TempInterfaceTankOutlet( TempInterfaceTankOutlet ),
			LastTempInterfaceTankOutlet( LastTempInterfaceTankOutlet ),
			BranchList( BranchList ),
			ConnectList( ConnectList ),
			TotalBranches( TotalBranches ),
			NodeNumIn( NodeNumIn ),
			NodeNameIn( NodeNameIn ),
			NodeNumOut( NodeNumOut ),
			NodeNameOut( NodeNameOut ),
			NumSplitters( NumSplitters ),
			NumMixers( NumMixers ),
			SplitterExists( SplitterExists ),
			MixerExists( MixerExists ),
			TotalPumps( TotalPumps ),
			BranchPumpsExist( BranchPumpsExist ),
			Pumps( Pumps ),
			TotalPumpHeat( TotalPumpHeat ),
			BypassExists( BypassExists ),
			InletNodeSetPt( InletNodeSetPt ),
			OutletNodeSetPt( OutletNodeSetPt ),
			EMSCtrl( EMSCtrl ),
			EMSValue( EMSValue ),
			FlowRestrictionFlag( FlowRestrictionFlag ),
			FlowLock( FlowLock ),
			TotalConnected( TotalConnected ),
			Connected( Connected ),
			Branch( Branch ),
			Splitter( Splitter ),
			Mixer( Mixer ),
			HasPressureComponents( HasPressureComponents ),
			HasParallelPressComps( HasParallelPressComps ),
			PressureDrop( PressureDrop ),
			PressureEffectiveK( PressureEffectiveK ),
			errCount_LoadWasntDist( errCount_LoadWasntDist ),
			errIndex_LoadWasntDist( errIndex_LoadWasntDist ),
			errCount_LoadRemains( errCount_LoadRemains ),
			errIndex_LoadRemains( errIndex_LoadRemains ),
			LoopSideInlet_TankTemp( LoopSideInlet_TankTemp ),
			InletNode( InletNode ),
			OutletNode( OutletNode )
		{}

	};

	struct PlantLoopData
	{
		// Members
		std::string Name; // Name of the component list
		std::string FluidName; // Name of the fluid specified for this loop
		int FluidType; // Type of fluid in the loop
		int FluidIndex; // Index for Fluid in FluidProperties
		int MFErrIndex; // for recurring mass flow errors
		int MFErrIndex1; // for recurring mass flow errors
		int MFErrIndex2; // for recurring mass flow errors
		// (see CheckPlantMixerSplitterConsistency)
		// Loop Operating Setpoints and Limits
		int TempSetPointNodeNum; // Node Number for Loop Temp SP associated with SP manager
		int MaxBranch; // Max branches in the loop
		Real64 MinTemp; // Minimum temperature allowed in the loop
		Real64 MaxTemp; // Maximum temperature allowed in the loop
		int MinTempErrIndex; // for recurring too cold errors
		int MaxTempErrIndex; // for recurring too hot errors
		Real64 MinVolFlowRate; // Minimum flow rate allowed in the loop
		Real64 MaxVolFlowRate; // Maximum flow rate allowed in the loop
		bool MaxVolFlowRateWasAutoSized; // true if previous was set to autosized in the input
		Real64 MinMassFlowRate; // Minimum flow rate allowed in the loop
		Real64 MaxMassFlowRate; // Maximum flow rate allowed in the loop
		Real64 Volume; // Volume of the fluid in the loop
		bool VolumeWasAutoSized; //true if Volume was set to autocalculate
		Real64 Mass; // Mass of the fluid in the loop
		bool EMSCtrl;
		Real64 EMSValue;
		// Loop Inlet and Outlet Nodes
		Array1D< HalfLoopData > LoopSide; // Half loop data (Demand side or Supply Side)
		std::string OperationScheme; // Operation scheme name for the loop
		int NumOpSchemes; // Number of items in list identified by "OpScheme"
		Array1D< OperationData > OpScheme; // Operation scheme data
		int LoadDistribution; // Load distribution scheme 1 for optimal, 2 for overloading
		int PlantSizNum; // index to corresponding plant sizing data array
		int LoopDemandCalcScheme; // Load distribution scheme 1 SingleSetPoint,
		// 2 DualSetPointwithDeadBand
		int CommonPipeType;
		//  TYPE(TwoWayCommonPipeData), &
		//       ALLOCATABLE                 :: TwoWayCommonPipe                ! two-way common pipe data, primary secondary loop model
		std::string EconomizerHtExchanger; // DSU review, should move these out of here
		std::string EconPlantSideSensedNodeName; // DSU review, should move these out of here
		std::string EconCondSideSensedNodeName; // DSU review, should move these out of here
		int EconPlantSideSensedNodeNum; // DSU review, should move these out of here
		int EconCondSideSensedNodeNum; // DSU review, should move these out of here
		int EconPlacement; // DSU review, should move these out of here
		int EconBranch; // DSU review, should move these out of here
		int EconComp; // DSU review, should move these out of here
		Real64 EconControlTempDiff; // DSU review, should move these out of here
		bool LoopHasConnectionComp;
		int TypeOfLoop;
		int PressureSimType;
		bool HasPressureComponents;
		Real64 PressureDrop;
		bool UsePressureForPumpCalcs;
		Real64 PressureEffectiveK;

		// Default Constructor
		PlantLoopData() :
			FluidType( 0 ),
			FluidIndex( 0 ),
			MFErrIndex( 0 ),
			MFErrIndex1( 0 ),
			MFErrIndex2( 0 ),
			TempSetPointNodeNum( 0 ),
			MaxBranch( 0 ),
			MinTemp( 0.0 ),
			MaxTemp( 0.0 ),
			MinTempErrIndex( 0 ),
			MaxTempErrIndex( 0 ),
			MinVolFlowRate( 0.0 ),
			MaxVolFlowRate( 0.0 ),
			MaxVolFlowRateWasAutoSized( false ),
			MinMassFlowRate( 0.0 ),
			MaxMassFlowRate( 0.0 ),
			Volume( 0.0 ),
			VolumeWasAutoSized ( false ), //true if Volume was set to autocalculate
			Mass( 0.0 ),
			EMSCtrl( false ),
			EMSValue( 0.0 ),
			NumOpSchemes( 0 ),
			LoadDistribution( 0 ),
			PlantSizNum( 0 ),
			LoopDemandCalcScheme( 0 ),
			CommonPipeType( 0 ),
			EconPlantSideSensedNodeNum( 0 ),
			EconCondSideSensedNodeNum( 0 ),
			EconPlacement( 0 ),
			EconBranch( 0 ),
			EconComp( 0 ),
			EconControlTempDiff( 0.0 ),
			LoopHasConnectionComp( false ),
			TypeOfLoop( 0 ),
			PressureSimType( 1 ),
			HasPressureComponents( false ),
			PressureDrop( 0.0 ),
			UsePressureForPumpCalcs( false ),
			PressureEffectiveK( 0.0 )
		{}

		// Member Constructor
		PlantLoopData(
			std::string const & Name, // Name of the component list
			std::string const & FluidName, // Name of the fluid specified for this loop
			int const FluidType, // Type of fluid in the loop
			int const FluidIndex, // Index for Fluid in FluidProperties
			int const MFErrIndex, // for recurring mass flow errors
			int const MFErrIndex1, // for recurring mass flow errors
			int const MFErrIndex2, // for recurring mass flow errors
			int const TempSetPointNodeNum, // Node Number for Loop Temp SP associated with SP manager
			int const MaxBranch, // Max branches in the loop
			Real64 const MinTemp, // Minimum temperature allowed in the loop
			Real64 const MaxTemp, // Maximum temperature allowed in the loop
			int const MinTempErrIndex, // for recurring too cold errors
			int const MaxTempErrIndex, // for recurring too hot errors
			Real64 const MinVolFlowRate, // Minimum flow rate allowed in the loop
			Real64 const MaxVolFlowRate, // Maximum flow rate allowed in the loop
			bool const MaxVolFlowRateWasAutoSized, // true if previous was set to autosized in the input
			Real64 const MinMassFlowRate, // Minimum flow rate allowed in the loop
			Real64 const MaxMassFlowRate, // Maximum flow rate allowed in the loop
			Real64 const Volume, // Volume of the fluid in the loop
			bool const VolumeWasAutoSized, // true if previous was set to autocal
			Real64 const Mass, // Mass of the fluid in the loop
			bool const EMSCtrl,
			Real64 const EMSValue,
			Array1< HalfLoopData > const & LoopSide, // Half loop data (Demand side or Supply Side)
			std::string const & OperationScheme, // Operation scheme name for the loop
			int const NumOpSchemes, // Number of items in list identified by "OpScheme"
			Array1< OperationData > const & OpScheme, // Operation scheme data
			int const LoadDistribution, // Load distribution scheme 1 for optimal, 2 for overloading
			int const PlantSizNum, // index to corresponding plant sizing data array
			int const LoopDemandCalcScheme, // Load distribution scheme 1 SingleSetPoint,
			int const CommonPipeType,
			std::string const & EconomizerHtExchanger, // DSU review, should move these out of here
			std::string const & EconPlantSideSensedNodeName, // DSU review, should move these out of here
			std::string const & EconCondSideSensedNodeName, // DSU review, should move these out of here
			int const EconPlantSideSensedNodeNum, // DSU review, should move these out of here
			int const EconCondSideSensedNodeNum, // DSU review, should move these out of here
			int const EconPlacement, // DSU review, should move these out of here
			int const EconBranch, // DSU review, should move these out of here
			int const EconComp, // DSU review, should move these out of here
			Real64 const EconControlTempDiff, // DSU review, should move these out of here
			bool const LoopHasConnectionComp,
			int const TypeOfLoop,
			int const PressureSimType,
			bool const HasPressureComponents,
			Real64 const PressureDrop,
			bool const UsePressureForPumpCalcs,
			Real64 const PressureEffectiveK
		) :
			Name( Name ),
			FluidName( FluidName ),
			FluidType( FluidType ),
			FluidIndex( FluidIndex ),
			MFErrIndex( MFErrIndex ),
			MFErrIndex1( MFErrIndex1 ),
			MFErrIndex2( MFErrIndex2 ),
			TempSetPointNodeNum( TempSetPointNodeNum ),
			MaxBranch( MaxBranch ),
			MinTemp( MinTemp ),
			MaxTemp( MaxTemp ),
			MinTempErrIndex( MinTempErrIndex ),
			MaxTempErrIndex( MaxTempErrIndex ),
			MinVolFlowRate( MinVolFlowRate ),
			MaxVolFlowRate( MaxVolFlowRate ),
			MaxVolFlowRateWasAutoSized( MaxVolFlowRateWasAutoSized ),
			MinMassFlowRate( MinMassFlowRate ),
			MaxMassFlowRate( MaxMassFlowRate ),
			Volume( Volume ),
			VolumeWasAutoSized( VolumeWasAutoSized ),
			Mass( Mass ),
			EMSCtrl( EMSCtrl ),
			EMSValue( EMSValue ),
			LoopSide( LoopSide ),
			OperationScheme( OperationScheme ),
			NumOpSchemes( NumOpSchemes ),
			OpScheme( OpScheme ),
			LoadDistribution( LoadDistribution ),
			PlantSizNum( PlantSizNum ),
			LoopDemandCalcScheme( LoopDemandCalcScheme ),
			CommonPipeType( CommonPipeType ),
			EconomizerHtExchanger( EconomizerHtExchanger ),
			EconPlantSideSensedNodeName( EconPlantSideSensedNodeName ),
			EconCondSideSensedNodeName( EconCondSideSensedNodeName ),
			EconPlantSideSensedNodeNum( EconPlantSideSensedNodeNum ),
			EconCondSideSensedNodeNum( EconCondSideSensedNodeNum ),
			EconPlacement( EconPlacement ),
			EconBranch( EconBranch ),
			EconComp( EconComp ),
			EconControlTempDiff( EconControlTempDiff ),
			LoopHasConnectionComp( LoopHasConnectionComp ),
			TypeOfLoop( TypeOfLoop ),
			PressureSimType( PressureSimType ),
			HasPressureComponents( HasPressureComponents ),
			PressureDrop( PressureDrop ),
			UsePressureForPumpCalcs( UsePressureForPumpCalcs ),
			PressureEffectiveK( PressureEffectiveK )
		{}

	};

	struct ComponentData
	{
		// Members
		std::string Name; // Name of the component
		std::string NodeNameIn; // Name of Node In
		std::string NodeNameOut; // Name of Node Out
		int NodeNumIn; // Inlet node number
		int NodeNumOut; // Outlet node number

		// Default Constructor
		ComponentData() :
			NodeNumIn( 0 ),
			NodeNumOut( 0 )
		{}

		// Member Constructor
		ComponentData(
			std::string const & Name, // Name of the component
			std::string const & NodeNameIn, // Name of Node In
			std::string const & NodeNameOut, // Name of Node Out
			int const NodeNumIn, // Inlet node number
			int const NodeNumOut // Outlet node number
		) :
			Name( Name ),
			NodeNameIn( NodeNameIn ),
			NodeNameOut( NodeNameOut ),
			NodeNumIn( NodeNumIn ),
			NodeNumOut( NodeNumOut )
		{}

	};

	struct PipeData
	{
		// Members
		std::string Name; // Pipe name
		int TypeOf;
		int EquipNum;
		int FlowCtrl; // Pipe control (should always be BYPASS)
		std::string NodeNameIn; // Pipe inlet node name
		int NodeNumIn; // Pipe inlet node number
		std::string NodeNameOut; // Pipe outlet node name
		int NodeNumOut; // Pipe outlet node number
		int ParentHalfLoop; // Half loop where the pipe is present

		// Default Constructor
		PipeData() :
			TypeOf( 0 ),
			EquipNum( 0 ),
			FlowCtrl( 0 ),
			NodeNumIn( 0 ),
			NodeNumOut( 0 ),
			ParentHalfLoop( 0 )
		{}

		// Member Constructor
		PipeData(
			std::string const & Name, // Pipe name
			int const TypeOf,
			int const EquipNum,
			int const FlowCtrl, // Pipe control (should always be BYPASS)
			std::string const & NodeNameIn, // Pipe inlet node name
			int const NodeNumIn, // Pipe inlet node number
			std::string const & NodeNameOut, // Pipe outlet node name
			int const NodeNumOut, // Pipe outlet node number
			int const ParentHalfLoop // Half loop where the pipe is present
		) :
			Name( Name ),
			TypeOf( TypeOf ),
			EquipNum( EquipNum ),
			FlowCtrl( FlowCtrl ),
			NodeNameIn( NodeNameIn ),
			NodeNumIn( NodeNumIn ),
			NodeNameOut( NodeNameOut ),
			NodeNumOut( NodeNumOut ),
			ParentHalfLoop( ParentHalfLoop )
		{}

	};

	struct PlantAvailMgrData
	{
		// Members
		int NumAvailManagers; // number of availability managers for this plant loop
		int AvailStatus; // system availability status
		int StartTime; // cycle on time (in SimTimeSteps)
		int StopTime; // cycle off time (in SimTimeSteps)
		Array1D_string AvailManagerName; // name of each availability manager
		Array1D_int AvailManagerType; // type of availability manager
		Array1D_int AvailManagerNum; // index of availability manager

		// Default Constructor
		PlantAvailMgrData() :
			NumAvailManagers( 0 ),
			AvailStatus( 0 ),
			StartTime( 0 ),
			StopTime( 0 )
		{}

		// Member Constructor
		PlantAvailMgrData(
			int const NumAvailManagers, // number of availability managers for this plant loop
			int const AvailStatus, // system availability status
			int const StartTime, // cycle on time (in SimTimeSteps)
			int const StopTime, // cycle off time (in SimTimeSteps)
			Array1_string const & AvailManagerName, // name of each availability manager
			Array1_int const & AvailManagerType, // type of availability manager
			Array1_int const & AvailManagerNum // index of availability manager
		) :
			NumAvailManagers( NumAvailManagers ),
			AvailStatus( AvailStatus ),
			StartTime( StartTime ),
			StopTime( StopTime ),
			AvailManagerName( AvailManagerName ),
			AvailManagerType( AvailManagerType ),
			AvailManagerNum( AvailManagerNum )
		{}

	};

	struct LoopSideReportVars
	{
		// Members
		Real64 LoopSetPtDemandAtInlet;
		Real64 ThisSideLoadAlterations;

		// Default Constructor
		LoopSideReportVars() :
			LoopSetPtDemandAtInlet( 0.0 ),
			ThisSideLoadAlterations( 0.0 )
		{}

		// Member Constructor
		LoopSideReportVars(
			Real64 const LoopSetPtDemandAtInlet,
			Real64 const ThisSideLoadAlterations
		) :
			LoopSetPtDemandAtInlet( LoopSetPtDemandAtInlet ),
			ThisSideLoadAlterations( ThisSideLoadAlterations )
		{}

	};

	struct ReportVars
	{
		// Members
		// Whole loop descriptions
		Real64 CoolingDemand; // Plant Loop Cooling Demand, W
		Real64 HeatingDemand; // Plant Loop Heating Demand[W]
		Real64 DemandNotDispatched; // Plant Loop Demand that was not distributed [W]
		Real64 UnmetDemand; // Plant Loop Unmet Demand [W]
		// Loop side data
		Array1D< LoopSideReportVars > LoopSide;
		Real64 BypassFrac; // Debug Variable
		Real64 InletNodeFlowrate; // Debug Variable
		Real64 InletNodeTemperature; // Debug Variable
		Real64 OutletNodeFlowrate; // Debug Variable
		Real64 OutletNodeTemperature; // Debug Variable
		int LastLoopSideSimulated;

		// Default Constructor
		ReportVars() :
			CoolingDemand( 0.0 ),
			HeatingDemand( 0.0 ),
			DemandNotDispatched( 0.0 ),
			UnmetDemand( 0.0 ),
			LoopSide( 2 ),
			BypassFrac( 0.0 ),
			InletNodeFlowrate( 0.0 ),
			InletNodeTemperature( 0.0 ),
			OutletNodeFlowrate( 0.0 ),
			OutletNodeTemperature( 0.0 ),
			LastLoopSideSimulated( 0 )
		{}

		// Member Constructor
		ReportVars(
			Real64 const CoolingDemand, // Plant Loop Cooling Demand, W
			Real64 const HeatingDemand, // Plant Loop Heating Demand[W]
			Real64 const DemandNotDispatched, // Plant Loop Demand that was not distributed [W]
			Real64 const UnmetDemand, // Plant Loop Unmet Demand [W]
			Array1< LoopSideReportVars > const & LoopSide,
			Real64 const BypassFrac, // Debug Variable
			Real64 const InletNodeFlowrate, // Debug Variable
			Real64 const InletNodeTemperature, // Debug Variable
			Real64 const OutletNodeFlowrate, // Debug Variable
			Real64 const OutletNodeTemperature, // Debug Variable
			int const LastLoopSideSimulated
		) :
			CoolingDemand( CoolingDemand ),
			HeatingDemand( HeatingDemand ),
			DemandNotDispatched( DemandNotDispatched ),
			UnmetDemand( UnmetDemand ),
			LoopSide( 2, LoopSide ),
			BypassFrac( BypassFrac ),
			InletNodeFlowrate( InletNodeFlowrate ),
			InletNodeTemperature( InletNodeTemperature ),
			OutletNodeFlowrate( OutletNodeFlowrate ),
			OutletNodeTemperature( OutletNodeTemperature ),
			LastLoopSideSimulated( LastLoopSideSimulated )
		{}

	};

	struct PlantConnection
	{
		// Members
		int LoopType; // 1 = Plant, 2 = Condenser
		int LoopNum;
		int BranchNum;
		int CompNum;

		// Default Constructor
		PlantConnection() :
			LoopType( 0 ),
			LoopNum( 0 ),
			BranchNum( 0 ),
			CompNum( 0 )
		{}

		// Member Constructor
		PlantConnection(
			int const LoopType, // 1 = Plant, 2 = Condenser
			int const LoopNum,
			int const BranchNum,
			int const CompNum
		) :
			LoopType( LoopType ),
			LoopNum( LoopNum ),
			BranchNum( BranchNum ),
			CompNum( CompNum )
		{}

	};

	struct MeterData
	{
		// Members
		std::string ReportVarName;
		std::string ReportVarUnits;
		int ResourceType;
		std::string EndUse;
		int EndUse_CompMode;
		std::string Group;
		int ReportVarIndex;
		int ReportVarIndexType;
		int ReportVarType;
		Real64 CurMeterReading;

		// Default Constructor
		MeterData() :
			ResourceType( 0 ),
			EndUse_CompMode( 0 ),
			ReportVarIndex( 0 ),
			ReportVarIndexType( 0 ),
			ReportVarType( 0 ),
			CurMeterReading( 0.0 )
		{}

		// Member Constructor
		MeterData(
			std::string const & ReportVarName,
			std::string const & ReportVarUnits,
			int const ResourceType,
			std::string const & EndUse,
			int const EndUse_CompMode,
			std::string const & Group,
			int const ReportVarIndex,
			int const ReportVarIndexType,
			int const ReportVarType,
			Real64 const CurMeterReading
		) :
			ReportVarName( ReportVarName ),
			ReportVarUnits( ReportVarUnits ),
			ResourceType( ResourceType ),
			EndUse( EndUse ),
			EndUse_CompMode( EndUse_CompMode ),
			Group( Group ),
			ReportVarIndex( ReportVarIndex ),
			ReportVarIndexType( ReportVarIndexType ),
			ReportVarType( ReportVarType ),
			CurMeterReading( CurMeterReading )
		{}

	};

	struct SubSubcomponentData // data for an individual component
	{
		// Members
		std::string TypeOf; // The 'keyWord' identifying  component type
		std::string Name; // Component name
		int CompIndex; // Component Index in whatever is using this component
		std::string NodeNameIn; // Component inlet node name
		std::string NodeNameOut; // Component outlet node name
		bool ON; // TRUE = designated component or operation scheme available
		int NodeNumIn; // Component inlet node number
		int NodeNumOut; // Component outlet node number
		bool MeteredVarsFound;
		int NumMeteredVars;
		int EnergyTransComp; // 1=EnergyTransfer, 0=No EnergyTransfer - Reporting flag
		Real64 TotPlantSupplyElec;
		Real64 PlantSupplyElecEff;
		Real64 PeakPlantSupplyElecEff;
		Real64 TotPlantSupplyGas;
		Real64 PlantSupplyGasEff;
		Real64 PeakPlantSupplyGasEff;
		Real64 TotPlantSupplyPurch;
		Real64 PlantSupplyPurchEff;
		Real64 PeakPlantSupplyPurchEff;
		Real64 TotPlantSupplyOther;
		Real64 PlantSupplyOtherEff;
		Real64 PeakPlantSupplyOtherEff;
		Real64 Capacity;
		Real64 Efficiency;
		int OpMode;
		Array1D< MeterData > MeteredVar; // Index of energy output report data
		int AirSysToPlantPtr; // 0=No plant connection, >0 = index to AirSysToPlant array

		// Default Constructor
		SubSubcomponentData() :
			CompIndex( 0 ),
			ON( true ),
			NodeNumIn( 0 ),
			NodeNumOut( 0 ),
			MeteredVarsFound( false ),
			NumMeteredVars( 0 ),
			EnergyTransComp( 0 ),
			TotPlantSupplyElec( 0.0 ),
			PlantSupplyElecEff( 0.0 ),
			PeakPlantSupplyElecEff( 0.0 ),
			TotPlantSupplyGas( 0.0 ),
			PlantSupplyGasEff( 0.0 ),
			PeakPlantSupplyGasEff( 0.0 ),
			TotPlantSupplyPurch( 0.0 ),
			PlantSupplyPurchEff( 0.0 ),
			PeakPlantSupplyPurchEff( 0.0 ),
			TotPlantSupplyOther( 0.0 ),
			PlantSupplyOtherEff( 0.0 ),
			PeakPlantSupplyOtherEff( 0.0 ),
			Capacity( 0.0 ),
			Efficiency( 0.0 ),
			OpMode( 0 ),
			AirSysToPlantPtr( 0 )
		{}

		// Member Constructor
		SubSubcomponentData(
			std::string const & TypeOf, // The 'keyWord' identifying  component type
			std::string const & Name, // Component name
			int const CompIndex, // Component Index in whatever is using this component
			std::string const & NodeNameIn, // Component inlet node name
			std::string const & NodeNameOut, // Component outlet node name
			bool const ON, // TRUE = designated component or operation scheme available
			int const NodeNumIn, // Component inlet node number
			int const NodeNumOut, // Component outlet node number
			bool const MeteredVarsFound,
			int const NumMeteredVars,
			int const EnergyTransComp, // 1=EnergyTransfer, 0=No EnergyTransfer - Reporting flag
			Real64 const TotPlantSupplyElec,
			Real64 const PlantSupplyElecEff,
			Real64 const PeakPlantSupplyElecEff,
			Real64 const TotPlantSupplyGas,
			Real64 const PlantSupplyGasEff,
			Real64 const PeakPlantSupplyGasEff,
			Real64 const TotPlantSupplyPurch,
			Real64 const PlantSupplyPurchEff,
			Real64 const PeakPlantSupplyPurchEff,
			Real64 const TotPlantSupplyOther,
			Real64 const PlantSupplyOtherEff,
			Real64 const PeakPlantSupplyOtherEff,
			Real64 const Capacity,
			Real64 const Efficiency,
			int const OpMode,
			Array1< MeterData > const & MeteredVar, // Index of energy output report data
			int const AirSysToPlantPtr // 0=No plant connection, >0 = index to AirSysToPlant array
		) :
			TypeOf( TypeOf ),
			Name( Name ),
			CompIndex( CompIndex ),
			NodeNameIn( NodeNameIn ),
			NodeNameOut( NodeNameOut ),
			ON( ON ),
			NodeNumIn( NodeNumIn ),
			NodeNumOut( NodeNumOut ),
			MeteredVarsFound( MeteredVarsFound ),
			NumMeteredVars( NumMeteredVars ),
			EnergyTransComp( EnergyTransComp ),
			TotPlantSupplyElec( TotPlantSupplyElec ),
			PlantSupplyElecEff( PlantSupplyElecEff ),
			PeakPlantSupplyElecEff( PeakPlantSupplyElecEff ),
			TotPlantSupplyGas( TotPlantSupplyGas ),
			PlantSupplyGasEff( PlantSupplyGasEff ),
			PeakPlantSupplyGasEff( PeakPlantSupplyGasEff ),
			TotPlantSupplyPurch( TotPlantSupplyPurch ),
			PlantSupplyPurchEff( PlantSupplyPurchEff ),
			PeakPlantSupplyPurchEff( PeakPlantSupplyPurchEff ),
			TotPlantSupplyOther( TotPlantSupplyOther ),
			PlantSupplyOtherEff( PlantSupplyOtherEff ),
			PeakPlantSupplyOtherEff( PeakPlantSupplyOtherEff ),
			Capacity( Capacity ),
			Efficiency( Efficiency ),
			OpMode( OpMode ),
			MeteredVar( MeteredVar ),
			AirSysToPlantPtr( AirSysToPlantPtr )
		{}

	};

	struct SubcomponentData // data for an individual component
	{
		// Members
		std::string TypeOf; // The 'keyWord' identifying  component type
		std::string Name; // Component name
		int CompIndex; // Component Index in whatever is using this component
		bool Parent; // TRUE = designated component is made up of sub-components
		int NumSubSubComps;
		std::string NodeNameIn; // Component inlet node name
		std::string NodeNameOut; // Component outlet node name
		int NodeNumIn; // Component inlet node number
		int NodeNumOut; // Component outlet node number
		bool MeteredVarsFound;
		bool ON; // TRUE = designated component or operation scheme available
		int NumMeteredVars;
		int EnergyTransComp; // 1=EnergyTransfer, 0=No EnergyTransfer - Reporting flag
		Real64 Capacity;
		Real64 Efficiency;
		int OpMode;
		Real64 TotPlantSupplyElec;
		Real64 PlantSupplyElecEff;
		Real64 PeakPlantSupplyElecEff;
		Real64 TotPlantSupplyGas;
		Real64 PlantSupplyGasEff;
		Real64 PeakPlantSupplyGasEff;
		Real64 TotPlantSupplyPurch;
		Real64 PlantSupplyPurchEff;
		Real64 PeakPlantSupplyPurchEff;
		Real64 TotPlantSupplyOther;
		Real64 PlantSupplyOtherEff;
		Real64 PeakPlantSupplyOtherEff;
		int AirSysToPlantPtr; // 0=No plant connection, >0 = index to AirSysToPlant array
		Real64 LoopLoadFrac;
		Array1D< MeterData > MeteredVar; // Index of energy output report data
		Array1D< SubSubcomponentData > SubSubComp; // Component list

		// Default Constructor
		SubcomponentData() :
			CompIndex( 0 ),
			Parent( false ),
			NumSubSubComps( 0 ),
			NodeNumIn( 0 ),
			NodeNumOut( 0 ),
			MeteredVarsFound( false ),
			ON( true ),
			NumMeteredVars( 0 ),
			EnergyTransComp( 0 ),
			Capacity( 0.0 ),
			Efficiency( 0.0 ),
			OpMode( 0 ),
			TotPlantSupplyElec( 0.0 ),
			PlantSupplyElecEff( 0.0 ),
			PeakPlantSupplyElecEff( 0.0 ),
			TotPlantSupplyGas( 0.0 ),
			PlantSupplyGasEff( 0.0 ),
			PeakPlantSupplyGasEff( 0.0 ),
			TotPlantSupplyPurch( 0.0 ),
			PlantSupplyPurchEff( 0.0 ),
			PeakPlantSupplyPurchEff( 0.0 ),
			TotPlantSupplyOther( 0.0 ),
			PlantSupplyOtherEff( 0.0 ),
			PeakPlantSupplyOtherEff( 0.0 ),
			AirSysToPlantPtr( 0 ),
			LoopLoadFrac( 0.0 )
		{}

		// Member Constructor
		SubcomponentData(
			std::string const & TypeOf, // The 'keyWord' identifying  component type
			std::string const & Name, // Component name
			int const CompIndex, // Component Index in whatever is using this component
			bool const Parent, // TRUE = designated component is made up of sub-components
			int const NumSubSubComps,
			std::string const & NodeNameIn, // Component inlet node name
			std::string const & NodeNameOut, // Component outlet node name
			int const NodeNumIn, // Component inlet node number
			int const NodeNumOut, // Component outlet node number
			bool const MeteredVarsFound,
			bool const ON, // TRUE = designated component or operation scheme available
			int const NumMeteredVars,
			int const EnergyTransComp, // 1=EnergyTransfer, 0=No EnergyTransfer - Reporting flag
			Real64 const Capacity,
			Real64 const Efficiency,
			int const OpMode,
			Real64 const TotPlantSupplyElec,
			Real64 const PlantSupplyElecEff,
			Real64 const PeakPlantSupplyElecEff,
			Real64 const TotPlantSupplyGas,
			Real64 const PlantSupplyGasEff,
			Real64 const PeakPlantSupplyGasEff,
			Real64 const TotPlantSupplyPurch,
			Real64 const PlantSupplyPurchEff,
			Real64 const PeakPlantSupplyPurchEff,
			Real64 const TotPlantSupplyOther,
			Real64 const PlantSupplyOtherEff,
			Real64 const PeakPlantSupplyOtherEff,
			int const AirSysToPlantPtr, // 0=No plant connection, >0 = index to AirSysToPlant array
			Real64 const LoopLoadFrac,
			Array1< MeterData > const & MeteredVar, // Index of energy output report data
			Array1< SubSubcomponentData > const & SubSubComp // Component list
		) :
			TypeOf( TypeOf ),
			Name( Name ),
			CompIndex( CompIndex ),
			Parent( Parent ),
			NumSubSubComps( NumSubSubComps ),
			NodeNameIn( NodeNameIn ),
			NodeNameOut( NodeNameOut ),
			NodeNumIn( NodeNumIn ),
			NodeNumOut( NodeNumOut ),
			MeteredVarsFound( MeteredVarsFound ),
			ON( ON ),
			NumMeteredVars( NumMeteredVars ),
			EnergyTransComp( EnergyTransComp ),
			Capacity( Capacity ),
			Efficiency( Efficiency ),
			OpMode( OpMode ),
			TotPlantSupplyElec( TotPlantSupplyElec ),
			PlantSupplyElecEff( PlantSupplyElecEff ),
			PeakPlantSupplyElecEff( PeakPlantSupplyElecEff ),
			TotPlantSupplyGas( TotPlantSupplyGas ),
			PlantSupplyGasEff( PlantSupplyGasEff ),
			PeakPlantSupplyGasEff( PeakPlantSupplyGasEff ),
			TotPlantSupplyPurch( TotPlantSupplyPurch ),
			PlantSupplyPurchEff( PlantSupplyPurchEff ),
			PeakPlantSupplyPurchEff( PeakPlantSupplyPurchEff ),
			TotPlantSupplyOther( TotPlantSupplyOther ),
			PlantSupplyOtherEff( PlantSupplyOtherEff ),
			PeakPlantSupplyOtherEff( PeakPlantSupplyOtherEff ),
			AirSysToPlantPtr( AirSysToPlantPtr ),
			LoopLoadFrac( LoopLoadFrac ),
			MeteredVar( MeteredVar ),
			SubSubComp( SubSubComp )
		{}

	};

	struct ReportCompData
	{
		// Members
		bool Parent; // TRUE = designated component is made up of sub-components
		std::string TypeOf; // The 'keyWord' identifying  component type
		std::string Name; // Component name
		int CompIndex; // Component Index in whatever is using this component
		std::string NodeNameIn; // Component inlet node name
		std::string NodeNameOut; // Component outlet node name
		int NodeNumIn; // Component inlet node number
		int NodeNumOut; // Component outlet node number
		int NumMeteredVars;
		int NumSubComps;
		Real64 LoopLoadFrac; // Fraction of loop load met by component
		Real64 TotPlantSupplyElec;
		Real64 TotPlantSupplyGas;
		Real64 TotPlantSupplyPurch;
		Real64 TotPlantSupplyOther;
		PlantConnection ConnectPlant; // Index of energy output report data
		Array1D< MeterData > MeteredVar; // Index of energy output report data
		Array1D< SubcomponentData > SubComp;

		// Default Constructor
		ReportCompData() :
			Parent( false ),
			CompIndex( 0 ),
			NodeNumIn( 0 ),
			NodeNumOut( 0 ),
			NumMeteredVars( 0 ),
			NumSubComps( 0 ),
			LoopLoadFrac( 0.0 ),
			TotPlantSupplyElec( 0.0 ),
			TotPlantSupplyGas( 0.0 ),
			TotPlantSupplyPurch( 0.0 ),
			TotPlantSupplyOther( 0.0 )
		{}

		// Member Constructor
		ReportCompData(
			bool const Parent, // TRUE = designated component is made up of sub-components
			std::string const & TypeOf, // The 'keyWord' identifying  component type
			std::string const & Name, // Component name
			int const CompIndex, // Component Index in whatever is using this component
			std::string const & NodeNameIn, // Component inlet node name
			std::string const & NodeNameOut, // Component outlet node name
			int const NodeNumIn, // Component inlet node number
			int const NodeNumOut, // Component outlet node number
			int const NumMeteredVars,
			int const NumSubComps,
			Real64 const LoopLoadFrac, // Fraction of loop load met by component
			Real64 const TotPlantSupplyElec,
			Real64 const TotPlantSupplyGas,
			Real64 const TotPlantSupplyPurch,
			Real64 const TotPlantSupplyOther,
			PlantConnection const & ConnectPlant, // Index of energy output report data
			Array1< MeterData > const & MeteredVar, // Index of energy output report data
			Array1< SubcomponentData > const & SubComp
		) :
			Parent( Parent ),
			TypeOf( TypeOf ),
			Name( Name ),
			CompIndex( CompIndex ),
			NodeNameIn( NodeNameIn ),
			NodeNameOut( NodeNameOut ),
			NodeNumIn( NodeNumIn ),
			NodeNumOut( NodeNumOut ),
			NumMeteredVars( NumMeteredVars ),
			NumSubComps( NumSubComps ),
			LoopLoadFrac( LoopLoadFrac ),
			TotPlantSupplyElec( TotPlantSupplyElec ),
			TotPlantSupplyGas( TotPlantSupplyGas ),
			TotPlantSupplyPurch( TotPlantSupplyPurch ),
			TotPlantSupplyOther( TotPlantSupplyOther ),
			ConnectPlant( ConnectPlant ),
			MeteredVar( MeteredVar ),
			SubComp( SubComp )
		{}

	};

	struct ReportBranchData
	{
		// Members
		std::string Name; // Name of the branch
		int TotalComponents; // Total number of components on the branch
		int NodeNumIn; // Branch inlet node number
		int NodeNumOut; // Branch outlet node number
		Array1D< ReportCompData > Comp; // Component type list

		// Default Constructor
		ReportBranchData() :
			TotalComponents( 0 ),
			NodeNumIn( 0 ),
			NodeNumOut( 0 )
		{}

		// Member Constructor
		ReportBranchData(
			std::string const & Name, // Name of the branch
			int const TotalComponents, // Total number of components on the branch
			int const NodeNumIn, // Branch inlet node number
			int const NodeNumOut, // Branch outlet node number
			Array1< ReportCompData > const & Comp // Component type list
		) :
			Name( Name ),
			TotalComponents( TotalComponents ),
			NodeNumIn( NodeNumIn ),
			NodeNumOut( NodeNumOut ),
			Comp( Comp )
		{}

	};

	struct ReportLoopData
	{
		// Members
		std::string Name; // Name of the component list
		int NodeNumIn; // Node number for the inlet to this loop
		std::string NodeNameIn; // Node name for the inlet to this loop
		int NodeNumOut; // Node number for the outlet to this loop
		std::string NodeNameOut; // Node name for the outlet to this loop
		Real64 Electric;
		Real64 Gas;
		Real64 Purchased;
		Real64 OtherEnergy;
		int TotalBranches; // Total number of branches on the loop
		Real64 LoopVentLoad;
		Real64 VentLoadFrac;
		Array1D< ReportBranchData > Branch; // Branch data

		// Default Constructor
		ReportLoopData() :
			NodeNumIn( 0 ),
			NodeNumOut( 0 ),
			Electric( 0.0 ),
			Gas( 0.0 ),
			Purchased( 0.0 ),
			OtherEnergy( 0.0 ),
			TotalBranches( 0 ),
			LoopVentLoad( 0.0 ),
			VentLoadFrac( 0.0 )
		{}

		// Member Constructor
		ReportLoopData(
			std::string const & Name, // Name of the component list
			int const NodeNumIn, // Node number for the inlet to this loop
			std::string const & NodeNameIn, // Node name for the inlet to this loop
			int const NodeNumOut, // Node number for the outlet to this loop
			std::string const & NodeNameOut, // Node name for the outlet to this loop
			Real64 const Electric,
			Real64 const Gas,
			Real64 const Purchased,
			Real64 const OtherEnergy,
			int const TotalBranches, // Total number of branches on the loop
			Real64 const LoopVentLoad,
			Real64 const VentLoadFrac,
			Array1< ReportBranchData > const & Branch // Branch data
		) :
			Name( Name ),
			NodeNumIn( NodeNumIn ),
			NodeNameIn( NodeNameIn ),
			NodeNumOut( NodeNumOut ),
			NodeNameOut( NodeNameOut ),
			Electric( Electric ),
			Gas( Gas ),
			Purchased( Purchased ),
			OtherEnergy( OtherEnergy ),
			TotalBranches( TotalBranches ),
			LoopVentLoad( LoopVentLoad ),
			VentLoadFrac( VentLoadFrac ),
			Branch( Branch )
		{}

	};

	struct PlantCallingOrderInfoStruct
	{
		// Members
		int LoopIndex; // plant or condenser loop indexes in calling order
		int LoopSide; // plant or condenser loop sides in calling order
		//  INTEGER :: InterAct1LoopIndex     = 0 ! primary interaction dependency reference loop index
		//  INTEGER :: InterAct1LoopSide      = 0 ! primary interaction dependency reference loop side
		//  INTEGER :: InterAct2LoopIndex     = 0 ! secondary interaction dependency reference loop index
		//  INTEGER :: InterAct2LoopSide      = 0 ! secondary interaction dependency reference loop side
		int LoopPumpSimulationType; // type of pump topology on half loop

		// Default Constructor
		PlantCallingOrderInfoStruct() :
			LoopIndex( 0 ),
			LoopSide( 0 ),
			LoopPumpSimulationType( 0 )
		{}

		// Member Constructor
		PlantCallingOrderInfoStruct(
			int const LoopIndex, // plant or condenser loop indexes in calling order
			int const LoopSide, // plant or condenser loop sides in calling order
			int const LoopPumpSimulationType // type of pump topology on half loop
		) :
			LoopIndex( LoopIndex ),
			LoopSide( LoopSide ),
			LoopPumpSimulationType( LoopPumpSimulationType )
		{}

	};

	// Object Data
	extern Array1D< PipeData > Pipe;
	extern Array1D< PlantLoopData > PlantLoop;
	extern Array1D< PlantAvailMgrData > PlantAvailMgr;
	extern Array1D< ReportVars > PlantReport;
	extern Array1D< ReportLoopData > VentRepPlantSupplySide;
	extern Array1D< ReportLoopData > VentRepPlantDemandSide;
	extern Array1D< ReportLoopData > VentRepCondSupplySide;
	extern Array1D< ReportLoopData > VentRepCondDemandSide;
	extern Array1D< PlantCallingOrderInfoStruct > PlantCallingOrderInfo;

	// Functions

	// Clears the global data in DataPlant.
	// Needed for unit tests, should not be normally called.
	void
	clear_state();


	void
	ScanPlantLoopsForObject(
		std::string const & CompName,
		int const CompType,
		int & LoopNum,
		int & LoopSideNum,
		int & BranchNum,
		int & CompNum,
		Optional< Real64 const > LowLimitTemp = _,
		Optional< Real64 const > HighLimitTemp = _,
		Optional_int CountMatchPlantLoops = _,
		Optional_int_const InletNodeNumber = _,
		Optional_int_const SingleLoopSearch = _,
		Optional_bool errFlag = _
	);

	void
	ScanPlantLoopsForNodeNum(
		std::string const & CallerName, // really used for error messages
		int const NodeNum, // index in Node structure of node to be scanned
		int & LoopNum, // return value for plant loop
		int & LoopSideNum, // return value for plant loop side
		int & BranchNum,
		Optional_int CompNum = _
	);

	bool
	AnyPlantLoopSidesNeedSim();

	void
	SetAllPlantSimFlagsToValue( bool const Value );

	int
	GetLoopSidePumpIndex(
		int const LoopNum,
		int const LoopSideNum,
		int const BranchNum,
		int const CompNum
	);

	void
	ShowBranchesOnLoop( int const LoopNum ); // Loop number of loop

	int
	MyPlantSizingIndex(
		std::string const & CompType, // component description
		std::string const & CompName, // user name of component
		int const NodeNumIn, // component water inlet node
		int const NodeNumOut, // component water outlet node
		bool & ErrorsFound, // set to true if there's an error
		Optional_bool_const SupressErrors = _ // used for WSHP's where condenser loop may not be on a plant loop
	);

	bool
	verifyTwoNodeNumsOnSamePlantLoop(
		int const nodeIndexA,
		int const nodeIndexB
	);

	//     NOTICE

	//     Copyright (c) 1996-2014 The Board of Trustees of the University of Illinois
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

#endif
