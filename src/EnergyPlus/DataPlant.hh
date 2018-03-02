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
#include <PlantLoopSolver.hh>
#include <OutputProcessor.hh>
#include <Plant/Operation/Enums.hh>
#include <Plant/Topology/Loop.hh>

namespace EnergyPlus {

namespace DataPlant {

	// Using/Aliasing
	using DataLoopNode::SensedNodeFlagValue;

	int const OptimalLoading( 1 ); // Optimal Load Distribution Scheme
	int const SequentialLoading( 2 ); // Sequential Load Distribution Scheme
	int const UniformLoading( 3 ); // Uniform Load Distribution Scheme
	int const UniformPLRLoading( 4 ); // Uniform PLR Load Distribution Scheme
	int const SequentialUniformPLRLoading( 5 ); // Sequential Uniform PLR Load Distribution Scheme

	extern int const LoadRangeBasedMin;
	extern int const LoadRangeBasedMax;

	// SimFlagCriteriaTypes for use in performing interconnect re-sim checks
	extern int const CriteriaType_MassFlowRate;
	extern int const CriteriaType_Temperature;
	extern int const CriteriaType_HeatTransferRate;

	// Criteria percentage limits for determining re-simulation of connected loop sides
	extern Real64 const CriteriaDelta_MassFlowRate;
	extern Real64 const CriteriaDelta_Temperature;
	extern Real64 const CriteriaDelta_HeatTransferRate;


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

	// Parameters for common pipe
	extern int const CommonPipe_No;
	extern int const CommonPipe_Single;
	extern int const CommonPipe_TwoWay;

	// Parameters for loop side location
	extern int const DemandSupply_No;
	extern int const DemandSide;
	extern int const SupplySide;

	// Parameters for economizer
	extern int const Integrated;
	extern int const NonIntegrated;
	extern int const None;

	// Parameters for tolerance
	extern Real64 const LoopDemandTol; // minimum significant loop cooling or heating demand
	extern Real64 const DeltaTempTol; // minimum significant loop temperature difference

	// Parameters for Component/Equipment Types  (ref: TypeOf in CompData)
	extern int const LoopType_Plant;
	extern int const LoopType_Condenser;
	extern int const LoopType_Both;

	// Parameters for FlowLock standardization
	extern int const FlowPumpQuery; // Used to ask the pumps for their min/max avail based on no constraints
	extern int const FlowUnlocked; // components request flow
	extern int const FlowLocked; // components take their inlet flow

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
	extern Array1D_string const SimPlantEquipTypes;

	extern Array1D_string const ccSimPlantEquipTypes;

	extern Array1D_int const ValidLoopEquipTypes;

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
	extern int const TypeOf_GrndHtExchgSystem;
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
	extern int const TypeOf_CoolingPanel_Simple;
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

	extern Array1D< Real64 > const ConvergenceHistoryARR;
	extern Real64 const sum_ConvergenceHistoryARR;
	extern Real64 const square_sum_ConvergenceHistoryARR;
	extern Real64 const sum_square_ConvergenceHistoryARR;

	extern int NumPipes; // Total number of pipes
	extern int NumPlantPipes; // Total number of plant pipes
	extern int NumCondPipes; // Total number of condenser pipes
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

	extern Array1D_bool LoadChangeDownStream; // sim control flag.

	extern int PlantManageSubIterations; // tracks plant iterations to characterize solver
	extern int PlantManageHalfLoopCalls; // tracks number of half loop calls

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

	};

	struct MeterData
	{
		// Members
		std::string ReportVarName;
		OutputProcessor::Unit ReportVarUnits;
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
			ReportVarUnits( OutputProcessor::Unit::None ),
			ResourceType( 0 ),
			EndUse_CompMode( 0 ),
			ReportVarIndex( 0 ),
			ReportVarIndexType( 0 ),
			ReportVarType( 0 ),
			CurMeterReading( 0.0 )
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

	};

	struct PlantCallingOrderInfoStruct
	{
		// Members
		int LoopIndex; // plant or condenser loop indexes in calling order
		int LoopSide; // plant or condenser loop sides in calling order
		int LoopPumpSimulationType; // type of pump topology on half loop

		// Default Constructor
		PlantCallingOrderInfoStruct() :
			LoopIndex( 0 ),
			LoopSide( 0 ),
			LoopPumpSimulationType( 0 )
		{}

	};

	// Object Data
	extern Array1D< PlantLoopData > PlantLoop;
	extern Array1D< PlantAvailMgrData > PlantAvailMgr;
	extern Array1D< ReportVars > PlantReport;
	extern Array1D< ReportLoopData > VentRepPlantSupplySide;
	extern Array1D< ReportLoopData > VentRepPlantDemandSide;
	extern Array1D< ReportLoopData > VentRepCondSupplySide;
	extern Array1D< ReportLoopData > VentRepCondDemandSide;
	extern Array1D< PlantCallingOrderInfoStruct > PlantCallingOrderInfo;

	// Clears the global data in DataPlant.
	// Needed for unit tests, should not be normally called.
	void
	clear_state();

} // DataPlant

} // EnergyPlus

#endif
