// EnergyPlus, Copyright (c) 1996-2016, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
//
// If you have questions about your rights to use or distribute this software, please contact
// Berkeley Lab's Innovation & Partnerships Office at IPO@lbl.gov.
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
//     similar designation, without Lawrence Berkeley National Laboratory's prior written consent.
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
//
// You are under no obligation whatsoever to provide any bug fixes, patches, or upgrades to the
// features, functionality or performance of the source code ("Enhancements") to anyone; however,
// if you choose to make your Enhancements available either publicly, or directly to Lawrence
// Berkeley National Laboratory, without imposing a separate written license agreement for such
// Enhancements, then you hereby grant the following license: a non-exclusive, royalty-free
// perpetual license to install, use, modify, prepare derivative works, incorporate into other
// computer software, distribute, and sublicense such enhancements or derivative works thereof,
// in binary and source code form.

// C++ Headers
#include <cassert>
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <PlantHeatExchangerFluidToFluid.hh>
#include <BranchNodeConnections.hh>
#include <DataBranchAirLoopPlant.hh>
#include <DataEnvironment.hh>
#include <DataHVACGlobals.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <EMSManager.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <OutputReportPredefined.hh>
#include <PlantUtilities.hh>
#include <ReportSizingManager.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace PlantHeatExchangerFluidToFluid {

	// Module containing the routines dealing with the HeatExchanger:FluidToFluid

	// MODULE INFORMATION:
	//       AUTHOR         B. Griffith, derived from legacy code by  Sankaranarayanan K P, and S. Rees
	//       DATE WRITTEN   November 2012
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// Simulate a generic plant heat exchanger with a variety of control options

	// METHODOLOGY EMPLOYED:
	// <description>

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataPlant;
	using namespace DataLoopNode;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	int const CrossFlowBothUnMixed( 1 );
	int const CrossFlowBothMixed( 2 );
	int const CrossFlowSupplyLoopMixedDemandLoopUnMixed( 3 );
	int const CrossFlowSupplyLoopUnMixedDemandLoopMixed( 4 );
	int const CounterFlow( 5 );
	int const ParallelFlow( 6 );
	int const Ideal( 7 );

	int const UncontrolledOn( 1001 );
	int const OperationSchemeModulated( 1002 );
	int const OperationSchemeOnOff( 1003 );
	int const HeatingSetPointModulated( 1004 );
	int const HeatingSetPointOnOff( 1005 );
	int const CoolingSetPointModulated( 1006 );
	int const CoolingSetPointOnOff( 1007 );
	int const DualDeadBandSetPointModulated( 1008 );
	int const DualDeadBandSetPointOnOff( 1009 );
	int const CoolingDifferentialOnOff( 1010 );
	int const CoolingSetPointOnOffWithComponentOverride( 1011 );
	int const TrackComponentOnOff( 1012 );

	int const WetBulbTemperature( 10 );
	int const DryBulbTemperature( 11 );
	int const LoopTemperature( 12 );

	int const HeatingSupplySideLoop( 501 );
	int const CoolingSupplySideLoop( 502 );

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	std::string ComponentClassName( "HeatExchanger:FluidToFluid" );
	int NumberOfPlantFluidHXs( 0 );
	bool GetInput( true );
	Array1D_bool CheckFluidHXs;

	// SUBROUTINE SPECIFICATIONS FOR MODULE

	// Object Data
	Array1D< HeatExchangerStruct > FluidHX;

	// Functions

	void
	SimFluidHeatExchanger(
		int const LoopNum, // plant loop sim call originated from
		int const EP_UNUSED( LoopSideNum ), // plant loop side sim call originated from
		std::string const & EP_UNUSED( EquipType ), // type of equipment, 'PlantComponent:UserDefined'
		std::string const & EquipName, // user name for component
		int & CompIndex,
		bool & InitLoopEquip,
		Real64 const MyLoad,
		Real64 & MaxCap,
		Real64 & MinCap,
		Real64 & OptCap,
		bool const FirstHVACIteration
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   November 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Main entry point and simulation manager for heat exchanger

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using General::TrimSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int CompNum;

		if ( GetInput ) {
			GetFluidHeatExchangerInput();
			GetInput = false;
		}

		// Find the correct Equipment
		if ( CompIndex == 0 ) {
			CompNum = FindItemInList( EquipName, FluidHX );
			if ( CompNum == 0 ) {
				ShowFatalError( "SimFluidHeatExchanger: HeatExchanger:FluidToFluid not found" );
			}
			CompIndex = CompNum;
		} else {
			CompNum = CompIndex;
			if ( CompNum < 1 || CompNum > NumberOfPlantFluidHXs ) {
				ShowFatalError( "SimFluidHeatExchanger: Invalid CompIndex passed=" + TrimSigDigits( CompNum ) + ", Number of heat exchangers =" + TrimSigDigits( NumberOfPlantFluidHXs ) + ", Entered heat exchanger name = " + EquipName );
			}
			if ( CheckFluidHXs( CompNum ) ) {
				if ( EquipName != FluidHX( CompNum ).Name ) {
					ShowFatalError( "SimFluidHeatExchanger: Invalid CompIndex passed=" + TrimSigDigits( CompNum ) + ", heat exchanger name=" + EquipName + ", stored name for that index=" + FluidHX( CompNum ).Name );
				}
				CheckFluidHXs( CompNum ) = false;
			}
		}

		if ( InitLoopEquip ) {
			InitFluidHeatExchanger( CompNum, LoopNum );
			if ( LoopNum == FluidHX( CompNum ).DemandSideLoop.LoopNum ) {
				MinCap = 0.0;
				MaxCap = FluidHX( CompNum ).DemandSideLoop.MaxLoad;
				OptCap = FluidHX( CompNum ).DemandSideLoop.MaxLoad * 0.9;
			} else if ( LoopNum == FluidHX( CompNum ).SupplySideLoop.LoopNum ) {
				SizeFluidHeatExchanger( CompNum ); // only call sizing from the loop that sizes are based on
				MinCap = 0.0;
				MaxCap = FluidHX( CompNum ).SupplySideLoop.MaxLoad;
				OptCap = FluidHX( CompNum ).SupplySideLoop.MaxLoad * 0.9;
			}
		}

		InitFluidHeatExchanger( CompNum, LoopNum );

		// for op scheme led HXs, only call controls if called from Loop Supply Side
		if ( ( FluidHX( CompNum ).ControlMode == OperationSchemeModulated ) || ( FluidHX( CompNum ).ControlMode == OperationSchemeOnOff ) ) {
			if ( LoopNum == FluidHX( CompNum ).SupplySideLoop.LoopNum ) {
				ControlFluidHeatExchanger( CompNum, LoopNum, MyLoad, FirstHVACIteration );
			}
		} else {
			ControlFluidHeatExchanger( CompNum, LoopNum, MyLoad, FirstHVACIteration );
		}

		CalcFluidHeatExchanger( CompNum, Node( FluidHX( CompNum ).SupplySideLoop.InletNodeNum ).MassFlowRate, Node( FluidHX( CompNum ).DemandSideLoop.InletNodeNum ).MassFlowRate );

		UpdateFluidHeatExchanger( CompNum );

		ReportFluidHeatExchanger( CompNum );

	}

	void
	GetFluidHeatExchangerInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   November 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// get input for heat exchanger model

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectDefMaxArgs;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using InputProcessor::VerifyName;
		using InputProcessor::SameString;
		using InputProcessor::FindItem;
		using General::RoundSigDigits;
		using NodeInputManager::GetOnlySingleNode;
		using BranchNodeConnections::TestCompSet;
		using DataGlobals::ScheduleAlwaysOn;
		using DataGlobals::AnyEnergyManagementSystemInModel;
		using ScheduleManager::GetScheduleIndex;
		using EMSManager::CheckIfNodeSetPointManagedByEMS;
		using EMSManager::iTemperatureSetPoint;
		using EMSManager::iTemperatureMinSetPoint;
		using EMSManager::iTemperatureMaxSetPoint;
		using DataSizing::AutoSize;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetFluidHeatExchangerInput: " );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool ErrorsFound( false );
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		int IOStat; // IO Status when calling get input subroutine
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		static int MaxNumAlphas( 0 ); // argument for call to GetObjectDefMaxArgs
		static int MaxNumNumbers( 0 ); // argument for call to GetObjectDefMaxArgs
		static int TotalArgs( 0 ); // argument for call to GetObjectDefMaxArgs
		Array1D_string cAlphaFieldNames;
		Array1D_string cNumericFieldNames;
		Array1D_bool lNumericFieldBlanks;
		Array1D_bool lAlphaFieldBlanks;
		Array1D_string cAlphaArgs;
		Array1D< Real64 > rNumericArgs;
		std::string cCurrentModuleObject;
		int CompLoop;
		bool NodeEMSSetPointMissing;

		cCurrentModuleObject = "HeatExchanger:FluidToFluid";

		NumberOfPlantFluidHXs = GetNumObjectsFound( cCurrentModuleObject );
		if ( NumberOfPlantFluidHXs == 0 ) return;

		GetObjectDefMaxArgs( cCurrentModuleObject, TotalArgs, NumAlphas, NumNums );
		MaxNumNumbers = NumNums;
		MaxNumAlphas = NumAlphas;

		cAlphaFieldNames.allocate( MaxNumAlphas );
		cAlphaArgs.allocate( MaxNumAlphas );
		lAlphaFieldBlanks.dimension( MaxNumAlphas, false );
		cNumericFieldNames.allocate( MaxNumNumbers );
		rNumericArgs.dimension( MaxNumNumbers, 0.0 );
		lNumericFieldBlanks.dimension( MaxNumNumbers, false );

		if ( NumberOfPlantFluidHXs > 0 ) {
			FluidHX.allocate( NumberOfPlantFluidHXs );
			CheckFluidHXs.dimension( NumberOfPlantFluidHXs, true );
			for ( CompLoop = 1; CompLoop <= NumberOfPlantFluidHXs; ++CompLoop ) {
				GetObjectItem( cCurrentModuleObject, CompLoop, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
				IsNotOK = false;
				IsBlank = false;
				VerifyName( cAlphaArgs( 1 ), FluidHX, CompLoop - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
				}
				FluidHX( CompLoop ).Name = cAlphaArgs( 1 );

				if ( lAlphaFieldBlanks( 2 ) ) {
					FluidHX( CompLoop ).AvailSchedNum = ScheduleAlwaysOn;
				} else {
					FluidHX( CompLoop ).AvailSchedNum = GetScheduleIndex( cAlphaArgs( 2 ) );
					if ( FluidHX( CompLoop ).AvailSchedNum <= 0 ) {
						ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid entry." );
						ShowContinueError( "Invalid " + cAlphaFieldNames( 2 ) + " = " + cAlphaArgs( 2 ) );
						ShowContinueError( "Schedule was not found " );
						ErrorsFound = true;
					}
				}

				FluidHX( CompLoop ).DemandSideLoop.InletNodeNum = GetOnlySingleNode( cAlphaArgs( 3 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
				FluidHX( CompLoop ).DemandSideLoop.OutletNodeNum = GetOnlySingleNode( cAlphaArgs( 4 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
				TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 3 ), cAlphaArgs( 4 ), "Loop Demand Side Plant Nodes" );
				FluidHX( CompLoop ).DemandSideLoop.DesignVolumeFlowRate = rNumericArgs( 1 );
				if ( FluidHX( CompLoop ).DemandSideLoop.DesignVolumeFlowRate == AutoSize ) {
					FluidHX( CompLoop ).DemandSideLoop.DesignVolumeFlowRateWasAutoSized = true;
				}

				FluidHX( CompLoop ).SupplySideLoop.InletNodeNum = GetOnlySingleNode( cAlphaArgs( 5 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Inlet, 2, ObjectIsNotParent );
				FluidHX( CompLoop ).SupplySideLoop.OutletNodeNum = GetOnlySingleNode( cAlphaArgs( 6 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Outlet, 2, ObjectIsNotParent );
				TestCompSet( cCurrentModuleObject, cAlphaArgs( 1 ), cAlphaArgs( 5 ), cAlphaArgs( 6 ), "Loop Supply Side Plant Nodes" );
				FluidHX( CompLoop ).SupplySideLoop.DesignVolumeFlowRate = rNumericArgs( 2 );
				if ( FluidHX( CompLoop ).SupplySideLoop.DesignVolumeFlowRate == AutoSize ) {
					FluidHX( CompLoop ).SupplySideLoop.DesignVolumeFlowRateWasAutoSized = true;
				}

				if ( SameString( cAlphaArgs( 7 ), "CrossFlowBothUnMixed" ) ) {
					FluidHX( CompLoop ).HeatExchangeModelType = CrossFlowBothUnMixed;
				} else if ( SameString( cAlphaArgs( 7 ), "CrossFlowBothMixed" ) ) {
					FluidHX( CompLoop ).HeatExchangeModelType = CrossFlowBothMixed;
				} else if ( SameString( cAlphaArgs( 7 ), "CrossFlowSupplyMixedDemandUnMixed" ) ) {
					FluidHX( CompLoop ).HeatExchangeModelType = CrossFlowSupplyLoopMixedDemandLoopUnMixed;
				} else if ( SameString( cAlphaArgs( 7 ), "CrossFlowSupplyUnMixedDemandMixed" ) ) {
					FluidHX( CompLoop ).HeatExchangeModelType = CrossFlowSupplyLoopUnMixedDemandLoopMixed;
				} else if ( SameString( cAlphaArgs( 7 ), "CounterFlow" ) ) {
					FluidHX( CompLoop ).HeatExchangeModelType = CounterFlow;
				} else if ( SameString( cAlphaArgs( 7 ), "ParallelFlow" ) ) {
					FluidHX( CompLoop ).HeatExchangeModelType = ParallelFlow;
				} else if ( SameString( cAlphaArgs( 7 ), "Ideal" ) ) {
					FluidHX( CompLoop ).HeatExchangeModelType = Ideal;
				} else {
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + cAlphaFieldNames( 7 ) + " = " + cAlphaArgs( 7 ) );
					ErrorsFound = true;
				}

				if ( ! lNumericFieldBlanks( 3 ) ) {
					FluidHX( CompLoop ).UA = rNumericArgs( 3 );
					if ( FluidHX( CompLoop ).UA == AutoSize ) {
						FluidHX( CompLoop ).UAWasAutoSized = true;
					}
				} else {
					if ( FluidHX( CompLoop ).HeatExchangeModelType != Ideal ) {
						ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid entry." );
						ShowContinueError( "Missing entry for " + cNumericFieldNames( 3 ) );
						ErrorsFound = true;
					}
				}

				if ( SameString( cAlphaArgs( 8 ), "UncontrolledOn" ) ) {
					FluidHX( CompLoop ).ControlMode = UncontrolledOn;
				} else if ( SameString( cAlphaArgs( 8 ), "OperationSchemeModulated" ) ) {
					FluidHX( CompLoop ).ControlMode = OperationSchemeModulated;
				} else if ( SameString( cAlphaArgs( 8 ), "OperationSchemeOnOff" ) ) {
					FluidHX( CompLoop ).ControlMode = OperationSchemeOnOff;
				} else if ( SameString( cAlphaArgs( 8 ), "HeatingSetpointModulated" ) ) {
					FluidHX( CompLoop ).ControlMode = HeatingSetPointModulated;
				} else if ( SameString( cAlphaArgs( 8 ), "HeatingSetpointOnOff" ) ) {
					FluidHX( CompLoop ).ControlMode = HeatingSetPointOnOff;
				} else if ( SameString( cAlphaArgs( 8 ), "CoolingSetpointModulated" ) ) {
					FluidHX( CompLoop ).ControlMode = CoolingSetPointModulated;
				} else if ( SameString( cAlphaArgs( 8 ), "CoolingSetpointOnOff" ) ) {
					FluidHX( CompLoop ).ControlMode = CoolingSetPointOnOff;
				} else if ( SameString( cAlphaArgs( 8 ), "DualDeadbandSetpointModulated" ) ) {
					FluidHX( CompLoop ).ControlMode = DualDeadBandSetPointModulated;
				} else if ( SameString( cAlphaArgs( 8 ), "DualDeadbandSetpointOnOff" ) ) {
					FluidHX( CompLoop ).ControlMode = DualDeadBandSetPointOnOff;
				} else if ( SameString( cAlphaArgs( 8 ), "CoolingDifferentialOnOff" ) ) {
					FluidHX( CompLoop ).ControlMode = CoolingDifferentialOnOff;
				} else if ( SameString( cAlphaArgs( 8 ), "CoolingSetpointOnOffWithComponentOverride" ) ) {
					FluidHX( CompLoop ).ControlMode = CoolingSetPointOnOffWithComponentOverride;
				} else if ( SameString( cAlphaArgs( 8 ), "TrackComponentOnOff" ) ) {
					FluidHX( CompLoop ).ControlMode = TrackComponentOnOff;
				} else {
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "Invalid " + cAlphaFieldNames( 8 ) + " = " + cAlphaArgs( 8 ) );
					ErrorsFound = true;
				}

				if ( ! lAlphaFieldBlanks( 9 ) ) {
					FluidHX( CompLoop ).SetPointNodeNum = GetOnlySingleNode( cAlphaArgs( 9 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Sensor, 1, ObjectIsNotParent );
					// check that node actually has setpoints on it
					if ( ( FluidHX( CompLoop ).ControlMode == HeatingSetPointModulated ) || ( FluidHX( CompLoop ).ControlMode == HeatingSetPointOnOff ) || ( FluidHX( CompLoop ).ControlMode == CoolingSetPointModulated ) || ( FluidHX( CompLoop ).ControlMode == CoolingSetPointOnOff ) || ( FluidHX( CompLoop ).ControlMode == CoolingSetPointOnOffWithComponentOverride ) ) {
						if ( Node( FluidHX( CompLoop ).SetPointNodeNum ).TempSetPoint == SensedNodeFlagValue ) {
							if ( ! AnyEnergyManagementSystemInModel ) {
								ShowSevereError( RoutineName + " Missing temperature setpoint for node = " + cAlphaArgs( 9 ) );
								ShowContinueError( "Occurs for " + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) );
								ShowContinueError( " Use a setpoint manager to place a single temperature setpoint on the node" );
								ErrorsFound = true;
							} else {
								// need call to EMS to check node
								NodeEMSSetPointMissing = false;
								CheckIfNodeSetPointManagedByEMS( FluidHX( CompLoop ).SetPointNodeNum, iTemperatureSetPoint, NodeEMSSetPointMissing );
								if ( NodeEMSSetPointMissing ) {
									ShowSevereError( RoutineName + " Missing temperature setpoint for node = " + cAlphaArgs( 9 ) );
									ShowContinueError( "Occurs for " + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) );
									ShowContinueError( "Use a setpoint manager or EMS actuator to place a single temperature setpoint on the node" );
									ErrorsFound = true;
								}
							}
						}
					} else if ( ( FluidHX( CompLoop ).ControlMode == DualDeadBandSetPointModulated ) || ( FluidHX( CompLoop ).ControlMode == DualDeadBandSetPointOnOff ) ) {
						if ( ( Node( FluidHX( CompLoop ).SetPointNodeNum ).TempSetPointHi == SensedNodeFlagValue ) || ( Node( FluidHX( CompLoop ).SetPointNodeNum ).TempSetPointLo == SensedNodeFlagValue ) ) {
							if ( ! AnyEnergyManagementSystemInModel ) {
								ShowSevereError( RoutineName + " Missing dual temperature setpoints for node = " + cAlphaArgs( 9 ) );
								ShowContinueError( "Occurs for " + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) );
								ShowContinueError( " Use a setpoint manager to place a dual temperature setpoint on the node" );
								ErrorsFound = true;
							} else {
								// need call to EMS to check node
								NodeEMSSetPointMissing = false;
								CheckIfNodeSetPointManagedByEMS( FluidHX( CompLoop ).SetPointNodeNum, iTemperatureMinSetPoint, NodeEMSSetPointMissing );
								CheckIfNodeSetPointManagedByEMS( FluidHX( CompLoop ).SetPointNodeNum, iTemperatureMaxSetPoint, NodeEMSSetPointMissing );
								if ( NodeEMSSetPointMissing ) {
									ShowSevereError( RoutineName + " Missing temperature setpoint for node = " + cAlphaArgs( 9 ) );
									ShowContinueError( "Occurs for " + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) );
									ShowContinueError( "Use a setpoint manager or EMS actuators to place a dual temperature setpoints on the node" );
									ErrorsFound = true;
								}
							}
						}
					}

				} else {
					// need to name a setpoint node if using a setpoint type control mode
					if ( ( FluidHX( CompLoop ).ControlMode == HeatingSetPointModulated ) || ( FluidHX( CompLoop ).ControlMode == HeatingSetPointOnOff ) || ( FluidHX( CompLoop ).ControlMode == CoolingSetPointModulated ) || ( FluidHX( CompLoop ).ControlMode == CoolingSetPointOnOff ) || ( FluidHX( CompLoop ).ControlMode == DualDeadBandSetPointModulated ) || ( FluidHX( CompLoop ).ControlMode == DualDeadBandSetPointOnOff ) || ( FluidHX( CompLoop ).ControlMode == CoolingSetPointOnOffWithComponentOverride ) ) {
						ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid entry." );
						ShowContinueError( "Missing entry for " + cAlphaFieldNames( 9 ) );
						ErrorsFound = true;
					}
				}

				if ( ! lNumericFieldBlanks( 4 ) ) {
					FluidHX( CompLoop ).TempControlTol = rNumericArgs( 4 );
				} else {
					FluidHX( CompLoop ).TempControlTol = 0.01;
				}

				FluidHX( CompLoop ).HeatTransferMeteringEndUse = cAlphaArgs( 10 );

				if ( ! lAlphaFieldBlanks( 11 ) ) {
					FluidHX( CompLoop ).OtherCompSupplySideLoop.InletNodeNum = GetOnlySingleNode( cAlphaArgs( 11 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Actuator, 1, ObjectIsNotParent );
				} else {
					if ( FluidHX( CompLoop ).ControlMode == CoolingSetPointOnOffWithComponentOverride ) {
						ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid entry." );
						ShowContinueError( "Missing entry for " + cAlphaFieldNames( 11 ) );
						ErrorsFound = true;
					}
				}

				if ( ! lAlphaFieldBlanks( 12 ) ) {
					FluidHX( CompLoop ).OtherCompDemandSideLoop.InletNodeNum = GetOnlySingleNode( cAlphaArgs( 12 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Water, NodeConnectionType_Actuator, 1, ObjectIsNotParent );
				} else {
					if ( FluidHX( CompLoop ).ControlMode == CoolingSetPointOnOffWithComponentOverride ) {
						ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid entry." );
						ShowContinueError( "Missing entry for " + cAlphaFieldNames( 12 ) );
						ErrorsFound = true;
					}
				}

				if ( ! lAlphaFieldBlanks( 13 ) ) {
					if ( SameString( cAlphaArgs( 13 ), "WetBulbTemperature" ) ) {
						FluidHX( CompLoop ).ControlSignalTemp = WetBulbTemperature;
					} else if ( SameString( cAlphaArgs( 13 ), "DryBulbTemperature" ) ) {
						FluidHX( CompLoop ).ControlSignalTemp = DryBulbTemperature;
					} else if ( SameString( cAlphaArgs( 13 ), "Loop" ) ) {
						FluidHX( CompLoop ).ControlSignalTemp = LoopTemperature;
					}
				} else {
					if ( FluidHX( CompLoop ).ControlMode == CoolingSetPointOnOffWithComponentOverride ) {
						ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid entry." );
						ShowContinueError( "Missing entry for " + cAlphaFieldNames( 13 ) );
						ErrorsFound = true;
					}

				}

				if ( ! lNumericFieldBlanks( 5 ) ) {
					FluidHX( CompLoop ).SizingFactor = rNumericArgs( 5 );
				} else {
					FluidHX( CompLoop ).SizingFactor = 1.0;
				}

				if ( ! lNumericFieldBlanks( 6 ) ) {
					FluidHX( CompLoop ).MinOperationTemp = rNumericArgs( 6 );
				} else {
					FluidHX( CompLoop ).MinOperationTemp = -9999.0;
				}

				if ( ! lNumericFieldBlanks( 7 ) ) {
					FluidHX( CompLoop ).MaxOperationTemp = rNumericArgs( 7 );
				} else {
					FluidHX( CompLoop ).MaxOperationTemp = 9999.0;
				}

			}
		}

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found in processing " + cCurrentModuleObject + " input." );
		}

		for ( CompLoop = 1; CompLoop <= NumberOfPlantFluidHXs; ++CompLoop ) {

			SetupOutputVariable( "Fluid Heat Exchanger Heat Transfer Rate [W]", FluidHX( CompLoop ).HeatTransferRate, "System", "Average", FluidHX( CompLoop ).Name );

			SetupOutputVariable( "Fluid Heat Exchanger Heat Transfer Energy [J]", FluidHX( CompLoop ).HeatTransferEnergy, "System", "Sum", FluidHX( CompLoop ).Name, _, "ENERGYTRANSFER", FluidHX( CompLoop ).HeatTransferMeteringEndUse, _, "Plant" );

			SetupOutputVariable( "Fluid Heat Exchanger Loop Supply Side Mass Flow Rate [kg/s]", FluidHX( CompLoop ).SupplySideLoop.InletMassFlowRate, "System", "Average", FluidHX( CompLoop ).Name );
			SetupOutputVariable( "Fluid Heat Exchanger Loop Supply Side Inlet Temperature [C]", FluidHX( CompLoop ).SupplySideLoop.InletTemp, "System", "Average", FluidHX( CompLoop ).Name );
			SetupOutputVariable( "Fluid Heat Exchanger Loop Supply Side Outlet Temperature [C]", FluidHX( CompLoop ).SupplySideLoop.OutletTemp, "System", "Average", FluidHX( CompLoop ).Name );
			SetupOutputVariable( "Fluid Heat Exchanger Loop Demand Side Mass Flow Rate [kg/s]", FluidHX( CompLoop ).DemandSideLoop.InletMassFlowRate, "System", "Average", FluidHX( CompLoop ).Name );
			SetupOutputVariable( "Fluid Heat Exchanger Loop Demand Side Inlet Temperature [C]", FluidHX( CompLoop ).DemandSideLoop.InletTemp, "System", "Average", FluidHX( CompLoop ).Name );
			SetupOutputVariable( "Fluid Heat Exchanger Loop Demand Side Outlet Temperature [C]", FluidHX( CompLoop ).DemandSideLoop.OutletTemp, "System", "Average", FluidHX( CompLoop ).Name );
			SetupOutputVariable( "Fluid Heat Exchanger Operation Status [ ]", FluidHX( CompLoop ).OperationStatus, "System", "Average", FluidHX( CompLoop ).Name );
			SetupOutputVariable( "Fluid Heat Exchanger Effectiveness [ ]", FluidHX( CompLoop ).Effectiveness, "System", "Average", FluidHX( CompLoop ).Name );
		}

	}

	void
	InitFluidHeatExchanger(
		int const CompNum,
		int const EP_UNUSED( LoopNum )
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   november, 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Initialize heat exchanger model

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using PlantUtilities::InitComponentNodes;
		using PlantUtilities::InterConnectTwoPlantLoopSides;
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetSpecificHeatGlycol;
		using DataGlobals::BeginEnvrnFlag;
		using DataGlobals::InitConvTemp;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineNameNoColon( "InitFluidHeatExchanger" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool MyOneTimeFlag( true ); // one time flag
		static Array1D_bool MyEnvrnFlag; // environment flag
		static Array1D_bool MyFlag;
		bool errFlag;
		static std::string const RoutineName( "InitFluidHeatExchanger: " );
		Real64 rho;
		int LoopNum2;
		int LoopSideNum;
		int BranchNum;
		int LoopCompNum;

		if ( MyOneTimeFlag ) {
			MyFlag.allocate( NumberOfPlantFluidHXs );
			MyEnvrnFlag.allocate( NumberOfPlantFluidHXs );
			MyFlag = true;
			MyEnvrnFlag = true;
			MyOneTimeFlag = false;
		}

		if ( MyFlag( CompNum ) ) {
			// locate the main two connections to the plant loops
			errFlag = false;
			ScanPlantLoopsForObject( FluidHX( CompNum ).Name, TypeOf_FluidToFluidPlantHtExchg, FluidHX( CompNum ).DemandSideLoop.LoopNum, FluidHX( CompNum ).DemandSideLoop.LoopSideNum, FluidHX( CompNum ).DemandSideLoop.BranchNum, FluidHX( CompNum ).DemandSideLoop.CompNum, _, _, _, FluidHX( CompNum ).DemandSideLoop.InletNodeNum, _, errFlag );

			if ( FluidHX( CompNum ).DemandSideLoop.LoopSideNum != DemandSide ) { // throw error
				ShowSevereError( RoutineName + " Invalid connections for " + ccSimPlantEquipTypes( TypeOf_FluidToFluidPlantHtExchg ) + " name = \"" + FluidHX( CompNum ).Name + "\"" );
				ShowContinueError( "The \"Loop Demand Side\" connections are not on the Demand Side of a plant loop" );
				errFlag = true;
			}

			ScanPlantLoopsForObject( FluidHX( CompNum ).Name, TypeOf_FluidToFluidPlantHtExchg, FluidHX( CompNum ).SupplySideLoop.LoopNum, FluidHX( CompNum ).SupplySideLoop.LoopSideNum, FluidHX( CompNum ).SupplySideLoop.BranchNum, FluidHX( CompNum ).SupplySideLoop.CompNum, _, _, _, FluidHX( CompNum ).SupplySideLoop.InletNodeNum, _, errFlag );

			if ( FluidHX( CompNum ).SupplySideLoop.LoopSideNum != SupplySide ) { // throw error
				ShowSevereError( RoutineName + " Invalid connections for " + ccSimPlantEquipTypes( TypeOf_FluidToFluidPlantHtExchg ) + " name = \"" + FluidHX( CompNum ).Name + "\"" );
				ShowContinueError( "The \"Loop Supply Side\" connections are not on the Supply Side of a plant loop" );
				errFlag = true;
			}

			// make sure it is not the same loop on both sides.
			if ( FluidHX( CompNum ).SupplySideLoop.LoopNum == FluidHX( CompNum ).DemandSideLoop.LoopNum ) { // user is being too tricky, don't allow
				ShowSevereError( RoutineName + " Invalid connections for " + ccSimPlantEquipTypes( TypeOf_FluidToFluidPlantHtExchg ) + " name = \"" + FluidHX( CompNum ).Name + "\"" );
				ShowContinueError( "The \"Loop Supply Side\" and \"Loop Demand Side\" need to be on different loops." );
				errFlag = true;
			} else {

				InterConnectTwoPlantLoopSides( FluidHX( CompNum ).SupplySideLoop.LoopNum, FluidHX( CompNum ).SupplySideLoop.LoopSideNum, FluidHX( CompNum ).DemandSideLoop.LoopNum, FluidHX( CompNum ).DemandSideLoop.LoopSideNum, TypeOf_FluidToFluidPlantHtExchg, true );
			}

			//find remote component if control mode is of that type.
			if ( FluidHX( CompNum ).ControlMode == CoolingSetPointOnOffWithComponentOverride ) {

				ScanPlantLoopsForNodeNum( RoutineName, FluidHX( CompNum ).OtherCompSupplySideLoop.InletNodeNum, FluidHX( CompNum ).OtherCompSupplySideLoop.LoopNum, FluidHX( CompNum ).OtherCompSupplySideLoop.LoopSideNum, FluidHX( CompNum ).OtherCompSupplySideLoop.BranchNum, FluidHX( CompNum ).OtherCompSupplySideLoop.CompNum );

				ScanPlantLoopsForNodeNum( RoutineName, FluidHX( CompNum ).OtherCompDemandSideLoop.InletNodeNum, FluidHX( CompNum ).OtherCompDemandSideLoop.LoopNum, FluidHX( CompNum ).OtherCompDemandSideLoop.LoopSideNum, FluidHX( CompNum ).OtherCompDemandSideLoop.BranchNum, FluidHX( CompNum ).OtherCompDemandSideLoop.CompNum );

				// revise how loads served category for other controlled equipment
				LoopNum2 = FluidHX( CompNum ).OtherCompSupplySideLoop.LoopNum;
				LoopSideNum = FluidHX( CompNum ).OtherCompSupplySideLoop.LoopSideNum;
				BranchNum = FluidHX( CompNum ).OtherCompSupplySideLoop.BranchNum;
				LoopCompNum = FluidHX( CompNum ).OtherCompSupplySideLoop.CompNum;

				{ auto const SELECT_CASE_var( PlantLoop( LoopNum2 ).LoopSide( LoopSideNum ).Branch( BranchNum ).Comp( LoopCompNum ).HowLoadServed );

				if ( SELECT_CASE_var == HowMet_ByNominalCap ) {
					PlantLoop( LoopNum2 ).LoopSide( LoopSideNum ).Branch( BranchNum ).Comp( LoopCompNum ).HowLoadServed = HowMet_ByNominalCapFreeCoolCntrl;
				} else if ( SELECT_CASE_var == HowMet_ByNominalCapLowOutLimit ) {
					PlantLoop( LoopNum2 ).LoopSide( LoopSideNum ).Branch( BranchNum ).Comp( LoopCompNum ).HowLoadServed = HowMet_ByNominalCapLowOutLimitFreeCoolCntrl;
				}}

				{ auto const SELECT_CASE_var( FluidHX( CompNum ).ControlSignalTemp );
				if ( SELECT_CASE_var == WetBulbTemperature ) {
					PlantLoop( LoopNum2 ).LoopSide( LoopSideNum ).Branch( BranchNum ).Comp( LoopCompNum ).FreeCoolCntrlMode = FreeCoolControlMode_WetBulb;
				} else if ( SELECT_CASE_var == DryBulbTemperature ) {
					PlantLoop( LoopNum2 ).LoopSide( LoopSideNum ).Branch( BranchNum ).Comp( LoopCompNum ).FreeCoolCntrlMode = FreeCoolControlMode_DryBulb;
				} else if ( SELECT_CASE_var == LoopTemperature ) {
					PlantLoop( LoopNum2 ).LoopSide( LoopSideNum ).Branch( BranchNum ).Comp( LoopCompNum ).FreeCoolCntrlMode = FreeCoolControlMode_Loop;
					PlantLoop( LoopNum2 ).LoopSide( LoopSideNum ).Branch( BranchNum ).Comp( LoopCompNum ).FreeCoolCntrlNodeNum = FluidHX( CompNum ).OtherCompDemandSideLoop.InletNodeNum;
				}}

			}
			if ( FluidHX( CompNum ).ControlMode == TrackComponentOnOff ) {
				if ( FluidHX( CompNum ).OtherCompSupplySideLoop.InletNodeNum > 0 ) {
					ScanPlantLoopsForObject( FluidHX( CompNum ).ComponentUserName, FluidHX( CompNum ).ComponentTypeOfNum, FluidHX( CompNum ).OtherCompSupplySideLoop.LoopNum, FluidHX( CompNum ).OtherCompSupplySideLoop.LoopSideNum, FluidHX( CompNum ).OtherCompSupplySideLoop.BranchNum, FluidHX( CompNum ).OtherCompSupplySideLoop.CompNum, _, _, _, FluidHX( CompNum ).OtherCompSupplySideLoop.InletNodeNum, _, errFlag );
				}
				if ( FluidHX( CompNum ).OtherCompDemandSideLoop.InletNodeNum > 0 ) {
					ScanPlantLoopsForObject( FluidHX( CompNum ).ComponentUserName, FluidHX( CompNum ).ComponentTypeOfNum, FluidHX( CompNum ).OtherCompDemandSideLoop.LoopNum, FluidHX( CompNum ).OtherCompDemandSideLoop.LoopSideNum, FluidHX( CompNum ).OtherCompDemandSideLoop.BranchNum, FluidHX( CompNum ).OtherCompDemandSideLoop.CompNum, _, _, _, FluidHX( CompNum ).OtherCompDemandSideLoop.InletNodeNum, _, errFlag );
				}
			}

			if ( errFlag ) {
				ShowFatalError( RoutineName + "Program terminated due to previous condition(s)." );
			}
			MyFlag( CompNum ) = false;
		} // plant setup

		if ( BeginEnvrnFlag && MyEnvrnFlag( CompNum ) && ( PlantFirstSizesOkayToFinalize ) ) {

			rho = GetDensityGlycol( PlantLoop( FluidHX( CompNum ).DemandSideLoop.LoopNum ).FluidName, InitConvTemp, PlantLoop( FluidHX( CompNum ).DemandSideLoop.LoopNum ).FluidIndex, RoutineNameNoColon );
			FluidHX( CompNum ).DemandSideLoop.MassFlowRateMax = rho * FluidHX( CompNum ).DemandSideLoop.DesignVolumeFlowRate;
			InitComponentNodes( FluidHX( CompNum ).DemandSideLoop.MassFlowRateMin, FluidHX( CompNum ).DemandSideLoop.MassFlowRateMax, FluidHX( CompNum ).DemandSideLoop.InletNodeNum, FluidHX( CompNum ).DemandSideLoop.OutletNodeNum, FluidHX( CompNum ).DemandSideLoop.LoopNum, FluidHX( CompNum ).DemandSideLoop.LoopSideNum, FluidHX( CompNum ).DemandSideLoop.BranchNum, FluidHX( CompNum ).DemandSideLoop.CompNum );

			rho = GetDensityGlycol( PlantLoop( FluidHX( CompNum ).SupplySideLoop.LoopNum ).FluidName, InitConvTemp, PlantLoop( FluidHX( CompNum ).SupplySideLoop.LoopNum ).FluidIndex, RoutineNameNoColon );
			FluidHX( CompNum ).SupplySideLoop.MassFlowRateMax = rho * FluidHX( CompNum ).SupplySideLoop.DesignVolumeFlowRate;
			InitComponentNodes( FluidHX( CompNum ).SupplySideLoop.MassFlowRateMin, FluidHX( CompNum ).SupplySideLoop.MassFlowRateMax, FluidHX( CompNum ).SupplySideLoop.InletNodeNum, FluidHX( CompNum ).SupplySideLoop.OutletNodeNum, FluidHX( CompNum ).SupplySideLoop.LoopNum, FluidHX( CompNum ).SupplySideLoop.LoopSideNum, FluidHX( CompNum ).SupplySideLoop.BranchNum, FluidHX( CompNum ).SupplySideLoop.CompNum );
			MyEnvrnFlag( CompNum ) = false;
		}
		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag( CompNum ) = true;
		}

		FluidHX( CompNum ).DemandSideLoop.InletTemp = Node( FluidHX( CompNum ).DemandSideLoop.InletNodeNum ).Temp;
		FluidHX( CompNum ).SupplySideLoop.InletTemp = Node( FluidHX( CompNum ).SupplySideLoop.InletNodeNum ).Temp;

		if ( FluidHX( CompNum ).ControlMode == CoolingSetPointOnOffWithComponentOverride ) {
			// store current value for setpoint in central plant loop data structure
			LoopNum2 = FluidHX( CompNum ).OtherCompSupplySideLoop.LoopNum;
			LoopSideNum = FluidHX( CompNum ).OtherCompSupplySideLoop.LoopSideNum;
			BranchNum = FluidHX( CompNum ).OtherCompSupplySideLoop.BranchNum;
			LoopCompNum = FluidHX( CompNum ).OtherCompSupplySideLoop.CompNum;

			PlantLoop( LoopNum2 ).LoopSide( LoopSideNum ).Branch( BranchNum ).Comp( LoopCompNum ).FreeCoolCntrlMinCntrlTemp = Node( FluidHX( CompNum ).SetPointNodeNum ).TempSetPoint;
		}

	}

	void
	SizeFluidHeatExchanger( int const CompNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   December 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Size plant heat exchanger flow rates, UA, and max capacity

		// METHODOLOGY EMPLOYED:
		// the supply side flow rate is obtained from the plant sizing structure
		// the demand side is sized to match the supply side
		// the UA is sized for an effectiveness of 1.0 using sizing temps
		// the capacity uses the full HX model

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataSizing;
		using DataHVACGlobals::SmallWaterVolFlow;
		using DataGlobals::InitConvTemp;
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetSpecificHeatGlycol;
		using OutputReportPredefined::pdchMechType;
		using OutputReportPredefined::pdchMechNomCap;
		using OutputReportPredefined::PreDefTableEntry;
		using PlantUtilities::RegisterPlantCompDesignFlow;
		using ReportSizingManager::ReportSizingOutput;
		using DataPlant::PlantFirstSizesOkayToReport;
		using DataPlant::PlantFinalSizesOkayToReport;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "SizeFluidHeatExchanger" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int PltSizNumSupSide; // Plant Sizing index for Loop Supply Side
		int PltSizNumDmdSide; // plant sizing index for Loop Demand Side
		Real64 tmpSupSideDesignVolFlowRate;
		Real64 tmpDmdSideDesignVolFlowRate;
		Real64 tmpUA;
		Real64 tmpDeltaTSupLoop;
		Real64 tmpDeltaTloopToLoop( 0.0 );
		bool ErrorsFound;
		Real64 Cp;
		Real64 rho;
		Real64 tmpDesCap;
		Real64 SupSideMdot;
		Real64 DmdSideMdot;

		// first deal with Loop Supply Side
		ErrorsFound = false;
		PltSizNumSupSide = PlantLoop( FluidHX( CompNum ).SupplySideLoop.LoopNum ).PlantSizNum;
		PltSizNumDmdSide = PlantLoop( FluidHX( CompNum ).DemandSideLoop.LoopNum ).PlantSizNum;
		tmpSupSideDesignVolFlowRate = FluidHX( CompNum ).SupplySideLoop.DesignVolumeFlowRate;
		if ( FluidHX( CompNum ).SupplySideLoop.DesignVolumeFlowRateWasAutoSized ) {
			if ( PltSizNumSupSide > 0 ) {
				if ( PlantSizData( PltSizNumSupSide ).DesVolFlowRate >= SmallWaterVolFlow ) {
					tmpSupSideDesignVolFlowRate = PlantSizData( PltSizNumSupSide ).DesVolFlowRate * FluidHX( CompNum ).SizingFactor;
					if ( PlantFirstSizesOkayToFinalize ) FluidHX( CompNum ).SupplySideLoop.DesignVolumeFlowRate = tmpSupSideDesignVolFlowRate;
				} else {
					tmpSupSideDesignVolFlowRate = 0.0;
					if ( PlantFirstSizesOkayToFinalize ) FluidHX( CompNum ).SupplySideLoop.DesignVolumeFlowRate = tmpSupSideDesignVolFlowRate;
				}
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( "HeatExchanger:FluidToFluid", FluidHX( CompNum ).Name,
						"Loop Supply Side Design Fluid Flow Rate [m3/s]", FluidHX( CompNum ).SupplySideLoop.DesignVolumeFlowRate );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( "HeatExchanger:FluidToFluid", FluidHX( CompNum ).Name,
						"Initial Loop Supply Side Design Fluid Flow Rate [m3/s]", FluidHX( CompNum ).SupplySideLoop.DesignVolumeFlowRate );
				}
			} else {
				if ( PlantFirstSizesOkayToFinalize ) {
					ShowSevereError( "SizeFluidHeatExchanger: Autosizing of requires a loop Sizing:Plant object" );
					ShowContinueError( "Occurs in heat exchanger object=" + FluidHX( CompNum ).Name );
					ErrorsFound = true;
				}
			}
		}
		RegisterPlantCompDesignFlow( FluidHX( CompNum ).SupplySideLoop.InletNodeNum, tmpSupSideDesignVolFlowRate );

		// second deal with Loop Demand Side
		tmpDmdSideDesignVolFlowRate = FluidHX( CompNum ).DemandSideLoop.DesignVolumeFlowRate;
		if ( FluidHX( CompNum ).DemandSideLoop.DesignVolumeFlowRateWasAutoSized ) {
			if ( tmpSupSideDesignVolFlowRate > SmallWaterVolFlow ) {
				tmpDmdSideDesignVolFlowRate = tmpSupSideDesignVolFlowRate;
				if ( PlantFirstSizesOkayToFinalize ) FluidHX( CompNum ).DemandSideLoop.DesignVolumeFlowRate = tmpDmdSideDesignVolFlowRate;
			} else {
				tmpDmdSideDesignVolFlowRate = 0.0;
				if ( PlantFirstSizesOkayToFinalize ) FluidHX( CompNum ).DemandSideLoop.DesignVolumeFlowRate = tmpDmdSideDesignVolFlowRate;
			}
			if ( PlantFinalSizesOkayToReport ) {
				ReportSizingOutput( "HeatExchanger:FluidToFluid", FluidHX( CompNum ).Name,
					"Loop Demand Side Design Fluid Flow Rate [m3/s]", FluidHX( CompNum ).DemandSideLoop.DesignVolumeFlowRate );
			}
			if ( PlantFirstSizesOkayToReport ) {
				ReportSizingOutput( "HeatExchanger:FluidToFluid", FluidHX( CompNum ).Name,
					"Initial Loop Demand Side Design Fluid Flow Rate [m3/s]", FluidHX( CompNum ).DemandSideLoop.DesignVolumeFlowRate );
			}
		}
		RegisterPlantCompDesignFlow( FluidHX( CompNum ).DemandSideLoop.InletNodeNum, tmpDmdSideDesignVolFlowRate );

		// size UA if needed
		tmpUA = FluidHX( CompNum ).UA;
		if ( FluidHX( CompNum ).UAWasAutoSized ) {
			// get nominal delta T between two loops
			if ( PltSizNumSupSide > 0 && PltSizNumDmdSide > 0 ) {

				{ auto const SELECT_CASE_var( PlantSizData( PltSizNumSupSide ).LoopType );

				if ( SELECT_CASE_var == HeatingLoop ) {
					tmpDeltaTloopToLoop = std::abs( ( PlantSizData( PltSizNumSupSide ).ExitTemp - PlantSizData( PltSizNumSupSide ).DeltaT ) - PlantSizData( PltSizNumDmdSide ).ExitTemp );
				} else if ( SELECT_CASE_var == CoolingLoop ) {
					tmpDeltaTloopToLoop = std::abs( ( PlantSizData( PltSizNumSupSide ).ExitTemp + PlantSizData( PltSizNumSupSide ).DeltaT ) - PlantSizData( PltSizNumDmdSide ).ExitTemp );
				} else if ( SELECT_CASE_var == CondenserLoop ) {
					tmpDeltaTloopToLoop = std::abs( ( PlantSizData( PltSizNumSupSide ).ExitTemp + PlantSizData( PltSizNumSupSide ).DeltaT ) - PlantSizData( PltSizNumDmdSide ).ExitTemp );
				} else if ( SELECT_CASE_var == SteamLoop ) {
					tmpDeltaTloopToLoop = std::abs( ( PlantSizData( PltSizNumSupSide ).ExitTemp - PlantSizData( PltSizNumSupSide ).DeltaT ) - PlantSizData( PltSizNumDmdSide ).ExitTemp );
				} else {
					assert( false );
				}}

				tmpDeltaTloopToLoop = max( 2.0, tmpDeltaTloopToLoop );
				tmpDeltaTSupLoop = PlantSizData( PltSizNumSupSide ).DeltaT;
				if ( tmpSupSideDesignVolFlowRate >= SmallWaterVolFlow ) {

					Cp = GetSpecificHeatGlycol( PlantLoop( FluidHX( CompNum ).SupplySideLoop.LoopNum ).FluidName, InitConvTemp, PlantLoop( FluidHX( CompNum ).SupplySideLoop.LoopNum ).FluidIndex, RoutineName );

					rho = GetDensityGlycol( PlantLoop( FluidHX( CompNum ).SupplySideLoop.LoopNum ).FluidName, InitConvTemp, PlantLoop( FluidHX( CompNum ).SupplySideLoop.LoopNum ).FluidIndex, RoutineName );

					tmpDesCap = Cp * rho * tmpDeltaTSupLoop * tmpSupSideDesignVolFlowRate;
					tmpUA = tmpDesCap / tmpDeltaTloopToLoop;
					if ( PlantFirstSizesOkayToFinalize ) FluidHX( CompNum ).UA = tmpUA;
				} else {
					tmpUA = 0.0;
					if ( PlantFirstSizesOkayToFinalize ) FluidHX( CompNum ).UA = tmpUA;
				}
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( "HeatExchanger:FluidToFluid", FluidHX( CompNum ).Name,
						"Heat Exchanger U-Factor Times Area Value [W/C]", FluidHX( CompNum ).UA );
					ReportSizingOutput( "HeatExchanger:FluidToFluid", FluidHX( CompNum ).Name,
						"Loop-to-loop Temperature Difference Used to Size Heat Exchanger U-Factor Times Area Value [C]", tmpDeltaTloopToLoop );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( "HeatExchanger:FluidToFluid", FluidHX( CompNum ).Name,
						"Initial Heat Exchanger U-Factor Times Area Value [W/C]", FluidHX( CompNum ).UA );
					ReportSizingOutput( "HeatExchanger:FluidToFluid", FluidHX( CompNum ).Name,
						"Initial Loop-to-loop Temperature Difference Used to Size Heat Exchanger U-Factor Times Area Value [C]", tmpDeltaTloopToLoop );
				}
			} else {
				if ( PlantFirstSizesOkayToFinalize ) {
					ShowSevereError( "SizeFluidHeatExchanger: Autosizing of heat Exchanger UA requires a loop Sizing:Plant objects for both loops" );
					ShowContinueError( "Occurs in heat exchanger object=" + FluidHX( CompNum ).Name );
					ErrorsFound = true;
				}
			}

		}

		// size capacities for load range based op schemes
		if ( PlantFirstSizesOkayToFinalize ) {

			if ( PltSizNumSupSide > 0 ) {
				{ auto const SELECT_CASE_var( PlantSizData( PltSizNumSupSide ).LoopType );
				if ( SELECT_CASE_var == HeatingLoop ) {
					Node( FluidHX( CompNum ).SupplySideLoop.InletNodeNum ).Temp = ( PlantSizData( PltSizNumSupSide ).ExitTemp - PlantSizData( PltSizNumSupSide ).DeltaT );
				} else if ( SELECT_CASE_var == CoolingLoop ) {
					Node( FluidHX( CompNum ).SupplySideLoop.InletNodeNum ).Temp = ( PlantSizData( PltSizNumSupSide ).ExitTemp + PlantSizData( PltSizNumSupSide ).DeltaT );
				} else if ( SELECT_CASE_var == CondenserLoop ) {
					Node( FluidHX( CompNum ).SupplySideLoop.InletNodeNum ).Temp = ( PlantSizData( PltSizNumSupSide ).ExitTemp + PlantSizData( PltSizNumSupSide ).DeltaT );
				} else if ( SELECT_CASE_var == SteamLoop ) {
					Node( FluidHX( CompNum ).SupplySideLoop.InletNodeNum ).Temp = ( PlantSizData( PltSizNumSupSide ).ExitTemp - PlantSizData( PltSizNumSupSide ).DeltaT );
				}}

			} else { // don't rely on sizing, use loop setpoints
				// loop supply side
				if ( PlantLoop( FluidHX( CompNum ).SupplySideLoop.LoopNum ).LoopDemandCalcScheme == SingleSetPoint ) {
					Node( FluidHX( CompNum ).SupplySideLoop.InletNodeNum ).Temp = Node( PlantLoop( FluidHX( CompNum ).SupplySideLoop.LoopNum ).TempSetPointNodeNum ).TempSetPoint;
				} else if ( PlantLoop( FluidHX( CompNum ).SupplySideLoop.LoopNum ).LoopDemandCalcScheme == DualSetPointDeadBand ) {
					Node( FluidHX( CompNum ).SupplySideLoop.InletNodeNum ).Temp = ( Node( PlantLoop( FluidHX( CompNum ).SupplySideLoop.LoopNum ).TempSetPointNodeNum ).TempSetPointHi + Node( PlantLoop( FluidHX( CompNum ).SupplySideLoop.LoopNum ).TempSetPointNodeNum ).TempSetPointLo ) / 2.0;
				}

			}

			if ( PltSizNumDmdSide > 0 ) {
				Node( FluidHX( CompNum ).DemandSideLoop.InletNodeNum ).Temp = PlantSizData( PltSizNumDmdSide ).ExitTemp;
			} else { // don't rely on sizing, use loop setpoints
				// loop demand side
				if ( PlantLoop( FluidHX( CompNum ).DemandSideLoop.LoopNum ).LoopDemandCalcScheme == SingleSetPoint ) {
					Node( FluidHX( CompNum ).DemandSideLoop.InletNodeNum ).Temp = Node( PlantLoop( FluidHX( CompNum ).DemandSideLoop.LoopNum ).TempSetPointNodeNum ).TempSetPoint;
				} else if ( PlantLoop( FluidHX( CompNum ).DemandSideLoop.LoopNum ).LoopDemandCalcScheme == DualSetPointDeadBand ) {
					Node( FluidHX( CompNum ).DemandSideLoop.InletNodeNum ).Temp = ( Node( PlantLoop( FluidHX( CompNum ).DemandSideLoop.LoopNum ).TempSetPointNodeNum ).TempSetPointHi + Node( PlantLoop( FluidHX( CompNum ).DemandSideLoop.LoopNum ).TempSetPointNodeNum ).TempSetPointLo ) / 2.0;
				}
			}

			rho = GetDensityGlycol( PlantLoop( FluidHX( CompNum ).SupplySideLoop.LoopNum ).FluidName, InitConvTemp, PlantLoop( FluidHX( CompNum ).SupplySideLoop.LoopNum ).FluidIndex, RoutineName );
			SupSideMdot = FluidHX( CompNum ).SupplySideLoop.DesignVolumeFlowRate * rho;
			rho = GetDensityGlycol( PlantLoop( FluidHX( CompNum ).DemandSideLoop.LoopNum ).FluidName, InitConvTemp, PlantLoop( FluidHX( CompNum ).DemandSideLoop.LoopNum ).FluidIndex, RoutineName );
			DmdSideMdot = FluidHX( CompNum ).DemandSideLoop.DesignVolumeFlowRate * rho;

			CalcFluidHeatExchanger( CompNum, SupSideMdot, DmdSideMdot );
			FluidHX( CompNum ).SupplySideLoop.MaxLoad = std::abs( FluidHX( CompNum ).HeatTransferRate );

		}
		if ( PlantFinalSizesOkayToReport ) {
			PreDefTableEntry( pdchMechType, FluidHX( CompNum ).Name, "HeatExchanger:FluidToFluid" );
			PreDefTableEntry( pdchMechNomCap, FluidHX( CompNum ).Name, FluidHX( CompNum ).SupplySideLoop.MaxLoad );
		}

	}

	void
	ControlFluidHeatExchanger(
		int const CompNum,
		int const EP_UNUSED( LoopNum ),
		Real64 const MyLoad,
		bool FirstHVACIteration
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   November 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// determine control state for fluid to fluid heat exchanger
		// make fluid flow requests accordingly

		// METHODOLOGY EMPLOYED:
		// long CASE statement for different control options

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataBranchAirLoopPlant::MassFlowTolerance;
		using ScheduleManager::GetCurrentScheduleValue;
		using DataHVACGlobals::SmallLoad;
		using FluidProperties::GetSpecificHeatGlycol;
		using PlantUtilities::SetComponentFlowRate;
		using DataEnvironment::OutDryBulbTemp;
		using DataEnvironment::OutWetBulbTemp;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "ControlFluidHeatExchanger" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 AvailSchedValue;
		bool ScheduledOff;
		bool LimitTrippedOff;
		Real64 mdotSupSide;
		Real64 mdotDmdSide;
		Real64 DeltaTCooling;
		Real64 DeltaTHeating;
		Real64 DeltaTCoolSetPointDemand;
		Real64 DeltaTCoolSetPointSupply;
		Real64 DeltaTHeatSetPointDemand;
		Real64 DeltaTHeatSetPointSupply;
		Real64 cp; // specific heat of fluid
		Real64 TargetLeavingTemp; // target temperature deg. C
		Real64 SetPointTemp; // temperature setpoint for single setpoint
		Real64 SetPointTempLo; // low setpoint for dual deadband temperature setpoints
		Real64 SetPointTempHi; // High setpoint for dual deadband temperature setpoints
		Real64 ControlSignalValue( 0.0 );
		bool ChillerShutDown;

		// check if available by schedule
		AvailSchedValue = GetCurrentScheduleValue( FluidHX( CompNum ).AvailSchedNum );
		if ( AvailSchedValue <= 0 ) {
			ScheduledOff = true;
		} else {
			ScheduledOff = false;
		}

		// check if operational limits trip off unit
		LimitTrippedOff = false;
		if ( ( Node( FluidHX( CompNum ).SupplySideLoop.InletNodeNum ).Temp < FluidHX( CompNum ).MinOperationTemp ) || ( Node( FluidHX( CompNum ).DemandSideLoop.InletNodeNum ).Temp < FluidHX( CompNum ).MinOperationTemp ) ) {
			LimitTrippedOff = true;
		}
		if ( ( Node( FluidHX( CompNum ).SupplySideLoop.InletNodeNum ).Temp > FluidHX( CompNum ).MaxOperationTemp ) || ( Node( FluidHX( CompNum ).DemandSideLoop.InletNodeNum ).Temp > FluidHX( CompNum ).MaxOperationTemp ) ) {
			LimitTrippedOff = true;
		}

		if ( ! ScheduledOff && ! LimitTrippedOff ) {

			{ auto const SELECT_CASE_var( FluidHX( CompNum ).ControlMode );

			if ( SELECT_CASE_var == UncontrolledOn ) {

				// make passive request for supply side loop flow
				mdotSupSide = FluidHX( CompNum ).SupplySideLoop.MassFlowRateMax;
				SetComponentFlowRate( mdotSupSide, FluidHX( CompNum ).SupplySideLoop.InletNodeNum, FluidHX( CompNum ).SupplySideLoop.OutletNodeNum, FluidHX( CompNum ).SupplySideLoop.LoopNum, FluidHX( CompNum ).SupplySideLoop.LoopSideNum, FluidHX( CompNum ).SupplySideLoop.BranchNum, FluidHX( CompNum ).SupplySideLoop.CompNum );
				if ( mdotSupSide > MassFlowTolerance ) {
					// if supply side loop has massflow, request demand side flow
					mdotDmdSide = FluidHX( CompNum ).DemandSideLoop.MassFlowRateMax;
				} else {
					mdotDmdSide = 0.0;
				}
				SetComponentFlowRate( mdotDmdSide, FluidHX( CompNum ).DemandSideLoop.InletNodeNum, FluidHX( CompNum ).DemandSideLoop.OutletNodeNum, FluidHX( CompNum ).DemandSideLoop.LoopNum, FluidHX( CompNum ).DemandSideLoop.LoopSideNum, FluidHX( CompNum ).DemandSideLoop.BranchNum, FluidHX( CompNum ).DemandSideLoop.CompNum );

			} else if ( SELECT_CASE_var == OperationSchemeModulated ) {

				if ( std::abs( MyLoad ) > SmallLoad ) {
					if ( MyLoad < -1.0 * SmallLoad ) { // requesting cooling
						DeltaTCooling = FluidHX( CompNum ).SupplySideLoop.InletTemp - FluidHX( CompNum ).DemandSideLoop.InletTemp;
						if ( DeltaTCooling > FluidHX( CompNum ).TempControlTol ) { // can do cooling so turn on
							mdotSupSide = FluidHX( CompNum ).SupplySideLoop.MassFlowRateMax;
							SetComponentFlowRate( mdotSupSide, FluidHX( CompNum ).SupplySideLoop.InletNodeNum, FluidHX( CompNum ).SupplySideLoop.OutletNodeNum, FluidHX( CompNum ).SupplySideLoop.LoopNum, FluidHX( CompNum ).SupplySideLoop.LoopSideNum, FluidHX( CompNum ).SupplySideLoop.BranchNum, FluidHX( CompNum ).SupplySideLoop.CompNum );
							if ( mdotSupSide > MassFlowTolerance ) {
								// if supply side loop has massflow, request demand side flow
								cp = GetSpecificHeatGlycol( PlantLoop( FluidHX( CompNum ).SupplySideLoop.LoopNum ).FluidName, FluidHX( CompNum ).SupplySideLoop.InletTemp, PlantLoop( FluidHX( CompNum ).SupplySideLoop.LoopNum ).FluidIndex, RoutineName );
								TargetLeavingTemp = FluidHX( CompNum ).SupplySideLoop.InletTemp - std::abs( MyLoad ) / ( cp * mdotSupSide );

								FindHXDemandSideLoopFlow( CompNum, TargetLeavingTemp, CoolingSupplySideLoop );
							} else { // no flow on supply side so do not request flow on demand side
								mdotDmdSide = 0.0;
								SetComponentFlowRate( mdotDmdSide, FluidHX( CompNum ).DemandSideLoop.InletNodeNum, FluidHX( CompNum ).DemandSideLoop.OutletNodeNum, FluidHX( CompNum ).DemandSideLoop.LoopNum, FluidHX( CompNum ).DemandSideLoop.LoopSideNum, FluidHX( CompNum ).DemandSideLoop.BranchNum, FluidHX( CompNum ).SupplySideLoop.CompNum );
							}
						} else { // not able to cool so turn off
							mdotSupSide = 0.0;
							SetComponentFlowRate( mdotSupSide, FluidHX( CompNum ).SupplySideLoop.InletNodeNum, FluidHX( CompNum ).SupplySideLoop.OutletNodeNum, FluidHX( CompNum ).SupplySideLoop.LoopNum, FluidHX( CompNum ).SupplySideLoop.LoopSideNum, FluidHX( CompNum ).SupplySideLoop.BranchNum, FluidHX( CompNum ).SupplySideLoop.CompNum );
							//issue 4959, make demand side flow request on first hvac iteration so demand side loop can run as a trial to get a fresh demand side inlet temperature value
							if ( FirstHVACIteration ) {
								mdotDmdSide = FluidHX( CompNum ).DemandSideLoop.MassFlowRateMax;
							} else {
								mdotDmdSide = 0.0;
							}
							SetComponentFlowRate( mdotDmdSide, FluidHX( CompNum ).DemandSideLoop.InletNodeNum, FluidHX( CompNum ).DemandSideLoop.OutletNodeNum, FluidHX( CompNum ).DemandSideLoop.LoopNum, FluidHX( CompNum ).DemandSideLoop.LoopSideNum, FluidHX( CompNum ).DemandSideLoop.BranchNum, FluidHX( CompNum ).SupplySideLoop.CompNum );
						}

					} else { // requesting heating
						DeltaTHeating = FluidHX( CompNum ).DemandSideLoop.InletTemp - FluidHX( CompNum ).SupplySideLoop.InletTemp;
						if ( DeltaTHeating > FluidHX( CompNum ).TempControlTol ) { // can do heating so turn on
							mdotSupSide = FluidHX( CompNum ).SupplySideLoop.MassFlowRateMax;
							SetComponentFlowRate( mdotSupSide, FluidHX( CompNum ).SupplySideLoop.InletNodeNum, FluidHX( CompNum ).SupplySideLoop.OutletNodeNum, FluidHX( CompNum ).SupplySideLoop.LoopNum, FluidHX( CompNum ).SupplySideLoop.LoopSideNum, FluidHX( CompNum ).SupplySideLoop.BranchNum, FluidHX( CompNum ).SupplySideLoop.CompNum );
							if ( mdotSupSide > MassFlowTolerance ) {
								cp = GetSpecificHeatGlycol( PlantLoop( FluidHX( CompNum ).SupplySideLoop.LoopNum ).FluidName, FluidHX( CompNum ).SupplySideLoop.InletTemp, PlantLoop( FluidHX( CompNum ).SupplySideLoop.LoopNum ).FluidIndex, RoutineName );
								TargetLeavingTemp = FluidHX( CompNum ).SupplySideLoop.InletTemp + std::abs( MyLoad ) / ( cp * mdotSupSide );

								FindHXDemandSideLoopFlow( CompNum, TargetLeavingTemp, HeatingSupplySideLoop );
							} else { // no flow on supply side so do not request flow on demand side
								mdotDmdSide = 0.0;
								SetComponentFlowRate( mdotDmdSide, FluidHX( CompNum ).DemandSideLoop.InletNodeNum, FluidHX( CompNum ).DemandSideLoop.OutletNodeNum, FluidHX( CompNum ).DemandSideLoop.LoopNum, FluidHX( CompNum ).DemandSideLoop.LoopSideNum, FluidHX( CompNum ).DemandSideLoop.BranchNum, FluidHX( CompNum ).DemandSideLoop.CompNum );
							}
						} else { // not able to heat so turn off
							mdotSupSide = 0.0;
							SetComponentFlowRate( mdotSupSide, FluidHX( CompNum ).SupplySideLoop.InletNodeNum, FluidHX( CompNum ).SupplySideLoop.OutletNodeNum, FluidHX( CompNum ).SupplySideLoop.LoopNum, FluidHX( CompNum ).SupplySideLoop.LoopSideNum, FluidHX( CompNum ).SupplySideLoop.BranchNum, FluidHX( CompNum ).SupplySideLoop.CompNum );
							//issue 4959, make demand side flow request on first hvac iteration so demand side loop can run as a trial to get a fresh demand side inlet temperature value
							if ( FirstHVACIteration ) {
								mdotDmdSide = FluidHX( CompNum ).DemandSideLoop.MassFlowRateMax;
							} else {
								mdotDmdSide = 0.0;
							}
							SetComponentFlowRate( mdotDmdSide, FluidHX( CompNum ).DemandSideLoop.InletNodeNum, FluidHX( CompNum ).DemandSideLoop.OutletNodeNum, FluidHX( CompNum ).DemandSideLoop.LoopNum, FluidHX( CompNum ).DemandSideLoop.LoopSideNum, FluidHX( CompNum ).DemandSideLoop.BranchNum, FluidHX( CompNum ).DemandSideLoop.CompNum );
						}
					}

				} else { //  no load
					mdotSupSide = 0.0;
					SetComponentFlowRate( mdotSupSide, FluidHX( CompNum ).SupplySideLoop.InletNodeNum, FluidHX( CompNum ).SupplySideLoop.OutletNodeNum, FluidHX( CompNum ).SupplySideLoop.LoopNum, FluidHX( CompNum ).SupplySideLoop.LoopSideNum, FluidHX( CompNum ).SupplySideLoop.BranchNum, FluidHX( CompNum ).SupplySideLoop.CompNum );
					mdotDmdSide = 0.0;
					SetComponentFlowRate( mdotDmdSide, FluidHX( CompNum ).DemandSideLoop.InletNodeNum, FluidHX( CompNum ).DemandSideLoop.OutletNodeNum, FluidHX( CompNum ).DemandSideLoop.LoopNum, FluidHX( CompNum ).DemandSideLoop.LoopSideNum, FluidHX( CompNum ).DemandSideLoop.BranchNum, FluidHX( CompNum ).DemandSideLoop.CompNum );
				}

			} else if ( SELECT_CASE_var == OperationSchemeOnOff ) {
				if ( std::abs( MyLoad ) > SmallLoad ) {
					if ( MyLoad < SmallLoad ) { // requesting cooling
						DeltaTCooling = FluidHX( CompNum ).SupplySideLoop.InletTemp - FluidHX( CompNum ).DemandSideLoop.InletTemp;
						if ( DeltaTCooling > FluidHX( CompNum ).TempControlTol ) { // can do cooling so turn on
							mdotSupSide = FluidHX( CompNum ).SupplySideLoop.MassFlowRateMax;
							SetComponentFlowRate( mdotSupSide, FluidHX( CompNum ).SupplySideLoop.InletNodeNum, FluidHX( CompNum ).SupplySideLoop.OutletNodeNum, FluidHX( CompNum ).SupplySideLoop.LoopNum, FluidHX( CompNum ).SupplySideLoop.LoopSideNum, FluidHX( CompNum ).SupplySideLoop.BranchNum, FluidHX( CompNum ).SupplySideLoop.CompNum );
							if ( mdotSupSide > MassFlowTolerance ) {
								mdotDmdSide = FluidHX( CompNum ).DemandSideLoop.MassFlowRateMax;
							} else {
								mdotDmdSide = 0.0;
							}

							SetComponentFlowRate( mdotDmdSide, FluidHX( CompNum ).DemandSideLoop.InletNodeNum, FluidHX( CompNum ).DemandSideLoop.OutletNodeNum, FluidHX( CompNum ).DemandSideLoop.LoopNum, FluidHX( CompNum ).DemandSideLoop.LoopSideNum, FluidHX( CompNum ).DemandSideLoop.BranchNum, FluidHX( CompNum ).DemandSideLoop.CompNum );
						} else { // not able to cool so turn off
							mdotSupSide = 0.0;
							SetComponentFlowRate( mdotSupSide, FluidHX( CompNum ).SupplySideLoop.InletNodeNum, FluidHX( CompNum ).SupplySideLoop.OutletNodeNum, FluidHX( CompNum ).SupplySideLoop.LoopNum, FluidHX( CompNum ).SupplySideLoop.LoopSideNum, FluidHX( CompNum ).SupplySideLoop.BranchNum, FluidHX( CompNum ).SupplySideLoop.CompNum );
							//issue 4959, make demand side flow request on first hvac iteration so demand side loop can run as a trial to get a fresh demand side inlet temperature value
							if ( FirstHVACIteration ) {
								mdotDmdSide = FluidHX( CompNum ).DemandSideLoop.MassFlowRateMax;
							} else {
								mdotDmdSide = 0.0;
							}
							SetComponentFlowRate( mdotDmdSide, FluidHX( CompNum ).DemandSideLoop.InletNodeNum, FluidHX( CompNum ).DemandSideLoop.OutletNodeNum, FluidHX( CompNum ).DemandSideLoop.LoopNum, FluidHX( CompNum ).DemandSideLoop.LoopSideNum, FluidHX( CompNum ).DemandSideLoop.BranchNum, FluidHX( CompNum ).DemandSideLoop.CompNum );
						}

					} else { // requesting heating
						DeltaTHeating = FluidHX( CompNum ).DemandSideLoop.InletTemp - FluidHX( CompNum ).SupplySideLoop.InletTemp;
						if ( DeltaTHeating > FluidHX( CompNum ).TempControlTol ) { // can do heating so turn on
							mdotSupSide = FluidHX( CompNum ).SupplySideLoop.MassFlowRateMax;
							SetComponentFlowRate( mdotSupSide, FluidHX( CompNum ).SupplySideLoop.InletNodeNum, FluidHX( CompNum ).SupplySideLoop.OutletNodeNum, FluidHX( CompNum ).SupplySideLoop.LoopNum, FluidHX( CompNum ).SupplySideLoop.LoopSideNum, FluidHX( CompNum ).SupplySideLoop.BranchNum, FluidHX( CompNum ).SupplySideLoop.CompNum );
							if ( mdotSupSide > MassFlowTolerance ) {
								mdotDmdSide = FluidHX( CompNum ).DemandSideLoop.MassFlowRateMax;
							} else {
								mdotDmdSide = 0.0;
							}
							SetComponentFlowRate( mdotDmdSide, FluidHX( CompNum ).DemandSideLoop.InletNodeNum, FluidHX( CompNum ).DemandSideLoop.OutletNodeNum, FluidHX( CompNum ).DemandSideLoop.LoopNum, FluidHX( CompNum ).DemandSideLoop.LoopSideNum, FluidHX( CompNum ).DemandSideLoop.BranchNum, FluidHX( CompNum ).DemandSideLoop.CompNum );
						} else { // not able to heat so turn off
							mdotSupSide = 0.0;
							SetComponentFlowRate( mdotSupSide, FluidHX( CompNum ).SupplySideLoop.InletNodeNum, FluidHX( CompNum ).SupplySideLoop.OutletNodeNum, FluidHX( CompNum ).SupplySideLoop.LoopNum, FluidHX( CompNum ).SupplySideLoop.LoopSideNum, FluidHX( CompNum ).SupplySideLoop.BranchNum, FluidHX( CompNum ).SupplySideLoop.CompNum );
							//issue 4959, make demand side flow request on first hvac iteration so demand side loop can run as a trial to get a fresh demand side inlet temperature value
							if ( FirstHVACIteration ) {
								mdotDmdSide = FluidHX( CompNum ).DemandSideLoop.MassFlowRateMax;
							} else {
								mdotDmdSide = 0.0;
							}
							SetComponentFlowRate( mdotDmdSide, FluidHX( CompNum ).DemandSideLoop.InletNodeNum, FluidHX( CompNum ).DemandSideLoop.OutletNodeNum, FluidHX( CompNum ).DemandSideLoop.LoopNum, FluidHX( CompNum ).DemandSideLoop.LoopSideNum, FluidHX( CompNum ).DemandSideLoop.BranchNum, FluidHX( CompNum ).DemandSideLoop.CompNum );
						}
					}

				} else { // no load
					mdotSupSide = 0.0;
					SetComponentFlowRate( mdotSupSide, FluidHX( CompNum ).SupplySideLoop.InletNodeNum, FluidHX( CompNum ).SupplySideLoop.OutletNodeNum, FluidHX( CompNum ).SupplySideLoop.LoopNum, FluidHX( CompNum ).SupplySideLoop.LoopSideNum, FluidHX( CompNum ).SupplySideLoop.BranchNum, FluidHX( CompNum ).SupplySideLoop.CompNum );
					mdotDmdSide = 0.0;
					SetComponentFlowRate( mdotDmdSide, FluidHX( CompNum ).DemandSideLoop.InletNodeNum, FluidHX( CompNum ).DemandSideLoop.OutletNodeNum, FluidHX( CompNum ).DemandSideLoop.LoopNum, FluidHX( CompNum ).DemandSideLoop.LoopSideNum, FluidHX( CompNum ).DemandSideLoop.BranchNum, FluidHX( CompNum ).DemandSideLoop.CompNum );
				}

			} else if ( SELECT_CASE_var == HeatingSetPointModulated ) {

				SetPointTemp = Node( FluidHX( CompNum ).SetPointNodeNum ).TempSetPoint;
				DeltaTHeating = FluidHX( CompNum ).DemandSideLoop.InletTemp - FluidHX( CompNum ).SupplySideLoop.InletTemp;
				if ( ( DeltaTHeating > FluidHX( CompNum ).TempControlTol ) && ( SetPointTemp > FluidHX( CompNum ).SupplySideLoop.InletTemp ) ) {
					// can and want to heat
					mdotSupSide = FluidHX( CompNum ).SupplySideLoop.MassFlowRateMax;
					SetComponentFlowRate( mdotSupSide, FluidHX( CompNum ).SupplySideLoop.InletNodeNum, FluidHX( CompNum ).SupplySideLoop.OutletNodeNum, FluidHX( CompNum ).SupplySideLoop.LoopNum, FluidHX( CompNum ).SupplySideLoop.LoopSideNum, FluidHX( CompNum ).SupplySideLoop.BranchNum, FluidHX( CompNum ).SupplySideLoop.CompNum );
					if ( mdotSupSide > MassFlowTolerance ) {

						TargetLeavingTemp = SetPointTemp;
						FindHXDemandSideLoopFlow( CompNum, TargetLeavingTemp, HeatingSupplySideLoop );
					} else {
						mdotDmdSide = 0.0;
						SetComponentFlowRate( mdotDmdSide, FluidHX( CompNum ).DemandSideLoop.InletNodeNum, FluidHX( CompNum ).DemandSideLoop.OutletNodeNum, FluidHX( CompNum ).DemandSideLoop.LoopNum, FluidHX( CompNum ).DemandSideLoop.LoopSideNum, FluidHX( CompNum ).DemandSideLoop.BranchNum, FluidHX( CompNum ).DemandSideLoop.CompNum );
					}
				} else { // not able are wanting to heat so turn off
					mdotSupSide = 0.0;
					SetComponentFlowRate( mdotSupSide, FluidHX( CompNum ).SupplySideLoop.InletNodeNum, FluidHX( CompNum ).SupplySideLoop.OutletNodeNum, FluidHX( CompNum ).SupplySideLoop.LoopNum, FluidHX( CompNum ).SupplySideLoop.LoopSideNum, FluidHX( CompNum ).SupplySideLoop.BranchNum, FluidHX( CompNum ).SupplySideLoop.CompNum );
					//issue 4959, make demand side flow request on first hvac iteration so demand side loop can run as a trial to get a fresh demand side inlet temperature value
					if ( FirstHVACIteration ) {
						mdotDmdSide = FluidHX( CompNum ).DemandSideLoop.MassFlowRateMax;
					} else {
						mdotDmdSide = 0.0;
					}
					SetComponentFlowRate( mdotDmdSide, FluidHX( CompNum ).DemandSideLoop.InletNodeNum, FluidHX( CompNum ).DemandSideLoop.OutletNodeNum, FluidHX( CompNum ).DemandSideLoop.LoopNum, FluidHX( CompNum ).DemandSideLoop.LoopSideNum, FluidHX( CompNum ).DemandSideLoop.BranchNum, FluidHX( CompNum ).DemandSideLoop.CompNum );
				}

			} else if ( SELECT_CASE_var == HeatingSetPointOnOff ) {

				SetPointTemp = Node( FluidHX( CompNum ).SetPointNodeNum ).TempSetPoint;
				DeltaTHeating = FluidHX( CompNum ).DemandSideLoop.InletTemp - FluidHX( CompNum ).SupplySideLoop.InletTemp;
				if ( ( DeltaTHeating > FluidHX( CompNum ).TempControlTol ) && ( SetPointTemp > FluidHX( CompNum ).SupplySideLoop.InletTemp ) ) {
					// can and want to heat
					mdotSupSide = FluidHX( CompNum ).SupplySideLoop.MassFlowRateMax;
					SetComponentFlowRate( mdotSupSide, FluidHX( CompNum ).SupplySideLoop.InletNodeNum, FluidHX( CompNum ).SupplySideLoop.OutletNodeNum, FluidHX( CompNum ).SupplySideLoop.LoopNum, FluidHX( CompNum ).SupplySideLoop.LoopSideNum, FluidHX( CompNum ).SupplySideLoop.BranchNum, FluidHX( CompNum ).SupplySideLoop.CompNum );
					if ( mdotSupSide > MassFlowTolerance ) {
						mdotDmdSide = FluidHX( CompNum ).DemandSideLoop.MassFlowRateMax;
					} else {
						mdotDmdSide = 0.0;
					}
					SetComponentFlowRate( mdotDmdSide, FluidHX( CompNum ).DemandSideLoop.InletNodeNum, FluidHX( CompNum ).DemandSideLoop.OutletNodeNum, FluidHX( CompNum ).DemandSideLoop.LoopNum, FluidHX( CompNum ).DemandSideLoop.LoopSideNum, FluidHX( CompNum ).DemandSideLoop.BranchNum, FluidHX( CompNum ).DemandSideLoop.CompNum );
				} else { // not able or are wanting to heat so turn off
					mdotSupSide = 0.0;
					SetComponentFlowRate( mdotSupSide, FluidHX( CompNum ).SupplySideLoop.InletNodeNum, FluidHX( CompNum ).SupplySideLoop.OutletNodeNum, FluidHX( CompNum ).SupplySideLoop.LoopNum, FluidHX( CompNum ).SupplySideLoop.LoopSideNum, FluidHX( CompNum ).SupplySideLoop.BranchNum, FluidHX( CompNum ).SupplySideLoop.CompNum );
					//issue 4959, make demand side flow request on first hvac iteration so demand side loop can run as a trial to get a fresh demand side inlet temperature value
					if ( FirstHVACIteration ) {
						mdotDmdSide = FluidHX( CompNum ).DemandSideLoop.MassFlowRateMax;
					} else {
						mdotDmdSide = 0.0;
					}
					SetComponentFlowRate( mdotDmdSide, FluidHX( CompNum ).DemandSideLoop.InletNodeNum, FluidHX( CompNum ).DemandSideLoop.OutletNodeNum, FluidHX( CompNum ).DemandSideLoop.LoopNum, FluidHX( CompNum ).DemandSideLoop.LoopSideNum, FluidHX( CompNum ).DemandSideLoop.BranchNum, FluidHX( CompNum ).DemandSideLoop.CompNum );
				}

			} else if ( SELECT_CASE_var == CoolingSetPointModulated ) {

				SetPointTemp = Node( FluidHX( CompNum ).SetPointNodeNum ).TempSetPoint;
				DeltaTCooling = FluidHX( CompNum ).SupplySideLoop.InletTemp - FluidHX( CompNum ).DemandSideLoop.InletTemp;
				if ( ( DeltaTCooling > FluidHX( CompNum ).TempControlTol ) && ( SetPointTemp < FluidHX( CompNum ).SupplySideLoop.InletTemp ) ) {
					// can and want to cool
					mdotSupSide = FluidHX( CompNum ).SupplySideLoop.MassFlowRateMax;
					SetComponentFlowRate( mdotSupSide, FluidHX( CompNum ).SupplySideLoop.InletNodeNum, FluidHX( CompNum ).SupplySideLoop.OutletNodeNum, FluidHX( CompNum ).SupplySideLoop.LoopNum, FluidHX( CompNum ).SupplySideLoop.LoopSideNum, FluidHX( CompNum ).SupplySideLoop.BranchNum, FluidHX( CompNum ).SupplySideLoop.CompNum );
					if ( mdotSupSide > MassFlowTolerance ) {
						TargetLeavingTemp = SetPointTemp;
						FindHXDemandSideLoopFlow( CompNum, TargetLeavingTemp, CoolingSupplySideLoop );
					} else {
						mdotDmdSide = 0.0;
						SetComponentFlowRate( mdotDmdSide, FluidHX( CompNum ).DemandSideLoop.InletNodeNum, FluidHX( CompNum ).DemandSideLoop.OutletNodeNum, FluidHX( CompNum ).DemandSideLoop.LoopNum, FluidHX( CompNum ).DemandSideLoop.LoopSideNum, FluidHX( CompNum ).DemandSideLoop.BranchNum, FluidHX( CompNum ).DemandSideLoop.CompNum );
					}
				} else { // not able or are wanting to cool so turn off
					mdotSupSide = 0.0;
					SetComponentFlowRate( mdotSupSide, FluidHX( CompNum ).SupplySideLoop.InletNodeNum, FluidHX( CompNum ).SupplySideLoop.OutletNodeNum, FluidHX( CompNum ).SupplySideLoop.LoopNum, FluidHX( CompNum ).SupplySideLoop.LoopSideNum, FluidHX( CompNum ).SupplySideLoop.BranchNum, FluidHX( CompNum ).SupplySideLoop.CompNum );
					//issue 4959, make demand side flow request on first hvac iteration so demand side loop can run as a trial to get a fresh demand side inlet temperature value
					if ( FirstHVACIteration ) {
						mdotDmdSide = FluidHX( CompNum ).DemandSideLoop.MassFlowRateMax;
					} else {
						mdotDmdSide = 0.0;
					}
					SetComponentFlowRate( mdotDmdSide, FluidHX( CompNum ).DemandSideLoop.InletNodeNum, FluidHX( CompNum ).DemandSideLoop.OutletNodeNum, FluidHX( CompNum ).DemandSideLoop.LoopNum, FluidHX( CompNum ).DemandSideLoop.LoopSideNum, FluidHX( CompNum ).DemandSideLoop.BranchNum, FluidHX( CompNum ).DemandSideLoop.CompNum );
				}

			} else if ( SELECT_CASE_var == CoolingSetPointOnOff ) {

				SetPointTemp = Node( FluidHX( CompNum ).SetPointNodeNum ).TempSetPoint;
				DeltaTCooling = FluidHX( CompNum ).SupplySideLoop.InletTemp - FluidHX( CompNum ).DemandSideLoop.InletTemp;
				if ( ( DeltaTCooling > FluidHX( CompNum ).TempControlTol ) && ( SetPointTemp < FluidHX( CompNum ).SupplySideLoop.InletTemp ) ) {
					// can and want to cool
					mdotSupSide = FluidHX( CompNum ).SupplySideLoop.MassFlowRateMax;
					SetComponentFlowRate( mdotSupSide, FluidHX( CompNum ).SupplySideLoop.InletNodeNum, FluidHX( CompNum ).SupplySideLoop.OutletNodeNum, FluidHX( CompNum ).SupplySideLoop.LoopNum, FluidHX( CompNum ).SupplySideLoop.LoopSideNum, FluidHX( CompNum ).SupplySideLoop.BranchNum, FluidHX( CompNum ).SupplySideLoop.CompNum );
					if ( mdotSupSide > MassFlowTolerance ) {
						mdotDmdSide = FluidHX( CompNum ).DemandSideLoop.MassFlowRateMax;
					} else {
						mdotDmdSide = 0.0;
					}
					SetComponentFlowRate( mdotDmdSide, FluidHX( CompNum ).DemandSideLoop.InletNodeNum, FluidHX( CompNum ).DemandSideLoop.OutletNodeNum, FluidHX( CompNum ).DemandSideLoop.LoopNum, FluidHX( CompNum ).DemandSideLoop.LoopSideNum, FluidHX( CompNum ).DemandSideLoop.BranchNum, FluidHX( CompNum ).DemandSideLoop.CompNum );
				} else { // not able or are wanting to cool so turn off
					mdotSupSide = 0.0;
					SetComponentFlowRate( mdotSupSide, FluidHX( CompNum ).SupplySideLoop.InletNodeNum, FluidHX( CompNum ).SupplySideLoop.OutletNodeNum, FluidHX( CompNum ).SupplySideLoop.LoopNum, FluidHX( CompNum ).SupplySideLoop.LoopSideNum, FluidHX( CompNum ).SupplySideLoop.BranchNum, FluidHX( CompNum ).SupplySideLoop.CompNum );
					//issue 4959, make demand side flow request on first hvac iteration so demand side loop can run as a trial to get a fresh demand side inlet temperature value
					if ( FirstHVACIteration ) {
						mdotDmdSide = FluidHX( CompNum ).DemandSideLoop.MassFlowRateMax;
					} else {
						mdotDmdSide = 0.0;
					}
					SetComponentFlowRate( mdotDmdSide, FluidHX( CompNum ).DemandSideLoop.InletNodeNum, FluidHX( CompNum ).DemandSideLoop.OutletNodeNum, FluidHX( CompNum ).DemandSideLoop.LoopNum, FluidHX( CompNum ).DemandSideLoop.LoopSideNum, FluidHX( CompNum ).DemandSideLoop.BranchNum, FluidHX( CompNum ).DemandSideLoop.CompNum );
				}

			} else if ( SELECT_CASE_var == DualDeadBandSetPointModulated ) {

				SetPointTempLo = Node( FluidHX( CompNum ).SetPointNodeNum ).TempSetPointLo;
				SetPointTempHi = Node( FluidHX( CompNum ).SetPointNodeNum ).TempSetPointHi;
				DeltaTCooling = FluidHX( CompNum ).SupplySideLoop.InletTemp - FluidHX( CompNum ).DemandSideLoop.InletTemp;
				DeltaTCoolSetPointDemand = SetPointTempHi - FluidHX( CompNum ).DemandSideLoop.InletTemp;
				DeltaTCoolSetPointSupply = SetPointTempHi - FluidHX( CompNum ).SupplySideLoop.InletTemp;
				DeltaTHeating = FluidHX( CompNum ).DemandSideLoop.InletTemp - FluidHX( CompNum ).SupplySideLoop.InletTemp;
				DeltaTHeatSetPointDemand = SetPointTempLo - FluidHX( CompNum ).DemandSideLoop.InletTemp;
				DeltaTHeatSetPointSupply = SetPointTempLo - FluidHX( CompNum ).SupplySideLoop.InletTemp;
				if ( ( DeltaTCooling > FluidHX( CompNum ).TempControlTol ) && ( SetPointTempHi < FluidHX( CompNum ).SupplySideLoop.InletTemp ) ) {

					// can and want to cool
					mdotSupSide = FluidHX( CompNum ).SupplySideLoop.MassFlowRateMax;
					SetComponentFlowRate( mdotSupSide, FluidHX( CompNum ).SupplySideLoop.InletNodeNum, FluidHX( CompNum ).SupplySideLoop.OutletNodeNum, FluidHX( CompNum ).SupplySideLoop.LoopNum, FluidHX( CompNum ).SupplySideLoop.LoopSideNum, FluidHX( CompNum ).SupplySideLoop.BranchNum, FluidHX( CompNum ).SupplySideLoop.CompNum );
					if ( mdotSupSide > MassFlowTolerance ) {
						TargetLeavingTemp = SetPointTempHi;
						FindHXDemandSideLoopFlow( CompNum, TargetLeavingTemp, CoolingSupplySideLoop );
					} else {
						mdotDmdSide = 0.0;
						SetComponentFlowRate( mdotDmdSide, FluidHX( CompNum ).DemandSideLoop.InletNodeNum, FluidHX( CompNum ).DemandSideLoop.OutletNodeNum, FluidHX( CompNum ).DemandSideLoop.LoopNum, FluidHX( CompNum ).DemandSideLoop.LoopSideNum, FluidHX( CompNum ).DemandSideLoop.BranchNum, FluidHX( CompNum ).DemandSideLoop.CompNum );
					}
				} else if ( ( DeltaTHeating > FluidHX( CompNum ).TempControlTol ) && ( SetPointTempLo > FluidHX( CompNum ).SupplySideLoop.InletTemp ) ) {
					// can and want to heat
					mdotSupSide = FluidHX( CompNum ).SupplySideLoop.MassFlowRateMax;
					SetComponentFlowRate( mdotSupSide, FluidHX( CompNum ).SupplySideLoop.InletNodeNum, FluidHX( CompNum ).SupplySideLoop.OutletNodeNum, FluidHX( CompNum ).SupplySideLoop.LoopNum, FluidHX( CompNum ).SupplySideLoop.LoopSideNum, FluidHX( CompNum ).SupplySideLoop.BranchNum, FluidHX( CompNum ).SupplySideLoop.CompNum );
					if ( mdotSupSide > MassFlowTolerance ) {
						TargetLeavingTemp = SetPointTempLo;
						FindHXDemandSideLoopFlow( CompNum, TargetLeavingTemp, HeatingSupplySideLoop );
					} else {
						mdotDmdSide = 0.0;
						SetComponentFlowRate( mdotDmdSide, FluidHX( CompNum ).DemandSideLoop.InletNodeNum, FluidHX( CompNum ).DemandSideLoop.OutletNodeNum, FluidHX( CompNum ).DemandSideLoop.LoopNum, FluidHX( CompNum ).DemandSideLoop.LoopSideNum, FluidHX( CompNum ).DemandSideLoop.BranchNum, FluidHX( CompNum ).DemandSideLoop.CompNum );
					}
				} else { // not able or don't want conditioning
					mdotSupSide = 0.0;
					SetComponentFlowRate( mdotSupSide, FluidHX( CompNum ).SupplySideLoop.InletNodeNum, FluidHX( CompNum ).SupplySideLoop.OutletNodeNum, FluidHX( CompNum ).SupplySideLoop.LoopNum, FluidHX( CompNum ).SupplySideLoop.LoopSideNum, FluidHX( CompNum ).SupplySideLoop.BranchNum, FluidHX( CompNum ).SupplySideLoop.CompNum );
					//issue 4959, make demand side flow request on first hvac iteration so demand side loop can run as a trial to get a fresh demand side inlet temperature value
					if ( FirstHVACIteration ) {
						mdotDmdSide = FluidHX( CompNum ).DemandSideLoop.MassFlowRateMax;
					} else {
						mdotDmdSide = 0.0;
					}
					SetComponentFlowRate( mdotDmdSide, FluidHX( CompNum ).DemandSideLoop.InletNodeNum, FluidHX( CompNum ).DemandSideLoop.OutletNodeNum, FluidHX( CompNum ).DemandSideLoop.LoopNum, FluidHX( CompNum ).DemandSideLoop.LoopSideNum, FluidHX( CompNum ).DemandSideLoop.BranchNum, FluidHX( CompNum ).DemandSideLoop.CompNum );
				}

			} else if ( SELECT_CASE_var == DualDeadBandSetPointOnOff ) {

				SetPointTempLo = Node( FluidHX( CompNum ).SetPointNodeNum ).TempSetPointLo;
				SetPointTempHi = Node( FluidHX( CompNum ).SetPointNodeNum ).TempSetPointHi;
				DeltaTCooling = FluidHX( CompNum ).SupplySideLoop.InletTemp - FluidHX( CompNum ).DemandSideLoop.InletTemp;
				DeltaTHeating = FluidHX( CompNum ).DemandSideLoop.InletTemp - FluidHX( CompNum ).SupplySideLoop.InletTemp;
				if ( ( DeltaTCooling > FluidHX( CompNum ).TempControlTol ) && ( SetPointTempHi < FluidHX( CompNum ).SupplySideLoop.InletTemp ) ) {
					// can and want to cool
					mdotSupSide = FluidHX( CompNum ).SupplySideLoop.MassFlowRateMax;
					SetComponentFlowRate( mdotSupSide, FluidHX( CompNum ).SupplySideLoop.InletNodeNum, FluidHX( CompNum ).SupplySideLoop.OutletNodeNum, FluidHX( CompNum ).SupplySideLoop.LoopNum, FluidHX( CompNum ).SupplySideLoop.LoopSideNum, FluidHX( CompNum ).SupplySideLoop.BranchNum, FluidHX( CompNum ).SupplySideLoop.CompNum );
					if ( mdotSupSide > MassFlowTolerance ) {
						mdotDmdSide = FluidHX( CompNum ).DemandSideLoop.MassFlowRateMax;
					} else {
						mdotDmdSide = 0.0;
					}
					SetComponentFlowRate( mdotDmdSide, FluidHX( CompNum ).DemandSideLoop.InletNodeNum, FluidHX( CompNum ).DemandSideLoop.OutletNodeNum, FluidHX( CompNum ).DemandSideLoop.LoopNum, FluidHX( CompNum ).DemandSideLoop.LoopSideNum, FluidHX( CompNum ).DemandSideLoop.BranchNum, FluidHX( CompNum ).DemandSideLoop.CompNum );
				} else if ( ( DeltaTHeating > FluidHX( CompNum ).TempControlTol ) && ( SetPointTempLo > FluidHX( CompNum ).SupplySideLoop.InletTemp ) ) {
					// can and want to heat
					mdotSupSide = FluidHX( CompNum ).SupplySideLoop.MassFlowRateMax;
					SetComponentFlowRate( mdotSupSide, FluidHX( CompNum ).SupplySideLoop.InletNodeNum, FluidHX( CompNum ).SupplySideLoop.OutletNodeNum, FluidHX( CompNum ).SupplySideLoop.LoopNum, FluidHX( CompNum ).SupplySideLoop.LoopSideNum, FluidHX( CompNum ).SupplySideLoop.BranchNum, FluidHX( CompNum ).SupplySideLoop.CompNum );
					if ( mdotSupSide > MassFlowTolerance ) {
						mdotDmdSide = FluidHX( CompNum ).DemandSideLoop.MassFlowRateMax;
					} else {
						mdotDmdSide = 0.0;
					}
					SetComponentFlowRate( mdotDmdSide, FluidHX( CompNum ).DemandSideLoop.InletNodeNum, FluidHX( CompNum ).DemandSideLoop.OutletNodeNum, FluidHX( CompNum ).DemandSideLoop.LoopNum, FluidHX( CompNum ).DemandSideLoop.LoopSideNum, FluidHX( CompNum ).DemandSideLoop.BranchNum, FluidHX( CompNum ).DemandSideLoop.CompNum );
				} else { // not able or don't want conditioning
					mdotSupSide = 0.0;
					SetComponentFlowRate( mdotSupSide, FluidHX( CompNum ).SupplySideLoop.InletNodeNum, FluidHX( CompNum ).SupplySideLoop.OutletNodeNum, FluidHX( CompNum ).SupplySideLoop.LoopNum, FluidHX( CompNum ).SupplySideLoop.LoopSideNum, FluidHX( CompNum ).SupplySideLoop.BranchNum, FluidHX( CompNum ).SupplySideLoop.CompNum );
					//issue 4959, make demand side flow request on first hvac iteration so demand side loop can run as a trial to get a fresh demand side inlet temperature value
					if ( FirstHVACIteration ) {
						mdotDmdSide = FluidHX( CompNum ).DemandSideLoop.MassFlowRateMax;
					} else {
						mdotDmdSide = 0.0;
					}
					SetComponentFlowRate( mdotDmdSide, FluidHX( CompNum ).DemandSideLoop.InletNodeNum, FluidHX( CompNum ).DemandSideLoop.OutletNodeNum, FluidHX( CompNum ).DemandSideLoop.LoopNum, FluidHX( CompNum ).DemandSideLoop.LoopSideNum, FluidHX( CompNum ).DemandSideLoop.BranchNum, FluidHX( CompNum ).DemandSideLoop.CompNum );
				}

			} else if ( SELECT_CASE_var == CoolingDifferentialOnOff ) {

				DeltaTCooling = FluidHX( CompNum ).SupplySideLoop.InletTemp - FluidHX( CompNum ).DemandSideLoop.InletTemp;
				if ( DeltaTCooling > FluidHX( CompNum ).TempControlTol ) {
					//  want to cool
					mdotSupSide = FluidHX( CompNum ).SupplySideLoop.MassFlowRateMax;
					SetComponentFlowRate( mdotSupSide, FluidHX( CompNum ).SupplySideLoop.InletNodeNum, FluidHX( CompNum ).SupplySideLoop.OutletNodeNum, FluidHX( CompNum ).SupplySideLoop.LoopNum, FluidHX( CompNum ).SupplySideLoop.LoopSideNum, FluidHX( CompNum ).SupplySideLoop.BranchNum, FluidHX( CompNum ).SupplySideLoop.CompNum );
					if ( mdotSupSide > MassFlowTolerance ) {
						mdotDmdSide = FluidHX( CompNum ).DemandSideLoop.MassFlowRateMax;
					} else {
						mdotDmdSide = 0.0;
					}
					SetComponentFlowRate( mdotDmdSide, FluidHX( CompNum ).DemandSideLoop.InletNodeNum, FluidHX( CompNum ).DemandSideLoop.OutletNodeNum, FluidHX( CompNum ).DemandSideLoop.LoopNum, FluidHX( CompNum ).DemandSideLoop.LoopSideNum, FluidHX( CompNum ).DemandSideLoop.BranchNum, FluidHX( CompNum ).DemandSideLoop.CompNum );
				} else { // not wanting to cool so turn off
					mdotSupSide = 0.0;
					SetComponentFlowRate( mdotSupSide, FluidHX( CompNum ).SupplySideLoop.InletNodeNum, FluidHX( CompNum ).SupplySideLoop.OutletNodeNum, FluidHX( CompNum ).SupplySideLoop.LoopNum, FluidHX( CompNum ).SupplySideLoop.LoopSideNum, FluidHX( CompNum ).SupplySideLoop.BranchNum, FluidHX( CompNum ).SupplySideLoop.CompNum );
					//issue 4959, make demand side flow request on first hvac iteration so demand side loop can run as a trial to get a fresh demand side inlet temperature value
					if ( FirstHVACIteration ) {
						mdotDmdSide = FluidHX( CompNum ).DemandSideLoop.MassFlowRateMax;
					} else {
						mdotDmdSide = 0.0;
					}
					SetComponentFlowRate( mdotDmdSide, FluidHX( CompNum ).DemandSideLoop.InletNodeNum, FluidHX( CompNum ).DemandSideLoop.OutletNodeNum, FluidHX( CompNum ).DemandSideLoop.LoopNum, FluidHX( CompNum ).DemandSideLoop.LoopSideNum, FluidHX( CompNum ).DemandSideLoop.BranchNum, FluidHX( CompNum ).DemandSideLoop.CompNum );
				}

			} else if ( SELECT_CASE_var == CoolingSetPointOnOffWithComponentOverride ) {

				{ auto const SELECT_CASE_var1( FluidHX( CompNum ).ControlSignalTemp );
				if ( SELECT_CASE_var1 == WetBulbTemperature ) {
					ControlSignalValue = OutWetBulbTemp;
				} else if ( SELECT_CASE_var1 == DryBulbTemperature ) {
					ControlSignalValue = OutDryBulbTemp;
				} else if ( SELECT_CASE_var1 == LoopTemperature ) {
					// ControlSignalValue = FluidHX(CompNum)%DemandSideLoop%InletTemp
					ControlSignalValue = Node( FluidHX( CompNum ).OtherCompDemandSideLoop.InletNodeNum ).TempLastTimestep;
				} else {
					assert( false );
				}}

				SetPointTemp = Node( FluidHX( CompNum ).SetPointNodeNum ).TempSetPoint;
				DeltaTCooling = SetPointTemp - ControlSignalValue;
				//obtain shut down state
				ChillerShutDown = PlantLoop( FluidHX( CompNum ).OtherCompSupplySideLoop.LoopNum ).LoopSide( FluidHX( CompNum ).OtherCompSupplySideLoop.LoopSideNum ).Branch( FluidHX( CompNum ).OtherCompSupplySideLoop.BranchNum ).Comp( FluidHX( CompNum ).OtherCompSupplySideLoop.CompNum ).FreeCoolCntrlShutDown;
				if ( ChillerShutDown && ( DeltaTCooling > FluidHX( CompNum ).TempControlTol ) ) {
					// can and want to cool
					mdotSupSide = FluidHX( CompNum ).SupplySideLoop.MassFlowRateMax;
					SetComponentFlowRate( mdotSupSide, FluidHX( CompNum ).SupplySideLoop.InletNodeNum, FluidHX( CompNum ).SupplySideLoop.OutletNodeNum, FluidHX( CompNum ).SupplySideLoop.LoopNum, FluidHX( CompNum ).SupplySideLoop.LoopSideNum, FluidHX( CompNum ).SupplySideLoop.BranchNum, FluidHX( CompNum ).SupplySideLoop.CompNum );
					if ( mdotSupSide > MassFlowTolerance ) {
						mdotDmdSide = FluidHX( CompNum ).DemandSideLoop.MassFlowRateMax;
					} else {
						mdotDmdSide = 0.0;
					}
					SetComponentFlowRate( mdotDmdSide, FluidHX( CompNum ).DemandSideLoop.InletNodeNum, FluidHX( CompNum ).DemandSideLoop.OutletNodeNum, FluidHX( CompNum ).DemandSideLoop.LoopNum, FluidHX( CompNum ).DemandSideLoop.LoopSideNum, FluidHX( CompNum ).DemandSideLoop.BranchNum, FluidHX( CompNum ).DemandSideLoop.CompNum );

				} else {
					mdotSupSide = 0.0;
					SetComponentFlowRate( mdotSupSide, FluidHX( CompNum ).SupplySideLoop.InletNodeNum, FluidHX( CompNum ).SupplySideLoop.OutletNodeNum, FluidHX( CompNum ).SupplySideLoop.LoopNum, FluidHX( CompNum ).SupplySideLoop.LoopSideNum, FluidHX( CompNum ).SupplySideLoop.BranchNum, FluidHX( CompNum ).SupplySideLoop.CompNum );
					//issue 4959, make demand side flow request on first hvac iteration so demand side loop can run as a trial to get a fresh demand side inlet temperature value
					if ( FirstHVACIteration ) {
						mdotDmdSide = FluidHX( CompNum ).DemandSideLoop.MassFlowRateMax;
					} else {
						mdotDmdSide = 0.0;
					}
					SetComponentFlowRate( mdotDmdSide, FluidHX( CompNum ).DemandSideLoop.InletNodeNum, FluidHX( CompNum ).DemandSideLoop.OutletNodeNum, FluidHX( CompNum ).DemandSideLoop.LoopNum, FluidHX( CompNum ).DemandSideLoop.LoopSideNum, FluidHX( CompNum ).DemandSideLoop.BranchNum, FluidHX( CompNum ).DemandSideLoop.CompNum );
				}

			}}

		} else { // scheduled off
			mdotSupSide = 0.0;
			SetComponentFlowRate( mdotSupSide, FluidHX( CompNum ).SupplySideLoop.InletNodeNum, FluidHX( CompNum ).SupplySideLoop.OutletNodeNum, FluidHX( CompNum ).SupplySideLoop.LoopNum, FluidHX( CompNum ).SupplySideLoop.LoopSideNum, FluidHX( CompNum ).SupplySideLoop.BranchNum, FluidHX( CompNum ).SupplySideLoop.CompNum );
			mdotDmdSide = 0.0;
			SetComponentFlowRate( mdotDmdSide, FluidHX( CompNum ).DemandSideLoop.InletNodeNum, FluidHX( CompNum ).DemandSideLoop.OutletNodeNum, FluidHX( CompNum ).DemandSideLoop.LoopNum, FluidHX( CompNum ).DemandSideLoop.LoopSideNum, FluidHX( CompNum ).DemandSideLoop.BranchNum, FluidHX( CompNum ).DemandSideLoop.CompNum );
		}

	}

	void
	CalcFluidHeatExchanger(
		int const CompNum,
		Real64 const SupSideMdot, // mass flow rate of fluid entering from supply side loop
		Real64 const DmdSideMdot // mass flow rate of fluid entering from demand side loop
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B.Griffith, derived from CalcEconHeatExchanger by  Sankaranarayanan K P aug. 2007
		//       DATE WRITTEN   November 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Evalutate heat exchanger model and calculate leaving temperatures

		// METHODOLOGY EMPLOYED:
		// apply heat transfer model depending on type of HX used

		// REFERENCES:
		// na

		// Using/Aliasing
		using FluidProperties::GetSpecificHeatGlycol;
		using DataLoopNode::Node;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// SUBROUTINE PARAMETER DEFINITIONS:
		int const CmaxMixedCminUnmixed( 40 );
		int const CmaxUnMixedCminMixed( 41 );
		static std::string const RoutineName( "CalcFluidHeatExchanger" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 SupSideLoopInletTemp;
		Real64 DmdSideLoopInletTemp;
		Real64 SupSideLoopInletCp; // specific heat of fluid entering from supply side loop at inlet temp
		Real64 DmdSideLoopInletCp; // specific heat of fluid entering from demand side loop at inlet temp
		Real64 SupSideCapRate; // product of specific heat and mass flow for supply side loop at inlet temp
		Real64 DmdSideCapRate; // product of specific heat and mass flow for demand side loop at inlet temp
		Real64 MinCapRate; // minimum capacity flow rate
		Real64 MaxCapRate; // maximum capacity flow rate
		Real64 NTU; // number of transfer units for heat exchanger performance model
		Real64 CapRatio;
		Real64 ExpCheckValue1;
		Real64 ExpCheckValue2;
		Real64 Effectiveness( 0.0 );
		Real64 HeatTransferRate;
		int CrossFlowEquation;

		SupSideLoopInletTemp = Node( FluidHX( CompNum ).SupplySideLoop.InletNodeNum ).Temp;
		DmdSideLoopInletTemp = Node( FluidHX( CompNum ).DemandSideLoop.InletNodeNum ).Temp;

		SupSideLoopInletCp = GetSpecificHeatGlycol( PlantLoop( FluidHX( CompNum ).SupplySideLoop.LoopNum ).FluidName, SupSideLoopInletTemp, PlantLoop( FluidHX( CompNum ).SupplySideLoop.LoopNum ).FluidIndex, RoutineName );
		DmdSideLoopInletCp = GetSpecificHeatGlycol( PlantLoop( FluidHX( CompNum ).DemandSideLoop.LoopNum ).FluidName, DmdSideLoopInletTemp, PlantLoop( FluidHX( CompNum ).DemandSideLoop.LoopNum ).FluidIndex, RoutineName );

		SupSideCapRate = SupSideMdot * SupSideLoopInletCp;
		DmdSideCapRate = DmdSideMdot * DmdSideLoopInletCp;
		MinCapRate = min( SupSideCapRate, DmdSideCapRate );
		MaxCapRate = max( SupSideCapRate, DmdSideCapRate );

		if ( MinCapRate > 0.0 ) {

			{ auto const SELECT_CASE_var( FluidHX( CompNum ).HeatExchangeModelType );

			if ( SELECT_CASE_var == CrossFlowBothUnMixed ) {
				NTU = FluidHX( CompNum ).UA / MinCapRate;
				CapRatio = MinCapRate / MaxCapRate;
				ExpCheckValue1 = std::pow( NTU, 0.22 ) / CapRatio;
				ExpCheckValue2 = -CapRatio * std::pow( NTU, 0.78 );
				if ( ( ExpCheckValue1 > EXP_UpperLimit ) || ( ExpCheckValue2 > EXP_UpperLimit ) ) {
					if ( -NTU >= EXP_LowerLimit ) {
						Effectiveness = 1.0 - std::exp( -NTU );
						Effectiveness = min( 1.0, Effectiveness );
					} else {
						Effectiveness = 1.0;
					}
				} else {
					Effectiveness = 1.0 - std::exp( ( std::pow( NTU, 0.22 ) / CapRatio ) * ( std::exp( -CapRatio * std::pow( NTU, 0.78 ) ) - 1.0 ) );
					Effectiveness = min( 1.0, Effectiveness );
				}

			} else if ( SELECT_CASE_var == CrossFlowBothMixed ) {
				NTU = FluidHX( CompNum ).UA / MinCapRate;
				CapRatio = MinCapRate / MaxCapRate;
				ExpCheckValue1 = -CapRatio * NTU;
				ExpCheckValue2 = -NTU;
				if ( ExpCheckValue1 < EXP_LowerLimit ) {
					if ( ExpCheckValue2 >= EXP_LowerLimit ) {
						Effectiveness = 1.0 - std::exp( -NTU );
						Effectiveness = min( 1.0, Effectiveness );
					} else {
						Effectiveness = 1.0;
					}

				} else if ( ( std::exp( -NTU ) == 1.0 ) || ( NTU == 0.0 ) || ( std::exp( -CapRatio * NTU ) == 1.0 ) ) { // don't div by zero

					Effectiveness = 0.0;
				} else {
					Effectiveness = 1.0 / ( ( 1.0 / ( 1.0 - std::exp( -NTU ) ) ) + ( CapRatio / ( 1.0 - std::exp( -CapRatio * NTU ) ) ) - ( 1.0 / NTU ) );
					Effectiveness = min( 1.0, Effectiveness );
				}

			} else if ( ( SELECT_CASE_var == CrossFlowSupplyLoopMixedDemandLoopUnMixed ) || ( SELECT_CASE_var == CrossFlowSupplyLoopUnMixedDemandLoopMixed ) ) {

				if ( SupSideCapRate == MaxCapRate && FluidHX( CompNum ).HeatExchangeModelType == CrossFlowSupplyLoopMixedDemandLoopUnMixed ) {
					CrossFlowEquation = CmaxMixedCminUnmixed;
				} else if ( SupSideCapRate == MinCapRate && FluidHX( CompNum ).HeatExchangeModelType == CrossFlowSupplyLoopMixedDemandLoopUnMixed ) {
					CrossFlowEquation = CmaxUnMixedCminMixed;
				} else if ( DmdSideCapRate == MaxCapRate && FluidHX( CompNum ).HeatExchangeModelType == CrossFlowSupplyLoopUnMixedDemandLoopMixed ) {
					CrossFlowEquation = CmaxMixedCminUnmixed;
				} else if ( DmdSideCapRate == MinCapRate && FluidHX( CompNum ).HeatExchangeModelType == CrossFlowSupplyLoopUnMixedDemandLoopMixed ) {
					CrossFlowEquation = CmaxUnMixedCminMixed;
				} else {
					CrossFlowEquation = CmaxMixedCminUnmixed;
				}

				NTU = FluidHX( CompNum ).UA / MinCapRate;
				CapRatio = MinCapRate / MaxCapRate;
				if ( CrossFlowEquation == CmaxMixedCminUnmixed ) {
					ExpCheckValue1 = -NTU;
					if ( CapRatio == 0.0 ) { // protect div by zero
						if ( ExpCheckValue1 >= EXP_LowerLimit ) {
							Effectiveness = 1.0 - std::exp( -NTU );
							Effectiveness = min( 1.0, Effectiveness );
						} else {
							Effectiveness = 1.0;
						}
					} else if ( ExpCheckValue1 < EXP_LowerLimit ) {
						Effectiveness = 0.632 / CapRatio;
						Effectiveness = min( 1.0, Effectiveness );
					} else {
						Effectiveness = ( 1.0 / CapRatio ) * ( 1.0 - std::exp( CapRatio * std::exp( -NTU ) - 1.0 ) );
						Effectiveness = min( 1.0, Effectiveness );
					}
				} else if ( CrossFlowEquation == CmaxUnMixedCminMixed ) {
					ExpCheckValue1 = -CapRatio * NTU;
					if ( CapRatio == 0.0 ) {
						if ( -NTU >= EXP_LowerLimit ) {
							Effectiveness = 1.0 - std::exp( -NTU );
							Effectiveness = min( 1.0, Effectiveness );
						} else {
							Effectiveness = 1.0;
						}
					} else {
						ExpCheckValue2 = -( 1.0 / CapRatio ) * ( 1.0 - std::exp( -CapRatio * NTU ) );
						if ( ExpCheckValue2 < EXP_LowerLimit ) {
							Effectiveness = 1.0;
						} else {
							Effectiveness = 1.0 - std::exp( -( 1.0 / CapRatio ) * ( 1.0 - std::exp( -CapRatio * NTU ) ) );
							Effectiveness = min( 1.0, Effectiveness );
						}
					}
				} else {
					assert( false );
				}

			} else if ( SELECT_CASE_var == CounterFlow ) {
				NTU = FluidHX( CompNum ).UA / MinCapRate;
				CapRatio = MinCapRate / MaxCapRate;
				ExpCheckValue1 = -NTU * ( 1.0 - CapRatio );
				if ( ExpCheckValue1 > EXP_UpperLimit ) {
					if ( -NTU >= EXP_LowerLimit ) {
						Effectiveness = 1.0 - std::exp( -NTU );
						Effectiveness = min( 1.0, Effectiveness );
					} else {
						Effectiveness = 1.0;
					}
				} else if ( CapRatio * std::exp( -NTU * ( 1.0 - CapRatio ) ) == 1.0 ) {
					if ( -NTU >= EXP_LowerLimit ) {
						Effectiveness = 1.0 - std::exp( -NTU );
						Effectiveness = min( 1.0, Effectiveness );
					} else {
						Effectiveness = 1.0;
					}
				} else {
					Effectiveness = ( 1.0 - std::exp( -NTU * ( 1.0 - CapRatio ) ) ) / ( 1.0 - CapRatio * std::exp( -NTU * ( 1.0 - CapRatio ) ) );
					Effectiveness = min( 1.0, Effectiveness );
				}

			} else if ( SELECT_CASE_var == ParallelFlow ) {
				NTU = FluidHX( CompNum ).UA / MinCapRate;
				CapRatio = MinCapRate / MaxCapRate;
				ExpCheckValue1 = -NTU * ( 1.0 + CapRatio );
				if ( ExpCheckValue1 > EXP_UpperLimit ) {
					if ( -NTU >= EXP_LowerLimit ) {
						Effectiveness = 1.0 - std::exp( -NTU );
						Effectiveness = min( 1.0, Effectiveness );
					} else {
						Effectiveness = 1.0;
					}
				} else {
					Effectiveness = ( 1.0 - std::exp( -NTU * ( 1.0 + CapRatio ) ) ) / ( 1.0 + CapRatio );
					Effectiveness = min( 1.0, Effectiveness );
				}

			} else if ( SELECT_CASE_var == Ideal ) {
				Effectiveness = 1.0;
			} else {
				assert( false );
			}}

		} else { // no capacity
			Effectiveness = 0.0;

		}

		HeatTransferRate = Effectiveness * MinCapRate * ( SupSideLoopInletTemp - DmdSideLoopInletTemp ); // + means supply side is cooled

		if ( SupSideMdot > 0.0 ) {
			FluidHX( CompNum ).SupplySideLoop.OutletTemp = SupSideLoopInletTemp - HeatTransferRate / ( SupSideLoopInletCp * SupSideMdot );
		} else {
			FluidHX( CompNum ).SupplySideLoop.OutletTemp = SupSideLoopInletTemp;
		}

		if ( DmdSideMdot > 0.0 ) {
			FluidHX( CompNum ).DemandSideLoop.OutletTemp = DmdSideLoopInletTemp + HeatTransferRate / ( DmdSideLoopInletCp * DmdSideMdot );
		} else {
			FluidHX( CompNum ).DemandSideLoop.OutletTemp = DmdSideLoopInletTemp;
		}
		FluidHX( CompNum ).Effectiveness = Effectiveness;
		FluidHX( CompNum ).HeatTransferRate = HeatTransferRate;
		FluidHX( CompNum ).SupplySideLoop.InletTemp = SupSideLoopInletTemp;
		FluidHX( CompNum ).SupplySideLoop.InletMassFlowRate = SupSideMdot;
		FluidHX( CompNum ).DemandSideLoop.InletTemp = DmdSideLoopInletTemp;
		FluidHX( CompNum ).DemandSideLoop.InletMassFlowRate = DmdSideMdot;

	}

	void
	FindHXDemandSideLoopFlow(
		int const CompNum,
		Real64 const TargetSupplySideLoopLeavingTemp,
		int const HXActionMode
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   November 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// modulate demand side flow rate to hit a target leaving temperature (within tolerance)

		// METHODOLOGY EMPLOYED:
		// uses E+'s Regula Falsi numercial method

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::WarmupFlag;
		using General::RoundSigDigits;
		using General::SolveRegulaFalsi;
		using PlantUtilities::SetComponentFlowRate;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const MaxIte( 500 ); // Maximum number of iterations for solver
		Real64 const Acc( 1.e-3 ); // Accuracy of solver result

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int SolFla; // Flag of solver
		Array1D< Real64 > Par( 2 ); // Parameter array passed to solver

		Real64 LeavingTempMinFlow;
		Real64 LeavingTempFullFlow;
		Real64 SupSideMdot; // mass flow rate of fluid entering from supply side loop
		Real64 DmdSideMdot; // mass flow rate of fluid entering from demand side loop

		SupSideMdot = Node( FluidHX( CompNum ).SupplySideLoop.InletNodeNum ).MassFlowRate;
		// first see if root is bracketed
		// min demand flow
		DmdSideMdot = FluidHX( CompNum ).DemandSideLoop.MassFlowRateMin;
		CalcFluidHeatExchanger( CompNum, SupSideMdot, DmdSideMdot );
		LeavingTempMinFlow = FluidHX( CompNum ).SupplySideLoop.OutletTemp;

		// full demand flow
		DmdSideMdot = FluidHX( CompNum ).DemandSideLoop.MassFlowRateMax;
		CalcFluidHeatExchanger( CompNum, SupSideMdot, DmdSideMdot );
		LeavingTempFullFlow = FluidHX( CompNum ).SupplySideLoop.OutletTemp;

		{ auto const SELECT_CASE_var( HXActionMode );

		if ( SELECT_CASE_var == HeatingSupplySideLoop ) {
			if ( ( LeavingTempFullFlow > TargetSupplySideLoopLeavingTemp ) && ( TargetSupplySideLoopLeavingTemp > LeavingTempMinFlow ) ) {
				// need to solve
				Par( 1 ) = double( CompNum ); // HX index
				Par( 2 ) = TargetSupplySideLoopLeavingTemp;

				SolveRegulaFalsi( Acc, MaxIte, SolFla, DmdSideMdot, HXDemandSideLoopFlowResidual, FluidHX( CompNum ).DemandSideLoop.MassFlowRateMin, FluidHX( CompNum ).DemandSideLoop.MassFlowRateMax, Par );

				if ( SolFla == -1 ) { // no convergence
					if ( ! WarmupFlag ) {
						if ( FluidHX( CompNum ).DmdSideModulatSolvNoConvergeErrorCount < 1 ) {
							++FluidHX( CompNum ).DmdSideModulatSolvNoConvergeErrorCount;
							ShowWarningError( ComponentClassName + " named " + FluidHX( CompNum ).Name + " - Iteration Limit exceeded calculating demand side loop flow rate" );
							ShowContinueError( "Simulation continues with calculated demand side mass flow rate = " + RoundSigDigits( DmdSideMdot, 7 ) );
						}
						ShowRecurringWarningErrorAtEnd( ComponentClassName + " named " + FluidHX( CompNum ).Name + " - Iteration Limit exceeded calculating demand side loop flow rate continues.", FluidHX( CompNum ).DmdSideModulatSolvNoConvergeErrorIndex, DmdSideMdot, DmdSideMdot );
					}
				} else if ( SolFla == -2 ) { //f(x0) and f(x1) have the same sign
					DmdSideMdot = FluidHX( CompNum ).DemandSideLoop.MassFlowRateMax * ( LeavingTempFullFlow - TargetSupplySideLoopLeavingTemp ) / ( LeavingTempFullFlow - LeavingTempMinFlow );
					if ( ! WarmupFlag ) {
						if ( FluidHX( CompNum ).DmdSideModulatSolvFailErrorCount < 1 ) {
							++FluidHX( CompNum ).DmdSideModulatSolvFailErrorCount;
							ShowWarningError( ComponentClassName + " named " + FluidHX( CompNum ).Name + " - Solver failed to calculate demand side loop flow rate" );
							ShowContinueError( "Simulation continues with estimated demand side mass flow rate = " + RoundSigDigits( DmdSideMdot, 7 ) );
						}
						ShowRecurringWarningErrorAtEnd( ComponentClassName + " named " + FluidHX( CompNum ).Name + " - Solver failed to calculate demand side loop flow rate continues.", FluidHX( CompNum ).DmdSideModulatSolvFailErrorIndex, DmdSideMdot, DmdSideMdot );
					}
				}
				SetComponentFlowRate( DmdSideMdot, FluidHX( CompNum ).DemandSideLoop.InletNodeNum, FluidHX( CompNum ).DemandSideLoop.OutletNodeNum, FluidHX( CompNum ).DemandSideLoop.LoopNum, FluidHX( CompNum ).DemandSideLoop.LoopSideNum, FluidHX( CompNum ).DemandSideLoop.BranchNum, FluidHX( CompNum ).DemandSideLoop.CompNum );

			} else if ( ( TargetSupplySideLoopLeavingTemp >= LeavingTempFullFlow ) && ( LeavingTempFullFlow > LeavingTempMinFlow ) ) {
				// run at full flow
				DmdSideMdot = FluidHX( CompNum ).DemandSideLoop.MassFlowRateMax;
				SetComponentFlowRate( DmdSideMdot, FluidHX( CompNum ).DemandSideLoop.InletNodeNum, FluidHX( CompNum ).DemandSideLoop.OutletNodeNum, FluidHX( CompNum ).DemandSideLoop.LoopNum, FluidHX( CompNum ).DemandSideLoop.LoopSideNum, FluidHX( CompNum ).DemandSideLoop.BranchNum, FluidHX( CompNum ).DemandSideLoop.CompNum );

			} else if ( LeavingTempMinFlow >= TargetSupplySideLoopLeavingTemp ) {

				// run at min flow
				DmdSideMdot = FluidHX( CompNum ).DemandSideLoop.MassFlowRateMin;
				SetComponentFlowRate( DmdSideMdot, FluidHX( CompNum ).DemandSideLoop.InletNodeNum, FluidHX( CompNum ).DemandSideLoop.OutletNodeNum, FluidHX( CompNum ).DemandSideLoop.LoopNum, FluidHX( CompNum ).DemandSideLoop.LoopSideNum, FluidHX( CompNum ).DemandSideLoop.BranchNum, FluidHX( CompNum ).DemandSideLoop.CompNum );
			}
		} else if ( SELECT_CASE_var == CoolingSupplySideLoop ) {
			if ( ( LeavingTempFullFlow < TargetSupplySideLoopLeavingTemp ) && ( TargetSupplySideLoopLeavingTemp < LeavingTempMinFlow ) ) {
				// need to solve
				Par( 1 ) = double( CompNum ); // HX index
				Par( 2 ) = TargetSupplySideLoopLeavingTemp;

				SolveRegulaFalsi( Acc, MaxIte, SolFla, DmdSideMdot, HXDemandSideLoopFlowResidual, FluidHX( CompNum ).DemandSideLoop.MassFlowRateMin, FluidHX( CompNum ).DemandSideLoop.MassFlowRateMax, Par );

				if ( SolFla == -1 ) { // no convergence
					if ( ! WarmupFlag ) {
						if ( FluidHX( CompNum ).DmdSideModulatSolvNoConvergeErrorCount < 1 ) {
							++FluidHX( CompNum ).DmdSideModulatSolvNoConvergeErrorCount;
							ShowWarningError( ComponentClassName + " named " + FluidHX( CompNum ).Name + " - Iteration Limit exceeded calculating demand side loop flow rate" );
							ShowContinueError( "Simulation continues with calculated demand side mass flow rate = " + RoundSigDigits( DmdSideMdot, 7 ) );
						}
						ShowRecurringWarningErrorAtEnd( ComponentClassName + " named " + FluidHX( CompNum ).Name + " - Iteration Limit exceeded calculating demand side loop flow rate continues.", FluidHX( CompNum ).DmdSideModulatSolvNoConvergeErrorIndex, DmdSideMdot, DmdSideMdot );
					}
				} else if ( SolFla == -2 ) { //f(x0) and f(x1) have the same sign
					DmdSideMdot = FluidHX( CompNum ).DemandSideLoop.MassFlowRateMax * ( LeavingTempFullFlow - TargetSupplySideLoopLeavingTemp ) / ( LeavingTempFullFlow - LeavingTempMinFlow );
					if ( ! WarmupFlag ) {
						if ( FluidHX( CompNum ).DmdSideModulatSolvFailErrorCount < 1 ) {
							++FluidHX( CompNum ).DmdSideModulatSolvFailErrorCount;
							ShowWarningError( ComponentClassName + " named " + FluidHX( CompNum ).Name + " - Solver failed to calculate demand side loop flow rate" );
							ShowContinueError( "Simulation continues with estimated demand side mass flow rate = " + RoundSigDigits( DmdSideMdot, 7 ) );
						}
						ShowRecurringWarningErrorAtEnd( ComponentClassName + " named " + FluidHX( CompNum ).Name + " - Solver failed to calculate demand side loop flow rate continues.", FluidHX( CompNum ).DmdSideModulatSolvFailErrorIndex, DmdSideMdot, DmdSideMdot );
					}
				}
				SetComponentFlowRate( DmdSideMdot, FluidHX( CompNum ).DemandSideLoop.InletNodeNum, FluidHX( CompNum ).DemandSideLoop.OutletNodeNum, FluidHX( CompNum ).DemandSideLoop.LoopNum, FluidHX( CompNum ).DemandSideLoop.LoopSideNum, FluidHX( CompNum ).DemandSideLoop.BranchNum, FluidHX( CompNum ).DemandSideLoop.CompNum );
			} else if ( ( TargetSupplySideLoopLeavingTemp <= LeavingTempFullFlow ) && ( LeavingTempFullFlow < LeavingTempMinFlow ) ) {
				// run at full flow
				DmdSideMdot = FluidHX( CompNum ).DemandSideLoop.MassFlowRateMax;
				SetComponentFlowRate( DmdSideMdot, FluidHX( CompNum ).DemandSideLoop.InletNodeNum, FluidHX( CompNum ).DemandSideLoop.OutletNodeNum, FluidHX( CompNum ).DemandSideLoop.LoopNum, FluidHX( CompNum ).DemandSideLoop.LoopSideNum, FluidHX( CompNum ).DemandSideLoop.BranchNum, FluidHX( CompNum ).DemandSideLoop.CompNum );
			} else if ( LeavingTempMinFlow <= TargetSupplySideLoopLeavingTemp ) {

				// run at min flow
				DmdSideMdot = FluidHX( CompNum ).DemandSideLoop.MassFlowRateMin;
				SetComponentFlowRate( DmdSideMdot, FluidHX( CompNum ).DemandSideLoop.InletNodeNum, FluidHX( CompNum ).DemandSideLoop.OutletNodeNum, FluidHX( CompNum ).DemandSideLoop.LoopNum, FluidHX( CompNum ).DemandSideLoop.LoopSideNum, FluidHX( CompNum ).DemandSideLoop.BranchNum, FluidHX( CompNum ).DemandSideLoop.CompNum );
			}

		}}

	}

	Real64
	HXDemandSideLoopFlowResidual(
		Real64 const DmdSideMassFlowRate,
		Array1< Real64 > const & Par // Par(1) = HX index number
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   December 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// calculate residual value for regula falsi solver

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using PlantUtilities::SetComponentFlowRate;

		// Return value
		Real64 Residuum; // Residual to be minimized to zero

		// Argument array dimensioning

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:
		// Par(2) = desired supply side loop outlet temperature [C]

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int CompNum;
		Real64 SupSideLoopOutletTemp;
		Real64 MdotTrial;
		Real64 SupSideMdot; // mass flow rate of fluid entering from supply side loop

		MdotTrial = DmdSideMassFlowRate;
		CompNum = int( Par( 1 ) );
		SupSideMdot = Node( FluidHX( CompNum ).SupplySideLoop.InletNodeNum ).MassFlowRate;

		CalcFluidHeatExchanger( CompNum, SupSideMdot, MdotTrial );

		SupSideLoopOutletTemp = FluidHX( CompNum ).SupplySideLoop.OutletTemp;

		Residuum = Par( 2 ) - SupSideLoopOutletTemp;

		return Residuum;

	}

	void
	UpdateFluidHeatExchanger( int const CompNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   December 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// update calculate results

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		Node( FluidHX( CompNum ).DemandSideLoop.OutletNodeNum ).Temp = FluidHX( CompNum ).DemandSideLoop.OutletTemp;
		Node( FluidHX( CompNum ).SupplySideLoop.OutletNodeNum ).Temp = FluidHX( CompNum ).SupplySideLoop.OutletTemp;

	}

	void
	ReportFluidHeatExchanger( int const CompNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   December, 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// update heat exchanger report variables

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHVACGlobals::TimeStepSys;
		using DataHVACGlobals::SmallLoad;
		using DataGlobals::SecInHour;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		FluidHX( CompNum ).HeatTransferEnergy = FluidHX( CompNum ).HeatTransferRate * TimeStepSys * SecInHour;

		if ( ( std::abs( FluidHX( CompNum ).HeatTransferRate ) > SmallLoad ) && ( FluidHX( CompNum ).DemandSideLoop.InletMassFlowRate > 0.0 ) && ( FluidHX( CompNum ).SupplySideLoop.InletMassFlowRate > 0.0 ) ) {
			FluidHX( CompNum ).OperationStatus = 1.0;
		} else {
			FluidHX( CompNum ).OperationStatus = 0.0;
		}

	}

} // PlantHeatExchangerFluidToFluid

} // EnergyPlus
