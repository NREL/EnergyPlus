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
#include <PlantLocation.hh>
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
	std::string nsvComponentClassName( "HeatExchanger:FluidToFluid" );
	int NumberOfPlantFluidHXs( 0 );
	bool GetInput( true );
	Array1D_bool CheckFluidHXs;

	// SUBROUTINE SPECIFICATIONS FOR MODULE

	// Object Data
	Array1D< HeatExchangerStruct > FluidHX;

	// Functions
	PlantComponent * HeatExchangerStruct::factory( int EP_UNUSED( objectType ), std::string objectName ) {
		if ( GetHXInput ) {
			GetFluidHeatExchangerInput();
			GetHXInput =  false;
		}

		for ( auto & hx : FluidHX) {
			if ( hx.Name == objectName ) {
				return &hx;
			}
		}
		// If we didn't find it, fatal
		ShowFatalError( "HeatExchangerStruct::factory: Error getting inputs for heat exchanger named: " + objectName );
		// Shut up the compiler
		return nullptr;

	}

	void HeatExchangerStruct::getDesignCapacities( const PlantLocation & calledFromLocation, Real64 & MaxLoad, Real64 & MinLoad, Real64 & OptLoad ) {
			if ( calledFromLocation.loopNum == this->DemandSideLoop.LoopNum ) {
				MinLoad = 0.0;
				MaxLoad = this->DemandSideLoop.MaxLoad;
				OptLoad = this->DemandSideLoop.MaxLoad * 0.9;
			} else if ( calledFromLocation.loopNum == this->SupplySideLoop.LoopNum ) {
				MinLoad = 0.0;
				MaxLoad = this->SupplySideLoop.MaxLoad;
				OptLoad = this->SupplySideLoop.MaxLoad * 0.9;
			}
	
	}

	void HeatExchangerStruct::onInitLoopEquip( const PlantLocation & calledFromLocation ) {

		this->init();

		this->size();

	}

	void HeatExchangerStruct::simulate( const PlantLocation & calledFromLocation, bool const FirstHVACIteration, Real64 & CurLoad ) {

		this->init();

		// for op scheme led HXs, only call controls if called from Loop Supply Side
		if ( ( this->ControlMode == OperationSchemeModulated ) || ( this->ControlMode == OperationSchemeOnOff ) ) {
			if ( calledFromLocation.loopNum == this->SupplySideLoop.LoopNum ) {
				this->control( CurLoad );
			}
		} else {
			this->control( CurLoad );
		}

		this->calc( Node( this->SupplySideLoop.InletNodeNum ).MassFlowRate, Node( this->DemandSideLoop.InletNodeNum ).MassFlowRate );

		this->update();

		this->report();

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

	void HeatExchangerStruct::init()
	{

		using PlantUtilities::InitComponentNodes;
		using PlantUtilities::InterConnectTwoPlantLoopSides;
		using FluidProperties::GetDensityGlycol;
		using FluidProperties::GetSpecificHeatGlycol;
		using DataGlobals::BeginEnvrnFlag;
		using DataGlobals::InitConvTemp;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineNameNoColon( "HeatExchangerStruct::init" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		bool errFlag;
		static std::string const RoutineName( "HeatExchangerStruct::init: " );
		Real64 rho;


		if ( this->myFlag ) {
			// locate the main two connections to the plant loops
			errFlag = false;
			ScanPlantLoopsForObject( this->Name, TypeOf_FluidToFluidPlantHtExchg, this->DemandSideLoop.LoopNum, this->DemandSideLoop.LoopSideNum, this->DemandSideLoop.BranchNum, this->DemandSideLoop.CompNum, _, _, _, this->DemandSideLoop.InletNodeNum, _, errFlag );

			if ( this->DemandSideLoop.LoopSideNum != DemandSide ) { // throw error
				ShowSevereError( RoutineName + " Invalid connections for " + ccSimPlantEquipTypes( TypeOf_FluidToFluidPlantHtExchg ) + " name = \"" + this->Name + "\"" );
				ShowContinueError( "The \"Loop Demand Side\" connections are not on the Demand Side of a plant loop" );
				errFlag = true;
			}

			ScanPlantLoopsForObject( this->Name, TypeOf_FluidToFluidPlantHtExchg, this->SupplySideLoop.LoopNum, this->SupplySideLoop.LoopSideNum, this->SupplySideLoop.BranchNum, this->SupplySideLoop.CompNum, _, _, _, this->SupplySideLoop.InletNodeNum, _, errFlag );

			if ( this->SupplySideLoop.LoopSideNum != SupplySide ) { // throw error
				ShowSevereError( RoutineName + " Invalid connections for " + ccSimPlantEquipTypes( TypeOf_FluidToFluidPlantHtExchg ) + " name = \"" + this->Name + "\"" );
				ShowContinueError( "The \"Loop Supply Side\" connections are not on the Supply Side of a plant loop" );
				errFlag = true;
			}

			// make sure it is not the same loop on both sides.
			if ( this->SupplySideLoop.LoopNum == this->DemandSideLoop.LoopNum ) { // user is being too tricky, don't allow
				ShowSevereError( RoutineName + " Invalid connections for " + ccSimPlantEquipTypes( TypeOf_FluidToFluidPlantHtExchg ) + " name = \"" + this->Name + "\"" );
				ShowContinueError( "The \"Loop Supply Side\" and \"Loop Demand Side\" need to be on different loops." );
				errFlag = true;
			} else {

				InterConnectTwoPlantLoopSides( this->SupplySideLoop.LoopNum, this->SupplySideLoop.LoopSideNum, this->DemandSideLoop.LoopNum, this->DemandSideLoop.LoopSideNum, TypeOf_FluidToFluidPlantHtExchg, true );
			}

			//find remote component if control mode is of that type.
			if ( this->ControlMode == CoolingSetPointOnOffWithComponentOverride ) {

				ScanPlantLoopsForNodeNum( RoutineName, this->OtherCompSupplySideLoop.InletNodeNum, this->OtherCompSupplySideLoop.LoopNum, this->OtherCompSupplySideLoop.LoopSideNum, this->OtherCompSupplySideLoop.BranchNum, this->OtherCompSupplySideLoop.CompNum );

				ScanPlantLoopsForNodeNum( RoutineName, this->OtherCompDemandSideLoop.InletNodeNum, this->OtherCompDemandSideLoop.LoopNum, this->OtherCompDemandSideLoop.LoopSideNum, this->OtherCompDemandSideLoop.BranchNum, this->OtherCompDemandSideLoop.CompNum );

				// revise how loads served category for other controlled equipment

				{ auto const SELECT_CASE_var( PlantLoop( this->OtherCompSupplySideLoop.LoopNum ).LoopSide( this->OtherCompSupplySideLoop.LoopSideNum ).Branch( this->OtherCompSupplySideLoop.BranchNum ).Comp( this->OtherCompSupplySideLoop.CompNum ).HowLoadServed );

				if ( SELECT_CASE_var == HowMet_ByNominalCap ) {
					PlantLoop( this->OtherCompSupplySideLoop.LoopNum ).LoopSide( this->OtherCompSupplySideLoop.LoopSideNum ).Branch( this->OtherCompSupplySideLoop.BranchNum ).Comp( this->OtherCompSupplySideLoop.CompNum ).HowLoadServed = HowMet_ByNominalCapFreeCoolCntrl;
				} else if ( SELECT_CASE_var == HowMet_ByNominalCapLowOutLimit ) {
					PlantLoop( this->OtherCompSupplySideLoop.LoopNum ).LoopSide( this->OtherCompSupplySideLoop.LoopSideNum ).Branch( this->OtherCompSupplySideLoop.BranchNum ).Comp( this->OtherCompSupplySideLoop.CompNum ).HowLoadServed = HowMet_ByNominalCapLowOutLimitFreeCoolCntrl;
				}}

				{ auto const SELECT_CASE_var( this->ControlSignalTemp );
				if ( SELECT_CASE_var == WetBulbTemperature ) {
					PlantLoop( this->OtherCompSupplySideLoop.LoopNum ).LoopSide( this->OtherCompSupplySideLoop.LoopSideNum ).Branch( this->OtherCompSupplySideLoop.BranchNum ).Comp( this->OtherCompSupplySideLoop.CompNum ).FreeCoolCntrlMode = FreeCoolControlMode_WetBulb;
				} else if ( SELECT_CASE_var == DryBulbTemperature ) {
					PlantLoop( this->OtherCompSupplySideLoop.LoopNum ).LoopSide( this->OtherCompSupplySideLoop.LoopSideNum ).Branch( this->OtherCompSupplySideLoop.BranchNum ).Comp( this->OtherCompSupplySideLoop.CompNum ).FreeCoolCntrlMode = FreeCoolControlMode_DryBulb;
				} else if ( SELECT_CASE_var == LoopTemperature ) {
					PlantLoop( this->OtherCompSupplySideLoop.LoopNum ).LoopSide( this->OtherCompSupplySideLoop.LoopSideNum ).Branch( this->OtherCompSupplySideLoop.BranchNum ).Comp( this->OtherCompSupplySideLoop.CompNum ).FreeCoolCntrlMode = FreeCoolControlMode_Loop;
					PlantLoop( this->OtherCompSupplySideLoop.LoopNum ).LoopSide( this->OtherCompSupplySideLoop.LoopSideNum ).Branch( this->OtherCompSupplySideLoop.BranchNum ).Comp( this->OtherCompSupplySideLoop.CompNum ).FreeCoolCntrlNodeNum = this->OtherCompDemandSideLoop.InletNodeNum;
				}}

			}
			if ( this->ControlMode == TrackComponentOnOff ) {
				if ( this->OtherCompSupplySideLoop.InletNodeNum > 0 ) {
					ScanPlantLoopsForObject( this->ComponentUserName, this->ComponentTypeOfNum, this->OtherCompSupplySideLoop.LoopNum, this->OtherCompSupplySideLoop.LoopSideNum, this->OtherCompSupplySideLoop.BranchNum, this->OtherCompSupplySideLoop.CompNum, _, _, _, this->OtherCompSupplySideLoop.InletNodeNum, _, errFlag );
				}
				if ( this->OtherCompDemandSideLoop.InletNodeNum > 0 ) {
					ScanPlantLoopsForObject( this->ComponentUserName, this->ComponentTypeOfNum, this->OtherCompDemandSideLoop.LoopNum, this->OtherCompDemandSideLoop.LoopSideNum, this->OtherCompDemandSideLoop.BranchNum, this->OtherCompDemandSideLoop.CompNum, _, _, _, this->OtherCompDemandSideLoop.InletNodeNum, _, errFlag );
				}
			}

			if ( errFlag ) {
				ShowFatalError( RoutineName + "Program terminated due to previous condition(s)." );
			}
			this->myFlag = false;
		} // plant setup

		if ( BeginEnvrnFlag && this->myEnvrnFlag && ( PlantFirstSizesOkayToFinalize ) ) {

			rho = GetDensityGlycol( PlantLoop( this->DemandSideLoop.LoopNum ).FluidName, InitConvTemp, PlantLoop( this->DemandSideLoop.LoopNum ).FluidIndex, RoutineNameNoColon );
			this->DemandSideLoop.MassFlowRateMax = rho * this->DemandSideLoop.DesignVolumeFlowRate;
			InitComponentNodes( this->DemandSideLoop.MassFlowRateMin, this->DemandSideLoop.MassFlowRateMax, this->DemandSideLoop.InletNodeNum, this->DemandSideLoop.OutletNodeNum, this->DemandSideLoop.LoopNum, this->DemandSideLoop.LoopSideNum, this->DemandSideLoop.BranchNum, this->DemandSideLoop.CompNum );

			rho = GetDensityGlycol( PlantLoop( this->SupplySideLoop.LoopNum ).FluidName, InitConvTemp, PlantLoop( this->SupplySideLoop.LoopNum ).FluidIndex, RoutineNameNoColon );
			this->SupplySideLoop.MassFlowRateMax = rho * this->SupplySideLoop.DesignVolumeFlowRate;
			InitComponentNodes( this->SupplySideLoop.MassFlowRateMin, this->SupplySideLoop.MassFlowRateMax, this->SupplySideLoop.InletNodeNum, this->SupplySideLoop.OutletNodeNum, this->SupplySideLoop.LoopNum, this->SupplySideLoop.LoopSideNum, this->SupplySideLoop.BranchNum, this->SupplySideLoop.CompNum );
			this->myEnvrnFlag = false;
		}
		if ( ! BeginEnvrnFlag ) {
			this->myEnvrnFlag = true;
		}

		this->DemandSideLoop.InletTemp = Node( this->DemandSideLoop.InletNodeNum ).Temp;
		this->SupplySideLoop.InletTemp = Node( this->SupplySideLoop.InletNodeNum ).Temp;

		if ( this->ControlMode == CoolingSetPointOnOffWithComponentOverride ) {
			// store current value for setpoint in central plant loop data structure

			PlantLoop( this->OtherCompSupplySideLoop.LoopNum ).LoopSide( this->OtherCompSupplySideLoop.LoopSideNum ).Branch( this->OtherCompSupplySideLoop.BranchNum ).Comp( this->OtherCompSupplySideLoop.CompNum ).FreeCoolCntrlMinCntrlTemp = Node( this->SetPointNodeNum ).TempSetPoint;
		}

	}

	void HeatExchangerStruct::size() {

		// PURPOSE OF THIS SUBROUTINE:
		// Size plant heat exchanger flow rates, UA, and max capacity

		// METHODOLOGY EMPLOYED:
		// the supply side flow rate is obtained from the plant sizing structure
		// the demand side is sized to match the supply side
		// the UA is sized for an effectiveness of 1.0 using sizing temps
		// the capacity uses the full HX model


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
		static std::string const RoutineName( "HeatExchangerStruct::size" );

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
		PltSizNumSupSide = PlantLoop( this->SupplySideLoop.LoopNum ).PlantSizNum;
		PltSizNumDmdSide = PlantLoop( this->DemandSideLoop.LoopNum ).PlantSizNum;
		tmpSupSideDesignVolFlowRate = this->SupplySideLoop.DesignVolumeFlowRate;
		if ( this->SupplySideLoop.DesignVolumeFlowRateWasAutoSized ) {
			if ( PltSizNumSupSide > 0 ) {
				if ( PlantSizData( PltSizNumSupSide ).DesVolFlowRate >= SmallWaterVolFlow ) {
					tmpSupSideDesignVolFlowRate = PlantSizData( PltSizNumSupSide ).DesVolFlowRate * this->SizingFactor;
					if ( PlantFirstSizesOkayToFinalize ) this->SupplySideLoop.DesignVolumeFlowRate = tmpSupSideDesignVolFlowRate;
				} else {
					tmpSupSideDesignVolFlowRate = 0.0;
					if ( PlantFirstSizesOkayToFinalize ) this->SupplySideLoop.DesignVolumeFlowRate = tmpSupSideDesignVolFlowRate;
				}
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( "HeatExchanger:FluidToFluid", this->Name,
						"Loop Supply Side Design Fluid Flow Rate [m3/s]", this->SupplySideLoop.DesignVolumeFlowRate );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( "HeatExchanger:FluidToFluid", this->Name,
						"Initial Loop Supply Side Design Fluid Flow Rate [m3/s]", this->SupplySideLoop.DesignVolumeFlowRate );
				}
			} else {
				if ( PlantFirstSizesOkayToFinalize ) {
					ShowSevereError( "SizeFluidHeatExchanger: Autosizing of requires a loop Sizing:Plant object" );
					ShowContinueError( "Occurs in heat exchanger object=" + this->Name );
					ErrorsFound = true;
				}
			}
		}
		RegisterPlantCompDesignFlow( this->SupplySideLoop.InletNodeNum, tmpSupSideDesignVolFlowRate );

		// second deal with Loop Demand Side
		tmpDmdSideDesignVolFlowRate = this->DemandSideLoop.DesignVolumeFlowRate;
		if ( this->DemandSideLoop.DesignVolumeFlowRateWasAutoSized ) {
			if ( tmpSupSideDesignVolFlowRate > SmallWaterVolFlow ) {
				tmpDmdSideDesignVolFlowRate = tmpSupSideDesignVolFlowRate;
				if ( PlantFirstSizesOkayToFinalize ) this->DemandSideLoop.DesignVolumeFlowRate = tmpDmdSideDesignVolFlowRate;
			} else {
				tmpDmdSideDesignVolFlowRate = 0.0;
				if ( PlantFirstSizesOkayToFinalize ) this->DemandSideLoop.DesignVolumeFlowRate = tmpDmdSideDesignVolFlowRate;
			}
			if ( PlantFinalSizesOkayToReport ) {
				ReportSizingOutput( "HeatExchanger:FluidToFluid", this->Name,
					"Loop Demand Side Design Fluid Flow Rate [m3/s]", this->DemandSideLoop.DesignVolumeFlowRate );
			}
			if ( PlantFirstSizesOkayToReport ) {
				ReportSizingOutput( "HeatExchanger:FluidToFluid", this->Name,
					"Initial Loop Demand Side Design Fluid Flow Rate [m3/s]", this->DemandSideLoop.DesignVolumeFlowRate );
			}
		}
		RegisterPlantCompDesignFlow( this->DemandSideLoop.InletNodeNum, tmpDmdSideDesignVolFlowRate );

		// size UA if needed
		tmpUA = this->UA;
		if ( this->UAWasAutoSized ) {
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

					Cp = GetSpecificHeatGlycol( PlantLoop( this->SupplySideLoop.LoopNum ).FluidName, InitConvTemp, PlantLoop( this->SupplySideLoop.LoopNum ).FluidIndex, RoutineName );

					rho = GetDensityGlycol( PlantLoop( this->SupplySideLoop.LoopNum ).FluidName, InitConvTemp, PlantLoop( this->SupplySideLoop.LoopNum ).FluidIndex, RoutineName );

					tmpDesCap = Cp * rho * tmpDeltaTSupLoop * tmpSupSideDesignVolFlowRate;
					tmpUA = tmpDesCap / tmpDeltaTloopToLoop;
					if ( PlantFirstSizesOkayToFinalize ) this->UA = tmpUA;
				} else {
					tmpUA = 0.0;
					if ( PlantFirstSizesOkayToFinalize ) this->UA = tmpUA;
				}
				if ( PlantFinalSizesOkayToReport ) {
					ReportSizingOutput( "HeatExchanger:FluidToFluid", this->Name,
						"Heat Exchanger U-Factor Times Area Value [W/C]", this->UA );
					ReportSizingOutput( "HeatExchanger:FluidToFluid", this->Name,
						"Loop-to-loop Temperature Difference Used to Size Heat Exchanger U-Factor Times Area Value [C]", tmpDeltaTloopToLoop );
				}
				if ( PlantFirstSizesOkayToReport ) {
					ReportSizingOutput( "HeatExchanger:FluidToFluid", this->Name,
						"Initial Heat Exchanger U-Factor Times Area Value [W/C]", this->UA );
					ReportSizingOutput( "HeatExchanger:FluidToFluid", this->Name,
						"Initial Loop-to-loop Temperature Difference Used to Size Heat Exchanger U-Factor Times Area Value [C]", tmpDeltaTloopToLoop );
				}
			} else {
				if ( PlantFirstSizesOkayToFinalize ) {
					ShowSevereError( "SizeFluidHeatExchanger: Autosizing of heat Exchanger UA requires a loop Sizing:Plant objects for both loops" );
					ShowContinueError( "Occurs in heat exchanger object=" + this->Name );
					ErrorsFound = true;
				}
			}

		}

		// size capacities for load range based op schemes
		if ( PlantFirstSizesOkayToFinalize ) {

			if ( PltSizNumSupSide > 0 ) {
				{ auto const SELECT_CASE_var( PlantSizData( PltSizNumSupSide ).LoopType );
				if ( SELECT_CASE_var == HeatingLoop ) {
					Node( this->SupplySideLoop.InletNodeNum ).Temp = ( PlantSizData( PltSizNumSupSide ).ExitTemp - PlantSizData( PltSizNumSupSide ).DeltaT );
				} else if ( SELECT_CASE_var == CoolingLoop ) {
					Node( this->SupplySideLoop.InletNodeNum ).Temp = ( PlantSizData( PltSizNumSupSide ).ExitTemp + PlantSizData( PltSizNumSupSide ).DeltaT );
				} else if ( SELECT_CASE_var == CondenserLoop ) {
					Node( this->SupplySideLoop.InletNodeNum ).Temp = ( PlantSizData( PltSizNumSupSide ).ExitTemp + PlantSizData( PltSizNumSupSide ).DeltaT );
				} else if ( SELECT_CASE_var == SteamLoop ) {
					Node( this->SupplySideLoop.InletNodeNum ).Temp = ( PlantSizData( PltSizNumSupSide ).ExitTemp - PlantSizData( PltSizNumSupSide ).DeltaT );
				}}

			} else { // don't rely on sizing, use loop setpoints
				// loop supply side
				if ( PlantLoop( this->SupplySideLoop.LoopNum ).LoopDemandCalcScheme == SingleSetPoint ) {
					Node( this->SupplySideLoop.InletNodeNum ).Temp = Node( PlantLoop( this->SupplySideLoop.LoopNum ).TempSetPointNodeNum ).TempSetPoint;
				} else if ( PlantLoop( this->SupplySideLoop.LoopNum ).LoopDemandCalcScheme == DualSetPointDeadBand ) {
					Node( this->SupplySideLoop.InletNodeNum ).Temp = ( Node( PlantLoop( this->SupplySideLoop.LoopNum ).TempSetPointNodeNum ).TempSetPointHi + Node( PlantLoop( this->SupplySideLoop.LoopNum ).TempSetPointNodeNum ).TempSetPointLo ) / 2.0;
				}

			}

			if ( PltSizNumDmdSide > 0 ) {
				Node( this->DemandSideLoop.InletNodeNum ).Temp = PlantSizData( PltSizNumDmdSide ).ExitTemp;
			} else { // don't rely on sizing, use loop setpoints
				// loop demand side
				if ( PlantLoop( this->DemandSideLoop.LoopNum ).LoopDemandCalcScheme == SingleSetPoint ) {
					Node( this->DemandSideLoop.InletNodeNum ).Temp = Node( PlantLoop( this->DemandSideLoop.LoopNum ).TempSetPointNodeNum ).TempSetPoint;
				} else if ( PlantLoop( this->DemandSideLoop.LoopNum ).LoopDemandCalcScheme == DualSetPointDeadBand ) {
					Node( this->DemandSideLoop.InletNodeNum ).Temp = ( Node( PlantLoop( this->DemandSideLoop.LoopNum ).TempSetPointNodeNum ).TempSetPointHi + Node( PlantLoop( this->DemandSideLoop.LoopNum ).TempSetPointNodeNum ).TempSetPointLo ) / 2.0;
				}
			}

			rho = GetDensityGlycol( PlantLoop( this->SupplySideLoop.LoopNum ).FluidName, InitConvTemp, PlantLoop( this->SupplySideLoop.LoopNum ).FluidIndex, RoutineName );
			SupSideMdot = this->SupplySideLoop.DesignVolumeFlowRate * rho;
			rho = GetDensityGlycol( PlantLoop( this->DemandSideLoop.LoopNum ).FluidName, InitConvTemp, PlantLoop( this->DemandSideLoop.LoopNum ).FluidIndex, RoutineName );
			DmdSideMdot = this->DemandSideLoop.DesignVolumeFlowRate * rho;

			this->calc( SupSideMdot, DmdSideMdot );
			this->SupplySideLoop.MaxLoad = std::abs( this->HeatTransferRate );

		}
		if ( PlantFinalSizesOkayToReport ) {
			PreDefTableEntry( pdchMechType, this->Name, "HeatExchanger:FluidToFluid" );
			PreDefTableEntry( pdchMechNomCap, this->Name, this->SupplySideLoop.MaxLoad );
		}

	}

	void HeatExchangerStruct::control( Real64 const MyLoad ) {


		// PURPOSE OF THIS SUBROUTINE:
		// determine control state for fluid to fluid heat exchanger
		// make fluid flow requests accordingly

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
		AvailSchedValue = GetCurrentScheduleValue( this->AvailSchedNum );
		if ( AvailSchedValue <= 0 ) {
			ScheduledOff = true;
		} else {
			ScheduledOff = false;
		}

		// check if operational limits trip off unit
		LimitTrippedOff = false;
		if ( ( Node( this->SupplySideLoop.InletNodeNum ).Temp < this->MinOperationTemp ) || ( Node( this->DemandSideLoop.InletNodeNum ).Temp < this->MinOperationTemp ) ) {
			LimitTrippedOff = true;
		}
		if ( ( Node( this->SupplySideLoop.InletNodeNum ).Temp > this->MaxOperationTemp ) || ( Node( this->DemandSideLoop.InletNodeNum ).Temp > this->MaxOperationTemp ) ) {
			LimitTrippedOff = true;
		}

		if ( ! ScheduledOff && ! LimitTrippedOff ) {

			{ auto const SELECT_CASE_var( this->ControlMode );

			if ( SELECT_CASE_var == UncontrolledOn ) {

				// make passive request for supply side loop flow
				mdotSupSide = this->SupplySideLoop.MassFlowRateMax;
				SetComponentFlowRate( mdotSupSide, this->SupplySideLoop.InletNodeNum, this->SupplySideLoop.OutletNodeNum, this->SupplySideLoop.LoopNum, this->SupplySideLoop.LoopSideNum, this->SupplySideLoop.BranchNum, this->SupplySideLoop.CompNum );
				if ( mdotSupSide > MassFlowTolerance ) {
					// if supply side loop has massflow, request demand side flow
					mdotDmdSide = this->DemandSideLoop.MassFlowRateMax;
				} else {
					mdotDmdSide = 0.0;
				}
				SetComponentFlowRate( mdotDmdSide, this->DemandSideLoop.InletNodeNum, this->DemandSideLoop.OutletNodeNum, this->DemandSideLoop.LoopNum, this->DemandSideLoop.LoopSideNum, this->DemandSideLoop.BranchNum, this->DemandSideLoop.CompNum );

			} else if ( SELECT_CASE_var == OperationSchemeModulated ) {

				if ( std::abs( MyLoad ) > SmallLoad ) {
					if ( MyLoad < -1.0 * SmallLoad ) { // requesting cooling
						DeltaTCooling = this->SupplySideLoop.InletTemp - this->DemandSideLoop.InletTemp;
						if ( DeltaTCooling > this->TempControlTol ) { // can do cooling so turn on
							mdotSupSide = this->SupplySideLoop.MassFlowRateMax;
							SetComponentFlowRate( mdotSupSide, this->SupplySideLoop.InletNodeNum, this->SupplySideLoop.OutletNodeNum, this->SupplySideLoop.LoopNum, this->SupplySideLoop.LoopSideNum, this->SupplySideLoop.BranchNum, this->SupplySideLoop.CompNum );
							if ( mdotSupSide > MassFlowTolerance ) {
								// if supply side loop has massflow, request demand side flow
								cp = GetSpecificHeatGlycol( PlantLoop( this->SupplySideLoop.LoopNum ).FluidName, this->SupplySideLoop.InletTemp, PlantLoop( this->SupplySideLoop.LoopNum ).FluidIndex, RoutineName );
								TargetLeavingTemp = this->SupplySideLoop.InletTemp - std::abs( MyLoad ) / ( cp * mdotSupSide );

								this->findHXDemandSideLoopFlow( TargetLeavingTemp, CoolingSupplySideLoop );
							} else { // no flow on supply side so do not request flow on demand side
								mdotDmdSide = 0.0;
								SetComponentFlowRate( mdotDmdSide, this->DemandSideLoop.InletNodeNum, this->DemandSideLoop.OutletNodeNum, this->DemandSideLoop.LoopNum, this->DemandSideLoop.LoopSideNum, this->DemandSideLoop.BranchNum, this->SupplySideLoop.CompNum );
							}
						} else { // not able to cool so turn off
							mdotSupSide = 0.0;
							SetComponentFlowRate( mdotSupSide, this->SupplySideLoop.InletNodeNum, this->SupplySideLoop.OutletNodeNum, this->SupplySideLoop.LoopNum, this->SupplySideLoop.LoopSideNum, this->SupplySideLoop.BranchNum, this->SupplySideLoop.CompNum );
							mdotDmdSide = 0.0;
							SetComponentFlowRate( mdotDmdSide, this->DemandSideLoop.InletNodeNum, this->DemandSideLoop.OutletNodeNum, this->DemandSideLoop.LoopNum, this->DemandSideLoop.LoopSideNum, this->DemandSideLoop.BranchNum, this->SupplySideLoop.CompNum );
						}

					} else { // requesting heating
						DeltaTHeating = this->DemandSideLoop.InletTemp - this->SupplySideLoop.InletTemp;
						if ( DeltaTHeating > this->TempControlTol ) { // can do heating so turn on
							mdotSupSide = this->SupplySideLoop.MassFlowRateMax;
							SetComponentFlowRate( mdotSupSide, this->SupplySideLoop.InletNodeNum, this->SupplySideLoop.OutletNodeNum, this->SupplySideLoop.LoopNum, this->SupplySideLoop.LoopSideNum, this->SupplySideLoop.BranchNum, this->SupplySideLoop.CompNum );
							if ( mdotSupSide > MassFlowTolerance ) {
								cp = GetSpecificHeatGlycol( PlantLoop( this->SupplySideLoop.LoopNum ).FluidName, this->SupplySideLoop.InletTemp, PlantLoop( this->SupplySideLoop.LoopNum ).FluidIndex, RoutineName );
								TargetLeavingTemp = this->SupplySideLoop.InletTemp + std::abs( MyLoad ) / ( cp * mdotSupSide );

								this->findHXDemandSideLoopFlow( TargetLeavingTemp, HeatingSupplySideLoop );
							} else { // no flow on supply side so do not request flow on demand side
								mdotDmdSide = 0.0;
								SetComponentFlowRate( mdotDmdSide, this->DemandSideLoop.InletNodeNum, this->DemandSideLoop.OutletNodeNum, this->DemandSideLoop.LoopNum, this->DemandSideLoop.LoopSideNum, this->DemandSideLoop.BranchNum, this->DemandSideLoop.CompNum );
							}
						} else { // not able to heat so turn off
							mdotSupSide = 0.0;
							SetComponentFlowRate( mdotSupSide, this->SupplySideLoop.InletNodeNum, this->SupplySideLoop.OutletNodeNum, this->SupplySideLoop.LoopNum, this->SupplySideLoop.LoopSideNum, this->SupplySideLoop.BranchNum, this->SupplySideLoop.CompNum );
							mdotDmdSide = 0.0;
							SetComponentFlowRate( mdotDmdSide, this->DemandSideLoop.InletNodeNum, this->DemandSideLoop.OutletNodeNum, this->DemandSideLoop.LoopNum, this->DemandSideLoop.LoopSideNum, this->DemandSideLoop.BranchNum, this->DemandSideLoop.CompNum );
						}
					}

				} else { //  no load
					mdotSupSide = 0.0;
					SetComponentFlowRate( mdotSupSide, this->SupplySideLoop.InletNodeNum, this->SupplySideLoop.OutletNodeNum, this->SupplySideLoop.LoopNum, this->SupplySideLoop.LoopSideNum, this->SupplySideLoop.BranchNum, this->SupplySideLoop.CompNum );
					mdotDmdSide = 0.0;
					SetComponentFlowRate( mdotDmdSide, this->DemandSideLoop.InletNodeNum, this->DemandSideLoop.OutletNodeNum, this->DemandSideLoop.LoopNum, this->DemandSideLoop.LoopSideNum, this->DemandSideLoop.BranchNum, this->DemandSideLoop.CompNum );
				}

			} else if ( SELECT_CASE_var == OperationSchemeOnOff ) {
				if ( std::abs( MyLoad ) > SmallLoad ) {
					if ( MyLoad < SmallLoad ) { // requesting cooling
						DeltaTCooling = this->SupplySideLoop.InletTemp - this->DemandSideLoop.InletTemp;
						if ( DeltaTCooling > this->TempControlTol ) { // can do cooling so turn on
							mdotSupSide = this->SupplySideLoop.MassFlowRateMax;
							SetComponentFlowRate( mdotSupSide, this->SupplySideLoop.InletNodeNum, this->SupplySideLoop.OutletNodeNum, this->SupplySideLoop.LoopNum, this->SupplySideLoop.LoopSideNum, this->SupplySideLoop.BranchNum, this->SupplySideLoop.CompNum );
							if ( mdotSupSide > MassFlowTolerance ) {
								mdotDmdSide = this->DemandSideLoop.MassFlowRateMax;
							} else {
								mdotDmdSide = 0.0;
							}

							SetComponentFlowRate( mdotDmdSide, this->DemandSideLoop.InletNodeNum, this->DemandSideLoop.OutletNodeNum, this->DemandSideLoop.LoopNum, this->DemandSideLoop.LoopSideNum, this->DemandSideLoop.BranchNum, this->DemandSideLoop.CompNum );
						} else { // not able to cool so turn off
							mdotSupSide = 0.0;
							SetComponentFlowRate( mdotSupSide, this->SupplySideLoop.InletNodeNum, this->SupplySideLoop.OutletNodeNum, this->SupplySideLoop.LoopNum, this->SupplySideLoop.LoopSideNum, this->SupplySideLoop.BranchNum, this->SupplySideLoop.CompNum );
							mdotDmdSide = 0.0;
							SetComponentFlowRate( mdotDmdSide, this->DemandSideLoop.InletNodeNum, this->DemandSideLoop.OutletNodeNum, this->DemandSideLoop.LoopNum, this->DemandSideLoop.LoopSideNum, this->DemandSideLoop.BranchNum, this->DemandSideLoop.CompNum );
						}

					} else { // requesting heating
						DeltaTHeating = this->DemandSideLoop.InletTemp - this->SupplySideLoop.InletTemp;
						if ( DeltaTHeating > this->TempControlTol ) { // can do heating so turn on
							mdotSupSide = this->SupplySideLoop.MassFlowRateMax;
							SetComponentFlowRate( mdotSupSide, this->SupplySideLoop.InletNodeNum, this->SupplySideLoop.OutletNodeNum, this->SupplySideLoop.LoopNum, this->SupplySideLoop.LoopSideNum, this->SupplySideLoop.BranchNum, this->SupplySideLoop.CompNum );
							if ( mdotSupSide > MassFlowTolerance ) {
								mdotDmdSide = this->DemandSideLoop.MassFlowRateMax;
							} else {
								mdotDmdSide = 0.0;
							}
							SetComponentFlowRate( mdotDmdSide, this->DemandSideLoop.InletNodeNum, this->DemandSideLoop.OutletNodeNum, this->DemandSideLoop.LoopNum, this->DemandSideLoop.LoopSideNum, this->DemandSideLoop.BranchNum, this->DemandSideLoop.CompNum );
						} else { // not able to heat so turn off
							mdotSupSide = 0.0;
							SetComponentFlowRate( mdotSupSide, this->SupplySideLoop.InletNodeNum, this->SupplySideLoop.OutletNodeNum, this->SupplySideLoop.LoopNum, this->SupplySideLoop.LoopSideNum, this->SupplySideLoop.BranchNum, this->SupplySideLoop.CompNum );
							mdotDmdSide = 0.0;
							SetComponentFlowRate( mdotDmdSide, this->DemandSideLoop.InletNodeNum, this->DemandSideLoop.OutletNodeNum, this->DemandSideLoop.LoopNum, this->DemandSideLoop.LoopSideNum, this->DemandSideLoop.BranchNum, this->DemandSideLoop.CompNum );
						}
					}

				} else { // no load
					mdotSupSide = 0.0;
					SetComponentFlowRate( mdotSupSide, this->SupplySideLoop.InletNodeNum, this->SupplySideLoop.OutletNodeNum, this->SupplySideLoop.LoopNum, this->SupplySideLoop.LoopSideNum, this->SupplySideLoop.BranchNum, this->SupplySideLoop.CompNum );
					mdotDmdSide = 0.0;
					SetComponentFlowRate( mdotDmdSide, this->DemandSideLoop.InletNodeNum, this->DemandSideLoop.OutletNodeNum, this->DemandSideLoop.LoopNum, this->DemandSideLoop.LoopSideNum, this->DemandSideLoop.BranchNum, this->DemandSideLoop.CompNum );
				}

			} else if ( SELECT_CASE_var == HeatingSetPointModulated ) {

				SetPointTemp = Node( this->SetPointNodeNum ).TempSetPoint;
				DeltaTHeating = this->DemandSideLoop.InletTemp - this->SupplySideLoop.InletTemp;
				if ( ( DeltaTHeating > this->TempControlTol ) && ( SetPointTemp > this->SupplySideLoop.InletTemp ) ) {
					// can and want to heat
					mdotSupSide = this->SupplySideLoop.MassFlowRateMax;
					SetComponentFlowRate( mdotSupSide, this->SupplySideLoop.InletNodeNum, this->SupplySideLoop.OutletNodeNum, this->SupplySideLoop.LoopNum, this->SupplySideLoop.LoopSideNum, this->SupplySideLoop.BranchNum, this->SupplySideLoop.CompNum );
					if ( mdotSupSide > MassFlowTolerance ) {

						TargetLeavingTemp = SetPointTemp;
						this->findHXDemandSideLoopFlow( TargetLeavingTemp, HeatingSupplySideLoop );
					} else {
						mdotDmdSide = 0.0;
						SetComponentFlowRate( mdotDmdSide, this->DemandSideLoop.InletNodeNum, this->DemandSideLoop.OutletNodeNum, this->DemandSideLoop.LoopNum, this->DemandSideLoop.LoopSideNum, this->DemandSideLoop.BranchNum, this->DemandSideLoop.CompNum );
					}
				} else { // not able are wanting to heat so turn off
					mdotSupSide = 0.0;
					SetComponentFlowRate( mdotSupSide, this->SupplySideLoop.InletNodeNum, this->SupplySideLoop.OutletNodeNum, this->SupplySideLoop.LoopNum, this->SupplySideLoop.LoopSideNum, this->SupplySideLoop.BranchNum, this->SupplySideLoop.CompNum );
					mdotDmdSide = 0.0;
					SetComponentFlowRate( mdotDmdSide, this->DemandSideLoop.InletNodeNum, this->DemandSideLoop.OutletNodeNum, this->DemandSideLoop.LoopNum, this->DemandSideLoop.LoopSideNum, this->DemandSideLoop.BranchNum, this->DemandSideLoop.CompNum );
				}

			} else if ( SELECT_CASE_var == HeatingSetPointOnOff ) {

				SetPointTemp = Node( this->SetPointNodeNum ).TempSetPoint;
				DeltaTHeating = this->DemandSideLoop.InletTemp - this->SupplySideLoop.InletTemp;
				if ( ( DeltaTHeating > this->TempControlTol ) && ( SetPointTemp > this->SupplySideLoop.InletTemp ) ) {
					// can and want to heat
					mdotSupSide = this->SupplySideLoop.MassFlowRateMax;
					SetComponentFlowRate( mdotSupSide, this->SupplySideLoop.InletNodeNum, this->SupplySideLoop.OutletNodeNum, this->SupplySideLoop.LoopNum, this->SupplySideLoop.LoopSideNum, this->SupplySideLoop.BranchNum, this->SupplySideLoop.CompNum );
					if ( mdotSupSide > MassFlowTolerance ) {
						mdotDmdSide = this->DemandSideLoop.MassFlowRateMax;
					} else {
						mdotDmdSide = 0.0;
					}
					SetComponentFlowRate( mdotDmdSide, this->DemandSideLoop.InletNodeNum, this->DemandSideLoop.OutletNodeNum, this->DemandSideLoop.LoopNum, this->DemandSideLoop.LoopSideNum, this->DemandSideLoop.BranchNum, this->DemandSideLoop.CompNum );
				} else { // not able are wanting to heat so turn off
					mdotSupSide = 0.0;
					SetComponentFlowRate( mdotSupSide, this->SupplySideLoop.InletNodeNum, this->SupplySideLoop.OutletNodeNum, this->SupplySideLoop.LoopNum, this->SupplySideLoop.LoopSideNum, this->SupplySideLoop.BranchNum, this->SupplySideLoop.CompNum );
					mdotDmdSide = 0.0;
					SetComponentFlowRate( mdotDmdSide, this->DemandSideLoop.InletNodeNum, this->DemandSideLoop.OutletNodeNum, this->DemandSideLoop.LoopNum, this->DemandSideLoop.LoopSideNum, this->DemandSideLoop.BranchNum, this->DemandSideLoop.CompNum );
				}

			} else if ( SELECT_CASE_var == CoolingSetPointModulated ) {

				SetPointTemp = Node( this->SetPointNodeNum ).TempSetPoint;
				DeltaTCooling = this->SupplySideLoop.InletTemp - this->DemandSideLoop.InletTemp;
				if ( ( DeltaTCooling > this->TempControlTol ) && ( SetPointTemp < this->SupplySideLoop.InletTemp ) ) {
					// can and want to cool
					mdotSupSide = this->SupplySideLoop.MassFlowRateMax;
					SetComponentFlowRate( mdotSupSide, this->SupplySideLoop.InletNodeNum, this->SupplySideLoop.OutletNodeNum, this->SupplySideLoop.LoopNum, this->SupplySideLoop.LoopSideNum, this->SupplySideLoop.BranchNum, this->SupplySideLoop.CompNum );
					if ( mdotSupSide > MassFlowTolerance ) {
						TargetLeavingTemp = SetPointTemp;
						this->findHXDemandSideLoopFlow( TargetLeavingTemp, CoolingSupplySideLoop );
					} else {
						mdotDmdSide = 0.0;
						SetComponentFlowRate( mdotDmdSide, this->DemandSideLoop.InletNodeNum, this->DemandSideLoop.OutletNodeNum, this->DemandSideLoop.LoopNum, this->DemandSideLoop.LoopSideNum, this->DemandSideLoop.BranchNum, this->DemandSideLoop.CompNum );
					}
				} else { // not able are wanting to cool so turn off
					mdotSupSide = 0.0;
					SetComponentFlowRate( mdotSupSide, this->SupplySideLoop.InletNodeNum, this->SupplySideLoop.OutletNodeNum, this->SupplySideLoop.LoopNum, this->SupplySideLoop.LoopSideNum, this->SupplySideLoop.BranchNum, this->SupplySideLoop.CompNum );
					mdotDmdSide = 0.0;
					SetComponentFlowRate( mdotDmdSide, this->DemandSideLoop.InletNodeNum, this->DemandSideLoop.OutletNodeNum, this->DemandSideLoop.LoopNum, this->DemandSideLoop.LoopSideNum, this->DemandSideLoop.BranchNum, this->DemandSideLoop.CompNum );
				}

			} else if ( SELECT_CASE_var == CoolingSetPointOnOff ) {

				SetPointTemp = Node( this->SetPointNodeNum ).TempSetPoint;
				DeltaTCooling = this->SupplySideLoop.InletTemp - this->DemandSideLoop.InletTemp;
				if ( ( DeltaTCooling > this->TempControlTol ) && ( SetPointTemp < this->SupplySideLoop.InletTemp ) ) {
					// can and want to cool
					mdotSupSide = this->SupplySideLoop.MassFlowRateMax;
					SetComponentFlowRate( mdotSupSide, this->SupplySideLoop.InletNodeNum, this->SupplySideLoop.OutletNodeNum, this->SupplySideLoop.LoopNum, this->SupplySideLoop.LoopSideNum, this->SupplySideLoop.BranchNum, this->SupplySideLoop.CompNum );
					if ( mdotSupSide > MassFlowTolerance ) {
						mdotDmdSide = this->DemandSideLoop.MassFlowRateMax;
					} else {
						mdotDmdSide = 0.0;
					}
					SetComponentFlowRate( mdotDmdSide, this->DemandSideLoop.InletNodeNum, this->DemandSideLoop.OutletNodeNum, this->DemandSideLoop.LoopNum, this->DemandSideLoop.LoopSideNum, this->DemandSideLoop.BranchNum, this->DemandSideLoop.CompNum );
				} else { // not able or are wanting to cool so turn off
					mdotSupSide = 0.0;
					SetComponentFlowRate( mdotSupSide, this->SupplySideLoop.InletNodeNum, this->SupplySideLoop.OutletNodeNum, this->SupplySideLoop.LoopNum, this->SupplySideLoop.LoopSideNum, this->SupplySideLoop.BranchNum, this->SupplySideLoop.CompNum );
					mdotDmdSide = 0.0;
					SetComponentFlowRate( mdotDmdSide, this->DemandSideLoop.InletNodeNum, this->DemandSideLoop.OutletNodeNum, this->DemandSideLoop.LoopNum, this->DemandSideLoop.LoopSideNum, this->DemandSideLoop.BranchNum, this->DemandSideLoop.CompNum );
				}

			} else if ( SELECT_CASE_var == DualDeadBandSetPointModulated ) {

				SetPointTempLo = Node( this->SetPointNodeNum ).TempSetPointLo;
				SetPointTempHi = Node( this->SetPointNodeNum ).TempSetPointHi;
				DeltaTCooling = this->SupplySideLoop.InletTemp - this->DemandSideLoop.InletTemp;
				DeltaTCoolSetPointDemand = SetPointTempHi - this->DemandSideLoop.InletTemp;
				DeltaTCoolSetPointSupply = SetPointTempHi - this->SupplySideLoop.InletTemp;
				DeltaTHeating = this->DemandSideLoop.InletTemp - this->SupplySideLoop.InletTemp;
				DeltaTHeatSetPointDemand = SetPointTempLo - this->DemandSideLoop.InletTemp;
				DeltaTHeatSetPointSupply = SetPointTempLo - this->SupplySideLoop.InletTemp;
				if ( ( DeltaTCooling > this->TempControlTol ) && ( SetPointTempHi < this->SupplySideLoop.InletTemp ) ) {

					// can and want to cool
					mdotSupSide = this->SupplySideLoop.MassFlowRateMax;
					SetComponentFlowRate( mdotSupSide, this->SupplySideLoop.InletNodeNum, this->SupplySideLoop.OutletNodeNum, this->SupplySideLoop.LoopNum, this->SupplySideLoop.LoopSideNum, this->SupplySideLoop.BranchNum, this->SupplySideLoop.CompNum );
					if ( mdotSupSide > MassFlowTolerance ) {
						TargetLeavingTemp = SetPointTempHi;
						this->findHXDemandSideLoopFlow( TargetLeavingTemp, CoolingSupplySideLoop );
					} else {
						mdotDmdSide = 0.0;
						SetComponentFlowRate( mdotDmdSide, this->DemandSideLoop.InletNodeNum, this->DemandSideLoop.OutletNodeNum, this->DemandSideLoop.LoopNum, this->DemandSideLoop.LoopSideNum, this->DemandSideLoop.BranchNum, this->DemandSideLoop.CompNum );
					}
				} else if ( ( DeltaTHeating > this->TempControlTol ) && ( SetPointTempLo > this->SupplySideLoop.InletTemp ) ) {
					// can and want to heat
					mdotSupSide = this->SupplySideLoop.MassFlowRateMax;
					SetComponentFlowRate( mdotSupSide, this->SupplySideLoop.InletNodeNum, this->SupplySideLoop.OutletNodeNum, this->SupplySideLoop.LoopNum, this->SupplySideLoop.LoopSideNum, this->SupplySideLoop.BranchNum, this->SupplySideLoop.CompNum );
					if ( mdotSupSide > MassFlowTolerance ) {
						TargetLeavingTemp = SetPointTempLo;
						this->findHXDemandSideLoopFlow( TargetLeavingTemp, HeatingSupplySideLoop );
					} else {
						mdotDmdSide = 0.0;
						SetComponentFlowRate( mdotDmdSide, this->DemandSideLoop.InletNodeNum, this->DemandSideLoop.OutletNodeNum, this->DemandSideLoop.LoopNum, this->DemandSideLoop.LoopSideNum, this->DemandSideLoop.BranchNum, this->DemandSideLoop.CompNum );
					}
				} else { // not able or don't want conditioning
					mdotSupSide = 0.0;
					SetComponentFlowRate( mdotSupSide, this->SupplySideLoop.InletNodeNum, this->SupplySideLoop.OutletNodeNum, this->SupplySideLoop.LoopNum, this->SupplySideLoop.LoopSideNum, this->SupplySideLoop.BranchNum, this->SupplySideLoop.CompNum );
					mdotDmdSide = 0.0;
					SetComponentFlowRate( mdotDmdSide, this->DemandSideLoop.InletNodeNum, this->DemandSideLoop.OutletNodeNum, this->DemandSideLoop.LoopNum, this->DemandSideLoop.LoopSideNum, this->DemandSideLoop.BranchNum, this->DemandSideLoop.CompNum );
				}

			} else if ( SELECT_CASE_var == DualDeadBandSetPointOnOff ) {

				SetPointTempLo = Node( this->SetPointNodeNum ).TempSetPointLo;
				SetPointTempHi = Node( this->SetPointNodeNum ).TempSetPointHi;
				DeltaTCooling = this->SupplySideLoop.InletTemp - this->DemandSideLoop.InletTemp;
				DeltaTHeating = this->DemandSideLoop.InletTemp - this->SupplySideLoop.InletTemp;
				if ( ( DeltaTCooling > this->TempControlTol ) && ( SetPointTempHi < this->SupplySideLoop.InletTemp ) ) {
					// can and want to cool
					mdotSupSide = this->SupplySideLoop.MassFlowRateMax;
					SetComponentFlowRate( mdotSupSide, this->SupplySideLoop.InletNodeNum, this->SupplySideLoop.OutletNodeNum, this->SupplySideLoop.LoopNum, this->SupplySideLoop.LoopSideNum, this->SupplySideLoop.BranchNum, this->SupplySideLoop.CompNum );
					if ( mdotSupSide > MassFlowTolerance ) {
						mdotDmdSide = this->DemandSideLoop.MassFlowRateMax;
					} else {
						mdotDmdSide = 0.0;
					}
					SetComponentFlowRate( mdotDmdSide, this->DemandSideLoop.InletNodeNum, this->DemandSideLoop.OutletNodeNum, this->DemandSideLoop.LoopNum, this->DemandSideLoop.LoopSideNum, this->DemandSideLoop.BranchNum, this->DemandSideLoop.CompNum );
				} else if ( ( DeltaTHeating > this->TempControlTol ) && ( SetPointTempLo > this->SupplySideLoop.InletTemp ) ) {
					// can and want to heat
					mdotSupSide = this->SupplySideLoop.MassFlowRateMax;
					SetComponentFlowRate( mdotSupSide, this->SupplySideLoop.InletNodeNum, this->SupplySideLoop.OutletNodeNum, this->SupplySideLoop.LoopNum, this->SupplySideLoop.LoopSideNum, this->SupplySideLoop.BranchNum, this->SupplySideLoop.CompNum );
					if ( mdotSupSide > MassFlowTolerance ) {
						mdotDmdSide = this->DemandSideLoop.MassFlowRateMax;
					} else {
						mdotDmdSide = 0.0;
					}
					SetComponentFlowRate( mdotDmdSide, this->DemandSideLoop.InletNodeNum, this->DemandSideLoop.OutletNodeNum, this->DemandSideLoop.LoopNum, this->DemandSideLoop.LoopSideNum, this->DemandSideLoop.BranchNum, this->DemandSideLoop.CompNum );
				} else { // not able or don't want conditioning
					mdotSupSide = 0.0;
					SetComponentFlowRate( mdotSupSide, this->SupplySideLoop.InletNodeNum, this->SupplySideLoop.OutletNodeNum, this->SupplySideLoop.LoopNum, this->SupplySideLoop.LoopSideNum, this->SupplySideLoop.BranchNum, this->SupplySideLoop.CompNum );
					mdotDmdSide = 0.0;
					SetComponentFlowRate( mdotDmdSide, this->DemandSideLoop.InletNodeNum, this->DemandSideLoop.OutletNodeNum, this->DemandSideLoop.LoopNum, this->DemandSideLoop.LoopSideNum, this->DemandSideLoop.BranchNum, this->DemandSideLoop.CompNum );
				}

			} else if ( SELECT_CASE_var == CoolingDifferentialOnOff ) {

				DeltaTCooling = this->SupplySideLoop.InletTemp - this->DemandSideLoop.InletTemp;
				if ( DeltaTCooling > this->TempControlTol ) {
					//  want to cool
					mdotSupSide = this->SupplySideLoop.MassFlowRateMax;
					SetComponentFlowRate( mdotSupSide, this->SupplySideLoop.InletNodeNum, this->SupplySideLoop.OutletNodeNum, this->SupplySideLoop.LoopNum, this->SupplySideLoop.LoopSideNum, this->SupplySideLoop.BranchNum, this->SupplySideLoop.CompNum );
					if ( mdotSupSide > MassFlowTolerance ) {
						mdotDmdSide = this->DemandSideLoop.MassFlowRateMax;
					} else {
						mdotDmdSide = 0.0;
					}
					SetComponentFlowRate( mdotDmdSide, this->DemandSideLoop.InletNodeNum, this->DemandSideLoop.OutletNodeNum, this->DemandSideLoop.LoopNum, this->DemandSideLoop.LoopSideNum, this->DemandSideLoop.BranchNum, this->DemandSideLoop.CompNum );
				} else { // not wanting to cool so turn off
					mdotSupSide = 0.0;
					SetComponentFlowRate( mdotSupSide, this->SupplySideLoop.InletNodeNum, this->SupplySideLoop.OutletNodeNum, this->SupplySideLoop.LoopNum, this->SupplySideLoop.LoopSideNum, this->SupplySideLoop.BranchNum, this->SupplySideLoop.CompNum );
					mdotDmdSide = 0.0;
					SetComponentFlowRate( mdotDmdSide, this->DemandSideLoop.InletNodeNum, this->DemandSideLoop.OutletNodeNum, this->DemandSideLoop.LoopNum, this->DemandSideLoop.LoopSideNum, this->DemandSideLoop.BranchNum, this->DemandSideLoop.CompNum );
				}

			} else if ( SELECT_CASE_var == CoolingSetPointOnOffWithComponentOverride ) {

				{ auto const SELECT_CASE_var1( this->ControlSignalTemp );
				if ( SELECT_CASE_var1 == WetBulbTemperature ) {
					ControlSignalValue = OutWetBulbTemp;
				} else if ( SELECT_CASE_var1 == DryBulbTemperature ) {
					ControlSignalValue = OutDryBulbTemp;
				} else if ( SELECT_CASE_var1 == LoopTemperature ) {
					// ControlSignalValue = FluidHX(CompNum)%DemandSideLoop%InletTemp
					ControlSignalValue = Node( this->OtherCompDemandSideLoop.InletNodeNum ).TempLastTimestep;
				} else {
					assert( false );
				}}

				SetPointTemp = Node( this->SetPointNodeNum ).TempSetPoint;
				DeltaTCooling = SetPointTemp - ControlSignalValue;
				//obtain shut down state
				ChillerShutDown = PlantLoop( this->OtherCompSupplySideLoop.LoopNum ).LoopSide( this->OtherCompSupplySideLoop.LoopSideNum ).Branch( this->OtherCompSupplySideLoop.BranchNum ).Comp( this->OtherCompSupplySideLoop.CompNum ).FreeCoolCntrlShutDown;
				if ( ChillerShutDown && ( DeltaTCooling > this->TempControlTol ) ) {
					// can and want to cool
					mdotSupSide = this->SupplySideLoop.MassFlowRateMax;
					SetComponentFlowRate( mdotSupSide, this->SupplySideLoop.InletNodeNum, this->SupplySideLoop.OutletNodeNum, this->SupplySideLoop.LoopNum, this->SupplySideLoop.LoopSideNum, this->SupplySideLoop.BranchNum, this->SupplySideLoop.CompNum );
					if ( mdotSupSide > MassFlowTolerance ) {
						mdotDmdSide = this->DemandSideLoop.MassFlowRateMax;
					} else {
						mdotDmdSide = 0.0;
					}
					SetComponentFlowRate( mdotDmdSide, this->DemandSideLoop.InletNodeNum, this->DemandSideLoop.OutletNodeNum, this->DemandSideLoop.LoopNum, this->DemandSideLoop.LoopSideNum, this->DemandSideLoop.BranchNum, this->DemandSideLoop.CompNum );

				} else {
					mdotSupSide = 0.0;
					SetComponentFlowRate( mdotSupSide, this->SupplySideLoop.InletNodeNum, this->SupplySideLoop.OutletNodeNum, this->SupplySideLoop.LoopNum, this->SupplySideLoop.LoopSideNum, this->SupplySideLoop.BranchNum, this->SupplySideLoop.CompNum );
					mdotDmdSide = 0.0;
					SetComponentFlowRate( mdotDmdSide, this->DemandSideLoop.InletNodeNum, this->DemandSideLoop.OutletNodeNum, this->DemandSideLoop.LoopNum, this->DemandSideLoop.LoopSideNum, this->DemandSideLoop.BranchNum, this->DemandSideLoop.CompNum );
				}

			}}

		} else { // scheduled off
			mdotSupSide = 0.0;
			SetComponentFlowRate( mdotSupSide, this->SupplySideLoop.InletNodeNum, this->SupplySideLoop.OutletNodeNum, this->SupplySideLoop.LoopNum, this->SupplySideLoop.LoopSideNum, this->SupplySideLoop.BranchNum, this->SupplySideLoop.CompNum );
			mdotDmdSide = 0.0;
			SetComponentFlowRate( mdotDmdSide, this->DemandSideLoop.InletNodeNum, this->DemandSideLoop.OutletNodeNum, this->DemandSideLoop.LoopNum, this->DemandSideLoop.LoopSideNum, this->DemandSideLoop.BranchNum, this->DemandSideLoop.CompNum );
		}

	}

	void HeatExchangerStruct::calc(
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

		SupSideLoopInletTemp = Node( this->SupplySideLoop.InletNodeNum ).Temp;
		DmdSideLoopInletTemp = Node( this->DemandSideLoop.InletNodeNum ).Temp;

		SupSideLoopInletCp = GetSpecificHeatGlycol( PlantLoop( this->SupplySideLoop.LoopNum ).FluidName, SupSideLoopInletTemp, PlantLoop( this->SupplySideLoop.LoopNum ).FluidIndex, RoutineName );
		DmdSideLoopInletCp = GetSpecificHeatGlycol( PlantLoop( this->DemandSideLoop.LoopNum ).FluidName, DmdSideLoopInletTemp, PlantLoop( this->DemandSideLoop.LoopNum ).FluidIndex, RoutineName );

		SupSideCapRate = SupSideMdot * SupSideLoopInletCp;
		DmdSideCapRate = DmdSideMdot * DmdSideLoopInletCp;
		MinCapRate = min( SupSideCapRate, DmdSideCapRate );
		MaxCapRate = max( SupSideCapRate, DmdSideCapRate );

		if ( MinCapRate > 0.0 ) {

			{ auto const SELECT_CASE_var( this->HeatExchangeModelType );

			if ( SELECT_CASE_var == CrossFlowBothUnMixed ) {
				NTU = this->UA / MinCapRate;
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
				NTU = this->UA / MinCapRate;
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

				if ( SupSideCapRate == MaxCapRate && this->HeatExchangeModelType == CrossFlowSupplyLoopMixedDemandLoopUnMixed ) {
					CrossFlowEquation = CmaxMixedCminUnmixed;
				} else if ( SupSideCapRate == MinCapRate && this->HeatExchangeModelType == CrossFlowSupplyLoopMixedDemandLoopUnMixed ) {
					CrossFlowEquation = CmaxUnMixedCminMixed;
				} else if ( DmdSideCapRate == MaxCapRate && this->HeatExchangeModelType == CrossFlowSupplyLoopUnMixedDemandLoopMixed ) {
					CrossFlowEquation = CmaxMixedCminUnmixed;
				} else if ( DmdSideCapRate == MinCapRate && this->HeatExchangeModelType == CrossFlowSupplyLoopUnMixedDemandLoopMixed ) {
					CrossFlowEquation = CmaxUnMixedCminMixed;
				} else {
					CrossFlowEquation = CmaxMixedCminUnmixed;
				}

				NTU = this->UA / MinCapRate;
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
				NTU = this->UA / MinCapRate;
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
				NTU = this->UA / MinCapRate;
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
			this->SupplySideLoop.OutletTemp = SupSideLoopInletTemp - HeatTransferRate / ( SupSideLoopInletCp * SupSideMdot );
		} else {
			this->SupplySideLoop.OutletTemp = SupSideLoopInletTemp;
		}

		if ( DmdSideMdot > 0.0 ) {
			this->DemandSideLoop.OutletTemp = DmdSideLoopInletTemp + HeatTransferRate / ( DmdSideLoopInletCp * DmdSideMdot );
		} else {
			this->DemandSideLoop.OutletTemp = DmdSideLoopInletTemp;
		}
		this->Effectiveness = Effectiveness;
		this->HeatTransferRate = HeatTransferRate;
		this->SupplySideLoop.InletTemp = SupSideLoopInletTemp;
		this->SupplySideLoop.InletMassFlowRate = SupSideMdot;
		this->DemandSideLoop.InletTemp = DmdSideLoopInletTemp;
		this->DemandSideLoop.InletMassFlowRate = DmdSideMdot;

	}

	void HeatExchangerStruct::findHXDemandSideLoopFlow(
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
		Array1D< Real64 > Par( 1 ); // Parameter array passed to solver

		Real64 LeavingTempMinFlow;
		Real64 LeavingTempFullFlow;
		Real64 SupSideMdot; // mass flow rate of fluid entering from supply side loop
		Real64 DmdSideMdot; // mass flow rate of fluid entering from demand side loop

		SupSideMdot = Node( this->SupplySideLoop.InletNodeNum ).MassFlowRate;
		// first see if root is bracketed
		// min demand flow
		DmdSideMdot = this->DemandSideLoop.MassFlowRateMin;
		this->calc( SupSideMdot, DmdSideMdot );
		LeavingTempMinFlow = this->SupplySideLoop.OutletTemp;

		// full demand flow
		DmdSideMdot = this->DemandSideLoop.MassFlowRateMax;
		this->calc( SupSideMdot, DmdSideMdot );
		LeavingTempFullFlow = this->SupplySideLoop.OutletTemp;

		{ auto const SELECT_CASE_var( HXActionMode );

		if ( SELECT_CASE_var == HeatingSupplySideLoop ) {
			if ( ( LeavingTempFullFlow > TargetSupplySideLoopLeavingTemp ) && ( TargetSupplySideLoopLeavingTemp > LeavingTempMinFlow ) ) {
				// need to solve
				Par( 1 ) = TargetSupplySideLoopLeavingTemp;

				SolveRegulaFalsi( Acc, MaxIte, SolFla, DmdSideMdot, [ this ]( Real64 const X, Array1< Real64 > const & Par ) -> Real64 { return HXDemandSideLoopFlowResidual( X, Par ); }, this->DemandSideLoop.MassFlowRateMin, this->DemandSideLoop.MassFlowRateMax, Par );

				if ( SolFla == -1 ) { // no convergence
					if ( ! WarmupFlag ) {
						if ( this->DmdSideModulatSolvNoConvergeErrorCount < 1 ) {
							++this->DmdSideModulatSolvNoConvergeErrorCount;
							ShowWarningError( nsvComponentClassName + " named " + this->Name + " - Iteration Limit exceeded calculating demand side loop flow rate" );
							ShowContinueError( "Simulation continues with calculated demand side mass flow rate = " + RoundSigDigits( DmdSideMdot, 7 ) );
						}
						ShowRecurringWarningErrorAtEnd( nsvComponentClassName + " named " + this->Name + " - Iteration Limit exceeded calculating demand side loop flow rate continues.", this->DmdSideModulatSolvNoConvergeErrorIndex, DmdSideMdot, DmdSideMdot );
					}
				} else if ( SolFla == -2 ) { //f(x0) and f(x1) have the same sign
					DmdSideMdot = this->DemandSideLoop.MassFlowRateMax * ( LeavingTempFullFlow - TargetSupplySideLoopLeavingTemp ) / ( LeavingTempFullFlow - LeavingTempMinFlow );
					if ( ! WarmupFlag ) {
						if ( this->DmdSideModulatSolvFailErrorCount < 1 ) {
							++this->DmdSideModulatSolvFailErrorCount;
							ShowWarningError( nsvComponentClassName + " named " + this->Name + " - Solver failed to calculate demand side loop flow rate" );
							ShowContinueError( "Simulation continues with estimated demand side mass flow rate = " + RoundSigDigits( DmdSideMdot, 7 ) );
						}
						ShowRecurringWarningErrorAtEnd( nsvComponentClassName + " named " + this->Name + " - Solver failed to calculate demand side loop flow rate continues.", this->DmdSideModulatSolvFailErrorIndex, DmdSideMdot, DmdSideMdot );
					}
				}
				SetComponentFlowRate( DmdSideMdot, this->DemandSideLoop.InletNodeNum, this->DemandSideLoop.OutletNodeNum, this->DemandSideLoop.LoopNum, this->DemandSideLoop.LoopSideNum, this->DemandSideLoop.BranchNum, this->DemandSideLoop.CompNum );

			} else if ( ( TargetSupplySideLoopLeavingTemp >= LeavingTempFullFlow ) && ( LeavingTempFullFlow > LeavingTempMinFlow ) ) {
				// run at full flow
				DmdSideMdot = this->DemandSideLoop.MassFlowRateMax;
				SetComponentFlowRate( DmdSideMdot, this->DemandSideLoop.InletNodeNum, this->DemandSideLoop.OutletNodeNum, this->DemandSideLoop.LoopNum, this->DemandSideLoop.LoopSideNum, this->DemandSideLoop.BranchNum, this->DemandSideLoop.CompNum );

			} else if ( LeavingTempMinFlow >= TargetSupplySideLoopLeavingTemp ) {

				// run at min flow
				DmdSideMdot = this->DemandSideLoop.MassFlowRateMin;
				SetComponentFlowRate( DmdSideMdot, this->DemandSideLoop.InletNodeNum, this->DemandSideLoop.OutletNodeNum, this->DemandSideLoop.LoopNum, this->DemandSideLoop.LoopSideNum, this->DemandSideLoop.BranchNum, this->DemandSideLoop.CompNum );
			}
		} else if ( SELECT_CASE_var == CoolingSupplySideLoop ) {
			if ( ( LeavingTempFullFlow < TargetSupplySideLoopLeavingTemp ) && ( TargetSupplySideLoopLeavingTemp < LeavingTempMinFlow ) ) {
				// need to solve

				Par( 1 ) = TargetSupplySideLoopLeavingTemp;

				SolveRegulaFalsi( Acc, MaxIte, SolFla, DmdSideMdot, [ this ]( Real64 const X, Array1< Real64 > const & Par ) -> Real64 { return HXDemandSideLoopFlowResidual( X, Par ); }, this->DemandSideLoop.MassFlowRateMin, this->DemandSideLoop.MassFlowRateMax, Par );

				if ( SolFla == -1 ) { // no convergence
					if ( ! WarmupFlag ) {
						if ( this->DmdSideModulatSolvNoConvergeErrorCount < 1 ) {
							++this->DmdSideModulatSolvNoConvergeErrorCount;
							ShowWarningError( nsvComponentClassName + " named " + this->Name + " - Iteration Limit exceeded calculating demand side loop flow rate" );
							ShowContinueError( "Simulation continues with calculated demand side mass flow rate = " + RoundSigDigits( DmdSideMdot, 7 ) );
						}
						ShowRecurringWarningErrorAtEnd( nsvComponentClassName + " named " + this->Name + " - Iteration Limit exceeded calculating demand side loop flow rate continues.", this->DmdSideModulatSolvNoConvergeErrorIndex, DmdSideMdot, DmdSideMdot );
					}
				} else if ( SolFla == -2 ) { //f(x0) and f(x1) have the same sign
					DmdSideMdot = this->DemandSideLoop.MassFlowRateMax * ( LeavingTempFullFlow - TargetSupplySideLoopLeavingTemp ) / ( LeavingTempFullFlow - LeavingTempMinFlow );
					if ( ! WarmupFlag ) {
						if ( this->DmdSideModulatSolvFailErrorCount < 1 ) {
							++this->DmdSideModulatSolvFailErrorCount;
							ShowWarningError( nsvComponentClassName + " named " + this->Name + " - Solver failed to calculate demand side loop flow rate" );
							ShowContinueError( "Simulation continues with estimated demand side mass flow rate = " + RoundSigDigits( DmdSideMdot, 7 ) );
						}
						ShowRecurringWarningErrorAtEnd( nsvComponentClassName + " named " + this->Name + " - Solver failed to calculate demand side loop flow rate continues.", this->DmdSideModulatSolvFailErrorIndex, DmdSideMdot, DmdSideMdot );
					}
				}
				SetComponentFlowRate( DmdSideMdot, this->DemandSideLoop.InletNodeNum, this->DemandSideLoop.OutletNodeNum, this->DemandSideLoop.LoopNum, this->DemandSideLoop.LoopSideNum, this->DemandSideLoop.BranchNum, this->DemandSideLoop.CompNum );
			} else if ( ( TargetSupplySideLoopLeavingTemp <= LeavingTempFullFlow ) && ( LeavingTempFullFlow < LeavingTempMinFlow ) ) {
				// run at full flow
				DmdSideMdot = this->DemandSideLoop.MassFlowRateMax;
				SetComponentFlowRate( DmdSideMdot, this->DemandSideLoop.InletNodeNum, this->DemandSideLoop.OutletNodeNum, this->DemandSideLoop.LoopNum, this->DemandSideLoop.LoopSideNum, this->DemandSideLoop.BranchNum, this->DemandSideLoop.CompNum );
			} else if ( LeavingTempMinFlow <= TargetSupplySideLoopLeavingTemp ) {

				// run at min flow
				DmdSideMdot = this->DemandSideLoop.MassFlowRateMin;
				SetComponentFlowRate( DmdSideMdot, this->DemandSideLoop.InletNodeNum, this->DemandSideLoop.OutletNodeNum, this->DemandSideLoop.LoopNum, this->DemandSideLoop.LoopSideNum, this->DemandSideLoop.BranchNum, this->DemandSideLoop.CompNum );
			}

		}}

	}

	Real64 HeatExchangerStruct::HXDemandSideLoopFlowResidual(
		Real64 const DmdSideMassFlowRate,
		Array1< Real64 > const & Par 
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

		SupSideMdot = Node( this->SupplySideLoop.InletNodeNum ).MassFlowRate;

		this->calc( SupSideMdot, MdotTrial );

		SupSideLoopOutletTemp = this->SupplySideLoop.OutletTemp;

		Residuum = Par( 1 ) - SupSideLoopOutletTemp;

		return Residuum;

	}

	void HeatExchangerStruct::update()
	{
		Node( this->DemandSideLoop.OutletNodeNum ).Temp = this->DemandSideLoop.OutletTemp;
		Node( this->SupplySideLoop.OutletNodeNum ).Temp = this->SupplySideLoop.OutletTemp;
	}

	void HeatExchangerStruct::report()
	{
		using DataHVACGlobals::TimeStepSys;
		using DataHVACGlobals::SmallLoad;
		using DataGlobals::SecInHour;

		this->HeatTransferEnergy = this->HeatTransferRate * TimeStepSys * SecInHour;

		if ( ( std::abs( this->HeatTransferRate ) > SmallLoad ) && ( this->DemandSideLoop.InletMassFlowRate > 0.0 ) && ( this->SupplySideLoop.InletMassFlowRate > 0.0 ) ) {
			this->OperationStatus = 1.0;
		} else {
			this->OperationStatus = 0.0;
		}

	}

} // PlantHeatExchangerFluidToFluid

} // EnergyPlus
