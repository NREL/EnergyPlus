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
#include <algorithm>
#include <cassert>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <PlantManager.hh>
#include <BranchInputManager.hh>
#include <DataBranchAirLoopPlant.hh>
#include <DataConvergParams.hh>
#include <DataEnvironment.hh>
#include <DataErrorTracking.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <EMSManager.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <GroundHeatExchangers.hh>
#include <HVACInterfaceManager.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <PipeHeatTransfer.hh>
#include <Pipes.hh>
#include <PlantLoadProfile.hh>
#include <PlantLoopEquip.hh>
#include <PlantLoopSolver.hh>
#include <PlantUtilities.hh>
#include <PondGroundHeatExchanger.hh>
#include <ReportSizingManager.hh>
#include <ScheduleManager.hh>
#include <SetPointManager.hh>
#include <SurfaceGroundHeatExchanger.hh>
#include <SystemAvailabilityManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace PlantManager {

	// MODULE INFORMATION:
	//       AUTHOR         Sankaranarayanan K P, Rich Liesen
	//       DATE WRITTEN   May 2005
	//       MODIFIED
	//       RE-ENGINEERED  Sept. 2010 D. Fisher, Edwin Lee, Brent Griffith
	//                      major plant upgrades:
	//                         Single half loop solver
	//                         Automated branch control types
	//                         new loop sequencing structure
	//                         Temperature out range checks

	// PURPOSE OF THIS MODULE:
	// This module serves as the driver for the plant simulation. All necessary iterations and update related to plant
	// connections are performed in this module.

	// METHODOLOGY EMPLOYED:
	// Standard EnergyPlus methodology

	// REFERENCES: none

	// OTHER NOTES: none

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataGlobals;
	using namespace DataHVACGlobals;
	using namespace DataPlant;
	using namespace DataBranchAirLoopPlant;
	using namespace DataLoopNode;
	using namespace FluidProperties;
	using PlantLoopSolver::PlantHalfLoopSolver;

	// Data
	// MODULE PARAMETER DEFINITIONS
	int const MaxBranchLevel( 200 );
	int const Plant( 1 );
	int const Condenser( 2 );
	int const SupplyLoopPumpSingleSplitMix( 1 );
	int const DemandSingleSplitterMixer( 1 );
	int const TempSetPt( 1001 );
	int const FlowSetPt( 1007 );
	bool InitLoopEquip( true );
	bool GetCompSizFac( true );

	static std::string const fluidNameSteam( "STEAM" );

	//MODULE DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DEFINITIONS
	int PlantSupplyLoopCase( 0 );
	int PlantDemandLoopCase( 0 );

	Array1D_int SupplySideInletNode; // Node number for the supply side inlet
	Array1D_int SupplySideOutletNode; // Node number for the supply side outlet
	Array1D_int DemandSideInletNode; // Inlet node on the demand side

	// SUBROUTINE SPECIFICATIONS:
	//The following public routines are called from HVAC Manager
	//PUBLIC  CheckPlantLoopData      !called from SimHVAC

	// Object Data
	Array1D< LoopPipeData > LoopPipe;
	TempLoopData TempLoop; // =(' ',' ',' ',0, , , ,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.)

	// MODULE SUBROUTINES

	// Functions

	void
	clear_state()
	{
		InitLoopEquip = true;
		GetCompSizFac = true;
		PlantSupplyLoopCase = 0;
		PlantDemandLoopCase = 0;
		SupplySideInletNode.deallocate();
		SupplySideOutletNode.deallocate();
		DemandSideInletNode.deallocate();
		LoopPipe.deallocate();
		TempLoop = TempLoopData();
	}

	void
	ManagePlantLoops(
		bool const FirstHVACIteration,
		bool & SimAirLoops, // True when the air loops need to be (re)simulated
		bool & SimZoneEquipment, // True when zone equipment components need to be (re)simulated
		bool & EP_UNUSED( SimNonZoneEquipment ), // True when non-zone equipment components need to be (re)simulated
		bool & SimPlantLoops, // True when some part of Plant needs to be (re)simulated
		bool & SimElecCircuits // True when electic circuits need to be (re)simulated
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Sankaranarayanan K P
		//       DATE WRITTEN   Apr 2005
		//       MODIFIED
		//       RE-ENGINEERED  B. Griffith, Feb. 2010

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine manages the plant loop simulation

		// METHODOLOGY EMPLOYED:
		// Set up the while iteration block for the plant loop simulation.
		// Calls half loop sides to be simulated in predetermined order.
		// Reset the flags as necessary

		// REFERENCES:
		// na

		// USE STATEMENTS: NA

		// Using/Aliasing
		using PlantUtilities::LogPlantConvergencePoints;
		using DataConvergParams::MinPlantSubIterations;
		using DataConvergParams::MaxPlantSubIterations;

		// SUBROUTINE ARGUMENT DEFINITIONS

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS

		// SUBROUTINE VARIABLE DEFINITIONS
		int IterPlant;
		int LoopNum;
		int LoopSide;
		int LoopSideNum;
		int OtherSide;
		bool SimHalfLoopFlag;
		int HalfLoopNum;
		int CurntMinPlantSubIterations;

		if ( std::any_of( PlantLoop.begin(), PlantLoop.end(), []( DataPlant::PlantLoopData const & e ){ return ( e.CommonPipeType == DataPlant::CommonPipe_Single ) || ( e.CommonPipeType == DataPlant::CommonPipe_TwoWay ); } ) ) {
			CurntMinPlantSubIterations = max( 7, MinPlantSubIterations );
		} else {
			CurntMinPlantSubIterations = MinPlantSubIterations;
		}

		if ( TotNumLoops <= 0 ) { // quick return if no plant in model
			SimPlantLoops = false;
			return;
		}

		IterPlant = 0;
		InitializeLoops( FirstHVACIteration );

		while ( ( SimPlantLoops ) && ( IterPlant <= MaxPlantSubIterations ) ) {
			// go through half loops in predetermined calling order
			for ( HalfLoopNum = 1; HalfLoopNum <= TotNumHalfLoops; ++HalfLoopNum ) {

				LoopNum = PlantCallingOrderInfo( HalfLoopNum ).LoopIndex;
				LoopSide = PlantCallingOrderInfo( HalfLoopNum ).LoopSide;
				OtherSide = 3 - LoopSide; //will give us 1 if LoopSide is 2, or 2 if LoopSide is 1

				auto & this_loop( PlantLoop( LoopNum ) );
				auto & this_loop_side( this_loop.LoopSide( LoopSide ) );
				auto & other_loop_side( this_loop.LoopSide( OtherSide ) );

				SimHalfLoopFlag = this_loop_side.SimLoopSideNeeded; //set half loop sim flag

				if ( SimHalfLoopFlag || IterPlant <= CurntMinPlantSubIterations ) {

					PlantHalfLoopSolver( FirstHVACIteration, LoopSide, LoopNum, other_loop_side.SimLoopSideNeeded );

					// Always set this side to false,  so that it won't keep being turned on just because of first hvac
					this_loop_side.SimLoopSideNeeded = false;

					// If we did the demand side, turn on the supply side (only if we need to do it last)
					if ( LoopSide == DemandSide ) {
						if ( this_loop.HasPressureComponents ) {
							other_loop_side.SimLoopSideNeeded = false;
						}
					}

					// Update the report variable
					PlantReport( LoopNum ).LastLoopSideSimulated = LoopSide;

					++PlantManageHalfLoopCalls;
				}

			} // half loop based calling order...

			// decide new status for SimPlantLoops flag
			SimPlantLoops = false;
			for ( LoopNum = 1; LoopNum <= TotNumLoops; ++LoopNum ) {
				for ( LoopSideNum = 1; LoopSideNum <= 2; ++LoopSideNum ) {
					if ( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).SimLoopSideNeeded ) {
						SimPlantLoops = true;
						goto LoopLevel_exit;
					}
				}
			}
			LoopLevel_exit: ;

			++IterPlant; // Increment the iteration counter
			if ( IterPlant < CurntMinPlantSubIterations ) SimPlantLoops = true;
			++PlantManageSubIterations; // these are summed across all half loops for reporting
		} //while

		// add check for non-plant system sim flag updates
		//  could set SimAirLoops, SimElecCircuits, SimZoneEquipment flags for now
		for ( LoopNum = 1; LoopNum <= TotNumLoops; ++LoopNum ) {
			for ( LoopSide = DemandSide; LoopSide <= SupplySide; ++LoopSide ) {
			auto & this_loop_side( PlantLoop(LoopNum).LoopSide(LoopSide) );
			if ( this_loop_side.SimAirLoopsNeeded ) SimAirLoops = true;
				if ( this_loop_side.SimZoneEquipNeeded ) SimZoneEquipment = true;
				//  IF (this_loop_side.SimNonZoneEquipNeeded) SimNonZoneEquipment = .TRUE.
				if ( this_loop_side.SimElectLoadCentrNeeded ) SimElecCircuits = true;
			}
		}

		//Also log the convergence history of all loopsides once complete
		LogPlantConvergencePoints( FirstHVACIteration );

	}

	void
	GetPlantLoopData()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Sankaranarayanan K P
		//       DATE WRITTEN   April 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine reads the primary plant loop
		// attributes from the input file

		// METHODOLOGY EMPLOYED:
		// calls the Input Processor to retrieve data from input file.

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::GetObjectItemNum;
		using InputProcessor::VerifyName;
		using InputProcessor::SameString;
		using InputProcessor::FindItemInList;
		using namespace DataIPShortCuts; // Data for field names, blank numerics
		using ScheduleManager::GetScheduleIndex;
		using SetPointManager::IsNodeOnSetPtManager;
		auto & TempSetPt( SetPointManager::iCtrlVarType_Temp );
		using NodeInputManager::GetOnlySingleNode;
		using namespace BranchInputManager;
		using DataSizing::AutoSize;
		using SystemAvailabilityManager::GetPlantAvailabilityManager;
		using FluidProperties::CheckFluidPropertyName;
		using FluidProperties::FindGlycol;
		using General::RoundSigDigits;
		using DataConvergParams::PlantConvergence;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetPlant/CondenserLoopData: " );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int LoopNum; // DO loop counter for loops
		int PressSimLoop; // DO loop counter for pressure simulation type
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		int IOStat; // IO Status when calling get input subroutine
		int NumFluids; // number of fluids in sim
		int PlantLoopNum;
		int CondLoopNum;
		Array1D_string Alpha( 18 ); // dimension to num of alpha fields in input
		Array1D< Real64 > Num( 30 ); // dimension to num of numeric data fields in input
		static bool ErrorsFound( false );
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		std::string LoadingScheme;
		bool ErrFound;
		std::string CurrentModuleObject; // for ease in renaming.
		bool MatchedPressureString;
		int PressSimAlphaIndex;
		//  INTEGER :: OpSchemeFound

		// FLOW:
		CurrentModuleObject = "PlantLoop";
		NumPlantLoops = GetNumObjectsFound( CurrentModuleObject ); // Get the number of primary plant loops
		CurrentModuleObject = "CondenserLoop";
		NumCondLoops = GetNumObjectsFound( CurrentModuleObject ); // Get the number of Condenser loops
		TotNumLoops = NumPlantLoops + NumCondLoops;
		ErrFound = false;

		if ( TotNumLoops > 0 ) {
			PlantLoop.allocate( TotNumLoops );
			PlantConvergence.allocate( TotNumLoops );
			if ( ! allocated( PlantAvailMgr ) ) {
				PlantAvailMgr.allocate( TotNumLoops );
			}
		} else {
			return;
		}

		for ( LoopNum = 1; LoopNum <= TotNumLoops; ++LoopNum ) {
			Alpha = "";
			Num = 0.0;

			//set up some references
			auto & this_loop( PlantLoop( LoopNum ) );
			this_loop.LoopSide.allocate( 2 );
			auto & this_demand_side( this_loop.LoopSide( 1 ) );
			auto & this_supply_side( this_loop.LoopSide( 2 ) );
			if ( LoopNum <= NumPlantLoops ) {
				PlantLoopNum = LoopNum;
				this_loop.TypeOfLoop = Plant;
				CurrentModuleObject = "PlantLoop";
				GetObjectItem( CurrentModuleObject, PlantLoopNum, Alpha, NumAlphas, Num, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			} else {
				CondLoopNum = LoopNum - NumPlantLoops;
				this_loop.TypeOfLoop = Condenser;
				CurrentModuleObject = "CondenserLoop";
				GetObjectItem( CurrentModuleObject, CondLoopNum, Alpha, NumAlphas, Num, NumNums, IOStat, lNumericFieldBlanks, _, cAlphaFieldNames, cNumericFieldNames );
			}

			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alpha( 1 ), PlantLoop, LoopNum - 1, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alpha( 1 ) = "xxxxx";
			}

			this_loop.Name = Alpha( 1 ); // Load the Plant Loop Name

			if ( SameString( Alpha( 2 ), "STEAM" ) ) {
				this_loop.FluidType = NodeType_Steam;
				this_loop.FluidName = Alpha( 2 );
			} else if ( SameString( Alpha( 2 ), "WATER" ) ) {
				this_loop.FluidType = NodeType_Water;
				this_loop.FluidName = Alpha( 2 );
				this_loop.FluidIndex = FindGlycol( Alpha( 2 ) );
			} else if ( SameString( Alpha( 2 ), "USERDEFINEDFLUIDTYPE" ) ) {
				this_loop.FluidType = NodeType_Water;
				this_loop.FluidName = Alpha( 3 );
				// check for valid fluid name
				NumFluids = CheckFluidPropertyName( Alpha( 3 ) );
				if ( NumFluids == 0 ) {
					ShowSevereError( CurrentModuleObject + "=\"" + Alpha( 1 ) + "\", missing fluid data for Plant loop." );
					ErrorsFound = true;
				} else {
					this_loop.FluidIndex = FindGlycol( Alpha( 3 ) );
					if ( this_loop.FluidIndex == 0 ) {
						ShowSevereError( CurrentModuleObject + "=\"" + Alpha( 1 ) + "\", invalid glycol fluid data for Plant loop." );
						ErrorsFound = true;
					}
				}
			} else {
				ShowWarningError( "Input error: " + cAlphaFieldNames( 2 ) + '=' + Alpha( 2 ) + "entered, in " + CurrentModuleObject + '=' + Alpha( 1 ) );
				ShowContinueError( "Will default to Water." );

				this_loop.FluidType = NodeType_Water;
				this_loop.FluidName = "WATER";
				this_loop.FluidIndex = FindGlycol( "WATER" );
			}

			this_loop.OperationScheme = Alpha( 4 ); // Load the Plant Control Scheme Priority List

			// Load the temperature and flow rate maximum and minimum limits
			this_loop.MaxTemp = Num( 1 );
			this_loop.MinTemp = Num( 2 );
			this_loop.MaxVolFlowRate = Num( 3 );
			if ( this_loop.MaxVolFlowRate == AutoSize ) {
				this_loop.MaxVolFlowRateWasAutoSized = true;
			}
			this_loop.MinVolFlowRate = Num( 4 );

			//The Plant loop volume for both halves of the loop is read in and used in this module for the
			// correct loop temperature step.  Loop data is read in supply side, but the volume is not used in
			// a calculation there.
			this_loop.Volume = Num( 5 );
			if ( lNumericFieldBlanks( 5 ) ) this_loop.Volume = AutoCalculate;
			if ( this_loop.Volume == AutoCalculate ) {
				this_loop.VolumeWasAutoSized = true;
			}


			// Load the Loop Inlet and Outlet Nodes and Connection Info (Alpha(7-10) are related to the supply side)
			this_supply_side.NodeNameIn = Alpha( 6 );
			this_supply_side.NodeNameOut = Alpha( 7 );
			this_supply_side.BranchList = Alpha( 8 );
			this_supply_side.ConnectList = Alpha( 9 );
			this_demand_side.NodeNameIn = Alpha( 10 );
			this_demand_side.NodeNameOut = Alpha( 11 );
			this_demand_side.BranchList = Alpha( 12 );
			this_demand_side.ConnectList = Alpha( 13 );

			this_supply_side.NodeNumIn = GetOnlySingleNode( Alpha( 6 ), ErrorsFound, CurrentModuleObject, Alpha( 1 ), this_loop.FluidType, NodeConnectionType_Inlet, 1, ObjectIsParent );
			this_supply_side.NodeNumOut = GetOnlySingleNode( Alpha( 7 ), ErrorsFound, CurrentModuleObject, Alpha( 1 ), this_loop.FluidType, NodeConnectionType_Outlet, 1, ObjectIsParent );
			this_demand_side.NodeNumIn = GetOnlySingleNode( Alpha( 10 ), ErrorsFound, CurrentModuleObject, Alpha( 1 ), this_loop.FluidType, NodeConnectionType_Inlet, 1, ObjectIsParent );
			this_demand_side.NodeNumOut = GetOnlySingleNode( Alpha( 11 ), ErrorsFound, CurrentModuleObject, Alpha( 1 ), this_loop.FluidType, NodeConnectionType_Outlet, 1, ObjectIsParent );

			this_demand_side.InletNodeSetPt = IsNodeOnSetPtManager( this_demand_side.NodeNumIn, TempSetPt );
			this_demand_side.OutletNodeSetPt = IsNodeOnSetPtManager( this_demand_side.NodeNumOut, TempSetPt );
			this_supply_side.InletNodeSetPt = IsNodeOnSetPtManager( this_supply_side.NodeNumIn, TempSetPt );
			this_supply_side.OutletNodeSetPt = IsNodeOnSetPtManager( this_supply_side.NodeNumOut, TempSetPt );
			this_loop.TempSetPointNodeNum = GetOnlySingleNode( Alpha( 5 ), ErrorsFound, CurrentModuleObject, Alpha( 1 ), this_loop.FluidType, NodeConnectionType_Sensor, 1, ObjectIsParent );

			// Load the load distribution scheme.
			LoadingScheme = Alpha( 14 );
			if ( SameString( LoadingScheme, "Optimal" ) ) {
				this_loop.LoadDistribution = OptimalLoading;
			} else if ( SameString( LoadingScheme, "SequentialLoad" ) ) {
				this_loop.LoadDistribution = SequentialLoading;
			} else if ( SameString( LoadingScheme, "UniformLoad" ) ) {
				this_loop.LoadDistribution = UniformLoading;
			} else if ( SameString( LoadingScheme, "UniformPLR" ) ) {
				this_loop.LoadDistribution = UniformPLRLoading;
			} else if ( SameString( LoadingScheme, "SequentialUniformPLR" ) ) {
				this_loop.LoadDistribution = SequentialUniformPLRLoading;
			} else {
				ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alpha( 1 ) + "\", Invalid choice." );
				ShowContinueError( "..." + cAlphaFieldNames( 14 ) + "=\"" + Alpha( 14 ) + "\"." );
				ShowContinueError( "Will default to SequentialLoad." ); // TODO rename point
				this_loop.LoadDistribution = SequentialLoading;
			}

			//When dual setpoint is allowed in condenser loop modify this code. Sankar 06/29/2009
			if ( this_loop.TypeOfLoop == Plant ) {
				// Get the Loop Demand Calculation Scheme
				if ( SameString( Alpha( 16 ), "SingleSetpoint" ) ) {
					this_loop.LoopDemandCalcScheme = SingleSetPoint;
				} else if ( SameString( Alpha( 16 ), "DualSetpointDeadband" ) ) {
					if ( this_loop.FluidType == NodeType_Steam ) {
						ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alpha( 1 ) + "\", Invalid choice." );
						ShowContinueError( cAlphaFieldNames( 16 ) + "=\"" + Alpha( 16 ) + "\" not valid for " + cAlphaFieldNames( 2 ) + "= Steam" );
						ShowContinueError( "Will reset " + cAlphaFieldNames( 16 ) + " = SingleSetPoint and simulation will continue." );
						this_loop.LoopDemandCalcScheme = SingleSetPoint;
					} else {
						this_loop.LoopDemandCalcScheme = DualSetPointDeadBand;
					}
				} else if ( SameString( Alpha( 16 ), "" ) ) {
					this_loop.LoopDemandCalcScheme = SingleSetPoint;
				} else {
					ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alpha( 1 ) + "\", Invalid choice." );
					ShowContinueError( "..." + cAlphaFieldNames( 16 ) + "=\"" + Alpha( 16 ) + "\"." );
					ShowContinueError( "Will default to SingleSetPoint." ); // TODO rename point
					this_loop.LoopDemandCalcScheme = SingleSetPoint;
				}
			} else if ( this_loop.TypeOfLoop == Condenser ) {
				this_loop.LoopDemandCalcScheme = SingleSetPoint;
			}

			//When Commonpipe is allowed in condenser loop modify this code. Sankar 06/29/2009
			if ( this_loop.TypeOfLoop == Plant ) {
				if ( SameString( Alpha( 17 ), "CommonPipe" ) ) {
					this_loop.CommonPipeType = CommonPipe_Single;
				} else if ( SameString( Alpha( 17 ), "TwoWayCommonPipe" ) ) {
					this_loop.CommonPipeType = CommonPipe_TwoWay;
				} else if ( SameString( Alpha( 17 ), "None" ) || lAlphaFieldBlanks( 17 ) ) {
					this_loop.CommonPipeType = CommonPipe_No;
				} else {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alpha( 1 ) + "\", Invalid choice." );
					ShowContinueError( "Invalid " + cAlphaFieldNames( 17 ) + "=\"" + Alpha( 17 ) + "\"." );
					ShowContinueError( "Refer to I/O reference document for more details." );
					ErrorsFound = true;
				}
			} else if ( this_loop.TypeOfLoop == Condenser ) {
				this_loop.CommonPipeType = CommonPipe_No;
			}

			if ( this_loop.CommonPipeType == CommonPipe_TwoWay ) {
				if ( this_demand_side.InletNodeSetPt && this_supply_side.InletNodeSetPt ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alpha( 1 ) + "\", Invalid condition." );
					ShowContinueError( "While using a two way common pipe there can be setpoint on only one node other than Plant Supply Outlet node." );
					ShowContinueError( "Currently both Plant Demand inlet and plant supply inlet have setpoints." );
					ShowContinueError( "Select one of the two nodes and rerun the simulation." );
					ErrorsFound = true;
				}
				if ( ! this_demand_side.InletNodeSetPt && ! this_supply_side.InletNodeSetPt ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alpha( 1 ) + "\", Invalid condition." );
					ShowContinueError( "While using a two way common pipe there must be a setpoint in addition to the Plant Supply Outlet node." );
					ShowContinueError( "Currently neither plant demand inlet nor plant supply inlet have setpoints." );
					ShowContinueError( "Select one of the two nodes and rerun the simulation." );
					ErrorsFound = true;
				}
			}

			//Pressure Simulation Type Input
			//First set the alpha index in the object as it is different for plant/condenser
			//When CommonPipe, etc., is allowed in condenser loop, modify this code.  Edwin/Sankar 08/12/2009
			if ( this_loop.TypeOfLoop == Plant ) {
				PressSimAlphaIndex = 18;
			} else {
				PressSimAlphaIndex = 15;
			}

			if ( NumAlphas >= PressSimAlphaIndex ) {
				MatchedPressureString = false;

				//Check all types
				for ( PressSimLoop = 1; PressSimLoop <= 4; ++PressSimLoop ) {
					if ( SameString( Alpha( PressSimAlphaIndex ), PressureSimType( PressSimLoop ) ) ) {
						this_loop.PressureSimType = PressSimLoop;
						MatchedPressureString = true;
						break;
					}
				}

				//If we found a match, check to make sure it is one of the valid
				// ones for this phase of pressure implementation
				if ( MatchedPressureString ) {
					if ( ( this_loop.PressureSimType == Press_NoPressure ) || ( this_loop.PressureSimType == Press_PumpPowerCorrection ) || ( this_loop.PressureSimType == Press_FlowCorrection ) ) {
						//We are OK here, move on
					} else {
						//We have an erroneous input, alert user
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alpha( 1 ) + "\", Invalid choice." );
						ShowContinueError( "Invalid " + cAlphaFieldNames( PressSimAlphaIndex ) + "=\"" + Alpha( PressSimAlphaIndex ) + "\"." );
						ShowContinueError( "Currently only options are: " );
						ShowContinueError( "  - " + PressureSimType( Press_NoPressure ) );
						ShowContinueError( "  - " + PressureSimType( Press_PumpPowerCorrection ) );
						ShowContinueError( "  - " + PressureSimType( Press_FlowCorrection ) );
						ErrorsFound = true;
					}
				}

				//if we made it this far and didn't get a match, check for blank
				if ( ! MatchedPressureString ) {
					if ( Alpha( PressSimAlphaIndex ) == "" ) {
						this_loop.PressureSimType = Press_NoPressure;
						MatchedPressureString = true;
						break;
					}
				}

				//if we made it this far, there was no match, and it wasn't blank
				if ( ! MatchedPressureString ) {
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alpha( 1 ) + "\", Invalid condition." );
					ShowContinueError( "Invalid " + cAlphaFieldNames( PressSimAlphaIndex ) + "=\"" + Alpha( PressSimAlphaIndex ) + "\"." );
					ErrorsFound = true;
				}

			}

			ErrFound = false;

			if ( this_loop.TypeOfLoop == Plant ) {
				GetPlantAvailabilityManager( Alpha( 15 ), LoopNum, TotNumLoops, ErrFound );
			}

			if ( ErrFound ) {
				ShowContinueError( "Input errors in  " + CurrentModuleObject + '=' + Alpha( 1 ) );
				ErrorsFound = true;
			}

			if ( GetFirstBranchInletNodeName( this_demand_side.BranchList ) != this_demand_side.NodeNameIn ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alpha( 1 ) + "\", Invalid condition." );
				ShowContinueError( "The inlet node of the first branch in the " + cAlphaFieldNames( 12 ) + '=' + Alpha( 12 ) ); //"Plant Demand Side Branch List"
				ShowContinueError( "is not the same as the " + cAlphaFieldNames( 10 ) + '=' + Alpha( 10 ) ); // "Plant Demand Side Inlet Node Name"
				ShowContinueError( "Branch List Inlet Node Name=" + GetFirstBranchInletNodeName( this_demand_side.BranchList ) ); // TODO rename point
				ShowContinueError( "Branches in a BRANCH LIST must be listed in flow order: inlet branch, then parallel branches, then outlet branch." ); // TODO rename point
				ErrorsFound = true;
			}

			if ( GetLastBranchOutletNodeName( this_demand_side.BranchList ) != this_demand_side.NodeNameOut ) {
				//"Plant Demand Side Branch List"
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alpha( 1 ) + "\", Invalid condition." );
				ShowContinueError( "The outlet node of the last branch in the " + cAlphaFieldNames( 12 ) + '=' + Alpha( 12 ) );
				//"Plant Demand Side Outlet Node Name"
				ShowContinueError( "is not the same as the " + cAlphaFieldNames( 11 ) + '=' + Alpha( 11 ) );
				ShowContinueError( "Branch List Outlet Node Name=" + GetLastBranchOutletNodeName( this_demand_side.BranchList ) ); // TODO rename point
				// TODO rename point
				ShowContinueError( "Branches in a BRANCH LIST must be listed in flow order: inlet branch, then parallel branches, then outlet branch." );
				ErrorsFound = true;
			}

			if ( GetFirstBranchInletNodeName( this_supply_side.BranchList ) != this_supply_side.NodeNameIn ) {
				//"Plant Supply Side Branch List"
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alpha( 1 ) + "\", Invalid condition." );
				ShowContinueError( "The inlet node of the first branch in the " + cAlphaFieldNames( 8 ) + '=' + Alpha( 8 ) );
				//"Plant Supply Side Inlet Node Name
				ShowContinueError( "is not the same as the " + cAlphaFieldNames( 6 ) + '=' + Alpha( 6 ) );
				ShowContinueError( "Branch List Inlet Node Name=" + GetFirstBranchInletNodeName( this_supply_side.BranchList ) ); // TODO rename point
				// TODO rename point
				ShowContinueError( "Branches in a BRANCH LIST must be listed in flow order: inlet branch, then parallel branches, then outlet branch." );
				ErrorsFound = true;
			}

			if ( GetLastBranchOutletNodeName( this_supply_side.BranchList ) != this_supply_side.NodeNameOut ) {
				//"Plant Supply Side Branch List"
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alpha( 1 ) + "\", Invalid condition." );
				ShowContinueError( "The outlet node of the last branch in the " + cAlphaFieldNames( 8 ) + '=' + Alpha( 8 ) );
				//"Plant Supply Side Outlet Node Name"
				ShowContinueError( "is not the same as the " + cAlphaFieldNames( 7 ) + '=' + Alpha( 7 ) );
				ShowContinueError( "Branch List Outlet Node Name=" + GetLastBranchOutletNodeName( this_supply_side.BranchList ) ); // TODO rename point
				// TODO rename point
				ShowContinueError( "Branches in a BRANCH LIST must be listed in flow order: inlet branch, then parallel branches, then outlet branch." );
				ErrorsFound = true;
			}

		}

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found in processing input. Preceding conditions cause termination." );
		}

		// set up loop status (set by system availability managers) report variables
		// Condenser loop does not have availability manager yet. Once implemented, move the setup output variable to
		// outside the IF statement.
		for ( LoopNum = 1; LoopNum <= TotNumLoops; ++LoopNum ) {

			SetupOutputVariable( "Plant System Cycle On Off Status []", PlantAvailMgr( LoopNum ).AvailStatus, "Plant", "Average", PlantLoop( LoopNum ).Name );

		}

	}

	void
	GetPlantInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Sankaranarayanan K P
		//       DATE WRITTEN   April 2005
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine gets input either through the Plant Loop derived type
		// or by calls out to the branch manager to obtain data.  By the end of
		// the routine the module level derived type Loop should be fully allocated
		// and fully populated.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace InputProcessor;
		using namespace NodeInputManager;
		using namespace BranchInputManager;

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumHalfLoops;
		int LoopNum; // DO loop counter for loops
		int HalfLoopNum;
		int NumOfPipesInLoop;
		int SysPipeNum;
		int LoopSideNum;
		int BranchNum; // DO loop counter for branches
		int CompNum; // DO loop counter for components
		int NodeNum; // DO loop counter for nodes
		int PipeNum; // Counter for pipes
		int Outlet;
		int Inlet;
		int NumParams;
		int NumAlphas;
		int NumNumbers;
		int SplitNum;
		int MixNum;
		int NumConnectorsInLoop;
		int ConnNum;
		std::string::size_type Pos;
		int TotCompsOnBranch;
		int MaxNumAlphas;
		int MaxNumNumbers;

		bool SplitInBranch;
		bool MixerOutBranch;
		static bool ErrorsFound( false );
		bool DemandSideHasPump;
		bool ASeriesBranchHasPump;
		bool AParallelBranchHasPump;

		std::string LoopIdentifier;

		static Array1D_string BranchNames; // Branch names from GetBranchList call
		static Array1D_string CompTypes; // Branch names from GetBranchList call
		static Array1D_string CompNames; // Branch names from GetBranchList call
		static Array1D_int CompCtrls; // Branch names from GetBranchList call
		static Array1D_string InletNodeNames; // Node names from GetBranchData call
		static Array1D_string OutletNodeNames; // Node names from GetBranchData call
		static Array1D_int InletNodeNumbers; // Node numbers from GetBranchData call
		static Array1D_int OutletNodeNumbers; // Node numbers from GetBranchData call
		static Array1D_bool SplitOutBranch;
		static Array1D_bool MixerInBranch;
		bool errFlag;
		int GeneralEquipType;
		int TypeOfNum;
		int LoopNumInArray;

		GetObjectDefMaxArgs( "Connector:Splitter", NumParams, NumAlphas, NumNumbers );
		MaxNumAlphas = NumAlphas;
		MaxNumNumbers = NumNumbers;
		GetObjectDefMaxArgs( "Connector:Mixer", NumParams, NumAlphas, NumNumbers );
		MaxNumAlphas = max( MaxNumAlphas, NumAlphas );
		MaxNumNumbers = max( MaxNumNumbers, NumNumbers );
		// FLOW:

		//  TotNumLoops = NumPlantLoops + NumCondLoops    !Needed when including condenser.
		NumHalfLoops = 2 * TotNumLoops; //Will be NumLoops when condenser added
		NumPipes = 0;
		NumPlantPipes = 0;
		NumCondPipes = 0;
		LoopPipe.allocate( NumHalfLoops );
		HalfLoopNum = 0;
		SysPipeNum = 0;

		for ( LoopNum = 1; LoopNum <= TotNumLoops; ++LoopNum ) { // Begin demand side loops ... When condenser is added becomes NumLoops
			DemandSideHasPump = false;
			TempLoop.LoopHasConnectionComp = false;
			TempLoop.Name = PlantLoop( LoopNum ).Name;

			for ( LoopSideNum = DemandSide; LoopSideNum <= SupplySide; ++LoopSideNum ) {
				ASeriesBranchHasPump = false;
				AParallelBranchHasPump = false;
				NumOfPipesInLoop = 0; // Initialization
				++HalfLoopNum;
				TempLoop.BypassExists = false;
				if ( PlantLoop( LoopNum ).TypeOfLoop == Plant && LoopSideNum == DemandSide ) {
					LoopIdentifier = "Plant Demand";
				} else if ( PlantLoop( LoopNum ).TypeOfLoop == Plant && LoopSideNum == SupplySide ) {
					LoopIdentifier = "Plant Supply";
				} else if ( PlantLoop( LoopNum ).TypeOfLoop == Condenser && LoopSideNum == DemandSide ) {
					LoopIdentifier = "Condenser Demand";
				} else if ( PlantLoop( LoopNum ).TypeOfLoop == Condenser && LoopSideNum == SupplySide ) {
					LoopIdentifier = "Condenser Supply";
				}

				TempLoop.BranchList = PlantLoop( LoopNum ).LoopSide( LoopSideNum ).BranchList;
				TempLoop.ConnectList = PlantLoop( LoopNum ).LoopSide( LoopSideNum ).ConnectList;

				// Get the branch list and size the Branch portion of the Loop derived type
				TempLoop.TotalBranches = NumBranchesInBranchList( TempLoop.BranchList );
				BranchNames.allocate( TempLoop.TotalBranches );
				BranchNames = "";
				GetBranchList( TempLoop.Name, TempLoop.BranchList, TempLoop.TotalBranches, BranchNames, LoopIdentifier );
				TempLoop.Branch.allocate( TempLoop.TotalBranches );

				// Cycle through all of the branches and set up the node data
				for ( BranchNum = 1; BranchNum <= TempLoop.TotalBranches; ++BranchNum ) {

					TempLoop.Branch( BranchNum ).Name = BranchNames( BranchNum );

					TempLoop.Branch( BranchNum ).TotalComponents = NumCompsInBranch( BranchNames( BranchNum ) );

					TempLoop.Branch( BranchNum ).IsBypass = false;

					CompTypes.allocate( TempLoop.Branch( BranchNum ).TotalComponents );
					CompNames.allocate( TempLoop.Branch( BranchNum ).TotalComponents );
					CompCtrls.dimension( TempLoop.Branch( BranchNum ).TotalComponents, 0 );
					InletNodeNames.allocate( TempLoop.Branch( BranchNum ).TotalComponents );
					InletNodeNumbers.dimension( TempLoop.Branch( BranchNum ).TotalComponents, 0 );
					OutletNodeNames.allocate( TempLoop.Branch( BranchNum ).TotalComponents );
					OutletNodeNumbers.dimension( TempLoop.Branch( BranchNum ).TotalComponents, 0 );

					GetBranchData( TempLoop.Name, BranchNames( BranchNum ), TempLoop.Branch( BranchNum ).MaxVolFlowRate, TempLoop.Branch( BranchNum ).PressureCurveType, TempLoop.Branch( BranchNum ).PressureCurveIndex, TempLoop.Branch( BranchNum ).TotalComponents, CompTypes, CompNames, InletNodeNames, InletNodeNumbers, OutletNodeNames, OutletNodeNumbers, ErrorsFound ); // Why is this Vdot and not mdot?

					TempLoop.Branch( BranchNum ).Comp.allocate( TempLoop.Branch( BranchNum ).TotalComponents );

					for ( CompNum = 1; CompNum <= TempLoop.Branch( BranchNum ).TotalComponents; ++CompNum ) {
						// set up some references
						auto & this_comp_type( CompTypes( CompNum ) );
						auto & this_comp( TempLoop.Branch( BranchNum ).Comp( CompNum ) );

						this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
						this_comp.TypeOf = this_comp_type;

						if ( SameString( this_comp_type, "Pipe:Adiabatic" ) ) {
							this_comp.TypeOf_Num = TypeOf_Pipe;
							this_comp.GeneralEquipType = GenEquipTypes_Pipe;
							this_comp.CurOpSchemeType = NoControlOpSchemeType;
							this_comp.compPtr = Pipes::LocalPipeData::factory( TypeOf_Pipe, CompNames( CompNum ) );
						} else if ( SameString( this_comp_type, "Pipe:Adiabatic:Steam" ) ) {
							this_comp.TypeOf_Num = TypeOf_PipeSteam;
							this_comp.GeneralEquipType = GenEquipTypes_Pipe;
							this_comp.CurOpSchemeType = NoControlOpSchemeType;
							this_comp.compPtr = Pipes::LocalPipeData::factory( TypeOf_PipeSteam, CompNames( CompNum ) );
						} else if ( SameString( this_comp_type, "Pipe:Outdoor" ) ) {
							this_comp.TypeOf_Num = TypeOf_PipeExterior;
							this_comp.GeneralEquipType = GenEquipTypes_Pipe;
							this_comp.CurOpSchemeType = NoControlOpSchemeType;
							this_comp.compPtr = PipeHeatTransfer::PipeHTData::factory( TypeOf_PipeExterior, CompNames( CompNum ) );
						} else if ( SameString( this_comp_type, "Pipe:Indoor" ) ) {
							this_comp.TypeOf_Num = TypeOf_PipeInterior;
							this_comp.GeneralEquipType = GenEquipTypes_Pipe;
							this_comp.CurOpSchemeType = NoControlOpSchemeType;
							this_comp.compPtr = PipeHeatTransfer::PipeHTData::factory( TypeOf_PipeInterior, CompNames( CompNum ) );
						} else if ( SameString( this_comp_type, "Pipe:Underground" ) ) {
							this_comp.TypeOf_Num = TypeOf_PipeUnderground;
							this_comp.GeneralEquipType = GenEquipTypes_Pipe;
							this_comp.CurOpSchemeType = NoControlOpSchemeType;
							this_comp.compPtr = PipeHeatTransfer::PipeHTData::factory( TypeOf_PipeUnderground, CompNames( CompNum ) );
						} else if ( SameString( this_comp_type, "PipingSystem:Underground:PipeCircuit" ) ) {
							this_comp.TypeOf_Num = TypeOf_PipingSystemPipeCircuit;
							this_comp.GeneralEquipType = GenEquipTypes_Pipe;
							this_comp.CurOpSchemeType = NoControlOpSchemeType;
						} else if ( has_prefixi( this_comp_type, "Pump" ) || has_prefixi( this_comp_type, "HeaderedPumps" ) ) {
							if ( has_prefixi( this_comp_type, "Pump:VariableSpeed" ) ) {
								this_comp.TypeOf_Num = TypeOf_PumpVariableSpeed;
							} else if ( has_prefixi( this_comp_type, "Pump:ConstantSpeed" ) ) {
								this_comp.TypeOf_Num = TypeOf_PumpConstantSpeed;
							} else if ( has_prefixi( this_comp_type, "Pump:VariableSpeed:Condensate" ) ) {
								this_comp.TypeOf_Num = TypeOf_PumpCondensate;
							} else if ( has_prefixi( this_comp_type, "HeaderedPumps:ConstantSpeed" ) ) {
								this_comp.TypeOf_Num = TypeOf_PumpBankConstantSpeed;
							} else if ( has_prefixi( this_comp_type, "HeaderedPumps:VariableSpeed" ) ) {
								this_comp.TypeOf_Num = TypeOf_PumpBankVariableSpeed;
							} else {
								//discover unsupported equipment on branches.
								ShowSevereError( "GetPlantInput: trying to process a pump type that is not supported, dev note" );
								ShowContinueError( "Component Type =" + this_comp_type );
							}
							this_comp.GeneralEquipType = GenEquipTypes_Pump;
							this_comp.CurOpSchemeType = PumpOpSchemeType;
							if ( LoopSideNum == DemandSide ) DemandSideHasPump = true;
							if ( BranchNum == 1 || BranchNum == TempLoop.TotalBranches ) {
								ASeriesBranchHasPump = true;
							} else {
								AParallelBranchHasPump = true;
							}
							StoreAPumpOnCurrentTempLoop( LoopNum, LoopSideNum, BranchNum, CompNum, CompNames( CompNum ), OutletNodeNumbers( CompNum ), AParallelBranchHasPump );
						} else if ( SameString( this_comp_type, "WaterHeater:Mixed" ) ) {
							this_comp.TypeOf_Num = TypeOf_WtrHeaterMixed;
							this_comp.GeneralEquipType = GenEquipTypes_WaterThermalTank;
							if ( LoopSideNum == DemandSide ) {
								this_comp.CurOpSchemeType = DemandOpSchemeType;
							} else if ( LoopSideNum == SupplySide ) {
								this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
							}
						} else if ( SameString( this_comp_type, "WaterHeater:Stratified" ) ) {
							this_comp.TypeOf_Num = TypeOf_WtrHeaterStratified;
							this_comp.GeneralEquipType = GenEquipTypes_WaterThermalTank;
							if ( LoopSideNum == DemandSide ) {
								this_comp.CurOpSchemeType = DemandOpSchemeType;
							} else if ( LoopSideNum == SupplySide ) {
								this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
							}
						} else if ( SameString( this_comp_type, "ChillerHeater:Absorption:Directfired" ) ) {
							this_comp.TypeOf_Num = TypeOf_Chiller_DFAbsorption;
							this_comp.GeneralEquipType = GenEquipTypes_Chiller;
						} else if ( SameString( this_comp_type, "ChillerHeater:Absorption:DoubleEffect" ) ) {
							this_comp.TypeOf_Num = TypeOf_Chiller_ExhFiredAbsorption;
							this_comp.GeneralEquipType = GenEquipTypes_Chiller;
						} else if ( SameString( this_comp_type, "ThermalStorage:ChilledWater:Mixed" ) ) {
							this_comp.TypeOf_Num = TypeOf_ChilledWaterTankMixed;
							this_comp.GeneralEquipType = GenEquipTypes_ThermalStorage;
							if ( LoopSideNum == DemandSide ) {
								this_comp.CurOpSchemeType = DemandOpSchemeType;
							} else if ( LoopSideNum == SupplySide ) {
								this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
							}
						} else if ( SameString( this_comp_type, "ThermalStorage:ChilledWater:Stratified" ) ) {
							this_comp.TypeOf_Num = TypeOf_ChilledWaterTankStratified;
							this_comp.GeneralEquipType = GenEquipTypes_ThermalStorage;
							if ( LoopSideNum == DemandSide ) {
								this_comp.CurOpSchemeType = DemandOpSchemeType;
							} else if ( LoopSideNum == SupplySide ) {
								this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
							}
						} else if ( SameString( this_comp_type, "WaterUse:Connections" ) ) {
							this_comp.TypeOf_Num = TypeOf_WaterUseConnection;
							this_comp.GeneralEquipType = GenEquipTypes_WaterUse;
							this_comp.CurOpSchemeType = DemandOpSchemeType;
						} else if ( SameString( this_comp_type, "Coil:Cooling:Water" ) ) {
							this_comp.TypeOf_Num = TypeOf_CoilWaterCooling;
							this_comp.GeneralEquipType = GenEquipTypes_DemandCoil;
							this_comp.CurOpSchemeType = DemandOpSchemeType;
						} else if ( SameString( this_comp_type, "Coil:Cooling:Water:DetailedGeometry" ) ) {
							this_comp.TypeOf_Num = TypeOf_CoilWaterDetailedFlatCooling;
							this_comp.GeneralEquipType = GenEquipTypes_DemandCoil;
							this_comp.CurOpSchemeType = DemandOpSchemeType;
						} else if ( SameString( this_comp_type, "Coil:Heating:Water" ) ) {
							this_comp.TypeOf_Num = TypeOf_CoilWaterSimpleHeating;
							this_comp.GeneralEquipType = GenEquipTypes_DemandCoil;
							this_comp.CurOpSchemeType = DemandOpSchemeType;
						} else if ( SameString( this_comp_type, "Coil:Heating:Steam" ) ) {
							this_comp.TypeOf_Num = TypeOf_CoilSteamAirHeating;
							this_comp.GeneralEquipType = GenEquipTypes_DemandCoil;
							this_comp.CurOpSchemeType = DemandOpSchemeType;
						} else if ( SameString( this_comp_type, "SolarCollector:FlatPlate:Water" ) ) {
							this_comp.TypeOf_Num = TypeOf_SolarCollectorFlatPlate;
							this_comp.GeneralEquipType = GenEquipTypes_SolarCollector;
							if ( LoopSideNum == DemandSide ) {
								this_comp.CurOpSchemeType = DemandOpSchemeType;
							} else if ( LoopSideNum == SupplySide ) {
								this_comp.CurOpSchemeType = UncontrolledOpSchemeType;
							}
						} else if ( SameString( this_comp_type, "SolarCollector:IntegralCollectorStorage" ) ) {
							this_comp.TypeOf_Num = TypeOf_SolarCollectorICS;
							this_comp.GeneralEquipType = GenEquipTypes_SolarCollector;
							if ( LoopSideNum == DemandSide ) {
								this_comp.CurOpSchemeType = DemandOpSchemeType;
							} else if ( LoopSideNum == SupplySide ) {
								this_comp.CurOpSchemeType = UncontrolledOpSchemeType;
							}
						} else if ( SameString( this_comp_type, "LoadProfile:Plant" ) ) {
							this_comp.TypeOf_Num = TypeOf_PlantLoadProfile;
							this_comp.GeneralEquipType = GenEquipTypes_LoadProfile;
							this_comp.CurOpSchemeType = DemandOpSchemeType;
							this_comp.compPtr = PlantLoadProfile::PlantProfileData::factory( CompNames( CompNum ) );
						} else if ( SameString( this_comp_type, "GroundHeatExchanger:Vertical" ) ) {
							this_comp.TypeOf_Num = TypeOf_GrndHtExchgVertical;
							this_comp.GeneralEquipType = GenEquipTypes_GroundHeatExchanger;
							this_comp.CurOpSchemeType = UncontrolledOpSchemeType;
							this_comp.compPtr = GroundHeatExchangers::GLHEBase::factory( TypeOf_GrndHtExchgVertical, CompNames( CompNum ) );
						} else if ( SameString( this_comp_type, "GroundHeatExchanger:Surface" ) ) {
							this_comp.TypeOf_Num = TypeOf_GrndHtExchgSurface;
							this_comp.GeneralEquipType = GenEquipTypes_GroundHeatExchanger;
							this_comp.CurOpSchemeType = UncontrolledOpSchemeType;
							this_comp.compPtr = SurfaceGroundHeatExchanger::SurfaceGroundHeatExchangerData::factory( TypeOf_GrndHtExchgSurface, CompNames( CompNum ) );
						} else if ( SameString( this_comp_type, "GroundHeatExchanger:Pond" ) ) {
							this_comp.TypeOf_Num = TypeOf_GrndHtExchgPond;
							this_comp.GeneralEquipType = GenEquipTypes_GroundHeatExchanger;
							this_comp.CurOpSchemeType = UncontrolledOpSchemeType;
							this_comp.compPtr = PondGroundHeatExchanger::PondGroundHeatExchangerData::factory( TypeOf_GrndHtExchgPond, CompNames( CompNum ) );
						} else if ( SameString( this_comp_type, "GroundHeatExchanger:Slinky" ) ) {
							this_comp.TypeOf_Num = TypeOf_GrndHtExchgSlinky;
							this_comp.GeneralEquipType = GenEquipTypes_GroundHeatExchanger;
							this_comp.CurOpSchemeType = UncontrolledOpSchemeType;
							this_comp.compPtr = GroundHeatExchangers::GLHEBase::factory( TypeOf_GrndHtExchgSlinky, CompNames( CompNum ) );
						} else if ( SameString( this_comp_type, "Chiller:Electric:EIR" ) ) {
							this_comp.TypeOf_Num = TypeOf_Chiller_ElectricEIR;
							this_comp.GeneralEquipType = GenEquipTypes_Chiller;
							if ( LoopSideNum == DemandSide ) {
								this_comp.CurOpSchemeType = DemandOpSchemeType;
							} else if ( LoopSideNum == SupplySide ) {
								this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
							}
						} else if ( SameString( this_comp_type, "Chiller:Electric:ReformulatedEIR" ) ) {
							this_comp.TypeOf_Num = TypeOf_Chiller_ElectricReformEIR;
							this_comp.GeneralEquipType = GenEquipTypes_Chiller;
							if ( LoopSideNum == DemandSide ) {
								this_comp.CurOpSchemeType = DemandOpSchemeType;
							} else if ( LoopSideNum == SupplySide ) {
								this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
							}
						} else if ( SameString( this_comp_type, "Chiller:Electric" ) ) {
							this_comp.TypeOf_Num = TypeOf_Chiller_Electric;
							this_comp.GeneralEquipType = GenEquipTypes_Chiller;
							if ( LoopSideNum == DemandSide ) {
								this_comp.CurOpSchemeType = DemandOpSchemeType;
							} else if ( LoopSideNum == SupplySide ) {
								this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
							}
						} else if ( SameString( this_comp_type, "Chiller:EngineDriven" ) ) {
							this_comp.TypeOf_Num = TypeOf_Chiller_EngineDriven;
							this_comp.GeneralEquipType = GenEquipTypes_Chiller;
							if ( LoopSideNum == DemandSide ) {
								this_comp.CurOpSchemeType = DemandOpSchemeType;
							} else if ( LoopSideNum == SupplySide ) {
								this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
							}
						} else if ( SameString( this_comp_type, "Chiller:CombustionTurbine" ) ) {
							this_comp.TypeOf_Num = TypeOf_Chiller_CombTurbine;
							this_comp.GeneralEquipType = GenEquipTypes_Chiller;
							if ( LoopSideNum == DemandSide ) {
								this_comp.CurOpSchemeType = DemandOpSchemeType;
							} else if ( LoopSideNum == SupplySide ) {
								this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
							}
						} else if ( SameString( this_comp_type, "Chiller:ConstantCOP" ) ) {
							this_comp.TypeOf_Num = TypeOf_Chiller_ConstCOP;
							this_comp.GeneralEquipType = GenEquipTypes_Chiller;
							if ( LoopSideNum == DemandSide ) {
								this_comp.CurOpSchemeType = DemandOpSchemeType;
							} else if ( LoopSideNum == SupplySide ) {
								this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
							}
						} else if ( SameString( this_comp_type, "Boiler:HotWater" ) ) {
							this_comp.TypeOf_Num = TypeOf_Boiler_Simple;
							this_comp.GeneralEquipType = GenEquipTypes_Boiler;
							this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
						} else if ( SameString( this_comp_type, "Boiler:Steam" ) ) {
							this_comp.TypeOf_Num = TypeOf_Boiler_Steam;
							this_comp.GeneralEquipType = GenEquipTypes_Boiler;
							this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
						} else if ( SameString( this_comp_type, "Chiller:Absorption:Indirect" ) ) {
							this_comp.TypeOf_Num = TypeOf_Chiller_Indirect_Absorption;
							this_comp.GeneralEquipType = GenEquipTypes_Chiller;
							if ( LoopSideNum == DemandSide ) {
								this_comp.CurOpSchemeType = DemandOpSchemeType;
							} else if ( LoopSideNum == SupplySide ) {
								this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
							}
						} else if ( SameString( this_comp_type, "Chiller:Absorption" ) ) {
							this_comp.TypeOf_Num = TypeOf_Chiller_Absorption;
							this_comp.GeneralEquipType = GenEquipTypes_Chiller;
							if ( LoopSideNum == DemandSide ) {
								this_comp.CurOpSchemeType = DemandOpSchemeType;
							} else if ( LoopSideNum == SupplySide ) {
								this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
							}
						} else if ( SameString( this_comp_type, "CoolingTower:SingleSpeed" ) ) {
							this_comp.TypeOf_Num = TypeOf_CoolingTower_SingleSpd;
							this_comp.GeneralEquipType = GenEquipTypes_CoolingTower;
							this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
						} else if ( SameString( this_comp_type, "CoolingTower:TwoSpeed" ) ) {
							this_comp.TypeOf_Num = TypeOf_CoolingTower_TwoSpd;
							this_comp.GeneralEquipType = GenEquipTypes_CoolingTower;
							this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
						} else if ( SameString( this_comp_type, "CoolingTower:VariableSpeed" ) ) {
							this_comp.TypeOf_Num = TypeOf_CoolingTower_VarSpd;
							this_comp.GeneralEquipType = GenEquipTypes_CoolingTower;
						} else if ( SameString( this_comp_type, "CoolingTower:VariableSpeed:Merkel" ) ) {
							this_comp.TypeOf_Num = TypeOf_CoolingTower_VarSpdMerkel;
							this_comp.GeneralEquipType = GenEquipTypes_CoolingTower;
						} else if ( SameString( this_comp_type, "Generator:FuelCell:ExhaustGasToWaterHeatExchanger" ) ) {
							this_comp.TypeOf_Num = TypeOf_Generator_FCExhaust;
							this_comp.GeneralEquipType = GenEquipTypes_Generator;
							this_comp.CurOpSchemeType = DemandOpSchemeType;
						} else if ( SameString( this_comp_type, "WaterHeater:HeatPump:PumpedCondenser" ) ) {
							this_comp.TypeOf_Num = TypeOf_HeatPumpWtrHeaterPumped;
							this_comp.GeneralEquipType = GenEquipTypes_WaterThermalTank;
							this_comp.CurOpSchemeType = DemandOpSchemeType;
						} else if ( SameString( this_comp_type, "WaterHeater:HeatPump:WrappedCondenser" ) ) {
							this_comp.TypeOf_Num = TypeOf_HeatPumpWtrHeaterWrapped;
							this_comp.GeneralEquipType = GenEquipTypes_WaterThermalTank;
							this_comp.CurOpSchemeType = DemandOpSchemeType;
						} else if ( SameString( this_comp_type, "HeatPump:WatertoWater:EquationFit:Cooling" ) ) {
							this_comp.TypeOf_Num = TypeOf_HPWaterEFCooling;
							this_comp.GeneralEquipType = GenEquipTypes_HeatPump;
							if ( LoopSideNum == DemandSide ) {
								this_comp.CurOpSchemeType = DemandOpSchemeType;
							} else if ( LoopSideNum == SupplySide ) {
								this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
							}
						} else if ( SameString( this_comp_type, "HeatPump:WatertoWater:EquationFit:Heating" ) ) {
							this_comp.TypeOf_Num = TypeOf_HPWaterEFHeating;
							this_comp.GeneralEquipType = GenEquipTypes_HeatPump;
							if ( LoopSideNum == DemandSide ) {
								this_comp.CurOpSchemeType = DemandOpSchemeType;
							} else if ( LoopSideNum == SupplySide ) {
								this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
							}
						} else if ( SameString( this_comp_type, "HeatPump:WaterToWater:ParameterEstimation:Heating" ) ) {
							this_comp.TypeOf_Num = TypeOf_HPWaterPEHeating;
							this_comp.GeneralEquipType = GenEquipTypes_HeatPump;
							if ( LoopSideNum == DemandSide ) {
								this_comp.CurOpSchemeType = DemandOpSchemeType;
							} else if ( LoopSideNum == SupplySide ) {
								this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
							}
						} else if ( SameString( this_comp_type, "HeatPump:WaterToWater:ParameterEstimation:Cooling" ) ) {
							this_comp.TypeOf_Num = TypeOf_HPWaterPECooling;
							this_comp.GeneralEquipType = GenEquipTypes_HeatPump;
							if ( LoopSideNum == DemandSide ) {
								this_comp.CurOpSchemeType = DemandOpSchemeType;
							} else if ( LoopSideNum == SupplySide ) {
								this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
							}
						} else if ( SameString( this_comp_type, "AirConditioner:VariableRefrigerantFlow" ) ) {
							this_comp.TypeOf_Num = TypeOf_HeatPumpVRF;
							this_comp.GeneralEquipType = GenEquipTypes_HeatPump;
							if ( LoopSideNum == DemandSide ) {
								this_comp.CurOpSchemeType = DemandOpSchemeType;
							} else if ( LoopSideNum == SupplySide ) {
								this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
							}
						} else if ( SameString( this_comp_type, "DistrictCooling" ) ) {
							this_comp.TypeOf_Num = TypeOf_PurchChilledWater;
							this_comp.GeneralEquipType = GenEquipTypes_Purchased;
						} else if ( SameString( this_comp_type, "DistrictHeating" ) ) {
							this_comp.TypeOf_Num = TypeOf_PurchHotWater;
							this_comp.GeneralEquipType = GenEquipTypes_Purchased;
						} else if ( SameString( this_comp_type, "ThermalStorage:Ice:Simple" ) ) {
							this_comp.TypeOf_Num = TypeOf_TS_IceSimple;
							this_comp.GeneralEquipType = GenEquipTypes_ThermalStorage;
						} else if ( SameString( this_comp_type, "ThermalStorage:Ice:Detailed" ) ) {
							this_comp.TypeOf_Num = TypeOf_TS_IceDetailed;
							this_comp.GeneralEquipType = GenEquipTypes_ThermalStorage;
						} else if ( SameString( this_comp_type, "TemperingValve" ) ) {
							this_comp.TypeOf_Num = TypeOf_ValveTempering;
							this_comp.GeneralEquipType = GenEquipTypes_Valve;
						} else if ( SameString( this_comp_type, "HeatExchanger:FluidToFluid" ) ) {
							this_comp.TypeOf_Num = TypeOf_FluidToFluidPlantHtExchg;
							this_comp.GeneralEquipType = GenEquipTypes_HeatExchanger;
							if ( LoopSideNum == DemandSide ) {
								this_comp.CurOpSchemeType = DemandOpSchemeType;
							} else if ( LoopSideNum == SupplySide ) {
								this_comp.CurOpSchemeType = FreeRejectionOpSchemeType;
							}
						} else if ( SameString( this_comp_type, "Generator:MicroTurbine" ) ) {
							this_comp.TypeOf_Num = TypeOf_Generator_MicroTurbine;
							this_comp.GeneralEquipType = GenEquipTypes_Generator;
							this_comp.CurOpSchemeType = DemandOpSchemeType;
						} else if ( SameString( this_comp_type, "Generator:InternalCombustionEngine" ) ) {
							this_comp.TypeOf_Num = TypeOf_Generator_ICEngine;
							this_comp.GeneralEquipType = GenEquipTypes_Generator;
							this_comp.CurOpSchemeType = DemandOpSchemeType;
						} else if ( SameString( this_comp_type, "Generator:CombustionTurbine" ) ) {
							this_comp.TypeOf_Num = TypeOf_Generator_CTurbine;
							this_comp.GeneralEquipType = GenEquipTypes_Generator;
							this_comp.CurOpSchemeType = DemandOpSchemeType;
						} else if ( SameString( this_comp_type, "Generator:MicroCHP" ) ) {
							this_comp.TypeOf_Num = TypeOf_Generator_MicroCHP;
							this_comp.GeneralEquipType = GenEquipTypes_Generator;
							this_comp.CurOpSchemeType = DemandOpSchemeType;
						} else if ( SameString( this_comp_type, "Generator:FuelCell:StackCooler" ) ) {
							this_comp.TypeOf_Num = TypeOf_Generator_FCStackCooler;
							this_comp.GeneralEquipType = GenEquipTypes_Generator;
							this_comp.CurOpSchemeType = DemandOpSchemeType;
						} else if ( SameString( this_comp_type, "Fluidcooler:SingleSpeed" ) ) {
							this_comp.TypeOf_Num = TypeOf_FluidCooler_SingleSpd;
							this_comp.GeneralEquipType = GenEquipTypes_FluidCooler;
						} else if ( SameString( this_comp_type, "Fluidcooler:TwoSpeed" ) ) {
							this_comp.TypeOf_Num = TypeOf_FluidCooler_TwoSpd;
							this_comp.GeneralEquipType = GenEquipTypes_FluidCooler;
						} else if ( SameString( this_comp_type, "EvaporativeFluidcooler:SingleSpeed" ) ) {
							this_comp.TypeOf_Num = TypeOf_EvapFluidCooler_SingleSpd;
							this_comp.GeneralEquipType = GenEquipTypes_EvapFluidCooler;
						} else if ( SameString( this_comp_type, "EvaporativeFluidcooler:TwoSpeed" ) ) {
							this_comp.TypeOf_Num = TypeOf_EvapFluidCooler_TwoSpd;
							this_comp.GeneralEquipType = GenEquipTypes_EvapFluidCooler;
						} else if ( SameString( this_comp_type, "SolarCollector:FlatPlate:PhotovoltaicThermal" ) ) {
							this_comp.TypeOf_Num = TypeOf_PVTSolarCollectorFlatPlate;
							this_comp.GeneralEquipType = GenEquipTypes_SolarCollector;
							if ( LoopSideNum == DemandSide ) {
								this_comp.CurOpSchemeType = DemandOpSchemeType;
							} else if ( LoopSideNum == SupplySide ) {
								this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
							}
						} else if ( SameString( this_comp_type, "CentralHeatPumpSystem" ) ) {
							this_comp.TypeOf_Num = TypeOf_CentralGroundSourceHeatPump;
							this_comp.GeneralEquipType = GenEquipTypes_CentralHeatPumpSystem;

							//now deal with demand components of the ZoneHVAC type served by ControlCompOutput
						} else if ( SameString( this_comp_type, "ZoneHVAC:Baseboard:RadiantConvective:Water" ) ) {
							this_comp.TypeOf_Num = TypeOf_Baseboard_Rad_Conv_Water;
							this_comp.GeneralEquipType = GenEquipTypes_ZoneHVACDemand;
							this_comp.CurOpSchemeType = DemandOpSchemeType;
						} else if ( SameString( this_comp_type, "ZoneHVAC:Baseboard:Convective:Water" ) ) {
							this_comp.TypeOf_Num = TypeOf_Baseboard_Conv_Water;
							this_comp.GeneralEquipType = GenEquipTypes_ZoneHVACDemand;
							this_comp.CurOpSchemeType = DemandOpSchemeType;
						} else if ( SameString( this_comp_type, "ZoneHVAC:Baseboard:RadiantConvective:Steam" ) ) {
							this_comp.TypeOf_Num = TypeOf_Baseboard_Rad_Conv_Steam;
							this_comp.GeneralEquipType = GenEquipTypes_ZoneHVACDemand;
							this_comp.CurOpSchemeType = DemandOpSchemeType;
						} else if ( SameString( this_comp_type, "ZoneHVAC:LowTemperatureRadiant:VariableFlow" ) ) {
							this_comp.TypeOf_Num = TypeOf_LowTempRadiant_VarFlow;
							this_comp.GeneralEquipType = GenEquipTypes_ZoneHVACDemand;
							this_comp.CurOpSchemeType = DemandOpSchemeType;
						} else if ( SameString( this_comp_type, "ZoneHVAC:LowTemperatureRadiant:ConstantFlow" ) ) {
							this_comp.TypeOf_Num = TypeOf_LowTempRadiant_ConstFlow;
							this_comp.GeneralEquipType = GenEquipTypes_ZoneHVACDemand;
							this_comp.CurOpSchemeType = DemandOpSchemeType;
						} else if ( SameString( this_comp_type, "AirTerminal:SingleDuct:ConstantVolume:CooledBeam" ) ) {
							this_comp.TypeOf_Num = TypeOf_CooledBeamAirTerminal;
							this_comp.GeneralEquipType = GenEquipTypes_ZoneHVACDemand;
							this_comp.CurOpSchemeType = DemandOpSchemeType;
						} else if ( SameString( this_comp_type, "AirTerminal:SingleDuct:ConstantVolume:FourPipeBeam" ) ) {
							this_comp.TypeOf_Num = TypeOf_FourPipeBeamAirTerminal;
							this_comp.GeneralEquipType = GenEquipTypes_ZoneHVACDemand;
							this_comp.CurOpSchemeType = DemandOpSchemeType;
						} else if ( SameString( this_comp_type, "AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed" ) ) {
							this_comp.TypeOf_Num = TypeOf_MultiSpeedHeatPumpRecovery;
							this_comp.GeneralEquipType = GenEquipTypes_ZoneHVACDemand;
							this_comp.CurOpSchemeType = DemandOpSchemeType;
						} else if ( SameString( this_comp_type, "AirLoopHVAC:UnitarySystem" ) ) {
							this_comp.TypeOf_Num = TypeOf_UnitarySystemRecovery;
							this_comp.GeneralEquipType = GenEquipTypes_ZoneHVACDemand;
							this_comp.CurOpSchemeType = DemandOpSchemeType;
						} else if ( SameString( this_comp_type, "Coil:Heating:WaterToAirHeatPump:EquationFit" ) ) {
							this_comp.TypeOf_Num = TypeOf_CoilWAHPHeatingEquationFit;
							this_comp.GeneralEquipType = GenEquipTypes_DemandCoil;
							this_comp.CurOpSchemeType = DemandOpSchemeType;
						} else if ( SameString( this_comp_type, "Coil:Cooling:WaterToAirHeatPump:EquationFit" ) ) {
							this_comp.TypeOf_Num = TypeOf_CoilWAHPCoolingEquationFit;
							this_comp.GeneralEquipType = GenEquipTypes_DemandCoil;
							this_comp.CurOpSchemeType = DemandOpSchemeType;
						} else if ( SameString( this_comp_type, "Coil:Heating:WaterToAirHeatPump:VariableSpeedEquationFit" ) ) {
							this_comp.TypeOf_Num = TypeOf_CoilVSWAHPHeatingEquationFit;
							this_comp.GeneralEquipType = GenEquipTypes_DemandCoil;
							this_comp.CurOpSchemeType = DemandOpSchemeType;
						} else if ( SameString( this_comp_type, "Coil:Cooling:WaterToAirHeatPump:VariableSpeedEquationFit" ) ) {
							this_comp.TypeOf_Num = TypeOf_CoilVSWAHPCoolingEquationFit;
							this_comp.GeneralEquipType = GenEquipTypes_DemandCoil;
							this_comp.CurOpSchemeType = DemandOpSchemeType;
						} else if ( SameString( this_comp_type, "Coil:Heating:WaterToAirHeatPump:ParameterEstimation" ) ) {
							this_comp.TypeOf_Num = TypeOf_CoilWAHPHeatingParamEst;
							this_comp.GeneralEquipType = GenEquipTypes_DemandCoil;
							this_comp.CurOpSchemeType = DemandOpSchemeType;
						} else if ( SameString( this_comp_type, "Coil:Cooling:WaterToAirHeatPump:ParameterEstimation" ) ) {
							this_comp.TypeOf_Num = TypeOf_CoilWAHPCoolingParamEst;
							this_comp.GeneralEquipType = GenEquipTypes_DemandCoil;
							this_comp.CurOpSchemeType = DemandOpSchemeType;
						} else if ( SameString( this_comp_type, "Refrigeration:Condenser:WaterCooled" ) ) {
							this_comp.TypeOf_Num = TypeOf_RefrigSystemWaterCondenser;
							this_comp.GeneralEquipType = GenEquipTypes_Refrigeration;
							this_comp.CurOpSchemeType = DemandOpSchemeType;
						} else if ( SameString( this_comp_type, "Refrigeration:CompressorRack" ) ) {
							this_comp.TypeOf_Num = TypeOf_RefrigerationWaterCoolRack;
							this_comp.GeneralEquipType = GenEquipTypes_Refrigeration;
							this_comp.CurOpSchemeType = DemandOpSchemeType;
						} else if ( SameString( this_comp_type, "PlantComponent:UserDefined" ) ) {
							this_comp.TypeOf_Num = TypeOf_PlantComponentUserDefined;
							this_comp.GeneralEquipType = GenEquipTypes_PlantComponent;
							this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
						} else if ( SameString( this_comp_type, "Coil:UserDefined" ) ) {
							this_comp.TypeOf_Num = TypeOf_CoilUserDefined;
							this_comp.GeneralEquipType = GenEquipTypes_PlantComponent;
							this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
						} else if ( SameString( this_comp_type, "ZoneHVAC:ForcedAir:UserDefined" ) ) {
							this_comp.TypeOf_Num = TypeOf_ZoneHVACAirUserDefined;
							this_comp.GeneralEquipType = GenEquipTypes_PlantComponent;
							this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
						} else if ( SameString( this_comp_type, "AirTerminal:SingleDuct:UserDefined" ) ) {
							this_comp.TypeOf_Num = TypeOf_AirTerminalUserDefined;
							this_comp.GeneralEquipType = GenEquipTypes_PlantComponent;
							this_comp.CurOpSchemeType = UnknownStatusOpSchemeType;
						} else if ( SameString( this_comp_type, "PlantComponent:TemperatureSource" ) ) {
							this_comp.TypeOf_Num = TypeOf_WaterSource;
							this_comp.GeneralEquipType = GenEquipTypes_PlantComponent;
							this_comp.CurOpSchemeType = UncontrolledOpSchemeType;
						} else if ( SameString( this_comp_type, "GroundHeatExchanger:HorizontalTrench" ) ) {
							this_comp.TypeOf_Num = TypeOf_GrndHtExchgHorizTrench;
							this_comp.GeneralEquipType = GenEquipTypes_Pipe;
							this_comp.CurOpSchemeType = TypeOf_GrndHtExchgHorizTrench;
						} else if ( SameString( this_comp_type, "Coil:Cooling:DX:SingleSpeed:ThermalStorage" ) ) {
							this_comp.TypeOf_Num = TypeOf_PackagedTESCoolingCoil;
							this_comp.GeneralEquipType = GenEquipTypes_DemandCoil;
							this_comp.CurOpSchemeType = DemandOpSchemeType;
						} else if ( SameString( this_comp_type, "SwimmingPool:Indoor" ) ) {
							this_comp.TypeOf_Num = TypeOf_SwimmingPool_Indoor;
							this_comp.GeneralEquipType = GenEquipTypes_ZoneHVACDemand;
							this_comp.CurOpSchemeType = DemandOpSchemeType;
						} else {
							//discover unsupported equipment on branches.
							ShowSevereError( "GetPlantInput: Branch=\"" + BranchNames( BranchNum ) + "\", invalid component on branch." );
							ShowContinueError( "...invalid component type=\"" + this_comp_type + "\", name=\"" + CompNames( CompNum ) + "\"." );
							//            ErrorsFound=.TRUE.
						}

						this_comp.Name = CompNames( CompNum );
						this_comp.NodeNameIn = InletNodeNames( CompNum );
						this_comp.NodeNumIn = InletNodeNumbers( CompNum );
						this_comp.NodeNameOut = OutletNodeNames( CompNum );
						this_comp.NodeNumOut = OutletNodeNumbers( CompNum );

						// Increment pipe counter if component is a pipe
						if ( this_comp.TypeOf_Num == TypeOf_Pipe || this_comp.TypeOf_Num == TypeOf_PipeInterior || this_comp.TypeOf_Num == TypeOf_PipeExterior || this_comp.TypeOf_Num == TypeOf_PipeUnderground || this_comp.TypeOf_Num == TypeOf_PipeSteam ) {
							++NumOfPipesInLoop;
							if ( PlantLoop( LoopNum ).TypeOfLoop == Plant ) {
								++NumPlantPipes;
							} else if ( PlantLoop( LoopNum ).TypeOfLoop == Condenser ) {
								++NumCondPipes;
							}
							++NumPipes;
						}

						TempLoop.Branch( BranchNum ).NodeNumIn = TempLoop.Branch( BranchNum ).Comp( 1 ).NodeNumIn;

						// find branch outlet node
						TempLoop.Branch( BranchNum ).NodeNumOut = TempLoop.Branch( BranchNum ).Comp( TempLoop.Branch( BranchNum ).TotalComponents ).NodeNumOut;

					}

					CompTypes.deallocate();
					CompNames.deallocate();
					CompCtrls.deallocate();
					InletNodeNames.deallocate();
					InletNodeNumbers.deallocate();
					OutletNodeNames.deallocate();
					OutletNodeNumbers.deallocate();

				}

				BranchNames.deallocate();

				if ( ASeriesBranchHasPump && AParallelBranchHasPump ) {
					ShowSevereError( "Current version does not support Loop pumps and branch pumps together" );
					ShowContinueError( "Occurs in loop " + TempLoop.Name );
					ErrorsFound = true;
				}

				// Obtain the Splitter and Mixer information
				if ( TempLoop.ConnectList == "" ) {
					NumofSplitters = 0;
					NumofMixers = 0;
				} else {
					errFlag = false;
					GetNumSplitterMixerInConntrList( TempLoop.Name, TempLoop.ConnectList, NumofSplitters, NumofMixers, errFlag );
					if ( errFlag ) {
						ErrorsFound = true;
					}
					if ( NumofSplitters != NumofMixers ) {
						ShowSevereError( "GetPlantInput: Loop Name=" + TempLoop.Name + ", ConnectorList=" + TempLoop.ConnectList + ", unequal number of splitters and mixers" );
						ErrorsFound = true;
					}
				}

				if ( NumofSplitters > 0 ) {
					TempLoop.SplitterExists = true;
				} else {
					TempLoop.SplitterExists = false;
				}

				if ( NumofMixers > 0 ) {
					TempLoop.MixerExists = true;
				} else {
					TempLoop.MixerExists = false;
				}

				if ( ErrorsFound ) {
					ShowFatalError( "GetPlantInput: Previous Severe errors cause termination." );
				}

				NumConnectorsInLoop = NumofSplitters + NumofMixers;
				TempLoop.Splitter.allocate( NumofSplitters );
				SplitNum = 1;
				for ( ConnNum = 1; ConnNum <= NumConnectorsInLoop; ++ConnNum ) {

					if ( SplitNum > NumofSplitters ) break;
					OutletNodeNames.allocate( MaxNumAlphas );
					OutletNodeNumbers.allocate( MaxNumAlphas );
					GetLoopSplitter( TempLoop.Name, TempLoop.ConnectList, TempLoop.Splitter( SplitNum ).Name, TempLoop.Splitter( SplitNum ).Exists, TempLoop.Splitter( SplitNum ).NodeNameIn, TempLoop.Splitter( SplitNum ).NodeNumIn, TempLoop.Splitter( SplitNum ).TotalOutletNodes, OutletNodeNames, OutletNodeNumbers, ErrorsFound, ConnNum, SplitNum );

					if ( SplitNum == 1 ) {
						OutletNodeNames.deallocate();
						OutletNodeNumbers.deallocate();
						continue;
					}

					// Map the inlet node to the splitter to a branch number
					if ( TempLoop.Splitter( SplitNum - 1 ).Exists ) {
						// Map the inlet node to the splitter to a branch number
						SplitInBranch = false;
						for ( BranchNum = 1; BranchNum <= TempLoop.TotalBranches; ++BranchNum ) {
							CompNum = TempLoop.Branch( BranchNum ).TotalComponents;
							if ( TempLoop.Splitter( SplitNum - 1 ).NodeNumIn == TempLoop.Branch( BranchNum ).Comp( CompNum ).NodeNumOut ) {
								TempLoop.Splitter( SplitNum - 1 ).BranchNumIn = BranchNum;
								SplitInBranch = true;
								break; // BranchNum DO loop
							}
						}
						if ( ! SplitInBranch ) {
							ShowSevereError( "Splitter Inlet Branch not found, Splitter=" + TempLoop.Splitter( SplitNum - 1 ).Name );
							ShowContinueError( "Splitter Branch Inlet name=" + TempLoop.Splitter( SplitNum - 1 ).NodeNameIn );
							ShowContinueError( "In Loop=" + TempLoop.Name );
							ErrorsFound = true;
						}

						TempLoop.Splitter( SplitNum - 1 ).NodeNameOut.allocate( TempLoop.Splitter( SplitNum - 1 ).TotalOutletNodes );
						TempLoop.Splitter( SplitNum - 1 ).NodeNumOut.dimension( TempLoop.Splitter( SplitNum - 1 ).TotalOutletNodes, 0 );
						TempLoop.Splitter( SplitNum - 1 ).BranchNumOut.dimension( TempLoop.Splitter( SplitNum - 1 ).TotalOutletNodes, 0 );

						SplitOutBranch.allocate( TempLoop.Splitter( SplitNum - 1 ).TotalOutletNodes );
						SplitOutBranch = false;
						for ( NodeNum = 1; NodeNum <= TempLoop.Splitter( SplitNum - 1 ).TotalOutletNodes; ++NodeNum ) {
							TempLoop.Splitter( SplitNum - 1 ).NodeNameOut( NodeNum ) = OutletNodeNames( NodeNum );
							TempLoop.Splitter( SplitNum - 1 ).NodeNumOut( NodeNum ) = OutletNodeNumbers( NodeNum );
							// The following DO loop series is intended to store the branch number for each outlet
							// branch of the splitter
							for ( BranchNum = 1; BranchNum <= TempLoop.TotalBranches; ++BranchNum ) {
								if ( TempLoop.Splitter( SplitNum - 1 ).NodeNumOut( NodeNum ) == TempLoop.Branch( BranchNum ).Comp( 1 ).NodeNumIn ) {
									TempLoop.Splitter( SplitNum - 1 ).BranchNumOut( NodeNum ) = BranchNum;
									SplitOutBranch( NodeNum ) = true;
									break; // BranchNum DO loop
								}
							}
						}

						for ( Outlet = 1; Outlet <= TempLoop.Splitter( SplitNum - 1 ).TotalOutletNodes; ++Outlet ) {
							if ( SplitOutBranch( Outlet ) ) continue;
							ShowSevereError( "Splitter Outlet Branch not found, Splitter=" + TempLoop.Splitter( SplitNum - 1 ).Name );
							ShowContinueError( "Splitter Branch Outlet node name=" + TempLoop.Splitter( SplitNum - 1 ).NodeNameOut( Outlet ) );
							ShowContinueError( "In Loop=" + TempLoop.Name );
							ShowContinueError( "Loop BranchList=" + TempLoop.BranchList );
							ShowContinueError( "Loop ConnectorList=" + TempLoop.ConnectList );
							ErrorsFound = true;
						}

						SplitOutBranch.deallocate();

					} // Splitter exists
					OutletNodeNames.deallocate();
					OutletNodeNumbers.deallocate();
				}

				TempLoop.Mixer.allocate( NumofMixers );
				MixNum = 1;
				for ( ConnNum = 1; ConnNum <= NumConnectorsInLoop; ++ConnNum ) {

					if ( MixNum > NumofMixers ) break;
					InletNodeNames.allocate( MaxNumAlphas );
					InletNodeNumbers.allocate( MaxNumAlphas );
					GetLoopMixer( TempLoop.Name, TempLoop.ConnectList, TempLoop.Mixer( MixNum ).Name, TempLoop.Mixer( MixNum ).Exists, TempLoop.Mixer( MixNum ).NodeNameOut, TempLoop.Mixer( MixNum ).NodeNumOut, TempLoop.Mixer( MixNum ).TotalInletNodes, InletNodeNames, InletNodeNumbers, ErrorsFound, ConnNum, MixNum );

					if ( MixNum == 1 ) {
						InletNodeNames.deallocate();
						InletNodeNumbers.deallocate();
						continue;
					}
					// Map the outlet node of the mixer to a branch number
					if ( TempLoop.Mixer( MixNum - 1 ).Exists ) {
						// Map the outlet node of the mixer to a branch number
						MixerOutBranch = false;
						for ( BranchNum = 1; BranchNum <= TempLoop.TotalBranches; ++BranchNum ) {
							if ( TempLoop.Mixer( MixNum - 1 ).NodeNumOut == TempLoop.Branch( BranchNum ).Comp( 1 ).NodeNumIn ) {
								TempLoop.Mixer( MixNum - 1 ).BranchNumOut = BranchNum;
								MixerOutBranch = true;
								break; // BranchNum DO loop
							}
						}
						if ( ! MixerOutBranch ) {
							ShowSevereError( "Mixer Outlet Branch not found, Mixer=" + TempLoop.Mixer( MixNum - 1 ).Name );
							ErrorsFound = true;
						}

						TempLoop.Mixer( MixNum - 1 ).NodeNameIn.allocate( TempLoop.Mixer( MixNum - 1 ).TotalInletNodes );
						TempLoop.Mixer( MixNum - 1 ).NodeNumIn.dimension( TempLoop.Mixer( MixNum - 1 ).TotalInletNodes, 0 );
						TempLoop.Mixer( MixNum - 1 ).BranchNumIn.dimension( TempLoop.Mixer( MixNum - 1 ).TotalInletNodes, 0 );

						MixerInBranch.allocate( TempLoop.Mixer( MixNum - 1 ).TotalInletNodes );
						MixerInBranch = false;
						for ( NodeNum = 1; NodeNum <= TempLoop.Mixer( MixNum - 1 ).TotalInletNodes; ++NodeNum ) {
							TempLoop.Mixer( MixNum - 1 ).NodeNameIn( NodeNum ) = InletNodeNames( NodeNum );
							TempLoop.Mixer( MixNum - 1 ).NodeNumIn( NodeNum ) = InletNodeNumbers( NodeNum );
							// The following DO loop series is intended to store the branch number for each inlet
							// branch of the mixer
							for ( BranchNum = 1; BranchNum <= TempLoop.TotalBranches; ++BranchNum ) {
								CompNum = TempLoop.Branch( BranchNum ).TotalComponents;
								if ( TempLoop.Mixer( MixNum - 1 ).NodeNumIn( NodeNum ) == TempLoop.Branch( BranchNum ).Comp( CompNum ).NodeNumOut ) {
									TempLoop.Mixer( MixNum - 1 ).BranchNumIn( NodeNum ) = BranchNum;
									MixerInBranch( NodeNum ) = true;
									break; // BranchNum DO loop
								}
							}
						}

						for ( Inlet = 1; Inlet <= TempLoop.Mixer( MixNum - 1 ).TotalInletNodes; ++Inlet ) {
							if ( MixerInBranch( Inlet ) ) continue;
							ShowSevereError( "Mixer Inlet Branch not found, Mixer=" + TempLoop.Mixer( MixNum - 1 ).Name );
							ShowContinueError( "Mixer Branch Inlet name=" + TempLoop.Mixer( MixNum - 1 ).NodeNameIn( Inlet ) );
							ShowContinueError( "In Loop=" + TempLoop.Name );
							ShowContinueError( "Loop BranchList=" + TempLoop.BranchList );
							ShowContinueError( "Loop ConnectorList=" + TempLoop.ConnectList );
							ErrorsFound = true;
						}

						MixerInBranch.deallocate();
					} // Mixer exists
					InletNodeNames.deallocate();
					InletNodeNumbers.deallocate();
				}

				if ( NumOfPipesInLoop > 0 ) {
					PipeNum = 0;
					LoopPipe( HalfLoopNum ).Pipe.allocate( NumOfPipesInLoop );
					for ( BranchNum = 1; BranchNum <= TempLoop.TotalBranches; ++BranchNum ) {
						for ( CompNum = 1; CompNum <= TempLoop.Branch( BranchNum ).TotalComponents; ++CompNum ) {
							if ( TempLoop.Branch( BranchNum ).Comp( CompNum ).TypeOf_Num == TypeOf_Pipe || TempLoop.Branch( BranchNum ).Comp( CompNum ).TypeOf_Num == TypeOf_PipeSteam || TempLoop.Branch( BranchNum ).Comp( CompNum ).TypeOf_Num == TypeOf_PipeInterior || TempLoop.Branch( BranchNum ).Comp( CompNum ).TypeOf_Num == TypeOf_PipeUnderground || TempLoop.Branch( BranchNum ).Comp( CompNum ).TypeOf_Num == TypeOf_PipeExterior ) {

								++PipeNum;
								if ( PipeNum > NumOfPipesInLoop ) ShowFatalError( "Pipe counting problem in GetPlantSideLoops" );

								LoopPipe( HalfLoopNum ).NumPipes = NumOfPipesInLoop;
								LoopPipe( HalfLoopNum ).Pipe( PipeNum ).Name = TempLoop.Branch( BranchNum ).Comp( CompNum ).Name;
								if ( TempLoop.Branch( BranchNum ).Comp( CompNum ).TypeOf_Num == TypeOf_Pipe ) {
									LoopPipe( HalfLoopNum ).Pipe( PipeNum ).TypeOf = TypeOf_Pipe;
								} else if ( TempLoop.Branch( BranchNum ).Comp( CompNum ).TypeOf_Num == TypeOf_PipeSteam ) {
									LoopPipe( HalfLoopNum ).Pipe( PipeNum ).TypeOf = TypeOf_PipeSteam;
								} else if ( TempLoop.Branch( BranchNum ).Comp( CompNum ).TypeOf_Num == TypeOf_PipeInterior ) {
									LoopPipe( HalfLoopNum ).Pipe( PipeNum ).TypeOf = TypeOf_PipeInterior;
								} else if ( TempLoop.Branch( BranchNum ).Comp( CompNum ).TypeOf_Num == TypeOf_PipeExterior ) {
									LoopPipe( HalfLoopNum ).Pipe( PipeNum ).TypeOf = TypeOf_PipeExterior;
								} else if ( TempLoop.Branch( BranchNum ).Comp( CompNum ).TypeOf_Num == TypeOf_PipeUnderground ) {
									LoopPipe( HalfLoopNum ).Pipe( PipeNum ).TypeOf = TypeOf_PipeUnderground;
								}
								LoopPipe( HalfLoopNum ).Pipe( PipeNum ).NodeNameIn = TempLoop.Branch( BranchNum ).Comp( CompNum ).NodeNameIn;
								LoopPipe( HalfLoopNum ).Pipe( PipeNum ).NodeNumIn = TempLoop.Branch( BranchNum ).Comp( CompNum ).NodeNumIn;
								LoopPipe( HalfLoopNum ).Pipe( PipeNum ).NodeNameOut = TempLoop.Branch( BranchNum ).Comp( CompNum ).NodeNameOut;
								LoopPipe( HalfLoopNum ).Pipe( PipeNum ).NodeNumOut = TempLoop.Branch( BranchNum ).Comp( CompNum ).NodeNumOut;

								if ( TempLoop.Branch( BranchNum ).Comp( CompNum ).TypeOf_Num == TypeOf_Pipe || TempLoop.Branch( BranchNum ).Comp( CompNum ).TypeOf_Num == TypeOf_PipeSteam ) {
									//                Call InitializePipes(TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_Num,  &
									//                            LoopPipe(HalfLoopNum)%Pipe(PipeNum)%Name,  &
									//                            TempLoop%Branch(BranchNum)%Comp(CompNum)%CompNum, &
									//                            0.0d0)
								} else if ( TempLoop.Branch( BranchNum ).Comp( CompNum ).TypeOf_Num == TypeOf_PipeInterior || TempLoop.Branch( BranchNum ).Comp( CompNum ).TypeOf_Num == TypeOf_PipeUnderground || TempLoop.Branch( BranchNum ).Comp( CompNum ).TypeOf_Num == TypeOf_PipeExterior ) {
									//InitializeHeatTransferPipes( TempLoop.Branch( BranchNum ).Comp( CompNum ).TypeOf_Num, LoopPipe( HalfLoopNum ).Pipe( PipeNum ).Name, TempLoop.Branch( BranchNum ).Comp( CompNum ).CompNum );
								}
							}
						}
					}
				}

				PlantLoop( LoopNum ).LoopSide( LoopSideNum ).SplitterExists = TempLoop.SplitterExists;
				PlantLoop( LoopNum ).LoopSide( LoopSideNum ).MixerExists = TempLoop.MixerExists;
				PlantLoop( LoopNum ).LoopSide( LoopSideNum ).BypassExists = TempLoop.BypassExists;

				PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch.allocate( TempLoop.TotalBranches );
				PlantLoop( LoopNum ).LoopSide( LoopSideNum ).TotalBranches = TempLoop.TotalBranches;
				PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch = TempLoop.Branch;

				PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Splitter.allocate( NumofSplitters );
				PlantLoop( LoopNum ).LoopSide( LoopSideNum ).NumSplitters = NumofSplitters;
				PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Splitter = TempLoop.Splitter;

				PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Mixer.allocate( NumofMixers );
				PlantLoop( LoopNum ).LoopSide( LoopSideNum ).NumMixers = NumofMixers;
				PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Mixer = TempLoop.Mixer;

				//   Add condenser CASE statement when required.

				TempLoop.Branch.deallocate();
				TempLoop.Splitter.deallocate();
				TempLoop.Mixer.deallocate();

			} // ... end LoopSideNum=DemandSide,SupplySide

			PlantLoop( LoopNum ).LoopHasConnectionComp = TempLoop.LoopHasConnectionComp;

			// CR 7883 check for missing demand side pump if common pipe set.
			if ( ( PlantLoop( LoopNum ).CommonPipeType != CommonPipe_No ) && ( ! DemandSideHasPump ) ) {
				ShowSevereError( "Input Error: a common pipe arrangement was selected but there is no pump for the secondary loop." );
				ShowContinueError( "Occurs in PlantLoop = " + TempLoop.Name );
				ShowContinueError( "Add a pump to the demand side of this plant loop." );
				ErrorsFound = true;
			}

		} // ...end of demand side loops DO loop

		Pipe.allocate( NumPipes ); // Pipe definition in DataPlant
		SysPipeNum = 0;

		for ( HalfLoopNum = 1; HalfLoopNum <= NumHalfLoops; ++HalfLoopNum ) {
			for ( PipeNum = 1; PipeNum <= LoopPipe( HalfLoopNum ).NumPipes; ++PipeNum ) {
				++SysPipeNum;
				if ( mod( HalfLoopNum, 2 ) != 0 ) {
					Pipe( SysPipeNum ).ParentHalfLoop = DemandSide;
				} else {
					Pipe( SysPipeNum ).ParentHalfLoop = SupplySide;
				}
				Pipe( SysPipeNum ).Name = LoopPipe( HalfLoopNum ).Pipe( PipeNum ).Name;
				Pipe( SysPipeNum ).TypeOf = LoopPipe( HalfLoopNum ).Pipe( PipeNum ).TypeOf;
				Pipe( SysPipeNum ).NodeNameIn = LoopPipe( HalfLoopNum ).Pipe( PipeNum ).NodeNameIn;
				Pipe( SysPipeNum ).NodeNumIn = LoopPipe( HalfLoopNum ).Pipe( PipeNum ).NodeNumIn;
				Pipe( SysPipeNum ).NodeNameOut = LoopPipe( HalfLoopNum ).Pipe( PipeNum ).NodeNameOut;
				Pipe( SysPipeNum ).NodeNumOut = LoopPipe( HalfLoopNum ).Pipe( PipeNum ).NodeNumOut;
			}
		}

		LoopPipe.deallocate();

		//DSU? can we clean this out this next do loop now? looks like bandaids.
		for ( LoopNum = 1; LoopNum <= TotNumLoops; ++LoopNum ) {
			auto & this_supply_side( PlantLoop( LoopNum ).LoopSide( SupplySide ) );
			for ( BranchNum = 1; BranchNum <= this_supply_side.TotalBranches; ++BranchNum ) {
				auto & this_branch( this_supply_side.Branch( BranchNum ) );
				for ( CompNum = 1; CompNum <= this_branch.TotalComponents; ++CompNum ) {
					auto & this_comp( this_branch.Comp( CompNum ) );
					Pos = index( this_comp.TypeOf, ':' );
					if ( Pos != std::string::npos ) {
						GeneralEquipType = FindItemInList( this_comp.TypeOf.substr( 0, Pos ), GeneralEquipTypes, NumGeneralEquipTypes );
					} else {
						GeneralEquipType = 0;
					}
					if ( GeneralEquipType == 0 ) {
						if ( has_prefixi( this_comp.TypeOf, "HeaderedPumps" ) ) {
							GeneralEquipType = GenEquipTypes_Pump;
						} else if ( has_prefixi( this_comp.TypeOf, "WaterHeater:HeatPump" ) ) {
							GeneralEquipType = GenEquipTypes_WaterThermalTank;
						} else if ( SameString( this_comp.TypeOf, "TemperingValve" ) ) {
							GeneralEquipType = GenEquipTypes_Valve;
						} else if ( has_prefixi( this_comp.TypeOf, "Pipe:Adiabatic" ) ) {
							GeneralEquipType = GenEquipTypes_Pipe;
						} else if ( has_prefixi( this_comp.TypeOf, "PipingSystem" ) ) {
							GeneralEquipType = GenEquipTypes_Pipe;
						} else if ( has_prefixi( this_comp.TypeOf, "Thermalstorage:ChilledWater:Mixed" ) ) {
							GeneralEquipType = GenEquipTypes_ThermalStorage;
						} else if ( has_prefixi( this_comp.TypeOf, "Thermalstorage:ChilledWater:Stratified" ) ) {
							GeneralEquipType = GenEquipTypes_ThermalStorage;
						} else if ( SameString( this_comp.TypeOf, "ChillerHeater:Absorption:DirectFired" ) ) {
							GeneralEquipType = GenEquipTypes_Chiller;
						} else if ( SameString( this_comp.TypeOf, "ChillerHeater:Absorption:DoubleEffect" ) ) {
							GeneralEquipType = GenEquipTypes_Chiller;
						} else if ( has_prefixi( this_comp.TypeOf, "District" ) ) {
							GeneralEquipType = GenEquipTypes_Purchased;
						} else if ( SameString( this_comp.TypeOf, "GroundHeatExchanger:Vertical" ) ) {
							GeneralEquipType = GenEquipTypes_GroundHeatExchanger;
						} else if ( SameString( this_comp.TypeOf, "GroundHeatExchanger:Surface" ) ) {
							GeneralEquipType = GenEquipTypes_GroundHeatExchanger;
						} else if ( SameString( this_comp.TypeOf, "GroundHeatExchanger:Pond" ) ) {
							GeneralEquipType = GenEquipTypes_GroundHeatExchanger;
						} else if ( SameString( this_comp.TypeOf, "GroundHeatExchanger:Slinky" ) ) {
							GeneralEquipType = GenEquipTypes_GroundHeatExchanger;
						} else if ( SameString( this_comp.TypeOf, "PlantComponent:TemperatureSource" ) ) {
							GeneralEquipType = GenEquipTypes_HeatExchanger;
						} else if ( SameString( this_comp.TypeOf, "CENTRALHEATPUMPSYSTEM" ) ) {
							GeneralEquipType = GenEquipTypes_CentralHeatPumpSystem;
						} else {
							ShowSevereError( "GetPlantInput: PlantLoop=\"" + PlantLoop( LoopNum ).Name + "\" invalid equipment type." );
							ShowContinueError( "...on Branch=\"" + PlantLoop( LoopNum ).LoopSide( SupplySide ).Branch( BranchNum ).Name + "\"." );
							ShowContinueError( "...Equipment type=\"" + this_comp.TypeOf + "\"." );
							ShowContinueError( "...Equipment name=\"" + this_comp.Name + "\"." );
							ErrorsFound = true;
						}
					}

					this_comp.GeneralEquipType = GeneralEquipType;

					// Set up "TypeOf" Num
					TypeOfNum = FindItemInList( this_comp.TypeOf, SimPlantEquipTypes, NumSimPlantEquipTypes );
					if ( TypeOfNum == 0 ) {
						if ( SameString(this_comp.TypeOf, "WaterHeater:HeatPump:PumpedCondenser") ) {
							this_comp.TypeOf_Num = TypeOf_HeatPumpWtrHeaterPumped;
						} else if ( SameString(this_comp.TypeOf, "WaterHeater:HeatPump:WrappedCondenser")) {
							this_comp.TypeOf_Num = TypeOf_HeatPumpWtrHeaterWrapped;
						} else if ( ! has_prefixi( this_comp.TypeOf, "Pump" ) && ! has_prefixi( this_comp.TypeOf, "HeaderedPump" ) ) {
							// Error.  May have already been flagged under General
							if ( GeneralEquipType != 0 ) { // if GeneralEquipmentType == 0, then already flagged
								ShowSevereError( "GetPlantInput: PlantLoop=\"" + PlantLoop( LoopNum ).Name + "\" invalid equipment type." );
								ShowContinueError( "...on Branch=\"" + PlantLoop( LoopNum ).LoopSide( SupplySide ).Branch( BranchNum ).Name + "\"." );
								ShowContinueError( "...Equipment type=\"" + this_comp.TypeOf + "\"." );
								ShowContinueError( "...Equipment name=\"" + this_comp.Name + "\"." );
								ErrorsFound = true;
							}
						}
					} else {
						this_comp.TypeOf_Num = TypeOfNum;
					}

				}
			}
		}

		if ( ErrorsFound ) {
			ShowFatalError( "GetPlantInput: Errors in getting PlantLoop Input" );
		}

		if ( NumPlantLoops > 0 ) VentRepPlantSupplySide.allocate( NumPlantLoops );
		if ( NumPlantLoops > 0 ) VentRepPlantDemandSide.allocate( NumPlantLoops );

		for ( LoopNum = 1; LoopNum <= NumPlantLoops; ++LoopNum ) {

			// set up references for this loop
			auto & this_plant_loop( PlantLoop( LoopNum ) );
			auto & this_plant_supply ( this_plant_loop.LoopSide( SupplySide ) );
			auto & this_vent_plant_supply( VentRepPlantSupplySide( LoopNum ) );
			auto & this_plant_demand ( this_plant_loop.LoopSide( DemandSide ) );
			auto & this_vent_plant_demand( VentRepPlantDemandSide( LoopNum ) );

			this_vent_plant_supply.Name = this_plant_loop.Name;
			this_vent_plant_supply.NodeNumIn = this_plant_supply.NodeNumIn;
			this_vent_plant_supply.NodeNameIn = this_plant_supply.NodeNameIn;
			this_vent_plant_supply.NodeNumOut = this_plant_supply.NodeNumOut;
			this_vent_plant_supply.NodeNameOut = this_plant_supply.NodeNameOut;
			this_vent_plant_supply.TotalBranches = this_plant_supply.TotalBranches;

			if ( this_vent_plant_supply.TotalBranches > 0 ) this_vent_plant_supply.Branch.allocate( this_vent_plant_supply.TotalBranches );

			for ( BranchNum = 1; BranchNum <= this_vent_plant_supply.TotalBranches; ++BranchNum ) {

				auto & this_plant_supply_branch( PlantLoop( LoopNum ).LoopSide( SupplySide ).Branch( BranchNum ) );
				auto & this_vent_plant_supply_branch( VentRepPlantSupplySide( LoopNum ).Branch( BranchNum ) );

				this_vent_plant_supply_branch.Name = this_plant_supply_branch.Name;
				this_vent_plant_supply_branch.NodeNumIn = this_plant_supply_branch.NodeNumIn;
				this_vent_plant_supply_branch.NodeNumOut = this_plant_supply_branch.NodeNumOut;
				this_vent_plant_supply_branch.TotalComponents = this_plant_supply_branch.TotalComponents;
				if ( this_vent_plant_supply_branch.TotalComponents > 0 ) {
					TotCompsOnBranch = this_vent_plant_supply_branch.TotalComponents;
					this_vent_plant_supply_branch.Comp.allocate( TotCompsOnBranch );
				}

				for ( CompNum = 1; CompNum <= VentRepPlantSupplySide( LoopNum ).Branch( BranchNum ).TotalComponents; ++CompNum ) {

					auto & this_plant_supply_comp( PlantLoop( LoopNum ).LoopSide( SupplySide ).Branch( BranchNum ).Comp( CompNum ) );
					auto & this_vent_plant_supply_comp( VentRepPlantSupplySide( LoopNum ).Branch( BranchNum ).Comp( CompNum ) );

					this_vent_plant_supply_comp.Name = this_plant_supply_comp.Name;
					this_vent_plant_supply_comp.TypeOf = this_plant_supply_comp.TypeOf;
					this_vent_plant_supply_comp.NodeNameIn = this_plant_supply_comp.NodeNameIn;
					this_vent_plant_supply_comp.NodeNameOut = this_plant_supply_comp.NodeNameOut;
					this_vent_plant_supply_comp.NodeNumIn = this_plant_supply_comp.NodeNumIn;
					this_vent_plant_supply_comp.NodeNumOut = this_plant_supply_comp.NodeNumOut;

				} // loop over components in branches on the loop (ventilation report data)

			} // loop over branches on the loop (ventilation report data)

			this_vent_plant_demand.Name = this_plant_loop.Name;
			this_vent_plant_demand.NodeNumIn = this_plant_demand.NodeNumIn;
			this_vent_plant_demand.NodeNameIn = this_plant_demand.NodeNameIn;
			this_vent_plant_demand.NodeNumOut = this_plant_demand.NodeNumOut;
			this_vent_plant_demand.NodeNameOut = this_plant_demand.NodeNameOut;
			this_vent_plant_demand.TotalBranches = this_plant_demand.TotalBranches;

			if ( this_vent_plant_demand.TotalBranches > 0 ) this_vent_plant_demand.Branch.allocate( this_vent_plant_demand.TotalBranches );

			for ( BranchNum = 1; BranchNum <= this_vent_plant_demand.TotalBranches; ++BranchNum ) {

				auto & this_plant_demand_branch( PlantLoop( LoopNum ).LoopSide( DemandSide ).Branch( BranchNum ) );
				auto & this_vent_plant_demand_branch( VentRepPlantDemandSide( LoopNum ).Branch( BranchNum ) );

				this_vent_plant_demand_branch.Name = this_plant_demand_branch.Name;
				this_vent_plant_demand_branch.NodeNumIn = this_plant_demand_branch.NodeNumIn;
				this_vent_plant_demand_branch.NodeNumOut = this_plant_demand_branch.NodeNumOut;
				this_vent_plant_demand_branch.TotalComponents = this_plant_demand_branch.TotalComponents;
				if ( this_vent_plant_demand_branch.TotalComponents > 0 ) {
					TotCompsOnBranch = this_vent_plant_demand_branch.TotalComponents;
					this_vent_plant_demand_branch.Comp.allocate( TotCompsOnBranch );
				}

				for ( CompNum = 1; CompNum <= this_vent_plant_demand_branch.TotalComponents; ++CompNum ) {

					auto & this_plant_demand_comp( PlantLoop( LoopNum ).LoopSide( DemandSide ).Branch( BranchNum ).Comp( CompNum ) );
					auto & this_vent_plant_demand_comp( VentRepPlantDemandSide( LoopNum ).Branch( BranchNum ).Comp( CompNum ) );

					this_vent_plant_demand_comp.Name = this_plant_demand_comp.Name;
					this_vent_plant_demand_comp.TypeOf = this_plant_demand_comp.TypeOf;
					this_vent_plant_demand_comp.NodeNameIn = this_plant_demand_comp.NodeNameIn;
					this_vent_plant_demand_comp.NodeNameOut = this_plant_demand_comp.NodeNameOut;
					this_vent_plant_demand_comp.NodeNumIn = this_plant_demand_comp.NodeNumIn;
					this_vent_plant_demand_comp.NodeNumOut = this_plant_demand_comp.NodeNumOut;

				} // loop over components in branches on the loop (ventilation report data)

			} // loop over branches on the loop (ventilation report data)

		} // loop over plant supply loops (ventilation report data)

		if ( NumCondLoops > 0 ) VentRepCondSupplySide.allocate( NumCondLoops );
		if ( NumCondLoops > 0 ) VentRepCondDemandSide.allocate( NumCondLoops );

		for ( LoopNum = 1; LoopNum <= NumCondLoops; ++LoopNum ) {

			LoopNumInArray = LoopNum + NumPlantLoops;

			// set up references for this loop
			auto & this_cond_loop( PlantLoop( LoopNumInArray ) );
			auto & this_cond_supply ( this_cond_loop.LoopSide( SupplySide ) );
			auto & this_vent_cond_supply( VentRepCondSupplySide( LoopNum ) );
			auto & this_cond_demand ( this_cond_loop.LoopSide( DemandSide ) );
			auto & this_vent_cond_demand( VentRepCondDemandSide( LoopNum ) );

			this_vent_cond_supply.Name = this_cond_loop.Name;
			this_vent_cond_supply.NodeNumIn = this_cond_supply.NodeNumIn;
			this_vent_cond_supply.NodeNameIn = this_cond_supply.NodeNameIn;
			this_vent_cond_supply.NodeNumOut = this_cond_supply.NodeNumOut;
			this_vent_cond_supply.NodeNameOut = this_cond_supply.NodeNameOut;
			this_vent_cond_supply.TotalBranches = this_cond_supply.TotalBranches;
			if ( this_vent_cond_supply.TotalBranches > 0 ) this_vent_cond_supply.Branch.allocate( this_vent_cond_supply.TotalBranches );

			for ( BranchNum = 1; BranchNum <= this_vent_cond_supply.TotalBranches; ++BranchNum ) {

				auto & this_cond_supply_branch( this_cond_supply.Branch( BranchNum ) );
				auto & this_vent_cond_supply_branch( this_vent_cond_supply.Branch( BranchNum ) );

				this_vent_cond_supply_branch.Name = this_cond_supply_branch.Name;
				this_vent_cond_supply_branch.NodeNumIn = this_cond_supply_branch.NodeNumIn;
				this_vent_cond_supply_branch.NodeNumOut = this_cond_supply_branch.NodeNumOut;
				this_vent_cond_supply_branch.TotalComponents = this_cond_supply_branch.TotalComponents;
				if ( this_vent_cond_supply_branch.TotalComponents > 0 ) {
					TotCompsOnBranch = this_vent_cond_supply_branch.TotalComponents;
					this_vent_cond_supply_branch.Comp.allocate( TotCompsOnBranch );
				}

				for ( CompNum = 1; CompNum <= this_vent_cond_supply_branch.TotalComponents; ++CompNum ) {

					auto & this_cond_supply_comp( this_cond_loop.LoopSide( SupplySide ).Branch( BranchNum ).Comp( CompNum ) );
					auto & this_vent_cond_supply_comp( this_vent_cond_supply.Branch( BranchNum ).Comp( CompNum ) );

					this_vent_cond_supply_comp.Name = this_cond_supply_comp.Name;
					this_vent_cond_supply_comp.TypeOf = this_cond_supply_comp.TypeOf;
					this_vent_cond_supply_comp.NodeNameIn = this_cond_supply_comp.NodeNameIn;
					this_vent_cond_supply_comp.NodeNameOut = this_cond_supply_comp.NodeNameOut;
					this_vent_cond_supply_comp.NodeNumIn = this_cond_supply_comp.NodeNumIn;
					this_vent_cond_supply_comp.NodeNumOut = this_cond_supply_comp.NodeNumOut;

				} // loop over components in branches on the loop (ventilation report data)

			} // loop over branches on the loop (ventilation report data)

			this_vent_cond_demand.Name = this_cond_loop.Name;
			this_vent_cond_demand.NodeNumIn = this_cond_demand.NodeNumIn;
			this_vent_cond_demand.NodeNameIn = this_cond_demand.NodeNameIn;
			this_vent_cond_demand.NodeNumOut = this_cond_demand.NodeNumOut;
			this_vent_cond_demand.NodeNameOut = this_cond_demand.NodeNameOut;
			this_vent_cond_demand.TotalBranches = this_cond_demand.TotalBranches;
			if ( this_vent_cond_demand.TotalBranches > 0 ) this_vent_cond_demand.Branch.allocate( this_vent_cond_demand.TotalBranches );

			for ( BranchNum = 1; BranchNum <= this_vent_cond_demand.TotalBranches; ++BranchNum ) {

				auto & this_cond_demand_branch( this_cond_demand.Branch( BranchNum ) );
				auto & this_vent_cond_demand_branch( this_vent_cond_demand.Branch( BranchNum ) );

				this_vent_cond_demand_branch.Name = this_cond_demand_branch.Name;
				this_vent_cond_demand_branch.NodeNumIn = this_cond_demand_branch.NodeNumIn;
				this_vent_cond_demand_branch.NodeNumOut = this_cond_demand_branch.NodeNumOut;
				this_vent_cond_demand_branch.TotalComponents = this_cond_demand_branch.TotalComponents;
				if ( this_vent_cond_demand_branch.TotalComponents > 0 ) {
					TotCompsOnBranch = this_vent_cond_demand_branch.TotalComponents;
					this_vent_cond_demand_branch.Comp.allocate( TotCompsOnBranch );
				}

				for ( CompNum = 1; CompNum <= this_vent_cond_demand_branch.TotalComponents; ++CompNum ) {

					auto & this_cond_demand_comp( this_cond_demand_branch.Comp( CompNum ) );
					auto & this_vent_cond_demand_comp( this_vent_cond_demand_branch.Comp( CompNum ) );

					this_vent_cond_demand_comp.Name = this_cond_demand_comp.Name;
					this_vent_cond_demand_comp.TypeOf = this_cond_demand_comp.TypeOf;
					this_vent_cond_demand_comp.NodeNameIn = this_cond_demand_comp.NodeNameIn;
					this_vent_cond_demand_comp.NodeNameOut = this_cond_demand_comp.NodeNameOut;
					this_vent_cond_demand_comp.NodeNumIn = this_cond_demand_comp.NodeNumIn;
					this_vent_cond_demand_comp.NodeNumOut = this_cond_demand_comp.NodeNumOut;

				} // loop over components in branches on the loop (ventilation report data)

			} // loop over branches on the loop (ventilation report data)

		} // loop over plant supply loops (ventilation report data)

	}

	void
	SetupReports()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   July 2001
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine initializes the plant supply side reports.
		// It was created during the splitting of supply and demand side functions.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na
		// Using/Aliasing
		using DataPlant::PlantReport;
		using DataPlant::PlantLoop;
		using DataPlant::DemandSide;
		using DataPlant::SupplySide;
		using DataPlant::DemandOpSchemeType;
		using DataGlobals::DisplayAdvancedReportVariables;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int LoopNum; // DO loop counter (plant supply sides)
		int LoopSideNum;
		int BranchNum;
		int CompNum;
		int MaxBranches; // Maximum number of branches on any plant loop (used for allocating arrays)
		std::string CurrentModuleObject; // for ease in renaming.
		int FluidIndex;

		// FLOW:
		MaxBranches = 0;
		for ( LoopNum = 1; LoopNum <= TotNumLoops; ++LoopNum ) {
			MaxBranches = max( MaxBranches, PlantLoop( LoopNum ).LoopSide( DemandSide ).TotalBranches );
			MaxBranches = max( MaxBranches, PlantLoop( LoopNum ).LoopSide( SupplySide ).TotalBranches );
			PlantLoop( LoopNum ).MaxBranch = MaxBranches;
		}

		PlantReport.allocate( TotNumLoops );

		for ( auto & e : PlantReport ) {
			e.CoolingDemand = 0.0;
			e.HeatingDemand = 0.0;
			e.DemandNotDispatched = 0.0;
			e.UnmetDemand = 0.0;
			e.InletNodeTemperature = 0.0;
			e.OutletNodeTemperature = 0.0;
			e.InletNodeFlowrate = 0.0;
			e.BypassFrac = 0.0;
			e.OutletNodeFlowrate = 0.0;
		}

		for ( LoopNum = 1; LoopNum <= TotNumLoops; ++LoopNum ) {
			if ( LoopNum <= NumPlantLoops ) {
				CurrentModuleObject = "Plant Loop";
			} else {
				CurrentModuleObject = "Cond Loop";
			}
			// CurrentModuleObject='Plant/Condenser Loop'
			SetupOutputVariable( "Plant Supply Side Cooling Demand Rate [W]", PlantReport( LoopNum ).CoolingDemand, "System", "Average", PlantLoop( LoopNum ).Name );
			SetupOutputVariable( "Plant Supply Side Heating Demand Rate [W]", PlantReport( LoopNum ).HeatingDemand, "System", "Average", PlantLoop( LoopNum ).Name );
			SetupOutputVariable( "Plant Supply Side Inlet Mass Flow Rate [kg/s]", PlantReport( LoopNum ).InletNodeFlowrate, "System", "Average", PlantLoop( LoopNum ).Name );

			SetupOutputVariable( "Plant Supply Side Inlet Temperature [C]", PlantReport( LoopNum ).InletNodeTemperature, "System", "Average", PlantLoop( LoopNum ).Name );
			SetupOutputVariable( "Plant Supply Side Outlet Temperature [C]", PlantReport( LoopNum ).OutletNodeTemperature, "System", "Average", PlantLoop( LoopNum ).Name );

			SetupOutputVariable( "Plant Supply Side Not Distributed Demand Rate [W]", PlantReport( LoopNum ).DemandNotDispatched, "System", "Average", PlantLoop( LoopNum ).Name );
			SetupOutputVariable( "Plant Supply Side Unmet Demand Rate [W]", PlantReport( LoopNum ).UnmetDemand, "System", "Average", PlantLoop( LoopNum ).Name );

			// Debug variables -- used by OSU developers
			SetupOutputVariable( "Debug Plant Loop Bypass Fraction []", PlantReport( LoopNum ).BypassFrac, "System", "Average", PlantLoop( LoopNum ).Name );
			//    CALL SetupOutputVariable('Debug SSInletNode Flowrate[kg/s]', &
			//           PlantReport(LoopNum)%InletNodeFlowrate,'System','Average',PlantLoop(LoopNum)%Name)
			//    CALL SetupOutputVariable('Debug SSInletNode Temperature[C]', &
			//           PlantReport(LoopNum)%InletNodeTemperature,'System','Average',PlantLoop(LoopNum)%Name)
			//    CALL SetupOutputVariable('Debug SSOutletNode Flowrate [kg/s]', &
			//           PlantReport(LoopNum)%OutletNodeFlowrate,'System','Average',PlantLoop(LoopNum)%Name)
			//    CALL SetupOutputVariable('Debug SSOutletNode Temperature[C]', &
			//           PlantReport(LoopNum)%OutletNodeTemperature,'System','Average',PlantLoop(LoopNum)%Name)
			SetupOutputVariable( "Debug Plant Last Simulated Loop Side []", PlantReport( LoopNum ).LastLoopSideSimulated, "System", "Average", PlantLoop( LoopNum ).Name );
		}

		// setup more variables inside plant data structure
		// CurrentModuleObject='Plant/Condenser Loop(Advanced)'
		if ( DisplayAdvancedReportVariables ) {
			for ( LoopNum = 1; LoopNum <= TotNumLoops; ++LoopNum ) {
				SetupOutputVariable( "Plant Demand Side Lumped Capacitance Temperature [C]", PlantLoop( LoopNum ).LoopSide( DemandSide ).LoopSideInlet_TankTemp, "System", "Average", PlantLoop( LoopNum ).Name );
				SetupOutputVariable( "Plant Supply Side Lumped Capacitance Temperature [C]", PlantLoop( LoopNum ).LoopSide( SupplySide ).LoopSideInlet_TankTemp, "System", "Average", PlantLoop( LoopNum ).Name );
				for ( LoopSideNum = DemandSide; LoopSideNum <= SupplySide; ++LoopSideNum ) {
					for ( BranchNum = 1; BranchNum <= PlantLoop( LoopNum ).LoopSide( LoopSideNum ).TotalBranches; ++BranchNum ) {
						for ( CompNum = 1; CompNum <= PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).TotalComponents; ++CompNum ) {
							if ( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).Comp( CompNum ).CurOpSchemeType != DemandOpSchemeType ) {
								SetupOutputVariable( "Plant Component Distributed Demand Rate [W]", PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).Comp( CompNum ).MyLoad, "System", "Average", PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).Comp( CompNum ).Name );
							}
						}
					}
				}
			}
		}

		// now traverse plant loops and set fluid type index in all nodes on the loop
		for ( LoopNum = 1; LoopNum <= TotNumLoops; ++LoopNum ) {
			FluidIndex = PlantLoop( LoopNum ).FluidIndex;
			for ( LoopSideNum = DemandSide; LoopSideNum <= SupplySide; ++LoopSideNum ) {
				Node( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).NodeNumIn ).FluidIndex = FluidIndex;
				Node( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).NodeNumOut ).FluidIndex = FluidIndex;
				for ( BranchNum = 1; BranchNum <= PlantLoop( LoopNum ).LoopSide( LoopSideNum ).TotalBranches; ++BranchNum ) {
					for ( CompNum = 1; CompNum <= PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).TotalComponents; ++CompNum ) {
						Node( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).Comp( CompNum ).NodeNumIn ).FluidIndex = FluidIndex;
						Node( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).Comp( CompNum ).NodeNumOut ).FluidIndex = FluidIndex;
					}
				}
			}
		} // plant loops

	}

	void
	InitializeLoops( bool const FirstHVACIteration ) // true if first iteration of the simulation
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Sankaranarayanan K P
		//       DATE WRITTEN   May 2005
		//       MODIFIED       Dan Fisher Aug. 2008
		//                      Brent Griffith May 2009 EMS setpoint check
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine initializes the
		// Plant loop nodes one time at the beginning of the simulation.
		// It also reinitializes loop temperatures if loop setpoint
		// temperature changes. Branch levels for all branches are also set.

		// METHODOLOGY EMPLOYED:
		// Needs description, as appropriate.

		// REFERENCES:
		// na

		// Using/Aliasing
		using ScheduleManager::GetCurrentScheduleValue;
		using namespace DataSizing;
		using PlantLoopEquip::SimPlantEquip;
		using General::RoundSigDigits;
		using EMSManager::iTemperatureSetPoint;
		using EMSManager::CheckIfNodeSetPointManagedByEMS;
		using EMSManager::iTemperatureMaxSetPoint;
		using EMSManager::iTemperatureMinSetPoint;
		using PlantUtilities::SetAllFlowLocks;
		using DataHVACGlobals::NumPlantLoops;
		using DataHVACGlobals::NumCondLoops;
		using PlantLoopSolver::SimulateAllLoopSidePumps;
		using DataPlant::PlantFirstSizesOkayToReport;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int LoopNum; // plant loop counter
		int LoopSideNum;
		int BranchNum; // branch loop counter
		int CompNum; // plant side component counter
		int SensedNode;

		Real64 LoopSetPointTemp; // the loop control or setpoint temperature

		static bool ErrorsFound( false );
		bool FinishSizingFlag;

		static bool SupplyEnvrnFlag( true );
		//  LOGICAL,SAVE  :: MySizeFlag = .TRUE.
		static bool MySetPointCheckFlag( true );

		static Array1D_bool PlantLoopSetPointInitFlag;

		int HalfLoopNum;
		int passNum;

		if ( ! allocated( PlantLoopSetPointInitFlag ) ) {
			PlantLoopSetPointInitFlag.allocate( TotNumLoops );
		}

		// Initialize the setpoints  for Load range based schemes only as determined by the init flag
		// The input already requires a loop setpoint.  The plantloop object requires
		// specification of a loop node and corresponding setpoint manager.  Using a 'component setpoint'
		// control scheme does NOT eliminate the requirement for a plant loop setpoint.  So there is
		// already the possibility that a component setpoint controlled object on the loop outlet
		// branch would have the same setpoint node as the loop.  I don't think setpoint manager traps
		// for this user input error, but it might.  Since both loop and component setpoints already
		// peacefully coexist on the loop, we can allow the user to intentionally specify and use both.
		// The only change required is to NOT smear the loop setpoint over all the loop nodes.  Just
		// read it from the setpoint node and use it.  In the short term it will remain up to the user
		// to specify the location of the loop setpoint control node and avoid conflicts with component
		// setpoint nodes.  Operationally, we will ignore the user specified placement of the loop setpoint
		// node and assume that it is physically located at each half loop outlet for purposes of calculating loop
		// demand.  Long term, I recommend that we:
		//     1. specify the setpointmanager:plant object name (not the node name) in the plantloop/condloop objects
		//     2. write a new setpoint manager (setpointmanager:plant) that is more suitable for plant use and
		//        accomodates AIR and GROUND setpoints...with offsets.

		//*****************************************************************
		//ONE TIME LOOP NODE SETPOINT CHECK
		//*****************************************************************
		if ( MySetPointCheckFlag && DoSetPointTest ) {

			// check for missing setpoints
			for ( LoopNum = 1; LoopNum <= TotNumLoops; ++LoopNum ) {
				LoopSetPointTemp = Node( PlantLoop( LoopNum ).TempSetPointNodeNum ).TempSetPoint;

				SensedNode = PlantLoop( LoopNum ).TempSetPointNodeNum;
				if ( SensedNode > 0 ) {
					if ( Node( SensedNode ).TempSetPoint == SensedNodeFlagValue ) {
						if ( ! AnyEnergyManagementSystemInModel ) {
							ShowSevereError( "PlantManager: No Setpoint Manager Defined for Node=" + NodeID( SensedNode ) + " in PlantLoop=" + PlantLoop( LoopNum ).Name );
							ShowContinueError( "Add Temperature Setpoint Manager with Control Variable = \"Temperature\" for this PlantLoop." );
							SetPointErrorFlag = true;
						} else {
							// need call to EMS to check node
							CheckIfNodeSetPointManagedByEMS( SensedNode, iTemperatureSetPoint, SetPointErrorFlag );
							if ( SetPointErrorFlag ) {
								ShowSevereError( "PlantManager: No Setpoint Manager Defined for Node=" + NodeID( SensedNode ) + " in PlantLoop=" + PlantLoop( LoopNum ).Name );
								ShowContinueError( "Add Temperature Setpoint Manager with Control Variable = \"Temperature\" for this PlantLoop." );
								ShowContinueError( "Or add EMS Actuator to provide temperature setpoint at this node" );
							}
						}
					}

				}
			}
			MySetPointCheckFlag = false;
		}
		//*****************************************************************
		// END ONE TIME LOOP NODE SETPOINT CHECK

		//*****************************************************************
		//First Pass PUMP AND SIZING INIT
		//*****************************************************************
		if ( ! PlantFirstSizeCompleted ) {

			SetAllFlowLocks( FlowUnlocked );
			FinishSizingFlag = false;
			PlantFirstSizesOkayToFinalize = false; // set global flag for when it ready to store final sizes
			PlantFirstSizesOkayToReport = false;
			PlantFinalSizesOkayToReport = false;
			GetCompSizFac = true;
			for ( passNum = 1; passNum <= 4; ++passNum ) { //begin while loop to iterate over the next calls sequentially
				InitLoopEquip = true;

				// Step 2, call component models it  using PlantCallingOrderInfo for sizing
				for ( HalfLoopNum = 1; HalfLoopNum <= TotNumHalfLoops; ++HalfLoopNum ) {
					LoopNum = PlantCallingOrderInfo( HalfLoopNum ).LoopIndex;
					LoopSideNum = PlantCallingOrderInfo( HalfLoopNum ).LoopSide;
					CurLoopNum = LoopNum;

					for ( BranchNum = 1; BranchNum <= PlantLoop( LoopNum ).LoopSide( LoopSideNum ).TotalBranches; ++BranchNum ) {
						for ( CompNum = 1; CompNum <= PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).TotalComponents; ++CompNum ) {
							SimPlantEquip( LoopNum, LoopSideNum, BranchNum, CompNum, FirstHVACIteration, InitLoopEquip, GetCompSizFac );
						} //-CompNum
					} //-BranchNum
				}

				// step 3, revise calling order
				// have now called each plant component model at least once with InitLoopEquip = .TRUE.
				//  this means the calls to InterConnectTwoPlantLoopSides have now been made, so rework calling order
				RevisePlantCallingOrder();

				// Step 4: Simulate plant loop components so their design flows are included

				for ( HalfLoopNum = 1; HalfLoopNum <= TotNumHalfLoops; ++HalfLoopNum ) {

					LoopNum = PlantCallingOrderInfo( HalfLoopNum ).LoopIndex;
					LoopSideNum = PlantCallingOrderInfo( HalfLoopNum ).LoopSide;
					CurLoopNum = LoopNum;
					if ( LoopSideNum == SupplySide ) {
						SizePlantLoop( LoopNum, FinishSizingFlag );
					}
				}
			GetCompSizFac = false;
			} // iterative passes thru sizing related routines.  end while?

			//Step 5 now one more time for the final
			for ( HalfLoopNum = 1; HalfLoopNum <= TotNumHalfLoops; ++HalfLoopNum ) {
				if (DoHVACSizingSimulation ) {
					PlantFirstSizesOkayToFinalize = true;
					FinishSizingFlag = true;
					PlantFirstSizesOkayToReport = true;
					PlantFinalSizesOkayToReport = false;
				} else {
					PlantFirstSizesOkayToFinalize = true;
					FinishSizingFlag = true;
					PlantFirstSizesOkayToReport = false;
					PlantFinalSizesOkayToReport = true;
				}
				LoopNum = PlantCallingOrderInfo( HalfLoopNum ).LoopIndex;
				LoopSideNum = PlantCallingOrderInfo( HalfLoopNum ).LoopSide;
				CurLoopNum = LoopNum;
				if ( LoopSideNum == SupplySide ) {
					SizePlantLoop( LoopNum, FinishSizingFlag );
				}
				//pumps are special so call them directly
				SimulateAllLoopSidePumps(LoopNum , LoopSideNum);
				for ( BranchNum = 1; BranchNum <= PlantLoop( LoopNum ).LoopSide( LoopSideNum ).TotalBranches; ++BranchNum ) {
					for ( CompNum = 1; CompNum <= PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).TotalComponents; ++CompNum ) {
						SimPlantEquip( LoopNum, LoopSideNum, BranchNum, CompNum, FirstHVACIteration, InitLoopEquip, GetCompSizFac );
					} //-CompNum
				} //-BranchNum
//				if ( PlantLoop( LoopNum ).PlantSizNum > 0 ) PlantSizData( PlantLoop( LoopNum ).PlantSizNum ).VolFlowSizingDone = true;


			}

			PlantFirstSizeCompleted = true;
			PlantFirstSizesOkayToReport = false;
		}
		//*****************************************************************
		//END First Pass SIZING INIT
		//*****************************************************************
		//*****************************************************************
		//BEGIN Resizing Pass for HVAC Sizing Simultion Adjustments
		//*****************************************************************
		if ( RedoSizesHVACSimulation && ! PlantReSizingCompleted ) {


			// cycle through plant equipment calling with InitLoopEquip true
			InitLoopEquip = true;
			GetCompSizFac = false;
			for ( HalfLoopNum = 1; HalfLoopNum <= TotNumHalfLoops; ++HalfLoopNum ) {
					LoopNum = PlantCallingOrderInfo( HalfLoopNum ).LoopIndex;
					LoopSideNum = PlantCallingOrderInfo( HalfLoopNum ).LoopSide;
					CurLoopNum = LoopNum;

					for ( BranchNum = 1; BranchNum <= PlantLoop( LoopNum ).LoopSide( LoopSideNum ).TotalBranches; ++BranchNum ) {
						for ( CompNum = 1; CompNum <= PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).TotalComponents; ++CompNum ) {
							SimPlantEquip( LoopNum, LoopSideNum, BranchNum, CompNum, FirstHVACIteration, InitLoopEquip, GetCompSizFac );
						} //-CompNum
					} //-BranchNum
			}

			//reset loop level
			PlantFinalSizesOkayToReport = true;
			for ( LoopNum = 1; LoopNum <= TotNumLoops; ++LoopNum ) {
				ResizePlantLoopLevelSizes(LoopNum);
			}

			InitLoopEquip = true;


			//now call everything again to reporting turned on
			for ( HalfLoopNum = 1; HalfLoopNum <= TotNumHalfLoops; ++HalfLoopNum ) {
					LoopNum = PlantCallingOrderInfo( HalfLoopNum ).LoopIndex;
					LoopSideNum = PlantCallingOrderInfo( HalfLoopNum ).LoopSide;
					CurLoopNum = LoopNum;

					for ( BranchNum = 1; BranchNum <= PlantLoop( LoopNum ).LoopSide( LoopSideNum ).TotalBranches; ++BranchNum ) {
						for ( CompNum = 1; CompNum <= PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).TotalComponents; ++CompNum ) {
							SimPlantEquip( LoopNum, LoopSideNum, BranchNum, CompNum, FirstHVACIteration, InitLoopEquip, GetCompSizFac );
						} //-CompNum
					} //-BranchNum
					//pumps are special so call them directly
					SimulateAllLoopSidePumps(LoopNum , LoopSideNum);
			}


			PlantReSizingCompleted = true;
			PlantFinalSizesOkayToReport = false;
		}
		//*****************************************************************
		//END Resizing Pass for HVAC Sizing Simultion Adjustments
		//*****************************************************************
		//*****************************************************************
		//BEGIN ONE TIME ENVIRONMENT INITS
		//*****************************************************************
		if ( SupplyEnvrnFlag && BeginEnvrnFlag ) {

			for ( LoopNum = 1; LoopNum <= TotNumLoops; ++LoopNum ) {
				for ( LoopSideNum = DemandSide; LoopSideNum <= SupplySide; ++LoopSideNum ) {
					// check if setpoints being placed on node properly
					if ( PlantLoop( LoopNum ).LoopDemandCalcScheme == DualSetPointDeadBand ) {
						if ( Node( PlantLoop( LoopNum ).TempSetPointNodeNum ).TempSetPointHi == SensedNodeFlagValue ) {
							if ( ! AnyEnergyManagementSystemInModel ) {
								ShowSevereError( "Plant Loop: missing high temperature setpoint for dual setpoint deadband demand scheme" );
								ShowContinueError( "Node Referenced =" + NodeID( PlantLoop( LoopNum ).TempSetPointNodeNum ) );
								ShowContinueError( "Use a SetpointManager:Scheduled:DualSetpoint to establish appropriate setpoints" );
								SetPointErrorFlag = true;
							} else {
								CheckIfNodeSetPointManagedByEMS( PlantLoop( LoopNum ).TempSetPointNodeNum, iTemperatureMaxSetPoint, SetPointErrorFlag );
								if ( SetPointErrorFlag ) {
									ShowSevereError( "Plant Loop: missing high temperature setpoint for dual setpoint deadband demand scheme" );
									ShowContinueError( "Node Referenced =" + NodeID( PlantLoop( LoopNum ).TempSetPointNodeNum ) );
									ShowContinueError( "Use a SetpointManager:Scheduled:DualSetpoint to establish appropriate setpoints" );
									ShowContinueError( "Or add EMS Actuator for Temperature Maximum Setpoint" );

								} //SetPointErrorFlag
							} //Not EMS
						} //Node TSPhi = Sensed
						if ( Node( PlantLoop( LoopNum ).TempSetPointNodeNum ).TempSetPointLo == SensedNodeFlagValue ) {
							if ( ! AnyEnergyManagementSystemInModel ) {
								ShowSevereError( "Plant Loop: missing low temperature setpoint for dual setpoint deadband demand scheme" );
								ShowContinueError( "Node Referenced =" + NodeID( PlantLoop( LoopNum ).TempSetPointNodeNum ) );
								ShowContinueError( "Use a SetpointManager:Scheduled:DualSetpoint to establish appropriate setpoints" );
								SetPointErrorFlag = true;
							} else {
								CheckIfNodeSetPointManagedByEMS( PlantLoop( LoopNum ).TempSetPointNodeNum, iTemperatureMinSetPoint, SetPointErrorFlag );
								if ( SetPointErrorFlag ) {
									ShowSevereError( "Plant Loop: missing low temperature setpoint for dual setpoint deadband demand scheme" );
									ShowContinueError( "Node Referenced =" + NodeID( PlantLoop( LoopNum ).TempSetPointNodeNum ) );
									ShowContinueError( "Use a SetpointManager:Scheduled:DualSetpoint to establish appropriate setpoints" );
									ShowContinueError( "Or add EMS Actuator for Temperature Minimum Setpoint" );

								} //SetPointErrorFlag
							} //NOT EMS
						} //Node TSPtLo = Sensed...
					} //LoopDemandScheme = DualSPDB
				} //LOOPSIDE
			} //PLANT LOOP

			//Any per-environment load distribution init should be OK here
			//Just clear away any trailing MyLoad for now...
			//This could likely be moved into InitLoadDistribution also...
			for ( LoopNum = 1; LoopNum <= TotNumLoops; ++LoopNum ) {
				for ( LoopSideNum = DemandSide; LoopSideNum <= SupplySide; ++LoopSideNum ) {
					for ( BranchNum = 1; BranchNum <= PlantLoop( LoopNum ).LoopSide( LoopSideNum ).TotalBranches; ++BranchNum ) {
						for ( CompNum = 1; CompNum <= PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).TotalComponents; ++CompNum ) {
							PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).Comp( CompNum ).MyLoad = 0.0;
							PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).Comp( CompNum ).FreeCoolCntrlShutDown = false;
							PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).Comp( CompNum ).Available = false;
						}
					}
				}
			}

			SupplyEnvrnFlag = false;
			//!*****************************************************************
			// !END OF ONE TIME ENVIRONMENT INITS
			//!*****************************************************************
		} //
		if ( ! BeginEnvrnFlag ) SupplyEnvrnFlag = true;

		if ( ErrorsFound ) ShowFatalError( "Preceding errors caused termination" );

	}

	void
	ReInitPlantLoopsAtFirstHVACIteration()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Sept 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// initialize node mass flow requests

		// METHODOLOGY EMPLOYED:
		// called from SimHVAC to reset mass flow rate requests
		// this contains all the initializ

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataEnvironment::StdBaroPress;
		using HVACInterfaceManager::PlantCommonPipe;
		using ScheduleManager::GetCurrentScheduleValue;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const StartQuality( 1.0 );
		Real64 const StartHumRat( 0.0 );
		static std::string const RoutineNameAlt( "InitializeLoops" );
		static std::string const RoutineName( "PlantManager:InitializeLoop" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int LoopNum; // plant loop counter
		int LoopIn;
		Real64 LoopMaxMassFlowRate; // maximum allowable loop mass flow rate
		Real64 LoopSetPointTemp; // the loop control or setpoint temperature
		Real64 LoopMaxTemp; // maximum allowable loop temperature
		Real64 LoopMinTemp; // minimum allowable loop temperature
		Real64 LoopSetPointTempLo; // the loop control or setpoint temperature
		Real64 LoopSetPointTempHi; // the loop control or setpoint temperature
		Real64 SecondaryLoopSetPointTemp; // loop setpoint temperature for common pipes with different secondary setpt
		int LoopSideNum;
		int BranchNum; // branch loop counter
		int OpNum; // operation scheme counter
		int CompNum; // plant side component counter
		int BranchInlet; // branch inlet node number
		int ComponentInlet; // component inlet node number
		int ComponentOutlet; // component outlet node number
		static bool MyEnvrnFlag( true );
		Real64 LoopMinMassFlowRate; // minimum allowable loop mass flow rate
		Real64 SteamDensity;
		Real64 SteamTemp;
		Real64 StartEnthalpy;
		Real64 Cp;
		Real64 rho;
		Real64 LoopSetPointTemperatureHi;
		Real64 LoopSetPointTemperatureLo;

		//*****************************************************************
		//BEGIN ENVIRONMENT INITS
		//*****************************************************************

		if ( MyEnvrnFlag && BeginEnvrnFlag ) {

			for ( LoopNum = 1; LoopNum <= TotNumLoops; ++LoopNum ) {
				for ( LoopSideNum = DemandSide; LoopSideNum <= SupplySide; ++LoopSideNum ) {

					{ auto const SELECT_CASE_var( PlantLoop( LoopNum ).LoopDemandCalcScheme );

					if ( SELECT_CASE_var == SingleSetPoint ) {
						LoopSetPointTemp = Node( PlantLoop( LoopNum ).TempSetPointNodeNum ).TempSetPoint;

					} else if ( SELECT_CASE_var == DualSetPointDeadBand ) {
						// Get the range of setpoints
						LoopSetPointTemperatureHi = Node( PlantLoop( LoopNum ).TempSetPointNodeNum ).TempSetPointHi;
						LoopSetPointTemperatureLo = Node( PlantLoop( LoopNum ).TempSetPointNodeNum ).TempSetPointLo;
						LoopSetPointTemp = ( LoopSetPointTemperatureLo + LoopSetPointTemperatureHi ) / 2.0;
					}}

					if ( ( PlantLoop( LoopNum ).CommonPipeType == CommonPipe_TwoWay ) && ( LoopSideNum == DemandSide ) && ( PlantLoop( LoopNum ).LoopSide( DemandSide ).InletNodeSetPt ) ) { // get a second setpoint for secondaryLoop
						// if the plant loop is two common pipe configured for temperature control on secondary side inlet, then
						// we want to initialize the demand side of the loop using that setpoint
						LoopSetPointTemp = Node( PlantLoop( LoopNum ).LoopSide( DemandSide ).NodeNumIn ).TempSetPoint;
					}

					// Check the Loop Setpoint and make sure it is bounded by the Loop Max and Min
					LoopMaxTemp = PlantLoop( LoopNum ).MaxTemp;
					LoopMinTemp = PlantLoop( LoopNum ).MinTemp;

					// trap for -999 and set to average of limits if so
					if ( LoopSetPointTemp == SensedNodeFlagValue ) {
						LoopSetPointTemp = ( LoopMinTemp + LoopMaxTemp ) / 2.0;
					}
					// Check it against the loop temperature limits
					LoopSetPointTemp = min( LoopMaxTemp, LoopSetPointTemp );
					LoopSetPointTemp = max( LoopMinTemp, LoopSetPointTemp );

					//Initialize the capacitance model at the tank interface, and other loop side values
					PlantLoop( LoopNum ).LoopSide( LoopSideNum ).TempInterfaceTankOutlet = LoopSetPointTemp;
					PlantLoop( LoopNum ).LoopSide( LoopSideNum ).LastTempInterfaceTankOutlet = LoopSetPointTemp;
					PlantLoop( LoopNum ).LoopSide( LoopSideNum ).LoopSideInlet_TankTemp = LoopSetPointTemp;
					PlantLoop( LoopNum ).LoopSide( LoopSideNum ).TotalPumpHeat = 0.0;
					if ( allocated( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Pumps ) ) for ( auto & e : PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Pumps ) e.PumpHeatToFluid = 0.0;
					PlantLoop( LoopNum ).LoopSide( LoopSideNum ).FlowRequest = 0.0;
					PlantLoop( LoopNum ).LoopSide( LoopSideNum ).TimeElapsed = 0.0;
					PlantLoop( LoopNum ).LoopSide( LoopSideNum ).FlowLock = 0;
					PlantLoop( LoopNum ).LoopSide( LoopSideNum ).InletNode.TemperatureHistory = 0.0;
					PlantLoop( LoopNum ).LoopSide( LoopSideNum ).InletNode.MassFlowRateHistory = 0.0;
					PlantLoop( LoopNum ).LoopSide( LoopSideNum ).OutletNode.TemperatureHistory = 0.0;
					PlantLoop( LoopNum ).LoopSide( LoopSideNum ).OutletNode.MassFlowRateHistory = 0.0;

					if ( PlantLoop( LoopNum ).FluidType != NodeType_Steam ) {
						Cp = GetSpecificHeatGlycol( PlantLoop( LoopNum ).FluidName, LoopSetPointTemp, PlantLoop( LoopNum ).FluidIndex, RoutineNameAlt );
						StartEnthalpy = Cp * LoopSetPointTemp;
					}
					// Use Min/Max flow rates to initialize loop
					if ( PlantLoop( LoopNum ).FluidType == NodeType_Water ) {
						rho = GetDensityGlycol( PlantLoop( LoopNum ).FluidName, LoopSetPointTemp, PlantLoop( LoopNum ).FluidIndex, RoutineNameAlt );

						LoopMaxMassFlowRate = PlantLoop( LoopNum ).MaxVolFlowRate * rho;
						LoopMinMassFlowRate = PlantLoop( LoopNum ).MinVolFlowRate * rho;

					}
					//use saturated liquid of steam at the loop setpoint temp as the starting enthalpy for a water loop
					if ( PlantLoop( LoopNum ).FluidType == NodeType_Steam ) {
						SteamTemp = 100.0;
						SteamDensity = GetSatDensityRefrig( fluidNameSteam, SteamTemp, 1.0, PlantLoop( LoopNum ).FluidIndex, RoutineName );
						LoopMaxMassFlowRate = PlantLoop( LoopNum ).MaxVolFlowRate * SteamDensity;
						StartEnthalpy = GetSatEnthalpyRefrig( fluidNameSteam, LoopSetPointTemp, 0.0, PlantLoop( LoopNum ).FluidIndex, RoutineName );
						LoopMinMassFlowRate = PlantLoop( LoopNum ).MinVolFlowRate * SteamDensity;
					}

					LoopMaxMassFlowRate = max( 0.0, LoopMaxMassFlowRate );
					LoopMinMassFlowRate = max( 0.0, LoopMinMassFlowRate );

					//Initial all loop nodes by initializing all component inlet and outlet nodes
					for ( BranchNum = 1; BranchNum <= PlantLoop( LoopNum ).LoopSide( LoopSideNum ).TotalBranches; ++BranchNum ) {
						for ( CompNum = 1; CompNum <= PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).TotalComponents; ++CompNum ) {
							ComponentInlet = PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).Comp( CompNum ).NodeNumIn;
							ComponentOutlet = PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).Comp( CompNum ).NodeNumOut;
							BranchInlet = PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).NodeNumIn;

							Node( ComponentInlet ).Temp = LoopSetPointTemp;
							Node( ComponentInlet ).TempMin = LoopMinTemp;
							Node( ComponentInlet ).TempMax = LoopMaxTemp;
							Node( ComponentInlet ).TempLastTimestep = LoopSetPointTemp;

							Node( ComponentInlet ).MassFlowRate = 0.0;
							PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).Comp( CompNum ).MyLoad = 0.0;
							PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).Comp( CompNum ).Available = false;
							PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).Comp( CompNum ).FreeCoolCntrlShutDown = false;
							PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).RequestedMassFlow = 0.0;

							if ( Node( ComponentInlet ).MassFlowRateMin > 0.0 ) {
								Node( ComponentInlet ).MassFlowRateMinAvail = Node( ComponentInlet ).MassFlowRateMin;
							} else {
								Node( ComponentInlet ).MassFlowRateMin = LoopMinMassFlowRate;
								Node( ComponentInlet ).MassFlowRateMinAvail = LoopMinMassFlowRate;
							}

							if ( Node( ComponentInlet ).MassFlowRateMax > 0.0 ) {
								Node( ComponentInlet ).MassFlowRateMaxAvail = Node( ComponentInlet ).MassFlowRateMax;
							} else {
								Node( ComponentInlet ).MassFlowRateMax = LoopMaxMassFlowRate;
								Node( ComponentInlet ).MassFlowRateMaxAvail = LoopMaxMassFlowRate;
							}

							Node( ComponentInlet ).MassFlowRateRequest = 0.0;
							Node( ComponentInlet ).Quality = StartQuality;
							Node( ComponentInlet ).Press = StdBaroPress;
							Node( ComponentInlet ).Enthalpy = StartEnthalpy;
							Node( ComponentInlet ).HumRat = StartHumRat;

							Node( ComponentOutlet ).FluidType = Node( BranchInlet ).FluidType;
							Node( ComponentOutlet ).Temp = Node( BranchInlet ).Temp;
							Node( ComponentOutlet ).TempMin = Node( BranchInlet ).TempMin;
							Node( ComponentOutlet ).TempMax = Node( BranchInlet ).TempMax;
							Node( ComponentOutlet ).TempLastTimestep = Node( BranchInlet ).TempLastTimestep;
							Node( ComponentOutlet ).MassFlowRate = Node( BranchInlet ).MassFlowRate;
							Node( ComponentOutlet ).MassFlowRateMin = Node( BranchInlet ).MassFlowRateMin;
							Node( ComponentOutlet ).MassFlowRateMax = Node( BranchInlet ).MassFlowRateMax;
							Node( ComponentOutlet ).MassFlowRateMinAvail = Node( BranchInlet ).MassFlowRateMinAvail;
							Node( ComponentOutlet ).MassFlowRateMaxAvail = Node( BranchInlet ).MassFlowRateMaxAvail;
							Node( ComponentOutlet ).MassFlowRateRequest = 0.0;
							Node( ComponentOutlet ).Quality = StartQuality;
							Node( ComponentOutlet ).Press = StdBaroPress;
							Node( ComponentOutlet ).Enthalpy = StartEnthalpy;
							Node( ComponentOutlet ).HumRat = StartHumRat;
						} //COMPONENT LOOP
					} //BRANCH LOOP
				} //LOOPSIDE
			} //PLANT LOOP
			for ( auto & e : PlantReport ) {
				e.CoolingDemand = 0.0;
				e.HeatingDemand = 0.0;
				e.DemandNotDispatched = 0.0;
				e.UnmetDemand = 0.0;
				e.LastLoopSideSimulated = 0;
				e.InletNodeFlowrate = 0.0;
				e.InletNodeTemperature = 0.0;
				e.OutletNodeFlowrate = 0.0;
				e.OutletNodeTemperature = 0.0;
			}

			MyEnvrnFlag = false;
			//*****************************************************************
			//END OF ENVIRONMENT INITS
			//*****************************************************************
		}

		if ( ! BeginEnvrnFlag ) MyEnvrnFlag = true;

		// FirstHVACiteration inits
		for ( LoopNum = 1; LoopNum <= TotNumLoops; ++LoopNum ) {
			LoopIn = PlantLoop( LoopNum ).LoopSide( DemandSide ).NodeNumIn; //DSU? Demand/Supply side inlet??
			//UPDATE LOOP FLOW SETPOINT
			//    Node(LoopIn)%MassFlowRateSetPoint =  LoopMaxMassFlowRate !DSU? this is suspect, may not be set?
			//UPDATE LOOP TEMPERATURE SETPOINTS

			LoopSetPointTemp = Node( PlantLoop( LoopNum ).TempSetPointNodeNum ).TempSetPoint;

			// Check the Loop Setpoint and make sure it is bounded by the Loop Max and Min
			LoopMaxTemp = PlantLoop( LoopNum ).MaxTemp;
			LoopMinTemp = PlantLoop( LoopNum ).MinTemp;
			// Check it against the loop temperature limits
			LoopSetPointTemp = min( LoopMaxTemp, LoopSetPointTemp );
			LoopSetPointTemp = max( LoopMinTemp, LoopSetPointTemp );

			//Update supply side loop setpoint in plant data structure
			PlantLoop( LoopNum ).LoopSide( SupplySide ).TempSetPoint = LoopSetPointTemp;
			PlantLoop( LoopNum ).LoopSide( DemandSide ).TempSetPoint = LoopSetPointTemp;

			//Update supply side hi-lo setpoints for dual SP control
			if ( PlantLoop( LoopNum ).LoopDemandCalcScheme == DualSetPointDeadBand ) {
				LoopSetPointTempHi = Node( PlantLoop( LoopNum ).TempSetPointNodeNum ).TempSetPointHi;
				LoopSetPointTempLo = Node( PlantLoop( LoopNum ).TempSetPointNodeNum ).TempSetPointLo;
				LoopSetPointTempHi = min( LoopMaxTemp, LoopSetPointTempHi );
				LoopSetPointTempHi = max( LoopMinTemp, LoopSetPointTempHi );
				LoopSetPointTempLo = min( LoopMaxTemp, LoopSetPointTempLo );
				LoopSetPointTempLo = max( LoopMinTemp, LoopSetPointTempLo );
				PlantLoop( LoopNum ).LoopSide( SupplySide ).TempSetPointHi = LoopSetPointTempHi;
				PlantLoop( LoopNum ).LoopSide( SupplySide ).TempSetPointLo = LoopSetPointTempLo;
			}

			//update demand side loop setpoint in plant data structure
			if ( PlantLoop( LoopNum ).CommonPipeType == CommonPipe_TwoWay ) { // get a second setpoint for secondaryLoop
				// if the plant loop is two common pipe configured for temperature control on secondary side inlet, then
				// we want to initialize the demand side of the loop using that setpoint
				if ( PlantLoop( LoopNum ).LoopSide( DemandSide ).InletNodeSetPt ) {
					SecondaryLoopSetPointTemp = Node( PlantLoop( LoopNum ).LoopSide( DemandSide ).NodeNumIn ).TempSetPoint;
					SecondaryLoopSetPointTemp = min( LoopMaxTemp, SecondaryLoopSetPointTemp );
					SecondaryLoopSetPointTemp = max( LoopMinTemp, SecondaryLoopSetPointTemp );
					PlantLoop( LoopNum ).LoopSide( DemandSide ).TempSetPoint = SecondaryLoopSetPointTemp;
					//Since Dual setpoint not explicitly available for demand side, we can't do the
					//bounding check on hi/lo setpoint.  IF we did we would over-write
					//the SensedNodeFlagValue of -999 for no dual setpoint case.
					PlantLoop( LoopNum ).LoopSide( DemandSide ).TempSetPointHi = Node( PlantLoop( LoopNum ).LoopSide( DemandSide ).NodeNumIn ).TempSetPointHi;
					PlantLoop( LoopNum ).LoopSide( DemandSide ).TempSetPointLo = Node( PlantLoop( LoopNum ).LoopSide( DemandSide ).NodeNumIn ).TempSetPointLo;
				}

				//initialize common pipe flows to zero.
				if ( allocated( PlantCommonPipe ) ) {
					PlantCommonPipe( LoopNum ).PriToSecFlow = 0.0;
					PlantCommonPipe( LoopNum ).SecToPriFlow = 0.0;
					PlantCommonPipe( LoopNum ).PriCPLegFlow = 0.0;
					PlantCommonPipe( LoopNum ).SecCPLegFlow = 0.0;
				}
			} else { //no secondary loop, so use supply side loop SP on demand side too.
				PlantLoop( LoopNum ).LoopSide( DemandSide ).TempSetPoint = LoopSetPointTemp;
				if ( PlantLoop( LoopNum ).LoopDemandCalcScheme == DualSetPointDeadBand ) {
					PlantLoop( LoopNum ).LoopSide( DemandSide ).TempSetPointHi = LoopSetPointTempHi;
					PlantLoop( LoopNum ).LoopSide( DemandSide ).TempSetPointLo = LoopSetPointTempLo;
				}
			}

			for ( LoopSideNum = DemandSide; LoopSideNum <= SupplySide; ++LoopSideNum ) {
				for ( BranchNum = 1; BranchNum <= PlantLoop( LoopNum ).LoopSide( LoopSideNum ).TotalBranches; ++BranchNum ) {
					for ( CompNum = 1; CompNum <= PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).TotalComponents; ++CompNum ) {
						ComponentInlet = PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).Comp( CompNum ).NodeNumIn;
						ComponentOutlet = PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Branch( BranchNum ).Comp( CompNum ).NodeNumOut;

						//reinit to node hardware limits
						Node( ComponentInlet ).MassFlowRateMinAvail = Node( ComponentInlet ).MassFlowRateMin;
						Node( ComponentOutlet ).MassFlowRateMinAvail = Node( ComponentInlet ).MassFlowRateMin;
						Node( ComponentInlet ).MassFlowRateMaxAvail = Node( ComponentInlet ).MassFlowRateMax;
						Node( ComponentOutlet ).MassFlowRateMaxAvail = Node( ComponentInlet ).MassFlowRateMax;

						Node( ComponentInlet ).MassFlowRateRequest = 0.0;
						Node( ComponentOutlet ).MassFlowRateRequest = 0.0;

					}
				}
			}

			for ( OpNum = 1; OpNum <= PlantLoop( LoopNum ).NumOpSchemes; ++OpNum ) {
				// If the operating scheme is scheduled "OFF", go to next scheme
				if ( GetCurrentScheduleValue( PlantLoop( LoopNum ).OpScheme( OpNum ).SchedPtr ) <= 0.0 ) {
					PlantLoop( LoopNum ).OpScheme( OpNum ).Available = false;
				} else {
					PlantLoop( LoopNum ).OpScheme( OpNum ).Available = true;
				}
			}
		}

	}

	void
	UpdateNodeThermalHistory()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Sept 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// update temperature history for plant capacitance model and other

		// METHODOLOGY EMPLOYED:
		// copy current values into "LastTimestep" values

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		// array assignment
		if ( NumOfNodes > 0 ) {
			for ( auto & e : Node ) { //MA
				e.TempLastTimestep = e.Temp;
				e.EnthalpyLastTimestep = e.Enthalpy;
			}
		}

	}

	void
	CheckPlantOnAbort()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Septemeber 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Called once E+ is in the process of aborting because of fatal error
		//  check for plant input problems to help users find problems in input files

		// METHODOLOGY EMPLOYED:
		//  search plant data structures for issues that may help solve problems in input files
		//  1.   if loop side has a splitter/mixer and one branch in there is control type bypass,
		//       then another branch in the s/m needs to be active
		//  other checks could/should be added!

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na
		// Using/Aliasing
		using DataErrorTracking::AskForPlantCheckOnAbort;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int LoopNum; // DO loop counter for loops
		bool ActiveCntrlfound; // used to search for active control branches in parallel with bypass branches
		int ParalBranchNum; // used to search for active control branches in parallel with bypass branches
		int ParalBranchNum2; // used to search for active control branches in parallel with bypass branches
		int BranchNum2; // used to search for active control branches in parallel with bypass branches
		int SideNum;
		int numLoopSides;
		int SplitNum;
		int BranchNum; // DO loop counter for branches
		int CompNum; // do loop for multiple components on a branch
		bool ShouldBeACTIVE;

		if ( ! ( AskForPlantCheckOnAbort ) ) {
			return;
		}

		if ( ! ( TotNumLoops > 0 ) ) return;
		if ( ! ( allocated( PlantLoop ) ) ) return;

		for ( LoopNum = 1; LoopNum <= TotNumLoops; ++LoopNum ) {
			numLoopSides = 2;
			for ( SideNum = 1; SideNum <= numLoopSides; ++SideNum ) {
				if ( ! ( PlantLoop( LoopNum ).LoopSide( SideNum ).SplitterExists ) ) continue;
				for ( SplitNum = 1; SplitNum <= PlantLoop( LoopNum ).LoopSide( SideNum ).NumSplitters; ++SplitNum ) {
					for ( ParalBranchNum = 1; ParalBranchNum <= PlantLoop( LoopNum ).LoopSide( SideNum ).Splitter( SplitNum ).TotalOutletNodes; ++ParalBranchNum ) {
						BranchNum = PlantLoop( LoopNum ).LoopSide( SideNum ).Splitter( SplitNum ).BranchNumOut( ParalBranchNum );
						if ( PlantLoop( LoopNum ).LoopSide( SideNum ).Branch( BranchNum ).IsBypass ) { // we know there is a bypass
							// check that there is at least one 'Active' control type in parallel with bypass branch
							ActiveCntrlfound = false;
							for ( ParalBranchNum2 = 1; ParalBranchNum2 <= PlantLoop( LoopNum ).LoopSide( SideNum ).Splitter( SplitNum ).TotalOutletNodes; ++ParalBranchNum2 ) {
								BranchNum2 = PlantLoop( LoopNum ).LoopSide( SideNum ).Splitter( SplitNum ).BranchNumOut( ParalBranchNum2 );
								if ( PlantLoop( LoopNum ).LoopSide( SideNum ).Branch( BranchNum2 ).ControlType == ControlType_Active ) {
									ActiveCntrlfound = true;
								}
							}
							if ( ! ( ActiveCntrlfound ) ) {
								ShowWarningError( "Check control types on branches between splitter and mixer in PlantLoop=" + PlantLoop( LoopNum ).Name );
								ShowContinueError( "Found a BYPASS branch with no ACTIVE branch in parallel with it" );
								ShowContinueError( "In certain (but not all) situations, this can cause problems; please verify your inputs" );
								ShowContinueError( "Bypass branch named: " + PlantLoop( LoopNum ).LoopSide( SideNum ).Branch( BranchNum ).Name );
							}
						} // bypass present

						//check for possible components on demand side that should be ACTIVE but are not
						if ( SideNum == DemandSide ) {
							// check for presences of the following components whose branch control type should be active
							// WATER HEATER:MIXED
							// WATER HEATER:STRATIFIED
							// WATER USE CONNECTIONS
							// COIL:WATER:COOLING
							// COIL:WATER:SIMPLEHEATING
							// COIL:STEAM:AIRHEATING
							// SOLAR COLLECTOR:FLAT PLATE
							// PLANT LOAD PROFILE
							for ( CompNum = 1; CompNum <= PlantLoop( LoopNum ).LoopSide( SideNum ).Branch( BranchNum ).TotalComponents; ++CompNum ) {
								ShouldBeACTIVE = false;
								{ auto const SELECT_CASE_var( PlantLoop( LoopNum ).LoopSide( SideNum ).Branch( BranchNum ).Comp( CompNum ).TypeOf_Num );

								if ( SELECT_CASE_var == TypeOf_WtrHeaterMixed ) {
									ShouldBeACTIVE = true;
								} else if ( SELECT_CASE_var == TypeOf_WtrHeaterStratified ) {
									ShouldBeACTIVE = true;
								} else if ( SELECT_CASE_var == TypeOf_WaterUseConnection ) {
									ShouldBeACTIVE = true;
								} else if ( SELECT_CASE_var == TypeOf_CoilWaterCooling ) {
									ShouldBeACTIVE = true;
								} else if ( SELECT_CASE_var == TypeOf_CoilWaterDetailedFlatCooling ) {
									ShouldBeACTIVE = true;
								} else if ( SELECT_CASE_var == TypeOf_CoilWaterSimpleHeating ) {
									ShouldBeACTIVE = true;
								} else if ( SELECT_CASE_var == TypeOf_CoilSteamAirHeating ) {
									ShouldBeACTIVE = true;
								} else if ( SELECT_CASE_var == TypeOf_SolarCollectorFlatPlate ) {
									ShouldBeACTIVE = true;
								} else if ( SELECT_CASE_var == TypeOf_PlantLoadProfile ) {
									ShouldBeACTIVE = true;
								} else {
									// not a demand side component that we know needs to be active, do nothing

								}}

								if ( ShouldBeACTIVE ) {
									{ auto const SELECT_CASE_var( PlantLoop( LoopNum ).LoopSide( SideNum ).Branch( BranchNum ).ControlType );

									if ( SELECT_CASE_var == ControlType_Unknown ) {
										ShowWarningError( "Found potential problem with Control Type for Branch named: " + PlantLoop( LoopNum ).LoopSide( SideNum ).Branch( BranchNum ).Name );
										ShowContinueError( "This branch should (probably) be ACTIVE but has control type unknown" );
									} else if ( SELECT_CASE_var == ControlType_Active ) {
										// do nothing, this is correct control type.
									} else if ( SELECT_CASE_var == ControlType_Passive ) {
										ShowWarningError( "Found potential problem with Control Type for Branch named: " + PlantLoop( LoopNum ).LoopSide( SideNum ).Branch( BranchNum ).Name );
										ShowContinueError( "This branch should (probably) be ACTIVE but has control type PASSIVE" );
									} else if ( SELECT_CASE_var == ControlType_SeriesActive ) {
										// do nothing, should be okay. (? don't really understand SeriesActive though)
									} else if ( SELECT_CASE_var == ControlType_Bypass ) {
										ShowWarningError( "Found potential problem with Control Type for Branch named: " + PlantLoop( LoopNum ).LoopSide( SideNum ).Branch( BranchNum ).Name );
										ShowContinueError( "This branch should (probably) be ACTIVE but has control type Bypass" );
									}}
								} // should be active
							} //comp num loop
						} // demand side

					} // splitter outlet nodes
				} // splitters
				//check to see if bypass exists in demand side. If not warn error of possible flow problems
				if ( ! PlantLoop( LoopNum ).LoopSide( SideNum ).BypassExists ) {
					if ( SideNum == DemandSide ) {
						ShowWarningError( "There is no BYPASS component in the demand-side of PlantLoop =" + PlantLoop( LoopNum ).Name );
						ShowContinueError( "You may be able to fix the fatal error above by adding a demand-side BYPASS PIPE." );
					}
				}
			} // loop sides
		} // plant loops

	}

	//SUBROUTINE CheckPlantLoopData

	//          ! SUBROUTINE INFORMATION:
	//          !       AUTHOR         B. Griffith
	//          !       DATE WRITTEN   May 2008
	//          !       MODIFIED       na
	//          !       RE-ENGINEERED  na

	//          ! PURPOSE OF THIS SUBROUTINE:
	//          ! This routine checks plant loop for input problems early in the simulation
	//          ! Some of the same checks also occur in CheckPlantOnAbort but those only execute if aborted
	//          ! Additional plant loop input checks can be added here.

	//          ! METHODOLOGY EMPLOYED:
	//          ! Test plant loop data for know issues.
	//          !  1. CR 7431.  detect presence of water coils and check for "ACTIVE" branch control.

	//          ! REFERENCES:
	//          ! na

	//          ! USE STATEMENTS:
	//          ! na

	//  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

	//          ! SUBROUTINE ARGUMENT DEFINITIONS:
	//          ! na

	//          ! SUBROUTINE PARAMETER DEFINITIONS:
	//          ! na

	//          ! INTERFACE BLOCK SPECIFICATIONS:
	//          ! na

	//          ! DERIVED TYPE DEFINITIONS:
	//          ! na

	//          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
	//  LOGICAL :: ShouldBeACTIVE
	//  INTEGER :: SideNum
	//  INTEGER :: numLoopSides
	//unused-1208  INTEGER :: SplitNum
	//  INTEGER :: BranchNum  ! DO loop counter for branches
	//  INTEGER :: CompNum    ! do loop for multiple components on a branch
	//  INTEGER :: LoopNum    ! DO loop counter for loops

	//  IF (.not. (TotNumLoops  > 0)) RETURN
	//  IF (.not.(ALLOCATED(PlantLoop))) RETURN

	//  DO LoopNum = 1, TotNumLoops
	//    numLoopSides = 2
	//    DO SideNum = 1, numLoopSides
	//      DO BranchNum =1, PlantLoop(LoopNum)%LoopSide(SideNum)%TotalBranches
	//        DO CompNum= 1,  PlantLoop(LoopNum)%LoopSide(SideNum)%Branch(BranchNum)%TotalComponents
	//          ShouldBeACTIVE = .FALSE.

	//          SELECT CASE (PlantLoop(LoopNum)%LoopSide(SideNum)%Branch(BranchNum)%Comp(CompNum)%TypeOf_Num)
	//          ! for now, check that all water coils are on "active" branch.
	//          CASE (TypeOf_WaterUseConnection)
	//            ShouldBeACTIVE = .TRUE.
	//          CASE (TypeOf_CoilWaterCooling)
	//            ShouldBeACTIVE = .TRUE.
	//          CASE (TypeOf_CoilWaterDetailedFlatCooling)
	//            ShouldBeACTIVE = .TRUE.
	//          CASE (TypeOf_CoilWaterSimpleHeating)
	//            ShouldBeACTIVE = .TRUE.
	//          CASE (TypeOf_CoilSteamAirHeating)
	//            ShouldBeACTIVE = .TRUE.

	//          CASE DEFAULT

	//          END SELECT

	//          If (ShouldBeACTIVE) THEN
	//            SELECT CASE (PlantLoop(LoopNum)%LoopSide(SideNum)%Branch(BranchNum)%Comp(CompNum)%FlowCtrl)

	//            CASE (ControlType_Unknown)
	//               CALL ShowWarningError('Found potential problem with Control Type for Branch named: '&
	//                             //TRIM(PlantLoop(LoopNum)%LoopSide(SideNum)%Branch(BranchNum)%Name) )
	//                             !DSU3 note, this confuses branch and components, should have reported out comp name as well.
	//               CALL ShowContinueError('This branch should (probably) be ACTIVE but has control type unknown')
	//            CASE (ControlType_Active)
	//              ! do nothing, this is correct control type.
	//            CASE (ControlType_Passive)
	//               CALL ShowSevereError('Found problem with Control Type for Branch named: '&
	//                             //TRIM(PlantLoop(LoopNum)%LoopSide(SideNum)%Branch(BranchNum)%Name) )
	//               CALL ShowContinueError('This branch should be ACTIVE but has control type PASSIVE')
	//            CASE (ControlType_SeriesActive)
	//              ! do nothing, should be okay. (? don't really understand SeriesActive though)
	//            CASE (ControlType_Bypass)
	//               CALL ShowSevereError('Found problem with Control Type for Branch named: '&
	//                             //TRIM(PlantLoop(LoopNum)%LoopSide(SideNum)%Branch(BranchNum)%Name) )
	//               CALL ShowContinueError('This branch should be ACTIVE but has control type Bypass')
	//            END SELECT
	//          ENDIF ! should be active
	//        ENDDO !comp num loop
	//      ENDDO ! branches
	//    ENDDO ! loop sides
	//  ENDDO ! plant loops

	//  RETURN

	//END SUBROUTINE CheckPlantLoopData

	void
	InitOneTimePlantSizingInfo( int const LoopNum ) // loop being initialized for sizing
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   April 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// one time init what can be set up related to plant sizing data structure.

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataSizing::NumPltSizInput;
		using DataSizing::PlantSizData;
		using DataSizing::PlantSizingData;
		using InputProcessor::FindItemInList;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int PlantSizNum( 0 ); // index of Plant Sizing data for this loop

		if ( PlantLoop( LoopNum ).PlantSizNum == 0 ) {
			if ( NumPltSizInput > 0 ) {
				PlantSizNum = FindItemInList( PlantLoop( LoopNum ).Name, PlantSizData, &PlantSizingData::PlantLoopName );
				if ( PlantSizNum > 0 ) {
					PlantLoop( LoopNum ).PlantSizNum = PlantSizNum;
				}
			}
		}

	}

	void
	SizePlantLoop(
		int const LoopNum, // Supply side loop being simulated
		bool const OkayToFinish
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   December 2001
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for sizing the supply side of Plant Loops for which loop flow rates
		// have not been specified in the input.

		// METHODOLOGY EMPLOYED:
		// Obtains volumetric flow rate data from the PlantSizData array..

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataSizing;
		using InputProcessor::FindItemInList;
		using General::RoundSigDigits;
		using PlantLoopEquip::SimPlantEquip;
		using FluidProperties::GetDensityGlycol;
		using ReportSizingManager::ReportSizingOutput;

		// Locals
		bool InitLoopEquip( true );

		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "SizePlantLoop" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int PlantSizNum( 0 ); // index of Plant Sizing data for this loop
		int BranchNum; // DO loop counter for cycling through branches on a demand side loop
		int CompNum; // DO loop counter for cycling through components on a demand side loop
		int SupNodeNum; // component inlet water node number
		int WaterCompNum; // DO loop counter for cycling through all the components that demand water
		bool ErrorsFound( false ); // If errors detected in input
		// bool SimNestedLoop( false );
		bool ReSize( false );
		bool AllSizFac( true );
		Real64 LoopSizFac( 0.0 );
		Real64 AvLoopSizFac;
		Real64 PlantSizFac( 1.0 );
		Real64 MaxSizFac( 0.0 );
		Real64 BranchSizFac;
		Real64 NumBrSizFac( 0.0 );
		Real64 FluidDensity( 0.0 ); // local value from glycol routine
		bool Finalize( OkayToFinish );

		if ( PlantLoop( LoopNum ).PlantSizNum > 0 ) {
			ReSize = true;
			PlantSizNum = PlantLoop( LoopNum ).PlantSizNum;
			// PlantSizData(PlantSizNum)%DesVolFlowRate = 0.0D0 ! DSU2
		} else {
			if ( NumPltSizInput > 0 ) {
				PlantSizNum = FindItemInList( PlantLoop( LoopNum ).Name, PlantSizData, &PlantSizingData::PlantLoopName );
			}
		}
		PlantLoop( LoopNum ).PlantSizNum = PlantSizNum;
		// calculate a loop sizing factor and a branch sizing factor. Note that components without a sizing factor
		// are assigned sizing factors of zero in this calculation
		if ( PlantSizNum > 0 ) {
			if ( GetCompSizFac ) {
				for ( BranchNum = 1; BranchNum <= PlantLoop( LoopNum ).LoopSide( SupplySide ).TotalBranches; ++BranchNum ) {
					BranchSizFac = 0.0;
					PlantLoop( LoopNum ).LoopSide( SupplySide ).Branch( BranchNum ).PumpSizFac = 1.0;
					if ( PlantLoop( LoopNum ).LoopSide( SupplySide ).NodeNumIn == PlantLoop( LoopNum ).LoopSide( SupplySide ).Branch( BranchNum ).NodeNumIn ) continue;
					if ( PlantLoop( LoopNum ).LoopSide( SupplySide ).NodeNumOut == PlantLoop( LoopNum ).LoopSide( SupplySide ).Branch( BranchNum ).NodeNumOut ) continue;
					for ( CompNum = 1; CompNum <= PlantLoop( LoopNum ).LoopSide( SupplySide ).Branch( BranchNum ).TotalComponents; ++CompNum ) {
						SimPlantEquip( LoopNum, SupplySide, BranchNum, CompNum, true, InitLoopEquip, GetCompSizFac );
						BranchSizFac = max( BranchSizFac, PlantLoop( LoopNum ).LoopSide( SupplySide ).Branch( BranchNum ).Comp( CompNum ).SizFac );
					}
					LoopSizFac += BranchSizFac;
					MaxSizFac = max( MaxSizFac, BranchSizFac );
					if ( BranchSizFac > 0.0 ) {
						PlantLoop( LoopNum ).LoopSide( SupplySide ).Branch( BranchNum ).PumpSizFac = BranchSizFac;
						++NumBrSizFac;
					} else {
						AllSizFac = false;
					}
				}
				AvLoopSizFac = LoopSizFac / max( 1.0, NumBrSizFac );

				if ( AvLoopSizFac > 0.0 && AvLoopSizFac < 1.0 ) {
					PlantSizFac = LoopSizFac;
				} else if ( AvLoopSizFac > 1.0 ) {
					PlantSizFac = MaxSizFac;
				} else {
					PlantSizFac = 1.0;
				}
				// store the sizing factor now, for later reuse,
				PlantSizData( PlantSizNum ).PlantSizFac = PlantSizFac;
				// might deprecate this next bit in favor of simpler storage in PlantSizData structure
				for ( BranchNum = 1; BranchNum <= PlantLoop( LoopNum ).LoopSide( SupplySide ).TotalBranches; ++BranchNum ) {
					if ( PlantLoop( LoopNum ).LoopSide( SupplySide ).NodeNumIn == PlantLoop( LoopNum ).LoopSide( SupplySide ).Branch( BranchNum ).NodeNumIn ) {
						PlantLoop( LoopNum ).LoopSide( SupplySide ).Branch( BranchNum ).PumpSizFac = PlantSizFac;
					}
					if ( PlantLoop( LoopNum ).LoopSide( SupplySide ).NodeNumOut == PlantLoop( LoopNum ).LoopSide( SupplySide ).Branch( BranchNum ).NodeNumOut ) {
						PlantLoop( LoopNum ).LoopSide( SupplySide ).Branch( BranchNum ).PumpSizFac = PlantSizFac;
					}
				}

			} else {
				// fill PlantSizFac from data structure
				for ( BranchNum = 1; BranchNum <= PlantLoop( LoopNum ).LoopSide( SupplySide ).TotalBranches; ++BranchNum ) {
					if ( PlantLoop( LoopNum ).LoopSide( SupplySide ).NodeNumIn == PlantLoop( LoopNum ).LoopSide( SupplySide ).Branch( BranchNum ).NodeNumIn ) {
						PlantSizFac = PlantLoop( LoopNum ).LoopSide( SupplySide ).Branch( BranchNum ).PumpSizFac;
						break;
					}
				}
			}

			// sum up contributions from CompDesWaterFlow, demand side size request (non-coincident)
			PlantSizData( PlantSizNum ).DesVolFlowRate = 0.0; // init for summation
			for ( BranchNum = 1; BranchNum <= PlantLoop( LoopNum ).LoopSide( DemandSide ).TotalBranches; ++BranchNum ) {
				for ( CompNum = 1; CompNum <= PlantLoop( LoopNum ).LoopSide( DemandSide ).Branch( BranchNum ).TotalComponents; ++CompNum ) {
					SupNodeNum = PlantLoop( LoopNum ).LoopSide( DemandSide ).Branch( BranchNum ).Comp( CompNum ).NodeNumIn;
					for ( WaterCompNum = 1; WaterCompNum <= SaveNumPlantComps; ++WaterCompNum ) {
						if ( SupNodeNum == CompDesWaterFlow( WaterCompNum ).SupNode ) {
							PlantSizData( PlantSizNum ).DesVolFlowRate += CompDesWaterFlow( WaterCompNum ).DesVolFlowRate;
						}
					}
				}
			}

			if ( ! PlantLoop( LoopNum ).MaxVolFlowRateWasAutoSized && ( PlantLoop( LoopNum ).MaxVolFlowRate > 0.0 ) ) {
					// if the user puts in a large throwaway value for hard max plant loop size, they may not want this affecting anything else.
					//  but if they put in a smaller value, then it should cap the design size, so use hard value if it is smaller than non-coincident result
					PlantSizData( PlantSizNum ).DesVolFlowRate = std::min( PlantSizData( PlantSizNum ).DesVolFlowRate, PlantLoop( LoopNum ).MaxVolFlowRate );
			}

		}

		if ( PlantLoop( LoopNum ).MaxVolFlowRateWasAutoSized ) {

			if ( ( PlantSizNum > 0 ) ) {

					if ( PlantSizData( PlantSizNum ).DesVolFlowRate >= SmallWaterVolFlow ) {
						PlantLoop( LoopNum ).MaxVolFlowRate = PlantSizData( PlantSizNum ).DesVolFlowRate * PlantSizData( PlantSizNum ).PlantSizFac;
					} else {
						PlantLoop( LoopNum ).MaxVolFlowRate = 0.0;
						if ( PlantFinalSizesOkayToReport ) {
							ShowWarningError( "SizePlantLoop: Calculated Plant Sizing Design Volume Flow Rate=["
								+ RoundSigDigits( PlantSizData( PlantSizNum ).DesVolFlowRate, 2 ) + "] is too small. Set to 0.0" );
							ShowContinueError( "..occurs for PlantLoop=" + PlantLoop( LoopNum ).Name );
						}
					}
					if ( Finalize ) {
						if ( PlantFinalSizesOkayToReport ) {
							if ( PlantLoop( LoopNum ).TypeOfLoop == LoopType_Plant ) {
								ReportSizingOutput( "PlantLoop", PlantLoop( LoopNum ).Name,
									"Maximum Loop Flow Rate [m3/s]", PlantLoop( LoopNum ).MaxVolFlowRate );
							} else if ( PlantLoop( LoopNum ).TypeOfLoop == LoopType_Condenser ) {
								ReportSizingOutput( "CondenserLoop", PlantLoop( LoopNum ).Name,
									"Maximum Loop Flow Rate [m3/s]", PlantLoop( LoopNum ).MaxVolFlowRate );
							}
						}
						if ( PlantFirstSizesOkayToReport ) {
							if ( PlantLoop( LoopNum ).TypeOfLoop == LoopType_Plant ) {
								ReportSizingOutput( "PlantLoop", PlantLoop( LoopNum ).Name,
									"Initial Maximum Loop Flow Rate [m3/s]", PlantLoop( LoopNum ).MaxVolFlowRate );
							} else if ( PlantLoop( LoopNum ).TypeOfLoop == LoopType_Condenser ) {
								ReportSizingOutput( "CondenserLoop", PlantLoop( LoopNum ).Name,
									"Initial Maximum Loop Flow Rate [m3/s]", PlantLoop( LoopNum ).MaxVolFlowRate );
							}
						}
					}

			} else {
				if (PlantFirstSizesOkayToFinalize) {
					ShowFatalError( "Autosizing of plant loop requires a loop Sizing:Plant object" );
					ShowContinueError( "Occurs in PlantLoop object=" + PlantLoop( LoopNum ).Name );
					ErrorsFound = true;
				}
			}

		}

		// Small loop mass no longer introduces instability. Checks and warnings removed by SJR 20 July 2007.
		if ( PlantLoop( LoopNum ).VolumeWasAutoSized ) {
			// Although there is no longer a stability requirement (mass can be zero), autosizing is formulated the same way.
			PlantLoop( LoopNum ).Volume = PlantLoop( LoopNum ).MaxVolFlowRate * TimeStepZone * SecInHour / 0.8;
			if (PlantFinalSizesOkayToReport) {
				if ( PlantLoop( LoopNum ).TypeOfLoop == LoopType_Plant ) {
					// condenser loop vs plant loop breakout needed.
					ReportSizingOutput( "PlantLoop", PlantLoop( LoopNum ).Name,
					"Plant Loop Volume [m3]", PlantLoop( LoopNum ).Volume );
				} else if ( PlantLoop( LoopNum ).TypeOfLoop == LoopType_Condenser ) {
					ReportSizingOutput( "CondenserLoop", PlantLoop( LoopNum ).Name,
					"Condenser Loop Volume [m3]", PlantLoop( LoopNum ).Volume );
				}
			}
			if (PlantFirstSizesOkayToReport) {
				if ( PlantLoop( LoopNum ).TypeOfLoop == LoopType_Plant ) {
					// condenser loop vs plant loop breakout needed.
					ReportSizingOutput( "PlantLoop", PlantLoop( LoopNum ).Name,
					"Initial Plant Loop Volume [m3]", PlantLoop( LoopNum ).Volume );
				} else if ( PlantLoop( LoopNum ).TypeOfLoop == LoopType_Condenser ) {
					ReportSizingOutput( "CondenserLoop", PlantLoop( LoopNum ).Name,
					"Initial Condenser Loop Volume [m3]", PlantLoop( LoopNum ).Volume );
				}
			}
		}

		//should now have plant volume, calculate plant volume's mass for fluid type
		if ( PlantLoop( LoopNum ).FluidType == NodeType_Water ) {
			FluidDensity = GetDensityGlycol( PlantLoop( LoopNum ).FluidName, InitConvTemp, PlantLoop( LoopNum ).FluidIndex, RoutineName );
		} else if ( PlantLoop( LoopNum ).FluidType == NodeType_Steam ) {
			FluidDensity = GetSatDensityRefrig( fluidNameSteam, 100.0, 1.0, PlantLoop( LoopNum ).FluidIndex, RoutineName );
		} else {
			assert( false );
		}

		PlantLoop( LoopNum ).Mass = PlantLoop( LoopNum ).Volume * FluidDensity;

		PlantLoop( LoopNum ).MaxMassFlowRate = PlantLoop( LoopNum ).MaxVolFlowRate * FluidDensity;
		PlantLoop( LoopNum ).MinMassFlowRate = PlantLoop( LoopNum ).MinVolFlowRate * FluidDensity;

		if ( ErrorsFound ) {
			ShowFatalError( "Preceding sizing errors cause program termination" );
		}


	}


	void
	ResizePlantLoopLevelSizes(
		int const LoopNum // Supply side loop being simulated
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Jan 2015
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is for redon the sizing of plant loops to support HVAC Sizing Simulation

		// METHODOLOGY EMPLOYED:
		// Obtains volumetric flow rate data from the PlantSizData array..

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataSizing;
		using InputProcessor::FindItemInList;
		using General::RoundSigDigits;
		using PlantLoopEquip::SimPlantEquip;
		using FluidProperties::GetDensityGlycol;
		using ReportSizingManager::ReportSizingOutput;
		using DataPlant::PlantLoop;

		// Locals
		// bool InitLoopEquip( true );

		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "ResizePlantLoop" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int PlantSizNum( 0 ); // index of Plant Sizing data for this loop
		int BranchNum; // DO loop counter for cycling through branches on a demand side loop
		int CompNum; // DO loop counter for cycling through components on a demand side loop
		int SupNodeNum; // component inlet water node number
		int WaterCompNum; // DO loop counter for cycling through all the components that demand water
		bool ErrorsFound( false ); // If errors detected in input
		// bool SimNestedLoop( false );
		bool ReSize;

		Real64 FluidDensity( 0.0 ); // local value from glycol routine

		Real64 PlantSizeFac;


		ReSize = false;

		PlantSizNum = PlantLoop( LoopNum ).PlantSizNum;

		// fill PlantSizFac from data structure
		for ( BranchNum = 1; BranchNum <= PlantLoop( LoopNum ).LoopSide( SupplySide ).TotalBranches; ++BranchNum ) {
			if ( PlantLoop( LoopNum ).LoopSide( SupplySide ).NodeNumIn == PlantLoop( LoopNum ).LoopSide( SupplySide ).Branch( BranchNum ).NodeNumIn ) {
				PlantSizeFac = PlantLoop( LoopNum ).LoopSide( SupplySide ).Branch( BranchNum ).PumpSizFac;
				break;
			}
		}
		if (PlantSizData( PlantSizNum ).ConcurrenceOption == NonCoincident) {
		// we can have plant loops that are non-coincident along with some that are coincident
		// so refresh sum of registered flows (they may have changed)

			PlantSizData( PlantSizNum ).DesVolFlowRate = 0.0; // init for summation
			for ( BranchNum = 1; BranchNum <= PlantLoop( LoopNum ).LoopSide( DemandSide ).TotalBranches; ++BranchNum ) {
				for ( CompNum = 1; CompNum <= PlantLoop( LoopNum ).LoopSide( DemandSide ).Branch( BranchNum ).TotalComponents; ++CompNum ) {
					SupNodeNum = PlantLoop( LoopNum ).LoopSide( DemandSide ).Branch( BranchNum ).Comp( CompNum ).NodeNumIn;
					for ( WaterCompNum = 1; WaterCompNum <= SaveNumPlantComps; ++WaterCompNum ) {
						if ( SupNodeNum == CompDesWaterFlow( WaterCompNum ).SupNode ) {
							PlantSizData( PlantSizNum ).DesVolFlowRate += CompDesWaterFlow( WaterCompNum ).DesVolFlowRate;
						}
					}
				}
			}

		}

		if ( PlantLoop( LoopNum ).MaxVolFlowRateWasAutoSized ) {

			if ( ( PlantSizNum > 0 ) ) {

					if ( PlantSizData( PlantSizNum ).DesVolFlowRate >= SmallWaterVolFlow ) {
						PlantLoop( LoopNum ).MaxVolFlowRate = PlantSizData( PlantSizNum ).DesVolFlowRate * PlantSizeFac;
					} else {
						PlantLoop( LoopNum ).MaxVolFlowRate = 0.0;
						if ( PlantFinalSizesOkayToReport ) {
							ShowWarningError( "SizePlantLoop: Calculated Plant Sizing Design Volume Flow Rate=["
								+ RoundSigDigits( PlantSizData( PlantSizNum ).DesVolFlowRate, 2 ) + "] is too small. Set to 0.0" );
							ShowContinueError( "..occurs for PlantLoop=" + PlantLoop( LoopNum ).Name );
						}
					}
					if ( PlantFinalSizesOkayToReport ) {
						if ( PlantLoop( LoopNum ).TypeOfLoop == LoopType_Plant ) {
							ReportSizingOutput( "PlantLoop", PlantLoop( LoopNum ).Name,
								"Maximum Loop Flow Rate [m3/s]", PlantLoop( LoopNum ).MaxVolFlowRate );
						} else if ( PlantLoop( LoopNum ).TypeOfLoop == LoopType_Condenser ) {
							ReportSizingOutput( "CondenserLoop", PlantLoop( LoopNum ).Name,
								"Maximum Loop Flow Rate [m3/s]", PlantLoop( LoopNum ).MaxVolFlowRate );

						}
					}
				}
		}


		// Small loop mass no longer introduces instability. Checks and warnings removed by SJR 20 July 2007.
		if ( PlantLoop( LoopNum ).VolumeWasAutoSized ) {
			// Although there is no longer a stability requirement (mass can be zero), autosizing is formulated the same way.
			PlantLoop( LoopNum ).Volume = PlantLoop( LoopNum ).MaxVolFlowRate * TimeStepZoneSec / 0.8;
			if ( PlantLoop( LoopNum ).TypeOfLoop == LoopType_Plant ) {
				// condenser loop vs plant loop breakout needed.
				ReportSizingOutput( "PlantLoop", PlantLoop( LoopNum ).Name, "Plant Loop Volume [m3]", PlantLoop( LoopNum ).Volume );
			} else if ( PlantLoop( LoopNum ).TypeOfLoop == LoopType_Condenser ) {
				ReportSizingOutput( "CondenserLoop", PlantLoop( LoopNum ).Name, "Condenser Loop Volume [m3]", PlantLoop( LoopNum ).Volume );
			}
		}

		//should now have plant volume, calculate plant volume's mass for fluid type
		if ( PlantLoop( LoopNum ).FluidType == NodeType_Water ) {
			FluidDensity = GetDensityGlycol( PlantLoop( LoopNum ).FluidName, InitConvTemp, PlantLoop( LoopNum ).FluidIndex, RoutineName );
		} else if ( PlantLoop( LoopNum ).FluidType == NodeType_Steam ) {
			FluidDensity = GetSatDensityRefrig( fluidNameSteam, 100.0, 1.0, PlantLoop( LoopNum ).FluidIndex, RoutineName );
		} else {
			assert( false );
		}

		PlantLoop( LoopNum ).Mass = PlantLoop( LoopNum ).Volume * FluidDensity;

		PlantLoop( LoopNum ).MaxMassFlowRate = PlantLoop( LoopNum ).MaxVolFlowRate * FluidDensity;
		PlantLoop( LoopNum ).MinMassFlowRate = PlantLoop( LoopNum ).MinVolFlowRate * FluidDensity;

		if ( ErrorsFound ) {
			ShowFatalError( "Preceding sizing errors cause program termination" );
		}


	}

	void
	SetupInitialPlantCallingOrder()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Feb 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// setup the order that plant loops are to be called

		// METHODOLOGY EMPLOYED:
		// simple rule-based allocation of which order to call the half loops
		//  initially just mimicing historical practice until a better set of rules is
		// developed
		// 1.  first call all plant demand sides
		// 2.  second call all plant supply sides
		// 3.  third call all condenser demand sides
		// 4.  fourth call all condenser supply sides

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int OrderIndex; // local
		int I; // local loop

		TotNumHalfLoops = 2 * TotNumLoops;

		if ( TotNumHalfLoops <= 0 ) return;

		// first allocate to total number of plant half loops

		if ( ! allocated( PlantCallingOrderInfo ) ) PlantCallingOrderInfo.allocate( TotNumHalfLoops );

		// set plant loop demand sides
		for ( I = 1; I <= NumPlantLoops; ++I ) {
			PlantCallingOrderInfo( I ).LoopIndex = I;
			PlantCallingOrderInfo( I ).LoopSide = DemandSide;
		}

		// set plant loop supply sides
		for ( I = 1; I <= NumPlantLoops; ++I ) {
			OrderIndex = I + NumPlantLoops;
			PlantCallingOrderInfo( OrderIndex ).LoopIndex = I;
			PlantCallingOrderInfo( OrderIndex ).LoopSide = SupplySide;
		}

		// set condenser Loop demand sides
		for ( I = 1; I <= NumCondLoops; ++I ) {
			OrderIndex = 2 * NumPlantLoops + I;
			PlantCallingOrderInfo( OrderIndex ).LoopIndex = NumPlantLoops + I;
			PlantCallingOrderInfo( OrderIndex ).LoopSide = DemandSide;
		}

		// set condenser Loop supply sides
		for ( I = 1; I <= NumCondLoops; ++I ) {
			OrderIndex = 2 * NumPlantLoops + NumCondLoops + I;
			PlantCallingOrderInfo( OrderIndex ).LoopIndex = NumPlantLoops + I;
			PlantCallingOrderInfo( OrderIndex ).LoopSide = SupplySide;
		}

		// legacy one-time calling control stuff moved here from manager routine, hopefully remove
		if ( ! allocated( LoadChangeDownStream ) ) LoadChangeDownStream.allocate( TotNumLoops );

	}

	void
	RevisePlantCallingOrder()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   april 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// setup the order that plant loops are to be called

		// METHODOLOGY EMPLOYED:
		// simple rule-based allocation of which order to call the half loops
		// Examine for interconnected components and rearrange to impose the following rules

		// REFERENCES:
		// na

		// Using/Aliasing
		using PlantUtilities::ShiftPlantLoopSideCallingOrder;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int HalfLoopNum;
		int LoopNum;
		int LoopSideNum;
		int OtherLoopNum;
		int OtherLoopSideNum;
		static int OtherLoopCallingIndex( 0 );
		static int OtherLoopDemandSideCallingIndex( 0 );
		static int NewOtherDemandSideCallingIndex( 0 );
		static int newCallingIndex( 0 );
		bool thisLoopPutsDemandOnAnother;
		int ConnctNum;

		for ( HalfLoopNum = 1; HalfLoopNum <= TotNumHalfLoops; ++HalfLoopNum ) {

			LoopNum = PlantCallingOrderInfo( HalfLoopNum ).LoopIndex;
			LoopSideNum = PlantCallingOrderInfo( HalfLoopNum ).LoopSide;

			if ( allocated( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Connected ) ) {
				for ( ConnctNum = 1; ConnctNum <= isize( PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Connected ); ++ConnctNum ) {
					OtherLoopNum = PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Connected( ConnctNum ).LoopNum;
					OtherLoopSideNum = PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Connected( ConnctNum ).LoopSideNum;
					OtherLoopCallingIndex = FindLoopSideInCallingOrder( OtherLoopNum, OtherLoopSideNum );

					thisLoopPutsDemandOnAnother = PlantLoop( LoopNum ).LoopSide( LoopSideNum ).Connected( ConnctNum ).LoopDemandsOnRemote;
					if ( thisLoopPutsDemandOnAnother ) { // make sure this loop side is called before the other loop side
						if ( OtherLoopCallingIndex < HalfLoopNum ) { //rearrange
							newCallingIndex = min( HalfLoopNum + 1, TotNumHalfLoops );
							ShiftPlantLoopSideCallingOrder( OtherLoopCallingIndex, newCallingIndex );
						}

					} else { // make sure the other is called before this one
						if ( OtherLoopCallingIndex > HalfLoopNum ) { //rearrange
							newCallingIndex = max( HalfLoopNum, 1 );

							if ( OtherLoopSideNum == SupplySide ) { //if this is a supplyside, don't push it before its own demand side
								OtherLoopDemandSideCallingIndex = FindLoopSideInCallingOrder( OtherLoopNum, DemandSide );
								if ( OtherLoopDemandSideCallingIndex < HalfLoopNum ) { // good to go
									newCallingIndex = min( OtherLoopDemandSideCallingIndex + 1, TotNumHalfLoops ); //put it right after its demand side
									ShiftPlantLoopSideCallingOrder( OtherLoopCallingIndex, newCallingIndex );
								} else { // move both sides of other loop before this, keeping demand side in front
									NewOtherDemandSideCallingIndex = max( HalfLoopNum, 1 );
									ShiftPlantLoopSideCallingOrder( OtherLoopDemandSideCallingIndex, NewOtherDemandSideCallingIndex );
									// get fresh pointer after it has changed in previous call
									OtherLoopCallingIndex = FindLoopSideInCallingOrder( OtherLoopNum, OtherLoopSideNum );
									newCallingIndex = NewOtherDemandSideCallingIndex + 1;
									ShiftPlantLoopSideCallingOrder( OtherLoopCallingIndex, newCallingIndex );
								}
							} else {
								ShiftPlantLoopSideCallingOrder( OtherLoopCallingIndex, newCallingIndex );
							}
						}
					}

				}
			}

		}

	}

	int
	FindLoopSideInCallingOrder(
		int const LoopNum,
		int const LoopSide
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   April 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// locate loop and loop side in calling order structure

		// METHODOLOGY EMPLOYED:
		// returns integer "pointer" index to calling order structure

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		int CallingIndex;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int HalfLoopNum;

		CallingIndex = 0;

		for ( HalfLoopNum = 1; HalfLoopNum <= TotNumHalfLoops; ++HalfLoopNum ) {
			if ( ( LoopNum == PlantCallingOrderInfo( HalfLoopNum ).LoopIndex ) && ( LoopSide == PlantCallingOrderInfo( HalfLoopNum ).LoopSide ) ) {

				CallingIndex = HalfLoopNum;

			}
		}
		return CallingIndex;
	}

	void
	StoreAPumpOnCurrentTempLoop(
		int const LoopNum,
		int const LoopSideNum,
		int const BranchNum,
		int const CompNum,
		std::string const & PumpName,
		int const PumpOutletNode,
		bool const HasBranchPumps
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   April 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine reallocates the pumps data structure in the LoopSide data structure
		//  and adds the pump data passed in as the next pumpe

		// METHODOLOGY EMPLOYED:
		//Fills the following location items in the pump data structure which resides on the LoopSide
		// TYPE LoopSidePumpInformation
		//   CHARACTER(len=MaxNameLength)     :: PumpName              = ' '
		//   INTEGER                          :: PumpTypeOf            = 0
		//   INTEGER                          :: BranchNum             = 0
		//   INTEGER                          :: CompNum               = 0
		//   ...

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataPlant::LoopSidePumpInformation; // , SimPlantEquipTypes
		// USE InputProcessor, ONLY: FindItemInList

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// Object Data

		auto & loop_side( PlantLoop( LoopNum ).LoopSide( LoopSideNum ) );
		auto & pumps( loop_side.Pumps );
		int const nPumpsAfterIncrement = loop_side.TotalPumps = pumps.size() + 1;
		pumps.redimension( nPumpsAfterIncrement );
		pumps( nPumpsAfterIncrement ).PumpName = PumpName;
		// pumps( nPumpsAfterIncrement ).PumpTypeOf = FindItemInList( PumpType, SimPlantEquipTypes );
		pumps( nPumpsAfterIncrement ).BranchNum = BranchNum;
		pumps( nPumpsAfterIncrement ).CompNum = CompNum;
		pumps( nPumpsAfterIncrement ).PumpOutletNode = PumpOutletNode;
		loop_side.BranchPumpsExist = HasBranchPumps;
	}

	void
	SetupBranchControlTypes()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   March 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// set the control types on plant branches using heuristics.
		//  Trying to obsolete branch control type  input

		// METHODOLOGY EMPLOYED:
		// set component control types based on component type
		//  process branches and set branch level control types based on the type of components on them
		//  Rules applied
		//   - Most component models are active
		//   - Pipes are passive unless located between splitter/mixers when assumed to be bypass
		//   - A branch with multiple active components becomes SeriesActive and so do its components

		// REFERENCES:
		// na

		// Using/Aliasing

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

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
		bool BranchIsInSplitterMixer;
		int ComponentFlowCtrl;
		int ActiveCount;
		int BypassCount;
		int NumComponentsOnBranch;
		int NumCount;

		// first set component level control type (obsoletes one input in field set for Branch )
		if ( allocated( PlantLoop ) ) {
			NumCount = size( PlantLoop );
		} else {
			NumCount = 0;
		}
		for ( LoopCtr = 1; LoopCtr <= NumCount; ++LoopCtr ) { //SIZE(PlantLoop)
			for ( LoopSideCtr = DemandSide; LoopSideCtr <= SupplySide; ++LoopSideCtr ) {
				for ( BranchCtr = 1; BranchCtr <= PlantLoop( LoopCtr ).LoopSide( LoopSideCtr ).TotalBranches; ++BranchCtr ) {
					BranchIsInSplitterMixer = false;
					// test if this branch is inside a splitter/mixer
					if ( PlantLoop( LoopCtr ).LoopSide( LoopSideCtr ).SplitterExists ) {
						if ( ( BranchCtr > 1 ) && ( BranchCtr < PlantLoop( LoopCtr ).LoopSide( LoopSideCtr ).TotalBranches ) ) {
							BranchIsInSplitterMixer = true;
						}

					}

					NumComponentsOnBranch = PlantLoop( LoopCtr ).LoopSide( LoopSideCtr ).Branch( BranchCtr ).TotalComponents;

					for ( CompCtr = 1; CompCtr <= isize( PlantLoop( LoopCtr ).LoopSide( LoopSideCtr ).Branch( BranchCtr ).Comp ); ++CompCtr ) {

						auto & this_component( PlantLoop( LoopCtr ).LoopSide( LoopSideCtr ).Branch( BranchCtr ).Comp( CompCtr ) );

						{ auto const SELECT_CASE_var( this_component.TypeOf_Num );

						if ( SELECT_CASE_var == TypeOf_Other ) { //                             = -1
							this_component.FlowCtrl = ControlType_Unknown;
							this_component.FlowPriority = LoopFlowStatus_Unknown;
							this_component.HowLoadServed = HowMet_Unknown;
						} else if ( SELECT_CASE_var == TypeOf_Boiler_Simple ) { //         =  1
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
							this_component.HowLoadServed = HowMet_ByNominalCapHiOutLimit;
						} else if ( SELECT_CASE_var == TypeOf_Boiler_Steam ) { //                      =  2
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
							this_component.HowLoadServed = HowMet_ByNominalCap;
						} else if ( SELECT_CASE_var == TypeOf_Chiller_Absorption ) { //                =  3  ! older BLAST absorption chiller
							this_component.FlowCtrl = ControlType_Active;
							if ( LoopSideCtr == DemandSide ) {
								this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
								this_component.HowLoadServed = HowMet_NoneDemand;
							} else {
								this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
								this_component.HowLoadServed = HowMet_ByNominalCapLowOutLimit;
							}
						} else if ( SELECT_CASE_var == TypeOf_Chiller_Indirect_Absorption ) { //       =  4  ! revised absorption chiller
							this_component.FlowCtrl = ControlType_Active;

							if ( LoopSideCtr == DemandSide ) {
								this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
								this_component.HowLoadServed = HowMet_NoneDemand;
							} else {
								this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
								this_component.HowLoadServed = HowMet_ByNominalCapLowOutLimit;
							}
						} else if ( SELECT_CASE_var == TypeOf_Chiller_CombTurbine ) { //           =  5
							this_component.FlowCtrl = ControlType_Active;
							if ( LoopSideCtr == DemandSide ) {
								this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
								this_component.HowLoadServed = HowMet_NoneDemand;
							} else {
								this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
								this_component.HowLoadServed = HowMet_ByNominalCapLowOutLimit;
							}
						} else if ( SELECT_CASE_var == TypeOf_Chiller_ConstCOP ) { //                 =  6
							this_component.FlowCtrl = ControlType_Active;

							if ( LoopSideCtr == DemandSide ) {
								this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
								this_component.HowLoadServed = HowMet_NoneDemand;
							} else {
								this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
								this_component.HowLoadServed = HowMet_ByNominalCap;
							}
						} else if ( SELECT_CASE_var == TypeOf_Chiller_DFAbsorption ) { //             =  7
							this_component.FlowCtrl = ControlType_Active;
							if ( LoopSideCtr == DemandSide ) {
								this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
								this_component.HowLoadServed = HowMet_NoneDemand;
							} else {
								this_component.FlowPriority = LoopFlowStatus_NeedyIfLoopOn;
								this_component.HowLoadServed = HowMet_ByNominalCapLowOutLimit;
							}
						} else if ( SELECT_CASE_var == TypeOf_Chiller_ExhFiredAbsorption ) { //             =  76
							this_component.FlowCtrl = ControlType_Active;
							if ( LoopSideCtr == DemandSide ) {
								this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
								this_component.HowLoadServed = HowMet_NoneDemand;
							} else {
								this_component.FlowPriority = LoopFlowStatus_NeedyIfLoopOn;
								this_component.HowLoadServed = HowMet_ByNominalCapLowOutLimit;
							}
						} else if ( SELECT_CASE_var == TypeOf_Chiller_Electric ) { //                 =  8
							this_component.FlowCtrl = ControlType_Active;
							if ( LoopSideCtr == DemandSide ) {
								this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
								this_component.HowLoadServed = HowMet_NoneDemand;
							} else {
								this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
								this_component.HowLoadServed = HowMet_ByNominalCapLowOutLimit;
							}
						} else if ( SELECT_CASE_var == TypeOf_Chiller_ElectricEIR ) { //              =  9
							this_component.FlowCtrl = ControlType_Active;
							if ( LoopSideCtr == DemandSide ) {
								this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
								this_component.HowLoadServed = HowMet_NoneDemand;
							} else {
								this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
								this_component.HowLoadServed = HowMet_ByNominalCapLowOutLimit;
							}
						} else if ( SELECT_CASE_var == TypeOf_Chiller_ElectricReformEIR ) { //        = 10
							this_component.FlowCtrl = ControlType_Active;
							if ( LoopSideCtr == DemandSide ) {
								this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
								this_component.HowLoadServed = HowMet_NoneDemand;
							} else {
								this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
								this_component.HowLoadServed = HowMet_ByNominalCapLowOutLimit;
							}
						} else if ( SELECT_CASE_var == TypeOf_Chiller_EngineDriven ) { //             = 11
							this_component.FlowCtrl = ControlType_Active;
							this_component.HowLoadServed = HowMet_ByNominalCapLowOutLimit;
							if ( LoopSideCtr == DemandSide ) {
								this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
								this_component.HowLoadServed = HowMet_NoneDemand;
							} else {
								this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
								this_component.HowLoadServed = HowMet_ByNominalCapLowOutLimit;
							}
						} else if ( SELECT_CASE_var == TypeOf_CoolingTower_SingleSpd ) { //           = 12
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
							this_component.HowLoadServed = HowMet_ByNominalCap;
						} else if ( SELECT_CASE_var == TypeOf_CoolingTower_TwoSpd ) { //              = 13
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
							this_component.HowLoadServed = HowMet_ByNominalCap;
						} else if ( SELECT_CASE_var == TypeOf_CoolingTower_VarSpd ) { //              = 14
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
							this_component.HowLoadServed = HowMet_ByNominalCap;
						} else if ( SELECT_CASE_var == TypeOf_CoolingTower_VarSpdMerkel ) { //              = 89
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
							this_component.HowLoadServed = HowMet_ByNominalCap;
						} else if ( SELECT_CASE_var == TypeOf_Generator_FCExhaust ) { //              = 15
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
							this_component.HowLoadServed = HowMet_PassiveCap;

						} else if ( SELECT_CASE_var == TypeOf_HeatPumpWtrHeaterPumped || SELECT_CASE_var == TypeOf_HeatPumpWtrHeaterWrapped ) { //                = 16, 92
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
							this_component.HowLoadServed = HowMet_PassiveCap;
						} else if ( SELECT_CASE_var == TypeOf_HPWaterEFCooling ) { //                 = 17
							this_component.FlowCtrl = ControlType_Active;
							if ( LoopSideCtr == DemandSide ) {
								this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
								this_component.HowLoadServed = HowMet_NoneDemand;
							} else {
								this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
								this_component.HowLoadServed = HowMet_ByNominalCap;
							}

						} else if ( SELECT_CASE_var == TypeOf_HPWaterEFHeating ) { //                 = 18
							this_component.FlowCtrl = ControlType_Active;
							if ( LoopSideCtr == DemandSide ) {
								this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
								this_component.HowLoadServed = HowMet_NoneDemand;
							} else {
								this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
								this_component.HowLoadServed = HowMet_ByNominalCap;
							}
						} else if ( SELECT_CASE_var == TypeOf_HPWaterPECooling ) { //                 = 19
							this_component.FlowCtrl = ControlType_Active;
							if ( LoopSideCtr == DemandSide ) {
								this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
								this_component.HowLoadServed = HowMet_NoneDemand;
							} else {
								this_component.FlowPriority = LoopFlowStatus_NeedyIfLoopOn;
								this_component.HowLoadServed = HowMet_ByNominalCap;
							}
						} else if ( SELECT_CASE_var == TypeOf_HPWaterPEHeating ) { //                 = 20
							this_component.FlowCtrl = ControlType_Active;
							if ( LoopSideCtr == DemandSide ) {
								this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
								this_component.HowLoadServed = HowMet_NoneDemand;
							} else {
								this_component.FlowPriority = LoopFlowStatus_NeedyIfLoopOn;
								this_component.HowLoadServed = HowMet_ByNominalCap;
							}
						} else if ( SELECT_CASE_var == TypeOf_Pipe ) { //                             = 21
							this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
							this_component.HowLoadServed = HowMet_NoneDemand;
							if ( BranchIsInSplitterMixer ) {
								if ( NumComponentsOnBranch == 1 ) {
									this_component.FlowCtrl = ControlType_Bypass;
								} else if ( NumComponentsOnBranch > 1 ) {
									this_component.FlowCtrl = ControlType_Passive;
								} else {
									this_component.FlowCtrl = ControlType_Bypass;
								}
							} else {
								this_component.FlowCtrl = ControlType_Passive;
							}
						} else if ( SELECT_CASE_var == TypeOf_PipeSteam ) { //                        = 22
							this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
							this_component.HowLoadServed = HowMet_NoneDemand;
							if ( BranchIsInSplitterMixer ) {
								if ( NumComponentsOnBranch == 1 ) {
									this_component.FlowCtrl = ControlType_Bypass;
								} else if ( NumComponentsOnBranch > 1 ) {
									this_component.FlowCtrl = ControlType_Passive;
								} else {
									this_component.FlowCtrl = ControlType_Bypass;
								}
							} else {
								this_component.FlowCtrl = ControlType_Passive;
							}
						} else if ( SELECT_CASE_var == TypeOf_PipeExterior ) { //                     = 23
							this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
							this_component.HowLoadServed = HowMet_NoneDemand;
							if ( BranchIsInSplitterMixer ) {
								if ( NumComponentsOnBranch == 1 ) {
									this_component.FlowCtrl = ControlType_Bypass;
								} else if ( NumComponentsOnBranch > 1 ) {
									this_component.FlowCtrl = ControlType_Passive;
								} else {
									this_component.FlowCtrl = ControlType_Bypass;
								}
							} else {
								this_component.FlowCtrl = ControlType_Passive;
							}
						} else if ( SELECT_CASE_var == TypeOf_PipeInterior ) { //                     = 24
							this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
							this_component.HowLoadServed = HowMet_NoneDemand;
							if ( BranchIsInSplitterMixer ) {
								if ( NumComponentsOnBranch == 1 ) {
									this_component.FlowCtrl = ControlType_Bypass;
								} else if ( NumComponentsOnBranch > 1 ) {
									this_component.FlowCtrl = ControlType_Passive;
								} else {
									this_component.FlowCtrl = ControlType_Bypass;
								}
							} else {
								this_component.FlowCtrl = ControlType_Passive;
							}
						} else if ( SELECT_CASE_var == TypeOf_PipeUnderground ) { //                  = 25
							this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
							this_component.HowLoadServed = HowMet_NoneDemand;
							if ( BranchIsInSplitterMixer ) {
								if ( NumComponentsOnBranch == 1 ) {
									this_component.FlowCtrl = ControlType_Bypass;
								} else if ( NumComponentsOnBranch > 1 ) {
									this_component.FlowCtrl = ControlType_Passive;
								} else {
									this_component.FlowCtrl = ControlType_Bypass;
								}
							} else {
								this_component.FlowCtrl = ControlType_Passive;
							}
						} else if ( SELECT_CASE_var == TypeOf_PurchChilledWater ) { //                = 26
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
							this_component.HowLoadServed = HowMet_ByNominalCapLowOutLimit;
						} else if ( SELECT_CASE_var == TypeOf_PurchHotWater ) { //                    = 27
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
							this_component.HowLoadServed = HowMet_ByNominalCapHiOutLimit;
						} else if ( SELECT_CASE_var == TypeOf_TS_IceDetailed ) { //                   = 28
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_NeedyIfLoopOn;
							this_component.HowLoadServed = HowMet_PassiveCap;
						} else if ( SELECT_CASE_var == TypeOf_TS_IceSimple ) { //                    = 29
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_NeedyIfLoopOn;
							this_component.HowLoadServed = HowMet_PassiveCap;
						} else if ( SELECT_CASE_var == TypeOf_ValveTempering ) { //                  = 30
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_NeedyIfLoopOn;
							this_component.HowLoadServed = HowMet_NoneDemand;
						} else if ( SELECT_CASE_var == TypeOf_WtrHeaterMixed ) { //                   = 31
							if ( LoopSideCtr == DemandSide ) {
								this_component.FlowCtrl = ControlType_Active;
								this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
								this_component.HowLoadServed = HowMet_NoneDemand;
							} else {
								this_component.FlowCtrl = ControlType_Active;
								this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
								this_component.HowLoadServed = HowMet_PassiveCap;
							}
						} else if ( SELECT_CASE_var == TypeOf_WtrHeaterStratified ) { //              = 32
							if ( LoopSideCtr == DemandSide ) {
								this_component.FlowCtrl = ControlType_Active;
								this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
								this_component.HowLoadServed = HowMet_NoneDemand;
							} else {
								this_component.FlowCtrl = ControlType_Active;
								this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
								this_component.HowLoadServed = HowMet_PassiveCap;
							}
						} else if ( SELECT_CASE_var == TypeOf_PumpVariableSpeed ) { //                 = 33
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
							this_component.HowLoadServed = HowMet_NoneDemand;
						} else if ( SELECT_CASE_var == TypeOf_PumpConstantSpeed ) { //                 = 34
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_NeedyIfLoopOn;
							this_component.HowLoadServed = HowMet_NoneDemand;
						} else if ( SELECT_CASE_var == TypeOf_PumpCondensate ) { //                    = 35
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
							this_component.HowLoadServed = HowMet_NoneDemand;
						} else if ( SELECT_CASE_var == TypeOf_PumpBankVariableSpeed ) { //             = 36
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_NeedyIfLoopOn;
							this_component.HowLoadServed = HowMet_NoneDemand;
						} else if ( SELECT_CASE_var == TypeOf_PumpBankConstantSpeed ) { //             = 37
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_NeedyIfLoopOn;
							this_component.HowLoadServed = HowMet_NoneDemand;
						} else if ( SELECT_CASE_var == TypeOf_WaterUseConnection ) { //              = 38
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
							this_component.HowLoadServed = HowMet_NoneDemand;
						} else if ( SELECT_CASE_var == TypeOf_CoilWaterCooling ) { //               = 39  ! demand side component
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
							this_component.HowLoadServed = HowMet_NoneDemand;
						} else if ( SELECT_CASE_var == TypeOf_CoilWaterDetailedFlatCooling ) { //      = 40  ! demand side component
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
							this_component.HowLoadServed = HowMet_NoneDemand;
						} else if ( SELECT_CASE_var == TypeOf_CoilWaterSimpleHeating ) { //           = 41  ! demand side component
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
							this_component.HowLoadServed = HowMet_NoneDemand;
						} else if ( SELECT_CASE_var == TypeOf_CoilSteamAirHeating ) { //         = 42  ! demand side component
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
							this_component.HowLoadServed = HowMet_NoneDemand;
						} else if ( SELECT_CASE_var == TypeOf_SolarCollectorFlatPlate ) { //         = 43  ! demand side component
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
							this_component.HowLoadServed = HowMet_PassiveCap;
						} else if ( SELECT_CASE_var == TypeOf_PlantLoadProfile ) { //            = 44  ! demand side component
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
							this_component.HowLoadServed = HowMet_NoneDemand;
						} else if ( SELECT_CASE_var == TypeOf_GrndHtExchgVertical ) { //            = 45
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
							this_component.HowLoadServed = HowMet_PassiveCap;
						} else if ( SELECT_CASE_var == TypeOf_GrndHtExchgSurface ) { //            = 46
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
							this_component.HowLoadServed = HowMet_PassiveCap;
						} else if ( SELECT_CASE_var == TypeOf_GrndHtExchgPond ) { //            = 47
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
							this_component.HowLoadServed = HowMet_PassiveCap;
						} else if ( SELECT_CASE_var == TypeOf_Generator_MicroTurbine ) { //          = 48  !newer FSEC turbine
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
							this_component.HowLoadServed = HowMet_ByNominalCap;
						} else if ( SELECT_CASE_var == TypeOf_Generator_ICEngine ) { //             = 49
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
							this_component.HowLoadServed = HowMet_ByNominalCap;
						} else if ( SELECT_CASE_var == TypeOf_Generator_CTurbine ) { //             = 50  !older BLAST turbine
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
							this_component.HowLoadServed = HowMet_ByNominalCap;
						} else if ( SELECT_CASE_var == TypeOf_Generator_MicroCHP ) { //              = 51
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
							this_component.HowLoadServed = HowMet_ByNominalCap;
						} else if ( SELECT_CASE_var == TypeOf_Generator_FCStackCooler ) { //         = 52
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
							this_component.HowLoadServed = HowMet_ByNominalCap;
						} else if ( SELECT_CASE_var == TypeOf_FluidCooler_SingleSpd ) { //           = 53
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
							this_component.HowLoadServed = HowMet_PassiveCap;
						} else if ( SELECT_CASE_var == TypeOf_FluidCooler_TwoSpd ) { //            = 54
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
							this_component.HowLoadServed = HowMet_PassiveCap;
						} else if ( SELECT_CASE_var == TypeOf_EvapFluidCooler_SingleSpd ) { //       = 55
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
							this_component.HowLoadServed = HowMet_PassiveCap;
						} else if ( SELECT_CASE_var == TypeOf_EvapFluidCooler_TwoSpd ) { //         = 56
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
							this_component.HowLoadServed = HowMet_PassiveCap;
						} else if ( SELECT_CASE_var == TypeOf_ChilledWaterTankMixed ) { //         = 57
							if ( LoopSideCtr == DemandSide ) {
								this_component.FlowCtrl = ControlType_Active;
								this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
								this_component.HowLoadServed = HowMet_NoneDemand;
							} else {
								this_component.FlowCtrl = ControlType_Active;
								this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
								this_component.HowLoadServed = HowMet_PassiveCap;
							}
						} else if ( SELECT_CASE_var == TypeOf_ChilledWaterTankStratified ) { //      = 58
							if ( LoopSideCtr == DemandSide ) {
								this_component.FlowCtrl = ControlType_Active;
								this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
								this_component.HowLoadServed = HowMet_NoneDemand;
							} else {
								this_component.FlowCtrl = ControlType_Active;
								this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
								this_component.HowLoadServed = HowMet_PassiveCap;
							}
						} else if ( SELECT_CASE_var == TypeOf_PVTSolarCollectorFlatPlate ) { //      = 59
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
							this_component.HowLoadServed = HowMet_PassiveCap;
							//next batch for ZoneHVAC
						} else if ( SELECT_CASE_var == TypeOf_Baseboard_Conv_Water ) { //        = 60
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
							this_component.HowLoadServed = HowMet_NoneDemand;
						} else if ( SELECT_CASE_var == TypeOf_Baseboard_Rad_Conv_Steam ) { //      = 61
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
							this_component.HowLoadServed = HowMet_NoneDemand;
						} else if ( SELECT_CASE_var == TypeOf_Baseboard_Rad_Conv_Water ) { //      = 62
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
							this_component.HowLoadServed = HowMet_NoneDemand;
						} else if ( SELECT_CASE_var == TypeOf_LowTempRadiant_VarFlow ) {
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
							this_component.HowLoadServed = HowMet_NoneDemand;
						} else if ( SELECT_CASE_var == TypeOf_LowTempRadiant_ConstFlow ) {
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
							this_component.HowLoadServed = HowMet_NoneDemand;
						} else if ( SELECT_CASE_var == TypeOf_CooledBeamAirTerminal ) {
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
							this_component.HowLoadServed = HowMet_NoneDemand;
						} else if ( SELECT_CASE_var == TypeOf_FourPipeBeamAirTerminal ) {
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
							this_component.HowLoadServed = HowMet_NoneDemand;
						} else if ( SELECT_CASE_var == TypeOf_CoilWAHPHeatingEquationFit ) {
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
							this_component.HowLoadServed = HowMet_NoneDemand;
						} else if ( SELECT_CASE_var == TypeOf_CoilWAHPCoolingEquationFit ) {
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
							this_component.HowLoadServed = HowMet_NoneDemand;
						} else if ( SELECT_CASE_var == TypeOf_CoilVSWAHPHeatingEquationFit ) {
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
							this_component.HowLoadServed = HowMet_NoneDemand;
						} else if ( SELECT_CASE_var == TypeOf_CoilVSWAHPCoolingEquationFit ) {
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
							this_component.HowLoadServed = HowMet_NoneDemand;
						} else if ( SELECT_CASE_var == TypeOf_CoilWAHPHeatingParamEst ) {
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
							this_component.HowLoadServed = HowMet_NoneDemand;
						} else if ( SELECT_CASE_var == TypeOf_CoilWAHPCoolingParamEst ) {
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
							this_component.HowLoadServed = HowMet_NoneDemand;
						} else if ( SELECT_CASE_var == TypeOf_RefrigSystemWaterCondenser ) {
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
							this_component.HowLoadServed = HowMet_PassiveCap;
						} else if ( SELECT_CASE_var == TypeOf_RefrigerationWaterCoolRack ) {
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
							this_component.HowLoadServed = HowMet_PassiveCap;
						} else if ( SELECT_CASE_var == TypeOf_MultiSpeedHeatPumpRecovery ) {
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
							this_component.HowLoadServed = HowMet_PassiveCap;
						} else if ( SELECT_CASE_var == TypeOf_UnitarySystemRecovery ) {
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
							this_component.HowLoadServed = HowMet_PassiveCap;
						} else if ( SELECT_CASE_var == TypeOf_PipingSystemPipeCircuit ) {
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
							this_component.HowLoadServed = HowMet_PassiveCap;
						} else if ( SELECT_CASE_var == TypeOf_SolarCollectorICS ) { //         = 75
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
							this_component.HowLoadServed = HowMet_PassiveCap;
						} else if ( SELECT_CASE_var == TypeOf_PlantComponentUserDefined ) {
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_Unknown;
							this_component.HowLoadServed = HowMet_Unknown;
						} else if ( SELECT_CASE_var == TypeOf_CoilUserDefined ) {
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_Unknown;
							this_component.HowLoadServed = HowMet_Unknown;
						} else if ( SELECT_CASE_var == TypeOf_ZoneHVACAirUserDefined ) {
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_Unknown;
							this_component.HowLoadServed = HowMet_Unknown;
						} else if ( SELECT_CASE_var == TypeOf_AirTerminalUserDefined ) {
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_Unknown;
							this_component.HowLoadServed = HowMet_Unknown;
						} else if ( SELECT_CASE_var == TypeOf_HeatPumpVRF ) { //       =  82  ! AirConditioner:VariableRefrigerantFlow
							this_component.FlowCtrl = ControlType_Active;

							if ( LoopSideCtr == DemandSide ) {
								this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
								this_component.HowLoadServed = HowMet_NoneDemand;
							} else { // should never happen
								this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
								this_component.HowLoadServed = HowMet_PassiveCap;
							}
						} else if ( SELECT_CASE_var == TypeOf_WaterSource ) {
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
							this_component.HowLoadServed = HowMet_ByNominalCapLowOutLimit;
						} else if ( SELECT_CASE_var == TypeOf_GrndHtExchgHorizTrench ) { // = 83  GroundHeatExchanger:HorizontalTrench
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
							this_component.HowLoadServed = HowMet_PassiveCap;
						} else if ( SELECT_CASE_var == TypeOf_FluidToFluidPlantHtExchg ) { //          = 84
							this_component.FlowCtrl = ControlType_Active;
							if ( LoopSideCtr == DemandSide ) {
								this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
								this_component.HowLoadServed = HowMet_NoneDemand;
							} else {
								this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
								this_component.HowLoadServed = HowMet_PassiveCap;
							}
						} else if ( SELECT_CASE_var == TypeOf_CentralGroundSourceHeatPump ) { // 86
							this_component.FlowCtrl = ControlType_Active;
							if ( LoopSideCtr == DemandSide ) {
								this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
								this_component.HowLoadServed = HowMet_NoneDemand;
							} else {
								this_component.FlowPriority = LoopFlowStatus_NeedyIfLoopOn;
								this_component.HowLoadServed = HowMet_ByNominalCap;
							}
						} else if ( SELECT_CASE_var == TypeOf_PackagedTESCoolingCoil ) { // 88
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_TakesWhatGets;
							this_component.HowLoadServed = HowMet_NoneDemand;
						} else if ( SELECT_CASE_var == TypeOf_SwimmingPool_Indoor ) { // 90
							this_component.FlowCtrl = ControlType_Active;
							this_component.FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn;
							this_component.HowLoadServed = HowMet_NoneDemand;
						} else {
							ShowSevereError( "SetBranchControlTypes: Caught unexpected equipment type of number" );

						}}

					}
				}
			}
		}

		// now set up branch control types based on components.

		if ( allocated( PlantLoop ) ) {
			NumCount = size( PlantLoop );
		} else {
			NumCount = 0;
		}
		for ( LoopCtr = 1; LoopCtr <= NumCount; ++LoopCtr ) { //SIZE(PlantLoop)
			for ( LoopSideCtr = DemandSide; LoopSideCtr <= SupplySide; ++LoopSideCtr ) {
				for ( BranchCtr = 1; BranchCtr <= PlantLoop( LoopCtr ).LoopSide( LoopSideCtr ).TotalBranches; ++BranchCtr ) {
					ActiveCount = 0;
					BypassCount = 0;
					for ( CompCtr = 1; CompCtr <= isize( PlantLoop( LoopCtr ).LoopSide( LoopSideCtr ).Branch( BranchCtr ).Comp ); ++CompCtr ) {
						ComponentFlowCtrl = PlantLoop( LoopCtr ).LoopSide( LoopSideCtr ).Branch( BranchCtr ).Comp( CompCtr ).FlowCtrl;

						{ auto const SELECT_CASE_var( ComponentFlowCtrl );

						if ( SELECT_CASE_var == ControlType_Unknown ) {
							PlantLoop( LoopCtr ).LoopSide( LoopSideCtr ).Branch( BranchCtr ).ControlType = ControlType_Passive;
						} else if ( SELECT_CASE_var == ControlType_Active ) {
							++ActiveCount;
							if ( ActiveCount > 1 ) {
								//  assume multiple active components in series means branch is SeriesActive
								PlantLoop( LoopCtr ).LoopSide( LoopSideCtr ).Branch( BranchCtr ).ControlType = ControlType_SeriesActive;
								// assume all components on branch are to be SeriesActive as well
								for ( auto & e : PlantLoop( LoopCtr ).LoopSide( LoopSideCtr ).Branch( BranchCtr ).Comp ) e.FlowCtrl = ControlType_SeriesActive;
							} else {
								PlantLoop( LoopCtr ).LoopSide( LoopSideCtr ).Branch( BranchCtr ).ControlType = ControlType_Active;
							}

							if ( BypassCount > 0 ) {
								ShowSevereError( "An active component is on the same branch as a pipe situated between splitter/mixer" );
								ShowContinueError( "Occurs in Branch=" + PlantLoop( LoopCtr ).LoopSide( LoopSideCtr ).Branch( BranchCtr ).Name );
								ShowContinueError( "Occurs in Plant Loop=" + PlantLoop( LoopCtr ).Name );
								ShowContinueError( "SetupBranchControlTypes: and the simulation continues" );
								// DSU3 note not sure why this is so bad.  heat transfer pipe might be a good reason to allow this?
								//   this used to fatal in older PlantFlowResolver.
							}

							// test for active component in series with bypass
						} else if ( SELECT_CASE_var == ControlType_Bypass ) {

							++BypassCount;
							PlantLoop( LoopCtr ).LoopSide( LoopSideCtr ).Branch( BranchCtr ).ControlType = ControlType_Bypass;
							PlantLoop( LoopCtr ).LoopSide( LoopSideCtr ).Branch( BranchCtr ).IsBypass = true;
							PlantLoop( LoopCtr ).LoopSide( LoopSideCtr ).BypassExists = true;

							if ( CompCtr > 1 ) {
								ShowSevereError( "A pipe used as a bypass should not be in series with another component" );
								ShowContinueError( "Occurs in Branch = " + PlantLoop( LoopCtr ).LoopSide( LoopSideCtr ).Branch( BranchCtr ).Name );
								ShowContinueError( "Occurs in PlantLoop = " + PlantLoop( LoopCtr ).Name );
								ShowFatalError( "SetupBranchControlTypes: preceding condition causes termination." );
							}

						} else if ( SELECT_CASE_var == ControlType_Passive ) {
							if ( ActiveCount > 0 ) {
								// do nothing, branch set before)
							} else {
								if ( BypassCount > 0 ) {

								} else {
									PlantLoop( LoopCtr ).LoopSide( LoopSideCtr ).Branch( BranchCtr ).ControlType = ControlType_Passive;
								}
							}
						} else if ( SELECT_CASE_var == ControlType_SeriesActive ) {
							// do nothing, already set when more than one active component found on a branch

						}}

					}
				}
			}
		}

	}

	void
	CheckIfAnyPlant()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Sept 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// determine if any plant loops will be ever be set up

		// METHODOLOGY EMPLOYED:
		// use input processor ot find number of plant loops

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using InputProcessor::GetNumObjectsFound;
		using DataGlobals::AnyPlantInModel;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int numPlantLoopsCheck;
		int numCondenserLoopsCheck;

		cCurrentModuleObject = "PlantLoop";
		numPlantLoopsCheck = GetNumObjectsFound( cCurrentModuleObject );

		cCurrentModuleObject = "CondenserLoop";
		numCondenserLoopsCheck = GetNumObjectsFound( cCurrentModuleObject );

		if ( ( numPlantLoopsCheck + numCondenserLoopsCheck ) > 0 ) {
			AnyPlantInModel = true;
		} else {
			AnyPlantInModel = false;
			PlantLoop.allocate( 0 );
		}

	}

} // PlantManager

} // EnergyPlus
