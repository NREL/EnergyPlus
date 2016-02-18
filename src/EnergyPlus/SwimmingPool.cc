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
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <SwimmingPool.hh>
#include <BranchNodeConnections.hh>
#include <DataBranchAirLoopPlant.hh>
#include <DataConversions.hh>
#include <DataEnvironment.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHeatBalSurface.hh>
#include <DataHVACGlobals.hh>
#include <DataLoopNode.hh>
#include <DataPlant.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSizing.hh>
#include <DataSurfaceLists.hh>
#include <DataSurfaces.hh>
#include <DataZoneEnergyDemands.hh>
#include <DataZoneEquipment.hh>
#include <EMSManager.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <GeneralRoutines.hh>
#include <HeatBalanceSurfaceManager.hh>
#include <InputProcessor.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <PlantUtilities.hh>
#include <Psychrometrics.hh>
#include <ReportSizingManager.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace SwimmingPool {

	// Module containing the routines dealing with swimming pools

	// MODULE INFORMATION:
	//       AUTHOR         Rick Strand, Ho-Sung Kim
	//       DATE WRITTEN   June 2012 (F90) and October 2014 (C++)
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// The purpose of this module is to encapsulate the data and algorithms required
 	// to manage the SwimmingPool System Component.

	// METHODOLOGY EMPLOYED:
	// The swimming pool acts as a surface within the heat balance and then connects
	// to the plant via a water loop.

	// REFERENCES:
	// 1. ASHRAE (2011). 2011 ASHRAE Handbook - HVAC Applications. Atlanta: American Society of Heating,
	//    Refrigerating and Air-Conditioning Engineers, Inc., p.5.6-5.9.
	// 2. Janis, R. and W. Tao (2005). Mechanical and Electrical Systems in Buildings. 3rd ed. Upper
	//    Saddle River, NJ: Pearson Education, Inc., p.246.
	// 3. Kittler, R. (1989). Indoor Natatorium Design and Energy Recycling. ASHRAE Transactions 95(1), p.521-526.
	// 4. Smith, C., R. Jones, and G. Lof (1993). Energy Requirements and Potential Savings for Heated
	//    Indoor Swimming Pools. ASHRAE Transactions 99(2), p.864-874.
	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataSurfaces::Surface;
	using DataSurfaces::TotSurfaces;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// System types:
	static std::string const BlankString;

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	// Standard, run-of-the-mill variables...
	int NumSwimmingPools( 0 ); // Number of swimming pools
	Array1D_bool CheckEquipName;
	Array1D_int SurfaceToPoolIndex;
	Array1D< Real64 > QPoolSrcAvg; // Average source over the time step for a particular radiant surface
	Array1D< Real64 > HeatTransCoefsAvg; // Average denominator term over the time step for a particular pool
	Array1D< Real64 > ZeroSourceSumHATsurf; // Equal to SumHATsurf for all the walls in a zone with no source
	// Record keeping variables used to calculate QRadSysSrcAvg locally
	Array1D< Real64 > LastQPoolSrc; // Need to keep the last value in case we are still iterating
	Array1D< Real64 > LastHeatTransCoefs; // Need to keep the last value in case we are still iterating
	Array1D< Real64 > LastSysTimeElapsed; // Need to keep the last value in case we are still iterating
	Array1D< Real64 > LastTimeStepSys; // Need to keep the last value in case we are still iterating

	// SUBROUTINE SPECIFICATIONS FOR MODULE LowTempRadiantSystem

	// Object Data
	Array1D< SwimmingPoolData > Pool;

	// Functions

	void
	clear_state()
	{
		NumSwimmingPools = 0;
		CheckEquipName.deallocate();
		SurfaceToPoolIndex.deallocate();
		QPoolSrcAvg.deallocate();
		HeatTransCoefsAvg.deallocate();
		ZeroSourceSumHATsurf.deallocate();
		LastQPoolSrc.deallocate();
		LastHeatTransCoefs.deallocate();
		LastSysTimeElapsed.deallocate();
		LastTimeStepSys.deallocate();
		Pool.deallocate();
	}

	void
	SimSwimmingPool (
		bool const FirstHVACIteration
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand, Ho-Sung Kim
		//       DATE WRITTEN   October 2014
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine manages the simulation of SwimmingPool.
		// This driver manages the calls to all of
		// the other drivers and simulation algorithms.

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology (Get, Init, Calc, Update, Report, etc.)

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHeatBalFanSys::SumConvPool;
		using DataHeatBalFanSys::SumLatentPool;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool GetInputFlag( true ); // First time, input is "gotten"
		int PoolNum; // Pool number index

		// FLOW:
		if ( GetInputFlag ) {
			GetSwimmingPool();
			GetInputFlag = false;
		}

		// System wide (for all pools) inits
		SumConvPool = 0.0;
		SumLatentPool = 0.0;

		for ( PoolNum = 1; PoolNum <= NumSwimmingPools; ++PoolNum ) {

			InitSwimmingPool( FirstHVACIteration, PoolNum );

			CalcSwimmingPool( PoolNum );

			UpdateSwimmingPool( PoolNum );

		}

		if ( NumSwimmingPools > 0 ) HeatBalanceSurfaceManager::CalcHeatBalanceInsideSurf();

		ReportSwimmingPool();

	}

	void
	GetSwimmingPool()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand, Ho-Sung Kim
		//       DATE WRITTEN   October 2014
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine reads the input for all swimming pools present in
		// the user input file.  This will contain all of the information needed
		// to simulate a swimming pool.

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.

		// REFERENCES:
		// na

		// Using/Aliasing
		using BranchNodeConnections::TestCompSet;
		using DataHeatBalance::Construct;
		using General::TrimSigDigits;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using InputProcessor::SameString;
		using InputProcessor::GetObjectDefMaxArgs;
		using InputProcessor::VerifyName;
		using NodeInputManager::GetOnlySingleNode;
		using ScheduleManager::GetScheduleIndex;
		using DataSurfaces::Surface;
		using DataSurfaces::SurfaceClass_Floor;
		using DataSurfaces::TotSurfaces;
		using DataSurfaces::HeatTransferModel_CTF;
		using DataSurfaces::SurfaceClass_Window;
		using namespace DataLoopNode;
		using namespace DataSurfaceLists;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetSwimmingPool: " ); // include trailing blank space
		Real64 const MinCoverFactor( 0.0 ); // minimum value for cover factors
		Real64 const MaxCoverFactor( 1.0 ); // maximum value for cover factors
		Real64 const MinDepth( 0.05 ); // minimum average pool depth (to avoid obvious input errors)
		Real64 const MaxDepth( 10.0 ); // maximum average pool depth (to avoid obvious input errors)
		Real64 const MinPowerFactor( 0.0 ); // minimum power factor for miscellaneous equipment

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool ErrorsFound( false ); // Set to true if something goes wrong
		std::string CurrentModuleObject; // for ease in getting objects
		Array1D_string Alphas; // Alpha items for object
		Array1D_string cAlphaFields; // Alpha field names
		Array1D_string cNumericFields; // Numeric field names
		int IOStatus; // Used in GetObjectItem
		int Item; // Item to be "gotten"
		int MaxAlphas; // Maximum number of alphas for these input keywords
		int MaxNumbers; // Maximum number of numbers for these input keywords
		Array1D< Real64 > Numbers; // Numeric items for object
		int NumAlphas; // Number of Alphas for each GetObjectItem call
		int NumArgs; // Unused variable that is part of a subroutine call
		int NumNumbers; // Number of Numbers for each GetObjectItem call
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		Array1D_bool lAlphaBlanks; // Logical array, alpha field input BLANK = .TRUE.
		Array1D_bool lNumericBlanks; // Logical array, numeric field input BLANK = .TRUE.
		int SurfNum; // Surface number

		// FLOW:
		// Initializations and allocations
		MaxAlphas = 0;
		MaxNumbers = 0;

		GetObjectDefMaxArgs( "SwimmingPool:Indoor", NumArgs, NumAlphas, NumNumbers );
		MaxAlphas = max( MaxAlphas, NumAlphas );
		MaxNumbers = max( MaxNumbers, NumNumbers );

		Alphas.allocate( MaxAlphas );
		Alphas = "";
		Numbers.allocate( MaxNumbers );
		Numbers = 0.0;
		cAlphaFields.allocate( MaxAlphas );
		cAlphaFields = "";
		cNumericFields.allocate( MaxNumbers );
		cNumericFields = "";
		lAlphaBlanks.allocate( MaxAlphas );
		lAlphaBlanks = true;
		lNumericBlanks.allocate( MaxNumbers );
		lNumericBlanks = true;

		NumSwimmingPools = GetNumObjectsFound( "SwimmingPool:Indoor" );
		CheckEquipName.allocate( NumSwimmingPools );
		CheckEquipName = true;

		Pool.allocate( NumSwimmingPools );
		SurfaceToPoolIndex.allocate( TotSurfaces );
		SurfaceToPoolIndex = 0;

		// Obtain all of the user data related to indoor swimming pools...
		CurrentModuleObject = "SwimmingPool:Indoor";
		for ( Item = 1; Item <= NumSwimmingPools; ++Item ) {

			GetObjectItem( CurrentModuleObject, Item, Alphas, NumAlphas, Numbers, NumNumbers, IOStatus, lNumericBlanks, lAlphaBlanks, cAlphaFields, cNumericFields );

			IsNotOK = false;
			IsBlank = false;
			VerifyName( Alphas( 1 ), Pool, Item, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) Alphas( 1 ) = "xxxxx";
			}
			Pool( Item ).Name = Alphas( 1 );

			Pool( Item ).SurfaceName = Alphas( 2 );
			Pool( Item ).SurfacePtr = 0;
			for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
				if ( SameString( Surface( SurfNum ).Name, Pool( Item ).SurfaceName ) ) {
					Pool( Item ).SurfacePtr = SurfNum;
					break;
				}
			}
			if ( Pool( Item ).SurfacePtr <= 0 ) {
				ShowSevereError( RoutineName + "Invalid " + cAlphaFields( 2 ) + " = " + Alphas( 2 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
				ErrorsFound = true;
			} else if ( Surface( Pool( Item ).SurfacePtr ).PartOfVentSlabOrRadiantSurface ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + "\", Invalid Surface" );
				ShowContinueError( cAlphaFields( 2 ) + "=\"" + Alphas( 2 ) + "\" has been used in another radiant system, ventilated slab, or pool." );
				ShowContinueError( "A single surface can only be a radiant system, a ventilated slab, or a pool.  It CANNOT be more than one of these." );
				ErrorsFound = true;
				// Something present that is not allowed for a swimming pool (non-CTF algorithm, movable insulation, or radiant source/sink
			} else if ( Surface( Pool( Item ).SurfacePtr ).HeatTransferAlgorithm != HeatTransferModel_CTF ) {
				ShowSevereError( Surface( Pool( Item ).SurfacePtr ).Name + " is a pool and is attempting to use a non-CTF solution algorithm.  This is not allowed.  Use the CTF solution algorithm for this surface." );
				ErrorsFound = true;
			} else if ( Surface( Pool( Item ).SurfacePtr).Class == SurfaceClass_Window ) {
				ShowSevereError( Surface( Pool( Item ).SurfacePtr ).Name + " is a pool and is defined as a window.  This is not allowed.  A pool must be a floor that is NOT a window." );
				ErrorsFound = true;
			} else if ( Surface( Pool( Item ).SurfacePtr ).MaterialMovInsulInt > 0 ) {
				ShowSevereError( Surface( Pool( Item ).SurfacePtr ).Name + " is a pool and has movable insulation.  This is not allowed.  Remove the movable insulation for this surface." );
				ErrorsFound = true;
			} else if ( Construct( Surface( Pool( Item ).SurfacePtr ).Construction ).SourceSinkPresent ) {
				ShowSevereError( Surface( Pool( Item ).SurfacePtr ).Name + " is a pool and uses a construction with a source/sink.  This is not allowed.  Use a standard construction for this surface." );
				ErrorsFound = true;
			} else { // ( Pool( Item ).SurfacePtr > 0 )
				Surface( Pool( Item ).SurfacePtr ).PartOfVentSlabOrRadiantSurface = true;
				Surface( Pool( Item ).SurfacePtr ).IsPool = true;
				SurfaceToPoolIndex( Pool( Item ).SurfacePtr ) = Item;
				// Check to make sure pool surface is a floor
				if ( Surface( Pool( Item ).SurfacePtr).Class != SurfaceClass_Floor ) {
					ShowSevereError(RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + " contains a surface name that is NOT a floor." );
					ShowContinueError( "A swimming pool must be associated with a surface that is a FLOOR.  Association with other surface types is not permitted.");
					ErrorsFound = true;
				}
			}
			Pool( Item ).ZonePtr = Surface( Pool( Item ).SurfacePtr ).Zone;

			Pool( Item ).AvgDepth = Numbers( 1 );
			if ( Pool( Item ).AvgDepth < MinDepth ) {
				ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + " has an average depth that is too small." );
				ShowContinueError( "The pool average depth has been reset to the minimum allowed depth." );
			} else if ( Pool( Item).AvgDepth > MaxDepth ) {
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + " has an average depth that is too large." );
				ShowContinueError( "The pool depth must be less than the maximum average depth of 10 meters." );
				ErrorsFound = true;
			}

			Pool( Item ).ActivityFactorSchedName = Alphas( 3 );
			Pool( Item ).ActivityFactorSchedPtr = GetScheduleIndex( Alphas( 3 ) );
			if ( ( Pool( Item ).ActivityFactorSchedPtr == 0 ) && ( ! lAlphaBlanks( 3 ) ) ) {
				ShowSevereError( cAlphaFields( 3 ) + " not found: " + Alphas( 3 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
				ErrorsFound = true;
			}

			Pool( Item ).MakeupWaterSupplySchedName = Alphas( 4 );
			Pool( Item ).MakeupWaterSupplySchedPtr = GetScheduleIndex( Alphas( 4 ) );
			if ( ( Pool( Item ).MakeupWaterSupplySchedPtr == 0 ) && ( ! lAlphaBlanks( 4 ) ) ) {
				ShowSevereError( cAlphaFields( 4 ) + " not found: " + Alphas( 4 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
				ErrorsFound = true;
			}

			Pool( Item ).CoverSchedName = Alphas( 5 );
			Pool( Item ).CoverSchedPtr = GetScheduleIndex( Alphas( 5 ) );
			if ( ( Pool( Item ).CoverSchedPtr == 0 ) && ( ! lAlphaBlanks( 5 ) ) ) {
				ShowSevereError( cAlphaFields( 5 ) + " not found: " + Alphas( 5 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
				ErrorsFound = true;
			}

			Pool( Item ).CoverEvapFactor = Numbers( 2 );
			if ( Pool( Item ).CoverEvapFactor < MinCoverFactor ) {
				ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + " has an evaporation cover factor less than zero." );
				ShowContinueError( "The evaporation cover factor has been reset to zero." );
				Pool( Item ).CoverEvapFactor = MinCoverFactor;
			} else if ( Pool( Item).CoverEvapFactor > MaxCoverFactor ) {
				ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + " has an evaporation cover factor greater than one." );
				ShowContinueError( "The evaporation cover factor has been reset to one." );
				Pool( Item ).CoverEvapFactor = MaxCoverFactor;
			}

			Pool( Item ).CoverConvFactor = Numbers( 3 );
			if ( Pool( Item ).CoverConvFactor < MinCoverFactor ) {
				ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + " has a convection cover factor less than zero." );
				ShowContinueError( "The convection cover factor has been reset to zero." );
				Pool( Item ).CoverConvFactor = MinCoverFactor;
			} else if ( Pool( Item).CoverConvFactor > MaxCoverFactor ) {
				ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + " has a convection cover factor greater than one." );
				ShowContinueError( "The convection cover factor has been reset to one." );
				Pool( Item ).CoverConvFactor = MaxCoverFactor;
			}

			Pool( Item ).CoverSWRadFactor = Numbers( 4 );
			if ( Pool( Item ).CoverSWRadFactor < MinCoverFactor ) {
				ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + " has a short-wavelength radiation cover factor less than zero." );
				ShowContinueError( "The short-wavelength radiation cover factor has been reset to zero." );
				Pool( Item ).CoverSWRadFactor = MinCoverFactor;
			} else if ( Pool( Item).CoverSWRadFactor > MaxCoverFactor ) {
				ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + " has a short-wavelength radiation cover factor greater than one." );
				ShowContinueError( "The short-wavelength radiation cover factor has been reset to one." );
				Pool( Item ).CoverSWRadFactor = MaxCoverFactor;
			}

			Pool( Item ).CoverLWRadFactor = Numbers( 5 );
			if ( Pool( Item ).CoverLWRadFactor < MinCoverFactor ) {
				ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + " has a long-wavelength radiation cover factor less than zero." );
				ShowContinueError( "The long-wavelength radiation cover factor has been reset to zero." );
				Pool( Item ).CoverLWRadFactor = MinCoverFactor;
			} else if ( Pool( Item).CoverLWRadFactor > MaxCoverFactor ) {
				ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + " has a long-wavelength radiation cover factor greater than one." );
				ShowContinueError( "The long-wavelength radiation cover factor has been reset to one." );
				Pool( Item ).CoverLWRadFactor = MaxCoverFactor;
			}

			Pool( Item ).WaterInletNodeName = Alphas( 6 );
			Pool( Item ).WaterOutletNodeName = Alphas( 7 );
			Pool( Item ).WaterInletNode = GetOnlySingleNode( Alphas( 6 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Water, NodeConnectionType_Inlet, 1, ObjectIsNotParent );
			Pool( Item ).WaterOutletNode = GetOnlySingleNode( Alphas( 7 ), ErrorsFound, CurrentModuleObject, Alphas( 1 ), NodeType_Water, NodeConnectionType_Outlet, 1, ObjectIsNotParent );
			if ( ( ! lAlphaBlanks( 6 ) ) || ( ! lAlphaBlanks( 7 ) ) ) {
				TestCompSet( CurrentModuleObject, Alphas( 1 ), Alphas( 6 ), Alphas( 7 ), "Hot Water Nodes" );
			}
			Pool( Item ).WaterVolFlowMax = Numbers( 6 );
			Pool( Item ).MiscPowerFactor = Numbers( 7 );
			if ( Pool( Item ).MiscPowerFactor < MinPowerFactor ) {
				ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + " has a miscellaneous power factor less than zero." );
				ShowContinueError( "The miscellaneous power factor has been reset to zero." );
				Pool( Item ).MiscPowerFactor = MinPowerFactor;
			}

			Pool( Item ).SetPtTempSchedName = Alphas( 8 );
			Pool( Item ).SetPtTempSchedPtr = GetScheduleIndex( Alphas( 8 ) );
			if ( ( Pool( Item ).SetPtTempSchedPtr == 0 ) && ( ! lAlphaBlanks( 8 ) ) ) {
				ShowSevereError( cAlphaFields( 8 ) + " not found: " + Alphas( 8 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
				ErrorsFound = true;
			}
			if ( lAlphaBlanks(8) ) {
				ShowSevereError( cAlphaFields( 8 ) + " left blank.  This is NOT allowed as there must be a pool water setpoint temperature." );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
				ErrorsFound = true;
			}

			Pool( Item ).MaxNumOfPeople = Numbers( 8 );
			if ( Pool( Item ).MaxNumOfPeople < 0.0 ) {
				ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + Alphas( 1 ) + " was entered with negative people.  This is not allowed." );
				ShowContinueError( "The number of people has been reset to zero." );
				Pool( Item ).MaxNumOfPeople = 0.0;
			}

			Pool( Item ).PeopleSchedName = Alphas( 9 );
			Pool( Item ).PeopleSchedPtr = GetScheduleIndex( Alphas( 9 ) );
			if ( ( Pool( Item ).PeopleSchedPtr == 0 ) && ( ! lAlphaBlanks( 9 ) ) ) {
				ShowSevereError( cAlphaFields( 9 ) + " not found: " + Alphas( 9 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
				ErrorsFound = true;
			}

			Pool( Item ).PeopleHeatGainSchedName = Alphas( 10 );
			Pool( Item ).PeopleHeatGainSchedPtr = GetScheduleIndex( Alphas( 10 ) );
			if ( ( Pool( Item ).PeopleHeatGainSchedPtr == 0 ) && ( ! lAlphaBlanks( 10 ) ) ) {
				ShowSevereError( cAlphaFields( 10 ) + " not found: " + Alphas( 10 ) );
				ShowContinueError( "Occurs in " + CurrentModuleObject + " = " + Alphas( 1 ) );
				ErrorsFound = true;
			}

		}

		Alphas.deallocate();
		Numbers.deallocate();
		cAlphaFields.deallocate();
		cNumericFields.deallocate();
		lAlphaBlanks.deallocate();
		lNumericBlanks.deallocate();

		if ( ErrorsFound ) {
			ShowFatalError( RoutineName + "Errors found in swimming pool input. Preceding conditions cause termination." );
		}

		// Set up the output variables for swimming pools
		// CurrentModuleObject = "SwimmingPool:Indoor"
		for ( Item = 1; Item <= NumSwimmingPools; ++Item ) {
			SetupOutputVariable( "Indoor Pool Makeup Water Rate [m3/s]", Pool(Item).MakeUpWaterVolFlowRate, "System", "Average", Pool(Item).Name);
			SetupOutputVariable( "Indoor Pool Makeup Water Volume [m3]", Pool( Item ).MakeUpWaterVol, "System", "Sum", Pool( Item ).Name, _, "MainsWater", "Heating", _, "System");
			SetupOutputVariable( "Indoor Pool Makeup Water Temperature [C]", Pool( Item ).CurMakeupWaterTemp, "System", "Average", Pool( Item ).Name );
			SetupOutputVariable( "Indoor Pool Water Temperature [C]", Pool( Item ).PoolWaterTemp, "System", "Average", Pool( Item ).Name );
			SetupOutputVariable( "Indoor Pool Inlet Water Temperature [C]", Pool( Item ).WaterInletTemp, "System", "Average", Pool( Item ).Name );
			SetupOutputVariable( "Indoor Pool Inlet Water Mass Flow Rate [kg/s]", Pool( Item ).WaterMassFlowRate, "System", "Average", Pool( Item ).Name );
			SetupOutputVariable( "Indoor Pool Miscellaneous Equipment Power [W]", Pool( Item ).MiscEquipPower, "System", "Average", Pool( Item ).Name );
			SetupOutputVariable( "Indoor Pool Miscellaneous Equipment Energy [J]", Pool( Item ).MiscEquipEnergy, "System", "Sum", Pool( Item ).Name);
			SetupOutputVariable( "Indoor Pool Water Heating Rate [W]", Pool( Item ).HeatPower, "System", "Average", Pool( Item ).Name );
			SetupOutputVariable( "Indoor Pool Water Heating Energy [J]", Pool( Item ).HeatEnergy, "System", "Sum", Pool( Item ).Name, _, "ENERGYTRANSFER", "HEATINGCOILS", _, "System" );
			SetupOutputVariable( "Indoor Pool Radiant to Convection by Cover [W]", Pool( Item ).RadConvertToConvect, "System", "Average", Pool( Item ).Name );
			SetupOutputVariable( "Indoor Pool People Heat Gain [W]", Pool( Item ).PeopleHeatGain, "System", "Average", Pool( Item ).Name );
			SetupOutputVariable( "Indoor Pool Current Activity Factor []", Pool( Item ).CurActivityFactor, "System", "Average", Pool( Item ).Name );
			SetupOutputVariable( "Indoor Pool Current Cover Factor []", Pool( Item ).CurCoverSchedVal, "System", "Average", Pool( Item ).Name );
			SetupOutputVariable( "Indoor Pool Evaporative Heat Loss Rate [W]", Pool( Item ).EvapHeatLossRate, "System", "Average", Pool( Item ).Name );
			SetupOutputVariable( "Indoor Pool Evaporative Heat Loss Energy [J]", Pool( Item ).EvapEnergyLoss, "System", "Sum", Pool( Item ).Name);
		}

	}

	void
	InitSwimmingPool(
		bool const FirstHVACIteration, // true during the first HVAC iteration
		int const PoolNum // Index for the low temperature radiant system under consideration within the derived types
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand, Ho-Sung Kim
		//       DATE WRITTEN   October 2014
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine initializes variables relating to low temperature radiant
		// systems.

		// METHODOLOGY EMPLOYED:
		// Simply initializes whatever needs initializing.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::BeginEnvrnFlag;
		using DataGlobals::AnyPlantInModel;
		using DataLoopNode::Node;
		using ScheduleManager::GetCurrentScheduleValue;
		using DataPlant::ScanPlantLoopsForObject;
		using DataPlant::PlantLoop;
		using DataPlant::TypeOf_SwimmingPool_Indoor;
		using PlantUtilities::SetComponentFlowRate;
		using PlantUtilities::InitComponentNodes;
		using FluidProperties::GetDensityGlycol;
		using DataEnvironment::WaterMainsTemp;
		using DataGlobals::NumOfZones;
		using DataGlobals::BeginTimeStepFlag;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "InitSwimmingPool" );
		Real64 const MinActivityFactor = 0.0; // Minimum value for activity factor
		Real64 const MaxActivityFactor = 10.0; // Maximum value for activity factor (realistically)

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool MyOneTimeFlag( true ); // Flag for one-time initializations
		static bool MyEnvrnFlagGeneral( true );
		static Array1D_bool MyPlantScanFlagPool;
		bool errFlag;
		Real64 mdot;
		Real64 HeatGainPerPerson;
		Real64 PeopleModifier;
		int ZoneNum;
		int SurfNum;
		Real64 Density;

		// FLOW:

		if ( MyOneTimeFlag ) {
			MyOneTimeFlag = false;
			MyPlantScanFlagPool.allocate( NumSwimmingPools );
			MyPlantScanFlagPool = true;

			if ( MyPlantScanFlagPool( PoolNum ) && allocated( PlantLoop ) ) {
				errFlag = false;
				if ( Pool( PoolNum ).WaterInletNode > 0 ) {
					ScanPlantLoopsForObject( Pool( PoolNum ).Name, TypeOf_SwimmingPool_Indoor, Pool( PoolNum ).HWLoopNum, Pool( PoolNum ).HWLoopSide, Pool( PoolNum ).HWBranchNum, Pool( PoolNum ).HWCompNum, _, _, _, Pool( PoolNum ).WaterInletNode, _, errFlag );
					if ( errFlag ) {
						ShowFatalError( RoutineName + ": Program terminated due to previous condition(s)." );
					}
				}
				MyPlantScanFlagPool( PoolNum ) = false;
			} else if ( MyPlantScanFlagPool( PoolNum ) && ! AnyPlantInModel ) {
				MyPlantScanFlagPool( PoolNum ) = false;
			}
			ZeroSourceSumHATsurf.allocate( NumOfZones );
			ZeroSourceSumHATsurf = 0.0;
			QPoolSrcAvg.allocate( TotSurfaces );
			QPoolSrcAvg = 0.0;
			HeatTransCoefsAvg.allocate( TotSurfaces );
			HeatTransCoefsAvg = 0.0;
			LastQPoolSrc.allocate( TotSurfaces );
			LastQPoolSrc = 0.0;
			LastHeatTransCoefs.allocate( TotSurfaces );
			LastHeatTransCoefs = 0.0;
			LastSysTimeElapsed.allocate( TotSurfaces );
			LastSysTimeElapsed = 0.0;
			LastTimeStepSys.allocate( TotSurfaces );
			LastTimeStepSys = 0.0;
		}

		if ( BeginEnvrnFlag && MyEnvrnFlagGeneral ) {
			ZeroSourceSumHATsurf = 0.0;
			QPoolSrcAvg = 0.0;
			HeatTransCoefsAvg = 0.0;
			LastQPoolSrc = 0.0;
			LastHeatTransCoefs = 0.0;
			LastSysTimeElapsed = 0.0;
			LastTimeStepSys = 0.0;
			MyEnvrnFlagGeneral = false;
		}
		if ( ! BeginEnvrnFlag ) MyEnvrnFlagGeneral = true;


		if ( BeginEnvrnFlag ) {
			Pool( PoolNum ).PoolWaterTemp = 23.0;
			Pool( PoolNum ).HeatPower = 0.0;
			Pool( PoolNum ).HeatEnergy = 0.0;
			Pool( PoolNum ).MiscEquipPower = 0.0;
			Pool( PoolNum ).MiscEquipEnergy = 0.0;
			Pool( PoolNum ).WaterInletTemp = 0.0;
			Pool( PoolNum ).WaterOutletTemp = 0.0;
			Pool( PoolNum ).WaterMassFlowRate = 0.0;
			Pool( PoolNum ).PeopleHeatGain = 0.0;
			Density =GetDensityGlycol( "WATER", Pool( PoolNum ).PoolWaterTemp, Pool( PoolNum ).GlycolIndex, RoutineName );
			Pool( PoolNum ).WaterMass = Surface( Pool( PoolNum ).SurfacePtr ).Area * Pool( PoolNum ).AvgDepth * Density;
			Pool( PoolNum ).WaterMassFlowRateMax = Pool( PoolNum ).WaterVolFlowMax * Density;
			if ( ! MyPlantScanFlagPool( PoolNum ) ) {
				if ( Pool( PoolNum ).WaterInletNode > 0 ) {
					InitComponentNodes( 0.0, Pool( PoolNum ).WaterMassFlowRateMax, Pool( PoolNum ).WaterInletNode, Pool( PoolNum ).WaterOutletNode, Pool( PoolNum ).HWLoopNum, Pool( PoolNum ).HWLoopSide, Pool( PoolNum ).HWBranchNum, Pool( PoolNum ).HWCompNum );
				}
			}
		}

		if ( BeginTimeStepFlag && FirstHVACIteration ) { // This is the first pass through in a particular time step

			ZoneNum = Pool( PoolNum ).ZonePtr;
			ZeroSourceSumHATsurf( ZoneNum ) = SumHATsurf( ZoneNum ); // Set this to figure what part of the load the radiant system meets
			SurfNum = Pool( PoolNum ).SurfacePtr;
			QPoolSrcAvg( SurfNum ) = 0.0; // Initialize this variable to zero (pool parameters "off")
			HeatTransCoefsAvg( SurfNum ) = 0.0; // Initialize this variable to zero (pool parameters "off")
			LastQPoolSrc( SurfNum ) = 0.0; // At the start of a time step, reset to zero so average calculation can begin again
			LastSysTimeElapsed( SurfNum ) = 0.0; // At the start of a time step, reset to zero so average calculation can begin again
			LastTimeStepSys( SurfNum ) = 0.0; // At the start of a time step, reset to zero so average calculation can begin again
		}

		// initialize the flow rate for the component on the plant side (this follows standard procedure for other components like low temperature radiant systems)
		mdot = 0.0;
		SetComponentFlowRate( mdot, Pool( PoolNum ).WaterInletNode, Pool( PoolNum ).WaterOutletNode, Pool( PoolNum ).HWLoopNum, Pool( PoolNum ).HWLoopSide, Pool( PoolNum ).HWBranchNum, Pool( PoolNum ).HWCompNum );
		Pool( PoolNum ).WaterInletTemp = Node( Pool( PoolNum ).WaterInletNode ).Temp;

		// get the schedule values for different scheduled parameters
		if ( Pool( PoolNum ).ActivityFactorSchedPtr > 0 ) {
			Pool( PoolNum ).CurActivityFactor = GetCurrentScheduleValue( Pool( PoolNum ).ActivityFactorSchedPtr );
			if ( Pool( PoolNum ).CurActivityFactor < MinActivityFactor ) {
				Pool( PoolNum ).CurActivityFactor = MinActivityFactor;
				ShowWarningError( RoutineName + ": Swimming Pool =\"" + Pool( PoolNum ).Name  + " Activity Factor Schedule =\"" + Pool( PoolNum ).ActivityFactorSchedName + " has a negative value.  This is not allowed." );
				ShowContinueError( "The activity factor has been reset to zero." );
			}
			if ( Pool( PoolNum ).CurActivityFactor > MaxActivityFactor ) {
				Pool( PoolNum ).CurActivityFactor = 1.0;
				ShowWarningError( RoutineName + ": Swimming Pool =\"" + Pool( PoolNum ).Name  + " Activity Factor Schedule =\"" + Pool( PoolNum ).ActivityFactorSchedName + " has a value larger than 10.  This is not allowed." );
				ShowContinueError( "The activity factor has been reset to unity." );
			}
		} else {
			// default is activity factor of 1.0
			Pool( PoolNum ).CurActivityFactor = 1.0;
		}

		Pool( PoolNum ).CurSetPtTemp = GetCurrentScheduleValue( Pool( PoolNum ).SetPtTempSchedPtr );

		if ( Pool( PoolNum ).MakeupWaterSupplySchedPtr > 0 ) {
			Pool( PoolNum ).CurMakeupWaterTemp = GetCurrentScheduleValue( Pool( PoolNum ).MakeupWaterSupplySchedPtr );
		} else {
			// use water main temperaure if no schedule present in input
			Pool( PoolNum ).CurMakeupWaterTemp = WaterMainsTemp;
		}

		// determine the current heat gain from people
		if ( Pool( PoolNum ).PeopleHeatGainSchedPtr > 0 ) {
			HeatGainPerPerson = GetCurrentScheduleValue( Pool(PoolNum).PeopleHeatGainSchedPtr );
			if ( HeatGainPerPerson < 0.0 ) {
				ShowWarningError( RoutineName + ": Swimming Pool =\"" + Pool( PoolNum ).Name  + " Heat Gain Schedule =\"" + Pool( PoolNum ).PeopleHeatGainSchedName + " has a negative value.  This is not allowed." );
				ShowContinueError( "The heat gain per person has been reset to zero." );
				HeatGainPerPerson = 0.0;
			}
			if ( Pool( PoolNum ).PeopleSchedPtr > 0 ) {
				PeopleModifier = GetCurrentScheduleValue( Pool( PoolNum ).PeopleSchedPtr );
				if ( PeopleModifier < 0.0 ) {
					ShowWarningError( RoutineName + ": Swimming Pool =\"" + Pool( PoolNum ).Name  + " People Schedule =\"" + Pool( PoolNum ).PeopleSchedName + " has a negative value.  This is not allowed." );
					ShowContinueError( "The number of people has been reset to zero." );
					PeopleModifier = 0.0;
				}
			} else { // no people schedule entered--assume that full number always present
				PeopleModifier = 1.0;
			}
		} else { // no heat gain schedule added--assume a zero value for Heat Gain per Person and no people present
			HeatGainPerPerson = 0.0;
			PeopleModifier = 0.0;
		}
		Pool( PoolNum ).PeopleHeatGain = PeopleModifier * HeatGainPerPerson * Pool( PoolNum ).MaxNumOfPeople;

		// once cover schedule value is established, define the current values of the cover heat transfer factors
		if ( Pool( PoolNum ).CoverSchedPtr > 0 ) {
			Pool( PoolNum ).CurCoverSchedVal = GetCurrentScheduleValue( Pool( PoolNum ).CoverSchedPtr );
			if ( Pool( PoolNum ).CurCoverSchedVal > 1.0 ) {
				ShowWarningError(RoutineName + ": Swimming Pool =\"" + Pool( PoolNum ).Name  + " Cover Schedule =\"" + Pool( PoolNum ).CoverSchedName + " has a value greater than 1.0 (100%).  This is not allowed." );
				ShowContinueError( "The cover has been reset to one or fully covered." );
				Pool( PoolNum ).CurCoverSchedVal = 1.0;
			} else if ( Pool( PoolNum ).CurCoverSchedVal < 0.0 ) {
				ShowWarningError(RoutineName + ": Swimming Pool =\"" + Pool( PoolNum ).Name  + " Cover Schedule =\"" + Pool( PoolNum ).CoverSchedName + " has a negative value.  This is not allowed." );
				ShowContinueError( "The cover has been reset to zero or uncovered." );
				Pool( PoolNum ).CurCoverSchedVal = 0.0;
			}
		} else {
			// default is NO pool cover
			Pool( PoolNum ).CurCoverSchedVal = 0.0;
		}
		// for the current cover factors, a value of 1.0 means that the pool is open (not covered)
		// the user input values determine the amount the pool cover degrades one of the factors
		// for example, if the cover reduces convection by 50% and the pool is half covered, then
		// the reduction factor for convection is 25% or 75% of the normal value.  this establishes
		// the following relationships and how they are used in other parts of the code.
		// note that for the radiation factors, the reduction in absorption of radiation caused by
		// the cover will result in a net imbalance if this energy which is no longer accounted for
		// in the surface heat balance is not accounted for elsewhere.  thus, these terms will dump
		// any reduced radiation into the air heat balance as an additional convective gain to avoid
		// any loss of energy in the overall heat balance.
		Pool( PoolNum ).CurCoverEvapFac = 1.0 - ( Pool( PoolNum ).CurCoverSchedVal * Pool( PoolNum ).CoverEvapFactor );
		Pool( PoolNum ).CurCoverConvFac = 1.0 - ( Pool( PoolNum ).CurCoverSchedVal * Pool( PoolNum ).CoverConvFactor );
		Pool( PoolNum ).CurCoverSWRadFac = 1.0 - ( Pool( PoolNum ).CurCoverSchedVal * Pool( PoolNum ).CoverSWRadFactor );
		Pool( PoolNum ).CurCoverLWRadFac = 1.0 - ( Pool( PoolNum ).CurCoverSchedVal * Pool( PoolNum ).CoverLWRadFactor );

	}

	void
	CalcSwimmingPool(
		int const PoolNum // number of the swimming pool
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand, Ho-Sung Kim
		//       DATE WRITTEN   October 2014
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine simulates the components making up the Indoor Swimming Pool model.

		// METHODOLOGY EMPLOYED:
		// The swimming pool is modeled as a SURFACE to get access to all of the existing
		// surface related algorithms.  This subroutine mainly models the components of the
		// swimming pool so that information can be used in a standard surface heat balance.
		// The pool is assumed to be located at the inside surface face with a possible cover
		// affecting the heat balance.  The pool model takes the form of an equation solving
		// for the inside surface temperature which is assumed to be the same as the pool
		// water temperature.
		// Standard Heat Balance Equation:
		//		TempSurfInTmp( SurfNum ) = ( CTFConstInPart( SurfNum ) + QRadThermInAbs( SurfNum ) + QRadSWInAbs( SurfNum ) + HConvIn( SurfNum ) * RefAirTemp( SurfNum ) + NetLWRadToSurf( SurfNum ) + Construct( ConstrNum ).CTFSourceIn( 0 ) * QsrcHist( 1, SurfNum ) + QHTRadSysSurf( SurfNum ) + QHWBaseboardSurf( SurfNum ) + QSteamBaseboardSurf( SurfNum ) + QElecBaseboardSurf( SurfNum ) + IterDampConst * TempInsOld( SurfNum ) + Construct( ConstrNum ).CTFCross( 0 ) * TH11 ) / ( Construct( ConstrNum ).CTFInside( 0 ) + HConvIn( SurfNum ) + IterDampConst ); // Constant part of conduction eq (history terms) | LW radiation from internal sources | SW radiation from internal sources | Convection from surface to zone air | Net radiant exchange with other zone surfaces | Heat source/sink term for radiant systems | (if there is one present) | Radiant flux from high temp radiant heater | Radiant flux from a hot water baseboard heater | Radiant flux from a steam baseboard heater | Radiant flux from an electric baseboard heater | Iterative damping term (for stability) | Current conduction from | the outside surface | Coefficient for conduction (current time) | Convection and damping term
		// That equation is modified to include pool specific terms and removes the IterDampConst
		// term which is for iterations within the inside surface heat balance.  Then, the resulting
		// equation is solved for the plant loop mass flow rate.  It also assigns the appropriate
		// terms for use in the actual heat balance routine.



		// REFERENCES:
		//  1. ASHRAE (2011). 2011 ASHRAE Handbook - HVAC Applications. Atlanta: American Society of Heating,
		//     Refrigerating and Air-Conditioning Engineers, Inc., p.5.6-5.9.
		//  2. Janis, R. and W. Tao (2005). Mechanical and Electrical Systems in Buildings. 3rd ed. Upper
		//     Saddle River, NJ: Pearson Education, Inc., p.246.
		//  3. Kittler, R. (1989). Indoor Natatorium Design and Energy Recycling. ASHRAE Transactions 95(1), p.521-526.
		//  4. Smith, C., R. Jones, and G. Lof (1993). Energy Requirements and Potential Savings for Heated
		//     Indoor Swimming Pools. ASHRAE Transactions 99(2), p.864-874.

		// Using/Aliasing
		using DataHeatBalFanSys::MAT;
		using DataHeatBalFanSys::ZoneAirHumRatAvg;
		using ScheduleManager::GetCurrentScheduleValue;
		using DataConversions::CFA;
		using DataConversions::CFMF;
		using Psychrometrics::PsyPsatFnTemp;
		using Psychrometrics::PsyRhFnTdbWPb;
		using Psychrometrics::PsyHfgAirFnWTdb;
		using DataEnvironment::OutBaroPress;
		using DataHeatBalance::QRadThermInAbs;
		using DataHeatBalSurface::NetLWRadToSurf;
		using DataHeatBalFanSys::QHTRadSysSurf;
		using DataHeatBalFanSys::QHWBaseboardSurf;
		using DataHeatBalFanSys::QSteamBaseboardSurf;
		using DataHeatBalFanSys::QElecBaseboardSurf;
		using DataHeatBalSurface::QRadSWInAbs;
		using FluidProperties::GetSpecificHeatGlycol;
		using DataHeatBalSurface::TH;
		using DataSurfaces::Surface;
		using DataGlobals::TimeStepZoneSec;
		using DataGlobals::SecInHour;
		using DataHVACGlobals::TimeStepSys;
		using DataHeatBalance::Construct;
		using DataHeatBalSurface::CTFConstInPart;
		using DataHeatBalFanSys::QPoolSurfNumerator;
		using DataHeatBalFanSys::PoolHeatTransCoefs;
		using DataLoopNode::Node;
		using PlantUtilities::SetComponentFlowRate;
		using DataHeatBalFanSys::SumConvPool;
		using DataHeatBalFanSys::SumLatentPool;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "CalcSwimmingPool" );
		static Real64 const CFinHg( 0.00029613 ); // Multiple pressure in Pa by this constant to get inches of Hg

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 HConvIn; // convection coefficient for pool
		Real64 EvapRate; // evaporation rate for pool in kg/s
		Real64 EvapEnergyLossPerArea; // energy effect of evaporation rate per unit area in W/m2
		Real64 PSatPool; // saturation pressure at pool water temperature
		Real64 PParAir; // partial pressure of vapor of zone air
		int ZoneNum; // index to zone array
		Real64 LWtotal; // total flux from long-wavelength radiation to surface
		Real64 LWsum; // summation of all long-wavelenth radiation going to surface
		Real64 SWtotal; // total flux from short-wavelength radiation to surface
		Real64 Cp; // specific heat of pool water
		Real64 TH11; // current outside surface temperature
		Real64 TH22; // previous pool water temperature
		int ConstrNum; // construction number index
		Real64 PeopleGain; // heat gain from people in pool (assumed to be all convective)
		int SurfNum; // surface number of floor that is the pool
		Real64 TInSurf; // Setpoint temperature for pool which is also the goal temperature and also the inside surface face temperature
		Real64 Tmuw; // Inlet makeup water temperature
		Real64 TLoopInletTemp; // Inlet water temperature from the plant loop
		Real64 CondTerms; // Conduction terms for the "surface" heat balance
		Real64 ConvTerm; // Convection term for the "surface" heat balance
		Real64 PoolMassTerm; // Pool mass * Cp / Time Step
		Real64 MUWTerm; // Makeup water term for the "surface" heat balance
		Real64 CpDeltaTi; // inverse of specific heat of water times the plant loop temperature difference
		Real64 MassFlowRate; // Target mass flow rate to achieve the proper setpoint temperature

		// FLOW:
		// initialize local variables
		SurfNum = Pool( PoolNum ).SurfacePtr;
		ZoneNum = Surface( SurfNum ).Zone;

		// Convection coefficient calculation
		HConvIn = 0.22 * std::pow( abs( Pool( PoolNum ).PoolWaterTemp - MAT( ZoneNum ) ), 1.0 / 3.0 ) * Pool( PoolNum ).CurCoverConvFac;

		// Evaporation calculation:
		// Evaporation Rate (lb/h) = 0.1 * Area (ft2) * Activity Factor * (Psat,pool - Ppar,air) (in Hg)
		// So evaporation rate, area, and pressures have to be converted to standard E+ units (kg/s, m2, and Pa, respectively)
		// Evaporation Rate per Area = Evaporation Rate * Heat of Vaporization / Area of Surface
		PSatPool = PsyPsatFnTemp( Pool( PoolNum ).PoolWaterTemp, RoutineName );
		PParAir = PsyPsatFnTemp( MAT( ZoneNum ), RoutineName ) * PsyRhFnTdbWPb( MAT( ZoneNum ), ZoneAirHumRatAvg( ZoneNum ), OutBaroPress );
		if ( PSatPool < PParAir ) PSatPool = PParAir;
		EvapRate = ( 0.1 * ( Surface( SurfNum ).Area / CFA ) * Pool( PoolNum ).CurActivityFactor * ( ( PSatPool - PParAir ) * CFinHg ) ) * CFMF * Pool( PoolNum ).CurCoverEvapFac;
		Pool( PoolNum ).MakeUpWaterMassFlowRate = EvapRate;
		EvapEnergyLossPerArea = -EvapRate *  PsyHfgAirFnWTdb( ZoneAirHumRatAvg( ZoneNum ), MAT( ZoneNum ) ) / Surface( SurfNum ).Area;
		Pool( PoolNum ).EvapHeatLossRate = EvapEnergyLossPerArea * Surface( SurfNum ).Area;
		// LW and SW radiation term modification: any "excess" radiation blocked by the cover gets convected
		// to the air directly and added to the zone air heat balance
		LWsum = ( QRadThermInAbs( SurfNum ) +  NetLWRadToSurf( SurfNum ) + QHTRadSysSurf( SurfNum ) + QHWBaseboardSurf( SurfNum ) + QSteamBaseboardSurf( SurfNum ) + QElecBaseboardSurf( SurfNum ) );
		LWtotal = Pool( PoolNum ).CurCoverLWRadFac * LWsum;
		SWtotal = Pool( PoolNum ).CurCoverSWRadFac * QRadSWInAbs( SurfNum );
		Pool( PoolNum ).RadConvertToConvect = ( ( 1.0 - Pool( PoolNum ).CurCoverLWRadFac ) * LWsum ) + ( ( 1.0 - Pool( PoolNum ).CurCoverSWRadFac ) * QRadSWInAbs( SurfNum ) );

		// Heat gain from people (assumed to be all convective to pool water)
		PeopleGain = Pool( PoolNum ).PeopleHeatGain / Surface( SurfNum ).Area;

		// Get an estimate of the pool water specific heat
		Cp = GetSpecificHeatGlycol( "WATER", Pool( PoolNum ).PoolWaterTemp, Pool( PoolNum ).GlycolIndex, RoutineName );

		TH22 = TH( 2, 2, SurfNum ); // inside surface temperature at the previous time step equals the old pool water temperature
		TH11 = TH( 1, 1, SurfNum ); // outside surface temperature at the current time step
		ConstrNum = Surface( SurfNum ).Construction;
		TInSurf = Pool( PoolNum ).CurSetPtTemp;
		Tmuw = Pool( PoolNum ).CurMakeupWaterTemp;
		TLoopInletTemp = Node( Pool( PoolNum ).WaterInletNode ).Temp;
		Pool( PoolNum ).WaterInletTemp = TLoopInletTemp;

		CondTerms = CTFConstInPart( SurfNum ) + Construct( ConstrNum ).CTFCross( 0 ) * TH11 - Construct( ConstrNum ).CTFInside( 0 ) * TInSurf;
		ConvTerm = HConvIn * ( MAT( ZoneNum ) - TInSurf );
		PoolMassTerm = Pool( PoolNum ).WaterMass * Cp * ( TH22 - TInSurf ) / ( TimeStepSys * SecInHour ) / Surface( SurfNum ).Area; // Use TimeStepSys here because this is a calculation for how much heat to add at the system time step and it is not a surface heat balance being done at the zone time step level
		MUWTerm = EvapRate * Cp * ( Tmuw - TInSurf ) / Surface( SurfNum ).Area;
		if ( TLoopInletTemp <= TInSurf ) {
			CpDeltaTi = 0.0;
		} else {
			CpDeltaTi = 1.0 / ( Cp * ( TInSurf - TLoopInletTemp ) );
		}
		// Now calculate the requested mass flow rate from the plant loop to achieve the proper pool temperature
		// old equation using surface heat balance form: MassFlowRate = CpDeltaTi * ( CondTerms + ConvTerm + SWtotal + LWtotal + PeopleGain + PoolMassTerm + MUWTerm + EvapEnergyLossPerArea );
		MassFlowRate = ( Pool( PoolNum ).WaterMass / ( TimeStepSys * SecInHour ) ) * ( ( TInSurf - TH22 ) / ( TLoopInletTemp - TInSurf ) );
		if ( MassFlowRate > Pool( PoolNum ).WaterMassFlowRateMax ) {
			MassFlowRate = Pool( PoolNum ).WaterMassFlowRateMax;
		} else if ( MassFlowRate < 0.0 ) {
			MassFlowRate = 0.0;
		}
		SetComponentFlowRate( MassFlowRate, Pool( PoolNum ).WaterInletNode, Pool( PoolNum ).WaterOutletNode, Pool( PoolNum ).HWLoopNum, Pool( PoolNum ).HWLoopSide, Pool( PoolNum ).HWBranchNum, Pool( PoolNum ).HWCompNum );
		Pool( PoolNum ).WaterMassFlowRate = MassFlowRate;

		// We now have a flow rate so we can assemble the terms needed for the surface heat balance that is solved for the inside face temperature
		QPoolSurfNumerator( SurfNum ) = SWtotal + LWtotal + PeopleGain + EvapEnergyLossPerArea + HConvIn * MAT( ZoneNum ) + ( EvapRate * Tmuw + MassFlowRate * TLoopInletTemp +  ( Pool( PoolNum ).WaterMass * TH22 / TimeStepZoneSec ) ) * Cp / Surface( SurfNum ).Area;
		PoolHeatTransCoefs( SurfNum ) = HConvIn + ( EvapRate + MassFlowRate +  ( Pool( PoolNum ).WaterMass / TimeStepZoneSec ) ) * Cp / Surface( SurfNum ).Area;

		// Finally take care of the latent and convective gains resulting from the pool
		SumConvPool( ZoneNum ) += Pool( PoolNum ).RadConvertToConvect;
		SumLatentPool( ZoneNum ) += EvapRate *  PsyHfgAirFnWTdb( ZoneAirHumRatAvg( ZoneNum ), MAT( ZoneNum ) );
	}

	void
	UpdateSwimmingPool(
		int const PoolNum // number of the swimming pool
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand, Ho-Sung Kim
		//       DATE WRITTEN   October 2014
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine does any updating that needs to be done for the
		// swimming pool model.

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::TimeStepZone;
		using DataHVACGlobals::TimeStepSys;
		using DataHVACGlobals::SysTimeElapsed;
		using DataLoopNode::Node;
		using FluidProperties::GetSpecificHeatGlycol;
		using DataPlant::PlantLoop;
		using PlantUtilities::SafeCopyPlantNode;
		using PlantUtilities::SetComponentFlowRate;
		using DataHeatBalFanSys::QPoolSurfNumerator;
		using DataHeatBalFanSys::PoolHeatTransCoefs;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "UpdateSwimmingPool" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int SurfNum; // surface number/pointer
		int WaterInletNode; // inlet node number
		int WaterOutletNode; // outlet node number
		Real64 WaterMassFlow; // water mass flow rate

		// FLOW:

		SurfNum = Pool( PoolNum ).SurfacePtr;

		if ( LastSysTimeElapsed( SurfNum ) == SysTimeElapsed ) {
			// Still iterating or reducing system time step, so subtract old values which were
			// not valid
			QPoolSrcAvg( SurfNum ) -= LastQPoolSrc( SurfNum ) * LastTimeStepSys( SurfNum ) / TimeStepZone;
			HeatTransCoefsAvg( SurfNum ) -= LastHeatTransCoefs( SurfNum ) * LastTimeStepSys( SurfNum ) / TimeStepZone;
		}

		// Update the running average and the "last" values with the current values of the appropriate variables
		QPoolSrcAvg( SurfNum ) += QPoolSurfNumerator( SurfNum ) * TimeStepSys / TimeStepZone;
		HeatTransCoefsAvg( SurfNum ) += PoolHeatTransCoefs( SurfNum ) * TimeStepSys / TimeStepZone;

		LastQPoolSrc( SurfNum ) = QPoolSurfNumerator( SurfNum );
		LastHeatTransCoefs( SurfNum ) = PoolHeatTransCoefs( SurfNum );
		LastSysTimeElapsed( SurfNum ) = SysTimeElapsed;
		LastTimeStepSys( SurfNum ) = TimeStepSys;

		WaterInletNode = Pool( PoolNum ).WaterInletNode;
		WaterOutletNode = Pool( PoolNum ).WaterOutletNode;
		SafeCopyPlantNode( WaterInletNode, WaterOutletNode );

		WaterMassFlow = Node( WaterInletNode ).MassFlowRate;
		if ( WaterMassFlow > 0.0 ) Node( WaterOutletNode ).Temp = Pool( PoolNum ).PoolWaterTemp;

	}

	void
	UpdatePoolSourceValAvg( bool & SwimmingPoolOn ) // .TRUE. if the swimming pool "runs" this zone time step
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   October 2014
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// To transfer the average value of the pool heat balance term over the entire
		// zone time step back to the heat balance routines so that the heat
		// balance algorithms can simulate one last time with the average source
		// to maintain some reasonable amount of continuity and energy balance
		// in the temperature and flux histories.

		// METHODOLOGY EMPLOYED:
		// All of the record keeping for the average term is done in the Update
		// routine so the only other thing that this subroutine does is check to
		// see if the system was even on.  If any average term is non-zero, then
		// one or more of the swimming pools was running.  Method borrowed from
		// radiant systems.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		using DataHeatBalFanSys::QPoolSurfNumerator;
		using DataHeatBalFanSys::PoolHeatTransCoefs;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const CloseEnough( 0.01 ); // Some arbitrarily small value to avoid zeros and numbers that are almost the same

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int SurfNum; // DO loop counter for surface index

		// FLOW:
		SwimmingPoolOn = false;

		// If this was never allocated, then there are no radiant systems in this input file (just RETURN)
		if ( ! allocated( QPoolSrcAvg ) ) return;

		// If it was allocated, then we have to check to see if this was running at all...
		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
			if ( QPoolSrcAvg( SurfNum ) != 0.0 ) {
				SwimmingPoolOn = true;
				break; //DO loop
			}
		}

		QPoolSurfNumerator = QPoolSrcAvg;
		PoolHeatTransCoefs = HeatTransCoefsAvg;

		// For interzone surfaces, QPoolSrcAvg was only updated for the "active" side.  The active side
		// would have a non-zero value at this point.  If the numbers differ, then we have to manually update.
		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
			if ( Surface( SurfNum ).ExtBoundCond > 0 && Surface( SurfNum ).ExtBoundCond != SurfNum ) {
				if ( std::abs( QPoolSurfNumerator( SurfNum ) - QPoolSurfNumerator( Surface( SurfNum ).ExtBoundCond ) ) > CloseEnough ) { // numbers differ
					if ( std::abs( QPoolSurfNumerator( SurfNum ) ) > std::abs( QPoolSurfNumerator( Surface( SurfNum ).ExtBoundCond ) ) ) {
						QPoolSurfNumerator( Surface( SurfNum ).ExtBoundCond ) = QPoolSurfNumerator( SurfNum );
					} else {
						QPoolSurfNumerator( SurfNum ) = QPoolSurfNumerator( Surface( SurfNum ).ExtBoundCond );
					}
				}
			}
		}
		// For interzone surfaces, PoolHeatTransCoefs was only updated for the "active" side.  The active side
		// would have a non-zero value at this point.  If the numbers differ, then we have to manually update.
		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
			if ( Surface( SurfNum ).ExtBoundCond > 0 && Surface( SurfNum ).ExtBoundCond != SurfNum ) {
				if ( std::abs( PoolHeatTransCoefs( SurfNum ) - PoolHeatTransCoefs( Surface( SurfNum ).ExtBoundCond ) ) > CloseEnough ) { // numbers differ
					if ( std::abs( PoolHeatTransCoefs( SurfNum ) ) > std::abs( PoolHeatTransCoefs( Surface( SurfNum ).ExtBoundCond ) ) ) {
						PoolHeatTransCoefs( Surface( SurfNum ).ExtBoundCond ) = PoolHeatTransCoefs( SurfNum );
					} else {
						PoolHeatTransCoefs( SurfNum ) = PoolHeatTransCoefs( Surface( SurfNum ).ExtBoundCond );
					}
				}
			}
		}

	}

	Real64
	SumHATsurf( int const ZoneNum ) // Zone number
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   July 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function calculates the zone sum of Hc*Area*Tsurf.  It replaces the old SUMHAT.
		// The SumHATsurf code below is also in the CalcZoneSums subroutine in ZoneTempPredictorCorrector
		// and should be updated accordingly.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataSurfaces;
		using namespace DataHeatBalance;
		using namespace DataHeatBalSurface;

		// Return value
		Real64 SumHATsurf;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int SurfNum; // Surface number
		Real64 Area; // Effective surface area

		// FLOW:
		SumHATsurf = 0.0;

		for ( SurfNum = Zone( ZoneNum ).SurfaceFirst; SurfNum <= Zone( ZoneNum ).SurfaceLast; ++SurfNum ) {
			if ( ! Surface( SurfNum ).HeatTransSurf ) continue; // Skip non-heat transfer surfaces

			Area = Surface( SurfNum ).Area;

			if ( Surface( SurfNum ).Class == SurfaceClass_Window ) {
				if ( SurfaceWindow( SurfNum ).ShadingFlag == IntShadeOn || SurfaceWindow( SurfNum ).ShadingFlag == IntBlindOn ) {
					// The area is the shade or blind are = sum of the glazing area and the divider area (which is zero if no divider)
					Area += SurfaceWindow( SurfNum ).DividerArea;
				}

				if ( SurfaceWindow( SurfNum ).FrameArea > 0.0 ) {
					// Window frame contribution
					SumHATsurf += HConvIn( SurfNum ) * SurfaceWindow( SurfNum ).FrameArea * ( 1.0 + SurfaceWindow( SurfNum ).ProjCorrFrIn ) * SurfaceWindow( SurfNum ).FrameTempSurfIn;
				}

				if ( SurfaceWindow( SurfNum ).DividerArea > 0.0 && SurfaceWindow( SurfNum ).ShadingFlag != IntShadeOn && SurfaceWindow( SurfNum ).ShadingFlag != IntBlindOn ) {
					// Window divider contribution (only from shade or blind for window with divider and interior shade or blind)
					SumHATsurf += HConvIn( SurfNum ) * SurfaceWindow( SurfNum ).DividerArea * ( 1.0 + 2.0 * SurfaceWindow( SurfNum ).ProjCorrDivIn ) * SurfaceWindow( SurfNum ).DividerTempSurfIn;
				}
			}

			SumHATsurf += HConvIn( SurfNum ) * Area * TempSurfInTmp( SurfNum );
		}

		return SumHATsurf;

	}

	void
	ReportSwimmingPool()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand, Ho-Sung Kim
		//       DATE WRITTEN   October 2014
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine simply produces output for the swimming pool model.

		// METHODOLOGY EMPLOYED:
		// Standard EnergyPlus methodology.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::TimeStepZone;
		using DataGlobals::SecInHour;
		using DataHVACGlobals::TimeStepSys;
		using DataSurfaces::Surface;
		using FluidProperties::GetSpecificHeatGlycol;
		using FluidProperties::GetDensityGlycol;
		using DataHeatBalSurface::TH;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "ReportSwimmingPool" );
		Real64 const MinDensity = 1.0;	// to avoid a divide by zero

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int PoolNum; // pool number index
		int SurfNum; // surface number index
		Real64 Cp; // specific heat of water
		Real64 Density; // density of water

		// FLOW:
		for ( PoolNum = 1; PoolNum <= NumSwimmingPools; ++PoolNum ) {

			SurfNum = Pool( PoolNum ).SurfacePtr;

			// First transfer the surface inside temperature data to the current pool water temperature
			Pool( PoolNum ).PoolWaterTemp = TH( 2, 1, SurfNum );

			// Next calculate the amount of heating done by the plant loop
			Cp = GetSpecificHeatGlycol( "WATER", Pool( PoolNum ).PoolWaterTemp, Pool( PoolNum ).GlycolIndex, RoutineName );
			Pool( PoolNum ).HeatPower = Pool( PoolNum ).WaterMassFlowRate * Cp * ( Pool( PoolNum ).WaterInletTemp - Pool( PoolNum ).PoolWaterTemp );

			// Now the power consumption of miscellaneous equipment
			Density = GetDensityGlycol("WATER", Pool( PoolNum ).PoolWaterTemp, Pool( PoolNum ).GlycolIndex, RoutineName );
			if ( Density > MinDensity ) {
				Pool( PoolNum ).MiscEquipPower = Pool( PoolNum ).MiscPowerFactor * Pool( PoolNum ).WaterMassFlowRate / Density;
			} else {
				Pool( PoolNum ).MiscEquipPower = 0.0;
			}

			// Also the radiant exchange converted to convection by the pool cover
			Pool( PoolNum ).RadConvertToConvectRep = Pool( PoolNum ).RadConvertToConvect * Surface( SurfNum ).Area;

			// Finally calculate the summed up report variables
			Pool( PoolNum ).MiscEquipEnergy = Pool( PoolNum ).MiscEquipPower * TimeStepSys * SecInHour;
			Pool( PoolNum ).HeatEnergy = Pool( PoolNum ).HeatPower * TimeStepSys * SecInHour;
			Pool( PoolNum ).MakeUpWaterMass = Pool( PoolNum ).MakeUpWaterMassFlowRate * TimeStepSys * SecInHour;
			Pool( PoolNum ).EvapEnergyLoss = Pool( PoolNum ).EvapHeatLossRate * TimeStepSys * SecInHour;

			Pool( PoolNum ).MakeUpWaterVolFlowRate = MakeUpWaterVolFlowFunct(Pool( PoolNum ).MakeUpWaterMassFlowRate, Density);
			Pool( PoolNum ).MakeUpWaterVol = MakeUpWaterVolFunct(Pool( PoolNum ).MakeUpWaterMass, Density);
		}

	}

	Real64
	MakeUpWaterVolFlowFunct( Real64 MakeUpWaterMassFlowRate, Real64 Density )
	{
		Real64 MakeUpWaterVolumeFlow;
		MakeUpWaterVolumeFlow = MakeUpWaterMassFlowRate / Density;
		return MakeUpWaterVolumeFlow;
	}

	Real64
	MakeUpWaterVolFunct( Real64 MakeUpWaterMass, Real64 Density )
	{
		Real64 MakeUpWaterVolume;
		MakeUpWaterVolume = MakeUpWaterMass / Density;
		return MakeUpWaterVolume;
	}

} // SwimmingPool

} // EnergyPlus
