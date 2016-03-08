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
#include <cmath>
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/ArrayS.functions.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <HeatBalanceManager.hh>
#include <ConductionTransferFunctionCalc.hh>
#include <DataBSDFWindow.hh>
#include <DataComplexFenestration.hh>
#include <DataContaminantBalance.hh>
#include <DataDaylighting.hh>
#include <DataEnvironment.hh>
#include <DataGlobals.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHeatBalSurface.hh>
#include <DataIPShortCuts.hh>
#include <DataPrecisionGlobals.hh>
#include <DataReportingFlags.hh>
#include <DataRoomAirModel.hh>
#include <DataStringGlobals.hh>
#include <DataSurfaces.hh>
#include <DataSystemVariables.hh>
#include <DataWindowEquivalentLayer.hh>
#include <DaylightingDevices.hh>
#include <DaylightingManager.hh>
#include <DisplayRoutines.hh>
#include <EconomicTariff.hh>
#include <EMSManager.hh>
#include <General.hh>
#include <HeatBalanceSurfaceManager.hh>
#include <HVACSizingSimulationManager.hh>
#include <InputProcessor.hh>
#include <InternalHeatGains.hh>
#include <MatrixDataManager.hh>
#include <NodeInputManager.hh>
#include <OutputProcessor.hh>
#include <OutputReportTabular.hh>
#include <ScheduleManager.hh>
#include <SolarShading.hh>
#include <SurfaceGeometry.hh>
#include <SurfaceOctree.hh>
#include <UtilityRoutines.hh>
#include <WindowComplexManager.hh>
#include <WindowEquivalentLayer.hh>
#include <WindowManager.hh>

namespace EnergyPlus {

namespace HeatBalanceManager {

	// Module containing the heat balance simulation routines
	// calculation (initialization) routines

	// MODULE INFORMATION:
	//       AUTHOR         Richard J. Liesen
	//       DATE WRITTEN   February 1998
	//       MODIFIED       November 1998, FW
	//       MODIFIED       April 1999, LKL
	//       MODIFIED       Dec 2006 DJS of PSU for ecoroof
	//       Added          Dec 2008 TH for thermochromic windows:
	//                       new subroutine CreateTCConstructions called by GetHeatBalanceInput
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// To encapsulate the data and algorithms required to
	// manage the heat balance simulation on the building.

	// METHODOLOGY EMPLOYED:

	// REFERENCES:
	// The heat balance method is outlined in the "Tarp Algorithms Manual"
	// The methods are also summarized in many BSO Theses and papers.

	// OTHER NOTES:
	// This module was created from IBLAST subroutines

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataComplexFenestration;
	using namespace DataGlobals;
	using namespace DataEnvironment;
	using namespace DataHeatBalFanSys;
	using namespace DataHeatBalance;
	using namespace DataHeatBalSurface;
	using namespace DataRoomAirModel;
	using namespace DataIPShortCuts;
	// Use statements for access to subroutines in other modules
	using InputProcessor::GetNumObjectsFound;
	using InputProcessor::GetObjectItem;
	using InputProcessor::SameString;
	using InputProcessor::FindItemInList;
	using InputProcessor::VerifyName;
	using InputProcessor::GetObjectItemNum;
	using ScheduleManager::GetScheduleIndex;
	using DataSurfaces::TotSurfaces;
	using DataSurfaces::FrameDivider;
	using DataSurfaces::FrameDividerProperties;
	using DataSurfaces::CalcSolRefl;
	using DataSurfaces::SurfaceWindow;
	using DataSurfaces::StormWindow;
	using DataSurfaces::TotStormWin;
	using DataSurfaces::DividedLite;
	using DataSurfaces::Suspended;
	using DataSurfaces::ShadingTransmittanceVaries;
	using WindowManager::W5LsqFit;
	using DataContaminantBalance::Contaminant;
	using DataContaminantBalance::ZoneAirCO2;
	using DataContaminantBalance::ZoneAirCO2Temp;
	using DataContaminantBalance::ZoneAirCO2Avg;
	using DataContaminantBalance::OutdoorCO2;
	using DataContaminantBalance::ZoneAirGC;
	using DataContaminantBalance::ZoneAirGCTemp;
	using DataContaminantBalance::ZoneAirGCAvg;
	using DataContaminantBalance::OutdoorGC;
	using ScheduleManager::GetCurrentScheduleValue;
	using WindowComplexManager::CalculateBasisLength;
	using DataWindowEquivalentLayer::TotWinEquivLayerConstructs;

	// Data
	// MODULE PARAMETER DEFINITIONS
	static std::string const BlankString;
	static gio::Fmt fmtA( "(A)" );

	Array1D_string const PassFail( 2, { "Fail", "Pass" } );

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:

	namespace {
		// These were static variables within different functions. They were pulled out into the namespace
		// to facilitate easier unit testing of those functions.
		// These are purposefully not in the header file as an extern variable. No one outside of this should
		// use these. They are cleared by clear_state() for use by unit tests, but normal simulations should be unaffected.
		// This is purposefully in an anonymous namespace so nothing outside this implementation file can use it.
		bool ManageHeatBalanceGetInputFlag( true );
	}


	//Real Variables for the Heat Balance Simulation
	//Variables used to determine warmup convergence
	Array1D< Real64 > MaxCoolLoadPrevDay; // Max cooling load from the previous day
	Array1D< Real64 > MaxCoolLoadZone; // Maximum zone cooling load from the current day
	Array1D< Real64 > MaxHeatLoadPrevDay; // Max heating load from the previous day
	Array1D< Real64 > MaxHeatLoadZone; // Maximum zone heating load from the current day
	Array1D< Real64 > MaxTempPrevDay; // Max temperature from the previous day
	Array1D< Real64 > MaxTempZone; // Maximum zone temperature from the current day
	Array1D< Real64 > MinTempPrevDay; // Min temperature from the previous day
	Array1D< Real64 > MinTempZone; // Minimum zone temperature from the current day

	//Variables used to report difference in temperature and load from the last two warmup days
	Array1D< Real64 > WarmupTempDiff; // Temperature difference between the last two warmup days
	Array1D< Real64 > WarmupLoadDiff; // Zone load differences between the last two warmup days
	Array1D< Real64 > TempZoneSecPrevDay; // Zone air temperature from the second last warmup day
	Array1D< Real64 > LoadZoneSecPrevDay; // Zone load from the second last warmup day
	Array1D< Real64 > TempZonePrevDay; // Zone air temperature from the previous day
	Array1D< Real64 > LoadZonePrevDay; // Zone load from the previuos day
	Array1D< Real64 > TempZone; // Zone air temperature from the current warmup day
	Array1D< Real64 > LoadZone; // Zone load from the current warmup day

	Array2D< Real64 > TempZoneRpt; // Zone air temperature to report (average over all warmup days)
	Array1D< Real64 > TempZoneRptStdDev; // Zone air temperature to report (std dev over all warmup days)
	Array2D< Real64 > LoadZoneRpt; // Zone load to report (average over all warmup days)
	Array1D< Real64 > LoadZoneRptStdDev; // Zone load to report (std dev over all warmup days)
	Array2D< Real64 > MaxLoadZoneRpt; // Maximum zone load for reporting calcs
	int CountWarmupDayPoints; // Count of warmup timesteps (to achieve warmup)

	std::string CurrentModuleObject; // to assist in getting input

	// Subroutine Specifications for the Heat Balance Module
	// Driver Routines

	// Input reader routines for the module

	// Initialization routines for module

	// Record Keeping/Utility Routines for Module

	// Reporting routines for module

	// Object Data
	Array1D< WarmupConvergence > WarmupConvergenceValues;

	// MODULE SUBROUTINES:
	//*************************************************************************

	// Functions

	// Clears the global data in HeatBalanceManager.
	// Needed for unit tests, should not be normally called.
	void
	clear_state()
	{
		ManageHeatBalanceGetInputFlag = true;
		MaxCoolLoadPrevDay.deallocate();
		MaxCoolLoadZone.deallocate();
		MaxHeatLoadPrevDay.deallocate();
		MaxHeatLoadZone.deallocate();
		MaxTempPrevDay.deallocate();
		MaxTempZone.deallocate();
		MinTempPrevDay.deallocate();
		MinTempZone.deallocate();
		WarmupTempDiff.deallocate();
		WarmupLoadDiff.deallocate();
		TempZoneSecPrevDay.deallocate();
		LoadZoneSecPrevDay.deallocate();
		TempZonePrevDay.deallocate();
		LoadZonePrevDay.deallocate();
		TempZone.deallocate();
		LoadZone.deallocate();
		TempZoneRpt.deallocate();
		TempZoneRptStdDev.deallocate();
		LoadZoneRpt.deallocate();
		LoadZoneRptStdDev.deallocate();
		MaxLoadZoneRpt.deallocate();
		CountWarmupDayPoints = int();
		CurrentModuleObject = std::string();
		WarmupConvergenceValues.deallocate();
	}

	void
	ManageHeatBalance()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   January 1997
		//       MODIFIED       February 1998 Richard Liesen
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine manages the heat balance method of calculating
		// building thermal loads.  It is called from the SimulationManager
		// at the time step level.  This driver manages the calls to all of
		// the other modules, drivers, and simulation algorithms.

		// METHODOLOGY EMPLOYED:
		// The order of this routine was taken from HeatBalanceModule with routine
		//  and Data Structuring

		// REFERENCES:
		// Legacy code from (I)BLAST, subroutine SIMZGD.

		// Using/Aliasing
		using namespace HeatBalanceSurfaceManager;
		using EMSManager::ManageEMS;
		using EMSManager::UpdateEMSTrendVariables;
		using DataGlobals::emsCallFromEndZoneTimestepBeforeZoneReporting;
		using DataGlobals::emsCallFromEndZoneTimestepAfterZoneReporting;
		using DataGlobals::emsCallFromBeginNewEvironmentAfterWarmUp;

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
		//////////// hoisted into namespace changed ManageHeatBalanceGetInputFlag////////////
		// static bool GetInputFlag( true );
		////////////////////////////////////////////////

		// FLOW:

		// Get the heat balance input at the beginning of the simulation only
		if ( ManageHeatBalanceGetInputFlag ) {
			GetHeatBalanceInput(); // Obtains heat balance related parameters from input file

			// Surface octree setup
			//  The surface octree holds live references to surfaces so it must be updated
			//   if in the future surfaces are altered after this point
			if ( TotSurfaces >= DaylightingManager::octreeCrossover ) { // Octree can be active
				if ( GetNumObjectsFound( "Daylighting:Controls" ) > 0 ) { // Daylighting is active
					surfaceOctree.init( DataSurfaces::Surface ); // Set up surface octree
				}
			}

			for ( auto & surface : DataSurfaces::Surface ) surface.set_computed_geometry(); // Set up extra surface geometry info for PierceSurface

			ManageHeatBalanceGetInputFlag = false;
		}

		// These Inits will still have to be looked at as the routines are re-engineered further
		InitHeatBalance(); // Initialize all heat balance related parameters

		// Solve the zone heat balance by first calling the Surface Heat Balance Manager
		// and then the Air Heat Balance Manager is called by the Surface Heat Balance
		// Manager.  The order of execution is still important and the zone cannot
		// go through any record keeping before the HVAC system has run because there
		// may be a radiant system in the building which will require iteration between
		// the HVAC system (called from the Air Heat Balance) and the zone (simulated
		// in the Surface Heat Balance Manager).  In the future, this may be improved.
		ManageSurfaceHeatBalance();
		ManageEMS( emsCallFromEndZoneTimestepBeforeZoneReporting ); // EMS calling point
		RecKeepHeatBalance(); // Do any heat balance related record keeping

		// This call has been moved to the FanSystemModule and does effect the output file
		//   You do get a shift in the Air Handling System Summary for the building electric loads
		// IF ((.NOT.WarmupFlag).AND.(DayOfSim.GT.0)) CALL RCKEEP  ! Do fan system accounting (to be moved later)

		ReportHeatBalance(); // Manage heat balance reporting until the new reporting is in place

		ManageEMS( emsCallFromEndZoneTimestepAfterZoneReporting ); // EMS calling point

		UpdateEMSTrendVariables();

		if ( WarmupFlag && EndDayFlag ) {

			CheckWarmupConvergence();
			if ( ! WarmupFlag ) {
				DayOfSim = 0; // Reset DayOfSim if Warmup converged
				DayOfSimChr = "0";

				ManageEMS( emsCallFromBeginNewEvironmentAfterWarmUp ); // calling point
			}

		}

		if ( ! WarmupFlag && EndDayFlag && DayOfSim == 1 && ! DoingSizing ) {
			ReportWarmupConvergence();
		}

	}

	// Get Input Section of the Module
	//******************************************************************************

	void
	GetHeatBalanceInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   September 1997
		//       MODIFIED       February 1998 Richard Liesen
		//                      November 1998 FW
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is the main driver for initializations within the
		// heat balance.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InternalHeatGains::ManageInternalHeatGains;

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
		static bool ErrorsFound( false ); // If errors detected in input
		bool ValidSimulationWithNoZones;

		// FLOW:

		GetProjectControlData( ErrorsFound );

		GetSiteAtmosphereData( ErrorsFound );

		GetWindowGlassSpectralData( ErrorsFound );

		GetMaterialData( ErrorsFound ); // Read materials from input file/transfer from legacy data structure

		GetFrameAndDividerData( ErrorsFound );

		GetConstructData( ErrorsFound ); // Read constructs from input file/transfer from legacy data structure

		GetBuildingData( ErrorsFound ); // Read building data from input file

		// Added SV 6/26/2013 to load scheduled surface gains
		GetScheduledSurfaceGains( ErrorsFound );

		// Added TH 1/9/2009 to create thermochromic window constructions
		CreateTCConstructions( ErrorsFound );

		if ( TotSurfaces > 0 && NumOfZones == 0 ) {
			ValidSimulationWithNoZones = CheckValidSimulationObjects();
			if ( ! ValidSimulationWithNoZones ) {
				ShowSevereError( "GetHeatBalanceInput: There are surfaces in input but no zones found.  Invalid simulation." );
				ErrorsFound = true;
			}
		}

		CheckUsedConstructions( ErrorsFound );

		if ( ErrorsFound ) {
			ShowFatalError( "Errors found in Building Input, Program Stopped" );
		}

		// following is done to "get internal heat gains" input so that lights are gotten before
		// daylighting input
		ManageInternalHeatGains( true );

	}

	void
	CheckUsedConstructions( bool & ErrorsFound )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   August 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Counts or details unused constructions.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::RoundSigDigits;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using namespace DataIPShortCuts;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const NumConstrObjects( 5 );
		static Array1D_string const ConstrObjects( NumConstrObjects, { "Pipe:Indoor", "Pipe:Outdoor", "Pipe:Underground", "GroundHeatExchanger:Surface", "DaylightingDevice:Tubular" } );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Unused;
		int Loop;
		int NumObjects;
		int NumAlphas;
		int NumNumbers;
		int Status;
		int CNum;
		int ONum;
		bool InErrFlag; // Preserve (no current use) the input status of ErrorsFound

		InErrFlag = ErrorsFound;

		// Needs to account for Pipe:HeatTransfer/indoor, etc constructions.
		for ( ONum = 1; ONum <= NumConstrObjects; ++ONum ) {
			NumObjects = GetNumObjectsFound( ConstrObjects( ONum ) );
			for ( Loop = 1; Loop <= NumObjects; ++Loop ) {
				GetObjectItem( ConstrObjects( ONum ), Loop, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, Status );
				if ( ONum != 5 ) {
					CNum = FindItemInList( cAlphaArgs( 2 ), Construct );
				} else {
					CNum = FindItemInList( cAlphaArgs( 4 ), Construct );
				}
				if ( CNum == 0 ) continue;
				Construct( CNum ).IsUsed = true;
			}
		}
		Unused = TotConstructs - std::count_if( Construct.begin(), Construct.end(), []( DataHeatBalance::ConstructionData const & e ){ return e.IsUsed; } );
		if ( Unused > 0 ) {
			if ( ! DisplayExtraWarnings ) {
				ShowWarningError( "CheckUsedConstructions: There are " + RoundSigDigits( Unused ) + " nominally unused constructions in input." );
				ShowContinueError( "For explicit details on each unused construction, use Output:Diagnostics,DisplayExtraWarnings;" );
			} else {
				ShowWarningError( "CheckUsedConstructions: There are " + RoundSigDigits( Unused ) + " nominally unused constructions in input." );
				ShowContinueError( "Each Unused construction is shown." );
				for ( Loop = 1; Loop <= TotConstructs; ++Loop ) {
					if ( Construct( Loop ).IsUsed ) continue;
					ShowMessage( "Construction=" + Construct( Loop ).Name );
				}
			}
		}

	}

	bool
	CheckValidSimulationObjects()
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   July 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// If an input file presents with surfaces but no zones, there are certain objects
		// that must be present for the simulation to be valid.  This check was necessitated by
		// an input file that was entirely detached shading surfaces but no zones (and nothing else).
		// Other objects include Solar Collectors, PV arrays.

		// METHODOLOGY EMPLOYED:
		// Check for specific objects that must be present for such a simulation to be valid.

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;

		// Return value
		bool ValidSimulation; // True is other objects appear to make this a valid simulation.

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na
		ValidSimulation = false;
		if ( GetNumObjectsFound( "SolarCollector:FlatPlate:Water" ) > 0 ) {
			ValidSimulation = true;
		} else if ( GetNumObjectsFound( "Generator:Photovoltaic" ) > 0 ) {
			ValidSimulation = true;
		} else if ( GetNumObjectsFound( "Generator:InternalCombustionEngine" ) > 0 ) {
			ValidSimulation = true;
		} else if ( GetNumObjectsFound( "Generator:CombustionTurbine" ) > 0 ) {
			ValidSimulation = true;
		} else if ( GetNumObjectsFound( "Generator:FuelCell" ) > 0 ) {
			ValidSimulation = true;
		} else if ( GetNumObjectsFound( "Generator:MicroCHP" ) > 0 ) {
			ValidSimulation = true;
		} else if ( GetNumObjectsFound( "Generator:MicroTurbine" ) > 0 ) {
			ValidSimulation = true;
		} else if ( GetNumObjectsFound( "Generator:WindTurbine" ) > 0 ) {
			ValidSimulation = true;
		}

		return ValidSimulation;

	}

	void
	SetPreConstructionInputParameters()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Edwin Lee
		//       DATE WRITTEN   October 2014
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine sets parameters that need to be established before any heat balance inputs are read

		int NumAlpha;
		int NumNumber;
		int IOStat;

		// Get all the construction objects to determine the max layers and use this as the value for DataHeatBalance::MaxSolidWinLayers
		// The variable MaxSolidWinLayers is initialized to zero to immediately catch any issues with timing of this routine

		// start by setting this to 5; it will satisfy the regular window constructions (Construction) and the Window5 files (Construction:WindowDataFile)
		MaxSolidWinLayers = 7;

		// Construction:ComplexFenestrationState have a limit of 10 layers, so set it up to 10 if they are present
		if ( GetNumObjectsFound( "Construction:ComplexFenestrationState" ) > 0 ) {
			MaxSolidWinLayers = max( MaxSolidWinLayers, 10 );
		}

		// then process the rest of the relevant constructions
		std::string constructName( "Construction:WindowEquivalentLayer" );
		int numConstructions( GetNumObjectsFound( constructName ) );
		for ( int constructionNum = 1; constructionNum <= numConstructions; ++constructionNum ) {
			GetObjectItem( constructName, constructionNum, cAlphaArgs, NumAlpha, rNumericArgs, NumNumber, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			int numLayersInThisConstruct( NumAlpha - 1 );
			MaxSolidWinLayers = max( MaxSolidWinLayers, numLayersInThisConstruct );
		}

		// construction types being ignored as they are opaque: Construction:CfactorUndergroundWall, Construction:FfactorGroundFloor, Construction:InternalSource


	}

	void
	GetProjectControlData( bool & ErrorsFound ) // Set to true if errors detected during getting data
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   October 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine gets the project control data before the rest of the building data (such as
		// materials) is obtained.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// This routine gets the following objects:
		// BUILDING
		// INSIDE CONVECTION ALGORITHM
		// OUTSIDE CONVECTION ALGORITHM
		// SOLUTION ALGORITHM
		// ASHRAE Handbook of Fundamentals, Chap 16, for the setting of Site Atmospheric defaults based
		//   on terrain.
		// ZoneAirHeatBalanceAlgorithm, Added by L. Gu, 12/09
		// ZoneAirContaminantBalance, Added by L. Gu, 06/10

		// Using/Aliasing
		using General::RoundSigDigits;
		using DataSystemVariables::lMinimalShadowing;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetProjectControlData: " );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Array1D_string AlphaName( 4 );
		Array1D< Real64 > BuildingNumbers( 5 );
		int NumAlpha;
		int NumNumber;
		int IOStat;
		int NumObjects;
		std::string::size_type TMP;

		// Formats
		static gio::Fmt Format_721( "('! <Building Information>, Building Name,North Axis {deg},Terrain, ',' Loads Convergence Tolerance Value,Temperature Convergence Tolerance Value, ',' Solar Distribution,Maximum Number of Warmup Days,Minimum Number of Warmup Days')" );
		static gio::Fmt Format_720( "(' Building Information',8(',',A))" );
		static gio::Fmt Format_722( "('! <Inside Convection Algorithm>, Algorithm {Simple | TARP | CeilingDiffuser | AdaptiveConvectionAlgorithm}',/,'Inside Convection Algorithm,',A)" );
		static gio::Fmt Format_723( "('! <Outside Convection Algorithm>, ','Algorithm {SimpleCombined | TARP | MoWitt | DOE-2 | AdaptiveConvectionAlgorithm}',/,'Outside Convection Algorithm,',A)" );
		static gio::Fmt Format_724( "('! <Sky Radiance Distribution>, Value {Anisotropic}',/,'Sky Radiance Distribution,Anisotropic')" );
		static gio::Fmt Format_726( "('! <Zone Air Solution Algorithm>, Value {ThirdOrderBackwardDifference | AnalyticalSolution | EulerMethod}')" );
		static gio::Fmt Format_727( "(' Zone Air Solution Algorithm, ',A)" );
		static gio::Fmt Format_728( "('! <Zone Air Carbon Dioxide Balance Simulation>, Simulation {Yes/No}, Carbon Dioxide Concentration')" );
		static gio::Fmt Format_730( "(' Zone Air Carbon Dioxide Balance Simulation, ',A,',',A)" );
		static gio::Fmt Format_729( "('! <Zone Air Generic Contaminant Balance Simulation>, Simulation {Yes/No}, Generic Contaminant Concentration')" );
		static gio::Fmt Format_731( "(' Zone Air Generic Contaminant Balance Simulation, ',A,',',A)" );
		static gio::Fmt Format_732( "('! <Zone Air Mass Flow Balance Simulation>, Enforce Mass Balance, Adjust Zone Mixing, Adjust Zone Infiltration {AddInfiltration | AdjustInfiltration | None}, Infiltration Zones {MixingSourceZonesOnly | AllZones}')" );
		static gio::Fmt Format_733( "(' Zone Air Mass Flow Balance Simulation, ',A,',',A,',',A,',',A)" );

		//Assign the values to the building data

		CurrentModuleObject = "Building";
		NumObjects = GetNumObjectsFound( CurrentModuleObject );

		if ( NumObjects > 0 ) {
			GetObjectItem( CurrentModuleObject, 1, AlphaName, NumAlpha, BuildingNumbers, NumNumber, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			// Building Name (remove certain characters)
			BuildingName = AlphaName( 1 );
			TMP = index( BuildingName, char( 1 ) );
			while ( TMP != std::string::npos ) {
				BuildingName[ TMP ] = ',';
				TMP = index( BuildingName, char( 1 ) );
			}
			TMP = index( BuildingName, char( 2 ) );
			while ( TMP != std::string::npos ) {
				BuildingName[ TMP ] = '!';
				TMP = index( BuildingName, char( 2 ) );
			}
			TMP = index( BuildingName, char( 3 ) );
			while ( TMP != std::string::npos ) {
				BuildingName[ TMP ] = '\\';
				TMP = index( BuildingName, char( 3 ) );
			}
			// Building Azimuth (no validation)
			BuildingAzimuth = mod( BuildingNumbers( 1 ), 360.0 );
			// Terrain
			if ( AlphaName( 2 ) == "COUNTRY" || AlphaName( 2 ) == "1" ) {
				SiteWindExp = 0.14;
				SiteWindBLHeight = 270.0;
				AlphaName( 2 ) = "Country";
			} else if ( AlphaName( 2 ) == "SUBURBS" || AlphaName( 2 ) == "2" || AlphaName( 2 ) == "SUBURB" ) {
				SiteWindExp = 0.22;
				SiteWindBLHeight = 370.0;
				AlphaName( 2 ) = "Suburbs";
			} else if ( AlphaName( 2 ) == "CITY" || AlphaName( 2 ) == "3" ) {
				SiteWindExp = 0.33;
				SiteWindBLHeight = 460.0;
				AlphaName( 2 ) = "City";
			} else if ( AlphaName( 2 ) == "OCEAN" ) {
				SiteWindExp = 0.10;
				SiteWindBLHeight = 210.0;
				AlphaName( 2 ) = "Ocean";
			} else if ( AlphaName( 2 ) == "URBAN" ) {
				SiteWindExp = 0.22;
				SiteWindBLHeight = 370.0;
				AlphaName( 2 ) = "Urban";
			} else {
				ShowSevereError( RoutineName + CurrentModuleObject + ": " + cAlphaFieldNames( 2 ) + " invalid=" + AlphaName( 2 ) );
				SiteWindExp = 0.14;
				SiteWindBLHeight = 270.0;
				AlphaName( 2 ) = AlphaName( 2 ) + "-invalid";
				ErrorsFound = true;
			}
			// Loads Convergence Tolerance Value
			LoadsConvergTol = BuildingNumbers( 2 );
			if ( LoadsConvergTol <= 0.0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + ": " + cNumericFieldNames( 2 ) + " value invalid, [" + RoundSigDigits( LoadsConvergTol, 3 ) + ']' );
				ErrorsFound = true;
			}
			// Temperature Convergence Tolerance Value
			TempConvergTol = BuildingNumbers( 3 );
			if ( TempConvergTol <= 0.0 ) {
				ShowSevereError( RoutineName + CurrentModuleObject + ": " + cNumericFieldNames( 2 ) + " value invalid, [" + RoundSigDigits( TempConvergTol, 3 ) + ']' );
				ErrorsFound = true;
			}
			// Solar Distribution
			if ( has_prefix( AlphaName( 3 ), "MIN" ) || AlphaName( 3 ) == "-1" || lMinimalShadowing ) {
				SolarDistribution = MinimalShadowing;
				AlphaName( 3 ) = "MinimalShadowing";
				CalcSolRefl = false;
			} else if ( AlphaName( 3 ) == "FULLEXTERIOR" || AlphaName( 3 ) == "0" ) {
				SolarDistribution = FullExterior;
				AlphaName( 3 ) = "FullExterior";
				CalcSolRefl = false;
			} else if ( AlphaName( 3 ) == "FULLINTERIORANDEXTERIOR" || AlphaName( 3 ) == "1" ) {
				SolarDistribution = FullInteriorExterior;
				AlphaName( 3 ) = "FullInteriorAndExterior";
				CalcSolRefl = false;
			} else if ( AlphaName( 3 ) == "FULLEXTERIORWITHREFLECTIONS" ) {
				SolarDistribution = FullExterior;
				AlphaName( 3 ) = "FullExteriorWithReflectionsFromExteriorSurfaces";
				CalcSolRefl = true;
			} else if ( AlphaName( 3 ) == "FULLINTERIORANDEXTERIORWITHREFLECTIONS" ) {
				SolarDistribution = FullInteriorExterior;
				AlphaName( 3 ) = "FullInteriorAndExteriorWithReflectionsFromExteriorSurfaces";
				CalcSolRefl = true;
			} else {
				ShowSevereError( RoutineName + CurrentModuleObject + ": " + cAlphaFieldNames( 3 ) + " invalid=" + AlphaName( 3 ) );
				ErrorsFound = true;
				AlphaName( 3 ) = AlphaName( 3 ) + "-invalid";
			}
			// Maximum Number of Warmup Days
			if ( ! lNumericFieldBlanks( 4 ) ) {
				MaxNumberOfWarmupDays = BuildingNumbers( 4 );
				if ( MaxNumberOfWarmupDays <= 0 ) {
					ShowSevereError( RoutineName + CurrentModuleObject + ": " + cNumericFieldNames( 4 ) + " invalid, [" + RoundSigDigits( MaxNumberOfWarmupDays ) + "], " + RoundSigDigits( DefaultMaxNumberOfWarmupDays ) + " will be used" );
					MaxNumberOfWarmupDays = DefaultMaxNumberOfWarmupDays;
				}
			} else {
				MaxNumberOfWarmupDays = DefaultMaxNumberOfWarmupDays;
			}
			// Minimum Number of Warmup Days
			if ( ! lNumericFieldBlanks( 5 ) ) {
				MinNumberOfWarmupDays = BuildingNumbers( 5 );
				if ( MinNumberOfWarmupDays <= 0 ) {
					ShowWarningError( RoutineName + CurrentModuleObject + ": " + cNumericFieldNames( 5 ) + " invalid, [" + RoundSigDigits( MinNumberOfWarmupDays ) + "], " + RoundSigDigits( DefaultMinNumberOfWarmupDays ) + " will be used" );
					MinNumberOfWarmupDays = DefaultMinNumberOfWarmupDays;
				}
			} else {
				MinNumberOfWarmupDays = DefaultMinNumberOfWarmupDays;
			}
			if ( MinNumberOfWarmupDays > MaxNumberOfWarmupDays ) {
				ShowWarningError( RoutineName + CurrentModuleObject + ": " + cNumericFieldNames( 5 ) + " [" + RoundSigDigits( MinNumberOfWarmupDays ) + "]  is greater than " + cNumericFieldNames( 4 ) + " [" + RoundSigDigits( MaxNumberOfWarmupDays ) + "], " + RoundSigDigits( MinNumberOfWarmupDays ) + " will be used." );
				MaxNumberOfWarmupDays = MinNumberOfWarmupDays;
			}
			if ( MinNumberOfWarmupDays < 6 ) {
				ShowWarningError( RoutineName + CurrentModuleObject + ": " + cNumericFieldNames( 5 ) + " potentially invalid. Experience has shown that most files will converge within " + RoundSigDigits( DefaultMaxNumberOfWarmupDays ) + " warmup days. " );
				ShowContinueError( "...Choosing less than " + RoundSigDigits( DefaultMinNumberOfWarmupDays ) + " warmup days may have adverse effects on the simulation results, particularly design day simulations. " );
				ShowContinueError( "...Users should only alter this default if they are certain that less than " + RoundSigDigits( DefaultMinNumberOfWarmupDays ) + " warmup days is appropriate for a particular file. " );
				ShowContinueError( "...Verify that convergence to desired results are achieved. You can report values during warmup days to ascertain convergence." );
			}
		} else {
			ShowSevereError( RoutineName + " A " + CurrentModuleObject + " Object must be entered." );
			ErrorsFound = true;
			BuildingName = "NOT ENTERED";
			AlphaName( 2 ) = "NOT ENTERED";
			AlphaName( 3 ) = "NOT ENTERED";
			MaxNumberOfWarmupDays = DefaultMaxNumberOfWarmupDays;
			MinNumberOfWarmupDays = DefaultMinNumberOfWarmupDays;
		}

		// Write Building Information to the initialization output file
		gio::write( OutputFileInits, Format_721 );

		gio::write( OutputFileInits, Format_720 ) << BuildingName << RoundSigDigits( BuildingAzimuth, 3 ) << AlphaName( 2 ) << RoundSigDigits( LoadsConvergTol, 5 ) << RoundSigDigits( TempConvergTol, 5 ) << AlphaName( 3 ) << RoundSigDigits( MaxNumberOfWarmupDays ) << RoundSigDigits( MinNumberOfWarmupDays );
		// Above should be validated...

		CurrentModuleObject = "SurfaceConvectionAlgorithm:Inside";
		NumObjects = GetNumObjectsFound( CurrentModuleObject );
		if ( NumObjects > 0 ) {
			GetObjectItem( CurrentModuleObject, 1, AlphaName, NumAlpha, BuildingNumbers, NumNumber, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			{ auto const SELECT_CASE_var( AlphaName( 1 ) );

			if ( SELECT_CASE_var == "SIMPLE" ) {
				DefaultInsideConvectionAlgo = ASHRAESimple;
				AlphaName( 1 ) = "Simple";

			} else if ( ( SELECT_CASE_var == "TARP" ) || ( SELECT_CASE_var == "DETAILED" ) ) {
				DefaultInsideConvectionAlgo = ASHRAETARP;
				if ( AlphaName( 1 ) == "DETAILED" ) {
					ShowSevereError( "GetInsideConvectionAlgorithm: Deprecated value for " + CurrentModuleObject + ", defaulting to TARP, entered value=" + AlphaName( 1 ) );
				}
				AlphaName( 1 ) = "TARP";

			} else if ( SELECT_CASE_var == "CEILINGDIFFUSER" ) {
				DefaultInsideConvectionAlgo = CeilingDiffuser;
				AlphaName( 1 ) = "CeilingDiffuser";

			} else if ( SELECT_CASE_var == "TROMBEWALL" ) {
				DefaultInsideConvectionAlgo = TrombeWall;
				ShowSevereError( "GetInsideConvectionAlgorithm: TrombeWall has been used as a global definition. This is a zone oriented value.  Will be illegal in the future." );
				AlphaName( 1 ) = "TrombeWall";

			} else if ( SELECT_CASE_var == "ADAPTIVECONVECTIONALGORITHM" ) {
				DefaultInsideConvectionAlgo = AdaptiveConvectionAlgorithm;
				AlphaName( 1 ) = "AdaptiveConvectionAlgorithm";

			} else {
				ShowWarningError( "GetInsideConvectionAlgorithm: Invalid value for " + CurrentModuleObject + ", defaulting to TARP, invalid value=" + AlphaName( 1 ) );
				DefaultInsideConvectionAlgo = ASHRAETARP;
				AlphaName( 1 ) = "TARP";

			}}
		} else {
			// default value, if not specified
			DefaultInsideConvectionAlgo = ASHRAETARP;
			AlphaName( 1 ) = "TARP";

		}
		gio::write( OutputFileInits, Format_722 ) << AlphaName( 1 );

		//Get only the first (if more were input)
		CurrentModuleObject = "SurfaceConvectionAlgorithm:Outside";
		NumObjects = GetNumObjectsFound( CurrentModuleObject );
		if ( NumObjects > 0 ) {
			GetObjectItem( "SurfaceConvectionAlgorithm:Outside", 1, AlphaName, NumAlpha, BuildingNumbers, NumNumber, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			{ auto const SELECT_CASE_var( AlphaName( 1 ) );

			if ( ( SELECT_CASE_var == "SIMPLECOMBINED" ) || ( SELECT_CASE_var == "SIMPLE" ) ) {
				DefaultOutsideConvectionAlgo = ASHRAESimple;
				if ( AlphaName( 1 ) == "SIMPLE" ) {
					ShowSevereError( "GetOutsideConvectionAlgorithm: Deprecated value for " + CurrentModuleObject + ", defaulting to SimpleCombined, entered value=" + AlphaName( 1 ) );
				}
				AlphaName( 1 ) = "SimpleCombined";

			} else if ( ( SELECT_CASE_var == "TARP" ) || ( SELECT_CASE_var == "DETAILED" ) || ( SELECT_CASE_var == "BLAST" ) ) {
				DefaultOutsideConvectionAlgo = ASHRAETARP;
				if ( AlphaName( 1 ) == "DETAILED" ) {
					ShowSevereError( "GetOutsideConvectionAlgorithm: Deprecated value for " + CurrentModuleObject + ", defaulting to TARP, entered value=" + AlphaName( 1 ) );
				}
				if ( AlphaName( 1 ) == "BLAST" ) {
					ShowSevereError( "GetOutsideConvectionAlgorithm: Deprecated value for " + CurrentModuleObject + ", defaulting to TARP, entered value=" + AlphaName( 1 ) );
				}
				AlphaName( 1 ) = "TARP";

			} else if ( SELECT_CASE_var == "MOWITT" ) {
				DefaultOutsideConvectionAlgo = MoWiTTHcOutside;
				AlphaName( 1 ) = "MoWitt";

			} else if ( ( SELECT_CASE_var == "DOE-2" ) || ( SELECT_CASE_var == "DOE2" ) ) {
				DefaultOutsideConvectionAlgo = DOE2HcOutside;
				if ( AlphaName( 1 ) == "DOE2" ) {
					ShowSevereError( "GetOutsideConvectionAlgorithm: Deprecated value for " + CurrentModuleObject + ", defaulting to DOE-2, entered value=" + AlphaName( 1 ) );
				}
				AlphaName( 1 ) = "DOE-2";

			} else if ( SELECT_CASE_var == "ADAPTIVECONVECTIONALGORITHM" ) {
				DefaultOutsideConvectionAlgo = AdaptiveConvectionAlgorithm;
				AlphaName( 1 ) = "AdaptiveConvectionAlgorithm";

			} else {
				ShowWarningError( "GetOutsideConvectionAlgorithm: Invalid value for " + CurrentModuleObject + ", defaulting to DOE-2, invalid value=" + AlphaName( 1 ) );
				DefaultOutsideConvectionAlgo = DOE2HcOutside;
				AlphaName( 1 ) = "DOE-2";

			}}
		} else {
			// default value, if not specified
			DefaultOutsideConvectionAlgo = DOE2HcOutside;
			AlphaName( 1 ) = "DOE-2";

		}

		gio::write( OutputFileInits, Format_723 ) << AlphaName( 1 );

		CurrentModuleObject = "HeatBalanceAlgorithm";
		NumObjects = GetNumObjectsFound( CurrentModuleObject );
		if ( NumObjects > 0 ) {
			GetObjectItem( CurrentModuleObject, 1, AlphaName, NumAlpha, BuildingNumbers, NumNumber, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			{ auto const SELECT_CASE_var( AlphaName( 1 ) );
			//The default is CTF = 0.  Then the moisture solution is EMPD =2
			if ( ( SELECT_CASE_var == "CONDUCTIONTRANSFERFUNCTION" ) || ( SELECT_CASE_var == "DEFAULT" ) || ( SELECT_CASE_var == "CTF" ) ) {
				OverallHeatTransferSolutionAlgo = UseCTF;
				AlphaName( 1 ) = "CTF - Conduction Transfer Function";

			} else if ( ( SELECT_CASE_var == "MOISTUREPENETRATIONDEPTHCONDUCTIONTRANSFERFUNCTION" ) || ( SELECT_CASE_var == "EMPD" ) ) {
				OverallHeatTransferSolutionAlgo = UseEMPD;
				AlphaName( 1 ) = "EMPD - Effective Moisture Penetration Depth";

			} else if ( ( SELECT_CASE_var == "CONDUCTIONFINITEDIFFERENCE" ) || ( SELECT_CASE_var == "CONDFD" ) || ( SELECT_CASE_var == "CONDUCTIONFINITEDIFFERENCEDETAILED" ) ) {
				OverallHeatTransferSolutionAlgo = UseCondFD;
				AlphaName( 1 ) = "CONDFD - Conduction Finite Difference";
				if ( NumOfTimeStepInHour < 20 ) {
					ShowSevereError( "GetSolutionAlgorithm: " + CurrentModuleObject + ' ' + cAlphaFieldNames( 1 ) + " is Conduction Finite Difference but Number of TimeSteps in Hour < 20, Value is " + RoundSigDigits( NumOfTimeStepInHour ) + '.' );
					ShowContinueError( "...Suggested minimum number of time steps in hour for Conduction Finite Difference solutions is 20. Errors or inaccurate calculations may occur." );
				}

			} else if ( ( SELECT_CASE_var == "COMBINEDHEATANDMOISTUREFINITEELEMENT" ) || ( SELECT_CASE_var == "HAMT" ) ) {
				OverallHeatTransferSolutionAlgo = UseHAMT;
				AlphaName( 1 ) = "HAMT - Combined Heat and Moisture Transfer Finite Element";
				if ( NumOfTimeStepInHour < 20 ) {
					ShowSevereError( "GetSolutionAlgorithm: " + CurrentModuleObject + ' ' + cAlphaFieldNames( 1 ) + " is Combined Heat and Moisture Finite Element but Number of TimeSteps in Hour < 20, Value is " + RoundSigDigits( NumOfTimeStepInHour ) + '.' );
					ShowContinueError( "...Suggested minimum number of time steps in hour for Combined Heat and Moisture Finite Element solutions is 20. Errors or inaccurate calculations may occur." );
					ShowContinueError( "...If the simulation crashes, look at material properties (esp porosity), use timestep=60, or less layers in your constructions." );
				}

			} else {
				OverallHeatTransferSolutionAlgo = UseCTF;
				AlphaName( 1 ) = "CTF - Conduction Transfer Function";
			}}

			if ( NumNumber > 0 ) {
				MaxSurfaceTempLimit = BuildingNumbers( 1 );
				MaxSurfaceTempLimitBeforeFatal = MaxSurfaceTempLimit * 2.5;
				if ( MaxSurfaceTempLimit < MinSurfaceTempLimit ) {
				} else if ( MaxSurfaceTempLimit < 0.0 ) {
					MaxSurfaceTempLimit = DefaultSurfaceTempLimit;
					MaxSurfaceTempLimitBeforeFatal = MaxSurfaceTempLimit * 2.5;
				}
			}

			if ( ! lNumericFieldBlanks( 2 ) ) {
				LowHConvLimit = BuildingNumbers( 2 );
			}
			if ( ! lNumericFieldBlanks( 3 ) ) {
				HighHConvLimit = BuildingNumbers( 3 );
			}

		} else {
			OverallHeatTransferSolutionAlgo = UseCTF;
			AlphaName( 1 ) = "ConductionTransferFunction";
			MaxSurfaceTempLimit = DefaultSurfaceTempLimit;
			MaxSurfaceTempLimitBeforeFatal = MaxSurfaceTempLimit * 2.5;
		}

		HeatTransferAlgosUsed.allocate( 1 );
		HeatTransferAlgosUsed( 1 ) = OverallHeatTransferSolutionAlgo;

		// algorithm input checks now deferred until surface properties are read in,
		//  moved to SurfaceGeometry.cc routine GetSurfaceHeatTransferAlgorithmOverrides

		gio::write( OutputFileInits, Format_724 );

		CurrentModuleObject = "Compliance:Building";
		NumObjects = GetNumObjectsFound( CurrentModuleObject );

		if ( NumObjects > 0 ) {
			GetObjectItem( CurrentModuleObject, 1, AlphaName, NumAlpha, BuildingNumbers, NumNumber, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			// Building Rotation for Appendix G
			BuildingRotationAppendixG = mod( BuildingNumbers( 1 ), 360.0 );
		}

		// A new object is added by L. Gu, 12/09
		CurrentModuleObject = "ZoneAirHeatBalanceAlgorithm";
		NumObjects = GetNumObjectsFound( CurrentModuleObject );
		if ( NumObjects > 0 ) {
			GetObjectItem( CurrentModuleObject, 1, AlphaName, NumAlpha, BuildingNumbers, NumNumber, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			if ( NumAlpha > 0 ) {
				{ auto const SELECT_CASE_var( AlphaName( 1 ) );
				if ( ( SELECT_CASE_var == "3RDORDERBACKWARDDIFFERENCE" ) || ( SELECT_CASE_var == "THIRDORDERBACKWARDDIFFERENCE" ) ) {
					ZoneAirSolutionAlgo = Use3rdOrder;
					AlphaName( 1 ) = "ThirdOrderBackwardDifference";
				} else if ( SELECT_CASE_var == "ANALYTICALSOLUTION" ) {
					ZoneAirSolutionAlgo = UseAnalyticalSolution;
					AlphaName( 1 ) = "AnalyticalSolution";
				} else if ( SELECT_CASE_var == "EULERMETHOD" ) {
					ZoneAirSolutionAlgo = UseEulerMethod;
					AlphaName( 1 ) = "EulerMethod";
				} else {
					ZoneAirSolutionAlgo = Use3rdOrder;
					AlphaName( 1 ) = "ThirdOrderBackwardDifference";
					ShowWarningError( CurrentModuleObject + ": Invalid input of " + cAlphaFieldNames( 1 ) + ". The default choice is assigned = " + AlphaName( 1 ) );
					ShowContinueError( "Valid choices are: ThirdOrderBackwardDifference, AnalyticalSolution, or EulerMethod." );
				}}
			}
		} else {
			ZoneAirSolutionAlgo = Use3rdOrder;
			AlphaName( 1 ) = "ThirdOrderBackwardDifference";
		}

		// Write Solution Algorithm to the initialization output file for User Verification
		gio::write( OutputFileInits, Format_726 );
		gio::write( OutputFileInits, Format_727 ) << AlphaName( 1 );

		// A new object is added by L. Gu, 06/10
		CurrentModuleObject = "ZoneAirContaminantBalance";
		NumObjects = GetNumObjectsFound( CurrentModuleObject );
		if ( NumObjects > 0 ) {
			GetObjectItem( CurrentModuleObject, 1, AlphaName, NumAlpha, BuildingNumbers, NumNumber, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			if ( NumAlpha > 0 ) {
				{ auto const SELECT_CASE_var( AlphaName( 1 ) );
				if ( SELECT_CASE_var == "YES" ) {
					Contaminant.CO2Simulation = true;
					Contaminant.SimulateContaminants = true;
				} else if ( SELECT_CASE_var == "NO" ) {
					Contaminant.CO2Simulation = false;
				} else {
					Contaminant.CO2Simulation = false;
					AlphaName( 1 ) = "NO";
					ShowWarningError( CurrentModuleObject + ": Invalid input of " + cAlphaFieldNames( 1 ) + ". The default choice is assigned = NO" );
				}}
			}
			if ( NumAlpha == 1 && Contaminant.CO2Simulation ) {
				if ( Contaminant.CO2Simulation ) {
					ShowSevereError( CurrentModuleObject + ", " + cAlphaFieldNames( 2 ) + " is required and not given." );
					ErrorsFound = true;
				}
			} else if ( NumAlpha > 1 && Contaminant.CO2Simulation ) {
				Contaminant.CO2OutdoorSchedPtr = GetScheduleIndex( AlphaName( 2 ) );
				if ( Contaminant.CO2OutdoorSchedPtr == 0 ) {
					ShowSevereError( CurrentModuleObject + ", " + cAlphaFieldNames( 2 ) + " not found: " + AlphaName( 2 ) );
					ErrorsFound = true;
				}
			}
			if ( NumAlpha > 2 ) {
				{ auto const SELECT_CASE_var( AlphaName( 3 ) );
				if ( SELECT_CASE_var == "YES" ) {
					Contaminant.GenericContamSimulation = true;
					if ( ! Contaminant.CO2Simulation ) Contaminant.SimulateContaminants = true;
				} else if ( SELECT_CASE_var == "NO" ) {
					Contaminant.GenericContamSimulation = false;
				} else {
					Contaminant.GenericContamSimulation = false;
					AlphaName( 3 ) = "NO";
					ShowWarningError( CurrentModuleObject + ": Invalid input of " + cAlphaFieldNames( 3 ) + ". The default choice is assigned = NO" );
				}}
				if ( NumAlpha == 3 && Contaminant.GenericContamSimulation ) {
					if ( Contaminant.GenericContamSimulation ) {
						ShowSevereError( CurrentModuleObject + ", " + cAlphaFieldNames( 4 ) + " is required and not given." );
						ErrorsFound = true;
					}
				} else if ( NumAlpha > 3 && Contaminant.GenericContamSimulation ) {
					Contaminant.GenericContamOutdoorSchedPtr = GetScheduleIndex( AlphaName( 4 ) );
					if ( Contaminant.GenericContamOutdoorSchedPtr == 0 ) {
						ShowSevereError( CurrentModuleObject + ", " + cAlphaFieldNames( 4 ) + " not found: " + AlphaName( 4 ) );
						ErrorsFound = true;
					}
				}
			}
		} else {
			Contaminant.SimulateContaminants = false;
			Contaminant.CO2Simulation = false;
			Contaminant.GenericContamSimulation = false;
			AlphaName( 1 ) = "NO";
			AlphaName( 3 ) = "NO";
		}

		gio::write( OutputFileInits, Format_728 );
		if ( Contaminant.SimulateContaminants && Contaminant.CO2Simulation ) {
			gio::write( OutputFileInits, Format_730 ) << "Yes" << AlphaName( 1 );
		} else {
			gio::write( OutputFileInits, Format_730 ) << "No" << "N/A";
		}

		gio::write( OutputFileInits, Format_729 );
		if ( Contaminant.SimulateContaminants && Contaminant.GenericContamSimulation ) {
			gio::write( OutputFileInits, Format_731 ) << "Yes" << AlphaName( 3 );
		} else {
			gio::write( OutputFileInits, Format_731 ) << "No" << "N/A";
		}

		// A new object is added by B. Nigusse, 02/14
		CurrentModuleObject = "ZoneAirMassFlowConservation";
		NumObjects = GetNumObjectsFound(CurrentModuleObject);
		ZoneAirMassFlow.EnforceZoneMassBalance = false;

		if ( NumObjects > 0 ) {
			GetObjectItem( CurrentModuleObject, 1, AlphaName, NumAlpha, BuildingNumbers, NumNumber, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			if ( NumAlpha > 0 ) {
				{ auto const SELECT_CASE_var( AlphaName( 1 ) );
				if ( SELECT_CASE_var == "YES" ) {
					ZoneAirMassFlow.BalanceMixing = true;
					ZoneAirMassFlow.EnforceZoneMassBalance = true;
					AlphaName( 1 ) = "Yes";
				} else if ( SELECT_CASE_var == "NO" ) {
					ZoneAirMassFlow.BalanceMixing = false;
					AlphaName( 1 ) = "No";
				} else {
					ZoneAirMassFlow.BalanceMixing = false;
					AlphaName(1) = "No";
					ShowWarningError( CurrentModuleObject + ": Invalid input of " + cAlphaFieldNames(1) + ". The default choice is assigned = No" );
				}}
			}
			if ( NumAlpha > 1 ) {
				{ auto const SELECT_CASE_var( AlphaName( 2 ) );
				if ( SELECT_CASE_var == "ADDINFILTRATIONFLOW" ) {
					ZoneAirMassFlow.InfiltrationTreatment = AddInfiltrationFlow;
					ZoneAirMassFlow.EnforceZoneMassBalance = true;
					AlphaName( 2 ) = "AddInfiltrationFlow";
					if ( !Contaminant.CO2Simulation ) Contaminant.SimulateContaminants = true;
				} else if ( SELECT_CASE_var == "ADJUSTINFILTRATIONFLOW" ) {
					ZoneAirMassFlow.InfiltrationTreatment = AdjustInfiltrationFlow;
					ZoneAirMassFlow.EnforceZoneMassBalance = true;
					AlphaName( 2 ) = "AddInfiltrationFlow";
					if ( !Contaminant.CO2Simulation ) Contaminant.SimulateContaminants = true;
				} else if ( SELECT_CASE_var == "NONE" ) {
					ZoneAirMassFlow.InfiltrationTreatment = NoInfiltrationFlow;
					AlphaName( 2 ) = "None";
				} else {
					ZoneAirMassFlow.InfiltrationTreatment = AddInfiltrationFlow;
					ZoneAirMassFlow.EnforceZoneMassBalance = true;
					AlphaName( 2 ) = "AddInfiltrationFlow";
					ShowWarningError( CurrentModuleObject + ": Invalid input of " + cAlphaFieldNames(2) + ". The default choice is assigned = AddInfiltrationFlow" );
				}}
			} else {
				ZoneAirMassFlow.InfiltrationTreatment = AddInfiltrationFlow;
				ZoneAirMassFlow.EnforceZoneMassBalance = true;
				AlphaName( 2 ) = "AddInfiltrationFlow";
			}
			if ( ZoneAirMassFlow.InfiltrationTreatment == NoInfiltrationFlow ) {
				AlphaName( 3 ) = "N/A";
			} else {
				if ( NumAlpha > 2 ) {
						{ auto const SELECT_CASE_var( AlphaName( 3 ) );
						if ( SELECT_CASE_var == "MIXINGSOURCEZONESONLY" ) {
							ZoneAirMassFlow.InfiltrationZoneType = MixingSourceZonesOnly;
							AlphaName( 3 ) = "MixingSourceZonesOnly";
						} else if ( SELECT_CASE_var == "ALLZONES" ) {
							ZoneAirMassFlow.InfiltrationZoneType = AllZones;
							AlphaName( 3 ) = "AllZones";
						} else {
							ZoneAirMassFlow.InfiltrationZoneType = MixingSourceZonesOnly;
							AlphaName( 3 ) = "MixingSourceZonesOnly";
							ShowWarningError( CurrentModuleObject + ": Invalid input of " + cAlphaFieldNames( 3 ) + ". The default choice is assigned = MixingSourceZonesOnly" );
						}}
				} else {
					ZoneAirMassFlow.InfiltrationZoneType = MixingSourceZonesOnly;
					AlphaName( 3 ) = "MixingSourceZonesOnly";
				}
			}
		} else {
			ZoneAirMassFlow.EnforceZoneMassBalance = false;
		}

		gio::write( OutputFileInits, Format_732 );
		if ( ZoneAirMassFlow.EnforceZoneMassBalance ) {
			gio::write( OutputFileInits, Format_733 ) << "Yes" << AlphaName( 1 ) << AlphaName( 2 ) << AlphaName( 3 );
		} else {
			gio::write( OutputFileInits, Format_733 ) << "No" << "N/A" << "N/A" << "N/A";
		}

	}

	void
	GetSiteAtmosphereData( bool & ErrorsFound )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Graham Ellis
		//       DATE WRITTEN   January 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Reads the input data for the SITE ATMOSPHERIC VARIATION object.

		// METHODOLOGY EMPLOYED:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumObjects;
		int NumAlphas; // Number of elements in the alpha array
		int NumNums; // Number of elements in the numeric array
		int IOStat; // IO Status when calling get input subroutine
		Array1D_string AlphArray( 1 ); // Character string data
		Array1D< Real64 > NumArray( 3 ); // Numeric data

		// Formats
		static gio::Fmt Format_720( "('Environment:Site Atmospheric Variation',3(',',A))" );

		// FLOW:
		CurrentModuleObject = "Site:HeightVariation";
		NumObjects = GetNumObjectsFound( CurrentModuleObject );

		if ( NumObjects == 1 ) {
			GetObjectItem( CurrentModuleObject, 1, AlphArray, NumAlphas, NumArray, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			if ( NumNums > 0 ) SiteWindExp = NumArray( 1 );
			if ( NumNums > 1 ) SiteWindBLHeight = NumArray( 2 );
			if ( NumNums > 2 ) SiteTempGradient = NumArray( 3 );

		} else if ( NumObjects > 1 ) {
			ShowSevereError( "Too many " + CurrentModuleObject + " objects, only 1 allowed." );
			ErrorsFound = true;
		} else { //  None entered
			// IDD defaults would have this:
			// Building object defaults use Terrain to set SiteWindExp and SiteWindBLHeight but would
			// be overridden by a Site Atmospheric Variation Object.
			//SiteWindExp = 0.22
			//SiteWindBLHeight = 370.0
			SiteTempGradient = 0.0065;
		}

		// Write to the initialization output file
		gio::write( OutputFileInits, fmtA ) << "! <Environment:Site Atmospheric Variation>,Wind Speed Profile Exponent {},Wind Speed Profile Boundary Layer Thickness {m},Air Temperature Gradient Coefficient {K/m}";

		gio::write( OutputFileInits, Format_720 ) << RoundSigDigits( SiteWindExp, 3 ) << RoundSigDigits( SiteWindBLHeight, 3 ) << RoundSigDigits( SiteTempGradient, 6 );

	}

	void
	GetMaterialData( bool & ErrorsFound ) // set to true if errors found in input
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Liesen
		//       DATE WRITTEN   September 1997
		//       MODIFIED       April 1999; L.Lawrie
		//                      Sept 1999, FCW, Window5 modifications
		//                      Mar 2001, FCW, WindowShade mods
		//                      Sep 2001, FCW, add Material:WindowGasMixture
		//                      Oct 2001, FCW, add Material:WindowBlind
		//                      Dec 2003, FCW, add glass solar/visible transmittance dirt factor
		//                      Feb 2009, TH, added WindowMaterial:GlazingGroup:Thermochromic

		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// The purpose of this subroutine is to serve as a transfer agent
		// between the input file and the material derived type.  The new input
		// file is working, and this file reads the material data directly
		// from the input file and transfer that information to the new data
		// structure.  Data read in this routine is stored in a
		// derived type (Material) defined in the DataHeatBalance module.

		// In April 1999, a new set of material definitions replaced the one "all-purpose"
		// material definition.  There are now 10 flavors of materials.  Definitions from
		// the IDD appear below before their counterpart "gets".

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::RoundSigDigits;
		using General::ScanForReports;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int IOStat; // IO Status when calling get input subroutine
		Array1D_string MaterialNames( 5 ); // Number of Material Alpha names defined
		int MaterNum; // Counter to keep track of the material number
		int MaterialNumAlpha; // Number of material alpha names being passed
		int MaterialNumProp; // Number of material properties being passed
		Array1D< Real64 > MaterialProps( 27 ); // Temporary array to transfer material properties
		int RegMat; // Regular Materials -- full property definition
		int RegRMat; // Regular Materials -- R only property definition
		int AirMat; // Air space materias in opaque constructions
		int IRTMat; // Infrared Transmitting Materials -- R only property definition

		int EcoRoofMat; // Materials for ecoRoof
		int NumGas; // Index for loop over gap gases in a mixture
		int NumGases; // Number of gasses in a mixture
		int GasType; // Gas type index: 1=air, 2=argon, 3=krypton, 4=xenon
		int Loop;
		int ICoeff; // Gas property coefficient index
		bool ErrorInName;
		bool IsBlank;
		std::string TypeOfGas; // Type of window gas fill (Air, Argon, Krypton, &
		// Xenon, or Custom
		Real64 MinSlatAngGeom; // Minimum and maximum slat angle allowed by slat geometry (deg)
		Real64 MaxSlatAngGeom;
		Real64 ReflectivitySol; // Glass reflectivity, solar
		Real64 ReflectivityVis; // Glass reflectivity, visible
		Real64 TransmittivitySol; // Glass transmittivity, solar
		Real64 TransmittivityVis; // Glass transmittivity, visible
		static bool DoReport( false );
		Real64 DenomRGas; // Denominator for WindowGas calculations of NominalR
		Real64 Openness; // insect screen oppenness fraction = (1-d/s)^2

		// Added TH 1/9/2009 to read the thermochromic glazings
		static int iTC( 0 );
		static int iMat( 0 );

		// Added TH 7/27/2009 for constructions defined with F or C factro method
		int TotFfactorConstructs; // Number of slabs-on-grade or underground floor constructions defined with F factors
		int TotCfactorConstructs; // Number of underground wall constructions defined with C factors

		// Formats
		static gio::Fmt Format_701( "(' Material Details',10(',',A))" );
		static gio::Fmt Format_702( "(' Material:Air',2(',',A))" );

		// FLOW:

		RegMat = GetNumObjectsFound( "Material" );
		RegRMat = GetNumObjectsFound( "Material:NoMass" );
		IRTMat = GetNumObjectsFound( "Material:InfraredTransparent" );
		AirMat = GetNumObjectsFound( "Material:AirGap" );
		W5GlsMat = GetNumObjectsFound( "WindowMaterial:Glazing" );
		W5GlsMatAlt = GetNumObjectsFound( "WindowMaterial:Glazing:RefractionExtinctionMethod" );
		W5GasMat = GetNumObjectsFound( "WindowMaterial:Gas" );
		W5GasMatMixture = GetNumObjectsFound( "WindowMaterial:GasMixture" );
		TotShades = GetNumObjectsFound( "WindowMaterial:Shade" );
		TotComplexShades = GetNumObjectsFound( "WindowMaterial:ComplexShade" );
		TotComplexGaps = GetNumObjectsFound( "WindowMaterial:Gap" );
		TotScreens = GetNumObjectsFound( "WindowMaterial:Screen" );
		TotBlinds = GetNumObjectsFound( "WindowMaterial:Blind" );
		EcoRoofMat = GetNumObjectsFound( "Material:RoofVegetation" );
		TotSimpleWindow = GetNumObjectsFound( "WindowMaterial:SimpleGlazingSystem" );

		W5GlsMatEQL = GetNumObjectsFound( "WindowMaterial:Glazing:EquivalentLayer" );
		TotShadesEQL = GetNumObjectsFound( "WindowMaterial:Shade:EquivalentLayer" );
		TotDrapesEQL = GetNumObjectsFound( "WindowMaterial:Drape:EquivalentLayer" );
		TotBlindsEQL = GetNumObjectsFound( "WindowMaterial:Blind:EquivalentLayer" );
		TotScreensEQL = GetNumObjectsFound( "WindowMaterial:Screen:EquivalentLayer" );
		W5GapMatEQL = GetNumObjectsFound( "WindowMaterial:Gap:EquivalentLayer" );

		TotMaterials = RegMat + RegRMat + AirMat + W5GlsMat + W5GlsMatAlt + W5GasMat + W5GasMatMixture + TotShades + TotScreens + TotBlinds + EcoRoofMat + IRTMat + TotSimpleWindow + TotComplexShades + TotComplexGaps + W5GlsMatEQL + TotShadesEQL + TotDrapesEQL + TotBlindsEQL + TotScreensEQL + W5GapMatEQL;

		TotFfactorConstructs = GetNumObjectsFound( "Construction:FfactorGroundFloor" );
		TotCfactorConstructs = GetNumObjectsFound( "Construction:CfactorUndergroundWall" );
		if ( TotFfactorConstructs + TotCfactorConstructs >= 1 ) {
			// Add a new fictitious insulation layer and a thermal mass layer for each F or C factor defined construction
			TotMaterials += 1 + TotFfactorConstructs + TotCfactorConstructs;
		}

		Material.allocate( TotMaterials ); // Allocate the array Size to the number of materials

		NominalR.dimension( TotMaterials, 0.0 );

		MaterNum = 0;

		// Regular Materials

		CurrentModuleObject = "Material";
		for ( Loop = 1; Loop <= RegMat; ++Loop ) {

			//Call Input Get routine to retrieve material data
			GetObjectItem( CurrentModuleObject, Loop, MaterialNames, MaterialNumAlpha, MaterialProps, MaterialNumProp, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			ErrorInName = false;
			IsBlank = false;
			VerifyName( MaterialNames( 1 ), Material, MaterNum, ErrorInName, IsBlank, CurrentModuleObject + " Name" );
			if ( ErrorInName ) {
				ShowContinueError( "...All Material names must be unique regardless of subtype." );
				ErrorsFound = true;
				continue;
			}

			//Load the material derived type from the input data.
			++MaterNum;
			Material( MaterNum ).Group = RegularMaterial;
			Material( MaterNum ).Name = MaterialNames( 1 );

			ValidateMaterialRoughness( MaterNum, MaterialNames( 2 ), ErrorsFound );

			Material( MaterNum ).Thickness = MaterialProps( 1 );
			Material( MaterNum ).Conductivity = MaterialProps( 2 );
			Material( MaterNum ).Density = MaterialProps( 3 );
			Material( MaterNum ).SpecHeat = MaterialProps( 4 );
			// min fields is 6 -- previous four will be there
			if ( MaterialNumProp >= 5 ) {
				Material( MaterNum ).AbsorpThermal = MaterialProps( 5 );
				Material( MaterNum ).AbsorpThermalInput = MaterialProps( 5 );
			} else {
				Material( MaterNum ).AbsorpThermal = 0.9;
				Material( MaterNum ).AbsorpThermalInput = 0.9;
			}
			if ( MaterialNumProp >= 6 ) {
				Material( MaterNum ).AbsorpSolar = MaterialProps( 6 );
				Material( MaterNum ).AbsorpSolarInput = MaterialProps( 6 );
			} else {
				Material( MaterNum ).AbsorpSolar = 0.7;
				Material( MaterNum ).AbsorpSolarInput = 0.7;
			}
			if ( MaterialNumProp >= 7 ) {
				Material( MaterNum ).AbsorpVisible = MaterialProps( 7 );
				Material( MaterNum ).AbsorpVisibleInput = MaterialProps( 7 );
			} else {
				Material( MaterNum ).AbsorpVisible = 0.7;
				Material( MaterNum ).AbsorpVisibleInput = 0.7;
			}

			if ( Material( MaterNum ).Conductivity > 0.0 ) {
				NominalR( MaterNum ) = Material( MaterNum ).Thickness / Material( MaterNum ).Conductivity;
				Material( MaterNum ).Resistance = NominalR( MaterNum );
			} else {
				ShowSevereError( "Positive thermal conductivity required for material " + Material( MaterNum ).Name );
				ErrorsFound = true;
			}

		}

		// Add the 6" heavy concrete for constructions defined with F or C factor method
		if ( TotFfactorConstructs + TotCfactorConstructs >= 1 ) {
			++MaterNum;

			Material( MaterNum ).Group = RegularMaterial;
			Material( MaterNum ).Name = "~FC_Concrete";
			Material( MaterNum ).Thickness = 0.15; // m, 0.15m = 6 inches
			Material( MaterNum ).Conductivity = 1.95; // W/mK
			Material( MaterNum ).Density = 2240.0; // kg/m3
			Material( MaterNum ).SpecHeat = 900.0; // J/kgK
			Material( MaterNum ).Roughness = MediumRough;
			Material( MaterNum ).AbsorpSolar = 0.7;
			Material( MaterNum ).AbsorpThermal = 0.9;
			Material( MaterNum ).AbsorpVisible = 0.7;
			NominalR( MaterNum ) = Material( MaterNum ).Thickness / Material( MaterNum ).Conductivity;
			Material( MaterNum ).Resistance = NominalR( MaterNum );

			++RegMat;
		}

		CurrentModuleObject = "Material:NoMass";
		for ( Loop = 1; Loop <= RegRMat; ++Loop ) {

			//Call Input Get routine to retrieve material data
			GetObjectItem( CurrentModuleObject, Loop, MaterialNames, MaterialNumAlpha, MaterialProps, MaterialNumProp, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			ErrorInName = false;
			IsBlank = false;
			VerifyName( MaterialNames( 1 ), Material, MaterNum, ErrorInName, IsBlank, CurrentModuleObject + " Name" );
			if ( ErrorInName ) {
				ShowContinueError( "...All Material names must be unique regardless of subtype." );
				ErrorsFound = true;
				continue;
			}

			//Load the material derived type from the input data.
			++MaterNum;
			Material( MaterNum ).Group = RegularMaterial;
			Material( MaterNum ).Name = MaterialNames( 1 );

			ValidateMaterialRoughness( MaterNum, MaterialNames( 2 ), ErrorsFound );

			Material( MaterNum ).Resistance = MaterialProps( 1 );
			Material( MaterNum ).ROnly = true;
			if ( MaterialNumProp >= 2 ) {
				Material( MaterNum ).AbsorpThermal = MaterialProps( 2 );
				Material( MaterNum ).AbsorpThermalInput = MaterialProps( 2 );
			} else {
				Material( MaterNum ).AbsorpThermal = 0.9;
				Material( MaterNum ).AbsorpThermalInput = 0.9;
			}
			if ( MaterialNumProp >= 3 ) {
				Material( MaterNum ).AbsorpSolar = MaterialProps( 3 );
				Material( MaterNum ).AbsorpSolarInput = MaterialProps( 3 );
			} else {
				Material( MaterNum ).AbsorpSolar = 0.7;
				Material( MaterNum ).AbsorpSolarInput = 0.7;
			}
			if ( MaterialNumProp >= 4 ) {
				Material( MaterNum ).AbsorpVisible = MaterialProps( 4 );
				Material( MaterNum ).AbsorpVisibleInput = MaterialProps( 4 );
			} else {
				Material( MaterNum ).AbsorpVisible = 0.7;
				Material( MaterNum ).AbsorpVisibleInput = 0.7;
			}

			NominalR( MaterNum ) = Material( MaterNum ).Resistance;

		}

		// Add a fictitious insulation layer for each construction defined with F or C factor method
		if ( TotFfactorConstructs + TotCfactorConstructs >= 1 ) {
			for ( Loop = 1; Loop <= TotFfactorConstructs + TotCfactorConstructs; ++Loop ) {
				++MaterNum;
				Material( MaterNum ).Group = RegularMaterial;
				Material( MaterNum ).Name = "~FC_Insulation_" + RoundSigDigits( Loop );
				Material( MaterNum ).ROnly = true;
				Material( MaterNum ).Roughness = MediumRough;
				Material( MaterNum ).AbsorpSolar = 0.0;
				Material( MaterNum ).AbsorpThermal = 0.0;
				Material( MaterNum ).AbsorpVisible = 0.0;
			}
			RegRMat += TotFfactorConstructs + TotCfactorConstructs;
		}

		// Air Materials (for air spaces in opaque constructions)
		CurrentModuleObject = "Material:AirGap";
		for ( Loop = 1; Loop <= AirMat; ++Loop ) {

			//Call Input Get routine to retrieve material data
			GetObjectItem( CurrentModuleObject, Loop, MaterialNames, MaterialNumAlpha, MaterialProps, MaterialNumProp, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			ErrorInName = false;
			IsBlank = false;
			VerifyName( MaterialNames( 1 ), Material, MaterNum, ErrorInName, IsBlank, CurrentModuleObject + " Name" );
			if ( ErrorInName ) {
				ShowContinueError( "...All Material names must be unique regardless of subtype." );
				ErrorsFound = true;
				continue;
			}

			//Load the material derived type from the input data.
			++MaterNum;
			Material( MaterNum ).Group = Air;
			Material( MaterNum ).Name = MaterialNames( 1 );

			Material( MaterNum ).Roughness = MediumRough;

			Material( MaterNum ).Resistance = MaterialProps( 1 );
			Material( MaterNum ).ROnly = true;

			NominalR( MaterNum ) = Material( MaterNum ).Resistance;

		}

		CurrentModuleObject = "Material:InfraredTransparent";
		for ( Loop = 1; Loop <= IRTMat; ++Loop ) {

			//Call Input Get routine to retrieve material data
			GetObjectItem( CurrentModuleObject, Loop, MaterialNames, MaterialNumAlpha, MaterialProps, MaterialNumProp, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			ErrorInName = false;
			IsBlank = false;
			VerifyName( MaterialNames( 1 ), Material, MaterNum, ErrorInName, IsBlank, CurrentModuleObject + " Name" );
			if ( ErrorInName ) {
				ShowContinueError( "...All Material names must be unique regardless of subtype." );
				ErrorsFound = true;
				continue;
			}

			++MaterNum;
			Material( MaterNum ).Group = IRTMaterial;

			//Load the material derived type from the input data.
			Material( MaterNum ).Name = MaterialNames( 1 );

			if ( MaterialNumProp >= 1 ) {
				Material( MaterNum ).Resistance = MaterialProps( 1 );
				Material( MaterNum ).ROnly = true;
			} else {
				Material( MaterNum ).Resistance = 0.01;
			}
			if ( MaterialNumProp >= 2 ) {
				Material( MaterNum ).AbsorpThermal = MaterialProps( 2 );
				Material( MaterNum ).AbsorpThermalInput = MaterialProps( 2 );
			} else {
				Material( MaterNum ).AbsorpThermal = 0.9999;
				Material( MaterNum ).AbsorpThermalInput = 0.9999;
			}
			if ( MaterialNumProp >= 3 ) {
				Material( MaterNum ).AbsorpSolar = MaterialProps( 3 );
				Material( MaterNum ).AbsorpSolarInput = MaterialProps( 3 );
			} else {
				Material( MaterNum ).AbsorpSolar = 1.0;
				Material( MaterNum ).AbsorpSolarInput = 1.0;
			}
			if ( MaterialNumProp >= 4 ) {
				Material( MaterNum ).AbsorpVisible = MaterialProps( 4 );
				Material( MaterNum ).AbsorpVisibleInput = MaterialProps( 4 );
			} else {
				Material( MaterNum ).AbsorpVisible = 1.0;
				Material( MaterNum ).AbsorpVisibleInput = 1.0;
			}

			NominalR( MaterNum ) = Material( MaterNum ).Resistance;

		}

		// Glass materials, regular input: transmittance and front/back reflectance

		CurrentModuleObject = "WindowMaterial:Glazing";
		for ( Loop = 1; Loop <= W5GlsMat; ++Loop ) {

			//Call Input Get routine to retrieve material data
			GetObjectItem( CurrentModuleObject, Loop, MaterialNames, MaterialNumAlpha, MaterialProps, MaterialNumProp, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			ErrorInName = false;
			IsBlank = false;
			VerifyName( MaterialNames( 1 ), Material, MaterNum, ErrorInName, IsBlank, CurrentModuleObject + " Name" );
			if ( ErrorInName ) {
				ShowContinueError( "...All Material names must be unique regardless of subtype." );
				ErrorsFound = true;
				continue;
			}

			++MaterNum;
			Material( MaterNum ).Group = WindowGlass;

			//Load the material derived type from the input data.

			Material( MaterNum ).Name = MaterialNames( 1 );
			Material( MaterNum ).Roughness = VerySmooth;
			Material( MaterNum ).ROnly = true;
			Material( MaterNum ).Thickness = MaterialProps( 1 );
			Material( MaterNum ).Trans = MaterialProps( 2 );
			Material( MaterNum ).ReflectSolBeamFront = MaterialProps( 3 );
			Material( MaterNum ).ReflectSolBeamBack = MaterialProps( 4 );
			Material( MaterNum ).TransVis = MaterialProps( 5 );
			Material( MaterNum ).ReflectVisBeamFront = MaterialProps( 6 );
			Material( MaterNum ).ReflectVisBeamBack = MaterialProps( 7 );
			Material( MaterNum ).TransThermal = MaterialProps( 8 );
			Material( MaterNum ).AbsorpThermalFront = MaterialProps( 9 );
			Material( MaterNum ).AbsorpThermalBack = MaterialProps( 10 );
			Material( MaterNum ).Conductivity = MaterialProps( 11 );
			Material( MaterNum ).GlassTransDirtFactor = MaterialProps( 12 );
			Material( MaterNum ).YoungModulus = MaterialProps( 13 );
			Material( MaterNum ).PoissonsRatio = MaterialProps( 14 );
			if ( MaterialProps( 12 ) == 0.0 ) Material( MaterNum ).GlassTransDirtFactor = 1.0;
			Material( MaterNum ).AbsorpThermal = Material( MaterNum ).AbsorpThermalBack;

			if ( Material( MaterNum ).Conductivity > 0.0 ) {
				NominalR( MaterNum ) = Material( MaterNum ).Thickness / Material( MaterNum ).Conductivity;
				Material( MaterNum ).Resistance = NominalR( MaterNum );
			} else {
				ErrorsFound = true;
				ShowSevereError( "Window glass material " + Material( MaterNum ).Name + " has Conductivity = 0.0, must be >0.0, default = .9" );
			}

			Material( MaterNum ).GlassSpectralDataPtr = 0;
			if ( TotSpectralData > 0 && ! lAlphaFieldBlanks( 3 ) ) {
				Material( MaterNum ).GlassSpectralDataPtr = FindItemInList( MaterialNames( 3 ), SpectralData );
			}
			if ( SameString( MaterialNames( 2 ), "SpectralAverage" ) ) Material( MaterNum ).GlassSpectralDataPtr = 0;
			// No need for spectral data for BSDF either
			if ( SameString( MaterialNames( 2 ), "BSDF" ) ) Material( MaterNum ).GlassSpectralDataPtr = 0;
			if ( Material( MaterNum ).GlassSpectralDataPtr == 0 && SameString( MaterialNames( 2 ), "Spectral" ) ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + Material( MaterNum ).Name + "\" has " + cAlphaFieldNames( 2 ) + " = Spectral but has no matching MaterialProperty:GlazingSpectralData set" );
				if ( lAlphaFieldBlanks( 3 ) ) {
					ShowContinueError( "..." + cAlphaFieldNames( 3 ) + " is blank." );
				} else {
					ShowContinueError( "..." + cAlphaFieldNames( 3 ) + "=\"" + MaterialNames( 3 ) + "\" not found as item in MaterialProperty:GlazingSpectralData objects." );
				}
			}

			if ( ! SameString( MaterialNames( 2 ), "SpectralAverage" ) && ! SameString( MaterialNames( 2 ), "Spectral" ) && ! SameString( MaterialNames( 2 ), "BSDF" ) ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + Material( MaterNum ).Name + "\", invalid specification." );
				ShowContinueError( cAlphaFieldNames( 2 ) + " must be SpectralAverage, Spectral or BSDF, value=" + MaterialNames( 2 ) );
			}

			// TH 8/24/2011, allow glazing properties MaterialProps(2 to 10) to equal 0 or 1: 0.0 =< Prop <= 1.0
			// Fixed CR 8413 - modeling spandrel panels as glazing systems
			if ( SameString( MaterialNames( 2 ), "SpectralAverage" ) ) {

				if ( MaterialProps( 2 ) + MaterialProps( 3 ) > 1.0 ) {
					ErrorsFound = true;
					ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value combination." );
					ShowContinueError( cNumericFieldNames( 2 ) + " + " + cNumericFieldNames( 3 ) + " not <= 1.0" );
				}

				if ( MaterialProps( 2 ) + MaterialProps( 4 ) > 1.0 ) {
					ErrorsFound = true;
					ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value combination." );
					ShowContinueError( cNumericFieldNames( 2 ) + " + " + cNumericFieldNames( 4 ) + " not <= 1.0" );
				}

				if ( MaterialProps( 5 ) + MaterialProps( 6 ) > 1.0 ) {
					ErrorsFound = true;
					ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value combination." );
					ShowContinueError( cNumericFieldNames( 5 ) + " + " + cNumericFieldNames( 6 ) + " not <= 1.0" );
				}

				if ( MaterialProps( 5 ) + MaterialProps( 7 ) > 1.0 ) {
					ErrorsFound = true;
					ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value combination." );
					ShowContinueError( cNumericFieldNames( 5 ) + " + " + cNumericFieldNames( 7 ) + " not <= 1.0" );
				}

				if ( MaterialProps( 8 ) + MaterialProps( 9 ) > 1.0 ) {
					ErrorsFound = true;
					ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value combination." );
					ShowContinueError( cNumericFieldNames( 8 ) + " + " + cNumericFieldNames( 9 ) + " not <= 1.0" );
				}

				if ( MaterialProps( 8 ) + MaterialProps( 10 ) > 1.0 ) {
					ErrorsFound = true;
					ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value combination." );
					ShowContinueError( cNumericFieldNames( 8 ) + " + " + cNumericFieldNames( 10 ) + " not <= 1.0" );
				}

				if ( MaterialProps( 2 ) < 0.0 ) {
					ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value." );
					ShowContinueError( cNumericFieldNames( 2 ) + " not >= 0.0" );
					ErrorsFound = true;
				}

				if ( MaterialProps( 2 ) > 1.0 ) {
					ErrorsFound = true;
					ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value." );
					ShowContinueError( cNumericFieldNames( 2 ) + " not <= 1.0" );
				}

				if ( MaterialProps( 3 ) < 0.0 || MaterialProps( 3 ) > 1.0 ) {
					ErrorsFound = true;
					ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value." );
					ShowContinueError( cNumericFieldNames( 3 ) + " not >= 0.0 and <= 1.0" );
				}

				if ( MaterialProps( 4 ) < 0.0 || MaterialProps( 4 ) > 1.0 ) {
					ErrorsFound = true;
					ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value." );
					ShowContinueError( cNumericFieldNames( 4 ) + " not >= 0.0 and <= 1.0" );
				}

				if ( MaterialProps( 5 ) < 0.0 ) {
					ShowWarningError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", minimal value." );
					ShowWarningError( cNumericFieldNames( 5 ) + " not >= 0.0" );
				}

				if ( MaterialProps( 5 ) > 1.0 ) {
					ErrorsFound = true;
					ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value." );
					ShowContinueError( cNumericFieldNames( 5 ) + " not <= 1.0" );
				}

				if ( MaterialProps( 6 ) < 0.0 || MaterialProps( 6 ) > 1.0 ) {
					ErrorsFound = true;
					ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value." );
					ShowContinueError( cNumericFieldNames( 6 ) + " not >= 0.0 and <= 1.0" );
				}

				if ( MaterialProps( 7 ) < 0.0 || MaterialProps( 7 ) > 1.0 ) {
					ErrorsFound = true;
					ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value." );
					ShowContinueError( cNumericFieldNames( 7 ) + " not >= 0.0 and <= 1.0" );
				}

			}

			if ( MaterialProps( 8 ) > 1.0 ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value." );
				ShowContinueError( cNumericFieldNames( 8 ) + " not <= 1.0" );
			}

			if ( MaterialProps( 9 ) <= 0.0 || MaterialProps( 9 ) >= 1.0 ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value." );
				ShowContinueError( cNumericFieldNames( 9 ) + " not > 0.0 and < 1.0" );
			}

			if ( MaterialProps( 10 ) <= 0.0 || MaterialProps( 10 ) >= 1.0 ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value." );
				ShowContinueError( cNumericFieldNames( 10 ) + " not > 0.0 and < 1.0" );
			}

			if ( MaterialProps( 11 ) <= 0.0 ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value." );
				ShowContinueError( cNumericFieldNames( 11 ) + " not > 0.0" );
			}

			if ( MaterialProps( 13 ) < 0.0 ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value." );
				ShowContinueError( cNumericFieldNames( 13 ) + " not > 0.0" );
			}

			if ( MaterialProps( 14 ) < 0.0 || MaterialProps( 14 ) >= 1.0 ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value." );
				ShowContinueError( cNumericFieldNames( 14 ) + " not > 0.0 and < 1.0" );
			}

			if ( MaterialNames( 4 ) == "" ) {
				Material( MaterNum ).SolarDiffusing = false;
			} else if ( MaterialNames( 4 ) == "YES" ) {
				Material( MaterNum ).SolarDiffusing = true;
			} else if ( MaterialNames( 4 ) == "NO" ) {
				Material( MaterNum ).SolarDiffusing = false;
			} else {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value." );
				ShowContinueError( cNumericFieldNames( 4 ) + " must be Yes or No, entered value=" + MaterialNames( 4 ) );
			}

		}

		// Glass materials, alternative input: index of refraction and extinction coefficient

		CurrentModuleObject = "WindowMaterial:Glazing:RefractionExtinctionMethod";
		for ( Loop = 1; Loop <= W5GlsMatAlt; ++Loop ) {

			//Call Input Get routine to retrieve material data
			GetObjectItem( CurrentModuleObject, Loop, MaterialNames, MaterialNumAlpha, MaterialProps, MaterialNumProp, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			ErrorInName = false;
			IsBlank = false;
			VerifyName( MaterialNames( 1 ), Material, MaterNum, ErrorInName, IsBlank, CurrentModuleObject + " Name" );
			if ( ErrorInName ) {
				ShowContinueError( "...All Material names must be unique regardless of subtype." );
				ErrorsFound = true;
				continue;
			}

			++MaterNum;
			Material( MaterNum ).Group = WindowGlass;

			//Load the material derived type from the input data.

			Material( MaterNum ).Name = MaterialNames( 1 );
			Material( MaterNum ).Roughness = VerySmooth;
			Material( MaterNum ).Thickness = MaterialProps( 1 );
			Material( MaterNum ).ROnly = true;

			// Calculate solar and visible transmittance and reflectance at normal incidence from thickness,
			// index of refraction and extinction coefficient. With the alternative input the front and back
			// properties are assumed to be the same.

			ReflectivitySol = pow_2( ( MaterialProps( 2 ) - 1.0 ) / ( MaterialProps( 2 ) + 1.0 ) );
			ReflectivityVis = pow_2( ( MaterialProps( 4 ) - 1.0 ) / ( MaterialProps( 4 ) + 1.0 ) );
			TransmittivitySol = std::exp( -MaterialProps( 3 ) * MaterialProps( 1 ) );
			TransmittivityVis = std::exp( -MaterialProps( 5 ) * MaterialProps( 1 ) );
			Material( MaterNum ).Trans = TransmittivitySol * pow_2( 1.0 - ReflectivitySol ) / ( 1.0 - pow_2( ReflectivitySol * TransmittivitySol ) );
			Material( MaterNum ).ReflectSolBeamFront = ReflectivitySol * ( 1.0 + pow_2( 1.0 - ReflectivitySol ) * pow_2( TransmittivitySol ) / ( 1.0 - pow_2( ReflectivitySol * TransmittivitySol ) ) );
			Material( MaterNum ).ReflectSolBeamBack = Material( MaterNum ).ReflectSolBeamFront;
			Material( MaterNum ).TransVis = TransmittivityVis * pow_2( 1.0 - ReflectivityVis ) / ( 1.0 - pow_2( ReflectivityVis * TransmittivityVis ) );

			Material( MaterNum ).ReflectVisBeamFront = ReflectivityVis * ( 1.0 + pow_2( 1.0 - ReflectivityVis ) * pow_2( TransmittivityVis ) / ( 1.0 - pow_2( ReflectivityVis * TransmittivityVis ) ) );
			Material( MaterNum ).ReflectVisBeamBack = Material( MaterNum ).ReflectSolBeamFront;
			Material( MaterNum ).TransThermal = MaterialProps( 6 );
			Material( MaterNum ).AbsorpThermalFront = MaterialProps( 7 );
			Material( MaterNum ).AbsorpThermalBack = MaterialProps( 7 );
			Material( MaterNum ).Conductivity = MaterialProps( 8 );
			Material( MaterNum ).GlassTransDirtFactor = MaterialProps( 9 );
			if ( MaterialProps( 9 ) == 0.0 ) Material( MaterNum ).GlassTransDirtFactor = 1.0;
			Material( MaterNum ).AbsorpThermal = Material( MaterNum ).AbsorpThermalBack;

			if ( Material( MaterNum ).Conductivity > 0.0 ) {
				NominalR( MaterNum ) = Material( MaterNum ).Thickness / Material( MaterNum ).Conductivity;
				Material( MaterNum ).Resistance = NominalR( MaterNum );
			}

			Material( MaterNum ).GlassSpectralDataPtr = 0;

			if ( MaterialProps( 6 ) + MaterialProps( 7 ) >= 1.0 ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value combination." );
				ShowContinueError( cNumericFieldNames( 6 ) + " + " + cNumericFieldNames( 7 ) + " not < 1.0" );
			}

			if ( MaterialNames( 2 ) == "" ) {
				Material( MaterNum ).SolarDiffusing = false;
			} else if ( MaterialNames( 2 ) == "YES" ) {
				Material( MaterNum ).SolarDiffusing = true;
			} else if ( MaterialNames( 2 ) == "NO" ) {
				Material( MaterNum ).SolarDiffusing = false;
			} else {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value." );
				ShowContinueError( cNumericFieldNames( 2 ) + " must be Yes or No, entered value=" + MaterialNames( 4 ) );
			}

		}

		// Glass materials, equivalent layer (ASHWAT) method
		CurrentModuleObject = "WindowMaterial:Glazing:EquivalentLayer";
		for ( Loop = 1; Loop <= W5GlsMatEQL; ++Loop ) {

			//Call Input Get routine to retrieve material data
			GetObjectItem( CurrentModuleObject, Loop, MaterialNames, MaterialNumAlpha, MaterialProps, MaterialNumProp, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			ErrorInName = false;
			IsBlank = false;
			VerifyName( MaterialNames( 1 ), Material, MaterNum, ErrorInName, IsBlank, CurrentModuleObject + " Name" );
			if ( ErrorInName ) {
				ShowContinueError( "...All Material names must be unique regardless of subtype." );
				ErrorsFound = true;
				continue;
			}

			++MaterNum;
			Material( MaterNum ).Group = GlassEquivalentLayer;

			//Load the material derived type from the input data.
			Material( MaterNum ).Name = MaterialNames( 1 );
			Material( MaterNum ).Roughness = VerySmooth;
			Material( MaterNum ).ROnly = true;

			Material( MaterNum ).TausFrontBeamBeam = MaterialProps( 1 );
			Material( MaterNum ).TausBackBeamBeam = MaterialProps( 2 );
			Material( MaterNum ).ReflFrontBeamBeam = MaterialProps( 3 );
			Material( MaterNum ).ReflBackBeamBeam = MaterialProps( 4 );
			Material( MaterNum ).TausFrontBeamBeamVis = MaterialProps( 5 );
			Material( MaterNum ).TausBackBeamBeamVis = MaterialProps( 6 );
			Material( MaterNum ).ReflFrontBeamBeamVis = MaterialProps( 7 );
			Material( MaterNum ).ReflBackBeamBeamVis = MaterialProps( 8 );
			Material( MaterNum ).TausFrontBeamDiff = MaterialProps( 9 );
			Material( MaterNum ).TausBackBeamDiff = MaterialProps( 10 );
			Material( MaterNum ).ReflFrontBeamDiff = MaterialProps( 11 );
			Material( MaterNum ).ReflBackBeamDiff = MaterialProps( 12 );
			Material( MaterNum ).TausFrontBeamDiffVis = MaterialProps( 13 );
			Material( MaterNum ).TausBackBeamDiffVis = MaterialProps( 14 );
			Material( MaterNum ).ReflFrontBeamDiffVis = MaterialProps( 15 );
			Material( MaterNum ).ReflBackBeamDiffVis = MaterialProps( 16 );
			Material( MaterNum ).TausDiffDiff = MaterialProps( 17 );
			Material( MaterNum ).ReflFrontDiffDiff = MaterialProps( 18 );
			Material( MaterNum ).ReflBackDiffDiff = MaterialProps( 19 );
			Material( MaterNum ).TausDiffDiffVis = MaterialProps( 20 );
			Material( MaterNum ).ReflFrontDiffDiffVis = MaterialProps( 21 );
			Material( MaterNum ).ReflBackDiffDiffVis = MaterialProps( 22 );
			Material( MaterNum ).TausThermal = MaterialProps( 23 );
			Material( MaterNum ).EmissThermalFront = MaterialProps( 24 );
			Material( MaterNum ).EmissThermalBack = MaterialProps( 25 );
			// Assumes thermal emissivity is the same as thermal absorptance
			Material( MaterNum ).AbsorpThermalFront = Material( MaterNum ).EmissThermalFront;
			Material( MaterNum ).AbsorpThermalBack = Material( MaterNum ).EmissThermalBack;
			Material( MaterNum ).TransThermal = Material( MaterNum ).TausThermal;

			if ( SameString( MaterialNames( 2 ), "SpectralAverage" ) ) Material( MaterNum ).GlassSpectralDataPtr = 0;

			//IF(Material(MaterNum)%GlassSpectralDataPtr == 0 .AND. SameString(MaterialNames(2),'Spectral')) THEN
			//  ErrorsFound = .TRUE.
			//  CALL ShowSevereError(TRIM(CurrentModuleObject)//'="'//Trim(Material(MaterNum)%Name)// &
			//        '" has '//TRIM(cAlphaFieldNames(2))//' = Spectral but has no matching MaterialProperty:GlazingSpectralData set')
			//  IF (lAlphaFieldBlanks(3)) THEN
			//    CALL ShowContinueError('...'//TRIM(cAlphaFieldNames(3))//' is blank.')
			//  ELSE
			//    CALL ShowContinueError('...'//TRIM(cAlphaFieldNames(3))//'="'//TRIM(MaterialNames(3))//  &
			//       '" not found as item in MaterialProperty:GlazingSpectralData objects.')
			//  END IF
			//END IF

			if ( ! SameString( MaterialNames( 2 ), "SpectralAverage" ) ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + Material( MaterNum ).Name + "\", invalid specification." );
				ShowContinueError( cAlphaFieldNames( 2 ) + " must be SpectralAverage, value=" + MaterialNames( 2 ) );
			}

		} // W5GlsMatEQL loop

		// Window gas materials (for gaps with a single gas)

		CurrentModuleObject = "WindowMaterial:Gas";
		for ( Loop = 1; Loop <= W5GasMat; ++Loop ) {

			//Call Input Get routine to retrieve material data
			GetObjectItem( CurrentModuleObject, Loop, MaterialNames, MaterialNumAlpha, MaterialProps, MaterialNumProp, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			ErrorInName = false;
			IsBlank = false;
			VerifyName( MaterialNames( 1 ), Material, MaterNum, ErrorInName, IsBlank, CurrentModuleObject + " Name" );
			if ( ErrorInName ) {
				ShowContinueError( "...All Material names must be unique regardless of subtype." );
				ErrorsFound = true;
				continue;
			}

			++MaterNum;
			Material( MaterNum ).Group = WindowGas;
			Material( MaterNum ).GasType( 1 ) = -1;
			Material( MaterNum ).NumberOfGasesInMixture = 1;
			Material( MaterNum ).GasFract( 1 ) = 1.0;

			//Load the material derived type from the input data.

			Material( MaterNum ).Name = MaterialNames( 1 );
			Material( MaterNum ).NumberOfGasesInMixture = 1;
			TypeOfGas = MaterialNames( 2 );
			if ( TypeOfGas == "AIR" ) Material( MaterNum ).GasType( 1 ) = 1;
			if ( TypeOfGas == "ARGON" ) Material( MaterNum ).GasType( 1 ) = 2;
			if ( TypeOfGas == "KRYPTON" ) Material( MaterNum ).GasType( 1 ) = 3;
			if ( TypeOfGas == "XENON" ) Material( MaterNum ).GasType( 1 ) = 4;
			if ( TypeOfGas == "CUSTOM" ) Material( MaterNum ).GasType( 1 ) = 0;

			if ( Material( MaterNum ).GasType( 1 ) == -1 ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value." );
				ShowContinueError( cAlphaFieldNames( 2 ) + " entered value=\"" + TypeOfGas + "\" should be Air, Argon, Krypton, Xenon or Custom." );
			}

			Material( MaterNum ).Roughness = MediumRough;

			Material( MaterNum ).Thickness = MaterialProps( 1 );
			Material( MaterNum ).ROnly = true;

			GasType = Material( MaterNum ).GasType( 1 );
			if ( GasType >= 1 && GasType <= 4 ) {
				Material( MaterNum ).GasWght( 1 ) = GasWght( GasType );
				Material( MaterNum ).GasSpecHeatRatio( 1 ) = GasSpecificHeatRatio( GasType );
				for ( ICoeff = 1; ICoeff <= 3; ++ICoeff ) {
					Material( MaterNum ).GasCon( ICoeff, 1 ) = GasCoeffsCon( ICoeff, GasType );
					Material( MaterNum ).GasVis( ICoeff, 1 ) = GasCoeffsVis( ICoeff, GasType );
					Material( MaterNum ).GasCp( ICoeff, 1 ) = GasCoeffsCp( ICoeff, GasType );
				}
			}

			// Custom gas

			if ( GasType == 0 ) {
				for ( ICoeff = 1; ICoeff <= 3; ++ICoeff ) {
					Material( MaterNum ).GasCon( ICoeff, 1 ) = MaterialProps( 1 + ICoeff );
					Material( MaterNum ).GasVis( ICoeff, 1 ) = MaterialProps( 4 + ICoeff );
					Material( MaterNum ).GasCp( ICoeff, 1 ) = MaterialProps( 7 + ICoeff );
				}
				Material( MaterNum ).GasWght( 1 ) = MaterialProps( 11 );
				Material( MaterNum ).GasSpecHeatRatio( 1 ) = MaterialProps( 12 );

				// Check for errors in custom gas properties
				//      IF(Material(MaterNum)%GasCon(1,1) <= 0.0) THEN
				//        ErrorsFound = .TRUE.
				//        CALL ShowSevereError('Conductivity Coefficient A for custom window gas='&
				//                 //TRIM(MaterialNames(1))//' should be > 0.')
				//      END IF

				if ( Material( MaterNum ).GasVis( 1, 1 ) <= 0.0 ) {
					ErrorsFound = true;
					ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value." );
					ShowContinueError( cNumericFieldNames( 3 + ICoeff ) + " not > 0.0" );
				}
				if ( Material( MaterNum ).GasCp( 1, 1 ) <= 0.0 ) {
					ErrorsFound = true;
					ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value." );
					ShowContinueError( cNumericFieldNames( 5 + ICoeff ) + " not > 0.0" );
				}
				if ( Material( MaterNum ).GasWght( 1 ) <= 0.0 ) {
					ErrorsFound = true;
					ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value." );
					ShowContinueError( cNumericFieldNames( 8 ) + " not > 0.0" );
				}
			}

			// Nominal resistance of gap at room temperature
			if ( ! ErrorsFound ) {
				DenomRGas = ( Material( MaterNum ).GasCon( 1, 1 ) + Material( MaterNum ).GasCon( 2, 1 ) * 300.0 + Material( MaterNum ).GasCon( 3, 1 ) * 90000.0 );
				if ( DenomRGas > 0.0 ) {
					NominalR( MaterNum ) = Material( MaterNum ).Thickness / DenomRGas;
				} else {
					ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value." );
					ShowContinueError( "Nominal resistance of gap at room temperature calculated at a negative Conductivity=[" + RoundSigDigits( DenomRGas, 3 ) + "]." );
					ErrorsFound = true;
				}
			}

		}

		// Window gap materials (for gaps with a single gas for EquivalentLayer)

		CurrentModuleObject = "WindowMaterial:Gap:EquivalentLayer";
		for ( Loop = 1; Loop <= W5GapMatEQL; ++Loop ) {

			//Call Input Get routine to retrieve material data
			GetObjectItem( CurrentModuleObject, Loop, MaterialNames, MaterialNumAlpha, MaterialProps, MaterialNumProp, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			ErrorInName = false;
			IsBlank = false;
			VerifyName( MaterialNames( 1 ), Material, MaterNum, ErrorInName, IsBlank, CurrentModuleObject + " Name" );
			if ( ErrorInName ) {
				ShowContinueError( "...All Material names must be unique regardless of subtype." );
				ErrorsFound = true;
				continue;
			}

			++MaterNum;
			Material( MaterNum ).Group = GapEquivalentLayer;
			Material( MaterNum ).GasType( 1 ) = -1;
			Material( MaterNum ).NumberOfGasesInMixture = 1;
			Material( MaterNum ).GasFract( 1 ) = 1.0;

			//Load the material derived type from the input data.

			Material( MaterNum ).Name = MaterialNames( 1 );
			Material( MaterNum ).NumberOfGasesInMixture = 1;
			TypeOfGas = MaterialNames( 2 );
			Material( MaterNum ).GasName = TypeOfGas;
			if ( TypeOfGas == "AIR" ) Material( MaterNum ).GasType( 1 ) = 1;
			if ( TypeOfGas == "ARGON" ) Material( MaterNum ).GasType( 1 ) = 2;
			if ( TypeOfGas == "KRYPTON" ) Material( MaterNum ).GasType( 1 ) = 3;
			if ( TypeOfGas == "XENON" ) Material( MaterNum ).GasType( 1 ) = 4;
			if ( TypeOfGas == "CUSTOM" ) Material( MaterNum ).GasType( 1 ) = 0;

			if ( Material( MaterNum ).GasType( 1 ) == -1 ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value." );
				ShowContinueError( cAlphaFieldNames( 2 ) + " entered value=\"" + TypeOfGas + "\" should be Air, Argon, Krypton, Xenon" );
			}

			Material( MaterNum ).Roughness = MediumRough;

			Material( MaterNum ).Thickness = MaterialProps( 1 );
			Material( MaterNum ).ROnly = true;

			GasType = Material( MaterNum ).GasType( 1 );
			if ( GasType >= 1 && GasType <= 4 ) {
				Material( MaterNum ).GasWght( 1 ) = GasWght( GasType );
				Material( MaterNum ).GasSpecHeatRatio( 1 ) = GasSpecificHeatRatio( GasType );
				for ( ICoeff = 1; ICoeff <= 3; ++ICoeff ) {
					Material( MaterNum ).GasCon( ICoeff, 1 ) = GasCoeffsCon( ICoeff, GasType );
					Material( MaterNum ).GasVis( ICoeff, 1 ) = GasCoeffsVis( ICoeff, GasType );
					Material( MaterNum ).GasCp( ICoeff, 1 ) = GasCoeffsCp( ICoeff, GasType );
				}
			}

			if ( ! lAlphaFieldBlanks( 2 ) ) {
				// Get gap vent type
				if ( SameString( MaterialNames( 3 ), "Sealed" ) ) {
					Material( MaterNum ).GapVentType = 1;
				} else if ( SameString( MaterialNames( 3 ), "VentedIndoor" ) ) {
					Material( MaterNum ).GapVentType = 2;
				} else if ( SameString( MaterialNames( 3 ), "VentedOutdoor" ) ) {
					Material( MaterNum ).GapVentType = 3;
				} else {
					ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal gap vent type." );
					ShowContinueError( "Gap vent type allowed are Sealed, VentedIndoor, or VentedOutdoor." + cAlphaFieldNames( 3 ) + " entered =" + MaterialNames( 3 ) );
					Material( MaterNum ).GapVentType = 1;
					//ErrorsFound=.TRUE.
				}
			}

			if ( GasType == 0 ) {
				for ( ICoeff = 1; ICoeff <= 3; ++ICoeff ) {
					Material( MaterNum ).GasCon( ICoeff, 1 ) = MaterialProps( 1 + ICoeff );
					Material( MaterNum ).GasVis( ICoeff, 1 ) = MaterialProps( 4 + ICoeff );
					Material( MaterNum ).GasCp( ICoeff, 1 ) = MaterialProps( 7 + ICoeff );
				}
				Material( MaterNum ).GasWght( 1 ) = MaterialProps( 11 );
				Material( MaterNum ).GasSpecHeatRatio( 1 ) = MaterialProps( 12 );

				if ( Material( MaterNum ).GasVis( 1, 1 ) <= 0.0 ) {
					ErrorsFound = true;
					ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value." );
					ShowContinueError( cNumericFieldNames( 5 ) + " not > 0.0" );
				}
				if ( Material( MaterNum ).GasCp( 1, 1 ) <= 0.0 ) {
					ErrorsFound = true;
					ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value." );
					ShowContinueError( cNumericFieldNames( 8 ) + " not > 0.0" );
				}
				if ( Material( MaterNum ).GasWght( 1 ) <= 0.0 ) {
					ErrorsFound = true;
					ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value." );
					ShowContinueError( cNumericFieldNames( 11 ) + " not > 0.0" );
				}

			}

			// Nominal resistance of gap at room temperature
			if ( ! ErrorsFound ) {
				DenomRGas = ( Material( MaterNum ).GasCon( 1, 1 ) + Material( MaterNum ).GasCon( 2, 1 ) * 300.0 + Material( MaterNum ).GasCon( 3, 1 ) * 90000.0 );
				if ( DenomRGas > 0.0 ) {
					NominalR( MaterNum ) = Material( MaterNum ).Thickness / DenomRGas;
				} else {
					ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value." );
					ShowContinueError( "Nominal resistance of gap at room temperature calculated at a negative Conductivity=[" + RoundSigDigits( DenomRGas, 3 ) + "]." );
					ErrorsFound = true;
				}
			}

		}

		// Window gas mixtures (for gaps with two or more gases)

		CurrentModuleObject = "WindowMaterial:GasMixture";
		for ( Loop = 1; Loop <= W5GasMatMixture; ++Loop ) {

			//Call Input Get routine to retrieve material data
			GetObjectItem( CurrentModuleObject, Loop, cAlphaArgs, MaterialNumAlpha, MaterialProps, MaterialNumProp, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			ErrorInName = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), Material, MaterNum, ErrorInName, IsBlank, CurrentModuleObject + " Name" );
			if ( ErrorInName ) {
				ShowContinueError( "...All Material names must be unique regardless of subtype." );
				ErrorsFound = true;
				continue;
			}

			++MaterNum;
			Material( MaterNum ).Group = WindowGasMixture;
			Material( MaterNum ).GasType = -1;

			//Load the material derived type from the input data.

			Material( MaterNum ).Name = cAlphaArgs( 1 );
			NumGases = MaterialProps( 2 );
			Material( MaterNum ).NumberOfGasesInMixture = NumGases;
			for ( NumGas = 1; NumGas <= NumGases; ++NumGas ) {
				TypeOfGas = cAlphaArgs( 1 + NumGas );
				if ( TypeOfGas == "AIR" ) Material( MaterNum ).GasType( NumGas ) = 1;
				if ( TypeOfGas == "ARGON" ) Material( MaterNum ).GasType( NumGas ) = 2;
				if ( TypeOfGas == "KRYPTON" ) Material( MaterNum ).GasType( NumGas ) = 3;
				if ( TypeOfGas == "XENON" ) Material( MaterNum ).GasType( NumGas ) = 4;
				if ( Material( MaterNum ).GasType( NumGas ) == -1 ) {
					ErrorsFound = true;
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", Illegal value." );
					ShowContinueError( cAlphaFieldNames( 2 + NumGas ) + " entered value=\"" + TypeOfGas + "\" should be Air, Argon, Krypton, or Xenon." );
				}
			}

			Material( MaterNum ).Roughness = MediumRough; // Unused

			Material( MaterNum ).Thickness = MaterialProps( 1 );
			if ( Material( MaterNum ).Thickness <= 0.0 ) {
				ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", Illegal value." );
				ShowContinueError( cNumericFieldNames( 1 ) + " must be greater than 0." );
			}
			Material( MaterNum ).ROnly = true;

			for ( NumGas = 1; NumGas <= NumGases; ++NumGas ) {
				GasType = Material( MaterNum ).GasType( NumGas );
				if ( GasType >= 1 && GasType <= 4 ) {
					Material( MaterNum ).GasWght( NumGas ) = GasWght( GasType );
					Material( MaterNum ).GasSpecHeatRatio( NumGas ) = GasSpecificHeatRatio( GasType );
					Material( MaterNum ).GasFract( NumGas ) = MaterialProps( 2 + NumGas );
					for ( ICoeff = 1; ICoeff <= 3; ++ICoeff ) {
						Material( MaterNum ).GasCon( ICoeff, NumGas ) = GasCoeffsCon( ICoeff, GasType );
						Material( MaterNum ).GasVis( ICoeff, NumGas ) = GasCoeffsVis( ICoeff, GasType );
						Material( MaterNum ).GasCp( ICoeff, NumGas ) = GasCoeffsCp( ICoeff, GasType );
					}
				}
			}

			// Nominal resistance of gap at room temperature (based on first gas in mixture)
			NominalR( MaterNum ) = Material( MaterNum ).Thickness / ( Material( MaterNum ).GasCon( 1, 1 ) + Material( MaterNum ).GasCon( 2, 1 ) * 300.0 + Material( MaterNum ).GasCon( 3, 1 ) * 90000.0 );

		}

		// Window Shade Materials

		CurrentModuleObject = "WindowMaterial:Shade";
		for ( Loop = 1; Loop <= TotShades; ++Loop ) {

			//Call Input Get routine to retrieve material data
			GetObjectItem( CurrentModuleObject, Loop, MaterialNames, MaterialNumAlpha, MaterialProps, MaterialNumProp, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			ErrorInName = false;
			IsBlank = false;
			VerifyName( MaterialNames( 1 ), Material, MaterNum, ErrorInName, IsBlank, CurrentModuleObject + " Name" );
			if ( ErrorInName ) {
				ShowContinueError( "...All Material names must be unique regardless of subtype." );
				ErrorsFound = true;
				continue;
			}

			++MaterNum;
			Material( MaterNum ).Group = Shade;

			//Load the material derived type from the input data.

			Material( MaterNum ).Name = MaterialNames( 1 );
			Material( MaterNum ).Roughness = MediumRough;
			Material( MaterNum ).Trans = MaterialProps( 1 );
			Material( MaterNum ).ReflectShade = MaterialProps( 2 );
			Material( MaterNum ).TransVis = MaterialProps( 3 );
			Material( MaterNum ).ReflectShadeVis = MaterialProps( 4 );
			Material( MaterNum ).AbsorpThermal = MaterialProps( 5 );
			Material( MaterNum ).AbsorpThermalInput = MaterialProps( 5 );
			Material( MaterNum ).TransThermal = MaterialProps( 6 );
			Material( MaterNum ).Thickness = MaterialProps( 7 );
			Material( MaterNum ).Conductivity = MaterialProps( 8 );
			Material( MaterNum ).AbsorpSolar = max( 0.0, 1.0 - Material( MaterNum ).Trans - Material( MaterNum ).ReflectShade );
			Material( MaterNum ).AbsorpSolarInput = Material( MaterNum ).AbsorpSolar;
			Material( MaterNum ).WinShadeToGlassDist = MaterialProps( 9 );
			Material( MaterNum ).WinShadeTopOpeningMult = MaterialProps( 10 );
			Material( MaterNum ).WinShadeBottomOpeningMult = MaterialProps( 11 );
			Material( MaterNum ).WinShadeLeftOpeningMult = MaterialProps( 12 );
			Material( MaterNum ).WinShadeRightOpeningMult = MaterialProps( 13 );
			Material( MaterNum ).WinShadeAirFlowPermeability = MaterialProps( 14 );
			Material( MaterNum ).ROnly = true;

			if ( Material( MaterNum ).Conductivity > 0.0 ) {
				NominalR( MaterNum ) = Material( MaterNum ).Thickness / Material( MaterNum ).Conductivity;
			} else {
				NominalR( MaterNum ) = 1.0;
			}

			if ( MaterialProps( 1 ) + MaterialProps( 2 ) >= 1.0 ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value combination." );
				ShowContinueError( cNumericFieldNames( 1 ) + " + " + cNumericFieldNames( 2 ) + " not < 1.0" );
			}

			if ( MaterialProps( 3 ) + MaterialProps( 4 ) >= 1.0 ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value combination." );
				ShowContinueError( cNumericFieldNames( 3 ) + " + " + cNumericFieldNames( 4 ) + " not < 1.0" );
			}

			if ( MaterialProps( 5 ) + MaterialProps( 6 ) >= 1.0 ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value combination." );
				ShowContinueError( cNumericFieldNames( 5 ) + " + " + cNumericFieldNames( 6 ) + " not < 1.0" );
			}

		}

		// Window Shade Materials

		CurrentModuleObject = "WindowMaterial:Shade:EquivalentLayer";
		for ( Loop = 1; Loop <= TotShadesEQL; ++Loop ) {

			MaterialProps = 0;

			//Call Input Get routine to retrieve material data
			GetObjectItem( CurrentModuleObject, Loop, MaterialNames, MaterialNumAlpha, MaterialProps, MaterialNumProp, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			ErrorInName = false;
			IsBlank = false;
			VerifyName( MaterialNames( 1 ), Material, MaterNum, ErrorInName, IsBlank, CurrentModuleObject + " Name" );
			if ( ErrorInName ) {
				ShowContinueError( "...All Material names must be unique regardless of subtype." );
				ErrorsFound = true;
				continue;
			}

			++MaterNum;
			Material( MaterNum ).Group = ShadeEquivalentLayer;

			Material( MaterNum ).Name = MaterialNames( 1 );
			Material( MaterNum ).Roughness = MediumRough;
			Material( MaterNum ).ROnly = true;

			//  Front side and back side have the same beam-Beam Transmittance
			Material( MaterNum ).TausFrontBeamBeam = MaterialProps( 1 );
			Material( MaterNum ).TausBackBeamBeam = MaterialProps( 1 );
			Material( MaterNum ).TausFrontBeamDiff = MaterialProps( 2 );
			Material( MaterNum ).TausBackBeamDiff = MaterialProps( 3 );
			Material( MaterNum ).ReflFrontBeamDiff = MaterialProps( 4 );
			Material( MaterNum ).ReflBackBeamDiff = MaterialProps( 5 );
			Material( MaterNum ).TausFrontBeamBeamVis = MaterialProps( 6 );
			Material( MaterNum ).TausFrontBeamDiffVis = MaterialProps( 7 );
			Material( MaterNum ).ReflFrontBeamDiffVis = MaterialProps( 8 );
			Material( MaterNum ).TausThermal = MaterialProps( 9 );
			Material( MaterNum ).EmissThermalFront = MaterialProps( 10 );
			Material( MaterNum ).EmissThermalBack = MaterialProps( 11 );
			// Assumes thermal emissivity is the same as thermal absorptance
			Material( MaterNum ).AbsorpThermalFront = Material( MaterNum ).EmissThermalFront;
			Material( MaterNum ).AbsorpThermalBack = Material( MaterNum ).EmissThermalBack;
			Material( MaterNum ).TransThermal = Material( MaterNum ).TausThermal;

			if ( MaterialProps( 1 ) + MaterialProps( 2 ) + MaterialProps( 4 ) >= 1.0 ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value combination." );
				ShowContinueError( cNumericFieldNames( 1 ) + " + " + cNumericFieldNames( 2 ) + " + " + cNumericFieldNames( 4 ) + "not < 1.0" );
			}
			if ( MaterialProps( 1 ) + MaterialProps( 3 ) + MaterialProps( 5 ) >= 1.0 ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value combination." );
				ShowContinueError( cNumericFieldNames( 1 ) + " + " + cNumericFieldNames( 3 ) + " + " + cNumericFieldNames( 5 ) + "not < 1.0" );
			}
			if ( MaterialProps( 6 ) + MaterialProps( 7 ) + MaterialProps( 8 ) >= 1.0 ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value combination." );
				ShowContinueError( cNumericFieldNames( 6 ) + " + " + cNumericFieldNames( 7 ) + " + " + cNumericFieldNames( 8 ) + "not < 1.0" );
			}
			if ( MaterialProps( 9 ) + MaterialProps( 10 ) >= 1.0 ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value combination." );
				ShowContinueError( cNumericFieldNames( 9 ) + " + " + cNumericFieldNames( 10 ) + " not < 1.0" );
			}
			if ( MaterialProps( 9 ) + MaterialProps( 11 ) >= 1.0 ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value combination." );
				ShowContinueError( cNumericFieldNames( 9 ) + " + " + cNumericFieldNames( 11 ) + " not < 1.0" );
			}

		} // TotShadesEQL loop

		// Window drape materials

		CurrentModuleObject = "WindowMaterial:Drape:EquivalentLayer";
		for ( Loop = 1; Loop <= TotDrapesEQL; ++Loop ) {

			MaterialProps = 0;

			//Call Input Get routine to retrieve material data
			GetObjectItem( CurrentModuleObject, Loop, MaterialNames, MaterialNumAlpha, MaterialProps, MaterialNumProp, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			ErrorInName = false;
			IsBlank = false;
			VerifyName( MaterialNames( 1 ), Material, MaterNum, ErrorInName, IsBlank, CurrentModuleObject + " Name" );
			if ( ErrorInName ) {
				ShowContinueError( "...All Material names must be unique regardless of subtype." );
				ErrorsFound = true;
				continue;
			}

			++MaterNum;
			Material( MaterNum ).Group = DrapeEquivalentLayer;

			Material( MaterNum ).Name = MaterialNames( 1 );
			Material( MaterNum ).Roughness = MediumRough;
			Material( MaterNum ).ROnly = true;

			//  Front side and back side have the same properties
			Material( MaterNum ).TausFrontBeamBeam = MaterialProps( 1 );
			Material( MaterNum ).TausBackBeamBeam = MaterialProps( 1 );

			Material( MaterNum ).TausFrontBeamDiff = MaterialProps( 2 );
			Material( MaterNum ).TausBackBeamDiff = MaterialProps( 3 );

			Material( MaterNum ).ReflFrontBeamDiff = MaterialProps( 4 );
			Material( MaterNum ).ReflBackBeamDiff = MaterialProps( 5 );
			Material( MaterNum ).TausFrontBeamBeamVis = MaterialProps( 6 );
			Material( MaterNum ).TausFrontBeamDiffVis = MaterialProps( 7 );
			Material( MaterNum ).ReflFrontBeamDiffVis = MaterialProps( 8 );
			Material( MaterNum ).TausThermal = MaterialProps( 9 );
			Material( MaterNum ).EmissThermalFront = MaterialProps( 10 );
			Material( MaterNum ).EmissThermalBack = MaterialProps( 11 );
			// Assumes thermal emissivity is the same as thermal absorptance
			Material( MaterNum ).AbsorpThermalFront = Material( MaterNum ).EmissThermalFront;
			Material( MaterNum ).AbsorpThermalBack = Material( MaterNum ).EmissThermalBack;
			Material( MaterNum ).TransThermal = Material( MaterNum ).TausThermal;

			if ( ! lNumericFieldBlanks( 12 ) && ! lNumericFieldBlanks( 13 ) ) {
				if ( MaterialProps( 12 ) != 0.0 && MaterialProps( 13 ) != 0.0 ) {
					Material( MaterNum ).PleatedDrapeWidth = MaterialProps( 12 );
					Material( MaterNum ).PleatedDrapeLength = MaterialProps( 13 );
					Material( MaterNum ).ISPleatedDrape = true;
				}
			} else {
				Material( MaterNum ).ISPleatedDrape = false;
			}
			if ( MaterialProps( 1 ) + MaterialProps( 2 ) + MaterialProps( 4 ) >= 1.0 ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value combination." );
				ShowContinueError( cNumericFieldNames( 1 ) + " + " + cNumericFieldNames( 2 ) + " + " + cNumericFieldNames( 4 ) + "not < 1.0" );
			}
			if ( MaterialProps( 6 ) + MaterialProps( 7 ) + MaterialProps( 8 ) >= 1.0 ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value combination." );
				ShowContinueError( cNumericFieldNames( 4 ) + " + " + cNumericFieldNames( 5 ) + " + " + cNumericFieldNames( 6 ) + "not < 1.0" );
			}
			if ( MaterialProps( 9 ) + MaterialProps( 10 ) > 1.0 ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value combination." );
				ShowContinueError( cNumericFieldNames( 9 ) + " + " + cNumericFieldNames( 10 ) + " not < 1.0" );
			}

		} // TotDrapesEQL loop

		// Window Screen Materials

		CurrentModuleObject = "WindowMaterial:Screen";
		for ( Loop = 1; Loop <= TotScreens; ++Loop ) {

			//Call GetObjectItem routine to retrieve material data
			GetObjectItem( CurrentModuleObject, Loop, MaterialNames, MaterialNumAlpha, MaterialProps, MaterialNumProp, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			ErrorInName = false;
			IsBlank = false;
			VerifyName( MaterialNames( 1 ), Material, MaterNum, ErrorInName, IsBlank, CurrentModuleObject + " Name" );
			if ( ErrorInName ) {
				ShowContinueError( "...All Material names must be unique regardless of subtype." );
				ErrorsFound = true;
				continue;
			}

			++MaterNum;
			Material( MaterNum ).Group = Screen;

			//Load the material derived type from the input data.

			Material( MaterNum ).Name = MaterialNames( 1 );
			Material( MaterNum ).ReflectanceModeling = MaterialNames( 2 );
			if ( ! ( SameString( MaterialNames( 2 ), "DoNotModel" ) || SameString( MaterialNames( 2 ), "ModelAsDirectBeam" ) || SameString( MaterialNames( 2 ), "ModelAsDiffuse" ) ) ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value." );
				ShowContinueError( cAlphaFieldNames( 2 ) + "=\"" + MaterialNames( 2 ) + "\", must be one of DoNotModel, ModelAsDirectBeam or ModelAsDiffuse." );
			}
			Material( MaterNum ).Roughness = MediumRough;
			Material( MaterNum ).ReflectShade = MaterialProps( 1 );
			if ( Material( MaterNum ).ReflectShade < 0.0 || Material( MaterNum ).ReflectShade > 1.0 ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value." );
				ShowContinueError( cNumericFieldNames( 1 ) + " must be >= 0 and <= 1" );
			}
			Material( MaterNum ).ReflectShadeVis = MaterialProps( 2 );
			if ( Material( MaterNum ).ReflectShadeVis < 0.0 || Material( MaterNum ).ReflectShadeVis > 1.0 ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value." );
				ShowContinueError( cNumericFieldNames( 2 ) + " must be >= 0 and <= 1 for material " + Material( MaterNum ).Name + '.' );
			}
			Material( MaterNum ).AbsorpThermal = MaterialProps( 3 );
			Material( MaterNum ).AbsorpThermalInput = MaterialProps( 3 );
			if ( Material( MaterNum ).AbsorpThermal < 0.0 || Material( MaterNum ).AbsorpThermal > 1.0 ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value." );
				ShowContinueError( cNumericFieldNames( 3 ) + " must be >= 0 and <= 1" );
			}
			Material( MaterNum ).Conductivity = MaterialProps( 4 );
			Material( MaterNum ).Thickness = MaterialProps( 6 ); // thickness = diameter

			if ( MaterialProps( 5 ) > 0.0 ) {
				//      SurfaceScreens(ScNum)%ScreenDiameterToSpacingRatio = MaterialProps(6)/MaterialProps(5) or 1-SQRT(Material(MaterNum)%Trans
				if ( MaterialProps( 6 ) / MaterialProps( 5 ) >= 1.0 ) {
					ErrorsFound = true;
					ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value combination." );
					ShowContinueError( cNumericFieldNames( 6 ) + " must be less than " + cNumericFieldNames( 5 ) );
				} else {
					//       Calculate direct normal transmittance (open area fraction)
					Material( MaterNum ).Trans = pow_2( 1.0 - MaterialProps( 6 ) / MaterialProps( 5 ) );
				}
			} else {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value." );
				ShowContinueError( cNumericFieldNames( 5 ) + " must be > 0." );
				MaterialProps( 5 ) = 0.000000001;
			}

			if ( MaterialProps( 6 ) <= 0.0 ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value." );
				ShowContinueError( cNumericFieldNames( 6 ) + " must be > 0." );
			}

			//   Modify reflectance to account for the open area in the screen assembly
			Material( MaterNum ).ReflectShade *= ( 1.0 - Material( MaterNum ).Trans );
			Material( MaterNum ).ReflectShadeVis *= ( 1.0 - Material( MaterNum ).Trans );

			Material( MaterNum ).WinShadeToGlassDist = MaterialProps( 7 );
			if ( Material( MaterNum ).WinShadeToGlassDist < 0.001 || Material( MaterNum ).WinShadeToGlassDist > 1.0 ) {
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value." );
				ShowContinueError( cNumericFieldNames( 7 ) + " must be greater than or equal to 0.001 and less than or equal to 1." );
			}

			Material( MaterNum ).WinShadeTopOpeningMult = MaterialProps( 8 );
			if ( Material( MaterNum ).WinShadeTopOpeningMult < 0.0 || Material( MaterNum ).WinShadeTopOpeningMult > 1.0 ) {
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value." );
				ShowContinueError( cNumericFieldNames( 8 ) + " must be greater than or equal to 0 and less than or equal to 1." );
			}

			Material( MaterNum ).WinShadeBottomOpeningMult = MaterialProps( 9 );
			if ( Material( MaterNum ).WinShadeBottomOpeningMult < 0.0 || Material( MaterNum ).WinShadeBottomOpeningMult > 1.0 ) {
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value." );
				ShowContinueError( cNumericFieldNames( 9 ) + " must be greater than or equal to 0 and less than or equal to 1." );
			}

			Material( MaterNum ).WinShadeLeftOpeningMult = MaterialProps( 10 );
			if ( Material( MaterNum ).WinShadeLeftOpeningMult < 0.0 || Material( MaterNum ).WinShadeLeftOpeningMult > 1.0 ) {
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value." );
				ShowContinueError( cNumericFieldNames( 10 ) + " must be greater than or equal to 0 and less than or equal to 1." );
			}

			Material( MaterNum ).WinShadeRightOpeningMult = MaterialProps( 11 );
			if ( Material( MaterNum ).WinShadeRightOpeningMult < 0.0 || Material( MaterNum ).WinShadeRightOpeningMult > 1.0 ) {
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value." );
				ShowContinueError( cNumericFieldNames( 11 ) + " must be greater than or equal to 0 and less than or equal to 1." );
			}

			Material( MaterNum ).ScreenMapResolution = MaterialProps( 12 );
			if ( Material( MaterNum ).ScreenMapResolution < 0 || Material( MaterNum ).ScreenMapResolution > 5 || Material( MaterNum ).ScreenMapResolution == 4 ) {
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value." );
				ShowContinueError( cNumericFieldNames( 12 ) + " must be 0, 1, 2, 3, or 5." );
				ErrorsFound = true;
			}

			//   Default air flow permeability to open area fraction
			Material( MaterNum ).WinShadeAirFlowPermeability = Material( MaterNum ).Trans;
			Material( MaterNum ).TransThermal = Material( MaterNum ).Trans;
			Material( MaterNum ).TransVis = Material( MaterNum ).Trans;

			Material( MaterNum ).ROnly = true;

			//   Calculate absorptance accounting for the open area in the screen assembly (used only in CreateShadedWindowConstruction)
			Material( MaterNum ).AbsorpSolar = max( 0.0, 1.0 - Material( MaterNum ).Trans - Material( MaterNum ).ReflectShade );
			Material( MaterNum ).AbsorpSolarInput = Material( MaterNum ).AbsorpSolar;
			Material( MaterNum ).AbsorpVisible = max( 0.0, 1.0 - Material( MaterNum ).TransVis - Material( MaterNum ).ReflectShadeVis );
			Material( MaterNum ).AbsorpVisibleInput = Material( MaterNum ).AbsorpVisible;
			Material( MaterNum ).AbsorpThermal *= ( 1.0 - Material( MaterNum ).Trans );
			Material( MaterNum ).AbsorpThermalInput = Material( MaterNum ).AbsorpThermal;

			if ( Material( MaterNum ).Conductivity > 0.0 ) {
				NominalR( MaterNum ) = ( 1.0 - Material( MaterNum ).Trans ) * Material( MaterNum ).Thickness / Material( MaterNum ).Conductivity;
			} else {
				NominalR( MaterNum ) = 1.0;
				ShowWarningError( "Conductivity for material=\"" + Material( MaterNum ).Name + "\" must be greater than 0 for calculating Nominal R-value, Nominal R is defaulted to 1 and the simulation continues." );
			}

			if ( Material( MaterNum ).Trans + Material( MaterNum ).ReflectShade >= 1.0 ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value combination." );
				ShowContinueError( "Calculated solar transmittance + solar reflectance not < 1.0" );
				ShowContinueError( "See Engineering Reference for calculation procedure for solar transmittance." );
			}

			if ( Material( MaterNum ).TransVis + Material( MaterNum ).ReflectShadeVis >= 1.0 ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value combination." );
				ShowContinueError( "Calculated visible transmittance + visible reflectance not < 1.0" );
				ShowContinueError( "See Engineering Reference for calculation procedure for visible solar transmittance." );
			}

			if ( Material( MaterNum ).TransThermal + Material( MaterNum ).AbsorpThermal >= 1.0 ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value combination." );
				ShowSevereError( "Thermal hemispherical emissivity plus open area fraction (1-diameter/spacing)**2 not < 1.0" );
			}

		}

		CurrentModuleObject = "WindowMaterial:Screen:EquivalentLayer";
		for ( Loop = 1; Loop <= TotScreensEQL; ++Loop ) {

			MaterialProps = 0;

			//Call GetObjectItem routine to retrieve material data
			GetObjectItem( CurrentModuleObject, Loop, MaterialNames, MaterialNumAlpha, MaterialProps, MaterialNumProp, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			ErrorInName = false;
			IsBlank = false;
			VerifyName( MaterialNames( 1 ), Material, MaterNum, ErrorInName, IsBlank, CurrentModuleObject + " Name" );
			if ( ErrorInName ) {
				ShowContinueError( "...All Material names must be unique regardless of subtype." );
				ErrorsFound = true;
				continue;
			}

			++MaterNum;
			Material( MaterNum ).Group = ScreenEquivalentLayer;

			// Load the material derived type from the input data.
			// WindowMaterial:Screen:EquivalentLayer,
			Material( MaterNum ).Name = MaterialNames( 1 );
			Material( MaterNum ).Roughness = MediumRough;
			Material( MaterNum ).ROnly = true;
			Material( MaterNum ).TausFrontBeamBeam = MaterialProps( 1 );
			Material( MaterNum ).TausBackBeamBeam = MaterialProps( 1 );
			Material( MaterNum ).TausFrontBeamDiff = MaterialProps( 2 );
			Material( MaterNum ).TausBackBeamDiff = MaterialProps( 2 );
			Material( MaterNum ).ReflFrontBeamDiff = MaterialProps( 3 );
			Material( MaterNum ).ReflBackBeamDiff = MaterialProps( 3 );
			Material( MaterNum ).TausFrontBeamBeamVis = MaterialProps( 4 );
			Material( MaterNum ).TausFrontBeamDiffVis = MaterialProps( 5 );
			Material( MaterNum ).ReflFrontDiffDiffVis = MaterialProps( 6 );
			Material( MaterNum ).TausThermal = MaterialProps( 7 );
			Material( MaterNum ).EmissThermalFront = MaterialProps( 8 );
			Material( MaterNum ).EmissThermalBack = MaterialProps( 8 );

			// Assumes thermal emissivity is the same as thermal absorptance
			Material( MaterNum ).AbsorpThermalFront = Material( MaterNum ).EmissThermalFront;
			Material( MaterNum ).AbsorpThermalBack = Material( MaterNum ).EmissThermalBack;
			Material( MaterNum ).TransThermal = Material( MaterNum ).TausThermal;

			if ( MaterialProps( 3 ) < 0.0 || MaterialProps( 3 ) > 1.0 ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value." );
				ShowContinueError( cNumericFieldNames( 3 ) + " must be >= 0 and <= 1" );
			}

			if ( MaterialProps( 6 ) < 0.0 || MaterialProps( 6 ) > 1.0 ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value." );
				ShowContinueError( cNumericFieldNames( 6 ) + " must be >= 0 and <= 1 for material " + Material( MaterNum ).Name + '.' );
			}

			if ( ! lNumericFieldBlanks( 9 ) ) {
				if ( MaterialProps( 9 ) > 0.00001 ) {
					Material( MaterNum ).ScreenWireSpacing = MaterialProps( 9 ); // screen wire spacing
				} else {
					ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value." );
					ShowContinueError( cNumericFieldNames( 9 ) + " must be > 0." );
					ShowContinueError( "...Setting screen wire spacing to a default value of 0.025m and simulation continues." );
					Material( MaterNum ).ScreenWireSpacing = 0.025;

				}
			}

			if ( ! lNumericFieldBlanks( 10 ) ) {
				if ( MaterialProps( 10 ) > 0.00001 && MaterialProps( 10 ) < Material( MaterNum ).ScreenWireSpacing ) {
					Material( MaterNum ).ScreenWireDiameter = MaterialProps( 10 ); // screen wire spacing
				} else {
					ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value." );
					ShowContinueError( cNumericFieldNames( 10 ) + " must be > 0." );
					ShowContinueError( "...Setting screen wire diameter to a default value of 0.005m and simulation continues." );
					Material( MaterNum ).ScreenWireDiameter = 0.005;
				}
			}

			if ( Material( MaterNum ).ScreenWireSpacing > 0.0 ) {
				if ( Material( MaterNum ).ScreenWireDiameter / Material( MaterNum ).ScreenWireSpacing >= 1.0 ) {
					ErrorsFound = true;
					ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value combination." );
					ShowContinueError( cNumericFieldNames( 10 ) + " must be less than " + cNumericFieldNames( 9 ) );
				} else {
					//  Calculate direct normal transmittance (open area fraction)
					Openness = pow_2( 1.0 - Material( MaterNum ).ScreenWireDiameter / Material( MaterNum ).ScreenWireSpacing );
					if ( ( Material( MaterNum ).TausFrontBeamBeam - Openness ) / Openness > 0.01 ) {
						ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", screen openness specified." );
						ShowContinueError( cNumericFieldNames( 1 ) + " is > 1.0% of the value calculated from input fields:" );
						ShowContinueError( cNumericFieldNames( 9 ) + " and " + ( cNumericFieldNames( 10 ) ) );
						ShowContinueError( " using the formula (1-diameter/spacing)**2" );
						ShowContinueError( " ...the screen diameter is recalculated from the material openness specified " );
						ShowContinueError( " ...and wire spacing using the formula = wire spacing * (1.0 - SQRT(Opennes))" );
						Material( MaterNum ).ScreenWireDiameter = Material( MaterNum ).ScreenWireSpacing * ( 1.0 - std::sqrt( Material( MaterNum ).TausFrontBeamBeam ) );
						ShowContinueError( " ...Recalculated " + cNumericFieldNames( 10 ) + '=' + RoundSigDigits( Material( MaterNum ).ScreenWireDiameter, 4 ) + " m" );
					}
				}
			}

			if ( Material( MaterNum ).TausFrontBeamBeam + Material( MaterNum ).ReflFrontBeamDiff >= 1.0 ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value combination." );
				ShowContinueError( "Calculated solar transmittance + solar reflectance not < 1.0" );
				ShowContinueError( "See Engineering Reference for calculation procedure for solar transmittance." );
			}

			if ( Material( MaterNum ).TausFrontBeamBeamVis + Material( MaterNum ).ReflFrontDiffDiffVis >= 1.0 ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value combination." );
				ShowContinueError( "Calculated visible transmittance + visible reflectance not < 1.0" );
				ShowContinueError( "See Engineering Reference for calculation procedure for visible solar transmittance." );
			}
			if ( Material( MaterNum ).TransThermal + Material( MaterNum ).AbsorpThermal >= 1.0 ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value combination." );
				ShowSevereError( "Thermal hemispherical emissivity plus open area fraction (1-diameter/spacing)**2 not < 1.0" );
			}

		} // TotScreensEQL loop

		// Window Blind Materials

		if ( TotBlinds > 0 ) {
			Blind.allocate( TotBlinds ); // Allocate the array Size to the number of blinds
		}

		CurrentModuleObject = "WindowMaterial:Blind";
		for ( Loop = 1; Loop <= TotBlinds; ++Loop ) {

			//Call Input Get routine to retrieve material data
			GetObjectItem( CurrentModuleObject, Loop, MaterialNames, MaterialNumAlpha, MaterialProps, MaterialNumProp, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			ErrorInName = false;
			IsBlank = false;
			VerifyName( MaterialNames( 1 ), Material, MaterNum, ErrorInName, IsBlank, CurrentModuleObject + " Name" );
			if ( ErrorInName ) {
				ShowContinueError( "...All Material names must be unique regardless of subtype." );
				ErrorsFound = true;
				continue;
			}

			++MaterNum;
			Material( MaterNum ).Group = WindowBlind;

			//Load the material derived type from the input data.

			Material( MaterNum ).Name = MaterialNames( 1 );
			Blind( Loop ).Name = MaterialNames( 1 );
			Material( MaterNum ).Roughness = Rough;
			Material( MaterNum ).BlindDataPtr = Loop;
			Material( MaterNum ).ROnly = true;

			Blind( Loop ).MaterialNumber = MaterNum;
			if ( SameString( MaterialNames( 2 ), "Horizontal" ) ) {
				Blind( Loop ).SlatOrientation = Horizontal;
			} else if ( SameString( MaterialNames( 2 ), "Vertical" ) ) {
				Blind( Loop ).SlatOrientation = Vertical;
			}
			Blind( Loop ).SlatWidth = MaterialProps( 1 );
			Blind( Loop ).SlatSeparation = MaterialProps( 2 );
			Blind( Loop ).SlatThickness = MaterialProps( 3 );
			Blind( Loop ).SlatAngle = MaterialProps( 4 );
			Blind( Loop ).SlatConductivity = MaterialProps( 5 );
			Blind( Loop ).SlatTransSolBeamDiff = MaterialProps( 6 );
			Blind( Loop ).SlatFrontReflSolBeamDiff = MaterialProps( 7 );
			Blind( Loop ).SlatBackReflSolBeamDiff = MaterialProps( 8 );
			Blind( Loop ).SlatTransSolDiffDiff = MaterialProps( 9 );
			Blind( Loop ).SlatFrontReflSolDiffDiff = MaterialProps( 10 );
			Blind( Loop ).SlatBackReflSolDiffDiff = MaterialProps( 11 );
			Blind( Loop ).SlatTransVisBeamDiff = MaterialProps( 12 );
			Blind( Loop ).SlatFrontReflVisBeamDiff = MaterialProps( 13 );
			Blind( Loop ).SlatBackReflVisBeamDiff = MaterialProps( 14 );
			Blind( Loop ).SlatTransVisDiffDiff = MaterialProps( 15 );
			Blind( Loop ).SlatFrontReflVisDiffDiff = MaterialProps( 16 );
			Blind( Loop ).SlatBackReflVisDiffDiff = MaterialProps( 17 );
			Blind( Loop ).SlatTransIR = MaterialProps( 18 );
			Blind( Loop ).SlatFrontEmissIR = MaterialProps( 19 );
			Blind( Loop ).SlatBackEmissIR = MaterialProps( 20 );
			Blind( Loop ).BlindToGlassDist = MaterialProps( 21 );
			Blind( Loop ).BlindTopOpeningMult = MaterialProps( 22 );
			Blind( Loop ).BlindBottomOpeningMult = MaterialProps( 23 );
			Blind( Loop ).BlindLeftOpeningMult = MaterialProps( 24 );
			Blind( Loop ).BlindRightOpeningMult = MaterialProps( 25 );
			Blind( Loop ).MinSlatAngle = MaterialProps( 26 );
			Blind( Loop ).MaxSlatAngle = MaterialProps( 27 );

			// TH 2/11/2010. For CR 8010
			// By default all blinds have fixed slat angle, new blinds with variable slat angle are created if
			//  they are used with window shading controls that adjust slat angles like ScheduledSlatAngle or BlockBeamSolar
			Blind( Loop ).SlatAngleType = FixedSlats;

			if ( Blind( Loop ).SlatWidth < Blind( Loop ).SlatSeparation ) {
				ShowWarningError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Slat Angles/Widths" );
				ShowContinueError( cNumericFieldNames( 1 ) + " [" + RoundSigDigits( Blind( Loop ).SlatWidth, 2 ) + "] is less than " + cNumericFieldNames( 2 ) + " [" + RoundSigDigits( Blind( Loop ).SlatSeparation, 2 ) + "]." );
				ShowContinueError( "This will allow direct beam to be transmitted when Slat angle = 0." );
			}

			if ( ! SameString( MaterialNames( 2 ), "Horizontal" ) && ! SameString( MaterialNames( 2 ), "Vertical" ) ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value" );
				ShowContinueError( cAlphaFieldNames( 2 ) + "=\"" + MaterialNames( 2 ) + "\", must be Horizontal or Vertical." );
			}

			if ( ( MaterialProps( 6 ) + MaterialProps( 7 ) >= 1.0 ) ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value combination." );
				ShowContinueError( cNumericFieldNames( 6 ) + " + " + cNumericFieldNames( 7 ) + " not < 1.0" );
			}
			if ( ( MaterialProps( 6 ) + MaterialProps( 8 ) >= 1.0 ) ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value combination." );
				ShowContinueError( cNumericFieldNames( 6 ) + " + " + cNumericFieldNames( 8 ) + " not < 1.0" );
			}

			if ( ( MaterialProps( 9 ) + MaterialProps( 10 ) >= 1.0 ) ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value combination." );
				ShowContinueError( cNumericFieldNames( 9 ) + " + " + cNumericFieldNames( 10 ) + " not < 1.0" );
			}
			if ( ( MaterialProps( 9 ) + MaterialProps( 11 ) >= 1.0 ) ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value combination." );
				ShowContinueError( cNumericFieldNames( 9 ) + " + " + cNumericFieldNames( 11 ) + " not < 1.0" );
			}

			if ( ( MaterialProps( 12 ) + MaterialProps( 13 ) >= 1.0 ) || ( MaterialProps( 12 ) + MaterialProps( 14 ) >= 1.0 ) ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value combination." );
				ShowContinueError( cNumericFieldNames( 12 ) + " + " + cNumericFieldNames( 13 ) + " not < 1.0 OR" );
				ShowContinueError( cNumericFieldNames( 12 ) + " + " + cNumericFieldNames( 14 ) + " not < 1.0" );
			}

			if ( ( MaterialProps( 12 ) + MaterialProps( 13 ) >= 1.0 ) ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value combination." );
				ShowContinueError( cNumericFieldNames( 12 ) + " + " + cNumericFieldNames( 13 ) + " not < 1.0" );
			}
			if ( ( MaterialProps( 12 ) + MaterialProps( 14 ) >= 1.0 ) ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value combination." );
				ShowContinueError( cNumericFieldNames( 12 ) + " + " + cNumericFieldNames( 14 ) + " not < 1.0" );
			}

			if ( ( MaterialProps( 15 ) + MaterialProps( 16 ) >= 1.0 ) ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value combination." );
				ShowContinueError( cNumericFieldNames( 15 ) + " + " + cNumericFieldNames( 16 ) + " not < 1.0" );
			}
			if ( ( MaterialProps( 15 ) + MaterialProps( 17 ) >= 1.0 ) ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value combination." );
				ShowContinueError( cNumericFieldNames( 15 ) + " + " + cNumericFieldNames( 17 ) + " not < 1.0" );
			}

			// Require that beam and diffuse properties be the same
			if ( std::abs( MaterialProps( 9 ) - MaterialProps( 6 ) ) > 1.e-5 ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value combination." );
				ShowContinueError( cNumericFieldNames( 6 ) + " must equal " + cNumericFieldNames( 9 ) );
			}

			if ( std::abs( MaterialProps( 10 ) - MaterialProps( 7 ) ) > 1.e-5 ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value combination." );
				ShowContinueError( cNumericFieldNames( 7 ) + " must equal " + cNumericFieldNames( 10 ) );
			}

			if ( std::abs( MaterialProps( 11 ) - MaterialProps( 8 ) ) > 1.e-5 ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value combination." );
				ShowContinueError( cNumericFieldNames( 8 ) + " must equal " + cNumericFieldNames( 11 ) );
			}

			if ( std::abs( MaterialProps( 15 ) - MaterialProps( 12 ) ) > 1.e-5 ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value combination." );
				ShowContinueError( cNumericFieldNames( 12 ) + " must equal " + cNumericFieldNames( 15 ) );
			}

			if ( std::abs( MaterialProps( 16 ) - MaterialProps( 13 ) ) > 1.e-5 ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value combination." );
				ShowContinueError( cNumericFieldNames( 13 ) + " must equal " + cNumericFieldNames( 16 ) );
			}

			if ( std::abs( MaterialProps( 17 ) - MaterialProps( 14 ) ) > 1.e-5 ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value combination." );
				ShowContinueError( cNumericFieldNames( 14 ) + " must equal " + cNumericFieldNames( 17 ) );
			}

			if ( ( MaterialProps( 18 ) + MaterialProps( 19 ) >= 1.0 ) ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value combination." );
				ShowContinueError( cNumericFieldNames( 18 ) + " + " + cNumericFieldNames( 19 ) + " not < 1.0" );
			}
			if ( ( MaterialProps( 18 ) + MaterialProps( 20 ) >= 1.0 ) ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value combination." );
				ShowContinueError( cNumericFieldNames( 18 ) + " + " + cNumericFieldNames( 20 ) + " not < 1.0" );
			}

			if ( Blind( Loop ).BlindToGlassDist < 0.5 * Blind( Loop ).SlatWidth ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value combination." );
				ShowContinueError( cNumericFieldNames( 21 ) + " is less than half of the " + cNumericFieldNames( 1 ) );
			}

			// Minimum and maximum slat angles allowed by slat geometry
			if ( Blind( Loop ).SlatWidth > Blind( Loop ).SlatSeparation ) {
				MinSlatAngGeom = std::asin( Blind( Loop ).SlatThickness / ( Blind( Loop ).SlatThickness + Blind( Loop ).SlatSeparation ) ) / DegToRadians;
			} else {
				MinSlatAngGeom = 0.0;
			}
			MaxSlatAngGeom = 180.0 - MinSlatAngGeom;

			// Error if input slat angle not in range allowed by slat geometry
			if ( ( Blind( Loop ).SlatSeparation + Blind( Loop ).SlatThickness ) < Blind( Loop ).SlatWidth ) {
				if ( Blind( Loop ).SlatAngle < MinSlatAngGeom ) {
					ErrorsFound = true;
					ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value combination." );
					ShowContinueError( cNumericFieldNames( 4 ) + "=[" + RoundSigDigits( Blind( Loop ).SlatAngle, 1 ) + "], is less than smallest allowed by slat dimensions and spacing, [" + RoundSigDigits( MinSlatAngGeom, 1 ) + "] deg." );
				} else if ( Blind( Loop ).SlatAngle > MaxSlatAngGeom ) {
					ErrorsFound = true;
					ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value combination." );
					ShowContinueError( cNumericFieldNames( 4 ) + "=[" + RoundSigDigits( Blind( Loop ).SlatAngle, 1 ) + "], is greater than largest allowed by slat dimensions and spacing, [" + RoundSigDigits( MinSlatAngGeom, 1 ) + "] deg." );
				}
			}

			// By default all Blinds are "fixed" slats.  Only with Shading Control is one considered variable and this check
			// is now done when that happens.  9.3.2009 LKL

			//    IF(Blind(Loop)%SlatAngleType == VariableSlats) THEN
			//      ! Error if maximum slat angle less than minimum
			//      IF(Blind(Loop)%MaxSlatAngle < Blind(Loop)%MinSlatAngle) THEN
			//        ErrorsFound = .TRUE.
			//        CALL ShowSevereError(TRIM(CurrentModuleObject)//'="'//TRIM(MaterialNames(1))//'", Illegal value combination.')
			//        CALL ShowContinueError(TRIM(cNumericFieldNames(26))//'=['//TRIM(RoundSigDigits(Blind(Loop)%MinSlatAngle,1))//  &
			//           '], is greater than '//TRIM(cNumericFieldNames(27))//'=['//  &
			//           TRIM(RoundSigDigits(Blind(Loop)%MaxSlatAngle,1))//'] deg.')
			//      END IF
			//      ! Error if input slat angle not in input min/max range
			//      IF(Blind(Loop)%MaxSlatAngle > Blind(Loop)%MinSlatAngle .AND. (Blind(Loop)%SlatAngle < Blind(Loop)%MinSlatAngle &
			//          .OR. Blind(Loop)%SlatAngle > Blind(Loop)%MaxSlatAngle)) THEN
			//        ErrorsFound = .TRUE.
			//        CALL ShowSevereError(TRIM(CurrentModuleObject)//'="'//TRIM(MaterialNames(1))//'", Illegal value combination.')
			//        CALL ShowContinueError(TRIM(cNumericFieldNames(4))//'=['//TRIM(RoundSigDigits(Blind(Loop)%SlatAngle,1))//  &
			//           '] is outside of the input min/max range, min=['//TRIM(RoundSigDigits(Blind(Loop)%MinSlatAngle,1))//  &
			//           '], max=['//TRIM(RoundSigDigits(Blind(Loop)%MaxSlatAngle,1))//'] deg.')
			//      END IF
			//      ! Error if input minimum slat angle is less than that allowed by slat geometry
			//      IF(Blind(Loop)%MinSlatAngle < MinSlatAngGeom) THEN
			//        CALL ShowSevereError(TRIM(CurrentModuleObject)//'="'//TRIM(MaterialNames(1))//'", Illegal value combination.')
			//        CALL ShowContinueError(TRIM(cNumericFieldNames(26))//'=['//TRIM(RoundSigDigits(Blind(Loop)%MinSlatAngle,1))//  &
			//           '] is less than the smallest allowed by slat dimensions and spacing, min=['//  &
			//           TRIM(RoundSigDigits(MinSlatAngGeom,1))//'] deg.')
			//        CALL ShowContinueError('Minimum Slat Angle will be set to '//TRIM(RoundSigDigits(MinSlatAngGeom,1))//' deg.')
			//        Blind(Loop)%MinSlatAngle = MinSlatAngGeom
			//      END IF
			//      ! Error if input maximum slat angle is greater than that allowed by slat geometry
			//      IF(Blind(Loop)%MaxSlatAngle > MaxSlatAngGeom) THEN
			//        CALL ShowWarningError(TRIM(CurrentModuleObject)//'="'//TRIM(MaterialNames(1))//'", Illegal value combination.')
			//        CALL ShowContinueError(TRIM(cNumericFieldNames(27))//'=['//TRIM(RoundSigDigits(Blind(Loop)%MaxSlatAngle,1))//  &
			//           '] is greater than the largest allowed by slat dimensions and spacing, ['//  &
			//           TRIM(RoundSigDigits(MaxSlatAngGeom,1))//'] deg.')
			//        CALL ShowContinueError('Maximum Slat Angle will be set to '//TRIM(RoundSigDigits(MaxSlatAngGeom,1))//' deg.')
			//        Blind(Loop)%MaxSlatAngle = MaxSlatAngGeom
			//      END IF
			//    END IF  ! End of check if slat angle is variable

		}

		// Window Blind Materials for EquivalentLayer Model

		CurrentModuleObject = "WindowMaterial:Blind:EquivalentLayer";
		for ( Loop = 1; Loop <= TotBlindsEQL; ++Loop ) {

			//Call Input Get routine to retrieve material data
			GetObjectItem( CurrentModuleObject, Loop, MaterialNames, MaterialNumAlpha, MaterialProps, MaterialNumProp, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			ErrorInName = false;
			IsBlank = false;
			VerifyName( MaterialNames( 1 ), Material, MaterNum, ErrorInName, IsBlank, CurrentModuleObject + " Name" );
			if ( ErrorInName ) {
				ShowContinueError( "...All Material names must be unique regardless of subtype." );
				ErrorsFound = true;
				continue;
			}

			++MaterNum;
			Material( MaterNum ).Group = BlindEquivalentLayer;

			Material( MaterNum ).Name = MaterialNames( 1 );
			Material( MaterNum ).Roughness = Rough;
			Material( MaterNum ).ROnly = true;

			if ( SameString( MaterialNames( 2 ), "Horizontal" ) ) {
				Material( MaterNum ).SlatOrientation = Horizontal;
			} else if ( SameString( MaterialNames( 2 ), "Vertical" ) ) {
				Material( MaterNum ).SlatOrientation = Vertical;
			}
			Material( MaterNum ).SlatWidth = MaterialProps( 1 );
			Material( MaterNum ).SlatSeparation = MaterialProps( 2 );
			Material( MaterNum ).SlatCrown = MaterialProps( 3 );
			Material( MaterNum ).SlatAngle = MaterialProps( 4 );

			Material( MaterNum ).TausFrontBeamDiff = MaterialProps( 5 );
			Material( MaterNum ).TausBackBeamDiff = MaterialProps( 6 );
			Material( MaterNum ).ReflFrontBeamDiff = MaterialProps( 7 );
			Material( MaterNum ).ReflBackBeamDiff = MaterialProps( 8 );

			if ( ! lNumericFieldBlanks( 9 ) && ! lNumericFieldBlanks( 10 ) && ! lNumericFieldBlanks( 11 ) && ! lNumericFieldBlanks( 12 ) ) {
				Material( MaterNum ).TausFrontBeamDiffVis = MaterialProps( 9 );
				Material( MaterNum ).TausBackBeamDiffVis = MaterialProps( 10 );
				Material( MaterNum ).ReflFrontBeamDiffVis = MaterialProps( 11 );
				Material( MaterNum ).ReflBackBeamDiffVis = MaterialProps( 12 );
			}
			if ( ! lNumericFieldBlanks( 13 ) && ! lNumericFieldBlanks( 14 ) && ! lNumericFieldBlanks( 15 ) ) {
				Material( MaterNum ).TausDiffDiff = MaterialProps( 13 );
				Material( MaterNum ).ReflFrontDiffDiff = MaterialProps( 14 );
				Material( MaterNum ).ReflBackDiffDiff = MaterialProps( 15 );
			}
			if ( ! lNumericFieldBlanks( 16 ) && ! lNumericFieldBlanks( 17 ) && ! lNumericFieldBlanks( 18 ) ) {
				Material( MaterNum ).TausDiffDiffVis = MaterialProps( 13 );
				Material( MaterNum ).ReflFrontDiffDiffVis = MaterialProps( 14 );
				Material( MaterNum ).ReflBackDiffDiffVis = MaterialProps( 15 );
			}
			if ( ! lNumericFieldBlanks( 19 ) && ! lNumericFieldBlanks( 20 ) && ! lNumericFieldBlanks( 21 ) ) {
				Material( MaterNum ).TausThermal = MaterialProps( 19 );
				Material( MaterNum ).EmissThermalFront = MaterialProps( 20 );
				Material( MaterNum ).EmissThermalBack = MaterialProps( 21 );
			}
			// Assumes thermal emissivity is the same as thermal absorptance
			Material( MaterNum ).AbsorpThermalFront = Material( MaterNum ).EmissThermalFront;
			Material( MaterNum ).AbsorpThermalBack = Material( MaterNum ).EmissThermalBack;
			Material( MaterNum ).TransThermal = Material( MaterNum ).TausThermal;

			// By default all blinds have fixed slat angle,
			//  they are used with window shading controls that adjust slat angles like MaximizeSolar or BlockBeamSolar
			if ( ! lAlphaFieldBlanks( 3 ) ) {
				if ( SameString( MaterialNames( 3 ), "FixedSlatAngle" ) ) {
					Material( MaterNum ).SlatAngleType = 0;
				} else if ( SameString( MaterialNames( 3 ), "MaximizeSolar" ) ) {
					Material( MaterNum ).SlatAngleType = 1;
				} else if ( SameString( MaterialNames( 3 ), "BlockBeamSolar" ) ) {
					Material( MaterNum ).SlatAngleType = 2;
				} else {
					Material( MaterNum ).SlatAngleType = 0;
				}
			} else {
				Material( MaterNum ).SlatAngleType = 0;
			}
			if ( Material( MaterNum ).SlatWidth < Material( MaterNum ).SlatSeparation ) {
				ShowWarningError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Slat Seperation/Width" );
				ShowContinueError( cNumericFieldNames( 1 ) + " [" + RoundSigDigits( Material( MaterNum ).SlatWidth, 2 ) + "] is less than " + cNumericFieldNames( 2 ) + " [" + RoundSigDigits( Material( MaterNum ).SlatSeparation, 2 ) + "]." );
				ShowContinueError( "This will allow direct beam to be transmitted when Slat angle = 0." );
			}
			if ( Material( MaterNum ).SlatSeparation < 0.001 ) {
				ShowWarningError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Slat Seperation" );
				ShowContinueError( cNumericFieldNames( 2 ) + " [" + RoundSigDigits( Material( MaterNum ).SlatSeparation, 2 ) + "]. Slate spacing must be > 0.0" );
				ShowContinueError( "...Setting slate spacing to default value of 0.025 m and simulation continues." );
				Material( MaterNum ).SlatSeparation = 0.025;
			}
			if ( Material( MaterNum ).SlatWidth < 0.001 || Material( MaterNum ).SlatWidth >= 2.0 * Material( MaterNum ).SlatSeparation ) {
				ShowWarningError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Slat Width" );
				ShowContinueError( cNumericFieldNames( 1 ) + " [" + RoundSigDigits( Material( MaterNum ).SlatWidth, 2 ) + "]. Slat width range is 0 < Width <= 2*Spacing" );
				ShowContinueError( "...Setting slate width equal to slate spacing and simulation continues." );
				Material( MaterNum ).SlatWidth = Material( MaterNum ).SlatSeparation;
			}
			if ( Material( MaterNum ).SlatCrown < 0.0 || Material( MaterNum ).SlatCrown >= 0.5 * Material( MaterNum ).SlatWidth ) {
				ShowWarningError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Slat Crown" );
				ShowContinueError( cNumericFieldNames( 3 ) + " [" + RoundSigDigits( Material( MaterNum ).SlatCrown, 2 ) + "]. Slat crwon range is 0 <= crown < 0.5*Width" );
				ShowContinueError( "...Setting slate crown to 0.0 and simulation continues." );
				Material( MaterNum ).SlatCrown = 0.0;
			}
			if ( Material( MaterNum ).SlatAngle < -90.0 || Material( MaterNum ).SlatAngle > 90.0 ) {
				ShowWarningError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Slat Angle" );
				ShowContinueError( cNumericFieldNames( 4 ) + " [" + RoundSigDigits( Material( MaterNum ).SlatAngle, 2 ) + "]. Slat angle range is -90.0 <= Angle < 90.0" );
				ShowContinueError( "...Setting slate angle to 0.0 and simulation continues." );
				Material( MaterNum ).SlatAngle = 0.0;
			}

			if ( ! SameString( MaterialNames( 2 ), "Horizontal" ) && ! SameString( MaterialNames( 2 ), "Vertical" ) ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value" );
				ShowContinueError( cAlphaFieldNames( 2 ) + "=\"" + MaterialNames( 2 ) + "\", must be Horizontal or Vertical." );
			}

			if ( ( MaterialProps( 5 ) + MaterialProps( 7 ) >= 1.0 ) ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value combination." );
				ShowContinueError( cNumericFieldNames( 5 ) + " + " + cNumericFieldNames( 7 ) + " not < 1.0" );
			}
			if ( ( MaterialProps( 6 ) + MaterialProps( 8 ) >= 1.0 ) ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value combination." );
				ShowContinueError( cNumericFieldNames( 6 ) + " + " + cNumericFieldNames( 8 ) + " not < 1.0" );
			}
			if ( ( MaterialProps( 9 ) + MaterialProps( 11 ) >= 1.0 ) ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value combination." );
				ShowContinueError( cNumericFieldNames( 9 ) + " + " + cNumericFieldNames( 11 ) + " not < 1.0" );
			}
			if ( ( MaterialProps( 10 ) + MaterialProps( 12 ) >= 1.0 ) ) {
				ErrorsFound = true;
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value combination." );
				ShowContinueError( cNumericFieldNames( 10 ) + " + " + cNumericFieldNames( 12 ) + " not < 1.0" );
			}

		} // TotBlindsEQL loop

		// EcoRoof Materials
		//PSU 2006
		CurrentModuleObject = "Material:RoofVegetation";
		for ( Loop = 1; Loop <= EcoRoofMat; ++Loop ) {
			//Call Input Get Routine to retrieve material data from ecoroof

			GetObjectItem( CurrentModuleObject, Loop, MaterialNames, MaterialNumAlpha, MaterialProps, MaterialNumProp, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			ErrorInName = false;
			IsBlank = false;
			VerifyName( MaterialNames( 1 ), Material, MaterNum, ErrorInName, IsBlank, CurrentModuleObject + " Name" );
			if ( ErrorInName ) {
				ShowContinueError( "...All Material names must be unique regardless of subtype." );
				ErrorsFound = true;
				continue;
			}

			//this part is similar to the regular material
			//Load the material derived type from the input data.
			++MaterNum;
			Material( MaterNum ).Group = EcoRoof;

			//this part is new for Ecoroof properties,
			//especially for the Plant Layer of the ecoroof
			Material( MaterNum ).HeightOfPlants = MaterialProps( 1 );
			Material( MaterNum ).LAI = MaterialProps( 2 );
			Material( MaterNum ).Lreflectivity = MaterialProps( 3 ); // Albedo
			Material( MaterNum ).LEmissitivity = MaterialProps( 4 );
			Material( MaterNum ).RStomata = MaterialProps( 5 );

			Material( MaterNum ).Name = MaterialNames( 1 );
			//need to treat the A2 with is just the name of the soil(it is
			// not important)
			ValidateMaterialRoughness( MaterNum, MaterialNames( 3 ), ErrorsFound );
			if ( SameString( MaterialNames( 4 ), "Simple" ) ) {
				Material( MaterNum ).EcoRoofCalculationMethod = 1;
			} else if ( SameString( MaterialNames( 4 ), "Advanced" ) || lAlphaFieldBlanks( 4 ) ) {
				Material( MaterNum ).EcoRoofCalculationMethod = 2;
			} else {
				ShowSevereError( CurrentModuleObject + "=\"" + MaterialNames( 1 ) + "\", Illegal value" );
				ShowContinueError( cAlphaFieldNames( 4 ) + "=\"" + MaterialNames( 4 ) + "\"." );
				ShowContinueError( "...Valid values are \"Simple\" or \"Advanced\"." );
				ErrorsFound = true;
			}

			Material( MaterNum ).Thickness = MaterialProps( 6 );
			Material( MaterNum ).Conductivity = MaterialProps( 7 );
			Material( MaterNum ).Density = MaterialProps( 8 );
			Material( MaterNum ).SpecHeat = MaterialProps( 9 );
			Material( MaterNum ).AbsorpThermal = MaterialProps( 10 ); // emissivity
			Material( MaterNum ).AbsorpSolar = MaterialProps( 11 ); // (1 - Albedo)
			Material( MaterNum ).AbsorpVisible = MaterialProps( 12 );
			Material( MaterNum ).Porosity = MaterialProps( 13 );
			Material( MaterNum ).MinMoisture = MaterialProps( 14 );
			Material( MaterNum ).InitMoisture = MaterialProps( 15 );

			if ( Material( MaterNum ).Conductivity > 0.0 ) {
				NominalR( MaterNum ) = Material( MaterNum ).Thickness / Material( MaterNum ).Conductivity;
				Material( MaterNum ).Resistance = NominalR( MaterNum );
			} else {
				ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" is not defined correctly." );
				ShowContinueError( cNumericFieldNames( 7 ) + " is <=0." );
				ErrorsFound = true;
			}

		}

		// Thermochromic glazing group
		// get the number of WindowMaterial:GlazingGroup:Thermochromic objects in the idf file
		CurrentModuleObject = "WindowMaterial:GlazingGroup:Thermochromic";
		TotTCGlazings = GetNumObjectsFound( CurrentModuleObject );
		if ( TotTCGlazings >= 1 ) {
			// Read TC glazings
			TCGlazings.allocate( TotTCGlazings );

			for ( Loop = 1; Loop <= TotTCGlazings; ++Loop ) {
				//Get each TCGlazings from the input processor
				GetObjectItem( CurrentModuleObject, Loop, cAlphaArgs, MaterialNumAlpha, rNumericArgs, MaterialNumProp, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

				ErrorInName = false;
				IsBlank = false;

				// Verify unique names
				VerifyName( cAlphaArgs( 1 ), TCGlazings, Loop - 1, ErrorInName, IsBlank, CurrentModuleObject + " Name" );
				if ( ErrorInName ) {
					ShowContinueError( "...All Thermochromic Glazing names must be unique regardless of subtype." );
					ErrorsFound = true;
					continue;
				}

				if ( MaterialNumProp + 1 != MaterialNumAlpha ) {
					ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" is not defined correctly." );
					ShowContinueError( "Check number of " + cAlphaFieldNames( 2 ) + " compared to number of " + cNumericFieldNames( 1 ) );
					ErrorsFound = true;
					continue;
				}

				//Allocate arrays
				TCGlazings( Loop ).SpecTemp.allocate( MaterialNumProp );
				TCGlazings( Loop ).LayerName.allocate( MaterialNumProp );
				TCGlazings( Loop ).LayerPoint.allocate( MaterialNumProp );
				TCGlazings( Loop ).SpecTemp = 0.0;
				TCGlazings( Loop ).LayerName = "";
				TCGlazings( Loop ).LayerPoint = 0;

				TCGlazings( Loop ).Name = cAlphaArgs( 1 );
				TCGlazings( Loop ).NumGlzMat = MaterialNumProp;

				for ( iTC = 1; iTC <= MaterialNumProp; ++iTC ) {
					TCGlazings( Loop ).SpecTemp( iTC ) = rNumericArgs( iTC );
					TCGlazings( Loop ).LayerName( iTC ) = cAlphaArgs( 1 + iTC );

					// Find this glazing material in the material list
					iMat = FindItemInList( cAlphaArgs( 1 + iTC ), Material );
					if ( iMat != 0 ) {
						//TC glazing
						Material( iMat ).SpecTemp = rNumericArgs( iTC );
						Material( iMat ).TCParent = Loop;
						TCGlazings( Loop ).LayerPoint( iTC ) = iMat;

						//test that named material is of the right type
						if ( Material( iMat ).Group != WindowGlass ) {
							ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" is not defined correctly." );
							ShowContinueError( "Material named: " + cAlphaArgs( 1 + iTC ) + " is not a window glazing " );
							ErrorsFound = true;
						}

					} else { // thow error because not found
						ShowSevereError( CurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" is not defined correctly." );
						ShowContinueError( "Material named: " + cAlphaArgs( 1 + iTC ) + " was not found " );
						ErrorsFound = true;
					}
				}
			}
		}

		cCurrentModuleObject = "WindowMaterial:SimpleGlazingSystem";
		for ( Loop = 1; Loop <= TotSimpleWindow; ++Loop ) {

			GetObjectItem( cCurrentModuleObject, Loop, cAlphaArgs, MaterialNumAlpha, rNumericArgs, MaterialNumProp, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			ErrorInName = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), Material, MaterNum, ErrorInName, IsBlank, cCurrentModuleObject + " Name" );
			if ( ErrorInName ) {
				ShowContinueError( "...All Material names must be unique regardless of subtype." );
				ErrorsFound = true;
				continue;
			}
			++MaterNum;
			Material( MaterNum ).Group = WindowSimpleGlazing;
			Material( MaterNum ).Name = cAlphaArgs( 1 );
			Material( MaterNum ).SimpleWindowUfactor = rNumericArgs( 1 );
			Material( MaterNum ).SimpleWindowSHGC = rNumericArgs( 2 );
			if ( ! lNumericFieldBlanks( 3 ) ) {
				Material( MaterNum ).SimpleWindowVisTran = rNumericArgs( 3 );
				Material( MaterNum ).SimpleWindowVTinputByUser = true;
			}

			SetupSimpleWindowGlazingSystem( MaterNum );

		}

		//Simon: Place to load materials for complex fenestrations
		if ( ( TotComplexShades > 0 ) || ( TotComplexGaps > 0 ) ) {
			SetupComplexFenestrationMaterialInput( MaterNum, ErrorsFound );
			if ( ErrorsFound ) {
				ShowSevereError( "Errors found in processing complex fenestration material input" );
			}
		}
		ScanForReports( "Constructions", DoReport, "Materials" );

		if ( DoReport ) {

			gio::write( OutputFileInits, fmtA ) << "! <Material Details>,Material Name,ThermalResistance {m2-K/w},Roughness,Thickness {m},Conductivity {w/m-K},Density {kg/m3},Specific Heat {J/kg-K},Absorptance:Thermal,Absorptance:Solar,Absorptance:Visible";

			gio::write( OutputFileInits, fmtA ) << "! <Material:Air>,Material Name,ThermalResistance {m2-K/w}";

			for ( MaterNum = 1; MaterNum <= TotMaterials; ++MaterNum ) {

				{ auto const SELECT_CASE_var( Material( MaterNum ).Group );
				if ( SELECT_CASE_var == Air ) {
					gio::write( OutputFileInits, Format_702 ) << Material( MaterNum ).Name << RoundSigDigits( Material( MaterNum ).Resistance, 4 );
				} else {
					gio::write( OutputFileInits, Format_701 ) << Material( MaterNum ).Name << RoundSigDigits( Material( MaterNum ).Resistance, 4 ) << DisplayMaterialRoughness( Material( MaterNum ).Roughness ) << RoundSigDigits( Material( MaterNum ).Thickness, 4 ) << RoundSigDigits( Material( MaterNum ).Conductivity, 3 ) << RoundSigDigits( Material( MaterNum ).Density, 3 ) << RoundSigDigits( Material( MaterNum ).SpecHeat, 3 ) << RoundSigDigits( Material( MaterNum ).AbsorpThermal, 4 ) << RoundSigDigits( Material( MaterNum ).AbsorpSolar, 4 ) << RoundSigDigits( Material( MaterNum ).AbsorpVisible, 4 );

				}}

			}

		}

		//  FORMATS.

		if ( AnyEnergyManagementSystemInModel ) { // setup surface property EMS actuators

			for ( MaterNum = 1; MaterNum <= TotMaterials; ++MaterNum ) {
				if ( Material( MaterNum ).Group != RegularMaterial ) continue;
				SetupEMSActuator( "Material", Material( MaterNum ).Name, "Surface Property Solar Absorptance", "[ ]", Material( MaterNum ).AbsorpSolarEMSOverrideOn, Material( MaterNum ).AbsorpSolarEMSOverride );
				SetupEMSActuator( "Material", Material( MaterNum ).Name, "Surface Property Thermal Absorptance", "[ ]", Material( MaterNum ).AbsorpThermalEMSOverrideOn, Material( MaterNum ).AbsorpThermalEMSOverride );
				SetupEMSActuator( "Material", Material( MaterNum ).Name, "Surface Property Visible Absorptance", "[ ]", Material( MaterNum ).AbsorpVisibleEMSOverrideOn, Material( MaterNum ).AbsorpVisibleEMSOverride );
			}
		}

	}

	void
	GetWindowGlassSpectralData( bool & ErrorsFound ) // set to true if errors found in input
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   May 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Gets spectral data (transmittance, front reflectance, and back
		// reflectance at normal incidence vs. wavelength) for glass

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::TrimSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetWindowGlassSpectralData: " );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int IOStat; // IO Status when calling get input subroutine
		Array1D_string SpecDataNames( 1 ); // Spectral data alpha names
		int SpecDataNumAlpha; // Number of spectral data alpha names being passed
		int SpecDataNumProp; // Number of spectral data properties being passed
		Array1D< Real64 > SpecDataProps; // Temporary array to transfer spectal data properties
		int Loop;
		bool ErrorInName;
		bool IsBlank;
		int LamNum; // Wavelength number
		int TotLam; // Total wavelengths
		Real64 Lam; // Wavelength (microns)
		Real64 Tau; // Transmittance, front reflectance, back reflectance
		Real64 RhoF;
		Real64 RhoB;

		CurrentModuleObject = "MaterialProperty:GlazingSpectralData";
		TotSpectralData = GetNumObjectsFound( CurrentModuleObject );
		SpectralData.allocate( TotSpectralData );
		if ( TotSpectralData > 0 ) SpecDataProps.allocate( MaxSpectralDataElements * 4 );

		for ( Loop = 1; Loop <= TotSpectralData; ++Loop ) {

			// Call Input Get routine to retrieve spectral data
			// Name is followed by up to 450 sets of normal-incidence measured values of
			// [wavelength (microns), transmittance, front reflectance, back reflectance] for
			// wavelengths covering the short-wave solar spectrum (from about 0.25 to 2.5 microns)
			GetObjectItem( CurrentModuleObject, Loop, SpecDataNames, SpecDataNumAlpha, SpecDataProps, SpecDataNumProp, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			ErrorInName = false;
			IsBlank = false;
			VerifyName( SpecDataNames( 1 ), SpectralData, Loop, ErrorInName, IsBlank, CurrentModuleObject + " Name" );
			if ( ErrorInName ) {
				ErrorsFound = true;
				continue;
			}

			//Load the spectral data derived type from the input data.
			SpectralData( Loop ).Name = SpecDataNames( 1 );
			TotLam = SpecDataNumProp / 4;
			if ( mod( SpecDataNumProp, 4 ) != 0 ) {
				ShowWarningError( RoutineName + CurrentModuleObject + "=\"" + SpecDataNames( 1 ) + "\" invalid set." );
				ShowContinueError( "... set not even multiple of 4 items (Wavelength,Trans,ReflFront,ReflBack), number of items in dataset = " + TrimSigDigits( SpecDataNumProp ) );
				ShowContinueError( "... remainder after div by 4 = " + TrimSigDigits( mod( SpecDataNumProp, 4 ) ) + ", remainder items will be set to 0.0" );
				SpecDataProps( {SpecDataNumProp + 1,min( SpecDataNumProp + 4, MaxSpectralDataElements * 4 )} ) = 0.0;
			}
			if ( TotLam > MaxSpectralDataElements ) {
				ErrorsFound = true;
				ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + SpecDataNames( 1 ) + "\" invalid set." );
				ShowContinueError( "... More than max [" + TrimSigDigits( MaxSpectralDataElements ) + "] (Wavelength,Trans,ReflFront,ReflBack) entries in set." );
				continue;
			}
			SpectralData( Loop ).NumOfWavelengths = TotLam;

			SpectralData( Loop ).WaveLength.allocate( TotLam ); // Wavelength (microns)
			SpectralData( Loop ).Trans.allocate( TotLam ); // Transmittance at normal incidence
			SpectralData( Loop ).ReflFront.allocate( TotLam ); // Front reflectance at normal incidence
			SpectralData( Loop ).ReflBack.allocate( TotLam ); // Back reflectance at normal incidence

			for ( LamNum = 1; LamNum <= TotLam; ++LamNum ) {
				SpectralData( Loop ).WaveLength( LamNum ) = SpecDataProps( 4 * LamNum - 3 );
				SpectralData( Loop ).Trans( LamNum ) = SpecDataProps( 4 * LamNum - 2 );
				// Following is needed since angular calculation in subr TransAndReflAtPhi
				// fails for Trans = 0.0
				if ( SpectralData( Loop ).Trans( LamNum ) < 0.001 ) SpectralData( Loop ).Trans( LamNum ) = 0.001;
				SpectralData( Loop ).ReflFront( LamNum ) = SpecDataProps( 4 * LamNum - 1 );
				SpectralData( Loop ).ReflBack( LamNum ) = SpecDataProps( 4 * LamNum );
			}

			// Check integrity of the spectral data
			for ( LamNum = 1; LamNum <= TotLam; ++LamNum ) {
				Lam = SpectralData( Loop ).WaveLength( LamNum );
				Tau = SpectralData( Loop ).Trans( LamNum );
				RhoF = SpectralData( Loop ).ReflFront( LamNum );
				RhoB = SpectralData( Loop ).ReflBack( LamNum );
				if ( LamNum < TotLam ) {
					if ( SpectralData( Loop ).WaveLength( LamNum + 1 ) <= Lam ) {
						ErrorsFound = true;
						ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + SpecDataNames( 1 ) + "\" invalid set." );
						ShowContinueError( "... Wavelengths not in increasing order. at wavelength#=" + TrimSigDigits( LamNum ) + ", value=[" + TrimSigDigits( Lam, 4 ) + "], next is [" + TrimSigDigits( SpectralData( Loop ).WaveLength( LamNum + 1 ), 4 ) + "]." );
					}
				}

				if ( Lam < 0.1 || Lam > 4.0 ) {
					ErrorsFound = true;
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + SpecDataNames( 1 ) + "\" invalid value." );
					ShowContinueError( "... A wavelength is not in the range 0.1 to 4.0 microns; at wavelength#=" + TrimSigDigits( LamNum ) + ", value=[" + TrimSigDigits( Lam, 4 ) + "]." );
				}

				// TH 2/15/2011. CR 8343
				// IGDB (International Glazing Database) does not meet the above strict restrictions.
				//  Relax rules to allow directly use of spectral data from IGDB
				if ( Tau > 1.01 ) {
					ErrorsFound = true;
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + SpecDataNames( 1 ) + "\" invalid value." );
					ShowContinueError( "... A transmittance is > 1.0; at wavelength#=" + TrimSigDigits( LamNum ) + ", value=[" + TrimSigDigits( Tau, 4 ) + "]." );
				}

				if ( RhoF < 0.0 || RhoF > 1.02 || RhoB < 0.0 || RhoB > 1.02 ) {
					ErrorsFound = true;
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + SpecDataNames( 1 ) + "\" invalid value." );
					ShowContinueError( "... A reflectance is < 0.0 or > 1.0; at wavelength#=" + TrimSigDigits( LamNum ) + ", RhoF value=[" + TrimSigDigits( RhoF, 4 ) + "]." );
					ShowContinueError( "... A reflectance is < 0.0 or > 1.0; at wavelength#=" + TrimSigDigits( LamNum ) + ", RhoB value=[" + TrimSigDigits( RhoB, 4 ) + "]." );
				}

				if ( ( Tau + RhoF ) > 1.03 || ( Tau + RhoB ) > 1.03 ) {
					ErrorsFound = true;
					ShowSevereError( RoutineName + CurrentModuleObject + "=\"" + SpecDataNames( 1 ) + "\" invalid value." );
					ShowContinueError( "... Transmittance + reflectance) > 1.0 for an entry; at wavelength#=" + TrimSigDigits( LamNum ) + ", value(Tau+RhoF)=[" + TrimSigDigits( ( Tau + RhoF ), 4 ) + "], value(Tau+RhoB)=[" + TrimSigDigits( ( Tau + RhoB ), 4 ) + "]." );
				}

			}

		}

		if ( TotSpectralData > 0 ) SpecDataProps.deallocate();

	}

	void
	ValidateMaterialRoughness(
		int const MaterNum, // Which Material number being validated.
		std::string const & Roughness, // Roughness String
		bool & ErrorsFound // If errors found
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   April 1999
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine compares the input Roughness value against the
		// valid values and sets the correct value in the Material Data Structure.

		// METHODOLOGY EMPLOYED:
		// Error message provided if not valid.

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

		//Select the correct Number for the associated ascii name for the roughness type
		if ( SameString( Roughness, "VeryRough" ) ) Material( MaterNum ).Roughness = VeryRough;
		if ( SameString( Roughness, "Rough" ) ) Material( MaterNum ).Roughness = Rough;
		if ( SameString( Roughness, "MediumRough" ) ) Material( MaterNum ).Roughness = MediumRough;
		if ( SameString( Roughness, "MediumSmooth" ) ) Material( MaterNum ).Roughness = MediumSmooth;
		if ( SameString( Roughness, "Smooth" ) ) Material( MaterNum ).Roughness = Smooth;
		if ( SameString( Roughness, "VerySmooth" ) ) Material( MaterNum ).Roughness = VerySmooth;

		// Was it set?
		if ( Material( MaterNum ).Roughness == 0 ) {
			ShowSevereError( "Material=" + Material( MaterNum ).Name + ",Illegal Roughness=" + Roughness );
			ErrorsFound = true;
		}

	}

	void
	GetConstructData( bool & ErrorsFound ) // If errors found in input
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Liesen
		//       DATE WRITTEN   September 1997
		//       MODIFIED       January 2003, FCW: accommodate between-glass shading device
		//                      July 2009, TH: added constructions defined with F and C factors
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This file reads the input through the input processor for Constructions.
		// Data read in this routine is stored in a derived type (Construct)
		// defined in the DataHeatBalance module.
		// This subroutine only sets those parameters which must be obtained
		// from the input file--all other portions of the Construct derived
		// type are set during the initializations.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataStringGlobals;
		using DataBSDFWindow::TotComplexFenStates;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int ConstrNum; // Counter to keep track of the construction number
		int Layer; // loop index for each of the construction layers
		int ConstructNumAlpha; // Number of construction alpha names being passed
		int DummyNumProp; // dummy variable for properties being passed
		int IOStat; // IO Status when calling get input subroutine
		Array1D_string ConstructAlphas( {0,MaxLayersInConstruct} ); // Construction Alpha names defined
		Array1D< Real64 > DummyProps( 4 ); // Temporary array to transfer construction properties
		bool ErrorInName;
		bool IsBlank;
		int Loop;
		int TotRegConstructs; // Number of "regular" constructions (no embedded sources or sinks and

		int TotFfactorConstructs; // Number of slabs-on-grade or underground floor constructions defined with F factors
		int TotCfactorConstructs; // Number of underground wall constructions defined with C factors

		int TotSourceConstructs; // Number of constructions with embedded sources or sinks
		int TotWindow5Constructs; // Number of constructions from Window5 data file
		bool ConstructionFound; // True if input window construction name is found in the
		//  Window5 data file
		bool EOFonW5File; // True if EOF encountered reading Window5 data file
		static bool NoRegularMaterialsUsed( true );
		int MaterialLayerGroup; // window contruction layer material group index

		int iMatGlass; // number of glass layers
		Array1D_string WConstructNames;

		// FLOW:

		//Get the Total number of Constructions from the input
		TotRegConstructs = GetNumObjectsFound( "Construction" );
		TotSourceConstructs = GetNumObjectsFound( "Construction:InternalSource" );

		TotFfactorConstructs = GetNumObjectsFound( "Construction:FfactorGroundFloor" );
		TotCfactorConstructs = GetNumObjectsFound( "Construction:CfactorUndergroundWall" );
		TotComplexFenStates = GetNumObjectsFound( "Construction:ComplexFenestrationState" );
		TotWindow5Constructs = GetNumObjectsFound( "Construction:WindowDataFile" );
		TotWinEquivLayerConstructs = GetNumObjectsFound( "Construction:WindowEquivalentLayer" );

		WConstructNames.allocate( TotWindow5Constructs );

		TotConstructs = TotRegConstructs + TotFfactorConstructs + TotCfactorConstructs + TotSourceConstructs + TotComplexFenStates + TotWinEquivLayerConstructs;

		NominalRforNominalUCalculation.dimension( TotConstructs, 0.0 );
		NominalU.dimension( TotConstructs, 0.0 );

		//Allocate the array to the number of constructions/initialize selected variables
		Construct.allocate( TotConstructs );
		//Note: If TotWindow5Constructs > 0, additional constructions are created in
		//subr. SearchWindow5DataFile corresponding to those found on the data file.
		for ( auto & e : Construct ) {
			// Initialize CTF and History terms
			e.NumCTFTerms = 0;
			e.NumHistories = 0;

			// Initialize some heat source/sink variables
			e.SourceSinkPresent = false; // "default" is no source or sink present
			e.SolutionDimensions = 1; // "default" is 1-D heat transfer
			e.SourceAfterLayer = 0; // this has no meaning if a source/sink is not present
			e.TempAfterLayer = 0; // this has no meaning if a source/sink is not present
			e.ThicknessPerpend = 0.0; // this has no meaning if a source/sink is not present

			e.W5FrameDivider = 0;
			e.FromWindow5DataFile = false;
		}


		ConstrNum = 0;

		CurrentModuleObject = "Construction";
		for ( Loop = 1; Loop <= TotRegConstructs; ++Loop ) { // Loop through all constructs in the input...

			//Get the object names for each construction from the input processor
			GetObjectItem( CurrentModuleObject, Loop, ConstructAlphas, ConstructNumAlpha, DummyProps, DummyNumProp, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			ErrorInName = false;
			IsBlank = false;
			VerifyName( ConstructAlphas( 0 ), Construct, ConstrNum, ErrorInName, IsBlank, CurrentModuleObject + " Name" );
			if ( ErrorInName ) {
				ErrorsFound = true;
				continue;
			}

			//Glass layer counter
			iMatGlass = 0;

			++ConstrNum;
			//Assign Construction name to the Derived Type using the zeroth position of the array
			Construct( ConstrNum ).Name = ConstructAlphas( 0 );

			//Set the total number of layers for the construction
			Construct( ConstrNum ).TotLayers = ConstructNumAlpha - 1;

			// Loop through all of the layers of the construct to match the material names.
			// The loop index is the number minus 1
			for ( Layer = 1; Layer <= ConstructNumAlpha - 1; ++Layer ) {

				//Find the material in the list of materials

				Construct( ConstrNum ).LayerPoint( Layer ) = FindItemInList( ConstructAlphas( Layer ), Material );

				// count number of glass layers
				if ( Construct( ConstrNum ).LayerPoint( Layer ) > 0 ) {
					if ( Material( Construct( ConstrNum ).LayerPoint( Layer ) ).Group == WindowGlass ) ++iMatGlass;
					MaterialLayerGroup = Material( Construct( ConstrNum ).LayerPoint( Layer ) ).Group;
					if ( ( MaterialLayerGroup == GlassEquivalentLayer ) || ( MaterialLayerGroup == ShadeEquivalentLayer ) || ( MaterialLayerGroup == DrapeEquivalentLayer ) || ( MaterialLayerGroup == BlindEquivalentLayer ) || ( MaterialLayerGroup == ScreenEquivalentLayer ) || ( MaterialLayerGroup == GapEquivalentLayer ) ) {
						ShowSevereError( "Invalid material layer type in window " + CurrentModuleObject + " = " + Construct( ConstrNum ).Name );
						ShowSevereError( "Equivalent Layer material type = " + ConstructAlphas( Layer ) + " is allowed only in Construction:WindowEquivalentLayer window object." );
						ErrorsFound = true;
					}
				}

				if ( Construct( ConstrNum ).LayerPoint( Layer ) == 0 ) {
					//This may be a TC GlazingGroup
					Construct( ConstrNum ).LayerPoint( Layer ) = FindItemInList( ConstructAlphas( Layer ), TCGlazings );

					if ( Construct( ConstrNum ).LayerPoint( Layer ) > 0 ) {
						//reset layer pointer to the first glazing in the TC GlazingGroup
						Construct( ConstrNum ).LayerPoint( Layer ) = TCGlazings( Construct( ConstrNum ).LayerPoint( Layer ) ).LayerPoint( 1 );
						Construct( ConstrNum ).TCLayer = Construct( ConstrNum ).LayerPoint( Layer );
						if ( Material( Construct( ConstrNum ).LayerPoint( Layer ) ).Group == WindowGlass ) ++iMatGlass;
						Construct( ConstrNum ).TCFlag = 1;
						Construct( ConstrNum ).TCMasterConst = ConstrNum;
						Construct( ConstrNum ).TCGlassID = iMatGlass; // the TC glass layer ID
						Construct( ConstrNum ).TCLayerID = Layer;
						Construct( ConstrNum ).TypeIsWindow = true;
					}
				}

				if ( Construct( ConstrNum ).LayerPoint( Layer ) == 0 ) {
					ShowSevereError( "Did not find matching material for " + CurrentModuleObject + ' ' + Construct( ConstrNum ).Name + ", missing material = " + ConstructAlphas( Layer ) );
					ErrorsFound = true;
				} else {
					NominalRforNominalUCalculation( ConstrNum ) += NominalR( Construct( ConstrNum ).LayerPoint( Layer ) );
					if ( Material( Construct( ConstrNum ).LayerPoint( Layer ) ).Group == RegularMaterial && ! Material( Construct( ConstrNum ).LayerPoint( Layer ) ).ROnly ) {
						NoRegularMaterialsUsed = false;
					}
				}

			} // ...end of the Layer DO loop

		} // ...end of Regular Construction DO loop

		TotRegConstructs = ConstrNum;

		// Added TH 7/2009 for underground walls and floors constructions
		if ( TotFfactorConstructs + TotCfactorConstructs >= 1 ) {
			CreateFCfactorConstructions( ConstrNum, ErrorsFound );
			if ( ErrorsFound ) {
				ShowSevereError( "Errors found in creating the constructions defined with Ffactor or Cfactor method" );
			}
			TotRegConstructs += TotFfactorConstructs + TotCfactorConstructs;
		}

		// Added BG 6/2010 for complex fenestration
		if ( TotComplexFenStates > 0 ) {
			SetupComplexFenestrationStateInput( ConstrNum, ErrorsFound );
			if ( ErrorsFound ) {
				ShowSevereError( "Errors found in processing complex fenestration input" );
			}
			TotRegConstructs += TotComplexFenStates;
		}

		ConstrNum = 0;

		CurrentModuleObject = "Construction:InternalSource";
		for ( Loop = 1; Loop <= TotSourceConstructs; ++Loop ) { // Loop through all constructs with sources in the input...

			//Get the object names for each construction from the input processor
			GetObjectItem( CurrentModuleObject, Loop, ConstructAlphas, ConstructNumAlpha, DummyProps, DummyNumProp, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			ErrorInName = false;
			IsBlank = false;
			VerifyName( ConstructAlphas( 0 ), Construct, TotRegConstructs + ConstrNum, ErrorInName, IsBlank, CurrentModuleObject + " Name" );
			if ( ErrorInName ) {
				ErrorsFound = true;
				continue;
			}

			++ConstrNum;
			//Assign Construction name to the Derived Type using the zeroth position of the array
			Construct( TotRegConstructs + ConstrNum ).Name = ConstructAlphas( 0 );

			// Obtain the source/sink data
			if ( DummyNumProp != 4 ) {
				ShowSevereError( CurrentModuleObject + ": Wrong number of numerical inputs for " + Construct( ConstrNum ).Name );
				ErrorsFound = true;
			}
			Construct( TotRegConstructs + ConstrNum ).SourceSinkPresent = true;
			Construct( TotRegConstructs + ConstrNum ).SourceAfterLayer = int( DummyProps( 1 ) );
			Construct( TotRegConstructs + ConstrNum ).TempAfterLayer = int( DummyProps( 2 ) );
			Construct( TotRegConstructs + ConstrNum ).SolutionDimensions = int( DummyProps( 3 ) );
			if ( ( Construct( TotRegConstructs + ConstrNum ).SolutionDimensions < 1 ) || ( Construct( TotRegConstructs + ConstrNum ).SolutionDimensions > 2 ) ) {
				ShowWarningError( "Construction:InternalSource must be either 1- or 2-D.  Reset to 1-D solution." );
				ShowContinueError( "Construction=" + Construct( TotRegConstructs + ConstrNum ).Name + " is affected." );
				Construct( TotRegConstructs + ConstrNum ).SolutionDimensions = 1;
			}
			Construct( TotRegConstructs + ConstrNum ).ThicknessPerpend = DummyProps( 4 ) / 2.0;

			//Set the total number of layers for the construction
			Construct( TotRegConstructs + ConstrNum ).TotLayers = ConstructNumAlpha - 1;
			if ( Construct( TotRegConstructs + ConstrNum ).TotLayers <= 1 ) {
				ShowSevereError( "Construction " + Construct( TotRegConstructs + ConstrNum ).Name + " has an internal source or sink and thus must have more than a single layer" );
				ErrorsFound = true;
			}
			if ( ( Construct( TotRegConstructs + ConstrNum ).SourceAfterLayer >= Construct( TotRegConstructs + ConstrNum ).TotLayers ) || ( Construct( TotRegConstructs + ConstrNum ).SourceAfterLayer <= 0 ) ) {
				ShowWarningError( "Construction " + Construct( TotRegConstructs + ConstrNum ).Name + " must have a source that is between two layers" );
				ShowContinueError( "The source after layer parameter has been set to one less than the number of layers." );
				Construct( TotRegConstructs + ConstrNum ).SourceAfterLayer = Construct( TotRegConstructs + ConstrNum ).TotLayers - 1;
			}
			if ( ( Construct( TotRegConstructs + ConstrNum ).TempAfterLayer >= Construct( TotRegConstructs + ConstrNum ).TotLayers ) || ( Construct( TotRegConstructs + ConstrNum ).TempAfterLayer <= 0 ) ) {
				ShowWarningError( "Construction " + Construct( TotRegConstructs + ConstrNum ).Name + " must have a temperature calculation that is between two layers" );
				ShowContinueError( "The temperature calculation after layer parameter has been set to one less than the number of layers." );
				Construct( TotRegConstructs + ConstrNum ).TempAfterLayer = Construct( TotRegConstructs + ConstrNum ).TotLayers - 1;
			}

			// Loop through all of the layers of the construct to match the material names.
			// The loop index is the number minus 1
			for ( Layer = 1; Layer <= ConstructNumAlpha - 1; ++Layer ) {

				//Find the material in the list of materials

				Construct( TotRegConstructs + ConstrNum ).LayerPoint( Layer ) = FindItemInList( ConstructAlphas( Layer ), Material );

				if ( Construct( TotRegConstructs + ConstrNum ).LayerPoint( Layer ) == 0 ) {
					ShowSevereError( "Did not find matching material for " + CurrentModuleObject + ' ' + Construct( ConstrNum ).Name + ", missing material = " + ConstructAlphas( Layer ) );
					ErrorsFound = true;
				} else {
					NominalRforNominalUCalculation( TotRegConstructs + ConstrNum ) += NominalR( Construct( TotRegConstructs + ConstrNum ).LayerPoint( Layer ) );
					if ( Material( Construct( TotRegConstructs + ConstrNum ).LayerPoint( Layer ) ).Group == RegularMaterial && ! Material( Construct( TotRegConstructs + ConstrNum ).LayerPoint( Layer ) ).ROnly ) {
						NoRegularMaterialsUsed = false;
					}
				}

			} // ...end of the Layer DO loop

		} // ...end of Source Construction DO loop

		TotSourceConstructs = ConstrNum;
		TotRegConstructs += TotSourceConstructs;
		TotConstructs = TotRegConstructs;

		if ( TotConstructs > 0 && NoRegularMaterialsUsed ) {
			ShowSevereError( "This building has no thermal mass which can cause an unstable solution." );
			ShowContinueError( "Use Material object for all opaque material definitions except very light insulation layers." );
		}

		ConstrNum = 0;
		CurrentModuleObject = "Construction:WindowEquivalentLayer";
		for ( Loop = 1; Loop <= TotWinEquivLayerConstructs; ++Loop ) { // Loop through all constructs with Window EquivalentLayer ...

			//Get the object names for each construction from the input processor
			GetObjectItem( CurrentModuleObject, Loop, ConstructAlphas, ConstructNumAlpha, DummyProps, DummyNumProp, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			ErrorInName = false;
			IsBlank = false;
			VerifyName( ConstructAlphas( 0 ), Construct, TotRegConstructs + ConstrNum, ErrorInName, IsBlank, CurrentModuleObject + " Name" );
			if ( ErrorInName ) {
				ErrorsFound = true;
				continue;
			}

			++ConstrNum;
			//Assign Construction name to the Derived Type using the zeroth position of the array
			Construct( TotRegConstructs + ConstrNum ).Name = ConstructAlphas( 0 );

			// Set the total number of layers for the construction
			Construct( TotRegConstructs + ConstrNum ).TotLayers = ConstructNumAlpha - 1;
			if ( Construct( TotRegConstructs + ConstrNum ).TotLayers < 1 ) {
				ShowSevereError( "Construction " + Construct( TotRegConstructs + ConstrNum ).Name + " must have at least a single layer" );
				ErrorsFound = true;
			}

			// Loop through all of the layers of the construct to match the material names.
			// The loop index is the number minus 1
			for ( Layer = 1; Layer <= ConstructNumAlpha - 1; ++Layer ) {

				//Find the material in the list of materials
				Construct( TotRegConstructs + ConstrNum ).LayerPoint( Layer ) = FindItemInList( ConstructAlphas( Layer ), Material );

				if ( Construct( TotRegConstructs + ConstrNum ).LayerPoint( Layer ) == 0 ) {
					ShowSevereError( "Did not find matching material for " + CurrentModuleObject + ' ' + Construct( ConstrNum ).Name + ", missing material = " + ConstructAlphas( Layer ) );
					ErrorsFound = true;
				} else {
					if ( ConstructNumAlpha <= 2 ) {

					} else {
						NominalRforNominalUCalculation( TotRegConstructs + ConstrNum ) += NominalR( Construct( TotRegConstructs + ConstrNum ).LayerPoint( Layer ) );
					}
				}

			} // Layer loop
			Construct( TotRegConstructs + ConstrNum ).EQLConsPtr = ConstrNum;
			Construct( TotRegConstructs + ConstrNum ).WindowTypeEQL = true;
		} // TotWinEquivLayerConstructs loop

		TotWinEquivLayerConstructs = ConstrNum;
		TotRegConstructs += TotWinEquivLayerConstructs;
		TotConstructs = TotRegConstructs;
		//-------------------------------------------------------------------------------
		ConstrNum = 0;

		CurrentModuleObject = "Construction:WindowDataFile";
		for ( Loop = 1; Loop <= TotWindow5Constructs; ++Loop ) { // Loop through all Window5 constructions. These constructions come
			// from the Window5 data file and can be referenced only by windows

			//Get the object names for each construction from the input processor
			GetObjectItem( CurrentModuleObject, Loop, ConstructAlphas, ConstructNumAlpha, DummyProps, DummyNumProp, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			ErrorInName = false;
			IsBlank = false;
			VerifyName( ConstructAlphas( 0 ), WConstructNames, ConstrNum, ErrorInName, IsBlank, CurrentModuleObject + " Name" );
			if ( ErrorInName && ! IsBlank ) {
				ShowContinueError( "...first instance will be used." );
				continue;
			}
			if ( IsBlank ) {
				ErrorsFound = true;
				continue;
			}

			++ConstrNum;
			WConstructNames( ConstrNum ) = ConstructAlphas( 0 );

			// Obtain the data
			if ( DummyNumProp != 0 ) {
				ShowSevereError( "Construction From Window5 Data File: there should be no numerical inputs for " + ConstructAlphas( 0 ) );
				ErrorsFound = true;
				continue;
			}

			// See if this construction is in the W5DataFile produced by the WINDOW 5 program;
			// if so, ConstructionFound will be set to true and the Material objects
			// associated with the construction will be created in subr. SearchWindow5DataFile.
			// (If the matching construction on the Window5 data file has two glazing systems, a
			// second construction and its associated materials will be created in subr.
			// SearchWindow5DataFile and TotConstructs WILL BE INCREMENTED BY 1 in that routine.
			// A FrameAndDivider object will also be created if window on data file has a
			// frame or divider.)

			std::string window5DataFileName;
			if ( ConstructAlphas( 1 ) == "" ) {
				window5DataFileName = CurrentWorkingFolder + "Window5DataFile.dat";
			} else {
				window5DataFileName = ConstructAlphas( 1 );
			}
			DisplayString( "Searching Window5 data file for Construction=" + ConstructAlphas( 0 ) );

			SearchWindow5DataFile( window5DataFileName, ConstructAlphas( 0 ), ConstructionFound, EOFonW5File, ErrorsFound );

			if ( EOFonW5File || ! ConstructionFound ) {
				DisplayString( "--Construction not found" );
				ErrorsFound = true;
				ShowSevereError( "No match on WINDOW5 data file for Construction=" + ConstructAlphas( 0 ) + ", or error in data file." );
				ShowContinueError( "...Looking on file=" + window5DataFileName );
				continue;
			}

		} // ...end of Window5 Constructions DO loop

		WConstructNames.deallocate();

		// set some (default) properties of the Construction Derived Type
		for ( ConstrNum = 1; ConstrNum <= TotConstructs; ++ConstrNum ) {

			if ( NominalRforNominalUCalculation( ConstrNum ) != 0.0 ) {
				NominalU( ConstrNum ) = 1.0 / NominalRforNominalUCalculation( ConstrNum );
			} else {
				if ( ! Construct( ConstrNum ).WindowTypeEQL ) {
					ShowSevereError( "Nominal U is zero, for construction=" + Construct( ConstrNum ).Name );
					ErrorsFound = true;
				}
			}

			CheckAndSetConstructionProperties( ConstrNum, ErrorsFound );

		} // End of ConstrNum DO loop

	}

	void
	GetBuildingData( bool & ErrorsFound ) // If errors found in input
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   November 1997
		//       MODIFIED       October 1998, FW; May 1999 FW; Oct 2004 LKL
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine calls other routines to get the Zone, and Surface data
		//  from the input file.

		// METHODOLOGY EMPLOYED:
		// The GetObjectItem routines are employed to retrieve the data.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace SurfaceGeometry;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		GetZoneData( ErrorsFound ); // Read Zone data from input file

		SetupZoneGeometry( ErrorsFound );

	}

	void
	GetZoneData( bool & ErrorsFound ) // If errors found in input
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   November 1997
		//       MODIFIED       PGE: Added ZONE LIST and ZONE GROUP objects, Nov 2003
		//                      RJH: Added init of DElight member of ZoneDaylight object, Jan 2004
		//                      JG: Added Part of Total Floor Area field March 2006
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine gets the zone data for each zone in the input file.

		// METHODOLOGY EMPLOYED:
		// The GetObjectItem routines are employed to retrieve the data.

		// REFERENCES:
		// IDD Definition for Zone object

		// Using/Aliasing
		using DataDaylighting::ZoneDaylight;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetZoneData: " );
		//  INTEGER, PARAMETER :: MaxZonesInList = 100 ! This is to allow DIMENSIONing below

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//  CHARACTER(len=MaxNameLength), DIMENSION(MaxZonesInList + 1) :: Alphas
		//  REAL(r64), DIMENSION(8)              :: Numbers
		int NumAlphas;
		int NumNumbers;
		int IOStatus;
		int ZoneLoop;
		std::string::size_type TMP;
		int Loop;
		int ListNum;
		int ZoneNum;
		std::string ZoneName;
		int GroupNum;
		bool ErrorInName;
		bool IsBlank;

		cCurrentModuleObject = "Zone";
		NumOfZones = GetNumObjectsFound( cCurrentModuleObject );

		Zone.allocate( NumOfZones );

		ZoneDaylight.allocate( NumOfZones );

		ZoneLoop = 0;

		for ( Loop = 1; Loop <= NumOfZones; ++Loop ) {

			rNumericArgs = 0.0; // Zero out just in case
			GetObjectItem( cCurrentModuleObject, Loop, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			TMP = index( cAlphaArgs( 1 ), char( 1 ) );
			while ( TMP != std::string::npos ) {
				cAlphaArgs( 1 )[ TMP ] = ',';
				TMP = index( cAlphaArgs( 1 ), char( 1 ) );
			}
			TMP = index( cAlphaArgs( 1 ), char( 2 ) );
			while ( TMP != std::string::npos ) {
				cAlphaArgs( 1 )[ TMP ] = '!';
				TMP = index( cAlphaArgs( 1 ), char( 2 ) );
			}

			//    Make sure Zone Name is unique
			ErrorInName = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), Zone, ZoneLoop, ErrorInName, IsBlank, cCurrentModuleObject + " Name" );
			if ( ErrorInName ) {
				ErrorsFound = true;
				continue;
			}

			++ZoneLoop;
			ProcessZoneData( cCurrentModuleObject, ZoneLoop, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames, ErrorsFound );

		} // Loop

		for ( Loop = 1; Loop <= NumOfZones; ++Loop ) {
			// Check to see if "nominally" controlled -- Zone Name appears in Zone Equip Configuration
			// relies on zone name being the "name" of the Zone Controlled Equip Configuration
			if ( GetObjectItemNum( "ZoneHVAC:EquipmentConnections", Zone( Loop ).Name ) > 0 ) {
				Zone( Loop ).isNominalControlled = true;
			} else {
				Zone( Loop ).isNominalControlled = false;
			}
		}

		// Get ZONE LIST objects
		cCurrentModuleObject = "ZoneList";
		NumOfZoneLists = GetNumObjectsFound( cCurrentModuleObject );

		if ( NumOfZoneLists > 0 ) {

			ZoneList.allocate( NumOfZoneLists );

			for ( ListNum = 1; ListNum <= NumOfZoneLists; ++ListNum ) {
				GetObjectItem( cCurrentModuleObject, ListNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

				// List name
				ErrorInName = false;
				IsBlank = false;
				VerifyName( cAlphaArgs( 1 ), ZoneList, ListNum - 1, ErrorInName, IsBlank, cCurrentModuleObject + " Name" );

				if ( ErrorInName ) {
					ErrorsFound = true;
				}

				ZoneList( ListNum ).Name = cAlphaArgs( 1 );
				if ( FindItemInList( ZoneList( ListNum ).Name, Zone ) > 0 ) {
					ShowWarningError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\":  is a duplicate of a zone name." );
					ShowContinueError( "This could be a problem in places where either a Zone Name or a Zone List can be used." );
				}

				// List of zones
				ZoneList( ListNum ).NumOfZones = NumAlphas - 1;

				if ( ZoneList( ListNum ).NumOfZones < 1 ) {
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\":  No zones specified." );
					ErrorsFound = true;
				} else {
					ZoneList( ListNum ).Zone.allocate( ZoneList( ListNum ).NumOfZones );
					ZoneList( ListNum ).Zone = 0;

					for ( ZoneNum = 1; ZoneNum <= ZoneList( ListNum ).NumOfZones; ++ZoneNum ) {
						ZoneName = cAlphaArgs( ZoneNum + 1 );
						ZoneList( ListNum ).MaxZoneNameLength = max( ZoneList( ListNum ).MaxZoneNameLength, len( ZoneName ) );
						ZoneList( ListNum ).Zone( ZoneNum ) = FindItemInList( ZoneName, Zone );
						if ( ZoneList( ListNum ).Zone( ZoneNum ) == 0 ) {
							ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\":  " + cAlphaFieldNames( ZoneNum + 1 ) + ' ' + ZoneName + " not found." );
							ErrorsFound = true;
						}

						// Check for duplicate zones
						for ( Loop = 1; Loop <= ZoneNum - 1; ++Loop ) {
							if ( ZoneList( ListNum ).Zone( ZoneNum ) == ZoneList( ListNum ).Zone( Loop ) ) {
								ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\":  " + cAlphaFieldNames( ZoneNum + 1 ) + ' ' + ZoneName + " appears more than once in list." );
								ErrorsFound = true;
							}
						} // Loop
					} // ZoneNum
				}

			} // ListNum
		}

		// Get ZONE GROUP objects
		cCurrentModuleObject = "ZoneGroup";
		NumOfZoneGroups = GetNumObjectsFound( cCurrentModuleObject );

		if ( NumOfZoneGroups > 0 ) {
			ZoneGroup.allocate( NumOfZoneGroups );

			for ( GroupNum = 1; GroupNum <= NumOfZoneGroups; ++GroupNum ) {
				GetObjectItem( cCurrentModuleObject, GroupNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

				// Group name
				ErrorInName = false;
				IsBlank = false;
				VerifyName( cAlphaArgs( 1 ), ZoneGroup, GroupNum - 1, ErrorInName, IsBlank, cCurrentModuleObject + " Name" );

				if ( ErrorInName ) {
					ErrorsFound = true;
				}

				ZoneGroup( GroupNum ).Name = cAlphaArgs( 1 );

				// Multiplier - checked already by IDD rules
				ZoneGroup( GroupNum ).Multiplier = rNumericArgs( 1 );

				// Zone list
				ListNum = FindItemInList( cAlphaArgs( 2 ), ZoneList );
				ZoneGroup( GroupNum ).ZoneList = ListNum;

				if ( ListNum == 0 ) {
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\":  " + cAlphaFieldNames( 2 ) + " named " + cAlphaArgs( 2 ) + " not found." );
					ErrorsFound = true;
				} else {
					// Check to make sure list is not in use by another ZONE GROUP
					for ( Loop = 1; Loop <= GroupNum - 1; ++Loop ) {
						if ( ZoneGroup( GroupNum ).ZoneList == ZoneGroup( Loop ).ZoneList ) {
							ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\":  " + cAlphaFieldNames( 2 ) + " already used by " + cCurrentModuleObject + " named " + ZoneGroup( Loop ).Name + '.' );
							ErrorsFound = true;
						}
					} // Loop

					// Set group multiplier for each zone in the list
					for ( Loop = 1; Loop <= ZoneList( ListNum ).NumOfZones; ++Loop ) {
						ZoneNum = ZoneList( ListNum ).Zone( Loop );

						if ( ZoneNum > 0 ) {
							// Check to make sure group multiplier was not already set by another ZONE GROUP
							if ( Zone( ZoneNum ).ListGroup == 0 ) {
								Zone( ZoneNum ).ListMultiplier = ZoneGroup( GroupNum ).Multiplier;
								Zone( ZoneNum ).ListGroup = ListNum;
							} else {
								ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\":  Zone " + Zone( ZoneNum ).Name + " in ZoneList already exists in ZoneList of another ZoneGroup." );
								ShowContinueError( "Previous ZoneList=" + ZoneList( Zone( ZoneNum ).ListGroup ).Name );
								ErrorsFound = true;
							}
						}
					} // Loop
				}

			} // GroupNum
		}

		//allocate the array the holds the predefined report data
		ZonePreDefRep.allocate( NumOfZones );

	}

	void
	ProcessZoneData(
		std::string const & cCurrentModuleObject,
		int const ZoneLoop,
		Array1_string const & cAlphaArgs,
		int & NumAlphas,
		Array1< Real64 > const & rNumericArgs,
		int & NumNumbers,
		Array1_bool const & EP_UNUSED( lNumericFieldBlanks ), //Unused
		Array1_bool const & lAlphaFieldBlanks,
		Array1_string const & cAlphaFieldNames,
		Array1_string const & EP_UNUSED( cNumericFieldNames ), //Unused
		bool & ErrorsFound // If errors found in input
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   November 1997
		//       MODIFIED       PGE: Added ZONE LIST and ZONE GROUP objects, Nov 2003
		//                      RJH: Added init of DElight member of ZoneDaylight object, Jan 2004
		//                      JG: Added Part of Total Floor Area field March 2006
		//       RE-ENGINEERED  MJW: Split out processing zone input to facilitate unit testing, Nov 2014

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine gets the zone data for each zone in the input file.

		// METHODOLOGY EMPLOYED:
		// The GetObjectItem routines are employed to retrieve the data.

		// REFERENCES:
		// IDD Definition for Zone object

		// Using/Aliasing
		using DataDaylighting::ZoneDaylight;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "ProcessZoneData: " );
		//  INTEGER, PARAMETER :: MaxZonesInList = 100 ! This is to allow DIMENSIONing below

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//  CHARACTER(len=MaxNameLength), DIMENSION(MaxZonesInList + 1) :: Alphas
		//  REAL(r64), DIMENSION(8)              :: Numbers

		Zone( ZoneLoop ).Name = cAlphaArgs( 1 );
		if ( NumNumbers >= 1 ) Zone( ZoneLoop ).RelNorth = rNumericArgs( 1 );
		if ( NumNumbers >= 2 ) Zone( ZoneLoop ).OriginX = rNumericArgs( 2 );
		if ( NumNumbers >= 3 ) Zone( ZoneLoop ).OriginY = rNumericArgs( 3 );
		if ( NumNumbers >= 4 ) Zone( ZoneLoop ).OriginZ = rNumericArgs( 4 );
		if ( NumNumbers >= 5 ) Zone( ZoneLoop ).OfType = rNumericArgs( 5 );
		Zone( ZoneLoop ).OfType = StandardZone;
		if ( NumNumbers >= 6 ) Zone( ZoneLoop ).Multiplier = rNumericArgs( 6 );
		if ( NumNumbers >= 7 ) Zone( ZoneLoop ).CeilingHeight = rNumericArgs( 7 );
		if ( NumNumbers >= 8 ) Zone( ZoneLoop ).Volume = rNumericArgs( 8 );
		if ( NumNumbers >= 9 ) Zone( ZoneLoop ).UserEnteredFloorArea = rNumericArgs( 9 );

		if ( NumAlphas > 1 && !lAlphaFieldBlanks( 2 ) ) {
				{ auto const SELECT_CASE_var( cAlphaArgs( 2 ) );

				if ( SELECT_CASE_var == "SIMPLE" ) {
					Zone( ZoneLoop ).InsideConvectionAlgo = ASHRAESimple;

				} else if ( ( SELECT_CASE_var == "TARP" ) || ( SELECT_CASE_var == "DETAILED" ) ) {
					if ( cAlphaArgs( 2 ) == "DETAILED" ) {
						ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + Zone( ZoneLoop ).Name + "\"." );
						ShowContinueError( "Deprecated value in " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\", defaulting to TARP." );
					}
					Zone( ZoneLoop ).InsideConvectionAlgo = ASHRAETARP;

				} else if ( SELECT_CASE_var == "CEILINGDIFFUSER" ) {
					Zone( ZoneLoop ).InsideConvectionAlgo = CeilingDiffuser;

				} else if ( SELECT_CASE_var == "TROMBEWALL" ) {
					Zone( ZoneLoop ).InsideConvectionAlgo = TrombeWall;

				} else if ( SELECT_CASE_var == "ADAPTIVECONVECTIONALGORITHM" ) {
					Zone( ZoneLoop ).InsideConvectionAlgo = AdaptiveConvectionAlgorithm;

				} else {
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + Zone( ZoneLoop ).Name + "\"." );
					ShowContinueError( "Invalid value for " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\"." );
					ErrorsFound = true;
					//Zone( ZoneLoop ).InsideConvectionAlgo = ASHRAETARP;

				}}
		} else {
			// No zone specific algorithm specified, use default Inside Convection Algorithm
			Zone( ZoneLoop ).InsideConvectionAlgo = DefaultInsideConvectionAlgo;

		}

		if ( NumAlphas > 2 && !lAlphaFieldBlanks( 3 ) ) {
				{ auto const SELECT_CASE_var( cAlphaArgs( 3 ) );

				if ( ( SELECT_CASE_var == "SIMPLECOMBINED" ) || ( SELECT_CASE_var == "SIMPLE" ) ) {
					if ( cAlphaArgs( 3 ) == "SIMPLE" ) {
						ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + Zone( ZoneLoop ).Name + "\"." );
						ShowContinueError( "Deprecated value in " + cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 3 ) + "\", defaulting to SimpleCombined." );
					}
					Zone( ZoneLoop ).OutsideConvectionAlgo = ASHRAESimple;

				} else if ( ( SELECT_CASE_var == "TARP" ) || ( SELECT_CASE_var == "DETAILED" ) || ( SELECT_CASE_var == "BLAST" ) ) {
					if ( cAlphaArgs( 3 ) == "DETAILED" ) {
						ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + Zone( ZoneLoop ).Name + "\"." );
						ShowContinueError( "Deprecated value in " + cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 3 ) + "\", defaulting to TARP." );
					}
					if ( cAlphaArgs( 3 ) == "BLAST" ) {
						ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + Zone( ZoneLoop ).Name + "\"." );
						ShowContinueError( "Deprecated value in " + cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 3 ) + "\", defaulting to TARP." );
					}
					Zone( ZoneLoop ).OutsideConvectionAlgo = ASHRAETARP;

				} else if ( SELECT_CASE_var == "MOWITT" ) {
					Zone( ZoneLoop ).OutsideConvectionAlgo = MoWiTTHcOutside;

				} else if ( ( SELECT_CASE_var == "DOE2" ) || ( SELECT_CASE_var == "DOE-2" ) ) {
					Zone( ZoneLoop ).OutsideConvectionAlgo = DOE2HcOutside;

				} else if ( SELECT_CASE_var == "ADAPTIVECONVECTIONALGORITHM" ) {
					Zone( ZoneLoop ).OutsideConvectionAlgo = AdaptiveConvectionAlgorithm;

				} else {
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + Zone( ZoneLoop ).Name + "\"." );
					ShowContinueError( "Invalid value for " + cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 3 ) + "\"." );
					ErrorsFound = true;
					//Zone( ZoneLoop ).OutsideConvectionAlgo = AdaptiveConvectionAlgorithm;

				}}
		} else {
			// No zone specific algorithm specified, use default Outside Convection Algorithm
			Zone( ZoneLoop ).OutsideConvectionAlgo = DefaultOutsideConvectionAlgo;

		}

		// Process the input field:    Part of Total Floor Area
		//   The default value is YES and so only NO needs to be handled
		if ( NumAlphas > 3 ) {
			if ( SameString( "No", cAlphaArgs( 4 ) ) ) {
				Zone( ZoneLoop ).isPartOfTotalArea = false;
			} else if ( SameString( "Yes", cAlphaArgs( 4 ) ) || lAlphaFieldBlanks( 4 ) ) {
				Zone( ZoneLoop ).isPartOfTotalArea = true;
			} else {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + Zone( ZoneLoop ).Name + "\"." );
				ShowContinueError( "Invalid value for " + cAlphaFieldNames( 4 ) + "=\"" + cAlphaArgs( 4 ) + "\"." );
				ErrorsFound = true;
			}
		}

		// Zone outdoor environmental variables, used for zone infiltration/ventilation
		SetupOutputVariable( "Zone Outdoor Air Drybulb Temperature [C]", Zone( ZoneLoop ).OutDryBulbTemp, "Zone", "Average", Zone( ZoneLoop ).Name );
		SetupOutputVariable( "Zone Outdoor Air Wetbulb Temperature [C]", Zone( ZoneLoop ).OutWetBulbTemp, "Zone", "Average", Zone( ZoneLoop ).Name );
		SetupOutputVariable( "Zone Outdoor Air Wind Speed [m/s]", Zone( ZoneLoop ).WindSpeed, "Zone", "Average", Zone( ZoneLoop ).Name );
	}

	// End of Get Input subroutines for the HB Module
	//******************************************************************************

	// Beginning Initialization Section of the Module
	//******************************************************************************

	void
	InitHeatBalance()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   April 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is the main driver for initializations within the
		// heat balance.

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger initialization events.  Some of the files
		//  have been moved to other heat balance managers.  More of these initializations
		//  will have to continue to be re-structured.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace ConductionTransferFunctionCalc;
		using namespace WindowManager;
		using namespace SolarShading;
		using DaylightingDevices::InitDaylightingDevices;
		using DataSystemVariables::DetailedSolarTimestepIntegration;
		//  USE DataRoomAirModel, ONLY: IsZoneDV,IsZoneCV,HVACMassFlow, ZoneDVMixedFlag
		using WindowEquivalentLayer::InitEquivalentLayerWindowCalculations;

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
		int StormWinNum; // Number of StormWindow object
		int SurfNum; // Surface number
		static bool ChangeSet( true ); // Toggle for checking storm windows

		// FLOW:

		if ( BeginSimFlag ) {
			AllocateHeatBalArrays(); // Allocate the Module Arrays
			if ( any_eq( HeatTransferAlgosUsed, UseCTF ) || any_eq( HeatTransferAlgosUsed, UseEMPD ) ) {
				DisplayString( "Initializing Response Factors" );
				InitConductionTransferFunctions(); // Initialize the response factors
			}

			DisplayString( "Initializing Window Optical Properties" );
			InitEquivalentLayerWindowCalculations(); // Initialize the EQL window optical properties
			InitGlassOpticalCalculations(); // Initialize the window optical properties
			InitDaylightingDevices(); // Initialize any daylighting devices
			DisplayString( "Initializing Solar Calculations" );
			InitSolarCalculations(); // Initialize the shadowing calculations

		}

		if ( BeginEnvrnFlag ) {

			MaxHeatLoadPrevDay = 0.0;
			MaxCoolLoadPrevDay = 0.0;
			MaxTempPrevDay = 0.0;
			MinTempPrevDay = 0.0;
			MaxHeatLoadZone = -9999.0;
			MaxCoolLoadZone = -9999.0;
			MaxTempZone = -9999.0;
			MinTempZone = 1000.0;
			TempZone = -9999.0;
			LoadZone = -9999.0;
			TempZonePrevDay = 1000.0;
			LoadZonePrevDay = -9999.0;
			TempZoneSecPrevDay = 1000.0;
			TempZoneSecPrevDay = -9999.0;
			WarmupTempDiff = 0.0;
			WarmupLoadDiff = 0.0;
			TempZoneRpt = 0.0;
			LoadZoneRpt = 0.0;
			MaxLoadZoneRpt = 0.0;
			CountWarmupDayPoints = 0;

			for ( auto & e : SurfaceWindow ) {
				e.ThetaFace = 296.15;
				e.EffInsSurfTemp = 23.0;
			}

		}

		if ( TotStormWin > 0 ) {
			if ( BeginDayFlag ) {
				SetStormWindowControl();
				ChangeSet = false;
			} else if ( ! ChangeSet ) {
				StormWinChangeThisDay = false;
				for ( StormWinNum = 1; StormWinNum <= TotStormWin; ++StormWinNum ) {
					SurfNum = StormWindow( StormWinNum ).BaseWindowNum;
					SurfaceWindow( SurfNum ).StormWinFlagPrevDay = SurfaceWindow( SurfNum ).StormWinFlag;
				}
				ChangeSet = true;
			}
		}

		if ( BeginDayFlag ) {
			if ( ! WarmupFlag ) {
				if ( DayOfSim == 1 ) {
					MaxHeatLoadZone = -9999.0;
					MaxCoolLoadZone = -9999.0;
					MaxTempZone = -9999.0;
					MinTempZone = 1000.0;
				}
			}
			if ( ! DetailedSolarTimestepIntegration ) {
				PerformSolarCalculations();
			}
		}

		if ( DetailedSolarTimestepIntegration ) { // always redo solar calcs
			PerformSolarCalculations();
		}

	}

	void
	AllocateHeatBalArrays()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Liesen
		//       DATE WRITTEN   February 1998
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine allocates the arrays to meet simulation requirements

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger variable allocation.

		// REFERENCES:
		// na

		// USE STATEMENTS:

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

		// FLOW:

		// Use the total number of zones or surfaces to allocate variables to avoid a limit
		// Allocate real Variables
		// Following used for Calculations
		//  Allocate variables in DataHeatBalSys
		SumConvHTRadSys.dimension( NumOfZones, 0.0 );
		SumLatentHTRadSys.dimension( NumOfZones, 0.0 );
		SumConvPool.dimension( NumOfZones, 0.0 );
		SumLatentPool.dimension( NumOfZones, 0.0 );
		QHTRadSysToPerson.dimension( NumOfZones, 0.0 );
		QHWBaseboardToPerson.dimension( NumOfZones, 0.0 );
		QSteamBaseboardToPerson.dimension( NumOfZones, 0.0 );
		QElecBaseboardToPerson.dimension( NumOfZones, 0.0 );
		XMAT.dimension( NumOfZones, 23.0 );
		XM2T.dimension( NumOfZones, 23.0 );
		XM3T.dimension( NumOfZones, 23.0 );
		XM4T.dimension( NumOfZones, 23.0 );
		DSXMAT.dimension( NumOfZones, 23.0 );
		DSXM2T.dimension( NumOfZones, 23.0 );
		DSXM3T.dimension( NumOfZones, 23.0 );
		DSXM4T.dimension( NumOfZones, 23.0 );
		XMPT.dimension( NumOfZones, 23.0 );
		MCPI.dimension( NumOfZones, 0.0 );
		MCPTI.dimension( NumOfZones, 0.0 );
		MCPV.dimension( NumOfZones, 0.0 );
		MCPTV.dimension( NumOfZones, 0.0 );
		MCPM.dimension( NumOfZones, 0.0 );
		MCPTM.dimension( NumOfZones, 0.0 );
		MixingMassFlowZone.dimension( NumOfZones, 0.0 );
		MixingMassFlowXHumRat.dimension( NumOfZones, 0.0 );
		ZoneReOrder.allocate( NumOfZones );
		ZoneMassBalanceFlag.dimension( NumOfZones, false );
		ZoneInfiltrationFlag.dimension( NumOfZones, false );
		ZoneReOrder = 0;
		ZoneLatentGain.dimension( NumOfZones, 0.0 );
		OAMFL.dimension( NumOfZones, 0.0 );
		VAMFL.dimension( NumOfZones, 0.0 );
		ZTAV.dimension( NumOfZones, 23.0 );
		ZTAVComf.dimension( NumOfZones, 23.0 );
		ZT.dimension( NumOfZones, 23.0 );
		TempTstatAir.dimension( NumOfZones, 23.0 );
		MAT.dimension( NumOfZones, 23.0 );
		ZoneTMX.dimension( NumOfZones, 23.0 );
		ZoneTM2.dimension( NumOfZones, 23.0 );
		// Allocate this zone air humidity ratio
		ZoneAirHumRatAvg.dimension( NumOfZones, 0.01 );
		ZoneAirHumRatAvgComf.dimension( NumOfZones, 0.01 );
		ZoneAirHumRat.dimension( NumOfZones, 0.01 );
		ZoneAirHumRatOld.dimension( NumOfZones, 0.01 );
		SumHmAW.dimension( NumOfZones, 0.0 );
		SumHmARa.dimension( NumOfZones, 0.0 );
		SumHmARaW.dimension( NumOfZones, 0.0 );
		MCPTE.dimension( NumOfZones, 0.0 );
		MCPE.dimension( NumOfZones, 0.0 );
		EAMFL.dimension( NumOfZones, 0.0 );
		MCPTC.dimension( NumOfZones, 0.0 );
		MCPC.dimension( NumOfZones, 0.0 );
		CTMFL.dimension( NumOfZones, 0.0 );
		MDotCPOA.dimension( NumOfZones, 0.0 );
		MDotOA.dimension( NumOfZones, 0.0 );
		if ( Contaminant.CO2Simulation ) {
			OutdoorCO2 = GetCurrentScheduleValue( Contaminant.CO2OutdoorSchedPtr );
			ZoneAirCO2.dimension( NumOfZones, OutdoorCO2 );
			ZoneAirCO2Temp.dimension( NumOfZones, OutdoorCO2 );
			ZoneAirCO2Avg.dimension( NumOfZones, OutdoorCO2 );
		}
		if ( Contaminant.GenericContamSimulation ) {
			OutdoorGC = GetCurrentScheduleValue( Contaminant.GenericContamOutdoorSchedPtr );
			ZoneAirGC.dimension( NumOfZones, OutdoorGC );
			ZoneAirGCTemp.dimension( NumOfZones, OutdoorGC );
			ZoneAirGCAvg.dimension( NumOfZones, OutdoorGC );
		}
		MaxTempPrevDay.dimension( NumOfZones, 0.0 );
		MinTempPrevDay.dimension( NumOfZones, 0.0 );
		MaxHeatLoadPrevDay.dimension( NumOfZones, 0.0 );
		MaxCoolLoadPrevDay.dimension( NumOfZones, 0.0 );
		MaxHeatLoadZone.dimension( NumOfZones, -9999.0 );
		MaxCoolLoadZone.dimension( NumOfZones, -9999.0 );
		MaxTempZone.dimension( NumOfZones, -9999.0 );
		MinTempZone.dimension( NumOfZones, 1000.0 );
		TempZonePrevDay.dimension( NumOfZones, 0.0 );
		LoadZonePrevDay.dimension( NumOfZones, 0.0 );
		TempZoneSecPrevDay.dimension( NumOfZones, 0.0 );
		LoadZoneSecPrevDay.dimension( NumOfZones, 0.0 );
		WarmupTempDiff.dimension( NumOfZones, 0.0 );
		WarmupLoadDiff.dimension( NumOfZones, 0.0 );
		TempZone.dimension( NumOfZones, 0.0 );
		LoadZone.dimension( NumOfZones, 0.0 );
		TempZoneRpt.dimension( NumOfZones, NumOfTimeStepInHour * 24, 0.0 );
		LoadZoneRpt.dimension( NumOfZones, NumOfTimeStepInHour * 24, 0.0 );
		MaxLoadZoneRpt.dimension( NumOfZones, NumOfTimeStepInHour * 24, 0.0 );
		WarmupConvergenceValues.allocate( NumOfZones );
		TempZoneRptStdDev.allocate( NumOfTimeStepInHour * 24 );
		LoadZoneRptStdDev.allocate( NumOfTimeStepInHour * 24 );
		//MassConservation.allocate( NumOfZones );

		CountWarmupDayPoints = 0;

	}

	// End Initialization Section of the Module
	//******************************************************************************

	// Beginning of Record Keeping subroutines for the HB Module
	// *****************************************************************************

	void
	RecKeepHeatBalance()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   April 1997
		//       MODIFIED       June 2011, Daeho Kang for individual zone maximums & convergence outputs
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is the main driver for record keeping within the
		// heat balance.

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger record keeping events.

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::RoundSigDigits;
		using DataSystemVariables::ReportDetailedWarmupConvergence;

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
		//  CHARACTER(len=MaxNameLength) :: ZoneName
		int ZoneNum;
		static bool FirstWarmupWrite( true );

		// Formats
		static gio::Fmt Format_731( "(' Warmup Convergence Information, ',A,',',A,',',A,',',A,',',A)" );
		static gio::Fmt Format_732( "('! <Warmup Convergence Information>,Zone Name,Time Step,Hour of Day,Warmup Temperature Difference {deltaC},','Warmup Load Difference {W}')" );

		// FLOW:

		// Always do the following record keeping (every time step):
		// Record Maxs & Mins for individual zone
		for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
			if ( ZTAV( ZoneNum ) > MaxTempZone( ZoneNum ) ) {
				MaxTempZone( ZoneNum ) = ZTAV( ZoneNum );
			}
			if ( ZTAV( ZoneNum ) < MinTempZone( ZoneNum ) ) {
				MinTempZone( ZoneNum ) = ZTAV( ZoneNum );
			}
			if ( SNLoadHeatRate( ZoneNum ) > MaxHeatLoadZone( ZoneNum ) ) {
				MaxHeatLoadZone( ZoneNum ) = SNLoadHeatRate( ZoneNum );
			}
			if ( SNLoadCoolRate( ZoneNum ) > MaxCoolLoadZone( ZoneNum ) ) {
				MaxCoolLoadZone( ZoneNum ) = SNLoadCoolRate( ZoneNum );
			}

			// Record temperature and load for individual zone
			TempZoneSecPrevDay( ZoneNum ) = TempZonePrevDay( ZoneNum );
			LoadZoneSecPrevDay( ZoneNum ) = LoadZonePrevDay( ZoneNum );
			TempZonePrevDay( ZoneNum ) = TempZone( ZoneNum );
			LoadZonePrevDay( ZoneNum ) = LoadZone( ZoneNum );
			TempZone( ZoneNum ) = ZTAV( ZoneNum );
			LoadZone( ZoneNum ) = max( SNLoadHeatRate( ZoneNum ), std::abs( SNLoadCoolRate( ZoneNum ) ) );

			// Calculate differences in temperature and load for the last two warmup days
			if ( ! WarmupFlag && DayOfSim == 1 && ! DoingSizing ) {
				WarmupTempDiff( ZoneNum ) = std::abs( TempZoneSecPrevDay( ZoneNum ) - TempZonePrevDay( ZoneNum ) );
				WarmupLoadDiff( ZoneNum ) = std::abs( LoadZoneSecPrevDay( ZoneNum ) - LoadZonePrevDay( ZoneNum ) );
				if ( ZoneNum == 1 ) ++CountWarmupDayPoints;
				TempZoneRpt( ZoneNum, CountWarmupDayPoints ) = WarmupTempDiff( ZoneNum );
				LoadZoneRpt( ZoneNum, CountWarmupDayPoints ) = WarmupLoadDiff( ZoneNum );
				MaxLoadZoneRpt( ZoneNum, CountWarmupDayPoints ) = LoadZone( ZoneNum );

				if ( ReportDetailedWarmupConvergence ) { // only do this detailed thing when requested by user is on
					// Write Warmup Convergence Information to the initialization output file
					if ( FirstWarmupWrite ) {
						gio::write( OutputFileInits, Format_732 );
						FirstWarmupWrite = false;
					}

					gio::write( OutputFileInits, Format_731 ) << Zone( ZoneNum ).Name << RoundSigDigits( TimeStep ) << RoundSigDigits( HourOfDay ) << RoundSigDigits( WarmupTempDiff( ZoneNum ), 10 ) << RoundSigDigits( WarmupLoadDiff( ZoneNum ), 10 );

				}
			}

		}

		// There is no hourly record keeping in the heat balance.

		// There is no daily record keeping in the heat balance.

		// There is no environment level record keeping in the heat balance.

		// There is no simulation level record keeping in the heat balance.

	}

	void
	CheckWarmupConvergence()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   April 1997
		//       MODIFIED       June 2011, Daeho Kang for individual zone comparison
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine checks warmup convergence values.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const MinLoad( 100.0 ); // Minimum laods for convergence check
		// To avoid big percentage difference in low load situations

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ZoneNum;
		static bool WarmupConvergenceWarning( false );
		static bool SizingWarmupConvergenceWarning( false );
		bool ConvergenceChecksFailed;

		// Convergence criteria for warmup days:
		// Perform another warmup day unless both the % change in loads and
		// absolute change in zone temp min & max are less than their criteria.

		ConvergenceChecksFailed = false;

		if ( NumOfZones <= 0 ) { // if there are no zones, immediate convergence
			WarmupFlag = false;
		} else {
			for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {

				WarmupConvergenceValues( ZoneNum ).TestMaxTempValue = std::abs( MaxTempPrevDay( ZoneNum ) - MaxTempZone( ZoneNum ) );
				WarmupConvergenceValues( ZoneNum ).TestMinTempValue = std::abs( MinTempPrevDay( ZoneNum ) - MinTempZone( ZoneNum ) );
				if ( WarmupConvergenceValues( ZoneNum ).TestMaxTempValue <= TempConvergTol ) {
					WarmupConvergenceValues( ZoneNum ).PassFlag( 1 ) = 2;
				} else {
					ConvergenceChecksFailed = true;
					WarmupConvergenceValues( ZoneNum ).PassFlag( 1 ) = 1;
				}

				if ( WarmupConvergenceValues( ZoneNum ).TestMinTempValue <= TempConvergTol ) {
					WarmupConvergenceValues( ZoneNum ).PassFlag( 2 ) = 2;
				} else {
					ConvergenceChecksFailed = true;
					WarmupConvergenceValues( ZoneNum ).PassFlag( 2 ) = 1;
				}

				if ( MaxHeatLoadZone( ZoneNum ) > 1.0e-4 ) { // make sure load big enough to divide
					MaxHeatLoadZone( ZoneNum ) = std::abs( max( MaxHeatLoadZone( ZoneNum ), MinLoad ) );
					WarmupConvergenceValues( ZoneNum ).TestMaxHeatLoadValue = std::abs( ( MaxHeatLoadZone( ZoneNum ) - MaxHeatLoadPrevDay( ZoneNum ) ) / MaxHeatLoadZone( ZoneNum ) );
					if ( WarmupConvergenceValues( ZoneNum ).TestMaxHeatLoadValue <= LoadsConvergTol ) {
						WarmupConvergenceValues( ZoneNum ).PassFlag( 3 ) = 2;
					} else {
						ConvergenceChecksFailed = true;
						WarmupConvergenceValues( ZoneNum ).PassFlag( 3 ) = 1;
					}
				} else {
					WarmupConvergenceValues( ZoneNum ).PassFlag( 3 ) = 2;
				}

				if ( MaxCoolLoadZone( ZoneNum ) > 1.0e-4 ) {
					MaxCoolLoadZone( ZoneNum ) = std::abs( max( MaxCoolLoadZone( ZoneNum ), MinLoad ) );
					WarmupConvergenceValues( ZoneNum ).TestMaxCoolLoadValue = std::abs( ( MaxCoolLoadZone( ZoneNum ) - MaxCoolLoadPrevDay( ZoneNum ) ) / MaxCoolLoadZone( ZoneNum ) );
					if ( WarmupConvergenceValues( ZoneNum ).TestMaxCoolLoadValue <= LoadsConvergTol ) {
						WarmupConvergenceValues( ZoneNum ).PassFlag( 4 ) = 2;
					} else {
						ConvergenceChecksFailed = true;
						WarmupConvergenceValues( ZoneNum ).PassFlag( 4 ) = 1;
					}
				} else {
					WarmupConvergenceValues( ZoneNum ).PassFlag( 4 ) = 2;
				}

				if ( DayOfSim >= MaxNumberOfWarmupDays && WarmupFlag ) {
					// Check convergence for individual zone
					if ( sum( WarmupConvergenceValues( ZoneNum ).PassFlag ) != 8 ) { // pass=2 * 4 values for convergence
						ShowSevereError( "CheckWarmupConvergence: Loads Initialization, Zone=\"" + Zone( ZoneNum ).Name + "\" did not converge after " + RoundSigDigits( MaxNumberOfWarmupDays ) + " warmup days." );
						if ( ! WarmupConvergenceWarning && ! DoingSizing ) {
							ShowContinueError( "See Warmup Convergence Information in .eio file for details." );
							WarmupConvergenceWarning = true;
						} else if ( ! SizingWarmupConvergenceWarning && DoingSizing ) {
							ShowContinueError( "Warmup Convergence failing during sizing." );
							SizingWarmupConvergenceWarning = true;
						}
						if ( RunPeriodEnvironment ) {
							ShowContinueError( "...Environment(RunPeriod)=\"" + EnvironmentName + "\"" );
						} else {
							ShowContinueError( "...Environment(SizingPeriod)=\"" + EnvironmentName + "\"" );
						}

						ShowContinueError( "..Max Temp Comparison = " + RoundSigDigits( WarmupConvergenceValues( ZoneNum ).TestMaxTempValue, 2 ) + " vs Temperature Convergence Tolerance=" + RoundSigDigits( TempConvergTol, 2 ) + " - " + PassFail( WarmupConvergenceValues( ZoneNum ).PassFlag( 1 ) ) + " Convergence" );
						ShowContinueError( "..Min Temp Comparison = " + RoundSigDigits( WarmupConvergenceValues( ZoneNum ).TestMinTempValue, 2 ) + " vs Temperature Convergence Tolerance=" + RoundSigDigits( TempConvergTol, 2 ) + " - " + PassFail( WarmupConvergenceValues( ZoneNum ).PassFlag( 2 ) ) + " Convergence" );
						ShowContinueError( "..Max Heat Load Comparison = " + RoundSigDigits( WarmupConvergenceValues( ZoneNum ).TestMaxHeatLoadValue, 4 ) + " vs Loads Convergence Tolerance=" + RoundSigDigits( LoadsConvergTol, 2 ) + " - " + PassFail( WarmupConvergenceValues( ZoneNum ).PassFlag( 3 ) ) + " Convergence" );
						ShowContinueError( "..Max Cool Load Comparison = " + RoundSigDigits( WarmupConvergenceValues( ZoneNum ).TestMaxCoolLoadValue, 4 ) + " vs Loads Convergence Tolerance=" + RoundSigDigits( LoadsConvergTol, 2 ) + " - " + PassFail( WarmupConvergenceValues( ZoneNum ).PassFlag( 4 ) ) + " Convergence" );
					}

				}

				// Transfer current daily max and min loads and temperatures to the
				// variables containing the last day's values
				MaxHeatLoadPrevDay( ZoneNum ) = MaxHeatLoadZone( ZoneNum );
				MaxCoolLoadPrevDay( ZoneNum ) = MaxCoolLoadZone( ZoneNum );
				MaxTempPrevDay( ZoneNum ) = MaxTempZone( ZoneNum );
				MinTempPrevDay( ZoneNum ) = MinTempZone( ZoneNum );

				MaxHeatLoadZone( ZoneNum ) = -9999.0;
				MaxCoolLoadZone( ZoneNum ) = -9999.0;
				MaxTempZone( ZoneNum ) = -9999.0;
				MinTempZone( ZoneNum ) = 1000.0;

			}

			// Limit the number of warmup days, regardless of the number of zones
			// in the building, to some arbitrary value based on common sense and
			// experience with the (I)BLAST program.  If too many warmup days were
			// required, notify the program user.

			if ( ( DayOfSim >= MaxNumberOfWarmupDays ) && WarmupFlag && ConvergenceChecksFailed ) {
				if ( MaxNumberOfWarmupDays < DefaultMaxNumberOfWarmupDays ) {
					ShowSevereError( "CheckWarmupConvergence: User supplied maximum warmup days=" + RoundSigDigits( MaxNumberOfWarmupDays ) + " is insufficient." );
					ShowContinueError( "Suggest setting maximum number of warmup days to at least " + RoundSigDigits( DefaultMaxNumberOfWarmupDays ) + '.' );
				}
			}

			// Set warmup flag to true depending on value of ConvergenceChecksFailed (true=fail)
			// and minimum number of warmup days
			if ( ! ConvergenceChecksFailed && DayOfSim >= MinNumberOfWarmupDays ) {
				WarmupFlag = false;
			} else if ( ! ConvergenceChecksFailed && DayOfSim < MinNumberOfWarmupDays ) {
				WarmupFlag = true;
			}

			// If max warmup days reached and still WarmupFlag, then go to non-warmup state.
			// prior messages will have been displayed
			if ( ( DayOfSim >= MaxNumberOfWarmupDays ) && WarmupFlag ) {
				WarmupFlag = false;
			}

		}

	}

	void
	ReportWarmupConvergence()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   October 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// na

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::RoundSigDigits;

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
		int ZoneNum;
		static bool FirstWarmupWrite( true );
		Real64 AverageZoneTemp;
		Real64 AverageZoneLoad;
		Real64 StdDevZoneTemp;
		Real64 StdDevZoneLoad;
		std::string EnvHeader;
		int Num; // loop control

		// Formats
		static gio::Fmt Format_730( "('! <Warmup Convergence Information>,Zone Name,Environment Type/Name,','Average Warmup Temperature Difference {deltaC},','Std Dev Warmup Temperature Difference {deltaC},Max Temperature Pass/Fail Convergence,','Min Temperature Pass/Fail Convergence,Average Warmup Load Difference {W},Std Dev Warmup Load Difference {W},','Heating Load Pass/Fail Convergence,Cooling Load Pass/Fail Convergence')" );
		static gio::Fmt Format_731( "(' Warmup Convergence Information',10(',',A))" );

		if ( ! WarmupFlag ) { // Report out average/std dev
			// Write Warmup Convervence Information to the initialization output file
			if ( FirstWarmupWrite && NumOfZones > 0 ) {
				gio::write( OutputFileInits, Format_730 );
				FirstWarmupWrite = false;
			}

			TempZoneRptStdDev = 0.0;
			LoadZoneRptStdDev = 0.0;

			if ( RunPeriodEnvironment ) {
				EnvHeader = "RunPeriod:";
			} else {
				EnvHeader = "SizingPeriod:";
			}

			for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
				AverageZoneTemp = sum( TempZoneRpt( ZoneNum, {1,CountWarmupDayPoints} ) ) / double( CountWarmupDayPoints );
				for ( Num = 1; Num <= CountWarmupDayPoints; ++Num ) {
					if ( MaxLoadZoneRpt( ZoneNum, Num ) > 1.e-4 ) {
						LoadZoneRpt( ZoneNum, Num ) /= MaxLoadZoneRpt( ZoneNum, Num );
					} else {
						LoadZoneRpt( ZoneNum, Num ) = 0.0;
					}
				}
				AverageZoneLoad = sum( LoadZoneRpt( ZoneNum, {1,CountWarmupDayPoints} ) ) / double( CountWarmupDayPoints );
				StdDevZoneTemp = 0.0;
				StdDevZoneLoad = 0.0;
				for ( Num = 1; Num <= CountWarmupDayPoints; ++Num ) {
					TempZoneRptStdDev( Num ) = pow_2( TempZoneRpt( ZoneNum, Num ) - AverageZoneTemp );
					LoadZoneRptStdDev( Num ) = pow_2( LoadZoneRpt( ZoneNum, Num ) - AverageZoneLoad );
				}
				StdDevZoneTemp = std::sqrt( sum( TempZoneRptStdDev( {1,CountWarmupDayPoints} ) ) / double( CountWarmupDayPoints ) );
				StdDevZoneLoad = std::sqrt( sum( LoadZoneRptStdDev( {1,CountWarmupDayPoints} ) ) / double( CountWarmupDayPoints ) );

				gio::write( OutputFileInits, Format_731 ) << Zone( ZoneNum ).Name << EnvHeader + ' ' + EnvironmentName << RoundSigDigits( AverageZoneTemp, 10 ) << RoundSigDigits( StdDevZoneTemp, 10 ) << PassFail( WarmupConvergenceValues( ZoneNum ).PassFlag( 1 ) ) << PassFail( WarmupConvergenceValues( ZoneNum ).PassFlag( 2 ) ) << RoundSigDigits( AverageZoneLoad, 10 ) << RoundSigDigits( StdDevZoneLoad, 10 ) << PassFail( WarmupConvergenceValues( ZoneNum ).PassFlag( 3 ) ) << PassFail( WarmupConvergenceValues( ZoneNum ).PassFlag( 4 ) );
			}

		}

	}

	//        End of Record Keeping subroutines for the HB Module
	// *****************************************************************************

	// Beginning of Reporting subroutines for the HB Module
	// *****************************************************************************

	void
	ReportHeatBalance()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   July 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is the main driver for reporting within the heat
		// balance.

		// METHODOLOGY EMPLOYED:
		// Uses the status flags to trigger record keeping events.

		// REFERENCES:
		// na

		// Using/Aliasing
		using OutputReportTabular::UpdateTabularReports;
		using ScheduleManager::ReportScheduleValues;
		using NodeInputManager::CalcMoreNodeInfo;
		using EconomicTariff::UpdateUtilityBills; // added for computing annual utility costs
		using DataSystemVariables::ReportDuringWarmup; // added for FMI
		using DataSystemVariables::UpdateDataDuringWarmupExternalInterface;
		using namespace DataReportingFlags;
		using DataGlobals::KindOfSim;
		using DataGlobals::ksHVACSizeDesignDay;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static gio::Fmt EndOfHeaderFormat( "('End of Data Dictionary')" ); // End of data dictionary marker
		static gio::Fmt EnvironmentStampFormat( "(a,',',a,3(',',f7.2),',',f7.2)" ); // Format descriptor for environ stamp

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//  LOGICAL, SAVE :: PrintEnvrnStamp=.FALSE.

		// FLOW:

		// Time step level reporting:

		ReportScheduleValues();
		if ( !WarmupFlag && DoOutputReporting ) {
			CalcMoreNodeInfo();
			UpdateDataandReport( ZoneTSReporting );
			if ( KindOfSim == ksHVACSizeDesignDay || KindOfSim == ksHVACSizeRunPeriodDesign ) {
				if ( hvacSizingSimulationManager ) hvacSizingSimulationManager->UpdateSizingLogsZoneStep();
			}

			UpdateTabularReports( ZoneTSReporting );
			UpdateUtilityBills();
		} else if ( ! KickOffSimulation && DoOutputReporting && ReportDuringWarmup ) {
			if ( BeginDayFlag && ! PrintEnvrnStampWarmupPrinted ) {
				PrintEnvrnStampWarmup = true;
				PrintEnvrnStampWarmupPrinted = true;
			}
			if ( ! BeginDayFlag ) PrintEnvrnStampWarmupPrinted = false;
			if ( PrintEnvrnStampWarmup ) {
				if ( PrintEndDataDictionary && DoOutputReporting ) {
					gio::write( OutputFileStandard, EndOfHeaderFormat );
					gio::write( OutputFileMeters, EndOfHeaderFormat );
					PrintEndDataDictionary = false;
				}
				if ( DoOutputReporting ) {
					gio::write( OutputFileStandard, EnvironmentStampFormat ) << "1" << "Warmup {" + cWarmupDay + "} " + EnvironmentName << Latitude << Longitude << TimeZoneNumber << Elevation;
					gio::write( OutputFileMeters, EnvironmentStampFormat ) << "1" << "Warmup {" + cWarmupDay + "} " + EnvironmentName << Latitude << Longitude << TimeZoneNumber << Elevation;
					PrintEnvrnStampWarmup = false;
				}
			}
			CalcMoreNodeInfo();
			UpdateDataandReport( ZoneTSReporting );
			if ( KindOfSim == ksHVACSizeDesignDay || KindOfSim == ksHVACSizeRunPeriodDesign ) {
				if ( hvacSizingSimulationManager ) hvacSizingSimulationManager->UpdateSizingLogsZoneStep();
			}

		} else if ( UpdateDataDuringWarmupExternalInterface ) { // added for FMI
			UpdateDataandReport( ZoneTSReporting );
			if ( KindOfSim == ksHVACSizeDesignDay || KindOfSim == ksHVACSizeRunPeriodDesign ) {
				if ( hvacSizingSimulationManager ) hvacSizingSimulationManager->UpdateSizingLogsZoneStep();
			}
		}
		// There is no hourly reporting in the heat balance.

		// There is no daily reporting in the heat balance.

		// There is no simulation level record keeping in the heat balance.

	}

	//        End of Reporting subroutines for the HB Module

	void
	GetFrameAndDividerData( bool & ErrorsFound ) // set to true if errors found in input
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   May 2000
		//       MODIFIED       April 2002 (FCW): get window reveal data
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Gets input data for window frame and/or divider and/or window
		// inside/outside reveal.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using InputProcessor::VerifyName;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int IOStat; // IO Status when calling get input subroutine
		Array1D_string FrameDividerNames( 2 ); // Frame/Divider Alpha names
		int FrameDividerNum; // Counter to keep track of the frame/divider number
		int FrameDividerNumAlpha; // Number of frame/divider alpha names being passed
		int FrameDividerNumProp; // Number of frame/divider properties being passed
		Array1D< Real64 > FrameDividerProps( 23 ); // Temporary array to transfer frame/divider properties
		int Loop;
		bool ErrorInName;
		bool IsBlank;

		CurrentModuleObject = "WindowProperty:FrameAndDivider";
		TotFrameDivider = GetNumObjectsFound( CurrentModuleObject );
		FrameDivider.allocate( TotFrameDivider );
		if ( TotFrameDivider == 0 ) return;

		FrameDividerNum = 0;

		for ( Loop = 1; Loop <= TotFrameDivider; ++Loop ) {

			//Call Input Get routine to retrieve frame/divider data
			GetObjectItem( CurrentModuleObject, Loop, FrameDividerNames, FrameDividerNumAlpha, FrameDividerProps, FrameDividerNumProp, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			ErrorInName = false;
			IsBlank = false;
			VerifyName( FrameDividerNames( 1 ), FrameDivider, FrameDividerNum, ErrorInName, IsBlank, CurrentModuleObject + " Name" );
			if ( ErrorInName ) {
				ErrorsFound = true;
				continue;
			}

			//Load the frame/divider derived type from the input data.
			++FrameDividerNum;
			FrameDivider( FrameDividerNum ).Name = FrameDividerNames( 1 );
			FrameDivider( FrameDividerNum ).FrameWidth = FrameDividerProps( 1 );
			FrameDivider( FrameDividerNum ).FrameProjectionOut = FrameDividerProps( 2 );
			FrameDivider( FrameDividerNum ).FrameProjectionIn = FrameDividerProps( 3 );
			if ( FrameDivider( FrameDividerNum ).FrameWidth == 0.0 ) {
				FrameDivider( FrameDividerNum ).FrameProjectionOut = 0.0;
				FrameDivider( FrameDividerNum ).FrameProjectionIn = 0.0;
			}
			FrameDivider( FrameDividerNum ).FrameConductance = FrameDividerProps( 4 );
			FrameDivider( FrameDividerNum ).FrEdgeToCenterGlCondRatio = FrameDividerProps( 5 );
			FrameDivider( FrameDividerNum ).FrameSolAbsorp = FrameDividerProps( 6 );
			FrameDivider( FrameDividerNum ).FrameVisAbsorp = FrameDividerProps( 7 );
			FrameDivider( FrameDividerNum ).FrameEmis = FrameDividerProps( 8 );
			if ( SameString( FrameDividerNames( 2 ), "DividedLite" ) ) {
				FrameDivider( FrameDividerNum ).DividerType = DividedLite;
			} else if ( SameString( FrameDividerNames( 2 ), "Suspended" ) ) {
				FrameDivider( FrameDividerNum ).DividerType = Suspended;
			} else {
				ShowWarningError( CurrentModuleObject + "=\"" + FrameDividerNames( 1 ) + "\", Invalid " + cAlphaFieldNames( 2 ) );
				ShowContinueError( "Entered=\"" + FrameDividerNames( 2 ) + "\", must be DividedLite or Suspended.  Will be set to DividedLite." );
				FrameDivider( FrameDividerNum ).DividerType = DividedLite;
			}
			FrameDivider( FrameDividerNum ).DividerWidth = FrameDividerProps( 9 );
			FrameDivider( FrameDividerNum ).HorDividers = FrameDividerProps( 10 );
			FrameDivider( FrameDividerNum ).VertDividers = FrameDividerProps( 11 );
			FrameDivider( FrameDividerNum ).DividerProjectionOut = FrameDividerProps( 12 );
			FrameDivider( FrameDividerNum ).DividerProjectionIn = FrameDividerProps( 13 );
			if ( FrameDivider( FrameDividerNum ).DividerWidth == 0.0 || FrameDivider( FrameDividerNum ).DividerType == Suspended ) {
				FrameDivider( FrameDividerNum ).DividerProjectionOut = 0.0;
				FrameDivider( FrameDividerNum ).DividerProjectionIn = 0.0;
			}
			FrameDivider( FrameDividerNum ).DividerConductance = FrameDividerProps( 14 );
			FrameDivider( FrameDividerNum ).DivEdgeToCenterGlCondRatio = FrameDividerProps( 15 );
			FrameDivider( FrameDividerNum ).DividerSolAbsorp = FrameDividerProps( 16 );
			FrameDivider( FrameDividerNum ).DividerVisAbsorp = FrameDividerProps( 17 );
			FrameDivider( FrameDividerNum ).DividerEmis = FrameDividerProps( 18 );
			FrameDivider( FrameDividerNum ).OutsideRevealSolAbs = FrameDividerProps( 19 );
			FrameDivider( FrameDividerNum ).InsideSillDepth = FrameDividerProps( 20 );
			FrameDivider( FrameDividerNum ).InsideSillSolAbs = FrameDividerProps( 21 );
			FrameDivider( FrameDividerNum ).InsideReveal = FrameDividerProps( 22 );
			FrameDivider( FrameDividerNum ).InsideRevealSolAbs = FrameDividerProps( 23 );

			if ( FrameDivider( FrameDividerNum ).DividerWidth > 0.0 && ( FrameDivider( FrameDividerNum ).HorDividers == 0 && FrameDivider( FrameDividerNum ).VertDividers == 0 ) ) {
				ShowWarningError( CurrentModuleObject + ": In FrameAndDivider " + FrameDivider( FrameDividerNum ).Name + ' ' + cNumericFieldNames( 9 ) + " > 0 " );
				ShowContinueError( "...but " + cNumericFieldNames( 10 ) + " = 0 and " + cNumericFieldNames( 11 ) + " = 0." );
				ShowContinueError( "..." + cNumericFieldNames( 9 ) + " set to 0." );
				FrameDivider( FrameDividerNum ).DividerWidth = 0.0;
			}
			// Prevent InsideSillDepth < InsideReveal
			if ( FrameDivider( FrameDividerNum ).InsideSillDepth < FrameDivider( FrameDividerNum ).InsideReveal ) {
				ShowWarningError( CurrentModuleObject + ": In FrameAndDivider " + FrameDivider( FrameDividerNum ).Name + ' ' + cNumericFieldNames( 20 ) + " is less than " + cNumericFieldNames( 22 ) + "; it will be set to " + cNumericFieldNames( 22 ) + '.' );
				FrameDivider( FrameDividerNum ).InsideSillDepth = FrameDivider( FrameDividerNum ).InsideReveal;
			}

			//    ! Warn if InsideSillDepth OR InsideReveal > 0.2meters to warn of inaccuracies
			//    IF(FrameDivider(FrameDividerNum)%InsideSillDepth > 0.2d0) THEN
			//      CALL ShowWarningError(TRIM(CurrentModuleObject)//': In FrameAndDivider '//TRIM(FrameDivider(FrameDividerNum)%Name)// &
			//        ' '//TRIM(cNumericFieldNames(20))//' is greater than 0.2 meters, which could cause inaccuracies in zone cooling energy.')
			//    END IF
			//    IF(FrameDivider(FrameDividerNum)%InsideReveal > 0.2d0) THEN
			//      CALL ShowWarningError(TRIM(CurrentModuleObject)//': In FrameAndDivider '//TRIM(FrameDivider(FrameDividerNum)%Name)// &
			//        ' '//TRIM(cNumericFieldNames(22))//' is greater than 0.2 meters, which could cause inaccuracies in zone cooling energy.')
			//    END IF

		}

	}

	void
	SearchWindow5DataFile(
		std::string const & DesiredFileName, // File name that contains the Window5 constructions.
		std::string const & DesiredConstructionName, // Name that will be searched for in the Window5 data file
		bool & ConstructionFound, // True if DesiredConstructionName is in the Window5 data file
		bool & EOFonFile, // True if EOF during file read
		bool & ErrorsFound // True if there is a problem with the entry requested from the data file
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   August 2001
		//       MODIFIED       June 2002, FW: do all reallocation here for constructions found on
		//                        data file; 1 new construction of entry has one glazing system;
		//                        2 new constructions if entry has two glazing systems.
		//                      Nov 2002, FW: skip read of mullion data line if one glazing system;
		//                        add error messages for bad data; increase length of input line
		//                        from 132 to 200 to handle case where Window5 puts in extra blanks
		//                        in gas data line.
		//                      Feb 2007, LKL: Add more checks on Window5DataFile
		//                      Jan 2008, LKL: Change Edge/Cond ratio check.
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Searches the WINDOW5 data file for a window with the name "DesiredConstructionName,"
		// which is the name of an idf Construction input using CONSTRUCTION FROM WINDOW5 DATA FILE.
		// (The WINDOW5 data file contains data for one or more complete windows --
		// glazing, frame, mullion, and divider.
		// WINDOW5 writes the data file for export to EnergyPlus so that an annual energy
		// analysis can be done on exactly the same window without having to re-input into
		// EnergyPlus.)

		// If a match is found, a Construction is created and the Material objects associated with
		// the Construction are created. If there is an associated frame or
		// divider in the Window5 data file for this Construction, a FrameAndDivider object will
		// also be created.

		// If the window on the data file has two glazing systems, a second Construction (and its
		// associated materials) corresponding to the second glazing system is created.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::MakeUPPERCase;
		using namespace DataStringGlobals;
		using General::POLYF; // POLYF       ! Polynomial in cosine of angle of incidence
		using General::TrimSigDigits;
		using DataSystemVariables::iUnicode_end;
		using DataSystemVariables::GoodIOStatValue;
		using DataSystemVariables::TempFullFileName;
		using DataSystemVariables::CheckForActualFileName;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static Array1D_string const NumName( 5, { "1", "2", "3", "4", "5" } );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static int W5DataFileNum;
		int FileLineCount; // counter for number of lines read (used in some error messages)
		Array1D_string DataLine( 100 ); // Array of data lines
		std::string NextLine; // Line of data
		std::string WindowNameInW5DataFile;
		std::string W5Name;
		Array1D_string GasName( 3 ); // Gas name from data file
		std::string LayerName; // Layer name from data file
		std::string MullionOrientation; // Horizontal, vertical or none
		int LineNum;
		int ReadStat; // File read status
		Array1D_int NGlass( 2 ); // Number of glass layers in glazing system
		Array2D_int NumGases( 4, 2 ); // Number of gases in each gap of a glazing system
		Array2D_int MaterNumSysGlass( 5, 2 ); // Material numbers for glazing system / glass combinations
		Array2D_int MaterNumSysGap( 4, 2 ); // Material numbers for glazing system / gap combinations
		int TotMaterialsPrev; // Number of materials before adding ones from W5DataFile
		int TotFrameDividerPrev; // Number of FrameAndDivider objects before adding ones from W5DataFile
		Array1D_int NGaps( 2 ); // Number of gaps in window construction
		int NGlSys; // Number of glazing systems (normally 1, but 2 for mullioned window
		//  with two different glazing systems
		int loop; // DO loop counter
		int ILine; // Line counter
		int ConstrNum; // Construction number
		int IGlass; // Glass layer counter
		int IGap; // Gap counter
		int IGas; // Gas counter
		//  INTEGER            :: ICoeff              ! Gas property coefficient counter
		int IGlSys; // Glazing system counter
		int MaterNum; // Material number
		int MatNum;
		int FrDivNum; // FrameDivider number
		bool exists; // True if Window5 data file exists
		Array1D< Real64 > WinHeight( 2 ); // Height, width for glazing system (m)
		Array1D< Real64 > WinWidth( 2 );
		Array1D< Real64 > UValCenter( 2 ); // Center of glass U-value (W/m2-K) for glazing system
		Array1D< Real64 > SCCenter( 2 ); // Center of glass shading coefficient for glazing system
		Array1D< Real64 > SHGCCenter( 2 ); // Center of glass solar heat gain coefficient for glazing system
		Array1D< Real64 > TVisCenter( 2 ); // Center of glass visible transmittance for glazing system
		Array1D< Real64 > Tsol( 11 ); // Solar transmittance vs incidence angle; diffuse trans.
		Array2D< Real64 > AbsSol( 11, 5 ); // Solar absorptance vs inc. angle in each glass layer
		Array1D< Real64 > Rfsol( 11 ); // Front solar reflectance vs inc. angle
		Array1D< Real64 > Rbsol( 11 ); // Back solar reflectance vs inc. angle
		Array1D< Real64 > Tvis( 11 ); // Visible transmittance vs inc. angle
		Array1D< Real64 > Rfvis( 11 ); // Front visible reflectance vs inc. angle
		Array1D< Real64 > Rbvis( 11 ); // Back visible reflectance vs inc. angle
		Array1D< Real64 > CosPhiIndepVar( 10 ); // Cosine of incidence angle from 0 to 90 deg in 10 deg increments
		int IPhi; // Incidence angle counter
		Real64 Phi; // Incidence angle (deg)
		Real64 CosPhi; // Cosine of incidence angle
		Array1D< Real64 > tsolFit( 10 ); // Fitted solar transmittance vs incidence angle
		Array1D< Real64 > tvisFit( 10 ); // Fitted visible transmittance vs incidence angle
		Array1D< Real64 > rfsolFit( 10 ); // Fitted solar front reflectance vs incidence angle
		Array2D< Real64 > solabsFit( 5, 10 ); // Fitted solar absorptance vs incidence angle for each glass layer
		Array1D_string DividerType( 2 ); // Divider type: DividedLite or Suspended
		Real64 FrameWidth;
		Real64 MullionWidth;
		Real64 FrameProjectionOut;
		Real64 FrameProjectionIn;
		Real64 FrameConductance;
		Real64 FrEdgeToCenterGlCondRatio;
		Real64 FrameSolAbsorp;
		Real64 FrameVisAbsorp;
		Real64 FrameEmis;
		Array1D_int HorDividers( 2 ); // For divider: number horizontal for each glazing system
		Array1D_int VertDividers( 2 ); // For divider: number vertical for each glazing system
		Array1D< Real64 > DividerWidth( 2 );
		Array1D< Real64 > DividerProjectionOut( 2 );
		Array1D< Real64 > DividerProjectionIn( 2 );
		Array1D< Real64 > DividerConductance( 2 );
		Array1D< Real64 > DivEdgeToCenterGlCondRatio( 2 );
		Array1D< Real64 > DividerSolAbsorp( 2 );
		Array1D< Real64 > DividerVisAbsorp( 2 );
		Array1D< Real64 > DividerEmis( 2 );
		std::string::size_type endcol;

		// Object Data

		// In the following four gas-related data sets, the first
		//  index is gas type (1=air, 2=Argon, 3=Krypton, 4=Xenon)
		//  and the second index gives a,b,c in the expression
		//  property value = a + bT(K) + cT(K)**2, where T is mean
		//  gap temperature in deg K.

		ConstructionFound = false;
		//ErrorsFound = .FALSE.
		EOFonFile = false;

		CheckForActualFileName( DesiredFileName, exists, TempFullFileName );
		//INQUIRE(FILE=TRIM(DesiredFileName), EXIST=exists)
		if ( ! exists ) {
			ShowSevereError( "HeatBalanceManager: SearchWindow5DataFile: Could not locate Window5 Data File, expecting it as file name=" + DesiredFileName );
			ShowContinueError( "Certain run environments require a full path to be included with the file name in the input field." );
			ShowContinueError( "Try again with putting full path and file name in the field." );
			ShowFatalError( "Program terminates due to these conditions." );
		}

		W5DataFileNum = GetNewUnitNumber();
		{ IOFlags flags; flags.ACTION( "read" ); gio::open( W5DataFileNum, TempFullFileName, flags ); if ( flags.err() ) goto Label999; }
		gio::read( W5DataFileNum, fmtA ) >> NextLine;
		endcol = len( NextLine );
		if ( endcol > 0 ) {
			if ( int( NextLine[ endcol - 1 ] ) == iUnicode_end ) {
				ShowSevereError( "SearchWindow5DataFile: For \"" + DesiredConstructionName + "\" in " + DesiredFileName + " fiile, appears to be a Unicode or binary file." );
				ShowContinueError( "...This file cannot be read by this program. Please save as PC or Unix file and try again" );
				ShowFatalError( "Program terminates due to previous condition." );
			}
		}

		gio::rewind( W5DataFileNum );
		FileLineCount = 0;

		{ IOFlags flags; gio::read( W5DataFileNum, fmtA, flags ) >> NextLine; ReadStat = flags.ios(); }
		if ( ReadStat < GoodIOStatValue ) goto Label1000;
		++FileLineCount;
		if ( ! has_prefixi( NextLine, "WINDOW5" ) ) {
			ShowSevereError( "HeatBalanceManager: SearchWindow5DataFile: Error in Data File=" + DesiredFileName );
			ShowFatalError( "Error reading Window5 Data File: first word of window entry is \"" + NextLine.substr( 0, 7 ) + "\", should be Window5." );
		}

Label10: ;
		for ( LineNum = 2; LineNum <= 5; ++LineNum ) {
			{ IOFlags flags; gio::read( W5DataFileNum, fmtA, flags ) >> DataLine( LineNum ); ReadStat = flags.ios(); }
			if ( ReadStat < GoodIOStatValue ) goto Label1000;
			++FileLineCount;
		}

		// Get window name and check for match
		gio::read( DataLine( 4 ).substr( 19 ), fmtA ) >> W5Name;
		WindowNameInW5DataFile = MakeUPPERCase( W5Name );
		if ( DesiredConstructionName != WindowNameInW5DataFile ) {
			// Doesn't match; read through file until next window entry is found
Label20: ;
			{ IOFlags flags; gio::read( W5DataFileNum, fmtA, flags ) >> NextLine; ReadStat = flags.ios(); }
			if ( ReadStat < GoodIOStatValue ) goto Label1000;
			++FileLineCount;
			if ( ! has_prefixi( NextLine, "WINDOW5" ) ) goto Label20;
			// Beginning of next window entry found
			goto Label10;
		} else {
			// Match found
			ConstructionFound = true;

			// Create Material:WindowGlass, Material:WindowGas, Construction
			// and WindowFrameAndDividerObjects for this window

			{ IOFlags flags; gio::read( W5DataFileNum, fmtA, flags ) >> NextLine; ReadStat = flags.ios(); }
			if ( ReadStat < GoodIOStatValue ) goto Label1000;
			++FileLineCount;
			gio::read( NextLine.substr( 19 ), "*" ) >> NGlSys;
			if ( NGlSys <= 0 || NGlSys > 2 ) {
				ShowFatalError( "Construction=" + DesiredConstructionName + " from the Window5 data file cannot be used: it has " + TrimSigDigits( NGlSys ) + " glazing systems; only 1 or 2 are allowed." );
			}
			{ IOFlags flags; gio::read( W5DataFileNum, fmtA, flags ) >> NextLine; ReadStat = flags.ios(); }
			if ( ReadStat < GoodIOStatValue ) goto Label1000;
			++FileLineCount;
			for ( IGlSys = 1; IGlSys <= NGlSys; ++IGlSys ) {
				{ IOFlags flags; gio::read( W5DataFileNum, fmtA, flags ) >> NextLine; ReadStat = flags.ios(); }
				if ( ReadStat < GoodIOStatValue ) goto Label1000;
				++FileLineCount;
				{ IOFlags flags; gio::read( NextLine.substr( 19 ), "*", flags ) >> WinHeight( IGlSys ) >> WinWidth( IGlSys ) >> NGlass( IGlSys ) >> UValCenter( IGlSys ) >> SCCenter( IGlSys ) >> SHGCCenter( IGlSys ) >> TVisCenter( IGlSys ); ReadStat = flags.ios(); }
				if ( ReadStat != 0 ) {
					ShowSevereError( "HeatBalanceManager: SearchWindow5DataFile: Error in Read of glazing system values. For glazing system=" + TrimSigDigits( IGlSys ) );
					ShowContinueError( "Line (~" + TrimSigDigits( FileLineCount ) + ") in error (first 100 characters)=" + NextLine.substr( 0, 100 ) );
					ErrorsFound = true;
				}
				if ( WinHeight( IGlSys ) == 0.0 || WinWidth( IGlSys ) == 0.0 ) {
					ShowSevereError( "HeatBalanceManager: SearchWindow5DataFile: Construction=" + DesiredConstructionName + " from the Window5 data file cannot be used: it has window height or width = 0 for glazing system " + TrimSigDigits( IGlSys ) );
					ErrorsFound = true;
				}
				if ( NGlass( IGlSys ) <= 0 || NGlass( IGlSys ) > 4 ) {
					ShowSevereError( "HeatBalanceManager: SearchWindow5DataFile: Construction=" + DesiredConstructionName + " from the Window5 data file cannot be used: it has 0 or more than 4 glass layers in glazing system " + TrimSigDigits( IGlSys ) );
					ErrorsFound = true;
				}
				if ( UValCenter( IGlSys ) <= 0.0 ) {
					ShowSevereError( "HeatBalanceManager: SearchWindow5DataFile: Construction=" + DesiredConstructionName + " from the Window5 data file cannot be used: it has Center-of-Glass U-value <= 0 in glazing system " + TrimSigDigits( IGlSys ) );
					ErrorsFound = true;
				}
				if ( SCCenter( IGlSys ) <= 0.0 ) {
					ShowSevereError( "HeatBalanceManager: SearchWindow5DataFile: Construction=" + DesiredConstructionName + " from the Window5 data file cannot be used: it has Shading Coefficient <= 0 in glazing system " + TrimSigDigits( IGlSys ) );
					ErrorsFound = true;
				}
				if ( SHGCCenter( IGlSys ) <= 0.0 ) {
					ShowSevereError( "HeatBalanceManager: SearchWindow5DataFile: Construction=" + DesiredConstructionName + " from the Window5 data file cannot be used: it has SHGC <= 0 in glazing system " + TrimSigDigits( IGlSys ) );
					ErrorsFound = true;
				}
				WinHeight( IGlSys ) *= 0.001;
				WinWidth( IGlSys ) *= 0.001;
			}
			for ( LineNum = 1; LineNum <= 11; ++LineNum ) {
				{ IOFlags flags; gio::read( W5DataFileNum, fmtA, flags ) >> DataLine( LineNum ); ReadStat = flags.ios(); }
				if ( ReadStat == -1 ) goto Label1000;
			}

			// Mullion width and orientation
			MullionWidth = 0.0;
			MullionOrientation = "Vertical";
			if ( NGlSys == 2 ) {
				{ IOFlags flags; gio::read( DataLine( 10 ).substr( 19 ), "*", flags ) >> MullionWidth; ReadStat = flags.ios(); }
				if ( ReadStat != 0 ) {
					ShowSevereError( "HeatBalanceManager: SearchWindow5DataFile: Error in Read of Mullion Width." );
					ShowContinueError( "Line (~" + TrimSigDigits( FileLineCount + 10 ) + ") in error (first 100 characters)=" + DataLine( 10 ).substr( 0, 100 ) );
					ErrorsFound = true;
				}
				MullionWidth *= 0.001;
				{ IOFlags flags; gio::read( DataLine( 10 ).substr( 88 ), "*", flags ) >> MullionOrientation; ReadStat = flags.ios(); }
				if ( ReadStat != 0 ) {
					ShowSevereError( "HeatBalanceManager: SearchWindow5DataFile: Error in Read of Mullion Orientation." );
					ShowContinueError( "Line (~" + TrimSigDigits( FileLineCount + 10 ) + ") in error (first 100 characters)=" + DataLine( 10 ).substr( 0, 100 ) );
					ErrorsFound = true;
				}
			}

			// Frame data; if there are two glazing systems, the frame is assumed to be
			// the same for both.
			FrameWidth = 0.0;
			FrameProjectionOut = 0.0;
			FrameProjectionIn = 0.0;
			FrameConductance = 0.0;
			FrEdgeToCenterGlCondRatio = 0.0;
			FrameSolAbsorp = 0.0;
			FrameVisAbsorp = 0.0;
			FrameEmis = 0.0;
			{ IOFlags flags; gio::read( DataLine( 11 ).substr( 19 ), "*", flags ) >> FrameWidth >> FrameProjectionOut >> FrameProjectionIn >> FrameConductance >> FrEdgeToCenterGlCondRatio >> FrameSolAbsorp >> FrameVisAbsorp >> FrameEmis; ReadStat = flags.ios(); }
			if ( ReadStat != 0 ) {
				ShowSevereError( "HeatBalanceManager: SearchWindow5DataFile: Error in Read of frame data values." );
				ShowContinueError( "Line (~" + TrimSigDigits( FileLineCount + 11 ) + ") in error (first 100 characters)=" + DataLine( 11 ).substr( 0, 100 ) );
				ErrorsFound = true;
			}
			if ( FrameWidth > 0.0 ) {
				if ( FrameConductance <= 0.0 ) {
					ShowSevereError( "HeatBalanceManager: SearchWindow5DataFile: Construction=" + DesiredConstructionName + " from the Window5 data file cannot be used: it has Frame Conductance <= 0.0" );
					ErrorsFound = true;
				}
				// Relax this check for Window5 data: 1/28/2008.
				//        IF(FrEdgeToCenterGlCondRatio < 1.0) THEN
				//            CALL ShowSevereError('HeatBalanceManager: SearchWindow5DataFile: Construction='//TRIM(DesiredConstructionName)// &
				//            ' from the Window5 data file cannot be used: it has Frame Edge-of-Glass Conduction Ratio < 1.0')
				//          ErrorsFound = .TRUE.
				//        END IF
				if ( FrameSolAbsorp < 0.0 || FrameSolAbsorp > 1.0 ) {
					ShowSevereError( "HeatBalanceManager: SearchWindow5DataFile: Construction=" + DesiredConstructionName + " from the Window5 data file cannot be used: it has Frame Solar Absorptance < 0.0 or > 1.0" );
					ErrorsFound = true;
				}
				if ( FrameEmis <= 0.0 || FrameEmis >= 1.0 ) {
					ShowSevereError( "HeatBalanceManager: SearchWindow5DataFile: Construction=" + DesiredConstructionName + " from the Window5 data file cannot be used: it has Frame Emissivity <= 0.0 or >= 1.0" );
					ErrorsFound = true;
				}
			}
			FrameWidth *= 0.001;
			FrameProjectionOut *= 0.001;
			FrameProjectionIn *= 0.001;
			FileLineCount += 11;

			{ IOFlags flags; gio::read( W5DataFileNum, fmtA, flags ) >> NextLine; ReadStat = flags.ios(); }
			if ( ReadStat < GoodIOStatValue ) goto Label1000;
			++FileLineCount;

			// Divider data for each glazing system
			for ( IGlSys = 1; IGlSys <= NGlSys; ++IGlSys ) {
				{ IOFlags flags; gio::read( W5DataFileNum, fmtA, flags ) >> NextLine; ReadStat = flags.ios(); }
				if ( ReadStat < GoodIOStatValue ) goto Label1000;
				++FileLineCount;
				{ IOFlags flags; gio::read( NextLine.substr( 19 ), "*", flags ) >> DividerWidth( IGlSys ) >> DividerProjectionOut( IGlSys ) >> DividerProjectionIn( IGlSys ) >> DividerConductance( IGlSys ) >> DivEdgeToCenterGlCondRatio( IGlSys ) >> DividerSolAbsorp( IGlSys ) >> DividerVisAbsorp( IGlSys ) >> DividerEmis( IGlSys ) >> DividerType( IGlSys ) >> HorDividers( IGlSys ) >> VertDividers( IGlSys ); ReadStat = flags.ios(); }
				if ( ReadStat != 0 ) {
					ShowSevereError( "HeatBalanceManager: SearchWindow5DataFile: Error in Read of divider data values. For Glazing System=" + TrimSigDigits( IGlSys ) );
					ShowContinueError( "Line (~" + TrimSigDigits( FileLineCount + 11 ) + ") in error (first 100 characters)=" + NextLine.substr( 0, 100 ) );
					ErrorsFound = true;
				}
				uppercase( DividerType( IGlSys ) );
				if ( DividerWidth( IGlSys ) > 0.0 ) {
					if ( HorDividers( IGlSys ) == 0 && VertDividers( IGlSys ) == 0 ) {
						ShowSevereError( "HeatBalanceManager: SearchWindow5DataFile: Construction=" + DesiredConstructionName + " from the Window5 data file cannot be used:" );
						ShowContinueError( "glazing system " + TrimSigDigits( IGlSys ) + " has a divider but number of horizontal and vertical divider elements = 0" );
						ErrorsFound = true;
					}
					if ( DividerConductance( IGlSys ) <= 0.0 ) {
						ShowSevereError( "HeatBalanceManager: SearchWindow5DataFile: Construction=" + DesiredConstructionName + " from the Window5 data file cannot be used:" );
						ShowContinueError( "glazing system " + TrimSigDigits( IGlSys ) + " has Divider Conductance <= 0.0" );
						ErrorsFound = true;
					}
					if ( DivEdgeToCenterGlCondRatio( IGlSys ) < 1.0 ) {
						ShowSevereError( "HeatBalanceManager: SearchWindow5DataFile: Construction=" + DesiredConstructionName + " from the Window5 data file cannot be used:" );
						ShowContinueError( "glazing system " + TrimSigDigits( IGlSys ) + " has Divider Edge-Of-Glass Conduction Ratio < 1.0" );
						ErrorsFound = true;
					}
					if ( DividerSolAbsorp( IGlSys ) < 0.0 || DividerSolAbsorp( IGlSys ) > 1.0 ) {
						ShowSevereError( "HeatBalanceManager: SearchWindow5DataFile: Construction=" + DesiredConstructionName + " from the Window5 data file cannot be used:" );
						ShowContinueError( "glazing system " + TrimSigDigits( IGlSys ) + " has Divider Solar Absorptance < 0.0 or > 1.0" );
						ErrorsFound = true;
					}
					if ( DividerEmis( IGlSys ) <= 0.0 || DividerEmis( IGlSys ) >= 1.0 ) {
						ShowSevereError( "HeatBalanceManager: SearchWindow5DataFile: Construction=" + DesiredConstructionName + " from the Window5 data file cannot be used:" );
						ShowContinueError( "glazing system " + TrimSigDigits( IGlSys ) + " has Divider Emissivity <= 0.0 or >= 1.0" );
						ErrorsFound = true;
					}
					if ( DividerType( IGlSys ) != "DIVIDEDLITE" && DividerType( IGlSys ) != "SUSPENDED" ) {
						ShowSevereError( "HeatBalanceManager: SearchWindow5DataFile: Construction=" + DesiredConstructionName + " from the Window5 data file cannot be used:" );
						ShowContinueError( "glazing system " + TrimSigDigits( IGlSys ) + " has Divider Type = " + DividerType( IGlSys ) + "; it should be DIVIDEDLITE or SUSPENDED." );
						ErrorsFound = true;
					}
				}
				DividerWidth( IGlSys ) *= 0.001;
				if ( DividerType( IGlSys ) == "DIVIDEDLITE" ) {
					DividerProjectionOut( IGlSys ) *= 0.001;
					DividerProjectionIn( IGlSys ) *= 0.001;
				} else {
					DividerProjectionOut( IGlSys ) = 0.0;
					DividerProjectionIn( IGlSys ) = 0.0;
				}
			}

			if ( ErrorsFound ) ShowFatalError( "HeatBalanceManager: SearchWindow5DataFile: Construction=" + DesiredConstructionName + " from the Window5 data file cannot be used because of above errors" );

			TotMaterialsPrev = TotMaterials;
			for ( IGlSys = 1; IGlSys <= NGlSys; ++IGlSys ) {
				NGaps( IGlSys ) = NGlass( IGlSys ) - 1;
				TotMaterials += NGlass( IGlSys ) + NGaps( IGlSys );
			}

			// Create Material objects

			// reallocate Material type

			Material.redimension( TotMaterials );
			NominalR.redimension( TotMaterials, 0.0 );

			// Initialize new materials
			for ( loop = TotMaterialsPrev + 1; loop <= TotMaterials; ++loop ) {
				Material( loop ).Name = "";
				Material( loop ).Group = -1;
				Material( loop ).Roughness = 0;
				Material( loop ).Conductivity = 0.0;
				Material( loop ).Density = 0.0;
				Material( loop ).IsoMoistCap = 0.0;
				Material( loop ).Porosity = 0.0;
				Material( loop ).Resistance = 0.0;
				Material( loop ).SpecHeat = 0.0;
				Material( loop ).ThermGradCoef = 0.0;
				Material( loop ).Thickness = 0.0;
				Material( loop ).VaporDiffus = 0.0;
				Material( loop ).AbsorpSolar = 0.0;
				Material( loop ).AbsorpThermal = 0.0;
				Material( loop ).AbsorpVisible = 0.0;
				Material( loop ).ReflectShade = 0.0;
				Material( loop ).Trans = 0.0;
				Material( loop ).ReflectShadeVis = 0.0;
				Material( loop ).TransVis = 0.0;
				Material( loop ).GlassTransDirtFactor = 1.0;
				Material( loop ).SolarDiffusing = false;
				Material( loop ).AbsorpThermalBack = 0.0;
				Material( loop ).AbsorpThermalFront = 0.0;
				Material( loop ).ReflectSolBeamBack = 0.0;
				Material( loop ).ReflectSolBeamFront = 0.0;
				Material( loop ).ReflectSolDiffBack = 0.0;
				Material( loop ).ReflectSolDiffFront = 0.0;
				Material( loop ).ReflectVisBeamBack = 0.0;
				Material( loop ).ReflectVisBeamFront = 0.0;
				Material( loop ).ReflectVisDiffBack = 0.0;
				Material( loop ).ReflectVisDiffFront = 0.0;
				Material( loop ).TransSolBeam = 0.0;
				Material( loop ).TransThermal = 0.0;
				Material( loop ).TransVisBeam = 0.0;
				Material( loop ).GlassSpectralDataPtr = 0;
				Material( loop ).NumberOfGasesInMixture = 0;
				Material( loop ).GasCon = 0.0;
				Material( loop ).GasVis = 0.0;
				Material( loop ).GasCp = 0.0;
				Material( loop ).GasType = 0;
				Material( loop ).GasWght = 0.0;
				Material( loop ).GasSpecHeatRatio = 0.0;
				Material( loop ).GasFract = 0.0;
				Material( loop ).WinShadeToGlassDist = 0.0;
				Material( loop ).WinShadeTopOpeningMult = 0.0;
				Material( loop ).WinShadeBottomOpeningMult = 0.0;
				Material( loop ).WinShadeLeftOpeningMult = 0.0;
				Material( loop ).WinShadeRightOpeningMult = 0.0;
				Material( loop ).WinShadeAirFlowPermeability = 0.0;
				Material( loop ).BlindDataPtr = 0;
				Material( loop ).EMPDVALUE = 0.0;
				Material( loop ).MoistACoeff = 0.0;
				Material( loop ).MoistBCoeff = 0.0;
				Material( loop ).MoistCCoeff = 0.0;
				Material( loop ).MoistDCoeff = 0.0;
			}

			// Glass objects
			{ IOFlags flags; gio::read( W5DataFileNum, fmtA, flags ) >> NextLine; ReadStat = flags.ios(); }
			if ( ReadStat < GoodIOStatValue ) goto Label1000;
			++FileLineCount;
			MaterNum = TotMaterialsPrev;
			for ( IGlSys = 1; IGlSys <= NGlSys; ++IGlSys ) {
				for ( IGlass = 1; IGlass <= NGlass( IGlSys ); ++IGlass ) {
					++MaterNum;
					MaterNumSysGlass( IGlass, IGlSys ) = MaterNum;
					Material( MaterNum ).Group = WindowGlass;
					{ IOFlags flags; gio::read( W5DataFileNum, fmtA, flags ) >> NextLine; ReadStat = flags.ios(); }
					++FileLineCount;
					gio::read( NextLine.substr( 25 ), "*" ) >> Material( MaterNum ).Thickness >> Material( MaterNum ).Conductivity >> Material( MaterNum ).Trans >> Material( MaterNum ).ReflectSolBeamFront >> Material( MaterNum ).ReflectSolBeamBack >> Material( MaterNum ).TransVis >> Material( MaterNum ).ReflectVisBeamFront >> Material( MaterNum ).ReflectVisBeamBack >> Material( MaterNum ).TransThermal >> Material( MaterNum ).AbsorpThermalFront >> Material( MaterNum ).AbsorpThermalBack >> LayerName;
					Material( MaterNum ).Thickness *= 0.001;
					if ( Material( MaterNum ).Thickness <= 0.0 ) {
					}
					if ( NGlSys == 1 ) {
						Material( MaterNum ).Name = "W5:" + DesiredConstructionName + ":GLASS" + NumName( IGlass );
					} else {
						Material( MaterNum ).Name = "W5:" + DesiredConstructionName + ':' + NumName( IGlSys ) + ":GLASS" + NumName( IGlass );
					}
					Material( MaterNum ).Roughness = VerySmooth;
					Material( MaterNum ).AbsorpThermal = Material( MaterNum ).AbsorpThermalBack;
					if ( Material( MaterNum ).Thickness <= 0.0 ) {
						ShowSevereError( "SearchWindow5DataFile: Material=\"" + Material( MaterNum ).Name + "\" has thickness of 0.0.  Will be set to thickness = .001 but inaccuracies may result." );
						ShowContinueError( "Line being read=" + NextLine );
						ShowContinueError( "Thickness field starts at column 26=" + NextLine.substr( 25 ) );
						Material( MaterNum ).Thickness = 0.001;
					}
				}
			}

			// Gap objects
			{ IOFlags flags; gio::read( W5DataFileNum, fmtA, flags ) >> NextLine; ReadStat = flags.ios(); }
			if ( ReadStat < GoodIOStatValue ) goto Label1000;
			++FileLineCount;
			for ( IGlSys = 1; IGlSys <= NGlSys; ++IGlSys ) {
				for ( IGap = 1; IGap <= NGaps( IGlSys ); ++IGap ) {
					++MaterNum;
					MaterNumSysGap( IGap, IGlSys ) = MaterNum;
					{ IOFlags flags; gio::read( W5DataFileNum, fmtA, flags ) >> NextLine; ReadStat = flags.ios(); }
					++FileLineCount;
					gio::read( NextLine.substr( 23 ), "*" ) >> Material( MaterNum ).Thickness >> NumGases( IGap, IGlSys );
					if ( NGlSys == 1 ) {
						Material( MaterNum ).Name = "W5:" + DesiredConstructionName + ":GAP" + NumName( IGap );
					} else {
						Material( MaterNum ).Name = "W5:" + DesiredConstructionName + ':' + NumName( IGlSys ) + ":GAP" + NumName( IGap );
					}
					Material( MaterNum ).Thickness *= 0.001;
					Material( MaterNum ).Roughness = MediumRough; // Unused
				}
			}

			{ IOFlags flags; gio::read( W5DataFileNum, fmtA, flags ) >> NextLine; ReadStat = flags.ios(); }
			if ( ReadStat < GoodIOStatValue ) goto Label1000;
			++FileLineCount;
			for ( IGlSys = 1; IGlSys <= NGlSys; ++IGlSys ) {
				for ( IGap = 1; IGap <= NGaps( IGlSys ); ++IGap ) {
					MaterNum = MaterNumSysGap( IGap, IGlSys );
					Material( MaterNum ).NumberOfGasesInMixture = NumGases( IGap, IGlSys );
					Material( MaterNum ).Group = WindowGas;
					if ( NumGases( IGap, IGlSys ) > 1 ) Material( MaterNum ).Group = WindowGasMixture;
					for ( IGas = 1; IGas <= NumGases( IGap, IGlSys ); ++IGas ) {
						{ IOFlags flags; gio::read( W5DataFileNum, fmtA, flags ) >> NextLine; ReadStat = flags.ios(); }
						++FileLineCount;
						gio::read( NextLine.substr( 19 ), "*" ) >> GasName( IGas ) >> Material( MaterNum ).GasFract( IGas ) >> Material( MaterNum ).GasWght( IGas ) >> Material( MaterNum ).GasCon( _, IGas ) >> Material( MaterNum ).GasVis( _, IGas ) >> Material( MaterNum ).GasCp( _, IGas );
						// Nominal resistance of gap at room temperature (based on first gas in mixture)
						NominalR( MaterNum ) = Material( MaterNum ).Thickness / ( Material( MaterNum ).GasCon( 1, 1 ) + Material( MaterNum ).GasCon( 2, 1 ) * 300.0 + Material( MaterNum ).GasCon( 3, 1 ) * 90000.0 );
					}
				}
			}

			// Construction objects

			// reallocate Construct types
			TotConstructs += NGlSys;
			Construct.redimension( TotConstructs );
			NominalRforNominalUCalculation.redimension( TotConstructs );
			NominalU.redimension( TotConstructs );

			{ IOFlags flags; gio::read( W5DataFileNum, fmtA, flags ) >> NextLine; ReadStat = flags.ios(); }
			if ( ReadStat < GoodIOStatValue ) goto Label1000;
			++FileLineCount;

			for ( IGlSys = 1; IGlSys <= NGlSys; ++IGlSys ) {
				ConstrNum = TotConstructs - NGlSys + IGlSys;
				if ( IGlSys == 1 ) {
					Construct( ConstrNum ).Name = DesiredConstructionName;
				} else {
					Construct( ConstrNum ).Name = DesiredConstructionName + ":2";
				}
				for ( loop = 1; loop <= MaxLayersInConstruct; ++loop ) {
					Construct( ConstrNum ).LayerPoint( loop ) = 0;
				}
				Construct( ConstrNum ).InsideAbsorpSolar = 0.0;
				Construct( ConstrNum ).OutsideAbsorpSolar = 0.0;
				Construct( ConstrNum ).DayltPropPtr = 0;
				Construct( ConstrNum ).CTFCross = 0.0;
				Construct( ConstrNum ).CTFFlux = 0.0;
				Construct( ConstrNum ).CTFInside = 0.0;
				Construct( ConstrNum ).CTFOutside = 0.0;
				Construct( ConstrNum ).CTFSourceIn = 0.0;
				Construct( ConstrNum ).CTFSourceOut = 0.0;
				Construct( ConstrNum ).CTFTimeStep = 0.0;
				Construct( ConstrNum ).CTFTSourceOut = 0.0;
				Construct( ConstrNum ).CTFTSourceIn = 0.0;
				Construct( ConstrNum ).CTFTSourceQ = 0.0;
				Construct( ConstrNum ).CTFTUserOut = 0.0;
				Construct( ConstrNum ).CTFTUserIn = 0.0;
				Construct( ConstrNum ).CTFTUserSource = 0.0;
				Construct( ConstrNum ).NumHistories = 0;
				Construct( ConstrNum ).NumCTFTerms = 0;
				Construct( ConstrNum ).UValue = 0.0;
				Construct( ConstrNum ).SourceSinkPresent = false;
				Construct( ConstrNum ).SolutionDimensions = 0;
				Construct( ConstrNum ).SourceAfterLayer = 0;
				Construct( ConstrNum ).TempAfterLayer = 0;
				Construct( ConstrNum ).ThicknessPerpend = 0.0;
				Construct( ConstrNum ).AbsDiff = 0.0;
				Construct( ConstrNum ).AbsDiffBack = 0.0;
				Construct( ConstrNum ).AbsDiffShade = 0.0;
				Construct( ConstrNum ).AbsDiffBackShade = 0.0;
				Construct( ConstrNum ).ShadeAbsorpThermal = 0.0;
				Construct( ConstrNum ).AbsBeamCoef = 0.0;
				Construct( ConstrNum ).AbsBeamBackCoef = 0.0;
				Construct( ConstrNum ).AbsBeamShadeCoef = 0.0;
				Construct( ConstrNum ).AbsDiffIn = 0.0;
				Construct( ConstrNum ).AbsDiffOut = 0.0;
				Construct( ConstrNum ).TransDiff = 0.0;
				Construct( ConstrNum ).TransDiffVis = 0.0;
				Construct( ConstrNum ).ReflectSolDiffBack = 0.0;
				Construct( ConstrNum ).ReflectSolDiffFront = 0.0;
				Construct( ConstrNum ).ReflectVisDiffBack = 0.0;
				Construct( ConstrNum ).ReflectVisDiffFront = 0.0;
				Construct( ConstrNum ).TransSolBeamCoef = 0.0;
				Construct( ConstrNum ).TransVisBeamCoef = 0.0;
				Construct( ConstrNum ).ReflSolBeamFrontCoef = 0.0;
				Construct( ConstrNum ).ReflSolBeamBackCoef = 0.0;
				Construct( ConstrNum ).W5FrameDivider = 0;
				Construct( ConstrNum ).TotLayers = NGlass( IGlSys ) + NGaps( IGlSys );
				Construct( ConstrNum ).TotGlassLayers = NGlass( IGlSys );
				Construct( ConstrNum ).TotSolidLayers = NGlass( IGlSys );

				for ( IGlass = 1; IGlass <= NGlass( IGlSys ); ++IGlass ) {
					Construct( ConstrNum ).LayerPoint( 2 * IGlass - 1 ) = MaterNumSysGlass( IGlass, IGlSys );
					if ( IGlass < NGlass( IGlSys ) ) Construct( ConstrNum ).LayerPoint( 2 * IGlass ) = MaterNumSysGap( IGlass, IGlSys );
				}

				Construct( ConstrNum ).OutsideRoughness = VerySmooth;
				Construct( ConstrNum ).InsideAbsorpThermal = Material( TotMaterialsPrev + NGlass( IGlSys ) ).AbsorpThermalBack;
				Construct( ConstrNum ).OutsideAbsorpThermal = Material( TotMaterialsPrev + 1 ).AbsorpThermalFront;
				Construct( ConstrNum ).TypeIsWindow = true;
				Construct( ConstrNum ).FromWindow5DataFile = true;
				Construct( ConstrNum ).W5FileGlazingSysHeight = WinHeight( IGlSys );
				Construct( ConstrNum ).W5FileGlazingSysWidth = WinWidth( IGlSys );
				if ( SameString( MullionOrientation, "Vertical" ) ) {
					Construct( ConstrNum ).W5FileMullionOrientation = Vertical;
				} else if ( SameString( MullionOrientation, "Horizontal" ) ) {
					Construct( ConstrNum ).W5FileMullionOrientation = Horizontal;
				} else {
				}
				Construct( ConstrNum ).W5FileMullionWidth = MullionWidth;

				// Fill Construct with system transmission, reflection and absorption properties

				for ( IPhi = 1; IPhi <= 10; ++IPhi ) {
					CosPhiIndepVar( IPhi ) = std::cos( ( IPhi - 1 ) * 10.0 * DegToRadians );
				}

				{ IOFlags flags; gio::read( W5DataFileNum, fmtA, flags ) >> NextLine; ReadStat = flags.ios(); }
				if ( ReadStat < GoodIOStatValue ) goto Label1000;
				++FileLineCount;
				if ( IGlSys == 1 ) {
					{ IOFlags flags; gio::read( W5DataFileNum, fmtA, flags ) >> NextLine; ReadStat = flags.ios(); }
					if ( ReadStat < GoodIOStatValue ) goto Label1000;
					++FileLineCount;
				}
				{ IOFlags flags; gio::read( W5DataFileNum, fmtA, flags ) >> NextLine; ReadStat = flags.ios(); }
				if ( ReadStat < GoodIOStatValue ) goto Label1000;
				++FileLineCount;
				{ IOFlags flags; gio::read( NextLine.substr( 5 ), "*", flags ) >> Tsol; ReadStat = flags.ios(); }
				if ( ReadStat != 0 ) {
					ShowSevereError( "HeatBalanceManager: SearchWindow5DataFile: Error in Read of TSol values." );
					ShowContinueError( "Line (~" + TrimSigDigits( FileLineCount ) + ") in error (first 100 characters)=" + NextLine.substr( 0, 100 ) );
					ErrorsFound = true;
				} else if ( any_lt( Tsol, 0.0 ) || any_gt( Tsol, 1.0 ) ) {
					ShowSevereError( "HeatBalanceManager: SearchWindow5DataFile: Error in Read of TSol values. (out of range [0,1])" );
					ShowContinueError( "Line (~" + TrimSigDigits( FileLineCount ) + ") in error (first 100 characters)=" + NextLine.substr( 0, 100 ) );
					ErrorsFound = true;
				}
				for ( IGlass = 1; IGlass <= NGlass( IGlSys ); ++IGlass ) {
					{ IOFlags flags; gio::read( W5DataFileNum, fmtA, flags ) >> NextLine; ReadStat = flags.ios(); }
					++FileLineCount;
					{ IOFlags flags; gio::read( NextLine.substr( 5 ), "*", flags ) >> AbsSol( _, IGlass ); ReadStat = flags.ios(); }
					if ( ReadStat != 0 ) {
						ShowSevereError( "HeatBalanceManager: SearchWindow5DataFile: Error in Read of AbsSol values. For Glass=" + TrimSigDigits( IGlass ) );
						ShowContinueError( "Line (~" + TrimSigDigits( FileLineCount ) + ") in error (first 100 characters)=" + NextLine.substr( 0, 100 ) );
						ErrorsFound = true;
					} else if ( any_lt( AbsSol( _, IGlass ), 0.0 ) || any_gt( AbsSol( _, IGlass ), 1.0 ) ) {
						ShowSevereError( "HeatBalanceManager: SearchWindow5DataFile: Error in Read of AbsSol values. (out of range [0,1]) For Glass=" + TrimSigDigits( IGlass ) );
						ShowContinueError( "Line (~" + TrimSigDigits( FileLineCount ) + ") in error (first 100 characters)=" + NextLine.substr( 0, 100 ) );
						ErrorsFound = true;
					}
				}
				for ( ILine = 1; ILine <= 5; ++ILine ) {
					{ IOFlags flags; gio::read( W5DataFileNum, fmtA, flags ) >> DataLine( ILine ); ReadStat = flags.ios(); }
				}
				{ IOFlags flags; gio::read( DataLine( 1 ).substr( 5 ), "*", flags ) >> Rfsol; ReadStat = flags.ios(); }
				if ( ReadStat != 0 ) {
					ShowSevereError( "HeatBalanceManager: SearchWindow5DataFile: Error in Read of RfSol values." );
					ShowContinueError( "Line (~" + TrimSigDigits( FileLineCount + 1 ) + ") in error (first 100 characters)=" + DataLine( 1 ).substr( 0, 100 ) );
					ErrorsFound = true;
				} else if ( any_lt( Rfsol, 0.0 ) || any_gt( Rfsol, 1.0 ) ) {
					ShowSevereError( "HeatBalanceManager: SearchWindow5DataFile: Error in Read of RfSol values. (out of range [0,1])" );
					ShowContinueError( "Line (~" + TrimSigDigits( FileLineCount + 1 ) + ") in error (first 100 characters)=" + DataLine( 1 ).substr( 0, 100 ) );
					ErrorsFound = true;
				}
				{ IOFlags flags; gio::read( DataLine( 2 ).substr( 5 ), "*", flags ) >> Rbsol; ReadStat = flags.ios(); }
				if ( ReadStat != 0 ) {
					ShowSevereError( "HeatBalanceManager: SearchWindow5DataFile: Error in Read of RbSol values." );
					ShowContinueError( "Line (~" + TrimSigDigits( FileLineCount + 2 ) + ") in error (first 100 characters)=" + DataLine( 2 ).substr( 0, 100 ) );
					ErrorsFound = true;
				} else if ( any_lt( Rbsol, 0.0 ) || any_gt( Rbsol, 1.0 ) ) {
					ShowSevereError( "HeatBalanceManager: SearchWindow5DataFile: Error in Read of RbSol values. (out of range [0,1])" );
					ShowContinueError( "Line (~" + TrimSigDigits( FileLineCount + 2 ) + ") in error (first 100 characters)=" + DataLine( 2 ).substr( 0, 100 ) );
					ErrorsFound = true;
				}
				{ IOFlags flags; gio::read( DataLine( 3 ).substr( 5 ), "*", flags ) >> Tvis; ReadStat = flags.ios(); }
				if ( ReadStat != 0 ) {
					ShowSevereError( "HeatBalanceManager: SearchWindow5DataFile: Error in Read of Tvis values." );
					ShowContinueError( "Line (~" + TrimSigDigits( FileLineCount + 3 ) + ") in error (first 100 characters)=" + DataLine( 3 ).substr( 0, 100 ) );
					ErrorsFound = true;
				} else if ( any_lt( Tvis, 0.0 ) || any_gt( Tvis, 1.0 ) ) {
					ShowSevereError( "HeatBalanceManager: SearchWindow5DataFile: Error in Read of Tvis values. (out of range [0,1])" );
					ShowContinueError( "Line (~" + TrimSigDigits( FileLineCount + 3 ) + ") in error (first 100 characters)=" + DataLine( 3 ).substr( 0, 100 ) );
					ErrorsFound = true;
				}
				{ IOFlags flags; gio::read( DataLine( 4 ).substr( 5 ), "*", flags ) >> Rfvis; ReadStat = flags.ios(); }
				if ( ReadStat != 0 ) {
					ShowSevereError( "HeatBalanceManager: SearchWindow5DataFile: Error in Read of Rfvis values." );
					ShowContinueError( "Line (~" + TrimSigDigits( FileLineCount + 4 ) + ") in error (first 100 characters)=" + DataLine( 4 ).substr( 0, 100 ) );
					ErrorsFound = true;
				} else if ( any_lt( Rfvis, 0.0 ) || any_gt( Rfvis, 1.0 ) ) {
					ShowSevereError( "HeatBalanceManager: SearchWindow5DataFile: Error in Read of Rfvis values. (out of range [0,1])" );
					ShowContinueError( "Line (~" + TrimSigDigits( FileLineCount + 4 ) + ") in error (first 100 characters)=" + DataLine( 4 ).substr( 0, 100 ) );
					ErrorsFound = true;
				}
				{ IOFlags flags; gio::read( DataLine( 5 ).substr( 5 ), "*", flags ) >> Rbvis; ReadStat = flags.ios(); }
				if ( ReadStat != 0 ) {
					ShowSevereError( "HeatBalanceManager: SearchWindow5DataFile: Error in Read of Rbvis values." );
					ShowContinueError( "Line (~" + TrimSigDigits( FileLineCount + 5 ) + ") in error (first 100 characters)=" + DataLine( 5 ).substr( 0, 100 ) );
					ErrorsFound = true;
				} else if ( any_lt( Rbvis, 0.0 ) || any_gt( Rbvis, 1.0 ) ) {
					ShowSevereError( "HeatBalanceManager: SearchWindow5DataFile: Error in Read of Rbvis values. (out of range [0,1])" );
					ShowContinueError( "Line (~" + TrimSigDigits( FileLineCount + 5 ) + ") in error (first 100 characters)=" + DataLine( 5 ).substr( 0, 100 ) );
					ErrorsFound = true;
				}
				FileLineCount += 5;

				if ( ErrorsFound ) ShowFatalError( "HeatBalanceManager: SearchWindow5DataFile: Construction=" + DesiredConstructionName + " from the Window5 data file cannot be used because of above errors" );

				// Hemis
				Construct( ConstrNum ).TransDiff = Tsol( 11 );
				Construct( ConstrNum ).TransDiffVis = Tvis( 11 );
				Construct( ConstrNum ).ReflectSolDiffFront = Rfsol( 11 );
				Construct( ConstrNum ).ReflectSolDiffBack = Rbsol( 11 );
				Construct( ConstrNum ).ReflectVisDiffFront = Rfvis( 11 );
				Construct( ConstrNum ).ReflectVisDiffBack = Rbvis( 11 );

				W5LsqFit( CosPhiIndepVar, Tsol, 6, 1, 10, Construct( ConstrNum ).TransSolBeamCoef );
				W5LsqFit( CosPhiIndepVar, Tvis, 6, 1, 10, Construct( ConstrNum ).TransVisBeamCoef );
				W5LsqFit( CosPhiIndepVar, Rfsol, 6, 1, 10, Construct( ConstrNum ).ReflSolBeamFrontCoef );
				for ( IGlass = 1; IGlass <= NGlass( IGlSys ); ++IGlass ) {
					W5LsqFit( CosPhiIndepVar, AbsSol( _, IGlass ), 6, 1, 10, Construct( ConstrNum ).AbsBeamCoef( _, IGlass ) );
				}

				// For comparing fitted vs. input distribution in incidence angle
				for ( IPhi = 1; IPhi <= 10; ++IPhi ) {
					Phi = double( IPhi - 1 ) * 10.0;
					CosPhi = std::cos( Phi * DegToRadians );
					if ( std::abs( CosPhi ) < 0.0001 ) CosPhi = 0.0;
					tsolFit( IPhi ) = POLYF( CosPhi, Construct( ConstrNum ).TransSolBeamCoef );
					tvisFit( IPhi ) = POLYF( CosPhi, Construct( ConstrNum ).TransVisBeamCoef );
					rfsolFit( IPhi ) = POLYF( CosPhi, Construct( ConstrNum ).ReflSolBeamFrontCoef );
					for ( IGlass = 1; IGlass <= NGlass( IGlSys ); ++IGlass ) {
						solabsFit( IGlass, IPhi ) = POLYF( CosPhi, Construct( ConstrNum ).AbsBeamCoef( {1,6}, IGlass ) );
					}
				}
				// end

				// NominalRforNominalUCalculation of this construction (actually the total resistance of all of its layers; gas layer
				// conductivity here ignores convective efffects in gap.)
				NominalRforNominalUCalculation( ConstrNum ) = 0.0;
				for ( loop = 1; loop <= NGlass( IGlSys ) + NGaps( IGlSys ); ++loop ) {
					MatNum = Construct( ConstrNum ).LayerPoint( loop );
					if ( Material( MatNum ).Group == WindowGlass ) {
						NominalRforNominalUCalculation( ConstrNum ) += Material( MatNum ).Thickness / Material( MatNum ).Conductivity;
					} else if ( Material( MatNum ).Group == WindowGas || Material( MatNum ).Group == WindowGasMixture ) {
						// If mixture, use conductivity of first gas in mixture
						NominalRforNominalUCalculation( ConstrNum ) += Material( MatNum ).Thickness / ( Material( MatNum ).GasCon( 1, 1 ) + Material( MatNum ).GasCon( 2, 1 ) * 300.0 + Material( MatNum ).GasCon( 3, 1 ) * 90000.0 );
					}
				}

			} // End of loop over glazing systems

			// WindowFrameAndDivider objects

			TotFrameDividerPrev = TotFrameDivider;
			for ( IGlSys = 1; IGlSys <= NGlSys; ++IGlSys ) {
				if ( FrameWidth > 0.0 || DividerWidth( IGlSys ) > 0.0 ) {
					++TotFrameDivider;
					Construct( TotConstructs - NGlSys + IGlSys ).W5FrameDivider = TotFrameDivider;
				}
			}

			if ( TotFrameDivider > TotFrameDividerPrev ) {
				FrameDivider.redimension( TotFrameDivider );
			}

			for ( IGlSys = 1; IGlSys <= NGlSys; ++IGlSys ) {
				if ( FrameWidth > 0.0 || DividerWidth( IGlSys ) > 0.0 ) {
					FrDivNum = Construct( TotConstructs - NGlSys + IGlSys ).W5FrameDivider;
					FrameDivider( FrDivNum ).FrameWidth = FrameWidth;
					FrameDivider( FrDivNum ).FrameProjectionOut = FrameProjectionOut;
					FrameDivider( FrDivNum ).FrameProjectionIn = FrameProjectionIn;
					FrameDivider( FrDivNum ).FrameConductance = FrameConductance;
					FrameDivider( FrDivNum ).FrEdgeToCenterGlCondRatio = FrEdgeToCenterGlCondRatio;
					FrameDivider( FrDivNum ).FrameSolAbsorp = FrameSolAbsorp;
					FrameDivider( FrDivNum ).FrameVisAbsorp = FrameVisAbsorp;
					FrameDivider( FrDivNum ).FrameEmis = FrameEmis;
					FrameDivider( FrDivNum ).FrameEdgeWidth = 0.06355; // 2.5 in
					if ( SameString( MullionOrientation, "Vertical" ) ) {
						FrameDivider( FrDivNum ).MullionOrientation = Vertical;
					} else if ( SameString( MullionOrientation, "Horizontal" ) ) {
						FrameDivider( FrDivNum ).MullionOrientation = Horizontal;
					}
					if ( SameString( DividerType( IGlSys ), "DividedLite" ) ) {
						FrameDivider( FrDivNum ).DividerType = DividedLite;
					} else if ( SameString( DividerType( IGlSys ), "Suspended" ) ) {
						FrameDivider( FrDivNum ).DividerType = Suspended;
					}
					FrameDivider( FrDivNum ).DividerWidth = DividerWidth( IGlSys );
					FrameDivider( FrDivNum ).HorDividers = HorDividers( IGlSys );
					FrameDivider( FrDivNum ).VertDividers = VertDividers( IGlSys );
					FrameDivider( FrDivNum ).DividerProjectionOut = DividerProjectionOut( IGlSys );
					FrameDivider( FrDivNum ).DividerProjectionIn = DividerProjectionIn( IGlSys );
					FrameDivider( FrDivNum ).DividerConductance = DividerConductance( IGlSys );
					FrameDivider( FrDivNum ).DivEdgeToCenterGlCondRatio = DivEdgeToCenterGlCondRatio( IGlSys );
					FrameDivider( FrDivNum ).DividerSolAbsorp = DividerSolAbsorp( IGlSys );
					FrameDivider( FrDivNum ).DividerVisAbsorp = DividerVisAbsorp( IGlSys );
					FrameDivider( FrDivNum ).DividerEmis = DividerEmis( IGlSys );
					FrameDivider( FrDivNum ).DividerEdgeWidth = 0.06355; // 2.5 in
					if ( NGlSys == 1 ) {
						FrameDivider( FrDivNum ).Name = "W5:" + DesiredConstructionName;
					} else {
						FrameDivider( FrDivNum ).Name = "W5:" + DesiredConstructionName + ':' + NumName( IGlSys );
					}
				}
			}

			if ( FrameWidth > 0.0 && DividerWidth( 1 ) > 0.0 ) {
				DisplayString( "--Construction and associated frame and divider found" );
			} else if ( FrameWidth > 0.0 ) {
				DisplayString( "--Construction and associated frame found" );
			} else if ( DividerWidth( 1 ) > 0.0 ) {
				DisplayString( "--Construction and associated divider found" );
			} else {
				DisplayString( "--Construction without frame or divider found" );
			}

		}

		gio::close( W5DataFileNum );
		return;

Label999: ;
		ShowFatalError( "HeatBalanceManager: SearchWindow5DataFile: Could not open Window5 Data File, expecting it as file name=" + DesiredFileName );
		return;

Label1000: ;
		EOFonFile = true;
		gio::close( W5DataFileNum );

	}

	void
	SetStormWindowControl()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   Jan 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Sets the storm window flag for each window, which is:
		//  -1: if storm window is not applicable (this will always be the value for interior
		//        windows since storm windows can only be applied to exterior windows
		//   0: if the window has a storm window but it is off
		//   1: if the window has a storm window and it is on

		// A "storm window" is a single layer of exterior glass separated from the main window by air gap.
		// Whether the storm window is in place is determined by the following values, which
		// which are specified in the Storm Window object for the window:
		//  -Month that Storm Window Is Put On
		//  -Day of Month that Storm Window Is Put On
		//  -Month that Storm Window Is Taken Off
		//  -Day of Month that Storm Window Is Taken Off

		// REFERENCES:na
		// Using/Aliasing
		using General::BetweenDates;

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS:na

		// INTERFACE BLOCK SPECIFICATIONS:na

		// DERIVED TYPE DEFINITIONS:na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int SurfNum; // Surface number
		int StormWinNum; // Number of storm window object
		int StormWinFlag; // Storm window flag; this routine sets the following values:
		//   0: if the storm window is off this time step
		//   1: if the storm window is on this time step
		int DateOff; // Date Off for calculation

		StormWinChangeThisDay = false;

		for ( StormWinNum = 1; StormWinNum <= TotStormWin; ++StormWinNum ) {
			SurfNum = StormWindow( StormWinNum ).BaseWindowNum;
			SurfaceWindow( SurfNum ).StormWinFlagPrevDay = SurfaceWindow( SurfNum ).StormWinFlag;
			DateOff = StormWindow( StormWinNum ).DateOff - 1;
			// Note: Dateon = Dateoff is not allowed and will have produced an error in getinput.
			if ( DateOff == 0 ) DateOff = 366;
			if ( BetweenDates( DayOfYear_Schedule, StormWindow( StormWinNum ).DateOn, DateOff ) ) {
				StormWinFlag = 1;
			} else {
				StormWinFlag = 0;
			}
			SurfaceWindow( SurfNum ).StormWinFlag = StormWinFlag;
			if ( BeginSimFlag ) SurfaceWindow( SurfNum ).StormWinFlagPrevDay = StormWinFlag;
			if ( SurfaceWindow( SurfNum ).StormWinFlag != SurfaceWindow( SurfNum ).StormWinFlagPrevDay ) StormWinChangeThisDay = true;
		}

	}

	void
	CreateFCfactorConstructions(
		int & ConstrNum, // Counter for Constructions
		bool & ErrorsFound // If errors found in input
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Tianzhen Hong
		//       DATE WRITTEN   July 2009
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine goes through each construction defined with Ffactor or Cfactor method,
		// and creates a construction (concrete + insulation) used in the heat transfer calculation.
		// This subroutine only gets called once in the GetConstructionData subroutine

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataStringGlobals;
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// ASHRAE Handbook Fundamental 2005
		//Thermal resistance of the inside air film, m2.K/W. Average of 0.14 (heat flow up) and 0.11 (heat flow down)
		Real64 const Rfilm_in( 0.125 );
		//Thermal resistance of the outside air film used in calculating the Ffactor, m2.K/W. 0.17/5.678
		Real64 const Rfilm_out( 0.03 );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ConstructNumAlpha; // Number of construction alpha names being passed
		int DummyNumProp; // dummy variable for properties being passed
		int IOStat; // IO Status when calling get input subroutine
		Array1D_string ConstructAlphas( 1 ); // Construction Alpha names defined
		Array1D< Real64 > DummyProps( 4 ); // Temporary array to transfer construction properties
		bool ErrorInName;
		bool IsBlank;
		int Loop;

		int TotFfactorConstructs; // Number of slabs-on-grade or underground floor constructions defined with F factors
		int TotCfactorConstructs; // Number of underground wall constructions defined with C factors

		Real64 Ffactor; // Ffactor in W/m-K, applies to deltaT of outside - indoor air temperature
		Real64 Cfactor; // Cfactor in W/m2-K, does not include soil or air films
		Real64 Area; // floor area in m2
		Real64 PerimeterExposed; // perimeter exposed in m
		Real64 Height; // Height of the underground wall in m

		Real64 Reff; // Effective thermal resistance, m2.K/W
		Real64 Rcon; // Concrete layer thermal resistance, m2.K/W
		Real64 Rfic; // Thermal resistance of the fictitious material, m2.K/W
		int MaterNum; // Material index
		Real64 Rsoilequ; // Effective R-value of soil for underground walls
		int iFCConcreteLayer; // Layer pointer to the materials array

		// First get the concrete layer
		iFCConcreteLayer = FindItemInList( "~FC_Concrete", Material );
		Rcon = Material( iFCConcreteLayer ).Resistance;

		// Count number of constructions defined with Ffactor or Cfactor method
		TotFfactorConstructs = GetNumObjectsFound( "Construction:FfactorGroundFloor" );
		TotCfactorConstructs = GetNumObjectsFound( "Construction:CfactorUndergroundWall" );

		// First create ground floor constructions defined with F factor method if any
		CurrentModuleObject = "Construction:FfactorGroundFloor";

		// Loop through all constructs defined with Ffactor method
		for ( Loop = 1; Loop <= TotFfactorConstructs; ++Loop ) {

			// Get the object names for each construction from the input processor
			GetObjectItem( CurrentModuleObject, Loop, ConstructAlphas, ConstructNumAlpha, DummyProps, DummyNumProp, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			ErrorInName = false;
			IsBlank = false;
			VerifyName( ConstructAlphas( 1 ), Construct, ConstrNum, ErrorInName, IsBlank, CurrentModuleObject + " Name" );
			if ( ErrorInName ) {
				ErrorsFound = true;
				continue;
			}

			++ConstrNum;

			Construct( ConstrNum ).Name = ConstructAlphas( 1 );
			Construct( ConstrNum ).TypeIsFfactorFloor = true;

			Ffactor = DummyProps( 1 );
			Area = DummyProps( 2 );
			PerimeterExposed = DummyProps( 3 );

			Construct( ConstrNum ).Area = Area;
			Construct( ConstrNum ).PerimeterExposed = PerimeterExposed;
			Construct( ConstrNum ).FFactor = Ffactor;

			if ( Ffactor <= 0.0 ) {
				ShowSevereError( CurrentModuleObject + "=\"" + ConstructAlphas( 1 ) + "\" has " + cNumericFieldNames( 1 ) + " <= 0.0, must be > 0.0." );
				ShowContinueError( "Entered value=[" + RoundSigDigits( Ffactor, 2 ) + ']' );
				ErrorsFound = true;
			}

			if ( Area <= 0.0 ) {
				ShowSevereError( CurrentModuleObject + "=\"" + ConstructAlphas( 1 ) + "\" has " + cNumericFieldNames( 2 ) + " <= 0.0, must be > 0.0." );
				ShowContinueError( "Entered value=[" + RoundSigDigits( Area, 2 ) + ']' );
				ErrorsFound = true;
			}

			if ( PerimeterExposed < 0.0 ) {
				ShowSevereError( CurrentModuleObject + "=\"" + ConstructAlphas( 1 ) + "\" has " + cNumericFieldNames( 3 ) + " <= 0.0, must be > 0.0." );
				ShowContinueError( "Entered value=[" + RoundSigDigits( PerimeterExposed, 2 ) + ']' );
				ErrorsFound = true;
			}

			// The construction has two layers which have been created in GetMaterialData
			Construct( ConstrNum ).TotLayers = 2;

			// The concrete is the inside layer
			Construct( ConstrNum ).LayerPoint( 2 ) = iFCConcreteLayer;

			// The fictitious insulation is the outside layer
			MaterNum = FindItemInList( "~FC_Insulation_" + RoundSigDigits( Loop ), Material );
			Construct( ConstrNum ).LayerPoint( 1 ) = MaterNum;

			// Calculate the thermal resistance of the fictitious insulation layer
			// effective thermal resistance excludes inside and outside air films
			if ( PerimeterExposed > 0.0 ) {
				Reff = Area / ( PerimeterExposed * Ffactor ) - Rfilm_in - Rfilm_out;
			} else { // PerimeterExposed = 0 for underground floor, assume R-1000 (IP)
				Reff = 177.0;
			}

			Rfic = Reff - Rcon;
			if ( Rfic <= 0.0 ) {
				ShowSevereError( CurrentModuleObject + "=\"" + ConstructAlphas( 1 ) + "\" has calculated R value <= 0.0, must be > 0.0." );
				ShowContinueError( "Calculated value=[" + RoundSigDigits( Rfic, 2 ) + "] Check definition." );
				ErrorsFound = true;
			}

			Material( MaterNum ).Resistance = Rfic;
			NominalR( MaterNum ) = Rfic;

			//excluding thermal resistance of inside or outside air film
			// 1/Reff gets reported as the "U-Factor no Film" in the summary report Envelope Summary | Opaque Exterior
			NominalRforNominalUCalculation( ConstrNum ) = Reff;
		}

		// Then create underground wall constructions defined with C factor method if any
		CurrentModuleObject = "Construction:CfactorUndergroundWall";

		for ( Loop = 1; Loop <= TotCfactorConstructs; ++Loop ) { // Loop through all constructs defined with Ffactor method

			//Get the object names for each construction from the input processor
			GetObjectItem( CurrentModuleObject, Loop, ConstructAlphas, ConstructNumAlpha, DummyProps, DummyNumProp, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			ErrorInName = false;
			IsBlank = false;
			VerifyName( ConstructAlphas( 1 ), Construct, ConstrNum, ErrorInName, IsBlank, CurrentModuleObject + " Name" );
			if ( ErrorInName ) {
				ErrorsFound = true;
				continue;
			}

			++ConstrNum;

			Construct( ConstrNum ).Name = ConstructAlphas( 1 );
			Construct( ConstrNum ).TypeIsCfactorWall = true;

			Cfactor = DummyProps( 1 );
			Height = DummyProps( 2 );

			Construct( ConstrNum ).Height = Height;
			Construct( ConstrNum ).CFactor = Cfactor;

			if ( Cfactor <= 0.0 ) {
				ShowSevereError( CurrentModuleObject + ' ' + ConstructAlphas( 1 ) + " has " + cNumericFieldNames( 1 ) + " <= 0.0, must be > 0.0." );
				ShowContinueError( "Entered value=[" + RoundSigDigits( Cfactor, 2 ) + ']' );
				ErrorsFound = true;
			}

			if ( Height <= 0.0 ) {
				ShowSevereError( CurrentModuleObject + ' ' + ConstructAlphas( 1 ) + " has " + cNumericFieldNames( 2 ) + " <= 0.0, must be > 0.0." );
				ShowContinueError( "Entered value=[" + RoundSigDigits( Height, 2 ) + ']' );
				ErrorsFound = true;
			}

			// The construction has two layers which have been created in GetMaterialData
			Construct( ConstrNum ).TotLayers = 2;

			// The concrete is the inside layer
			Construct( ConstrNum ).LayerPoint( 2 ) = iFCConcreteLayer;

			// The fictitious insulation is the outside layer
			MaterNum = FindItemInList( "~FC_Insulation_" + RoundSigDigits( Loop + TotFfactorConstructs ), Material );
			Construct( ConstrNum ).LayerPoint( 1 ) = MaterNum;

			// CR 8886 Rsoil should be in SI unit. From ASHRAE 90.1-2010 SI
			if ( Height <= 0.25 ) {
				Rsoilequ = 0.12; //m2K/W
			} else if ( Height >= 2.5 ) {
				Rsoilequ = 0.92;
			} else { // regression from ASHRAE 90.1-2010 SI TABLE C6.10.1 Effective R-Value of Soil, R2 = 0.9967
				Rsoilequ = 0.0607 + 0.3479 * Height;
			}

			// effective thermal resistance excludes inside and outside air films
			Reff = 1.0 / Cfactor + Rsoilequ; // Cfactor does not include air films

			Rfic = Reff - Rcon;
			if ( Rfic <= 0 ) {
				ShowSevereError( CurrentModuleObject + "=\"" + ConstructAlphas( 1 ) + "\" has calculated R value <= 0.0, must be > 0.0." );
				ShowContinueError( "Calculated value=[" + RoundSigDigits( Rfic, 2 ) + "] Check definition." );
				ErrorsFound = true;
			}

			Material( MaterNum ).Resistance = Rfic;
			NominalR( MaterNum ) = Rfic;

			//Reff includes the wall itself and soil, but excluding thermal resistance of inside or outside air film
			// 1/Reff gets reported as the "U-Factor no Film" in the summary report Envelope Summary | Opaque Exterior
			NominalRforNominalUCalculation( ConstrNum ) = Reff;
		}

	}

	void
	GetScheduledSurfaceGains( bool & ErrorsFound ) // If errors found in input
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Simon Vidanovic
		//       DATE WRITTEN   June 2013
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Loads scheduled surface gains for solar incident on interior side of the surfaces and absorbed solar energy in
		// window layers

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using InputProcessor::GetObjectDefMaxArgs;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using General::TrimSigDigits;
		using DataSurfaces::TotSurfaces;
		using DataSurfaces::Surface;
		using DataSurfaces::TotSurfIncSolSSG;
		using DataSurfaces::SurfIncSolSSG;
		using DataSurfaces::TotFenLayAbsSSG;
		using DataSurfaces::FenLayAbsSSG;
		using DataHeatBalance::Construct;
		using DataHeatBalance::TotConstructs;
		using ScheduleManager::GetScheduleIndex;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetScheduledSurfaceGains: " );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumArgs;
		int NumAlpha;
		int NumNumeric;
		int Loop;
		bool ErrorInName;
		bool IsBlank;
		int IOStat;
		int SurfNum;
		int ConstrNum;
		int ScheduleNum;
		int i;
		int NumOfScheduledLayers;
		bool NumOfLayersMatch;
		int iZone;

		//-----------------------------------------------------------------------
		//                SurfaceProperty:SolarIncidentInside
		//-----------------------------------------------------------------------
		cCurrentModuleObject = "SurfaceProperty:SolarIncidentInside";

		// Check if IDD definition is correct
		GetObjectDefMaxArgs( cCurrentModuleObject, NumArgs, NumAlpha, NumNumeric );
		if ( NumAlpha != 4 ) {
			ShowSevereError( RoutineName + cCurrentModuleObject + ": Object Definition indicates not = 4 Alpha Objects, Number Indicated=" + TrimSigDigits( NumAlpha ) );
			ErrorsFound = true;
		}

		TotSurfIncSolSSG = GetNumObjectsFound( cCurrentModuleObject );
		if ( TotSurfIncSolSSG > 0 ) {
			if ( ! allocated( SurfIncSolSSG ) ) {
				SurfIncSolSSG.allocate( TotSurfIncSolSSG );
			}

			for ( Loop = 1; Loop <= TotSurfIncSolSSG; ++Loop ) {
				GetObjectItem( cCurrentModuleObject, Loop, cAlphaArgs, NumAlpha, rNumericArgs, NumNumeric, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
				ErrorInName = false;
				IsBlank = false;
				VerifyName( cAlphaArgs( 1 ), SurfIncSolSSG, Loop, ErrorInName, IsBlank, cCurrentModuleObject + " Name" );
				if ( ErrorInName ) {
					ShowContinueError( "...each SurfaceProperty:SolarIncidentInside name must not duplicate other SurfaceProperty:SolarIncidentInside name" );
					ErrorsFound = true;
					continue;
				}

				SurfIncSolSSG( Loop ).Name = cAlphaArgs( 1 );

				// Assign surface number
				SurfNum = FindItemInList( cAlphaArgs( 2 ), Surface );
				if ( SurfNum == 0 ) {
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", object. Illegal value for " + cAlphaFieldNames( 2 ) + " has been found." );
					ShowContinueError( cAlphaFieldNames( 2 ) + " entered value = \"" + cAlphaArgs( 2 ) + "\" no corresponding surface (ref BuildingSurface:Detailed) has been found in the input file." );
					ErrorsFound = true;
				} else {
					SurfIncSolSSG( Loop ).SurfPtr = SurfNum;
				}

				// Assign construction number
				ConstrNum = FindItemInList( cAlphaArgs( 3 ), Construct );
				if ( ConstrNum == 0 ) {
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", object. Illegal value for " + cAlphaFieldNames( 3 ) + " has been found." );
					ShowContinueError( cAlphaFieldNames( 3 ) + " entered value = \"" + cAlphaArgs( 3 ) + "\" no corresponding construction (ref Construction) has been found in the input file." );
					ErrorsFound = true;
				} else {
					SurfIncSolSSG( Loop ).ConstrPtr = ConstrNum;
				}

				// Assign schedule number
				ScheduleNum = GetScheduleIndex( cAlphaArgs( 4 ) );
				if ( ScheduleNum == 0 ) {
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", object. Illegal value for " + cAlphaFieldNames( 4 ) + " has been found." );
					ShowContinueError( cAlphaFieldNames( 4 ) + " entered value = \"" + cAlphaArgs( 4 ) + "\" no corresponding schedule has been found in the input file." );
					ErrorsFound = true;
				} else {
					SurfIncSolSSG( Loop ).SchedPtr = ScheduleNum;
				}
			}
		}

		//-----------------------------------------------------------------------
		//                SurfaceProperty:SolarIncidentInside
		//-----------------------------------------------------------------------
		cCurrentModuleObject = "ComplexFenestrationProperty:SolarAbsorbedLayers";

		TotFenLayAbsSSG = GetNumObjectsFound( cCurrentModuleObject );
		if ( TotFenLayAbsSSG > 0 ) {
			if ( ! allocated( FenLayAbsSSG ) ) {
				FenLayAbsSSG.allocate( TotFenLayAbsSSG );
			}

			for ( Loop = 1; Loop <= TotFenLayAbsSSG; ++Loop ) {
				GetObjectItem( cCurrentModuleObject, Loop, cAlphaArgs, NumAlpha, rNumericArgs, NumNumeric, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
				ErrorInName = false;
				IsBlank = false;
				VerifyName( cAlphaArgs( 1 ), FenLayAbsSSG, Loop, ErrorInName, IsBlank, cCurrentModuleObject + " Name" );
				if ( ErrorInName ) {
					ShowContinueError( "...each ComplexFenestrationProperty:SolarAbsorbedLayers name must not duplicate other ComplexFenestrationProperty:SolarAbsorbedLayers name" );
					ErrorsFound = true;
					continue;
				}

				FenLayAbsSSG( Loop ).Name = cAlphaArgs( 1 );

				// Assign surface number
				SurfNum = FindItemInList( cAlphaArgs( 2 ), Surface );
				if ( SurfNum == 0 ) {
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", object. Illegal value for " + cAlphaFieldNames( 2 ) + " has been found." );
					ShowContinueError( cAlphaFieldNames( 2 ) + " entered value = \"" + cAlphaArgs( 2 ) + "\" no corresponding surface (ref BuildingSurface:Detailed) has been found in the input file." );
					ErrorsFound = true;
				} else {
					FenLayAbsSSG( Loop ).SurfPtr = SurfNum;
				}

				// Assign construction number
				ConstrNum = FindItemInList( cAlphaArgs( 3 ), Construct );
				if ( ConstrNum == 0 ) {
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", object. Illegal value for " + cAlphaFieldNames( 3 ) + " has been found." );
					ShowContinueError( cAlphaFieldNames( 3 ) + " entered value = \"" + cAlphaArgs( 3 ) + "\" no corresponding construction (ref Construction) has been found in the input file." );
					ErrorsFound = true;
				} else {
					FenLayAbsSSG( Loop ).ConstrPtr = ConstrNum;
					NumOfScheduledLayers = NumAlpha - 3;
					NumOfLayersMatch = false;
					// Check if number of layers in construction matches number of layers in schedule surface gains object
					if ( NumOfScheduledLayers == Construct( ConstrNum ).TotSolidLayers ) {
						NumOfLayersMatch = true;
					}

					if ( ! NumOfLayersMatch ) {
						ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", object. Number of scheduled surface gains for each layer does not match number of layers in referenced construction." );
						ShowContinueError( cAlphaArgs( 1 ) + " have " + TrimSigDigits( NumOfScheduledLayers ) + " scheduled layers and " + cAlphaArgs( 3 ) + " have " + TrimSigDigits( Construct( ConstrNum ).TotSolidLayers ) + " layers." );
						ErrorsFound = true;
					}

					if ( ! allocated( FenLayAbsSSG( Loop ).SchedPtrs ) ) {
						FenLayAbsSSG( Loop ).SchedPtrs.allocate( NumOfScheduledLayers );
					}

					FenLayAbsSSG( Loop ).NumOfSched = NumOfScheduledLayers;

					for ( i = 1; i <= NumOfScheduledLayers; ++i ) {
						ScheduleNum = GetScheduleIndex( cAlphaArgs( i + 3 ) );
						if ( ScheduleNum == 0 ) {
							ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", object. Illegal value for " + cAlphaFieldNames( NumOfScheduledLayers + 3 ) + " has been found." );
							ShowContinueError( cAlphaFieldNames( NumOfScheduledLayers + 3 ) + " entered value = \"" + cAlphaArgs( NumOfScheduledLayers + 3 ) + "\" no corresponding schedule has been found in the input file." );
							ErrorsFound = true;
						} else {
							FenLayAbsSSG( Loop ).SchedPtrs( i ) = ScheduleNum;
						}
					}
				}
			}
		}

		// Check if scheduled surface gains are assigined to each surface in every zone.  If not then warning message to user will be
		// issued
		if ( ( TotSurfIncSolSSG > 0 ) || ( TotFenLayAbsSSG > 0 ) ) {
			for ( iZone = 1; iZone <= NumOfZones; ++iZone ) {
				CheckScheduledSurfaceGains( iZone );
			}
		}

	}

	void
	CheckScheduledSurfaceGains( int const ZoneNum ) // Zone number for which error check will be performed
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Simon Vidanovic
		//       DATE WRITTEN   July 2013
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Check if all surfaces within zone are scheduled with surface gains. If not all surfaces within zone are scheduled,
		// warning message will be issued and program will continue to execute.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using SolarShading::WindowScheduledSolarAbs;
		using SolarShading::SurfaceScheduledSolarInc;
		using namespace DataSurfaces;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int iSurf;
		int iConst;
		int SchedPtr; // scheduled surface gains pointer
		bool ZoneUnscheduled; // true if all surfaces in the zone are unscheduled
		bool ZoneScheduled; // true if all surfaces in the zone are scheduled

		ZoneUnscheduled = false;
		ZoneScheduled = false;

		for ( iSurf = Zone( ZoneNum ).SurfaceFirst; iSurf <= Zone( ZoneNum ).SurfaceLast; ++iSurf ) {
			iConst = Surface( iSurf ).Construction;
			if ( Surface( iSurf ).Class == SurfaceClass_Window ) {
				SchedPtr = WindowScheduledSolarAbs( iSurf, iConst );
			} else {
				SchedPtr = SurfaceScheduledSolarInc( iSurf, iConst );
			}
			if ( iSurf == Zone( ZoneNum ).SurfaceFirst ) {
				if ( SchedPtr != 0 ) {
					ZoneScheduled = true;
					ZoneUnscheduled = false;
				} else {
					ZoneScheduled = false;
					ZoneUnscheduled = true;
				}
			} else {
				if ( SchedPtr != 0 ) {
					ZoneUnscheduled = false;
				} else {
					ZoneScheduled = false;
				}
			}

			if ( ( ! ZoneScheduled ) && ( ! ZoneUnscheduled ) ) {
				// zone is nor scheduled nor unscheduled
				ShowWarningError( "Zone " + Zone( ZoneNum ).Name + " does not have all surfaces scheduled with surface gains." );
				ShowContinueError( "If at least one surface in the zone is scheduled with surface gains, then all other surfaces within the same zone should be scheduled as well." );
				break;
			}

		}

		if ( ( ! ZoneScheduled ) && ( ! ZoneUnscheduled ) ) {
			for ( iSurf = Zone( ZoneNum ).SurfaceFirst; iSurf <= Zone( ZoneNum ).SurfaceLast; ++iSurf ) {
				iConst = Surface( iSurf ).Construction;
				if ( Surface( iSurf ).Class == SurfaceClass_Window ) {
					SchedPtr = WindowScheduledSolarAbs( iSurf, iConst );
				} else {
					SchedPtr = SurfaceScheduledSolarInc( iSurf, iConst );
				}

				if ( SchedPtr == 0 ) {
					ShowContinueError( "Surface " + Surface( iSurf ).Name + " does not have scheduled surface gains." );
				}
			}
		}

	}

	void
	CreateTCConstructions( bool & EP_UNUSED( ErrorsFound ) ) // If errors found in input
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Tianzhen Hong
		//       DATE WRITTEN   January 2009
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine goes through each TC master construction and creates a complete series
		// of the slave thermochromic constructions.
		// This subroutine only gets called once in the GetHeatBalanceInput subroutine
		//  after materials, constructions and building geometry data are read.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Using/Aliasing
		using namespace DataStringGlobals;
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

		int Loop;
		static int iTC( 0 );
		static int iMat( 0 );
		static int NumNewConst( 0 );
		static int iTCG( 0 );

		NumNewConst = 0;
		for ( Loop = 1; Loop <= TotConstructs; ++Loop ) {
			if ( Construct( Loop ).TCFlag == 1 ) {
				iTCG = Material( Construct( Loop ).TCLayer ).TCParent;
				if ( iTCG == 0 ) continue; // hope this was caught already
				iMat = TCGlazings( iTCG ).NumGlzMat;
				for ( iTC = 1; iTC <= iMat; ++iTC ) {
					++NumNewConst;
				}
			}
		}

		if ( NumNewConst == 0 ) return; // no need to go further

		// Increase Construct() and copy the extra constructions
		Construct.redimension( TotConstructs + NumNewConst );
		NominalRforNominalUCalculation.redimension( TotConstructs + NumNewConst );
		NominalU.redimension( TotConstructs + NumNewConst );

		NumNewConst = TotConstructs;
		for ( Loop = 1; Loop <= TotConstructs; ++Loop ) {
			if ( Construct( Loop ).TCFlag == 1 ) {
				iTCG = Material( Construct( Loop ).TCLayer ).TCParent;
				if ( iTCG == 0 ) continue; // hope this was caught already
				iMat = TCGlazings( iTCG ).NumGlzMat;
				for ( iTC = 1; iTC <= iMat; ++iTC ) {
					++NumNewConst;
					Construct( NumNewConst ) = Construct( Loop ); // copy data
					Construct( NumNewConst ).Name = Construct( Loop ).Name + "_TC_" + RoundSigDigits( TCGlazings( iTCG ).SpecTemp( iTC ), 0 );
					Construct( NumNewConst ).TCLayer = TCGlazings( iTCG ).LayerPoint( iTC );
					Construct( NumNewConst ).LayerPoint( Construct( Loop ).TCLayerID ) = Construct( NumNewConst ).TCLayer;
					Construct( NumNewConst ).TCFlag = 1;
					Construct( NumNewConst ).TCMasterConst = Loop;
					Construct( NumNewConst ).TCLayerID = Construct( Loop ).TCLayerID;
					Construct( NumNewConst ).TCGlassID = Construct( Loop ).TCGlassID;
					Construct( NumNewConst ).TypeIsWindow = true;
				}
			}
		}
		TotConstructs = NumNewConst;

	}

	void
	SetupSimpleWindowGlazingSystem( int & MaterNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   January 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Convert simple window performance indices into all the properties needed to
		// describe a single, equivalent glass layer

		// METHODOLOGY EMPLOYED:
		// The simple window indices are converted to a single materal layer using a "block model"

		// REFERENCES:
		// draft paper by Arasteh, Kohler, and Griffith

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
		static Real64 Riw( 0.0 ); // thermal resistance of interior film coefficient under winter conditions (m2-K/W)
		static Real64 Row( 0.0 ); // theraml resistance of exterior film coefficient under winter conditions (m2-K/W)
		static Real64 Rlw( 0.0 ); // thermal resistance of block model layer (m2-K/W)
		static Real64 Ris( 0.0 ); // thermal resistance of interior film coefficient under summer conditions (m2-K/W)
		static Real64 Ros( 0.0 ); // theraml resistance of exterior film coefficient under summer conditions (m2-K/W)
		static Real64 InflowFraction( 0.0 ); // inward flowing fraction for SHGC, intermediate value non dimensional
		static Real64 SolarAbsorb( 0.0 ); // solar aborptance
		static bool ErrorsFound( false );
		static Real64 TsolLowSide( 0.0 ); // intermediate solar transmission for interpolating
		static Real64 TsolHiSide( 0.0 ); // intermediate solar transmission for interpolating
		static Real64 DeltaSHGCandTsol( 0.0 ); // intermediate difference
		static Real64 RLowSide( 0.0 );
		static Real64 RHiSide( 0.0 );

		// first fill out defaults
		Material( MaterNum ).GlassSpectralDataPtr = 0;
		Material( MaterNum ).SolarDiffusing = false;
		Material( MaterNum ).Roughness = VerySmooth;
		Material( MaterNum ).TransThermal = 0.0;
		Material( MaterNum ).AbsorpThermalBack = 0.84;
		Material( MaterNum ).AbsorpThermalFront = 0.84;
		Material( MaterNum ).AbsorpThermal = Material( MaterNum ).AbsorpThermalBack;

		// step 1. Determine U-factor without film coefficients
		// Simple window model has its own correlation for film coefficients (m2-K/W) under Winter conditions as function of U-factor
		if ( Material( MaterNum ).SimpleWindowUfactor < 5.85 ) {
			Riw = 1.0 / ( 0.359073 * std::log( Material( MaterNum ).SimpleWindowUfactor ) + 6.949915 );
		} else {
			Riw = 1.0 / ( 1.788041 * Material( MaterNum ).SimpleWindowUfactor - 2.886625 );
		}
		Row = 1.0 / ( 0.025342 * Material( MaterNum ).SimpleWindowUfactor + 29.163853 );

		// determine 1/U without film coefficients
		Rlw = ( 1.0 / Material( MaterNum ).SimpleWindowUfactor ) - Riw - Row;
		if ( Rlw <= 0.0 ) { // U factor of film coefficients is better than user input.
			Rlw = max( Rlw, 0.001 );
			ShowWarningError( "WindowMaterial:SimpleGlazingSystem: " + Material( MaterNum ).Name + " has U-factor higher than that provided by surface film resistances, Check value of U-factor" );
		}

		// Step 2. determine layer thickness.

		if ( ( 1.0 / Rlw ) > 7.0 ) {
			Material( MaterNum ).Thickness = 0.002;
		} else {
			Material( MaterNum ).Thickness = 0.05914 - ( 0.00714 / Rlw );
		}

		// Step 3. determine effective conductivity

		Material( MaterNum ).Conductivity = Material( MaterNum ).Thickness / Rlw;
		if ( Material( MaterNum ).Conductivity > 0.0 ) {
			NominalR( MaterNum ) = Rlw;
			Material( MaterNum ).Resistance = Rlw;
		} else {
			ErrorsFound = true;
			ShowSevereError( "WindowMaterial:SimpleGlazingSystem: " + Material( MaterNum ).Name + " has Conductivity <= 0.0, must be >0.0, Check value of U-factor" );
		}

		//step 4. determine solar transmission (revised to 10-1-2009 version from LBNL.)

		if ( Material( MaterNum ).SimpleWindowUfactor > 4.5 ) {

			if ( Material( MaterNum ).SimpleWindowSHGC < 0.7206 ) {

				Material( MaterNum ).Trans = 0.939998 * pow_2( Material( MaterNum ).SimpleWindowSHGC ) + 0.20332 * Material( MaterNum ).SimpleWindowSHGC;
			} else { // >= 0.7206

				Material( MaterNum ).Trans = 1.30415 * Material( MaterNum ).SimpleWindowSHGC - 0.30515;

			}

		} else if ( Material( MaterNum ).SimpleWindowUfactor < 3.4 ) {

			if ( Material( MaterNum ).SimpleWindowSHGC <= 0.15 ) {
				Material( MaterNum ).Trans = 0.41040 * Material( MaterNum ).SimpleWindowSHGC;
			} else { // > 0.15
				Material( MaterNum ).Trans = 0.085775 * pow_2( Material( MaterNum ).SimpleWindowSHGC ) + 0.963954 * Material( MaterNum ).SimpleWindowSHGC - 0.084958;
			}
		} else { // interpolate. 3.4 <= Ufactor <= 4.5

			if ( Material( MaterNum ).SimpleWindowSHGC < 0.7206 ) {
				TsolHiSide = 0.939998 * pow_2( Material( MaterNum ).SimpleWindowSHGC ) + 0.20332 * Material( MaterNum ).SimpleWindowSHGC;
			} else { // >= 0.7206
				TsolHiSide = 1.30415 * Material( MaterNum ).SimpleWindowSHGC - 0.30515;
			}

			if ( Material( MaterNum ).SimpleWindowSHGC <= 0.15 ) {
				TsolLowSide = 0.41040 * Material( MaterNum ).SimpleWindowSHGC;
			} else { // > 0.15
				TsolLowSide = 0.085775 * pow_2( Material( MaterNum ).SimpleWindowSHGC ) + 0.963954 * Material( MaterNum ).SimpleWindowSHGC - 0.084958;
			}

			Material( MaterNum ).Trans = ( ( Material( MaterNum ).SimpleWindowUfactor - 3.4 ) / ( 4.5 - 3.4 ) ) * ( TsolHiSide - TsolLowSide ) + TsolLowSide;

		}
		if ( Material( MaterNum ).Trans < 0.0 ) Material( MaterNum ).Trans = 0.0;

		//step 5.  determine solar reflectances

		DeltaSHGCandTsol = Material( MaterNum ).SimpleWindowSHGC - Material( MaterNum ).Trans;

		if ( Material( MaterNum ).SimpleWindowUfactor > 4.5 ) {

			Ris = 1.0 / ( 29.436546 * pow_3( DeltaSHGCandTsol ) - 21.943415 * pow_2( DeltaSHGCandTsol ) + 9.945872 * DeltaSHGCandTsol + 7.426151 );
			Ros = 1.0 / ( 2.225824 * DeltaSHGCandTsol + 20.577080 );
		} else if ( Material( MaterNum ).SimpleWindowUfactor < 3.4 ) {

			Ris = 1.0 / ( 199.8208128 * pow_3( DeltaSHGCandTsol ) - 90.639733 * pow_2( DeltaSHGCandTsol ) + 19.737055 * DeltaSHGCandTsol + 6.766575 );
			Ros = 1.0 / ( 5.763355 * DeltaSHGCandTsol + 20.541528 );
		} else { // interpolate. 3.4 <= Ufactor <= 4.5
			//inside first
			RLowSide = 1.0 / ( 199.8208128 * pow_3( DeltaSHGCandTsol ) - 90.639733 * pow_2( DeltaSHGCandTsol ) + 19.737055 * DeltaSHGCandTsol + 6.766575 );
			RHiSide = 1.0 / ( 29.436546 * pow_3( DeltaSHGCandTsol ) - 21.943415 * pow_2( DeltaSHGCandTsol ) + 9.945872 * DeltaSHGCandTsol + 7.426151 );
			Ris = ( ( Material( MaterNum ).SimpleWindowUfactor - 3.4 ) / ( 4.5 - 3.4 ) ) * ( RLowSide - RHiSide ) + RLowSide;
			// then outside
			RLowSide = 1.0 / ( 5.763355 * DeltaSHGCandTsol + 20.541528 );
			RHiSide = 1.0 / ( 2.225824 * DeltaSHGCandTsol + 20.577080 );
			Ros = ( ( Material( MaterNum ).SimpleWindowUfactor - 3.4 ) / ( 4.5 - 3.4 ) ) * ( RLowSide - RHiSide ) + RLowSide;

		}

		InflowFraction = ( Ros + 0.5 * Rlw ) / ( Ros + Rlw + Ris );

		SolarAbsorb = ( Material( MaterNum ).SimpleWindowSHGC - Material( MaterNum ).Trans ) / InflowFraction;
		Material( MaterNum ).ReflectSolBeamBack = 1.0 - Material( MaterNum ).Trans - SolarAbsorb;
		Material( MaterNum ).ReflectSolBeamFront = Material( MaterNum ).ReflectSolBeamBack;

		//step 6. determine visible properties.
		if ( Material( MaterNum ).SimpleWindowVTinputByUser ) {
			Material( MaterNum ).TransVis = Material( MaterNum ).SimpleWindowVisTran;
			Material( MaterNum ).ReflectVisBeamBack = -0.7409 * pow_3( Material( MaterNum ).TransVis ) + 1.6531 * pow_2( Material( MaterNum ).TransVis ) - 1.2299 * Material( MaterNum ).TransVis + 0.4545;
			if ( Material( MaterNum ).TransVis + Material( MaterNum ).ReflectVisBeamBack >= 1.0 ) {
				Material( MaterNum ).ReflectVisBeamBack = 0.999 - Material( MaterNum ).TransVis;
			}

			Material( MaterNum ).ReflectVisBeamFront = -0.0622 * pow_3( Material( MaterNum ).TransVis ) + 0.4277 * pow_2( Material( MaterNum ).TransVis ) - 0.4169 * Material( MaterNum ).TransVis + 0.2399;
			if ( Material( MaterNum ).TransVis + Material( MaterNum ).ReflectVisBeamFront >= 1.0 ) {
				Material( MaterNum ).ReflectVisBeamFront = 0.999 - Material( MaterNum ).TransVis;
			}
		} else {
			Material( MaterNum ).TransVis = Material( MaterNum ).Trans;
			Material( MaterNum ).ReflectVisBeamBack = Material( MaterNum ).ReflectSolBeamBack;
			Material( MaterNum ).ReflectVisBeamFront = Material( MaterNum ).ReflectSolBeamFront;
		}

		//step 7. The dependence on incident angle is in subroutine TransAndReflAtPhi

		//step 8.  Hemispherical terms are averaged using standard method

		if ( ErrorsFound ) {
			ShowFatalError( "Program halted because of input problem(s) in WindowMaterial:SimpleGlazingSystem" );
		}

	}

	void
	SetupComplexFenestrationMaterialInput(
		int & MaterNum, // num of material items thus far
		bool & ErrorsFound
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Simon Vidanovic
		//       DATE WRITTEN   March 2012
		//       MODIFIED       May 2013 (Simon Vidanovic)
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// get input for complex fenestration materials

		// METHODOLOGY EMPLOYED:
		// usual GetInput processing.

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHeatBalance::Material;
		using General::RoundSigDigits;

		// SUBROUTINE ARGUMENT DEFINITIONS:

		// Locals
		// SUBROUTINE PARAMETER DEFINITIONS
		static std::string const RoutineName( "SetupComplexFenestrationMaterialInput: " );

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Array1D_string MaterialNames( 5 ); // Number of Material Alpha names defined
		Array1D< Real64 > MaterialProps( 27 ); // Temporary array to transfer material properties
		int Loop;
		int NumAlphas; // Number of Alphas for each GetObjectItem call
		int NumNumbers; // Number of Numbers for each GetObjectItem call
		int IOStatus; // Used in GetObjectItem
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name

		//Reading WindowGap:SupportPillar
		cCurrentModuleObject = "WindowGap:SupportPillar";
		W7SupportPillars = GetNumObjectsFound( cCurrentModuleObject );
		SupportPillar.allocate( W7SupportPillars );
		for ( Loop = 1; Loop <= W7SupportPillars; ++Loop ) {
			GetObjectItem( cCurrentModuleObject, Loop, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;

			// Verify unique names
			VerifyName( cAlphaArgs( 1 ), SupportPillar, Loop, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", object. Illegal value for " + cAlphaFieldNames( 1 ) + " has been found." );
				ShowContinueError( "...All Material names must be unique regardless of subtype." );
				continue;
			}

			SupportPillar( Loop ).Name = cAlphaArgs( 1 );
			SupportPillar( Loop ).Spacing = rNumericArgs( 1 );
			SupportPillar( Loop ).Radius = rNumericArgs( 2 );

			if ( rNumericArgs( 1 ) <= 0.0 ) {
				ErrorsFound = true;
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", object. Illegal value for " + cNumericFieldNames( 1 ) + " has been found." );
				ShowContinueError( cNumericFieldNames( 1 ) + " must be > 0, entered value = " + RoundSigDigits( rNumericArgs( 1 ), 2 ) );
			}

			if ( rNumericArgs( 2 ) <= 0.0 ) {
				ErrorsFound = true;
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", object. Illegal value for " + cNumericFieldNames( 2 ) + " has been found." );
				ShowContinueError( cNumericFieldNames( 2 ) + " must be > 0, entered value = " + RoundSigDigits( rNumericArgs( 2 ), 2 ) );
			}
		}

		//Reading WindowGap:DeflectionState
		cCurrentModuleObject = "WindowGap:DeflectionState";
		W7DeflectionStates = GetNumObjectsFound( cCurrentModuleObject );
		DeflectionState.allocate( W7DeflectionStates );
		for ( Loop = 1; Loop <= W7DeflectionStates; ++Loop ) {
			GetObjectItem( cCurrentModuleObject, Loop, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;

			// Verify unique names
			VerifyName( cAlphaArgs( 1 ), DeflectionState, Loop, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", object. Illegal value for " + cAlphaFieldNames( 1 ) + " has been found." );
				ShowContinueError( "...All Material names must be unique regardless of subtype." );
				continue;
			}

			DeflectionState( Loop ).Name = cAlphaArgs( 1 );
			DeflectionState( Loop ).DeflectedThickness = rNumericArgs( 1 );
			if ( rNumericArgs( 1 ) < 0.0 ) {
				ErrorsFound = true;
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", object. Illegal value for " + cNumericFieldNames( 1 ) + " has been found." );
				ShowContinueError( cNumericFieldNames( 1 ) + " must be >= 0, entered value = " + RoundSigDigits( rNumericArgs( 1 ), 2 ) );
			}
		}

		//Reading WindowMaterial:Gap

		cCurrentModuleObject = "WindowMaterial:Gap";
		W7MaterialGaps = GetNumObjectsFound( cCurrentModuleObject );
		//ALLOCATE(DeflectionState(W7DeflectionStates))
		for ( Loop = 1; Loop <= W7MaterialGaps; ++Loop ) {
			GetObjectItem( cCurrentModuleObject, Loop, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;

			// Verify unique names
			VerifyName( cAlphaArgs( 1 ), Material, MaterNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", object. Illegal value for " + cAlphaFieldNames( 1 ) + " has been found." );
				ShowContinueError( "...All Material names must be unique regardless of subtype." );
				continue;
			}

			++MaterNum;
			Material( MaterNum ).Group = ComplexWindowGap;
			Material( MaterNum ).Roughness = Rough;
			Material( MaterNum ).ROnly = true;

			Material( MaterNum ).Name = cAlphaArgs( 1 );

			Material( MaterNum ).Thickness = rNumericArgs( 1 );
			if ( rNumericArgs( 1 ) <= 0.0 ) {
				ErrorsFound = true;
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", object. Illegal value for " + cNumericFieldNames( 1 ) + " has been found." );
				ShowContinueError( cNumericFieldNames( 1 ) + " must be > 0, entered " + RoundSigDigits( rNumericArgs( 1 ), 2 ) );
			}

			Material( MaterNum ).Pressure = rNumericArgs( 2 );
			if ( rNumericArgs( 2 ) <= 0.0 ) {
				ErrorsFound = true;
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", object. Illegal value for " + cNumericFieldNames( 2 ) + " has been found." );
				ShowContinueError( cNumericFieldNames( 2 ) + " must be > 0, entered " + RoundSigDigits( rNumericArgs( 2 ), 2 ) );
			}

			if ( ! lAlphaFieldBlanks( 2 ) ) {
				Material( MaterNum ).GasPointer = FindItemInList( cAlphaArgs( 2 ), Material );
			} else {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", object. Illegal value for " + cAlphaFieldNames( 1 ) + " has been found." );
				ShowContinueError( cCurrentModuleObject + " does not have assigned WindowMaterial:Gas or WindowMaterial:GasMixutre." );
			}
			if ( ! lAlphaFieldBlanks( 3 ) ) {
				Material( MaterNum ).DeflectionStatePtr = FindItemInList( cAlphaArgs( 3 ), DeflectionState );
			}
			if ( ! lAlphaFieldBlanks( 4 ) ) {
				Material( MaterNum ).SupportPillarPtr = FindItemInList( cAlphaArgs( 4 ), SupportPillar );
			}
		}

		//Reading WindowMaterial:ComplexShade
		cCurrentModuleObject = "WindowMaterial:ComplexShade";
		TotComplexShades = GetNumObjectsFound( cCurrentModuleObject );

		if ( TotComplexShades > 0 ) {
			ComplexShade.allocate( TotComplexShades ); // Allocate the array Size to the number of complex shades
		}

		for ( Loop = 1; Loop <= TotComplexShades; ++Loop ) {
			GetObjectItem( cCurrentModuleObject, Loop, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			IsNotOK = false;
			IsBlank = false;

			// Verify unique names
			VerifyName( cAlphaArgs( 1 ), ComplexShade, Loop, IsNotOK, IsBlank, CurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", object. Illegal value for " + cAlphaFieldNames( 1 ) + " has been found." );
				ShowContinueError( "...All Material names must be unique regardless of subtype." );
				continue;
			}

			++MaterNum;
			Material( MaterNum ).Group = ComplexWindowShade;
			Material( MaterNum ).Roughness = Rough;
			Material( MaterNum ).ROnly = true;

			//Assign pointer to ComplexShade
			Material( MaterNum ).ComplexShadePtr = Loop;

			Material( MaterNum ).Name = cAlphaArgs( 1 );
			ComplexShade( Loop ).Name = cAlphaArgs( 1 );

			{ auto const SELECT_CASE_var( cAlphaArgs( 2 ) );

			if ( SELECT_CASE_var == "OTHERSHADINGTYPE" ) {
				ComplexShade( Loop ).LayerType = csOtherShadingType;
			} else if ( SELECT_CASE_var == "VENETIAN" ) {
				ComplexShade( Loop ).LayerType = csVenetian;
			} else if ( SELECT_CASE_var == "WOVEN" ) {
				ComplexShade( Loop ).LayerType = csWoven;
			} else if ( SELECT_CASE_var == "PERFORATED" ) {
				ComplexShade( Loop ).LayerType = csPerforated;
			} else if ( SELECT_CASE_var == "BSDF" ) {
				ComplexShade( Loop ).LayerType = csBSDF;
			} else {
				ErrorsFound = true;
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", object. Illegal value for " + cAlphaFieldNames( 2 ) + " has been found." );
				ShowContinueError( cAlphaFieldNames( 2 ) + " entered value = \"" + cAlphaArgs( 2 ) + "\" should be OtherShadingType, Venetian, Woven, Perforated or BSDF." );
			}}

			ComplexShade( Loop ).Thickness = rNumericArgs( 1 );
			Material( MaterNum ).Thickness = rNumericArgs( 1 );
			ComplexShade( Loop ).Conductivity = rNumericArgs( 2 );
			Material( MaterNum ).Conductivity = rNumericArgs( 2 );
			ComplexShade( Loop ).IRTransmittance = rNumericArgs( 3 );
			ComplexShade( Loop ).FrontEmissivity = rNumericArgs( 4 );
			ComplexShade( Loop ).BackEmissivity = rNumericArgs( 5 );

			// Simon: in heat balance radiation exchange routines AbsorpThermal is used
			// and program will crash if value is not assigned.  Not sure if this is correct
			// or some additional calculation is necessary. Simon TODO
			Material( MaterNum ).AbsorpThermal = rNumericArgs( 5 );
			Material( MaterNum ).AbsorpThermalFront = rNumericArgs( 4 );
			Material( MaterNum ).AbsorpThermalBack = rNumericArgs( 5 );

			ComplexShade( Loop ).TopOpeningMultiplier = rNumericArgs( 6 );
			ComplexShade( Loop ).BottomOpeningMultiplier = rNumericArgs( 7 );
			ComplexShade( Loop ).LeftOpeningMultiplier = rNumericArgs( 8 );
			ComplexShade( Loop ).RightOpeningMultiplier = rNumericArgs( 9 );
			ComplexShade( Loop ).FrontOpeningMultiplier = rNumericArgs( 10 );

			ComplexShade( Loop ).SlatWidth = rNumericArgs( 11 );
			ComplexShade( Loop ).SlatSpacing = rNumericArgs( 12 );
			ComplexShade( Loop ).SlatThickness = rNumericArgs( 13 );
			ComplexShade( Loop ).SlatAngle = rNumericArgs( 14 );
			ComplexShade( Loop ).SlatConductivity = rNumericArgs( 15 );
			ComplexShade( Loop ).SlatCurve = rNumericArgs( 16 );

			//IF (Material(MaterNum)%Conductivity > 0.0) THEN
			//  NominalR(MaterNum)=Material(MaterNum)%Thickness/Material(MaterNum)%Conductivity
			//ELSE
			//  NominalR(MaterNum)=1.0
			//ENDIF

			if ( rNumericArgs( 1 ) <= 0.0 ) {
				ErrorsFound = true;
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", object. Illegal value for " + cNumericFieldNames( 1 ) + " has been found." );
				ShowContinueError( cNumericFieldNames( 1 ) + " must be > 0, entered value = " + RoundSigDigits( rNumericArgs( 1 ), 2 ) );
			}

			if ( rNumericArgs( 2 ) <= 0.0 ) {
				ErrorsFound = true;
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", object. Illegal value for " + cNumericFieldNames( 2 ) + " has been found." );
				ShowContinueError( cNumericFieldNames( 2 ) + " must be > 0, entered value = " + RoundSigDigits( rNumericArgs( 2 ), 2 ) );
			}

			if ( ( rNumericArgs( 3 ) < 0.0 ) || ( rNumericArgs( 3 ) > 1.0 ) ) {
				ErrorsFound = true;
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", object. Illegal value for " + cNumericFieldNames( 3 ) + " has been found." );
				ShowContinueError( cNumericFieldNames( 3 ) + " value must be >= 0 and <= 1, entered value = " + RoundSigDigits( rNumericArgs( 3 ), 2 ) );
			}

			if ( ( rNumericArgs( 4 ) < 0.0 ) || ( rNumericArgs( 4 ) > 1.0 ) ) {
				ErrorsFound = true;
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", object. Illegal value for " + cNumericFieldNames( 4 ) + " has been found." );
				ShowContinueError( cNumericFieldNames( 4 ) + " value must be >= 0 and <= 1, entered value = " + RoundSigDigits( rNumericArgs( 4 ), 2 ) );
			}

			if ( ( rNumericArgs( 5 ) < 0.0 ) || ( rNumericArgs( 5 ) > 1.0 ) ) {
				ErrorsFound = true;
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", object. Illegal value for " + cNumericFieldNames( 5 ) + " has been found." );
				ShowContinueError( cNumericFieldNames( 5 ) + " value must be >= 0 and <= 1, entered value = " + RoundSigDigits( rNumericArgs( 5 ), 2 ) );
			}

			if ( ( rNumericArgs( 6 ) < 0.0 ) || ( rNumericArgs( 6 ) > 1.0 ) ) {
				ErrorsFound = true;
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", object. Illegal value for " + cNumericFieldNames( 6 ) + " has been found." );
				ShowContinueError( cNumericFieldNames( 6 ) + " must be >= 0 or <= 1, entered value = " + RoundSigDigits( rNumericArgs( 6 ), 2 ) );
			}

			if ( ( rNumericArgs( 7 ) < 0.0 ) || ( rNumericArgs( 7 ) > 1.0 ) ) {
				ErrorsFound = true;
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", object. Illegal value for " + cNumericFieldNames( 7 ) + " has been found." );
				ShowContinueError( cNumericFieldNames( 7 ) + " must be >=0 or <=1, entered " + RoundSigDigits( rNumericArgs( 7 ), 2 ) );
			}

			if ( ( rNumericArgs( 8 ) < 0.0 ) || ( rNumericArgs( 8 ) > 1.0 ) ) {
				ErrorsFound = true;
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", object. Illegal value for " + cNumericFieldNames( 8 ) + " has been found." );
				ShowContinueError( cNumericFieldNames( 8 ) + " must be >=0 or <=1, entered value = " + RoundSigDigits( rNumericArgs( 8 ), 2 ) );
			}

			if ( ( rNumericArgs( 9 ) < 0.0 ) || ( rNumericArgs( 9 ) > 1.0 ) ) {
				ErrorsFound = true;
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", object. Illegal value for " + cNumericFieldNames( 9 ) + " has been found." );
				ShowContinueError( cNumericFieldNames( 9 ) + " must be >=0 or <=1, entered value = " + RoundSigDigits( rNumericArgs( 9 ), 2 ) );
			}

			if ( ( rNumericArgs( 10 ) < 0.0 ) || ( rNumericArgs( 10 ) > 1.0 ) ) {
				ErrorsFound = true;
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", object. Illegal value for " + cNumericFieldNames( 10 ) + " has been found." );
				ShowContinueError( cNumericFieldNames( 10 ) + " must be >=0 or <=1, entered value = " + RoundSigDigits( rNumericArgs( 10 ), 2 ) );
			}

			if ( ComplexShade( Loop ).LayerType == csVenetian ) {
				if ( rNumericArgs( 11 ) <= 0.0 ) {
					ErrorsFound = true;
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", object. Illegal value for " + cNumericFieldNames( 11 ) + " has been found." );
					ShowContinueError( cNumericFieldNames( 11 ) + " must be >0, entered value = " + RoundSigDigits( rNumericArgs( 11 ), 2 ) );
				}

				if ( rNumericArgs( 12 ) <= 0.0 ) {
					ErrorsFound = true;
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", object. Illegal value for " + cNumericFieldNames( 12 ) + " has been found." );
					ShowContinueError( cNumericFieldNames( 12 ) + " must be >0, entered value = " + RoundSigDigits( rNumericArgs( 12 ), 2 ) );
				}

				if ( rNumericArgs( 13 ) <= 0.0 ) {
					ErrorsFound = true;
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", object. Illegal value for " + cNumericFieldNames( 13 ) + " has been found." );
					ShowContinueError( cNumericFieldNames( 13 ) + " must be >0, entered value = " + RoundSigDigits( rNumericArgs( 13 ), 2 ) );
				}

				if ( ( rNumericArgs( 14 ) < -90.0 ) || ( rNumericArgs( 14 ) > 90.0 ) ) {
					ErrorsFound = true;
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", object. Illegal value for " + cNumericFieldNames( 14 ) + " has been found." );
					ShowContinueError( cNumericFieldNames( 14 ) + " must be >=-90 and <=90, entered value = " + RoundSigDigits( rNumericArgs( 14 ), 2 ) );
				}

				if ( rNumericArgs( 15 ) <= 0.0 ) {
					ErrorsFound = true;
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", object. Illegal value for " + cNumericFieldNames( 15 ) + " has been found." );
					ShowContinueError( cNumericFieldNames( 15 ) + " must be >0, entered value = " + RoundSigDigits( rNumericArgs( 15 ), 2 ) );
				}

				if ( ( rNumericArgs( 16 ) < 0.0 ) || ( ( rNumericArgs( 16 ) > 0.0 ) && ( rNumericArgs( 16 ) < ( rNumericArgs( 11 ) / 2 ) ) ) ) {
					ErrorsFound = true;
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", object. Illegal value for " + cNumericFieldNames( 16 ) + " has been found." );
					ShowContinueError( cNumericFieldNames( 16 ) + " must be =0 or greater than SlatWidth/2, entered value = " + RoundSigDigits( rNumericArgs( 16 ), 2 ) );
				}
			}

			if ( ErrorsFound ) ShowFatalError( "Error in complex fenestration material input." );

		}

	}

	void
	SetupComplexFenestrationStateInput(
		int & ConstrNum, // num of construction items thus far
		bool & ErrorsFound
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   June 2010
		//       MODIFIED       January 2012 (Simon Vidanovic)
		//       MODIFIED       May 2012 (Simon Vidanovic)
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// get input for complex fenestration construction

		// METHODOLOGY EMPLOYED:
		// usual GetInput processing.  Matrix input from MatrixDataManager

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using InputProcessor::GetObjectDefMaxArgs;
		using namespace MatrixDataManager;
		using namespace DataBSDFWindow;
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "SetupComlexFenestrationStateInput: " );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//The following moved to DataBSDFWindow module:
		//INTEGER :: TotComplexFenStates   ! Number of complex fenestration construction definitions
		int I; // do loop index
		int Loop; // do loop counter
		int NumAlphas; // Number of Alphas for each GetObjectItem call
		int NumNumbers; // Number of Numbers for each GetObjectItem call
		int TotalArgs; // Number of fields for each GetObjectItem call
		int IOStatus; // Used in GetObjectItem
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name
		int iMatGlass; // number of glass layers
		int NumRows; // temporary size of matrix
		int NumCols; // temporary size of matrix
		int NBasis; // temporary number of elements in basis
		int Layer; // loop counter for material layers
		int AlphaIndex;
		int ThermalModelNum; // number of thermal model parameters object
		int NumOfTotalLayers; // total number of layers in the construction
		int NumOfOpticalLayers; // number of optical layers in the construction (excluding gasses and gas mixtures)
		int currentOpticalLayer; // current optical layer number.  This is important since optical structures should
		// be loaded only with optical layers

		// When reading Construction:ComplexFenestrationState, there is a call of GetMatrix2D which also uses same
		// variables from DataIPShortCuts.  Since this can cause some errors in reading, it is important
		// to declare local variables for reading Construction:ComplexFenestrationState object(s)
		Array1D_string locAlphaFieldNames;
		Array1D_string locNumericFieldNames;
		Array1D_bool locNumericFieldBlanks;
		Array1D_bool locAlphaFieldBlanks;
		Array1D_string locAlphaArgs;
		Array1D< Real64 > locNumericArgs;
		std::string locCurrentModuleObject;

		//Reading WindowThermalModel:Params
		cCurrentModuleObject = "WindowThermalModel:Params";
		TotThermalModels = GetNumObjectsFound( cCurrentModuleObject );
		WindowThermalModel.allocate( TotThermalModels );

		for ( Loop = 1; Loop <= TotThermalModels; ++Loop ) {
			GetObjectItem( cCurrentModuleObject, Loop, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, _, cAlphaFieldNames, cNumericFieldNames );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), WindowThermalModel, TotThermalModels - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				continue;
			}

			WindowThermalModel( Loop ).Name = cAlphaArgs( 1 );

			WindowThermalModel( Loop ).SDScalar = rNumericArgs( 1 );
			if ( ( rNumericArgs( 1 ) < 0.0 ) || ( rNumericArgs( 1 ) > 1.0 ) ) {
				ErrorsFound = true;
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", object. Illegal value for " + cNumericFieldNames( 1 ) + " has been found." );
				ShowContinueError( cNumericFieldNames( 1 ) + " should be >= 0.0 and <= 1.0, entered value = " + RoundSigDigits( rNumericArgs( 1 ), 2 ) );
			}

			{ auto const SELECT_CASE_var( cAlphaArgs( 2 ) );
			if ( SELECT_CASE_var == "ISO15099" ) {
				WindowThermalModel( Loop ).CalculationStandard = csISO15099;
			} else if ( SELECT_CASE_var == "EN673DECLARED" ) {
				WindowThermalModel( Loop ).CalculationStandard = csEN673Declared;
			} else if ( SELECT_CASE_var == "EN673DESIGN" ) {
				WindowThermalModel( Loop ).CalculationStandard = csEN673Design;
			} else {
				ErrorsFound = true;
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", object. Illegal value for " + cAlphaFieldNames( 2 ) + " has been found." );
				ShowContinueError( cAlphaFieldNames( 2 ) + " entered value = \"" + cAlphaArgs( 2 ) + "\" should be ISO15099, EN673Declared or EN673Design." );
			}}

			{ auto const SELECT_CASE_var( cAlphaArgs( 3 ) );
			if ( SELECT_CASE_var == "ISO15099" ) {
				WindowThermalModel( Loop ).ThermalModel = tmISO15099;
			} else if ( SELECT_CASE_var == "SCALEDCAVITYWIDTH" ) {
				WindowThermalModel( Loop ).ThermalModel = tmScaledCavityWidth;
			} else if ( SELECT_CASE_var == "CONVECTIVESCALARMODEL_NOSDTHICKNESS" ) {
				WindowThermalModel( Loop ).ThermalModel = tmConvectiveScalarModel_NoSDThickness;
			} else if ( SELECT_CASE_var == "CONVECTIVESCALARMODEL_WITHSDTHICKNESS" ) {
				WindowThermalModel( Loop ).ThermalModel = tmConvectiveScalarModel_WithSDThickness;
			} else {
				ErrorsFound = true;
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", object. Illegal value for " + cAlphaFieldNames( 3 ) + " has been found." );
				ShowContinueError( cAlphaFieldNames( 3 ) + " entered value = \"" + cAlphaArgs( 3 ) + "\" should be ISO15099, ScaledCavityWidth, ConvectiveScalarModel_NoSDThickness or ConvectiveScalarModel_WithSDThickness." );
			}}

			{ auto const SELECT_CASE_var( cAlphaArgs( 4 ) );
			if ( SELECT_CASE_var == "NODEFLECTION" ) {
				WindowThermalModel( Loop ).DeflectionModel = dmNoDeflection;
			} else if ( SELECT_CASE_var == "TEMPERATUREANDPRESSUREINPUT" ) {
				WindowThermalModel( Loop ).DeflectionModel = dmTemperatureAndPressureInput;
			} else if ( SELECT_CASE_var == "MEASUREDDEFLECTION" ) {
				WindowThermalModel( Loop ).DeflectionModel = dmMeasuredDeflection;
			} else {
				ErrorsFound = true;
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", object. Illegal value for " + cAlphaFieldNames( 4 ) + " has been found." );
				ShowContinueError( cAlphaFieldNames( 4 ) + " entered value = \"" + cAlphaArgs( 4 ) + "\" should be NoDeflection, TemperatureAndPressureInput or MeasuredDeflection." );
			}}

			if ( WindowThermalModel( Loop ).DeflectionModel == dmTemperatureAndPressureInput ) {
				WindowThermalModel( Loop ).VacuumPressureLimit = rNumericArgs( 2 );
				if ( rNumericArgs( 2 ) <= 0.0 ) {
					ErrorsFound = true;
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", object. Illegal value for " + cNumericFieldNames( 2 ) + " has been found." );
					ShowContinueError( cNumericFieldNames( 2 ) + " must be > 0, entered value = " + RoundSigDigits( rNumericArgs( 2 ), 2 ) );
				}

				WindowThermalModel( Loop ).InitialTemperature = rNumericArgs( 3 );
				if ( rNumericArgs( 3 ) <= 0.0 ) {
					ErrorsFound = true;
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", object. Illegal value for " + cNumericFieldNames( 3 ) + " has been found." );
					ShowContinueError( cNumericFieldNames( 3 ) + " must be > 0, entered value = " + RoundSigDigits( rNumericArgs( 3 ), 2 ) );
				}

				WindowThermalModel( Loop ).InitialPressure = rNumericArgs( 4 );
				if ( rNumericArgs( 4 ) <= 0.0 ) {
					ErrorsFound = true;
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + ", object. Illegal value for " + cNumericFieldNames( 4 ) + " has been found." );
					ShowContinueError( cNumericFieldNames( 4 ) + " must be > 0, entered value = " + RoundSigDigits( rNumericArgs( 4 ), 2 ) );
				}
			}

		} //DO Loop = 1, TotThermalModels

		//Reading Construction:ComplexFenestrationState
		locCurrentModuleObject = "Construction:ComplexFenestrationState";
		TotComplexFenStates = GetNumObjectsFound( locCurrentModuleObject );

		GetObjectDefMaxArgs( locCurrentModuleObject, TotalArgs, NumAlphas, NumNumbers );
		if ( ! allocated( locAlphaFieldNames ) ) locAlphaFieldNames.allocate( NumAlphas );
		if ( ! allocated( locNumericFieldNames ) ) locNumericFieldNames.allocate( NumNumbers );
		if ( ! allocated( locNumericFieldBlanks ) ) locNumericFieldBlanks.allocate( NumNumbers );
		if ( ! allocated( locAlphaFieldBlanks ) ) locAlphaFieldBlanks.allocate( NumAlphas );
		if ( ! allocated( locAlphaArgs ) ) locAlphaArgs.allocate( NumAlphas );
		if ( ! allocated( locNumericArgs ) ) locNumericArgs.allocate( NumNumbers );

		FirstBSDF = ConstrNum + 1; // Location of first BSDF construction input (They will be consecutive)
		for ( Loop = 1; Loop <= TotComplexFenStates; ++Loop ) {
			GetObjectItem( locCurrentModuleObject, Loop, locAlphaArgs, NumAlphas, locNumericArgs, NumNumbers, IOStatus, locNumericFieldBlanks, _, locAlphaFieldNames, locNumericFieldNames );
			++ConstrNum;
			IsNotOK = false;
			IsBlank = false;
			VerifyName( locAlphaArgs( 1 ), Construct, ConstrNum - 1, IsNotOK, IsBlank, locCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				continue;
			}
			//Glass layer counter
			iMatGlass = 0;
			//Simon TODO: This is to be confirmed.  If this is just initial value, then we might want to make better guess
			NominalRforNominalUCalculation( ConstrNum ) = 0.1;
			//Simon TODO: If I do not put this, then it is considered that surface is NOT window
			Construct( ConstrNum ).TransDiff = 0.1; //This is a place holder to flag
			//the construction as a window until
			//the correct value is entered in WindowComplexManager

			//Now override the deraults as appropriate
			Construct( ConstrNum ).Name = locAlphaArgs( 1 );

			//    ALLOCATE(Construct(ConstrNum)%BSDFInput)

			//Construct(ConstrNum)%BSDFInput%ThermalConstruction = ThConstNum

			{ auto const SELECT_CASE_var( locAlphaArgs( 2 ) ); // Basis Type Keyword
			if ( SELECT_CASE_var == "LBNLWINDOW" ) {
				Construct( ConstrNum ).BSDFInput.BasisType = BasisType_WINDOW;
			} else if ( SELECT_CASE_var == "USERDEFINED" ) {
				Construct( ConstrNum ).BSDFInput.BasisType = BasisType_Custom;
			} else {
				// throw error
				ErrorsFound = true;
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + locAlphaArgs( 1 ) + ", object. Illegal value for " + locAlphaFieldNames( 2 ) + " has been found." );
				ShowContinueError( locAlphaFieldNames( 2 ) + " entered value=\"" + locAlphaArgs( 2 ) + "\" should be LBNLWindow or UserDefined." );
			}}

			{ auto const SELECT_CASE_var( locAlphaArgs( 3 ) ); // Basis Symmetry Keyword
			if ( SELECT_CASE_var == "AXISYMMETRIC" ) {
				Construct( ConstrNum ).BSDFInput.BasisSymmetryType = BasisSymmetry_Axisymmetric;
			} else if ( SELECT_CASE_var == "NONE" ) {
				Construct( ConstrNum ).BSDFInput.BasisSymmetryType = BasisSymmetry_None;
			} else {
				// throw error
				ErrorsFound = true;
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + locAlphaArgs( 1 ) + ", object. Illegal value for " + locAlphaFieldNames( 3 ) + " has been found." );
				ShowContinueError( locAlphaFieldNames( 3 ) + " entered value = \"" + locAlphaArgs( 3 ) + "\" should be Axisymmetric or None." );
			}}

			//Simon: Assign thermal model number
			ThermalModelNum = FindItemInList( locAlphaArgs( 4 ), WindowThermalModel );
			if ( ThermalModelNum == 0 ) {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + locAlphaArgs( 1 ) + ", object. Illegal value for " + locAlphaFieldNames( 4 ) + " has been found." );
				ShowContinueError( locAlphaFieldNames( 4 ) + " entered value = \"" + locAlphaArgs( 4 ) + "\" no corresponding thermal model (WindowThermalModel:Params) found in the input file." );
			} else {
				Construct( ConstrNum ).BSDFInput.ThermalModel = ThermalModelNum;
			}

			// ***************************************************************************************
			// Basis matrix
			// ***************************************************************************************
			Construct( ConstrNum ).BSDFInput.BasisMatIndex = MatrixIndex( locAlphaArgs( 5 ) );
			Get2DMatrixDimensions( Construct( ConstrNum ).BSDFInput.BasisMatIndex, NumRows, NumCols );
			Construct( ConstrNum ).BSDFInput.BasisMatNrows = NumRows;
			Construct( ConstrNum ).BSDFInput.BasisMatNcols = NumCols;

			if ( NumCols != 2 && NumCols != 1 ) {
				ErrorsFound = true;
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + locAlphaArgs( 1 ) + ", object. Illegal value for " + locAlphaFieldNames( 5 ) + " has been found." );
				ShowContinueError( locAlphaFieldNames( 5 ) + " entered value=\"" + locAlphaArgs( 5 ) + "\" invalid matrix dimensions.  Basis matrix dimension can only be 2 x 1." );
			}
			Construct( ConstrNum ).BSDFInput.BasisMat.allocate( NumCols, NumRows );
			Get2DMatrix( Construct( ConstrNum ).BSDFInput.BasisMatIndex, Construct( ConstrNum ).BSDFInput.BasisMat );
			if ( Construct( ConstrNum ).BSDFInput.BasisType == BasisType_WINDOW ) CalculateBasisLength( Construct( ConstrNum ).BSDFInput, ConstrNum, Construct( ConstrNum ).BSDFInput.NBasis );

			//determine number of layers and optical layers
			NumOfTotalLayers = ( NumAlphas - 9 ) / 3;
			Construct( ConstrNum ).TotLayers = NumOfTotalLayers;

			NumOfOpticalLayers = NumOfTotalLayers / 2 + 1;

			Construct( ConstrNum ).BSDFInput.NumLayers = NumOfOpticalLayers;
			Construct( ConstrNum ).BSDFInput.Layer.allocate( NumOfOpticalLayers );

			// check for incomplete field set
			if ( mod( ( NumAlphas - 9 ), 3 ) != 0 ) {
				//throw warning if incomplete field set
				ErrorsFound = true;
				ShowSevereError( RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs( 1 ) + ", object. Incomplete field set found." );
				ShowContinueError( locAlphaArgs( 1 ) + " is missing some of the layers or/and gaps." );
			}

			if ( Construct( ConstrNum ).BSDFInput.BasisSymmetryType == BasisSymmetry_None ) {
				//Non-Symmetric basis

				NBasis = Construct( ConstrNum ).BSDFInput.NBasis;

				// *******************************************************************************
				// Solar front transmittance
				// *******************************************************************************
				Construct( ConstrNum ).BSDFInput.SolFrtTransIndex = MatrixIndex( locAlphaArgs( 6 ) );
				Get2DMatrixDimensions( Construct( ConstrNum ).BSDFInput.SolFrtTransIndex, NumRows, NumCols );
				Construct( ConstrNum ).BSDFInput.SolFrtTransNrows = NumRows;
				Construct( ConstrNum ).BSDFInput.SolFrtTransNcols = NumCols;

				if ( NumRows != NBasis ) {
					ErrorsFound = true;
					ShowSevereError( RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs( 1 ) + ", object. Illegal matrix size has been found." );
					ShowContinueError( "Solar front transmittance matrix \"" + locAlphaArgs( 6 ) + "\" is not the same size as it is defined by basis definition. Basis size is defined by Matrix:TwoDimension = \"" + locAlphaArgs( 5 ) + "\"." );
				}

				if ( NumRows != NumCols ) {
					ErrorsFound = true;
					ShowSevereError( RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs( 1 ) + "\", object. Invalid BSDF matrix dimensions." );
					ShowContinueError( "Solar front transmittance matrix \"" + locAlphaArgs( 6 ) + "\" must have the same number of rows and columns." );
				}

				if ( Construct( ConstrNum ).BSDFInput.BasisType == BasisType_Custom ) {
					Construct( ConstrNum ).BSDFInput.NBasis = NumRows; // For custom basis, no rows in transmittance
					// matrix defines the basis length
				}

				Construct( ConstrNum ).BSDFInput.SolFrtTrans.allocate( NumCols, NumRows );
				if ( Construct( ConstrNum ).BSDFInput.SolFrtTransIndex == 0 ) {
					ErrorsFound = true;
					ShowSevereError( RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs( 1 ) + ", object. Referenced Matrix:TwoDimension is missing from the input file." );
					ShowContinueError( "Solar front transmittance Matrix:TwoDimension = \"" + locAlphaArgs( 6 ) + "\" is missing from the input file." );
				} else {
					Get2DMatrix( Construct( ConstrNum ).BSDFInput.SolFrtTransIndex, Construct( ConstrNum ).BSDFInput.SolFrtTrans );
				}

				// *******************************************************************************
				// Solar back reflectance
				// *******************************************************************************
				Construct( ConstrNum ).BSDFInput.SolBkReflIndex = MatrixIndex( locAlphaArgs( 7 ) );
				Get2DMatrixDimensions( Construct( ConstrNum ).BSDFInput.SolBkReflIndex, NumRows, NumCols );
				Construct( ConstrNum ).BSDFInput.SolBkReflNrows = NumRows;
				Construct( ConstrNum ).BSDFInput.SolBkReflNcols = NumCols;

				if ( NumRows != NBasis ) {
					ErrorsFound = true;
					ShowSevereError( RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs( 1 ) + ", object. Illegal matrix size has been found." );
					ShowContinueError( "Solar back reflectance matrix \"" + locAlphaArgs( 7 ) + "\" is not the same size as it is defined by basis definition. Basis size is defined by Matrix:TwoDimension = \"" + locAlphaArgs( 5 ) + "\"." );
				}

				if ( NumRows != NumCols ) {
					ErrorsFound = true;
					ShowSevereError( RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs( 1 ) + "\", object. Invalid BSDF matrix dimensions." );
					ShowContinueError( "Solar bakc reflectance matrix \"" + locAlphaArgs( 7 ) + "\" must have the same number of rows and columns." );
				}

				Construct( ConstrNum ).BSDFInput.SolBkRefl.allocate( NumCols, NumRows );
				if ( Construct( ConstrNum ).BSDFInput.SolBkReflIndex == 0 ) {
					ErrorsFound = true;
					ShowSevereError( RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs( 1 ) + ", object. Referenced Matrix:TwoDimension is missing from the input file." );
					ShowContinueError( "Solar back reflectance Matrix:TwoDimension = \"" + locAlphaArgs( 7 ) + "\" is missing from the input file." );
				} else {
					Get2DMatrix( Construct( ConstrNum ).BSDFInput.SolBkReflIndex, Construct( ConstrNum ).BSDFInput.SolBkRefl );
				}

				// *******************************************************************************
				// Visible front transmittance
				// *******************************************************************************
				Construct( ConstrNum ).BSDFInput.VisFrtTransIndex = MatrixIndex( locAlphaArgs( 8 ) );
				Get2DMatrixDimensions( Construct( ConstrNum ).BSDFInput.VisFrtTransIndex, NumRows, NumCols );
				Construct( ConstrNum ).BSDFInput.VisFrtTransNrows = NumRows;
				Construct( ConstrNum ).BSDFInput.VisFrtTransNcols = NumCols;

				if ( NumRows != NBasis ) {
					ErrorsFound = true;
					ShowSevereError( RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs( 1 ) + ", object. Illegal matrix size has been found." );
					ShowContinueError( "Visible front transmittance matrix \"" + locAlphaArgs( 8 ) + "\" is not the same size as it is defined by basis definition. Basis size is defined by Matrix:TwoDimension = \"" + locAlphaArgs( 5 ) + "\"." );
				}

				if ( NumRows != NumCols ) {
					ErrorsFound = true;
					ShowSevereError( RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs( 1 ) + "\", object. Invalid BSDF matrix dimensions." );
					ShowContinueError( "Visible front transmittance matrix \"" + locAlphaArgs( 8 ) + "\" must have the same number of rows and columns." );
				}

				Construct( ConstrNum ).BSDFInput.VisFrtTrans.allocate( NumCols, NumRows );
				if ( Construct( ConstrNum ).BSDFInput.VisFrtTransIndex == 0 ) {
					ErrorsFound = true;
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + locAlphaArgs( 1 ) + ", object. Referenced Matrix:TwoDimension is missing from the input file." );
					ShowContinueError( "Visible front transmittance Matrix:TwoDimension = \"" + locAlphaArgs( 8 ) + "\" is missing from the input file." );
				} else {
					Get2DMatrix( Construct( ConstrNum ).BSDFInput.VisFrtTransIndex, Construct( ConstrNum ).BSDFInput.VisFrtTrans );
				}

				// *******************************************************************************
				// Visible back reflectance
				// *******************************************************************************
				Construct( ConstrNum ).BSDFInput.VisBkReflIndex = MatrixIndex( locAlphaArgs( 9 ) );
				Get2DMatrixDimensions( Construct( ConstrNum ).BSDFInput.VisBkReflIndex, NumRows, NumCols );
				Construct( ConstrNum ).BSDFInput.VisBkReflNrows = NumRows;
				Construct( ConstrNum ).BSDFInput.VisBkReflNcols = NumCols;

				if ( NumRows != NBasis ) {
					ErrorsFound = true;
					ShowSevereError( RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs( 1 ) + ", object. Illegal matrix size has been found." );
					ShowContinueError( "Visible back reflectance matrix \"" + locAlphaArgs( 9 ) + "\" is not the same size as it is defined by basis definition. Basis size is defined by Matrix:TwoDimension = \"" + locAlphaArgs( 5 ) + "\"." );
				}

				if ( NumRows != NumCols ) {
					ErrorsFound = true;
					ShowSevereError( RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs( 1 ) + "\", object. Invalid BSDF matrix dimensions." );
					ShowContinueError( "Visible back reflectance \"" + locAlphaArgs( 9 ) + "\" must have the same number of rows and columns." );
				}

				Construct( ConstrNum ).BSDFInput.VisBkRefl.allocate( NumCols, NumRows );
				if ( Construct( ConstrNum ).BSDFInput.VisBkReflIndex == 0 ) {
					ErrorsFound = true;
					ShowSevereError( RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs( 1 ) + ", object. Referenced Matrix:TwoDimension is missing from the input file." );
					ShowContinueError( "Visble back reflectance Matrix:TwoDimension = \"" + locAlphaArgs( 9 ) + "\" is missing from the input file." );
				} else {
					Get2DMatrix( Construct( ConstrNum ).BSDFInput.VisBkReflIndex, Construct( ConstrNum ).BSDFInput.VisBkRefl );
				}

				//ALLOCATE(Construct(ConstrNum)%BSDFInput%Layer(NumOfOpticalLayers))
				for ( Layer = 1; Layer <= Construct( ConstrNum ).TotLayers; ++Layer ) {
					AlphaIndex = 9 + ( Layer * 3 ) - 2;
					currentOpticalLayer = int( Layer / 2 ) + 1;
					//Material info is contained in the thermal construct
					Construct( ConstrNum ).LayerPoint( Layer ) = FindItemInList( locAlphaArgs( AlphaIndex ), Material );

					//Simon: Load only if optical layer
					if ( mod( Layer, 2 ) != 0 ) {
						Construct( ConstrNum ).BSDFInput.Layer( currentOpticalLayer ).MaterialIndex = Construct( ConstrNum ).LayerPoint( Layer );

						++AlphaIndex;
						// *******************************************************************************
						// Front absorptance matrix
						// *******************************************************************************
						Construct( ConstrNum ).BSDFInput.Layer( currentOpticalLayer ).FrtAbsIndex = MatrixIndex( locAlphaArgs( AlphaIndex ) );
						Get2DMatrixDimensions( Construct( ConstrNum ).BSDFInput.Layer( currentOpticalLayer ).FrtAbsIndex, NumRows, NumCols );

						if ( NumRows != 1 ) {
							ErrorsFound = true;
							ShowSevereError( RoutineName + locCurrentModuleObject + " = \"" + locAlphaArgs( 1 ) + "\", object. Incorrect matrix dimension." );
							ShowContinueError( "Front absorbtance Matrix:TwoDimension = \"" + locAlphaArgs( AlphaIndex ) + "\" for layer " + RoundSigDigits( currentOpticalLayer ) + " must have only one row." );
						}

						if ( NumCols != NBasis ) {
							ErrorsFound = true;
							ShowSevereError( RoutineName + locCurrentModuleObject + " = \"" + locAlphaArgs( 1 ) + "\", object. Incorrect matrix dimension." );
							ShowContinueError( "Front absorbtance Matrix:TwoDimension = \"" + locAlphaArgs( AlphaIndex ) + "\" for layer " + RoundSigDigits( currentOpticalLayer ) + " must have same number of columns as it is defined by basis matrix." );
							ShowContinueError( "Matrix has " + RoundSigDigits( NumCols ) + " number of columns, while basis definition specifies " + RoundSigDigits( NBasis ) + " number of columns." );
						}

						Construct( ConstrNum ).BSDFInput.Layer( currentOpticalLayer ).AbsNcols = NumCols;
						Construct( ConstrNum ).BSDFInput.Layer( currentOpticalLayer ).FrtAbs.allocate( NumCols, NumRows );
						if ( Construct( ConstrNum ).BSDFInput.Layer( currentOpticalLayer ).FrtAbsIndex == 0 ) {
							ErrorsFound = true;
							ShowSevereError( RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs( 1 ) + ", object. Referenced Matrix:TwoDimension is missing from the input file." );
							ShowContinueError( "Front absorbtance Matrix:TwoDimension = \"" + locAlphaArgs( AlphaIndex ) + "\" for layer " + RoundSigDigits( currentOpticalLayer ) + " is missing from the input file." );
						} else {
							Get2DMatrix( Construct( ConstrNum ).BSDFInput.Layer( currentOpticalLayer ).FrtAbsIndex, Construct( ConstrNum ).BSDFInput.Layer( currentOpticalLayer ).FrtAbs );
						}

						++AlphaIndex;
						// *******************************************************************************
						// Back absorptance matrix
						// *******************************************************************************
						Construct( ConstrNum ).BSDFInput.Layer( currentOpticalLayer ).BkAbsIndex = MatrixIndex( locAlphaArgs( AlphaIndex ) );
						Get2DMatrixDimensions( Construct( ConstrNum ).BSDFInput.Layer( currentOpticalLayer ).BkAbsIndex, NumRows, NumCols );

						if ( NumRows != 1 ) {
							ErrorsFound = true;
							ShowSevereError( RoutineName + locCurrentModuleObject + " = \"" + locAlphaArgs( 1 ) + "\", object. Incorrect matrix dimension." );
							ShowContinueError( "Back absorbtance Matrix:TwoDimension = \"" + locAlphaArgs( AlphaIndex ) + "\" for layer " + RoundSigDigits( currentOpticalLayer ) + " must have only one row." );
						}

						if ( NumCols != NBasis ) {
							ErrorsFound = true;
							ShowSevereError( RoutineName + locCurrentModuleObject + " = \"" + locAlphaArgs( 1 ) + "\", object. Incorrect matrix dimension." );
							ShowContinueError( "Back absorbtance Matrix:TwoDimension = \"" + locAlphaArgs( AlphaIndex ) + "\" for layer " + RoundSigDigits( currentOpticalLayer ) + " must have same number of columns as it is defined by basis matrix." );
							ShowContinueError( "Matrix has " + RoundSigDigits( NumCols ) + " number of columns, while basis definition specifies " + RoundSigDigits( NBasis ) + " number of columns." );
						}

						Construct( ConstrNum ).BSDFInput.Layer( currentOpticalLayer ).BkAbs.allocate( NumCols, NumRows );
						if ( Construct( ConstrNum ).BSDFInput.Layer( currentOpticalLayer ).BkAbsIndex == 0 ) {
							ErrorsFound = true;
							ShowSevereError( RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs( 1 ) + ", object. Referenced Matrix:TwoDimension is missing from the input file." );
							ShowContinueError( "Back absorbtance Matrix:TwoDimension = \"" + locAlphaArgs( AlphaIndex ) + "\" for layer " + RoundSigDigits( currentOpticalLayer ) + " is missing from the input file." );
						} else {
							Get2DMatrix( Construct( ConstrNum ).BSDFInput.Layer( currentOpticalLayer ).BkAbsIndex, Construct( ConstrNum ).BSDFInput.Layer( currentOpticalLayer ).BkAbs );
						}
					} //if (Mod(Layer, 2) <> 0) then

				}
			} else {
				//Axisymmetric basis
				NBasis = Construct( ConstrNum ).BSDFInput.NBasis; //Basis length has already been calculated
				BSDFTempMtrx.allocate( NBasis, 1 );

				// *******************************************************************************
				// Solar front transmittance
				// *******************************************************************************
				Construct( ConstrNum ).BSDFInput.SolFrtTransIndex = MatrixIndex( locAlphaArgs( 6 ) );
				Get2DMatrixDimensions( Construct( ConstrNum ).BSDFInput.SolFrtTransIndex, NumRows, NumCols );
				Construct( ConstrNum ).BSDFInput.SolFrtTransNrows = NBasis;
				Construct( ConstrNum ).BSDFInput.SolFrtTransNcols = NBasis;

				if ( NumRows != NBasis ) {
					ErrorsFound = true;
					ShowSevereError( RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs( 1 ) + ", object. Illegal matrix size has been found." );
					ShowContinueError( "Solar front transmittance matrix \"" + locAlphaArgs( 6 ) + "\" is not the same size as it is defined by basis definition. Basis size is defined by Matrix:TwoDimension = \"" + locAlphaArgs( 5 ) + "\"." );
				}

				if ( NumRows != NumCols ) {
					ErrorsFound = true;
					ShowSevereError( RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs( 1 ) + "\", object. Invalid BSDF matrix dimensions." );
					ShowContinueError( "Solar front transmittance matrix \"" + locAlphaArgs( 6 ) + "\" must have the same number of rows and columns." );
				}

				Construct( ConstrNum ).BSDFInput.SolFrtTrans.allocate( NBasis, NBasis );
				if ( Construct( ConstrNum ).BSDFInput.SolFrtTransIndex == 0 ) {
					ErrorsFound = true;
					ShowSevereError( RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs( 1 ) + ", object. Referenced Matrix:TwoDimension is missing from the input file." );
					ShowContinueError( "Solar front transmittance Matrix:TwoDimension = \"" + locAlphaArgs( 6 ) + "\" is missing from the input file." );
				} else {
					Get2DMatrix( Construct( ConstrNum ).BSDFInput.SolFrtTransIndex, BSDFTempMtrx );

					Construct( ConstrNum ).BSDFInput.SolFrtTrans = 0.0;
					for ( I = 1; I <= NBasis; ++I ) {
						Construct( ConstrNum ).BSDFInput.SolFrtTrans( I, I ) = BSDFTempMtrx( I, 1 );
					}
				}

				// *******************************************************************************
				// Solar back reflectance
				// *******************************************************************************
				Construct( ConstrNum ).BSDFInput.SolBkReflIndex = MatrixIndex( locAlphaArgs( 7 ) );
				Get2DMatrixDimensions( Construct( ConstrNum ).BSDFInput.SolBkReflIndex, NumRows, NumCols );
				Construct( ConstrNum ).BSDFInput.SolBkReflNrows = NBasis;
				Construct( ConstrNum ).BSDFInput.SolBkReflNcols = NBasis;

				if ( NumRows != NBasis ) {
					ErrorsFound = true;
					ShowSevereError( RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs( 1 ) + ", object. Illegal matrix size has been found." );
					ShowContinueError( "Solar back reflectance matrix \"" + locAlphaArgs( 7 ) + "\" is not the same size as it is defined by basis definition. Basis size is defined by Matrix:TwoDimension = \"" + locAlphaArgs( 5 ) + "\"." );
				}

				if ( NumRows != NumCols ) {
					ErrorsFound = true;
					ShowSevereError( RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs( 1 ) + "\", object. Invalid BSDF matrix dimensions." );
					ShowContinueError( "Solar back reflectance matrix \"" + locAlphaArgs( 7 ) + "\" must have the same number of rows and columns." );
				}

				Construct( ConstrNum ).BSDFInput.SolBkRefl.allocate( NBasis, NBasis );
				if ( Construct( ConstrNum ).BSDFInput.SolBkReflIndex == 0 ) {
					ErrorsFound = true;
					ShowSevereError( RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs( 1 ) + ", object. Referenced Matrix:TwoDimension is missing from the input file." );
					ShowContinueError( "Solar back reflectance Matrix:TwoDimension = \"" + locAlphaArgs( 7 ) + "\" is missing from the input file." );
				} else {
					Get2DMatrix( Construct( ConstrNum ).BSDFInput.SolBkReflIndex, BSDFTempMtrx );
					Construct( ConstrNum ).BSDFInput.SolBkRefl = 0.0;
					for ( I = 1; I <= NBasis; ++I ) {
						Construct( ConstrNum ).BSDFInput.SolBkRefl( I, I ) = BSDFTempMtrx( I, 1 );
					}
				}

				// *******************************************************************************
				// Visible front transmittance
				// *******************************************************************************
				Construct( ConstrNum ).BSDFInput.VisFrtTransIndex = MatrixIndex( locAlphaArgs( 8 ) );
				Get2DMatrixDimensions( Construct( ConstrNum ).BSDFInput.VisFrtTransIndex, NumRows, NumCols );
				Construct( ConstrNum ).BSDFInput.VisFrtTransNrows = NBasis;
				Construct( ConstrNum ).BSDFInput.VisFrtTransNcols = NBasis;

				if ( NumRows != NBasis ) {
					ErrorsFound = true;
					ShowSevereError( RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs( 1 ) + ", object. Illegal matrix size has been found." );
					ShowContinueError( "Visible front transmittance matrix \"" + locAlphaArgs( 8 ) + "\" is not the same size as it is defined by basis definition. Basis size is defined by Matrix:TwoDimension = \"" + locAlphaArgs( 5 ) + "\"." );
				}

				if ( NumRows != NumCols ) {
					ErrorsFound = true;
					ShowSevereError( RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs( 1 ) + "\", object. Invalid BSDF matrix dimensions." );
					ShowContinueError( "Visible front transmittance matrix \"" + locAlphaArgs( 8 ) + "\" must have the same number of rows and columns." );
				}

				Construct( ConstrNum ).BSDFInput.VisFrtTrans.allocate( NBasis, NBasis );
				if ( Construct( ConstrNum ).BSDFInput.VisFrtTransIndex == 0 ) {
					ErrorsFound = true;
					ShowSevereError( RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs( 1 ) + ", object. Referenced Matrix:TwoDimension is missing from the input file." );
					ShowContinueError( "Visible front transmittance Matrix:TwoDimension = \"" + locAlphaArgs( 8 ) + "\" is missing from the input file." );
				} else {
					Get2DMatrix( Construct( ConstrNum ).BSDFInput.VisFrtTransIndex, BSDFTempMtrx );
					Construct( ConstrNum ).BSDFInput.VisFrtTrans = 0.0;
					for ( I = 1; I <= NBasis; ++I ) {
						Construct( ConstrNum ).BSDFInput.VisFrtTrans( I, I ) = BSDFTempMtrx( I, 1 );
					}
				}

				// *******************************************************************************
				// Visible back reflectance
				// *******************************************************************************
				Construct( ConstrNum ).BSDFInput.VisBkReflIndex = MatrixIndex( locAlphaArgs( 9 ) );
				Get2DMatrixDimensions( Construct( ConstrNum ).BSDFInput.VisBkReflIndex, NumRows, NumCols );
				Construct( ConstrNum ).BSDFInput.VisBkReflNrows = NBasis;
				Construct( ConstrNum ).BSDFInput.VisBkReflNcols = NBasis;

				if ( NumRows != NBasis ) {
					ErrorsFound = true;
					ShowSevereError( RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs( 1 ) + ", object. Illegal matrix size has been found." );
					ShowContinueError( "Visible back reflectance matrix \"" + locAlphaArgs( 9 ) + "\" is not the same size as it is defined by basis definition. Basis size is defined by Matrix:TwoDimension = \"" + locAlphaArgs( 5 ) + "\"." );
				}

				if ( NumRows != NumCols ) {
					ErrorsFound = true;
					ShowSevereError( RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs( 1 ) + "\", object. Invalid BSDF matrix dimensions." );
					ShowContinueError( "Visible back reflectance matrix \"" + locAlphaArgs( 9 ) + "\" must have the same number of rows and columns." );
				}

				Construct( ConstrNum ).BSDFInput.VisBkRefl.allocate( NBasis, NBasis );
				if ( Construct( ConstrNum ).BSDFInput.VisBkReflIndex == 0 ) {
					ErrorsFound = true;
					ShowSevereError( RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs( 1 ) + ", object. Referenced Matrix:TwoDimension is missing from the input file." );
					ShowContinueError( "Visible back reflectance Matrix:TwoDimension = \"" + locAlphaArgs( 9 ) + "\" is missing from the input file." );
				} else {
					Get2DMatrix( Construct( ConstrNum ).BSDFInput.VisBkReflIndex, BSDFTempMtrx );
					Construct( ConstrNum ).BSDFInput.VisBkRefl = 0.0;
					for ( I = 1; I <= NBasis; ++I ) {
						Construct( ConstrNum ).BSDFInput.VisBkRefl( I, I ) = BSDFTempMtrx( I, 1 );
					}
				}

				//determine number of layers
				//Construct(ConstrNum)%TotLayers = (NumAlphas - 9)/3

				// check for incomplete field set
				//IF (Mod((NumAlphas - 9), 3) /= 0) Then
				//throw warning if incomplete field set
				//  CALL ShowWarningError('Construction:ComplexFenestrationState: Axisymmetric properties have incomplete field &
				//   & set')
				//ENDIF

				//ALLOCATE(Construct(ConstrNum)%BSDFInput%Layer(NumOfOpticalLayers))
				for ( Layer = 1; Layer <= Construct( ConstrNum ).TotLayers; ++Layer ) {
					AlphaIndex = 9 + ( Layer * 3 ) - 2;
					currentOpticalLayer = int( Layer / 2 ) + 1;

					Construct( ConstrNum ).LayerPoint( Layer ) = FindItemInList( locAlphaArgs( AlphaIndex ), Material );

					if ( mod( Layer, 2 ) != 0 ) {
						Construct( ConstrNum ).BSDFInput.Layer( currentOpticalLayer ).MaterialIndex = Construct( ConstrNum ).LayerPoint( Layer );

						// *******************************************************************************
						// Front absorptance matrix
						// *******************************************************************************
						++AlphaIndex;
						Construct( ConstrNum ).BSDFInput.Layer( currentOpticalLayer ).FrtAbsIndex = MatrixIndex( locAlphaArgs( AlphaIndex ) );
						Get2DMatrixDimensions( Construct( ConstrNum ).BSDFInput.Layer( currentOpticalLayer ).FrtAbsIndex, NumRows, NumCols );

						if ( NumRows != 1 ) {
							ErrorsFound = true;
							ShowSevereError( RoutineName + locCurrentModuleObject + " = \"" + locAlphaArgs( 1 ) + "\", object. Incorrect matrix dimension." );
							ShowContinueError( "Front absorbtance Matrix:TwoDimension = \"" + locAlphaArgs( AlphaIndex ) + "\" for layer " + RoundSigDigits( currentOpticalLayer ) + " must have only one row." );
						}

						if ( NumCols != NBasis ) {
							ErrorsFound = true;
							ShowSevereError( RoutineName + locCurrentModuleObject + " = \"" + locAlphaArgs( 1 ) + "\", object. Incorrect matrix dimension." );
							ShowContinueError( "Front absorbtance Matrix:TwoDimension = \"" + locAlphaArgs( AlphaIndex ) + "\" for layer " + RoundSigDigits( currentOpticalLayer ) + " must have same number of columns as it is defined by basis matrix." );
							ShowContinueError( "Matrix has " + RoundSigDigits( NumCols ) + " number of columns, while basis definition specifies " + RoundSigDigits( NBasis ) + " number of columns." );
						}

						Construct( ConstrNum ).BSDFInput.Layer( currentOpticalLayer ).AbsNcols = NumCols;
						Construct( ConstrNum ).BSDFInput.Layer( currentOpticalLayer ).FrtAbs.allocate( NumCols, NumRows );

						if ( Construct( ConstrNum ).BSDFInput.Layer( currentOpticalLayer ).FrtAbsIndex == 0 ) {
							ErrorsFound = true;
							ShowSevereError( RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs( 1 ) + ", object. Referenced Matrix:TwoDimension is missing from the input file." );
							ShowContinueError( "Front absorbtance Matrix:TwoDimension = \"" + locAlphaArgs( AlphaIndex ) + "\" for layer " + RoundSigDigits( currentOpticalLayer ) + " is missing from the input file." );
						} else {
							Get2DMatrix( Construct( ConstrNum ).BSDFInput.Layer( currentOpticalLayer ).FrtAbsIndex, Construct( ConstrNum ).BSDFInput.Layer( currentOpticalLayer ).FrtAbs );
						}

						// *******************************************************************************
						// Back absorptance matrix
						// *******************************************************************************
						++AlphaIndex;
						Construct( ConstrNum ).BSDFInput.Layer( currentOpticalLayer ).BkAbsIndex = MatrixIndex( locAlphaArgs( AlphaIndex ) );
						Get2DMatrixDimensions( Construct( ConstrNum ).BSDFInput.Layer( currentOpticalLayer ).BkAbsIndex, NumRows, NumCols );

						if ( NumRows != 1 ) {
							ErrorsFound = true;
							ShowSevereError( RoutineName + locCurrentModuleObject + " = \"" + locAlphaArgs( 1 ) + "\", object. Incorrect matrix dimension." );
							ShowContinueError( "Back absorbtance Matrix:TwoDimension = \"" + locAlphaArgs( AlphaIndex ) + "\" for layer " + RoundSigDigits( currentOpticalLayer ) + " must have only one row." );
						}

						if ( NumCols != NBasis ) {
							ErrorsFound = true;
							ShowSevereError( RoutineName + locCurrentModuleObject + " = \"" + locAlphaArgs( 1 ) + "\", object. Incorrect matrix dimension." );
							ShowContinueError( "Back absorbtance Matrix:TwoDimension = \"" + locAlphaArgs( AlphaIndex ) + "\" for layer " + RoundSigDigits( currentOpticalLayer ) + " must have same number of columns as it is defined by basis matrix." );
							ShowContinueError( "Matrix has " + RoundSigDigits( NumCols ) + " number of columns, while basis definition specifies " + RoundSigDigits( NBasis ) + " number of columns." );
						}

						Construct( ConstrNum ).BSDFInput.Layer( currentOpticalLayer ).BkAbs.allocate( NumCols, NumRows );

						if ( Construct( ConstrNum ).BSDFInput.Layer( currentOpticalLayer ).BkAbsIndex == 0 ) {
							ErrorsFound = true;
							ShowSevereError( RoutineName + locCurrentModuleObject + "=\"" + locAlphaArgs( 1 ) + ", object. Referenced Matrix:TwoDimension is missing from the input file." );
							ShowContinueError( "Back absorbtance Matrix:TwoDimension = \"" + locAlphaArgs( AlphaIndex ) + "\" for layer " + RoundSigDigits( currentOpticalLayer ) + " is missing from the input file." );
						} else {
							Get2DMatrix( Construct( ConstrNum ).BSDFInput.Layer( currentOpticalLayer ).BkAbsIndex, Construct( ConstrNum ).BSDFInput.Layer( currentOpticalLayer ).BkAbs );
						}
					} // if (Mod(Layer, 2) <> 0) then
				}

				BSDFTempMtrx.deallocate();
			}
			Construct( ConstrNum ).TypeIsWindow = true;
			Construct( ConstrNum ).WindowTypeBSDF = true;
		}

		// Do not forget to deallocate localy allocated variables
		if ( allocated( locAlphaFieldNames ) ) locAlphaFieldNames.deallocate();
		if ( allocated( locNumericFieldNames ) ) locNumericFieldNames.deallocate();
		if ( allocated( locNumericFieldBlanks ) ) locNumericFieldBlanks.deallocate();
		if ( allocated( locAlphaFieldBlanks ) ) locAlphaFieldBlanks.deallocate();
		if ( allocated( locAlphaArgs ) ) locAlphaArgs.deallocate();
		if ( allocated( locNumericArgs ) ) locNumericArgs.deallocate();

		if ( ErrorsFound ) ShowFatalError( "Error in complex fenestration input." );

	}

} // HeatBalanceManager

} // EnergyPlus
