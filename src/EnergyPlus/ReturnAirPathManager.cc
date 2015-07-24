// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <ReturnAirPathManager.hh>
#include <DataAirflowNetwork.hh>
#include <DataGlobals.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPrecisionGlobals.hh>
#include <DataZoneEquipment.hh>
#include <GeneralRoutines.hh>
#include <InputProcessor.hh>
#include <MixerComponent.hh>
#include <NodeInputManager.hh>
#include <UtilityRoutines.hh>
#include <ZonePlenum.hh>

namespace EnergyPlus {

namespace ReturnAirPathManager {
	// Module containing the routines dealing with the AirLoopHVAC:ReturnPath (formerly Return Air Path)

	// MODULE INFORMATION:
	//       AUTHOR         Russ Taylor
	//       DATE WRITTEN   January 1998
	//       MODIFIED       Lawrie, September 1999 -- consolidate ReturnAirPath data structure
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// To manage the return air path.

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES: none

	// OTHER NOTES: none

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataGlobals::BeginEnvrnFlag;
	using DataGlobals::BeginDayFlag;
	using DataZoneEquipment::ReturnAirPath;
	using DataZoneEquipment::NumReturnAirPaths;
	using DataZoneEquipment::ZoneMixer_Type;
	using DataZoneEquipment::ZoneReturnPlenum_Type;

	// Use statements for access to subroutines in other modules

	// Data
	//MODULE PARAMETER DEFINITIONS
	// na

	//DERIVED TYPE DEFINITIONS
	// na

	//MODULE VARIABLE DECLARATIONS:
	// na

	//SUBROUTINE SPECIFICATIONS FOR MODULE ReturnAirPathManager

	// Functions

	void
	SimReturnAirPath()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Russ Taylor
		//       DATE WRITTEN:    Nov 1997

		// PURPOSE OF THIS SUBROUTINE: This subroutine

		// METHODOLOGY EMPLOYED:

		// REFERENCES:

		// USE STATEMENTS:

		// Locals
		int ReturnAirPathNum;
		static bool GetInputFlag( true ); // Flag set to make sure you get input once

		// Obtains and Allocates Mixer related parameters from input file
		if ( GetInputFlag ) { //First time subroutine has been entered
			GetReturnAirPathInput();
			GetInputFlag = false;
		}

		for ( ReturnAirPathNum = 1; ReturnAirPathNum <= NumReturnAirPaths; ++ReturnAirPathNum ) {

			CalcReturnAirPath( ReturnAirPathNum );

		}

	}

	void
	GetReturnAirPathInput()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Russ Taylor
		//       DATE WRITTEN:    Nov 1997

		// PURPOSE OF THIS SUBROUTINE: This subroutine

		// METHODOLOGY EMPLOYED:

		// REFERENCES:

		// USE STATEMENTS:

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::SameString;
		using namespace DataIPShortCuts;
		using NodeInputManager::GetOnlySingleNode;
		using namespace DataLoopNode;

		// Locals
		int PathNum;
		int CompNum;
		int NumAlphas;
		int NumNums;
		int IOStat;
		int Counter;
		static bool ErrorsFound( false );
		bool IsNotOK; // Flag to verify name
		bool IsBlank; // Flag for blank name

		if ( allocated( ReturnAirPath ) ) {
			return;
		}
		cCurrentModuleObject = "AirLoopHVAC:ReturnPath";
		NumReturnAirPaths = GetNumObjectsFound( cCurrentModuleObject );

		if ( NumReturnAirPaths > 0 ) {

			ReturnAirPath.allocate( NumReturnAirPaths );

			for ( PathNum = 1; PathNum <= NumReturnAirPaths; ++PathNum ) {

				GetObjectItem( cCurrentModuleObject, PathNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat );

				IsNotOK = false;
				IsBlank = false;
				VerifyName( cAlphaArgs( 1 ), ReturnAirPath.Name(), PathNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
				if ( IsNotOK ) {
					ErrorsFound = true;
					if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
				}
				ReturnAirPath( PathNum ).Name = cAlphaArgs( 1 );
				ReturnAirPath( PathNum ).NumOfComponents = nint( ( NumAlphas - 2.0 ) / 2.0 );

				ReturnAirPath( PathNum ).OutletNodeNum = GetOnlySingleNode( cAlphaArgs( 2 ), ErrorsFound, cCurrentModuleObject, cAlphaArgs( 1 ), NodeType_Air, NodeConnectionType_Outlet, 1, ObjectIsParent );

				ReturnAirPath( PathNum ).ComponentType.allocate( ReturnAirPath( PathNum ).NumOfComponents );
				ReturnAirPath( PathNum ).ComponentType = "";
				ReturnAirPath( PathNum ).ComponentType_Num.allocate( ReturnAirPath( PathNum ).NumOfComponents );
				ReturnAirPath( PathNum ).ComponentType_Num = 0;
				ReturnAirPath( PathNum ).ComponentName.allocate( ReturnAirPath( PathNum ).NumOfComponents );
				ReturnAirPath( PathNum ).ComponentName = "";
				ReturnAirPath( PathNum ).ComponentIndex.allocate( ReturnAirPath( PathNum ).NumOfComponents );
				ReturnAirPath( PathNum ).ComponentIndex = 0;
				Counter = 3;

				for ( CompNum = 1; CompNum <= ReturnAirPath( PathNum ).NumOfComponents; ++CompNum ) {

					if ( ( SameString( cAlphaArgs( Counter ), "AirLoopHVAC:ZoneMixer" ) ) || ( SameString( cAlphaArgs( Counter ), "AirLoopHVAC:ReturnPlenum" ) ) ) {

						ReturnAirPath( PathNum ).ComponentType( CompNum ) = cAlphaArgs( Counter );
						ReturnAirPath( PathNum ).ComponentName( CompNum ) = cAlphaArgs( Counter + 1 );
						ValidateComponent( ReturnAirPath( PathNum ).ComponentType( CompNum ), ReturnAirPath( PathNum ).ComponentName( CompNum ), IsNotOK, "AirLoopHVAC:ReturnPath" );
						if ( IsNotOK ) {
							ShowContinueError( "In AirLoopHVAC:ReturnPath =" + ReturnAirPath( PathNum ).Name );
							ErrorsFound = true;
						}
						if ( SameString( cAlphaArgs( Counter ), "AirLoopHVAC:ZoneMixer" ) ) ReturnAirPath( PathNum ).ComponentType_Num( CompNum ) = ZoneMixer_Type;
						if ( SameString( cAlphaArgs( Counter ), "AirLoopHVAC:ReturnPlenum" ) ) ReturnAirPath( PathNum ).ComponentType_Num( CompNum ) = ZoneReturnPlenum_Type;
					} else {
						ShowSevereError( "Unhandled component type in AirLoopHVAC:ReturnPath of " + cAlphaArgs( Counter ) );
						ShowContinueError( "Occurs in AirLoopHVAC:ReturnPath = " + ReturnAirPath( PathNum ).Name );
						ShowContinueError( "Must be \"AirLoopHVAC:ZoneMixer\" or \"AirLoopHVAC:ReturnPlenum\"" );
						ErrorsFound = true;
					}

					Counter += 2;

				}

			}

		}

		if ( ErrorsFound ) {
			ShowFatalError( "Errors found getting AirLoopHVAC:ReturnPath.  Preceding condition(s) causes termination." );
		}

	}

	void
	InitReturnAirPath( int & EP_UNUSED( ReturnAirPathNum ) ) // unused1208
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Russ Taylor
		//       DATE WRITTEN:    Nov 1997

		// PURPOSE OF THIS SUBROUTINE: This subroutine

		// METHODOLOGY EMPLOYED:

		// REFERENCES:

		// USE STATEMENTS:

	}

	void
	CalcReturnAirPath( int & ReturnAirPathNum )
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Russ Taylor
		//       DATE WRITTEN:    Nov 1997

		// PURPOSE OF THIS SUBROUTINE: This subroutine

		// METHODOLOGY EMPLOYED:

		// REFERENCES:

		// USE STATEMENTS:

		// Using/Aliasing
		using MixerComponent::SimAirMixer;
		using ZonePlenum::SimAirZonePlenum;
		using DataAirflowNetwork::SimulateAirflowNetwork;
		using DataAirflowNetwork::AirflowNetworkFanActivated;
		using DataAirflowNetwork::AirflowNetworkControlMultizone;

		// Locals
		int ComponentNum;

		for ( ComponentNum = 1; ComponentNum <= ReturnAirPath( ReturnAirPathNum ).NumOfComponents; ++ComponentNum ) {

			{ auto const SELECT_CASE_var( ReturnAirPath( ReturnAirPathNum ).ComponentType_Num( ComponentNum ) );

			if ( SELECT_CASE_var == ZoneMixer_Type ) { // 'AirLoopHVAC:ZoneMixer'

				if ( ! ( AirflowNetworkFanActivated && SimulateAirflowNetwork > AirflowNetworkControlMultizone ) ) {
					SimAirMixer( ReturnAirPath( ReturnAirPathNum ).ComponentName( ComponentNum ), ReturnAirPath( ReturnAirPathNum ).ComponentIndex( ComponentNum ) );
				}

			} else if ( SELECT_CASE_var == ZoneReturnPlenum_Type ) { // 'AirLoopHVAC:ReturnPlenum'

				SimAirZonePlenum( ReturnAirPath( ReturnAirPathNum ).ComponentName( ComponentNum ), ZoneReturnPlenum_Type, ReturnAirPath( ReturnAirPathNum ).ComponentIndex( ComponentNum ) );

			} else {
				ShowSevereError( "Invalid AirLoopHVAC:ReturnPath Component=" + ReturnAirPath( ReturnAirPathNum ).ComponentType( ComponentNum ) );
				ShowContinueError( "Occurs in AirLoopHVAC:ReturnPath =" + ReturnAirPath( ReturnAirPathNum ).Name );
				ShowFatalError( "Preceding condition causes termination." );

			}}

		}

	}

	void
	ReportReturnAirPath( int & EP_UNUSED( ReturnAirPathNum ) ) // unused1208
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR:          Russ Taylor
		//       DATE WRITTEN:    Nov 1997

		// PURPOSE OF THIS SUBROUTINE: This subroutine

		// METHODOLOGY EMPLOYED:

		// REFERENCES:

		// USE STATEMENTS:

	}

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

} // ReturnAirPathManager

} // EnergyPlus
