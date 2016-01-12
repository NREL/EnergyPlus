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

	namespace {
		bool GetInputFlag( true );
		bool ErrorsFound( false );
	}

	//DERIVED TYPE DEFINITIONS
	// na

	//MODULE VARIABLE DECLARATIONS:
	// na

	//SUBROUTINE SPECIFICATIONS FOR MODULE ReturnAirPathManager

	// Functions

	void
	clear_state()
	{
		GetInputFlag = true;
		ErrorsFound = false;
	}

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
		//////////// hoisted into namespace ////////////////////////////////////////////////
		// static bool GetInputFlag( true ); // Flag set to make sure you get input once
		////////////////////////////////////////////////////////////////////////////////////

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
		//////////// hoisted into namespace ////////////////////////////////////////////////
		// static bool ErrorsFound( false );
		////////////////////////////////////////////////////////////////////////////////////
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
				VerifyName( cAlphaArgs( 1 ), ReturnAirPath, PathNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
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

} // ReturnAirPathManager

} // EnergyPlus
