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
#include <limits>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array2D.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <RoomAirModelManager.hh>
#include <CrossVentMgr.hh>
#include <DataAirflowNetwork.hh>
#include <DataEnvironment.hh>
#include <DataErrorTracking.hh>
#include <DataGlobals.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHVACGlobals.hh>
#include <DataIPShortCuts.hh>
#include <DataLoopNode.hh>
#include <DataPrecisionGlobals.hh>
#include <DataRoomAirModel.hh>
#include <DataSurfaces.hh>
#include <DataUCSDSharedData.hh>
#include <DataZoneEquipment.hh>
#include <DisplacementVentMgr.hh>
#include <Fans.hh>
#include <General.hh>
#include <InternalHeatGains.hh>
#include <InputProcessor.hh>
#include <MundtSimMgr.hh>
#include <OutputProcessor.hh>
#include <Psychrometrics.hh>
#include <RoomAirModelAirflowNetwork.hh>
#include <RoomAirModelUserTempPattern.hh>
#include <ScheduleManager.hh>
#include <UFADManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace RoomAirModelManager {

	// MODULE INFORMATION
	//       AUTHOR         Weixiu Kong
	//       DATE WRITTEN   March 2003
	//       MODIFIED       July 2003, CC
	//                      Aug, 2005, BG
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// Contains subroutines for managing the room air models

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataGlobals; // ,                ONLY : MaxNameLength
	using InputProcessor::SameString;
	using namespace DataRoomAirModel;
	using General::RoundSigDigits;

	// Data
	// MODULE PARAMETER DEFINITIONS
	static std::string const BlankString;

	// DERIVED TYPE DEFINITIONS
	// na

	// MODULE VARIABLE DECLARATIONS:

	bool GetUCSDDVDataFlag( true ); // UCSD
	bool GetAirModelData( true ); // Used to "get" all air model data

	// SUBROUTINE SPECIFICATIONS FOR MODULE

	// MODULE SUBROUTINES:

	// Functions

	void
	clear_state()
	{
		GetUCSDDVDataFlag = true;
		GetAirModelData = true;
	}

	void
	ManageAirModel( int & ZoneNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Weixiu Kong
		//       DATE WRITTEN   April 2003
		//       MODIFIED       July 2003, CC
		//                      Jan 2004, CC
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//     manage room air models.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using MundtSimMgr::ManageMundtModel;
		using DisplacementVentMgr::ManageUCSDDVModel;
		using CrossVentMgr::ManageUCSDCVModel;
		using RoomAirModelUserTempPattern::ManageUserDefinedPatterns;
		using UFADManager::ManageUCSDUFModels;
		using RoomAirModelAirflowNetwork::SimRoomAirModelAirflowNetwork;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
//		static bool GetAirModelData( true ); // Used to "get" all air model data

		// FLOW:
		if ( GetAirModelData ) {
			GetAirModelDatas();
			GetAirModelData = false;
		}

		if ( UCSDModelUsed ) {
			SharedDVCVUFDataInit( ZoneNum );
		}

		{ auto const SELECT_CASE_var( AirModel( ZoneNum ).AirModelType );

		if ( SELECT_CASE_var == RoomAirModel_UserDefined ) {

			ManageUserDefinedPatterns( ZoneNum );

		} else if ( SELECT_CASE_var == RoomAirModel_Mixing ) { // Mixing air model
			// do nothing

		} else if ( SELECT_CASE_var == RoomAirModel_Mundt ) { // Mundt air model
			// simulate room airflow using Mundt model
			ManageMundtModel( ZoneNum );

		} else if ( SELECT_CASE_var == RoomAirModel_UCSDDV ) { //UCDV Displacement Ventilation model
			// simulate room airflow using UCSDDV model
			ManageUCSDDVModel( ZoneNum );

		} else if ( SELECT_CASE_var == RoomAirModel_UCSDCV ) { //UCSD Cross Ventilation model
			// simulate room airflow using UCSDDV model
			ManageUCSDCVModel( ZoneNum );

		} else if ( SELECT_CASE_var == RoomAirModel_UCSDUFI ) { // UCSD UFAD interior zone model
			// simulate room airflow using the UCSDUFI model
			ManageUCSDUFModels( ZoneNum, RoomAirModel_UCSDUFI );

		} else if ( SELECT_CASE_var == RoomAirModel_UCSDUFE ) { // UCSD UFAD exterior zone model
			// simulate room airflow using the UCSDUFE model
			ManageUCSDUFModels( ZoneNum, RoomAirModel_UCSDUFE );

		} else if ( SELECT_CASE_var == RoomAirModel_AirflowNetwork ) { // RoomAirflowNetwork zone model
			// simulate room airflow using the AirflowNetwork - based model
			SimRoomAirModelAirflowNetwork( ZoneNum );

		} else { // mixing air model
			// do nothing
		}}

	}

	//*****************************************************************************************

	void
	GetAirModelDatas()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   March 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine "gets" all the data for the "RoomAir" models by calling individual
		// routines.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

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
		bool ErrorsFound;

		ErrorsFound = false;
		// get air node input data for all zones
		GetAirNodeData( ErrorsFound );

		// get mundt model controls for all zones
		GetMundtData( ErrorsFound );

		// get airflow network model info for all zones
		GetRoomAirflowNetworkData( ErrorsFound );

		// get UCSDDV model controls for all zones
		GetDisplacementVentData( ErrorsFound );

		// get UCSDCV model controls for all zones
		GetCrossVentData( ErrorsFound );

		// get BTG's user-defined patterns for all zones
		GetUserDefinedPatternData( ErrorsFound );

		// get UCSD UFAD interior zone model controls for all zones
		// get UCSD UFAD exterior zone model controls for all zones
		GetUFADZoneData( ErrorsFound );

		if ( ErrorsFound ) {
			ShowFatalError( "GetAirModelData: Errors found getting air model input.  Program terminates." );
		}

	}

	void
	GetUserDefinedPatternData( bool & ErrorsFound ) // True if errors found during this get input routine
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Aug 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine "gets" all the data for the "User-Defined RoomAir"

		// METHODOLOGY EMPLOYED:
		// usual energyplus input routines
		// for the actual patterns, a single structure array holds
		// different patterns in nested derived types.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na
		// Using/Aliasing
		using DataGlobals::NumOfZones;
		using InputProcessor::FindItemInList;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::GetObjectDefMaxArgs;
		using namespace DataIPShortCuts;
		using DataSurfaces::Surface;
		using DataSurfaces::SurfaceClass_IntMass;
		using DataHeatBalance::Zone;
		using ScheduleManager::GetScheduleIndex;
		using RoomAirModelUserTempPattern::FigureNDheightInZone;
		using DataZoneEquipment::ZoneEquipConfig;
		using DataZoneEquipment::EquipConfiguration;
		using DataErrorTracking::TotalWarningErrors;
		using DataErrorTracking::TotalRoomAirPatternTooLow;
		using DataErrorTracking::TotalRoomAirPatternTooHigh;
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetUserDefinedPatternData: " );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumAlphas; // number of alphas
		int NumNumbers; // Number of numbers encountered
		int Status; // Notes if there was an error in processing the input

		int thisSurfinZone; // working variable for indexing surfaces within a ZoneInfo structure
		int thisHBsurfID; // working variable for indexing surfaces in main Surface structure
		int thisPattern;

		//unused1208  INTEGER         :: void  ! unused integer needed for parameter in subroutine call
		//unused1208  INTEGER         :: MaxAlphaCount !max number of alphas in a type of extensible object
		//unused1208  INTEGER         :: MaxNumCount !Max number of Numbers in a type of extensible object

		int i; // do loop indexer
		int NumPairs; // number of zeta/deltaTai pairs
		int ObjNum; // loop indexer of input objects if the same type
		int ZoneNum; // zone number in heat balance domain
		int found; // test for FindItemInList

		//access input file and setup
		numTempDistContrldZones = GetNumObjectsFound( cUserDefinedControlObject );

		NumConstantGradient = GetNumObjectsFound( cTempPatternConstGradientObject );
		NumTwoGradientInterp = GetNumObjectsFound( cTempPatternTwoGradientObject );
		NumNonDimensionalHeight = GetNumObjectsFound( cTempPatternNDHeightObject );
		NumSurfaceMapping = GetNumObjectsFound( cTempPatternSurfMapObject );

		NumAirTempPatterns = NumConstantGradient + NumTwoGradientInterp + NumNonDimensionalHeight + NumSurfaceMapping;

		cCurrentModuleObject = cUserDefinedControlObject;
		if ( numTempDistContrldZones == 0 ) {
			if ( NumAirTempPatterns != 0 ) { // user may have missed control object
				ShowWarningError( "Missing " + cCurrentModuleObject + " object needed to use roomair temperature patterns" );
				// ErrorsFound = .TRUE.
			}
			return;
		}

		// now allocate AirPatternZoneInfo to length of all zones for easy indexing
		if ( ! allocated( AirPatternZoneInfo ) ) {
			AirPatternZoneInfo.allocate( NumOfZones );
		}

		for ( ObjNum = 1; ObjNum <= numTempDistContrldZones; ++ObjNum ) {

			GetObjectItem( cCurrentModuleObject, ObjNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, Status, _, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			//first get zone ID
			ZoneNum = FindItemInList( cAlphaArgs( 2 ), Zone );
			if ( ZoneNum == 0 ) { //throw error
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid data." );
				ShowContinueError( "Invalid-not found " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\"." );
				ErrorsFound = true;
				return; // halt to avoid hard crash
			}
			AirPatternZoneInfo( ZoneNum ).IsUsed = true;
			AirPatternZoneInfo( ZoneNum ).Name = cAlphaArgs( 1 ); //Name of this Control Object
			AirPatternZoneInfo( ZoneNum ).ZoneName = cAlphaArgs( 2 ); // Zone Name

			AirPatternZoneInfo( ZoneNum ).AvailSched = cAlphaArgs( 3 );
			if ( lAlphaFieldBlanks( 3 ) ) {
				AirPatternZoneInfo( ZoneNum ).AvailSchedID = ScheduleAlwaysOn;
			} else {
				AirPatternZoneInfo( ZoneNum ).AvailSchedID = GetScheduleIndex( cAlphaArgs( 3 ) );
				if ( AirPatternZoneInfo( ZoneNum ).AvailSchedID == 0 ) {
					ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid data." );
					ShowContinueError( "Invalid-not found " + cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 3 ) + "\"." );
					ErrorsFound = true;
				}
			}

			AirPatternZoneInfo( ZoneNum ).PatternCntrlSched = cAlphaArgs( 4 ); // Schedule Name for Leading Pattern Control for this Zone
			AirPatternZoneInfo( ZoneNum ).PatternSchedID = GetScheduleIndex( cAlphaArgs( 4 ) );
			if ( AirPatternZoneInfo( ZoneNum ).PatternSchedID == 0 ) {
				ShowSevereError( RoutineName + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid data." );
				ShowContinueError( "Invalid-not found " + cAlphaFieldNames( 4 ) + "=\"" + cAlphaArgs( 4 ) + "\"." );
				ErrorsFound = true;
			}

			AirPatternZoneInfo( ZoneNum ).ZoneID = ZoneNum;

			//   figure number of surfaces for this zone
			AirPatternZoneInfo( ZoneNum ).totNumSurfs = Zone( ZoneNum ).SurfaceLast - Zone( ZoneNum ).SurfaceFirst + 1;
			//   allocate nested derived type for surface info
			AirPatternZoneInfo( ZoneNum ).Surf.allocate( AirPatternZoneInfo( ZoneNum ).totNumSurfs );

			//   Fill in what we know for nested structure for surfaces
			for ( thisSurfinZone = 1; thisSurfinZone <= AirPatternZoneInfo( ZoneNum ).totNumSurfs; ++thisSurfinZone ) {
				thisHBsurfID = Zone( ZoneNum ).SurfaceFirst + thisSurfinZone - 1;
				if ( Surface( thisHBsurfID ).Class == SurfaceClass_IntMass ) {
					AirPatternZoneInfo( ZoneNum ).Surf( thisSurfinZone ).SurfID = thisHBsurfID;
					AirPatternZoneInfo( ZoneNum ).Surf( thisSurfinZone ).Zeta = 0.5;
					continue;
				}

				AirPatternZoneInfo( ZoneNum ).Surf( thisSurfinZone ).SurfID = thisHBsurfID;

				AirPatternZoneInfo( ZoneNum ).Surf( thisSurfinZone ).Zeta = FigureNDheightInZone( thisHBsurfID );

			} //loop through surfaces in this zone

		} // loop through number of 'RoomAir:TemperaturePattern:UserDefined' objects

		// Check against AirModel.  Make sure there is a match here.
		for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
			if ( AirModel( ZoneNum ).AirModelType != RoomAirModel_UserDefined ) continue;
			if ( AirPatternZoneInfo( ZoneNum ).IsUsed ) continue; // There is a Room Air Temperatures object for this zone
			ShowSevereError( RoutineName + "AirModel for Zone=[" + Zone( ZoneNum ).Name + "] is indicated as \"User Defined\"." );
			ShowContinueError( "...but missing a " + cCurrentModuleObject + " object for control." );
			ErrorsFound = true;
		}

		// now get user defined temperature patterns
		if ( ! allocated( RoomAirPattern ) ) {
			RoomAirPattern.allocate( NumAirTempPatterns );
		}

		// Four different objects to get
		cCurrentModuleObject = cTempPatternConstGradientObject;
		for ( ObjNum = 1; ObjNum <= NumConstantGradient; ++ObjNum ) {
			thisPattern = ObjNum;
			GetObjectItem( cCurrentModuleObject, ObjNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, Status, _, _, cAlphaFieldNames, cNumericFieldNames );

			RoomAirPattern( thisPattern ).Name = cAlphaArgs( 1 );
			RoomAirPattern( thisPattern ).PatrnID = rNumericArgs( 1 );
			RoomAirPattern( thisPattern ).PatternMode = ConstGradTempPattern;
			RoomAirPattern( thisPattern ).DeltaTstat = rNumericArgs( 2 );
			RoomAirPattern( thisPattern ).DeltaTleaving = rNumericArgs( 3 );
			RoomAirPattern( thisPattern ).DeltaTexhaust = rNumericArgs( 4 );
			RoomAirPattern( thisPattern ).GradPatrn.Gradient = rNumericArgs( 5 );

		}

		cCurrentModuleObject = cTempPatternTwoGradientObject;
		for ( ObjNum = 1; ObjNum <= NumTwoGradientInterp; ++ObjNum ) {
			thisPattern = NumConstantGradient + ObjNum;
			GetObjectItem( cCurrentModuleObject, ObjNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, Status, _, _, cAlphaFieldNames, cNumericFieldNames );
			RoomAirPattern( thisPattern ).PatternMode = TwoGradInterpPattern;
			RoomAirPattern( thisPattern ).Name = cAlphaArgs( 1 );
			RoomAirPattern( thisPattern ).PatrnID = rNumericArgs( 1 );
			RoomAirPattern( thisPattern ).TwoGradPatrn.TstatHeight = rNumericArgs( 2 );
			RoomAirPattern( thisPattern ).TwoGradPatrn.TleavingHeight = rNumericArgs( 3 );
			RoomAirPattern( thisPattern ).TwoGradPatrn.TexhaustHeight = rNumericArgs( 4 );
			RoomAirPattern( thisPattern ).TwoGradPatrn.LowGradient = rNumericArgs( 5 );
			RoomAirPattern( thisPattern ).TwoGradPatrn.HiGradient = rNumericArgs( 6 );

			if ( SameString( cAlphaArgs( 2 ), "OutdoorDryBulbTemperature" ) ) {
				RoomAirPattern( thisPattern ).TwoGradPatrn.InterpolationMode = OutdoorDryBulbMode;
			} else if ( SameString( cAlphaArgs( 2 ), "ZoneDryBulbTemperature" ) ) {
				RoomAirPattern( thisPattern ).TwoGradPatrn.InterpolationMode = ZoneAirTempMode;
			} else if ( SameString( cAlphaArgs( 2 ), "ZoneAndOutdoorTemperatureDifference" ) ) {
				RoomAirPattern( thisPattern ).TwoGradPatrn.InterpolationMode = DeltaOutdoorZone;
			} else if ( SameString( cAlphaArgs( 2 ), "SensibleCoolingLoad" ) ) {
				RoomAirPattern( thisPattern ).TwoGradPatrn.InterpolationMode = SensibleCoolingMode;
			} else if ( SameString( cAlphaArgs( 2 ), "SensibleHeatingLoad" ) ) {
				RoomAirPattern( thisPattern ).TwoGradPatrn.InterpolationMode = SensibleHeatingMode;
			} else {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 2 ) + " = " + cAlphaArgs( 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}

			RoomAirPattern( thisPattern ).TwoGradPatrn.UpperBoundTempScale = rNumericArgs( 7 );
			RoomAirPattern( thisPattern ).TwoGradPatrn.LowerBoundTempScale = rNumericArgs( 8 );

			RoomAirPattern( thisPattern ).TwoGradPatrn.UpperBoundHeatRateScale = rNumericArgs( 9 );
			RoomAirPattern( thisPattern ).TwoGradPatrn.LowerBoundHeatRateScale = rNumericArgs( 10 );

			// now test the input some
			if ( RoomAirPattern( thisPattern ).TwoGradPatrn.HiGradient == RoomAirPattern( thisPattern ).TwoGradPatrn.LowGradient ) {
				ShowWarningError( "Upper and lower gradients equal, use " + cTempPatternConstGradientObject + " instead " );
				ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
			}
			if ( ( RoomAirPattern( thisPattern ).TwoGradPatrn.UpperBoundTempScale == RoomAirPattern( thisPattern ).TwoGradPatrn.LowerBoundTempScale ) && ( ( RoomAirPattern( thisPattern ).TwoGradPatrn.InterpolationMode == OutdoorDryBulbMode ) || ( RoomAirPattern( thisPattern ).TwoGradPatrn.InterpolationMode == ZoneAirTempMode ) || ( RoomAirPattern( thisPattern ).TwoGradPatrn.InterpolationMode == DeltaOutdoorZone ) ) ) {
				// throw error, will cause divide by zero when used for scaling
				ShowSevereError( "Error in temperature scale in " + cCurrentModuleObject + ": " + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}
			if ( ( RoomAirPattern( thisPattern ).TwoGradPatrn.HiGradient == RoomAirPattern( thisPattern ).TwoGradPatrn.LowGradient ) && ( ( RoomAirPattern( thisPattern ).TwoGradPatrn.InterpolationMode == SensibleCoolingMode ) || ( RoomAirPattern( thisPattern ).TwoGradPatrn.InterpolationMode == SensibleHeatingMode ) ) ) {
				// throw error, will cause divide by zero when used for scaling
				ShowSevereError( "Error in load scale in " + cCurrentModuleObject + ": " + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}

		}

		cCurrentModuleObject = cTempPatternNDHeightObject;
		for ( ObjNum = 1; ObjNum <= NumNonDimensionalHeight; ++ObjNum ) {
			thisPattern = NumConstantGradient + NumTwoGradientInterp + ObjNum;
			RoomAirPattern( thisPattern ).PatternMode = NonDimenHeightPattern;

			GetObjectItem( cCurrentModuleObject, ObjNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, Status, _, _, cAlphaFieldNames, cNumericFieldNames );
			RoomAirPattern( thisPattern ).Name = cAlphaArgs( 1 );
			RoomAirPattern( thisPattern ).PatrnID = rNumericArgs( 1 );
			RoomAirPattern( thisPattern ).DeltaTstat = rNumericArgs( 2 );
			RoomAirPattern( thisPattern ).DeltaTleaving = rNumericArgs( 3 );
			RoomAirPattern( thisPattern ).DeltaTexhaust = rNumericArgs( 4 );

			NumPairs = std::floor( ( double( NumNumbers ) - 4.0 ) / 2.0 );

			// TODO error checking

			RoomAirPattern( thisPattern ).VertPatrn.ZetaPatrn.allocate( NumPairs );
			RoomAirPattern( thisPattern ).VertPatrn.DeltaTaiPatrn.allocate( NumPairs );

			// init these since they can't be in derived type
			RoomAirPattern( thisPattern ).VertPatrn.ZetaPatrn = 0.0;
			RoomAirPattern( thisPattern ).VertPatrn.DeltaTaiPatrn = 0.0;

			for ( i = 0; i <= NumPairs - 1; ++i ) {

				RoomAirPattern( thisPattern ).VertPatrn.ZetaPatrn( i + 1 ) = rNumericArgs( 2 * i + 5 );
				RoomAirPattern( thisPattern ).VertPatrn.DeltaTaiPatrn( i + 1 ) = rNumericArgs( 2 * i + 6 );

			}

			//TODO  check order (TODO sort ? )
			for ( i = 2; i <= NumPairs; ++i ) {
				if ( RoomAirPattern( thisPattern ).VertPatrn.ZetaPatrn( i ) < RoomAirPattern( thisPattern ).VertPatrn.ZetaPatrn( i - 1 ) ) {
					ShowSevereError( "Zeta values not in increasing order in " + cCurrentModuleObject + ": " + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}
			}
		}

		cCurrentModuleObject = cTempPatternSurfMapObject;
		for ( ObjNum = 1; ObjNum <= NumSurfaceMapping; ++ObjNum ) {
			thisPattern = NumConstantGradient + NumTwoGradientInterp + NumNonDimensionalHeight + ObjNum;
			RoomAirPattern( thisPattern ).PatternMode = SurfMapTempPattern;

			GetObjectItem( cCurrentModuleObject, ObjNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, Status, _, _, cAlphaFieldNames, cNumericFieldNames );
			RoomAirPattern( thisPattern ).Name = cAlphaArgs( 1 );
			RoomAirPattern( thisPattern ).PatrnID = rNumericArgs( 1 );
			RoomAirPattern( thisPattern ).DeltaTstat = rNumericArgs( 2 );
			RoomAirPattern( thisPattern ).DeltaTleaving = rNumericArgs( 3 );
			RoomAirPattern( thisPattern ).DeltaTexhaust = rNumericArgs( 4 );

			NumPairs = NumNumbers - 4;

			if ( NumPairs != ( NumAlphas - 1 ) ) {
				ShowSevereError( "Error in number of entries in " + cCurrentModuleObject + " object: " + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}
			RoomAirPattern( thisPattern ).MapPatrn.SurfName.allocate( NumPairs );
			RoomAirPattern( thisPattern ).MapPatrn.DeltaTai.allocate( NumPairs );
			RoomAirPattern( thisPattern ).MapPatrn.SurfID.allocate( NumPairs );

			// init just allocated
			RoomAirPattern( thisPattern ).MapPatrn.SurfName = "";
			RoomAirPattern( thisPattern ).MapPatrn.DeltaTai = 0.0;
			RoomAirPattern( thisPattern ).MapPatrn.SurfID = 0;

			for ( i = 1; i <= NumPairs; ++i ) {
				RoomAirPattern( thisPattern ).MapPatrn.SurfName( i ) = cAlphaArgs( i + 1 );
				RoomAirPattern( thisPattern ).MapPatrn.DeltaTai( i ) = rNumericArgs( i + 4 );
				found = FindItemInList( cAlphaArgs( i + 1 ), Surface );
				if ( found != 0 ) {
					RoomAirPattern( thisPattern ).MapPatrn.SurfID( i ) = found;
				} else {
					ShowSevereError( "Surface name not found in " + cCurrentModuleObject + " object: " + cAlphaArgs( 1 ) );
					ErrorsFound = true;
				}

			}
			RoomAirPattern( thisPattern ).MapPatrn.NumSurfs = NumPairs;

		}

		if ( TotalRoomAirPatternTooLow > 0 ) {
			ShowWarningError( "GetUserDefinedPatternData: RoomAirModelUserTempPattern: " + RoundSigDigits( TotalRoomAirPatternTooLow ) + " problem(s) in non-dimensional height calculations, too low surface height(s) in relation to floor height of zone(s)." );
			ShowContinueError( "...Use OutputDiagnostics,DisplayExtraWarnings; to see details." );
			TotalWarningErrors += TotalRoomAirPatternTooLow;
		}
		if ( TotalRoomAirPatternTooHigh > 0 ) {
			ShowWarningError( "GetUserDefinedPatternData: RoomAirModelUserTempPattern: " + RoundSigDigits( TotalRoomAirPatternTooHigh ) + " problem(s) in non-dimensional height calculations, too high surface height(s) in relation to ceiling height of zone(s)." );
			ShowContinueError( "...Use OutputDiagnostics,DisplayExtraWarnings; to see details." );
			TotalWarningErrors += TotalRoomAirPatternTooHigh;
		}

		// now do one time setups from and checks on user data

		// Find and set return and exhaust node ids

		for ( i = 1; i <= NumOfZones; ++i ) {
			if ( AirPatternZoneInfo( i ).IsUsed ) {
				// first get return and exhaust air node index
				found = FindItemInList( AirPatternZoneInfo( i ).ZoneName, ZoneEquipConfig, &EquipConfiguration::ZoneName );
				if ( found != 0 ) {

					AirPatternZoneInfo( i ).ReturnAirNodeID = ZoneEquipConfig( found ).ReturnAirNode;
					AirPatternZoneInfo( i ).ZoneNodeID = ZoneEquipConfig( found ).ZoneNode;
					if ( allocated( ZoneEquipConfig( found ).ExhaustNode ) ) {
						AirPatternZoneInfo( i ).ExhaustAirNodeID.allocate( ZoneEquipConfig( found ).NumExhaustNodes );
						AirPatternZoneInfo( i ).ExhaustAirNodeID = ZoneEquipConfig( found ).ExhaustNode;
					} //exhaust nodes present
				} //found ZoneEquipConf

				// second get zone height values
				AirPatternZoneInfo( i ).ZoneHeight = Zone( i ).CeilingHeight;

			} //air pattern is used
		}

	}

	void
	GetAirNodeData( bool & ErrorsFound ) // True if errors found during this get input routine
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   August 2001
		//       RE-ENGINEERED  April 2003, Weixiu Kong
		//       MODIFIED       July 2003, CC
		//                      Jan 2004, CC

		// PURPOSE OF THIS SUBROUTINE:
		//     Get AirNode data for all zones at once

		// METHODOLOGY EMPLOYED:
		//     Use input processer to get input from idf file

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::GetObjectItemNum;
		using InputProcessor::FindItemInList;
		using InputProcessor::VerifyName;
		using namespace DataIPShortCuts;
		using DataHeatBalance::Zone;
		using DataSurfaces::Surface;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumAlphas; // States which alpha value to read from a
		// "Number" line
		int NumNumbers; // Number of numbers encountered
		int Status; // Notes if there was an error in processing the input
		int AirNodeNum; // Index number for air nodes
		int ZoneNum; // Index number for zones
		int NumSurfsInvolved; // Number of surfaces involved with air nodes
		int SurfCount; // Number of surfaces involved with air nodes
		// (used for checking error)
		int SurfNum; // Index number for surfaces
		int SurfFirst; // Index number for first surface of zones
		int NumOfSurfs; // Index number for last surface of zones
		int ListSurfNum; // Index number of surfaces listed in the air node object
		bool SurfNeeded;
		bool IsNotOK;
		bool IsBlank;

		// FLOW:

		if ( ! MundtModelUsed ) return;

		// Initialize default values for air nodes
		TotNumOfZoneAirNodes.allocate( NumOfZones );
		TotNumOfAirNodes = 0;
		TotNumOfZoneAirNodes = 0;

		cCurrentModuleObject = "RoomAir:Node";
		TotNumOfAirNodes = GetNumObjectsFound( cCurrentModuleObject );

		if ( TotNumOfAirNodes <= 0 ) {
			// no air node object is found, terminate the program
			ShowSevereError( "No " + cCurrentModuleObject + " objects found in input." );
			ShowContinueError( "The OneNodeDisplacementVentilation model requires " + cCurrentModuleObject + " objects" );
			ErrorsFound = true;
			return;
		} else {
			// air node objects are found so allocate airnode variable
			AirNode.allocate( TotNumOfAirNodes );
		}

		for ( AirNodeNum = 1; AirNodeNum <= TotNumOfAirNodes; ++AirNodeNum ) {

			// get air node objects
			GetObjectItem( cCurrentModuleObject, AirNodeNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, Status, _, _, cAlphaFieldNames, cNumericFieldNames );
			IsNotOK = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), AirNode, AirNodeNum - 1, IsNotOK, IsBlank, cCurrentModuleObject + " Name" );
			if ( IsNotOK ) {
				ErrorsFound = true;
				if ( IsBlank ) cAlphaArgs( 1 ) = "xxxxx";
			}
			AirNode( AirNodeNum ).Name = cAlphaArgs( 1 );

			AirNode( AirNodeNum ).ZoneName = cAlphaArgs( 3 ); // Zone name
			AirNode( AirNodeNum ).ZonePtr = FindItemInList( AirNode( AirNodeNum ).ZoneName, Zone );
			if ( AirNode( AirNodeNum ).ZonePtr == 0 ) {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 3 ) + " = " + cAlphaArgs( 3 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			} else {
				ZoneNum = AirNode( AirNodeNum ).ZonePtr;
				NumOfSurfs = Zone( ZoneNum ).SurfaceLast - Zone( ZoneNum ).SurfaceFirst + 1;
				AirNode( AirNodeNum ).SurfMask.allocate( NumOfSurfs );
			}

			{ auto const nodeType( cAlphaArgs( 2 ) );
			if ( nodeType == "INLET" ) {
				AirNode( AirNodeNum ).ClassType = InletAirNode;
			} else if ( nodeType == "FLOOR" ) {
				AirNode( AirNodeNum ).ClassType = FloorAirNode;
			} else if ( nodeType == "CONTROL" ) {
				AirNode( AirNodeNum ).ClassType = ControlAirNode;
			} else if ( nodeType == "CEILING" ) {
				AirNode( AirNodeNum ).ClassType = CeilingAirNode;
			} else if ( nodeType == "MUNDTROOM" ) {
				AirNode( AirNodeNum ).ClassType = MundtRoomAirNode;
			} else if ( nodeType == "RETURN" ) {
				AirNode( AirNodeNum ).ClassType = ReturnAirNode;
				//            CASE ('PLUME1')
				//                AirNode(AirNodeNum)%ClassType   = PlumeAirNode1
				//            CASE ('PLUME2')
				//                AirNode(AirNodeNum)%ClassType   = PlumeAirNode2
				//            CASE ('PLUME3')
				//                AirNode(AirNodeNum)%ClassType   = PlumeAirNode3
				//            CASE ('PLUME4')
				//                AirNode(AirNodeNum)%ClassType   = PlumeAirNode4
				//            CASE ('REESROOM1')
				//                AirNode(AirNodeNum)%ClassType   = RoomAirNode1
				//            CASE ('REESROOM2')
				//                AirNode(AirNodeNum)%ClassType   = RoomAirNode2
				//            CASE ('REESROOM3')
				//                AirNode(AirNodeNum)%ClassType   = RoomAirNode3
				//            CASE ('REESROOM4')
				//                AirNode(AirNodeNum)%ClassType   = RoomAirNode4
			} else {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 2 ) + " = " + cAlphaArgs( 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}}

			AirNode( AirNodeNum ).Height = rNumericArgs( 1 ); // Air node height
			NumSurfsInvolved = NumAlphas - 3; // Number of surfaces involved with air nodes

			// Initialize
			AirNode( AirNodeNum ).SurfMask = false;

			if ( NumSurfsInvolved <= 0 ) {

				// report severe error since the following air nodes require surfaces associated with them
				{ auto const nodeType( cAlphaArgs( 2 ) );
				if ( nodeType == "FLOOR" || nodeType == "CEILING" || nodeType == "MUNDTROOM" || nodeType == "PLUME4" || nodeType == "REESROOM1" || nodeType == "REESROOM2" || nodeType == "REESROOM3" || nodeType == "REESROOM4" ) {
					// terminate the program due to a severe error in the specified input
					ShowSevereError( "GetAirNodeData: " + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid air node specification." );
					ShowContinueError( "Mundt Room Air Model: No surface names specified.  Air node=\"" + AirNode( AirNodeNum ).Name + " requires name of surfaces associated with it." );
					ErrorsFound = true;
				} else {
				}}

			} else {

				// initialize
				SurfNeeded = true;

				// report warning error since the following air nodes do not require surfaces associated with them
				// and assign .FALSE. to 'SurfNeeded'
				{ auto const nodeType( cAlphaArgs( 2 ) );
				if ( nodeType == "INLET" || nodeType == "CONTROL" || nodeType == "RETURN" || nodeType == "PLUME1" || nodeType == "PLUME2" || nodeType == "PLUME3" ) {
					ShowWarningError( "GetAirNodeData: " + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid linkage" );
					ShowContinueError( "Mundt Room Air Model: No surface names needed.  Air node=\"" + AirNode( AirNodeNum ).Name + " does not relate to any surfaces." );
					SurfNeeded = false;
				} else {
				}}

				if ( SurfNeeded ) {

					// this air node is in this zone; hence, first get name of all surfaces in this zone
					ZoneNum = AirNode( AirNodeNum ).ZonePtr;
					SurfFirst = Zone( ZoneNum ).SurfaceFirst;
					NumOfSurfs = Zone( ZoneNum ).SurfaceLast - Zone( ZoneNum ).SurfaceFirst + 1;

					// terminate the program due to a severe error in the specified input
					if ( ( NumSurfsInvolved ) > NumOfSurfs ) {
						ShowFatalError( "GetAirNodeData: Mundt Room Air Model: Number of surfaces connected to " + AirNode( AirNodeNum ).Name + " is greater than number of surfaces in " + Zone( ZoneNum ).Name );
						return;
					}

					// relate surfaces to this air node and check to see whether surface names are specified correctly or not
					SurfCount = 0;
					--SurfFirst;
					for ( ListSurfNum = 4; ListSurfNum <= NumAlphas; ++ListSurfNum ) {
						for ( SurfNum = 1; SurfNum <= NumOfSurfs; ++SurfNum ) {
							if ( cAlphaArgs( ListSurfNum ) == Surface( SurfFirst + SurfNum ).Name ) {
								AirNode( AirNodeNum ).SurfMask( SurfNum ) = true;
								++SurfCount;
							}
						}
					}

					// report warning error since surface names are specified correctly
					if ( ( NumSurfsInvolved ) != SurfCount ) {
						ShowWarningError( "GetAirNodeData: Mundt Room Air Model: Some surface names specified for " + AirNode( AirNodeNum ).Name + " are not in " + Zone( ZoneNum ).Name );
					}

				}

			}

		}

		// get number of air nodes in each zone
		for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {

			// this zone uses other air model so skip the rest
			if ( AirModel( ZoneNum ).AirModelType != RoomAirModel_Mundt ) continue;

			// this zone uses a nodal air model so get number of air nodes in each zone
			for ( AirNodeNum = 1; AirNodeNum <= TotNumOfAirNodes; ++AirNodeNum ) {
				if ( SameString( AirNode( AirNodeNum ).ZoneName, Zone( ZoneNum ).Name ) ) {
					++TotNumOfZoneAirNodes( ZoneNum );
				}
			}

		}

	}

	//*****************************************************************************************

	void
	GetMundtData( bool & ErrorsFound ) // True if errors found during this get input routine
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   August 2001
		//       MODIFIED       na
		//       RE-ENGINEERED  April 2003, Weixiu Kong
		//                      July 2003, CC

		// PURPOSE OF THIS SUBROUTINE:
		//     Get Mundt model controls for all zones at once

		// METHODOLOGY EMPLOYED:
		//     Use input processer to get input from idf file

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using namespace DataIPShortCuts;
		using DataHeatBalance::Zone;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumAlphas;
		int NumNumbers; // Number of numbers encountered
		int Status; // Notes if there was an error in processing the input
		int ControlNum; // Index number
		int NumOfMundtContrl; // Number of Mundt Model Controls
		int ZoneNum; // Index number for zones

		// FLOW:

		if ( ! MundtModelUsed ) return;

		// Initialize default values for Mundt model controls
		ConvectiveFloorSplit.allocate( NumOfZones );
		InfiltratFloorSplit.allocate( NumOfZones );
		ConvectiveFloorSplit = 0.0;
		InfiltratFloorSplit = 0.0;

		cCurrentModuleObject = "RoomAirSettings:OneNodeDisplacementVentilation";
		NumOfMundtContrl = GetNumObjectsFound( cCurrentModuleObject );
		if ( NumOfMundtContrl > NumOfZones ) {
			ShowSevereError( "Too many " + cCurrentModuleObject + " objects in input file" );
			ShowContinueError( "There cannot be more " + cCurrentModuleObject + " objects than number of zones." );
			ErrorsFound = true;
		}

		if ( NumOfMundtContrl == 0 ) {
			ShowWarningError( "No " + cCurrentModuleObject + " objects found, program assumes no convection or infiltration gains near floors" );
			return;
		}

		// this zone uses Mundt model so get Mundt Model Control
		// loop through all 'RoomAirSettings:OneNodeDisplacementVentilation' objects
		for ( ControlNum = 1; ControlNum <= NumOfMundtContrl; ++ControlNum ) {
			GetObjectItem( cCurrentModuleObject, ControlNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, Status, _, _, cAlphaFieldNames, cNumericFieldNames );
			ZoneNum = FindItemInList( cAlphaArgs( 1 ), Zone );
			if ( ZoneNum == 0 ) {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 1 ) + " = " + cAlphaArgs( 1 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
				ShowContinueError( "Not a valid zone name." );
				ErrorsFound = true;
				continue;
			}
			if ( AirModel( ZoneNum ).AirModelType != RoomAirModel_Mundt ) {
				ShowSevereError( "Zone specified=\"" + cAlphaArgs( 1 ) + "\", Air Model type is not OneNodeDisplacementVentilation." );
				ShowContinueError( "Air Model Type for zone=" + ChAirModel( AirModel( ZoneNum ).AirModelType ) );
				ErrorsFound = true;
				continue;
			}
			ConvectiveFloorSplit( ZoneNum ) = rNumericArgs( 1 );
			InfiltratFloorSplit( ZoneNum ) = rNumericArgs( 2 );
		}

	}

	void
	GetDisplacementVentData( bool & ErrorsFound ) // True if errors found during this get input routine
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         G. Carrilho da Graca
		//       DATE WRITTEN   January 2004
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS SUBROUTINE:
		//  Get UCSD Displacement ventilation model controls for all zones at once

		// METHODOLOGY EMPLOYED:
		// Use input processor to get input from idf file

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using namespace DataIPShortCuts;
		using DataHeatBalance::Zone;
		using namespace ScheduleManager;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int IOStat;
		int NumAlpha;
		int NumNumber;
		int Loop;

		if ( ! UCSDModelUsed ) return;
		cCurrentModuleObject = "RoomAirSettings:ThreeNodeDisplacementVentilation";
		TotUCSDDV = GetNumObjectsFound( cCurrentModuleObject );

		if ( TotUCSDDV <= 0 ) return;

		ZoneUCSDDV.allocate( TotUCSDDV );

		for ( Loop = 1; Loop <= TotUCSDDV; ++Loop ) {

			GetObjectItem( cCurrentModuleObject, Loop, cAlphaArgs, NumAlpha, rNumericArgs, NumNumber, IOStat, _, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			// First is Zone Name
			ZoneUCSDDV( Loop ).ZoneName = cAlphaArgs( 1 );
			ZoneUCSDDV( Loop ).ZonePtr = FindItemInList( cAlphaArgs( 1 ), Zone );
			if ( ZoneUCSDDV( Loop ).ZonePtr == 0 ) {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 1 ) + " = " + cAlphaArgs( 1 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
				ShowContinueError( "Zone Name not found." );
				ErrorsFound = true;
			} else {
				IsZoneDV( ZoneUCSDDV( Loop ).ZonePtr ) = true;
			}
			// Second Alpha is Schedule Name
			ZoneUCSDDV( Loop ).SchedGainsName = cAlphaArgs( 2 );
			ZoneUCSDDV( Loop ).SchedGainsPtr = GetScheduleIndex( cAlphaArgs( 2 ) );
			if ( ZoneUCSDDV( Loop ).SchedGainsPtr == 0 ) {
				if ( lAlphaFieldBlanks( 2 ) ) {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 2 ) + " = " + cAlphaArgs( 2 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
					ShowContinueError( " Schedule name must be input." );
					ErrorsFound = true;
				} else {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 2 ) + " = " + cAlphaArgs( 2 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
					ShowContinueError( "Schedule name was not found." );
					ErrorsFound = true;
				}
			}

			ZoneUCSDDV( Loop ).NumPlumesPerOcc = rNumericArgs( 1 );
			ZoneUCSDDV( Loop ).ThermostatHeight = rNumericArgs( 2 );
			ZoneUCSDDV( Loop ).ComfortHeight = rNumericArgs( 3 );
			ZoneUCSDDV( Loop ).TempTrigger = rNumericArgs( 4 );

		}

	}

	void
	GetCrossVentData( bool & ErrorsFound ) // True if errors found during this get input routine
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         G. Carrilho da Graca
		//       DATE WRITTEN   October 2004
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS SUBROUTINE:
		//  Get UCSD Cross ventilation model controls for all zones at once

		// METHODOLOGY EMPLOYED:
		// Use input processor to get input from idf file

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using InputProcessor::FindItem;
		using InputProcessor::SameString;
		using namespace DataIPShortCuts;
		using DataHeatBalance::Zone;
		using namespace ScheduleManager;

		using DataSurfaces::Surface;
		using namespace DataAirflowNetwork;
		using DataHeatBalance::TotPeople;
		using DataHeatBalance::People;
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int IOStat;
		int NumAlpha;
		int NumNumber;
		int Loop;
		int Loop2;
		int ThisZone;
		static int CompNum( 0 );
		static int TypeNum( 0 );
		static int NodeNum1( 0 );
		static int NodeNum2( 0 );

		if ( ! UCSDModelUsed ) return;
		cCurrentModuleObject = "RoomAirSettings:CrossVentilation";
		TotUCSDCV = GetNumObjectsFound( cCurrentModuleObject );

		if ( TotUCSDCV <= 0 ) return;

		ZoneUCSDCV.allocate( TotUCSDCV );

		for ( Loop = 1; Loop <= TotUCSDCV; ++Loop ) {

			GetObjectItem( cCurrentModuleObject, Loop, cAlphaArgs, NumAlpha, rNumericArgs, NumNumber, IOStat, _, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			// First is Zone Name
			ZoneUCSDCV( Loop ).ZoneName = cAlphaArgs( 1 );
			ZoneUCSDCV( Loop ).ZonePtr = FindItemInList( cAlphaArgs( 1 ), Zone );
			if ( ZoneUCSDCV( Loop ).ZonePtr == 0 ) {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 1 ) + " = " + cAlphaArgs( 1 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
				ShowContinueError( "Zone name was not found." );
				ErrorsFound = true;
			} else {
				IsZoneCV( ZoneUCSDCV( Loop ).ZonePtr ) = true;
			}
			// Second Alpha is Schedule Name
			ZoneUCSDCV( Loop ).SchedGainsName = cAlphaArgs( 2 );
			ZoneUCSDCV( Loop ).SchedGainsPtr = GetScheduleIndex( cAlphaArgs( 2 ) );
			if ( ZoneUCSDCV( Loop ).SchedGainsPtr == 0 ) {
				if ( lAlphaFieldBlanks( 2 ) ) {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 2 ) + " = " + cAlphaArgs( 2 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
					ShowContinueError( "Schedule name field is blank." );
					ErrorsFound = true;
				} else {
					ShowSevereError( "Invalid " + cAlphaFieldNames( 2 ) + " = " + cAlphaArgs( 2 ) );
					ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
					ShowContinueError( "Schedule name was not found." );
					ErrorsFound = true;
				}
			}

			// Third Alpha is a string: JET or RECIRCULATION
			if ( SameString( cAlphaArgs( 3 ), "Jet" ) ) {
				ZoneUCSDCV( Loop ).VforComfort = VComfort_Jet;
			} else if ( SameString( cAlphaArgs( 3 ), "Recirculation" ) ) {
				ZoneUCSDCV( Loop ).VforComfort = VComfort_Recirculation;
			} else {
				ZoneUCSDCV( Loop ).VforComfort = VComfort_Invalid;
			}

			for ( Loop2 = 1; Loop2 <= TotPeople; ++Loop2 ) {
				if ( People( Loop2 ).ZonePtr != ZoneUCSDCV( Loop ).ZonePtr ) continue;
				if ( People( Loop2 ).Fanger ) {
					if ( ZoneUCSDCV( Loop ).VforComfort == VComfort_Invalid ) {
						if ( lAlphaFieldBlanks( 3 ) ) {
							ShowSevereError( "Invalid " + cAlphaFieldNames( 3 ) + " = " + cAlphaArgs( 3 ) );
							ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
							ShowContinueError( "Airflow region used for thermal comfort evaluation is required for Zone=" + cAlphaArgs( 1 ) );
							ShowContinueError( "Field is blank, please choose Jet or Recirculation." );
							ErrorsFound = true;
						} else {
							ShowSevereError( "Invalid " + cAlphaFieldNames( 3 ) + " = " + cAlphaArgs( 3 ) );
							ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
							ShowContinueError( "Airflow region used for thermal comfort evaluation is required for Zone=" + cAlphaArgs( 1 ) );
							ShowContinueError( "Please choose Jet or Recirculation." );
							ErrorsFound = true;
						}
					}
				}
			}

			ThisZone = ZoneUCSDCV( Loop ).ZonePtr;
			if ( ThisZone == 0 ) continue;

			// Following depend on valid zone

			Loop2 = FindItemInList( Zone( ZoneUCSDCV( Loop ).ZonePtr ).Name, MultizoneZoneData, &MultizoneZoneProp::ZoneName );
			if ( Loop2 == 0 ) {
				ShowSevereError( "Problem with " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
				ShowContinueError( "AirflowNetwork airflow model must be active in this zone" );
				ErrorsFound = true;
			}

			// If a crack is used it must have an air flow coefficient = 0.5
			for ( Loop2 = 1; Loop2 <= NumOfLinksMultiZone; ++Loop2 ) {
				NodeNum1 = MultizoneSurfaceData( Loop2 ).NodeNums( 1 );
				NodeNum2 = MultizoneSurfaceData( Loop2 ).NodeNums( 2 );
				if ( Surface( MultizoneSurfaceData( Loop2 ).SurfNum ).Zone == ThisZone || ( AirflowNetworkNodeData( NodeNum2 ).EPlusZoneNum == ThisZone && AirflowNetworkNodeData( NodeNum1 ).EPlusZoneNum > 0 ) || ( AirflowNetworkNodeData( NodeNum2 ).EPlusZoneNum > 0 && AirflowNetworkNodeData( NodeNum1 ).EPlusZoneNum == ThisZone ) ) {
					CompNum = AirflowNetworkLinkageData( Loop2 ).CompNum;
					TypeNum = AirflowNetworkCompData( CompNum ).TypeNum;
					if ( AirflowNetworkCompData( CompNum ).CompTypeNum == CompTypeNum_SCR ) {
						if ( MultizoneSurfaceCrackData( TypeNum ).FlowExpo != 0.50 ) {
							AirModel( ThisZone ).AirModelType = RoomAirModel_Mixing;
							ShowWarningError( "Problem with " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
							ShowWarningError( "Roomair model will not be applied for Zone=" + cAlphaArgs( 1 ) + '.' );
							ShowContinueError( "AirflowNetwrok:Multizone:Surface crack object must have an air flow coefficient = 0.5, value was=" + RoundSigDigits( MultizoneSurfaceCrackData( TypeNum ).FlowExpo, 2 ) );
						}
					}
				}
			}
		}

	}

	void
	GetUFADZoneData( bool & ErrorsFound ) // True if errors found during this get input routine
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Buhl
		//       DATE WRITTEN   August 2005
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS SUBROUTINE:
		//  Get UCSD UFAD interior zone model controls for all zones at once

		// METHODOLOGY EMPLOYED:
		// Use input processor to get input from idf file

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using InputProcessor::SameString;
		using namespace DataIPShortCuts;
		using DataHeatBalance::Zone;
		using namespace ScheduleManager;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int IOStat;
		int NumAlpha;
		int NumNumber;
		int Loop;

		if ( ! UCSDModelUsed ) {
			TotUCSDUI = 0;
			TotUCSDUE = 0;
			return;
		}
		cCurrentModuleObject = "RoomAirSettings:UnderFloorAirDistributionInterior";
		TotUCSDUI = GetNumObjectsFound( cCurrentModuleObject );
		cCurrentModuleObject = "RoomAirSettings:UnderFloorAirDistributionExterior";
		TotUCSDUE = GetNumObjectsFound( cCurrentModuleObject );

		if ( TotUCSDUI <= 0 && TotUCSDUE <= 0 ) return;

		ZoneUCSDUI.allocate( TotUCSDUI );
		ZoneUCSDUE.allocate( TotUCSDUE );
		ZoneUFPtr.dimension( NumOfZones, 0 );

		cCurrentModuleObject = "RoomAirSettings:UnderFloorAirDistributionInterior";
		for ( Loop = 1; Loop <= TotUCSDUI; ++Loop ) {
			GetObjectItem( cCurrentModuleObject, Loop, cAlphaArgs, NumAlpha, rNumericArgs, NumNumber, IOStat, _, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			// First is Zone Name
			ZoneUCSDUI( Loop ).ZoneName = cAlphaArgs( 1 );
			ZoneUCSDUI( Loop ).ZonePtr = FindItemInList( cAlphaArgs( 1 ), Zone );
			ZoneUFPtr( ZoneUCSDUI( Loop ).ZonePtr ) = Loop;
			if ( ZoneUCSDUI( Loop ).ZonePtr == 0 ) {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 1 ) + " = " + cAlphaArgs( 1 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
				ShowContinueError( "Zone name was not found." );
				ErrorsFound = true;
			} else {
				IsZoneUI( ZoneUCSDUI( Loop ).ZonePtr ) = true;
			}
			// 2nd alpha is diffuser type
			if ( SameString( cAlphaArgs( 2 ), "Swirl" ) ) {
				ZoneUCSDUI( Loop ).DiffuserType = Swirl;
			} else if ( SameString( cAlphaArgs( 2 ), "VariableArea" ) ) {
				ZoneUCSDUI( Loop ).DiffuserType = VarArea;
			} else if ( SameString( cAlphaArgs( 2 ), "HorizontalSwirl" ) ) {
				ZoneUCSDUI( Loop ).DiffuserType = DisplVent;
			} else if ( SameString( cAlphaArgs( 2 ), "Custom" ) ) {
				ZoneUCSDUI( Loop ).DiffuserType = Custom;
			} else if ( SameString( cAlphaArgs( 2 ), "LinearBarGrille" ) ) {
				ZoneUCSDUI( Loop ).DiffuserType = LinBarGrille;
			} else {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 2 ) + " = " + cAlphaArgs( 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}
			// 1st number is Number of Diffusers per Zone
			ZoneUCSDUI( Loop ).DiffusersPerZone = rNumericArgs( 1 );
			// 2nd number is Power per Plume
			ZoneUCSDUI( Loop ).PowerPerPlume = rNumericArgs( 2 );
			// 3rd number is Design Effective Area of Diffuser
			ZoneUCSDUI( Loop ).DiffArea = rNumericArgs( 3 );
			// 4th number is Diffuser Slot Angle from Vertical
			ZoneUCSDUI( Loop ).DiffAngle = rNumericArgs( 4 );
			// 5th number is Thermostat Height
			ZoneUCSDUI( Loop ).ThermostatHeight = rNumericArgs( 5 );
			// 6th number is Comfort Height
			ZoneUCSDUI( Loop ).ComfortHeight = rNumericArgs( 6 );
			// 7th number is Temperature Difference Threshold for Reporting
			ZoneUCSDUI( Loop ).TempTrigger = rNumericArgs( 7 );
			// 8th number user-specified transition height
			ZoneUCSDUI( Loop ).TransHeight = rNumericArgs( 8 );
			// 9th number is Coefficient A in formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
			ZoneUCSDUI( Loop ).A_Kc = rNumericArgs( 9 );
			// 10th number is Coefficient B in formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
			ZoneUCSDUI( Loop ).B_Kc = rNumericArgs( 10 );
			// 11th number is Coefficient C in formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
			ZoneUCSDUI( Loop ).C_Kc = rNumericArgs( 11 );
			// 12th number is Coefficient D in formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
			ZoneUCSDUI( Loop ).D_Kc = rNumericArgs( 12 );
			// 13th number is Coefficient E in formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
			ZoneUCSDUI( Loop ).E_Kc = rNumericArgs( 13 );
		}

		cCurrentModuleObject = "RoomAirSettings:UnderFloorAirDistributionExterior";
		for ( Loop = 1; Loop <= TotUCSDUE; ++Loop ) {
			GetObjectItem( cCurrentModuleObject, Loop, cAlphaArgs, NumAlpha, rNumericArgs, NumNumber, IOStat, _, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			// First is Zone Name
			ZoneUCSDUE( Loop ).ZoneName = cAlphaArgs( 1 );
			ZoneUCSDUE( Loop ).ZonePtr = FindItemInList( cAlphaArgs( 1 ), Zone );
			ZoneUFPtr( ZoneUCSDUE( Loop ).ZonePtr ) = Loop;
			if ( ZoneUCSDUE( Loop ).ZonePtr == 0 ) {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 1 ) + " = " + cAlphaArgs( 1 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
				ShowContinueError( "Zone name was not found." );
				ErrorsFound = true;
			} else {
				IsZoneUI( ZoneUCSDUE( Loop ).ZonePtr ) = true;
			}
			// 2nd alpha is diffuser type
			if ( SameString( cAlphaArgs( 2 ), "Swirl" ) ) {
				ZoneUCSDUE( Loop ).DiffuserType = Swirl;
			} else if ( SameString( cAlphaArgs( 2 ), "VariableArea" ) ) {
				ZoneUCSDUE( Loop ).DiffuserType = VarArea;
			} else if ( SameString( cAlphaArgs( 2 ), "HorizontalSwirl" ) ) {
				ZoneUCSDUE( Loop ).DiffuserType = DisplVent;
			} else if ( SameString( cAlphaArgs( 2 ), "Custom" ) ) {
				ZoneUCSDUE( Loop ).DiffuserType = Custom;
			} else if ( SameString( cAlphaArgs( 2 ), "LinearBarGrille" ) ) {
				ZoneUCSDUE( Loop ).DiffuserType = LinBarGrille;
			} else {
				ShowSevereError( "Invalid " + cAlphaFieldNames( 2 ) + " = " + cAlphaArgs( 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}
			// 1st number is Number of Diffusers per Zone
			ZoneUCSDUE( Loop ).DiffusersPerZone = rNumericArgs( 1 );
			// 2nd number is Power per Plume
			ZoneUCSDUE( Loop ).PowerPerPlume = rNumericArgs( 2 );
			// 3rd number is Design Effective Area of Diffuser
			ZoneUCSDUE( Loop ).DiffArea = rNumericArgs( 3 );
			// 4th number is Diffuser Slot Angle from Vertical
			ZoneUCSDUE( Loop ).DiffAngle = rNumericArgs( 4 );
			// 5th number is Thermostat Height
			ZoneUCSDUE( Loop ).ThermostatHeight = rNumericArgs( 5 );
			// 6th number is Comfort Height
			ZoneUCSDUE( Loop ).ComfortHeight = rNumericArgs( 6 );
			// 7th number is Temperature Difference Threshold for Reporting
			ZoneUCSDUE( Loop ).TempTrigger = rNumericArgs( 7 );
			// 8th number user-specified transition height
			ZoneUCSDUE( Loop ).TransHeight = rNumericArgs( 8 );
			// 9th number is Coefficient A in formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
			ZoneUCSDUE( Loop ).A_Kc = rNumericArgs( 9 );
			// 10th number is Coefficient B in formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
			ZoneUCSDUE( Loop ).B_Kc = rNumericArgs( 10 );
			// 11th number is Coefficient C in formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
			ZoneUCSDUE( Loop ).C_Kc = rNumericArgs( 11 );
			// 12th number is Coefficient D in formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
			ZoneUCSDUE( Loop ).D_Kc = rNumericArgs( 12 );
			// 13th number is Coefficient E in formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
			ZoneUCSDUE( Loop ).E_Kc = rNumericArgs( 13 );
		}

	}

	void
	GetRoomAirflowNetworkData( bool & ErrorsFound ) // True if errors found during this get input routine
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   November 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//  Get RoomAirflowNetwork data for all zones at once

		// METHODOLOGY EMPLOYED:
		// Use input processor to get input from idf file

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using InputProcessor::SameString;
		using namespace DataIPShortCuts;
		using DataHeatBalance::Zone;
		using DataHeatBalance::ZoneIntGainDeviceTypes;
		using DataHeatBalance::NumZoneIntGainDeviceTypes;
		using ScheduleManager::GetScheduleIndex;
		using DataSurfaces::Surface;
		using DataSurfaces::SurfaceClass_IntMass;
		using General::RoundSigDigits;
		using InternalHeatGains::GetInternalGainDeviceIndex;
		using DataHVACGlobals::NumZoneHVACTerminalTypes;
		using DataHVACGlobals::ZoneHVACTerminalTypes;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Loop;  // local do loop index
		int NumAlphas;
		int NumNumbers;
		int status;
		int ZoneNum;
		int thisAirNodeinZone;
		int AlphaArgNum;
		int AirCntrlNodeNum;
		int TotNumOfRAFNNodeSurfLists;
		int TotNumOfRAFNNodeGainsLists;
		int TotNumOfRAFNNodeHVACLists;
		bool IntGainError;
		int RAFNNodeNum;
		bool foundList;
		int NumSurfsThisNode;
		int NumOfSurfs;
		int SurfCount;
		int SurfFirst;
		int ListSurfNum;
		int SurfNum;
		int numGains;
		int gainsLoop;
		int TypeNum;
		int numEquip;
		int EquipLoop;
		int TotNumEquip;
		bool IntEquipError;
		Real64 SumFraction;
		std::string Name;
		int GainNum;
		int RAFNNum;

		cCurrentModuleObject = "RoomAirSettings:AirflowNetwork";
		NumOfRoomAirflowNetControl = GetNumObjectsFound( cCurrentModuleObject );
		if ( NumOfRoomAirflowNetControl == 0 ) return;
		if ( NumOfRoomAirflowNetControl > NumOfZones ) {
			ShowSevereError( "Too many " + cCurrentModuleObject + " objects in input file" );
			ShowContinueError( "There cannot be more " + cCurrentModuleObject + " objects than number of zones." );
			ErrorsFound = true;
		}

		if ( !allocated( RoomAirflowNetworkZoneInfo ) ) {
			RoomAirflowNetworkZoneInfo.allocate( NumOfZones );
		}

		for ( Loop = 1; Loop <= NumOfRoomAirflowNetControl; ++Loop ) {
			GetObjectItem( cCurrentModuleObject, Loop, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, status, _, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			ZoneNum = FindItemInList( cAlphaArgs( 2 ), Zone, NumOfZones );
			if ( ZoneNum == 0 ) {
				ShowSevereError( "GetRoomAirflowNetworkData: Invalid " + cAlphaFieldNames( 2 ) + " = " + cAlphaArgs( 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 2 ) );
				ShowContinueError( "Not a valid zone name." );
				ErrorsFound = true;
				continue;
			}
			if ( AirModel( ZoneNum ).AirModelType != RoomAirModel_AirflowNetwork ) {
				ShowSevereError( "GetRoomAirflowNetworkData: Zone specified='" + cAlphaArgs( 1 ) + "', Air Model type is not AirflowNetwork." );
				ShowContinueError( "Air Model Type for zone =" + ChAirModel( AirModel( ZoneNum ).AirModelType ) );
				ErrorsFound = true;
				continue;
			}
			RoomAirflowNetworkZoneInfo( ZoneNum ).ZoneID = ZoneNum;
			RoomAirflowNetworkZoneInfo( ZoneNum ).RAFNNum = Loop;
			RoomAirflowNetworkZoneInfo( ZoneNum ).IsUsed = true;
			RoomAirflowNetworkZoneInfo( ZoneNum ).Name = cAlphaArgs( 1 );
			RoomAirflowNetworkZoneInfo( ZoneNum ).ZoneName = cAlphaArgs( 2 ); // Zone Name

			RoomAirflowNetworkZoneInfo( ZoneNum ).NumOfAirNodes = ( NumAlphas - 3 );

			if ( RoomAirflowNetworkZoneInfo( ZoneNum ).NumOfAirNodes > 0 ) {
				RoomAirflowNetworkZoneInfo( ZoneNum ).Node.allocate( RoomAirflowNetworkZoneInfo( ZoneNum ).NumOfAirNodes );
			}
			else {
				ShowSevereError( "GetRoomAirflowNetworkData: Incomplete input in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
				ErrorsFound = true;
			}

			for ( thisAirNodeinZone = 1; thisAirNodeinZone <= RoomAirflowNetworkZoneInfo( ZoneNum ).NumOfAirNodes; ++thisAirNodeinZone ) {
				AlphaArgNum = thisAirNodeinZone + 3;
				RoomAirflowNetworkZoneInfo( ZoneNum ).Node( thisAirNodeinZone ).Name = cAlphaArgs( AlphaArgNum );
			}
			// control point node
			AirCntrlNodeNum = FindItemInList( cAlphaArgs( 3 ), RoomAirflowNetworkZoneInfo( ZoneNum ).Node, RoomAirflowNetworkZoneInfo( ZoneNum ).NumOfAirNodes );
			if ( AirCntrlNodeNum == 0 ) {
				ShowSevereError( "GetRoomAirflowNetworkData: Invalid " + cAlphaFieldNames( 3 ) + " = " + cAlphaArgs( 3 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
				ShowContinueError( "Not a valid RoomAir:Node:AirflowNetwork name for this zone." );
				ErrorsFound = true;
				continue;
			}
			else {
				RoomAirflowNetworkZoneInfo( ZoneNum ).ControlAirNodeID = AirCntrlNodeNum;
			}
			RoomAirflowNetworkZoneInfo( ZoneNum ).totNumSurfs = Zone( ZoneNum ).SurfaceLast - Zone( ZoneNum ).SurfaceFirst + 1;
		} // loop thru NumOfRoomAirflowNetControl

		cCurrentModuleObject = "RoomAir:Node:AirflowNetwork";
		TotNumOfRoomAFNNodes = GetNumObjectsFound( cCurrentModuleObject );
		for ( Loop = 1; Loop <= TotNumOfRoomAFNNodes; ++Loop ) {
			GetObjectItem( cCurrentModuleObject, Loop, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, status, _,
				lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			ZoneNum = FindItemInList( cAlphaArgs( 2 ), Zone, NumOfZones );
			if ( ZoneNum == 0 ) {
				ShowSevereError( "GetRoomAirflowNetworkData: Invalid " + cAlphaFieldNames( 2 ) + " = " + cAlphaArgs( 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
				ShowContinueError( "Not a valid zone name." );
				ErrorsFound = true;
				continue;
			}

			RAFNNodeNum = FindItemInList( cAlphaArgs( 1 ), RoomAirflowNetworkZoneInfo( ZoneNum ).Node, RoomAirflowNetworkZoneInfo( ZoneNum ).NumOfAirNodes );
			if ( RAFNNodeNum == 0 ) {
				ShowSevereError( "GetRoomAirflowNetworkData: Invalid " + cAlphaFieldNames( 2 ) + " = " + cAlphaArgs( 2 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
				ShowContinueError( "Not a valid RoomAir:Node:AirflowNetwork name." );
				ErrorsFound = true;
				continue;
			}

			RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RAFNNodeNum ).ZoneVolumeFraction = rNumericArgs( 1 );
			if ( !lAlphaFieldBlanks( 3 ) ) {
				RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RAFNNodeNum ).NodeSurfListName = cAlphaArgs( 3 );
			}
			else {
				RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RAFNNodeNum ).HasSurfacesAssigned = false;
			}
			if ( !lAlphaFieldBlanks( 4 ) ) {
				RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RAFNNodeNum ).NodeIntGainsListName = cAlphaArgs( 4 );
			}
			else {
				RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RAFNNodeNum ).HasIntGainsAssigned = false;
			}
			if ( !lAlphaFieldBlanks( 5 ) ) {
				RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RAFNNodeNum ).NodeHVACListName = cAlphaArgs( 5 );
			}
			else {
				RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RAFNNodeNum ).HasHVACAssigned = false;
			}

		} // loop thru TotNumOfRoomAFNNodes

		cCurrentModuleObject = "RoomAir:Node:AirflowNetwork:AdjacentSurfaceList";
		TotNumOfRAFNNodeSurfLists = GetNumObjectsFound( cCurrentModuleObject );
		for ( Loop = 1; Loop <= TotNumOfRAFNNodeSurfLists; ++Loop ) {
			foundList = false;
			GetObjectItem( cCurrentModuleObject, Loop, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, status, _, _, cAlphaFieldNames, cNumericFieldNames );
			for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
				// find surface list
				if ( RoomAirflowNetworkZoneInfo( ZoneNum ).NumOfAirNodes > 0 ) {
					RAFNNodeNum = FindItemInList( cAlphaArgs( 1 ), RoomAirflowNetworkZoneInfo( ZoneNum ).Node, &RoomAirflowNetworkAirNodeNestedStruct::NodeSurfListName, RoomAirflowNetworkZoneInfo( ZoneNum ).NumOfAirNodes );
				} else {
					RAFNNodeNum = 0;
				}
				if ( RAFNNodeNum > 0 ) { // found it
					foundList = true;
					NumSurfsThisNode = NumAlphas - 1;
					NumOfSurfs = Zone( ZoneNum ).SurfaceLast - Zone( ZoneNum ).SurfaceFirst + 1;
					if ( allocated( RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RAFNNodeNum ).SurfMask ) ) {
						// throw error found twice
						ShowSevereError( "GetRoomAirflowNetworkData: Invalid " + cAlphaFieldNames( 1 ) + " = " + cAlphaArgs( 1 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
						ShowContinueError( "Duplicate RoomAir:Node:AirflowNetwork:AdjacentSurfaceList name." );
						ErrorsFound = true;
					}
					else {
						RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RAFNNodeNum ).SurfMask.allocate( RoomAirflowNetworkZoneInfo( ZoneNum ).totNumSurfs );
						RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RAFNNodeNum ).SurfMask = false; // init
						RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RAFNNodeNum ).HasSurfacesAssigned = true;
						// relate surfaces to this air node and check to see whether surface names are specified correctly or not
						SurfCount = 0;
						SurfFirst = Zone( ZoneNum ).SurfaceFirst - 1;
						for ( ListSurfNum = 2; ListSurfNum <= NumAlphas; ++ListSurfNum ) {
							for ( SurfNum = 1; SurfNum <= NumOfSurfs; ++SurfNum ) {
								// IF( cAlphaArgs( ListSurfNum ) == Surface( SurfFirst + SurfNum ).Name ) THEN
								if ( SameString( cAlphaArgs( ListSurfNum ), Surface( SurfFirst + SurfNum ).Name ) ) {
									RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RAFNNodeNum ).SurfMask( SurfNum ) = true;
									SurfCount = SurfCount + 1;
								}
							}
						}
						if ( NumSurfsThisNode != SurfCount ) {
							ShowSevereError( "GetRoomAirflowNetworkData: Invalid " + cAlphaFieldNames( 1 ) + " = " + cAlphaArgs( 1 ) );
							ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
							ShowContinueError( "Some surface names were not found in the zone" );
							ErrorsFound = true;
						}
					}
					break;
				}
			} // loop over zones
			if ( !foundList ) { // throw error
				ShowSevereError( "GetRoomAirflowNetworkData: Invalid " + cAlphaFieldNames( 1 ) + " = " + cAlphaArgs( 1 ) );
				ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
				ShowContinueError( "Did not find a RoomAir:Node:AirflowNetwork object that references this object" );
				ErrorsFound = true;
			}
		} // loop thru TotNumOfRAFNNodeSurfLists

		cCurrentModuleObject = "RoomAir:Node:AirflowNetwork:InternalGains";
		TotNumOfRAFNNodeGainsLists = GetNumObjectsFound( cCurrentModuleObject );
		for ( Loop = 1; Loop <= TotNumOfRAFNNodeGainsLists; ++Loop ) {
			foundList = false;
			GetObjectItem( cCurrentModuleObject, Loop, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, status, _, _, cAlphaFieldNames, cNumericFieldNames );
			if ( mod( ( NumAlphas + NumNumbers - 1 ), 3 ) != 0 ) {
				ShowSevereError( "GetRoomAirflowNetworkData: For " + cCurrentModuleObject + ": " + cAlphaArgs( 1 ) );
				ShowContinueError( "Extensible field set are not evenly divisable by 3. Number of data entries = " + RoundSigDigits( NumAlphas + NumNumbers - 1 ) );
				ErrorsFound = true;
				break;
			}
			for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
				// find surface list
				if ( RoomAirflowNetworkZoneInfo( ZoneNum ).NumOfAirNodes > 0 ) {
					RAFNNodeNum = FindItemInList( cAlphaArgs( 1 ), RoomAirflowNetworkZoneInfo( ZoneNum ).Node, &RoomAirflowNetworkAirNodeNestedStruct::NodeIntGainsListName, RoomAirflowNetworkZoneInfo( ZoneNum ).NumOfAirNodes );
				} else {
					RAFNNodeNum = 0;
				}
				if ( RAFNNodeNum > 0 ) { // found it
					foundList = true;
					numGains = ( NumAlphas + NumNumbers - 1 ) / 3;
					if ( allocated( RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RAFNNodeNum ).IntGain ) ) {
						ShowSevereError( "GetRoomAirflowNetworkData: Invalid " + cAlphaFieldNames( 1 ) + " = " + cAlphaArgs( 1 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
						ShowContinueError( "Duplicate " + cCurrentModuleObject + " name." );
						ErrorsFound = true;
					}
					else {
						RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RAFNNodeNum ).IntGain.allocate( numGains );
						RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RAFNNodeNum ).IntGainsDeviceIndices.allocate( numGains );
						RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RAFNNodeNum ).IntGainsFractions.allocate( numGains );
						RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RAFNNodeNum ).HasIntGainsAssigned = true;
						RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RAFNNodeNum ).NumIntGains = numGains;
						for ( gainsLoop = 1; gainsLoop <= numGains; ++gainsLoop ) {
							TypeNum = FindItemInList( cAlphaArgs( gainsLoop * 2 ), ZoneIntGainDeviceTypes, NumZoneIntGainDeviceTypes );
							if ( TypeNum > 0 ) {
								RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RAFNNodeNum ).IntGain( gainsLoop ).TypeOfNum = TypeNum;
							}
							else {
								ShowSevereError( "GetRoomAirflowNetworkData: Invalid " + cAlphaFieldNames( gainsLoop * 2 ) + " = " + cAlphaArgs( gainsLoop * 2 ) );
								ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
								ShowContinueError( "incorrect type of internal gain" );
								ErrorsFound = true;
							}
							RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RAFNNodeNum ).IntGain( gainsLoop ).Name = cAlphaArgs( gainsLoop * 2 + 1 );

							// verify type and name and get pointer to device in internal gains structure array
							IntGainError = false;
							GetInternalGainDeviceIndex( ZoneNum, RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RAFNNodeNum ).IntGain( gainsLoop ).TypeOfNum,
								RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RAFNNodeNum ).IntGain( gainsLoop ).Name, RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RAFNNodeNum ).IntGainsDeviceIndices( gainsLoop ), IntGainError );
							if ( IntGainError ) {
								ShowSevereError( "GetRoomAirflowNetworkData: Invalid " + cAlphaFieldNames( gainsLoop * 2 + 1 ) + " = " + cAlphaArgs( gainsLoop * 2 + 1 ) );
								ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
								ShowContinueError( "Internal gain did not match correctly" );
								ErrorsFound = true;
							}
							RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RAFNNodeNum ).IntGainsFractions( gainsLoop ) = rNumericArgs( gainsLoop );
						}

					}

				}
			}
		} // loop thru TotNumOfRAFNNodeGainsLists


		// Get data of HVAC equipment
		cCurrentModuleObject = "RoomAir:Node:AirflowNetwork:HVACEquipment";
		TotNumOfRAFNNodeHVACLists = GetNumObjectsFound( cCurrentModuleObject );
		for ( Loop = 1; Loop <= TotNumOfRAFNNodeHVACLists; ++Loop ) {
			GetObjectItem( cCurrentModuleObject, Loop, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, status, _, _, cAlphaFieldNames, cNumericFieldNames );
			if ( mod( ( NumAlphas + NumNumbers - 1 ), 4 ) != 0 ) {
				ShowSevereError( "GetRoomAirflowNetworkData: For " + cCurrentModuleObject + ": " + cAlphaArgs( 1 ) );
				ShowContinueError( "Extensible field set are not evenly divisable by 4. Number of data entries = " + RoundSigDigits( NumAlphas + NumNumbers - 1 ) );
				ErrorsFound = true;
				break;
			}
			for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
				// find surface list
				if ( RoomAirflowNetworkZoneInfo( ZoneNum ).NumOfAirNodes > 0 ) {
					RAFNNodeNum = FindItemInList( cAlphaArgs( 1 ), RoomAirflowNetworkZoneInfo( ZoneNum ).Node, &RoomAirflowNetworkAirNodeNestedStruct::NodeHVACListName, RoomAirflowNetworkZoneInfo( ZoneNum ).NumOfAirNodes );
				} else {
					RAFNNodeNum = 0;
				}
				if ( RAFNNodeNum > 0 ) { // found it
					foundList = true;
					numEquip = ( NumAlphas + NumNumbers - 1 ) / 4;
					RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RAFNNodeNum ).NumHVACs = numEquip;
					if ( allocated( RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RAFNNodeNum ).HVAC ) ) {
						ShowSevereError( "GetRoomAirflowNetworkData: Invalid " + cAlphaFieldNames( 1 ) + " = " + cAlphaArgs( 1 ) );
						ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
						ShowContinueError( "Duplicate " + cCurrentModuleObject + " name." );
						ErrorsFound = true;
					}
					else {
						RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RAFNNodeNum ).HVAC.allocate( numEquip );
						RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RAFNNodeNum ).HasHVACAssigned = true;
						for ( EquipLoop = 1; EquipLoop <= numEquip; ++EquipLoop ) {
							TypeNum = FindItemInList( cAlphaArgs( 2 + ( EquipLoop - 1 ) * 2 ), ZoneHVACTerminalTypes, NumZoneHVACTerminalTypes );
							if ( TypeNum > 0 ) {
								RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RAFNNodeNum ).HVAC( EquipLoop ).TypeOfNum = TypeNum;
							}
							else {
								ShowSevereError( "GetRoomAirflowNetworkData: Invalid " + cAlphaFieldNames( 2 + ( EquipLoop - 1 ) * 2 ) + " = " + cAlphaArgs( 2 + ( EquipLoop - 1 ) * 2 ) );
								ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
								ShowContinueError( "incorrect type of HVACEquipment" );
								ErrorsFound = true;
							}
							RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RAFNNodeNum ).HVAC( EquipLoop ).ObjectTypeName = cAlphaArgs( 2 + ( EquipLoop - 1 ) * 2 );
							RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RAFNNodeNum ).HVAC( EquipLoop ).Name = cAlphaArgs( 3 + ( EquipLoop - 1 ) * 2 );

							// verify type and name and get pointer to device in HVAC equipment type and name structure array
							TotNumEquip = GetNumObjectsFound( ZoneHVACTerminalTypes( TypeNum ) );
							if ( TotNumEquip == 0 ) {
								ShowSevereError( "GetRoomAirflowNetworkData: No such " + cAlphaFieldNames( 2 + ( EquipLoop - 1 ) * 2 ) + " = " + cAlphaArgs( 2 + ( EquipLoop - 1 ) * 2 ) );
								ShowContinueError( "is available in the input file in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
								ErrorsFound = true;
							}
							RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RAFNNodeNum ).HVAC( EquipLoop ).SupplyFraction = rNumericArgs( 1 + ( EquipLoop - 1 ) * 2 );
							RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RAFNNodeNum ).HVAC( EquipLoop ).ReturnFraction = rNumericArgs( 2 + ( EquipLoop - 1 ) * 2 );

							IntEquipError = CheckEquipName( ZoneNum, RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RAFNNodeNum ).HVAC( EquipLoop ).ObjectTypeName,
								RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RAFNNodeNum ).HVAC( EquipLoop ).Name,
								RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RAFNNodeNum ).HVAC( EquipLoop ).SupplyNodeName, RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RAFNNodeNum ).HVAC( EquipLoop ).ReturnNodeName,
								TotNumEquip, TypeNum );

							if ( !IntEquipError ) {
								ShowSevereError( "GetRoomAirflowNetworkData: Invalid " + cAlphaFieldNames( 3 + ( EquipLoop - 1 ) * 2 ) + " = " + cAlphaArgs( 2 + ( EquipLoop - 1 ) * 2 ) );
								ShowContinueError( "Entered in " + cCurrentModuleObject + " = " + cAlphaArgs( 1 ) );
								ShowContinueError( "Internal gain did not match correctly" );
								ErrorsFound = true;
							}
							//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
							//TYPE RoomAirflowNetworkHVACStruct
							//INTEGER::EquipConfigIndex = 0

						}

					}
				}

			}
		}  // loop thru TotNumOfRAFNNodeHVACLists

		// do some checks on input data
		for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
			if ( RoomAirflowNetworkZoneInfo( ZoneNum ).NumOfAirNodes > 0 ) {
				// Check zone volume fraction
				SumFraction = 0.0;
				for ( RAFNNodeNum = 1; RAFNNodeNum <= RoomAirflowNetworkZoneInfo( ZoneNum ).NumOfAirNodes; ++RAFNNodeNum ) {
					SumFraction = SumFraction + RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RAFNNodeNum ).ZoneVolumeFraction;
				}
				if ( std::abs( SumFraction - 1.0 ) > 0.001 ) {
					ShowSevereError( "GetRoomAirflowNetworkData: Invalid, zone volume fractions do not sum to 1.0" );
					ShowContinueError( "Entered in RoomAir:Node:AirflowNetwork with Zone Name = " + Zone( ZoneNum ).Name );
					ShowContinueError( "The Fraction of Zone Air Volume values across all the nodes needs to sum to 1.0." );
					ShowContinueError( "The sum of fractions entered = " + RoundSigDigits( SumFraction, 3 ) );
					ErrorsFound = true;
				}
				// Check internal gain fraction
				for ( RAFNNodeNum = 1; RAFNNodeNum <= RoomAirflowNetworkZoneInfo( ZoneNum ).NumOfAirNodes; ++RAFNNodeNum ) {
					for ( gainsLoop = 1; gainsLoop <= RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RAFNNodeNum ).NumIntGains; ++gainsLoop ) {
						if ( RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RAFNNodeNum ).IntGain( gainsLoop ).FractionCheck ) continue;
						SumFraction = RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RAFNNodeNum ).IntGainsFractions( gainsLoop );
						TypeNum = RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RAFNNodeNum ).IntGain( gainsLoop ).TypeOfNum;
						Name = RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RAFNNodeNum ).IntGain( gainsLoop ).Name;
						RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RAFNNodeNum ).IntGain( gainsLoop ).FractionCheck = true;
						for ( RAFNNum = 1; RAFNNum <= RoomAirflowNetworkZoneInfo( ZoneNum ).NumOfAirNodes; ++RAFNNum ) {
							for ( GainNum = 1; GainNum <= RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RAFNNum ).NumIntGains; ++GainNum ) {
								if ( RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RAFNNum ).IntGain( GainNum ).FractionCheck ) continue;
								if ( TypeNum == RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RAFNNum ).IntGain( GainNum ).TypeOfNum &&
									SameString( Name, RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RAFNNum ).IntGain( GainNum ).Name ) ) {
									SumFraction += RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RAFNNum ).IntGainsFractions( GainNum );
									RoomAirflowNetworkZoneInfo( ZoneNum ).Node( RAFNNum ).IntGain( GainNum ).FractionCheck = true;
								}
							}
						}
						if ( std::abs( SumFraction - 1.0 ) > 0.001 ) {
							ShowSevereError( "GetRoomAirflowNetworkData: Invalid, internal gain fractions do not sum to 1.0" );
							ShowContinueError( "Entered in RoomAir:Node:AirflowNetwork with Zone Name = " + Zone( ZoneNum ).Name + ", Intrnal gain name = " + Name );
							ShowContinueError( "The Fraction of internal gain across all the nodes needs to sum to 1.0." );
							ShowContinueError( "The sum of fractions entered = " + RoundSigDigits( SumFraction, 3 ) );
							ErrorsFound = true;
						}
					}
				}
			}
		}

		if ( !ErrorsFound ) {
			for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
				if ( RoomAirflowNetworkZoneInfo( ZoneNum ).IsUsed ) {
					if ( RoomAirflowNetworkZoneInfo( ZoneNum ).NumOfAirNodes > 0 ) {
						for ( Loop = 1; Loop <= RoomAirflowNetworkZoneInfo( ZoneNum ).NumOfAirNodes; ++Loop ) {
							SetupOutputVariable( "RoomAirflowNetwork Node Temperature [C]", RoomAirflowNetworkZoneInfo( ZoneNum ).Node( Loop ).AirTemp, "HVAC", "Average", RoomAirflowNetworkZoneInfo( ZoneNum ).Node( Loop ).Name );
							SetupOutputVariable( "RoomAirflowNetwork Node Humidity Ratio [kgWater/kgDryAir]", RoomAirflowNetworkZoneInfo( ZoneNum ).Node( Loop ).HumRat, "HVAC", "Average", RoomAirflowNetworkZoneInfo( ZoneNum ).Node( Loop ).Name );
							SetupOutputVariable( "RoomAirflowNetwork Node Relative Humidity [%]", RoomAirflowNetworkZoneInfo( ZoneNum ).Node( Loop ).RelHumidity, "HVAC", "Average", RoomAirflowNetworkZoneInfo( ZoneNum ).Node( Loop ).Name );
						}
					}
				}
			}
		}
	}

	void
	SharedDVCVUFDataInit( int & ZoneNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   March 2005
		//       MODIFIED       Aug, 2013, Sam Brunswick -- for RoomAirCrossCrossVent modifications
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine allocates and initializes(?) the data that is shared between the
		// UCSD models (DV and CV)

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataUCSDSharedData;
		using namespace DataEnvironment;
		using namespace DataHeatBalFanSys;
		using namespace DataSurfaces;
		using DataGlobals::NumOfZones;
		using DataHeatBalance::Zone;
		using namespace DataAirflowNetwork;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using DataZoneEquipment::ZoneEquipConfig;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const BaseDischargeCoef( 0.62 );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int SurfNum; // DO loop counter for surfaces
		int ZNum; // DO loop counter for zones
		static int contFloorBegin( 0 ); // counter
		static int contFloorLast( 0 ); // counter
		static int contFloor( 0 ); // counter
		static int contCeilingBegin( 0 ); // counter
		static int contCeilingLast( 0 ); // counter
		static int contCeiling( 0 ); // counter
		static int contWallBegin( 0 ); // counter
		static int contWallLast( 0 ); // counter
		static int contWall( 0 ); // counter
		static int contWindowBegin( 0 ); // counter
		static int contWindowLast( 0 ); // counter
		static int contWindow( 0 ); // counter
		static int contInternalBegin( 0 ); // counter
		static int contInternalLast( 0 ); // counter
		static int contInternal( 0 ); // counter
		static int contDoorBegin( 0 ); // counter
		static int contDoorLast( 0 ); // counter
		static int contDoor( 0 ); // counter
		static int Loop( 0 ); // counter
		static int Loop2( 0 ); // counter
		static int i( 0 ); // counter
		static int N( 0 ); // counter
		static Real64 Z1ZoneAux( 0.0 ); // Auxiliary variables
		static Real64 Z2ZoneAux( 0.0 ); // Auxiliary variables
		static Real64 Z1Zone( 0.0 ); // Auxiliary variables
		static Real64 Z2Zone( 0.0 ); // Auxiliary variables
		static Real64 CeilingHeightDiffMax( 0.1 ); // Maximum difference between wall height and ceiling height
		bool SetZoneAux;
		Array1D_int AuxSurf;
		int MaxSurf;
		Array2D_int AuxAirflowNetworkSurf;
		Real64 WidthFactMax;
		Real64 HeightFactMax;
		Real64 WidthFact;
		Real64 HeightFact;
		static int Loop3( 0 ); // counter
		int ZoneEquipConfigNum; // counter
		Real64 AinCV;
		int AirflowNetworkSurfPtr;
		int NSides;
		static bool MyOneTimeFlag( true );
		static Array1D_bool MyEnvrnFlag;

		static int CompNum( 0 ); // AirflowNetwork Component number
		static int TypeNum( 0 ); // Airflownetwork Type Number within a component
		static int NodeNum1( 0 ); // The first node number in an AirflowNetwork linkage data
		static int NodeNum2( 0 ); // The Second node number in an AirflowNetwork linkage data

		// Do the one time initializations
		if ( MyOneTimeFlag ) {

			MyEnvrnFlag.allocate( NumOfZones );

			APos_Wall.allocate( TotSurfaces );
			APos_Floor.allocate( TotSurfaces );
			APos_Ceiling.allocate( TotSurfaces );
			PosZ_Wall.allocate( NumOfZones * 2 );
			PosZ_Floor.allocate( NumOfZones * 2 );
			PosZ_Ceiling.allocate( NumOfZones * 2 );
			APos_Window.allocate( TotSurfaces );
			APos_Door.allocate( TotSurfaces );
			APos_Internal.allocate( TotSurfaces );
			PosZ_Window.allocate( NumOfZones * 2 );
			PosZ_Door.allocate( NumOfZones * 2 );
			PosZ_Internal.allocate( NumOfZones * 2 );
			HCeiling.allocate( TotSurfaces );
			HWall.allocate( TotSurfaces );
			HFloor.allocate( TotSurfaces );
			HInternal.allocate( TotSurfaces );
			HWindow.allocate( TotSurfaces );
			HDoor.allocate( TotSurfaces );

			AuxSurf.allocate( NumOfZones );

			ZoneCeilingHeight.allocate( NumOfZones * 2 );
			ZoneCeilingHeight = 0.0;

			// Arrays initializations
			APos_Wall = 0;
			APos_Floor = 0;
			APos_Ceiling = 0;
			PosZ_Wall = 0;
			PosZ_Floor = 0;
			PosZ_Ceiling = 0;
			APos_Window = 0;
			APos_Door = 0;
			APos_Internal = 0;
			PosZ_Window = 0;
			PosZ_Door = 0;
			PosZ_Internal = 0;
			HCeiling = 0.0;
			HWall = 0.0;
			HFloor = 0.0;
			HInternal = 0.0;
			HWindow = 0.0;
			HDoor = 0.0;

			// Put the surface and zone information in Apos and PosZ arrays
			for ( ZNum = 1; ZNum <= NumOfZones; ++ZNum ) {
				// advance ONE position in the arrays PosZ because this is a new zone
				contWallBegin = contWall + 1;
				contFloorBegin = contFloor + 1;
				contCeilingBegin = contCeiling + 1;
				contWindowBegin = contWindow + 1;
				contInternalBegin = contInternal + 1;
				contDoorBegin = contDoor + 1;
				SetZoneAux = true;

				// cycle in this zone for all the surfaces
				for ( SurfNum = Zone( ZNum ).SurfaceFirst; SurfNum <= Zone( ZNum ).SurfaceLast; ++SurfNum ) {
					if ( Surface( SurfNum ).Class != SurfaceClass_IntMass ) {
						// Recalculate lowest and highest height for the zone
						Z1Zone = std::numeric_limits< Real64 >::max();
						Z2Zone = std::numeric_limits< Real64 >::lowest();
						for ( int i = 1, u = Surface( SurfNum ).Sides; i <= u; ++i ) {
							Real64 const z_i( Surface( SurfNum ).Vertex( i ).z );
							Z1Zone = std::min( Z1Zone, z_i );
							Z2Zone = std::max( Z2Zone, z_i );
						}
					}

					if ( SetZoneAux ) {
						// lowest height for the zone (for the first surface of the zone)
						Z1ZoneAux = Z1Zone;
						// highest height for the zone (for the first surface of the zone)
						Z2ZoneAux = Z2Zone;
						SetZoneAux = false;
					}

					if ( Z1Zone < Z1ZoneAux ) {
						Z1ZoneAux = Z1Zone;
					}
					if ( Z2Zone > Z2ZoneAux ) {
						Z2ZoneAux = Z2Zone;
					}
					Z1Zone = Z1ZoneAux;
					Z2Zone = Z2ZoneAux;

					// Put the reference to this surface in the appropriate array
					if ( Surface( SurfNum ).Class == SurfaceClass_Floor ) {
						++contFloor;
						APos_Floor( contFloor ) = SurfNum;
					} else if ( Surface( SurfNum ).Class == SurfaceClass_Wall ) {
						++contWall;
						APos_Wall( contWall ) = SurfNum;
					} else if ( Surface( SurfNum ).Class == SurfaceClass_Window ) {
						++contWindow;
						APos_Window( contWindow ) = SurfNum;
					} else if ( Surface( SurfNum ).Class == SurfaceClass_IntMass ) {
						++contInternal;
						APos_Internal( contInternal ) = SurfNum;
					} else if ( Surface( SurfNum ).Class == SurfaceClass_Door ) {
						++contDoor;
						APos_Door( contDoor ) = SurfNum;
					} else {
						++contCeiling;
						APos_Ceiling( contCeiling ) = SurfNum;
					}
				} // Surfaces

				contWallLast = contWall;
				contFloorLast = contFloor;
				contCeilingLast = contCeiling;
				contWindowLast = contWindow;
				contDoorLast = contDoor;
				contInternalLast = contInternal;
				// PosZ_Wall (... + 1) has the Begin Wall reference in Apos_Wall for the ZNum
				// PosZ_Wall (... + 2) has the End Wall reference in Apos_Wall for the ZNum
				PosZ_Wall( ( ZNum - 1 ) * 2 + 1 ) = contWallBegin;
				PosZ_Wall( ( ZNum - 1 ) * 2 + 2 ) = contWallLast;
				PosZ_Floor( ( ZNum - 1 ) * 2 + 1 ) = contFloorBegin;
				PosZ_Floor( ( ZNum - 1 ) * 2 + 2 ) = contFloorLast;
				PosZ_Ceiling( ( ZNum - 1 ) * 2 + 1 ) = contCeilingBegin;
				PosZ_Ceiling( ( ZNum - 1 ) * 2 + 2 ) = contCeilingLast;
				PosZ_Window( ( ZNum - 1 ) * 2 + 1 ) = contWindowBegin;
				PosZ_Window( ( ZNum - 1 ) * 2 + 2 ) = contWindowLast;
				PosZ_Door( ( ZNum - 1 ) * 2 + 1 ) = contDoorBegin;
				PosZ_Door( ( ZNum - 1 ) * 2 + 2 ) = contDoorLast;
				PosZ_Internal( ( ZNum - 1 ) * 2 + 1 ) = contInternalBegin;
				PosZ_Internal( ( ZNum - 1 ) * 2 + 2 ) = contInternalLast;
				// Save the highest and lowest height for this zone
				ZoneCeilingHeight( ( ZNum - 1 ) * 2 + 1 ) = Z1Zone;
				ZoneCeilingHeight( ( ZNum - 1 ) * 2 + 2 ) = Z2Zone;

				if ( std::abs( ( Z2Zone - Z1Zone ) - Zone( ZNum ).CeilingHeight ) > CeilingHeightDiffMax ) {
					ShowWarningError( "RoomAirManager: Inconsistent ceiling heights in Zone: " + Zone( ZNum ).Name );
					ShowContinueError( "Lowest height=[" + RoundSigDigits( Z1Zone, 3 ) + "]." );
					ShowContinueError( "Highest height=[" + RoundSigDigits( Z2Zone, 3 ) + "]." );
					ShowContinueError( "Ceiling height=[" + RoundSigDigits( Zone( ZNum ).CeilingHeight, 3 ) + "]." );
				}
			} // Zones

			AuxSurf = 0;
			CVNumAirflowNetworkSurfaces = 0;

			// calculate maximum number of airflow network surfaces in each zone
			for ( Loop = 1; Loop <= NumOfLinksMultiZone; ++Loop ) {
				++AuxSurf( Surface( MultizoneSurfaceData( Loop ).SurfNum ).Zone );
				++CVNumAirflowNetworkSurfaces;
				// Check if this is an interzone airflow network surface
				if ( Surface( MultizoneSurfaceData( Loop ).SurfNum ).ExtBoundCond > 0 && ( MultizoneSurfaceData( Loop ).SurfNum != Surface( MultizoneSurfaceData( Loop ).SurfNum ).ExtBoundCond ) ) {
					++AuxSurf( Surface( Surface( MultizoneSurfaceData( Loop ).SurfNum ).ExtBoundCond ).Zone );
					++CVNumAirflowNetworkSurfaces;
				}
			}
			// calculate maximum number of airflow network surfaces in a single zone
			MaxSurf = AuxSurf( 1 );
			for ( Loop = 2; Loop <= NumOfZones; ++Loop ) {
				if ( AuxSurf( Loop ) > MaxSurf ) MaxSurf = AuxSurf( Loop );
			}

			if ( ! allocated( AirflowNetworkSurfaceUCSDCV ) ) {
				AirflowNetworkSurfaceUCSDCV.allocate( {0,MaxSurf}, NumOfZones );
			}
			if ( ! allocated( CVJetRecFlows ) ) {
				CVJetRecFlows.allocate( {0,MaxSurf}, NumOfZones );
			}
			AuxAirflowNetworkSurf.allocate( {0,MaxSurf}, NumOfZones );
			// Width and Height for airflow network surfaces
			if ( ! allocated( SurfParametersCVDV ) ) {
				SurfParametersCVDV.allocate( NumOfLinksMultiZone );
			}

			AirflowNetworkSurfaceUCSDCV = 0;
			// Organize surfaces in vector AirflowNetworkSurfaceUCSDCV(Zone, surface indexes)
			for ( Loop = 1; Loop <= NumOfZones; ++Loop ) {
				// the 0 component of the array has the number of relevant AirflowNetwork surfaces for the zone
				AirflowNetworkSurfaceUCSDCV( 0, Loop ) = AuxSurf( Loop );
				if ( AuxSurf( Loop ) != 0 ) {
					Real64 const ceilingHeight( ZoneCeilingHeight( ( Loop - 1 ) * 2 + 1 ) );
					SurfNum = 1;
					for ( Loop2 = 1; Loop2 <= NumOfLinksMultiZone; ++Loop2 ) {
						if ( Surface( MultizoneSurfaceData( Loop2 ).SurfNum ).Zone == Loop ) {
							// SurfNum has the reference surface number relative to AirflowNetworkSurfaceData
							AirflowNetworkSurfaceUCSDCV( SurfNum, Loop ) = Loop2;
							// calculate the surface width and height
							CompNum = AirflowNetworkLinkageData( Loop2 ).CompNum;
							TypeNum = AirflowNetworkCompData( CompNum ).TypeNum;
							if ( AirflowNetworkCompData( CompNum ).CompTypeNum == CompTypeNum_DOP ) {
								WidthFactMax = 0.0;
								HeightFactMax = 0.0;
								for ( Loop3 = 1; Loop3 <= MultizoneCompDetOpeningData( TypeNum ).NumFac; ++Loop3 ) {
									if ( Loop3 == 1 ) {
										WidthFact = MultizoneCompDetOpeningData( TypeNum ).WidthFac1;
										HeightFact = MultizoneCompDetOpeningData( TypeNum ).HeightFac1;
									}
									if ( Loop3 == 2 ) {
										WidthFact = MultizoneCompDetOpeningData( TypeNum ).WidthFac2;
										HeightFact = MultizoneCompDetOpeningData( TypeNum ).HeightFac2;
									}
									if ( Loop3 == 3 ) {
										WidthFact = MultizoneCompDetOpeningData( TypeNum ).WidthFac3;
										HeightFact = MultizoneCompDetOpeningData( TypeNum ).HeightFac3;
									}
									if ( Loop3 == 4 ) {
										WidthFact = MultizoneCompDetOpeningData( TypeNum ).WidthFac4;
										HeightFact = MultizoneCompDetOpeningData( TypeNum ).HeightFac4;
									}
									if ( WidthFact > WidthFactMax ) {
										WidthFactMax = WidthFact;
									}
									if ( HeightFact > HeightFactMax ) {
										HeightFactMax = HeightFact;
									}
								}
								SurfParametersCVDV( Loop2 ).Width = WidthFactMax * Surface( MultizoneSurfaceData( Loop2 ).SurfNum ).Width;
								SurfParametersCVDV( Loop2 ).Height = HeightFactMax * Surface( MultizoneSurfaceData( Loop2 ).SurfNum ).Height;
							} else if ( AirflowNetworkCompData( CompNum ).CompTypeNum == CompTypeNum_SCR ) { // surface type = CRACK
								SurfParametersCVDV( Loop2 ).Width = Surface( MultizoneSurfaceData( Loop2 ).SurfNum ).Width / 2;
								AinCV = MultizoneSurfaceCrackData( TypeNum ).FlowCoef / ( BaseDischargeCoef * std::sqrt( 2.0 / PsyRhoAirFnPbTdbW( OutBaroPress, MAT( Loop ), ZoneAirHumRat( Loop ) ) ) );
								SurfParametersCVDV( Loop2 ).Height = AinCV / SurfParametersCVDV( Loop2 ).Width;
							}
							// calculate the surface Zmin and Zmax
							if ( AirflowNetworkCompData( CompNum ).CompTypeNum == CompTypeNum_DOP ) {
								AirflowNetworkSurfPtr = MultizoneSurfaceData( Loop2 ).SurfNum;
								NSides = Surface( MultizoneSurfaceData( Loop2 ).SurfNum ).Sides;
								Real64 z_min( std::numeric_limits< Real64 >::max() ), z_max( std::numeric_limits< Real64 >::lowest() );
								for ( int i = 1; i <= NSides; ++i ) {
									Real64 const z_i( Surface( AirflowNetworkSurfPtr ).Vertex( i ).z );
									z_min = std::min( z_min, z_i );
									z_max = std::max( z_max, z_i );
								}
								SurfParametersCVDV( Loop2 ).Zmin = z_min - ceilingHeight;
								SurfParametersCVDV( Loop2 ).Zmax = z_max - ceilingHeight;
							} else if ( AirflowNetworkCompData( CompNum ).CompTypeNum == CompTypeNum_SCR ) { // surface type = CRACK
								AirflowNetworkSurfPtr = MultizoneSurfaceData( Loop2 ).SurfNum;
								NSides = Surface( MultizoneSurfaceData( Loop2 ).SurfNum ).Sides;
								Real64 z_min( std::numeric_limits< Real64 >::max() ), z_max( std::numeric_limits< Real64 >::lowest() );
								for ( int i = 1; i <= NSides; ++i ) {
									Real64 const z_i( Surface( AirflowNetworkSurfPtr ).Vertex( i ).z );
									z_min = std::min( z_min, z_i );
									z_max = std::max( z_max, z_i );
								}
								SurfParametersCVDV( Loop2 ).Zmin = z_min - ceilingHeight;
								SurfParametersCVDV( Loop2 ).Zmax = z_max - ceilingHeight;
							}

							++SurfNum;
							// Check if airflow network Surface is an interzone surface:
						} else {
							NodeNum1 = MultizoneSurfaceData( Loop2 ).NodeNums( 1 );
							NodeNum2 = MultizoneSurfaceData( Loop2 ).NodeNums( 2 );
							if ( ( AirflowNetworkNodeData( NodeNum2 ).EPlusZoneNum == Loop && AirflowNetworkNodeData( NodeNum1 ).EPlusZoneNum > 0 ) || ( AirflowNetworkNodeData( NodeNum2 ).EPlusZoneNum > 0 && AirflowNetworkNodeData( NodeNum1 ).EPlusZoneNum == Loop ) ) {
								AirflowNetworkSurfaceUCSDCV( SurfNum, Loop ) = Loop2;
								++SurfNum;
							}
						}
					}
				}
			}

			AuxSurf.deallocate();

			if ( any( IsZoneDV ) || any( IsZoneUI ) ) {
				MaxTempGrad.allocate( NumOfZones );
				AvgTempGrad.allocate( NumOfZones );
				TCMF.allocate( NumOfZones );
				FracMinFlow.allocate( NumOfZones );
				ZoneAirSystemON.allocate( NumOfZones );
				// Allocate histories of displacement ventilation temperatures PH 3/5/04
				MATFloor.allocate( NumOfZones );
				XMATFloor.allocate( NumOfZones );
				XM2TFloor.allocate( NumOfZones );
				XM3TFloor.allocate( NumOfZones );
				XM4TFloor.allocate( NumOfZones );
				DSXMATFloor.allocate( NumOfZones );
				DSXM2TFloor.allocate( NumOfZones );
				DSXM3TFloor.allocate( NumOfZones );
				DSXM4TFloor.allocate( NumOfZones );
				MATOC.allocate( NumOfZones );
				XMATOC.allocate( NumOfZones );
				XM2TOC.allocate( NumOfZones );
				XM3TOC.allocate( NumOfZones );
				XM4TOC.allocate( NumOfZones );
				DSXMATOC.allocate( NumOfZones );
				DSXM2TOC.allocate( NumOfZones );
				DSXM3TOC.allocate( NumOfZones );
				DSXM4TOC.allocate( NumOfZones );
				MATMX.allocate( NumOfZones );
				XMATMX.allocate( NumOfZones );
				XM2TMX.allocate( NumOfZones );
				XM3TMX.allocate( NumOfZones );
				XM4TMX.allocate( NumOfZones );
				DSXMATMX.allocate( NumOfZones );
				DSXM2TMX.allocate( NumOfZones );
				DSXM3TMX.allocate( NumOfZones );
				DSXM4TMX.allocate( NumOfZones );
				ZTM1Floor.allocate( NumOfZones );
				ZTM2Floor.allocate( NumOfZones );
				ZTM3Floor.allocate( NumOfZones );
				ZTM1OC.allocate( NumOfZones );
				ZTM2OC.allocate( NumOfZones );
				ZTM3OC.allocate( NumOfZones );
				ZTM1MX.allocate( NumOfZones );
				ZTM2MX.allocate( NumOfZones );
				ZTM3MX.allocate( NumOfZones );
				AIRRATFloor.allocate( NumOfZones );
				AIRRATOC.allocate( NumOfZones );
				AIRRATMX.allocate( NumOfZones );
				ZTOC.allocate( NumOfZones );
				ZTMX.allocate( NumOfZones );
				ZTFloor.allocate( NumOfZones );
				HeightTransition.allocate( NumOfZones );
				Phi.allocate( NumOfZones );
				Zone1Floor.allocate( NumOfZones );
				ZoneMXFloor.allocate( NumOfZones );
				ZoneM2Floor.allocate( NumOfZones );
				Zone1OC.allocate( NumOfZones );
				ZoneMXOC.allocate( NumOfZones );
				ZoneM2OC.allocate( NumOfZones );
				Zone1MX.allocate( NumOfZones );
				ZoneMXMX.allocate( NumOfZones );
				ZoneM2MX.allocate( NumOfZones );

				MaxTempGrad = 0.0;
				AvgTempGrad = 0.0;
				TCMF = 23.0;
				FracMinFlow = 0.0;
				//      ZoneDVMixedFlagRep    = 0.0
				ZoneAirSystemON = false;
				//      ZoneDVMixedFlag=0
				MATFloor = 23.0;
				XMATFloor = 23.0;
				XM2TFloor = 23.0;
				XM3TFloor = 23.0;
				XM4TFloor = 23.0;
				DSXMATFloor = 23.0;
				DSXM2TFloor = 23.0;
				DSXM3TFloor = 23.0;
				DSXM4TFloor = 23.0;
				MATOC = 23.0;
				XMATOC = 23.0;
				XM2TOC = 23.0;
				XM3TOC = 23.0;
				XM4TOC = 23.0;
				DSXMATOC = 23.0;
				DSXM2TOC = 23.0;
				DSXM3TOC = 23.0;
				DSXM4TOC = 23.0;
				MATMX = 23.0;
				XMATMX = 23.0;
				XM2TMX = 23.0;
				XM3TMX = 23.0;
				XM4TMX = 23.0;
				DSXMATMX = 23.0;
				DSXM2TMX = 23.0;
				DSXM3TMX = 23.0;
				DSXM4TMX = 23.0;
				ZTM1Floor = 23.0;
				ZTM2Floor = 23.0;
				ZTM3Floor = 23.0;
				ZTM1OC = 23.0;
				ZTM2OC = 23.0;
				ZTM3OC = 23.0;
				ZTM1MX = 23.0;
				ZTM2MX = 23.0;
				ZTM3MX = 23.0;
				Zone1Floor = 23.0;
				ZoneMXFloor = 23.0;
				ZoneM2Floor = 23.0;
				Zone1OC = 23.0;
				ZoneMXOC = 23.0;
				ZoneM2OC = 23.0;
				Zone1MX = 23.0;
				ZoneMXMX = 23.0;
				ZoneM2MX = 23.0;
				AIRRATFloor = 0.0;
				AIRRATOC = 0.0;
				AIRRATMX = 0.0;
				ZTOC = 23.0;
				ZTMX = 23.0;
				ZTFloor = 23.0;
				HeightTransition = 0.0;
				Phi = 0.0;
				HCeiling = 0.0;
				HWall = 0.0;
				HFloor = 0.0;
				HInternal = 0.0;
				HWindow = 0.0;
				HDoor = 0.0;

			}

			if ( any( IsZoneDV ) ) {

				DVHcIn.allocate( TotSurfaces );
				ZoneDVMixedFlagRep.allocate( NumOfZones );
				ZoneDVMixedFlag.allocate( NumOfZones );
				DVHcIn = 0.0;
				ZoneDVMixedFlagRep = 0.0;
				ZoneDVMixedFlag = 0;
				// Output variables and DV zone flag
				for ( Loop = 1; Loop <= NumOfZones; ++Loop ) {
					if ( AirModel( Loop ).AirModelType != RoomAirModel_UCSDDV ) continue; //don't set these up if they don't make sense
					//CurrentModuleObject='RoomAirSettings:ThreeNodeDisplacementVentilation'
					SetupOutputVariable( "Room Air Zone Mixed Subzone Temperature [C]", ZTMX( Loop ), "HVAC", "State", Zone( Loop ).Name );
					SetupOutputVariable( "Room Air Zone Occupied Subzone Temperature [C]", ZTOC( Loop ), "HVAC", "State", Zone( Loop ).Name );
					SetupOutputVariable( "Room Air Zone Floor Subzone Temperature [C]", ZTFloor( Loop ), "HVAC", "State", Zone( Loop ).Name );
					SetupOutputVariable( "Room Air Zone Transition Height [m]", HeightTransition( Loop ), "HVAC", "State", Zone( Loop ).Name );
					SetupOutputVariable( "Room Air Zone Recommended Minimum Flow Fraction []", FracMinFlow( Loop ), "HVAC", "State", Zone( Loop ).Name );
					SetupOutputVariable( "Room Air Zone Is Mixed Status []", ZoneDVMixedFlagRep( Loop ), "HVAC", "State", Zone( Loop ).Name );
					SetupOutputVariable( "Room Air Zone Average Temperature Gradient [K/m]", AvgTempGrad( Loop ), "HVAC", "State", Zone( Loop ).Name );
					SetupOutputVariable( "Room Air Zone Maximum Temperature Gradient [K/m]", MaxTempGrad( Loop ), "HVAC", "State", Zone( Loop ).Name );
					SetupOutputVariable( "Room Air Zone Thermal Comfort Effective Air Temperature [C]", TCMF( Loop ), "HVAC", "State", Zone( Loop ).Name );
					SetupOutputVariable( "Room Air Zone Thermostat Temperature [C]", TempTstatAir( Loop ), "HVAC", "State", Zone( Loop ).Name );
				}

			}

			if ( any( IsZoneUI ) ) {
				ZoneUFMixedFlag.allocate( NumOfZones );
				ZoneUFMixedFlagRep.allocate( NumOfZones );
				UFHcIn.allocate( TotSurfaces );
				ZoneUFGamma.allocate( NumOfZones );
				ZoneUFPowInPlumes.allocate( NumOfZones );
				ZoneUFPowInPlumesfromWindows.allocate( NumOfZones );
				ZoneUFMixedFlag = 0;
				ZoneUFMixedFlagRep = 0.0;
				UFHcIn = 0.0;
				ZoneUFGamma = 0.0;
				ZoneUFPowInPlumes = 0.0;
				ZoneUFPowInPlumesfromWindows = 0.0;
				// Output variables and UF zone flag
				for ( Loop = 1; Loop <= NumOfZones; ++Loop ) {
					if ( AirModel( Loop ).AirModelType != RoomAirModel_UCSDUFI ) continue; //don't set these up if they don't make sense
					//CurrentModuleObject='RoomAirSettings:UnderFloorAirDistributionInterior'
					SetupOutputVariable( "Room Air Zone Mixed Subzone Temperature [C]", ZTMX( Loop ), "HVAC", "State", Zone( Loop ).Name );
					SetupOutputVariable( "Room Air Zone Occupied Subzone Temperature [C]", ZTOC( Loop ), "HVAC", "State", Zone( Loop ).Name );
					SetupOutputVariable( "Room Air Zone Transition Height [m]", HeightTransition( Loop ), "HVAC", "State", Zone( Loop ).Name );
					SetupOutputVariable( "Room Air Zone Is Mixed Status []", ZoneUFMixedFlagRep( Loop ), "HVAC", "State", Zone( Loop ).Name );
					SetupOutputVariable( "Room Air Zone Average Temperature Gradient [K/m]", AvgTempGrad( Loop ), "HVAC", "State", Zone( Loop ).Name );
					SetupOutputVariable( "Room Air Zone Effective Comfort Air Temperature [C]", TCMF( Loop ), "HVAC", "State", Zone( Loop ).Name );
					SetupOutputVariable( "Room Air Zone Thermostat Temperature [C]", TempTstatAir( Loop ), "HVAC", "State", Zone( Loop ).Name );
					SetupOutputVariable( "Room Air Zone Transition Height Gamma Value []", ZoneUFGamma( Loop ), "HVAC", "State", Zone( Loop ).Name );
					SetupOutputVariable( "Room Air Zone Plume Heat Transfer Rate [W]", ZoneUFPowInPlumes( Loop ), "HVAC", "State", Zone( Loop ).Name );
					SetupOutputVariable( "Room Air Zone Temperature Stratification Fraction []", Phi( Loop ), "HVAC", "State", Zone( Loop ).Name );

					// set zone equip pointer in the UCSDUI data structure
					for ( ZoneEquipConfigNum = 1; ZoneEquipConfigNum <= NumOfZones; ++ZoneEquipConfigNum ) {
						if ( ZoneEquipConfig( ZoneEquipConfigNum ).ActualZoneNum == Loop ) {
							ZoneUCSDUI( ZoneUFPtr( Loop ) ).ZoneEquipPtr = ZoneEquipConfigNum;
							break;
						}
					} // ZoneEquipConfigNum
				}
				for ( Loop = 1; Loop <= NumOfZones; ++Loop ) {
					if ( AirModel( Loop ).AirModelType != RoomAirModel_UCSDUFE ) continue; //don't set these up if they don't make sense
					//CurrentModuleObject='RoomAirSettings:UnderFloorAirDistributionExterior'
					SetupOutputVariable( "Room Air Zone Mixed Subzone Temperature [C]", ZTMX( Loop ), "HVAC", "State", Zone( Loop ).Name );
					SetupOutputVariable( "Room Air Zone Occupied Subzone Temperature [C]", ZTOC( Loop ), "HVAC", "State", Zone( Loop ).Name );
					SetupOutputVariable( "Room Air Zone Transition Height [m]", HeightTransition( Loop ), "HVAC", "State", Zone( Loop ).Name );
					SetupOutputVariable( "Room Air Zone Is Mixed Status []", ZoneUFMixedFlagRep( Loop ), "HVAC", "State", Zone( Loop ).Name );
					SetupOutputVariable( "Room Air Zone Average Temperature Gradient [K/m]", AvgTempGrad( Loop ), "HVAC", "State", Zone( Loop ).Name );
					SetupOutputVariable( "Room Air Zone Effective Comfort Air Temperature [C]", TCMF( Loop ), "HVAC", "State", Zone( Loop ).Name );
					SetupOutputVariable( "Room Air Zone Thermostat Temperature [C]", TempTstatAir( Loop ), "HVAC", "State", Zone( Loop ).Name );
					SetupOutputVariable( "Room Air Zone Transition Height Gamma Value []", ZoneUFGamma( Loop ), "HVAC", "State", Zone( Loop ).Name );
					SetupOutputVariable( "Room Air Zone Plume Heat Transfer Rate [W]", ZoneUFPowInPlumes( Loop ), "HVAC", "State", Zone( Loop ).Name );
					SetupOutputVariable( "Room Air Zone Window Plume Heat Transfer Rate [W]", ZoneUFPowInPlumesfromWindows( Loop ), "HVAC", "State", Zone( Loop ).Name );
					SetupOutputVariable( "Room Air Zone Temperature Stratification Fraction []", Phi( Loop ), "HVAC", "State", Zone( Loop ).Name );
					// set zone equip pointer in the UCSDUE data structure
					for ( ZoneEquipConfigNum = 1; ZoneEquipConfigNum <= NumOfZones; ++ZoneEquipConfigNum ) {
						if ( ZoneEquipConfig( ZoneEquipConfigNum ).ActualZoneNum == Loop ) {
							ZoneUCSDUE( ZoneUFPtr( Loop ) ).ZoneEquipPtr = ZoneEquipConfigNum;
							break;
						}
					} // ZoneEquipConfigNum
				}
			}

			if ( any( IsZoneCV ) ) {
				CVHcIn.allocate( TotSurfaces );
				ZTJET.allocate( NumOfZones );
				// Most ZTJet takes defaults
				ZTREC.allocate( NumOfZones );
				RoomOutflowTemp.allocate( NumOfZones );
				// Most ZTREC takes defaults
				JetRecAreaRatio.allocate( NumOfZones );
				Urec.allocate( NumOfZones );
				Ujet.allocate( NumOfZones );
				Qrec.allocate( NumOfZones );
				Qtot.allocate( NumOfZones );
				RecInflowRatio.allocate( NumOfZones );
				Uhc.allocate( NumOfZones );
				Ain.allocate( NumOfZones );
				Tin.allocate( NumOfZones );
				Droom.allocate( NumOfZones );
				Dstar.allocate( NumOfZones );
				ZoneCVisMixing.allocate( NumOfZones );
				Rfr.allocate( NumOfZones );
				ZoneCVhasREC.allocate( NumOfZones );

				ZTJET = 23.0;
				RoomOutflowTemp = 23.0;
				ZTREC = 23.0;
				CVHcIn = 0.0;
				JetRecAreaRatio = 0.2;
				Urec = 0.2;
				Ujet = 0.2;
				Qrec = 0.2;
				Uhc = 0.2;
				Ain = 1.0;
				Tin = 23.0;
				Droom = 6.0;
				ZoneCVisMixing = 0.0;
				Rfr = 10.0;
				ZoneCVhasREC = 1.0;
				HCeiling = 0.0;
				HWall = 0.0;
				HFloor = 0.0;
				HInternal = 0.0;
				HWindow = 0.0;
				HDoor = 0.0;

				for ( Loop = 1; Loop <= NumOfZones; ++Loop ) {
					if ( AirModel( Loop ).AirModelType != RoomAirModel_UCSDCV ) continue; //don't set these up if they don't make sense
					ZoneEquipConfigNum = ZoneNum;
					// check whether this zone is a controlled zone or not
					if ( ZoneEquipConfig( ZoneEquipConfigNum ).IsControlled ) {
						IsZoneCV( Loop ) = false;
						AirModel( Loop ).SimAirModel = false;
						ShowSevereError( "Unmixed Flow: Cross Ventilation cannot be applied for Zone=" + Zone( Loop ).Name );
						ShowContinueError( "An HVAC system is present in the zone. Fully mixed airflow model will be used for Zone=" + Zone( Loop ).Name );
						continue;
					}
					//CurrentModuleObject='RoomAirSettings:CrossVentilation'
					SetupOutputVariable( "Room Air Zone Jet Region Temperature [C]", ZTJET( Loop ), "Zone", "Average", Zone( Loop ).Name );
					SetupOutputVariable( "Room Air Zone Recirculation Region Temperature [C]", ZTREC( Loop ), "Zone", "Average", Zone( Loop ).Name );
					SetupOutputVariable( "Room Air Zone Jet Region Average Air Velocity [m/s]", Ujet( Loop ), "Zone", "Average", Zone( Loop ).Name );
					SetupOutputVariable( "Room Air Zone Recirculation Region Average Air Velocity [m/s]", Urec( Loop ), "Zone", "Average", Zone( Loop ).Name );
					SetupOutputVariable( "Room Air Zone Recirculation and Inflow Rate Ratio []", RecInflowRatio( Loop ), "Zone", "Average", Zone( Loop ).Name );
					SetupOutputVariable( "Room Air Zone Inflow Opening Area [m2]", Ain( Loop ), "Zone", "Average", Zone( Loop ).Name );
					SetupOutputVariable( "Room Air Zone Room Length [m]", Dstar( Loop ), "Zone", "Average", Zone( Loop ).Name );
					SetupOutputVariable( "Room Air Zone Is Mixing Status []", ZoneCVisMixing( Loop ), "Zone", "State", Zone( Loop ).Name );
					SetupOutputVariable( "Room Air Zone Is Recirculating Status []", ZoneCVhasREC( Loop ), "Zone", "State", Zone( Loop ).Name );
					for ( i = 1; i <= AirflowNetworkSurfaceUCSDCV( 0, ZoneNum ); ++i ) {
						N = AirflowNetworkLinkageData( i ).CompNum;
						if ( AirflowNetworkCompData( N ).CompTypeNum == CompTypeNum_DOP ) {
							SurfNum = MultizoneSurfaceData( i ).SurfNum;
							SetupOutputVariable( "Room Air Window Jet Region Average Air Velocity [m/s]", CVJetRecFlows( i, Loop ).Ujet, "Zone", "Average", MultizoneSurfaceData( i ).SurfName );
						}
					}
				}
			}

			MyEnvrnFlag = true;

			MyOneTimeFlag = false;

		}

		// Do the Begin Environment initializations
		if ( BeginEnvrnFlag && MyEnvrnFlag( ZoneNum ) ) {

			if ( IsZoneDV( ZoneNum ) || IsZoneUI( ZoneNum ) ) {

				MaxTempGrad( ZoneNum ) = 0.0;
				AvgTempGrad( ZoneNum ) = 0.0;
				TCMF( ZoneNum ) = 23.0;
				FracMinFlow( ZoneNum ) = 0.0;
				ZoneAirSystemON( ZoneNum ) = false;
				MATFloor( ZoneNum ) = 23.0;
				XMATFloor( ZoneNum ) = 23.0;
				XM2TFloor( ZoneNum ) = 23.0;
				XM3TFloor( ZoneNum ) = 23.0;
				XM4TFloor( ZoneNum ) = 23.0;
				DSXMATFloor( ZoneNum ) = 23.0;
				DSXM2TFloor( ZoneNum ) = 23.0;
				DSXM3TFloor( ZoneNum ) = 23.0;
				DSXM4TFloor( ZoneNum ) = 23.0;
				MATOC( ZoneNum ) = 23.0;
				XMATOC( ZoneNum ) = 23.0;
				XM2TOC( ZoneNum ) = 23.0;
				XM3TOC( ZoneNum ) = 23.0;
				XM4TOC( ZoneNum ) = 23.0;
				DSXMATOC( ZoneNum ) = 23.0;
				DSXM2TOC( ZoneNum ) = 23.0;
				DSXM3TOC( ZoneNum ) = 23.0;
				DSXM4TOC( ZoneNum ) = 23.0;
				MATMX( ZoneNum ) = 23.0;
				XMATMX( ZoneNum ) = 23.0;
				XM2TMX( ZoneNum ) = 23.0;
				XM3TMX( ZoneNum ) = 23.0;
				XM4TMX( ZoneNum ) = 23.0;
				DSXMATMX( ZoneNum ) = 23.0;
				DSXM2TMX( ZoneNum ) = 23.0;
				DSXM3TMX( ZoneNum ) = 23.0;
				DSXM4TMX( ZoneNum ) = 23.0;
				ZTM1Floor( ZoneNum ) = 23.0;
				ZTM2Floor( ZoneNum ) = 23.0;
				ZTM3Floor( ZoneNum ) = 23.0;
				Zone1Floor( ZoneNum ) = 23.0;
				ZoneMXFloor( ZoneNum ) = 23.0;
				ZoneM2Floor( ZoneNum ) = 23.0;
				ZTM1OC( ZoneNum ) = 23.0;
				ZTM2OC( ZoneNum ) = 23.0;
				ZTM3OC( ZoneNum ) = 23.0;
				Zone1OC( ZoneNum ) = 23.0;
				ZoneMXOC( ZoneNum ) = 23.0;
				ZoneM2OC( ZoneNum ) = 23.0;
				ZTM1MX( ZoneNum ) = 23.0;
				ZTM2MX( ZoneNum ) = 23.0;
				ZTM3MX( ZoneNum ) = 23.0;
				Zone1MX( ZoneNum ) = 23.0;
				ZoneMXMX( ZoneNum ) = 23.0;
				ZoneM2MX( ZoneNum ) = 23.0;
				AIRRATFloor( ZoneNum ) = 0.0;
				AIRRATOC( ZoneNum ) = 0.0;
				AIRRATMX( ZoneNum ) = 0.0;
				ZTOC( ZoneNum ) = 23.0;
				ZTMX( ZoneNum ) = 23.0;
				ZTFloor( ZoneNum ) = 23.0;
				HeightTransition( ZoneNum ) = 0.0;
				Phi( ZoneNum ) = 0.0;
				HCeiling = 0.0;
				HWall = 0.0;
				HFloor = 0.0;
				HInternal = 0.0;
				HWindow = 0.0;
				HDoor = 0.0;

			}

			if ( IsZoneDV( ZoneNum ) ) {

				DVHcIn = 0.0;
				ZoneDVMixedFlagRep( ZoneNum ) = 0.0;
				ZoneDVMixedFlag( ZoneNum ) = 0;

			}

			if ( IsZoneUI( ZoneNum ) ) {

				UFHcIn = 0.0;
				ZoneUFMixedFlag( ZoneNum ) = 0;
				ZoneUFMixedFlagRep( ZoneNum ) = 0.0;
				ZoneUFGamma( ZoneNum ) = 0.0;
				ZoneUFPowInPlumes( ZoneNum ) = 0.0;
				ZoneUFPowInPlumesfromWindows( ZoneNum ) = 0.0;

			}

			if ( IsZoneCV( ZoneNum ) ) {
				ZTJET( ZoneNum ) = 23.0;
				RoomOutflowTemp( ZoneNum ) = 23.0;
				ZTREC( ZoneNum ) = 23.0;
				CVHcIn = 0.0;
				JetRecAreaRatio( ZoneNum ) = 0.2;
				Urec( ZoneNum ) = 0.2;
				Ujet( ZoneNum ) = 0.2;
				Uhc( ZoneNum ) = 0.2;
				Ain( ZoneNum ) = 1.0;
				Tin( ZoneNum ) = 23.0;
				Droom( ZoneNum ) = 6.0;
				Dstar( ZoneNum ) = 6.0;
				ZoneCVisMixing( ZoneNum ) = 0.0;
				Rfr( ZoneNum ) = 10.0;
				ZoneCVhasREC( ZoneNum ) = 1.0;
				HCeiling = 0.0;
				HWall = 0.0;
				HFloor = 0.0;
				HInternal = 0.0;
				HWindow = 0.0;
				HDoor = 0.0;

			}

			MyEnvrnFlag( ZoneNum ) = false;
		} // end one time inits

		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag( ZoneNum ) = true;
		}

	}

	void
	GetRAFNNodeNum(
		std::string const & RAFNNodeName, // Name of RoomAir:Node:AirflowNetwork
		int & ZoneNum, // The zone number associate with the node name
		int & RAFNNodeNum, // RoomAir:Node:AirflowNetwork Number
		bool & Errorfound // true if an error is found
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Lixing Gu
		//       DATE WRITTEN   November 2014
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the given RoomAirNode name and returns the Zone number and RoomAir node
		// number. If incorrect name is given, errorsfound is returned as true and value is returned
		// as zero.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int I;  // Zone index

		//Obtains and Allocates RoomAirSettings : AirflowNetwork
		if ( GetAirModelData ) {
			GetAirModelDatas();
			GetAirModelData = false;
		}

		Errorfound = false;
		RAFNNodeNum = 0;
		for ( I = 1; I <= NumOfZones; ++I ) {
			if ( RoomAirflowNetworkZoneInfo( I ).NumOfAirNodes > 0 ) {
				RAFNNodeNum = FindItemInList( RAFNNodeName, RoomAirflowNetworkZoneInfo( I ).Node, RoomAirflowNetworkZoneInfo( I ).NumOfAirNodes );
				if ( RAFNNodeNum > 0 ) {
					ZoneNum = I;
					break;
				}
			}
		}

		if ( RAFNNodeNum == 0 ) {
			Errorfound = true;
			ShowSevereError( "Could not find RoomAir:Node:AirflowNetwork number with AirflowNetwork:IntraZone:Node Name='" + RAFNNodeName );
		}
	}


	bool
	CheckEquipName(
		int ZoneNum,  // Zone number
		std::string const & EquipType, // Equipment type
		std::string const & EquipName, // Equipment Name
		std::string & SupplyNodeName, // Supply node name
		std::string & ReturnNodeName, // Return node name
		int TotNumEquip, // equipment type number
		int TypeNum // Supply air node number
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Lixing Gu
		//       DATE WRITTEN   March 2014
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function looks up the given RoomAirNode name and returns the Zone number and RoomAir node
		// number.If incorrect name is given, errorsfound is returned as true and value is returned
		// as zero.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using InputProcessor::SameString;
		using InputProcessor::GetObjectDefMaxArgs;
		using InputProcessor::GetObjectItem;
		using DataRoomAirModel::AirNode;
		using DataRoomAirModel::TotNumOfAirNodes;
		using DataLoopNode::NodeID;
		using namespace DataIPShortCuts;
		using Fans::GetFanOutletNode;
		using DataZoneEquipment::ZoneEquipConfig;

		// Return value
		bool EquipFind; // True if an error is found

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int NumAlphas;
		int NumNumbers;
		int I;
		int Status;                // Used in GetObjectItem
		int MaxNums = 0;           // Maximum number of numeric input fields
		int MaxAlphas = 0;         // Maximum number of alpha input fields
		int TotalArgs = 0;         // Total number of alpha and numeric arguments(max) for a
		Array1D_string Alphas;    // Alpha input items for object
		Array1D< Real64 > Numbers; // Numeric input items for object
		bool errorfound;

		NumAlphas = 1;
		NumNumbers = 1;
		EquipFind = false;

		SupplyNodeName = "";

		if ( TypeNum == 0 ) return EquipFind;

		GetObjectDefMaxArgs( EquipType, TotalArgs, NumAlphas, NumNumbers );

		MaxNums = max( MaxNums, NumNumbers );
		MaxAlphas = max( MaxAlphas, NumAlphas );

		if ( MaxNums > NumNumbers ) {
			Numbers.allocate( MaxNums );
			Numbers = 0.0;
		} else if ( !allocated( Numbers ) ) {
			Numbers.allocate( MaxNums );
		}

		if ( MaxAlphas > NumAlphas ) {
			Alphas.allocate( MaxAlphas );
			Alphas = "";
		} else if ( !allocated( Alphas ) ) {
			Alphas.allocate( NumAlphas );
		}

		for ( I = 1; I <= TotNumEquip; ++I ) {
			GetObjectItem( EquipType, I, Alphas, NumAlphas, Numbers, NumNumbers, Status );
			if ( SameString( Alphas( 1 ), EquipName ) ) {
				EquipFind = true;
				break;
			}
		}

		if ( TypeNum == 1 ) {  // ZoneHVAC:TerminalUnit : VariableRefrigerantFlow
			SupplyNodeName = Alphas( 4 );
			ReturnNodeName = ""; // Zone return node
		} else if ( TypeNum == 2 ) {  // ZoneHVAC : EnergyRecoveryVentilator
			I = GetFanOutletNode( "Fan:OnOff", Alphas( 4 ), errorfound );
			if ( errorfound ) {

			}
			SupplyNodeName = NodeID( I ); // ?????
			ReturnNodeName = ""; // Zone exhaust node
		} else if ( TypeNum == 3 ) {  // ZoneHVAC : FourPipeFanCoil
			SupplyNodeName = Alphas( 6 );
			ReturnNodeName = Alphas( 5 );
		} else if ( TypeNum == 4 ) {  // ZoneHVAC : OutdoorAirUnit
			SupplyNodeName = Alphas( 13 );
			ReturnNodeName = Alphas( 14 );
		} else if ( TypeNum == 5 ) {  // ZoneHVAC : PackagedTerminalAirConditioner
			SupplyNodeName = Alphas( 4 );
			ReturnNodeName = Alphas( 3 );
		} else if ( TypeNum == 6 ) {  // ZoneHVAC : PackagedTerminalHeatPump
			SupplyNodeName = Alphas( 4 );
			ReturnNodeName = Alphas( 3 );
		} else if ( TypeNum == 7 ) {  // ZoneHVAC : UnitHeater
			SupplyNodeName = Alphas( 4 );
			ReturnNodeName = Alphas( 3 );
		} else if ( TypeNum == 8 ) {  // ZoneHVAC : UnitVentilator
			SupplyNodeName = Alphas( 7 );
			ReturnNodeName = Alphas( 6 );
		} else if ( TypeNum == 9 ) {  // ZoneHVAC : VentilatedSlab
			SupplyNodeName = Alphas( 20 );
			ReturnNodeName = Alphas( 18 );
		} else if ( TypeNum == 10 ) {  // ZoneHVAC : WaterToAirHeatPump
			SupplyNodeName = Alphas( 4 );
			ReturnNodeName = Alphas( 3 );
		} else if ( TypeNum == 11 ) {  // ZoneHVAC : WindowAirConditioner
			SupplyNodeName = Alphas( 4 );
			ReturnNodeName = Alphas( 3 );
		} else if ( TypeNum == 12 ) {  // ZoneHVAC : Baseboard : RadiantConvective : Electric
			SupplyNodeName = ""; // convection only
		} else if ( TypeNum == 13 ) {  // ZoneHVAC : Baseboard : RadiantConvective : Water
			SupplyNodeName = "";
		} else if ( TypeNum == 14 ) {  // ZoneHVAC : Baseboard : RadiantConvective : Steam
			SupplyNodeName = "";
		} else if ( TypeNum == 15 ) {  // ZoneHVAC : Baseboard : Convective : Electric
			SupplyNodeName = "";
		} else if ( TypeNum == 16 ) {  // ZoneHVAC : Baseboard : Convective : Water
			SupplyNodeName = "";
		} else if ( TypeNum == 17 ) {  // ZoneHVAC : HighTemperatureRadiant
			SupplyNodeName = "";
		} else if ( TypeNum == 18 ) {  // ZoneHVAC : Dehumidifier : DX
			SupplyNodeName = Alphas( 4 );
			ReturnNodeName = Alphas( 3 );
		} else if ( TypeNum == 19 ) {  // ZoneHVAC : IdealLoadsAirSystem
			SupplyNodeName = Alphas( 3 );
			ReturnNodeName = Alphas( 4 );
		} else if ( TypeNum == 20 ) {  // ZoneHVAC : RefrigerationChillerSet
			SupplyNodeName = Alphas( 5 );
			ReturnNodeName = Alphas( 4 );
		} else if ( TypeNum == 21 ) {  // Fan : ZoneExhaust
			SupplyNodeName = ""; // ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? May not use
		} else if ( TypeNum == 22 ) {  // WaterHeater : HeatPump
			SupplyNodeName = Alphas( 8 );
			ReturnNodeName = Alphas( 7 );
		} else if ( TypeNum == 23 ) {  // AirTerminal : SingleDuct : Uncontrolled
			SupplyNodeName = Alphas( 3 );
			if ( allocated( ZoneEquipConfig ) ) {
				ReturnNodeName = NodeID( ZoneEquipConfig( ZoneNum ).ReturnAirNode ); // Zone return node
			} else {
				ReturnNodeName = "";
			}
		} else if ( TypeNum == 24 ) {  // AirTerminal : DualDuct : ConstantVolume
			SupplyNodeName = Alphas( 1 );
			if ( allocated( ZoneEquipConfig ) ) {
				ReturnNodeName = NodeID( ZoneEquipConfig( ZoneNum ).ReturnAirNode ); // Zone return node
			} else {
				ReturnNodeName = "";
			}
		} else if ( TypeNum == 25 ) {  // AirTerminal : DualDuct : VAV
			SupplyNodeName = Alphas( 1 );
			if ( allocated( ZoneEquipConfig ) ) {
				ReturnNodeName = NodeID( ZoneEquipConfig( ZoneNum ).ReturnAirNode ); // Zone return node
			} else {
				ReturnNodeName = "";
			}
		} else if ( TypeNum == 26 ) {  // AirTerminal : SingleDuct : ConstantVolume : Reheat
			SupplyNodeName = Alphas( 1 );
			if ( allocated( ZoneEquipConfig ) ) {
				ReturnNodeName = NodeID( ZoneEquipConfig( ZoneNum ).ReturnAirNode ); // Zone return node
			} else {
				ReturnNodeName = "";
			}
		} else if ( TypeNum == 27 ) {  // AirTerminal : SingleDuct : VAV : Reheat
			SupplyNodeName = Alphas( 1 );
			if ( allocated( ZoneEquipConfig ) ) {
				ReturnNodeName = NodeID( ZoneEquipConfig( ZoneNum ).ReturnAirNode ); // Zone return node
			} else {
				ReturnNodeName = "";
			}
		} else if ( TypeNum == 28 ) {  // AirTerminal : SingleDuct : VAV : NoReheat
			SupplyNodeName = Alphas( 1 );
			if ( allocated( ZoneEquipConfig ) ) {
				ReturnNodeName = NodeID( ZoneEquipConfig( ZoneNum ).ReturnAirNode ); // Zone return node
			} else {
				ReturnNodeName = "";
			}
		} else if ( TypeNum == 29 ) {  // AirTerminal : SingleDuct : SeriesPIU : Reheat
			SupplyNodeName = Alphas( 1 );
			if ( allocated( ZoneEquipConfig ) ) {
				ReturnNodeName = NodeID( ZoneEquipConfig( ZoneNum ).ReturnAirNode ); // Zone return node
			} else {
				ReturnNodeName = "";
			}
		} else if ( TypeNum == 30 ) {  // AirTerminal : SingleDuct : ParallelPIU : Reheat
			SupplyNodeName = Alphas( 1 );
			if ( allocated( ZoneEquipConfig ) ) {
				ReturnNodeName = NodeID( ZoneEquipConfig( ZoneNum ).ReturnAirNode ); // Zone return node
			} else {
				ReturnNodeName = "";
			}
		} else if ( TypeNum == 31 ) {  // AirTerminal : SingleDuct : ConstantVolume : FourPipeInduction
			SupplyNodeName = Alphas( 1 );
			if ( allocated( ZoneEquipConfig ) ) {
				ReturnNodeName = NodeID( ZoneEquipConfig( ZoneNum ).ReturnAirNode ); // Zone return node
			} else {
				ReturnNodeName = "";
			}
		} else if ( TypeNum == 32 ) {  // AirTerminal : SingleDuct : VAV : Reheat : VariableSpeedFan
			SupplyNodeName = Alphas( 1 );
			if ( allocated( ZoneEquipConfig ) ) {
				ReturnNodeName = NodeID( ZoneEquipConfig( ZoneNum ).ReturnAirNode ); // Zone return node
			} else {
				ReturnNodeName = "";
			}
		} else if ( TypeNum == 33 ) {  // AirTerminal : SingleDuct : VAV : HeatAndCool : Reheat
			SupplyNodeName = Alphas( 1 );
			if ( allocated( ZoneEquipConfig ) ) {
				ReturnNodeName = NodeID( ZoneEquipConfig( ZoneNum ).ReturnAirNode ); // Zone return node
			} else {
				ReturnNodeName = "";
			}
		} else if ( TypeNum == 34 ) {  // AirTerminal : SingleDuct : VAV : HeatAndCool : NoReheat
			SupplyNodeName = Alphas( 1 );
			if ( allocated( ZoneEquipConfig ) ) {
				ReturnNodeName = NodeID( ZoneEquipConfig( ZoneNum ).ReturnAirNode ); // Zone return node
			} else {
				ReturnNodeName = "";
			}
		} else if ( TypeNum == 35 ) {  // AirTerminal : SingleDuct : ConstantVolume : CooledBeam
			SupplyNodeName = Alphas( 5 );
			if ( allocated( ZoneEquipConfig ) ) {
				ReturnNodeName = NodeID( ZoneEquipConfig( ZoneNum ).ReturnAirNode ); // Zone return node
			} else {
				ReturnNodeName = "";
			}
		} else if ( TypeNum == 36 ) {  // AirTerminal : DualDuct : VAV : OutdoorAir
			SupplyNodeName = Alphas( 3 );
			if ( allocated( ZoneEquipConfig ) ) {
				ReturnNodeName = NodeID( ZoneEquipConfig( ZoneNum ).ReturnAirNode ); // Zone return node
			} else {
				ReturnNodeName = "";
			}
		} else if ( TypeNum == 37 ) {  // AirLoopHVACReturnAir
			SupplyNodeName = Alphas( 4 ); //
			ReturnNodeName = ""; //
		}

		// Need to find a better to handle allocate and deallocate
		if ( MaxAlphas > NumAlphas ) {
			Alphas.deallocate();
		}
		if ( MaxNums > NumNumbers ) {
			Numbers.deallocate();
		}

		return EquipFind;

	}


	//*****************************************************************************************

} // RoomAirModelManager

} // EnergyPlus
