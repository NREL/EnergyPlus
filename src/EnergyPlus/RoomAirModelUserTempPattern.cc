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
#include <ObjexxFCL/ArrayS.functions.hh>
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/member.functions.hh>

// EnergyPlus Headers
#include <RoomAirModelUserTempPattern.hh>
#include <DataEnvironment.hh>
#include <DataErrorTracking.hh>
#include <DataGlobals.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHVACGlobals.hh>
#include <DataLoopNode.hh>
#include <DataPrecisionGlobals.hh>
#include <DataRoomAirModel.hh>
#include <DataSurfaces.hh>
#include <DataZoneEquipment.hh>
#include <FluidProperties.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <InternalHeatGains.hh>
#include <OutputProcessor.hh>
#include <OutputReportTabular.hh>
#include <Psychrometrics.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace RoomAirModelUserTempPattern {

	// MODULE INFORMATION:
	//       AUTHOR         Brent Griffith
	//       DATE WRITTEN   August 2005 (started in January 2004)
	//       RE-ENGINEERED

	// PURPOSE OF THIS MODULE:
	// This module is the main module for running the
	// user-defined temperature pattern model.
	// This "air model" doesn't predict anything about the room air
	// but provides a method for users to model the
	// impact of non-uniform air temps.  the distribution of air temperatures
	// is defined by the user and referred to as a "pattern"

	// METHODOLOGY EMPLOYED:
	// This module contains all subroutines required by the
	// user defined temperature pattern roomair modeling.
	// See DataRoomAir.cc for variable declarations

	// REFERENCES:
	// none

	// OTHER NOTES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataGlobals::DisplayExtraWarnings;
	using namespace DataRoomAirModel;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	static std::string const BlankString;

	// MODULE DERIVED TYPE DEFINITIONS:

	// INTERFACE BLOCK SPECIFICATIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:
	// see DataRoomAir

	// SUBROUTINE SPECIFICATIONS FOR MODULE TempDistSimMgr

	// main subsroutine

	// get input routines are in RoomAirManager.cc

	// Routines for transferring data between Heat Balance and Air model domains

	// Routines for actual calculations in TempDist model

	// MODULE SUBROUTINES:

	// Functions

	void
	ManageUserDefinedPatterns( int const ZoneNum ) // index number for the specified zone
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   January 2004/Aug 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//  manage the user-defined air temp. distribution model

		// METHODOLOGY EMPLOYED:
		// calls subroutines

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		// FLOW:

		// transfer data from surface domain to air domain for the specified zone
		InitTempDistModel( ZoneNum );

		GetSurfHBDataForTempDistModel( ZoneNum );

		// perform TempDist model calculations
		CalcTempDistModel( ZoneNum );

		// transfer data from air domain back to surface domain for the specified zone
		SetSurfHBDataForTempDistModel( ZoneNum );

	}

	//****************************************************

	void
	InitTempDistModel( int const ZoneNum ) // index number for the specified zone
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         <author>
		//       DATE WRITTEN   <date_written>
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::NumOfZones;
		using DataGlobals::BeginEnvrnFlag;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static Array1D_bool MyEnvrnFlag; // flag for init once at start of environment
		static bool MyOneTimeFlag( true ); // one time setup flag
		int SurfNum; // do loop counter

		if ( MyOneTimeFlag ) {
			MyEnvrnFlag.dimension( NumOfZones, true );
			MyOneTimeFlag = false;
		}

		if ( BeginEnvrnFlag && MyEnvrnFlag( ZoneNum ) ) {
			AirPatternZoneInfo( ZoneNum ).TairMean = 23.0;
			AirPatternZoneInfo( ZoneNum ).Tstat = 23.0;
			AirPatternZoneInfo( ZoneNum ).Tleaving = 23.0;
			AirPatternZoneInfo( ZoneNum ).Texhaust = 23.0;
			AirPatternZoneInfo( ZoneNum ).Gradient = 0.0;
			for ( SurfNum = 1; SurfNum <= AirPatternZoneInfo( ZoneNum ).totNumSurfs; ++SurfNum ) {
				AirPatternZoneInfo( ZoneNum ).Surf( SurfNum ).TadjacentAir = 23.0;
			}
			MyEnvrnFlag( ZoneNum ) = false;
		}

		if ( ! BeginEnvrnFlag ) MyEnvrnFlag( ZoneNum ) = true;

		// init report variable
		AirPatternZoneInfo( ZoneNum ).Gradient = 0.0;

	}

	void
	GetSurfHBDataForTempDistModel( int const ZoneNum ) // index number for the specified zone
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   August 2005
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//  map data from Heat Balance domain to Room Air Modeling Domain
		//  for the current zone, (only need mean air temp)
		//  also acts as an init routine

		// METHODOLOGY EMPLOYED:
		// use ZT from DataHeatBalFanSys

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Using/Aliasing
		using DataHeatBalFanSys::MAT;
		using DataHeatBalFanSys::ZT;
		using DataHeatBalFanSys::ZTAV;
		using InputProcessor::FindItem;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//unused    INTEGER    :: thisZoneInfo

		//intialize in preperation for calculations
		AirPatternZoneInfo( ZoneNum ).Tstat = MAT( ZoneNum );
		AirPatternZoneInfo( ZoneNum ).Tleaving = MAT( ZoneNum );
		AirPatternZoneInfo( ZoneNum ).Texhaust = MAT( ZoneNum );
		for ( auto & e : AirPatternZoneInfo( ZoneNum ).Surf ) e.TadjacentAir = MAT( ZoneNum );

		// the only input this method needs is the zone MAT or ZT or ZTAV  ?  (original was ZT)
		AirPatternZoneInfo( ZoneNum ).TairMean = MAT( ZoneNum ); // this is lagged from previous corrector result

	}

	//*****************************************************************************************

	void
	CalcTempDistModel( int const ZoneNum ) // index number for the specified zone
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   August 2005
		//       MODIFIED
		//       RE-ENGINEERED

		// PURPOSE OF THIS SUBROUTINE:
		// figure out which pattern is scheduled and call
		// appropriate subroutine

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataSurfaces::ZoneMeanAirTemp;
		using ScheduleManager::GetCurrentScheduleValue;
		using InputProcessor::FindItem;
		using OutputReportTabular::IntToStr;
		using General::FindNumberInList;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//unused    INTEGER    :: thisZoneInfo
		Real64 AvailTest;
		int CurntPatternKey;
		int CurPatrnID;
		//unused    INTEGER    :: thisZoneInfoSurf
		//unused    INTEGER    :: lowSideID
		//unused    INTEGER    :: highSideID
		//unused    REAL(r64)  :: thisZeta
		//unused    REAL(r64)  :: lowSideZeta
		//unused    REAL(r64)  :: hiSideZeta
		//unused    REAL(r64)  :: fractBtwn
		//unused    REAL(r64)  :: tmpDeltaTai

		//first determine availability
		AvailTest = GetCurrentScheduleValue( AirPatternZoneInfo( ZoneNum ).AvailSchedID );

		if ( ( AvailTest != 1.0 ) || ( ! AirPatternZoneInfo( ZoneNum ).IsUsed ) ) {
			// model not to be used. Use complete mixing method

			AirPatternZoneInfo( ZoneNum ).Tstat = AirPatternZoneInfo( ZoneNum ).TairMean;
			AirPatternZoneInfo( ZoneNum ).Tleaving = AirPatternZoneInfo( ZoneNum ).TairMean;
			AirPatternZoneInfo( ZoneNum ).Texhaust = AirPatternZoneInfo( ZoneNum ).TairMean;
			for ( auto & e : AirPatternZoneInfo( ZoneNum ).Surf ) e.TadjacentAir = AirPatternZoneInfo( ZoneNum ).TairMean;

			return;

		} else { // choose pattern and call subroutine

			CurntPatternKey = GetCurrentScheduleValue( AirPatternZoneInfo( ZoneNum ).PatternSchedID );

			CurPatrnID = FindNumberInList( CurntPatternKey, RoomAirPattern, &TemperaturePatternStruct::PatrnID );

			if ( CurPatrnID == 0 ) {
				// throw error here ? way to test schedules before getting to this point?
				ShowFatalError( "User defined room air pattern index not found: " + IntToStr( CurntPatternKey ) );
				return;
			}

			{ auto const SELECT_CASE_var( RoomAirPattern( CurPatrnID ).PatternMode );

			if ( SELECT_CASE_var == ConstGradTempPattern ) {

				FigureConstGradPattern( CurPatrnID, ZoneNum );

			} else if ( SELECT_CASE_var == TwoGradInterpPattern ) {

				FigureTwoGradInterpPattern( CurPatrnID, ZoneNum );

			} else if ( SELECT_CASE_var == NonDimenHeightPattern ) {

				FigureHeightPattern( CurPatrnID, ZoneNum );

			} else if ( SELECT_CASE_var == SurfMapTempPattern ) {

				FigureSurfMapPattern( CurPatrnID, ZoneNum );

			} else {
				//should not come here

			}}

		} // availability control construct

	}

	void
	FigureSurfMapPattern(
		int const PattrnID,
		int const ZoneNum
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B Griffith
		//       DATE WRITTEN   August 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// main calculation routine for surface pattern

		// METHODOLOGY EMPLOYED:
		// simple polling and applying prescribed
		// delta Tai's to current mean air temp
		// on a surface by surface basis

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::FindNumberInList;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 Tmean;
		int found;
		int i;

		Tmean = AirPatternZoneInfo( ZoneNum ).TairMean;

		for ( i = 1; i <= AirPatternZoneInfo( ZoneNum ).totNumSurfs; ++i ) {
			// cycle through zone surfaces and look for match
			found = FindNumberInList( AirPatternZoneInfo( ZoneNum ).Surf( i ).SurfID, RoomAirPattern( PattrnID ).MapPatrn.SurfID, RoomAirPattern( PattrnID ).MapPatrn.NumSurfs );
			if ( found != 0 ) { // if surf is in map then assign, else give it MAT
				AirPatternZoneInfo( ZoneNum ).Surf( i ).TadjacentAir = RoomAirPattern( PattrnID ).MapPatrn.DeltaTai( found ) + Tmean;
			} else {
				AirPatternZoneInfo( ZoneNum ).Surf( i ).TadjacentAir = Tmean;
			}
		}

		AirPatternZoneInfo( ZoneNum ).Tstat = RoomAirPattern( PattrnID ).DeltaTstat + Tmean;
		AirPatternZoneInfo( ZoneNum ).Tleaving = RoomAirPattern( PattrnID ).DeltaTleaving + Tmean;
		AirPatternZoneInfo( ZoneNum ).Texhaust = RoomAirPattern( PattrnID ).DeltaTexhaust + Tmean;

	}

	void
	FigureHeightPattern(
		int const PattrnID,
		int const ZoneNum
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B Griffith
		//       DATE WRITTEN   August 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// calculate the pattern for non-dimensional vertical profile

		// METHODOLOGY EMPLOYED:
		// treat profile as lookup table and interpolate

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na
		// Using/Aliasing
		using FluidProperties::FindArrayIndex;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 Tmean;
		int lowSideID;
		int highSideID;
		Real64 thisZeta;
		int i;
		Real64 lowSideZeta;
		Real64 hiSideZeta;
		Real64 fractBtwn;
		Real64 tmpDeltaTai;

		tmpDeltaTai = 0.0;
		Tmean = AirPatternZoneInfo( ZoneNum ).TairMean;

		for ( i = 1; i <= AirPatternZoneInfo( ZoneNum ).totNumSurfs; ++i ) {

			thisZeta = AirPatternZoneInfo( ZoneNum ).Surf( i ).Zeta;
			lowSideID = FindArrayIndex( thisZeta, RoomAirPattern( PattrnID ).VertPatrn.ZetaPatrn );
			highSideID = lowSideID + 1;
			if ( lowSideID == 0 ) lowSideID = 1; //protect against array bounds

			lowSideZeta = RoomAirPattern( PattrnID ).VertPatrn.ZetaPatrn( lowSideID );
			if ( highSideID <= isize( RoomAirPattern( PattrnID ).VertPatrn.ZetaPatrn ) ) {
				hiSideZeta = RoomAirPattern( PattrnID ).VertPatrn.ZetaPatrn( highSideID );
			} else { //trap array bounds
				hiSideZeta = lowSideZeta;
			}
			if ( ( hiSideZeta - lowSideZeta ) != 0.0 ) {
				fractBtwn = ( thisZeta - lowSideZeta ) / ( hiSideZeta - lowSideZeta );
				tmpDeltaTai = RoomAirPattern( PattrnID ).VertPatrn.DeltaTaiPatrn( lowSideID ) + fractBtwn * ( RoomAirPattern( PattrnID ).VertPatrn.DeltaTaiPatrn( highSideID ) - RoomAirPattern( PattrnID ).VertPatrn.DeltaTaiPatrn( lowSideID ) );

			} else { // would divide by zero, using low side value

				tmpDeltaTai = RoomAirPattern( PattrnID ).VertPatrn.DeltaTaiPatrn( lowSideID );

			}

			AirPatternZoneInfo( ZoneNum ).Surf( i ).TadjacentAir = tmpDeltaTai + Tmean;

		} //surfaces in this zone

		AirPatternZoneInfo( ZoneNum ).Tstat = RoomAirPattern( PattrnID ).DeltaTstat + Tmean;
		AirPatternZoneInfo( ZoneNum ).Tleaving = RoomAirPattern( PattrnID ).DeltaTleaving + Tmean;
		AirPatternZoneInfo( ZoneNum ).Texhaust = RoomAirPattern( PattrnID ).DeltaTexhaust + Tmean;

	}

	void
	FigureTwoGradInterpPattern(
		int const PattrnID,
		int const ZoneNum
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B Griffith
		//       DATE WRITTEN   Aug 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// calculate two gradient interpolation pattern

		// METHODOLOGY EMPLOYED:
		// Case statement controls how interpolations are done
		// based on user selected mode.
		// calculations vary by mode

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na
		// Using/Aliasing
		using DataHeatBalance::Zone;
		using DataHeatBalance::SNLoadCoolRate;
		using DataHeatBalance::SNLoadHeatRate;
		using DataGlobals::NumOfZones;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 Tmean; // MAT deg C
		Real64 Grad; // vertical temperature gradient C/m
		Real64 DeltaT; // temperature difference
		Real64 CoolLoad; // sensible cooling load
		Real64 HeatLoad; // sensible heating load
		Real64 ZetaTmean; // non-dimensional height for mean air temp
		int i; // do loop index
		Real64 thisZeta; // non-dimensional height
		Real64 DeltaHeight; // height difference in m
		Real64 tempDeltaTai; // temporary temperature difference
		static Array1D_bool SetupOutputFlag; // flag to set up output variable one-time if 2-grad model used
		static bool MyOneTimeFlag( true );

		if ( MyOneTimeFlag ) {
			SetupOutputFlag.dimension( NumOfZones, true ); // init
			MyOneTimeFlag = false;
		}

		if ( SetupOutputFlag( ZoneNum ) ) {
			SetupOutputVariable( "Room Air Zone Vertical Temperature Gradient [K/m]", AirPatternZoneInfo( ZoneNum ).Gradient, "HVAC", "State", AirPatternZoneInfo( ZoneNum ).ZoneName );

			SetupOutputFlag( ZoneNum ) = false;
		}

		Tmean = AirPatternZoneInfo( ZoneNum ).TairMean;

		//determine gradient depending on mode
		{ auto const SELECT_CASE_var( RoomAirPattern( PattrnID ).TwoGradPatrn.InterpolationMode );

		if ( SELECT_CASE_var == OutdoorDryBulbMode ) {

			Grad = OutdoorDryBulbGrad(Zone(ZoneNum).OutDryBulbTemp, RoomAirPattern(PattrnID).TwoGradPatrn.UpperBoundTempScale, RoomAirPattern(PattrnID).TwoGradPatrn.HiGradient, RoomAirPattern(PattrnID).TwoGradPatrn.LowerBoundTempScale, RoomAirPattern(PattrnID).TwoGradPatrn.LowGradient);

		} else if ( SELECT_CASE_var == ZoneAirTempMode ) {

			if ( Tmean >= RoomAirPattern( PattrnID ).TwoGradPatrn.UpperBoundTempScale ) {
				Grad = RoomAirPattern( PattrnID ).TwoGradPatrn.HiGradient;

			} else if ( Tmean <= RoomAirPattern( PattrnID ).TwoGradPatrn.LowerBoundTempScale ) {

				Grad = RoomAirPattern( PattrnID ).TwoGradPatrn.LowGradient;
			} else { // interpolate
				if ( ( RoomAirPattern( PattrnID ).TwoGradPatrn.UpperBoundTempScale - RoomAirPattern( PattrnID ).TwoGradPatrn.LowerBoundTempScale ) == 0.0 ) {
					// bad user input, trapped during get input
					Grad = RoomAirPattern( PattrnID ).TwoGradPatrn.LowGradient;
				} else {

					Grad = RoomAirPattern( PattrnID ).TwoGradPatrn.LowGradient + ( ( Tmean - RoomAirPattern( PattrnID ).TwoGradPatrn.LowerBoundTempScale ) / ( RoomAirPattern( PattrnID ).TwoGradPatrn.UpperBoundTempScale - RoomAirPattern( PattrnID ).TwoGradPatrn.LowerBoundTempScale ) ) * ( RoomAirPattern( PattrnID ).TwoGradPatrn.HiGradient - RoomAirPattern( PattrnID ).TwoGradPatrn.LowGradient );

				}
			}

		} else if ( SELECT_CASE_var == DeltaOutdoorZone ) {
			DeltaT = Zone( ZoneNum ).OutDryBulbTemp - Tmean;
			if ( DeltaT >= RoomAirPattern( PattrnID ).TwoGradPatrn.UpperBoundTempScale ) {
				Grad = RoomAirPattern( PattrnID ).TwoGradPatrn.HiGradient;

			} else if ( DeltaT <= RoomAirPattern( PattrnID ).TwoGradPatrn.LowerBoundTempScale ) {

				Grad = RoomAirPattern( PattrnID ).TwoGradPatrn.LowGradient;
			} else { // interpolate
				if ( ( RoomAirPattern( PattrnID ).TwoGradPatrn.UpperBoundTempScale - RoomAirPattern( PattrnID ).TwoGradPatrn.LowerBoundTempScale ) == 0.0 ) {
					// bad user input, trapped during get input
					Grad = RoomAirPattern( PattrnID ).TwoGradPatrn.LowGradient;
				} else {

					Grad = RoomAirPattern( PattrnID ).TwoGradPatrn.LowGradient + ( ( DeltaT - RoomAirPattern( PattrnID ).TwoGradPatrn.LowerBoundTempScale ) / ( RoomAirPattern( PattrnID ).TwoGradPatrn.UpperBoundTempScale - RoomAirPattern( PattrnID ).TwoGradPatrn.LowerBoundTempScale ) ) * ( RoomAirPattern( PattrnID ).TwoGradPatrn.HiGradient - RoomAirPattern( PattrnID ).TwoGradPatrn.LowGradient );
				}
			}

		} else if ( SELECT_CASE_var == SensibleCoolingMode ) {

			CoolLoad = SNLoadCoolRate( ZoneNum );
			if ( CoolLoad >= RoomAirPattern( PattrnID ).TwoGradPatrn.UpperBoundHeatRateScale ) {
				Grad = RoomAirPattern( PattrnID ).TwoGradPatrn.HiGradient;

			} else if ( CoolLoad <= RoomAirPattern( PattrnID ).TwoGradPatrn.LowerBoundHeatRateScale ) {

				Grad = RoomAirPattern( PattrnID ).TwoGradPatrn.LowGradient;
			} else { // interpolate
				if ( ( RoomAirPattern( PattrnID ).TwoGradPatrn.UpperBoundHeatRateScale - RoomAirPattern( PattrnID ).TwoGradPatrn.LowerBoundHeatRateScale ) == 0.0 ) {
					Grad = RoomAirPattern( PattrnID ).TwoGradPatrn.LowGradient;
				} else {

					Grad = RoomAirPattern( PattrnID ).TwoGradPatrn.LowGradient + ( ( CoolLoad - RoomAirPattern( PattrnID ).TwoGradPatrn.LowerBoundHeatRateScale ) / ( RoomAirPattern( PattrnID ).TwoGradPatrn.UpperBoundHeatRateScale - RoomAirPattern( PattrnID ).TwoGradPatrn.LowerBoundHeatRateScale ) ) * ( RoomAirPattern( PattrnID ).TwoGradPatrn.HiGradient - RoomAirPattern( PattrnID ).TwoGradPatrn.LowGradient );
				}
			}

		} else if ( SELECT_CASE_var == SensibleHeatingMode ) {

			HeatLoad = SNLoadHeatRate( ZoneNum );
			if ( HeatLoad >= RoomAirPattern( PattrnID ).TwoGradPatrn.UpperBoundHeatRateScale ) {
				Grad = RoomAirPattern( PattrnID ).TwoGradPatrn.HiGradient;

			} else if ( HeatLoad <= RoomAirPattern( PattrnID ).TwoGradPatrn.LowerBoundHeatRateScale ) {

				Grad = RoomAirPattern( PattrnID ).TwoGradPatrn.LowGradient;
			} else { // interpolate
				if ( ( RoomAirPattern( PattrnID ).TwoGradPatrn.UpperBoundHeatRateScale - RoomAirPattern( PattrnID ).TwoGradPatrn.LowerBoundHeatRateScale ) == 0.0 ) {
					Grad = RoomAirPattern( PattrnID ).TwoGradPatrn.LowGradient;
				} else {

					Grad = RoomAirPattern( PattrnID ).TwoGradPatrn.LowGradient + ( ( HeatLoad - RoomAirPattern( PattrnID ).TwoGradPatrn.LowerBoundHeatRateScale ) / ( RoomAirPattern( PattrnID ).TwoGradPatrn.UpperBoundHeatRateScale - RoomAirPattern( PattrnID ).TwoGradPatrn.LowerBoundHeatRateScale ) ) * ( RoomAirPattern( PattrnID ).TwoGradPatrn.HiGradient - RoomAirPattern( PattrnID ).TwoGradPatrn.LowGradient );
				}
			}

		}}

		ZetaTmean = 0.5; // by definition,

		for ( i = 1; i <= AirPatternZoneInfo( ZoneNum ).totNumSurfs; ++i ) {
			thisZeta = AirPatternZoneInfo( ZoneNum ).Surf( i ).Zeta;

			DeltaHeight = -1.0 * ( ZetaTmean - thisZeta ) * AirPatternZoneInfo( ZoneNum ).ZoneHeight;

			tempDeltaTai = DeltaHeight * Grad;

			AirPatternZoneInfo( ZoneNum ).Surf( i ).TadjacentAir = tempDeltaTai + Tmean;

		}

		AirPatternZoneInfo( ZoneNum ).Tstat = -1.0 * ( 0.5 * AirPatternZoneInfo( ZoneNum ).ZoneHeight - RoomAirPattern( PattrnID ).TwoGradPatrn.TstatHeight ) * Grad + Tmean;
		AirPatternZoneInfo( ZoneNum ).Tleaving = -1.0 * ( 0.5 * AirPatternZoneInfo( ZoneNum ).ZoneHeight - RoomAirPattern( PattrnID ).TwoGradPatrn.TleavingHeight ) * Grad + Tmean;
		AirPatternZoneInfo( ZoneNum ).Texhaust = -1.0 * ( 0.5 * AirPatternZoneInfo( ZoneNum ).ZoneHeight - RoomAirPattern( PattrnID ).TwoGradPatrn.TexhaustHeight ) * Grad + Tmean;

		AirPatternZoneInfo( ZoneNum ).Gradient = Grad;

	}
	Real64
	OutdoorDryBulbGrad(
		Real64 DryBulbTemp, // Zone(ZoneNum).OutDryBulbTemp
		Real64 UpperBound, // RoomAirPattern(PattrnID).TwoGradPatrn.UpperBoundTempScale
		Real64 HiGradient, // RoomAirPattern(PattrnID).TwoGradPatrn.HiGradient
		Real64 LowerBound, // RoomAirPattern(PattrnID).TwoGradPatrn.LowerBoundTempScale
		Real64 LowGradient // RoomAirPattern(PattrnID).TwoGradPatrn.LowGradient
	)
	{
		Real64 Grad;
		if (DryBulbTemp >= UpperBound) {
			Grad = HiGradient;

		}
		else if (DryBulbTemp <= LowerBound) {

			Grad = LowGradient;
		}
		else { // interpolate

			if ((UpperBound - LowerBound) == 0.0) {
				// bad user input. should be trapped during get input in RoomAirManager.cc
				Grad = LowGradient;
			}
			else {

				Grad = LowGradient + ((DryBulbTemp - LowerBound) / (UpperBound -LowerBound)) * (HiGradient - LowGradient);

			}
		}
		return Grad;
	}

	void
	FigureConstGradPattern(
		int const PattrnID,
		int const ZoneNum
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   August 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

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
		Real64 Tmean; // MAT
		int i; // loop counter
		Real64 Grad; // vertical temperature gradient
		Real64 ZetaTmean; // non-dimens. height for MAT, 0.5
		Real64 thisZeta; // temporary non-dimens height
		Real64 DeltaHeight; // temporary height difference
		Real64 tempDeltaTai; // temporary Delta Tai

		Tmean = AirPatternZoneInfo( ZoneNum ).TairMean;
		Grad = RoomAirPattern( PattrnID ).GradPatrn.Gradient;

		ZetaTmean = 0.5; // by definition,

		for ( i = 1; i <= AirPatternZoneInfo( ZoneNum ).totNumSurfs; ++i ) {
			thisZeta = AirPatternZoneInfo( ZoneNum ).Surf( i ).Zeta;
			DeltaHeight = -1.0 * ( ZetaTmean - thisZeta ) * AirPatternZoneInfo( ZoneNum ).ZoneHeight;
			tempDeltaTai = DeltaHeight * Grad;
			AirPatternZoneInfo( ZoneNum ).Surf( i ).TadjacentAir = tempDeltaTai + Tmean;
		}

		AirPatternZoneInfo( ZoneNum ).Tstat = RoomAirPattern( PattrnID ).DeltaTstat + Tmean;
		AirPatternZoneInfo( ZoneNum ).Tleaving = RoomAirPattern( PattrnID ).DeltaTleaving + Tmean;
		AirPatternZoneInfo( ZoneNum ).Texhaust = RoomAirPattern( PattrnID ).DeltaTexhaust + Tmean;

	}

	//*****************************************************************************************

	Real64
	FigureNDheightInZone( int const thisHBsurf ) // index in main Surface array
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         B.Griffith
		//       DATE WRITTEN   aug 2005, Jan2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// return a non-dimensional height zeta

		// METHODOLOGY EMPLOYED:
		// figure average floor height (follows code in surfacegeometry.cc
		// use ceiling height from Zone structure
		// non dimensionalize surface's centroid's Z value

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na
		// Using/Aliasing
		using DataSurfaces::Surface;
		using DataSurfaces::SurfaceClass_Floor;
		using DataSurfaces::SurfaceClass_Wall;
		using DataHeatBalance::Zone;
		using DataVectorTypes::Vector;
		using General::RoundSigDigits;
		using DataErrorTracking::TotalRoomAirPatternTooLow;
		using DataErrorTracking::TotalRoomAirPatternTooHigh;

		// Return value
		Real64 FigureNDheightInZone;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		Real64 const TolValue( 0.0001 );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int thisZone;
		Real64 ZoneZorig;
		Real64 ZoneCeilHeight;
		Real64 Zcm;
		Real64 SurfMinZ;
		Real64 SurfMaxZ;
		Real64 Zeta;
		Real64 FloorCount;
		Real64 ZFlrAvg;
		Real64 ZMax;
		Real64 ZMin;
		int Count;
		int SurfNum;
		Real64 Z1;
		Real64 Z2;

		// Get the centroid height for the surface
		Zcm = Surface( thisHBsurf ).Centroid.z;
		thisZone = Surface( thisHBsurf ).Zone;

		//this next Do block is copied from SurfaceGeometry.cc with modification for just floor Z
		// used find floor z.
		FloorCount = 0.0;
		ZFlrAvg = 0.0;
		ZMax = 0.0;
		ZMin = 0.0;
		Count = 0;
		for ( SurfNum = Zone( thisZone ).SurfaceFirst; SurfNum <= Zone( thisZone ).SurfaceLast; ++SurfNum ) {
			if ( Surface( SurfNum ).Class == SurfaceClass_Floor ) {
				// Use Average Z for surface, more important for roofs than floors...
				++FloorCount;
				Z1 = minval( Surface( SurfNum ).Vertex( {1,Surface( SurfNum ).Sides} ), &Vector::z );
				Z2 = maxval( Surface( SurfNum ).Vertex( {1,Surface( SurfNum ).Sides} ), &Vector::z );
				ZFlrAvg += ( Z1 + Z2 ) / 2.0;
			}
			if ( Surface( SurfNum ).Class == SurfaceClass_Wall ) {
				// Use Wall calculation in case no floor in zone
				++Count;
				if ( Count == 1 ) {
					ZMax = Surface( SurfNum ).Vertex( 1 ).z;
					ZMin = ZMax;
				}
				ZMax = max( ZMax, maxval( Surface( SurfNum ).Vertex( {1,Surface( SurfNum ).Sides} ), &Vector::z ) );
				ZMin = min( ZMin, minval( Surface( SurfNum ).Vertex( {1,Surface( SurfNum ).Sides} ), &Vector::z ) );
			}
		}
		if ( FloorCount > 0.0 ) {
			ZFlrAvg /= FloorCount;
		} else {
			ZFlrAvg = ZMin;
		}
		ZoneZorig = ZFlrAvg; // Z floor  [M]
		ZoneCeilHeight = Zone( thisZone ).CeilingHeight;

		// first check if some basic things are reasonable

		SurfMinZ = minval( Surface( thisHBsurf ).Vertex, &Vector::z );
		SurfMaxZ = maxval( Surface( thisHBsurf ).Vertex, &Vector::z );

		if ( SurfMinZ < ( ZoneZorig - TolValue ) ) {
			if ( DisplayExtraWarnings ) {
				ShowWarningError( "RoomAirModelUserTempPattern: Problem in non-dimensional height calculation" );
				ShowContinueError( "too low surface: " + Surface( thisHBsurf ).Name + " in zone: " + Zone( thisZone ).Name );
				ShowContinueError( "**** Average floor height of zone is: " + RoundSigDigits( ZoneZorig, 3 ) );
				ShowContinueError( "**** Surface minimum height is: " + RoundSigDigits( SurfMinZ, 3 ) );
			} else {
				++TotalRoomAirPatternTooLow;
			}
		}

		if ( SurfMaxZ > ( ZoneZorig + ZoneCeilHeight + TolValue ) ) {
			if ( DisplayExtraWarnings ) {
				ShowWarningError( "RoomAirModelUserTempPattern: Problem in non-dimensional height calculation" );
				ShowContinueError( " too high surface: " + Surface( thisHBsurf ).Name + " in zone: " + Zone( thisZone ).Name );
				ShowContinueError( "**** Average Ceiling height of zone is: " + RoundSigDigits( ( ZoneZorig + ZoneCeilHeight ), 3 ) );
				ShowContinueError( "**** Surface Maximum height is: " + RoundSigDigits( SurfMaxZ, 3 ) );
			} else {
				++TotalRoomAirPatternTooHigh;
			}
		}

		//non dimensionalize.
		Zeta = ( Zcm - ZoneZorig ) / ZoneCeilHeight;
		// bound so that floors and ceiling are just in from endpoints.

		if ( Zeta > 0.99 ) Zeta = 0.99;

		if ( Zeta < 0.01 ) Zeta = 0.01;

		FigureNDheightInZone = Zeta;

		return FigureNDheightInZone;
	}

	//***************************************************

	void
	SetSurfHBDataForTempDistModel( int const ZoneNum ) // index number for the specified zone
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   August 2005,Feb. 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//  map data from air domain back to surface domain for each zone
		//  collects code couples to remote data structures

		// METHODOLOGY EMPLOYED:
		// sets values in Heat balance variables

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Using/Aliasing
		using DataEnvironment::OutBaroPress;
		using DataLoopNode::Node;
		using DataSurfaces::Surface;
		using DataSurfaces::AdjacentAirTemp;
		using DataSurfaces::ZoneMeanAirTemp;
		using DataSurfaces::SurfaceWindow;
		using DataSurfaces::AirFlowWindow_Destination_ReturnAir;
		using DataHeatBalance::Zone;
		using DataHeatBalance::TempEffBulkAir;
		using DataHeatBalance::RefrigCaseCredit;
		using DataHeatBalFanSys::MAT;
		using DataHeatBalFanSys::ZT;
		using DataHeatBalFanSys::TempZoneThermostatSetPoint;
		using DataHeatBalFanSys::TempTstatAir;
		using DataHeatBalFanSys::SysDepZoneLoads;
		using DataHeatBalFanSys::ZoneLatentGain;
		using InputProcessor::FindItem;
		using Psychrometrics::PsyHFnTdbW;
		using Psychrometrics::PsyCpAirFnWTdb;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using Psychrometrics::PsyHgAirFnWTdb;
		using InternalHeatGains::SumAllReturnAirConvectionGains;
		using InternalHeatGains::SumAllReturnAirLatentGains;
		using DataHVACGlobals::RetTempMax;
		using DataHVACGlobals::RetTempMin;
		using DataGlobals::ZoneSizingCalc;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int SurfFirst; // index number of the first surface in the zone
		int SurfLast;
		Real64 QRetAir; // Heat to return air from lights
		Real64 CpAir; // Air heat capacity [J/kg-K]
		Real64 TempRetAir; // Return air temperature [C]
		Real64 TempZoneAir; // Zone air temperature [C]
		int ReturnNode; // Node number of controlled zone's return air
		int ZoneNode; // Node number of controlled zone
		int SurfNum; // Surface number
		Real64 MassFlowRA; // Return air mass flow [kg/s]
		Real64 FlowThisTS; // Window gap air mass flow [kg/s]
		Real64 WinGapFlowToRA; // Mass flow to return air from all airflow windows in zone [kg/s]
		Real64 WinGapFlowTtoRA; // Sum of mass flow times outlet temp for all airflow windows in zone [(kg/s)-C]
		Real64 WinGapTtoRA; // Temp of outlet flow mixture to return air from all airflow windows in zone [C]
		Real64 H2OHtOfVap; // Heat of vaporization of water (W/kg)
		Real64 RhoAir; // Density of air (Kg/m3)
		Real64 ZoneMult;
		Real64 SumRetAirLatentGainRate;

		// FLOW:

		SurfFirst = Zone( ZoneNum ).SurfaceFirst;
		SurfLast = Zone( ZoneNum ).SurfaceLast;

		// set air system leaving node conditions
		// this is not so easy.  THis task is normally done in CalcZoneLeavingConditions
		//  but efforts to do this update there were not succesful.
		//  Need to revisit how to best implement this. Ended up taking code from CalcZoneLeavingConditions
		//  ZoneNum is already equal to ActualZoneNum , changed block of source

		if ( AirPatternZoneInfo( ZoneNum ).ZoneNodeID != 0 ) {
			// the zone system node should get the conditions leaving the zone (but before return air heat gains are added).
			Node( AirPatternZoneInfo( ZoneNum ).ZoneNodeID ).Temp = AirPatternZoneInfo( ZoneNum ).Tleaving;
		}

		if ( AirPatternZoneInfo( ZoneNum ).ReturnAirNodeID != 0 ) {
			//BEGIN BLOCK of code from CalcZoneLeavingConditions*********************************
			ReturnNode = AirPatternZoneInfo( ZoneNum ).ReturnAirNodeID;
			ZoneNode = AirPatternZoneInfo( ZoneNum ).ZoneNodeID;
			ZoneMult = Zone( ZoneNum ).Multiplier * Zone( ZoneNum ).ListMultiplier;
			//RETURN AIR HEAT GAIN from the Lights statement; this heat gain is stored in
			// Add sensible heat gain from refrigerated cases with under case returns
			SumAllReturnAirConvectionGains( ZoneNum, QRetAir );

			CpAir = PsyCpAirFnWTdb( Node( ZoneNode ).HumRat, Node( ZoneNode ).Temp );

			// Need to add the energy to the return air from lights and from airflow windows. Where the heat
			// is added depends on if there is system flow or not.  If there is system flow the heat is added
			// to the Zone Return Node.  If there is no system flow then the heat is added back to the zone in the
			// Correct step through the SysDepZoneLoads variable.

			MassFlowRA = Node( ReturnNode ).MassFlowRate / ZoneMult;
			TempZoneAir = AirPatternZoneInfo( ZoneNum ).Tleaving; // key difference from
			TempRetAir = TempZoneAir;
			WinGapFlowToRA = 0.0;
			WinGapTtoRA = 0.0;
			WinGapFlowTtoRA = 0.0;

			for ( SurfNum = Zone( ZoneNum ).SurfaceFirst; SurfNum <= Zone( ZoneNum ).SurfaceLast; ++SurfNum ) {
				if ( SurfaceWindow( SurfNum ).AirflowThisTS > 0.0 && SurfaceWindow( SurfNum ).AirflowDestination == AirFlowWindow_Destination_ReturnAir ) {
					FlowThisTS = PsyRhoAirFnPbTdbW( OutBaroPress, SurfaceWindow( SurfNum ).TAirflowGapOutlet, Node( ZoneNode ).HumRat ) * SurfaceWindow( SurfNum ).AirflowThisTS * Surface( SurfNum ).Width;
					WinGapFlowToRA += FlowThisTS;
					WinGapFlowTtoRA += FlowThisTS * SurfaceWindow( SurfNum ).TAirflowGapOutlet;
				}
			}
			if ( WinGapFlowToRA > 0.0 ) WinGapTtoRA = WinGapFlowTtoRA / WinGapFlowToRA;

			if ( ! Zone( ZoneNum ).NoHeatToReturnAir ) {
				if ( MassFlowRA > 0.0 ) {
					if ( WinGapFlowToRA > 0.0 ) {
						// Add heat-to-return from window gap airflow
						if ( MassFlowRA >= WinGapFlowToRA ) {
							TempRetAir = ( WinGapFlowTtoRA + ( MassFlowRA - WinGapFlowToRA ) * TempZoneAir ) / MassFlowRA;
						} else {
							// All of return air comes from flow through airflow windows
							TempRetAir = WinGapTtoRA;
							// Put heat from window airflow that exceeds return air flow into zone air
							SysDepZoneLoads( ZoneNum ) += ( WinGapFlowToRA - MassFlowRA ) * CpAir * ( WinGapTtoRA - TempZoneAir );
						}
					}
					// Add heat-to-return from lights
					TempRetAir += QRetAir / ( MassFlowRA * CpAir );
					if ( TempRetAir > RetTempMax ) {
						Node( ReturnNode ).Temp = RetTempMax;
						if ( ! ZoneSizingCalc ) {
							SysDepZoneLoads( ZoneNum ) += CpAir * MassFlowRA * ( TempRetAir - RetTempMax );
						}
					} else if ( TempRetAir < RetTempMin ) {
						Node( ReturnNode ).Temp = RetTempMin;
						if ( ! ZoneSizingCalc ) {
							SysDepZoneLoads( ZoneNum ) += CpAir * MassFlowRA * ( TempRetAir - RetTempMin );
						}
					} else {
						Node( ReturnNode ).Temp = TempRetAir;
					}
				} else { // No return air flow
					// Assign all heat-to-return from window gap airflow to zone air
					if ( WinGapFlowToRA > 0.0 ) SysDepZoneLoads( ZoneNum ) += WinGapFlowToRA * CpAir * ( WinGapTtoRA - TempZoneAir );
					// Assign all heat-to-return from lights to zone air
					if ( QRetAir > 0.0 ) SysDepZoneLoads( ZoneNum ) += QRetAir;
					Node( ReturnNode ).Temp = Node( ZoneNode ).Temp;
				}
			} else {
				Node( ReturnNode ).Temp = Node( ZoneNode ).Temp;
			}

			// Update the rest of the Return Air Node conditions, if the return air system exists!
			Node( ReturnNode ).Press = Node( ZoneNode ).Press;

			H2OHtOfVap = PsyHgAirFnWTdb( Node( ZoneNode ).HumRat, Node( ReturnNode ).Temp );
			RhoAir = PsyRhoAirFnPbTdbW( OutBaroPress, Node( ReturnNode ).Temp, Node( ZoneNode ).HumRat );

			// Include impact of under case returns for refrigerated display cases when updateing return node
			// humidity ratio
			if ( ! Zone( ZoneNum ).NoHeatToReturnAir ) {
				if ( MassFlowRA > 0 ) {
					SumAllReturnAirLatentGains( ZoneNum, SumRetAirLatentGainRate );
					Node( ReturnNode ).HumRat = Node( ZoneNode ).HumRat + ( SumRetAirLatentGainRate / ( H2OHtOfVap * MassFlowRA ) );
				} else {
					// If no mass flow rate exists, include the latent HVAC case credit with the latent Zone case credit
					Node( ReturnNode ).HumRat = Node( ZoneNode ).HumRat;
					RefrigCaseCredit( ZoneNum ).LatCaseCreditToZone += RefrigCaseCredit( ZoneNum ).LatCaseCreditToHVAC;
					// shouldn't the HVAC term be zeroed out then?
					SumAllReturnAirLatentGains( ZoneNum, SumRetAirLatentGainRate );
					ZoneLatentGain( ZoneNum ) += SumRetAirLatentGainRate;
				}
			} else {
				Node( ReturnNode ).HumRat = Node( ZoneNode ).HumRat;
				RefrigCaseCredit( ZoneNum ).LatCaseCreditToZone += RefrigCaseCredit( ZoneNum ).LatCaseCreditToHVAC;
				// shouldn't the HVAC term be zeroed out then?
				SumAllReturnAirLatentGains( ZoneNum, SumRetAirLatentGainRate );
				ZoneLatentGain( ZoneNum ) += SumRetAirLatentGainRate;
			}

			Node( ReturnNode ).Enthalpy = PsyHFnTdbW( Node( ReturnNode ).Temp, Node( ReturnNode ).HumRat );

			//END BLOCK of code from CalcZoneLeavingConditions*********************************

		}

		// set exhaust node leaving temp if present
		if ( allocated( AirPatternZoneInfo( ZoneNum ).ExhaustAirNodeID ) ) {
			auto const & APZoneInfo( AirPatternZoneInfo( ZoneNum ) );
			auto const & EANodeID( APZoneInfo.ExhaustAirNodeID );
			Real64 const Texhaust( APZoneInfo.Texhaust );
			for ( int i = 1, ie = EANodeID.u(); i <= ie; ++i ) {
				Node( EANodeID( i ) ).Temp = Texhaust;
			}
		}

		// set thermostat reading for air system .
		TempTstatAir( ZoneNum ) = AirPatternZoneInfo( ZoneNum ).Tstat;

		// set results for all surface
		for ( int i = SurfFirst, j = 1; i <= SurfLast; ++i, ++j ) {
			TempEffBulkAir( i ) = AirPatternZoneInfo( ZoneNum ).Surf( j ).TadjacentAir;
		}

		// set flag for reference air temperature mode
		for ( int i = SurfFirst; i <= SurfLast; ++i ) {
			Surface( i ).TAirRef = AdjacentAirTemp;
		}

	}

	//*****************************************************************************************

} // RoomAirModelUserTempPattern

} // EnergyPlus
