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
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/member.functions.hh>

// EnergyPlus Headers
#include <DisplacementVentMgr.hh>
#include <ConvectionCoefficients.hh>
#include <DataAirflowNetwork.hh>
#include <DataEnvironment.hh>
#include <DataGlobals.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHeatBalSurface.hh>
#include <DataHVACGlobals.hh>
#include <DataLoopNode.hh>
#include <DataPrecisionGlobals.hh>
#include <DataRoomAirModel.hh>
#include <DataSurfaces.hh>
#include <DataUCSDSharedData.hh>
#include <DataZoneEquipment.hh>
#include <InputProcessor.hh>
#include <InternalHeatGains.hh>
#include <Psychrometrics.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace DisplacementVentMgr {

	// MODULE INFORMATION:
	//       AUTHOR         G. Carrilho da Graca
	//       DATE WRITTEN   February 2004
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// Routines that implement the UCSD Displacement Ventilation

	// METHODOLOGY EMPLOYED:

	// REFERENCES:

	// OTHER NOTES: none

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataGlobals;
	using namespace DataLoopNode;
	using namespace DataEnvironment;
	using namespace DataHeatBalance;
	using namespace DataHeatBalSurface;
	using namespace DataSurfaces;
	using namespace DataRoomAirModel;
	using ConvectionCoefficients::CalcDetailedHcInForDVModel;
	using DataHVACGlobals::SysTimeElapsed;
	using DataHVACGlobals::PreviousTimeStep;
	using DataHVACGlobals::ShortenTimeStepSysRoomAir;
	using namespace DataUCSDSharedData;
	using namespace DataAirflowNetwork;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	static std::string const BlankString;

	// DERIVED TYPE DEFINITIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:
	Real64 HAT_MX; // HAT_MX Convection Coefficient times Area times Temperature for the upper subzone
	Real64 HA_MX; // HA_MX Convection Coefficient times Area for the upper subzone
	Real64 HAT_OC; // HAT_OC Convection Coefficient times Area times Temperature for the lower subzone
	Real64 HA_OC; // HA_OC Convection Coefficient times Area for the lower subzone
	Real64 HAT_FLOOR; // HAT_FLOOR Convection Coefficient times Area times Temperature for the floor(?) subzone
	Real64 HA_FLOOR; // HA_FLOOR Convection Coefficient times Area for the floor(?) subzone
	Real64 HeightFloorSubzoneTop( 0.2 ); // Assumed thickness of floor subzone
	Real64 ThickOccupiedSubzoneMin( 0.2 ); // Minimum thickness of occupied subzone
	Real64 HeightIntMass( 0.0 ); // Height of internal mass surfaces, assumed vertical, cannot exceed ceiling height
	Real64 HeightIntMassDefault( 2.0 ); // Default height of internal mass surfaces

	// SUBROUTINE SPECIFICATIONS:

	// Functions

	void
	ManageUCSDDVModel( int const ZoneNum ) // index number for the specified zone
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         G. Carrilho da Graca
		//       DATE WRITTEN   February 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   manage the UCSD Displacement Ventilation model

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na
		// Using/Aliasing
		using DataHeatBalSurface::TempSurfIn;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// FLOW:

		// initialize Displacement Ventilation model
		InitUCSDDV( ZoneNum );

		// perform Displacement Ventilation model calculations
		CalcUCSDDV( ZoneNum );

	}

	//**************************************************************************************************

	void
	InitUCSDDV( int const ZoneNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         G. Carrilho da Graca
		//       DATE WRITTEN   February 2004
		//       MODIFIED       -
		//       RE-ENGINEERED  -

		// PURPOSE OF THIS SUBROUTINE:
		// Low Energy Cooling by Ventilation initialization subroutine.
		// All the data preparation needed to run the LECV models.
		// The subroutines sets up arrays with the locations in the main EnergyPlus surface array of
		// ceiling, windows, doors and walls. The zone maximum and minimum height is calculated.

		// -
		// METHODOLOGY EMPLOYED:
		// -
		// -
		// -
		// -

		// REFERENCES:
		// -
		// -

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool MyOneTimeFlag( true );
		static Array1D_bool MyEnvrnFlag;

		// Do the one time initializations
		if ( MyOneTimeFlag ) {
			MyEnvrnFlag.dimension( NumOfZones, true );
			HeightFloorSubzoneTop = 0.2;
			ThickOccupiedSubzoneMin = 0.2;
			HeightIntMassDefault = 2.0;
			MyOneTimeFlag = false;
		}

		// Do the begin environment initializations
		if ( BeginEnvrnFlag && MyEnvrnFlag( ZoneNum ) ) {
			HAT_MX = 0.0;
			HAT_OC = 0.0;
			HA_MX = 0.0;
			HA_OC = 0.0;
			HAT_FLOOR = 0.0;
			HA_FLOOR = 0.0;
			MyEnvrnFlag( ZoneNum ) = false;
		}

		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag( ZoneNum ) = true;
		}

		// initialize these module variables every timestep
		HeightIntMass = HeightIntMassDefault;

	}

	//**************************************************************************************************

	void
	HcUCSDDV(
		int const ZoneNum,
		Real64 const FractionHeight
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         G. Carrilho da Graca
		//       DATE WRITTEN   February 2004
		//       MODIFIED       -
		//       RE-ENGINEERED  -

		// PURPOSE OF THIS SUBROUTINE:
		// Main subroutine for convection calculation in the UCSD Displacement Ventilation model.
		// It calls CalcDetailedHcInForDVModel for convection coefficient
		// initial calculations and averages the final result comparing the position of the surface with
		// the interface subzone height.

		// METHODOLOGY EMPLOYED:
		// -
		// -
		// -
		// -

		// REFERENCES:
		// -
		// -

		// Using/Aliasing
		using namespace DataHeatBalFanSys;
		using namespace DataEnvironment;
		using namespace DataHeatBalance;
		using namespace InputProcessor;
		using ScheduleManager::GetScheduleIndex;
		using DataGlobals::BeginEnvrnFlag;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Ctd; // DO loop counter for surfaces
		Real64 HLD; // Convection coefficient for the lower area of surface
		Real64 TmedDV; // Average temperature for DV
		Real64 Z1; // auxiliary var for lowest height
		Real64 Z2; // auxiliary var for highest height
		Real64 ZSupSurf; // highest height for this surface
		Real64 ZInfSurf; // lowest height for this surface
		Real64 HLU; // Convection coefficient for the upper area of surface
		Real64 LayH; // Height of the Occupied/Mixed subzone interface
		Real64 LayFrac; // Fraction height of the Occupied/Mixed subzone interface
		int SurfNum; // Surface number

		HAT_MX = 0.0;
		HAT_OC = 0.0;
		HA_MX = 0.0;
		HA_OC = 0.0;
		HAT_FLOOR = 0.0;
		HA_FLOOR = 0.0;
		// Is the air flow model for this zone set to UCSDDV Displacement Ventilation?
		if ( IsZoneDV( ZoneNum ) ) {
			LayFrac = FractionHeight;
			LayH = FractionHeight * ( ZoneCeilingHeight( ( ZoneNum - 1 ) * 2 + 2 ) - ZoneCeilingHeight( ( ZoneNum - 1 ) * 2 + 1 ) );
			// WALL Hc, HA and HAT calculation
			for ( Ctd = PosZ_Wall( ( ZoneNum - 1 ) * 2 + 1 ); Ctd <= PosZ_Wall( ( ZoneNum - 1 ) * 2 + 2 ); ++Ctd ) {
				SurfNum = APos_Wall( Ctd );
				Surface( SurfNum ).TAirRef = AdjacentAirTemp;
				if ( SurfNum == 0 ) continue;
				Z1 = minval( Surface( SurfNum ).Vertex( {1,Surface( SurfNum ).Sides} ), &Vector::z );
				Z2 = maxval( Surface( SurfNum ).Vertex( {1,Surface( SurfNum ).Sides} ), &Vector::z );
				ZSupSurf = Z2 - ZoneCeilingHeight( ( ZoneNum - 1 ) * 2 + 1 );
				ZInfSurf = Z1 - ZoneCeilingHeight( ( ZoneNum - 1 ) * 2 + 1 );

				// The Wall surface is in the upper subzone
				if ( ZInfSurf > LayH ) {
					TempEffBulkAir( SurfNum ) = ZTMX( ZoneNum );
					CalcDetailedHcInForDVModel( SurfNum, TempSurfIn, DVHcIn );
					HWall( Ctd ) = DVHcIn( SurfNum );
					HAT_MX += Surface( SurfNum ).Area * TempSurfIn( SurfNum ) * HWall( Ctd );
					HA_MX += Surface( SurfNum ).Area * HWall( Ctd );
				}

				// The Wall surface is in the lower subzone
				if ( ZSupSurf < LayH ) {
					TempEffBulkAir( SurfNum ) = ZTOC( ZoneNum );
					CalcDetailedHcInForDVModel( SurfNum, TempSurfIn, DVHcIn );
					HWall( Ctd ) = DVHcIn( SurfNum );
					HAT_OC += Surface( SurfNum ).Area * TempSurfIn( SurfNum ) * HWall( Ctd );
					HA_OC += Surface( SurfNum ).Area * HWall( Ctd );
				}

				// The Wall surface is partially in upper and partially in lower subzone
				if ( ZInfSurf <= LayH && ZSupSurf >= LayH ) {
					TempEffBulkAir( SurfNum ) = ZTMX( ZoneNum );
					CalcDetailedHcInForDVModel( SurfNum, TempSurfIn, DVHcIn );
					HLU = DVHcIn( SurfNum );
					TempEffBulkAir( SurfNum ) = ZTOC( ZoneNum );
					CalcDetailedHcInForDVModel( SurfNum, TempSurfIn, DVHcIn );
					HLD = DVHcIn( SurfNum );
					TmedDV = ( ( ZSupSurf - LayH ) * ZTMX( ZoneNum ) + ( LayH - ZInfSurf ) * ZTOC( ZoneNum ) ) / ( ZSupSurf - ZInfSurf );
					HWall( Ctd ) = ( ( LayH - ZInfSurf ) * HLD + ( ZSupSurf - LayH ) * HLU ) / ( ZSupSurf - ZInfSurf );
					HAT_MX += Surface( SurfNum ).Area * ( ZSupSurf - LayH ) / ( ZSupSurf - ZInfSurf ) * TempSurfIn( SurfNum ) * HLU;
					HA_MX += Surface( SurfNum ).Area * ( ZSupSurf - LayH ) / ( ZSupSurf - ZInfSurf ) * HLU;
					HAT_OC += Surface( SurfNum ).Area * ( LayH - ZInfSurf ) / ( ZSupSurf - ZInfSurf ) * TempSurfIn( SurfNum ) * HLD;
					HA_OC += Surface( SurfNum ).Area * ( LayH - ZInfSurf ) / ( ZSupSurf - ZInfSurf ) * HLD;
					TempEffBulkAir( SurfNum ) = TmedDV;
				}

				DVHcIn( SurfNum ) = HWall( Ctd );

			} // END WALL

			// WINDOW Hc, HA and HAT CALCULATION
			for ( Ctd = PosZ_Window( ( ZoneNum - 1 ) * 2 + 1 ); Ctd <= PosZ_Window( ( ZoneNum - 1 ) * 2 + 2 ); ++Ctd ) {
				SurfNum = APos_Window( Ctd );
				Surface( SurfNum ).TAirRef = AdjacentAirTemp;
				if ( SurfNum == 0 ) continue;
				if ( Surface( SurfNum ).Tilt > 10.0 && Surface( SurfNum ).Tilt < 170.0 ) { // Window Wall
					Z1 = minval( Surface( SurfNum ).Vertex( {1,Surface( SurfNum ).Sides} ), &Vector::z );
					Z2 = maxval( Surface( SurfNum ).Vertex( {1,Surface( SurfNum ).Sides} ), &Vector::z );
					ZSupSurf = Z2 - ZoneCeilingHeight( ( ZoneNum - 1 ) * 2 + 1 );
					ZInfSurf = Z1 - ZoneCeilingHeight( ( ZoneNum - 1 ) * 2 + 1 );

					if ( ZInfSurf > LayH ) {
						TempEffBulkAir( SurfNum ) = ZTMX( ZoneNum );
						CalcDetailedHcInForDVModel( SurfNum, TempSurfIn, DVHcIn );
						HWindow( Ctd ) = DVHcIn( SurfNum );
						HAT_MX += Surface( SurfNum ).Area * TempSurfIn( SurfNum ) * HWindow( Ctd );
						HA_MX += Surface( SurfNum ).Area * HWindow( Ctd );
					}

					if ( ZSupSurf < LayH ) {
						TempEffBulkAir( SurfNum ) = ZTOC( ZoneNum );
						CalcDetailedHcInForDVModel( SurfNum, TempSurfIn, DVHcIn );
						HWindow( Ctd ) = DVHcIn( SurfNum );
						HAT_OC += Surface( SurfNum ).Area * TempSurfIn( SurfNum ) * HWindow( Ctd );
						HA_OC += Surface( SurfNum ).Area * HWindow( Ctd );
					}

					if ( ZInfSurf <= LayH && ZSupSurf >= LayH ) {
						TempEffBulkAir( SurfNum ) = ZTMX( ZoneNum );
						CalcDetailedHcInForDVModel( SurfNum, TempSurfIn, DVHcIn );
						HLU = DVHcIn( SurfNum );
						TempEffBulkAir( SurfNum ) = ZTOC( ZoneNum );
						CalcDetailedHcInForDVModel( SurfNum, TempSurfIn, DVHcIn );
						HLD = DVHcIn( SurfNum );
						TmedDV = ( ( ZSupSurf - LayH ) * ZTMX( ZoneNum ) + ( LayH - ZInfSurf ) * ZTOC( ZoneNum ) ) / ( ZSupSurf - ZInfSurf );
						HWindow( Ctd ) = ( ( LayH - ZInfSurf ) * HLD + ( ZSupSurf - LayH ) * HLU ) / ( ZSupSurf - ZInfSurf );
						HAT_MX += Surface( SurfNum ).Area * ( ZSupSurf - LayH ) / ( ZSupSurf - ZInfSurf ) * TempSurfIn( SurfNum ) * HLU;
						HA_MX += Surface( SurfNum ).Area * ( ZSupSurf - LayH ) / ( ZSupSurf - ZInfSurf ) * HLU;
						HAT_OC += Surface( SurfNum ).Area * ( LayH - ZInfSurf ) / ( ZSupSurf - ZInfSurf ) * TempSurfIn( SurfNum ) * HLD;
						HA_OC += Surface( SurfNum ).Area * ( LayH - ZInfSurf ) / ( ZSupSurf - ZInfSurf ) * HLD;
						TempEffBulkAir( SurfNum ) = TmedDV;
					}
				}

				if ( Surface( SurfNum ).Tilt <= 10.0 ) { // Window Ceiling
					TempEffBulkAir( SurfNum ) = ZTMX( ZoneNum );
					CalcDetailedHcInForDVModel( SurfNum, TempSurfIn, DVHcIn );
					HWindow( Ctd ) = DVHcIn( SurfNum );
					HAT_MX += Surface( SurfNum ).Area * TempSurfIn( SurfNum ) * HWindow( Ctd );
					HA_MX += Surface( SurfNum ).Area * HWindow( Ctd );
				}

				if ( Surface( SurfNum ).Tilt >= 170.0 ) { // Window Floor
					TempEffBulkAir( SurfNum ) = ZTOC( ZoneNum );
					CalcDetailedHcInForDVModel( SurfNum, TempSurfIn, DVHcIn );
					HWindow( Ctd ) = DVHcIn( SurfNum );
					HAT_OC += Surface( SurfNum ).Area * TempSurfIn( SurfNum ) * HWindow( Ctd );
					HA_OC += Surface( SurfNum ).Area * HWindow( Ctd );
				}

				DVHcIn( SurfNum ) = HWindow( Ctd );

			} // END WINDOW

			// DOOR Hc, HA and HAT CALCULATION
			for ( Ctd = PosZ_Door( ( ZoneNum - 1 ) * 2 + 1 ); Ctd <= PosZ_Door( ( ZoneNum - 1 ) * 2 + 2 ); ++Ctd ) { // DOOR
				SurfNum = APos_Door( Ctd );
				Surface( SurfNum ).TAirRef = AdjacentAirTemp;
				if ( SurfNum == 0 ) continue;
				Z1 = minval( Surface( SurfNum ).Vertex( {1,Surface( SurfNum ).Sides} ), &Vector::z );
				Z2 = maxval( Surface( SurfNum ).Vertex( {1,Surface( SurfNum ).Sides} ), &Vector::z );
				ZSupSurf = Z2 - ZoneCeilingHeight( ( ZoneNum - 1 ) * 2 + 1 );
				ZInfSurf = Z1 - ZoneCeilingHeight( ( ZoneNum - 1 ) * 2 + 1 );

				if ( ZInfSurf > LayH ) {
					TempEffBulkAir( SurfNum ) = ZTMX( ZoneNum );
					CalcDetailedHcInForDVModel( SurfNum, TempSurfIn, DVHcIn );
					HDoor( Ctd ) = DVHcIn( SurfNum );
					HAT_MX += Surface( SurfNum ).Area * TempSurfIn( SurfNum ) * HDoor( Ctd );
					HA_MX += Surface( SurfNum ).Area * HDoor( Ctd );
				}

				if ( ZSupSurf < LayH ) {
					TempEffBulkAir( SurfNum ) = ZTOC( ZoneNum );
					CalcDetailedHcInForDVModel( SurfNum, TempSurfIn, DVHcIn );
					HDoor( Ctd ) = DVHcIn( SurfNum );
					HAT_OC += Surface( SurfNum ).Area * TempSurfIn( SurfNum ) * HDoor( Ctd );
					HA_OC += Surface( SurfNum ).Area * HDoor( Ctd );
				}

				if ( ZInfSurf <= LayH && ZSupSurf >= LayH ) {
					TempEffBulkAir( SurfNum ) = ZTMX( ZoneNum );
					CalcDetailedHcInForDVModel( SurfNum, TempSurfIn, DVHcIn );
					HLU = DVHcIn( SurfNum );
					TempEffBulkAir( SurfNum ) = ZTOC( ZoneNum );
					CalcDetailedHcInForDVModel( SurfNum, TempSurfIn, DVHcIn );
					HLD = DVHcIn( SurfNum );
					TmedDV = ( ( ZSupSurf - LayH ) * ZTMX( ZoneNum ) + ( LayH - ZInfSurf ) * ZTOC( ZoneNum ) ) / ( ZSupSurf - ZInfSurf );
					HDoor( Ctd ) = ( ( LayH - ZInfSurf ) * HLD + ( ZSupSurf - LayH ) * HLU ) / ( ZSupSurf - ZInfSurf );
					HAT_MX += Surface( SurfNum ).Area * ( ZSupSurf - LayH ) / ( ZSupSurf - ZInfSurf ) * TempSurfIn( SurfNum ) * HLU;
					HA_MX += Surface( SurfNum ).Area * ( ZSupSurf - LayH ) / ( ZSupSurf - ZInfSurf ) * HLU;
					HAT_OC += Surface( SurfNum ).Area * ( LayH - ZInfSurf ) / ( ZSupSurf - ZInfSurf ) * TempSurfIn( SurfNum ) * HLD;
					HA_OC += Surface( SurfNum ).Area * ( LayH - ZInfSurf ) / ( ZSupSurf - ZInfSurf ) * HLD;
					TempEffBulkAir( SurfNum ) = TmedDV;
				}

				DVHcIn( SurfNum ) = HDoor( Ctd );

			} // END DOOR

			// INTERNAL Hc, HA and HAT CALCULATION
			HeightIntMass = min( HeightIntMassDefault, ( ZoneCeilingHeight( ( ZoneNum - 1 ) * 2 + 2 ) - ZoneCeilingHeight( ( ZoneNum - 1 ) * 2 + 1 ) ) );
			for ( Ctd = PosZ_Internal( ( ZoneNum - 1 ) * 2 + 1 ); Ctd <= PosZ_Internal( ( ZoneNum - 1 ) * 2 + 2 ); ++Ctd ) {
				SurfNum = APos_Internal( Ctd );
				Surface( SurfNum ).TAirRef = AdjacentAirTemp;
				if ( SurfNum == 0 ) continue;
				ZSupSurf = HeightIntMass;
				ZInfSurf = 0.0;

				if ( ZSupSurf < LayH ) {
					TempEffBulkAir( SurfNum ) = ZTOC( ZoneNum );
					CalcDetailedHcInForDVModel( SurfNum, TempSurfIn, DVHcIn );
					HInternal( Ctd ) = DVHcIn( SurfNum );
					HAT_OC += Surface( SurfNum ).Area * TempSurfIn( SurfNum ) * HInternal( Ctd );
					HA_OC += Surface( SurfNum ).Area * HInternal( Ctd );
				}

				if ( ZInfSurf <= LayH && ZSupSurf >= LayH ) {
					TempEffBulkAir( SurfNum ) = ZTMX( ZoneNum );
					CalcDetailedHcInForDVModel( SurfNum, TempSurfIn, DVHcIn );
					HLU = DVHcIn( SurfNum );
					TempEffBulkAir( SurfNum ) = ZTOC( ZoneNum );
					CalcDetailedHcInForDVModel( SurfNum, TempSurfIn, DVHcIn );
					HLD = DVHcIn( SurfNum );
					TmedDV = ( ( ZSupSurf - LayH ) * ZTMX( ZoneNum ) + ( LayH - ZInfSurf ) * ZTOC( ZoneNum ) ) / ( ZSupSurf - ZInfSurf );
					HInternal( Ctd ) = ( ( LayH - ZInfSurf ) * HLD + ( ZSupSurf - LayH ) * HLU ) / ( ZSupSurf - ZInfSurf );
					HAT_MX += Surface( SurfNum ).Area * ( ZSupSurf - LayH ) / ( ZSupSurf - ZInfSurf ) * TempSurfIn( SurfNum ) * HLU;
					HA_MX += Surface( SurfNum ).Area * ( ZSupSurf - LayH ) / ( ZSupSurf - ZInfSurf ) * HLU;
					HAT_OC += Surface( SurfNum ).Area * ( LayH - ZInfSurf ) / ( ZSupSurf - ZInfSurf ) * TempSurfIn( SurfNum ) * HLD;
					HA_OC += Surface( SurfNum ).Area * ( LayH - ZInfSurf ) / ( ZSupSurf - ZInfSurf ) * HLD;
					TempEffBulkAir( SurfNum ) = TmedDV;
				}

				DVHcIn( SurfNum ) = HInternal( Ctd );
			} // END INTERNAL

			// CEILING Hc, HA and HAT CALCULATION
			for ( Ctd = PosZ_Ceiling( ( ZoneNum - 1 ) * 2 + 1 ); Ctd <= PosZ_Ceiling( ( ZoneNum - 1 ) * 2 + 2 ); ++Ctd ) {
				SurfNum = APos_Ceiling( Ctd );
				Surface( SurfNum ).TAirRef = AdjacentAirTemp;
				if ( SurfNum == 0 ) continue;
				TempEffBulkAir( SurfNum ) = ZTMX( ZoneNum );
				CalcDetailedHcInForDVModel( SurfNum, TempSurfIn, DVHcIn );
				HCeiling( Ctd ) = DVHcIn( SurfNum );
				HAT_MX += Surface( SurfNum ).Area * TempSurfIn( SurfNum ) * HCeiling( Ctd );
				HA_MX += Surface( SurfNum ).Area * HCeiling( Ctd );
				DVHcIn( SurfNum ) = HCeiling( Ctd );
			} // END CEILING

			// FLOOR Hc, HA and HAT CALCULATION
			for ( Ctd = PosZ_Floor( ( ZoneNum - 1 ) * 2 + 1 ); Ctd <= PosZ_Floor( ( ZoneNum - 1 ) * 2 + 2 ); ++Ctd ) {
				SurfNum = APos_Floor( Ctd );
				Surface( SurfNum ).TAirRef = AdjacentAirTemp;
				if ( SurfNum == 0 ) continue;
				TempEffBulkAir( SurfNum ) = ZTFloor( ZoneNum );
				CalcDetailedHcInForDVModel( SurfNum, TempSurfIn, DVHcIn );
				HFloor( Ctd ) = DVHcIn( SurfNum );
				HAT_FLOOR += Surface( SurfNum ).Area * TempSurfIn( SurfNum ) * HFloor( Ctd );
				HA_FLOOR += Surface( SurfNum ).Area * HFloor( Ctd );
				TempEffBulkAir( SurfNum ) = ZTFloor( ZoneNum );
				DVHcIn( SurfNum ) = HFloor( Ctd );
			} // END FLOOR

		}

	}

	//**************************************************************************************************

	void
	CalcUCSDDV( int const ZoneNum ) // Which Zonenum
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         G. Carrilho da Graca
		//       DATE WRITTEN   February 2004
		//       MODIFIED       Brent Griffith June 2008 for new interpolation and time history
		//       RE-ENGINEERED  -

		// PURPOSE OF THIS SUBROUTINE:
		// Subroutine for displacement ventilation modelling.
		// This subroutine calculates the mixed subzone height, surface heat transfer coefficients and
		// room air equivalent temperatures and three space temperatures (floor subzone, occupied zone and upper,
		// mixed subzone temperature)

		// METHODOLOGY EMPLOYED:
		// -
		// -
		// -
		// -

		// REFERENCES:
		// Model developed by Paul Linden (UCSD), G. Carrilho da Graca (UCSD) and P. Haves (LBL).
		// Work funded by the California Energy Comission. More information on the model can found in:
		// "Simplified Models for Heat Transfer in Rooms" G. Carrilho da Graça, Ph.D. thesis UCSD. December 2003.

		// Using/Aliasing
		using namespace DataHeatBalFanSys;
		using namespace DataEnvironment;
		using namespace DataHeatBalance;
		using namespace InputProcessor;
		using ScheduleManager::GetScheduleIndex;
		using ScheduleManager::GetCurrentScheduleValue;
		using DataZoneEquipment::ZoneEquipConfig;
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using Psychrometrics::PsyCpAirFnWTdb;
		using DataHVACGlobals::TimeStepSys;
		using DataHVACGlobals::UseZoneTimeStepHistory;
		using InternalHeatGains::SumInternalConvectionGainsByTypes;
		using InternalHeatGains::SumReturnAirConvectionGainsByTypes;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static Real64 const OneThird( 1.0 / 3.0 );
		static Real64 const MinFlow_pow_fac( std::pow( 1.0 / 24.55 * 1.0, 1.0 / 0.6 ) );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 HeightFrac; // Fractional height of transition between occupied and mixed subzones
		Real64 GainsFrac; // Fraction of lower subzone internal gains that mix as opposed to forming plumes
		Real64 ConvGains; // Total convective gains in the room
		Real64 ConvGainsOccupiedSubzone; // Total convective gains released in occupied subzone
		Real64 ConvGainsMixedSubzone; // Total convective gains released in mixed subzone
		Real64 MCp_Total; // Total capacity rate into the zone - assumed to enter at low level
		Real64 ZTAveraged;
		Real64 TempDiffCritRep; // Minimum temperature difference between mixed and occupied subzones for reporting
		bool MIXFLAG;
		int Ctd;
		Real64 MinFlow;
		Real64 NumPLPP; // Number of plumes per person
		int NumberOfOccupants;
		Real64 MTGAUX;
		int ZoneEquipConfigNum;
		int NodeNum;
		Real64 PowerInPlumes;
		Real64 SumSysMCp;
		Real64 SumSysMCpT;
		Real64 NodeTemp;
		Real64 MassFlowRate;
		Real64 CpAir;
		Real64 MCpT_Total;
		int ZoneNodeNum; // index number of the zone node
		Real64 NumberOfPlumes;
		Real64 SumMCp;
		Real64 SumMCpT;
		Real64 AirCap;
		Real64 TempHistTerm;
		Real64 PowerPerPlume;
		Real64 HeightMixedSubzoneAve; // Height of center of mixed air subzone
		Real64 HeightOccupiedSubzoneAve; // Height of center of occupied air subzone
		Real64 HeightFloorSubzoneAve; // Height of center of floor air subzone
		Real64 HeightThermostat; // Height of center of thermostat/temperature control sensor
		Real64 HeightComfort; // Height at which air temperature value is used to calculate comfort
		Real64 CeilingHeight;
		Real64 ZoneMult; // total zone multiplier
		int Loop;
		int FlagApertures;
		static Real64 TempDepCoef( 0.0 ); // Formerly CoefSumha, coef in zone temp equation with dimensions of h*A
		static Real64 TempIndCoef( 0.0 ); // Formerly CoefSumhat, coef in zone temp equation with dimensions of h*A(T1
		static Array1D_int IntGainTypesOccupied( 28, { IntGainTypeOf_People, IntGainTypeOf_WaterHeaterMixed, IntGainTypeOf_WaterHeaterStratified, IntGainTypeOf_ThermalStorageChilledWaterMixed, IntGainTypeOf_ThermalStorageChilledWaterStratified, IntGainTypeOf_ElectricEquipment, IntGainTypeOf_GasEquipment, IntGainTypeOf_HotWaterEquipment, IntGainTypeOf_SteamEquipment, IntGainTypeOf_OtherEquipment, IntGainTypeOf_ZoneBaseboardOutdoorTemperatureControlled, IntGainTypeOf_GeneratorFuelCell, IntGainTypeOf_WaterUseEquipment, IntGainTypeOf_GeneratorMicroCHP, IntGainTypeOf_ElectricLoadCenterTransformer, IntGainTypeOf_ElectricLoadCenterInverterSimple, IntGainTypeOf_ElectricLoadCenterInverterFunctionOfPower, IntGainTypeOf_ElectricLoadCenterInverterLookUpTable, IntGainTypeOf_ElectricLoadCenterStorageBattery, IntGainTypeOf_ElectricLoadCenterStorageSimple, IntGainTypeOf_PipeIndoor, IntGainTypeOf_RefrigerationCase, IntGainTypeOf_RefrigerationCompressorRack, IntGainTypeOf_RefrigerationSystemAirCooledCondenser, IntGainTypeOf_RefrigerationSystemSuctionPipe, IntGainTypeOf_RefrigerationSecondaryReceiver, IntGainTypeOf_RefrigerationSecondaryPipe, IntGainTypeOf_RefrigerationWalkIn } );

		static Array1D_int IntGainTypesMixedSubzone( 2, { IntGainTypeOf_DaylightingDeviceTubular, IntGainTypeOf_Lights } );
		Real64 RetAirGain;

		// Exact solution or Euler method
		if ( ZoneAirSolutionAlgo != Use3rdOrder ) {
			if ( ShortenTimeStepSysRoomAir && TimeStepSys < TimeStepZone ) {
				if ( PreviousTimeStep < TimeStepZone ) {
					Zone1Floor( ZoneNum ) = ZoneM2Floor( ZoneNum );
					Zone1OC( ZoneNum ) = ZoneM2OC( ZoneNum );
					Zone1MX( ZoneNum ) = ZoneM2MX( ZoneNum );
				} else {
					Zone1Floor( ZoneNum ) = ZoneMXFloor( ZoneNum );
					Zone1OC( ZoneNum ) = ZoneMXOC( ZoneNum );
					Zone1MX( ZoneNum ) = ZoneMXMX( ZoneNum );
				}
			} else {
				Zone1Floor( ZoneNum ) = ZTFloor( ZoneNum );
				Zone1OC( ZoneNum ) = ZTOC( ZoneNum );
				Zone1MX( ZoneNum ) = ZTMX( ZoneNum );
			}
		}

		MIXFLAG = false;
		FlagApertures = 1;
		DVHcIn = HConvIn;
		CeilingHeight = ZoneCeilingHeight( ( ZoneNum - 1 ) * 2 + 2 ) - ZoneCeilingHeight( ( ZoneNum - 1 ) * 2 + 1 );
		ZoneMult = Zone( ZoneNum ).Multiplier * Zone( ZoneNum ).ListMultiplier;

		for ( Ctd = 1; Ctd <= TotUCSDDV; ++Ctd ) {
			if ( ZoneNum == ZoneUCSDDV( Ctd ).ZonePtr ) {
				GainsFrac = GetCurrentScheduleValue( ZoneUCSDDV( Ctd ).SchedGainsPtr );
				NumPLPP = ZoneUCSDDV( Ctd ).NumPlumesPerOcc;
				HeightThermostat = ZoneUCSDDV( Ctd ).ThermostatHeight;
				HeightComfort = ZoneUCSDDV( Ctd ).ComfortHeight;
				TempDiffCritRep = ZoneUCSDDV( Ctd ).TempTrigger;
			}
		}

		SumInternalConvectionGainsByTypes( ZoneNum, IntGainTypesOccupied, ConvGainsOccupiedSubzone );

		ConvGainsOccupiedSubzone += 0.5 * SysDepZoneLoadsLagged( ZoneNum );

		// Add heat to return air if zonal system (no return air) or cycling system (return air frequently very
		// low or zero)
		if ( Zone( ZoneNum ).NoHeatToReturnAir ) {
			SumReturnAirConvectionGainsByTypes( ZoneNum, IntGainTypesOccupied, RetAirGain );
			ConvGainsOccupiedSubzone += RetAirGain;

		}

		SumInternalConvectionGainsByTypes( ZoneNum, IntGainTypesMixedSubzone, ConvGainsMixedSubzone );
		ConvGainsMixedSubzone += SumConvHTRadSys( ZoneNum ) + SumConvPool( ZoneNum ) + 0.5 * SysDepZoneLoadsLagged( ZoneNum );
		if ( Zone( ZoneNum ).NoHeatToReturnAir ) {
			SumReturnAirConvectionGainsByTypes( ZoneNum, IntGainTypesMixedSubzone, RetAirGain );
			ConvGainsMixedSubzone += RetAirGain;
		}

		ConvGains = ConvGainsOccupiedSubzone + ConvGainsMixedSubzone;

		//=================== Entering air system temperature and flow====================
		SumSysMCp = 0.0;
		SumSysMCpT = 0.0;
		// Check to make sure if this is a controlled zone and determine ZoneEquipConfigNum
		ZoneEquipConfigNum = ZoneNum;
		if ( ZoneEquipConfig( ZoneEquipConfigNum ).IsControlled ) {
			for ( NodeNum = 1; NodeNum <= ZoneEquipConfig( ZoneEquipConfigNum ).NumInletNodes; ++NodeNum ) {
				NodeTemp = Node( ZoneEquipConfig( ZoneEquipConfigNum ).InletNode( NodeNum ) ).Temp;
				MassFlowRate = Node( ZoneEquipConfig( ZoneEquipConfigNum ).InletNode( NodeNum ) ).MassFlowRate;
				CpAir = PsyCpAirFnWTdb( ZoneAirHumRat( ZoneNum ), NodeTemp );
				SumSysMCp += MassFlowRate * CpAir;
				SumSysMCpT += MassFlowRate * CpAir * NodeTemp;
			}
		}

		SumMCp = MCPI( ZoneNum ) + MCPV( ZoneNum ) + MCPM( ZoneNum ) + MCPE( ZoneNum ) + MCPC( ZoneNum ) + MDotCPOA( ZoneNum );
		SumMCpT = MCPTI( ZoneNum ) + MCPTV( ZoneNum ) + MCPTM( ZoneNum ) + MCPTE( ZoneNum ) + MCPTC( ZoneNum ) + MDotCPOA( ZoneNum ) * Zone( ZoneNum ).OutDryBulbTemp;
		if ( SimulateAirflowNetwork == AirflowNetworkControlMultizone ) {
			SumMCp = AirflowNetworkExchangeData( ZoneNum ).SumMCp + AirflowNetworkExchangeData( ZoneNum ).SumMMCp;
			SumMCpT = AirflowNetworkExchangeData( ZoneNum ).SumMCpT + AirflowNetworkExchangeData( ZoneNum ).SumMMCpT;
		}

		MCp_Total = SumMCp + SumSysMCp;
		MCpT_Total = SumMCpT + SumSysMCpT;

		if ( TotPeople > 0 ) {
			NumberOfOccupants = 0;
			NumberOfPlumes = 0.0;
			for ( Ctd = 1; Ctd <= TotPeople; ++Ctd ) {
				if ( People( Ctd ).ZonePtr == ZoneNum ) {
					NumberOfOccupants += People( Ctd ).NumberOfPeople; // *GetCurrentScheduleValue(People(Ctd)%NumberOfPeoplePtr)
					NumberOfPlumes = NumberOfOccupants * NumPLPP;
				}
			}
			if ( NumberOfPlumes == 0.0 ) {
				NumberOfPlumes = 1.0;
			}
			PowerInPlumes = ( 1.0 - GainsFrac ) * ConvGainsOccupiedSubzone;
			PowerPerPlume = PowerInPlumes / NumberOfPlumes;
		} else {
			NumberOfPlumes = 1.0;
			PowerInPlumes = ( 1.0 - GainsFrac ) * ConvGainsOccupiedSubzone;
			PowerPerPlume = PowerInPlumes / NumberOfPlumes;
		}

		// When AirflowNetwork is used verify if bottom apertures are inflowing and upper apertures are
		// outflowing. The lower apertures have to be located below 0.8m and the upper apertures
		// have to be located above 1.8m.

		if ( NumOfLinksMultiZone > 0 ) {
			for ( Loop = 1; Loop <= AirflowNetworkSurfaceUCSDCV( 0, ZoneNum ); ++Loop ) {
				// direct AirflowNetwork surface

				if ( Surface( MultizoneSurfaceData( AirflowNetworkSurfaceUCSDCV( Loop, ZoneNum ) ).SurfNum ).Zone == ZoneNum ) {

					if ( ( SurfParametersCVDV( AirflowNetworkSurfaceUCSDCV( Loop, ZoneNum ) ).Zmax < 0.8 && AirflowNetworkLinkSimu( AirflowNetworkSurfaceUCSDCV( Loop, ZoneNum ) ).VolFLOW > 0 ) ) {
						FlagApertures = 0;
						break;
					}
					if ( SurfParametersCVDV( AirflowNetworkSurfaceUCSDCV( Loop, ZoneNum ) ).Zmin > 1.8 && AirflowNetworkLinkSimu( AirflowNetworkSurfaceUCSDCV( Loop, ZoneNum ) ).VolFLOW2 > 0 ) {
						FlagApertures = 0;
						break;
					}

					if ( ( SurfParametersCVDV( AirflowNetworkSurfaceUCSDCV( Loop, ZoneNum ) ).Zmin > 0.8 && SurfParametersCVDV( AirflowNetworkSurfaceUCSDCV( Loop, ZoneNum ) ).Zmin < 1.8 ) || ( SurfParametersCVDV( AirflowNetworkSurfaceUCSDCV( Loop, ZoneNum ) ).Zmax > 0.8 && SurfParametersCVDV( AirflowNetworkSurfaceUCSDCV( Loop, ZoneNum ) ).Zmax < 1.8 ) ) {
						FlagApertures = 0;
						break;
					}
					// indirect AirflowNetwork surface; this is an interzone surface
				} else {

					if ( SurfParametersCVDV( AirflowNetworkSurfaceUCSDCV( Loop, ZoneNum ) ).Zmax + Zone( Surface( MultizoneSurfaceData( AirflowNetworkSurfaceUCSDCV( Loop, ZoneNum ) ).SurfNum ).Zone ).OriginZ - Zone( ZoneNum ).OriginZ < 0.8 && AirflowNetworkLinkSimu( AirflowNetworkSurfaceUCSDCV( Loop, ZoneNum ) ).VolFLOW2 > 0 ) {
						FlagApertures = 0;
						break;
					}
					if ( SurfParametersCVDV( AirflowNetworkSurfaceUCSDCV( Loop, ZoneNum ) ).Zmin + Zone( Surface( MultizoneSurfaceData( AirflowNetworkSurfaceUCSDCV( Loop, ZoneNum ) ).SurfNum ).Zone ).OriginZ - Zone( ZoneNum ).OriginZ > 1.8 && AirflowNetworkLinkSimu( AirflowNetworkSurfaceUCSDCV( Loop, ZoneNum ) ).VolFLOW > 0 ) {
						FlagApertures = 0;
						break;
					}
					if ( ( SurfParametersCVDV( AirflowNetworkSurfaceUCSDCV( Loop, ZoneNum ) ).Zmin + Zone( Surface( MultizoneSurfaceData( AirflowNetworkSurfaceUCSDCV( Loop, ZoneNum ) ).SurfNum ).Zone ).OriginZ - Zone( ZoneNum ).OriginZ > 0.8 && SurfParametersCVDV( AirflowNetworkSurfaceUCSDCV( Loop, ZoneNum ) ).Zmin + Zone( Surface( MultizoneSurfaceData( AirflowNetworkSurfaceUCSDCV( Loop, ZoneNum ) ).SurfNum ).Zone ).OriginZ - Zone( ZoneNum ).OriginZ < 1.8 ) || ( SurfParametersCVDV( AirflowNetworkSurfaceUCSDCV( Loop, ZoneNum ) ).Zmax + Zone( Surface( MultizoneSurfaceData( AirflowNetworkSurfaceUCSDCV( Loop, ZoneNum ) ).SurfNum ).Zone ).OriginZ - Zone( ZoneNum ).OriginZ > 0.8 && SurfParametersCVDV( AirflowNetworkSurfaceUCSDCV( Loop, ZoneNum ) ).Zmax + Zone( Surface( MultizoneSurfaceData( AirflowNetworkSurfaceUCSDCV( Loop, ZoneNum ) ).SurfNum ).Zone ).OriginZ - Zone( ZoneNum ).OriginZ < 1.8 ) ) {
						FlagApertures = 0;
						break;
					}
				}
			}
		}

		if ( ( PowerInPlumes == 0.0 ) || ( MCpT_Total == 0.0 ) || FlagApertures == 0 ) {
			// The system will mix
			HeightFrac = 0.0;
		} else {
			Real64 const plume_fac( NumberOfPlumes * std::pow( PowerPerPlume, OneThird ) );
			HeightFrac = min( 24.55 * std::pow( MCp_Total * 0.000833 / plume_fac, 0.6 ) / CeilingHeight, 1.0 );
			for ( Ctd = 1; Ctd <= 4; ++Ctd ) {
				HcUCSDDV( ZoneNum, HeightFrac );
				//HeightFrac = min( 24.55 * std::pow( MCp_Total * 0.000833 / ( NumberOfPlumes * std::pow( PowerPerPlume, OneThird ) ), 0.6 ) / CeilingHeight, 1.0 ); //Tuned This does not vary in loop
				//EPTeam-replaces above (cause diffs)      HeightFrac = MIN(24.55d0*(MCp_Total*0.000833d0/(NumberOfPlumes*PowerPerPlume**(1.0d0/3.d0)))**0.6 / CeilingHeight , 1.0d0)
				HeightTransition( ZoneNum ) = HeightFrac * CeilingHeight;
				AIRRATFloor( ZoneNum ) = Zone( ZoneNum ).Volume * min( HeightTransition( ZoneNum ), HeightFloorSubzoneTop ) / CeilingHeight * ZoneVolCapMultpSens * PsyRhoAirFnPbTdbW( OutBaroPress, MATFloor( ZoneNum ), ZoneAirHumRat( ZoneNum ) ) * PsyCpAirFnWTdb( ZoneAirHumRat( ZoneNum ), MATFloor( ZoneNum ) ) / ( TimeStepSys * SecInHour );

				AIRRATOC( ZoneNum ) = Zone( ZoneNum ).Volume * ( HeightTransition( ZoneNum ) - min( HeightTransition( ZoneNum ), 0.2 ) ) / CeilingHeight * ZoneVolCapMultpSens * PsyRhoAirFnPbTdbW( OutBaroPress, MATOC( ZoneNum ), ZoneAirHumRat( ZoneNum ) ) * PsyCpAirFnWTdb( ZoneAirHumRat( ZoneNum ), MATOC( ZoneNum ) ) / ( TimeStepSys * SecInHour );

				AIRRATMX( ZoneNum ) = Zone( ZoneNum ).Volume * ( CeilingHeight - HeightTransition( ZoneNum ) ) / CeilingHeight * ZoneVolCapMultpSens * PsyRhoAirFnPbTdbW( OutBaroPress, MATMX( ZoneNum ), ZoneAirHumRat( ZoneNum ) ) * PsyCpAirFnWTdb( ZoneAirHumRat( ZoneNum ), MATMX( ZoneNum ) ) / ( TimeStepSys * SecInHour );

				if ( UseZoneTimeStepHistory ) {
					ZTM3Floor( ZoneNum ) = XM3TFloor( ZoneNum );
					ZTM2Floor( ZoneNum ) = XM2TFloor( ZoneNum );
					ZTM1Floor( ZoneNum ) = XMATFloor( ZoneNum );

					ZTM3OC( ZoneNum ) = XM3TOC( ZoneNum );
					ZTM2OC( ZoneNum ) = XM2TOC( ZoneNum );
					ZTM1OC( ZoneNum ) = XMATOC( ZoneNum );

					ZTM3MX( ZoneNum ) = XM3TMX( ZoneNum );
					ZTM2MX( ZoneNum ) = XM2TMX( ZoneNum );
					ZTM1MX( ZoneNum ) = XMATMX( ZoneNum );

				} else {
					ZTM3Floor( ZoneNum ) = DSXM3TFloor( ZoneNum );
					ZTM2Floor( ZoneNum ) = DSXM2TFloor( ZoneNum );
					ZTM1Floor( ZoneNum ) = DSXMATFloor( ZoneNum );

					ZTM3OC( ZoneNum ) = DSXM3TOC( ZoneNum );
					ZTM2OC( ZoneNum ) = DSXM2TOC( ZoneNum );
					ZTM1OC( ZoneNum ) = DSXMATOC( ZoneNum );

					ZTM3MX( ZoneNum ) = DSXM3TMX( ZoneNum );
					ZTM2MX( ZoneNum ) = DSXM2TMX( ZoneNum );
					ZTM1MX( ZoneNum ) = DSXMATMX( ZoneNum );

				}

				AirCap = AIRRATFloor( ZoneNum );
				TempHistTerm = AirCap * ( 3.0 * ZTM1Floor( ZoneNum ) - ( 3.0 / 2.0 ) * ZTM2Floor( ZoneNum ) + OneThird * ZTM3Floor( ZoneNum ) );
				TempDepCoef = HA_FLOOR + MCp_Total;
				TempIndCoef = HAT_FLOOR + MCpT_Total + NonAirSystemResponse( ZoneNum ) / ZoneMult;
				{ auto const SELECT_CASE_var( ZoneAirSolutionAlgo );
				if ( SELECT_CASE_var == Use3rdOrder ) {
					ZTFloor( ZoneNum ) = ( TempHistTerm + HAT_FLOOR + MCpT_Total + NonAirSystemResponse( ZoneNum ) / ZoneMult ) / ( ( 11.0 / 6.0 ) * AirCap + HA_FLOOR + MCp_Total );
				} else if ( SELECT_CASE_var == UseAnalyticalSolution ) {
					if ( TempDepCoef == 0.0 ) { // B=0
						ZTFloor( ZoneNum ) = Zone1Floor( ZoneNum ) + TempIndCoef / AirCap;
					} else {
						ZTFloor( ZoneNum ) = ( Zone1Floor( ZoneNum ) - TempIndCoef / TempDepCoef ) * std::exp( min( 700.0, -TempDepCoef / AirCap ) ) + TempIndCoef / TempDepCoef;
					}
				} else if ( SELECT_CASE_var == UseEulerMethod ) {
					ZTFloor( ZoneNum ) = ( AirCap * Zone1Floor( ZoneNum ) + TempIndCoef ) / ( AirCap + TempDepCoef );
				}}
				AirCap = AIRRATOC( ZoneNum );
				TempHistTerm = AirCap * ( 3.0 * ZTM1OC( ZoneNum ) - ( 3.0 / 2.0 ) * ZTM2OC( ZoneNum ) + OneThird * ZTM3OC( ZoneNum ) );
				TempDepCoef = HA_OC + MCp_Total;
				TempIndCoef = ConvGainsOccupiedSubzone * GainsFrac + HAT_OC + ZTFloor( ZoneNum ) * MCp_Total;
				{ auto const SELECT_CASE_var( ZoneAirSolutionAlgo );
				if ( SELECT_CASE_var == Use3rdOrder ) {
					ZTOC( ZoneNum ) = ( TempHistTerm + ConvGainsOccupiedSubzone * GainsFrac + HAT_OC + ZTFloor( ZoneNum ) * MCp_Total ) / ( ( 11.0 / 6.0 ) * AirCap + HA_OC + MCp_Total );
				} else if ( SELECT_CASE_var == UseAnalyticalSolution ) {
					if ( TempDepCoef == 0.0 ) { // B=0
						ZTOC( ZoneNum ) = Zone1OC( ZoneNum ) + TempIndCoef / AirCap;
					} else {
						if ( AirCap == 0.0 ) {
							ZTOC( ZoneNum ) = TempIndCoef / TempDepCoef;
						} else {
							ZTOC( ZoneNum ) = ( Zone1OC( ZoneNum ) - TempIndCoef / TempDepCoef ) * std::exp( min( 700.0, -TempDepCoef / AirCap ) ) + TempIndCoef / TempDepCoef;
						}
					}
				} else if ( SELECT_CASE_var == UseEulerMethod ) {
					ZTOC( ZoneNum ) = ( AirCap * Zone1OC( ZoneNum ) + TempIndCoef ) / ( AirCap + TempDepCoef );
				}}
				AirCap = AIRRATMX( ZoneNum );
				TempHistTerm = AirCap * ( 3.0 * ZTM1MX( ZoneNum ) - ( 3.0 / 2.0 ) * ZTM2MX( ZoneNum ) + OneThird * ZTM3MX( ZoneNum ) );
				TempDepCoef = HA_MX + MCp_Total;
				TempIndCoef = ConvGainsOccupiedSubzone * ( 1.0 - GainsFrac ) + ConvGainsMixedSubzone + HAT_MX + ZTOC( ZoneNum ) * MCp_Total;
				{ auto const SELECT_CASE_var( ZoneAirSolutionAlgo );
				if ( SELECT_CASE_var == Use3rdOrder ) {
					ZTMX( ZoneNum ) = ( TempHistTerm + ConvGainsOccupiedSubzone * ( 1.0 - GainsFrac ) + ConvGainsMixedSubzone + HAT_MX + ZTOC( ZoneNum ) * MCp_Total ) / ( ( 11.0 / 6.0 ) * AirCap + HA_MX + MCp_Total );
				} else if ( SELECT_CASE_var == UseAnalyticalSolution ) {
					if ( TempDepCoef == 0.0 ) { // B=0
						ZTMX( ZoneNum ) = Zone1MX( ZoneNum ) + TempIndCoef / AirCap;
					} else {
						if ( AirCap == 0.0 ) {
							ZTMX( ZoneNum ) = TempIndCoef / TempDepCoef;
						} else {
							ZTMX( ZoneNum ) = ( Zone1MX( ZoneNum ) - TempIndCoef / TempDepCoef ) * std::exp( min( 700.0, -TempDepCoef / AirCap ) ) + TempIndCoef / TempDepCoef;
						}
					}
				} else if ( SELECT_CASE_var == UseEulerMethod ) {
					ZTMX( ZoneNum ) = ( AirCap * Zone1MX( ZoneNum ) + TempIndCoef ) / ( AirCap + TempDepCoef );
				}}
			}

			// MinFlow for interface layer at z = 1.0
			MinFlow = MinFlow_pow_fac * plume_fac;
			//EPTeam above replaces (cause diffs?)   MinFlow = (1.0d0/24.55d0*1.0d0)**(1.0d0/0.6d0)*NumberOfPlumes*PowerPerPlume**(1.0/3.0)
			if ( MinFlow != 0.0 ) {
				FracMinFlow( ZoneNum ) = MCp_Total * 0.000833 / MinFlow;
			} else {
				FracMinFlow( ZoneNum ) = 9.999;
			}
			AirModel( ZoneNum ).SimAirModel = true;
		}

		//=============================== M I X E D  Calculation ==============================================
		if ( ZTMX( ZoneNum ) < ZTOC( ZoneNum ) || MCp_Total <= 0.0 || HeightFrac * CeilingHeight < ( HeightFloorSubzoneTop + ThickOccupiedSubzoneMin ) ) {
			MIXFLAG = true;
			HeightFrac = 0.0;
			AvgTempGrad( ZoneNum ) = 0.0;
			MaxTempGrad( ZoneNum ) = 0.0;
			AirModel( ZoneNum ).SimAirModel = false;
			AirCap = AIRRAT( ZoneNum );
			TempHistTerm = AirCap * ( 3.0 * ZTM1( ZoneNum ) - ( 3.0 / 2.0 ) * ZTM2( ZoneNum ) + OneThird * ZTM3( ZoneNum ) );

			for ( Ctd = 1; Ctd <= 3; ++Ctd ) {
				TempDepCoef = HA_MX + HA_OC + HA_FLOOR + MCp_Total;
				TempIndCoef = ConvGains + HAT_MX + HAT_OC + HAT_FLOOR + MCpT_Total;
				{ auto const SELECT_CASE_var( ZoneAirSolutionAlgo );
				if ( SELECT_CASE_var == Use3rdOrder ) {
					ZTAveraged = ( TempHistTerm + ConvGains + HAT_MX + HAT_OC + HAT_FLOOR + MCpT_Total ) / ( ( 11.0 / 6.0 ) * AirCap + HA_MX + HA_OC + HA_FLOOR + MCp_Total );
				} else if ( SELECT_CASE_var == UseAnalyticalSolution ) {
					if ( TempDepCoef == 0.0 ) { // B=0
						ZTAveraged = ZoneT1( ZoneNum ) + TempIndCoef / AirCap;
					} else {
						ZTAveraged = ( ZoneT1( ZoneNum ) - TempIndCoef / TempDepCoef ) * std::exp( min( 700.0, -TempDepCoef / AirCap ) ) + TempIndCoef / TempDepCoef;
					}
				} else if ( SELECT_CASE_var == UseEulerMethod ) {
					ZTAveraged = ( AirCap * ZoneT1( ZoneNum ) + TempIndCoef ) / ( AirCap + TempDepCoef );
				}}
				ZTOC( ZoneNum ) = ZTAveraged;
				ZTMX( ZoneNum ) = ZTAveraged;
				ZTFloor( ZoneNum ) = ZTAveraged;
				HcUCSDDV( ZoneNum, HeightFrac );
				TempDepCoef = HA_MX + HA_OC + HA_FLOOR + MCp_Total;
				TempIndCoef = ConvGains + HAT_MX + HAT_OC + HAT_FLOOR + MCpT_Total;
				{ auto const SELECT_CASE_var( ZoneAirSolutionAlgo );
				if ( SELECT_CASE_var == Use3rdOrder ) {
					ZTAveraged = ( TempHistTerm + ConvGains + HAT_MX + HAT_OC + HAT_FLOOR + MCpT_Total ) / ( ( 11.0 / 6.0 ) * AirCap + HA_MX + HA_OC + HA_FLOOR + MCp_Total );
				} else if ( SELECT_CASE_var == UseAnalyticalSolution ) {
					if ( TempDepCoef == 0.0 ) { // B=0
						ZTAveraged = ZoneT1( ZoneNum ) + TempIndCoef / AirCap;
					} else {
						ZTAveraged = ( ZoneT1( ZoneNum ) - TempIndCoef / TempDepCoef ) * std::exp( min( 700.0, -TempDepCoef / AirCap ) ) + TempIndCoef / TempDepCoef;
					}
				} else if ( SELECT_CASE_var == UseEulerMethod ) {
					ZTAveraged = ( AirCap * ZoneT1( ZoneNum ) + TempIndCoef ) / ( AirCap + TempDepCoef );
				}}
				ZTOC( ZoneNum ) = ZTAveraged;
				ZTMX( ZoneNum ) = ZTAveraged;
				ZTFloor( ZoneNum ) = ZTAveraged;
			}

		}
		//=========================================================================================

		// Comfort temperature and temperature at the thermostat/temperature control sensor

		HeightTransition( ZoneNum ) = HeightFrac * CeilingHeight;
		HeightMixedSubzoneAve = ( CeilingHeight + HeightTransition( ZoneNum ) ) / 2.0;
		HeightOccupiedSubzoneAve = ( HeightFloorSubzoneTop + HeightTransition( ZoneNum ) ) / 2.0;
		HeightFloorSubzoneAve = HeightFloorSubzoneTop / 2.0;

		// Comfort temperature

		if ( MIXFLAG ) {
			TCMF( ZoneNum ) = ZTAveraged;
		} else {
			if ( HeightComfort >= 0.0 && HeightComfort < HeightFloorSubzoneAve ) {
				ShowWarningError( "Displacement ventilation comfort height is in floor subzone in Zone: " + Zone( ZoneNum ).Name );
				TCMF( ZoneNum ) = ZTFloor( ZoneNum );
			} else if ( HeightComfort >= HeightFloorSubzoneAve && HeightComfort < HeightOccupiedSubzoneAve ) {
				TCMF( ZoneNum ) = ( ZTFloor( ZoneNum ) * ( HeightOccupiedSubzoneAve - HeightComfort ) + ZTOC( ZoneNum ) * ( HeightComfort - HeightFloorSubzoneAve ) ) / ( HeightOccupiedSubzoneAve - HeightFloorSubzoneAve );
				//!      TCMF(ZoneNum) = (ZTFloor(ZoneNum) * (HeightOccupiedSubzoneAve - HeightComfort) &
				//!                    + ZTMX(ZoneNum) * (HeightComfort - HeightFloorSubzoneAve)) &
				//!                    / (HeightOccupiedSubzoneAve - HeightFloorSubzoneAve)
			} else if ( HeightComfort >= HeightOccupiedSubzoneAve && HeightComfort < HeightMixedSubzoneAve ) {
				TCMF( ZoneNum ) = ( ZTOC( ZoneNum ) * ( HeightMixedSubzoneAve - HeightComfort ) + ZTMX( ZoneNum ) * ( HeightComfort - HeightOccupiedSubzoneAve ) ) / ( HeightMixedSubzoneAve - HeightOccupiedSubzoneAve );
			} else if ( HeightComfort >= HeightMixedSubzoneAve && HeightComfort <= CeilingHeight ) {
				TCMF( ZoneNum ) = ZTMX( ZoneNum );
			} else {
				ShowFatalError( "Displacement ventilation comfort height is above ceiling or below floor in Zone: " + Zone( ZoneNum ).Name );
			}
		}

		// Temperature at the thermostat/temperature control sensor

		if ( MIXFLAG ) {
			TempTstatAir( ZoneNum ) = ZTAveraged;
		} else {
			if ( HeightThermostat >= 0.0 && HeightThermostat < HeightFloorSubzoneAve ) {
				ShowWarningError( "Displacement thermostat is in floor subzone in Zone: " + Zone( ZoneNum ).Name );
				TempTstatAir( ZoneNum ) = ZTFloor( ZoneNum );
			} else if ( HeightThermostat >= HeightFloorSubzoneAve && HeightThermostat < HeightOccupiedSubzoneAve ) {
				TempTstatAir( ZoneNum ) = ( ZTFloor( ZoneNum ) * ( HeightOccupiedSubzoneAve - HeightThermostat ) + ZTOC( ZoneNum ) * ( HeightThermostat - HeightFloorSubzoneAve ) ) / ( HeightOccupiedSubzoneAve - HeightFloorSubzoneAve );
				//!      TempTstatAir(ZoneNum) = (ZTFloor(ZoneNum) * (HeightOccupiedSubzoneAve - HeightThermostat) &
				//!                    + ZTMX(ZoneNum) * (HeightThermostat - HeightFloorSubzoneAve)) &
				//!                    / (HeightOccupiedSubzoneAve - HeightFloorSubzoneAve)
			} else if ( HeightThermostat >= HeightOccupiedSubzoneAve && HeightThermostat < HeightMixedSubzoneAve ) {
				TempTstatAir( ZoneNum ) = ( ZTOC( ZoneNum ) * ( HeightMixedSubzoneAve - HeightThermostat ) + ZTMX( ZoneNum ) * ( HeightThermostat - HeightOccupiedSubzoneAve ) ) / ( HeightMixedSubzoneAve - HeightOccupiedSubzoneAve );
			} else if ( HeightThermostat >= HeightMixedSubzoneAve && HeightThermostat <= CeilingHeight ) {
				TempTstatAir( ZoneNum ) = ZTMX( ZoneNum );
			} else {
				ShowFatalError( "Displacement ventilation thermostat height is above ceiling or below floor in Zone: " + Zone( ZoneNum ).Name );
			}
		}

		// Temperature gradients

		if ( ( HeightMixedSubzoneAve - HeightFloorSubzoneAve ) > 0.1 ) {
			AvgTempGrad( ZoneNum ) = ( ZTMX( ZoneNum ) - ZTFloor( ZoneNum ) ) / ( HeightMixedSubzoneAve - HeightFloorSubzoneAve );
		} else {
			AvgTempGrad( ZoneNum ) = -9.999;
		}
		if ( ( HeightOccupiedSubzoneAve - HeightFloorSubzoneAve ) > 0.1 ) {
			MaxTempGrad( ZoneNum ) = ( ZTOC( ZoneNum ) - ZTFloor( ZoneNum ) ) / ( HeightOccupiedSubzoneAve - HeightFloorSubzoneAve );
		} else {
			MaxTempGrad( ZoneNum ) = -9.999;
		}
		if ( ( HeightMixedSubzoneAve - HeightOccupiedSubzoneAve ) > 0.1 ) {
			MTGAUX = ( ZTMX( ZoneNum ) - ZTOC( ZoneNum ) ) / ( HeightMixedSubzoneAve - HeightOccupiedSubzoneAve );
		} else {
			MTGAUX = -9.999;
		}

		if ( MTGAUX > MaxTempGrad( ZoneNum ) ) {
			MaxTempGrad( ZoneNum ) = MTGAUX;
		}

		if ( MIXFLAG ) {
			ZoneDVMixedFlag( ZoneNum ) = 1;
			AirModel( ZoneNum ).SimAirModel = false;
		} else {
			ZoneDVMixedFlag( ZoneNum ) = 0;
			AirModel( ZoneNum ).SimAirModel = true;
		}

		if ( ZoneEquipConfig( ZoneNum ).IsControlled ) {
			ZoneNodeNum = Zone( ZoneNum ).SystemZoneNodeNumber;
			Node( ZoneNodeNum ).Temp = ZTMX( ZoneNum );
		}

		// Mixed for reporting purposes
		if ( ( MIXFLAG ) || ( ( ZTMX( ZoneNum ) - ZTOC( ZoneNum ) ) < TempDiffCritRep ) ) {
			ZoneDVMixedFlagRep( ZoneNum ) = 1.0;
			FracMinFlow( ZoneNum ) = -1.0;
			HeightTransition( ZoneNum ) = -9.999;
			AvgTempGrad( ZoneNum ) = -9.999;
			MaxTempGrad( ZoneNum ) = -9.999;
		} else {
			ZoneDVMixedFlagRep( ZoneNum ) = 0.0;
		}

	}

} // DisplacementVentMgr

} // EnergyPlus
