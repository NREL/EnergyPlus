// C++ Headers
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/MArray.functions.hh>

// EnergyPlus Headers
#include <CrossVentMgr.hh>
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

namespace CrossVentMgr {

	// MODULE INFORMATION:
	//       AUTHOR         G. Carrilho da Graca
	//       DATE WRITTEN   October 2004
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// Routines that implement the UCSD Cross Ventilation

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
	using namespace DataUCSDSharedData;
	using namespace DataAirflowNetwork;

	// Data
	// MODULE PARAMETER DEFINITIONS:

	// DERIVED TYPE DEFINITIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:
	Real64 HAT_J( 0.0 ); // HAT_J Convection Coefficient times Area times Temperature for Jet subzone
	Real64 HA_J( 0.0 ); // HA_J  Convection Coefficient times Area for Jet subzone
	Real64 HAT_R( 0.0 ); // HAT_R Convection Coefficient times Area times Temperature for Recirculation subzone
	Real64 HA_R( 0.0 ); // HA_J  Convection Coefficient times Area for Recirculation subzone
	Real64 const Cjet1( 1.873 ); // First correlation constant for the jet velocity
	Real64 const Cjet2( 0.243 ); // Second correlation constant for the jet velocity
	Real64 const Crec1( 0.591 ); // First correlation constant for the recirculation velocity
	Real64 const Crec2( 0.070 ); // Second correlation constant for the recirculation velocity
	Real64 const CjetTemp( 0.849 ); // Correlation constant for the jet temperature rise
	Real64 const CrecTemp( 1.385 ); // Correlation constant for the recirculation temperature rise
	Real64 const CrecFlow1( 0.415 ); // First correlation constant for the recirculation flow rate
	Real64 const CrecFlow2( 0.466 ); // Second correlation constant for the recirculation flow rate

	// SUBROUTINE SPECIFICATIONS:

	// Functions

	void
	ManageUCSDCVModel( int const ZoneNum ) // index number for the specified zone
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         G. Carrilho da Graca
		//       DATE WRITTEN   October 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		//   manage the UCSD Cross Ventilation model

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

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// FLOW:

		// initialize Cross Ventilation model

		InitUCSDCV( ZoneNum );

		// perform Cross Ventilation model calculations
		CalcUCSDCV( ZoneNum );

	}

	//**************************************************************************************************

	void
	InitUCSDCV( int const ZoneNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         G. Carrilho da Graca
		//       DATE WRITTEN   October 2004
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

		// Using/Aliasing
		using namespace DataRoomAirModel;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		static bool MyOneTimeFlag( true );
		static Array1D_bool MyEnvrnFlag;

		// Do the one time initializations
		if ( MyOneTimeFlag ) {
			MyEnvrnFlag.dimension( NumOfZones, true );
			MyOneTimeFlag = false;
		}

		// Do the begin environment initializations
		if ( BeginEnvrnFlag && MyEnvrnFlag( ZoneNum ) ) {
			MyEnvrnFlag( ZoneNum ) = false;
		}

		if ( ! BeginEnvrnFlag ) {
			MyEnvrnFlag( ZoneNum ) = true;
		}

	}

	//**************************************************************************************************

	void
	HcUCSDCV( int const ZoneNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         G. Carrilho da Graca
		//       DATE WRITTEN   October 2004
		//       MODIFIED       8/2013 - Sam Brunswick
		//                      To improve convection coefficient calculation
		//       RE-ENGINEERED  -

		// PURPOSE OF THIS SUBROUTINE:
		// Main subroutine for convection calculation in the UCSD Cross Ventilation model.
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
		using ScheduleManager::GetScheduleIndex; // , GetDayScheduleValues
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
		int Ctd; // DO loop counter for surfaces
		int SurfNum; // Surface number
		Real64 Hjet;
		Real64 Hrec;

		// Initialize HAT and HA
		HAT_J = 0.0;
		HAT_R = 0.0;
		HA_J = 0.0;
		HA_R = 0.0;

		// Is the air flow model for this zone set to UCSDCV Cross Ventilation?
		if ( IsZoneCV( ZoneNum ) ) {
			// WALL Hc, HA and HAT calculation
			for ( Ctd = PosZ_Wall( ( ZoneNum - 1 ) * 2 + 1 ); Ctd <= PosZ_Wall( ( ZoneNum - 1 ) * 2 + 2 ); ++Ctd ) {
				SurfNum = APos_Wall( Ctd );
				Surface( SurfNum ).TAirRef = AdjacentAirTemp;
				if ( SurfNum == 0 ) continue;
				TempEffBulkAir( SurfNum ) = ZTREC( ZoneNum );
				CalcDetailedHcInForDVModel( SurfNum, TempSurfIn, CVHcIn, Urec );
				HWall( Ctd ) = CVHcIn( SurfNum );
				HAT_R += Surface( SurfNum ).Area * TempSurfIn( SurfNum ) * HWall( Ctd );
				HA_R += Surface( SurfNum ).Area * HWall( Ctd );
			} // END WALL
			// WINDOW Hc, HA and HAT CALCULATION
			for ( Ctd = PosZ_Window( ( ZoneNum - 1 ) * 2 + 1 ); Ctd <= PosZ_Window( ( ZoneNum - 1 ) * 2 + 2 ); ++Ctd ) {
				SurfNum = APos_Window( Ctd );
				Surface( SurfNum ).TAirRef = AdjacentAirTemp;
				if ( SurfNum == 0 ) continue;
				if ( Surface( SurfNum ).Tilt > 10.0 && Surface( SurfNum ).Tilt < 170.0 ) { // Window Wall
					TempEffBulkAir( SurfNum ) = ZTREC( ZoneNum );
					CalcDetailedHcInForDVModel( SurfNum, TempSurfIn, CVHcIn, Urec );
					HWindow( Ctd ) = CVHcIn( SurfNum );
					HAT_R += Surface( SurfNum ).Area * TempSurfIn( SurfNum ) * HWindow( Ctd );
					HA_R += Surface( SurfNum ).Area * HWindow( Ctd );
				}
				if ( Surface( SurfNum ).Tilt <= 10.0 ) { // Window Ceiling
					TempEffBulkAir( SurfNum ) = ZTJET( ZoneNum );
					CalcDetailedHcInForDVModel( SurfNum, TempSurfIn, CVHcIn, Ujet );
					Hjet = CVHcIn( SurfNum );
					TempEffBulkAir( SurfNum ) = ZTREC( ZoneNum );
					CalcDetailedHcInForDVModel( SurfNum, TempSurfIn, CVHcIn, Urec );
					Hrec = CVHcIn( SurfNum );
					HWindow( Ctd ) = JetRecAreaRatio( ZoneNum ) * Hjet + ( 1 - JetRecAreaRatio( ZoneNum ) ) * Hrec;
					HAT_R += Surface( SurfNum ).Area * ( 1.0 - JetRecAreaRatio( ZoneNum ) ) * TempSurfIn( SurfNum ) * Hrec;
					HA_R += Surface( SurfNum ).Area * ( 1.0 - JetRecAreaRatio( ZoneNum ) ) * Hrec;
					HAT_J += Surface( SurfNum ).Area * JetRecAreaRatio( ZoneNum ) * TempSurfIn( SurfNum ) * Hjet;
					HA_J += Surface( SurfNum ).Area * JetRecAreaRatio( ZoneNum ) * Hjet;
					TempEffBulkAir( SurfNum ) = JetRecAreaRatio( ZoneNum ) * ZTJET( ZoneNum ) + ( 1 - JetRecAreaRatio( ZoneNum ) ) * ZTREC( ZoneNum );
				}
				if ( Surface( SurfNum ).Tilt >= 170.0 ) { // Window Floor
					TempEffBulkAir( SurfNum ) = ZTJET( ZoneNum );
					CalcDetailedHcInForDVModel( SurfNum, TempSurfIn, CVHcIn, Ujet );
					Hjet = CVHcIn( SurfNum );
					TempEffBulkAir( SurfNum ) = ZTREC( ZoneNum );
					CalcDetailedHcInForDVModel( SurfNum, TempSurfIn, CVHcIn, Urec );
					Hrec = CVHcIn( SurfNum );
					HWindow( Ctd ) = JetRecAreaRatio( ZoneNum ) * Hjet + ( 1 - JetRecAreaRatio( ZoneNum ) ) * Hrec;
					HAT_R += Surface( SurfNum ).Area * ( 1.0 - JetRecAreaRatio( ZoneNum ) ) * TempSurfIn( SurfNum ) * Hrec;
					HA_R += Surface( SurfNum ).Area * ( 1.0 - JetRecAreaRatio( ZoneNum ) ) * Hrec;
					HAT_J += Surface( SurfNum ).Area * JetRecAreaRatio( ZoneNum ) * TempSurfIn( SurfNum ) * Hjet;
					HA_J += Surface( SurfNum ).Area * JetRecAreaRatio( ZoneNum ) * Hjet;
					TempEffBulkAir( SurfNum ) = JetRecAreaRatio( ZoneNum ) * ZTJET( ZoneNum ) + ( 1 - JetRecAreaRatio( ZoneNum ) ) * ZTREC( ZoneNum );
				}
				CVHcIn( SurfNum ) = HWindow( Ctd );
			} // END WINDOW
			// DOOR Hc, HA and HAT CALCULATION
			for ( Ctd = PosZ_Door( ( ZoneNum - 1 ) * 2 + 1 ); Ctd <= PosZ_Door( ( ZoneNum - 1 ) * 2 + 2 ); ++Ctd ) { // DOOR
				SurfNum = APos_Door( Ctd );
				Surface( SurfNum ).TAirRef = AdjacentAirTemp;
				if ( SurfNum == 0 ) continue;
				TempEffBulkAir( SurfNum ) = ZTREC( ZoneNum );
				CalcDetailedHcInForDVModel( SurfNum, TempSurfIn, CVHcIn, Urec );
				HDoor( Ctd ) = CVHcIn( SurfNum );
				HAT_R += Surface( SurfNum ).Area * TempSurfIn( SurfNum ) * HDoor( Ctd );
				HA_R += Surface( SurfNum ).Area * HDoor( Ctd );
			} // END DOOR
			// INTERNAL Hc, HA and HAT CALCULATION
			for ( Ctd = PosZ_Internal( ( ZoneNum - 1 ) * 2 + 1 ); Ctd <= PosZ_Internal( ( ZoneNum - 1 ) * 2 + 2 ); ++Ctd ) {
				SurfNum = APos_Internal( Ctd );
				Surface( SurfNum ).TAirRef = AdjacentAirTemp;
				if ( SurfNum == 0 ) continue;
				TempEffBulkAir( SurfNum ) = ZTREC( ZoneNum );
				CalcDetailedHcInForDVModel( SurfNum, TempSurfIn, CVHcIn, Urec );
				HInternal( Ctd ) = CVHcIn( SurfNum );
				HAT_R += Surface( SurfNum ).Area * TempSurfIn( SurfNum ) * HInternal( Ctd );
				HA_R += Surface( SurfNum ).Area * HInternal( Ctd );
			} // END INTERNAL

			// CEILING Hc, HA and HAT CALCULATION
			for ( Ctd = PosZ_Ceiling( ( ZoneNum - 1 ) * 2 + 1 ); Ctd <= PosZ_Ceiling( ( ZoneNum - 1 ) * 2 + 2 ); ++Ctd ) {
				SurfNum = APos_Ceiling( Ctd );
				Surface( SurfNum ).TAirRef = AdjacentAirTemp;
				if ( SurfNum == 0 ) continue;
				TempEffBulkAir( SurfNum ) = ZTJET( ZoneNum );
				CalcDetailedHcInForDVModel( SurfNum, TempSurfIn, CVHcIn, Ujet );
				Hjet = CVHcIn( SurfNum );
				TempEffBulkAir( SurfNum ) = ZTREC( ZoneNum );
				CalcDetailedHcInForDVModel( SurfNum, TempSurfIn, CVHcIn, Urec );
				Hrec = CVHcIn( SurfNum );
				HCeiling( Ctd ) = JetRecAreaRatio( ZoneNum ) * Hjet + ( 1 - JetRecAreaRatio( ZoneNum ) ) * Hrec;
				HAT_R += Surface( SurfNum ).Area * ( 1 - JetRecAreaRatio( ZoneNum ) ) * TempSurfIn( SurfNum ) * Hrec;
				HA_R += Surface( SurfNum ).Area * ( 1 - JetRecAreaRatio( ZoneNum ) ) * Hrec;
				HAT_J += Surface( SurfNum ).Area * JetRecAreaRatio( ZoneNum ) * TempSurfIn( SurfNum ) * Hjet;
				HA_J += Surface( SurfNum ).Area * JetRecAreaRatio( ZoneNum ) * Hjet;
				TempEffBulkAir( SurfNum ) = JetRecAreaRatio( ZoneNum ) * ZTJET( ZoneNum ) + ( 1 - JetRecAreaRatio( ZoneNum ) ) * ZTREC( ZoneNum );
				CVHcIn( SurfNum ) = HCeiling( Ctd );
			} // END CEILING
			// FLOOR Hc, HA and HAT CALCULATION
			for ( Ctd = PosZ_Floor( ( ZoneNum - 1 ) * 2 + 1 ); Ctd <= PosZ_Floor( ( ZoneNum - 1 ) * 2 + 2 ); ++Ctd ) {
				SurfNum = APos_Floor( Ctd );
				Surface( SurfNum ).TAirRef = AdjacentAirTemp;
				if ( SurfNum == 0 ) continue;
				TempEffBulkAir( SurfNum ) = ZTJET( ZoneNum );
				CalcDetailedHcInForDVModel( SurfNum, TempSurfIn, CVHcIn, Ujet );
				Hjet = CVHcIn( SurfNum );
				TempEffBulkAir( SurfNum ) = ZTREC( ZoneNum );
				CalcDetailedHcInForDVModel( SurfNum, TempSurfIn, CVHcIn, Urec );
				Hrec = CVHcIn( SurfNum );
				HFloor( Ctd ) = JetRecAreaRatio( ZoneNum ) * Hjet + ( 1 - JetRecAreaRatio( ZoneNum ) ) * Hrec;
				HAT_R += Surface( SurfNum ).Area * ( 1 - JetRecAreaRatio( ZoneNum ) ) * TempSurfIn( SurfNum ) * Hrec;
				HA_R += Surface( SurfNum ).Area * ( 1 - JetRecAreaRatio( ZoneNum ) ) * Hrec;
				HAT_J += Surface( SurfNum ).Area * JetRecAreaRatio( ZoneNum ) * TempSurfIn( SurfNum ) * Hjet;
				HA_J += Surface( SurfNum ).Area * JetRecAreaRatio( ZoneNum ) * Hjet;
				TempEffBulkAir( SurfNum ) = JetRecAreaRatio( ZoneNum ) * ZTJET( ZoneNum ) + ( 1 - JetRecAreaRatio( ZoneNum ) ) * ZTREC( ZoneNum );
				CVHcIn( SurfNum ) = HFloor( Ctd );
			} // END FLOOR
		}

	}

	//**************************************************************************************************

	void
	EvolveParaUCSDCV( int const ZoneNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         G. Carrilho da Graca
		//       DATE WRITTEN   October 2004
		//       MODIFIED       8/2013 - Sam Brunswick
		//                      To incorporate an improved model
		//                      and add modeling of multiple jets
		//       RE-ENGINEERED  -

		// PURPOSE OF THIS SUBROUTINE:
		// Subroutine for parameter actualization in the UCSD Cross Ventilation model.

		// METHODOLOGY EMPLOYED:
		// -
		// -
		// -
		// -

		// REFERENCES:
		// -
		// -

		// Using/Aliasing
		using namespace Psychrometrics;
		using namespace DataHeatBalFanSys;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		Real64 const MinUin( 0.2 );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Ctd; // counter
		int Ctd2; // counter
		int OPtr; // counter
		static Real64 Aroom; // Room area cross section
		static Real64 Wroom; // Room width
		Real64 Uin; // Inflow air velocity [m/s]
		Real64 CosPhi; // Angle (in degrees) between the wind and the outward normal of the dominant surface
		Real64 SurfNorm; // Outward normal of surface
		Real64 SumToZone; // Sum of velocities through
		Real64 MaxFlux;
		int MaxSurf;
		Real64 XX;
		Real64 YY;
		Real64 ZZ;
		Real64 XX_Wall;
		Real64 YY_Wall;
		Real64 ZZ_Wall;
		Real64 ActiveSurfNum;
		int NSides; // Number of sides in surface
		static int CompNum( 0 ); // AirflowNetwork Component number
		static int TypeNum( 0 ); // Airflownetwork Type Number within a component
		static int NodeNum1( 0 ); // The first node number in an AirflowNetwork linkage data
		static int NodeNum2( 0 ); // The Second node number in an AirflowNetwork linkage data

		MaxSurf = 0;
		SumToZone = 0.0;
		MaxFlux = 0.0;
		RecInflowRatio( ZoneNum ) = 0.0;

		// Identify the dominant aperture:
		MaxSurf = AirflowNetworkSurfaceUCSDCV( 1, ZoneNum );
		if ( Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).Zone == ZoneNum ) {
			// this is a direct airflow network aperture
			SumToZone = AirflowNetworkLinkSimu( AirflowNetworkSurfaceUCSDCV( 1, ZoneNum ) ).VolFLOW2;
			MaxFlux = AirflowNetworkLinkSimu( AirflowNetworkSurfaceUCSDCV( 1, ZoneNum ) ).VolFLOW2;
		} else {
			// this is an indirect airflow network aperture
			SumToZone = AirflowNetworkLinkSimu( AirflowNetworkSurfaceUCSDCV( 1, ZoneNum ) ).VolFLOW;
			MaxFlux = AirflowNetworkLinkSimu( AirflowNetworkSurfaceUCSDCV( 1, ZoneNum ) ).VolFLOW;
		}

		for ( Ctd2 = 2; Ctd2 <= AirflowNetworkSurfaceUCSDCV( 0, ZoneNum ); ++Ctd2 ) {
			if ( Surface( MultizoneSurfaceData( AirflowNetworkSurfaceUCSDCV( Ctd2, ZoneNum ) ).SurfNum ).Zone == ZoneNum ) {
				if ( AirflowNetworkLinkSimu( AirflowNetworkSurfaceUCSDCV( Ctd2, ZoneNum ) ).VolFLOW2 > MaxFlux ) {
					MaxFlux = AirflowNetworkLinkSimu( AirflowNetworkSurfaceUCSDCV( Ctd2, ZoneNum ) ).VolFLOW2;
					MaxSurf = AirflowNetworkSurfaceUCSDCV( Ctd2, ZoneNum );
				}
				SumToZone += AirflowNetworkLinkSimu( AirflowNetworkSurfaceUCSDCV( Ctd2, ZoneNum ) ).VolFLOW2;
			} else {
				if ( AirflowNetworkLinkSimu( AirflowNetworkSurfaceUCSDCV( Ctd2, ZoneNum ) ).VolFLOW > MaxFlux ) {
					MaxFlux = AirflowNetworkLinkSimu( AirflowNetworkSurfaceUCSDCV( Ctd2, ZoneNum ) ).VolFLOW;
					MaxSurf = AirflowNetworkSurfaceUCSDCV( Ctd2, ZoneNum );
				}
				SumToZone += AirflowNetworkLinkSimu( AirflowNetworkSurfaceUCSDCV( Ctd2, ZoneNum ) ).VolFLOW;
			}
		}

		// Check if wind direction is within +/- 90 degrees of the outward normal of the dominant surface
		SurfNorm = Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).Azimuth;
		CosPhi = std::cos( ( WindDir - SurfNorm ) * DegToRadians );
		if ( CosPhi <= 0 ) {
			AirModel( ZoneNum ).SimAirModel = false;
			CVJetRecFlows( _, ZoneNum ).Ujet() = 0.0;
			CVJetRecFlows( _, ZoneNum ).Urec() = 0.0;
			Urec( ZoneNum ) = 0.0;
			Ujet( ZoneNum ) = 0.0;
			Qrec( ZoneNum ) = 0.0;
			if ( Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).ExtBoundCond > 0 ) {
				Tin( ZoneNum ) = MAT( Surface( Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).ExtBoundCond ).Zone );
			} else if ( Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).ExtBoundCond == ExternalEnvironment ) {
				Tin( ZoneNum ) = Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).OutDryBulbTemp;
			} else if ( Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).ExtBoundCond == Ground ) {
				Tin( ZoneNum ) = Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).OutDryBulbTemp;
			} else if ( Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).ExtBoundCond == OtherSideCoefNoCalcExt || Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).ExtBoundCond == OtherSideCoefCalcExt ) {
				OPtr = Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).OSCPtr;
				OSC( OPtr ).OSCTempCalc = ( OSC( OPtr ).ZoneAirTempCoef * MAT( ZoneNum ) + OSC( OPtr ).ExtDryBulbCoef * Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).OutDryBulbTemp + OSC( OPtr ).ConstTempCoef * OSC( OPtr ).ConstTemp + OSC( OPtr ).GroundTempCoef * GroundTemp + OSC( OPtr ).WindSpeedCoef * Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).WindSpeed * Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).OutDryBulbTemp );
				Tin( ZoneNum ) = OSC( OPtr ).OSCTempCalc;
			} else {
				Tin( ZoneNum ) = Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).OutDryBulbTemp;
			}
			return;
		}

		// Calculate the opening area for all apertures
		for ( Ctd = 1; Ctd <= AirflowNetworkSurfaceUCSDCV( 0, ZoneNum ); ++Ctd ) {
			CompNum = AirflowNetworkLinkageData( Ctd ).CompNum;
			TypeNum = AirflowNetworkCompData( CompNum ).TypeNum;
			if ( AirflowNetworkCompData( CompNum ).CompTypeNum == CompTypeNum_DOP ) {
				CVJetRecFlows( Ctd, ZoneNum ).Area = SurfParametersCVDV( Ctd ).Width * SurfParametersCVDV( Ctd ).Height * MultizoneSurfaceData( Ctd ).OpenFactor;
			} else if ( AirflowNetworkCompData( CompNum ).CompTypeNum == CompTypeNum_SCR ) {
				CVJetRecFlows( Ctd, ZoneNum ).Area = SurfParametersCVDV( Ctd ).Width * SurfParametersCVDV( Ctd ).Height;
			} else {
				ShowSevereError( "RoomAirModelCrossVent:EvolveParaUCSDCV: Illegal leakage component referenced in the cross ventilation room air model" );
				ShowContinueError( "Surface " + AirflowNetworkLinkageData( Ctd ).Name + " in zone " + Zone( ZoneNum ).Name + " uses leakage component " + AirflowNetworkLinkageData( Ctd ).CompName );
				ShowContinueError( "Only leakage component types AirflowNetwork:MultiZone:Component:DetailedOpening and " );
				ShowContinueError( "AirflowNetwork:MultiZone:Surface:Crack can be used with the cross ventilation room air model" );
				ShowFatalError( "Previous severe error causes program termination" );
			}
		}

		// Calculate Droom, Wroom, Dstar
		// Droom the distance between the average point of the base surface of the airflow network Surface (if the base surface
		// is a Window or Door it looks for the second base surface).
		// Dstar is Droom corrected for wind angle
		Wroom = Zone( ZoneNum ).Volume / Zone( ZoneNum ).FloorArea;
		if ( ( Surface( Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).BaseSurf ).Sides == 3 ) || ( Surface( Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).BaseSurf ).Sides == 4 ) ) {
			XX = Surface( Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).BaseSurf ).Centroid.x;
			YY = Surface( Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).BaseSurf ).Centroid.y;
			ZZ = Surface( Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).BaseSurf ).Centroid.z;
		} else {
			// If the surface has more than 4 vertex then average the vertex coordinates in X, Y and Z.
			NSides = Surface( Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).BaseSurf ).Sides;
			XX = sum( Surface( Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).BaseSurf ).Vertex( {1,NSides} ).x() ) / double( NSides );
			YY = sum( Surface( Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).BaseSurf ).Vertex( {1,NSides} ).y() ) / double( NSides );
			ZZ = sum( Surface( Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).BaseSurf ).Vertex( {1,NSides} ).z() ) / double( NSides );
		}

		Real64 const Wroom_2( pow_2( Wroom ) );
		for ( Ctd = PosZ_Wall( 2 * ZoneNum - 1 ); Ctd <= PosZ_Wall( 2 * ZoneNum ); ++Ctd ) {
			if ( ( Surface( APos_Wall( Ctd ) ).Sides == 3 ) || ( Surface( APos_Wall( Ctd ) ).Sides == 4 ) ) {
				XX_Wall = Surface( APos_Wall( Ctd ) ).Centroid.x;
				YY_Wall = Surface( APos_Wall( Ctd ) ).Centroid.y;
				ZZ_Wall = Surface( APos_Wall( Ctd ) ).Centroid.z;
			} else {
				NSides = Surface( APos_Wall( Ctd ) ).Sides;
				XX_Wall = sum( Surface( APos_Wall( Ctd ) ).Vertex( {1,NSides} ).x() ) / double( NSides );
				YY_Wall = sum( Surface( APos_Wall( Ctd ) ).Vertex( {1,NSides} ).y() ) / double( NSides );
				ZZ_Wall = sum( Surface( APos_Wall( Ctd ) ).Vertex( {1,NSides} ).z() ) / double( NSides );
			}
			auto DroomTemp = std::sqrt( pow_2( XX - XX_Wall ) + pow_2( YY - YY_Wall ) + pow_2( ZZ - ZZ_Wall ) );
			if ( DroomTemp > Droom( ZoneNum ) ) {
				Droom( ZoneNum ) = DroomTemp;
			}
			Dstar( ZoneNum ) = min( Droom( ZoneNum ) / CosPhi, std::sqrt( Wroom_2 + pow_2( Droom( ZoneNum ) ) ) );
		}

		// Room area
		Aroom = Zone( ZoneNum ).Volume / Droom( ZoneNum );

		//Populate an array of inflow volume fluxes (Fin) for all apertures in the zone
		//Calculate inflow velocity (%Uin) for each aperture in the zone
		for ( Ctd = 1; Ctd <= AirflowNetworkSurfaceUCSDCV( 0, ZoneNum ); ++Ctd ) {
			if ( Surface( MultizoneSurfaceData( Ctd ).SurfNum ).Zone == ZoneNum ) {
				// this is a direct airflow network aperture
				CVJetRecFlows( Ctd, ZoneNum ).Fin = AirflowNetworkLinkSimu( AirflowNetworkSurfaceUCSDCV( Ctd, ZoneNum ) ).VolFLOW2;
			} else {
				// this is an indirect airflow network aperture
				CVJetRecFlows( Ctd, ZoneNum ).Fin = AirflowNetworkLinkSimu( AirflowNetworkSurfaceUCSDCV( Ctd, ZoneNum ) ).VolFLOW;
			}
			if ( CVJetRecFlows( Ctd, ZoneNum ).Area != 0 ) {
				CVJetRecFlows( Ctd, ZoneNum ).Uin = CVJetRecFlows( Ctd, ZoneNum ).Fin / CVJetRecFlows( Ctd, ZoneNum ).Area;
			} else {
				CVJetRecFlows( Ctd, ZoneNum ).Uin = 0.0;
			}
		}

		// Verify if Uin is higher than minimum for each aperture
		// Create a flow flag for each aperture
		// Calculate the total area of all active apertures
		ActiveSurfNum = 0.0;
		Ain( ZoneNum ) = 0.0;
		for ( Ctd = 1; Ctd <= AirflowNetworkSurfaceUCSDCV( 0, ZoneNum ); ++Ctd ) {
			if ( CVJetRecFlows( Ctd, ZoneNum ).Uin <= MinUin ) {
				CVJetRecFlows( Ctd, ZoneNum ).FlowFlag = 0;
			} else {
				CVJetRecFlows( Ctd, ZoneNum ).FlowFlag = 1;
			}
			ActiveSurfNum += CVJetRecFlows( Ctd, ZoneNum ).FlowFlag;
			Ain( ZoneNum ) += CVJetRecFlows( Ctd, ZoneNum ).Area * CVJetRecFlows( Ctd, ZoneNum ).FlowFlag;
		}

		// Verify if any of the apertures have minimum flow
		if ( ActiveSurfNum == 0 ) {
			AirModel( ZoneNum ).SimAirModel = false;
			if ( Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).ExtBoundCond > 0 ) {
				Tin( ZoneNum ) = MAT( Surface( Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).ExtBoundCond ).Zone );
			} else if ( Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).ExtBoundCond == ExternalEnvironment ) {
				Tin( ZoneNum ) = Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).OutDryBulbTemp;
			} else if ( Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).ExtBoundCond == Ground ) {
				Tin( ZoneNum ) = Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).OutDryBulbTemp;
			} else if ( Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).ExtBoundCond == OtherSideCoefNoCalcExt || Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).ExtBoundCond == OtherSideCoefCalcExt ) {
				OPtr = Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).OSCPtr;
				OSC( OPtr ).OSCTempCalc = ( OSC( OPtr ).ZoneAirTempCoef * MAT( ZoneNum ) + OSC( OPtr ).ExtDryBulbCoef * Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).OutDryBulbTemp + OSC( OPtr ).ConstTempCoef * OSC( OPtr ).ConstTemp + OSC( OPtr ).GroundTempCoef * GroundTemp + OSC( OPtr ).WindSpeedCoef * Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).WindSpeed * Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).OutDryBulbTemp );
				Tin( ZoneNum ) = OSC( OPtr ).OSCTempCalc;
			} else {
				Tin( ZoneNum ) = Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).OutDryBulbTemp;
			}
			Urec( ZoneNum ) = 0.0;
			Ujet( ZoneNum ) = 0.0;
			Qrec( ZoneNum ) = 0.0;
			CVJetRecFlows( _, ZoneNum ).Ujet() = 0.0;
			CVJetRecFlows( _, ZoneNum ).Urec() = 0.0;
			return;
		}

		// Calculate Uin, the area weighted average velocity of all the active apertures in the zone
		// Calculate Qtot, the total volumetric flow rate through all active openings in the zone
		Uin = 0.0;

		for ( Ctd = 1; Ctd <= AirflowNetworkSurfaceUCSDCV( 0, ZoneNum ); ++Ctd ) {
			Uin += CVJetRecFlows( Ctd, ZoneNum ).Area * CVJetRecFlows( Ctd, ZoneNum ).Uin * CVJetRecFlows( Ctd, ZoneNum ).FlowFlag / Ain( ZoneNum );
		}

		//Verify if Uin is higher than minimum:
		if ( Uin < MinUin ) {
			AirModel( ZoneNum ).SimAirModel = false;
			Urec( ZoneNum ) = 0.0;
			Ujet( ZoneNum ) = 0.0;
			Qrec( ZoneNum ) = 0.0;
			RecInflowRatio( ZoneNum ) = 0.0;
			CVJetRecFlows( _, ZoneNum ).Ujet() = 0.0;
			CVJetRecFlows( _, ZoneNum ).Urec() = 0.0;
			if ( Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).ExtBoundCond > 0 ) {
				Tin( ZoneNum ) = MAT( Surface( Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).ExtBoundCond ).Zone );
			} else if ( Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).ExtBoundCond == ExternalEnvironment ) {
				Tin( ZoneNum ) = Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).OutDryBulbTemp;
			} else if ( Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).ExtBoundCond == Ground ) {
				Tin( ZoneNum ) = Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).OutDryBulbTemp;
			} else if ( Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).ExtBoundCond == OtherSideCoefNoCalcExt || Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).ExtBoundCond == OtherSideCoefCalcExt ) {
				OPtr = Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).OSCPtr;
				OSC( OPtr ).OSCTempCalc = ( OSC( OPtr ).ZoneAirTempCoef * MAT( ZoneNum ) + OSC( OPtr ).ExtDryBulbCoef * Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).OutDryBulbTemp + OSC( OPtr ).ConstTempCoef * OSC( OPtr ).ConstTemp + OSC( OPtr ).GroundTempCoef * GroundTemp + OSC( OPtr ).WindSpeedCoef * Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).WindSpeed * Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).OutDryBulbTemp );
				Tin( ZoneNum ) = OSC( OPtr ).OSCTempCalc;

			} else {
				Tin( ZoneNum ) = Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).OutDryBulbTemp;
			}
			return;
		}

		// Evaluate parameter that determines whether recirculations are present
		for ( Ctd = 1; Ctd <= TotUCSDCV; ++Ctd ) {
			if ( ZoneNum == ZoneUCSDCV( Ctd ).ZonePtr ) {
				if ( Ain( ZoneNum ) / Aroom > 1.0 / 2.0 ) {
					JetRecAreaRatio( ZoneNum ) = 1.0;
				} else {
					JetRecAreaRatio( ZoneNum ) = std::sqrt( Ain( ZoneNum ) / Aroom );
				}
			}
		}

		AirModel( ZoneNum ).SimAirModel = true;
		// Calculate jet and recirculation velocities for all active apertures
		Ujet( ZoneNum ) = 0.0;
		Urec( ZoneNum ) = 0.0;
		Qrec( ZoneNum ) = 0.0;
		Qtot( ZoneNum ) = 0.0;
		CVJetRecFlows( _, ZoneNum ).Ujet() = 0.0;
		CVJetRecFlows( _, ZoneNum ).Urec() = 0.0;
		CVJetRecFlows( _, ZoneNum ).Qrec() = 0.0;
		for ( Ctd = 1; Ctd <= AirflowNetworkSurfaceUCSDCV( 0, ZoneNum ); ++Ctd ) {
			if ( CVJetRecFlows( Ctd, ZoneNum ).Uin != 0 ) {
				CVJetRecFlows( Ctd, ZoneNum ).Vjet = CVJetRecFlows( Ctd, ZoneNum ).Uin * std::sqrt( CVJetRecFlows( Ctd, ZoneNum ).Area ) * 6.3 * std::log( Dstar( ZoneNum ) / ( 6.0 * std::sqrt( CVJetRecFlows( Ctd, ZoneNum ).Area ) ) ) / Dstar( ZoneNum );
				CVJetRecFlows( Ctd, ZoneNum ).Yjet = Cjet1 * std::sqrt( CVJetRecFlows( Ctd, ZoneNum ).Area / Aroom ) * CVJetRecFlows( Ctd, ZoneNum ).Vjet / CVJetRecFlows( Ctd, ZoneNum ).Uin + Cjet2;
				CVJetRecFlows( Ctd, ZoneNum ).Yrec = Crec1 * std::sqrt( CVJetRecFlows( Ctd, ZoneNum ).Area / Aroom ) * CVJetRecFlows( Ctd, ZoneNum ).Vjet / CVJetRecFlows( Ctd, ZoneNum ).Uin + Crec2;
				CVJetRecFlows( Ctd, ZoneNum ).YQrec = CrecFlow1 * std::sqrt( CVJetRecFlows( Ctd, ZoneNum ).Area * Aroom ) * CVJetRecFlows( Ctd, ZoneNum ).Vjet / CVJetRecFlows( Ctd, ZoneNum ).Uin + CrecFlow2;
				CVJetRecFlows( Ctd, ZoneNum ).Ujet = CVJetRecFlows( Ctd, ZoneNum ).FlowFlag * CVJetRecFlows( Ctd, ZoneNum ).Yjet / CVJetRecFlows( Ctd, ZoneNum ).Uin;
				CVJetRecFlows( Ctd, ZoneNum ).Urec = CVJetRecFlows( Ctd, ZoneNum ).FlowFlag * CVJetRecFlows( Ctd, ZoneNum ).Yrec / CVJetRecFlows( Ctd, ZoneNum ).Uin;
				CVJetRecFlows( Ctd, ZoneNum ).Qrec = CVJetRecFlows( Ctd, ZoneNum ).FlowFlag * CVJetRecFlows( Ctd, ZoneNum ).YQrec / CVJetRecFlows( Ctd, ZoneNum ).Uin;
				Ujet( ZoneNum ) += CVJetRecFlows( Ctd, ZoneNum ).Area * CVJetRecFlows( Ctd, ZoneNum ).Ujet / Ain( ZoneNum );
				Urec( ZoneNum ) += CVJetRecFlows( Ctd, ZoneNum ).Area * CVJetRecFlows( Ctd, ZoneNum ).Urec / Ain( ZoneNum );
				Qrec( ZoneNum ) += CVJetRecFlows( Ctd, ZoneNum ).Qrec;
				Qtot( ZoneNum ) += CVJetRecFlows( Ctd, ZoneNum ).Fin * CVJetRecFlows( Ctd, ZoneNum ).FlowFlag;
				Urec( ZoneNum ) += CVJetRecFlows( Ctd, ZoneNum ).Area * CVJetRecFlows( Ctd, ZoneNum ).Urec / Ain( ZoneNum );
			}
		}

		// Ratio between recirculation flow rate and total inflow rate
		if ( Qtot( ZoneNum ) != 0 ) {
			RecInflowRatio( ZoneNum ) = Qrec( ZoneNum ) / Qtot( ZoneNum );
		} else {
			RecInflowRatio( ZoneNum ) = 0.0;
		}

		// Set Tin based on external conditions of the dominant aperture
		if ( Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).ExtBoundCond <= 0 ) {
			if ( Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).ExtBoundCond == ExternalEnvironment ) {
				Tin( ZoneNum ) = Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).OutDryBulbTemp;
			} else if ( Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).ExtBoundCond == Ground ) {
				Tin( ZoneNum ) = Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).OutDryBulbTemp;
			} else if ( Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).ExtBoundCond == OtherSideCoefNoCalcExt || Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).ExtBoundCond == OtherSideCoefCalcExt ) {
				OPtr = Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).OSCPtr;
				OSC( OPtr ).OSCTempCalc = ( OSC( OPtr ).ZoneAirTempCoef * MAT( ZoneNum ) + OSC( OPtr ).ExtDryBulbCoef * Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).OutDryBulbTemp + OSC( OPtr ).ConstTempCoef * OSC( OPtr ).ConstTemp + OSC( OPtr ).GroundTempCoef * GroundTemp + OSC( OPtr ).WindSpeedCoef * Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).WindSpeed * Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).OutDryBulbTemp );
				Tin( ZoneNum ) = OSC( OPtr ).OSCTempCalc;
			} else {
				Tin( ZoneNum ) = Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).OutDryBulbTemp;
			}
		} else {
			// adiabatic surface
			if ( MultizoneSurfaceData( MaxSurf ).SurfNum == Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).ExtBoundCond ) {
				NodeNum1 = AirflowNetworkLinkageData( MaxSurf ).NodeNums( 1 );
				NodeNum2 = AirflowNetworkLinkageData( MaxSurf ).NodeNums( 2 );
				if ( Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).Zone == ZoneNum ) {
					if ( AirflowNetworkNodeData( NodeNum1 ).EPlusZoneNum <= 0 ) {
						Tin( ZoneNum ) = Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).OutDryBulbTemp;
					} else if ( AirModel( AirflowNetworkNodeData( NodeNum1 ).EPlusZoneNum ).AirModelType == RoomAirModel_UCSDCV ) {
						Tin( ZoneNum ) = RoomOutflowTemp( AirflowNetworkNodeData( NodeNum1 ).EPlusZoneNum );
					} else {
						Tin( ZoneNum ) = MAT( AirflowNetworkNodeData( NodeNum1 ).EPlusZoneNum );
					}

				} else {

					if ( AirflowNetworkNodeData( NodeNum2 ).EPlusZoneNum <= 0 ) {
						Tin( ZoneNum ) = Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).OutDryBulbTemp;
					} else if ( AirModel( AirflowNetworkNodeData( NodeNum2 ).EPlusZoneNum ).AirModelType == RoomAirModel_UCSDCV ) {
						Tin( ZoneNum ) = RoomOutflowTemp( AirflowNetworkNodeData( NodeNum2 ).EPlusZoneNum );
					} else {
						Tin( ZoneNum ) = MAT( AirflowNetworkNodeData( NodeNum2 ).EPlusZoneNum );
					}
				}
			} else if ( ( Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).Zone == ZoneNum ) && ( AirModel( Surface( Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).ExtBoundCond ).Zone ).AirModelType == RoomAirModel_UCSDCV ) ) {
				Tin( ZoneNum ) = RoomOutflowTemp( Surface( Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).ExtBoundCond ).Zone );
			} else if ( ( Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).Zone != ZoneNum ) && ( AirModel( Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).Zone ).AirModelType == RoomAirModel_UCSDCV ) ) {
				Tin( ZoneNum ) = RoomOutflowTemp( MultizoneSurfaceData( MaxSurf ).SurfNum );
			} else {
				if ( Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).Zone == ZoneNum ) {
					Tin( ZoneNum ) = MAT( Surface( Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).ExtBoundCond ).Zone );
				} else {
					Tin( ZoneNum ) = MAT( Surface( MultizoneSurfaceData( MaxSurf ).SurfNum ).Zone );
				}
			}
		}

	}

	//**************************************************************************************************

	void
	CalcUCSDCV( int const ZoneNum ) // Which Zonenum
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         G. Carrilho da Graca
		//       DATE WRITTEN   October 2004
		//       MODIFIED       8/2013 - Sam Brunswick
		//                      To incorporate improved temperature calculations
		//       RE-ENGINEERED  -

		// PURPOSE OF THIS SUBROUTINE:
		// Subroutine for cross ventilation modelling.
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
		using Psychrometrics::PsyRhoAirFnPbTdbW;
		using Psychrometrics::PsyCpAirFnWTdb;
		using InternalHeatGains::SumAllInternalConvectionGains;
		using InternalHeatGains::SumAllReturnAirConvectionGains;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 GainsFrac; // Fraction of lower subzone internal gains that mix as opposed to forming plumes
		Real64 ConvGains; // Total convective gains in the room
		Real64 ConvGainsJet; // Total convective gains released in jet subzone
		Real64 ConvGainsRec; // Total convective gains released in recirculation subzone
		Real64 MCp_Total; // Total capacity rate into the zone - assumed to enter at low level
		Real64 ZTAveraged;

		int Ctd;
		Real64 MCpT_Total;
		Real64 L;
		Real64 ZoneMult; // total zone multiplier
		Real64 RetAirConvGain;

		GainsFrac = 0.0;
		ZoneMult = Zone( ZoneNum ).Multiplier * Zone( ZoneNum ).ListMultiplier;

		for ( Ctd = 1; Ctd <= TotUCSDCV; ++Ctd ) {
			if ( ZoneNum == ZoneUCSDCV( Ctd ).ZonePtr ) {
				GainsFrac = GetCurrentScheduleValue( ZoneUCSDCV( Ctd ).SchedGainsPtr );
			}
		}

		SumAllInternalConvectionGains( ZoneNum, ConvGains );
		ConvGains += SumConvHTRadSys( ZoneNum ) + SumConvPool( ZoneNum ) + SysDepZoneLoadsLagged( ZoneNum ) + NonAirSystemResponse( ZoneNum ) / ZoneMult;

		// Add heat to return air if zonal system (no return air) or cycling system (return air frequently very low or zero)
		if ( Zone( ZoneNum ).NoHeatToReturnAir ) {
			SumAllReturnAirConvectionGains( ZoneNum, RetAirConvGain );
			ConvGains += RetAirConvGain;
		}

		ConvGainsJet = ConvGains * GainsFrac;
		ConvGainsRec = ConvGains * ( 1.0 - GainsFrac );
		MCp_Total = MCPI( ZoneNum ) + MCPV( ZoneNum ) + MCPM( ZoneNum ) + MCPE( ZoneNum ) + MCPC( ZoneNum ) + MDotCPOA( ZoneNum );
		MCpT_Total = MCPTI( ZoneNum ) + MCPTV( ZoneNum ) + MCPTM( ZoneNum ) + MCPTE( ZoneNum ) + MCPTC( ZoneNum ) + MDotCPOA( ZoneNum ) * Zone( ZoneNum ).OutDryBulbTemp;

		if ( SimulateAirflowNetwork == AirflowNetworkControlMultizone ) {
			MCp_Total = AirflowNetworkExchangeData( ZoneNum ).SumMCp + AirflowNetworkExchangeData( ZoneNum ).SumMMCp;
			MCpT_Total = AirflowNetworkExchangeData( ZoneNum ).SumMCpT + AirflowNetworkExchangeData( ZoneNum ).SumMMCpT;
		}

		EvolveParaUCSDCV( ZoneNum );
		L = Droom( ZoneNum );

		if ( AirModel( ZoneNum ).SimAirModel ) {
			//=============================== CROSS VENTILATION  Calculation ==============================================
			ZoneCVisMixing( ZoneNum ) = 0.0;
			ZoneCVhasREC( ZoneNum ) = 1.0;
			for ( Ctd = 1; Ctd <= 4; ++Ctd ) {
				HcUCSDCV( ZoneNum );
				if ( JetRecAreaRatio( ZoneNum ) != 1.0 ) {
					ZTREC( ZoneNum ) = ( ConvGainsRec * CrecTemp + CrecTemp * HAT_R + Tin( ZoneNum ) * MCp_Total ) / ( CrecTemp * HA_R + MCp_Total );
				}
				ZTJET( ZoneNum ) = ( ConvGainsJet * CjetTemp + ConvGainsRec * CjetTemp + CjetTemp * HAT_J + CjetTemp * HAT_R + Tin( ZoneNum ) * MCp_Total - CjetTemp * HA_R * ZTREC( ZoneNum ) ) / ( CjetTemp * HA_J + MCp_Total );
				RoomOutflowTemp( ZoneNum ) = ( ConvGainsJet + ConvGainsRec + HAT_J + HAT_R + Tin( ZoneNum ) * MCp_Total - HA_J * ZTJET( ZoneNum ) - HA_R * ZTREC( ZoneNum ) ) / MCp_Total;
			}
			if ( JetRecAreaRatio( ZoneNum ) == 1.0 ) {
				ZoneCVhasREC( ZoneNum ) = 0.0;
				ZTREC( ZoneNum ) = RoomOutflowTemp( ZoneNum );
				ZTREC( ZoneNum ) = ZTJET( ZoneNum );
				ZTREC( ZoneNum ) = ZTJET( ZoneNum );
			}
			// If temperature increase is above 1.5C then go to mixing
			if ( RoomOutflowTemp( ZoneNum ) - Tin( ZoneNum ) > 1.5 ) {
				ZoneCVisMixing( ZoneNum ) = 1.0;
				ZoneCVhasREC( ZoneNum ) = 0.0;
				AirModel( ZoneNum ).SimAirModel = false;
				Ujet( ZoneNum ) = 0.0;
				Urec( ZoneNum ) = 0.0;
				Qrec( ZoneNum ) = 0.0;
				RecInflowRatio( ZoneNum ) = 0.0;
				CVJetRecFlows.Ujet() = 0.0;
				CVJetRecFlows.Urec() = 0.0;
				for ( Ctd = 1; Ctd <= 3; ++Ctd ) {
					ZTAveraged = MAT( ZoneNum );
					RoomOutflowTemp( ZoneNum ) = ZTAveraged;
					ZTJET( ZoneNum ) = ZTAveraged;
					ZTREC( ZoneNum ) = ZTAveraged;
					RoomOutflowTemp( ZoneNum ) = ZTAveraged;
					ZTREC( ZoneNum ) = ZTAveraged;
					ZTJET( ZoneNum ) = ZTAveraged;
					ZTREC( ZoneNum ) = ZTAveraged;
					HcUCSDCV( ZoneNum );
					ZTAveraged = MAT( ZoneNum );
					RoomOutflowTemp( ZoneNum ) = ZTAveraged;
					ZTJET( ZoneNum ) = ZTAveraged;
					ZTREC( ZoneNum ) = ZTAveraged;
					RoomOutflowTemp( ZoneNum ) = ZTAveraged;
					ZTREC( ZoneNum ) = ZTAveraged;
					ZTJET( ZoneNum ) = ZTAveraged;
					ZTREC( ZoneNum ) = ZTAveraged;
				}
			}
		} else {
			//=============================== M I X E D  Calculation ======================================================
			ZoneCVisMixing( ZoneNum ) = 1.0;
			ZoneCVhasREC( ZoneNum ) = 0.0;
			Ujet( ZoneNum ) = 0.0;
			Urec( ZoneNum ) = 0.0;
			Qrec( ZoneNum ) = 0.0;
			RecInflowRatio( ZoneNum ) = 0.0;
			CVJetRecFlows.Ujet() = 0.0;
			CVJetRecFlows.Urec() = 0.0;
			for ( Ctd = 1; Ctd <= 3; ++Ctd ) {
				ZTAveraged = MAT( ZoneNum );
				RoomOutflowTemp( ZoneNum ) = ZTAveraged;
				ZTJET( ZoneNum ) = ZTAveraged;
				ZTREC( ZoneNum ) = ZTAveraged;
				RoomOutflowTemp( ZoneNum ) = ZTAveraged;
				ZTREC( ZoneNum ) = ZTAveraged;
				ZTJET( ZoneNum ) = ZTAveraged;
				ZTREC( ZoneNum ) = ZTAveraged;
				HcUCSDCV( ZoneNum );
				ZTAveraged = MAT( ZoneNum );
				RoomOutflowTemp( ZoneNum ) = ZTAveraged;
				ZTJET( ZoneNum ) = ZTAveraged;
				ZTREC( ZoneNum ) = ZTAveraged;
				RoomOutflowTemp( ZoneNum ) = ZTAveraged;
				ZTREC( ZoneNum ) = ZTAveraged;
				ZTJET( ZoneNum ) = ZTAveraged;
				ZTREC( ZoneNum ) = ZTAveraged;
			}
		}
		//============================================================================================================

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

} // CrossVentMgr

} // EnergyPlus
