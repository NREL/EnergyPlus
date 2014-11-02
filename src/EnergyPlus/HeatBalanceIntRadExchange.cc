// C++ Headers
#include <cassert>
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/FArray.functions.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include <HeatBalanceIntRadExchange.hh>
#include <DataEnvironment.hh>
#include <DataGlobals.hh>
#include <DataHeatBalance.hh>
#include <DataIPShortCuts.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSurfaces.hh>
#include <DataSystemVariables.hh>
#include <DataTimings.hh>
#include <DataViewFactorInformation.hh>
#include <DisplayRoutines.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <UtilityRoutines.hh>
#include <WindowEquivalentLayer.hh>
#include <Timer.h>

namespace EnergyPlus {

#define EP_HBIRE_SEQ

namespace HeatBalanceIntRadExchange {
	// Module containing the routines dealing with the interior radiant exchange
	// between surfaces.

	// MODULE INFORMATION:
	//       AUTHOR         Rick Strand
	//       DATE WRITTEN   September 2000
	//       MODIFIED       Aug 2001, FW: recalculate ScriptF for a zone if window interior
	//                       shade/blind status is different from previous time step. This is
	//                       because ScriptF, which is used to calculate interior LW
	//                       exchange between surfaces, depends on inside surface emissivities,
	//                       which, for a window, depends on whether or not an interior
	//                       shade or blind is in place.
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// Part of the heat balance modularization/re-engineering.  Purpose of this
	// module is to replace the MRT with RBAL method of modeling radiant exchange
	// between interior surfaces.

	// METHODOLOGY EMPLOYED:
	// Standard EnergyPlus methodology

	// REFERENCES:
	// ASHRAE Loads Toolkit "Script F" routines by Curt Pedersen
	// Hottel, H.C., and A.F. Sarofim. "Radiative Transfer" (mainly chapter 3),
	//  McGraw-Hill, Inc., New York, 1967.

	// OTHER NOTES: none

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataGlobals;
	using namespace DataEnvironment;
	using namespace DataHeatBalance;
	using namespace DataSurfaces;
	using namespace DataSystemVariables;
	using namespace DataViewFactorInformation;
	using namespace DataTimings;

	// Data
	// MODULE PARAMETER DEFINITIONS
	static gio::Fmt const fmtLD( "*" );
	static gio::Fmt const fmtA( "(A)" );
	static gio::Fmt const fmtx( "(A,I4,1x,A,1x,6f16.8)" );
	static gio::Fmt const fmty( "(A,1x,6f16.8)" );

	// DERIVED TYPE DEFINITIONS
	// na

	// MODULE VARIABLE DECLARATIONS:
	int MaxNumOfZoneSurfaces; // Max saved to get large enough space for user input view factors

	// SUBROUTINE SPECIFICATIONS FOR MODULE HeatBalanceIntRadExchange

	// Functions

	void
	CalcInteriorRadExchange(
		FArray1S< Real64 > const SurfaceTemp, // Current surface temperatures
		int const SurfIterations, // Number of iterations in calling subroutine
		FArray1< Real64 > & NetLWRadToSurf, // Net long wavelength radiant exchange from other surfaces
		Optional_int_const ZoneToResimulate, // if passed in, then only calculate for this zone
		std::string const & CalledFrom
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   September 2000
		//       MODIFIED       6/18/01, FCW: calculate IR on windows
		//                      Jan 2002, FCW: add blinds with movable slats
		//                      Sep 2011 LKL/BG - resimulate only zones needing it for Radiant systems
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Determines the interior radiant exchange between surfaces using
		// Hottel's ScriptF method for the grey interchange between surfaces
		// in an enclosure.

		// METHODOLOGY EMPLOYED:
		// See reference

		// REFERENCES:
		// Hottel, H. C. and A. F. Sarofim, Radiative Transfer, Ch 3, McGraw Hill, 1967.

		// Using/Aliasing
		using General::InterpSlatAng; // Function for slat angle interpolation
		using namespace DataTimings;
		using WindowEquivalentLayer::EQLWindowInsideEffectiveEmiss;
		using InputProcessor::SameString;

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENTS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const StefanBoltzmannConst( 5.6697e-8 ); // Stefan-Boltzmann constant in W/(m2*K4)
		static gio::Fmt const fmtLD( "*" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool firstTime( true ); // Logical flag for one-time initializations
		int RecSurfNum; // Counter within DO loop (refers to main surface derived type index) RECEIVING SURFACE
		int RecZoneSurfNum; // DO loop counter for receiving surface within a zone (local derived type arrays)
		int SendSurfNum; // Counter within DO loop (refers to main surface derived type index) SENDING SURFACE

		int ZoneNum; // DO loop counter for zones
		int ConstrNumRec; // Receiving surface construction number
		int ConstrNumSend; // Sending surface construction number
		Real64 RecSurfTemp; // Receiving surface temperature (C)
		Real64 SendSurfTemp; // Sending surface temperature (C)
		Real64 RecSurfEmiss; // Inside surface emissivity
		int ZoneSurfNum; // Runs from 1 to number of surfaces in zone
		int SurfNum; // Surface number
		int ConstrNum; // Construction number
		bool IntShadeOrBlindStatusChanged; // True if status of interior shade or blind on at least
		// one window in a zone has changed from previous time step
		int ShadeFlag; // Window shading status current time step
		int ShadeFlagPrev; // Window shading status previous time step

		//variables added as part of strategy to reduce calculation time - Glazer 2011-04-22
//		Real64 SendSurfTempInKTo4th; // Sending surface temperature in K to 4th power
		Real64 RecSurfTempInKTo4th; // Receiving surface temperature in K to 4th power
		static FArray1D< Real64 > SendSurfaceTempInKto4thPrecalc;

		// FLOW:

#ifdef EP_Detailed_Timings
		epStartTime( "CalcInteriorRadExchange=" );
#endif
		if ( firstTime ) {
			InitInteriorRadExchange();
#ifdef EP_HBIRE_SEQ
			SendSurfaceTempInKto4thPrecalc.allocate( MaxNumOfZoneSurfaces );
#else
			SendSurfaceTempInKto4thPrecalc.allocate( TotSurfaces );
#endif
			firstTime = false;
			if ( DeveloperFlag ) {
				std::string tdstring;
				gio::write( tdstring, fmtLD ) << " OMP turned off, HBIRE loop executed in serial";
				DisplayString( tdstring );
			}
		}

		if ( KickOffSimulation || KickOffSizing ) return;

#ifdef EP_Count_Calls
		if ( ! present( ZoneToResimulate ) ) {
			++NumIntRadExchange_Calls;
		} else {
			++NumIntRadExchangeZ_Calls;
		}
		if ( CalledFrom.empty() ) {
			// do nothing
		} else if ( CalledFrom == "Main" ) {
			++NumIntRadExchangeMain_Calls;
		} else if ( CalledFrom == "Outside" ) {
			++NumIntRadExchangeOSurf_Calls;
		} else if ( CalledFrom == "Inside" ) {
			++NumIntRadExchangeISurf_Calls;
		}
#endif

		ConstrNumRec = 0;
		if ( ! present( ZoneToResimulate ) ) {
			NetLWRadToSurf = 0.0;
			SurfaceWindow.IRfromParentZone() = 0.0;
		}

		for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {

			auto const & zone( Zone( ZoneNum ) );

			if ( present( ZoneToResimulate ) ) {
				if ( ZoneNum != ZoneToResimulate ) {
					continue;
				} else {
					NetLWRadToSurf( {zone.SurfaceFirst,zone.SurfaceLast} ) = 0.0;
					SurfaceWindow( {zone.SurfaceFirst,zone.SurfaceLast} ).IRfromParentZone() = 0.0;
				}
			}

			auto & zone_info( ZoneInfo( ZoneNum ) );
			auto & zone_ScriptF( zone_info.ScriptF );
			auto & zone_SurfacePtr( zone_info.SurfacePtr );
			int const n_zone_Surfaces( zone_info.NumOfSurfaces );

			// Calculate ScriptF if first time step in environment and surface heat-balance iterations not yet started;
			// recalculate ScriptF if status of window interior shades or blinds has changed from
			// previous time step. This recalculation is required since ScriptF depends on the inside
			// emissivity of the inside surfaces, which, for windows, is (1) the emissivity of the
			// inside face of the inside glass layer if there is no interior shade/blind, or (2) the effective
			// emissivity of the shade/blind if the shade/blind is in place. (The "effective emissivity"
			// in this case is (1) the shade/blind emissivity if the shade/blind IR transmittance is zero,
			// or (2) a weighted average of the shade/blind emissivity and inside glass emissivity if the
			// shade/blind IR transmittance is not zero (which is sometimes the case for a "shade" and
			// usually the case for a blind). It assumed for switchable glazing that the inside surface
			// emissivity does not change if the glazing is switched on or off.

			// Determine if status of interior shade/blind on one or more windows in the zone has changed
			// from previous time step.

			if ( SurfIterations == 0 ) {

				IntShadeOrBlindStatusChanged = false;

				if ( ! BeginEnvrnFlag ) { // Check for change in shade/blind status
					for ( SurfNum = zone.SurfaceFirst; SurfNum <= zone.SurfaceLast; ++SurfNum ) {
						if ( IntShadeOrBlindStatusChanged ) break; // Need only check of one window's status has changed
						ConstrNum = Surface( SurfNum ).Construction;
						if ( ! Construct( ConstrNum ).TypeIsWindow ) continue;
						ShadeFlag = SurfaceWindow( SurfNum ).ShadingFlag;
						ShadeFlagPrev = SurfaceWindow( SurfNum ).ExtIntShadePrevTS;
						if ( ( ShadeFlagPrev != IntShadeOn && ShadeFlag == IntShadeOn ) || ( ShadeFlagPrev != IntBlindOn && ShadeFlag == IntBlindOn ) || ( ShadeFlagPrev == IntShadeOn && ShadeFlag != IntShadeOn ) || ( ShadeFlagPrev == IntBlindOn && ShadeFlag != IntBlindOn ) ) IntShadeOrBlindStatusChanged = true;
					}
				}

				if ( IntShadeOrBlindStatusChanged || BeginEnvrnFlag ) { // Calc inside surface emissivities for this time step
					for ( ZoneSurfNum = 1; ZoneSurfNum <= n_zone_Surfaces; ++ZoneSurfNum ) {
						SurfNum = zone_SurfacePtr( ZoneSurfNum );
						ConstrNum = Surface( SurfNum ).Construction;
						zone_info.Emissivity( ZoneSurfNum ) = Construct( ConstrNum ).InsideAbsorpThermal;
						auto const & surface_window( SurfaceWindow( SurfNum ) );
						if ( Construct( ConstrNum ).TypeIsWindow && ( surface_window.ShadingFlag == IntShadeOn || surface_window.ShadingFlag == IntBlindOn ) ) {
							zone_info.Emissivity( ZoneSurfNum ) = InterpSlatAng( surface_window.SlatAngThisTS, surface_window.MovableSlats, surface_window.EffShBlindEmiss ) + InterpSlatAng( surface_window.SlatAngThisTS, surface_window.MovableSlats, surface_window.EffGlassEmiss );
						}
					}

					CalcScriptF( n_zone_Surfaces, zone_info.Area, zone_info.F, zone_info.Emissivity, zone_ScriptF );
					// precalc - multiply by StefanBoltzmannConstant
					zone_ScriptF *= StefanBoltzmannConst;
				}

			} // End of check if SurfIterations = 0

			// precalculate the fourth power of surface temperature as part of strategy to reduce calculation time - Glazer 2011-04-22
			for ( int SendZoneSurfNum = 1; SendZoneSurfNum <= n_zone_Surfaces; ++SendZoneSurfNum ) {
				SendSurfNum = zone_SurfacePtr( SendZoneSurfNum );
				auto const & surface_window( SurfaceWindow( SendSurfNum ) );
				ConstrNumSend = Surface( SendSurfNum ).Construction;
				auto const & construct( Construct( ConstrNumSend ) );
				SendSurfTemp = SurfaceTemp( SendSurfNum );
				if ( construct.TypeIsWindow && surface_window.OriginalClass != SurfaceClass_TDD_Diffuser && ! construct.WindowTypeEQL ) {
					if ( SurfIterations == 0 && surface_window.ShadingFlag <= 0 ) {
						SendSurfTemp = surface_window.ThetaFace( 2 * construct.TotGlassLayers ) - KelvinConv;
					} else if ( surface_window.ShadingFlag == IntShadeOn || surface_window.ShadingFlag == IntBlindOn ) {
						SendSurfTemp = surface_window.EffInsSurfTemp;
					}
				} else if ( construct.WindowTypeEQL ) {
					SendSurfTemp = surface_window.EffInsSurfTemp;
				}
#ifdef EP_HBIRE_SEQ
				SendSurfaceTempInKto4thPrecalc( SendZoneSurfNum ) = pow_4( SendSurfTemp + KelvinConv );
#else
				SendSurfaceTempInKto4thPrecalc( SendSurfNum ) = pow_4( SendSurfTemp + KelvinConv );
#endif
			}

			// these are the money do loops.
			auto const sizeR( zone_ScriptF.size1() ); // For linear indexing
			for ( RecZoneSurfNum = 1; RecZoneSurfNum <= n_zone_Surfaces; ++RecZoneSurfNum ) {
				RecSurfNum = zone_SurfacePtr( RecZoneSurfNum );
				ConstrNumRec = Surface( RecSurfNum ).Construction;
				auto const & construct( Construct( ConstrNumRec ) );
				RecSurfTemp = SurfaceTemp( RecSurfNum );
				RecSurfEmiss = construct.InsideAbsorpThermal;
				auto & surface_window( SurfaceWindow( RecSurfNum ) );
				auto & netLWRadToRecSurf( NetLWRadToSurf( RecSurfNum ) );
				if ( construct.TypeIsWindow && surface_window.OriginalClass != SurfaceClass_TDD_Diffuser && ! construct.WindowTypeEQL ) {
					if ( SurfIterations == 0 && surface_window.ShadingFlag <= 0 ) {
						// If the window is bare this TS and it is the first time through we use the previous TS glass
						// temperature whether or not the window was shaded in the previous TS. If the window was shaded
						// the previous time step this temperature is a better starting value than the shade temperature.
						RecSurfTemp = surface_window.ThetaFace( 2 * construct.TotGlassLayers ) - KelvinConv;
						// For windows with an interior shade or blind an effective inside surface temp
						// and emiss is used here that is a weighted combination of shade/blind and glass temp and emiss.
					} else if ( surface_window.ShadingFlag == IntShadeOn || surface_window.ShadingFlag == IntBlindOn ) {
						RecSurfTemp = surface_window.EffInsSurfTemp;
						RecSurfEmiss = InterpSlatAng( surface_window.SlatAngThisTS, surface_window.MovableSlats, surface_window.EffShBlindEmiss ) + InterpSlatAng( surface_window.SlatAngThisTS, surface_window.MovableSlats, surface_window.EffGlassEmiss );
					}
				} else if ( construct.WindowTypeEQL ) {
					RecSurfEmiss = EQLWindowInsideEffectiveEmiss( ConstrNumRec );
					RecSurfTemp = surface_window.EffInsSurfTemp;
				}
				// precalculate the fourth power of surface temperature as part of strategy to reduce calculation time - Glazer 2011-04-22
				RecSurfTempInKTo4th = pow_4( RecSurfTemp + KelvinConv );
				//      IF (ABS(RecSurfTempInKTo4th) > 1.d100) THEN
				//        SendZoneSurfNum=1
				//      ENDIF

				// Calculate net long-wave radiation for opaque surfaces and incident
				// long-wave radiation for windows.
				auto lRS( zone_ScriptF.index( RecZoneSurfNum, 1 ) );
				if ( construct.TypeIsWindow ) { // Window
					Real64 const RecSurfEmiss_inv( 1.0 / RecSurfEmiss );
					Real64 netLWRadToRecSurf_acc( 0.0 ); // Local accumulator
					for ( int SendZoneSurfNum = 1; SendZoneSurfNum <= n_zone_Surfaces; ++SendZoneSurfNum, lRS += sizeR ) {
						Real64 const scriptF( zone_ScriptF[ lRS ] ); // [ lRS ] == ( RecZoneSurfNum, SendZoneSurfNum )
#ifdef EP_HBIRE_SEQ
						Real64 const temp_ink_4th( SendSurfaceTempInKto4thPrecalc( SendZoneSurfNum ) );
#else
						SendSurfNum = zone_SurfacePtr( SendZoneSurfNum );
						Real64 const temp_ink_4th( SendSurfaceTempInKto4thPrecalc( SendSurfNum ) );
#endif
						if ( RecZoneSurfNum != SendZoneSurfNum ) {
							netLWRadToRecSurf_acc += scriptF * ( temp_ink_4th - RecSurfTempInKTo4th );
						}
						// Calculate interior LW incident on window rather than net LW for use in window layer heat balance calculation.
						surface_window.IRfromParentZone += ( scriptF * temp_ink_4th ) * RecSurfEmiss_inv;

						// Per BG -- this should never happened.  (CR6346,CR6550 caused this to be put in.  Now removed. LKL 1/2013)
						//          IF (SurfaceWindow(RecSurfNum)%IRfromParentZone < 0.0) THEN
						//            CALL ShowRecurringWarningErrorAtEnd('CalcInteriorRadExchange: Window_IRFromParentZone negative, Window="'// &
						//                TRIM(Surface(RecSurfNum)%Name)//'"',  &
						//                SurfaceWindow(RecSurfNum)%IRErrCount)
						//            CALL ShowRecurringContinueErrorAtEnd('..occurs in Zone="'//TRIM(Surface(RecSurfNum)%ZoneName)//  &
						//                '", reset to 0.0 for remaining calculations.',SurfaceWindow(RecSurfNum)%IRErrCountC)
						//            SurfaceWindow(RecSurfNum)%IRfromParentZone=0.0
						//          ENDIF
					}
					netLWRadToRecSurf += netLWRadToRecSurf_acc;
				} else {
					Real64 netLWRadToRecSurf_acc( 0.0 ); // Local accumulator
					for ( int SendZoneSurfNum = 1; SendZoneSurfNum <= n_zone_Surfaces; ++SendZoneSurfNum, lRS += sizeR ) {
						if ( RecZoneSurfNum != SendZoneSurfNum ) {
#ifdef EP_HBIRE_SEQ
							netLWRadToRecSurf_acc += zone_ScriptF[ lRS ] * ( SendSurfaceTempInKto4thPrecalc( SendZoneSurfNum ) - RecSurfTempInKTo4th ); // [ lRS ] == ( RecZoneSurfNum, SendZoneSurfNum )
#else
							SendSurfNum = zone_SurfacePtr( SendZoneSurfNum );
							netLWRadToRecSurf_acc += zone_ScriptF[ lRS ] * ( SendSurfaceTempInKto4thPrecalc( SendSurfNum ) - RecSurfTempInKTo4th ); // [ lRS ] == ( RecZoneSurfNum, SendZoneSurfNum )
#endif
						}
					}
					netLWRadToRecSurf += netLWRadToRecSurf_acc;
				}
			}
		}

#ifdef EP_Detailed_Timings
		epStopTime( "CalcInteriorRadExchange=" );
#endif

	}

	void
	InitInteriorRadExchange()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   September 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Initializes the various parameters for Hottel's ScriptF method for
		// the grey interchange between surfaces in an enclosure.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::GetObjectDefMaxArgs;
		using General::RoundSigDigits;
		using General::ScanForReports;

		// Locals
		// SUBROUTINE ARGUMENTS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static gio::Fmt const AFormat( "(A)" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumOfZoneSurfaces; // total number of surfaces in the zone.
		int SurfNum; // Counter within DO loop (refers to main surface derived type index)
		int ZoneNum; // DO loop counter for zones
		int ZoneSurfNum; // DO loop counter for surfaces within a zone (refers to local derived type arrays)
		int Findex; // index to print view factors
		int Vindex; // index for vertices
		int NumZonesWithUserFbyS; // Zones with user input,  used for flag here
		bool NoUserInputF; // Logical flag signifying no input F's for zone
		static bool ViewFactorReport; // Flag to output view factor report in eio file
		static bool ErrorsFound( false );
		Real64 CheckValue1;
		Real64 CheckValue2;
		Real64 FinalCheckValue;
		FArray2D< Real64 > SaveApproximateViewFactors; // Save for View Factor reporting
		Real64 RowSum;
		Real64 FixedRowSum;
		int NumIterations;
		std::string Option1; // view factor report option

		// FLOW:

		ZoneInfo.allocate( NumOfZones ); // Allocate the entire derived type

		ScanForReports( "ViewFactorInfo", ViewFactorReport, _, Option1 );

		if ( ViewFactorReport ) { // Print heading
			gio::write( OutputFileInits, fmtA ) << "! <Surface View Factor and Grey Interchange Information>";
			gio::write( OutputFileInits, fmtA ) << "! <View Factor - Zone Information>,Zone Name,Number of Surfaces";
			gio::write( OutputFileInits, fmtA ) << "! <View Factor - Surface Information>,Surface Name,Surface Class,Area {m2},Azimuth," "Tilt,Thermal Emissivity,#Sides,Vertices";
			gio::write( OutputFileInits, fmtA ) << "! <View Factor / Grey Interchange Type>,Surface Name(s)";
			gio::write( OutputFileInits, fmtA ) << "! <View Factor>,Surface Name,Surface Class,Row Sum,View Factors for each Surface";
		}

		cCurrentModuleObject = "ZoneProperty:UserViewFactors:bySurfaceName";
		NumZonesWithUserFbyS = GetNumObjectsFound( cCurrentModuleObject );

		MaxNumOfZoneSurfaces = 0;
		for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {

			if ( ZoneNum == 1 ) {
				if ( DisplayAdvancedReportVariables ) gio::write( OutputFileInits, fmtA ) << "! <Surface View Factor Check Values>,Zone Name,Original Check Value," "Calculated Fixed Check Value,Final Check Value,Number of Iterations,Fixed RowSum Convergence," "Used RowSum Convergence";
			}

			ZoneInfo( ZoneNum ).Name = Zone( ZoneNum ).Name;

			NumOfZoneSurfaces = 0;
			for ( SurfNum = Zone( ZoneNum ).SurfaceFirst; SurfNum <= Zone( ZoneNum ).SurfaceLast; ++SurfNum ) {
				if ( Surface( SurfNum ).HeatTransSurf ) ++NumOfZoneSurfaces;
			}
			ZoneInfo( ZoneNum ).NumOfSurfaces = NumOfZoneSurfaces;
			MaxNumOfZoneSurfaces = max( MaxNumOfZoneSurfaces, NumOfZoneSurfaces );
			if ( ZoneInfo( ZoneNum ).NumOfSurfaces < 1 ) ShowFatalError( "No surfaces in a zone in InitInteriorRadExchange" );

			// Allocate the parts of the derived type
			ZoneInfo( ZoneNum ).F.allocate( ZoneInfo( ZoneNum ).NumOfSurfaces, ZoneInfo( ZoneNum ).NumOfSurfaces );
			ZoneInfo( ZoneNum ).F = 0.0;
			ZoneInfo( ZoneNum ).ScriptF.allocate( ZoneInfo( ZoneNum ).NumOfSurfaces, ZoneInfo( ZoneNum ).NumOfSurfaces );
			ZoneInfo( ZoneNum ).ScriptF = 0.0;
			ZoneInfo( ZoneNum ).Area.allocate( ZoneInfo( ZoneNum ).NumOfSurfaces );
			ZoneInfo( ZoneNum ).Area = 0.0;
			ZoneInfo( ZoneNum ).Emissivity.allocate( ZoneInfo( ZoneNum ).NumOfSurfaces );
			ZoneInfo( ZoneNum ).Emissivity = 0.0;
			ZoneInfo( ZoneNum ).Azimuth.allocate( ZoneInfo( ZoneNum ).NumOfSurfaces );
			ZoneInfo( ZoneNum ).Azimuth = 0.0;
			ZoneInfo( ZoneNum ).Tilt.allocate( ZoneInfo( ZoneNum ).NumOfSurfaces );
			ZoneInfo( ZoneNum ).Tilt = 0.0;
			ZoneInfo( ZoneNum ).SurfacePtr.allocate( ZoneInfo( ZoneNum ).NumOfSurfaces );
			ZoneInfo( ZoneNum ).SurfacePtr = 0;

			// Initialize the surface pointer array
			ZoneSurfNum = 0;
			for ( SurfNum = Zone( ZoneNum ).SurfaceFirst; SurfNum <= Zone( ZoneNum ).SurfaceLast; ++SurfNum ) {
				if ( ! Surface( SurfNum ).HeatTransSurf ) continue;
				++ZoneSurfNum;
				ZoneInfo( ZoneNum ).SurfacePtr( ZoneSurfNum ) = SurfNum;
			}
			// Initialize the area and emissivity arrays
			for ( ZoneSurfNum = 1; ZoneSurfNum <= ZoneInfo( ZoneNum ).NumOfSurfaces; ++ZoneSurfNum ) {
				SurfNum = ZoneInfo( ZoneNum ).SurfacePtr( ZoneSurfNum );

				//************************************************
				if ( ! Construct( Surface( SurfNum ).Construction ).TypeIsIRT ) {
					ZoneInfo( ZoneNum ).Area( ZoneSurfNum ) = Surface( SurfNum ).Area;
				} else {
					// Double area for infrared transparent (IRT) surfaces
					ZoneInfo( ZoneNum ).Area( ZoneSurfNum ) = 2.0 * Surface( SurfNum ).Area;
				}
				//***********************************************

				ZoneInfo( ZoneNum ).Emissivity( ZoneSurfNum ) = Construct( Surface( SurfNum ).Construction ).InsideAbsorpThermal;
				ZoneInfo( ZoneNum ).Azimuth( ZoneSurfNum ) = Surface( SurfNum ).Azimuth;
				ZoneInfo( ZoneNum ).Tilt( ZoneSurfNum ) = Surface( SurfNum ).Tilt;
			}

			if ( ZoneInfo( ZoneNum ).NumOfSurfaces == 1 ) {
				// If there is only one surface in a zone, then there is no radiant exchange
				ZoneInfo( ZoneNum ).F = 0.0;
				ZoneInfo( ZoneNum ).ScriptF = 0.0;
				if ( DisplayAdvancedReportVariables ) gio::write( OutputFileInits, fmtA ) << "Surface View Factor Check Values," + Zone( ZoneNum ).Name + ",0,0,0,-1,0,0";
				continue; // Go to the next zone in the  ZoneNum DO loop
			}

			//  Get user supplied view factors if available in idf.

			NoUserInputF = true;

			if ( NumZonesWithUserFbyS > 0 ) {

				GetInputViewFactorsbyName( ZoneInfo( ZoneNum ).Name, ZoneInfo( ZoneNum ).NumOfSurfaces, ZoneInfo( ZoneNum ).F, ZoneInfo( ZoneNum ).SurfacePtr, NoUserInputF, ErrorsFound ); // Obtains user input view factors from input file
			}

			if ( NoUserInputF ) {

				// Calculate the view factors and make sure they satisfy reciprocity
				CalcApproximateViewFactors( ZoneInfo( ZoneNum ).NumOfSurfaces, ZoneInfo( ZoneNum ).Area, ZoneInfo( ZoneNum ).Azimuth, ZoneInfo( ZoneNum ).Tilt, ZoneInfo( ZoneNum ).F, ZoneInfo( ZoneNum ).SurfacePtr );
			}

			if ( ViewFactorReport ) { // Allocate and save user or approximate view factors for reporting.
				SaveApproximateViewFactors.allocate( ZoneInfo( ZoneNum ).NumOfSurfaces, ZoneInfo( ZoneNum ).NumOfSurfaces );
				SaveApproximateViewFactors = ZoneInfo( ZoneNum ).F;
			}

			FixViewFactors( ZoneInfo( ZoneNum ).NumOfSurfaces, ZoneInfo( ZoneNum ).Area, ZoneInfo( ZoneNum ).F, ZoneNum, CheckValue1, CheckValue2, FinalCheckValue, NumIterations, FixedRowSum );

			// Calculate the script F factors
			CalcScriptF( ZoneInfo( ZoneNum ).NumOfSurfaces, ZoneInfo( ZoneNum ).Area, ZoneInfo( ZoneNum ).F, ZoneInfo( ZoneNum ).Emissivity, ZoneInfo( ZoneNum ).ScriptF );

			if ( ViewFactorReport ) { // Write to SurfInfo File
				// Zone Surface Information Output
				gio::write( OutputFileInits, fmtA ) << "Surface View Factor - Zone Information," + ZoneInfo( ZoneNum ).Name + ',' + RoundSigDigits( ZoneInfo( ZoneNum ).NumOfSurfaces );

				for ( SurfNum = 1; SurfNum <= ZoneInfo( ZoneNum ).NumOfSurfaces; ++SurfNum ) {
					gio::write( OutputFileInits, "(A,',',A,$)" )
						<< "Surface View Factor - Surface Information,"
						+ Surface( ZoneInfo( ZoneNum ).SurfacePtr( SurfNum ) ).Name + ','
						+ cSurfaceClass( Surface( ZoneInfo( ZoneNum ).SurfacePtr( SurfNum ) ).Class )
						<< RoundSigDigits( ZoneInfo( ZoneNum ).Area( SurfNum ), 4 ) + ','
						+ RoundSigDigits( ZoneInfo( ZoneNum ).Azimuth( SurfNum ), 4 ) + ','
						+ RoundSigDigits( ZoneInfo( ZoneNum ).Tilt( SurfNum ), 4 ) + ','
						+ RoundSigDigits( ZoneInfo( ZoneNum ).Emissivity( SurfNum ), 4 ) + ','
						+ RoundSigDigits( Surface( ZoneInfo( ZoneNum ).SurfacePtr( SurfNum ) ).Sides );
					for ( Vindex = 1; Vindex <= Surface( ZoneInfo( ZoneNum ).SurfacePtr( SurfNum ) ).Sides; ++Vindex ) {
						auto & Vertex = Surface( ZoneInfo( ZoneNum ).SurfacePtr( SurfNum ) ).Vertex( Vindex );
						gio::write( OutputFileInits, "(3(',',A),$)" )
							<< RoundSigDigits( Vertex.x, 4 )
							<< RoundSigDigits( Vertex.y, 4 )
							<< RoundSigDigits( Vertex.z, 4 );
					} gio::write( OutputFileInits );
				}

				gio::write( OutputFileInits, "(A,A,$)" )
					<< "Approximate or User Input ViewFactors"
					<< ",To Surface,Surface Class,RowSum";
				for ( SurfNum = 1; SurfNum <= ZoneInfo( ZoneNum ).NumOfSurfaces; ++SurfNum ) {
					gio::write( OutputFileInits, "(',',A,$)" )
						<< Surface( ZoneInfo( ZoneNum ).SurfacePtr( SurfNum ) ).Name;
				} gio::write( OutputFileInits );

				for ( Findex = 1; Findex <= ZoneInfo( ZoneNum ).NumOfSurfaces; ++Findex ) {
					RowSum = sum( SaveApproximateViewFactors( Findex, _ ) );
					gio::write( OutputFileInits, "(A,3(',',A),$)" )
						<< "View Factor"
						<< Surface( ZoneInfo( ZoneNum ).SurfacePtr( Findex ) ).Name
						<< cSurfaceClass( Surface( ZoneInfo( ZoneNum ).SurfacePtr( Findex ) ).Class )
						<< RoundSigDigits( RowSum, 4 );
					for ( SurfNum = 1; SurfNum <= ZoneInfo( ZoneNum ).NumOfSurfaces; ++SurfNum ) {
						gio::write( OutputFileInits, "(',',A,$)" )
							<< RoundSigDigits( SaveApproximateViewFactors( Findex, SurfNum ), 4 );
					} gio::write( OutputFileInits );
				}
			}

			if ( ViewFactorReport ) {
				gio::write( OutputFileInits, "(A,A,$)" ) << "Final ViewFactors" << ",To Surface,Surface Class,RowSum";
				for ( SurfNum = 1; SurfNum <= ZoneInfo( ZoneNum ).NumOfSurfaces; ++SurfNum ) {
					gio::write( OutputFileInits, "(',',A,$)" ) << Surface( ZoneInfo( ZoneNum ).SurfacePtr( SurfNum ) ).Name;
				} gio::write( OutputFileInits );

				for ( Findex = 1; Findex <= ZoneInfo( ZoneNum ).NumOfSurfaces; ++Findex ) {
					RowSum = sum( ZoneInfo( ZoneNum ).F( Findex, _ ) );
					gio::write( OutputFileInits, "(A,3(',',A),$)" )
						<< "View Factor"
						<< Surface( ZoneInfo( ZoneNum ).SurfacePtr( Findex ) ).Name
						<< cSurfaceClass( Surface( ZoneInfo( ZoneNum ).SurfacePtr( Findex ) ).Class )
						<< RoundSigDigits( RowSum, 4 );
					for ( SurfNum = 1; SurfNum <= ZoneInfo( ZoneNum ).NumOfSurfaces; ++SurfNum ) {
						gio::write( OutputFileInits, "(',',A,$)" ) << RoundSigDigits( ZoneInfo( ZoneNum ).F( Findex, SurfNum ), 4 );
					} gio::write( OutputFileInits );
				}

				if ( Option1 == "IDF" ) {
					gio::write( OutputFileDebug, fmtA ) << "!======== original input factors ===========================";
					gio::write( OutputFileDebug, fmtA ) << "ZoneProperty:UserViewFactors:bySurfaceName," + ZoneInfo( ZoneNum ).Name + ',';
					for ( SurfNum = 1; SurfNum <= ZoneInfo( ZoneNum ).NumOfSurfaces; ++SurfNum ) {
						for ( Findex = 1; Findex <= ZoneInfo( ZoneNum ).NumOfSurfaces; ++Findex ) {
							if ( ! ( SurfNum == ZoneInfo( ZoneNum ).NumOfSurfaces && Findex == ZoneInfo( ZoneNum ).NumOfSurfaces ) ) {
								gio::write( OutputFileDebug, fmtA ) << "  " + Surface( ZoneInfo( ZoneNum ).SurfacePtr( SurfNum ) ).Name + ',' + Surface( ZoneInfo( ZoneNum ).SurfacePtr( Findex ) ).Name + ',' + RoundSigDigits( ZoneInfo( ZoneNum ).F( SurfNum, Findex ), 6 ) + ',';
							} else {
								gio::write( OutputFileDebug, fmtA ) << "  " + Surface( ZoneInfo( ZoneNum ).SurfacePtr( SurfNum ) ).Name + ',' + Surface( ZoneInfo( ZoneNum ).SurfacePtr( Findex ) ).Name + ',' + RoundSigDigits( ZoneInfo( ZoneNum ).F( SurfNum, Findex ), 6 ) + ';';
							}
						}
					}
					gio::write( OutputFileDebug, fmtA ) << "!============= end of data ======================";

					gio::write( OutputFileDebug, fmtA ) << "!============ final view factors =======================";
					gio::write( OutputFileDebug, fmtA ) << "ZoneProperty:UserViewFactors:bySurfaceName," + ZoneInfo( ZoneNum ).Name + ',';
					for ( SurfNum = 1; SurfNum <= ZoneInfo( ZoneNum ).NumOfSurfaces; ++SurfNum ) {
						for ( Findex = 1; Findex <= ZoneInfo( ZoneNum ).NumOfSurfaces; ++Findex ) {
							if ( ! ( SurfNum == ZoneInfo( ZoneNum ).NumOfSurfaces && Findex == ZoneInfo( ZoneNum ).NumOfSurfaces ) ) {
								gio::write( OutputFileDebug, fmtA ) << "  " + Surface( ZoneInfo( ZoneNum ).SurfacePtr( SurfNum ) ).Name + ',' + Surface( ZoneInfo( ZoneNum ).SurfacePtr( Findex ) ).Name + ',' + RoundSigDigits( ZoneInfo( ZoneNum ).F( SurfNum, Findex ), 6 ) + ',';
							} else {
								gio::write( OutputFileDebug, fmtA ) << "  " + Surface( ZoneInfo( ZoneNum ).SurfacePtr( SurfNum ) ).Name + ',' + Surface( ZoneInfo( ZoneNum ).SurfacePtr( Findex ) ).Name + ',' + RoundSigDigits( ZoneInfo( ZoneNum ).F( SurfNum, Findex ), 6 ) + ';';
							}
						}
					}
					gio::write( OutputFileDebug, fmtA ) << "!============= end of data ======================";
				}

			}

			if ( ViewFactorReport ) {
				gio::write( OutputFileInits, "(A,A,$)" )
					<< "Script F Factors"
					<< ",X Surface";
				for ( SurfNum = 1; SurfNum <= ZoneInfo( ZoneNum ).NumOfSurfaces; ++SurfNum ) {
					gio::write( OutputFileInits, "(',',A,$)" ) <<
						Surface( ZoneInfo( ZoneNum ).SurfacePtr( SurfNum ) ).Name;
				} gio::write( OutputFileInits );
				for ( Findex = 1; Findex <= ZoneInfo( ZoneNum ).NumOfSurfaces; ++Findex ) {
					gio::write( OutputFileInits, "(A,',',A,$)" )
						<< "Script F Factor"
						<< Surface( ZoneInfo( ZoneNum ).SurfacePtr( Findex ) ).Name;
					for ( SurfNum = 1; SurfNum <= ZoneInfo( ZoneNum ).NumOfSurfaces; ++SurfNum ) {
						gio::write( OutputFileInits, "(',',A,$)" )
							<< RoundSigDigits( ZoneInfo( ZoneNum ).ScriptF( Findex, SurfNum ), 4 );
					} gio::write( OutputFileInits );
				}
			}

			if ( ViewFactorReport ) { // Deallocate saved approximate/user view factors
				SaveApproximateViewFactors.deallocate();
			}

			RowSum = 0.0;
			for ( Findex = 1; Findex <= ZoneInfo( ZoneNum ).NumOfSurfaces; ++Findex ) {
				RowSum += sum( ZoneInfo( ZoneNum ).F( Findex, _ ) );
			}
			RowSum = std::abs( RowSum - ZoneInfo( ZoneNum ).NumOfSurfaces );
			FixedRowSum = std::abs( FixedRowSum - ZoneInfo( ZoneNum ).NumOfSurfaces );
			if ( DisplayAdvancedReportVariables ) {
				gio::write( OutputFileInits, "(8A)" )
					<< "Surface View Factor Check Values,"
					 + Zone( ZoneNum ).Name + ','
					 + RoundSigDigits( CheckValue1, 6 ) + ','
					 + RoundSigDigits( CheckValue2, 6 ) + ','
					 + RoundSigDigits( FinalCheckValue, 6 ) + ','
					 + RoundSigDigits( NumIterations ) + ','
					 + RoundSigDigits( FixedRowSum, 6 ) + ','
					 + RoundSigDigits( RowSum, 6 );
			}

		}

		if ( ErrorsFound ) {
			ShowFatalError( "InitInteriorRadExchange: Errors found during initialization of radiant exchange.  Program terminated." );
		}

	}

	void
	GetInputViewFactors(
		std::string const & ZoneName, // Needed to check for user input view factors.
		int const N, // NUMBER OF SURFACES
		FArray2A< Real64 > F, // USER INPUT DIRECT VIEW FACTOR MATRIX (N X N)
		FArray1A_int const SPtr, // pointer to actual surface number
		bool & NoUserInputF, // Flag signifying no input F's for this
		bool & ErrorsFound // True when errors are found in number of fields vs max args
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Curt Pedersen
		//       DATE WRITTEN   September 2005
		//       MODIFIED       Linda Lawrie;September 2010
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine gets the user view factor info.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::GetObjectItemNum;
		using General::TrimSigDigits;

		// Argument array dimensioning
		F.dim( N, N );
		SPtr.dim( N );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//  INTEGER   :: NumZonesWithUserF
		int UserFZoneIndex;
		int NumAlphas;
		int NumNums;
		int IOStat;
		int index;
		int inx1;
		int inx2;
		//unused  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: ZoneSurfaceNames

		NoUserInputF = true;
		UserFZoneIndex = GetObjectItemNum( "ZoneProperty:UserViewFactors", ZoneName );

		if ( UserFZoneIndex > 0 ) {
			NoUserInputF = false;

			GetObjectItem( "ZoneProperty:UserViewFactors", UserFZoneIndex, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			if ( NumNums < 3 * pow_2( N ) ) {
				ShowSevereError( "GetInputViewFactors: " + cCurrentModuleObject + "=\"" + ZoneName + "\", not enough values." );
				ShowContinueError( "...Number of input values [" + TrimSigDigits( NumNums ) + "] is less than the required number=[" + TrimSigDigits( 3 * pow_2( N ) ) + "]." );
				ErrorsFound = true;
				NumNums = 0;
			}
			F = 0.0;
			for ( index = 1; index <= NumNums; index += 3 ) {
				inx1 = rNumericArgs( index );
				inx2 = rNumericArgs( index + 1 );
				F( inx1, inx2 ) = rNumericArgs( index + 2 );
			}
		}

	}

	void
	GetInputViewFactorsbyName(
		std::string const & ZoneName, // Needed to check for user input view factors.
		int const N, // NUMBER OF SURFACES
		FArray2A< Real64 > F, // USER INPUT DIRECT VIEW FACTOR MATRIX (N X N)
		FArray1A_int const SPtr, // pointer to actual surface number
		bool & NoUserInputF, // Flag signifying no input F's for this
		bool & ErrorsFound // True when errors are found in number of fields vs max args
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Curt Pedersen
		//       DATE WRITTEN   September 2005
		//       MODIFIED       Linda Lawrie;September 2010
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine gets the user view factor info.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::GetObjectItemNum;
		using InputProcessor::FindItemInList;
		using General::TrimSigDigits;

		// Argument array dimensioning
		F.dim( N, N );
		SPtr.dim( N );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int UserFZoneIndex;
		int NumAlphas;
		int NumNums;
		int IOStat;
		int index;
		int numinx1;
		int inx1;
		int inx2;
		FArray1D_string ZoneSurfaceNames;

		NoUserInputF = true;
		UserFZoneIndex = GetObjectItemNum( "ZoneProperty:UserViewFactors:bySurfaceName", ZoneName );

		if ( UserFZoneIndex > 0 ) {
			ZoneSurfaceNames.allocate( N );
			for ( index = 1; index <= N; ++index ) {
				ZoneSurfaceNames( index ) = Surface( SPtr( index ) ).Name;
			}
			NoUserInputF = false;

			GetObjectItem( "ZoneProperty:UserViewFactors:bySurfaceName", UserFZoneIndex, cAlphaArgs, NumAlphas, rNumericArgs, NumNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			if ( NumNums < pow_2( N ) ) {
				ShowSevereError( "GetInputViewFactors: " + cCurrentModuleObject + "=\"" + ZoneName + "\", not enough values." );
				ShowContinueError( "...Number of input values [" + TrimSigDigits( NumNums ) + "] is less than the required number=[" + TrimSigDigits( pow_2( N ) ) + "]." );
				ErrorsFound = true;
				NumNums = 0; // cancel getting any coordinates
			}
			F = 0.0;
			numinx1 = 0;

			for ( index = 2; index <= NumAlphas; index += 2 ) {
				inx1 = FindItemInList( cAlphaArgs( index ), ZoneSurfaceNames, N );
				inx2 = FindItemInList( cAlphaArgs( index + 1 ), ZoneSurfaceNames, N );
				if ( inx1 == 0 ) {
					ShowSevereError( "GetInputViewFactors: " + cCurrentModuleObject + "=\"" + ZoneName + "\", invalid surface name." );
					ShowContinueError( "...Surface name=\"" + cAlphaArgs( index ) + "\", not in this zone." );
					ErrorsFound = true;
				}
				if ( inx2 == 0 ) {
					ShowSevereError( "GetInputViewFactors: " + cCurrentModuleObject + "=\"" + ZoneName + "\", invalid surface name." );
					ShowContinueError( "...Surface name=\"" + cAlphaArgs( index + 2 ) + "\", not in this zone." );
					ErrorsFound = true;
				}
				++numinx1;
				if ( inx1 > 0 && inx2 > 0 ) F( inx1, inx2 ) = rNumericArgs( numinx1 );
			}
			ZoneSurfaceNames.deallocate();
		}

	}

	void
	CalcApproximateViewFactors(
		int const N, // NUMBER OF SURFACES
		FArray1A< Real64 > const A, // AREA VECTOR- ASSUMED,BE N ELEMENTS LONG
		FArray1A< Real64 > const Azimuth, // Facing angle of the surface (in degrees)
		FArray1A< Real64 > const Tilt, // Tilt angle of the surface (in degrees)
		FArray2A< Real64 > F, // APPROXIMATE DIRECT VIEW FACTOR MATRIX (N X N)
		FArray1A_int const SPtr // pointer to REAL(r64) surface number (for error message)
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Curt Pedersen
		//       DATE WRITTEN   July 2000
		//       MODIFIED       March 2001 (RKS) to disallow surfaces facing the same direction to interact radiatively
		//                      May 2002 (COP) to include INTMASS, FLOOR, ROOF and CEILING.
		//       RE-ENGINEERED  September 2000 (RKS for EnergyPlus)

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine approximates view factors using an area weighting.
		// This is improved by one degree by not allowing surfaces facing the same
		// direction to "see" each other.

		// METHODOLOGY EMPLOYED:
		// Each surface sees some area of other surfaces within the zone.  The view
		// factors from the surface to the other seen surfaces are defined by their
		// area over the summed area of seen surfaces.  Surfaces facing the same angle
		// are assumed to not be able to see each other.
		//  Modified May 2002 to cover poorly defined surface orientation.  Now all thermal masses, roofs and
		//  ceilings are "seen" by other surfaces. Floors are seen by all other surfaces, but
		//  not by other floors.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Argument array dimensioning
		A.dim( N );
		Azimuth.dim( N );
		Tilt.dim( N );
		F.dim( N, N );
		SPtr.dim( N );

		// Locals
		// SUBROUTINE ARGUMENTS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const SameAngleLimit( 10.0 ); // If the difference in the azimuth angles are above this value (degrees),
		// then the surfaces are assumed to be facing different directions.

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int i; // DO loop counters for surfaces in the zone
		int j;
		FArray1D< Real64 > ZoneArea; // Sum of the area of all zone surfaces seen

		// FLOW:
		// Calculate the sum of the areas seen by all zone surfaces
		ZoneArea.allocate( N );
		ZoneArea = 0.0;
		for ( i = 1; i <= N; ++i ) {
			for ( j = 1; j <= N; ++j ) {
				// Assumption is that a surface cannot see itself or any other surface
				// that is facing the same direction (has the same azimuth)
				//  Modified to use Class of surface to permit INTMASS to be seen by all surfaces,
				//  FLOOR to be seen by all except other floors, and ROOF and CEILING by all.
				//  Skip same surface
				if ( i == j ) continue;
				//  Include INTMASS, FLOOR(for others), CEILING, ROOF  and different facing surfaces.
				//  Roofs/ceilings always see floors
				if ( ( Surface( SPtr( j ) ).Class == SurfaceClass_IntMass ) || ( Surface( SPtr( j ) ).Class == SurfaceClass_Floor ) || ( Surface( SPtr( j ) ).Class == SurfaceClass_Roof && Surface( SPtr( i ) ).Class == SurfaceClass_Floor ) || ( ( std::abs( Azimuth( i ) - Azimuth( j ) ) > SameAngleLimit ) || ( std::abs( Tilt( i ) - Tilt( j ) ) > SameAngleLimit ) ) ) { // Everything sees internal mass surfaces | Everything except other floors sees floors

					ZoneArea( i ) += A( j );

				}
			}
			if ( ZoneArea( i ) <= 0.0 ) {
				ShowWarningError( "CalcApproximateViewFactors: Zero area for all other zone surfaces." );
				ShowContinueError( "Happens for Surface=\"" + Surface( SPtr( i ) ).Name + "\" in Zone=" + Zone( Surface( SPtr( i ) ).Zone ).Name );
			}
		}

		// Set up the approximate view factors.  First these are initialized to all zero.
		// This will clear out any junk leftover from whenever.  Then, for each zone
		// surface, set the view factor from that surface to other surfaces as the
		// area of the other surface divided by the sum of the area of all zone surfaces
		// that the original surface can actually see (calculated above).  This will
		// allow that the sum of all view factors from the original surface to all other
		// surfaces will equal unity.  F(I,J)=0 if I=J or if the surfaces face the same
		// direction.
		//  Modified to use Class of surface to permit INTMASS to be seen by all surfaces,
		//  FLOOR to be seen by all except other floors, and ROOF and CEILING by all.
		// The second IF statement is intended to avoid a divide by zero if
		// there are no other surfaces in the zone that can be seen.
		F = 0.0;
		for ( i = 1; i <= N; ++i ) {
			for ( j = 1; j <= N; ++j ) {

				//  Skip same surface

				if ( i == j ) continue;
				//  Include INTMASS, FLOOR(for others), CEILING/ROOF  and different facing surfaces.
				if ( ( Surface( SPtr( j ) ).Class == SurfaceClass_IntMass ) || ( Surface( SPtr( j ) ).Class == SurfaceClass_Floor ) || ( Surface( SPtr( j ) ).Class == SurfaceClass_Roof ) || ( ( std::abs( Azimuth( i ) - Azimuth( j ) ) > SameAngleLimit ) || ( std::abs( Tilt( i ) - Tilt( j ) ) > SameAngleLimit ) ) ) {
					if ( ZoneArea( i ) > 0.0 ) F( i, j ) = A( j ) / ( ZoneArea( i ) );
				}

			}
		}

		ZoneArea.deallocate();

	}

	void
	FixViewFactors(
		int const N, // NUMBER OF SURFACES
		FArray1A< Real64 > const A, // AREA VECTOR- ASSUMED,BE N ELEMENTS LONG
		FArray2A< Real64 > F, // APPROXIMATE DIRECT VIEW FACTOR MATRIX (N X N)
		int const ZoneNum, // Zone number being fixe
		Real64 & OriginalCheckValue, // check of SUM(F) - N
		Real64 & FixedCheckValue, // check after fixed of SUM(F) - N
		Real64 & FinalCheckValue, // the one to go with
		int & NumIterations, // number of iterations to fixed
		Real64 & RowSum // RowSum of Fixed
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Curt Pedersen
		//       DATE WRITTEN   July 2000
		//       MODIFIED       September 2000 (RKS for EnergyPlus)
		//                      April 2005,COP added capability to handle a
		//                      surface larger than sum of all others (nonenclosure)
		//                      by using a Fii view factor for that surface. Process is
		//                      now much more robust and stable.
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine fixes approximate view factors and enforces reciprocity
		// and completeness.

		// METHODOLOGY EMPLOYED:
		// A(i)*F(i,j)=A(j)*F(j,i); F(i,i)=0.; SUM(F(i,j)=1.0, j=1,N)
		// Subroutine takes approximate view factors and enforces reciprocity by
		// averaging AiFij and AjFji.  Then it determines a set of row coefficients
		// which can be multipled by each AF product to force the sum of AiFij for
		// each row to equal Ai, and applies them. Completeness is checked, and if
		// not satisfied, the AF averaging and row modifications are repeated until
		// completeness is within a preselected small deviation from 1.0
		// The routine also checks the number of surfaces and if N<=3, just enforces reciprocity.

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::RoundSigDigits;

		// Argument array dimensioning
		A.dim( N );
		F.dim( N, N );

		// Locals
		// SUBROUTINE ARGUMENTS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const PrimaryConvergence( 0.001 );
		Real64 const DifferenceConvergence( 0.00001 );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		FArray2D< Real64 > AF; // = (AREA * DIRECT VIEW FACTOR) MATRIX
		FArray2D< Real64 > AFTranspose;
		FArray2D< Real64 > AFAverage;
		FArray2D< Real64 > FixedAF;
		FArray2D< Real64 > FixedF; // CORRECTED MATRIX OF VIEW FACTORS (N X N)
		FArray2D< Real64 > FixedAFTranspose;
		FArray1D< Real64 > RowCoefficient;
		Real64 LargestArea;
		Real64 ConvrgNew;
		Real64 ConvrgOld;
		Real64 Accelerator; // RowCoefficient multipler to accelerate convergence
		Real64 CheckConvergeTolerance; // check value for actual warning

		bool Converged;
		int i;
		int j;
		static int LargestSurf( 0 );

		// FLOW:
		OriginalCheckValue = std::abs( sum( F ) - N );

		//  Allocate and zero arrays
		AF.allocate( N, N );
		AFTranspose.allocate( N, N );
		AFAverage.allocate( N, N );
		FixedAF.allocate( N, N );
		FixedAFTranspose.allocate( N, N );

		AF = 0.0;
		AFTranspose = 0.0;
		FixedAF = 0.0;
		Accelerator = 1.0;
		ConvrgOld = 10.0;
		LargestArea = maxval( A );

		FixedAF = F; // store for largest area check

		//  Check for Strange Geometry
		if ( LargestArea > ( sum( A ) - LargestArea ) ) {
			for ( i = 1; i <= N; ++i ) {
				if ( LargestArea != A( i ) ) continue;
				LargestSurf = i;
				break;
			}
			FixedAF( LargestSurf, LargestSurf ) = min( 0.9, 1.2 * LargestArea / sum( A ) ); // Give self view to big surface
		}

		//  Set up AF matrix.
		for ( i = 1; i <= N; ++i ) {
			for ( j = 1; j <= N; ++j ) {
				AF( i, j ) = FixedAF( i, j ) * A( i );
			}
		}

		//  Enforce reciprocity by averaging AiFij and AjFji
		AFTranspose = transpose( AF );
		AFAverage = 0.5 * ( AF + AFTranspose );

		FixedAF = AFAverage; //Initialize Fixed Matrix

		AF.deallocate();
		AFTranspose.deallocate();
		AFAverage.deallocate();

		FixedF.allocate( N, N );
		RowCoefficient.allocate( N );
		FixedF = 0.0;
		RowCoefficient = 1.0;

		NumIterations = 0;
		RowSum = 0.0;
		//  Check for physically unreasonable enclosures.

		if ( N <= 3 ) {
			for ( i = 1; i <= N; ++i ) {
				for ( j = 1; j <= N; ++j ) {
					FixedF( i, j ) = FixedAF( i, j ) / A( i );
				}
			}

			ShowWarningError( "Surfaces in Zone=\"" + Zone( ZoneNum ).Name + "\" do not define an enclosure." );
			ShowContinueError( "Number of surfaces <= 3, view factors are set to force reciprocity." );

			F = FixedF;
			FixedCheckValue = std::abs( sum( FixedF ) - N );
			FinalCheckValue = FixedCheckValue;
			RowSum = 0.0;
			for ( i = 1; i <= N; ++i ) {
				RowSum += sum( FixedF( i, _ ) );
			}
			Zone( ZoneNum ).EnforcedReciprocity = true;
			FixedAF.deallocate();
			FixedF.deallocate();
			FixedAFTranspose.deallocate();
			RowCoefficient.deallocate();
			return; // Do not iterate, stop with reciprocity satisfied.

		} //  N <= 3 Case

		//  Regular fix cases
		Converged = false;
		while ( ! Converged ) {
			++NumIterations;
			for ( i = 1; i <= N; ++i ) {
				// Determine row coefficients which will enforce closure.
				if ( std::abs( sum( FixedAF( i, {1,N} ) ) ) > 1.0e-10 ) {
					RowCoefficient( i ) = A( i ) / sum( FixedAF( i, {1,N} ) );
				} else {
					RowCoefficient( i ) = 1.0;
				}
				FixedAF( i, {1,N} ) *= RowCoefficient( i );
			}

			//  Enforce reciprocity by averaging AiFij and AjFji
			FixedAFTranspose = transpose( FixedAF );
			FixedAF = 0.5 * ( FixedAFTranspose + FixedAF );

			//  Form FixedF matrix
			for ( i = 1; i <= N; ++i ) {
				for ( j = 1; j <= N; ++j ) {
					FixedF( i, j ) = FixedAF( i, j ) / A( i );
					if ( std::abs( FixedF( i, j ) ) < 1.e-10 ) {
						FixedF( i, j ) = 0.0;
						FixedAF( i, j ) = 0.0;
					}
				}
			}

			ConvrgNew = std::abs( sum( FixedF ) - N );
			if ( std::abs( ConvrgOld - ConvrgNew ) < DifferenceConvergence || ConvrgNew <= PrimaryConvergence ) { //  Change in sum of Fs must be small.
				Converged = true;
			}
			ConvrgOld = ConvrgNew;
			if ( NumIterations > 400 ) { //  If everything goes bad,enforce reciprocity and go home.
				//  Enforce reciprocity by averaging AiFij and AjFji
				FixedAFTranspose = transpose( FixedAF );
				FixedAF = 0.5 * ( FixedAFTranspose + FixedAF );

				//  Form FixedF matrix
				for ( i = 1; i <= N; ++i ) {
					for ( j = 1; j <= N; ++j ) {
						FixedF( i, j ) = FixedAF( i, j ) / A( i );
					}
				}
				CheckConvergeTolerance = std::abs( sum( FixedF ) - N );
				if ( CheckConvergeTolerance > 0.005 ) {
					ShowWarningError( "FixViewFactors: View factors not complete. Check for " "bad surface descriptions or unenclosed zone=\"" + Zone( ZoneNum ).Name + "\"." );
					ShowContinueError( "Enforced reciprocity has tolerance (ideal is 0)=[" + RoundSigDigits( CheckConvergeTolerance, 6 ) + "], Row Sum (ideal is " + RoundSigDigits( N ) + ")=[" + RoundSigDigits( RowSum, 2 ) + "]." );
					ShowContinueError( "If zone is unusual, or tolerance is on the order of 0.001, view factors are probably OK." );
				}
				FixedCheckValue = std::abs( sum( FixedF ) - N );
				FinalCheckValue = FixedCheckValue;
				if ( std::abs( FixedCheckValue ) < std::abs( OriginalCheckValue ) ) {
					F = FixedF;
					FinalCheckValue = FixedCheckValue;
				}
				RowSum = 0.0;
				for ( i = 1; i <= N; ++i ) {
					RowSum += sum( FixedF( i, _ ) );
				}
				FixedAF.deallocate();
				FixedF.deallocate();
				FixedAFTranspose.deallocate();
				RowCoefficient.deallocate();
				return;
			}
		}
		FixedCheckValue = ConvrgNew;
		if ( FixedCheckValue < OriginalCheckValue ) {
			F = FixedF;
			FinalCheckValue = FixedCheckValue;
		} else {
			FinalCheckValue = OriginalCheckValue;
			RowSum = 0.0;
			for ( i = 1; i <= N; ++i ) {
				RowSum += sum( FixedF( i, _ ) );
			}
			if ( std::abs( RowSum - N ) < PrimaryConvergence ) {
				F = FixedF;
				FinalCheckValue = FixedCheckValue;
			} else {
				ShowWarningError( "FixViewFactors: View factors not complete. Check for " "bad surface descriptions or unenclosed zone=\"" + Zone( ZoneNum ).Name + "\"." );
			}
		}

		FixedAF.deallocate();
		FixedF.deallocate();
		FixedAFTranspose.deallocate();
		RowCoefficient.deallocate();

	}

	void
	CalcScriptF(
		int const N, // Number of surfaces
		FArray1< Real64 > const & A, // AREA VECTOR- ASSUMED,BE N ELEMENTS LONG
		FArray2< Real64 > const & F, // DIRECT VIEW FACTOR MATRIX (N X N)
		FArray1< Real64 > & EMISS, // VECTOR OF SURFACE EMISSIVITIES
		FArray2< Real64 > & ScriptF // MATRIX OF SCRIPT F FACTORS (N X N)
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Curt Pedersen
		//       DATE WRITTEN   1980
		//       MODIFIED       July 2000 (COP for the ASHRAE Loads Toolkit)
		//       RE-ENGINEERED  September 2000 (RKS for EnergyPlus)
		//       RE-ENGINEERED  June 2014 (Stuart Mentzer): Performance tuned

		// PURPOSE OF THIS SUBROUTINE:
		// Determines Hottel's ScriptF coefficients which account for the total
		// grey interchange between surfaces in an enclosure.

		// METHODOLOGY EMPLOYED:
		// See reference

		// REFERENCES:
		// Hottel, H. C. and A. F. Sarofim, Radiative Transfer, Ch 3, McGraw Hill, 1967.

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENTS:
		// --Must satisfy reciprocity and completeness:
		//  A(i)*F(i,j)=A(j)*F(j,i); F(i,i)=0.; SUM(F(i,j)=1.0, j=1,N)

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const MaxEmissLimit( 0.99999 ); // Limit the emissivity internally/avoid a divide by zero error

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// Validate argument array dimensions
		assert( N >= 0 ); // Do we need to allow for N==0?
		assert( ( A.l() == 1 ) && ( A.u() == N ) );
		assert( ( F.l1() == 1 ) && ( F.u1() == N ) );
		assert( ( F.l2() == 1 ) && ( F.u2() == N ) );
		assert( ( EMISS.l() == 1 ) && ( EMISS.u() == N ) );
		assert( equal_dimensions( F, ScriptF ) );

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// FLOW:

#ifdef EP_Count_Calls
		++NumCalcScriptF_Calls;
#endif

		// Load Cmatrix with AF (AREA * DIRECT VIEW FACTOR) matrix
		FArray2D< Real64 > Cmatrix( N, N ); // = (AF - EMISS/REFLECTANCE) matrix (but plays other roles)
		assert( equal_dimensions( Cmatrix, F ) ); // For linear indexing
		FArray2D< Real64 >::size_type l( 0u );
		for ( int j = 1; j <= N; ++j ) {
			for ( int i = 1; i <= N; ++i, ++l ) {
				Cmatrix[ l ] = A( i ) * F[ l ]; // [ l ] == ( i, j )
			}
		}

		// Load Cmatrix with (AF - EMISS/REFLECTANCE) matrix
		FArray1D< Real64 > Excite( N ); // Excitation vector = A*EMISS/REFLECTANCE
		l = 0u;
		for ( int i = 1; i <= N; ++i, l += N + 1 ) {
			Real64 EMISS_i( EMISS( i ) );
			if ( EMISS_i > MaxEmissLimit ) { // Check/limit EMISS for this surface to avoid divide by zero below
				EMISS_i = EMISS( i ) = MaxEmissLimit;
				ShowWarningError( "A thermal emissivity above 0.99999 was detected. This is not allowed. Value was reset to 0.99999" );
			}
			Real64 const EMISS_i_fac( A( i ) / ( 1.0 - EMISS_i ) );
			Excite( i ) = -EMISS_i * EMISS_i_fac; // Set up matrix columns for partial radiosity calculation
			Cmatrix[ l ] -= EMISS_i_fac; // Coefficient matrix for partial radiosity calculation // [ l ] == ( i, i )
		}

		FArray2D< Real64 > Cinverse( N, N ); // Inverse of Cmatrix
		CalcMatrixInverse( Cmatrix, Cinverse ); // SOLVE THE LINEAR SYSTEM
		Cmatrix.clear(); // Release memory ASAP

		// Scale Cinverse colums by excitation to get partial radiosity matrix
		l = 0u;
		for ( int j = 1; j <= N; ++j ) {
			Real64 const e_j( Excite( j ) );
			for ( int i = 1; i <= N; ++i, ++l ) {
				Cinverse[ l ] *= e_j; // [ l ] == ( i, j )
			}
		}
		Excite.clear(); // Release memory ASAP

		// Form Script F matrix
		assert( equal_dimensions( Cinverse, ScriptF ) ); // For linear indexing
		for ( int i = 1; i <= N; ++i ) { // Inefficient order for cache but can reuse multiplier so faster choice depends on N
			Real64 const EMISS_i( EMISS( i ) );
			Real64 const EMISS_fac( EMISS_i / ( 1.0 - EMISS_i ) );
			l = static_cast< FArray2D< Real64 >::size_type >( i - 1 );
			for ( int j = 1; j <= N; ++j, l += N ) {
				if ( i == j ) {
					//        ScriptF(I,J) = EMISS(I)/(1.0d0-EMISS(I))*(Jmatrix(I,J)-Delta*EMISS(I)), where Delta=1
					ScriptF[ l ] = EMISS_fac * ( Cinverse[ l ] - EMISS_i ); // [ l ] = ( i, j )
				} else {
					//        ScriptF(I,J) = EMISS(I)/(1.0d0-EMISS(I))*(Jmatrix(I,J)-Delta*EMISS(I)), where Delta=0
					ScriptF[ l ] = EMISS_fac * Cinverse[ l ]; // [ l ] == ( i, j )
				}
			}
		}

	}

	void
	CalcMatrixInverse(
		FArray2< Real64 > & A, // Matrix: Gets reduced to L\U form
		FArray2< Real64 > & I // Returned as inverse matrix
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jakob Asmundsson
		//       DATE WRITTEN   January 1999
		//       MODIFIED       September 2000 (RKS for EnergyPlus)
		//       RE-ENGINEERED  June 2014 (Stuart Mentzer): Performance/memory tuning rewrite

		// PURPOSE OF THIS SUBROUTINE:
		// To find the inverse of Matrix, using partial pivoting.

		// METHODOLOGY EMPLOYED:
		// Inverse is found using partial pivoting and Gauss elimination

		// REFERENCES:
		// Any Linear Algebra book

		// Validation
		assert( A.square() );
		assert( A.I1() == A.I2() );
		assert( equal_dimensions( A, I ) );

		// Initialization
		int const l( A.l1() );
		int const u( A.u1() );
		int const n( u - l + 1 );
		I.to_identity(); // I starts out as identity

		// Could do row scaling here to improve condition and then check min pivot isn't too small

		// Compute in-place LU decomposition of [A|I] with row pivoting
		for ( int i = l; i <= u; ++i ) {

			// Find pivot row in column i below diagonal
			int iPiv = i;
			Real64 aPiv( std::abs( A( i, i ) ) );
			auto ki( A.index( i + 1, i ) );
			for ( int k = i + 1; k <= u; ++k, ++ki ) {
				Real64 const aAki( std::abs( A[ ki ] ) ); // [ ki ] == ( k, i )
				if ( aAki > aPiv ) {
					iPiv = k;
					aPiv = aAki;
				}
			}
			assert( aPiv != 0.0 ); //? Is zero pivot possible for some user inputs? If so if test/handler needed

			// Swap row i with pivot row
			if ( iPiv != i ) {
				auto ij( A.index( i, l ) ); // [ ij ] == ( i, j )
				auto pj( A.index( iPiv, l ) ); // [ pj ] == ( iPiv, j )
				for ( int j = l; j <= u; ++j, ij += n, pj += n ) {
					Real64 const Aij( A[ ij ] );
					A[ ij ] = A[ pj ];
					A[ pj ] = Aij;
					Real64 const Iij( I[ ij ] );
					I[ ij ] = I[ pj ];
					I[ pj ] = Iij;
				}
			}

			// Put multipliers in column i and reduce block below A(i,i)
			Real64 const Aii_inv( 1.0 / A( i, i ) );
			for ( int k = i + 1; k <= u; ++k ) {
				Real64 const multiplier( A( k, i ) * Aii_inv );
				A( k, i ) = multiplier;
				if ( multiplier != 0.0 ) {
					auto ij( A.index( i, i + 1 ) ); // [ ij ] == ( i, j )
					auto kj( A.index( k, i + 1 ) ); // [ kj ] == ( k, j )
					for ( int j = i + 1; j <= u; ++j, ij += n, kj += n ) {
						A[ kj ] -= multiplier * A[ ij ];
					}
					ij = A.index( i, l );
					kj = A.index( k, l );
					for ( int j = l; j <= u; ++j, ij += n, kj += n ) {
						Real64 const Iij( I[ ij ] );
						if ( Iij != 0.0 ) {
							I[ kj ] -= multiplier * Iij;
						}
					}
				}
			}

		}

		// Perform back-substitution on [U|I] to put inverse in I
		for ( int k = u; k >= l; --k ) {
			Real64 const Akk_inv( 1.0 / A( k, k ) );
			auto kj( A.index( k, l ) ); // [ kj ] == ( k, j )
			for( int j = l; j <= u; ++j, kj += n ) {
				I[ kj ] *= Akk_inv;
			}
			auto ik( A.index( l, k ) ); // [ ik ] == ( i, k )
			for ( int i = l; i < k; ++i, ++ik ) { // Eliminate kth column entries from I in rows above k
				Real64 const Aik( A[ ik ] );
				auto ij( A.index( i, l ) ); // [ ij ] == ( i, j )
				auto kj( A.index( k, l ) ); // [ kj ] == ( k, j )
				for( int j = l; j <= u; ++j, ij += n, kj += n ) {
					I[ ij ] -= Aik * I[ kj ];
				}
			}
		}

	}

	//     NOTICE

	//     Copyright © 1996-2014 The Board of Trustees of the University of Illinois
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

} // HeatBalanceIntRadExchange

} // EnergyPlus
