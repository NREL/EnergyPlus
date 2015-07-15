// C++ Headers
#include <cassert>
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
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

	// Amir Roth 2017-07-01: __ep_assume_aligned is a portable
	// compiler hint that tells the compiler that address A of
	// type T is aligned to an N byte boundary.  Alignment hints
	// are used by some compilers to generate optimized vector
	// memory load/store code.

	// TODO: move to a header file somewhere
	// TODO: add MSVC support once MSVC "catches up"
	// TODO: simplify macro by combining with __restrict?
	// TODO: encapsulate in aligned/padded ArrayXD templates?

#if defined(__GNUC__) && !defined(__clang__)
#define __ep_assume_aligned(T, A, N) A = (T)__builtin_assume_aligned(A, N)
#elif defined(__INTEL_COMPILER) 
#define __ep_assume_aligned(T, A, N) __assume_aligned(A, N)
#else
#define __ep_assume_aligned(T, A, N)
#endif 

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
	static gio::Fmt fmtLD( "*" );
	static gio::Fmt fmtA( "(A)" );
	static gio::Fmt fmtx( "(A,I4,1x,A,1x,6f16.8)" );
	static gio::Fmt fmty( "(A,1x,6f16.8)" );

	// DERIVED TYPE DEFINITIONS
	// na

	// MODULE VARIABLE DECLARATIONS:
	int MaxNumOfZoneSurfaces; // Max saved to get large enough space for user input view factors

	// SUBROUTINE SPECIFICATIONS FOR MODULE HeatBalanceIntRadExchange

	// Functions
	void
	CalcScriptF(
		int const N, // Number of surfaces
		Array1< Real64 > const & A, // AREA VECTOR- ASSUMED,BE N ELEMENTS LONG
		Array2< Real64 > const & F, // DIRECT VIEW FACTOR MATRIX (N X N)
		Array1< Real64 > & EMISS, // VECTOR OF SURFACE EMISSIVITIES
		Real64 * ScriptF
	);

	void
	CalcInteriorRadExchange(
		Array1S< Real64 > const SurfaceTemp, // Current surface temperatures
		int const SurfIterations, // Number of iterations in calling subroutine
		Array1< Real64 > & NetLWRadToSurf, // Net long wavelength radiant exchange from other surfaces
		Optional_int_const ZoneToResimulate, // if passed in, then only calculate for this zone
		std::string const & EP_UNUSED( CalledFrom )
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

		// Types

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
		static gio::Fmt fmtLD( "*" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static bool firstTime( true ); // Logical flag for one-time initializations
	
		int ConstrNumRec; // Receiving surface construction number
		int SurfNum; // Surface number
		int ConstrNum; // Construction number
		bool IntShadeOrBlindStatusChanged; // True if status of interior shade or blind on at least
		// one window in a zone has changed from previous time step
		int ShadeFlag; // Window shading status current time step
		int ShadeFlagPrev; // Window shading status previous time step

		//variables added as part of strategy to reduce calculation time - Glazer 2011-04-22
		static Array1D< Real64 > SurfaceTempK4;
		static Array1D< Real64 > SurfaceEmiss;

		// Amir Roth 2015-07-01: variables added as part of
		// vectorization strategy - could be eliminated if
		// Surface array is resorted so that window and
		// non-window surfaces are contiguous within a zone
		static Array1D< Real64 > IRfromParentZone_Temp;
		static Array1D< Real64 > NetLWRadToSurf_Temp;

		// FLOW:

#ifdef EP_Detailed_Timings
		epStartTime( "CalcInteriorRadExchange=" );
#endif
		if ( firstTime ) {
			InitInteriorRadExchange();
			SurfaceTempK4.allocate( MaxNumOfZoneSurfaces );
			SurfaceEmiss.allocate( MaxNumOfZoneSurfaces );

			NetLWRadToSurf_Temp.allocate( MaxNumOfZoneSurfaces );
			IRfromParentZone_Temp.allocate( MaxNumOfZoneSurfaces );

			firstTime = false;
			if ( DeveloperFlag ) {
				std::string tdstring;
				gio::write( tdstring, fmtLD ) << " OMP turned off, HBIRE loop executed in serial";
				DisplayString( tdstring );
			}
		}

		if ( KickOffSimulation || KickOffSizing ) return;

		bool const PartialResimulate( present( ZoneToResimulate ) );

#ifdef EP_Count_Calls
		if ( ! PartialResimulate ) {
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
		if ( PartialResimulate ) {
			ZoneData const & zone( Zone( ZoneToResimulate ) );
			NetLWRadToSurf( {zone.SurfaceFirst,zone.SurfaceLast} ) = 0.0;
			SurfaceWindow( {zone.SurfaceFirst,zone.SurfaceLast} ).IRfromParentZone() = 0.0;
		} else {
			NetLWRadToSurf = 0.0;
			SurfaceWindow.IRfromParentZone() = 0.0;
		}

		for ( int ZoneNum = ( PartialResimulate ? ZoneToResimulate() : 1 ), ZoneNum_end = ( PartialResimulate ? ZoneToResimulate() : NumOfZones ); ZoneNum <= ZoneNum_end; ++ZoneNum ) {

			ZoneData const & zone( Zone( ZoneNum ) );
			ZoneViewFactorInformation & zvfi( ZoneInfo( ZoneNum ) );

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
						if ( ( ShadeFlagPrev != IntShadeOn && ShadeFlag == IntShadeOn ) ||  ( ShadeFlagPrev != IntBlindOn && ShadeFlag == IntBlindOn ) || 
						     ( ShadeFlagPrev == IntShadeOn && ShadeFlag != IntShadeOn ) || ( ShadeFlagPrev == IntBlindOn && ShadeFlag != IntBlindOn ) ) 
							IntShadeOrBlindStatusChanged = true;
					}
				}

				if ( IntShadeOrBlindStatusChanged || BeginEnvrnFlag ) { // Calc inside surface emissivities for this time step
					for ( int ZoneSurfNum = 1; ZoneSurfNum <= zvfi.NumOfSurfaces; ++ZoneSurfNum ) {
						SurfNum = zvfi.SurfacePtr( ZoneSurfNum );
						ConstrNum = Surface( SurfNum ).Construction;
						zvfi.Emissivity( ZoneSurfNum ) = Construct( ConstrNum ).InsideAbsorpThermal;
						auto const & surface_window( SurfaceWindow( SurfNum ) );
						if ( Construct( ConstrNum ).TypeIsWindow && ( surface_window.ShadingFlag == IntShadeOn || surface_window.ShadingFlag == IntBlindOn ) ) {
							zvfi.Emissivity( ZoneSurfNum ) = 
								InterpSlatAng( surface_window.SlatAngThisTS, surface_window.MovableSlats, surface_window.EffShBlindEmiss ) + 
								InterpSlatAng( surface_window.SlatAngThisTS, surface_window.MovableSlats, surface_window.EffGlassEmiss );
						}
					}

					CalcScriptF( zvfi.NumOfSurfaces, zvfi.Area, zvfi.F, zvfi.Emissivity, zvfi.ScriptF );
					// precalc - multiply by StefanBoltzmannConstant

					int Npad = (( zvfi.NumOfSurfaces + 1) >> 1 ) << 1;
					for (int i = 0; i < zvfi.NumOfSurfaces; ++i) {
						Real64 * __restrict ScriptFi0 ( &zvfi.ScriptF[(i*Npad)] );
						__ep_assume_aligned(Real64 *, ScriptFi0, 16);
						for (int j = 0; j < zvfi.NumOfSurfaces; ++j)
							ScriptFi0[j] *= StefanBoltzmannConst;
					} // for i
				}

			} // End of check if SurfIterations = 0

			// precalculate the fourth power of surface temperature as part of strategy to reduce calculation time - Glazer 2011-04-22
			for ( int ZoneSurfNum = 0; ZoneSurfNum < zvfi.NumOfSurfaces; ++ZoneSurfNum ) {
				int SurfNum = zvfi.SurfacePtr[ ZoneSurfNum ];
				auto const & window( SurfaceWindow( SurfNum ) );
				int ConstrNum = Surface( SurfNum ).Construction;
				ConstructionData const & construct( Construct( ConstrNum ) );
				if ( construct.WindowTypeEQL ) {
					SurfaceTempK4[ ZoneSurfNum ] = window.EffInsSurfTemp;
					SurfaceEmiss[ ZoneSurfNum ] = EQLWindowInsideEffectiveEmiss( ConstrNum );
				} else if ( construct.TypeIsWindow && window.OriginalClass != SurfaceClass_TDD_Diffuser ) {
					if ( SurfIterations == 0 && window.ShadingFlag <= 0 ) {
						// If the window is bare this TS and it is the first time through we use the previous TS glass
						// temperature whether or not the window was shaded in the previous TS. If the window was shaded
						// the previous time step this temperature is a better starting value than the shade temperature.
						SurfaceTempK4[ ZoneSurfNum ] = window.ThetaFace( 2 * construct.TotGlassLayers ) - KelvinConv;
						SurfaceEmiss[ ZoneSurfNum ] = construct.InsideAbsorpThermal;
					} else if ( window.ShadingFlag == IntShadeOn || window.ShadingFlag == IntBlindOn ) {
						// For windows with an interior shade or blind an effective inside surface temp
						// and emiss is used here that is a weighted combination of shade/blind and glass temp and emiss.
						SurfaceTempK4[ ZoneSurfNum ] = window.EffInsSurfTemp;
						SurfaceEmiss[ ZoneSurfNum ] = 
							InterpSlatAng( window.SlatAngThisTS, window.MovableSlats, window.EffShBlindEmiss ) + 
							InterpSlatAng( window.SlatAngThisTS, window.MovableSlats, window.EffGlassEmiss );					
					} else {
						SurfaceTempK4[ ZoneSurfNum ] = SurfaceTemp( SurfNum );
						SurfaceEmiss[ ZoneSurfNum ] = construct.InsideAbsorpThermal;
					}
				} else {
					SurfaceTempK4[ ZoneSurfNum ] = SurfaceTemp( SurfNum );
					SurfaceEmiss[ ZoneSurfNum ] = construct.InsideAbsorpThermal;
				}
			} // for ZoneSurfNum

			// Amir Roth 2015-07-01: Split off SurfaceTemp = pow4(SurfaceTemp) calculation so that it will vectorize.

			// __restrict is a hint that tells the compiler that the array vecSurfaceTempK4 does not overlap in memory with 
			// any other array, this helps the compiler decide that the code is vectorizable.
			Real64 * __restrict vecSurfaceTempK4( &SurfaceTempK4[ 0 ] );
			// __ep_assume_aligned tells the compiler that vecSurfaceTempK4 is aligned in memory allowing it to generate
			// optimized vector load/store code. 
			__ep_assume_aligned(Real64 *, vecSurfaceTempK4, 16);
			// Both __restrict and __assume_aligned hints require raw pointers, hence need to use raw pointers here.  These 
			// pointers do not manage or "own" memory, they simply point at memory in a way that allows the compiler to optimize
			// better
			for ( int ZoneSurfNum = 0; ZoneSurfNum < zvfi.NumOfSurfaces; ++ZoneSurfNum ) {
				vecSurfaceTempK4[ ZoneSurfNum ] = pow_4( vecSurfaceTempK4 [ ZoneSurfNum ] + KelvinConv );
			} // for ZoneSurfNum


			// See comments above for explanation of __restrict and __ep_assume_aligned
			Real64 * __restrict vecNetLWRadToSurf_Temp( &NetLWRadToSurf_Temp[ 0 ] );
			__ep_assume_aligned(Real64 *, vecNetLWRadToSurf_Temp, 16);
			Real64 * __restrict vecIRfromParentZone_Temp( &IRfromParentZone_Temp[ 0 ] );
			__ep_assume_aligned(Real64 *, vecIRfromParentZone_Temp, 16);

			for (int ZoneSurfNum = 0; ZoneSurfNum < zvfi.NumOfSurfaces; ++ZoneSurfNum) {
				vecNetLWRadToSurf_Temp[ZoneSurfNum] = 0.0;
				vecIRfromParentZone_Temp[ZoneSurfNum] = 0.0;
			} // ZoneSurfNum

			// These are the money loops
			
			// Amir Roth 2015-07-01: vectorize the inner loop for performance.  Made SendZoneSurfNum the outer loop to enable 
			// vectorization.
			for ( int SendZoneSurfNum = 0; SendZoneSurfNum < zvfi.NumOfSurfaces; ++SendZoneSurfNum ) {

				int RecZoneSurfNum = 0;

				// See comments above for explanation of __restrict and __ep_assume_aligned vectorization hints
				Real64 * __restrict vecNetLWRadToSurf_Temp( &NetLWRadToSurf_Temp[ RecZoneSurfNum ] );
				__ep_assume_aligned(Real64 *, vecNetLWRadToSurf_Temp, 16);
				Real64 * __restrict vecIRfromParentZone_Temp( &IRfromParentZone_Temp[ RecZoneSurfNum ] );
				__ep_assume_aligned(Real64 *, vecIRfromParentZone_Temp, 16);
				Real64 * __restrict vecSurfaceTempK4( &SurfaceTempK4[ RecZoneSurfNum ] );
				__ep_assume_aligned(Real64 *, vecSurfaceTempK4, 16);

				// ScriptF is a "hand-rolled" padded 2D array.  Npad is the size of a padded row.
				int Npad = (( zvfi.NumOfSurfaces + 1 ) >> 1 ) << 1;
				Real64 * __restrict vecZvfiScriptF( &zvfi.ScriptF[ (SendZoneSurfNum * Npad) + RecZoneSurfNum ] );
				__ep_assume_aligned(Real64 *, vecZvfiScriptF, 16);

			
				// Calculate net long-wave radiation for opaque surfaces and incident
				// long-wave radiation for windows.

				// Amir Roth 2015-07-01: vectorized inner loop.
				for ( ; RecZoneSurfNum < zvfi.NumOfSurfaces; ++RecZoneSurfNum ) {
					// Calculate interior LW incident on window rather than net LW for use in window layer heat balance calculation.

					vecIRfromParentZone_Temp[ RecZoneSurfNum ] += vecZvfiScriptF[ RecZoneSurfNum ] * vecSurfaceTempK4[ SendZoneSurfNum ];
					vecNetLWRadToSurf_Temp[ RecZoneSurfNum ] += vecZvfiScriptF[ RecZoneSurfNum ] * ( vecSurfaceTempK4[ SendZoneSurfNum ] - vecSurfaceTempK4[ RecZoneSurfNum ] ); 					
					// Per BG -- this should never happened.  (CR6346,CR6550 caused this to be put in.  Now removed. LKL 1/2013)
					//          IF (SurfaceWindow(RecSurfNum)%IRfromParentZone < 0.0) THEN
					//            CALL ShowRecurringWarningErrorAtEnd('CalcInteriorRadExchange: Window_IRFromParentZone negative, Window="'// &
					//                TRIM(Surface(RecSurfNum)%Name)//'"',  &
					//                SurfaceWindow(RecSurfNum)%IRErrCount)
					//            CALL ShowRecurringContinueErrorAtEnd('..occurs in Zone="'//TRIM(Surface(RecSurfNum)%ZoneName)//  &
					//                '", reset to 0.0 for remaining calculations.',SurfaceWindow(RecSurfNum)%IRErrCountC)
					//            SurfaceWindow(RecSurfNum)%IRfromParentZone=0.0
					//          ENDIF
					

				} // for SendZoneSurfNum
			} // for RecZoneSurfNum


			// Amir Roth 2015-07-01: because loops with conditionals will not vectorize and because "money" Send->Recv inner loop had a 
			// conditional, pulled that conditional out and implemented it here.
			for ( int ZoneSurfNum = 0; ZoneSurfNum < zvfi.NumOfSurfaces; ++ZoneSurfNum ) {
				int SurfNum = zvfi.SurfacePtr[ ZoneSurfNum ];
				NetLWRadToSurf( SurfNum ) += NetLWRadToSurf_Temp[ ZoneSurfNum ];
				if ( Construct( Surface( SurfNum ).Construction ).TypeIsWindow ) 
					SurfaceWindow( SurfNum ).IRfromParentZone += IRfromParentZone_Temp[ ZoneSurfNum ] / SurfaceEmiss [ ZoneSurfNum ];
			} // for ZoneSurfNum
		} // for ZoneNum

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
		static gio::Fmt AFormat( "(A)" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumOfZoneSurfaces; // total number of surfaces in the zone.
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
		Array2D< Real64 > SaveApproximateViewFactors; // Save for View Factor reporting
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
			gio::write( OutputFileInits, fmtA ) << "! <View Factor - Surface Information>,Surface Name,Surface Class,Area {m2},Azimuth,Tilt,Thermal Emissivity,#Sides,Vertices";
			gio::write( OutputFileInits, fmtA ) << "! <View Factor / Grey Interchange Type>,Surface Name(s)";
			gio::write( OutputFileInits, fmtA ) << "! <View Factor>,Surface Name,Surface Class,Row Sum,View Factors for each Surface";
		}

		cCurrentModuleObject = "ZoneProperty:UserViewFactors:bySurfaceName";
		NumZonesWithUserFbyS = GetNumObjectsFound( cCurrentModuleObject );

		MaxNumOfZoneSurfaces = 0;
		for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {

			if ( ZoneNum == 1 ) {
				if ( DisplayAdvancedReportVariables ) gio::write( OutputFileInits, fmtA ) << "! <Surface View Factor Check Values>,Zone Name,Original Check Value,Calculated Fixed Check Value,Final Check Value,Number of Iterations,Fixed RowSum Convergence,Used RowSum Convergence";
			}

			ZoneInfo( ZoneNum ).Name = Zone( ZoneNum ).Name;

			NumOfZoneSurfaces = 0;
			for ( int SurfNum = Zone( ZoneNum ).SurfaceFirst, SurfNum_end = Zone( ZoneNum ).SurfaceLast; SurfNum <= SurfNum_end; ++SurfNum ) {
				if ( Surface( SurfNum ).HeatTransSurf ) ++NumOfZoneSurfaces;
			}
			ZoneInfo( ZoneNum ).NumOfSurfaces = NumOfZoneSurfaces;
			MaxNumOfZoneSurfaces = max( MaxNumOfZoneSurfaces, NumOfZoneSurfaces );
			if ( NumOfZoneSurfaces < 1 ) ShowFatalError( "No surfaces in a zone in InitInteriorRadExchange" );

			// Allocate the parts of the derived type
			ZoneInfo( ZoneNum ).F.dimension( NumOfZoneSurfaces, NumOfZoneSurfaces, 0.0 );

			int Npad = (( NumOfZoneSurfaces + 1 ) >> 1 ) << 1;
			ZoneInfo( ZoneNum ).ScriptF = new Real64[ NumOfZoneSurfaces * Npad ];
			for (int i = 0; i < NumOfZoneSurfaces * Npad; ++i) {
				ZoneInfo(ZoneNum).ScriptF[i] = 0.0;
			}

			ZoneInfo( ZoneNum ).Area.dimension( NumOfZoneSurfaces, 0.0 );
			ZoneInfo( ZoneNum ).Emissivity.dimension( NumOfZoneSurfaces, 0.0 );
			ZoneInfo( ZoneNum ).Azimuth.dimension( NumOfZoneSurfaces, 0.0 );
			ZoneInfo( ZoneNum ).Tilt.dimension( NumOfZoneSurfaces, 0.0 );
			ZoneInfo( ZoneNum ).SurfacePtr.dimension( NumOfZoneSurfaces, 0 );

			// Initialize the surface pointer array
			ZoneSurfNum = 0;
			for ( int SurfNum = Zone( ZoneNum ).SurfaceFirst, SurfNum_end = Zone( ZoneNum ).SurfaceLast; SurfNum <= SurfNum_end; ++SurfNum ) {
				if ( ! Surface( SurfNum ).HeatTransSurf ) continue;
				++ZoneSurfNum;
				ZoneInfo( ZoneNum ).SurfacePtr( ZoneSurfNum ) = SurfNum;
			}
			// Initialize the area and emissivity arrays
			for ( ZoneSurfNum = 1; ZoneSurfNum <= NumOfZoneSurfaces; ++ZoneSurfNum ) {
				int const SurfNum = ZoneInfo( ZoneNum ).SurfacePtr( ZoneSurfNum );

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

			if ( NumOfZoneSurfaces == 1 ) {
				// If there is only one surface in a zone, then there is no radiant exchange
				ZoneInfo( ZoneNum ).F = 0.0;
				ZoneInfo( ZoneNum ).ScriptF[0] = 0.0;
				if ( DisplayAdvancedReportVariables ) gio::write( OutputFileInits, fmtA ) << "Surface View Factor Check Values," + Zone( ZoneNum ).Name + ",0,0,0,-1,0,0";
				continue; // Go to the next zone in the  ZoneNum DO loop
			}

			//  Get user supplied view factors if available in idf.

			NoUserInputF = true;

			if ( NumZonesWithUserFbyS > 0 ) {

				GetInputViewFactorsbyName( ZoneInfo( ZoneNum ).Name, NumOfZoneSurfaces, ZoneInfo( ZoneNum ).F, ZoneInfo( ZoneNum ).SurfacePtr, NoUserInputF, ErrorsFound ); // Obtains user input view factors from input file
			}

			if ( NoUserInputF ) {

				// Calculate the view factors and make sure they satisfy reciprocity
				CalcApproximateViewFactors( NumOfZoneSurfaces, ZoneInfo( ZoneNum ).Area, ZoneInfo( ZoneNum ).Azimuth, ZoneInfo( ZoneNum ).Tilt, ZoneInfo( ZoneNum ).F, ZoneInfo( ZoneNum ).SurfacePtr );
			}

			if ( ViewFactorReport ) { // Allocate and save user or approximate view factors for reporting.
				SaveApproximateViewFactors.allocate( NumOfZoneSurfaces, NumOfZoneSurfaces );
				SaveApproximateViewFactors = ZoneInfo( ZoneNum ).F;
			}

			FixViewFactors( NumOfZoneSurfaces, ZoneInfo( ZoneNum ).Area, ZoneInfo( ZoneNum ).F, ZoneNum, CheckValue1, CheckValue2, FinalCheckValue, NumIterations, FixedRowSum );

			// Calculate the script F factors
			CalcScriptF( NumOfZoneSurfaces, ZoneInfo( ZoneNum ).Area, ZoneInfo( ZoneNum ).F, ZoneInfo( ZoneNum ).Emissivity, ZoneInfo( ZoneNum ).ScriptF );

			if ( ViewFactorReport ) { // Write to SurfInfo File
				// Zone Surface Information Output
				gio::write( OutputFileInits, fmtA ) << "Surface View Factor - Zone Information," + ZoneInfo( ZoneNum ).Name + ',' + RoundSigDigits( NumOfZoneSurfaces );

				for ( int SurfNum = 1; SurfNum <= NumOfZoneSurfaces; ++SurfNum ) {
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
				for ( int SurfNum = 1; SurfNum <= NumOfZoneSurfaces; ++SurfNum ) {
					gio::write( OutputFileInits, "(',',A,$)" )
						<< Surface( ZoneInfo( ZoneNum ).SurfacePtr( SurfNum ) ).Name;
				} gio::write( OutputFileInits );

				for ( Findex = 1; Findex <= NumOfZoneSurfaces; ++Findex ) {
					RowSum = sum( SaveApproximateViewFactors( _, Findex ) );
					gio::write( OutputFileInits, "(A,3(',',A),$)" )
						<< "View Factor"
						<< Surface( ZoneInfo( ZoneNum ).SurfacePtr( Findex ) ).Name
						<< cSurfaceClass( Surface( ZoneInfo( ZoneNum ).SurfacePtr( Findex ) ).Class )
						<< RoundSigDigits( RowSum, 4 );
					for ( int SurfNum = 1; SurfNum <= NumOfZoneSurfaces; ++SurfNum ) {
						gio::write( OutputFileInits, "(',',A,$)" )
							<< RoundSigDigits( SaveApproximateViewFactors( SurfNum, Findex ), 4 );
					} gio::write( OutputFileInits );
				}
			}

			if ( ViewFactorReport ) {
				gio::write( OutputFileInits, "(A,A,$)" ) << "Final ViewFactors" << ",To Surface,Surface Class,RowSum";
				for ( int SurfNum = 1; SurfNum <= NumOfZoneSurfaces; ++SurfNum ) {
					gio::write( OutputFileInits, "(',',A,$)" ) << Surface( ZoneInfo( ZoneNum ).SurfacePtr( SurfNum ) ).Name;
				} gio::write( OutputFileInits );

				for ( Findex = 1; Findex <= NumOfZoneSurfaces; ++Findex ) {
					RowSum = sum( ZoneInfo( ZoneNum ).F( _, Findex ) );
					gio::write( OutputFileInits, "(A,3(',',A),$)" )
						<< "View Factor"
						<< Surface( ZoneInfo( ZoneNum ).SurfacePtr( Findex ) ).Name
						<< cSurfaceClass( Surface( ZoneInfo( ZoneNum ).SurfacePtr( Findex ) ).Class )
						<< RoundSigDigits( RowSum, 4 );
					for ( int SurfNum = 1; SurfNum <= NumOfZoneSurfaces; ++SurfNum ) {
						gio::write( OutputFileInits, "(',',A,$)" ) << RoundSigDigits( ZoneInfo( ZoneNum ).F( SurfNum, Findex ), 4 );
					} gio::write( OutputFileInits );
				}

				if ( Option1 == "IDF" ) {
					gio::write( OutputFileDebug, fmtA ) << "!======== original input factors ===========================";
					gio::write( OutputFileDebug, fmtA ) << "ZoneProperty:UserViewFactors:bySurfaceName," + ZoneInfo( ZoneNum ).Name + ',';
					for ( int SurfNum = 1; SurfNum <= NumOfZoneSurfaces; ++SurfNum ) {
						for ( Findex = 1; Findex <= NumOfZoneSurfaces; ++Findex ) {
							if ( ! ( SurfNum == NumOfZoneSurfaces && Findex == NumOfZoneSurfaces ) ) {
								gio::write( OutputFileDebug, fmtA ) << "  " + Surface( ZoneInfo( ZoneNum ).SurfacePtr( SurfNum ) ).Name + ',' + Surface( ZoneInfo( ZoneNum ).SurfacePtr( Findex ) ).Name + ',' + RoundSigDigits( ZoneInfo( ZoneNum ).F( Findex, SurfNum ), 6 ) + ',';
							} else {
								gio::write( OutputFileDebug, fmtA ) << "  " + Surface( ZoneInfo( ZoneNum ).SurfacePtr( SurfNum ) ).Name + ',' + Surface( ZoneInfo( ZoneNum ).SurfacePtr( Findex ) ).Name + ',' + RoundSigDigits( ZoneInfo( ZoneNum ).F( Findex, SurfNum ), 6 ) + ';';
							}
						}
					}
					gio::write( OutputFileDebug, fmtA ) << "!============= end of data ======================";

					gio::write( OutputFileDebug, fmtA ) << "!============ final view factors =======================";
					gio::write( OutputFileDebug, fmtA ) << "ZoneProperty:UserViewFactors:bySurfaceName," + ZoneInfo( ZoneNum ).Name + ',';
					for ( int SurfNum = 1; SurfNum <= NumOfZoneSurfaces; ++SurfNum ) {
						for ( Findex = 1; Findex <= NumOfZoneSurfaces; ++Findex ) {
							if ( ! ( SurfNum == NumOfZoneSurfaces && Findex == NumOfZoneSurfaces ) ) {
								gio::write( OutputFileDebug, fmtA ) << "  " + Surface( ZoneInfo( ZoneNum ).SurfacePtr( SurfNum ) ).Name + ',' + Surface( ZoneInfo( ZoneNum ).SurfacePtr( Findex ) ).Name + ',' + RoundSigDigits( ZoneInfo( ZoneNum ).F( Findex, SurfNum ), 6 ) + ',';
							} else {
								gio::write( OutputFileDebug, fmtA ) << "  " + Surface( ZoneInfo( ZoneNum ).SurfacePtr( SurfNum ) ).Name + ',' + Surface( ZoneInfo( ZoneNum ).SurfacePtr( Findex ) ).Name + ',' + RoundSigDigits( ZoneInfo( ZoneNum ).F( Findex, SurfNum ), 6 ) + ';';
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
				for ( int SurfNum = 1; SurfNum <= NumOfZoneSurfaces; ++SurfNum ) {
					gio::write( OutputFileInits, "(',',A,$)" ) <<
						Surface( ZoneInfo( ZoneNum ).SurfacePtr( SurfNum ) ).Name;
				} gio::write( OutputFileInits );
				for ( Findex = 1; Findex <= NumOfZoneSurfaces; ++Findex ) {
					gio::write( OutputFileInits, "(A,',',A,$)" )
						<< "Script F Factor"
						<< Surface( ZoneInfo( ZoneNum ).SurfacePtr( Findex ) ).Name;
					int Npad = ((NumOfZoneSurfaces + 1) >> 1) << 1;
					for ( int SurfNum = 1; SurfNum <= NumOfZoneSurfaces; ++SurfNum ) {
						gio::write( OutputFileInits, "(',',A,$)" )
							<< RoundSigDigits( ZoneInfo( ZoneNum ).ScriptF[ (Npad*(Findex-1)) + (SurfNum-1) ], 4 );
					} gio::write( OutputFileInits );
				}
			}

			if ( ViewFactorReport ) { // Deallocate saved approximate/user view factors
				SaveApproximateViewFactors.deallocate();
			}

			RowSum = 0.0;
			for ( Findex = 1; Findex <= NumOfZoneSurfaces; ++Findex ) {
				RowSum += sum( ZoneInfo( ZoneNum ).F( _, Findex ) );
			}
			RowSum = std::abs( RowSum - NumOfZoneSurfaces );
			FixedRowSum = std::abs( FixedRowSum - NumOfZoneSurfaces );
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
		Array2A< Real64 > F, // USER INPUT DIRECT VIEW FACTOR MATRIX (N X N)
		Array1A_int const SPtr, // pointer to actual surface number
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
				F( inx2, inx1 ) = rNumericArgs( index + 2 );
			}
		}

	}

	void
	GetInputViewFactorsbyName(
		std::string const & ZoneName, // Needed to check for user input view factors.
		int const N, // NUMBER OF SURFACES
		Array2A< Real64 > F, // USER INPUT DIRECT VIEW FACTOR MATRIX (N X N)
		Array1A_int const SPtr, // pointer to actual surface number
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
		Array1D_string ZoneSurfaceNames;

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
				if ( inx1 > 0 && inx2 > 0 ) F( inx2, inx1 ) = rNumericArgs( numinx1 );
			}
			ZoneSurfaceNames.deallocate();
		}

	}

	void
	CalcApproximateViewFactors(
		int const N, // NUMBER OF SURFACES
		Array1A< Real64 > const A, // AREA VECTOR- ASSUMED,BE N ELEMENTS LONG
		Array1A< Real64 > const Azimuth, // Facing angle of the surface (in degrees)
		Array1A< Real64 > const Tilt, // Tilt angle of the surface (in degrees)
		Array2A< Real64 > F, // APPROXIMATE DIRECT VIEW FACTOR MATRIX (N X N)
		Array1A_int const SPtr // pointer to REAL(r64) surface number (for error message)
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
		Array1D< Real64 > ZoneArea; // Sum of the area of all zone surfaces seen

		// FLOW:
		// Calculate the sum of the areas seen by all zone surfaces
		ZoneArea.dimension( N, 0.0 );
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
					if ( ZoneArea( i ) > 0.0 ) F( j, i ) = A( j ) / ( ZoneArea( i ) );
				}

			}
		}

		ZoneArea.deallocate();

	}

	void
	FixViewFactors(
		int const N, // NUMBER OF SURFACES
		Array1A< Real64 > const A, // AREA VECTOR- ASSUMED,BE N ELEMENTS LONG
		Array2A< Real64 > F, // APPROXIMATE DIRECT VIEW FACTOR MATRIX (N X N)
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
		Array2D< Real64 > FixedAF( F ); // store for largest area check

		Accelerator = 1.0;
		ConvrgOld = 10.0;
		LargestArea = maxval( A );

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
		Array2D< Real64 > AF( N, N ); // = (AREA * DIRECT VIEW FACTOR) MATRIX
		for ( i = 1; i <= N; ++i ) {
			for ( j = 1; j <= N; ++j ) {
				AF( j, i ) = FixedAF( j, i ) * A( i );
			}
		}

		//  Enforce reciprocity by averaging AiFij and AjFji
		FixedAF = 0.5 * ( AF + transpose( AF ) ); //Performance Slow way to average with transpose (heap use)

		AF.deallocate();

		Array2D< Real64 > FixedF( N, N ); // CORRECTED MATRIX OF VIEW FACTORS (N X N)

		NumIterations = 0;
		RowSum = 0.0;
		//  Check for physically unreasonable enclosures.

		if ( N <= 3 ) {
			for ( i = 1; i <= N; ++i ) {
				for ( j = 1; j <= N; ++j ) {
					FixedF( j, i ) = FixedAF( j, i ) / A( i );
				}
			}

			ShowWarningError( "Surfaces in Zone=\"" + Zone( ZoneNum ).Name + "\" do not define an enclosure." );
			ShowContinueError( "Number of surfaces <= 3, view factors are set to force reciprocity." );

			F = FixedF;
			RowSum = sum( FixedF );
			FinalCheckValue = FixedCheckValue = std::abs( RowSum - N );
			Zone( ZoneNum ).EnforcedReciprocity = true;
			return; // Do not iterate, stop with reciprocity satisfied.

		} //  N <= 3 Case

		//  Regular fix cases
		Array1D< Real64 > RowCoefficient( N );
		Converged = false;
		while ( ! Converged ) {
			++NumIterations;
			for ( i = 1; i <= N; ++i ) {
				// Determine row coefficients which will enforce closure.
				Real64 const sum_FixedAF_i( sum( FixedAF( _, i ) ) );
				if ( std::abs( sum_FixedAF_i ) > 1.0e-10 ) {
					RowCoefficient( i ) = A( i ) / sum_FixedAF_i;
				} else {
					RowCoefficient( i ) = 1.0;
				}
				FixedAF( _, i ) *= RowCoefficient( i );
			}

			//  Enforce reciprocity by averaging AiFij and AjFji
			FixedAF = 0.5 * ( FixedAF + transpose( FixedAF ) );

			//  Form FixedF matrix
			for ( i = 1; i <= N; ++i ) {
				for ( j = 1; j <= N; ++j ) {
					FixedF( j, i ) = FixedAF( j, i ) / A( i );
					if ( std::abs( FixedF( j, i ) ) < 1.e-10 ) {
						FixedF( j, i ) = 0.0;
						FixedAF( j, i ) = 0.0;
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
				FixedAF = 0.5 * ( FixedAF + transpose( FixedAF ) );

				//  Form FixedF matrix
				for ( i = 1; i <= N; ++i ) {
					for ( j = 1; j <= N; ++j ) {
						FixedF( j, i ) = FixedAF( j, i ) / A( i );
					}
				}
				Real64 const sum_FixedF( sum( FixedF ) );
				FinalCheckValue = FixedCheckValue = CheckConvergeTolerance = std::abs( sum_FixedF - N );
				if ( CheckConvergeTolerance > 0.005 ) {
					ShowWarningError( "FixViewFactors: View factors not complete. Check for bad surface descriptions or unenclosed zone=\"" + Zone( ZoneNum ).Name + "\"." );
					ShowContinueError( "Enforced reciprocity has tolerance (ideal is 0)=[" + RoundSigDigits( CheckConvergeTolerance, 6 ) + "], Row Sum (ideal is " + RoundSigDigits( N ) + ")=[" + RoundSigDigits( RowSum, 2 ) + "]." );
					ShowContinueError( "If zone is unusual, or tolerance is on the order of 0.001, view factors are probably OK." );
				}
				if ( std::abs( FixedCheckValue ) < std::abs( OriginalCheckValue ) ) {
					F = FixedF;
					FinalCheckValue = FixedCheckValue;
				}
				RowSum = sum_FixedF;
				return;
			}
		}
		FixedCheckValue = ConvrgNew;
		if ( FixedCheckValue < OriginalCheckValue ) {
			F = FixedF;
			FinalCheckValue = FixedCheckValue;
		} else {
			FinalCheckValue = OriginalCheckValue;
			RowSum = sum( FixedF );
			if ( std::abs( RowSum - N ) < PrimaryConvergence ) {
				F = FixedF;
				FinalCheckValue = FixedCheckValue;
			} else {
				ShowWarningError( "FixViewFactors: View factors not complete. Check for bad surface descriptions or unenclosed zone=\"" + Zone( ZoneNum ).Name + "\"." );
			}
		}

	}

	void
	CalcScriptF(
		int const N, // Number of surfaces
		Array1< Real64 > const & A, // AREA VECTOR- ASSUMED,BE N ELEMENTS LONG
		Array2< Real64 > const & F, // DIRECT VIEW FACTOR MATRIX (N X N)
		Array1< Real64 > & EMISS, // VECTOR OF SURFACE EMISSIVITIES
		Real64 *ScriptF // Hottel's ScriptF, Amir Roth 2015-07-01: this is a hand-rolled "padded" 2D array.  Should be replaced by an Array2DPadded object.
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

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// FLOW:

#ifdef EP_Count_Calls
		++NumCalcScriptF_Calls;
#endif

		// Load Cmatrix with AF (AREA * DIRECT VIEW FACTOR) matrix
		Array2D< Real64 > Cmatrix( N, N ); // = (AF - EMISS/REFLECTANCE) matrix (but plays other roles)
		assert( equal_dimensions( Cmatrix, F ) ); // For linear indexing
		Array2D< Real64 >::size_type l( 0u );
		for ( int j = 1; j <= N; ++j ) {
			for ( int i = 1, li = A.index(i); i <= N; ++i, ++l, ++li ) {
				Cmatrix[ l ] = A[ li ] * F[ l ]; // [ l ] == ( i, j )
			}
		}

		// Load Cmatrix with (AF - EMISS/REFLECTANCE) matrix
		Array1D< Real64 > Excite( N ); // Excitation vector = A*EMISS/REFLECTANCE
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

		Array2D< Real64 > Cinverse( N, N ); // Inverse of Cmatrix
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

		// Form ScriptF matrix 
		// Amir Roth 2015-07-01: Npad is the size of a "padded" row
		unsigned int Npad = ((N+1) >> 1) << 1;

		for ( int i = 1; i <= N; ++i ) { 
			for ( int j = 1, lj = EMISS.index(j), ill = Cinverse.index(i, j), sll = (i-1)*Npad+(j-1); j <= N; ++j, ++lj, ++ill, ++sll ) {
				// These need to be inside the inner loop so that the whole thing
				// vectorizes.  If we try to pull these out then the inner loses unit stride.
				Real64 const EMISS_j = EMISS[ lj ];
				Real64 const EMISS_facj = EMISS_j / ( 1.0 - EMISS_j ); 

				//        ScriptF(I,J) = EMISS(I)/(1.0d0-EMISS(I))*(Jmatrix(I,J)-Delta*EMISS(I)), where Delta=0
				ScriptF[ sll ] = EMISS_facj * Cinverse[ ill ]; // [ l ] == ( i, j )
			}
			// pull out the (i == j) case so that inner loop above will vectorize
			int iii = Cinverse.index(i, i);
			int sii = (i-1)*Npad+(i-1);
			int li = EMISS.index(i);
			Real64 const EMISS_i = EMISS[ li ];
			Real64 const EMISS_faci = EMISS_i / ( 1.0 - EMISS_i);
			//        ScriptF(I,J) = EMISS(I)/(1.0d0-EMISS(I))*(Jmatrix(I,J)-Delta*EMISS(I)), where Delta=1
			ScriptF[ sii ] = EMISS_faci * ( Cinverse[ iii ] - EMISS_i );
		}
	}

	void
	CalcMatrixInverse(
		Array2< Real64 > & A, // Matrix: Gets reduced to L\U form
		Array2< Real64 > & I // Returned as inverse matrix
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
			auto ik( A.index( i, i + 1 ) );
			for ( int k = i + 1; k <= u; ++k, ++ik ) {
				Real64 const aAki( std::abs( A[ ik ] ) ); // [ ik ] == ( i, k )
				if ( aAki > aPiv ) {
					iPiv = k;
					aPiv = aAki;
				}
			}
			assert( aPiv != 0.0 ); //? Is zero pivot possible for some user inputs? If so if test/handler needed

			// Swap row i with pivot row
			if ( iPiv != i ) {
				auto ji( A.index( l, i ) ); // [ ji ] == ( j, i )
				auto pj( A.index( l, iPiv ) ); // [ pj ] == ( j, iPiv )
				for ( int j = l; j <= u; ++j, ji += n, pj += n ) {
					Real64 const Aij( A[ ji ] );
					A[ ji ] = A[ pj ];
					A[ pj ] = Aij;
					Real64 const Iij( I[ ji ] );
					I[ ji ] = I[ pj ];
					I[ pj ] = Iij;
				}
			}

			// Put multipliers in column i and reduce block below A(i,i)
			Real64 const Aii_inv( 1.0 / A( i, i ) );
			for ( int k = i + 1; k <= u; ++k ) {
				Real64 const multiplier( A( i, k ) * Aii_inv );
				A( i, k ) = multiplier;
				if ( multiplier != 0.0 ) {
					auto ji( A.index( i + 1, i ) ); // [ ji ] == ( j, i )
					auto jk( A.index( i + 1, k ) ); // [ jk ] == ( j, k )
					for ( int j = i + 1; j <= u; ++j, ji += n, jk += n ) {
						A[ jk ] -= multiplier * A[ ji ];
					}
					ji = A.index( l, i );
					jk = A.index( l, k );
					for ( int j = l; j <= u; ++j, ji += n, jk += n ) {
						Real64 const Iij( I[ ji ] );
						if ( Iij != 0.0 ) {
							I[ jk ] -= multiplier * Iij;
						}
					}
				}
			}

		}

		// Perform back-substitution on [U|I] to put inverse in I
		for ( int k = u; k >= l; --k ) {
			Real64 const Akk_inv( 1.0 / A( k, k ) );
			auto jk( A.index( l, k ) ); // [ jk ] == ( j, k )
			for ( int j = l; j <= u; ++j, jk += n ) {
				I[ jk ] *= Akk_inv;
			}
			auto ik( A.index( k, l ) ); // [ ik ] == ( i, k )
			for ( int i = l; i < k; ++i, ++ik ) { // Eliminate kth column entries from I in rows above k
				Real64 const Aik( A[ ik ] );
				auto ji( A.index( l, i ) ); // [ ji ] == ( j, i )
				auto jk( A.index( l, k ) ); // [ jk ] == ( k, j )
				for ( int j = l; j <= u; ++j, ji += n, jk += n ) {
					I[ ji ] -= Aik * I[ jk ];
				}
			}
		}

	}

	//     NOTICE

	//     Copyright � 1996-2014 The Board of Trustees of the University of Illinois
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
