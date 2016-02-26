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
#include <cassert>
#include <cmath>
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/member.functions.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <SurfaceGeometry.hh>
#include <DataEnvironment.hh>
#include <DataErrorTracking.hh>
#include <DataGlobals.hh>
#include <DataHeatBalance.hh>
#include <DataHeatBalSurface.hh>
#include <DataIPShortCuts.hh>
#include <DataPrecisionGlobals.hh>
#include <DataReportingFlags.hh>
#include <DataWindowEquivalentLayer.hh>
#include <DisplayRoutines.hh>
#include <EMSManager.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <OutputProcessor.hh>
#include <OutputReportPredefined.hh>
#include <ScheduleManager.hh>
#include <UtilityRoutines.hh>
#include <Vectors.hh>

namespace EnergyPlus {

namespace SurfaceGeometry {

	// Module containing the routines dealing with the Surface Geometry

	// MODULE INFORMATION:
	//       AUTHOR         Linda Lawrie
	//       DATE WRITTEN   June 2000
	//       MODIFIED       DJS (PSU Dec 2006) to add ecoroof
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This module performs the functions required of the surface geometry.

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES: none

	// OTHER NOTES: none

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataGlobals;
	using namespace DataEnvironment;
	using namespace DataHeatBalance;
	using namespace DataSurfaces;
	using DataWindowEquivalentLayer::CFSMAXNL;

	// Use statements for access to subroutines in other modules
	// na

	// Data
	//MODULE PARAMETER DEFINITIONS
	static std::string const BlankString;
	Array1D_string const BaseSurfCls( 3, { "WALL", "FLOOR", "ROOF" } );
	Array1D_string const SubSurfCls( 6, { "WINDOW", "DOOR", "GLASSDOOR", "SHADING", "TUBULARDAYLIGHTDOME", "TUBULARDAYLIGHTDIFFUSER" } );
	Array1D_int const BaseSurfIDs( 3, { SurfaceClass_Wall, SurfaceClass_Floor, SurfaceClass_Roof } );

	Array1D_int const SubSurfIDs( 6, { SurfaceClass_Window, SurfaceClass_Door, SurfaceClass_GlassDoor, SurfaceClass_Shading, SurfaceClass_TDD_Dome, SurfaceClass_TDD_Diffuser } );

	int const UnenteredAdjacentZoneSurface( -998 ); // allows users to enter one zone surface ("Zone")
	// referencing another in adjacent zone
	int const UnreconciledZoneSurface( -999 ); // interim value between entering surfaces ("Surface") and reconciling
	// surface names in other zones

	static gio::Fmt fmtLD( "*" );
	static gio::Fmt fmtA( "(A)" );
	static gio::Fmt fmt3( "(A,3(1x,f18.13))" );

	// DERIVED TYPE DEFINITIONS

	//MODULE VARIABLE DECLARATIONS:

	namespace {
		// These were static variables within different functions. They were pulled out into the namespace
		// to facilitate easier unit testing of those functions.
		// These are purposefully not in the header file as an extern variable. No one outside of this should
		// use these. They are cleared by clear_state() for use by unit tests, but normal simulations should be unaffected.
		// This is purposefully in an anonymous namespace so nothing outside this implementation file can use it.
		bool ProcessSurfaceVerticesOneTimeFlag( true );
		int checkSubSurfAzTiltNormErrCount( 0 );
		Array1D< Real64 > Xpsv;
		Array1D< Real64 > Ypsv;
		Array1D< Real64 > Zpsv;
	}


	// Following are used only during getting vertices, so are module variables here.
	Real64 CosBldgRelNorth( 0.0 ); // Cosine of the building rotation (relative north) (includes appendix G rotation)
	Real64 SinBldgRelNorth( 0.0 ); // Sine of the building rotation (relative north)   (includes appendix G rotation)
	Real64 CosBldgRotAppGonly( 0.0 ); // Cosine of the building rotation for appendix G only(relative north)
	Real64 SinBldgRotAppGonly( 0.0 ); // Sine of the building rotation for appendix G only (relative north)
	Array1D< Real64 > CosZoneRelNorth; // Cosine of the zone rotation (relative north)
	Array1D< Real64 > SinZoneRelNorth; // Sine of the zone rotation (relative north)

	bool NoGroundTempObjWarning( true ); // This will cause a warning to be issued if surfaces with "Ground"
	// outside environment are used but no ground temperature object was input.
	bool NoFCGroundTempObjWarning( true ); // This will cause a warning to be issued if surfaces with "GroundFCfactorMethod"
	// outside environment are used but no FC ground temperatures was input.
	bool RectSurfRefWorldCoordSystem( false ); // GlobalGeometryRules=World (true) or Relative (false)
	int Warning1Count( 0 ); // counts of Modify Window 5/6 windows
	int Warning2Count( 0 ); // counts of overriding exterior windows with Window 5/6 glazing systems
	int Warning3Count( 0 ); // counts of overriding interior windows with Window 5/6 glazing systems

	//SUBROUTINE SPECIFICATIONS FOR MODULE SurfaceGeometry

	// Object Data
	Array1D< SurfaceData > SurfaceTmp; // Allocated/Deallocated during input processing

	// Functions

	// Clears the global data in HeatBalanceManager.
	// Needed for unit tests, should not be normally called.
	void
	clear_state()
	{
		ProcessSurfaceVerticesOneTimeFlag = true;
		checkSubSurfAzTiltNormErrCount = 0;
		Xpsv.deallocate();
		Ypsv.deallocate();
		Zpsv.deallocate();
		// Following are used only during getting vertices, so are module variables here.
		CosBldgRelNorth = 0.0;
		SinBldgRelNorth = 0.0;
		CosBldgRotAppGonly = 0.0;
		SinBldgRotAppGonly = 0.0;
		CosZoneRelNorth.deallocate();
		SinZoneRelNorth.deallocate();
		NoGroundTempObjWarning = true;
		NoFCGroundTempObjWarning = true;
		RectSurfRefWorldCoordSystem = false;
		Warning1Count = 0;
		Warning2Count = 0;
		Warning3Count = 0;
		SurfaceTmp.deallocate();
	}

	void
	SetupZoneGeometry( bool & ErrorsFound )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         George Walton
		//       DATE WRITTEN   September 1977
		//       MODIFIED       April 2002 (FCW): add warning for Solar Distribution
		//                      = FullInteriorExterior when window has reveal
		//                      Add fatal error when triangular window has reveal
		//                      May 2002(FCW): Allow triangular windows to have reveal (subr SHDRVL
		//                      in SolarShading). Remove above warning and fatal error.
		//       RE-ENGINEERED  November 1997 (RKS,LKL)

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine controls the processing of detached shadowing and
		// zone surfaces for computing their vertices.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataVectorTypes;
		using namespace OutputReportPredefined;
		using General::RoundSigDigits;
		using namespace DataReportingFlags;
		using InputProcessor::GetNumSectionsFound;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static gio::Fmt ValFmt( "(F20.2)" );
		static gio::Fmt fmtA( "(A)" );
		static std::string const RoutineName( "SetUpZoneGeometry: " );
		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 AverageHeight; // Used to keep track of average height of a surface/zone
		int SurfNum; // Surface number (DO loop counter)
		int ZoneNum; // Zone number for current surface and DO loop counter
		Real64 ZMax; // Maximum Z of a surface (detailed outside coefficient calculation)
		Real64 ZMin; // Minimum Z of a surface (detailed outside coefficient calculation)
		Real64 ZCeilAvg;
		Real64 CeilCount;
		Real64 ZFlrAvg;
		Real64 FloorCount;
		Real64 TotSurfArea;
		Real64 Z1;
		Real64 Z2;
		std::string String1;
		std::string String2;
		std::string String3;
		int Count; // To count wall surfaces for ceiling height calculation
		Array1D_bool ZoneCeilingHeightEntered;
		Array1D< Real64 > ZoneCeilingArea;
		static int ErrCount( 0 );
		Real64 NominalUwithConvCoeffs;
		std::string cNominalU;
		std::string cNominalUwithConvCoeffs;
		bool isWithConvCoefValid;
		//  INTEGER, ALLOCATABLE, DIMENSION(:) :: ZoneSurfacesCount
		//  INTEGER, ALLOCATABLE, DIMENSION(:) :: ZoneSubSurfacesCount
		//  INTEGER, ALLOCATABLE, DIMENSION(:) :: ZoneShadingSurfacesCount

		bool nonInternalMassSurfacesPresent;
		bool DetailedWWR;


		// Formats
		static gio::Fmt Format_720( "(' Zone Information, ',A,28(',',A))" );
		static gio::Fmt Format_721( "('! <Zone Information>,Zone Name,North Axis {deg},','Origin X-Coordinate {m},Origin Y-Coordinate {m},Origin Z-Coordinate {m},','Centroid X-Coordinate {m},Centroid Y-Coordinate {m},Centroid Z-Coordinate {m},','Type,Zone Multiplier,Zone List Multiplier,Minimum X {m},Maximum X {m},','Minimum Y {m},Maximum Y {m},Minimum Z {m},Maximum Z {m},Ceiling Height {m},Volume {m3},','Zone Inside Convection Algorithm {Simple-Detailed-CeilingDiffuser-TrombeWall},','Zone Outside Convection Algorithm {Simple-Detailed-Tarp-MoWitt-DOE-2-BLAST},',' Floor Area {m2},Exterior Gross Wall Area {m2},Exterior Net Wall Area {m2},Exterior Window Area {m2},',' Number of Surfaces, Number of SubSurfaces, Number of Shading SubSurfaces, ',' Part of Total Building Area')" );

		// FLOW:
		// Allocations and initializations...

		// Zones must have been "gotten" before this call
		// The RelNorth variables are used if "relative" coordinates are input as well
		// as setting up DaylightingCoords

		// these include building north axis and Building Rotation for Appendix G
		CosBldgRelNorth = std::cos( -( BuildingAzimuth + BuildingRotationAppendixG ) * DegToRadians );
		SinBldgRelNorth = std::sin( -( BuildingAzimuth + BuildingRotationAppendixG ) * DegToRadians );

		// these are only for Building Rotation for Appendix G when using world coordinate system
		CosBldgRotAppGonly = std::cos( -BuildingRotationAppendixG * DegToRadians );
		SinBldgRotAppGonly = std::sin( -BuildingRotationAppendixG * DegToRadians );

		CosZoneRelNorth.allocate( NumOfZones );
		SinZoneRelNorth.allocate( NumOfZones );

		ZoneCeilingHeightEntered.dimension( NumOfZones, false );
		ZoneCeilingArea.dimension( NumOfZones, 0.0 );

		for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {

			CosZoneRelNorth( ZoneNum ) = std::cos( -Zone( ZoneNum ).RelNorth * DegToRadians );
			SinZoneRelNorth( ZoneNum ) = std::sin( -Zone( ZoneNum ).RelNorth * DegToRadians );

		}
		GetSurfaceData( ErrorsFound );

		if ( ErrorsFound ) {
			CosZoneRelNorth.deallocate();
			SinZoneRelNorth.deallocate();
			return;
		}

		GetWindowGapAirflowControlData( ErrorsFound );

		GetStormWindowData( ErrorsFound );

		if ( ! ErrorsFound && TotStormWin > 0 ) CreateStormWindowConstructions();

		CosZoneRelNorth.deallocate();
		SinZoneRelNorth.deallocate();

		AllocateModuleArrays(); // This needs to be moved to the main manager routine of SSG at a later date

		AirSkyRadSplit.dimension( TotSurfaces, 0.0 );

		CalcWindowRevealReflection = false; // Set to True in ProcessSurfaceVertices if beam solar reflection from window reveals
		// is requested for one or more exterior windows.
		BuildingShadingCount = 0;
		FixedShadingCount = 0;
		AttachedShadingCount = 0;

		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) { // Loop through all surfaces...

			AirSkyRadSplit( SurfNum ) = std::sqrt( 0.5 * ( 1.0 + Surface( SurfNum ).CosTilt ) );

			// Set flag that determines whether a surface is a shadowing surface
			Surface( SurfNum ).ShadowingSurf = false;
			if ( Surface( SurfNum ).Class == SurfaceClass_Shading || Surface( SurfNum ).Class == SurfaceClass_Detached_F || Surface( SurfNum ).Class == SurfaceClass_Detached_B ) Surface( SurfNum ).ShadowingSurf = true;
			if ( Surface( SurfNum ).Class == SurfaceClass_Shading ) ++AttachedShadingCount;
			if ( Surface( SurfNum ).Class == SurfaceClass_Detached_F ) ++FixedShadingCount;
			if ( Surface( SurfNum ).Class == SurfaceClass_Detached_B ) ++BuildingShadingCount;

			if ( Surface( SurfNum ).Class != SurfaceClass_IntMass ) ProcessSurfaceVertices( SurfNum, ErrorsFound );
		}

		for ( auto & e : Zone ) {
			e.ExtWindowArea = 0.0;
			e.HasInterZoneWindow = false;
			e.HasWindow = false;
			e.ExtGrossWallArea = 0.0;
			e.ExtNetWallArea = 0.0;
			e.TotalSurfArea = 0.0;
		}

		DetailedWWR = ( GetNumSectionsFound( "DETAILEDWWR_DEBUG" ) > 0 );
		if ( DetailedWWR ) {
			gio::write( OutputFileDebug, fmtA ) << "=======User Entered Classification =================";
			gio::write( OutputFileDebug, fmtA ) << "Surface,Class,Area,Tilt";
		}

		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) { // Loop through all surfaces to find windows...

			if ( ! Surface( SurfNum ).HeatTransSurf ) continue; // Skip shadowing (sub)surfaces
			ZoneNum = Surface( SurfNum ).Zone;
			Zone( ZoneNum ).TotalSurfArea += Surface( SurfNum ).Area;
			if ( Construct( Surface( SurfNum ).Construction ).TypeIsWindow ) {
				Zone( ZoneNum ).TotalSurfArea += SurfaceWindow( SurfNum ).FrameArea;
				Zone( ZoneNum ).HasWindow = true;
			}
			if ( Surface( SurfNum ).Class == SurfaceClass_Roof ) ZoneCeilingArea( ZoneNum ) += Surface( SurfNum ).Area;
			if ( ! Construct( Surface( SurfNum ).Construction ).TypeIsWindow ) {
				if ( Surface( SurfNum ).ExtBoundCond == ExternalEnvironment || Surface( SurfNum ).ExtBoundCond == OtherSideCondModeledExt ) {
					Zone( ZoneNum ).ExteriorTotalSurfArea += Surface( SurfNum ).GrossArea;
					if ( Surface( SurfNum ).Class == SurfaceClass_Wall ) {
						Zone( ZoneNum ).ExtNetWallArea += Surface( SurfNum ).Area;
						Zone( ZoneNum ).ExtGrossWallArea += Surface( SurfNum ).GrossArea;
						Zone( ZoneNum ).ExtGrossWallArea_Multiplied += Surface( SurfNum ).GrossArea * Zone( ZoneNum ).Multiplier * Zone( ZoneNum ).ListMultiplier;
						if ( DetailedWWR ) {
							gio::write( OutputFileDebug, fmtA ) << Surface( SurfNum ).Name + ",Wall," + RoundSigDigits( Surface( SurfNum ).GrossArea * Zone( ZoneNum ).Multiplier * Zone( ZoneNum ).ListMultiplier, 2 ) + ',' + RoundSigDigits( Surface( SurfNum ).Tilt, 1 );
						}
					}
				} else if ( Surface( SurfNum ).ExtBoundCond == Ground || Surface( SurfNum ).ExtBoundCond == GroundFCfactorMethod ) {
					Zone( ZoneNum ).ExteriorTotalGroundSurfArea += Surface( SurfNum ).GrossArea;
					if ( Surface( SurfNum ).Class == SurfaceClass_Wall ) {
						Zone( ZoneNum ).ExtGrossGroundWallArea += Surface( SurfNum ).GrossArea;
						Zone( ZoneNum ).ExtGrossGroundWallArea_Multiplied += Surface( SurfNum ).GrossArea * Zone( ZoneNum ).Multiplier * Zone( ZoneNum ).ListMultiplier;
						if ( DetailedWWR ) {
							gio::write( OutputFileDebug, fmtA ) << Surface( SurfNum ).Name + ",Wall-GroundContact," + RoundSigDigits( Surface( SurfNum ).GrossArea * Zone( ZoneNum ).Multiplier * Zone( ZoneNum ).ListMultiplier, 2 ) + ',' + RoundSigDigits( Surface( SurfNum ).Tilt, 1 );
						}
					}
				}

			} else { // For Windows

				if ( ( Surface( SurfNum ).ExtBoundCond > 0 ) && ( Surface( SurfNum ).BaseSurf != SurfNum ) ) { // Interzone window present
					if ( ! IgnoreInteriorWindowTransmission ) {
						Zone( Surface( SurfNum ).Zone ).HasInterZoneWindow = true;
					}
				} else {
					if ( ( ( Surface( SurfNum ).ExtBoundCond == ExternalEnvironment ) || ( Surface( SurfNum ).ExtBoundCond == OtherSideCondModeledExt ) ) && ( Surface( SurfNum ).Class != SurfaceClass_TDD_Dome ) ) {
						Zone( Surface( SurfNum ).Zone ).ExtWindowArea += Surface( SurfNum ).GrossArea;
						Zone( Surface( SurfNum ).Zone ).ExtWindowArea_Multiplied = Zone( Surface( SurfNum ).Zone ).ExtWindowArea + Surface( SurfNum ).GrossArea * Surface( SurfNum ).Multiplier * Zone( ZoneNum ).Multiplier * Zone( ZoneNum ).ListMultiplier;
						if ( DetailedWWR ) {
							gio::write( OutputFileDebug, fmtA ) << Surface( SurfNum ).Name + ",Window," + RoundSigDigits( Surface( SurfNum ).GrossArea * Surface( SurfNum ).Multiplier * Zone( ZoneNum ).Multiplier * Zone( ZoneNum ).ListMultiplier, 2 ) + ',' + RoundSigDigits( Surface( SurfNum ).Tilt, 1 );
						}
					}
				}

			}

		} // ...end of surfaces windows DO loop

		//  DO SurfNum = 1, TotSurfaces ! Set areas for Sunlit area calculations for Windows
		//    IF (Surface(SurfNum)%Class /= SurfaceClass_Window) CYCLE
		//    SurfaceWindow(SurfNum)%AreaCalcForSunlitArea = (Surface(SurfNum)%Area + SurfaceWindow(SurfNum)%DividerArea) /  &
		//                                  Surface(SurfNum)%Multiplier
		//  ENDDO
		if ( DetailedWWR ) {
			gio::write( OutputFileDebug, fmtA ) << "========================";
			gio::write( OutputFileDebug, fmtA ) << "Zone,ExtWallArea,ExtWindowArea";
		}

		for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
			CeilCount = 0.0;
			FloorCount = 0.0;
			Count = 0;
			AverageHeight = 0.0;
			ZCeilAvg = 0.0;
			ZFlrAvg = 0.0;
			ZMax = -99999.0;
			ZMin = 99999.0;
			if ( DetailedWWR ) {
				gio::write( OutputFileDebug, fmtA ) << Zone( ZoneNum ).Name + ',' + RoundSigDigits( Zone( ZoneNum ).ExtGrossWallArea, 2 ) + ',' + RoundSigDigits( Zone( ZoneNum ).ExtWindowArea, 2 );
			}
			for ( SurfNum = Zone( ZoneNum ).SurfaceFirst; SurfNum <= Zone( ZoneNum ).SurfaceLast; ++SurfNum ) {
				if ( Surface( SurfNum ).Class == SurfaceClass_Roof ) {
					// Use Average Z for surface, more important for roofs than floors...
					++CeilCount;
					Z1 = minval( Surface( SurfNum ).Vertex( {1,Surface( SurfNum ).Sides} ), &Vector::z );
					Z2 = maxval( Surface( SurfNum ).Vertex( {1,Surface( SurfNum ).Sides} ), &Vector::z );
					//        ZCeilAvg=ZCeilAvg+(Z1+Z2)/2.d0
					ZCeilAvg += ( ( Z1 + Z2 ) / 2.0 ) * ( Surface( SurfNum ).Area / ZoneCeilingArea( ZoneNum ) );
				}
				if ( Surface( SurfNum ).Class == SurfaceClass_Floor ) {
					// Use Average Z for surface, more important for roofs than floors...
					++FloorCount;
					Z1 = minval( Surface( SurfNum ).Vertex( {1,Surface( SurfNum ).Sides} ), &Vector::z );
					Z2 = maxval( Surface( SurfNum ).Vertex( {1,Surface( SurfNum ).Sides} ), &Vector::z );
					//        ZFlrAvg=ZFlrAvg+(Z1+Z2)/2.d0
					ZFlrAvg += ( ( Z1 + Z2 ) / 2.0 ) * ( Surface( SurfNum ).Area / Zone( ZoneNum ).FloorArea );
				}
				if ( Surface( SurfNum ).Class == SurfaceClass_Wall ) {
					// Use Wall calculation in case no roof & floor in zone
					++Count;
					if ( Count == 1 ) {
						ZMax = Surface( SurfNum ).Vertex( 1 ).z;
						ZMin = ZMax;
					}
					ZMax = max( ZMax, maxval( Surface( SurfNum ).Vertex( {1,Surface( SurfNum ).Sides} ), &Vector::z ) );
					ZMin = min( ZMin, minval( Surface( SurfNum ).Vertex( {1,Surface( SurfNum ).Sides} ), &Vector::z ) );
				}
			}
			if ( CeilCount > 0.0 && FloorCount > 0.0 ) {
				//      ZCeilAvg=ZCeilAvg/CeilCount
				//      ZFlrAvg=ZFlrAvg/FloorCount
				AverageHeight = ZCeilAvg - ZFlrAvg;
			} else {
				AverageHeight = ( ZMax - ZMin );
			}
			if ( AverageHeight <= 0.0 ) {
				AverageHeight = ( ZMax - ZMin );
			}

			if ( Zone( ZoneNum ).CeilingHeight > 0.0 ) {
				ZoneCeilingHeightEntered( ZoneNum ) = true;
				if ( AverageHeight > 0.0 ) {
					if ( std::abs( AverageHeight - Zone( ZoneNum ).CeilingHeight ) / Zone( ZoneNum ).CeilingHeight > 0.05 ) {
						if ( ErrCount == 1 && ! DisplayExtraWarnings ) {
							ShowWarningError( RoutineName + "Entered Ceiling Height for some zone(s) significantly different from calculated Ceiling Height" );
							ShowContinueError( "...use Output:Diagnostics,DisplayExtraWarnings; to show more details on each max iteration exceeded." );
						}
						if ( DisplayExtraWarnings ) {
							ShowWarningError( RoutineName + "Entered Ceiling Height for Zone=\"" + Zone( ZoneNum ).Name + "\" significantly different from calculated Ceiling Height" );
							gio::write( String1, ValFmt ) << Zone( ZoneNum ).CeilingHeight;
							strip( String1 );
							gio::write( String2, ValFmt ) << AverageHeight;
							strip( String2 );
							ShowContinueError( RoutineName + "Entered Ceiling Height=" + String1 + ", Calculated Ceiling Height=" + String2 + ", entered height will be used in calculations." );
						}
					}
				}
			}
			if ( ( Zone( ZoneNum ).CeilingHeight <= 0.0 ) && ( AverageHeight > 0.0 ) ) Zone( ZoneNum ).CeilingHeight = AverageHeight;

		}

		CalculateZoneVolume( ErrorsFound, ZoneCeilingHeightEntered ); // Calculate Zone Volumes

		// Calculate zone centroid (and min/max x,y,z for zone)
		for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
			nonInternalMassSurfacesPresent = false;
			TotSurfArea = 0.0;
			Zone( ZoneNum ).Centroid = Vector( 0.0, 0.0, 0.0 );
			if ( Surface( Zone( ZoneNum ).SurfaceFirst ).Sides > 0 ) {
				Zone( ZoneNum ).MinimumX = Surface( Zone( ZoneNum ).SurfaceFirst ).Vertex( 1 ).x;
				Zone( ZoneNum ).MaximumX = Surface( Zone( ZoneNum ).SurfaceFirst ).Vertex( 1 ).x;
				Zone( ZoneNum ).MinimumY = Surface( Zone( ZoneNum ).SurfaceFirst ).Vertex( 1 ).y;
				Zone( ZoneNum ).MaximumY = Surface( Zone( ZoneNum ).SurfaceFirst ).Vertex( 1 ).y;
				Zone( ZoneNum ).MinimumZ = Surface( Zone( ZoneNum ).SurfaceFirst ).Vertex( 1 ).z;
				Zone( ZoneNum ).MaximumZ = Surface( Zone( ZoneNum ).SurfaceFirst ).Vertex( 1 ).z;
			}
			for ( SurfNum = Zone( ZoneNum ).SurfaceFirst; SurfNum <= Zone( ZoneNum ).SurfaceLast; ++SurfNum ) {
				if ( Surface( SurfNum ).Class == SurfaceClass_IntMass ) continue;
				nonInternalMassSurfacesPresent = true;
				if ( Surface( SurfNum ).Class == SurfaceClass_Wall || ( Surface( SurfNum ).Class == SurfaceClass_Roof ) || ( Surface( SurfNum ).Class == SurfaceClass_Floor ) ) {

					Zone( ZoneNum ).Centroid.x += Surface( SurfNum ).Centroid.x * Surface( SurfNum ).GrossArea;
					Zone( ZoneNum ).Centroid.y += Surface( SurfNum ).Centroid.y * Surface( SurfNum ).GrossArea;
					Zone( ZoneNum ).Centroid.z += Surface( SurfNum ).Centroid.z * Surface( SurfNum ).GrossArea;
					TotSurfArea += Surface( SurfNum ).GrossArea;
				}
				Zone( ZoneNum ).MinimumX = min( Zone( ZoneNum ).MinimumX, minval( Surface( SurfNum ).Vertex( {1,Surface( SurfNum ).Sides} ), &Vector::x ) );
				Zone( ZoneNum ).MaximumX = max( Zone( ZoneNum ).MaximumX, maxval( Surface( SurfNum ).Vertex( {1,Surface( SurfNum ).Sides} ), &Vector::x ) );
				Zone( ZoneNum ).MinimumY = min( Zone( ZoneNum ).MinimumY, minval( Surface( SurfNum ).Vertex( {1,Surface( SurfNum ).Sides} ), &Vector::y ) );
				Zone( ZoneNum ).MaximumY = max( Zone( ZoneNum ).MaximumY, maxval( Surface( SurfNum ).Vertex( {1,Surface( SurfNum ).Sides} ), &Vector::y ) );
				Zone( ZoneNum ).MinimumZ = min( Zone( ZoneNum ).MinimumZ, minval( Surface( SurfNum ).Vertex( {1,Surface( SurfNum ).Sides} ), &Vector::z ) );
				Zone( ZoneNum ).MaximumZ = max( Zone( ZoneNum ).MaximumZ, maxval( Surface( SurfNum ).Vertex( {1,Surface( SurfNum ).Sides} ), &Vector::z ) );
			}
			if ( TotSurfArea > 0.0 ) {
				Zone( ZoneNum ).Centroid.x /= TotSurfArea;
				Zone( ZoneNum ).Centroid.y /= TotSurfArea;
				Zone( ZoneNum ).Centroid.z /= TotSurfArea;
			}
			if ( ! nonInternalMassSurfacesPresent ) {
				ShowSevereError( RoutineName + "Zone=\"" + Zone( ZoneNum ).Name + "\" has only internal mass surfaces.  Need at least one other surface." );
				ErrorsFound = true;
			}
		}

		ZoneCeilingHeightEntered.deallocate();
		ZoneCeilingArea.deallocate();

		AdjacentZoneToSurface.dimension( TotSurfaces, 0 );
		// note -- adiabatic surfaces will show same zone as surface
		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
			if ( Surface( SurfNum ).ExtBoundCond <= 0 ) continue;
			AdjacentZoneToSurface( SurfNum ) = Surface( Surface( SurfNum ).ExtBoundCond ).Zone;
		}

		for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
			for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
				if ( ! Surface( SurfNum ).HeatTransSurf && Surface( SurfNum ).ZoneName == Zone( ZoneNum ).Name ) ++Zone( ZoneNum ).NumShadingSurfaces;

				if ( Surface( SurfNum ).Zone != ZoneNum ) continue;

				if ( Surface( SurfNum ).HeatTransSurf && ( Surface( SurfNum ).Class == SurfaceClass_Wall || Surface( SurfNum ).Class == SurfaceClass_Roof || Surface( SurfNum ).Class == SurfaceClass_Floor ) ) ++Zone( ZoneNum ).NumSurfaces;

				if ( Surface( SurfNum ).HeatTransSurf && ( Surface( SurfNum ).Class == SurfaceClass_Window || Surface( SurfNum ).Class == SurfaceClass_GlassDoor || Surface( SurfNum ).Class == SurfaceClass_Door || Surface( SurfNum ).Class == SurfaceClass_TDD_Dome || Surface( SurfNum ).Class == SurfaceClass_TDD_Diffuser ) ) ++Zone( ZoneNum ).NumSubSurfaces;

			} // surfaces
		} // zones

		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
			if ( Surface( SurfNum ).Construction > 0 && Surface( SurfNum ).Construction <= TotConstructs ) {
				NominalUwithConvCoeffs = ComputeNominalUwithConvCoeffs( SurfNum, isWithConvCoefValid );
				if ( isWithConvCoefValid ) {
					cNominalUwithConvCoeffs = RoundSigDigits( NominalUwithConvCoeffs, 3 );
				} else {
					cNominalUwithConvCoeffs = "[invalid]";
				}
				if ( ( Surface( SurfNum ).Class == SurfaceClass_Window ) || ( Surface( SurfNum ).Class == SurfaceClass_TDD_Dome ) ) {
					// SurfaceClass_Window also covers glass doors and TDD:Diffusers
					cNominalU = "N/A";
				} else {
					cNominalU = RoundSigDigits( NominalU( Surface( SurfNum ).Construction ), 3 );
				}
			} else {
				cNominalUwithConvCoeffs = "**";
				cNominalU = "**";
			}

			// save the U-value nominal for use later in tabular report
			Surface( SurfNum ).UNomWOFilm = cNominalU;
			Surface( SurfNum ).UNomFilm = cNominalUwithConvCoeffs;
			//populate the predefined report related to u-values with films
			//only exterior surfaces including underground
			if ( ( Surface( SurfNum ).ExtBoundCond == ExternalEnvironment ) || ( Surface( SurfNum ).ExtBoundCond == Ground ) ) {
				{ auto const SELECT_CASE_var( Surface( SurfNum ).Class );
				if ( ( SELECT_CASE_var == SurfaceClass_Wall ) || ( SELECT_CASE_var == SurfaceClass_Floor ) || ( SELECT_CASE_var == SurfaceClass_Roof ) ) {
					PreDefTableEntry( pdchOpUfactFilm, Surface( SurfNum ).Name, NominalUwithConvCoeffs, 3 );
				} else if ( SELECT_CASE_var == SurfaceClass_Door ) {
					PreDefTableEntry( pdchDrUfactFilm, Surface( SurfNum ).Name, NominalUwithConvCoeffs, 3 );
				}}
			}
		} // surfaces

		// Write number of shadings to initialization output file
		gio::write( OutputFileInits, fmtA ) << "! <Shading Summary>, Number of Fixed Detached Shades, Number of Building Detached Shades, Number of Attached Shades";

		gio::write( OutputFileInits, fmtA ) << " Shading Summary," + RoundSigDigits( FixedShadingCount ) + ',' + RoundSigDigits( BuildingShadingCount ) + ',' + RoundSigDigits( AttachedShadingCount );

		// Write number of zones header to initialization output file
		gio::write( OutputFileInits, fmtA ) << "! <Zone Summary>, Number of Zones, Number of Zone Surfaces, Number of SubSurfaces";

		gio::write( OutputFileInits, fmtA ) << " Zone Summary," + RoundSigDigits( NumOfZones ) + ',' + RoundSigDigits( TotSurfaces - FixedShadingCount - BuildingShadingCount - AttachedShadingCount ) + ',' + RoundSigDigits( sum( Zone, &ZoneData::NumSubSurfaces ) );

		// Write Zone Information header to the initialization output file
		gio::write( OutputFileInits, Format_721 );

		for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
			// Write Zone Information to the initialization output file

			{ auto const SELECT_CASE_var( Zone( ZoneNum ).InsideConvectionAlgo );
			if ( SELECT_CASE_var == ASHRAESimple ) {
				String1 = "Simple";
			} else if ( SELECT_CASE_var == ASHRAETARP ) {
				String1 = "TARP";
			} else if ( SELECT_CASE_var == CeilingDiffuser ) {
				String1 = "CeilingDiffuser";
			} else if ( SELECT_CASE_var == TrombeWall ) {
				String1 = "TrombeWall";
			} else if ( SELECT_CASE_var == AdaptiveConvectionAlgorithm ) {
				String1 = "AdaptiveConvectionAlgorithm";
			}}

			{ auto const SELECT_CASE_var( Zone( ZoneNum ).OutsideConvectionAlgo );
			if ( SELECT_CASE_var == ASHRAESimple ) {
				String2 = "Simple";
			} else if ( SELECT_CASE_var == ASHRAETARP ) {
				String2 = "TARP";
			} else if ( SELECT_CASE_var == TarpHcOutside ) {
				String2 = "TARP";
			} else if ( SELECT_CASE_var == MoWiTTHcOutside ) {
				String2 = "MoWitt";
			} else if ( SELECT_CASE_var == DOE2HcOutside ) {
				String2 = "DOE-2";
				//      CASE (BLASTHcOutside)
				//        String2='BLAST'
			} else if ( SELECT_CASE_var == AdaptiveConvectionAlgorithm ) {
				String2 = "AdaptiveConvectionAlgorithm";

			}}

			if ( Zone( ZoneNum ).isPartOfTotalArea ) {
				String3 = "Yes";
			} else {
				String3 = "No";
			}

			gio::write( OutputFileInits, Format_720 ) << Zone( ZoneNum ).Name << RoundSigDigits( Zone( ZoneNum ).RelNorth, 1 ) << RoundSigDigits( Zone( ZoneNum ).OriginX, 2 ) << RoundSigDigits( Zone( ZoneNum ).OriginY, 2 ) << RoundSigDigits( Zone( ZoneNum ).OriginZ, 2 ) << RoundSigDigits( Zone( ZoneNum ).Centroid.x, 2 ) << RoundSigDigits( Zone( ZoneNum ).Centroid.y, 2 ) << RoundSigDigits( Zone( ZoneNum ).Centroid.z, 2 ) << RoundSigDigits( Zone( ZoneNum ).OfType ) << RoundSigDigits( Zone( ZoneNum ).Multiplier ) << RoundSigDigits( Zone( ZoneNum ).ListMultiplier ) << RoundSigDigits( Zone( ZoneNum ).MinimumX, 2 ) << RoundSigDigits( Zone( ZoneNum ).MaximumX, 2 ) << RoundSigDigits( Zone( ZoneNum ).MinimumY, 2 ) << RoundSigDigits( Zone( ZoneNum ).MaximumY, 2 ) << RoundSigDigits( Zone( ZoneNum ).MinimumZ, 2 ) << RoundSigDigits( Zone( ZoneNum ).MaximumZ, 2 ) << RoundSigDigits( Zone( ZoneNum ).CeilingHeight, 2 ) << RoundSigDigits( Zone( ZoneNum ).Volume, 2 ) << String1 << String2 << RoundSigDigits( Zone( ZoneNum ).FloorArea, 2 ) << RoundSigDigits( Zone( ZoneNum ).ExtGrossWallArea, 2 ) << RoundSigDigits( Zone( ZoneNum ).ExtNetWallArea, 2 ) << RoundSigDigits( Zone( ZoneNum ).ExtWindowArea, 2 ) << RoundSigDigits( Zone( ZoneNum ).NumSurfaces ) << RoundSigDigits( Zone( ZoneNum ).NumSubSurfaces ) << RoundSigDigits( Zone( ZoneNum ).NumShadingSurfaces ) << String3;

		} // ZoneNum

		// Do the Stratosphere check
		SetZoneOutBulbTempAt();
		CheckZoneOutBulbTempAt();

		//  IF (ALLOCATED(ZoneSurfacesCount)) DEALLOCATE(ZoneSurfacesCount)
		//  IF (ALLOCATED(ZoneSubSurfacesCount)) DEALLOCATE(ZoneSubSurfacesCount)
		//  IF (ALLOCATED(ZoneShadingSurfacesCount)) DEALLOCATE(ZoneShadingSurfacesCount)

	}

	void
	AllocateModuleArrays()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Rick Strand
		//       DATE WRITTEN   February 1998
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine allocates all of the arrays at the module level which
		// require allocation.

		// METHODOLOGY EMPLOYED:
		// Allocation is dependent on the user input file.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		// FLOW:

		ShadeV.allocate( TotSurfaces );
		for ( auto & e : ShadeV ) e.NVert = 0;
		// Individual components (XV,YV,ZV) allocated in routine ProcessSurfaceVertices
		X0.dimension( TotSurfaces, 0.0 );
		Y0.dimension( TotSurfaces, 0.0 );
		Z0.dimension( TotSurfaces, 0.0 );

		CBZone.dimension( NumOfZones, 0.0 );
		DSZone.dimension( NumOfZones, 0.0 );
		DGZone.dimension( NumOfZones, 0.0 );
		DBZone.dimension( NumOfZones, 0.0 );
		DBZoneSSG.dimension( NumOfZones, 0.0 );
		QSDifSol.dimension( NumOfZones, 0.0 );
		AISurf.dimension( TotSurfaces, 0.0 );
		AOSurf.dimension( TotSurfaces, 0.0 );
		BmToBmReflFacObs.dimension( TotSurfaces, 0.0 );
		BmToDiffReflFacObs.dimension( TotSurfaces, 0.0 );
		BmToDiffReflFacGnd.dimension( TotSurfaces, 0.0 );
		AWinSurf.dimension( CFSMAXNL + 1, TotSurfaces, 0.0 );
		AWinCFOverlap.dimension( MaxSolidWinLayers, TotSurfaces, 0.0 );

	}

	void
	GetSurfaceData( bool & ErrorsFound ) // If errors found in input
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Richard Liesen
		//       DATE WRITTEN   November 1997
		//       MODIFIED       April 1999, Linda Lawrie
		//                      Dec. 2000, FW (add "one-wall zone" checks)
		//       RE-ENGINEERED  May 2000, Linda Lawrie (breakout surface type gets)

		// PURPOSE OF THIS SUBROUTINE:
		// The purpose of this subroutine is to read in the surface information
		// from the input data file and interpret and put in the derived type

		// METHODOLOGY EMPLOYED:
		// The order of surfaces does not matter and the surfaces are resorted into
		// the hierarchical order:
		//  Detached Surfaces
		//  Base Surface for zone x
		//    Subsurfaces for base surface
		//  Base Surface for zone x
		//    etc
		//  Heat Transfer Surfaces and Shading surfaces are mixed in the list
		//  Pointers are set in the zones (First, Last)

		// REFERENCES:
		//   This routine manages getting the input for the following Objects:
		// SurfaceGeometry
		// Surface:Shading:Detached
		// Surface:HeatTransfer
		// Surface:HeatTransfer:Sub
		// Surface:Shading:Attached
		// Surface:InternalMass

		// Vertex input:
		//  N3 , \field Number of Surface Vertices -- Number of (X,Y,Z) groups in this surface
		//       \note currently limited 3 or 4, later?
		//       \min 3
		//       \max 4
		//       \memo vertices are given in SurfaceGeometry coordinates -- if relative, all surface coordinates
		//       \memo are "relative" to the Zone Origin.  if WCS, then building and zone origins are used
		//       \memo for some internal calculations, but all coordinates are given in an "absolute" system.
		//  N4,  \field Vertex 1 X-coordinate
		//       \units m
		//       \type real
		//  N5 , \field Vertex 1 Y-coordinate
		//       \units m
		//       \type real
		//  N6 , \field Vertex 1 Z-coordinate
		//       \units m
		//       \type real
		//  N7,  \field Vertex 2 X-coordinate
		//       \units m
		//       \type real
		//  N8,  \field Vertex 2 Y-coordinate
		//       \units m
		//       \type real
		//  N9,  \field Vertex 2 Z-coordinate
		//       \units m
		//       \type real
		//  N10, \field Vertex 3 X-coordinate
		//       \units m
		//       \type real
		//  N11, \field Vertex 3 Y-coordinate
		//       \units m
		//       \type real
		//  N12, \field Vertex 3 Z-coordinate
		//       \units m
		//       \type real
		//  N13, \field Vertex 4 X-coordinate
		//       \units m
		//       \type real
		//  N14, \field Vertex 4 Y-coordinate
		//       \type real
		//       \units m
		//  N15; \field Vertex 4 Z-coordinate
		//       \units m
		//       \type real

		// The vertices are stored in the surface derived type.
		//      +(1)-------------------------(4)+
		//      |                               |
		//      |                               |
		//      |                               |
		//      +(2)-------------------------(3)+
		//  The above diagram shows the actual coordinate points of a typical wall
		//  (you're on the outside looking toward the wall) as stored into
		//  Surface%Vertex(1:<number-of-sides>)

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using InputProcessor::SameString;
		using InputProcessor::VerifyName;
		using General::TrimSigDigits;
		using General::RoundSigDigits;
		using namespace Vectors;
		using ScheduleManager::GetScheduleMinValue;
		using ScheduleManager::GetScheduleMaxValue;
		using namespace DataErrorTracking;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const SurfaceClass_Moved( -1 );
		static std::string const RoutineName( "GetSurfaceData: " );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int ConstrNum; // Construction number
		int SubSurfNum; // DO loop counter/index for sub-surface number
		int SurfNum; // DO loop counter/index for surface number
		int ZoneNum; // DO loop counter (zones)
		int Found; // For matching interzone surfaces
		int ConstrNumFound; // Construction number of matching interzone surface
		static bool NonMatch( false ); // Error for non-matching interzone surfaces
		int Lay; // Layer number
		int MovedSurfs; // Number of Moved Surfaces (when sorting into hierarchical structure)
		static bool SurfError( false ); // General Surface Error, causes fatal error at end of routine
		int Loop;
		int BaseSurfNum;
		int TotLay; // Total layers in a construction
		int TotLayFound; // Total layers in the construction of a matching interzone surface
		int TotDetachedFixed; // Total Shading:Site:Detailed entries
		int TotDetachedBldg; // Total Shading:Building:Detailed entries
		int TotRectDetachedFixed; // Total Shading:Site entries
		int TotRectDetachedBldg; // Total Shading:Building entries
		int TotHTSurfs; // Number of BuildingSurface:Detailed items to obtain
		int TotDetailedWalls; // Number of Wall:Detailed items to obtain
		int TotDetailedRoofs; // Number of RoofCeiling:Detailed items to obtain
		int TotDetailedFloors; // Number of Floor:Detailed items to obtain
		int TotHTSubs; // Number of FenestrationSurface:Detailed items to obtain
		int TotShdSubs; // Number of Shading:Zone:Detailed items to obtain
		int TotIntMass; // Number of InternalMass items to obtain
		// Simple Surfaces (Rectangular)
		int TotRectExtWalls; // Number of Exterior Walls to obtain
		int TotRectIntWalls; // Number of Adiabatic Walls to obtain
		int TotRectIZWalls; // Number of Interzone Walls to obtain
		int TotRectUGWalls; // Number of Underground to obtain
		int TotRectRoofs; // Number of Roofs to obtain
		int TotRectCeilings; // Number of Adiabatic Ceilings to obtain
		int TotRectIZCeilings; // Number of Interzone Ceilings to obtain
		int TotRectGCFloors; // Number of Floors with Ground Contact to obtain
		int TotRectIntFloors; // Number of Adiabatic Walls to obtain
		int TotRectIZFloors; // Number of Interzone Floors to obtain
		int TotRectWindows;
		int TotRectDoors;
		int TotRectGlazedDoors;
		int TotRectIZWindows;
		int TotRectIZDoors;
		int TotRectIZGlazedDoors;
		int TotOverhangs;
		int TotOverhangsProjection;
		int TotFins;
		int TotFinsProjection;
		std::string ClassMsg;
		std::string Msg2;
		int OpaqueHTSurfs; // Number of floors, walls and roofs in a zone
		int OpaqueHTSurfsWithWin; // Number of floors, walls and roofs with windows in a zone
		int InternalMassSurfs; // Number of internal mass surfaces in a zone
		static bool RelWarning( false );
		int ConstrNumSh; // Shaded construction number for a window
		int LayNumOutside; // Outside material numbers for a shaded construction
		int BlNum; // Blind number
		bool WinShadingCtrlReferenced; // True if a WindowShadingControl is referenced by at least one window
		int ShadingCtrl; // WindowShadingControl number
		int AddedSubSurfaces; // Subsurfaces (windows) added when windows reference Window5 Data File
		// entries with two glazing systems
		int NeedToAddSurfaces; // Surfaces that will be added due to "unentered" other zone surface
		int NeedToAddSubSurfaces; // SubSurfaces that will be added due to "unentered" other zone surface
		int CurNewSurf;
		int FirstTotalSurfaces;
		int NVert;
		int Vert;
		int n;
		Real64 SurfWorldAz;
		Real64 SurfTilt;

		int MultFound;
		int MultSurfNum;
		std::string MultString;
		static bool WarningDisplayed( false );
		static int ErrCount2( 0 );
		static int ErrCount3( 0 );
		static int ErrCount4( 0 ); // counts of interzone area mismatches.
		bool SubSurfaceSevereDisplayed;
		bool subSurfaceError( false );
		// INTEGER :: Warning4Count=0  ! counts of nonmatched flat surface subsurface orientations
		// INTEGER :: Warning5Count=0  ! counts of nonmatched flat surface subsurface orientations - could not be resolved
		bool errFlag;

		int iTmp1;
		int iTmp2;
		//unused  INTEGER :: SchID
		int BlNumNew;
		int WinShadingControlPtr;
		int ShadingType;
		//unused  REAL(r64) :: SchSlatAngle = 0.0D0
		//unused  LOGICAL :: initmsg
		int ErrCount;
		Real64 diffp;
		//  LOGICAL :: Located
		bool izConstDiff; // differences in construction for IZ surfaces
		bool izConstDiffMsg; // display message about hb diffs only once.

		// FLOW:
		// Get the total number of surfaces to allocate derived type and for surface loops

		GetGeometryParameters( ErrorsFound );

		if ( WorldCoordSystem ) {
			if ( BuildingAzimuth != 0.0 ) RelWarning = true;
			for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
				if ( Zone( ZoneNum ).RelNorth != 0.0 ) RelWarning = true;
			}
			if ( RelWarning && ! WarningDisplayed ) {
				ShowWarningError( RoutineName + "World Coordinate System selected.  Any non-zero Building/Zone North Axes or non-zero Zone Origins are ignored." );
				ShowContinueError( "These may be used in daylighting reference point coordinate calculations but not in normal geometry inputs." );
				WarningDisplayed = true;
			}
			RelWarning = false;
			for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
				if ( Zone( ZoneNum ).OriginX != 0.0 ) RelWarning = true;
				if ( Zone( ZoneNum ).OriginY != 0.0 ) RelWarning = true;
				if ( Zone( ZoneNum ).OriginZ != 0.0 ) RelWarning = true;
			}
			if ( RelWarning && ! WarningDisplayed ) {
				ShowWarningError( RoutineName + "World Coordinate System selected.  Any non-zero Building/Zone North Axes or non-zero Zone Origins are ignored." );
				ShowContinueError( "These may be used in daylighting reference point coordinate calculations but not in normal geometry inputs." );
				WarningDisplayed = true;
			}
		}

		TotDetachedFixed = GetNumObjectsFound( "Shading:Site:Detailed" );
		TotDetachedBldg = GetNumObjectsFound( "Shading:Building:Detailed" );
		TotRectDetachedFixed = GetNumObjectsFound( "Shading:Site" );
		TotRectDetachedBldg = GetNumObjectsFound( "Shading:Building" );
		TotHTSurfs = GetNumObjectsFound( "BuildingSurface:Detailed" );
		TotDetailedWalls = GetNumObjectsFound( "Wall:Detailed" );
		TotDetailedRoofs = GetNumObjectsFound( "RoofCeiling:Detailed" );
		TotDetailedFloors = GetNumObjectsFound( "Floor:Detailed" );
		TotHTSubs = GetNumObjectsFound( "FenestrationSurface:Detailed" );
		TotShdSubs = GetNumObjectsFound( "Shading:Zone:Detailed" );
		TotOverhangs = GetNumObjectsFound( "Shading:Overhang" );
		TotOverhangsProjection = GetNumObjectsFound( "Shading:Overhang:Projection" );
		TotFins = GetNumObjectsFound( "Shading:Fin" );
		TotFinsProjection = GetNumObjectsFound( "Shading:Fin:Projection" );
		TotIntMass = GetNumObjectsFound( "InternalMass" );
		TotRectWindows = GetNumObjectsFound( "Window" );
		TotRectDoors = GetNumObjectsFound( "Door" );
		TotRectGlazedDoors = GetNumObjectsFound( "GlazedDoor" );
		TotRectIZWindows = GetNumObjectsFound( "Window:Interzone" );
		TotRectIZDoors = GetNumObjectsFound( "Door:Interzone" );
		TotRectIZGlazedDoors = GetNumObjectsFound( "GlazedDoor:Interzone" );
		TotRectExtWalls = GetNumObjectsFound( "Wall:Exterior" );
		TotRectIntWalls = GetNumObjectsFound( "Wall:Adiabatic" );
		TotRectIZWalls = GetNumObjectsFound( "Wall:Interzone" );
		TotRectUGWalls = GetNumObjectsFound( "Wall:Underground" );
		TotRectRoofs = GetNumObjectsFound( "Roof" );
		TotRectCeilings = GetNumObjectsFound( "Ceiling:Adiabatic" );
		TotRectIZCeilings = GetNumObjectsFound( "Ceiling:Interzone" );
		TotRectGCFloors = GetNumObjectsFound( "Floor:GroundContact" );
		TotRectIntFloors = GetNumObjectsFound( "Floor:Adiabatic" );
		TotRectIZFloors = GetNumObjectsFound( "Floor:Interzone" );

		TotOSC = 0;

		TotSurfaces = ( TotDetachedFixed + TotDetachedBldg + TotRectDetachedFixed + TotRectDetachedBldg ) * 2 + TotHTSurfs + TotHTSubs + TotShdSubs * 2 + TotIntMass + TotOverhangs * 2 + TotOverhangsProjection * 2 + TotFins * 4 + TotFinsProjection * 4 + TotDetailedWalls + TotDetailedRoofs + TotDetailedFloors + TotRectWindows + TotRectDoors + TotRectGlazedDoors + TotRectIZWindows + TotRectIZDoors + TotRectIZGlazedDoors + TotRectExtWalls + TotRectIntWalls + TotRectIZWalls + TotRectUGWalls + TotRectRoofs + TotRectCeilings + TotRectIZCeilings + TotRectGCFloors + TotRectIntFloors + TotRectIZFloors;

		SurfaceTmp.allocate( TotSurfaces ); // Allocate the Surface derived type appropriately
		// SurfaceTmp structure is allocated via derived type initialization.

		SurfNum = 0;
		AddedSubSurfaces = 0;
		AskForSurfacesReport = true;

		GetDetShdSurfaceData( ErrorsFound, SurfNum, TotDetachedFixed, TotDetachedBldg );

		GetRectDetShdSurfaceData( ErrorsFound, SurfNum, TotRectDetachedFixed, TotRectDetachedBldg );

		GetHTSurfaceData( ErrorsFound, SurfNum, TotHTSurfs, TotDetailedWalls, TotDetailedRoofs, TotDetailedFloors, BaseSurfCls, BaseSurfIDs, NeedToAddSurfaces );

		GetRectSurfaces( ErrorsFound, SurfNum, TotRectExtWalls, TotRectIntWalls, TotRectIZWalls, TotRectUGWalls, TotRectRoofs, TotRectCeilings, TotRectIZCeilings, TotRectGCFloors, TotRectIntFloors, TotRectIZFloors, BaseSurfIDs, NeedToAddSurfaces );

		GetHTSubSurfaceData( ErrorsFound, SurfNum, TotHTSubs, SubSurfCls, SubSurfIDs, AddedSubSurfaces, NeedToAddSubSurfaces );

		GetRectSubSurfaces( ErrorsFound, SurfNum, TotRectWindows, TotRectDoors, TotRectGlazedDoors, TotRectIZWindows, TotRectIZDoors, TotRectIZGlazedDoors, SubSurfIDs, AddedSubSurfaces, NeedToAddSubSurfaces );

		GetAttShdSurfaceData( ErrorsFound, SurfNum, TotShdSubs );

		GetSimpleShdSurfaceData( ErrorsFound, SurfNum, TotOverhangs, TotOverhangsProjection, TotFins, TotFinsProjection );

		GetIntMassSurfaceData( ErrorsFound, SurfNum, TotIntMass );

		GetMovableInsulationData( ErrorsFound );

		if ( CalcSolRefl ) GetShadingSurfReflectanceData( ErrorsFound );

		TotSurfaces = SurfNum + AddedSubSurfaces + NeedToAddSurfaces + NeedToAddSubSurfaces;

		// Have to make room for added surfaces, if needed
		FirstTotalSurfaces = SurfNum + AddedSubSurfaces;
		if ( NeedToAddSurfaces + NeedToAddSubSurfaces > 0 ) {
			SurfaceTmp.redimension( TotSurfaces );
		}

		SurfaceWindow.allocate( TotSurfaces );

		// add the "need to add" surfaces
		//Debug    write(outputfiledebug,*) ' need to add ',NeedtoAddSurfaces+NeedToAddSubSurfaces
		if ( NeedToAddSurfaces + NeedToAddSubSurfaces > 0 ) CurNewSurf = FirstTotalSurfaces;
		for ( SurfNum = 1; SurfNum <= FirstTotalSurfaces; ++SurfNum ) {
			if ( SurfaceTmp( SurfNum ).ExtBoundCond != UnenteredAdjacentZoneSurface ) continue;
			// Need to add surface
			++CurNewSurf;
			//Debug    write(outputfiledebug,*) ' adding surface=',curnewsurf
			SurfaceTmp( CurNewSurf ) = SurfaceTmp( SurfNum );
			//  Basic parameters are the same for both surfaces.
			Found = FindItemInList( SurfaceTmp( SurfNum ).ExtBoundCondName, Zone, NumOfZones );
			if ( Found == 0 ) continue;
			SurfaceTmp( CurNewSurf ).Zone = Found;
			SurfaceTmp( CurNewSurf ).ZoneName = Zone( Found ).Name;
			// Reverse Construction
			SurfaceTmp( CurNewSurf ).Construction = AssignReverseConstructionNumber( SurfaceTmp( SurfNum ).Construction, SurfError );
			SurfaceTmp( CurNewSurf ).ConstructionStoredInputValue = SurfaceTmp( CurNewSurf ).Construction;
			// Reverse Vertices
			NVert = SurfaceTmp( SurfNum ).Sides;
			for ( Vert = 1; Vert <= SurfaceTmp( SurfNum ).Sides; ++Vert ) {
				SurfaceTmp( CurNewSurf ).Vertex( Vert ) = SurfaceTmp( SurfNum ).Vertex( NVert );
				--NVert;
			}
			if ( SurfaceTmp( CurNewSurf ).Sides > 2 ) {
				CreateNewellAreaVector( SurfaceTmp( CurNewSurf ).Vertex, SurfaceTmp( CurNewSurf ).Sides, SurfaceTmp( CurNewSurf ).NewellAreaVector );
				SurfaceTmp( CurNewSurf ).GrossArea = VecLength( SurfaceTmp( CurNewSurf ).NewellAreaVector );
				SurfaceTmp( CurNewSurf ).Area = SurfaceTmp( CurNewSurf ).GrossArea;
				SurfaceTmp( CurNewSurf ).NetAreaShadowCalc = SurfaceTmp( CurNewSurf ).Area;
				CreateNewellSurfaceNormalVector( SurfaceTmp( CurNewSurf ).Vertex, SurfaceTmp( CurNewSurf ).Sides, SurfaceTmp( CurNewSurf ).NewellSurfaceNormalVector );
				DetermineAzimuthAndTilt( SurfaceTmp( CurNewSurf ).Vertex, SurfaceTmp( CurNewSurf ).Sides, SurfWorldAz, SurfTilt, SurfaceTmp( CurNewSurf ).lcsx, SurfaceTmp( CurNewSurf ).lcsy, SurfaceTmp( CurNewSurf ).lcsz, SurfaceTmp( CurNewSurf ).GrossArea, SurfaceTmp( CurNewSurf ).NewellSurfaceNormalVector );
				SurfaceTmp( CurNewSurf ).Azimuth = SurfWorldAz;
				SurfaceTmp( CurNewSurf ).Tilt = SurfTilt;

				// Sine and cosine of azimuth and tilt
				SurfaceTmp( CurNewSurf ).SinAzim = std::sin( SurfWorldAz * DegToRadians );
				SurfaceTmp( CurNewSurf ).CosAzim = std::cos( SurfWorldAz * DegToRadians );
				SurfaceTmp( CurNewSurf ).SinTilt = std::sin( SurfTilt * DegToRadians );
				SurfaceTmp( CurNewSurf ).CosTilt = std::cos( SurfTilt * DegToRadians );
				// Outward normal unit vector (pointing away from room)
				SurfaceTmp( CurNewSurf ).OutNormVec = SurfaceTmp( CurNewSurf ).NewellSurfaceNormalVector;
				for ( n = 1; n <= 3; ++n ) {
					if ( std::abs( SurfaceTmp( CurNewSurf ).OutNormVec( n ) - 1.0 ) < 1.e-06 ) SurfaceTmp( CurNewSurf ).OutNormVec( n ) = +1.0;
					if ( std::abs( SurfaceTmp( CurNewSurf ).OutNormVec( n ) + 1.0 ) < 1.e-06 ) SurfaceTmp( CurNewSurf ).OutNormVec( n ) = -1.0;
					if ( std::abs( SurfaceTmp( CurNewSurf ).OutNormVec( n ) ) < 1.e-06 ) SurfaceTmp( CurNewSurf ).OutNormVec( n ) = 0.0;
				}

				// Can perform tests on this surface here
				SurfaceTmp( CurNewSurf ).ViewFactorSky = 0.5 * ( 1.0 + SurfaceTmp( CurNewSurf ).CosTilt );
				SurfaceTmp( CurNewSurf ).ViewFactorGround = 0.5 * ( 1.0 - SurfaceTmp( CurNewSurf ).CosTilt );

				// The following IR view factors are modified in subr. SkyDifSolarShading if there are shadowing
				// surfaces
				SurfaceTmp( CurNewSurf ).ViewFactorSkyIR = SurfaceTmp( CurNewSurf ).ViewFactorSky;
				SurfaceTmp( CurNewSurf ).ViewFactorGroundIR = 0.5 * ( 1.0 - SurfaceTmp( CurNewSurf ).CosTilt );
			}

			// Change Name
			SurfaceTmp( CurNewSurf ).Name = "iz-" + SurfaceTmp( SurfNum ).Name;
			//Debug   write(outputfiledebug,*) ' new surf name=',TRIM(SurfaceTmp(CurNewSurf)%Name)
			//Debug   write(outputfiledebug,*) ' new surf in zone=',TRIM(surfacetmp(curnewsurf)%zoneName)
			SurfaceTmp( CurNewSurf ).ExtBoundCond = UnreconciledZoneSurface;
			SurfaceTmp( SurfNum ).ExtBoundCond = UnreconciledZoneSurface;
			SurfaceTmp( CurNewSurf ).ExtBoundCondName = SurfaceTmp( SurfNum ).Name;
			SurfaceTmp( SurfNum ).ExtBoundCondName = SurfaceTmp( CurNewSurf ).Name;
			if ( SurfaceTmp( CurNewSurf ).Class == SurfaceClass_Roof || SurfaceTmp( CurNewSurf ).Class == SurfaceClass_Wall || SurfaceTmp( CurNewSurf ).Class == SurfaceClass_Floor ) {
				// base surface
				if ( SurfaceTmp( SurfNum ).Class == SurfaceClass_Roof ) {
					SurfaceTmp( CurNewSurf ).Class = SurfaceClass_Floor;
					//Debug          write(outputfiledebug,*) ' new surfaces is a floor'
				} else if ( SurfaceTmp( SurfNum ).Class == SurfaceClass_Floor ) {
					SurfaceTmp( CurNewSurf ).Class = SurfaceClass_Roof;
					//Debug          write(outputfiledebug,*) ' new surfaces is a roof'
				}
				SurfaceTmp( CurNewSurf ).BaseSurf = CurNewSurf;
				SurfaceTmp( CurNewSurf ).BaseSurfName = SurfaceTmp( CurNewSurf ).Name;
				//Debug        write(outputfiledebug,*) ' basesurf, extboundcondname=',TRIM(SurfaceTmp(CurNewSurf)%ExtBoundCondName)
			} else {
				// subsurface
				Found = FindItemInList( "iz-" + SurfaceTmp( SurfNum ).BaseSurfName, SurfaceTmp, FirstTotalSurfaces + CurNewSurf - 1 );
				if ( Found > 0 ) {
					SurfaceTmp( CurNewSurf ).BaseSurfName = "iz-" + SurfaceTmp( SurfNum ).BaseSurfName;
					SurfaceTmp( CurNewSurf ).BaseSurf = Found;
					SurfaceTmp( Found ).Area -= SurfaceTmp( CurNewSurf ).Area;
					if ( SurfaceTmp( CurNewSurf ).Class == SurfaceClass_Window || SurfaceTmp( CurNewSurf ).Class == SurfaceClass_GlassDoor ) {
						SurfaceTmp( Found ).NetAreaShadowCalc -= SurfaceTmp( CurNewSurf ).Area / SurfaceTmp( CurNewSurf ).Multiplier;
					} else { // Door, TDD:Diffuser, TDD:DOME
						SurfaceTmp( Found ).NetAreaShadowCalc -= SurfaceTmp( CurNewSurf ).Area;
					}
					SurfaceTmp( CurNewSurf ).ExtBoundCond = SurfaceTmp( Found ).ExtBoundCond;
					SurfaceTmp( CurNewSurf ).ExtBoundCondName = SurfaceTmp( SurfNum ).Name;
					SurfaceTmp( CurNewSurf ).ExtSolar = SurfaceTmp( Found ).ExtSolar;
					SurfaceTmp( CurNewSurf ).ExtWind = SurfaceTmp( Found ).ExtWind;
					SurfaceTmp( CurNewSurf ).Zone = SurfaceTmp( Found ).Zone;
					SurfaceTmp( CurNewSurf ).ZoneName = SurfaceTmp( Found ).ZoneName;
					SurfaceTmp( CurNewSurf ).OSCPtr = SurfaceTmp( Found ).OSCPtr;
					//Debug        write(outputfiledebug,*) ' subsurf, extboundcondname=',TRIM(SurfaceTmp(CurNewSurf)%ExtBoundCondName)
					//Debug        write(outputfiledebug,*) ' subsurf, basesurf=',TRIM('iz-'//SurfaceTmp(SurfNum)%BaseSurfName)
				} else {
					ShowSevereError( RoutineName + "Adding unentered subsurface, could not find base surface=" + "iz-" + SurfaceTmp( SurfNum ).BaseSurfName );
					SurfError = true;
				}
			}

		}
		//**********************************************************************************
		// After all of the surfaces have been defined then the base surfaces for the
		// sub-surfaces can be defined.  Loop through surfaces and match with the sub-surface
		// names.
		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
			if ( ! SurfaceTmp( SurfNum ).HeatTransSurf ) continue;

			// why are we doing this again?  this should have already been done.
			if ( SameString( SurfaceTmp( SurfNum ).BaseSurfName, SurfaceTmp( SurfNum ).Name ) ) {
				Found = SurfNum;
			} else {
				Found = FindItemInList( SurfaceTmp( SurfNum ).BaseSurfName, SurfaceTmp, TotSurfaces );
			}
			if ( Found > 0 ) {
				SurfaceTmp( SurfNum ).BaseSurf = Found;
				if ( SurfNum != Found ) { // for subsurfaces
					if ( SurfaceTmp( SurfNum ).HeatTransSurf ) ++SurfaceTmp( Found ).NumSubSurfaces;
					if ( SurfaceTmp( SurfNum ).Class < SurfaceClass_Window || SurfaceTmp( SurfNum ).Class > SurfaceClass_TDD_Diffuser ) {
						if ( SurfaceTmp( SurfNum ).Class == 0 ) {
							ShowSevereError( RoutineName + "Invalid SubSurface detected, Surface=" + SurfaceTmp( SurfNum ).Name );
						} else {
							ShowSevereError( RoutineName + "Invalid SubSurface detected, Surface=" + SurfaceTmp( SurfNum ).Name + ", class=" + BaseSurfCls( SurfaceTmp( SurfNum ).Class ) + " invalid class for subsurface" );
							SurfError = true;
						}
					}
				}
			}

		} // ...end of the Surface DO loop for finding BaseSurf
		//**********************************************************************************

		// The surfaces need to be hierarchical.  Input is allowed to be in any order.  In
		// this section it is reordered into:

		//    Detached shadowing surfaces
		//    For each zone:
		//      For each Wall
		//        subsurfaces (windows, doors, shading) for that wall
		//      For each Floor
		//        subsurfaces for that floor
		//      For each Roof
		//        subsurfaces for that roof/ceiling
		//      Internal Mass
		//    After reordering, MovedSurfs should equal TotSurfaces

		MovedSurfs = 0;
		Surface.allocate( TotSurfaces ); // Allocate the Surface derived type appropriately

		// Move all Detached Surfaces to Front

		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
			if ( SurfaceTmp( SurfNum ).Class != SurfaceClass_Detached_F && SurfaceTmp( SurfNum ).Class != SurfaceClass_Detached_B && SurfaceTmp( SurfNum ).Class != SurfaceClass_Shading ) continue;

			//  A shading surface

			++MovedSurfs;
			Surface( MovedSurfs ) = SurfaceTmp( SurfNum );
			SurfaceTmp( SurfNum ).Class = SurfaceClass_Moved; //'Moved'
		}

		//  For each zone

		for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {

			//  For each Base Surface Type (Wall, Floor, Roof)

			for ( Loop = 1; Loop <= 3; ++Loop ) {

				for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {

					if ( SurfaceTmp( SurfNum ).Zone == 0 ) continue;

					if ( ! SameString( SurfaceTmp( SurfNum ).ZoneName, Zone( ZoneNum ).Name ) ) continue;
					if ( SurfaceTmp( SurfNum ).Class != BaseSurfIDs( Loop ) ) continue;

					++MovedSurfs;
					Surface( MovedSurfs ) = SurfaceTmp( SurfNum );
					SurfaceTmp( SurfNum ).Class = SurfaceClass_Moved; // 'Moved'
					SurfaceTmp( SurfNum ).BaseSurf = -1; // Default has base surface = base surface
					BaseSurfNum = MovedSurfs;
					Surface( MovedSurfs ).BaseSurf = BaseSurfNum;

					//  Find all subsurfaces to this surface
					for ( SubSurfNum = 1; SubSurfNum <= TotSurfaces; ++SubSurfNum ) {

						if ( SurfaceTmp( SubSurfNum ).Zone == 0 ) continue;
						if ( SurfaceTmp( SubSurfNum ).BaseSurf != SurfNum ) continue;

						++MovedSurfs;
						Surface( MovedSurfs ) = SurfaceTmp( SubSurfNum );
						SurfaceTmp( SubSurfNum ).Class = SurfaceClass_Moved; // 'Moved'
						Surface( MovedSurfs ).BaseSurf = BaseSurfNum;
						SurfaceTmp( SubSurfNum ).BaseSurf = -1;
					}
				}
			}

			for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {

				if ( SurfaceTmp( SurfNum ).ZoneName != Zone( ZoneNum ).Name ) continue;
				if ( SurfaceTmp( SurfNum ).Class != SurfaceClass_IntMass ) continue;

				++MovedSurfs;
				Surface( MovedSurfs ) = SurfaceTmp( SurfNum );
				Surface( MovedSurfs ).BaseSurf = MovedSurfs;
				SurfaceTmp( SurfNum ).Class = SurfaceClass_Moved; // 'Moved'
			}
		}

		if ( MovedSurfs != TotSurfaces ) {
			gio::write( ClassMsg, fmtLD ) << MovedSurfs;
			strip( ClassMsg );
			gio::write( Msg2, fmtLD ) << TotSurfaces;
			strip( Msg2 );
			ShowSevereError( RoutineName + "Reordered # of Surfaces (" + ClassMsg + ") not = Total # of Surfaces (" + Msg2 + ')' );
			SurfError = true;
			for ( Loop = 1; Loop <= TotSurfaces; ++Loop ) {
				if ( SurfaceTmp( Loop ).Class != SurfaceClass_Moved ) {
					if ( SurfaceTmp( Loop ).Class > 100 ) {
						ShowSevereError( RoutineName + "Error in Surface= \"" + SurfaceTmp( Loop ).Name + "\" Class=" + cSurfaceClass( SurfaceTmp( Loop ).Class - 100 ) + " indicated Zone=\"" + SurfaceTmp( Loop ).ZoneName + "\"" );
					}
				}
			}
			ShowWarningError( RoutineName + "Remaining surface checks will use \"reordered number of surfaces\", not number of original surfaces" );
		}

		SurfaceTmp.deallocate(); // DeAllocate the Temp Surface derived type

		//  For each Base Surface Type (Wall, Floor, Roof)

		for ( Loop = 1; Loop <= 3; ++Loop ) {

			for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {

				if ( Surface( SurfNum ).Zone == 0 ) continue;

				if ( Surface( SurfNum ).Class != BaseSurfIDs( Loop ) ) continue;

				//  Find all subsurfaces to this surface
				for ( SubSurfNum = 1; SubSurfNum <= TotSurfaces; ++SubSurfNum ) {

					if ( SurfNum == SubSurfNum ) continue;
					if ( Surface( SubSurfNum ).Zone == 0 ) continue;
					if ( Surface( SubSurfNum ).BaseSurf != SurfNum ) continue;

					// Check facing angle of Sub compared to base
					checkSubSurfAzTiltNorm( Surface( SurfNum ), Surface( SubSurfNum ), subSurfaceError );
					if (subSurfaceError) SurfError= true;
				}
			}
		}

		//**********************************************************************************
		// Now, match up interzone surfaces
		NonMatch = false;
		izConstDiffMsg = false;
		for ( SurfNum = 1; SurfNum <= MovedSurfs; ++SurfNum ) { //TotSurfaces
			//  Clean up Shading Surfaces, make sure they don't go through here.
			//  Shading surfaces have "Zone=0", should also have "BaseSurf=0"
			//  PGE: Revised so that shading surfaces can have BaseSurf /= 0 if they are daylighting shelves
			//       or other exterior reflecting surfaces.
			//IF (Surface(SurfNum)%Zone == 0) THEN
			//  Surface(SurfNum)%BaseSurf=0
			//  CYCLE
			//ENDIF
			if ( ! Surface( SurfNum ).HeatTransSurf ) continue;
			//   If other surface, match it up
			//  Both interzone and "internal" surfaces have this pointer set
			//  Internal surfaces point to themselves, Interzone to another
			if ( Surface( SurfNum ).ExtBoundCond == UnreconciledZoneSurface ) {
				if ( not_blank( Surface( SurfNum ).ExtBoundCondName ) ) {
					if ( Surface( SurfNum ).ExtBoundCondName == Surface( SurfNum ).Name ) {
						Found = SurfNum;
					} else {
						Found = FindItemInList( Surface( SurfNum ).ExtBoundCondName, Surface, MovedSurfs );
					}
					if ( Found != 0 ) {
						Surface( SurfNum ).ExtBoundCond = Found;
						// Check that matching surface is also "OtherZoneSurface"
						if ( Surface( Found ).ExtBoundCond <= 0 && Surface( Found ).ExtBoundCond != UnreconciledZoneSurface ) {
							ShowSevereError( RoutineName + "Potential \"OtherZoneSurface\" is not matched correctly:" );

							ShowContinueError( "Surface=" + Surface( SurfNum ).Name + ", Zone=" + Surface( SurfNum ).ZoneName );
							ShowContinueError( "Nonmatched Other/InterZone Surface=" + Surface( Found ).Name + ", Zone=" + Surface( Found ).ZoneName );
							SurfError = true;
						}
						// Check that matching interzone surface has construction with reversed layers
						if ( Found != SurfNum ) { // Interzone surface
							// Make sure different zones too (CR 4110)
							if ( Surface( SurfNum ).Zone == Surface( Found ).Zone ) {
								++ErrCount2;
								if ( ErrCount2 == 1 && ! DisplayExtraWarnings ) {
									ShowWarningError( RoutineName + "CAUTION -- Interzone surfaces are occuring in the same zone(s)." );
									ShowContinueError( "...use Output:Diagnostics,DisplayExtraWarnings; to show more details on individual occurrences." );
								}
								if ( DisplayExtraWarnings ) {
									ShowWarningError( RoutineName + "CAUTION -- Interzone surfaces are usually in different zones" );
									ShowContinueError( "Surface=" + Surface( SurfNum ).Name + ", Zone=" + Surface( SurfNum ).ZoneName );
									ShowContinueError( "Surface=" + Surface( Found ).Name + ", Zone=" + Surface( Found ).ZoneName );
								}
							}
							ConstrNum = Surface( SurfNum ).Construction;
							ConstrNumFound = Surface( Found ).Construction;
							if ( ConstrNum <= 0 || ConstrNumFound <= 0 ) continue;
							if ( Construct( ConstrNum ).ReverseConstructionNumLayersWarning && Construct( ConstrNumFound ).ReverseConstructionNumLayersWarning ) continue;
							if ( Construct( ConstrNum ).ReverseConstructionLayersOrderWarning && Construct( ConstrNumFound ).ReverseConstructionLayersOrderWarning ) continue;
							TotLay = Construct( ConstrNum ).TotLayers;
							TotLayFound = Construct( ConstrNumFound ).TotLayers;
							if ( TotLay != TotLayFound ) { // Different number of layers
								// match on like Uvalues (nominal)
								if ( std::abs( NominalU( ConstrNum ) - NominalU( ConstrNumFound ) ) > 0.001 ) {
									ShowSevereError( RoutineName + "Construction " + Construct( ConstrNum ).Name + " of interzone surface " + Surface( SurfNum ).Name + " does not have the same number of layers as the construction " + Construct( ConstrNumFound ).Name + " of adjacent surface " + Surface( Found ).Name );
									if ( ! Construct( ConstrNum ).ReverseConstructionNumLayersWarning || ! Construct( ConstrNumFound ).ReverseConstructionNumLayersWarning ) {
										ShowContinueError( "...this problem for this pair will not be reported again." );
										Construct( ConstrNum ).ReverseConstructionNumLayersWarning = true;
										Construct( ConstrNumFound ).ReverseConstructionNumLayersWarning = true;
									}
									SurfError = true;
								}
							} else { // Same number of layers; check for reverse layers
								// check layers as number of layers is the same
								izConstDiff = false;
								// ok if same nominal U
								for ( Lay = 1; Lay <= TotLay; ++Lay ) {
									if ( Construct( ConstrNum ).LayerPoint( Lay ) != Construct( ConstrNumFound ).LayerPoint( TotLay - Lay + 1 ) ) {
										izConstDiff = true;
										break; // exit when diff
									}
								}
								if ( izConstDiff && std::abs( NominalU( ConstrNum ) - NominalU( ConstrNumFound ) ) > 0.001 ) {
									ShowSevereError( RoutineName + "Construction " + Construct( ConstrNum ).Name + " of interzone surface " + Surface( SurfNum ).Name + " does not have the same materials in the reverse order as the construction " + Construct( ConstrNumFound ).Name + " of adjacent surface " + Surface( Found ).Name );
									if ( ! Construct( ConstrNum ).ReverseConstructionLayersOrderWarning || ! Construct( ConstrNumFound ).ReverseConstructionLayersOrderWarning ) {
										ShowContinueError( "...this problem for this pair will not be reported again." );
										Construct( ConstrNum ).ReverseConstructionLayersOrderWarning = true;
										Construct( ConstrNumFound ).ReverseConstructionLayersOrderWarning = true;
									}
									SurfError = true;
								} else if ( izConstDiff ) {
									ShowWarningError( RoutineName + "Construction " + Construct( ConstrNum ).Name + " of interzone surface " + Surface( SurfNum ).Name + " does not have the same materials in the reverse order as the construction " + Construct( ConstrNumFound ).Name + " of adjacent surface " + Surface( Found ).Name );
									ShowContinueError( "...but Nominal U values are similar, diff=[" + RoundSigDigits( std::abs( NominalU( ConstrNum ) - NominalU( ConstrNumFound ) ), 4 ) + "] ... simulation proceeds." );
									if ( ! izConstDiffMsg ) {
										ShowContinueError( "...if the two zones are expected to have significantly different temperatures, the proper \"reverse\" construction should be created." );
										izConstDiffMsg = true;
									}
									if ( ! Construct( ConstrNum ).ReverseConstructionLayersOrderWarning || ! Construct( ConstrNumFound ).ReverseConstructionLayersOrderWarning ) {
										ShowContinueError( "...this problem for this pair will not be reported again." );
										Construct( ConstrNum ).ReverseConstructionLayersOrderWarning = true;
										Construct( ConstrNumFound ).ReverseConstructionLayersOrderWarning = true;
									}
								}
							}

							// If significantly different areas -- this would not be good
							MultFound = Zone( Surface( Found ).Zone ).Multiplier * Zone( Surface( Found ).Zone ).ListMultiplier;
							MultSurfNum = Zone( Surface( SurfNum ).Zone ).Multiplier * Zone( Surface( SurfNum ).Zone ).ListMultiplier;
							if ( Surface( Found ).Area > 0.0 ) {
								if ( std::abs( ( Surface( Found ).Area * MultFound - Surface( SurfNum ).Area * MultSurfNum ) / Surface( Found ).Area * MultFound ) > 0.02 ) { // 2% difference in areas
									++ErrCount4;
									if ( ErrCount4 == 1 && ! DisplayExtraWarnings ) {
										ShowWarningError( RoutineName + "InterZone Surface Areas do not match as expected and might not satisfy conservation of energy:" );
										ShowContinueError( "...use Output:Diagnostics,DisplayExtraWarnings; to show more details on individual mismatches." );
									}
									if ( DisplayExtraWarnings ) {
										ShowWarningError( RoutineName + "InterZone Surface Areas do not match as expected and might not satisfy conservation of energy:" );

										if ( MultFound == 1 && MultSurfNum == 1 ) {
											ShowContinueError( "  Area=" + TrimSigDigits( Surface( SurfNum ).Area, 1 ) + " in Surface=" + Surface( SurfNum ).Name + ", Zone=" + Surface( SurfNum ).ZoneName );
											ShowContinueError( "  Area=" + TrimSigDigits( Surface( Found ).Area, 1 ) + " in Surface=" + Surface( Found ).Name + ", Zone=" + Surface( Found ).ZoneName );
										} else { // Show multiplier info
											gio::write( MultString, fmtLD ) << MultSurfNum;
											strip( MultString );
											ShowContinueError( "  Area=" + TrimSigDigits( Surface( SurfNum ).Area, 1 ) + ", Multipliers=" + MultString + ", Total Area=" + TrimSigDigits( Surface( SurfNum ).Area * MultSurfNum, 1 ) + " in Surface=" + Surface( SurfNum ).Name + " Zone=" + Surface( SurfNum ).ZoneName );
											gio::write( MultString, fmtLD ) << MultFound;
											strip( MultString );
											ShowContinueError( "  Area=" + TrimSigDigits( Surface( Found ).Area, 1 ) + ", Multipliers=" + MultString + ", Total Area=" + TrimSigDigits( Surface( Found ).Area * MultFound, 1 ) + " in Surface=" + Surface( Found ).Name + " Zone=" + Surface( Found ).ZoneName );
										}
									}
								}
							}
							// Check opposites Azimuth and Tilt
							// Tilt
							if ( std::abs( std::abs( Surface( Found ).Tilt + Surface( SurfNum ).Tilt ) - 180.0 ) > 1.0 ) {
								ShowWarningError( RoutineName + "InterZone Surface Tilts do not match as expected." );
								ShowContinueError( "  Tilt=" + TrimSigDigits( Surface( SurfNum ).Tilt, 1 ) + " in Surface=" + Surface( SurfNum ).Name + ", Zone=" + Surface( SurfNum ).ZoneName );
								ShowContinueError( "  Tilt=" + TrimSigDigits( Surface( Found ).Tilt, 1 ) + " in Surface=" + Surface( Found ).Name + ", Zone=" + Surface( Found ).ZoneName );
							}
							// check surface class match.  interzone surface.
							if ( ( Surface( SurfNum ).Class == SurfaceClass_Wall && Surface( Found ).Class != SurfaceClass_Wall ) || ( Surface( SurfNum ).Class != SurfaceClass_Wall && Surface( Found ).Class == SurfaceClass_Wall ) ) {
								ShowWarningError( RoutineName + "InterZone Surface Classes do not match as expected." );
								ShowContinueError( "Surface=\"" + Surface( SurfNum ).Name + "\", surface class=" + cSurfaceClass( Surface( SurfNum ).Class ) );
								ShowContinueError( "Adjacent Surface=\"" + Surface( Found ).Name + "\", surface class=" + cSurfaceClass( Surface( Found ).Class ) );
								ShowContinueError( "Other errors/warnings may follow about these surfaces." );
							}
							if ( ( Surface( SurfNum ).Class == SurfaceClass_Roof && Surface( Found ).Class != SurfaceClass_Floor ) || ( Surface( SurfNum ).Class != SurfaceClass_Roof && Surface( Found ).Class == SurfaceClass_Floor ) ) {
								ShowWarningError( RoutineName + "InterZone Surface Classes do not match as expected." );
								ShowContinueError( "Surface=\"" + Surface( SurfNum ).Name + "\", surface class=" + cSurfaceClass( Surface( SurfNum ).Class ) );
								ShowContinueError( "Adjacent Surface=\"" + Surface( Found ).Name + "\", surface class=" + cSurfaceClass( Surface( Found ).Class ) );
								ShowContinueError( "Other errors/warnings may follow about these surfaces." );
							}
							if ( Surface( SurfNum ).Class != SurfaceClass_Roof && Surface( SurfNum ).Class != SurfaceClass_Floor ) {
								// Walls, Windows, Doors, Glass Doors
								if ( Surface( SurfNum ).Class != SurfaceClass_Wall ) {
									// Surface is a Door, Window or Glass Door
									if ( Surface( SurfNum ).BaseSurf == 0 ) continue; // error detected elsewhere
									if ( Surface( Surface( SurfNum ).BaseSurf ).Class == SurfaceClass_Roof || Surface( Surface( SurfNum ).BaseSurf ).Class == SurfaceClass_Floor ) continue;
								}
								if ( std::abs( std::abs( Surface( SurfNum ).Azimuth - Surface( Found ).Azimuth ) - 180.0 ) > 1.0 ) {
									if ( std::abs( Surface( SurfNum ).SinTilt ) > 0.5 || DisplayExtraWarnings ) {
										// if horizontal surfaces, then these are windows/doors/etc in those items.
										ShowWarningError( RoutineName + "InterZone Surface Azimuths do not match as expected." );
										ShowContinueError( "  Azimuth=" + TrimSigDigits( Surface( SurfNum ).Azimuth, 1 ) + ", Tilt=" + TrimSigDigits( Surface( SurfNum ).Tilt, 1 ) + ", in Surface=" + Surface( SurfNum ).Name + ", Zone=" + Surface( SurfNum ).ZoneName );
										ShowContinueError( "  Azimuth=" + TrimSigDigits( Surface( Found ).Azimuth, 1 ) + ", Tilt=" + TrimSigDigits( Surface( Found ).Tilt, 1 ) + ", in Surface=" + Surface( Found ).Name + ", Zone=" + Surface( Found ).ZoneName );
										ShowContinueError( "..surface class of first surface=" + cSurfaceClass( Surface( SurfNum ).Class ) );
										ShowContinueError( "..surface class of second surface=" + cSurfaceClass( Surface( Found ).Class ) );
									}
								}
							} else { // Roofs, Floors
								// should be looking at opposite tilts, not azimuth for roof/floor matches...
								//              IF (ABS(ABS(Surface(SurfNum)%Azimuth+Surface(Found)%Azimuth)-360.) > 1.0d0) THEN
								//                CALL ShowWarningError('InterZone Surface Azimuths do not match as expected.')
								//                CALL ShowContinueError('  Azimuth='//TRIM(TrimSigDigits(Surface(SurfNum)%Azimuth,1))//  &
								//                                       ' in Surface='//TRIM(Surface(SurfNum)%Name)//', Zone='//TRIM(Surface(SurfNum)%ZoneName))
								//                CALL ShowContinueError('  Azimuth='//TRIM(TrimSigDigits(Surface(Found)%Azimuth,1))//  &
								//                                       ' in Surface='//TRIM(Surface(Found)%Name)//', Zone='//TRIM(Surface(Found)%ZoneName))
								//              ENDIF
							}

							// Make sure exposures (Sun, Wind) are the same.....and are "not"
							if ( Surface( SurfNum ).ExtSolar || Surface( Found ).ExtSolar ) {
								ShowWarningError( RoutineName + "Interzone surfaces cannot be \"SunExposed\" -- removing SunExposed" );
								ShowContinueError( "  Surface=" + Surface( SurfNum ).Name + ", Zone=" + Surface( SurfNum ).ZoneName );
								ShowContinueError( "  Surface=" + Surface( Found ).Name + ", Zone=" + Surface( Found ).ZoneName );
								Surface( SurfNum ).ExtSolar = false;
								Surface( Found ).ExtSolar = false;
							}
							if ( Surface( SurfNum ).ExtWind || Surface( Found ).ExtWind ) {
								ShowWarningError( RoutineName + "Interzone surfaces cannot be \"WindExposed\" -- removing WindExposed" );
								ShowContinueError( "  Surface=" + Surface( SurfNum ).Name + ", Zone=" + Surface( SurfNum ).ZoneName );
								ShowContinueError( "  Surface=" + Surface( Found ).Name + ", Zone=" + Surface( Found ).ZoneName );
								Surface( SurfNum ).ExtWind = false;
								Surface( Found ).ExtWind = false;
							}
						}
						// Set opposing surface back to this one (regardless of error)
						Surface( Found ).ExtBoundCond = SurfNum;
						// Check subsurfaces...  make sure base surface is also an interzone surface
						if ( Surface( SurfNum ).BaseSurf != SurfNum ) { // Subsurface
							if ( ( Surface( SurfNum ).ExtBoundCond != SurfNum ) && not_blank( Surface( SurfNum ).ExtBoundCondName ) ) {
								// if not internal subsurface
								if ( Surface( Surface( SurfNum ).BaseSurf ).ExtBoundCond == Surface( SurfNum ).BaseSurf ) {
									// base surface is not interzone surface
									ShowSevereError( RoutineName + "SubSurface=\"" + Surface( SurfNum ).Name + "\" is an interzone subsurface." );
									ShowContinueError( "..but the Base Surface is not an interzone surface, Surface=\"" + Surface( Surface( SurfNum ).BaseSurf ).Name + "\"." );
									SurfError = true;
								}
							}
						}
					} else {
						//  Seems unlikely that an internal surface would be missing itself, so this message
						//  only indicates for adjacent (interzone) surfaces.
						ShowSevereError( RoutineName + "Adjacent Surface not found: " + Surface( SurfNum ).ExtBoundCondName + " adjacent to surface " + Surface( SurfNum ).Name );
						NonMatch = true;
						SurfError = true;
					}
				} else if ( Surface( SurfNum ).BaseSurf != SurfNum ) { // Subsurface
					if ( Surface( Surface( SurfNum ).BaseSurf ).ExtBoundCond > 0 && Surface( Surface( SurfNum ).BaseSurf ).ExtBoundCond != Surface( SurfNum ).BaseSurf ) { // If Interzone surface, subsurface must be also.
						ShowSevereError( RoutineName + "SubSurface on Interzone Surface must be an Interzone SubSurface." );
						ShowContinueError( "...OutsideFaceEnvironment is blank, in Surface=" + Surface( SurfNum ).Name );
						SurfError = true;
					} else {
						++ErrCount3;
						if ( ErrCount3 == 1 && ! DisplayExtraWarnings ) {
							ShowWarningError( RoutineName + "Blank name for Outside Boundary Condition Objects." );
							ShowContinueError( "...use Output:Diagnostics,DisplayExtraWarnings; to show more details on individual surfaces." );
						}
						if ( DisplayExtraWarnings ) {
							ShowWarningError( RoutineName + "Blank name for Outside Boundary Condition Object, in surface=" + Surface( SurfNum ).Name );
							ShowContinueError( "Resetting this surface to be an internal zone surface, zone=" + Surface( SurfNum ).ZoneName );
						}
						Surface( SurfNum ).ExtBoundCondName = Surface( SurfNum ).Name;
						Surface( SurfNum ).ExtBoundCond = SurfNum;
					}
				} else {
					++ErrCount3;
					if ( ErrCount3 == 1 && ! DisplayExtraWarnings ) {
						ShowSevereError( RoutineName + "Blank name for Outside Boundary Condition Objects." );
						ShowContinueError( "...use Output:Diagnostics,DisplayExtraWarnings; to show more details on individual surfaces." );
					}
					if ( DisplayExtraWarnings ) {
						ShowWarningError( RoutineName + "Blank name for Outside Boundary Condition Object, in surface=" + Surface( SurfNum ).Name );
						ShowContinueError( "Resetting this surface to be an internal zone (adiabatic) surface, zone=" + Surface( SurfNum ).ZoneName );
					}
					Surface( SurfNum ).ExtBoundCondName = Surface( SurfNum ).Name;
					Surface( SurfNum ).ExtBoundCond = SurfNum;
					SurfError = true;
				}
			}

		} // ...end of the Surface DO loop for finding BaseSurf
		if ( NonMatch ) {
			ShowSevereError( RoutineName + "Non matching interzone surfaces found" );
		}

		//**********************************************************************************
		// Warn about interzone surfaces that have adiabatic windows/vice versa
		SubSurfaceSevereDisplayed = false;
		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
			if ( ! Surface( SurfNum ).HeatTransSurf ) continue;
			if ( Surface( SurfNum ).BaseSurf == SurfNum ) continue; // base surface
			// not base surface.  Check it.
			if ( Surface( Surface( SurfNum ).BaseSurf ).ExtBoundCond <= 0 ) { // exterior or other base surface
				if ( Surface( SurfNum ).ExtBoundCond != Surface( Surface( SurfNum ).BaseSurf ).ExtBoundCond ) { // should match base surface
					if ( Surface( SurfNum ).ExtBoundCond == SurfNum ) {
						ShowSevereError( RoutineName + "Subsurface=\"" + Surface( SurfNum ).Name + "\" exterior condition [adiabatic surface] in a base surface=\"" + Surface( Surface( SurfNum ).BaseSurf ).Name + "\" with exterior condition [" + cExtBoundCondition( Surface( Surface( SurfNum ).BaseSurf ).ExtBoundCond ) + ']' );
						SurfError = true;
					} else if ( Surface( SurfNum ).ExtBoundCond > 0 ) {
						ShowSevereError( RoutineName + "Subsurface=\"" + Surface( SurfNum ).Name + "\" exterior condition [interzone surface] in a base surface=\"" + Surface( Surface( SurfNum ).BaseSurf ).Name + "\" with exterior condition [" + cExtBoundCondition( Surface( Surface( SurfNum ).BaseSurf ).ExtBoundCond ) + ']' );
						SurfError = true;
					} else if ( Surface( Surface( SurfNum ).BaseSurf ).ExtBoundCond == OtherSideCondModeledExt ) {
						ShowWarningError( RoutineName + "Subsurface=\"" + Surface( SurfNum ).Name + "\" exterior condition [" + cExtBoundCondition( Surface( SurfNum ).ExtBoundCond ) + "] in a base surface=\"" + Surface( Surface( SurfNum ).BaseSurf ).Name + "\" with exterior condition [" + cExtBoundCondition( Surface( Surface( SurfNum ).BaseSurf ).ExtBoundCond ) + ']' );
						ShowContinueError( "...SubSurface will not use the exterior condition model of the base surface." );
					} else {
						ShowSevereError( RoutineName + "Subsurface=\"" + Surface( SurfNum ).Name + "\" exterior condition [" + cExtBoundCondition( Surface( SurfNum ).ExtBoundCond ) + "] in a base surface=\"" + Surface( Surface( SurfNum ).BaseSurf ).Name + "\" with exterior condition [" + cExtBoundCondition( Surface( Surface( SurfNum ).BaseSurf ).ExtBoundCond ) + ']' );
						SurfError = true;
					}
					if ( ! SubSurfaceSevereDisplayed && SurfError ) {
						ShowContinueError( "...calculations for heat balance would be compromised." );
						SubSurfaceSevereDisplayed = true;
					}
				}
			} else if ( Surface( Surface( SurfNum ).BaseSurf ).BaseSurf == Surface( Surface( SurfNum ).BaseSurf ).ExtBoundCond ) {
				// adiabatic surface. make sure subsurfaces match
				if ( Surface( SurfNum ).ExtBoundCond != SurfNum ) { // not adiabatic surface
					if ( Surface( SurfNum ).ExtBoundCond > 0 ) {
						ShowSevereError( RoutineName + "Subsurface=\"" + Surface( SurfNum ).Name + "\" exterior condition [interzone surface] in a base surface=\"" + Surface( Surface( SurfNum ).BaseSurf ).Name + "\" with exterior condition [adiabatic surface]" );
					} else {
						ShowSevereError( RoutineName + "Subsurface=\"" + Surface( SurfNum ).Name + "\" exterior condition [" + cExtBoundCondition( Surface( SurfNum ).ExtBoundCond ) + "] in a base surface=\"" + Surface( Surface( SurfNum ).BaseSurf ).Name + "\" with exterior condition [adiabatic surface]" );
					}
					if ( ! SubSurfaceSevereDisplayed ) {
						ShowContinueError( "...calculations for heat balance would be compromised." );
						SubSurfaceSevereDisplayed = true;
					}
					SurfError = true;
				}
			} else if ( Surface( Surface( SurfNum ).BaseSurf ).ExtBoundCond > 0 ) { // interzone surface
				if ( Surface( SurfNum ).ExtBoundCond == SurfNum ) {
					ShowSevereError( RoutineName + "Subsurface=\"" + Surface( SurfNum ).Name + "\" is an adiabatic surface in an Interzone base surface=\"" + Surface( Surface( SurfNum ).BaseSurf ).Name + "\"" );
					if ( ! SubSurfaceSevereDisplayed ) {
						ShowContinueError( "...calculations for heat balance would be compromised." );
						SubSurfaceSevereDisplayed = true;
					}
					//        SurfError=.TRUE.
				}
			}
		}

		//**********************************************************************************
		//   Set up Zone Surface Pointers
		for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
			for ( SurfNum = 1; SurfNum <= MovedSurfs; ++SurfNum ) { //TotSurfaces
				if ( Surface( SurfNum ).Zone == ZoneNum ) {
					if ( Zone( ZoneNum ).SurfaceFirst == 0 ) {
						Zone( ZoneNum ).SurfaceFirst = SurfNum;
						break;
					}
				}
			}
		}
		//  Surface First pointers are set, set last
		if ( NumOfZones > 0 ) {
			Zone( NumOfZones ).SurfaceLast = TotSurfaces;
		}
		for ( ZoneNum = 1; ZoneNum <= NumOfZones - 1; ++ZoneNum ) {
			Zone( ZoneNum ).SurfaceLast = Zone( ZoneNum + 1 ).SurfaceFirst - 1;
		}

		for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
			if ( Zone( ZoneNum ).SurfaceFirst == 0 ) {
				ShowSevereError( RoutineName + "Zone has no surfaces, Zone=" + Zone( ZoneNum ).Name );
				SurfError = true;
			}
		}

		// Set up Floor Areas for Zones
		if ( ! SurfError ) {
			for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
				for ( SurfNum = Zone( ZoneNum ).SurfaceFirst; SurfNum <= Zone( ZoneNum ).SurfaceLast; ++SurfNum ) {
					if ( Surface( SurfNum ).Class == SurfaceClass_Floor ) {
						Zone( ZoneNum ).FloorArea += Surface( SurfNum ).Area;
						Zone( ZoneNum ).HasFloor = true;
					}
					if ( Surface( SurfNum ).Class == SurfaceClass_Roof ) {
						Zone( ZoneNum ).HasRoof = true;
					}
				}
			}
			ErrCount = 0;
			for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
				Zone( ZoneNum ).CalcFloorArea = Zone( ZoneNum ).FloorArea;
				if ( Zone( ZoneNum ).UserEnteredFloorArea != AutoCalculate ) {
					// Check entered vs calculated
					if ( Zone( ZoneNum ).UserEnteredFloorArea > 0.0 ) { // User entered zone floor area,
						// produce message if not near calculated
						if ( Zone( ZoneNum ).CalcFloorArea > 0.0 ) {
							diffp = std::abs( Zone( ZoneNum ).CalcFloorArea - Zone( ZoneNum ).UserEnteredFloorArea ) / Zone( ZoneNum ).UserEnteredFloorArea;
							if ( diffp > 0.05 ) {
								++ErrCount;
								if ( ErrCount == 1 && ! DisplayExtraWarnings ) {
									ShowWarningError( RoutineName + "Entered Zone Floor Areas differ from calculated Zone Floor Area(s)." );
									ShowContinueError( "...use Output:Diagnostics,DisplayExtraWarnings; to show more details on individual zones." );
								}
								if ( DisplayExtraWarnings ) {
									// Warn user of using specified Zone Floor Area
									ShowWarningError( RoutineName + "Entered Floor Area entered for Zone=\"" + Zone( ZoneNum ).Name + "\" significantly different from calculated Floor Area" );
									ShowContinueError( "Entered Zone Floor Area value=" + RoundSigDigits( Zone( ZoneNum ).UserEnteredFloorArea, 2 ) + ", Calculated Zone Floor Area value=" + RoundSigDigits( Zone( ZoneNum ).CalcFloorArea, 2 ) + ", entered Floor Area will be used in calculations." );
								}
							}
						}
						Zone( ZoneNum ).FloorArea = Zone( ZoneNum ).UserEnteredFloorArea;
						Zone( ZoneNum ).HasFloor = true;
					}
				} else {
					Zone( ZoneNum ).FloorArea = Zone( ZoneNum ).CalcFloorArea; // redundant, already done.
				}
			}
		}

		for ( SurfNum = 1; SurfNum <= MovedSurfs; ++SurfNum ) { //TotSurfaces
			if ( Surface( SurfNum ).Area < 1.e-06 ) {
				ShowSevereError( RoutineName + "Zero or negative surface area[" + RoundSigDigits( Surface( SurfNum ).Area, 5 ) + "], Surface=" + Surface( SurfNum ).Name );
				SurfError = true;
			}
			if ( Surface( SurfNum ).Area >= 1.e-06 && Surface( SurfNum ).Area < 0.001 ) {
				ShowWarningError( RoutineName + "Very small surface area[" + RoundSigDigits( Surface( SurfNum ).Area, 5 ) + "], Surface=" + Surface( SurfNum ).Name );
			}
		}

		for ( SurfNum = 1; SurfNum <= MovedSurfs; ++SurfNum ) { //TotSurfaces
			// GLASSDOORs and TDD:DIFFUSERs will be treated as windows in the subsequent heat transfer and daylighting
			// calculations. Reset class to 'Window' after saving the original designation in SurfaceWindow.

			SurfaceWindow( SurfNum ).OriginalClass = Surface( SurfNum ).Class;

			if ( Surface( SurfNum ).Class == SurfaceClass_GlassDoor || Surface( SurfNum ).Class == SurfaceClass_TDD_Diffuser ) Surface( SurfNum ).Class = SurfaceClass_Window;

			if ( Surface( SurfNum ).Class == SurfaceClass_TDD_Dome ) {
				// Reset the TDD:DOME subsurface to act as a base surface that can shade and be shaded
				// NOTE: This must be set early so that subsequent shading calculations are done correctly
				Surface( SurfNum ).BaseSurf = SurfNum;
			}
		}

		errFlag = false;
		if ( ! SurfError ) {
			for ( SurfNum = 1; SurfNum <= MovedSurfs; ++SurfNum ) { //TotSurfaces
				// Set ShadedConstruction numbers for windows whose shaded constructions were created
				// when shading device was specified in the WindowShadingControl for the window
				if ( Surface( SurfNum ).ShadedConstruction != 0 ) SurfaceWindow( SurfNum ).ShadedConstruction = Surface( SurfNum ).ShadedConstruction;

				// no need to set the below -- it is the default
				// Set variable that indicates if shading device has movable slats
				//      SurfaceWindow(SurfNum)%MovableSlats = .FALSE.

				// TH 2/9/2010. Fixed for CR 8010 for speed up purpose rather than fixing the problem
				WinShadingControlPtr = Surface( SurfNum ).WindowShadingControlPtr;
				if ( WinShadingControlPtr != 0 ) {
					if ( WindowShadingControl( WinShadingControlPtr ).SlatAngleControlForBlinds != WSC_SAC_FixedSlatAngle ) SurfaceWindow( SurfNum ).MovableSlats = true;
					// for a constant schedule of slat angle, it acts the same way as fixed angle
					// TH 3/14/2011, CR 8347. Code was commented out due to the use of ExternalInterface (BCVTB)
					//IF(WindowShadingControl(WinShadingControlPtr)%SlatAngleControlForBlinds == WSC_SAC_ScheduledSlatAngle) THEN
					// get schedule index
					//  SchID = WindowShadingControl(WinShadingControlPtr)%SlatAngleSchedule
					//  IF (SchID /= 0 ) THEN
					//    SchSlatAngle = GetScheduleMinValue(SchID)
					//    IF (SchSlatAngle == GetScheduleMaxValue(SchID)) THEN
					//      SurfaceWindow(SurfNum)%MovableSlats = .FALSE.
					//    ENDIF
					//  ENDIF
					//ENDIF
				}

				ConstrNumSh = SurfaceWindow( SurfNum ).ShadedConstruction;
				if ( ConstrNumSh <= 0 ) continue;

				ShadingType = WindowShadingControl( WinShadingControlPtr ).ShadingType;

				// only for blinds
				if ( ShadingType == WSC_ST_ExteriorBlind || ShadingType == WSC_ST_InteriorBlind || ShadingType == WSC_ST_BetweenGlassBlind ) {

					// TH 1/7/2010. CR 7930
					// The old code did not consider between-glass blind. Also there should not be two blinds - both interior and exterior
					// Use the new generic code (assuming only one blind) as follows
					for ( iTmp1 = 1; iTmp1 <= Construct( ConstrNumSh ).TotLayers; ++iTmp1 ) {
						iTmp2 = Construct( ConstrNumSh ).LayerPoint( iTmp1 );
						if ( Material( iTmp2 ).Group == WindowBlind ) {
							BlNum = Material( iTmp2 ).BlindDataPtr;
							SurfaceWindow( SurfNum ).BlindNumber = BlNum;
							// TH 2/18/2010. CR 8010
							// if it is a blind with movable slats, create one new blind and set it to VariableSlat if not done so yet.
							//  the new blind is created only once, it can be shared by multiple windows though.
							if ( SurfaceWindow( SurfNum ).MovableSlats && Blind( BlNum ).SlatAngleType != VariableSlats ) {
								errFlag = false;
								AddVariableSlatBlind( BlNum, BlNumNew, errFlag );
								// point to the new blind
								Material( iTmp2 ).BlindDataPtr = BlNumNew;
								// window surface points to new blind
								SurfaceWindow( SurfNum ).BlindNumber = BlNumNew;
							}
							break;
						}
					}

					if ( errFlag ) {
						ErrorsFound = true;
						ShowContinueError( "WindowProperty:ShadingControl " + WindowShadingControl( WinShadingControlPtr ).Name + " has errors, program will terminate." );
					}

					// TH 5/17/2010. Fixed for CR 8121. Overwrite the blind slat angle with the constant scheduled value
					// TH 3/14/2011. With fix for CR 8347, the following code is no longer needed.
					//IF (SurfaceWindow(SurfNum)%BlindNumber >0 .AND. WinShadingControlPtr >0 ) THEN
					//  IF (.NOT. SurfaceWindow(SurfNum)%MovableSlats .AND. &
					//    WindowShadingControl(WinShadingControlPtr)%SlatAngleControlForBlinds == WSC_SAC_ScheduledSlatAngle) THEN
					//    Blind(SurfaceWindow(SurfNum)%BlindNumber)%SlatAngle = SchSlatAngle
					//  ENDIF
					//ENDIF

				}

			} // End of surface loop

			// Warning if a WindowShadingControl is not referenced by any window; user may think
			// window shading is occurring when it really isn't
			for ( ShadingCtrl = 1; ShadingCtrl <= TotWinShadingControl; ++ShadingCtrl ) {
				WinShadingCtrlReferenced = false;
				for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
					if ( Surface( SurfNum ).WindowShadingControlPtr == ShadingCtrl ) WinShadingCtrlReferenced = true;
				}
				if ( ! WinShadingCtrlReferenced ) {
					ShowWarningError( RoutineName + "WindowProperty:ShadingControl: \"" + WindowShadingControl( ShadingCtrl ).Name + "\" is not referenced by any window." );
				}
			}
		}

		// Check for zones with not enough surfaces
		for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
			OpaqueHTSurfs = 0;
			OpaqueHTSurfsWithWin = 0;
			InternalMassSurfs = 0;
			if ( Zone( ZoneNum ).SurfaceFirst == 0 ) continue; // Zone with no surfaces
			for ( SurfNum = Zone( ZoneNum ).SurfaceFirst; SurfNum <= Zone( ZoneNum ).SurfaceLast; ++SurfNum ) {
				if ( Surface( SurfNum ).Class == SurfaceClass_Floor || Surface( SurfNum ).Class == SurfaceClass_Wall || Surface( SurfNum ).Class == SurfaceClass_Roof ) ++OpaqueHTSurfs;
				if ( Surface( SurfNum ).Class == SurfaceClass_IntMass ) ++InternalMassSurfs;
				if ( Surface( SurfNum ).Class == SurfaceClass_Window ) {
					// Count base surface only once for multiple windows on a wall
					if ( SurfNum > 1 && Surface( SurfNum - 1 ).Class != SurfaceClass_Window ) ++OpaqueHTSurfsWithWin;
				}
			}
			if ( OpaqueHTSurfsWithWin == 1 && OpaqueHTSurfs == 1 && InternalMassSurfs == 0 ) {
				SurfError = true;
				ShowSevereError( RoutineName + "Zone " + Zone( ZoneNum ).Name + " has only one floor, wall or roof, and this surface has a window." );
				ShowContinueError( "Add more floors, walls or roofs, or an internal mass surface." );
			}
			if ( ( OpaqueHTSurfs + InternalMassSurfs ) < 6 ) {
				ShowWarningError( RoutineName + "The total number of floors, walls, roofs and internal mass surfaces in Zone " + Zone( ZoneNum ).Name );
				ShowContinueError( "is < 6. This may cause an inaccurate zone heat balance calculation." );
			}
		}

		// set up vertex of centroid for each surface.
		CalcSurfaceCentroid();

		SetupShadeSurfacesForSolarCalcs(); // if shading surfaces are solar collectors or PV, then we need full solar calc.

		LayNumOutside = 0;
		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
			// Check for EcoRoof and only 1 allowed to be used.
			if ( ! Surface( SurfNum ).ExtEcoRoof ) continue;
			if ( LayNumOutside == 0 ) {
				LayNumOutside = Construct( Surface( SurfNum ).Construction ).LayerPoint( 1 );
				continue;
			}
			if ( LayNumOutside != Construct( Surface( SurfNum ).Construction ).LayerPoint( 1 ) ) {
				ShowSevereError( RoutineName + "Only one EcoRoof Material is currently allowed for all constructions." );
				ShowContinueError( "... first material=" + Material( LayNumOutside ).Name );
				ShowContinueError( "... conflicting Construction=" + Construct( Surface( SurfNum ).Construction ).Name + " uses material=" + Material( Construct( Surface( SurfNum ).Construction ).LayerPoint( 1 ) ).Name );
				ErrorsFound = true;
			}
		}

		// Set flag that determines whether a surface can be an exterior obstruction
		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
			Surface( SurfNum ).ShadowSurfPossibleObstruction = false;
			// Exclude non-exterior heat transfer surfaces (but not OtherSideCondModeledExt = -4 CR7640)
			if ( Surface( SurfNum ).HeatTransSurf && Surface( SurfNum ).ExtBoundCond > 0 ) continue;
			if ( Surface( SurfNum ).HeatTransSurf && Surface( SurfNum ).ExtBoundCond == Ground ) continue;
			if ( Surface( SurfNum ).HeatTransSurf && Surface( SurfNum ).ExtBoundCond == OtherSideCoefNoCalcExt ) continue;
			if ( Surface( SurfNum ).HeatTransSurf && Surface( SurfNum ).ExtBoundCond == OtherSideCoefCalcExt ) continue;
			// Exclude windows and doors, i.e., consider only their base surfaces as possible obstructions
			if ( Surface( SurfNum ).Class == SurfaceClass_Window || Surface( SurfNum ).Class == SurfaceClass_Door ) continue;
			// Exclude duplicate shading surfaces
			// TH 3/25/2010 CR 7872
			//  Shading surface names can start with Mir, a better way to use another flag
			//   to indicate whether a surface is a mirrored one.
			//IF(Surface(SurfNum)%Name(1:3) == 'Mir') CYCLE
			if ( Surface( SurfNum ).MirroredSurf ) continue;

			Surface( SurfNum ).ShadowSurfPossibleObstruction = true;
		}

		// Check for IRT surfaces in invalid places.
		iTmp1 = 0;
		if ( std::any_of( Construct.begin(), Construct.end(), []( DataHeatBalance::ConstructionData const & e ){ return e.TypeIsIRT; } ) ) {
			for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
				if ( ! Surface( SurfNum ).HeatTransSurf ) continue; // ignore shading surfaces
				if ( Surface( SurfNum ).ExtBoundCond > 0 && Surface( SurfNum ).ExtBoundCond != SurfNum ) continue; // interzone, not adiabatic surface
				if ( ! Construct( Surface( SurfNum ).Construction ).TypeIsIRT ) continue;
				if ( ! DisplayExtraWarnings ) {
					++iTmp1;
				} else {
					ShowWarningError( RoutineName + "Surface=\"" + Surface( SurfNum ).Name + "\" uses InfraredTransparent construction in a non-interzone surface. (illegal use)" );
				}
			}
			if ( iTmp1 > 0 ) {
				ShowWarningError( RoutineName + "Surfaces use InfraredTransparent constructions " + TrimSigDigits( iTmp1 ) + " in non-interzone surfaces. (illegal use)" );
				ShowContinueError( "For explicit details on each use, use Output:Diagnostics,DisplayExtraWarnings;" );
			}
		}

		// Note, could do same for Window Area and detecting if Interzone Surface in Zone

		if ( Warning1Count > 0 ) {
			ShowWarningMessage( RoutineName + "Window dimensions differ from Window 5/6 data file dimensions, " + TrimSigDigits( Warning1Count ) + " times." );
			ShowContinueError( "This will affect the frame heat transfer calculation if the frame in the Data File entry" );
			ShowContinueError( "is not uniform, i.e., has sections with different geometry and/or thermal properties." );
			ShowContinueError( "For explicit details on each window, use Output:Diagnostics,DisplayExtraWarnings;" );
		}
		if ( Warning2Count > 0 ) {
			ShowWarningMessage( RoutineName + "Exterior Windows have been replaced with Window 5/6 two glazing systems, " + TrimSigDigits( Warning2Count ) + " times." );
			ShowContinueError( "Note that originally entered dimensions are overridden." );
			ShowContinueError( "For explicit details on each window, use Output:Diagnostics,DisplayExtraWarnings;" );
		}
		if ( Warning3Count > 0 ) {
			ShowWarningMessage( RoutineName + "Interior Windows have been replaced with Window 5/6 two glazing systems, " + TrimSigDigits( Warning3Count ) + " times." );
			ShowContinueError( "Note that originally entered dimensions are overridden." );
			ShowContinueError( "For explicit details on each window, use Output:Diagnostics,DisplayExtraWarnings;" );
		}

		if ( TotalMultipliedWindows > 0 ) {
			ShowWarningMessage( RoutineName + "There are " + TrimSigDigits( TotalMultipliedWindows ) + " window/glass door(s) that may cause inaccurate shadowing due to Solar Distribution." );
			ShowContinueError( "For explicit details on each window, use Output:Diagnostics,DisplayExtraWarnings;" );
			TotalWarningErrors += TotalMultipliedWindows;
		}
		if ( TotalCoincidentVertices > 0 ) {
			ShowWarningMessage( RoutineName + "There are " + TrimSigDigits( TotalCoincidentVertices ) + " coincident/collinear vertices; These have been deleted unless the deletion would bring the number of surface sides < 3." );
			ShowContinueError( "For explicit details on each problem surface, use Output:Diagnostics,DisplayExtraWarnings;" );
			TotalWarningErrors += TotalCoincidentVertices;
		}
		if ( TotalDegenerateSurfaces > 0 ) {
			ShowSevereMessage( RoutineName + "There are " + TrimSigDigits( TotalDegenerateSurfaces ) + " degenerate surfaces; Degenerate surfaces are those with number of sides < 3." );
			ShowContinueError( "These surfaces should be deleted." );
			ShowContinueError( "For explicit details on each problem surface, use Output:Diagnostics,DisplayExtraWarnings;" );
			TotalSevereErrors += TotalDegenerateSurfaces;
		}

		GetHTSurfExtVentedCavityData( ErrorsFound );

		GetSurfaceHeatTransferAlgorithmOverrides( ErrorsFound );

		if ( SurfError || ErrorsFound ) {
			ErrorsFound = true;
			ShowFatalError( RoutineName + "Errors discovered, program terminates." );
		}

		int TotShadSurf = TotDetachedFixed + TotDetachedBldg + TotRectDetachedFixed + TotRectDetachedBldg + TotShdSubs + TotOverhangs + TotOverhangsProjection + TotFins + TotFinsProjection;
		int NumDElightControls = GetNumObjectsFound( "Daylighting:DElight:Controls" );
		int NumDElightRefPt = GetNumObjectsFound( "Daylighting:DElight:ReferencePoint" );
		int NumDElightCmplxFen = GetNumObjectsFound( "Daylighting:DElight:ComplexFenestration" );
		int TotDElightObj = NumDElightControls + NumDElightRefPt + NumDElightCmplxFen;
		if ( TotShadSurf > 0 && TotDElightObj > 0 ){
			ShowWarningError( RoutineName + "When using DElight daylighting the presence of exterior shading surfaces is ignored." );
		}
	}

	void
	checkSubSurfAzTiltNorm(
		SurfaceData & baseSurface, // Base surface data (in)
		SurfaceData & subSurface, // Subsurface data (in)
		bool & surfaceError // True if surface azimuths or tilts differ by more than error tolerance
	)
	{
		bool sameSurfNormal( false ); // True if surface has the same surface normal within tolerance
		bool baseSurfHoriz( false ); // True if base surface is near horizontal
		Real64 const warningTolerance( 30.0 );
		Real64 const errorTolerance( 90.0 );

		surfaceError = false;

		// Check if base surface and subsurface have the same normal
		Vectors::CompareTwoVectors( baseSurface.NewellSurfaceNormalVector, subSurface.NewellSurfaceNormalVector, sameSurfNormal, 0.001 );
		if ( sameSurfNormal ) { // copy lcs vectors
			// Prior logic tested for azimuth difference < 30 and then skipped this - this caused large diffs in CmplxGlz_MeasuredDeflectionAndShading
			// Restoring that check here but will require further investigation (MJW Dec 2015)
			if ( std::abs( baseSurface.Azimuth - subSurface.Azimuth ) > warningTolerance ) {
				subSurface.lcsx = baseSurface.lcsx;
				subSurface.lcsy = baseSurface.lcsy;
				subSurface.lcsy = baseSurface.lcsy;
			}
		} else {
			// Not sure what this does, but keeping for now (MJW Dec 2015)
			if ( std::abs( subSurface.Azimuth - 360.0 ) < 0.01 ) {
				subSurface.Azimuth = 360.0 - subSurface.Azimuth;
			}
			if ( std::abs( baseSurface.Azimuth - 360.0 ) < 0.01 ) {
				baseSurface.Azimuth = 360.0 - baseSurface.Azimuth;
			}

			// Is base surface horizontal? If so, ignore azimuth differences
			if ( std::abs( baseSurface.Tilt ) <= 1.0e-5 || std::abs( baseSurface.Tilt - 180.0 ) <= 1.0e-5 ) baseSurfHoriz = true;

			if ( ( ( std::abs( baseSurface.Azimuth - subSurface.Azimuth ) > errorTolerance ) && ! baseSurfHoriz ) || ( std::abs( baseSurface.Tilt - subSurface.Tilt ) > errorTolerance ) ) {
				surfaceError = true;
				ShowSevereError( "checkSubSurfAzTiltNorm: Outward facing angle of subsurface differs more than " + General::RoundSigDigits( errorTolerance, 1 ) + " degrees from base surface." );
				ShowContinueError( "Subsurface=\"" + subSurface.Name + "\" Tilt = " + General::RoundSigDigits( subSurface.Tilt, 1 ) + "  Azimuth = " + General::RoundSigDigits( subSurface.Azimuth, 1 ) );
				ShowContinueError( "Base surface=\"" + baseSurface.Name + "\" Tilt = " + General::RoundSigDigits( baseSurface.Tilt, 1 ) + "  Azimuth = " + General::RoundSigDigits( baseSurface.Azimuth, 1 ) );
			} else if ( ( ( std::abs( baseSurface.Azimuth - subSurface.Azimuth ) > warningTolerance ) && !baseSurfHoriz ) || ( std::abs( baseSurface.Tilt - subSurface.Tilt ) > warningTolerance ) ) {
				++checkSubSurfAzTiltNormErrCount;
				if ( checkSubSurfAzTiltNormErrCount == 1 && !DisplayExtraWarnings ) {
					ShowWarningError( "checkSubSurfAzTiltNorm: Some Outward Facing angles of subsurfaces differ more than " + General::RoundSigDigits( warningTolerance, 1 ) + " degrees from base surface." );
					ShowContinueError( "...use Output:Diagnostics,DisplayExtraWarnings; to show more details on individual surfaces." );
				}
				if ( DisplayExtraWarnings ) {
					ShowWarningError( "checkSubSurfAzTiltNorm: Outward facing angle of subsurface differs more than " + General::RoundSigDigits( warningTolerance, 1 ) + " degrees from base surface." );
					ShowContinueError( "Subsurface=\"" + subSurface.Name + "\" Tilt = " + General::RoundSigDigits( subSurface.Tilt, 1 ) + "  Azimuth = " + General::RoundSigDigits( subSurface.Azimuth, 1 ) );
					ShowContinueError( "Base surface=\"" + baseSurface.Name + "\" Tilt = " + General::RoundSigDigits( baseSurface.Tilt, 1 ) + "  Azimuth = " + General::RoundSigDigits( baseSurface.Azimuth, 1 ) );
				}
			}
		}
	}

	void
	GetGeometryParameters( bool & ErrorsFound ) // set to true if errors found during input
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   May 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine reads in the "Surface Geometry" parameters, verifies them,
		// and sets "global" variables that will tell other routines how the surface
		// vertices are expected in input.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// GlobalGeometryRules Definition
		//GlobalGeometryRules,
		//      \required-object
		//      \unique-object
		//  A1, \field Starting Vertex Position
		//      \required-field
		//      \note Specified as entry for a 4 sided surface/rectangle
		//      \note Surfaces are specified as viewed from outside the surface
		//      \note Shading surfaces as viewed from behind.  (towards what they are shading)
		//      \type choice
		//      \key UpperLeftCorner
		//      \key LowerLeftCorner
		//      \key UpperRightCorner
		//      \key LowerRightCorner
		//  A2, \field Vertex Entry Direction
		//      \required-field
		//      \type choice
		//      \key Counterclockwise
		//      \key Clockwise
		//  A3, \field Coordinate System
		//      \required-field
		//      \note relative -- coordinates are entered relative to zone origin
		//      \note world -- all coordinates entered are "absolute" for this facility
		//      \note absolute -- same as world
		//      \type choice
		//      \key Relative
		//      \key World
		//      \key Absolute
		//  A4, \field Daylighting Reference Point Coordinate System
		//      \type choice
		//      \key Relative
		//      \default Relative
		//      \note Relative -- coordinates are entered relative to zone origin
		//      \key World
		//      \note World -- all coordinates entered are "absolute" for this facility
		//      \key Absolute
		//      \note absolute -- same as world
		//  A5; \field Rectangular Surface Coordinate System
		//      \type choice
		//      \key Relative
		//      \default Relative
		//      \note Relative -- Starting corner is entered relative to zone origin
		//      \key World
		//      \note World -- Starting corner is entered in "absolute"
		//      \key Absolute
		//      \note absolute -- same as world

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItem;
		using InputProcessor::SameString;
		using InputProcessor::VerifyName;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static Array1D_string const AbCorners( 4, { "ULC", "LLC", "LRC", "URC" } );
		static Array1D_string const FlCorners( 4, { "UpperLeftCorner", "LowerLeftCorner", "LowerRightCorner", "UpperRightCorner" } );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumStmt;
		Array1D_string GAlphas( 5 );
		int NAlphas;
		Array1D< Real64 > GNum( 1 );
		int NNum;
		int IOStat;
		bool OK;
		int Found;
		std::string OutMsg;

		// Formats
		static gio::Fmt Format_720( "(A)" );

		cCurrentModuleObject = "GlobalGeometryRules";
		NumStmt = GetNumObjectsFound( cCurrentModuleObject );
		OutMsg = " Surface Geometry,";

		{ auto const SELECT_CASE_var( NumStmt );

		if ( SELECT_CASE_var == 1 ) {
			// This is the valid case
			GetObjectItem( cCurrentModuleObject, 1, GAlphas, NAlphas, GNum, NNum, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			// Even though these will be validated, set defaults in case error here -- wont
			// cause aborts in later surface gets (hopefully)
			Corner = UpperLeftCorner;
			WorldCoordSystem = true;
			CCW = true;

			OK = false;
			Found = FindItem( GAlphas( 1 ), AbCorners, 4 );
			if ( Found == 0 ) {
				Found = FindItem( GAlphas( 1 ), FlCorners, 4 );
				if ( Found == 0 ) {
					ShowSevereError( cCurrentModuleObject + ": Invalid " + cAlphaFieldNames( 1 ) + '=' + GAlphas( 1 ) );
					ErrorsFound = true;
				} else {
					Corner = Found;
					OK = true;
					OutMsg += FlCorners( Corner ) + ',';
				}
			} else {
				Corner = Found;
				OutMsg += FlCorners( Corner ) + ',';
				OK = true;
			}

			OK = false;
			if ( SameString( GAlphas( 2 ), "CCW" ) || SameString( GAlphas( 2 ), "Counterclockwise" ) ) {
				CCW = true;
				OutMsg += "Counterclockwise,";
				OK = true;
			}
			if ( SameString( GAlphas( 2 ), "CW" ) || SameString( GAlphas( 2 ), "Clockwise" ) ) {
				CCW = false;
				OutMsg += "Clockwise,";
				OK = true;
			}
			if ( ! OK ) {
				ShowSevereError( cCurrentModuleObject + ": Invalid " + cAlphaFieldNames( 2 ) + '=' + GAlphas( 2 ) );
				ErrorsFound = true;
			}

			OK = false;
			if ( SameString( GAlphas( 3 ), "WCS" ) || SameString( GAlphas( 3 ), "WorldCoordinateSystem" ) || SameString( GAlphas( 3 ), "World" ) || SameString( GAlphas( 3 ), "Absolute" ) ) {
				WorldCoordSystem = true;
				OutMsg += "WorldCoordinateSystem,";
				OK = true;
			}
			if ( has_prefixi( GAlphas( 3 ), "Rel" ) || has_prefixi( GAlphas( 3 ), "Relative" ) || SameString( GAlphas( 3 ), "Local" ) ) {
				WorldCoordSystem = false;
				OutMsg += "RelativeCoordinateSystem,";
				OK = true;
			}
			if ( ! OK ) {
				ShowWarningError( cCurrentModuleObject + ": Invalid " + cAlphaFieldNames( 3 ) + '=' + GAlphas( 3 ) );
				ShowContinueError( cAlphaFieldNames( 3 ) + " defaults to \"WorldCoordinateSystem\"" );
				WorldCoordSystem = true;
				OutMsg += "WorldCoordinateSystem,";
			}

			OK = false;
			if ( SameString( GAlphas( 4 ), "WCS" ) || SameString( GAlphas( 4 ), "WorldCoordinateSystem" ) || SameString( GAlphas( 4 ), "World" ) || SameString( GAlphas( 4 ), "Absolute" ) ) {
				DaylRefWorldCoordSystem = true;
				OutMsg += "WorldCoordinateSystem,";
				OK = true;
			}
			if ( has_prefixi( GAlphas( 4 ), "Rel" ) || has_prefixi( GAlphas( 4 ), "Relative" ) || SameString( GAlphas( 4 ), "Local" ) || GAlphas( 4 ).empty() ) {
				DaylRefWorldCoordSystem = false;
				OutMsg += "RelativeCoordinateSystem,";
				OK = true;
			}
			if ( ! OK ) {
				ShowWarningError( cCurrentModuleObject + ": Invalid " + cAlphaFieldNames( 4 ) + '=' + GAlphas( 4 ) );
				ShowContinueError( cAlphaFieldNames( 4 ) + " defaults to \"RelativeToZoneOrigin\"" );
				DaylRefWorldCoordSystem = false;
				OutMsg += "RelativeToZoneOrigin,";
			}

			OK = false;
			if ( SameString( GAlphas( 5 ), "WCS" ) || SameString( GAlphas( 5 ), "WorldCoordinateSystem" ) || SameString( GAlphas( 5 ), "World" ) || SameString( GAlphas( 5 ), "Absolute" ) ) {
				RectSurfRefWorldCoordSystem = true;
				OutMsg += "WorldCoordinateSystem";
				OK = true;
			}
			if ( has_prefixi( GAlphas( 5 ), "Rel" ) || has_prefixi( GAlphas( 5 ), "Relative" ) || SameString( GAlphas( 5 ), "Local" ) || GAlphas( 5 ).empty() ) {
				RectSurfRefWorldCoordSystem = false;
				OutMsg += "RelativeToZoneOrigin";
				OK = true;
			}
			if ( ! OK ) {
				ShowWarningError( cCurrentModuleObject + ": Invalid " + cAlphaFieldNames( 5 ) + '=' + GAlphas( 5 ) );
				ShowContinueError( cAlphaFieldNames( 5 ) + " defaults to \"RelativeToZoneOrigin\"" );
				RectSurfRefWorldCoordSystem = false;
				OutMsg += "RelativeToZoneOrigin";
			}

		} else if ( SELECT_CASE_var == 0 ) {

			ShowSevereError( cCurrentModuleObject + ": Required object not found." );
			OutMsg += "None found in input";
			ErrorsFound = true;

		} else {

			ShowSevereError( cCurrentModuleObject + ": Too many objects entered.  Only one allowed." );
			ErrorsFound = true;

		}}

		if ( ! WorldCoordSystem ) {
			if ( DaylRefWorldCoordSystem ) {
				ShowWarningError( cCurrentModuleObject + ": Potential mismatch of coordinate specifications." );
				ShowContinueError( cAlphaFieldNames( 3 ) + "=\"" + GAlphas( 3 ) + "\"; while " );
				ShowContinueError( cAlphaFieldNames( 4 ) + "=\"" + GAlphas( 4 ) + "\"." );
			}
			if ( RectSurfRefWorldCoordSystem ) {
				ShowWarningError( cCurrentModuleObject + ": Potential mismatch of coordinate specifications." );
				ShowContinueError( cAlphaFieldNames( 3 ) + "=\"" + GAlphas( 3 ) + "\"; while " );
				ShowContinueError( cAlphaFieldNames( 5 ) + "=\"" + GAlphas( 5 ) + "\"." );
			}
		}

		gio::write( OutputFileInits, Format_720 ) << "! <SurfaceGeometry>,Starting Corner,Vertex Input Direction,Coordinate System,Daylight Reference Point Coordinate System,Rectangular (Simple) Surface Coordinate System";
		gio::write( OutputFileInits, Format_720 ) << OutMsg;

	}

	void
	GetDetShdSurfaceData(
		bool & ErrorsFound, // Error flag indicator (true if errors found)
		int & SurfNum, // Count of Current SurfaceNumber
		int const TotDetachedFixed, // Number of Fixed Detached Shading Surfaces to obtain
		int const TotDetachedBldg // Number of Building Detached Shading Surfaces to obtain
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   May 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine gets the Detached Shading Surface Data,
		// checks it for errors, etc.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// Detached Shading Surface Definition(s)
		//Surface:Shading:Detached:Fixed,
		//       \memo used for shading elements such as trees
		//       \memo these items are fixed in space and would not move with relative geometry
		//  A1 , \field User Supplied Surface Name
		//       \required-field
		//       \type alpha
		//  A2,  \field TransSchedShadowSurf
		//       \note Transmittance schedule for the shading device, defaults to zero (always opaque)
		//       \type object-list
		//       \object-list ScheduleNames
		//  N1 , \field Number of Surface Vertex Groups -- Number of (X,Y,Z) groups in this surface
		//       \required-field
		//       \note shown with 12 vertex coordinates -- extensible object
		//       \autocalculatable
		//       \default autocalculate
		//       \minimum 3
		//       \note Rules for vertices are given in SurfaceGeometry coordinates --
		//       \note For this object all surface coordinates are relative to the building origin (0,0,0)
		//       \note and will rotate with the BUILDING north axis.
		//  N2,  \field Vertex 1 X-coordinate
		//       \units m
		//       \type real
		//  N3-37; as indicated by the N1 value
		//Surface:Shading:Detached:Building,
		//       \memo used for shading elements such as trees, other buildings, parts of this building not being modeled
		//       \memo these items are relative to the current building and would move with relative geometry
		//  A1 , \field User Supplied Surface Name
		//       \required-field
		//       \type alpha
		//  A2,  \field TransSchedShadowSurf
		//       \note Transmittance schedule for the shading device, defaults to zero (always opaque)
		//       \type object-list
		//       \object-list ScheduleNames
		//  N1 , \field Number of Surface Vertex Groups -- Number of (X,Y,Z) groups in this surface
		//       \required-field
		//       \note shown with 12 vertex coordinates -- extensible object
		//       \autocalculatable
		//       \default autocalculate
		//       \minimum 3
		//       \note Rules for vertices are given in SurfaceGeometry coordinates --
		//       \note For this object all surface coordinates are relative to the building origin (0,0,0)
		//       \note and will rotate with the BUILDING north axis.
		//  N2,  \field Vertex 1 X-coordinate
		//       \units m
		//       \type real
		//  N3-37; as indicated by the N1 value

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using namespace DataReportingFlags;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::GetObjectDefMaxArgs;
		using ScheduleManager::GetScheduleIndex;
		using ScheduleManager::CheckScheduleValueMinMax;
		using ScheduleManager::GetScheduleMinValue;
		using ScheduleManager::GetScheduleMaxValue;
		using General::TrimSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static Array1D_string const cModuleObjects( 2, { "Shading:Site:Detailed", "Shading:Building:Detailed" } );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int IOStat; // IO Status when calling get input subroutine
		int NumAlphas; // Number of material alpha names being passed
		int NumNumbers; // Number of material properties being passed
		int Loop;
		bool ErrorInName;
		bool IsBlank;
		int Item;
		int ItemsToGet;
		int ClassItem;
		int numSides;
		Real64 SchedMinValue;
		Real64 SchedMaxValue;

		if ( ( TotDetachedFixed + TotDetachedBldg ) > 0 && SolarDistribution == MinimalShadowing ) {
			ShowWarningError( "Detached shading effects are ignored when Solar Distribution = MinimalShadowing" );
		}

		if ( ( TotDetachedFixed + TotDetachedBldg ) == 0 ) return;

		for ( Item = 1; Item <= 2; ++Item ) {

			cCurrentModuleObject = cModuleObjects( Item );
			if ( Item == 1 ) {
				ItemsToGet = TotDetachedFixed;
				ClassItem = SurfaceClass_Detached_F;
			} else { //IF (Item == 2) THEN
				ItemsToGet = TotDetachedBldg;
				ClassItem = SurfaceClass_Detached_B;
			}

			GetObjectDefMaxArgs( cCurrentModuleObject, Loop, NumAlphas, NumNumbers );
			if ( NumAlphas != 2 ) {
				ShowSevereError( cCurrentModuleObject + ": Object Definition indicates not = 2 Alpha Objects, Number Indicated=" + TrimSigDigits( NumAlphas ) );
				ErrorsFound = true;
			}

			for ( Loop = 1; Loop <= ItemsToGet; ++Loop ) {
				GetObjectItem( cCurrentModuleObject, Loop, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
				ErrorInName = false;
				IsBlank = false;
				VerifyName( cAlphaArgs( 1 ), SurfaceTmp, SurfNum, ErrorInName, IsBlank, cCurrentModuleObject + " Name" );
				if ( ErrorInName ) {
					ShowContinueError( "...each surface name must not duplicate other surface names (of any type)" );
					ErrorsFound = true;
					continue;
				}

				++SurfNum;
				SurfaceTmp( SurfNum ).Name = cAlphaArgs( 1 ); // Set the Surface Name in the Derived Type
				SurfaceTmp( SurfNum ).Class = ClassItem;
				SurfaceTmp( SurfNum ).HeatTransSurf = false;
				// Base transmittance of a shadowing (sub)surface
				if ( ! lAlphaFieldBlanks( 2 ) ) {
					// Schedule for a shadowing (sub)surface
					SurfaceTmp( SurfNum ).SchedShadowSurfIndex = GetScheduleIndex( cAlphaArgs( 2 ) );
					if ( SurfaceTmp( SurfNum ).SchedShadowSurfIndex == 0 ) {
						ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", " + cAlphaFieldNames( 2 ) + " not found=" + cAlphaArgs( 2 ) );
						ErrorsFound = true;
					}
				} else {
					SurfaceTmp( SurfNum ).SchedShadowSurfIndex = 0;
				}
				if ( SurfaceTmp( SurfNum ).SchedShadowSurfIndex != 0 ) {
					if ( ! CheckScheduleValueMinMax( SurfaceTmp( SurfNum ).SchedShadowSurfIndex, ">=", 0.0, "<=", 1.0 ) ) {
						ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\", values not in range [0,1]." );
						ErrorsFound = true;
					}
					SchedMinValue = GetScheduleMinValue( SurfaceTmp( SurfNum ).SchedShadowSurfIndex );
					SurfaceTmp( SurfNum ).SchedMinValue = SchedMinValue;
					SchedMaxValue = GetScheduleMaxValue( SurfaceTmp( SurfNum ).SchedShadowSurfIndex );
					if ( SchedMinValue == 1.0 ) {
						ShowWarningError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\", is always transparent." );
						SurfaceTmp( SurfNum ).IsTransparent = true;
					}
					if ( SchedMinValue < 0.0 ) {
						ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\", has schedule values < 0." );
						ShowContinueError( "...Schedule values < 0 have no meaning for shading elements." );
					}
					if ( SchedMaxValue > 1.0 ) {
						ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\", has schedule values > 1." );
						ShowContinueError( "...Schedule values > 1 have no meaning for shading elements." );
					}
					if ( std::abs( SchedMinValue - SchedMaxValue ) > 1.0e-6 ) {
						SurfaceTmp( SurfNum ).ShadowSurfSchedVaries = true;
						ShadingTransmittanceVaries = true;
					}
				}
				if ( lNumericFieldBlanks( 1 ) || rNumericArgs( 1 ) == AutoCalculate ) {
					numSides = ( NumNumbers - 1 ) / 3;
					SurfaceTmp( SurfNum ).Sides = numSides;
					if ( mod( NumNumbers - 1, 3 ) != 0 ) {
						ShowWarningError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", " + cNumericFieldNames( 1 ) + " not even multiple of 3. Will read in " + TrimSigDigits( SurfaceTmp( SurfNum ).Sides ) );
					}
					if ( numSides < 3 ) {
						ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", " + cNumericFieldNames( 1 ) + " (autocalculate) must be >= 3. Only " + TrimSigDigits( SurfaceTmp( SurfNum ).Sides ) + " provided." );
						ErrorsFound = true;
						continue;
					}
				} else {
					numSides = ( NumNumbers - 1 ) / 3;
					SurfaceTmp( SurfNum ).Sides = rNumericArgs( 1 );
					if ( numSides > SurfaceTmp( SurfNum ).Sides ) {
						ShowWarningError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", field " + cNumericFieldNames( 1 ) + '=' + TrimSigDigits( SurfaceTmp( SurfNum ).Sides ) );
						ShowContinueError( "...but " + TrimSigDigits( numSides ) + " were entered. Only the indicated " + cNumericFieldNames( 1 ) + " will be used." );
					}
				}
				SurfaceTmp( SurfNum ).Vertex.allocate( SurfaceTmp( SurfNum ).Sides );
				GetVertices( SurfNum, SurfaceTmp( SurfNum ).Sides, rNumericArgs( {2,_} ) );
				CheckConvexity( SurfNum, SurfaceTmp( SurfNum ).Sides );
				if ( MakeMirroredDetachedShading ) {
					MakeMirrorSurface( SurfNum );
				}
			}

		} // Item Loop

	}

	void
	GetRectDetShdSurfaceData(
		bool & ErrorsFound, // Error flag indicator (true if errors found)
		int & SurfNum, // Count of Current SurfaceNumber
		int const TotRectDetachedFixed, // Number of Fixed Detached Shading Surfaces to obtain
		int const TotRectDetachedBldg // Number of Building Detached Shading Surfaces to obtain
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   January 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Gets the simple, rectantular detached surfaces.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using namespace DataReportingFlags;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::GetObjectDefMaxArgs;
		using General::TrimSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static Array1D_string const cModuleObjects( 2, { "Shading:Site", "Shading:Building" } );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int IOStat; // IO Status when calling get input subroutine
		int NumAlphas; // Number of material alpha names being passed
		int NumNumbers; // Number of material properties being passed
		int Loop;
		bool ErrorInName;
		bool IsBlank;
		int Item;
		int ItemsToGet;
		int ClassItem;

		if ( ( TotRectDetachedFixed + TotRectDetachedBldg ) > 0 && SolarDistribution == MinimalShadowing ) {
			ShowWarningError( "Detached shading effects are ignored when Solar Distribution = MinimalShadowing" );
		}

		if ( TotRectDetachedFixed + TotRectDetachedBldg == 0 ) return;

		for ( Item = 1; Item <= 2; ++Item ) {

			cCurrentModuleObject = cModuleObjects( Item );
			if ( Item == 1 ) {
				ItemsToGet = TotRectDetachedFixed;
				ClassItem = SurfaceClass_Detached_F;
			} else { //IF (Item == 2) THEN
				ItemsToGet = TotRectDetachedBldg;
				ClassItem = SurfaceClass_Detached_B;
			}

			GetObjectDefMaxArgs( cCurrentModuleObject, Loop, NumAlphas, NumNumbers );
			if ( NumAlphas != 1 ) {
				ShowSevereError( cCurrentModuleObject + ": Object Definition indicates not = 1 Alpha Objects, Number Indicated=" + TrimSigDigits( NumAlphas ) );
				ErrorsFound = true;
			}

			for ( Loop = 1; Loop <= ItemsToGet; ++Loop ) {
				GetObjectItem( cCurrentModuleObject, Loop, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
				ErrorInName = false;
				IsBlank = false;
				VerifyName( cAlphaArgs( 1 ), SurfaceTmp, SurfNum, ErrorInName, IsBlank, cCurrentModuleObject + " Name" );
				if ( ErrorInName ) {
					ShowContinueError( "...each surface name must not duplicate other surface names (of any type)" );
					ErrorsFound = true;
					continue;
				}

				++SurfNum;
				SurfaceTmp( SurfNum ).Name = cAlphaArgs( 1 ); // Set the Surface Name in the Derived Type
				SurfaceTmp( SurfNum ).Class = ClassItem;
				SurfaceTmp( SurfNum ).HeatTransSurf = false;

				SurfaceTmp( SurfNum ).Azimuth = rNumericArgs( 1 );
				if ( SurfaceTmp( SurfNum ).Class == SurfaceClass_Detached_B && ! WorldCoordSystem ) {
					SurfaceTmp( SurfNum ).Azimuth += BuildingAzimuth;
				}
				if ( SurfaceTmp( SurfNum ).Class == SurfaceClass_Detached_B ) {
					SurfaceTmp( SurfNum ).Azimuth += BuildingRotationAppendixG;
				}
				SurfaceTmp( SurfNum ).Tilt = rNumericArgs( 2 );

				SurfaceTmp( SurfNum ).Sides = 4;
				SurfaceTmp( SurfNum ).Vertex.allocate( SurfaceTmp( SurfNum ).Sides );

				MakeRectangularVertices( SurfNum, rNumericArgs( 3 ), rNumericArgs( 4 ), rNumericArgs( 5 ), rNumericArgs( 6 ), rNumericArgs( 7 ), RectSurfRefWorldCoordSystem );

				if ( SurfaceTmp( SurfNum ).Area <= 0.0 ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", Surface Area <= 0.0; Entered Area=" + TrimSigDigits( SurfaceTmp( SurfNum ).Area, 2 ) );
					ErrorsFound = true;
				}

				if ( MakeMirroredDetachedShading ) {
					MakeMirrorSurface( SurfNum );
				}
			}

		} // Item Loop

	}

	void
	GetHTSurfaceData(
		bool & ErrorsFound, // Error flag indicator (true if errors found)
		int & SurfNum, // Count of Current SurfaceNumber
		int const TotHTSurfs, // Number of Heat Transfer Base Surfaces to obtain
		int const TotDetailedWalls, // Number of Wall:Detailed items to obtain
		int const TotDetailedRoofs, // Number of RoofCeiling:Detailed items to obtain
		int const TotDetailedFloors, // Number of Floor:Detailed items to obtain
		Array1S_string const BaseSurfCls, // Valid Classes for Base Surfaces
		Array1S_int const BaseSurfIDs,
		int & NeedToAddSurfaces // Number of surfaces to add, based on unentered IZ surfaces
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   May 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine gets the HeatTransfer Surface Data,
		// checks it for errors, etc.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// Heat Transfer Surface Definition
		//BuildingSurface:Detailed,
		//  \extensible:3 -- duplicate last set of x,y,z coordinates (last 3 fields), remembering to remove ; from "inner" fields.
		//  \format vertices
		//  A1 , \field Name
		//       \required-field
		//       \type alpha
		//       \reference SurfaceNames
		//       \reference SurfAndSubSurfNames
		//       \reference AllHeatTranSurfNames
		//       \reference HeatTranBaseSurfNames
		//       \reference OutFaceEnvNames
		//       \reference AllHeatTranAngFacNames
		//       \reference RadGroupAndSurfNames
		//       \reference SurfGroupAndHTSurfNames
		//       \reference AllShadingAndHTSurfNames
		//  A2 , \field Surface Type
		//       \required-field
		//       \type choice
		//       \key Floor
		//       \key Wall
		//       \key Ceiling
		//       \key Roof
		//  A3 , \field Construction Name
		//       \required-field
		//       \note To be matched with a construction in this input file
		//       \type object-list
		//       \object-list ConstructionNames
		//  A4 , \field Zone Name
		//       \required-field
		//       \note Zone the surface is a part of
		//       \type object-list
		//       \object-list ZoneNames
		//  A5 , \field Outside Boundary Condition
		//       \required-field
		//       \type choice
		//       \key Adiabatic
		//       \key Surface
		//       \key Zone
		//       \key Outdoors
		//       \key Ground
		//       \key GroundFCfactorMethod
		//       \key OtherSideCoefficients
		//       \key OtherSideConditionsModel
		//       \key GroundSlabPreprocessorAverage
		//       \key GroundSlabPreprocessorCore
		//       \key GroundSlabPreprocessorPerimeter
		//       \key GroundBasementPreprocessorAverageWall
		//       \key GroundBasementPreprocessorAverageFloor
		//       \key GroundBasementPreprocessorUpperWall
		//       \key GroundBasementPreprocessorLowerWall
		//  A6,  \field Outside Boundary Condition Object
		//       \type object-list
		//       \object-list OutFaceEnvNames
		//       \note Non-blank only if the field Outside Boundary Condition is Surface,
		//       \note Zone, OtherSideCoefficients or OtherSideConditionsModel
		//       \note If Surface, specify name of corresponding surface in adjacent zone or
		//       \note specify current surface name for internal partition separating like zones
		//       \note If Zone, specify the name of the corresponding zone and
		//       \note the program will generate the corresponding interzone surface
		//       \note If OtherSideCoefficients, specify name of SurfaceProperty:OtherSideCoefficients
		//       \note If OtherSideConditionsModel, specify name of SurfaceProperty:OtherSideConditionsModel
		//  A7 , \field Sun Exposure
		//       \required-field
		//       \type choice
		//       \key SunExposed
		//       \key NoSun
		//       \default SunExposed
		//  A8,  \field Wind Exposure
		//       \required-field
		//       \type choice
		//       \key WindExposed
		//       \key NoWind
		//       \default WindExposed
		//  N1,  \field View Factor to Ground
		//       \type real
		//       \note From the exterior of the surface
		//       \note Unused if one uses the "reflections" options in Solar Distribution in Building input
		//       \note unless a DaylightingDevice:Shelf or DaylightingDevice:Tubular object has been specified.
		//       \note autocalculate will automatically calculate this value from the tilt of the surface
		//       \autocalculatable
		//       \minimum 0.0
		//       \maximum 1.0
		//       \default autocalculate
		//  N2 , \field Number of Vertices
		//       \note shown with 120 vertex coordinates -- extensible object
		//       \note  "extensible" -- duplicate last set of x,y,z coordinates (last 3 fields),
		//       \note remembering to remove ; from "inner" fields.
		//       \note for clarity in any error messages, renumber the fields as well.
		//       \note (and changing z terminator to a comma "," for all but last one which needs a semi-colon ";")
		//       \autocalculatable
		//       \minimum 3
		//       \default autocalculate
		//       \note vertices are given in GlobalGeometryRules coordinates -- if relative, all surface coordinates
		//       \note are "relative" to the Zone Origin.  If world, then building and zone origins are used
		//       \note for some internal calculations, but all coordinates are given in an "absolute" system.
		//  N3-xx as indicated by the N3 value

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using InputProcessor::VerifyName;
		using InputProcessor::SameString;
		using InputProcessor::GetObjectDefMaxArgs;
		using General::RoundSigDigits;
		using General::TrimSigDigits;

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static Array1D_string const cModuleObjects( 4, { "BuildingSurface:Detailed", "Wall:Detailed", "Floor:Detailed", "RoofCeiling:Detailed" } );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int IOStat; // IO Status when calling get input subroutine
		int SurfaceNumAlpha; // Number of material alpha names being passed
		int SurfaceNumProp; // Number of material properties being passed
		int ZoneNum; // DO loop counter (zones)
		int Found; // For matching interzone surfaces
		int Loop;
		bool ErrorInName;
		bool IsBlank;
		int Item;
		int ItemsToGet;
		int ClassItem;
		int ArgPointer;
		int numSides;

		GetOSCData( ErrorsFound );
		GetOSCMData( ErrorsFound );

		NeedToAddSurfaces = 0;

		for ( Item = 1; Item <= 4; ++Item ) {

			cCurrentModuleObject = cModuleObjects( Item );
			if ( Item == 1 ) {
				ItemsToGet = TotHTSurfs;
				ClassItem = 0;
			} else if ( Item == 2 ) {
				ItemsToGet = TotDetailedWalls;
				ClassItem = 1;
			} else if ( Item == 3 ) {
				ItemsToGet = TotDetailedFloors;
				ClassItem = 2;
			} else { //IF (Item == 4) THEN
				ItemsToGet = TotDetailedRoofs;
				ClassItem = 3;
			}

			GetObjectDefMaxArgs( cCurrentModuleObject, Loop, SurfaceNumAlpha, SurfaceNumProp );
			if ( Item == 1 ) {
				if ( SurfaceNumAlpha != 8 ) {
					ShowSevereError( cCurrentModuleObject + ": Object Definition indicates not = 8 Alpha Objects, Number Indicated=" + TrimSigDigits( SurfaceNumAlpha ) );
					ErrorsFound = true;
				}
			} else {
				if ( SurfaceNumAlpha != 7 ) {
					ShowSevereError( cCurrentModuleObject + ": Object Definition indicates not = 7 Alpha Objects, Number Indicated=" + TrimSigDigits( SurfaceNumAlpha ) );
					ErrorsFound = true;
				}
			}

			for ( Loop = 1; Loop <= ItemsToGet; ++Loop ) {
				GetObjectItem( cCurrentModuleObject, Loop, cAlphaArgs, SurfaceNumAlpha, rNumericArgs, SurfaceNumProp, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
				ErrorInName = false;
				IsBlank = false;
				VerifyName( cAlphaArgs( 1 ), SurfaceTmp, SurfNum, ErrorInName, IsBlank, cCurrentModuleObject + " Name" );
				if ( ErrorInName ) {
					ShowContinueError( "...each surface name must not duplicate other surface names (of any type)" );
					ErrorsFound = true;
					continue;
				}

				++SurfNum;
				SurfaceTmp( SurfNum ).Name = cAlphaArgs( 1 ); // Set the Surface Name in the Derived Type
				ArgPointer = 2;
				if ( Item == 1 ) {
					if ( cAlphaArgs( 2 ) == "CEILING" ) cAlphaArgs( 2 ) = "ROOF";
					ClassItem = FindItemInList( cAlphaArgs( 2 ), BaseSurfCls, 3 );
					if ( ClassItem == 0 ) {
						ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) );
						ErrorsFound = true;
					} else {
						SurfaceTmp( SurfNum ).Class = BaseSurfIDs( ClassItem );
					}
					++ArgPointer;
				} else {
					SurfaceTmp( SurfNum ).Class = BaseSurfIDs( ClassItem );
				}

				SurfaceTmp( SurfNum ).Construction = FindItemInList( cAlphaArgs( ArgPointer ), Construct, TotConstructs );

				if ( SurfaceTmp( SurfNum ).Construction == 0 ) {
					ErrorsFound = true;
					ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", invalid " + cAlphaFieldNames( ArgPointer ) + "=\"" + cAlphaArgs( ArgPointer ) + "\"." );
				} else if ( Construct( SurfaceTmp( SurfNum ).Construction ).TypeIsWindow ) {
					ErrorsFound = true;
					ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", invalid " + cAlphaFieldNames( ArgPointer ) + "=\"" + cAlphaArgs( ArgPointer ) + "\" - has Window materials." );
					if ( Item == 1 ) {
						ShowContinueError( "...because " + cAlphaFieldNames( 2 ) + '=' + cAlphaArgs( 2 ) );
					} else {
						ShowContinueError( "...because Surface Type=" + BaseSurfCls( ClassItem ) );
					}
				} else {
					Construct( SurfaceTmp( SurfNum ).Construction ).IsUsed = true;
					SurfaceTmp( SurfNum ).ConstructionStoredInputValue = SurfaceTmp( SurfNum ).Construction;
				}
				SurfaceTmp( SurfNum ).HeatTransSurf = true;
				SurfaceTmp( SurfNum ).BaseSurf = SurfNum;
				SurfaceTmp( SurfNum ).BaseSurfName = SurfaceTmp( SurfNum ).Name;

				++ArgPointer;
				SurfaceTmp( SurfNum ).ZoneName = cAlphaArgs( ArgPointer );
				ZoneNum = FindItemInList( SurfaceTmp( SurfNum ).ZoneName, Zone, NumOfZones );

				if ( ZoneNum != 0 ) {
					SurfaceTmp( SurfNum ).Zone = ZoneNum;
				} else {
					ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", invalid " + cAlphaFieldNames( ArgPointer ) + "=\"" + cAlphaArgs( ArgPointer ) + "\"." );
					SurfaceTmp( SurfNum ).Class += 100;
					SurfaceTmp( SurfNum ).ZoneName = "Unknown Zone";
					ErrorsFound = true;
				}
				// Get the ExteriorBoundaryCondition flag from input There are 4 conditions that
				// can take place. The conditions are set with a 0, -1, or -2, or all of the
				// zone names have to be looked at and generate the interzone array number
				++ArgPointer;
				SurfaceTmp( SurfNum ).ExtBoundCondName = cAlphaArgs( ArgPointer + 1 );

				if ( SameString( cAlphaArgs( ArgPointer ), "Outdoors" ) ) {
					SurfaceTmp( SurfNum ).ExtBoundCond = ExternalEnvironment;

				} else if ( SameString( cAlphaArgs( ArgPointer ), "Adiabatic" ) ) {
					SurfaceTmp( SurfNum ).ExtBoundCond = UnreconciledZoneSurface;
					SurfaceTmp( SurfNum ).ExtBoundCondName = SurfaceTmp( SurfNum ).Name;

				} else if ( SameString( cAlphaArgs( ArgPointer ), "Ground" ) ) {
					SurfaceTmp( SurfNum ).ExtBoundCond = Ground;

					if ( NoGroundTempObjWarning ) {
						if ( ! GroundTempObjInput ) {
							ShowWarningError( "GetHTSurfaceData: Surfaces with interface to Ground found but no \"Ground Temperatures\" were input." );
							ShowContinueError( "Found first in surface=" + cAlphaArgs( 1 ) );
							ShowContinueError( "Defaults, constant throughout the year of (" + RoundSigDigits( GroundTemp, 1 ) + ") will be used." );
						}
						NoGroundTempObjWarning = false;
					}

					// Added for FCfactor method
				} else if ( SameString( cAlphaArgs( ArgPointer ), "GroundFCfactorMethod" ) ) {
					SurfaceTmp( SurfNum ).ExtBoundCond = GroundFCfactorMethod;
					if ( NoFCGroundTempObjWarning ) {
						if ( ! FCGroundTemps ) {
							ShowSevereError( "GetHTSurfaceData: Surfaces with interface to GroundFCfactorMethod found but no \"FC Ground Temperatures\" were input." );
							ShowContinueError( "Found first in surface=" + cAlphaArgs( 1 ) );
							ShowContinueError( "Either add a \"Site:GroundTemperature:FCfactorMethod\" object or use a weather file with Ground Temperatures." );
							ErrorsFound = true;
							NoFCGroundTempObjWarning = false;
						}
					}
					if ( SurfaceTmp( SurfNum ).Construction > 0 ) {
						if ( SurfaceTmp( SurfNum ).Class == SurfaceClass_Wall && ! Construct( SurfaceTmp( SurfNum ).Construction ).TypeIsCfactorWall ) {
							ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", invalid " + cAlphaFieldNames( ArgPointer ) );
							ShowContinueError( "Construction=\"" + Construct( SurfaceTmp( SurfNum ).Construction ).Name + "\" is not type Construction:CfactorUndergroundWall." );
							ErrorsFound = true;
						}
						if ( SurfaceTmp( SurfNum ).Class == SurfaceClass_Floor && ! Construct( SurfaceTmp( SurfNum ).Construction ).TypeIsFfactorFloor ) {
							ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", invalid " + cAlphaFieldNames( ArgPointer ) );
							ShowContinueError( "Construction=\"" + Construct( SurfaceTmp( SurfNum ).Construction ).Name + "\" is not type Construction:FfactorGroundFloor." );
							ErrorsFound = true;
						}
					}

				} else if ( SameString( cAlphaArgs( ArgPointer ), "OtherSideCoefficients" ) ) {
					Found = FindItemInList( SurfaceTmp( SurfNum ).ExtBoundCondName, OSC, TotOSC );
					if ( Found == 0 ) {
						ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", invalid " + cAlphaFieldNames( ArgPointer + 1 ) + "=\"" + cAlphaArgs( ArgPointer + 1 ) + "\"." );
						ShowContinueError( " no OtherSideCoefficients of that name." );
						ErrorsFound = true;
					} else {
						SurfaceTmp( SurfNum ).OSCPtr = Found;
						if ( OSC( Found ).SurfFilmCoef > 0.0 ) {
							SurfaceTmp( SurfNum ).ExtBoundCond = OtherSideCoefCalcExt;
						} else {
							SurfaceTmp( SurfNum ).ExtBoundCond = OtherSideCoefNoCalcExt;
						}
					}

				} else if ( SameString( cAlphaArgs( ArgPointer ), "Surface" ) ) {
					// it has to be another surface which needs to be found
					// this will be found on the second pass through the surface input
					// for flagging, set the value to UnreconciledZoneSurface
					// name (ExtBoundCondName) will be validated later.
					SurfaceTmp( SurfNum ).ExtBoundCond = UnreconciledZoneSurface;
					if ( lAlphaFieldBlanks( ArgPointer + 1 ) ) {
						SurfaceTmp( SurfNum ).ExtBoundCondName = SurfaceTmp( SurfNum ).Name;
						ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", invalid " + cAlphaFieldNames( ArgPointer + 1 ) + "=<blank>." );
						ShowContinueError( ".." + cAlphaFieldNames( ArgPointer ) + "=\"Surface\" must be non-blank." );
						ShowContinueError( "..This surface will become an adiabatic surface - no doors/windows allowed." );
					}

				} else if ( SameString( cAlphaArgs( ArgPointer ), "Zone" ) ) {
					// This is the code for an unmatched "other surface"
					// will be set up later.
					SurfaceTmp( SurfNum ).ExtBoundCond = UnenteredAdjacentZoneSurface;
					// check OutsideFaceEnvironment for legal zone
					Found = FindItemInList( SurfaceTmp( SurfNum ).ExtBoundCondName, Zone, NumOfZones );
					++NeedToAddSurfaces;

					if ( Found == 0 ) {
						ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", invalid " + cAlphaFieldNames( ArgPointer ) + "=\"" + cAlphaArgs( ArgPointer ) + "\"." );
						ShowContinueError( "..Referenced as Zone for this surface." );
						ErrorsFound = true;
					}

				} else if ( SameString( cAlphaArgs( ArgPointer ), "OtherSideConditionsModel" ) ) {
					Found = FindItemInList( SurfaceTmp( SurfNum ).ExtBoundCondName, OSCM, TotOSCM );
					if ( Found == 0 ) {
						ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", invalid " + cAlphaFieldNames( ArgPointer ) + "=\"" + cAlphaArgs( ArgPointer ) + "\"." );
						ErrorsFound = true;
					}
					SurfaceTmp( SurfNum ).OSCMPtr = Found;
					SurfaceTmp( SurfNum ).ExtBoundCond = OtherSideCondModeledExt;

				} else if ( SameString( cAlphaArgs( ArgPointer ), "GroundSlabPreprocessorAverage" ) || SameString( cAlphaArgs( ArgPointer ), "GroundSlabPreprocessorCore" ) || SameString( cAlphaArgs( ArgPointer ), "GroundSlabPreprocessorPerimeter" ) || SameString( cAlphaArgs( ArgPointer ), "GroundBasementPreprocessorAverageFloor" ) || SameString( cAlphaArgs( ArgPointer ), "GroundBasementPreprocessorAverageWall" ) || SameString( cAlphaArgs( ArgPointer ), "GroundBasementPreprocessorUpperWall" ) || SameString( cAlphaArgs( ArgPointer ), "GroundBasementPreprocessorLowerWall" ) ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", invalid " + cAlphaFieldNames( ArgPointer ) + "=\"" + cAlphaArgs( ArgPointer ) + "\"." );
					ShowContinueError( "The ExpandObjects program has not been run or is not in your EnergyPlus.exe folder." );
					ErrorsFound = true;

				} else {
					ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", invalid " + cAlphaFieldNames( ArgPointer ) + "=\"" + cAlphaArgs( ArgPointer ) + "\"." );
					ShowContinueError( "Should be one of \"Outdoors\", \"Adiabatic\", Ground\", \"Surface\", \"OtherSideCoefficients\", \"OtherSideConditionsModel\" or \"Zone\"" );
					ErrorsFound = true;
				} // ... End of the ExtBoundCond logical IF Block

				ArgPointer += 2;
				//Set the logical flag for the exterior solar
				if ( SameString( cAlphaArgs( ArgPointer ), "SunExposed" ) ) {
					SurfaceTmp( SurfNum ).ExtSolar = true;

					if ( ( SurfaceTmp( SurfNum ).ExtBoundCond != ExternalEnvironment ) && ( SurfaceTmp( SurfNum ).ExtBoundCond != OtherSideCondModeledExt ) ) {
						ShowWarningError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", " + cAlphaFieldNames( ArgPointer ) + "=\"" + cAlphaArgs( ArgPointer ) + "\"." );
						ShowContinueError( "..This surface is not exposed to External Environment.  Sun exposure has no effect." );
					}

				} else if ( SameString( cAlphaArgs( ArgPointer ), "NoSun" ) ) {
					SurfaceTmp( SurfNum ).ExtSolar = false;
				} else {
					ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", invalid " + cAlphaFieldNames( ArgPointer ) + "=\"" + cAlphaArgs( ArgPointer ) + "\"." );
					ErrorsFound = true;
				}

				++ArgPointer;
				//Set the logical flag for the exterior wind
				if ( SameString( cAlphaArgs( ArgPointer ), "WindExposed" ) ) {
					SurfaceTmp( SurfNum ).ExtWind = true;
				} else if ( SameString( cAlphaArgs( ArgPointer ), "NoWind" ) ) {
					SurfaceTmp( SurfNum ).ExtWind = false;
				} else {
					ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", invalid " + cAlphaFieldNames( ArgPointer ) + "=\"" + cAlphaArgs( ArgPointer ) + "\"." );
					ErrorsFound = true;
				}

				//Set the logical flag for the EcoRoof presented, this is only based on the flag in the construction type
				if ( SurfaceTmp( SurfNum ).Construction > 0 ) SurfaceTmp( SurfNum ).ExtEcoRoof = Construct( SurfaceTmp( SurfNum ).Construction ).TypeIsEcoRoof;

				SurfaceTmp( SurfNum ).ViewFactorGround = rNumericArgs( 1 );
				if ( lNumericFieldBlanks( 1 ) ) SurfaceTmp( SurfNum ).ViewFactorGround = AutoCalculate;
				if ( lNumericFieldBlanks( 2 ) || rNumericArgs( 2 ) == AutoCalculate ) {
					numSides = ( SurfaceNumProp - 2 ) / 3;
					SurfaceTmp( SurfNum ).Sides = numSides;
					if ( mod( SurfaceNumProp - 2, 3 ) != 0 ) {
						ShowWarningError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", " + cNumericFieldNames( 2 ) + " not even multiple of 3. Will read in " + TrimSigDigits( SurfaceTmp( SurfNum ).Sides ) );
					}
					if ( numSides < 3 ) {
						ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", " + cNumericFieldNames( 2 ) + " (autocalculate) must be >= 3. Only " + TrimSigDigits( SurfaceTmp( SurfNum ).Sides ) + " provided." );
						ErrorsFound = true;
						continue;
					}
				} else {
					numSides = ( SurfaceNumProp - 2 ) / 3;
					SurfaceTmp( SurfNum ).Sides = rNumericArgs( 2 );
					if ( numSides > SurfaceTmp( SurfNum ).Sides ) {
						ShowWarningError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", field " + cNumericFieldNames( 2 ) + '=' + TrimSigDigits( SurfaceTmp( SurfNum ).Sides ) );
						ShowContinueError( "...but " + TrimSigDigits( numSides ) + " were entered. Only the indicated " + cNumericFieldNames( 2 ) + " will be used." );
					}
				}
				SurfaceTmp( SurfNum ).Vertex.allocate( SurfaceTmp( SurfNum ).Sides );
				GetVertices( SurfNum, SurfaceTmp( SurfNum ).Sides, rNumericArgs( {3,_} ) );
				if ( SurfaceTmp( SurfNum ).Area <= 0.0 ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", Surface Area <= 0.0; Entered Area=" + TrimSigDigits( SurfaceTmp( SurfNum ).Area, 2 ) );
					ErrorsFound = true;
				}

				CheckConvexity( SurfNum, SurfaceTmp( SurfNum ).Sides );
				if ( SurfaceTmp( SurfNum ).Construction > 0 ) {
					//Check wall height for the CFactor walls
					if ( SurfaceTmp( SurfNum ).Class == SurfaceClass_Wall && Construct( SurfaceTmp( SurfNum ).Construction ).TypeIsCfactorWall ) {
						if ( std::abs( SurfaceTmp( SurfNum ).Height - Construct( SurfaceTmp( SurfNum ).Construction ).Height ) > 0.05 ) {
							ShowWarningError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", underground Wall Height = " + TrimSigDigits( SurfaceTmp( SurfNum ).Height, 2 ) );
							ShowContinueError( "..which does not match its construction height." );
						}
					}

					//Check area and perimeter for the FFactor floors
					if ( SurfaceTmp( SurfNum ).Class == SurfaceClass_Floor && Construct( SurfaceTmp( SurfNum ).Construction ).TypeIsFfactorFloor ) {
						if ( std::abs( SurfaceTmp( SurfNum ).Area - Construct( SurfaceTmp( SurfNum ).Construction ).Area ) > 0.1 ) {
							ShowWarningError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", underground Floor Area = " + TrimSigDigits( SurfaceTmp( SurfNum ).Area, 2 ) );
							ShowContinueError( "..which does not match its construction area." );
						}
						if ( SurfaceTmp( SurfNum ).Perimeter < Construct( SurfaceTmp( SurfNum ).Construction ).PerimeterExposed - 0.1 ) {
							ShowWarningError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", underground Floor Perimeter = " + TrimSigDigits( SurfaceTmp( SurfNum ).Perimeter, 2 ) );
							ShowContinueError( "..which is less than its construction exposed perimeter." );
						}
					}
				}

			}
		} // Item Looop

	}

	void
	GetRectSurfaces(
		bool & ErrorsFound, // Error flag indicator (true if errors found)
		int & SurfNum, // Count of Current SurfaceNumber
		int const TotRectExtWalls, // Number of Exterior Walls to obtain
		int const TotRectIntWalls, // Number of Adiabatic Walls to obtain
		int const TotRectIZWalls, // Number of Interzone Walls to obtain
		int const TotRectUGWalls, // Number of Underground to obtain
		int const TotRectRoofs, // Number of Roofs to obtain
		int const TotRectCeilings, // Number of Adiabatic Ceilings to obtain
		int const TotRectIZCeilings, // Number of Interzone Ceilings to obtain
		int const TotRectGCFloors, // Number of Floors with Ground Contact to obtain
		int const TotRectIntFloors, // Number of Adiabatic Walls to obtain
		int const TotRectIZFloors, // Number of Interzone Floors to obtain
		Array1S_int const BaseSurfIDs, // ID Assignments for valid surface classes
		int & NeedToAddSurfaces // Number of surfaces to add, based on unentered IZ surfaces
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   December 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Get simple (rectangular, LLC corner specified) walls

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using InputProcessor::VerifyName;
		using InputProcessor::GetObjectDefMaxArgs;
		using General::TrimSigDigits;
		using General::RoundSigDigits;

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static Array1D_string const cModuleObjects( 10, { "Wall:Exterior", "Wall:Adiabatic", "Wall:Interzone", "Wall:Underground", "Roof", "Ceiling:Adiabatic", "Ceiling:Interzone", "Floor:GroundContact", "Floor:Adiabatic", "Floor:Interzone" } );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		int Item;
		int ItemsToGet;
		int Loop;
		int NumAlphas;
		int NumNumbers;
		int IOStat; // IO Status when calling get input subroutine
		int Found; // For matching base surfaces
		bool ErrorInName;
		bool IsBlank;
		bool GettingIZSurfaces;
		int OtherSurfaceField;
		int ExtBoundCondition;
		int ClassItem;
		int ZoneNum;

		for ( Item = 1; Item <= 10; ++Item ) {

			cCurrentModuleObject = cModuleObjects( Item );
			if ( Item == 1 ) {
				ItemsToGet = TotRectExtWalls;
				GettingIZSurfaces = false;
				OtherSurfaceField = 0;
				ExtBoundCondition = ExternalEnvironment;
				ClassItem = 1;
			} else if ( Item == 2 ) {
				ItemsToGet = TotRectIntWalls;
				GettingIZSurfaces = false;
				OtherSurfaceField = 0;
				ExtBoundCondition = UnreconciledZoneSurface;
				ClassItem = 1;
			} else if ( Item == 3 ) {
				ItemsToGet = TotRectIZWalls;
				GettingIZSurfaces = true;
				OtherSurfaceField = 4;
				ExtBoundCondition = UnreconciledZoneSurface;
				ClassItem = 1;
			} else if ( Item == 4 ) {
				ItemsToGet = TotRectUGWalls;
				GettingIZSurfaces = false;
				OtherSurfaceField = 0;
				ExtBoundCondition = Ground;
				ClassItem = 1;
			} else if ( Item == 5 ) {
				ItemsToGet = TotRectRoofs;
				GettingIZSurfaces = false;
				OtherSurfaceField = 0;
				ExtBoundCondition = ExternalEnvironment;
				ClassItem = 3;
			} else if ( Item == 6 ) {
				ItemsToGet = TotRectCeilings;
				GettingIZSurfaces = false;
				OtherSurfaceField = 0;
				ExtBoundCondition = UnreconciledZoneSurface;
				ClassItem = 3;
			} else if ( Item == 7 ) {
				ItemsToGet = TotRectIZCeilings;
				GettingIZSurfaces = false;
				OtherSurfaceField = 4;
				ExtBoundCondition = UnreconciledZoneSurface;
				ClassItem = 3;
			} else if ( Item == 8 ) {
				ItemsToGet = TotRectGCFloors;
				GettingIZSurfaces = false;
				OtherSurfaceField = 0;
				ExtBoundCondition = Ground;
				ClassItem = 2;
			} else if ( Item == 9 ) {
				ItemsToGet = TotRectIntFloors;
				GettingIZSurfaces = false;
				OtherSurfaceField = 0;
				ExtBoundCondition = UnreconciledZoneSurface;
				ClassItem = 2;
			} else { //IF (Item == 10) THEN
				ItemsToGet = TotRectIZFloors;
				GettingIZSurfaces = true;
				OtherSurfaceField = 4;
				ExtBoundCondition = UnreconciledZoneSurface;
				ClassItem = 2;
			}

			for ( Loop = 1; Loop <= ItemsToGet; ++Loop ) {
				GetObjectItem( cCurrentModuleObject, Loop, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
				ErrorInName = false;
				IsBlank = false;
				VerifyName( cAlphaArgs( 1 ), SurfaceTmp, SurfNum, ErrorInName, IsBlank, cCurrentModuleObject + " Name" );
				if ( ErrorInName ) {
					ShowContinueError( "...each surface name must not duplicate other surface names (of any type)" );
					ErrorsFound = true;
					continue;
				}

				if ( NumNumbers < 7 ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", Too few number of numeric args=[" + TrimSigDigits( NumNumbers ) + "]." );
					ErrorsFound = true;
				}

				++SurfNum;
				SurfaceTmp( SurfNum ).Name = cAlphaArgs( 1 ); // Set the Surface Name in the Derived Type
				SurfaceTmp( SurfNum ).Class = BaseSurfIDs( ClassItem ); // Set class number

				SurfaceTmp( SurfNum ).Construction = FindItemInList( cAlphaArgs( 2 ), Construct, TotConstructs );

				if ( SurfaceTmp( SurfNum ).Construction == 0 ) {
					ErrorsFound = true;
					ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\"." );
				} else if ( Construct( SurfaceTmp( SurfNum ).Construction ).TypeIsWindow ) {
					ErrorsFound = true;
					ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", invalid " + cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 2 ) + "\" - has Window materials." );
					ShowContinueError( "...because " + cAlphaFieldNames( 2 ) + '=' + cAlphaArgs( 2 ) );
				} else {
					Construct( SurfaceTmp( SurfNum ).Construction ).IsUsed = true;
					SurfaceTmp( SurfNum ).ConstructionStoredInputValue = SurfaceTmp( SurfNum ).Construction;
				}
				SurfaceTmp( SurfNum ).HeatTransSurf = true;
				SurfaceTmp( SurfNum ).BaseSurf = SurfNum;
				SurfaceTmp( SurfNum ).BaseSurfName = SurfaceTmp( SurfNum ).Name;

				SurfaceTmp( SurfNum ).ZoneName = cAlphaArgs( 3 );
				ZoneNum = FindItemInList( SurfaceTmp( SurfNum ).ZoneName, Zone, NumOfZones );

				if ( ZoneNum != 0 ) {
					SurfaceTmp( SurfNum ).Zone = ZoneNum;
				} else {
					ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", invalid " + cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 3 ) + "\"." );
					SurfaceTmp( SurfNum ).Class += 100;
					SurfaceTmp( SurfNum ).ZoneName = "Unknown Zone";
					ErrorsFound = true;
				}

				SurfaceTmp( SurfNum ).ExtBoundCond = ExtBoundCondition;
				if ( SurfaceTmp( SurfNum ).Construction > 0 ) {
					if ( SurfaceTmp( SurfNum ).Class == SurfaceClass_Wall && Construct( SurfaceTmp( SurfNum ).Construction ).TypeIsCfactorWall && SurfaceTmp( SurfNum ).ExtBoundCond == Ground ) {
						SurfaceTmp( SurfNum ).ExtBoundCond = GroundFCfactorMethod;
					} else if ( Construct( SurfaceTmp( SurfNum ).Construction ).TypeIsCfactorWall ) {
						ErrorsFound = true;
						ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", Construction type is \"Construction:CfactorUndergroundWall\" but invalid for this object." );
					}
					if ( SurfaceTmp( SurfNum ).Class == SurfaceClass_Floor && Construct( SurfaceTmp( SurfNum ).Construction ).TypeIsFfactorFloor && SurfaceTmp( SurfNum ).ExtBoundCond == Ground ) {
						SurfaceTmp( SurfNum ).ExtBoundCond = GroundFCfactorMethod;
					} else if ( Construct( SurfaceTmp( SurfNum ).Construction ).TypeIsFfactorFloor ) {
						ErrorsFound = true;
						ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", Construction type is \"Construction:FfactorGroundFloor\" but invalid for this object." );
					}
				}
				SurfaceTmp( SurfNum ).ExtSolar = false;
				SurfaceTmp( SurfNum ).ExtWind = false;
				SurfaceTmp( SurfNum ).ViewFactorGround = AutoCalculate;

				if ( SurfaceTmp( SurfNum ).ExtBoundCond == ExternalEnvironment ) {
					SurfaceTmp( SurfNum ).ExtSolar = true;
					SurfaceTmp( SurfNum ).ExtWind = true;

					//Set the logical flag for the EcoRoof presented, this is only based on the flag in the construction type
					if ( SurfaceTmp( SurfNum ).Construction > 0 ) SurfaceTmp( SurfNum ).ExtEcoRoof = Construct( SurfaceTmp( SurfNum ).Construction ).TypeIsEcoRoof;

				} else if ( SurfaceTmp( SurfNum ).ExtBoundCond == UnreconciledZoneSurface ) {
					if ( GettingIZSurfaces ) {
						SurfaceTmp( SurfNum ).ExtBoundCondName = cAlphaArgs( OtherSurfaceField );
						Found = FindItemInList( SurfaceTmp( SurfNum ).ExtBoundCondName, Zone, NumOfZones );
						// see if match to zone, then it's an unentered other surface, else reconciled later
						if ( Found > 0 ) {
							++NeedToAddSurfaces;
							SurfaceTmp( SurfNum ).ExtBoundCond = UnenteredAdjacentZoneSurface;
						}
					} else {
						SurfaceTmp( SurfNum ).ExtBoundCondName = SurfaceTmp( SurfNum ).Name;
					}

				} else if ( SurfaceTmp( SurfNum ).ExtBoundCond == Ground ) {

					if ( NoGroundTempObjWarning ) {
						if ( ! GroundTempObjInput ) {
							ShowWarningError( "GetRectSurfaces: Surfaces with interface to Ground found but no \"Ground Temperatures\" were input." );
							ShowContinueError( "Found first in surface=" + cAlphaArgs( 1 ) );
							ShowContinueError( "Defaults, constant throughout the year of (" + RoundSigDigits( GroundTemp, 1 ) + ") will be used." );
						}
						NoGroundTempObjWarning = false;
					}

				} else if ( SurfaceTmp( SurfNum ).ExtBoundCond == GroundFCfactorMethod ) {
					if ( NoFCGroundTempObjWarning ) {
						if ( ! FCGroundTemps ) {
							ShowSevereError( "GetRectSurfaces: Surfaces with interface to GroundFCfactorMethod found but no \"FC Ground Temperatures\" were input." );
							ShowContinueError( "Found first in surface=" + cAlphaArgs( 1 ) );
							ShowContinueError( "Either add a \"Site:GroundTemperature:FCfactorMethod\" object or use a weather file with Ground Temperatures." );
							ErrorsFound = true;
							NoFCGroundTempObjWarning = false;
						}
					}

				} // ... End of the ExtBoundCond logical IF Block

				SurfaceTmp( SurfNum ).Azimuth = rNumericArgs( 1 );
				SurfaceTmp( SurfNum ).Tilt = rNumericArgs( 2 );
				if ( ! WorldCoordSystem ) {
					if ( ZoneNum != 0 ) {
						SurfaceTmp( SurfNum ).Azimuth += BuildingAzimuth + Zone( ZoneNum ).RelNorth;
					}
				}
				if ( ZoneNum != 0 ) {
					SurfaceTmp( SurfNum ).Azimuth += BuildingRotationAppendixG;
				}

				SurfaceTmp( SurfNum ).Sides = 4;
				SurfaceTmp( SurfNum ).Vertex.allocate( SurfaceTmp( SurfNum ).Sides );

				MakeRectangularVertices( SurfNum, rNumericArgs( 3 ), rNumericArgs( 4 ), rNumericArgs( 5 ), rNumericArgs( 6 ), rNumericArgs( 7 ), RectSurfRefWorldCoordSystem );

				if ( SurfaceTmp( SurfNum ).Area <= 0.0 ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", Surface Area <= 0.0; Entered Area=" + TrimSigDigits( SurfaceTmp( SurfNum ).Area, 2 ) );
					ErrorsFound = true;
				}

				//Check wall height for the CFactor walls
				if ( SurfaceTmp( SurfNum ).Class == SurfaceClass_Wall && SurfaceTmp( SurfNum ).ExtBoundCond == GroundFCfactorMethod ) {
					if ( std::abs( SurfaceTmp( SurfNum ).Height - Construct( SurfaceTmp( SurfNum ).Construction ).Height ) > 0.05 ) {
						ShowWarningError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", underground Wall Height = " + TrimSigDigits( SurfaceTmp( SurfNum ).Height, 2 ) );
						ShowContinueError( "..which deos not match its construction height." );
					}
				}

				//Check area and perimeter for the FFactor floors
				if ( SurfaceTmp( SurfNum ).Class == SurfaceClass_Floor && SurfaceTmp( SurfNum ).ExtBoundCond == GroundFCfactorMethod ) {
					if ( std::abs( SurfaceTmp( SurfNum ).Area - Construct( SurfaceTmp( SurfNum ).Construction ).Area ) > 0.1 ) {
						ShowWarningError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", underground Floor Area = " + TrimSigDigits( SurfaceTmp( SurfNum ).Area, 2 ) );
						ShowContinueError( "..which does not match its construction area." );
					}
					if ( SurfaceTmp( SurfNum ).Perimeter < Construct( SurfaceTmp( SurfNum ).Construction ).PerimeterExposed - 0.1 ) {
						ShowWarningError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", underground Floor Perimeter = " + TrimSigDigits( SurfaceTmp( SurfNum ).Perimeter, 2 ) );
						ShowContinueError( "..which is less than its construction exposed perimeter." );
					}
				}
			} // Getting Items

		}

	}

	void
	MakeRectangularVertices(
		int const SurfNum,
		Real64 const XCoord,
		Real64 const YCoord,
		Real64 const ZCoord,
		Real64 const Length,
		Real64 const Height,
		bool const SurfWorldCoordSystem
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   December 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine creates world/3d coordinates for rectangular surfaces using azimuth, tilt, LLC (X,Y,Z), length & height.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace Vectors;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 SurfAzimuth; // Surface Azimuth/Facing (same as Base Surface)
		Real64 SurfTilt; // Tilt (same as Base Surface)
		Real64 XLLC;
		Real64 YLLC;
		Real64 ZLLC;
		Real64 CosSurfAzimuth;
		Real64 SinSurfAzimuth;
		Real64 CosSurfTilt;
		Real64 SinSurfTilt;
		Array1D< Real64 > XX( 4 );
		Array1D< Real64 > YY( 4 );
		Real64 Xb;
		Real64 Yb;
		Real64 Perimeter;
		int n;
		int Vrt;

		if ( SurfaceTmp( SurfNum ).Zone == 0 && ( SurfaceTmp( SurfNum ).Class != SurfaceClass_Detached_F && SurfaceTmp( SurfNum ).Class != SurfaceClass_Detached_B ) ) return;

		SurfaceTmp( SurfNum ).Height = Height;
		SurfaceTmp( SurfNum ).Width = Length;

		SurfAzimuth = SurfaceTmp( SurfNum ).Azimuth;
		SurfTilt = SurfaceTmp( SurfNum ).Tilt;
		CosSurfAzimuth = std::cos( SurfAzimuth * DegToRadians );
		SinSurfAzimuth = std::sin( SurfAzimuth * DegToRadians );
		CosSurfTilt = std::cos( SurfTilt * DegToRadians );
		SinSurfTilt = std::sin( SurfTilt * DegToRadians );
		if ( ! SurfWorldCoordSystem ) {
			if ( SurfaceTmp( SurfNum ).Zone > 0 ) {
				Xb = XCoord * CosZoneRelNorth( SurfaceTmp( SurfNum ).Zone ) - YCoord * SinZoneRelNorth( SurfaceTmp( SurfNum ).Zone ) + Zone( SurfaceTmp( SurfNum ).Zone ).OriginX;
				Yb = XCoord * SinZoneRelNorth( SurfaceTmp( SurfNum ).Zone ) + YCoord * CosZoneRelNorth( SurfaceTmp( SurfNum ).Zone ) + Zone( SurfaceTmp( SurfNum ).Zone ).OriginY;
				XLLC = Xb * CosBldgRelNorth - Yb * SinBldgRelNorth;
				YLLC = Xb * SinBldgRelNorth + Yb * CosBldgRelNorth;
				ZLLC = ZCoord + Zone( SurfaceTmp( SurfNum ).Zone ).OriginZ;
			} else {
				if ( SurfaceTmp( SurfNum ).Class == SurfaceClass_Detached_B ) {
					Xb = XCoord;
					Yb = YCoord;
					XLLC = Xb * CosBldgRelNorth - Yb * SinBldgRelNorth;
					YLLC = Xb * SinBldgRelNorth + Yb * CosBldgRelNorth;
					ZLLC = ZCoord;
				} else {
					XLLC = XCoord;
					YLLC = YCoord;
					ZLLC = ZCoord;
				}
			}
		} else {
			// for world coordinates, only rotate for appendix G
			Xb = XCoord;
			Yb = YCoord;
			ZLLC = ZCoord;
			if ( SurfaceTmp( SurfNum ).Class != SurfaceClass_Detached_F ) {
				XLLC = Xb * CosBldgRotAppGonly - Yb * SinBldgRotAppGonly;
				YLLC = Xb * SinBldgRotAppGonly + Yb * CosBldgRotAppGonly;
			} else {
				XLLC = Xb;
				YLLC = Yb;
			}
		}

		XX( 1 ) = 0.0;
		XX( 2 ) = 0.0;
		XX( 3 ) = Length;
		XX( 4 ) = Length;
		YY( 1 ) = Height;
		YY( 4 ) = Height;
		YY( 3 ) = 0.0;
		YY( 2 ) = 0.0;

		for ( n = 1; n <= SurfaceTmp( SurfNum ).Sides; ++n ) {
			Vrt = n;
			SurfaceTmp( SurfNum ).Vertex( Vrt ).x = XLLC - XX( n ) * CosSurfAzimuth - YY( n ) * CosSurfTilt * SinSurfAzimuth;
			SurfaceTmp( SurfNum ).Vertex( Vrt ).y = YLLC + XX( n ) * SinSurfAzimuth - YY( n ) * CosSurfTilt * CosSurfAzimuth;
			SurfaceTmp( SurfNum ).Vertex( Vrt ).z = ZLLC + YY( n ) * SinSurfTilt;
		}

		CreateNewellAreaVector( SurfaceTmp( SurfNum ).Vertex, SurfaceTmp( SurfNum ).Sides, SurfaceTmp( SurfNum ).NewellAreaVector );
		SurfaceTmp( SurfNum ).GrossArea = VecLength( SurfaceTmp( SurfNum ).NewellAreaVector );
		SurfaceTmp( SurfNum ).Area = SurfaceTmp( SurfNum ).GrossArea;
		SurfaceTmp( SurfNum ).NetAreaShadowCalc = SurfaceTmp( SurfNum ).Area;
		CreateNewellSurfaceNormalVector( SurfaceTmp( SurfNum ).Vertex, SurfaceTmp( SurfNum ).Sides, SurfaceTmp( SurfNum ).NewellSurfaceNormalVector );
		DetermineAzimuthAndTilt( SurfaceTmp( SurfNum ).Vertex, SurfaceTmp( SurfNum ).Sides, SurfAzimuth, SurfTilt, SurfaceTmp( SurfNum ).lcsx, SurfaceTmp( SurfNum ).lcsy, SurfaceTmp( SurfNum ).lcsz, SurfaceTmp( SurfNum ).GrossArea, SurfaceTmp( SurfNum ).NewellSurfaceNormalVector );
		SurfaceTmp( SurfNum ).Azimuth = SurfAzimuth;
		SurfaceTmp( SurfNum ).Tilt = SurfTilt;
		// Sine and cosine of azimuth and tilt
		SurfaceTmp( SurfNum ).SinAzim = SinSurfAzimuth;
		SurfaceTmp( SurfNum ).CosAzim = CosSurfAzimuth;
		SurfaceTmp( SurfNum ).SinTilt = SinSurfTilt;
		SurfaceTmp( SurfNum ).CosTilt = CosSurfTilt;
		SurfaceTmp( SurfNum ).ViewFactorGround = 0.5 * ( 1.0 - SurfaceTmp( SurfNum ).CosTilt );
		// Outward normal unit vector (pointing away from room)
		SurfaceTmp( SurfNum ).OutNormVec = SurfaceTmp( SurfNum ).NewellSurfaceNormalVector;
		for ( n = 1; n <= 3; ++n ) {
			if ( std::abs( SurfaceTmp( SurfNum ).OutNormVec( n ) - 1.0 ) < 1.e-06 ) SurfaceTmp( SurfNum ).OutNormVec( n ) = +1.0;
			if ( std::abs( SurfaceTmp( SurfNum ).OutNormVec( n ) + 1.0 ) < 1.e-06 ) SurfaceTmp( SurfNum ).OutNormVec( n ) = -1.0;
			if ( std::abs( SurfaceTmp( SurfNum ).OutNormVec( n ) ) < 1.e-06 ) SurfaceTmp( SurfNum ).OutNormVec( n ) = 0.0;
		}

		//  IF (SurfaceTmp(SurfNum)%Class == SurfaceClass_Roof .and. SurfTilt > 80.) THEN
		//    WRITE(TiltString,'(F5.1)') SurfTilt
		//    TiltString=ADJUSTL(TiltString)
		//    CALL ShowWarningError('Roof/Ceiling Tilt='//TRIM(TiltString)//', much greater than expected tilt of 0,'// &
		//                          ' for Surface='//TRIM(SurfaceTmp(SurfNum)%Name)//  &
		//                          ', in Zone='//TRIM(SurfaceTmp(SurfNum)%ZoneName))
		//  ENDIF
		//  IF (SurfaceTmp(SurfNum)%Class == SurfaceClass_Floor .and. SurfTilt < 170.) THEN
		//    WRITE(TiltString,'(F5.1)') SurfTilt
		//    TiltString=ADJUSTL(TiltString)
		//    CALL ShowWarningError('Floor Tilt='//TRIM(TiltString)//', much less than expected tilt of 180,'//   &
		//                          ' for Surface='//TRIM(SurfaceTmp(SurfNum)%Name)//  &
		//                          ', in Zone='//TRIM(SurfaceTmp(SurfNum)%ZoneName))
		//  ENDIF

		// Can perform tests on this surface here
		SurfaceTmp( SurfNum ).ViewFactorSky = 0.5 * ( 1.0 + SurfaceTmp( SurfNum ).CosTilt );
		// The following IR view factors are modified in subr. SkyDifSolarShading if there are shadowing
		// surfaces
		SurfaceTmp( SurfNum ).ViewFactorSkyIR = SurfaceTmp( SurfNum ).ViewFactorSky;
		SurfaceTmp( SurfNum ).ViewFactorGroundIR = 0.5 * ( 1.0 - SurfaceTmp( SurfNum ).CosTilt );

		Perimeter = distance( SurfaceTmp( SurfNum ).Vertex( SurfaceTmp( SurfNum ).Sides ), SurfaceTmp( SurfNum ).Vertex( 1 ) );
		for ( Vrt = 2; Vrt <= SurfaceTmp( SurfNum ).Sides; ++Vrt ) {
			Perimeter += distance( SurfaceTmp( SurfNum ).Vertex( Vrt ), SurfaceTmp( SurfNum ).Vertex( Vrt - 1 ) );
		}
		SurfaceTmp( SurfNum ).Perimeter = Perimeter;

		// Call to transform vertices

		TransformVertsByAspect( SurfNum, SurfaceTmp( SurfNum ).Sides );

	}

	void
	GetHTSubSurfaceData(
		bool & ErrorsFound, // Error flag indicator (true if errors found)
		int & SurfNum, // Count of Current SurfaceNumber
		int const TotHTSubs, // Number of Heat Transfer SubSurfaces to obtain
		Array1S_string const SubSurfCls, // Valid Classes for Sub Surfaces
		Array1S_int const SubSurfIDs, // ID Assignments for valid sub surface classes
		int & AddedSubSurfaces, // Subsurfaces added when windows reference Window5
		int & NeedToAddSurfaces // Number of surfaces to add, based on unentered IZ surfaces
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   May 2000
		//       MODIFIED       August 2012 - line up subsurfaces with base surface types
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine gets the HeatTransfer Sub Surface Data,
		// checks it for errors, etc.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// Heat Transfer Subsurface Definition
		// FenestrationSurface:Detailed,
		//        \min-fields 19
		//        \memo Used for windows, doors, glass doors, tubular daylighting devices
		//        \format vertices
		//   A1 , \field Name
		//        \required-field
		//        \type alpha
		//   A2 , \field Surface Type
		//        \required-field
		//        \type choice
		//        \key Window
		//        \key Door
		//        \key GlassDoor
		//        \key TubularDaylightDome
		//        \key TubularDaylightDiffuser
		//   A3 , \field Construction Name
		//        \required-field
		//        \note To be matched with a construction in this input file
		//        \type object-list
		//        \object-list ConstructionNames
		//   A4 , \field Building Surface Name
		//        \required-field
		//        \type object-list
		//        \object-list SurfaceNames
		//   A5,  \field Outside Boundary Condition Object
		//        \type object-list
		//        \object-list OutFaceEnvNames
		//        \note Non-blank only if base surface field Outside Boundary Condition is
		//        \note Surface or OtherSideCoefficients
		//        \note If Base Surface's Surface, specify name of corresponding subsurface in adjacent zone or
		//        \note specify current subsurface name for internal partition separating like zones
		//        \note If OtherSideCoefficients, specify name of SurfaceProperty:OtherSideCoefficients
		//        \note  or leave blank to inherit Base Surface's OtherSide Coefficients
		//   N1, \field View Factor to Ground
		//        \type real
		//        \note From the exterior of the surface
		//        \note Unused if one uses the "reflections" options in Solar Distribution in Building input
		//        \note unless a DaylightingDevice:Shelf or DaylightingDevice:Tubular object has been specified.
		//        \note autocalculate will automatically calculate this value from the tilt of the surface
		//        \autocalculatable
		//        \minimum 0.0
		//        \maximum 1.0
		//        \default autocalculate
		//   A6, \field Shading Control Name
		//        \note enter the name of a WindowProperty:ShadingControl object
		//        \type object-list
		//        \object-list WindowShadeControlNames
		//        \note used for windows and glass doors only
		//        \note If not specified, window or glass door has no shading (blind, roller shade, etc.)
		//   A7, \field Frame and Divider Name
		//        \note Enter the name of a WindowProperty:FrameAndDivider object
		//        \type object-list
		//        \object-list WindowFrameAndDividerNames
		//        \note Used only for exterior windows (rectangular) and glass doors.
		//        \note Unused for triangular windows.
		//        \note If not specified (blank), window or glass door has no frame or divider
		//        \note and no beam solar reflection from reveal surfaces.
		//   N2 , \field Multiplier
		//        \note Used only for Surface Type = WINDOW, GLASSDOOR or DOOR
		//        \note Non-integer values will be truncated to integer
		//        \default 1.0
		//        \minimum 1.0
		//   N3 , \field Number of Vertices
		//        \minimum 3
		//        \maximum 4
		//        \autocalculatable
		//        \default autocalculate
		//        \note vertices are given in GlobalGeometryRules coordinates -- if relative, all surface coordinates
		//        \note are "relative" to the Zone Origin.  If world, then building and zone origins are used
		//        \note for some internal calculations, but all coordinates are given in an "absolute" system.
		//  N4-15 as indicated by the N3 value

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using InputProcessor::VerifyName;
		using InputProcessor::GetObjectDefMaxArgs;
		using General::TrimSigDigits;
		using General::RoundSigDigits;

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		//  data file entry with two glazing systems

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int IOStat; // IO Status when calling get input subroutine
		int SurfaceNumAlpha; // Number of material alpha names being passed
		int SurfaceNumProp; // Number of material properties being passed
		int Found; // For matching interzone surfaces
		int Loop;
		bool ErrorInName;
		bool IsBlank;
		int ValidChk;
		int numSides;

		GetWindowShadingControlData( ErrorsFound );

		cCurrentModuleObject = "FenestrationSurface:Detailed";
		GetObjectDefMaxArgs( cCurrentModuleObject, Loop, SurfaceNumAlpha, SurfaceNumProp );

		if ( SurfaceNumAlpha != 7 ) {
			ShowSevereError( cCurrentModuleObject + ": Object Definition indicates not = 7 Alpha Objects, Number Indicated=" + TrimSigDigits( SurfaceNumAlpha ) );
			ErrorsFound = true;
		}

		if ( SurfaceNumProp != 15 ) {
			ShowSevereError( cCurrentModuleObject + ": Object Definition indicates > 15 Numeric Objects, Number Indicated=" + TrimSigDigits( SurfaceNumAlpha ) );
			ErrorsFound = true;
		}
		NeedToAddSurfaces = 0;

		for ( Loop = 1; Loop <= TotHTSubs; ++Loop ) {
			GetObjectItem( cCurrentModuleObject, Loop, cAlphaArgs, SurfaceNumAlpha, rNumericArgs, SurfaceNumProp, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			ErrorInName = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), SurfaceTmp, SurfNum, ErrorInName, IsBlank, cCurrentModuleObject + " Name" );
			if ( ErrorInName ) {
				ShowContinueError( "...each surface name must not duplicate other surface names (of any type)" );
				ErrorsFound = true;
				continue;
			}

			if ( SurfaceNumProp < 12 ) {
				ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", Too few number of numeric args=[" + TrimSigDigits( SurfaceNumProp ) + "]." );
				ErrorsFound = true;
			}

			++SurfNum;
			SurfaceTmp( SurfNum ).Name = cAlphaArgs( 1 ); // Set the Surface Name in the Derived Type
			ValidChk = FindItemInList( cAlphaArgs( 2 ), SubSurfCls, 6 );
			if ( ValidChk == 0 ) {
				ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) );
				ErrorsFound = true;
			} else {
				SurfaceTmp( SurfNum ).Class = SubSurfIDs( ValidChk ); // Set class number
			}

			SurfaceTmp( SurfNum ).Construction = FindItemInList( cAlphaArgs( 3 ), Construct, TotConstructs );

			if ( SurfaceTmp( SurfNum ).Construction == 0 ) {
				ErrorsFound = true;
				ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", invalid " + cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 3 ) + "\"." );
			} else {
				Construct( SurfaceTmp( SurfNum ).Construction ).IsUsed = true;
				SurfaceTmp( SurfNum ).ConstructionStoredInputValue = SurfaceTmp( SurfNum ).Construction;
			}

			if ( SurfaceTmp( SurfNum ).Class == SurfaceClass_Window || SurfaceTmp( SurfNum ).Class == SurfaceClass_GlassDoor || SurfaceTmp( SurfNum ).Class == SurfaceClass_TDD_Diffuser || SurfaceTmp( SurfNum ).Class == SurfaceClass_TDD_Dome ) {

				if ( SurfaceTmp( SurfNum ).Construction != 0 ) {
					if ( ! Construct( SurfaceTmp( SurfNum ).Construction ).TypeIsWindow ) {
						ErrorsFound = true;
						ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\" has an opaque surface construction; it should have a window construction." );
					}
				}

			} else if ( SurfaceTmp( SurfNum ).Construction != 0 ) {
				if ( Construct( SurfaceTmp( SurfNum ).Construction ).TypeIsWindow ) {
					ErrorsFound = true;
					ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", invalid " + cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 3 ) + "\" - has Window materials." );
					ShowContinueError( "...because " + cAlphaFieldNames( 2 ) + '=' + cAlphaArgs( 2 ) );
				}
			}

			SurfaceTmp( SurfNum ).HeatTransSurf = true;

			SurfaceTmp( SurfNum ).BaseSurfName = cAlphaArgs( 4 );
			//  The subsurface inherits properties from the base surface
			//  Exterior conditions, Zone, etc.
			//  We can figure out the base surface though, because they've all been entered
			Found = FindItemInList( SurfaceTmp( SurfNum ).BaseSurfName, SurfaceTmp, TotSurfaces );
			if ( Found > 0 ) {
				SurfaceTmp( SurfNum ).BaseSurf = Found;
				SurfaceTmp( SurfNum ).ExtBoundCond = SurfaceTmp( Found ).ExtBoundCond;
				SurfaceTmp( SurfNum ).ExtBoundCondName = SurfaceTmp( Found ).ExtBoundCondName;
				SurfaceTmp( SurfNum ).ExtSolar = SurfaceTmp( Found ).ExtSolar;
				SurfaceTmp( SurfNum ).ExtWind = SurfaceTmp( Found ).ExtWind;
				SurfaceTmp( SurfNum ).Zone = SurfaceTmp( Found ).Zone;
				SurfaceTmp( SurfNum ).ZoneName = SurfaceTmp( Found ).ZoneName;
				SurfaceTmp( SurfNum ).OSCPtr = SurfaceTmp( Found ).OSCPtr;
				if ( SurfaceTmp( Found ).ExtBoundCond == UnreconciledZoneSurface && SurfaceTmp( Found ).ExtBoundCondName == SurfaceTmp( Found ).Name ) { // Adiabatic surface, no windows or doors allowed
					ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", invalid " + cAlphaFieldNames( 4 ) + "=\"" + cAlphaArgs( 4 ) + "\"." );
					ShowContinueError( "... adiabatic surfaces cannot have windows or doors." );
					ShowContinueError( "... no solar transmission will result for these windows or doors. You must have interior windows or doors on Interzone surfaces for transmission to result." );
				}
			} else {
				ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", invalid " + cAlphaFieldNames( 4 ) + "=\"" + cAlphaArgs( 4 ) );
				SurfaceTmp( SurfNum ).ZoneName = "Unknown Zone";
				ErrorsFound = true;
			}

			if ( SurfaceTmp( SurfNum ).Class == SurfaceClass_TDD_Dome || SurfaceTmp( SurfNum ).Class == SurfaceClass_TDD_Diffuser ) {
				SurfaceTmp( SurfNum ).ExtBoundCond = ExternalEnvironment;
			}

			if ( SurfaceTmp( SurfNum ).ExtBoundCond == ExternalEnvironment ) {
				if ( ! lAlphaFieldBlanks( 5 ) ) {
					ShowWarningError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", invalid field " + cAlphaFieldNames( 5 ) );
					ShowContinueError( "...when Base surface uses \"Outdoors\" as " + cAlphaFieldNames( 5 ) + ", subsurfaces need to be blank to inherit the outdoor characteristics." );
					ShowContinueError( "...Surface external characteristics changed to reflect base surface." );
				}
			}

			if ( SurfaceTmp( SurfNum ).ExtBoundCond == UnreconciledZoneSurface ) { // "Surface" Base Surface
				if ( ! lAlphaFieldBlanks( 5 ) ) {
					SurfaceTmp( SurfNum ).ExtBoundCondName = cAlphaArgs( 5 );
				} else {
					ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", invalid blank " + cAlphaFieldNames( 5 ) );
					ShowContinueError( "...when Base surface uses \"Surface\" as " + cAlphaFieldNames( 5 ) + ", subsurfaces must also specify specific surfaces in the adjacent zone." );
					SurfaceTmp( SurfNum ).ExtBoundCondName = cAlphaArgs( 5 ); // putting it as blank will not confuse things later.
					ErrorsFound = true;
				}
			}

			if ( SurfaceTmp( SurfNum ).ExtBoundCond == UnenteredAdjacentZoneSurface ) { // "Zone" - unmatched interior surface
				++NeedToAddSurfaces;
				// ignoring window5datafiles for now -- will need to add.
			}

			if ( SurfaceTmp( SurfNum ).ExtBoundCond == OtherSideCoefNoCalcExt || SurfaceTmp( SurfNum ).ExtBoundCond == OtherSideCoefCalcExt ) {
				if ( ! lAlphaFieldBlanks( 5 ) ) { // Otherside Coef special Name
					Found = FindItemInList( cAlphaArgs( 5 ), OSC, TotOSC );
					if ( Found == 0 ) {
						ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", invalid " + cAlphaFieldNames( 5 ) + "=\"" + cAlphaArgs( 5 ) + "\"." );
						ShowContinueError( "...base surface requires that this subsurface have OtherSideCoefficients -- not found." );
						ErrorsFound = true;
					} else { // found
						// The following allows for a subsurface that has different characteristics than
						// the base surface with OtherSide Coeff -- do we want that or is it an error?
						SurfaceTmp( SurfNum ).OSCPtr = Found;
						SurfaceTmp( SurfNum ).ExtBoundCondName = cAlphaArgs( 5 );
						if ( OSC( Found ).SurfFilmCoef > 0.0 ) {
							SurfaceTmp( SurfNum ).ExtBoundCond = OtherSideCoefCalcExt;
						} else {
							SurfaceTmp( SurfNum ).ExtBoundCond = OtherSideCoefNoCalcExt;
						}
					}
				}
			}

			if ( SurfaceTmp( SurfNum ).ExtBoundCond == OtherSideCondModeledExt ) {
				SurfaceTmp( SurfNum ).ExtBoundCond = ExternalEnvironment;
			}

			if ( SurfaceTmp( SurfNum ).ExtBoundCondName == BlankString ) {
				SurfaceTmp( SurfNum ).ExtBoundCondName = SurfaceTmp( SurfNum ).Name;
			}
			SurfaceTmp( SurfNum ).ViewFactorGround = rNumericArgs( 1 );
			if ( lNumericFieldBlanks( 1 ) ) SurfaceTmp( SurfNum ).ViewFactorGround = AutoCalculate;

			if ( lNumericFieldBlanks( 3 ) || rNumericArgs( 3 ) == AutoCalculate ) {
				rNumericArgs( 3 ) = ( SurfaceNumProp - 3 ) / 3;
				SurfaceTmp( SurfNum ).Sides = rNumericArgs( 3 );
				if ( mod( SurfaceNumProp - 3, 3 ) != 0 ) {
					ShowWarningError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", " + cNumericFieldNames( 3 ) + " not even multiple of 3. Will read in " + TrimSigDigits( SurfaceTmp( SurfNum ).Sides ) );
				}
				if ( rNumericArgs( 3 ) < 3 ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", " + cNumericFieldNames( 3 ) + " (autocalculate) must be >= 3. Only " + TrimSigDigits( SurfaceTmp( SurfNum ).Sides ) + " provided." );
					ErrorsFound = true;
					continue;
				}
			} else {
				numSides = ( SurfaceNumProp - 2 ) / 3;
				SurfaceTmp( SurfNum ).Sides = rNumericArgs( 3 );
				if ( numSides > SurfaceTmp( SurfNum ).Sides ) {
					ShowWarningError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", field " + cNumericFieldNames( 3 ) + '=' + TrimSigDigits( SurfaceTmp( SurfNum ).Sides ) );
					ShowContinueError( "...but " + TrimSigDigits( numSides ) + " were entered. Only the indicated " + cNumericFieldNames( 3 ) + " will be used." );
				}
			}
			SurfaceTmp( SurfNum ).Vertex.allocate( SurfaceTmp( SurfNum ).Sides );
			if ( SurfaceTmp( SurfNum ).Class == SurfaceClass_Window || SurfaceTmp( SurfNum ).Class == SurfaceClass_GlassDoor || SurfaceTmp( SurfNum ).Class == SurfaceClass_Door ) SurfaceTmp( SurfNum ).Multiplier = int( rNumericArgs( 2 ) );
			// Only windows, glass doors and doors can have Multiplier > 1:
			if ( ( SurfaceTmp( SurfNum ).Class != SurfaceClass_Window && SurfaceTmp( SurfNum ).Class != SurfaceClass_GlassDoor && SurfaceTmp( SurfNum ).Class != SurfaceClass_Door ) && rNumericArgs( 2 ) > 1.0 ) {
				ShowWarningError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", invalid " + cNumericFieldNames( 2 ) + "=[" + TrimSigDigits( rNumericArgs( 2 ), 1 ) + "]." );
				ShowContinueError( "...because " + cAlphaFieldNames( 2 ) + '=' + cAlphaArgs( 2 ) + " multiplier will be set to 1.0." );
				SurfaceTmp( SurfNum ).Multiplier = 1.0;
			}

			GetVertices( SurfNum, SurfaceTmp( SurfNum ).Sides, rNumericArgs( {4,_} ) );

			CheckConvexity( SurfNum, SurfaceTmp( SurfNum ).Sides );
			SurfaceTmp( SurfNum ).WindowShadingControlPtr = 0;

			if ( SurfaceTmp( SurfNum ).Class == SurfaceClass_Window || SurfaceTmp( SurfNum ).Class == SurfaceClass_GlassDoor || SurfaceTmp( SurfNum ).Class == SurfaceClass_TDD_Diffuser || SurfaceTmp( SurfNum ).Class == SurfaceClass_TDD_Dome ) {

				if ( SurfaceTmp( SurfNum ).ExtBoundCond == OtherSideCoefNoCalcExt || SurfaceTmp( SurfNum ).ExtBoundCond == OtherSideCoefCalcExt ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", Other side coefficients are not allowed with windows." );
					ErrorsFound = true;
				}

				if ( SurfaceTmp( SurfNum ).ExtBoundCond == Ground ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", Exterior boundary condition = Ground is not be allowed with windows." );
					ErrorsFound = true;
				}

				if ( ! cAlphaArgs( 6 ).empty() ) {
					if ( TotWinShadingControl > 0 ) {
						SurfaceTmp( SurfNum ).WindowShadingControlPtr = FindItemInList( cAlphaArgs( 6 ), WindowShadingControl, TotWinShadingControl );
					}
					if ( SurfaceTmp( SurfNum ).WindowShadingControlPtr == 0 ) {
						ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", invalid " + cAlphaFieldNames( 6 ) + "=\"" + cAlphaArgs( 6 ) + "\"." );
						ErrorsFound = true;
					}

					// Error if this is not an exterior window and shading device has been specified
					// PETER: should doors be disallowed too?
					if ( SurfaceTmp( SurfNum ).WindowShadingControlPtr > 0 && SurfaceTmp( SurfNum ).ExtBoundCond != ExternalEnvironment ) {

						ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", invalid " + cAlphaFieldNames( 6 ) + " because it is not an exterior window." );
						ErrorsFound = true;

					} else if ( Construct( SurfaceTmp( SurfNum ).Construction ).WindowTypeEQL && SurfaceTmp( SurfNum ).WindowShadingControlPtr > 0 ) {

						ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", invalid " + cAlphaFieldNames( 6 ) + "=\"" + cAlphaArgs( 6 ) + "\"." );
						ShowContinueError( ".. equivalent layer window model does not use shading control object." );
						ShowContinueError( ".. Shading control is set to none or zero, and simulation continues." );
						SurfaceTmp( SurfNum ).WindowShadingControlPtr = 0;
					}
				}

				CheckWindowShadingControlFrameDivider( "GetHTSubSurfaceData", ErrorsFound, SurfNum, 7 );

				if ( SurfaceTmp( SurfNum ).Sides == 3 ) { // Triangular window
					if ( ! cAlphaArgs( 7 ).empty() ) {
						ShowWarningError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", invalid " + cAlphaFieldNames( 7 ) + "=\"" + cAlphaArgs( 7 ) + "\"." );
						ShowContinueError( ".. because it is a triangular window and cannot have a frame or divider or reveal reflection." );
						ShowContinueError( "Frame, divider and reveal reflection will be ignored for this window." );
					}
					SurfaceTmp( SurfNum ).FrameDivider = 0;
				} // End of check if window is triangular or rectangular

			} // check on non-opaquedoor subsurfaces

			CheckSubSurfaceMiscellaneous( "GetHTSubSurfaceData", ErrorsFound, SurfNum, cAlphaArgs( 1 ), cAlphaArgs( 3 ), AddedSubSurfaces );

		} // End of main loop over subsurfaces

	}

	void
	GetRectSubSurfaces(
		bool & ErrorsFound, // Error flag indicator (true if errors found)
		int & SurfNum, // Count of Current SurfaceNumber
		int const TotWindows, // Number of Window SubSurfaces to obtain
		int const TotDoors, // Number of Door SubSurfaces to obtain
		int const TotGlazedDoors, // Number of Glass Door SubSurfaces to obtain
		int const TotIZWindows, // Number of Interzone Window SubSurfaces to obtain
		int const TotIZDoors, // Number of Interzone Door SubSurfaces to obtain
		int const TotIZGlazedDoors, // Number of Interzone Glass Door SubSurfaces to obtain
		Array1S_int const SubSurfIDs, // ID Assignments for valid sub surface classes
		int & AddedSubSurfaces, // Subsurfaces added when windows reference Window5
		int & NeedToAddSubSurfaces // Number of surfaces to add, based on unentered IZ surfaces
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   December 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Get simple (rectangular, relative origin to base surface) windows, doors, glazed doors.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using InputProcessor::VerifyName;
		using InputProcessor::GetObjectDefMaxArgs;
		using General::TrimSigDigits;
		using General::RoundSigDigits;

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		//  data file entry with two glazing systems

		// SUBROUTINE PARAMETER DEFINITIONS:
		static Array1D_string const cModuleObjects( 6, { "Window", "Door", "GlazedDoor", "Window:Interzone", "Door:Interzone", "GlazedDoor:Interzone" } );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Item;
		int ItemsToGet;
		int Loop;
		int NumAlphas;
		int NumNumbers;
		int IOStat; // IO Status when calling get input subroutine
		int Found; // For matching base surfaces
		bool ErrorInName;
		bool IsBlank;
		bool GettingIZSurfaces;
		int WindowShadingField;
		int FrameField;
		int OtherSurfaceField;
		int ClassItem;
		int IZFound;

		for ( Item = 1; Item <= 6; ++Item ) {

			cCurrentModuleObject = cModuleObjects( Item );
			if ( Item == 1 ) {
				ItemsToGet = TotWindows;
				GettingIZSurfaces = false;
				WindowShadingField = 4;
				FrameField = 5;
				OtherSurfaceField = 0;
				ClassItem = 1;
			} else if ( Item == 2 ) {
				ItemsToGet = TotDoors;
				GettingIZSurfaces = false;
				WindowShadingField = 0;
				FrameField = 0;
				OtherSurfaceField = 0;
				ClassItem = 2;
			} else if ( Item == 3 ) {
				ItemsToGet = TotGlazedDoors;
				GettingIZSurfaces = false;
				WindowShadingField = 4;
				FrameField = 5;
				OtherSurfaceField = 0;
				ClassItem = 3;
			} else if ( Item == 4 ) {
				ItemsToGet = TotIZWindows;
				GettingIZSurfaces = true;
				WindowShadingField = 0;
				FrameField = 0;
				OtherSurfaceField = 4;
				ClassItem = 1;
			} else if ( Item == 5 ) {
				ItemsToGet = TotIZDoors;
				GettingIZSurfaces = true;
				WindowShadingField = 0;
				FrameField = 0;
				OtherSurfaceField = 4;
				ClassItem = 2;
			} else { // Item = 6
				ItemsToGet = TotIZGlazedDoors;
				GettingIZSurfaces = true;
				WindowShadingField = 0;
				FrameField = 0;
				OtherSurfaceField = 4;
				ClassItem = 3;
			}

			for ( Loop = 1; Loop <= ItemsToGet; ++Loop ) {
				GetObjectItem( cCurrentModuleObject, Loop, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
				ErrorInName = false;
				IsBlank = false;
				VerifyName( cAlphaArgs( 1 ), SurfaceTmp, SurfNum, ErrorInName, IsBlank, cCurrentModuleObject + " Name" );
				if ( ErrorInName ) {
					ShowContinueError( "...each surface name must not duplicate other surface names (of any type)" );
					ErrorsFound = true;
					continue;
				}

				if ( NumNumbers < 5 ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", Too few number of numeric args=[" + TrimSigDigits( NumNumbers ) + "]." );
					ErrorsFound = true;
				}

				++SurfNum;
				SurfaceTmp( SurfNum ).Name = cAlphaArgs( 1 ); // Set the Surface Name in the Derived Type
				SurfaceTmp( SurfNum ).Class = SubSurfIDs( ClassItem ); // Set class number

				SurfaceTmp( SurfNum ).Construction = FindItemInList( cAlphaArgs( 2 ), Construct, TotConstructs );

				if ( SurfaceTmp( SurfNum ).Construction == 0 ) {
					ErrorsFound = true;
					ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\"." );
				} else {
					Construct( SurfaceTmp( SurfNum ).Construction ).IsUsed = true;
					SurfaceTmp( SurfNum ).ConstructionStoredInputValue = SurfaceTmp( SurfNum ).Construction;
				}

				if ( SurfaceTmp( SurfNum ).Class == SurfaceClass_Window || SurfaceTmp( SurfNum ).Class == SurfaceClass_GlassDoor ) {

					if ( SurfaceTmp( SurfNum ).Construction != 0 ) {
						if ( ! Construct( SurfaceTmp( SurfNum ).Construction ).TypeIsWindow ) {
							ErrorsFound = true;
							ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\" has an opaque surface construction; it should have a window construction." );
						}
					}

				} else if ( SurfaceTmp( SurfNum ).Construction != 0 ) {
					if ( Construct( SurfaceTmp( SurfNum ).Construction ).TypeIsWindow ) {
						ErrorsFound = true;
						ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\" - has Window materials." );
					}
				}

				SurfaceTmp( SurfNum ).HeatTransSurf = true;

				SurfaceTmp( SurfNum ).BaseSurfName = cAlphaArgs( 3 );
				//  The subsurface inherits properties from the base surface
				//  Exterior conditions, Zone, etc.
				//  We can figure out the base surface though, because they've all been entered
				Found = FindItemInList( SurfaceTmp( SurfNum ).BaseSurfName, SurfaceTmp, TotSurfaces );
				if ( Found > 0 ) {
					SurfaceTmp( SurfNum ).BaseSurf = Found;
					SurfaceTmp( SurfNum ).ExtBoundCond = SurfaceTmp( Found ).ExtBoundCond;
					SurfaceTmp( SurfNum ).ExtBoundCondName = SurfaceTmp( Found ).ExtBoundCondName;
					SurfaceTmp( SurfNum ).ExtSolar = SurfaceTmp( Found ).ExtSolar;
					SurfaceTmp( SurfNum ).ExtWind = SurfaceTmp( Found ).ExtWind;
					SurfaceTmp( SurfNum ).Tilt = SurfaceTmp( Found ).Tilt;
					SurfaceTmp( SurfNum ).Azimuth = SurfaceTmp( Found ).Azimuth;
					SurfaceTmp( SurfNum ).Zone = SurfaceTmp( Found ).Zone;
					SurfaceTmp( SurfNum ).ZoneName = SurfaceTmp( Found ).ZoneName;
					SurfaceTmp( SurfNum ).OSCPtr = SurfaceTmp( Found ).OSCPtr;
					SurfaceTmp( SurfNum ).ViewFactorGround = SurfaceTmp( Found ).ViewFactorGround;
					SurfaceTmp( SurfNum ).ViewFactorSky = SurfaceTmp( Found ).ViewFactorSky;
				} else {
					ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", invalid " + cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 3 ) );
					SurfaceTmp( SurfNum ).ZoneName = "Unknown Zone";
					ErrorsFound = true;
					continue;
				}
				if ( SurfaceTmp( Found ).ExtBoundCond == UnreconciledZoneSurface && SurfaceTmp( Found ).ExtBoundCondName == SurfaceTmp( Found ).Name ) { // Adiabatic surface, no windows or doors allowed
					ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", invalid " + cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 3 ) + "\"." );
					ShowContinueError( "... adiabatic surfaces cannot have windows or doors." );
					ShowContinueError( "... no solar transmission will result for these windows or doors. You must have interior windows or doors on Interzone surfaces for transmission to result." );
				}

				if ( SurfaceTmp( SurfNum ).ExtBoundCond == UnreconciledZoneSurface ) { // "Surface" Base Surface
					if ( ! GettingIZSurfaces ) {
						ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", invalid use of object" );
						ShowContinueError( "...when Base surface uses \"Surface\" as " + cAlphaFieldNames( 5 ) + ", subsurfaces must also specify specific surfaces in the adjacent zone." );
						ShowContinueError( "...Please use " + cCurrentModuleObject + ":Interzone to enter this surface." );
						SurfaceTmp( SurfNum ).ExtBoundCondName = BlankString; // putting it as blank will not confuse things later.
						ErrorsFound = true;
					}
				}

				if ( SurfaceTmp( SurfNum ).ExtBoundCond == UnreconciledZoneSurface ) { // "Surface" Base Surface
					if ( GettingIZSurfaces ) {
						SurfaceTmp( SurfNum ).ExtBoundCondName = cAlphaArgs( OtherSurfaceField );
						IZFound = FindItemInList( SurfaceTmp( SurfNum ).ExtBoundCondName, Zone, NumOfZones );
						if ( IZFound > 0 ) SurfaceTmp( SurfNum ).ExtBoundCond = UnenteredAdjacentZoneSurface;
					} else { // Interior Window
						SurfaceTmp( SurfNum ).ExtBoundCondName = SurfaceTmp( SurfNum ).Name;
					}
				}

				// This is the parent's property:
				if ( SurfaceTmp( SurfNum ).ExtBoundCond == UnenteredAdjacentZoneSurface ) { // OtherZone - unmatched interior surface
					if ( GettingIZSurfaces ) {
						++NeedToAddSubSurfaces;
					} else { // Interior Window
						ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", invalid Interzone Surface, specify " + cCurrentModuleObject + ":InterZone" );
						ShowContinueError( "...when base surface is an interzone surface, subsurface must also be an interzone surface." );
						++NeedToAddSubSurfaces;
						ErrorsFound = true;
					}
				}

				if ( GettingIZSurfaces ) {
					if ( lAlphaFieldBlanks( OtherSurfaceField ) ) {
						// blank -- set it up for unentered adjacent zone
						if ( SurfaceTmp( SurfNum ).ExtBoundCond == UnenteredAdjacentZoneSurface ) { // already set but need Zone
							SurfaceTmp( SurfNum ).ExtBoundCondName = SurfaceTmp( Found ).ExtBoundCondName; // base surface has it
						} else if ( SurfaceTmp( SurfNum ).ExtBoundCond == UnreconciledZoneSurface ) {
							SurfaceTmp( SurfNum ).ExtBoundCondName = SurfaceTmp( Found ).ZoneName; // base surface has it
							SurfaceTmp( SurfNum ).ExtBoundCond = UnenteredAdjacentZoneSurface;
						} else { // not correct boundary condition for interzone subsurface
							ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", invalid Base Surface type for Interzone Surface" );
							ShowContinueError( "...when base surface is not an interzone surface, subsurface must also not be an interzone surface." );
							ErrorsFound = true;
						}
					}
				}

				if ( SurfaceTmp( SurfNum ).ExtBoundCond == OtherSideCondModeledExt ) {
					SurfaceTmp( SurfNum ).ExtBoundCond = ExternalEnvironment;
				}

				//      SurfaceTmp(SurfNum)%ViewFactorGround = AutoCalculate

				SurfaceTmp( SurfNum ).Sides = 4;
				SurfaceTmp( SurfNum ).Vertex.allocate( SurfaceTmp( SurfNum ).Sides );
				if ( SurfaceTmp( SurfNum ).Class == SurfaceClass_Window || SurfaceTmp( SurfNum ).Class == SurfaceClass_GlassDoor || SurfaceTmp( SurfNum ).Class == SurfaceClass_Door ) SurfaceTmp( SurfNum ).Multiplier = int( rNumericArgs( 1 ) );
				// Only windows, glass doors and doors can have Multiplier > 1:
				if ( ( SurfaceTmp( SurfNum ).Class != SurfaceClass_Window && SurfaceTmp( SurfNum ).Class != SurfaceClass_GlassDoor && SurfaceTmp( SurfNum ).Class != SurfaceClass_Door ) && rNumericArgs( 1 ) > 1.0 ) {
					ShowWarningError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", invalid " + cNumericFieldNames( 1 ) + "=[" + TrimSigDigits( rNumericArgs( 1 ), 1 ) + "]." );
					ShowContinueError( "...because " + cAlphaFieldNames( 1 ) + '=' + cAlphaArgs( 1 ) + " multiplier will be set to 1.0." );
					SurfaceTmp( SurfNum ).Multiplier = 1.0;
				}

				MakeRelativeRectangularVertices( SurfaceTmp( SurfNum ).BaseSurf, SurfNum, rNumericArgs( 2 ), rNumericArgs( 3 ), rNumericArgs( 4 ), rNumericArgs( 5 ) );

				if ( SurfaceTmp( SurfNum ).Area <= 0.0 ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", Surface Area <= 0.0; Entered Area=" + TrimSigDigits( SurfaceTmp( SurfNum ).Area, 2 ) );
					ErrorsFound = true;
				}

				SurfaceTmp( SurfNum ).WindowShadingControlPtr = 0;

				if ( ! GettingIZSurfaces && ( SurfaceTmp( SurfNum ).Class == SurfaceClass_Window || SurfaceTmp( SurfNum ).Class == SurfaceClass_GlassDoor ) ) {

					if ( SurfaceTmp( SurfNum ).ExtBoundCond == OtherSideCoefNoCalcExt || SurfaceTmp( SurfNum ).ExtBoundCond == OtherSideCoefCalcExt ) {
						ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", Other side coefficients are not allowed with windows." );
						ErrorsFound = true;
					}

					if ( SurfaceTmp( SurfNum ).ExtBoundCond == Ground ) {
						ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", Exterior boundary condition = Ground is not be allowed with windows." );
						ErrorsFound = true;
					}

					if ( ! cAlphaArgs( WindowShadingField ).empty() ) {
						if ( TotWinShadingControl > 0 ) {
							SurfaceTmp( SurfNum ).WindowShadingControlPtr = FindItemInList( cAlphaArgs( WindowShadingField ), WindowShadingControl, TotWinShadingControl );
						}
						if ( SurfaceTmp( SurfNum ).WindowShadingControlPtr == 0 ) {
							ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", invalid " + cAlphaFieldNames( WindowShadingField ) + "=\"" + cAlphaArgs( WindowShadingField ) + "\"." );
							ErrorsFound = true;
						}

						// Error if this is not an exterior window and shading device has been specified
						// PETER: should doors be disallowed too?
						if ( SurfaceTmp( SurfNum ).WindowShadingControlPtr > 0 && SurfaceTmp( SurfNum ).ExtBoundCond != ExternalEnvironment ) {

							ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", invalid " + cAlphaFieldNames( WindowShadingField ) + " because it is not an exterior window." );
							ErrorsFound = true;
						}
					}

					CheckWindowShadingControlFrameDivider( "GetRectSubSurfaces", ErrorsFound, SurfNum, FrameField );

				} // check on non-opaquedoor subsurfaces

				CheckSubSurfaceMiscellaneous( "GetRectSubSurfaces", ErrorsFound, SurfNum, cAlphaArgs( 1 ), cAlphaArgs( 2 ), AddedSubSurfaces );

			} // Getting Items

		}

	}

	void
	CheckWindowShadingControlFrameDivider(
		std::string const & cRoutineName, // routine name calling this one (for error messages)
		bool & ErrorsFound, // true if errors have been found or are found here
		int const SurfNum, // current surface number
		int const FrameField // field number for frame/divider
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   December 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine performs checks on WindowShadingControl settings and Frame/Divider Settings.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using InputProcessor::FindItemInList;
		using General::TrimSigDigits;
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int WSCPtr; // WindowShadingControl Index
		int ConstrNumSh; // Construction number with Shade
		int ConstrNum; // Construction number
		int ShDevNum; // Shading Device number
		int Lay; // Layer number
		int TotGlassLayers; // Number of glass layers in window construction
		int TotLayers; // Number of layers in unshaded construction
		int TotShLayers; // Number of layers in shaded construction
		int MatGap; // Gap material number
		int MatGap1; // Material number of gap to left (outer side) of between-glass shade/blind
		int MatGap2; // Material number of gap to right (inner side) of between-glass shade/blind
		int MatSh; // Between-glass shade/blind material number
		Real64 MatGapCalc; // Calculated MatGap diff for shaded vs non-shaded constructions

		// If WindowShadingControl has been specified for this window --
		// Set shaded construction number if shaded construction was specified in WindowShadingControl.
		// Otherwise, create shaded construction if WindowShadingControl for this window has
		// interior or exterior shade/blind (but not between-glass shade/blind) specified.

		WSCPtr = SurfaceTmp( SurfNum ).WindowShadingControlPtr;
		ConstrNumSh = 0;
		if ( ! ErrorsFound && WSCPtr > 0 ) {
			ConstrNumSh = WindowShadingControl( WSCPtr ).ShadedConstruction;
			if ( ConstrNumSh > 0 ) {
				SurfaceTmp( SurfNum ).ShadedConstruction = ConstrNumSh;
			} else {
				if ( WindowShadingControl( WSCPtr ).ShadingType == WSC_ST_InteriorShade || WindowShadingControl( WSCPtr ).ShadingType == WSC_ST_InteriorBlind || WindowShadingControl( WSCPtr ).ShadingType == WSC_ST_ExteriorShade || WindowShadingControl( WSCPtr ).ShadingType == WSC_ST_ExteriorScreen || WindowShadingControl( WSCPtr ).ShadingType == WSC_ST_ExteriorBlind ) {
					ShDevNum = WindowShadingControl( WSCPtr ).ShadingDevice;
					if ( ShDevNum > 0 ) {
						CreateShadedWindowConstruction( SurfNum, WSCPtr, ShDevNum );
						ConstrNumSh = SurfaceTmp( SurfNum ).ShadedConstruction;
					}
				}
			}
		}

		// Error checks for shades and blinds

		ConstrNum = SurfaceTmp( SurfNum ).Construction;
		if ( ! ErrorsFound && WSCPtr > 0 && ConstrNum > 0 && ConstrNumSh > 0 ) {

			if ( WindowShadingControl( WSCPtr ).ShadingType == WSC_ST_InteriorShade || WindowShadingControl( WSCPtr ).ShadingType == WSC_ST_InteriorBlind ) {
				TotLayers = Construct( ConstrNum ).TotLayers;
				TotShLayers = Construct( ConstrNumSh ).TotLayers;
				if ( TotShLayers - 1 != TotLayers ) {
					ShowWarningError( "WindowProperty:ShadingControl: Interior shade or blind: Potential problem in match of unshaded/shaded constructions, shaded should have 1 more layers than unshaded." );
					ShowContinueError( "Unshaded construction=" + Construct( ConstrNum ).Name );
					ShowContinueError( "Shaded construction=" + Construct( ConstrNumSh ).Name );
					ShowContinueError( "If preceding two constructions are same name, you have likely specified a WindowProperty:ShadingControl (Field #3) with the Window Construction rather than a shaded construction." );
				}
				for ( Lay = 1; Lay <= Construct( ConstrNum ).TotLayers; ++Lay ) {
					if ( Construct( ConstrNum ).LayerPoint( Lay ) != Construct( ConstrNumSh ).LayerPoint( Lay ) ) {
						ErrorsFound = true;
						ShowSevereError( " The glass and gas layers in the shaded and unshaded constructions do not match for window=" + SurfaceTmp( SurfNum ).Name );
						ShowContinueError( "Unshaded construction=" + Construct( ConstrNum ).Name );
						ShowContinueError( "Shaded construction=" + Construct( ConstrNumSh ).Name );
						break;
					}
				}
			}

			if ( WindowShadingControl( WSCPtr ).ShadingType == WSC_ST_ExteriorShade || WindowShadingControl( WSCPtr ).ShadingType == WSC_ST_ExteriorScreen || WindowShadingControl( WSCPtr ).ShadingType == WSC_ST_ExteriorBlind ) {
				TotLayers = Construct( ConstrNum ).TotLayers;
				TotShLayers = Construct( ConstrNumSh ).TotLayers;
				if ( TotShLayers - 1 != TotLayers ) {
					ShowWarningError( "WindowProperty:ShadingControl: Exterior shade, screen or blind: Potential problem in match of unshaded/shaded constructions, shaded should have 1 more layer than unshaded." );
					ShowContinueError( "Unshaded construction=" + Construct( ConstrNum ).Name );
					ShowContinueError( "Shaded construction=" + Construct( ConstrNumSh ).Name );
					ShowContinueError( "If preceding two constructions have the same name, you have likely specified a WindowProperty:ShadingControl (Field #3) with the Window Construction rather than a shaded construction." );
				}
				for ( Lay = 1; Lay <= Construct( ConstrNum ).TotLayers; ++Lay ) {
					if ( Construct( ConstrNum ).LayerPoint( Lay ) != Construct( ConstrNumSh ).LayerPoint( Lay + 1 ) ) {
						ErrorsFound = true;
						ShowSevereError( " The glass and gas layers in the shaded and unshaded constructions do not match for window=" + SurfaceTmp( SurfNum ).Name );
						ShowContinueError( "Unshaded construction=" + Construct( ConstrNum ).Name );
						ShowContinueError( "Shaded construction=" + Construct( ConstrNumSh ).Name );
						break;
					}
				}
			}

			if ( WindowShadingControl( WSCPtr ).ShadingType == WSC_ST_BetweenGlassShade || WindowShadingControl( WSCPtr ).ShadingType == WSC_ST_BetweenGlassBlind ) {
				// Divider not allowed with between-glass shade or blind
				if ( SurfaceTmp( SurfNum ).FrameDivider > 0 ) {
					if ( FrameDivider( SurfaceTmp( SurfNum ).FrameDivider ).DividerWidth > 0.0 ) {
						ShowWarningError( "A divider cannot be specified for window " + SurfaceTmp( SurfNum ).Name );
						ShowContinueError( ", which has a between-glass shade or blind." );
						ShowContinueError( "Calculation will proceed without the divider for this window." );
						FrameDivider( SurfaceTmp( SurfNum ).FrameDivider ).DividerWidth = 0.0;
					}
				}
				// Check consistency of gap widths between unshaded and shaded constructions
				TotGlassLayers = Construct( ConstrNum ).TotGlassLayers;
				TotLayers = Construct( ConstrNum ).TotLayers;
				TotShLayers = Construct( ConstrNumSh ).TotLayers;
				if ( TotShLayers - 2 != TotLayers ) {
					ShowWarningError( "WindowProperty:ShadingControl: Between Glass Shade/Blind: Potential problem in match of unshaded/shaded constructions, shaded should have 2 more layers than unshaded." );
					ShowContinueError( "Unshaded construction=" + Construct( ConstrNum ).Name );
					ShowContinueError( "Shaded construction=" + Construct( ConstrNumSh ).Name );
					ShowContinueError( "If preceding two constructions are same name, you have likely specified a WindowProperty:ShadingControl (Field #3) with the Window Construction rather than a shaded construction." );
				}
				if ( Construct( ConstrNum ).LayerPoint( TotLayers ) != Construct( ConstrNumSh ).LayerPoint( TotShLayers ) ) {
					ShowSevereError( cRoutineName + ": Mis-match in unshaded/shaded inside layer materials.  These should match." );
					ShowContinueError( "Unshaded construction=" + Construct( ConstrNum ).Name + ", Material=" + Material( Construct( ConstrNum ).LayerPoint( TotLayers ) ).Name );
					ShowContinueError( "Shaded construction=" + Construct( ConstrNumSh ).Name + ", Material=" + Material( Construct( ConstrNumSh ).LayerPoint( TotShLayers ) ).Name );
					ErrorsFound = true;
				}
				if ( Construct( ConstrNum ).LayerPoint( 1 ) != Construct( ConstrNumSh ).LayerPoint( 1 ) ) {
					ShowSevereError( cRoutineName + ": Mis-match in unshaded/shaded inside layer materials.  These should match." );
					ShowContinueError( "Unshaded construction=" + Construct( ConstrNum ).Name + ", Material=" + Material( Construct( ConstrNum ).LayerPoint( 1 ) ).Name );
					ShowContinueError( "Shaded construction=" + Construct( ConstrNumSh ).Name + ", Material=" + Material( Construct( ConstrNumSh ).LayerPoint( 1 ) ).Name );
					ErrorsFound = true;
				}
				if ( TotGlassLayers == 2 || TotGlassLayers == 3 ) {
					MatGap = Construct( ConstrNum ).LayerPoint( 2 * TotGlassLayers - 2 );
					MatGap1 = Construct( ConstrNumSh ).LayerPoint( 2 * TotGlassLayers - 2 );
					MatGap2 = Construct( ConstrNumSh ).LayerPoint( 2 * TotGlassLayers );
					MatSh = Construct( ConstrNumSh ).LayerPoint( 2 * TotGlassLayers - 1 );
					if ( WindowShadingControl( WSCPtr ).ShadingType == WSC_ST_BetweenGlassBlind ) {
						MatGapCalc = std::abs( Material( MatGap ).Thickness - ( Material( MatGap1 ).Thickness + Material( MatGap2 ).Thickness ) );
						if ( MatGapCalc > 0.001 ) {
							ShowSevereError( cRoutineName + ": The gap width(s) for the unshaded window construction " + Construct( ConstrNum ).Name );
							ShowContinueError( "are inconsistent with the gap widths for shaded window construction " + Construct( ConstrNumSh ).Name );
							ShowContinueError( "for window " + SurfaceTmp( SurfNum ).Name + ", which has a between-glass blind." );
							ShowContinueError( "..Material=" + Material( MatGap ).Name + " thickness=" + RoundSigDigits( Material( MatGap ).Thickness, 3 ) + " -" );
							ShowContinueError( "..( Material=" + Material( MatGap1 ).Name + " thickness=" + RoundSigDigits( Material( MatGap1 ).Thickness, 3 ) + " +" );
							ShowContinueError( "..Material=" + Material( MatGap2 ).Name + " thickness=" + RoundSigDigits( Material( MatGap2 ).Thickness, 3 ) + " )=[" + RoundSigDigits( MatGapCalc, 3 ) + "] >.001" );
							ErrorsFound = true;
						}
					} else { // Between-glass shade
						MatGapCalc = std::abs( Material( MatGap ).Thickness - ( Material( MatGap1 ).Thickness + Material( MatGap2 ).Thickness + Material( MatSh ).Thickness ) );
						if ( MatGapCalc > 0.001 ) {
							ShowSevereError( cRoutineName + ": The gap width(s) for the unshaded window construction " + Construct( ConstrNum ).Name );
							ShowContinueError( "are inconsistent with the gap widths for shaded window construction " + Construct( ConstrNumSh ).Name );
							ShowContinueError( "for window " + SurfaceTmp( SurfNum ).Name + ", which has a between-glass shade." );
							ShowContinueError( "..Material=" + Material( MatGap ).Name + " thickness=" + RoundSigDigits( Material( MatGap ).Thickness, 3 ) + " -" );
							ShowContinueError( "...( Material=" + Material( MatGap1 ).Name + " thickness=" + RoundSigDigits( Material( MatGap1 ).Thickness, 3 ) + " +" );
							ShowContinueError( "..Material=" + Material( MatGap2 ).Name + " thickness=" + RoundSigDigits( Material( MatGap2 ).Thickness, 3 ) + " +" );
							ShowContinueError( "..Material=" + Material( MatSh ).Name + " thickness=" + RoundSigDigits( Material( MatSh ).Thickness, 3 ) + " )=[" + RoundSigDigits( MatGapCalc, 3 ) + "] >.001" );
							ErrorsFound = true;
						}
					}
				}
			}
		}

		if ( SurfaceTmp( SurfNum ).Sides != 3 ) { // Rectangular Window
			// Initialize the FrameDivider number for this window. W5FrameDivider will be positive if
			// this window's construction came from the Window5 data file and that construction had an
			// associated frame or divider. It will be zero if the window's construction is not from the
			// Window5 data file, or the construction is from the data file, but the construction has no
			// associated frame or divider. Note that if there is a FrameDivider candidate for this
			// window from the Window5 data file it is used instead of the window's input FrameDivider.

			if ( SurfaceTmp( SurfNum ).Construction != 0 ) {
				SurfaceTmp( SurfNum ).FrameDivider = Construct( SurfaceTmp( SurfNum ).Construction ).W5FrameDivider;

				// Warning if FrameAndDivider for this window is over-ridden by one from Window5 Data File
				if ( SurfaceTmp( SurfNum ).FrameDivider > 0 && ! lAlphaFieldBlanks( FrameField ) ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", " + cAlphaFieldNames( FrameField ) + "=\"" + cAlphaArgs( FrameField ) + "\"" );
					ShowContinueError( "will be replaced with FrameAndDivider from Window5 Data File entry " + Construct( SurfaceTmp( SurfNum ).Construction ).Name );
				}

				if ( ! lAlphaFieldBlanks( FrameField ) && SurfaceTmp( SurfNum ).FrameDivider == 0 ) {
					SurfaceTmp( SurfNum ).FrameDivider = FindItemInList( cAlphaArgs( FrameField ), FrameDivider );
					if ( SurfaceTmp( SurfNum ).FrameDivider == 0 ) {
						if ( ! Construct( SurfaceTmp( SurfNum ).Construction ).WindowTypeEQL ) {
							ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", invalid " + cAlphaFieldNames( FrameField ) + "=\"" + cAlphaArgs( FrameField ) + "\"" );
							ErrorsFound = true;
						} else {
							ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", invalid " + cAlphaFieldNames( FrameField ) + "=\"" + cAlphaArgs( FrameField ) + "\"" );
							ShowContinueError( "...Frame/Divider is not supported in Equivalent Layer Window model." );
						}
					}
					// Divider not allowed with between-glass shade or blind
					if ( ! ErrorsFound && WSCPtr > 0 && ConstrNumSh > 0 ) {
						if ( WindowShadingControl( WSCPtr ).ShadingType == WSC_ST_BetweenGlassShade || WindowShadingControl( WSCPtr ).ShadingType == WSC_ST_BetweenGlassBlind ) {
							if ( SurfaceTmp( SurfNum ).FrameDivider > 0 ) {
								if ( FrameDivider( SurfaceTmp( SurfNum ).FrameDivider ).DividerWidth > 0.0 ) {
									ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", invalid " + cAlphaFieldNames( FrameField ) + "=\"" + cAlphaArgs( FrameField ) + "\"" );
									ShowContinueError( "Divider cannot be specified because the construction has a between-glass shade or blind." );
									ShowContinueError( "Calculation will proceed without the divider for this window." );
									ShowContinueError( "Divider width = [" + RoundSigDigits( FrameDivider( SurfaceTmp( SurfNum ).FrameDivider ).DividerWidth, 2 ) + "]." );
									FrameDivider( SurfaceTmp( SurfNum ).FrameDivider ).DividerWidth = 0.0;
								}
							} // End of check if window has divider
						} // End of check if window has a between-glass shade or blind
					} // End of check if window has a shaded construction
				} // End of check if window has an associated FrameAndDivider
			} // End of check if window has a construction
		}

		if ( Construct( SurfaceTmp( SurfNum ).Construction ).WindowTypeEQL ) {
			if ( SurfaceTmp( SurfNum ).FrameDivider > 0 ) {
				// Equivalent Layer window does not have frame/divider model
				ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", invalid " + cAlphaFieldNames( FrameField ) + "=\"" + cAlphaArgs( FrameField ) + "\"" );
				ShowContinueError( "Frame/Divider is not supported in Equivalent Layer Window model." );
				SurfaceTmp( SurfNum ).FrameDivider = 0;
			}
		}

	}

	void
	CheckSubSurfaceMiscellaneous(
		std::string const & cRoutineName, // routine name calling this one (for error messages)
		bool & ErrorsFound, // true if errors have been found or are found here
		int const SurfNum, // current surface number
		std::string const & SubSurfaceName, // name of the surface
		std::string const & SubSurfaceConstruction, // name of the construction
		int & AddedSubSurfaces
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   December 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine performs miscellaneous checks on subsurfaces: Windows, GlassDoors, Doors, Tubular Devices.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		//  USE DataIPShortCuts
		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using General::TrimSigDigits;
		using General::RoundSigDigits;
		using namespace DataErrorTracking;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumShades; // count on number of shading layers
		int Lay; // Layer number
		int LayerPtr; // Layer pointer
		int ConstrNum; // Construction number
		int Found; // when item is found

		// Warning if window has multiplier > 1 and SolarDistribution = FullExterior or FullInteriorExterior

		if ( ( SurfaceTmp( SurfNum ).Class == SurfaceClass_Window || SurfaceTmp( SurfNum ).Class == SurfaceClass_GlassDoor ) && SolarDistribution > MinimalShadowing && SurfaceTmp( SurfNum ).Multiplier > 1.0 ) {
			if ( DisplayExtraWarnings ) {
				ShowWarningError( cRoutineName + ": A Multiplier > 1.0 for window/glass door " + SurfaceTmp( SurfNum ).Name );
				ShowContinueError( "in conjunction with SolarDistribution = FullExterior or FullInteriorExterior" );
				ShowContinueError( "can cause inaccurate shadowing on the window and/or" );
				ShowContinueError( "inaccurate interior solar distribution from the window." );
			}
			++TotalMultipliedWindows;
		}

		//  Require that a construction referenced by a surface that is a window
		//  NOT have a shading device layer; use WindowShadingControl to specify a shading device.
		ConstrNum = SurfaceTmp( SurfNum ).Construction;
		if ( ConstrNum > 0 ) {
			NumShades = 0;
			for ( Lay = 1; Lay <= Construct( ConstrNum ).TotLayers; ++Lay ) {
				LayerPtr = Construct( ConstrNum ).LayerPoint( Lay );
				if ( LayerPtr == 0 ) continue; // Error is caught already, will terminate later
				if ( Material( LayerPtr ).Group == Shade || Material( LayerPtr ).Group == WindowBlind || Material( LayerPtr ).Group == Screen ) ++NumShades;
			}
			if ( NumShades != 0 ) {
				ShowSevereError( cRoutineName + ": Window \"" + SubSurfaceName + "\" must not directly reference" );
				ShowContinueError( "a Construction (i.e, \"" + SubSurfaceConstruction + "\") with a shading device." );
				ShowContinueError( "Use WindowProperty:ShadingControl to specify a shading device for a window." );
				ErrorsFound = true;
			}
		}

		// Disallow glass transmittance dirt factor for interior windows and glass doors

		if ( SurfaceTmp( SurfNum ).ExtBoundCond != ExternalEnvironment && ( SurfaceTmp( SurfNum ).Class == SurfaceClass_Window || SurfaceTmp( SurfNum ).Class == SurfaceClass_GlassDoor ) ) {
			ConstrNum = SurfaceTmp( SurfNum ).Construction;
			if ( ConstrNum > 0 ) {
				for ( Lay = 1; Lay <= Construct( ConstrNum ).TotLayers; ++Lay ) {
					LayerPtr = Construct( ConstrNum ).LayerPoint( Lay );
					if ( Material( LayerPtr ).Group == WindowGlass && Material( LayerPtr ).GlassTransDirtFactor < 1.0 ) {
						ShowSevereError( cRoutineName + ": Interior Window or GlassDoor " + SubSurfaceName + " has a glass layer with" );
						ShowContinueError( "Dirt Correction Factor for Solar and Visible Transmittance < 1.0" );
						ShowContinueError( "A value less than 1.0 for this factor is only allowed for exterior windows and glass doors." );
						ErrorsFound = true;
					}
				}
			}
		}

		// If this is a window with a construction from the Window5DataFile, call routine that will
		// (1) if one glazing system on Data File, give warning message if window height or width
		//     differ by more than 10% from those of the glazing system on the Data File;
		// (2) if two glazing systems (separated by a mullion) on Data File, create a second window
		//     and adjust the dimensions of the original and second windows to those on the Data File

		if ( SurfaceTmp( SurfNum ).Construction != 0 ) {

			if ( Construct( SurfaceTmp( SurfNum ).Construction ).FromWindow5DataFile ) {

				ModifyWindow( SurfNum, ErrorsFound, AddedSubSurfaces );

			} else {
				// Calculate net area for base surface (note that ModifyWindow, above, adjusts net area of
				// base surface for case where window construction is from Window5 Data File
				// In case there is in error in this window's base surface (i.e. none)..
				if ( SurfaceTmp( SurfNum ).BaseSurf > 0 ) {
					SurfaceTmp( SurfaceTmp( SurfNum ).BaseSurf ).Area -= SurfaceTmp( SurfNum ).Area;

					// Subtract TDD:DIFFUSER area from other side interzone surface
					if ( ( SurfaceTmp( SurfNum ).Class == SurfaceClass_TDD_Diffuser ) && not_blank( SurfaceTmp( SurfaceTmp( SurfNum ).BaseSurf ).ExtBoundCondName ) ) { // Base surface is an interzone surface

						// Lookup interzone surface of the base surface
						// (Interzone surfaces have not been assigned yet, but all base surfaces should already be loaded.)
						Found = FindItemInList( SurfaceTmp( SurfaceTmp( SurfNum ).BaseSurf ).ExtBoundCondName, SurfaceTmp, SurfNum );
						if ( Found != 0 ) SurfaceTmp( Found ).Area -= SurfaceTmp( SurfNum ).Area;
					}

					if ( SurfaceTmp( SurfaceTmp( SurfNum ).BaseSurf ).Area <= 0.0 ) {
						ShowSevereError( cRoutineName + ": Surface Openings have too much area for base surface=" + SurfaceTmp( SurfaceTmp( SurfNum ).BaseSurf ).Name );
						ShowContinueError( "Opening Surface creating error=" + SurfaceTmp( SurfNum ).Name );
						ErrorsFound = true;
					}
					// Net area of base surface with unity window multipliers (used in shadowing checks)
					// For Windows, Glass Doors and Doors, just one area is subtracted.  For the rest, should be
					// full area.
					if ( SurfaceTmp( SurfNum ).Class == SurfaceClass_Window || SurfaceTmp( SurfNum ).Class == SurfaceClass_GlassDoor ) {
						SurfaceTmp( SurfaceTmp( SurfNum ).BaseSurf ).NetAreaShadowCalc -= SurfaceTmp( SurfNum ).Area / SurfaceTmp( SurfNum ).Multiplier;
					} else if ( SurfaceTmp( SurfNum ).Class == SurfaceClass_Door ) { // Door, TDD:Diffuser, TDD:DOME
						SurfaceTmp( SurfaceTmp( SurfNum ).BaseSurf ).NetAreaShadowCalc -= SurfaceTmp( SurfNum ).Area / SurfaceTmp( SurfNum ).Multiplier;
					} else {
						SurfaceTmp( SurfaceTmp( SurfNum ).BaseSurf ).NetAreaShadowCalc -= SurfaceTmp( SurfNum ).Area;
					}
				}
			}
		}

	}

	void
	MakeRelativeRectangularVertices(
		int const BaseSurfNum, // Base surface
		int const SurfNum,
		Real64 const XCoord,
		Real64 const ZCoord,
		Real64 const Length,
		Real64 const Height
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   December 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine creates world/3d coordinates for rectangular surfaces using relative X and Z, length & height.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace Vectors;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 SurfAzimuth; // Surface Azimuth/Facing (same as Base Surface)
		Real64 SurfTilt; // Tilt (same as Base Surface)
		Real64 XLLC;
		Real64 YLLC;
		Real64 ZLLC;
		Real64 CosSurfAzimuth;
		Real64 SinSurfAzimuth;
		Real64 CosSurfTilt;
		Real64 SinSurfTilt;
		Real64 BaseCosSurfAzimuth;
		Real64 BaseSinSurfAzimuth;
		Real64 BaseCosSurfTilt;
		Real64 BaseSinSurfTilt;
		Array1D< Real64 > XX( 4 );
		Array1D< Real64 > YY( 4 );
		Real64 Perimeter;
		int n;
		int Vrt;

		if ( BaseSurfNum == 0 ) return; // invalid base surface, don't bother

		// Tilt and Facing (Azimuth) will be same as the Base Surface

		SurfaceTmp( SurfNum ).Height = Height;
		SurfaceTmp( SurfNum ).Width = Length;

		SurfAzimuth = SurfaceTmp( SurfNum ).Azimuth;
		SurfTilt = SurfaceTmp( SurfNum ).Tilt;
		CosSurfAzimuth = std::cos( SurfAzimuth * DegToRadians );
		SinSurfAzimuth = std::sin( SurfAzimuth * DegToRadians );
		CosSurfTilt = std::cos( SurfTilt * DegToRadians );
		SinSurfTilt = std::sin( SurfTilt * DegToRadians );
		BaseCosSurfAzimuth = SurfaceTmp( BaseSurfNum ).CosAzim;
		BaseSinSurfAzimuth = SurfaceTmp( BaseSurfNum ).SinAzim;
		BaseCosSurfTilt = SurfaceTmp( BaseSurfNum ).CosTilt;
		BaseSinSurfTilt = SurfaceTmp( BaseSurfNum ).SinTilt;

		XLLC = SurfaceTmp( BaseSurfNum ).Vertex( 2 ).x - XCoord * BaseCosSurfAzimuth - ZCoord * BaseCosSurfTilt * BaseSinSurfAzimuth;
		YLLC = SurfaceTmp( BaseSurfNum ).Vertex( 2 ).y + XCoord * BaseSinSurfAzimuth - ZCoord * BaseCosSurfTilt * BaseCosSurfAzimuth;
		ZLLC = SurfaceTmp( BaseSurfNum ).Vertex( 2 ).z + ZCoord * BaseSinSurfTilt;

		XX( 1 ) = 0.0;
		XX( 2 ) = 0.0;
		XX( 3 ) = Length;
		XX( 4 ) = Length;
		YY( 1 ) = Height;
		YY( 4 ) = Height;
		YY( 3 ) = 0.0;
		YY( 2 ) = 0.0;

		for ( n = 1; n <= SurfaceTmp( SurfNum ).Sides; ++n ) {
			Vrt = n;
			SurfaceTmp( SurfNum ).Vertex( Vrt ).x = XLLC - XX( n ) * CosSurfAzimuth - YY( n ) * CosSurfTilt * SinSurfAzimuth;
			SurfaceTmp( SurfNum ).Vertex( Vrt ).y = YLLC + XX( n ) * SinSurfAzimuth - YY( n ) * CosSurfTilt * CosSurfAzimuth;
			SurfaceTmp( SurfNum ).Vertex( Vrt ).z = ZLLC + YY( n ) * SinSurfTilt;
		}

		CreateNewellAreaVector( SurfaceTmp( SurfNum ).Vertex, SurfaceTmp( SurfNum ).Sides, SurfaceTmp( SurfNum ).NewellAreaVector );
		SurfaceTmp( SurfNum ).GrossArea = VecLength( SurfaceTmp( SurfNum ).NewellAreaVector );
		SurfaceTmp( SurfNum ).Area = SurfaceTmp( SurfNum ).GrossArea;
		SurfaceTmp( SurfNum ).NetAreaShadowCalc = SurfaceTmp( SurfNum ).Area;
		CreateNewellSurfaceNormalVector( SurfaceTmp( SurfNum ).Vertex, SurfaceTmp( SurfNum ).Sides, SurfaceTmp( SurfNum ).NewellSurfaceNormalVector );
		DetermineAzimuthAndTilt( SurfaceTmp( SurfNum ).Vertex, SurfaceTmp( SurfNum ).Sides, SurfAzimuth, SurfTilt, SurfaceTmp( SurfNum ).lcsx, SurfaceTmp( SurfNum ).lcsy, SurfaceTmp( SurfNum ).lcsz, SurfaceTmp( SurfNum ).GrossArea, SurfaceTmp( SurfNum ).NewellSurfaceNormalVector );
		SurfaceTmp( SurfNum ).Azimuth = SurfAzimuth;
		SurfaceTmp( SurfNum ).Tilt = SurfTilt;
		// Sine and cosine of azimuth and tilt
		SurfaceTmp( SurfNum ).SinAzim = SinSurfAzimuth;
		SurfaceTmp( SurfNum ).CosAzim = CosSurfAzimuth;
		SurfaceTmp( SurfNum ).SinTilt = SinSurfTilt;
		SurfaceTmp( SurfNum ).CosTilt = CosSurfTilt;
		if ( SurfaceTmp( SurfNum ).Class != SurfaceClass_Window && SurfaceTmp( SurfNum ).Class != SurfaceClass_GlassDoor && SurfaceTmp( SurfNum ).Class != SurfaceClass_Door ) SurfaceTmp( SurfNum ).ViewFactorGround = 0.5 * ( 1.0 - SurfaceTmp( SurfNum ).CosTilt );
		// Outward normal unit vector (pointing away from room)
		SurfaceTmp( SurfNum ).OutNormVec = SurfaceTmp( SurfNum ).NewellSurfaceNormalVector;
		for ( n = 1; n <= 3; ++n ) {
			if ( std::abs( SurfaceTmp( SurfNum ).OutNormVec( n ) - 1.0 ) < 1.e-06 ) SurfaceTmp( SurfNum ).OutNormVec( n ) = +1.0;
			if ( std::abs( SurfaceTmp( SurfNum ).OutNormVec( n ) + 1.0 ) < 1.e-06 ) SurfaceTmp( SurfNum ).OutNormVec( n ) = -1.0;
			if ( std::abs( SurfaceTmp( SurfNum ).OutNormVec( n ) ) < 1.e-06 ) SurfaceTmp( SurfNum ).OutNormVec( n ) = 0.0;
		}

		//  IF (SurfaceTmp(SurfNum)%Class == SurfaceClass_Roof .and. SurfTilt > 80.) THEN
		//    WRITE(TiltString,'(F5.1)') SurfTilt
		//    TiltString=ADJUSTL(TiltString)
		//    CALL ShowWarningError('Roof/Ceiling Tilt='//TRIM(TiltString)//', much greater than expected tilt of 0,'// &
		//                          ' for Surface='//TRIM(SurfaceTmp(SurfNum)%Name)//  &
		//                          ', in Zone='//TRIM(SurfaceTmp(SurfNum)%ZoneName))
		//  ENDIF
		//  IF (SurfaceTmp(SurfNum)%Class == SurfaceClass_Floor .and. SurfTilt < 170.) THEN
		//    WRITE(TiltString,'(F5.1)') SurfTilt
		//    TiltString=ADJUSTL(TiltString)
		//    CALL ShowWarningError('Floor Tilt='//TRIM(TiltString)//', much less than expected tilt of 180,'//   &
		//                          ' for Surface='//TRIM(SurfaceTmp(SurfNum)%Name)//  &
		//                          ', in Zone='//TRIM(SurfaceTmp(SurfNum)%ZoneName))
		//  ENDIF
		if ( SurfaceTmp( SurfNum ).Class == SurfaceClass_Window || SurfaceTmp( SurfNum ).Class == SurfaceClass_GlassDoor || SurfaceTmp( SurfNum ).Class == SurfaceClass_Door ) SurfaceTmp( SurfNum ).Area *= SurfaceTmp( SurfNum ).Multiplier;
		// Can perform tests on this surface here
		SurfaceTmp( SurfNum ).ViewFactorSky = 0.5 * ( 1.0 + SurfaceTmp( SurfNum ).CosTilt );
		// The following IR view factors are modified in subr. SkyDifSolarShading if there are shadowing
		// surfaces
		SurfaceTmp( SurfNum ).ViewFactorSkyIR = SurfaceTmp( SurfNum ).ViewFactorSky;
		SurfaceTmp( SurfNum ).ViewFactorGroundIR = 0.5 * ( 1.0 - SurfaceTmp( SurfNum ).CosTilt );

		Perimeter = distance( SurfaceTmp( SurfNum ).Vertex( SurfaceTmp( SurfNum ).Sides ), SurfaceTmp( SurfNum ).Vertex( 1 ) );
		for ( Vrt = 2; Vrt <= SurfaceTmp( SurfNum ).Sides; ++Vrt ) {
			Perimeter += distance( SurfaceTmp( SurfNum ).Vertex( Vrt ), SurfaceTmp( SurfNum ).Vertex( Vrt - 1 ) );
		}
		SurfaceTmp( SurfNum ).Perimeter = Perimeter;

		// Call to transform vertices

		TransformVertsByAspect( SurfNum, SurfaceTmp( SurfNum ).Sides );

	}

	void
	GetAttShdSurfaceData(
		bool & ErrorsFound, // Error flag indicator (true if errors found)
		int & SurfNum, // Count of Current SurfaceNumber
		int const TotShdSubs // Number of Attached Shading SubSurfaces to obtain
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   May 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine gets the HeatTransfer Surface Data,
		// checks it for errors, etc.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		//  Attached Shading Surface Definition
		//Surface:Shading:Attached,
		//       \memo used For fins, overhangs, elements that shade the building, are attached to the building
		//       \memo but are not part of the heat transfer calculations
		//  A1 , \field User Supplied Surface Name
		//       \required-field
		//       \type alpha
		//       \reference AttachedShadingSurfNames
		//  A2 , \field Base Surface Name
		//       \required-field
		//       \type object-list
		//       \object-list SurfaceNames
		//  A3,  \field TransSchedShadowSurf
		//       \note Transmittance schedule for the shading device, defaults to zero (always opaque)
		//       \type object-list
		//       \object-list ScheduleNames
		//  N1 , \field Number of Surface Vertex Groups -- Number of (X,Y,Z) groups in this surface
		//       \required-field
		//       \note currently limited 3 or 4, later?
		//       \minimum 3
		//       \maximum 4
		//       \note vertices are given in SurfaceGeometry coordinates -- if relative, all surface coordinates
		//       \note are "relative" to the Zone Origin.  if WCS, then building and zone origins are used
		//       \note for some internal calculations, but all coordinates are given in an "absolute" system.
		//  N2,  \field Vertex 1 X-coordinate
		//       \units m
		//       \type real
		//  N3-13; as indicated by the N2 value

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using InputProcessor::VerifyName;
		using InputProcessor::GetObjectDefMaxArgs;
		using ScheduleManager::GetScheduleIndex;
		using ScheduleManager::CheckScheduleValueMinMax;
		using ScheduleManager::GetScheduleMinValue;
		using ScheduleManager::GetScheduleMaxValue;
		using General::TrimSigDigits;
		using namespace DataReportingFlags;

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
		int NumAlphas; // Number of alpha names being passed
		int NumNumbers; // Number of properties being passed
		int Found; // For matching interzone surfaces
		int Loop;
		bool ErrorInName;
		bool IsBlank;
		Real64 SchedMinValue;
		Real64 SchedMaxValue;

		if ( TotShdSubs > 0 && SolarDistribution == MinimalShadowing ) {
			ShowWarningError( "Shading effects of Fins and Overhangs are ignored when Solar Distribution = MinimalShadowing" );
		}

		cCurrentModuleObject = "Shading:Zone:Detailed";
		GetObjectDefMaxArgs( cCurrentModuleObject, Loop, NumAlphas, NumNumbers );
		if ( NumAlphas != 3 ) {
			ShowSevereError( cCurrentModuleObject + ": Object Definition indicates not = 3 Alpha Objects, Number Indicated=" + TrimSigDigits( NumAlphas ) );
			ErrorsFound = true;
		}

		for ( Loop = 1; Loop <= TotShdSubs; ++Loop ) {
			GetObjectItem( cCurrentModuleObject, Loop, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			ErrorInName = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), SurfaceTmp, SurfNum, ErrorInName, IsBlank, cCurrentModuleObject + " Name" );
			if ( ErrorInName ) {
				ShowContinueError( "...each surface name must not duplicate other surface names (of any type)" );
				ErrorsFound = true;
				continue;
			}

			++SurfNum;
			SurfaceTmp( SurfNum ).Name = cAlphaArgs( 1 ); // Set the Surface Name in the Derived Type
			SurfaceTmp( SurfNum ).Class = SurfaceClass_Shading;
			SurfaceTmp( SurfNum ).HeatTransSurf = false;
			SurfaceTmp( SurfNum ).BaseSurfName = cAlphaArgs( 2 );
			//  The subsurface inherits properties from the base surface
			//  Exterior conditions, Zone, etc.
			//  We can figure out the base surface though, because they've all been entered
			Found = FindItemInList( SurfaceTmp( SurfNum ).BaseSurfName, SurfaceTmp, TotSurfaces );
			if ( Found > 0 ) {
				//SurfaceTmp(SurfNum)%BaseSurf=Found
				SurfaceTmp( SurfNum ).ExtBoundCond = SurfaceTmp( Found ).ExtBoundCond;
				SurfaceTmp( SurfNum ).ExtSolar = SurfaceTmp( Found ).ExtSolar;
				SurfaceTmp( SurfNum ).ExtWind = SurfaceTmp( Found ).ExtWind;
				SurfaceTmp( SurfNum ).Zone = SurfaceTmp( Found ).Zone; // Necessary to do relative coordinates in GetVertices below
				SurfaceTmp( SurfNum ).ZoneName = SurfaceTmp( Found ).ZoneName; // Necessary to have surface drawn in OutputReports
			} else {
				ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) );
				ErrorsFound = true;
			}
			if ( SurfaceTmp( SurfNum ).ExtBoundCond == UnenteredAdjacentZoneSurface ) {
				ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) );
				ShowContinueError( "...trying to attach a shading device to an interzone surface." );
				ErrorsFound = true;
				SurfaceTmp( SurfNum ).ExtBoundCond = ExternalEnvironment; // reset so program won't crash during "add surfaces"
			}
			if ( SurfaceTmp( SurfNum ).ExtBoundCond == UnreconciledZoneSurface ) {
				ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) );
				ShowContinueError( "...trying to attach a shading device to an interior surface." );
				ErrorsFound = true;
				SurfaceTmp( SurfNum ).ExtBoundCond = ExternalEnvironment; // reset so program won't crash during "add surfaces"
			}

			if ( ! lAlphaFieldBlanks( 3 ) ) {
				SurfaceTmp( SurfNum ).SchedShadowSurfIndex = GetScheduleIndex( cAlphaArgs( 3 ) );
				if ( SurfaceTmp( SurfNum ).SchedShadowSurfIndex == 0 ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", " + cAlphaFieldNames( 3 ) + " not found=\"" + cAlphaArgs( 3 ) );
					ErrorsFound = true;
				}
			} else {
				SurfaceTmp( SurfNum ).SchedShadowSurfIndex = 0;
			}
			if ( SurfaceTmp( SurfNum ).SchedShadowSurfIndex != 0 ) {
				if ( ! CheckScheduleValueMinMax( SurfaceTmp( SurfNum ).SchedShadowSurfIndex, ">=", 0.0, "<=", 1.0 ) ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", " + cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 3 ) + "\", values not in range [0,1]." );
					ErrorsFound = true;
				}
				SchedMinValue = GetScheduleMinValue( SurfaceTmp( SurfNum ).SchedShadowSurfIndex );
				SurfaceTmp( SurfNum ).SchedMinValue = SchedMinValue;
				SchedMaxValue = GetScheduleMaxValue( SurfaceTmp( SurfNum ).SchedShadowSurfIndex );
				if ( SchedMinValue == 1.0 ) {
					ShowWarningError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\", is always transparent." );
					SurfaceTmp( SurfNum ).IsTransparent = true;
				}
				if ( SchedMinValue < 0.0 ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\", has schedule values < 0." );
					ShowContinueError( "...Schedule values < 0 have no meaning for shading elements." );
				}
				if ( SchedMaxValue > 1.0 ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\", has schedule values > 1." );
					ShowContinueError( "...Schedule values > 1 have no meaning for shading elements." );
				}
				if ( std::abs( SchedMinValue - SchedMaxValue ) > 1.0e-6 ) {
					SurfaceTmp( SurfNum ).ShadowSurfSchedVaries = true;
					ShadingTransmittanceVaries = true;
				}
			}
			if ( lNumericFieldBlanks( 1 ) || rNumericArgs( 1 ) == AutoCalculate ) {
				rNumericArgs( 1 ) = ( NumNumbers - 1 ) / 3;
				SurfaceTmp( SurfNum ).Sides = rNumericArgs( 1 );
				if ( mod( NumNumbers - 1, 3 ) != 0 ) {
					ShowWarningError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", " + cNumericFieldNames( 1 ) + " not even multiple of 3. Will read in " + TrimSigDigits( SurfaceTmp( SurfNum ).Sides ) );
				}
				if ( rNumericArgs( 1 ) < 3 ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", " + cNumericFieldNames( 1 ) + " (autocalculate) must be >= 3. Only " + TrimSigDigits( SurfaceTmp( SurfNum ).Sides ) + " provided." );
					ErrorsFound = true;
					continue;
				}
			} else {
				SurfaceTmp( SurfNum ).Sides = rNumericArgs( 1 );
			}
			SurfaceTmp( SurfNum ).Vertex.allocate( SurfaceTmp( SurfNum ).Sides );
			GetVertices( SurfNum, SurfaceTmp( SurfNum ).Sides, rNumericArgs( {2,_} ) );
			CheckConvexity( SurfNum, SurfaceTmp( SurfNum ).Sides );
			//    IF (SurfaceTmp(SurfNum)%Sides == 3) THEN
			//      CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
			//                        ' should not be triangular.')
			//      CALL ShowContinueError('...Check results carefully.')
			//      ErrorsFound=.TRUE.
			//    ENDIF
			// Reset surface to be "detached"
			SurfaceTmp( SurfNum ).BaseSurf = 0;
			//    SurfaceTmp(SurfNum)%BaseSurfName='  '
			SurfaceTmp( SurfNum ).Zone = 0;
			//SurfaceTmp(SurfNum)%ZoneName='  '
			if ( MakeMirroredAttachedShading ) {
				MakeMirrorSurface( SurfNum );
			}

		}

	}

	void
	GetSimpleShdSurfaceData(
		bool & ErrorsFound, // Error flag indicator (true if errors found)
		int & SurfNum, // Count of Current SurfaceNumber
		int const TotOverhangs, // Number of Overhangs to obtain
		int const TotOverhangsProjection, // Number of Overhangs (projection) to obtain
		int const TotFins, // Number of Fins to obtain
		int const TotFinsProjection // Number of Fins (projection) to obtain
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   January 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Get simple overhang and fin descriptions.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using InputProcessor::VerifyName;
		using InputProcessor::GetObjectDefMaxArgs;
		using General::TrimSigDigits;
		using General::RoundSigDigits;
		using namespace DataReportingFlags;
		using namespace Vectors;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static Array1D_string const cModuleObjects( 4, { "Shading:Overhang", "Shading:Overhang:Projection", "Shading:Fin", "Shading:Fin:Projection" } );
		static gio::Fmt dfmt( "(A,3(2x,f6.2))" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Item;
		int ItemsToGet;
		int Loop;
		int NumAlphas;
		int NumNumbers;
		int IOStat; // IO Status when calling get input subroutine
		int Found; // For matching base surfaces
		bool ErrorInName;
		bool IsBlank;
		Real64 Depth;
		Real64 Length;
		Real64 Xp;
		Real64 Yp;
		Real64 Zp;
		Real64 XLLC;
		Real64 YLLC;
		int BaseSurfNum;
		Real64 TiltAngle;
		bool MakeFin;

		if ( ( TotOverhangs + TotOverhangsProjection + TotFins + TotFinsProjection ) > 0 && SolarDistribution == MinimalShadowing ) {
			ShowWarningError( "Shading effects of Fins and Overhangs are ignored when Solar Distribution = MinimalShadowing" );
		}

		for ( Item = 1; Item <= 4; ++Item ) {

			cCurrentModuleObject = cModuleObjects( Item );
			if ( Item == 1 ) {
				ItemsToGet = TotOverhangs;
			} else if ( Item == 2 ) {
				ItemsToGet = TotOverhangsProjection;
			} else if ( Item == 3 ) {
				ItemsToGet = TotFins;
			} else { // ! (Item == 4) THEN
				ItemsToGet = TotFinsProjection;
			}

			for ( Loop = 1; Loop <= ItemsToGet; ++Loop ) {
				GetObjectItem( cCurrentModuleObject, Loop, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
				ErrorInName = false;
				IsBlank = false;
				VerifyName( cAlphaArgs( 1 ), SurfaceTmp, SurfNum, ErrorInName, IsBlank, cCurrentModuleObject + " Name" );
				if ( ErrorInName ) {
					ShowContinueError( "...each surface name must not duplicate other surface names (of any type)" );
					ErrorsFound = true;
					continue;
				}

				++SurfNum;
				SurfaceTmp( SurfNum ).Name = cAlphaArgs( 1 ); // Set the Surface Name in the Derived Type
				SurfaceTmp( SurfNum ).Class = SurfaceClass_Shading;
				SurfaceTmp( SurfNum ).HeatTransSurf = false;
				// this object references a window or door....
				Found = FindItemInList( cAlphaArgs( 2 ), SurfaceTmp, TotSurfaces );
				if ( Found > 0 ) {
					BaseSurfNum = SurfaceTmp( Found ).BaseSurf;
					SurfaceTmp( SurfNum ).BaseSurfName = SurfaceTmp( Found ).BaseSurfName;
					SurfaceTmp( SurfNum ).ExtBoundCond = SurfaceTmp( Found ).ExtBoundCond;
					SurfaceTmp( SurfNum ).ExtSolar = SurfaceTmp( Found ).ExtSolar;
					SurfaceTmp( SurfNum ).ExtWind = SurfaceTmp( Found ).ExtWind;
					SurfaceTmp( SurfNum ).Zone = SurfaceTmp( Found ).Zone; // Necessary to do relative coordinates in GetVertices below
					SurfaceTmp( SurfNum ).ZoneName = SurfaceTmp( Found ).ZoneName; // Necessary to have surface drawn in OutputReports
				} else {
					ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) );
					ErrorsFound = true;
					continue;
				}
				if ( SurfaceTmp( SurfNum ).ExtBoundCond == UnenteredAdjacentZoneSurface ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) );
					ShowContinueError( "...trying to attach a shading device to an interzone surface." );
					ErrorsFound = true;
					SurfaceTmp( SurfNum ).ExtBoundCond = ExternalEnvironment; // reset so program won't crash during "add surfaces"
				}
				if ( SurfaceTmp( SurfNum ).ExtBoundCond == UnreconciledZoneSurface ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) );
					ShowContinueError( "...trying to attach a shading device to an interior surface." );
					ErrorsFound = true;
					SurfaceTmp( SurfNum ).ExtBoundCond = ExternalEnvironment; // reset so program won't crash during "add surfaces"
				}

				SurfaceTmp( SurfNum ).SchedShadowSurfIndex = 0;

				//===== Overhang =====

				if ( Item < 3 ) {
					//  Found is the surface window or door.
					//   N1,  \field Height above Window or Door
					//        \units m
					//   N2,  \field Tilt Angle from Window/Door
					//        \units deg
					//        \default 90
					//        \minimum 0
					//        \maximum 180
					//   N3,  \field Left extension from Window/Door Width
					//        \units m
					//   N4,  \field Right extension from Window/Door Width
					//        \note N3 + N4 + Window/Door Width is Overhang Length
					//        \units m
					//   N5;  \field Depth
					//        \units m
					// for projection option:
					//   N5;  \field Depth as Fraction of Window/Door Height
					//        \units m
					Length = rNumericArgs( 3 ) + rNumericArgs( 4 ) + SurfaceTmp( Found ).Width;
					if ( Item == 1 ) {
						Depth = rNumericArgs( 5 );
					} else if ( Item == 2 ) {
						Depth = rNumericArgs( 5 ) * SurfaceTmp( Found ).Height;
					}

					if ( Length * Depth <= 0.0 ) {
						ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", illegal surface area=[" + RoundSigDigits( Length * Depth, 2 ) + "]. Surface will NOT be entered." );
						continue;
					}

					TiltAngle = SurfaceTmp( Found ).Tilt + rNumericArgs( 2 );
					SurfaceTmp( SurfNum ).Tilt = TiltAngle;
					SurfaceTmp( SurfNum ).Azimuth = SurfaceTmp( Found ).Azimuth;

					// Make it relative to surface origin.....
					Xp = SurfaceTmp( Found ).Vertex( 2 ).x - SurfaceTmp( BaseSurfNum ).Vertex( 2 ).x;
					Yp = SurfaceTmp( Found ).Vertex( 2 ).y - SurfaceTmp( BaseSurfNum ).Vertex( 2 ).y;
					Zp = SurfaceTmp( Found ).Vertex( 2 ).z - SurfaceTmp( BaseSurfNum ).Vertex( 2 ).z;

					XLLC = -Xp * SurfaceTmp( BaseSurfNum ).CosAzim + Yp * SurfaceTmp( BaseSurfNum ).SinAzim;

					YLLC = -Xp * SurfaceTmp( BaseSurfNum ).SinAzim * SurfaceTmp( BaseSurfNum ).CosTilt - Yp * SurfaceTmp( BaseSurfNum ).CosAzim * SurfaceTmp( BaseSurfNum ).CosTilt + Zp * SurfaceTmp( BaseSurfNum ).SinTilt;

					SurfaceTmp( SurfNum ).Sides = 4;
					SurfaceTmp( SurfNum ).Vertex.allocate( SurfaceTmp( SurfNum ).Sides );

					MakeRelativeRectangularVertices( BaseSurfNum, SurfNum, XLLC - rNumericArgs( 3 ), YLLC + SurfaceTmp( Found ).Height + rNumericArgs( 1 ), Length, Depth );

					// Reset surface to be "detached"
					//    SurfaceTmp(SurfNum)%BaseSurfName='  '
					//    SurfaceTmp(SurfNum)%ZoneName='  '

					SurfaceTmp( SurfNum ).BaseSurf = 0;
					SurfaceTmp( SurfNum ).Zone = 0;

					// and mirror
					if ( MakeMirroredAttachedShading ) {
						MakeMirrorSurface( SurfNum );
					}

				} else { // Fins

					//===== Fins =====

					//===== Left Fin =====

					//   N1,  \field Left Extension from Window/Door
					//        \units m
					//   N2,  \field Left Distance Above Top of Window
					//        \units m
					//   N3,  \field Left Distance Below Bottom of Window
					//        \units m
					//        \note N2 + N3 + height of Window/Door is height of Fin
					//   N4,  \field Left Tilt Angle from Window/Door
					//        \units deg
					//        \default 90
					//        \minimum 0
					//        \maximum 180
					//   N5,  \field Left Depth
					//        \units m
					// for projection option:
					//   N5,  \field Left Depth as Fraction of Window/Door Width
					//        \units m
					SurfaceTmp( SurfNum ).Name = SurfaceTmp( SurfNum ).Name + " Left";
					Length = rNumericArgs( 2 ) + rNumericArgs( 3 ) + SurfaceTmp( Found ).Height;
					if ( Item == 3 ) {
						Depth = rNumericArgs( 5 );
					} else if ( Item == 4 ) {
						Depth = rNumericArgs( 5 ) * SurfaceTmp( Found ).Width;
					}

					MakeFin = true;
					if ( Length * Depth <= 0.0 ) {
						ShowWarningError( cCurrentModuleObject + "=Left Fin of \"" + cAlphaArgs( 1 ) + "\", illegal surface area=[" + RoundSigDigits( Length * Depth, 2 ) + "]. Surface will NOT be entered." );
						MakeFin = false;
					}

					if ( MakeFin ) {
						TiltAngle = SurfaceTmp( Found ).Tilt;
						SurfaceTmp( SurfNum ).Tilt = TiltAngle;
						SurfaceTmp( SurfNum ).Azimuth = SurfaceTmp( Found ).Azimuth - ( 180.0 - rNumericArgs( 4 ) );

						// Make it relative to surface origin.....

						Xp = SurfaceTmp( Found ).Vertex( 2 ).x - SurfaceTmp( BaseSurfNum ).Vertex( 2 ).x;
						Yp = SurfaceTmp( Found ).Vertex( 2 ).y - SurfaceTmp( BaseSurfNum ).Vertex( 2 ).y;
						Zp = SurfaceTmp( Found ).Vertex( 2 ).z - SurfaceTmp( BaseSurfNum ).Vertex( 2 ).z;

						XLLC = -Xp * SurfaceTmp( BaseSurfNum ).CosAzim + Yp * SurfaceTmp( BaseSurfNum ).SinAzim;

						YLLC = -Xp * SurfaceTmp( BaseSurfNum ).SinAzim * SurfaceTmp( BaseSurfNum ).CosTilt - Yp * SurfaceTmp( BaseSurfNum ).CosAzim * SurfaceTmp( BaseSurfNum ).CosTilt + Zp * SurfaceTmp( BaseSurfNum ).SinTilt;

						SurfaceTmp( SurfNum ).CosAzim = std::cos( SurfaceTmp( SurfNum ).Azimuth * DegToRadians );
						SurfaceTmp( SurfNum ).SinAzim = std::sin( SurfaceTmp( SurfNum ).Azimuth * DegToRadians );
						SurfaceTmp( SurfNum ).CosTilt = std::cos( SurfaceTmp( SurfNum ).Tilt * DegToRadians );
						SurfaceTmp( SurfNum ).SinTilt = std::sin( SurfaceTmp( SurfNum ).Tilt * DegToRadians );

						SurfaceTmp( SurfNum ).Sides = 4;
						SurfaceTmp( SurfNum ).Vertex.allocate( SurfaceTmp( SurfNum ).Sides );

						MakeRelativeRectangularVertices( BaseSurfNum, SurfNum, XLLC - rNumericArgs( 1 ), YLLC - rNumericArgs( 3 ), -Depth, Length );

						// Reset surface to be "detached"
						//    SurfaceTmp(SurfNum)%BaseSurfName='  '
						//    SurfaceTmp(SurfNum)%ZoneName='  '

						SurfaceTmp( SurfNum ).BaseSurf = 0;
						SurfaceTmp( SurfNum ).Zone = 0;

						// and mirror
						if ( MakeMirroredAttachedShading ) {
							MakeMirrorSurface( SurfNum );
						}
					} else {
						--SurfNum;
					}

					//===== Right Fin =====

					//   N6,  \field Right Extension from Window/Door
					//        \units m
					//   N7,  \field Right Distance Above Top of Window
					//        \units m
					//   N8,  \field Right Distance Below Bottom of Window
					//        \note N7 + N8 + height of Window/Door is height of Fin
					//        \units m
					//   N9,  \field Right Tilt Angle from Window/Door
					//        \units deg
					//        \default 90
					//        \minimum 0
					//        \maximum 180
					//   N10; \field Right Depth
					//        \units m
					// for projection option:
					//   N10; \field Right Depth as Fraction of Window/Door Width
					//        \units m

					++SurfNum;
					SurfaceTmp( SurfNum ).Name = cAlphaArgs( 1 ) + " Right"; // Set the Surface Name in the Derived Type
					SurfaceTmp( SurfNum ).Class = SurfaceClass_Shading;
					SurfaceTmp( SurfNum ).HeatTransSurf = false;
					BaseSurfNum = SurfaceTmp( Found ).BaseSurf;
					SurfaceTmp( SurfNum ).BaseSurfName = SurfaceTmp( Found ).BaseSurfName;
					SurfaceTmp( SurfNum ).ExtBoundCond = SurfaceTmp( Found ).ExtBoundCond;
					SurfaceTmp( SurfNum ).ExtSolar = SurfaceTmp( Found ).ExtSolar;
					SurfaceTmp( SurfNum ).ExtWind = SurfaceTmp( Found ).ExtWind;
					SurfaceTmp( SurfNum ).Zone = SurfaceTmp( Found ).Zone; // Necessary to do relative coordinates in GetVertices below
					SurfaceTmp( SurfNum ).ZoneName = SurfaceTmp( Found ).ZoneName; // Necessary to have surface drawn in OutputReports

					SurfaceTmp( SurfNum ).SchedShadowSurfIndex = 0;
					Length = rNumericArgs( 7 ) + rNumericArgs( 8 ) + SurfaceTmp( Found ).Height;
					if ( Item == 3 ) {
						Depth = rNumericArgs( 10 );
					} else if ( Item == 4 ) {
						Depth = rNumericArgs( 10 ) * SurfaceTmp( Found ).Width;
					}

					MakeFin = true;
					if ( Length * Depth <= 0.0 ) {
						ShowWarningError( cCurrentModuleObject + "=Right Fin of \"" + cAlphaArgs( 1 ) + "\", illegal surface area=[" + RoundSigDigits( Length * Depth, 2 ) + "]. Surface will NOT be entered." );
						MakeFin = false;
					}

					if ( MakeFin ) {
						// Make it relative to surface origin.....

						Xp = SurfaceTmp( Found ).Vertex( 2 ).x - SurfaceTmp( BaseSurfNum ).Vertex( 2 ).x;
						Yp = SurfaceTmp( Found ).Vertex( 2 ).y - SurfaceTmp( BaseSurfNum ).Vertex( 2 ).y;
						Zp = SurfaceTmp( Found ).Vertex( 2 ).z - SurfaceTmp( BaseSurfNum ).Vertex( 2 ).z;

						XLLC = -Xp * SurfaceTmp( BaseSurfNum ).CosAzim + Yp * SurfaceTmp( BaseSurfNum ).SinAzim;

						YLLC = -Xp * SurfaceTmp( BaseSurfNum ).SinAzim * SurfaceTmp( BaseSurfNum ).CosTilt - Yp * SurfaceTmp( BaseSurfNum ).CosAzim * SurfaceTmp( BaseSurfNum ).CosTilt + Zp * SurfaceTmp( BaseSurfNum ).SinTilt;

						TiltAngle = SurfaceTmp( Found ).Tilt;
						SurfaceTmp( SurfNum ).Tilt = TiltAngle;
						SurfaceTmp( SurfNum ).Azimuth = SurfaceTmp( Found ).Azimuth - ( 180.0 - rNumericArgs( 9 ) );
						SurfaceTmp( SurfNum ).CosAzim = std::cos( SurfaceTmp( SurfNum ).Azimuth * DegToRadians );
						SurfaceTmp( SurfNum ).SinAzim = std::sin( SurfaceTmp( SurfNum ).Azimuth * DegToRadians );
						SurfaceTmp( SurfNum ).CosTilt = std::cos( SurfaceTmp( SurfNum ).Tilt * DegToRadians );
						SurfaceTmp( SurfNum ).SinTilt = std::sin( SurfaceTmp( SurfNum ).Tilt * DegToRadians );

						SurfaceTmp( SurfNum ).Sides = 4;
						SurfaceTmp( SurfNum ).Vertex.allocate( SurfaceTmp( SurfNum ).Sides );

						MakeRelativeRectangularVertices( BaseSurfNum, SurfNum, XLLC + SurfaceTmp( Found ).Width + rNumericArgs( 6 ), YLLC - rNumericArgs( 8 ), -Depth, Length );

						// Reset surface to be "detached"
						//    SurfaceTmp(SurfNum)%BaseSurfName='  '
						//    SurfaceTmp(SurfNum)%ZoneName='  '

						SurfaceTmp( SurfNum ).BaseSurf = 0;
						SurfaceTmp( SurfNum ).Zone = 0;

						// and mirror
						if ( MakeMirroredAttachedShading ) {
							MakeMirrorSurface( SurfNum );
						}
					} else {
						--SurfNum;
					}
				}

			}

		}

	}

	void
	GetIntMassSurfaceData(
		bool & ErrorsFound, // Error flag indicator (true if errors found)
		int & SurfNum, // Count of Current SurfaceNumber
		int const TotIntMass // Number of Internal Mass Surfaces to obtain
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   May 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine gets the Internal Surface Data,
		// checks it for errors, etc.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// Internal Mass Surface Definition
		//Surface:HeatTransfer:InternalMass,
		//       \note used to describe internal zone surface area that does not need to be part of geometric representation
		//  A1 , \field User Supplied Surface Name
		//       \type alpha
		//       \reference SurfaceNames
		//  A2 , \field Construction Name of the Surface
		//       \note To be matched with a construction in this input file
		//       \type object-list
		//       \object-list ConstructionNames
		//  A3 , \field Interior Environment
		//       \note Zone the surface is a part of
		//       \type object-list
		//       \object-list ZoneNames
		//  N1,  \field View factor to Person (to people?)
		//       \type real
		//       \note from the interior of the surface
		//  N2 ; \field Surface area
		//       \units m2

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using InputProcessor::VerifyName;
		using namespace Vectors;

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
		int SurfaceNumAlpha; // Number of material alpha names being passed
		int SurfaceNumProp; // Number of material properties being passed
		int ZoneNum; // DO loop counter (zones)
		int Loop;
		bool ErrorInName;
		bool IsBlank;

		cCurrentModuleObject = "InternalMass";
		for ( Loop = 1; Loop <= TotIntMass; ++Loop ) {
			GetObjectItem( cCurrentModuleObject, Loop, cAlphaArgs, SurfaceNumAlpha, rNumericArgs, SurfaceNumProp, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			ErrorInName = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), SurfaceTmp, SurfNum, ErrorInName, IsBlank, cCurrentModuleObject + " Name" );
			if ( ErrorInName ) {
				ShowContinueError( "...each surface name must not duplicate other surface names (of any type)" );
				ErrorsFound = true;
				continue;
			}

			++SurfNum;
			SurfaceTmp( SurfNum ).Name = cAlphaArgs( 1 ); // Set the Surface Name in the Derived Type
			SurfaceTmp( SurfNum ).Class = SurfaceClass_IntMass;
			SurfaceTmp( SurfNum ).HeatTransSurf = true;
			SurfaceTmp( SurfNum ).Construction = FindItemInList( cAlphaArgs( 2 ), Construct, TotConstructs );

			if ( SurfaceTmp( SurfNum ).Construction == 0 ) {
				ErrorsFound = true;
				ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", " + cAlphaFieldNames( 2 ) + " not found=" + cAlphaArgs( 2 ) );
			} else if ( Construct( SurfaceTmp( SurfNum ).Construction ).TypeIsWindow ) {
				ErrorsFound = true;
				ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\" - has Window materials." );
			} else {
				Construct( SurfaceTmp( SurfNum ).Construction ).IsUsed = true;
				SurfaceTmp( SurfNum ).ConstructionStoredInputValue = SurfaceTmp( SurfNum ).Construction;
			}
			SurfaceTmp( SurfNum ).ZoneName = cAlphaArgs( 3 );
			ZoneNum = FindItemInList( SurfaceTmp( SurfNum ).ZoneName, Zone, NumOfZones );

			if ( ZoneNum != 0 ) {
				SurfaceTmp( SurfNum ).Zone = ZoneNum;
			} else {
				ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", invalid " + cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 3 ) + "\"." );
				SurfaceTmp( SurfNum ).Class += 100;
				//      SurfaceTmp(SurfNum)%Class=0
				SurfaceTmp( SurfNum ).ZoneName = "Unknown Zone";
				ErrorsFound = true;
			}
			SurfaceTmp( SurfNum ).GrossArea = rNumericArgs( 1 );
			SurfaceTmp( SurfNum ).Area = SurfaceTmp( SurfNum ).GrossArea;
			SurfaceTmp( SurfNum ).NetAreaShadowCalc = SurfaceTmp( SurfNum ).Area;
			SurfaceTmp( SurfNum ).Width = SurfaceTmp( SurfNum ).Area;
			SurfaceTmp( SurfNum ).Height = 1.0;
			SurfaceTmp( SurfNum ).Tilt = 90.0;
			SurfaceTmp( SurfNum ).CosTilt = 0.0; //Tuned Was std::cos( 90.0 * DegToRadians )
			SurfaceTmp( SurfNum ).SinTilt = 1.0; // Tuned Was std::sin( 90.0 * DegToRadians )
			SurfaceTmp( SurfNum ).Azimuth = 0.0;
			SurfaceTmp( SurfNum ).CosAzim = 1.0; //Tuned Was std::cos( 0.0 )
			SurfaceTmp( SurfNum ).SinAzim = 0.0; //Tuned Was std::sin( 0.0 )
			// Outward normal unit vector (pointing away from room)
			SurfaceTmp( SurfNum ).OutNormVec = SurfaceTmp( SurfNum ).lcsz;
			SurfaceTmp( SurfNum ).ViewFactorSky = 0.5;
			SurfaceTmp( SurfNum ).ExtSolar = false;
			SurfaceTmp( SurfNum ).ExtWind = false;
			SurfaceTmp( SurfNum ).BaseSurf = SurfNum;
			SurfaceTmp( SurfNum ).BaseSurfName = SurfaceTmp( SurfNum ).Name;
			SurfaceTmp( SurfNum ).ExtBoundCondName = SurfaceTmp( SurfNum ).Name;
			SurfaceTmp( SurfNum ).ExtBoundCond = UnreconciledZoneSurface;

		}

	}

	void
	GetShadingSurfReflectanceData( bool & ErrorsFound ) // If errors found in input
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   Sept 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Gets data for a Shading Surface Reflectance object.  This is only called when the
		// Solar Distribution is to be calculated for reflectances.

		// METHODOLOGY EMPLOYED: na
		// REFERENCES: na

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// INTERFACE BLOCK SPECIFICATIONS:na
		// DERIVED TYPE DEFINITIONS:na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int IOStat; // IO Status when calling get input subroutine
		int NumAlpha; // Number of alpha names being passed
		int NumProp; // Number of properties being passed
		int TotShadingSurfaceReflectance; // Total Shading Surface Refleftance statements
		int Loop; // DO loop index
		int SurfNum; // Surface number
		int GlConstrNum; // Glazing construction number
		bool WrongSurfaceType;

		// For shading surfaces, initialize value of reflectance values to default values. These values
		// may be overridden below for shading surfaces with an associated Shading Surface Reflectance object.
		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
			if ( ! ( SurfaceTmp( SurfNum ).Class == SurfaceClass_Shading || SurfaceTmp( SurfNum ).Class == SurfaceClass_Detached_F || SurfaceTmp( SurfNum ).Class == SurfaceClass_Detached_B || SurfaceTmp( SurfNum ).Class == SurfaceClass_Overhang || SurfaceTmp( SurfNum ).Class == SurfaceClass_Fin ) ) continue;
			SurfaceTmp( SurfNum ).ShadowSurfDiffuseSolRefl = 0.2;
			SurfaceTmp( SurfNum ).ShadowSurfDiffuseVisRefl = 0.2;
			SurfaceTmp( SurfNum ).ShadowSurfGlazingFrac = 0.0;
			SurfaceTmp( SurfNum ).ShadowSurfGlazingConstruct = 0;
		}

		// Get the total number of Shading Surface Reflectance objects
		cCurrentModuleObject = "ShadingProperty:Reflectance";
		TotShadingSurfaceReflectance = GetNumObjectsFound( cCurrentModuleObject );
		//  IF(TotShadingSurfaceReflectance.EQ.0) RETURN

		for ( Loop = 1; Loop <= TotShadingSurfaceReflectance; ++Loop ) {

			GetObjectItem( cCurrentModuleObject, Loop, cAlphaArgs, NumAlpha, rNumericArgs, NumProp, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			SurfNum = FindItemInList( cAlphaArgs( 1 ), SurfaceTmp, TotSurfaces );
			if ( SurfNum == 0 ) {
				ShowWarningError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid specification" );
				ShowContinueError( ".. not found " + cAlphaFieldNames( 1 ) + "=\"" + cAlphaArgs( 1 ) + "\"." );
				//      ErrorsFound =.TRUE.
				continue;
			}

			// Check that associated surface is a shading surface
			WrongSurfaceType = false;
			if ( SurfNum != 0 ) {
				if ( ! ( SurfaceTmp( SurfNum ).Class == SurfaceClass_Shading || SurfaceTmp( SurfNum ).Class == SurfaceClass_Detached_F || SurfaceTmp( SurfNum ).Class == SurfaceClass_Detached_B || SurfaceTmp( SurfNum ).Class == SurfaceClass_Overhang || SurfaceTmp( SurfNum ).Class == SurfaceClass_Fin ) ) WrongSurfaceType = true;
				if ( WrongSurfaceType ) {
					ShowSevereError( "GetShadingSurfReflectanceData: " + cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", surface is not a shading surface." );
					ErrorsFound = true;
					continue;
				}
			}

			// If associated surface is a shading surface, set reflectance values
			SurfaceTmp( SurfNum ).ShadowSurfGlazingFrac = rNumericArgs( 3 );
			SurfaceTmp( SurfNum ).ShadowSurfDiffuseSolRefl = ( 1.0 - rNumericArgs( 3 ) ) * rNumericArgs( 1 );
			SurfaceTmp( SurfNum ).ShadowSurfDiffuseVisRefl = ( 1.0 - rNumericArgs( 3 ) ) * rNumericArgs( 2 );
			if ( rNumericArgs( 3 ) > 0.0 ) {
				GlConstrNum = FindItemInList( cAlphaArgs( 2 ), Construct, TotConstructs );
				if ( GlConstrNum == 0 ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + SurfaceTmp( SurfNum ).Name + "\", " + cAlphaFieldNames( 2 ) + " not found=" + cAlphaArgs( 2 ) );
					ErrorsFound = true;
				} else {
					Construct( GlConstrNum ).IsUsed = true;
				}
				SurfaceTmp( SurfNum ).ShadowSurfGlazingConstruct = GlConstrNum;
			}
			SurfNum = FindItemInList( "Mir-" + cAlphaArgs( 1 ), SurfaceTmp, TotSurfaces );
			if ( SurfNum == 0 ) continue;
			SurfaceTmp( SurfNum ).ShadowSurfGlazingFrac = rNumericArgs( 3 );
			SurfaceTmp( SurfNum ).ShadowSurfDiffuseSolRefl = ( 1.0 - rNumericArgs( 3 ) ) * rNumericArgs( 1 );
			SurfaceTmp( SurfNum ).ShadowSurfDiffuseVisRefl = ( 1.0 - rNumericArgs( 3 ) ) * rNumericArgs( 2 );
			if ( rNumericArgs( 3 ) > 0.0 ) {
				GlConstrNum = FindItemInList( cAlphaArgs( 2 ), Construct, TotConstructs );
				if ( GlConstrNum != 0 ) {
					Construct( GlConstrNum ).IsUsed = true;
				}
				SurfaceTmp( SurfNum ).ShadowSurfGlazingConstruct = GlConstrNum;
			}

		} // End of loop over Shading Surface Reflectance objects

		// Write reflectance values to .eio file.
		gio::write( OutputFileInits, fmtA ) << "! <ShadingProperty Reflectance>,Shading Surface Name,Shading Type,Diffuse Solar Reflectance, Diffuse Visible Reflectance,Surface Glazing Fraction,Surface Glazing Contruction";

		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
			if ( ! ( SurfaceTmp( SurfNum ).Class == SurfaceClass_Shading || SurfaceTmp( SurfNum ).Class == SurfaceClass_Detached_F || SurfaceTmp( SurfNum ).Class == SurfaceClass_Detached_B || SurfaceTmp( SurfNum ).Class == SurfaceClass_Overhang || SurfaceTmp( SurfNum ).Class == SurfaceClass_Fin ) ) continue;
			if ( SurfaceTmp( SurfNum ).ShadowSurfGlazingConstruct != 0 ) {
				gio::write( OutputFileInits, fmtA ) << "ShadingProperty Reflectance," + SurfaceTmp( SurfNum ).Name + ',' + cSurfaceClass( SurfaceTmp( SurfNum ).Class ) + ',' + RoundSigDigits( SurfaceTmp( SurfNum ).ShadowSurfDiffuseSolRefl, 2 ) + ',' + RoundSigDigits( SurfaceTmp( SurfNum ).ShadowSurfDiffuseVisRefl, 2 ) + ',' + RoundSigDigits( SurfaceTmp( SurfNum ).ShadowSurfGlazingFrac, 2 ) + ',' + Construct( SurfaceTmp( SurfNum ).ShadowSurfGlazingConstruct ).Name;
			} else {
				gio::write( OutputFileInits, fmtA ) << "ShadingProperty Reflectance," + SurfaceTmp( SurfNum ).Name + ',' + cSurfaceClass( SurfaceTmp( SurfNum ).Class ) + ',' + RoundSigDigits( SurfaceTmp( SurfNum ).ShadowSurfDiffuseSolRefl, 2 ) + ',' + RoundSigDigits( SurfaceTmp( SurfNum ).ShadowSurfDiffuseVisRefl, 2 ) + ',' + RoundSigDigits( SurfaceTmp( SurfNum ).ShadowSurfGlazingFrac, 2 ) + ", N/A";
			}
		}

	}

	void
	GetHTSurfExtVentedCavityData( bool & ErrorsFound ) // Error flag indicator (true if errors found)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         BGriffith
		//       DATE WRITTEN   January 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// load input data for Exterior Vented Cavity Special case for heat transfer surfaces

		// METHODOLOGY EMPLOYED:
		// usual E+ input processes

		// REFERENCES:
		// derived from SUBROUTINE GetTranspiredCollectorInput

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::GetObjectDefMaxArgs;
		using InputProcessor::FindItemInList;
		using InputProcessor::SameString;
		using InputProcessor::VerifyName;
		using General::TrimSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int Item; // Item to be "gotten"
		int NumAlphas; // Number of Alphas for each GetObjectItem call
		int NumNumbers; // Number of Numbers for each GetObjectItem call
		int MaxNumAlphas; // argument for call to GetObjectDefMaxArgs
		int MaxNumNumbers; // argument for call to GetObjectDefMaxArgs
		int Dummy; // argument for call to GetObjectDefMaxArgs
		int IOStatus; // Used in GetObjectItem
		int Found;
		int AlphaOffset; // local temp var
		std::string Roughness;
		int ThisSurf; // do loop counter
		Real64 AvgAzimuth; // temp for error checking
		Real64 AvgTilt; // temp for error checking
		int SurfID; // local surface "pointer"
		bool IsBlank;
		bool ErrorInName;

		cCurrentModuleObject = "SurfaceProperty:ExteriorNaturalVentedCavity";
		GetObjectDefMaxArgs( cCurrentModuleObject, Dummy, MaxNumAlphas, MaxNumNumbers );

		if ( MaxNumNumbers != 8 ) {
			ShowSevereError( cCurrentModuleObject + ": Object Definition indicates not = 8 Number Objects, Number Indicated=" + TrimSigDigits( MaxNumNumbers ) );
			ErrorsFound = true;
		}

		TotExtVentCav = GetNumObjectsFound( cCurrentModuleObject );

		ExtVentedCavity.allocate( TotExtVentCav );

		for ( Item = 1; Item <= TotExtVentCav; ++Item ) {
			GetObjectItem( cCurrentModuleObject, Item, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			// first handle cAlphaArgs
			ErrorInName = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), ExtVentedCavity, Item - 1, ErrorInName, IsBlank, cCurrentModuleObject + " Name" );
			if ( ErrorInName ) {
				ShowContinueError( "...cannot not duplicate other names" );
				ErrorsFound = true;
				continue;
			}
			ExtVentedCavity( Item ).Name = cAlphaArgs( 1 );

			ExtVentedCavity( Item ).OSCMName = cAlphaArgs( 2 );
			if ( ! lAlphaFieldBlanks( 2 ) ) {
				Found = FindItemInList( ExtVentedCavity( Item ).OSCMName, OSCM, TotOSCM );
				if ( Found == 0 ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + ExtVentedCavity( Item ).Name + "\", invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\"." );
					ErrorsFound = true;
				}
			} else {
				Found = 0;
				ShowSevereError( cCurrentModuleObject + "=\"" + ExtVentedCavity( Item ).Name + "\", invalid " + cAlphaFieldNames( 2 ) + " cannot be blank." );
				ErrorsFound = true;
			}
			ExtVentedCavity( Item ).OSCMPtr = Found;

			Roughness = cAlphaArgs( 3 );
			//Select the correct Number for the associated ascii name for the roughness type
			if ( SameString( Roughness, "VeryRough" ) ) ExtVentedCavity( Item ).BaffleRoughness = VeryRough;
			if ( SameString( Roughness, "Rough" ) ) ExtVentedCavity( Item ).BaffleRoughness = Rough;
			if ( SameString( Roughness, "MediumRough" ) ) ExtVentedCavity( Item ).BaffleRoughness = MediumRough;
			if ( SameString( Roughness, "MediumSmooth" ) ) ExtVentedCavity( Item ).BaffleRoughness = MediumSmooth;
			if ( SameString( Roughness, "Smooth" ) ) ExtVentedCavity( Item ).BaffleRoughness = Smooth;
			if ( SameString( Roughness, "VerySmooth" ) ) ExtVentedCavity( Item ).BaffleRoughness = VerySmooth;

			// Was it set?
			if ( ExtVentedCavity( Item ).BaffleRoughness == 0 ) {
				ShowSevereError( cCurrentModuleObject + "=\"" + ExtVentedCavity( Item ).Name + "\", invalid " + cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 3 ) );
				ErrorsFound = true;
			}

			AlphaOffset = 3;
			ExtVentedCavity( Item ).NumSurfs = NumAlphas - AlphaOffset;
			if ( ExtVentedCavity( Item ).NumSurfs == 0 ) {
				ShowSevereError( cCurrentModuleObject + "=\"" + ExtVentedCavity( Item ).Name + "\", no underlying surfaces specified. Must have at least one." );
				ErrorsFound = true;
				continue;
			}
			ExtVentedCavity( Item ).SurfPtrs.allocate( ExtVentedCavity( Item ).NumSurfs );
			ExtVentedCavity( Item ).SurfPtrs = 0;
			for ( ThisSurf = 1; ThisSurf <= ExtVentedCavity( Item ).NumSurfs; ++ThisSurf ) {
				Found = FindItemInList( cAlphaArgs( ThisSurf + AlphaOffset ), Surface, TotSurfaces );
				if ( Found == 0 ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + ExtVentedCavity( Item ).Name + "\", invalid " + cAlphaFieldNames( ThisSurf + AlphaOffset ) + "=\"" + cAlphaArgs( ThisSurf + AlphaOffset ) );
					ErrorsFound = true;
					continue;
				}
				// check that surface is appropriate, Heat transfer, Sun, Wind,
				if ( ! Surface( Found ).HeatTransSurf ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + ExtVentedCavity( Item ).Name + "\", invalid " + cAlphaFieldNames( ThisSurf + AlphaOffset ) + "=\"" + cAlphaArgs( ThisSurf + AlphaOffset ) );
					ShowContinueError( "...because it is not a Heat Transfer Surface." );
					ErrorsFound = true;
					continue;
				}
				if ( ! Surface( Found ).ExtSolar ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + ExtVentedCavity( Item ).Name + "\", invalid " + cAlphaFieldNames( ThisSurf + AlphaOffset ) + "=\"" + cAlphaArgs( ThisSurf + AlphaOffset ) );
					ShowContinueError( "...because it is not exposed to Sun." );
					ErrorsFound = true;
					continue;
				}
				if ( ! Surface( Found ).ExtWind ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + ExtVentedCavity( Item ).Name + "\", invalid " + cAlphaFieldNames( ThisSurf + AlphaOffset ) + "=\"" + cAlphaArgs( ThisSurf + AlphaOffset ) );
					ShowContinueError( "...because it is not exposed to Wind." );
					ErrorsFound = true;
					continue;
				}
				if ( Surface( Found ).ExtBoundCond != OtherSideCondModeledExt ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + ExtVentedCavity( Item ).Name + "\", is invalid" );
					ShowContinueError( "...because " + cAlphaFieldNames( ThisSurf + AlphaOffset ) + "=\"" + cAlphaArgs( ThisSurf + AlphaOffset ) + "\"." );
					ShowContinueError( "...is not an OtherSideConditionedModel surface." );
					ErrorsFound = true;
					continue;
				}
				ExtVentedCavity( Item ).SurfPtrs( ThisSurf ) = Found;

				// now set info in Surface structure
				Surface( Found ).ExtCavNum = Item;
				Surface( Found ).ExtCavityPresent = true;

			}

			if ( ErrorsFound ) continue; // previous inner do loop may have detected problems that need to be cycle'd again to avoid crash

			// now that we should have all the surfaces, do some preperations and checks.

			// are they all similar tilt and azimuth? Issue warnings so people can do it if they really want
			Real64 const surfaceArea( sum_sub( Surface, &SurfaceData::Area, ExtVentedCavity( Item ).SurfPtrs ) );
//			AvgAzimuth = sum( Surface( ExtVentedCavity( Item ).SurfPtrs ).Azimuth * Surface( ExtVentedCavity( Item ).SurfPtrs ).Area ) / sum( Surface( ExtVentedCavity( Item ).SurfPtrs ).Area ); //Autodesk:F2C++ Array subscript usage: Replaced by below
			AvgAzimuth = sum_product_sub( Surface, &SurfaceData::Azimuth, &SurfaceData::Area, ExtVentedCavity( Item ).SurfPtrs ) / surfaceArea; //Autodesk:F2C++ Functions handle array subscript usage
//			AvgTilt = sum( Surface( ExtVentedCavity( Item ).SurfPtrs ).Tilt * Surface( ExtVentedCavity( Item ).SurfPtrs ).Area ) / sum( Surface( ExtVentedCavity( Item ).SurfPtrs ).Area ); //Autodesk:F2C++ Array subscript usage: Replaced by below
			AvgTilt = sum_product_sub( Surface, &SurfaceData::Tilt, &SurfaceData::Area, ExtVentedCavity( Item ).SurfPtrs ) / surfaceArea; //Autodesk:F2C++ Functions handle array subscript usage
			for ( ThisSurf = 1; ThisSurf <= ExtVentedCavity( Item ).NumSurfs; ++ThisSurf ) {
				SurfID = ExtVentedCavity( Item ).SurfPtrs( ThisSurf );
				if ( std::abs( Surface( SurfID ).Azimuth - AvgAzimuth ) > 15.0 ) {
					ShowWarningError( cCurrentModuleObject + "=\"" + ExtVentedCavity( Item ).Name + ", Surface " + Surface( SurfID ).Name + " has Azimuth different from others in the associated group." );
				}
				if ( std::abs( Surface( SurfID ).Tilt - AvgTilt ) > 10.0 ) {
					ShowWarningError( cCurrentModuleObject + "=\"" + ExtVentedCavity( Item ).Name + ", Surface " + Surface( SurfID ).Name + " has Tilt different from others in the associated group." );
				}

				//test that there are no windows.  Now allow windows
				// If (Surface(SurfID)%GrossArea >  Surface(SurfID)%Area) Then
				//      Call ShowWarningError('Surface '//TRIM(Surface(SurfID)%name)//' has a subsurface whose area is not being ' &
				//         //'subtracted in the group of surfaces associated with '//TRIM(ExtVentedCavity(Item)%Name))
				// endif

			}
			ExtVentedCavity( Item ).Tilt = AvgTilt;
			ExtVentedCavity( Item ).Azimuth = AvgAzimuth;

			// find area weighted centroid.
//			ExtVentedCavity( Item ).Centroid.z = sum( Surface( ExtVentedCavity( Item ).SurfPtrs ).Centroid.z * Surface( ExtVentedCavity( Item ).SurfPtrs ).Area ) / sum( Surface( ExtVentedCavity( Item ).SurfPtrs ).Area ); //Autodesk:F2C++ Array subscript usage: Replaced by below
			ExtVentedCavity( Item ).Centroid.z = sum_product_sub( Surface, &SurfaceData::Centroid, &Vector::z, Surface, &SurfaceData::Area, ExtVentedCavity( Item ).SurfPtrs ) / surfaceArea; //Autodesk:F2C++ Functions handle array subscript usage

			//now handle rNumericArgs from input object
			ExtVentedCavity( Item ).Porosity = rNumericArgs( 1 );
			ExtVentedCavity( Item ).LWEmitt = rNumericArgs( 2 );
			ExtVentedCavity( Item ).SolAbsorp = rNumericArgs( 3 );
			ExtVentedCavity( Item ).HdeltaNPL = rNumericArgs( 4 );
			ExtVentedCavity( Item ).PlenGapThick = rNumericArgs( 5 );
			if ( ExtVentedCavity( Item ).PlenGapThick <= 0.0 ) {
				ShowSevereError( cCurrentModuleObject + "=\"" + ExtVentedCavity( Item ).Name + "\", invalid ." );
				ErrorsFound = true;
				ShowContinueError( "...because field \"" + cNumericFieldNames( 5 ) + "\" must be greater than Zero=[" + TrimSigDigits( rNumericArgs( 5 ), 2 ) + "]." );
				continue;
			}
			ExtVentedCavity( Item ).AreaRatio = rNumericArgs( 6 );
			ExtVentedCavity( Item ).Cv = rNumericArgs( 7 );
			ExtVentedCavity( Item ).Cd = rNumericArgs( 8 );

			// Fill out data we now know
			// sum areas of HT surface areas
//			ExtVentedCavity( Item ).ProjArea = sum( Surface( ExtVentedCavity( Item ).SurfPtrs ).Area ); //Autodesk:F2C++ Array subscript usage: Replaced by below
			ExtVentedCavity( Item ).ProjArea = surfaceArea;
			if ( ExtVentedCavity( Item ).ProjArea <= 0.0 ) {
				ShowSevereError( cCurrentModuleObject + "=\"" + ExtVentedCavity( Item ).Name + "\", invalid ." );
				ErrorsFound = true;
				ShowContinueError( "...because gross area of underlying surfaces must be greater than Zero=[" + TrimSigDigits( ExtVentedCavity( Item ).ProjArea, 2 ) + "]." );
				continue;
			}
			ExtVentedCavity( Item ).ActualArea = ExtVentedCavity( Item ).ProjArea * ExtVentedCavity( Item ).AreaRatio;

			SetupOutputVariable( "Surface Exterior Cavity Baffle Surface Temperature [C]", ExtVentedCavity( Item ).Tbaffle, "System", "Average", ExtVentedCavity( Item ).Name );
			SetupOutputVariable( "Surface Exterior Cavity Air Drybulb Temperature [C]", ExtVentedCavity( Item ).TAirCav, "System", "Average", ExtVentedCavity( Item ).Name );
			SetupOutputVariable( "Surface Exterior Cavity Total Natural Ventilation Air Change Rate [ACH]", ExtVentedCavity( Item ).PassiveACH, "System", "Average", ExtVentedCavity( Item ).Name );
			SetupOutputVariable( "Surface Exterior Cavity Total Natural Ventilation Mass Flow Rate [kg/s]", ExtVentedCavity( Item ).PassiveMdotVent, "System", "Average", ExtVentedCavity( Item ).Name );
			SetupOutputVariable( "Surface Exterior Cavity Natural Ventilation from Wind Mass Flow Rate [kg/s]", ExtVentedCavity( Item ).PassiveMdotWind, "System", "Average", ExtVentedCavity( Item ).Name );
			SetupOutputVariable( "Surface Exterior Cavity Natural Ventilation from Buoyancy Mass Flow Rate [kg/s]", ExtVentedCavity( Item ).PassiveMdotTherm, "System", "Average", ExtVentedCavity( Item ).Name );

		}

	}

	void
	GetSurfaceHeatTransferAlgorithmOverrides( bool & ErrorsFound )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith, portions from ApplyConvectionValue by Linda Lawrie
		//       DATE WRITTEN   July 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// <description>

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using DataSurfaces::Surface;
		using DataHeatBalance::HeatTransferAlgosUsed;
		using DataHeatBalance::NumberOfHeatTransferAlgosUsed;
		using DataHeatBalance::LowHConvLimit;
		using DataHeatBalance::HighHConvLimit;
		using General::RoundSigDigits;

		using DataHeatBalSurface::MaxSurfaceTempLimit;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int CountHTAlgoObjectsSingleSurf;
		int CountHTAlgoObjectsMultiSurf;
		int CountHTAlgoObjectsSurfList;
		int IOStatus; // Used in GetObjectItem
		static bool ErrorsFoundSingleSurf( false );
		static bool ErrorsFoundMultiSurf( false );
		static bool ErrorsFoundSurfList( false );
		static bool ErrorsFoundByConstruct( false );
		int tmpAlgoInput;
		int Item;
		int Item1;
		int NumAlphas;
		int NumNumbers;
		int Found;
		bool SurfacesOfType;
		int SurfNum;
		//  INTEGER :: Index
		int NumEMPDMat;
		int NumPCMat;
		int NumVTCMat;
		int NumHAMTMat1;
		int NumHAMTMat2;
		int NumHAMTMat3;
		int NumHAMTMat4;
		int NumHAMTMat5;
		int NumHAMTMat6;
		int SumHAMTMat;
		bool msgneeded;
		std::string AlgoName;

		// Formats
		static gio::Fmt Format_725( "('Surface Heat Transfer Algorithm, ',A,',',A,',',A,',',A)" );

		// first initialize each heat transfer surface with the overall model type, array assignment
		for ( auto & e : Surface ) e.HeatTransferAlgorithm = HeatTransferAlgosUsed( 1 );

		cCurrentModuleObject = "SurfaceProperty:HeatTransferAlgorithm";
		CountHTAlgoObjectsSingleSurf = GetNumObjectsFound( cCurrentModuleObject );

		cCurrentModuleObject = "SurfaceProperty:HeatTransferAlgorithm";
		for ( Item = 1; Item <= CountHTAlgoObjectsSingleSurf; ++Item ) {
			GetObjectItem( cCurrentModuleObject, Item, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			ErrorsFoundSingleSurf = false;
			Found = FindItemInList( cAlphaArgs( 1 ), Surface, TotSurfaces );

			if ( Found == 0 ) {
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", did not find matching surface." );
				ErrorsFoundSingleSurf = true;

			}

			{ auto const SELECT_CASE_var( cAlphaArgs( 2 ) );

			if ( SELECT_CASE_var == "CONDUCTIONTRANSFERFUNCTION" ) {
				tmpAlgoInput = HeatTransferModel_CTF;
			} else if ( SELECT_CASE_var == "MOISTUREPENETRATIONDEPTHCONDUCTIONTRANSFERFUNCTION" ) {
				tmpAlgoInput = HeatTransferModel_EMPD;
			} else if ( SELECT_CASE_var == "COMBINEDHEATANDMOISTUREFINITEELEMENT" ) {
				tmpAlgoInput = HeatTransferModel_HAMT;
			} else if ( SELECT_CASE_var == "CONDUCTIONFINITEDIFFERENCE" ) {
				tmpAlgoInput = HeatTransferModel_CondFD;
			} else {
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) );
				ErrorsFoundSingleSurf = true;
			}}

			if ( ! ErrorsFoundSingleSurf ) {
				Surface( Found ).HeatTransferAlgorithm = tmpAlgoInput;

				if ( ! any_eq( HeatTransferAlgosUsed, tmpAlgoInput ) ) { // add new algo
					HeatTransferAlgosUsed.redimension( ++NumberOfHeatTransferAlgosUsed );
					HeatTransferAlgosUsed( NumberOfHeatTransferAlgosUsed ) = tmpAlgoInput;
				}

			} else {
				ErrorsFound = true;
			}
		} // single surface heat transfer algorithm override

		cCurrentModuleObject = "SurfaceProperty:HeatTransferAlgorithm:MultipleSurface";
		CountHTAlgoObjectsMultiSurf = GetNumObjectsFound( cCurrentModuleObject );

		for ( Item = 1; Item <= CountHTAlgoObjectsMultiSurf; ++Item ) {
			GetObjectItem( cCurrentModuleObject, Item, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			ErrorsFoundMultiSurf = false;
			{ auto const SELECT_CASE_var( cAlphaArgs( 3 ) );

			if ( SELECT_CASE_var == "CONDUCTIONTRANSFERFUNCTION" ) {
				tmpAlgoInput = HeatTransferModel_CTF;
			} else if ( SELECT_CASE_var == "MOISTUREPENETRATIONDEPTHCONDUCTIONTRANSFERFUNCTION" ) {
				tmpAlgoInput = HeatTransferModel_EMPD;
			} else if ( SELECT_CASE_var == "COMBINEDHEATANDMOISTUREFINITEELEMENT" ) {
				tmpAlgoInput = HeatTransferModel_HAMT;
			} else if ( SELECT_CASE_var == "CONDUCTIONFINITEDIFFERENCE" ) {
				tmpAlgoInput = HeatTransferModel_CondFD;
			} else {
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid " + cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 3 ) );
				ErrorsFoundMultiSurf = true;
			}}

			{ auto const SELECT_CASE_var( cAlphaArgs( 2 ) );

			if ( SELECT_CASE_var == "ALLEXTERIORSURFACES" ) {
				SurfacesOfType = false;
				for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
					if ( ! Surface( SurfNum ).HeatTransSurf ) continue;
					if ( Surface( SurfNum ).ExtBoundCond > 0 ) continue; // Interior surfaces
					if ( Construct( Surface( SurfNum ).Construction ).TypeIsWindow ) continue;
					SurfacesOfType = true;
					Surface( SurfNum ).HeatTransferAlgorithm = tmpAlgoInput;
				}

			} else if ( SELECT_CASE_var == "ALLEXTERIORWALLS" ) {
				SurfacesOfType = false;
				for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
					if ( ! Surface( SurfNum ).HeatTransSurf ) continue;
					if ( Surface( SurfNum ).ExtBoundCond > 0 ) continue; // Interior surfaces
					if ( Surface( SurfNum ).Class != SurfaceClass_Wall ) continue;
					if ( Construct( Surface( SurfNum ).Construction ).TypeIsWindow ) continue;
					SurfacesOfType = true;
					Surface( SurfNum ).HeatTransferAlgorithm = tmpAlgoInput;
				}

			} else if ( SELECT_CASE_var == "ALLEXTERIORROOFS" ) {
				SurfacesOfType = false;
				for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
					if ( ! Surface( SurfNum ).HeatTransSurf ) continue;
					if ( Surface( SurfNum ).ExtBoundCond > 0 ) continue; // Interior surfaces
					if ( Surface( SurfNum ).Class != SurfaceClass_Roof ) continue;
					if ( Construct( Surface( SurfNum ).Construction ).TypeIsWindow ) continue;
					SurfacesOfType = true;
					Surface( SurfNum ).HeatTransferAlgorithm = tmpAlgoInput;
				}

			} else if ( SELECT_CASE_var == "ALLEXTERIORFLOORS" ) {
				SurfacesOfType = false;
				for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
					if ( ! Surface( SurfNum ).HeatTransSurf ) continue;
					if ( Surface( SurfNum ).ExtBoundCond > 0 ) continue; // Interior surfaces
					if ( Surface( SurfNum ).Class != SurfaceClass_Floor ) continue;
					if ( Construct( Surface( SurfNum ).Construction ).TypeIsWindow ) continue;
					SurfacesOfType = true;
					Surface( SurfNum ).HeatTransferAlgorithm = tmpAlgoInput;
				}

			} else if ( SELECT_CASE_var == "ALLGROUNDCONTACTSURFACES" ) {
				SurfacesOfType = false;
				for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
					if ( ! Surface( SurfNum ).HeatTransSurf ) continue;
					if ( Surface( SurfNum ).ExtBoundCond != Ground ) continue; // ground BC
					if ( Construct( Surface( SurfNum ).Construction ).TypeIsWindow ) continue;
					SurfacesOfType = true;
					Surface( SurfNum ).HeatTransferAlgorithm = tmpAlgoInput;
				}
			} else if ( SELECT_CASE_var == "ALLINTERIORSURFACES" ) {
				SurfacesOfType = false;
				for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
					if ( ! Surface( SurfNum ).HeatTransSurf ) continue;
					if ( Surface( SurfNum ).ExtBoundCond <= 0 ) continue; // Exterior surfaces
					if ( Construct( Surface( SurfNum ).Construction ).TypeIsWindow ) continue;
					SurfacesOfType = true;
					Surface( SurfNum ).HeatTransferAlgorithm = tmpAlgoInput;
				}

			} else if ( SELECT_CASE_var == "ALLINTERIORWALLS" ) {
				SurfacesOfType = false;
				for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
					if ( ! Surface( SurfNum ).HeatTransSurf ) continue;
					if ( Surface( SurfNum ).ExtBoundCond <= 0 ) continue; // Exterior surfaces
					if ( Surface( SurfNum ).Class != SurfaceClass_Wall ) continue;
					if ( Construct( Surface( SurfNum ).Construction ).TypeIsWindow ) continue;
					SurfacesOfType = true;
					Surface( SurfNum ).HeatTransferAlgorithm = tmpAlgoInput;
				}

			} else if ( ( SELECT_CASE_var == "ALLINTERIORROOFS" ) || ( SELECT_CASE_var == "ALLINTERIORCEILINGS" ) ) {
				SurfacesOfType = false;
				for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
					if ( ! Surface( SurfNum ).HeatTransSurf ) continue;
					if ( Surface( SurfNum ).ExtBoundCond <= 0 ) continue; // Exterior surfaces
					if ( Surface( SurfNum ).Class != SurfaceClass_Roof ) continue;
					if ( Construct( Surface( SurfNum ).Construction ).TypeIsWindow ) continue;
					SurfacesOfType = true;
					Surface( SurfNum ).HeatTransferAlgorithm = tmpAlgoInput;
				}

			} else if ( SELECT_CASE_var == "ALLINTERIORFLOORS" ) {
				SurfacesOfType = false;
				for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
					if ( ! Surface( SurfNum ).HeatTransSurf ) continue;
					if ( Surface( SurfNum ).ExtBoundCond <= 0 ) continue; // Exterior surfaces
					if ( Surface( SurfNum ).Class != SurfaceClass_Floor ) continue;
					if ( Construct( Surface( SurfNum ).Construction ).TypeIsWindow ) continue;
					SurfacesOfType = true;
					Surface( SurfNum ).HeatTransferAlgorithm = tmpAlgoInput;
				}
			} else {
				SurfacesOfType = false;
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) );
				ErrorsFoundMultiSurf = true;
			}}

			if ( ! SurfacesOfType ) {
				ShowWarningError( "In " + cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", for Multiple Surface Assignment=\"" + cAlphaArgs( 2 ) + "\", there were no surfaces of that type found for assignment." );
			} else {
				if ( ! any_eq( HeatTransferAlgosUsed, tmpAlgoInput ) ) { // add new algo
					HeatTransferAlgosUsed.redimension( ++NumberOfHeatTransferAlgosUsed );
					HeatTransferAlgosUsed( NumberOfHeatTransferAlgosUsed ) = tmpAlgoInput;
				}
			}
			if ( ErrorsFoundMultiSurf ) ErrorsFound = true;

		} // multi surface heat transfer algo override

		cCurrentModuleObject = "SurfaceProperty:HeatTransferAlgorithm:SurfaceList";
		CountHTAlgoObjectsSurfList = GetNumObjectsFound( cCurrentModuleObject );
		for ( Item = 1; Item <= CountHTAlgoObjectsSurfList; ++Item ) {
			GetObjectItem( cCurrentModuleObject, Item, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			ErrorsFoundSurfList = false;
			{ auto const SELECT_CASE_var( cAlphaArgs( 2 ) );

			if ( SELECT_CASE_var == "CONDUCTIONTRANSFERFUNCTION" ) {
				tmpAlgoInput = HeatTransferModel_CTF;
			} else if ( SELECT_CASE_var == "MOISTUREPENETRATIONDEPTHCONDUCTIONTRANSFERFUNCTION" ) {
				tmpAlgoInput = HeatTransferModel_EMPD;
			} else if ( SELECT_CASE_var == "COMBINEDHEATANDMOISTUREFINITEELEMENT" ) {
				tmpAlgoInput = HeatTransferModel_HAMT;
			} else if ( SELECT_CASE_var == "CONDUCTIONFINITEDIFFERENCE" ) {
				tmpAlgoInput = HeatTransferModel_CondFD;
			} else {
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) );
				ErrorsFoundSurfList = true;
			}}

			for ( Item1 = 3; Item1 <= NumAlphas; ++Item1 ) {

				Found = FindItemInList( cAlphaArgs( Item1 ), Surface, TotSurfaces );

				if ( Found == 0 ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", did not find matching surface." );
					ShowContinueError( "Name of surface not found = \"" + cAlphaArgs( Item1 ) + "\"" );
					ErrorsFoundSurfList = true;

				}

				if ( ! ErrorsFoundSurfList ) {
					Surface( Found ).HeatTransferAlgorithm = tmpAlgoInput;
					if ( ! any_eq( HeatTransferAlgosUsed, tmpAlgoInput ) ) { // add new algo
						HeatTransferAlgosUsed.redimension( ++NumberOfHeatTransferAlgosUsed );
						HeatTransferAlgosUsed( NumberOfHeatTransferAlgosUsed ) = tmpAlgoInput;
					}
				} else {
					ErrorsFound = true;
				}

			}

		}

		cCurrentModuleObject = "SurfaceProperty:HeatTransferAlgorithm:Construction";
		CountHTAlgoObjectsSurfList = GetNumObjectsFound( cCurrentModuleObject );
		for ( Item = 1; Item <= CountHTAlgoObjectsSurfList; ++Item ) {
			GetObjectItem( cCurrentModuleObject, Item, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			ErrorsFoundByConstruct = false;
			{ auto const SELECT_CASE_var( cAlphaArgs( 2 ) );

			if ( SELECT_CASE_var == "CONDUCTIONTRANSFERFUNCTION" ) {
				tmpAlgoInput = HeatTransferModel_CTF;
			} else if ( SELECT_CASE_var == "MOISTUREPENETRATIONDEPTHCONDUCTIONTRANSFERFUNCTION" ) {
				tmpAlgoInput = HeatTransferModel_EMPD;
			} else if ( SELECT_CASE_var == "COMBINEDHEATANDMOISTUREFINITEELEMENT" ) {
				tmpAlgoInput = HeatTransferModel_HAMT;
			} else if ( SELECT_CASE_var == "CONDUCTIONFINITEDIFFERENCE" ) {
				tmpAlgoInput = HeatTransferModel_CondFD;
			} else {
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) );
				ErrorsFoundByConstruct = true;
			}}

			Found = FindItemInList( cAlphaArgs( 3 ), Construct, TotConstructs );
			if ( Found == 0 ) {
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid " + cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 3 ) );
				ErrorsFoundByConstruct = true;
			}

			if ( ! ErrorsFoundByConstruct ) {
				for ( Item1 = 1; Item1 <= TotSurfaces; ++Item1 ) {
					if ( Surface( Item1 ).Construction == Found ) {
						Surface( Item1 ).HeatTransferAlgorithm = tmpAlgoInput;
						if ( ! any_eq( HeatTransferAlgosUsed, tmpAlgoInput ) ) { // add new algo
							HeatTransferAlgosUsed.redimension( ++NumberOfHeatTransferAlgosUsed );
							HeatTransferAlgosUsed( NumberOfHeatTransferAlgosUsed ) = tmpAlgoInput;
						}
					}
				}
			}
		}

		// test for missing materials for algorithms selected
		NumEMPDMat = GetNumObjectsFound( "MaterialProperty:MoisturePenetrationDepth:Settings" );
		NumPCMat = GetNumObjectsFound( "MaterialProperty:PhaseChange" ); // needs detailed algo
		NumVTCMat = GetNumObjectsFound( "MaterialProperty:VariableThermalConductivity" );
		NumHAMTMat1 = GetNumObjectsFound( "MaterialProperty:HeatAndMoistureTransfer:Settings" );
		NumHAMTMat2 = GetNumObjectsFound( "MaterialProperty:HeatAndMoistureTransfer:SorptionIsotherm" );
		NumHAMTMat3 = GetNumObjectsFound( "MaterialProperty:HeatAndMoistureTransfer:Suction" );
		NumHAMTMat4 = GetNumObjectsFound( "MaterialProperty:HeatAndMoistureTransfer:Redistribution" );
		NumHAMTMat5 = GetNumObjectsFound( "MaterialProperty:HeatAndMoistureTransfer:Diffusion" );
		NumHAMTMat6 = GetNumObjectsFound( "MaterialProperty:HeatAndMoistureTransfer:ThermalConductivity" );
		SumHAMTMat = NumHAMTMat1 + NumHAMTMat2 + NumHAMTMat3 + NumHAMTMat4 + NumHAMTMat5 + NumHAMTMat6;
		msgneeded = false;

		if ( NumEMPDMat > 0 && ! any_eq( HeatTransferAlgosUsed, HeatTransferModel_EMPD ) ) {
			ShowWarningError( "The input file includes " + RoundSigDigits( NumEMPDMat ) + " MaterialProperty:MoisturePenetrationDepth:Settings objects but the moisture penetration depth algorithm is not used anywhere." );
			msgneeded = true;
		}
		if ( NumPCMat > 0 && ! any_eq( HeatTransferAlgosUsed, HeatTransferModel_CondFD ) ) {
			ShowWarningError( "The input file includes " + RoundSigDigits( NumPCMat ) + " MaterialProperty:PhaseChange objects but the conduction finite difference algorithm is not used anywhere." );
			msgneeded = true;
		}
		if ( NumVTCMat > 0 && ! any_eq( HeatTransferAlgosUsed, HeatTransferModel_CondFD ) ) {
			ShowWarningError( "The input file includes " + RoundSigDigits( NumVTCMat ) + " MaterialProperty:VariableThermalConductivity objects but the conduction finite difference algorithm is not used anywhere." );
			msgneeded = true;
		}
		if ( SumHAMTMat > 0 && ! any_eq( HeatTransferAlgosUsed, HeatTransferModel_HAMT ) ) {
			ShowWarningError( "The input file includes " + RoundSigDigits( SumHAMTMat ) + " MaterialProperty:HeatAndMoistureTransfer:* objects but the combined heat and moisture finite difference algorithm is not used anywhere." );
			msgneeded = true;
		}
		if ( msgneeded ) {
			ShowContinueError( "Previous materials will be ignored due to HeatBalanceAlgorithm choice." );
		}
		msgneeded = false;
		if ( NumEMPDMat == 0 && any_eq( HeatTransferAlgosUsed, HeatTransferModel_EMPD ) ) {
			ShowWarningError( "The moisture penetration depth conduction transfer function algorithm is used but the input file includes no MaterialProperty:MoisturePenetrationDepth:Settings objects." );
			msgneeded = true;
		}
		if ( SumHAMTMat == 0 && any_eq( HeatTransferAlgosUsed, HeatTransferModel_HAMT ) ) {
			ShowWarningError( "The combined heat and moisture finite element algorithm is used but the input file includes no MaterialProperty:HeatAndMoistureTransfer:* objects." );
			msgneeded = true;
		}
		if ( msgneeded ) {
			ShowContinueError( "Certain materials objects are necessary to achieve proper results with the heat transfer algorithm(s) selected." );
		}

		// Write Solution Algorithm to the initialization output file for User Verification
		gio::write( OutputFileInits, fmtA ) << "! <Surface Heat Transfer Algorithm>, Value {CTF - ConductionTransferFunction | EMPD - MoisturePenetrationDepthConductionTransferFunction | CondFD - ConductionFiniteDifference | HAMT - CombinedHeatAndMoistureFiniteElement} - Description,Inside Surface Max Temperature Limit{C}, Surface Convection Coefficient Lower Limit {W/m2-K}, Surface Convection Coefficient Upper Limit {W/m2-K}";

		for ( Item1 = 1; Item1 <= NumberOfHeatTransferAlgosUsed; ++Item1 ) {
			AlgoName = "";
			{ auto const SELECT_CASE_var( HeatTransferAlgosUsed( Item1 ) );

			if ( SELECT_CASE_var == HeatTransferModel_CTF ) {
				AlgoName = "CTF - ConductionTransferFunction";
			} else if ( SELECT_CASE_var == HeatTransferModel_CondFD ) {
				AlgoName = "CondFD - ConductionFiniteDifference";
			} else if ( SELECT_CASE_var == HeatTransferModel_EMPD ) {
				AlgoName = "EMPD - MoisturePenetrationDepthConductionTransferFunction";
			} else if ( SELECT_CASE_var == HeatTransferModel_HAMT ) {
				AlgoName = "HAMT - CombinedHeatAndMoistureFiniteElement";
			}}

			gio::write( OutputFileInits, Format_725 ) << AlgoName << RoundSigDigits( MaxSurfaceTempLimit, 0 ) << RoundSigDigits( LowHConvLimit, 2 ) << RoundSigDigits( HighHConvLimit, 1 );
		}

		//Assign model type to windows, shading surfaces, and TDDs
		for ( Item = 1; Item <= TotSurfaces; ++Item ) {
			if ( Surface( Item ).Class == SurfaceClass_Window || Surface( Item ).Class == SurfaceClass_GlassDoor ) {
				// todo, add complex fenestration switch  HeatTransferModel_ComplexFenestration
				if ( SurfaceWindow( Item ).WindowModelType == WindowBSDFModel ) {
					Surface( Item ).HeatTransferAlgorithm = HeatTransferModel_ComplexFenestration;
				} else {
					Surface( Item ).HeatTransferAlgorithm = HeatTransferModel_Window5;
				}
			}
			if ( Surface( Item ).Class == SurfaceClass_Detached_B || Surface( Item ).Class == SurfaceClass_Detached_F || Surface( Item ).Class == SurfaceClass_Shading || Surface( Item ).Class == SurfaceClass_Overhang || Surface( Item ).Class == SurfaceClass_Fin ) {
				Surface( Item ).HeatTransferAlgorithm = HeatTransferModel_None;
			}
			if ( Surface( Item ).Class == SurfaceClass_TDD_Diffuser ) {
				Surface( Item ).HeatTransferAlgorithm = HeatTransferModel_TDD;
			}

		}

	}

	void
	GetVertices(
		int const SurfNum, // Current surface number
		int const NSides, // Number of sides to figure
		Array1S< Real64 > const Vertices // Vertices, in specified order
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   May 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine gets the surface vertices from the arrays
		// passed by the calling routine.  These had previously been obtained
		// from the InputProcessor (GetObjectItem).  This routine will provide
		// a standard place for determining various properties of the surface
		// from the vertices.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace Vectors;
		using General::RoundSigDigits;
		using namespace DataErrorTracking;

		// Locals
		static gio::Fmt fmt3( "(A,I5,A,3(1X,F18.13))" );

		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "GetVertices: " );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Ptr; // Pointer into Vertice array
		int n; // Loop counter
		int NSrc; // Used for CW -> CCW transformation
		int NTar; // Used for CW -> CCW transformation
		Real64 SurfWorldAz;
		Real64 SurfTilt;
		Real64 Perimeter; // Perimeter length of the surface
		int Vrt; // Used for calculating perimeter
		Real64 Xb; // Intermediate calculation
		Real64 Yb; // Intermediate calculation
		int ZoneNum;
		int ThisCorner;
		std::string TiltString;
		Real64 ThisWidth;
		Real64 ThisHeight;
		Real64 DistanceCheck;
		//unused    REAL(r64) :: ccwtest
		//unused    LOGICAL   :: SurfaceCCW
		Real64 dotp;

		// Object Data
		Vector const TestVector( 0.0, 0.0, 1.0 );
		Vector temp;

		if ( NSides > MaxVerticesPerSurface ) MaxVerticesPerSurface = NSides;
		Ptr = 1;
		for ( n = 1; n <= NSides; ++n ) {
			SurfaceTmp( SurfNum ).Vertex( n ).x = Vertices( Ptr );
			++Ptr;
			SurfaceTmp( SurfNum ).Vertex( n ).y = Vertices( Ptr );
			++Ptr;
			SurfaceTmp( SurfNum ).Vertex( n ).z = Vertices( Ptr );
			++Ptr;
		}

		// Address changing vertices if they were put in in CW order rather than CCW
		if ( ! CCW ) {
			// If even number of sides, this will transfer appropriately
			// If odd number, will leave the "odd" one, which is what you want.
			NSrc = NSides;
			NTar = 2;
			for ( n = 1; n <= ( NSides - 1 ) / 2; ++n ) {
				temp = SurfaceTmp( SurfNum ).Vertex( NSrc );
				SurfaceTmp( SurfNum ).Vertex( NSrc ) = SurfaceTmp( SurfNum ).Vertex( NTar );
				SurfaceTmp( SurfNum ).Vertex( NTar ) = temp;
				--NSrc;
				++NTar;
			}
		}
		// Now address which "Corner" has been put in first.  Note: the azimuth and tilt and area
		// calculations do not care which corner is put in first.
		// 2/2011 - don't think the shading calculations have a corner preference.  Will keep this for
		// consistency (for now)
		ThisCorner = Corner;
		while ( ThisCorner != UpperLeftCorner ) {
			if ( NSides < 4 ) {
				if ( ThisCorner == UpperRightCorner ) {
					ThisCorner = UpperLeftCorner;
					break;
				}
			}
			NTar = ThisCorner;
			NSrc = ThisCorner + 1;
			if ( NSrc > NSides ) NSrc = 1;
			for ( n = 1; n <= NSides - 1; ++n ) {
				temp = SurfaceTmp( SurfNum ).Vertex( NTar );
				SurfaceTmp( SurfNum ).Vertex( NTar ) = SurfaceTmp( SurfNum ).Vertex( NSrc );
				SurfaceTmp( SurfNum ).Vertex( NSrc ) = temp;
				++NTar;
				++NSrc;
				if ( NTar > NSides ) NTar = 1;
				if ( NSrc > NSides ) NSrc = 1;
			}
			++ThisCorner;
			if ( ThisCorner > NSides ) ThisCorner = 1;
		} // Corners
		if ( ! WorldCoordSystem ) {
			// Input in "relative" coordinates, use Building and Zone North Axes and Origins
			//                                  to translate each point (including rotation for Appendix G)
			ZoneNum = SurfaceTmp( SurfNum ).Zone;
			if ( ZoneNum > 0 ) {
				for ( n = 1; n <= NSides; ++n ) {
					Xb = SurfaceTmp( SurfNum ).Vertex( n ).x * CosZoneRelNorth( ZoneNum ) - SurfaceTmp( SurfNum ).Vertex( n ).y * SinZoneRelNorth( ZoneNum ) + Zone( ZoneNum ).OriginX;
					Yb = SurfaceTmp( SurfNum ).Vertex( n ).x * SinZoneRelNorth( ZoneNum ) + SurfaceTmp( SurfNum ).Vertex( n ).y * CosZoneRelNorth( ZoneNum ) + Zone( ZoneNum ).OriginY;
					SurfaceTmp( SurfNum ).Vertex( n ).x = Xb * CosBldgRelNorth - Yb * SinBldgRelNorth;
					SurfaceTmp( SurfNum ).Vertex( n ).y = Xb * SinBldgRelNorth + Yb * CosBldgRelNorth;
					SurfaceTmp( SurfNum ).Vertex( n ).z += Zone( ZoneNum ).OriginZ;
				}
			} else if ( SurfaceTmp( SurfNum ).Class == SurfaceClass_Detached_B ) {
				for ( n = 1; n <= NSides; ++n ) {
					Xb = SurfaceTmp( SurfNum ).Vertex( n ).x;
					Yb = SurfaceTmp( SurfNum ).Vertex( n ).y;
					SurfaceTmp( SurfNum ).Vertex( n ).x = Xb * CosBldgRelNorth - Yb * SinBldgRelNorth;
					SurfaceTmp( SurfNum ).Vertex( n ).y = Xb * SinBldgRelNorth + Yb * CosBldgRelNorth;
				}
			}
		} else {
			//if world coordinate only need to rotate for Appendix G
			ZoneNum = SurfaceTmp( SurfNum ).Zone;
			if ( ZoneNum > 0 ) {
				for ( n = 1; n <= NSides; ++n ) {
					Xb = SurfaceTmp( SurfNum ).Vertex( n ).x;
					Yb = SurfaceTmp( SurfNum ).Vertex( n ).y;
					SurfaceTmp( SurfNum ).Vertex( n ).x = Xb * CosBldgRotAppGonly - Yb * SinBldgRotAppGonly;
					SurfaceTmp( SurfNum ).Vertex( n ).y = Xb * SinBldgRotAppGonly + Yb * CosBldgRotAppGonly;
				}
			} else if ( SurfaceTmp( SurfNum ).Class == SurfaceClass_Detached_B ) {
				for ( n = 1; n <= NSides; ++n ) {
					Xb = SurfaceTmp( SurfNum ).Vertex( n ).x;
					Yb = SurfaceTmp( SurfNum ).Vertex( n ).y;
					SurfaceTmp( SurfNum ).Vertex( n ).x = Xb * CosBldgRotAppGonly - Yb * SinBldgRotAppGonly;
					SurfaceTmp( SurfNum ).Vertex( n ).y = Xb * SinBldgRotAppGonly + Yb * CosBldgRotAppGonly;
				}
			}
		}

		if ( NSides > 2 ) {
			DistanceCheck = distance( SurfaceTmp( SurfNum ).Vertex( SurfaceTmp( SurfNum ).Sides ), SurfaceTmp( SurfNum ).Vertex( 1 ) );
			if ( DistanceCheck < 0.01 ) {
				if ( DisplayExtraWarnings ) {
					ShowWarningError( RoutineName + "Distance between two vertices < .01, possibly coincident. for Surface=" + SurfaceTmp( SurfNum ).Name + ", in Zone=" + SurfaceTmp( SurfNum ).ZoneName );
					ShowContinueError( "Vertex [" + RoundSigDigits( SurfaceTmp( SurfNum ).Sides ) + "]=(" + RoundSigDigits( SurfaceTmp( SurfNum ).Vertex( SurfaceTmp( SurfNum ).Sides ).x, 2 ) + ',' + RoundSigDigits( SurfaceTmp( SurfNum ).Vertex( SurfaceTmp( SurfNum ).Sides ).y, 2 ) + ',' + RoundSigDigits( SurfaceTmp( SurfNum ).Vertex( SurfaceTmp( SurfNum ).Sides ).z, 2 ) + ')' );
					ShowContinueError( "Vertex [" + RoundSigDigits( 1 ) + "]=(" + RoundSigDigits( SurfaceTmp( SurfNum ).Vertex( 1 ).x, 2 ) + ',' + RoundSigDigits( SurfaceTmp( SurfNum ).Vertex( 1 ).y, 2 ) + ',' + RoundSigDigits( SurfaceTmp( SurfNum ).Vertex( 1 ).z, 2 ) + ')' );
				}
				++TotalCoincidentVertices;
				if ( SurfaceTmp( SurfNum ).Sides > 3 ) {
					if ( DisplayExtraWarnings ) {
						ShowContinueError( "Dropping Vertex [" + RoundSigDigits( SurfaceTmp( SurfNum ).Sides ) + "]." );
					}
					--SurfaceTmp( SurfNum ).Sides;
				} else {
					if ( DisplayExtraWarnings ) {
						ShowContinueError( "Cannot Drop Vertex [" + RoundSigDigits( SurfaceTmp( SurfNum ).Sides ) + "]; Number of Surface Sides at minimum. This surface is now a degenerate surface." );
					}
					++TotalDegenerateSurfaces;
					// mark degenerate surface?
				}
				DistanceCheck = 0.0;
			}
			Perimeter = DistanceCheck;
			//      DO Vrt=2,SurfaceTmp(SurfNum)%Sides
			Vrt = 2;
			while ( true ) {
				DistanceCheck = distance( SurfaceTmp( SurfNum ).Vertex( Vrt ), SurfaceTmp( SurfNum ).Vertex( Vrt - 1 ) );
				if ( DistanceCheck < 0.01 ) {
					if ( DisplayExtraWarnings ) {
						ShowWarningError( RoutineName + "Distance between two vertices < .01, possibly coincident. for Surface=" + SurfaceTmp( SurfNum ).Name + ", in Zone=" + SurfaceTmp( SurfNum ).ZoneName );
						ShowContinueError( "Vertex [" + RoundSigDigits( Vrt ) + "]=(" + RoundSigDigits( SurfaceTmp( SurfNum ).Vertex( Vrt ).x, 2 ) + ',' + RoundSigDigits( SurfaceTmp( SurfNum ).Vertex( Vrt ).y, 2 ) + ',' + RoundSigDigits( SurfaceTmp( SurfNum ).Vertex( Vrt ).z, 2 ) + ')' );
						ShowContinueError( "Vertex [" + RoundSigDigits( Vrt - 1 ) + "]=(" + RoundSigDigits( SurfaceTmp( SurfNum ).Vertex( Vrt - 1 ).x, 2 ) + ',' + RoundSigDigits( SurfaceTmp( SurfNum ).Vertex( Vrt - 1 ).y, 2 ) + ',' + RoundSigDigits( SurfaceTmp( SurfNum ).Vertex( Vrt - 1 ).z, 2 ) + ')' );
					}
					++TotalCoincidentVertices;
					if ( Vrt == SurfaceTmp( SurfNum ).Sides ) {
						if ( SurfaceTmp( SurfNum ).Sides > 3 ) {
							if ( DisplayExtraWarnings ) {
								ShowContinueError( "Dropping Vertex [" + RoundSigDigits( SurfaceTmp( SurfNum ).Sides ) + "]." );
							}
							--SurfaceTmp( SurfNum ).Sides;
						} else {
							if ( DisplayExtraWarnings ) {
								ShowContinueError( "Cannot Drop Vertex [" + RoundSigDigits( SurfaceTmp( SurfNum ).Sides ) + "]; Number of Surface Sides at minimum. This surface is now a degenerate surface." );
							}
							++TotalDegenerateSurfaces;
							// mark degenerate surface?
						}
						DistanceCheck = 0.0;
					} else {
						if ( SurfaceTmp( SurfNum ).Sides > 3 ) {
							if ( DisplayExtraWarnings ) {
								ShowContinueError( "Dropping Vertex [" + RoundSigDigits( Vrt ) + "]." );
							}
							for ( n = Vrt; n <= SurfaceTmp( SurfNum ).Sides - 1; ++n ) {
								SurfaceTmp( SurfNum ).Vertex( n ).x = SurfaceTmp( SurfNum ).Vertex( n + 1 ).x;
								SurfaceTmp( SurfNum ).Vertex( n ).y = SurfaceTmp( SurfNum ).Vertex( n + 1 ).y;
								SurfaceTmp( SurfNum ).Vertex( n ).z = SurfaceTmp( SurfNum ).Vertex( n + 1 ).z;
							}
							--SurfaceTmp( SurfNum ).Sides;
						} else {
							if ( DisplayExtraWarnings ) {
								ShowContinueError( "Cannot Drop Vertex [" + RoundSigDigits( SurfaceTmp( SurfNum ).Sides ) + "]; Number of Surface Sides at minimum. This surface is now a degenerate surface." );
							}
							++TotalDegenerateSurfaces;
							// mark degenerate surface?
						}
						DistanceCheck = 0.0;
					}
				}
				Perimeter += DistanceCheck;
				++Vrt;
				if ( Vrt > SurfaceTmp( SurfNum ).Sides ) break;
			}

			SurfaceTmp( SurfNum ).Perimeter = Perimeter;

			CreateNewellSurfaceNormalVector( SurfaceTmp( SurfNum ).Vertex, SurfaceTmp( SurfNum ).Sides, SurfaceTmp( SurfNum ).NewellSurfaceNormalVector );
			CreateNewellAreaVector( SurfaceTmp( SurfNum ).Vertex, SurfaceTmp( SurfNum ).Sides, SurfaceTmp( SurfNum ).NewellAreaVector );
			// For surfaces with subsurfaces, the following two areas are turned into net areas later by
			// subtracting subsurface areas
			SurfaceTmp( SurfNum ).GrossArea = VecLength( SurfaceTmp( SurfNum ).NewellAreaVector );
			SurfaceTmp( SurfNum ).Area = SurfaceTmp( SurfNum ).GrossArea;
			SurfaceTmp( SurfNum ).NetAreaShadowCalc = SurfaceTmp( SurfNum ).Area;
			DetermineAzimuthAndTilt( SurfaceTmp( SurfNum ).Vertex, SurfaceTmp( SurfNum ).Sides, SurfWorldAz, SurfTilt, SurfaceTmp( SurfNum ).lcsx, SurfaceTmp( SurfNum ).lcsy, SurfaceTmp( SurfNum ).lcsz, SurfaceTmp( SurfNum ).GrossArea, SurfaceTmp( SurfNum ).NewellSurfaceNormalVector );
			dotp = dot( SurfaceTmp( SurfNum ).NewellSurfaceNormalVector, TestVector );
			if ( SurfaceTmp( SurfNum ).Class == SurfaceClass_Roof && dotp < -0.000001 ) {
				TiltString = RoundSigDigits( SurfTilt, 1 );
				ShowWarningError( RoutineName + "Roof/Ceiling is upside down! Tilt angle=[" + TiltString + "], should be near 0, Surface=\"" + SurfaceTmp( SurfNum ).Name + "\", in Zone=\"" + SurfaceTmp( SurfNum ).ZoneName + "\"." );
				ShowContinueError( "Automatic fix is attempted." );
				ReverseAndRecalculate( SurfNum, SurfaceTmp( SurfNum ).Sides, SurfWorldAz, SurfTilt );
			} else if ( SurfaceTmp( SurfNum ).Class == SurfaceClass_Roof && SurfTilt > 80.0 ) {
				TiltString = RoundSigDigits( SurfTilt, 1 );
				ShowWarningError( RoutineName + "Roof/Ceiling is not oriented correctly! Tilt angle=[" + TiltString + "], should be near 0, Surface=\"" + SurfaceTmp( SurfNum ).Name + "\", in Zone=\"" + SurfaceTmp( SurfNum ).ZoneName + "\"." );
			}
			if ( SurfaceTmp( SurfNum ).Class == SurfaceClass_Floor && dotp > 0.000001 ) {
				TiltString = RoundSigDigits( SurfTilt, 1 );
				ShowWarningError( RoutineName + "Floor is upside down! Tilt angle=[" + TiltString + "], should be near 180, Surface=\"" + SurfaceTmp( SurfNum ).Name + "\", in Zone=\"" + SurfaceTmp( SurfNum ).ZoneName + "\"." );
				ShowContinueError( "Automatic fix is attempted." );
				ReverseAndRecalculate( SurfNum, SurfaceTmp( SurfNum ).Sides, SurfWorldAz, SurfTilt );
			} else if ( SurfaceTmp( SurfNum ).Class == SurfaceClass_Floor && SurfTilt < 158.2 ) { // slope/grade = 40%!
				TiltString = RoundSigDigits( SurfTilt, 1 );
				ShowWarningError( RoutineName + "Floor is not oriented correctly! Tilt angle=[" + TiltString + "], should be near 180, Surface=\"" + SurfaceTmp( SurfNum ).Name + "\", in Zone=\"" + SurfaceTmp( SurfNum ).ZoneName + "\"." );
			}
			SurfaceTmp( SurfNum ).Azimuth = SurfWorldAz;
			SurfaceTmp( SurfNum ).Tilt = SurfTilt;

			// Sine and cosine of azimuth and tilt
			SurfaceTmp( SurfNum ).SinAzim = std::sin( SurfWorldAz * DegToRadians );
			SurfaceTmp( SurfNum ).CosAzim = std::cos( SurfWorldAz * DegToRadians );
			SurfaceTmp( SurfNum ).SinTilt = std::sin( SurfTilt * DegToRadians );
			SurfaceTmp( SurfNum ).CosTilt = std::cos( SurfTilt * DegToRadians );
			if ( SurfaceTmp( SurfNum ).ViewFactorGround == AutoCalculate ) {
				SurfaceTmp( SurfNum ).ViewFactorGround = 0.5 * ( 1.0 - SurfaceTmp( SurfNum ).CosTilt );
			}
			// Outward normal unit vector (pointing away from room)
			SurfaceTmp( SurfNum ).OutNormVec = SurfaceTmp( SurfNum ).NewellSurfaceNormalVector;
			for ( n = 1; n <= 3; ++n ) {
				if ( std::abs( SurfaceTmp( SurfNum ).OutNormVec( n ) - 1.0 ) < 1.e-06 ) SurfaceTmp( SurfNum ).OutNormVec( n ) = +1.0;
				if ( std::abs( SurfaceTmp( SurfNum ).OutNormVec( n ) + 1.0 ) < 1.e-06 ) SurfaceTmp( SurfNum ).OutNormVec( n ) = -1.0;
				if ( std::abs( SurfaceTmp( SurfNum ).OutNormVec( n ) ) < 1.e-06 ) SurfaceTmp( SurfNum ).OutNormVec( n ) = 0.0;
			}

			if ( SurfaceTmp( SurfNum ).Class == SurfaceClass_Window || SurfaceTmp( SurfNum ).Class == SurfaceClass_GlassDoor || SurfaceTmp( SurfNum ).Class == SurfaceClass_Door ) SurfaceTmp( SurfNum ).Area *= SurfaceTmp( SurfNum ).Multiplier;
			// Can perform tests on this surface here
			SurfaceTmp( SurfNum ).ViewFactorSky = 0.5 * ( 1.0 + SurfaceTmp( SurfNum ).CosTilt );
			// The following IR view factors are modified in subr. SkyDifSolarShading if there are shadowing
			// surfaces
			SurfaceTmp( SurfNum ).ViewFactorSkyIR = SurfaceTmp( SurfNum ).ViewFactorSky;
			SurfaceTmp( SurfNum ).ViewFactorGroundIR = 0.5 * ( 1.0 - SurfaceTmp( SurfNum ).CosTilt );

			// Call to transform vertices

			TransformVertsByAspect( SurfNum, SurfaceTmp( SurfNum ).Sides );

		} else {
			ShowFatalError( RoutineName + "Called with less than 2 sides, Surface=" + SurfaceTmp( SurfNum ).Name );
		}

		// Preliminary Height/Width
		temp = SurfaceTmp( SurfNum ).Vertex( 3 ) - SurfaceTmp( SurfNum ).Vertex( 2 );
		ThisWidth = VecLength( temp );
		temp = SurfaceTmp( SurfNum ).Vertex( 2 ) - SurfaceTmp( SurfNum ).Vertex( 1 );
		ThisHeight = VecLength( temp );
		SurfaceTmp( SurfNum ).Height = ThisHeight;
		SurfaceTmp( SurfNum ).Width = ThisWidth;

	}

	void
	ReverseAndRecalculate(
		int const SurfNum, // Surface number for the surface
		int const NSides, // number of sides to surface
		Real64 & SurfAzimuth, // Surface Facing angle (will be 0 for roofs/floors)
		Real64 & SurfTilt // Surface tilt (
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   February 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine reverses the vertices for a surface (needed when roof/floor is upside down)
		// and recalculates the azimuth, etc for the surface.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace Vectors;
		using General::RoundSigDigits;

		// Locals
		static gio::Fmt fmt3( "(A,I5,A,3(1X,F18.13))" );

		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "ReverseAndRecalculate: " );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int n; // Loop Control
		int RevPtr; // pointer for reversing vertices
		std::string TiltString;

		// Object Data
		Array1D< Vector > Vertices( NSides ); // Vertices, in specified order

		for ( n = 1; n <= NSides; ++n ) {
			Vertices( n ) = SurfaceTmp( SurfNum ).Vertex( n );
		}
		RevPtr = NSides;
		for ( n = 1; n <= NSides; ++n ) {
			SurfaceTmp( SurfNum ).Vertex( n ) = Vertices( RevPtr );
			--RevPtr;
		}

		gio::write( OutputFileDebug, fmtLD ) << "Reversing Surface Name=" + SurfaceTmp( SurfNum ).Name;
		for ( n = 1; n <= NSides; ++n ) {
			gio::write( OutputFileDebug, fmt3 ) << "side=" << n << " abs coord vertex=" << SurfaceTmp( SurfNum ).Vertex( n ).x << SurfaceTmp( SurfNum ).Vertex( n ).y << SurfaceTmp( SurfNum ).Vertex( n ).z;
		}

		CreateNewellSurfaceNormalVector( SurfaceTmp( SurfNum ).Vertex, SurfaceTmp( SurfNum ).Sides, SurfaceTmp( SurfNum ).NewellSurfaceNormalVector );
		DetermineAzimuthAndTilt( SurfaceTmp( SurfNum ).Vertex, SurfaceTmp( SurfNum ).Sides, SurfAzimuth, SurfTilt, SurfaceTmp( SurfNum ).lcsx, SurfaceTmp( SurfNum ).lcsy, SurfaceTmp( SurfNum ).lcsz, SurfaceTmp( SurfNum ).GrossArea, SurfaceTmp( SurfNum ).NewellSurfaceNormalVector );
		if ( SurfaceTmp( SurfNum ).Class == SurfaceClass_Roof && SurfTilt > 80.0 ) {
			TiltString = RoundSigDigits( SurfTilt, 1 );
			ShowWarningError( RoutineName + "Roof/Ceiling is still upside down! Tilt angle=[" + TiltString + "], should be near 0, please fix manually." );
		}
		if ( SurfaceTmp( SurfNum ).Class == SurfaceClass_Floor && SurfTilt < 158.2 ) { // 40% grade!
			ShowWarningError( RoutineName + "Floor is still upside down! Tilt angle=[" + TiltString + "], should be near 180, please fix manually." );
		}

	}

	void
	MakeMirrorSurface( int & SurfNum ) // In=>Surface to Mirror, Out=>new Surface index
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   June 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine creates a "mirror" surface using the indicated surface.
		// This is the simple approach for bi-directional shading devices.  If, perchance,
		// the user has already taken care of this (e.g. fins in middle of wall), there will
		// be extra shading devices shown.

		// METHODOLOGY EMPLOYED:
		// Reverse the vertices in the original surface.  Add "bi" to name.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace Vectors;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Vert;
		int NVert;
		Real64 SurfWorldAz;
		Real64 SurfTilt;
		int n;
		//  TYPE(Vector) :: temp1

		NVert = SurfaceTmp( SurfNum ).Sides;
		SurfaceTmp( SurfNum + 1 ).Vertex.allocate( NVert );
		// doesn't work when Vertex are pointers  SurfaceTmp(SurfNum+1)=SurfaceTmp(SurfNum)
		SurfaceTmp( SurfNum + 1 ).Name = SurfaceTmp( SurfNum ).Name;
		SurfaceTmp( SurfNum + 1 ).Construction = SurfaceTmp( SurfNum ).Construction;
		SurfaceTmp( SurfNum + 1 ).ConstructionStoredInputValue = SurfaceTmp( SurfNum ).ConstructionStoredInputValue;
		SurfaceTmp( SurfNum + 1 ).Class = SurfaceTmp( SurfNum ).Class;
		SurfaceTmp( SurfNum + 1 ).GrossArea = SurfaceTmp( SurfNum ).GrossArea;
		SurfaceTmp( SurfNum + 1 ).Area = SurfaceTmp( SurfNum ).Area;
		SurfaceTmp( SurfNum + 1 ).Azimuth = SurfaceTmp( SurfNum ).Azimuth;
		SurfaceTmp( SurfNum + 1 ).Height = SurfaceTmp( SurfNum ).Height;
		SurfaceTmp( SurfNum + 1 ).Reveal = SurfaceTmp( SurfNum ).Reveal;
		SurfaceTmp( SurfNum + 1 ).Shape = SurfaceTmp( SurfNum ).Shape;
		SurfaceTmp( SurfNum + 1 ).Sides = SurfaceTmp( SurfNum ).Sides;
		SurfaceTmp( SurfNum + 1 ).Tilt = SurfaceTmp( SurfNum ).Tilt;
		SurfaceTmp( SurfNum + 1 ).Width = SurfaceTmp( SurfNum ).Width;
		SurfaceTmp( SurfNum + 1 ).HeatTransSurf = SurfaceTmp( SurfNum ).HeatTransSurf;
		SurfaceTmp( SurfNum + 1 ).BaseSurfName = SurfaceTmp( SurfNum ).BaseSurfName;
		SurfaceTmp( SurfNum + 1 ).BaseSurf = SurfaceTmp( SurfNum ).BaseSurf;
		SurfaceTmp( SurfNum + 1 ).ZoneName = SurfaceTmp( SurfNum ).ZoneName;
		SurfaceTmp( SurfNum + 1 ).Zone = SurfaceTmp( SurfNum ).Zone;
		SurfaceTmp( SurfNum + 1 ).ExtBoundCondName = SurfaceTmp( SurfNum ).ExtBoundCondName;
		SurfaceTmp( SurfNum + 1 ).ExtBoundCond = SurfaceTmp( SurfNum ).ExtBoundCond;
		SurfaceTmp( SurfNum + 1 ).ExtSolar = SurfaceTmp( SurfNum ).ExtSolar;
		SurfaceTmp( SurfNum + 1 ).ExtWind = SurfaceTmp( SurfNum ).ExtWind;
		SurfaceTmp( SurfNum + 1 ).ViewFactorGround = SurfaceTmp( SurfNum ).ViewFactorGround;
		SurfaceTmp( SurfNum + 1 ).ViewFactorSky = SurfaceTmp( SurfNum ).ViewFactorSky;
		SurfaceTmp( SurfNum + 1 ).ViewFactorGroundIR = SurfaceTmp( SurfNum ).ViewFactorGroundIR;
		SurfaceTmp( SurfNum + 1 ).ViewFactorSkyIR = SurfaceTmp( SurfNum ).ViewFactorSkyIR;
		SurfaceTmp( SurfNum + 1 ).SchedShadowSurfIndex = SurfaceTmp( SurfNum ).SchedShadowSurfIndex;
		SurfaceTmp( SurfNum + 1 ).ShadowSurfSchedVaries = SurfaceTmp( SurfNum ).ShadowSurfSchedVaries;
		SurfaceTmp( SurfNum + 1 ).SchedMinValue = SurfaceTmp( SurfNum ).SchedMinValue;
		SurfaceTmp( SurfNum + 1 ).IsTransparent = SurfaceTmp( SurfNum ).IsTransparent;
		SurfaceTmp( SurfNum + 1 ).ShadowingSurf = SurfaceTmp( SurfNum ).ShadowingSurf;
		SurfaceTmp( SurfNum + 1 ).MaterialMovInsulExt = SurfaceTmp( SurfNum ).MaterialMovInsulExt;
		SurfaceTmp( SurfNum + 1 ).MaterialMovInsulInt = SurfaceTmp( SurfNum ).MaterialMovInsulInt;
		SurfaceTmp( SurfNum + 1 ).SchedMovInsulExt = SurfaceTmp( SurfNum ).SchedMovInsulExt;
		SurfaceTmp( SurfNum + 1 ).SchedMovInsulInt = SurfaceTmp( SurfNum ).SchedMovInsulInt;
		SurfaceTmp( SurfNum + 1 ).WindowShadingControlPtr = SurfaceTmp( SurfNum ).WindowShadingControlPtr;
		SurfaceTmp( SurfNum + 1 ).ShadedConstruction = SurfaceTmp( SurfNum ).ShadedConstruction;
		SurfaceTmp( SurfNum + 1 ).FrameDivider = SurfaceTmp( SurfNum ).FrameDivider;
		SurfaceTmp( SurfNum + 1 ).Multiplier = SurfaceTmp( SurfNum ).Multiplier;
		SurfaceTmp( SurfNum + 1 ).NetAreaShadowCalc = SurfaceTmp( SurfNum ).NetAreaShadowCalc;
		SurfaceTmp( SurfNum + 1 ).Perimeter = SurfaceTmp( SurfNum ).Perimeter;

		for ( Vert = 1; Vert <= SurfaceTmp( SurfNum ).Sides; ++Vert ) {
			SurfaceTmp( SurfNum + 1 ).Vertex( Vert ) = SurfaceTmp( SurfNum ).Vertex( NVert );
			--NVert;
		}
		++SurfNum;
		SurfaceTmp( SurfNum ).Name = "Mir-" + SurfaceTmp( SurfNum - 1 ).Name;

		// TH 3/26/2010
		SurfaceTmp( SurfNum ).MirroredSurf = true;

		if ( SurfaceTmp( SurfNum ).Sides > 2 ) {
			CreateNewellAreaVector( SurfaceTmp( SurfNum ).Vertex, SurfaceTmp( SurfNum ).Sides, SurfaceTmp( SurfNum ).NewellAreaVector );
			SurfaceTmp( SurfNum ).GrossArea = VecLength( SurfaceTmp( SurfNum ).NewellAreaVector );
			SurfaceTmp( SurfNum ).Area = SurfaceTmp( SurfNum ).GrossArea;
			SurfaceTmp( SurfNum ).NetAreaShadowCalc = SurfaceTmp( SurfNum ).Area;
			CreateNewellSurfaceNormalVector( SurfaceTmp( SurfNum ).Vertex, SurfaceTmp( SurfNum ).Sides, SurfaceTmp( SurfNum ).NewellSurfaceNormalVector );
			DetermineAzimuthAndTilt( SurfaceTmp( SurfNum ).Vertex, SurfaceTmp( SurfNum ).Sides, SurfWorldAz, SurfTilt, SurfaceTmp( SurfNum ).lcsx, SurfaceTmp( SurfNum ).lcsy, SurfaceTmp( SurfNum ).lcsz, SurfaceTmp( SurfNum ).GrossArea, SurfaceTmp( SurfNum ).NewellSurfaceNormalVector );
			SurfaceTmp( SurfNum ).Azimuth = SurfWorldAz;
			SurfaceTmp( SurfNum ).Tilt = SurfTilt;

			// Sine and cosine of azimuth and tilt
			SurfaceTmp( SurfNum ).SinAzim = std::sin( SurfWorldAz * DegToRadians );
			SurfaceTmp( SurfNum ).CosAzim = std::cos( SurfWorldAz * DegToRadians );
			SurfaceTmp( SurfNum ).SinTilt = std::sin( SurfTilt * DegToRadians );
			SurfaceTmp( SurfNum ).CosTilt = std::cos( SurfTilt * DegToRadians );
			// Outward normal unit vector (pointing away from room)
			SurfaceTmp( SurfNum ).OutNormVec = SurfaceTmp( SurfNum ).NewellSurfaceNormalVector;
			for ( n = 1; n <= 3; ++n ) {
				if ( std::abs( SurfaceTmp( SurfNum ).OutNormVec( n ) - 1.0 ) < 1.e-06 ) SurfaceTmp( SurfNum ).OutNormVec( n ) = +1.0;
				if ( std::abs( SurfaceTmp( SurfNum ).OutNormVec( n ) + 1.0 ) < 1.e-06 ) SurfaceTmp( SurfNum ).OutNormVec( n ) = -1.0;
				if ( std::abs( SurfaceTmp( SurfNum ).OutNormVec( n ) ) < 1.e-06 ) SurfaceTmp( SurfNum ).OutNormVec( n ) = 0.0;
			}

			// Can perform tests on this surface here
			SurfaceTmp( SurfNum ).ViewFactorSky = 0.5 * ( 1.0 + SurfaceTmp( SurfNum ).CosTilt );
			// The following IR view factors are modified in subr. SkyDifSolarShading if there are shadowing
			// surfaces
			SurfaceTmp( SurfNum ).ViewFactorSkyIR = SurfaceTmp( SurfNum ).ViewFactorSky;
			SurfaceTmp( SurfNum ).ViewFactorGroundIR = 0.5 * ( 1.0 - SurfaceTmp( SurfNum ).CosTilt );
		}

	}

	void
	GetWindowShadingControlData( bool & ErrorsFound ) // If errors found in input
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   November 1998
		//       MODIFIED       Aug 2001 (FW): add handling of new ShadingControlIsScheduled
		//                      and GlareControlIsActive fields
		//                      Nov 2001 (FW): add ShadingDevice as alternative to ShadedConstruction
		//                      Dec 2001 (FW): add slat angle controls for blinds
		//                      Aug 2002 (FW): add Setpoint2; check that specified control type is legal
		//                      Feb 2003 (FW): add error if Material Name of Shading Device is used with
		//                        Shading Type = BetweenGlassShade or BetweenGlassBlind
		//                      Dec 2003 (FW): improve BetweenGlassBlind error messages
		//                      Feb 2009 (BG): improve error checking for OnIfScheduleAllows
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Reads in the window shading control information
		// from the input data file, interprets it and puts it in the derived type

		// METHODOLOGY EMPLOYED:

		// REFERENCES:

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using InputProcessor::VerifyName;
		using ScheduleManager::GetScheduleIndex;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const NumValidShadingTypes( 8 );
		static Array1D_string const cValidShadingTypes( NumValidShadingTypes, { "INTERIORSHADE", "EXTERIORSHADE", "EXTERIORSCREEN", "INTERIORBLIND", "EXTERIORBLIND", "BETWEENGLASSSHADE", "BETWEENGLASSBLIND", "SWITCHABLEGLAZING" } );
		static Array1D_int const ValidShadingTypes( NumValidShadingTypes, { WSC_ST_InteriorShade, WSC_ST_ExteriorShade, WSC_ST_ExteriorScreen, WSC_ST_InteriorBlind, WSC_ST_ExteriorBlind, WSC_ST_BetweenGlassShade, WSC_ST_BetweenGlassBlind, WSC_ST_SwitchableGlazing } );

		int const NumValidWindowShadingControlTypes( 21 );
		static Array1D_string const cValidWindowShadingControlTypes( NumValidWindowShadingControlTypes, { "ALWAYSON", "ALWAYSOFF", "ONIFSCHEDULEALLOWS", "ONIFHIGHSOLARONWINDOW", "ONIFHIGHHORIZONTALSOLAR", "ONIFHIGHOUTDOORAIRTEMPERATURE", "ONIFHIGHZONEAIRTEMPERATURE", "ONIFHIGHZONECOOLING", "ONIFHIGHGLARE", "MEETDAYLIGHTILLUMINANCESETPOINT", "ONNIGHTIFLOWOUTDOORTEMPANDOFFDAY", "ONNIGHTIFLOWINSIDETEMPANDOFFDAY", "ONNIGHTIFHEATINGANDOFFDAY", "ONNIGHTIFLOWOUTDOORTEMPANDONDAYIFCOOLING", "ONNIGHTIFHEATINGANDONDAYIFCOOLING", "OFFNIGHTANDONDAYIFCOOLINGANDHIGHSOLARONWINDOW", "ONNIGHTANDONDAYIFCOOLINGANDHIGHSOLARONWINDOW", "ONIFHIGHOUTDOORAIRTEMPANDHIGHSOLARONWINDOW", "ONIFHIGHOUTDOORAIRTEMPANDHIGHHORIZONTALSOLAR", "ONIFHIGHZONEAIRTEMPANDHIGHSOLARONWINDOW", "ONIFHIGHZONEAIRTEMPANDHIGHHORIZONTALSOLAR" } );

		static Array1D_int const ValidWindowShadingControlTypes( NumValidWindowShadingControlTypes, { WSCT_AlwaysOn, WSCT_AlwaysOff, WSCT_OnIfScheduled, WSCT_HiSolar, WSCT_HiHorzSolar, WSCT_HiOutAirTemp, WSCT_HiZoneAirTemp, WSCT_HiZoneCooling, WSCT_HiGlare, WSCT_MeetDaylIlumSetp, WSCT_OnNightLoOutTemp_OffDay, WSCT_OnNightLoInTemp_OffDay, WSCT_OnNightIfHeating_OffDay, WSCT_OnNightLoOutTemp_OnDayCooling, WSCT_OnNightIfHeating_OnDayCooling, WSCT_OffNight_OnDay_HiSolarWindow, WSCT_OnNight_OnDay_HiSolarWindow, WSCT_OnHiOutTemp_HiSolarWindow, WSCT_OnHiOutTemp_HiHorzSolar, WSCT_OnHiZoneTemp_HiSolarWindow, WSCT_OnHiZoneTemp_HiHorzSolar } ); // 'ALWAYSON                                    ', & | 'ALWAYSOFF                                   ', & | 'ONIFSCHEDULEALLOWS                          ', & | 'ONIFHIGHSOLARONWINDOW                       ', & | 'ONIFHIGHHORIZONTALSOLAR                     ', & | 'ONIFHIGHOUTDOORAIRTEMPERATURE                      ', & | 'ONIFHIGHZONEAIRTEMPERATURE                         ', & | 'ONIFHIGHZONECOOLING                         ', & | 'ONIFHIGHGLARE                               ', & | 'MEETDAYLIGHTILLUMINANCESETPOINT             ', & | 'ONNIGHTIFLOWOUTDOORTEMPANDOFFDAY              ', & | 'ONNIGHTIFLOWINSIDETEMPANDOFFDAY               ', & | 'ONNIGHTIFHEATINGANDOFFDAY                     ', & | 'ONNIGHTIFLOWOUTDOORTEMPANDONDAYIFCOOLING      ', & | 'ONNIGHTIFHEATINGANDONDAYIFCOOLING             ', & | 'OFFNIGHTANDONDAYIFCOOLINGANDHIGHSOLARONWINDOW ', & | 'ONNIGHTANDONDAYIFCOOLINGANDHIGHSOLARONWINDOW  ', & | 'ONIFHIGHOUTDOORAIRTEMPANDHIGHSOLARONWINDOW  ', & | 'ONIFHIGHOUTDOORAIRTEMPANDHIGHHORIZONTALSOLAR', & | 'ONIFHIGHZONEAIRTEMPANDHIGHSOLARONWINDOW     ', & | 'ONIFHIGHZONEAIRTEMPANDHIGHHORIZONTALSOLAR   '/)

		// INTERFACE BLOCK SPECIFICATIONS:na
		// DERIVED TYPE DEFINITIONS:na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int IOStat; // IO Status when calling get input subroutine
		int ControlNumAlpha; // Number of control alpha names being passed
		int ControlNumProp; // Number of control properties being passed
		int ControlNum; // DO loop counter/index for window shading control number
		int IShadedConst; // Construction number of shaded construction
		int IShadingDevice; // Material number of shading device
		int NLayers; // Layers in shaded construction
		bool ErrorInName;
		bool IsBlank;
		int Loop;
		int ShTyp; // Shading type
		std::string ControlType; // Shading control type
		bool BGShadeBlindError; // True if problem with construction that is supposed to have between-glass
		// shade or blind
		int Found;

		// FLOW:
		// Get the total number of window shading control blocks
		cCurrentModuleObject = "WindowProperty:ShadingControl";
		TotWinShadingControl = GetNumObjectsFound( cCurrentModuleObject );
		if ( TotWinShadingControl == 0 ) return;

		WindowShadingControl.allocate( TotWinShadingControl );

		ControlNum = 0;
		for ( Loop = 1; Loop <= TotWinShadingControl; ++Loop ) {

			GetObjectItem( cCurrentModuleObject, Loop, cAlphaArgs, ControlNumAlpha, rNumericArgs, ControlNumProp, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			ErrorInName = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), WindowShadingControl, ControlNum, ErrorInName, IsBlank, cCurrentModuleObject + " Name" );
			if ( ErrorInName ) {
				ErrorsFound = true;
				continue;
			}

			++ControlNum;
			WindowShadingControl( ControlNum ).Name = cAlphaArgs( 1 ); // Set the Control Name in the Derived Type
			WindowShadingControl( ControlNum ).ShadedConstruction = FindItemInList( cAlphaArgs( 3 ), Construct, TotConstructs );
			WindowShadingControl( ControlNum ).ShadingDevice = FindItemInList( cAlphaArgs( 8 ), Material, TotMaterials );
			WindowShadingControl( ControlNum ).Schedule = GetScheduleIndex( cAlphaArgs( 5 ) );
			WindowShadingControl( ControlNum ).SetPoint = rNumericArgs( 1 );
			WindowShadingControl( ControlNum ).SetPoint2 = rNumericArgs( 2 );
			WindowShadingControl( ControlNum ).ShadingControlIsScheduled = false;
			if ( cAlphaArgs( 6 ) == "YES" ) WindowShadingControl( ControlNum ).ShadingControlIsScheduled = true;
			WindowShadingControl( ControlNum ).GlareControlIsActive = false;
			if ( cAlphaArgs( 7 ) == "YES" ) WindowShadingControl( ControlNum ).GlareControlIsActive = true;
			WindowShadingControl( ControlNum ).SlatAngleSchedule = GetScheduleIndex( cAlphaArgs( 10 ) );

			ControlType = cAlphaArgs( 4 );

			if ( ControlType == "SCHEDULE" ) {
				ControlType = "ONIFSCHEDULEALLOWS";
				WindowShadingControl( ControlNum ).ShadingControlIsScheduled = true;
				WindowShadingControl( ControlNum ).GlareControlIsActive = false;
				ShowWarningError( cCurrentModuleObject + "=\"" + WindowShadingControl( ControlNum ).Name + "\" is using obsolete " + cAlphaFieldNames( 4 ) + "=\"" + cAlphaArgs( 4 ) + "\", changing to \"" + ControlType + "\"" );
				// Error if schedule has not been specified
				if ( WindowShadingControl( ControlNum ).Schedule <= 0 ) {
					ErrorsFound = true;
					ShowWarningError( cCurrentModuleObject + "=\"" + WindowShadingControl( ControlNum ).Name + " has " + cAlphaFieldNames( 4 ) + " \"" + ControlType + "\" but a schedule has not been specified." );
				}
			}

			if ( has_prefix( ControlType, "SCHEDULEAND" ) ) {
				ControlType = "ONIFHIGH" + ControlType.substr( 11 );
				WindowShadingControl( ControlNum ).ShadingControlIsScheduled = true;
				WindowShadingControl( ControlNum ).GlareControlIsActive = false;
				ShowWarningError( cCurrentModuleObject + "=\"" + WindowShadingControl( ControlNum ).Name + "\" is using obsolete " + cAlphaFieldNames( 4 ) + "=\"" + cAlphaArgs( 4 ) + "\", changing to \"" + ControlType + "\"" );
				// Error if schedule has not been specified
				if ( WindowShadingControl( ControlNum ).Schedule <= 0 ) {
					ErrorsFound = true;
					ShowWarningError( cCurrentModuleObject + "=\"" + WindowShadingControl( ControlNum ).Name + " has " + cAlphaFieldNames( 4 ) + " \"" + ControlType + "\" but a schedule has not been specified." );
				}
			}

			if ( has_prefix( ControlType, "GLAREOR" ) ) {
				ControlType = "ONIFHIGH" + ControlType.substr( 7 );
				WindowShadingControl( ControlNum ).ShadingControlIsScheduled = false;
				WindowShadingControl( ControlNum ).GlareControlIsActive = true;
				ShowWarningError( cCurrentModuleObject + "=\"" + WindowShadingControl( ControlNum ).Name + "\" is using obsolete " + cAlphaFieldNames( 4 ) + "=\"" + cAlphaArgs( 4 ) + "\", changing to \"" + ControlType + "\"" );
			}

			if ( ControlType == "GLARE" ) {
				ControlType = "ONIFHIGHGLARE";
				WindowShadingControl( ControlNum ).ShadingControlIsScheduled = false;
				WindowShadingControl( ControlNum ).GlareControlIsActive = true;
				ShowWarningError( cCurrentModuleObject + "=\"" + WindowShadingControl( ControlNum ).Name + "\" is using obsolete " + cAlphaFieldNames( 4 ) + "=\"" + cAlphaArgs( 4 ) + "\", changing to \"" + ControlType + "\"" );
			}

			if ( WindowShadingControl( ControlNum ).ShadingDevice > 0 ) {
				if ( Material( WindowShadingControl( ControlNum ).ShadingDevice ).Group == Screen && ! ( ControlType == "ALWAYSON" || ControlType == "ALWAYSOFF" || ControlType == "ONIFSCHEDULEALLOWS" ) ) {
					ErrorsFound = true;
					ShowSevereError( cCurrentModuleObject + "=\"" + WindowShadingControl( ControlNum ).Name + "\" invalid " + cAlphaFieldNames( 4 ) + "=\"" + cAlphaArgs( 4 ) + "\" for exterior screens." );
					ShowContinueError( "Valid shading control types for exterior window screens are ALWAYSON, ALWAYSOFF, or ONIFSCHEDULEALLOWS." );
				}
			} else {
				if ( WindowShadingControl( ControlNum ).ShadedConstruction > 0 ) {
					Construct( WindowShadingControl( ControlNum ).ShadedConstruction ).IsUsed = true;
					if ( Material( Construct( WindowShadingControl( ControlNum ).ShadedConstruction ).LayerPoint( 1 ) ).Group == Screen && ! ( ControlType == "ALWAYSON" || ControlType == "ALWAYSOFF" || ControlType == "ONIFSCHEDULEALLOWS" ) ) {
						ErrorsFound = true;
						ShowSevereError( cCurrentModuleObject + "=\"" + WindowShadingControl( ControlNum ).Name + "\" invalid " + cAlphaFieldNames( 4 ) + "=\"" + cAlphaArgs( 4 ) + "\" for exterior screens." );
						ShowContinueError( "Valid shading control types for exterior window screens are ALWAYSON, ALWAYSOFF, or ONIFSCHEDULEALLOWS." );
					}
				} else if ( lAlphaFieldBlanks( 3 ) ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + WindowShadingControl( ControlNum ).Name + "\", " + cAlphaFieldNames( 3 ) + " is blank." );
					ShowContinueError( "A valid construction is required." );
					ErrorsFound = true;
				} else {
					ShowSevereError( cCurrentModuleObject + "=\"" + WindowShadingControl( ControlNum ).Name + "\", " + cAlphaFieldNames( 3 ) + " is invalid." );
					ShowContinueError( "Construction=\"" + cAlphaArgs( 3 ) + "\" was used. A valid construction is required." );
					ErrorsFound = true;
				}
			}

			// Warning if setpoint is unintentionally zero
			if ( WindowShadingControl( ControlNum ).SetPoint == 0 && ControlType != "ALWAYSON" && ControlType != "ALWAYSOFF" && ControlType != "ONIFSCHEDULEALLOWS" && ControlType != "SCHEDULE" && ControlType != "ONIFHIGHGLARE" && ControlType != "GLARE" && ControlType != "DAYLIGHTILLUMINANCE" ) {
				ShowWarningError( cCurrentModuleObject + "=\"" + WindowShadingControl( ControlNum ).Name + "\", The first SetPoint is zero." );
				ShowContinueError( "..You may have forgotten to specify that setpoint." );
			}

			// Upward compatibility for old Shading Control Type names
			if ( ControlType == "SOLARONWINDOW" || ControlType == "HORIZONTALSOLAR" || ControlType == "OUTSIDEAIRTEMP" || ControlType == "ZONEAIRTEMP" || ControlType == "ZONECOOLING" ) {
				ControlType = "ONIFHIGH" + ControlType;
				WindowShadingControl( ControlNum ).ShadingControlIsScheduled = false;
				WindowShadingControl( ControlNum ).GlareControlIsActive = false;
				ShowWarningError( cCurrentModuleObject + "=\"" + WindowShadingControl( ControlNum ).Name + "\" is using obsolete " + cAlphaFieldNames( 4 ) + "=\"" + cAlphaArgs( 4 ) + "\", changing to \"" + ControlType + "\"" );
			}

			// Error if illegal control type
			Found = FindItemInList( ControlType, cValidWindowShadingControlTypes, NumValidWindowShadingControlTypes );
			if ( Found == 0 ) {
				ErrorsFound = true;
				ShowSevereError( cCurrentModuleObject + "=\"" + WindowShadingControl( ControlNum ).Name + "\" invalid " + cAlphaFieldNames( 4 ) + "=\"" + cAlphaArgs( 4 ) + "\"." );
			} else {
				WindowShadingControl( ControlNum ).ShadingControlType = ValidWindowShadingControlTypes( Found );
			}

			// Error checks
			if ( cAlphaArgs( 6 ) != "YES" && cAlphaArgs( 6 ) != "NO" ) {
				ErrorsFound = true;
				ShowSevereError( cCurrentModuleObject + "=\"" + WindowShadingControl( ControlNum ).Name + "\" invalid " + cAlphaFieldNames( 6 ) + "=\"" + cAlphaArgs( 6 ) + "\"." );
			}
			if ( cAlphaArgs( 7 ) != "YES" && cAlphaArgs( 7 ) != "NO" ) {
				ErrorsFound = true;
				ShowSevereError( cCurrentModuleObject + "=\"" + WindowShadingControl( ControlNum ).Name + "\" invalid " + cAlphaFieldNames( 7 ) + "=\"" + cAlphaArgs( 7 ) + "\"." );
			}

			if ( ( WindowShadingControl( ControlNum ).ShadingControlType == WSCT_OnIfScheduled ) && ( ! WindowShadingControl( ControlNum ).ShadingControlIsScheduled ) ) { // CR 7709 BG
				ErrorsFound = true;
				ShowSevereError( cCurrentModuleObject + " = \"" + WindowShadingControl( ControlNum ).Name + "\" invalid, " + cAlphaFieldNames( 6 ) + " must be set to \"Yes\" for " + cAlphaFieldNames( 4 ) + " = OnIfScheduleAllows" );
			}

			if ( cAlphaArgs( 9 ) != "FIXEDSLATANGLE" && cAlphaArgs( 9 ) != "SCHEDULEDSLATANGLE" && cAlphaArgs( 9 ) != "BLOCKBEAMSOLAR" ) {
				ErrorsFound = true;
				ShowSevereError( cCurrentModuleObject + "=\"" + WindowShadingControl( ControlNum ).Name + "\" invalid " + cAlphaFieldNames( 9 ) + "=\"" + cAlphaArgs( 9 ) + "\"." );
			} else if ( cAlphaArgs( 9 ) == "FIXEDSLATANGLE" ) {
				WindowShadingControl( ControlNum ).SlatAngleControlForBlinds = WSC_SAC_FixedSlatAngle;
			} else if ( cAlphaArgs( 9 ) == "SCHEDULEDSLATANGLE" ) {
				WindowShadingControl( ControlNum ).SlatAngleControlForBlinds = WSC_SAC_ScheduledSlatAngle;
			} else if ( cAlphaArgs( 9 ) == "BLOCKBEAMSOLAR" ) {
				WindowShadingControl( ControlNum ).SlatAngleControlForBlinds = WSC_SAC_BlockBeamSolar;
			}

			// For upward compatibility change old "noninsulating" and "insulating" shade types to
			// INTERIORSHADE or EXTERIORSHADE
			if ( cAlphaArgs( 2 ) == "INTERIORNONINSULATINGSHADE" || cAlphaArgs( 2 ) == "INTERIORINSULATINGSHADE" ) {
				ShowWarningError( cCurrentModuleObject + "=\"" + WindowShadingControl( ControlNum ).Name + "\" is using obsolete " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\", changing to \"InteriorShade\"" );
				WindowShadingControl( ControlNum ).ShadingType = WSC_ST_InteriorShade;
				cAlphaArgs( 2 ) = "INTERIORSHADE";
			}
			if ( cAlphaArgs( 2 ) == "EXTERIORNONINSULATINGSHADE" || cAlphaArgs( 2 ) == "EXTERIORINSULATINGSHADE" ) {
				ShowWarningError( cCurrentModuleObject + "=\"" + WindowShadingControl( ControlNum ).Name + "\" is using obsolete " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\", changing to \"ExteriorShade\"" );
				WindowShadingControl( ControlNum ).ShadingType = WSC_ST_ExteriorShade;
				cAlphaArgs( 2 ) = "EXTERIORSHADE";
			}

			if ( ControlType == "MEETDAYLIGHTILLUMINANCESETPOINT" && cAlphaArgs( 2 ) != "SWITCHABLEGLAZING" ) {
				ErrorsFound = true;
				ShowSevereError( cCurrentModuleObject + "=\"" + WindowShadingControl( ControlNum ).Name + "\" invalid " + cAlphaFieldNames( 4 ) + "=\"" + cAlphaArgs( 4 ) + "\"." );
				ShowContinueError( "..." + cAlphaFieldNames( 2 ) + " must be SwitchableGlazing for this control, but entered type=\"" + cAlphaArgs( 2 ) + "\"." );
			}

			// Check for illegal shading type name
			Found = FindItemInList( cAlphaArgs( 2 ), cValidShadingTypes, NumValidShadingTypes );
			if ( Found == 0 ) {
				ErrorsFound = true;
				ShowSevereError( cCurrentModuleObject + "=\"" + WindowShadingControl( ControlNum ).Name + "\" invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\"." );
			} else {
				WindowShadingControl( ControlNum ).ShadingType = ValidShadingTypes( Found );
			}

			ShTyp = WindowShadingControl( ControlNum ).ShadingType;
			IShadedConst = WindowShadingControl( ControlNum ).ShadedConstruction;
			IShadingDevice = WindowShadingControl( ControlNum ).ShadingDevice;

			if ( IShadedConst == 0 && IShadingDevice == 0 ) {
				ShowSevereError( cCurrentModuleObject + "=\"" + WindowShadingControl( ControlNum ).Name + "\" has no matching shaded construction or shading device." );
				ErrorsFound = true;
			} else if ( IShadedConst == 0 && IShadingDevice > 0 ) {
				if ( ShTyp == WSC_ST_SwitchableGlazing ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + WindowShadingControl( ControlNum ).Name + "\" has " + cAlphaArgs( 2 ) + "= SwitchableGlazing but no matching shaded construction" );
					ErrorsFound = true;
				}
				if ( ( ShTyp == WSC_ST_InteriorShade || ShTyp == WSC_ST_ExteriorShade ) && Material( IShadingDevice ).Group != Shade ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + WindowShadingControl( ControlNum ).Name + "\" has " + cAlphaArgs( 2 ) + "= InteriorShade or ExteriorShade but matching shading device is not a window shade" );
					ShowContinueError( cAlphaFieldNames( 8 ) + " in error=\"" + Material( IShadingDevice ).Name + "\"." );
					ErrorsFound = true;
				}
				if ( ( ShTyp == WSC_ST_ExteriorScreen ) && Material( IShadingDevice ).Group != Screen ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + WindowShadingControl( ControlNum ).Name + "\" has " + cAlphaArgs( 2 ) + "= ExteriorScreen but matching shading device is not a window screen" );
					ShowContinueError( cAlphaFieldNames( 8 ) + " in error=\"" + Material( IShadingDevice ).Name + "\"." );
					ErrorsFound = true;
				}
				if ( ( ShTyp == WSC_ST_InteriorBlind || ShTyp == WSC_ST_ExteriorBlind ) && Material( IShadingDevice ).Group != WindowBlind ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + WindowShadingControl( ControlNum ).Name + "\" has " + cAlphaArgs( 2 ) + "= InteriorBlind or ExteriorBlind but matching shading device is not a window blind" );
					ShowContinueError( cAlphaFieldNames( 8 ) + " in error=\"" + Material( IShadingDevice ).Name + "\"." );
					ErrorsFound = true;
				}
				if ( ShTyp == WSC_ST_BetweenGlassShade || ShTyp == WSC_ST_BetweenGlassBlind ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + WindowShadingControl( ControlNum ).Name + "\" has " + cAlphaArgs( 2 ) + "= BetweenGlassShade or BetweenGlassBlind and" );
					ShowContinueError( cAlphaFieldNames( 8 ) + " is specified. This is illegal. Specify shaded construction instead." );
					ErrorsFound = true;
				}
			} else if ( IShadedConst > 0 && IShadingDevice > 0 ) {
				IShadingDevice = 0;
				ShowWarningError( cCurrentModuleObject + "=\"" + WindowShadingControl( ControlNum ).Name + "\" Both " + cAlphaFieldNames( 3 ) + " and " + cAlphaFieldNames( 8 ) + " are specified." );
				ShowContinueError( "The " + cAlphaFieldNames( 3 ) + "=\"" + Construct( IShadedConst ).Name + "\" will be used." );
			}

			// If type = interior or exterior shade or blind require that the shaded construction
			// have a shade layer in the correct position
			if ( IShadedConst != 0 ) {

				NLayers = Construct( IShadedConst ).TotLayers;
				BGShadeBlindError = false;
				IShadingDevice = 0;
				if ( Construct( IShadedConst ).LayerPoint( NLayers ) != 0 ) {
					if ( WindowShadingControl( ControlNum ).ShadingType == WSC_ST_InteriorShade ) {
						IShadingDevice = Construct( IShadedConst ).LayerPoint( NLayers );
						if ( Material( Construct( IShadedConst ).LayerPoint( NLayers ) ).Group != Shade ) {
							ErrorsFound = true;
							ShowSevereError( cCurrentModuleObject + "=\"" + WindowShadingControl( ControlNum ).Name + "\" the " + cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 3 ) + "\"" );
							ShowContinueError( "of " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\" should have a shade layer on the inside of the window." );
						}
					} else if ( WindowShadingControl( ControlNum ).ShadingType == WSC_ST_ExteriorShade ) {
						IShadingDevice = Construct( IShadedConst ).LayerPoint( 1 );
						if ( Material( Construct( IShadedConst ).LayerPoint( 1 ) ).Group != Shade ) {
							ErrorsFound = true;
							ShowSevereError( cCurrentModuleObject + "=\"" + WindowShadingControl( ControlNum ).Name + "\" the " + cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 3 ) + "\"" );
							ShowContinueError( "of " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\" should have a shade layer on the outside of the window." );
						}
					} else if ( WindowShadingControl( ControlNum ).ShadingType == WSC_ST_ExteriorScreen ) {
						IShadingDevice = Construct( IShadedConst ).LayerPoint( 1 );
						if ( Material( Construct( IShadedConst ).LayerPoint( 1 ) ).Group != Screen ) {
							ErrorsFound = true;
							ShowSevereError( cCurrentModuleObject + "=\"" + WindowShadingControl( ControlNum ).Name + "\" the " + cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 3 ) + "\"" );
							ShowContinueError( "of " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\" should have a screen layer on the outside of the window." );
						}
					} else if ( WindowShadingControl( ControlNum ).ShadingType == WSC_ST_InteriorBlind ) {
						IShadingDevice = Construct( IShadedConst ).LayerPoint( NLayers );
						if ( Material( Construct( IShadedConst ).LayerPoint( NLayers ) ).Group != WindowBlind ) {
							ErrorsFound = true;
							ShowSevereError( cCurrentModuleObject + "=\"" + WindowShadingControl( ControlNum ).Name + "\" the " + cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 3 ) + "\"" );
							ShowContinueError( "of " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\" should have a blind layer on the inside of the window." );
						}
					} else if ( WindowShadingControl( ControlNum ).ShadingType == WSC_ST_ExteriorBlind ) {
						IShadingDevice = Construct( IShadedConst ).LayerPoint( 1 );
						if ( Material( Construct( IShadedConst ).LayerPoint( 1 ) ).Group != WindowBlind ) {
							ErrorsFound = true;
							ShowSevereError( cCurrentModuleObject + "=\"" + WindowShadingControl( ControlNum ).Name + "\" the " + cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 3 ) + "\"" );
							ShowContinueError( "of " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\" should have a blind layer on the outside of the window." );
						}
					} else if ( WindowShadingControl( ControlNum ).ShadingType == WSC_ST_BetweenGlassShade ) {
						if ( NLayers != 5 && NLayers != 7 ) BGShadeBlindError = true;
						if ( NLayers == 5 ) {
							if ( Material( Construct( IShadedConst ).LayerPoint( 3 ) ).Group != Shade ) BGShadeBlindError = true;
						}
						if ( NLayers == 7 ) {
							if ( Material( Construct( IShadedConst ).LayerPoint( 5 ) ).Group != Shade ) BGShadeBlindError = true;
						}
						if ( BGShadeBlindError ) {
							ErrorsFound = true;
							ShowSevereError( cCurrentModuleObject + "=\"" + WindowShadingControl( ControlNum ).Name + "\" the " + cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 3 ) + "\"" );
							ShowContinueError( "of " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\" should have two or three glass layers and a" );
							ShowContinueError( "between-glass shade layer with a gas layer on each side." );
						}
					} else if ( WindowShadingControl( ControlNum ).ShadingType == WSC_ST_BetweenGlassBlind ) {
						if ( NLayers != 5 && NLayers != 7 ) BGShadeBlindError = true;
						if ( NLayers == 5 ) {
							if ( Material( Construct( IShadedConst ).LayerPoint( 3 ) ).Group != WindowBlind ) BGShadeBlindError = true;
						}
						if ( NLayers == 7 ) {
							if ( Material( Construct( IShadedConst ).LayerPoint( 5 ) ).Group != WindowBlind ) BGShadeBlindError = true;
						}
						if ( BGShadeBlindError ) {
							ErrorsFound = true;
							ShowSevereError( cCurrentModuleObject + "=\"" + WindowShadingControl( ControlNum ).Name + "\" the " + cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 3 ) + "\"" );
							ShowContinueError( "of " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\" should have two or three glass layers and a" );
							ShowContinueError( "between-glass blind layer with a gas layer on each side." );
						}
					}
				}
				if ( IShadingDevice > 0 ) {
					if ( ( ShTyp == WSC_ST_InteriorShade || ShTyp == WSC_ST_ExteriorShade ) && Material( IShadingDevice ).Group != Shade ) {
						ShowSevereError( cCurrentModuleObject + "=\"" + WindowShadingControl( ControlNum ).Name + "\" has " + cAlphaFieldNames( 2 ) + "= InteriorShade or ExteriorShade but matching shading device is not a window shade" );
						ShowContinueError( "Shading Device in error=\"" + Material( IShadingDevice ).Name + "\"." );
						ErrorsFound = true;
					}
					if ( ( ShTyp == WSC_ST_ExteriorScreen ) && Material( IShadingDevice ).Group != Screen ) {
						ShowSevereError( cCurrentModuleObject + "=\"" + WindowShadingControl( ControlNum ).Name + "\" has " + cAlphaFieldNames( 2 ) + "= ExteriorScreen but matching shading device is not an exterior window screen." );
						ShowContinueError( "Shading Device in error=\"" + Material( IShadingDevice ).Name + "\"." );
						ErrorsFound = true;
					}
					if ( ( ShTyp == WSC_ST_InteriorBlind || ShTyp == WSC_ST_ExteriorBlind ) && Material( IShadingDevice ).Group != WindowBlind ) {
						ShowSevereError( cCurrentModuleObject + "=\"" + WindowShadingControl( ControlNum ).Name + "\" has " + cAlphaFieldNames( 2 ) + "= InteriorBlind or ExteriorBlind but matching shading device is not a window blind." );
						ShowContinueError( "Shading Device in error=\"" + Material( IShadingDevice ).Name + "\"." );
						ErrorsFound = true;
					}
				}

			}

		} // End of loop over window shading controls

	}

	void
	GetStormWindowData( bool & ErrorsFound ) // If errors found in input
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   December 2003
		//       MODIFIED       na

		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Reads in the storm window data from the input file,
		// interprets it and puts it in the derived type

		// METHODOLOGY EMPLOYED:

		// REFERENCES:

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using InputProcessor::VerifyName;
		using General::JulianDay;
		using General::TrimSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:na
		// INTERFACE BLOCK SPECIFICATIONS:na
		// DERIVED TYPE DEFINITIONS:na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int IOStat; // IO Status when calling get input subroutine
		int StormWinNumAlpha; // Number of alpha names being passed
		int StormWinNumProp; // Number of properties being passed
		int StormWinNum; // Index for storm window number
		int loop; // Do loop counter
		int SurfNum; // Surface number
		int MatNum; // Material number

		// FLOW:

		// Get the total number of storm window input objects
		cCurrentModuleObject = "WindowProperty:StormWindow";
		TotStormWin = GetNumObjectsFound( cCurrentModuleObject );
		if ( TotStormWin == 0 ) return;

		StormWindow.allocate( TotStormWin );

		StormWinNum = 0;
		for ( loop = 1; loop <= TotStormWin; ++loop ) {

			GetObjectItem( cCurrentModuleObject, loop, cAlphaArgs, StormWinNumAlpha, rNumericArgs, StormWinNumProp, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			++StormWinNum;
			StormWindow( StormWinNum ).BaseWindowNum = FindItemInList( cAlphaArgs( 1 ), Surface, TotSurfaces );
			StormWindow( StormWinNum ).StormWinMaterialNum = FindItemInList( cAlphaArgs( 2 ), Material, TotMaterials );
			StormWindow( StormWinNum ).StormWinDistance = rNumericArgs( 1 );
			StormWindow( StormWinNum ).MonthOn = rNumericArgs( 2 );
			StormWindow( StormWinNum ).DayOfMonthOn = rNumericArgs( 3 );
			StormWindow( StormWinNum ).DateOn = JulianDay( StormWindow( StormWinNum ).MonthOn, StormWindow( StormWinNum ).DayOfMonthOn, 1 );
			StormWindow( StormWinNum ).MonthOff = rNumericArgs( 4 );
			StormWindow( StormWinNum ).DayOfMonthOff = rNumericArgs( 5 );
			StormWindow( StormWinNum ).DateOff = JulianDay( StormWindow( StormWinNum ).MonthOff, StormWindow( StormWinNum ).DayOfMonthOff, 1 );

			if ( StormWindow( StormWinNum ).DateOn == StormWindow( StormWinNum ).DateOff ) {
				ShowSevereError( cCurrentModuleObject + ": Date On = Date Off -- not allowed, occured in WindowProperty:StormWindow Input #" + TrimSigDigits( StormWinNum ) );
				ErrorsFound = true;
			}

			{ auto const SELECT_CASE_var( StormWindow( StormWinNum ).MonthOn );

			if ( ( SELECT_CASE_var == 1 ) || ( SELECT_CASE_var == 3 ) || ( SELECT_CASE_var == 5 ) || ( SELECT_CASE_var == 7 ) || ( SELECT_CASE_var == 8 ) || ( SELECT_CASE_var == 10 ) || ( SELECT_CASE_var == 12 ) ) {
				if ( StormWindow( StormWinNum ).DayOfMonthOn > 31 ) {
					ShowSevereError( cCurrentModuleObject + ": Date On (Day of Month) [" + TrimSigDigits( StormWindow( StormWinNum ).DayOfMonthOn ) + "], invalid for WindowProperty:StormWindow Input #" + TrimSigDigits( StormWinNum ) );
					ErrorsFound = true;
				}
			} else if ( ( SELECT_CASE_var == 4 ) || ( SELECT_CASE_var == 6 ) || ( SELECT_CASE_var == 9 ) || ( SELECT_CASE_var == 11 ) ) {
				if ( StormWindow( StormWinNum ).DayOfMonthOn > 30 ) {
					ShowSevereError( cCurrentModuleObject + ": Date On (Day of Month) [" + TrimSigDigits( StormWindow( StormWinNum ).DayOfMonthOn ) + "], invalid for WindowProperty:StormWindow Input #" + TrimSigDigits( StormWinNum ) );
					ErrorsFound = true;
				}
			} else if ( SELECT_CASE_var == 2 ) {
				if ( StormWindow( StormWinNum ).DayOfMonthOn > 29 ) {
					ShowSevereError( cCurrentModuleObject + ": Date On (Day of Month) [" + TrimSigDigits( StormWindow( StormWinNum ).DayOfMonthOn ) + "], invalid for WindowProperty:StormWindow Input #" + TrimSigDigits( StormWinNum ) );
					ErrorsFound = true;
				}
			} else {
				ShowSevereError( cCurrentModuleObject + ": Date On Month [" + TrimSigDigits( StormWindow( StormWinNum ).MonthOn ) + "], invalid for WindowProperty:StormWindow Input #" + TrimSigDigits( StormWinNum ) );
				ErrorsFound = true;
			}}
			{ auto const SELECT_CASE_var( StormWindow( StormWinNum ).MonthOff );

			if ( ( SELECT_CASE_var == 1 ) || ( SELECT_CASE_var == 3 ) || ( SELECT_CASE_var == 5 ) || ( SELECT_CASE_var == 7 ) || ( SELECT_CASE_var == 8 ) || ( SELECT_CASE_var == 10 ) || ( SELECT_CASE_var == 12 ) ) {
				if ( StormWindow( StormWinNum ).DayOfMonthOff > 31 ) {
					ShowSevereError( cCurrentModuleObject + ": Date Off (Day of Month) [" + TrimSigDigits( StormWindow( StormWinNum ).DayOfMonthOff ) + "], invalid for WindowProperty:StormWindow Input #" + TrimSigDigits( StormWinNum ) );
					ErrorsFound = true;
				}
			} else if ( ( SELECT_CASE_var == 4 ) || ( SELECT_CASE_var == 6 ) || ( SELECT_CASE_var == 9 ) || ( SELECT_CASE_var == 11 ) ) {
				if ( StormWindow( StormWinNum ).DayOfMonthOff > 30 ) {
					ShowSevereError( cCurrentModuleObject + ": Date Off (Day of Month) [" + TrimSigDigits( StormWindow( StormWinNum ).DayOfMonthOff ) + "], invalid for WindowProperty:StormWindow Input #" + TrimSigDigits( StormWinNum ) );
					ErrorsFound = true;
				}
			} else if ( SELECT_CASE_var == 2 ) {
				if ( StormWindow( StormWinNum ).DayOfMonthOff > 29 ) {
					ShowSevereError( cCurrentModuleObject + ": Date Off (Day of Month) [" + TrimSigDigits( StormWindow( StormWinNum ).DayOfMonthOff ) + "], invalid for WindowProperty:StormWindow Input #" + TrimSigDigits( StormWinNum ) );
					ErrorsFound = true;
				}
			} else {
				ShowSevereError( cCurrentModuleObject + ": Date Off Month [" + TrimSigDigits( StormWindow( StormWinNum ).MonthOff ) + "], invalid for WindowProperty:StormWindow Input #" + TrimSigDigits( StormWinNum ) );
				ErrorsFound = true;
			}}
		}

		// Error checks

		for ( StormWinNum = 1; StormWinNum <= TotStormWin; ++StormWinNum ) {
			// Require BaseWindowNum be that of an exterior window
			SurfNum = StormWindow( StormWinNum ).BaseWindowNum;
			if ( SurfNum == 0 ) {
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid." );
				ErrorsFound = true;
			} else {
				if ( Surface( SurfNum ).Class != SurfaceClass_Window || Surface( SurfNum ).ExtBoundCond != 0 ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
					ShowSevereError( "cannot be used with surface=" + Surface( SurfNum ).Name );
					ShowContinueError( "because that surface is not an exterior window." );
					ErrorsFound = true;
				}
			}

			// Require that storm window material be glass
			MatNum = StormWindow( StormWinNum ).StormWinMaterialNum;
			if ( SurfNum > 0 ) {
				if ( MatNum == 0 ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
					ShowContinueError( cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\" not found as storm window layer." );
					ErrorsFound = true;
				} else {
					if ( Material( MatNum ).Group != WindowGlass ) {
						ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
						ShowContinueError( cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "must be a WindowMaterial:Glazing or WindowMaterial:Glazing:RefractionExtinctionMethod" );
						ErrorsFound = true;
					}
				}
			}

			// Error if base window has airflow control
			if ( SurfNum > 0 ) {
				if ( SurfaceWindow( SurfNum ).AirflowControlType != 0 ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\"" );
					ShowContinueError( " cannot be used because it is an airflow window (i.e., has WindowProperty:AirflowControl specified)" );
					ErrorsFound = true;
				}
			}

			// Check for reversal of on and off times
			if ( SurfNum > 0 ) {
				if ( ( Latitude > 0.0 && ( StormWindow( StormWinNum ).MonthOn < StormWindow( StormWinNum ).MonthOff ) ) || ( Latitude <= 0.0 && ( StormWindow( StormWinNum ).MonthOn > StormWindow( StormWinNum ).MonthOff ) ) ) {
					ShowWarningError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" check times that storm window" );
					ShowContinueError( "is put on (month=" + TrimSigDigits( StormWindow( StormWinNum ).MonthOn ) + ", day=" + TrimSigDigits( StormWindow( StormWinNum ).DayOfMonthOn ) + ") and taken off (month=" + TrimSigDigits( StormWindow( StormWinNum ).MonthOff ) + ", day=" + TrimSigDigits( StormWindow( StormWinNum ).DayOfMonthOff ) + ");" );
					ShowContinueError( "these times may be reversed for your building latitude=" + TrimSigDigits( Latitude, 2 ) + " deg." );
				}
			}
		}

	}

	void
	GetWindowGapAirflowControlData( bool & ErrorsFound ) // If errors found in input
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   Feb 2003
		//       MODIFIED       June 2003, FCW: add destination = return air;
		//                        more error messages
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Reads in the window airflow control information from the input data file,
		// interprets it and puts it in the SurfaceWindow derived type

		// METHODOLOGY EMPLOYED: na
		// REFERENCES: na

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using InputProcessor::SameString;
		using ScheduleManager::GetScheduleIndex;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:na
		// INTERFACE BLOCK SPECIFICATIONS:na
		// DERIVED TYPE DEFINITIONS:na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int IOStat; // IO Status when calling get input subroutine
		int ControlNumAlpha; // Number of control alpha names being passed
		int ControlNumProp; // Number of control properties being passed
		int TotWinAirflowControl; // Total window airflow control statements
		bool WrongSurfaceType; // True if associated surface is not 2- or 3-pane exterior window
		int Loop;
		int SurfNum; // Surface number
		int ConstrNum( 0 ); // Construction number
		int ConstrNumSh; // Shaded Construction number
		int WSCPtr; // Window shading control pointer
		int MatGapFlow; // Material number of gas in airflow gap of window's construction
		int MatGapFlow1; // Material number of gas on either side of a between-glass shade/blind
		int MatGapFlow2;
		// of the shaded construction of airflow window

		// Get the total number of window airflow control statements
		cCurrentModuleObject = "WindowProperty:AirflowControl";
		TotWinAirflowControl = GetNumObjectsFound( cCurrentModuleObject );
		if ( TotWinAirflowControl == 0 ) return;

		for ( Loop = 1; Loop <= TotWinAirflowControl; ++Loop ) { // Loop through all surfaces in the input...

			GetObjectItem( cCurrentModuleObject, Loop, cAlphaArgs, ControlNumAlpha, rNumericArgs, ControlNumProp, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			SurfNum = FindItemInList( cAlphaArgs( 1 ), Surface, TotSurfaces );
			if ( SurfNum == 0 ) {
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" not found." );
				ErrorsFound = true;
			}
			// Check that associated surface is a 2- or 3-pane exterior window
			WrongSurfaceType = false;
			if ( SurfNum != 0 ) {
				if ( Surface( SurfNum ).Class != SurfaceClass_Window ) WrongSurfaceType = true;
				if ( Surface( SurfNum ).Class == SurfaceClass_Window ) {
					ConstrNum = Surface( SurfNum ).Construction;
					if ( Construct( ConstrNum ).TotGlassLayers != 2 && Construct( ConstrNum ).TotGlassLayers != 3 ) WrongSurfaceType = true;
					if ( Surface( SurfNum ).ExtBoundCond != ExternalEnvironment ) WrongSurfaceType = true;
				}
				if ( WrongSurfaceType ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" is not an exterior window with 2 or 3 glass layers." );
					ErrorsFound = true;
				}
			}

			// Error if illegal airflow source
			if ( cAlphaArgs( 2 ) != "INDOORAIR" && cAlphaArgs( 2 ) != "OUTDOORAIR" ) {
				ErrorsFound = true;
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\"" );
			}

			// Error if illegal airflow destination
			if ( cAlphaArgs( 3 ) != "INDOORAIR" && cAlphaArgs( 3 ) != "OUTDOORAIR" && cAlphaArgs( 3 ) != "RETURNAIR" ) {
				ErrorsFound = true;
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 3 ) + "\"" );
			}

			// Error if source = OutsideAir and destination = ReturnAir
			if ( cAlphaArgs( 2 ) == "OUTDOORAIR" && cAlphaArgs( 3 ) == "RETURNAIR" ) {
				ErrorsFound = true;
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\"" );
				ShowContinueError( "..when " + cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 3 ) + "\"" );
			}

			// Error if illegal airflow control type
			if ( cAlphaArgs( 4 ) != "ALWAYSONATMAXIMUMFLOW" && cAlphaArgs( 4 ) != "ALWAYSOFF" && cAlphaArgs( 4 ) != "SCHEDULEDONLY" ) {
				ErrorsFound = true;
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 4 ) + "=\"" + cAlphaArgs( 4 ) + "\"" );
			}

			// Error if illegal value for Airflow Has Multiplier Schedule
			if ( cAlphaArgs( 5 ) != "YES" && cAlphaArgs( 5 ) != "NO" ) {
				ErrorsFound = true;
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 5 ) + "=\"" + cAlphaArgs( 5 ) + "\"" );
			}

			// Error if Airflow Control Type = ScheduledOnly and Airflow Has Multiplier Schedule = No
			if ( cAlphaArgs( 4 ) == "SCHEDULEDONLY" && cAlphaArgs( 5 ) == "NO" ) {
				ErrorsFound = true;
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" invalid " + cAlphaFieldNames( 4 ) + "=\"" + cAlphaArgs( 4 ) + "\"" );
				ShowContinueError( "..when " + cAlphaFieldNames( 5 ) + "=\"" + cAlphaArgs( 5 ) + "\"" );

			}

			// Warning if Airflow Control Type = AlwaysOnAtMaxFlow and Airflow Has Multiplier Schedule = Yes
			if ( cAlphaArgs( 4 ) == "ALWAYSONATMAXIMUMFLOW" && cAlphaArgs( 5 ) == "YES" ) {
				ShowWarningError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "has " + cAlphaFieldNames( 4 ) + "=\"" + cAlphaArgs( 4 ) + "\"" );
				ShowContinueError( "..but " + cAlphaFieldNames( 5 ) + "=\"" + cAlphaArgs( 5 ) + "If specified, the " + cAlphaFieldNames( 5 ) + " will be ignored." );
			}

			// Warning if Airflow Control Type = AlwaysOff and Airflow Has Multiplier Schedule = Yes
			if ( cAlphaArgs( 4 ) == "ALWAYSOFF" && cAlphaArgs( 5 ) == "YES" ) {
				ShowWarningError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "has " + cAlphaFieldNames( 4 ) + "=\"" + cAlphaArgs( 4 ) + "\"" );
				ShowContinueError( "..but " + cAlphaFieldNames( 5 ) + "=\"" + cAlphaArgs( 5 ) + "\". If specified, the " + cAlphaFieldNames( 5 ) + " will be ignored." );
			}

			if ( SurfNum > 0 ) {
				AirflowWindows = true;
				if ( SameString( cAlphaArgs( 2 ), "IndoorAir" ) ) {
					SurfaceWindow( SurfNum ).AirflowSource = AirFlowWindow_Source_IndoorAir;
				} else if ( SameString( cAlphaArgs( 2 ), "OutdoorAir" ) ) {
					SurfaceWindow( SurfNum ).AirflowSource = AirFlowWindow_Source_OutdoorAir;
				}
				if ( SameString( cAlphaArgs( 3 ), "IndoorAir" ) ) {
					SurfaceWindow( SurfNum ).AirflowDestination = AirFlowWindow_Destination_IndoorAir;
				} else if ( SameString( cAlphaArgs( 3 ), "OutdoorAir" ) ) {
					SurfaceWindow( SurfNum ).AirflowDestination = AirFlowWindow_Destination_OutdoorAir;
				} else if ( SameString( cAlphaArgs( 3 ), "ReturnAir" ) ) {
					SurfaceWindow( SurfNum ).AirflowDestination = AirFlowWindow_Destination_ReturnAir;
				}
				if ( SameString( cAlphaArgs( 4 ), "AlwaysOnAtMaximumFlow" ) ) {
					SurfaceWindow( SurfNum ).AirflowControlType = AirFlowWindow_ControlType_MaxFlow;
				} else if ( SameString( cAlphaArgs( 4 ), "AlwaysOff" ) ) {
					SurfaceWindow( SurfNum ).AirflowControlType = AirFlowWindow_ControlType_AlwaysOff;
				} else if ( SameString( cAlphaArgs( 4 ), "ScheduledOnly" ) ) {
					SurfaceWindow( SurfNum ).AirflowControlType = AirFlowWindow_ControlType_Schedule;
				}
				SurfaceWindow( SurfNum ).MaxAirflow = rNumericArgs( 1 );
				if ( cAlphaArgs( 4 ) == "SCHEDULEDONLY" && cAlphaArgs( 5 ) == "YES" ) {
					if ( lAlphaFieldBlanks( 6 ) ) {
						ErrorsFound = true;
						ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", has " + cAlphaFieldNames( 4 ) + "=\"" + cAlphaArgs( 4 ) + "\"" );
						ShowContinueError( "..and " + cAlphaFieldNames( 5 ) + "=\"" + cAlphaArgs( 5 ) + "\", but no " + cAlphaFieldNames( 6 ) + " specified." );
					} else {
						SurfaceWindow( SurfNum ).AirflowHasSchedule = true;
						SurfaceWindow( SurfNum ).AirflowSchedulePtr = GetScheduleIndex( cAlphaArgs( 6 ) );
						if ( SurfaceWindow( SurfNum ).AirflowSchedulePtr == 0 ) {
							ErrorsFound = true;
							ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid " + cAlphaFieldNames( 6 ) + "=\"" + cAlphaArgs( 6 ) + "\"" );
						}
					}
				}
				// Warning if associated window is an interior window
				if ( Surface( SurfNum ).ExtBoundCond != ExternalEnvironment && ! ErrorsFound ) ShowWarningError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", is an Interior window; cannot be an airflow window." );
				if ( ! ErrorsFound ) {
					// Require that gas in airflow gap has type = air
					MatGapFlow = Construct( ConstrNum ).LayerPoint( 2 );
					if ( Construct( ConstrNum ).TotGlassLayers == 3 ) MatGapFlow = Construct( ConstrNum ).LayerPoint( 4 );
					if ( Material( MatGapFlow ).GasType( 1 ) != 1 ) {
						ErrorsFound = true;
						ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", Gas type not air in airflow gap of construction " + Construct( ConstrNum ).Name );
					}
					// Require that gas be air in airflow gaps on either side of a between glass shade/blind
					WSCPtr = Surface( SurfNum ).WindowShadingControlPtr;
					if ( WSCPtr > 0 ) {
						if ( WindowShadingControl( WSCPtr ).ShadingType == WSC_ST_BetweenGlassShade || WindowShadingControl( WSCPtr ).ShadingType == WSC_ST_BetweenGlassBlind ) {
							ConstrNumSh = WindowShadingControl( WSCPtr ).ShadedConstruction;
							if ( Construct( ConstrNum ).TotGlassLayers == 2 ) {
								MatGapFlow1 = Construct( ConstrNumSh ).LayerPoint( 2 );
								MatGapFlow2 = Construct( ConstrNumSh ).LayerPoint( 4 );
							} else {
								MatGapFlow1 = Construct( ConstrNumSh ).LayerPoint( 4 );
								MatGapFlow2 = Construct( ConstrNumSh ).LayerPoint( 6 );
							}
							if ( Material( MatGapFlow1 ).GasType( 1 ) != 1 || Material( MatGapFlow2 ).GasType( 1 ) != 1 ) {
								ErrorsFound = true;
								ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", gas type must be air on either side of the shade/blind" );
							}
						}
					}
				}
			}

		} // End of loop over window airflow controls

	}

	void
	GetOSCData( bool & ErrorsFound )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   May 2000
		//       MODIFIED       Jul 2011, M.J. Witte and C.O. Pedersen, add new fields to OSC for last T, max and min
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine gets the OtherSideCoefficient data.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// Other Side Coefficient Definition
		// OtherSideCoefficients,
		//       \memo This object sets the other side conditions for a surface in a variety of ways.
		//   A1, \field OtherSideCoeff Name
		//       \required-field
		//       \reference OSCNames
		//       \reference OutFaceEnvNames
		//   N1, \field Combined convective/radiative film coefficient
		//       \required-field
		//       \type real
		//       \note if>0, N1 becomes exterior convective/radiative film coefficient and other fields
		//       \note are used to calc outside air temp then exterior surface temp based on outside air
		//       \note and specified coefficient
		//       \note if<=0, then remaining fields calculate the outside surface temperature(?)
		//       \note following fields are used in the equation:
		//       \note SurfTemp=N7*TempZone + N4*OutsideDryBulb + N2*N3 + GroundTemp*N5 + WindSpeed*N6*OutsideDryBulb
		//   N2, \field User selected Constant Temperature
		//       \units C
		//       \type real
		//       \note This parameter will be overwritten by the values from the schedule(A2 below) if one is present
		//   N3, \field Coefficient modifying the user selected constant temperature
		//       \note This coefficient is used even with a schedule.  It should normally be 1.0 in that case
		//   N4, \field Coefficient modifying the external dry bulb temperature
		//       \type real
		//   N5, \field Coefficient modifying the ground temperature
		//       \type real
		//   N6, \field Coefficient modifying the wind speed term (s/m)
		//       \type real
		//   N7, \field Coefficient modifying the zone air temperature part of the equation
		//       \type real
		//   A2, \field ScheduleName for constant temperature
		//       \note Name of Schedule for values of "const" temperature.
		//       \note Schedule values replace N2 - User selected constant temperature.
		//       \type object-list
		//       \object-list ScheduleNames
		//   A3, \field Sinusoidal Variation of Constant Temperature Coefficient
		//       \note Optionally used to vary Constant Temperature Coefficient with unitary sine wave
		//       \type choice
		//       \key Yes
		//       \key No
		//       \default No
		//   N8; \field Period of Sinusoidal Variation
		//       \note Use with sinusoidal variation to define the time period
		//       \type real
		//       \units hr
		//       \default 24
		//  N9, \field Previous Other Side Temperature Coefficient
		//      \note This coeffient multiplies the other side temperature result from the
		//      \note previous zone timestep
		//      \type real
		//      \default 0
		// N10, \field Minimum Other Side Temperature
		//      \type real
		//      \units C
		//      \default -100
		// N11; \field Maximum Other Side Temperature
		//      \type real
		//      \units C
		//      \default 200

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using InputProcessor::VerifyName;
		using InputProcessor::SameString;
		using ScheduleManager::GetScheduleIndex;
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static gio::Fmt OSCFormat1( "('! <Other Side Coefficients>,Name,Combined convective/radiative film coefficient {W/m2-K},User selected Constant Temperature {C},Coefficient modifying the constant temperature term,Coefficient modifying the external dry bulb temperature term,Coefficient modifying the ground temperature term,Coefficient modifying the wind speed term {s/m},Coefficient modifying the zone air temperature term,Constant Temperature Schedule Name,Sinusoidal Variation,Period of Sinusoidal Variation,Previous Other Side Temperature Coefficient,Minimum Other Side Temperature {C},Maximum Other Side Temperature {C}')" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumAlphas;
		int NumProps;
		int Loop;
		int IOStat;
		int OSCNum;
		bool ErrorInName;
		bool IsBlank;
		std::string cOSCLimitsString;

		cCurrentModuleObject = "SurfaceProperty:OtherSideCoefficients";
		TotOSC = GetNumObjectsFound( cCurrentModuleObject );
		OSC.allocate( TotOSC );

		OSCNum = 0;
		for ( Loop = 1; Loop <= TotOSC; ++Loop ) {
			GetObjectItem( cCurrentModuleObject, Loop, cAlphaArgs, NumAlphas, rNumericArgs, NumProps, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			ErrorInName = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), OSC, OSCNum, ErrorInName, IsBlank, cCurrentModuleObject + " Name" );
			if ( ErrorInName ) {
				ErrorsFound = true;
				continue;
			}

			++OSCNum;
			OSC( OSCNum ).Name = cAlphaArgs( 1 );
			OSC( OSCNum ).SurfFilmCoef = rNumericArgs( 1 );
			OSC( OSCNum ).ConstTemp = rNumericArgs( 2 ); //  This will be replaced if  schedule is used
			OSC( OSCNum ).ConstTempCoef = rNumericArgs( 3 ); //  This multiplier is used (even with schedule).  It should normally be 1.0
			OSC( OSCNum ).ExtDryBulbCoef = rNumericArgs( 4 );
			OSC( OSCNum ).GroundTempCoef = rNumericArgs( 5 );
			OSC( OSCNum ).WindSpeedCoef = rNumericArgs( 6 );
			OSC( OSCNum ).ZoneAirTempCoef = rNumericArgs( 7 );
			OSC( OSCNum ).SinusoidPeriod = rNumericArgs( 8 );

			if ( ( ! lAlphaFieldBlanks( 2 ) ) && ( NumAlphas != 1 ) ) { //  Const temp will come from schedule specified below.
				OSC( OSCNum ).ConstTempScheduleName = cAlphaArgs( 2 );
				if ( ! OSC( OSCNum ).ConstTempScheduleName.empty() ) {
					OSC( OSCNum ).ConstTempScheduleIndex = GetScheduleIndex( OSC( OSCNum ).ConstTempScheduleName );
					if ( OSC( OSCNum ).ConstTempScheduleIndex == 0 ) {
						ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) );
						ErrorsFound = true;
					}
				}
			}

			if ( ! lAlphaFieldBlanks( 3 ) ) {

				if ( SameString( cAlphaArgs( 3 ), "No" ) ) {
					OSC( OSCNum ).SinusoidalConstTempCoef = false;
				} else if ( SameString( cAlphaArgs( 3 ), "Yes" ) ) {
					OSC( OSCNum ).SinusoidalConstTempCoef = true;
				} else {
					ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid " + cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 3 ) );
					ErrorsFound = true;
				}
			}

			if ( rNumericArgs( 1 ) > 0.0 && ! any_ne( rNumericArgs( {3,7} ), 0.0 ) && ( ! OSC( OSCNum ).SinusoidalConstTempCoef ) ) {
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" has zeros for all coefficients." );
				ShowContinueError( "...The outdoor air temperature for surfaces using this OtherSideCoefficients object will always be 0C." );
			}

			if ( rNumericArgs( 1 ) <= 0.0 && ! any_ne( rNumericArgs( {3,7} ), 0.0 ) && ( ! OSC( OSCNum ).SinusoidalConstTempCoef ) ) {
				ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\" has zeros for all coefficients." );
				ShowContinueError( "...The outside surface temperature for surfaces using this OtherSideCoefficients object will always be 0C." );
			}

			OSC( OSCNum ).TPreviousCoef = rNumericArgs( 9 );

			if ( ! lNumericFieldBlanks( 10 ) ) {
				OSC( OSCNum ).MinLimitPresent = true;
				OSC( OSCNum ).MinTempLimit = rNumericArgs( 10 );
				cOSCLimitsString = RoundSigDigits( rNumericArgs( 10 ), 3 );
			} else {
				cOSCLimitsString = "N/A";
			}
			if ( ! lNumericFieldBlanks( 11 ) ) {
				OSC( OSCNum ).MaxLimitPresent = true;
				OSC( OSCNum ).MaxTempLimit = rNumericArgs( 11 );
				cOSCLimitsString += "," + RoundSigDigits( rNumericArgs( 10 ), 3 );
			} else {
				cOSCLimitsString += ",N/A";
			}

		}

		for ( Loop = 1; Loop <= TotOSC; ++Loop ) {
			if ( Loop == 1 ) {
				gio::write( OutputFileInits, OSCFormat1 );
			}
			if ( OSC( Loop ).SurfFilmCoef > 0.0 ) {
				cAlphaArgs( 1 ) = RoundSigDigits( OSC( Loop ).SurfFilmCoef, 3 );
				SetupOutputVariable( "Surface Other Side Coefficients Exterior Air Drybulb Temperature [C]", OSC( Loop ).OSCTempCalc, "System", "Average", OSC( Loop ).Name );
			} else {
				cAlphaArgs( 1 ) = "N/A";
			}
			if ( OSC( Loop ).ConstTempScheduleIndex != 0 ) {
				cAlphaArgs( 2 ) = OSC( Loop ).ConstTempScheduleName;
				gio::write( OutputFileInits, fmtA ) << "Other Side Coefficients," + OSC( Loop ).Name + ',' + cAlphaArgs( 1 ) + ",N/A," + RoundSigDigits( OSC( Loop ).ConstTempCoef, 3 ) + ',' + RoundSigDigits( OSC( Loop ).ExtDryBulbCoef, 3 ) + ',' + RoundSigDigits( OSC( Loop ).GroundTempCoef, 3 ) + ',' + RoundSigDigits( OSC( Loop ).WindSpeedCoef, 3 ) + ',' + RoundSigDigits( OSC( Loop ).ZoneAirTempCoef, 3 ) + ',' + cAlphaArgs( 2 ) + ',' + cAlphaArgs( 3 ) + ',' + RoundSigDigits( OSC( Loop ).SinusoidPeriod, 3 ) + ',' + RoundSigDigits( OSC( Loop ).TPreviousCoef, 3 ) + ',' + cOSCLimitsString;
			} else {
				cAlphaArgs( 2 ) = "N/A";
				gio::write( OutputFileInits, fmtA ) << "Other Side Coefficients," + OSC( Loop ).Name + ',' + cAlphaArgs( 1 ) + ',' + RoundSigDigits( OSC( Loop ).ConstTemp, 2 ) + ',' + RoundSigDigits( OSC( Loop ).ConstTempCoef, 3 ) + ',' + RoundSigDigits( OSC( Loop ).ExtDryBulbCoef, 3 ) + ',' + RoundSigDigits( OSC( Loop ).GroundTempCoef, 3 ) + ',' + RoundSigDigits( OSC( Loop ).WindSpeedCoef, 3 ) + ',' + RoundSigDigits( OSC( Loop ).ZoneAirTempCoef, 3 ) + ',' + cAlphaArgs( 2 ) + ',' + cAlphaArgs( 3 ) + ',' + RoundSigDigits( OSC( Loop ).SinusoidPeriod, 3 ) + ',' + RoundSigDigits( OSC( Loop ).TPreviousCoef, 3 ) + ',' + cOSCLimitsString;
			}

		}

	}

	void
	GetOSCMData( bool & ErrorsFound )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   November 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine gets the OtherSideConditionsModel data.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// derived from GetOSCData subroutine by Linda Lawrie

		//  OtherSideConditionsModel,
		//      \memo This object sets up modifying the other side conditions for a surface from other model results.
		//  A1, \field OtherSideConditionsModel Name
		//      \required-field
		//      \reference OSCMNames
		//      \reference OutFaceEnvNames
		//  A2; \field Type of Model to determine Boundary Conditions
		//      \type choice
		//      \key Transpired Collector
		//      \key Vented PV Cavity
		//      \key Hybrid PV Transpired Collector

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using InputProcessor::VerifyName;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static gio::Fmt OSCMFormat1( "('! <Other Side Conditions Model>,Name,Class')" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumAlphas;
		int NumProps;
		int Loop;
		int IOStat;
		int OSCMNum;
		bool ErrorInName;
		bool IsBlank;

		cCurrentModuleObject = "SurfaceProperty:OtherSideConditionsModel";
		TotOSCM = GetNumObjectsFound( cCurrentModuleObject );
		OSCM.allocate( TotOSCM );
		// OSCM is already initialized in derived type defn.

		OSCMNum = 0;
		for ( Loop = 1; Loop <= TotOSCM; ++Loop ) {
			GetObjectItem( cCurrentModuleObject, Loop, cAlphaArgs, NumAlphas, rNumericArgs, NumProps, IOStat );
			ErrorInName = false;
			IsBlank = false;
			VerifyName( cAlphaArgs( 1 ), OSCM, OSCMNum, ErrorInName, IsBlank, cCurrentModuleObject + " Name" );
			if ( ErrorInName ) {
				ErrorsFound = true;
				continue;
			}

			++OSCMNum;
			OSCM( OSCMNum ).Name = cAlphaArgs( 1 );
			// Note no validation of the below at this time:
			OSCM( OSCMNum ).Class = cAlphaArgs( 2 );
			// setup output vars for modeled coefficients
			SetupOutputVariable( "Surface Other Side Conditions Modeled Convection Air Temperature [C]", OSCM( OSCMNum ).TConv, "System", "Average", OSCM( OSCMNum ).Name );
			SetupOutputVariable( "Surface Other Side Conditions Modeled Convection Heat Transfer Coefficient [W/m2-K]", OSCM( OSCMNum ).HConv, "System", "Average", OSCM( OSCMNum ).Name );
			SetupOutputVariable( "Surface Other Side Conditions Modeled Radiation Temperature [C]", OSCM( OSCMNum ).TRad, "System", "Average", OSCM( OSCMNum ).Name );
			SetupOutputVariable( "Surface Other Side Conditions Modeled Radiation Heat Transfer Coefficient [W/m2-K]", OSCM( OSCMNum ).HRad, "System", "Average", OSCM( OSCMNum ).Name );

			if ( AnyEnergyManagementSystemInModel ) {
				SetupEMSActuator( "Other Side Boundary Conditions", OSCM( OSCMNum ).Name, "Convection Bulk Air Temperature", "[C]", OSCM( OSCMNum ).EMSOverrideOnTConv, OSCM( OSCMNum ).EMSOverrideTConvValue );
				SetupEMSActuator( "Other Side Boundary Conditions", OSCM( OSCMNum ).Name, "Convection Heat Transfer Coefficient", "[W/m2-K]", OSCM( OSCMNum ).EMSOverrideOnHConv, OSCM( OSCMNum ).EMSOverrideHConvValue );
				SetupEMSActuator( "Other Side Boundary Conditions", OSCM( OSCMNum ).Name, "Radiation Effective Temperature", "[C]", OSCM( OSCMNum ).EMSOverrideOnTRad, OSCM( OSCMNum ).EMSOverrideTRadValue );
				SetupEMSActuator( "Other Side Boundary Conditions", OSCM( OSCMNum ).Name, "Radiation Linear Heat Transfer Coefficient", "[W/m2-K]", OSCM( OSCMNum ).EMSOverrideOnHrad, OSCM( OSCMNum ).EMSOverrideHradValue );
			}
		}

		for ( Loop = 1; Loop <= TotOSCM; ++Loop ) {
			if ( Loop == 1 ) {
				gio::write( OutputFileInits, OSCMFormat1 );
			}
			gio::write( OutputFileInits, fmtA ) << "Other Side Conditions Model," + OSCM( Loop ).Name + ',' + OSCM( Loop ).Class;
		}

	}

	void
	GetMovableInsulationData( bool & ErrorsFound ) // If errors found in input
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   May 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine gets the movable insulation data that can be associated with
		// a surface.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// Movable Insulation Definition
		// SurfaceControl:MovableInsulation,
		//       \memo Exterior or Interior Insulation on opaque surfaces
		//   A1, \field Insulation Type
		//       \required-field
		//       \type choice
		//       \key Outside
		//       \key Inside
		//   A2, \field Surface Name
		//       \required-field
		//       \type object-list
		//       \object-list SurfaceNames
		//   A3, \field Material Name
		//       \required-field
		//       \object-list MaterialName
		//   A4; \field Schedule Name
		//        \required-field
		//        \type object-list
		//        \object-list ScheduleNames

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using InputProcessor::VerifyName;
		using InputProcessor::SameString;
		using ScheduleManager::GetScheduleIndex;
		using General::TrimSigDigits;
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NAlphas;
		int NNums;
		int IOStat;
		int Loop;
		int NMatInsul;
		int SurfNum;
		int MaterNum;
		int SchNum;
		int InslType;

		cCurrentModuleObject = "SurfaceControl:MovableInsulation";
		NMatInsul = GetNumObjectsFound( cCurrentModuleObject );
		for ( Loop = 1; Loop <= NMatInsul; ++Loop ) {
			GetObjectItem( cCurrentModuleObject, Loop, cAlphaArgs, NAlphas, rNumericArgs, NNums, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			SurfNum = FindItemInList( cAlphaArgs( 2 ), SurfaceTmp, TotSurfaces );
			MaterNum = FindItemInList( cAlphaArgs( 3 ), Material, TotMaterials );
			SchNum = GetScheduleIndex( cAlphaArgs( 4 ) );
			if ( SameString( cAlphaArgs( 1 ), "Outside" ) ) {
				InslType = 1;
			} else if ( SameString( cAlphaArgs( 1 ), "Inside" ) ) {
				InslType = 2;
			} else {
				InslType = 0;
				ShowSevereError( cCurrentModuleObject + ", " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\", invalid data." );
				ShowContinueError( " invalid " + cAlphaFieldNames( 1 ) + "=\"" + cAlphaArgs( 1 ) + "\", [should be Inside or Outside]" );
				ErrorsFound = false;
			}
			if ( SurfNum == 0 ) {
				ShowSevereError( cCurrentModuleObject + ", " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\", invalid data." );
				ShowContinueError( " invalid (not found) " + cAlphaFieldNames( 2 ) );
				ErrorsFound = true;
			} else {
				if ( MaterNum == 0 ) {
					ShowSevereError( cCurrentModuleObject + ", " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\", invalid data." );
					ShowContinueError( " invalid (not found) " + cAlphaFieldNames( 3 ) + "=\"" + cAlphaArgs( 3 ) + "\"" );
					ErrorsFound = true;
				} else {
					if ( SchNum == 0 ) {
						ShowSevereError( cCurrentModuleObject + ", " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\", invalid data." );
						ShowContinueError( " invalid (not found) " + cAlphaFieldNames( 4 ) + "=\"" + cAlphaArgs( 4 ) + "\"" );
						ErrorsFound = true;
					} else {
						{ auto const SELECT_CASE_var( InslType );
						if ( SELECT_CASE_var == 1 ) {
							if ( SurfaceTmp( SurfNum ).MaterialMovInsulExt > 0 ) {
								ShowSevereError( cCurrentModuleObject + ", " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\", already assigned." );
								ShowContinueError( "\"Outside\", was already assigned Material=\"" + Material( SurfaceTmp( SurfNum ).MaterialMovInsulInt ).Name + "\"." );
								ShowContinueError( "attempting to assign Material=\"" + Material( MaterNum ).Name + "\"." );
								ErrorsFound = true;
							}
							SurfaceTmp( SurfNum ).MaterialMovInsulExt = MaterNum;
							SurfaceTmp( SurfNum ).SchedMovInsulExt = SchNum;
							if ( Material( MaterNum ).Resistance <= 0.0 ) {
								if ( Material( MaterNum ).Conductivity <= 0.0 || Material( MaterNum ).Thickness <= 0.0 ) {
									ShowSevereError( cCurrentModuleObject + ", " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\", invalid material." );
									ShowContinueError( "\"Outside\", invalid material for movable insulation." );
									ShowContinueError( "Material=\"" + Material( MaterNum ).Name + "\",Resistance=[" + RoundSigDigits( Material( MaterNum ).Resistance, 3 ) + "], must be > 0 for use in Movable Insulation." );
									ErrorsFound = true;
								} else if ( Material( MaterNum ).Conductivity > 0.0 ) {
									Material( MaterNum ).Resistance = Material( MaterNum ).Thickness / Material( MaterNum ).Conductivity;
								}
							}
							if ( Material( MaterNum ).Conductivity <= 0.0 ) {
								if ( Material( MaterNum ).Resistance <= 0.0 ) {
									ShowSevereError( cCurrentModuleObject + ", " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\", invalid material." );
									ShowContinueError( "\"Outside\", invalid material for movable insulation." );
									ShowContinueError( "Material=\"" + Material( MaterNum ).Name + "\",Conductivity=[" + RoundSigDigits( Material( MaterNum ).Conductivity, 3 ) + "], must be > 0 for use in Movable Insulation." );
									ErrorsFound = true;
								}
							}
						} else if ( SELECT_CASE_var == 2 ) {
							if ( SurfaceTmp( SurfNum ).MaterialMovInsulInt > 0 ) {
								ShowSevereError( cCurrentModuleObject + ", " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\", already assigned." );
								ShowContinueError( "\"Inside\", was already assigned Material=\"" + Material( SurfaceTmp( SurfNum ).MaterialMovInsulInt ).Name + "\"." );
								ShowContinueError( "attempting to assign Material=\"" + Material( MaterNum ).Name + "\"." );
								ErrorsFound = true;
							}
							SurfaceTmp( SurfNum ).MaterialMovInsulInt = MaterNum;
							SurfaceTmp( SurfNum ).SchedMovInsulInt = SchNum;
							if ( Material( MaterNum ).Resistance <= 0.0 ) {
								if ( Material( MaterNum ).Conductivity <= 0.0 || Material( MaterNum ).Thickness <= 0.0 ) {
									ShowSevereError( cCurrentModuleObject + ", " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\", invalid material." );
									ShowContinueError( "\"Inside\", invalid material for movable insulation." );
									ShowContinueError( "Material=\"" + Material( MaterNum ).Name + "\",Resistance=[" + RoundSigDigits( Material( MaterNum ).Resistance, 3 ) + "], must be > 0 for use in Movable Insulation." );
									ErrorsFound = true;
								} else if ( Material( MaterNum ).Conductivity > 0.0 ) {
									Material( MaterNum ).Resistance = Material( MaterNum ).Thickness / Material( MaterNum ).Conductivity;
								}
							}
						} else {
						}}
						if ( SurfaceTmp( SurfNum ).Class == SurfaceClass_Window ) {
							ShowSevereError( cCurrentModuleObject + ", " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\"" );
							ShowContinueError( "invalid use on a Window. Use WindowProperty:ShadingControl instead." );
							ErrorsFound = true;
						}
					}
				}
			}
		}

	}

	void
	CalculateZoneVolume(
		bool & ErrorsFound, // If errors found in input
		Array1S_bool const CeilingHeightEntered
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Legacy Code
		//       DATE WRITTEN   1992-1994
		//       MODIFIED       Sep 2007
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine calculates the volume (m3) of a zone using the
		// surfaces as possible.

		// METHODOLOGY EMPLOYED:
		// Uses surface area information for calculations.  Modified to use the
		// user-entered ceiling height (x floor area, if applicable) instead of using
		// the calculated volume when the user enters the ceiling height.

		// REFERENCES:
		// Legacy Code (IBLAST)

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using InputProcessor::GetNumSectionsFound;
		using namespace Vectors;
		using General::RoundSigDigits;

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static gio::Fmt VolFmt( "(F20.2)" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 MinimumVolume; // The minimum allowable Zone volume (equivalent to a ceiling height of 2.5 meters)
		Real64 SumAreas; // Sum of the Zone surface areas that are not "internal mass"
		Real64 SurfCount; // Surface Count
		int SurfNum; // Loop counter for surfaces
		int ZoneNum; // Loop counter for Zones
		bool ErrorFlag;
		Real64 TempVolume; // Temporary for calculating volume
		Array1D_int surfacenotused;
		int notused;
		int NFaces;
		int NActFaces;
		Real64 CalcVolume;
		bool initmsg;
		int iside;
		static bool ShowZoneSurfaces( false );
		static bool ShowZoneSurfaceHeaders( true );
		static int ErrCount( 0 );

		// Object Data
		Polyhedron ZoneStruct;

		initmsg = true;
		ShowZoneSurfaces = ( GetNumSectionsFound( "SHOWZONESURFACES_DEBUG" ) > 0 );

		for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {

			if ( ! Zone( ZoneNum ).HasFloor ) {
				ShowWarningError( "No floor exists in Zone=\"" + Zone( ZoneNum ).Name + "\", zone floor area is zero. All values for this zone that are entered per floor area will be zero." );
			}

			SumAreas = 0.0;
			SurfCount = 0.0;
			NFaces = Zone( ZoneNum ).SurfaceLast - Zone( ZoneNum ).SurfaceFirst + 1;
			notused = 0;
			ZoneStruct.NumSurfaceFaces = NFaces;
			ZoneStruct.SurfaceFace.allocate( NFaces );
			NActFaces = 0;
			surfacenotused.dimension( NFaces, 0 );

			for ( SurfNum = Zone( ZoneNum ).SurfaceFirst; SurfNum <= Zone( ZoneNum ).SurfaceLast; ++SurfNum ) {

				// Only include Base Surfaces in Calc.

				if ( Surface( SurfNum ).Class != SurfaceClass_Wall && Surface( SurfNum ).Class != SurfaceClass_Floor && Surface( SurfNum ).Class != SurfaceClass_Roof ) {
					++notused;
					surfacenotused( notused ) = SurfNum;
					continue;
				}

				++NActFaces;
				ZoneStruct.SurfaceFace( NActFaces ).FacePoints.allocate( Surface( SurfNum ).Sides );
				ZoneStruct.SurfaceFace( NActFaces ).NSides = Surface( SurfNum ).Sides;
				ZoneStruct.SurfaceFace( NActFaces ).SurfNum = SurfNum;
				ZoneStruct.SurfaceFace( NActFaces ).FacePoints( {1,Surface( SurfNum ).Sides} ) = Surface( SurfNum ).Vertex( {1,Surface( SurfNum ).Sides} );
				CreateNewellAreaVector( ZoneStruct.SurfaceFace( NActFaces ).FacePoints, ZoneStruct.SurfaceFace( NActFaces ).NSides, ZoneStruct.SurfaceFace( NActFaces ).NewellAreaVector );
				SumAreas += VecLength( ZoneStruct.SurfaceFace( NActFaces ).NewellAreaVector );
			}
			ZoneStruct.NumSurfaceFaces = NActFaces;
			SurfCount = double( NActFaces );
			CalcPolyhedronVolume( ZoneStruct, CalcVolume );

			if ( Zone( ZoneNum ).FloorArea > 0.0 ) {
				MinimumVolume = Zone( ZoneNum ).FloorArea * 2.5;
				if ( Zone( ZoneNum ).CeilingHeight > 0.0 ) {
					MinimumVolume = Zone( ZoneNum ).FloorArea * Zone( ZoneNum ).CeilingHeight;
				}
			} else {
				if ( SurfCount > 0 ) {
					MinimumVolume = pow_3( std::sqrt( SumAreas / SurfCount ) );
				} else {
					MinimumVolume = 0.0;
				}
			}
			if ( CalcVolume > 0.0 ) {
				TempVolume = CalcVolume;
			} else {
				TempVolume = MinimumVolume;
			}

			if ( Zone( ZoneNum ).Volume > 0.0 ) { // User entered zone volume, produce message if not near calculated
				if ( TempVolume > 0.0 ) {
					if ( std::abs( TempVolume - Zone( ZoneNum ).Volume ) / Zone( ZoneNum ).Volume > 0.05 ) {
						++ErrCount;
						if ( ErrCount == 1 && ! DisplayExtraWarnings ) {
							if ( initmsg ) {
								ShowMessage( "Note that the following warning(s) may/will occur if you have not enclosed your zone completely." );
								initmsg = false;
							}
							ShowWarningError( "Entered Zone Volumes differ from calculated zone volume(s)." );
							ShowContinueError( "...use Output:Diagnostics,DisplayExtraWarnings; to show more details on individual zones." );
						}
						if ( DisplayExtraWarnings ) {
							if ( initmsg ) {
								ShowMessage( "Note that the following warning(s) may/will occur if you have not enclosed your zone completely." );
								initmsg = false;
							}
							// Warn user of using specified Zone Volume
							ShowWarningError( "Entered Volume entered for Zone=\"" + Zone( ZoneNum ).Name + "\" significantly different from calculated Volume" );
							ShowContinueError( "Entered Zone Volume value=" + RoundSigDigits( Zone( ZoneNum ).Volume, 2 ) + ", Calculated Zone Volume value=" + RoundSigDigits( TempVolume, 2 ) + ", entered volume will be used in calculations." );
						}
					}
				}
			} else if ( CeilingHeightEntered( ZoneNum ) ) { // User did not enter zone volume, but entered ceiling height
				if ( Zone( ZoneNum ).FloorArea > 0.0 ) {
					Zone( ZoneNum ).Volume = Zone( ZoneNum ).FloorArea * Zone( ZoneNum ).CeilingHeight;
				} else { // ceiling height entered but floor area zero
					Zone( ZoneNum ).Volume = TempVolume;
				}
			} else { // Neither ceiling height nor volume entered
				Zone( ZoneNum ).Volume = TempVolume;
			}

			if ( Zone( ZoneNum ).Volume <= 0.0 ) {
				ShowSevereError( "Indicated Zone Volume <= 0.0 for Zone=" + Zone( ZoneNum ).Name );
				ShowContinueError( "Zone Volume calculated was=" + RoundSigDigits( Zone( ZoneNum ).Volume, 2 ) );
			}

			if ( ShowZoneSurfaces ) {
				if ( ShowZoneSurfaceHeaders ) {
					gio::write( OutputFileDebug, fmtLD ) << "===================================";
					gio::write( OutputFileDebug, fmtLD ) << "showing zone surfaces used and not used in volume calculation";
					gio::write( OutputFileDebug, fmtLD ) << "for volume calculation, only floors, walls and roofs/ceilings are used";
					gio::write( OutputFileDebug, fmtLD ) << "surface class, 1=wall, 2=floor, 3=roof/ceiling";
					gio::write( OutputFileDebug, fmtLD ) << "unused surface class(es), 5=internal mass, 11=window, 12=glass door";
					gio::write( OutputFileDebug, fmtLD ) << "                          13=door, 14=shading, 15=overhang, 16=fin";
					gio::write( OutputFileDebug, fmtLD ) << "                          17=TDD Dome, 18=TDD Diffuser";
					ShowZoneSurfaceHeaders = false;
				}
				gio::write( OutputFileDebug, fmtLD ) << "===================================";
				gio::write( OutputFileDebug, fmtLD ) << "zone=" << Zone( ZoneNum ).Name << " calc volume=" << CalcVolume;
				gio::write( OutputFileDebug, fmtLD ) << " nsurfaces=" << NFaces << " nactual=" << NActFaces;
			}
			for ( SurfNum = 1; SurfNum <= ZoneStruct.NumSurfaceFaces; ++SurfNum ) {
				if ( ShowZoneSurfaces ) {
					if ( SurfNum <= NActFaces ) {
						gio::write( OutputFileDebug, fmtLD ) << "surface=" << ZoneStruct.SurfaceFace( SurfNum ).SurfNum << " nsides=" << ZoneStruct.SurfaceFace( SurfNum ).NSides;
						gio::write( OutputFileDebug, fmtLD ) << "surface name=" << Surface( ZoneStruct.SurfaceFace( SurfNum ).SurfNum ).Name << " class=" << Surface( ZoneStruct.SurfaceFace( SurfNum ).SurfNum ).Class;
						gio::write( OutputFileDebug, fmtLD ) << "area=" << Surface( ZoneStruct.SurfaceFace( SurfNum ).SurfNum ).GrossArea;
						for ( iside = 1; iside <= ZoneStruct.SurfaceFace( SurfNum ).NSides; ++iside ) {
							auto const & FacePoint( ZoneStruct.SurfaceFace( SurfNum ).FacePoints( iside ) );
							gio::write( OutputFileDebug, fmtLD ) << FacePoint.x << FacePoint.y << FacePoint.z;
						}
					}
				}
				ZoneStruct.SurfaceFace( SurfNum ).FacePoints.deallocate();
			}
			if ( ShowZoneSurfaces ) {
				for ( SurfNum = 1; SurfNum <= notused; ++SurfNum ) {
					gio::write( OutputFileDebug, fmtLD ) << "notused:surface=" << surfacenotused( SurfNum ) << " name=" << Surface( surfacenotused( SurfNum ) ).Name << " class=" << Surface( surfacenotused( SurfNum ) ).Class;
				}
			}

			ZoneStruct.SurfaceFace.deallocate();
			surfacenotused.deallocate();

		}

		ErrorFlag = false;
		for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
			if ( Zone( ZoneNum ).Volume <= 0.0 ) ErrorFlag = true;
		}
		if ( ErrorFlag ) {
			ShowSevereError( "All ZONE Volumes must be > 0.0" );
			ErrorsFound = true;
		}

	}

	void
	ProcessSurfaceVertices(
		int const ThisSurf, // Surface Number
		bool & ErrorsFound
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Legacy Code (Walton)
		//       DATE WRITTEN   1976
		//       MODIFIED        FW, Mar 2002: Add triangular windows
		//                       FW, May 2002: modify test for 4-sided but non-rectangular subsurfaces
		//                       FW, Sep 2002: add shape for base surfaces (walls and detached shading surfaces)
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine processes each surface into the vertex representation used
		// by the shading procedures.
		// This routine depends on the surfaces coming in:
		//  Base Surface
		//   SubSurface (Window/Door)
		//   SubSurface
		//  Base Surface
		//   SubSurface
		//   SubSurface
		//  Thus, some attributes of the "Base Surface" must be SAVEd.

		// METHODOLOGY EMPLOYED:
		// Detached Shading, Base Surfaces, Attached Shading surfaces are represented in the
		// same manner as original.  Subsurfaces (windows, doors) are a "relative coordinate".

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::TrimSigDigits;
		using namespace Vectors;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const RoutineName( "ProcessSurfaceVertices: " );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//////////// hoisted into namespace
		//static bool OneTimeFlag( true ); // now ProcessSurfaceVerticesOneTimeFlag
		//static Array1D< Real64 > X; // now Xpsv (to avoid conflicts with CheckConvexity)
		//static Array1D< Real64 > Y; // now Ypsv
		//static Array1D< Real64 > Z; // now Zpsv
		////////////////////////////////////////////////

		// LOCAL VARIABLES
		//  REAL(r64) :: X00    ! Intermediate Result
		//  REAL(r64) :: Y00    ! Intermediate Result
		//  REAL(r64) :: Z00    ! Intermediate Result
		//  REAL(r64) :: A(3,3) ! Surface Rotation Matrix
		//  REAL(r64), SAVE :: B(3,3) ! Inverse Of Rotation Matrix
		Real64 X1; // Intermediate Result
		Real64 Y1; // Intermediate Result
		Real64 Z1; // Intermediate Result
		static Real64 XSHIFT; // Shift of X to Lower Left Corner
		static Real64 YSHIFT; // Shift of Y to Lower Left Corner
		Real64 XLLC; // X-coordinate of lower left corner
		Real64 YLLC; // Y-coordinate of lower left corner
		Real64 ZLLC; // Z-coordinate of lower left corner
		//  INTEGER :: I  ! Loop Control
		//  INTEGER :: J  ! Loop Control
		int n; // Vertex Number in Loop
		int ThisBaseSurface; // Current base surface
		Real64 Xp;
		Real64 Yp;
		Real64 Zp;
		static Real64 BaseCosAzimuth;
		static Real64 BaseCosTilt;
		static Real64 BaseSinAzimuth;
		static Real64 BaseSinTilt;
		static Real64 BaseXLLC;
		static Real64 BaseYLLC;
		static Real64 BaseZLLC;
		Real64 SurfWorldAz; // Surface Azimuth (facing)
		Real64 SurfTilt; // Surface Tilt
		//  TYPE(PlaneEq) PlanarEQ
		//  TYPE(Vector), dimension(3) :: TriVect
		//  REAL(r64) testval
		//  INTEGER ploop
		//  INTEGER vloop
		int ThisShape( 0 );
		bool BaseSurface; // True if a base surface or a detached shading surface
		Real64 ThisSurfAz;
		Real64 ThisSurfTilt;
		Real64 ThisReveal;
		Real64 ThisWidth;
		Real64 ThisHeight;
		int FrDivNum; // Frame/divider number
		Real64 FrWidth; // Frame width for exterior windows (m)
		Real64 FrArea; // Frame area for exterior windows(m2)
		Real64 DivWidth; // Divider width for exterior windows (m)
		Real64 DivArea; // Divider area for exterior windows (m2)
		Real64 DivFrac; // Fraction of divider area without overlaps
		bool ErrorInSurface; // false/true, depending on pass through routine
		bool SError;
		bool HeatTransSurf;
		bool IsCoPlanar;
		Real64 OutOfLine;
		int LastVertexInError;

		// Object Data
		PlaneEq BasePlane;
		Vector TVect;
		Vector CoordinateTransVector;

		ErrorInSurface = false;

		if ( ProcessSurfaceVerticesOneTimeFlag ) {
			Xpsv.allocate( MaxVerticesPerSurface );
			Ypsv.allocate( MaxVerticesPerSurface );
			Zpsv.allocate( MaxVerticesPerSurface );
			Xpsv = 0.0;
			Ypsv = 0.0;
			Zpsv = 0.0;
			ProcessSurfaceVerticesOneTimeFlag = false;
		}

		// Categorize this surface

		if ( Surface( ThisSurf ).BaseSurf == 0 || Surface( ThisSurf ).BaseSurf == ThisSurf ) {
			BaseSurface = true;
		} else {
			BaseSurface = false;
		}

		ThisBaseSurface = Surface( ThisSurf ).BaseSurf; // Dont know if this is still needed or not
		HeatTransSurf = Surface( ThisSurf ).HeatTransSurf;

		// Kludge for daylighting shelves
		if ( Surface( ThisSurf ).ShadowingSurf ) {
			ThisBaseSurface = ThisSurf;
			HeatTransSurf = true;
		}

		//IF (Surface(ThisSurf)%Name(1:3) /= 'Mir') THEN
		if ( ! Surface( ThisSurf ).MirroredSurf ) {
			CalcCoPlanarNess( Surface( ThisSurf ).Vertex, Surface( ThisSurf ).Sides, IsCoPlanar, OutOfLine, LastVertexInError );
			if ( ! IsCoPlanar ) {
				if ( OutOfLine > 0.01 ) {
					ShowSevereError( RoutineName + "Suspected non-planar surface:\"" + Surface( ThisSurf ).Name + "\", Max \"out of line\"=" + TrimSigDigits( OutOfLine, 5 ) + " at Vertex # " + TrimSigDigits( LastVertexInError ) );
				} else {
					ShowWarningError( RoutineName + "Possible non-planar surface:\"" + Surface( ThisSurf ).Name + "\", Max \"out of line\"=" + TrimSigDigits( OutOfLine, 5 ) + " at Vertex # " + TrimSigDigits( LastVertexInError ) );
				}
				//       ErrorInSurface=.TRUE.
			}
		}

		if ( BaseSurface ) {
			SurfWorldAz = Surface( ThisSurf ).Azimuth;
			SurfTilt = Surface( ThisSurf ).Tilt;
			BaseCosAzimuth = std::cos( SurfWorldAz * DegToRadians );
			BaseSinAzimuth = std::sin( SurfWorldAz * DegToRadians );
			BaseCosTilt = std::cos( SurfTilt * DegToRadians );
			BaseSinTilt = std::sin( SurfTilt * DegToRadians );
			for ( n = 1; n <= Surface( ThisSurf ).Sides; ++n ) {
				Xpsv( n ) = Surface( ThisSurf ).Vertex( n ).x;
				Ypsv( n ) = Surface( ThisSurf ).Vertex( n ).y;
				Zpsv( n ) = Surface( ThisSurf ).Vertex( n ).z;
			}
			BaseXLLC = Surface( ThisSurf ).Vertex( 2 ).x;
			BaseYLLC = Surface( ThisSurf ).Vertex( 2 ).y;
			BaseZLLC = Surface( ThisSurf ).Vertex( 2 ).z;
			TVect = Surface( ThisSurf ).Vertex( 3 ) - Surface( ThisSurf ).Vertex( 2 );
			ThisWidth = VecLength( TVect );
			TVect = Surface( ThisSurf ).Vertex( 2 ) - Surface( ThisSurf ).Vertex( 1 );
			ThisHeight = VecLength( TVect );
			Surface( ThisSurf ).Width = ThisWidth;
			Surface( ThisSurf ).Height = ThisHeight; // For a horizontal surface this is actually length!
			if ( Surface( ThisSurf ).Sides == 3 ) {
				Surface( ThisSurf ).Shape = Triangle;
			} else if ( Surface( ThisSurf ).Sides == 4 ) {
				// Test for rectangularity
				if ( isRectangle( ThisSurf ) ) {
					Surface( ThisSurf ).Shape = Rectangle;
				} else {
					Surface( ThisSurf ).Shape = Quadrilateral;
				}
			} else { // Surface( ThisSurf ).Sides > 4
				Surface( ThisSurf ).Shape = Polygonal;
				if ( std::abs( ThisHeight * ThisWidth - Surface( ThisSurf ).GrossArea ) > 0.001 ) {
					Surface( ThisSurf ).Width = std::sqrt( Surface( ThisSurf ).GrossArea );
					Surface( ThisSurf ).Height = Surface( ThisSurf ).Width;
					ThisWidth = Surface( ThisSurf ).Width;
					ThisHeight = Surface( ThisSurf ).Height;
				}
			}

		} else { // It's a subsurface to previous basesurface in this set of calls

			ThisSurfAz = Surface( ThisSurf ).Azimuth;
			ThisSurfTilt = Surface( ThisSurf ).Tilt;

			if ( HeatTransSurf ) {

				if ( Surface( ThisSurf ).Sides == 4 ) {
					ThisShape = RectangularDoorWindow;
				} else if ( Surface( ThisSurf ).Sides == 3 && Surface( ThisSurf ).Class == SurfaceClass_Window ) {
					ThisShape = TriangularWindow;
				} else if ( Surface( ThisSurf ).Sides == 3 && Surface( ThisSurf ).Class == SurfaceClass_Door ) {
					ThisShape = TriangularDoor;
				} else {
					assert( false );
				}

			} else { //  this is a shadowing subsurface

				if ( std::abs( Surface( Surface( ThisSurf ).BaseSurf ).Tilt - ThisSurfTilt ) <= 0.01 ) {
					// left or right fin
					if ( ThisSurfAz < 0.0 ) ThisSurfAz += 360.0;
					if ( ThisSurfAz > Surface( Surface( ThisSurf ).BaseSurf ).Azimuth ) {
						ThisShape = RectangularLeftFin;
					} else {
						ThisShape = RectangularRightFin;
					}
				} else {
					ThisShape = RectangularOverhang;
				}

			}

			// Setting relative coordinates for shadowing calculations for subsurfaces
			{ auto const SELECT_CASE_var( ThisShape );

			if ( SELECT_CASE_var == RectangularDoorWindow ) { // Rectangular heat transfer subsurface

				PlaneEquation( Surface( Surface( ThisSurf ).BaseSurf ).Vertex, Surface( Surface( ThisSurf ).BaseSurf ).Sides, BasePlane, SError );
				if ( SError ) {
					ShowSevereError( RoutineName + "Degenerate surface (likely two vertices equal):\"" + Surface( ThisSurf ).Name + "\"." );
					ErrorInSurface = true;
				}
				ThisReveal = -Pt2Plane( Surface( ThisSurf ).Vertex( 2 ), BasePlane );
				if ( std::abs( ThisReveal ) < 0.0002 ) ThisReveal = 0.0;
				Surface( ThisSurf ).Reveal = ThisReveal;
				Xp = Surface( ThisSurf ).Vertex( 2 ).x - BaseXLLC;
				Yp = Surface( ThisSurf ).Vertex( 2 ).y - BaseYLLC;
				Zp = Surface( ThisSurf ).Vertex( 2 ).z - BaseZLLC;
				XLLC = -Xp * BaseCosAzimuth + Yp * BaseSinAzimuth;
				YLLC = -Xp * BaseSinAzimuth * BaseCosTilt - Yp * BaseCosAzimuth * BaseCosTilt + Zp * BaseSinTilt;
				ZLLC = Xp * BaseSinAzimuth * BaseSinTilt + Yp * BaseCosAzimuth * BaseSinTilt + Zp * BaseCosTilt;
				TVect = Surface( ThisSurf ).Vertex( 3 ) - Surface( ThisSurf ).Vertex( 2 );
				ThisWidth = VecLength( TVect );
				TVect = Surface( ThisSurf ).Vertex( 2 ) - Surface( ThisSurf ).Vertex( 1 );
				ThisHeight = VecLength( TVect );
				Surface( ThisSurf ).Width = ThisWidth;
				Surface( ThisSurf ).Height = ThisHeight;

				// Test for rectangularity
				if ( ! isRectangle( ThisSurf ) ) {
					ShowSevereError( RoutineName + "Suspected 4-sided but non-rectangular Window, Door or GlassDoor:" );
					ShowContinueError( "Surface=" + Surface( ThisSurf ).Name );
					ErrorInSurface = true;
				}

				Xpsv( 1 ) = XLLC;
				Xpsv( 2 ) = XLLC;
				Xpsv( 3 ) = XLLC + Surface( ThisSurf ).Width;
				Xpsv( 4 ) = XLLC + Surface( ThisSurf ).Width;
				Ypsv( 1 ) = YLLC + Surface( ThisSurf ).Height;
				Ypsv( 4 ) = YLLC + Surface( ThisSurf ).Height;
				Ypsv( 2 ) = YLLC;
				Ypsv( 3 ) = YLLC;
				Zpsv( 1 ) = ZLLC;
				Zpsv( 2 ) = ZLLC;
				Zpsv( 3 ) = ZLLC;
				Zpsv( 4 ) = ZLLC;

				if ( Surface( ThisSurf ).Class == SurfaceClass_Window && Surface( ThisSurf ).ExtBoundCond == ExternalEnvironment && Surface( ThisSurf ).FrameDivider > 0 ) {
					FrDivNum = Surface( ThisSurf ).FrameDivider;
					// Set flag for calculating beam solar reflection from outside and/or inside window reveal
					if ( ( Surface( ThisSurf ).Reveal > 0.0 && FrameDivider( FrDivNum ).OutsideRevealSolAbs > 0.0 ) || ( FrameDivider( FrDivNum ).InsideSillDepth > 0.0 && FrameDivider( FrDivNum ).InsideSillSolAbs > 0.0 ) || ( FrameDivider( FrDivNum ).InsideReveal > 0.0 && FrameDivider( FrDivNum ).InsideRevealSolAbs > 0.0 ) ) CalcWindowRevealReflection = true;

					// For exterior window with frame, subtract frame area from base surface
					// (only rectangular windows are allowed to have a frame and/or divider;
					// Surface(ThisSurf)%FrameDivider will be 0 for triangular windows)
					FrWidth = FrameDivider( FrDivNum ).FrameWidth;
					if ( FrWidth > 0.0 ) {
						FrArea = ( Surface( ThisSurf ).Height + 2.0 * FrWidth ) * ( Surface( ThisSurf ).Width + 2.0 * FrWidth ) - Surface( ThisSurf ).Area / Surface( ThisSurf ).Multiplier;
						SurfaceWindow( ThisSurf ).FrameArea = FrArea * Surface( ThisSurf ).Multiplier;
						if ( ( Surface( Surface( ThisSurf ).BaseSurf ).Area - SurfaceWindow( ThisSurf ).FrameArea ) <= 0.0 ) {
							ShowSevereError( RoutineName + "Base Surface=\"" + Surface( Surface( ThisSurf ).BaseSurf ).Name + "\", " );
							ShowContinueError( "Window Surface=\"" + Surface( ThisSurf ).Name + "\" area (with frame) is too large to fit on the surface." );
							ShowContinueError( "Base surface area (-windows and doors)=[" + TrimSigDigits( Surface( Surface( ThisSurf ).BaseSurf ).Area, 2 ) + "] m2, frame area=[" + TrimSigDigits( SurfaceWindow( ThisSurf ).FrameArea, 2 ) + "] m2." );
							ErrorInSurface = true;
						}
						Surface( Surface( ThisSurf ).BaseSurf ).Area -= SurfaceWindow( ThisSurf ).FrameArea;
					}
					// If exterior window has divider, subtract divider area to get glazed area
					DivWidth = FrameDivider( Surface( ThisSurf ).FrameDivider ).DividerWidth;
					if ( DivWidth > 0.0 && ! ErrorInSurface ) {
						DivArea = DivWidth * ( FrameDivider( FrDivNum ).HorDividers * Surface( ThisSurf ).Width + FrameDivider( FrDivNum ).VertDividers * Surface( ThisSurf ).Height - FrameDivider( FrDivNum ).HorDividers * FrameDivider( FrDivNum ).VertDividers * DivWidth );
						SurfaceWindow( ThisSurf ).DividerArea = DivArea * Surface( ThisSurf ).Multiplier;
						if ( ( Surface( ThisSurf ).Area - SurfaceWindow( ThisSurf ).DividerArea ) <= 0.0 ) {
							ShowSevereError( RoutineName + "Divider area exceeds glazed opening for window " + Surface( ThisSurf ).Name );
							ShowContinueError( "Window surface area=[" + TrimSigDigits( Surface( ThisSurf ).Area, 2 ) + "] m2, divider area=[" + TrimSigDigits( SurfaceWindow( ThisSurf ).DividerArea, 2 ) + "] m2." );
							ErrorInSurface = true;
						}
						Surface( ThisSurf ).Area -= SurfaceWindow( ThisSurf ).DividerArea; // Glazed area
						if ( DivArea <= 0.0 ) {
							ShowWarningError( RoutineName + "Calculated Divider Area <= 0.0 for Window=" + Surface( ThisSurf ).Name );
							if ( FrameDivider( FrDivNum ).HorDividers == 0 ) {
								ShowContinueError( "..Number of Horizontal Dividers = 0." );
							}
							if ( FrameDivider( FrDivNum ).VertDividers == 0 ) {
								ShowContinueError( "..Number of Vertical Dividers = 0." );
							}
						} else {
							SurfaceWindow( ThisSurf ).GlazedFrac = Surface( ThisSurf ).Area / ( Surface( ThisSurf ).Area + SurfaceWindow( ThisSurf ).DividerArea );
							// Correction factor for portion of divider subject to divider projection correction
							DivFrac = ( 1.0 - FrameDivider( FrDivNum ).HorDividers * FrameDivider( FrDivNum ).VertDividers * pow_2( DivWidth ) / DivArea );
							SurfaceWindow( ThisSurf ).ProjCorrDivOut = DivFrac * FrameDivider( FrDivNum ).DividerProjectionOut / DivWidth;
							SurfaceWindow( ThisSurf ).ProjCorrDivIn = DivFrac * FrameDivider( FrDivNum ).DividerProjectionIn / DivWidth;
							// Correction factor for portion of frame subject to frame projection correction
							if ( FrWidth > 0.0 ) {
								SurfaceWindow( ThisSurf ).ProjCorrFrOut = ( FrameDivider( FrDivNum ).FrameProjectionOut / FrWidth ) * ( ThisHeight + ThisWidth - ( FrameDivider( FrDivNum ).HorDividers + FrameDivider( FrDivNum ).VertDividers ) * DivWidth ) / ( ThisHeight + ThisWidth + 2 * FrWidth );
								SurfaceWindow( ThisSurf ).ProjCorrFrIn = ( FrameDivider( FrDivNum ).FrameProjectionIn / FrWidth ) * ( ThisHeight + ThisWidth - ( FrameDivider( FrDivNum ).HorDividers + FrameDivider( FrDivNum ).VertDividers ) * DivWidth ) / ( ThisHeight + ThisWidth + 2 * FrWidth );

							}
						}
					}
				}

			} else if ( ( SELECT_CASE_var == TriangularWindow ) || ( SELECT_CASE_var == TriangularDoor ) ) {

				PlaneEquation( Surface( Surface( ThisSurf ).BaseSurf ).Vertex, Surface( Surface( ThisSurf ).BaseSurf ).Sides, BasePlane, SError );
				if ( SError ) {
					ShowSevereError( RoutineName + "Degenerate surface (likely two vertices equal):\"" + Surface( ThisSurf ).Name + "\"." );
					ErrorInSurface = true;
				}
				ThisReveal = -Pt2Plane( Surface( ThisSurf ).Vertex( 2 ), BasePlane );
				if ( std::abs( ThisReveal ) < 0.0002 ) ThisReveal = 0.0;
				Surface( ThisSurf ).Reveal = ThisReveal;
				Xp = Surface( ThisSurf ).Vertex( 2 ).x - BaseXLLC;
				Yp = Surface( ThisSurf ).Vertex( 2 ).y - BaseYLLC;
				Zp = Surface( ThisSurf ).Vertex( 2 ).z - BaseZLLC;
				Xpsv( 2 ) = -Xp * BaseCosAzimuth + Yp * BaseSinAzimuth;
				Ypsv( 2 ) = -Xp * BaseSinAzimuth * BaseCosTilt - Yp * BaseCosAzimuth * BaseCosTilt + Zp * BaseSinTilt;
				Zpsv( 2 ) = Xp * BaseSinAzimuth * BaseSinTilt + Yp * BaseCosAzimuth * BaseSinTilt + Zp * BaseCosTilt;
				TVect = Surface( ThisSurf ).Vertex( 3 ) - Surface( ThisSurf ).Vertex( 2 );
				ThisWidth = VecLength( TVect );
				TVect = Surface( ThisSurf ).Vertex( 2 ) - Surface( ThisSurf ).Vertex( 1 );
				ThisHeight = VecLength( TVect );
				Surface( ThisSurf ).Width = ThisWidth;
				Surface( ThisSurf ).Height = ThisHeight;
				// Effective height and width of a triangular window for use in calc of convective air flow
				// in gap between glass and shading device when shading device is present
				Surface( ThisSurf ).Height = 4.0 * Surface( ThisSurf ).Area / ( 3.0 * Surface( ThisSurf ).Width );
				Surface( ThisSurf ).Width *= 0.75;

				Xp = Surface( ThisSurf ).Vertex( 1 ).x - BaseXLLC;
				Yp = Surface( ThisSurf ).Vertex( 1 ).y - BaseYLLC;
				Zp = Surface( ThisSurf ).Vertex( 1 ).z - BaseZLLC;
				Xpsv( 1 ) = -Xp * BaseCosAzimuth + Yp * BaseSinAzimuth;
				Ypsv( 1 ) = -Xp * BaseSinAzimuth * BaseCosTilt - Yp * BaseCosAzimuth * BaseCosTilt + Zp * BaseSinTilt;
				Zpsv( 1 ) = Xp * BaseSinAzimuth * BaseSinTilt + Yp * BaseCosAzimuth * BaseSinTilt + Zp * BaseCosTilt;

				Xp = Surface( ThisSurf ).Vertex( 3 ).x - BaseXLLC;
				Yp = Surface( ThisSurf ).Vertex( 3 ).y - BaseYLLC;
				Zp = Surface( ThisSurf ).Vertex( 3 ).z - BaseZLLC;
				Xpsv( 3 ) = -Xp * BaseCosAzimuth + Yp * BaseSinAzimuth;
				Ypsv( 3 ) = -Xp * BaseSinAzimuth * BaseCosTilt - Yp * BaseCosAzimuth * BaseCosTilt + Zp * BaseSinTilt;
				Zpsv( 3 ) = Xp * BaseSinAzimuth * BaseSinTilt + Yp * BaseCosAzimuth * BaseSinTilt + Zp * BaseCosTilt;

			} else if ( SELECT_CASE_var == RectangularOverhang ) {

				Xp = Surface( ThisSurf ).Vertex( 2 ).x - BaseXLLC;
				Yp = Surface( ThisSurf ).Vertex( 2 ).y - BaseYLLC;
				Zp = Surface( ThisSurf ).Vertex( 2 ).z - BaseZLLC;
				XLLC = -Xp * BaseCosAzimuth + Yp * BaseSinAzimuth;
				YLLC = -Xp * BaseSinAzimuth * BaseCosTilt - Yp * BaseCosAzimuth * BaseCosTilt + Zp * BaseSinTilt;
				ZLLC = Xp * BaseSinAzimuth * BaseSinTilt + Yp * BaseCosAzimuth * BaseSinTilt + Zp * BaseCosTilt;
				TVect = Surface( ThisSurf ).Vertex( 3 ) - Surface( ThisSurf ).Vertex( 2 );
				ThisWidth = VecLength( TVect );
				TVect = Surface( ThisSurf ).Vertex( 2 ) - Surface( ThisSurf ).Vertex( 1 );
				ThisHeight = VecLength( TVect );
				Surface( ThisSurf ).Width = ThisWidth;
				Surface( ThisSurf ).Height = ThisHeight;
				Xpsv( 1 ) = XLLC;
				Xpsv( 2 ) = XLLC;
				Xpsv( 3 ) = XLLC + Surface( ThisSurf ).Width;
				Xpsv( 4 ) = XLLC + Surface( ThisSurf ).Width;
				Ypsv( 1 ) = YLLC;
				Ypsv( 2 ) = YLLC;
				Ypsv( 3 ) = YLLC;
				Ypsv( 4 ) = YLLC;
				Zpsv( 1 ) = Surface( ThisSurf ).Height;
				Zpsv( 4 ) = Surface( ThisSurf ).Height;
				Zpsv( 2 ) = 0.0;
				Zpsv( 3 ) = 0.0;

			} else if ( SELECT_CASE_var == RectangularLeftFin ) {

				Xp = Surface( ThisSurf ).Vertex( 2 ).x - BaseXLLC;
				Yp = Surface( ThisSurf ).Vertex( 2 ).y - BaseYLLC;
				Zp = Surface( ThisSurf ).Vertex( 2 ).z - BaseZLLC;
				XLLC = -Xp * BaseCosAzimuth + Yp * BaseSinAzimuth;
				YLLC = -Xp * BaseSinAzimuth * BaseCosTilt - Yp * BaseCosAzimuth * BaseCosTilt + Zp * BaseSinTilt;
				ZLLC = Xp * BaseSinAzimuth * BaseSinTilt + Yp * BaseCosAzimuth * BaseSinTilt + Zp * BaseCosTilt;
				TVect = Surface( ThisSurf ).Vertex( 3 ) - Surface( ThisSurf ).Vertex( 2 );
				ThisWidth = VecLength( TVect );
				TVect = Surface( ThisSurf ).Vertex( 2 ) - Surface( ThisSurf ).Vertex( 1 );
				ThisHeight = VecLength( TVect );
				Surface( ThisSurf ).Width = ThisWidth;
				Surface( ThisSurf ).Height = ThisHeight;
				Xpsv( 1 ) = XLLC;
				Xpsv( 2 ) = XLLC;
				Xpsv( 3 ) = XLLC;
				Xpsv( 4 ) = XLLC;
				Ypsv( 1 ) = YLLC;
				Ypsv( 2 ) = YLLC;
				Ypsv( 3 ) = YLLC + Surface( ThisSurf ).Width;
				Ypsv( 4 ) = YLLC + Surface( ThisSurf ).Width;
				Zpsv( 1 ) = Surface( ThisSurf ).Height;
				Zpsv( 4 ) = Surface( ThisSurf ).Height;
				Zpsv( 2 ) = 0.0;
				Zpsv( 3 ) = 0.0;

			} else if ( SELECT_CASE_var == RectangularRightFin ) {

				Xp = Surface( ThisSurf ).Vertex( 2 ).x - BaseXLLC;
				Yp = Surface( ThisSurf ).Vertex( 2 ).y - BaseYLLC;
				Zp = Surface( ThisSurf ).Vertex( 2 ).z - BaseZLLC;
				XLLC = -Xp * BaseCosAzimuth + Yp * BaseSinAzimuth;
				YLLC = -Xp * BaseSinAzimuth * BaseCosTilt - Yp * BaseCosAzimuth * BaseCosTilt + Zp * BaseSinTilt;
				ZLLC = Xp * BaseSinAzimuth * BaseSinTilt + Yp * BaseCosAzimuth * BaseSinTilt + Zp * BaseCosTilt;
				TVect = Surface( ThisSurf ).Vertex( 3 ) - Surface( ThisSurf ).Vertex( 2 );
				ThisWidth = VecLength( TVect );
				TVect = Surface( ThisSurf ).Vertex( 2 ) - Surface( ThisSurf ).Vertex( 1 );
				ThisHeight = VecLength( TVect );
				Surface( ThisSurf ).Width = ThisWidth;
				Surface( ThisSurf ).Height = ThisHeight;
				Xpsv( 1 ) = XLLC;
				Xpsv( 2 ) = XLLC;
				Xpsv( 3 ) = XLLC;
				Xpsv( 4 ) = XLLC;
				Ypsv( 1 ) = YLLC + Surface( ThisSurf ).Width;
				Ypsv( 2 ) = YLLC + Surface( ThisSurf ).Width;
				Ypsv( 3 ) = YLLC;
				Ypsv( 4 ) = YLLC;
				Zpsv( 1 ) = Surface( ThisSurf ).Height;
				Zpsv( 4 ) = Surface( ThisSurf ).Height;
				Zpsv( 2 ) = 0.0;
				Zpsv( 3 ) = 0.0;

			} else {
				// Error Condition
				ShowSevereError( RoutineName + "Incorrect surface shape number.", OutputFileStandard );
				ShowContinueError( "Please notify EnergyPlus support of this error and send input file." );
				ErrorInSurface = true;

			}}

			for ( n = 1; n <= Surface( ThisSurf ).Sides; ++n ) {
				// if less than 1/10 inch
				Xpsv( n ) = nint64( 10000.0 * Xpsv( n ) ) / 10000.0;
				if ( std::abs( Xpsv( n ) ) < 0.0025 ) Xpsv( n ) = 0.0;
				Ypsv( n ) = nint64( 10000.0 * Ypsv( n ) ) / 10000.0;
				if ( std::abs( Ypsv( n ) ) < 0.0025 ) Ypsv( n ) = 0.0;
				Zpsv( n ) = nint64( 10000.0 * Zpsv( n ) ) / 10000.0;
				if ( std::abs( Zpsv( n ) ) < 0.0025 ) Zpsv( n ) = 0.0;
			}

			Surface( ThisSurf ).Shape = ThisShape;

		} // End of check if ThisSurf is a base surface

		if ( ErrorInSurface ) {
			ErrorsFound = true;
			return;
		}

		// Transfer to XV,YV,ZV arrays

		ShadeV( ThisSurf ).NVert = Surface( ThisSurf ).Sides;
		ShadeV( ThisSurf ).XV.allocate( Surface( ThisSurf ).Sides );
		ShadeV( ThisSurf ).YV.allocate( Surface( ThisSurf ).Sides );
		ShadeV( ThisSurf ).ZV.allocate( Surface( ThisSurf ).Sides );

		for ( n = 1; n <= Surface( ThisSurf ).Sides; ++n ) {
			// if less than 1/10 inch
			ShadeV( ThisSurf ).XV( n ) = Xpsv( n );
			ShadeV( ThisSurf ).YV( n ) = Ypsv( n );
			ShadeV( ThisSurf ).ZV( n ) = Zpsv( n );
		}

		// Process Surfaces According to Type of Coordinate Origin.
		if ( BaseSurface ) {

			// General Surfaces:
			CalcCoordinateTransformation( ThisSurf, CoordinateTransVector ); //X00,Y00,Z00,X,Y,Z,A)    ! Compute Coordinate Transformation

			// RECORD DIRECTION COSINES.
			if ( HeatTransSurf ) { // This is a general surface but not detached shading surface

				// RECORD COORDINATE TRANSFORMATION FOR BASE SURFACES.
				X0( ThisBaseSurface ) = CoordinateTransVector.x;
				Y0( ThisBaseSurface ) = CoordinateTransVector.y;
				Z0( ThisBaseSurface ) = CoordinateTransVector.z;

				// COMPUTE INVERSE TRANSFORMATION.
				X1 = Xpsv( 2 ) - CoordinateTransVector.x;
				Y1 = Ypsv( 2 ) - CoordinateTransVector.y;
				Z1 = Zpsv( 2 ) - CoordinateTransVector.z;
				XSHIFT = Surface( ThisBaseSurface ).lcsx.x * X1 + Surface( ThisBaseSurface ).lcsx.y * Y1 + Surface( ThisBaseSurface ).lcsx.z * Z1;
				YSHIFT = Surface( ThisBaseSurface ).lcsy.x * X1 + Surface( ThisBaseSurface ).lcsy.y * Y1 + Surface( ThisBaseSurface ).lcsy.z * Z1;

			}

			// SUBSURFACES: (Surface(ThisSurf)%BaseSurf /= ThisSurf)
		} else {
			// WINDOWS OR DOORS:

			// SHIFT RELATIVE COORDINATES FROM LOWER LEFT CORNER TO ORIGIN DEFINED
			// BY CTRAN AND SET DIRECTION COSINES SAME AS BASE SURFACE.

			for ( n = 1; n <= Surface( ThisSurf ).Sides; ++n ) {
				ShadeV( ThisSurf ).XV( n ) += XSHIFT;
				ShadeV( ThisSurf ).YV( n ) += YSHIFT;
			}

		}

		if ( ErrorInSurface ) {
			ErrorsFound = true;
		}

	}

	void
	CalcCoordinateTransformation(
		int const SurfNum, // Surface Number
		Vector & CompCoordTranslVector // Coordinate Translation Vector
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         George Walton, BLAST
		//       DATE WRITTEN   August 1976
		//       MODIFIED       LKL, May 2004 -- >4 sided polygons
		//       RE-ENGINEERED  Yes

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine develops a coordinate transformation such that the X-axis goes
		// through points 2 and 3 and the Y-axis goes through point 1
		// of a plane figure in 3-d space.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// 'NECAP' - NASA'S Energy-Cost Analysis Program

		// Using/Aliasing
		using namespace Vectors;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static gio::Fmt ErrFmt( "(' (',F8.3,',',F8.3,',',F8.3,')')" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int I; // Loop Control
		Real64 Gamma; // Intermediate Result
		Real64 DotSelfX23;
		static std::string ErrLineOut; // Character string for producing error messages

		// Object Data
		Vector x21;
		Vector x23;

		// Determine Components of the Coordinate Translation Vector.

		x21 = Surface( SurfNum ).Vertex( 2 ) - Surface( SurfNum ).Vertex( 1 );
		x23 = Surface( SurfNum ).Vertex( 2 ) - Surface( SurfNum ).Vertex( 3 );

		DotSelfX23 = magnitude_squared( x23 );

		if ( std::abs( DotSelfX23 ) <= .1e-6 ) {
			ShowSevereError( "CalcCoordinateTransformation: Invalid dot product, surface=\"" + Surface( SurfNum ).Name + "\":" );
			for ( I = 1; I <= Surface( SurfNum ).Sides; ++I ) {
				auto const & point( Surface( SurfNum ).Vertex( I ) );
				gio::write( ErrLineOut, ErrFmt ) << point.x << point.y << point.z;
				ShowContinueError( ErrLineOut );
			}
			ShowFatalError( "CalcCoordinateTransformation: Program terminates due to preceding condition.", OutputFileStandard );
			return;
		}

		Gamma = dot( x21, x23 ) / magnitude_squared( x23 );

		CompCoordTranslVector = Surface( SurfNum ).Vertex( 2 ) + Gamma * ( Surface( SurfNum ).Vertex( 3 ) - Surface( SurfNum ).Vertex( 2 ) );

	}

	void
	CreateShadedWindowConstruction(
		int const SurfNum, // Surface number
		int const WSCPtr, // Pointer to WindowShadingControl for SurfNum
		int const ShDevNum // Shading device material number for WSCptr
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   Nov 2001
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Creates a shaded window construction for windows whose WindowShadingControl
		// has a shading device specified instead of a shaded construction

		// METHODOLOGY EMPLOYED:na

		// REFERENCES:na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:na
		// INTERFACE BLOCK SPECIFICATIONS;na
		// DERIVED TYPE DEFINITIONS:na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ConstrNum; // Number of unshaded construction
		int ConstrNewSh; // Number of shaded construction that is created
		std::string ShDevName; // Shading device material name
		std::string ConstrName; // Unshaded construction name
		std::string ConstrNameSh; // Shaded construction name
		int TotLayersOld; // Total layers in old (unshaded) construction
		int TotLayersNew; // Total layers in new (shaded) construction
		//  INTEGER :: loop                            ! DO loop index

		ShDevName = Material( ShDevNum ).Name;
		ConstrNum = SurfaceTmp( SurfNum ).Construction;
		ConstrName = Construct( ConstrNum ).Name;
		if ( WindowShadingControl( WSCPtr ).ShadingType == WSC_ST_InteriorShade || WindowShadingControl( WSCPtr ).ShadingType == WSC_ST_InteriorBlind ) {
			ConstrNameSh = ConstrName + ':' + ShDevName + ":INT";
		} else {
			ConstrNameSh = ConstrName + ':' + ShDevName + ":EXT";
		}

		// If this construction name already exists, set the surface's shaded construction number to it

		ConstrNewSh = FindItemInList( ConstrNameSh, Construct );

		if ( ConstrNewSh > 0 ) {
			SurfaceTmp( SurfNum ).ShadedConstruction = ConstrNewSh;
		} else {

			// Create new construction

			ConstrNewSh = TotConstructs + 1;
			SurfaceTmp( SurfNum ).ShadedConstruction = ConstrNewSh;
			TotConstructs = ConstrNewSh;
			Construct.redimension( TotConstructs );
			NominalRforNominalUCalculation.redimension( TotConstructs );
			NominalRforNominalUCalculation( TotConstructs ) = 0.0;
			NominalU.redimension( TotConstructs );
			NominalU( TotConstructs ) = 0.0;

			TotLayersOld = Construct( ConstrNum ).TotLayers;
			TotLayersNew = TotLayersOld + 1;

			Construct( ConstrNewSh ).LayerPoint = 0;

			if ( WindowShadingControl( WSCPtr ).ShadingType == WSC_ST_InteriorShade || WindowShadingControl( WSCPtr ).ShadingType == WSC_ST_InteriorBlind ) {
				// Interior shading device
				Construct( ConstrNewSh ).LayerPoint( {1,TotLayersOld} ) = Construct( ConstrNum ).LayerPoint( {1,TotLayersOld} );
				Construct( ConstrNewSh ).LayerPoint( TotLayersNew ) = ShDevNum;
				Construct( ConstrNewSh ).InsideAbsorpSolar = Material( ShDevNum ).AbsorpSolar;
				Construct( ConstrNewSh ).OutsideAbsorpSolar = Material( Construct( ConstrNewSh ).LayerPoint( 1 ) ).AbsorpSolar;
				Construct( ConstrNewSh ).OutsideAbsorpThermal = Material( Construct( ConstrNewSh ).LayerPoint( 1 ) ).AbsorpThermalFront;
			} else {
				// Exterior shading device
				Construct( ConstrNewSh ).LayerPoint( 1 ) = ShDevNum;
				Construct( ConstrNewSh ).LayerPoint( {2,TotLayersNew} ) = Construct( ConstrNum ).LayerPoint( {1,TotLayersOld} );
				Construct( ConstrNewSh ).InsideAbsorpSolar = Material( Construct( ConstrNewSh ).LayerPoint( TotLayersNew ) ).AbsorpSolar;
				Construct( ConstrNewSh ).OutsideAbsorpSolar = Material( ShDevNum ).AbsorpSolar;
				Construct( ConstrNewSh ).OutsideAbsorpThermal = Material( ShDevNum ).AbsorpThermalFront;
			}
			// The following InsideAbsorpThermal applies only to inside glass; it is corrected
			//  later in InitGlassOpticalCalculations if construction has inside shade or blind.
			Construct( ConstrNewSh ).InsideAbsorpThermal = Material( Construct( ConstrNum ).LayerPoint( TotLayersOld ) ).AbsorpThermalBack;
			Construct( ConstrNewSh ).OutsideRoughness = VerySmooth;
			Construct( ConstrNewSh ).DayltPropPtr = 0;
			Construct( ConstrNewSh ).CTFCross = 0.0;
			Construct( ConstrNewSh ).CTFFlux = 0.0;
			Construct( ConstrNewSh ).CTFInside = 0.0;
			Construct( ConstrNewSh ).CTFOutside = 0.0;
			Construct( ConstrNewSh ).CTFSourceIn = 0.0;
			Construct( ConstrNewSh ).CTFSourceOut = 0.0;
			Construct( ConstrNewSh ).CTFTimeStep = 0.0;
			Construct( ConstrNewSh ).CTFTSourceOut = 0.0;
			Construct( ConstrNewSh ).CTFTSourceIn = 0.0;
			Construct( ConstrNewSh ).CTFTSourceQ = 0.0;
			Construct( ConstrNewSh ).CTFTUserOut = 0.0;
			Construct( ConstrNewSh ).CTFTUserIn = 0.0;
			Construct( ConstrNewSh ).CTFTUserSource = 0.0;
			Construct( ConstrNewSh ).NumHistories = 0;
			Construct( ConstrNewSh ).NumCTFTerms = 0;
			Construct( ConstrNewSh ).UValue = 0.0;
			Construct( ConstrNewSh ).SourceSinkPresent = false;
			Construct( ConstrNewSh ).SolutionDimensions = 0;
			Construct( ConstrNewSh ).SourceAfterLayer = 0;
			Construct( ConstrNewSh ).TempAfterLayer = 0;
			Construct( ConstrNewSh ).ThicknessPerpend = 0.0;
			Construct( ConstrNewSh ).AbsDiff = 0.0;
			Construct( ConstrNewSh ).AbsDiffBack = 0.0;
			Construct( ConstrNewSh ).AbsDiffShade = 0.0;
			Construct( ConstrNewSh ).AbsDiffBackShade = 0.0;
			Construct( ConstrNewSh ).ShadeAbsorpThermal = 0.0;
			Construct( ConstrNewSh ).AbsBeamCoef = 0.0;
			Construct( ConstrNewSh ).AbsBeamBackCoef = 0.0;
			Construct( ConstrNewSh ).AbsBeamShadeCoef = 0.0;
			Construct( ConstrNewSh ).TransDiff = 0.0;
			Construct( ConstrNewSh ).TransDiffVis = 0.0;
			Construct( ConstrNewSh ).ReflectSolDiffBack = 0.0;
			Construct( ConstrNewSh ).ReflectSolDiffFront = 0.0;
			Construct( ConstrNewSh ).ReflectVisDiffBack = 0.0;
			Construct( ConstrNewSh ).ReflectVisDiffFront = 0.0;
			Construct( ConstrNewSh ).TransSolBeamCoef = 0.0;
			Construct( ConstrNewSh ).TransVisBeamCoef = 0.0;
			Construct( ConstrNewSh ).ReflSolBeamFrontCoef = 0.0;
			Construct( ConstrNewSh ).ReflSolBeamBackCoef = 0.0;
			Construct( ConstrNewSh ).W5FrameDivider = 0;
			Construct( ConstrNewSh ).FromWindow5DataFile = false;

			Construct( ConstrNewSh ).Name = ConstrNameSh;
			Construct( ConstrNewSh ).TotLayers = TotLayersNew;
			Construct( ConstrNewSh ).TotSolidLayers = Construct( ConstrNum ).TotSolidLayers + 1;
			Construct( ConstrNewSh ).TotGlassLayers = Construct( ConstrNum ).TotGlassLayers;
			Construct( ConstrNewSh ).TypeIsWindow = true;
			Construct( ConstrNewSh ).IsUsed = true;

		}

	}

	void
	CreateStormWindowConstructions()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   Jan 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// For windows with an associated StormWindow object, creates a construction
		// consisting of the base construction plus a storm window and air gap on the outside.
		// If the window has an interior or between-glass shade/blind, also creates a
		// construction consisting of the storm window added to the shaded construction.

		// METHODOLOGY EMPLOYED:na
		// REFERENCES:na

		// Using/Aliasing
		using InputProcessor::FindItemInList;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// SUBROUTINE PARAMETER DEFINITIONS:na
		// INTERFACE BLOCK SPECIFICATIONS;na
		// DERIVED TYPE DEFINITIONS:na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int SurfNum; // Surface number
		int StormWinNum; // Number of StormWindow object
		int ConstrNum; // Number of unshaded construction
		int ConstrNumSh; // Number of shaded construction
		int ConstrOld; // Number of old construction (unshaded or shaded)
		int ConstrNewSt; // Number of unshaded storm window construction that is created
		int ConstrNewStSh; // Number of shaded storm window construction that is created
		int ConstrNew; // Number of new construction with storm window (unshaded or shaded)
		int MatNewStAir; // Number of created air layer material
		std::string ConstrName; // Name of original unshaded window construction
		std::string ConstrNameSh; // Name of original shaded window construction
		std::string ConstrNameSt; // Name of unshaded construction with storm window
		std::string ConstrNameStSh; // Name of shaded construction with storm window
		std::string MatNameStAir; // Name of created air layer material
		int StormWinMatNum; // Material number of storm window glass layer
		int IntDistance; // Thickness of air gap between storm window and rest of window (mm)
		std::string ChrIntDistance; // Character representation of IntDistance
		std::string ChrNum; // Character representation of storm window number
		int TotLayers; // Total layers in a construction
		int TotGlassLayers; // Total glass layers in a construction
		int TotLayersOld; // Total layers in old (without storm window) construction
		int MatIntSh; // Material number of interior shade or blind
		int MatBGsh; // Material number of between-glass shade or blind
		int loop; // DO loop index
		bool ShAndSt; // True if unshaded and shaded window can have a storm window
		//  INTEGER :: LenName               ! Name length

		DisplayString( "Creating Storm Window Constructions" );

		for ( StormWinNum = 1; StormWinNum <= TotStormWin; ++StormWinNum ) {
			SurfNum = StormWindow( StormWinNum ).BaseWindowNum;
			ConstrNum = Surface( SurfNum ).Construction;
			// Fatal error if base construction has more than three glass layers
			if ( Construct( ConstrNum ).TotGlassLayers > 3 ) {
				ShowFatalError( "Window=" + Surface( SurfNum ).Name + " has more than 3 glass layers; a storm window cannot be applied." );
			}
			ConstrNumSh = Surface( SurfNum ).ShadedConstruction;
			ConstrName = Construct( ConstrNum ).Name;
			StormWinMatNum = StormWindow( StormWinNum ).StormWinMaterialNum;
			IntDistance = int( 1000 * StormWindow( StormWinNum ).StormWinDistance );
			gio::write( ChrIntDistance, fmtLD ) << IntDistance;
			strip( ChrIntDistance );
			// Set ShAndSt, which is true if the window has a shaded construction to which a storm window
			// can be added. (A storm window can be added if there is an interior shade or blind and up to three
			// glass layers, or there is a between-glass shade or blind and two glass layers.)
			ShAndSt = false;
			if ( ConstrNumSh > 0 ) {
				ConstrNameSh = Construct( ConstrNumSh ).Name;
				TotLayers = Construct( ConstrNumSh ).TotLayers;
				TotGlassLayers = Construct( ConstrNumSh ).TotGlassLayers;
				MatIntSh = Construct( ConstrNumSh ).LayerPoint( TotLayers );
				MatBGsh = 0;
				if ( TotLayers == 5 ) MatBGsh = Construct( ConstrNumSh ).LayerPoint( 3 );
				if ( TotGlassLayers <= 3 && ( Material( MatIntSh ).Group == Shade || Material( MatIntSh ).Group == WindowBlind ) ) ShAndSt = true;
				if ( MatBGsh > 0 ) {
					if ( Material( MatBGsh ).Group == Shade || Material( MatBGsh ).Group == WindowBlind ) ShAndSt = true;
				}
				if ( ! ShAndSt ) {
					ShowContinueError( "Window=" + Surface( SurfNum ).Name + " has a shaded construction to which a storm window cannot be applied." );
					ShowContinueError( "Storm windows can only be applied to shaded constructions that:" );
					ShowContinueError( "have an interior shade or blind and up to three glass layers, or" );
					ShowContinueError( "have a between-glass shade or blind and two glass layers." );
					ShowFatalError( "EnergyPlus is exiting due to reason stated above." );
				}
			}

			// Loop over unshaded (loop=1) and shaded (loop=2) constructions and create new constructions
			// with storm window and air gap added on outside
			for ( loop = 1; loop <= 2; ++loop ) {
				if ( loop == 1 ) {
					gio::write( ChrNum, fmtLD ) << StormWinNum;
					strip( ChrNum );
					ConstrNameSt = "BARECONSTRUCTIONWITHSTORMWIN:" + ChrNum;
					// If this construction name already exists, set the surface's storm window construction number to it
					ConstrNewSt = FindItemInList( ConstrNameSt, Construct, TotConstructs );
					ConstrNewStSh = 0;
					if ( ConstrNewSt > 0 ) Surface( SurfNum ).StormWinConstruction = ConstrNewSt;
				} else {
					if ( ! ShAndSt ) break;
					ConstrNameStSh = "SHADEDCONSTRUCTIONWITHSTORMWIN:" + ChrNum;
					ConstrNewStSh = FindItemInList( ConstrNameStSh, Construct, TotConstructs );
					if ( ConstrNewStSh > 0 ) Surface( SurfNum ).StormWinShadedConstruction = ConstrNewStSh;
				}

				if ( loop == 1 && ConstrNewSt == 0 ) {
					// If necessary, create new material corresponding to the air layer between the storm winddow
					// and the rest of the window
					MatNameStAir = "AIR:STORMWIN:" + ChrIntDistance + "MM";
					MatNewStAir = FindItemInList( MatNameStAir, Material, TotMaterials );
					if ( MatNewStAir == 0 ) {
						// Create new material
						MatNewStAir = TotMaterials + 1;
						TotMaterials = MatNewStAir;
						Material.redimension( TotMaterials );
						NominalR.redimension( TotMaterials );
						Material( TotMaterials ).Name = MatNameStAir;
						Material( TotMaterials ).Group = WindowGas;
						Material( TotMaterials ).Roughness = 3;
						Material( TotMaterials ).Conductivity = 0.0;
						Material( TotMaterials ).Density = 0.0;
						Material( TotMaterials ).IsoMoistCap = 0.0;
						Material( TotMaterials ).Porosity = 0.0;
						Material( TotMaterials ).Resistance = 0.0;
						Material( TotMaterials ).SpecHeat = 0.0;
						Material( TotMaterials ).ThermGradCoef = 0.0;
						Material( TotMaterials ).Thickness = StormWindow( StormWinNum ).StormWinDistance;
						Material( TotMaterials ).VaporDiffus = 0.0;
						Material( TotMaterials ).GasType = 0;
						Material( TotMaterials ).GasCon = 0.0;
						Material( TotMaterials ).GasVis = 0.0;
						Material( TotMaterials ).GasCp = 0.0;
						Material( TotMaterials ).GasWght = 0.0;
						Material( TotMaterials ).GasFract = 0.0;
						Material( TotMaterials ).GasType( 1 ) = 1;
						Material( TotMaterials ).GlassSpectralDataPtr = 0;
						Material( TotMaterials ).NumberOfGasesInMixture = 1;
						Material( TotMaterials ).GasCon( 1, 1 ) = 2.873e-3;
						Material( TotMaterials ).GasCon( 2, 1 ) = 7.760e-5;
						Material( TotMaterials ).GasVis( 1, 1 ) = 3.723e-6;
						Material( TotMaterials ).GasVis( 2, 1 ) = 4.940e-8;
						Material( TotMaterials ).GasCp( 1, 1 ) = 1002.737;
						Material( TotMaterials ).GasCp( 2, 1 ) = 1.2324e-2;
						Material( TotMaterials ).GasWght( 1 ) = 28.97;
						Material( TotMaterials ).GasFract( 1 ) = 1.0;
						Material( TotMaterials ).AbsorpSolar = 0.0;
						Material( TotMaterials ).AbsorpThermal = 0.0;
						Material( TotMaterials ).AbsorpVisible = 0.0;
						Material( TotMaterials ).Trans = 0.0;
						Material( TotMaterials ).TransVis = 0.0;
						Material( TotMaterials ).GlassTransDirtFactor = 0.0;
						Material( TotMaterials ).ReflectShade = 0.0;
						Material( TotMaterials ).ReflectShadeVis = 0.0;
						Material( TotMaterials ).AbsorpThermalBack = 0.0;
						Material( TotMaterials ).AbsorpThermalFront = 0.0;
						Material( TotMaterials ).ReflectSolBeamBack = 0.0;
						Material( TotMaterials ).ReflectSolBeamFront = 0.0;
						Material( TotMaterials ).ReflectSolDiffBack = 0.0;
						Material( TotMaterials ).ReflectSolDiffFront = 0.0;
						Material( TotMaterials ).ReflectVisBeamBack = 0.0;
						Material( TotMaterials ).ReflectVisBeamFront = 0.0;
						Material( TotMaterials ).ReflectVisDiffBack = 0.0;
						Material( TotMaterials ).ReflectVisDiffFront = 0.0;
						Material( TotMaterials ).TransSolBeam = 0.0;
						Material( TotMaterials ).TransThermal = 0.0;
						Material( TotMaterials ).TransVisBeam = 0.0;
						Material( TotMaterials ).BlindDataPtr = 0;
						Material( TotMaterials ).WinShadeToGlassDist = 0.0;
						Material( TotMaterials ).WinShadeTopOpeningMult = 0.0;
						Material( TotMaterials ).WinShadeBottomOpeningMult = 0.0;
						Material( TotMaterials ).WinShadeLeftOpeningMult = 0.0;
						Material( TotMaterials ).WinShadeRightOpeningMult = 0.0;
						Material( TotMaterials ).WinShadeAirFlowPermeability = 0.0;
						Material( TotMaterials ).EMPDVALUE = 0.0;
						Material( TotMaterials ).MoistACoeff = 0.0;
						Material( TotMaterials ).MoistBCoeff = 0.0;
						Material( TotMaterials ).MoistCCoeff = 0.0;
						Material( TotMaterials ).MoistDCoeff = 0.0;
						Material( TotMaterials ).EMPDaCoeff = 0.0;
						Material( TotMaterials ).EMPDbCoeff = 0.0;
						Material( TotMaterials ).EMPDcCoeff = 0.0;
						Material( TotMaterials ).EMPDdCoeff = 0.0;
					} // End of check if new air layer material has to be created
				}

				if ( ( loop == 1 && ConstrNewSt == 0 ) || ( loop == 2 && ConstrNewStSh == 0 ) ) {
					// Create new constructions
					ConstrNew = TotConstructs + 1;
					if ( loop == 1 ) {
						Surface( SurfNum ).StormWinConstruction = ConstrNew;
					} else {
						Surface( SurfNum ).StormWinShadedConstruction = ConstrNew;
					}
					TotConstructs = ConstrNew;
					Construct.redimension( TotConstructs );
					NominalRforNominalUCalculation.redimension( TotConstructs );
					NominalU.redimension( TotConstructs );

					ConstrOld = ConstrNum;
					if ( loop == 2 ) ConstrOld = ConstrNumSh;
					TotLayersOld = Construct( ConstrOld ).TotLayers;
					Construct( ConstrNew ).LayerPoint( {1,MaxLayersInConstruct} ) = 0;
					Construct( ConstrNew ).LayerPoint( 1 ) = StormWinMatNum;
					Construct( ConstrNew ).LayerPoint( 2 ) = MatNewStAir;
					Construct( ConstrNew ).LayerPoint( {3,TotLayersOld + 2} ) = Construct( ConstrOld ).LayerPoint( {1,TotLayersOld} );
					Construct( ConstrNew ).Name = ConstrNameSt;
					if ( loop == 2 ) Construct( ConstrNew ).Name = ConstrNameStSh;
					Construct( ConstrNew ).TotLayers = TotLayersOld + 2;
					Construct( ConstrNew ).TotSolidLayers = Construct( ConstrOld ).TotSolidLayers + 1;
					Construct( ConstrNew ).TotGlassLayers = Construct( ConstrOld ).TotGlassLayers + 1;
					Construct( ConstrNew ).TypeIsWindow = true;
					Construct( ConstrNew ).InsideAbsorpVis = 0.0;
					Construct( ConstrNew ).OutsideAbsorpVis = 0.0;
					Construct( ConstrNew ).InsideAbsorpSolar = 0.0;
					Construct( ConstrNew ).OutsideAbsorpSolar = 0.0;
					Construct( ConstrNew ).InsideAbsorpThermal = Construct( ConstrOld ).InsideAbsorpThermal;
					Construct( ConstrNew ).OutsideAbsorpThermal = Material( StormWinMatNum ).AbsorpThermalFront;
					Construct( ConstrNew ).OutsideRoughness = VerySmooth;
					Construct( ConstrNew ).DayltPropPtr = 0;
					Construct( ConstrNew ).CTFCross = 0.0;
					Construct( ConstrNew ).CTFFlux = 0.0;
					Construct( ConstrNew ).CTFInside = 0.0;
					Construct( ConstrNew ).CTFOutside = 0.0;
					Construct( ConstrNew ).CTFSourceIn = 0.0;
					Construct( ConstrNew ).CTFSourceOut = 0.0;
					Construct( ConstrNew ).CTFTimeStep = 0.0;
					Construct( ConstrNew ).CTFTSourceOut = 0.0;
					Construct( ConstrNew ).CTFTSourceIn = 0.0;
					Construct( ConstrNew ).CTFTSourceQ = 0.0;
					Construct( ConstrNew ).CTFTUserOut = 0.0;
					Construct( ConstrNew ).CTFTUserIn = 0.0;
					Construct( ConstrNew ).CTFTUserSource = 0.0;
					Construct( ConstrNew ).NumHistories = 0;
					Construct( ConstrNew ).NumCTFTerms = 0;
					Construct( ConstrNew ).UValue = 0.0;
					Construct( ConstrNew ).SourceSinkPresent = false;
					Construct( ConstrNew ).SolutionDimensions = 0;
					Construct( ConstrNew ).SourceAfterLayer = 0;
					Construct( ConstrNew ).TempAfterLayer = 0;
					Construct( ConstrNew ).ThicknessPerpend = 0.0;
					Construct( ConstrNew ).AbsDiffIn = 0.0;
					Construct( ConstrNew ).AbsDiffOut = 0.0;
					Construct( ConstrNew ).AbsDiff = 0.0;
					Construct( ConstrNew ).AbsDiffBack = 0.0;
					Construct( ConstrNew ).AbsDiffShade = 0.0;
					Construct( ConstrNew ).AbsDiffBackShade = 0.0;
					Construct( ConstrNew ).ShadeAbsorpThermal = 0.0;
					Construct( ConstrNew ).AbsBeamCoef = 0.0;
					Construct( ConstrNew ).AbsBeamBackCoef = 0.0;
					Construct( ConstrNew ).AbsBeamShadeCoef = 0.0;
					Construct( ConstrNew ).TransDiff = 0.0;
					Construct( ConstrNew ).TransDiffVis = 0.0;
					Construct( ConstrNew ).ReflectSolDiffBack = 0.0;
					Construct( ConstrNew ).ReflectSolDiffFront = 0.0;
					Construct( ConstrNew ).ReflectVisDiffBack = 0.0;
					Construct( ConstrNew ).ReflectVisDiffFront = 0.0;
					Construct( ConstrNew ).TransSolBeamCoef = 0.0;
					Construct( ConstrNew ).TransVisBeamCoef = 0.0;
					Construct( ConstrNew ).ReflSolBeamFrontCoef = 0.0;
					Construct( ConstrNew ).ReflSolBeamBackCoef = 0.0;
					Construct( ConstrNew ).W5FrameDivider = 0;
					Construct( ConstrNew ).FromWindow5DataFile = false;
					Construct( ConstrNew ).W5FileMullionWidth = 0.0;
					Construct( ConstrNew ).W5FileMullionOrientation = 0;
					Construct( ConstrNew ).W5FileGlazingSysWidth = 0.0;
					Construct( ConstrNew ).W5FileGlazingSysHeight = 0.0;
				} // End of check if new window constructions have to be created
			} // End of loop over unshaded and shaded window constructions
		} // End of loop over storm window objects

	}

	void
	ModifyWindow(
		int const SurfNum, // SurfNum has construction of glazing system from Window5 Data File;
		bool & ErrorsFound, // Set to true if errors found
		int & AddedSubSurfaces // Subsurfaces added when window references a
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   Feb 2002
		//       MODIFIED       June 2004, FCW: SinAzim, CosAzim, SinTilt, CosTilt, OutNormVec, GrossArea
		//                       and Perimeter weren't being set for created window for case when
		//                       window from Window5DataFile had two different glazing systems. Also,
		//                       GrossArea and Perimeter of original window were not being recalculated.
		//                      October 2007, LKL: Net area for shading calculations was not being
		//                       recalculated.
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// If a window from the Window5DataFile has one glazing system, modify the
		// vertex coordinates of the original window to correspond to the dimensions
		// of the glazing system on the Data File.
		// If a window from the Window5DataFile has two different glazing systems, split
		// the window into two separate windows with different properties and adjust the
		// vertices of these windows taking into account the dimensions of the glazing systems
		// on the Data File and the width and orientation of the mullion that separates
		// the glazing systems.

		// METHODOLOGY EMPLOYED:na
		// REFERENCES:na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using General::RoundSigDigits;
		using namespace Vectors;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// If there is a second glazing systme on the Data File, SurfNum+1
		// has the construction of the second glazing system.

		// 2-glazing system Window5 data file entry
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		//unused1208  INTEGER :: TotSurfacesPrev                 ! Total number of surfaces before splitting window
		//unused1208  INTEGER :: loop                            ! DO loop index
		Real64 H; // Height and width of original window (m)
		Real64 W;
		//unused1208  REAL(r64)    :: MulWidth                        ! Mullion width (m)
		Real64 h1; // height and width of first glazing system (m)
		Real64 w1;
		//unused1208  REAL(r64)    :: h2,w2                           ! height and width of second glazing system (m)
		//unused1208  type (rectangularwindow) :: NewCoord
		int IConst; // Construction number of first glazing system
		int IConst2; // Construction number of second glazing system
		std::string Const2Name; // Name of construction of second glazing system
		//unused1208  REAL(r64)    :: AreaNew                         ! Sum of areas of the two glazing systems (m2)

		struct rectangularwindow
		{
			// Members
			Array1D< Vector > Vertex;

			// Default Constructor
			rectangularwindow() :
				Vertex( 4 )
			{}

		};

		// Object Data
		Vector TVect;
		rectangularwindow OriginalCoord;

		IConst = SurfaceTmp( SurfNum ).Construction;

		// Height and width of original window
		TVect = SurfaceTmp( SurfNum ).Vertex( 3 ) - SurfaceTmp( SurfNum ).Vertex( 2 );
		W = VecLength( TVect ); //SQRT((X(3)-X(2))**2 + (Y(3)-Y(2))**2 + (Z(3)-Z(2))**2)
		TVect = SurfaceTmp( SurfNum ).Vertex( 2 ) - SurfaceTmp( SurfNum ).Vertex( 1 );
		H = VecLength( TVect ); //SQRT((X(1)-X(2))**2 + (Y(1)-Y(2))**2 + (Z(1)-Z(2))**2)

		// Save coordinates of original window in case Window 5 data overwrites.
		OriginalCoord.Vertex( {1,SurfaceTmp( SurfNum ).Sides} ) = SurfaceTmp( SurfNum ).Vertex( {1,SurfaceTmp( SurfNum ).Sides} );

		// Height and width of first glazing system
		h1 = Construct( IConst ).W5FileGlazingSysHeight;
		w1 = Construct( IConst ).W5FileGlazingSysWidth;

		Const2Name = Construct( IConst ).Name + ":2";
		IConst2 = FindItemInList( Const2Name, Construct );

		if ( IConst2 == 0 ) { // Only one glazing system on Window5 Data File for this window.

			// So... original dimensions and area of window are used (entered in IDF)
			// Warning if dimensions of original window differ from those on Data File by more than 10%

			if ( std::abs( ( H - h1 ) / H ) > 0.10 || std::abs( ( W - w1 ) / W ) > 0.10 ) {

				if ( DisplayExtraWarnings ) {
					ShowWarningError( "SurfaceGeometry: ModifyWindow: Window " + SurfaceTmp( SurfNum ).Name + " uses the Window5 Data File Construction " + Construct( IConst ).Name );
					ShowContinueError( "The height " + RoundSigDigits( H, 3 ) + "(m) or width " + RoundSigDigits( W, 3 ) + " (m) of this window differs by more than 10%" );
					ShowContinueError( "from the corresponding height " + RoundSigDigits( h1, 3 ) + " (m) or width " + RoundSigDigits( w1, 3 ) + " (m) on the Window5 Data file." );
					ShowContinueError( "This will affect the frame heat transfer calculation if the frame in the Data File entry" );
					ShowContinueError( "is not uniform, i.e., has sections with different geometry and/or thermal properties." );
				} else {
					++Warning1Count;
				}

			}

			// Calculate net area for base surface
			SurfaceTmp( SurfaceTmp( SurfNum ).BaseSurf ).Area -= SurfaceTmp( SurfNum ).Area;
			if ( SurfaceTmp( SurfaceTmp( SurfNum ).BaseSurf ).Area <= 0.0 ) {
				ShowSevereError( "Subsurfaces have too much area for base surface=" + SurfaceTmp( SurfaceTmp( SurfNum ).BaseSurf ).Name );
				ShowContinueError( "Subsurface creating error=" + SurfaceTmp( SurfNum ).Name );
				ErrorsFound = true;
			}

			// Net area of base surface with unity window multipliers (used in shadowing checks)
			SurfaceTmp( SurfaceTmp( SurfNum ).BaseSurf ).NetAreaShadowCalc -= SurfaceTmp( SurfNum ).Area / SurfaceTmp( SurfNum ).Multiplier;

		} else { // Two glazing systems on Window5 data file for this window

			// if exterior window, okay.

			if ( SurfaceTmp( SurfNum ).ExtBoundCond == ExternalEnvironment ) {
				//There are two glazing systems (separated by a vertical or horizontal mullion) on the Window5 Data File.
				// Fill in geometry data for the second window (corresponding to the second glazing system on the data file.
				// The first glazing system is assumed to be at left for vertical mullion, at bottom for horizontal mullion.
				// The second glazing system is assumed to be at right for vertical mullion, at top for horizontal mullion.
				// The lower left-hand corner of the original window (its vertex #2) is assumed to coincide with
				// vertex #2 of the first glazing system.

				if ( DisplayExtraWarnings ) {
					ShowMessage( "SurfaceGeometry: ModifyWindow: Window " + SurfaceTmp( SurfNum ).Name + " has been replaced with the Window 5/6 two glazing system=\"" + Construct( IConst ).Name + "\"." );
					ShowContinueError( "Note that originally entered dimensions are overridden." );
				} else {
					++Warning2Count;
				}

				// Allocate another window
				AddWindow( SurfNum, ErrorsFound, AddedSubSurfaces );

			} else if ( SurfaceTmp( SurfNum ).ExtBoundCond > 0 ) { // Interior window, specified  ! not external environment

				if ( DisplayExtraWarnings ) {
					ShowWarningError( "SurfaceGeometry: ModifyWindow: Interior Window " + SurfaceTmp( SurfNum ).Name + " has been replaced with the Window 5/6 two glazing system=\"" + Construct( IConst ).Name + "\"." );
					ShowContinueError( "Please check to make sure interior window is correct. Note that originally entered dimensions are overridden." );
				} else {
					++Warning3Count;
				}

				AddWindow( SurfNum, ErrorsFound, AddedSubSurfaces );

			} else { // Interior window, specified not entered

				ShowSevereError( "SurfaceGeometry: ModifyWindow: Interior Window " + SurfaceTmp( SurfNum ).Name + " is a window in an adjacent zone." );
				ShowContinueError( "Attempted to add/reverse Window 5/6 multiple glazing system=\"" + Construct( IConst ).Name + "\"." );
				ShowContinueError( "Cannot use these Window 5/6 constructs for these Interior Windows. Program will terminate." );
				ErrorsFound = true;

			}

		} // End of check if one or two glazing systems are on the Window5 Data File

	}

	void
	AddWindow(
		int const SurfNum, // SurfNum has construction of glazing system from Window5 Data File;
		bool & ErrorsFound, // Set to true if errors found
		int & AddedSubSurfaces // Subsurfaces added when window references a
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   Nov 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine is called from ModifyWindow to add a window.  Allows it to be
		// called in more than one place in the calling routine so as to be able to have
		// specific warnings or errors issued.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::FindItemInList;
		using General::RoundSigDigits;
		using namespace Vectors;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// If there is a second glazing systme on the Data File, SurfNum+1
		// has the construction of the second glazing system.

		// 2-glazing system Window5 data file entry

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int loop; // DO loop index
		Real64 H; // Height and width of original window (m)
		Real64 W;
		Real64 MulWidth; // Mullion width (m)
		Real64 h1; // height and width of first glazing system (m)
		Real64 w1;
		Real64 h2; // height and width of second glazing system (m)
		Real64 w2;
		Real64 xa; // Vertex intermediate variables (m)
		Real64 ya;
		Real64 za;
		Real64 xb;
		Real64 yb;
		Real64 zb;
		Real64 dx; // Vertex displacements from original window (m)
		Real64 dy;
		int IConst; // Construction number of first glazing system
		int IConst2; // Construction number of second glazing system
		std::string Const2Name; // Name of construction of second glazing system
		Real64 AreaNew; // Sum of areas of the two glazing systems (m2)

		struct rectangularwindow
		{
			// Members
			Array1D< Vector > Vertex;

			// Default Constructor
			rectangularwindow() :
				Vertex( 4 )
			{}

		};

		// Object Data
		Vector TVect;
		rectangularwindow NewCoord;
		rectangularwindow OriginalCoord;

		IConst = SurfaceTmp( SurfNum ).Construction;

		// Height and width of original window
		TVect = SurfaceTmp( SurfNum ).Vertex( 3 ) - SurfaceTmp( SurfNum ).Vertex( 2 );
		W = VecLength( TVect ); //SQRT((X(3)-X(2))**2 + (Y(3)-Y(2))**2 + (Z(3)-Z(2))**2)
		TVect = SurfaceTmp( SurfNum ).Vertex( 2 ) - SurfaceTmp( SurfNum ).Vertex( 1 );
		H = VecLength( TVect ); //SQRT((X(1)-X(2))**2 + (Y(1)-Y(2))**2 + (Z(1)-Z(2))**2)

		// Save coordinates of original window in case Window 5 data overwrites.
		OriginalCoord.Vertex( {1,SurfaceTmp( SurfNum ).Sides} ) = SurfaceTmp( SurfNum ).Vertex( {1,SurfaceTmp( SurfNum ).Sides} );

		// Height and width of first glazing system
		h1 = Construct( IConst ).W5FileGlazingSysHeight;
		w1 = Construct( IConst ).W5FileGlazingSysWidth;

		Const2Name = Construct( IConst ).Name + ":2";
		IConst2 = FindItemInList( Const2Name, Construct );

		++AddedSubSurfaces;
		SurfaceTmp.redimension( ++TotSurfaces );

		SurfaceTmp( TotSurfaces ).Vertex.allocate( 4 );

		SurfaceTmp( TotSurfaces ).Name = SurfaceTmp( SurfNum ).Name + ":2";
		SurfaceTmp( TotSurfaces ).Construction = IConst2;
		SurfaceTmp( TotSurfaces ).ConstructionStoredInputValue = IConst2;
		SurfaceTmp( TotSurfaces ).Class = SurfaceTmp( SurfNum ).Class;
		SurfaceTmp( TotSurfaces ).Azimuth = SurfaceTmp( SurfNum ).Azimuth;
		// Sine and cosine of azimuth and tilt
		SurfaceTmp( TotSurfaces ).SinAzim = SurfaceTmp( SurfNum ).SinAzim;
		SurfaceTmp( TotSurfaces ).CosAzim = SurfaceTmp( SurfNum ).CosAzim;
		SurfaceTmp( TotSurfaces ).SinTilt = SurfaceTmp( SurfNum ).SinTilt;
		SurfaceTmp( TotSurfaces ).CosTilt = SurfaceTmp( SurfNum ).CosTilt;
		// Outward normal unit vector (pointing away from room)
		SurfaceTmp( TotSurfaces ).Centroid = SurfaceTmp( SurfNum ).Centroid;
		SurfaceTmp( TotSurfaces ).lcsx = SurfaceTmp( SurfNum ).lcsx;
		SurfaceTmp( TotSurfaces ).lcsy = SurfaceTmp( SurfNum ).lcsy;
		SurfaceTmp( TotSurfaces ).lcsz = SurfaceTmp( SurfNum ).lcsz;
		SurfaceTmp( TotSurfaces ).NewellAreaVector = SurfaceTmp( SurfNum ).NewellAreaVector;
		SurfaceTmp( TotSurfaces ).OutNormVec = SurfaceTmp( SurfNum ).OutNormVec;
		SurfaceTmp( TotSurfaces ).Reveal = SurfaceTmp( SurfNum ).Reveal;
		SurfaceTmp( TotSurfaces ).Shape = SurfaceTmp( SurfNum ).Shape;
		SurfaceTmp( TotSurfaces ).Sides = SurfaceTmp( SurfNum ).Sides;
		SurfaceTmp( TotSurfaces ).Tilt = SurfaceTmp( SurfNum ).Tilt;
		SurfaceTmp( TotSurfaces ).HeatTransSurf = SurfaceTmp( SurfNum ).HeatTransSurf;
		SurfaceTmp( TotSurfaces ).BaseSurfName = SurfaceTmp( SurfNum ).BaseSurfName;
		SurfaceTmp( TotSurfaces ).BaseSurf = SurfaceTmp( SurfNum ).BaseSurf;
		SurfaceTmp( TotSurfaces ).ZoneName = SurfaceTmp( SurfNum ).ZoneName;
		SurfaceTmp( TotSurfaces ).Zone = SurfaceTmp( SurfNum ).Zone;
		SurfaceTmp( TotSurfaces ).ExtBoundCondName = SurfaceTmp( SurfNum ).ExtBoundCondName;
		SurfaceTmp( TotSurfaces ).ExtBoundCond = SurfaceTmp( SurfNum ).ExtBoundCond;
		SurfaceTmp( TotSurfaces ).ExtSolar = SurfaceTmp( SurfNum ).ExtSolar;
		SurfaceTmp( TotSurfaces ).ExtWind = SurfaceTmp( SurfNum ).ExtWind;
		SurfaceTmp( TotSurfaces ).ViewFactorGround = SurfaceTmp( SurfNum ).ViewFactorGround;
		SurfaceTmp( TotSurfaces ).ViewFactorSky = SurfaceTmp( SurfNum ).ViewFactorSky;
		SurfaceTmp( TotSurfaces ).ViewFactorGroundIR = SurfaceTmp( SurfNum ).ViewFactorGroundIR;
		SurfaceTmp( TotSurfaces ).ViewFactorSkyIR = SurfaceTmp( SurfNum ).ViewFactorSkyIR;
		SurfaceTmp( TotSurfaces ).OSCPtr = SurfaceTmp( SurfNum ).OSCPtr;
		SurfaceTmp( TotSurfaces ).SchedShadowSurfIndex = SurfaceTmp( SurfNum ).SchedShadowSurfIndex;
		SurfaceTmp( TotSurfaces ).ShadowSurfSchedVaries = SurfaceTmp( SurfNum ).ShadowSurfSchedVaries;
		SurfaceTmp( TotSurfaces ).MaterialMovInsulExt = SurfaceTmp( SurfNum ).MaterialMovInsulExt;
		SurfaceTmp( TotSurfaces ).MaterialMovInsulInt = SurfaceTmp( SurfNum ).MaterialMovInsulInt;
		SurfaceTmp( TotSurfaces ).SchedMovInsulExt = SurfaceTmp( SurfNum ).SchedMovInsulExt;
		SurfaceTmp( TotSurfaces ).WindowShadingControlPtr = SurfaceTmp( SurfNum ).WindowShadingControlPtr;
		SurfaceTmp( TotSurfaces ).ShadedConstruction = SurfaceTmp( SurfNum ).ShadedConstruction;
		SurfaceTmp( TotSurfaces ).FrameDivider = SurfaceTmp( SurfNum ).FrameDivider;
		SurfaceTmp( TotSurfaces ).Multiplier = SurfaceTmp( SurfNum ).Multiplier;
		SurfaceTmp( TotSurfaces ).NetAreaShadowCalc = SurfaceTmp( SurfNum ).NetAreaShadowCalc;

		MulWidth = Construct( IConst ).W5FileMullionWidth;
		w2 = Construct( IConst2 ).W5FileGlazingSysWidth;
		h2 = Construct( IConst2 ).W5FileGlazingSysHeight;

		// Correction to net area of base surface. Add back in the original glazing area and subtract the
		// area of the two glazing systems. Note that for Surface(SurfNum)%Class = 'Window' the effect
		// of a window multiplier is included in the glazing area. Note that frame areas are subtracted later.

		AreaNew = SurfaceTmp( SurfNum ).Multiplier * ( h1 * w1 + h2 * w2 ); // both glazing systems
		// Adjust net area for base surface
		SurfaceTmp( SurfaceTmp( SurfNum ).BaseSurf ).Area -= AreaNew;

		// Net area of base surface with unity window multipliers (used in shadowing checks)
		SurfaceTmp( SurfaceTmp( SurfNum ).BaseSurf ).NetAreaShadowCalc -= AreaNew / SurfaceTmp( SurfNum ).Multiplier;

		// Reset area, etc. of original window
		SurfaceTmp( SurfNum ).Area = SurfaceTmp( SurfNum ).Multiplier * ( h1 * w1 );
		SurfaceTmp( SurfNum ).GrossArea = SurfaceTmp( SurfNum ).Area;
		SurfaceTmp( SurfNum ).NetAreaShadowCalc = h1 * w1;
		SurfaceTmp( SurfNum ).Perimeter = 2 * ( h1 + w1 );
		SurfaceTmp( SurfNum ).Height = h1;
		SurfaceTmp( SurfNum ).Width = w1;
		// Set area, etc. of new window
		SurfaceTmp( TotSurfaces ).Area = SurfaceTmp( TotSurfaces ).Multiplier * ( h2 * w2 );
		SurfaceTmp( TotSurfaces ).GrossArea = SurfaceTmp( TotSurfaces ).Area;
		SurfaceTmp( TotSurfaces ).NetAreaShadowCalc = h2 * w2;
		SurfaceTmp( TotSurfaces ).Perimeter = 2 * ( h2 + w2 );
		SurfaceTmp( TotSurfaces ).Height = h2;
		SurfaceTmp( TotSurfaces ).Width = w2;

		if ( SurfaceTmp( SurfaceTmp( SurfNum ).BaseSurf ).Area <= 0.0 ) {
			ShowSevereError( "SurfaceGeometry: ModifyWindow: Subsurfaces have too much area for base surface=" + SurfaceTmp( SurfaceTmp( SurfNum ).BaseSurf ).Name );
			ShowContinueError( "Subsurface (window) creating error=" + SurfaceTmp( SurfNum ).Name );
			ShowContinueError( "This window has been replaced by two windows from the Window5 Data File of total area " + RoundSigDigits( AreaNew, 2 ) + " m2" );
			ErrorsFound = true;
		}

		// Assign vertices to the new window; modify vertices of original window.
		// In the following, vertices are numbered counter-clockwise with vertex #1 at the upper left.

		if ( Construct( IConst ).W5FileMullionOrientation == Vertical ) {

			// VERTICAL MULLION: original window is modified to become left-hand glazing (system #1);
			// new window is created to become right-hand glazing (system #2)

			// Left-hand glazing

			// Vertex 1
			dx = 0.0;
			dy = H - h1;
			xa = OriginalCoord.Vertex( 1 ).x + ( dy / H ) * ( OriginalCoord.Vertex( 2 ).x - OriginalCoord.Vertex( 1 ).x );
			ya = OriginalCoord.Vertex( 1 ).y + ( dy / H ) * ( OriginalCoord.Vertex( 2 ).y - OriginalCoord.Vertex( 1 ).y );
			za = OriginalCoord.Vertex( 1 ).z + ( dy / H ) * ( OriginalCoord.Vertex( 2 ).z - OriginalCoord.Vertex( 1 ).z );
			xb = OriginalCoord.Vertex( 4 ).x + ( dy / H ) * ( OriginalCoord.Vertex( 3 ).x - OriginalCoord.Vertex( 4 ).x );
			yb = OriginalCoord.Vertex( 4 ).y + ( dy / H ) * ( OriginalCoord.Vertex( 3 ).y - OriginalCoord.Vertex( 4 ).y );
			zb = OriginalCoord.Vertex( 4 ).z + ( dy / H ) * ( OriginalCoord.Vertex( 3 ).z - OriginalCoord.Vertex( 4 ).z );
			NewCoord.Vertex( 1 ).x = xa + ( dx / W ) * ( xb - xa );
			NewCoord.Vertex( 1 ).y = ya + ( dx / W ) * ( yb - ya );
			NewCoord.Vertex( 1 ).z = za + ( dx / W ) * ( zb - za );

			// Vertex 2
			dx = 0.0;
			dy = H;
			xa = OriginalCoord.Vertex( 1 ).x + ( dy / H ) * ( OriginalCoord.Vertex( 2 ).x - OriginalCoord.Vertex( 1 ).x );
			ya = OriginalCoord.Vertex( 1 ).y + ( dy / H ) * ( OriginalCoord.Vertex( 2 ).y - OriginalCoord.Vertex( 1 ).y );
			za = OriginalCoord.Vertex( 1 ).z + ( dy / H ) * ( OriginalCoord.Vertex( 2 ).z - OriginalCoord.Vertex( 1 ).z );
			xb = OriginalCoord.Vertex( 4 ).x + ( dy / H ) * ( OriginalCoord.Vertex( 3 ).x - OriginalCoord.Vertex( 4 ).x );
			yb = OriginalCoord.Vertex( 4 ).y + ( dy / H ) * ( OriginalCoord.Vertex( 3 ).y - OriginalCoord.Vertex( 4 ).y );
			zb = OriginalCoord.Vertex( 4 ).z + ( dy / H ) * ( OriginalCoord.Vertex( 3 ).z - OriginalCoord.Vertex( 4 ).z );
			NewCoord.Vertex( 2 ).x = xa + ( dx / W ) * ( xb - xa );
			NewCoord.Vertex( 2 ).y = ya + ( dx / W ) * ( yb - ya );
			NewCoord.Vertex( 2 ).z = za + ( dx / W ) * ( zb - za );

			// Vertex 3
			dx = w1;
			dy = H;
			xa = OriginalCoord.Vertex( 1 ).x + ( dy / H ) * ( OriginalCoord.Vertex( 2 ).x - OriginalCoord.Vertex( 1 ).x );
			ya = OriginalCoord.Vertex( 1 ).y + ( dy / H ) * ( OriginalCoord.Vertex( 2 ).y - OriginalCoord.Vertex( 1 ).y );
			za = OriginalCoord.Vertex( 1 ).z + ( dy / H ) * ( OriginalCoord.Vertex( 2 ).z - OriginalCoord.Vertex( 1 ).z );
			xb = OriginalCoord.Vertex( 4 ).x + ( dy / H ) * ( OriginalCoord.Vertex( 3 ).x - OriginalCoord.Vertex( 4 ).x );
			yb = OriginalCoord.Vertex( 4 ).y + ( dy / H ) * ( OriginalCoord.Vertex( 3 ).y - OriginalCoord.Vertex( 4 ).y );
			zb = OriginalCoord.Vertex( 4 ).z + ( dy / H ) * ( OriginalCoord.Vertex( 3 ).z - OriginalCoord.Vertex( 4 ).z );
			NewCoord.Vertex( 3 ).x = xa + ( dx / W ) * ( xb - xa );
			NewCoord.Vertex( 3 ).y = ya + ( dx / W ) * ( yb - ya );
			NewCoord.Vertex( 3 ).z = za + ( dx / W ) * ( zb - za );

			// Vertex 4
			dx = w1;
			dy = H - h1;
			xa = OriginalCoord.Vertex( 1 ).x + ( dy / H ) * ( OriginalCoord.Vertex( 2 ).x - OriginalCoord.Vertex( 1 ).x );
			ya = OriginalCoord.Vertex( 1 ).y + ( dy / H ) * ( OriginalCoord.Vertex( 2 ).y - OriginalCoord.Vertex( 1 ).y );
			za = OriginalCoord.Vertex( 1 ).z + ( dy / H ) * ( OriginalCoord.Vertex( 2 ).z - OriginalCoord.Vertex( 1 ).z );
			xb = OriginalCoord.Vertex( 4 ).x + ( dy / H ) * ( OriginalCoord.Vertex( 3 ).x - OriginalCoord.Vertex( 4 ).x );
			yb = OriginalCoord.Vertex( 4 ).y + ( dy / H ) * ( OriginalCoord.Vertex( 3 ).y - OriginalCoord.Vertex( 4 ).y );
			zb = OriginalCoord.Vertex( 4 ).z + ( dy / H ) * ( OriginalCoord.Vertex( 3 ).z - OriginalCoord.Vertex( 4 ).z );
			NewCoord.Vertex( 4 ).x = xa + ( dx / W ) * ( xb - xa );
			NewCoord.Vertex( 4 ).y = ya + ( dx / W ) * ( yb - ya );
			NewCoord.Vertex( 4 ).z = za + ( dx / W ) * ( zb - za );

			for ( loop = 1; loop <= SurfaceTmp( SurfNum ).Sides; ++loop ) {
				SurfaceTmp( SurfNum ).Vertex( loop ) = NewCoord.Vertex( loop );
			}

			// Right-hand glazing

			// Vertex 1
			dx = w1 + MulWidth;
			dy = H - ( h1 + h2 ) / 2.0;
			xa = OriginalCoord.Vertex( 1 ).x + ( dy / H ) * ( OriginalCoord.Vertex( 2 ).x - OriginalCoord.Vertex( 1 ).x );
			ya = OriginalCoord.Vertex( 1 ).y + ( dy / H ) * ( OriginalCoord.Vertex( 2 ).y - OriginalCoord.Vertex( 1 ).y );
			za = OriginalCoord.Vertex( 1 ).z + ( dy / H ) * ( OriginalCoord.Vertex( 2 ).z - OriginalCoord.Vertex( 1 ).z );
			xb = OriginalCoord.Vertex( 4 ).x + ( dy / H ) * ( OriginalCoord.Vertex( 3 ).x - OriginalCoord.Vertex( 4 ).x );
			yb = OriginalCoord.Vertex( 4 ).y + ( dy / H ) * ( OriginalCoord.Vertex( 3 ).y - OriginalCoord.Vertex( 4 ).y );
			zb = OriginalCoord.Vertex( 4 ).z + ( dy / H ) * ( OriginalCoord.Vertex( 3 ).z - OriginalCoord.Vertex( 4 ).z );
			NewCoord.Vertex( 1 ).x = xa + ( dx / W ) * ( xb - xa );
			NewCoord.Vertex( 1 ).y = ya + ( dx / W ) * ( yb - ya );
			NewCoord.Vertex( 1 ).z = za + ( dx / W ) * ( zb - za );

			// Vertex 2
			dx = w1 + MulWidth;
			dy = H + ( h2 - h1 ) / 2.0;
			xa = OriginalCoord.Vertex( 1 ).x + ( dy / H ) * ( OriginalCoord.Vertex( 2 ).x - OriginalCoord.Vertex( 1 ).x );
			ya = OriginalCoord.Vertex( 1 ).y + ( dy / H ) * ( OriginalCoord.Vertex( 2 ).y - OriginalCoord.Vertex( 1 ).y );
			za = OriginalCoord.Vertex( 1 ).z + ( dy / H ) * ( OriginalCoord.Vertex( 2 ).z - OriginalCoord.Vertex( 1 ).z );
			xb = OriginalCoord.Vertex( 4 ).x + ( dy / H ) * ( OriginalCoord.Vertex( 3 ).x - OriginalCoord.Vertex( 4 ).x );
			yb = OriginalCoord.Vertex( 4 ).y + ( dy / H ) * ( OriginalCoord.Vertex( 3 ).y - OriginalCoord.Vertex( 4 ).y );
			zb = OriginalCoord.Vertex( 4 ).z + ( dy / H ) * ( OriginalCoord.Vertex( 3 ).z - OriginalCoord.Vertex( 4 ).z );
			NewCoord.Vertex( 2 ).x = xa + ( dx / W ) * ( xb - xa );
			NewCoord.Vertex( 2 ).y = ya + ( dx / W ) * ( yb - ya );
			NewCoord.Vertex( 2 ).z = za + ( dx / W ) * ( zb - za );

			// Vertex 3
			dx = w1 + MulWidth + w2;
			dy = H + ( h2 - h1 ) / 2.0;
			xa = OriginalCoord.Vertex( 1 ).x + ( dy / H ) * ( OriginalCoord.Vertex( 2 ).x - OriginalCoord.Vertex( 1 ).x );
			ya = OriginalCoord.Vertex( 1 ).y + ( dy / H ) * ( OriginalCoord.Vertex( 2 ).y - OriginalCoord.Vertex( 1 ).y );
			za = OriginalCoord.Vertex( 1 ).z + ( dy / H ) * ( OriginalCoord.Vertex( 2 ).z - OriginalCoord.Vertex( 1 ).z );
			xb = OriginalCoord.Vertex( 4 ).x + ( dy / H ) * ( OriginalCoord.Vertex( 3 ).x - OriginalCoord.Vertex( 4 ).x );
			yb = OriginalCoord.Vertex( 4 ).y + ( dy / H ) * ( OriginalCoord.Vertex( 3 ).y - OriginalCoord.Vertex( 4 ).y );
			zb = OriginalCoord.Vertex( 4 ).z + ( dy / H ) * ( OriginalCoord.Vertex( 3 ).z - OriginalCoord.Vertex( 4 ).z );
			NewCoord.Vertex( 3 ).x = xa + ( dx / W ) * ( xb - xa );
			NewCoord.Vertex( 3 ).y = ya + ( dx / W ) * ( yb - ya );
			NewCoord.Vertex( 3 ).z = za + ( dx / W ) * ( zb - za );

			// Vertex 4
			dx = w1 + MulWidth + w2;
			dy = H - ( h1 + h2 ) / 2.0;
			xa = OriginalCoord.Vertex( 1 ).x + ( dy / H ) * ( OriginalCoord.Vertex( 2 ).x - OriginalCoord.Vertex( 1 ).x );
			ya = OriginalCoord.Vertex( 1 ).y + ( dy / H ) * ( OriginalCoord.Vertex( 2 ).y - OriginalCoord.Vertex( 1 ).y );
			za = OriginalCoord.Vertex( 1 ).z + ( dy / H ) * ( OriginalCoord.Vertex( 2 ).z - OriginalCoord.Vertex( 1 ).z );
			xb = OriginalCoord.Vertex( 4 ).x + ( dy / H ) * ( OriginalCoord.Vertex( 3 ).x - OriginalCoord.Vertex( 4 ).x );
			yb = OriginalCoord.Vertex( 4 ).y + ( dy / H ) * ( OriginalCoord.Vertex( 3 ).y - OriginalCoord.Vertex( 4 ).y );
			zb = OriginalCoord.Vertex( 4 ).z + ( dy / H ) * ( OriginalCoord.Vertex( 3 ).z - OriginalCoord.Vertex( 4 ).z );
			NewCoord.Vertex( 4 ).x = xa + ( dx / W ) * ( xb - xa );
			NewCoord.Vertex( 4 ).y = ya + ( dx / W ) * ( yb - ya );
			NewCoord.Vertex( 4 ).z = za + ( dx / W ) * ( zb - za );

			for ( loop = 1; loop <= SurfaceTmp( TotSurfaces ).Sides; ++loop ) {
				SurfaceTmp( TotSurfaces ).Vertex( loop ) = NewCoord.Vertex( loop );
			}

		} else { // Horizontal mullion

			// HORIZONTAL MULLION: original window is modified to become bottom glazing (system #1);
			// new window is created to become top glazing (system #2)

			// Bottom glazing

			// Vertex 1
			dx = 0.0;
			dy = H - h1;
			xa = OriginalCoord.Vertex( 1 ).x + ( dy / H ) * ( OriginalCoord.Vertex( 2 ).x - OriginalCoord.Vertex( 1 ).x );
			ya = OriginalCoord.Vertex( 1 ).y + ( dy / H ) * ( OriginalCoord.Vertex( 2 ).y - OriginalCoord.Vertex( 1 ).y );
			za = OriginalCoord.Vertex( 1 ).z + ( dy / H ) * ( OriginalCoord.Vertex( 2 ).z - OriginalCoord.Vertex( 1 ).z );
			xb = OriginalCoord.Vertex( 4 ).x + ( dy / H ) * ( OriginalCoord.Vertex( 3 ).x - OriginalCoord.Vertex( 4 ).x );
			yb = OriginalCoord.Vertex( 4 ).y + ( dy / H ) * ( OriginalCoord.Vertex( 3 ).y - OriginalCoord.Vertex( 4 ).y );
			zb = OriginalCoord.Vertex( 4 ).z + ( dy / H ) * ( OriginalCoord.Vertex( 3 ).z - OriginalCoord.Vertex( 4 ).z );
			NewCoord.Vertex( 1 ).x = xa + ( dx / W ) * ( xb - xa );
			NewCoord.Vertex( 1 ).y = ya + ( dx / W ) * ( yb - ya );
			NewCoord.Vertex( 1 ).z = za + ( dx / W ) * ( zb - za );

			// Vertex 2
			dx = 0.0;
			dy = H;
			xa = OriginalCoord.Vertex( 1 ).x + ( dy / H ) * ( OriginalCoord.Vertex( 2 ).x - OriginalCoord.Vertex( 1 ).x );
			ya = OriginalCoord.Vertex( 1 ).y + ( dy / H ) * ( OriginalCoord.Vertex( 2 ).y - OriginalCoord.Vertex( 1 ).y );
			za = OriginalCoord.Vertex( 1 ).z + ( dy / H ) * ( OriginalCoord.Vertex( 2 ).z - OriginalCoord.Vertex( 1 ).z );
			xb = OriginalCoord.Vertex( 4 ).x + ( dy / H ) * ( OriginalCoord.Vertex( 3 ).x - OriginalCoord.Vertex( 4 ).x );
			yb = OriginalCoord.Vertex( 4 ).y + ( dy / H ) * ( OriginalCoord.Vertex( 3 ).y - OriginalCoord.Vertex( 4 ).y );
			zb = OriginalCoord.Vertex( 4 ).z + ( dy / H ) * ( OriginalCoord.Vertex( 3 ).z - OriginalCoord.Vertex( 4 ).z );
			NewCoord.Vertex( 2 ).x = xa + ( dx / W ) * ( xb - xa );
			NewCoord.Vertex( 2 ).y = ya + ( dx / W ) * ( yb - ya );
			NewCoord.Vertex( 2 ).z = za + ( dx / W ) * ( zb - za );

			// Vertex 3
			dx = w1;
			dy = H;
			xa = OriginalCoord.Vertex( 1 ).x + ( dy / H ) * ( OriginalCoord.Vertex( 2 ).x - OriginalCoord.Vertex( 1 ).x );
			ya = OriginalCoord.Vertex( 1 ).y + ( dy / H ) * ( OriginalCoord.Vertex( 2 ).y - OriginalCoord.Vertex( 1 ).y );
			za = OriginalCoord.Vertex( 1 ).z + ( dy / H ) * ( OriginalCoord.Vertex( 2 ).z - OriginalCoord.Vertex( 1 ).z );
			xb = OriginalCoord.Vertex( 4 ).x + ( dy / H ) * ( OriginalCoord.Vertex( 3 ).x - OriginalCoord.Vertex( 4 ).x );
			yb = OriginalCoord.Vertex( 4 ).y + ( dy / H ) * ( OriginalCoord.Vertex( 3 ).y - OriginalCoord.Vertex( 4 ).y );
			zb = OriginalCoord.Vertex( 4 ).z + ( dy / H ) * ( OriginalCoord.Vertex( 3 ).z - OriginalCoord.Vertex( 4 ).z );
			NewCoord.Vertex( 3 ).x = xa + ( dx / W ) * ( xb - xa );
			NewCoord.Vertex( 3 ).y = ya + ( dx / W ) * ( yb - ya );
			NewCoord.Vertex( 3 ).z = za + ( dx / W ) * ( zb - za );

			// Vertex 4
			dx = w1;
			dy = H - h1;
			xa = OriginalCoord.Vertex( 1 ).x + ( dy / H ) * ( OriginalCoord.Vertex( 2 ).x - OriginalCoord.Vertex( 1 ).x );
			ya = OriginalCoord.Vertex( 1 ).y + ( dy / H ) * ( OriginalCoord.Vertex( 2 ).y - OriginalCoord.Vertex( 1 ).y );
			za = OriginalCoord.Vertex( 1 ).z + ( dy / H ) * ( OriginalCoord.Vertex( 2 ).z - OriginalCoord.Vertex( 1 ).z );
			xb = OriginalCoord.Vertex( 4 ).x + ( dy / H ) * ( OriginalCoord.Vertex( 3 ).x - OriginalCoord.Vertex( 4 ).x );
			yb = OriginalCoord.Vertex( 4 ).y + ( dy / H ) * ( OriginalCoord.Vertex( 3 ).y - OriginalCoord.Vertex( 4 ).y );
			zb = OriginalCoord.Vertex( 4 ).z + ( dy / H ) * ( OriginalCoord.Vertex( 3 ).z - OriginalCoord.Vertex( 4 ).z );
			NewCoord.Vertex( 4 ).x = xa + ( dx / W ) * ( xb - xa );
			NewCoord.Vertex( 4 ).y = ya + ( dx / W ) * ( yb - ya );
			NewCoord.Vertex( 4 ).z = za + ( dx / W ) * ( zb - za );

			for ( loop = 1; loop <= SurfaceTmp( SurfNum ).Sides; ++loop ) {
				SurfaceTmp( SurfNum ).Vertex( loop ) = NewCoord.Vertex( loop );
			}

			// Top glazing

			// Vertex 1
			dx = ( w1 - w2 ) / 2.0;
			dy = H - ( h1 + h2 + MulWidth );
			xa = OriginalCoord.Vertex( 1 ).x + ( dy / H ) * ( OriginalCoord.Vertex( 2 ).x - OriginalCoord.Vertex( 1 ).x );
			ya = OriginalCoord.Vertex( 1 ).y + ( dy / H ) * ( OriginalCoord.Vertex( 2 ).y - OriginalCoord.Vertex( 1 ).y );
			za = OriginalCoord.Vertex( 1 ).z + ( dy / H ) * ( OriginalCoord.Vertex( 2 ).z - OriginalCoord.Vertex( 1 ).z );
			xb = OriginalCoord.Vertex( 4 ).x + ( dy / H ) * ( OriginalCoord.Vertex( 3 ).x - OriginalCoord.Vertex( 4 ).x );
			yb = OriginalCoord.Vertex( 4 ).y + ( dy / H ) * ( OriginalCoord.Vertex( 3 ).y - OriginalCoord.Vertex( 4 ).y );
			zb = OriginalCoord.Vertex( 4 ).z + ( dy / H ) * ( OriginalCoord.Vertex( 3 ).z - OriginalCoord.Vertex( 4 ).z );
			NewCoord.Vertex( 1 ).x = xa + ( dx / W ) * ( xb - xa );
			NewCoord.Vertex( 1 ).y = ya + ( dx / W ) * ( yb - ya );
			NewCoord.Vertex( 1 ).z = za + ( dx / W ) * ( zb - za );

			// Vertex 2
			dx = ( w1 - w2 ) / 2.0;
			dy = H - ( h1 + MulWidth );
			xa = OriginalCoord.Vertex( 1 ).x + ( dy / H ) * ( OriginalCoord.Vertex( 2 ).x - OriginalCoord.Vertex( 1 ).x );
			ya = OriginalCoord.Vertex( 1 ).y + ( dy / H ) * ( OriginalCoord.Vertex( 2 ).y - OriginalCoord.Vertex( 1 ).y );
			za = OriginalCoord.Vertex( 1 ).z + ( dy / H ) * ( OriginalCoord.Vertex( 2 ).z - OriginalCoord.Vertex( 1 ).z );
			xb = OriginalCoord.Vertex( 4 ).x + ( dy / H ) * ( OriginalCoord.Vertex( 3 ).x - OriginalCoord.Vertex( 4 ).x );
			yb = OriginalCoord.Vertex( 4 ).y + ( dy / H ) * ( OriginalCoord.Vertex( 3 ).y - OriginalCoord.Vertex( 4 ).y );
			zb = OriginalCoord.Vertex( 4 ).z + ( dy / H ) * ( OriginalCoord.Vertex( 3 ).z - OriginalCoord.Vertex( 4 ).z );
			NewCoord.Vertex( 2 ).x = xa + ( dx / W ) * ( xb - xa );
			NewCoord.Vertex( 2 ).y = ya + ( dx / W ) * ( yb - ya );
			NewCoord.Vertex( 2 ).z = za + ( dx / W ) * ( zb - za );

			// Vertex 3
			dx = ( w1 + w2 ) / 2.0;
			dy = H - ( h1 + MulWidth );
			xa = OriginalCoord.Vertex( 1 ).x + ( dy / H ) * ( OriginalCoord.Vertex( 2 ).x - OriginalCoord.Vertex( 1 ).x );
			ya = OriginalCoord.Vertex( 1 ).y + ( dy / H ) * ( OriginalCoord.Vertex( 2 ).y - OriginalCoord.Vertex( 1 ).y );
			za = OriginalCoord.Vertex( 1 ).z + ( dy / H ) * ( OriginalCoord.Vertex( 2 ).z - OriginalCoord.Vertex( 1 ).z );
			xb = OriginalCoord.Vertex( 4 ).x + ( dy / H ) * ( OriginalCoord.Vertex( 3 ).x - OriginalCoord.Vertex( 4 ).x );
			yb = OriginalCoord.Vertex( 4 ).y + ( dy / H ) * ( OriginalCoord.Vertex( 3 ).y - OriginalCoord.Vertex( 4 ).y );
			zb = OriginalCoord.Vertex( 4 ).z + ( dy / H ) * ( OriginalCoord.Vertex( 3 ).z - OriginalCoord.Vertex( 4 ).z );
			NewCoord.Vertex( 3 ).x = xa + ( dx / W ) * ( xb - xa );
			NewCoord.Vertex( 3 ).y = ya + ( dx / W ) * ( yb - ya );
			NewCoord.Vertex( 3 ).z = za + ( dx / W ) * ( zb - za );

			// Vertex 4
			dx = ( w1 + w2 ) / 2.0;
			dy = H - ( h1 + h2 + MulWidth );
			xa = OriginalCoord.Vertex( 1 ).x + ( dy / H ) * ( OriginalCoord.Vertex( 2 ).x - OriginalCoord.Vertex( 1 ).x );
			ya = OriginalCoord.Vertex( 1 ).y + ( dy / H ) * ( OriginalCoord.Vertex( 2 ).y - OriginalCoord.Vertex( 1 ).y );
			za = OriginalCoord.Vertex( 1 ).z + ( dy / H ) * ( OriginalCoord.Vertex( 2 ).z - OriginalCoord.Vertex( 1 ).z );
			xb = OriginalCoord.Vertex( 4 ).x + ( dy / H ) * ( OriginalCoord.Vertex( 3 ).x - OriginalCoord.Vertex( 4 ).x );
			yb = OriginalCoord.Vertex( 4 ).y + ( dy / H ) * ( OriginalCoord.Vertex( 3 ).y - OriginalCoord.Vertex( 4 ).y );
			zb = OriginalCoord.Vertex( 4 ).z + ( dy / H ) * ( OriginalCoord.Vertex( 3 ).z - OriginalCoord.Vertex( 4 ).z );
			NewCoord.Vertex( 4 ).x = xa + ( dx / W ) * ( xb - xa );
			NewCoord.Vertex( 4 ).y = ya + ( dx / W ) * ( yb - ya );
			NewCoord.Vertex( 4 ).z = za + ( dx / W ) * ( zb - za );

			for ( loop = 1; loop <= SurfaceTmp( TotSurfaces ).Sides; ++loop ) {
				SurfaceTmp( TotSurfaces ).Vertex( loop ) = NewCoord.Vertex( loop );
			}

		} // End of check if vertical or horizontal mullion

	}

	void
	TransformVertsByAspect(
		int const SurfNum, // Current surface number
		int const NSides // Number of sides to figure
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent T Griffith
		//       DATE WRITTEN   April 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Alter input for surface geometry
		// Optimizing building design for energy can involve
		//  altering building geometry.  Rather than assemble routines to transform
		//  geometry through pre-processing on input, it may be simpler to change
		//  vertices within EnergyPlus since it already reads the data from the input
		//  file and there would no longer be a need to rewrite the text data.
		//  This is essentially a crude hack to allow adjusting geometry with
		//  a single parameter...

		// METHODOLOGY EMPLOYED:
		// once vertices have been converted to WCS, change them to reflect a different aspect
		// ratio for the entire building based on user input.
		// This routine is called once for each surface by subroutine GetVertices

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using namespace InputProcessor;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const CurrentModuleObject( "GeometryTransform" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Array1D_string cAlphas( 1 );
		Array1D< Real64 > rNumerics( 2 );
		int NAlphas;
		int NNum;
		int IOStat;
		static Real64 OldAspectRatio;
		static Real64 NewAspectRatio;
		static bool firstTime( true );
		static bool noTransform( true );
		static std::string transformPlane;
		int n;
		Real64 Xo;
		Real64 XnoRot;
		Real64 Xtrans;
		Real64 Yo;
		Real64 YnoRot;
		Real64 Ytrans;
		//begin execution
		//get user input...

		if ( firstTime ) {
			if ( GetNumObjectsFound( CurrentModuleObject ) == 1 ) {
				GetObjectItem( CurrentModuleObject, 1, cAlphas, NAlphas, rNumerics, NNum, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
				OldAspectRatio = rNumerics( 1 );
				NewAspectRatio = rNumerics( 2 );
				transformPlane = cAlphas( 1 );
				if ( transformPlane != "XY" ) {
					ShowWarningError( CurrentModuleObject + ": invalid " + cAlphaFieldNames( 1 ) + "=\"" + cAlphas( 1 ) + "...ignored." );
				}
				firstTime = false;
				noTransform = false;
				AspectTransform = true;
				if ( WorldCoordSystem ) {
					ShowWarningError( CurrentModuleObject + ": must use Relative Coordinate System.  Transform request ignored." );
					noTransform = true;
					AspectTransform = false;
				}
			} else {
				firstTime = false;
			}
		}
		if ( noTransform ) return;

		//check surface type.
		if ( ! SurfaceTmp( SurfNum ).HeatTransSurf ) {
			// Site Shading do not get transformed.
			if ( SurfaceTmp( SurfNum ).Class == SurfaceClass_Detached_F ) return;
		}

		//testing method of transforming  x and y coordinates as follows

		// this works if not rotated wrt north axis ... but if it is, then trouble
		// try to first derotate it , transform by aspect and then rotate back.

		for ( n = 1; n <= NSides; ++n ) {
			Xo = SurfaceTmp( SurfNum ).Vertex( n ).x; // world coordinates.... shifted by relative north angle...
			Yo = SurfaceTmp( SurfNum ).Vertex( n ).y;
			// next derotate the building
			XnoRot = Xo * CosBldgRelNorth + Yo * SinBldgRelNorth;
			YnoRot = Yo * CosBldgRelNorth - Xo * SinBldgRelNorth;
			// translate
			Xtrans = XnoRot * std::sqrt( NewAspectRatio / OldAspectRatio );
			Ytrans = YnoRot * std::sqrt( OldAspectRatio / NewAspectRatio );
			// rerotate
			SurfaceTmp( SurfNum ).Vertex( n ).x = Xtrans * CosBldgRelNorth - Ytrans * SinBldgRelNorth;

			SurfaceTmp( SurfNum ).Vertex( n ).y = Xtrans * SinBldgRelNorth + Ytrans * CosBldgRelNorth;
		}

	}

	void
	CalcSurfaceCentroid()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   Feb. 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// compute centroid of all the surfaces in the main
		// surface structure. Store the vertex coordinates of
		// the centroid in the 'SURFACE' derived type.

		// METHODOLOGY EMPLOYED:
		// The centroid of triangle is easily computed by averaging the vertices
		// The centroid of a quadrilateral is computed by area weighting the centroids
		// of two triangles.
		// (Algorithm would need to be changed for higher order
		// polygons with more than four sides).

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace Vectors;
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// Object Data
		static Array1D< Vector > Triangle1( 3 ); // working struct for a 3-sided surface
		static Array1D< Vector > Triangle2( 3 ); // working struct for a 3-sided surface
		static Vector const zero_vector( 0.0 );
		Vector centroid;

		int negZcount( 0 ); // for warning error in surface centroids

		// loop through all the surfaces
		for ( int ThisSurf = 1; ThisSurf <= TotSurfaces; ++ThisSurf ) {
			auto & surface( Surface( ThisSurf ) );

			if ( surface.Class == SurfaceClass_IntMass ) continue;

			auto const & vertex( surface.Vertex );

			{ auto const SELECT_CASE_var( surface.Sides ); //is this a 3- or 4-sided surface

			if ( SELECT_CASE_var == 3 ) { //3-sided polygon

				centroid = cen( vertex( 1 ), vertex( 2 ), vertex( 3 ) );

			} else if ( SELECT_CASE_var == 4 ) { //4-sided polygon

				// split into 2 3-sided polygons (Triangle 1 and Triangle 2)
				Triangle1( 1 ) = vertex( 1 );
				Triangle1( 2 ) = vertex( 2 );
				Triangle1( 3 ) = vertex( 3 );
				Triangle2( 1 ) = vertex( 1 );
				Triangle2( 2 ) = vertex( 3 );
				Triangle2( 3 ) = vertex( 4 );

				// get total Area of quad.
				Real64 const TotalArea( surface.GrossArea );
				if ( TotalArea <= 0.0 ) {
					//catch a problem....
					ShowWarningError( "CalcSurfaceCentroid: zero area surface, for surface=" + surface.Name );
					continue;
				}

				// get area fraction of triangles.
				Real64 const Tri1Area( AreaPolygon( 3, Triangle1 ) / TotalArea );
				Real64 const Tri2Area( AreaPolygon( 3, Triangle2 ) / TotalArea );

				// get centroid of Triangle 1
				Vector cen1( cen( Triangle1( 1 ), Triangle1( 2 ), Triangle1( 3 ) ) );

				// get centroid of Triangle 2
				Vector cen2( cen( Triangle2( 1 ), Triangle2( 2 ), Triangle2( 3 ) ) );

				// find area weighted combination of the two centroids (coded to avoid temporary Vectors)
				cen1 *= Tri1Area;
				cen2 *= Tri2Area;
				centroid = cen1;
				centroid += cen2;

			} else if ( ( SELECT_CASE_var >= 5 ) ) { //multi-sided polygon
				// (Maybe triangulate?  For now, use old "z" average method")
				// and X and Y -- straight average

				//        X1=MINVAL(Surface(ThisSurf)%Vertex(1:Surface(ThisSurf)%Sides)%x)
				//        X2=MAXVAL(Surface(ThisSurf)%Vertex(1:Surface(ThisSurf)%Sides)%x)
				//        Y1=MINVAL(Surface(ThisSurf)%Vertex(1:Surface(ThisSurf)%Sides)%y)
				//        Y2=MAXVAL(Surface(ThisSurf)%Vertex(1:Surface(ThisSurf)%Sides)%y)
				//        Z1=MINVAL(Surface(ThisSurf)%Vertex(1:Surface(ThisSurf)%Sides)%z)
				//        Z2=MAXVAL(Surface(ThisSurf)%Vertex(1:Surface(ThisSurf)%Sides)%z)
				//        Xcm=(X1+X2)/2.0d0
				//        Ycm=(Y1+Y2)/2.0d0
				//        Zcm=(Z1+Z2)/2.0d0

				// Calc centroid as average of surfaces
				centroid = 0.0;
				for ( int vert = 1; vert <= surface.Sides; ++vert ) {
					centroid += vertex( vert );
				}
				centroid /= double( surface.Sides );

			} else {

				if ( ! surface.Name.empty() ) {
					ShowWarningError( "CalcSurfaceCentroid: caught problem with # of sides, for surface=" + surface.Name );
					ShowContinueError( "... number of sides must be >= 3, this surface # sides=" + RoundSigDigits( surface.Sides ) );
				} else {
					ShowWarningError( "CalcSurfaceCentroid: caught problem with # of sides, for surface=#" + RoundSigDigits( ThisSurf ) );
					ShowContinueError( "...surface name is blank. Examine surfaces -- this may be a problem with ill-formed interzone surfaces." );
					ShowContinueError( "... number of sides must be >= 3, this surface # sides=" + RoundSigDigits( surface.Sides ) );
				}
				centroid = 0.0;

			}}

			// store result in the surface structure in DataSurfaces
			surface.Centroid = centroid;

			if ( centroid.z < 0.0 ) {
				if ( surface.ExtWind || surface.ExtBoundCond == ExternalEnvironment ) ++negZcount;
			}

		} //loop through surfaces

		if ( negZcount > 0 ) {
			ShowWarningError( "CalcSurfaceCentroid: " + RoundSigDigits( negZcount ) + " Surfaces have the Z coordinate < 0." );
			ShowContinueError( "...in any calculations, Wind Speed will be 0.0 for these surfaces." );
			ShowContinueError( "...in any calculations, Outside temperatures will be the outside temperature + " + RoundSigDigits( WeatherFileTempModCoeff, 3 ) + " for these surfaces." );
			ShowContinueError( "...that is, these surfaces will have conditions as though at ground level." );
		}

	}

	void
	SetupShadeSurfacesForSolarCalcs()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   Dec. 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// determine if Shading surfaces need full solar calcs because they
		// are also used to define geometry of an active solar component.
		// Normally, a shading surface is not included in calculations of incident solar, only shading

		// METHODOLOGY EMPLOYED:
		// Mine solar renewables input and collect surface names.
		// find shading surfaces with names that match those in solar objects.
		// setup flags for shading surfaces so that the solar renewables can resuse incident solar calcs
		// new solar component models that use shading surfaces will have to extend the code here.

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using InputProcessor::SameString;
		using namespace DataIPShortCuts;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Array1D_string TmpCandidateSurfaceNames;
		Array1D_string TmpCandidateICSSurfaceNames;
		Array1D_string TmpCandidateICSBCTypeNames;
		int NumCandidateNames;
		int NumOfCollectors;
		int NumOfICSUnits;
		int NumOfFlatPlateUnits;
		int NumPVTs;
		int NumPVs;
		int SurfNum;
		int Found;
		int CollectorNum;
		int PVTnum;
		int PVnum;
		int NumAlphas; // Number of alpha names being passed
		int NumNumbers; // Number of numeric parameters being passed
		int IOStatus;

		//First collect names of surfaces referenced by active solar components
		cCurrentModuleObject = "SolarCollector:FlatPlate:Water";
		NumOfFlatPlateUnits = GetNumObjectsFound( cCurrentModuleObject );
		cCurrentModuleObject = "SolarCollector:FlatPlate:PhotovoltaicThermal";
		NumPVTs = GetNumObjectsFound( cCurrentModuleObject );
		cCurrentModuleObject = "Generator:Photovoltaic";
		NumPVs = GetNumObjectsFound( cCurrentModuleObject );
		cCurrentModuleObject = "SolarCollector:IntegralCollectorStorage";
		NumOfICSUnits = GetNumObjectsFound( cCurrentModuleObject );

		NumCandidateNames = NumOfFlatPlateUnits + NumPVTs + NumPVs + NumOfICSUnits;
		NumOfCollectors = NumOfFlatPlateUnits + NumOfICSUnits;

		TmpCandidateSurfaceNames.allocate( NumCandidateNames );
		TmpCandidateICSSurfaceNames.allocate( NumOfCollectors );
		TmpCandidateICSBCTypeNames.allocate( NumOfCollectors );

		if ( NumOfCollectors > 0 ) {
			cCurrentModuleObject = "SolarCollector:FlatPlate:Water";
			for ( CollectorNum = 1; CollectorNum <= NumOfFlatPlateUnits; ++CollectorNum ) {

				GetObjectItem( cCurrentModuleObject, CollectorNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus );

				TmpCandidateSurfaceNames( CollectorNum ) = cAlphaArgs( 3 );
				TmpCandidateICSBCTypeNames( CollectorNum ) = "";
			}
		}

		if ( NumPVTs > 0 ) {
			cCurrentModuleObject = "SolarCollector:FlatPlate:PhotovoltaicThermal";
			for ( PVTnum = 1; PVTnum <= NumPVTs; ++PVTnum ) {

				GetObjectItem( cCurrentModuleObject, PVTnum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus );

				TmpCandidateSurfaceNames( NumOfFlatPlateUnits + PVTnum ) = cAlphaArgs( 2 );
			}
		}

		if ( NumPVs > 0 ) {
			cCurrentModuleObject = "Generator:Photovoltaic";
			for ( PVnum = 1; PVnum <= NumPVs; ++PVnum ) {
				GetObjectItem( cCurrentModuleObject, PVnum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus );
				TmpCandidateSurfaceNames( NumOfFlatPlateUnits + NumPVTs + PVnum ) = cAlphaArgs( 2 );
			}
		}

		if ( NumOfICSUnits > 0 ) {
			cCurrentModuleObject = "SolarCollector:IntegralCollectorStorage";
			for ( CollectorNum = 1; CollectorNum <= NumOfICSUnits; ++CollectorNum ) {
				GetObjectItem( cCurrentModuleObject, CollectorNum, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus );
				TmpCandidateSurfaceNames( NumOfFlatPlateUnits + NumPVTs + NumPVs + CollectorNum ) = cAlphaArgs( 3 );
				TmpCandidateICSSurfaceNames( NumOfFlatPlateUnits + CollectorNum ) = cAlphaArgs( 3 );
				TmpCandidateICSBCTypeNames( NumOfFlatPlateUnits + CollectorNum ) = cAlphaArgs( 4 );
			}
		}

		// loop through all the surfaces
		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {

			Found = FindItemInList( Surface( SurfNum ).Name, TmpCandidateSurfaceNames, NumCandidateNames );
			if ( Found > 0 ) {
				if ( ! Surface( SurfNum ).HeatTransSurf ) { // not BIPV, must be a shading surf with solar device
					// Setup missing values to allow shading surfaces to model incident solar and wind
					Surface( SurfNum ).ExtSolar = true;
					Surface( SurfNum ).ExtWind = true;
					Surface( SurfNum ).ViewFactorGround = 0.5 * ( 1.0 - Surface( SurfNum ).CosTilt );

				}
				// check if this surface is used for ICS collector mounting and has OthersideCondictionsModel as its
				// boundary condition
				if ( NumOfICSUnits > 0 ) {
					for ( CollectorNum = 1; CollectorNum <= NumOfCollectors; ++CollectorNum ) {
						if ( SameString( Surface( SurfNum ).Name, TmpCandidateICSSurfaceNames( CollectorNum ) ) && SameString( TmpCandidateICSBCTypeNames( CollectorNum ), "OTHERSIDECONDITIONSMODEL" ) ) {
							Surface( SurfNum ).IsICS = true;
							Surface( SurfNum ).ICSPtr = CollectorNum;
						}

					}
				}

			} // end of IF (Found > 0) Then

		}

	}

	void
	CheckConvexity(
		int const SurfNum, // Current surface number
		int const NSides // Number of sides to figure
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Tyler Hoyt
		//       DATE WRITTEN   December 2010
		//       MODIFIED       CR8752 - incorrect note of non-convex polygons
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE: This subroutine verifies the convexity of a
		// surface that is exposed to the sun in the case that full shading calculations
		// are required. The calculation conveniently detects collinear points as well,
		// and returns a list of indices that are collinear within the plane of the surface.

		// METHODOLOGY EMPLOYED: First the surface is determined to have dimension 2 in
		// either the xy, yz, or xz plane. That plane is selected to do the testing.
		// Vectors representing the edges of the polygon and the perpendicular dot product
		// between adjacent edges are computed. This allows the turning angle to be determined.
		// If the turning angle is greater than pi/2, it turns to the right, and if it is
		// less than pi/2, it turns left. The direction of the turn is stored, and if it
		// changes as the edges are iterated the surface is not convex. Meanwhile it stores
		// the indices of vertices that are collinear and are later removed.

		// REFERENCES:
		// http://mathworld.wolfram.com/ConvexPolygon.html

		// Using/Aliasing
		using General::RoundSigDigits;
		using namespace DataErrorTracking;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const TurnThreshold( 0.000001 ); // Sensitivity of convexity test, in radians
		static gio::Fmt ErrFmt( "(' (',F8.3,',',F8.3,',',F8.3,')')" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int n; // Loop index
		int Np1; // Loop index
		int Np2; // Loop index
		Real64 Det; // Determinant for picking projection plane
		Real64 DotProd; // Dot product for determining angle
		Real64 Theta; // Angle between edge vectors
		Real64 LastTheta; // Angle between edge vectors
		Real64 V1len; // Edge vector length
		Real64 V2len; // Edge vector length
		bool SignFlag; // Direction of edge turn : true is right, false is left
		bool PrevSignFlag( false ); // Container for the sign of the previous iteration's edge turn
		static Array1D< Real64 > X; // containers for x,y,z vertices of the surface
		static Array1D< Real64 > Y;
		static Array1D< Real64 > Z;
		static Array1D< Real64 > A; // containers for convexity test
		static Array1D< Real64 > B;
		static Array1D_int SurfCollinearVerts; // Array containing indices of collinear vertices
		static int VertSize; // size of X,Y,Z,A,B arrays
		Real64 cosarg;
		int M; // Array index for SurfCollinearVerts container
		int J; // Loop index
		int K; // Loop index
		int Ind; // Location of surface vertex to be removed
		static bool firstTime( true );
		static Real64 ACosZero; // set on firstTime
		bool SurfCollinearWarning;
		static std::string ErrLineOut; // Character string for producing error messages

		if ( firstTime ) {
			ACosZero = std::acos( 0.0 );
			X.allocate( MaxVerticesPerSurface + 2 );
			Y.allocate( MaxVerticesPerSurface + 2 );
			Z.allocate( MaxVerticesPerSurface + 2 );
			A.allocate( MaxVerticesPerSurface + 2 );
			B.allocate( MaxVerticesPerSurface + 2 );
			SurfCollinearVerts.allocate( MaxVerticesPerSurface );
			VertSize = MaxVerticesPerSurface;
			firstTime = false;
		}

		if ( NSides > VertSize ) {
			X.deallocate();
			Y.deallocate();
			Z.deallocate();
			A.deallocate();
			B.deallocate();
			SurfCollinearVerts.deallocate();
			X.allocate( MaxVerticesPerSurface + 2 );
			Y.allocate( MaxVerticesPerSurface + 2 );
			Z.allocate( MaxVerticesPerSurface + 2 );
			A.allocate( MaxVerticesPerSurface + 2 );
			B.allocate( MaxVerticesPerSurface + 2 );
			SurfCollinearVerts.allocate( MaxVerticesPerSurface );
			VertSize = MaxVerticesPerSurface;
		}

		for ( n = 1; n <= NSides; ++n ) {
			X( n ) = SurfaceTmp( SurfNum ).Vertex( n ).x;
			Y( n ) = SurfaceTmp( SurfNum ).Vertex( n ).y;
			Z( n ) = SurfaceTmp( SurfNum ).Vertex( n ).z;
		}
		X( NSides + 1 ) = SurfaceTmp( SurfNum ).Vertex( 1 ).x;
		Y( NSides + 1 ) = SurfaceTmp( SurfNum ).Vertex( 1 ).y;
		Z( NSides + 1 ) = SurfaceTmp( SurfNum ).Vertex( 1 ).z;
		X( NSides + 2 ) = SurfaceTmp( SurfNum ).Vertex( 2 ).x;
		Y( NSides + 2 ) = SurfaceTmp( SurfNum ).Vertex( 2 ).y;
		Z( NSides + 2 ) = SurfaceTmp( SurfNum ).Vertex( 2 ).z;

		// Determine a suitable plane in which to do the tests
		Det = 0.0;
		for ( n = 1; n <= NSides; ++n ) {
			Det += X( n ) * Y( n + 1 ) - X( n + 1 ) * Y( n );
		}
		if ( std::abs( Det ) > 1.e-4 ) {
			A = X;
			B = Y;
		} else {
			Det = 0.0;
			for ( n = 1; n <= NSides; ++n ) {
				Det += X( n ) * Z( n + 1 ) - X( n + 1 ) * Z( n );
			}
			if ( std::abs( Det ) > 1.e-4 ) {
				A = X;
				B = Z;
			} else {
				Det = 0.0;
				for ( n = 1; n <= NSides; ++n ) {
					Det += Y( n ) * Z( n + 1 ) - Y( n + 1 ) * Z( n );
				}
				if ( std::abs( Det ) > 1.e-4 ) {
					A = Y;
					B = Z;
				} else {
					// This condition should not be reached if the surfaces are guaranteed to be planar already
					ShowSevereError( "CheckConvexity: Surface=\"" + SurfaceTmp( SurfNum ).Name + "\" is non-planar." );
					ShowContinueError( "Coincident Vertices will be removed as possible." );
					for ( n = 1; n <= SurfaceTmp( SurfNum ).Sides; ++n ) {
						auto const & point( SurfaceTmp( SurfNum ).Vertex( n ) );
						gio::write( ErrLineOut, ErrFmt ) << point.x << point.y << point.z;
						ShowContinueError( ErrLineOut );
					}
				}
			}
		}

		M = 0;
		SurfCollinearWarning = false;
		for ( n = 1; n <= NSides; ++n ) { // perform convexity test in the plane determined above.
			V1len = std::sqrt( pow_2( A( n + 1 ) - A( n ) ) + pow_2( B( n + 1 ) - B( n ) ) );
			V2len = std::sqrt( pow_2( A( n + 2 ) - A( n + 1 ) ) + pow_2( B( n + 2 ) - B( n + 1 ) ) );
			if ( V1len <= 1.e-8 || V2len <= 1.e-8 ) continue;
			DotProd = ( A( n + 1 ) - A( n ) ) * ( B( n + 2 ) - B( n + 1 ) ) - ( B( n + 1 ) - B( n ) ) * ( A( n + 2 ) - A( n + 1 ) );
			cosarg = DotProd / ( V1len * V2len );
			if ( cosarg < -1.0 ) {
				cosarg = -1.0;
			} else if ( cosarg > 1.0 ) {
				cosarg = 1.0;
			}
			Theta = std::acos( cosarg );
			if ( Theta < ( ACosZero - TurnThreshold ) ) {
				SignFlag = true;
			} else {
				if ( Theta > ( ACosZero + TurnThreshold ) ) {
					SignFlag = false;
				} else { // Store the index of the collinear vertex for removal
					if ( ! SurfCollinearWarning ) {
						if ( DisplayExtraWarnings ) {
							ShowWarningError( "CheckConvexity: Surface=\"" + SurfaceTmp( SurfNum ).Name + "\", Collinear points have been removed." );
						}
						SurfCollinearWarning = true;
					}
					++TotalCoincidentVertices;
					++M;
					SurfCollinearVerts( M ) = n + 1;
					continue;
				}
			}

			if ( n == 1 ) {
				PrevSignFlag = SignFlag;
				LastTheta = Theta;
				continue;
			}

			if ( SignFlag != PrevSignFlag ) {
				if ( SolarDistribution != MinimalShadowing && SurfaceTmp( SurfNum ).ExtSolar ) {
					if ( DisplayExtraWarnings ) {
						ShowWarningError( "CheckConvexity: Zone=\"" + Zone( SurfaceTmp( SurfNum ).Zone ).Name + "\", Surface=\"" + SurfaceTmp( SurfNum ).Name + "\" is non-convex." );
						Np1 = n + 1;
						if ( Np1 > NSides ) Np1 -= NSides;
						Np2 = n + 2;
						if ( Np2 > NSides ) Np2 -= NSides;
						ShowContinueError( "...vertex " + RoundSigDigits( n ) + " to vertex " + RoundSigDigits( Np1 ) + " to vertex " + RoundSigDigits( Np2 ) );
						ShowContinueError( "...vertex " + RoundSigDigits( n ) + "=[" + RoundSigDigits( X( n ), 2 ) + ',' + RoundSigDigits( Y( n ), 2 ) + ',' + RoundSigDigits( Z( n ), 2 ) + ']' );
						ShowContinueError( "...vertex " + RoundSigDigits( Np1 ) + "=[" + RoundSigDigits( X( n + 1 ), 2 ) + ',' + RoundSigDigits( Y( n + 1 ), 2 ) + ',' + RoundSigDigits( Z( n + 1 ), 2 ) + ']' );
						ShowContinueError( "...vertex " + RoundSigDigits( Np2 ) + "=[" + RoundSigDigits( X( n + 2 ), 2 ) + ',' + RoundSigDigits( Y( n + 2 ), 2 ) + ',' + RoundSigDigits( Z( n + 2 ), 2 ) + ']' );
						//          CALL ShowContinueError('...theta angle=['//TRIM(RoundSigDigits(Theta,6))//']')
						//          CALL ShowContinueError('...last theta angle=['//TRIM(RoundSigDigits(LastTheta,6))//']')
					}
				}
				SurfaceTmp( SurfNum ).IsConvex = false;
				break;
			}
			PrevSignFlag = SignFlag;
			LastTheta = Theta;
		}

		// must check to make sure don't remove NSides below 3
		if ( M > 0 ) { // Remove the collinear points determined above
			if ( NSides - M > 2 ) {
				SurfaceTmp( SurfNum ).Sides = NSides - M;
			} else { // too many
				if ( DisplayExtraWarnings ) {
					ShowWarningError( "CheckConvexity: Surface=\"" + SurfaceTmp( SurfNum ).Name + "\" has [" + RoundSigDigits( M ) + "] collinear points." );
					ShowContinueError( "...too many to remove all.  Will leave the surface with 3 sides. But this is now a degenerate surface" );
				}
				++TotalDegenerateSurfaces;
				SurfaceTmp( SurfNum ).Sides = max( NSides - M, 3 );
				M = NSides - SurfaceTmp( SurfNum ).Sides;
			}
			for ( J = 1; J <= M; ++J ) {
				Ind = SurfCollinearVerts( J );
				for ( K = Ind; K <= NSides - J; ++K ) {
					SurfaceTmp( SurfNum ).Vertex( K - J + 1 ).x = SurfaceTmp( SurfNum ).Vertex( K - J + 2 ).x;
					SurfaceTmp( SurfNum ).Vertex( K - J + 1 ).y = SurfaceTmp( SurfNum ).Vertex( K - J + 2 ).y;
					SurfaceTmp( SurfNum ).Vertex( K - J + 1 ).z = SurfaceTmp( SurfNum ).Vertex( K - J + 2 ).z;
				}
			}
		}

	}

	bool
	isRectangle(
		int const ThisSurf // Surface number
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         M.J. Witte
		//       DATE WRITTEN   October 2015

		// PURPOSE: Check if a 4-sided surface is a rectangle

		using namespace Vectors;

		Real64 Diagonal1; // Length of diagonal of 4-sided figure from vertex 1 to vertex 3 (m)
		Real64 Diagonal2; // Length of diagonal of 4-sided figure from vertex 2 to vertex 4 (m)
		Real64 DotProd; // Dot product of two adjacent sides - to test for right angle
		Real64 const cos89deg = std::cos( 89.0 * DegToRadians ); // tolerance for right angle
		Vector Vect32; // normalized vector from vertex 3 to vertex 2
		Vector Vect21; // normalized vector from vertex 2 to vertex 1

		Diagonal1 = VecLength( Surface( ThisSurf ).Vertex( 1 ) - Surface( ThisSurf ).Vertex( 3 ) );
		Diagonal2 = VecLength( Surface( ThisSurf ).Vertex( 2 ) - Surface( ThisSurf ).Vertex( 4 ) );
		// Test for rectangularity
		if ( std::abs( Diagonal1 - Diagonal2 ) < 0.020 ) { // This tolerance based on coincident vertex tolerance of 0.01
			Vect32 = VecNormalize( Surface( ThisSurf ).Vertex( 3 ) - Surface( ThisSurf ).Vertex( 2 ) );
			Vect21 = VecNormalize( Surface( ThisSurf ).Vertex( 2 ) - Surface( ThisSurf ).Vertex( 1 ) );
			DotProd = dot( Vect32, Vect21 );
			if ( abs( DotProd ) <= cos89deg ) {
				return true;
			} else {
				return false;
			}
		} else {
			return false;
		}

	}

} // SurfaceGeometry

} // EnergyPlus
