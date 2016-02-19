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
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/member.functions.hh>
#include <ObjexxFCL/random.hh>
#include <ObjexxFCL/string.functions.hh>
#include <ObjexxFCL/Vector2.hh>
#include <ObjexxFCL/Vector3.hh>
#include <ObjexxFCL/Vector4.hh>

// EnergyPlus Headers
#include <DaylightingManager.hh>
#include <CommandLineInterface.hh>
#include <DataBSDFWindow.hh>
#include <DataDaylighting.hh>
#include <DataDaylightingDevices.hh>
#include <DataEnvironment.hh>
#include <DataErrorTracking.hh>
#include <DataGlobals.hh>
#include <DataHeatBalance.hh>
#include <DataIPShortCuts.hh>
#include <DataPrecisionGlobals.hh>
#include <DataStringGlobals.hh>
#include <DataSurfaces.hh>
#include <DataSystemVariables.hh>
#include <DaylightingDevices.hh>
#include <DElightManagerF.hh>
#include <DisplayRoutines.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <InternalHeatGains.hh>
#include <OutputProcessor.hh>
#include <OutputReportPredefined.hh>
#include <PierceSurface.hh>
#include <ScheduleManager.hh>
#include <SolarReflectionManager.hh>
#include <SQLiteProcedures.hh>
#include <SurfaceOctree.hh>
#include <UtilityRoutines.hh>
#include <Vectors.hh>
#include <WindowComplexManager.hh>

namespace EnergyPlus {

namespace DaylightingManager {

	// MODULE INFORMATION
	//       AUTHOR         Fred Winkelmann
	//       DATE WRITTEN   July 1997, December 1998
	//       MODIFIED       Oct 2004; LKL -- Efficiencies and code restructure
	//                      Aug 2012: BG -- Added availability schedule
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// Manages the daylighting calculations for each thermal zone that has an associated
	// Daylighting:Controls object.

	// Includes calculation of interior daylight illuminance and glare
	// from each of the windows in a zone, control of window shading devices
	// to reduce glare, and control of overhead electric lighting in response
	// to interior daylight illuminance level at one or two user-specified
	// reference points at which sensors are located.

	// METHODOLOGY EMPLOYED:
	// REFERENCES:
	// "Daylighting Calculation in DOE-2," F.C.Winkelmann, LBL-11353, May 1983
	// "Daylighting Simulation in the DOE-2 Building Energy Analysis Program,"
	// F.C. Winkelmann and S. Selkowitz, Energy and Buildings 8(1985)271-286

	// OTHER NOTES:
	// This module was created from DOE-2.1E subroutines.

	// Correspondence between DOE-2.1E and EnergyPlus subroutine names:

	// DOE-2.1E    EnergyPlus                      In Module           Called from Module
	// DAVREF      DayltgAveInteriorReflectance    DaylightingManager DaylightingManager
	// DCOF        CalcDayltgCoefficients          DaylightingManager DaylightingManager
	// DCROSS      DayltgCrossProduct              DaylightingManager DaylightingManager
	// DEXTIL      DayltgCurrentExtHorizIllum      WeatherManager     WeatherManager
	// DGLARE      DayltgGlare                     DaylightingManager DaylightingManager
	// DHILL       DayltgExtHorizIllum             DaylightingManager DaylightingManager
	// DHITSH      DayltgHitObstruction            DaylightingManager DaylightingManager
	// DINTIL      DayltgInteriorIllum             DaylightingManager HeatBalanceSurfaceManager
	// DLTSYS      DayltgElecLightingControl       DaylightingManager HeatBalanceSurfaceManager
	// DNSOL       not used
	// DPFAC       DayltgPositionFactor            DaylightingManager DaylightingManager
	// DPIERC      PierceSurface                   PierceSurface      DaylightingManager
	// DREFLT      DayltgInterReflectedIllum       DaylightingManager DaylightingManager
	// DSKYLU      DayltgSkyLuminance              DaylightingManager DaylightingManager
	// DTHLIM      DayltgAzimuthLimits             DaylightingManager DaylightingManager
	// DLUMEF      DayltgLuminousEfficacy          WeatherManager     WeatherManager

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataGlobals;
	using namespace DataHeatBalance;
	using namespace DataSurfaces;
	using namespace DataEnvironment;
	using namespace DataDaylighting;
	using namespace DataDaylightingDevices;
	using DataBSDFWindow::BSDFDaylghtPosition;
	using DataBSDFWindow::ComplexWind;

	using namespace ScheduleManager;
	//USE Vectors

	// Data
	// MODULE PARAMETER DEFINITIONS:
	static std::string const BlankString;

	// Surface count crossover for using octree algorithm
	// The octree gives lower computational complexity for much higher performance
	//  as the surface count increases but has some overhead such that the direct
	//  algorithm can be more efficient at small surface counts.
	// Testing to date shows that the octree performance is close to that of the
	//  direct algorithm even with small surface counts and that there is no single
	//  crossover that is ideal for all models: some cases with 10-30 surfaces were
	//  faster with the octree but another with 80 surfaces was faster with the
	//  direct algorithm.
	// A reasonable, conservative crossover is selected but may be refined as more
	//  experience is gained.
	int const octreeCrossover( 100 ); // Octree surface count crossover

	// MODULE VARIABLE DECLARATIONS:
	int TotWindowsWithDayl( 0 ); // Total number of exterior windows in all daylit zones
	int OutputFileDFS( 0 ); // Unit number for daylight factors
	Array1D< Real64 > DaylIllum( MaxRefPoints, 0.0 ); // Daylight illuminance at reference points (lux)
	Real64 PHSUN( 0.0 ); // Solar altitude (radians)
	Real64 SPHSUN( 0.0 ); // Sine of solar altitude
	Real64 CPHSUN( 0.0 ); // Cosine of solar altitude
	Real64 THSUN( 0.0 ); // Solar azimuth (rad) in Absolute Coordinate System (azimuth=0 along east)
	Array1D< Real64 > PHSUNHR( 24, 0.0 ); // Hourly values of PHSUN
	Array1D< Real64 > SPHSUNHR( 24, 0.0 ); // Hourly values of the sine of PHSUN
	Array1D< Real64 > CPHSUNHR( 24, 0.0 ); // Hourly values of the cosine of PHSUN
	Array1D< Real64 > THSUNHR( 24, 0.0 ); // Hourly values of THSUN

	// In the following I,J,K arrays:
	// I = 1 for clear sky, 2 for clear turbid, 3 for intermediate, 4 for overcast;
	// J = 1 for bare window, 2 - 12 for shaded;
	// K = sun position index.
	Array3D< Real64 > EINTSK( 24, MaxSlatAngs+1, 4, 0.0 ); // Sky-related portion of internally reflected illuminance
	Array2D< Real64 > EINTSU( 24, MaxSlatAngs+1, 0.0 ); // Sun-related portion of internally reflected illuminance,
	// excluding entering beam
	Array2D< Real64 > EINTSUdisk( 24, MaxSlatAngs+1, 0.0 ); // Sun-related portion of internally reflected illuminance
	// due to entering beam
	Array3D< Real64 > WLUMSK( 24, MaxSlatAngs+1, 4, 0.0 ); // Sky-related window luminance
	Array2D< Real64 > WLUMSU( 24, MaxSlatAngs+1, 0.0 ); // Sun-related window luminance, excluding view of solar disk
	Array2D< Real64 > WLUMSUdisk( 24, MaxSlatAngs+1, 0.0 ); // Sun-related window luminance, due to view of solar disk

	Array2D< Real64 > GILSK( 24, 4, 0.0 ); // Horizontal illuminance from sky, by sky type, for each hour of the day
	Array1D< Real64 > GILSU( 24, 0.0 ); // Horizontal illuminance from sun for each hour of the day

	Array3D< Real64 > EDIRSK( 24, MaxSlatAngs+1, 4 ); // Sky-related component of direct illuminance
	Array2D< Real64 > EDIRSU( 24, MaxSlatAngs+1 ); // Sun-related component of direct illuminance (excluding beam solar at ref pt)
	Array2D< Real64 > EDIRSUdisk( 24, MaxSlatAngs+1 ); // Sun-related component of direct illuminance due to beam solar at ref pt
	Array3D< Real64 > AVWLSK( 24, MaxSlatAngs+1, 4 ); // Sky-related average window luminance
	Array2D< Real64 > AVWLSU( 24, MaxSlatAngs+1 ); // Sun-related average window luminance, excluding view of solar disk
	Array2D< Real64 > AVWLSUdisk( 24, MaxSlatAngs+1 ); // Sun-related average window luminance due to view of solar disk

	// Allocatable daylight factor arrays  -- are in the ZoneDaylight Structure

	Array2D< Real64 > TDDTransVisBeam;
	Array3D< Real64 > TDDFluxInc;
	Array3D< Real64 > TDDFluxTrans;

	Array2D_int MapErrIndex;
	Array2D_int RefErrIndex;

	Array1D_bool CheckTDDZone;

	std::string mapLine; // character variable to hold map outputs

	// SUBROUTINE SPECIFICATIONS FOR MODULE DaylightingModule

	// MODULE SUBROUTINES:

	// Functions

	void
	DayltgAveInteriorReflectance( int & ZoneNum ) // Zone number
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   July 1997
		//       MODIFIED       Mar 2004, FCW: add calculation of following SurfaceWindow variables:
		//                        ZoneAreaMinusThisSurf, ZoneAreaReflProdMinusThisSurf, RhoCeilingWall,
		//                        RhoFloorWall, FractionUpgoing. Add calculation of ZoneDaylight%FloorVisRefl.
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Called by CalcDayltgCoefficients for each daylit zone. Determines total
		// area and area-weighted average visible reflectance of
		// all inside faces of the surfaces of a zone.  In addition, finds
		// area and average reflectance of interzone, underground and exterior
		// heat-transfer surfaces in the following categories: floor (tilt > 170 deg),
		// ceiling (tilt < 10 deg), and wall (10 < tilt < 170 deg).
		// The window reflectance values used here assume the windows have no shading
		// devices. This information is used in the calculation of the
		// internally-reflected daylighting component.

		// Finds total number of exterior windows in the space.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// Based on DOE-2.1E subroutine DAVREF

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int IType; // Surface type/class
		Real64 AREA; // Inside surface area (m2)
		Real64 AInsTot; // Total inside surface area of a zone (m2)
		Real64 ARHTOT; // Sum over surfaces of AREA*(inside visible reflectance) (m2)
		int ISurf; // Surface number
		int IWin; // Window number
		int ITILT; // Surface tilt category (1 = floor, 2 = wall, 3 = ceiling)
		int IT; // Tilt index
		static Vector3< Real64 > AR; // Inside surface area sum for floor/wall/ceiling (m2)
		static Vector3< Real64 > ARH; // Inside surface area*reflectance sum for floor/wall/ceiling (m2)
		static Vector3< Real64 > AP; // Zone inside surface floor/wall/ceiling area without a selected
		//  floor/wall/ceiling (m2)
		static Vector3< Real64 > ARHP; // Zone inside surface floor/wall/ceiling area*reflectance without
		//  a selected floor/wall/ceiling (m2)
		Real64 ATWL; // Opaque surface area (m2)
		Real64 ARHTWL; // ATWL times inside visible reflectance of surface (m2)
		int IWinDr; // Window/door surface number
		Real64 ETA; // Ratio of floor-to-window-center height and average floor-to-ceiling height

		// FLOW:

		// Total inside surface area, including windows
		AInsTot = 0.0;
		// Sum of products of inside surface area * vis reflectance
		ARHTOT = 0.0;
		// Area sum and area * reflectance sum for different orientations
		AR = 0.0;
		ARH = 0.0;
		// Loop over surfaces
		for ( ISurf = Zone( ZoneNum ).SurfaceFirst; ISurf <= Zone( ZoneNum ).SurfaceLast; ++ISurf ) {
			IType = Surface( ISurf ).Class;
			// Error if window has multiplier > 1 since this causes incorrect illuminance calc
			if ( IType == SurfaceClass_Window && Surface( ISurf ).Multiplier > 1.0 ) {
				if ( ZoneDaylight( ZoneNum ).TotalDaylRefPoints > 0 ) {
					ShowSevereError( "DayltgAveInteriorReflectance: Multiplier > 1.0 for window " + Surface( ISurf ).Name + " in Zone=" + Surface( ISurf ).ZoneName );
					ShowContinueError( "...not allowed since it is in a zone with daylighting." );
					ShowFatalError( "Progrem terminates due to preceding conditions." );
				} else {
					ShowSevereError( "DayltgAveInteriorReflectance: Multiplier > 1.0 for window " + Surface( ISurf ).Name + " in Zone=" + Surface( ISurf ).ZoneName );
					ShowContinueError( "...an adjacent Zone has daylighting. Simulation cannot proceed." );
					ShowFatalError( "Progrem terminates due to preceding conditions." );
				}
			}
			if ( IType == SurfaceClass_Wall || IType == SurfaceClass_Floor || IType == SurfaceClass_Roof || IType == SurfaceClass_Window || IType == SurfaceClass_Door ) {
				AREA = Surface( ISurf ).Area;
				// In following, FrameArea and DividerArea can be non-zero only for exterior windows
				AInsTot += AREA + SurfaceWindow( ISurf ).FrameArea * ( 1.0 + 0.5 * SurfaceWindow( ISurf ).ProjCorrFrIn ) + SurfaceWindow( ISurf ).DividerArea * ( 1.0 + SurfaceWindow( ISurf ).ProjCorrDivIn );
				ARHTOT += AREA * Construct( Surface( ISurf ).Construction ).ReflectVisDiffBack + SurfaceWindow( ISurf ).FrameArea * ( 1.0 + 0.5 * SurfaceWindow( ISurf ).ProjCorrFrIn ) * ( 1.0 - SurfaceWindow( ISurf ).FrameSolAbsorp ) + SurfaceWindow( ISurf ).DividerArea * ( 1.0 + SurfaceWindow( ISurf ).ProjCorrDivIn ) * ( 1.0 - SurfaceWindow( ISurf ).DividerSolAbsorp );
				ITILT = 3; // Ceiling
				if ( Surface( ISurf ).Tilt > 10.0 && Surface( ISurf ).Tilt < 170.0 ) ITILT = 2; // Wall
				if ( Surface( ISurf ).Tilt >= 170.0 ) ITILT = 1; // Floor
				AR( ITILT ) += AREA + SurfaceWindow( ISurf ).FrameArea * ( 1.0 + 0.5 * SurfaceWindow( ISurf ).ProjCorrFrIn ) + SurfaceWindow( ISurf ).DividerArea * ( 1.0 + SurfaceWindow( ISurf ).ProjCorrDivIn );
				ARH( ITILT ) += AREA * Construct( Surface( ISurf ).Construction ).ReflectVisDiffBack + SurfaceWindow( ISurf ).FrameArea * ( 1.0 + 0.5 * SurfaceWindow( ISurf ).ProjCorrFrIn ) * ( 1.0 - SurfaceWindow( ISurf ).FrameSolAbsorp ) + SurfaceWindow( ISurf ).DividerArea * ( 1.0 + SurfaceWindow( ISurf ).ProjCorrDivIn ) * ( 1.0 - SurfaceWindow( ISurf ).DividerSolAbsorp );

			}
		}

		// Average inside surface reflectance of zone
		ZoneDaylight( ZoneNum ).AveVisDiffReflect = ARHTOT / AInsTot;
		// Total inside surface area of zone
		ZoneDaylight( ZoneNum ).TotInsSurfArea = AInsTot;
		// Average floor visible reflectance
		ZoneDaylight( ZoneNum ).FloorVisRefl = ARH( 3 ) / ( AR( 3 ) + 1.e-6 );

		for ( ISurf = Zone( ZoneNum ).SurfaceFirst; ISurf <= Zone( ZoneNum ).SurfaceLast; ++ISurf ) {
			IType = Surface( ISurf ).Class;
			if ( IType == SurfaceClass_Wall || IType == SurfaceClass_Floor || IType == SurfaceClass_Roof ) {
				// Remove this surface from the zone inside surface area and area*reflectivity
				// The resulting areas are AP(ITILT). The resulting area*reflectivity is ARHP(ITILT).
				// Initialize gross area of surface (including subsurfaces)
				ATWL = Surface( ISurf ).Area; // This is the surface area less subsurfaces
				// Area * reflectance for this surface, excluding attached windows and doors
				ARHTWL = Surface( ISurf ).Area * Construct( Surface( ISurf ).Construction ).ReflectVisDiffBack;
				// Tilt index
				if ( Surface( ISurf ).Tilt > 45.0 && Surface( ISurf ).Tilt < 135.0 ) {
					ITILT = 2; // Wall
				} else if ( Surface( ISurf ).Tilt >= 135.0 ) {
					ITILT = 1; // Floor
				} else {
					ITILT = 3; // Ceiling
				}
				// Loop over windows and doors on this wall
				for ( IWinDr = Zone( ZoneNum ).SurfaceFirst; IWinDr <= Zone( ZoneNum ).SurfaceLast; ++IWinDr ) {
					if ( ( Surface( IWinDr ).Class == SurfaceClass_Window || Surface( IWinDr ).Class == SurfaceClass_Door ) && Surface( IWinDr ).BaseSurf == ISurf ) {
						ATWL += Surface( IWinDr ).Area + SurfaceWindow( IWinDr ).FrameArea * ( 1.0 + 0.5 * SurfaceWindow( IWinDr ).ProjCorrFrIn ) + SurfaceWindow( IWinDr ).DividerArea * ( 1.0 + SurfaceWindow( IWinDr ).ProjCorrDivIn );
						ARHTWL += Surface( IWinDr ).Area * Construct( Surface( IWinDr ).Construction ).ReflectVisDiffBack + SurfaceWindow( IWinDr ).FrameArea * ( 1.0 + 0.5 * SurfaceWindow( IWinDr ).ProjCorrFrIn ) * ( 1.0 - SurfaceWindow( IWinDr ).FrameSolAbsorp ) + SurfaceWindow( IWinDr ).DividerArea * ( 1.0 + SurfaceWindow( IWinDr ).ProjCorrDivIn ) * ( 1.0 - SurfaceWindow( IWinDr ).DividerSolAbsorp );
					}
				}
				// Inside surface area of floor, walls and ceilings, minus surface ISurf and its subsurfaces
				for ( IT = 1; IT <= 3; ++IT ) {
					if ( IT == ITILT ) {
						AP( IT ) = AR( IT ) - ATWL;
						ARHP( IT ) = ARH( IT ) - ARHTWL;
					} else {
						AP( IT ) = AR( IT );
						ARHP( IT ) = ARH( IT );
					}
				}
				SurfaceWindow( ISurf ).ZoneAreaMinusThisSurf = AP;
				SurfaceWindow( ISurf ).ZoneAreaReflProdMinusThisSurf = ARHP;
			}
		} // End of loop over opaque surfaces in zone

		for ( IWin = Zone( ZoneNum ).SurfaceFirst; IWin <= Zone( ZoneNum ).SurfaceLast; ++IWin ) {
			if ( Surface( IWin ).Class == SurfaceClass_Window ) {
				ISurf = Surface( IWin ).BaseSurf;
				// Ratio of floor-to-window-center height and average floor-to-ceiling height
				ETA = max( 0.0, min( 1.0, ( SurfaceWindow( IWin ).WinCenter( 3 ) - Zone( ZoneNum ).OriginZ ) * Zone( ZoneNum ).FloorArea / Zone( ZoneNum ).Volume ) );
				AP = SurfaceWindow( ISurf ).ZoneAreaMinusThisSurf;
				ARHP = SurfaceWindow( ISurf ).ZoneAreaReflProdMinusThisSurf;
				// Average reflectance seen by light moving up (RhoCeilingWall) and down (RhoFloorWall)
				// across horizontal plane through center of window
				SurfaceWindow( IWin ).RhoCeilingWall = ( ARHP( 2 ) * ( 1.0 - ETA ) + ARHP( 3 ) ) / ( AP( 2 ) * ( 1.0 - ETA ) + AP( 3 ) + 1.0e-5 );
				SurfaceWindow( IWin ).RhoFloorWall = ( ARHP( 2 ) * ETA + ARHP( 1 ) ) / ( AP( 2 ) * ETA + AP( 1 ) + 1.e-9 );

				// Angle factor for windows with diffusing shades. SurfaceWindow(IWin)%FractionUpgoing is
				// fraction of light from the shade that goes up toward ceiling and upper part of walls.
				// 1 - SurfaceWindow(IWin)%FractionUpgoing is fraction that goes down toward floor and lower part of walls.
				SurfaceWindow( IWin ).FractionUpgoing = Surface( IWin ).Tilt / 180.0;

				// Daylighting shelf simplication:  All light goes up to the ceiling regardless of orientation of shelf
				if ( Surface( IWin ).Shelf > 0 ) {
					if ( Shelf( Surface( IWin ).Shelf ).InSurf > 0 ) SurfaceWindow( IWin ).FractionUpgoing = 1.0;
				}
			}
		}

	}

	void
	CalcDayltgCoefficients()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   July 1997
		//       MODIFIED       FW, Jan 2002: add variable slat angle blinds
		//                      FW, Mar 2002: add triangular windows
		//                      FW, Oct 2002: remove warning on window discretization relative to
		//                                    reference point distance to window plane
		//                      FW, Jan 2003: add between-glass shades and blinds
		//                      FW, Apr 2003: initialize shading type to 'NOSHADE' in window loop
		//                      PE, May 2003: add light pipes (tubular daylighting devices)
		//                      FW, Jul 2003: account for possible non-zero transmittance of
		//                                    shading surfaces (previously all shading surfaces were
		//                                    assumed to be opaque)
		//                      PE, Aug 2003: add daylighting shelves
		//                      FW, Sep 2003: write the bare-window overcast sky daylight factors to the eio file
		//                      FW, Nov 2003: add exterior beam and sky solar diffuse reflection from obstructions;
		//                                    add beam solar and sky solar reflection from ground with obstructions.
		//                      FW, Nov 2003: change expression for NDIVX, NDIVY (no. of window elements in X,Y) to
		//                                    round up to nearest integer rather than down
		//                      FW, Nov 2003: add specular reflection of beam solar from obstructions
		//                      RJH, Jan 2004: add alternative daylighting analysis using DElight
		//                                     All modifications demarked with RJH (Rob Hitchcock)
		//                      FW, Feb 2004: add daylighting through interior windows
		//                      FW, Apr 2004: add light well efficiency that multiplies glazing transmittance
		//                      FW, Apr 2004: add diffusing glazing
		//                      RJH, Jul 2004: add error handling for warnings/errors returned from DElight
		//                      LKL, Oct 2004: Separate "map" and "ref" point calculations -- move some input routines to
		//                                     separate routines.
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates daylighting factors for later use in the time-step loop.

		// METHODOLOGY EMPLOYED:

		// For each combination of exterior window and reference point in a zone,
		// calculates daylighting factors (interior illuminance / exterior illuminance)
		// and glare factors for clear and overcast skies and for windows with and
		// without shading devices. These factors are calculated for each hourly
		// sun position for design days and for selected days throughout the year.

		// If a target zone has one or more interior windows, also calculates daylighting
		// factors for the target zone that are associated with exterior windows in adjacent
		// zones that share interior windows with the target zone.

		// The daylight illuminance at a reference point from a window is determined
		// by dividing the window into rectangular elements and calculating the illuminance
		// reaching the reference point directly from each element. The illumination
		// from an element can come from the sky or ground if the window is unshaded, or from
		// a shading device illuminated by solar radiation. Also considered are the
		// illuminance contribution from interreflection among the zone's interior surfaces
		// and sunlight striking the reference point.

		// In calculating sky-related interior illuminance and luminance quantities,
		// the sky luminance for the different sky types are determined from distributions
		// in which the zenith luminance is normalized to 1.0 cd/m2. Similarly, sun-related
		// illuminance and luminance quantities are based on beam normal solar illuminance
		// normalized to 1.0 lux.
		// The daylight and glare factors calculated in this subroutine are used in DayltgInteriorIllum
		// to get the daylight illuminance and glare at each time step.
		// Based on this information and user-input lighting setpoint and type of lighting
		// control system, DayltgElecLightingControl then determines how much the overhead eletric lighting
		// can be reduced.

		// REFERENCES:
		// Based on DOE-2.1E subroutine DCOF.

		// Using/Aliasing
		using General::BlindBeamBeamTrans;
		using General::RoundSigDigits;
		using DaylightingDevices::FindTDDPipe;
		using DaylightingDevices::TransTDD;
		using DataSystemVariables::DetailedSolarTimestepIntegration;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static gio::Fmt fmtA( "(A)" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ZoneNum; // Zone number
		int IHR; // Hour of day counter
		int IWin; // Window counter
		int loop; // DO loop indices
		static bool firstTime( true );
		static bool FirstTimeDaylFacCalc( true );
		Real64 DaylFac1; // sky daylight factor at ref pt 1
		Real64 DaylFac2; // sky daylight factor at ref pt 2

		// added for output all daylight factors
		int write_stat;
		Real64 DFClrSky1;
		Real64 DFClrTbSky1;
		Real64 DFIntSky1;
		Real64 DFOcSky1;
		Real64 DFClrSky2;
		Real64 DFClrTbSky2;
		Real64 DFIntSky2;
		Real64 DFOcSky2;
		Real64 SlatAngle;
		int ISA;
		int ICtrl;
		int ISlatAngle;

		static bool CreateDFSReportFile( true );
		static bool doSkyReporting( true );

		// Formats
		static gio::Fmt Format_700( "('! <Sky Daylight Factors>, MonthAndDay, Zone Name, Window Name, Daylight Fac: Ref Pt #1, Daylight Fac: Ref Pt #2')" );

		// FLOW:
		if ( firstTime ) {
			GetDaylightingParametersInput();
			CheckTDDsAndLightShelvesInDaylitZones();
			firstTime = false;
			if ( allocated( CheckTDDZone ) ) CheckTDDZone.deallocate();
		} // End of check if firstTime

		// Find the total number of exterior windows associated with all Daylighting:Detailed zones.
		// An exterior window is associated with such a zone if (1) it is an exterior window in the zone, or
		// (2) it is an exterior window in an adjacent zone that shares an interior window with the zone.
		// Note that exterior windows in category (2) may be counted more than once if an adjacent zone
		// is adjacent to more than one daylit zone with which the adjacent zone shares interior windows.
		// If there are no interior windows in a building, than TotWindowsWithDayl is just the total number of
		// exterior windows in Daylighting:Detailed zones. Note that it is possible for a
		// Daylighting:Detailed zone to have zero exterior windows of its own, but it may have an interior
		// through which daylight passes from adjacent zones with exterior windows.
		if ( BeginSimFlag ) {
			TotWindowsWithDayl = 0;
			for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
				TotWindowsWithDayl += ZoneDaylight( ZoneNum ).NumOfDayltgExtWins;
			}
		}

		if ( TotWindowsWithDayl == 0 ) return;

		//-----------------------------------------!
		// Detailed daylighting factor calculation !
		//-----------------------------------------!
		if ( ! DetailedSolarTimestepIntegration && ! KickOffSizing && ! KickOffSimulation ) {
			if ( WarmupFlag ) {
				DisplayString( "Calculating Detailed Daylighting Factors, Start Date=" + CurMnDy );
			} else {
				DisplayString( "Updating Detailed Daylighting Factors, Start Date=" + CurMnDy );
			}
		}

		if ( BeginSimFlag ) {

			// Find minimum solid angle subtended by an interior window in Daylighting:Detailed zones.
			// Used in calculating daylighting through interior windows.
			CalcMinIntWinSolidAngs();

			TDDTransVisBeam.allocate( 24, NumOfTDDPipes );
			TDDFluxInc.allocate( 24, 4, NumOfTDDPipes );
			TDDFluxTrans.allocate( 24, 4, NumOfTDDPipes );

			// Warning if detailed daylighting has been requested for a zone with no associated exterior windows.
			for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
				if ( ZoneDaylight( ZoneNum ).TotalDaylRefPoints > 0 && ZoneDaylight( ZoneNum ).NumOfDayltgExtWins == 0 ) {
					ShowWarningError( "Detailed daylighting will not be done for zone=" + Zone( ZoneNum ).Name );
					ShowContinueError( "because it has no associated exterior windows." );
				}
			}

			// Find area and reflectance quantities used in calculating inter-reflected illuminance.
			for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
				//TH 9/10/2009. Need to calculate for zones without daylighting controls (TotalDaylRefPoints = 0)
				// but with adjacent zones having daylighting controls.
				if ( ( ZoneDaylight( ZoneNum ).TotalDaylRefPoints > 0 && ZoneDaylight( ZoneNum ).NumOfDayltgExtWins > 0 ) || ZoneDaylight( ZoneNum ).AdjZoneHasDayltgCtrl ) {
					DayltgAveInteriorReflectance( ZoneNum );
				}
			}

		}

		// Zero daylighting factor arrays
		if ( ! DetailedSolarTimestepIntegration ) {
			TDDTransVisBeam = 0.0;
			TDDFluxInc = 0.0;
			TDDFluxTrans = 0.0;
		} else {
			TDDTransVisBeam( HourOfDay, {1,NumOfTDDPipes} ) = 0.0;
			TDDFluxInc( HourOfDay, {1,4}, {1,NumOfTDDPipes} ) = 0.0;
			TDDFluxTrans( HourOfDay, {1,4}, {1,NumOfTDDPipes} ) = 0.0;
		}

		if ( ! DetailedSolarTimestepIntegration ) {
			if ( BeginDayFlag ) {
				// Calculate hourly sun angles, clear sky zenith luminance, and exterior horizontal illuminance
				PHSUN = 0.0;
				SPHSUN = 0.0;
				CPHSUN = 0.0;
				THSUN = 0.0;

				PHSUNHR = 0.0;
				SPHSUNHR = 0.0;
				CPHSUNHR = 0.0;
				THSUNHR = 0.0;
				GILSK = 0.0;
				GILSU = 0.0;
				for ( IHR = 1; IHR <= 24; ++IHR ) {
					if ( SUNCOSHR( IHR, 3 ) < SunIsUpValue ) continue; // Skip if sun is below horizon //Autodesk SUNCOSHR was uninitialized here
					PHSUN = PiOvr2 - std::acos( SUNCOSHR( IHR, 3 ) );
					PHSUNHR( IHR ) = PHSUN;
					SPHSUNHR( IHR ) = std::sin( PHSUN );
					CPHSUNHR( IHR ) = std::cos( PHSUN );
					THSUNHR( IHR ) = std::atan2( SUNCOSHR( IHR, 2 ), SUNCOSHR( IHR, 1 ) );
					// Get exterior horizontal illuminance from sky and sun
					THSUN = THSUNHR( IHR );
					SPHSUN = SPHSUNHR( IHR );
					CPHSUN = CPHSUNHR( IHR );
					DayltgExtHorizIllum( GILSK( IHR, 1 ), GILSU( IHR ) );
				}
			}
		} else { //timestep integrated calculations
			PHSUN = 0.0;
			SPHSUN = 0.0;
			CPHSUN = 0.0;
			THSUN = 0.0;

			PHSUNHR( HourOfDay ) = 0.0;
			SPHSUNHR( HourOfDay ) = 0.0;
			CPHSUNHR( HourOfDay ) = 0.0;
			THSUNHR( HourOfDay ) = 0.0;
			GILSK( HourOfDay, {1,4} ) = 0.0;
			GILSU( HourOfDay ) = 0.0;
			if ( ! ( SUNCOSHR( HourOfDay, 3 ) < SunIsUpValue ) ) { // Skip if sun is below horizon
				PHSUN = PiOvr2 - std::acos( SUNCOSHR( HourOfDay, 3 ) );
				PHSUNHR( HourOfDay ) = PHSUN;
				SPHSUNHR( HourOfDay ) = std::sin( PHSUN );
				CPHSUNHR( HourOfDay ) = std::cos( PHSUN );
				THSUNHR( HourOfDay ) = std::atan2( SUNCOSHR( HourOfDay, 2 ), SUNCOSHR( HourOfDay, 1 ) );
				// Get exterior horizontal illuminance from sky and sun
				THSUN = THSUNHR( HourOfDay );
				SPHSUN = SPHSUNHR( HourOfDay );
				CPHSUN = CPHSUNHR( HourOfDay );
				DayltgExtHorizIllum( GILSK( HourOfDay, 1 ), GILSU( HourOfDay ) );
			}

		}

		//           -----------
		// ---------- ZONE LOOP ----------
		//           -----------

		for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
			// Skip zones that are not Daylighting:Detailed zones.
			// TotalDaylRefPoints = 0 means zone has (1) no daylighting or
			// (3) Daylighting:DElight
			if ( ZoneDaylight( ZoneNum ).TotalDaylRefPoints == 0 ) continue;

			// Skip zones with no exterior windows in the zone or in adjacent zone with which an interior window is shared
			if ( ZoneDaylight( ZoneNum ).NumOfDayltgExtWins == 0 ) continue;

			CalcDayltgCoeffsRefMapPoints( ZoneNum );

		} // End of zone loop, ZoneNum

		if ( doSkyReporting ) {
			if ( ! KickOffSizing && ! KickOffSimulation ) {
				if ( FirstTimeDaylFacCalc && TotWindowsWithDayl > 0 ) {
					// Write the bare-window four sky daylight factors at noon time to the eio file; this is done only
					// for first time that daylight factors are calculated and so is insensitive to possible variation
					// due to change in ground reflectance from month to month, or change in storm window status.
					gio::write( OutputFileInits, Format_700 );
					for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
						if ( ZoneDaylight( ZoneNum ).NumOfDayltgExtWins == 0 ) continue;
						for ( loop = 1; loop <= ZoneDaylight( ZoneNum ).NumOfDayltgExtWins; ++loop ) {
							IWin = ZoneDaylight( ZoneNum ).DayltgExtWinSurfNums( loop );
							// For this report, do not include ext wins in zone adjacent to ZoneNum since the inter-reflected
							// component will not be calculated for these windows until the time-step loop.
							if ( Surface( IWin ).Zone == ZoneNum ) {
								// clear sky
								DaylFac1 = ZoneDaylight( ZoneNum ).DaylIllFacSky( 12, 1, 1, 1, loop );
								DaylFac2 = 0.0;
								if ( ZoneDaylight( ZoneNum ).TotalDaylRefPoints > 1 ) DaylFac2 = ZoneDaylight( ZoneNum ).DaylIllFacSky( 12, 1, 1, 2, loop );
								gio::write( OutputFileInits, fmtA ) << " Clear Sky Daylight Factors," + CurMnDy + ',' + Zone( ZoneNum ).Name + ',' + Surface( IWin ).Name + ',' + RoundSigDigits( DaylFac1, 4 ) + ',' + RoundSigDigits( DaylFac2, 4 );

								// clear Turbid sky
								DaylFac1 = ZoneDaylight( ZoneNum ).DaylIllFacSky( 12, 1, 2, 1, loop );
								DaylFac2 = 0.0;
								if ( ZoneDaylight( ZoneNum ).TotalDaylRefPoints > 1 ) DaylFac2 = ZoneDaylight( ZoneNum ).DaylIllFacSky( 12, 1, 2, 2, loop );
								gio::write( OutputFileInits, fmtA ) << " Clear Turbid Sky Daylight Factors," + CurMnDy + ',' + Zone( ZoneNum ).Name + ',' + Surface( IWin ).Name + ',' + RoundSigDigits( DaylFac1, 4 ) + ',' + RoundSigDigits( DaylFac2, 4 );

								// Intermediate sky
								DaylFac1 = ZoneDaylight( ZoneNum ).DaylIllFacSky( 12, 1, 3, 1, loop );
								DaylFac2 = 0.0;
								if ( ZoneDaylight( ZoneNum ).TotalDaylRefPoints > 1 ) DaylFac2 = ZoneDaylight( ZoneNum ).DaylIllFacSky( 12, 1, 3, 2, loop );
								gio::write( OutputFileInits, fmtA ) << " Intermediate Sky Daylight Factors," + CurMnDy + ',' + Zone( ZoneNum ).Name + ',' + Surface( IWin ).Name + ',' + RoundSigDigits( DaylFac1, 4 ) + ',' + RoundSigDigits( DaylFac2, 4 );

								// Overcast sky
								DaylFac1 = ZoneDaylight( ZoneNum ).DaylIllFacSky( 12, 1, 4, 1, loop );
								DaylFac2 = 0.0;
								if ( ZoneDaylight( ZoneNum ).TotalDaylRefPoints > 1 ) DaylFac2 = ZoneDaylight( ZoneNum ).DaylIllFacSky( 12, 1, 4, 2, loop );
								gio::write( OutputFileInits, fmtA ) << " Overcast Sky Daylight Factors," + CurMnDy + ',' + Zone( ZoneNum ).Name + ',' + Surface( IWin ).Name + ',' + RoundSigDigits( DaylFac1, 4 ) + ',' + RoundSigDigits( DaylFac2, 4 );
							}
						}
					}
					FirstTimeDaylFacCalc = false;
					doSkyReporting = false;
				}
			}
		}

		// TH 7/2010 report all daylight factors for the two reference points of daylight zones ...

		// Skip if no daylight windows
		if ( TotWindowsWithDayl == 0 ) return;

		// Skip if no request of reporting
		if ( ( ! DFSReportSizingDays ) && ( ! DFSReportAllShadowCalculationDays ) ) return;

		// Skip duplicate calls
		if ( KickOffSizing ) return;
		if ( DoingSizing ) return;
		if ( KickOffSimulation ) return;

		if ( DFSReportSizingDays ) {
			if ( DoWeathSim && DoDesDaySim ) {
				if ( KindOfSim == ksRunPeriodWeather ) return;
			}
		}

		if ( DFSReportAllShadowCalculationDays ) {
			if ( KindOfSim != ksRunPeriodWeather ) return;
		}

		// open a new file eplusout.dfs for saving the daylight factors
		if ( CreateDFSReportFile ) {
			OutputFileDFS = GetNewUnitNumber();
			{ IOFlags flags; flags.ACTION( "write" ); gio::open( OutputFileDFS, DataStringGlobals::outputDfsFileName, flags ); write_stat = flags.ios(); }
			if ( write_stat != 0 ) {
				ShowFatalError( "CalcDayltgCoefficients: Could not open file "+DataStringGlobals::outputDfsFileName+" for output (write)." );
			} else {
				gio::write( OutputFileDFS, fmtA ) << "This file contains daylight factors for all exterior windows of daylight zones.";
				gio::write( OutputFileDFS, fmtA ) << "If only one reference point the last 4 columns in the data will be zero.";
				gio::write( OutputFileDFS, fmtA ) << "MonthAndDay,Zone Name,Window Name,Window State";
				gio::write( OutputFileDFS, fmtA ) << "Hour,Daylight Factor for Clear Sky at Reference point 1,Daylight Factor for Clear Turbid Sky at Reference point 1,Daylight Factor for Intermediate Sky at Reference point 1,Daylight Factor for Overcast Sky at Reference point 1,Daylight Factor for Clear Sky at Reference point 2,Daylight Factor for Clear Turbid Sky at Reference point 2,Daylight Factor for Intermediate Sky at Reference point 2,Daylight Factor for Overcast Sky at Reference point 2";
			}
			CreateDFSReportFile = false;
		}

		for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
			if ( ZoneDaylight( ZoneNum ).NumOfDayltgExtWins == 0 ) continue;

			for ( loop = 1; loop <= ZoneDaylight( ZoneNum ).NumOfDayltgExtWins; ++loop ) {
				IWin = ZoneDaylight( ZoneNum ).DayltgExtWinSurfNums( loop );
				ICtrl = Surface( IWin ).WindowShadingControlPtr;

				// For this report, do not include ext wins in zone adjacent to ZoneNum since the inter-reflected
				// component will not be calculated for these windows until the time-step loop.
				if ( Surface( IWin ).Zone == ZoneNum ) {

					if ( SurfaceWindow( IWin ).MovableSlats ) {
						// variable slat angle - MaxSlatangle sets
						ISA = MaxSlatAngs + 1;
					} else if ( ICtrl > 0 ) {
						// window shade or blind with fixed slat angle
						ISA = 2;
					} else {
						// base window
						ISA = 1;
					}

					// loop over each slat angle
					for ( ISlatAngle = 1; ISlatAngle <= ISA; ++ISlatAngle ) {
						if ( ISlatAngle == 1 ) {
							// base window without shades, screens, or blinds
							gio::write( OutputFileDFS, fmtA ) << CurMnDy + ',' + Zone( ZoneNum ).Name + ',' + Surface( IWin ).Name + ",Base Window";
						} else if ( ISlatAngle == 2 && ISA == 2 ) {
							// window shade or blind with fixed slat angle
							gio::write( OutputFileDFS, fmtA ) << CurMnDy + ',' + Zone( ZoneNum ).Name + ',' + Surface( IWin ).Name + ", ";
						} else {
							// blind with variable slat angle
							SlatAngle = 180.0 / double( MaxSlatAngs - 1 ) * double( ISlatAngle - 2 );
							gio::write( OutputFileDFS, fmtA ) << CurMnDy + ',' + Zone( ZoneNum ).Name + ',' + Surface( IWin ).Name + ',' + RoundSigDigits( SlatAngle, 1 );
						}

						for ( IHR = 1; IHR <= 24; ++IHR ) {
							// daylight reference point 1
							DFClrSky1 = ZoneDaylight( ZoneNum ).DaylIllFacSky( IHR, ISlatAngle, 1, 1, loop ); // clear sky
							DFClrTbSky1 = ZoneDaylight( ZoneNum ).DaylIllFacSky( IHR, ISlatAngle, 2, 1, loop ); // clear Turbid sky
							DFIntSky1 = ZoneDaylight( ZoneNum ).DaylIllFacSky( IHR, ISlatAngle, 3, 1, loop ); // Intermediate sky
							DFOcSky1 = ZoneDaylight( ZoneNum ).DaylIllFacSky( IHR, ISlatAngle, 4, 1, loop ); // Overcast sky

							// daylight reference point 2
							if ( ZoneDaylight( ZoneNum ).TotalDaylRefPoints > 1 ) {
								DFClrSky2 = ZoneDaylight( ZoneNum ).DaylIllFacSky( IHR, ISlatAngle, 1, 2, loop );
								DFClrTbSky2 = ZoneDaylight( ZoneNum ).DaylIllFacSky( IHR, ISlatAngle, 2, 2, loop );
								DFIntSky2 = ZoneDaylight( ZoneNum ).DaylIllFacSky( IHR, ISlatAngle, 3, 2, loop );
								DFOcSky2 = ZoneDaylight( ZoneNum ).DaylIllFacSky( IHR, ISlatAngle, 4, 2, loop );
							} else {
								DFClrSky2 = 0.0;
								DFClrTbSky2 = 0.0;
								DFIntSky2 = 0.0;
								DFOcSky2 = 0.0;
							}

							// write daylight factors - 4 sky types for each daylight ref point
							gio::write( OutputFileDFS, fmtA ) << RoundSigDigits( IHR ) + ',' + RoundSigDigits( DFClrSky1, 5 ) + ',' + RoundSigDigits( DFClrTbSky1, 5 ) + ',' + RoundSigDigits( DFIntSky1, 5 ) + ',' + RoundSigDigits( DFOcSky1, 5 ) + ',' + RoundSigDigits( DFClrSky2, 5 ) + ',' + RoundSigDigits( DFClrTbSky2, 5 ) + ',' + RoundSigDigits( DFIntSky2, 5 ) + ',' + RoundSigDigits( DFOcSky2, 5 );
						} // hour loop
					}
				}
			} // exterior windows in zone loop
		} // zone loop

	}

	void
	CalcDayltgCoeffsRefMapPoints( int const ZoneNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   October 2004
		//       MODIFIED       May 2006 (RR): added exterior window screens
		//                      April 2012 (LKL); change to allow multiple maps per zone
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine does the daylighting coefficient calculation for the
		// daylighting and illuminance map reference points.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::BlindBeamBeamTrans;
		using General::SafeDivide;
		using General::RoundSigDigits;
		using DaylightingDevices::FindTDDPipe;
		using DaylightingDevices::TransTDD;
		using namespace Vectors;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int IWin; // Window counter
		int PipeNum; // TDD pipe object number
		int loopwin; // loop index for exterior windows associated with a daylit zone
		static bool VeryFirstTime( true );
		int TZoneNum;
		bool ErrorsFound;
		int MapNum;

		if ( VeryFirstTime ) {
			// make sure all necessary surfaces match to pipes
			ErrorsFound = false;
			for ( TZoneNum = 1; TZoneNum <= NumOfZones; ++TZoneNum ) {
				for ( loopwin = 1; loopwin <= ZoneDaylight( TZoneNum ).NumOfDayltgExtWins; ++loopwin ) {
					IWin = ZoneDaylight( TZoneNum ).DayltgExtWinSurfNums( loopwin );
					if ( SurfaceWindow( IWin ).OriginalClass != SurfaceClass_TDD_Diffuser ) continue;
					// Look up the TDD:DOME object
					PipeNum = FindTDDPipe( IWin );
					if ( PipeNum == 0 ) {
						ShowSevereError( "GetTDDInput: Surface=" + Surface( IWin ).Name + ", TDD:Dome object does not reference a valid Diffuser object." );
						ShowContinueError( "...needs DaylightingDevice:Tubular of same name as Surface." );
						ErrorsFound = true;
					}
				}
			}

			if ( ErrorsFound ) {
				ShowFatalError( "Not all TubularDaylightDome objects have corresponding DaylightingDevice:Tubular objects. Program terminates." );
			}
			VeryFirstTime = false;
		}

		//Calc for daylighting reference points
		CalcDayltgCoeffsRefPoints( ZoneNum );
		if ( ! DoingSizing && ! KickOffSimulation ) {
			//Calc for illuminance map
			if ( TotIllumMaps > 0 ) {
				for ( MapNum = 1; MapNum <= TotIllumMaps; ++MapNum ) {
					if ( IllumMapCalc( MapNum ).Zone != ZoneNum ) continue;
					if ( WarmupFlag ) {
						DisplayString( "Calculating Daylighting Coefficients (Map Points), Zone=" + Zone( ZoneNum ).Name );
					} else {
						DisplayString( "Updating Daylighting Coefficients (Map Points), Zone=" + Zone( ZoneNum ).Name );
					}
				}
				CalcDayltgCoeffsMapPoints( ZoneNum );
			}
		}

	}

	void
	CalcDayltgCoeffsRefPoints( int const ZoneNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   April 2012
		//       MODIFIED       November 2012 (B. Griffith), refactor for detailed timestep integration and remove duplicate code
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Provides calculations for Daylighting Coefficients for daylighting reference points

		// METHODOLOGY EMPLOYED:
		// April 2012 change: Was previously part of CalcDayltgCoeffsRefMapPoints -- broken out to allow multiple
		// maps per zone

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::BlindBeamBeamTrans;
		using General::SafeDivide;
		using General::RoundSigDigits;
		using DaylightingDevices::FindTDDPipe;
		using DaylightingDevices::TransTDD;
		using namespace Vectors;
		using DataSystemVariables::DetailedSolarTimestepIntegration;
		using DataEnvironment::SunIsUp;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		static Vector3< Real64 > W2; // Second vertex of window
		static Vector3< Real64 > W3; // Third vertex of window
		static Vector3< Real64 > W21; // Vector from window vertex 2 to window vertex 1
		static Vector3< Real64 > W23; // Vector from window vertex 2 to window vertex 3
		static Vector3< Real64 > RREF; // Location of a reference point in absolute coordinate system
		static Vector3< Real64 > RREF2; // Location of virtual reference point in absolute coordinate system
		static Vector3< Real64 > RWIN; // Center of a window element in absolute coordinate system
		static Vector3< Real64 > RWIN2; // Center of a window element for TDD:DOME (if exists) in abs coord sys
		static Vector3< Real64 > Ray; // Unit vector along ray from reference point to window element
		static Vector3< Real64 > WNORM2; // Unit vector normal to TDD:DOME (if exists)
		static Vector3< Real64 > VIEWVC; // View vector in absolute coordinate system
		static Vector3< Real64 > U2; // Second vertex of window for TDD:DOME (if exists)
		static Vector3< Real64 > U21; // Vector from window vertex 2 to window vertex 1 for TDD:DOME (if exists)
		static Vector3< Real64 > U23; // Vector from window vertex 2 to window vertex 3 for TDD:DOME (if exists)
//		static Vector2< Real64 > ZF; // Fraction of zone controlled by each reference point //Unused

		static Vector3< Real64 > VIEWVC2; // Virtual view vector in absolute coordinate system
		int IHR; // Hour of day counter
		int NRF; // Number of daylighting reference points in a zone
		int IL; // Reference point counter
		Real64 AZVIEW; // Azimuth of view vector in absolute coord system for
		//  glare calculation (radians)
		int IConst; // Construction counter
		int ICtrl; // Window control counter
		int IWin; // Window counter
		int IWin2; // Secondary window counter (for TDD:DOME object, if exists)
		int InShelfSurf; // Inside daylighting shelf surface number
		int ShType; // Window shading type
		int BlNum; // Window Blind Number
		int LSHCAL; // Interior shade calculation flag: 0=not yet
		//  calculated, 1=already calculated
		int NWX; // Number of window elements in x direction for dayltg calc
		int NWY; // Number of window elements in y direction for dayltg calc
		int NWYlim; // For triangle, largest NWY for a given IX
		int IX; // Counter for window elements in the x direction
		int IY; // Counter for window elements in the y direction
		Real64 COSB; // Cosine of angle between window outward normal and ray from
		//  reference point to window element
		Real64 PHRAY; // Altitude of ray from reference point to window element (radians)
		Real64 THRAY; // Azimuth of ray from reference point to window element (radians)
		Real64 DOMEGA; // Solid angle subtended by window element wrt reference point (steradians)
		Real64 TVISB; // Visible transmittance of window for COSB angle of incidence (times light well
		//   efficiency, if appropriate)
		int ISunPos; // Sun position counter; used to avoid calculating various
		//  quantities that do not depend on sun position.
		Real64 ObTrans; // Product of solar transmittances of exterior obstructions hit by ray
		// from reference point through a window element
		int loopwin; // loop index for exterior windows associated with a daylit zone
		bool is_Rectangle; // True if window is rectangular
		bool is_Triangle; // True if window is triangular
		Real64 DWX; // Horizontal dimension of window element (m)
		Real64 DWY; // Vertical dimension of window element (m)
		Real64 DAXY; // Area of window element
		Real64 SkyObstructionMult; // Ratio of obstructed to unobstructed sky diffuse at a ground point
		int ExtWinType; // Exterior window type (InZoneExtWin, AdjZoneExtWin, NotInOrAdjZoneExtWin)
		static bool refFirstTime( true );
		int BRef;
		int ILB;
		bool hitIntObs; // True iff interior obstruction hit
		bool hitExtObs; // True iff ray from ref pt to ext win hits an exterior obstruction
		Real64 TVISIntWin; // Visible transmittance of int win at COSBIntWin for light from ext win
		Real64 TVISIntWinDisk; // Visible transmittance of int win at COSBIntWin for sun
		static bool MySunIsUpFlag( false );

		int WinEl; // Current window element

		if ( refFirstTime && std::any_of( ZoneDaylight.begin(), ZoneDaylight.end(), []( ZoneDaylightCalc const & e ){ return e.TotalDaylRefPoints > 0; } ) ) {
			RefErrIndex.allocate( maxval( ZoneDaylight, &ZoneDaylightCalc::TotalDaylRefPoints ), TotSurfaces );
			RefErrIndex = 0;
			refFirstTime = false;
		}

		// Azimuth of view vector in absolute coord sys
		AZVIEW = ( ZoneDaylight( ZoneNum ).ViewAzimuthForGlare + Zone( ZoneNum ).RelNorth + BuildingAzimuth + BuildingRotationAppendixG ) * DegToRadians;
		// View vector components in absolute coord sys
		VIEWVC( 1 ) = std::sin( AZVIEW );
		VIEWVC( 2 ) = std::cos( AZVIEW );
		VIEWVC( 3 ) = 0.0;

		ZoneDaylight( ZoneNum ).DaylIllumAtRefPt = 0.0; // Daylight illuminance at reference points (lux)
		ZoneDaylight( ZoneNum ).GlareIndexAtRefPt = 0.0; // Glare index at reference points
		ZoneDaylight( ZoneNum ).SolidAngAtRefPt = 0.0;
		ZoneDaylight( ZoneNum ).SolidAngAtRefPtWtd = 0.0;
		ZoneDaylight( ZoneNum ).IllumFromWinAtRefPt = 0.0;
		ZoneDaylight( ZoneNum ).BackLumFromWinAtRefPt = 0.0;
		ZoneDaylight( ZoneNum ).SourceLumFromWinAtRefPt = 0.0;

		if ( ! DetailedSolarTimestepIntegration ) {

			ZoneDaylight( ZoneNum ).DaylIllFacSky = 0.0;
			ZoneDaylight( ZoneNum ).DaylSourceFacSky = 0.0;
			ZoneDaylight( ZoneNum ).DaylBackFacSky = 0.0;
			ZoneDaylight( ZoneNum ).DaylIllFacSun = 0.0;
			ZoneDaylight( ZoneNum ).DaylIllFacSunDisk = 0.0;
			ZoneDaylight( ZoneNum ).DaylSourceFacSun = 0.0;
			ZoneDaylight( ZoneNum ).DaylSourceFacSunDisk = 0.0;
			ZoneDaylight( ZoneNum ).DaylBackFacSun = 0.0;
			ZoneDaylight( ZoneNum ).DaylBackFacSunDisk = 0.0;
		} else {

			ZoneDaylight( ZoneNum ).DaylIllFacSky( HourOfDay, {1,MaxSlatAngs + 1}, {1,4}, {1,MaxRefPoints}, {1,ZoneDaylight( ZoneNum ).NumOfDayltgExtWins} ) = 0.0;
			ZoneDaylight( ZoneNum ).DaylSourceFacSky( HourOfDay, {1,MaxSlatAngs + 1}, {1,4}, {1,MaxRefPoints}, {1,ZoneDaylight( ZoneNum ).NumOfDayltgExtWins} ) = 0.0;
			ZoneDaylight( ZoneNum ).DaylBackFacSky( HourOfDay, {1,MaxSlatAngs + 1}, {1,4}, {1,MaxRefPoints}, {1,ZoneDaylight( ZoneNum ).NumOfDayltgExtWins} ) = 0.0;
			ZoneDaylight( ZoneNum ).DaylIllFacSun( HourOfDay, {1,MaxSlatAngs + 1}, {1,MaxRefPoints}, {1,ZoneDaylight( ZoneNum ).NumOfDayltgExtWins} ) = 0.0;
			ZoneDaylight( ZoneNum ).DaylIllFacSunDisk( HourOfDay, {1,MaxSlatAngs + 1}, {1,MaxRefPoints}, {1,ZoneDaylight( ZoneNum ).NumOfDayltgExtWins} ) = 0.0;
			ZoneDaylight( ZoneNum ).DaylSourceFacSun( HourOfDay, {1,MaxSlatAngs + 1}, {1,MaxRefPoints}, {1,ZoneDaylight( ZoneNum ).NumOfDayltgExtWins} ) = 0.0;
			ZoneDaylight( ZoneNum ).DaylSourceFacSunDisk( HourOfDay, {1,MaxSlatAngs + 1}, {1,MaxRefPoints}, {1,ZoneDaylight( ZoneNum ).NumOfDayltgExtWins} ) = 0.0;
			ZoneDaylight( ZoneNum ).DaylBackFacSun( HourOfDay, {1,MaxSlatAngs + 1}, {1,MaxRefPoints}, {1,ZoneDaylight( ZoneNum ).NumOfDayltgExtWins} ) = 0.0;
			ZoneDaylight( ZoneNum ).DaylBackFacSunDisk( HourOfDay, {1,MaxSlatAngs + 1}, {1,MaxRefPoints}, {1,ZoneDaylight( ZoneNum ).NumOfDayltgExtWins} ) = 0.0;
		}

		NRF = ZoneDaylight( ZoneNum ).TotalDaylRefPoints;
		BRef = 0;

		for ( IL = 1; IL <= NRF; ++IL ) {
			// Reference point in absolute coordinate system
			RREF = ZoneDaylight( ZoneNum ).DaylRefPtAbsCoord( {1,3}, IL ); // (x, y, z)

			//           -------------
			// ---------- WINDOW LOOP ----------
			//           -------------
			for ( loopwin = 1; loopwin <= ZoneDaylight( ZoneNum ).NumOfDayltgExtWins; ++loopwin ) {

				FigureDayltgCoeffsAtPointsSetupForWindow( ZoneNum, IL, loopwin, CalledForRefPoint, RREF, VIEWVC, IWin, IWin2, NWX, NWY, W2, W3, W21, W23, LSHCAL, InShelfSurf, ICtrl, ShType, BlNum, WNORM2, ExtWinType, IConst, RREF2, DWX, DWY, DAXY, U2, U23, U21, VIEWVC2, is_Rectangle, is_Triangle );
				//           ---------------------
				// ---------- WINDOW ELEMENT LOOP ----------
				//           ---------------------

				WinEl = 0;

				for ( IX = 1; IX <= NWX; ++IX ) {
					if ( is_Rectangle ) {
						NWYlim = NWY;
					} else if ( is_Triangle ) {
						NWYlim = NWY - IX + 1;
					}

					for ( IY = 1; IY <= NWYlim; ++IY ) {

						++WinEl;

						FigureDayltgCoeffsAtPointsForWindowElements( ZoneNum, IL, loopwin, CalledForRefPoint, WinEl, IWin, IWin2, IX, IY, SkyObstructionMult, W2, W21, W23, RREF, NWYlim, VIEWVC2, DWX, DWY, DAXY, U2, U23, U21, RWIN, RWIN2, Ray, PHRAY, LSHCAL, COSB, ObTrans, TVISB, DOMEGA, THRAY, hitIntObs, hitExtObs, WNORM2, ExtWinType, IConst, RREF2, is_Triangle, TVISIntWin, TVISIntWinDisk );

						//           -------------------
						// ---------- SUN POSITION LOOP ----------
						//           -------------------

						// Sun position counter. Used to avoid calculating various quantities
						// that do not depend on sun position.

						if ( ! DetailedSolarTimestepIntegration ) {
							ISunPos = 0;
							for ( IHR = 1; IHR <= 24; ++IHR ) {

								FigureDayltgCoeffsAtPointsForSunPosition( ZoneNum, IL, IX, NWX, IY, NWYlim, WinEl, IWin, IWin2, IHR, ISunPos, SkyObstructionMult, RWIN2, Ray, PHRAY, LSHCAL, InShelfSurf, COSB, ObTrans, TVISB, DOMEGA, ICtrl, ShType, BlNum, THRAY, WNORM2, ExtWinType, IConst, AZVIEW, RREF2, hitIntObs, hitExtObs, CalledForRefPoint, TVISIntWin, TVISIntWinDisk );

							} // End of hourly sun position loop, IHR
						} else { //timestep integrated
							if ( SunIsUp && ! MySunIsUpFlag ) {
								ISunPos = 0;
								MySunIsUpFlag = true;
							} else if ( SunIsUp && MySunIsUpFlag ) {
								ISunPos = 1;
							} else if ( ! SunIsUp && MySunIsUpFlag ) {
								MySunIsUpFlag = false;
								ISunPos = -1;
							} else if ( ! SunIsUp && ! MySunIsUpFlag ) {
								ISunPos = -1;
							}

							FigureDayltgCoeffsAtPointsForSunPosition( ZoneNum, IL, IX, NWX, IY, NWYlim, WinEl, IWin, IWin2, HourOfDay, ISunPos, SkyObstructionMult, RWIN2, Ray, PHRAY, LSHCAL, InShelfSurf, COSB, ObTrans, TVISB, DOMEGA, ICtrl, ShType, BlNum, THRAY, WNORM2, ExtWinType, IConst, AZVIEW, RREF2, hitIntObs, hitExtObs, CalledForRefPoint, TVISIntWin, TVISIntWinDisk );
						}

					} // End of window Y-element loop, IY
				} // End of window X-element loop, IX

				// Loop again over hourly sun positions and calculate daylight factors by adding
				// direct and inter-reflected illum components, then dividing by exterior horiz illum.
				// Also calculate corresponding glare factors.

				ILB = BRef + IL;

				if ( ! DetailedSolarTimestepIntegration ) {
					ISunPos = 0;
					for ( IHR = 1; IHR <= 24; ++IHR ) {
						FigureRefPointDayltgFactorsToAddIllums( ZoneNum, ILB, IHR, ISunPos, IWin, loopwin, NWX, NWY, ICtrl );

					} // End of sun position loop, IHR
				} else {
					if ( SunIsUp && ! MySunIsUpFlag ) {
						ISunPos = 0;
						MySunIsUpFlag = true;
					} else if ( SunIsUp && MySunIsUpFlag ) {
						ISunPos = 1;
					} else if ( ! SunIsUp && MySunIsUpFlag ) {
						MySunIsUpFlag = false;
						ISunPos = -1;
					} else if ( ! SunIsUp && ! MySunIsUpFlag ) {
						ISunPos = -1;
					}
					FigureRefPointDayltgFactorsToAddIllums( ZoneNum, ILB, HourOfDay, ISunPos, IWin, loopwin, NWX, NWY, ICtrl );
				}
			} // End of window loop, loopwin - IWin

		} // End of reference point loop, IL

	}

	void
	CalcDayltgCoeffsMapPoints( int const ZoneNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   April 2012
		//       MODIFIED      November 2012 (B. Griffith), refactor for detailed timestep integration and remove duplicate code
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Provides calculations for Daylighting Coefficients for map illuminance points

		// METHODOLOGY EMPLOYED:
		// Was previously part of CalcDayltgCoeffsRefMapPoints -- broken out to all multiple
		// maps per zone

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::BlindBeamBeamTrans;
		using General::SafeDivide;
		using General::RoundSigDigits;
		using DaylightingDevices::FindTDDPipe;
		using DaylightingDevices::TransTDD;
		using namespace Vectors;
		using DataSystemVariables::DetailedSolarTimestepIntegration;
		using DataEnvironment::SunIsUp;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:

		// the daylighting and glare factors

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		static Vector3< Real64 > W2; // Second vertex of window
		static Vector3< Real64 > W3; // Third vertex of window
		static Vector3< Real64 > U2; // Second vertex of window for TDD:DOME (if exists)
		static Vector3< Real64 > RREF; // Location of a reference point in absolute coordinate system
		static Vector3< Real64 > RREF2; // Location of virtual reference point in absolute coordinate system
		static Vector3< Real64 > RWIN; // Center of a window element in absolute coordinate system
		static Vector3< Real64 > RWIN2; // Center of a window element for TDD:DOME (if exists) in abs coord sys
		static Vector3< Real64 > Ray; // Unit vector along ray from reference point to window element
		static Vector3< Real64 > W21; // Vector from window vertex 2 to window vertex 1
		static Vector3< Real64 > W23; // Vector from window vertex 2 to window vertex 3
		static Vector3< Real64 > U21; // Vector from window vertex 2 to window vertex 1 for TDD:DOME (if exists)
		static Vector3< Real64 > U23; // Vector from window vertex 2 to window vertex 3 for TDD:DOME (if exists)
		static Vector3< Real64 > WNORM2; // Unit vector normal to TDD:DOME (if exists)
		static Vector3< Real64 > VIEWVC; // View vector in absolute coordinate system
		static Vector3< Real64 > VIEWVC2; // Virtual view vector in absolute coordinate system
//		static Vector2< Real64 > ZF; // Fraction of zone controlled by each reference point //Unused
		//  In the following four variables, I=1 for clear sky, 2 for overcast.
		int IHR; // Hour of day counter
		int NRF; // Number of daylighting reference points in a zone
		int IL; // Reference point counter
		Real64 AZVIEW; // Azimuth of view vector in absolute coord system for
		//  glare calculation (radians)
		int IConst; // Construction counter
		int ICtrl; // Window control counter
		int IWin; // Window counter
		int IWin2; // Secondary window counter (for TDD:DOME object, if exists)
		int InShelfSurf; // Inside daylighting shelf surface number
		int ShType; // Window shading type
		int BlNum; // Window Blind Number
		int LSHCAL; // Interior shade calculation flag: 0=not yet
		//  calculated, 1=already calculated
		int NWX; // Number of window elements in x direction for dayltg calc
		int NWY; // Number of window elements in y direction for dayltg calc
		int NWYlim; // For triangle, largest NWY for a given IX
		Real64 DWX; // Horizontal dimension of window element (m)
		Real64 DWY; // Vertical dimension of window element (m)
		int IX; // Counter for window elements in the x direction
		int IY; // Counter for window elements in the y direction
		Real64 COSB; // Cosine of angle between window outward normal and ray from
		//  reference point to window element
		Real64 PHRAY; // Altitude of ray from reference point to window element (radians)
		Real64 THRAY; // Azimuth of ray from reference point to window element (radians)
		Real64 DOMEGA; // Solid angle subtended by window element wrt reference point (steradians)
		Real64 TVISB; // Visible transmittance of window for COSB angle of incidence (times light well
		//   efficiency, if appropriate)
		int ISunPos; // Sun position counter; used to avoid calculating various
		//  quantities that do not depend on sun position.
		Real64 ObTrans; // Product of solar transmittances of exterior obstructions hit by ray
		// from reference point through a window element
		int loopwin; // loop index for exterior windows associated with a daylit zone
		bool is_Rectangle; // True if window is rectangular
		bool is_Triangle; // True if window is triangular
		Real64 DAXY; // Area of window element
		Real64 SkyObstructionMult; // Ratio of obstructed to unobstructed sky diffuse at a ground point
		int ExtWinType; // Exterior window type (InZoneExtWin, AdjZoneExtWin, NotInOrAdjZoneExtWin)
		int ILB;
		int MapNum; // Loop for map number
		bool hitIntObs; // True iff interior obstruction hit
		bool hitExtObs; // True iff ray from ref pt to ext win hits an exterior obstruction
		Real64 TVISIntWin; // Visible transmittance of int win at COSBIntWin for light from ext win
		Real64 TVISIntWinDisk; // Visible transmittance of int win at COSBIntWin for sun
//		Array2D< Real64 > MapWindowSolidAngAtRefPt; //Inactive Only allocated and assigning to: Also only 1 value used at a time
//		Array2D< Real64 > MapWindowSolidAngAtRefPtWtd; // Only 1 value used at a time: Replaced by below
		Real64 MapWindowSolidAngAtRefPtWtd;
		static bool mapFirstTime( true );
		static bool MySunIsUpFlag( false );
		int WinEl; // window elements counter

		if ( mapFirstTime && TotIllumMaps > 0 ) {
			IL = -999;
			for ( MapNum = 1; MapNum <= TotIllumMaps; ++MapNum ) {
				IL = max( IL, IllumMapCalc( MapNum ).TotalMapRefPoints );
			}
			MapErrIndex.dimension( IL, TotSurfaces, 0 );
			mapFirstTime = false;
		}

		// Azimuth of view vector in absolute coord sys
		AZVIEW = ( ZoneDaylight( ZoneNum ).ViewAzimuthForGlare + Zone( ZoneNum ).RelNorth + BuildingAzimuth + BuildingRotationAppendixG ) * DegToRadians;
		// View vector components in absolute coord sys
		VIEWVC( 1 ) = std::sin( AZVIEW );
		VIEWVC( 2 ) = std::cos( AZVIEW );
		VIEWVC( 3 ) = 0.0;

		for ( MapNum = 1; MapNum <= TotIllumMaps; ++MapNum ) {

			if ( IllumMapCalc( MapNum ).Zone != ZoneNum ) continue;

			IllumMapCalc( MapNum ).DaylIllumAtMapPt = 0.0; // Daylight illuminance at reference points (lux)
			IllumMapCalc( MapNum ).GlareIndexAtMapPt = 0.0; // Glare index at reference points
			IllumMapCalc( MapNum ).SolidAngAtMapPt = 0.0;
			IllumMapCalc( MapNum ).SolidAngAtMapPtWtd = 0.0;
			IllumMapCalc( MapNum ).IllumFromWinAtMapPt = 0.0;
			IllumMapCalc( MapNum ).BackLumFromWinAtMapPt = 0.0;
			IllumMapCalc( MapNum ).SourceLumFromWinAtMapPt = 0.0;
			if ( ! DetailedSolarTimestepIntegration ) {
				IllumMapCalc( MapNum ).DaylIllFacSky = 0.0;
				IllumMapCalc( MapNum ).DaylSourceFacSky = 0.0;
				IllumMapCalc( MapNum ).DaylBackFacSky = 0.0;
				IllumMapCalc( MapNum ).DaylIllFacSun = 0.0;
				IllumMapCalc( MapNum ).DaylIllFacSunDisk = 0.0;
				IllumMapCalc( MapNum ).DaylSourceFacSun = 0.0;
				IllumMapCalc( MapNum ).DaylSourceFacSunDisk = 0.0;
				IllumMapCalc( MapNum ).DaylBackFacSun = 0.0;
				IllumMapCalc( MapNum ).DaylBackFacSunDisk = 0.0;
			} else {
				IllumMapCalc( MapNum ).DaylIllFacSky( HourOfDay, {1,MaxSlatAngs + 1}, {1,4}, {1,MaxRefPoints}, {1,ZoneDaylight( ZoneNum ).NumOfDayltgExtWins} ) = 0.0;
				IllumMapCalc( MapNum ).DaylSourceFacSky( HourOfDay, {1,MaxSlatAngs + 1}, {1,4}, {1,MaxRefPoints}, {1,ZoneDaylight( ZoneNum ).NumOfDayltgExtWins} ) = 0.0;
				IllumMapCalc( MapNum ).DaylBackFacSky( HourOfDay, {1,MaxSlatAngs + 1}, {1,4}, {1,MaxRefPoints}, {1,ZoneDaylight( ZoneNum ).NumOfDayltgExtWins} ) = 0.0;
				IllumMapCalc( MapNum ).DaylIllFacSun( HourOfDay, {1,MaxSlatAngs + 1}, {1,MaxRefPoints}, {1,ZoneDaylight( ZoneNum ).NumOfDayltgExtWins} ) = 0.0;
				IllumMapCalc( MapNum ).DaylIllFacSunDisk( HourOfDay, {1,MaxSlatAngs + 1}, {1,MaxRefPoints}, {1,ZoneDaylight( ZoneNum ).NumOfDayltgExtWins} ) = 0.0;
				IllumMapCalc( MapNum ).DaylSourceFacSun( HourOfDay, {1,MaxSlatAngs + 1}, {1,MaxRefPoints}, {1,ZoneDaylight( ZoneNum ).NumOfDayltgExtWins} ) = 0.0;
				IllumMapCalc( MapNum ).DaylSourceFacSunDisk( HourOfDay, {1,MaxSlatAngs + 1}, {1,MaxRefPoints}, {1,ZoneDaylight( ZoneNum ).NumOfDayltgExtWins} ) = 0.0;
				IllumMapCalc( MapNum ).DaylBackFacSun( HourOfDay, {1,MaxSlatAngs + 1}, {1,MaxRefPoints}, {1,ZoneDaylight( ZoneNum ).NumOfDayltgExtWins} ) = 0.0;
				IllumMapCalc( MapNum ).DaylBackFacSunDisk( HourOfDay, {1,MaxSlatAngs + 1}, {1,MaxRefPoints}, {1,ZoneDaylight( ZoneNum ).NumOfDayltgExtWins} ) = 0.0;
			}
			NRF = IllumMapCalc( MapNum ).TotalMapRefPoints;

//			MapWindowSolidAngAtRefPt.allocate( NRF, ZoneDaylight( ZoneNum ).NumOfDayltgExtWins ); //Inactive
//			MapWindowSolidAngAtRefPtWtd.allocate( NRF, ZoneDaylight( ZoneNum ).NumOfDayltgExtWins ); // Not an array anymore

			for ( IL = 1; IL <= NRF; ++IL ) {

				RREF = IllumMapCalc( MapNum ).MapRefPtAbsCoord( {1,3}, IL ); // (x, y, z)

				//           -------------
				// ---------- WINDOW LOOP ----------
				//           -------------

//				MapWindowSolidAngAtRefPt = 0.0; //Inactive
				MapWindowSolidAngAtRefPtWtd = 0.0;

				for ( loopwin = 1; loopwin <= ZoneDaylight( ZoneNum ).NumOfDayltgExtWins; ++loopwin ) {

					FigureDayltgCoeffsAtPointsSetupForWindow( ZoneNum, IL, loopwin, CalledForMapPoint, RREF, VIEWVC, IWin, IWin2, NWX, NWY, W2, W3, W21, W23, LSHCAL, InShelfSurf, ICtrl, ShType, BlNum, WNORM2, ExtWinType, IConst, RREF2, DWX, DWY, DAXY, U2, U23, U21, VIEWVC2, is_Rectangle, is_Triangle, MapNum, MapWindowSolidAngAtRefPtWtd ); // Inactive MapWindowSolidAngAtRefPt arg removed
					//           ---------------------
					// ---------- WINDOW ELEMENT LOOP ----------
					//           ---------------------
					WinEl = 0;

					for ( IX = 1; IX <= NWX; ++IX ) {
						if ( is_Rectangle ) {
							NWYlim = NWY;
						} else if ( is_Triangle ) {
							NWYlim = NWY - IX + 1;
						}

						for ( IY = 1; IY <= NWYlim; ++IY ) {

							++WinEl;

							FigureDayltgCoeffsAtPointsForWindowElements( ZoneNum, IL, loopwin, CalledForMapPoint, WinEl, IWin, IWin2, IX, IY, SkyObstructionMult, W2, W21, W23, RREF, NWYlim, VIEWVC2, DWX, DWY, DAXY, U2, U23, U21, RWIN, RWIN2, Ray, PHRAY, LSHCAL, COSB, ObTrans, TVISB, DOMEGA, THRAY, hitIntObs, hitExtObs, WNORM2, ExtWinType, IConst, RREF2, is_Triangle, TVISIntWin, TVISIntWinDisk, MapNum, MapWindowSolidAngAtRefPtWtd ); // Inactive MapWindowSolidAngAtRefPt arg removed
							//           -------------------
							// ---------- SUN POSITION LOOP ----------
							//           -------------------

							// Sun position counter. Used to avoid calculating various quantities
							// that do not depend on sun position.
							if ( ! DetailedSolarTimestepIntegration ) {
								ISunPos = 0;
								for ( IHR = 1; IHR <= 24; ++IHR ) {
									FigureDayltgCoeffsAtPointsForSunPosition( ZoneNum, IL, IX, NWX, IY, NWYlim, WinEl, IWin, IWin2, IHR, ISunPos, SkyObstructionMult, RWIN2, Ray, PHRAY, LSHCAL, InShelfSurf, COSB, ObTrans, TVISB, DOMEGA, ICtrl, ShType, BlNum, THRAY, WNORM2, ExtWinType, IConst, AZVIEW, RREF2, hitIntObs, hitExtObs, CalledForMapPoint, TVISIntWin, TVISIntWinDisk, MapNum, MapWindowSolidAngAtRefPtWtd );
								} // End of hourly sun position loop, IHR
							} else {
								if ( SunIsUp && ! MySunIsUpFlag ) {
									ISunPos = 0;
									MySunIsUpFlag = true;
								} else if ( SunIsUp && MySunIsUpFlag ) {
									ISunPos = 1;
								} else if ( ! SunIsUp && MySunIsUpFlag ) {
									MySunIsUpFlag = false;
									ISunPos = -1;
								} else if ( ! SunIsUp && ! MySunIsUpFlag ) {
									ISunPos = -1;
								}
								FigureDayltgCoeffsAtPointsForSunPosition( ZoneNum, IL, IX, NWX, IY, NWYlim, WinEl, IWin, IWin2, HourOfDay, ISunPos, SkyObstructionMult, RWIN2, Ray, PHRAY, LSHCAL, InShelfSurf, COSB, ObTrans, TVISB, DOMEGA, ICtrl, ShType, BlNum, THRAY, WNORM2, ExtWinType, IConst, AZVIEW, RREF2, hitIntObs, hitExtObs, CalledForMapPoint, TVISIntWin, TVISIntWinDisk, MapNum, MapWindowSolidAngAtRefPtWtd );

							}
						} // End of window Y-element loop, IY
					} // End of window X-element loop, IX

					if ( ! DetailedSolarTimestepIntegration ) {
						// Loop again over hourly sun positions and calculate daylight factors by adding
						// direct and inter-reflected illum components, then dividing by exterior horiz illum.
						// Also calculate corresponding glare factors.
						ILB = IL;
						for ( IHR = 1; IHR <= 24; ++IHR ) {
							FigureMapPointDayltgFactorsToAddIllums( ZoneNum, MapNum, ILB, IHR, IWin, loopwin, NWX, NWY, ICtrl );
						} // End of sun position loop, IHR
					} else {
						ILB = IL;
						FigureMapPointDayltgFactorsToAddIllums( ZoneNum, MapNum, ILB, HourOfDay, IWin, loopwin, NWX, NWY, ICtrl );

					}

				} // End of window loop, loopwin - IWin

			} // End of reference point loop, IL

		} // MapNum

	}

	void
	FigureDayltgCoeffsAtPointsSetupForWindow(
		int const ZoneNum,
		int const iRefPoint,
		int const loopwin,
		int const CalledFrom, // indicate  which type of routine called this routine
		Vector3< Real64 > const & RREF, // Location of a reference point in absolute coordinate system
		Vector3< Real64 > const & VIEWVC, // View vector in absolute coordinate system
		int & IWin,
		int & IWin2,
		int & NWX,
		int & NWY,
		Vector3< Real64 > & W2, // Second vertex of window
		Vector3< Real64 > & W3, // Third vertex of window
		Vector3< Real64 > & W21, // Vector from window vertex 2 to window vertex 1
		Vector3< Real64 > & W23, // Vector from window vertex 2 to window vertex 3
		int & LSHCAL, // Interior shade calculation flag:  0=not yet calculated, 1=already calculated
		int & InShelfSurf, // Inside daylighting shelf surface number
		int & ICtrl, // Window control counter
		int & ShType, // Window shading type
		int & BlNum, // Window blind number
		Vector3< Real64 > & WNORM2, // Unit vector normal to window
		int & ExtWinType, // Exterior window type (InZoneExtWin, AdjZoneExtWin, NotInOrAdjZoneExtWin)
		int & IConst, // Construction counter
		Vector3< Real64 > & RREF2, // Location of virtual reference point in absolute coordinate system
		Real64 & DWX, // Horizontal dimension of window element (m)
		Real64 & DWY, // Vertical dimension of window element (m)
		Real64 & DAXY, // Area of window element
		Vector3< Real64 > & U2, // Second vertex of window for TDD:DOME (if exists)
		Vector3< Real64 > & U23, // Vector from window vertex 2 to window vertex 3 for TDD:DOME (if exists)
		Vector3< Real64 > & U21, // Vector from window vertex 2 to window vertex 1 for TDD:DOME (if exists)
		Vector3< Real64 > & VIEWVC2, // Virtual view vector in absolute coordinate system
		bool & is_Rectangle, // True if window is rectangular
		bool & is_Triangle, // True if window is triangular
		Optional_int_const MapNum,
//		Optional< Real64 > MapWindowSolidAngAtRefPt, //Inactive
		Optional< Real64 > MapWindowSolidAngAtRefPtWtd
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   November 2012, refactor from legacy code by Fred Winklemann
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// collect code to setup calculations for each window for daylighting coefficients

		// METHODOLOGY EMPLOYED:
		// switch as need to serve both reference points and map points based on calledFrom

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace Vectors;
		using General::POLYF;
		using General::BlindBeamBeamTrans;
		using General::SafeDivide;
		using General::RoundSigDigits;
		using DaylightingDevices::FindTDDPipe;
		using DataSystemVariables::DetailedSolarTimestepIntegration;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ZoneNumThisWin; // A window's zone number
		int ShelfNum; // Daylighting shelf object number

		static Vector3< Real64 > W1; // First vertex of window (where vertices are numbered
		// counter-clockwise starting at upper left as viewed
		// from inside of room
		int IConstShaded; // Shaded construction counter
//		int ScNum; // Window screen number //Unused Set but never used
		Real64 WW; // Window width (m)
		Real64 HW; // Window height (m)
		static Vector3< Real64 > WC; // Center point of window
		static Vector3< Real64 > REFWC; // Vector from reference point to center of window
		static Vector3< Real64 > WNORM; // Unit vector normal to window (pointing away from room)
		int NDIVX; // Number of window x divisions for daylighting calc
		int NDIVY; // Number of window y divisions for daylighting calc
		Real64 ALF; // Distance from reference point to window plane (m)
		static Vector3< Real64 > W2REF; // Vector from window origin to project of ref. pt. on window plane
		Real64 D1a; // Projection of vector from window origin to reference
		//  on window X  axis (m)
		Real64 D1b; // Projection of vector from window origin to reference
		//  on window Y axis (m)
		Real64 SolidAngExtWin; // Approx. solid angle subtended by an ext. window wrt ref pt
		Real64 SolidAngMinIntWin; // Approx. smallest solid angle subtended by an int. window wrt ref pt
		Real64 SolidAngRatio; // Ratio of SolidAngExtWin and SolidAngMinIntWin
		int PipeNum; // TDD pipe object number
		static Vector3< Real64 > REFD; // Vector from ref pt to center of win in TDD:DIFFUSER coord sys (if exists)
		static Vector3< Real64 > VIEWVD; // Virtual view vector in TDD:DIFFUSER coord sys (if exists)
		static Vector3< Real64 > U1; // First vertex of window for TDD:DOME (if exists)
		static Vector3< Real64 > U3; // Third vertex of window for TDD:DOME (if exists)
		Real64 SinCornerAng; // For triangle, sine of corner angle of window element

		// Complex fenestration variables
//		int CplxFenState; // Current complex fenestration state //Unused Set but never used
//		int NReflSurf; // Number of blocked beams for complex fenestration //Unused Set but never used
		int NRefPts; // number of reference points
//		int WinEl; // Current window element //Unused Set but never used
		static Vector3< Real64 > RayVector;
//		Real64 TransBeam; // Obstructions transmittance for incoming BSDF rays (temporary variable) //Unused Set but never used

		// Complex fenestration variables
//		CplxFenState = 0; //Unused Set but never used
//		NReflSurf = 0; //Unused Set but never used
//		WinEl = 0; //Unused Set but never used
//		TransBeam = 0.0; //Unused Set but never used
		NRefPts = 0;

		IWin = ZoneDaylight( ZoneNum ).DayltgExtWinSurfNums( loopwin );

		if ( CalledFrom == CalledForRefPoint ) {
			ZoneDaylight( ZoneNum ).SolidAngAtRefPt( loopwin, iRefPoint ) = 0.0;
			ZoneDaylight( ZoneNum ).SolidAngAtRefPtWtd( loopwin, iRefPoint ) = 0.0;
		} else if ( CalledFrom == CalledForMapPoint ) {
			IllumMapCalc( MapNum ).SolidAngAtMapPt( loopwin, iRefPoint ) = 0.0;
			IllumMapCalc( MapNum ).SolidAngAtMapPtWtd( loopwin, iRefPoint ) = 0.0;
		}
		ZoneNumThisWin = Surface( Surface( IWin ).BaseSurf ).Zone;
		if ( ZoneNumThisWin == ZoneNum ) {
			ExtWinType = InZoneExtWin;
		} else {
			ExtWinType = AdjZoneExtWin;
		}

		IConst = Surface( IWin ).Construction;
		if ( SurfaceWindow( IWin ).StormWinFlag == 1 ) IConst = Surface( IWin ).StormWinConstruction;

		// TH Added 6/29/2009.
		// For thermochromic windows, the daylight and glare factors are calculated for a base window cosntruction
		//  at base TC layer temperature. During each time step calculations at DayltgInteriorIllum,
		//  DayltgInteriorMapIllum, and DayltgGlare, the daylight and glare factors are adjusted by the visible
		//  transmittance ratio = VT of actual TC window based on last hour TC layer temperature / VT of the base TC window
		if ( Construct( IConst ).TCFlag == 1 ) {
			// For thermochromic windows, use the base window construction at base temperature of the TC layer
			IConst = Construct( IConst ).TCMasterConst;
		}

		ICtrl = Surface( IWin ).WindowShadingControlPtr;
		ShType = WSC_ST_NoShade; // 'NOSHADE'
		BlNum = 0;
//		ScNum = 0; //Unused Set but never used
		if ( ICtrl > 0 ) ShType = WindowShadingControl( ICtrl ).ShadingType;
		BlNum = SurfaceWindow( IWin ).BlindNumber;
//		ScNum = SurfaceWindow( IWin ).ScreenNumber; //Unused Set but never used

		ShelfNum = Surface( IWin ).Shelf;
		if ( ShelfNum > 0 ) {
			InShelfSurf = Shelf( Surface( IWin ).Shelf ).InSurf; // Inside daylighting shelf present if > 0
		} else {
			InShelfSurf = 0;
		}

		is_Rectangle = false;
		is_Triangle = false;
		if ( Surface( IWin ).Sides == 3 ) is_Triangle = true;
		if ( Surface( IWin ).Sides == 4 ) is_Rectangle = true;

		if ( is_Rectangle ) {
			// Vertices of window (numbered counter-clockwise starting at upper left as viewed
			// from inside of room). Assumes original vertices are numbered counter-clockwise from
			// upper left as viewed from outside.
			W3 = Surface( IWin ).Vertex( 2 );
			W2 = Surface( IWin ).Vertex( 3 );
			W1 = Surface( IWin ).Vertex( 4 );
		} else if ( is_Triangle ) {
			W3 = Surface( IWin ).Vertex( 2 );
			W2 = Surface( IWin ).Vertex( 3 );
			W1 = Surface( IWin ).Vertex( 1 );
		}

		// Shade/blind calculation flag
		LSHCAL = 0;

		// Visible transmittance at normal incidence
		SurfaceWindow( IWin ).VisTransSelected = POLYF( 1.0, Construct( IConst ).TransVisBeamCoef ) * SurfaceWindow( IWin ).GlazedFrac;
		// For windows with switchable glazing, ratio of visible transmittance at normal
		// incidence for fully switched (dark) state to that of unswitched state
		SurfaceWindow( IWin ).VisTransRatio = 1.0;
		if ( ICtrl > 0 ) {
			if ( ShType == WSC_ST_SwitchableGlazing ) {
				IConstShaded = Surface( IWin ).ShadedConstruction;
				SurfaceWindow( IWin ).VisTransRatio = SafeDivide( POLYF( 1.0, Construct( IConstShaded ).TransVisBeamCoef ), POLYF( 1.0, Construct( IConst ).TransVisBeamCoef ) );
			}
		}

		// Unit vectors from window vertex 2 to 1 and 2 to 3,
		// center point of window, and vector from ref pt to center of window
		W21 = W1 - W2;
		W23 = W3 - W2;
		HW =  W21.magnitude();
		WW =  W23.magnitude();
		if ( is_Rectangle ) {
			WC = W2 + ( W23 + W21 ) / 2.0;
		} else if ( is_Triangle ) {
			WC = W2 + ( W23 + W21 ) / 3.0;
		}
		SurfaceWindow( IWin ).WinCenter = WC;
		REFWC = WC - RREF;
		// Unit vectors
		W21 /= HW;
		W23 /= WW;

		// Unit vector normal to window (pointing away from room)
		WNORM = Surface( IWin ).lcsz;

		// Initialize number of window elements
		NDIVX = 40;
		NDIVY = 40;

		// Distance from ref point to window plane
		ALF = std::abs( dot( WNORM, REFWC ) );
		if ( CalledFrom == CalledForRefPoint ) {
			// Check if ref point to close to window due to input error (0.1524 m below is 0.5 ft)
			if ( ALF < 0.1524 && ExtWinType == InZoneExtWin ) {
				// Ref pt is close to window plane. Get vector from window
				// origin to projection of ref pt on window plane.
				W2REF = RREF + ALF * WNORM - W2;

				D1a = dot( W2REF, W23 );
				D1b = dot( W2REF, W21 );

				//            ! Error message if ref pt is too close to window.
				if ( D1a > 0.0 && D1b > 0.0 && D1b <= HW && D1a <= WW ) {
					ShowSevereError( "CalcDaylightCoeffRefPoints: Daylighting calculation cannot be done for zone " + Zone( ZoneNum ).Name + " because reference point #" + RoundSigDigits( iRefPoint ) + " is less than 0.15m (6\") from window plane " + Surface( IWin ).Name );
					ShowContinueError( "Distance=[" + RoundSigDigits( ALF, 5 ) + "]. This is too close; check position of reference point." );
					ShowFatalError( "Program terminates due to preceding condition." );
				}
			} else if ( ALF < 0.1524 && ExtWinType == AdjZoneExtWin ) {
				if ( RefErrIndex( iRefPoint, IWin ) == 0 ) { // only show error message once
					ShowWarningError( "CalcDaylightCoeffRefPoints: For Zone=\"" + Zone( ZoneNum ).Name + "\" External Window=\"" + Surface( IWin ).Name + "\"in Zone=\"" + Zone( Surface( IWin ).Zone ).Name + "\" reference point is less than 0.15m (6\") from window plane " );
					ShowContinueError( "Distance=[" + RoundSigDigits( ALF, 1 ) + " m] to ref point=[" + RoundSigDigits( RREF( 1 ), 1 ) + ',' + RoundSigDigits( RREF( 2 ), 1 ) + ',' + RoundSigDigits( RREF( 3 ), 1 ) + "], Inaccuracy in Daylighting Calcs may result." );
					RefErrIndex( iRefPoint, IWin ) = 1;
				}
			}
		} else if ( CalledFrom == CalledForMapPoint ) {
			if ( ALF < 0.1524 && ExtWinType == AdjZoneExtWin ) {
				if ( MapErrIndex( iRefPoint, IWin ) == 0 ) { // only show error message once
					ShowWarningError( "CalcDaylightCoeffMapPoints: For Zone=\"" + Zone( ZoneNum ).Name + "\" External Window=\"" + Surface( IWin ).Name + "\"in Zone=\"" + Zone( Surface( IWin ).Zone ).Name + "\" map point is less than 0.15m (6\") from window plane " );
					ShowContinueError( "Distance=[" + RoundSigDigits( ALF, 1 ) + " m] map point=[" + RoundSigDigits( RREF( 1 ), 1 ) + ',' + RoundSigDigits( RREF( 2 ), 1 ) + ',' + RoundSigDigits( RREF( 3 ), 1 ) + "], Inaccuracy in Map Calcs may result." );
					MapErrIndex( iRefPoint, IWin ) = 1;
				}
			}
		}
		// Number of window elements in X and Y for daylighting calculation
		if ( ALF > 0.1524 ) {
			NDIVX = 1 + int( 4.0 * WW / ALF );
			NDIVY = 1 + int( 4.0 * HW / ALF );
		}

		if ( ExtWinType == AdjZoneExtWin ) {
			// Adjust number of exterior window elements to give acceptable number of rays through
			// interior windows in the zone (for accuracy of interior window daylighting calculation)
			SolidAngExtWin = SafeDivide( ( ( Surface( IWin ).Area + SurfaceWindow( IWin ).DividerArea ) / Surface( IWin ).Multiplier ), pow_2( ALF ) );
			SolidAngMinIntWin = ZoneDaylight( ZoneNum ).MinIntWinSolidAng;
			SolidAngRatio = max( 1.0, SolidAngExtWin / SolidAngMinIntWin );
			NDIVX *= std::sqrt( SolidAngRatio );
			NDIVY *= std::sqrt( SolidAngRatio );
		}

		NWX = min( 40, NDIVX );
		NWY = min( 40, NDIVY );

		// Discretization of triangle is simpler if NWX = NWY
		if ( is_Triangle ) {
			NWX = max( NWX, NWY );
			NWY = NWX;
		}

		// Edge lengths of window elements
		DWX = WW / NWX;
		DWY = HW / NWY;

		// Azimuth and altitude of window normal
		SurfaceWindow( IWin ).Phi = std::asin( WNORM( 3 ) );
		if ( std::abs( WNORM( 1 ) ) > 1.0e-5 || std::abs( WNORM( 2 ) ) > 1.0e-5 ) {
			SurfaceWindow( IWin ).Theta = std::atan2( WNORM( 2 ), WNORM( 1 ) );
		} else {
			SurfaceWindow( IWin ).Theta = 0.0;
		}

		// Recalculation of values for TDD:DOME
		if ( SurfaceWindow( IWin ).OriginalClass == SurfaceClass_TDD_Diffuser ) {

			// Look up the TDD:DOME object
			PipeNum = FindTDDPipe( IWin );
			IWin2 = TDDPipe( PipeNum ).Dome;

			// Calculate reference point coords relative to the diffuser coordinate system
			// W21, W23, and WNORM are the unit vectors
			REFD( 1 ) = dot( REFWC, W21 );
			REFD( 2 ) = dot( REFWC, W23 );
			REFD( 3 ) = dot( REFWC, WNORM );

			// Calculate view vector coords relative to the diffuser coordinate system
			VIEWVD( 1 ) = dot( VIEWVC, W21 );
			VIEWVD( 2 ) = dot( VIEWVC, W23 );
			VIEWVD( 3 ) = dot( VIEWVC, WNORM );

			U3 = Surface( IWin2 ).Vertex( 2 );
			U2 = Surface( IWin2 ).Vertex( 3 );

			if ( Surface( IWin2 ).Sides == 4 ) {
				// Vertices of window (numbered counter-clockwise starting
				// at upper left as viewed from inside of room)
				// Assumes original vertices are numbered counter-clockwise from
				// upper left as viewed from outside.
				U3 = Surface( IWin2 ).Vertex( 2 );
				U2 = Surface( IWin2 ).Vertex( 3 );
				U1 = Surface( IWin2 ).Vertex( 4 );
			} else if ( Surface( IWin2 ).Sides == 3 ) {
				U3 = Surface( IWin2 ).Vertex( 2 );
				U2 = Surface( IWin2 ).Vertex( 3 );
				U1 = Surface( IWin2 ).Vertex( 1 );
			}

			// Unit vectors from window vertex 2 to 1 and 2 to 3,
			// center point of window, and vector from ref pt to center of window
			U21 = U1 - U2;
			U23 = U3 - U2;
			HW = U21.magnitude();
			WW = U23.magnitude();
			if ( Surface( IWin2 ).Sides == 4 ) {
				WC = U2 + ( U23 + U21 ) / 2.0;
			} else if ( Surface( IWin2 ).Sides == 3 ) {
				WC = U2 + ( U23 + U21 ) / 3.0;
			}
			SurfaceWindow( IWin2 ).WinCenter = WC;
			// Unit vectors
			U21 /= HW;
			U23 /= WW;

			// Unit vector normal to dome (pointing away from TDD)
			// These are specific to the exterior.
			// NOTE:  Preserve WNORM for later in the code.
			WNORM2 = cross( U21, U23 ).normalize();

			// Azimuth and altitude of dome normal
			// These are specific to the exterior.
			SurfaceWindow( IWin2 ).Phi = std::asin( WNORM2( 3 ) );
			if ( std::abs( WNORM2( 1 ) ) > 1.0e-5 || std::abs( WNORM2( 2 ) ) > 1.0e-5 ) {
				SurfaceWindow( IWin2 ).Theta = std::atan2( WNORM2( 2 ), WNORM2( 1 ) );
			} else {
				SurfaceWindow( IWin2 ).Theta = 0.0;
			}

			// Calculate new virtual reference point coords relative to dome coord system
			// W21, W23, and WNORM2 are now the unit vectors for the dome coord system
			REFWC = REFD( 1 ) * U21 + REFD( 2 ) * U23 + REFD( 3 ) * WNORM2;
			RREF2 = WC - REFWC;

			// Calculate new virtual view vector coords relative to dome coord system
			VIEWVC2 = VIEWVD( 1 ) * U21 + VIEWVD( 2 ) * U23 + VIEWVD( 3 ) * WNORM2;

			// Copy several values from the diffuser so that DayltgInterReflectedIllum works correctly
			// These are specific to the interior.
			SurfaceWindow( IWin2 ).RhoCeilingWall = SurfaceWindow( IWin ).RhoCeilingWall;
			SurfaceWindow( IWin2 ).RhoFloorWall = SurfaceWindow( IWin ).RhoFloorWall;
			SurfaceWindow( IWin2 ).FractionUpgoing = SurfaceWindow( IWin ).FractionUpgoing;
			SurfaceWindow( IWin2 ).GlazedFrac = SurfaceWindow( IWin ).GlazedFrac;

		} else {
			// This is not a TDD:DIFFUSER.  Make sure nothing is messed up for a regular window.
			IWin2 = IWin;
			WNORM2 = WNORM;
			RREF2 = RREF;
			VIEWVC2 = VIEWVC;

			U2 = W2;
			U21 = W21;
			U23 = W23;
		}

		// Initialize bsdf daylighting coefficients here.  Only one time initialization
		if ( SurfaceWindow( IWin ).WindowModelType == WindowBSDFModel ) {
			if ( ! ComplexWind( IWin ).DaylightingInitialized ) {
				if ( CalledFrom == CalledForMapPoint ) {
					NRefPts = IllumMapCalc( MapNum ).TotalMapRefPoints;
				} else if ( CalledFrom == CalledForRefPoint ) {
					NRefPts = ZoneDaylight( ZoneNum ).TotalDaylRefPoints;
				}
				InitializeCFSDaylighting( ZoneNum, IWin, NWX, NWY, RREF, NRefPts, iRefPoint, CalledFrom, MapNum );
				//if ((WinEl == (NWX * NWY)).and.(CalledFrom == CalledForMapPoint).and.(NRefPts == iRefPoint)) then
				if ( ( CalledFrom == CalledForMapPoint ) && ( NRefPts == iRefPoint ) ) {
					ComplexWind( IWin ).DaylightingInitialized = true;
				}
			}
		}

		if ( ! DetailedSolarTimestepIntegration ) {
			// Initialize sky and sun components of direct illuminance (arrays EDIRSK, EDIRSU, EDIRSUdisk)
			// and average window luminance (arrays AVWLSK, AVWLSU, AVWLSUdisk), at ref pt.
			EDIRSK = 0.0;
			EDIRSU = 0.0;
			EDIRSUdisk = 0.0;
			AVWLSK = 0.0;
			AVWLSU = 0.0;
			AVWLSUdisk = 0.0;
		} else {
			EDIRSK( HourOfDay, {1,MaxSlatAngs + 1}, {1,4} ) = 0.0;
			EDIRSU( HourOfDay, {1,MaxSlatAngs + 1} ) = 0.0;
			EDIRSUdisk( HourOfDay, {1,MaxSlatAngs + 1} ) = 0.0;
			AVWLSK( HourOfDay, {1,MaxSlatAngs + 1}, {1,4} ) = 0.0;
			AVWLSU( HourOfDay, {1,MaxSlatAngs + 1} ) = 0.0;
			AVWLSUdisk( HourOfDay, {1,MaxSlatAngs + 1} ) = 0.0;
		}
		if ( CalledFrom == CalledForRefPoint ) {
			// Initialize solid angle subtended by window wrt ref pt
			// and solid angle weighted by glare position factor
			SurfaceWindow( IWin ).SolidAngAtRefPt( iRefPoint ) = 0.0;
			SurfaceWindow( IWin ).SolidAngAtRefPtWtd( iRefPoint ) = 0.0;
		} else if ( CalledFrom == CalledForMapPoint ) {
			// Initialize solid angle subtended by window wrt ref pt
			// and solid angle weighted by glare position factor
//			if ( MapWindowSolidAngAtRefPt.present() ) MapWindowSolidAngAtRefPt = 0.0; //Inactive
			MapWindowSolidAngAtRefPtWtd = 0.0;
		}
		// Area of window element
		if ( is_Rectangle ) {
			DAXY = DWX * DWY;
		} else if ( is_Triangle ) {
			SinCornerAng = std::sqrt( 1.0 - pow_2( dot( W21, W23 ) ) );
			DAXY = DWX * DWY * SinCornerAng;
		}

	}

	void
	FigureDayltgCoeffsAtPointsForWindowElements(
		int const ZoneNum,
		int const iRefPoint,
		int const loopwin,
		int const CalledFrom, // indicate  which type of routine called this routine
		int const WinEl, // Current window element number
		int const IWin,
		int const IWin2,
		int const iXelement,
		int const iYelement,
		Real64 & SkyObstructionMult,
		Vector3< Real64 > const & W2, // Second vertex of window
		Vector3< Real64 > const & W21, // Vector from window vertex 2 to window vertex 1
		Vector3< Real64 > const & W23, // Vector from window vertex 2 to window vertex 3
		Vector3< Real64 > const & RREF, // Location of a reference point in absolute coordinate system
		int const NWYlim, // For triangle, largest NWY for a given IX
		Vector3< Real64 > const & VIEWVC2, // Virtual view vector in absolute coordinate system
		Real64 const DWX, // Horizontal dimension of window element (m)
		Real64 const DWY, // Vertical dimension of window element (m)
		Real64 const DAXY, // Area of window element
		Vector3< Real64 > const & U2, // Second vertex of window for TDD:DOME (if exists)
		Vector3< Real64 > const & U23, // Vector from window vertex 2 to window vertex 3 for TDD:DOME (if exists)
		Vector3< Real64 > const & U21, // Vector from window vertex 2 to window vertex 1 for TDD:DOME (if exists)
		Vector3< Real64 > & RWIN, // Center of a window element for TDD:DOME (if exists) in abs coord sys
		Vector3< Real64 > & RWIN2, // Center of a window element for TDD:DOME (if exists) in abs coord sys
		Vector3< Real64 > & Ray, // Unit vector along ray from reference point to window element
		Real64 & PHRAY, // Altitude of ray from reference point to window element (radians)
		int & LSHCAL, // Interior shade calculation flag:  0=not yet calculated, 1=already calculated
		Real64 & COSB, // Cosine of angle between window outward normal and ray from reference point to window element
		Real64 & ObTrans, // Product of solar transmittances of exterior obstructions hit by ray
		Real64 & TVISB, // Visible transmittance of window for COSB angle of incidence (times light well
		Real64 & DOMEGA, // Solid angle subtended by window element wrt reference point (steradians)
		Real64 & THRAY, // Azimuth of ray from reference point to window element (radians)
		bool & hitIntObs, // True iff interior obstruction hit
		bool & hitExtObs, // True iff ray from ref pt to ext win hits an exterior obstruction
		Vector3< Real64 > const & WNORM2, // Unit vector normal to window
		int const ExtWinType, // Exterior window type (InZoneExtWin, AdjZoneExtWin, NotInOrAdjZoneExtWin)
		int const IConst, // Construction counter
		Vector3< Real64 > const & RREF2, // Location of virtual reference point in absolute coordinate system
		bool const is_Triangle,
		Real64 & TVISIntWin, // Visible transmittance of int win at COSBIntWin for light from ext win
		Real64 & TVISIntWinDisk, // Visible transmittance of int win at COSBIntWin for sun
		Optional_int_const MapNum,
//		Optional< Real64 > MapWindowSolidAngAtRefPt, //Inactive
		Optional< Real64 > MapWindowSolidAngAtRefPtWtd
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   November 2012, refactor from legacy code by Fred Winklemann
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// collect code to do calculations for each window element for daylighting coefficients

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// switch as need to serve both reference points and map points based on calledFrom

		// Using/Aliasing
		using DaylightingDevices::TransTDD;
		using DaylightingDevices::FindTDDPipe;
		using General::POLYF;
		using namespace Vectors;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// from reference point through a window element
		//   efficiency, if appropriate)

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 DIS; // Distance between reference point and center of window element (m)
		Real64 DAXY1; // For triangle, area of window element at end of column
		Real64 POSFAC; // Position factor for a window element / ref point / view vector combination
		Real64 RR; // Distance from ref point to intersection of view vector
		//  and plane normal to view vector and window element (m)
		Real64 ASQ; // Square of distance from above intersection to window element (m2)
		Real64 YD; // Vertical displacement of window element wrt ref point
		Real64 XR; // Horizontal displacement ratio
		Real64 YR; // Vertical displacement ratio

		int IntWinHitNum; // Surface number of interior window that is intersected
		bool hitIntWin; // Ray from ref pt passes through interior window
		int PipeNum; // TDD pipe object number
		int IntWin; // Interior window surface index
		static Vector3< Real64 > HitPtIntWin; // Intersection point on an interior window for ray from ref pt to ext win (m)
		Real64 COSBIntWin; // Cos of angle between int win outward normal and ray betw ref pt and
		//  exterior window element or between ref pt and sun

		Real64 Alfa; // Intermediate variable
		Real64 Beta; // Intermediate variable
		Real64 HorDis; // Distance between ground hit point and proj'n of center
		//  of window element onto ground (m)
		static Vector3< Real64 > GroundHitPt; // Coordinates of point that ray hits ground (m)
		static Vector3< Real64 > URay; // Unit vector in (Phi,Theta) direction
		static Vector3< Real64 > ObsHitPt; // Coordinates of hit point on an obstruction (m)

		// Local complex fenestration variables
		int CplxFenState; // Current complex fenestration state
		int NReflSurf; // Number of blocked beams for complex fenestration
		int ICplxFen; // Complex fenestration counter
		int RayIndex;
		static Vector3< Real64 > RayVector;
		Real64 TransBeam; // Obstructions transmittance for incoming BSDF rays (temporary variable)

		++LSHCAL;
		SkyObstructionMult = 1.0;

		// Center of win element in absolute coord sys
		RWIN = W2 + ( double( iXelement ) - 0.5 ) * W23 * DWX + ( double( iYelement ) - 0.5 ) * W21 * DWY;

		// Center of win element on TDD:DOME in absolute coord sys
		// If no TDD, RWIN2 = RWIN
		RWIN2 = U2 + ( double( iXelement ) - 0.5 ) * U23 * DWX + ( double( iYelement ) - 0.5 ) * U21 * DWY;

		// Distance between ref pt and window element
		DIS = distance( RWIN, RREF );

		// Unit vector along ray from ref pt to element
		Ray = ( RWIN - RREF ) / DIS;

		// Cosine of angle between ray and window outward normal
		COSB = dot( WNORM2, Ray );

		// If COSB > 0, direct light from window can reach ref pt. Otherwise go to loop
		// over sun position and calculate inter-reflected component of illuminance
		if ( COSB > 0.0 ) {
			// Azimuth (-pi to pi) and altitude (-pi/2 to pi/2) of ray. Azimuth = 0 is along east.
			PHRAY = std::asin( Ray( 3 ) );
			if ( std::abs( Ray( 1 ) ) > 1.0e-5 || std::abs( Ray( 2 ) ) > 1.0e-5 ) {
				THRAY = std::atan2( Ray( 2 ), Ray( 1 ) );
			} else {
				THRAY = 0.0;
			}

			// Solid angle subtended by element wrt ref pt.
			DAXY1 = DAXY;
			// For triangle, at end of Y column only one half of parallelopiped's area contributes
			if ( is_Triangle && iYelement == NWYlim ) DAXY1 = 0.5 * DAXY;
			DOMEGA = DAXY1 * COSB / ( DIS * DIS );

			// Calculate position factor (used in glare calculation) for this
			// win element / ref pt / view-vector combination
			POSFAC = 0.0;

			// Distance from ref pt to intersection of view vector and plane
			// normal to view vector containing the window element

			RR = DIS * dot( Ray, VIEWVC2 );
			if ( RR > 0.0 ) {
				// Square of distance from above intersection point to win element
				ASQ = DIS * DIS - RR * RR;
				// Vertical displacement of win element wrt ref pt
				YD = RWIN2( 3 ) - RREF2( 3 );
				// Horizontal and vertical displacement ratio and position factor
				XR = std::sqrt( std::abs( ASQ - YD * YD ) ) / RR;
				YR = std::abs( YD / RR );
				POSFAC = DayltgGlarePositionFactor( XR, YR );
			}

			hitIntObs = false;
			IntWinHitNum = 0;
			hitIntWin = false;
			TVISIntWinDisk = 0.0; // Init Value
			TVISIntWin = 0.0;

			if ( SurfaceWindow( IWin ).OriginalClass == SurfaceClass_TDD_Diffuser ) {
				// Look up the TDD:DOME object
				PipeNum = FindTDDPipe( IWin );
				// Unshaded visible transmittance of TDD for a single ray from sky/ground element
				TVISB = TransTDD( PipeNum, COSB, VisibleBeam ) * SurfaceWindow( IWin ).GlazedFrac;

			} else { // Regular window
				if ( SurfaceWindow( IWin ).WindowModelType != WindowBSDFModel ) {
					// Vis trans of glass for COSB incidence angle
					TVISB = POLYF( COSB, Construct( IConst ).TransVisBeamCoef ) * SurfaceWindow( IWin ).GlazedFrac * SurfaceWindow( IWin ).LightWellEff;
				} else {
					// Complex fenestration needs to use different equation for visible transmittance.  That will be calculated later
					// in the code since it depends on different incoming directions.  For now, just put zero to differentiate from
					// regular windows
					TVISB = 0.0;
				}
				if ( ExtWinType == AdjZoneExtWin ) {
					// Does ray pass through an interior window in zone (ZoneNum) containing the ref point?
					for ( IntWin = Zone( ZoneNum ).SurfaceFirst; IntWin <= Zone( ZoneNum ).SurfaceLast; ++IntWin ) {
						if ( Surface( IntWin ).Class == SurfaceClass_Window && Surface( IntWin ).ExtBoundCond >= 1 ) {
							if ( Surface( Surface( IntWin ).ExtBoundCond ).Zone == Surface( IWin ).Zone ) {
								PierceSurface( IntWin, RREF, Ray, HitPtIntWin, hitIntWin );
								if ( hitIntWin ) {
									IntWinHitNum = IntWin;
									COSBIntWin = dot( Surface( IntWin ).OutNormVec, Ray );
									if ( COSBIntWin <= 0.0 ) {
										hitIntWin = false;
										IntWinHitNum = 0;
										continue;
									}
									TVISIntWin = POLYF( COSBIntWin, Construct( Surface( IntWin ).Construction ).TransVisBeamCoef );
									TVISB *= TVISIntWin;
									break; // Ray passes thru interior window; exit from DO loop
								}
							}
						}
					} // End of loop over surfaces in zone ZoneNum

					if ( !hitIntWin ) {
						// Ray does not pass through an int win in ZoneNum. Therefore, it hits the opaque part
						// of a surface between ref point in ZoneNum and ext win element in adjacent zone.
						hitIntObs = true;
					}
				} // End of check if this is an ext win in an adjacent zone
			} // End of check if TDD:Diffuser or regular exterior window or complex fenestration

			// Check for interior obstructions
			if ( ExtWinType == InZoneExtWin && !hitIntObs ) {
				// Check for obstruction between reference point and window element
				// Returns hitIntObs = true iff obstruction is hit
				// (Example of interior obstruction is a wall in an L-shaped room that lies
				// between reference point and window.)
				DayltgHitInteriorObstruction( IWin, RREF, RWIN, hitIntObs );
			}

			if ( ExtWinType == AdjZoneExtWin && IntWinHitNum > 0 && !hitIntObs ) {
				// Check for obstruction between ref point and interior window through which ray passes
				DayltgHitInteriorObstruction( IntWinHitNum, RREF, HitPtIntWin, hitIntObs );
				if ( !hitIntObs ) {
					// Check for obstruction between intersection point on int window and ext win element
					DayltgHitBetWinObstruction( IntWinHitNum, IWin, HitPtIntWin, RWIN, hitIntObs );
				}
			}
			if ( CalledFrom == CalledForRefPoint ) {
				if ( !hitIntObs ) {
					if ( ExtWinType == InZoneExtWin || ( ExtWinType == AdjZoneExtWin && hitIntWin ) ) {
						// Increment solid angle subtended by portion of window above ref pt
						SurfaceWindow( IWin ).SolidAngAtRefPt( iRefPoint ) += DOMEGA;
						ZoneDaylight( ZoneNum ).SolidAngAtRefPt( loopwin, iRefPoint ) += DOMEGA;
						// Increment position-factor-modified solid angle
						SurfaceWindow( IWin ).SolidAngAtRefPtWtd( iRefPoint ) += DOMEGA * POSFAC;
						ZoneDaylight( ZoneNum ).SolidAngAtRefPtWtd( loopwin, iRefPoint ) += DOMEGA * POSFAC;
					}
				}
			} else if ( CalledFrom == CalledForMapPoint ) {
				if ( !hitIntObs ) {
					if ( ExtWinType == InZoneExtWin || ( ExtWinType == AdjZoneExtWin && hitIntWin ) ) {
//						if ( MapWindowSolidAngAtRefPt.present() ) MapWindowSolidAngAtRefPt += DOMEGA; //Inactive
						IllumMapCalc( MapNum ).SolidAngAtMapPt( loopwin, iRefPoint ) += DOMEGA;
						MapWindowSolidAngAtRefPtWtd += DOMEGA * POSFAC;
						IllumMapCalc( MapNum ).SolidAngAtMapPtWtd( loopwin, iRefPoint ) += DOMEGA * POSFAC;
					}
				}
			}
			if ( hitIntObs ) ObTrans = 0.0;

			hitExtObs = false;
			if ( !hitIntObs ) {
				// No interior obstruction was hit.
				// Check for exterior obstructions between window element and sky/ground.
				// Get product of transmittances of obstructions hit by ray.
				// ObTrans = 1.0 will be returned if no exterior obstructions are hit.

				if ( SurfaceWindow( IWin ).WindowModelType != WindowBSDFModel ) {
					// the IHR (now HourOfDay) here is/was not correct, this is outside of hour loop
					// the hour is used to query schedule for transmission , not sure what to do
					// it will work for detailed and never did work correctly before.
					DayltgHitObstruction( HourOfDay, IWin2, RWIN2, Ray, ObTrans );
					if ( ObTrans < 1.0 ) hitExtObs = true;
				} else {
					// Transmittance from exterior obstruction surfaces is calculated here. This needs to be done for each timestep
					// in order to account for changes in exterior surface transmittances
					CplxFenState = SurfaceWindow( IWin ).ComplexFen.CurrentState;
					if ( CalledFrom == CalledForRefPoint ) {
						NReflSurf = ComplexWind( IWin ).DaylghtGeom( CplxFenState ).RefPoint( iRefPoint ).NReflSurf( WinEl );
					} else {
						NReflSurf = ComplexWind( IWin ).DaylghtGeom( CplxFenState ).IlluminanceMap( iRefPoint, MapNum ).NReflSurf( WinEl );
					}
					for ( ICplxFen = 1; ICplxFen <= NReflSurf; ++ICplxFen ) {
						if ( CalledFrom == CalledForRefPoint ) {
							RayIndex = ComplexWind( IWin ).DaylghtGeom( CplxFenState ).RefPoint( iRefPoint ).RefSurfIndex( ICplxFen, WinEl );
						} else {
							RayIndex = ComplexWind( IWin ).DaylghtGeom( CplxFenState ).IlluminanceMap( iRefPoint, MapNum ).RefSurfIndex( ICplxFen, WinEl );
						}
						RayVector = ComplexWind( IWin ).Geom( CplxFenState ).sInc( RayIndex );
						// It will get product of all transmittances
						DayltgHitObstruction( HourOfDay, IWin, RWIN, RayVector, TransBeam );
						// IF (TransBeam > 0.0d0) ObTrans = TransBeam
						if ( CalledFrom == CalledForRefPoint ) {
							ComplexWind( IWin ).DaylghtGeom( CplxFenState ).RefPoint( iRefPoint ).TransOutSurf( ICplxFen, WinEl ) = TransBeam;
						} else {
							ComplexWind( IWin ).DaylghtGeom( CplxFenState ).IlluminanceMap( iRefPoint, MapNum ).TransOutSurf( ICplxFen, WinEl ) = TransBeam;
						}
					}
					// This will avoid obstruction multiplier calculations for non-CFS window
					ObTrans = 0.0;
				}
			}

			if ( CalcSolRefl && PHRAY < 0.0 && ObTrans > 1.0e-6 ) {
				// Calculate effect of obstructions on shading of sky diffuse reaching the ground point hit
				// by the ray. This effect is given by the ratio SkyObstructionMult =
				// (obstructed sky diffuse at ground point)/(unobstructed sky diffuse at ground point).
				// This ratio is calculated for an isotropic sky.
				// Ground point hit by the ray:
				Alfa = std::acos( -Ray( 3 ) );
				Beta = std::atan2( Ray( 2 ), Ray( 1 ) );
				HorDis = ( RWIN2( 3 ) - GroundLevelZ ) * std::tan( Alfa );
				GroundHitPt( 3 ) = GroundLevelZ;
				GroundHitPt( 1 ) = RWIN2( 1 ) + HorDis * std::cos( Beta );
				GroundHitPt( 2 ) = RWIN2( 2 ) + HorDis * std::sin( Beta );

				SkyObstructionMult = CalcObstrMultiplier( GroundHitPt, AltAngStepsForSolReflCalc, AzimAngStepsForSolReflCalc );
			} // End of check if solar reflection calculation is in effect

		} // End of check if COSB > 0

	}

	void
	InitializeCFSDaylighting(
		int const ZoneNum, // Current zone number
		int const IWin, // Complex fenestration number
		int const NWX, // Number of horizontal divisions
		int const NWY, // Number of vertical divisions
		Vector3< Real64 > const & RefPoint, // reference point coordinates
		int const NRefPts, // Number of reference points
		int const iRefPoint, // Reference points counter
		int const CalledFrom,
		Optional_int_const MapNum
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Simon Vidanovic
		//       DATE WRITTEN   April 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// For incoming BSDF window direction calucates wheter bin is coming from sky, ground or reflected surface.
		// Routine also calculates intersection points with ground and exterior reflection surfaces.

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace Vectors;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		int NumOfWinEl; // Number of window elements
		int CurFenState; // Current fenestration state

		Real64 DWX; // Window element width
		Real64 DWY; // Window element height
		Real64 WinElArea; // Window element area

		// window coordinates and vectors
		static Vector3< Real64 > W1;
		static Vector3< Real64 > W2;
		static Vector3< Real64 > W3;
		static Vector3< Real64 > W21;
		static Vector3< Real64 > W23;

		// window elements counters
		// integer :: IX ! horizontal elements
		// integer :: IY ! vertical elements

		// TYPE(Vector) :: HitPt ! surface hit point
		// REAL(r64), dimension(3) :: RWin ! window element center point (same as centroid)
		static Vector3< Real64 > WNorm; // unit vector from window (point towards outside)

		//REAL(r64) :: DotProd     !Temporary variable for manipulating dot product .dot.
		//REAL(r64) :: LeastHitDsq  ! dist^2 from window element center to hit point
		//REAL(r64) :: HitDsq
		//REAL(r64), dimension(3)  ::  V    !vector array
		//REAL(r64), dimension(3) :: GroundHitPt    ! Coordinates of point that ray hits ground (m)
		int NBasis; // number of incident basis directions for current state
		int NTrnBasis; // number of outgoing basis directions for current state

		// reference point variables
		// REAL(r64), dimension(3) :: RefPoint ! reference point
		static Vector3< Real64 > Ray; // vector along ray from window to reference point
		static Vector3< Real64 > RayNorm; // unit vector along ray from window to reference point
		static Vector3< Real64 > InterPoint; // Intersection point

		// Position factor variables
		Real64 AZVIEW; // Azimuth of view vector

		// Object Data
		BSDFDaylghtPosition elPos; // altitude and azimuth of intersection element
		Vector Vec; // temporary vector variable

		NumOfWinEl = NWX * NWY;

		DWX = Surface( IWin ).Width / NWX;
		DWY = Surface( IWin ).Height / NWY;

		AZVIEW = ( ZoneDaylight( ZoneNum ).ViewAzimuthForGlare + Zone( ZoneNum ).RelNorth + BuildingAzimuth + BuildingRotationAppendixG ) * DegToRadians;

		// Perform necessary calculations for window coordinates and vectors.  This will be used to calculate centroids for
		// each window element
		W1 = 0.0;
		W2 = 0.0;
		W3 = 0.0;

		if ( Surface( IWin ).Sides == 4 ) {
			W3 = Surface( IWin ).Vertex( 2 );
			W2 = Surface( IWin ).Vertex( 3 );
			W1 = Surface( IWin ).Vertex( 4 );
		} else if ( Surface( IWin ).Sides == 3 ) {
			W3 = Surface( IWin ).Vertex( 2 );
			W2 = Surface( IWin ).Vertex( 3 );
			W1 = Surface( IWin ).Vertex( 1 );
		}

		W21 = W1 - W2;
		W23 = W3 - W2;

		W21 /= Surface( IWin ).Height;
		W23 /= Surface( IWin ).Width;

		WNorm = Surface( IWin ).lcsz;

		WinElArea = DWX * DWY;
		if ( Surface( IWin ).Sides == 3 ) {
			WinElArea *= std::sqrt( 1.0 - pow_2( dot( W21, W23 ) ) );
		}

		if ( CalledFrom == CalledForMapPoint ) {

			if ( ! allocated( ComplexWind( IWin ).IlluminanceMap ) ) {
				ComplexWind( IWin ).IlluminanceMap.allocate( NRefPts, TotIllumMaps );
			}

			AllocateForCFSRefPointsGeometry( ComplexWind( IWin ).IlluminanceMap( iRefPoint, MapNum ), NumOfWinEl );

		} else if ( CalledFrom == CalledForRefPoint ) {
			if ( ! allocated( ComplexWind( IWin ).RefPoint ) ) {
				ComplexWind( IWin ).RefPoint.allocate( NRefPts );
			}

			AllocateForCFSRefPointsGeometry( ComplexWind( IWin ).RefPoint( iRefPoint ), NumOfWinEl );
		}

		//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		//! Allocation for each complex fenestration state reference points
		//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		if ( ! allocated( ComplexWind( IWin ).DaylghtGeom ) ) {
			ComplexWind( IWin ).DaylghtGeom.allocate( ComplexWind( IWin ).NumStates );
		}

		// Calculation needs to be performed for each state
		for ( CurFenState = 1; CurFenState <= ComplexWind( IWin ).NumStates; ++CurFenState ) {
			NBasis = ComplexWind( IWin ).Geom( CurFenState ).Inc.NBasis;
			NTrnBasis = ComplexWind( IWin ).Geom( CurFenState ).Trn.NBasis;

			if ( CalledFrom == CalledForMapPoint ) {
				if ( TotIllumMaps > 0 ) {
					// illuminance map for each state
					if ( ! allocated( ComplexWind( IWin ).DaylghtGeom( CurFenState ).IlluminanceMap ) ) {
						ComplexWind( IWin ).DaylghtGeom( CurFenState ).IlluminanceMap.allocate( NRefPts, TotIllumMaps );
					}

					AllocateForCFSRefPointsState( ComplexWind( IWin ).DaylghtGeom( CurFenState ).IlluminanceMap( iRefPoint, MapNum ), NumOfWinEl, NBasis, NTrnBasis );

					InitializeCFSStateData( ComplexWind( IWin ).DaylghtGeom( CurFenState ).IlluminanceMap( iRefPoint, MapNum ), ComplexWind( IWin ).IlluminanceMap( iRefPoint, MapNum ), ZoneNum, IWin, RefPoint, CurFenState, NBasis, NTrnBasis, AZVIEW, NWX, NWY, W2, W21, W23, DWX, DWY, WNorm, WinElArea, CalledFrom, MapNum );
				}

			} else if ( CalledFrom == CalledForRefPoint ) {
				if ( ! allocated( ComplexWind( IWin ).DaylghtGeom( CurFenState ).RefPoint ) ) {
					ComplexWind( IWin ).DaylghtGeom( CurFenState ).RefPoint.allocate( NRefPts );
				}

				AllocateForCFSRefPointsState( ComplexWind( IWin ).DaylghtGeom( CurFenState ).RefPoint( iRefPoint ), NumOfWinEl, NBasis, NTrnBasis );

				InitializeCFSStateData( ComplexWind( IWin ).DaylghtGeom( CurFenState ).RefPoint( iRefPoint ), ComplexWind( IWin ).RefPoint( iRefPoint ), ZoneNum, IWin, RefPoint, CurFenState, NBasis, NTrnBasis, AZVIEW, NWX, NWY, W2, W21, W23, DWX, DWY, WNorm, WinElArea, CalledFrom, MapNum );
			}
		}

	}

	void
	InitializeCFSStateData(
		BSDFRefPoints & StateRefPoint,
		BSDFRefPointsGeomDescr & DaylghtGeomDescr,
		int const EP_UNUSED( ZoneNum ), // Current zone number
		int const iWin,
		Vector3< Real64 > const & RefPoint, // reference point
		int const CurFenState,
		int const NBasis,
		int const NTrnBasis,
		Real64 const AZVIEW,
		int const NWX,
		int const NWY,
		Vector3< Real64 > const & W2,
		Vector3< Real64 > const & W21,
		Vector3< Real64 > const & W23,
		Real64 const DWX,
		Real64 const DWY,
		Vector3< Real64 > const & WNorm, // unit vector from window (point towards outside)
		Real64 const WinElArea,
		int const EP_UNUSED( CalledFrom ),
		Optional_int_const EP_UNUSED( MapNum )
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Simon Vidanovic
		//       DATE WRITTEN   June 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Initialize daylight state data for current

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace Vectors;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		//integer, intent(in) :: NRefPt

		// SUBROUTINE LOCAL VARIABLES
		int curWinEl;
		int IRay;
		bool hit;
		int TotHits;
		int JSurf;
		Real64 DotProd; // Temporary variable for manipulating dot product .dot.
		int NSky;
		int NGnd;
		int NReflSurf;
		int MaxTotHits;
		int IX;
		int IY;
		static Vector3< Real64 > RWin; // window element center point (same as centroid)
		static Vector3< Real64 > V; // vector array
		Real64 LeastHitDsq; // dist^2 from window element center to hit point
		Real64 HitDsq;
		Real64 TransRSurf;
		int I;
		int J;
		static Vector3< Real64 > GroundHitPt; // Coordinates of point that ray hits ground (m)

		// Refrence point data
		// integer :: iRefPoint

		// temporary arrays for surfaces
		// Each complex fenestration state can have different number of basis elements
		// This is the reason for making these temporary arrays local
		Array1D_int TmpSkyInd( NBasis, 0 ); // Temporary sky index list
		Array1D_int TmpGndInd( NBasis, 0 ); // Temporary gnd index list
		Array1D< Real64 > TmpGndMultiplier( NBasis, 0.0 ); // Temporary ground obstruction multiplier
		Array1D_int TmpRfSfInd( NBasis, 0 ); // Temporary RefSurfIndex
		Array1D_int TmpRfRyNH( NBasis, 0 ); // Temporary RefRayNHits
		Array2D_int TmpHSurfNo( TotSurfaces, NBasis, 0 ); // Temporary HitSurfNo
		Array2D< Real64 > TmpHSurfDSq( TotSurfaces, NBasis, 0.0 ); // Temporary HitSurfDSq

		// Object Data
		Vector Centroid; // current window element centroid
		Vector HitPt; // surface hit point
		Array1D< Vector > TmpGndPt( NBasis, Vector( 0.0, 0.0, 0.0 ) ); // Temporary ground intersection list
		Array2D< Vector > TmpHitPt( TotSurfaces, NBasis, Vector( 0.0, 0.0, 0.0 ) ); // Temporary HitPt

		// find if reference point belongs to light tube of outgoing bsdf direction.  This works for entire window and not window
		// elements.
		// initialization for each reference point
		//do iRefPoint = 1, NRefPt
		//if (CalledFrom == CalledForRefPoint) then
		//  RefPoint = ZoneDaylight(ZoneNum)%DaylRefPtAbsCoord(iRefPoint, 1:3)
		//else
		//  RefPoint = IllumMapCalc(MapNum)%MapRefPtAbsCoord(irefPoint, 1:3)
		//end if

		CFSRefPointPosFactor( RefPoint, StateRefPoint, iWin, CurFenState, NTrnBasis, AZVIEW );
		//end do

		curWinEl = 0;
		// loop through window elements. This will calculate sky, ground and reflection bins for each window element
		for ( IX = 1; IX <= NWX; ++IX ) {
			for ( IY = 1; IY <= NWY; ++IY ) {

				++curWinEl;

				// centroid coordinates for current window element
				Centroid = W2 + ( double( IX ) - 0.5 ) * W23 * DWX + ( double( IY ) - 0.5 ) * W21 * DWY;
				RWin = Centroid;

				//do iRefPoint = 1, NRefPt
				//if (CalledFrom == CalledForRefPoint) then
				//  RefPoint = ZoneDaylight(ZoneNum)%DaylRefPtAbsCoord(iRefPoint, 1:3)
				//else
				//  RefPoint = IllumMapCalc(MapNum)%MapRefPtAbsCoord(iRefPoint, 1:3)
				//end if

				CFSRefPointSolidAngle( RefPoint, RWin, WNorm, StateRefPoint, DaylghtGeomDescr, iWin, CurFenState, NTrnBasis, curWinEl, WinElArea );
				//end do

				NSky = 0;
				NGnd = 0;
				NReflSurf = 0;
				MaxTotHits = 0;
				// Calculation of potential surface obstruction for each incoming direction
				for ( IRay = 1; IRay <= NBasis; ++IRay ) {

					hit = false;
					TotHits = 0;
					for ( JSurf = 1; JSurf <= TotSurfaces; ++JSurf ) {
						// the following test will cycle on anything except exterior surfaces and shading surfaces
						if ( Surface( JSurf ).HeatTransSurf && Surface( JSurf ).ExtBoundCond != ExternalEnvironment ) continue;
						//  skip the base surface containing the window and any other subsurfaces of that surface
						if ( JSurf == Surface( iWin ).BaseSurf || Surface( JSurf ).BaseSurf == Surface( iWin ).BaseSurf ) continue;
						//  skip surfaces that face away from the window
						DotProd = dot( ComplexWind( iWin ).Geom( CurFenState ).sInc( IRay ), Surface( JSurf ).NewellSurfaceNormalVector );
						if ( DotProd >= 0 ) continue;
						PierceSurface( JSurf, Centroid, ComplexWind( iWin ).Geom( CurFenState ).sInc( IRay ), HitPt, hit );
						if ( !hit ) continue; // Miss: Try next surface
						if ( TotHits == 0 ) {
							// First hit for this ray
							TotHits = 1;
							++NReflSurf;
							TmpRfSfInd( NReflSurf ) = IRay;
							TmpRfRyNH( NReflSurf ) = 1;
							TmpHSurfNo( 1, NReflSurf ) = JSurf;
							TmpHitPt( 1, NReflSurf ) = HitPt;
							V = HitPt - Centroid; // vector array from window ctr to hit pt
							LeastHitDsq = V.magnitude_squared(); //dist^2 window ctr to hit pt
							TmpHSurfDSq( 1, NReflSurf ) = LeastHitDsq;
							if ( ! Surface( JSurf ).HeatTransSurf && Surface( JSurf ).SchedShadowSurfIndex != 0 ) {
								TransRSurf = 1.0; // If a shadowing surface may have a scheduled transmittance, treat it here as completely transparent
							} else {
								TransRSurf = 0.0;
							}
						} else {
							V = HitPt - Centroid;
							HitDsq = V.magnitude_squared();
							if ( HitDsq >= LeastHitDsq ) {
								if ( TransRSurf > 0.0 ) { // forget the new hit if the closer hit is opaque
									J = TotHits + 1;
									if ( TotHits > 1 ) {
										for ( I = 2; I <= TotHits; ++I ) {
											if ( HitDsq < TmpHSurfDSq( I, NReflSurf ) ) {
												J = I;
												break;
											}
										}
										if ( ! Surface( JSurf ).HeatTransSurf && Surface( JSurf ).SchedShadowSurfIndex == 0 ) {
											//  The new hit is opaque, so we can drop all the hits further away
											TmpHSurfNo( J, NReflSurf ) = JSurf;
											TmpHitPt( J, NReflSurf ) = HitPt;
											TmpHSurfDSq( J, NReflSurf ) = HitDsq;
											TotHits = J;
										} else {
											//  The new hit is scheduled (presumed transparent), so keep the more distant hits
											//     Note that all the hists in the list will be transparent except the last,
											//       which may be either transparent or opaque
											if ( TotHits >= J ) {
												for ( I = TotHits; I >= J; --I ) {
													TmpHSurfNo( I + 1, NReflSurf ) = TmpHSurfNo( I, NReflSurf );
													TmpHitPt( I + 1, NReflSurf ) = TmpHitPt( I, NReflSurf );
													TmpHSurfDSq( I + 1, NReflSurf ) = TmpHSurfDSq( I, NReflSurf );
												}
												TmpHSurfNo( J, NReflSurf ) = JSurf;
												TmpHitPt( J, NReflSurf ) = HitPt;
												TmpHSurfDSq( J, NReflSurf ) = HitDsq;
												++TotHits;
											}
										} // if (.NOT.Surface(JSurf)%HeatTransSurf .AND. Surface(JSurf)%SchedShadowSurfIndex == 0)  then
									} // if (TotHits > 1) then
								} // if (TransRSurf  > 0.0d0) then
							} else { // if (HitDsq >= LeastHitDsq) then
								//  A new closest hit.  If it is opaque, drop the current hit list,
								//    otherwise add it at the front
								LeastHitDsq = HitDsq;
								if ( ! Surface( JSurf ).HeatTransSurf && Surface( JSurf ).SchedShadowSurfIndex != 0 ) {
									TransRSurf = 1.0; // New closest hit is transparent, keep the existing hit list
									for ( I = TotHits; I >= 1; --I ) {
										TmpHSurfNo( I + 1, NReflSurf ) = TmpHSurfNo( I, NReflSurf );
										TmpHitPt( I + 1, NReflSurf ) = TmpHitPt( I, NReflSurf );
										TmpHSurfDSq( I + 1, NReflSurf ) = TmpHSurfDSq( I, NReflSurf );
										++TotHits;
									}
								} else {
									TransRSurf = 0.0; // New closest hit is opaque, drop the existing hit list
									TotHits = 1;
								}
								TmpHSurfNo( 1, NReflSurf ) = JSurf; // In either case the new hit is put in position 1
								TmpHitPt( 1, NReflSurf ) = HitPt;
								TmpHSurfDSq( 1, NReflSurf ) = LeastHitDsq;
							}
						}
					} // do JSurf = 1, TotSurfaces
					if ( TotHits <= 0 ) {
						//This ray reached the sky or ground unobstructed
						if ( ComplexWind( iWin ).Geom( CurFenState ).sInc( IRay ).z < 0.0 ) {
							//A ground ray
							++NGnd;
							TmpGndInd( NGnd ) = IRay;
							TmpGndPt( NGnd ).x = Centroid.x - ( ComplexWind( iWin ).Geom( CurFenState ).sInc( IRay ).x / ComplexWind( iWin ).Geom( CurFenState ).sInc( IRay ).z ) * Centroid.z;
							TmpGndPt( NGnd ).y = Centroid.y - ( ComplexWind( iWin ).Geom( CurFenState ).sInc( IRay ).y / ComplexWind( iWin ).Geom( CurFenState ).sInc( IRay ).z ) * Centroid.z;
							TmpGndPt( NGnd ).z = 0.0;

							// for solar reflectance calculations, need to precalculate obstruction multipliers
							if ( CalcSolRefl ) {
								GroundHitPt = TmpGndPt( NGnd );
								TmpGndMultiplier( NGnd ) = CalcObstrMultiplier( GroundHitPt, AltAngStepsForSolReflCalc, AzimAngStepsForSolReflCalc );
							}
						} else {
							//A sky ray
							++NSky;
							TmpSkyInd( NSky ) = IRay;
						}
					} else {
						//Save the number of hits for this ray
						TmpRfRyNH( NReflSurf ) = TotHits;
					}
					MaxTotHits = max( MaxTotHits, TotHits );
				} // do IRay = 1, ComplexWind(IWin)%Geom(CurFenState)%Inc%NBasis

				// Fill up state data for current window element data
				StateRefPoint.NSky( curWinEl ) = NSky;
				StateRefPoint.SkyIndex( {1,NSky}, curWinEl ) = TmpSkyInd( {1,NSky} );

				StateRefPoint.NGnd( curWinEl ) = NGnd;
				StateRefPoint.GndIndex( {1,NGnd}, curWinEl ) = TmpGndInd( {1,NGnd} );
				StateRefPoint.GndPt( {1,NGnd}, curWinEl ) = TmpGndPt( {1,NGnd} );
				StateRefPoint.GndObstrMultiplier( {1,NGnd}, curWinEl ) = TmpGndMultiplier( {1,NGnd} );

				StateRefPoint.NReflSurf( curWinEl ) = NReflSurf;
				StateRefPoint.RefSurfIndex( {1,NReflSurf}, curWinEl ) = TmpRfSfInd( {1,NReflSurf} );
				StateRefPoint.RefRayNHits( {1,NReflSurf}, curWinEl ) = TmpRfRyNH( {1,NReflSurf} );
				StateRefPoint.HitSurfNo( {1,MaxTotHits}, {1,NReflSurf}, curWinEl ) = TmpHSurfNo( {1,MaxTotHits}, {1,NReflSurf} );
				StateRefPoint.HitSurfDSq( {1,MaxTotHits}, {1,NReflSurf}, curWinEl ) = TmpHSurfDSq( {1,MaxTotHits}, {1,NReflSurf} );
				StateRefPoint.HitPt( {1,MaxTotHits}, {1,NReflSurf}, curWinEl ) = TmpHitPt( {1,MaxTotHits}, {1,NReflSurf} );
			} // do IY = 1, NWY
		} // do IX = 1, NWX

	}

	void
	AllocateForCFSRefPointsState(
		BSDFRefPoints & StateRefPoint,
		int const NumOfWinEl,
		int const NBasis,
		int const NTrnBasis
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Simon Vidanovic
		//       DATE WRITTEN   June 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Memory allocation for complex fenestration systems reference points geometry

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace Vectors;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLES

		if ( ! allocated( StateRefPoint.NSky ) ) {
			StateRefPoint.NSky.allocate( NumOfWinEl );
			StateRefPoint.NSky = 0;
		}

		if ( ! allocated( StateRefPoint.SkyIndex ) ) {
			StateRefPoint.SkyIndex.allocate( NBasis, NumOfWinEl );
			StateRefPoint.SkyIndex = 0;
		}

		if ( ! allocated( StateRefPoint.NGnd ) ) {
			StateRefPoint.NGnd.allocate( NumOfWinEl );
			StateRefPoint.NGnd = 0;
		}

		if ( ! allocated( StateRefPoint.GndIndex ) ) {
			StateRefPoint.GndIndex.allocate( NBasis, NumOfWinEl );
			StateRefPoint.GndIndex = 0;
		}

		if ( ! allocated( StateRefPoint.GndPt ) ) {
			StateRefPoint.GndPt.allocate( NBasis, NumOfWinEl );
			StateRefPoint.GndPt = Vector( 0.0, 0.0, 0.0 );
		}

		if ( ! allocated( StateRefPoint.GndObstrMultiplier ) ) {
			StateRefPoint.GndObstrMultiplier.allocate( NBasis, NumOfWinEl );
			StateRefPoint.GndObstrMultiplier = 0.0;
		}

		if ( ! allocated( StateRefPoint.NReflSurf ) ) {
			StateRefPoint.NReflSurf.allocate( NumOfWinEl );
			StateRefPoint.NReflSurf = 0;
		}

		if ( ! allocated( StateRefPoint.RefSurfIndex ) ) {
			StateRefPoint.RefSurfIndex.allocate( NBasis, NumOfWinEl );
			StateRefPoint.RefSurfIndex = 0;
		}

		if ( ! allocated( StateRefPoint.TransOutSurf ) ) {
			StateRefPoint.TransOutSurf.allocate( NBasis, NumOfWinEl );
			StateRefPoint.TransOutSurf = 1.0;
		}

		if ( ! allocated( StateRefPoint.RefRayNHits ) ) {
			StateRefPoint.RefRayNHits.allocate( NBasis, NumOfWinEl );
			StateRefPoint.RefRayNHits = 0;
		}

		if ( ! allocated( StateRefPoint.HitSurfNo ) ) {
			StateRefPoint.HitSurfNo.allocate( TotSurfaces, NBasis, NumOfWinEl );
			StateRefPoint.HitSurfNo = 0;
		}

		if ( ! allocated( StateRefPoint.HitSurfDSq ) ) {
			StateRefPoint.HitSurfDSq.allocate( TotSurfaces, NBasis, NumOfWinEl );
			StateRefPoint.HitSurfDSq = 0.0;
		}

		if ( ! allocated( StateRefPoint.HitPt ) ) {
			StateRefPoint.HitPt.allocate( TotSurfaces, NBasis, NumOfWinEl );
			StateRefPoint.HitPt = Vector( 0.0, 0.0, 0.0 );
		}

		if ( ! allocated( StateRefPoint.RefPointIndex ) ) {
			StateRefPoint.RefPointIndex.allocate( NumOfWinEl );
			StateRefPoint.RefPointIndex = 0;
		}

		if ( ! allocated( StateRefPoint.RefPointIntersection ) ) {
			StateRefPoint.RefPointIntersection.allocate( NTrnBasis );
			StateRefPoint.RefPointIntersection = false;
		}

		if ( ! allocated( StateRefPoint.RefPtIntPosFac ) ) {
			StateRefPoint.RefPtIntPosFac.allocate( NTrnBasis );
			StateRefPoint.RefPtIntPosFac = 0.0;
		}

	}

	void
	AllocateForCFSRefPointsGeometry(
		BSDFRefPointsGeomDescr & RefPointsGeomDescr,
		int const NumOfWinEl
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Simon Vidanovic
		//       DATE WRITTEN   June 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Memory allocation for complex fenestration systems reference points geometry

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace Vectors;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		//integer, intent(in) :: NRefPts

		// SUBROUTINE LOCAL VARIABLES

		if ( ! allocated( RefPointsGeomDescr.SolidAngle ) ) {
			RefPointsGeomDescr.SolidAngle.allocate( NumOfWinEl );
			RefPointsGeomDescr.SolidAngle = 0.0;
		}

		if ( ! allocated( RefPointsGeomDescr.SolidAngleVec ) ) {
			RefPointsGeomDescr.SolidAngleVec.allocate( NumOfWinEl );
			RefPointsGeomDescr.SolidAngleVec = Vector( 0.0, 0.0, 0.0 );
		}

	}

	void
	CFSRefPointSolidAngle(
		Vector3< Real64 > const & RefPoint,
		Vector3< Real64 > const & RWin,
		Vector3< Real64 > const & WNorm,
		BSDFRefPoints & RefPointMap,
		BSDFRefPointsGeomDescr & RefPointGeomMap,
		int const iWin,
		int const CurFenState,
		int const NTrnBasis,
		int const curWinEl,
		Real64 const WinElArea
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Simon Vidanovic
		//       DATE WRITTEN   June 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculate position factor for given reference point.

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace Vectors;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		//integer, intent(in) :: iRefPoint

		// SUBROUTINE LOCAL VARIABLES
		static Vector3< Real64 > Ray;
		static Vector3< Real64 > RayNorm;
		static Vector3< Real64 > V;
		Real64 BestMatch;
		int iTrnRay;
		Real64 temp;
		Real64 Dist;
		Real64 CosB;

		// calculate vector from center of window element to the current reference point
		Ray = RefPoint - RWin;

		// figure out outgoing beam direction from current reference point
		BestMatch = 0.0;
		for ( iTrnRay = 1; iTrnRay <= NTrnBasis; ++iTrnRay ) {
			V = ComplexWind( iWin ).Geom( CurFenState ).sTrn( iTrnRay );
			temp = dot( Ray, V );
			if ( temp > BestMatch ) {
				BestMatch = temp;
				RefPointMap.RefPointIndex( curWinEl ) = iTrnRay;
			}
		}

		// calculate solid view angle
		Dist = Ray.magnitude();
		RayNorm = Ray / ( -Dist );
		RefPointGeomMap.SolidAngleVec( curWinEl ) = RayNorm;
		CosB = dot( WNorm, RayNorm );
		RefPointGeomMap.SolidAngle( curWinEl ) = WinElArea * CosB / ( Dist * Dist );

	}

	void
	CFSRefPointPosFactor(
		Vector3< Real64 > const & RefPoint,
		BSDFRefPoints & RefPointMap,
		int const iWin,
		int const CurFenState,
		int const NTrnBasis,
		Real64 const AZVIEW
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Simon Vidanovic
		//       DATE WRITTEN   June 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculate position factor for given reference point.

		// METHODOLOGY EMPLOYED:
		// <description>

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace Vectors;
		using WindowComplexManager::DaylghtAltAndAzimuth;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		//integer, intent(in) :: iRefPoint

		// SUBROUTINE LOCAL VARIABLES
		int iTrnRay;
		Real64 XR;
		Real64 YR;
		static Vector3< Real64 > V;
		bool hit;
		static Vector3< Real64 > InterPoint;

		// Object Data
		BSDFDaylghtPosition elPos; // altitude and azimuth of intersection element

		auto const & sTrn( ComplexWind( iWin ).Geom( CurFenState ).sTrn );
		for ( iTrnRay = 1; iTrnRay <= NTrnBasis; ++iTrnRay ) {
			V = sTrn( iTrnRay );
			V.negate();
			PierceSurface( iWin, RefPoint, V, InterPoint, hit );
			if ( hit ) {
				RefPointMap.RefPointIntersection( iTrnRay ) = true;

				elPos = DaylghtAltAndAzimuth( V );

				XR = std::tan( std::abs( PiOvr2 - AZVIEW - elPos.Azimuth ) + 0.001 );
				YR = std::tan( elPos.Altitude + 0.001 );
				RefPointMap.RefPtIntPosFac( iTrnRay ) = DayltgGlarePositionFactor( XR, YR );
			}
		}

	}

	Real64
	CalcObstrMultiplier(
		Vector3< Real64 > const & GroundHitPt, // Coordinates of point that ray hits ground (m)
		int const AltSteps, // Number of steps in altitude angle for solar reflection calc
		int const AzimSteps // Number of steps in azimuth angle of solar reflection calc
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Simon Vidanovic
		//       DATE WRITTEN   April 2013, refactor from legacy code by Fred Winklemann
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// collect code to do obstruction multiplier from ground point

		// METHODOLOGY EMPLOYED:
		// Send rays upward from hit point and see which ones are unobstructed and so go to sky.
		// Divide hemisphere centered at ground hit point into elements of altitude Phi and
		// azimuth Theta and create upward-going ground ray unit vector at each Phi,Theta pair.
		// Phi = 0 at the horizon; Phi = Pi/2 at the zenith.

		// REFERENCES:
		// <>

		// USE STATEMENTS:
		using DataSurfaces::AzimAngStepsForSolReflCalc;

		// Return value
		Real64 ObstrMultiplier;

		// Locals
		static Vector3< Real64 > URay; // Unit vector in (Phi,Theta) direction
		Real64 DPhi; // Phi increment (radians)
		Real64 DTheta; // Theta increment (radians)
		Real64 SkyGndUnObs; // Unobstructed sky irradiance at a ground point
		Real64 SkyGndObs; // Obstructed sky irradiance at a ground point

		Real64 Phi; // Altitude  angle of ray from a ground point (radians)
		Real64 SPhi; // Sin of Phi
		Real64 CPhi; // cos of Phi
		Real64 Theta; // Azimuth angle of ray from a ground point (radians)

		Real64 CosIncAngURay; // Cosine of incidence angle of URay on ground plane
		Real64 dOmegaGnd; // Solid angle element of ray from ground point (steradians)
		Real64 IncAngSolidAngFac; // CosIncAngURay*dOmegaGnd/Pi
		bool hitObs; // True iff obstruction is hit
		static Vector3< Real64 > ObsHitPt; // Coordinates of hit point on an obstruction (m)
		static int AltSteps_last( 0 );
		static Array1D< Real64 > cos_Phi( AltAngStepsForSolReflCalc / 2 ); // cos( Phi ) table
		static Array1D< Real64 > sin_Phi( AltAngStepsForSolReflCalc / 2 ); // sin( Phi ) table
		static int AzimSteps_last( 0 );
		static Array1D< Real64 > cos_Theta( 2 * AzimAngStepsForSolReflCalc ); // cos( Theta ) table
		static Array1D< Real64 > sin_Theta( 2 * AzimAngStepsForSolReflCalc ); // sin( Theta ) table

		assert( AzimSteps <= AzimAngStepsForSolReflCalc );

		DPhi = PiOvr2 / ( AltSteps / 2.0 );
		DTheta = Pi / AzimSteps;
		SkyGndObs = 0.0;
		SkyGndUnObs = 0.0;

		//Tuned Precompute Phi trig table
		if ( AltSteps != AltSteps_last ) {
			for ( int IPhi = 1, IPhi_end = ( AltSteps / 2 ); IPhi <= IPhi_end; ++IPhi ) {
				Phi = ( IPhi - 0.5 ) * DPhi;
				cos_Phi( IPhi ) = std::cos( Phi );
				sin_Phi( IPhi ) = std::sin( Phi );
			}
			AltSteps_last = AltSteps;
		}

		//Tuned Precompute Theta trig table
		if ( AzimSteps != AzimSteps_last ) {
			for ( int ITheta = 1; ITheta <= 2 * AzimSteps; ++ITheta ) {
				Theta = ( ITheta - 0.5 ) * DTheta;
				cos_Theta( ITheta ) = std::cos( Theta );
				sin_Theta( ITheta ) = std::sin( Theta );
			}
			AzimSteps_last = AzimSteps;
		}

		// Altitude loop
		for ( int IPhi = 1, IPhi_end = ( AltSteps / 2 ); IPhi <= IPhi_end; ++IPhi ) {
			SPhi = sin_Phi( IPhi );
			CPhi = cos_Phi( IPhi );

			// Third component of ground ray unit vector in (Theta,Phi) direction
			URay( 3 ) = SPhi;
			dOmegaGnd = CPhi * DTheta * DPhi;
			// Cosine of angle of incidence of ground ray on ground plane
			CosIncAngURay = SPhi;
			IncAngSolidAngFac = CosIncAngURay * dOmegaGnd / Pi;
			// Azimuth loop
			for ( int ITheta = 1; ITheta <= 2 * AzimSteps; ++ITheta ) {
				URay( 1 ) = CPhi * cos_Theta( ITheta );
				URay( 2 ) = CPhi * sin_Theta( ITheta );
				SkyGndUnObs += IncAngSolidAngFac;
				// Does this ground ray hit an obstruction?
				hitObs = false;
				if ( TotSurfaces < octreeCrossover ) { // Linear search through surfaces

					for ( int ObsSurfNum = 1; ObsSurfNum <= TotSurfaces; ++ObsSurfNum ) {
						if ( Surface( ObsSurfNum ).ShadowSurfPossibleObstruction ) {
							PierceSurface( ObsSurfNum, GroundHitPt, URay, ObsHitPt, hitObs ); // Check if ray pierces surface
							if ( hitObs ) break;
						}
					}

				} else { // Surface octree search

					// Lambda function for the octree to test for surface hit
					auto surfaceHit = [&GroundHitPt,&hitObs]( SurfaceData const & surface ) -> bool {
						if ( surface.ShadowSurfPossibleObstruction ) {
							PierceSurface( surface, GroundHitPt, URay, ObsHitPt, hitObs ); // Check if ray pierces surface
							return hitObs; // Ray pierces surface
						} else {
							return false;
						}
					};

					// Check octree surface candidates until a hit is found, if any
					Vector3< Real64 > const URay_inv( SurfaceOctreeCube::safe_inverse( URay ) );
					surfaceOctree.hasSurfaceRayIntersectsCube( GroundHitPt, URay, URay_inv, surfaceHit );

				}

				if ( hitObs ) continue; // Obstruction hit
				// Sky is hit
				SkyGndObs += IncAngSolidAngFac;
			} // End of azimuth loop
		} // End of altitude loop

		// in case ground point is surrounded by obstructions (SkyGndUnObs == 0), then multiplier will be equal to zero
		// This should not happen anyway because in that case ray would not be able to reach ground point
		ObstrMultiplier = 0.0;

		if ( SkyGndUnObs != 0.0 ) {
			ObstrMultiplier = SkyGndObs / SkyGndUnObs;
		}

		return ObstrMultiplier;
	}

	void
	FigureDayltgCoeffsAtPointsForSunPosition(
		int const ZoneNum,
		int const iRefPoint,
		int const iXelement,
		int const NWX, // Number of window elements in x direction for dayltg calc
		int const iYelement,
		int const NWY, // Number of window elements in y direction for dayltg calc
		int const WinEl, // Current window element counter
		int const IWin,
		int const IWin2,
		int const iHour,
		int & ISunPos,
		Real64 const SkyObstructionMult,
		Vector3< Real64 > const & RWIN2, // Center of a window element for TDD:DOME (if exists) in abs coord sys
		Vector3< Real64 > const & Ray, // Unit vector along ray from reference point to window element
		Real64 const PHRAY, // Altitude of ray from reference point to window element (radians)
		int const LSHCAL, // Interior shade calculation flag:  0=not yet calculated, 1=already calculated
		int const InShelfSurf, // Inside daylighting shelf surface number
		Real64 const COSB, // Cosine of angle between window outward normal and ray from reference point to window element
		Real64 const ObTrans, // Product of solar transmittances of exterior obstructions hit by ray from reference point through a window element
		Real64 const TVISB, // Visible transmittance of window for COSB angle of incidence (times light well efficiency, if appropriate)
		Real64 const DOMEGA, // Solid angle subtended by window element wrt reference point (steradians)
		int const ICtrl, // Window control counter
		int const ShType, // Window shading type
		int const BlNum, // Window blind number
		Real64 const THRAY, // Azimuth of ray from reference point to window element (radians)
		Vector3< Real64 > const & WNORM2, // Unit vector normal to window
		int const ExtWinType, // Exterior window type (InZoneExtWin, AdjZoneExtWin, NotInOrAdjZoneExtWin)
		int const IConst, // Construction counter
		Real64 const AZVIEW, // Azimuth of view vector in absolute coord system for glare calculation (radians)
		Vector3< Real64 > const & RREF2, // Location of virtual reference point in absolute coordinate system
		bool const hitIntObs, // True iff interior obstruction hit
		bool const hitExtObs, // True iff ray from ref pt to ext win hits an exterior obstruction
		int const CalledFrom, // indicate  which type of routine called this routine
		Real64 & TVISIntWin, // Visible transmittance of int win at COSBIntWin for light from ext win
		Real64 & TVISIntWinDisk, // Visible transmittance of int win at COSBIntWin for sun
		Optional_int_const MapNum,
		Optional< Real64 const > MapWindowSolidAngAtRefPtWtd
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith
		//       DATE WRITTEN   November 2012, refactor from legacy code by Fred Winklemann
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// collect code for calculations sun position aspects for daylighting coefficients

		// METHODOLOGY EMPLOYED:
		// switch as need to serve both reference points and map points based on calledFrom

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::POLYF;
		using General::BlindBeamBeamTrans;
		using DataSystemVariables::DetailedSkyDiffuseAlgorithm;
		using SolarReflectionManager::SolReflRecSurf;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		if ( SUNCOSHR( iHour, 3 ) < SunIsUpValue ) return;

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static Vector3< Real64 > const RREF( 0.0 ); // Location of a reference point in absolute coordinate system //Autodesk Was used uninitialized: Never set here // Made static for performance and const for now until issue addressed
		static Vector4< Real64 > XEDIRSK; // Illuminance contribution from luminance element, sky-related
//		Real64 XEDIRSU; // Illuminance contribution from luminance element, sun-related //Unused Set but never used
		static Vector4< Real64 > XAVWLSK; // Luminance of window element, sky-related
		static Vector3< Real64 > RAYCOS; // Unit vector from reference point to sun
		int JB; // Slat angle counter
		static Array1D< Real64 > TransBmBmMult( MaxSlatAngs ); // Beam-beam transmittance of isolated blind
		static Array1D< Real64 > TransBmBmMultRefl( MaxSlatAngs ); // As above but for beam reflected from exterior obstruction
		Real64 ProfAng; // Solar profile angle on a window (radians)
		Real64 POSFAC; // Position factor for a window element / ref point / view vector combination
		Real64 XR; // Horizontal displacement ratio
		Real64 YR; // Vertical displacement ratio
		bool hit; // True iff ray from ref point thru window element hits an obstruction

		Real64 ObTransDisk; // Product of solar transmittances of exterior obstructions hit by ray
		// from reference point to sun
		static Vector3< Real64 > HP; // Hit coordinates, if ray hits
		Real64 LumAtHitPtFrSun; // Luminance at hit point of obstruction by reflection of direct light from
		//  sun (cd/m2)
		int ISky; // Sky type index: 1=clear, 2=clear turbid, 3=intermediate, 4=overcast

		Real64 ELUM; // Sky or ground luminance (cd/m2)
		Real64 DEDIR; // Illuminance contribution at reference point from window element (lux)
		Real64 COSI; // Cosine of angle between direct sun and window outward normal
		bool hitWin; // True iff ray passes thru window
		Real64 TVISS; // Direct solar visible transmittance of window at given angle of incidence
		//  (times light well efficiency, if appropriate)
		Real64 XAVWL; // XAVWL*TVISS is contribution of window luminance from solar disk (cd/m2)

		Real64 SlatAng; // Blind slat angle (rad)
		int NearestHitSurfNum; // Surface number of nearest obstruction
		int NearestHitSurfNumX; // Surface number to use when obstruction is a shadowing surface
		static Vector3< Real64 > NearestHitPt; // Hit point of ray on nearest obstruction
//		Real64 SunObstructionMult; // = 1.0 if sun hits a ground point; otherwise = 0.0
		Real64 Alfa; // Intermediate variables
//		Real64 Beta; //Unused
		static Vector3< Real64 > GroundHitPt; // Coordinates of point that ray hits ground (m)
		bool hitObs; // True iff obstruction is hit
		static Vector3< Real64 > ObsHitPt; // Coordinates of hit point on an obstruction (m)
		int ObsConstrNum; // Construction number of obstruction
		Real64 ObsVisRefl; // Visible reflectance of obstruction
		Real64 SkyReflVisLum; // Reflected sky luminance at hit point divided by

		int RecSurfNum; // Receiving surface number
		int ReflSurfNum; // Reflecting surface number
		int ReflSurfNumX;
		static Vector3< Real64 > ReflNorm; // Normal vector to reflecting surface
		Real64 CosIncAngRefl; // Cos of angle of incidence of beam on reflecting surface
		static Vector3< Real64 > SunVecMir; // Sun ray mirrored in reflecting surface
		Real64 CosIncAngRec; // Cos of angle of incidence of reflected beam on receiving window
		bool hitRefl; // True iff ray hits reflecting surface
		static Vector3< Real64 > HitPtRefl; // Point that ray hits reflecting surface
		Real64 ReflDistanceSq; // Distance squared between ref pt and hit point on reflecting surf (m^2)
		Real64 ReflDistance; // Distance between ref pt and hit point on reflecting surf (m)
		bool hitObsRefl; // True iff obstruction hit between ref pt and reflection point
		static Vector3< Real64 > HitPtObs; // Hit point on obstruction
		int ReflSurfRecNum; // Receiving surface number for a reflecting window
		Real64 SpecReflectance; // Specular reflectance of a reflecting surface
		Real64 TVisRefl; // Bare window vis trans for reflected beam
		//  (times light well efficiency, if appropriate)
		int ConstrNumRefl; // Window construction number for a specularly reflecting shading surf
		Real64 PHSUNrefl; // Altitude angle of reflected sun (radians)
		Real64 THSUNrefl; // Azimuth anggle of reflected sun (radians)

		bool hitIntWinDisk; // True iff ray from ref pt to sun passes thru an int window
		bool hitIntObsDisk; // True iff ray from ref pt to sun hits an interior obstruction
//		bool hitExtObsDisk; // True iff ray from ref pt to sun hits an exterior obstruction //Unused Set but never used

		static Vector3< Real64 > HitPtIntWinDisk; // Intersection point on an interior window for ray from ref pt to sun (m)
		int IntWinDiskHitNum; // Surface number of int window intersected by ray betw ref pt and sun
		Real64 COSBIntWin; // Cos of angle between int win outward normal and ray betw ref pt and
		//  exterior window element or between ref pt and sun
		Real64 TVisIntWinMult; // Interior window vis trans multiplier for ext win in adjacent zone
		Real64 TVisIntWinDiskMult; // Interior window vis trans solar disk multiplier for ext win in adj zone
		Real64 WindowSolidAngleDaylightPoint;

		++ISunPos;

		// Altitude of sun (degrees)
		PHSUN = PHSUNHR( iHour );
		SPHSUN = SPHSUNHR( iHour );
		CPHSUN = CPHSUNHR( iHour );

		// Azimuth of sun in absolute coord sys
		THSUN = THSUNHR( iHour );

		// First time through, call routine to calculate inter-reflected illuminance
		// at reference point and luminance of window with shade, screen or blind.

		// Rob/TH - Not sure whether this call is necessary for interior zones with interior windows only.
		//  new code would be -
		// IF (LSHCAL == 1 .AND. ExtWinType /= AdjZoneExtWin) CALL DayltgInterReflectedIllum(ISunPos,IHR,ZoneNum,IWin2)
		if ( SurfaceWindow( IWin ).WindowModelType != WindowBSDFModel ) {
			if ( LSHCAL == 1 ) DayltgInterReflectedIllum( ISunPos, iHour, ZoneNum, IWin2 );
		} else {
			if ( LSHCAL == 1 ) DayltgInterReflectedIllumComplexFenestration( IWin2, WinEl, iHour, ZoneNum, iRefPoint, CalledFrom, MapNum );
			if ( COSB <= 0.0 ) return;
			DayltgDirectIllumComplexFenestration( IWin, WinEl, iHour, ZoneNum, iRefPoint, CalledFrom, MapNum );
			// Call direct sun component only once since calculation is done for entire window
			if ( WinEl == ( NWX * NWY ) ) {
				DayltgDirectSunDiskComplexFenestration( IWin2, ZoneNum, iHour, iRefPoint, WinEl, AZVIEW, CalledFrom, MapNum, MapWindowSolidAngAtRefPtWtd );
			}
			return;
		}

		// Daylighting shelf simplification:  The shelf completely blocks all view of the window,
		// only interrelflected illumination is allowed (see DayltgInterReflectedIllum above).
		// Everything else in this loop has to do with direct luminance from the window.
		if ( InShelfSurf > 0 ) return;

		if ( COSB <= 0.0 ) return;

		XEDIRSK = 0.0;
//		XEDIRSU = 0.0; //Unused Set but never used
		XAVWLSK = 0.0;
		Real64 const Ray_3( Ray( 3 ) );
		Real64 const DOMEGA_Ray_3( DOMEGA * Ray_3 );

		// Add contribution of this window element to glare and to
		// direct illuminance at reference point

		// The I,J,K indices for sky and sun components of direct illuminance
		// (EDIRSK, EDIRSU) and average window luminance (AVWLSK, AVWLSU) are:
		// I=1 for clear sky, =2 Clear turbid, =3 Intermediate, =4 Overcast;
		// J=1 for bare window, =2 for window with shade or fixed slat-angle blind;
		//  = 2,3,...,MaxSlatAngs+1 for window with variable slat-angle blind;
		// K = sun position index.

		// ----- CASE I -- BARE WINDOW (no shading device)

		// Beam solar and sky solar reflected from nearest obstruction.
		// In the following hitIntObs == false  ==> no interior obstructions hit, and
		//                  hitExtObs == true  ==> one or more exterior obstructions hit.
		if ( CalcSolRefl && !hitIntObs && hitExtObs ) {
			// One or more exterior obstructions was hit; get contribution of reflection
			// from nearest obstruction.
			// Find obstruction whose hit point is closest to this ray's window element
			DayltgClosestObstruction( RWIN2, Ray, NearestHitSurfNum, NearestHitPt );
			if ( NearestHitSurfNum > 0 ) {

				// Beam solar reflected from nearest obstruction

				DayltgSurfaceLumFromSun( iHour, Ray, NearestHitSurfNum, NearestHitPt, LumAtHitPtFrSun );
				AVWLSU( iHour, 1 ) += LumAtHitPtFrSun * TVISB;
				if ( PHRAY >= 0.0 ) EDIRSU( iHour, 1 ) += LumAtHitPtFrSun * DOMEGA_Ray_3 * TVISB;

				// Sky solar reflected from nearest obstruction

				ObsConstrNum = Surface( NearestHitSurfNum ).Construction;
				if ( ObsConstrNum > 0 ) {
					// Exterior building surface is nearest hit
					if ( ! Construct( ObsConstrNum ).TypeIsWindow ) {
						// Obstruction is not a window, i.e., is an opaque surface
						ObsVisRefl = 1.0 - Material( Construct( ObsConstrNum ).LayerPoint( 1 ) ).AbsorpVisible;
					} else {
						// Obstruction is a window; assume it is bare
						if ( SurfaceWindow( NearestHitSurfNum ).StormWinFlag == 1 ) ObsConstrNum = Surface( NearestHitSurfNum ).StormWinConstruction;
						ObsVisRefl = Construct( ObsConstrNum ).ReflectVisDiffFront;
					}
				} else {
					// Shadowing surface is nearest hit
					if ( Surface( NearestHitSurfNum ).Shelf > 0 ) {
						// This is a daylighting shelf, for which reflection is separately calculated
						ObsVisRefl = 0.0;
					} else {
						ObsVisRefl = Surface( NearestHitSurfNum ).ShadowSurfDiffuseVisRefl;
						if ( Surface( NearestHitSurfNum ).ShadowSurfGlazingConstruct > 0 ) ObsVisRefl += Surface( NearestHitSurfNum ).ShadowSurfGlazingFrac * Construct( Surface( NearestHitSurfNum ).ShadowSurfGlazingConstruct ).ReflectVisDiffFront;
					}
				}
				NearestHitSurfNumX = NearestHitSurfNum;
				// Each shadowing surface has a "mirror" duplicate surface facing in the opposite direction.
				// The following gets the correct side of a shadowing surface for reflection.
				if ( Surface( NearestHitSurfNum ).ShadowingSurf ) {
					if ( dot( Ray, Surface( NearestHitSurfNum ).OutNormVec ) > 0.0 ) NearestHitSurfNumX = NearestHitSurfNum + 1;
				}
				if ( ! DetailedSkyDiffuseAlgorithm || ! ShadingTransmittanceVaries || SolarDistribution == MinimalShadowing ) {
					SkyReflVisLum = ObsVisRefl * Surface( NearestHitSurfNumX ).ViewFactorSky * DifShdgRatioIsoSky( NearestHitSurfNumX ) / Pi;
				} else {
					SkyReflVisLum = ObsVisRefl * Surface( NearestHitSurfNumX ).ViewFactorSky * DifShdgRatioIsoSkyHRTS( 1, iHour, NearestHitSurfNumX ) / Pi;
				}
				assert( equal_dimensions( AVWLSK, EDIRSK ) );
				auto l2( GILSK.index( iHour, 1 ) );
				auto l3( AVWLSK.index( iHour, 1, 1 ) );
				for ( ISky = 1; ISky <= 4; ++ISky, ++l2, ++l3 ) { // [ l2 ] == ( ISky, iHour ) // [ l3 ] == ( ISky, 1, iHour )
					XAVWLSK( ISky ) = GILSK[ l2 ] * SkyReflVisLum;
					AVWLSK[ l3 ] += XAVWLSK( ISky ) * TVISB;
					if ( PHRAY >= 0.0 ) {
						XEDIRSK( ISky ) = GILSK[ l2 ] * SkyReflVisLum * DOMEGA_Ray_3;
						EDIRSK[ l3 ] += XEDIRSK( ISky ) * TVISB;
					}
				}
			}
		} // End of check if solar reflection calculation is in effect

		if ( ObTrans > 1.e-6 ) {
			// Ray did not hit an obstruction or the transmittance product of hit obstructions is non-zero.
			// Contribution of sky or ground luminance in cd/m2
			if ( SurfaceWindow( IWin ).OriginalClass == SurfaceClass_TDD_Diffuser ) {
				// Make all transmitted light diffuse for a TDD with a bare diffuser
				assert( equal_dimensions( AVWLSK, WLUMSK ) );
				assert( equal_dimensions( AVWLSK, EDIRSK ) );
				auto l3( AVWLSK.index( iHour, 1, 1 ) );
				for ( ISky = 1; ISky <= 4; ++ISky, ++l3 ) { // [ l3 ] == ( ISky, 1, iHour )
					AVWLSK[ l3 ] += WLUMSK[ l3 ];
					if ( ISky == 1 ) {
						AVWLSU( iHour, 1 ) += WLUMSU( iHour, 1 );
						AVWLSUdisk( iHour, 1 ) += WLUMSUdisk( iHour, 1 );
					}
					if ( PHRAY > 0.0 ) {
						EDIRSK[ l3 ] += WLUMSK[ l3 ] * DOMEGA_Ray_3;
						if ( ISky == 1 ) EDIRSU( iHour, 1 ) += WLUMSU( iHour, 1 ) * DOMEGA_Ray_3;
					}
				}

			} else { // Bare window
				//Tuned Hoisted operations out of loop and linear indexing
				if ( CalcSolRefl ) { // Coordinates of ground point hit by the ray
					Alfa = std::acos( -Ray_3 );
					Real64 const Ray_1( Ray( 1 ) );
					Real64 const Ray_2( Ray( 2 ) );
//					Beta = std::atan2( Ray_2, Ray_1 ); //Unused Tuning below eliminated use
					Real64 HorDis( ( RWIN2( 3 ) - GroundLevelZ ) * std::tan( Alfa ) ); // Distance between ground hit point and proj'n of center
					GroundHitPt( 3 ) = GroundLevelZ;
//Tuned Replaced by below: sqrt is faster than sincos
//					GroundHitPt( 1 ) = RWIN2( 1 ) + HorDis * std::cos( Beta );
//					GroundHitPt( 2 ) = RWIN2( 2 ) + HorDis * std::sin( Beta );
					Real64 const Ray_r( std::sqrt( square( Ray_1 ) + square( Ray_2 ) ) );
					if ( Ray_r > 0.0 ) {
						HorDis /= Ray_r;
						GroundHitPt( 1 ) = RWIN2( 1 ) + HorDis * Ray_1;
						GroundHitPt( 2 ) = RWIN2( 2 ) + HorDis * Ray_2;
					} else { // Treat as angle==0
						GroundHitPt( 1 ) = RWIN2( 1 ) + HorDis;
						GroundHitPt( 2 ) = RWIN2( 2 );
					}
				}
				Real64 const GILSK_mult( ( GndReflectanceForDayltg / Pi ) * ObTrans * SkyObstructionMult );
				Real64 const TVISB_ObTrans( TVISB * ObTrans );
				Real64 const AVWLSU_add( TVISB_ObTrans * GILSU( iHour ) * ( GndReflectanceForDayltg / Pi ) );
				Vector3< Real64 > const SUNCOS_iHour( SUNCOSHR( iHour, {1,3} ) );
				assert( equal_dimensions( EDIRSK, AVWLSK ) );
				auto l( EDIRSK.index( iHour, 1, 1 ) );
				for ( ISky = 1; ISky <= 4; ++ISky, ++l ) { // [ l ] == ( iHour, 1, ISky )
					if ( PHRAY > 0.0 ) { // Ray heads upward to sky
						ELUM = DayltgSkyLuminance( ISky, THRAY, PHRAY );
						XEDIRSK( ISky ) = ELUM * DOMEGA_Ray_3;
						DEDIR = XEDIRSK( ISky ) * TVISB;
						EDIRSK[ l ] += DEDIR * ObTrans;
						AVWLSK[ l ] += ELUM * TVISB_ObTrans;
						XAVWLSK( ISky ) = ELUM * ObTrans;
					} else { // PHRAY <= 0.
						// Ray heads downward to ground.
						// Contribution from sky diffuse reflected from ground
						XAVWLSK( ISky ) = GILSK( iHour, ISky ) * GILSK_mult;
						AVWLSK[ l ] += TVISB * XAVWLSK( ISky );
						// Contribution from beam solar reflected from ground (beam reaching ground point
						// can be obstructed [SunObstructionMult < 1.0] if CalcSolRefl = .TRUE.)
						if ( ISky == 1 ) {
							//SunObstructionMult = 1.0; //Tuned
							if ( CalcSolRefl ) { // Coordinates of ground point hit by the ray
								// Sun reaches ground point if vector from this point to the sun is unobstructed
								hitObs = false;
								for ( int ObsSurfNum = 1; ObsSurfNum <= TotSurfaces; ++ObsSurfNum ) {
									if ( ! Surface( ObsSurfNum ).ShadowSurfPossibleObstruction ) continue;
									PierceSurface( ObsSurfNum, GroundHitPt, SUNCOS_iHour, ObsHitPt, hitObs );
									if ( hitObs ) break;
								}
								//if ( hitObs ) SunObstructionMult = 0.0;
								if ( !hitObs ) AVWLSU( iHour, 1 ) += AVWLSU_add;
							} else {
								AVWLSU( iHour, 1 ) += AVWLSU_add;
							}
						} // End of check if ISky = 1
					} // End of check if ray is going up or down
				} // End of loop over sky types
			} // End of check if bare window or TDD:DIFFUSER
		} // End of check if ObTrans > 1.E-6

		// Illuminance from beam solar (without interior reflection)
		// Just run this once on the last pass
		if ( iXelement == NWX && iYelement == NWY ) { // Last pass

			// Beam solar reaching reference point directly without exterior reflection

			// Unit vector from ref. pt. to sun
			RAYCOS( 1 ) = CPHSUN * std::cos( THSUN );
			RAYCOS( 2 ) = CPHSUN * std::sin( THSUN );
			RAYCOS( 3 ) = SPHSUN;

			// Is sun on front side of exterior window?
			COSI = dot( WNORM2, RAYCOS );
			if ( COSI > 0.0 ) {

				// Does RAYCOS pass thru exterior window? HP is point that RAYCOS intersects window plane.
				PierceSurface( IWin2, RREF2, RAYCOS, HP, hitWin );
				hitIntObsDisk = false;
				if ( hitWin ) {
					if ( ExtWinType == InZoneExtWin ) {
						// Check for interior obstructions between reference point and HP.
						DayltgHitInteriorObstruction( IWin2, RREF2, HP, hitIntObsDisk );
					}
					ObTransDisk = 0.0; // Init value
					// Init flag for vector from RP to sun passing through interior window
					hitIntWinDisk = false;
					if ( ExtWinType == AdjZoneExtWin ) { // This block is for RPs in zones with interior windows
						// adjacent to zones with exterior windows
						// Does RAYCOS pass through interior window in zone containing RP?
						// Loop over zone surfaces looking for interior windows between reference point and sun
						for ( int IntWinDisk = Zone( ZoneNum ).SurfaceFirst, IntWinDisk_end = Zone( ZoneNum ).SurfaceLast; IntWinDisk <= IntWinDisk_end; ++IntWinDisk ) {
							if ( Surface( IntWinDisk ).Class == SurfaceClass_Window && Surface( IntWinDisk ).ExtBoundCond >= 1 ) {
								if ( Surface( Surface( IntWinDisk ).ExtBoundCond ).Zone == Surface( IWin2 ).Zone ) {
									PierceSurface( IntWinDisk, RREF, RAYCOS, HitPtIntWinDisk, hitIntWinDisk );
									if ( hitIntWinDisk ) {
										IntWinDiskHitNum = IntWinDisk;
										COSBIntWin = dot( Surface( IntWinDisk ).OutNormVec, RAYCOS );
										if ( COSBIntWin <= 0.0 ) {
											hitIntWinDisk = false;
											IntWinDiskHitNum = 0;
											continue;
										}
										TVISIntWinDisk = POLYF( COSBIntWin, Construct( Surface( IntWinDisk ).Construction ).TransVisBeamCoef );
										break;
									}
								}
							}
						}

						if ( !hitIntWinDisk ) { // Vector from RP to sun does not pass through interior window
							ObTransDisk = 0.0;
							hit = true; //!fcw Is this needed?
						}

						// Check for interior obstructions between ref point and interior window
						hitIntObsDisk = false;
						if ( hitIntWinDisk ) {
							DayltgHitInteriorObstruction( IntWinDiskHitNum, RREF, HitPtIntWinDisk, hitIntObsDisk );
							// If no obstruction between RP and hit int win, check for obstruction
							// between int win and ext win
							if ( !hitIntObsDisk ) {
								DayltgHitBetWinObstruction( IntWinDiskHitNum, IWin2, HitPtIntWinDisk, HP, hitIntObsDisk );
							}
						}
						if ( hitIntObsDisk ) ObTransDisk = 0.0;
					} // case where RP is in zone with interior window adjacent to zone with exterior window

//					hitExtObsDisk = false; //Unused Set but never used
					// RJH 08-25-07 hitIntWinDisk should not be reset to false here, and should be tested below.
					// This is to correct logic flaw causing direct solar to reach adjacent zone refpt
					// when vector to sun does not pass through interior window
					// hitIntWinDisk = false
					if ( !hitIntObsDisk ) { // No interior obstruction was hit
						// Net transmittance of exterior obstructions encountered by RAYCOS
						// ObTransDisk = 1.0 will be returned if no exterior obstructions are hit.
						DayltgHitObstruction( iHour, IWin2, RREF2, RAYCOS, ObTransDisk );
//						if ( ObTransDisk < 1.0 ) hitExtObsDisk = true; //Unused Set but never used
						// RJH 08-26-07 However, if this is a case of interior window
						// and vector to sun does not pass through interior window
						// then reset ObTransDisk to 0.0 since it is the key test for adding
						// contribution of sun to RP below.
						if ( ( ExtWinType == AdjZoneExtWin ) && ( !hitIntWinDisk ) ) {
							ObTransDisk = 0.0;
						}
					}

					// PETER: need side wall mounted TDD to test this
					// PETER: probably need to replace RREF2 with RWIN2
					// PETER: need to check for interior obstructions too.

					if ( ObTransDisk > 1.e-6 ) {

						// Sun reaches reference point;  increment illuminance.
						// Direct normal illuminance is normalized to 1.0

						if ( SurfaceWindow( IWin ).OriginalClass == SurfaceClass_TDD_Diffuser ) {
							// No beam is transmitted.  Takes care of TDD with a bare diffuser and all types of blinds.
							TVISS = 0.0;
						} else {
							// Beam transmittance for bare window and all types of blinds
							TVISS = POLYF( COSI, Construct( IConst ).TransVisBeamCoef ) * SurfaceWindow( IWin ).GlazedFrac * SurfaceWindow( IWin ).LightWellEff;
							if ( ExtWinType == AdjZoneExtWin && hitIntWinDisk ) TVISS *= TVISIntWinDisk;
						}

						EDIRSUdisk( iHour, 1 ) = RAYCOS( 3 ) * TVISS * ObTransDisk; // Bare window

						TransBmBmMult = 0.0;
						if ( ShType == WSC_ST_ExteriorBlind || ShType == WSC_ST_InteriorBlind || ShType == WSC_ST_BetweenGlassBlind ) {
							ProfileAngle( IWin, RAYCOS, Blind( BlNum ).SlatOrientation, ProfAng );
							// Contribution of beam passing through slats and reaching reference point
							for ( JB = 1; JB <= MaxSlatAngs; ++JB ) {
								//IF (.NOT.SurfaceWindow(IWin)%MovableSlats .AND. JB > 1) EXIT
								if ( SurfaceWindow( IWin ).MovableSlats ) {
									SlatAng = ( JB - 1 ) * Pi / ( MaxSlatAngs - 1 );
								} else {
									SlatAng = Blind( BlNum ).SlatAngle * DegToRadians;
								}
								TransBmBmMult( JB ) = BlindBeamBeamTrans( ProfAng, SlatAng, Blind( BlNum ).SlatWidth, Blind( BlNum ).SlatSeparation, Blind( BlNum ).SlatThickness );
								EDIRSUdisk( iHour, JB + 1 ) = RAYCOS( 3 ) * TVISS * TransBmBmMult( JB ) * ObTransDisk;

								// do this only once for fixed slat blinds
								if ( ! SurfaceWindow( IWin ).MovableSlats ) break;
							}
						} else if ( ShType == WSC_ST_ExteriorScreen ) {
							//                          pass angle from sun to window normal here using PHSUN and THSUN from above and surface angles
							//                          SunAltitudeToWindowNormalAngle = PHSUN - SurfaceWindow(IWin)%Phi
							//                          SunAzimuthToWindowNormalAngle = THSUN - SurfaceWindow(IWin)%Theta
							CalcScreenTransmittance( IWin, ( PHSUN - SurfaceWindow( IWin ).Phi ), ( THSUN - SurfaceWindow( IWin ).Theta ) );
							TransBmBmMult( 1 ) = SurfaceScreens( SurfaceWindow( IWin ).ScreenNumber ).BmBmTrans;
							EDIRSUdisk( iHour, 2 ) = RAYCOS( 3 ) * TVISS * TransBmBmMult( 1 ) * ObTransDisk;
						}

						// Glare from solar disk

						// Position factor for sun (note that AZVIEW is wrt y-axis and THSUN is wrt
						// x-axis of absolute coordinate system.
						XR = std::tan( std::abs( PiOvr2 - AZVIEW - THSUN ) + 0.001 );
						YR = std::tan( PHSUN + 0.001 );
						POSFAC = DayltgGlarePositionFactor( XR, YR );

						{ auto const SELECT_CASE_var( CalledFrom );

						if ( SELECT_CASE_var == CalledForRefPoint ) {
							WindowSolidAngleDaylightPoint = SurfaceWindow( IWin ).SolidAngAtRefPtWtd( iRefPoint );
						} else if ( SELECT_CASE_var == CalledForMapPoint ) {
							WindowSolidAngleDaylightPoint = MapWindowSolidAngAtRefPtWtd;
						}}

						if ( POSFAC != 0.0 && WindowSolidAngleDaylightPoint > 0.000001 ) {
							// Increment window luminance.  Luminance of solar disk (cd/m2)
							// is 1.47*10^4*(direct normal solar illuminance) for direct normal solar
							// illuminance in lux (lumens/m2). For purposes of calculating daylight factors
							// direct normal solar illuminance = 1.0.
							// Solid angle subtended by sun is 0.000068 steradians

							XAVWL = 14700.0 * std::sqrt( 0.000068 * POSFAC ) * double( NWX * NWY ) / std::pow( WindowSolidAngleDaylightPoint, 0.8 );
							AVWLSUdisk( iHour, 1 ) = XAVWL * TVISS * ObTransDisk; // Bare window

							if ( ShType == WSC_ST_ExteriorBlind || ShType == WSC_ST_InteriorBlind || ShType == WSC_ST_BetweenGlassBlind ) {
								for ( JB = 1; JB <= MaxSlatAngs; ++JB ) {
									//IF (.NOT. SurfaceWindow(IWin)%MovableSlats .AND. JB > 1) EXIT
									AVWLSUdisk( iHour, JB + 1 ) = XAVWL * TVISS * TransBmBmMult( JB ) * ObTransDisk;
									if ( ! SurfaceWindow( IWin ).MovableSlats ) break;
								}
							} else if ( ShType == WSC_ST_ExteriorScreen ) {
								AVWLSUdisk( iHour, 2 ) = XAVWL * TVISS * TransBmBmMult( 1 ) * ObTransDisk;
							}
						} // Position Factor
					} // Beam avoids all obstructions
				} // Beam passes thru window
			} // Sun on front side

			// Beam solar reaching reference point after beam-beam (specular) reflection from
			// an exterior surface

			if ( CalcSolRefl ) {
				// Receiving surface number corresponding this window
				RecSurfNum = Surface( IWin2 ).ShadowSurfRecSurfNum;
				if ( RecSurfNum > 0 ) { // interior windows do not apply
					if ( SolReflRecSurf( RecSurfNum ).NumPossibleObs > 0 ) {
						// This window has associated obstructions that could reflect beam onto the window
						for ( int loop = 1, loop_end = SolReflRecSurf( RecSurfNum ).NumPossibleObs; loop <= loop_end; ++loop ) {
							ReflSurfNum = SolReflRecSurf( RecSurfNum ).PossibleObsSurfNums( loop );
							ReflSurfNumX = ReflSurfNum;
							// Each shadowing surface has a "mirror" duplicate surface facing in the opposite direction.
							// The following gets the correct side of a shadowing surface for reflection.
							if ( Surface( ReflSurfNum ).ShadowingSurf ) {
								if ( dot( RAYCOS, Surface( ReflSurfNum ).OutNormVec ) < 0.0 ) ReflSurfNumX = ReflSurfNum + 1;
							}
							// Require that the surface can have specular reflection
							if ( Surface( ReflSurfNum ).Class == SurfaceClass_Window || Surface( ReflSurfNum ).ShadowSurfGlazingFrac > 0.0 ) {
								ReflNorm = Surface( ReflSurfNumX ).OutNormVec;
								// Vector to sun that is mirrored in obstruction
								SunVecMir = RAYCOS - 2.0 * dot( RAYCOS, ReflNorm ) * ReflNorm;
								// Skip if reflecting surface is not sunlit
								if ( SunlitFrac( 1, iHour, ReflSurfNumX ) < 0.01 ) continue;
								// Skip if altitude angle of mirrored sun is negative since reflected sun cannot
								// reach reference point in this case
								if ( SunVecMir( 3 ) <= 0.0 ) continue;
								// Cosine of incidence angle of reflected beam on window
								CosIncAngRec = dot( Surface( IWin2 ).OutNormVec, SunVecMir );
								if ( CosIncAngRec <= 0.0 ) continue;
								// Does ray from ref. pt. along SunVecMir pass through window?
								PierceSurface( IWin2, RREF2, SunVecMir, HP, hitWin );
								if ( !hitWin ) continue; // Ray did not pass through window
								// Check if this ray hits interior obstructions
								DayltgHitInteriorObstruction( IWin2, RREF2, HP, hit );
								if ( hit ) continue; // Interior obstruction was hit
								// Does ray hit this reflecting surface?
								PierceSurface( ReflSurfNum, RREF2, SunVecMir, HitPtRefl, hitRefl );
								if ( !hitRefl ) continue; // Ray did not hit this reflecting surface
								ReflDistanceSq = distance_squared( HitPtRefl, RREF2 );
								ReflDistance = std::sqrt( ReflDistanceSq );
								// Is ray from ref. pt. to reflection point (HitPtRefl) obstructed?
								hitObsRefl = false;
								for ( int loop2 = 1, loop2_end = SolReflRecSurf( RecSurfNum ).NumPossibleObs; loop2 <= loop2_end; ++loop2 ) {
									int const ObsSurfNum = SolReflRecSurf( RecSurfNum ).PossibleObsSurfNums( loop2 );
									if ( ObsSurfNum == ReflSurfNum || ObsSurfNum == Surface( ReflSurfNum ).BaseSurf ) continue;
									PierceSurface( ObsSurfNum, RREF2, SunVecMir, ReflDistance, HitPtObs, hitObs ); // ReflDistance cutoff added
									if ( hitObs ) { // => Could skip distance check (unless < vs <= ReflDistance really matters)
										if ( distance_squared( HitPtObs, RREF2 ) < ReflDistanceSq ) { // Distance squared from ref pt to reflection point
											hitObsRefl = true;
											break;
										}
									}
								}
								if ( hitObsRefl ) continue; // Obstruction closer than reflection pt. was hit; go to next obstruction
								// There is no obstruction for this ray between ref pt and hit pt on reflecting surface.
								// See if ray from hit pt on reflecting surface to original (unmirrored) sun position is obstructed
								hitObs = false;
								if ( Surface( ReflSurfNum ).Class == SurfaceClass_Window ) {
									// Reflecting surface is a window.
									// Receiving surface number for this reflecting window.
									ReflSurfRecNum = Surface( ReflSurfNum ).ShadowSurfRecSurfNum;
									if ( ReflSurfRecNum > 0 ) {
										// Loop over possible obstructions for this reflecting window
										for ( int loop2 = 1, loop2_end = SolReflRecSurf( ReflSurfRecNum ).NumPossibleObs; loop2 <= loop2_end; ++loop2 ) {
											int const ObsSurfNum = SolReflRecSurf( ReflSurfRecNum ).PossibleObsSurfNums( loop2 );
											PierceSurface( ObsSurfNum, HitPtRefl, RAYCOS, HitPtObs, hitObs );
											if ( hitObs ) break;
										}
									}
								} else {
									// Reflecting surface is a building shade
									for ( int ObsSurfNum = 1; ObsSurfNum <= TotSurfaces; ++ObsSurfNum ) {
										if ( ! Surface( ObsSurfNum ).ShadowSurfPossibleObstruction ) continue;
										if ( ObsSurfNum == ReflSurfNum ) continue;
										PierceSurface( ObsSurfNum, HitPtRefl, RAYCOS, HitPtObs, hitObs );
										if ( hitObs ) break;
									}
								} // End of check if reflector is a window or shadowing surface

								if ( hitObs ) continue; // Obstruction hit between reflection hit point and sun; go to next obstruction

								// No obstructions. Calculate reflected beam illuminance at ref. pt. from this reflecting surface.
								SpecReflectance = 0.0;
								CosIncAngRefl = std::abs( dot( RAYCOS, ReflNorm ) );
								if ( Surface( ReflSurfNum ).Class == SurfaceClass_Window ) {
									ConstrNumRefl = Surface( ReflSurfNum ).Construction;
									if ( SurfaceWindow( ReflSurfNum ).StormWinFlag == 1 ) ConstrNumRefl = Surface( ReflSurfNum ).StormWinConstruction;
									SpecReflectance = POLYF( std::abs( CosIncAngRefl ), Construct( ConstrNumRefl ).ReflSolBeamFrontCoef );
								}
								if ( Surface( ReflSurfNum ).ShadowingSurf && Surface( ReflSurfNum ).ShadowSurfGlazingConstruct > 0 ) SpecReflectance = Surface( ReflSurfNum ).ShadowSurfGlazingFrac * POLYF( std::abs( CosIncAngRefl ), Construct( Surface( ReflSurfNum ).ShadowSurfGlazingConstruct ).ReflSolBeamFrontCoef );
								TVisRefl = POLYF( CosIncAngRec, Construct( IConst ).TransVisBeamCoef ) * SurfaceWindow( IWin ).GlazedFrac * SurfaceWindow( IWin ).LightWellEff;
								EDIRSUdisk( iHour, 1 ) += SunVecMir( 3 ) * SpecReflectance * TVisRefl; // Bare window

								TransBmBmMultRefl = 0.0;
								if ( ShType == WSC_ST_ExteriorBlind || ShType == WSC_ST_InteriorBlind || ShType == WSC_ST_BetweenGlassBlind ) {
									ProfileAngle( IWin, SunVecMir, Blind( BlNum ).SlatOrientation, ProfAng );
									// Contribution of reflected beam passing through slats and reaching reference point
									Real64 const Pi_SlatAng_fac( Pi / ( MaxSlatAngs - 1 ) );
									for ( JB = 1; JB <= MaxSlatAngs; ++JB ) {
										//IF (.NOT.SurfaceWindow(IWin)%MovableSlats .AND. JB > 1) EXIT
										if ( SurfaceWindow( IWin ).MovableSlats ) {
											SlatAng = double( JB - 1 ) * Pi_SlatAng_fac;
										} else {
											SlatAng = Blind( BlNum ).SlatAngle * DegToRadians;
										}
										TransBmBmMultRefl( JB ) = BlindBeamBeamTrans( ProfAng, SlatAng, Blind( BlNum ).SlatWidth, Blind( BlNum ).SlatSeparation, Blind( BlNum ).SlatThickness );
										EDIRSUdisk( iHour, JB + 1 ) += SunVecMir( 3 ) * SpecReflectance * TVisRefl * TransBmBmMultRefl( JB );

										if ( ! SurfaceWindow( IWin ).MovableSlats ) break;
									}
								} else if ( ShType == WSC_ST_ExteriorScreen ) {
									//                             pass angle from sun to window normal here using PHSUN and THSUN from above and surface angles
									//                             SunAltitudeToWindowNormalAngle = PHSUN - SurfaceWindow(IWin)%Phi
									//                             SunAzimuthToWindowNormalAngle = THSUN - SurfaceWindow(IWin)%Theta
									CalcScreenTransmittance( IWin, ( PHSUN - SurfaceWindow( IWin ).Phi ), ( THSUN - SurfaceWindow( IWin ).Theta ) );
									TransBmBmMultRefl( 1 ) = SurfaceScreens( SurfaceWindow( IWin ).ScreenNumber ).BmBmTrans;
									EDIRSUdisk( iHour, 2 ) += SunVecMir( 3 ) * SpecReflectance * TVisRefl * TransBmBmMultRefl( 1 );
								} // End of check if window has a blind or screen

								// Glare from reflected solar disk

								PHSUNrefl = SunVecMir( 3 );
								THSUNrefl = std::atan2( SunVecMir( 2 ), SunVecMir( 1 ) );
								XR = std::tan( std::abs( PiOvr2 - AZVIEW - THSUNrefl ) + 0.001 );
								YR = std::tan( PHSUNrefl + 0.001 );
								POSFAC = DayltgGlarePositionFactor( XR, YR );
								if ( POSFAC != 0.0 && SurfaceWindow( IWin ).SolidAngAtRefPtWtd( iRefPoint ) > 0.000001 ) {
									XAVWL = 14700.0 * std::sqrt( 0.000068 * POSFAC ) * double( NWX * NWY ) / std::pow( SurfaceWindow( IWin ).SolidAngAtRefPtWtd( iRefPoint ), 0.8 );
									AVWLSUdisk( iHour, 1 ) += XAVWL * TVisRefl * SpecReflectance; // Bare window
									if ( ShType == WSC_ST_ExteriorBlind || ShType == WSC_ST_InteriorBlind || ShType == WSC_ST_BetweenGlassBlind ) {
										for ( JB = 1; JB <= MaxSlatAngs; ++JB ) {
											//IF(.NOT. SurfaceWindow(IWin)%MovableSlats .AND. JB > 1) EXIT
											AVWLSUdisk( iHour, JB + 1 ) += XAVWL * TVisRefl * SpecReflectance * TransBmBmMultRefl( JB );
											if ( ! SurfaceWindow( IWin ).MovableSlats ) break;
										}
									} else if ( ShType == WSC_ST_ExteriorScreen ) {
										AVWLSUdisk( iHour, 2 ) += XAVWL * TVisRefl * SpecReflectance * TransBmBmMultRefl( 1 );
									}
								}
							} // End of check that obstruction can specularly reflect
						} // End of loop over obstructions associated with this window

					} // End of check if this window has associated obstructions
				} // End of check to see if this is exterior type window
			} // End of check if exterior reflection calculation is in effect

		} // Last pass

		if ( ( ICtrl > 0 && ( ShType == WSC_ST_InteriorShade || ShType == WSC_ST_ExteriorShade || ShType == WSC_ST_BetweenGlassShade || ShType == WSC_ST_InteriorBlind || ShType == WSC_ST_ExteriorBlind || ShType == WSC_ST_BetweenGlassBlind || ShType == WSC_ST_ExteriorScreen ) ) || SurfaceWindow( IWin ).SolarDiffusing ) {

			// ----- CASE II -- WINDOW WITH SCREEN, SHADE, BLIND, OR DIFFUSING WINDOW

			// Interior window visible transmittance multiplier for exterior window in adjacent zone
			TVisIntWinMult = 1.0;
			TVisIntWinDiskMult = 1.0;
			if ( Surface( IWin ).Zone != ZoneNum ) {
				TVisIntWinMult = TVISIntWin;
				TVisIntWinDiskMult = TVISIntWinDisk;
			}

			Real64 const DOMEGA_Ray_3_TVisIntWinMult( DOMEGA_Ray_3 * TVisIntWinMult );
			for ( ISky = 1; ISky <= 4; ++ISky ) {
				for ( JB = 1; JB <= MaxSlatAngs; ++JB ) {
					//IF (.NOT.SurfaceWindow(IWin)%MovableSlats .AND. JB > 1) EXIT
					AVWLSK( iHour, JB + 1, ISky ) += WLUMSK( iHour, JB + 1, ISky ) * TVisIntWinMult;
					if ( ISky == 1 ) {
						AVWLSU( iHour, JB + 1 ) += WLUMSU( iHour, JB + 1 ) * TVisIntWinMult;
						AVWLSUdisk( iHour, JB + 1 ) += WLUMSUdisk( iHour, JB + 1 ) * TVisIntWinDiskMult;
					}
					if ( PHRAY > 0.0 ) {
						EDIRSK( iHour, JB + 1, ISky ) += WLUMSK( iHour, JB + 1, ISky ) * DOMEGA_Ray_3_TVisIntWinMult;
						if ( ISky == 1 ) EDIRSU( iHour, JB + 1 ) += WLUMSU( iHour, JB + 1 ) * DOMEGA_Ray_3_TVisIntWinMult;
					}
					if ( ! SurfaceWindow( IWin ).MovableSlats ) break;
				}
			}
		}

	}

	void
	FigureRefPointDayltgFactorsToAddIllums(
		int const ZoneNum,
		int const iRefPoint,
		int const iHour,
		int & ISunPos,
		int const IWin,
		int const loopwin,
		int const NWX, // Number of window elements in x direction for dayltg calc
		int const NWY, // Number of window elements in y direction for dayltg calc
		int const ICtrl // Window control counter
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith, Oct 2012, derived from legacy code by Fred Winkelmann
		//       DATE WRITTEN   Oct. 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// calculation worker routine to fill daylighting coefficients

		// METHODOLOGY EMPLOYED:
		// this version is just for reference points.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const tmpDFCalc( 0.05 ); // cut off illuminance (lux) for exterior horizontal in calculating
		// the daylighting and glare factors

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ISky; // Sky type index: 1=clear, 2=clear turbid, 3=intermediate, 4=overcast
		int JSH; // Shading index: J=1 is unshaded window, J=2 is shaded window
		Real64 VTR; // For switchable glazing, ratio of visible transmittance of
		//  fully-switched state to that of the unswitched state

		if ( SUNCOSHR( iHour, 3 ) < SunIsUpValue ) return;

		++ISunPos;

		// Altitude of sun (degrees)
		PHSUN = PHSUNHR( iHour );
		SPHSUN = SPHSUNHR( iHour );
		CPHSUN = CPHSUNHR( iHour );

		// Azimuth of sun in absolute coord sys
		THSUN = THSUNHR( iHour );

		for ( ISky = 1; ISky <= 4; ++ISky ) { // Loop over sky types

			// Loop over shading index (1=bare window; 2=diffusing glazing, shade, screen or fixed slat-angle blind;
			// 2 to MaxSlatAngs+1 for variable slat-angle blind)

			// TH. 9/22/2009. CR 7625 - daylight illuminance spikes during some sunset hours due to the calculated sky and sun
			//  related daylight factors > 1, which theoretically can occur when sun is perpendicular to the window
			//  and interior surfaces with high visible reflectance.
			// Added tmpDFCalc (default to 0.05 lux) as the cap for GILSK and GILSU in calculating the daylight factors
			//  the assumption behind it is if exterior horizontal surface does not get daylight, spaces do not get daylight.

			for ( JSH = 1; JSH <= MaxSlatAngs + 1; ++JSH ) {
				if ( ! SurfaceWindow( IWin ).MovableSlats && JSH > 2 ) break;

				if ( GILSK( iHour, ISky ) > tmpDFCalc ) {
					ZoneDaylight( ZoneNum ).DaylIllFacSky( iHour, JSH, ISky, iRefPoint, loopwin ) = ( EDIRSK( iHour, JSH, ISky ) + EINTSK( iHour, JSH, ISky ) ) / GILSK( iHour, ISky );
					ZoneDaylight( ZoneNum ).DaylSourceFacSky( iHour, JSH, ISky, iRefPoint, loopwin ) = AVWLSK( iHour, JSH, ISky ) / ( NWX * NWY * GILSK( iHour, ISky ) );
					ZoneDaylight( ZoneNum ).DaylBackFacSky( iHour, JSH, ISky, iRefPoint, loopwin ) = EINTSK( iHour, JSH, ISky ) * ZoneDaylight( ZoneNum ).AveVisDiffReflect / ( Pi * GILSK( iHour, ISky ) );
				} else {
					ZoneDaylight( ZoneNum ).DaylIllFacSky( iHour, JSH, ISky, iRefPoint, loopwin ) = 0.0;
					ZoneDaylight( ZoneNum ).DaylSourceFacSky( iHour, JSH, ISky, iRefPoint, loopwin ) = 0.0;
					ZoneDaylight( ZoneNum ).DaylBackFacSky( iHour, JSH, ISky, iRefPoint, loopwin ) = 0.0;
				}

				if ( ISky == 1 ) {
					if ( GILSU( iHour ) > tmpDFCalc ) {
						ZoneDaylight( ZoneNum ).DaylIllFacSun( iHour, JSH, iRefPoint, loopwin ) = ( EDIRSU( iHour, JSH ) + EINTSU( iHour, JSH ) ) / ( GILSU( iHour ) + 0.0001 );
						ZoneDaylight( ZoneNum ).DaylIllFacSunDisk( iHour, JSH, iRefPoint, loopwin ) = ( EDIRSUdisk( iHour, JSH ) + EINTSUdisk( iHour, JSH ) ) / ( GILSU( iHour ) + 0.0001 );

						ZoneDaylight( ZoneNum ).DaylSourceFacSun( iHour, JSH, iRefPoint, loopwin ) = AVWLSU( iHour, JSH ) / ( NWX * NWY * ( GILSU( iHour ) + 0.0001 ) );
						ZoneDaylight( ZoneNum ).DaylSourceFacSunDisk( iHour, JSH, iRefPoint, loopwin ) = AVWLSUdisk( iHour, JSH ) / ( NWX * NWY * ( GILSU( iHour ) + 0.0001 ) );

						ZoneDaylight( ZoneNum ).DaylBackFacSun( iHour, JSH, iRefPoint, loopwin ) = EINTSU( iHour, JSH ) * ZoneDaylight( ZoneNum ).AveVisDiffReflect / ( Pi * ( GILSU( iHour ) + 0.0001 ) );
						ZoneDaylight( ZoneNum ).DaylBackFacSunDisk( iHour, JSH, iRefPoint, loopwin ) = EINTSUdisk( iHour, JSH ) * ZoneDaylight( ZoneNum ).AveVisDiffReflect / ( Pi * ( GILSU( iHour ) + 0.0001 ) );
					} else {
						ZoneDaylight( ZoneNum ).DaylIllFacSun( iHour, JSH, iRefPoint, loopwin ) = 0.0;
						ZoneDaylight( ZoneNum ).DaylIllFacSunDisk( iHour, JSH, iRefPoint, loopwin ) = 0.0;

						ZoneDaylight( ZoneNum ).DaylSourceFacSun( iHour, JSH, iRefPoint, loopwin ) = 0.0;
						ZoneDaylight( ZoneNum ).DaylSourceFacSunDisk( iHour, JSH, iRefPoint, loopwin ) = 0.0;

						ZoneDaylight( ZoneNum ).DaylBackFacSun( iHour, JSH, iRefPoint, loopwin ) = 0.0;
						ZoneDaylight( ZoneNum ).DaylBackFacSunDisk( iHour, JSH, iRefPoint, loopwin ) = 0.0;
					}
				}
			} // End of shading index loop, JSH

			// For switchable glazing put daylighting factors for switched (dark) state in IS=2 location
			if ( ICtrl > 0 ) {
				if ( WindowShadingControl( ICtrl ).ShadingType == WSC_ST_SwitchableGlazing ) {
					VTR = SurfaceWindow( IWin ).VisTransRatio;
					ZoneDaylight( ZoneNum ).DaylIllFacSky( iHour, 2, ISky, iRefPoint, loopwin ) = ZoneDaylight( ZoneNum ).DaylIllFacSky( iHour, 1, ISky, iRefPoint, loopwin ) * VTR;
					ZoneDaylight( ZoneNum ).DaylSourceFacSky( iHour, 2, ISky, iRefPoint, loopwin ) = ZoneDaylight( ZoneNum ).DaylSourceFacSky( iHour, 1, ISky, iRefPoint, loopwin ) * VTR;
					ZoneDaylight( ZoneNum ).DaylBackFacSky( iHour, 2, ISky, iRefPoint, loopwin ) = ZoneDaylight( ZoneNum ).DaylBackFacSky( iHour, 1, ISky, iRefPoint, loopwin ) * VTR;
					if ( ISky == 1 ) {
						ZoneDaylight( ZoneNum ).DaylIllFacSun( iHour, 2, iRefPoint, loopwin ) = ZoneDaylight( ZoneNum ).DaylIllFacSun( iHour, 1, iRefPoint, loopwin ) * VTR;
						ZoneDaylight( ZoneNum ).DaylSourceFacSun( iHour, 2, iRefPoint, loopwin ) = ZoneDaylight( ZoneNum ).DaylSourceFacSun( iHour, 1, iRefPoint, loopwin ) * VTR;
						ZoneDaylight( ZoneNum ).DaylBackFacSun( iHour, 2, iRefPoint, loopwin ) = ZoneDaylight( ZoneNum ).DaylBackFacSun( iHour, 1, iRefPoint, loopwin ) * VTR;
						ZoneDaylight( ZoneNum ).DaylIllFacSunDisk( iHour, 2, iRefPoint, loopwin ) = ZoneDaylight( ZoneNum ).DaylIllFacSunDisk( iHour, 1, iRefPoint, loopwin ) * VTR;
						ZoneDaylight( ZoneNum ).DaylSourceFacSunDisk( iHour, 2, iRefPoint, loopwin ) = ZoneDaylight( ZoneNum ).DaylSourceFacSunDisk( iHour, 1, iRefPoint, loopwin ) * VTR;
						ZoneDaylight( ZoneNum ).DaylBackFacSunDisk( iHour, 2, iRefPoint, loopwin ) = ZoneDaylight( ZoneNum ).DaylBackFacSunDisk( iHour, 1, iRefPoint, loopwin ) * VTR;
					}
				}
			} // ICtrl > 0

		} // End of sky type loop, ISky

	}

	void
	FigureMapPointDayltgFactorsToAddIllums(
		int const ZoneNum,
		int const MapNum,
		int const iMapPoint,
		int const iHour,
		int const IWin,
		int const loopwin,
		int const NWX, // Number of window elements in x direction for dayltg calc
		int const NWY, // Number of window elements in y direction for dayltg calc
		int const ICtrl // Window control counter
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         B. Griffith, Oct 2012, derived from legacy code by Fred Winkelmann, Peter Ellis, Linda Lawrie
		//       DATE WRITTEN   Nov. 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// calculation worker routine to fill daylighting coefficients

		// METHODOLOGY EMPLOYED:
		// this version is just for map points.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const tmpDFCalc( 0.05 ); // cut off illuminance (lux) for exterior horizontal in calculating
		// the daylighting and glare factors

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ISky; // Sky type index: 1=clear, 2=clear turbid, 3=intermediate, 4=overcast
		int JSH; // Shading index: J=1 is unshaded window, J=2 is shaded window
		Real64 VTR; // For switchable glazing, ratio of visible transmittance of
		//  fully-switched state to that of the unswitched state

		if ( SUNCOSHR( iHour, 3 ) < SunIsUpValue ) return;

		// Altitude of sun (degrees)
		PHSUN = PHSUNHR( iHour );
		SPHSUN = SPHSUNHR( iHour );
		CPHSUN = CPHSUNHR( iHour );

		// Azimuth of sun in absolute coord sys
		THSUN = THSUNHR( iHour );

		for ( ISky = 1; ISky <= 4; ++ISky ) { // Loop over sky types

			// Loop over shading index (1=bare window; 2=diffusing glazing, shade, screen or fixed slat-angle blind;
			// 2 to MaxSlatAngs+1 for variable slat-angle blind)

			// TH. 9/22/2009. CR 7625 - daylight illuminance spikes during some sunset hours due to the calculated sky and sun
			//  related daylight factors > 1, which theoretically can occur when sun is perpendicular to the window
			//  and interior surfaces with high visible reflectance.
			// Added tmpDFCalc (default to 0.05 lux) as the cap for GILSK and GILSU in calculating the daylight factors
			//  the assumption behind it is if exterior horizontal surface does not get daylight, spaces do not get daylight.

			for ( JSH = 1; JSH <= MaxSlatAngs + 1; ++JSH ) {
				if ( ! SurfaceWindow( IWin ).MovableSlats && JSH > 2 ) break;

				if ( GILSK( iHour, ISky ) > tmpDFCalc ) {
					IllumMapCalc( MapNum ).DaylIllFacSky( iHour, JSH, ISky, iMapPoint, loopwin ) = ( EDIRSK( iHour, JSH, ISky ) + EINTSK( iHour, JSH, ISky ) ) / GILSK( iHour, ISky );
					IllumMapCalc( MapNum ).DaylSourceFacSky( iHour, JSH, ISky, iMapPoint, loopwin ) = AVWLSK( iHour, JSH, ISky ) / ( NWX * NWY * GILSK( iHour, ISky ) );
					IllumMapCalc( MapNum ).DaylBackFacSky( iHour, JSH, ISky, iMapPoint, loopwin ) = EINTSK( iHour, JSH, ISky ) * ZoneDaylight( ZoneNum ).AveVisDiffReflect / ( Pi * GILSK( iHour, ISky ) );
				} else {
					IllumMapCalc( MapNum ).DaylIllFacSky( iHour, JSH, ISky, iMapPoint, loopwin ) = 0.0;
					IllumMapCalc( MapNum ).DaylSourceFacSky( iHour, JSH, ISky, iMapPoint, loopwin ) = 0.0;
					IllumMapCalc( MapNum ).DaylBackFacSky( iHour, JSH, ISky, iMapPoint, loopwin ) = 0.0;
				}

				if ( ISky == 1 ) {
					if ( GILSU( iHour ) > tmpDFCalc ) {
						IllumMapCalc( MapNum ).DaylIllFacSun( iHour, JSH, iMapPoint, loopwin ) = ( EDIRSU( iHour, JSH ) + EINTSU( iHour, JSH ) ) / ( GILSU( iHour ) + 0.0001 );
						IllumMapCalc( MapNum ).DaylIllFacSunDisk( iHour, JSH, iMapPoint, loopwin ) = ( EDIRSUdisk( iHour, JSH ) + EINTSUdisk( iHour, JSH ) ) / ( GILSU( iHour ) + 0.0001 );

						IllumMapCalc( MapNum ).DaylSourceFacSun( iHour, JSH, iMapPoint, loopwin ) = AVWLSU( iHour, JSH ) / ( NWX * NWY * ( GILSU( iHour ) + 0.0001 ) );
						IllumMapCalc( MapNum ).DaylSourceFacSunDisk( iHour, JSH, iMapPoint, loopwin ) = AVWLSUdisk( iHour, JSH ) / ( NWX * NWY * ( GILSU( iHour ) + 0.0001 ) );

						IllumMapCalc( MapNum ).DaylBackFacSun( iHour, JSH, iMapPoint, loopwin ) = EINTSU( iHour, JSH ) * ZoneDaylight( ZoneNum ).AveVisDiffReflect / ( Pi * ( GILSU( iHour ) + 0.0001 ) );
						IllumMapCalc( MapNum ).DaylBackFacSunDisk( iHour, JSH, iMapPoint, loopwin ) = EINTSUdisk( iHour, JSH ) * ZoneDaylight( ZoneNum ).AveVisDiffReflect / ( Pi * ( GILSU( iHour ) + 0.0001 ) );
					} else {
						IllumMapCalc( MapNum ).DaylIllFacSun( iHour, JSH, iMapPoint, loopwin ) = 0.0;
						IllumMapCalc( MapNum ).DaylIllFacSunDisk( iHour, JSH, iMapPoint, loopwin ) = 0.0;

						IllumMapCalc( MapNum ).DaylSourceFacSun( iHour, JSH, iMapPoint, loopwin ) = 0.0;
						IllumMapCalc( MapNum ).DaylSourceFacSunDisk( iHour, JSH, iMapPoint, loopwin ) = 0.0;

						IllumMapCalc( MapNum ).DaylBackFacSun( iHour, JSH, iMapPoint, loopwin ) = 0.0;
						IllumMapCalc( MapNum ).DaylBackFacSunDisk( iHour, JSH, iMapPoint, loopwin ) = 0.0;
					}
				}
			} // End of shading index loop, JSH

			// For switchable glazing put daylighting factors for switched (dark) state in IS=2 location
			if ( ICtrl > 0 ) {
				if ( WindowShadingControl( ICtrl ).ShadingType == WSC_ST_SwitchableGlazing ) {
					VTR = SurfaceWindow( IWin ).VisTransRatio;
					IllumMapCalc( MapNum ).DaylIllFacSky( iHour, 2, ISky, iMapPoint, loopwin ) = IllumMapCalc( MapNum ).DaylIllFacSky( iHour, 1, ISky, iMapPoint, loopwin ) * VTR;
					IllumMapCalc( MapNum ).DaylSourceFacSky( iHour, 2, ISky, iMapPoint, loopwin ) = IllumMapCalc( MapNum ).DaylSourceFacSky( iHour, 1, ISky, iMapPoint, loopwin ) * VTR;
					IllumMapCalc( MapNum ).DaylBackFacSky( iHour, 2, ISky, iMapPoint, loopwin ) = IllumMapCalc( MapNum ).DaylBackFacSky( iHour, 1, ISky, iMapPoint, loopwin ) * VTR;
					if ( ISky == 1 ) {
						IllumMapCalc( MapNum ).DaylIllFacSun( iHour, 2, iMapPoint, loopwin ) = IllumMapCalc( MapNum ).DaylIllFacSun( iHour, 1, iMapPoint, loopwin ) * VTR;
						IllumMapCalc( MapNum ).DaylSourceFacSun( iHour, 2, iMapPoint, loopwin ) = IllumMapCalc( MapNum ).DaylSourceFacSun( iHour, 1, iMapPoint, loopwin ) * VTR;
						IllumMapCalc( MapNum ).DaylBackFacSun( iHour, 2, iMapPoint, loopwin ) = IllumMapCalc( MapNum ).DaylBackFacSun( iHour, 1, iMapPoint, loopwin ) * VTR;
						IllumMapCalc( MapNum ).DaylIllFacSunDisk( iHour, 2, iMapPoint, loopwin ) = IllumMapCalc( MapNum ).DaylIllFacSunDisk( iHour, 1, iMapPoint, loopwin ) * VTR;
						IllumMapCalc( MapNum ).DaylSourceFacSunDisk( iHour, 2, iMapPoint, loopwin ) = IllumMapCalc( MapNum ).DaylSourceFacSunDisk( iHour, 1, iMapPoint, loopwin ) * VTR;
						IllumMapCalc( MapNum ).DaylBackFacSunDisk( iHour, 2, iMapPoint, loopwin ) = IllumMapCalc( MapNum ).DaylBackFacSunDisk( iHour, 1, iMapPoint, loopwin ) * VTR;
					}
				}
			} // ICtrl > 0

		} // End of sky type loop, ISky

	}

	void
	GetDaylightingParametersInput()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   Oct 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine provides a simple structure to get all daylighting
		// parameters.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		// RJH DElight Modification Begin
		using namespace DElightManagerF; // Module for managing DElight subroutines
		// RJH DElight Modification End
		using DataSystemVariables::GoodIOStatValue;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		static gio::Fmt fmtA( "(A)" );

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int TotDaylightingDetailed; // Total Daylighting:Detailed inputs
		int IntWin; // Interior window surface index
		bool ErrorsFound; // Error flag
		int SurfNum; // Surface counter (loop)
		int WindowShadingControlPtr; // Pointer for WindowShadingControl
		int ZoneNum; // Zone Number (loop counter)
		int SurfNumAdj; // Surface Number for adjacent surface
		int ZoneNumAdj; // Zone Number for adjacent zone
		// RJH DElight Modification Begin - local variable declarations
		int TotDaylightingDElight; // Total Daylighting:DElight inputs
		Real64 dLatitude; // double for argument passing
		int iErrorFlag; // Error Flag for warning/errors returned from DElight
		int iDElightErrorFile; // Unit number for reading DElight Error File
		int iReadStatus; // Error File Read Status
		std::string cErrorLine; // Each DElight Error line can be up to 210 characters long
		std::string cErrorMsg; // Each DElight Error Message can be up to 200 characters long
		bool bEndofErrFile; // End of Error File flag
		bool bRecordsOnErrFile; // true if there are records on the error file
		// RJH DElight Modification End - local variable declarations

		int NumReports;
		int NumNames;
		int NumNumbers;
		int IOStat;

		ErrorsFound = false;
		cCurrentModuleObject = "Daylighting:Controls";
		TotDaylightingDetailed = GetNumObjectsFound( cCurrentModuleObject );
		if ( TotDaylightingDetailed > 0 ) {
			GetDaylightingParametersDetaild( TotDaylightingDetailed, ErrorsFound );
			GetLightWellData( ErrorsFound );
			if ( ErrorsFound ) ShowFatalError( "Program terminated for above reasons, related to DAYLIGHTING" );
			DayltgSetupAdjZoneListsAndPointers();
		}

		for ( SurfNum = 1; SurfNum <= TotSurfaces; ++SurfNum ) {
			if ( Surface( SurfNum ).Class != SurfaceClass_Window ) continue;
			ZoneNum = Surface( SurfNum ).Zone;
			if ( ZoneDaylight( ZoneNum ).TotalDaylRefPoints > 0 ) {
				if ( ! SurfaceWindow( SurfNum ).SurfDayLightInit ) {
					SurfaceWindow( SurfNum ).SolidAngAtRefPt.allocate( MaxRefPoints );
					SurfaceWindow( SurfNum ).SolidAngAtRefPt = 0.0;
					SurfaceWindow( SurfNum ).SolidAngAtRefPtWtd.allocate( MaxRefPoints );
					SurfaceWindow( SurfNum ).SolidAngAtRefPtWtd = 0.0;
					SurfaceWindow( SurfNum ).IllumFromWinAtRefPt.allocate( 2, MaxRefPoints );
					SurfaceWindow( SurfNum ).IllumFromWinAtRefPt = 0.0;
					SurfaceWindow( SurfNum ).BackLumFromWinAtRefPt.allocate( 2, MaxRefPoints );
					SurfaceWindow( SurfNum ).BackLumFromWinAtRefPt = 0.0;
					SurfaceWindow( SurfNum ).SourceLumFromWinAtRefPt.allocate( 2, MaxRefPoints );
					SurfaceWindow( SurfNum ).SourceLumFromWinAtRefPt = 0.0;
					SurfaceWindow( SurfNum ).SurfDayLightInit = true;
				}
			} else {
				SurfNumAdj = Surface( SurfNum ).ExtBoundCond;
				if ( SurfNumAdj > 0 ) {
					ZoneNumAdj = Surface( SurfNumAdj ).Zone;
					if ( ZoneDaylight( ZoneNumAdj ).TotalDaylRefPoints > 0 ) {
						if ( ! SurfaceWindow( SurfNum ).SurfDayLightInit ) {
							SurfaceWindow( SurfNum ).SolidAngAtRefPt.allocate( MaxRefPoints );
							SurfaceWindow( SurfNum ).SolidAngAtRefPt = 0.0;
							SurfaceWindow( SurfNum ).SolidAngAtRefPtWtd.allocate( MaxRefPoints );
							SurfaceWindow( SurfNum ).SolidAngAtRefPtWtd = 0.0;
							SurfaceWindow( SurfNum ).IllumFromWinAtRefPt.allocate( 2, MaxRefPoints );
							SurfaceWindow( SurfNum ).IllumFromWinAtRefPt = 0.0;
							SurfaceWindow( SurfNum ).BackLumFromWinAtRefPt.allocate( 2, MaxRefPoints );
							SurfaceWindow( SurfNum ).BackLumFromWinAtRefPt = 0.0;
							SurfaceWindow( SurfNum ).SourceLumFromWinAtRefPt.allocate( 2, MaxRefPoints );
							SurfaceWindow( SurfNum ).SourceLumFromWinAtRefPt = 0.0;
							SurfaceWindow( SurfNum ).SurfDayLightInit = true;
						}
					}
				}
			}

			if ( Surface( SurfNum ).ExtBoundCond == ExternalEnvironment ) {

				WindowShadingControlPtr = Surface( SurfNum ).WindowShadingControlPtr;
				if ( WindowShadingControlPtr > 0 ) {
					if ( WindowShadingControl( WindowShadingControlPtr ).GlareControlIsActive ) {
						// Error if GlareControlIsActive but window is not in a Daylighting:Detailed zone
						if ( ZoneDaylight( Surface( SurfNum ).Zone ).TotalDaylRefPoints == 0 ) {
							ShowSevereError( "Window=" + Surface( SurfNum ).Name + " has Window Shading Control with" );
							ShowContinueError( "GlareControlIsActive = Yes but it is not in a Daylighting zone." );
							ShowContinueError( "Zone indicated=" + Zone( ZoneNum ).Name );
							ErrorsFound = true;
						}
						// Error if GlareControlIsActive and window is in a Daylighting:Detailed zone with
						// an interior window adjacent to another Daylighting:Detailed zone
						if ( ZoneDaylight( ZoneNum ).TotalDaylRefPoints > 0 ) {
							for ( IntWin = Zone( ZoneNum ).SurfaceFirst; IntWin <= Zone( ZoneNum ).SurfaceLast; ++IntWin ) {
								SurfNumAdj = Surface( IntWin ).ExtBoundCond;
								if ( Surface( IntWin ).Class == SurfaceClass_Window && SurfNumAdj > 0 ) {
									ZoneNumAdj = Surface( SurfNumAdj ).Zone;
									if ( ZoneDaylight( ZoneNumAdj ).TotalDaylRefPoints > 0 ) {
										ShowSevereError( "Window=" + Surface( SurfNum ).Name + " has Window Shading Control with" );
										ShowContinueError( "GlareControlIsActive = Yes and is in a Daylighting zone" );
										ShowContinueError( "that shares an interior window with another Daylighting zone" );
										ShowContinueError( "Adjacent Zone indicated=" + Zone( ZoneNumAdj ).Name );
										ErrorsFound = true;
									}
								}
							}
						}
					}

					if ( WindowShadingControl( WindowShadingControlPtr ).ShadingControlType == WSCT_MeetDaylIlumSetp ) {
						// Error if window has ShadingControlType = MeetDaylightingIlluminanceSetpoint &
						// but is not in a Daylighting:Detailed zone
						if ( ZoneDaylight( Surface( SurfNum ).Zone ).TotalDaylRefPoints == 0 ) {
							ShowSevereError( "Window=" + Surface( SurfNum ).Name + " has Window Shading Control with" );
							ShowContinueError( "MeetDaylightingIlluminanceSetpoint but it is not in a Daylighting zone." );
							ShowContinueError( "Zone indicated=" + Zone( ZoneNum ).Name );
							ErrorsFound = true;
						}
						// Error if window has ShadingControlType = MeetDaylightIlluminanceSetpoint and is in a &
						// Daylighting:Detailed zone with an interior window adjacent to another Daylighting:Detailed zone
						if ( ZoneDaylight( ZoneNum ).TotalDaylRefPoints > 0 ) {
							for ( IntWin = Zone( ZoneNum ).SurfaceFirst; IntWin <= Zone( ZoneNum ).SurfaceLast; ++IntWin ) {
								SurfNumAdj = Surface( IntWin ).ExtBoundCond;
								if ( Surface( IntWin ).Class == SurfaceClass_Window && SurfNumAdj > 0 ) {
									ZoneNumAdj = Surface( SurfNumAdj ).Zone;
									if ( ZoneDaylight( ZoneNumAdj ).TotalDaylRefPoints > 0 ) {
										ShowSevereError( "Window=" + Surface( SurfNum ).Name + " has Window Shading Control with" );
										ShowContinueError( "MeetDaylightIlluminanceSetpoint and is in a Daylighting zone" );
										ShowContinueError( "that shares an interior window with another Daylighting zone" );
										ShowContinueError( "Adjacent Zone indicated=" + Zone( ZoneNumAdj ).Name );
										ErrorsFound = true;
									}
								}
							}
						}
					}

				}
			}
		}

		// RJH DElight Modification Begin - Calls to DElight preprocessing subroutines
		TotDaylightingDElight = GetNumObjectsFound( "Daylighting:DELight:Controls" );
		if ( TotDaylightingDElight > 0 ) {
			dLatitude = Latitude;
			DisplayString( "Calculating DElight Daylighting Factors" );
			DElightInputGenerator();
			// Init Error Flag to 0 (no Warnings or Errors)
			DisplayString( "ReturnFrom DElightInputGenerator" );
			iErrorFlag = 0;
			DisplayString( "Calculating DElight DaylightCoefficients" );
			GenerateDElightDaylightCoefficients( dLatitude, iErrorFlag );
			// Check Error Flag for Warnings or Errors returning from DElight
			// RJH 2008-03-07: open file for READWRITE and DELETE file after processing
			DisplayString( "ReturnFrom DElight DaylightCoefficients Calc" );
			if ( iErrorFlag != 0 ) {
				// Open DElight Daylight Factors Error File for reading
				iDElightErrorFile = GetNewUnitNumber();
				{ IOFlags flags; flags.ACTION( "READWRITE" ); gio::open( iDElightErrorFile, DataStringGlobals::outputDelightDfdmpFileName, flags ); }

				// Sequentially read lines in DElight Daylight Factors Error File
				// and process them using standard EPlus warning/error handling calls
				// Process all error/warning messages first
				// Then, if any error has occurred, ShowFatalError to terminate processing
				bEndofErrFile = false;
				bRecordsOnErrFile = false;
				while ( ! bEndofErrFile ) {
					{ IOFlags flags; gio::read( iDElightErrorFile, fmtA, flags ) >> cErrorLine; iReadStatus = flags.ios(); }
					if ( iReadStatus < GoodIOStatValue ) {
						bEndofErrFile = true;
						continue;
					}
					bRecordsOnErrFile = true;
					// Is the current line a Warning message?
					if ( has_prefix( cErrorLine, "WARNING: " ) ) {
						cErrorMsg = cErrorLine.substr( 9 );
						ShowWarningError( cErrorMsg );
					}
					// Is the current line an Error message?
					if ( has_prefix( cErrorLine, "ERROR: " ) ) {
						cErrorMsg = cErrorLine.substr( 7 );
						ShowSevereError( cErrorMsg );
						iErrorFlag = 1;
					}
				}

				// Close and Delete DElight Error File
				if ( bRecordsOnErrFile ) {
					{ IOFlags flags; flags.DISPOSE( "DELETE" ); gio::close( iDElightErrorFile, flags ); }
				} else {
					{ IOFlags flags; flags.DISPOSE( "DELETE" ); gio::close( iDElightErrorFile, flags ); }
				}
				// If any DElight Error occurred then ShowFatalError to terminate
				if ( iErrorFlag > 0 ) {
					ErrorsFound = true;
				}
			} else {
				// Open, Close, and Delete DElight Daylight Factors Error File for reading
				iDElightErrorFile = GetNewUnitNumber();
				{ IOFlags flags; flags.ACTION( "READWRITE" ); gio::open( iDElightErrorFile, DataStringGlobals::outputDelightDfdmpFileName, flags ); }
				{ IOFlags flags; flags.DISPOSE( "DELETE" ); gio::close( iDElightErrorFile, flags ); }
			}
			SetupDElightOutput4EPlus();
		}
		// RJH DElight Modification End - Calls to DElight preprocessing subroutines

		// TH 6/3/2010, added to report daylight factors
		cCurrentModuleObject = "Output:DaylightFactors";
		NumReports = GetNumObjectsFound( cCurrentModuleObject );
		if ( NumReports > 0 ) {
			GetObjectItem( cCurrentModuleObject, 1, cAlphaArgs, NumNames, rNumericArgs, NumNumbers, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			if ( has_prefix( cAlphaArgs( 1 ), "SIZINGDAYS" ) ) {
				DFSReportSizingDays = true;
			} else if ( has_prefix( cAlphaArgs( 1 ), "ALLSHADOWCALCULATIONDAYS" ) ) {
				DFSReportAllShadowCalculationDays = true;
			}
		}

		if ( ErrorsFound ) ShowFatalError( "Program terminated for above reasons" );

	}

	void
	GetDaylightingParametersDetaild(
		int const TotDaylightingDetailed, // Total "simple" daylighting inputs
		bool & ErrorsFound
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   March 2002
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Obtain the user input data for Daylighting:Detailed objects in the input file.
		// For detailed daylighting, a calculation of interior daylight illuminance is done at one
		//    or two reference points; the illuminance level, setpoint and type of control
		//    system determines lighting power reduction.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// none

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::VerifyName;
		using InputProcessor::FindItemInList;
		using DataStringGlobals::CharSpace;
		using DataStringGlobals::CharComma;
		using DataStringGlobals::CharTab;
		using InternalHeatGains::CheckLightsReplaceableMinMaxForZone;
		using InternalHeatGains::GetDesignLightingLevelForZone;
		using General::TrimSigDigits;
		using General::RoundSigDigits;
		using namespace OutputReportPredefined;
		using ScheduleManager::GetScheduleIndex;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static gio::Fmt fmtA( "(A)" );

		// INTERFACE BLOCK SPECIFICATIONS: na
		// DERIVED TYPE DEFINITIONS: na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int IOStat;
		int Loop1;
		int MapStyleIn;
		int NumAlpha;
		int NumNumber;
		int ZoneNum;
		int MapNum;
		int RefPt;
		int X;
		int Y;
		int SurfLoop;
		int AddMapPoints;
		int ZoneFound;
		std::string refName;
		Real64 CosBldgRelNorth; // Cosine of Building rotation
		Real64 SinBldgRelNorth; // Sine of Building rotation
		Real64 CosZoneRelNorth; // Cosine of Zone rotation
		Real64 SinZoneRelNorth; // Sine of Zone rotation
		static Real64 CosBldgRotAppGonly( 0.0 ); // Cosine of the building rotation for appendix G only (relative north)
		static Real64 SinBldgRotAppGonly( 0.0 ); // Sine of the building rotation for appendix G only (relative north)
		Real64 Xb; // temp var for transformation calc
		Real64 Yb; // temp var for transformation calc
		Real64 Xo;
		Real64 XnoRot;
		Real64 Xtrans;
		Real64 Yo;
		Real64 YnoRot;
		Real64 Ytrans;
		bool doTransform;
		Real64 OldAspectRatio;
		Real64 NewAspectRatio;
		Real64 rLightLevel;
		Array1D_bool ZoneMsgDone;
		Array1D_int ZoneMapCount;

		// FLOW:

		// Calc cos and sin of Building Relative North values for later use in transforming Reference Point coordinates
		CosBldgRelNorth = std::cos( -( BuildingAzimuth + BuildingRotationAppendixG ) * DegToRadians );
		SinBldgRelNorth = std::sin( -( BuildingAzimuth + BuildingRotationAppendixG ) * DegToRadians );
		// these are only for Building Rotation for Appendix G when using world coordinate system
		CosBldgRotAppGonly = std::cos( -BuildingRotationAppendixG * DegToRadians );
		SinBldgRotAppGonly = std::sin( -BuildingRotationAppendixG * DegToRadians );

		doTransform = false;
		OldAspectRatio = 1.0;
		NewAspectRatio = 1.0;

		CheckForGeometricTransform( doTransform, OldAspectRatio, NewAspectRatio );

		// Get and initialize illuminance map objects
		cCurrentModuleObject = "Output:IlluminanceMap";
		TotIllumMaps = GetNumObjectsFound( cCurrentModuleObject );

		IllumMap.allocate( TotIllumMaps );
		IllumMapCalc.allocate( TotIllumMaps );
		ZoneMapCount.dimension( NumOfZones, 0 );

		if ( TotIllumMaps > 0 ) {
			for ( MapNum = 1; MapNum <= TotIllumMaps; ++MapNum ) {
				GetObjectItem( cCurrentModuleObject, MapNum, cAlphaArgs, NumAlpha, rNumericArgs, NumNumber, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
				IllumMap( MapNum ).Name = cAlphaArgs( 1 );
				IllumMap( MapNum ).Zone = FindItemInList( cAlphaArgs( 2 ), Zone );

				if ( IllumMap( MapNum ).Zone == 0 ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid " + cAlphaFieldNames( 2 ) + "=\"" + cAlphaArgs( 2 ) + "\"." );
					ErrorsFound = true;
				}

				IllumMapCalc( MapNum ).Zone = IllumMap( MapNum ).Zone;
				if ( IllumMap( MapNum ).Zone != 0 ) {
					++ZoneMapCount( IllumMap( MapNum ).Zone );
				}
				IllumMap( MapNum ).Z = rNumericArgs( 1 );

				IllumMap( MapNum ).Xmin = rNumericArgs( 2 );
				IllumMap( MapNum ).Xmax = rNumericArgs( 3 );
				if ( rNumericArgs( 2 ) > rNumericArgs( 3 ) ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "..." + cNumericFieldNames( 2 ) + '[' + RoundSigDigits( rNumericArgs( 2 ), 2 ) + "] must be <= " + cNumericFieldNames( 3 ) + '[' + RoundSigDigits( rNumericArgs( 3 ), 2 ) + "]." );
					ErrorsFound = true;
				}
				IllumMap( MapNum ).Xnum = rNumericArgs( 4 );
				if ( IllumMap( MapNum ).Xnum != 1 ) {
					IllumMap( MapNum ).Xinc = ( IllumMap( MapNum ).Xmax - IllumMap( MapNum ).Xmin ) / ( IllumMap( MapNum ).Xnum - 1 );
				} else {
					IllumMap( MapNum ).Xinc = 0.0;
				}

				IllumMap( MapNum ).Ymin = rNumericArgs( 5 );
				IllumMap( MapNum ).Ymax = rNumericArgs( 6 );
				if ( rNumericArgs( 5 ) > rNumericArgs( 6 ) ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", invalid entry." );
					ShowContinueError( "..." + cNumericFieldNames( 5 ) + '[' + RoundSigDigits( rNumericArgs( 5 ), 2 ) + "] must be <= " + cNumericFieldNames( 6 ) + '[' + RoundSigDigits( rNumericArgs( 6 ), 2 ) + "]." );
					ErrorsFound = true;
				}
				IllumMap( MapNum ).Ynum = rNumericArgs( 7 );
				if ( IllumMap( MapNum ).Ynum != 1 ) {
					IllumMap( MapNum ).Yinc = ( IllumMap( MapNum ).Ymax - IllumMap( MapNum ).Ymin ) / ( IllumMap( MapNum ).Ynum - 1 );
				} else {
					IllumMap( MapNum ).Yinc = 0.0;
				}
				if ( IllumMap( MapNum ).Xnum * IllumMap( MapNum ).Ynum > MaxMapRefPoints ) {
					ShowSevereError( cCurrentModuleObject + "=\"" + cAlphaArgs( 1 ) + "\", too many map points specified." );
					ShowContinueError( "..." + cNumericFieldNames( 4 ) + '[' + RoundSigDigits( IllumMap( MapNum ).Xnum ) + "] * " + cNumericFieldNames( 7 ) + '[' + RoundSigDigits( IllumMap( MapNum ).Ynum ) + "].= [" + RoundSigDigits( IllumMap( MapNum ).Xnum * IllumMap( MapNum ).Ynum ) + "] must be <= [" + RoundSigDigits( MaxMapRefPoints ) + "]." );
					ErrorsFound = true;
				}
			} // MapNum

			cCurrentModuleObject = "OutputControl:IlluminanceMap:Style";
			MapStyleIn = GetNumObjectsFound( cCurrentModuleObject );

			if ( MapStyleIn == 0 ) {
				cAlphaArgs( 1 ) = "COMMA";
				MapColSep = CharComma; //comma
			} else if ( MapStyleIn == 1 ) {
				GetObjectItem( cCurrentModuleObject, 1, cAlphaArgs, NumAlpha, rNumericArgs, NumNumber, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
				if ( cAlphaArgs( 1 ) == "COMMA" ) {
					MapColSep = CharComma; //comma
				} else if ( cAlphaArgs( 1 ) == "TAB" ) {
					MapColSep = CharTab; //tab
				} else if ( cAlphaArgs( 1 ) == "FIXED" || cAlphaArgs( 1 ) == "SPACE" ) {
					MapColSep = CharSpace; // space
				} else {
					MapColSep = CharComma; //comma
					ShowWarningError( cCurrentModuleObject + ": invalid " + cAlphaFieldNames( 1 ) + "=\"" + cAlphaArgs( 1 ) + "\", Commas will be used to separate fields." );
					cAlphaArgs( 1 ) = "COMMA";
				}
			}
			gio::write( OutputFileInits, fmtA ) << "! <Daylighting:Illuminance Maps>,#Maps,Style";
			ConvertCaseToLower( cAlphaArgs( 1 ), cAlphaArgs( 2 ) );
			cAlphaArgs( 1 ).erase( 1 );
			cAlphaArgs( 1 ) += cAlphaArgs( 2 ).substr( 1 );
			gio::write( OutputFileInits, "('Daylighting:Illuminance Maps,',A,',',A)" ) << TrimSigDigits( TotIllumMaps ) << cAlphaArgs( 1 );

		}

		for ( Loop1 = 1; Loop1 <= NumOfZones; ++Loop1 ) {
			ZoneDaylight( Loop1 ).ZoneToMap.allocate( ZoneMapCount( Loop1 ) );
			ZoneDaylight( Loop1 ).ZoneToMap = 0;
			ZoneDaylight( Loop1 ).MapCount = 0;
		}

		for ( MapNum = 1; MapNum <= TotIllumMaps; ++MapNum ) {
			if ( IllumMap( MapNum ).Zone == 0 ) continue;
			++ZoneDaylight( IllumMap( MapNum ).Zone ).MapCount;
			ZoneDaylight( IllumMap( MapNum ).Zone ).ZoneToMap( ZoneDaylight( IllumMap( MapNum ).Zone ).MapCount ) = MapNum;
		}

		ZoneMapCount.deallocate();

		cCurrentModuleObject = "Daylighting:Controls";
		for ( Loop1 = 1; Loop1 <= TotDaylightingDetailed; ++Loop1 ) {
			cAlphaArgs = "";
			rNumericArgs = 0.0;
			GetObjectItem( cCurrentModuleObject, Loop1, cAlphaArgs, NumAlpha, rNumericArgs, NumNumber, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			// First is Zone Name
			ZoneFound = FindItemInList( cAlphaArgs( 1 ), Zone );
			if ( ZoneFound == 0 ) {
				ShowSevereError( cCurrentModuleObject + ": invalid " + cAlphaFieldNames( 1 ) + "=\"" + cAlphaArgs( 1 ) + "\"." );
				ErrorsFound = true;
				continue;
			}
			auto & zone( Zone( ZoneFound ) );
			auto & zone_daylight( ZoneDaylight( ZoneFound ) );

			// Calc cos and sin of Zone Relative North values for later use in transforming Reference Point coordinates
			CosZoneRelNorth = std::cos( -zone.RelNorth * DegToRadians );
			SinZoneRelNorth = std::sin( -zone.RelNorth * DegToRadians );

			if ( zone_daylight.DaylightType != NoDaylighting ) {
				ShowSevereError( cCurrentModuleObject + ": Attempted to apply Detailed Daylighting to a Zone with Previous Daylighting" );
				ShowContinueError( "Error discovered for Zone=" + cAlphaArgs( 1 ) );
				ShowContinueError( "Previously applied Daylighting Type=" + DaylightTypes( zone_daylight.DaylightType ) );
				ErrorsFound = true;
				continue;
			}
			zone_daylight.DaylightType = DetailedDaylighting;
			zone_daylight.TotalDaylRefPoints = rNumericArgs( 1 );

			rLightLevel = GetDesignLightingLevelForZone( ZoneFound );
			CheckLightsReplaceableMinMaxForZone( ZoneFound );

			zone_daylight.DaylRefPtAbsCoord.allocate( 3, MaxRefPoints );
			zone_daylight.DaylRefPtAbsCoord = 0.0;
			zone_daylight.DaylRefPtInBounds.allocate( MaxRefPoints );
			zone_daylight.DaylRefPtInBounds = true;
			zone_daylight.FracZoneDaylit.allocate( MaxRefPoints );
			zone_daylight.FracZoneDaylit = 0.0;
			zone_daylight.IllumSetPoint.allocate( MaxRefPoints );
			zone_daylight.IllumSetPoint = 0.0;
			zone_daylight.RefPtPowerReductionFactor.allocate( MaxRefPoints );
			zone_daylight.RefPtPowerReductionFactor = 1.0;
			zone_daylight.DaylIllumAtRefPt.allocate( MaxRefPoints );
			zone_daylight.DaylIllumAtRefPt = 0.0;
			zone_daylight.GlareIndexAtRefPt.allocate( MaxRefPoints );
			zone_daylight.GlareIndexAtRefPt = 0.0;
			zone_daylight.BacLum.allocate( MaxRefPoints );
			zone_daylight.BacLum = 0.0;

			//added TH 12/2/2008
			zone_daylight.TimeExceedingGlareIndexSPAtRefPt.allocate( MaxRefPoints );
			zone_daylight.TimeExceedingGlareIndexSPAtRefPt = 0.0;

			//added TH 7/6/2009
			zone_daylight.TimeExceedingDaylightIlluminanceSPAtRefPt.allocate( MaxRefPoints );
			zone_daylight.TimeExceedingDaylightIlluminanceSPAtRefPt = 0.0;

			if ( zone_daylight.TotalDaylRefPoints >= 1 ) {
				if ( DaylRefWorldCoordSystem ) {
					//transform only by appendix G rotation
					zone_daylight.DaylRefPtAbsCoord( 1, 1 ) = rNumericArgs( 2 ) * CosBldgRotAppGonly - rNumericArgs( 3 ) * SinBldgRotAppGonly;
					zone_daylight.DaylRefPtAbsCoord( 2, 1 ) = rNumericArgs( 2 ) * SinBldgRotAppGonly + rNumericArgs( 3 ) * CosBldgRotAppGonly;
					zone_daylight.DaylRefPtAbsCoord( 3, 1 ) = rNumericArgs( 4 );
				} else {
					//Transform reference point coordinates into building coordinate system
					Xb = rNumericArgs( 2 ) * CosZoneRelNorth - rNumericArgs( 3 ) * SinZoneRelNorth + zone.OriginX;
					Yb = rNumericArgs( 2 ) * SinZoneRelNorth + rNumericArgs( 3 ) * CosZoneRelNorth + zone.OriginY;
					//Transform into World Coordinate System
					zone_daylight.DaylRefPtAbsCoord( 1, 1 ) = Xb * CosBldgRelNorth - Yb * SinBldgRelNorth;
					zone_daylight.DaylRefPtAbsCoord( 2, 1 ) = Xb * SinBldgRelNorth + Yb * CosBldgRelNorth;
					zone_daylight.DaylRefPtAbsCoord( 3, 1 ) = rNumericArgs( 4 ) + zone.OriginZ;
					if ( doTransform ) {
						Xo = zone_daylight.DaylRefPtAbsCoord( 1, 1 ); // world coordinates.... shifted by relative north angle...
						Yo = zone_daylight.DaylRefPtAbsCoord( 2, 1 );
						// next derotate the building
						XnoRot = Xo * CosBldgRelNorth + Yo * SinBldgRelNorth;
						YnoRot = Yo * CosBldgRelNorth - Xo * SinBldgRelNorth;
						// translate
						Xtrans = XnoRot * std::sqrt( NewAspectRatio / OldAspectRatio );
						Ytrans = YnoRot * std::sqrt( OldAspectRatio / NewAspectRatio );
						// rerotate
						zone_daylight.DaylRefPtAbsCoord( 1, 1 ) = Xtrans * CosBldgRelNorth - Ytrans * SinBldgRelNorth;

						zone_daylight.DaylRefPtAbsCoord( 2, 1 ) = Xtrans * SinBldgRelNorth + Ytrans * CosBldgRelNorth;
					}
				}
				zone_daylight.FracZoneDaylit( 1 ) = rNumericArgs( 8 );
				zone_daylight.IllumSetPoint( 1 ) = rNumericArgs( 10 );
			}
			if ( zone_daylight.TotalDaylRefPoints >= 2 ) {
				if ( DaylRefWorldCoordSystem ) {
					//transform only by appendix G rotation
					zone_daylight.DaylRefPtAbsCoord( 1, 2 ) = rNumericArgs( 5 ) * CosBldgRotAppGonly - rNumericArgs( 6 ) * SinBldgRotAppGonly;
					zone_daylight.DaylRefPtAbsCoord( 2, 2 ) = rNumericArgs( 5 ) * SinBldgRotAppGonly + rNumericArgs( 6 ) * CosBldgRotAppGonly;
					zone_daylight.DaylRefPtAbsCoord( 3, 2 ) = rNumericArgs( 7 );
				} else {
					//Transform reference point coordinates into building coordinate system
					Xb = rNumericArgs( 5 ) * CosZoneRelNorth - rNumericArgs( 6 ) * SinZoneRelNorth + zone.OriginX;
					Yb = rNumericArgs( 5 ) * SinZoneRelNorth + rNumericArgs( 6 ) * CosZoneRelNorth + zone.OriginY;
					//Transform into World Coordinate System
					zone_daylight.DaylRefPtAbsCoord( 1, 2 ) = Xb * CosBldgRelNorth - Yb * SinBldgRelNorth;
					zone_daylight.DaylRefPtAbsCoord( 2, 2 ) = Xb * SinBldgRelNorth + Yb * CosBldgRelNorth;
					zone_daylight.DaylRefPtAbsCoord( 3, 2 ) = rNumericArgs( 7 ) + zone.OriginZ;
					if ( doTransform ) {
						Xo = zone_daylight.DaylRefPtAbsCoord( 1, 2 ); // world coordinates.... shifted by relative north angle...
						Yo = zone_daylight.DaylRefPtAbsCoord( 2, 2 );
						// next derotate the building
						XnoRot = Xo * CosBldgRelNorth + Yo * SinBldgRelNorth;
						YnoRot = Yo * CosBldgRelNorth - Xo * SinBldgRelNorth;
						// translate
						Xtrans = XnoRot * std::sqrt( NewAspectRatio / OldAspectRatio );
						Ytrans = YnoRot * std::sqrt( OldAspectRatio / NewAspectRatio );
						// rerotate
						zone_daylight.DaylRefPtAbsCoord( 1, 2 ) = Xtrans * CosBldgRelNorth - Ytrans * SinBldgRelNorth;

						zone_daylight.DaylRefPtAbsCoord( 2, 2 ) = Xtrans * SinBldgRelNorth + Ytrans * CosBldgRelNorth;
					}
				}
				zone_daylight.FracZoneDaylit( 2 ) = rNumericArgs( 9 );
				zone_daylight.IllumSetPoint( 2 ) = rNumericArgs( 11 );
			}
			for ( RefPt = 1; RefPt <= zone_daylight.TotalDaylRefPoints; ++RefPt ) {
				if ( zone_daylight.DaylRefPtAbsCoord( 1, RefPt ) < zone.MinimumX || zone_daylight.DaylRefPtAbsCoord( 1, RefPt ) > zone.MaximumX ) {
					zone_daylight.DaylRefPtInBounds( RefPt ) = false;
					ShowWarningError( "GetDetailedDaylighting: Reference point X Value outside Zone Min/Max X, Zone=" + zone.Name );
					ShowContinueError( "...X Reference Point= " + RoundSigDigits( zone_daylight.DaylRefPtAbsCoord( 1, RefPt ), 2 ) + ", Zone Minimum X= " + RoundSigDigits( zone.MinimumX, 2 ) + ", Zone Maximum X= " + RoundSigDigits( zone.MaximumX, 2 ) );
					if ( zone_daylight.DaylRefPtAbsCoord( 1, RefPt ) < zone.MinimumX ) {
						ShowContinueError( "...X Reference Distance Outside MinimumX= " + RoundSigDigits( zone.MinimumX - zone_daylight.DaylRefPtAbsCoord( 1, RefPt ), 4 ) + " m." );
					} else {
						ShowContinueError( "...X Reference Distance Outside MaximumX= " + RoundSigDigits( zone_daylight.DaylRefPtAbsCoord( 1, RefPt ) - zone.MaximumX, 4 ) + " m." );
					}
				}
				if ( zone_daylight.DaylRefPtAbsCoord( 2, RefPt ) < zone.MinimumY || zone_daylight.DaylRefPtAbsCoord( 2, RefPt ) > zone.MaximumY ) {
					zone_daylight.DaylRefPtInBounds( RefPt ) = false;
					ShowWarningError( "GetDetailedDaylighting: Reference point Y Value outside Zone Min/Max Y, Zone=" + zone.Name );
					ShowContinueError( "...Y Reference Point= " + RoundSigDigits( zone_daylight.DaylRefPtAbsCoord( 2, RefPt ), 2 ) + ", Zone Minimum Y= " + RoundSigDigits( zone.MinimumY, 2 ) + ", Zone Maximum Y= " + RoundSigDigits( zone.MaximumY, 2 ) );
					if ( zone_daylight.DaylRefPtAbsCoord( 2, RefPt ) < zone.MinimumY ) {
						ShowContinueError( "...Y Reference Distance Outside MinimumY= " + RoundSigDigits( zone.MinimumY - zone_daylight.DaylRefPtAbsCoord( 2, RefPt ), 4 ) + " m." );
					} else {
						ShowContinueError( "...Y Reference Distance Outside MaximumY= " + RoundSigDigits( zone_daylight.DaylRefPtAbsCoord( 2, RefPt ) - zone.MaximumY, 4 ) + " m." );
					}
				}
				if ( zone_daylight.DaylRefPtAbsCoord( 3, RefPt ) < zone.MinimumZ || zone_daylight.DaylRefPtAbsCoord( 3, RefPt ) > zone.MaximumZ ) {
					zone_daylight.DaylRefPtInBounds( RefPt ) = false;
					ShowWarningError( "GetDetailedDaylighting: Reference point Z Value outside Zone Min/Max Z, Zone=" + zone.Name );
					ShowContinueError( "...Z Reference Point= " + RoundSigDigits( zone_daylight.DaylRefPtAbsCoord( 3, RefPt ), 2 ) + ", Zone Minimum Z= " + RoundSigDigits( zone.MinimumZ, 2 ) + ", Zone Maximum Z= " + RoundSigDigits( zone.MaximumZ, 2 ) );
					if ( zone_daylight.DaylRefPtAbsCoord( 3, RefPt ) < zone.MinimumZ ) {
						ShowContinueError( "...Z Reference Distance Outside MinimumZ= " + RoundSigDigits( zone.MinimumZ - zone_daylight.DaylRefPtAbsCoord( 3, RefPt ), 4 ) + " m." );
					} else {
						ShowContinueError( "...Z Reference Distance Outside MaximumZ= " + RoundSigDigits( zone_daylight.DaylRefPtAbsCoord( 3, RefPt ) - zone.MaximumZ, 4 ) + " m." );
					}
				}
			} // RefPt
			if ( sum( zone_daylight.FracZoneDaylit ) < 1.0 ) {
				ShowWarningError( "GetDetailedDaylighting: Fraction of Zone controlled by the Daylighting reference points is < 1.0." );
				ShowContinueError( "..discovered in \"" + cCurrentModuleObject + "\" for Zone=\"" + cAlphaArgs( 1 ) + "\", only " + RoundSigDigits( sum( zone_daylight.FracZoneDaylit ), 2 ) + " of the zone is controlled." );
			}
			if ( sum( zone_daylight.FracZoneDaylit ) > 1.0 ) {
				ShowSevereError( "GetDetailedDaylighting: Fraction of Zone controlled by the Daylighting reference points is > 1.0." );
				ShowContinueError( "..discovered in \"" + cCurrentModuleObject + "\" for Zone=\"" + cAlphaArgs( 1 ) + "\", trying to control " + RoundSigDigits( sum( zone_daylight.FracZoneDaylit ), 2 ) + " of the zone." );
				ErrorsFound = true;
			}
			zone_daylight.LightControlType = rNumericArgs( 12 ); // Relies on IDD limits for verification
			zone_daylight.ViewAzimuthForGlare = rNumericArgs( 13 );
			zone_daylight.MaxGlareallowed = rNumericArgs( 14 );
			zone_daylight.MinPowerFraction = rNumericArgs( 15 );
			zone_daylight.MinLightFraction = rNumericArgs( 16 );
			zone_daylight.LightControlSteps = rNumericArgs( 17 );
			if ( zone_daylight.LightControlType == 2 && zone_daylight.LightControlSteps <= 0 ) {
				ShowWarningError( "GetDetailedDaylighting: For Stepped Control, the number of steps must be > 0" );
				ShowContinueError( "..discovered in \"" + cCurrentModuleObject + "\" for Zone=\"" + cAlphaArgs( 1 ) + "\", will use 1" );
				zone_daylight.LightControlSteps = 1;
			}
			zone_daylight.LightControlProbability = rNumericArgs( 18 );

			if ( ! lAlphaFieldBlanks( 2 ) ) {
				zone_daylight.AvailSchedNum = GetScheduleIndex( cAlphaArgs( 2 ) );
				if ( zone_daylight.AvailSchedNum == 0 ) {
					ShowWarningError( "Invalid " + cAlphaFieldNames( 2 ) + " = " + cAlphaArgs( 2 ) + ", occurs in " + cCurrentModuleObject + "object for " + cAlphaFieldNames( 1 ) + "=\"" + cAlphaArgs( 1 ) );
					ShowContinueError( "Schedule was not found so controls will always be available, and the simulation continues." );
					zone_daylight.AvailSchedNum = ScheduleAlwaysOn;
				}
			} else {
				zone_daylight.AvailSchedNum = ScheduleAlwaysOn;
			}

			if ( zone_daylight.TotalDaylRefPoints >= 1 ) {
				refName = cAlphaArgs( 1 ) + " - REF 1";
				PreDefTableEntry( pdchDyLtZone, refName, cAlphaArgs( 1 ) );
				PreDefTableEntry( pdchDyLtKind, refName, "Detailed" );
				// (1=continuous, 2=stepped, 3=continuous/off)
				{ auto const SELECT_CASE_var( zone_daylight.LightControlType );
				if ( SELECT_CASE_var == 1 ) {
					PreDefTableEntry( pdchDyLtCtrl, refName, "Continuous" );
				} else if ( SELECT_CASE_var == 2 ) {
					PreDefTableEntry( pdchDyLtCtrl, refName, "Stepped" );
				} else if ( SELECT_CASE_var == 3 ) {
					PreDefTableEntry( pdchDyLtCtrl, refName, "Continuous/Off" );
				}}
				PreDefTableEntry( pdchDyLtFrac, refName, zone_daylight.FracZoneDaylit( 1 ) );
				PreDefTableEntry( pdchDyLtWInst, refName, rLightLevel );
				PreDefTableEntry( pdchDyLtWCtrl, refName, rLightLevel * zone_daylight.FracZoneDaylit( 1 ) );
			}
			if ( zone_daylight.TotalDaylRefPoints >= 2 ) {
				refName = cAlphaArgs( 1 ) + " - REF 2";
				PreDefTableEntry( pdchDyLtZone, refName, cAlphaArgs( 1 ) );
				PreDefTableEntry( pdchDyLtKind, refName, "Detailed" );
				// (1=continuous, 2=stepped, 3=continuous/off)
				{ auto const SELECT_CASE_var( zone_daylight.LightControlType );
				if ( SELECT_CASE_var == 1 ) {
					PreDefTableEntry( pdchDyLtCtrl, refName, "Continuous" );
				} else if ( SELECT_CASE_var == 2 ) {
					PreDefTableEntry( pdchDyLtCtrl, refName, "Stepped" );
				} else if ( SELECT_CASE_var == 3 ) {
					PreDefTableEntry( pdchDyLtCtrl, refName, "Continuous/Off" );
				}}
				PreDefTableEntry( pdchDyLtFrac, refName, zone_daylight.FracZoneDaylit( 2 ) );
				PreDefTableEntry( pdchDyLtWInst, refName, rLightLevel );
				PreDefTableEntry( pdchDyLtWCtrl, refName, rLightLevel * zone_daylight.FracZoneDaylit( 2 ) );
			}

			// Check for illuminance maps associated with this zone
			for ( MapNum = 1; MapNum <= TotIllumMaps; ++MapNum ) {
				if ( IllumMap( MapNum ).Zone == ZoneFound ) {
					if ( IllumMap( MapNum ).Xnum * IllumMap( MapNum ).Ynum > 0 ) {
						// Add additional daylighting reference points for map
						AddMapPoints = IllumMap( MapNum ).Xnum * IllumMap( MapNum ).Ynum;
						IllumMapCalc( MapNum ).TotalMapRefPoints = AddMapPoints;
						IllumMapCalc( MapNum ).MapRefPtAbsCoord.allocate( 3, AddMapPoints );
						IllumMapCalc( MapNum ).MapRefPtAbsCoord = 0.0;
						IllumMapCalc( MapNum ).MapRefPtInBounds.allocate( AddMapPoints );
						IllumMapCalc( MapNum ).MapRefPtInBounds = true;
						IllumMapCalc( MapNum ).DaylIllumAtMapPt.allocate( AddMapPoints );
						IllumMapCalc( MapNum ).DaylIllumAtMapPt = 0.0;
						IllumMapCalc( MapNum ).GlareIndexAtMapPt.allocate( AddMapPoints );
						IllumMapCalc( MapNum ).GlareIndexAtMapPt = 0.0;
						IllumMapCalc( MapNum ).DaylIllumAtMapPtHr.allocate( AddMapPoints );
						IllumMapCalc( MapNum ).DaylIllumAtMapPtHr = 0.0;
						IllumMapCalc( MapNum ).GlareIndexAtMapPtHr.allocate( AddMapPoints );
						IllumMapCalc( MapNum ).GlareIndexAtMapPtHr = 0.0;

						if ( AddMapPoints > MaxMapRefPoints ) {
							ShowSevereError( "GetDaylighting Parameters: Total Map Reference points entered is greater than maximum allowed." );
							ShowContinueError( "Occurs in Zone=" + zone.Name );
							ShowContinueError( "Maximum reference points allowed=" + TrimSigDigits( MaxMapRefPoints ) + ", entered amount (when error first occurred)=" + TrimSigDigits( AddMapPoints ) );
							ErrorsFound = true;
							break;
						}
						RefPt = 1;
						// Calc cos and sin of Zone Relative North values for later use in transforming Map Point coordinates
						//CosZoneRelNorth = std::cos( -zone.RelNorth * DegToRadians ); //Tuned These should not be changing
						//SinZoneRelNorth = std::sin( -zone.RelNorth * DegToRadians );
						if ( IllumMap( MapNum ).Xnum != 1 ) {
							IllumMap( MapNum ).Xinc = ( IllumMap( MapNum ).Xmax - IllumMap( MapNum ).Xmin ) / ( IllumMap( MapNum ).Xnum - 1 );
						} else {
							IllumMap( MapNum ).Xinc = 0.0;
						}
						if ( IllumMap( MapNum ).Ynum != 1 ) {
							IllumMap( MapNum ).Yinc = ( IllumMap( MapNum ).Ymax - IllumMap( MapNum ).Ymin ) / ( IllumMap( MapNum ).Ynum - 1 );
						} else {
							IllumMap( MapNum ).Yinc = 0.0;
						}

						// Map points and increments are stored in AbsCoord and then that is operated on if relative coords entered.
						for ( Y = 1; Y <= IllumMap( MapNum ).Ynum; ++Y ) {
							for ( X = 1; X <= IllumMap( MapNum ).Xnum; ++X ) {
								IllumMapCalc( MapNum ).MapRefPtAbsCoord( 1, RefPt ) = IllumMap( MapNum ).Xmin + ( X - 1 ) * IllumMap( MapNum ).Xinc;
								IllumMapCalc( MapNum ).MapRefPtAbsCoord( 2, RefPt ) = IllumMap( MapNum ).Ymin + ( Y - 1 ) * IllumMap( MapNum ).Yinc;
								IllumMapCalc( MapNum ).MapRefPtAbsCoord( 3, RefPt ) = IllumMap( MapNum ).Z;
								++RefPt;
							}
						}
						RefPt = 1;
						for ( Y = 1; Y <= IllumMap( MapNum ).Ynum; ++Y ) {
							for ( X = 1; X <= IllumMap( MapNum ).Xnum; ++X ) {
								if ( ! DaylRefWorldCoordSystem ) {
									Xb = IllumMapCalc( MapNum ).MapRefPtAbsCoord( 1, RefPt ) * CosZoneRelNorth - IllumMapCalc( MapNum ).MapRefPtAbsCoord( 2, RefPt ) * SinZoneRelNorth + zone.OriginX;
									Yb = IllumMapCalc( MapNum ).MapRefPtAbsCoord( 1, RefPt ) * SinZoneRelNorth + IllumMapCalc( MapNum ).MapRefPtAbsCoord( 2, RefPt ) * CosZoneRelNorth + zone.OriginY;
									IllumMapCalc( MapNum ).MapRefPtAbsCoord( 1, RefPt ) = Xb * CosBldgRelNorth - Yb * SinBldgRelNorth;
									IllumMapCalc( MapNum ).MapRefPtAbsCoord( 2, RefPt ) = Xb * SinBldgRelNorth + Yb * CosBldgRelNorth;
									IllumMapCalc( MapNum ).MapRefPtAbsCoord( 3, RefPt ) += zone.OriginZ;
									if ( doTransform ) {
										Xo = IllumMapCalc( MapNum ).MapRefPtAbsCoord( 1, RefPt ); // world coordinates.... shifted by relative north angle...
										Yo = IllumMapCalc( MapNum ).MapRefPtAbsCoord( 2, RefPt );
										// next derotate the building
										XnoRot = Xo * CosBldgRelNorth + Yo * SinBldgRelNorth;
										YnoRot = Yo * CosBldgRelNorth - Xo * SinBldgRelNorth;
										// translate
										Xtrans = XnoRot * std::sqrt( NewAspectRatio / OldAspectRatio );
										Ytrans = YnoRot * std::sqrt( OldAspectRatio / NewAspectRatio );
										// rerotate
										IllumMapCalc( MapNum ).MapRefPtAbsCoord( 1, RefPt ) = Xtrans * CosBldgRelNorth - Ytrans * SinBldgRelNorth;

										IllumMapCalc( MapNum ).MapRefPtAbsCoord( 2, RefPt ) = Xtrans * SinBldgRelNorth + Ytrans * CosBldgRelNorth;
									}
								} else {
									Xb = IllumMapCalc( MapNum ).MapRefPtAbsCoord( 1, RefPt );
									Yb = IllumMapCalc( MapNum ).MapRefPtAbsCoord( 2, RefPt );
									IllumMapCalc( MapNum ).MapRefPtAbsCoord( 1, RefPt ) = Xb * CosBldgRotAppGonly - Yb * SinBldgRotAppGonly;
									IllumMapCalc( MapNum ).MapRefPtAbsCoord( 2, RefPt ) = Xb * SinBldgRotAppGonly + Yb * CosBldgRotAppGonly;
								}
								if ( RefPt == 1 ) {
									IllumMap( MapNum ).Xmin = IllumMapCalc( MapNum ).MapRefPtAbsCoord( 1, RefPt );
									IllumMap( MapNum ).Ymin = IllumMapCalc( MapNum ).MapRefPtAbsCoord( 2, RefPt );
									IllumMap( MapNum ).Xmax = IllumMapCalc( MapNum ).MapRefPtAbsCoord( 1, RefPt );
									IllumMap( MapNum ).Ymax = IllumMapCalc( MapNum ).MapRefPtAbsCoord( 2, RefPt );
									IllumMap( MapNum ).Z = IllumMapCalc( MapNum ).MapRefPtAbsCoord( 3, RefPt );
								}
								IllumMap( MapNum ).Xmin = min( IllumMap( MapNum ).Xmin, IllumMapCalc( MapNum ).MapRefPtAbsCoord( 1, RefPt ) );
								IllumMap( MapNum ).Ymin = min( IllumMap( MapNum ).Ymin, IllumMapCalc( MapNum ).MapRefPtAbsCoord( 2, RefPt ) );
								IllumMap( MapNum ).Xmax = max( IllumMap( MapNum ).Xmax, IllumMapCalc( MapNum ).MapRefPtAbsCoord( 1, RefPt ) );
								IllumMap( MapNum ).Ymax = max( IllumMap( MapNum ).Ymax, IllumMapCalc( MapNum ).MapRefPtAbsCoord( 2, RefPt ) );
								if ( ( IllumMapCalc( MapNum ).MapRefPtAbsCoord( 1, RefPt ) < zone.MinimumX && ( zone.MinimumX - IllumMapCalc( MapNum ).MapRefPtAbsCoord( 1, RefPt ) ) > 0.001 ) || ( IllumMapCalc( MapNum ).MapRefPtAbsCoord( 1, RefPt ) > zone.MaximumX && ( IllumMapCalc( MapNum ).MapRefPtAbsCoord( 1, RefPt ) - zone.MaximumX ) > 0.001 ) || ( IllumMapCalc( MapNum ).MapRefPtAbsCoord( 2, RefPt ) < zone.MinimumY && ( zone.MinimumY - IllumMapCalc( MapNum ).MapRefPtAbsCoord( 2, RefPt ) ) > 0.001 ) || ( IllumMapCalc( MapNum ).MapRefPtAbsCoord( 2, RefPt ) > zone.MaximumY && ( IllumMapCalc( MapNum ).MapRefPtAbsCoord( 2, RefPt ) - zone.MaximumY ) > 0.001 ) || ( IllumMapCalc( MapNum ).MapRefPtAbsCoord( 3, RefPt ) < zone.MinimumZ && ( zone.MinimumZ - IllumMapCalc( MapNum ).MapRefPtAbsCoord( 3, RefPt ) ) > 0.001 ) || ( IllumMapCalc( MapNum ).MapRefPtAbsCoord( 3, RefPt ) > zone.MaximumZ && ( IllumMapCalc( MapNum ).MapRefPtAbsCoord( 3, RefPt ) - zone.MaximumZ ) > 0.001 ) ) {
									IllumMapCalc( MapNum ).MapRefPtInBounds( RefPt ) = false;
								}
								// Test extremes of Map Points against Zone Min/Max
								if ( RefPt == 1 || RefPt == IllumMapCalc( MapNum ).TotalMapRefPoints ) {
									if ( ( IllumMapCalc( MapNum ).MapRefPtAbsCoord( 1, RefPt ) < zone.MinimumX || IllumMapCalc( MapNum ).MapRefPtAbsCoord( 1, RefPt ) > zone.MaximumX ) && ! IllumMapCalc( MapNum ).MapRefPtInBounds( RefPt ) ) {
										ShowWarningError( "GetDetailedDaylighting: Reference Map point #[" + RoundSigDigits( RefPt ) + "], X Value outside Zone Min/Max X, Zone=" + zone.Name );
										ShowContinueError( "...X Reference Point= " + RoundSigDigits( IllumMapCalc( MapNum ).MapRefPtAbsCoord( 1, RefPt ), 2 ) + ", Zone Minimum X= " + RoundSigDigits( zone.MinimumX, 2 ) + ", Zone Maximum X= " + RoundSigDigits( zone.MaximumX, 2 ) );
										if ( IllumMapCalc( MapNum ).MapRefPtAbsCoord( 1, RefPt ) < zone.MinimumX ) {
											ShowContinueError( "...X Reference Distance Outside MinimumX= " + RoundSigDigits( zone.MinimumX - IllumMapCalc( MapNum ).MapRefPtAbsCoord( 1, RefPt ), 4 ) + " m." );
										} else {
											ShowContinueError( "...X Reference Distance Outside MaximumX= " + RoundSigDigits( IllumMapCalc( MapNum ).MapRefPtAbsCoord( 1, RefPt ) - zone.MaximumX, 4 ) + " m." );
										}
									}
									if ( ( IllumMapCalc( MapNum ).MapRefPtAbsCoord( 2, RefPt ) < zone.MinimumY || IllumMapCalc( MapNum ).MapRefPtAbsCoord( 2, RefPt ) > zone.MaximumY ) && ! IllumMapCalc( MapNum ).MapRefPtInBounds( RefPt ) ) {
										ShowWarningError( "GetDetailedDaylighting: Reference Map point #[" + RoundSigDigits( RefPt ) + "], Y Value outside Zone Min/Max Y, Zone=" + zone.Name );
										ShowContinueError( "...Y Reference Point= " + RoundSigDigits( IllumMapCalc( MapNum ).MapRefPtAbsCoord( 2, RefPt ), 2 ) + ", Zone Minimum Y= " + RoundSigDigits( zone.MinimumY, 2 ) + ", Zone Maximum Y= " + RoundSigDigits( zone.MaximumY, 2 ) );
										if ( IllumMapCalc( MapNum ).MapRefPtAbsCoord( 2, RefPt ) < zone.MinimumY ) {
											ShowContinueError( "...Y Reference Distance Outside MinimumY= " + RoundSigDigits( zone.MinimumY - IllumMapCalc( MapNum ).MapRefPtAbsCoord( 2, RefPt ), 4 ) + " m." );
										} else {
											ShowContinueError( "...Y Reference Distance Outside MaximumY= " + RoundSigDigits( IllumMapCalc( MapNum ).MapRefPtAbsCoord( 2, RefPt ) - zone.MaximumY, 4 ) + " m." );
										}
									}
									if ( ( IllumMapCalc( MapNum ).MapRefPtAbsCoord( 3, RefPt ) < zone.MinimumZ || IllumMapCalc( MapNum ).MapRefPtAbsCoord( 3, RefPt ) > zone.MaximumZ ) && ! IllumMapCalc( MapNum ).MapRefPtInBounds( RefPt ) ) {
										ShowWarningError( "GetDetailedDaylighting: Reference Map point #[" + RoundSigDigits( RefPt ) + "], Z Value outside Zone Min/Max Z, Zone=" + zone.Name );
										ShowContinueError( "...Z Reference Point= " + RoundSigDigits( IllumMapCalc( MapNum ).MapRefPtAbsCoord( 3, RefPt ), 2 ) + ", Zone Minimum Z= " + RoundSigDigits( zone.MinimumZ, 2 ) + ", Zone Maximum Z= " + RoundSigDigits( zone.MaximumZ, 2 ) );
										if ( IllumMapCalc( MapNum ).MapRefPtAbsCoord( 3, RefPt ) < zone.MinimumZ ) {
											ShowContinueError( "...Z Reference Distance Outside MinimumZ= " + RoundSigDigits( zone.MinimumZ - IllumMapCalc( MapNum ).MapRefPtAbsCoord( 3, RefPt ), 4 ) + " m." );
										} else {
											ShowContinueError( "...Z Reference Distance Outside MaximumZ= " + RoundSigDigits( IllumMapCalc( MapNum ).MapRefPtAbsCoord( 3, RefPt ) - zone.MaximumZ, 4 ) + " m." );
										}
									}
								}
								++RefPt;
							} // X
						} // Y

					}
				}
			} // MapNum

		}

		ZoneMsgDone.dimension( NumOfZones, false );
		for ( MapNum = 1; MapNum <= TotIllumMaps; ++MapNum ) {
			if ( IllumMap( MapNum ).Zone == 0 ) continue;
			if ( ZoneDaylight( IllumMap( MapNum ).Zone ).DaylightType != DetailedDaylighting && ! ZoneMsgDone( IllumMap( MapNum ).Zone ) ) {
				ShowSevereError( "Zone Name in Output:IlluminanceMap is not used for Daylighting:Controls=" + Zone( IllumMap( MapNum ).Zone ).Name );
				ErrorsFound = true;
			}
		}
		ZoneMsgDone.deallocate();

		if ( TotIllumMaps > 0 ) {
			gio::write( OutputFileInits, fmtA ) << "! <Daylighting:Illuminance Maps:Detail>,Name,Zone,XMin {m},XMax {m},Xinc {m},#X Points,YMin {m},YMax {m},Yinc {m},#Y Points,Z {m}";
		}
		for ( MapNum = 1; MapNum <= TotIllumMaps; ++MapNum ) {
			gio::write( OutputFileInits, "('Daylighting:Illuminance Maps:Detail',11(',',A))" ) << IllumMap( MapNum ).Name << Zone( IllumMap( MapNum ).Zone ).Name << RoundSigDigits( IllumMap( MapNum ).Xmin, 2 ) << RoundSigDigits( IllumMap( MapNum ).Xmax, 2 ) << RoundSigDigits( IllumMap( MapNum ).Xinc, 2 ) << RoundSigDigits( IllumMap( MapNum ).Xnum ) << RoundSigDigits( IllumMap( MapNum ).Ymin, 2 ) << RoundSigDigits( IllumMap( MapNum ).Ymax, 2 ) << RoundSigDigits( IllumMap( MapNum ).Yinc, 2 ) << RoundSigDigits( IllumMap( MapNum ).Ynum ) << RoundSigDigits( IllumMap( MapNum ).Z, 2 );
		}

		if ( ErrorsFound ) return;

		for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {

			if ( ZoneDaylight( ZoneNum ).TotalDaylRefPoints == 0 ) continue;

			if ( ZoneDaylight( ZoneNum ).TotalDaylRefPoints > 0 ) {
				SetupOutputVariable( "Daylighting Reference Point 1 Illuminance [lux]", ZoneDaylight( ZoneNum ).DaylIllumAtRefPt( 1 ), "Zone", "Average", Zone( ZoneNum ).Name );
				SetupOutputVariable( "Daylighting Reference Point 1 Glare Index []", ZoneDaylight( ZoneNum ).GlareIndexAtRefPt( 1 ), "Zone", "Average", Zone( ZoneNum ).Name );

				//added TH 12/2/2008 to calculate the time exceeding the glare index setpoint
				SetupOutputVariable( "Daylighting Reference Point 1 Glare Index Setpoint Exceeded Time [hr]", ZoneDaylight( ZoneNum ).TimeExceedingGlareIndexSPAtRefPt( 1 ), "Zone", "Sum", Zone( ZoneNum ).Name );

				//added TH 7/6/2009 to calculate the time exceeding the illuminance setpoint
				SetupOutputVariable( "Daylighting Reference Point 1 Daylight Illuminance Setpoint Exceeded Time [hr]", ZoneDaylight( ZoneNum ).TimeExceedingDaylightIlluminanceSPAtRefPt( 1 ), "Zone", "Sum", Zone( ZoneNum ).Name );
			}

			if ( ZoneDaylight( ZoneNum ).TotalDaylRefPoints > 1 ) {
				SetupOutputVariable( "Daylighting Reference Point 2 Illuminance [lux]", ZoneDaylight( ZoneNum ).DaylIllumAtRefPt( 2 ), "Zone", "Average", Zone( ZoneNum ).Name );
				SetupOutputVariable( "Daylighting Reference Point 2 Glare Index []", ZoneDaylight( ZoneNum ).GlareIndexAtRefPt( 2 ), "Zone", "Average", Zone( ZoneNum ).Name );

				//added TH 12/2/2008 to calculate the time exceeding the glare index setpoint
				SetupOutputVariable( "Daylighting Reference Point 2 Glare Index Setpoint Exceeded Time [hr]", ZoneDaylight( ZoneNum ).TimeExceedingGlareIndexSPAtRefPt( 2 ), "Zone", "Sum", Zone( ZoneNum ).Name );

				//added TH 7/6/2009 to calculate the time exceeding the illuminance setpoint
				SetupOutputVariable( "Daylighting Reference Point 2 Daylight Illuminance Setpoint Exceeded Time [hr]", ZoneDaylight( ZoneNum ).TimeExceedingDaylightIlluminanceSPAtRefPt( 2 ), "Zone", "Sum", Zone( ZoneNum ).Name );
			}
			SetupOutputVariable( "Daylighting Lighting Power Multiplier []", ZoneDaylight( ZoneNum ).ZonePowerReductionFactor, "Zone", "Average", Zone( ZoneNum ).Name );
		}

		for ( SurfLoop = 1; SurfLoop <= TotSurfaces; ++SurfLoop ) {
			if ( Surface( SurfLoop ).Class == SurfaceClass_Window && Surface( SurfLoop ).ExtSolar ) {
				if ( ZoneDaylight( Surface( SurfLoop ).Zone ).TotalDaylRefPoints > 0 && ! Zone( Surface( SurfLoop ).Zone ).HasInterZoneWindow ) {
					SetupOutputVariable( "Daylighting Window Reference Point 1 Illuminance [lux]", SurfaceWindow( SurfLoop ).IllumFromWinAtRefPt1Rep, "Zone", "Average", Surface( SurfLoop ).Name );
					SetupOutputVariable( "Daylighting Window Reference Point 1 View Luminance [cd/m2]", SurfaceWindow( SurfLoop ).LumWinFromRefPt1Rep, "Zone", "Average", Surface( SurfLoop ).Name );
					if ( ZoneDaylight( Surface( SurfLoop ).Zone ).TotalDaylRefPoints > 1 ) {
						SetupOutputVariable( "Daylighting Window Reference Point 2 Illuminance [lux]", SurfaceWindow( SurfLoop ).IllumFromWinAtRefPt2Rep, "Zone", "Average", Surface( SurfLoop ).Name );
						SetupOutputVariable( "Daylighting Window Reference Point 2 View Luminance [cd/m2]", SurfaceWindow( SurfLoop ).LumWinFromRefPt2Rep, "Zone", "Average", Surface( SurfLoop ).Name );
					}
				}
			}
		}

	}

	void
	CheckTDDsAndLightShelvesInDaylitZones()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Brent Griffith
		//       DATE WRITTEN   Dec 2007
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine checks daylighting input for TDDs and light shelfs
		//  which need to be checked after daylighting input has been read in (CR 7145)
		//  (eventually this should be changed once/if implementations change to decouple from daylighting calcs so that
		//  these devices can be used in models without daylighting controls
		// CR 7145 was for TDDs, but also implenting check for light shelves, the other "daylighting device"

		// METHODOLOGY EMPLOYED:
		// loop thru daylighting devices and check that their zones have daylight controls

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataHeatBalance::Zone;
		using DataDaylighting::ZoneDaylight;
		using DataDaylighting::NoDaylighting;
		using namespace DataDaylightingDevices;
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static gio::Fmt fmtA( "(A)" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int PipeNum; // TDD pipe object number
		int ShelfNum; // light shelf object number
		int SurfNum; // daylight device surface number
		bool ErrorsFound;
		static bool firstTime( true );

		if ( firstTime ) {
			CheckTDDZone.dimension( NumOfZones, true );
			firstTime = false;
		}

		ErrorsFound = false;

		for ( PipeNum = 1; PipeNum <= NumOfTDDPipes; ++PipeNum ) {
			SurfNum = TDDPipe( PipeNum ).Diffuser;
			if ( SurfNum > 0 ) {
				if ( ZoneDaylight( Surface( SurfNum ).Zone ).DaylightType == NoDaylighting ) {
					ShowSevereError( "DaylightingDevice:Tubular = " + TDDPipe( PipeNum ).Name + ":  is not connected to a Zone that has Daylighting.  " );
					ShowContinueError( "Add Daylighting:Controls (or Daylighting:DELight:Controls) to Zone named:  " + Zone( Surface( SurfNum ).Zone ).Name );
					ShowContinueError( "A sufficient control is provided on the .dbg file." );
					ErrorsFound = true;
					if ( CheckTDDZone( Surface( SurfNum ).Zone ) ) {
						gio::write( OutputFileDebug, fmtA ) << " ! Following control is to allow tubular reporting in this Zone";
						gio::write( OutputFileDebug, fmtA ) << "Daylighting:Controls,  !- this control controls 0% of zone.";
						gio::write( OutputFileDebug, fmtA ) << "   " + Zone( Surface( SurfNum ).Zone ).Name + ",  !- Zone Name";
						gio::write( OutputFileDebug, fmtA ) << "     1,   !- Total Daylighting Reference Points";
						if ( DaylRefWorldCoordSystem ) {
							// world coordinates, use zone origin for ref pt
							gio::write( OutputFileDebug, fmtA ) << "   " + RoundSigDigits( Zone( Surface( SurfNum ).Zone ).OriginX, 2 ) + ",   !- X-Coordinate of First Reference Point {m}";
							gio::write( OutputFileDebug, fmtA ) << "   " + RoundSigDigits( Zone( Surface( SurfNum ).Zone ).OriginY, 2 ) + ",   !- Y-Coordinate of First Reference Point {m}";
							gio::write( OutputFileDebug, fmtA ) << "   " + RoundSigDigits( Zone( Surface( SurfNum ).Zone ).OriginZ, 2 ) + ",   !- Z-Coordinate of First Reference Point {m}";
						} else {
							// relative coordinates, use 0,0,0 for ref pt
							gio::write( OutputFileDebug, fmtA ) << "   0.0,   !- X-Coordinate of First Reference Point {m}";
							gio::write( OutputFileDebug, fmtA ) << "   0.0,   !- Y-Coordinate of First Reference Point {m}";
							gio::write( OutputFileDebug, fmtA ) << "   0.0,   !- Z-Coordinate of First Reference Point {m}";
						}
						gio::write( OutputFileDebug, fmtA ) << "      ,   !- X-Coordinate of Second Reference Point";
						gio::write( OutputFileDebug, fmtA ) << "      ,   !- Y-Coordinate of Second Reference Point";
						gio::write( OutputFileDebug, fmtA ) << "      ,   !- Z-Coordinate of Second Reference Point";
						gio::write( OutputFileDebug, fmtA ) << "   0.0,   !- Fraction of Zone Controlled by First Reference Point";
						gio::write( OutputFileDebug, fmtA ) << "   0.0,   !- Fraction of Zone Controlled by Second Reference Point";
						gio::write( OutputFileDebug, fmtA ) << "   0.0,   !- Illuminance Setpoint at First Reference Point";
						gio::write( OutputFileDebug, fmtA ) << "   0.0,   !- Illuminance Setpoint at Second Reference Point";
						gio::write( OutputFileDebug, fmtA ) << "     3,   !- Lighting Control Type";
						gio::write( OutputFileDebug, fmtA ) << "   0.0,   !- Glare Calculation Azimuth Angle of View Direction Clockwise from Zone y-Axis";
						gio::write( OutputFileDebug, fmtA ) << "      ,   !- Maximum Allowable Discomfort Glare Index";
						gio::write( OutputFileDebug, fmtA ) << "   0.0,   !- Minimum Input Power Fraction for Continuous Dimming Control";
						gio::write( OutputFileDebug, fmtA ) << "   0.0,   !- Minimum Light Output Fraction for Continuous Dimming Control";
						gio::write( OutputFileDebug, fmtA ) << "     0,   !- Number of Stepped Control Steps";
						gio::write( OutputFileDebug, fmtA ) << "   0.0;   !- Probability Lighting will be Reset When Needed in Manual Stepped Control";

						CheckTDDZone( Surface( SurfNum ).Zone ) = false;
					}
				}

			} else { // SurfNum == 0
				// should not come here (would have already been caught in TDD get input), but is an error
				ShowSevereError( "DaylightingDevice:Tubular = " + TDDPipe( PipeNum ).Name + ":  Diffuser surface not found " );
				ErrorsFound = true;
			}
		} // PipeNum

		for ( ShelfNum = 1; ShelfNum <= NumOfShelf; ++ShelfNum ) {
			SurfNum = Shelf( ShelfNum ).Window;
			//    IF (SurfNum > 0) THEN
			//      IF (ZoneDaylight(Surface(SurfNum)%zone)%DaylightType == NoDaylighting) THEN
			//        CALL ShowSevereError('DaylightingDevice:Shelf = '//TRIM(Shelf(ShelfNum)%Name)// &
			//            ':  is not connected to a Zone that has Daylighting.  ')
			//        CALL ShowContinueError('Add Daylighting:Controls (or Daylighting:DELight:Controls) ' //&
			//            'to Zone named:  '//TRIM(Zone(Surface(SurfNum)%zone)%name) )
			//          ErrorsFound = .TRUE.
			//      ENDIF
			//    ELSE ! SurfNum == 0
			if ( SurfNum == 0 ) {
				// should not come here (would have already been caught in shelf get input), but is an error
				ShowSevereError( "DaylightingDevice:Shelf = " + Shelf( ShelfNum ).Name + ":  window not found " );
				ErrorsFound = true;
			}
		} // ShelfNum

		if ( ErrorsFound ) ShowFatalError( "CheckTDDsAndLightShelvesInDaylitZones: Errors in DAYLIGHTING input." );

	}

	void
	GetLightWellData( bool & ErrorsFound ) // If errors found in input
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   Apr 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Gets data for a light well associated with a rectangular exterior window.
		// Calculates light well efficiency, defined as the ratio of the amount of visible
		// solar radiation leaving a well to the amount entering the well.

		// METHODOLOGY EMPLOYED:
		// Based on fit to Fig. 8-21, "Efficiency factors for various depths of light wells
		// based on well-interreflectance values," Lighting Handbook, 8th Edition, Illuminating
		// Engineering Society of North America, 1993.

		// REFERENCES: see above.

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using InputProcessor::GetNumObjectsFound;
		using InputProcessor::GetObjectItem;
		using InputProcessor::FindItemInList;
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:na
		// INTERFACE BLOCK SPECIFICATIONS:na
		// DERIVED TYPE DEFINITIONS:na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int IOStat; // IO Status when calling get input subroutine
		int NumAlpha; // Number of alpha names being passed
		int NumProp; // Number of properties being passed
		int TotLightWells; // Total Light Well objects
		int loop; // DO loop index
		int SurfNum; // Surface number
		bool WrongSurfaceType; // True if associated surface is not an exterior window
		Real64 HeightWell; // Well height (from window to bottom of well) (m)
		Real64 PerimWell; // Well perimeter (at bottom of well) (m)
		Real64 AreaWell; // Well area (at bottom of well) (m2)
		Real64 VisReflWell; // Area-weighted visible reflectance of well walls
		Real64 WellCavRatio; // Well cavity ratio

		// Get the total number of Light Well objects
		cCurrentModuleObject = "DaylightingDevice:LightWell";
		TotLightWells = GetNumObjectsFound( cCurrentModuleObject );
		if ( TotLightWells == 0 ) return;

		for ( loop = 1; loop <= TotLightWells; ++loop ) {

			GetObjectItem( cCurrentModuleObject, loop, cAlphaArgs, NumAlpha, rNumericArgs, NumProp, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );

			SurfNum = FindItemInList( cAlphaArgs( 1 ), Surface );
			if ( SurfNum == 0 ) {
				ShowSevereError( cCurrentModuleObject + ": invalid " + cAlphaFieldNames( 1 ) + "=\"" + cAlphaArgs( 1 ) + "\" not found." );
			}

			// Check that associated surface is an exterior window
			WrongSurfaceType = false;
			if ( SurfNum != 0 ) {
				if ( Surface( SurfNum ).Class != SurfaceClass_Window && Surface( SurfNum ).ExtBoundCond != ExternalEnvironment ) WrongSurfaceType = true;
				if ( WrongSurfaceType ) {
					ShowSevereError( cCurrentModuleObject + ": invalid " + cAlphaFieldNames( 1 ) + "=\"" + cAlphaArgs( 1 ) + "\" - not an exterior window." );
					ErrorsFound = true;
				}
			}

			if ( ! ErrorsFound ) {

				// Associated surface is an exterior window; calculate light well efficiency.

				SurfaceWindow( SurfNum ).LightWellEff = 1.0;
				HeightWell = rNumericArgs( 1 );
				PerimWell = rNumericArgs( 2 );
				AreaWell = rNumericArgs( 3 );
				VisReflWell = rNumericArgs( 4 );

				// Warning if light well area is less than window area
				if ( AreaWell < ( Surface( SurfNum ).Area + SurfaceWindow( SurfNum ).DividerArea - 0.1 ) ) {
					ShowSevereError( cCurrentModuleObject + ": invalid " + cAlphaFieldNames( 1 ) + "=\"" + cAlphaArgs( 1 ) + "\" - Areas." );
					ShowContinueError( "has Area of Bottom of Well=" + RoundSigDigits( Surface( SurfNum ).Area, 1 ) + " that is less than window area=" + RoundSigDigits( AreaWell, 1 ) );
				}

				if ( HeightWell >= 0.0 && PerimWell > 0.0 && AreaWell > 0.0 ) {
					WellCavRatio = 2.5 * HeightWell * PerimWell / AreaWell;
					SurfaceWindow( SurfNum ).LightWellEff = std::exp( -WellCavRatio * ( 0.16368 - 0.14467 * VisReflWell ) );
				}

			}

		} // End of loop over light well objects

	}

	void
	DayltgGlare(
		int & IL, // Reference point index: 1=first ref pt, 2=second ref pt
		Real64 & BLUM, // Window background (surround) luminance (cd/m2)
		Real64 & GLINDX, // Glare index
		int & ZoneNum // Zone number
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   July 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// CALCULATE GLARE INDEX.

		// METHODOLOGY EMPLOYED:
		// Called from DayltgInteriorIllum.  Finds glare index at reference
		// point no. IL in a space using the Cornell/BRS large source
		// glare formula. BLUM is the background luminance (cd/m**2).
		// TH comment 1/21/2010: The SurfaceWindow(IWin)%ShadingFlag has to be set
		//  before calling this subroutine. For switchable glazings this is tricky
		//  because the ZoneDaylight(ZoneNum)%SourceLumFromWinAtRefPt(IL,2,loop)
		//  may change every time step to represent intermediate switched state.

		// REFERENCES:
		// Based on DOE-2.1E subroutine DGLARE.

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 GTOT; // Glare constant
		Real64 GTOT1; // Portion of glare constant
		Real64 GTOT2; // Portion of glare constant
		int IWin; // Window counter
		int IS; // Window shading index: 1=unshaded, 2=shaded
		int loop; // Loop index

		// FLOW:
		// Initialize glare constant
		GTOT = 0.0;

		// Loop over exterior windows associated with zone
		for ( loop = 1; loop <= ZoneDaylight( ZoneNum ).NumOfDayltgExtWins; ++loop ) {
			IWin = ZoneDaylight( ZoneNum ).DayltgExtWinSurfNums( loop );
			IS = 1;
			if ( ( SurfaceWindow( IWin ).ShadingFlag >= 1 && SurfaceWindow( IWin ).ShadingFlag <= 9 ) || SurfaceWindow( IWin ).SolarDiffusing ) IS = 2;
			// Conversion from ft-L to cd/m2, with cd/m2 = 0.2936 ft-L, gives the 0.4794 factor
			// below, which is (0.2936)**0.6
			GTOT1 = 0.4794 * ( std::pow( ZoneDaylight( ZoneNum ).SourceLumFromWinAtRefPt( loop, IS, IL ), 1.6 ) ) * std::pow( ZoneDaylight( ZoneNum ).SolidAngAtRefPtWtd( loop, IL ), 0.8 );
			GTOT2 = BLUM + 0.07 * ( std::sqrt( ZoneDaylight( ZoneNum ).SolidAngAtRefPt( loop, IL ) ) ) * ZoneDaylight( ZoneNum ).SourceLumFromWinAtRefPt( loop, IS, IL );
			GTOT += GTOT1 / ( GTOT2 + 0.000001 );
		}

		// Glare index (adding 0.000001 prevents LOG10 (0))
		GLINDX = 10.0 * std::log10( GTOT + 0.000001 );
		// Set glare index to zero for GTOT < 1
		GLINDX = max( 0.0, GLINDX );

	}

	void
	DayltgGlareWithIntWins(
		Array1< Real64 > & GLINDX, // Glare index
		int const ZoneNum // Zone number
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   March 2004
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculate daylighting glare index for zones with interior windows.

		// METHODOLOGY EMPLOYED:
		// Finds glare index at reference point IL in a daylit zone using the Cornell/BRS large source
		// glare formula. Takes into account inter-reflected illuminance from light entering
		// the zone through interior windows

		// REFERENCES:
		// Based on subroutine DayltgGlare.

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS

		// SUBROUTINE PARAMETER DEFINITIONS: na
		// INTERFACE BLOCK SPECIFICATIONS: na
		// DERIVED TYPE DEFINITIONS: na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int IL; // Reference point index: 1=first ref pt, 2=second ref pt
		Real64 GTOT; // Glare constant
		Real64 GTOT1; // Portion of glare constant
		Real64 GTOT2; // Portion of glare constant
		int IWin; // Window counter
		int IS; // Window shading index: 1=unshaded, 2=shaded
		Real64 BacLum; // Background luminance (cd/m2)
		int loop; // Loop index
		int RefPoints; // Number of daylighting reference points in zone
		// FLOW:
		// Initialize glare constant

		GTOT = 0.0;

		// Calculate background luminance including effect of inter-reflected illuminance from light
		// entering zone through its interior windows

		RefPoints = min( 2, ZoneDaylight( ZoneNum ).TotalDaylRefPoints );
		for ( IL = 1; IL <= RefPoints; ++IL ) {
			BacLum = ZoneDaylight( ZoneNum ).BacLum( IL ) + ZoneDaylight( ZoneNum ).InterReflIllFrIntWins * ZoneDaylight( ZoneNum ).AveVisDiffReflect / Pi;
			BacLum = max( ZoneDaylight( ZoneNum ).IllumSetPoint( IL ) * ZoneDaylight( ZoneNum ).AveVisDiffReflect / Pi, BacLum );

			// Loop over exterior windows associated with zone
			for ( loop = 1; loop <= ZoneDaylight( ZoneNum ).NumOfDayltgExtWins; ++loop ) {
				IWin = ZoneDaylight( ZoneNum ).DayltgExtWinSurfNums( loop );
				IS = 1;
				if ( ( SurfaceWindow( IWin ).ShadingFlag >= 1 && SurfaceWindow( IWin ).ShadingFlag <= 9 ) || SurfaceWindow( IWin ).SolarDiffusing ) IS = 2;
				// Conversion from ft-L to cd/m2, with cd/m2 = 0.2936 ft-L, gives the 0.4794 factor
				// below, which is (0.2936)**0.6
				GTOT1 = 0.4794 * ( std::pow( ZoneDaylight( ZoneNum ).SourceLumFromWinAtRefPt( loop, IS, IL ), 1.6 ) ) * std::pow( ZoneDaylight( ZoneNum ).SolidAngAtRefPtWtd( loop, IL ), 0.8 );
				GTOT2 = BacLum + 0.07 * ( std::sqrt( ZoneDaylight( ZoneNum ).SolidAngAtRefPt( loop, IL ) ) ) * ZoneDaylight( ZoneNum ).SourceLumFromWinAtRefPt( loop, IS, IL );
				GTOT += GTOT1 / ( GTOT2 + 0.000001 );
			}

			// Glare index
			GLINDX( IL ) = 10.0 * std::log10( GTOT + 0.000001 );
			// Set glare index to zero for GTOT < 1
			GLINDX( IL ) = max( 0.0, GLINDX( IL ) );
		}

	}

	void
	DayltgExtHorizIllum(
		Array1A< Real64 > HISK, // Horizontal illuminance from sky for different sky types
		Real64 & HISU // Horizontal illuminance from sun for unit beam normal
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   July 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates exterior daylight illuminance.

		// METHODOLOGY EMPLOYED:
		// Called by CalcDayltgCoefficients. Calculates illuminance
		// on unobstructed horizontal surface by integrating
		// over the luminance distribution of standard CIE skies.
		// Calculates horizontal beam illuminance.
		// REFERENCES:
		// Based on DOE-2.1E subroutine DHILL.

		// USE STATEMENTS:
		// na

		// Argument array dimensioning
		HISK.dim( 4 );

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		//  and overcast sky (lux)
		//   illuminance (lux)

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const NTH( 18 ); // Number of azimuth steps for sky integration
		int const NPH( 8 ); // Number of altitude steps for sky integration
		Real64 const DTH( ( 2.0 * Pi ) / double( NTH ) ); // Sky integration azimuth stepsize (radians)
		Real64 const DPH( PiOvr2 / double( NPH ) ); // Sky integration altitude stepsize (radians)

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int IPH; // Altitude index for sky integration
		int ITH; // Azimuth index for sky integration
		static Array1D< Real64 > PH( NPH ); // Altitude of sky element (radians)
		static Array1D< Real64 > TH( NTH ); // Azimuth of sky element (radians)
		int ISky; // Sky type index
		static Array1D< Real64 > SPHCPH( NPH ); // Sine times cosine of altitude of sky element
		static bool firstTime( true ); // flag for first time thru to initialize

		// FLOW:
		// Integrate to obtain illuminance from sky.
		// The contribution in lumens/m2 from a patch of sky at altitude PH and azimuth TH
		// is L(TH,PH)*SIN(PH)*COS(PH)*DTH*DPH, where L(TH,PH) is the luminance
		// of the patch in cd/m2.
		//  Init
		if ( firstTime ) {
			for ( IPH = 1; IPH <= NPH; ++IPH ) {
				PH( IPH ) = ( IPH - 0.5 ) * DPH;
				SPHCPH( IPH ) = std::sin( PH( IPH ) ) * std::cos( PH( IPH ) ); // DA = COS(PH)*DTH*DPH
			}
			for ( ITH = 1; ITH <= NTH; ++ITH ) {
				TH( ITH ) = ( ITH - 0.5 ) * DTH;
			}
			firstTime = false;
		}

		HISK = 0.0;

		// Sky integration
		for ( IPH = 1; IPH <= NPH; ++IPH ) {
			Real64 const PH_IPH( PH( IPH ) );
			Real64 const SPHCPH_IPH( SPHCPH( IPH ) );
			for ( ITH = 1; ITH <= NTH; ++ITH ) {
				Real64 const TH_ITH( TH( ITH ) );
				for ( ISky = 1; ISky <= 4; ++ISky ) {
					HISK( ISky ) += DayltgSkyLuminance( ISky, TH_ITH, PH_IPH ) * SPHCPH_IPH;
				}
			}
		}

		for ( ISky = 1; ISky <= 4; ++ISky ) {
			HISK( ISky ) *= DTH * DPH;
		}

		// Direct solar horizontal illum (for unit direct normal illuminance)
		HISU = SPHSUN * 1.0;

	}

	void
	DayltgHitObstruction(
		int const IHOUR, // Hour number
		int const IWin, // Window index
		Vector3< Real64 > const & R1, // Origin of ray (m)
		Vector3< Real64 > const & RN, // Unit vector along ray
		Real64 & ObTrans // Product of solar transmittances of exterior obstructions
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   July 1997
		//       MODIFIED       FCW, May 2003: update list of surface classes that qualify as obstructions;
		//                        add interior surfaces as possible obstructors;
		//                        return from DO loop over surfaces as soon as any obstruction is hit;
		//                      FCW, July 2003: change from returning whether an obstruction is hit or not
		//                        to product of solar transmittances of hit obstructions.
		//                      FCW, Nov 2003: remove interior surfaces as possible obstructors since there
		//                        is now a separate check for interior obstructions; exclude windows and
		//                        doors as obstructors since if they are obstructors their base surfaces will
		//                        also be obstructors
		//       RE-ENGINEERED  Sept 2015. Stuart Mentzer. Octree for performance.

		// PURPOSE OF THIS SUBROUTINE:
		// Determines the product of the solar transmittances of the obstructions hit by a ray
		// from R1 in the direction of vector RN.

		// REFERENCES:
		// Based on DOE-2.1E subroutine DHITSH.

		// Using/Aliasing
		using ScheduleManager::LookUpScheduleValue;

		// Local declarations
		int IType; // Surface type/class:  mirror surfaces of shading surfaces
		static Vector3< Real64 > HP; // Hit coordinates, if ray hits an obstruction
		bool hit; // True iff a particular obstruction is hit

		ObTrans = 1.0;

		auto const & window( Surface( IWin ) );
		auto const window_iBaseSurf( window.BaseSurf );

		// Loop over potentially obstructing surfaces, which can be building elements, like walls, or shadowing surfaces, like overhangs
		// Building elements are assumed to be opaque
		// A shadowing surface is opaque unless its transmittance schedule value is non-zero
		if ( TotSurfaces < octreeCrossover ) { // Linear search through surfaces

			for ( int ISurf = 1; ISurf <= TotSurfaces; ++ISurf ) {
				auto const & surface( Surface( ISurf ) );
				if ( ! surface.ShadowSurfPossibleObstruction ) continue;
				IType = surface.Class;
				if ( ( IType == SurfaceClass_Wall || IType == SurfaceClass_Roof || IType == SurfaceClass_Floor ) && ( ISurf != window_iBaseSurf ) ) {
					PierceSurface( ISurf, R1, RN, HP, hit );
					if ( hit ) { // Building element is hit (assumed opaque)
						ObTrans = 0.0;
						break;
					}
				} else if ( surface.ShadowingSurf ) {
					PierceSurface( ISurf, R1, RN, HP, hit );
					if ( hit ) { // Shading surface is hit
						// Get solar transmittance of the shading surface
						Real64 const Trans( surface.SchedShadowSurfIndex > 0 ? LookUpScheduleValue( surface.SchedShadowSurfIndex, IHOUR, 1 ) : 0.0 );
						if ( Trans < 1.e-6 ) {
							ObTrans = 0.0;
							break;
						} else {
							ObTrans *= Trans;
						}
					}
				}
			}

		} else { // Surface octree search

			auto const & window_base( window_iBaseSurf > 0 ? Surface( window_iBaseSurf ) : window );
			auto const window_base_p( &window_base );

			// Lambda function for the octree to test for surface hit and update transmittance if hit
			auto solarTransmittance = [=,&R1,&RN,&hit,&ObTrans]( SurfaceData const & surface ) -> bool {
				if ( ! surface.ShadowSurfPossibleObstruction ) return false; //Do Consider separate octree without filtered surfaces
				auto const sClass( surface.Class );
				if ( ( sClass == SurfaceClass_Wall || sClass == SurfaceClass_Roof || sClass == SurfaceClass_Floor ) && ( &surface != window_base_p ) ) {
					PierceSurface( surface, R1, RN, HP, hit );
					if ( hit ) { // Building element is hit (assumed opaque)
						ObTrans = 0.0;
						return true;
					}
				} else if ( surface.ShadowingSurf ) {
					PierceSurface( surface, R1, RN, HP, hit );
					if ( hit ) { // Shading surface is hit
						// Get solar transmittance of the shading surface
						Real64 const Trans( surface.SchedShadowSurfIndex > 0 ? LookUpScheduleValue( surface.SchedShadowSurfIndex, IHOUR, 1 ) : 0.0 );
						if ( Trans < 1.e-6 ) {
							ObTrans = 0.0;
							return true;
						} else {
							ObTrans *= Trans;
							return ObTrans == 0.0;
						}
					}
				}
				return false;
			};

			// Check octree surface candidates for hits: short circuits if zero transmittance reached
			Vector3< Real64 > const RN_inv( SurfaceOctreeCube::safe_inverse( RN ) );
			surfaceOctree.processSomeSurfaceRayIntersectsCube( R1, RN, RN_inv, solarTransmittance );

		}

	}

	void
	DayltgHitInteriorObstruction(
		int const IWin, // Window index
		Vector3< Real64 > const & R1, // Origin of ray (m)
		Vector3< Real64 > const & R2, // Destination of ray (m)
		bool & hit // True iff ray hits an obstruction
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   July 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  Sept 2015. Stuart Mentzer. Octree for performance.

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine checks for interior obstructions between reference point and window element.

		// Preconditions
		assert( magnitude( R2 - R1 ) > 0.0 ); // Protect normalize() from divide by zero

		// Local declarations
		int IType; // Surface type/class
		static Vector3< Real64 > HP; // Hit coordinates, if ray hits an obstruction
		static Vector3< Real64 > RN; // Unit vector along ray

		hit = false;
		RN = ( R2 - R1 ).normalize(); // Make unit vector
		Real64 const d12( distance( R1, R2 ) ); // Distance between R1 and R2

		auto const & window( Surface( IWin ) );
		auto const window_Zone( window.Zone );
		auto const window_iBaseSurf( window.BaseSurf );
		auto const & window_base( window_iBaseSurf > 0 ? Surface( window_iBaseSurf ) : window );
		auto const window_base_iExtBoundCond( window_base.ExtBoundCond );

		// Loop over potentially obstructing surfaces, which can be building elements, like walls, or shadowing surfaces, like overhangs
		if ( TotSurfaces < octreeCrossover ) { // Linear search through surfaces

			for ( int ISurf = 1; ISurf <= TotSurfaces; ++ISurf ) {
				auto const & surface( Surface( ISurf ) );
				IType = surface.Class;
				if ( ( surface.ShadowingSurf ) || // Shadowing surface
				 ( ( surface.Zone == window_Zone ) && // Wall/ceiling/floor is in same zone as window
				 ( IType == SurfaceClass_Wall || IType == SurfaceClass_Roof || IType == SurfaceClass_Floor ) &&
				 ( ISurf != window_iBaseSurf ) && ( ISurf != window_base_iExtBoundCond ) ) ) // Exclude window's base or base-adjacent surfaces
				{
					PierceSurface( ISurf, R1, RN, d12, HP, hit ); // Check if R2-R1 segment pierces surface
					if ( hit ) break; // Segment pierces surface: Don't check the rest
				}
			}

		} else { // Surface octree search

			auto const window_base_p( &window_base );
			auto const & window_base_adjacent( window_base_iExtBoundCond > 0 ? Surface( window_base_iExtBoundCond ) : window_base );
			auto const window_base_adjacent_p( &window_base_adjacent );

			// Lambda function for the octree to test for surface hit
			auto surfaceHit = [=,&R1,&hit]( SurfaceData const & surface ) -> bool {
				auto const sClass( surface.Class );
				if ( ( surface.ShadowingSurf ) || // Shadowing surface
				 ( ( surface.Zone == window_Zone ) && // Surface is in same zone as window
				 ( sClass == SurfaceClass_Wall || sClass == SurfaceClass_Roof || sClass == SurfaceClass_Floor ) && // Wall, ceiling/roof, or floor
				 ( &surface != window_base_p ) && ( &surface != window_base_adjacent_p ) ) ) // Exclude window's base or base-adjacent surfaces
				{
					PierceSurface( surface, R1, RN, d12, HP, hit ); // Check if R2-R1 segment pierces surface
					return hit;
				} else {
					return false;
				}
			};

			// Check octree surface candidates until a hit is found, if any
			surfaceOctree.hasSurfaceSegmentIntersectsCube( R1, R2, surfaceHit );

		}

	}

	void
	DayltgHitBetWinObstruction(
		int const IWin1, // Surface number of origin window
		int const IWin2, // Surface number of destination window
		Vector3< Real64 > const & R1, // Origin of ray (on IWin1) (m)
		Vector3< Real64 > const & R2, // Destination of ray (on IWin2) (m)
		bool & hit // True iff ray hits an obstruction
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   Feb 2004
		//       MODIFIED na
		//       RE-ENGINEERED  Sept 2015. Stuart Mentzer. Octree for performance.

		// PURPOSE OF THIS SUBROUTINE:
		// Determines if a ray from point R1 on window IWin1 to point R2
		// on window IWin2 hits an obstruction

		// Preconditions
		assert( magnitude( R2 - R1 ) > 0.0 ); // Protect normalize() from divide by zero

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int IType; // Surface type/class
		static Vector3< Real64 > HP; // Hit coordinates, if ray hits an obstruction surface (m)
		static Vector3< Real64 > RN; // Unit vector along ray from R1 to R2

		hit = false;
		RN = ( R2 - R1 ).normalize(); // Unit vector
		Real64 const d12( distance( R1, R2 ) ); // Distance between R1 and R2 (m)

		auto const & window1( Surface( IWin1 ) );
		auto const window1_iBaseSurf( window1.BaseSurf );
		auto const & window1_base( window1_iBaseSurf > 0 ? Surface( window1_iBaseSurf ) : window1 );
		auto const window1_base_iExtBoundCond( window1_base.ExtBoundCond );

		auto const & window2( Surface( IWin2 ) );
		auto const window2_Zone( window2.Zone );
		auto const window2_iBaseSurf( window2.BaseSurf );
		auto const & window2_base( window2_iBaseSurf > 0 ? Surface( window2_iBaseSurf ) : window2 );
		auto const window2_base_iExtBoundCond( window2_base.ExtBoundCond );

		// Preconditions
//		assert( window1.Zone == window2_Zone ); //? This is violated in PurchAirWithDoubleFacadeDaylighting so then why the asymmetry of only checking for wall/roof/floor for window2 zone below?

		// Loop over potentially obstructing surfaces, which can be building elements, like walls, or shadowing surfaces, like overhangs
		if ( TotSurfaces < octreeCrossover ) { // Linear search through surfaces

			for ( int ISurf = 1; ISurf <= TotSurfaces; ++ISurf ) {
				auto const & surface( Surface( ISurf ) );
				IType = surface.Class;
				if ( ( surface.ShadowingSurf ) || // Shadowing surface
				 ( ( surface.Zone == window2_Zone ) && // Wall/ceiling/floor is in same zone as windows
				 ( IType == SurfaceClass_Wall || IType == SurfaceClass_Roof || IType == SurfaceClass_Floor ) && // Wall, ceiling/roof, or floor
				 ( ISurf != window1_iBaseSurf ) && ( ISurf != window2_iBaseSurf ) && // Exclude windows' base surfaces
				 ( ISurf != window1_base_iExtBoundCond ) && ( ISurf != window2_base_iExtBoundCond ) ) ) // Exclude windows' base-adjacent surfaces
				{
					PierceSurface( ISurf, R1, RN, d12, HP, hit ); // Check if R2-R1 segment pierces surface
					if ( hit ) break; // Segment pierces surface: Don't check the rest
				}
			}

		} else { // Surface octree search

			auto const window1_base_p( &window1_base );
			auto const & window1_base_adjacent( window1_base_iExtBoundCond > 0 ? Surface( window1_base_iExtBoundCond ) : window1_base );
			auto const window1_base_adjacent_p( &window1_base_adjacent );

			auto const window2_base_p( &window2_base );
			auto const & window2_base_adjacent( window2_base_iExtBoundCond > 0 ? Surface( window2_base_iExtBoundCond ) : window2_base );
			auto const window2_base_adjacent_p( &window2_base_adjacent );

			// Lambda function for the octree to test for surface hit
			auto surfaceHit = [=,&R1,&hit]( SurfaceData const & surface ) -> bool {
				auto const sClass( surface.Class );
				if ( ( surface.ShadowingSurf ) || // Shadowing surface
				 ( ( surface.Zone == window2_Zone ) && // Surface is in same zone as window
				 ( sClass == SurfaceClass_Wall || sClass == SurfaceClass_Roof || sClass == SurfaceClass_Floor ) && // Wall, ceiling/roof, or floor
				 ( &surface != window1_base_p ) && ( &surface != window2_base_p ) && // Exclude windows' base surfaces
				 ( &surface != window1_base_adjacent_p ) && ( &surface != window2_base_adjacent_p ) ) ) // Exclude windows' base-adjacent surfaces
				{
					PierceSurface( surface, R1, RN, d12, HP, hit ); // Check if R2-R1 segment pierces surface
					return hit;
				} else {
					return false;
				}
			};

			// Check octree surface candidates until a hit is found, if any
			surfaceOctree.hasSurfaceSegmentIntersectsCube( R1, R2, surfaceHit );

		}

	}

	void
	DayltgInteriorIllum( int & ZoneNum ) // Zone number
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   July 1997
		//       MODIFIED       March 2000, FCW: interpolate clear-sky daylight factors using
		//                      HourOfDay/WeightNow and NextHour/WeightNextHour. Previously
		//                      only HourOfDay was used
		//                      Jan 2001, FCW: interpolate in slat angle for windows with blinds
		//                      that have movable slats
		//                      Oct 2002, LKL: changed interpolation steps to HourOfDay/WeightNow
		//                      LastHour/WeightPreviousHour
		//                      Aug 2003, FCW: fix bug that prevented ShadingControlType =
		//                      MEETDAYLIGHTILLUMINANCESETPOINT from working
		//                      Mar 2004, FCW: fix bug in calc of illuminance setpoint contribution
		//                      to background luminance: now it is divided by pi to give cd/m2
		//                      Mar 2004, FCW: modify to handle daylighting through interior windows
		//                      June 2009, TH: modified for thermochromic windows
		//                      Jan 2010, TH (CR 7984): added iterations for switchable windows with shading
		//                       control of MeetDaylightIlluminanceSetpoint and glare control is active
		//                       Also corrected bugs (CR 7988) for switchable glazings not related to CR 7984

		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Using daylighting factors and exterior illuminance, determine
		// the current-hour interior daylight illuminance and glare index
		// at each reference point in a space. Deploy window shading window by window
		// if glare control is active for window and if the acceptable glare index
		// is exceeded at both reference points.

		// Called by InitSurfaceHeatBalance.

		// METHODOLOGY EMPLOYED:na

		// REFERENCES:
		// Based on DOE-2.1E subroutine DINTIL.

		// Using/Aliasing
		using General::POLYF;
		using General::InterpSlatAng;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		Real64 const tmpSWIterStep( 0.05 ); // step of switching factor, assuming maximum of 20 switching states

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NREFPT; // Number of daylighting reference points
		int ISky; // Sky type index
		int ISky1; // Sky type index values for averaging two sky types
		int ISky2;
		static Vector2< Real64 > SetPnt; // Illuminance setpoint at reference points (lux)
		static Array2D< Real64 > DFSKHR( 2, 4 ); // Sky daylight factor for sky type (first index),
		//   bare/shaded window (second index)
		static Vector2< Real64 > DFSUHR; // Sun daylight factor for bare/shaded window
		static Array2D< Real64 > BFSKHR( 2, 4 ); // Sky background luminance factor for sky type (first index),
		//   bare/shaded window (second index)
		static Vector2< Real64 > BFSUHR; // Sun background luminance factor for bare/shaded window
		static Array2D< Real64 > SFSKHR( 2, 4 ); // Sky source luminance factor for sky type (first index),
		//   bare/shaded window (second index)
		static Vector2< Real64 > SFSUHR; // Sun source luminance factor for bare/shaded window
		static Array2D< Real64 > WDAYIL( 2, 2 ); // Illuminance from window at reference point (first index)
		//   for shade open/closed (second index)
		static Array2D< Real64 > WBACLU( 2, 2 ); // Background illuminance from window at reference point (first index)
		//   for shade open/closed (second index)
		static Vector2< Real64 > RDAYIL; // Illuminance from window at reference point after closing shade
		static Vector2< Real64 > RBACLU; // Background illuminance from window at reference point after closing shade
		static Vector2< Real64 > GLRNDX; // Glare index at reference point
		static Vector2< Real64 > GLRNEW; // New glare index at reference point
		int IL; // Reference point index
		int IWin; // Window index
		int IS; // IS=1 for unshaded window, =2 for shaded window
		int ISWFLG; // Switchable glazing flag: =1 if one or more windows in a zone
		//  has switchable glazing that adjusts visible transmittance to just meet
		//  daylighting setpoint; =0 otherwise.
		int IConst; // Window construction pointer
		int IConstShaded; // Pointer to shaded window construction
		int ICtrl; // Window shading control pointer
		Real64 DILLSW; // Illuminance a ref point from windows that can be switched,
		Real64 DILLUN;
		//  and from those that can't (lux)
		Real64 ASETIL; // Illuminance ratio (lux)
		Real64 TVIS1; // Visible transmittance at normal incidence of unswitched glazing
		Real64 TVIS2; // Visible transmittance at normal incidence of fully-switched glazing
		Real64 VTRAT; // Ratio between switched and unswitched visible transmittance at normal incidence
		Real64 BACL; // Window background (surround) luminance for glare calc (cd/m2)
		Real64 SkyWeight; // Weighting factor used to average two different sky types
		static Vector4< Real64 > HorIllSky; // Horizontal illuminance for different sky types
		Real64 HorIllSkyFac; // Ratio between horizontal illuminance from sky horizontal irradiance and
		//   luminous efficacy and horizontal illuminance from averaged sky
		Real64 SlatAng; // Blind slat angle (rad)
		bool VarSlats; // True if slats are movable, i.e., variable angle
		bool GlareFlag; // True if maximum glare is exceeded
		int loop; // Loop index

		Real64 VTRatio; // VT (visible transmittance) ratio = VTNow / VTMaster
		Real64 VTNow; // VT of the time step actual TC window
		Real64 VTMaster; // VT of the base/master TC window

		// Added variables for glare iterations for switchable glazings
		static Real64 tmpSWSL1( 0.0 );
		static Real64 tmpSWSL2( 0.0 );
		static Real64 tmpSWFactor( 0.0 ); // new switching factor to meet glare criteria
//		static Real64 tmpSWFactor0( 0.0 ); // original switching factor to meet daylight illuminance //Unused Set but never used
		static Real64 tmpMult( 0.0 );
		static bool GlareOK( false );
		static Array3D< Real64 > tmpIllumFromWinAtRefPt;
		static Array3D< Real64 > tmpBackLumFromWinAtRefPt;
		static Array3D< Real64 > tmpSourceLumFromWinAtRefPt;
		static bool firstTime( true ); // true first time routine is called

		static bool blnCycle( false );

		// Three arrays to save original clear and dark (fully switched) states'
		//  zone/window daylighting properties.
		if ( firstTime ) {
			int const d1( max( maxval( Zone, &ZoneData::NumSubSurfaces ), maxval( ZoneDaylight, &ZoneDaylightCalc::NumOfDayltgExtWins ) ) );
			tmpIllumFromWinAtRefPt.allocate( d1, 2, 2 );
			tmpBackLumFromWinAtRefPt.allocate( d1, 2, 2 );
			tmpSourceLumFromWinAtRefPt.allocate( d1, 2, 2 );
			firstTime = false;
		}
		tmpIllumFromWinAtRefPt = 0.0;
		tmpBackLumFromWinAtRefPt = 0.0;
		tmpSourceLumFromWinAtRefPt = 0.0;

		// FLOW:
		// Limit the number of control reference points to 2
		NREFPT = ZoneDaylight( ZoneNum ).TotalDaylRefPoints;
		if ( NREFPT > 2 ) NREFPT = 2;

		// Initialize reference point illuminance and window background luminance
		for ( IL = 1; IL <= NREFPT; ++IL ) {
			SetPnt( IL ) = ZoneDaylight( ZoneNum ).IllumSetPoint( IL );
			DaylIllum( IL ) = 0.0;
			ZoneDaylight( ZoneNum ).BacLum( IL ) = 0.0;
		}

		if ( SkyClearness > 3.0 ) { // Sky is average of clear and clear turbid
			SkyWeight = min( 1.0, ( SkyClearness - 3.0 ) / 3.0 );
			ISky1 = 1;
			ISky2 = 2;
		} else if ( SkyClearness > 1.2 ) { // Sky is average of clear turbid and intermediate
			SkyWeight = ( SkyClearness - 1.2 ) / 1.8;
			ISky1 = 2;
			ISky2 = 3;
		} else { // Sky is average of intermediate and overcast
			SkyWeight = min( 1.0, max( 0.0, ( SkyClearness - 1.0 ) / 0.2, ( SkyBrightness - 0.05 ) / 0.4 ) );
			ISky1 = 3;
			ISky2 = 4;
		}

		// First loop over exterior windows associated with this zone. The window may be an exterior window in
		// the zone or an exterior window in an adjacent zone that shares an interior window with the zone.
		// Find contribution of each window to the daylight illum and to the glare numerator at each reference point.
		// Use shading flags set in WindowShadingManager.
		for ( loop = 1; loop <= ZoneDaylight( ZoneNum ).NumOfDayltgExtWins; ++loop ) {
			IWin = ZoneDaylight( ZoneNum ).DayltgExtWinSurfNums( loop );

			// Added TH 6/29/2009 for thermochromic windows
			VTRatio = 1.0;
			if ( NREFPT > 0 ) {
				IConst = Surface( IWin ).Construction;
				if ( Construct( IConst ).TCFlag == 1 ) {
					// For thermochromic windows, daylight and glare factors are always calculated
					//  based on the master construction. They need to be adjusted by the VTRatio, including:
					//  ZoneDaylight()%DaylIllFacSky, DaylIllFacSun, DaylIllFacSunDisk; DaylBackFacSky,
					//  DaylBackFacSun, DaylBackFacSunDisk, DaylSourceFacSky, DaylSourceFacSun, DaylSourceFacSunDisk
					VTNow = POLYF( 1.0, Construct( IConst ).TransVisBeamCoef );
					VTMaster = POLYF( 1.0, Construct( Construct( IConst ).TCMasterConst ).TransVisBeamCoef );
					VTRatio = VTNow / VTMaster;
				}
			}

			// Loop over reference points
			for ( IL = 1; IL <= NREFPT; ++IL ) {

				// Daylight factors for current sun position
				for ( ISky = 1; ISky <= 4; ++ISky ) {

					// ===Bare window===
					DFSKHR( 1, ISky ) = VTRatio * ( WeightNow * ZoneDaylight( ZoneNum ).DaylIllFacSky( HourOfDay, 1, ISky, IL, loop ) + WeightPreviousHour * ZoneDaylight( ZoneNum ).DaylIllFacSky( PreviousHour, 1, ISky, IL, loop ) );

					if ( ISky == 1 ) DFSUHR( 1 ) = VTRatio * ( WeightNow * ( ZoneDaylight( ZoneNum ).DaylIllFacSun( HourOfDay, 1, IL, loop ) + ZoneDaylight( ZoneNum ).DaylIllFacSunDisk( HourOfDay, 1, IL, loop ) ) + WeightPreviousHour * ( ZoneDaylight( ZoneNum ).DaylIllFacSun( PreviousHour, 1, IL, loop ) + ZoneDaylight( ZoneNum ).DaylIllFacSunDisk( PreviousHour, 1, IL, loop ) ) );

					BFSKHR( 1, ISky ) = VTRatio * ( WeightNow * ZoneDaylight( ZoneNum ).DaylBackFacSky( HourOfDay, 1, ISky, IL, loop ) + WeightPreviousHour * ZoneDaylight( ZoneNum ).DaylBackFacSky( PreviousHour, 1, ISky, IL, loop ) );

					if ( ISky == 1 ) BFSUHR( 1 ) = VTRatio * ( WeightNow * ( ZoneDaylight( ZoneNum ).DaylBackFacSun( HourOfDay, 1, IL, loop ) + ZoneDaylight( ZoneNum ).DaylBackFacSunDisk( HourOfDay, 1, IL, loop ) ) + WeightPreviousHour * ( ZoneDaylight( ZoneNum ).DaylBackFacSun( PreviousHour, 1, IL, loop ) + ZoneDaylight( ZoneNum ).DaylBackFacSunDisk( PreviousHour, 1, IL, loop ) ) );

					SFSKHR( 1, ISky ) = VTRatio * ( WeightNow * ZoneDaylight( ZoneNum ).DaylSourceFacSky( HourOfDay, 1, ISky, IL, loop ) + WeightPreviousHour * ZoneDaylight( ZoneNum ).DaylSourceFacSky( PreviousHour, 1, ISky, IL, loop ) );

					if ( ISky == 1 ) SFSUHR( 1 ) = VTRatio * ( WeightNow * ( ZoneDaylight( ZoneNum ).DaylSourceFacSun( HourOfDay, 1, IL, loop ) + ZoneDaylight( ZoneNum ).DaylSourceFacSunDisk( HourOfDay, 1, IL, loop ) ) + WeightPreviousHour * ( ZoneDaylight( ZoneNum ).DaylSourceFacSun( PreviousHour, 1, IL, loop ) + ZoneDaylight( ZoneNum ).DaylSourceFacSunDisk( PreviousHour, 1, IL, loop ) ) );

					if ( SurfaceWindow( IWin ).ShadingFlag >= 1 || SurfaceWindow( IWin ).SolarDiffusing ) {

						// ===Shaded window or window with diffusing glass===
						if ( ! SurfaceWindow( IWin ).MovableSlats ) {
							// Shade, screen, blind with fixed slats, or diffusing glass
							DFSKHR( 2, ISky ) = VTRatio * ( WeightNow * ZoneDaylight( ZoneNum ).DaylIllFacSky( HourOfDay, 2, ISky, IL, loop ) + WeightPreviousHour * ZoneDaylight( ZoneNum ).DaylIllFacSky( PreviousHour, 2, ISky, IL, loop ) );

							if ( ISky == 1 ) {
								DFSUHR( 2 ) = VTRatio * ( WeightNow * ZoneDaylight( ZoneNum ).DaylIllFacSun( HourOfDay, 2, IL, loop ) + WeightPreviousHour * ZoneDaylight( ZoneNum ).DaylIllFacSun( PreviousHour, 2, IL, loop ) );

								if ( ! SurfaceWindow( IWin ).SlatsBlockBeam ) DFSUHR( 2 ) += VTRatio * ( WeightNow * ZoneDaylight( ZoneNum ).DaylIllFacSunDisk( HourOfDay, 2, IL, loop ) + WeightPreviousHour * ZoneDaylight( ZoneNum ).DaylIllFacSunDisk( PreviousHour, 2, IL, loop ) );
							}

							BFSKHR( 2, ISky ) = VTRatio * ( WeightNow * ZoneDaylight( ZoneNum ).DaylBackFacSky( HourOfDay, 2, ISky, IL, loop ) + WeightPreviousHour * ZoneDaylight( ZoneNum ).DaylBackFacSky( PreviousHour, 2, ISky, IL, loop ) );

							if ( ISky == 1 ) {
								BFSUHR( 2 ) = VTRatio * ( WeightNow * ZoneDaylight( ZoneNum ).DaylBackFacSun( HourOfDay, 2, IL, loop ) + WeightPreviousHour * ZoneDaylight( ZoneNum ).DaylBackFacSun( PreviousHour, 2, IL, loop ) );
								if ( ! SurfaceWindow( IWin ).SlatsBlockBeam ) BFSUHR( 2 ) += VTRatio * ( WeightNow * ZoneDaylight( ZoneNum ).DaylBackFacSunDisk( HourOfDay, 2, IL, loop ) + WeightPreviousHour * ZoneDaylight( ZoneNum ).DaylBackFacSunDisk( PreviousHour, 2, IL, loop ) );
							}

							SFSKHR( 2, ISky ) = VTRatio * ( WeightNow * ZoneDaylight( ZoneNum ).DaylSourceFacSky( HourOfDay, 2, ISky, IL, loop ) + WeightPreviousHour * ZoneDaylight( ZoneNum ).DaylSourceFacSky( PreviousHour, 2, ISky, IL, loop ) );

							if ( ISky == 1 ) {
								SFSUHR( 2 ) = VTRatio * ( WeightNow * ZoneDaylight( ZoneNum ).DaylSourceFacSun( HourOfDay, 2, IL, loop ) + WeightPreviousHour * ZoneDaylight( ZoneNum ).DaylSourceFacSun( PreviousHour, 2, IL, loop ) );
								if ( ! SurfaceWindow( IWin ).SlatsBlockBeam ) SFSUHR( 2 ) += VTRatio * ( WeightNow * ZoneDaylight( ZoneNum ).DaylSourceFacSunDisk( HourOfDay, 2, IL, loop ) + WeightPreviousHour * ZoneDaylight( ZoneNum ).DaylSourceFacSunDisk( PreviousHour, 2, IL, loop ) );
							}

						} else { // Blind with movable slats
							VarSlats = SurfaceWindow( IWin ).MovableSlats;
							SlatAng = SurfaceWindow( IWin ).SlatAngThisTS;

							DFSKHR( 2, ISky ) = VTRatio * ( WeightNow * InterpSlatAng( SlatAng, VarSlats, ZoneDaylight( ZoneNum ).DaylIllFacSky( HourOfDay, {2,MaxSlatAngs + 1}, ISky, IL, loop ) ) + WeightPreviousHour * InterpSlatAng( SlatAng, VarSlats, ZoneDaylight( ZoneNum ).DaylIllFacSky( PreviousHour, {2,MaxSlatAngs + 1}, ISky, IL, loop ) ) );

							if ( ISky == 1 ) {
								DFSUHR( 2 ) = VTRatio * ( WeightNow * InterpSlatAng( SlatAng, VarSlats, ZoneDaylight( ZoneNum ).DaylIllFacSun( HourOfDay, {2,MaxSlatAngs + 1}, IL, loop ) ) + WeightPreviousHour * InterpSlatAng( SlatAng, VarSlats, ZoneDaylight( ZoneNum ).DaylIllFacSun( PreviousHour, {2,MaxSlatAngs + 1}, IL, loop ) ) );

								// We add the contribution from the solar disk if slats do not block beam solar
								// TH CR 8010. DaylIllFacSunDisk needs to be interpolated!
								//IF (.NOT.SurfaceWindow(IWin)%SlatsBlockBeam) DFSUHR(2) = DFSUHR(2) + &
								//            VTRatio * (WeightNow * ZoneDaylight(ZoneNum)%DaylIllFacSunDisk(loop,IL,2,HourOfDay) + &
								//            WeightPreviousHour * ZoneDaylight(ZoneNum)%DaylIllFacSunDisk(loop,IL,2,PreviousHour))
								if ( ! SurfaceWindow( IWin ).SlatsBlockBeam ) DFSUHR( 2 ) += VTRatio * ( WeightNow * InterpSlatAng( SlatAng, VarSlats, ZoneDaylight( ZoneNum ).DaylIllFacSunDisk( HourOfDay, {2,MaxSlatAngs + 1}, IL, loop ) ) + WeightPreviousHour * InterpSlatAng( SlatAng, VarSlats, ZoneDaylight( ZoneNum ).DaylIllFacSunDisk( PreviousHour, {2,MaxSlatAngs + 1}, IL, loop ) ) );
							}

							BFSKHR( 2, ISky ) = VTRatio * ( WeightNow * InterpSlatAng( SlatAng, VarSlats, ZoneDaylight( ZoneNum ).DaylBackFacSky( HourOfDay, {2,MaxSlatAngs + 1}, ISky, IL, loop ) ) + WeightPreviousHour * InterpSlatAng( SlatAng, VarSlats, ZoneDaylight( ZoneNum ).DaylBackFacSky( PreviousHour, {2,MaxSlatAngs + 1}, ISky, IL, loop ) ) );

							if ( ISky == 1 ) {
								BFSUHR( 2 ) = VTRatio * ( WeightNow * InterpSlatAng( SlatAng, VarSlats, ZoneDaylight( ZoneNum ).DaylBackFacSun( HourOfDay, {2,MaxSlatAngs + 1}, IL, loop ) ) + WeightPreviousHour * InterpSlatAng( SlatAng, VarSlats, ZoneDaylight( ZoneNum ).DaylBackFacSun( PreviousHour, {2,MaxSlatAngs + 1}, IL, loop ) ) );

								// TH CR 8010. DaylBackFacSunDisk needs to be interpolated!
								//IF (.NOT.SurfaceWindow(IWin)%SlatsBlockBeam) THEN
								//  BFSUHR(2) = BFSUHR(2) + &
								//            VTRatio * (WeightNow * ZoneDaylight(ZoneNum)%DaylBackFacSunDisk(loop,IL,2,HourOfDay) + &
								//            WeightPreviousHour * ZoneDaylight(ZoneNum)%DaylBackFacSunDisk(loop,IL,2,PreviousHour))
								if ( ! SurfaceWindow( IWin ).SlatsBlockBeam ) {
									BFSUHR( 2 ) += VTRatio * ( WeightNow * InterpSlatAng( SlatAng, VarSlats, ZoneDaylight( ZoneNum ).DaylBackFacSunDisk( HourOfDay, {2,MaxSlatAngs + 1}, IL, loop ) ) + WeightPreviousHour * InterpSlatAng( SlatAng, VarSlats, ZoneDaylight( ZoneNum ).DaylBackFacSunDisk( PreviousHour, {2,MaxSlatAngs + 1}, IL, loop ) ) );
								}
							}

							SFSKHR( 2, ISky ) = VTRatio * ( WeightNow * InterpSlatAng( SlatAng, VarSlats, ZoneDaylight( ZoneNum ).DaylSourceFacSky( HourOfDay, {2,MaxSlatAngs + 1}, ISky, IL, loop ) ) + WeightPreviousHour * InterpSlatAng( SlatAng, VarSlats, ZoneDaylight( ZoneNum ).DaylSourceFacSky( PreviousHour, {2,MaxSlatAngs + 1}, ISky, IL, loop ) ) );

							if ( ISky == 1 ) {
								SFSUHR( 2 ) = VTRatio * ( WeightNow * InterpSlatAng( SlatAng, VarSlats, ZoneDaylight( ZoneNum ).DaylSourceFacSun( HourOfDay, {2,MaxSlatAngs + 1}, IL, loop ) ) + WeightPreviousHour * InterpSlatAng( SlatAng, VarSlats, ZoneDaylight( ZoneNum ).DaylSourceFacSun( PreviousHour, {2,MaxSlatAngs + 1}, IL, loop ) ) );

								// TH CR 8010. DaylSourceFacSunDisk needs to be interpolated!
								//IF (.NOT.SurfaceWindow(IWin)%SlatsBlockBeam) THEN
								//  SFSUHR(2) = SFSUHR(2) + &
								//           VTRatio * (WeightNow * ZoneDaylight(ZoneNum)%DaylSourceFacSunDisk(loop,IL,2,HourOfDay) + &
								//           WeightPreviousHour * ZoneDaylight(ZoneNum)%DaylSourceFacSunDisk(loop,IL,2,PreviousHour))
								if ( ! SurfaceWindow( IWin ).SlatsBlockBeam ) {
									SFSUHR( 2 ) += VTRatio * ( WeightNow * InterpSlatAng( SlatAng, VarSlats, ZoneDaylight( ZoneNum ).DaylSourceFacSunDisk( HourOfDay, {2,MaxSlatAngs + 1}, IL, loop ) ) + WeightPreviousHour * InterpSlatAng( SlatAng, VarSlats, ZoneDaylight( ZoneNum ).DaylSourceFacSunDisk( PreviousHour, {2,MaxSlatAngs + 1}, IL, loop ) ) );
								}
							}

						} // End of check if window has blind with movable slats
					} // End of check if window is shaded or has diffusing glass
				} // End of sky type loop, ISky

				// Get illuminance at ref point from bare and shaded window by
				// multiplying daylight factors by exterior horizontal illuminance

				// Adding 0.001 in the following prevents zero HorIllSky in early morning or late evening when sun
				// is up in the present time step but GILSK(ISky,HourOfDay) and GILSK(ISky,NextHour) are both zero.
				for ( ISky = 1; ISky <= 4; ++ISky ) {
					// HorIllSky(ISky) = WeightNow * GILSK(ISky,HourOfDay) + WeightNextHour * GILSK(ISky,NextHour) + 0.001
					HorIllSky( ISky ) = WeightNow * GILSK( HourOfDay, ISky ) + WeightPreviousHour * GILSK( PreviousHour, ISky ) + 0.001;
				}

				// HISKF is current time step horizontal illuminance from sky, calculated in DayltgLuminousEfficacy,
				// which is called in WeatherManager. HISUNF is current time step horizontal illuminance from sun,
				// also calculated in DayltgLuminousEfficacy.

				HorIllSkyFac = HISKF / ( ( 1 - SkyWeight ) * HorIllSky( ISky2 ) + SkyWeight * HorIllSky( ISky1 ) );

				for ( IS = 1; IS <= 2; ++IS ) {
					if ( IS == 2 && SurfaceWindow( IWin ).ShadingFlag <= 0 && ! SurfaceWindow( IWin ).SolarDiffusing ) break;

					ZoneDaylight( ZoneNum ).IllumFromWinAtRefPt( loop, IS, IL ) = DFSUHR( IS ) * HISUNF + HorIllSkyFac * ( DFSKHR( IS, ISky1 ) * SkyWeight * HorIllSky( ISky1 ) + DFSKHR( IS, ISky2 ) * ( 1.0 - SkyWeight ) * HorIllSky( ISky2 ) );
					ZoneDaylight( ZoneNum ).BackLumFromWinAtRefPt( loop, IS, IL ) = BFSUHR( IS ) * HISUNF + HorIllSkyFac * ( BFSKHR( IS, ISky1 ) * SkyWeight * HorIllSky( ISky1 ) + BFSKHR( IS, ISky2 ) * ( 1.0 - SkyWeight ) * HorIllSky( ISky2 ) );

					ZoneDaylight( ZoneNum ).SourceLumFromWinAtRefPt( loop, IS, IL ) = SFSUHR( IS ) * HISUNF + HorIllSkyFac * ( SFSKHR( IS, ISky1 ) * SkyWeight * HorIllSky( ISky1 ) + SFSKHR( IS, ISky2 ) * ( 1.0 - SkyWeight ) * HorIllSky( ISky2 ) );

					ZoneDaylight( ZoneNum ).SourceLumFromWinAtRefPt( loop, IS, IL ) = max( ZoneDaylight( ZoneNum ).SourceLumFromWinAtRefPt( loop, IS, IL ), 0.0 );

					// Added TH 1/21/2010 - save the original clear and dark (fully switched) states'
					//  zone daylighting values, needed for switachable glazings
					tmpIllumFromWinAtRefPt( loop, IS, IL ) = ZoneDaylight( ZoneNum ).IllumFromWinAtRefPt( loop, IS, IL );
					tmpBackLumFromWinAtRefPt( loop, IS, IL ) = ZoneDaylight( ZoneNum ).BackLumFromWinAtRefPt( loop, IS, IL );
					tmpSourceLumFromWinAtRefPt( loop, IS, IL ) = ZoneDaylight( ZoneNum ).SourceLumFromWinAtRefPt( loop, IS, IL );
				} // IS

			} // End of reference point loop, IL
		} // End of first loop over exterior windows associated with this zone

		// Initialize flag that one or more windows has switchable glazing
		// control that adjusts visible transmittance to just meet dayltg setpoint
		// (and the window has not already been switched)
		ISWFLG = 0;

		// Second loop over windows. Find total daylight illuminance and background luminance
		// for each ref pt from all exterior windows associated with the zone.  Use shading flags.
		// This illuminance excludes contribution of inter-reflected illuminance produced by solar
		// entering the zone through interior windows (which is calculated in DayltgInterReflIllFrIntWins.

		for ( loop = 1; loop <= ZoneDaylight( ZoneNum ).NumOfDayltgExtWins; ++loop ) {
			IWin = ZoneDaylight( ZoneNum ).DayltgExtWinSurfNums( loop );
			ICtrl = Surface( IWin ).WindowShadingControlPtr;
			if ( ICtrl > 0 && ISWFLG == 0 ) {
				if ( WindowShadingControl( ICtrl ).ShadingControlType == WSCT_MeetDaylIlumSetp && SurfaceWindow( IWin ).ShadingFlag == GlassConditionallyLightened ) ISWFLG = 1;
			}

			// Determine if illuminance contribution is from bare or shaded window
			//  For switchable glazings with shading control type of WSCT_MeetDaylIlumSetp,
			//   the shading flag is initialized at GlassConditionallyLightened (20), and
			//   the window is initialized at clear state: IS = 1
			//  For other windows with glare control, the shading flag is initialized at >10, to be determined
			IS = 1;
			if ( ( SurfaceWindow( IWin ).ShadingFlag >= 1 && SurfaceWindow( IWin ).ShadingFlag <= 9 ) || SurfaceWindow( IWin ).SolarDiffusing ) IS = 2;

			for ( IL = 1; IL <= NREFPT; ++IL ) {
				DaylIllum( IL ) += ZoneDaylight( ZoneNum ).IllumFromWinAtRefPt( loop, IS, IL );
				ZoneDaylight( ZoneNum ).BacLum( IL ) += ZoneDaylight( ZoneNum ).BackLumFromWinAtRefPt( loop, IS, IL );
			}
		} // End of second window loop over exterior windows associated with this zone

		// Optical switching control (e.g. electrochromic glass) to adjust
		// window's vis trans downward so daylight level equals or is as
		// close as possible to the illuminance setpoint at first reference point.
		// Assumes vis trans in the fully switched state is less than that in the
		// unswitched state. Assumes some windows in a space may have this control and
		// others not.

		// If daylight illuminance is above setpoint, allow switching
		if ( ISWFLG != 0 && DaylIllum( 1 ) > SetPnt( 1 ) ) {

			// Third loop over windows.  Get illuminance at ref pt 1 from
			// windows that can be switched (DILLSW) and those that can't (DILLUN).
			// Windows that can be switched are initially in the unswitched state.
			DILLSW = 0.0;
			DILLUN = 0.0;
			for ( loop = 1; loop <= ZoneDaylight( ZoneNum ).NumOfDayltgExtWins; ++loop ) {
				IWin = ZoneDaylight( ZoneNum ).DayltgExtWinSurfNums( loop );
				ICtrl = Surface( IWin ).WindowShadingControlPtr;
				IS = 1;
				if ( ( SurfaceWindow( IWin ).ShadingFlag >= 1 && SurfaceWindow( IWin ).ShadingFlag <= 9 ) || SurfaceWindow( IWin ).SolarDiffusing ) IS = 2;
				if ( ICtrl > 0 ) {
					if ( SurfaceWindow( IWin ).ShadingFlag == GlassConditionallyLightened && WindowShadingControl( ICtrl ).ShadingControlType == WSCT_MeetDaylIlumSetp ) {
						DILLSW += ZoneDaylight( ZoneNum ).IllumFromWinAtRefPt( loop, IS, 1 );
					} else {
						DILLUN += ZoneDaylight( ZoneNum ).IllumFromWinAtRefPt( loop, IS, 1 );
					}
				}
			} // End of third window loop, IWin

			// Transmittance multiplier
			ASETIL = ( SetPnt( 1 ) - DILLUN ) / ( DILLSW + 0.00001 );

			// ASETIL < 1 means there's enough light, so check for switching
			if ( ASETIL < 1.0 ) {

				// Fourth loop over windows to determine which to switch
				for ( loop = 1; loop <= ZoneDaylight( ZoneNum ).NumOfDayltgExtWins; ++loop ) {
					IWin = ZoneDaylight( ZoneNum ).DayltgExtWinSurfNums( loop );

					ICtrl = Surface( IWin ).WindowShadingControlPtr;
					if ( ICtrl == 0 ) continue;

					if ( SurfaceWindow( IWin ).ShadingFlag != GlassConditionallyLightened || WindowShadingControl( ICtrl ).ShadingControlType != WSCT_MeetDaylIlumSetp ) continue;

					IConst = Surface( IWin ).Construction;
					if ( SurfaceWindow( IWin ).StormWinFlag == 1 ) IConst = Surface( IWin ).StormWinConstruction;
					// Vis trans at normal incidence of unswitched glass
					TVIS1 = POLYF( 1.0, Construct( IConst ).TransVisBeamCoef ) * SurfaceWindow( IWin ).GlazedFrac;

					// Vis trans at normal incidence of fully switched glass
					IConstShaded = Surface( IWin ).ShadedConstruction;
					TVIS2 = POLYF( 1.0, Construct( IConstShaded ).TransVisBeamCoef ) * SurfaceWindow( IWin ).GlazedFrac;

					// Reset shading flag to indicate that window is shaded by being partially or fully switched
					SurfaceWindow( IWin ).ShadingFlag = SwitchableGlazing;

					// ASETIL < 0 means illuminance from non-daylight-switchable windows exceeds setpoint,
					// so completely switch all daylight-switchable windows to minimize solar gain
					if ( ASETIL <= 0.0 ) {
						SurfaceWindow( IWin ).SwitchingFactor = 1.0;
						SurfaceWindow( IWin ).VisTransSelected = TVIS2;
					} else {
						// Case where 0 < ASETIL < 1: darken glass in all
						// daylight-switchable windows to just meet illuminance setpoint
						// From this equation: SETPNT(1) = DILLUN + DILLSW/TVIS1 * VisTransSelected
						SurfaceWindow( IWin ).VisTransSelected = max( TVIS2, ASETIL * TVIS1 ) + 0.000001;
						SurfaceWindow( IWin ).SwitchingFactor = ( TVIS1 - SurfaceWindow( IWin ).VisTransSelected ) / ( TVIS1 - TVIS2 + 0.000001 );
						// bound switching factor between 0 and 1
						SurfaceWindow( IWin ).SwitchingFactor = min( 1.0, SurfaceWindow( IWin ).SwitchingFactor );
						SurfaceWindow( IWin ).SwitchingFactor = max( 0.0, SurfaceWindow( IWin ).SwitchingFactor );
					}

					// Adjust daylight quantities based on ratio between switched and unswitched visible transmittance
					for ( IL = 1; IL <= NREFPT; ++IL ) {
						// DaylIllum(IL) and BacLum(IL) were calculated at the clear state: IS = 1,
						//  and need to adjusted for intermediate switched state at VisTransSelected: IS = 2
						IS = 1;
						VTRAT = SurfaceWindow( IWin ).VisTransSelected / ( TVIS1 + 0.000001 );
						DaylIllum( IL ) += ( VTRAT - 1.0 ) * ZoneDaylight( ZoneNum ).IllumFromWinAtRefPt( loop, IS, IL );
						ZoneDaylight( ZoneNum ).BacLum( IL ) += ( VTRAT - 1.0 ) * ZoneDaylight( ZoneNum ).BackLumFromWinAtRefPt( loop, IS, IL );

						// Adjust illum, background illum and source luminance for this window in intermediate switched state
						//  for later use in the DayltgGlare calc because SurfaceWindow(IWin)%ShadingFlag = SwitchableGlazing = 2
						IS = 2;
						VTRAT = SurfaceWindow( IWin ).VisTransSelected / ( TVIS2 + 0.000001 );
						ZoneDaylight( ZoneNum ).IllumFromWinAtRefPt( loop, IS, IL ) = VTRAT * tmpIllumFromWinAtRefPt( loop, IS, IL );
						ZoneDaylight( ZoneNum ).BackLumFromWinAtRefPt( loop, IS, IL ) = VTRAT * tmpBackLumFromWinAtRefPt( loop, IS, IL );
						ZoneDaylight( ZoneNum ).SourceLumFromWinAtRefPt( loop, IS, IL ) = VTRAT * tmpSourceLumFromWinAtRefPt( loop, IS, IL );
					} // IL

					// If new daylight does not exceed the illuminance setpoint, done, no more checking other switchable glazings
					//  even though this should not happen because all switchable glazings suppose to be dimmed by a same ratio ASETIL
					//   In real world, this can be improved by setting priority of each switchable glazing to switch - NFP.
					if ( DaylIllum( 1 ) <= SetPnt( 1 ) ) {
						break;
					}
				} // End of fourth window loop, IWin -- end of switching to control daylight illuminance

			} // ASETIL < 1
		} // ISWFLG /= 0 .AND. DaylIllum(1) > SETPNT(1)

		// Calculate glare index at each reference point assuming the daylight illuminance setpoint is
		//  met at both reference points, either by daylight or electric lights
		for ( IL = 1; IL <= NREFPT; ++IL ) {
			BACL = max( SetPnt( IL ) * ZoneDaylight( ZoneNum ).AveVisDiffReflect / Pi, ZoneDaylight( ZoneNum ).BacLum( IL ) );
			// DayltgGlare uses ZoneDaylight(ZoneNum)%SourceLumFromWinAtRefPt(IL,1,loop) for unshaded windows, and
			//  ZoneDaylight(ZoneNum)%SourceLumFromWinAtRefPt(IL,2,loop) for shaded windows
			DayltgGlare( IL, BACL, GLRNDX( IL ), ZoneNum );
		}

		// Check if glare level is less than maximum allowed at each ref pt.  If maximum
		// is exceeded at either ref pt, attempt to reduce glare to acceptable level by closing
		// shading device on windows that have shades that have not already been closed.
		GlareFlag = false;
		for ( IL = 1; IL <= NREFPT; ++IL ) {
			if ( GLRNDX( IL ) > ZoneDaylight( ZoneNum ).MaxGlareallowed ) {
				GlareFlag = true;
				break;
			}
		}

		if ( GlareFlag ) {
			// Glare is too high at a ref pt.  Loop through windows.
			for ( loop = 1; loop <= ZoneDaylight( ZoneNum ).NumOfDayltgExtWins; ++loop ) {
				IWin = ZoneDaylight( ZoneNum ).DayltgExtWinSurfNums( loop );

				// Check if window is eligible for glare control
				// TH 1/21/2010. Switchable glazings already in partially switched state
				//  should be allowed to further dim to control glare
				//IF (SurfaceWindow(IWin)%ShadingFlag < 10) CYCLE
				if ( SurfaceWindow( IWin ).ShadingFlag < 10 && SurfaceWindow( IWin ).ShadingFlag != SwitchableGlazing ) continue;

				ICtrl = Surface( IWin ).WindowShadingControlPtr;
				if ( ICtrl == 0 ) continue;
				if ( WindowShadingControl( ICtrl ).GlareControlIsActive ) {

					// Illuminance (WDAYIL) and background luminance (WBACLU) contribution from this
					// window without shading (IS=1) and with shading (IS=2) for each ref pt
					//  For switchable windows, this may be partially switched rather than fully dark
					for ( IL = 1; IL <= NREFPT; ++IL ) {
						for ( IS = 1; IS <= 2; ++IS ) {
							WDAYIL( IS, IL ) = ZoneDaylight( ZoneNum ).IllumFromWinAtRefPt( loop, IS, IL );
							WBACLU( IS, IL ) = ZoneDaylight( ZoneNum ).BackLumFromWinAtRefPt( loop, IS, IL );
						}
					}

					// Recalculate illuminance and glare with shading on this window.
					//  For switchable glazings, this is the fully switched (dark) state
					for ( IL = 1; IL <= NREFPT; ++IL ) {
						if ( SurfaceWindow( IWin ).ShadingFlag != SwitchableGlazing ) {
							// for non switchable glazings or switchable glazings not switched yet (still in clear state)
							//  SurfaceWindow(IWin)%ShadingFlag = GlassConditionallyLightened
							RDAYIL( IL ) = DaylIllum( IL ) - WDAYIL( 1, IL ) + WDAYIL( 2, IL );
							RBACLU( IL ) = ZoneDaylight( ZoneNum ).BacLum( IL ) - WBACLU( 1, IL ) + WBACLU( 2, IL );
						} else {
							// switchable glazings already in partially switched state when calc the RDAYIL(IL) & RBACLU(IL)
							RDAYIL( IL ) = DaylIllum( IL ) - WDAYIL( 2, IL ) + tmpIllumFromWinAtRefPt( loop, 2, IL );
							RBACLU( IL ) = ZoneDaylight( ZoneNum ).BacLum( IL ) - WBACLU( 2, IL ) + tmpBackLumFromWinAtRefPt( loop, 2, IL );
						}
					}

					if ( SurfaceWindow( IWin ).ShadingFlag != SwitchableGlazing ) SurfaceWindow( IWin ).ShadingFlag /= 10;

					//For switchable glazings, it is switched to fully dark state,
					// update ZoneDaylight(ZoneNum)%SourceLumFromWinAtRefPt(IL,2,loop) for use in DayltgGlare
					if ( SurfaceWindow( IWin ).ShadingFlag == SwitchableGlazing ) {
						for ( IL = 1; IL <= NREFPT; ++IL ) {
							ZoneDaylight( ZoneNum ).SourceLumFromWinAtRefPt( loop, 2, IL ) = tmpSourceLumFromWinAtRefPt( loop, 2, IL );
							ZoneDaylight( ZoneNum ).IllumFromWinAtRefPt( loop, 2, IL ) = tmpIllumFromWinAtRefPt( loop, 2, IL );
							ZoneDaylight( ZoneNum ).BackLumFromWinAtRefPt( loop, 2, IL ) = tmpBackLumFromWinAtRefPt( loop, 2, IL );
						}

						IConst = Surface( IWin ).Construction;
						// Vis trans at normal incidence of unswitched glass
						TVIS1 = POLYF( 1.0, Construct( IConst ).TransVisBeamCoef ) * SurfaceWindow( IWin ).GlazedFrac;

						// Vis trans at normal incidence of fully switched glass
						IConstShaded = Surface( IWin ).ShadedConstruction;
						TVIS2 = POLYF( 1.0, Construct( IConstShaded ).TransVisBeamCoef ) * SurfaceWindow( IWin ).GlazedFrac;
					}

					// Re-calc daylight and glare at shaded state. For switchable glazings, it is the fully dark state.
					for ( IL = 1; IL <= NREFPT; ++IL ) {
						BACL = max( SetPnt( IL ) * ZoneDaylight( ZoneNum ).AveVisDiffReflect / Pi, RBACLU( IL ) );
						// DayltgGlare uses ZoneDaylight(ZoneNum)%SourceLumFromWinAtRefPt(IL,2,loop) for shaded state
						DayltgGlare( IL, BACL, GLRNEW( IL ), ZoneNum );
					}

					blnCycle = false;
					if ( NREFPT == 1 && GLRNEW( 1 ) > GLRNDX( 1 ) ) {
						// One ref pt;  go to next window if glare has increased.
						blnCycle = true;
					} else if ( NREFPT > 1 ) {
						// Two ref pts.  There are three cases depending on glare values.
						if ( GLRNDX( 1 ) > ZoneDaylight( ZoneNum ).MaxGlareallowed && GLRNDX( 2 ) > ZoneDaylight( ZoneNum ).MaxGlareallowed ) {
							// (1) Initial glare too high at both ref pts.  Deploy shading on
							//     this window if this decreases glare at both ref pts.
							if ( GLRNEW( 1 ) > GLRNDX( 1 ) || GLRNEW( 2 ) > GLRNDX( 2 ) ) blnCycle = true;
						} else if ( GLRNDX( 1 ) > ZoneDaylight( ZoneNum ).MaxGlareallowed && GLRNDX( 2 ) <= ZoneDaylight( ZoneNum ).MaxGlareallowed ) {
							// (2) Initial glare too high only at first ref pt.  Deploy shading
							//     on this window if glare at first ref pt decreases and
							//     glare at second ref pt stays below max.
							if ( GLRNEW( 1 ) > GLRNDX( 1 ) || GLRNEW( 2 ) > ZoneDaylight( ZoneNum ).MaxGlareallowed ) blnCycle = true;
						} else {
							// (3) Initial glare too high at second ref pt.  Deploy shading if glare
							//     at second ref pt decreases and glare at first ref pt stays below max.
							if ( GLRNEW( 2 ) > GLRNDX( 2 ) || GLRNEW( 1 ) > ZoneDaylight( ZoneNum ).MaxGlareallowed ) blnCycle = true;
						}
					}

					// Shading this window has not improved the glare situation.
					// Reset shading flag to no shading condition, go to next window.
					if ( blnCycle ) {
						//  for switchable glazings, reset properties to clear state or partial switched state?
						if ( SurfaceWindow( IWin ).ShadingFlag == SwitchableGlazing ) {
							SurfaceWindow( IWin ).SwitchingFactor = 0.0;
							SurfaceWindow( IWin ).VisTransSelected = TVIS1;

							// RESET properties for fully dark state
							for ( IL = 1; IL <= NREFPT; ++IL ) {
								ZoneDaylight( ZoneNum ).IllumFromWinAtRefPt( loop, 2, IL ) = tmpIllumFromWinAtRefPt( loop, 2, IL );
								ZoneDaylight( ZoneNum ).BackLumFromWinAtRefPt( loop, 2, IL ) = tmpBackLumFromWinAtRefPt( loop, 2, IL );
								ZoneDaylight( ZoneNum ).SourceLumFromWinAtRefPt( loop, 2, IL ) = tmpSourceLumFromWinAtRefPt( loop, 2, IL );
							}
						}

						SurfaceWindow( IWin ).ShadingFlag = ShadeOff;
						continue;
					}

					// Shading this window has improved the glare situation.
					// Reset background luminance, glare index, and daylight illuminance at each ref pt.
					// For switchable glazings, this is fully switched, dark state
					for ( IL = 1; IL <= NREFPT; ++IL ) {
						ZoneDaylight( ZoneNum ).BacLum( IL ) = RBACLU( IL );
						GLRNDX( IL ) = GLRNEW( IL );
						DaylIllum( IL ) = RDAYIL( IL );
					}

					// TH comments (5/22/2009): seems for EC windows, if the calculated glare exceeds the max setpoint,
					//  the EC windows will be reset to fully dark state which significantly reduces the available daylight.
					//  A better way is to dim the EC windows as necessary just to meet the glare index, which will still
					//  provide more daylight while not exceeding the max glare! The question is then how to set the
					//  SwitchingFactor to just meet the glare index.
					//  This was addressed in CR 7984 for E+ 5.0. 1/19/2010

					// If switchable glazing, set switching factor to 1: fully switched.
					if ( SurfaceWindow( IWin ).ShadingFlag == SwitchableGlazing ) {
//						tmpSWFactor0 = SurfaceWindow( IWin ).SwitchingFactor; // save original switching factor //Unused Set but never used
						SurfaceWindow( IWin ).SwitchingFactor = 1.0;
						SurfaceWindow( IWin ).VisTransSelected = TVIS2;

						// restore fully dark values
						for ( IL = 1; IL <= NREFPT; ++IL ) {
							WDAYIL( 2, IL ) = tmpIllumFromWinAtRefPt( loop, 2, IL );
							WBACLU( 2, IL ) = tmpBackLumFromWinAtRefPt( loop, 2, IL );
							ZoneDaylight( ZoneNum ).IllumFromWinAtRefPt( loop, 2, IL ) = tmpIllumFromWinAtRefPt( loop, 2, IL );
							ZoneDaylight( ZoneNum ).BackLumFromWinAtRefPt( loop, 2, IL ) = tmpBackLumFromWinAtRefPt( loop, 2, IL );
							ZoneDaylight( ZoneNum ).SourceLumFromWinAtRefPt( loop, 2, IL ) = tmpSourceLumFromWinAtRefPt( loop, 2, IL );
						}
					}

					// Check if glare now acceptable at each ref pt.
					GlareOK = false;
					if ( NREFPT == 1 ) {
						if ( GLRNDX( 1 ) <= ZoneDaylight( ZoneNum ).MaxGlareallowed ) GlareOK = true;
					} else if ( NREFPT > 1 ) {
						if ( GLRNDX( 1 ) <= ZoneDaylight( ZoneNum ).MaxGlareallowed && GLRNDX( 2 ) <= ZoneDaylight( ZoneNum ).MaxGlareallowed ) GlareOK = true;
					}

					if ( GlareOK ) {
						if ( SurfaceWindow( IWin ).ShadingFlag == SwitchableGlazing && WindowShadingControl( ICtrl ).ShadingControlType == WSCT_MeetDaylIlumSetp ) {
							// Added TH 1/14/2010
							// Only for switchable glazings with MeetDaylightIlluminanceSetpoint control
							// The glazing is in fully dark state, it might lighten a bit to provide more daylight
							//  while meeting maximum discomfort glare index
							// Iteration to find the right switching factor meeting the glare index

							// get fully dark state values
							tmpSWSL1 = tmpSourceLumFromWinAtRefPt( loop, 2, 1 );
							if ( NREFPT > 1 ) tmpSWSL2 = tmpSourceLumFromWinAtRefPt( loop, 2, 2 );

							// use simple fixed step search in iteraction, can be improved in future
							tmpSWFactor = 1.0 - tmpSWIterStep;
							while ( tmpSWFactor > 0 ) {
								// calc new glare at new switching state
								for ( IL = 1; IL <= NREFPT; ++IL ) {
									RDAYIL( IL ) = DaylIllum( IL ) + ( WDAYIL( 1, IL ) - WDAYIL( 2, IL ) ) * ( 1.0 - tmpSWFactor );
									RBACLU( IL ) = ZoneDaylight( ZoneNum ).BacLum( IL ) + ( WBACLU( 1, IL ) - WBACLU( 2, IL ) ) * ( 1.0 - tmpSWFactor );
									BACL = max( SetPnt( IL ) * ZoneDaylight( ZoneNum ).AveVisDiffReflect / Pi, RBACLU( IL ) );
									// needs to update SourceLumFromWinAtRefPt(IL,2,loop) before re-calc DayltgGlare
									tmpMult = ( TVIS1 - ( TVIS1 - TVIS2 ) * tmpSWFactor ) / TVIS2;
									if ( IL == 1 ) {
										ZoneDaylight( ZoneNum ).SourceLumFromWinAtRefPt( loop, 2, IL ) = tmpSWSL1 * tmpMult;
									} else {
										ZoneDaylight( ZoneNum ).SourceLumFromWinAtRefPt( loop, 2, IL ) = tmpSWSL2 * tmpMult;
									}
									// Calc new glare
									DayltgGlare( IL, BACL, GLRNEW( IL ), ZoneNum );
								}

								// Check whether new glare is OK
								GlareOK = false;
								if ( NREFPT == 1 ) {
									if ( GLRNEW( 1 ) <= ZoneDaylight( ZoneNum ).MaxGlareallowed ) GlareOK = true;
								} else if ( NREFPT > 1 ) {
									if ( GLRNEW( 1 ) <= ZoneDaylight( ZoneNum ).MaxGlareallowed && GLRNEW( 2 ) <= ZoneDaylight( ZoneNum ).MaxGlareallowed ) GlareOK = true;
								}

								if ( GlareOK ) {
									if ( tmpSWFactor >= tmpSWIterStep ) {
										// Continue to lighten the glazing
										tmpSWFactor -= tmpSWIterStep;
										continue;
									} else {
										// Glare still OK but glazing already in clear state, no more lighten
										break;
									}
								} else {
									// Glare too high, exit and use previous switching state
									tmpSWFactor += tmpSWIterStep;
									break;
								}
							}

							// Final re-calculation if needed
							if ( ! GlareOK ) {
								// Glare too high, use previous state and re-calc
								for ( IL = 1; IL <= NREFPT; ++IL ) {
									RDAYIL( IL ) = DaylIllum( IL ) + ( WDAYIL( 1, IL ) - WDAYIL( 2, IL ) ) * ( 1.0 - tmpSWFactor );
									RBACLU( IL ) = ZoneDaylight( ZoneNum ).BacLum( IL ) + ( WBACLU( 1, IL ) - WBACLU( 2, IL ) ) * ( 1.0 - tmpSWFactor );
									BACL = max( SetPnt( IL ) * ZoneDaylight( ZoneNum ).AveVisDiffReflect / Pi, RBACLU( IL ) );

									// needs to update SourceLumFromWinAtRefPt(IL,2,IWin) before re-calc DayltgGlare
									tmpMult = ( TVIS1 - ( TVIS1 - TVIS2 ) * tmpSWFactor ) / TVIS2;
									if ( IL == 1 ) {
										ZoneDaylight( ZoneNum ).SourceLumFromWinAtRefPt( loop, 2, 1 ) = tmpSWSL1 * tmpMult;
									} else {
										ZoneDaylight( ZoneNum ).SourceLumFromWinAtRefPt( loop, 2, 2 ) = tmpSWSL2 * tmpMult;
									}
									DayltgGlare( IL, BACL, GLRNEW( IL ), ZoneNum );
								}
							}

							//Update final results
							for ( IL = 1; IL <= NREFPT; ++IL ) {
								ZoneDaylight( ZoneNum ).BacLum( IL ) = RBACLU( IL );
								GLRNDX( IL ) = GLRNEW( IL );
								DaylIllum( IL ) = RDAYIL( IL );

								tmpMult = ( TVIS1 - ( TVIS1 - TVIS2 ) * tmpSWFactor ) / TVIS2;
								//update report variables
								ZoneDaylight( ZoneNum ).IllumFromWinAtRefPt( loop, 2, IL ) = tmpIllumFromWinAtRefPt( loop, 2, IL ) * tmpMult;
								ZoneDaylight( ZoneNum ).BackLumFromWinAtRefPt( loop, 2, IL ) = tmpBackLumFromWinAtRefPt( loop, 2, IL ) * tmpMult;
							}
							SurfaceWindow( IWin ).SwitchingFactor = tmpSWFactor;
							SurfaceWindow( IWin ).VisTransSelected = TVIS1 - ( TVIS1 - TVIS2 ) * tmpSWFactor;

						} else {
							//For un-switchable glazing or switchable glazing but not MeetDaylightIlluminaceSetpoint control,
							// it is in shaded state and glare is ok - job is done, exit the window loop - IWin
							break;
						}
						//   ELSE
						//     ! glare still high at either ref pt. go to next window
						//     !  clean up for switchable glazings
						//     IF (SurfaceWindow(IWin)%ShadingFlag == SwitchableGlazing) THEN
						//       ! Already in fully dark state
						//       DO IL = 1,NREFPT
						//         ZoneDaylight(ZoneNum)%SourceLumFromWinAtRefPt(IL,2,loop) = tmpSourceLumFromWinAtRefPt(IL,2,loop)
						//         ZoneDaylight(ZoneNum)%IllumFromWinAtRefPt(IL,2,loop) = tmpIllumFromWinAtRefPt(IL,2,loop)
						//         ZoneDaylight(ZoneNum)%BackLumFromWinAtRefPt(IL,2,loop) = tmpBackLumFromWinAtRefPt(IL,2,loop)
						//       END DO
						//     ENDIF
					}

				} // End of check if window glare control is active
			} // End of window loop, IWin
		} // GlareFlag

		// Loop again over windows and reset remaining shading flags that
		// are 10 or higher (i.e., conditionally off) to off
		for ( IWin = Zone( ZoneNum ).SurfaceFirst; IWin <= Zone( ZoneNum ).SurfaceLast; ++IWin ) {
			if ( Surface( IWin ).Class != SurfaceClass_Window ) continue;
			if ( Surface( IWin ).ExtBoundCond != ExternalEnvironment ) continue;
			if ( SurfaceWindow( IWin ).ShadingFlag >= 10 ) SurfaceWindow( IWin ).ShadingFlag = ShadeOff;
		}

		// Variables for reporting
		for ( IL = 1; IL <= NREFPT; ++IL ) {
			ZoneDaylight( ZoneNum ).DaylIllumAtRefPt( IL ) = DaylIllum( IL );
			ZoneDaylight( ZoneNum ).GlareIndexAtRefPt( IL ) = GLRNDX( IL );

			//added TH 12/2/2008
			if ( GLRNDX( IL ) > ZoneDaylight( ZoneNum ).MaxGlareallowed ) {
				ZoneDaylight( ZoneNum ).TimeExceedingGlareIndexSPAtRefPt( IL ) = TimeStepZone; //fraction of hours
			} else {
				ZoneDaylight( ZoneNum ).TimeExceedingGlareIndexSPAtRefPt( IL ) = 0.0;
			}

			//added TH 7/6/2009
			if ( DaylIllum( IL ) > ZoneDaylight( ZoneNum ).IllumSetPoint( IL ) ) {
				ZoneDaylight( ZoneNum ).TimeExceedingDaylightIlluminanceSPAtRefPt( IL ) = TimeStepZone; //fraction of hours
			} else {
				ZoneDaylight( ZoneNum ).TimeExceedingDaylightIlluminanceSPAtRefPt( IL ) = 0.0;
			}
		}

		// The following report variables are valid only for daylit zones without interior windows
		if ( ! Zone( ZoneNum ).HasInterZoneWindow ) {
			for ( loop = 1; loop <= ZoneDaylight( ZoneNum ).NumOfDayltgExtWins; ++loop ) {
				IWin = ZoneDaylight( ZoneNum ).DayltgExtWinSurfNums( loop );
				IS = 1;
				if ( SurfaceWindow( IWin ).ShadingFlag > 0 || SurfaceWindow( IWin ).SolarDiffusing ) IS = 2;
				SurfaceWindow( IWin ).IllumFromWinAtRefPt1Rep = ZoneDaylight( ZoneNum ).IllumFromWinAtRefPt( loop, IS, 1 );
				SurfaceWindow( IWin ).LumWinFromRefPt1Rep = ZoneDaylight( ZoneNum ).SourceLumFromWinAtRefPt( loop, IS, 1 );
				if ( ZoneDaylight( ZoneNum ).TotalDaylRefPoints > 1 ) {
					SurfaceWindow( IWin ).IllumFromWinAtRefPt2Rep = ZoneDaylight( ZoneNum ).IllumFromWinAtRefPt( loop, IS, 2 );
					SurfaceWindow( IWin ).LumWinFromRefPt2Rep = ZoneDaylight( ZoneNum ).SourceLumFromWinAtRefPt( loop, IS, 2 );
				}
			}
		}

	}

	void
	DayltgInteriorTDDIllum()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   October 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculate the TDD Pipe illuminance values

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
		int PipeNum; // TDD pipe object number
		Real64 TDDTransVisDiffNow; // TDD diffuse visible transmittance at the current hour
		Real64 TDDTransVisDiffPrev; // TDD diffuse visible transmittance at the previous hour
		static Vector4< Real64 > TDDTransVisDiff; // Weighted diffuse visible transmittance for each sky type
		int ISky; // Sky type index
		int ISky1; // Sky type index values for averaging two sky types
		int ISky2;
		Real64 SkyWeight; // Weighting factor used to average two different sky types

		if ( SkyClearness > 3.0 ) { // Sky is average of clear and clear turbid
			SkyWeight = min( 1.0, ( SkyClearness - 3.0 ) / 3.0 );
			ISky1 = 1;
			ISky2 = 2;
		} else if ( SkyClearness > 1.2 ) { // Sky is average of clear turbid and intermediate
			SkyWeight = ( SkyClearness - 1.2 ) / 1.8;
			ISky1 = 2;
			ISky2 = 3;
		} else { // Sky is average of intermediate and overcast
			SkyWeight = min( 1.0, max( 0.0, ( SkyClearness - 1.0 ) / 0.2, ( SkyBrightness - 0.05 ) / 0.4 ) );
			ISky1 = 3;
			ISky2 = 4;
		}

		// Calculate and report TDD visible transmittances
		for ( PipeNum = 1; PipeNum <= NumOfTDDPipes; ++PipeNum ) {

			TDDPipe( PipeNum ).TransVisBeam = WeightNow * TDDTransVisBeam( HourOfDay, PipeNum ) + WeightPreviousHour * TDDTransVisBeam( PreviousHour, PipeNum );

			for ( ISky = 1; ISky <= 4; ++ISky ) {
				if ( TDDFluxInc( HourOfDay, ISky, PipeNum ) > 0.0 ) {
					TDDTransVisDiffNow = TDDFluxTrans( HourOfDay, ISky, PipeNum ) / TDDFluxInc( HourOfDay, ISky, PipeNum );
				} else {
					TDDTransVisDiffNow = 0.0;
				}

				if ( TDDFluxInc( PreviousHour, ISky, PipeNum ) > 0.0 ) {
					TDDTransVisDiffPrev = TDDFluxTrans( PreviousHour, ISky, PipeNum ) / TDDFluxInc( PreviousHour, ISky, PipeNum );
				} else {
					TDDTransVisDiffPrev = 0.0;
				}

				TDDTransVisDiff( ISky ) = WeightNow * TDDTransVisDiffNow + WeightPreviousHour * TDDTransVisDiffPrev;
			} // ISky

			TDDPipe( PipeNum ).TransVisDiff = SkyWeight * TDDTransVisDiff( ISky1 ) + ( 1.0 - SkyWeight ) * TDDTransVisDiff( ISky2 );
		} // PipeNum

	}

	void
	DayltgElecLightingControl( int & ZoneNum ) // Zone number
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   July 1997
		//       MODIFIED       Mar 2004, FCW: add inter-reflected illuminance from interior windows to DaylIllum
		//                      Apr 2004, FCW: move CALL ReportIllumMap from DayltgInteriorIllum2 (DayltgInteriorMapIllum)
		//                      Apr 2010, BG NREL: remove inter-reflected illuminance to stop double counting
		//                      Aug 2012, BG NREL: added availability schedule logic
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// For a daylit space, determines lighting power reduction factor due to
		// daylighting for different lighting control systems.

		// Called by InitSurfaceHeatBalance.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// Based on DOE-2.1E subroutine DLTSYS.

		// Using/Aliasing
		using ScheduleManager::GetCurrentScheduleValue;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 TotReduction; // Electric lighting power reduction factor for a zone
		//  due to daylighting
		int NREFPT; // Number of daylighting reference points in a zone
		Real64 ZFTOT; // Fraction of zone's floor area that has daylighting controls
		int IL; // Reference point index
		int LSYSTP; // Lighting control type: 1=continuous dimming, 2=stepped,
		//  3=continuous dimming then off
		Real64 ZFRAC; // Fraction of zone controlled by a reference point
		Real64 FL; // Fraction electric lighting output required to meet setpoint
		Real64 FP; // Fraction electric lighting power input required to meet setpoint
		Real64 XRAN; // Random number between 0 and 1
		int MapNum; // Illuminance map number
		int ILM;
		bool ScheduledAvailable;

		// FLOW:

		TotReduction = 0.0;
		//  ScheduledAvailable = .TRUE.

		// check if scheduled to be available
		//  IF (ZoneDaylight(ZoneNum)%AvailSchedNum > 0) THEN
		if ( GetCurrentScheduleValue( ZoneDaylight( ZoneNum ).AvailSchedNum ) > 0.0 ) {
			ScheduledAvailable = true;
		} else {
			ScheduledAvailable = false;
		}
		//  ENDIF

		if ( ScheduledAvailable ) {
			// Limit the number of control reference points to 2
			NREFPT = ZoneDaylight( ZoneNum ).TotalDaylRefPoints;
			if ( NREFPT > 2 ) NREFPT = 2;

			// Total fraction of zone that is daylit
			ZFTOT = ZoneDaylight( ZoneNum ).FracZoneDaylit( 1 );
			if ( NREFPT > 1 ) ZFTOT += ZoneDaylight( ZoneNum ).FracZoneDaylit( 2 );

			// Loop over reference points
			for ( IL = 1; IL <= NREFPT; ++IL ) {
				DaylIllum( IL ) = ZoneDaylight( ZoneNum ).DaylIllumAtRefPt( IL );
				if ( DaylIllum( IL ) >= ZoneDaylight( ZoneNum ).IllumSetPoint( IL ) ) {
					FL = 0.0;
				} else {
					FL = ( ZoneDaylight( ZoneNum ).IllumSetPoint( IL ) - DaylIllum( IL ) ) / ZoneDaylight( ZoneNum ).IllumSetPoint( IL );
				}

				// BRANCH ON LIGHTING SYSTEM TYPE
				LSYSTP = ZoneDaylight( ZoneNum ).LightControlType;
				if ( LSYSTP != 2 ) {
					// Continuously dimmable system with linear power curve
					// Fractional output power required to meet setpoint
					FP = 1.0;
					// LIGHT-CTRL-TYPE = CONTINUOUS (LSYSTP = 1)
					if ( FL <= ZoneDaylight( ZoneNum ).MinLightFraction ) FP = ZoneDaylight( ZoneNum ).MinPowerFraction;
					// LIGHT-CTRL-TYPE = CONTINUOUS/OFF (LSYSTP = 3)
					if ( FL <= ZoneDaylight( ZoneNum ).MinLightFraction && LSYSTP == 3 ) FP = 0.0;
					if ( FL > ZoneDaylight( ZoneNum ).MinLightFraction && FL < 1.0 ) FP = ( FL + ( 1.0 - FL ) * ZoneDaylight( ZoneNum ).MinPowerFraction - ZoneDaylight( ZoneNum ).MinLightFraction ) / ( 1.0 - ZoneDaylight( ZoneNum ).MinLightFraction );

				} else { // LSYSTP = 2
					// Stepped system
					FP = 0.0;
					if ( DaylIllum( IL ) > 0.0 && DaylIllum( IL ) < ZoneDaylight( ZoneNum ).IllumSetPoint( IL ) ) FP = double( int( ZoneDaylight( ZoneNum ).LightControlSteps * FL ) + 1 ) / double( ZoneDaylight( ZoneNum ).LightControlSteps );

					if ( DaylIllum( IL ) == 0.0 ) FP = 1.0;

					if ( ZoneDaylight( ZoneNum ).LightControlProbability < 1.0 ) {
						// Manual operation.  Occupant sets lights one level too high a fraction of the time equal to
						// 1. - ZoneDaylight(ZoneNum)%LightControlProbability.  RANDOM_NUMBER returns a random number
						// between 0 and 1.
						RANDOM_NUMBER( XRAN );
						if ( XRAN >= ZoneDaylight( ZoneNum ).LightControlProbability ) {
							// Set level one higher
							if ( FP < 1.0 ) FP += ( 1.0 / double( ZoneDaylight( ZoneNum ).LightControlSteps ) );
						} // XRAN
					} // Light Control Probability < 1
				} // Lighting System Type

				ZoneDaylight( ZoneNum ).RefPtPowerReductionFactor( IL ) = FP;

				// Accumulate net ltg power reduction factor for entire zone
				ZFRAC = ZoneDaylight( ZoneNum ).FracZoneDaylit( IL );
				TotReduction += ZoneDaylight( ZoneNum ).RefPtPowerReductionFactor( IL ) * ZFRAC;

			} // End of loop over reference points, IL

			// Correct for fraction of zone (1-ZFTOT) not controlled by
			// the reference points.  For this fraction (which is usually zero),
			// the electric lighting is unaffected and the power reduction
			// factor is therefore 1.0.
			TotReduction += ( 1.0 - ZFTOT );
		} else { // controls not currently available
			TotReduction = 1.0;
		}
		ZoneDaylight( ZoneNum ).ZonePowerReductionFactor = TotReduction;

		//  IF(TotIllumMaps > 0 .and. .not. DoingSizing .and. .not. WarmupFlag .and. .not. KickoffSimulation) THEN
		if ( TotIllumMaps > 0 && ! DoingSizing && ! WarmupFlag ) {
			// If an illuminance map is associated with this zone, generate the map
			if ( TimeStep == 1 ) mapResultsToReport = false;
			for ( ILM = 1; ILM <= ZoneDaylight( ZoneNum ).MapCount; ++ILM ) {
				MapNum = ZoneDaylight( ZoneNum ).ZoneToMap( ILM );
				for ( IL = 1; IL <= IllumMapCalc( MapNum ).TotalMapRefPoints; ++IL ) {
					IllumMapCalc( MapNum ).DaylIllumAtMapPtHr( IL ) += IllumMapCalc( MapNum ).DaylIllumAtMapPt( IL ) / double( NumOfTimeStepInHour );
					if ( IllumMapCalc( MapNum ).DaylIllumAtMapPtHr( IL ) > 0.0 ) {
						mapResultsToReport = true;
						mapResultsReported = true;
					}
				}
				ReportIllumMap( MapNum );
				if ( TimeStep == NumOfTimeStepInHour ) {
					IllumMapCalc( MapNum ).DaylIllumAtMapPtHr = 0.0;
					IllumMapCalc( MapNum ).DaylIllumAtMapPt = 0.0;
				}
			}
		}

	}

	Real64
	DayltgGlarePositionFactor(
		Real64 & X, // Lateral and vertical distance of luminous window element from
		Real64 & Y
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   July 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// by table interpolation, evaluates the
		// Hopkinson position factor used in glare calculation
		// (Hopkinson, Petherbridge, AND Longmore -- Daylighting,
		// London, 1966, PP 307, 323).  X (Y) is the lateral
		// (vertical) distance of luminous window element from
		// horizontal line of vision, divided by horizontal distance
		// from eye of observer. The array PF contains values of
		// the position factor for X = 0, 0.5, 1.0, 1.5, 2.0, 2.5,
		// and 3.0 and Y = 0, 0.5, 1.0, 1.5, 2.0. Called by CalcDayltgCoefficients.

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// Based on DOE-2.1E subroutine DPFAC.

		// USE STATEMENTS:
		// na

		// Return value
		Real64 DayltgGlarePositionFactor; // Position factor

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:
		//  horizontal line of vision, divided by horizontal distance from
		//  eye of observer

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int IX; // Lateral and vertical displacement indices
		int IY;
		Real64 X1; // Lateral and vertical displacement ratios
		Real64 Y1;
		Real64 FA; // Intermediate variables
		Real64 FB;

		static Array2D< Real64 > const PF( 5, 7, reshape2< Real64, int >( { 1.00, 0.492, 0.226, 0.128, 0.081, 0.061, 0.057, 0.123, 0.119, 0.065, 0.043, 0.029, 0.026, 0.023, 0.019, 0.026, 0.019, 0.016, 0.014, 0.011, 0.011, 0.008, 0.008, 0.008, 0.008, 0.008, 0.006, 0.006, 0.0, 0.0, 0.003, 0.003, 0.003, 0.003, 0.003 }, { 5, 7 } ) ); // Position factor array // Explicit reshape2 template args are work-around for VC++2013 bug

		// FLOW:
		DayltgGlarePositionFactor = 0.0;
		if ( X < 0.0 || X >= 3.0 ) return DayltgGlarePositionFactor;
		if ( Y < 0.0 || Y >= 2.0 ) return DayltgGlarePositionFactor;

		IX = 1 + int( 2.0 * X );
		IY = 1 + int( 2.0 * Y );
		X1 = 0.5 * double( IX - 1 );
		Y1 = 0.5 * double( IY - 1 );
		FA = PF( IY, IX ) + 2.0 * ( X - X1 ) * ( PF( IY, IX + 1 ) - PF( IY, IX ) );
		FB = PF( IY + 1, IX ) + 2.0 * ( X - X1 ) * ( PF( IY + 1, IX + 1 ) - PF( IY + 1, IX ) );
		DayltgGlarePositionFactor = FA + 2.0 * ( Y - Y1 ) * ( FB - FA );

		return DayltgGlarePositionFactor;

	}

	void
	DayltgInterReflectedIllum(
		int const ISunPos, // Sun position counter; used to avoid calculating various
		int const IHR, // Hour of day
		int const ZoneNum, // Zone number
		int const IWin // Window index
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   July 1997
		//       MODIFIED       FCW December 1998
		//                      FCW June 2001: Add blind calculations
		//                      FCW Jan 2001: Add blinds with movable slats
		//                      FCW Jan 2003: Add between-glass blinds
		//                      FCW Jul 2003: account for transmittance of shading surfaces
		//                       (previously these were assumed opaque even if transmittance schedule
		//                        value was non-zero)
		//                      FCW Aug 2003: modify initialization of WLUMSK from WLUMSK = 0. TO
		//                        WLUMSK(:,:,IHR) = 0. Otherwise values calculated in previous
		//                        call are incorrectly zeroed. Result was that window luminance with
		//                        shade or blind included only contribution from first window element
		//                        in window element loop in CalcDayltgCoefficients, thus seriously
		//                        undercalculating window luminance for windows with more than one
		//                        window element. Similarly, modified initialization of WLUMSU from
		//                        WLUMSU = 0. to WLUMSU(:,IHR) = 0., and of WLUMSUdisk from
		//                        WLUMSUdisk = 0. to WLUMSUdisk(:,IHR) = 0.
		//                      PGE Aug 2003: Add daylighting shelves.
		//                      FCW Nov 2003: Add beam solar and sky solar reflected from obstructions;
		//                                    add beam solar reflected from ground accounting for obstructions.
		//                      FCW Nov 2003: increase NPHMAX from 9 to 10 to avoid rays with altitude angle = 0
		//                                    for vertical surfaces.
		//                      FCW Nov 2003: fix the expression for min and max limits of azimuth; old expression
		//                                    broke down for window normals with negative altitude angle
		//                      FCW Nov 2003: add specular reflection from exterior obstructions
		//                      FCW Apr 2004: add light well efficiency multiplying window transmittance
		//                      FCW Apr 2004: add diffusing glazing
		//                      RAR (FSEC)  May 2006: add exterior window screen
		//                      B. Griffith NREL April 2010: CR7869 add adjacent zone area if window is not on this zone
		//                                    apply interior window transmission and blocking to beam transmission from ext win
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Called from CalcDayltgCoefficients for each window and reference point in a daylit
		// space, for each sun position. Calculates illuminance (EINTSK and EINTSU) at reference point due
		// to internally reflected light by integrating to determine the amount of flux from
		// sky and ground (and beam reflected from obstructions) transmitted through
		// the center of the window and then reflecting this
		// light from the inside surfaces of the space.  The "split-flux" method is used
		// (Lynes, Principles of Natural Lighting, 1968).  EINT is determined for
		// different sky types and for window with and without shades, screens or blinds.
		// Also finds luminance (WLUMSK and WLUMSU) of window with shade or blind, &
		// or with diffusing glass, for different sky types.

		// METHODOLOGY EMPLOYED:na

		// REFERENCES:
		// Based on DOE-2.1E subroutine DREFLT.

		// Using/Aliasing
		using General::InterpProfAng;
		using General::POLYF;
		using General::BlindBeamBeamTrans;
		using DaylightingDevices::FindTDDPipe;
		using DaylightingDevices::TransTDD;
		using DataSystemVariables::DetailedSkyDiffuseAlgorithm;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		//  quantities that do not depend on sun position.

		// SUBROUTINE PARAMETER DEFINITIONS:
		int const NPHMAX( 10 ); // Number of sky/ground integration steps in altitude
		int const NTHMAX( 16 ); // Number of sky/ground integration steps in azimuth

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// In the following I,J arrays:
		// I = sky type;
		// J = 1 for bare window, 2 and above for window with shade or blind.
		static Array2D< Real64 > FLFWSK( MaxSlatAngs+1, 4 ); // Sky-related downgoing luminous flux
		static Array1D< Real64 > FLFWSU( MaxSlatAngs+1 ); // Sun-related downgoing luminous flux, excluding entering beam
		static Array1D< Real64 > FLFWSUdisk( MaxSlatAngs+1 ); // Sun-related downgoing luminous flux, due to entering beam
		static Array2D< Real64 > FLCWSK( MaxSlatAngs+1, 4 ); // Sky-related upgoing luminous flux
		static Array1D< Real64 > FLCWSU( MaxSlatAngs+1 ); // Sun-related upgoing luminous flux

		int ISky; // Sky type index: 1=clear, 2=clear turbid,
		//  3=intermediate, 4=overcast
		static Array1D< Real64 > TransMult( MaxSlatAngs ); // Transmittance multiplier
		static Array1D< Real64 > TransBmBmMult( MaxSlatAngs ); // Isolated blind beam-beam transmittance
		Real64 DPH; // Sky/ground element altitude and azimuth increments (radians)
		Real64 DTH;
		int IPH; // Sky/ground element altitude and azimuth indices
		int ITH;
		Real64 PH; // Sky/ground element altitude and azimuth (radians)
		Real64 TH;
		Real64 SPH; // Sine and cosine of PH
		Real64 CPH;
		Real64 PHMIN; // Limits of altitude integration (radians)
		Real64 PHMAX;
		Real64 ThMin; // Limits of azimuth integration (radians)
		Real64 ThMax;
		Real64 PhWin; // Altitude, azimuth angle of window normal (radians)
		Real64 ThWin;
		Real64 ACosTanTan; // ACOS(-TAN(Ph)*TAN(PhWin))
		int IConst; // Construction pointer
		Real64 DA; // CPH*DTH*DPH
		Real64 COSB; // Cosine of angle of incidence of light from sky or ground
		Real64 TVISBR; // Transmittance of window without shading at COSB
		//  (times light well efficiency, if appropriate)
		static Vector4< Real64 > ZSK; // Sky-related and sun-related illuminance on window from sky/ground
		Real64 ZSU;
		//  element for clear and overcast sky
		static Vector3< Real64 > U; // Unit vector in (PH,TH) direction
		Real64 ObTrans; // Product of solar transmittances of obstructions seen by a light ray
		static Array2D< Real64 > ObTransM( NPHMAX, NTHMAX ); // ObTrans value for each (TH,PH) direction
		//unused  REAL(r64)         :: HitPointLumFrClearSky     ! Luminance of obstruction from clear sky (cd/m2)
		//unused  REAL(r64)         :: HitPointLumFrOvercSky     ! Luminance of obstruction from overcast sky (cd/m2)
		//unused  REAL(r64)         :: HitPointLumFrSun          ! Luminance of obstruction from sun (cd/m2)
		int ICtrl; // Window control pointer
		int IConstShaded; // Pointer to shaded construction for a window
		int JSH; // Shading index: JSH=1 is bare window, JSH=2 is shaded window
		Real64 COSBSun; // Cosine of angle of incidence of direct sun on window
		Real64 TVISBSun; // Window's visible transmittance at COSBSun
		//  (times light well efficiency, if appropriate)
		Real64 ZSU1; // Transmitted direct normal illuminance (lux)
		//  CHARACTER(len=32) :: ShType                    ! Window shading device type
		int ShType; // Window shading device type
		bool ShadeOn; // True if exterior or interior window shade present
		bool BlindOn; // True if exterior or interior window blind present
		bool ScreenOn; // True if exterior window screen present
		int BlNum; // Blind number
//		int ScNum; // Screen number //Unused Set but never used
		int PipeNum; // TDD pipe object number
		int ShelfNum; // Daylighting shelf object number
		int InShelfSurf; // Inside daylighting shelf surface number
		int OutShelfSurf; // Outside daylighting shelf surface number
		Real64 TransBlBmDiffFront; // Isolated blind vis beam-diffuse front transmittance
		Real64 TransScBmDiffFront; // Isolated screen vis beam-diffuse front transmittance
		Real64 TransScDiffDiffFront; // Isolated screen vis diffuse-diffuse front transmittance
		Real64 ReflGlDiffDiffBack; // Bare glazing system vis diffuse back reflectance
		Real64 ReflGlDiffDiffFront; // Bare glazing system vis diffuse front reflectance
		Real64 ReflBlBmDiffFront; // Isolated blind vis beam-diffuse front reflectance
		Real64 TransBlDiffDiffFront; // Isolated blind vis diffuse-diffuse front transmittance
		Real64 ReflBlDiffDiffFront; // Isolated blind vis diffuse-diffuse front reflectance
		Real64 ReflBlDiffDiffBack; // Isolated blind vis diffuse-diffuse back reflectance
		Real64 ReflScDiffDiffBack; // Isolated screen vis diffuse-diffuse back reflectance
		Real64 ProfAng; // Solar profile angle (radians)
		Real64 SlatAng; // Blind slat angle
		int JB; // Blind slat angle index
		Real64 t1; // Beam-beam vis trans of bare glass layers 1 and 2
		Real64 t2;
		Real64 td2; // Diffuse-diffuse vis trans of bare glass layers 2 and 3
		Real64 td3;
		Real64 rbd1; // Beam-diffuse back vis reflectance of bare glass layers 1 and 2
		Real64 rbd2;
		Real64 rfd2; // Beam-diffuse front vis reflectance of bare glass layers 2 and 3
		Real64 rfd3;
		Real64 tfshBd; // Beam-diffuse front vis trans of bare blind
		Real64 rfshB; // Beam-diffuse front vis reflectance of bare blind
		Real64 tfshd; // Diffuse-diffuse front vis trans of bare blind
		Real64 rbshd; // Diffuse-diffuse back vis reflectance of bare blind
		//unused  REAL(r64)         :: A                         ! Intermediate value for azimuth limits calculation
		Real64 ZSUObsRefl; // Illuminance on window from beam solar reflected by an
		//  obstruction (for unit beam normal illuminance)
		int NearestHitSurfNum; // Surface number of nearest obstruction
		int NearestHitSurfNumX; // Surface number to use when obstruction is a shadowing surface
		static Vector3< Real64 > NearestHitPt; // Hit point of ray on nearest obstruction (m)
		Real64 LumAtHitPtFrSun; // Luminance at hit point on obstruction from solar reflection
		//  for unit beam normal illuminance (cd/m2)
		Real64 SunObstructionMult; // = 1 if sun hits a ground point; otherwise = 0
		static Array2D< Real64 > SkyObstructionMult( NPHMAX, NTHMAX ); // Ratio of obstructed to unobstructed sky diffuse at
		// a ground point for each (TH,PH) direction
		Real64 Alfa; // Direction angles for ray heading towards the ground (radians)
		Real64 Beta;
		Real64 HorDis; // Distance between ground hit point and proj'n of window center onto ground (m)
		static Vector3< Real64 > GroundHitPt; // Coordinates of point that ray from window center hits the ground (m)
		int ObsSurfNum; // Obstruction surface number
		bool hitObs; // True iff obstruction is hit
		static Vector3< Real64 > ObsHitPt; // Coordinates of hit point on an obstruction (m)
		int ObsConstrNum; // Construction number of obstruction
		Real64 ObsVisRefl; // Visible reflectance of obstruction
		Real64 SkyReflVisLum; // Reflected sky luminance at hit point divided by unobstructed sky
		//  diffuse horizontal illuminance [(cd/m2)/lux]
		Real64 dReflObsSky; // Contribution to sky-related illuminance on window due to sky diffuse
		//  reflection from an obstruction
		static Vector3< Real64 > URay; // Unit vector in (Phi,Theta) direction
		Real64 TVisSunRefl; // Diffuse vis trans of bare window for beam reflection calc
		//  (times light well efficiency, if appropriate)
		Real64 ZSU1refl; // Beam normal illuminance times ZSU1refl = illuminance on window
		//  due to specular reflection from exterior surfaces

		int ZoneNumThisWin; // temporary to check if this window is actually in adjacent zone
		int ExtWinType; // Exterior window type (InZoneExtWin, AdjZoneExtWin, NotInOrAdjZoneExtWin)
		Real64 ZoneInsideSurfArea; // temporary for calculations, total surface area of zone surfaces m2
		int IntWinAdjZoneExtWinNum; // the index of the exterior window in IntWinAdjZoneExtWin nested struct
		int AdjExtWinLoop; // loop index for searching IntWinAdjZoneExtWin
		int IntWinLoop; // loop index for searching interior windows
		int IntWinNum; // window index for interior windows associated with exterior windows
		Real64 COSBintWin;

		ZoneNumThisWin = Surface( Surface( IWin ).BaseSurf ).Zone;
		// The inside surface area, ZoneDaylight(ZoneNum)%TotInsSurfArea was calculated in subr DayltgAveInteriorReflectance

		if ( ZoneNumThisWin == ZoneNum ) {
			ExtWinType = InZoneExtWin;
			ZoneInsideSurfArea = ZoneDaylight( ZoneNum ).TotInsSurfArea;
			IntWinAdjZoneExtWinNum = 0;
		} else {
			ExtWinType = AdjZoneExtWin;
			// If window is exterior window in adjacent zone, then use areas of both zones
			ZoneInsideSurfArea = ZoneDaylight( ZoneNum ).TotInsSurfArea + ZoneDaylight( ZoneNumThisWin ).TotInsSurfArea;
			// find index in IntWinAdjZoneExtWin
			for ( AdjExtWinLoop = 1; AdjExtWinLoop <= ZoneDaylight( ZoneNum ).NumOfIntWinAdjZoneExtWins; ++AdjExtWinLoop ) {
				if ( IWin == ZoneDaylight( ZoneNum ).IntWinAdjZoneExtWin( AdjExtWinLoop ).SurfNum ) { // found it
					IntWinAdjZoneExtWinNum = AdjExtWinLoop;
					break; // added TH 4/13/2010
				}
			}
		}

		// FLOW:
		// Initialize window luminance and fluxes for split-flux calculation
		WLUMSK( IHR, _, _ ) = 0.0;
		WLUMSU( IHR, _ ) = 0.0;
		WLUMSUdisk( IHR, _ ) = 0.0;
		FLFWSK = 0.0;
		FLFWSU = 0.0;
		FLFWSUdisk = 0.0;
		FLCWSK = 0.0;
		FLCWSU = 0.0;

		IConst = Surface( IWin ).Construction;
		if ( SurfaceWindow( IWin ).StormWinFlag == 1 ) IConst = Surface( IWin ).StormWinConstruction;
		BlindOn = false;
		ShadeOn = false;
		ScreenOn = false;

		if ( SurfaceWindow( IWin ).OriginalClass == SurfaceClass_TDD_Dome ) {
			PipeNum = FindTDDPipe( IWin );
		}

		ShelfNum = Surface( IWin ).Shelf;
		if ( ShelfNum > 0 ) {
			InShelfSurf = Shelf( ShelfNum ).InSurf; // Inside daylighting shelf present if > 0
			OutShelfSurf = Shelf( ShelfNum ).OutSurf; // Outside daylighting shelf present if > 0
		} else {
			InShelfSurf = 0;
			OutShelfSurf = 0;
		}

		// Divide sky and ground into elements of altitude PH and
		// azimuth TH, and add the contribution of light coming from each
		// element to the transmitted flux at the center of the window
		// Azimuth ranges over a maximum of 2 Pi radians.
		// Altitude ranges over a maximum of Pi/2 radians between -Pi/2 < PH < +Pi/2, so that elements are not counted twice
		// PH = 0 at the horizon; PH = Pi/2 at the zenith
		PHMIN = max( -PiOvr2, SurfaceWindow( IWin ).Phi - PiOvr2 );
		PHMAX = min( PiOvr2, SurfaceWindow( IWin ).Phi + PiOvr2 );
		DPH = ( PHMAX - PHMIN ) / double( NPHMAX );

		// Sky/ground element altitude integration
		Vector3< Real64 > const SUNCOS_IHR( SUNCOSHR( IHR, {1,3} ) );
		for ( IPH = 1; IPH <= NPHMAX; ++IPH ) {
			PH = PHMIN + ( double( IPH ) - 0.5 ) * DPH;

			SPH = std::sin( PH );
			CPH = std::cos( PH );
			// Third component of unit vector in (TH,PH) direction
			U( 3 ) = SPH;

			// Limits of azimuth integration
			PhWin = SurfaceWindow( IWin ).Phi;
			ThWin = SurfaceWindow( IWin ).Theta;
			if ( PhWin >= 0.0 ) {
				if ( PH >= PiOvr2 - PhWin ) {
					ThMin = -Pi;
					ThMax = Pi;
				} else {
					ACosTanTan = std::acos( -std::tan( PH ) * std::tan( PhWin ) );
					ThMin = ThWin - std::abs( ACosTanTan );
					ThMax = ThWin + std::abs( ACosTanTan );
				}

			} else { // PhiSurf < 0.0
				if ( PH <= -PhWin - PiOvr2 ) {
					ThMin = -Pi;
					ThMax = Pi;
				} else {
					ACosTanTan = std::acos( -std::tan( PH ) * std::tan( PhWin ) );
					ThMin = ThWin - std::abs( ACosTanTan );
					ThMax = ThWin + std::abs( ACosTanTan );
				}
			}

			DTH = ( ThMax - ThMin ) / double( NTHMAX );
			DA = CPH * DTH * DPH;

			// Sky/ground element azimuth integration
			Real64 const sin_window_phi( std::sin( SurfaceWindow( IWin ).Phi ) );
			Real64 const cos_window_phi( std::cos( SurfaceWindow( IWin ).Phi ) );
			for ( ITH = 1; ITH <= NTHMAX; ++ITH ) {
				TH = ThMin + ( double( ITH ) - 0.5 ) * DTH;
				U( 1 ) = CPH * std::cos( TH );
				U( 2 ) = CPH * std::sin( TH );
				// Cosine of angle of incidence of light from sky or ground element
				COSB = SPH * sin_window_phi + CPH * cos_window_phi * std::cos( TH - SurfaceWindow( IWin ).Theta );
				if ( COSB < 0.0 ) continue; // Sky/ground elements behind window (although there shouldn't be any)

				// Initialize illuminance on window for this sky/ground element
				ZSK = 0.0;
				ZSU = 0.0;
				// Initialize illuminance on window from beam solar reflection if ray hits an obstruction
				ZSUObsRefl = 0.0;

				if ( ISunPos == 1 ) { // Intersection calculation has to be done only for first sun position
					// Determine net transmittance of obstructions that the ray hits. ObTrans will be 1.0
					// if no obstructions are hit.
					DayltgHitObstruction( IHR, IWin, SurfaceWindow( IWin ).WinCenter, U, ObTrans );
					ObTransM( IPH, ITH ) = ObTrans;
				}

				// SKY AND GROUND RADIATION ON WINDOW

				// Contribution is from sky if PH > 0 (ray goes upward), and from ground if PH < 0 (ray goes downward)
				// (There may also be contributions from reflection from obstructions; see 'BEAM SOLAR AND SKY SOLAR
				// REFLECTED FROM NEAREST OBSTRUCTION,' below.)

				if ( ISunPos == 1 ) SkyObstructionMult( IPH, ITH ) = 1.0;
				if ( PH > 0.0 ) { // Contribution is from sky
					for ( ISky = 1; ISky <= 4; ++ISky ) {
						ZSK( ISky ) = DayltgSkyLuminance( ISky, TH, PH ) * COSB * DA * ObTransM( IPH, ITH );
					}
				} else { // PH <= 0.0; contribution is from ground
					if ( CalcSolRefl && ObTransM( IPH, ITH ) > 1.e-6 && ISunPos == 1 ) {
						// Calculate effect of obstructions on shading of sky diffuse reaching the ground point hit
						// by the ray. This effect is given by the ratio SkyObstructionMult =
						// (obstructed sky diffuse at ground point)/(unobstructed sky diffuse at ground point).
						// This ratio is calculated for an isotropic sky.
						// Ground point hit by the ray:
						Alfa = std::acos( -U( 3 ) );
						Beta = std::atan2( U( 2 ), U( 1 ) );
						HorDis = ( SurfaceWindow( IWin ).WinCenter( 3 ) - GroundLevelZ ) * std::tan( Alfa );
						GroundHitPt( 3 ) = GroundLevelZ;
						GroundHitPt( 1 ) = SurfaceWindow( IWin ).WinCenter( 1 ) + HorDis * std::cos( Beta );
						GroundHitPt( 2 ) = SurfaceWindow( IWin ).WinCenter( 2 ) + HorDis * std::sin( Beta );

						SkyObstructionMult( IPH, ITH ) = CalcObstrMultiplier( GroundHitPt, AltAngStepsForSolReflCalc, AzimAngStepsForSolReflCalc );
					} // End of check if solar reflection calc is in effect
					for ( ISky = 1; ISky <= 4; ++ISky ) {
						// Below, luminance of ground in cd/m2 is illuminance on ground in lumens/m2
						// times ground reflectance, divided by pi, times obstruction multiplier.
						ZSK( ISky ) = ( GILSK( IHR, ISky ) * GndReflectanceForDayltg / Pi ) * COSB * DA * ObTransM( IPH, ITH ) * SkyObstructionMult( IPH, ITH );
					}
					// Determine if sun illuminates the point that ray hits the ground. If the solar reflection
					// calculation has been requested (CalcSolRefl = .TRUE.) shading by obstructions, including
					// the building itself, is considered in determining whether sun hits the ground point.
					// Otherwise this shading is ignored and the sun always hits the ground point.
					SunObstructionMult = 1.0;
					if ( CalcSolRefl && ObTransM( IPH, ITH ) > 1.e-6 ) {
						// Sun reaches ground point if vector from this point to the sun is unobstructed
						hitObs = false;
						for ( ObsSurfNum = 1; ObsSurfNum <= TotSurfaces; ++ObsSurfNum ) {
							if ( ! Surface( ObsSurfNum ).ShadowSurfPossibleObstruction ) continue;
							PierceSurface( ObsSurfNum, GroundHitPt, SUNCOS_IHR, ObsHitPt, hitObs );
							if ( hitObs ) break;
						}
						if ( hitObs ) SunObstructionMult = 0.0;
					}
					ZSU = ( GILSU( IHR ) * GndReflectanceForDayltg / Pi ) * COSB * DA * ObTransM( IPH, ITH ) * SunObstructionMult;
				}

				// BEAM SOLAR AND SKY SOLAR REFLECTED FROM NEAREST OBSTRUCTION

				if ( CalcSolRefl && ObTransM( IPH, ITH ) < 1.0 ) {
					// Find obstruction whose hit point is closest to the center of the window
					DayltgClosestObstruction( SurfaceWindow( IWin ).WinCenter, U, NearestHitSurfNum, NearestHitPt );
					if ( NearestHitSurfNum > 0 ) {

						// Beam solar reflected from nearest obstruction.
						DayltgSurfaceLumFromSun( IHR, U, NearestHitSurfNum, NearestHitPt, LumAtHitPtFrSun );
						ZSUObsRefl = LumAtHitPtFrSun * COSB * DA;
						ZSU += ZSUObsRefl;

						// Sky solar reflected from nearest obstruction.
						ObsConstrNum = Surface( NearestHitSurfNum ).Construction;
						if ( ObsConstrNum > 0 ) {
							// Exterior building surface is nearest hit
							if ( ! Construct( ObsConstrNum ).TypeIsWindow ) {
								// Obstruction is not a window, i.e., is an opaque surface
								ObsVisRefl = 1.0 - Material( Construct( ObsConstrNum ).LayerPoint( 1 ) ).AbsorpVisible;
							} else {
								// Obstruction is a window; assume it is bare
								if ( SurfaceWindow( NearestHitSurfNum ).StormWinFlag == 1 ) ObsConstrNum = Surface( NearestHitSurfNum ).StormWinConstruction;
								ObsVisRefl = Construct( ObsConstrNum ).ReflectVisDiffFront;
							}
						} else {
							// Shadowing surface is nearest hit
							if ( Surface( NearestHitSurfNum ).Shelf > 0 ) {
								// Skip daylighting shelves, whose reflection is separately calculated
								ObsVisRefl = 0.0;
							} else {
								ObsVisRefl = Surface( NearestHitSurfNum ).ShadowSurfDiffuseVisRefl;
								if ( Surface( NearestHitSurfNum ).ShadowSurfGlazingConstruct > 0 ) ObsVisRefl += Surface( NearestHitSurfNum ).ShadowSurfGlazingFrac * Construct( Surface( NearestHitSurfNum ).ShadowSurfGlazingConstruct ).ReflectVisDiffFront;
								// Note in the above that ShadowSurfDiffuseVisRefl is the reflectance of opaque part of
								// shadowing surface times (1 - ShadowSurfGlazingFrac)
							}
						}
						NearestHitSurfNumX = NearestHitSurfNum;
						// Each shadowing surface has a "mirror" duplicate surface facing in the opposite direction.
						// The following gets the correct side of a shadowing surface for reflection.
						if ( Surface( NearestHitSurfNum ).ShadowingSurf ) {
							if ( dot( U, Surface( NearestHitSurfNum ).OutNormVec ) > 0.0 ) NearestHitSurfNumX = NearestHitSurfNum + 1;
						}
						if ( ! DetailedSkyDiffuseAlgorithm || ! ShadingTransmittanceVaries || SolarDistribution == MinimalShadowing ) {
							SkyReflVisLum = ObsVisRefl * Surface( NearestHitSurfNumX ).ViewFactorSky * DifShdgRatioIsoSky( NearestHitSurfNumX ) / Pi;
						} else {
							SkyReflVisLum = ObsVisRefl * Surface( NearestHitSurfNumX ).ViewFactorSky * DifShdgRatioIsoSkyHRTS( 1, IHR, NearestHitSurfNumX ) / Pi;
						}
						dReflObsSky = SkyReflVisLum * COSB * DA;
						for ( ISky = 1; ISky <= 4; ++ISky ) {
							ZSK( ISky ) += GILSK( IHR, ISky ) * dReflObsSky;
						}
					}
				} // End of check if exterior solar reflection calculation is active

				//  ===Bare window (no shade or blind; non-diffusing glass)===

				// Increment flux entering space and window luminance (cd/m2).
				// FLCW--(I,J) = part of incoming flux (in lumens) that goes up to ceiling and upper part of walls.
				// FLFW--(I,J) = part that goes down to floor and lower part of walls

				if ( SurfaceWindow( IWin ).OriginalClass == SurfaceClass_TDD_Dome ) {
					// Unshaded visible transmittance of TDD for a single ray from sky/ground element
					TVISBR = TransTDD( PipeNum, COSB, VisibleBeam ) * SurfaceWindow( IWin ).GlazedFrac;

					// Make all transmitted light diffuse for a TDD with a bare diffuser
					for ( ISky = 1; ISky <= 4; ++ISky ) {
						WLUMSK( IHR, 1, ISky ) += ZSK( ISky ) * TVISBR / Pi;
						FLFWSK( 1, ISky ) += ZSK( ISky ) * TVISBR * ( 1.0 - SurfaceWindow( IWin ).FractionUpgoing );
						FLCWSK( 1, ISky ) += ZSK( ISky ) * TVISBR * SurfaceWindow( IWin ).FractionUpgoing;

						// For later calculation of diffuse visible transmittance
						TDDFluxInc( IHR, ISky, PipeNum ) += ZSK( ISky );
						TDDFluxTrans( IHR, ISky, PipeNum ) += ZSK( ISky ) * TVISBR;

						if ( ISky == 1 ) {
							WLUMSU( IHR, 1 ) += ZSU * TVISBR / Pi;
							FLFWSU( 1 ) += ZSU * TVISBR * ( 1.0 - SurfaceWindow( IWin ).FractionUpgoing );
							FLCWSU( 1 ) += ZSU * TVISBR * SurfaceWindow( IWin ).FractionUpgoing;

							// For later calculation of diffuse visible transmittance
							TDDFluxInc( IHR, ISky, PipeNum ) += ZSU;
							TDDFluxTrans( IHR, ISky, PipeNum ) += ZSU * TVISBR;
						}
					}

				} else { // Bare window

					// Transmittance of bare window for this sky/ground element
					TVISBR = POLYF( COSB, Construct( IConst ).TransVisBeamCoef ) * SurfaceWindow( IWin ).GlazedFrac * SurfaceWindow( IWin ).LightWellEff;

					if ( InShelfSurf > 0 ) { // Inside daylighting shelf
						// Daylighting shelf simplification:  All light is diffuse
						// SurfaceWindow(IWin)%FractionUpgoing is already set to 1.0 earlier
						for ( ISky = 1; ISky <= 4; ++ISky ) {
							FLCWSK( 1, ISky ) += ZSK( ISky ) * TVISBR * SurfaceWindow( IWin ).FractionUpgoing;

							if ( ISky == 1 ) {
								FLCWSU( 1 ) += ZSU * TVISBR * SurfaceWindow( IWin ).FractionUpgoing;
							}
						}

					} else { // Normal window

						// CR 7869  correct TVISBR if disk beam passes thru interior window
						if ( ExtWinType == AdjZoneExtWin ) {
							// modify TVISBR by second window transmission
							// first determine if ray from point passes thru any interior window
							hitObs = false;
							for ( IntWinLoop = 1; IntWinLoop <= ZoneDaylight( ZoneNum ).IntWinAdjZoneExtWin( IntWinAdjZoneExtWinNum ).NumOfIntWindows; ++IntWinLoop ) {
								IntWinNum = ZoneDaylight( ZoneNum ).IntWinAdjZoneExtWin( IntWinAdjZoneExtWinNum ).IntWinNum( IntWinLoop );
								PierceSurface( IntWinNum, SurfaceWindow( IntWinNum ).WinCenter, SUNCOS_IHR, ObsHitPt, hitObs );
								if ( hitObs ) { // disk passes thru
									// cosine of incidence angle of light from sky or ground element for
									COSBintWin = SPH * std::sin( SurfaceWindow( IntWinNum ).Phi ) + CPH * std::cos( SurfaceWindow( IntWinNum ).Phi ) * std::cos( TH - SurfaceWindow( IntWinNum ).Theta );
									TVISBR *= POLYF( COSBintWin, Construct( Surface( IntWinNum ).Construction ).TransVisBeamCoef );
									break;
								}
							}
							if ( !hitObs ) { // blocked by opaque parts, beam does not actually pass thru interior window to reach zone
								TVISBR = 0.0;
							}
						}

						for ( ISky = 1; ISky <= 4; ++ISky ) {
							//IF (PH < 0.0d0) THEN
							//Fixed by FCW, Nov. 2003:
							if ( PH > 0.0 ) {
								FLFWSK( 1, ISky ) += ZSK( ISky ) * TVISBR;
								if ( ISky == 1 ) FLFWSU( 1 ) += ZSU * TVISBR;
							} else {
								FLCWSK( 1, ISky ) += ZSK( ISky ) * TVISBR;
								if ( ISky == 1 ) FLCWSU( 1 ) += ZSU * TVISBR;
							}

						}
					} // End of check if window with daylighting shelf or normal window
				} // End of check if TDD:DOME or bare window

				// Check if window has shade or blind
				ICtrl = Surface( IWin ).WindowShadingControlPtr;
				if ( ICtrl > 0 ) {
					ShType = WindowShadingControl( ICtrl ).ShadingType;
					BlNum = SurfaceWindow( IWin ).BlindNumber;
//					ScNum = SurfaceWindow( IWin ).ScreenNumber; //Unused Set but never used

					ShadeOn = ( ShType == WSC_ST_InteriorShade || ShType == WSC_ST_ExteriorShade || ShType == WSC_ST_BetweenGlassShade );
					BlindOn = ( ShType == WSC_ST_InteriorBlind || ShType == WSC_ST_ExteriorBlind || ShType == WSC_ST_BetweenGlassBlind );
					ScreenOn = ( ShType == WSC_ST_ExteriorScreen );
				}

				if ( ShadeOn || BlindOn || ScreenOn || SurfaceWindow( IWin ).SolarDiffusing ) {

					// ===Window with interior or exterior shade or blind, exterior screen, or with diffusing glass===

					// Increment flux entering space and window luminance. Shades and diffusing glass are
					// assumed to be perfect diffusers, i.e., the transmittance is independent of angle of
					// incidence and the transmitted light is isotropic. The transmittance of a blind is
					// assumed to depend on profile angle and slat angle; the diffuse light entering the room from
					// the slats of the blind is assumed to be isotropic. With blinds, light can also enter
					// the room by passing between the slats without reflection. The beam transmittance of a screen
					// is assumed to depend on sun azimuth and azimuth angle.

					// For light from a shade, or from diffusing glass, or from the slats of a blind, a flux fraction,
					// SurfaceWindow(IWin)%FractionUpgoing (determined by window tilt), goes up toward
					// ceiling and upper part of walls, and 1-Surfacewindow(iwin)%FractionUpgoing
					// goes down toward floor and lower part of walls. For a blind, the light passing
					// between the slats goes either up or down depending on the altitude angle of the
					// element from which the light came. For a screen, the light passing
					// between the screen's cylinders goes either up or down depending on the altitude angle of the
					// element from which the light came.

					IConstShaded = Surface( IWin ).ShadedConstruction;
					if ( SurfaceWindow( IWin ).StormWinFlag == 1 ) IConstShaded = Surface( IWin ).StormWinShadedConstruction;
					if ( SurfaceWindow( IWin ).SolarDiffusing ) IConstShaded = Surface( IWin ).Construction;

					// Transmittance of window including shade, screen or blind
					TransBmBmMult = 0.0;
					TransMult = 0.0;

					if ( ShadeOn ) { // Shade
						if ( SurfaceWindow( IWin ).OriginalClass == SurfaceClass_TDD_Dome ) {
							// Shaded visible transmittance of TDD for a single ray from sky/ground element
							TransMult( 1 ) = TransTDD( PipeNum, COSB, VisibleBeam ) * SurfaceWindow( IWin ).GlazedFrac;
						} else { // Shade only, no TDD
							// Calculate transmittance of the combined window and shading device for this sky/ground element
							TransMult( 1 ) = POLYF( COSB, Construct( IConstShaded ).TransVisBeamCoef ) * SurfaceWindow( IWin ).GlazedFrac * SurfaceWindow( IWin ).LightWellEff;
						}

					} else if ( ScreenOn ) { // Screen: get beam-beam, beam-diffuse and diffuse-diffuse vis trans/ref of screen and glazing system
						CalcScreenTransmittance( IWin, ( PH - SurfaceWindow( IWin ).Phi ), ( TH - SurfaceWindow( IWin ).Theta ) );
						ReflGlDiffDiffFront = Construct( IConst ).ReflectVisDiffFront;
						ReflScDiffDiffBack = SurfaceScreens( SurfaceWindow( IWin ).ScreenNumber ).DifReflectVis;
						TransScBmDiffFront = SurfaceScreens( SurfaceWindow( IWin ).ScreenNumber ).BmDifTransVis;
						TransMult( 1 ) = TransScBmDiffFront * SurfaceWindow( IWin ).GlazedFrac * Construct( IConst ).TransDiffVis / ( 1 - ReflGlDiffDiffFront * ReflScDiffDiffBack ) * SurfaceWindow( IWin ).LightWellEff;
						TransBmBmMult( 1 ) = SurfaceScreens( SurfaceWindow( IWin ).ScreenNumber ).BmBmTransVis;

					} else if ( BlindOn ) { // Blind: get beam-diffuse and beam-beam vis trans of blind+glazing system
						// PETER:  As long as only interior blinds are allowed for TDDs, no need to change TransMult calculation
						//         for TDDs because it is based on TVISBR which is correctly calculated for TDDs above.

						ProfileAngle( IWin, U, Blind( BlNum ).SlatOrientation, ProfAng );

						for ( JB = 1; JB <= MaxSlatAngs; ++JB ) {
							if ( ! SurfaceWindow( IWin ).MovableSlats && JB > 1 ) break;

							TransBlBmDiffFront = InterpProfAng( ProfAng, Blind( BlNum ).VisFrontBeamDiffTrans( JB, {1,37} ) );

							if ( ShType == WSC_ST_InteriorBlind ) { // Interior blind
								ReflGlDiffDiffBack = Construct( IConst ).ReflectVisDiffBack;
								ReflBlBmDiffFront = InterpProfAng( ProfAng, Blind( BlNum ).VisFrontBeamDiffRefl( JB, {1,37} ) );
								ReflBlDiffDiffFront = Blind( BlNum ).VisFrontDiffDiffRefl( JB );
								TransBlDiffDiffFront = Blind( BlNum ).VisFrontDiffDiffTrans( JB );
								TransMult( JB ) = TVISBR * ( TransBlBmDiffFront + ReflBlBmDiffFront * ReflGlDiffDiffBack * TransBlDiffDiffFront / ( 1.0 - ReflBlDiffDiffFront * ReflGlDiffDiffBack ) );

							} else if ( ShType == WSC_ST_ExteriorBlind ) { // Exterior blind
								ReflGlDiffDiffFront = Construct( IConst ).ReflectVisDiffFront;
								ReflBlDiffDiffBack = Blind( BlNum ).VisBackDiffDiffRefl( JB );
								TransMult( JB ) = TransBlBmDiffFront * SurfaceWindow( IWin ).GlazedFrac * Construct( IConst ).TransDiffVis / ( 1.0 - ReflGlDiffDiffFront * ReflBlDiffDiffBack ) * SurfaceWindow( IWin ).LightWellEff;

							} else { // Between-glass blind
								t1 = POLYF( COSB, Construct( IConst ).tBareVisCoef( {1,6}, 1 ) );
								td2 = Construct( IConst ).tBareVisDiff( 2 );
								rbd1 = Construct( IConst ).rbBareVisDiff( 1 );
								rfd2 = Construct( IConst ).rfBareVisDiff( 2 );
								tfshBd = InterpProfAng( ProfAng, Blind( BlNum ).VisFrontBeamDiffTrans( JB, {1,37} ) );
								tfshd = Blind( BlNum ).VisFrontDiffDiffTrans( JB );
								rfshB = InterpProfAng( ProfAng, Blind( BlNum ).VisFrontBeamDiffRefl( JB, {1,37} ) );
								rbshd = Blind( BlNum ).VisFrontDiffDiffRefl( JB );
								if ( Construct( IConst ).TotGlassLayers == 2 ) { // 2 glass layers
									TransMult( JB ) = t1 * ( tfshBd * ( 1.0 + rfd2 * rbshd ) + rfshB * rbd1 * tfshd ) * td2 * SurfaceWindow( IWin ).LightWellEff;
								} else { // 3 glass layers; blind between layers 2 and 3
									t2 = POLYF( COSB, Construct( IConst ).tBareVisCoef( {1,6}, 2 ) );
									td3 = Construct( IConst ).tBareVisDiff( 3 );
									rfd3 = Construct( IConst ).rfBareVisDiff( 3 );
									rbd2 = Construct( IConst ).rbBareVisDiff( 2 );
									TransMult( JB ) = t1 * t2 * ( tfshBd * ( 1.0 + rfd3 * rbshd ) + rfshB * ( rbd2 * tfshd + td2 * rbd1 * td2 * tfshd ) ) * td3 * SurfaceWindow( IWin ).LightWellEff;
								}
							}

							if ( SurfaceWindow( IWin ).MovableSlats ) {
								SlatAng = ( JB - 1 ) * Pi / ( MaxSlatAngs - 1 );
							} else {
								SlatAng = Blind( BlNum ).SlatAngle * DegToRadians;
							}
							TransBmBmMult( JB ) = TVISBR * BlindBeamBeamTrans( ProfAng, SlatAng, Blind( BlNum ).SlatWidth, Blind( BlNum ).SlatSeparation, Blind( BlNum ).SlatThickness );
						} // End of loop over slat angles

					} else { // Diffusing glass
						TransMult( 1 ) = POLYF( COSB, Construct( IConstShaded ).TransVisBeamCoef ) * SurfaceWindow( IWin ).GlazedFrac * SurfaceWindow( IWin ).LightWellEff;
					} // End of check if shade, blind or diffusing glass

					if ( SurfaceWindow( IWin ).OriginalClass == SurfaceClass_TDD_Dome ) {
						// No beam is transmitted.  This takes care of all types of screens and blinds.
						TransBmBmMult = 0.0;
					}

					// Daylighting shelf simplification:  No beam makes it past end of shelf, all light is diffuse
					if ( InShelfSurf > 0 ) { // Inside daylighting shelf
						TransBmBmMult = 0.0; // No beam, diffuse only
					}

					// TransBmBmMult is used in the following for windows with blinds or screens to get contribution from light
					// passing directly between slats or between screen material without reflection.

					for ( ISky = 1; ISky <= 4; ++ISky ) {
						for ( JB = 1; JB <= MaxSlatAngs; ++JB ) {
							// EXIT after first pass if not movable slats or exterior window screen
							if ( ! SurfaceWindow( IWin ).MovableSlats && JB > 1 ) break;

							WLUMSK( IHR, JB + 1, ISky ) += ZSK( ISky ) * TransMult( JB ) / Pi;
							FLFWSK( JB + 1, ISky ) += ZSK( ISky ) * TransMult( JB ) * ( 1.0 - SurfaceWindow( IWin ).FractionUpgoing );
							if ( PH > 0.0 && ( BlindOn || ScreenOn ) ) FLFWSK( JB + 1, ISky ) += ZSK( ISky ) * TransBmBmMult( JB );
							FLCWSK( JB + 1, ISky ) += ZSK( ISky ) * TransMult( JB ) * SurfaceWindow( IWin ).FractionUpgoing;
							if ( PH <= 0.0 && ( BlindOn || ScreenOn ) ) FLCWSK( JB + 1, ISky ) += ZSK( ISky ) * TransBmBmMult( JB );
							if ( ISky == 1 ) {
								WLUMSU( IHR, JB + 1 ) += ZSU * TransMult( JB ) / Pi;
								FLFWSU( JB + 1 ) += ZSU * TransMult( JB ) * ( 1.0 - SurfaceWindow( IWin ).FractionUpgoing );
								if ( PH > 0.0 && ( BlindOn || ScreenOn ) ) FLFWSU( JB + 1 ) += ZSU * TransBmBmMult( JB );
								FLCWSU( JB + 1 ) += ZSU * TransMult( JB ) * SurfaceWindow( IWin ).FractionUpgoing;
								if ( PH <= 0.0 && ( BlindOn || ScreenOn ) ) FLCWSU( JB + 1 ) += ZSU * TransBmBmMult( JB );
							}
						}
					}
				} // End of window with shade, screen, blind or diffusing glass

			} // End of azimuth integration loop, ITH
		} // End of altitude integration loop, IPH

		if ( OutShelfSurf > 0 ) { // Outside daylighting shelf
			// Add exterior diffuse illuminance due to outside shelf
			// Since all of the illuminance is added to the zone as upgoing diffuse, it can be added as a lump sum here

			TVISBR = Construct( IConst ).TransDiffVis; // Assume diffuse transmittance for shelf illuminance

			for ( ISky = 1; ISky <= 4; ++ISky ) {
				// This is only an estimate because the anisotropic sky view of the shelf is not yet taken into account.
				// AnisoSkyMult would be great to use but it is not available until the heat balance starts up.
				ZSK( ISky ) = GILSK( IHR, ISky ) * 1.0 * Shelf( ShelfNum ).OutReflectVis * Shelf( ShelfNum ).ViewFactor;

				// SurfaceWindow(IWin)%FractionUpgoing is already set to 1.0 earlier
				FLCWSK( 1, ISky ) += ZSK( ISky ) * TVISBR * SurfaceWindow( IWin ).FractionUpgoing;

				if ( ISky == 1 ) {
					ZSU = GILSU( IHR ) * SunlitFracHR( IHR, OutShelfSurf ) * Shelf( ShelfNum ).OutReflectVis * Shelf( ShelfNum ).ViewFactor;
					FLCWSU( 1 ) += ZSU * TVISBR * SurfaceWindow( IWin ).FractionUpgoing;
				}
			} // ISKY
		}

		// Sky-related portion of internally reflected illuminance.
		// The inside surface area, ZoneDaylight(ZoneNum)%TotInsSurfArea, and ZoneDaylight(ZoneNum)%AveVisDiffReflect,
		// were calculated in subr DayltgAveInteriorReflectance.

		for ( ISky = 1; ISky <= 4; ++ISky ) {
			for ( JSH = 1; JSH <= MaxSlatAngs + 1; ++JSH ) {
				if ( ! SurfaceWindow( IWin ).MovableSlats && JSH > 2 ) break;
				// Full area of window is used in following since effect of dividers on reducing
				// effective window transmittance has already been accounted for in calc of FLFWSK and FLCWSK.
				EINTSK( IHR, JSH, ISky ) = ( FLFWSK( JSH, ISky ) * SurfaceWindow( IWin ).RhoFloorWall + FLCWSK( JSH, ISky ) * SurfaceWindow( IWin ).RhoCeilingWall ) * ( Surface( IWin ).Area / SurfaceWindow( IWin ).GlazedFrac ) / ( ZoneInsideSurfArea * ( 1.0 - ZoneDaylight( ZoneNum ).AveVisDiffReflect ) );
			} // JSH
		} // ISKY

		// BEAM SOLAR RADIATION ON WINDOW

		// Beam reaching window directly (without specular reflection from exterior obstructions)

		if ( SunlitFracHR( IHR, IWin ) > 0.0 ) {
			// Cos of angle of incidence
			COSBSun = SPHSUN * std::sin( SurfaceWindow( IWin ).Phi ) + CPHSUN * std::cos( SurfaceWindow( IWin ).Phi ) * std::cos( THSUN - SurfaceWindow( IWin ).Theta );

			if ( COSBSun > 0.0 ) {
				// Multiply direct normal illuminance (normalized to 1.0 lux)
				// by incident angle factor and by fraction of window that is sunlit.
				// Note that in the following SunlitFracHR accounts for possibly non-zero transmittance of
				// shading surfaces.

				ZSU1 = COSBSun * SunlitFracHR( IHR, IWin );

				// Contribution to window luminance and downgoing flux

				// -- Bare window

				if ( SurfaceWindow( IWin ).OriginalClass == SurfaceClass_TDD_Dome ) {
					// Unshaded visible transmittance of TDD for collimated beam from the sun
					TVISBSun = TransTDD( PipeNum, COSBSun, VisibleBeam ) * SurfaceWindow( IWin ).GlazedFrac;
					TDDTransVisBeam( IHR, PipeNum ) = TVISBSun;

					FLFWSUdisk( 1 ) = 0.0; // Diffuse light only

					WLUMSU( IHR, 1 ) += ZSU1 * TVISBSun / Pi;
					FLFWSU( 1 ) += ZSU1 * TVISBSun * ( 1.0 - SurfaceWindow( IWin ).FractionUpgoing );
					FLCWSU( 1 ) += ZSU1 * TVISBSun * SurfaceWindow( IWin ).FractionUpgoing;

				} else { // Bare window
					TVISBSun = POLYF( COSBSun, Construct( IConst ).TransVisBeamCoef ) * SurfaceWindow( IWin ).GlazedFrac * SurfaceWindow( IWin ).LightWellEff;

					// Daylighting shelf simplification:  No beam makes it past end of shelf, all light is diffuse
					if ( InShelfSurf > 0 ) { // Inside daylighting shelf
						FLFWSUdisk( 1 ) = 0.0; // Diffuse light only

						// SurfaceWindow(IWin)%FractionUpgoing is already set to 1.0 earlier
						//WLUMSU(1,IHR) = WLUMSU(1,IHR) + ZSU1 * TVISBSun / PI
						//FLFWSU(1) = FLFWSU(1) + ZSU1 * TVISBSun * (1.0 - SurfaceWindow(IWin)%FractionUpgoing)
						FLCWSU( 1 ) += ZSU1 * TVISBSun * SurfaceWindow( IWin ).FractionUpgoing;
					} else { // Normal window
						FLFWSUdisk( 1 ) = ZSU1 * TVISBSun;
					}
				}

				// -- Window with shade, screen, blind or diffusing glass
				if ( ShadeOn || BlindOn || ScreenOn || SurfaceWindow( IWin ).SolarDiffusing ) {
					TransBmBmMult = 0.0;
					TransMult = 0.0;

					// TH 7/7/2010 moved from inside the loop: DO JB = 1,MaxSlatAngs
					if ( BlindOn ) ProfileAngle( IWin, SUNCOSHR( IHR, {1,3} ), Blind( BlNum ).SlatOrientation, ProfAng );

					for ( JB = 1; JB <= MaxSlatAngs; ++JB ) {
						if ( ! SurfaceWindow( IWin ).MovableSlats && JB > 1 ) break;

						if ( ShadeOn || ScreenOn || SurfaceWindow( IWin ).SolarDiffusing ) { // Shade or screen on or diffusing glass
							if ( SurfaceWindow( IWin ).OriginalClass == SurfaceClass_TDD_Dome ) {
								// Shaded visible transmittance of TDD for collimated beam from the sun
								TransMult( 1 ) = TransTDD( PipeNum, COSBSun, VisibleBeam ) * SurfaceWindow( IWin ).GlazedFrac;
							} else {
								if ( ScreenOn ) {
									TransMult( 1 ) = SurfaceScreens( SurfaceWindow( IWin ).ScreenNumber ).BmBmTransVis * SurfaceWindow( IWin ).GlazedFrac * SurfaceWindow( IWin ).LightWellEff;
								} else {
									TransMult( 1 ) = POLYF( COSBSun, Construct( IConstShaded ).TransVisBeamCoef ) * SurfaceWindow( IWin ).GlazedFrac * SurfaceWindow( IWin ).LightWellEff;
								}
							}

						} else { // Blind on

							// PETER:  As long as only interior blinds are allowed for TDDs, no need to change TransMult calculation
							//         for TDDs because it is based on TVISBSun which is correctly calculated for TDDs above.

							// TH 7/7/2010: This call is moved outside the loop - DO JB = 1,MaxSlatAngs
							//CALL ProfileAngle(IWin,SUNCOSHR(1:3,IHR),Blind(BlNum)%SlatOrientation,ProfAng)

							TransBlBmDiffFront = InterpProfAng( ProfAng, Blind( BlNum ).VisFrontBeamDiffTrans( JB, {1,37} ) );

							if ( ShType == WSC_ST_InteriorBlind ) { // Interior blind
								// TH CR 8121, 7/7/2010
								//ReflBlBmDiffFront = InterpProfAng(ProfAng,Blind(BlNum)%VisFrontBeamDiffRefl)
								ReflBlBmDiffFront = InterpProfAng( ProfAng, Blind( BlNum ).VisFrontBeamDiffRefl( JB, {1,37} ) );

								// TH added 7/12/2010 for CR 8121
								ReflBlDiffDiffFront = Blind( BlNum ).VisFrontDiffDiffRefl( JB );
								TransBlDiffDiffFront = Blind( BlNum ).VisFrontDiffDiffTrans( JB );

								TransMult( JB ) = TVISBSun * ( TransBlBmDiffFront + ReflBlBmDiffFront * ReflGlDiffDiffBack * TransBlDiffDiffFront / ( 1.0 - ReflBlDiffDiffFront * ReflGlDiffDiffBack ) );

							} else if ( ShType == WSC_ST_ExteriorBlind ) { // Exterior blind
								TransMult( JB ) = TransBlBmDiffFront * ( Construct( IConst ).TransDiffVis / ( 1.0 - ReflGlDiffDiffFront * Blind( BlNum ).VisBackDiffDiffRefl( JB ) ) ) * SurfaceWindow( IWin ).GlazedFrac * SurfaceWindow( IWin ).LightWellEff;

							} else { // Between-glass blind
								t1 = POLYF( COSBSun, Construct( IConst ).tBareVisCoef( {1,6}, 1 ) );
								tfshBd = InterpProfAng( ProfAng, Blind( BlNum ).VisFrontBeamDiffTrans( JB, {1,37} ) );
								rfshB = InterpProfAng( ProfAng, Blind( BlNum ).VisFrontBeamDiffRefl( JB, {1,37} ) );
								if ( Construct( IConst ).TotGlassLayers == 2 ) { // 2 glass layers
									TransMult( JB ) = t1 * ( tfshBd * ( 1.0 + rfd2 * rbshd ) + rfshB * rbd1 * tfshd ) * td2 * SurfaceWindow( IWin ).LightWellEff;
								} else { // 3 glass layers; blind between layers 2 and 3
									t2 = POLYF( COSBSun, Construct( IConst ).tBareVisCoef( {1,6}, 2 ) );
									TransMult( JB ) = t1 * t2 * ( tfshBd * ( 1.0 + rfd3 * rbshd ) + rfshB * ( rbd2 * tfshd + td2 * rbd1 * td2 * tfshd ) ) * td3 * SurfaceWindow( IWin ).LightWellEff;
								}
							}
							if ( SurfaceWindow( IWin ).MovableSlats ) {
								SlatAng = ( JB - 1 ) * Pi / ( MaxSlatAngs - 1 );
							} else {
								SlatAng = Blind( BlNum ).SlatAngle * DegToRadians;
							}
							TransBmBmMult( JB ) = TVISBSun * BlindBeamBeamTrans( ProfAng, SlatAng, Blind( BlNum ).SlatWidth, Blind( BlNum ).SlatSeparation, Blind( BlNum ).SlatThickness );
						} // ShadeOn/ScreenOn/BlindOn/Diffusing glass

						if ( SurfaceWindow( IWin ).OriginalClass == SurfaceClass_TDD_Dome ) {
							TransBmBmMult = 0.0; // No beam, diffuse only
						}

						// Daylighting shelf simplification:  No beam makes it past end of shelf, all light is diffuse
						if ( InShelfSurf > 0 ) { // Inside daylighting shelf
							TransBmBmMult = 0.0; // No beam, diffuse only (Not sure if this really works)
							// SurfaceWindow(IWin)%FractionUpgoing is already set to 1.0 earlier
						}

						WLUMSU( IHR, JB + 1 ) += ZSU1 * TransMult( JB ) / Pi;
						WLUMSUdisk( IHR, JB + 1 ) = ZSU1 * TransBmBmMult( JB ) / Pi;
						FLFWSU( JB + 1 ) += ZSU1 * TransMult( JB ) * ( 1.0 - SurfaceWindow( IWin ).FractionUpgoing );
						FLFWSUdisk( JB + 1 ) = ZSU1 * TransBmBmMult( JB );
						FLCWSU( JB + 1 ) += ZSU1 * TransMult( JB ) * SurfaceWindow( IWin ).FractionUpgoing;
					} // End of loop over slat angles
				} // End of window with shade or blind
			} // COSBSun > 0
		} // SunlitFracHR > 0

		// Beam reaching window after specular reflection from exterior obstruction

		// In the following, Beam normal illuminance times ZSU1refl = illuminance on window due to
		// specular reflection from exterior surfaces

		if ( CalcSolRefl && SurfaceWindow( IWin ).OriginalClass != SurfaceClass_TDD_Dome ) {
			ZSU1refl = ReflFacBmToBmSolObs( IHR, IWin );

			if ( ZSU1refl > 0.0 ) {
				// Contribution to window luminance and downgoing flux

				// -- Bare window. We use diffuse-diffuse transmittance here rather than beam-beam to avoid
				//    complications due to specular reflection from multiple exterior surfaces

				TVisSunRefl = Construct( IConst ).TransDiffVis * SurfaceWindow( IWin ).GlazedFrac * SurfaceWindow( IWin ).LightWellEff;
				// In the following it is assumed that all reflected beam is going downward, as it would be in the
				// important case of reflection from a highly glazed facade of a neighboring building. However, in
				// rare cases (such as upward specular reflection from a flat horizontal skylight) it may
				// actually be going upward.
				FLFWSUdisk( 1 ) += ZSU1refl * TVisSunRefl;

				// -- Window with shade, blind or diffusing glass

				if ( ShadeOn || BlindOn || ScreenOn || SurfaceWindow( IWin ).SolarDiffusing ) {
					TransBmBmMult = 0.0;
					TransMult = 0.0;

					for ( JB = 1; JB <= MaxSlatAngs; ++JB ) {
						if ( ! SurfaceWindow( IWin ).MovableSlats && JB > 1 ) break;

						if ( ShadeOn || SurfaceWindow( IWin ).SolarDiffusing ) { // Shade on or diffusing glass
							TransMult( 1 ) = Construct( IConstShaded ).TransDiffVis * SurfaceWindow( IWin ).GlazedFrac * SurfaceWindow( IWin ).LightWellEff;

						} else if ( ScreenOn ) { // Exterior screen on
							TransScDiffDiffFront = SurfaceScreens( SurfaceWindow( IWin ).ScreenNumber ).DifDifTransVis;
							TransMult( 1 ) = TransScDiffDiffFront * ( Construct( IConst ).TransDiffVis / ( 1.0 - ReflGlDiffDiffFront * ReflScDiffDiffBack ) ) * SurfaceWindow( IWin ).GlazedFrac * SurfaceWindow( IWin ).LightWellEff;

						} else { // Blind on
							TransBlDiffDiffFront = Blind( BlNum ).VisFrontDiffDiffTrans( JB );
							if ( ShType == WSC_ST_InteriorBlind ) { // Interior blind
								ReflBlDiffDiffFront = Blind( BlNum ).VisFrontDiffDiffRefl( JB );
								TransMult( JB ) = TVisSunRefl * ( TransBlDiffDiffFront + ReflBlDiffDiffFront * ReflGlDiffDiffBack * TransBlDiffDiffFront / ( 1.0 - ReflBlDiffDiffFront * ReflGlDiffDiffBack ) );

							} else if ( ShType == WSC_ST_ExteriorBlind ) { // Exterior blind
								TransMult( JB ) = TransBlDiffDiffFront * ( Construct( IConst ).TransDiffVis / ( 1.0 - ReflGlDiffDiffFront * Blind( BlNum ).VisBackDiffDiffRefl( JB ) ) ) * SurfaceWindow( IWin ).GlazedFrac * SurfaceWindow( IWin ).LightWellEff;

							} else { // Between-glass blind
								t1 = Construct( IConst ).tBareVisDiff( 1 );
								tfshBd = Blind( BlNum ).VisFrontDiffDiffTrans( JB );
								rfshB = Blind( BlNum ).VisFrontDiffDiffRefl( JB );
								if ( Construct( IConst ).TotGlassLayers == 2 ) { // 2 glass layers
									TransMult( JB ) = t1 * ( tfshBd * ( 1.0 + rfd2 * rbshd ) + rfshB * rbd1 * tfshd ) * td2 * SurfaceWindow( IWin ).LightWellEff;
								} else { // 3 glass layers; blind between layers 2 and 3
									t2 = Construct( IConst ).tBareVisDiff( 2 );
									TransMult( JB ) = t1 * t2 * ( tfshBd * ( 1.0 + rfd3 * rbshd ) + rfshB * ( rbd2 * tfshd + td2 * rbd1 * td2 * tfshd ) ) * td3 * SurfaceWindow( IWin ).LightWellEff;
								}
							} // End of check of interior/exterior/between-glass blind
						} // ShadeOn/BlindOn

						WLUMSU( IHR, JB + 1 ) += ZSU1refl * TransMult( JB ) / Pi;
						FLFWSU( JB + 1 ) += ZSU1refl * TransMult( JB ) * ( 1.0 - SurfaceWindow( IWin ).FractionUpgoing );
						FLCWSU( JB + 1 ) += ZSU1refl * TransMult( JB ) * SurfaceWindow( IWin ).FractionUpgoing;
					} // End of loop over slat angles
				} // End of check if window has shade, blind or diffusing glass
			} // End of check if ZSU1refl > 0.0
		} // End of check if solar reflections are in effect

		// Sun-related portion of internally reflected illuminance

		for ( JSH = 1; JSH <= MaxSlatAngs + 1; ++JSH ) {
			if ( ! SurfaceWindow( IWin ).MovableSlats && JSH > 2 ) break;

			// Full area of window is used in following since effect of dividers on reducing
			// effective window transmittance already accounted for in calc of FLFWSU and FLCWSU
			// CR 7869 added effect of intervening interior windows on transmittance and
			// added inside surface area of adjacent zone
			EINTSU( IHR, JSH ) = ( FLFWSU( JSH ) * SurfaceWindow( IWin ).RhoFloorWall + FLCWSU( JSH ) * SurfaceWindow( IWin ).RhoCeilingWall ) * ( Surface( IWin ).Area / SurfaceWindow( IWin ).GlazedFrac ) / ( ZoneInsideSurfArea * ( 1.0 - ZoneDaylight( ZoneNum ).AveVisDiffReflect ) );

			EINTSUdisk( IHR, JSH ) = FLFWSUdisk( JSH ) * SurfaceWindow( IWin ).RhoFloorWall * ( Surface( IWin ).Area / SurfaceWindow( IWin ).GlazedFrac ) / ( ZoneInsideSurfArea * ( 1.0 - ZoneDaylight( ZoneNum ).AveVisDiffReflect ) );
		}

	}

	void
	ComplexFenestrationLuminances(
		int const IWin,
		int const WinEl,
		int const NBasis,
		int const IHR,
		int const iRefPoint,
		Array2< Real64 > & ElementLuminanceSky, // sky related luminance at window element (exterior side)
		Array1< Real64 > & ElementLuminanceSun, // sun related luminance at window element (exterior side),
		Array1< Real64 > & ElementLuminanceSunDisk, // sun related luminance at window element (exterior side),
		int const CalledFrom,
		Optional_int_const MapNum
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Simon Vidanovic
		//       DATE WRITTEN   June 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:

		// METHODOLOGY EMPLOYED: na

		// REFERENCES:

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// exluding beam
		// due to sun beam

		int iIncElem;
		int iSky;
		int SolBmIndex;
		Real64 LambdaInc;
		Real64 Altitude;
		Real64 Azimuth;
		//  REAL(r64) :: CurCplxFenState
		int CurCplxFenState;
		Real64 SunObstrMultiplier; // sun obstruction multiplier used to determine if sun hit the ground point
		Real64 ObstrTrans; // product of all surface transmittances intersecting incoming beam

		Real64 BeamObstrMultiplier; // beam obstruction multiplier in case incoming beam is from the ground
		int ObsSurfNum; // Obstruction surface number
		bool hitObs; // True iff obstruction is hit
		static Vector3< Real64 > ObsHitPt; // Coordinates of hit point on an obstruction (m)
		static Vector3< Real64 > GroundHitPt; // Coordinates of point that ray from window center hits the ground (m)

		int NRefl; // number of exterior obstructions
		int iReflElem; // incoming direction blocking surfaces element counter
		int iReflElemIndex; // reflection element index

		int NGnd; // number of ground elements
		int iGndElem; // ground elements counter
		int iGndElemIndex; // ground element index

		CurCplxFenState = SurfaceWindow( IWin ).ComplexFen.CurrentState;

		// Calculate luminance from sky and sun excluding exterior obstruction transmittances and obstruction multipliers
		SolBmIndex = ComplexWind( IWin ).Geom( CurCplxFenState ).SolBmIndex( IHR, TimeStep );
		for ( iIncElem = 1; iIncElem <= NBasis; ++iIncElem ) {
			LambdaInc = ComplexWind( IWin ).Geom( CurCplxFenState ).Inc.Lamda( iIncElem );
			//COSB = ComplexWind(IWin)%Geom(CurCplxFenState)%CosInc(iIncElem)
			//DA = ComplexWind(IWin)%Geom(CurCplxFenState)%DAInc(iIncElem)
			Altitude = ComplexWind( IWin ).Geom( CurCplxFenState ).pInc( iIncElem ).Altitude;
			Azimuth = ComplexWind( IWin ).Geom( CurCplxFenState ).pInc( iIncElem ).Azimuth;
			if ( Altitude > 0.0 ) {
				// Ray from sky element
				for ( iSky = 1; iSky <= 4; ++iSky ) {
					ElementLuminanceSky( iSky, iIncElem ) = DayltgSkyLuminance( iSky, Azimuth, Altitude ) * LambdaInc;
				}
			} else if ( Altitude < 0.0 ) {
				// Ray from ground element
				// BeamObstrMultiplier = ComplexWind(IWin)%DaylghtGeom(CurCplxFenState)%GndObstrMultiplier(WinEl, iIncElem)
				for ( iSky = 1; iSky <= 4; ++iSky ) {
					ElementLuminanceSky( iSky, iIncElem ) = GILSK( IHR, iSky ) * GndReflectanceForDayltg / Pi * LambdaInc;
				}
				ElementLuminanceSun( iIncElem ) = GILSU( IHR ) * GndReflectanceForDayltg / Pi * LambdaInc;
			} else {
				// Ray from the element which is half sky and half ground
				for ( iSky = 1; iSky <= 4; ++iSky ) {
					// in this case half of the pach is coming from the sky and half from the ground
					ElementLuminanceSky( iSky, iIncElem ) = 0.5 * DayltgSkyLuminance( iSky, Azimuth, Altitude ) * LambdaInc;
					ElementLuminanceSky( iSky, iIncElem ) += 0.5 * GILSK( IHR, iSky ) * GndReflectanceForDayltg / Pi * LambdaInc;
				}
				ElementLuminanceSun( iIncElem ) = 0.5 * GILSU( IHR ) * GndReflectanceForDayltg / Pi * LambdaInc;
			}
			// Sun beam calculations
			if ( ( SolBmIndex == iIncElem ) && ( SunlitFracHR( IHR, IWin ) > 0.0 ) ) {
				ElementLuminanceSunDisk( iIncElem ) = 1.0;
			}
		}

		// add exterior obstructions transmittances to calculated luminances
		if ( CalledFrom == CalledForRefPoint ) {
			NRefl = ComplexWind( IWin ).DaylghtGeom( CurCplxFenState ).RefPoint( iRefPoint ).NReflSurf( WinEl );
		} else {
			NRefl = ComplexWind( IWin ).DaylghtGeom( CurCplxFenState ).IlluminanceMap( iRefPoint, MapNum ).NReflSurf( WinEl );
		}
		for ( iReflElem = 1; iReflElem <= NRefl; ++iReflElem ) {
			if ( CalledFrom == CalledForRefPoint ) {
				ObstrTrans = ComplexWind( IWin ).DaylghtGeom( CurCplxFenState ).RefPoint( iRefPoint ).TransOutSurf( iReflElem, WinEl );
				iReflElemIndex = ComplexWind( IWin ).DaylghtGeom( CurCplxFenState ).RefPoint( iRefPoint ).RefSurfIndex( iReflElem, WinEl );
			} else {
				ObstrTrans = ComplexWind( IWin ).DaylghtGeom( CurCplxFenState ).IlluminanceMap( iRefPoint, MapNum ).TransOutSurf( iReflElem, WinEl );
				iReflElemIndex = ComplexWind( IWin ).DaylghtGeom( CurCplxFenState ).IlluminanceMap( iRefPoint, MapNum ).RefSurfIndex( iReflElem, WinEl );
			}

			for ( iSky = 1; iSky <= 4; ++iSky ) {
				ElementLuminanceSky( iSky, iReflElemIndex ) *= ObstrTrans;
			}
			ElementLuminanceSun( iReflElemIndex ) *= ObstrTrans;
			ElementLuminanceSunDisk( iReflElemIndex ) *= ObstrTrans;
		}

		// add exterior ground element obstruction multipliers to calculated luminances. For sun reflection, calculate if
		// sun reaches the ground for that point
		if ( CalledFrom == CalledForRefPoint ) {
			NGnd = ComplexWind( IWin ).DaylghtGeom( CurCplxFenState ).RefPoint( iRefPoint ).NGnd( WinEl );
		} else {
			NGnd = ComplexWind( IWin ).DaylghtGeom( CurCplxFenState ).IlluminanceMap( iRefPoint, MapNum ).NGnd( WinEl );
		}
		Vector3< Real64 > const SUNCOS_IHR( SUNCOSHR( IHR, {1,3} ) );
		for ( iGndElem = 1; iGndElem <= NGnd; ++iGndElem ) {
			// case for sky elements. Integration is done over upper ground hemisphere to determine how many obstructions
			// were hit in the process
			if ( CalledFrom == CalledForRefPoint ) {
				BeamObstrMultiplier = ComplexWind( IWin ).DaylghtGeom( CurCplxFenState ).RefPoint( iRefPoint ).GndObstrMultiplier( iGndElem, WinEl );
				iGndElemIndex = ComplexWind( IWin ).DaylghtGeom( CurCplxFenState ).RefPoint( iRefPoint ).GndIndex( iGndElem, WinEl );
			} else {
				BeamObstrMultiplier = ComplexWind( IWin ).DaylghtGeom( CurCplxFenState ).IlluminanceMap( iRefPoint, MapNum ).GndObstrMultiplier( iGndElem, WinEl );
				iGndElemIndex = ComplexWind( IWin ).DaylghtGeom( CurCplxFenState ).IlluminanceMap( iRefPoint, MapNum ).GndIndex( iGndElem, WinEl );
			}
			for ( iSky = 1; iSky <= 4; ++iSky ) {
				ElementLuminanceSky( iSky, iGndElemIndex ) *= BeamObstrMultiplier;
			}

			// direct sun disk reflect off the ground
			SunObstrMultiplier = 1.0;
			if ( CalcSolRefl ) {
				// Sun reaches ground point if vector from this point to the sun is unobstructed
				hitObs = false;
				for ( ObsSurfNum = 1; ObsSurfNum <= TotSurfaces; ++ObsSurfNum ) {
					if ( ! Surface( ObsSurfNum ).ShadowSurfPossibleObstruction ) continue;
					if ( CalledFrom == CalledForRefPoint ) {
						GroundHitPt( 1 ) = ComplexWind( IWin ).DaylghtGeom( CurCplxFenState ).RefPoint( iRefPoint ).GndPt( iGndElem, WinEl ).x;
						GroundHitPt( 2 ) = ComplexWind( IWin ).DaylghtGeom( CurCplxFenState ).RefPoint( iRefPoint ).GndPt( iGndElem, WinEl ).y;
						GroundHitPt( 3 ) = ComplexWind( IWin ).DaylghtGeom( CurCplxFenState ).RefPoint( iRefPoint ).GndPt( iGndElem, WinEl ).z;
					} else {
						GroundHitPt( 1 ) = ComplexWind( IWin ).DaylghtGeom( CurCplxFenState ).IlluminanceMap( iRefPoint, MapNum ).GndPt( iGndElem, WinEl ).x;
						GroundHitPt( 2 ) = ComplexWind( IWin ).DaylghtGeom( CurCplxFenState ).IlluminanceMap( iRefPoint, MapNum ).GndPt( iGndElem, WinEl ).y;
						GroundHitPt( 3 ) = ComplexWind( IWin ).DaylghtGeom( CurCplxFenState ).IlluminanceMap( iRefPoint, MapNum ).GndPt( iGndElem, WinEl ).z;
					}

					PierceSurface( ObsSurfNum, GroundHitPt, SUNCOS_IHR, ObsHitPt, hitObs );
					if ( hitObs ) break;
				}
				if ( hitObs ) SunObstrMultiplier = 0.0;
			}
			ElementLuminanceSun( iGndElemIndex ) *= SunObstrMultiplier;
		}

	}

	void
	DayltgInterReflectedIllumComplexFenestration(
		int const IWin, // Window index
		int const WinEl, // Current window element counter
		int const IHR, // Hour of day
		int const ZoneNum, // Zone number
		int const iRefPoint, // reference point counter
		int const CalledFrom,
		Optional_int_const MapNum
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Simon Vidanovic
		//       DATE WRITTEN   April 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Called from CalcDayltgCoefficients for each complex (bsdf) fenestration and reference point in a daylit
		// space, for each sun position. Calculates illuminance (EINTSK and EINTSU) at reference point due
		// to internally reflected light by integrating to determine the amount of flux from
		// sky and ground (and beam reflected from obstructions) transmitted through
		// the center of the window and then reflecting this
		// light from the inside surfaces of the space.

		// METHODOLOGY EMPLOYED: na

		// REFERENCES:

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		Array2D< Real64 > FLSK; // Sky related luminous flux
		Array1D< Real64 > FLSU; // Sun related luminous flux, excluding entering beam
		Array1D< Real64 > FLSUdisk; // Sun related luminous flux, due to entering beam

		Array2D< Real64 > FirstFluxSK; // Sky related first reflected flux
		Array1D< Real64 > FirstFluxSU; // Sun related first reflected flux, excluding entering beam
		Array1D< Real64 > FirstFluxSUdisk; // Sun related first reflected flux, due to entering beam

		Array2D< Real64 > ElementLuminanceSky; // sky related luminance at window element (exterior side)
		Array1D< Real64 > ElementLuminanceSun; // sun related luminance at window element (exterior side), exluding beam
		Array1D< Real64 > ElementLuminanceSunDisk; // sun related luminance at window element (exterior side), due to sun beam
		// Total transmitted flux
//		static Vector4< Real64 > FLSKTot; //Unused
		Real64 FLSUTot;
		Real64 FLSUdiskTot;

		// Total for first relflected fluxes
		static Vector4< Real64 > FFSKTot;
		Real64 FFSUTot;
		Real64 FFSUdiskTot;

		Real64 COSIncSun; // cosine of sun incidence angle (from basis elements)

		int iSky; // Sky type index: 1=clear, 2=clear turbid, 3=intermediate, 4=overcast
		int iConst; // Construction number

		int CurCplxFenState;
		int NIncBasis;
		int NTrnBasis;
		int SolBmIndex; // index of current sun position

		int iIncElem; // incoming direction counter
		int iBackElem; // outgoing direction counter

		Real64 LambdaInc; // current lambda value for incoming direction
		//REAL(r64) :: LambdaTrn  ! current lambda value for incoming direction
		Real64 dirTrans; // directional bsdf transmittance
		Real64 ZoneInsideSurfArea;

		CurCplxFenState = SurfaceWindow( IWin ).ComplexFen.CurrentState;
		iConst = SurfaceWindow( IWin ).ComplexFen.State( CurCplxFenState ).Konst;
		NTrnBasis = ComplexWind( IWin ).Geom( CurCplxFenState ).Trn.NBasis;

		if ( ! allocated( FLSK ) ) FLSK.allocate( 4, NTrnBasis );
		FLSK = 0.0;
		if ( ! allocated( FLSU ) ) FLSU.dimension( NTrnBasis, 0.0 );
		if ( ! allocated( FLSUdisk ) ) FLSUdisk.dimension( NTrnBasis, 0.0 );

		if ( ! allocated( FirstFluxSK ) ) FirstFluxSK.allocate( 4, NTrnBasis );
		FirstFluxSK = 0.0;
		if ( ! allocated( FirstFluxSU ) ) FirstFluxSU.dimension( NTrnBasis, 0.0 );
		if ( ! allocated( FirstFluxSUdisk ) ) FirstFluxSUdisk.dimension( NTrnBasis, 0.0 );

		NIncBasis = ComplexWind( IWin ).Geom( CurCplxFenState ).Inc.NBasis;
		if ( ! allocated( ElementLuminanceSky ) ) ElementLuminanceSky.allocate( 4, NIncBasis );
		ElementLuminanceSky = 0.0;
		if ( ! allocated( ElementLuminanceSun ) ) ElementLuminanceSun.dimension( NIncBasis, 0.0 );
		if ( ! allocated( ElementLuminanceSunDisk ) ) ElementLuminanceSunDisk.dimension( NIncBasis, 0.0 );

		// Integration over sky/ground/sun elements is done over window incoming basis element and flux is calculated for each
		// outgoing direction. This is used to calculate first reflected flux

		ComplexFenestrationLuminances( IWin, WinEl, NIncBasis, IHR, iRefPoint, ElementLuminanceSky, ElementLuminanceSun, ElementLuminanceSunDisk, CalledFrom, MapNum );

		// luminance from sun disk needs to include fraction of sunlit area
		SolBmIndex = ComplexWind( IWin ).Geom( CurCplxFenState ).SolBmIndex( IHR, TimeStep );
		if ( SolBmIndex > 0 ) {
			COSIncSun = ComplexWind( IWin ).Geom( CurCplxFenState ).CosInc( SolBmIndex );
		} else {
			COSIncSun = 0.0;
		}
		ElementLuminanceSunDisk *= SunlitFracHR( IHR, IWin ) * COSIncSun;

//		FLSKTot = 0.0;
		FLSUTot = 0.0;
		FLSUdiskTot = 0.0;
		FFSKTot = 0.0;
		FFSUTot = 0.0;
		FFSUdiskTot = 0.0;
		// now calculate flux into each outgoing direction by integrating over all incoming directions
		for ( iBackElem = 1; iBackElem <= NTrnBasis; ++iBackElem ) {
			for ( iIncElem = 1; iIncElem <= NIncBasis; ++iIncElem ) {
				LambdaInc = ComplexWind( IWin ).Geom( CurCplxFenState ).Inc.Lamda( iIncElem );
				dirTrans = Construct( iConst ).BSDFInput.VisFrtTrans( iBackElem, iIncElem );

				for ( iSky = 1; iSky <= 4; ++iSky ) {
					FLSK( iSky, iBackElem ) += dirTrans * LambdaInc * ElementLuminanceSky( iSky, iIncElem );
				}

				FLSU( iBackElem ) += dirTrans * LambdaInc * ElementLuminanceSun( iIncElem );
				FLSUdisk( iBackElem ) += dirTrans * LambdaInc * ElementLuminanceSunDisk( iIncElem );
			}

			for ( iSky = 1; iSky <= 4; ++iSky ) {
				FirstFluxSK( iSky, iBackElem ) = FLSK( iSky, iBackElem ) * ComplexWind( IWin ).Geom( CurCplxFenState ).AveRhoVisOverlap( iBackElem );
				FFSKTot( iSky ) += FirstFluxSK( iSky, iBackElem );
//				FLSKTot( iSky ) += FLSK( iSky, iBackElem );
			}
			FirstFluxSU( iBackElem ) = FLSU( iBackElem ) * ComplexWind( IWin ).Geom( CurCplxFenState ).AveRhoVisOverlap( iBackElem );
			FFSUTot += FirstFluxSU( iBackElem );
			FLSUTot += FLSU( iBackElem );

			FirstFluxSUdisk( iBackElem ) = FLSUdisk( iBackElem ) * ComplexWind( IWin ).Geom( CurCplxFenState ).AveRhoVisOverlap( iBackElem );
			FFSUdiskTot += FirstFluxSUdisk( iBackElem );
			FLSUdiskTot += FLSUdisk( iBackElem );
		}

		ZoneInsideSurfArea = ZoneDaylight( ZoneNum ).TotInsSurfArea;
		for ( iSky = 1; iSky <= 4; ++iSky ) {
			EINTSK( IHR, 1, iSky ) = FFSKTot( iSky ) * ( Surface( IWin ).Area / SurfaceWindow( IWin ).GlazedFrac ) / ( ZoneInsideSurfArea * ( 1.0 - ZoneDaylight( ZoneNum ).AveVisDiffReflect ) );
		}
		EINTSU( IHR, 1 ) = FFSUTot * ( Surface( IWin ).Area / SurfaceWindow( IWin ).GlazedFrac ) / ( ZoneInsideSurfArea * ( 1.0 - ZoneDaylight( ZoneNum ).AveVisDiffReflect ) );
		EINTSUdisk( IHR, 1 ) = FFSUdiskTot * ( Surface( IWin ).Area / SurfaceWindow( IWin ).GlazedFrac ) / ( ZoneInsideSurfArea * ( 1.0 - ZoneDaylight( ZoneNum ).AveVisDiffReflect ) );

		if ( allocated( FLSK ) ) FLSK.deallocate();
		if ( allocated( FLSU ) ) FLSU.deallocate();
		if ( allocated( FLSUdisk ) ) FLSUdisk.deallocate();

		if ( allocated( FirstFluxSK ) ) FirstFluxSK.deallocate();
		if ( allocated( FirstFluxSU ) ) FirstFluxSU.deallocate();
		if ( allocated( FirstFluxSUdisk ) ) FirstFluxSUdisk.deallocate();

		if ( allocated( ElementLuminanceSky ) ) ElementLuminanceSky.deallocate();
		if ( allocated( ElementLuminanceSun ) ) ElementLuminanceSun.deallocate();
		if ( allocated( ElementLuminanceSunDisk ) ) ElementLuminanceSunDisk.deallocate();

	}

	void
	DayltgDirectIllumComplexFenestration(
		int const IWin, // Window index
		int const WinEl, // Current window element counter
		int const IHR, // Hour of day
		int const EP_UNUSED( ZoneNum ), // Zone number
		int const iRefPoint, // reference point index
		int const CalledFrom,
		Optional_int_const MapNum
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Simon Vidanovic
		//       DATE WRITTEN   June 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:

		// METHODOLOGY EMPLOYED: na

		// REFERENCES:

		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		//  quantities that do not depend on sun position.

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// Luminances from different sources to the window
		Array2D< Real64 > ElementLuminanceSky; // sky related luminance at window element (exterior side)
		Array1D< Real64 > ElementLuminanceSun; // sun related luminance at window element (exterior side),
		// exluding beam
		Array1D< Real64 > ElementLuminanceSunDisk; // sun related luminance at window element (exterior side),
		// due to sun beam

		static Vector4< Real64 > WinLumSK; // Sky related window luminance
		Real64 WinLumSU; // Sun related window luminance, excluding entering beam
		//REAL(r64) :: WinLumSUdisk  ! Sun related window luminance, due to entering beam

		static Vector4< Real64 > EDirSky; // Sky related direct illuminance
		Real64 EDirSun; // Sun related direct illuminance, excluding entering beam
//		Real64 EDirSunDisk; // Sun related direct illuminance, due to entering beam //Unused Set but never used

		int CurCplxFenState;
		int NIncBasis;
		int RefPointIndex; // reference point patch number
		int iIncElem;
		int iConst;
		int iSky;

		// REAL(r64) :: LambdaInc ! lambda for incident direation
		Real64 dirTrans; // directional BSDF transmittance

		Real64 dOmega; // solid view angle of current element
		Real64 zProjection; // z-axe projection of solid view angle (used to calculate amount of light at horizontal surface
		// laying at reference point)

		CurCplxFenState = SurfaceWindow( IWin ).ComplexFen.CurrentState;
		iConst = SurfaceWindow( IWin ).ComplexFen.State( CurCplxFenState ).Konst;
		NIncBasis = ComplexWind( IWin ).Geom( CurCplxFenState ).Inc.NBasis;

		if ( ! allocated( ElementLuminanceSky ) ) ElementLuminanceSky.allocate( 4, NIncBasis );
		ElementLuminanceSky = 0.0;
		if ( ! allocated( ElementLuminanceSun ) ) ElementLuminanceSun.dimension( NIncBasis, 0.0 );
		if ( ! allocated( ElementLuminanceSunDisk ) ) ElementLuminanceSunDisk.dimension( NIncBasis, 0.0 );

		ComplexFenestrationLuminances( IWin, WinEl, NIncBasis, IHR, iRefPoint, ElementLuminanceSky, ElementLuminanceSun, ElementLuminanceSunDisk, CalledFrom, MapNum );

		// find number of outgoing basis towards current reference point
		if ( CalledFrom == CalledForRefPoint ) {
			RefPointIndex = ComplexWind( IWin ).DaylghtGeom( CurCplxFenState ).RefPoint( iRefPoint ).RefPointIndex( WinEl );
			dOmega = ComplexWind( IWin ).RefPoint( iRefPoint ).SolidAngle( WinEl );
			zProjection = ComplexWind( IWin ).RefPoint( iRefPoint ).SolidAngleVec( WinEl ).z;
		} else {
			RefPointIndex = ComplexWind( IWin ).DaylghtGeom( CurCplxFenState ).IlluminanceMap( iRefPoint, MapNum ).RefPointIndex( WinEl );
			dOmega = ComplexWind( IWin ).IlluminanceMap( iRefPoint, MapNum ).SolidAngle( WinEl );
			zProjection = ComplexWind( IWin ).IlluminanceMap( iRefPoint, MapNum ).SolidAngleVec( WinEl ).z;
		}

		WinLumSK = 0.0;
		WinLumSU = 0.0;
		//WinLumSUdisk = 0.0d0
		EDirSky = 0.0;
		EDirSun = 0.0;
//		EDirSunDisk = 0.0; //Unused Set but never used

		for ( iIncElem = 1; iIncElem <= NIncBasis; ++iIncElem ) {
			// LambdaInc = ComplexWind(IWin)%Geom(CurCplxFenState)%Inc%Lamda(iIncElem)
			dirTrans = Construct( iConst ).BSDFInput.VisFrtTrans( RefPointIndex, iIncElem );

			for ( iSky = 1; iSky <= 4; ++iSky ) {
				WinLumSK( iSky ) += dirTrans * ElementLuminanceSky( iSky, iIncElem );
			}

			WinLumSU += dirTrans * ElementLuminanceSun( iIncElem );

			// For sun disk need to go throug outgoing directions and see which directions actually contain reference point
			//if ((PosFac /= 0.0d0).and.(dOmega > 1e-6)) then
			//WinLumSUdisk = WinLumSUdisk + dirTrans * ElementLuminanceSunDisk(iIncElem) * 14700.0d0 * sqrt(0.000068d0*PosFac) / &
			//  (dOmega**0.8d0)
			//end if
		}

		if ( zProjection > 0.0 ) {
			for ( iSky = 1; iSky <= 4; ++iSky ) {
				EDirSky( iSky ) = WinLumSK( iSky ) * dOmega * zProjection;
			}
			EDirSun = WinLumSU * dOmega * zProjection;
		}

		// Store solution in global variables
		for ( iSky = 1; iSky <= 4; ++iSky ) {
			AVWLSK( IHR, 1, iSky ) += WinLumSK( iSky );
			EDIRSK( IHR, 1, iSky ) += EDirSky( iSky );
		}

		AVWLSU( IHR, 1 ) += WinLumSU;
		EDIRSU( IHR, 1 ) += EDirSun;
		//AVWLSUdisk(1,IHR) = AVWLSUdisk(1,IHR) + WinLumSUdisk

	}

	void
	DayltgDirectSunDiskComplexFenestration(
		int const iWin, // Window index
		int const EP_UNUSED( ZoneNum ), // Zone number
		int const iHour, // Hour of day
		int const iRefPoint,
		int const NumEl, // Total number of window elements
		Real64 const AZVIEW, // Azimuth of view vector in absolute coord system for
		int const CalledFrom, // indicate  which type of routine called this routine
		Optional_int_const MapNum,
		Optional< Real64 const > MapWindowSolidAngAtRefPtWtd
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Simon Vidanovic
		//       DATE WRITTEN   June 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculate illuminance from sun disk for complex fenestration systems

		// METHODOLOGY EMPLOYED: na

		// REFERENCES:

		// USE STATEMENTS:

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		//  glare calculation (radians)

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int CurCplxFenState;
		int iConst;
		int SolBmIndex;
		int NTrnBasis;
		int iTrnElem;
		Real64 WindowSolidAngleDaylightPoint( 0.0 );
		Real64 XR;
		Real64 YR;
		Real64 PosFac;
		Real64 dirTrans;
		Real64 LambdaTrn;
		Real64 WinLumSunDisk; // window luminance from sun disk
		Real64 ELumSunDisk; // window illuminance from sun disk
		Real64 TransBeam; // transmittance of the beam for given direction
		static Vector3< Real64 > V; // temporary vector
		static Vector3< Real64 > RWin; // Window center
		Real64 RayZ; // z component of unit vector for outgoing direction
		bool refPointIntersect;

		CurCplxFenState = SurfaceWindow( iWin ).ComplexFen.CurrentState;
		iConst = SurfaceWindow( iWin ).ComplexFen.State( CurCplxFenState ).Konst;
		SolBmIndex = ComplexWind( iWin ).Geom( CurCplxFenState ).SolBmIndex( iHour, TimeStep );

		{ auto const SELECT_CASE_var( CalledFrom );
		if ( SELECT_CASE_var == CalledForRefPoint ) {
			WindowSolidAngleDaylightPoint = SurfaceWindow( iWin ).SolidAngAtRefPtWtd( iRefPoint );
		} else if ( SELECT_CASE_var == CalledForMapPoint ) {
			WindowSolidAngleDaylightPoint = MapWindowSolidAngAtRefPtWtd;
		} else {
			assert( false ); // Bad CalledFrom argument
		}}

		if ( WindowSolidAngleDaylightPoint < 1e-6 ) return;

		WinLumSunDisk = 0.0;
		ELumSunDisk = 0.0;
		NTrnBasis = ComplexWind( iWin ).Geom( CurCplxFenState ).Trn.NBasis;
		for ( iTrnElem = 1; iTrnElem <= NTrnBasis; ++iTrnElem ) {
			// if ray from any part of the window can reach reference point
			if ( CalledFrom == CalledForRefPoint ) {
				refPointIntersect = ComplexWind( iWin ).DaylghtGeom( CurCplxFenState ).RefPoint( iRefPoint ).RefPointIntersection( iTrnElem );
			} else {
				refPointIntersect = ComplexWind( iWin ).DaylghtGeom( CurCplxFenState ).IlluminanceMap( iRefPoint, MapNum ).RefPointIntersection( iTrnElem );
			}
			if ( refPointIntersect ) {
				if ( CalledFrom == CalledForRefPoint ) {
					PosFac = ComplexWind( iWin ).DaylghtGeom( CurCplxFenState ).RefPoint( iRefPoint ).RefPtIntPosFac( iTrnElem );
				} else {
					PosFac = ComplexWind( iWin ).DaylghtGeom( CurCplxFenState ).IlluminanceMap( iRefPoint, MapNum ).RefPtIntPosFac( iTrnElem );
				}
				RayZ = - ComplexWind( iWin ).Geom( CurCplxFenState ).sTrn( iTrnElem ).z;

				// Need to recalculate position factor for dominant direction in case of specular bsdf.  Otherwise this will produce
				// very inaccurate results because of position factor of the sun and bsdf pach can vary by lot
				if ( iTrnElem == SolBmIndex ) {
					XR = std::tan( std::abs( PiOvr2 - AZVIEW - THSUN ) + 0.001 );
					YR = std::tan( PHSUN + 0.001 );
					PosFac = DayltgGlarePositionFactor( XR, YR );
					RayZ = SPHSUN;
				}

				if ( PosFac != 0.0 ) {
					if ( SolBmIndex > 0 ) {
						dirTrans = Construct( iConst ).BSDFInput.VisFrtTrans( iTrnElem, SolBmIndex );
					} else {
						dirTrans = 0.0;
					}
					LambdaTrn = ComplexWind( iWin ).Geom( CurCplxFenState ).Trn.Lamda( iTrnElem );

					V( 1 ) = ComplexWind( iWin ).Geom( CurCplxFenState ).sTrn( iTrnElem ).x;
					V( 2 ) = ComplexWind( iWin ).Geom( CurCplxFenState ).sTrn( iTrnElem ).y;
					V( 3 ) = ComplexWind( iWin ).Geom( CurCplxFenState ).sTrn( iTrnElem ).z;
					V = -V;

					RWin( 1 ) = Surface( iWin ).Centroid.x;
					RWin( 2 ) = Surface( iWin ).Centroid.y;
					RWin( 3 ) = Surface( iWin ).Centroid.z;

					DayltgHitObstruction( iHour, iWin, RWin, V, TransBeam );

					WinLumSunDisk += ( 14700.0 * std::sqrt( 0.000068 * PosFac ) * double( NumEl ) / std::pow( WindowSolidAngleDaylightPoint, 0.8 ) ) * dirTrans * LambdaTrn * TransBeam;

					ELumSunDisk += RayZ * dirTrans * LambdaTrn * TransBeam;
				}
			}
		}

		AVWLSUdisk( iHour, 1 ) = WinLumSunDisk;
		EDIRSUdisk( iHour, 1 ) = ELumSunDisk;

	}

	Real64
	DayltgSkyLuminance(
		int const ISky, // Sky type: 1=clear, 2=clear turbid, 3=intermediate, 4=overcast
		Real64 const THSKY, // Azimuth and altitude of sky element (radians)
		Real64 const PHSKY
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   July 1997
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Called by CalcDayltgCoefficients, DayltgExtHorizIllum AND DayltgInterReflectedIllum.  gives
		// luminance in cd/m2 for four different sky types, as described in R.Perez, P.Ineichen,
		// R.Seals, J.Michalsky and R.Stewart, "Modeling daylight availability and irradiance
		// components from direct and global irradiance," Solar Energy 44, 1990, 271-289.
		// The luminance distributions in this routine are normalized such that
		// the zenith luminance is 1.0, i.e., DayltgSkyLuminance =
		// (sky luminance at THSKY, PHSKY)/(zenith luminance), which is dimensionless.
		// The sky types are:
		// 1. Standard CIE clear sky
		// 2. Standard CIE high-turbidity clear sky
		// 3. CIE intermediate sky
		// 4. CIE overcast sky

		// METHODOLOGY EMPLOYED:

		// REFERENCES:
		// Based on DOE-2.1E subroutine DSKYLU, which did only clear and overcast skies.

		// OTHER NOTES:
		// THSKY ranges from 0 to 2Pi starting with 0 directly East and rotating clockwise.
		// PHSKY ranges from 0 to Pi starting with 0 at the horizon and Pi/2 at the zenith.

		// USE STATEMENTS: na

		// Return value
		Real64 DayltgSkyLuminance( 0.0 ); // Luminance of sky element divided by zenith luminance

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 SPHSKY; // Sine of PHSKY
		Real64 G( 0.0 ); // Angle between sun and element of sky (radians)
		Real64 COSG( 0.0 ); // Cosine of G
		Real64 Z; // Solar zenith angle (radians)
		Real64 Z1; // Luminance factors (intermediate variables)
		Real64 Z2;
		Real64 Z3;
		Real64 Z4;

		//Autodesk:Return ISky in {1,2,3,4} should be checked or asserted or a default case should be added to SELECT block to set return value

		// FLOW:
		SPHSKY = max( std::sin( PHSKY ), 0.01 ); // Prevent floating point underflows
		Z = PiOvr2 - PHSUN;
		if ( ISky >= 1 && ISky <= 3 ) { // Following not needed for overcast sky
			COSG = SPHSKY * SPHSUN + std::cos( PHSKY ) * CPHSUN * std::cos( THSKY - THSUN );
			COSG = max( constant_minusone, min( COSG, 1.0 ) ); // Prevent out of range due to roundoff
			G = std::acos( COSG );
		}

		if ( ISky == 1 ) { // Clear Sky
			Z1 = 0.910 + 10.0 * std::exp( -3.0 * G ) + 0.45 * COSG * COSG;
			Z2 = 1.0 - std::exp( -0.32 / SPHSKY );
			Z3 = 0.27385 * ( 0.91 + 10.0 * std::exp( -3.0 * Z ) + 0.45 * SPHSUN * SPHSUN );
			DayltgSkyLuminance = Z1 * Z2 / Z3;

		} else if ( ISky == 2 ) { // Clear turbid sky
			Z1 = 0.856 + 16.0 * std::exp( -3.0 * G ) + 0.3 * COSG * COSG;
			Z2 = 1.0 - std::exp( -0.32 / SPHSKY );
			Z3 = 0.27385 * ( 0.856 + 16.0 * std::exp( -3.0 * Z ) + 0.3 * SPHSUN * SPHSUN );
			DayltgSkyLuminance = Z1 * Z2 / Z3;

		} else if ( ISky == 3 ) { // Intermediate sky
			Z1 = ( 1.35 * ( std::sin( 3.59 * PHSKY - 0.009 ) + 2.31 ) * std::sin( 2.6 * PHSUN + 0.316 ) + PHSKY + 4.799 ) / 2.326;
			Z2 = std::exp( -G * 0.563 * ( ( PHSUN - 0.008 ) * ( PHSKY + 1.059 ) + 0.812 ) );
			Z3 = 0.99224 * std::sin( 2.6 * PHSUN + 0.316 ) + 2.73852;
			Z4 = std::exp( -Z * 0.563 * ( ( PHSUN - 0.008 ) * 2.6298 + 0.812 ) );
			DayltgSkyLuminance = Z1 * Z2 / ( Z3 * Z4 );

		} else if ( ISky == 4 ) { // Overcast sky
			DayltgSkyLuminance = ( 1.0 + 2.0 * SPHSKY ) / 3.0;

		}

		return DayltgSkyLuminance;

	}

	void
	ProfileAngle(
		int const SurfNum, // Surface number
		Vector3< Real64 > const & CosDirSun, // Solar direction cosines
		int const HorOrVert, // If HORIZONTAL, calculates ProfileAngHor
		Real64 & ProfileAng // Solar profile angle (radians).
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   May 2001
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates profile angle for a surface.

		// REFERENCES: na
		// USE STATEMENTS:

		// Using/Aliasing
		using namespace DataGlobals;
		using namespace DataSurfaces;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// For HorOrVert = HORIZONTAL,
		//  this is the incidence angle in a plane that is normal to the window
		//  and parallel to the Y-axis of the window (the axis along
		//  which the height of the window is measured).
		//  For HorOrVert = VERTICAL,
		//  this is the incidence angle in a plane that is normal to the window
		//  and parallel to the X-axis of the window (the axis along
		//  which the width of the window is measured).
		// If VERTICAL, calculates ProfileAngVert

		// SUBROUTINE PARAMETER DEFINITIONS: na
		// INTERFACE BLOCK SPECIFICATIONS: na
		// DERIVED TYPE DEFINITIONS: na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 ElevSun; // Sun elevation; angle between sun and horizontal (radians)
		Real64 ElevWin; // Window elevation: angle between window outward normal and horizontal (radians)
		Real64 AzimWin; // Window azimuth (radians)
		Real64 AzimSun; // Sun azimuth (radians)
		static Vector3< Real64 > WinNorm; // Window outward normal unit vector
		Real64 ThWin; // Azimuth angle of WinNorm
		static Vector3< Real64 > SunPrime; // Projection of sun vector onto plane (perpendicular to
		//  window plane) determined by WinNorm and vector along
		//  baseline of window
		static Vector3< Real64 > WinNormCrossBase; // Cross product of WinNorm and vector along window baseline
		//  INTEGER            :: IComp             ! Vector component index

		// FLOW:
		if ( HorOrVert == Horizontal ) { // Profile angle for horizontal structures
			ElevWin = PiOvr2 - Surface( SurfNum ).Tilt * DegToRadians;
			AzimWin = ( 90.0 - Surface( SurfNum ).Azimuth ) * DegToRadians;
			ElevSun = std::asin( CosDirSun( 3 ) );
			AzimSun = std::atan2( CosDirSun( 2 ), CosDirSun( 1 ) );
			ProfileAng = std::atan( std::sin( ElevSun ) / std::abs( std::cos( ElevSun ) * std::cos( AzimWin - AzimSun ) ) ) - ElevWin;
		} else { // Profile angle for vertical structures
			ElevWin = PiOvr2 - Surface( SurfNum ).Tilt * DegToRadians;
			AzimWin = Surface( SurfNum ).Azimuth * DegToRadians; // 7952
			AzimSun = std::atan2( CosDirSun( 1 ), CosDirSun( 2 ) ); // 7952
			if ( std::abs( ElevWin ) < 0.1 ) { // Near-vertical window
				ProfileAng = AzimWin - AzimSun; //CR7952 allow sign changes.
			} else {
				WinNorm = Surface( SurfNum ).OutNormVec;
				ThWin = AzimWin - PiOvr2;
				Real64 const sin_ElevWin( std::sin( ElevWin ) );
				WinNormCrossBase( 1 ) = -sin_ElevWin * std::cos( ThWin );
				WinNormCrossBase( 2 ) = sin_ElevWin * std::sin( ThWin );
				WinNormCrossBase( 3 ) = std::cos( ElevWin );
				SunPrime = CosDirSun - WinNormCrossBase * dot( CosDirSun, WinNormCrossBase );
				ProfileAng = std::abs( std::acos( dot( WinNorm, SunPrime ) / SunPrime.magnitude() ) );
				//CR7952 correct sign of result for vertical slats
				if ( ( AzimWin - AzimSun ) < 0.0 ) ProfileAng = -1.0 * ProfileAng;
			}
			// Constrain to 0 to pi
			if ( ProfileAng > Pi ) ProfileAng = 2.0 * Pi - ProfileAng;
		}

	}

	void
	DayltgClosestObstruction(
		Vector3< Real64 > const & RecPt, // Point on window from which ray emanates (m)
		Vector3< Real64 > const & RayVec, // Unit vector along ray pointing away from window (m)
		int & NearestHitSurfNum, // Surface number of nearest obstruction that is hit by ray;
		Vector3< Real64 > & NearestHitPt // Ray's hit point on nearest obstruction (m)
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   November 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Determines surface number and hit point of closest exterior obstruction hit
		// by a ray from a window. If no obstruction is hit, NearestHitSurfNum = 0.

		// METHODOLOGY EMPLOYED: na

		// REFERENCES: na

		// USE STATEMENTS: na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		//  = 0 if no obstruction is hit.

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na
		static Vector3< Real64 > HitPt; // Hit point on an obstruction (m)
		bool hit; // True iff obstruction is hit

		// FLOW:

		NearestHitSurfNum = 0;
		Real64 NearestHitDistance_sq( std::numeric_limits< Real64 >::max() ); // Distance squared from receiving point to nearest hit point for a ray (m^2)
		NearestHitPt = 0.0;
		if ( TotSurfaces < octreeCrossover ) { // Linear search through surfaces

			for ( int ObsSurfNum = 1; ObsSurfNum <= TotSurfaces; ++ObsSurfNum ) {
				if ( Surface( ObsSurfNum ).ShadowSurfPossibleObstruction ) {
					// Determine if this ray hits the surface and, if so, get the distance from the receiving point to the hit
					PierceSurface( ObsSurfNum, RecPt, RayVec, HitPt, hit );
					if ( hit ) { // Ray pierces surface
						// If obstruction is a window and its base surface is the nearest obstruction hit so far set nearestHitSurface to this window
						// Note that in this case NearestHitDistance_sq has already been calculated, so does not have to be recalculated
						if ( ( Surface( ObsSurfNum ).Class == SurfaceClass_Window ) && ( Surface( ObsSurfNum ).BaseSurf == NearestHitSurfNum ) ) {
							NearestHitSurfNum = ObsSurfNum;
						} else {
							// Distance squared from receiving point to hit point
							Real64 const HitDistance_sq( distance_squared( HitPt, RecPt ) );
							// Reset NearestHitSurfNum and NearestHitDistance_sq if this hit point is closer than previous closest
							if ( HitDistance_sq < NearestHitDistance_sq ) {
								NearestHitDistance_sq = HitDistance_sq;
								NearestHitSurfNum = ObsSurfNum;
								NearestHitPt = HitPt;
							}
						}
					} // End of check if obstruction was hit
				}
			} // End of loop over possible obstructions for this ray

		} else { // Surface octree search

			SurfaceData const * nearestHitSurface( nullptr );

			// Lambda function for the octree to test for surface hit
			auto surfaceHit = [=,&RecPt,&RayVec,&hit,&NearestHitDistance_sq,&nearestHitSurface,&NearestHitPt]( SurfaceData const & surface ) {
				if ( surface.ShadowSurfPossibleObstruction ) {
					// Determine if this ray hits the surface and, if so, get the distance from the receiving point to the hit
					PierceSurface( surface, RecPt, RayVec, HitPt, hit ); // Check if ray pierces surface
					if ( hit ) { // Ray pierces surface
						// If obstruction is a window and its base surface is the nearest obstruction hit so far set nearestHitSurface to this window
						// Note that in this case NearestHitDistance_sq has already been calculated, so does not have to be recalculated
						if ( ( surface.Class == SurfaceClass_Window ) && ( surface.BaseSurf > 0 ) && ( &Surface( surface.BaseSurf ) == nearestHitSurface ) ) {
							nearestHitSurface = &surface;
						} else {
							// Distance squared from receiving point to hit point
							Real64 const HitDistance_sq( distance_squared( HitPt, RecPt ) );
							// Reset nearestHitSurface and NearestHitDistance_sq if this hit point is closer than previous closest
							if ( HitDistance_sq < NearestHitDistance_sq ) {
								NearestHitDistance_sq = HitDistance_sq;
								nearestHitSurface = &surface;
								NearestHitPt = HitPt;
							}
						}
					} // End of check if obstruction was hit
				}
			};

			// Process octree surface candidates
			Vector3< Real64 > const RayVec_inv( SurfaceOctreeCube::safe_inverse( RayVec ) );
			surfaceOctree.processSurfaceRayIntersectsCube( RecPt, RayVec, RayVec_inv, surfaceHit );
			if ( nearestHitSurface != nullptr ) { // Find surface number: This is inefficient: Improve when surfaces know their own number
				for ( int i = 1; i <= TotSurfaces; ++i ) {
					if ( &Surface( i ) == nearestHitSurface ) {
						NearestHitSurfNum = i;
						break;
					}
				}
				assert( NearestHitSurfNum != 0 );
			}

		}

	}

	void
	DayltgSurfaceLumFromSun(
		int const IHR, // Hour number
		Vector3< Real64 > const & Ray, // Ray from window to reflecting surface (m)
		int const ReflSurfNum, // Number of surface for which luminance is being calculated
		Vector3< Real64 > const & ReflHitPt, // Point on ReflSurfNum for luminance calculation (m)
		Real64 & LumAtReflHitPtFrSun // Luminance at ReflHitPt from beam solar reflection for unit
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   November 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates exterior surface luminance due to beam solar diffuse reflection.

		// METHODOLOGY EMPLOYED: na

		// REFERENCES: na

		// USE STATEMENTS: na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		//  beam normal illuminance (cd/m2)

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static Vector3< Real64 > ReflNorm; // Unit normal to reflecting surface (m)
		int ObsSurfNum; // Obstruction surface number
		bool hitObs; // True iff obstruction is hit
		static Vector3< Real64 > ObsHitPt; // Hit point on obstruction (m)
		Real64 CosIncAngAtHitPt; // Cosine of angle of incidence of sun at HitPt
		Real64 DiffVisRefl; // Diffuse visible reflectance of ReflSurfNum

		// FLOW:

		LumAtReflHitPtFrSun = 0.0;
		// Skip daylighting shelves since reflection from these is separately calculated
		if ( Surface( ReflSurfNum ).Shelf > 0 ) return;
		// Normal to reflecting surface in hemisphere containing window element
		ReflNorm = Surface( ReflSurfNum ).OutNormVec;
		if ( Surface( ReflSurfNum ).ShadowingSurf ) {
			if ( dot( ReflNorm, Ray ) > 0.0 ) {
				ReflNorm *= -1.0;
			}
		}
		// Cosine of angle of incidence of sun at HitPt if sun were to reach HitPt
		Vector3< Real64 > const SUNCOS_IHR( SUNCOSHR( IHR, {1,3} ) );
		CosIncAngAtHitPt = dot( ReflNorm, SUNCOS_IHR );
		// Require that the sun be in front of this surface relative to window element
		if ( CosIncAngAtHitPt <= 0.0 ) return; // Sun is in back of reflecting surface
		// Sun reaches ReflHitPt if vector from ReflHitPt to sun is unobstructed
		hitObs = false;
		for ( ObsSurfNum = 1; ObsSurfNum <= TotSurfaces; ++ObsSurfNum ) {
			if ( ! Surface( ObsSurfNum ).ShadowSurfPossibleObstruction ) continue;
			// Exclude as a possible obstructor ReflSurfNum and its base surface (if it has one)
			if ( ObsSurfNum == ReflSurfNum || ObsSurfNum == Surface( ReflSurfNum ).BaseSurf ) continue;
			PierceSurface( ObsSurfNum, ReflHitPt, SUNCOS_IHR, ObsHitPt, hitObs );
			if ( hitObs ) break;
		}
		if ( hitObs ) return; // Obstruction was hit, blocking sun
		// Obstruction was not hit; sun reaches ReflHitPt.
		// Calculate luminance at ReflHitPt due to beam solar reflection (for unit beam normal illuminance)
		if ( Surface( ReflSurfNum ).ShadowingSurf ) {
			DiffVisRefl = Surface( ReflSurfNum ).ShadowSurfDiffuseVisRefl;
			// Note that if the shadowing surface has a non-zero glazing fraction (e.g., neighboring bldg) that the above is
			// (1 - glazing fraction) * (vis refl of opaque part of shadowing surface); specular reflection is
			// excluded in this value of DiffVisRefl.
		} else { // Exterior building surface
			if ( ! Construct( Surface( ReflSurfNum ).Construction ).TypeIsWindow ) {
				DiffVisRefl = 1.0 - Construct( Surface( ReflSurfNum ).Construction ).OutsideAbsorpSolar;
			} else {
				// Window; assume bare so no beam-to-diffuse reflection
				DiffVisRefl = 0.0;
			}
		}
		LumAtReflHitPtFrSun = CosIncAngAtHitPt * DiffVisRefl / Pi;

	}

	void
	DayltgInteriorMapIllum( int & ZoneNum ) // Zone number
	{

		// *****super modified version of DayltgInteriorIllum by Peter Graham Ellis
		// *****removes all control code, just calculates illum and glare with previously determined control settings
		// *****this should be packaged into a subroutine called from 2 places

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   July 1997
		//       MODIFIED       March 2000, FW: interpolate clear-sky daylight factors using
		//                      HourOfDay/WeightNow and NextHour/WeightNextHour. Previously
		//                      only HourOfDay was used
		//                      Jan 2001, FW: interpolate in slat angle for windows with blinds
		//                      that have movable slats
		//                      Dec 2003, FW: fix bug--even though between-glass shade/blind is on
		//                        daylight illum at ref pt was calculated as though it was off
		//                      June 2009, TH: modified for thermochromic windows
		//                      March 2010, TH: fix bug (CR 8057) for electrochromic windows
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Using daylighting factors and exterior illuminance, determine
		// the current-hour interior daylight illuminance and glare index
		// at each reference point in a space.

		// Called by InitSurfaceHeatBalance.

		// METHODOLOGY EMPLOYED:na

		// REFERENCES:
		// Based on DOE-2.1E subroutine DINTIL.

		// Using/Aliasing
		using General::POLYF;
		using General::InterpSlatAng;

		// Locals
		static Array1D< Real64 > daylight_illum;

		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NREFPT; // Number of daylighting map reference points
		//INTEGER   :: REFPT1                ! 1st reference point
		int ISky; // Sky type index
		int ISky1; // Sky type index values for averaging two sky types
		int ISky2;
		static Array2D< Real64 > DFSKHR( 2, 4 ); // Sky daylight factor for sky type (first index),
		//   bare/shaded window (second index)
		static Vector2< Real64 > DFSUHR; // Sun daylight factor for bare/shaded window
		static Array2D< Real64 > BFSKHR( 2, 4 ); // Sky background luminance factor for sky type (first index),
		//   bare/shaded window (second index)
		static Vector2< Real64 > BFSUHR; // Sun background luminance factor for bare/shaded window
		static Array2D< Real64 > SFSKHR( 2, 4 ); // Sky source luminance factor for sky type (first index),
		//   bare/shaded window (second index)
		static Vector2< Real64 > SFSUHR; // Sun source luminance factor for bare/shaded window
		int IL; // Reference point index
		int IWin; // Window index
		int IS; // IS=1 for unshaded window, =2 for shaded window
		//INTEGER   :: ISWFLG                ! Switchable glazing flag: =1 if one or more windows in a zone
		//                                   !  has switchable glazing that adjusts visible transmittance to just meet
		//                                   !  daylighting setpoint; =0 otherwise.
		int ICtrl; // Window shading control pointer
		Real64 SkyWeight; // Weighting factor used to average two different sky types
		static Vector4< Real64 > HorIllSky; // Horizontal illuminance for different sky types
		Real64 HorIllSkyFac; // Ratio between horizontal illuminance from sky horizontal irradiance and
		//   luminous efficacy and horizontal illuminance from averaged sky
		Real64 SlatAng; // Blind slat angle (rad)
		bool VarSlats; // True if slats are movable, i.e., variable angle
		int loop; // Window loop index
		Real64 GTOT;
		Real64 GTOT1;
		Real64 GTOT2;
		static Array1D< Real64 > BACLUM;
		static Array1D< Real64 > GLRNDX;
		static bool FirstTimeFlag( true );
		int ILB;

		int IConst;
		Real64 VTRatio;
		Real64 VTNow;
		Real64 VTMaster;

		static Real64 VTDark( 0.0 ); // Visible transmittance (VT) of electrochromic (EC) windows in fully dark state
		static Real64 VTMULT( 1.0 ); // VT multiplier for EC windows
		static int IConstShaded( 0 ); // The shaded window construction for switchable windows
		int MapNum;
		int ILM;

		if ( FirstTimeFlag ) {
			daylight_illum.allocate( MaxMapRefPoints );
			BACLUM.allocate( MaxMapRefPoints );
			GLRNDX.allocate( MaxMapRefPoints );
			FirstTimeFlag = false;
		}

		if ( WarmupFlag ) return;
		//              Initialize reference point illuminance and window background luminance

		for ( ILM = 1; ILM <= ZoneDaylight( ZoneNum ).MapCount; ++ILM ) {

			MapNum = ZoneDaylight( ZoneNum ).ZoneToMap( ILM );
			//    IllumMapCalc(MapNum)%DaylIllumAtMapPt  = 0.0
			//    IllumMapCalc(MapNum)%GlareIndexAtMapPt = 0.0
			NREFPT = IllumMapCalc( MapNum ).TotalMapRefPoints;

			daylight_illum = 0.0;
			BACLUM = 0.0;
			GLRNDX = 0.0;

			if ( SkyClearness > 3.0 ) { //Sky is average of clear and clear turbid
				SkyWeight = min( 1.0, ( SkyClearness - 3.0 ) / 3.0 );
				ISky1 = 1;
				ISky2 = 2;
			} else if ( SkyClearness > 1.2 ) { //Sky is average of clear turbid and intermediate
				SkyWeight = ( SkyClearness - 1.2 ) / 1.8;
				ISky1 = 2;
				ISky2 = 3;
			} else { //Sky is average of intermediate and overcast
				SkyWeight = min( 1.0, max( 0.0, ( SkyClearness - 1.0 ) / 0.2, ( SkyBrightness - 0.05 ) / 0.4 ) );
				ISky1 = 3;
				ISky2 = 4;
			}

			//              First loop over windows in this space.
			//              Find contribution of each window to the daylight illum
			//              and to the glare numerator at each reference point.
			//              Use shading flags set in WindowShadingManager.

			for ( loop = 1; loop <= ZoneDaylight( ZoneNum ).NumOfDayltgExtWins; ++loop ) {
				IWin = ZoneDaylight( ZoneNum ).DayltgExtWinSurfNums( loop );

				// Added TH 6/29/2009 for thermochromic windows
				VTRatio = 1.0;
				if ( NREFPT > 0 ) {
					IConst = Surface( IWin ).Construction;
					if ( Construct( IConst ).TCFlag == 1 ) {
						// For thermochromic windows, daylight and glare factors are always calculated
						//  based on the master construction. They need to be adjusted by the VTRatio, including:
						//  ZoneDaylight()%DaylIllFacSky, DaylIllFacSun, DaylIllFacSunDisk; DaylBackFacSky,
						//  DaylBackFacSun, DaylBackFacSunDisk, DaylSourceFacSky, DaylSourceFacSun, DaylSourceFacSunDisk
						VTNow = POLYF( 1.0, Construct( IConst ).TransVisBeamCoef );
						VTMaster = POLYF( 1.0, Construct( Construct( IConst ).TCMasterConst ).TransVisBeamCoef );
						VTRatio = VTNow / VTMaster;
					}
				}

				//              Loop over reference points
				for ( ILB = 1; ILB <= NREFPT; ++ILB ) {

					//          Daylight factors for current sun position
					for ( ISky = 1; ISky <= 4; ++ISky ) {
						//                                ===Bare window===
						DFSKHR( 1, ISky ) = VTRatio * ( WeightNow * IllumMapCalc( MapNum ).DaylIllFacSky( HourOfDay, 1, ISky, ILB, loop ) + WeightPreviousHour * IllumMapCalc( MapNum ).DaylIllFacSky( PreviousHour, 1, ISky, ILB, loop ) );

						if ( ISky == 1 ) {
							DFSUHR( 1 ) = VTRatio * ( WeightNow * ( IllumMapCalc( MapNum ).DaylIllFacSun( HourOfDay, 1, ILB, loop ) + IllumMapCalc( MapNum ).DaylIllFacSunDisk( HourOfDay, 1, ILB, loop ) ) + WeightPreviousHour * ( IllumMapCalc( MapNum ).DaylIllFacSun( PreviousHour, 1, ILB, loop ) + IllumMapCalc( MapNum ).DaylIllFacSunDisk( PreviousHour, 1, ILB, loop ) ) );
						}

						BFSKHR( 1, ISky ) = VTRatio * ( WeightNow * IllumMapCalc( MapNum ).DaylBackFacSky( HourOfDay, 1, ISky, ILB, loop ) + WeightPreviousHour * IllumMapCalc( MapNum ).DaylBackFacSky( PreviousHour, 1, ISky, ILB, loop ) );

						if ( ISky == 1 ) {
							BFSUHR( 1 ) = VTRatio * ( WeightNow * ( IllumMapCalc( MapNum ).DaylBackFacSun( HourOfDay, 1, ILB, loop ) + IllumMapCalc( MapNum ).DaylBackFacSunDisk( HourOfDay, 1, ILB, loop ) ) + WeightPreviousHour * ( IllumMapCalc( MapNum ).DaylBackFacSun( PreviousHour, 1, ILB, loop ) + IllumMapCalc( MapNum ).DaylBackFacSunDisk( PreviousHour, 1, ILB, loop ) ) );
						}

						SFSKHR( 1, ISky ) = VTRatio * ( WeightNow * IllumMapCalc( MapNum ).DaylSourceFacSky( HourOfDay, 1, ISky, ILB, loop ) + WeightPreviousHour * IllumMapCalc( MapNum ).DaylSourceFacSky( PreviousHour, 1, ISky, ILB, loop ) );

						if ( ISky == 1 ) {
							SFSUHR( 1 ) = VTRatio * ( WeightNow * ( IllumMapCalc( MapNum ).DaylSourceFacSun( HourOfDay, 1, ILB, loop ) + IllumMapCalc( MapNum ).DaylSourceFacSunDisk( HourOfDay, 1, ILB, loop ) ) + WeightPreviousHour * ( IllumMapCalc( MapNum ).DaylSourceFacSun( PreviousHour, 1, ILB, loop ) + IllumMapCalc( MapNum ).DaylSourceFacSunDisk( PreviousHour, 1, ILB, loop ) ) );
						}

						if ( SurfaceWindow( IWin ).ShadingFlag >= 1 || SurfaceWindow( IWin ).SolarDiffusing ) {

							//                                 ===Shaded window===
							if ( ! SurfaceWindow( IWin ).MovableSlats ) {
								// Shade, screen, blind with fixed slats, or diffusing glass
								DFSKHR( 2, ISky ) = VTRatio * ( WeightNow * IllumMapCalc( MapNum ).DaylIllFacSky( HourOfDay, 2, ISky, ILB, loop ) + WeightPreviousHour * IllumMapCalc( MapNum ).DaylIllFacSky( PreviousHour, 2, ISky, ILB, loop ) );

								if ( ISky == 1 ) {
									DFSUHR( 2 ) = VTRatio * ( WeightNow * IllumMapCalc( MapNum ).DaylIllFacSun( HourOfDay, 2, ILB, loop ) + WeightPreviousHour * IllumMapCalc( MapNum ).DaylIllFacSun( PreviousHour, 2, ILB, loop ) );

									if ( ! SurfaceWindow( IWin ).SlatsBlockBeam ) {
										DFSUHR( 2 ) += VTRatio * ( WeightNow * IllumMapCalc( MapNum ).DaylIllFacSunDisk( HourOfDay, 2, ILB, loop ) + WeightPreviousHour * IllumMapCalc( MapNum ).DaylIllFacSunDisk( PreviousHour, 2, ILB, loop ) );
									}
								}

								BFSKHR( 2, ISky ) = VTRatio * ( WeightNow * IllumMapCalc( MapNum ).DaylBackFacSky( HourOfDay, 2, ISky, ILB, loop ) + WeightPreviousHour * IllumMapCalc( MapNum ).DaylBackFacSky( PreviousHour, 2, ISky, ILB, loop ) );

								if ( ISky == 1 ) {
									BFSUHR( 2 ) = VTRatio * ( WeightNow * IllumMapCalc( MapNum ).DaylBackFacSun( HourOfDay, 2, ILB, loop ) + WeightPreviousHour * IllumMapCalc( MapNum ).DaylBackFacSun( PreviousHour, 2, ILB, loop ) );
									if ( ! SurfaceWindow( IWin ).SlatsBlockBeam ) {
										BFSUHR( 2 ) += VTRatio * ( WeightNow * IllumMapCalc( MapNum ).DaylBackFacSunDisk( HourOfDay, 2, ILB, loop ) + WeightPreviousHour * IllumMapCalc( MapNum ).DaylBackFacSunDisk( PreviousHour, 2, ILB, loop ) );
									}
								}

								SFSKHR( 2, ISky ) = VTRatio * ( WeightNow * IllumMapCalc( MapNum ).DaylSourceFacSky( HourOfDay, 2, ISky, ILB, loop ) + WeightPreviousHour * IllumMapCalc( MapNum ).DaylSourceFacSky( PreviousHour, 2, ISky, ILB, loop ) );

								if ( ISky == 1 ) {
									SFSUHR( 2 ) = VTRatio * ( WeightNow * IllumMapCalc( MapNum ).DaylSourceFacSun( HourOfDay, 2, ILB, loop ) + WeightPreviousHour * IllumMapCalc( MapNum ).DaylSourceFacSun( PreviousHour, 2, ILB, loop ) );
									if ( ! SurfaceWindow( IWin ).SlatsBlockBeam ) {
										SFSUHR( 2 ) += VTRatio * ( WeightNow * IllumMapCalc( MapNum ).DaylSourceFacSunDisk( HourOfDay, 2, ILB, loop ) + WeightPreviousHour * IllumMapCalc( MapNum ).DaylSourceFacSunDisk( PreviousHour, 2, ILB, loop ) );
									}
								}

							} else { // Blind with movable slats
								VarSlats = SurfaceWindow( IWin ).MovableSlats;
								SlatAng = SurfaceWindow( IWin ).SlatAngThisTS;

								DFSKHR( 2, ISky ) = VTRatio * ( WeightNow * InterpSlatAng( SlatAng, VarSlats, IllumMapCalc( MapNum ).DaylIllFacSky( HourOfDay, {2,MaxSlatAngs + 1}, ISky, ILB, loop ) ) + WeightPreviousHour * InterpSlatAng( SlatAng, VarSlats, IllumMapCalc( MapNum ).DaylIllFacSky( PreviousHour, {2,MaxSlatAngs + 1}, ISky, ILB, loop ) ) );

								if ( ISky == 1 ) {
									DFSUHR( 2 ) = VTRatio * ( WeightNow * InterpSlatAng( SlatAng, VarSlats, IllumMapCalc( MapNum ).DaylIllFacSun( HourOfDay, {2,MaxSlatAngs + 1}, ILB, loop ) ) + WeightPreviousHour * InterpSlatAng( SlatAng, VarSlats, IllumMapCalc( MapNum ).DaylIllFacSun( PreviousHour, {2,MaxSlatAngs + 1}, ILB, loop ) ) );

									// We add the contribution from the solar disk if slats do not block beam solar
									// TH CR 8010, DaylIllFacSunDisk needs to be interpolated
									//IF(.NOT.SurfaceWindow(IWin)%SlatsBlockBeam) DFSUHR(2) = DFSUHR(2) + &
									//  VTRatio * (WeightNow * ZoneDaylight(ZoneNum)%DaylIllFacSunDisk(loop,ILB,2,HourOfDay) + &
									//            WeightPreviousHour * ZoneDaylight(ZoneNum)%DaylIllFacSunDisk(loop,ILB,2,PreviousHour))
									if ( ! SurfaceWindow( IWin ).SlatsBlockBeam ) {
										DFSUHR( 2 ) += VTRatio * ( WeightNow * InterpSlatAng( SlatAng, VarSlats, IllumMapCalc( MapNum ).DaylIllFacSunDisk( HourOfDay, {2,MaxSlatAngs + 1}, ILB, loop ) ) + WeightPreviousHour * InterpSlatAng( SlatAng, VarSlats, IllumMapCalc( MapNum ).DaylIllFacSunDisk( PreviousHour, {2,MaxSlatAngs + 1}, ILB, loop ) ) );
									}
								}

								BFSKHR( 2, ISky ) = VTRatio * ( WeightNow * InterpSlatAng( SlatAng, VarSlats, IllumMapCalc( MapNum ).DaylBackFacSky( HourOfDay, {2,MaxSlatAngs + 1}, ISky, ILB, loop ) ) + WeightPreviousHour * InterpSlatAng( SlatAng, VarSlats, IllumMapCalc( MapNum ).DaylBackFacSky( PreviousHour, {2,MaxSlatAngs + 1}, ISky, ILB, loop ) ) );

								if ( ISky == 1 ) {
									BFSUHR( 2 ) = VTRatio * ( WeightNow * InterpSlatAng( SlatAng, VarSlats, IllumMapCalc( MapNum ).DaylBackFacSun( HourOfDay, {2,MaxSlatAngs + 1}, ILB, loop ) ) + WeightPreviousHour * InterpSlatAng( SlatAng, VarSlats, IllumMapCalc( MapNum ).DaylBackFacSun( PreviousHour, {2,MaxSlatAngs + 1}, ILB, loop ) ) );

									// TH CR 8010, DaylBackFacSunDisk needs to be interpolated
									if ( ! SurfaceWindow( IWin ).SlatsBlockBeam ) {
										BFSUHR( 2 ) += VTRatio * ( WeightNow * InterpSlatAng( SlatAng, VarSlats, IllumMapCalc( MapNum ).DaylBackFacSunDisk( HourOfDay, {2,MaxSlatAngs + 1}, ILB, loop ) ) + WeightPreviousHour * InterpSlatAng( SlatAng, VarSlats, IllumMapCalc( MapNum ).DaylBackFacSunDisk( PreviousHour, {2,MaxSlatAngs + 1}, ILB, loop ) ) );
									}
								}

								SFSKHR( 2, ISky ) = VTRatio * ( WeightNow * InterpSlatAng( SlatAng, VarSlats, IllumMapCalc( MapNum ).DaylSourceFacSky( HourOfDay, {2,MaxSlatAngs + 1}, ISky, ILB, loop ) ) + WeightPreviousHour * InterpSlatAng( SlatAng, VarSlats, IllumMapCalc( MapNum ).DaylSourceFacSky( PreviousHour, {2,MaxSlatAngs + 1}, ISky, ILB, loop ) ) );

								if ( ISky == 1 ) {
									SFSUHR( 2 ) = VTRatio * ( WeightNow * InterpSlatAng( SlatAng, VarSlats, IllumMapCalc( MapNum ).DaylSourceFacSun( HourOfDay, {2,MaxSlatAngs + 1}, ILB, loop ) ) + WeightPreviousHour * InterpSlatAng( SlatAng, VarSlats, IllumMapCalc( MapNum ).DaylSourceFacSun( PreviousHour, {2,MaxSlatAngs + 1}, ILB, loop ) ) );

									// TH CR 8010, DaylSourceFacSunDisk needs to be interpolated
									if ( ! SurfaceWindow( IWin ).SlatsBlockBeam ) {
										SFSUHR( 2 ) += VTRatio * ( WeightNow * InterpSlatAng( SlatAng, VarSlats, IllumMapCalc( MapNum ).DaylSourceFacSunDisk( HourOfDay, {2,MaxSlatAngs + 1}, ILB, loop ) ) + WeightPreviousHour * InterpSlatAng( SlatAng, VarSlats, IllumMapCalc( MapNum ).DaylSourceFacSunDisk( PreviousHour, {2,MaxSlatAngs + 1}, ILB, loop ) ) );
									}
								}

							} // End of check if window has blind with movable slats

						} // End of check if window is shaded or has diffusing glass

					}

					//              Get illuminance at ref point from bare and shaded window by
					//              multiplying daylight factors by exterior horizontal illuminance

					// Adding 0.001 in the following prevents zero HorIllSky in early morning or late evening when sun
					// is up in the present time step but GILSK(ISky,HourOfDay) and GILSK(ISky,NextHour) are both zero.
					for ( ISky = 1; ISky <= 4; ++ISky ) {
						HorIllSky( ISky ) = WeightNow * GILSK( HourOfDay, ISky ) + WeightPreviousHour * GILSK( PreviousHour, ISky ) + 0.001;
					}

					// HISKF is current time step horizontal illuminance from sky, calculated in DayltgLuminousEfficacy,
					// which is called in WeatherManager. HISUNF is current time step horizontal illuminance from sun,
					// also calculated in DayltgLuminousEfficacy.
					HorIllSkyFac = HISKF / ( ( 1.0 - SkyWeight ) * HorIllSky( ISky2 ) + SkyWeight * HorIllSky( ISky1 ) );

					for ( IS = 1; IS <= 2; ++IS ) {
						if ( IS == 2 && SurfaceWindow( IWin ).ShadingFlag <= 0 && ! SurfaceWindow( IWin ).SolarDiffusing ) break;

						IllumMapCalc( MapNum ).IllumFromWinAtMapPt( loop, IS, ILB ) = DFSUHR( IS ) * HISUNF + HorIllSkyFac * ( DFSKHR( IS, ISky1 ) * SkyWeight * HorIllSky( ISky1 ) + DFSKHR( IS, ISky2 ) * ( 1.0 - SkyWeight ) * HorIllSky( ISky2 ) );

						IllumMapCalc( MapNum ).BackLumFromWinAtMapPt( loop, IS, ILB ) = BFSUHR( IS ) * HISUNF + HorIllSkyFac * ( BFSKHR( IS, ISky1 ) * SkyWeight * HorIllSky( ISky1 ) + BFSKHR( IS, ISky2 ) * ( 1.0 - SkyWeight ) * HorIllSky( ISky2 ) );

						IllumMapCalc( MapNum ).SourceLumFromWinAtMapPt( loop, IS, ILB ) = SFSUHR( IS ) * HISUNF + HorIllSkyFac * ( SFSKHR( IS, ISky1 ) * SkyWeight * HorIllSky( ISky1 ) + SFSKHR( IS, ISky2 ) * ( 1.0 - SkyWeight ) * HorIllSky( ISky2 ) );
						IllumMapCalc( MapNum ).SourceLumFromWinAtMapPt( loop, IS, ILB ) = max( IllumMapCalc( MapNum ).SourceLumFromWinAtMapPt( loop, IS, ILB ), 0.0 );
					}

				} // End of reference point loop
			} // End of first loop over windows

			//              Second loop over windows. Find total daylight illuminance
			//              and background luminance for each ref pt from all windows in
			//              the space.  Use shading flags.

			for ( loop = 1; loop <= ZoneDaylight( ZoneNum ).NumOfDayltgExtWins; ++loop ) {
				IWin = ZoneDaylight( ZoneNum ).DayltgExtWinSurfNums( loop );

				IS = 1;
				if ( ( SurfaceWindow( IWin ).ShadingFlag >= 1 && SurfaceWindow( IWin ).ShadingFlag <= 9 ) || SurfaceWindow( IWin ).SolarDiffusing ) IS = 2;

				// CR 8057. 3/17/2010.
				// Switchable windows may be in partially switched state rather than fully dark state
				VTMULT = 1.0;

				ICtrl = Surface( IWin ).WindowShadingControlPtr;
				if ( ICtrl > 0 ) {
					if ( WindowShadingControl( ICtrl ).ShadingControlType == WSCT_MeetDaylIlumSetp && SurfaceWindow( IWin ).ShadingFlag == SwitchableGlazing ) {
						// switchable windows in partial or fully switched state,
						//  get its intermediate VT calculated in DayltgInteriorIllum
						IConstShaded = Surface( IWin ).ShadedConstruction;
						if ( IConstShaded > 0 ) VTDark = POLYF( 1.0, Construct( IConstShaded ).TransVisBeamCoef ) * SurfaceWindow( IWin ).GlazedFrac;
						if ( VTDark > 0 ) VTMULT = SurfaceWindow( IWin ).VisTransSelected / VTDark;
					}
				}

				for ( IL = 1; IL <= NREFPT; ++IL ) {
					//              Determine if illuminance contribution is from bare or shaded window
					daylight_illum( IL ) += VTMULT * IllumMapCalc( MapNum ).IllumFromWinAtMapPt( loop, IS, IL );
					BACLUM( IL ) += VTMULT * IllumMapCalc( MapNum ).BackLumFromWinAtMapPt( loop, IS, IL );
				}

			} // End of second window loop

			//              Calculate glare index at each reference point
			for ( IL = 1; IL <= NREFPT; ++IL ) {
				//        Following code taken directly from DayltgGlare ... duplicate calculation
				// Initialize glare constant
				GTOT = 0.0;

				// Loop over exterior windows associated with zone
				for ( loop = 1; loop <= ZoneDaylight( ZoneNum ).NumOfDayltgExtWins; ++loop ) {
					IWin = ZoneDaylight( ZoneNum ).DayltgExtWinSurfNums( loop );
					IS = 1;
					if ( ( SurfaceWindow( IWin ).ShadingFlag >= 1 && SurfaceWindow( IWin ).ShadingFlag <= 9 ) || SurfaceWindow( IWin ).SolarDiffusing ) IS = 2;

					// CR 8057. 3/17/2010
					VTMULT = 1.0;

					ICtrl = Surface( IWin ).WindowShadingControlPtr;
					if ( ICtrl > 0 ) {
						if ( WindowShadingControl( ICtrl ).ShadingControlType == WSCT_MeetDaylIlumSetp && SurfaceWindow( IWin ).ShadingFlag == SwitchableGlazing ) {
							// switchable windows in partial or fully switched state,
							//  get its intermediate VT calculated in DayltgInteriorIllum
							IConstShaded = Surface( IWin ).ShadedConstruction;
							if ( IConstShaded > 0 ) VTDark = POLYF( 1.0, Construct( IConstShaded ).TransVisBeamCoef ) * SurfaceWindow( IWin ).GlazedFrac;
							if ( VTDark > 0 ) VTMULT = SurfaceWindow( IWin ).VisTransSelected / VTDark;
						}
					}

					// Conversion from ft-L to cd/m2, with cd/m2 = 0.2936 ft-L, gives the 0.4794 factor
					// below, which is (0.2936)**0.6
					GTOT1 = 0.4794 * ( std::pow( VTMULT * IllumMapCalc( MapNum ).SourceLumFromWinAtMapPt( loop, IS, IL ), 1.6 ) ) * std::pow( IllumMapCalc( MapNum ).SolidAngAtMapPtWtd( loop, IL ), 0.8 );
					GTOT2 = BACLUM( IL ) + 0.07 * ( std::sqrt( IllumMapCalc( MapNum ).SolidAngAtMapPt( loop, IL ) ) ) * VTMULT * IllumMapCalc( MapNum ).SourceLumFromWinAtMapPt( loop, IS, IL );
					GTOT += GTOT1 / ( GTOT2 + 0.000001 );
				}

				// Glare index (adding 0.000001 prevents LOG10 (0))
				GLRNDX( IL ) = 10.0 * std::log10( GTOT + 0.000001 );
				// Set glare index to zero for GTOT < 1
				GLRNDX( IL ) = max( 0.0, GLRNDX( IL ) );
			}

			//              Variables for reporting
			for ( IL = 1; IL <= NREFPT; ++IL ) {
				IllumMapCalc( MapNum ).DaylIllumAtMapPt( IL ) = max( daylight_illum( IL ), 0.0 );
				IllumMapCalc( MapNum ).GlareIndexAtMapPt( IL ) = GLRNDX( IL );
			}
		}

	}

	void
	ReportIllumMap( int const MapNum )
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Peter Ellis
		//       DATE WRITTEN   May 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine produces the Daylighting Illuminance Map output.  Each separate map (by zone)
		// is placed on a temporary file and later (see CloseReportIllumMaps) coallesced into a single
		// output file.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::RoundSigDigits;
		using DataStringGlobals::CharTab;
		using DataStringGlobals::CharComma;
		using DataStringGlobals::CharSpace;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static gio::Fmt FmtA( "(A)" );
		static gio::Fmt HrFmt( "(I2.2)" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		std::string String;
		int RefPt;
		int X;
		int Y;
		int R;
		//  REAL(r64)           :: NumOut
		int IllumOut;

		static bool firstTime( true );
		static Array1D_bool FirstTimeMaps;
		static Array1D_bool EnvrnPrint;
		static Array1D_string SavedMnDy;
		static Array2D_string RefPts;
		std::string MapNoString;
		std::string HrString;
		int linelen;
		std::string AddXorYString;
		// BSLLC Start
		static Array1D< Real64 > XValue;
		static Array1D< Real64 > YValue;
		static Array2D< Real64 > IllumValue;
		int SQMonth;
		int SQDayOfMonth;
		int IllumIndex;
		static bool SQFirstTime( true );
//		static bool CommaDelimited( true ); //Unused Set but never used
		// BSLLC Finish

		// FLOW:
		if ( firstTime ) {
			firstTime = false;
			FirstTimeMaps.dimension( TotIllumMaps, true );
			EnvrnPrint.dimension( TotIllumMaps, true );
			RefPts.allocate( NumOfZones, MaxRefPoints );
			SavedMnDy.allocate( TotIllumMaps );
		}

		if ( FirstTimeMaps( MapNum ) ) {

			FirstTimeMaps( MapNum ) = false;
			IllumMap( MapNum ).UnitNo = GetNewUnitNumber();
			MapNoString = RoundSigDigits( MapNum );
			if ( MapColSep == CharTab ) {
				{ IOFlags flags; flags.ACTION( "readwrite" ); flags.STATUS( "UNKNOWN" ); gio::open( IllumMap( MapNum ).UnitNo, DataStringGlobals::outputMapTabFileName + MapNoString, flags ); if ( flags.err() ) goto Label901; }
//				CommaDelimited = false; //Unused Set but never used
			} else if ( MapColSep == CharComma ) {
				{ IOFlags flags; flags.ACTION( "readwrite" ); flags.STATUS( "UNKNOWN" ); gio::open( IllumMap( MapNum ).UnitNo, DataStringGlobals::outputMapCsvFileName + MapNoString, flags ); if ( flags.err() ) goto Label902; }
//				CommaDelimited = true; //Unused Set but never used
			} else {
				{ IOFlags flags; flags.ACTION( "readwrite" ); flags.STATUS( "UNKNOWN" ); gio::open( IllumMap( MapNum ).UnitNo, DataStringGlobals::outputMapTxtFileName + MapNoString, flags ); if ( flags.err() ) goto Label903; }
//				CommaDelimited = false; //Unused Set but never used
			}

			SavedMnDy( MapNum ) = CurMnDyHr.substr( 0, 5 );

			IllumMap( MapNum ).Name = IllumMap( MapNum ).Name + " at " + RoundSigDigits( IllumMap( MapNum ).Z, 2 ) + 'm';

			for ( R = 1; R <= ZoneDaylight( IllumMap( MapNum ).Zone ).TotalDaylRefPoints; ++R ) {
				String = RoundSigDigits( R );
				RefPts( IllumMap( MapNum ).Zone, R ) = "RefPt" + String + "=(";
				String = RoundSigDigits( ZoneDaylight( IllumMap( MapNum ).Zone ).DaylRefPtAbsCoord( 1, R ), 2 );
				RefPts( IllumMap( MapNum ).Zone, R ) = RefPts( IllumMap( MapNum ).Zone, R ) + String + ':';
				String = RoundSigDigits( ZoneDaylight( IllumMap( MapNum ).Zone ).DaylRefPtAbsCoord( 2, R ), 2 );
				RefPts( IllumMap( MapNum ).Zone, R ) = RefPts( IllumMap( MapNum ).Zone, R ) + String + ':';
				String = RoundSigDigits( ZoneDaylight( IllumMap( MapNum ).Zone ).DaylRefPtAbsCoord( 3, R ), 2 );
				RefPts( IllumMap( MapNum ).Zone, R ) = RefPts( IllumMap( MapNum ).Zone, R ) + String + ')';
			}
		}
		if ( SavedMnDy( MapNum ) != CurMnDyHr.substr( 0, 5 ) ) {
			EnvrnPrint( MapNum ) = true;
			SavedMnDy( MapNum ) = CurMnDyHr.substr( 0, 5 );
		}
		if ( EnvrnPrint( MapNum ) ) {
			WriteDaylightMapTitle( MapNum, IllumMap( MapNum ).UnitNo, IllumMap( MapNum ).Name, EnvironmentName, IllumMap( MapNum ).Zone, RefPts( IllumMap( MapNum ).Zone, 1 ), RefPts( IllumMap( MapNum ).Zone, 2 ), IllumMap( MapNum ).Z );
			EnvrnPrint( MapNum ) = false;
		}

		if ( ! WarmupFlag ) {
			if ( TimeStep == NumOfTimeStepInHour ) { // Report only hourly

				// Write X scale column header
				gio::write( HrString, HrFmt ) << HourOfDay;
				mapLine = ' ' + SavedMnDy( MapNum ) + ' ' + HrString + ":00";
				if ( IllumMap( MapNum ).HeaderXLineLengthNeeded ) linelen = int( len( mapLine ) );
				RefPt = 1;
				for ( X = 1; X <= IllumMap( MapNum ).Xnum; ++X ) {
					AddXorYString = std::string( 1, MapColSep ) + '(' + RoundSigDigits( IllumMapCalc( MapNum ).MapRefPtAbsCoord( 1, RefPt ), 2 ) + ';' + RoundSigDigits( IllumMapCalc( MapNum ).MapRefPtAbsCoord( 2, RefPt ), 2 ) + ")=";
					if ( IllumMap( MapNum ).HeaderXLineLengthNeeded ) linelen += int( len( AddXorYString ) );
					mapLine += AddXorYString;
					++RefPt;
				} // X

				if ( IllumMap( MapNum ).HeaderXLineLengthNeeded ) {
					IllumMap( MapNum ).HeaderXLineLength = linelen;
					if ( static_cast< std::string::size_type >( IllumMap( MapNum ).HeaderXLineLength ) > len( mapLine ) ) {
						ShowWarningError( "ReportIllumMap: Map=\"" + IllumMap( MapNum ).Name + "\" -- the X Header overflows buffer -- will be truncated at " + RoundSigDigits( int( len( mapLine ) ) ) + " characters." );
						ShowContinueError( "...needed " + RoundSigDigits( IllumMap( MapNum ).HeaderXLineLength ) + " characters. Please contact EnergyPlus support." );
					}
					IllumMap( MapNum ).HeaderXLineLengthNeeded = false;
				}

				gio::write( IllumMap( MapNum ).UnitNo, FmtA ) << mapLine;

				// Write Y scale prefix and illuminance values
				RefPt = 1;
				for ( Y = 1; Y <= IllumMap( MapNum ).Ynum; ++Y ) {
					mapLine = "(" + RoundSigDigits( IllumMapCalc( MapNum ).MapRefPtAbsCoord( 1, RefPt ), 2 ) + ';' + RoundSigDigits( IllumMapCalc( MapNum ).MapRefPtAbsCoord( 2, RefPt ), 2 ) + ")=";
					for ( R = RefPt; R <= RefPt + IllumMap( MapNum ).Xnum - 1; ++R ) {
						IllumOut = nint( IllumMapCalc( MapNum ).DaylIllumAtMapPtHr( R ) );
						if ( IllumMapCalc( MapNum ).MapRefPtInBounds( R ) ) {
							String = RoundSigDigits( IllumOut );
						} else {
							String = RoundSigDigits( IllumOut );
							String = "*" + String;
						}
						mapLine += MapColSep + String;
					}

					gio::write( IllumMap( MapNum ).UnitNo, FmtA ) << mapLine;

					RefPt += IllumMap( MapNum ).Xnum;
				} // X

				if ( sqlite ) {
					if ( SQFirstTime ) {
						int const nX( maxval( IllumMap, &IllumMapData::Xnum ) );
						int const nY( maxval( IllumMap, &IllumMapData::Ynum ) );
						XValue.allocate( nX );
						YValue.allocate( nY );
						IllumValue.allocate( nX, nY );
						SQFirstTime = false;
					}

					SQMonth = Month;
					SQDayOfMonth = DayOfMonth;

					for ( Y = 1; Y <= IllumMap( MapNum ).Ynum; ++Y ) {
						YValue( Y ) = IllumMap( MapNum ).Ymin + ( Y - 1 ) * IllumMap( MapNum ).Yinc;
						for ( X = 1; X <= IllumMap( MapNum ).Xnum; ++X ) {
							XValue( X ) = IllumMap( MapNum ).Xmin + ( X - 1 ) * IllumMap( MapNum ).Xinc;
							IllumIndex = X + ( Y - 1 ) * IllumMap( MapNum ).Xnum;
							IllumValue( X, Y ) = nint( IllumMapCalc( MapNum ).DaylIllumAtMapPtHr( IllumIndex ) );
							if ( ! IllumMapCalc( MapNum ).MapRefPtInBounds( IllumIndex ) ) {
								IllumValue( X, Y ) = -IllumValue( X, Y );
							}
						} // X Loop
					} // Y Loop

					sqlite->createSQLiteDaylightMap( MapNum, SQMonth, SQDayOfMonth, HourOfDay, IllumMap( MapNum ).Xnum, XValue, IllumMap( MapNum ).Ynum, YValue, IllumValue );

				} // WriteOutputToSQLite
			} // end time step
		} // not Warmup

		return;

Label901: ;
		ShowFatalError( "ReportIllumMap: Could not open file "+ DataStringGlobals::outputMapTabFileName + MapNoString + "\" for output (write)." );
		return;

Label902: ;
		ShowFatalError( "ReportIllumMap: Could not open file "+ DataStringGlobals::outputMapCsvFileName + MapNoString + "\" for output (write)." );
		return;

Label903: ;
		ShowFatalError( "ReportIllumMap: Could not open file "+ DataStringGlobals::outputMapTxtFileName + MapNoString + "\" for output (write)." );

	}

	void
	CloseReportIllumMaps()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   June 2003
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine "closes" out the created daylight illuminance maps by merging them
		// into the "eplusout.map" file.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataStringGlobals::CharTab;
		using DataStringGlobals::CharComma;
		using DataStringGlobals::CharSpace;
		using General::TrimSigDigits;
		using DataErrorTracking::AbortProcessing;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static gio::Fmt FmtA( "(A)" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static int MapOutputFile;
		int MapNum;
		static int ios( 0 );
		int NumLines;

		if ( TotIllumMaps > 0 ) {

			MapOutputFile = GetNewUnitNumber(); // can add this to DataGlobals with the others...

			// Write map header
			if ( MapColSep == CharTab ) {
				{ IOFlags flags; flags.ACTION( "write" ); flags.STATUS( "UNKNOWN" ); gio::open( MapOutputFile, DataStringGlobals::outputMapTabFileName, flags ); if ( flags.err() ) goto Label901; }
			} else if ( MapColSep == CharComma ) {
				{ IOFlags flags; flags.ACTION( "write" ); flags.STATUS( "UNKNOWN" ); gio::open( MapOutputFile, DataStringGlobals::outputMapCsvFileName, flags ); if ( flags.err() ) goto Label902; }
			} else {
				{ IOFlags flags; flags.ACTION( "write" ); flags.STATUS( "UNKNOWN" ); gio::open( MapOutputFile, DataStringGlobals::outputMapTxtFileName, flags ); if ( flags.err() ) goto Label903; }
			}

			for ( MapNum = 1; MapNum <= TotIllumMaps; ++MapNum ) {
				if ( IllumMap( MapNum ).UnitNo == 0 ) continue; // fatal error processing
				NumLines = 0;
				gio::rewind( IllumMap( MapNum ).UnitNo );
				ios = 0;
				while ( ios == 0 ) {
					{ IOFlags flags; gio::read( IllumMap( MapNum ).UnitNo, FmtA, flags ) >> mapLine; ios = flags.ios(); }
					if ( ios > 0 ) { // usually a read error
						ShowFatalError( "CloseReportIllumMaps: Failed to read map. IOError=" + TrimSigDigits( ios ) );
					} else if ( ios != 0 ) {
						if ( NumLines == 0 ) {
							ShowSevereError( "CloseReportIllumMaps: IllumMap=\"" + IllumMap( MapNum ).Name + "\" is empty." );
						}
						break;
					}
					++NumLines;
					gio::write( MapOutputFile, FmtA ) << mapLine;
				}
				{ IOFlags flags; flags.DISPOSE( "DELETE" ); gio::close( IllumMap( MapNum ).UnitNo, flags ); }
			}

			if ( ! mapResultsReported && ! AbortProcessing ) {
				ShowSevereError( "CloseReportIllumMaps: Illuminance maps requested but no data ever reported. Likely cause is no solar." );
				gio::write( MapOutputFile, FmtA ) << "CloseReportIllumMaps: Illuminance maps requested but no data ever reported. Likely cause is no solar.";
			}

			gio::close( MapOutputFile );

		}

		return;

Label901: ;
		ShowFatalError( "CloseReportIllumMaps: Could not open file "+DataStringGlobals::outputMapTabFileName+" for output (write)." );
		return;

Label902: ;
		ShowFatalError( "CloseReportIllumMaps: Could not open file "+DataStringGlobals::outputMapCsvFileName+" for output (write)." );
		return;

Label903: ;
		ShowFatalError( "CloseReportIllumMaps: Could not open file "+DataStringGlobals::outputMapTxtFileName+" for output (write)." );

	}

	void
	CloseDFSFile()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   August 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Make sure DFSFile is closed at exit time.  Do not rely on operating system to
		// take care of it.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

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

		if ( OutputFileDFS > 0 ) gio::close( OutputFileDFS );

	}

	void
	DayltgSetupAdjZoneListsAndPointers()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   Feb. 2004
		//       MODIFIED:      June 2010;LKL - Merged two routines.
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// For each Daylighting:Detailed zone, Z, creates a list of other zones, Zadj,
		// that have one or more exterior windows and that share one or more interior
		// windows with Z. Used in calculation of daylighting through interior windows.

		// Sets the daylighting factor pointers for each Daylighting:Detailed zone. The pointer
		// may be associated with an exterior window in a daylit target zone or an exterior window in
		// an adjacent zone, daylit or not, that shares interior windows with the target zone.

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

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int ZoneNum; // Zone number
		int NumList; // Counter of adjacent zone numbers
		int ZoneNumAdj; // Zone number
		bool AdjZoneHasExtWins; // True if adjacent zone has one or more exterior windows
		int SurfNumAdj; // Surface number
		int SurfNumAdj2; // Surface number
		int ExtWinIndex;
		int IntWinIndex;
		int ZoneAdjLoop;
		int NumOfIntWindowsCount;
		int DayltgFacPtr; // Daylighting factor pointer
		int ZoneExtWinCtr; // Exterior window counter
		int SurfNum; // Surface number
		int loop; // DO loop index
		Array1D_int ZoneExtWin;
		int WinSize;
		int RefSize;
		int MapNum;

		// Formats
		static gio::Fmt Format_700( "('! <Zone/Window Adjacency Daylighting Counts>, Zone Name, ','Number of Exterior Windows, Number of Exterior Windows in Adjacent Zones')" );
		static gio::Fmt Format_701( "('Zone/Window Adjacency Daylighting Counts, ',A,',',A,',',A)" );
		static gio::Fmt Format_702( "('! <Zone/Window Adjacency Daylighting Matrix>, Zone Name, Number of Adjacent Zones with Windows,','Adjacent Zone Names - 1st 100 (max)')" );
		static gio::Fmt Format_703( "('Zone/Window Adjacency Daylighting Matrix, ',A,',',A,$)" );
		static gio::Fmt fmtCommaA( "(',',A,$)" );

		// FLOW:
		// Count number of exterior Windows (use to allocate arrays)

		// FLOW:

		for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
			// Count exterior windows in this zone
			for ( SurfNum = Zone( ZoneNum ).SurfaceFirst; SurfNum <= Zone( ZoneNum ).SurfaceLast; ++SurfNum ) {
				if ( ( Surface( SurfNum ).Class == SurfaceClass_Window && Surface( SurfNum ).ExtBoundCond == ExternalEnvironment ) || SurfaceWindow( SurfNum ).OriginalClass == SurfaceClass_TDD_Diffuser ) {
					++ZoneDaylight( ZoneNum ).TotalExtWindows;
				}
			}
		}

		for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
			NumList = 0;
			if ( ZoneDaylight( ZoneNum ).TotalDaylRefPoints == 0 ) continue;
			// This is a Daylighting:Detailed zone
			// Find adjacent zones
			for ( ZoneNumAdj = 1; ZoneNumAdj <= NumOfZones; ++ZoneNumAdj ) {
				if ( ZoneNumAdj == ZoneNum ) continue;
				// Require that ZoneNumAdj have a least one exterior window
				AdjZoneHasExtWins = false;
				for ( SurfNumAdj = Zone( ZoneNumAdj ).SurfaceFirst; SurfNumAdj <= Zone( ZoneNumAdj ).SurfaceLast; ++SurfNumAdj ) {
					if ( Surface( SurfNumAdj ).Class == SurfaceClass_Window && Surface( SurfNumAdj ).ExtBoundCond == ExternalEnvironment ) {
						AdjZoneHasExtWins = true;
						break;
					}
				}
				if ( ! AdjZoneHasExtWins ) continue;
				// Loop again through surfaces in ZoneNumAdj and see if any are interior windows adjacent to ZoneNum
				for ( SurfNumAdj = Zone( ZoneNumAdj ).SurfaceFirst; SurfNumAdj <= Zone( ZoneNumAdj ).SurfaceLast; ++SurfNumAdj ) {
					if ( Surface( SurfNumAdj ).Class == SurfaceClass_Window && Surface( SurfNumAdj ).ExtBoundCond >= 1 ) {
						// This is an interior window in ZoneNumAdj
						if ( Surface( Surface( SurfNumAdj ).ExtBoundCond ).Zone == ZoneNum ) {
							// This interior window is adjacent to ZoneNum
							++NumList;
							break;
						}
					}
				}
			}
			ZoneDaylight( ZoneNum ).AdjIntWinZoneNums.allocate( NumList );
			ZoneDaylight( ZoneNum ).AdjIntWinZoneNums = 0;
		}

		for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
			NumList = 0;
			if ( ZoneDaylight( ZoneNum ).TotalDaylRefPoints == 0 ) continue;
			// This is a Daylighting:Detailed zone
			// Find adjacent zones
			for ( ZoneNumAdj = 1; ZoneNumAdj <= NumOfZones; ++ZoneNumAdj ) {
				if ( ZoneNumAdj == ZoneNum ) continue;
				// Require that ZoneNumAdj have a least one exterior window
				AdjZoneHasExtWins = false;
				for ( SurfNumAdj = Zone( ZoneNumAdj ).SurfaceFirst; SurfNumAdj <= Zone( ZoneNumAdj ).SurfaceLast; ++SurfNumAdj ) {
					if ( Surface( SurfNumAdj ).Class == SurfaceClass_Window && Surface( SurfNumAdj ).ExtBoundCond == ExternalEnvironment ) {
						AdjZoneHasExtWins = true;
						break;
					}
				}
				if ( ! AdjZoneHasExtWins ) continue;
				// Loop again through surfaces in ZoneNumAdj and see if any are interior windows adjacent to ZoneNum
				for ( SurfNumAdj = Zone( ZoneNumAdj ).SurfaceFirst; SurfNumAdj <= Zone( ZoneNumAdj ).SurfaceLast; ++SurfNumAdj ) {
					if ( Surface( SurfNumAdj ).Class == SurfaceClass_Window && Surface( SurfNumAdj ).ExtBoundCond >= 1 ) {
						// This is an interior window in ZoneNumAdj
						if ( Surface( Surface( SurfNumAdj ).ExtBoundCond ).Zone == ZoneNum ) {
							// This interior window is adjacent to ZoneNum
							++NumList;
							ZoneDaylight( ZoneNum ).AdjIntWinZoneNums( NumList ) = ZoneNumAdj;
							ZoneDaylight( ZoneNumAdj ).AdjZoneHasDayltgCtrl = true;
							break;
						}
					}
				}
			}
			ZoneDaylight( ZoneNum ).NumOfIntWinAdjZones = NumList;
		}

		// now fill out information on relationship between adjacent exterior windows and associated interior windows
		for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
			// first find count of exterior windows
			if ( ZoneDaylight( ZoneNum ).NumOfIntWinAdjZones <= 0 ) {
				ZoneDaylight( ZoneNum ).NumOfIntWinAdjZoneExtWins = 0;
				continue;
			}
			for ( ZoneAdjLoop = 1; ZoneAdjLoop <= ZoneDaylight( ZoneNum ).NumOfIntWinAdjZones; ++ZoneAdjLoop ) {
				ZoneNumAdj = ZoneDaylight( ZoneNum ).AdjIntWinZoneNums( ZoneAdjLoop );
				for ( SurfNumAdj = Zone( ZoneNumAdj ).SurfaceFirst; SurfNumAdj <= Zone( ZoneNumAdj ).SurfaceLast; ++SurfNumAdj ) {
					if ( Surface( SurfNumAdj ).Class == SurfaceClass_Window && Surface( SurfNumAdj ).ExtBoundCond == ExternalEnvironment ) {
						++ZoneDaylight( ZoneNum ).NumOfIntWinAdjZoneExtWins;
					}
				}
			}
			// now allocate nested struct based on exterior window count
			ZoneDaylight( ZoneNum ).IntWinAdjZoneExtWin.allocate( ZoneDaylight( ZoneNum ).NumOfIntWinAdjZoneExtWins );

			// now fill nested structure
			ExtWinIndex = 0;
			for ( ZoneAdjLoop = 1; ZoneAdjLoop <= ZoneDaylight( ZoneNum ).NumOfIntWinAdjZones; ++ZoneAdjLoop ) {
				ZoneNumAdj = ZoneDaylight( ZoneNum ).AdjIntWinZoneNums( ZoneAdjLoop );
				for ( SurfNumAdj = Zone( ZoneNumAdj ).SurfaceFirst; SurfNumAdj <= Zone( ZoneNumAdj ).SurfaceLast; ++SurfNumAdj ) {
					if ( Surface( SurfNumAdj ).Class == SurfaceClass_Window && Surface( SurfNumAdj ).ExtBoundCond == ExternalEnvironment ) {
						++ExtWinIndex;
						ZoneDaylight( ZoneNum ).IntWinAdjZoneExtWin( ExtWinIndex ).SurfNum = SurfNumAdj;

						// now count interior windows shared by both zones
						NumOfIntWindowsCount = 0;
						for ( SurfNumAdj2 = Zone( ZoneNumAdj ).SurfaceFirst; SurfNumAdj2 <= Zone( ZoneNumAdj ).SurfaceLast; ++SurfNumAdj2 ) {
							if ( Surface( SurfNumAdj2 ).Class == SurfaceClass_Window && Surface( SurfNumAdj2 ).ExtBoundCond >= 1 ) {
								// This is an interior window in ZoneNumAdj
								if ( Surface( Surface( SurfNumAdj2 ).ExtBoundCond ).Zone == ZoneNum ) {
									// This interior window is adjacent to ZoneNum and associated with this
									++NumOfIntWindowsCount;
								}
							}
						}
						// allocate nested array
						ZoneDaylight( ZoneNum ).IntWinAdjZoneExtWin( ExtWinIndex ).IntWinNum.allocate( NumOfIntWindowsCount );
						ZoneDaylight( ZoneNum ).IntWinAdjZoneExtWin( ExtWinIndex ).IntWinNum = 0;
						IntWinIndex = 0;
						for ( SurfNumAdj2 = Zone( ZoneNumAdj ).SurfaceFirst; SurfNumAdj2 <= Zone( ZoneNumAdj ).SurfaceLast; ++SurfNumAdj2 ) {
							if ( Surface( SurfNumAdj2 ).Class == SurfaceClass_Window && Surface( SurfNumAdj2 ).ExtBoundCond >= 1 ) {
								// This is an interior window in ZoneNumAdj
								if ( Surface( Surface( SurfNumAdj2 ).ExtBoundCond ).Zone == ZoneNum ) {
									// This interior window is adjacent to ZoneNum and associated with this
									++IntWinIndex;
									ZoneDaylight( ZoneNum ).IntWinAdjZoneExtWin( ExtWinIndex ).IntWinNum( IntWinIndex ) = SurfNumAdj2;
								}
							}
						}
					}
				}
			}

		}

		ZoneExtWin.dimension( NumOfZones, 0 );

		for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
			if ( ZoneDaylight( ZoneNum ).TotalDaylRefPoints > 0 ) {
				// This is a Daylighting:Detailed zone

				// Get exterior windows in this zone
				for ( SurfNum = Zone( ZoneNum ).SurfaceFirst; SurfNum <= Zone( ZoneNum ).SurfaceLast; ++SurfNum ) {
					if ( ( Surface( SurfNum ).Class == SurfaceClass_Window && Surface( SurfNum ).ExtBoundCond == ExternalEnvironment ) || SurfaceWindow( SurfNum ).OriginalClass == SurfaceClass_TDD_Diffuser ) {
						++ZoneExtWin( ZoneNum );
					}
				}

				// Get exterior windows in adjacent zones that share interior windows with ZoneNum
				if ( ZoneDaylight( ZoneNum ).NumOfIntWinAdjZones > 0 ) {
					for ( loop = 1; loop <= ZoneDaylight( ZoneNum ).NumOfIntWinAdjZones; ++loop ) {
						ZoneNumAdj = ZoneDaylight( ZoneNum ).AdjIntWinZoneNums( loop );
						// Get exterior windows in ZoneNumAdj -- there must be at least one, otherwise
						// it would not be an "AdjIntWinZone"
						for ( SurfNumAdj = Zone( ZoneNumAdj ).SurfaceFirst; SurfNumAdj <= Zone( ZoneNumAdj ).SurfaceLast; ++SurfNumAdj ) {
							if ( ( Surface( SurfNumAdj ).Class == SurfaceClass_Window && Surface( SurfNumAdj ).ExtBoundCond == ExternalEnvironment ) || SurfaceWindow( SurfNumAdj ).OriginalClass == SurfaceClass_TDD_Diffuser ) {
								++ZoneExtWin( ZoneNum );
							}
						}
					}
				}

			} // End of check if a Daylighting:Detailed zone
		} // End of primary zone loop

		DayltgFacPtr = 0;
		for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
			ZoneDaylight( ZoneNum ).NumOfDayltgExtWins = 0;
			if ( ZoneDaylight( ZoneNum ).TotalDaylRefPoints > 0 ) {
				// This is a Daylighting:Detailed zone

				// Get exterior windows in this zone
				if ( ZoneExtWin( ZoneNum ) == 0 ) continue;
				ZoneDaylight( ZoneNum ).DayltgExtWinSurfNums.allocate( ZoneExtWin( ZoneNum ) );
				ZoneDaylight( ZoneNum ).DayltgExtWinSurfNums = 0;
				ZoneDaylight( ZoneNum ).DayltgFacPtrsForExtWins.allocate( ZoneExtWin( ZoneNum ) );
				ZoneDaylight( ZoneNum ).DayltgFacPtrsForExtWins = 0;

				ZoneDaylight( ZoneNum ).SolidAngAtRefPt.allocate( ZoneExtWin( ZoneNum ), ZoneDaylight( ZoneNum ).TotalDaylRefPoints );
				ZoneDaylight( ZoneNum ).SolidAngAtRefPt = 0.0;
				ZoneDaylight( ZoneNum ).SolidAngAtRefPtWtd.allocate( ZoneExtWin( ZoneNum ), ZoneDaylight( ZoneNum ).TotalDaylRefPoints );
				ZoneDaylight( ZoneNum ).SolidAngAtRefPtWtd = 0.0;
				ZoneDaylight( ZoneNum ).IllumFromWinAtRefPt.allocate( ZoneExtWin( ZoneNum ), 2, ZoneDaylight( ZoneNum ).TotalDaylRefPoints );
				ZoneDaylight( ZoneNum ).IllumFromWinAtRefPt = 0.0;
				ZoneDaylight( ZoneNum ).BackLumFromWinAtRefPt.allocate( ZoneExtWin( ZoneNum ), 2, ZoneDaylight( ZoneNum ).TotalDaylRefPoints );
				ZoneDaylight( ZoneNum ).BackLumFromWinAtRefPt = 0.0;
				ZoneDaylight( ZoneNum ).SourceLumFromWinAtRefPt.allocate( ZoneExtWin( ZoneNum ), 2, ZoneDaylight( ZoneNum ).TotalDaylRefPoints );
				ZoneDaylight( ZoneNum ).SourceLumFromWinAtRefPt = 0.0;

				for ( loop = 1; loop <= ZoneDaylight( ZoneNum ).MapCount; ++loop ) {
					MapNum = ZoneDaylight( ZoneNum ).ZoneToMap( loop );

					if ( IllumMapCalc( MapNum ).TotalMapRefPoints > 0 ) {
						// might be able to use TotalMapRefPoints for zone in below.
						IllumMapCalc( MapNum ).SolidAngAtMapPt.allocate( ZoneExtWin( ZoneNum ), IllumMapCalc( MapNum ).TotalMapRefPoints );
						IllumMapCalc( MapNum ).SolidAngAtMapPt = 0.0;
						IllumMapCalc( MapNum ).SolidAngAtMapPtWtd.allocate( ZoneExtWin( ZoneNum ), IllumMapCalc( MapNum ).TotalMapRefPoints );
						IllumMapCalc( MapNum ).SolidAngAtMapPtWtd = 0.0;
						IllumMapCalc( MapNum ).IllumFromWinAtMapPt.allocate( ZoneExtWin( ZoneNum ), 2, IllumMapCalc( MapNum ).TotalMapRefPoints );
						IllumMapCalc( MapNum ).IllumFromWinAtMapPt = 0.0;
						IllumMapCalc( MapNum ).BackLumFromWinAtMapPt.allocate( ZoneExtWin( ZoneNum ), 2, IllumMapCalc( MapNum ).TotalMapRefPoints );
						IllumMapCalc( MapNum ).BackLumFromWinAtMapPt = 0.0;
						IllumMapCalc( MapNum ).SourceLumFromWinAtMapPt.allocate( ZoneExtWin( ZoneNum ), 2, IllumMapCalc( MapNum ).TotalMapRefPoints );
						IllumMapCalc( MapNum ).SourceLumFromWinAtMapPt = 0.0;
					}
				}

				ZoneExtWinCtr = 0;

				for ( SurfNum = Zone( ZoneNum ).SurfaceFirst; SurfNum <= Zone( ZoneNum ).SurfaceLast; ++SurfNum ) {
					if ( ( Surface( SurfNum ).Class == SurfaceClass_Window && Surface( SurfNum ).ExtBoundCond == ExternalEnvironment ) || SurfaceWindow( SurfNum ).OriginalClass == SurfaceClass_TDD_Diffuser ) {
						++ZoneExtWinCtr;
						++DayltgFacPtr;
						ZoneDaylight( ZoneNum ).DayltgExtWinSurfNums( ZoneExtWinCtr ) = SurfNum;
						ZoneDaylight( ZoneNum ).DayltgFacPtrsForExtWins( ZoneExtWinCtr ) = DayltgFacPtr;
					}
				}

				// Get exterior windows in adjacent zones that share interior windows with ZoneNum
				if ( ZoneDaylight( ZoneNum ).NumOfIntWinAdjZones > 0 ) {
					for ( loop = 1; loop <= ZoneDaylight( ZoneNum ).NumOfIntWinAdjZones; ++loop ) {
						ZoneNumAdj = ZoneDaylight( ZoneNum ).AdjIntWinZoneNums( loop );
						// Get exterior windows in ZoneNumAdj -- there must be at least one, otherwise
						// it would not be an "AdjIntWinZone"
						for ( SurfNumAdj = Zone( ZoneNumAdj ).SurfaceFirst; SurfNumAdj <= Zone( ZoneNumAdj ).SurfaceLast; ++SurfNumAdj ) {
							if ( ( Surface( SurfNumAdj ).Class == SurfaceClass_Window && Surface( SurfNumAdj ).ExtBoundCond == ExternalEnvironment ) || SurfaceWindow( SurfNumAdj ).OriginalClass == SurfaceClass_TDD_Diffuser ) {
								++ZoneExtWinCtr;
								++DayltgFacPtr;
								ZoneDaylight( ZoneNum ).DayltgExtWinSurfNums( ZoneExtWinCtr ) = SurfNumAdj;
								ZoneDaylight( ZoneNum ).DayltgFacPtrsForExtWins( ZoneExtWinCtr ) = DayltgFacPtr;

								// If no daylighting in that zone, set up variables anyway:
								if ( ZoneDaylight( ZoneNumAdj ).TotalDaylRefPoints == 0 ) {
									if ( ! SurfaceWindow( SurfNumAdj ).SurfDayLightInit ) {
										SurfaceWindow( SurfNumAdj ).SolidAngAtRefPt.allocate( ZoneDaylight( ZoneNum ).TotalDaylRefPoints );
										SurfaceWindow( SurfNumAdj ).SolidAngAtRefPt = 0.0;
										SurfaceWindow( SurfNumAdj ).SolidAngAtRefPtWtd.allocate( ZoneDaylight( ZoneNum ).TotalDaylRefPoints );
										SurfaceWindow( SurfNumAdj ).SolidAngAtRefPtWtd = 0.0;
										SurfaceWindow( SurfNumAdj ).IllumFromWinAtRefPt.allocate( 2, ZoneDaylight( ZoneNum ).TotalDaylRefPoints );
										SurfaceWindow( SurfNumAdj ).IllumFromWinAtRefPt = 0.0;
										SurfaceWindow( SurfNumAdj ).BackLumFromWinAtRefPt.allocate( 2, ZoneDaylight( ZoneNum ).TotalDaylRefPoints );
										SurfaceWindow( SurfNumAdj ).BackLumFromWinAtRefPt = 0.0;
										SurfaceWindow( SurfNumAdj ).SourceLumFromWinAtRefPt.allocate( 2, ZoneDaylight( ZoneNum ).TotalDaylRefPoints );
										SurfaceWindow( SurfNumAdj ).SourceLumFromWinAtRefPt = 0.0;
										SurfaceWindow( SurfNumAdj ).SurfDayLightInit = true;
									}
								}
							}
						}
					}
				}

				ZoneDaylight( ZoneNum ).NumOfDayltgExtWins = ZoneExtWin( ZoneNum );
				WinSize = ZoneExtWin( ZoneNum );
				RefSize = 2;
				ZoneDaylight( ZoneNum ).DaylIllFacSky.allocate( 24, MaxSlatAngs + 1, 4, RefSize, WinSize );
				ZoneDaylight( ZoneNum ).DaylSourceFacSky.allocate( 24, MaxSlatAngs + 1, 4, RefSize, WinSize );
				ZoneDaylight( ZoneNum ).DaylBackFacSky.allocate( 24, MaxSlatAngs + 1, 4, RefSize, WinSize );
				ZoneDaylight( ZoneNum ).DaylIllFacSun.allocate( 24, MaxSlatAngs + 1, RefSize, WinSize );
				ZoneDaylight( ZoneNum ).DaylIllFacSunDisk.allocate( 24, MaxSlatAngs + 1, RefSize, WinSize );
				ZoneDaylight( ZoneNum ).DaylSourceFacSun.allocate( 24, MaxSlatAngs + 1, RefSize, WinSize );
				ZoneDaylight( ZoneNum ).DaylSourceFacSunDisk.allocate( 24, MaxSlatAngs + 1, RefSize, WinSize );
				ZoneDaylight( ZoneNum ).DaylBackFacSun.allocate( 24, MaxSlatAngs + 1, RefSize, WinSize );
				ZoneDaylight( ZoneNum ).DaylBackFacSunDisk.allocate( 24, MaxSlatAngs + 1, RefSize, WinSize );

				for ( loop = 1; loop <= ZoneDaylight( ZoneNum ).MapCount; ++loop ) {
					MapNum = ZoneDaylight( ZoneNum ).ZoneToMap( loop );
					RefSize = IllumMapCalc( MapNum ).TotalMapRefPoints;
					IllumMapCalc( MapNum ).DaylIllFacSky.allocate( 24, MaxSlatAngs + 1, 4, RefSize, WinSize );
					IllumMapCalc( MapNum ).DaylSourceFacSky.allocate( 24, MaxSlatAngs + 1, 4, RefSize, WinSize );
					IllumMapCalc( MapNum ).DaylBackFacSky.allocate( 24, MaxSlatAngs + 1, 4, RefSize, WinSize );
					IllumMapCalc( MapNum ).DaylIllFacSun.allocate( 24, MaxSlatAngs + 1, RefSize, WinSize );
					IllumMapCalc( MapNum ).DaylIllFacSunDisk.allocate( 24, MaxSlatAngs + 1, RefSize, WinSize );
					IllumMapCalc( MapNum ).DaylSourceFacSun.allocate( 24, MaxSlatAngs + 1, RefSize, WinSize );
					IllumMapCalc( MapNum ).DaylSourceFacSunDisk.allocate( 24, MaxSlatAngs + 1, RefSize, WinSize );
					IllumMapCalc( MapNum ).DaylBackFacSun.allocate( 24, MaxSlatAngs + 1, RefSize, WinSize );
					IllumMapCalc( MapNum ).DaylBackFacSunDisk.allocate( 24, MaxSlatAngs + 1, RefSize, WinSize );
				}

			} // End of check if a Daylighting:Detailed zone
		} // End of primary zone loop

		gio::write( OutputFileInits, Format_700 );
		for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
			if ( ZoneDaylight( ZoneNum ).TotalDaylRefPoints == 0 ) continue;
			gio::write( OutputFileInits, Format_701 ) << Zone( ZoneNum ).Name << RoundSigDigits( ZoneDaylight( ZoneNum ).TotalExtWindows ) << RoundSigDigits( ZoneDaylight( ZoneNum ).NumOfDayltgExtWins - ZoneDaylight( ZoneNum ).TotalExtWindows );
		}

		gio::write( OutputFileInits, Format_702 );
		for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
			if ( ZoneDaylight( ZoneNum ).TotalDaylRefPoints == 0 ) continue;
			gio::write( OutputFileInits, Format_703 ) << Zone( ZoneNum ).Name << RoundSigDigits( ZoneDaylight( ZoneNum ).NumOfIntWinAdjZones );
			for ( int loop = 1, loop_end = min( ZoneDaylight( ZoneNum ).NumOfIntWinAdjZones, 100 ); loop <= loop_end; ++loop ) {
				gio::write( OutputFileInits, fmtCommaA ) << Zone( ZoneDaylight( ZoneNum ).AdjIntWinZoneNums( loop ) ).Name;
			} gio::write( OutputFileInits );
		}

		ZoneExtWin.deallocate();

	}

	void
	DayltgInterReflIllFrIntWins( int & ZoneNum ) // Zone number
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   Mar. 2004
		//       MODIFIED:na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculates the inter-reflected illuminance in a daylit zone from beam
		// and diffuse daylight entering the zone through interior windows. This illuminance
		// is determined by the split-flux method and is assumed to be uniform, i.e., the same
		// at all reference points.

		// METHODOLOGY EMPLOYED:na
		// REFERENCES:na
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
		int IWin; // Window number
		int ConstrNum; // Window construction number
		int AdjZoneNum; // Adjacent zone number
		Real64 QDifTrans; // Luminous flux transmitted through an int win from adjacent zone (lumens)
		Real64 QDifTransUp; // Upgoing part of QDifTrans (lumens)
		Real64 QDifTransDn; // Downgoing part of QDifTrans (lumens)
		Real64 DifInterReflIllThisWin; // Inter-reflected illuminance due to QDifTrans (lux)
		Real64 BmInterReflIll; // Inter-reflected illuminance due to beam solar entering ZoneNum
		//  through its interior windows (lux)
		// FLOW:

		ZoneDaylight( ZoneNum ).InterReflIllFrIntWins = 0.0;

		for ( IWin = Zone( ZoneNum ).SurfaceFirst; IWin <= Zone( ZoneNum ).SurfaceLast; ++IWin ) {
			if ( Surface( IWin ).Class == SurfaceClass_Window && Surface( IWin ).ExtBoundCond >= 1 ) {
				// This is an interior window in ZoneNum
				ConstrNum = Surface( IWin ).Construction;
				AdjZoneNum = Surface( Surface( IWin ).ExtBoundCond ).Zone;
				QDifTrans = QSDifSol( AdjZoneNum ) * Construct( ConstrNum ).TransDiffVis * Surface( IWin ).Area * PDIFLW;
				QDifTransUp = QDifTrans * SurfaceWindow( IWin ).FractionUpgoing;
				QDifTransDn = QDifTrans * ( 1.0 - SurfaceWindow( IWin ).FractionUpgoing );
				if ( ZoneDaylight( ZoneNum ).TotInsSurfArea * ( 1.0 - ZoneDaylight( ZoneNum ).AveVisDiffReflect ) != 0.0 ) {
					DifInterReflIllThisWin = ( QDifTransDn * SurfaceWindow( IWin ).RhoFloorWall + QDifTransUp * SurfaceWindow( IWin ).RhoCeilingWall ) / ( ZoneDaylight( ZoneNum ).TotInsSurfArea * ( 1.0 - ZoneDaylight( ZoneNum ).AveVisDiffReflect ) );
				} else {
					DifInterReflIllThisWin = 0.0;
				}
				ZoneDaylight( ZoneNum ).InterReflIllFrIntWins += DifInterReflIllThisWin;
			}
		}

		// Add inter-reflected illuminance from beam solar entering ZoneNum through interior windows
		// TH, CR 7873, 9/17/2009
		BmInterReflIll = 0.0;
		if ( ZoneDaylight( ZoneNum ).TotInsSurfArea > 0 ) {
			BmInterReflIll = ( DBZoneIntWin( ZoneNum ) * BeamSolarRad * PDIRLW * ZoneDaylight( ZoneNum ).FloorVisRefl ) / ( ZoneDaylight( ZoneNum ).TotInsSurfArea * ( 1.0 - ZoneDaylight( ZoneNum ).AveVisDiffReflect ) );
		}

		ZoneDaylight( ZoneNum ).InterReflIllFrIntWins += BmInterReflIll;

	}

	void
	CalcMinIntWinSolidAngs()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Fred Winkelmann
		//       DATE WRITTEN   Feb. 2004
		//       MODIFIED:na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// For each Daylighting:Detailed zone finds the minimum solid angle subtended
		// by interior windows through which daylight can pass from adjacent zones with
		// exterior windows.

		// METHODOLOGY EMPLOYED:na
		// REFERENCES:na
		// Using/Aliasing
		using namespace Vectors;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS: na
		// SUBROUTINE PARAMETER DEFINITIONS: na
		// INTERFACE BLOCK SPECIFICATIONS: na
		// DERIVED TYPE DEFINITIONS: na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		int ZoneNum; // Zone number
		int ZoneNumAdj; // Adjacent zone number
		int IWin; // Window surface number
		int IL; // Reference point number
		int loop; // DO loop index
		bool is_Triangle; // True if window is a triangle
		bool is_Rectangle; // True if window is a rectangle
		bool IntWinNextToIntWinAdjZone; // True if an interior window is next to a zone with
		// one or more exterior windows
		Real64 IntWinSolidAng; // Approximation to solid angle subtended by an interior window
		// from a point a distance SQRT(zone floor area) away.
		static Vector3< Real64 > W1; // Window vertices
		static Vector3< Real64 > W2;
		static Vector3< Real64 > W3;
		static Vector3< Real64 > WC; // Center point of window
		static Vector3< Real64 > W21; // Unit vectors from window vertex 2 to 1 and 2 to 3
		static Vector3< Real64 > W23;
		Real64 HW; // Window height and width (m)
		Real64 WW;
		static Vector3< Real64 > RREF; // Location of a reference point in absolute coordinate system
		static Vector3< Real64 > Ray; // Unit vector along ray from reference point to window center
		static Vector3< Real64 > REFWC; // Vector from reference point to center of window
		static Vector3< Real64 > WNORM; // Unit vector normal to window (pointing away from room)
		Real64 DIS; // Distance from ref point to window center (m)
		Real64 COSB; // Cosine of angle between ray from ref pt to center of window
		//  and window outward normal

		// FLOW:

		for ( ZoneNum = 1; ZoneNum <= NumOfZones; ++ZoneNum ) {
			ZoneDaylight( ZoneNum ).MinIntWinSolidAng = 2.0 * Pi;
			if ( ZoneDaylight( ZoneNum ).TotalDaylRefPoints == 0 ) continue;
			if ( ZoneDaylight( ZoneNum ).NumOfIntWinAdjZones == 0 ) continue;
			for ( IWin = Zone( ZoneNum ).SurfaceFirst; IWin <= Zone( ZoneNum ).SurfaceLast; ++IWin ) {
				if ( Surface( IWin ).Class == SurfaceClass_Window && Surface( IWin ).ExtBoundCond >= 1 ) {
					ZoneNumAdj = Surface( Surface( IWin ).ExtBoundCond ).Zone;
					IntWinNextToIntWinAdjZone = false;
					for ( loop = 1; loop <= ZoneDaylight( ZoneNum ).NumOfIntWinAdjZones; ++loop ) {
						if ( ZoneNumAdj == ZoneDaylight( ZoneNum ).AdjIntWinZoneNums( loop ) ) {
							IntWinNextToIntWinAdjZone = true;
							break;
						}
					}
					if ( IntWinNextToIntWinAdjZone ) {
						for ( IL = 1; IL <= ZoneDaylight( ZoneNum ).TotalDaylRefPoints; ++IL ) {
							// Reference point in absolute coordinate system
							RREF = ZoneDaylight( ZoneNum ).DaylRefPtAbsCoord( {1,3}, IL );
							is_Triangle = ( Surface( IWin ).Sides == 3 );
							is_Rectangle = ( Surface( IWin ).Sides == 4 );
							if ( is_Rectangle ) {
								// Vertices of window numbered counter-clockwise starting at upper left as viewed
								// from inside of room. Assumes original vertices are numbered counter-clockwise from
								// upper left as viewed from outside.
								W3 = Surface( IWin ).Vertex( 2 );
								W2 = Surface( IWin ).Vertex( 3 );
								W1 = Surface( IWin ).Vertex( 4 );
							} else if ( is_Triangle ) {
								W3 = Surface( IWin ).Vertex( 2 );
								W2 = Surface( IWin ).Vertex( 3 );
								W1 = Surface( IWin ).Vertex( 1 );
							}
							// Unit vectors from window vertex 2 to 1 and 2 to 3, center point of window,
							// and vector from ref pt to center of window
							W21 = W1 - W2;
							W23 = W3 - W2;
							HW = W21.magnitude();
							WW = W23.magnitude();
							if ( is_Rectangle ) {
								WC = W2 + ( W23 + W21 ) / 2.0;
							} else if ( is_Triangle ) {
								WC = W2 + ( W23 + W21 ) / 3.0;
							}
							// Vector from ref point to center of window
							REFWC = WC - RREF;
							W21 /= HW;
							W23 /= WW;
							// Unit vector normal to window (pointing away from room)
							WNORM = Surface( IWin ).OutNormVec;
							// Distance from ref point to center of window
							DIS =  REFWC.magnitude();
							// Unit vector from ref point to center of window
							Ray = REFWC / DIS;
							// Cosine of angle between ray from ref pt to center of window and window outward normal
							COSB = dot( WNORM, Ray );
							if ( COSB > 0.01765 ) { // 0 <= B < 89 deg
								// Above test avoids case where ref point cannot receive daylight directly from the
								// interior window
								IntWinSolidAng = COSB * Surface( IWin ).Area / ( pow_2( DIS ) + 0.001 );
								ZoneDaylight( ZoneNum ).MinIntWinSolidAng = min( ZoneDaylight( ZoneNum ).MinIntWinSolidAng, IntWinSolidAng );
							}
						} // End of loop over reference points
					}
				}
			} // End of loop over surfaces in zone
		} // End of loop over zones

	}

	void
	CheckForGeometricTransform(
		bool & doTransform,
		Real64 & OldAspectRatio,
		Real64 & NewAspectRatio
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   February 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// check for geometrytransform in the daylighting access for reference and map points

		// METHODOLOGY EMPLOYED:
		// once reference points  have been converted to WCS,
		//  change them to reflect a different aspect
		// ratio for the entire building based on user input.

		// REFERENCES:
		// na

		// Using/Aliasing
		using namespace DataIPShortCuts;
		using namespace InputProcessor;
		using DataDaylighting::ZoneDaylight;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string const CurrentModuleObject( "GeometryTransform" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static Array1D_string cAlphas( 1 );
		static Array1D< Real64 > rNumerics;
		int NAlphas;
		int NNum;
		int IOStat;
		std::string transformPlane;

		//begin execution
		//get user input...
		doTransform = false;
		OldAspectRatio = 1.0;
		NewAspectRatio = 1.0;

		if ( GetNumObjectsFound( CurrentModuleObject ) == 1 ) {
			GetObjectItem( CurrentModuleObject, 1, cAlphas, NAlphas, rNumerics, NNum, IOStat, lNumericFieldBlanks, lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames );
			OldAspectRatio = rNumerics( 1 );
			NewAspectRatio = rNumerics( 2 );
			transformPlane = cAlphas( 1 );
			if ( transformPlane != "XY" ) {
				ShowWarningError( CurrentModuleObject + ": invalid " + cAlphaFieldNames( 1 ) + "=\"" + cAlphas( 1 ) + "...ignored." );
			}
			doTransform = true;
			AspectTransform = true;
		}
		if ( WorldCoordSystem ) {
			doTransform = false;
			AspectTransform = false;
		}

	}

	void
	WriteDaylightMapTitle(
		int const mapNum,
		int const unitNo,
		std::string const & mapName,
		std::string const & environmentName,
		int const ZoneNum,
		std::string const & refPt1,
		std::string const & refPt2,
		Real64 const zcoord
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Greg Stark
		//       DATE WRITTEN   Sept 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// The purpose of the routine is to allow the daylighting map data to be written in various formats

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static gio::Fmt FmtA( "(A)" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		std::string fullmapName; // for output to map units as well as SQL

		// must add correct number of commas at end
		fullmapName = Zone( ZoneNum ).Name + ':' + environmentName + ':' + mapName + " Illuminance [lux] (Hourly)";
		gio::write( unitNo, FmtA ) << "Date/Time," + fullmapName + MapColSep + refPt1 + MapColSep + refPt2 + MapColSep + MapColSep;

		if ( sqlite ) {
			sqlite->createSQLiteDaylightMapTitle( mapNum, fullmapName, environmentName, ZoneNum, refPt1, refPt2, zcoord );
		}

	}

} // DaylightingManager

} // EnergyPlus
