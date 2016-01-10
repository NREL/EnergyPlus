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

#ifndef DataDaylighting_hh_INCLUDED
#define DataDaylighting_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array2D.hh>
#include <ObjexxFCL/Array3D.hh>
#include <ObjexxFCL/Array4D.hh>
#include <ObjexxFCL/Array5D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace DataDaylighting {

	// Using/Aliasing

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:
	// Two kinds of reference points: used directly in daylighting, used to show illuminance map of zone
	extern int const MaxRefPoints; // Maximum number of daylighting reference points, 2
	extern int const MaxMapRefPoints; // Maximum number of Illuminance Map Ref Points

	extern int const NotInOrAdjZoneExtWin; // Exterior window is not in a Daylighting:Detailed zone
	// or in an adjacent zone with a shared interior window
	extern int const InZoneExtWin; // Exterior window is in a Daylighting:Detailed zone
	extern int const AdjZoneExtWin; // Exterior window is in a zone adjacent to a Daylighting:
	// Detailed zone with which it shares an interior window

	extern int const CalledForRefPoint;
	extern int const CalledForMapPoint;

	// Parameters for "DaylightType"
	extern int const NoDaylighting;
	extern int const DetailedDaylighting;
	extern int const DElightDaylighting;
	extern Array1D_string const DaylightTypes;

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE TYPE DECLARATIONS:

	// INTERFACE BLOCK SPECIFICATIONS: na

	// MODULE VARIABLE DECLARATIONS:
	extern int TotIllumMaps;
	extern bool mapResultsToReport; // used when only partial hour has "sun up"
	extern bool mapResultsReported; // when no map results are ever reported this will still be false
	extern char MapColSep; // Character for separating map columns (tab, space, comma)

	extern bool DFSReportSizingDays;
	extern bool DFSReportAllShadowCalculationDays;

	// Types

	struct IntWinAdjZoneExtWinStruct // nested structure for ZoneDaylight
	{
		// Members
		int SurfNum; // exterior window index
		int NumOfIntWindows; // count of interior windows associated with this ext win
		Array1D_int IntWinNum; // index numbers for interior windows assoc with this ext win

		// Default Constructor
		IntWinAdjZoneExtWinStruct() :
			SurfNum( 0 ),
			NumOfIntWindows( 0 )
		{}

	};

	struct ZoneDaylightCalc
	{
		// Members
		int DaylightType; // Type of Daylighting (1=Detailed, 2=DElight)
		int AvailSchedNum; // pointer to availability schedule if present
		int TotalDaylRefPoints; // Number of detailed daylighting reference points in a zone (0,1 or 2)
		int TotalDElightRefPts; // Number of DElight daylighting reference points in a zone (0,1 or 2) - RJH
		Array2D< Real64 > DaylRefPtAbsCoord; // =0.0 ! X,Y,Z coordinates of all daylighting reference points
		// in absolute coordinate system (m)
		// Points 1 and 2 are the control reference points
		Array1D_bool DaylRefPtInBounds; // True when coordinates are in bounds of zone coordinates
		Array1D< Real64 > FracZoneDaylit; // =0.0  ! Fraction of zone controlled by each reference point
		Array1D< Real64 > IllumSetPoint; // =0.0  ! Illuminance setpoint at each reference point (lux)
		int LightControlType; // Lighting control type (same for all reference points)
		// (1=continuous, 2=stepped, 3=continuous/off)
		Real64 ViewAzimuthForGlare; // View direction relative to window for glare calculation (deg)
		int MaxGlareallowed; // Maximum allowable discomfort glare index
		Real64 MinPowerFraction; // Minimum fraction of power input that continuous dimming system can dim down to
		Real64 MinLightFraction; // Minimum fraction of light output that continuous dimming system can dim down to
		int LightControlSteps; // Number of levels (excluding zero) of stepped control system
		Real64 LightControlProbability; // For manual control of stepped systems, probability that lighting will
		int TotalExtWindows; // Total number of exterior windows in the zone
		Real64 AveVisDiffReflect; // Area-weighted average inside surface visible reflectance of zone
		Array1D< Real64 > RefPtPowerReductionFactor; // =1.0  ! Electric power reduction factor at reference points
		// due to daylighting
		Real64 ZonePowerReductionFactor; // Electric power reduction factor for entire zone due to daylighting
		Array1D< Real64 > DaylIllumAtRefPt; // =0.0 ! Daylight illuminance at reference points (lux)
		Array1D< Real64 > GlareIndexAtRefPt; // =0.0 ! Glare index at reference points
		Array1D_int AdjIntWinZoneNums; // List of zone numbers of adjacent zones that have exterior windows and
		// share one or more interior windows with target zone
		int NumOfIntWinAdjZones; // Number of adjacent zones that have exterior windows and share one or
		// more interior windows with target zone
		int NumOfIntWinAdjZoneExtWins; // number of exterior windows associated with zone via interior windows
		Array1D< IntWinAdjZoneExtWinStruct > IntWinAdjZoneExtWin; // nested structure | info about exterior window associated with zone via interior window
		int NumOfDayltgExtWins; // Number of associated exterior windows providing daylight to this zone
		Array1D_int DayltgExtWinSurfNums; // List of surface numbers of zone's exterior windows or
		// exterior windows in adjacent zones sharing interior windows with the zone
		Array1D_int DayltgFacPtrsForExtWins; // Zone's daylighting factor pointers.
		// Entries in this list have a one-to-one
		// correspondence with the DayltgExtWinSurfNums list
		Real64 MinIntWinSolidAng; // Minimum solid angle subtended by an interior window in a zone
		Real64 TotInsSurfArea; // Total inside surface area of a daylit zone (m2)
		Real64 FloorVisRefl; // Area-weighted visible reflectance of floor of a daylit zone
		Real64 InterReflIllFrIntWins; // Inter-reflected illuminance due to beam and diffuse solar passing
		//  through a zone's interior windows (lux)
		Array1D< Real64 > BacLum; // =0.0 ! Background luminance at each reference point (cd/m2)
		Array2D< Real64 > SolidAngAtRefPt; // (MaxRefPoints,50)
		Array2D< Real64 > SolidAngAtRefPtWtd; // (MaxRefPoints,50)
		Array3D< Real64 > IllumFromWinAtRefPt; // (MaxRefPoints,2,50)
		Array3D< Real64 > BackLumFromWinAtRefPt; // (MaxRefPoints,2,50)
		Array3D< Real64 > SourceLumFromWinAtRefPt; // (MaxRefPoints,2,50)
		// Allocatable daylight factor arrays
		// Arguments for Dayl---Sky are:
		//  1: Daylit window number (1 to NumOfDayltgExtWins)
		//  2: Reference point number (1 to MaxRefPoints)
		//  3: Sky type (1 to 4; 1 = clear, 2 = clear turbid, 3 = intermediate, 4 = overcast
		//  4: Shading index (1 to MaxSlatAngs+1; 1 = bare window; 2 = with shade, or, if blinds
		//      2 = first slat position, 3 = second position, ..., MaxSlatAngs+1 = last position)
		//  5: Sun position index (1 to 24)
		Array5D< Real64 > DaylIllFacSky;
		Array5D< Real64 > DaylSourceFacSky;
		Array5D< Real64 > DaylBackFacSky;
		// Arguments for Dayl---Sun are:
		//  1: Daylit window number (1 to NumOfDayltgExtWins)
		//  2: Reference point number (1 to MaxRefPoints)
		//  3: Shading index (1 to MaxShadeIndex; 1 = no shade; 2 = with shade, or, if blinds
		//      2 = first slat position, 3 = second position, ..., MaxSlatAngs+1 = last position)
		//  4: Sun position index (1 to 24)
		Array4D< Real64 > DaylIllFacSun;
		Array4D< Real64 > DaylIllFacSunDisk;
		Array4D< Real64 > DaylSourceFacSun;
		Array4D< Real64 > DaylSourceFacSunDisk;
		Array4D< Real64 > DaylBackFacSun;
		Array4D< Real64 > DaylBackFacSunDisk;
		// Time exceeding maximum allowable discomfort glare index at reference points (hours)
		Array1D< Real64 > TimeExceedingGlareIndexSPAtRefPt;
		// Time exceeding daylight illuminance setpoint at reference points (hours)
		Array1D< Real64 > TimeExceedingDaylightIlluminanceSPAtRefPt;
		// True if at least one adjacent zone, sharing one or more interior windows, has daylighting control
		bool AdjZoneHasDayltgCtrl;
		int MapCount; // Number of maps assigned to Zone
		Array1D_int ZoneToMap; // Pointers to maps allocated to Zone

		// Default Constructor
		ZoneDaylightCalc() :
			DaylightType( 0 ),
			AvailSchedNum( 0 ),
			TotalDaylRefPoints( 0 ),
			TotalDElightRefPts( 0 ),
			LightControlType( 1 ),
			ViewAzimuthForGlare( 0.0 ),
			MaxGlareallowed( 0 ),
			MinPowerFraction( 0.0 ),
			MinLightFraction( 0.0 ),
			LightControlSteps( 0 ),
			LightControlProbability( 0.0 ),
			TotalExtWindows( 0 ),
			AveVisDiffReflect( 0.0 ),
			ZonePowerReductionFactor( 1.0 ),
			NumOfIntWinAdjZones( 0 ),
			NumOfIntWinAdjZoneExtWins( 0 ),
			NumOfDayltgExtWins( 0 ),
			MinIntWinSolidAng( 0.0 ),
			TotInsSurfArea( 0.0 ),
			FloorVisRefl( 0.0 ),
			InterReflIllFrIntWins( 0.0 ),
			AdjZoneHasDayltgCtrl( false ),
			MapCount( 0 )
		{}

	};

	struct IllumMapData
	{
		// Members
		std::string Name; // Map name
		int Zone; // Pointer to zone being mapped
		Real64 Z; // Elevation or height
		Real64 Xmin; // Minimum X value
		Real64 Xmax; // Maximum X value
		int Xnum; // Number of X reference points (going N-S)
		Real64 Xinc; // Increment between X reference points
		Real64 Ymin; // Minimum Y value
		Real64 Ymax; // Maximum Y value
		int Ynum; // Number of Y reference points (going E-W)
		Real64 Yinc; // Increment between Y reference points
		int UnitNo; // Unit number for map output (later merged to final file)
		bool HeaderXLineLengthNeeded; // X header will likely be the longest line in the file
		int HeaderXLineLength; // actual length of this X header line

		// Default Constructor
		IllumMapData() :
			Zone( 0 ),
			Z( 0.0 ),
			Xmin( 0.0 ),
			Xmax( 0.0 ),
			Xnum( 0 ),
			Xinc( 0.0 ),
			Ymin( 0.0 ),
			Ymax( 0.0 ),
			Ynum( 0 ),
			Yinc( 0.0 ),
			UnitNo( 0 ),
			HeaderXLineLengthNeeded( true ),
			HeaderXLineLength( 0 )
		{}

	};

	struct MapCalcData
	{
		// Members
		int TotalMapRefPoints; // Number of illuminance map reference points in this zone (up to 100)
		int Zone; // Pointer to zone being mapped
		Array2D< Real64 > MapRefPtAbsCoord; // X,Y,Z coordinates of all illuminance map reference points
		// in absolute coordinate system (m)
		// Points 1 and 2 are the control reference points
		Array1D_bool MapRefPtInBounds; // True when coordinates are in bounds of zone coordinates
		Array1D< Real64 > DaylIllumAtMapPt; // Daylight illuminance at illuminance map points (lux)
		Array1D< Real64 > GlareIndexAtMapPt; // Glare index at illuminance map points
		// following Hr - report avg hr
		Array1D< Real64 > DaylIllumAtMapPtHr; // Daylight illuminance at illuminance map points (lux)
		Array1D< Real64 > GlareIndexAtMapPtHr; // Glare index at illuminance map points
		Array2D< Real64 > SolidAngAtMapPt; // (MaxRefPoints,50)
		Array2D< Real64 > SolidAngAtMapPtWtd; // (MaxRefPoints,50)
		Array3D< Real64 > IllumFromWinAtMapPt; // (MaxRefPoints,2,50)
		Array3D< Real64 > BackLumFromWinAtMapPt; // (MaxRefPoints,2,50)
		Array3D< Real64 > SourceLumFromWinAtMapPt; // (MaxRefPoints,2,50)
		//  1: Daylit window number (1 to NumOfDayltgExtWins)
		//  2: Reference point number (1 to MaxRefPoints)
		//  3: Sky type (1 to 4; 1 = clear, 2 = clear turbid, 3 = intermediate, 4 = overcast
		//  4: Shading index (1 to MaxSlatAngs+1; 1 = bare window; 2 = with shade, or, if blinds
		//      2 = first slat position, 3 = second position, ..., MaxSlatAngs+1 = last position)
		//  5: Sun position index (1 to 24)
		Array5D< Real64 > DaylIllFacSky;
		Array5D< Real64 > DaylSourceFacSky;
		Array5D< Real64 > DaylBackFacSky;
		// Arguments for Dayl---Sun are:
		//  1: Daylit window number (1 to NumOfDayltgExtWins)
		//  2: Reference point number (1 to MaxRefPoints)
		//  3: Shading index (1 to MaxShadeIndex; 1 = no shade; 2 = with shade, or, if blinds
		//      2 = first slat position, 3 = second position, ..., MaxSlatAngs+1 = last position)
		//  4: Sun position index (1 to 24)
		Array4D< Real64 > DaylIllFacSun;
		Array4D< Real64 > DaylIllFacSunDisk;
		Array4D< Real64 > DaylSourceFacSun;
		Array4D< Real64 > DaylSourceFacSunDisk;
		Array4D< Real64 > DaylBackFacSun;
		Array4D< Real64 > DaylBackFacSunDisk;

		// Default Constructor
		MapCalcData() :
			TotalMapRefPoints( 0 ),
			Zone( 0 )
		{}

	};

	// Object Data
	extern Array1D< ZoneDaylightCalc > ZoneDaylight;
	extern Array1D< IllumMapData > IllumMap;
	extern Array1D< MapCalcData > IllumMapCalc;

} // DataDaylighting

} // EnergyPlus

#endif
