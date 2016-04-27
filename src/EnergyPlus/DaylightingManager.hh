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

#ifndef DaylightingManager_hh_INCLUDED
#define DaylightingManager_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1A.hh>
#include <ObjexxFCL/Array2A.hh>
#include <ObjexxFCL/Array2S.hh>
#include <ObjexxFCL/Array3D.hh>
#include <ObjexxFCL/Optional.hh>
#include <ObjexxFCL/Vector3.fwd.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataBSDFWindow.hh>

namespace EnergyPlus {

namespace DaylightingManager {

	// Using/Aliasing
	using DataBSDFWindow::BSDFRefPoints;
	using DataBSDFWindow::BSDFRefPointsGeomDescr;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	extern int const octreeCrossover; // Surface count crossover for switching to octree algorithm

	// MODULE VARIABLE DECLARATIONS:
	extern int TotWindowsWithDayl; // Total number of exterior windows in all daylit zones
	extern int OutputFileDFS; // Unit number for daylight factors
	extern Array1D< Real64 > DaylIllum; // Daylight illuminance at reference points (lux)
	extern Real64 PHSUN; // Solar altitude (radians)
	extern Real64 SPHSUN; // Sine of solar altitude
	extern Real64 CPHSUN; // Cosine of solar altitude
	extern Real64 THSUN; // Solar azimuth (rad) in Absolute Coordinate System (azimuth=0 along east)
	extern Array1D< Real64 > PHSUNHR; // Hourly values of PHSUN
	extern Array1D< Real64 > SPHSUNHR; // Hourly values of the sine of PHSUN
	extern Array1D< Real64 > CPHSUNHR; // Hourly values of the cosine of PHSUN
	extern Array1D< Real64 > THSUNHR; // Hourly values of THSUN

	// In the following I,J,K arrays:
	// I = 1 for clear sky, 2 for clear turbid, 3 for intermediate, 4 for overcast;
	// J = 1 for bare window, 2 - 12 for shaded;
	// K = sun position index.
	extern Array3D< Real64 > EINTSK; // Sky-related portion of internally reflected illuminance
	extern Array2D< Real64 > EINTSU; // Sun-related portion of internally reflected illuminance,
	// excluding entering beam
	extern Array2D< Real64 > EINTSUdisk; // Sun-related portion of internally reflected illuminance
	// due to entering beam
	extern Array3D< Real64 > WLUMSK; // Sky-related window luminance
	extern Array2D< Real64 > WLUMSU; // Sun-related window luminance, excluding view of solar disk
	extern Array2D< Real64 > WLUMSUdisk; // Sun-related window luminance, due to view of solar disk

	extern Array2D< Real64 > GILSK; // Horizontal illuminance from sky, by sky type, for each hour of the day
	extern Array1D< Real64 > GILSU; // Horizontal illuminance from sun for each hour of the day

	extern Array3D< Real64 > EDIRSK; // Sky-related component of direct illuminance
	extern Array2D< Real64 > EDIRSU; // Sun-related component of direct illuminance (excluding beam solar at ref pt)
	extern Array2D< Real64 > EDIRSUdisk; // Sun-related component of direct illuminance due to beam solar at ref pt
	extern Array3D< Real64 > AVWLSK; // Sky-related average window luminance
	extern Array2D< Real64 > AVWLSU; // Sun-related average window luminance, excluding view of solar disk
	extern Array2D< Real64 > AVWLSUdisk; // Sun-related average window luminance due to view of solar disk

	// Allocatable daylight factor arrays  -- are in the ZoneDaylight Structure

	extern Array2D< Real64 > TDDTransVisBeam;
	extern Array3D< Real64 > TDDFluxInc;
	extern Array3D< Real64 > TDDFluxTrans;

	extern Array2D_int MapErrIndex;
	extern Array2D_int RefErrIndex;

	extern Array1D_bool CheckTDDZone;

	extern std::string mapLine; // character variable to hold map outputs

	// Functions

	void
	DayltgAveInteriorReflectance( int & ZoneNum ); // Zone number

	void
	CalcDayltgCoefficients();

	void
	CalcDayltgCoeffsRefMapPoints( int const ZoneNum );

	void
	CalcDayltgCoeffsRefPoints( int const ZoneNum );

	void
	CalcDayltgCoeffsMapPoints( int const ZoneNum );

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
		bool & Rectangle, // True if window is rectangular
		bool & Triangle, // True if window is triangular
		Optional_int_const MapNum = _,
//		Optional< Real64 > MapWindowSolidAngAtRefPt = _, //Inactive
		Optional< Real64 > MapWindowSolidAngAtRefPtWtd = _
	);

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
		bool const Triangle,
		Real64 & TVISIntWin, // Visible transmittance of int win at COSBIntWin for light from ext win
		Real64 & TVISIntWinDisk, // Visible transmittance of int win at COSBIntWin for sun
		Optional_int_const MapNum = _,
//		Optional< Real64 > MapWindowSolidAngAtRefPt = _, //Inactive
		Optional< Real64 > MapWindowSolidAngAtRefPtWtd = _
	);

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
		Optional_int_const MapNum = _
	);

	void
	InitializeCFSStateData(
		BSDFRefPoints & StateRefPoint,
		BSDFRefPointsGeomDescr & DaylghtGeomDescr,
		int const ZoneNum, // Current zone number
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
		int const CalledFrom,
		Optional_int_const MapNum = _
	);

	void
	AllocateForCFSRefPointsState(
		BSDFRefPoints & StateRefPoint,
		int const NumOfWinEl,
		int const NBasis,
		int const NTrnBasis
	);

	void
	AllocateForCFSRefPointsGeometry(
		BSDFRefPointsGeomDescr & RefPointsGeomDescr,
		int const NumOfWinEl
	);

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
	);

	void
	CFSRefPointPosFactor(
		Vector3< Real64 > const & RefPoint,
		BSDFRefPoints & RefPointMap,
		int const iWin,
		int const CurFenState,
		int const NTrnBasis,
		Real64 const AZVIEW
	);

	Real64
	CalcObstrMultiplier(
		Vector3< Real64 > const & GroundHitPt, // Coordinates of point that ray hits ground (m)
		int const AltSteps, // Number of steps in altitude angle for solar reflection calc
		int const AzimSteps // Number of steps in azimuth angle of solar reflection calc
	);

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
		Optional_int_const MapNum = _,
		Optional< Real64 const > MapWindowSolidAngAtRefPtWtd = _
	);

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
	);

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
	);

	void
	GetDaylightingParametersInput();

	void
	GetDaylightingParametersDetaild(
		int const TotDaylightingDetailed, // Total "simple" daylighting inputs
		bool & ErrorsFound
	);

	void
	CheckTDDsAndLightShelvesInDaylitZones();

	void
	GetLightWellData( bool & ErrorsFound ); // If errors found in input

	void
	DayltgGlare(
		int & IL, // Reference point index: 1=first ref pt, 2=second ref pt
		Real64 & BLUM, // Window background (surround) luminance (cd/m2)
		Real64 & GLINDX, // Glare index
		int & ZoneNum // Zone number
	);

	void
	DayltgGlareWithIntWins(
		Array1< Real64 > & GLINDX, // Glare index
		int const ZoneNum // Zone number
	);

	void
	DayltgExtHorizIllum(
		Array1A< Real64 > HISK, // Horizontal illuminance from sky for different sky types
		Real64 & HISU // Horizontal illuminance from sun for unit beam normal
	);

	void
	DayltgHitObstruction(
		int const IHOUR, // Hour number
		int const IWin, // Window index
		Vector3< Real64 > const & R1, // Origin of ray (m)
		Vector3< Real64 > const & RN, // Unit vector along ray
		Real64 & ObTrans // Product of solar transmittances of exterior obstructions
	);

	void
	DayltgHitInteriorObstruction(
		int const IWin, // Window index
		Vector3< Real64 > const & R1, // Origin of ray (m)
		Vector3< Real64 > const & R2, // Destination of ray (m)
		bool & hit // True iff ray hits an obstruction
	);

	void
	DayltgHitBetWinObstruction(
		int const IWin1, // Surface number of origin window
		int const IWin2, // Surface number of destination window
		Vector3< Real64 > const & R1, // Origin of ray (on IWin1) (m)
		Vector3< Real64 > const & R2, // Destination of ray (on IWin2) (m)
		bool & hit // True iff ray hits an obstruction
	);

	void
	DayltgInteriorIllum( int & ZoneNum ); // Zone number

	void
	DayltgInteriorTDDIllum();

	void
	DayltgElecLightingControl( int & ZoneNum ); // Zone number

	Real64
	DayltgGlarePositionFactor(
		Real64 & X, // Lateral and vertical distance of luminous window element from
		Real64 & Y
	);

	void
	DayltgInterReflectedIllum(
		int const ISunPos, // Sun position counter; used to avoid calculating various
		int const IHR, // Hour of day
		int const ZoneNum, // Zone number
		int const IWin // Window index
	);

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
		Optional_int_const MapNum = _
	);

	void
	DayltgInterReflectedIllumComplexFenestration(
		int const IWin, // Window index
		int const WinEl, // Current window element counter
		int const IHR, // Hour of day
		int const ZoneNum, // Zone number
		int const iRefPoint, // reference point counter
		int const CalledFrom,
		Optional_int_const MapNum = _
	);

	void
	DayltgDirectIllumComplexFenestration(
		int const IWin, // Window index
		int const WinEl, // Current window element counter
		int const IHR, // Hour of day
		int const ZoneNum, // Zone number
		int const iRefPoint, // reference point index
		int const CalledFrom,
		Optional_int_const MapNum = _
	);

	void
	DayltgDirectSunDiskComplexFenestration(
		int const iWin, // Window index
		int const ZoneNum, // Zone number
		int const iHour, // Hour of day
		int const iRefPoint,
		int const NumEl, // Total number of window elements
		Real64 const AZVIEW, // Azimuth of view vector in absolute coord system for
		int const CalledFrom, // indicate  which type of routine called this routine
		Optional_int_const MapNum = _,
		Optional< Real64 const > MapWindowSolidAngAtRefPtWtd = _
	);

	Real64
	DayltgSkyLuminance(
		int const ISky, // Sky type: 1=clear, 2=clear turbid, 3=intermediate, 4=overcast
		Real64 const THSKY, // Azimuth and altitude of sky element (radians)
		Real64 const PHSKY
	);

	void
	ProfileAngle(
		int const SurfNum, // Surface number
		Vector3< Real64 > const & CosDirSun, // Solar direction cosines
		int const HorOrVert, // If HORIZONTAL, calculates ProfileAngHor
		Real64 & ProfileAng // Solar profile angle (radians).
	);

	void
	DayltgClosestObstruction(
		Vector3< Real64 > const & RecPt, // Point on window from which ray emanates (m)
		Vector3< Real64 > const & RayVec, // Unit vector along ray pointing away from window (m)
		int & NearestHitSurfNum, // Surface number of nearest obstruction that is hit by ray;
		Vector3< Real64 > & NearestHitPt // Ray's hit point on nearest obstruction (m)
	);

	void
	DayltgSurfaceLumFromSun(
		int const IHR, // Hour number
		Vector3< Real64 > const & Ray, // Ray from window to reflecting surface (m)
		int const ReflSurfNum, // Number of surface for which luminance is being calculated
		Vector3< Real64 > const & ReflHitPt, // Point on ReflSurfNum for luminance calculation (m)
		Real64 & LumAtReflHitPtFrSun // Luminance at ReflHitPt from beam solar reflection for unit
	);

	void
	DayltgInteriorMapIllum( int & ZoneNum ); // Zone number

	void
	ReportIllumMap( int const MapNum );

	void
	CloseReportIllumMaps();

	void
	CloseDFSFile();

	void
	DayltgSetupAdjZoneListsAndPointers();

	void
	DayltgInterReflIllFrIntWins( int & ZoneNum ); // Zone number

	void
	CalcMinIntWinSolidAngs();

	void
	CheckForGeometricTransform(
		bool & doTransform,
		Real64 & OldAspectRatio,
		Real64 & NewAspectRatio
	);

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
	);

} // DaylightingManager

} // EnergyPlus

#endif
