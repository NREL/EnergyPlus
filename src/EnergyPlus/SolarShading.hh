#ifndef SolarShading_hh_INCLUDED
#define SolarShading_hh_INCLUDED

// C++ Headers
#include <fstream>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1A.hh>
#include <ObjexxFCL/Array2D.hh>
#include <ObjexxFCL/Array3D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataBSDFWindow.hh>
#include <DataVectorTypes.hh>

namespace EnergyPlus {

namespace SolarShading {

	// Using/Aliasing
	using DataBSDFWindow::BSDFGeomDescr;
	using DataBSDFWindow::BSDFWindowGeomDescr;
	using DataVectorTypes::Vector;

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// General Parameters...
	extern Real64 const SmallIncrement; // Small increment added for shading/sunlit area calculations.
	extern Real64 const HCMULT; // Multiplier used to change meters to .01 millimeters for homogeneous coordinates.
	// Homogeneous Coordinates are represented in integers (64 bit). This changes the surface coordinates from meters
	// to .01 millimeters -- making that the resolution for shadowing, polygon clipping, etc.
	extern Real64 const sqHCMULT; // Square of HCMult used in Homogeneous coordinates
	extern Real64 const sqHCMULT_fac; // ( 0.5 / sqHCMULT ) factor
	extern Real64 const kHCMULT; // half of inverse square of HCMult used in Homogeneous coordinates

	// Parameters for use with the variable OverlapStatus...
	extern int const NoOverlap;
	extern int const FirstSurfWithinSecond;
	extern int const SecondSurfWithinFirst;
	extern int const PartialOverlap;
	extern int const TooManyVertices;
	extern int const TooManyFigures;
	extern Array1D_string const cOverLapStatus;

	// DERIVED TYPE DEFINITIONS:
	// INTERFACE BLOCK SPECIFICATIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:
	extern int MaxHCV; // Maximum number of HC vertices
	// (needs to be based on maxnumvertices)
	extern int MaxHCS; // 200      ! Maximum number of HC surfaces (was 56)
	// Following are initially set in AllocateModuleArrays
	extern int MAXHCArrayBounds; // Bounds based on Max Number of Vertices in surfaces
	extern int MAXHCArrayIncrement; // Increment based on Max Number of Vertices in surfaces
	// The following variable should be re-engineered to lower in module hierarchy but need more analysis
	extern int NVS; // Number of vertices of the shadow/clipped surface
	extern int NumVertInShadowOrClippedSurface;
	extern int CurrentSurfaceBeingShadowed;
	extern int CurrentShadowingSurface;
	extern int OverlapStatus; // Results of overlap calculation:
	// 1=No overlap; 2=NS1 completely within NS2
	// 3=NS2 completely within NS1; 4=Partial overlap

	extern Array1D< Real64 > CTHETA; // Cosine of angle of incidence of sun's rays on surface NS
	extern int FBKSHC; // HC location of first back surface
	extern int FGSSHC; // HC location of first general shadowing surface
	extern int FINSHC; // HC location of first back surface overlap
	extern int FRVLHC; // HC location of first reveal surface
	extern int FSBSHC; // HC location of first subsurface
	extern int LOCHCA; // Location of highest data in the HC arrays
	extern int NBKSHC; // Number of back surfaces in the HC arrays
	extern int NGSSHC; // Number of general shadowing surfaces in the HC arrays
	extern int NINSHC; // Number of back surface overlaps in the HC arrays
	extern int NRVLHC; // Number of reveal surfaces in HC array
	extern int NSBSHC; // Number of subsurfaces in the HC arrays
	extern bool CalcSkyDifShading; // True when sky diffuse solar shading is
	extern int ShadowingCalcFrequency; // Frequency for Shadowing Calculations
	extern int ShadowingDaysLeft; // Days left in current shadowing period
	extern bool debugging;
	extern std::ofstream shd_stream; // Shading file stream
	extern Array1D_int HCNS; // Surface number of back surface HC figures
	extern Array1D_int HCNV; // Number of vertices of each HC figure
	extern Array2D< Int64 > HCA; // 'A' homogeneous coordinates of sides
	extern Array2D< Int64 > HCB; // 'B' homogeneous coordinates of sides
	extern Array2D< Int64 > HCC; // 'C' homogeneous coordinates of sides
	extern Array2D< Int64 > HCX; // 'X' homogeneous coordinates of vertices of figure.
	extern Array2D< Int64 > HCY; // 'Y' homogeneous coordinates of vertices of figure.
	extern Array3D_int WindowRevealStatus;
	extern Array1D< Real64 > HCAREA; // Area of each HC figure.  Sign Convention:  Base Surface
	// - Positive, Shadow - Negative, Overlap between two shadows
	// - positive, etc., so that sum of HC areas=base sunlit area
	extern Array1D< Real64 > HCT; // Transmittance of each HC figure
	extern Array1D< Real64 > ISABSF; // For simple interior solar distribution (in which all beam
	// radiation entering zone is assumed to strike the floor),
	// fraction of beam radiation absorbed by each floor surface
	extern Array1D< Real64 > SAREA; // Sunlit area of heat transfer surface HTS
	// Excludes multiplier for windows
	// Shadowing combinations data structure...See ShadowingCombinations type
	extern int NumTooManyFigures;
	extern int NumTooManyVertices;
	extern int NumBaseSubSurround;
	extern Array1D< Real64 > SUNCOS; // Direction cosines of solar position
	extern Real64 XShadowProjection; // X projection of a shadow (formerly called C)
	extern Real64 YShadowProjection; // Y projection of a shadow (formerly called S)
	extern Array1D< Real64 > XTEMP; // Temporary 'X' values for HC vertices of the overlap
	extern Array1D< Real64 > XVC; // X-vertices of the clipped figure
	extern Array1D< Real64 > XVS; // X-vertices of the shadow
	extern Array1D< Real64 > YTEMP; // Temporary 'Y' values for HC vertices of the overlap
	extern Array1D< Real64 > YVC; // Y-vertices of the clipped figure
	extern Array1D< Real64 > YVS; // Y-vertices of the shadow
	extern Array1D< Real64 > ZVC; // Z-vertices of the clipped figure
	// Used in Sutherland Hodman poly clipping
	extern Array1D< Real64 > ATEMP; // Temporary 'A' values for HC vertices of the overlap
	extern Array1D< Real64 > BTEMP; // Temporary 'B' values for HC vertices of the overlap
	extern Array1D< Real64 > CTEMP; // Temporary 'C' values for HC vertices of the overlap
	extern Array1D< Real64 > XTEMP1; // Temporary 'X' values for HC vertices of the overlap
	extern Array1D< Real64 > YTEMP1; // Temporary 'Y' values for HC vertices of the overlap
	extern int maxNumberOfFigures;

	// SUBROUTINE SPECIFICATIONS FOR MODULE SolarShading

	// Types

	struct SurfaceErrorTracking
	{
		// Members
		int SurfIndex1; // Tracking for main error message
		int SurfIndex2; // Tracking for Overlapping Figure Name or Surface # 1
		int MiscIndex; // Used for other pertinent information to be stored

		// Default Constructor
		SurfaceErrorTracking() :
			SurfIndex1( 0 ),
			SurfIndex2( 0 ),
			MiscIndex( 0 )
		{}

		// Member Constructor
		SurfaceErrorTracking(
			int const SurfIndex1, // Tracking for main error message
			int const SurfIndex2, // Tracking for Overlapping Figure Name or Surface # 1
			int const MiscIndex // Used for other pertinent information to be stored
		) :
			SurfIndex1( SurfIndex1 ),
			SurfIndex2( SurfIndex2 ),
			MiscIndex( MiscIndex )
		{}

	};

	// Object Data
	extern Array1D< SurfaceErrorTracking > TrackTooManyFigures;
	extern Array1D< SurfaceErrorTracking > TrackTooManyVertices;
	extern Array1D< SurfaceErrorTracking > TrackBaseSubSurround;

	// Functions

	void
	InitSolarCalculations();

	void
	GetShadowingInput();

	void
	AllocateModuleArrays();

	void
	AnisoSkyViewFactors();

	void
	CHKBKS(
		int const NBS, // Surface Number of the potential back surface
		int const NRS // Surface Number of the potential shadow receiving surface
	);

	void
	CHKGSS(
		int const NRS, // Surface number of the potential shadow receiving surface
		int const NSS, // Surface number of the potential shadow casting surface
		Real64 const ZMIN, // Lowest point of the receiving surface
		bool & CannotShade // TRUE if shadow casting surface cannot shade receiving surface.
	);

	void
	CHKSBS(
		int const HTS, // Heat transfer surface number of the general receiving surf
		int const GRSNR, // Surface number of general receiving surface
		int const SBSNR // Surface number of subsurface
	);

	bool
	polygon_contains_point(
		int const nsides, // number of sides (vertices)
		Array1A< Vector > polygon_3d, // points of polygon
		Vector const & point_3d, // point to be tested
		bool const ignorex,
		bool const ignorey,
		bool const ignorez
	);

	void
	ComputeIntSolarAbsorpFactors();

	void
	CLIP(
		int const NVT,
		Array1< Real64 > & XVT,
		Array1< Real64 > & YVT,
		Array1< Real64 > & ZVT
	);

	void
	CTRANS(
		int const NS, // Surface number whose vertex coordinates are being transformed
		int const NGRS, // Base surface number for surface NS
		int & NVT, // Number of vertices for surface NS
		Array1< Real64 > & XVT, // XYZ coordinates of vertices of NS in plane of NGRS
		Array1< Real64 > & YVT,
		Array1< Real64 > & ZVT
	);

	void
	HTRANS(
		int const I, // Mode selector: 0 - Compute H.C. of sides
		int const NS, // Figure Number
		int const NumVertices // Number of vertices
	);

	void
	HTRANS0(
		int const NS, // Figure Number
		int const NumVertices // Number of vertices
	);

	void
	HTRANS1(
		int const NS, // Figure Number
		int const NumVertices // Number of vertices
	);

	void
	INCLOS(
		int const N1, // Figure number of figure 1
		int const N1NumVert, // Number of vertices of figure 1
		int const N2, // Figure number of figure 2
		int const N2NumVert, // Number of vertices of figure 2
		int & NumVerticesOverlap, // Number of vertices which overlap
		int & NIN // Number of vertices of figure 1 within figure 2
	);

	void
	INTCPT(
		int const NV1, // Number of vertices of figure NS1
		int const NV2, // Number of vertices of figure NS2
		int & NV3, // Number of vertices of figure NS3
		int const NS1, // Number of the figure being overlapped
		int const NS2 // Number of the figure doing overlapping
	);

	void
	CLIPPOLY(
		int const NS1, // Figure number of figure 1 (The subject polygon)
		int const NS2, // Figure number of figure 2 (The clipping polygon)
		int const NV1, // Number of vertices of figure 1
		int const NV2, // Number of vertices of figure 2
		int & NV3 // Number of vertices of figure 3
	);

	void
	MULTOL(
		int const NNN, // argument
		int const LOC0, // Location in the homogeneous coordinate array
		int const NRFIGS // Number of figures overlapped
	);

	void
	ORDER(
		int const NV3, // Number of vertices of figure NS3
		int const NS3 // Location to place results of overlap
	);

	void
	DeterminePolygonOverlap(
		int const NS1, // Number of the figure being overlapped
		int const NS2, // Number of the figure doing overlapping
		int const NS3 // Location to place results of overlap
	);

	void
	CalcPerSolarBeam(
		Real64 const AvgEqOfTime, // Average value of Equation of Time for period
		Real64 const AvgSinSolarDeclin, // Average value of Sine of Solar Declination for period
		Real64 const AvgCosSolarDeclin // Average value of Cosine of Solar Declination for period
	);

	void
	FigureSunCosines(
		int const iHour,
		int const iTimeStep,
		Real64 const EqOfTime, // value of Equation of Time for period
		Real64 const SinSolarDeclin, // value of Sine of Solar Declination for period
		Real64 const CosSolarDeclin // value of Cosine of Solar Declination for period
	);

	void
	FigureSolarBeamAtTimestep(
		int const iHour,
		int const iTimeStep
	);

	void
	DetermineShadowingCombinations();

	void
	SHADOW(
		int const iHour, // Hour index
		int const TS // Time Step
	);

	void
	SHDBKS(
		int const NGRS, // Number of the general receiving surface
		int const CurSurf,
		int const NBKS, // Number of back surfaces
		int const HTS // Heat transfer surface number of the general receiving surf
	);

	void
	SHDGSS(
		int const NGRS,
		int const iHour, // Hour Counter
		int const TS, // TimeStep
		int const CurSurf, // Current Surface
		int const NGSS, // Number of general shadowing surfaces
		int const HTS // Heat transfer surface number of the general receiving surf
	);

	void
	CalcInteriorSolarOverlaps(
		int const iHour, // Hour Index
		int const NBKS, // Number of back surfaces associated with this GRSNR (in general, only
		int const HTSS, // Surface number of the subsurface (exterior window)
		int const GRSNR, // General receiving surface number (base surface of the exterior window)
		int const TS // Time step Index
	);

	void
	CalcInteriorSolarDistribution();

	int
	WindowScheduledSolarAbs(
		int const SurfNum, // Surface number
		int const ConstNum // Construction number
	);

	int
	SurfaceScheduledSolarInc(
		int const SurfNum, // Surface number
		int const ConstNum // Construction number
	);

	void
	PerformSolarCalculations();

	void
	SHDRVL(
		int const HTSS, // Heat transfer surface number of the subsurface
		int const SBSNR, // Subsurface number
		int const Hour,
		int const TS
	);

	void
	SHDSBS(
		int const iHour, // Hour Index
		int const CurSurf,
		int const NBKS, // Number of back surfaces
		int const NSBS, // Number of subsurfaces
		int const HTS, // Heat transfer surface number of the general receiving surf
		int const TS // Time step Index
	);

	void
	SUN3(
		int const JulianDayOfYear, // Julian Day Of Year
		Real64 & SineOfSolarDeclination, // Sine of Solar Declination
		Real64 & EquationOfTime // Equation of Time (Degrees)
	);

	void
	SUN4(
		Real64 const CurrentTime, // Time to use in shadowing calculations
		Real64 const EqOfTime, // Equation of time for current day
		Real64 const SinSolarDeclin, // Sine of the Solar declination (current day)
		Real64 const CosSolarDeclin // Cosine of the Solar declination (current day)
	);

	void
	WindowShadingManager();

	void
	WindowGapAirflowControl();

	void
	SkyDifSolarShading();

	void
	CalcWindowProfileAngles();

	void
	CalcFrameDividerShadow(
		int const SurfNum, // Surface number
		int const FrDivNum, // Frame/divider number
		int const HourNum // Hour number
	);

	void
	CalcBeamSolarOnWinRevealSurface();

	void
	ReportSurfaceShading();

	void
	ReportSurfaceErrors();

	void
	ComputeWinShadeAbsorpFactors();

	void
	CalcWinTransDifSolInitialDistribution();

	void
	CalcInteriorWinTransDifSolInitialDistribution(
		int const ZoneNum, // Zone index number
		int const IntWinSurfNum, // Interior Window Surface number in Zone ZoneNum
		Real64 const IntWinDifSolarTransW // Diffuse Solar transmitted through Interior Window IntWinSurfNum from adjacent zone [W]
	);

	void
	CalcComplexWindowOverlap(
		BSDFGeomDescr & Geom, // State Geometry
		BSDFWindowGeomDescr const & Window, // Window Geometry
		int const ISurf // Surface number of the complex fenestration
	);

	void
	TimestepInitComplexFenestration();

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

} // SolarShading

} // EnergyPlus

#endif
