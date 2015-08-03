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

		// Member Constructor
		IntWinAdjZoneExtWinStruct(
			int const SurfNum, // exterior window index
			int const NumOfIntWindows, // count of interior windows associated with this ext win
			Array1_int const & IntWinNum // index numbers for interior windows assoc with this ext win
		) :
			SurfNum( SurfNum ),
			NumOfIntWindows( NumOfIntWindows ),
			IntWinNum( IntWinNum )
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

		// Member Constructor
		ZoneDaylightCalc(
			int const DaylightType, // Type of Daylighting (1=Detailed, 2=DElight)
			int const AvailSchedNum, // pointer to availability schedule if present
			int const TotalDaylRefPoints, // Number of detailed daylighting reference points in a zone (0,1 or 2)
			int const TotalDElightRefPts, // Number of DElight daylighting reference points in a zone (0,1 or 2) - RJH
			Array2< Real64 > const & DaylRefPtAbsCoord, // =0.0 ! X,Y,Z coordinates of all daylighting reference points
			Array1_bool const & DaylRefPtInBounds, // True when coordinates are in bounds of zone coordinates
			Array1< Real64 > const & FracZoneDaylit, // =0.0  ! Fraction of zone controlled by each reference point
			Array1< Real64 > const & IllumSetPoint, // =0.0  ! Illuminance setpoint at each reference point (lux)
			int const LightControlType, // Lighting control type (same for all reference points)
			Real64 const ViewAzimuthForGlare, // View direction relative to window for glare calculation (deg)
			int const MaxGlareallowed, // Maximum allowable discomfort glare index
			Real64 const MinPowerFraction, // Minimum fraction of power input that continuous dimming system can dim down to
			Real64 const MinLightFraction, // Minimum fraction of light output that continuous dimming system can dim down to
			int const LightControlSteps, // Number of levels (excluding zero) of stepped control system
			Real64 const LightControlProbability, // For manual control of stepped systems, probability that lighting will
			int const TotalExtWindows, // Total number of exterior windows in the zone
			Real64 const AveVisDiffReflect, // Area-weighted average inside surface visible reflectance of zone
			Array1< Real64 > const & RefPtPowerReductionFactor, // =1.0  ! Electric power reduction factor at reference points
			Real64 const ZonePowerReductionFactor, // Electric power reduction factor for entire zone due to daylighting
			Array1< Real64 > const & DaylIllumAtRefPt, // =0.0 ! Daylight illuminance at reference points (lux)
			Array1< Real64 > const & GlareIndexAtRefPt, // =0.0 ! Glare index at reference points
			Array1_int const & AdjIntWinZoneNums, // List of zone numbers of adjacent zones that have exterior windows and
			int const NumOfIntWinAdjZones, // Number of adjacent zones that have exterior windows and share one or
			int const NumOfIntWinAdjZoneExtWins, // number of exterior windows associated with zone via interior windows
			Array1< IntWinAdjZoneExtWinStruct > const & IntWinAdjZoneExtWin, // nested structure | info about exterior window associated with zone via interior window
			int const NumOfDayltgExtWins, // Number of associated exterior windows providing daylight to this zone
			Array1_int const & DayltgExtWinSurfNums, // List of surface numbers of zone's exterior windows or
			Array1_int const & DayltgFacPtrsForExtWins, // Zone's daylighting factor pointers.
			Real64 const MinIntWinSolidAng, // Minimum solid angle subtended by an interior window in a zone
			Real64 const TotInsSurfArea, // Total inside surface area of a daylit zone (m2)
			Real64 const FloorVisRefl, // Area-weighted visible reflectance of floor of a daylit zone
			Real64 const InterReflIllFrIntWins, // Inter-reflected illuminance due to beam and diffuse solar passing
			Array1< Real64 > const & BacLum, // =0.0 ! Background luminance at each reference point (cd/m2)
			Array2< Real64 > const & SolidAngAtRefPt, // (MaxRefPoints,50)
			Array2< Real64 > const & SolidAngAtRefPtWtd, // (MaxRefPoints,50)
			Array3< Real64 > const & IllumFromWinAtRefPt, // (MaxRefPoints,2,50)
			Array3< Real64 > const & BackLumFromWinAtRefPt, // (MaxRefPoints,2,50)
			Array3< Real64 > const & SourceLumFromWinAtRefPt, // (MaxRefPoints,2,50)
			Array5< Real64 > const & DaylIllFacSky,
			Array5< Real64 > const & DaylSourceFacSky,
			Array5< Real64 > const & DaylBackFacSky,
			Array4< Real64 > const & DaylIllFacSun,
			Array4< Real64 > const & DaylIllFacSunDisk,
			Array4< Real64 > const & DaylSourceFacSun,
			Array4< Real64 > const & DaylSourceFacSunDisk,
			Array4< Real64 > const & DaylBackFacSun,
			Array4< Real64 > const & DaylBackFacSunDisk,
			Array1< Real64 > const & TimeExceedingGlareIndexSPAtRefPt,
			Array1< Real64 > const & TimeExceedingDaylightIlluminanceSPAtRefPt,
			bool const AdjZoneHasDayltgCtrl,
			int const MapCount, // Number of maps assigned to Zone
			Array1_int const & ZoneToMap // Pointers to maps allocated to Zone
		) :
			DaylightType( DaylightType ),
			AvailSchedNum( AvailSchedNum ),
			TotalDaylRefPoints( TotalDaylRefPoints ),
			TotalDElightRefPts( TotalDElightRefPts ),
			DaylRefPtAbsCoord( DaylRefPtAbsCoord ),
			DaylRefPtInBounds( DaylRefPtInBounds ),
			FracZoneDaylit( FracZoneDaylit ),
			IllumSetPoint( IllumSetPoint ),
			LightControlType( LightControlType ),
			ViewAzimuthForGlare( ViewAzimuthForGlare ),
			MaxGlareallowed( MaxGlareallowed ),
			MinPowerFraction( MinPowerFraction ),
			MinLightFraction( MinLightFraction ),
			LightControlSteps( LightControlSteps ),
			LightControlProbability( LightControlProbability ),
			TotalExtWindows( TotalExtWindows ),
			AveVisDiffReflect( AveVisDiffReflect ),
			RefPtPowerReductionFactor( RefPtPowerReductionFactor ),
			ZonePowerReductionFactor( ZonePowerReductionFactor ),
			DaylIllumAtRefPt( DaylIllumAtRefPt ),
			GlareIndexAtRefPt( GlareIndexAtRefPt ),
			AdjIntWinZoneNums( AdjIntWinZoneNums ),
			NumOfIntWinAdjZones( NumOfIntWinAdjZones ),
			NumOfIntWinAdjZoneExtWins( NumOfIntWinAdjZoneExtWins ),
			IntWinAdjZoneExtWin( IntWinAdjZoneExtWin ),
			NumOfDayltgExtWins( NumOfDayltgExtWins ),
			DayltgExtWinSurfNums( DayltgExtWinSurfNums ),
			DayltgFacPtrsForExtWins( DayltgFacPtrsForExtWins ),
			MinIntWinSolidAng( MinIntWinSolidAng ),
			TotInsSurfArea( TotInsSurfArea ),
			FloorVisRefl( FloorVisRefl ),
			InterReflIllFrIntWins( InterReflIllFrIntWins ),
			BacLum( BacLum ),
			SolidAngAtRefPt( SolidAngAtRefPt ),
			SolidAngAtRefPtWtd( SolidAngAtRefPtWtd ),
			IllumFromWinAtRefPt( IllumFromWinAtRefPt ),
			BackLumFromWinAtRefPt( BackLumFromWinAtRefPt ),
			SourceLumFromWinAtRefPt( SourceLumFromWinAtRefPt ),
			DaylIllFacSky( DaylIllFacSky ),
			DaylSourceFacSky( DaylSourceFacSky ),
			DaylBackFacSky( DaylBackFacSky ),
			DaylIllFacSun( DaylIllFacSun ),
			DaylIllFacSunDisk( DaylIllFacSunDisk ),
			DaylSourceFacSun( DaylSourceFacSun ),
			DaylSourceFacSunDisk( DaylSourceFacSunDisk ),
			DaylBackFacSun( DaylBackFacSun ),
			DaylBackFacSunDisk( DaylBackFacSunDisk ),
			TimeExceedingGlareIndexSPAtRefPt( TimeExceedingGlareIndexSPAtRefPt ),
			TimeExceedingDaylightIlluminanceSPAtRefPt( TimeExceedingDaylightIlluminanceSPAtRefPt ),
			AdjZoneHasDayltgCtrl( AdjZoneHasDayltgCtrl ),
			MapCount( MapCount ),
			ZoneToMap( ZoneToMap )
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

		// Member Constructor
		IllumMapData(
			std::string const & Name, // Map name
			int const Zone, // Pointer to zone being mapped
			Real64 const Z, // Elevation or height
			Real64 const Xmin, // Minimum X value
			Real64 const Xmax, // Maximum X value
			int const Xnum, // Number of X reference points (going N-S)
			Real64 const Xinc, // Increment between X reference points
			Real64 const Ymin, // Minimum Y value
			Real64 const Ymax, // Maximum Y value
			int const Ynum, // Number of Y reference points (going E-W)
			Real64 const Yinc, // Increment between Y reference points
			int const UnitNo, // Unit number for map output (later merged to final file)
			bool const HeaderXLineLengthNeeded, // X header will likely be the longest line in the file
			int const HeaderXLineLength // actual length of this X header line
		) :
			Name( Name ),
			Zone( Zone ),
			Z( Z ),
			Xmin( Xmin ),
			Xmax( Xmax ),
			Xnum( Xnum ),
			Xinc( Xinc ),
			Ymin( Ymin ),
			Ymax( Ymax ),
			Ynum( Ynum ),
			Yinc( Yinc ),
			UnitNo( UnitNo ),
			HeaderXLineLengthNeeded( HeaderXLineLengthNeeded ),
			HeaderXLineLength( HeaderXLineLength )
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

		// Member Constructor
		MapCalcData(
			int const TotalMapRefPoints, // Number of illuminance map reference points in this zone (up to 100)
			int const Zone, // Pointer to zone being mapped
			Array2< Real64 > const & MapRefPtAbsCoord, // X,Y,Z coordinates of all illuminance map reference points
			Array1_bool const & MapRefPtInBounds, // True when coordinates are in bounds of zone coordinates
			Array1< Real64 > const & DaylIllumAtMapPt, // Daylight illuminance at illuminance map points (lux)
			Array1< Real64 > const & GlareIndexAtMapPt, // Glare index at illuminance map points
			Array1< Real64 > const & DaylIllumAtMapPtHr, // Daylight illuminance at illuminance map points (lux)
			Array1< Real64 > const & GlareIndexAtMapPtHr, // Glare index at illuminance map points
			Array2< Real64 > const & SolidAngAtMapPt, // (MaxRefPoints,50)
			Array2< Real64 > const & SolidAngAtMapPtWtd, // (MaxRefPoints,50)
			Array3< Real64 > const & IllumFromWinAtMapPt, // (MaxRefPoints,2,50)
			Array3< Real64 > const & BackLumFromWinAtMapPt, // (MaxRefPoints,2,50)
			Array3< Real64 > const & SourceLumFromWinAtMapPt, // (MaxRefPoints,2,50)
			Array5< Real64 > const & DaylIllFacSky,
			Array5< Real64 > const & DaylSourceFacSky,
			Array5< Real64 > const & DaylBackFacSky,
			Array4< Real64 > const & DaylIllFacSun,
			Array4< Real64 > const & DaylIllFacSunDisk,
			Array4< Real64 > const & DaylSourceFacSun,
			Array4< Real64 > const & DaylSourceFacSunDisk,
			Array4< Real64 > const & DaylBackFacSun,
			Array4< Real64 > const & DaylBackFacSunDisk
		) :
			TotalMapRefPoints( TotalMapRefPoints ),
			Zone( Zone ),
			MapRefPtAbsCoord( MapRefPtAbsCoord ),
			MapRefPtInBounds( MapRefPtInBounds ),
			DaylIllumAtMapPt( DaylIllumAtMapPt ),
			GlareIndexAtMapPt( GlareIndexAtMapPt ),
			DaylIllumAtMapPtHr( DaylIllumAtMapPtHr ),
			GlareIndexAtMapPtHr( GlareIndexAtMapPtHr ),
			SolidAngAtMapPt( SolidAngAtMapPt ),
			SolidAngAtMapPtWtd( SolidAngAtMapPtWtd ),
			IllumFromWinAtMapPt( IllumFromWinAtMapPt ),
			BackLumFromWinAtMapPt( BackLumFromWinAtMapPt ),
			SourceLumFromWinAtMapPt( SourceLumFromWinAtMapPt ),
			DaylIllFacSky( DaylIllFacSky ),
			DaylSourceFacSky( DaylSourceFacSky ),
			DaylBackFacSky( DaylBackFacSky ),
			DaylIllFacSun( DaylIllFacSun ),
			DaylIllFacSunDisk( DaylIllFacSunDisk ),
			DaylSourceFacSun( DaylSourceFacSun ),
			DaylSourceFacSunDisk( DaylSourceFacSunDisk ),
			DaylBackFacSun( DaylBackFacSun ),
			DaylBackFacSunDisk( DaylBackFacSunDisk )
		{}

	};

	// Object Data
	extern Array1D< ZoneDaylightCalc > ZoneDaylight;
	extern Array1D< IllumMapData > IllumMap;
	extern Array1D< MapCalcData > IllumMapCalc;

} // DataDaylighting

} // EnergyPlus

#endif
