#ifndef DataSurfaces_hh_INCLUDED
#define DataSurfaces_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1D.hh>
#include <ObjexxFCL/FArray2D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataBSDFWindow.hh>
#include <DataGlobals.hh>
#include <DataVectorTypes.hh>

namespace EnergyPlus {

namespace DataSurfaces {

	// Using/Aliasing
	using DataBSDFWindow::BSDFWindowDescript;
	using DataVectorTypes::Vector;

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:
	extern int const MaxSlatAngs;

	// Parameters to indicate surface shape for use with the Surface
	// derived type (see below):

	extern int const Triangle;
	extern int const Quadrilateral;
	extern int const Rectangle;
	extern int const Polygonal;
	extern int const RectangularDoorWindow;
	extern int const RectangularOverhang;
	extern int const RectangularLeftFin;
	extern int const RectangularRightFin;
	extern int const TriangularWindow;
	extern int const TriangularDoor;

	// Parameters to indicate exterior boundary conditions for use with
	// the Surface derived type (see below):
	// Note:  Positive values correspond to an interzone adjacent surface

	extern int const ExternalEnvironment;
	extern int const Ground;
	extern int const OtherSideCoefNoCalcExt;
	extern int const OtherSideCoefCalcExt;
	extern int const OtherSideCondModeledExt;
	extern int const GroundFCfactorMethod;

	extern FArray1D_string const cExtBoundCondition;

	// Parameters to indicate the first "corner" of a surface
	// Currently, these are used only during input of surfaces
	// They are here in order to facilitate later use in shading setup/calculations.
	extern int const UpperLeftCorner;
	extern int const LowerLeftCorner;
	extern int const LowerRightCorner;
	extern int const UpperRightCorner;

	// Parameters to indicate user specified convection coefficients (for surface)
	extern int const ConvCoefValue; // User specified "value" as the override type
	extern int const ConvCoefSchedule; // User specified "schedule" as the override type
	extern int const ConvCoefUserCurve; // User specified "UserCurve" as the override type
	extern int const ConvCoefSpecifiedModel; // one of the direct named model equation keys

	// Parameters to indicate reference air temperatures for inside surface temperature calculations
	extern int const ZoneMeanAirTemp; // mean air temperature of the zone => MAT
	extern int const AdjacentAirTemp; // air temperature adjacent ot surface => TempEffBulkAir
	extern int const ZoneSupplyAirTemp; // supply air temperature of the zone

	extern int const AltAngStepsForSolReflCalc; // Number of steps in altitude angle for solar reflection calc
	extern int const AzimAngStepsForSolReflCalc; // Number of steps in azimuth angle of solar reflection calc

	// Parameters to indicate surface classes
	// Surface Class (FLOOR, WALL, ROOF (incl's CEILING), WINDOW, DOOR, GLASSDOOR,
	// SHADING (includes OVERHANG, WING), DETACHED, INTMASS),
	// TDD:DOME, TDD:DIFFUSER (for tubular daylighting device)
	// (Note: GLASSDOOR and TDD:DIFFUSER get overwritten as WINDOW
	// in SurfaceGeometry.cc, SurfaceWindow%OriginalClass holds the true value)
	// why aren't these sequential (LKL - 13 Aug 2007)
	extern int const SurfaceClass_Wall;
	extern int const SurfaceClass_Floor;
	extern int const SurfaceClass_Roof;
	extern int const SurfaceClass_IntMass;
	extern int const SurfaceClass_Detached_B;
	extern int const SurfaceClass_Detached_F;
	extern int const SurfaceClass_Window;
	extern int const SurfaceClass_Door;
	extern int const SurfaceClass_GlassDoor;
	extern int const SurfaceClass_Shading;
	extern int const SurfaceClass_Overhang;
	extern int const SurfaceClass_Fin;
	extern int const SurfaceClass_TDD_Dome;
	extern int const SurfaceClass_TDD_Diffuser;

	//Parameters to indicate heat transfer model to use for surface
	extern int const HeatTransferModel_NotSet;
	extern int const HeatTransferModel_None; // shading surfaces for example
	extern int const HeatTransferModel_CTF;
	extern int const HeatTransferModel_EMPD;
	extern int const HeatTransferModel_CondFD;
	extern int const HeatTransferModel_HAMT;
	extern int const HeatTransferModel_Window5; // original detailed layer-by-layer based on window 4 and window 5
	extern int const HeatTransferModel_ComplexFenestration; // BSDF
	extern int const HeatTransferModel_TDD; // tubular daylighting device

	// Parameters for classification of outside face of surfaces
	extern int const OutConvClass_WindwardVertWall;
	extern int const OutConvClass_LeewardVertWall;
	extern int const OutConvClass_RoofStable;
	extern int const OutConvClass_RoofUnstable;

	// Parameters for adpative convection algorithm's classification of inside face of surfaces
	extern int const InConvClass_A1_VertWalls; // flow regime A1, vertical walls
	extern int const InConvClass_A1_StableHoriz; // flow regime A1
	extern int const InConvClass_A1_UnstableHoriz; // flow regime A1
	extern int const InConvClass_A1_HeatedFloor; // flow regime A1
	extern int const InConvClass_A1_ChilledCeil; // flow regime A1
	extern int const InConvClass_A1_StableTilted; // flow regime A1
	extern int const InConvClass_A1_UnstableTilted; // flow regime A1
	extern int const InConvClass_A1_Windows; // flow regime A1
	extern int const InConvClass_A2_VertWallsNonHeated; // flow regime A2
	extern int const InConvClass_A2_HeatedVerticalWall; // flow regime A2
	extern int const InConvClass_A2_StableHoriz; // flow regime A2
	extern int const InConvClass_A2_UnstableHoriz; // flow regime A2
	extern int const InConvClass_A2_StableTilted; // flow regime A2
	extern int const InConvClass_A2_UnstableTilted; // flow regime A2
	extern int const InConvClass_A2_Windows; // flow regime A2
	extern int const InConvClass_A3_VertWalls; // flow regime A3
	extern int const InConvClass_A3_StableHoriz; // flow regime A3
	extern int const InConvClass_A3_UnstableHoriz; // flow regime A3
	extern int const InConvClass_A3_StableTilted; // flow regime A3
	extern int const InConvClass_A3_UnstableTilted; // flow regime A3
	extern int const InConvClass_A3_Windows; // flow regime A3
	extern int const InConvClass_B_VertWalls; // flow regime B
	extern int const InConvClass_B_VertWallsNearHeat; // flow regime B
	extern int const InConvClass_B_StableHoriz; // flow regime B
	extern int const InConvClass_B_UnstableHoriz; // flow regime B
	extern int const InConvClass_B_StableTilted; // flow regime B
	extern int const InConvClass_B_UnstableTilted; // flow regime B
	extern int const InConvClass_B_Windows; // flow regime B
	extern int const InConvClass_C_Walls; // flow regime C
	extern int const InConvClass_C_Ceiling; // flow regime C
	extern int const InConvClass_C_Floor; // flow regime C
	extern int const InConvClass_C_Windows; // flow regime C
	extern int const InConvClass_D_Walls; // flow regime D
	extern int const InConvClass_D_StableHoriz; // flow regime D
	extern int const InConvClass_D_UnstableHoriz; // flow regime D
	extern int const InConvClass_D_StableTilted; // flow regime D
	extern int const InConvClass_D_UnstableTilted; // flow regime D
	extern int const InConvClass_D_Windows; // flow regime D
	extern int const InConvClass_E_AssistFlowWalls; // flow regime E
	extern int const InConvClass_E_OpposFlowWalls; // flow regime E
	extern int const InConvClass_E_StableFloor; // flow regime E
	extern int const InConvClass_E_UnstableFloor; // flow regime E
	extern int const InConvClass_E_StableCeiling; // flow regime E
	extern int const InConvClass_E_UnstableCieling; // flow regime E
	extern int const InConvClass_E_Windows; // flow regime E

	// Parameters for fenestration relative location in zone
	extern int const InConvWinLoc_NotSet;
	extern int const InConvWinLoc_LowerPartOfExteriorWall; // this is a window in the lower part of wall
	extern int const InConvWinLoc_UpperPartOfExteriorWall; // this is a window in the upper part of wall
	extern int const InConvWinLoc_WindowAboveThis; // this is a wall with window above it
	extern int const InConvWinLoc_WindowBelowThis; // this is a wall with window below it
	extern int const InConvWinLoc_LargePartOfExteriorWall; // this is a big window taking up most of wall

	// Parameters for window shade status
	extern int const NoShade;
	extern int const ShadeOff;
	extern int const IntShadeOn; // Interior shade on
	extern int const SwitchableGlazing;
	extern int const ExtShadeOn; // Exterior shade on
	extern int const ExtScreenOn; // Exterior screen on
	extern int const IntBlindOn; // Interior blind on
	extern int const ExtBlindOn; // Exterior blind on
	extern int const BGShadeOn; // Between-glass shade on
	extern int const BGBlindOn; // Between-glass blind on
	extern int const IntShadeConditionallyOff;
	extern int const GlassConditionallyLightened;
	extern int const ExtShadeConditionallyOff;
	extern int const IntBlindConditionallyOff;
	extern int const ExtBlindConditionallyOff;

	// WindowShadingControl Shading Types
	extern int const WSC_ST_NoShade;
	extern int const WSC_ST_InteriorShade;
	extern int const WSC_ST_SwitchableGlazing;
	extern int const WSC_ST_ExteriorShade;
	extern int const WSC_ST_InteriorBlind;
	extern int const WSC_ST_ExteriorBlind;
	extern int const WSC_ST_BetweenGlassShade;
	extern int const WSC_ST_BetweenGlassBlind;
	extern int const WSC_ST_ExteriorScreen;

	// WindowShadingControl Control Types
	extern int const WSCT_AlwaysOn; // AlwaysOn
	extern int const WSCT_AlwaysOff; // AlwaysOff
	extern int const WSCT_OnIfScheduled; // OnIfScheduleAllows
	extern int const WSCT_HiSolar; // OnIfHighSolarOnWindow
	extern int const WSCT_HiHorzSolar; // OnIfHighHorizontalSolar
	extern int const WSCT_HiOutAirTemp; // OnIfHighOutsideAirTemp
	extern int const WSCT_HiZoneAirTemp; // OnIfHighZoneAirTemp
	extern int const WSCT_HiZoneCooling; // OnIfHighZoneCooling
	extern int const WSCT_HiGlare; // OnIfHighGlare
	extern int const WSCT_MeetDaylIlumSetp; // MeetDaylightIlluminanceSetpoint
	extern int const WSCT_OnNightLoOutTemp_OffDay; // OnNightIfLowOutsideTemp/OffDay
	extern int const WSCT_OnNightLoInTemp_OffDay; // OnNightIfLowInsideTemp/OffDay
	extern int const WSCT_OnNightIfHeating_OffDay; // OnNightIfHeating/OffDay
	extern int const WSCT_OnNightLoOutTemp_OnDayCooling; // OnNightIfLowOutsideTemp/OnDayIfCooling
	extern int const WSCT_OnNightIfHeating_OnDayCooling; // OnNightIfHeating/OnDayIfCooling
	extern int const WSCT_OffNight_OnDay_HiSolarWindow; // OffNight/OnDayIfCoolingAndHighSolarOnWindow
	extern int const WSCT_OnNight_OnDay_HiSolarWindow; // OnNight/OnDayIfCoolingAndHighSolarOnWindow
	extern int const WSCT_OnHiOutTemp_HiSolarWindow; // OnIfHighOutsideAirTempAndHighSolarOnWindow
	extern int const WSCT_OnHiOutTemp_HiHorzSolar; // OnIfHighOutsideAirTempAndHighHorizontalSolar
	extern int const WSCT_OnHiZoneTemp_HiSolarWindow; // OnIfHighZoneAirTempAndHighSolarOnWindow
	extern int const WSCT_OnHiZoneTemp_HiHorzSolar; // OnIfHighZoneAirTempAndHighHorizontalSolar

	// WindowShadingControl Slat Angle Control for Blinds
	extern int const WSC_SAC_FixedSlatAngle;
	extern int const WSC_SAC_ScheduledSlatAngle;
	extern int const WSC_SAC_BlockBeamSolar;

	// Parameter for window screens beam reflectance accounting
	extern int const DoNotModel;
	extern int const ModelAsDirectBeam;
	extern int const ModelAsDiffuse;

	// Parameters for window divider type
	extern int const DividedLite;
	extern int const Suspended;

	// Parameters for air flow window source
	extern int const AirFlowWindow_Source_IndoorAir;
	extern int const AirFlowWindow_Source_OutdoorAir;

	// Parameters for air flow window destination
	extern int const AirFlowWindow_Destination_IndoorAir;
	extern int const AirFlowWindow_Destination_OutdoorAir;
	extern int const AirFlowWindow_Destination_ReturnAir;

	// Parameters for air flow window control
	extern int const AirFlowWindow_ControlType_MaxFlow;
	extern int const AirFlowWindow_ControlType_AlwaysOff;
	extern int const AirFlowWindow_ControlType_Schedule;

	// Parameters for window model selection
	extern int const Window5DetailedModel; // indicates original winkelmann window 5 implementation
	extern int const WindowBSDFModel; // indicates complex fenestration window 6 implementation
	extern int const WindowEQLModel; // indicates equivalent layer winodw model implementation

	// DERIVED TYPE DEFINITIONS:

	// Definitions used for scheduled surface gains

	// INTERFACE BLOCK SPECIFICATIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:

	extern int TotSurfaces; // Total number of surfaces (walls, floors, roofs, windows, shading surfaces, etc.--everything)
	extern int TotWindows; // Total number of windows
	extern int TotComplexWin; // Total number of windows with complex optical properties
	extern int TotStormWin; // Total number of storm window blocks
	extern int TotWinShadingControl; // Total number of window shading control blocks
	extern int TotIntConvCoeff; // Total number of interior convection coefficient (overrides)
	extern int TotExtConvCoeff; // Total number of exterior convection coefficient (overrides)
	extern int TotOSC; // Total number of Other Side Coefficient Blocks
	extern int TotOSCM; // Total number of Other Side Conditions Model Blocks.
	extern int TotExtVentCav;
	extern int TotSurfIncSolSSG; // Total number of scheduled surface gains for incident solar radiation on surface
	extern int TotFenLayAbsSSG; // Total number of scheduled surface gains for absorbed solar radiation in window layers
	extern int Corner; // Which corner is specified as the first vertice
	extern int MaxVerticesPerSurface; // Maximum number of vertices allowed for a single surface (default -- can go higher)

	extern int BuildingShadingCount; // Total number of Building External Shades
	extern int FixedShadingCount; // Total number of Fixed External Shades
	extern int AttachedShadingCount; // Total number of Shades attached to Zones

	extern bool AspectTransform; // Set to true when GeometryTransform object is used
	extern bool CalcSolRefl; // Set to true when Solar Reflection Calculations object is used
	extern bool CCW; // True if vertices will be entered in CounterClockWise Order
	extern bool WorldCoordSystem; // True if vertices will be "World Coordinates"
	// False means relative coordinates
	extern bool DaylRefWorldCoordSystem; // True if Daylight Reference Point vertices will be "World Coordinates"
	// False means relative coordinates
	extern int MaxRecPts; // Max number of receiving points on a surface for solar reflection calc
	extern int MaxReflRays; // Max number of rays from a receiving surface for solar reflection calc
	extern Real64 GroundLevelZ; // Z value of ground level for solar refl calc (m)
	extern bool AirflowWindows; // TRUE if one or more airflow windows

	extern bool ShadingTransmittanceVaries; // overall, shading transmittance varies for the building

	extern FArray1D_int InsideGlassCondensationFlag; // 1 if innermost glass inside surface temp < zone air dew point;
	// 0 otherwise
	extern FArray1D_int InsideFrameCondensationFlag; // 1 if frame inside surface temp < zone air dew point;
	// 0 otherwise
	extern FArray1D_int InsideDividerCondensationFlag; // 1 if divider inside surface temp < zone air dew point;
	// 0 otherwise
	extern FArray1D_int AdjacentZoneToSurface; // Array of adjacent zones to each surface

	extern FArray1D< Real64 > X0; // X-component of translation vector
	extern FArray1D< Real64 > Y0; // Y-component of translation vector
	extern FArray1D< Real64 > Z0; // Z-component of translation vector
	extern FArray1D< Real64 > DSZone; // Factor for sky diffuse solar radiation into a zone
	extern FArray1D< Real64 > DGZone; // Factor for ground diffuse solar radiation into a zone
	extern FArray1D< Real64 > DBZone; // Factor for diffuse radiation in a zone from
	// beam reflecting from inside surfaces
	extern FArray1D< Real64 > DBZoneSSG; // Factor for diffuse radiation in a zone from beam reflecting from inside surfaces. Used only for scheduled surface gains
	extern FArray1D< Real64 > CBZone; // Factor for beam solar absorbed by interior shades
	extern FArray1D< Real64 > AISurf; // Time step value of factor for beam
	// absorbed on inside of opaque surface
	extern FArray1D< Real64 > AOSurf; // Time step value of factor for beam
	// absorbed on outside of opaque surface
	extern FArray1D< Real64 > BmToBmReflFacObs; // Factor for incident solar from specular beam refl
	// from obstructions (W/m2)/(W/m2)
	extern FArray1D< Real64 > BmToDiffReflFacObs; // Factor for incident solar from diffuse beam refl
	// from obstructions (W/m2)/(W/m2)
	extern FArray1D< Real64 > BmToDiffReflFacGnd; // Factor for incident solar from diffuse beam refl from ground

	extern FArray2D< Real64 > AWinSurf; // Time step value of factor for beam
	// absorbed in window glass layers

	extern FArray2D< Real64 > AWinCFOverlap; // Time step value of factor for beam
	// absorbed in window glass layers which comes from other windows
	// It happens sometimes that beam enters one window and hits back of
	// second window. It is used in complex fenestration only

	extern FArray1D< Real64 > AirSkyRadSplit; // Fractional split between the air and
	// the sky for radiation from the surface
	// Fraction of sky IR coming from sky itself; 1-AirSkyRadSplit comes from the atmosphere.

	extern FArray1D< Real64 > WinTransSolar; // Exterior beam plus diffuse solar transmitted through window, or
	// window plus shade/blind, into zone (W)
	extern FArray1D< Real64 > WinBmSolar; // Exterior beam solar transmitted through window, or
	// window plus blind, into zone (W)

	extern FArray1D< Real64 > WinBmBmSolar; // Exterior beam-to-beam solar transmitted through window, or
	// window plus blind, into zone (W)
	extern FArray1D< Real64 > WinBmDifSolar; // Exterior beam-to-diffuse solar transmitted through window, or
	// window plus blind, into zone (W)

	extern FArray1D< Real64 > WinDifSolar; // Exterior diffuse solar transmitted through window, or
	// window plus shade/blind, into zone (W)
	extern FArray1D< Real64 > WinDirSolTransAtIncAngle; // Window's beam-beam solar transmittance at current timestep's
	// angle of incidence
	extern FArray1D< Real64 > WinHeatGain; // Total heat gain from window = WinTransSolar + (IR and convection from
	// glazing, or, if interior shade, IR and convection from
	// zone-side of shade plus gap air convection to zone) + (IR and
	// convection from frame) + (IR and convection from divider if no
	// interior shade) (W)
	extern FArray1D< Real64 > WinHeatGainRep; // Equals WinHeatGain when WinHeatGain >= 0.0
	extern FArray1D< Real64 > WinHeatLossRep; // Equals -WinHeatGain when WinHeatGain < 0.0

	extern FArray1D< Real64 > WinGainConvGlazToZoneRep; // component of WinHeatGain convect to zone from glazing (W)
	extern FArray1D< Real64 > WinGainIRGlazToZoneRep; // component of WinHeatGain net IR to zone from glazing (W)
	extern FArray1D< Real64 > WinLossSWZoneToOutWinRep; // component of WinHeatGain shortwave transmit back out (W)
	extern FArray1D< Real64 > WinGainFrameDividerToZoneRep; // component of WinHeatGain to zone from frame/divider (W)
	extern FArray1D< Real64 > WinGainConvGlazShadGapToZoneRep; // component of WinHeatGain convection to zone from
	// the gap between the inner most glazing and the shade   (W)
	extern FArray1D< Real64 > WinGainConvShadeToZoneRep; // component of WinHeatGain convect to zone from front shade (W)
	extern FArray1D< Real64 > WinGainIRShadeToZoneRep; // component of WinHeatGain net IR to zone from front shade (W)
	extern FArray1D< Real64 > OtherConvGainInsideFaceToZoneRep; // net imbalance of convection heat gain from equivalent Layer window inside face to zone air

	extern FArray1D< Real64 > WinGapConvHtFlowRep; // Convective heat flow from gap in airflow window (W)
	//REAL(r64), ALLOCATABLE, DIMENSION(:) :: OpaqSurfInsFaceCondGainRep !Equals Opaq Surf Ins Face Cond
	//                                                                   ! when Opaq Surf Ins Face Cond >= 0
	//REAL(r64), ALLOCATABLE, DIMENSION(:) :: OpaqSurfInsFaceCondLossRep !Equals -Opaq Surf Ins Face Cond
	//                                                                   ! when Opaq Surf Ins Face Cond  < 0
	extern FArray1D< Real64 > WinShadingAbsorbedSolar; // Exterior beam plus diffuse solar absorbed by
	//  window shading device (W)
	extern FArray1D< Real64 > WinSysSolTransmittance; // Effective solar transmittance of window + shading device,
	// if present
	extern FArray1D< Real64 > WinSysSolReflectance; // Effective solar reflectance of window + shading device,
	// if present
	extern FArray1D< Real64 > WinSysSolAbsorptance; // Effective solar absorptance of window + shading device,
	// if present
	extern FArray2D< Real64 > SUNCOSHR; // Hourly values of SUNCOS (solar direction cosines) //Autodesk:Init Zero-initialization added to avoid use uninitialized
	extern FArray2D< Real64 > ReflFacBmToDiffSolObs;
	extern FArray2D< Real64 > ReflFacBmToDiffSolGnd;
	extern FArray2D< Real64 > ReflFacBmToBmSolObs;
	extern FArray1D< Real64 > ReflFacSkySolObs;
	extern FArray1D< Real64 > ReflFacSkySolGnd;
	extern FArray2D< Real64 > CosIncAveBmToBmSolObs;
	extern FArray1D< Real64 > DBZoneIntWin; // Value of factor for beam solar entering a zone through interior windows
	// (considered to contribute to diffuse in zone)
	extern FArray1D< Real64 > SurfSunlitArea; // Sunlit area by surface number
	extern FArray1D< Real64 > SurfSunlitFrac; // Sunlit fraction by surface number
	//energy
	extern FArray1D< Real64 > WinTransSolarEnergy; // Energy of WinTransSolar [J]
	extern FArray1D< Real64 > WinBmSolarEnergy; // Energy of WinBmSolar [J]

	extern FArray1D< Real64 > WinBmBmSolarEnergy; // Beam-to-beam energy of WinBmSolar [J]
	extern FArray1D< Real64 > WinBmDifSolarEnergy; // Beam-to-diffuse energy of WinBmSolar [J]

	extern FArray1D< Real64 > WinDifSolarEnergy; // Energy of WinDifSolar [J]
	extern FArray1D< Real64 > WinHeatGainRepEnergy; // Energy of WinHeatGainRep [J]
	extern FArray1D< Real64 > WinHeatLossRepEnergy; // Energy of WinHeatLossRep [J]
	extern FArray1D< Real64 > WinShadingAbsorbedSolarEnergy; // Energy of WinShadingAbsorbedSolar [J]
	extern FArray1D< Real64 > WinGapConvHtFlowRepEnergy; // Energy of WinGapConvHtFlowRep [J]

	// SUBROUTINE SPECIFICATIONS FOR MODULE DataSurfaces:

	// Types

	struct SurfaceData
	{
		// Members
		std::string Name; // User supplied name of the surface (must be unique)
		int Construction; // Pointer to the construction in the Construct derived type
		bool EMSConstructionOverrideON; // if true, EMS is calling to override the construction value
		int EMSConstructionOverrideValue; // pointer value to use for Construction when overridden
		int ConstructionStoredInputValue; // holds the original value for Construction per surface input
		int Class;
		// Geometry related parameters
		int Shape; // Surface shape (Triangle=1,Quadrilateral=2,Rectangle=3,
		//                Rectangular Window/Door=4,Rectangular Overhang=5,
		//                Rectangular Left Fin=6,Rectangular Right Fin=7,
		//                Triangular Window=8)
		int Sides; // Number of side/vertices for this surface (based on Shape)
		Real64 Area; // Surface area of the surface (less any subsurfaces) {m2}
		Real64 GrossArea; // Surface area of the surface (including subsurfaces) {m2}
		Real64 NetAreaShadowCalc; // Area of a wall/floor/ceiling less subsurfaces assuming
		//  all windows, if present, have unity multiplier.
		// Wall/floor/ceiling/roof areas that include windows include
		//  frame (unity) areas.
		// Areas of Windows including divider (unity) area.
		// These areas are used in shadowing / sunlit area calculations.
		Real64 Perimeter; // Perimeter length of the surface {m}
		Real64 Azimuth; // Direction the surface outward normal faces (degrees) or FACING
		Real64 Height; // Height of the surface (m)
		Real64 Reveal; // Depth of the window reveal (m) if this surface is a window
		Real64 Tilt; // Angle (deg) between the ground outward normal and the surface outward normal
		Real64 Width; // Width of the surface (m)
		// Boundary conditions and interconnections
		bool HeatTransSurf; // True if surface is a heat transfer surface,
		// False if a (detached) shadowing (sub)surface
		int HeatTransferAlgorithm; // used for surface-specific heat transfer algorithm.
		std::string BaseSurfName; // Name of BaseSurf
		int BaseSurf; // "Base surface" for this surface.  Applies mainly to subsurfaces
		// in which case it points back to the base surface number.
		// Equals 0 for detached shading.
		// BaseSurf equals surface number for all other surfaces.
		int NumSubSurfaces; // Number of subsurfaces this surface has (doors/windows)
		std::string ZoneName; // User supplied name of the Zone
		int Zone; // Interior environment or zone the surface is a part of
		// Note that though attached shading surfaces are part of a zone, this
		// value is 0 there to facilitate using them as detached surfaces (more
		// accurate shading.
		std::string ExtBoundCondName; // Name for the Outside Environment Object
		int ExtBoundCond; // For an "interzone" surface, this is the adjacent surface number.
		// for an internal/adiabatic surface this is the current surface number.
		// Otherwise, 0=external environment, -1=ground,
		// -2=other side coefficients (OSC--won't always use CTFs)
		// -3=other side conditions model
		// During input, interim values of UnreconciledZoneSurface ("Surface") and
		// UnenteredAdjacentZoneSurface ("Zone") are used until reconciled.
		int LowTempErrCount;
		int HighTempErrCount;
		bool ExtSolar; // True if the "outside" of the surface is exposed to solar
		bool ExtWind; // True if the "outside" of the surface is exposed to wind
		// Heat transfer coefficients
		int IntConvCoeff; // Interior Convection Coefficient pointer (different data structure)
		// when being overridden
		bool EMSOverrideIntConvCoef; // if true, EMS is calling to override interior convection coefficeint
		Real64 EMSValueForIntConvCoef; // Value EMS is calling to use for interior convection coefficient [W/m2-K]
		int ExtConvCoeff; // Exterior Convection Coefficient pointer (different data structure)
		// when being overridden
		bool EMSOverrideExtConvCoef; // if true, EMS is calling to override exterior convection coefficeint
		Real64 EMSValueForExtConvCoef; // Value EMS is calling to use for exterior convection coefficient [W/m2-K]
		Real64 ViewFactorGround; // View factor to the ground from the exterior of the surface
		//   for diffuse solar radiation
		Real64 ViewFactorSky; // View factor to the sky from the exterior of the surface
		//   for diffuse solar radiation
		Real64 ViewFactorGroundIR; // View factor to the ground and shadowing surfaces from the
		//    exterior of the surface for IR radiation
		Real64 ViewFactorSkyIR; // View factor to the sky from the exterior of the surface for IR radiation
		// Special/optional other side coefficients (OSC)
		int OSCPtr; // Pointer to OSC data structure
		int OSCMPtr; // "Pointer" to OSCM data structure (other side conditions from a model)
		// Optional parameters specific to shadowing surfaces and subsurfaces (detached shading, overhangs, wings, etc.)
		int SchedShadowSurfIndex; // Schedule for a shadowing (sub)surface
		bool ShadowSurfSchedVaries; // true if the scheduling (transmittance) on a shading surface varies.
		bool ShadowingSurf; // True if a surface is a shadowing surface
		bool IsTransparent; // True if the schedule values are always 1.0 (or the minimum is 1.0)
		Real64 SchedMinValue; // Schedule minimum value.
		// Optional parameters specific to solar reflection from surfaces
		Real64 ShadowSurfDiffuseSolRefl; // Diffuse solar reflectance of opaque portion
		Real64 ShadowSurfDiffuseVisRefl; // Diffuse visible reflectance of opaque portion
		Real64 ShadowSurfGlazingFrac; // Glazing fraction
		int ShadowSurfGlazingConstruct; // Glazing construction number
		bool ShadowSurfPossibleObstruction; // True if a surface can be an exterior obstruction
		bool ShadowSurfPossibleReflector; // True if a surface can be an exterior reflector, not used!
		int ShadowSurfRecSurfNum; // Receiving surface number
		// Optional movable insulation parameters
		int MaterialMovInsulExt; // Pointer to the material used for exterior movable insulation
		int MaterialMovInsulInt; // Pointer to the material used for interior movable insulation
		int SchedMovInsulExt; // Schedule for exterior movable insulation
		int SchedMovInsulInt; // Schedule for interior movable insulation
		// Vertices
		FArray1D< Vector > Vertex; // Surface Vertices are represented by Number of Sides and Vector (type)
		Vector Centroid; // computed centroid (also known as center of mass or surface balance point)
		Vector lcsx;
		Vector lcsy;
		Vector lcsz;
		Vector NewellAreaVector;
		Vector NewellSurfaceNormalVector; // same as OutNormVec in vector notation
		FArray1D< Real64 > OutNormVec; // Direction cosines (outward normal vector) for surface
		Real64 SinAzim; // Sine of surface azimuth angle
		Real64 CosAzim; // Cosine of surface azimuth angle
		Real64 SinTilt; // Sine of surface tilt angle
		Real64 CosTilt; // Cosine of surface tilt angle
		bool IsConvex; // true if the surface is convex.
		bool IsDegenerate; // true if the surface is degenerate.
		// Window Parameters (when surface is Window)
		int WindowShadingControlPtr; // Pointer to shading control (windows only)
		int ShadedConstruction; // Shaded construction (windows only)
		int StormWinConstruction; // Construction with storm window (windows only)
		int StormWinShadedConstruction; // Shaded construction with storm window (windows only)
		int FrameDivider; // Pointer to frame and divider information (windows only)
		Real64 Multiplier; // Multiplies glazed area, frame area and divider area (windows only)
		// Daylighting pointers
		int Shelf; // Pointer to daylighting shelf
		int TAirRef; // Flag for reference air temperature
		// ZoneMeanAirTemp   = 1 = mean air temperature or MAT => for mixing air model with all convection algos
		// except inlet-dependent algo
		// AdjacentAirTemp   = 2 = adjacent air temperature or TempEffBulkAir => for nodal or zonal air model
		// with all convection algos except inlet-dependent algo
		// ZoneSupplyAirTemp = 3 = supply air temperature => for mixing air model with inlet-dependent algo
		// Default value is 'ZoneMeanAirTemp' and value for each particular surface will be changed only if
		// the inlet-dependent convection algorithm and/or nodal and zonal air models are used.
		Real64 OutDryBulbTemp; // Surface outside dry bulb air temperature, for surface heat balance (C)
		bool OutDryBulbTempEMSOverrideOn; // if true, EMS is calling to override the surface's outdoor air temp
		Real64 OutDryBulbTempEMSOverrideValue; // value to use for EMS override of outdoor air drybulb temp (C)
		Real64 OutWetBulbTemp; // Surface outside wet bulb air temperature, for surface heat balance (C)
		bool OutWetBulbTempEMSOverrideOn; // if true, EMS is calling to override the surface's outdoor wetbulb
		Real64 OutWetBulbTempEMSOverrideValue; // value to use for EMS override of outdoor air wetbulb temp (C)
		Real64 WindSpeed; // Surface outside wind speed, for surface heat balance (m/s)
		bool WindSpeedEMSOverrideOn;
		Real64 WindSpeedEMSOverrideValue;
		std::string UNomWOFilm; // Nominal U Value without films stored as string
		std::string UNomFilm; // Nominal U Value with films stored as string
		bool ExtEcoRoof; // True if the top outside construction material is of type Eco Roof
		bool ExtCavityPresent; // true if there is an exterior vented cavity on surface
		int ExtCavNum; // index for this surface in ExtVentedCavity structure (if any)
		bool IsPV; // true if this is a photovoltaic surface (dxf output)
		bool IsICS; // true if this is an ICS collector
		bool IsPool; // true if this is a pool
		int ICSPtr; // Index to ICS collector
		// TH added 3/26/2010
		bool MirroredSurf; // Ture if it is a mirrored surface
		// additional attributes for convection correlations
		int IntConvClassification; // current classification for inside face air flow regime and surface orientation
		int IntConvHcModelEq; // current convection model for inside face
		int IntConvHcUserCurveIndex; // current index to user convection model if used
		int OutConvClassification; // current classification for outside face wind regime and convection orientation
		int OutConvHfModelEq; // current convection model for forced convection at outside face
		int OutConvHfUserCurveIndex; // current index to user forced convection model if used
		int OutConvHnModelEq; // current Convection model for natural convection at outside face
		int OutConvHnUserCurveIndex; // current index to user natural convection model if used
		Real64 OutConvFaceArea; // area of larger building envelope facade that surface is a part of
		Real64 OutConvFacePerimeter; // perimeter of larger building envelope facade that surface is a part of
		Real64 OutConvFaceHeight; // height of larger building envelope facade that surface is a part of
		Real64 IntConvZoneWallHeight; // [m] height of larger inside building wall element that surface is a part of
		Real64 IntConvZonePerimLength; // [m] length of perimeter zone's exterior wall
		Real64 IntConvZoneHorizHydrDiam; // [m] hydraulic diameter, usually 4 times the zone floor area div by perimeter
		Real64 IntConvWindowWallRatio; // [-] area of windows over area of exterior wall for zone
		int IntConvWindowLocation; // relative location of window in zone for interior Hc models
		bool IntConvSurfGetsRadiantHeat;
		bool IntConvSurfHasActiveInIt;
		bool PartOfVentSlabOrRadiantSurface; // surface cannot be part of both a radiant surface & ventilated slab group
		// LG added 1/6/12
		Real64 GenericContam; // [ppm] Surface generic contaminant as a storage term for
		// the surface diffusion model

		// Default Constructor
		SurfaceData() :
			Construction( 0 ),
			EMSConstructionOverrideON( false ),
			EMSConstructionOverrideValue( 0 ),
			ConstructionStoredInputValue( 0 ),
			Class( 0 ),
			Shape( 0 ),
			Sides( 0 ),
			Area( 0.0 ),
			GrossArea( 0.0 ),
			NetAreaShadowCalc( 0.0 ),
			Perimeter( 0.0 ),
			Azimuth( 0.0 ),
			Height( 0.0 ),
			Reveal( 0.0 ),
			Tilt( 0.0 ),
			Width( 0.0 ),
			HeatTransSurf( false ),
			HeatTransferAlgorithm( HeatTransferModel_NotSet ),
			BaseSurf( 0 ),
			NumSubSurfaces( 0 ),
			Zone( 0 ),
			ExtBoundCond( 0 ),
			LowTempErrCount( 0 ),
			HighTempErrCount( 0 ),
			ExtSolar( false ),
			ExtWind( false ),
			IntConvCoeff( 0 ),
			EMSOverrideIntConvCoef( false ),
			EMSValueForIntConvCoef( 0.0 ),
			ExtConvCoeff( 0 ),
			EMSOverrideExtConvCoef( false ),
			EMSValueForExtConvCoef( 0.0 ),
			ViewFactorGround( 0.0 ),
			ViewFactorSky( 0.0 ),
			ViewFactorGroundIR( 0.0 ),
			ViewFactorSkyIR( 0.0 ),
			OSCPtr( 0 ),
			OSCMPtr( 0 ),
			SchedShadowSurfIndex( 0 ),
			ShadowSurfSchedVaries( false ),
			ShadowingSurf( false ),
			IsTransparent( false ),
			SchedMinValue( 0.0 ),
			ShadowSurfDiffuseSolRefl( 0.0 ),
			ShadowSurfDiffuseVisRefl( 0.0 ),
			ShadowSurfGlazingFrac( 0.0 ),
			ShadowSurfGlazingConstruct( 0 ),
			ShadowSurfPossibleObstruction( true ),
			ShadowSurfPossibleReflector( false ),
			ShadowSurfRecSurfNum( 0 ),
			MaterialMovInsulExt( 0 ),
			MaterialMovInsulInt( 0 ),
			SchedMovInsulExt( 0 ),
			SchedMovInsulInt( 0 ),
			Centroid( 0.0, 0.0, 0.0 ),
			lcsx( 0.0, 0.0, 0.0 ),
			lcsy( 0.0, 0.0, 0.0 ),
			lcsz( 0.0, 0.0, 0.0 ),
			NewellAreaVector( 0.0, 0.0, 0.0 ),
			NewellSurfaceNormalVector( 0.0, 0.0, 0.0 ),
			OutNormVec( 3, 0.0 ),
			SinAzim( 0.0 ),
			CosAzim( 0.0 ),
			SinTilt( 0.0 ),
			CosTilt( 0.0 ),
			IsConvex( true ),
			IsDegenerate( false ),
			WindowShadingControlPtr( 0 ),
			ShadedConstruction( 0 ),
			StormWinConstruction( 0 ),
			StormWinShadedConstruction( 0 ),
			FrameDivider( 0 ),
			Multiplier( 1.0 ),
			Shelf( 0 ),
			TAirRef( ZoneMeanAirTemp ),
			OutDryBulbTemp( 0.0 ),
			OutDryBulbTempEMSOverrideOn( false ),
			OutDryBulbTempEMSOverrideValue( 0.0 ),
			OutWetBulbTemp( 0.0 ),
			OutWetBulbTempEMSOverrideOn( false ),
			OutWetBulbTempEMSOverrideValue( 0.0 ),
			WindSpeed( 0.0 ),
			WindSpeedEMSOverrideOn( false ),
			WindSpeedEMSOverrideValue( 0.0 ),
			UNomWOFilm( "-              " ),
			UNomFilm( "-              " ),
			ExtEcoRoof( false ),
			ExtCavityPresent( false ),
			ExtCavNum( 0 ),
			IsPV( false ),
			IsICS( false ),
			IsPool( false ),
			ICSPtr( 0 ),
			MirroredSurf( false ),
			IntConvClassification( 0 ),
			IntConvHcModelEq( 0 ),
			IntConvHcUserCurveIndex( 0 ),
			OutConvClassification( 0 ),
			OutConvHfModelEq( 0 ),
			OutConvHfUserCurveIndex( 0 ),
			OutConvHnModelEq( 0 ),
			OutConvHnUserCurveIndex( 0 ),
			OutConvFaceArea( 0.0 ),
			OutConvFacePerimeter( 0.0 ),
			OutConvFaceHeight( 0.0 ),
			IntConvZoneWallHeight( 0.0 ),
			IntConvZonePerimLength( 0.0 ),
			IntConvZoneHorizHydrDiam( 0.0 ),
			IntConvWindowWallRatio( 0.0 ),
			IntConvWindowLocation( InConvWinLoc_NotSet ),
			IntConvSurfGetsRadiantHeat( false ),
			IntConvSurfHasActiveInIt( false ),
			PartOfVentSlabOrRadiantSurface( false ),
			GenericContam( 0.0 )
		{}

		// Member Constructor
		SurfaceData(
			std::string const & Name, // User supplied name of the surface (must be unique)
			int const Construction, // Pointer to the construction in the Construct derived type
			bool const EMSConstructionOverrideON, // if true, EMS is calling to override the construction value
			int const EMSConstructionOverrideValue, // pointer value to use for Construction when overridden
			int const ConstructionStoredInputValue, // holds the original value for Construction per surface input
			int const Class,
			int const Shape, // Surface shape (Triangle=1,Quadrilateral=2,Rectangle=3,
			int const Sides, // Number of side/vertices for this surface (based on Shape)
			Real64 const Area, // Surface area of the surface (less any subsurfaces) {m2}
			Real64 const GrossArea, // Surface area of the surface (including subsurfaces) {m2}
			Real64 const NetAreaShadowCalc, // Area of a wall/floor/ceiling less subsurfaces assuming
			Real64 const Perimeter, // Perimeter length of the surface {m}
			Real64 const Azimuth, // Direction the surface outward normal faces (degrees) or FACING
			Real64 const Height, // Height of the surface (m)
			Real64 const Reveal, // Depth of the window reveal (m) if this surface is a window
			Real64 const Tilt, // Angle (deg) between the ground outward normal and the surface outward normal
			Real64 const Width, // Width of the surface (m)
			bool const HeatTransSurf, // True if surface is a heat transfer surface,
			int const HeatTransferAlgorithm, // used for surface-specific heat transfer algorithm.
			std::string const & BaseSurfName, // Name of BaseSurf
			int const BaseSurf, // "Base surface" for this surface.  Applies mainly to subsurfaces
			int const NumSubSurfaces, // Number of subsurfaces this surface has (doors/windows)
			std::string const & ZoneName, // User supplied name of the Zone
			int const Zone, // Interior environment or zone the surface is a part of
			std::string const & ExtBoundCondName, // Name for the Outside Environment Object
			int const ExtBoundCond, // For an "interzone" surface, this is the adjacent surface number.
			int const LowTempErrCount,
			int const HighTempErrCount,
			bool const ExtSolar, // True if the "outside" of the surface is exposed to solar
			bool const ExtWind, // True if the "outside" of the surface is exposed to wind
			int const IntConvCoeff, // Interior Convection Coefficient pointer (different data structure)
			bool const EMSOverrideIntConvCoef, // if true, EMS is calling to override interior convection coefficeint
			Real64 const EMSValueForIntConvCoef, // Value EMS is calling to use for interior convection coefficient [W/m2-K]
			int const ExtConvCoeff, // Exterior Convection Coefficient pointer (different data structure)
			bool const EMSOverrideExtConvCoef, // if true, EMS is calling to override exterior convection coefficeint
			Real64 const EMSValueForExtConvCoef, // Value EMS is calling to use for exterior convection coefficient [W/m2-K]
			Real64 const ViewFactorGround, // View factor to the ground from the exterior of the surface
			Real64 const ViewFactorSky, // View factor to the sky from the exterior of the surface
			Real64 const ViewFactorGroundIR, // View factor to the ground and shadowing surfaces from the
			Real64 const ViewFactorSkyIR, // View factor to the sky from the exterior of the surface for IR radiation
			int const OSCPtr, // Pointer to OSC data structure
			int const OSCMPtr, // "Pointer" to OSCM data structure (other side conditions from a model)
			int const SchedShadowSurfIndex, // Schedule for a shadowing (sub)surface
			bool const ShadowSurfSchedVaries, // true if the scheduling (transmittance) on a shading surface varies.
			bool const ShadowingSurf, // True if a surface is a shadowing surface
			bool const IsTransparent, // True if the schedule values are always 1.0 (or the minimum is 1.0)
			Real64 const SchedMinValue, // Schedule minimum value.
			Real64 const ShadowSurfDiffuseSolRefl, // Diffuse solar reflectance of opaque portion
			Real64 const ShadowSurfDiffuseVisRefl, // Diffuse visible reflectance of opaque portion
			Real64 const ShadowSurfGlazingFrac, // Glazing fraction
			int const ShadowSurfGlazingConstruct, // Glazing construction number
			bool const ShadowSurfPossibleObstruction, // True if a surface can be an exterior obstruction
			bool const ShadowSurfPossibleReflector, // True if a surface can be an exterior reflector, not used!
			int const ShadowSurfRecSurfNum, // Receiving surface number
			int const MaterialMovInsulExt, // Pointer to the material used for exterior movable insulation
			int const MaterialMovInsulInt, // Pointer to the material used for interior movable insulation
			int const SchedMovInsulExt, // Schedule for exterior movable insulation
			int const SchedMovInsulInt, // Schedule for interior movable insulation
			FArray1< Vector > const & Vertex, // Surface Vertices are represented by Number of Sides and Vector (type)
			Vector const & Centroid, // computed centroid (also known as center of mass or surface balance point)
			Vector const & lcsx,
			Vector const & lcsy,
			Vector const & lcsz,
			Vector const & NewellAreaVector,
			Vector const & NewellSurfaceNormalVector, // same as OutNormVec in vector notation
			FArray1< Real64 > const & OutNormVec, // Direction cosines (outward normal vector) for surface
			Real64 const SinAzim, // Sine of surface azimuth angle
			Real64 const CosAzim, // Cosine of surface azimuth angle
			Real64 const SinTilt, // Sine of surface tilt angle
			Real64 const CosTilt, // Cosine of surface tilt angle
			bool const IsConvex, // true if the surface is convex.
			bool const IsDegenerate, // true if the surface is degenerate.
			int const WindowShadingControlPtr, // Pointer to shading control (windows only)
			int const ShadedConstruction, // Shaded construction (windows only)
			int const StormWinConstruction, // Construction with storm window (windows only)
			int const StormWinShadedConstruction, // Shaded construction with storm window (windows only)
			int const FrameDivider, // Pointer to frame and divider information (windows only)
			Real64 const Multiplier, // Multiplies glazed area, frame area and divider area (windows only)
			int const Shelf, // Pointer to daylighting shelf
			int const TAirRef, // Flag for reference air temperature
			Real64 const OutDryBulbTemp, // Surface outside dry bulb air temperature, for surface heat balance (C)
			bool const OutDryBulbTempEMSOverrideOn, // if true, EMS is calling to override the surface's outdoor air temp
			Real64 const OutDryBulbTempEMSOverrideValue, // value to use for EMS override of outdoor air drybulb temp (C)
			Real64 const OutWetBulbTemp, // Surface outside wet bulb air temperature, for surface heat balance (C)
			bool const OutWetBulbTempEMSOverrideOn, // if true, EMS is calling to override the surface's outdoor wetbulb
			Real64 const OutWetBulbTempEMSOverrideValue, // value to use for EMS override of outdoor air wetbulb temp (C)
			Real64 const WindSpeed, // Surface outside wind speed, for surface heat balance (m/s)
			bool const WindSpeedEMSOverrideOn,
			Real64 const WindSpeedEMSOverrideValue,
			std::string const & UNomWOFilm, // Nominal U Value without films stored as string
			std::string const & UNomFilm, // Nominal U Value with films stored as string
			bool const ExtEcoRoof, // True if the top outside construction material is of type Eco Roof
			bool const ExtCavityPresent, // true if there is an exterior vented cavity on surface
			int const ExtCavNum, // index for this surface in ExtVentedCavity structure (if any)
			bool const IsPV, // true if this is a photovoltaic surface (dxf output)
			bool const IsICS, // true if this is an ICS collector
			bool const IsPool, // true if this is a pool
			int const ICSPtr, // Index to ICS collector
			bool const MirroredSurf, // Ture if it is a mirrored surface
			int const IntConvClassification, // current classification for inside face air flow regime and surface orientation
			int const IntConvHcModelEq, // current convection model for inside face
			int const IntConvHcUserCurveIndex, // current index to user convection model if used
			int const OutConvClassification, // current classification for outside face wind regime and convection orientation
			int const OutConvHfModelEq, // current convection model for forced convection at outside face
			int const OutConvHfUserCurveIndex, // current index to user forced convection model if used
			int const OutConvHnModelEq, // current Convection model for natural convection at outside face
			int const OutConvHnUserCurveIndex, // current index to user natural convection model if used
			Real64 const OutConvFaceArea, // area of larger building envelope facade that surface is a part of
			Real64 const OutConvFacePerimeter, // perimeter of larger building envelope facade that surface is a part of
			Real64 const OutConvFaceHeight, // height of larger building envelope facade that surface is a part of
			Real64 const IntConvZoneWallHeight, // [m] height of larger inside building wall element that surface is a part of
			Real64 const IntConvZonePerimLength, // [m] length of perimeter zone's exterior wall
			Real64 const IntConvZoneHorizHydrDiam, // [m] hydraulic diameter, usually 4 times the zone floor area div by perimeter
			Real64 const IntConvWindowWallRatio, // [-] area of windows over area of exterior wall for zone
			int const IntConvWindowLocation, // relative location of window in zone for interior Hc models
			bool const IntConvSurfGetsRadiantHeat,
			bool const IntConvSurfHasActiveInIt,
			bool const PartOfVentSlabOrRadiantSurface, // surface cannot be part of both a radiant surface & ventilated slab group
			Real64 const GenericContam // [ppm] Surface generic contaminant as a storage term for
		) :
			Name( Name ),
			Construction( Construction ),
			EMSConstructionOverrideON( EMSConstructionOverrideON ),
			EMSConstructionOverrideValue( EMSConstructionOverrideValue ),
			ConstructionStoredInputValue( ConstructionStoredInputValue ),
			Class( Class ),
			Shape( Shape ),
			Sides( Sides ),
			Area( Area ),
			GrossArea( GrossArea ),
			NetAreaShadowCalc( NetAreaShadowCalc ),
			Perimeter( Perimeter ),
			Azimuth( Azimuth ),
			Height( Height ),
			Reveal( Reveal ),
			Tilt( Tilt ),
			Width( Width ),
			HeatTransSurf( HeatTransSurf ),
			HeatTransferAlgorithm( HeatTransferAlgorithm ),
			BaseSurfName( BaseSurfName ),
			BaseSurf( BaseSurf ),
			NumSubSurfaces( NumSubSurfaces ),
			ZoneName( ZoneName ),
			Zone( Zone ),
			ExtBoundCondName( ExtBoundCondName ),
			ExtBoundCond( ExtBoundCond ),
			LowTempErrCount( LowTempErrCount ),
			HighTempErrCount( HighTempErrCount ),
			ExtSolar( ExtSolar ),
			ExtWind( ExtWind ),
			IntConvCoeff( IntConvCoeff ),
			EMSOverrideIntConvCoef( EMSOverrideIntConvCoef ),
			EMSValueForIntConvCoef( EMSValueForIntConvCoef ),
			ExtConvCoeff( ExtConvCoeff ),
			EMSOverrideExtConvCoef( EMSOverrideExtConvCoef ),
			EMSValueForExtConvCoef( EMSValueForExtConvCoef ),
			ViewFactorGround( ViewFactorGround ),
			ViewFactorSky( ViewFactorSky ),
			ViewFactorGroundIR( ViewFactorGroundIR ),
			ViewFactorSkyIR( ViewFactorSkyIR ),
			OSCPtr( OSCPtr ),
			OSCMPtr( OSCMPtr ),
			SchedShadowSurfIndex( SchedShadowSurfIndex ),
			ShadowSurfSchedVaries( ShadowSurfSchedVaries ),
			ShadowingSurf( ShadowingSurf ),
			IsTransparent( IsTransparent ),
			SchedMinValue( SchedMinValue ),
			ShadowSurfDiffuseSolRefl( ShadowSurfDiffuseSolRefl ),
			ShadowSurfDiffuseVisRefl( ShadowSurfDiffuseVisRefl ),
			ShadowSurfGlazingFrac( ShadowSurfGlazingFrac ),
			ShadowSurfGlazingConstruct( ShadowSurfGlazingConstruct ),
			ShadowSurfPossibleObstruction( ShadowSurfPossibleObstruction ),
			ShadowSurfPossibleReflector( ShadowSurfPossibleReflector ),
			ShadowSurfRecSurfNum( ShadowSurfRecSurfNum ),
			MaterialMovInsulExt( MaterialMovInsulExt ),
			MaterialMovInsulInt( MaterialMovInsulInt ),
			SchedMovInsulExt( SchedMovInsulExt ),
			SchedMovInsulInt( SchedMovInsulInt ),
			Vertex( Vertex ),
			Centroid( Centroid ),
			lcsx( lcsx ),
			lcsy( lcsy ),
			lcsz( lcsz ),
			NewellAreaVector( NewellAreaVector ),
			NewellSurfaceNormalVector( NewellSurfaceNormalVector ),
			OutNormVec( 3, OutNormVec ),
			SinAzim( SinAzim ),
			CosAzim( CosAzim ),
			SinTilt( SinTilt ),
			CosTilt( CosTilt ),
			IsConvex( IsConvex ),
			IsDegenerate( IsDegenerate ),
			WindowShadingControlPtr( WindowShadingControlPtr ),
			ShadedConstruction( ShadedConstruction ),
			StormWinConstruction( StormWinConstruction ),
			StormWinShadedConstruction( StormWinShadedConstruction ),
			FrameDivider( FrameDivider ),
			Multiplier( Multiplier ),
			Shelf( Shelf ),
			TAirRef( TAirRef ),
			OutDryBulbTemp( OutDryBulbTemp ),
			OutDryBulbTempEMSOverrideOn( OutDryBulbTempEMSOverrideOn ),
			OutDryBulbTempEMSOverrideValue( OutDryBulbTempEMSOverrideValue ),
			OutWetBulbTemp( OutWetBulbTemp ),
			OutWetBulbTempEMSOverrideOn( OutWetBulbTempEMSOverrideOn ),
			OutWetBulbTempEMSOverrideValue( OutWetBulbTempEMSOverrideValue ),
			WindSpeed( WindSpeed ),
			WindSpeedEMSOverrideOn( WindSpeedEMSOverrideOn ),
			WindSpeedEMSOverrideValue( WindSpeedEMSOverrideValue ),
			UNomWOFilm( UNomWOFilm ),
			UNomFilm( UNomFilm ),
			ExtEcoRoof( ExtEcoRoof ),
			ExtCavityPresent( ExtCavityPresent ),
			ExtCavNum( ExtCavNum ),
			IsPV( IsPV ),
			IsICS( IsICS ),
			IsPool( IsPool ),
			ICSPtr( ICSPtr ),
			MirroredSurf( MirroredSurf ),
			IntConvClassification( IntConvClassification ),
			IntConvHcModelEq( IntConvHcModelEq ),
			IntConvHcUserCurveIndex( IntConvHcUserCurveIndex ),
			OutConvClassification( OutConvClassification ),
			OutConvHfModelEq( OutConvHfModelEq ),
			OutConvHfUserCurveIndex( OutConvHfUserCurveIndex ),
			OutConvHnModelEq( OutConvHnModelEq ),
			OutConvHnUserCurveIndex( OutConvHnUserCurveIndex ),
			OutConvFaceArea( OutConvFaceArea ),
			OutConvFacePerimeter( OutConvFacePerimeter ),
			OutConvFaceHeight( OutConvFaceHeight ),
			IntConvZoneWallHeight( IntConvZoneWallHeight ),
			IntConvZonePerimLength( IntConvZonePerimLength ),
			IntConvZoneHorizHydrDiam( IntConvZoneHorizHydrDiam ),
			IntConvWindowWallRatio( IntConvWindowWallRatio ),
			IntConvWindowLocation( IntConvWindowLocation ),
			IntConvSurfGetsRadiantHeat( IntConvSurfGetsRadiantHeat ),
			IntConvSurfHasActiveInIt( IntConvSurfHasActiveInIt ),
			PartOfVentSlabOrRadiantSurface( PartOfVentSlabOrRadiantSurface ),
			GenericContam( GenericContam )
		{}

	};

	struct SurfaceWindowCalc // Calculated window-related values
	{
		// Members
		int ShadingFlag; // -1: window has no shading device
		//   0: shading device is off
		//   1: interior shade is on
		//   2: glazing is switched to darker state
		//   3: exterior shade is on
		//   4: exterior screen is on
		//   6: interior blind is on
		//   7: exterior blind is on
		//   8: between-glass shade is on
		//   9: between-glass blind is on
		//  10: window has an interior shade that is off but may be
		//       triggered on later to control daylight glare
		//  20: window has switchable glazing that is unswitched but may be switched later
		//       to control daylight glare or daylight illuminance
		//  30: window has exterior shade that is off but may be triggered on later
		//       to control daylight glare
		//  60: window has an interior blind that is off but may be
		//       triggered on later to control daylight glare
		//  70: window has an exterior blind that is off but may be
		//       triggered on later to control daylight glare
		//  80: window has a between-glass shade that is off but may be
		//       triggered on later to control daylight glare
		//  90: window has a between-glass blind that is off but may be
		//       triggered on later to control daylight glare
		bool ShadingFlagEMSOn; // EMS control flag, true if EMS is controlling ShadingFlag with ShadingFlagEMSValue
		int ShadingFlagEMSValue; // EMS control value for Shading Flag
		int StormWinFlag; // -1: Storm window not applicable
		//   0: Window has storm window but it is off
		//   1: Window has storm window and it is on
		int StormWinFlagPrevDay; // Previous time step value of StormWinFlag
		Real64 FracTimeShadingDeviceOn; // For a single time step, = 0.0 if no shading device or shading device is off,
		//                         = 1.0 if shading device is on;
		// For time intervals longer than a time step, = fraction of time that shading
		// device is on.
		int ExtIntShadePrevTS; // 1 if exterior or interior blind or shade in place previous time step;
		// 0 otherwise
		int ShadedConstruction; // For windows with shading, the construction with shading
		bool SurfDayLightInit; // surface has been initialized for following 5 arrays
		FArray1D< Real64 > SolidAngAtRefPt; // Solid angle subtended by window from daylit ref points 1 and 2
		FArray1D< Real64 > SolidAngAtRefPtWtd; // Solid angle subtended by window from
		// ref pts weighted by glare pos factor
		FArray2D< Real64 > IllumFromWinAtRefPt; // Illuminance from window at ref pts for window
		// with and w/o shade (lux)
		FArray2D< Real64 > BackLumFromWinAtRefPt; // Window background luminance from window wrt ref pts (cd/m2)
		// with and w/o shade (cd/m2)
		FArray2D< Real64 > SourceLumFromWinAtRefPt; // Window luminance at ref pts for window
		// with and w/o shade (cd/m2)
		int DaylFacPoint; // Pointer to daylight factors for the window
		Real64 VisTransSelected; // Window vis trans at normal incidence selected for use in dayltg calculation
		Real64 SwitchingFactor; // Window switching factor (0.0 = unswitched; 1.0 = fully switched)
		FArray1D< Real64 > WinCenter; // X,Y,Z coordinates of window center point in building coord system
		Real64 Theta; // Azimuth of window normal (rad)
		Real64 Phi; // Altitude of window normal (rad)
		Real64 RhoCeilingWall; // Average interior reflectance seen by light moving up across horizontal
		//  plane thru center of window
		Real64 RhoFloorWall; // Same as above, but for light moving down
		Real64 FractionUpgoing; // Fraction light entering window that goes upward
		Real64 VisTransRatio; // For windows with switchable glazing, ratio of normal transmittance
		//  in switched state to that in unswitched state
		FArray1D< Real64 > ThetaFace; // Face temperatures of window layers (K)
		Real64 IRfromParentZone; // Incident IR from parent zone (W/m2)
		int IRErrCount; // For recurring error counts
		int IRErrCountC; // For recurring error counts (continuation)
		Real64 FrameArea; // Frame projected area (m2)
		Real64 FrameConductance; // Frame conductance [no air films] (W/m2-K)
		Real64 FrameSolAbsorp; // Frame solar absorptance (assumed same inside and outside)
		Real64 FrameVisAbsorp; // Frame visible absorptance (assumed same inside and outside)
		Real64 FrameEmis; // Frame thermal emissivity (thermal absorptance) (assumed same
		//   inside and outside)
		Real64 FrameAreaXEmiss; // Frame area times thermal emissivity (m2)
		Real64 FrameRadExchangeFactor; // Frame IR radiant exchange factor
		Real64 FrameHRadLinIn; // Frame linearized inside IR radiation conductance (W/m2-K)
		Real64 FrameRadThermalFluxRec; // Frame inside IR flux received (W/m2)
		Real64 FrameRadThermalFluxRecOld; // Previous value of frame inside IR flux received (W/m2)
		Real64 FrEdgeToCenterGlCondRatio; // Ratio of frame edge of glass conductance (without air films) to
		// center of glass conductance (without air films)
		Real64 FrameEdgeArea; // Area of glass near frame (m2)
		Real64 FrameTempSurfIn; // Frame inside surface temperature (C)
		Real64 FrameTempSurfInOld; // Previous value of frame inside surface temperature (C)
		Real64 FrameTempSurfOut; // Frame outside surface temperature (C)
		Real64 FrameQRadInAbs; // Radiation absorbed by inside of frame (short-wave from solar
		//   and lights; long-wave from internal gains) (W/m2)
		Real64 FrameQRadOutAbs; // Radiation absorbed by outside of frame (solar) (W/m2)
		Real64 ProjCorrFrOut; // Correction factor to absorbed radiation due to frame outside projection
		Real64 ProjCorrFrIn; // Correction factor to absorbed radiation due to frame inside projection
		int DividerType; // Divider type (1=DividedLite, 2=Suspended (between-pane))
		Real64 DividerArea; // Divider projected area (m2)
		Real64 DividerConductance; // Divider conductance [no air films] (W/m2-K)
		Real64 DividerSolAbsorp; // Divider solar absorptance (assumed same inside and outside)
		Real64 DividerVisAbsorp; // Divider visible absorptance (assumed same inside and outside)
		Real64 DividerEmis; // Divider thermal emissivity (thermal absorptance) (assumed same
		//   inside and outside)
		Real64 DividerAreaXEmiss; // Divider area times thermal emissivity (m2)
		Real64 DividerRadExchangeFactor; // Divider IR radiant exchange factor
		Real64 DividerHRadLinIn; // Divider linearized inside IR radiation conductance (W/m2-K)
		Real64 DividerRadThermalFluxRec; // Divider inside IR flux received (W/m2)
		Real64 DividerRadThermalFluxRecOld; // Previous value of divider inside IR flux received (W/m2)
		Real64 DivEdgeToCenterGlCondRatio; // Ratio of divider edge of glass conductance (without air films) to
		// center of glass conductance (without air films)
		Real64 DividerEdgeArea; // Area of glass near dividers (m2)
		Real64 DividerTempSurfIn; // Divider inside surface temperature (C)
		Real64 DividerTempSurfInOld; // Previous value of divider inside surface temperature (C)
		Real64 DividerTempSurfOut; // Divider outside surface temperature (C)
		Real64 DividerQRadInAbs; // Radiation absorbed by inside of divider (short-wave from solar
		//   and lights; long-wave from internal gains) (W/m2)
		Real64 DividerQRadOutAbs; // Radiation absorbed by outside of divider (solar) (W/m2)
		Real64 ProjCorrDivOut; // Correction factor to absorbed radiation due to divider outside projection
		Real64 ProjCorrDivIn; // Correction factor to absorbed radiation due to divider inside projection
		Real64 GlazedFrac; // (Glazed area)/(Glazed area + divider area)
		FArray1D< Real64 > OutProjSLFracMult; // Multiplier on sunlit fraction due to shadowing of glass by frame
		// and divider outside projections
		FArray1D< Real64 > InOutProjSLFracMult; // Multiplier on sunlit fraction due to shadowing of glass by frame
		// and divider inside and outside projections
		Real64 CenterGlArea; // Center of glass area (m2); area of glass where 1-D conduction dominates
		Real64 EdgeGlCorrFac; // Correction factor to center-of-glass conductance to account for
		//  2-D glass conduction thermal bridging effects near frame and divider
		int OriginalClass; // 0 or if entered originally as:
		// Window - SurfaceClass_Window
		// Glass Door - SurfaceClass_GlassDoor
		// tubular daylighting device dome - SurfaceClass_TDD_Dome
		// tubular daylighting device diffuser - SurfaceClass_TDD_Diffuser
		Real64 ExtBeamAbsByShade; // Exterior beam solar absorbed by window shade (W/m2)
		Real64 ExtDiffAbsByShade; // Exterior diffuse solar absorbed by window shade (W/m2)
		Real64 IntBeamAbsByShade; // Interior beam solar absorbed by window shade (W/m2)
		Real64 IntSWAbsByShade; // Interior diffuse solar plus short-wave from lights absorbed by window shade (W/m2)
		Real64 InitialDifSolAbsByShade; // Initial diffuse solar from ext and int windows absorbed by window shade (W/m2)
		Real64 IntLWAbsByShade; // Interior long-wave from zone lights and equipment absorbed by window shade (W/m2)
		FArray1D< Real64 > ShadeAbsFacFace; // Fraction of short-wave radiation incident on face 1 that is
		//  absorbed by face 1 and by the other face (face 2) when total absorbed
		//  radiation is apportioned to the two faces
		Real64 ConvCoeffWithShade; // Convection coefficient from glass or shade to gap air when
		//  interior or exterior shade is present (W/m2-K)
		Real64 ConvHeatFlowNatural; // Convective heat flow from gap between glass and interior shade or blind (W)
		Real64 ConvHeatGainToZoneAir; // Convective heat gain to zone air from window gap airflow (W)
		Real64 RetHeatGainToZoneAir; // Convective heat gain to return air sent to zone [W]
		Real64 DividerConduction; // Conduction through divider from outside to inside face (W)
		Real64 OtherConvHeatGain; // other convective = total conv - standard model prediction for EQL window model (W)
		int BlindNumber; // Blind number for a window with a blind
		FArray1D< Real64 > EffShBlindEmiss; // Effective emissivity of interior blind or shade
		FArray1D< Real64 > EffGlassEmiss; // Effective emissivity of glass adjacent to interior blind or shade
		Real64 EffInsSurfTemp; // Effective inside surface temperature for window with interior blind or
		//  shade; combination of shade/blind and glass temperatures (C)
		bool MovableSlats; // True if window has a blind with movable slats
		Real64 SlatAngThisTS; // Slat angle this time step for window with blind on (radians)
		Real64 SlatAngThisTSDeg; // Slat angle this time step for window with blind on (deg)
		bool SlatAngThisTSDegEMSon; // flag that indicate EMS system is actuating SlatAngThisTSDeg
		Real64 SlatAngThisTSDegEMSValue; // value that EMS sets for slat angle in degrees
		bool SlatsBlockBeam; // True if blind slats block incident beam solar
		Real64 BlindAirFlowPermeability; // Blind air-flow permeability for calculation of convective flow
		//  in gap between blind and glass
		Real64 TotGlazingThickness; // Total glazing thickness from outside of outer glass to inside of inner glass (m)
		Real64 ProfileAngHor; // Horizontal beam solar profile angle (degrees)
		Real64 ProfileAngVert; // Vertical beam solar profile angle (degrees)
		Real64 TanProfileAngHor; // Tangent of horizontal profile angle
		Real64 TanProfileAngVert; // Tangent of vertical profile angle
		Real64 InsideSillDepth; // Depth of inside sill (m)
		Real64 InsideReveal; // Depth of inside reveal (m)
		Real64 InsideSillSolAbs; // Solar absorptance of inside sill
		Real64 InsideRevealSolAbs; // Solar absorptance of inside reveal
		Real64 OutsideRevealSolAbs; // Solar absorptance of outside reveal
		Real64 BmSolAbsdInsReveal; // Multiplied by BeamSolarRad, gives beam solar absorbed
		// by inside reveal surfaces (m2)
		Real64 BmSolRefldInsReveal; // Multiplied by BeamSolarRad, gives beam solar reflected
		// by inside reveal surfaces (m2)
		Real64 BmSolRefldInsRevealReport; // Beam solar reflected by inside reveal surfaces, for reporting (W)
		Real64 BmSolRefldOutsRevealReport; // Beam solar reflected by outside reveal surfaces, for reporting (m2)
		Real64 BmSolAbsdOutsReveal; // Multiplied by BeamSolarRad, gives beam solar absorbed by
		// outside reveal surfaces (m2)
		Real64 OutsRevealDiffOntoGlazing; // Multiplied by BeamSolarRad, gives diffuse from beam reflection from
		//    outside reveal that is incident on the glazing per m2 of glazing (-)
		Real64 InsRevealDiffOntoGlazing; // Multiplied by BeamSolarRad, gives diffuse from beam reflection
		//  from inside reveal that is incident on the glazing per m2 of glazing (-)
		Real64 InsRevealDiffIntoZone; // Multiplied by BeamSolarRad, gives diffuse from beam reflection
		//  from inside reveal that goes into zone directly or reflected from glazing (m2)
		Real64 OutsRevealDiffOntoFrame; // Multiplied by BeamSolarRad, gives diffuse from beam reflection from outside reveal
		//   that is incident on the outside of the frame per m2 of frame (-)
		Real64 InsRevealDiffOntoFrame; // Multiplied by BeamSolarRad, gives diffuse from beam reflection from inside reveal
		//   that is incident on the outside of the frame per m2 of frame (-)
		// added for debugging CR 7596. TH 5/26/2009
		Real64 InsRevealDiffOntoGlazingReport; // Diffuse solar from beam reflection
		//  from inside reveal that is incident on the glazing (W)
		Real64 InsRevealDiffIntoZoneReport; // Diffuse from beam reflection
		//  from inside reveal that goes into zone directly or reflected from glazing (W)
		Real64 InsRevealDiffOntoFrameReport; // Diffuse from beam reflection from inside reveal
		//  that is incident on the frame (W)
		Real64 BmSolAbsdInsRevealReport; // Beam solar absorbed by inside reveal (W)
		Real64 BlTsolBmBm; // Time-step value of blind beam-beam solar transmittance (-)
		Real64 BlTsolBmDif; // Time-step value of blind beam-diffuse solar transmittance (-)
		Real64 BlTsolDifDif; // Time-step value of blind diffuse-diffuse solar transmittance (-)
		Real64 BlGlSysTsolBmBm; // Time-step value of blind/glass system beam-beam solar transmittance (-)
		Real64 BlGlSysTsolDifDif; // Time-step value of blind/glass system diffuse-diffuse solar transmittance (-)
		int ScreenNumber; // Screen number for a window with a screen (do not confuse with material number)
		Real64 ScTsolBmBm; // Time-step value of screen beam-beam solar transmittance (-)
		Real64 ScTsolBmDif; // Time-step value of screen beam-diffuse solar transmittance (-)
		Real64 ScTsolDifDif; // Time-step value of screen diffuse-diffuse solar transmittance (-)
		Real64 ScGlSysTsolBmBm; // Time-step value of screen/glass system beam-beam solar transmittance (-)
		Real64 ScGlSysTsolDifDif; // Time-step value of screen/glass system diffuse-diffuse solar transmittance (-)
		Real64 GlTsolBmBm; // Time-step value of glass beam-beam solar transmittance (-)
		Real64 GlTsolBmDif; // Time-step value of glass beam-diffuse solar transmittance (-)
		Real64 GlTsolDifDif; // Time-step value of glass diffuse-diffuse solar transmittance (-)
		int AirflowSource; // Source of gap airflow (INSIDEAIR, OUTSIDEAIR, etc.)
		int AirflowDestination; // Destination of gap airflow (INSIDEAIR, OUTSIDEAIR, etc.)
		Real64 MaxAirflow; // Maximum gap airflow (m3/s per m of glazing width)
		int AirflowControlType; // Gap airflow control type (ALWAYSONATMAXFLOW, etc.)
		bool AirflowHasSchedule; // True if gap airflow is scheduled
		int AirflowSchedulePtr; // Gap airflow schedule pointer
		Real64 AirflowThisTS; // Gap airflow this timestep (m3/s per m of glazing width)
		Real64 TAirflowGapOutlet; // Temperature of air leaving airflow gap between glass panes (C)
		int WindowCalcIterationsRep; // Number of iterations in window heat balance calculation
		Real64 BmSolTransThruIntWinRep; // Beam solar transmitted through interior window [W]
		Real64 VentingOpenFactorRep; // Window/door venting open factor, for reporting
		Real64 VentingOpenFactorMultRep; // Window/door opening modulation multiplier on venting open factor, for reporting
		Real64 InsideTempForVentingRep; // Inside air temp used to control window/door venting, for reporting (C)
		Real64 VentingAvailabilityRep; // Venting availability schedule value (0.0/1.0 = no venting allowed/not allowed)
		Real64 IllumFromWinAtRefPt1Rep; // Illuminance from window at reference point #1 [lux]
		Real64 IllumFromWinAtRefPt2Rep; // Illuminance from window at reference point #2 [lux]
		Real64 LumWinFromRefPt1Rep; // Window luminance as viewed from reference point #1 [cd/m2]
		Real64 LumWinFromRefPt2Rep; // Window luminance as viewed from reference point #2 [cd/m2]
		Real64 SkySolarInc; // Incident diffuse solar from sky; if CalcSolRefl is true, includes
		// reflection of sky diffuse and beam solar from exterior obstructions [W/m2]
		Real64 GndSolarInc; // Incident diffuse solar from ground; if CalcSolRefl is true, accounts
		// for shadowing of ground by building and obstructions [W/m2]
		Real64 SkyGndSolarInc; // Incident diffuse solar from ground-reflected sky radiation; used for
		//Complex Fen; if CalcSolRefl is true, accounts for shadowing of ground by building and obstructions [W/m2]
		Real64 BmGndSolarInc; // Incident diffuse solar from ground-reflected beam radiation; used for
		//Complex Fen; if CalcSolRefl is true, accounts for shadowing of ground by building and obstructions [W/m2]
		FArray1D< Real64 > ZoneAreaMinusThisSurf; // Zone inside surface area minus this surface and its subsurfaces
		// for floor/wall/ceiling (m2)
		FArray1D< Real64 > ZoneAreaReflProdMinusThisSurf; // Zone product of inside surface area times vis reflectance
		// minus this surface and its subsurfaces,
		// for floor/wall/ceiling (m2)
		Real64 LightWellEff; // Light well efficiency (multiplier on exterior window vis trans
		//  due to light well losses)
		bool SolarDiffusing; // True if exterior window with a construction that contains a
		//  diffusing glass layer
		//energy
		Real64 BmSolRefldInsRevealRepEnergy; // energy of BmSolRefldInsRevealReport [J]
		Real64 BmSolRefldOutsRevealRepEnergy; // energy of BmSolRefldOutsRevealReport [J]
		Real64 BmSolTransThruIntWinRepEnergy; // energy of BmSolTransThruIntWinRep [J]
		// Reporting
		Real64 FrameHeatGain;
		Real64 DividerHeatGain;
		Real64 FrameHeatLoss;
		Real64 DividerHeatLoss;
		// Added TH for thermochromic windows. 12/22/2008
		Real64 TCLayerTemp; // The temperature of the thermochromic layer of the window
		Real64 SpecTemp; // The specification temperature of the TC layer glass
		// Added for W6 integration June 2010
		int WindowModelType; // if set to WindowBSDFModel, then uses BSDF methods
		BSDFWindowDescript ComplexFen; // Data for complex fenestration, see DataBSDFWindow.cc for declaration

		// Default Constructor
		SurfaceWindowCalc() :
			ShadingFlag( ShadeOff ),
			ShadingFlagEMSOn( false ),
			ShadingFlagEMSValue( 0 ),
			StormWinFlag( -1 ),
			StormWinFlagPrevDay( -1 ),
			FracTimeShadingDeviceOn( 0.0 ),
			ExtIntShadePrevTS( 0 ),
			ShadedConstruction( 0 ),
			SurfDayLightInit( false ),
			DaylFacPoint( 0 ),
			VisTransSelected( 0.0 ),
			SwitchingFactor( 0.0 ),
			WinCenter( 3, 0.0 ),
			Theta( 0.0 ),
			Phi( 0.0 ),
			RhoCeilingWall( 0.0 ),
			RhoFloorWall( 0.0 ),
			FractionUpgoing( 0.0 ),
			VisTransRatio( 0.0 ),
			ThetaFace( 10, 296.15 ),
			IRfromParentZone( 0.0 ),
			IRErrCount( 0 ),
			IRErrCountC( 0 ),
			FrameArea( 0.0 ),
			FrameConductance( 0.0 ),
			FrameSolAbsorp( 0.0 ),
			FrameVisAbsorp( 0.0 ),
			FrameEmis( 0.0 ),
			FrameAreaXEmiss( 0.0 ),
			FrameRadExchangeFactor( 0.0 ),
			FrameHRadLinIn( 0.0 ),
			FrameRadThermalFluxRec( 0.0 ),
			FrameRadThermalFluxRecOld( 0.0 ),
			FrEdgeToCenterGlCondRatio( 1.0 ),
			FrameEdgeArea( 0.0 ),
			FrameTempSurfIn( 23.0 ),
			FrameTempSurfInOld( 23.0 ),
			FrameTempSurfOut( 23.0 ),
			FrameQRadInAbs( 0.0 ),
			FrameQRadOutAbs( 0.0 ),
			ProjCorrFrOut( 0.0 ),
			ProjCorrFrIn( 0.0 ),
			DividerType( 0 ),
			DividerArea( 0.0 ),
			DividerConductance( 0.0 ),
			DividerSolAbsorp( 0.0 ),
			DividerVisAbsorp( 0.0 ),
			DividerEmis( 0.0 ),
			DividerAreaXEmiss( 0.0 ),
			DividerRadExchangeFactor( 0.0 ),
			DividerHRadLinIn( 0.0 ),
			DividerRadThermalFluxRec( 0.0 ),
			DividerRadThermalFluxRecOld( 0.0 ),
			DivEdgeToCenterGlCondRatio( 1.0 ),
			DividerEdgeArea( 0.0 ),
			DividerTempSurfIn( 23.0 ),
			DividerTempSurfInOld( 23.0 ),
			DividerTempSurfOut( 23.0 ),
			DividerQRadInAbs( 0.0 ),
			DividerQRadOutAbs( 0.0 ),
			ProjCorrDivOut( 0.0 ),
			ProjCorrDivIn( 0.0 ),
			GlazedFrac( 1.0 ),
			OutProjSLFracMult( 24, 1.0 ),
			InOutProjSLFracMult( 24, 1.0 ),
			CenterGlArea( 0.0 ),
			EdgeGlCorrFac( 1.0 ),
			OriginalClass( 0 ),
			ExtBeamAbsByShade( 0.0 ),
			ExtDiffAbsByShade( 0.0 ),
			IntBeamAbsByShade( 0.0 ),
			IntSWAbsByShade( 0.0 ),
			InitialDifSolAbsByShade( 0.0 ),
			IntLWAbsByShade( 0.0 ),
			ShadeAbsFacFace( 2, 0.5 ),
			ConvCoeffWithShade( 0.0 ),
			ConvHeatFlowNatural( 0.0 ),
			ConvHeatGainToZoneAir( 0.0 ),
			RetHeatGainToZoneAir( 0.0 ),
			DividerConduction( 0.0 ),
			OtherConvHeatGain( 0.0 ),
			BlindNumber( 0 ),
			EffShBlindEmiss( MaxSlatAngs, 0.0 ),
			EffGlassEmiss( MaxSlatAngs, 0.0 ),
			EffInsSurfTemp( 23.0 ),
			MovableSlats( false ),
			SlatAngThisTS( 0.0 ),
			SlatAngThisTSDeg( 0.0 ),
			SlatAngThisTSDegEMSon( false ),
			SlatAngThisTSDegEMSValue( 0.0 ),
			SlatsBlockBeam( false ),
			BlindAirFlowPermeability( 0.0 ),
			TotGlazingThickness( 0.0 ),
			ProfileAngHor( 0.0 ),
			ProfileAngVert( 0.0 ),
			TanProfileAngHor( 0.0 ),
			TanProfileAngVert( 0.0 ),
			InsideSillDepth( 0.0 ),
			InsideReveal( 0.0 ),
			InsideSillSolAbs( 0.0 ),
			InsideRevealSolAbs( 0.0 ),
			OutsideRevealSolAbs( 0.0 ),
			BmSolAbsdInsReveal( 0.0 ),
			BmSolRefldInsReveal( 0.0 ),
			BmSolRefldInsRevealReport( 0.0 ),
			BmSolRefldOutsRevealReport( 0.0 ),
			BmSolAbsdOutsReveal( 0.0 ),
			OutsRevealDiffOntoGlazing( 0.0 ),
			InsRevealDiffOntoGlazing( 0.0 ),
			InsRevealDiffIntoZone( 0.0 ),
			OutsRevealDiffOntoFrame( 0.0 ),
			InsRevealDiffOntoFrame( 0.0 ),
			InsRevealDiffOntoGlazingReport( 0.0 ),
			InsRevealDiffIntoZoneReport( 0.0 ),
			InsRevealDiffOntoFrameReport( 0.0 ),
			BmSolAbsdInsRevealReport( 0.0 ),
			BlTsolBmBm( 0.0 ),
			BlTsolBmDif( 0.0 ),
			BlTsolDifDif( 0.0 ),
			BlGlSysTsolBmBm( 0.0 ),
			BlGlSysTsolDifDif( 0.0 ),
			ScreenNumber( 0 ),
			ScTsolBmBm( 0.0 ),
			ScTsolBmDif( 0.0 ),
			ScTsolDifDif( 0.0 ),
			ScGlSysTsolBmBm( 0.0 ),
			ScGlSysTsolDifDif( 0.0 ),
			GlTsolBmBm( 0.0 ),
			GlTsolBmDif( 0.0 ),
			GlTsolDifDif( 0.0 ),
			AirflowSource( 0 ),
			AirflowDestination( 0 ),
			MaxAirflow( 0.0 ),
			AirflowControlType( 0 ),
			AirflowHasSchedule( false ),
			AirflowSchedulePtr( 0 ),
			AirflowThisTS( 0.0 ),
			TAirflowGapOutlet( 0.0 ),
			WindowCalcIterationsRep( 0 ),
			BmSolTransThruIntWinRep( 0.0 ),
			VentingOpenFactorRep( 0.0 ),
			VentingOpenFactorMultRep( 0.0 ),
			InsideTempForVentingRep( 0.0 ),
			VentingAvailabilityRep( 0.0 ),
			IllumFromWinAtRefPt1Rep( 0.0 ),
			IllumFromWinAtRefPt2Rep( 0.0 ),
			LumWinFromRefPt1Rep( 0.0 ),
			LumWinFromRefPt2Rep( 0.0 ),
			SkySolarInc( 0.0 ),
			GndSolarInc( 0.0 ),
			SkyGndSolarInc( 0.0 ),
			BmGndSolarInc( 0.0 ),
			ZoneAreaMinusThisSurf( 3, 0.0 ),
			ZoneAreaReflProdMinusThisSurf( 3, 0.0 ),
			LightWellEff( 1.0 ),
			SolarDiffusing( false ),
			BmSolRefldInsRevealRepEnergy( 0.0 ),
			BmSolRefldOutsRevealRepEnergy( 0.0 ),
			BmSolTransThruIntWinRepEnergy( 0.0 ),
			FrameHeatGain( 0.0 ),
			DividerHeatGain( 0.0 ),
			FrameHeatLoss( 0.0 ),
			DividerHeatLoss( 0.0 ),
			TCLayerTemp( 0.0 ),
			SpecTemp( 0.0 ),
			WindowModelType( Window5DetailedModel )
		{}

		// Member Constructor
		SurfaceWindowCalc(
			int const ShadingFlag, // -1: window has no shading device
			bool const ShadingFlagEMSOn, // EMS control flag, true if EMS is controlling ShadingFlag with ShadingFlagEMSValue
			int const ShadingFlagEMSValue, // EMS control value for Shading Flag
			int const StormWinFlag, // -1: Storm window not applicable
			int const StormWinFlagPrevDay, // Previous time step value of StormWinFlag
			Real64 const FracTimeShadingDeviceOn, // For a single time step, = 0.0 if no shading device or shading device is off,
			int const ExtIntShadePrevTS, // 1 if exterior or interior blind or shade in place previous time step;
			int const ShadedConstruction, // For windows with shading, the construction with shading
			bool const SurfDayLightInit, // surface has been initialized for following 5 arrays
			FArray1< Real64 > const & SolidAngAtRefPt, // Solid angle subtended by window from daylit ref points 1 and 2
			FArray1< Real64 > const & SolidAngAtRefPtWtd, // Solid angle subtended by window from
			FArray2< Real64 > const & IllumFromWinAtRefPt, // Illuminance from window at ref pts for window
			FArray2< Real64 > const & BackLumFromWinAtRefPt, // Window background luminance from window wrt ref pts (cd/m2)
			FArray2< Real64 > const & SourceLumFromWinAtRefPt, // Window luminance at ref pts for window
			int const DaylFacPoint, // Pointer to daylight factors for the window
			Real64 const VisTransSelected, // Window vis trans at normal incidence selected for use in dayltg calculation
			Real64 const SwitchingFactor, // Window switching factor (0.0 = unswitched; 1.0 = fully switched)
			FArray1< Real64 > const & WinCenter, // X,Y,Z coordinates of window center point in building coord system
			Real64 const Theta, // Azimuth of window normal (rad)
			Real64 const Phi, // Altitude of window normal (rad)
			Real64 const RhoCeilingWall, // Average interior reflectance seen by light moving up across horizontal
			Real64 const RhoFloorWall, // Same as above, but for light moving down
			Real64 const FractionUpgoing, // Fraction light entering window that goes upward
			Real64 const VisTransRatio, // For windows with switchable glazing, ratio of normal transmittance
			FArray1< Real64 > const & ThetaFace, // Face temperatures of window layers (K)
			Real64 const IRfromParentZone, // Incident IR from parent zone (W/m2)
			int const IRErrCount, // For recurring error counts
			int const IRErrCountC, // For recurring error counts (continuation)
			Real64 const FrameArea, // Frame projected area (m2)
			Real64 const FrameConductance, // Frame conductance [no air films] (W/m2-K)
			Real64 const FrameSolAbsorp, // Frame solar absorptance (assumed same inside and outside)
			Real64 const FrameVisAbsorp, // Frame visible absorptance (assumed same inside and outside)
			Real64 const FrameEmis, // Frame thermal emissivity (thermal absorptance) (assumed same
			Real64 const FrameAreaXEmiss, // Frame area times thermal emissivity (m2)
			Real64 const FrameRadExchangeFactor, // Frame IR radiant exchange factor
			Real64 const FrameHRadLinIn, // Frame linearized inside IR radiation conductance (W/m2-K)
			Real64 const FrameRadThermalFluxRec, // Frame inside IR flux received (W/m2)
			Real64 const FrameRadThermalFluxRecOld, // Previous value of frame inside IR flux received (W/m2)
			Real64 const FrEdgeToCenterGlCondRatio, // Ratio of frame edge of glass conductance (without air films) to
			Real64 const FrameEdgeArea, // Area of glass near frame (m2)
			Real64 const FrameTempSurfIn, // Frame inside surface temperature (C)
			Real64 const FrameTempSurfInOld, // Previous value of frame inside surface temperature (C)
			Real64 const FrameTempSurfOut, // Frame outside surface temperature (C)
			Real64 const FrameQRadInAbs, // Radiation absorbed by inside of frame (short-wave from solar
			Real64 const FrameQRadOutAbs, // Radiation absorbed by outside of frame (solar) (W/m2)
			Real64 const ProjCorrFrOut, // Correction factor to absorbed radiation due to frame outside projection
			Real64 const ProjCorrFrIn, // Correction factor to absorbed radiation due to frame inside projection
			int const DividerType, // Divider type (1=DividedLite, 2=Suspended (between-pane))
			Real64 const DividerArea, // Divider projected area (m2)
			Real64 const DividerConductance, // Divider conductance [no air films] (W/m2-K)
			Real64 const DividerSolAbsorp, // Divider solar absorptance (assumed same inside and outside)
			Real64 const DividerVisAbsorp, // Divider visible absorptance (assumed same inside and outside)
			Real64 const DividerEmis, // Divider thermal emissivity (thermal absorptance) (assumed same
			Real64 const DividerAreaXEmiss, // Divider area times thermal emissivity (m2)
			Real64 const DividerRadExchangeFactor, // Divider IR radiant exchange factor
			Real64 const DividerHRadLinIn, // Divider linearized inside IR radiation conductance (W/m2-K)
			Real64 const DividerRadThermalFluxRec, // Divider inside IR flux received (W/m2)
			Real64 const DividerRadThermalFluxRecOld, // Previous value of divider inside IR flux received (W/m2)
			Real64 const DivEdgeToCenterGlCondRatio, // Ratio of divider edge of glass conductance (without air films) to
			Real64 const DividerEdgeArea, // Area of glass near dividers (m2)
			Real64 const DividerTempSurfIn, // Divider inside surface temperature (C)
			Real64 const DividerTempSurfInOld, // Previous value of divider inside surface temperature (C)
			Real64 const DividerTempSurfOut, // Divider outside surface temperature (C)
			Real64 const DividerQRadInAbs, // Radiation absorbed by inside of divider (short-wave from solar
			Real64 const DividerQRadOutAbs, // Radiation absorbed by outside of divider (solar) (W/m2)
			Real64 const ProjCorrDivOut, // Correction factor to absorbed radiation due to divider outside projection
			Real64 const ProjCorrDivIn, // Correction factor to absorbed radiation due to divider inside projection
			Real64 const GlazedFrac, // (Glazed area)/(Glazed area + divider area)
			FArray1< Real64 > const & OutProjSLFracMult, // Multiplier on sunlit fraction due to shadowing of glass by frame
			FArray1< Real64 > const & InOutProjSLFracMult, // Multiplier on sunlit fraction due to shadowing of glass by frame
			Real64 const CenterGlArea, // Center of glass area (m2); area of glass where 1-D conduction dominates
			Real64 const EdgeGlCorrFac, // Correction factor to center-of-glass conductance to account for
			int const OriginalClass, // 0 or if entered originally as:
			Real64 const ExtBeamAbsByShade, // Exterior beam solar absorbed by window shade (W/m2)
			Real64 const ExtDiffAbsByShade, // Exterior diffuse solar absorbed by window shade (W/m2)
			Real64 const IntBeamAbsByShade, // Interior beam solar absorbed by window shade (W/m2)
			Real64 const IntSWAbsByShade, // Interior diffuse solar plus short-wave from lights absorbed by window shade (W/m2)
			Real64 const InitialDifSolAbsByShade, // Initial diffuse solar from ext and int windows absorbed by window shade (W/m2)
			Real64 const IntLWAbsByShade, // Interior long-wave from zone lights and equipment absorbed by window shade (W/m2)
			FArray1< Real64 > const & ShadeAbsFacFace, // Fraction of short-wave radiation incident on face 1 that is
			Real64 const ConvCoeffWithShade, // Convection coefficient from glass or shade to gap air when
			Real64 const ConvHeatFlowNatural, // Convective heat flow from gap between glass and interior shade or blind (W)
			Real64 const ConvHeatGainToZoneAir, // Convective heat gain to zone air from window gap airflow (W)
			Real64 const RetHeatGainToZoneAir, // Convective heat gain to return air sent to zone [W]
			Real64 const DividerConduction, // Conduction through divider from outside to inside face (W)
			Real64 const OtherConvHeatGain, // other convective = total conv - standard model prediction for EQL window model (W)
			int const BlindNumber, // Blind number for a window with a blind
			FArray1< Real64 > const & EffShBlindEmiss, // Effective emissivity of interior blind or shade
			FArray1< Real64 > const & EffGlassEmiss, // Effective emissivity of glass adjacent to interior blind or shade
			Real64 const EffInsSurfTemp, // Effective inside surface temperature for window with interior blind or
			bool const MovableSlats, // True if window has a blind with movable slats
			Real64 const SlatAngThisTS, // Slat angle this time step for window with blind on (radians)
			Real64 const SlatAngThisTSDeg, // Slat angle this time step for window with blind on (deg)
			bool const SlatAngThisTSDegEMSon, // flag that indicate EMS system is actuating SlatAngThisTSDeg
			Real64 const SlatAngThisTSDegEMSValue, // value that EMS sets for slat angle in degrees
			bool const SlatsBlockBeam, // True if blind slats block incident beam solar
			Real64 const BlindAirFlowPermeability, // Blind air-flow permeability for calculation of convective flow
			Real64 const TotGlazingThickness, // Total glazing thickness from outside of outer glass to inside of inner glass (m)
			Real64 const ProfileAngHor, // Horizontal beam solar profile angle (degrees)
			Real64 const ProfileAngVert, // Vertical beam solar profile angle (degrees)
			Real64 const TanProfileAngHor, // Tangent of horizontal profile angle
			Real64 const TanProfileAngVert, // Tangent of vertical profile angle
			Real64 const InsideSillDepth, // Depth of inside sill (m)
			Real64 const InsideReveal, // Depth of inside reveal (m)
			Real64 const InsideSillSolAbs, // Solar absorptance of inside sill
			Real64 const InsideRevealSolAbs, // Solar absorptance of inside reveal
			Real64 const OutsideRevealSolAbs, // Solar absorptance of outside reveal
			Real64 const BmSolAbsdInsReveal, // Multiplied by BeamSolarRad, gives beam solar absorbed
			Real64 const BmSolRefldInsReveal, // Multiplied by BeamSolarRad, gives beam solar reflected
			Real64 const BmSolRefldInsRevealReport, // Beam solar reflected by inside reveal surfaces, for reporting (W)
			Real64 const BmSolRefldOutsRevealReport, // Beam solar reflected by outside reveal surfaces, for reporting (m2)
			Real64 const BmSolAbsdOutsReveal, // Multiplied by BeamSolarRad, gives beam solar absorbed by
			Real64 const OutsRevealDiffOntoGlazing, // Multiplied by BeamSolarRad, gives diffuse from beam reflection from
			Real64 const InsRevealDiffOntoGlazing, // Multiplied by BeamSolarRad, gives diffuse from beam reflection
			Real64 const InsRevealDiffIntoZone, // Multiplied by BeamSolarRad, gives diffuse from beam reflection
			Real64 const OutsRevealDiffOntoFrame, // Multiplied by BeamSolarRad, gives diffuse from beam reflection from outside reveal
			Real64 const InsRevealDiffOntoFrame, // Multiplied by BeamSolarRad, gives diffuse from beam reflection from inside reveal
			Real64 const InsRevealDiffOntoGlazingReport, // Diffuse solar from beam reflection
			Real64 const InsRevealDiffIntoZoneReport, // Diffuse from beam reflection
			Real64 const InsRevealDiffOntoFrameReport, // Diffuse from beam reflection from inside reveal
			Real64 const BmSolAbsdInsRevealReport, // Beam solar absorbed by inside reveal (W)
			Real64 const BlTsolBmBm, // Time-step value of blind beam-beam solar transmittance (-)
			Real64 const BlTsolBmDif, // Time-step value of blind beam-diffuse solar transmittance (-)
			Real64 const BlTsolDifDif, // Time-step value of blind diffuse-diffuse solar transmittance (-)
			Real64 const BlGlSysTsolBmBm, // Time-step value of blind/glass system beam-beam solar transmittance (-)
			Real64 const BlGlSysTsolDifDif, // Time-step value of blind/glass system diffuse-diffuse solar transmittance (-)
			int const ScreenNumber, // Screen number for a window with a screen (do not confuse with material number)
			Real64 const ScTsolBmBm, // Time-step value of screen beam-beam solar transmittance (-)
			Real64 const ScTsolBmDif, // Time-step value of screen beam-diffuse solar transmittance (-)
			Real64 const ScTsolDifDif, // Time-step value of screen diffuse-diffuse solar transmittance (-)
			Real64 const ScGlSysTsolBmBm, // Time-step value of screen/glass system beam-beam solar transmittance (-)
			Real64 const ScGlSysTsolDifDif, // Time-step value of screen/glass system diffuse-diffuse solar transmittance (-)
			Real64 const GlTsolBmBm, // Time-step value of glass beam-beam solar transmittance (-)
			Real64 const GlTsolBmDif, // Time-step value of glass beam-diffuse solar transmittance (-)
			Real64 const GlTsolDifDif, // Time-step value of glass diffuse-diffuse solar transmittance (-)
			int const AirflowSource, // Source of gap airflow (INSIDEAIR, OUTSIDEAIR, etc.)
			int const AirflowDestination, // Destination of gap airflow (INSIDEAIR, OUTSIDEAIR, etc.)
			Real64 const MaxAirflow, // Maximum gap airflow (m3/s per m of glazing width)
			int const AirflowControlType, // Gap airflow control type (ALWAYSONATMAXFLOW, etc.)
			bool const AirflowHasSchedule, // True if gap airflow is scheduled
			int const AirflowSchedulePtr, // Gap airflow schedule pointer
			Real64 const AirflowThisTS, // Gap airflow this timestep (m3/s per m of glazing width)
			Real64 const TAirflowGapOutlet, // Temperature of air leaving airflow gap between glass panes (C)
			int const WindowCalcIterationsRep, // Number of iterations in window heat balance calculation
			Real64 const BmSolTransThruIntWinRep, // Beam solar transmitted through interior window [W]
			Real64 const VentingOpenFactorRep, // Window/door venting open factor, for reporting
			Real64 const VentingOpenFactorMultRep, // Window/door opening modulation multiplier on venting open factor, for reporting
			Real64 const InsideTempForVentingRep, // Inside air temp used to control window/door venting, for reporting (C)
			Real64 const VentingAvailabilityRep, // Venting availability schedule value (0.0/1.0 = no venting allowed/not allowed)
			Real64 const IllumFromWinAtRefPt1Rep, // Illuminance from window at reference point #1 [lux]
			Real64 const IllumFromWinAtRefPt2Rep, // Illuminance from window at reference point #2 [lux]
			Real64 const LumWinFromRefPt1Rep, // Window luminance as viewed from reference point #1 [cd/m2]
			Real64 const LumWinFromRefPt2Rep, // Window luminance as viewed from reference point #2 [cd/m2]
			Real64 const SkySolarInc, // Incident diffuse solar from sky; if CalcSolRefl is true, includes
			Real64 const GndSolarInc, // Incident diffuse solar from ground; if CalcSolRefl is true, accounts
			Real64 const SkyGndSolarInc, // Incident diffuse solar from ground-reflected sky radiation; used for
			Real64 const BmGndSolarInc, // Incident diffuse solar from ground-reflected beam radiation; used for
			FArray1< Real64 > const & ZoneAreaMinusThisSurf, // Zone inside surface area minus this surface and its subsurfaces
			FArray1< Real64 > const & ZoneAreaReflProdMinusThisSurf, // Zone product of inside surface area times vis reflectance
			Real64 const LightWellEff, // Light well efficiency (multiplier on exterior window vis trans
			bool const SolarDiffusing, // True if exterior window with a construction that contains a
			Real64 const BmSolRefldInsRevealRepEnergy, // energy of BmSolRefldInsRevealReport [J]
			Real64 const BmSolRefldOutsRevealRepEnergy, // energy of BmSolRefldOutsRevealReport [J]
			Real64 const BmSolTransThruIntWinRepEnergy, // energy of BmSolTransThruIntWinRep [J]
			Real64 const FrameHeatGain,
			Real64 const DividerHeatGain,
			Real64 const FrameHeatLoss,
			Real64 const DividerHeatLoss,
			Real64 const TCLayerTemp, // The temperature of the thermochromic layer of the window
			Real64 const SpecTemp, // The specification temperature of the TC layer glass
			int const WindowModelType, // if set to WindowBSDFModel, then uses BSDF methods
			BSDFWindowDescript const & ComplexFen // Data for complex fenestration, see DataBSDFWindow.cc for declaration
		) :
			ShadingFlag( ShadingFlag ),
			ShadingFlagEMSOn( ShadingFlagEMSOn ),
			ShadingFlagEMSValue( ShadingFlagEMSValue ),
			StormWinFlag( StormWinFlag ),
			StormWinFlagPrevDay( StormWinFlagPrevDay ),
			FracTimeShadingDeviceOn( FracTimeShadingDeviceOn ),
			ExtIntShadePrevTS( ExtIntShadePrevTS ),
			ShadedConstruction( ShadedConstruction ),
			SurfDayLightInit( SurfDayLightInit ),
			SolidAngAtRefPt( SolidAngAtRefPt ),
			SolidAngAtRefPtWtd( SolidAngAtRefPtWtd ),
			IllumFromWinAtRefPt( IllumFromWinAtRefPt ),
			BackLumFromWinAtRefPt( BackLumFromWinAtRefPt ),
			SourceLumFromWinAtRefPt( SourceLumFromWinAtRefPt ),
			DaylFacPoint( DaylFacPoint ),
			VisTransSelected( VisTransSelected ),
			SwitchingFactor( SwitchingFactor ),
			WinCenter( 3, WinCenter ),
			Theta( Theta ),
			Phi( Phi ),
			RhoCeilingWall( RhoCeilingWall ),
			RhoFloorWall( RhoFloorWall ),
			FractionUpgoing( FractionUpgoing ),
			VisTransRatio( VisTransRatio ),
			ThetaFace( 10, ThetaFace ),
			IRfromParentZone( IRfromParentZone ),
			IRErrCount( IRErrCount ),
			IRErrCountC( IRErrCountC ),
			FrameArea( FrameArea ),
			FrameConductance( FrameConductance ),
			FrameSolAbsorp( FrameSolAbsorp ),
			FrameVisAbsorp( FrameVisAbsorp ),
			FrameEmis( FrameEmis ),
			FrameAreaXEmiss( FrameAreaXEmiss ),
			FrameRadExchangeFactor( FrameRadExchangeFactor ),
			FrameHRadLinIn( FrameHRadLinIn ),
			FrameRadThermalFluxRec( FrameRadThermalFluxRec ),
			FrameRadThermalFluxRecOld( FrameRadThermalFluxRecOld ),
			FrEdgeToCenterGlCondRatio( FrEdgeToCenterGlCondRatio ),
			FrameEdgeArea( FrameEdgeArea ),
			FrameTempSurfIn( FrameTempSurfIn ),
			FrameTempSurfInOld( FrameTempSurfInOld ),
			FrameTempSurfOut( FrameTempSurfOut ),
			FrameQRadInAbs( FrameQRadInAbs ),
			FrameQRadOutAbs( FrameQRadOutAbs ),
			ProjCorrFrOut( ProjCorrFrOut ),
			ProjCorrFrIn( ProjCorrFrIn ),
			DividerType( DividerType ),
			DividerArea( DividerArea ),
			DividerConductance( DividerConductance ),
			DividerSolAbsorp( DividerSolAbsorp ),
			DividerVisAbsorp( DividerVisAbsorp ),
			DividerEmis( DividerEmis ),
			DividerAreaXEmiss( DividerAreaXEmiss ),
			DividerRadExchangeFactor( DividerRadExchangeFactor ),
			DividerHRadLinIn( DividerHRadLinIn ),
			DividerRadThermalFluxRec( DividerRadThermalFluxRec ),
			DividerRadThermalFluxRecOld( DividerRadThermalFluxRecOld ),
			DivEdgeToCenterGlCondRatio( DivEdgeToCenterGlCondRatio ),
			DividerEdgeArea( DividerEdgeArea ),
			DividerTempSurfIn( DividerTempSurfIn ),
			DividerTempSurfInOld( DividerTempSurfInOld ),
			DividerTempSurfOut( DividerTempSurfOut ),
			DividerQRadInAbs( DividerQRadInAbs ),
			DividerQRadOutAbs( DividerQRadOutAbs ),
			ProjCorrDivOut( ProjCorrDivOut ),
			ProjCorrDivIn( ProjCorrDivIn ),
			GlazedFrac( GlazedFrac ),
			OutProjSLFracMult( 24, OutProjSLFracMult ),
			InOutProjSLFracMult( 24, InOutProjSLFracMult ),
			CenterGlArea( CenterGlArea ),
			EdgeGlCorrFac( EdgeGlCorrFac ),
			OriginalClass( OriginalClass ),
			ExtBeamAbsByShade( ExtBeamAbsByShade ),
			ExtDiffAbsByShade( ExtDiffAbsByShade ),
			IntBeamAbsByShade( IntBeamAbsByShade ),
			IntSWAbsByShade( IntSWAbsByShade ),
			InitialDifSolAbsByShade( InitialDifSolAbsByShade ),
			IntLWAbsByShade( IntLWAbsByShade ),
			ShadeAbsFacFace( 2, ShadeAbsFacFace ),
			ConvCoeffWithShade( ConvCoeffWithShade ),
			ConvHeatFlowNatural( ConvHeatFlowNatural ),
			ConvHeatGainToZoneAir( ConvHeatGainToZoneAir ),
			RetHeatGainToZoneAir( RetHeatGainToZoneAir ),
			DividerConduction( DividerConduction ),
			OtherConvHeatGain( OtherConvHeatGain ),
			BlindNumber( BlindNumber ),
			EffShBlindEmiss( MaxSlatAngs, EffShBlindEmiss ),
			EffGlassEmiss( MaxSlatAngs, EffGlassEmiss ),
			EffInsSurfTemp( EffInsSurfTemp ),
			MovableSlats( MovableSlats ),
			SlatAngThisTS( SlatAngThisTS ),
			SlatAngThisTSDeg( SlatAngThisTSDeg ),
			SlatAngThisTSDegEMSon( SlatAngThisTSDegEMSon ),
			SlatAngThisTSDegEMSValue( SlatAngThisTSDegEMSValue ),
			SlatsBlockBeam( SlatsBlockBeam ),
			BlindAirFlowPermeability( BlindAirFlowPermeability ),
			TotGlazingThickness( TotGlazingThickness ),
			ProfileAngHor( ProfileAngHor ),
			ProfileAngVert( ProfileAngVert ),
			TanProfileAngHor( TanProfileAngHor ),
			TanProfileAngVert( TanProfileAngVert ),
			InsideSillDepth( InsideSillDepth ),
			InsideReveal( InsideReveal ),
			InsideSillSolAbs( InsideSillSolAbs ),
			InsideRevealSolAbs( InsideRevealSolAbs ),
			OutsideRevealSolAbs( OutsideRevealSolAbs ),
			BmSolAbsdInsReveal( BmSolAbsdInsReveal ),
			BmSolRefldInsReveal( BmSolRefldInsReveal ),
			BmSolRefldInsRevealReport( BmSolRefldInsRevealReport ),
			BmSolRefldOutsRevealReport( BmSolRefldOutsRevealReport ),
			BmSolAbsdOutsReveal( BmSolAbsdOutsReveal ),
			OutsRevealDiffOntoGlazing( OutsRevealDiffOntoGlazing ),
			InsRevealDiffOntoGlazing( InsRevealDiffOntoGlazing ),
			InsRevealDiffIntoZone( InsRevealDiffIntoZone ),
			OutsRevealDiffOntoFrame( OutsRevealDiffOntoFrame ),
			InsRevealDiffOntoFrame( InsRevealDiffOntoFrame ),
			InsRevealDiffOntoGlazingReport( InsRevealDiffOntoGlazingReport ),
			InsRevealDiffIntoZoneReport( InsRevealDiffIntoZoneReport ),
			InsRevealDiffOntoFrameReport( InsRevealDiffOntoFrameReport ),
			BmSolAbsdInsRevealReport( BmSolAbsdInsRevealReport ),
			BlTsolBmBm( BlTsolBmBm ),
			BlTsolBmDif( BlTsolBmDif ),
			BlTsolDifDif( BlTsolDifDif ),
			BlGlSysTsolBmBm( BlGlSysTsolBmBm ),
			BlGlSysTsolDifDif( BlGlSysTsolDifDif ),
			ScreenNumber( ScreenNumber ),
			ScTsolBmBm( ScTsolBmBm ),
			ScTsolBmDif( ScTsolBmDif ),
			ScTsolDifDif( ScTsolDifDif ),
			ScGlSysTsolBmBm( ScGlSysTsolBmBm ),
			ScGlSysTsolDifDif( ScGlSysTsolDifDif ),
			GlTsolBmBm( GlTsolBmBm ),
			GlTsolBmDif( GlTsolBmDif ),
			GlTsolDifDif( GlTsolDifDif ),
			AirflowSource( AirflowSource ),
			AirflowDestination( AirflowDestination ),
			MaxAirflow( MaxAirflow ),
			AirflowControlType( AirflowControlType ),
			AirflowHasSchedule( AirflowHasSchedule ),
			AirflowSchedulePtr( AirflowSchedulePtr ),
			AirflowThisTS( AirflowThisTS ),
			TAirflowGapOutlet( TAirflowGapOutlet ),
			WindowCalcIterationsRep( WindowCalcIterationsRep ),
			BmSolTransThruIntWinRep( BmSolTransThruIntWinRep ),
			VentingOpenFactorRep( VentingOpenFactorRep ),
			VentingOpenFactorMultRep( VentingOpenFactorMultRep ),
			InsideTempForVentingRep( InsideTempForVentingRep ),
			VentingAvailabilityRep( VentingAvailabilityRep ),
			IllumFromWinAtRefPt1Rep( IllumFromWinAtRefPt1Rep ),
			IllumFromWinAtRefPt2Rep( IllumFromWinAtRefPt2Rep ),
			LumWinFromRefPt1Rep( LumWinFromRefPt1Rep ),
			LumWinFromRefPt2Rep( LumWinFromRefPt2Rep ),
			SkySolarInc( SkySolarInc ),
			GndSolarInc( GndSolarInc ),
			SkyGndSolarInc( SkyGndSolarInc ),
			BmGndSolarInc( BmGndSolarInc ),
			ZoneAreaMinusThisSurf( 3, ZoneAreaMinusThisSurf ),
			ZoneAreaReflProdMinusThisSurf( 3, ZoneAreaReflProdMinusThisSurf ),
			LightWellEff( LightWellEff ),
			SolarDiffusing( SolarDiffusing ),
			BmSolRefldInsRevealRepEnergy( BmSolRefldInsRevealRepEnergy ),
			BmSolRefldOutsRevealRepEnergy( BmSolRefldOutsRevealRepEnergy ),
			BmSolTransThruIntWinRepEnergy( BmSolTransThruIntWinRepEnergy ),
			FrameHeatGain( FrameHeatGain ),
			DividerHeatGain( DividerHeatGain ),
			FrameHeatLoss( FrameHeatLoss ),
			DividerHeatLoss( DividerHeatLoss ),
			TCLayerTemp( TCLayerTemp ),
			SpecTemp( SpecTemp ),
			WindowModelType( WindowModelType ),
			ComplexFen( ComplexFen )
		{}

	};

	struct FrameDividerProperties
	{
		// Members
		std::string Name; // Name of frame/divider
		Real64 FrameWidth; // Average width of frame in plane of window {m}
		Real64 FrameProjectionOut; // Distance normal to window between outside face of outer pane
		//  and outside of frame {m}
		Real64 FrameProjectionIn; // Distance normal to window between inside face of inner pane
		//  and inside of frame {m}
		Real64 FrameConductance; // Effective conductance of frame (no air films) {W/m2-K}
		Real64 FrameEdgeWidth; // default 2.5 in ! Width of glass edge region near frame {m}
		Real64 FrEdgeToCenterGlCondRatio; // Ratio of frame edge of glass conductance (without air films) to
		// center of glass conductance (without air films)
		Real64 FrameSolAbsorp; // Solar absorptance of frame corrected for self-shading
		Real64 FrameVisAbsorp; // Visible absorptance of frame corrected for self-shading
		Real64 FrameEmis; // Thermal emissivity of frame
		int DividerType; // Type of divider {DividedLite or Suspended (between-glass}
		Real64 DividerWidth; // Average width of divider in plane of window {m}
		int HorDividers; // Number of horizontal dividers
		int VertDividers; // Number of vertical dividers
		Real64 DividerProjectionOut; // Distance normal to window between outside face of outer pane
		//  and outside of divider {m}
		Real64 DividerProjectionIn; // Distance normal to window between inside face of inner pane
		//  and inside of divider {m}
		Real64 DividerEdgeWidth; // default 2.5 in ! Width of glass edge region near divider
		Real64 DividerConductance; // Effective conductance of divider (no air films) {W/m2-K}
		Real64 DivEdgeToCenterGlCondRatio; // Ratio of divider edge of glass conductance (without air films) to
		// center of glass conductance (without air films)
		Real64 DividerSolAbsorp; // Solar absorptance of divider corrected for self-shading
		Real64 DividerVisAbsorp; // Visible absorptance of divider corrected for self-shading
		Real64 DividerEmis; // Thermal emissivity of divider
		int MullionOrientation; // Horizontal or Vertical; used only for windows with two glazing systems
		//  divided by a mullion; obtained from Window5 data file.
		Real64 OutsideRevealSolAbs; // Solar absorptance of outside reveal
		Real64 InsideSillDepth; // Inside sill depth (m)
		Real64 InsideReveal; // Inside reveal (m)
		Real64 InsideSillSolAbs; // Solar absorptance of inside sill
		Real64 InsideRevealSolAbs; // Solar absorptance of inside reveal

		// Default Constructor
		FrameDividerProperties() :
			FrameWidth( 0.0 ),
			FrameProjectionOut( 0.0 ),
			FrameProjectionIn( 0.0 ),
			FrameConductance( 0.0 ),
			FrameEdgeWidth( 0.06355 ),
			FrEdgeToCenterGlCondRatio( 1.0 ),
			FrameSolAbsorp( 0.0 ),
			FrameVisAbsorp( 0.0 ),
			FrameEmis( 0.9 ),
			DividerType( 0 ),
			DividerWidth( 0.0 ),
			HorDividers( 0 ),
			VertDividers( 0 ),
			DividerProjectionOut( 0.0 ),
			DividerProjectionIn( 0.0 ),
			DividerEdgeWidth( 0.06355 ),
			DividerConductance( 0.0 ),
			DivEdgeToCenterGlCondRatio( 1.0 ),
			DividerSolAbsorp( 0.0 ),
			DividerVisAbsorp( 0.0 ),
			DividerEmis( 0.9 ),
			MullionOrientation( 0 ),
			OutsideRevealSolAbs( 0.0 ),
			InsideSillDepth( 0.0 ),
			InsideReveal( 0.0 ),
			InsideSillSolAbs( 0.0 ),
			InsideRevealSolAbs( 0.0 )
		{}

		// Member Constructor
		FrameDividerProperties(
			std::string const & Name, // Name of frame/divider
			Real64 const FrameWidth, // Average width of frame in plane of window {m}
			Real64 const FrameProjectionOut, // Distance normal to window between outside face of outer pane
			Real64 const FrameProjectionIn, // Distance normal to window between inside face of inner pane
			Real64 const FrameConductance, // Effective conductance of frame (no air films) {W/m2-K}
			Real64 const FrameEdgeWidth, // default 2.5 in ! Width of glass edge region near frame {m}
			Real64 const FrEdgeToCenterGlCondRatio, // Ratio of frame edge of glass conductance (without air films) to
			Real64 const FrameSolAbsorp, // Solar absorptance of frame corrected for self-shading
			Real64 const FrameVisAbsorp, // Visible absorptance of frame corrected for self-shading
			Real64 const FrameEmis, // Thermal emissivity of frame
			int const DividerType, // Type of divider {DividedLite or Suspended (between-glass}
			Real64 const DividerWidth, // Average width of divider in plane of window {m}
			int const HorDividers, // Number of horizontal dividers
			int const VertDividers, // Number of vertical dividers
			Real64 const DividerProjectionOut, // Distance normal to window between outside face of outer pane
			Real64 const DividerProjectionIn, // Distance normal to window between inside face of inner pane
			Real64 const DividerEdgeWidth, // default 2.5 in ! Width of glass edge region near divider
			Real64 const DividerConductance, // Effective conductance of divider (no air films) {W/m2-K}
			Real64 const DivEdgeToCenterGlCondRatio, // Ratio of divider edge of glass conductance (without air films) to
			Real64 const DividerSolAbsorp, // Solar absorptance of divider corrected for self-shading
			Real64 const DividerVisAbsorp, // Visible absorptance of divider corrected for self-shading
			Real64 const DividerEmis, // Thermal emissivity of divider
			int const MullionOrientation, // Horizontal or Vertical; used only for windows with two glazing systems
			Real64 const OutsideRevealSolAbs, // Solar absorptance of outside reveal
			Real64 const InsideSillDepth, // Inside sill depth (m)
			Real64 const InsideReveal, // Inside reveal (m)
			Real64 const InsideSillSolAbs, // Solar absorptance of inside sill
			Real64 const InsideRevealSolAbs // Solar absorptance of inside reveal
		) :
			Name( Name ),
			FrameWidth( FrameWidth ),
			FrameProjectionOut( FrameProjectionOut ),
			FrameProjectionIn( FrameProjectionIn ),
			FrameConductance( FrameConductance ),
			FrameEdgeWidth( FrameEdgeWidth ),
			FrEdgeToCenterGlCondRatio( FrEdgeToCenterGlCondRatio ),
			FrameSolAbsorp( FrameSolAbsorp ),
			FrameVisAbsorp( FrameVisAbsorp ),
			FrameEmis( FrameEmis ),
			DividerType( DividerType ),
			DividerWidth( DividerWidth ),
			HorDividers( HorDividers ),
			VertDividers( VertDividers ),
			DividerProjectionOut( DividerProjectionOut ),
			DividerProjectionIn( DividerProjectionIn ),
			DividerEdgeWidth( DividerEdgeWidth ),
			DividerConductance( DividerConductance ),
			DivEdgeToCenterGlCondRatio( DivEdgeToCenterGlCondRatio ),
			DividerSolAbsorp( DividerSolAbsorp ),
			DividerVisAbsorp( DividerVisAbsorp ),
			DividerEmis( DividerEmis ),
			MullionOrientation( MullionOrientation ),
			OutsideRevealSolAbs( OutsideRevealSolAbs ),
			InsideSillDepth( InsideSillDepth ),
			InsideReveal( InsideReveal ),
			InsideSillSolAbs( InsideSillSolAbs ),
			InsideRevealSolAbs( InsideRevealSolAbs )
		{}

	};

	struct StormWindowData
	{
		// Members
		int BaseWindowNum; // Surface number of associated exterior window
		int StormWinMaterialNum; // Material number of storm window glass
		Real64 StormWinDistance; // Distance between storm window glass and adjacent glass (m)
		int DateOn; // Date (julian) storm window is put on
		int MonthOn; // Month storm window is put on
		int DayOfMonthOn; // Day of month storm window is put on
		int DateOff; // Date (julian) storm window is taken off
		int MonthOff; // Month storm window is taken off
		int DayOfMonthOff; // Day of month storm window is taken off

		// Default Constructor
		StormWindowData() :
			BaseWindowNum( 0 ),
			StormWinMaterialNum( 0 ),
			StormWinDistance( 0.0 ),
			DateOn( 0 ),
			MonthOn( 0 ),
			DayOfMonthOn( 0 ),
			DateOff( 0 ),
			MonthOff( 0 ),
			DayOfMonthOff( 0 )
		{}

		// Member Constructor
		StormWindowData(
			int const BaseWindowNum, // Surface number of associated exterior window
			int const StormWinMaterialNum, // Material number of storm window glass
			Real64 const StormWinDistance, // Distance between storm window glass and adjacent glass (m)
			int const DateOn, // Date (julian) storm window is put on
			int const MonthOn, // Month storm window is put on
			int const DayOfMonthOn, // Day of month storm window is put on
			int const DateOff, // Date (julian) storm window is taken off
			int const MonthOff, // Month storm window is taken off
			int const DayOfMonthOff // Day of month storm window is taken off
		) :
			BaseWindowNum( BaseWindowNum ),
			StormWinMaterialNum( StormWinMaterialNum ),
			StormWinDistance( StormWinDistance ),
			DateOn( DateOn ),
			MonthOn( MonthOn ),
			DayOfMonthOn( DayOfMonthOn ),
			DateOff( DateOff ),
			MonthOff( MonthOff ),
			DayOfMonthOff( DayOfMonthOff )
		{}

	};

	struct WindowShadingControlData
	{
		// Members
		std::string Name; // User supplied name of this set of shading control data
		int ShadingType; // Shading type (InteriorShade, SwitchableGlazing,
		//  CHARACTER(len=32) :: ShadingType    = ' ' ! Shading type (InteriorShade, SwitchableGlazing,
		//  ExteriorShade,InteriorBlind,ExteriorBlind,BetweenGlassShade,
		//  BetweenGlassBlind, or ExteriorScreen)
		int ShadedConstruction; // Pointer to the shaded construction (for ShadingType=ExteriorScreen,InteriorShade,
		//  ExteriorShade,BetweenGlassShade,InteriorBlind,ExteriorBlind,BetweenGlassBlind;
		//  this must be a window construction with a screen, shade or blind layer)
		int ShadingDevice; // Pointer to the material for the shading device (for ShadingType=InteriorShade,
		//  ExteriorShade,BetweenGlassShade,InteriorBlind,ExteriorBlind,BetweenGlassBlind,
		//  ExteriorScreen;
		//  this must be a Material:WindowShade, Material:WindowScreen, or Material:WindowBlind
		int ShadingControlType; // Takes one of the following values that specifies type of shading control
		//  CHARACTER(len=60) :: ShadingControlType =' ' ! Takes one of the following values that specifies type of shading control
		// (control is active only when schedule value = 1; if no schedule
		// specified, schedule value defaults to 1)
		//  AlwaysOn: always shaded; not affected by schedule
		//  AlwaysOff: never shaded; not affected by schedule
		//  OnIfScheduleAllows: unshaded if sch val = 0, shaded if = 1
		//  OnIfHighSolarOnWindow: shaded if incident direct + diffuse > setpoint (W/m2 of window)
		//  OnIfHighHorizontalSolar: shaded if direct + diffuse horizontal solar > setpoint
		//   (W/m2 of ground)
		//  OnIfHighOutsideAirTemp: shaded if outside drybulb > setpoint (C)
		//  OnIfHighZoneAirTemp: shaded if previous time step zone temperature > setpoint (C)
		//  OnIfHighZoneCooling: shaded if previous time step zone cooling rate > setpoint (W)
		//  OnIfHighGlare: shaded if total daylight glare index at first daylighting reference point
		//   from all exterior windows in zone > maximum glare specified in daylighting
		//   input for zone.
		//  MeetDaylightIlluminanceSetpoint: shading is adjusted to just meet illuminance setpoint
		//   at first reference point (only for ShadingType=SwitchableGlazing)
		//       The following three controls are used primarily to reduce zone heating load. They
		//       can be used with any shading type but are most appropriate for opaque interior
		//       or exterior shades with a high insulating value ("opaque movable insulation").
		//  OnNightIfLowOutsideTemp/OffDay: shaded at night if outside temp < setpoint (C)
		//  OnNightIfLowInsideTemp/OffDay: shaded at night if previous time step zone air temp < setpoint (C)
		//  OnNightIfHeating/OffDay: shaded  at night if previous time step zone heating rate > setpoint (W)
		//       The following two controls are used to reduce zone heating and cooling loads.
		//       They can be used with any shading type but are most appropriate for translucent
		//       interior or exterior shades with a high insulating value ("translucent movable insulation")
		//  OnNightIfLowOutsideTemp/OnDayIfCooling: shaded at night if outside temp < setpoint (C);
		//                                         shaded daytime if prev. time step cooling rate > 0
		//  OnNightIfHeating/OnDayIfCooling: shaded at night if prev. time step heating rate > setpoint (W);
		//                                         shaded daytime if prev. time step cooling rate > 0
		//       The following two controls are used to reduce zone cooling load. They can be used
		//       with any shading type but are most appropriate for interior or exterior blinds, interior
		//       or exterior shades with low insulating value, or switchable glazing.
		//  OffNight/OnDayIfCoolingAndHighSolarOnWindow: shading off at night; shading on daytime if
		//                                         solar on window > setpoint (W/m2 of window) and
		//                                         prev. time step cooling rate > 0
		//  OnNight/OnDayIfCoolingAndHighSolarOnWindow: shading on at night; shading on daytime if
		//                                         solar on window > setpoint (W/m2 of window) and
		//                                         prev. time step cooling rate > 0
		int Schedule; // Pointer to schedule of 0 and 1 values: 0 => window is not shaded;
		//  1 => window is shaded if Type=Schedule or Type = ScheduleAnd...
		// and setpoint is exceeded.
		Real64 SetPoint; // Control setpoint (dimension depends on Trigger:
		//  W/m2 of window area for solar on window,
		//  W/m2 of ground area for horizontal solar,
		//  deg C for air temp, W for zone heating and
		//  cooling rate). Not used for Shading Control Type =
		//  MeetDaylightIlluminanceSetpoint or OnIfHighGlare.
		Real64 SetPoint2; // Second control setpoint for control types that take two setpoints.
		//   Dimension is deg C or W/m2.
		bool ShadingControlIsScheduled; // True if shading control has a schedule
		bool GlareControlIsActive; // True if shading control to reduce daylight glare is active
		int SlatAngleSchedule; // Pointer to schedule of slat angle values between 0.0 and 180.0 degrees
		int SlatAngleControlForBlinds; // Takes one of the following values that specifies
		//  CHARACTER(len=32) :: SlatAngleControlForBlinds = ' ' ! Takes one of the following values that specifies
		//  how slat angle is controled in a blind when ShadingType =
		//  InteriorBlind, ExteriorBlind or BetweenGlassBlind.
		//  FixedSlatAngle: the slat angle is fixed at the constant value given in the
		//    associated Material:WindowBlind
		//  ScheduledSlatAngle: the slat angle in degrees between 1 and 180 is given
		//    by the schedule with index SlatAngleSchedule
		//  BlockBeamSolar: if beam solar is incident on the window, and a blind is on the
		//    window, the slat angle is adjusted to just block beam solar; otherwise the
		//    slat angle is set to the value given in the associated Material:WindowBlind.

		// Default Constructor
		WindowShadingControlData() :
			ShadingType( WSC_ST_NoShade ),
			ShadedConstruction( 0 ),
			ShadingDevice( 0 ),
			ShadingControlType( 0 ),
			Schedule( 0 ),
			SetPoint( 0.0 ),
			SetPoint2( 0.0 ),
			ShadingControlIsScheduled( false ),
			GlareControlIsActive( false ),
			SlatAngleSchedule( 0 ),
			SlatAngleControlForBlinds( 0 )
		{}

		// Member Constructor
		WindowShadingControlData(
			std::string const & Name, // User supplied name of this set of shading control data
			int const ShadingType, // Shading type (InteriorShade, SwitchableGlazing,
			int const ShadedConstruction, // Pointer to the shaded construction (for ShadingType=ExteriorScreen,InteriorShade,
			int const ShadingDevice, // Pointer to the material for the shading device (for ShadingType=InteriorShade,
			int const ShadingControlType, // Takes one of the following values that specifies type of shading control
			int const Schedule, // Pointer to schedule of 0 and 1 values: 0 => window is not shaded;
			Real64 const SetPoint, // Control setpoint (dimension depends on Trigger:
			Real64 const SetPoint2, // Second control setpoint for control types that take two setpoints.
			bool const ShadingControlIsScheduled, // True if shading control has a schedule
			bool const GlareControlIsActive, // True if shading control to reduce daylight glare is active
			int const SlatAngleSchedule, // Pointer to schedule of slat angle values between 0.0 and 180.0 degrees
			int const SlatAngleControlForBlinds // Takes one of the following values that specifies
		) :
			Name( Name ),
			ShadingType( ShadingType ),
			ShadedConstruction( ShadedConstruction ),
			ShadingDevice( ShadingDevice ),
			ShadingControlType( ShadingControlType ),
			Schedule( Schedule ),
			SetPoint( SetPoint ),
			SetPoint2( SetPoint2 ),
			ShadingControlIsScheduled( ShadingControlIsScheduled ),
			GlareControlIsActive( GlareControlIsActive ),
			SlatAngleSchedule( SlatAngleSchedule ),
			SlatAngleControlForBlinds( SlatAngleControlForBlinds )
		{}

	};

	struct OSCData
	{
		// Members
		std::string Name; // Name of OSC
		Real64 ConstTemp; // User selected constant temperature (degrees C)
		Real64 ConstTempCoef; // Coefficient modifying the user selected constant temperature
		Real64 ExtDryBulbCoef; // Coefficient modifying the external dry bulb temperature
		Real64 GroundTempCoef; // Coefficient modifying the ground temperature
		Real64 SurfFilmCoef; // Combined convective/radiative film coefficient if >0, else use other coefficients
		Real64 WindSpeedCoef; // Coefficient modifying the wind speed term (s/m)
		Real64 ZoneAirTempCoef; // Coefficient modifying the zone air temperature part of the equation
		std::string ConstTempScheduleName; // Schedule name for scheduled outside temp
		int ConstTempScheduleIndex; // Index for scheduled outside temp.
		bool SinusoidalConstTempCoef; // If true then ConstTempCoef varies by sine wave
		Real64 SinusoidPeriod; // period of sine wave variation  (hr)
		Real64 TPreviousCoef; // Coefficient modifying the OSC temp from the previous timestep (dimensionless)
		Real64 TOutsideSurfPast; // Ouside surface temperature from previous timestep {C}
		Real64 MinTempLimit; // Minimum limit on OSC temp {deg C}
		Real64 MaxTempLimit; // Maximum limit on OSC temp {deg C}
		bool MinLimitPresent; // If TRUE then apply minimum limit on calculated OSC temp
		bool MaxLimitPresent; // If TRUE then apply maximum limit on calculated OSC temp
		Real64 OSCTempCalc; // Result of calculated temperature using OSC (degrees C)

		// Default Constructor
		OSCData() :
			ConstTemp( 0.0 ),
			ConstTempCoef( 0.0 ),
			ExtDryBulbCoef( 0.0 ),
			GroundTempCoef( 0.0 ),
			SurfFilmCoef( 0.0 ),
			WindSpeedCoef( 0.0 ),
			ZoneAirTempCoef( 0.0 ),
			ConstTempScheduleIndex( 0 ),
			SinusoidalConstTempCoef( false ),
			SinusoidPeriod( 0.0 ),
			TPreviousCoef( 0.0 ),
			TOutsideSurfPast( 0.0 ),
			MinTempLimit( 0.0 ),
			MaxTempLimit( 0.0 ),
			MinLimitPresent( false ),
			MaxLimitPresent( false ),
			OSCTempCalc( 0.0 )
		{}

		// Member Constructor
		OSCData(
			std::string const & Name, // Name of OSC
			Real64 const ConstTemp, // User selected constant temperature (degrees C)
			Real64 const ConstTempCoef, // Coefficient modifying the user selected constant temperature
			Real64 const ExtDryBulbCoef, // Coefficient modifying the external dry bulb temperature
			Real64 const GroundTempCoef, // Coefficient modifying the ground temperature
			Real64 const SurfFilmCoef, // Combined convective/radiative film coefficient if >0, else use other coefficients
			Real64 const WindSpeedCoef, // Coefficient modifying the wind speed term (s/m)
			Real64 const ZoneAirTempCoef, // Coefficient modifying the zone air temperature part of the equation
			std::string const & ConstTempScheduleName, // Schedule name for scheduled outside temp
			int const ConstTempScheduleIndex, // Index for scheduled outside temp.
			bool const SinusoidalConstTempCoef, // If true then ConstTempCoef varies by sine wave
			Real64 const SinusoidPeriod, // period of sine wave variation  (hr)
			Real64 const TPreviousCoef, // Coefficient modifying the OSC temp from the previous timestep (dimensionless)
			Real64 const TOutsideSurfPast, // Ouside surface temperature from previous timestep {C}
			Real64 const MinTempLimit, // Minimum limit on OSC temp {deg C}
			Real64 const MaxTempLimit, // Maximum limit on OSC temp {deg C}
			bool const MinLimitPresent, // If TRUE then apply minimum limit on calculated OSC temp
			bool const MaxLimitPresent, // If TRUE then apply maximum limit on calculated OSC temp
			Real64 const OSCTempCalc // Result of calculated temperature using OSC (degrees C)
		) :
			Name( Name ),
			ConstTemp( ConstTemp ),
			ConstTempCoef( ConstTempCoef ),
			ExtDryBulbCoef( ExtDryBulbCoef ),
			GroundTempCoef( GroundTempCoef ),
			SurfFilmCoef( SurfFilmCoef ),
			WindSpeedCoef( WindSpeedCoef ),
			ZoneAirTempCoef( ZoneAirTempCoef ),
			ConstTempScheduleName( ConstTempScheduleName ),
			ConstTempScheduleIndex( ConstTempScheduleIndex ),
			SinusoidalConstTempCoef( SinusoidalConstTempCoef ),
			SinusoidPeriod( SinusoidPeriod ),
			TPreviousCoef( TPreviousCoef ),
			TOutsideSurfPast( TOutsideSurfPast ),
			MinTempLimit( MinTempLimit ),
			MaxTempLimit( MaxTempLimit ),
			MinLimitPresent( MinLimitPresent ),
			MaxLimitPresent( MaxLimitPresent ),
			OSCTempCalc( OSCTempCalc )
		{}

	};

	struct OSCMData
	{
		// Members
		std::string Name; // Name of OSCM
		std::string Class; // type of Model for OSCM
		Real64 TConv; // Temperature of bulk air at other side face (degrees C)
		bool EMSOverrideOnTConv; // if true then EMS calling for convection bulk air temp override
		Real64 EMSOverrideTConvValue; // value for convection air temp when overridden
		Real64 HConv; // Convection coefficient (W/m2-K)
		bool EMSOverrideOnHConv; // if true then EMS calling for convection coef override
		Real64 EMSOverrideHConvValue; // value to use for convection coef when overridden
		Real64 TRad; // Effective temperature of surfaces exposed to other side face (degrees C)
		bool EMSOverrideOnTRad; // if true then EMS calling for radiation temp override
		Real64 EMSOverrideTRadValue; // value to use for rad temp when overridden
		Real64 HRad; // Linearized Radiation coefficient (W/m2-K)
		bool EMSOverrideOnHrad; // if true then EMS calling for radiation coef override
		Real64 EMSOverrideHradValue; // value to use for rad coef when overridden

		// Default Constructor
		OSCMData() :
			TConv( 20.0 ),
			EMSOverrideOnTConv( false ),
			EMSOverrideTConvValue( 0.0 ),
			HConv( 4.0 ),
			EMSOverrideOnHConv( false ),
			EMSOverrideHConvValue( 0.0 ),
			TRad( 20.0 ),
			EMSOverrideOnTRad( false ),
			EMSOverrideTRadValue( 0.0 ),
			HRad( 4.0 ),
			EMSOverrideOnHrad( false ),
			EMSOverrideHradValue( 0.0 )
		{}

		// Member Constructor
		OSCMData(
			std::string const & Name, // Name of OSCM
			std::string const & Class, // type of Model for OSCM
			Real64 const TConv, // Temperature of bulk air at other side face (degrees C)
			bool const EMSOverrideOnTConv, // if true then EMS calling for convection bulk air temp override
			Real64 const EMSOverrideTConvValue, // value for convection air temp when overridden
			Real64 const HConv, // Convection coefficient (W/m2-K)
			bool const EMSOverrideOnHConv, // if true then EMS calling for convection coef override
			Real64 const EMSOverrideHConvValue, // value to use for convection coef when overridden
			Real64 const TRad, // Effective temperature of surfaces exposed to other side face (degrees C)
			bool const EMSOverrideOnTRad, // if true then EMS calling for radiation temp override
			Real64 const EMSOverrideTRadValue, // value to use for rad temp when overridden
			Real64 const HRad, // Linearized Radiation coefficient (W/m2-K)
			bool const EMSOverrideOnHrad, // if true then EMS calling for radiation coef override
			Real64 const EMSOverrideHradValue // value to use for rad coef when overridden
		) :
			Name( Name ),
			Class( Class ),
			TConv( TConv ),
			EMSOverrideOnTConv( EMSOverrideOnTConv ),
			EMSOverrideTConvValue( EMSOverrideTConvValue ),
			HConv( HConv ),
			EMSOverrideOnHConv( EMSOverrideOnHConv ),
			EMSOverrideHConvValue( EMSOverrideHConvValue ),
			TRad( TRad ),
			EMSOverrideOnTRad( EMSOverrideOnTRad ),
			EMSOverrideTRadValue( EMSOverrideTRadValue ),
			HRad( HRad ),
			EMSOverrideOnHrad( EMSOverrideOnHrad ),
			EMSOverrideHradValue( EMSOverrideHradValue )
		{}

	};

	struct ConvectionCoefficient
	{
		// Members
		int WhichSurface; // Which surface number this is applied to
		std::string SurfaceName; // Which surface (name)
		int OverrideType; // Override type, 1=value, 2=schedule, 3=model, 4=user curve
		Real64 OverrideValue; // User specified value
		std::string ScheduleName; // Which surface (name)
		int ScheduleIndex; // if type="schedule" is used
		int UserCurveIndex; // if type=UserCurve is used
		int HcModelEq; // if type is one of specific model equations

		// Default Constructor
		ConvectionCoefficient() :
			WhichSurface( 0 ),
			OverrideType( 0 ),
			OverrideValue( 0.0 ),
			ScheduleIndex( 0 ),
			UserCurveIndex( 0 ),
			HcModelEq( 0 )
		{}

		// Member Constructor
		ConvectionCoefficient(
			int const WhichSurface, // Which surface number this is applied to
			std::string const & SurfaceName, // Which surface (name)
			int const OverrideType, // Override type, 1=value, 2=schedule, 3=model, 4=user curve
			Real64 const OverrideValue, // User specified value
			std::string const & ScheduleName, // Which surface (name)
			int const ScheduleIndex, // if type="schedule" is used
			int const UserCurveIndex, // if type=UserCurve is used
			int const HcModelEq // if type is one of specific model equations
		) :
			WhichSurface( WhichSurface ),
			SurfaceName( SurfaceName ),
			OverrideType( OverrideType ),
			OverrideValue( OverrideValue ),
			ScheduleName( ScheduleName ),
			ScheduleIndex( ScheduleIndex ),
			UserCurveIndex( UserCurveIndex ),
			HcModelEq( HcModelEq )
		{}

	};

	struct ShadingVertexData
	{
		// Members
		int NVert;
		FArray1D< Real64 > XV;
		FArray1D< Real64 > YV;
		FArray1D< Real64 > ZV;

		// Default Constructor
		ShadingVertexData()
		{}

		// Member Constructor
		ShadingVertexData(
			int const NVert,
			FArray1< Real64 > const & XV,
			FArray1< Real64 > const & YV,
			FArray1< Real64 > const & ZV
		) :
			NVert( NVert ),
			XV( XV ),
			YV( YV ),
			ZV( ZV )
		{}

	};

	struct ExtVentedCavityStruct
	{
		// Members
		// from input data
		std::string Name;
		std::string OSCMName; // OtherSideConditionsModel
		int OSCMPtr; // OtherSideConditionsModel index
		Real64 Porosity; // fraction of absorber plate [--]
		Real64 LWEmitt; // Thermal Emissivity of Baffle Surface [dimensionless]
		Real64 SolAbsorp; // Solar Absorbtivity of Baffle Surface [dimensionless]
		int BaffleRoughness; // surface roughness for exterior convection calcs.
		Real64 PlenGapThick; // Depth of Plenum Behind Baffle [m]
		int NumSurfs; // a single baffle can have multiple surfaces underneath it
		FArray1D_int SurfPtrs; // = 0  ! array of pointers for participating underlying surfaces
		Real64 HdeltaNPL; // Height scale for Cavity bouyancy  [m]
		Real64 AreaRatio; // Ratio of actual surface are to projected surface area [dimensionless]
		Real64 Cv; // volume-based effectiveness of openings for wind-driven vent when Passive
		Real64 Cd; // discharge coefficient of openings for bouyancy-driven vent when Passive
		// data from elswhere and calculated
		Real64 ActualArea; // Overall Area of Collect with surface corrugations.
		Real64 ProjArea; // Overall Area of Collector projected, as if flat [m2]
		Vector Centroid; // computed centroid
		Real64 TAirCav; // modeled drybulb temperature for air between baffle and wall [C]
		Real64 Tbaffle; // modeled surface temperature for baffle[C]
		Real64 TairLast; // Old Value for modeled drybulb temp of air between baffle and wall [C]
		Real64 TbaffleLast; // Old value for modeled surface temperature for baffle [C]
		Real64 HrPlen; // Modeled radiation coef for OSCM [W/m2-C]
		Real64 HcPlen; // Modeled Convection coef for OSCM [W/m2-C]
		Real64 MdotVent; // air mass flow exchanging with ambient when passive.
		Real64 Tilt; // Tilt from area weighted average of underlying surfaces
		Real64 Azimuth; // Azimuth from area weighted average of underlying surfaces
		Real64 QdotSource; // Source/sink term
		// reporting data
		Real64 Isc; // total incident solar on baffle [W]
		Real64 PassiveACH; // air changes per hour when passive [1/hr]
		Real64 PassiveMdotVent; // Total Nat Vent air change rate  [kg/s]
		Real64 PassiveMdotWind; // Nat Vent air change rate from Wind-driven [kg/s]
		Real64 PassiveMdotTherm; // Nat. Vent air change rate from bouyancy-driven flow [kg/s]

		// Default Constructor
		ExtVentedCavityStruct() :
			OSCMPtr( 0 ),
			Porosity( 0.0 ),
			LWEmitt( 0.0 ),
			SolAbsorp( 0.0 ),
			BaffleRoughness( 1 ),
			PlenGapThick( 0.0 ),
			NumSurfs( 0 ),
			HdeltaNPL( 0.0 ),
			AreaRatio( 0.0 ),
			Cv( 0.0 ),
			Cd( 0.0 ),
			ActualArea( 0.0 ),
			ProjArea( 0.0 ),
			Centroid( 0.0, 0.0, 0.0 ),
			TAirCav( 0.0 ),
			Tbaffle( 0.0 ),
			TairLast( 20.0 ),
			TbaffleLast( 20.0 ),
			HrPlen( 0.0 ),
			HcPlen( 0.0 ),
			MdotVent( 0.0 ),
			Tilt( 0.0 ),
			Azimuth( 0.0 ),
			QdotSource( 0.0 ),
			Isc( 0.0 ),
			PassiveACH( 0.0 ),
			PassiveMdotVent( 0.0 ),
			PassiveMdotWind( 0.0 ),
			PassiveMdotTherm( 0.0 )
		{}

		// Member Constructor
		ExtVentedCavityStruct(
			std::string const & Name,
			std::string const & OSCMName, // OtherSideConditionsModel
			int const OSCMPtr, // OtherSideConditionsModel index
			Real64 const Porosity, // fraction of absorber plate [--]
			Real64 const LWEmitt, // Thermal Emissivity of Baffle Surface [dimensionless]
			Real64 const SolAbsorp, // Solar Absorbtivity of Baffle Surface [dimensionless]
			int const BaffleRoughness, // surface roughness for exterior convection calcs.
			Real64 const PlenGapThick, // Depth of Plenum Behind Baffle [m]
			int const NumSurfs, // a single baffle can have multiple surfaces underneath it
			FArray1_int const & SurfPtrs, // = 0  ! array of pointers for participating underlying surfaces
			Real64 const HdeltaNPL, // Height scale for Cavity bouyancy  [m]
			Real64 const AreaRatio, // Ratio of actual surface are to projected surface area [dimensionless]
			Real64 const Cv, // volume-based effectiveness of openings for wind-driven vent when Passive
			Real64 const Cd, // discharge coefficient of openings for bouyancy-driven vent when Passive
			Real64 const ActualArea, // Overall Area of Collect with surface corrugations.
			Real64 const ProjArea, // Overall Area of Collector projected, as if flat [m2]
			Vector const & Centroid, // computed centroid
			Real64 const TAirCav, // modeled drybulb temperature for air between baffle and wall [C]
			Real64 const Tbaffle, // modeled surface temperature for baffle[C]
			Real64 const TairLast, // Old Value for modeled drybulb temp of air between baffle and wall [C]
			Real64 const TbaffleLast, // Old value for modeled surface temperature for baffle [C]
			Real64 const HrPlen, // Modeled radiation coef for OSCM [W/m2-C]
			Real64 const HcPlen, // Modeled Convection coef for OSCM [W/m2-C]
			Real64 const MdotVent, // air mass flow exchanging with ambient when passive.
			Real64 const Tilt, // Tilt from area weighted average of underlying surfaces
			Real64 const Azimuth, // Azimuth from area weighted average of underlying surfaces
			Real64 const QdotSource, // Source/sink term
			Real64 const Isc, // total incident solar on baffle [W]
			Real64 const PassiveACH, // air changes per hour when passive [1/hr]
			Real64 const PassiveMdotVent, // Total Nat Vent air change rate  [kg/s]
			Real64 const PassiveMdotWind, // Nat Vent air change rate from Wind-driven [kg/s]
			Real64 const PassiveMdotTherm // Nat. Vent air change rate from bouyancy-driven flow [kg/s]
		) :
			Name( Name ),
			OSCMName( OSCMName ),
			OSCMPtr( OSCMPtr ),
			Porosity( Porosity ),
			LWEmitt( LWEmitt ),
			SolAbsorp( SolAbsorp ),
			BaffleRoughness( BaffleRoughness ),
			PlenGapThick( PlenGapThick ),
			NumSurfs( NumSurfs ),
			SurfPtrs( SurfPtrs ),
			HdeltaNPL( HdeltaNPL ),
			AreaRatio( AreaRatio ),
			Cv( Cv ),
			Cd( Cd ),
			ActualArea( ActualArea ),
			ProjArea( ProjArea ),
			Centroid( Centroid ),
			TAirCav( TAirCav ),
			Tbaffle( Tbaffle ),
			TairLast( TairLast ),
			TbaffleLast( TbaffleLast ),
			HrPlen( HrPlen ),
			HcPlen( HcPlen ),
			MdotVent( MdotVent ),
			Tilt( Tilt ),
			Azimuth( Azimuth ),
			QdotSource( QdotSource ),
			Isc( Isc ),
			PassiveACH( PassiveACH ),
			PassiveMdotVent( PassiveMdotVent ),
			PassiveMdotWind( PassiveMdotWind ),
			PassiveMdotTherm( PassiveMdotTherm )
		{}

	};

	struct SurfaceSolarIncident
	{
		// Members
		std::string Name;
		int SurfPtr; // surface pointer
		int ConstrPtr; // construction pointer
		int SchedPtr; // schedule pointer

		// Default Constructor
		SurfaceSolarIncident() :
			SurfPtr( 0 ),
			ConstrPtr( 0 ),
			SchedPtr( 0 )
		{}

		// Member Constructor
		SurfaceSolarIncident(
			std::string const & Name,
			int const SurfPtr, // surface pointer
			int const ConstrPtr, // construction pointer
			int const SchedPtr // schedule pointer
		) :
			Name( Name ),
			SurfPtr( SurfPtr ),
			ConstrPtr( ConstrPtr ),
			SchedPtr( SchedPtr )
		{}

	};

	struct FenestrationSolarAbsorbed
	{
		// Members
		std::string Name;
		int SurfPtr; // surface pointer
		int ConstrPtr; // construction pointer
		int NumOfSched; // number of scheduled layers
		FArray1D_int SchedPtrs; // pointer to schedules for each layer in construction

		// Default Constructor
		FenestrationSolarAbsorbed() :
			SurfPtr( 0 ),
			ConstrPtr( 0 ),
			NumOfSched( 0 )
		{}

		// Member Constructor
		FenestrationSolarAbsorbed(
			std::string const & Name,
			int const SurfPtr, // surface pointer
			int const ConstrPtr, // construction pointer
			int const NumOfSched, // number of scheduled layers
			FArray1_int const & SchedPtrs // pointer to schedules for each layer in construction
		) :
			Name( Name ),
			SurfPtr( SurfPtr ),
			ConstrPtr( ConstrPtr ),
			NumOfSched( NumOfSched ),
			SchedPtrs( SchedPtrs )
		{}

	};

	// Object Data
	extern FArray1D< SurfaceData > Surface;
	extern FArray1D< SurfaceWindowCalc > SurfaceWindow;
	extern FArray1D< FrameDividerProperties > FrameDivider;
	extern FArray1D< StormWindowData > StormWindow;
	extern FArray1D< WindowShadingControlData > WindowShadingControl;
	extern FArray1D< OSCData > OSC;
	extern FArray1D< OSCMData > OSCM;
	extern FArray1D< ConvectionCoefficient > UserIntConvectionCoeffs;
	extern FArray1D< ConvectionCoefficient > UserExtConvectionCoeffs;
	extern FArray1D< ShadingVertexData > ShadeV;
	extern FArray1D< ExtVentedCavityStruct > ExtVentedCavity;
	extern FArray1D< SurfaceSolarIncident > SurfIncSolSSG;
	extern FArray1D< FenestrationSolarAbsorbed > FenLayAbsSSG;

	// Functions

	std::string
	cSurfaceClass( int const ClassNo );

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

} // DataSurfaces

} // EnergyPlus

#endif
