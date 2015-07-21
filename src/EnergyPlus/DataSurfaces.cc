// ObjexxFCL Headers

// EnergyPlus Headers
#include <DataSurfaces.hh>
#include <DataPrecisionGlobals.hh>

namespace EnergyPlus {

namespace DataSurfaces {

	// MODULE INFORMATION:
	//       AUTHOR         Linda Lawrie
	//       DATE WRITTEN   May 2000
	//       MODIFIED       July 2003, (CC) added a flag for reference air temperature
	//                      Dec 2006, DJS (PSU) added logical ecoroof variable
	//                      Dec 2008, TH added new properties to SurfaceWindowCalc for thermochromic windows
	//                      Jul 2011, M.J. Witte and C.O. Pedersen, add new fields to OSC for last T, max and min
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This data-only module contains derived types and other variables
	// associated with Surfaces, their shading calculations, etc.

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataVectorTypes;
	using namespace DataBSDFWindow;

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:
	int const MaxSlatAngs( 19 );

	// Parameters to indicate surface shape for use with the Surface
	// derived type (see below):

	int const Triangle( 1 );
	int const Quadrilateral( 2 );
	int const Rectangle( 3 );
	int const Polygonal( 9 );
	int const RectangularDoorWindow( 4 );
	int const RectangularOverhang( 5 );
	int const RectangularLeftFin( 6 );
	int const RectangularRightFin( 7 );
	int const TriangularWindow( 8 );
	int const TriangularDoor( 9 );

	// Parameters to indicate exterior boundary conditions for use with
	// the Surface derived type (see below):
	// Note:  Positive values correspond to an interzone adjacent surface

	int const ExternalEnvironment( 0 );
	int const Ground( -1 );
	int const OtherSideCoefNoCalcExt( -2 );
	int const OtherSideCoefCalcExt( -3 );
	int const OtherSideCondModeledExt( -4 );
	int const GroundFCfactorMethod( -5 );

	Array1D_string const cExtBoundCondition( {-5,0}, { "FCGround", "OSCM", "OSC", "OSC", "Ground", "ExternalEnvironment" } );

	// Parameters to indicate the first "corner" of a surface
	// Currently, these are used only during input of surfaces
	// They are here in order to facilitate later use in shading setup/calculations.
	int const UpperLeftCorner( 1 );
	int const LowerLeftCorner( 2 );
	int const LowerRightCorner( 3 );
	int const UpperRightCorner( 4 );

	// Parameters to indicate user specified convection coefficients (for surface)
	int const ConvCoefValue( 1 ); // User specified "value" as the override type
	int const ConvCoefSchedule( 2 ); // User specified "schedule" as the override type
	int const ConvCoefUserCurve( 3 ); // User specified "UserCurve" as the override type
	int const ConvCoefSpecifiedModel( 4 ); // one of the direct named model equation keys

	// Parameters to indicate reference air temperatures for inside surface temperature calculations
	int const ZoneMeanAirTemp( 1 ); // mean air temperature of the zone => MAT
	int const AdjacentAirTemp( 2 ); // air temperature adjacent ot surface => TempEffBulkAir
	int const ZoneSupplyAirTemp( 3 ); // supply air temperature of the zone

	int const AltAngStepsForSolReflCalc( 10 ); // Number of steps in altitude angle for solar reflection calc
	int const AzimAngStepsForSolReflCalc( 9 ); // Number of steps in azimuth angle of solar reflection calc

	// Parameters to indicate surface classes
	// Surface Class (FLOOR, WALL, ROOF (incl's CEILING), WINDOW, DOOR, GLASSDOOR,
	// SHADING (includes OVERHANG, WING), DETACHED, INTMASS),
	// TDD:DOME, TDD:DIFFUSER (for tubular daylighting device)
	// (Note: GLASSDOOR and TDD:DIFFUSER get overwritten as WINDOW
	// in SurfaceGeometry.cc, SurfaceWindow%OriginalClass holds the true value)
	// why aren't these sequential (LKL - 13 Aug 2007)
	int const SurfaceClass_Wall( 1 );
	int const SurfaceClass_Floor( 2 );
	int const SurfaceClass_Roof( 3 );
	int const SurfaceClass_IntMass( 5 );
	int const SurfaceClass_Detached_B( 6 );
	int const SurfaceClass_Detached_F( 7 );
	int const SurfaceClass_Window( 11 );
	int const SurfaceClass_Door( 13 );
	int const SurfaceClass_GlassDoor( 12 );
	int const SurfaceClass_Shading( 14 );
	int const SurfaceClass_Overhang( 15 );
	int const SurfaceClass_Fin( 16 );
	int const SurfaceClass_TDD_Dome( 17 );
	int const SurfaceClass_TDD_Diffuser( 18 );

	//Parameters to indicate heat transfer model to use for surface
	int const HeatTransferModel_NotSet( -1 );
	int const HeatTransferModel_None( 0 ); // shading surfaces for example
	int const HeatTransferModel_CTF( 1 );
	int const HeatTransferModel_EMPD( 2 );
	int const HeatTransferModel_CondFD( 5 );
	int const HeatTransferModel_HAMT( 6 );
	int const HeatTransferModel_Window5( 7 ); // original detailed layer-by-layer based on window 4 and window 5
	int const HeatTransferModel_ComplexFenestration( 8 ); // BSDF
	int const HeatTransferModel_TDD( 9 ); // tubular daylighting device

	// Parameters for classification of outside face of surfaces
	int const OutConvClass_WindwardVertWall( 101 );
	int const OutConvClass_LeewardVertWall( 102 );
	int const OutConvClass_RoofStable( 103 );
	int const OutConvClass_RoofUnstable( 104 );

	// Parameters for adpative convection algorithm's classification of inside face of surfaces
	int const InConvClass_A1_VertWalls( 1 ); // flow regime A1, vertical walls
	int const InConvClass_A1_StableHoriz( 2 ); // flow regime A1
	int const InConvClass_A1_UnstableHoriz( 3 ); // flow regime A1
	int const InConvClass_A1_HeatedFloor( 4 ); // flow regime A1
	int const InConvClass_A1_ChilledCeil( 5 ); // flow regime A1
	int const InConvClass_A1_StableTilted( 6 ); // flow regime A1
	int const InConvClass_A1_UnstableTilted( 7 ); // flow regime A1
	int const InConvClass_A1_Windows( 8 ); // flow regime A1
	int const InConvClass_A2_VertWallsNonHeated( 9 ); // flow regime A2
	int const InConvClass_A2_HeatedVerticalWall( 10 ); // flow regime A2
	int const InConvClass_A2_StableHoriz( 11 ); // flow regime A2
	int const InConvClass_A2_UnstableHoriz( 12 ); // flow regime A2
	int const InConvClass_A2_StableTilted( 13 ); // flow regime A2
	int const InConvClass_A2_UnstableTilted( 14 ); // flow regime A2
	int const InConvClass_A2_Windows( 15 ); // flow regime A2
	int const InConvClass_A3_VertWalls( 16 ); // flow regime A3
	int const InConvClass_A3_StableHoriz( 17 ); // flow regime A3
	int const InConvClass_A3_UnstableHoriz( 18 ); // flow regime A3
	int const InConvClass_A3_StableTilted( 19 ); // flow regime A3
	int const InConvClass_A3_UnstableTilted( 20 ); // flow regime A3
	int const InConvClass_A3_Windows( 21 ); // flow regime A3
	int const InConvClass_B_VertWalls( 22 ); // flow regime B
	int const InConvClass_B_VertWallsNearHeat( 23 ); // flow regime B
	int const InConvClass_B_StableHoriz( 24 ); // flow regime B
	int const InConvClass_B_UnstableHoriz( 25 ); // flow regime B
	int const InConvClass_B_StableTilted( 26 ); // flow regime B
	int const InConvClass_B_UnstableTilted( 27 ); // flow regime B
	int const InConvClass_B_Windows( 28 ); // flow regime B
	int const InConvClass_C_Walls( 29 ); // flow regime C
	int const InConvClass_C_Ceiling( 30 ); // flow regime C
	int const InConvClass_C_Floor( 31 ); // flow regime C
	int const InConvClass_C_Windows( 32 ); // flow regime C
	int const InConvClass_D_Walls( 33 ); // flow regime D
	int const InConvClass_D_StableHoriz( 34 ); // flow regime D
	int const InConvClass_D_UnstableHoriz( 35 ); // flow regime D
	int const InConvClass_D_StableTilted( 36 ); // flow regime D
	int const InConvClass_D_UnstableTilted( 37 ); // flow regime D
	int const InConvClass_D_Windows( 38 ); // flow regime D
	int const InConvClass_E_AssistFlowWalls( 39 ); // flow regime E
	int const InConvClass_E_OpposFlowWalls( 40 ); // flow regime E
	int const InConvClass_E_StableFloor( 41 ); // flow regime E
	int const InConvClass_E_UnstableFloor( 42 ); // flow regime E
	int const InConvClass_E_StableCeiling( 43 ); // flow regime E
	int const InConvClass_E_UnstableCieling( 44 ); // flow regime E
	int const InConvClass_E_Windows( 45 ); // flow regime E

	// Parameters for fenestration relative location in zone
	int const InConvWinLoc_NotSet( 0 );
	int const InConvWinLoc_LowerPartOfExteriorWall( 1 ); // this is a window in the lower part of wall
	int const InConvWinLoc_UpperPartOfExteriorWall( 2 ); // this is a window in the upper part of wall
	int const InConvWinLoc_WindowAboveThis( 3 ); // this is a wall with window above it
	int const InConvWinLoc_WindowBelowThis( 4 ); // this is a wall with window below it
	int const InConvWinLoc_LargePartOfExteriorWall( 5 ); // this is a big window taking up most of wall

	// Parameters for window shade status
	int const NoShade( -1 );
	int const ShadeOff( 0 );
	int const IntShadeOn( 1 ); // Interior shade on
	int const SwitchableGlazing( 2 );
	int const ExtShadeOn( 3 ); // Exterior shade on
	int const ExtScreenOn( 4 ); // Exterior screen on
	int const IntBlindOn( 6 ); // Interior blind on
	int const ExtBlindOn( 7 ); // Exterior blind on
	int const BGShadeOn( 8 ); // Between-glass shade on
	int const BGBlindOn( 9 ); // Between-glass blind on
	int const IntShadeConditionallyOff( 10 );
	int const GlassConditionallyLightened( 20 );
	int const ExtShadeConditionallyOff( 30 );
	int const IntBlindConditionallyOff( 60 );
	int const ExtBlindConditionallyOff( 70 );

	// WindowShadingControl Shading Types
	int const WSC_ST_NoShade( 0 );
	int const WSC_ST_InteriorShade( 1 );
	int const WSC_ST_SwitchableGlazing( 2 );
	int const WSC_ST_ExteriorShade( 3 );
	int const WSC_ST_InteriorBlind( 4 );
	int const WSC_ST_ExteriorBlind( 5 );
	int const WSC_ST_BetweenGlassShade( 6 );
	int const WSC_ST_BetweenGlassBlind( 7 );
	int const WSC_ST_ExteriorScreen( 8 );

	// WindowShadingControl Control Types
	int const WSCT_AlwaysOn( 1 ); // AlwaysOn
	int const WSCT_AlwaysOff( 2 ); // AlwaysOff
	int const WSCT_OnIfScheduled( 3 ); // OnIfScheduleAllows
	int const WSCT_HiSolar( 4 ); // OnIfHighSolarOnWindow
	int const WSCT_HiHorzSolar( 5 ); // OnIfHighHorizontalSolar
	int const WSCT_HiOutAirTemp( 6 ); // OnIfHighOutsideAirTemp
	int const WSCT_HiZoneAirTemp( 7 ); // OnIfHighZoneAirTemp
	int const WSCT_HiZoneCooling( 8 ); // OnIfHighZoneCooling
	int const WSCT_HiGlare( 9 ); // OnIfHighGlare
	int const WSCT_MeetDaylIlumSetp( 10 ); // MeetDaylightIlluminanceSetpoint
	int const WSCT_OnNightLoOutTemp_OffDay( 11 ); // OnNightIfLowOutsideTemp/OffDay
	int const WSCT_OnNightLoInTemp_OffDay( 12 ); // OnNightIfLowInsideTemp/OffDay
	int const WSCT_OnNightIfHeating_OffDay( 13 ); // OnNightIfHeating/OffDay
	int const WSCT_OnNightLoOutTemp_OnDayCooling( 14 ); // OnNightIfLowOutsideTemp/OnDayIfCooling
	int const WSCT_OnNightIfHeating_OnDayCooling( 15 ); // OnNightIfHeating/OnDayIfCooling
	int const WSCT_OffNight_OnDay_HiSolarWindow( 16 ); // OffNight/OnDayIfCoolingAndHighSolarOnWindow
	int const WSCT_OnNight_OnDay_HiSolarWindow( 17 ); // OnNight/OnDayIfCoolingAndHighSolarOnWindow
	int const WSCT_OnHiOutTemp_HiSolarWindow( 18 ); // OnIfHighOutsideAirTempAndHighSolarOnWindow
	int const WSCT_OnHiOutTemp_HiHorzSolar( 19 ); // OnIfHighOutsideAirTempAndHighHorizontalSolar
	int const WSCT_OnHiZoneTemp_HiSolarWindow( 20 ); // OnIfHighZoneAirTempAndHighSolarOnWindow
	int const WSCT_OnHiZoneTemp_HiHorzSolar( 21 ); // OnIfHighZoneAirTempAndHighHorizontalSolar

	// WindowShadingControl Slat Angle Control for Blinds
	int const WSC_SAC_FixedSlatAngle( 1 );
	int const WSC_SAC_ScheduledSlatAngle( 2 );
	int const WSC_SAC_BlockBeamSolar( 3 );

	// Parameter for window screens beam reflectance accounting
	int const DoNotModel( 0 );
	int const ModelAsDirectBeam( 1 );
	int const ModelAsDiffuse( 2 );

	// Parameters for window divider type
	int const DividedLite( 1 );
	int const Suspended( 2 );

	// Parameters for air flow window source
	int const AirFlowWindow_Source_IndoorAir( 1 );
	int const AirFlowWindow_Source_OutdoorAir( 2 );

	// Parameters for air flow window destination
	int const AirFlowWindow_Destination_IndoorAir( 1 );
	int const AirFlowWindow_Destination_OutdoorAir( 2 );
	int const AirFlowWindow_Destination_ReturnAir( 3 );

	// Parameters for air flow window control
	int const AirFlowWindow_ControlType_MaxFlow( 1 );
	int const AirFlowWindow_ControlType_AlwaysOff( 2 );
	int const AirFlowWindow_ControlType_Schedule( 3 );

	// Parameters for window model selection
	int const Window5DetailedModel( 100 ); // indicates original winkelmann window 5 implementation
	int const WindowBSDFModel( 101 ); // indicates complex fenestration window 6 implementation
	int const WindowEQLModel( 102 ); // indicates equivalent layer winodw model implementation

	// DERIVED TYPE DEFINITIONS:

	// Definitions used for scheduled surface gains

	// INTERFACE BLOCK SPECIFICATIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:

	int TotSurfaces( 0 ); // Total number of surfaces (walls, floors, roofs, windows, shading surfaces, etc.--everything)
	int TotWindows( 0 ); // Total number of windows
	int TotComplexWin( 0 ); // Total number of windows with complex optical properties
	int TotStormWin( 0 ); // Total number of storm window blocks
	int TotWinShadingControl( 0 ); // Total number of window shading control blocks
	int TotIntConvCoeff( 0 ); // Total number of interior convection coefficient (overrides)
	int TotExtConvCoeff( 0 ); // Total number of exterior convection coefficient (overrides)
	int TotOSC( 0 ); // Total number of Other Side Coefficient Blocks
	int TotOSCM( 0 ); // Total number of Other Side Conditions Model Blocks.
	int TotExtVentCav( 0 );
	int TotSurfIncSolSSG( 0 ); // Total number of scheduled surface gains for incident solar radiation on surface
	int TotFenLayAbsSSG( 0 ); // Total number of scheduled surface gains for absorbed solar radiation in window layers
	int Corner( 0 ); // Which corner is specified as the first vertice
	int MaxVerticesPerSurface( 4 ); // Maximum number of vertices allowed for a single surface (default -- can go higher)

	int BuildingShadingCount( 0 ); // Total number of Building External Shades
	int FixedShadingCount( 0 ); // Total number of Fixed External Shades
	int AttachedShadingCount( 0 ); // Total number of Shades attached to Zones

	bool AspectTransform( false ); // Set to true when GeometryTransform object is used
	bool CalcSolRefl( false ); // Set to true when Solar Reflection Calculations object is used
	bool CCW( false ); // True if vertices will be entered in CounterClockWise Order
	bool WorldCoordSystem( false ); // True if vertices will be "World Coordinates"
	// False means relative coordinates
	bool DaylRefWorldCoordSystem( false ); // True if Daylight Reference Point vertices will be "World Coordinates"
	// False means relative coordinates
	int MaxRecPts( 0 ); // Max number of receiving points on a surface for solar reflection calc
	int MaxReflRays( 0 ); // Max number of rays from a receiving surface for solar reflection calc
	Real64 GroundLevelZ( 0.0 ); // Z value of ground level for solar refl calc (m)
	bool AirflowWindows( false ); // TRUE if one or more airflow windows

	bool ShadingTransmittanceVaries( false ); // overall, shading transmittance varies for the building

	Array1D_int InsideGlassCondensationFlag; // 1 if innermost glass inside surface temp < zone air dew point;
	// 0 otherwise
	Array1D_int InsideFrameCondensationFlag; // 1 if frame inside surface temp < zone air dew point;
	// 0 otherwise
	Array1D_int InsideDividerCondensationFlag; // 1 if divider inside surface temp < zone air dew point;
	// 0 otherwise
	Array1D_int AdjacentZoneToSurface; // Array of adjacent zones to each surface

	Array1D< Real64 > X0; // X-component of translation vector
	Array1D< Real64 > Y0; // Y-component of translation vector
	Array1D< Real64 > Z0; // Z-component of translation vector
	Array1D< Real64 > DSZone; // Factor for sky diffuse solar radiation into a zone
	Array1D< Real64 > DGZone; // Factor for ground diffuse solar radiation into a zone
	Array1D< Real64 > DBZone; // Factor for diffuse radiation in a zone from
	// beam reflecting from inside surfaces
	Array1D< Real64 > DBZoneSSG; // Factor for diffuse radiation in a zone from beam reflecting from inside surfaces. Used only for scheduled surface gains
	Array1D< Real64 > CBZone; // Factor for beam solar absorbed by interior shades
	Array1D< Real64 > AISurf; // Time step value of factor for beam
	// absorbed on inside of opaque surface
	Array1D< Real64 > AOSurf; // Time step value of factor for beam
	// absorbed on outside of opaque surface
	Array1D< Real64 > BmToBmReflFacObs; // Factor for incident solar from specular beam refl
	// from obstructions (W/m2)/(W/m2)
	Array1D< Real64 > BmToDiffReflFacObs; // Factor for incident solar from diffuse beam refl
	// from obstructions (W/m2)/(W/m2)
	Array1D< Real64 > BmToDiffReflFacGnd; // Factor for incident solar from diffuse beam refl from ground

	Array2D< Real64 > AWinSurf; // Time step value of factor for beam
	// absorbed in window glass layers

	Array2D< Real64 > AWinCFOverlap; // Time step value of factor for beam
	// absorbed in window glass layers which comes from other windows
	// It happens sometimes that beam enters one window and hits back of
	// second window. It is used in complex fenestration only

	Array1D< Real64 > AirSkyRadSplit; // Fractional split between the air and
	// the sky for radiation from the surface
	// Fraction of sky IR coming from sky itself; 1-AirSkyRadSplit comes from the atmosphere.

	Array1D< Real64 > WinTransSolar; // Exterior beam plus diffuse solar transmitted through window, or
	// window plus shade/blind, into zone (W)
	Array1D< Real64 > WinBmSolar; // Exterior beam solar transmitted through window, or
	// window plus blind, into zone (W)

	Array1D< Real64 > WinBmBmSolar; // Exterior beam-to-beam solar transmitted through window, or
	// window plus blind, into zone (W)
	Array1D< Real64 > WinBmDifSolar; // Exterior beam-to-diffuse solar transmitted through window, or
	// window plus blind, into zone (W)

	Array1D< Real64 > WinDifSolar; // Exterior diffuse solar transmitted through window, or
	// window plus shade/blind, into zone (W)
	Array1D< Real64 > WinDirSolTransAtIncAngle; // Window's beam-beam solar transmittance at current timestep's
	// angle of incidence
	Array1D< Real64 > WinHeatGain; // Total heat gain from window = WinTransSolar + (IR and convection from
	// glazing, or, if interior shade, IR and convection from
	// zone-side of shade plus gap air convection to zone) + (IR and
	// convection from frame) + (IR and convection from divider if no
	// interior shade) (W)
	Array1D< Real64 > WinHeatGainRep; // Equals WinHeatGain when WinHeatGain >= 0.0
	Array1D< Real64 > WinHeatLossRep; // Equals -WinHeatGain when WinHeatGain < 0.0

	Array1D< Real64 > WinGainConvGlazToZoneRep; // component of WinHeatGain convect to zone from glazing (W)
	Array1D< Real64 > WinGainIRGlazToZoneRep; // component of WinHeatGain net IR to zone from glazing (W)
	Array1D< Real64 > WinLossSWZoneToOutWinRep; // component of WinHeatGain shortwave transmit back out (W)
	Array1D< Real64 > WinGainFrameDividerToZoneRep; // component of WinHeatGain to zone from frame/divider (W)
	Array1D< Real64 > WinGainConvGlazShadGapToZoneRep; // component of WinHeatGain convection to zone from
	// the gap between the inner most glazing and the shade   (W)
	Array1D< Real64 > WinGainConvShadeToZoneRep; // component of WinHeatGain convect to zone from front shade (W)
	Array1D< Real64 > WinGainIRShadeToZoneRep; // component of WinHeatGain net IR to zone from front shade (W)
	Array1D< Real64 > OtherConvGainInsideFaceToZoneRep; // net imbalance of convection heat gain from equivalent Layer window inside face to zone air

	Array1D< Real64 > WinGapConvHtFlowRep; // Convective heat flow from gap in airflow window (W)
	//REAL(r64), ALLOCATABLE, DIMENSION(:) :: OpaqSurfInsFaceCondGainRep !Equals Opaq Surf Ins Face Cond
	//                                                                   ! when Opaq Surf Ins Face Cond >= 0
	//REAL(r64), ALLOCATABLE, DIMENSION(:) :: OpaqSurfInsFaceCondLossRep !Equals -Opaq Surf Ins Face Cond
	//                                                                   ! when Opaq Surf Ins Face Cond  < 0
	Array1D< Real64 > WinShadingAbsorbedSolar; // Exterior beam plus diffuse solar absorbed by
	//  window shading device (W)
	Array1D< Real64 > WinSysSolTransmittance; // Effective solar transmittance of window + shading device,
	// if present
	Array1D< Real64 > WinSysSolReflectance; // Effective solar reflectance of window + shading device,
	// if present
	Array1D< Real64 > WinSysSolAbsorptance; // Effective solar absorptance of window + shading device,
	// if present
	Array2D< Real64 > SUNCOSHR( 24, 3, 0.0 ); // Hourly values of SUNCOS (solar direction cosines) //Autodesk:Init Zero-initialization added to avoid use uninitialized
	Array2D< Real64 > ReflFacBmToDiffSolObs;
	Array2D< Real64 > ReflFacBmToDiffSolGnd;
	Array2D< Real64 > ReflFacBmToBmSolObs;
	Array1D< Real64 > ReflFacSkySolObs;
	Array1D< Real64 > ReflFacSkySolGnd;
	Array2D< Real64 > CosIncAveBmToBmSolObs;
	Array1D< Real64 > DBZoneIntWin; // Value of factor for beam solar entering a zone through interior windows
	// (considered to contribute to diffuse in zone)
	Array1D< Real64 > SurfSunlitArea; // Sunlit area by surface number
	Array1D< Real64 > SurfSunlitFrac; // Sunlit fraction by surface number
	//energy
	Array1D< Real64 > WinTransSolarEnergy; // Energy of WinTransSolar [J]
	Array1D< Real64 > WinBmSolarEnergy; // Energy of WinBmSolar [J]

	Array1D< Real64 > WinBmBmSolarEnergy; // Beam-to-beam energy of WinBmSolar [J]
	Array1D< Real64 > WinBmDifSolarEnergy; // Beam-to-diffuse energy of WinBmSolar [J]

	Array1D< Real64 > WinDifSolarEnergy; // Energy of WinDifSolar [J]
	Array1D< Real64 > WinHeatGainRepEnergy; // Energy of WinHeatGainRep [J]
	Array1D< Real64 > WinHeatLossRepEnergy; // Energy of WinHeatLossRep [J]
	Array1D< Real64 > WinShadingAbsorbedSolarEnergy; // Energy of WinShadingAbsorbedSolar [J]
	Array1D< Real64 > WinGapConvHtFlowRepEnergy; // Energy of WinGapConvHtFlowRep [J]

	// SUBROUTINE SPECIFICATIONS FOR MODULE DataSurfaces:

	// Object Data
	Array1D< SurfaceData > Surface;
	Array1D< SurfaceWindowCalc > SurfaceWindow;
	Array1D< FrameDividerProperties > FrameDivider;
	Array1D< StormWindowData > StormWindow;
	Array1D< WindowShadingControlData > WindowShadingControl;
	Array1D< OSCData > OSC;
	Array1D< OSCMData > OSCM;
	Array1D< ConvectionCoefficient > UserIntConvectionCoeffs;
	Array1D< ConvectionCoefficient > UserExtConvectionCoeffs;
	Array1D< ShadingVertexData > ShadeV;
	Array1D< ExtVentedCavityStruct > ExtVentedCavity;
	Array1D< SurfaceSolarIncident > SurfIncSolSSG;
	Array1D< FenestrationSolarAbsorbed > FenLayAbsSSG;

	// Functions

	// Clears the global data in DataSurfaces.
	// Needed for unit tests, should not be normally called.
	void
	clear_state()
	{
		TotSurfaces = 0;
		TotWindows = 0;
		TotComplexWin = 0;
		TotStormWin = 0;
		TotWinShadingControl = 0;
		TotIntConvCoeff = 0;
		TotExtConvCoeff = 0;
		TotOSC = 0;
		TotOSCM = 0;
		TotExtVentCav = 0;
		TotSurfIncSolSSG = 0;
		TotFenLayAbsSSG = 0;
		Corner = 0;
		MaxVerticesPerSurface = 4;
		BuildingShadingCount = 0;
		FixedShadingCount = 0;
		AttachedShadingCount = 0;
		AspectTransform = false;
		CalcSolRefl = false;
		CCW = false;
		WorldCoordSystem = false;
		DaylRefWorldCoordSystem = false;
		MaxRecPts = 0;
		MaxReflRays = 0;
		GroundLevelZ = 0.0;
		AirflowWindows = false;
		ShadingTransmittanceVaries = false;
		InsideGlassCondensationFlag.deallocate();
		InsideFrameCondensationFlag.deallocate();
		InsideDividerCondensationFlag.deallocate();
		AdjacentZoneToSurface.deallocate();
		X0.deallocate();
		Y0.deallocate();
		Z0.deallocate();
		DSZone.deallocate();
		DGZone.deallocate();
		DBZone.deallocate();
		DBZoneSSG.deallocate();
		CBZone.deallocate();
		AISurf.deallocate();
		AOSurf.deallocate();
		BmToBmReflFacObs.deallocate();
		BmToDiffReflFacObs.deallocate();
		BmToDiffReflFacGnd.deallocate();
		AWinSurf.deallocate();
		AWinCFOverlap.deallocate();
		AirSkyRadSplit.deallocate();
		WinTransSolar.deallocate();
		WinBmSolar.deallocate();
		WinBmBmSolar.deallocate();
		WinBmDifSolar.deallocate();
		WinDifSolar.deallocate();
		WinDirSolTransAtIncAngle.deallocate();
		WinHeatGain.deallocate();
		WinHeatGainRep.deallocate();
		WinHeatLossRep.deallocate();
		WinGainConvGlazToZoneRep.deallocate();
		WinGainIRGlazToZoneRep.deallocate();
		WinLossSWZoneToOutWinRep.deallocate();
		WinGainFrameDividerToZoneRep.deallocate();
		WinGainConvGlazShadGapToZoneRep.deallocate();
		WinGainConvShadeToZoneRep.deallocate();
		WinGainIRShadeToZoneRep.deallocate();
		OtherConvGainInsideFaceToZoneRep.deallocate();
		WinGapConvHtFlowRep.deallocate();
		WinShadingAbsorbedSolar.deallocate();
		WinSysSolTransmittance.deallocate();
		WinSysSolReflectance.deallocate();
		WinSysSolAbsorptance.deallocate();
		SUNCOSHR.dimension( 24, 3, 0.0 );
		ReflFacBmToDiffSolObs.deallocate();
		ReflFacBmToDiffSolGnd.deallocate();
		ReflFacBmToBmSolObs.deallocate();
		ReflFacSkySolObs.deallocate();
		ReflFacSkySolGnd.deallocate();
		CosIncAveBmToBmSolObs.deallocate();
		DBZoneIntWin.deallocate();
		SurfSunlitArea.deallocate();
		SurfSunlitFrac.deallocate();
		WinTransSolarEnergy.deallocate();
		WinBmSolarEnergy.deallocate();
		WinBmBmSolarEnergy.deallocate();
		WinBmDifSolarEnergy.deallocate();
		WinDifSolarEnergy.deallocate();
		WinHeatGainRepEnergy.deallocate();
		WinHeatLossRepEnergy.deallocate();
		WinShadingAbsorbedSolarEnergy.deallocate();
		WinGapConvHtFlowRepEnergy.deallocate();
		Surface.deallocate();
		SurfaceWindow.deallocate();
		FrameDivider.deallocate();
		StormWindow.deallocate();
		WindowShadingControl.deallocate();
		OSC.deallocate();
		OSCM.deallocate();
		UserIntConvectionCoeffs.deallocate();
		UserExtConvectionCoeffs.deallocate();
		ShadeV.deallocate();
		ExtVentedCavity.deallocate();
		SurfIncSolSSG.deallocate();
		FenLayAbsSSG.deallocate();
	}

	std::string
	cSurfaceClass( int const ClassNo )
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   May 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function returns a string based on class number.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		std::string ClassName;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		{ auto const SELECT_CASE_var( ClassNo );
		if ( SELECT_CASE_var == SurfaceClass_Wall ) {
			ClassName = "Wall";

		} else if ( SELECT_CASE_var == SurfaceClass_Floor ) {
			ClassName = "Floor";

		} else if ( SELECT_CASE_var == SurfaceClass_Roof ) {
			ClassName = "Roof";

		} else if ( SELECT_CASE_var == SurfaceClass_Window ) {
			ClassName = "Window";

		} else if ( SELECT_CASE_var == SurfaceClass_GlassDoor ) {
			ClassName = "Glass Door";

		} else if ( SELECT_CASE_var == SurfaceClass_Door ) {
			ClassName = "Door";

		} else if ( SELECT_CASE_var == SurfaceClass_TDD_Dome ) {
			ClassName = "TubularDaylightDome";

		} else if ( SELECT_CASE_var == SurfaceClass_TDD_Diffuser ) {
			ClassName = "TubularDaylightDiffuser";

		} else if ( SELECT_CASE_var == SurfaceClass_IntMass ) {
			ClassName = "Internal Mass";

		} else if ( SELECT_CASE_var == SurfaceClass_Shading ) {
			ClassName = "Shading";

		} else if ( SELECT_CASE_var == SurfaceClass_Detached_B ) {
			ClassName = "Detached Shading:Building";

		} else if ( SELECT_CASE_var == SurfaceClass_Detached_F ) {
			ClassName = "Detached Shading:Fixed";

		} else {
			ClassName = "Invalid/Unknown";

		}}

		return ClassName;

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

} // DataSurfaces

} // EnergyPlus
