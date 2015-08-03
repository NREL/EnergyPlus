#ifndef DataHeatBalance_hh_INCLUDED
#define DataHeatBalance_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array2D.hh>
#include <ObjexxFCL/Array3D.hh>
#include <ObjexxFCL/Array4D.hh>
#include <ObjexxFCL/Optional.hh>
#include <ObjexxFCL/Reference.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataBSDFWindow.hh>
#include <DataComplexFenestration.hh>
#include <DataGlobals.hh>
#include <DataSurfaces.hh>
#include <DataVectorTypes.hh>
#include <DataWindowEquivalentLayer.hh>

namespace EnergyPlus {

namespace DataHeatBalance {

	// Using/Aliasing
	using namespace DataComplexFenestration;
	using DataBSDFWindow::BSDFWindowInputStruct;
	using DataComplexFenestration::GapDeflectionState;
	using DataComplexFenestration::GapSupportPillar;
	using DataComplexFenestration::WindowComplexShade;
	using DataComplexFenestration::WindowThermalModelParams;
	using DataGlobals::AutoCalculate;
	using DataSurfaces::MaxSlatAngs;
	using DataVectorTypes::Vector;
	using DataWindowEquivalentLayer::CFSMAXNL;

	// Data
	// module should be available to other modules and routines.  Thus,
	// all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:

	// Parameters for the definition and limitation of arrays:
	extern int const MaxLayersInConstruct; // Maximum number of layers allowed in a single construction
	extern int const MaxCTFTerms; // Maximum number of CTF terms allowed to still allow stability
	extern int MaxSolidWinLayers; // Maximum number of solid layers in a window construction
	extern int const MaxSpectralDataElements; // Maximum number in Spectral Data arrays.

	// Parameters to indicate material group type for use with the Material
	// derived type (see below):

	extern int const RegularMaterial;
	extern int const Air;
	extern int const Shade;
	extern int const WindowGlass;
	extern int const WindowGas;
	extern int const WindowBlind;
	extern int const WindowGasMixture;
	extern int const Screen;
	extern int const EcoRoof;
	extern int const IRTMaterial;
	extern int const WindowSimpleGlazing;
	extern int const ComplexWindowShade;
	extern int const ComplexWindowGap;

	extern int const GlassEquivalentLayer;
	extern int const ShadeEquivalentLayer;
	extern int const DrapeEquivalentLayer;
	extern int const BlindEquivalentLayer;
	extern int const ScreenEquivalentLayer;
	extern int const GapEquivalentLayer;

	extern Array1D_string const cMaterialGroupType;

	// Parameters to indicate surface roughness for use with the Material
	// derived type (see below):

	extern int const VeryRough;
	extern int const Rough;
	extern int const MediumRough;
	extern int const MediumSmooth;
	extern int const Smooth;
	extern int const VerySmooth;

	// Parameters to indicate blind orientation for use with the Material
	// derived type (see below):

	extern int const Horizontal;
	extern int const Vertical;
	extern int const FixedSlats;
	extern int const VariableSlats;
	// Parameters for Interior and Exterior Solar Distribution

	extern int const MinimalShadowing; // all incoming solar hits floor, no exterior shadowing except reveals
	extern int const FullExterior; // all incoming solar hits floor, full exterior shadowing
	extern int const FullInteriorExterior; // full interior solar distribution, full exterior solar shadowing
	extern int const FullExteriorWithRefl; // all incoming solar hits floor, full exterior shadowing and reflections
	extern int const FullInteriorExteriorWithRefl; // full interior solar distribution,
	// full exterior shadowing and reflections
	// Parameters to indicate the zone type for use with the Zone derived
	// type (see below--Zone%OfType):

	extern int const StandardZone;
	//INTEGER, PARAMETER :: PlenumZone = 2
	//INTEGER, PARAMETER :: SolarWallZone = 11  ! from old ZTYP, OSENV
	//INTEGER, PARAMETER :: RoofPondZone = 12   ! from old ZTYP, OSENV

	// Parameters to indicate the convection correlation being used for use with
	// InsideConvectionAlgo and OutsideConvectionAlgo

	extern int const ASHRAESimple;
	extern int const ASHRAETARP;
	extern int const CeilingDiffuser; // Only valid for inside use
	extern int const TrombeWall; // Only valid for inside use
	extern int const TarpHcOutside; // Only valid for outside use
	extern int const MoWiTTHcOutside; // Only valid for outside use
	extern int const DOE2HcOutside; // Only valid for outside use
	extern int const BLASTHcOutside; // Only valid for outside use
	extern int const AdaptiveConvectionAlgorithm;

	// Parameters for WarmupDays
	extern int const DefaultMaxNumberOfWarmupDays; // Default maximum number of warmup days allowed
	extern int const DefaultMinNumberOfWarmupDays; // Default minimum number of warmup days allowed

	// Parameters for Sky Radiance Distribution
	extern int const Isotropic;
	extern int const Anisotropic;

	// Parameters for HeatTransferAlgosUsed
	extern int const UseCTF;
	extern int const UseEMPD;
	extern int const UseCondFD;
	extern int const UseHAMT;

	// Parameters for ZoneAirSolutionAlgo
	extern int const Use3rdOrder;
	extern int const UseAnalyticalSolution;
	extern int const UseEulerMethod;

	// Parameter for MRT calculation type
	extern int const ZoneAveraged;
	extern int const SurfaceWeighted;
	extern int const AngleFactor;

	// Parameters for Ventilation
	extern int const NaturalVentilation;
	extern int const IntakeVentilation;
	extern int const ExhaustVentilation;
	extern int const BalancedVentilation;

	// Parameters for hybrid ventilation using Ventilation and Mixing objects
	extern int const HybridControlTypeIndiv;
	extern int const HybridControlTypeClose;
	extern int const HybridControlTypeGlobal;

	// System type, detailed refrigeration or refrigerated case rack
	extern int const RefrigSystemTypeDetailed;
	extern int const RefrigSystemTypeRack;

	// Refrigeration condenser type
	extern int const RefrigCondenserTypeAir;
	extern int const RefrigCondenserTypeEvap;
	extern int const RefrigCondenserTypeWater;
	extern int const RefrigCondenserTypeCascade;

	// Parameters for type of infiltration model
	extern int const InfiltrationDesignFlowRate;
	extern int const InfiltrationShermanGrimsrud;
	extern int const InfiltrationAIM2;

	// Parameters for type of ventilation model
	extern int const VentilationDesignFlowRate;
	extern int const VentilationWindAndStack;

	// Parameters for type of zone air balance model
	extern int const AirBalanceNone;
	extern int const AirBalanceQuadrature;

	// Parameter for source zone air flow mass balance infiltration treatment
	extern int const AddInfiltrationFlow;
	extern int const AdjustInfiltrationFlow;

	extern int const NumZoneIntGainDeviceTypes;

	extern Array1D_string const ZoneIntGainDeviceTypes; // 01 | 02 | 03 | 04 | 05 | 06 | 07 | 08 | 09 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25 | 26 | 27 | 28 | 29 | 30 | 31 | 32 | 33 | 34 | 35 | 36 | 37 | 38 | 39 | 40 | 41 | 42 | 43 | 44 | 45 | 46 | 47 | 48 | 49 | 50 | 51

	extern Array1D_string const ccZoneIntGainDeviceTypes; // 01 | 02 | 03 | 04 | 05 | 06 | 07 | 08 | 09 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25 | 26 | 27 | 28 | 29 | 30 | 31 | 32 | 33 | 34 | 35 | 36 | 37 | 38 | 39 | 40 | 41 | 42 | 43 | 44 | 45 | 46 | 47 | 48 | 49 | 50 | 51
	extern int const IntGainTypeOf_People;
	extern int const IntGainTypeOf_Lights;
	extern int const IntGainTypeOf_ElectricEquipment;
	extern int const IntGainTypeOf_GasEquipment;
	extern int const IntGainTypeOf_HotWaterEquipment;
	extern int const IntGainTypeOf_SteamEquipment;
	extern int const IntGainTypeOf_OtherEquipment;
	extern int const IntGainTypeOf_ZoneBaseboardOutdoorTemperatureControlled;
	extern int const IntGainTypeOf_ZoneContaminantSourceAndSinkCarbonDioxide;
	extern int const IntGainTypeOf_WaterUseEquipment;
	extern int const IntGainTypeOf_DaylightingDeviceTubular;
	extern int const IntGainTypeOf_WaterHeaterMixed;
	extern int const IntGainTypeOf_WaterHeaterStratified;
	extern int const IntGainTypeOf_ThermalStorageChilledWaterMixed;
	extern int const IntGainTypeOf_ThermalStorageChilledWaterStratified;
	extern int const IntGainTypeOf_GeneratorFuelCell;
	extern int const IntGainTypeOf_GeneratorMicroCHP;
	extern int const IntGainTypeOf_ElectricLoadCenterTransformer;
	extern int const IntGainTypeOf_ElectricLoadCenterInverterSimple;
	extern int const IntGainTypeOf_ElectricLoadCenterInverterFunctionOfPower;
	extern int const IntGainTypeOf_ElectricLoadCenterInverterLookUpTable;
	extern int const IntGainTypeOf_ElectricLoadCenterStorageBattery;
	extern int const IntGainTypeOf_ElectricLoadCenterStorageSimple;
	extern int const IntGainTypeOf_PipeIndoor;
	extern int const IntGainTypeOf_RefrigerationCase;
	extern int const IntGainTypeOf_RefrigerationCompressorRack;
	extern int const IntGainTypeOf_RefrigerationSystemAirCooledCondenser;
	extern int const IntGainTypeOf_RefrigerationTransSysAirCooledGasCooler;
	extern int const IntGainTypeOf_RefrigerationSystemSuctionPipe;
	extern int const IntGainTypeOf_RefrigerationTransSysSuctionPipeMT;
	extern int const IntGainTypeOf_RefrigerationTransSysSuctionPipeLT;
	extern int const IntGainTypeOf_RefrigerationSecondaryReceiver;
	extern int const IntGainTypeOf_RefrigerationSecondaryPipe;
	extern int const IntGainTypeOf_RefrigerationWalkIn;
	extern int const IntGainTypeOf_Pump_VarSpeed;
	extern int const IntGainTypeOf_Pump_ConSpeed;
	extern int const IntGainTypeOf_Pump_Cond;
	extern int const IntGainTypeOf_PumpBank_VarSpeed;
	extern int const IntGainTypeOf_PumpBank_ConSpeed;
	extern int const IntGainTypeOf_ZoneContaminantSourceAndSinkGenericContam;
	extern int const IntGainTypeOf_PlantComponentUserDefined;
	extern int const IntGainTypeOf_CoilUserDefined;
	extern int const IntGainTypeOf_ZoneHVACForcedAirUserDefined;
	extern int const IntGainTypeOf_AirTerminalUserDefined;
	extern int const IntGainTypeOf_PackagedTESCoilTank;
	extern int const IntGainTypeOf_ElectricEquipmentITEAirCooled;
	extern int const IntGainTypeOf_SecCoolingDXCoilSingleSpeed;
	extern int const IntGainTypeOf_SecHeatingDXCoilSingleSpeed;
	extern int const IntGainTypeOf_SecCoolingDXCoilTwoSpeed;
	extern int const IntGainTypeOf_SecCoolingDXCoilMultiSpeed;
	extern int const IntGainTypeOf_SecHeatingDXCoilMultiSpeed;

	//Parameters for checking surface heat transfer models
	extern Real64 const HighDiffusivityThreshold; // used to check if Material properties are out of line.
	extern Real64 const ThinMaterialLayerThreshold; // 3 mm lower limit to expected material layers

	// DERIVED TYPE DEFINITIONS:

	// thermochromic windows

	// For predefined tabular reporting

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:

	// MODULE VARIABLE Type DECLARATIONS:

	// INTERFACE BLOCK SPECIFICATIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:

	// SiteData aka building data
	extern Real64 LowHConvLimit; // Lowest allowed convection coefficient for detailed model
	// before reverting to the simple model.  This avoids a
	// divide by zero elsewhere.  Not based on any physical
	// reasoning, just the number that was picked.  It corresponds
	// to a delta T for a vertical surface of 0.000444C.
	//REAL(r64), PARAMETER :: LowHConvLimit = 1.0 !W/m2-K  Lowest allowed natural convection coefficient
	//                           ! A lower limit is needed to avoid numerical problems
	//                           ! Natural convection correlations are a function of temperature difference,
	//                           !   there are many times when those temp differences pass through zero leading to non-physical results
	//                           ! Value of 1.0 chosen here is somewhat arbitrary, but based on the following reasons:
	//                           !  1) Low values of HconvIn indicate a layer of high thermal resistance, however
	//                           !       the R-value of a convection film layer should be relatively low (compared to building surfaces)
	//                           !  2) The value of 1.0 corresponds to the thermal resistance of 0.05 m of batt insulation
	//                           !  3) Limit on the order of 1.0 is suggested by the abrupt changes in an inverse relationship
	//                           !  4) A conduction-only analysis can model a limit by considering the thermal performance of
	//                           !       boundary layer to be pure conduction (with no movement to enhance heat transfer);
	//                           !       Taking the still gas thermal conductivity for air at 0.0267 W/m-K (at 300K), then
	//                           !       this limit of 1.0 corresponds to a completely still layer of air that is around 0.025 m thick
	//                           !  5) The previous limit of 0.1 (before ver. 3.1) caused loads initialization problems in test files
	extern Real64 HighHConvLimit; // upper limit for HConv, mostly used for user input limits in practics. !W/m2-K
	extern Real64 MaxAllowedDelTempCondFD; // Convergence criteria for inside surface temperatures for CondFD

	extern std::string BuildingName; // Name of building
	extern Real64 BuildingAzimuth; // North Axis of Building
	extern Real64 LoadsConvergTol; // Tolerance value for Loads Convergence
	extern Real64 TempConvergTol; // Tolerance value for Temperature Convergence
	extern int DefaultInsideConvectionAlgo; // 1 = simple (ASHRAE); 2 = detailed (ASHRAE); 3 = ceiling diffuser;
	// 4 = trombe wall
	extern int DefaultOutsideConvectionAlgo; // 1 = simple (ASHRAE); 2 = detailed; etc (BLAST, TARP, MOWITT, DOE-2)
	extern int SolarDistribution; // Solar Distribution Algorithm
	extern int InsideSurfIterations; // Counts inside surface iterations
	extern int OverallHeatTransferSolutionAlgo; // UseCTF Solution, UseEMPD moisture solution, UseCondFD solution
	extern int NumberOfHeatTransferAlgosUsed;
	extern Array1D_int HeatTransferAlgosUsed;
	extern int MaxNumberOfWarmupDays; // Maximum number of warmup days allowed
	extern int MinNumberOfWarmupDays; // Minimum number of warmup days allowed
	extern Real64 CondFDRelaxFactor; // Relaxation factor, for looping across all the surfaces.
	extern Real64 CondFDRelaxFactorInput; // Relaxation factor, for looping across all the surfaces, user input value
	//LOGICAL ::  CondFDVariableProperties = .FALSE. ! if true, then variable conductivity or enthalpy in Cond FD.

	extern int ZoneAirSolutionAlgo; // ThirdOrderBackwardDifference, AnalyticalSolution, and EulerMethod
	extern Real64 BuildingRotationAppendixG; // Building Rotation for Appendix G
	extern bool ZoneAirMassBalanceSimulation; // if true, then enforces zone mass flow conservation

	//END SiteData

	extern int NumOfZoneLists; // Total number of zone lists
	extern int NumOfZoneGroups; // Total number of zone groups
	extern int NumPeopleStatements; // Number of People objects in input - possibly global assignments
	extern int NumLightsStatements; // Number of Lights objects in input - possibly global assignments
	extern int NumZoneElectricStatements; // Number of ZoneElectric objects in input - possibly global assignments
	extern int NumZoneGasStatements; // Number of ZoneGas objects in input - possibly global assignments
	extern int NumInfiltrationStatements; // Number of Design Flow Infiltration objects in input - possibly global assignments
	extern int NumVentilationStatements; // Number of Design Flow Ventilation objects in input - possibly global assignments
	extern int NumHotWaterEqStatements; // number of Hot Water Equipment objects in input. - possibly global assignments
	extern int NumSteamEqStatements; // number of Steam Equipment objects in input. - possibly global assignments
	extern int NumOtherEqStatements; // number of Other Equipment objects in input. - possibly global assignments
	extern int NumZoneITEqStatements; // Number of ElectricEquipment:ITE:AirCooled objects in input (ZoneList not supported for this object)
	extern int TotPeople; // Total People Statements in input and extrapolated from global assignments
	extern int TotLights; // Total Lights Statements in input and extrapolated from global assignments
	extern int TotElecEquip; // Total Electric Equipment Statements in input and extrapolated from global assignments
	extern int TotGasEquip; // Total Gas Equipment Statements in input
	extern int TotOthEquip; // Total Other Equipment Statements in input
	extern int TotHWEquip; // Total Hot Water Equipment Statements in input
	extern int TotStmEquip; // Total Steam Equipment Statements in input
	extern int TotInfiltration; // Total Infiltration Statements in input and extrapolated from global assignments
	extern int TotDesignFlowInfiltration; // number of Design Flow rate ZoneInfiltration in input
	extern int TotShermGrimsInfiltration; // number of Sherman Grimsrud (ZoneInfiltration:ResidentialBasic) in input
	extern int TotAIM2Infiltration; // number of AIM2 (ZoneInfiltration:ResidentialEnhanced) in input
	extern int TotVentilation; // Total Ventilation Statements in input
	extern int TotDesignFlowVentilation; // number of Design Flow rate ZoneVentilation in input
	extern int TotWindAndStackVentilation; // number of wind and stack open area ZoneVentilation in input
	extern int TotMixing; // Total Mixing Statements in input
	extern int TotCrossMixing; // Total Cross Mixing Statements in input
	extern int TotRefDoorMixing; // Total RefrigerationDoor Mixing Statements in input
	extern int TotBBHeat; // Total BBHeat Statements in input
	extern int TotMaterials; // Total number of unique materials (layers) in this simulation
	extern int TotConstructs; // Total number of unique constructions in this simulation
	extern int TotSpectralData; // Total window glass spectral data sets
	extern int W5GlsMat; // Window5 Glass Materials, specified by transmittance and front and back reflectance
	extern int W5GlsMatAlt; // Window5 Glass Materials, specified by index of refraction and extinction coeff
	extern int W5GasMat; // Window5 Single-Gas Materials
	extern int W5GasMatMixture; // Window5 Gas Mixtures
	extern int W7SupportPillars; // Complex fenestration support pillars
	extern int W7DeflectionStates; // Complex fenestration deflection states
	extern int W7MaterialGaps; // Complex fenestration material gaps
	extern int TotBlinds; // Total number of blind materials
	extern int TotScreens; // Total number of exterior window screen materials
	extern int TotTCGlazings; // Number of TC glazing object - WindowMaterial:Glazing:Thermochromic found in the idf file
	extern int NumSurfaceScreens; // Total number of screens on exterior windows
	extern int TotShades; // Total number of shade materials
	extern int TotComplexShades; // Total number of shading materials for complex fenestrations
	extern int TotComplexGaps; // Total number of window gaps for complex fenestrations
	extern int TotSimpleWindow; // number of simple window systems.

	extern int W5GlsMatEQL; // Window5 Single-Gas Materials for Equivalent Layer window model
	extern int TotShadesEQL; // Total number of shade materials for Equivalent Layer window model
	extern int TotDrapesEQL; // Total number of drape materials for Equivalent Layer window model
	extern int TotBlindsEQL; // Total number of blind materials for Equivalent Layer window model
	extern int TotScreensEQL; // Total number of exterior window screen materials for Equivalent Layer window model
	extern int W5GapMatEQL; // Window5 Equivalent Layer Single-Gas Materials

	extern int TotZoneAirBalance; // Total Zone Air Balance Statements in input
	extern int TotFrameDivider; // Total number of window frame/divider objects
	extern int AirFlowFlag;
	extern int TotCO2Gen; // Total CO2 source and sink statements in input
	extern bool CalcWindowRevealReflection; // True if window reveal reflection is to be calculated
	// for at least one exterior window
	extern bool StormWinChangeThisDay; // True if a storm window has been added or removed from any
	// window during the current day; can only be true for first
	// time step of the day.
	extern bool AdaptiveComfortRequested_CEN15251; // true if people objects have adaptive comfort requests. CEN15251
	extern bool AdaptiveComfortRequested_ASH55; // true if people objects have adaptive comfort requests. ASH55
	extern int NumRefrigeratedRacks; // Total number of refrigerated case compressor racks in input
	extern int NumRefrigSystems; // Total number of detailed refrigeration systems in input
	extern int NumRefrigCondensers; // Total number of detailed refrigeration condensers in input
	extern int NumRefrigChillerSets; // Total number of refrigerated warehouse coils in input
	extern Array1D< Real64 > SNLoadHeatEnergy;
	extern Array1D< Real64 > SNLoadCoolEnergy;
	extern Array1D< Real64 > SNLoadHeatRate;
	extern Array1D< Real64 > SNLoadCoolRate;
	extern Array1D< Real64 > SNLoadPredictedRate;
	extern Array1D< Real64 > SNLoadPredictedHSPRate; // Predicted load to heating setpoint (unmultiplied)
	extern Array1D< Real64 > SNLoadPredictedCSPRate; // Predicted load to cooling setpoint (unmultiplied)
	extern Array1D< Real64 > MoisturePredictedRate;

	extern Array1D< Real64 > ListSNLoadHeatEnergy;
	extern Array1D< Real64 > ListSNLoadCoolEnergy;
	extern Array1D< Real64 > ListSNLoadHeatRate;
	extern Array1D< Real64 > ListSNLoadCoolRate;

	extern Array1D< Real64 > GroupSNLoadHeatEnergy;
	extern Array1D< Real64 > GroupSNLoadCoolEnergy;
	extern Array1D< Real64 > GroupSNLoadHeatRate;
	extern Array1D< Real64 > GroupSNLoadCoolRate;

	extern Array1D< Real64 > MRT; // MEAN RADIANT TEMPERATURE (C)
	extern Array1D< Real64 > SUMAI; // 1 over the Sum of zone areas or 1/SumA
	extern Array1D< Real64 > ZoneTransSolar; // Exterior beam plus diffuse solar entering zone;
	//   sum of WinTransSolar for exterior windows in zone (W)
	extern Array1D< Real64 > ZoneWinHeatGain; // Heat gain to zone from all exterior windows (includes
	//   ZoneTransSolar); sum of WinHeatGain for exterior
	//   windows in zone (W)
	extern Array1D< Real64 > ZoneWinHeatGainRep; // = ZoneWinHeatGain when ZoneWinHeatGain >= 0
	extern Array1D< Real64 > ZoneWinHeatLossRep; // = -ZoneWinHeatGain when ZoneWinHeatGain < 0
	extern Array1D< Real64 > ZoneBmSolFrExtWinsRep; // Beam solar into zone from exterior windows [W]
	extern Array1D< Real64 > ZoneBmSolFrIntWinsRep; // Beam solar into zone from interior windows [W]
	extern Array1D< Real64 > InitialZoneDifSolReflW; // Initial diffuse solar in zone from ext and int windows
	// reflected from interior surfaces [W]
	extern Array1D< Real64 > ZoneDifSolFrExtWinsRep; // Diffuse solar into zone from exterior windows [W]
	extern Array1D< Real64 > ZoneDifSolFrIntWinsRep; // Diffuse solar into zone from interior windows [W]
	extern Array1D< Real64 > ZoneOpaqSurfInsFaceCond; // Zone inside face opaque surface conduction (W)
	extern Array1D< Real64 > ZoneOpaqSurfInsFaceCondGainRep; // = Zone inside face opaque surface conduction when >= 0
	extern Array1D< Real64 > ZoneOpaqSurfInsFaceCondLossRep; // = -Zone inside face opaque surface conduction when < 0
	extern Array1D< Real64 > ZoneOpaqSurfExtFaceCond; // Zone outside face opaque surface conduction (W)
	extern Array1D< Real64 > ZoneOpaqSurfExtFaceCondGainRep; // = Zone outside face opaque surface conduction when >= 0
	extern Array1D< Real64 > ZoneOpaqSurfExtFaceCondLossRep; // = -Zone outside face opaque surface conduction when < 0
	extern Array1D< Real64 > QRadThermInAbs; // Thermal radiation absorbed on inside surfaces
	extern Array2D< Real64 > QRadSWwinAbs; // Short wave radiation absorbed in window glass layers
	extern Array2D< Real64 > InitialDifSolwinAbs; // Initial diffuse solar absorbed in window glass layers
	// from inside(W/m2)
	extern Array1D< Real64 > QRadSWOutIncident; // Exterior beam plus diffuse solar incident on surface (W/m2)
	extern Array1D< Real64 > QRadSWOutIncidentBeam; // Exterior beam solar incident on surface (W/m2)
	extern Array1D< Real64 > BmIncInsSurfIntensRep; // Beam sol irrad from ext wins on inside of surface (W/m2)
	extern Array1D< Real64 > BmIncInsSurfAmountRep; // Beam sol amount from ext wins incident on inside of surface (W)
	extern Array1D< Real64 > IntBmIncInsSurfIntensRep; // Beam sol irrad from int wins on inside of surface (W/m2)
	extern Array1D< Real64 > IntBmIncInsSurfAmountRep; // Beam sol amount from int wins incident on inside of surface (W)
	extern Array1D< Real64 > QRadSWOutIncidentSkyDiffuse; // Exterior sky diffuse solar incident on surface (W/m2)
	extern Array1D< Real64 > QRadSWOutIncidentGndDiffuse; // Exterior ground diffuse solar incident on surface (W/m2)
	extern Array1D< Real64 > QRadSWOutIncBmToDiffReflGnd; // Exterior diffuse solar incident from beam to diffuse
	// reflection from ground (W/m2)
	extern Array1D< Real64 > QRadSWOutIncSkyDiffReflGnd; // Exterior diffuse solar incident from sky diffuse
	// reflection from ground (W/m2)
	extern Array1D< Real64 > QRadSWOutIncBmToBmReflObs; // Exterior beam solar incident from beam-to-beam
	// reflection from obstructions (W/m2)
	extern Array1D< Real64 > QRadSWOutIncBmToDiffReflObs; // Exterior diffuse solar incident from beam-to-diffuse
	// reflection from obstructions (W/m2)
	extern Array1D< Real64 > QRadSWOutIncSkyDiffReflObs; // Exterior diffuse solar incident from sky diffuse
	// reflection from obstructions (W/m2)
	extern Array1D< Real64 > CosIncidenceAngle; // Cosine of beam solar incidence angle (for reporting)
	extern Array1D_int BSDFBeamDirectionRep; // BSDF beam direction number for given complex fenestration state (for reporting) []
	extern Array1D< Real64 > BSDFBeamThetaRep; // BSDF beam Theta angle (for reporting) [rad]
	extern Array1D< Real64 > BSDFBeamPhiRep; // BSDF beam Phi angle (for reporting) [rad]

	extern Array1D< Real64 > QRadSWwinAbsTot; // Exterior beam plus diffuse solar absorbed in glass layers of window (W)
	extern Array2D< Real64 > QRadSWwinAbsLayer; // Exterior beam plus diffuse solar absorbed in glass layers of window (W)

	extern Array2D< Real64 > FenLaySurfTempFront; // Front surface temperatures of fenestration layers
	extern Array2D< Real64 > FenLaySurfTempBack; // Back surface temperatures of fenestration layers
	extern Array1D< Real64 > ZoneTransSolarEnergy; // Energy of ZoneTransSolar [J]
	extern Array1D< Real64 > ZoneWinHeatGainRepEnergy; // Energy of ZoneWinHeatGainRep [J]
	extern Array1D< Real64 > ZoneWinHeatLossRepEnergy; // Energy of ZoneWinHeatLossRep [J]
	extern Array1D< Real64 > ZoneBmSolFrExtWinsRepEnergy; // Energy of ZoneBmSolFrExtWinsRep [J]
	extern Array1D< Real64 > ZoneBmSolFrIntWinsRepEnergy; // Energy of ZoneBmSolFrIntWinsRep [J]
	extern Array1D< Real64 > ZoneDifSolFrExtWinsRepEnergy; // Energy of ZoneDifSolFrExtWinsRep [J]
	extern Array1D< Real64 > ZoneDifSolFrIntWinsRepEnergy; // Energy of ZoneDifSolFrIntWinsRep [J]
	extern Array1D< Real64 > ZnOpqSurfInsFaceCondGnRepEnrg; // Energy of ZoneOpaqSurfInsFaceCondGainRep [J]
	extern Array1D< Real64 > ZnOpqSurfInsFaceCondLsRepEnrg; // Energy of ZoneOpaqSurfInsFaceCondLossRep [J]
	extern Array1D< Real64 > ZnOpqSurfExtFaceCondGnRepEnrg; // Energy of ZoneOpaqSurfInsFaceCondGainRep [J]
	extern Array1D< Real64 > ZnOpqSurfExtFaceCondLsRepEnrg; // Energy of ZoneOpaqSurfInsFaceCondLossRep [J]
	extern Array1D< Real64 > BmIncInsSurfAmountRepEnergy; // energy of BmIncInsSurfAmountRep [J]
	extern Array1D< Real64 > IntBmIncInsSurfAmountRepEnergy; // energy of IntBmIncInsSurfAmountRep [J]
	extern Array1D< Real64 > QRadSWwinAbsTotEnergy; // Energy of QRadSWwinAbsTot [J]
	extern Array1D< Real64 > SWwinAbsTotalReport; // Report - Total interior/exterior shortwave
	//absorbed in all glass layers of window (W)
	extern Array1D< Real64 > InitialDifSolInAbsReport; // Report - Initial transmitted diffuse solar
	//absorbed on inside of surface (W)
	extern Array1D< Real64 > InitialDifSolInTransReport; // Report - Initial transmitted diffuse solar
	//transmitted out through inside of window surface (W)
	extern Array1D< Real64 > SWInAbsTotalReport; // Report - Total interior/exterior shortwave
	//absorbed on inside of surface (W)
	extern Array1D< Real64 > SWOutAbsTotalReport; // Report - Total exterior shortwave/solar
	//absorbed on outside of surface (W)
	extern Array1D< Real64 > SWOutAbsEnergyReport; // Report - Total exterior shortwave/solar
	//absorbed on outside of surface (j)

	extern Array1D< Real64 > NominalR; // Nominal R value of each material -- used in matching interzone surfaces
	extern Array1D< Real64 > NominalRforNominalUCalculation; // Nominal R values are summed to calculate NominalU values for constructions
	extern Array1D< Real64 > NominalU; // Nominal U value for each construction -- used in matching interzone surfaces

	// removed variables (these were all arrays):
	//REAL(r64), ALLOCATABLE, :: DifIncInsSurfIntensRep    !Diffuse sol irradiance from ext wins on inside of surface (W/m2)
	//REAL(r64), ALLOCATABLE, :: DifIncInsSurfAmountRep    !Diffuse sol amount from ext wins on inside of surface (W)
	//REAL(r64), ALLOCATABLE, :: IntDifIncInsSurfIntensRep    !Diffuse sol irradiance from int wins on inside of surface (W/m2)
	//REAL(r64), ALLOCATABLE, :: IntDifIncInsSurfAmountRep    !Diffuse sol amount from int wins on inside of surface (W)
	//REAL(r64), ALLOCATABLE, :: DifIncInsSurfAmountRepEnergy    !energy of DifIncInsSurfAmountRep [J]
	//REAL(r64), ALLOCATABLE, :: IntDifIncInsSurfAmountRepEnergy    !energy of IntDifIncInsSurfAmountRep [J]

	// Variables moved from HeatBalanceSurfaceManager and SolarShading
	// to avoid conflict with their use in WindowManager

	extern Array1D< Real64 > TempEffBulkAir; // air temperature adjacent to the surface used for
	// inside surface heat balances
	extern Array1D< Real64 > HConvIn; // INSIDE CONVECTION COEFFICIENT
	extern Array1D< Real64 > AnisoSkyMult; // Multiplier on exterior-surface sky view factor to
	// account for anisotropy of sky radiance; = 1.0 for
	// for isotropic sky

	// Moved from SolarShading to avoid conflicts in DaylightingDevices
	extern Array1D< Real64 > DifShdgRatioIsoSky; // Diffuse shading ratio (WithShdgIsoSky/WoShdgIsoSky)
	extern Array3D< Real64 > DifShdgRatioIsoSkyHRTS; // Diffuse shading ratio (WithShdgIsoSky/WoShdgIsoSky)
	extern Array1D< Real64 > curDifShdgRatioIsoSky; // Diffuse shading ratio (WithShdgIsoSky/WoShdgIsoSky)
	extern Array1D< Real64 > DifShdgRatioHoriz; // Horizon shading ratio (WithShdgHoriz/WoShdgHoriz)
	extern Array3D< Real64 > DifShdgRatioHorizHRTS; // Horizon shading ratio (WithShdgHoriz/WoShdgHoriz)
	extern Array1D< Real64 > WithShdgIsoSky; // Diffuse solar irradiance from sky on surface, with shading
	extern Array1D< Real64 > WoShdgIsoSky; // Diffuse solar from sky on surface, without shading
	extern Array1D< Real64 > WithShdgHoriz; // Diffuse solar irradiance from horizon portion of sky on surface,
	// with shading
	extern Array1D< Real64 > WoShdgHoriz; // Diffuse solar irradiance from horizon portion of sky on surface,
	// without shading
	extern Array1D< Real64 > MultIsoSky; // Contribution to eff sky view factor from isotropic sky
	extern Array1D< Real64 > MultCircumSolar; // Contribution to eff sky view factor from circumsolar brightening
	extern Array1D< Real64 > MultHorizonZenith; // Contribution to eff sky view factor from horizon or zenith brightening

	extern Array1D< Real64 > QS; // Zone short-wave flux density; used to calculate short-wave
	//     radiation absorbed on inside surfaces of zone
	extern Array1D< Real64 > QSLights; // Like QS, but Lights short-wave only.

	extern Array1D< Real64 > QSDifSol; // Like QS, but diffuse solar short-wave only.
	extern Array1D< Real64 > ITABSF; // FRACTION OF THERMAL FLUX ABSORBED (PER UNIT AREA)
	extern Array1D< Real64 > TMULT; // TMULT  - MULTIPLIER TO COMPUTE 'ITABSF'
	extern Array1D< Real64 > QL; // TOTAL THERMAL RADIATION ADDED TO ZONE
	extern Array2D< Real64 > SunlitFracHR; // Hourly fraction of heat transfer surface that is sunlit
	extern Array2D< Real64 > CosIncAngHR; // Hourly cosine of beam radiation incidence angle on surface
	extern Array3D< Real64 > SunlitFrac; // TimeStep fraction of heat transfer surface that is sunlit
	extern Array3D< Real64 > SunlitFracWithoutReveal; // For a window with reveal, the sunlit fraction
	// without shadowing by the reveal
	extern Array3D< Real64 > CosIncAng; // TimeStep cosine of beam radiation incidence angle on surface
	extern Array4D_int BackSurfaces; // For a given hour and timestep, a list of up to 20 surfaces receiving
	// beam solar radiation from a given exterior window
	extern Array4D< Real64 > OverlapAreas; // For a given hour and timestep, the areas of the exterior window sending
	// beam solar radiation to the surfaces listed in BackSurfaces
	//                       Air       Argon     Krypton   Xenon
	extern Array2D< Real64 > const GasCoeffsCon; // Gas conductivity coefficients for gases in a mixture

	//                       Air       Argon     Krypton   Xenon
	extern Array2D< Real64 > const GasCoeffsVis; // Gas viscosity coefficients for gases in a mixture

	//                     Air       Argon     Krypton   Xenon
	extern Array2D< Real64 > const GasCoeffsCp; // Gas specific heat coefficients for gases in a mixture

	//                       Air       Argon     Krypton   Xenon
	extern Array1D< Real64 > const GasWght; // Gas molecular weights for gases in a mixture

	extern Array1D< Real64 > const GasSpecificHeatRatio; // Gas specific heat ratios.  Used for gasses in low pressure

	//Variables Dimensioned to Number of Zones
	extern Array1D< Real64 > MVFC; // Design Mixing Flow Rate [m3/s] (Cross Zone Mixing)
	extern Array1D< Real64 > MTC; // Control Temperature For Mixing [C] (Cross Zone Mixing)

	extern Real64 ZeroPointerVal;

	// SUBROUTINE SPECIFICATIONS FOR MODULE DataHeatBalance:

	// Types

	struct MaterialProperties
	{
		// Members
		std::string Name; // Name of material layer
		int Group; // Material group type (see Material Parameters above.  Currently
		// active: RegularMaterial, Shade, Air, WindowGlass,
		// WindowGas, WindowBlind, WindowGasMixture, Screen, EcoRoof,
		// IRTMaterial, WindowSimpleGlazing, ComplexWindowShade, ComplexWindowGap)
		int Roughness; // Surface roughness index (See Surface Roughness parameters
		// above.  Current: VerySmooth, Smooth, MediumSmooth,
		// MediumRough, Rough, VeryRough)
		// Thermo-physical material properties
		Real64 Conductivity; // Thermal conductivity of layer (W/m2K)
		Real64 Density; // Layer density (kg/m3)
		Real64 IsoMoistCap; // Isothermal moisture capacity on water vapor density (m3/kg)
		Real64 Porosity; // Layer porosity
		Real64 Resistance; // Layer thermal resistance (alternative to Density,
		// Conductivity, Thickness, and Specific Heat; K/W)
		bool ROnly; // Material defined with "R" only
		Real64 SpecHeat; // Layer specific heat (J/kgK)
		Real64 ThermGradCoef; // Thermal-gradient coefficient for moisture capacity
		// based on the water vapor density (kg/kgK)
		Real64 Thickness; // Layer thickness (m)
		Real64 VaporDiffus; // Layer vapor diffusivity
		Array1D_int GasType; // Gas type (air=1, argon=2, krypton=3, xenon=4, custom=0) for
		//  up to 5 gases in a mixture [Window gas only].  It is defined as parameter (GasCoefs)
		int GlassSpectralDataPtr; // Number of a spectral data set associated with a window glass material
		int NumberOfGasesInMixture; // Number of gases in a window gas mixture
		Array2D< Real64 > GasCon; // Gas conductance coefficients for up to 5 gases in a mixture
		Array2D< Real64 > GasVis; // Gas viscosity coefficients for up to 5 gases in a mixture
		Array2D< Real64 > GasCp; // Gas specific-heat coefficients for up to 5 gases in a mixture
		Array1D< Real64 > GasWght; // Gas molecular weight for up to 5 gases in a mixture
		Array1D< Real64 > GasSpecHeatRatio; // Gas specific heat ratio (used for low pressure calculations)
		Array1D< Real64 > GasFract; // Gas fractions for up to 5 gases in a mixture
		// Radiation parameters
		Real64 AbsorpSolar; // Layer solar absorptance
		Real64 AbsorpSolarInput; // Layer solar absorptance input by user
		bool AbsorpSolarEMSOverrideOn; // if true, then EMS calling to override value for solar absorptance
		Real64 AbsorpSolarEMSOverride; // value to use when EMS calling to override value for solar absorptance
		Real64 AbsorpThermal; // Layer thermal absorptance
		Real64 AbsorpThermalInput; // Layer thermal absorptance input by user
		bool AbsorpThermalEMSOverrideOn; // if true, then EMS calling to override value for thermal absorptance
		Real64 AbsorpThermalEMSOverride; // value to use when EMS calling to override value for thermal absorptance
		Real64 AbsorpVisible; // Layer Visible Absorptance
		Real64 AbsorpVisibleInput; // Layer Visible Absorptance input by user
		bool AbsorpVisibleEMSOverrideOn; // if true, then EMS calling to override value for visible absorptance
		Real64 AbsorpVisibleEMSOverride; // value to use when EMS calling to override value for visible absorptance
		// Window-related radiation parameters
		Real64 Trans; // Transmittance of layer (glass, shade)
		Real64 TransVis; // Visible transmittance (at normal incidence)
		Real64 GlassTransDirtFactor; // Multiplier on glass transmittance due to dirt
		bool SolarDiffusing; // True if glass diffuses beam solar radiation
		Real64 ReflectShade; // Shade or screen reflectance (interior shade only)
		Real64 ReflectShadeVis; // Shade reflectance for visible radiation
		Real64 AbsorpThermalBack; // Infrared radiation back absorption
		Real64 AbsorpThermalFront; // Infrared radiation front absorption
		Real64 ReflectSolBeamBack; // Solar back reflectance (beam to everything)
		Real64 ReflectSolBeamFront; // Solar front reflectance (beam to everything)
		Real64 ReflectSolDiffBack; // Solar back diffuse reflectance
		Real64 ReflectSolDiffFront; // Solar front diffuse reflectance
		Real64 ReflectVisBeamBack; // Visible back reflectance (beam to everything)
		Real64 ReflectVisBeamFront; // Visible front reflectance (beam to everything)
		Real64 ReflectVisDiffBack; // Visible back diffuse reflectance
		Real64 ReflectVisDiffFront; // Visible front diffuse reflectance
		std::string ReflectanceModeling; // method used to account for screen scattering
		Real64 TransSolBeam; // Solar transmittance (beam to everything)
		Real64 TransThermal; // Infrared radiation transmittance
		Real64 TransVisBeam; // Visible transmittance (beam to everything)
		int BlindDataPtr; // Pointer to window blind data
		int ScreenDataPtr; // Pointer to window screen data
		int ScreenMapResolution; // Resolution of azimuth and altitude angles to print in transmittance map
		// Complex fenestration parameters
		Real64 YoungModulus; // Young's modulus (Pa) - used in window deflection calculations
		Real64 PoissonsRatio; // Poisson's ratio - used in window deflection calculations
		Real64 DeflectedThickness; // Minimum gap thickness in deflected state (m).  Used with measured deflection
		Real64 Pressure; // Window Gap pressure (Pa)
		int SupportPillarPtr; // Pointer to support pillar data
		int DeflectionStatePtr; // Pointer to deflection state
		int ComplexShadePtr; // Pointer to complex shade data
		int GasPointer; // Pointer to gas or gas mixture used in the gap
		// Window-shade thermal model parameters
		Real64 WinShadeToGlassDist; // Distance between window shade and adjacent glass (m)
		Real64 WinShadeTopOpeningMult; // Area of air-flow opening at top of shade, expressed as a fraction
		//  of the shade-to-glass opening area at the top of the shade
		Real64 WinShadeBottomOpeningMult; // Area of air-flow opening at bottom of shade, expressed as a fraction
		//  of the shade-to-glass opening area at the bottom of the shade
		Real64 WinShadeLeftOpeningMult; // Area of air-flow opening at left side of shade, expressed as a fraction
		//  of the shade-to-glass opening area at the left side of the shade
		Real64 WinShadeRightOpeningMult; // Area of air-flow opening at right side of shade, expressed as a fraction
		//  of the shade-to-glass opening area at the right side of the shade
		Real64 WinShadeAirFlowPermeability; // The effective area of openings in the shade itself, expressed as a
		//  fraction of the shade area
		bool EMPDMaterialProps; // True if EMPD properties have been assigned
		Real64 EMPDVALUE;
		Real64 MoistACoeff;
		Real64 MoistBCoeff;
		Real64 MoistCCoeff;
		Real64 MoistDCoeff;
		Real64 EMPDaCoeff;
		Real64 EMPDbCoeff;
		Real64 EMPDcCoeff;
		Real64 EMPDdCoeff;
		// EcoRoof-Related properties, essentially for the plant layer,
		//    the soil layer uses the same resource as a regular material
		int EcoRoofCalculationMethod; // 1-Simple, 2-SchaapGenuchten
		Real64 HeightOfPlants; // plants' height
		Real64 LAI; // LeafAreaIndex (Dimensionless???)
		Real64 Lreflectivity; // LeafReflectivity
		Real64 LEmissitivity; // LeafEmissivity
		Real64 InitMoisture; // Initial soil moisture DJS
		Real64 MinMoisture; // Minimum moisture allowed DJS
		Real64 RStomata; // Minimum stomatal resistance DJS
		// HAMT
		int niso; // Number of data points
		Array1D< Real64 > isodata; // isotherm values
		Array1D< Real64 > isorh; // isotherm RH values
		int nsuc; // Number of data points
		Array1D< Real64 > sucdata; // suction values
		Array1D< Real64 > sucwater; // suction water values
		int nred; // Number of data points
		Array1D< Real64 > reddata; // redistribution values
		Array1D< Real64 > redwater; // redistribution water values
		int nmu; // Number of data points
		Array1D< Real64 > mudata; // mu values
		Array1D< Real64 > murh; // mu rh values
		int ntc; // Number of data points
		Array1D< Real64 > tcdata; // thermal conductivity values
		Array1D< Real64 > tcwater; // thermal conductivity water values
		Real64 itemp; // initial Temperature
		Real64 irh; // Initial RH
		Real64 iwater; // Initial water content kg/kg
		int divs; // Number of divisions
		Real64 divsize; // Average Cell Size
		int divmin; // Minimum number of cells
		int divmax; // Maximum number of cells
		// Added 12/22/2008 for thermochromic window glazing material
		Real64 SpecTemp; // Temperature corresponding to the specified material properties
		int TCParent; // Reference to the parent object WindowMaterial:Glazing:Thermochromic
		// Simple Glazing System
		Real64 SimpleWindowUfactor; // user input for simple window U-factor with film coefs (W/m2-k)
		Real64 SimpleWindowSHGC; // user input for simple window Solar Heat Gain Coefficient (non-dimensional)
		Real64 SimpleWindowVisTran; // (optional) user input for simple window Visual Transmittance (non-dimensional)
		bool SimpleWindowVTinputByUser; // false means not input, true means user provide VT input
		bool WarnedForHighDiffusivity; // used to limit error messaging to just the first instance
		// Equivalent Layer (ASHWAT) Model
		Real64 ReflFrontBeamBeam; // Beam-Beam solar reflectance front at zero incident
		Real64 ReflBackBeamBeam; // Beam-Beam solar reflectance back at zero incident
		Real64 TausFrontBeamBeam; // Beam-Beam solar transmittance front at zero incident
		Real64 TausBackBeamBeam; // Beam-Beam solar transmittance back at zero incident
		Real64 ReflFrontBeamBeamVis; // Beam-Beam visible reflectance front at zero incident
		Real64 ReflBackBeamBeamVis; // Beam-Beam visible reflectance back at zero incident
		Real64 TausFrontBeamBeamVis; // Beam-Beam visible transmittance front at zero incident
		Real64 TausBackBeamBeamVis; // Beam-Beam visible transmittance back at zero incident
		Real64 ReflFrontBeamDiff; // Beam-Diffuse solar reflectance front at zero incident
		Real64 ReflBackBeamDiff; // Beam-Diffuse solar reflectance back at zero incident
		Real64 TausFrontBeamDiff; // Beam-Diffuse solar transmittance front at zero incident
		Real64 TausBackBeamDiff; // Beam-Diffuse solar transmittance back at zero incident
		Real64 ReflFrontBeamDiffVis; // Beam-Diffuse visible reflectance front at zero incident
		Real64 ReflBackBeamDiffVis; // Beam-Diffuse visible reflectance back at zero incident
		Real64 TausFrontBeamDiffVis; // Beam-Diffuse visible transmittance front at zero incident
		Real64 TausBackBeamDiffVis; // Beam-Diffuse visible transmittance back at zero incident
		Real64 ReflFrontDiffDiff; // Diffuse-Diffuse solar reflectance front
		Real64 ReflBackDiffDiff; // Diffuse-Diffuse solar reflectance back
		Real64 TausDiffDiff; // Diffuse-Diffuse solar transmittance (front and back)
		Real64 ReflFrontDiffDiffVis; // Diffuse-Diffuse visible reflectance front
		Real64 ReflBackDiffDiffVis; // Diffuse-Diffuse visible reflectance back
		Real64 TausDiffDiffVis; // Diffuse-Diffuse visible transmittance (front and back)
		Real64 EmissThermalFront; // Front side thermal or infrared Emissivity
		Real64 EmissThermalBack; // Back side thermal or infrared Emissivity
		Real64 TausThermal; // Thermal transmittance (front and back)
		int GapVentType; // Gap Ven type for equivalent Layer window model
		bool ISPleatedDrape; // if pleated drape= true, if nonpleated drape = false
		Real64 PleatedDrapeWidth; // width of the pleated drape fabric section
		Real64 PleatedDrapeLength; // length of the pleated drape fabric section
		Real64 ScreenWireSpacing; // insect screen wire spacing
		Real64 ScreenWireDiameter; // insect screen wire diameter
		Real64 SlatWidth; // slat width
		Real64 SlatSeparation; // slat seperation
		Real64 SlatCrown; // slat crown
		Real64 SlatAngle; // slat angle
		int SlatAngleType; // slat angle control type, 0=fixed, 1=maximize solar, 2=block beam
		int SlatOrientation; // horizontal or veritical
		std::string GasName; // Name of gas type ("Air", "Argon", "Krypton", "Xenon")

		// Default Constructor
		MaterialProperties() :
			Group( -1 ),
			Roughness( 0 ),
			Conductivity( 0.0 ),
			Density( 0.0 ),
			IsoMoistCap( 0.0 ),
			Porosity( 0.0 ),
			Resistance( 0.0 ),
			ROnly( false ),
			SpecHeat( 0.0 ),
			ThermGradCoef( 0.0 ),
			Thickness( 0.0 ),
			VaporDiffus( 0.0 ),
			GasType( 5, 0 ),
			GlassSpectralDataPtr( 0 ),
			NumberOfGasesInMixture( 0 ),
			GasCon( 3, 5, 0.0 ),
			GasVis( 3, 5, 0.0 ),
			GasCp( 3, 5, 0.0 ),
			GasWght( 5, 0.0 ),
			GasSpecHeatRatio( 5, 0.0 ),
			GasFract( 5, 0.0 ),
			AbsorpSolar( 0.0 ),
			AbsorpSolarInput( 0.0 ),
			AbsorpSolarEMSOverrideOn( false ),
			AbsorpSolarEMSOverride( 0.0 ),
			AbsorpThermal( 0.0 ),
			AbsorpThermalInput( 0.0 ),
			AbsorpThermalEMSOverrideOn( false ),
			AbsorpThermalEMSOverride( 0.0 ),
			AbsorpVisible( 0.0 ),
			AbsorpVisibleInput( 0.0 ),
			AbsorpVisibleEMSOverrideOn( false ),
			AbsorpVisibleEMSOverride( 0.0 ),
			Trans( 0.0 ),
			TransVis( 0.0 ),
			GlassTransDirtFactor( 1.0 ),
			SolarDiffusing( false ),
			ReflectShade( 0.0 ),
			ReflectShadeVis( 0.0 ),
			AbsorpThermalBack( 0.0 ),
			AbsorpThermalFront( 0.0 ),
			ReflectSolBeamBack( 0.0 ),
			ReflectSolBeamFront( 0.0 ),
			ReflectSolDiffBack( 0.0 ),
			ReflectSolDiffFront( 0.0 ),
			ReflectVisBeamBack( 0.0 ),
			ReflectVisBeamFront( 0.0 ),
			ReflectVisDiffBack( 0.0 ),
			ReflectVisDiffFront( 0.0 ),
			TransSolBeam( 0.0 ),
			TransThermal( 0.0 ),
			TransVisBeam( 0.0 ),
			BlindDataPtr( 0 ),
			ScreenDataPtr( 0 ),
			ScreenMapResolution( 0 ),
			YoungModulus( 0.0 ),
			PoissonsRatio( 0.0 ),
			DeflectedThickness( 0.0 ),
			Pressure( 0.0 ),
			SupportPillarPtr( 0 ),
			DeflectionStatePtr( 0 ),
			ComplexShadePtr( 0 ),
			GasPointer( 0 ),
			WinShadeToGlassDist( 0.0 ),
			WinShadeTopOpeningMult( 0.0 ),
			WinShadeBottomOpeningMult( 0.0 ),
			WinShadeLeftOpeningMult( 0.0 ),
			WinShadeRightOpeningMult( 0.0 ),
			WinShadeAirFlowPermeability( 0.0 ),
			EMPDMaterialProps( false ),
			EMPDVALUE( 0.0 ),
			MoistACoeff( 0.0 ),
			MoistBCoeff( 0.0 ),
			MoistCCoeff( 0.0 ),
			MoistDCoeff( 0.0 ),
			EMPDaCoeff( 0.0 ),
			EMPDbCoeff( 0.0 ),
			EMPDcCoeff( 0.0 ),
			EMPDdCoeff( 0.0 ),
			EcoRoofCalculationMethod( 0 ),
			HeightOfPlants( 0.0 ),
			LAI( 0.0 ),
			Lreflectivity( 0.0 ),
			LEmissitivity( 0.0 ),
			InitMoisture( 0.0 ),
			MinMoisture( 0.0 ),
			RStomata( 0.0 ),
			niso( -1 ),
			isodata( 27, 0.0 ),
			isorh( 27, 0.0 ),
			nsuc( -1 ),
			sucdata( 27, 0.0 ),
			sucwater( 27, 0.0 ),
			nred( -1 ),
			reddata( 27, 0.0 ),
			redwater( 27, 0.0 ),
			nmu( -1 ),
			mudata( 27, 0.0 ),
			murh( 27, 0.0 ),
			ntc( -1 ),
			tcdata( 27, 0.0 ),
			tcwater( 27, 0.0 ),
			itemp( 10.0 ),
			irh( 0.5 ),
			iwater( 0.2 ),
			divs( 3 ),
			divsize( 0.005 ),
			divmin( 3 ),
			divmax( 10 ),
			SpecTemp( 0.0 ),
			TCParent( 0 ),
			SimpleWindowUfactor( 0.0 ),
			SimpleWindowSHGC( 0.0 ),
			SimpleWindowVisTran( 0.0 ),
			SimpleWindowVTinputByUser( false ),
			WarnedForHighDiffusivity( false ),
			ReflFrontBeamBeam( 0.0 ),
			ReflBackBeamBeam( 0.0 ),
			TausFrontBeamBeam( 0.0 ),
			TausBackBeamBeam( 0.0 ),
			ReflFrontBeamBeamVis( 0.0 ),
			ReflBackBeamBeamVis( 0.0 ),
			TausFrontBeamBeamVis( 0.0 ),
			TausBackBeamBeamVis( 0.0 ),
			ReflFrontBeamDiff( 0.0 ),
			ReflBackBeamDiff( 0.0 ),
			TausFrontBeamDiff( 0.0 ),
			TausBackBeamDiff( 0.0 ),
			ReflFrontBeamDiffVis( 0.0 ),
			ReflBackBeamDiffVis( 0.0 ),
			TausFrontBeamDiffVis( 0.0 ),
			TausBackBeamDiffVis( 0.0 ),
			ReflFrontDiffDiff( 0.0 ),
			ReflBackDiffDiff( 0.0 ),
			TausDiffDiff( 0.0 ),
			ReflFrontDiffDiffVis( 0.0 ),
			ReflBackDiffDiffVis( 0.0 ),
			TausDiffDiffVis( 0.0 ),
			EmissThermalFront( 0.0 ),
			EmissThermalBack( 0.0 ),
			TausThermal( 0.0 ),
			GapVentType( 0 ),
			ISPleatedDrape( false ),
			PleatedDrapeWidth( 0.0 ),
			PleatedDrapeLength( 0.0 ),
			ScreenWireSpacing( 0.0 ),
			ScreenWireDiameter( 0.0 ),
			SlatWidth( 0.0 ),
			SlatSeparation( 0.0 ),
			SlatCrown( 0.0 ),
			SlatAngle( 0.0 ),
			SlatAngleType( 0 ),
			SlatOrientation( 0 )
		{}

		// Member Constructor
		MaterialProperties(
			std::string const & Name, // Name of material layer
			int const Group, // Material group type (see Material Parameters above.  Currently
			int const Roughness, // Surface roughness index (See Surface Roughness parameters
			Real64 const Conductivity, // Thermal conductivity of layer (W/m2K)
			Real64 const Density, // Layer density (kg/m3)
			Real64 const IsoMoistCap, // Isothermal moisture capacity on water vapor density (m3/kg)
			Real64 const Porosity, // Layer porosity
			Real64 const Resistance, // Layer thermal resistance (alternative to Density,
			bool const ROnly, // Material defined with "R" only
			Real64 const SpecHeat, // Layer specific heat (J/kgK)
			Real64 const ThermGradCoef, // Thermal-gradient coefficient for moisture capacity
			Real64 const Thickness, // Layer thickness (m)
			Real64 const VaporDiffus, // Layer vapor diffusivity
			Array1_int const & GasType, // Gas type (air=1, argon=2, krypton=3, xenon=4, custom=0) for
			int const GlassSpectralDataPtr, // Number of a spectral data set associated with a window glass material
			int const NumberOfGasesInMixture, // Number of gases in a window gas mixture
			Array2< Real64 > const & GasCon, // Gas conductance coefficients for up to 5 gases in a mixture
			Array2< Real64 > const & GasVis, // Gas viscosity coefficients for up to 5 gases in a mixture
			Array2< Real64 > const & GasCp, // Gas specific-heat coefficients for up to 5 gases in a mixture
			Array1< Real64 > const & GasWght, // Gas molecular weight for up to 5 gases in a mixture
			Array1< Real64 > const & GasSpecHeatRatio, // Gas specific heat ratio (used for low pressure calculations)
			Array1< Real64 > const & GasFract, // Gas fractions for up to 5 gases in a mixture
			Real64 const AbsorpSolar, // Layer solar absorptance
			Real64 const AbsorpSolarInput, // Layer solar absorptance input by user
			bool const AbsorpSolarEMSOverrideOn, // if true, then EMS calling to override value for solar absorptance
			Real64 const AbsorpSolarEMSOverride, // value to use when EMS calling to override value for solar absorptance
			Real64 const AbsorpThermal, // Layer thermal absorptance
			Real64 const AbsorpThermalInput, // Layer thermal absorptance input by user
			bool const AbsorpThermalEMSOverrideOn, // if true, then EMS calling to override value for thermal absorptance
			Real64 const AbsorpThermalEMSOverride, // value to use when EMS calling to override value for thermal absorptance
			Real64 const AbsorpVisible, // Layer Visible Absorptance
			Real64 const AbsorpVisibleInput, // Layer Visible Absorptance input by user
			bool const AbsorpVisibleEMSOverrideOn, // if true, then EMS calling to override value for visible absorptance
			Real64 const AbsorpVisibleEMSOverride, // value to use when EMS calling to override value for visible absorptance
			Real64 const Trans, // Transmittance of layer (glass, shade)
			Real64 const TransVis, // Visible transmittance (at normal incidence)
			Real64 const GlassTransDirtFactor, // Multiplier on glass transmittance due to dirt
			bool const SolarDiffusing, // True if glass diffuses beam solar radiation
			Real64 const ReflectShade, // Shade or screen reflectance (interior shade only)
			Real64 const ReflectShadeVis, // Shade reflectance for visible radiation
			Real64 const AbsorpThermalBack, // Infrared radiation back absorption
			Real64 const AbsorpThermalFront, // Infrared radiation front absorption
			Real64 const ReflectSolBeamBack, // Solar back reflectance (beam to everything)
			Real64 const ReflectSolBeamFront, // Solar front reflectance (beam to everything)
			Real64 const ReflectSolDiffBack, // Solar back diffuse reflectance
			Real64 const ReflectSolDiffFront, // Solar front diffuse reflectance
			Real64 const ReflectVisBeamBack, // Visible back reflectance (beam to everything)
			Real64 const ReflectVisBeamFront, // Visible front reflectance (beam to everything)
			Real64 const ReflectVisDiffBack, // Visible back diffuse reflectance
			Real64 const ReflectVisDiffFront, // Visible front diffuse reflectance
			std::string const & ReflectanceModeling, // method used to account for screen scattering
			Real64 const TransSolBeam, // Solar transmittance (beam to everything)
			Real64 const TransThermal, // Infrared radiation transmittance
			Real64 const TransVisBeam, // Visible transmittance (beam to everything)
			int const BlindDataPtr, // Pointer to window blind data
			int const ScreenDataPtr, // Pointer to window screen data
			int const ScreenMapResolution, // Resolution of azimuth and altitude angles to print in transmittance map
			Real64 const YoungModulus, // Young's modulus (Pa) - used in window deflection calculations
			Real64 const PoissonsRatio, // Poisson's ratio - used in window deflection calculations
			Real64 const DeflectedThickness, // Minimum gap thickness in deflected state (m).  Used with measured deflection
			Real64 const Pressure, // Window Gap pressure (Pa)
			int const SupportPillarPtr, // Pointer to support pillar data
			int const DeflectionStatePtr, // Pointer to deflection state
			int const ComplexShadePtr, // Pointer to complex shade data
			int const GasPointer, // Pointer to gas or gas mixture used in the gap
			Real64 const WinShadeToGlassDist, // Distance between window shade and adjacent glass (m)
			Real64 const WinShadeTopOpeningMult, // Area of air-flow opening at top of shade, expressed as a fraction
			Real64 const WinShadeBottomOpeningMult, // Area of air-flow opening at bottom of shade, expressed as a fraction
			Real64 const WinShadeLeftOpeningMult, // Area of air-flow opening at left side of shade, expressed as a fraction
			Real64 const WinShadeRightOpeningMult, // Area of air-flow opening at right side of shade, expressed as a fraction
			Real64 const WinShadeAirFlowPermeability, // The effective area of openings in the shade itself, expressed as a
			bool const EMPDMaterialProps, // True if EMPD properties have been assigned
			Real64 const EMPDVALUE,
			Real64 const MoistACoeff,
			Real64 const MoistBCoeff,
			Real64 const MoistCCoeff,
			Real64 const MoistDCoeff,
			Real64 const EMPDaCoeff,
			Real64 const EMPDbCoeff,
			Real64 const EMPDcCoeff,
			Real64 const EMPDdCoeff,
			int const EcoRoofCalculationMethod, // 1-Simple, 2-SchaapGenuchten
			Real64 const HeightOfPlants, // plants' height
			Real64 const LAI, // LeafAreaIndex (Dimensionless???)
			Real64 const Lreflectivity, // LeafReflectivity
			Real64 const LEmissitivity, // LeafEmissivity
			Real64 const InitMoisture, // Initial soil moisture DJS
			Real64 const MinMoisture, // Minimum moisture allowed DJS
			Real64 const RStomata, // Minimum stomatal resistance DJS
			int const niso, // Number of data points
			Array1< Real64 > const & isodata, // isotherm values
			Array1< Real64 > const & isorh, // isotherm RH values
			int const nsuc, // Number of data points
			Array1< Real64 > const & sucdata, // suction values
			Array1< Real64 > const & sucwater, // suction water values
			int const nred, // Number of data points
			Array1< Real64 > const & reddata, // redistribution values
			Array1< Real64 > const & redwater, // redistribution water values
			int const nmu, // Number of data points
			Array1< Real64 > const & mudata, // mu values
			Array1< Real64 > const & murh, // mu rh values
			int const ntc, // Number of data points
			Array1< Real64 > const & tcdata, // thermal conductivity values
			Array1< Real64 > const & tcwater, // thermal conductivity water values
			Real64 const itemp, // initial Temperature
			Real64 const irh, // Initial RH
			Real64 const iwater, // Initial water content kg/kg
			int const divs, // Number of divisions
			Real64 const divsize, // Average Cell Size
			int const divmin, // Minimum number of cells
			int const divmax, // Maximum number of cells
			Real64 const SpecTemp, // Temperature corresponding to the specified material properties
			int const TCParent, // Reference to the parent object WindowMaterial:Glazing:Thermochromic
			Real64 const SimpleWindowUfactor, // user input for simple window U-factor with film coefs (W/m2-k)
			Real64 const SimpleWindowSHGC, // user input for simple window Solar Heat Gain Coefficient (non-dimensional)
			Real64 const SimpleWindowVisTran, // (optional) user input for simple window Visual Transmittance (non-dimensional)
			bool const SimpleWindowVTinputByUser, // false means not input, true means user provide VT input
			bool const WarnedForHighDiffusivity, // used to limit error messaging to just the first instance
			Real64 const ReflFrontBeamBeam, // Beam-Beam solar reflectance front at zero incident
			Real64 const ReflBackBeamBeam, // Beam-Beam solar reflectance back at zero incident
			Real64 const TausFrontBeamBeam, // Beam-Beam solar transmittance front at zero incident
			Real64 const TausBackBeamBeam, // Beam-Beam solar transmittance back at zero incident
			Real64 const ReflFrontBeamBeamVis, // Beam-Beam visible reflectance front at zero incident
			Real64 const ReflBackBeamBeamVis, // Beam-Beam visible reflectance back at zero incident
			Real64 const TausFrontBeamBeamVis, // Beam-Beam visible transmittance front at zero incident
			Real64 const TausBackBeamBeamVis, // Beam-Beam visible transmittance back at zero incident
			Real64 const ReflFrontBeamDiff, // Beam-Diffuse solar reflectance front at zero incident
			Real64 const ReflBackBeamDiff, // Beam-Diffuse solar reflectance back at zero incident
			Real64 const TausFrontBeamDiff, // Beam-Diffuse solar transmittance front at zero incident
			Real64 const TausBackBeamDiff, // Beam-Diffuse solar transmittance back at zero incident
			Real64 const ReflFrontBeamDiffVis, // Beam-Diffuse visible reflectance front at zero incident
			Real64 const ReflBackBeamDiffVis, // Beam-Diffuse visible reflectance back at zero incident
			Real64 const TausFrontBeamDiffVis, // Beam-Diffuse visible transmittance front at zero incident
			Real64 const TausBackBeamDiffVis, // Beam-Diffuse visible transmittance back at zero incident
			Real64 const ReflFrontDiffDiff, // Diffuse-Diffuse solar reflectance front
			Real64 const ReflBackDiffDiff, // Diffuse-Diffuse solar reflectance back
			Real64 const TausDiffDiff, // Diffuse-Diffuse solar transmittance (front and back)
			Real64 const ReflFrontDiffDiffVis, // Diffuse-Diffuse visible reflectance front
			Real64 const ReflBackDiffDiffVis, // Diffuse-Diffuse visible reflectance back
			Real64 const TausDiffDiffVis, // Diffuse-Diffuse visible transmittance (front and back)
			Real64 const EmissThermalFront, // Front side thermal or infrared Emissivity
			Real64 const EmissThermalBack, // Back side thermal or infrared Emissivity
			Real64 const TausThermal, // Thermal transmittance (front and back)
			int const GapVentType, // Gap Ven type for equivalent Layer window model
			bool const ISPleatedDrape, // if pleated drape= true, if nonpleated drape = false
			Real64 const PleatedDrapeWidth, // width of the pleated drape fabric section
			Real64 const PleatedDrapeLength, // length of the pleated drape fabric section
			Real64 const ScreenWireSpacing, // insect screen wire spacing
			Real64 const ScreenWireDiameter, // insect screen wire diameter
			Real64 const SlatWidth, // slat width
			Real64 const SlatSeparation, // slat seperation
			Real64 const SlatCrown, // slat crown
			Real64 const SlatAngle, // slat angle
			int const SlatAngleType, // slat angle control type, 0=fixed, 1=maximize solar, 2=block beam
			int const SlatOrientation, // horizontal or veritical
			std::string const & GasName // Name of gas type ("Air", "Argon", "Krypton", "Xenon")
		) :
			Name( Name ),
			Group( Group ),
			Roughness( Roughness ),
			Conductivity( Conductivity ),
			Density( Density ),
			IsoMoistCap( IsoMoistCap ),
			Porosity( Porosity ),
			Resistance( Resistance ),
			ROnly( ROnly ),
			SpecHeat( SpecHeat ),
			ThermGradCoef( ThermGradCoef ),
			Thickness( Thickness ),
			VaporDiffus( VaporDiffus ),
			GasType( 5, GasType ),
			GlassSpectralDataPtr( GlassSpectralDataPtr ),
			NumberOfGasesInMixture( NumberOfGasesInMixture ),
			GasCon( 3, 5, GasCon ),
			GasVis( 3, 5, GasVis ),
			GasCp( 3, 5, GasCp ),
			GasWght( 5, GasWght ),
			GasSpecHeatRatio( 5, GasSpecHeatRatio ),
			GasFract( 5, GasFract ),
			AbsorpSolar( AbsorpSolar ),
			AbsorpSolarInput( AbsorpSolarInput ),
			AbsorpSolarEMSOverrideOn( AbsorpSolarEMSOverrideOn ),
			AbsorpSolarEMSOverride( AbsorpSolarEMSOverride ),
			AbsorpThermal( AbsorpThermal ),
			AbsorpThermalInput( AbsorpThermalInput ),
			AbsorpThermalEMSOverrideOn( AbsorpThermalEMSOverrideOn ),
			AbsorpThermalEMSOverride( AbsorpThermalEMSOverride ),
			AbsorpVisible( AbsorpVisible ),
			AbsorpVisibleInput( AbsorpVisibleInput ),
			AbsorpVisibleEMSOverrideOn( AbsorpVisibleEMSOverrideOn ),
			AbsorpVisibleEMSOverride( AbsorpVisibleEMSOverride ),
			Trans( Trans ),
			TransVis( TransVis ),
			GlassTransDirtFactor( GlassTransDirtFactor ),
			SolarDiffusing( SolarDiffusing ),
			ReflectShade( ReflectShade ),
			ReflectShadeVis( ReflectShadeVis ),
			AbsorpThermalBack( AbsorpThermalBack ),
			AbsorpThermalFront( AbsorpThermalFront ),
			ReflectSolBeamBack( ReflectSolBeamBack ),
			ReflectSolBeamFront( ReflectSolBeamFront ),
			ReflectSolDiffBack( ReflectSolDiffBack ),
			ReflectSolDiffFront( ReflectSolDiffFront ),
			ReflectVisBeamBack( ReflectVisBeamBack ),
			ReflectVisBeamFront( ReflectVisBeamFront ),
			ReflectVisDiffBack( ReflectVisDiffBack ),
			ReflectVisDiffFront( ReflectVisDiffFront ),
			ReflectanceModeling( ReflectanceModeling ),
			TransSolBeam( TransSolBeam ),
			TransThermal( TransThermal ),
			TransVisBeam( TransVisBeam ),
			BlindDataPtr( BlindDataPtr ),
			ScreenDataPtr( ScreenDataPtr ),
			ScreenMapResolution( ScreenMapResolution ),
			YoungModulus( YoungModulus ),
			PoissonsRatio( PoissonsRatio ),
			DeflectedThickness( DeflectedThickness ),
			Pressure( Pressure ),
			SupportPillarPtr( SupportPillarPtr ),
			DeflectionStatePtr( DeflectionStatePtr ),
			ComplexShadePtr( ComplexShadePtr ),
			GasPointer( GasPointer ),
			WinShadeToGlassDist( WinShadeToGlassDist ),
			WinShadeTopOpeningMult( WinShadeTopOpeningMult ),
			WinShadeBottomOpeningMult( WinShadeBottomOpeningMult ),
			WinShadeLeftOpeningMult( WinShadeLeftOpeningMult ),
			WinShadeRightOpeningMult( WinShadeRightOpeningMult ),
			WinShadeAirFlowPermeability( WinShadeAirFlowPermeability ),
			EMPDMaterialProps( EMPDMaterialProps ),
			EMPDVALUE( EMPDVALUE ),
			MoistACoeff( MoistACoeff ),
			MoistBCoeff( MoistBCoeff ),
			MoistCCoeff( MoistCCoeff ),
			MoistDCoeff( MoistDCoeff ),
			EMPDaCoeff( EMPDaCoeff ),
			EMPDbCoeff( EMPDbCoeff ),
			EMPDcCoeff( EMPDcCoeff ),
			EMPDdCoeff( EMPDdCoeff ),
			EcoRoofCalculationMethod( EcoRoofCalculationMethod ),
			HeightOfPlants( HeightOfPlants ),
			LAI( LAI ),
			Lreflectivity( Lreflectivity ),
			LEmissitivity( LEmissitivity ),
			InitMoisture( InitMoisture ),
			MinMoisture( MinMoisture ),
			RStomata( RStomata ),
			niso( niso ),
			isodata( 27, isodata ),
			isorh( 27, isorh ),
			nsuc( nsuc ),
			sucdata( 27, sucdata ),
			sucwater( 27, sucwater ),
			nred( nred ),
			reddata( 27, reddata ),
			redwater( 27, redwater ),
			nmu( nmu ),
			mudata( 27, mudata ),
			murh( 27, murh ),
			ntc( ntc ),
			tcdata( 27, tcdata ),
			tcwater( 27, tcwater ),
			itemp( itemp ),
			irh( irh ),
			iwater( iwater ),
			divs( divs ),
			divsize( divsize ),
			divmin( divmin ),
			divmax( divmax ),
			SpecTemp( SpecTemp ),
			TCParent( TCParent ),
			SimpleWindowUfactor( SimpleWindowUfactor ),
			SimpleWindowSHGC( SimpleWindowSHGC ),
			SimpleWindowVisTran( SimpleWindowVisTran ),
			SimpleWindowVTinputByUser( SimpleWindowVTinputByUser ),
			WarnedForHighDiffusivity( WarnedForHighDiffusivity ),
			ReflFrontBeamBeam( ReflFrontBeamBeam ),
			ReflBackBeamBeam( ReflBackBeamBeam ),
			TausFrontBeamBeam( TausFrontBeamBeam ),
			TausBackBeamBeam( TausBackBeamBeam ),
			ReflFrontBeamBeamVis( ReflFrontBeamBeamVis ),
			ReflBackBeamBeamVis( ReflBackBeamBeamVis ),
			TausFrontBeamBeamVis( TausFrontBeamBeamVis ),
			TausBackBeamBeamVis( TausBackBeamBeamVis ),
			ReflFrontBeamDiff( ReflFrontBeamDiff ),
			ReflBackBeamDiff( ReflBackBeamDiff ),
			TausFrontBeamDiff( TausFrontBeamDiff ),
			TausBackBeamDiff( TausBackBeamDiff ),
			ReflFrontBeamDiffVis( ReflFrontBeamDiffVis ),
			ReflBackBeamDiffVis( ReflBackBeamDiffVis ),
			TausFrontBeamDiffVis( TausFrontBeamDiffVis ),
			TausBackBeamDiffVis( TausBackBeamDiffVis ),
			ReflFrontDiffDiff( ReflFrontDiffDiff ),
			ReflBackDiffDiff( ReflBackDiffDiff ),
			TausDiffDiff( TausDiffDiff ),
			ReflFrontDiffDiffVis( ReflFrontDiffDiffVis ),
			ReflBackDiffDiffVis( ReflBackDiffDiffVis ),
			TausDiffDiffVis( TausDiffDiffVis ),
			EmissThermalFront( EmissThermalFront ),
			EmissThermalBack( EmissThermalBack ),
			TausThermal( TausThermal ),
			GapVentType( GapVentType ),
			ISPleatedDrape( ISPleatedDrape ),
			PleatedDrapeWidth( PleatedDrapeWidth ),
			PleatedDrapeLength( PleatedDrapeLength ),
			ScreenWireSpacing( ScreenWireSpacing ),
			ScreenWireDiameter( ScreenWireDiameter ),
			SlatWidth( SlatWidth ),
			SlatSeparation( SlatSeparation ),
			SlatCrown( SlatCrown ),
			SlatAngle( SlatAngle ),
			SlatAngleType( SlatAngleType ),
			SlatOrientation( SlatOrientation ),
			GasName( GasName )
		{}

	};

	struct TCGlazingsType
	{
		// Members
		std::string Name; // Name
		int NumGlzMat; // Number of TC glazing materials
		Array1D_int LayerPoint; // Layer pointer
		Array1D< Real64 > SpecTemp; // Temperature corresponding to the specified TC glaing optical data
		Array1D_string LayerName; // Name of the referenced WindowMaterial:Glazing object

		// Default Constructor
		TCGlazingsType() :
			NumGlzMat( 0 )
		{}

		// Member Constructor
		TCGlazingsType(
			std::string const & Name, // Name
			int const NumGlzMat, // Number of TC glazing materials
			Array1_int const & LayerPoint, // Layer pointer
			Array1< Real64 > const & SpecTemp, // Temperature corresponding to the specified TC glaing optical data
			Array1_string const & LayerName // Name of the referenced WindowMaterial:Glazing object
		) :
			Name( Name ),
			NumGlzMat( NumGlzMat ),
			LayerPoint( LayerPoint ),
			SpecTemp( SpecTemp ),
			LayerName( LayerName )
		{}

	};

	struct ConstructionData
	{
		// Members
		std::string Name; // Name of construction
		int TotLayers; // Total number of layers for the construction; for windows
		//  this is the total of the glass, gas and shade layers
		int TotSolidLayers; // Total number of solid (glass or shade) layers (windows only)
		int TotGlassLayers; // Total number of glass layers (windows only)
		Array1D_int LayerPoint; // Pointer array which refers back to
		// the Material structure; LayerPoint(i)=j->Material(j)%Name,etc
		bool IsUsed; // Marked true when the construction is used
		Real64 InsideAbsorpVis; // Inside Layer visible absorptance of an opaque surface; not used for windows.
		Real64 OutsideAbsorpVis; // Outside Layer visible absorptance of an opaque surface; not used for windows.
		Real64 InsideAbsorpSolar; // Inside Layer solar absorptance of an opaque surface; not used for windows.
		Real64 OutsideAbsorpSolar; // Outside Layer solar absorptance of an opaque surface; not used for windows.
		Real64 InsideAbsorpThermal; // Inside Layer Thermal absorptance for opaque surfaces or windows;
		// for windows, applies to innermost glass layer
		Real64 OutsideAbsorpThermal; // Outside Layer Thermal absorptance
		int OutsideRoughness; // Outside Surface roughness index (6=very smooth, 5=smooth,
		// 4=medium smooth, 3=medium rough, 2=rough, 1=very rough)
		int DayltPropPtr; // Pointer to Daylight Construction Properties
		int W5FrameDivider; // FrameDivider number for window construction from Window5 data file;
		//  zero is construction not from Window5 file or Window5 construction has no frame.
		// Conductive properties for the construction
		Array1D< Real64 > CTFCross; // Cross or Y terms of the CTF equation
		Array1D< Real64 > CTFFlux; // Flux history terms of the CTF equation
		Array1D< Real64 > CTFInside; // Inside or Z terms of the CTF equation
		Array1D< Real64 > CTFOutside; // Outside or X terms of the CTF equation
		Array1D< Real64 > CTFSourceIn; // Heat source/sink inside terms of CTF equation
		Array1D< Real64 > CTFSourceOut; // Heat source/sink outside terms of CTF equation
		Real64 CTFTimeStep; // Time increment for stable simulation of construct (could be greater than TimeStep)
		// The next three series of terms are used to calculate the temperature at the location of a source/sink
		// in the QTF formulation.  This calculation is necessary to allow the proper simulation of a
		// radiant system.
		Array1D< Real64 > CTFTSourceOut; // Outside terms of the CTF equation for interior temp
		// calc@source location
		Array1D< Real64 > CTFTSourceIn; // Inside terms of the CTF equation for interior temp
		// calc@source location
		Array1D< Real64 > CTFTSourceQ; // Source/sink terms of the CTF equation for interior temp
		// calc@source location
		// The next three series of terms are used to calculate the temperature at a location specified by the user.
		// This location must be between two layers and is intended to allow the user to evaluate whether or not
		// condensation is a possibility between material layers.
		Array1D< Real64 > CTFTUserOut; // Outside terms of the CTF equation for interior temp
		// calc@user location
		Array1D< Real64 > CTFTUserIn; // Inside terms of the CTF equation for interior temp
		// calc@user location
		Array1D< Real64 > CTFTUserSource; // Source/sink terms of the CTF equation for interior temp
		// calc@user location
		int NumHistories; // CTFTimeStep/TimeStepZone or the number of temp/flux history series
		// for the construction
		int NumCTFTerms; // Number of CTF terms for this construction (not including terms at current time)
		Real64 UValue; // Overall heat transfer coefficient for the construction
		int SolutionDimensions; // Number of dimensions in the solution (1 for normal constructions,
		// 1 or 2 for constructions with sources or sinks)-->may allow 3-D later?
		int SourceAfterLayer; // Source/sink is present after this layer in the construction
		int TempAfterLayer; // User is requesting a temperature calculation after this layer in the construction
		Real64 ThicknessPerpend; // Thickness between planes of symmetry in the direction
		// perpendicular to the main direction of heat transfer
		// (same as half the distance between tubes)
		// Moisture Transfer Functions term belong here as well
		// BLAST detailed solar model parameters
		Real64 AbsDiffIn; // Inner absorptance coefficient for diffuse radiation
		Real64 AbsDiffOut; // Outer absorptance coefficient for diffuse radiation
		// Variables for window constructions
		Array1D< Real64 > AbsDiff; // Diffuse solar absorptance for each glass layer,
		// bare glass or shade on
		Array2D< Real64 > BlAbsDiff; // Diffuse solar absorptance for each glass layer vs.
		// slat angle, blind on
		Array2D< Real64 > BlAbsDiffGnd; // Diffuse ground solar absorptance for each glass layer
		// vs. slat angle, blind on
		Array2D< Real64 > BlAbsDiffSky; // Diffuse sky solar absorptance for each glass layer
		// vs. slat angle, blind on
		Array1D< Real64 > AbsDiffBack; // Diffuse back solar absorptance for each glass layer
		Array2D< Real64 > BlAbsDiffBack; // Diffuse back solar absorptance for each glass layer,
		//  vs. slat angle, blind on
		Real64 AbsDiffShade; // Diffuse solar absorptance for shade
		Array1D< Real64 > AbsDiffBlind; // Diffuse solar absorptance for blind, vs. slat angle
		Array1D< Real64 > AbsDiffBlindGnd; // Diffuse ground solar absorptance for blind, vs. slat angle
		Array1D< Real64 > AbsDiffBlindSky; // Diffuse sky solar absorptance for blind, vs. slat angle
		Real64 AbsDiffBackShade; // Diffuse back solar absorptance for shade
		Array1D< Real64 > AbsDiffBackBlind; // Diffuse back solar absorptance for blind, vs. slat angle
		Real64 ShadeAbsorpThermal; // Diffuse back thermal absorptance of shade
		Array2D< Real64 > AbsBeamCoef; // Coefficients of incidence-angle polynomial for solar
		// absorptance for each solid glazing layer
		Array2D< Real64 > AbsBeamBackCoef; // As for AbsBeamCoef but for back-incident solar
		Array1D< Real64 > AbsBeamShadeCoef; // Coefficients of incidence-angle polynomial for solar
		// absorptance of shade
		Real64 TransDiff; // Diffuse solar transmittance, bare glass or shade on
		Array1D< Real64 > BlTransDiff; // Diffuse solar transmittance, blind present, vs. slat angle
		Array1D< Real64 > BlTransDiffGnd; // Ground diffuse solar transmittance, blind present, vs. slat angle
		Array1D< Real64 > BlTransDiffSky; // Sky diffuse solar transmittance, blind present, vs. slat angle
		Real64 TransDiffVis; // Diffuse visible transmittance, bare glass or shade on
		Array1D< Real64 > BlTransDiffVis; // Diffuse visible transmittance, blind present, vs. slat angle
		Real64 ReflectSolDiffBack; // Diffuse back solar reflectance, bare glass or shade on
		Array1D< Real64 > BlReflectSolDiffBack; // Diffuse back solar reflectance, blind present, vs. slat angle
		Real64 ReflectSolDiffFront; // Diffuse front solar reflectance, bare glass or shade on
		Array1D< Real64 > BlReflectSolDiffFront; // Diffuse front solar reflectance, blind present, vs. slat angle
		Real64 ReflectVisDiffBack; // Diffuse back visible reflectance, bare glass or shade on
		Array1D< Real64 > BlReflectVisDiffBack; // Diffuse back visible reflectance, blind present, vs. slat angle
		Real64 ReflectVisDiffFront; // Diffuse front visible reflectance, bare glass or shade on
		Array1D< Real64 > BlReflectVisDiffFront; // Diffuse front visible reflectance, blind present, vs. slat angle
		Array1D< Real64 > TransSolBeamCoef; // Coeffs of incidence-angle polynomial for beam sol trans,
		// bare glass or shade on
		Array1D< Real64 > TransVisBeamCoef; // Coeffs of incidence-angle polynomial for beam vis trans,
		// bare glass or shade on
		Array1D< Real64 > ReflSolBeamFrontCoef; // Coeffs of incidence-angle polynomial for beam sol front refl,
		// bare glass or shade on
		Array1D< Real64 > ReflSolBeamBackCoef; // Like ReflSolBeamFrontCoef, but for back-incident beam solar
		Array2D< Real64 > tBareSolCoef; // Isolated glass solar transmittance coeffs of inc. angle polynomial
		Array2D< Real64 > tBareVisCoef; // Isolated glass visible transmittance coeffs of inc. angle polynomial
		Array2D< Real64 > rfBareSolCoef; // Isolated glass front solar reflectance coeffs of inc. angle polynomial
		Array2D< Real64 > rfBareVisCoef; // Isolated glass front visible reflectance coeffs of inc. angle polynomial
		Array2D< Real64 > rbBareSolCoef; // Isolated glass back solar reflectance coeffs of inc. angle polynomial
		Array2D< Real64 > rbBareVisCoef; // Isolated glass back visible reflectance coeffs of inc. angle polynomial
		Array2D< Real64 > afBareSolCoef; // Isolated glass front solar absorptance coeffs of inc. angle polynomial
		Array2D< Real64 > abBareSolCoef; // Isolated glass back solar absorptance coeffs of inc. angle polynomial
		Array1D< Real64 > tBareSolDiff; // Isolated glass diffuse solar transmittance
		Array1D< Real64 > tBareVisDiff; // Isolated glass diffuse visible transmittance
		Array1D< Real64 > rfBareSolDiff; // Isolated glass diffuse solar front reflectance
		Array1D< Real64 > rfBareVisDiff; // Isolated glass diffuse visible front reflectance
		Array1D< Real64 > rbBareSolDiff; // Isolated glass diffuse solar back reflectance
		Array1D< Real64 > rbBareVisDiff; // Isolated glass diffuse visible back reflectance
		Array1D< Real64 > afBareSolDiff; // Isolated glass diffuse solar front absorptance
		Array1D< Real64 > abBareSolDiff; // Isolated glass diffuse solar back absorptance
		bool FromWindow5DataFile; // True if this is a window construction extracted from the Window5 data file
		Real64 W5FileMullionWidth; // Width of mullion for construction from Window5 data file (m)
		int W5FileMullionOrientation; // Orientation of mullion, if present, for Window5 data file construction,
		Real64 W5FileGlazingSysWidth; // Glass width for construction from Window5 data file (m)
		Real64 W5FileGlazingSysHeight; // Glass height for construction form Window5 data file (m)
		Real64 SummerSHGC; // Calculated ASHRAE SHGC for summer conditions
		Real64 VisTransNorm; // The normal visible transmittance
		Real64 SolTransNorm; // the normal solar transmittance
		bool SourceSinkPresent; // .TRUE. if there is a source/sink within this construction
		bool TypeIsWindow; // True if a window construction, false otherwise
		bool WindowTypeBSDF; // True for complex window, false otherwise
		bool TypeIsEcoRoof; // -- true for construction with ecoRoof outside, the flag
		//-- is turned on when the outside layer is of type EcoRoof
		bool TypeIsIRT; // -- true for construction with IRT material
		bool TypeIsCfactorWall; // -- true for construction with Construction:CfactorUndergroundWall
		bool TypeIsFfactorFloor; // -- true for construction with Construction:FfactorGroundFloor
		// Added TH 12/22/2008 for thermochromic windows
		int TCFlag; // 0: this construction is not a thermochromic window construction
		// 1: it is a TC window construction
		int TCLayer; // Reference to the TC glazing material layer in the Material array
		int TCMasterConst; // The master TC construction referenced by its slave constructions
		int TCLayerID; // Which material layer is the TC glazing, counting all material layers.
		int TCGlassID; // Which glass layer is the TC glazing, counting from glass layers only.
		//For CFactor underground walls
		Real64 CFactor;
		Real64 Height;
		//For FFactor slabs-on-grade or undeerground floors
		Real64 FFactor;
		Real64 Area;
		Real64 PerimeterExposed;
		bool ReverseConstructionNumLayersWarning;
		bool ReverseConstructionLayersOrderWarning;
		//Complex Fenestration
		BSDFWindowInputStruct BSDFInput; // nest structure with user input for complex fenestration
		// EquivalentLayer Window
		bool WindowTypeEQL; // True for equivalent layer window, false otherwise
		int EQLConsPtr; // Pointer to equivalent Layer window construction
		Array1D< Real64 > AbsDiffFrontEQL; // Diffuse layer system front absorptance for EQL window
		Array1D< Real64 > AbsDiffBackEQL; // Diffuse layer system back absorptance for EQL window
		Real64 TransDiffFrontEQL; // Diffuse system front transmittance for EQL window
		Real64 TransDiffBackEQL; // Diffuse system back transmittance for EQL window

		// Default Constructor
		ConstructionData() :
			TotLayers( 0 ),
			TotSolidLayers( 0 ),
			TotGlassLayers( 0 ),
			LayerPoint( MaxLayersInConstruct, 0 ),
			IsUsed( false ),
			InsideAbsorpVis( 0.0 ),
			OutsideAbsorpVis( 0.0 ),
			InsideAbsorpSolar( 0.0 ),
			OutsideAbsorpSolar( 0.0 ),
			InsideAbsorpThermal( 0.0 ),
			OutsideAbsorpThermal( 0.0 ),
			OutsideRoughness( 0 ),
			DayltPropPtr( 0 ),
			W5FrameDivider( 0 ),
			CTFCross( {0,MaxCTFTerms-1}, 0.0 ),
			CTFFlux( MaxCTFTerms-1, 0.0 ),
			CTFInside( {0,MaxCTFTerms-1}, 0.0 ),
			CTFOutside( {0,MaxCTFTerms-1}, 0.0 ),
			CTFSourceIn( {0,MaxCTFTerms-1}, 0.0 ),
			CTFSourceOut( {0,MaxCTFTerms-1}, 0.0 ),
			CTFTSourceOut( {0,MaxCTFTerms-1}, 0.0 ),
			CTFTSourceIn( {0,MaxCTFTerms-1}, 0.0 ),
			CTFTSourceQ( {0,MaxCTFTerms-1}, 0.0 ),
			CTFTUserOut( {0,MaxCTFTerms-1}, 0.0 ),
			CTFTUserIn( {0,MaxCTFTerms-1}, 0.0 ),
			CTFTUserSource( {0,MaxCTFTerms-1}, 0.0 ),
			NumHistories( 0 ),
			NumCTFTerms( 0 ),
			UValue( 0.0 ),
			SolutionDimensions( 0 ),
			SourceAfterLayer( 0 ),
			TempAfterLayer( 0 ),
			ThicknessPerpend( 0.0 ),
			AbsDiffIn( 0.0 ),
			AbsDiffOut( 0.0 ),
			AbsDiff( MaxSolidWinLayers, 0.0 ),
			BlAbsDiff( MaxSlatAngs, MaxSolidWinLayers, 0.0 ),
			BlAbsDiffGnd( MaxSlatAngs, MaxSolidWinLayers, 0.0 ),
			BlAbsDiffSky( MaxSlatAngs, MaxSolidWinLayers, 0.0 ),
			AbsDiffBack( MaxSolidWinLayers, 0.0 ),
			BlAbsDiffBack( MaxSlatAngs, MaxSolidWinLayers, 0.0 ),
			AbsDiffShade( 0.0 ),
			AbsDiffBlind( MaxSlatAngs, 0.0 ),
			AbsDiffBlindGnd( MaxSlatAngs, 0.0 ),
			AbsDiffBlindSky( MaxSlatAngs, 0.0 ),
			AbsDiffBackShade( 0.0 ),
			AbsDiffBackBlind( MaxSlatAngs, 0.0 ),
			ShadeAbsorpThermal( 0.0 ),
			AbsBeamCoef( 6, MaxSolidWinLayers, 0.0 ),
			AbsBeamBackCoef( 6, MaxSolidWinLayers, 0.0 ),
			AbsBeamShadeCoef( 6, 0.0 ),
			TransDiff( 0.0 ),
			BlTransDiff( MaxSlatAngs, 0.0 ),
			BlTransDiffGnd( MaxSlatAngs, 0.0 ),
			BlTransDiffSky( MaxSlatAngs, 0.0 ),
			TransDiffVis( 0.0 ),
			BlTransDiffVis( MaxSlatAngs, 0.0 ),
			ReflectSolDiffBack( 0.0 ),
			BlReflectSolDiffBack( MaxSlatAngs, 0.0 ),
			ReflectSolDiffFront( 0.0 ),
			BlReflectSolDiffFront( MaxSlatAngs, 0.0 ),
			ReflectVisDiffBack( 0.0 ),
			BlReflectVisDiffBack( MaxSlatAngs, 0.0 ),
			ReflectVisDiffFront( 0.0 ),
			BlReflectVisDiffFront( MaxSlatAngs, 0.0 ),
			TransSolBeamCoef( 6, 0.0 ),
			TransVisBeamCoef( 6, 0.0 ),
			ReflSolBeamFrontCoef( 6, 0.0 ),
			ReflSolBeamBackCoef( 6, 0.0 ),
			tBareSolCoef( 6, 5, 0.0 ),
			tBareVisCoef( 6, 5, 0.0 ),
			rfBareSolCoef( 6, 5, 0.0 ),
			rfBareVisCoef( 6, 5, 0.0 ),
			rbBareSolCoef( 6, 5, 0.0 ),
			rbBareVisCoef( 6, 5, 0.0 ),
			afBareSolCoef( 6, 5, 0.0 ),
			abBareSolCoef( 6, 5, 0.0 ),
			tBareSolDiff( 5, 0.0 ),
			tBareVisDiff( 5, 0.0 ),
			rfBareSolDiff( 5, 0.0 ),
			rfBareVisDiff( 5, 0.0 ),
			rbBareSolDiff( 5, 0.0 ),
			rbBareVisDiff( 5, 0.0 ),
			afBareSolDiff( 5, 0.0 ),
			abBareSolDiff( 5, 0.0 ),
			FromWindow5DataFile( false ),
			W5FileMullionWidth( 0.0 ),
			W5FileMullionOrientation( 0 ),
			W5FileGlazingSysWidth( 0.0 ),
			W5FileGlazingSysHeight( 0.0 ),
			SummerSHGC( 0.0 ),
			VisTransNorm( 0.0 ),
			SolTransNorm( 0.0 ),
			SourceSinkPresent( false ),
			TypeIsWindow( false ),
			WindowTypeBSDF( false ),
			TypeIsEcoRoof( false ),
			TypeIsIRT( false ),
			TypeIsCfactorWall( false ),
			TypeIsFfactorFloor( false ),
			TCFlag( 0 ),
			TCLayer( 0 ),
			TCMasterConst( 0 ),
			TCLayerID( 0 ),
			TCGlassID( 0 ),
			CFactor( 0.0 ),
			Height( 0.0 ),
			FFactor( 0.0 ),
			Area( 0.0 ),
			PerimeterExposed( 0.0 ),
			ReverseConstructionNumLayersWarning( false ),
			ReverseConstructionLayersOrderWarning( false ),
			WindowTypeEQL( false ),
			EQLConsPtr( 0 ),
			AbsDiffFrontEQL( CFSMAXNL, 0.0 ),
			AbsDiffBackEQL( CFSMAXNL, 0.0 ),
			TransDiffFrontEQL( 0.0 ),
			TransDiffBackEQL( 0.0 )
		{}

		// Member Constructor
		ConstructionData(
			std::string const & Name, // Name of construction
			int const TotLayers, // Total number of layers for the construction; for windows
			int const TotSolidLayers, // Total number of solid (glass or shade) layers (windows only)
			int const TotGlassLayers, // Total number of glass layers (windows only)
			Array1_int const & LayerPoint, // Pointer array which refers back to
			bool const IsUsed, // Marked true when the construction is used
			Real64 const InsideAbsorpVis, // Inside Layer visible absorptance of an opaque surface; not used for windows.
			Real64 const OutsideAbsorpVis, // Outside Layer visible absorptance of an opaque surface; not used for windows.
			Real64 const InsideAbsorpSolar, // Inside Layer solar absorptance of an opaque surface; not used for windows.
			Real64 const OutsideAbsorpSolar, // Outside Layer solar absorptance of an opaque surface; not used for windows.
			Real64 const InsideAbsorpThermal, // Inside Layer Thermal absorptance for opaque surfaces or windows;
			Real64 const OutsideAbsorpThermal, // Outside Layer Thermal absorptance
			int const OutsideRoughness, // Outside Surface roughness index (6=very smooth, 5=smooth,
			int const DayltPropPtr, // Pointer to Daylight Construction Properties
			int const W5FrameDivider, // FrameDivider number for window construction from Window5 data file;
			Array1< Real64 > const & CTFCross, // Cross or Y terms of the CTF equation
			Array1< Real64 > const & CTFFlux, // Flux history terms of the CTF equation
			Array1< Real64 > const & CTFInside, // Inside or Z terms of the CTF equation
			Array1< Real64 > const & CTFOutside, // Outside or X terms of the CTF equation
			Array1< Real64 > const & CTFSourceIn, // Heat source/sink inside terms of CTF equation
			Array1< Real64 > const & CTFSourceOut, // Heat source/sink outside terms of CTF equation
			Real64 const CTFTimeStep, // Time increment for stable simulation of construct (could be greater than TimeStep)
			Array1< Real64 > const & CTFTSourceOut, // Outside terms of the CTF equation for interior temp
			Array1< Real64 > const & CTFTSourceIn, // Inside terms of the CTF equation for interior temp
			Array1< Real64 > const & CTFTSourceQ, // Source/sink terms of the CTF equation for interior temp
			Array1< Real64 > const & CTFTUserOut, // Outside terms of the CTF equation for interior temp
			Array1< Real64 > const & CTFTUserIn, // Inside terms of the CTF equation for interior temp
			Array1< Real64 > const & CTFTUserSource, // Source/sink terms of the CTF equation for interior temp
			int const NumHistories, // CTFTimeStep/TimeStepZone or the number of temp/flux history series
			int const NumCTFTerms, // Number of CTF terms for this construction (not including terms at current time)
			Real64 const UValue, // Overall heat transfer coefficient for the construction
			int const SolutionDimensions, // Number of dimensions in the solution (1 for normal constructions,
			int const SourceAfterLayer, // Source/sink is present after this layer in the construction
			int const TempAfterLayer, // User is requesting a temperature calculation after this layer in the construction
			Real64 const ThicknessPerpend, // Thickness between planes of symmetry in the direction
			Real64 const AbsDiffIn, // Inner absorptance coefficient for diffuse radiation
			Real64 const AbsDiffOut, // Outer absorptance coefficient for diffuse radiation
			Array1< Real64 > const & AbsDiff, // Diffuse solar absorptance for each glass layer,
			Array2< Real64 > const & BlAbsDiff, // Diffuse solar absorptance for each glass layer vs.
			Array2< Real64 > const & BlAbsDiffGnd, // Diffuse ground solar absorptance for each glass layer
			Array2< Real64 > const & BlAbsDiffSky, // Diffuse sky solar absorptance for each glass layer
			Array1< Real64 > const & AbsDiffBack, // Diffuse back solar absorptance for each glass layer
			Array2< Real64 > const & BlAbsDiffBack, // Diffuse back solar absorptance for each glass layer,
			Real64 const AbsDiffShade, // Diffuse solar absorptance for shade
			Array1< Real64 > const & AbsDiffBlind, // Diffuse solar absorptance for blind, vs. slat angle
			Array1< Real64 > const & AbsDiffBlindGnd, // Diffuse ground solar absorptance for blind, vs. slat angle
			Array1< Real64 > const & AbsDiffBlindSky, // Diffuse sky solar absorptance for blind, vs. slat angle
			Real64 const AbsDiffBackShade, // Diffuse back solar absorptance for shade
			Array1< Real64 > const & AbsDiffBackBlind, // Diffuse back solar absorptance for blind, vs. slat angle
			Real64 const ShadeAbsorpThermal, // Diffuse back thermal absorptance of shade
			Array2< Real64 > const & AbsBeamCoef, // Coefficients of incidence-angle polynomial for solar
			Array2< Real64 > const & AbsBeamBackCoef, // As for AbsBeamCoef but for back-incident solar
			Array1< Real64 > const & AbsBeamShadeCoef, // Coefficients of incidence-angle polynomial for solar
			Real64 const TransDiff, // Diffuse solar transmittance, bare glass or shade on
			Array1< Real64 > const & BlTransDiff, // Diffuse solar transmittance, blind present, vs. slat angle
			Array1< Real64 > const & BlTransDiffGnd, // Ground diffuse solar transmittance, blind present, vs. slat angle
			Array1< Real64 > const & BlTransDiffSky, // Sky diffuse solar transmittance, blind present, vs. slat angle
			Real64 const TransDiffVis, // Diffuse visible transmittance, bare glass or shade on
			Array1< Real64 > const & BlTransDiffVis, // Diffuse visible transmittance, blind present, vs. slat angle
			Real64 const ReflectSolDiffBack, // Diffuse back solar reflectance, bare glass or shade on
			Array1< Real64 > const & BlReflectSolDiffBack, // Diffuse back solar reflectance, blind present, vs. slat angle
			Real64 const ReflectSolDiffFront, // Diffuse front solar reflectance, bare glass or shade on
			Array1< Real64 > const & BlReflectSolDiffFront, // Diffuse front solar reflectance, blind present, vs. slat angle
			Real64 const ReflectVisDiffBack, // Diffuse back visible reflectance, bare glass or shade on
			Array1< Real64 > const & BlReflectVisDiffBack, // Diffuse back visible reflectance, blind present, vs. slat angle
			Real64 const ReflectVisDiffFront, // Diffuse front visible reflectance, bare glass or shade on
			Array1< Real64 > const & BlReflectVisDiffFront, // Diffuse front visible reflectance, blind present, vs. slat angle
			Array1< Real64 > const & TransSolBeamCoef, // Coeffs of incidence-angle polynomial for beam sol trans,
			Array1< Real64 > const & TransVisBeamCoef, // Coeffs of incidence-angle polynomial for beam vis trans,
			Array1< Real64 > const & ReflSolBeamFrontCoef, // Coeffs of incidence-angle polynomial for beam sol front refl,
			Array1< Real64 > const & ReflSolBeamBackCoef, // Like ReflSolBeamFrontCoef, but for back-incident beam solar
			Array2< Real64 > const & tBareSolCoef, // Isolated glass solar transmittance coeffs of inc. angle polynomial
			Array2< Real64 > const & tBareVisCoef, // Isolated glass visible transmittance coeffs of inc. angle polynomial
			Array2< Real64 > const & rfBareSolCoef, // Isolated glass front solar reflectance coeffs of inc. angle polynomial
			Array2< Real64 > const & rfBareVisCoef, // Isolated glass front visible reflectance coeffs of inc. angle polynomial
			Array2< Real64 > const & rbBareSolCoef, // Isolated glass back solar reflectance coeffs of inc. angle polynomial
			Array2< Real64 > const & rbBareVisCoef, // Isolated glass back visible reflectance coeffs of inc. angle polynomial
			Array2< Real64 > const & afBareSolCoef, // Isolated glass front solar absorptance coeffs of inc. angle polynomial
			Array2< Real64 > const & abBareSolCoef, // Isolated glass back solar absorptance coeffs of inc. angle polynomial
			Array1< Real64 > const & tBareSolDiff, // Isolated glass diffuse solar transmittance
			Array1< Real64 > const & tBareVisDiff, // Isolated glass diffuse visible transmittance
			Array1< Real64 > const & rfBareSolDiff, // Isolated glass diffuse solar front reflectance
			Array1< Real64 > const & rfBareVisDiff, // Isolated glass diffuse visible front reflectance
			Array1< Real64 > const & rbBareSolDiff, // Isolated glass diffuse solar back reflectance
			Array1< Real64 > const & rbBareVisDiff, // Isolated glass diffuse visible back reflectance
			Array1< Real64 > const & afBareSolDiff, // Isolated glass diffuse solar front absorptance
			Array1< Real64 > const & abBareSolDiff, // Isolated glass diffuse solar back absorptance
			bool const FromWindow5DataFile, // True if this is a window construction extracted from the Window5 data file
			Real64 const W5FileMullionWidth, // Width of mullion for construction from Window5 data file (m)
			int const W5FileMullionOrientation, // Orientation of mullion, if present, for Window5 data file construction,
			Real64 const W5FileGlazingSysWidth, // Glass width for construction from Window5 data file (m)
			Real64 const W5FileGlazingSysHeight, // Glass height for construction form Window5 data file (m)
			Real64 const SummerSHGC, // Calculated ASHRAE SHGC for summer conditions
			Real64 const VisTransNorm, // The normal visible transmittance
			Real64 const SolTransNorm, // the normal solar transmittance
			bool const SourceSinkPresent, // .TRUE. if there is a source/sink within this construction
			bool const TypeIsWindow, // True if a window construction, false otherwise
			bool const WindowTypeBSDF, // True for complex window, false otherwise
			bool const TypeIsEcoRoof, // -- true for construction with ecoRoof outside, the flag
			bool const TypeIsIRT, // -- true for construction with IRT material
			bool const TypeIsCfactorWall, // -- true for construction with Construction:CfactorUndergroundWall
			bool const TypeIsFfactorFloor, // -- true for construction with Construction:FfactorGroundFloor
			int const TCFlag, // 0: this construction is not a thermochromic window construction
			int const TCLayer, // Reference to the TC glazing material layer in the Material array
			int const TCMasterConst, // The master TC construction referenced by its slave constructions
			int const TCLayerID, // Which material layer is the TC glazing, counting all material layers.
			int const TCGlassID, // Which glass layer is the TC glazing, counting from glass layers only.
			Real64 const CFactor,
			Real64 const Height,
			Real64 const FFactor,
			Real64 const Area,
			Real64 const PerimeterExposed,
			bool const ReverseConstructionNumLayersWarning,
			bool const ReverseConstructionLayersOrderWarning,
			BSDFWindowInputStruct const & BSDFInput, // nest structure with user input for complex fenestration
			bool const WindowTypeEQL, // True for equivalent layer window, false otherwise
			int const EQLConsPtr, // Pointer to equivalent Layer window construction
			Array1< Real64 > const & AbsDiffFrontEQL, // Diffuse layer system front absorptance for EQL window
			Array1< Real64 > const & AbsDiffBackEQL, // Diffuse layer system back absorptance for EQL window
			Real64 const TransDiffFrontEQL, // Diffuse system front transmittance for EQL window
			Real64 const TransDiffBackEQL // Diffuse system back transmittance for EQL window
		) :
			Name( Name ),
			TotLayers( TotLayers ),
			TotSolidLayers( TotSolidLayers ),
			TotGlassLayers( TotGlassLayers ),
			LayerPoint( MaxLayersInConstruct, LayerPoint ),
			IsUsed( IsUsed ),
			InsideAbsorpVis( InsideAbsorpVis ),
			OutsideAbsorpVis( OutsideAbsorpVis ),
			InsideAbsorpSolar( InsideAbsorpSolar ),
			OutsideAbsorpSolar( OutsideAbsorpSolar ),
			InsideAbsorpThermal( InsideAbsorpThermal ),
			OutsideAbsorpThermal( OutsideAbsorpThermal ),
			OutsideRoughness( OutsideRoughness ),
			DayltPropPtr( DayltPropPtr ),
			W5FrameDivider( W5FrameDivider ),
			CTFCross( {0,MaxCTFTerms-1}, CTFCross ),
			CTFFlux( MaxCTFTerms-1, CTFFlux ),
			CTFInside( {0,MaxCTFTerms-1}, CTFInside ),
			CTFOutside( {0,MaxCTFTerms-1}, CTFOutside ),
			CTFSourceIn( {0,MaxCTFTerms-1}, CTFSourceIn ),
			CTFSourceOut( {0,MaxCTFTerms-1}, CTFSourceOut ),
			CTFTimeStep( CTFTimeStep ),
			CTFTSourceOut( {0,MaxCTFTerms-1}, CTFTSourceOut ),
			CTFTSourceIn( {0,MaxCTFTerms-1}, CTFTSourceIn ),
			CTFTSourceQ( {0,MaxCTFTerms-1}, CTFTSourceQ ),
			CTFTUserOut( {0,MaxCTFTerms-1}, CTFTUserOut ),
			CTFTUserIn( {0,MaxCTFTerms-1}, CTFTUserIn ),
			CTFTUserSource( {0,MaxCTFTerms-1}, CTFTUserSource ),
			NumHistories( NumHistories ),
			NumCTFTerms( NumCTFTerms ),
			UValue( UValue ),
			SolutionDimensions( SolutionDimensions ),
			SourceAfterLayer( SourceAfterLayer ),
			TempAfterLayer( TempAfterLayer ),
			ThicknessPerpend( ThicknessPerpend ),
			AbsDiffIn( AbsDiffIn ),
			AbsDiffOut( AbsDiffOut ),
			AbsDiff( MaxSolidWinLayers, AbsDiff ),
			BlAbsDiff( MaxSlatAngs, MaxSolidWinLayers, BlAbsDiff ),
			BlAbsDiffGnd( MaxSlatAngs, MaxSolidWinLayers, BlAbsDiffGnd ),
			BlAbsDiffSky( MaxSlatAngs, MaxSolidWinLayers, BlAbsDiffSky ),
			AbsDiffBack( MaxSolidWinLayers, AbsDiffBack ),
			BlAbsDiffBack( MaxSlatAngs, MaxSolidWinLayers, BlAbsDiffBack ),
			AbsDiffShade( AbsDiffShade ),
			AbsDiffBlind( MaxSlatAngs, AbsDiffBlind ),
			AbsDiffBlindGnd( MaxSlatAngs, AbsDiffBlindGnd ),
			AbsDiffBlindSky( MaxSlatAngs, AbsDiffBlindSky ),
			AbsDiffBackShade( AbsDiffBackShade ),
			AbsDiffBackBlind( MaxSlatAngs, AbsDiffBackBlind ),
			ShadeAbsorpThermal( ShadeAbsorpThermal ),
			AbsBeamCoef( 6, MaxSolidWinLayers, AbsBeamCoef ),
			AbsBeamBackCoef( 6, MaxSolidWinLayers, AbsBeamBackCoef ),
			AbsBeamShadeCoef( 6, AbsBeamShadeCoef ),
			TransDiff( TransDiff ),
			BlTransDiff( MaxSlatAngs, BlTransDiff ),
			BlTransDiffGnd( MaxSlatAngs, BlTransDiffGnd ),
			BlTransDiffSky( MaxSlatAngs, BlTransDiffSky ),
			TransDiffVis( TransDiffVis ),
			BlTransDiffVis( MaxSlatAngs, BlTransDiffVis ),
			ReflectSolDiffBack( ReflectSolDiffBack ),
			BlReflectSolDiffBack( MaxSlatAngs, BlReflectSolDiffBack ),
			ReflectSolDiffFront( ReflectSolDiffFront ),
			BlReflectSolDiffFront( MaxSlatAngs, BlReflectSolDiffFront ),
			ReflectVisDiffBack( ReflectVisDiffBack ),
			BlReflectVisDiffBack( MaxSlatAngs, BlReflectVisDiffBack ),
			ReflectVisDiffFront( ReflectVisDiffFront ),
			BlReflectVisDiffFront( MaxSlatAngs, BlReflectVisDiffFront ),
			TransSolBeamCoef( 6, TransSolBeamCoef ),
			TransVisBeamCoef( 6, TransVisBeamCoef ),
			ReflSolBeamFrontCoef( 6, ReflSolBeamFrontCoef ),
			ReflSolBeamBackCoef( 6, ReflSolBeamBackCoef ),
			tBareSolCoef( 6, 5, tBareSolCoef ),
			tBareVisCoef( 6, 5, tBareVisCoef ),
			rfBareSolCoef( 6, 5, rfBareSolCoef ),
			rfBareVisCoef( 6, 5, rfBareVisCoef ),
			rbBareSolCoef( 6, 5, rbBareSolCoef ),
			rbBareVisCoef( 6, 5, rbBareVisCoef ),
			afBareSolCoef( 6, 5, afBareSolCoef ),
			abBareSolCoef( 6, 5, abBareSolCoef ),
			tBareSolDiff( 5, tBareSolDiff ),
			tBareVisDiff( 5, tBareVisDiff ),
			rfBareSolDiff( 5, rfBareSolDiff ),
			rfBareVisDiff( 5, rfBareVisDiff ),
			rbBareSolDiff( 5, rbBareSolDiff ),
			rbBareVisDiff( 5, rbBareVisDiff ),
			afBareSolDiff( 5, afBareSolDiff ),
			abBareSolDiff( 5, abBareSolDiff ),
			FromWindow5DataFile( FromWindow5DataFile ),
			W5FileMullionWidth( W5FileMullionWidth ),
			W5FileMullionOrientation( W5FileMullionOrientation ),
			W5FileGlazingSysWidth( W5FileGlazingSysWidth ),
			W5FileGlazingSysHeight( W5FileGlazingSysHeight ),
			SummerSHGC( SummerSHGC ),
			VisTransNorm( VisTransNorm ),
			SolTransNorm( SolTransNorm ),
			SourceSinkPresent( SourceSinkPresent ),
			TypeIsWindow( TypeIsWindow ),
			WindowTypeBSDF( WindowTypeBSDF ),
			TypeIsEcoRoof( TypeIsEcoRoof ),
			TypeIsIRT( TypeIsIRT ),
			TypeIsCfactorWall( TypeIsCfactorWall ),
			TypeIsFfactorFloor( TypeIsFfactorFloor ),
			TCFlag( TCFlag ),
			TCLayer( TCLayer ),
			TCMasterConst( TCMasterConst ),
			TCLayerID( TCLayerID ),
			TCGlassID( TCGlassID ),
			CFactor( CFactor ),
			Height( Height ),
			FFactor( FFactor ),
			Area( Area ),
			PerimeterExposed( PerimeterExposed ),
			ReverseConstructionNumLayersWarning( ReverseConstructionNumLayersWarning ),
			ReverseConstructionLayersOrderWarning( ReverseConstructionLayersOrderWarning ),
			BSDFInput( BSDFInput ),
			WindowTypeEQL( WindowTypeEQL ),
			EQLConsPtr( EQLConsPtr ),
			AbsDiffFrontEQL( CFSMAXNL, AbsDiffFrontEQL ),
			AbsDiffBackEQL( CFSMAXNL, AbsDiffBackEQL ),
			TransDiffFrontEQL( TransDiffFrontEQL ),
			TransDiffBackEQL( TransDiffBackEQL )
		{}

	};

	struct SpectralDataProperties
	{
		// Members
		std::string Name; // Name of spectral data set
		int NumOfWavelengths; // Number of wavelengths in the data set
		Array1D< Real64 > WaveLength; // Wavelength (microns)
		Array1D< Real64 > Trans; // Transmittance at normal incidence
		Array1D< Real64 > ReflFront; // Front reflectance at normal incidence
		Array1D< Real64 > ReflBack; // Back reflectance at normal incidence

		// Default Constructor
		SpectralDataProperties() :
			NumOfWavelengths( 0 )
		{}

		// Member Constructor
		SpectralDataProperties(
			std::string const & Name, // Name of spectral data set
			int const NumOfWavelengths, // Number of wavelengths in the data set
			Array1< Real64 > const & WaveLength, // Wavelength (microns)
			Array1< Real64 > const & Trans, // Transmittance at normal incidence
			Array1< Real64 > const & ReflFront, // Front reflectance at normal incidence
			Array1< Real64 > const & ReflBack // Back reflectance at normal incidence
		) :
			Name( Name ),
			NumOfWavelengths( NumOfWavelengths ),
			WaveLength( WaveLength ),
			Trans( Trans ),
			ReflFront( ReflFront ),
			ReflBack( ReflBack )
		{}

	};

	struct ZoneData
	{
		// Members
		std::string Name;
		int Multiplier; // Used in reporting and for systems calculations
		int ListMultiplier; // For Zone Group object:  used in reporting and systems calculations
		int ListGroup; // used only in Zone Group verification.  and for error message.
		Real64 RelNorth; // Relative North (to building north) [Degrees]
		Real64 OriginX; // X origin  [m]
		Real64 OriginY; // Y origin  [m]
		Real64 OriginZ; // Z origin  [m]
		Real64 CeilingHeight; // Ceiling Height entered by user [m] or calculated
		Real64 Volume; // Volume entered by user [m3] or calculated
		int OfType; // 1=Standard Zone, Not yet used:
		// 2=Plenum Zone, 11=Solar Wall, 12=Roof Pond
		Real64 UserEnteredFloorArea; // User input floor area for this zone
		// Calculated after input
		Real64 FloorArea; // Floor area used for this zone
		Real64 CalcFloorArea; // Calculated floor area used for this zone
		bool HasFloor; // Has "Floor" surface
		bool HasRoof; // Has "Roof" or "Ceiling" Surface
		bool HasInterZoneWindow; // Interzone Window(s) present in this zone
		bool HasWindow; // Window(s) present in this zone
		Real64 AirCapacity;
		Real64 ExtWindowArea; // Exterior Window Area for Zone
		Real64 ExtGrossWallArea; // Exterior Wall Area for Zone (Gross)
		Real64 ExtWindowArea_Multiplied; // Exterior Window Area for Zone with multipliers
		Real64 ExtGrossWallArea_Multiplied; // Exterior Wall Area for Zone (Gross) with multipliers
		Real64 ExtNetWallArea; // Exterior Wall Area for Zone (Net)
		Real64 TotalSurfArea; // Total surface area for Zone
		Real64 ExteriorTotalSurfArea; // Total surface area of all exterior surfaces for Zone
		// (ignoring windows as they will be included in their base surfaces)
		Real64 ExteriorTotalGroundSurfArea; // Total surface area of all surfaces for Zone with ground contact
		Real64 ExtGrossGroundWallArea; // Ground contact Wall Area for Zone (Gross)
		Real64 ExtGrossGroundWallArea_Multiplied; // Ground contact Wall Area for Zone (Gross) with multipliers
		int SystemZoneNodeNumber; // This is the zone node number for the system for a controlled zone
		bool IsControlled; // True when this is a controlled zone.
		int TempControlledZoneIndex; // this is the index number for TempControlledZone structure for lookup
		//            Pointers to Surface Data Structure
		int SurfaceFirst; // First Surface in Zone
		int SurfaceLast; // Last Surface in Zone
		int InsideConvectionAlgo; // Ref: appropriate values for Inside Convection solution
		int NumSurfaces; // Number of surfaces for this zone
		int NumSubSurfaces; // Number of subsurfaces for this zone (windows, doors, tdd dome and diffusers)
		int NumShadingSurfaces; // Number of shading surfaces for this zone
		int OutsideConvectionAlgo; // Ref: appropriate values for Outside Convection solution
		Vector Centroid; // Center of the zone found by averaging wall, floor, and roof centroids
		Real64 MinimumX; // Minimum X value for entire zone
		Real64 MaximumX; // Maximum X value for entire zone
		Real64 MinimumY; // Minimum Y value for entire zone
		Real64 MaximumY; // Maximum Y value for entire zone
		Real64 MinimumZ; // Minimum Z value for entire zone
		Real64 MaximumZ; // Maximum Z value for entire zone
		Real64 OutDryBulbTemp; // Zone outside dry bulb air temperature (C)
		Real64 OutWetBulbTemp; // Zone outside wet bulb air temperature (C)
		Real64 WindSpeed; // Zone outside wind speed (m/s)
		bool isPartOfTotalArea; // Count the zone area when determining the building total floor area
		bool isNominalOccupied; // has occupancy nominally specified
		bool isNominalControlled; // has Controlled Zone Equip Configuration reference
		Real64 TotOccupants; // total design occupancy
		// (sum of NumberOfPeople for the zone from People object)
		int AirHBimBalanceErrIndex; // error management counter
		bool NoHeatToReturnAir; // TRUE means that heat to return air should be added to the zone load
		bool RefrigCaseRA; // TRUE means there is potentially heat removal from return air
		// from refrigeration cases for this zone
		Real64 InternalHeatGains; // internal loads (W)
		Real64 NominalInfilVent; // internal infiltration/ventilaton
		Real64 NominalMixing; // internal mixing/cross mixing
		bool TempOutOfBoundsReported; // if any temp out of bounds errors, first will show zone details.
		bool EnforcedReciprocity; // if zone required forced reciprocity --
		//   less out of bounds temperature errors allowed
		int ZoneMinCO2SchedIndex; // Index for the schedule the schedule which determines minimum CO2 concentration
		int ZoneContamControllerSchedIndex; // Index for this schedule

		// Default Constructor
		ZoneData() :
			Multiplier( 1 ),
			ListMultiplier( 1 ),
			ListGroup( 0 ),
			RelNorth( 0.0 ),
			OriginX( 0.0 ),
			OriginY( 0.0 ),
			OriginZ( 0.0 ),
			CeilingHeight( AutoCalculate ),
			Volume( AutoCalculate ),
			OfType( 1 ),
			UserEnteredFloorArea( AutoCalculate ),
			FloorArea( 0.0 ),
			CalcFloorArea( 0.0 ),
			HasFloor( false ),
			HasRoof( false ),
			HasInterZoneWindow( false ),
			HasWindow( false ),
			AirCapacity( 0.0 ),
			ExtWindowArea( 0.0 ),
			ExtGrossWallArea( 0.0 ),
			ExtWindowArea_Multiplied( 0.0 ),
			ExtGrossWallArea_Multiplied( 0.0 ),
			ExtNetWallArea( 0.0 ),
			TotalSurfArea( 0.0 ),
			ExteriorTotalSurfArea( 0.0 ),
			ExteriorTotalGroundSurfArea( 0.0 ),
			ExtGrossGroundWallArea( 0.0 ),
			ExtGrossGroundWallArea_Multiplied( 0.0 ),
			SystemZoneNodeNumber( 0 ),
			IsControlled( false ),
			TempControlledZoneIndex( 0 ),
			SurfaceFirst( 0 ),
			SurfaceLast( 0 ),
			InsideConvectionAlgo( ASHRAESimple ),
			NumSurfaces( 0 ),
			NumSubSurfaces( 0 ),
			NumShadingSurfaces( 0 ),
			OutsideConvectionAlgo( ASHRAESimple ),
			Centroid( 0.0, 0.0, 0.0 ),
			MinimumX( 0.0 ),
			MaximumX( 0.0 ),
			MinimumY( 0.0 ),
			MaximumY( 0.0 ),
			MinimumZ( 0.0 ),
			MaximumZ( 0.0 ),
			OutDryBulbTemp( 0.0 ),
			OutWetBulbTemp( 0.0 ),
			WindSpeed( 0.0 ),
			isPartOfTotalArea( true ),
			isNominalOccupied( false ),
			isNominalControlled( false ),
			TotOccupants( 0.0 ),
			AirHBimBalanceErrIndex( 0 ),
			NoHeatToReturnAir( false ),
			RefrigCaseRA( false ),
			InternalHeatGains( 0.0 ),
			NominalInfilVent( 0.0 ),
			NominalMixing( 0.0 ),
			TempOutOfBoundsReported( false ),
			EnforcedReciprocity( false ),
			ZoneMinCO2SchedIndex( 0 ),
			ZoneContamControllerSchedIndex( 0 )
		{}

		// Member Constructor
		ZoneData(
			std::string const & Name,
			int const Multiplier, // Used in reporting and for systems calculations
			int const ListMultiplier, // For Zone Group object:  used in reporting and systems calculations
			int const ListGroup, // used only in Zone Group verification.  and for error message.
			Real64 const RelNorth, // Relative North (to building north) [Degrees]
			Real64 const OriginX, // X origin  [m]
			Real64 const OriginY, // Y origin  [m]
			Real64 const OriginZ, // Z origin  [m]
			Real64 const CeilingHeight, // Ceiling Height entered by user [m] or calculated
			Real64 const Volume, // Volume entered by user [m3] or calculated
			int const OfType, // 1=Standard Zone, Not yet used:
			Real64 const UserEnteredFloorArea, // User input floor area for this zone
			Real64 const FloorArea, // Floor area used for this zone
			Real64 const CalcFloorArea, // Calculated floor area used for this zone
			bool const HasFloor, // Has "Floor" surface
			bool const HasRoof, // Has "Roof" or "Ceiling" Surface
			bool const HasInterZoneWindow, // Interzone Window(s) present in this zone
			bool const HasWindow, // Window(s) present in this zone
			Real64 const AirCapacity,
			Real64 const ExtWindowArea, // Exterior Window Area for Zone
			Real64 const ExtGrossWallArea, // Exterior Wall Area for Zone (Gross)
			Real64 const ExtWindowArea_Multiplied, // Exterior Window Area for Zone with multipliers
			Real64 const ExtGrossWallArea_Multiplied, // Exterior Wall Area for Zone (Gross) with multipliers
			Real64 const ExtNetWallArea, // Exterior Wall Area for Zone (Net)
			Real64 const TotalSurfArea, // Total surface area for Zone
			Real64 const ExteriorTotalSurfArea, // Total surface area of all exterior surfaces for Zone
			Real64 const ExteriorTotalGroundSurfArea, // Total surface area of all surfaces for Zone with ground contact
			Real64 const ExtGrossGroundWallArea, // Ground contact Wall Area for Zone (Gross)
			Real64 const ExtGrossGroundWallArea_Multiplied, // Ground contact Wall Area for Zone (Gross) with multipliers
			int const SystemZoneNodeNumber, // This is the zone node number for the system for a controlled zone
			bool const IsControlled, // True when this is a controlled zone.
			int const TempControlledZoneIndex, // this is the index number for TempControlledZone structure for lookup
			int const SurfaceFirst, // First Surface in Zone
			int const SurfaceLast, // Last Surface in Zone
			int const InsideConvectionAlgo, // Ref: appropriate values for Inside Convection solution
			int const NumSurfaces, // Number of surfaces for this zone
			int const NumSubSurfaces, // Number of subsurfaces for this zone (windows, doors, tdd dome and diffusers)
			int const NumShadingSurfaces, // Number of shading surfaces for this zone
			int const OutsideConvectionAlgo, // Ref: appropriate values for Outside Convection solution
			Vector const & Centroid, // Center of the zone found by averaging wall, floor, and roof centroids
			Real64 const MinimumX, // Minimum X value for entire zone
			Real64 const MaximumX, // Maximum X value for entire zone
			Real64 const MinimumY, // Minimum Y value for entire zone
			Real64 const MaximumY, // Maximum Y value for entire zone
			Real64 const MinimumZ, // Minimum Z value for entire zone
			Real64 const MaximumZ, // Maximum Z value for entire zone
			Real64 const OutDryBulbTemp, // Zone outside dry bulb air temperature (C)
			Real64 const OutWetBulbTemp, // Zone outside wet bulb air temperature (C)
			Real64 const WindSpeed, // Zone outside wind speed (m/s)
			bool const isPartOfTotalArea, // Count the zone area when determining the building total floor area
			bool const isNominalOccupied, // has occupancy nominally specified
			bool const isNominalControlled, // has Controlled Zone Equip Configuration reference
			Real64 const TotOccupants, // total design occupancy
			int const AirHBimBalanceErrIndex, // error management counter
			bool const NoHeatToReturnAir, // TRUE means that heat to return air should be added to the zone load
			bool const RefrigCaseRA, // TRUE means there is potentially heat removal from return air
			Real64 const InternalHeatGains, // internal loads (W)
			Real64 const NominalInfilVent, // internal infiltration/ventilaton
			Real64 const NominalMixing, // internal mixing/cross mixing
			bool const TempOutOfBoundsReported, // if any temp out of bounds errors, first will show zone details.
			bool const EnforcedReciprocity, // if zone required forced reciprocity --
			int const ZoneMinCO2SchedIndex, // Index for the schedule the schedule which determines minimum CO2 concentration
			int const ZoneContamControllerSchedIndex // Index for this schedule
		) :
			Name( Name ),
			Multiplier( Multiplier ),
			ListMultiplier( ListMultiplier ),
			ListGroup( ListGroup ),
			RelNorth( RelNorth ),
			OriginX( OriginX ),
			OriginY( OriginY ),
			OriginZ( OriginZ ),
			CeilingHeight( CeilingHeight ),
			Volume( Volume ),
			OfType( OfType ),
			UserEnteredFloorArea( UserEnteredFloorArea ),
			FloorArea( FloorArea ),
			CalcFloorArea( CalcFloorArea ),
			HasFloor( HasFloor ),
			HasRoof( HasRoof ),
			HasInterZoneWindow( HasInterZoneWindow ),
			HasWindow( HasWindow ),
			AirCapacity( AirCapacity ),
			ExtWindowArea( ExtWindowArea ),
			ExtGrossWallArea( ExtGrossWallArea ),
			ExtWindowArea_Multiplied( ExtWindowArea_Multiplied ),
			ExtGrossWallArea_Multiplied( ExtGrossWallArea_Multiplied ),
			ExtNetWallArea( ExtNetWallArea ),
			TotalSurfArea( TotalSurfArea ),
			ExteriorTotalSurfArea( ExteriorTotalSurfArea ),
			ExteriorTotalGroundSurfArea( ExteriorTotalGroundSurfArea ),
			ExtGrossGroundWallArea( ExtGrossGroundWallArea ),
			ExtGrossGroundWallArea_Multiplied( ExtGrossGroundWallArea_Multiplied ),
			SystemZoneNodeNumber( SystemZoneNodeNumber ),
			IsControlled( IsControlled ),
			TempControlledZoneIndex( TempControlledZoneIndex ),
			SurfaceFirst( SurfaceFirst ),
			SurfaceLast( SurfaceLast ),
			InsideConvectionAlgo( InsideConvectionAlgo ),
			NumSurfaces( NumSurfaces ),
			NumSubSurfaces( NumSubSurfaces ),
			NumShadingSurfaces( NumShadingSurfaces ),
			OutsideConvectionAlgo( OutsideConvectionAlgo ),
			Centroid( Centroid ),
			MinimumX( MinimumX ),
			MaximumX( MaximumX ),
			MinimumY( MinimumY ),
			MaximumY( MaximumY ),
			MinimumZ( MinimumZ ),
			MaximumZ( MaximumZ ),
			OutDryBulbTemp( OutDryBulbTemp ),
			OutWetBulbTemp( OutWetBulbTemp ),
			WindSpeed( WindSpeed ),
			isPartOfTotalArea( isPartOfTotalArea ),
			isNominalOccupied( isNominalOccupied ),
			isNominalControlled( isNominalControlled ),
			TotOccupants( TotOccupants ),
			AirHBimBalanceErrIndex( AirHBimBalanceErrIndex ),
			NoHeatToReturnAir( NoHeatToReturnAir ),
			RefrigCaseRA( RefrigCaseRA ),
			InternalHeatGains( InternalHeatGains ),
			NominalInfilVent( NominalInfilVent ),
			NominalMixing( NominalMixing ),
			TempOutOfBoundsReported( TempOutOfBoundsReported ),
			EnforcedReciprocity( EnforcedReciprocity ),
			ZoneMinCO2SchedIndex( ZoneMinCO2SchedIndex ),
			ZoneContamControllerSchedIndex( ZoneContamControllerSchedIndex )
		{}

	};

	struct ZoneListData
	{
		// Members
		std::string Name; // Zone List name
		int NumOfZones; // Number of zones in the list
		std::string::size_type MaxZoneNameLength; // Max Name length of zones in the list
		Array1D_int Zone; // Pointers to zones in the list

		// Default Constructor
		ZoneListData() :
			NumOfZones( 0 ),
			MaxZoneNameLength( 0u )
		{}

		// Member Constructor
		ZoneListData(
			std::string const & Name, // Zone List name
			int const NumOfZones, // Number of zones in the list
			std::string::size_type const MaxZoneNameLength, // Max Name length of zones in the list
			Array1_int const & Zone // Pointers to zones in the list
		) :
			Name( Name ),
			NumOfZones( NumOfZones ),
			MaxZoneNameLength( MaxZoneNameLength ),
			Zone( Zone )
		{}

	};

	struct ZoneGroupData
	{
		// Members
		std::string Name; // Zone Group name
		int ZoneList; // Pointer to the zone list
		int Multiplier; // Zone List multiplier

		// Default Constructor
		ZoneGroupData() :
			ZoneList( 0 ),
			Multiplier( 1 )
		{}

		// Member Constructor
		ZoneGroupData(
			std::string const & Name, // Zone Group name
			int const ZoneList, // Pointer to the zone list
			int const Multiplier // Zone List multiplier
		) :
			Name( Name ),
			ZoneList( ZoneList ),
			Multiplier( Multiplier )
		{}

	};

	struct GlobalInternalGainMiscObject
	{
		// Members
		std::string Name;
		int ZoneOrZoneListPtr;
		int NumOfZones;
		int StartPtr;
		bool ZoneListActive;

		// Default Constructor
		GlobalInternalGainMiscObject() :
			ZoneOrZoneListPtr( 0 ),
			NumOfZones( 0 ),
			StartPtr( 0 ),
			ZoneListActive( false )
		{}

		// Member Constructor
		GlobalInternalGainMiscObject(
			std::string const & Name,
			int const ZoneOrZoneListPtr,
			int const NumOfZones,
			int const StartPtr,
			bool const ZoneListActive
		) :
			Name( Name ),
			ZoneOrZoneListPtr( ZoneOrZoneListPtr ),
			NumOfZones( NumOfZones ),
			StartPtr( StartPtr ),
			ZoneListActive( ZoneListActive )
		{}

	};

	struct PeopleData
	{
		// Members
		std::string Name; // PEOPLE object name
		int ZonePtr; // Pointer to the zone number for this people statement
		Real64 NumberOfPeople; // Maximum number of people for this statement
		int NumberOfPeoplePtr; // Pointer to schedule for number of people
		bool EMSPeopleOn; // EMS actuating number of people if .TRUE.
		Real64 EMSNumberOfPeople; // Value EMS is directing to use for override
		// Note that the schedule and maximum number was kept for people since it seemed likely that
		// users would want to assign the same schedule to multiple people statements.
		int ActivityLevelPtr; // Pointer to schedule for activity level
		Real64 FractionRadiant; // Percentage (fraction 0.0-1.0) of sensible heat gain from people
		// that is radiant
		Real64 FractionConvected; // Percentage (fraction 0.0-1.0) of sensible heat gain from people
		// that is convective
		Real64 NomMinNumberPeople; // Nominal Minimum Number of People (min sch X number of people)
		Real64 NomMaxNumberPeople; // Nominal Maximum Number of People (min sch X number of people)
		int WorkEffPtr; // Pointer to schedule for work efficiency
		int ClothingPtr; // Pointer to schedule for clothing insulation
		int ClothingMethodPtr;
		int ClothingType; // Name of clothing type
		int AirVelocityPtr; // Pointer to schedule for air velocity in zone
		bool Fanger; // True when Fanger calculation to be performed
		bool Pierce; // True when Pierce 2-node calculation to be performed
		bool KSU; // True when KSU 2-node calculation to be performed
		bool AdaptiveASH55; // True when ASHRAE Standard 55 adaptive comfort calculation
		//   to be performed
		bool AdaptiveCEN15251; // True when CEN Standard 15251 adaptive comfort calculation
		//   to be performed
		int MRTCalcType; // MRT calculation type (See MRT Calculation type parameters)
		int SurfacePtr; // Pointer to the name of surface
		std::string AngleFactorListName; // Name of angle factor list
		int AngleFactorListPtr; // Pointer to the name of angle factor list
		Real64 UserSpecSensFrac; // User specified sensible fraction
		bool Show55Warning; // show the warning messages about ASHRAE 55-2004
		Real64 CO2RateFactor; // Carbon Dioxide Generation Rate [m3/s-W]
		// Report variables
		Real64 NumOcc; // Number of occupants []
		Real64 TemperatureInZone; // Temperature in zone (C)
		Real64 RelativeHumidityInZone; // Relative humidity in zone
		Real64 RadGainRate; // Radiant heat gain [W]
		Real64 ConGainRate; // Convective heat gain [W]
		Real64 SenGainRate; // Sensible heat gain [W]
		Real64 LatGainRate; // Latent heat gain [W]
		Real64 TotGainRate; // Total heat gain [W]
		Real64 CO2GainRate; // Carbon Dioxide Gain Rate [m3/s]
		Real64 RadGainEnergy; // Radiant heat gain [J]
		Real64 ConGainEnergy; // Convective heat gain [J]
		Real64 SenGainEnergy; // Sensible heat gain [J]
		Real64 LatGainEnergy; // Latent heat gain [J]
		Real64 TotGainEnergy; // Total heat gain [J]
		// Air velocity check during run time for thermal comfort control
		int AirVelErrIndex; // Air velocity error index
		// For AdaptiveComfort tabular report
		Real64 TimeNotMetASH5580;
		Real64 TimeNotMetASH5590;
		Real64 TimeNotMetCEN15251CatI;
		Real64 TimeNotMetCEN15251CatII;
		Real64 TimeNotMetCEN15251CatIII;

		// Default Constructor
		PeopleData() :
			ZonePtr( 0 ),
			NumberOfPeople( 0.0 ),
			NumberOfPeoplePtr( -1 ),
			EMSPeopleOn( false ),
			EMSNumberOfPeople( 0.0 ),
			ActivityLevelPtr( -1 ),
			FractionRadiant( 0.0 ),
			FractionConvected( 0.0 ),
			NomMinNumberPeople( 0.0 ),
			NomMaxNumberPeople( 0.0 ),
			WorkEffPtr( -1 ),
			ClothingPtr( -1 ),
			ClothingMethodPtr( -1 ),
			ClothingType( -1 ),
			AirVelocityPtr( -1 ),
			Fanger( false ),
			Pierce( false ),
			KSU( false ),
			AdaptiveASH55( false ),
			AdaptiveCEN15251( false ),
			MRTCalcType( 0 ),
			SurfacePtr( -1 ),
			AngleFactorListPtr( -1 ),
			UserSpecSensFrac( 0.0 ),
			Show55Warning( false ),
			CO2RateFactor( 0.0 ),
			NumOcc( 0.0 ),
			TemperatureInZone( 0.0 ),
			RelativeHumidityInZone( 0.0 ),
			RadGainRate( 0.0 ),
			ConGainRate( 0.0 ),
			SenGainRate( 0.0 ),
			LatGainRate( 0.0 ),
			TotGainRate( 0.0 ),
			CO2GainRate( 0.0 ),
			RadGainEnergy( 0.0 ),
			ConGainEnergy( 0.0 ),
			SenGainEnergy( 0.0 ),
			LatGainEnergy( 0.0 ),
			TotGainEnergy( 0.0 ),
			AirVelErrIndex( 0 ),
			TimeNotMetASH5580( 0.0 ),
			TimeNotMetASH5590( 0.0 ),
			TimeNotMetCEN15251CatI( 0.0 ),
			TimeNotMetCEN15251CatII( 0.0 ),
			TimeNotMetCEN15251CatIII( 0.0 )
		{}

		// Member Constructor
		PeopleData(
			std::string const & Name, // PEOPLE object name
			int const ZonePtr, // Pointer to the zone number for this people statement
			Real64 const NumberOfPeople, // Maximum number of people for this statement
			int const NumberOfPeoplePtr, // Pointer to schedule for number of people
			bool const EMSPeopleOn, // EMS actuating number of people if .TRUE.
			Real64 const EMSNumberOfPeople, // Value EMS is directing to use for override
			int const ActivityLevelPtr, // Pointer to schedule for activity level
			Real64 const FractionRadiant, // Percentage (fraction 0.0-1.0) of sensible heat gain from people
			Real64 const FractionConvected, // Percentage (fraction 0.0-1.0) of sensible heat gain from people
			Real64 const NomMinNumberPeople, // Nominal Minimum Number of People (min sch X number of people)
			Real64 const NomMaxNumberPeople, // Nominal Maximum Number of People (min sch X number of people)
			int const WorkEffPtr, // Pointer to schedule for work efficiency
			int const ClothingPtr, // Pointer to schedule for clothing insulation
			int const ClothingMethodPtr,
			int const ClothingType, // Name of clothing type
			int const AirVelocityPtr, // Pointer to schedule for air velocity in zone
			bool const Fanger, // True when Fanger calculation to be performed
			bool const Pierce, // True when Pierce 2-node calculation to be performed
			bool const KSU, // True when KSU 2-node calculation to be performed
			bool const AdaptiveASH55, // True when ASHRAE Standard 55 adaptive comfort calculation
			bool const AdaptiveCEN15251, // True when CEN Standard 15251 adaptive comfort calculation
			int const MRTCalcType, // MRT calculation type (See MRT Calculation type parameters)
			int const SurfacePtr, // Pointer to the name of surface
			std::string const & AngleFactorListName, // Name of angle factor list
			int const AngleFactorListPtr, // Pointer to the name of angle factor list
			Real64 const UserSpecSensFrac, // User specified sensible fraction
			bool const Show55Warning, // show the warning messages about ASHRAE 55-2004
			Real64 const CO2RateFactor, // Carbon Dioxide Generation Rate [m3/s-W]
			Real64 const NumOcc, // Number of occupants []
			Real64 const TemperatureInZone, // Temperature in zone (C)
			Real64 const RelativeHumidityInZone, // Relative humidity in zone
			Real64 const RadGainRate, // Radiant heat gain [W]
			Real64 const ConGainRate, // Convective heat gain [W]
			Real64 const SenGainRate, // Sensible heat gain [W]
			Real64 const LatGainRate, // Latent heat gain [W]
			Real64 const TotGainRate, // Total heat gain [W]
			Real64 const CO2GainRate, // Carbon Dioxide Gain Rate [m3/s]
			Real64 const RadGainEnergy, // Radiant heat gain [J]
			Real64 const ConGainEnergy, // Convective heat gain [J]
			Real64 const SenGainEnergy, // Sensible heat gain [J]
			Real64 const LatGainEnergy, // Latent heat gain [J]
			Real64 const TotGainEnergy, // Total heat gain [J]
			int const AirVelErrIndex, // Air velocity error index
			Real64 const TimeNotMetASH5580,
			Real64 const TimeNotMetASH5590,
			Real64 const TimeNotMetCEN15251CatI,
			Real64 const TimeNotMetCEN15251CatII,
			Real64 const TimeNotMetCEN15251CatIII
		) :
			Name( Name ),
			ZonePtr( ZonePtr ),
			NumberOfPeople( NumberOfPeople ),
			NumberOfPeoplePtr( NumberOfPeoplePtr ),
			EMSPeopleOn( EMSPeopleOn ),
			EMSNumberOfPeople( EMSNumberOfPeople ),
			ActivityLevelPtr( ActivityLevelPtr ),
			FractionRadiant( FractionRadiant ),
			FractionConvected( FractionConvected ),
			NomMinNumberPeople( NomMinNumberPeople ),
			NomMaxNumberPeople( NomMaxNumberPeople ),
			WorkEffPtr( WorkEffPtr ),
			ClothingPtr( ClothingPtr ),
			ClothingMethodPtr( ClothingMethodPtr ),
			ClothingType( ClothingType ),
			AirVelocityPtr( AirVelocityPtr ),
			Fanger( Fanger ),
			Pierce( Pierce ),
			KSU( KSU ),
			AdaptiveASH55( AdaptiveASH55 ),
			AdaptiveCEN15251( AdaptiveCEN15251 ),
			MRTCalcType( MRTCalcType ),
			SurfacePtr( SurfacePtr ),
			AngleFactorListName( AngleFactorListName ),
			AngleFactorListPtr( AngleFactorListPtr ),
			UserSpecSensFrac( UserSpecSensFrac ),
			Show55Warning( Show55Warning ),
			CO2RateFactor( CO2RateFactor ),
			NumOcc( NumOcc ),
			TemperatureInZone( TemperatureInZone ),
			RelativeHumidityInZone( RelativeHumidityInZone ),
			RadGainRate( RadGainRate ),
			ConGainRate( ConGainRate ),
			SenGainRate( SenGainRate ),
			LatGainRate( LatGainRate ),
			TotGainRate( TotGainRate ),
			CO2GainRate( CO2GainRate ),
			RadGainEnergy( RadGainEnergy ),
			ConGainEnergy( ConGainEnergy ),
			SenGainEnergy( SenGainEnergy ),
			LatGainEnergy( LatGainEnergy ),
			TotGainEnergy( TotGainEnergy ),
			AirVelErrIndex( AirVelErrIndex ),
			TimeNotMetASH5580( TimeNotMetASH5580 ),
			TimeNotMetASH5590( TimeNotMetASH5590 ),
			TimeNotMetCEN15251CatI( TimeNotMetCEN15251CatI ),
			TimeNotMetCEN15251CatII( TimeNotMetCEN15251CatII ),
			TimeNotMetCEN15251CatIII( TimeNotMetCEN15251CatIII )
		{}

	};

	struct LightsData
	{
		// Members
		std::string Name; // LIGHTS object name
		int ZonePtr; // Which zone lights are in
		int SchedPtr; // Schedule for lights
		Real64 DesignLevel; // design level for lights [W]
		bool EMSLightsOn; // EMS actuating Lighting power if .TRUE.
		Real64 EMSLightingPower; // Value EMS is directing to use for override
		Real64 FractionReturnAir; // Percentage (fraction 0.0-1.0) of sensible heat gain that is return air
		Real64 FractionRadiant; // Percentage (fraction 0.0-1.0) of sensible heat gain that is radiant
		Real64 FractionShortWave; // Percentage (fraction 0.0-1.0) of sensible heat gain that is short wave
		Real64 FractionReplaceable; // Percentage (fraction 0.0-1.0) of sensible heat gain that is replaceable
		Real64 FractionConvected; // Percentage (fraction 0.0-1.0) of sensible heat gain that is convective
		bool FractionReturnAirIsCalculated;
		Real64 FractionReturnAirPlenTempCoeff1;
		Real64 FractionReturnAirPlenTempCoeff2;
		Real64 NomMinDesignLevel; // Nominal Minimum Design Level (min sch X design level)
		Real64 NomMaxDesignLevel; // Nominal Maximum Design Level (max sch X design level)
		bool ManageDemand; // Flag to indicate whether to use demand limiting
		Real64 DemandLimit; // Demand limit set by demand manager [W]
		// Report variables
		Real64 Power; // Electric power [W]
		Real64 RadGainRate; // Radiant heat gain [W]
		Real64 VisGainRate; // Visible heat gain [W]
		Real64 ConGainRate; // Convective heat gain [W]
		Real64 RetAirGainRate; // Return air heat gain [W]
		Real64 TotGainRate; // Total heat gain [W]
		Real64 Consumption; // Electric consumption [J]
		Real64 RadGainEnergy; // Radiant heat gain [J]
		Real64 VisGainEnergy; // Visible heat gain [J]
		Real64 ConGainEnergy; // Convective heat gain [J]
		Real64 RetAirGainEnergy; // Return air heat gain [J]
		Real64 TotGainEnergy; // Total heat gain [J]
		std::string EndUseSubcategory; // user defined name for the end use category
		Real64 SumConsumption; // sum of electric consumption [J] for reporting
		Real64 SumTimeNotZeroCons; // sum of time of positive electric consumption [hr]

		// Default Constructor
		LightsData() :
			ZonePtr( 0 ),
			SchedPtr( -1 ),
			DesignLevel( 0.0 ),
			EMSLightsOn( false ),
			EMSLightingPower( 0.0 ),
			FractionReturnAir( 0.0 ),
			FractionRadiant( 0.0 ),
			FractionShortWave( 0.0 ),
			FractionReplaceable( 0.0 ),
			FractionConvected( 0.0 ),
			FractionReturnAirIsCalculated( false ),
			FractionReturnAirPlenTempCoeff1( 0.0 ),
			FractionReturnAirPlenTempCoeff2( 0.0 ),
			NomMinDesignLevel( 0.0 ),
			NomMaxDesignLevel( 0.0 ),
			ManageDemand( false ),
			DemandLimit( 0.0 ),
			Power( 0.0 ),
			RadGainRate( 0.0 ),
			VisGainRate( 0.0 ),
			ConGainRate( 0.0 ),
			RetAirGainRate( 0.0 ),
			TotGainRate( 0.0 ),
			Consumption( 0.0 ),
			RadGainEnergy( 0.0 ),
			VisGainEnergy( 0.0 ),
			ConGainEnergy( 0.0 ),
			RetAirGainEnergy( 0.0 ),
			TotGainEnergy( 0.0 ),
			SumConsumption( 0.0 ),
			SumTimeNotZeroCons( 0.0 )
		{}

		// Member Constructor
		LightsData(
			std::string const & Name, // LIGHTS object name
			int const ZonePtr, // Which zone lights are in
			int const SchedPtr, // Schedule for lights
			Real64 const DesignLevel, // design level for lights [W]
			bool const EMSLightsOn, // EMS actuating Lighting power if .TRUE.
			Real64 const EMSLightingPower, // Value EMS is directing to use for override
			Real64 const FractionReturnAir, // Percentage (fraction 0.0-1.0) of sensible heat gain that is return air
			Real64 const FractionRadiant, // Percentage (fraction 0.0-1.0) of sensible heat gain that is radiant
			Real64 const FractionShortWave, // Percentage (fraction 0.0-1.0) of sensible heat gain that is short wave
			Real64 const FractionReplaceable, // Percentage (fraction 0.0-1.0) of sensible heat gain that is replaceable
			Real64 const FractionConvected, // Percentage (fraction 0.0-1.0) of sensible heat gain that is convective
			bool const FractionReturnAirIsCalculated,
			Real64 const FractionReturnAirPlenTempCoeff1,
			Real64 const FractionReturnAirPlenTempCoeff2,
			Real64 const NomMinDesignLevel, // Nominal Minimum Design Level (min sch X design level)
			Real64 const NomMaxDesignLevel, // Nominal Maximum Design Level (max sch X design level)
			bool const ManageDemand, // Flag to indicate whether to use demand limiting
			Real64 const DemandLimit, // Demand limit set by demand manager [W]
			Real64 const Power, // Electric power [W]
			Real64 const RadGainRate, // Radiant heat gain [W]
			Real64 const VisGainRate, // Visible heat gain [W]
			Real64 const ConGainRate, // Convective heat gain [W]
			Real64 const RetAirGainRate, // Return air heat gain [W]
			Real64 const TotGainRate, // Total heat gain [W]
			Real64 const Consumption, // Electric consumption [J]
			Real64 const RadGainEnergy, // Radiant heat gain [J]
			Real64 const VisGainEnergy, // Visible heat gain [J]
			Real64 const ConGainEnergy, // Convective heat gain [J]
			Real64 const RetAirGainEnergy, // Return air heat gain [J]
			Real64 const TotGainEnergy, // Total heat gain [J]
			std::string const & EndUseSubcategory, // user defined name for the end use category
			Real64 const SumConsumption, // sum of electric consumption [J] for reporting
			Real64 const SumTimeNotZeroCons // sum of time of positive electric consumption [hr]
		) :
			Name( Name ),
			ZonePtr( ZonePtr ),
			SchedPtr( SchedPtr ),
			DesignLevel( DesignLevel ),
			EMSLightsOn( EMSLightsOn ),
			EMSLightingPower( EMSLightingPower ),
			FractionReturnAir( FractionReturnAir ),
			FractionRadiant( FractionRadiant ),
			FractionShortWave( FractionShortWave ),
			FractionReplaceable( FractionReplaceable ),
			FractionConvected( FractionConvected ),
			FractionReturnAirIsCalculated( FractionReturnAirIsCalculated ),
			FractionReturnAirPlenTempCoeff1( FractionReturnAirPlenTempCoeff1 ),
			FractionReturnAirPlenTempCoeff2( FractionReturnAirPlenTempCoeff2 ),
			NomMinDesignLevel( NomMinDesignLevel ),
			NomMaxDesignLevel( NomMaxDesignLevel ),
			ManageDemand( ManageDemand ),
			DemandLimit( DemandLimit ),
			Power( Power ),
			RadGainRate( RadGainRate ),
			VisGainRate( VisGainRate ),
			ConGainRate( ConGainRate ),
			RetAirGainRate( RetAirGainRate ),
			TotGainRate( TotGainRate ),
			Consumption( Consumption ),
			RadGainEnergy( RadGainEnergy ),
			VisGainEnergy( VisGainEnergy ),
			ConGainEnergy( ConGainEnergy ),
			RetAirGainEnergy( RetAirGainEnergy ),
			TotGainEnergy( TotGainEnergy ),
			EndUseSubcategory( EndUseSubcategory ),
			SumConsumption( SumConsumption ),
			SumTimeNotZeroCons( SumTimeNotZeroCons )
		{}

	};

	struct ZoneEquipData // Electric, Gas, Other Equipment, CO2
	{
		// Members
		std::string Name; // EQUIPMENT object name
		int ZonePtr; // Which zone internal gain is in
		int SchedPtr; // Schedule for internal gain
		Real64 DesignLevel; // design level for internal gain [W]
		bool EMSZoneEquipOverrideOn; // EMS actuating equipment power if .TRUE.
		Real64 EMSEquipPower; // Value EMS is directing to use for override
		Real64 FractionLatent; // Percentage (fraction 0.0-1.0) of sensible heat gain that is latent
		Real64 FractionRadiant; // Percentage (fraction 0.0-1.0) of sensible heat gain that is radiant
		Real64 FractionLost; // Percentage (fraction 0.0-1.0) of sensible heat gain that is lost
		Real64 FractionConvected; // Percentage (fraction 0.0-1.0) of sensible heat gain that is convective
		Real64 CO2DesignRate; // CO2 design Rate [m3/s]
		Real64 CO2RateFactor; // CO2 rate factor [m3/s/W]
		Real64 NomMinDesignLevel; // Nominal Minimum Design Level (min sch X design level)
		Real64 NomMaxDesignLevel; // Nominal Maximum Design Level (max sch X design level)
		bool ManageDemand; // Flag to indicate whether to use demand limiting
		Real64 DemandLimit; // Demand limit set by demand manager [W]
		// Report variables
		Real64 Power; // Electric/Gas/Fuel power [W]
		Real64 RadGainRate; // Radiant heat gain [W]
		Real64 ConGainRate; // Convective heat gain [W]
		Real64 LatGainRate; // Latent heat gain [W]
		Real64 LostRate; // Lost energy (converted to work) [W]
		Real64 TotGainRate; // Total heat gain [W]
		Real64 CO2GainRate; // CO2 gain rate [m3/s]
		Real64 Consumption; // Electric/Gas/Fuel consumption [J]
		Real64 RadGainEnergy; // Radiant heat gain [J]
		Real64 ConGainEnergy; // Convective heat gain [J]
		Real64 LatGainEnergy; // Latent heat gain [J]
		Real64 LostEnergy; // Lost energy (converted to work) [J]
		Real64 TotGainEnergy; // Total heat gain [J]
		std::string EndUseSubcategory; // user defined name for the end use category

		// Default Constructor
		ZoneEquipData() :
			ZonePtr( 0 ),
			SchedPtr( 0 ),
			DesignLevel( 0.0 ),
			EMSZoneEquipOverrideOn( false ),
			EMSEquipPower( 0.0 ),
			FractionLatent( 0.0 ),
			FractionRadiant( 0.0 ),
			FractionLost( 0.0 ),
			FractionConvected( 0.0 ),
			CO2DesignRate( 0.0 ),
			CO2RateFactor( 0.0 ),
			NomMinDesignLevel( 0.0 ),
			NomMaxDesignLevel( 0.0 ),
			ManageDemand( false ),
			DemandLimit( 0.0 ),
			Power( 0.0 ),
			RadGainRate( 0.0 ),
			ConGainRate( 0.0 ),
			LatGainRate( 0.0 ),
			LostRate( 0.0 ),
			TotGainRate( 0.0 ),
			CO2GainRate( 0.0 ),
			Consumption( 0.0 ),
			RadGainEnergy( 0.0 ),
			ConGainEnergy( 0.0 ),
			LatGainEnergy( 0.0 ),
			LostEnergy( 0.0 ),
			TotGainEnergy( 0.0 )
		{}

		// Member Constructor
		ZoneEquipData(
			std::string const & Name, // EQUIPMENT object name
			int const ZonePtr, // Which zone internal gain is in
			int const SchedPtr, // Schedule for internal gain
			Real64 const DesignLevel, // design level for internal gain [W]
			bool const EMSZoneEquipOverrideOn, // EMS actuating equipment power if .TRUE.
			Real64 const EMSEquipPower, // Value EMS is directing to use for override
			Real64 const FractionLatent, // Percentage (fraction 0.0-1.0) of sensible heat gain that is latent
			Real64 const FractionRadiant, // Percentage (fraction 0.0-1.0) of sensible heat gain that is radiant
			Real64 const FractionLost, // Percentage (fraction 0.0-1.0) of sensible heat gain that is lost
			Real64 const FractionConvected, // Percentage (fraction 0.0-1.0) of sensible heat gain that is convective
			Real64 const CO2DesignRate, // CO2 design Rate [m3/s]
			Real64 const CO2RateFactor, // CO2 rate factor [m3/s/W]
			Real64 const NomMinDesignLevel, // Nominal Minimum Design Level (min sch X design level)
			Real64 const NomMaxDesignLevel, // Nominal Maximum Design Level (max sch X design level)
			bool const ManageDemand, // Flag to indicate whether to use demand limiting
			Real64 const DemandLimit, // Demand limit set by demand manager [W]
			Real64 const Power, // Electric/Gas/Fuel power [W]
			Real64 const RadGainRate, // Radiant heat gain [W]
			Real64 const ConGainRate, // Convective heat gain [W]
			Real64 const LatGainRate, // Latent heat gain [W]
			Real64 const LostRate, // Lost energy (converted to work) [W]
			Real64 const TotGainRate, // Total heat gain [W]
			Real64 const CO2GainRate, // CO2 gain rate [m3/s]
			Real64 const Consumption, // Electric/Gas/Fuel consumption [J]
			Real64 const RadGainEnergy, // Radiant heat gain [J]
			Real64 const ConGainEnergy, // Convective heat gain [J]
			Real64 const LatGainEnergy, // Latent heat gain [J]
			Real64 const LostEnergy, // Lost energy (converted to work) [J]
			Real64 const TotGainEnergy, // Total heat gain [J]
			std::string const & EndUseSubcategory // user defined name for the end use category
		) :
			Name( Name ),
			ZonePtr( ZonePtr ),
			SchedPtr( SchedPtr ),
			DesignLevel( DesignLevel ),
			EMSZoneEquipOverrideOn( EMSZoneEquipOverrideOn ),
			EMSEquipPower( EMSEquipPower ),
			FractionLatent( FractionLatent ),
			FractionRadiant( FractionRadiant ),
			FractionLost( FractionLost ),
			FractionConvected( FractionConvected ),
			CO2DesignRate( CO2DesignRate ),
			CO2RateFactor( CO2RateFactor ),
			NomMinDesignLevel( NomMinDesignLevel ),
			NomMaxDesignLevel( NomMaxDesignLevel ),
			ManageDemand( ManageDemand ),
			DemandLimit( DemandLimit ),
			Power( Power ),
			RadGainRate( RadGainRate ),
			ConGainRate( ConGainRate ),
			LatGainRate( LatGainRate ),
			LostRate( LostRate ),
			TotGainRate( TotGainRate ),
			CO2GainRate( CO2GainRate ),
			Consumption( Consumption ),
			RadGainEnergy( RadGainEnergy ),
			ConGainEnergy( ConGainEnergy ),
			LatGainEnergy( LatGainEnergy ),
			LostEnergy( LostEnergy ),
			TotGainEnergy( TotGainEnergy ),
			EndUseSubcategory( EndUseSubcategory )
		{}

	};

	struct ITEquipData // IT Equipment
	{
		// Members
		std::string Name; // EQUIPMENT object name
		int ZonePtr; // Which zone internal gain is in
		Real64 DesignTotalPower; // Design level for internal gain [W]
		Real64 NomMinDesignLevel; // Nominal Minimum Design Level (min sch X design level)
		Real64 NomMaxDesignLevel; // Nominal Maximum Design Level (max sch X design level)
		Real64 DesignFanPowerFrac; // Fraction (0.0-1.0) of design power level that is fans
		int OperSchedPtr; // Schedule pointer for design power input or operating schedule
		int CPULoadSchedPtr; // Schedule pointer for CPU loading schedule
		Real64 DesignTAirIn; // Design entering air dry-bulb temperature [C]
		Real64 DesignFanPower; // Design fan power input [W]
		Real64 DesignCPUPower; // Design CPU power input [W]
		Real64 DesignAirVolFlowRate; // Design air volume flow rate [m3/s]
		int Class; // Environmental class index (A1=1, A2=2, A3=3, A4=4, B=5, C=6)
		int AirFlowFLTCurve; // Index for airflow function of CPULoadFrac (x) and TAirIn (y) curve
		int CPUPowerFLTCurve; // Index for CPU power function of CPULoadFrac (x) and TAirIn (y) curve
		int FanPowerFFCurve; // Index for fan power function of flow fraction curve
		int AirConnectionType; // Air connection type (AdjustedSupply, ZoneAirNode, RoomAirModel)
		int InletRoomAirNodeNum; // Room air model node number for air inlet
		int OutletRoomAirNodeNum; // Room air model node number for air outlet
		int SupplyAirNodeNum; // Node number for supply air inlet
		Real64 DesignRecircFrac; // Design recirculation fraction (0.0-0.5)
		int RecircFLTCurve; // Index for recirculation function of CPULoadFrac (x) and TSupply (y) curve
		Real64 DesignUPSEfficiency; // Design power supply efficiency (>0.0 - 1.0)
		int UPSEfficFPLRCurve; // Index for recirculation function of part load ratio
		Real64 UPSLossToZoneFrac; // Fraction of UPS power loss to zone (0.0 - 1.0); remainder is lost
		std::string EndUseSubcategoryCPU; // User defined name for the end use category for the CPU
		std::string EndUseSubcategoryFan; // User defined name for the end use category for the Fans
		std::string EndUseSubcategoryUPS; // User defined name for the end use category for the power supply
		bool EMSCPUPowerOverrideOn; // EMS actuating CPU power if .TRUE.
		Real64 EMSCPUPower; // Value EMS is directing to use for override of CPU power [W]
		bool EMSFanPowerOverrideOn; // EMS actuating Fan power if .TRUE.
		Real64 EMSFanPower; // Value EMS is directing to use for override of Fan power [W]
		bool EMSUPSPowerOverrideOn; // EMS actuating UPS power if .TRUE.
		Real64 EMSUPSPower; // Value EMS is directing to use for override of UPS power [W]
		// Report variables
		Real64 CPUPower; // ITE CPU Electric Power [W]
		Real64 FanPower; // ITE Fan Electric Power [W]
		Real64 UPSPower; // ITE UPS Electric Power [W]
		Real64 CPUPowerAtDesign; // ITE CPU Electric Power at Design Inlet Conditions [W]
		Real64 FanPowerAtDesign; // ITE Fan Electric Power at Design Inlet Conditions [W]
		Real64 UPSGainRateToZone; // ITE UPS Heat Gain to Zone Rate [W] - convective gain
		Real64 ConGainRateToZone; // ITE Total Heat Gain to Zone Rate [W] - convective gain - includes heat gain from UPS, plus CPU and Fans if room air model not used
		Real64 CPUConsumption; // ITE CPU Electric Energy [J]
		Real64 FanConsumption; // ITE Fan Electric Energy [J]
		Real64 UPSConsumption; // ITE UPS Electric Energy [J]
		Real64 CPUEnergyAtDesign; // ITE CPU Electric Energy at Design Inlet Conditions [J]
		Real64 FanEnergyAtDesign; // ITE Fan Electric Energy at Design Inlet Conditions [J]
		Real64 UPSGainEnergyToZone; // ITE UPS Heat Gain to Zone Energy [J] - convective gain
		Real64 ConGainEnergyToZone; // ITE Total Heat Gain to Zone Energy [J] - convective gain - includes heat gain from UPS, plus CPU and Fans if room air model not used
		Real64 AirVolFlowStdDensity; // Air volume flow rate at standard density [m3/s]
		Real64 AirVolFlowCurDensity; // Air volume flow rate at current density [m3/s]
		Real64 AirMassFlow; // Air mass flow rate [kg/s]
		Real64 AirInletDryBulbT; // Air inlet dry-bulb temperature [C]
		Real64 AirInletDewpointT; // Air inlet dewpoit temperature [C]
		Real64 AirInletRelHum; // Air inlet relative humidity [%]
		Real64 AirOutletDryBulbT; // Air outlet dry-bulb temperature [C]
		Real64 SHI; // Supply Heat Index []
		Real64 TimeOutOfOperRange; // ITE Air Inlet Operating Range Exceeded Time [hr]
		Real64 TimeAboveDryBulbT; // ITE Air Inlet Dry-Bulb Temperature Above Operating Range Time [hr]
		Real64 TimeBelowDryBulbT; // ITE Air Inlet Dry-Bulb Temperature Below Operating Range Time [hr]
		Real64 TimeAboveDewpointT; // ITE Air Inlet Dewpoint Temperature Above Operating Range Time [hr]
		Real64 TimeBelowDewpointT; // ITE Air Inlet Dewpoint Temperature Below Operating Range Time [hr]
		Real64 TimeAboveRH; // ITE Air Inlet Relative Humidity Above Operating Range Time [hr]
		Real64 TimeBelowRH; // ITE Air Inlet Relative Humidity Below Operating Range Time [hr]
		Real64 DryBulbTAboveDeltaT; // ITE Air Inlet Dry-Bulb Temperature Difference Above Operating Range [deltaC]
		Real64 DryBulbTBelowDeltaT; // ITE Air Inlet Dry-Bulb Temperature Difference Below Operating Range [deltaC]
		Real64 DewpointTAboveDeltaT; // ITE Air Inlet Dewpoint Temperature Difference Above Operating Range [deltaC]
		Real64 DewpointTBelowDeltaT; // ITE Air Inlet Dewpoint Temperature Difference Below Operating Range [deltaC]
		Real64 RHAboveDeltaRH; // ITE Air Inlet Relative Humidity Difference Above Operating Range [%]
		Real64 RHBelowDeltaRH; // ITE Air Inlet Relative Humidity Difference Below Operating Range [%]

		// Default Constructor
		ITEquipData() :
			ZonePtr( 0 ),
			DesignTotalPower( 0.0 ),
			NomMinDesignLevel( 0.0 ),
			NomMaxDesignLevel( 0.0 ),
			DesignFanPowerFrac( 0.0 ),
			OperSchedPtr( 0 ),
			CPULoadSchedPtr( 0 ),
			DesignTAirIn( 0.0 ),
			DesignFanPower( 0.0 ),
			DesignCPUPower( 0.0 ),
			DesignAirVolFlowRate( 0.0 ),
			Class ( 0 ),
			AirFlowFLTCurve( 0 ),
			CPUPowerFLTCurve( 0 ),
			FanPowerFFCurve( 0 ),
			AirConnectionType( 0 ),
			InletRoomAirNodeNum( 0 ),
			OutletRoomAirNodeNum( 0 ),
			SupplyAirNodeNum( 0 ),
			DesignRecircFrac( 0.0 ),
			RecircFLTCurve( 0 ),
			DesignUPSEfficiency( 0.0 ),
			UPSEfficFPLRCurve( 0 ),
			UPSLossToZoneFrac( 0.0 ),
			EMSCPUPowerOverrideOn( false ),
			EMSCPUPower( 0.0 ),
			EMSFanPowerOverrideOn( false ),
			EMSFanPower( 0.0 ),
			EMSUPSPowerOverrideOn( false ),
			EMSUPSPower( 0.0 ),
			CPUPower( 0.0 ),
			FanPower( 0.0 ),
			UPSPower( 0.0 ),
			CPUPowerAtDesign( 0.0 ),
			FanPowerAtDesign( 0.0 ),
			UPSGainRateToZone( 0.0 ),
			ConGainRateToZone( 0.0 ),
			CPUConsumption( 0.0 ),
			FanConsumption( 0.0 ),
			UPSConsumption( 0.0 ),
			CPUEnergyAtDesign( 0.0 ),
			FanEnergyAtDesign( 0.0 ),
			UPSGainEnergyToZone( 0.0 ),
			ConGainEnergyToZone( 0.0 ),
			AirVolFlowStdDensity( 0.0 ),
			AirVolFlowCurDensity( 0.0 ),
			AirMassFlow( 0.0 ),
			AirInletDryBulbT( 0.0 ),
			AirInletDewpointT( 0.0 ),
			AirInletRelHum( 0.0 ),
			AirOutletDryBulbT( 0.0 ),
			SHI( 0.0 ),
			TimeOutOfOperRange( 0.0 ),
			TimeAboveDryBulbT( 0.0 ),
			TimeBelowDryBulbT( 0.0 ),
			TimeAboveDewpointT( 0.0 ),
			TimeBelowDewpointT( 0.0 ),
			TimeAboveRH( 0.0 ),
			TimeBelowRH( 0.0 ),
			DryBulbTAboveDeltaT( 0.0 ),
			DryBulbTBelowDeltaT( 0.0 ),
			DewpointTAboveDeltaT( 0.0 ),
			DewpointTBelowDeltaT( 0.0 ),
			RHAboveDeltaRH( 0.0 ),
			RHBelowDeltaRH( 0.0 )
		{}

		// Member Constructor
		ITEquipData(
			std::string const & Name, // EQUIPMENT object name
			int const ZonePtr, // Which zone internal gain is in
			Real64 const DesignTotalPower, // Design level for internal gain [W]
			Real64 const NomMinDesignLevel, // Nominal Minimum Design Level (min sch X design level)
			Real64 const NomMaxDesignLevel, // Nominal Maximum Design Level (max sch X design level)
			Real64 const DesignFanPowerFrac, // Fraction (0.0-1.0) of design power level that is fans
			int const OperSchedPtr, // Schedule pointer for design power input or operating schedule
			int const CPULoadSchedPtr, // Schedule pointer for CPU loading schedule
			Real64 const DesignTAirIn, // Design entering air temperature [C]
			Real64 const DesignFanPower, // Design fan power input [W]
			Real64 const DesignCPUPower, // Design CPU power input [W]
			Real64 const DesignAirVolFlowRate, // Design air volume flow rate [m3/s]
			int const Class, // Environmental class index (A1=1, A2=2, A3=3, A4=4, B=5, C=6)
			int const AirFlowFLTCurve, // Index for airflow function of CPULoadFrac (x) and TAirIn (y) curve
			int const CPUPowerFLTCurve, // Index for CPU power function of CPULoadFrac (x) and TAirIn (y) curve
			int const FanPowerFFCurve, // Index for fan power function of flow fraction curve
			int const AirConnectionType, // Air connection type (AdjustedSupply, ZoneAirNode, RoomAirModel)
			int InletRoomAirNodeNum, // Room air model node number for air inlet
			int OutletRoomAirNodeNum, // Room air model node number for air outlet
			int SupplyAirNodeNum, // Node number for supply air inlet
			Real64 const DesignRecircFrac, // Design recirculation fraction (0.0-0.5)
			int const RecircFLTCurve, // Index for recirculation function of CPULoadFrac (x) and TAirIn (y) curve
			Real64 const DesignUPSEfficiency, // Design power supply efficiency (>0.0 - 1.0)
			int const UPSEfficFPLRCurve, // Index for recirculation function of part load ratio
			Real64 const UPSLossToZoneFrac, // Fraction of UPS power loss to zone (0.0 - 1.0), remainder is lost
			std::string const & EndUseSubcategoryCPU, // user defined name for the end use category for the CPU
			std::string const & EndUseSubcategoryFan, // user defined name for the end use category for the Fans
			std::string const & EndUseSubcategoryUPS, // user defined name for the end use category for the power supply
			bool const EMSCPUPowerOverrideOn, // EMS actuating CPU power if .TRUE.
			Real64 const EMSCPUPower, // Value EMS is directing to use for override of CPU power [W]
			bool const EMSFanPowerOverrideOn, // EMS actuating Fan power if .TRUE.
			Real64 const EMSFanPower, // Value EMS is directing to use for override of Fan power [W]
			bool const EMSUPSPowerOverrideOn, // EMS actuating UPS power if .TRUE.
			Real64 const EMSUPSPower, // Value EMS is directing to use for override of UPS power [W]
			// Report variables
			Real64 const CPUPower, // ITE CPU Electric Power [W]
			Real64 const FanPower, // ITE Fan Electric Power [W]
			Real64 const UPSPower, // ITE UPS Electric Power [W]
			Real64 const CPUPowerAtDesign, // ITE CPU Electric Power at Design Inlet Conditions [W]
			Real64 const FanPowerAtDesign, // ITE Fan Electric Power at Design Inlet Conditions [W]
			Real64 const UPSGainRateToZone, // ITE UPS Heat Gain to Zone Rate [W] - convective gain
			Real64 const ConGainRateToZone, // ITE Total Heat Gain to Zone Rate [W] - convective gain - includes heat gain from UPS, plus CPU and Fans if room air model not used
			Real64 const CPUConsumption, // ITE CPU Electric Energy [J]
			Real64 const FanConsumption, // ITE Fan Electric Energy [J]
			Real64 const UPSConsumption, // ITE UPS Electric Energy [J]
			Real64 const CPUEnergyAtDesign, // ITE CPU Electric Energy at Design Inlet Conditions [J]
			Real64 const FanEnergyAtDesign, // ITE Fan Electric Energy at Design Inlet Conditions [J]
			Real64 const UPSGainEnergyToZone, // ITE UPS Heat Gain to Zone Energy [J] - convective gain
			Real64 const ConGainEnergyToZone, // ITE Total Heat Gain to Zone Energy [J] - convective gain - includes heat gain from UPS, plus CPU and Fans if room air model not used
			Real64 const AirVolFlowStdDensity, // Air volume flow rate at standard density [m3/s]
			Real64 const AirVolFlowCurDensity, // Air volume flow rate at current density [m3/s]
			Real64 const AirMassFlow, // Air mass flow rate [kg/s]
			Real64 const AirInletDryBulbT, // Air inlet dry-bulb temperature [C]
			Real64 const AirInletDewpointT, // Air inlet dewpoit temperature [C]
			Real64 const AirInletRelHum, // Air inlet relative humidity [%]
			Real64 const AirOutletDryBulbT, // Air outlet dry-bulb temperature [C]
			Real64 const SHI, // Supply Heat Index []
			Real64 const TimeOutOfOperRange, // ITE Air Inlet Operating Range Exceeded Time [hr]
			Real64 const TimeAboveDryBulbT, // ITE Air Inlet Dry-Bulb Temperature Above Operating Range Time [hr]
			Real64 const TimeBelowDryBulbT, // ITE Air Inlet Dry-Bulb Temperature Below Operating Range Time [hr]
			Real64 const TimeAboveDewpointT, // ITE Air Inlet Dewpoint Temperature Above Operating Range Time [hr]
			Real64 const TimeBelowDewpointT, // ITE Air Inlet Dewpoint Temperature Below Operating Range Time [hr]
			Real64 const TimeAboveRH, // ITE Air Inlet Relative Humidity Above Operating Range Time [hr]
			Real64 const TimeBelowRH, // ITE Air Inlet Relative Humidity Below Operating Range Time [hr]
			Real64 const DryBulbTAboveDeltaT, // ITE Air Inlet Dry-Bulb Temperature Difference Above Operating Range [deltaC]
			Real64 const DryBulbTBelowDeltaT, // ITE Air Inlet Dry-Bulb Temperature Difference Below Operating Range [deltaC]
			Real64 const DewpointTAboveDeltaT, // ITE Air Inlet Dewpoint Temperature Difference Above Operating Range [deltaC]
			Real64 const DewpointTBelowDeltaT, // ITE Air Inlet Dewpoint Temperature Difference Below Operating Range [deltaC]
			Real64 RHAboveDeltaRH, // ITE Air Inlet Relative Humidity Difference Above Operating Range [%]
			Real64 RHBelowDeltaRH // ITE Air Inlet Relative Humidity Difference Below Operating Range [%]
			) :
			Name( Name ),
			ZonePtr( ZonePtr ),
			DesignTotalPower( DesignTotalPower ),
			NomMinDesignLevel( NomMinDesignLevel ),
			NomMaxDesignLevel( NomMaxDesignLevel ),
			DesignFanPowerFrac( DesignFanPowerFrac ),
			OperSchedPtr( OperSchedPtr ),
			CPULoadSchedPtr( CPULoadSchedPtr ),
			DesignTAirIn( DesignTAirIn ),
			DesignFanPower( DesignFanPower ),
			DesignCPUPower( DesignCPUPower ),
			DesignAirVolFlowRate( DesignAirVolFlowRate ),
			Class( Class ),
			AirFlowFLTCurve( AirFlowFLTCurve ),
			CPUPowerFLTCurve( CPUPowerFLTCurve ),
			FanPowerFFCurve( FanPowerFFCurve ),
			AirConnectionType( AirConnectionType ),
			InletRoomAirNodeNum( InletRoomAirNodeNum ),
			OutletRoomAirNodeNum( OutletRoomAirNodeNum ),
			SupplyAirNodeNum( SupplyAirNodeNum ),
			DesignRecircFrac( DesignRecircFrac ),
			RecircFLTCurve( RecircFLTCurve ),
			DesignUPSEfficiency( DesignUPSEfficiency ),
			UPSEfficFPLRCurve( UPSEfficFPLRCurve ),
			UPSLossToZoneFrac( UPSLossToZoneFrac ),
			EndUseSubcategoryCPU( EndUseSubcategoryCPU ),
			EndUseSubcategoryFan( EndUseSubcategoryFan ),
			EndUseSubcategoryUPS( EndUseSubcategoryUPS ),
			EMSCPUPowerOverrideOn( EMSCPUPowerOverrideOn ),
			EMSCPUPower( EMSCPUPower ),
			EMSFanPowerOverrideOn( EMSFanPowerOverrideOn ),
			EMSFanPower( EMSFanPower ),
			EMSUPSPowerOverrideOn( EMSUPSPowerOverrideOn ),
			EMSUPSPower( EMSUPSPower ),
			CPUPower( CPUPower ),
			FanPower( FanPower ),
			UPSPower( UPSPower ),
			CPUPowerAtDesign( CPUPowerAtDesign ),
			FanPowerAtDesign( FanPowerAtDesign ),
			UPSGainRateToZone( UPSGainRateToZone ),
			ConGainRateToZone( ConGainRateToZone ),
			CPUConsumption( CPUConsumption ),
			FanConsumption( FanConsumption ),
			UPSConsumption( UPSConsumption ),
			CPUEnergyAtDesign( CPUEnergyAtDesign ),
			FanEnergyAtDesign( FanEnergyAtDesign ),
			UPSGainEnergyToZone( UPSGainEnergyToZone ),
			ConGainEnergyToZone( ConGainEnergyToZone ),
			AirVolFlowStdDensity( AirVolFlowStdDensity ),
			AirVolFlowCurDensity( AirVolFlowCurDensity ),
			AirMassFlow( AirMassFlow ),
			AirInletDryBulbT( AirInletDryBulbT ),
			AirInletDewpointT( AirInletDewpointT ),
			AirInletRelHum( AirInletRelHum ),
			AirOutletDryBulbT( AirOutletDryBulbT ),
			SHI( SHI ),
			TimeOutOfOperRange( TimeOutOfOperRange ),
			TimeAboveDryBulbT( TimeAboveDryBulbT ),
			TimeBelowDryBulbT( TimeBelowDryBulbT ),
			TimeAboveDewpointT( TimeAboveDewpointT ),
			TimeBelowDewpointT( TimeBelowDewpointT ),
			TimeAboveRH( TimeAboveRH ),
			TimeBelowRH( TimeBelowRH ),
			DryBulbTAboveDeltaT( DryBulbTAboveDeltaT ),
			DryBulbTBelowDeltaT( DryBulbTBelowDeltaT ),
			DewpointTAboveDeltaT( DewpointTAboveDeltaT ),
			DewpointTBelowDeltaT( DewpointTBelowDeltaT ),
			RHAboveDeltaRH( RHAboveDeltaRH ),
			RHBelowDeltaRH( RHBelowDeltaRH )
			{}

	};

	struct BBHeatData
	{
		// Members
		std::string Name; // BASEBOARD HEAT object name
		int ZonePtr;
		int SchedPtr;
		Real64 CapatLowTemperature;
		Real64 LowTemperature;
		Real64 CapatHighTemperature;
		Real64 HighTemperature;
		bool EMSZoneBaseboardOverrideOn; // EMS actuating equipment power if .TRUE.
		Real64 EMSZoneBaseboardPower; // Value EMS is directing to use for override
		Real64 FractionRadiant;
		Real64 FractionConvected;
		bool ManageDemand; // Flag to indicate whether to use demand limiting
		Real64 DemandLimit; // Demand limit set by demand manager [W]
		// Report variables
		Real64 Power; // Electric power [W]
		Real64 RadGainRate; // Radiant heat gain [W]
		Real64 ConGainRate; // Convective heat gain [W]
		Real64 TotGainRate; // Total heat gain [W]
		Real64 Consumption; // Electric consumption [J]
		Real64 RadGainEnergy; // Radiant heat gain [J]
		Real64 ConGainEnergy; // Convective heat gain [J]
		Real64 TotGainEnergy; // Total heat gain [J]
		std::string EndUseSubcategory; // user defined name for the end use category

		// Default Constructor
		BBHeatData() :
			ZonePtr( 0 ),
			SchedPtr( 0 ),
			CapatLowTemperature( 0.0 ),
			LowTemperature( 0.0 ),
			CapatHighTemperature( 0.0 ),
			HighTemperature( 0.0 ),
			EMSZoneBaseboardOverrideOn( false ),
			EMSZoneBaseboardPower( 0.0 ),
			FractionRadiant( 0.0 ),
			FractionConvected( 0.0 ),
			ManageDemand( false ),
			DemandLimit( 0.0 ),
			Power( 0.0 ),
			RadGainRate( 0.0 ),
			ConGainRate( 0.0 ),
			TotGainRate( 0.0 ),
			Consumption( 0.0 ),
			RadGainEnergy( 0.0 ),
			ConGainEnergy( 0.0 ),
			TotGainEnergy( 0.0 )
		{}

		// Member Constructor
		BBHeatData(
			std::string const & Name, // BASEBOARD HEAT object name
			int const ZonePtr,
			int const SchedPtr,
			Real64 const CapatLowTemperature,
			Real64 const LowTemperature,
			Real64 const CapatHighTemperature,
			Real64 const HighTemperature,
			bool const EMSZoneBaseboardOverrideOn, // EMS actuating equipment power if .TRUE.
			Real64 const EMSZoneBaseboardPower, // Value EMS is directing to use for override
			Real64 const FractionRadiant,
			Real64 const FractionConvected,
			bool const ManageDemand, // Flag to indicate whether to use demand limiting
			Real64 const DemandLimit, // Demand limit set by demand manager [W]
			Real64 const Power, // Electric power [W]
			Real64 const RadGainRate, // Radiant heat gain [W]
			Real64 const ConGainRate, // Convective heat gain [W]
			Real64 const TotGainRate, // Total heat gain [W]
			Real64 const Consumption, // Electric consumption [J]
			Real64 const RadGainEnergy, // Radiant heat gain [J]
			Real64 const ConGainEnergy, // Convective heat gain [J]
			Real64 const TotGainEnergy, // Total heat gain [J]
			std::string const & EndUseSubcategory // user defined name for the end use category
		) :
			Name( Name ),
			ZonePtr( ZonePtr ),
			SchedPtr( SchedPtr ),
			CapatLowTemperature( CapatLowTemperature ),
			LowTemperature( LowTemperature ),
			CapatHighTemperature( CapatHighTemperature ),
			HighTemperature( HighTemperature ),
			EMSZoneBaseboardOverrideOn( EMSZoneBaseboardOverrideOn ),
			EMSZoneBaseboardPower( EMSZoneBaseboardPower ),
			FractionRadiant( FractionRadiant ),
			FractionConvected( FractionConvected ),
			ManageDemand( ManageDemand ),
			DemandLimit( DemandLimit ),
			Power( Power ),
			RadGainRate( RadGainRate ),
			ConGainRate( ConGainRate ),
			TotGainRate( TotGainRate ),
			Consumption( Consumption ),
			RadGainEnergy( RadGainEnergy ),
			ConGainEnergy( ConGainEnergy ),
			TotGainEnergy( TotGainEnergy ),
			EndUseSubcategory( EndUseSubcategory )
		{}

	};

	struct InfiltrationData
	{
		// Members
		std::string Name;
		int ZonePtr; // Which zone infiltration is in
		int SchedPtr; // Schedule for infiltration
		int ModelType; // which model is used for infiltration
		// Design Flow Rate model terms
		Real64 DesignLevel;
		Real64 ConstantTermCoef;
		Real64 TemperatureTermCoef;
		Real64 VelocityTermCoef;
		Real64 VelocitySQTermCoef;
		// Effective Leakage Area, Sherman Grimsrud terms
		Real64 LeakageArea; // "AL" effective air leakage area
		Real64 BasicStackCoefficient; // "Cs" Stack coefficient
		Real64 BasicWindCoefficient; // "Cw" wind coefficient
		// Flow Coefficient, AIM-2, Walker and Wilson terms
		Real64 FlowCoefficient; // "c" Flow coefficient
		Real64 AIM2StackCoefficient; // "Cs" stack coefficient
		Real64 AIM2WindCoefficient; // "Cw" wind coefficient
		Real64 PressureExponent; // "n" pressure power law exponent
		Real64 ShelterFactor; // "s" shelter factor
		bool EMSOverrideOn; // if true then EMS is requesting to override
		Real64 EMSAirFlowRateValue; // value EMS is setting for air flow rate
		bool QuadratureSum; // If quadrature sum of zone air balance method is used
		int OABalancePtr; // A pointer to ZoneAirBalance If quadrature is true
		Real64 VolumeFlowRate; // infiltration air volume flow rate
		Real64 MassFlowRate;   // infiltration air mass flow rate

		// Default Constructor
		InfiltrationData() :
			ZonePtr( 0 ),
			SchedPtr( 0 ),
			ModelType( 0 ),
			DesignLevel( 0.0 ),
			ConstantTermCoef( 0.0 ),
			TemperatureTermCoef( 0.0 ),
			VelocityTermCoef( 0.0 ),
			VelocitySQTermCoef( 0.0 ),
			LeakageArea( 0.0 ),
			BasicStackCoefficient( 0.0 ),
			BasicWindCoefficient( 0.0 ),
			FlowCoefficient( 0.0 ),
			AIM2StackCoefficient( 0.0 ),
			AIM2WindCoefficient( 0.0 ),
			PressureExponent( 0.0 ),
			ShelterFactor( 0.0 ),
			EMSOverrideOn( false ),
			EMSAirFlowRateValue( 0.0 ),
			QuadratureSum( false ),
			OABalancePtr( 0 ),
			VolumeFlowRate( 0.0 ),
			MassFlowRate( 0.0 )
		{}

		// Member Constructor
		InfiltrationData(
			std::string const & Name,
			int const ZonePtr, // Which zone infiltration is in
			int const SchedPtr, // Schedule for infiltration
			int const ModelType, // which model is used for infiltration
			Real64 const DesignLevel,
			Real64 const ConstantTermCoef,
			Real64 const TemperatureTermCoef,
			Real64 const VelocityTermCoef,
			Real64 const VelocitySQTermCoef,
			Real64 const LeakageArea, // "AL" effective air leakage area
			Real64 const BasicStackCoefficient, // "Cs" Stack coefficient
			Real64 const BasicWindCoefficient, // "Cw" wind coefficient
			Real64 const FlowCoefficient, // "c" Flow coefficient
			Real64 const AIM2StackCoefficient, // "Cs" stack coefficient
			Real64 const AIM2WindCoefficient, // "Cw" wind coefficient
			Real64 const PressureExponent, // "n" pressure power law exponent
			Real64 const ShelterFactor, // "s" shelter factor
			bool const EMSOverrideOn, // if true then EMS is requesting to override
			Real64 const EMSAirFlowRateValue, // value EMS is setting for air flow rate
			bool const QuadratureSum, // If quadrature sum of zone air balance method is used
			int const OABalancePtr, // A pointer to ZoneAirBalance If quadrature is true
			Real64 const VolumeFlowRate, // infiltration air volume flow rate
			Real64 const MassFlowRate   // infiltration air mass flow rate
		) :
			Name( Name ),
			ZonePtr( ZonePtr ),
			SchedPtr( SchedPtr ),
			ModelType( ModelType ),
			DesignLevel( DesignLevel ),
			ConstantTermCoef( ConstantTermCoef ),
			TemperatureTermCoef( TemperatureTermCoef ),
			VelocityTermCoef( VelocityTermCoef ),
			VelocitySQTermCoef( VelocitySQTermCoef ),
			LeakageArea( LeakageArea ),
			BasicStackCoefficient( BasicStackCoefficient ),
			BasicWindCoefficient( BasicWindCoefficient ),
			FlowCoefficient( FlowCoefficient ),
			AIM2StackCoefficient( AIM2StackCoefficient ),
			AIM2WindCoefficient( AIM2WindCoefficient ),
			PressureExponent( PressureExponent ),
			ShelterFactor( ShelterFactor ),
			EMSOverrideOn( EMSOverrideOn ),
			EMSAirFlowRateValue( EMSAirFlowRateValue ),
			QuadratureSum( QuadratureSum ),
			OABalancePtr( OABalancePtr ),
			VolumeFlowRate( VolumeFlowRate ),
			MassFlowRate( MassFlowRate )
		{}

	};

	struct VentilationData
	{
		// Members
		std::string Name;
		int ZonePtr;
		int SchedPtr;
		int ModelType; // which model is used for ventilation: DesignFlowRate and WindandStackOpenArea
		Real64 DesignLevel;
		bool EMSSimpleVentOn; // EMS actuating ventilation flow rate if .TRUE.
		Real64 EMSimpleVentFlowRate; // Value EMS is directing to use for override
		Real64 MinIndoorTemperature;
		Real64 DelTemperature;
		int FanType;
		Real64 FanPressure;
		Real64 FanEfficiency;
		Real64 FanPower;
		Real64 AirTemp;
		Real64 ConstantTermCoef;
		Real64 TemperatureTermCoef;
		Real64 VelocityTermCoef;
		Real64 VelocitySQTermCoef;
		Real64 MaxIndoorTemperature;
		Real64 MinOutdoorTemperature;
		Real64 MaxOutdoorTemperature;
		Real64 MaxWindSpeed;
		int MinIndoorTempSchedPtr; // Minimum indoor temperature schedule index
		int MaxIndoorTempSchedPtr; // Maximum indoor temperature schedule index
		int DeltaTempSchedPtr; // Delta temperature schedule index
		int MinOutdoorTempSchedPtr; // Minimum outdoor temperature schedule index
		int MaxOutdoorTempSchedPtr; // Maximum outdoor temperature schedule index
		int IndoorTempErrCount; // Indoor temperature error count
		int OutdoorTempErrCount; // Outdoor temperature error count
		int IndoorTempErrIndex; // Indoor temperature error Index
		int OutdoorTempErrIndex; // Outdoor temperature error Index
		int HybridControlType; // Hybrid ventilation control type: 0 Individual, 1 Close, 2 Global
		int HybridControlMasterNum; // Hybrid ventilation control master object number
		bool HybridControlMasterStatus; // Hybrid ventilation control master object opening status
		bool QuadratureSum; // If quadrature sum of zone air balance method is used
		int OABalancePtr; // A pointer to ZoneAirBalance
		// WindandStackOpenArea
		Real64 OpenArea; // Opening area [m2]
		int OpenAreaSchedPtr; // Opening area fraction schedule pointer
		Real64 OpenEff; // Opening effectiveness [dimensionless]
		Real64 EffAngle; // Effective angle [degree]
		Real64 DH; // Height difference [m]
		Real64 DiscCoef; // Discharge coefficient

		// Default Constructor
		VentilationData() :
			ZonePtr( 0 ),
			SchedPtr( 0 ),
			ModelType( 0 ),
			DesignLevel( 0.0 ),
			EMSSimpleVentOn( false ),
			EMSimpleVentFlowRate( 0.0 ),
			MinIndoorTemperature( -100.0 ),
			DelTemperature( 0.0 ),
			FanType( 0 ),
			FanPressure( 0.0 ),
			FanEfficiency( 0.0 ),
			FanPower( 0.0 ),
			AirTemp( 0.0 ),
			ConstantTermCoef( 0.0 ),
			TemperatureTermCoef( 0.0 ),
			VelocityTermCoef( 0.0 ),
			VelocitySQTermCoef( 0.0 ),
			MaxIndoorTemperature( 100.0 ),
			MinOutdoorTemperature( -100.0 ),
			MaxOutdoorTemperature( 100.0 ),
			MaxWindSpeed( 40.0 ),
			MinIndoorTempSchedPtr( 0 ),
			MaxIndoorTempSchedPtr( 0 ),
			DeltaTempSchedPtr( 0 ),
			MinOutdoorTempSchedPtr( 0 ),
			MaxOutdoorTempSchedPtr( 0 ),
			IndoorTempErrCount( 0 ),
			OutdoorTempErrCount( 0 ),
			IndoorTempErrIndex( 0 ),
			OutdoorTempErrIndex( 0 ),
			HybridControlType( 0 ),
			HybridControlMasterNum( 0 ),
			HybridControlMasterStatus( false ),
			QuadratureSum( false ),
			OABalancePtr( 0 ),
			OpenArea( 0.0 ),
			OpenAreaSchedPtr( 0 ),
			OpenEff( 0.0 ),
			EffAngle( 0.0 ),
			DH( 0.0 ),
			DiscCoef( 0.0 )
		{}

		// Member Constructor
		VentilationData(
			std::string const & Name,
			int const ZonePtr,
			int const SchedPtr,
			int const ModelType, // which model is used for ventilation: DesignFlowRate and WindandStackOpenArea
			Real64 const DesignLevel,
			bool const EMSSimpleVentOn, // EMS actuating ventilation flow rate if .TRUE.
			Real64 const EMSimpleVentFlowRate, // Value EMS is directing to use for override
			Real64 const MinIndoorTemperature,
			Real64 const DelTemperature,
			int const FanType,
			Real64 const FanPressure,
			Real64 const FanEfficiency,
			Real64 const FanPower,
			Real64 const AirTemp,
			Real64 const ConstantTermCoef,
			Real64 const TemperatureTermCoef,
			Real64 const VelocityTermCoef,
			Real64 const VelocitySQTermCoef,
			Real64 const MaxIndoorTemperature,
			Real64 const MinOutdoorTemperature,
			Real64 const MaxOutdoorTemperature,
			Real64 const MaxWindSpeed,
			int const MinIndoorTempSchedPtr, // Minimum indoor temperature schedule index
			int const MaxIndoorTempSchedPtr, // Maximum indoor temperature schedule index
			int const DeltaTempSchedPtr, // Delta temperature schedule index
			int const MinOutdoorTempSchedPtr, // Minimum outdoor temperature schedule index
			int const MaxOutdoorTempSchedPtr, // Maximum outdoor temperature schedule index
			int const IndoorTempErrCount, // Indoor temperature error count
			int const OutdoorTempErrCount, // Outdoor temperature error count
			int const IndoorTempErrIndex, // Indoor temperature error Index
			int const OutdoorTempErrIndex, // Outdoor temperature error Index
			int const HybridControlType, // Hybrid ventilation control type: 0 Individual, 1 Close, 2 Global
			int const HybridControlMasterNum, // Hybrid ventilation control master object number
			bool const HybridControlMasterStatus, // Hybrid ventilation control master object opening status
			bool const QuadratureSum, // If quadrature sum of zone air balance method is used
			int const OABalancePtr, // A pointer to ZoneAirBalance
			Real64 const OpenArea, // Opening area [m2]
			int const OpenAreaSchedPtr, // Opening area fraction schedule pointer
			Real64 const OpenEff, // Opening effectiveness [dimensionless]
			Real64 const EffAngle, // Effective angle [degree]
			Real64 const DH, // Height difference [m]
			Real64 const DiscCoef // Discharge coefficient
		) :
			Name( Name ),
			ZonePtr( ZonePtr ),
			SchedPtr( SchedPtr ),
			ModelType( ModelType ),
			DesignLevel( DesignLevel ),
			EMSSimpleVentOn( EMSSimpleVentOn ),
			EMSimpleVentFlowRate( EMSimpleVentFlowRate ),
			MinIndoorTemperature( MinIndoorTemperature ),
			DelTemperature( DelTemperature ),
			FanType( FanType ),
			FanPressure( FanPressure ),
			FanEfficiency( FanEfficiency ),
			FanPower( FanPower ),
			AirTemp( AirTemp ),
			ConstantTermCoef( ConstantTermCoef ),
			TemperatureTermCoef( TemperatureTermCoef ),
			VelocityTermCoef( VelocityTermCoef ),
			VelocitySQTermCoef( VelocitySQTermCoef ),
			MaxIndoorTemperature( MaxIndoorTemperature ),
			MinOutdoorTemperature( MinOutdoorTemperature ),
			MaxOutdoorTemperature( MaxOutdoorTemperature ),
			MaxWindSpeed( MaxWindSpeed ),
			MinIndoorTempSchedPtr( MinIndoorTempSchedPtr ),
			MaxIndoorTempSchedPtr( MaxIndoorTempSchedPtr ),
			DeltaTempSchedPtr( DeltaTempSchedPtr ),
			MinOutdoorTempSchedPtr( MinOutdoorTempSchedPtr ),
			MaxOutdoorTempSchedPtr( MaxOutdoorTempSchedPtr ),
			IndoorTempErrCount( IndoorTempErrCount ),
			OutdoorTempErrCount( OutdoorTempErrCount ),
			IndoorTempErrIndex( IndoorTempErrIndex ),
			OutdoorTempErrIndex( OutdoorTempErrIndex ),
			HybridControlType( HybridControlType ),
			HybridControlMasterNum( HybridControlMasterNum ),
			HybridControlMasterStatus( HybridControlMasterStatus ),
			QuadratureSum( QuadratureSum ),
			OABalancePtr( OABalancePtr ),
			OpenArea( OpenArea ),
			OpenAreaSchedPtr( OpenAreaSchedPtr ),
			OpenEff( OpenEff ),
			EffAngle( EffAngle ),
			DH( DH ),
			DiscCoef( DiscCoef )
		{}

	};

	struct ZoneAirBalanceData
	{
		// Members
		std::string Name; // Object name
		std::string ZoneName; // Zone name
		int ZonePtr; // Zone number
		int BalanceMethod; // Air Balance Method: None=0, Quadrature = 1
		Real64 InducedAirRate; // Induced Outdoor Air Due to Duct Leakage Unbalance [m3/s]
		int InducedAirSchedPtr; // Induced Outdoor Air Fraction Schedule
		Real64 BalMassFlowRate; // balanced mass flow rate
		Real64 InfMassFlowRate; // unbalanced mass flow rate from infiltration
		Real64 NatMassFlowRate; // unbalanced mass flow rate from natural ventilaton
		Real64 ExhMassFlowRate; // unbalanced mass flow rate from exhaust ventilaton
		Real64 IntMassFlowRate; // unbalanced mass flow rate from intake ventilaton
		Real64 ERVMassFlowRate; // unbalanced mass flow rate from stand-alond ERV
		bool OneTimeFlag; // One time flag to get nodes of stand alond ERV
		int NumOfERVs; // Number of zone stand alone ERVs
		Array1D_int ERVInletNode; // Stand alone ERV supply air inlet nodes
		Array1D_int ERVExhaustNode; // Stand alone ERV air exhaust nodes

		// Default Constructor
		ZoneAirBalanceData() :
			ZonePtr( 0 ),
			BalanceMethod( 0 ),
			InducedAirRate( 0.0 ),
			InducedAirSchedPtr( 0 ),
			BalMassFlowRate( 0.0 ),
			InfMassFlowRate( 0.0 ),
			NatMassFlowRate( 0.0 ),
			ExhMassFlowRate( 0.0 ),
			IntMassFlowRate( 0.0 ),
			ERVMassFlowRate( 0.0 ),
			OneTimeFlag( false ),
			NumOfERVs( 0 )
		{}

		// Member Constructor
		ZoneAirBalanceData(
			std::string const & Name, // Object name
			std::string const & ZoneName, // Zone name
			int const ZonePtr, // Zone number
			int const BalanceMethod, // Air Balance Method: None=0, Quadrature = 1
			Real64 const InducedAirRate, // Induced Outdoor Air Due to Duct Leakage Unbalance [m3/s]
			int const InducedAirSchedPtr, // Induced Outdoor Air Fraction Schedule
			Real64 const BalMassFlowRate, // balanced mass flow rate
			Real64 const InfMassFlowRate, // unbalanced mass flow rate from infiltration
			Real64 const NatMassFlowRate, // unbalanced mass flow rate from natural ventilaton
			Real64 const ExhMassFlowRate, // unbalanced mass flow rate from exhaust ventilaton
			Real64 const IntMassFlowRate, // unbalanced mass flow rate from intake ventilaton
			Real64 const ERVMassFlowRate, // unbalanced mass flow rate from stand-alond ERV
			bool const OneTimeFlag, // One time flag to get nodes of stand alond ERV
			int const NumOfERVs, // Number of zone stand alone ERVs
			Array1_int const & ERVInletNode, // Stand alone ERV supply air inlet nodes
			Array1_int const & ERVExhaustNode // Stand alone ERV air exhaust nodes
		) :
			Name( Name ),
			ZoneName( ZoneName ),
			ZonePtr( ZonePtr ),
			BalanceMethod( BalanceMethod ),
			InducedAirRate( InducedAirRate ),
			InducedAirSchedPtr( InducedAirSchedPtr ),
			BalMassFlowRate( BalMassFlowRate ),
			InfMassFlowRate( InfMassFlowRate ),
			NatMassFlowRate( NatMassFlowRate ),
			ExhMassFlowRate( ExhMassFlowRate ),
			IntMassFlowRate( IntMassFlowRate ),
			ERVMassFlowRate( ERVMassFlowRate ),
			OneTimeFlag( OneTimeFlag ),
			NumOfERVs( NumOfERVs ),
			ERVInletNode( ERVInletNode ),
			ERVExhaustNode( ERVExhaustNode )
		{}

	};

	struct MixingData
	{
		// Members
		std::string Name;
		int ZonePtr;
		int SchedPtr;
		Real64 DesignLevel;
		int FromZone;
		Real64 DeltaTemperature;
		Real64 DesiredAirFlowRate;
		Real64 DesiredAirFlowRateSaved;
		Real64 MixingMassFlowRate;
		int DeltaTempSchedPtr; // Delta temperature schedule index
		int MinIndoorTempSchedPtr; // Minimum indoor temperature schedule index
		int MaxIndoorTempSchedPtr; // Maximum indoor temperature schedule index
		int MinSourceTempSchedPtr; // Minimum source zone temperature schedule index
		int MaxSourceTempSchedPtr; // Maximum source zone temperature schedule index
		int MinOutdoorTempSchedPtr; // Minimum outdoor temperature schedule index
		int MaxOutdoorTempSchedPtr; // Maximum outdoor temperature schedule index
		int IndoorTempErrCount; // Indoor temperature error count
		int SourceTempErrCount; // Source zone temperature error count
		int OutdoorTempErrCount; // Outdoor temperature error count
		int IndoorTempErrIndex; // Indoor temperature error Index
		int SourceTempErrIndex; // Source zone temperature error Index
		int OutdoorTempErrIndex; // Outdoor temperature error Index
		int HybridControlType; // Hybrid ventilation control type: 0 Individual, 1 Close, 2 Global
		int HybridControlMasterNum; // Hybrid ventilation control master ventilation object number
		int NumRefDoorConnections;
		bool EMSSimpleMixingOn; // EMS actuating ventilation flow rate if .TRUE.
		bool RefDoorMixFlag; // Refrigeration door mixing within zone
		Real64 EMSimpleMixingFlowRate; // Value EMS is directing to use for override
		Array1D_bool EMSRefDoorMixingOn;
		Array1D< Real64 > EMSRefDoorFlowRate;
		Array1D< Real64 > VolRefDoorFlowRate;
		Array1D_int OpenSchedPtr; // Schedule for Refrigeration door open fraction
		Array1D< Real64 > DoorHeight; // Door height for refrigeration door, m
		Array1D< Real64 > DoorArea; // Door area for refrigeration door, m2
		Array1D< Real64 > Protection; // Refrigeration door protection factor, dimensionless
		Array1D_int MateZonePtr; // Zone connected by refrigeration door (MateZone > ZonePtr)
		Array1D_string DoorMixingObjectName; // Used in one error statement and eio
		Array1D_string DoorProtTypeName; // Used in eio
		//Note, for mixing and crossmixing, this type dimensioned by number of mixing objects.
		//For ref door mixing, dimensioned by number of zones.

		// Default Constructor
		MixingData() :
			ZonePtr( 0 ),
			SchedPtr( 0 ),
			DesignLevel( 0.0 ),
			FromZone( 0 ),
			DeltaTemperature( 0.0 ),
			DesiredAirFlowRate( 0.0 ),
			DesiredAirFlowRateSaved( 0.0 ),
			MixingMassFlowRate( 0.0 ),
			DeltaTempSchedPtr( 0 ),
			MinIndoorTempSchedPtr( 0 ),
			MaxIndoorTempSchedPtr( 0 ),
			MinSourceTempSchedPtr( 0 ),
			MaxSourceTempSchedPtr( 0 ),
			MinOutdoorTempSchedPtr( 0 ),
			MaxOutdoorTempSchedPtr( 0 ),
			IndoorTempErrCount( 0 ),
			SourceTempErrCount( 0 ),
			OutdoorTempErrCount( 0 ),
			IndoorTempErrIndex( 0 ),
			SourceTempErrIndex( 0 ),
			OutdoorTempErrIndex( 0 ),
			HybridControlType( 0 ),
			HybridControlMasterNum( 0 ),
			NumRefDoorConnections( 0 ),
			EMSSimpleMixingOn( false ),
			RefDoorMixFlag( false ),
			EMSimpleMixingFlowRate( 0.0 )
		{}

		// Member Constructor
		MixingData(
			std::string const & Name,
			int const ZonePtr,
			int const SchedPtr,
			Real64 const DesignLevel,
			int const FromZone,
			Real64 const DeltaTemperature,
			Real64 const DesiredAirFlowRate,
			Real64 const DesiredAirFlowRateSaved,
			Real64 const MixingMassFlowRate,
			int const DeltaTempSchedPtr, // Delta temperature schedule index
			int const MinIndoorTempSchedPtr, // Minimum indoor temperature schedule index
			int const MaxIndoorTempSchedPtr, // Maximum indoor temperature schedule index
			int const MinSourceTempSchedPtr, // Minimum source zone temperature schedule index
			int const MaxSourceTempSchedPtr, // Maximum source zone temperature schedule index
			int const MinOutdoorTempSchedPtr, // Minimum outdoor temperature schedule index
			int const MaxOutdoorTempSchedPtr, // Maximum outdoor temperature schedule index
			int const IndoorTempErrCount, // Indoor temperature error count
			int const SourceTempErrCount, // Source zone temperature error count
			int const OutdoorTempErrCount, // Outdoor temperature error count
			int const IndoorTempErrIndex, // Indoor temperature error Index
			int const SourceTempErrIndex, // Source zone temperature error Index
			int const OutdoorTempErrIndex, // Outdoor temperature error Index
			int const HybridControlType, // Hybrid ventilation control type: 0 Individual, 1 Close, 2 Global
			int const HybridControlMasterNum, // Hybrid ventilation control master ventilation object number
			int const NumRefDoorConnections,
			bool const EMSSimpleMixingOn, // EMS actuating ventilation flow rate if .TRUE.
			bool const RefDoorMixFlag, // Refrigeration door mixing within zone
			Real64 const EMSimpleMixingFlowRate, // Value EMS is directing to use for override
			Array1_bool const & EMSRefDoorMixingOn,
			Array1< Real64 > const & EMSRefDoorFlowRate,
			Array1< Real64 > const & VolRefDoorFlowRate,
			Array1_int const & OpenSchedPtr, // Schedule for Refrigeration door open fraction
			Array1< Real64 > const & DoorHeight, // Door height for refrigeration door, m
			Array1< Real64 > const & DoorArea, // Door area for refrigeration door, m2
			Array1< Real64 > const & Protection, // Refrigeration door protection factor, dimensionless
			Array1_int const & MateZonePtr, // Zone connected by refrigeration door (MateZone > ZonePtr)
			Array1_string const & DoorMixingObjectName, // Used in one error statement and eio
			Array1_string const & DoorProtTypeName // Used in eio
		) :
			Name( Name ),
			ZonePtr( ZonePtr ),
			SchedPtr( SchedPtr ),
			DesignLevel( DesignLevel ),
			FromZone( FromZone ),
			DeltaTemperature( DeltaTemperature ),
			DesiredAirFlowRate( DesiredAirFlowRate ),
			DesiredAirFlowRateSaved( DesiredAirFlowRateSaved ),
			MixingMassFlowRate( MixingMassFlowRate ),
			DeltaTempSchedPtr( DeltaTempSchedPtr ),
			MinIndoorTempSchedPtr( MinIndoorTempSchedPtr ),
			MaxIndoorTempSchedPtr( MaxIndoorTempSchedPtr ),
			MinSourceTempSchedPtr( MinSourceTempSchedPtr ),
			MaxSourceTempSchedPtr( MaxSourceTempSchedPtr ),
			MinOutdoorTempSchedPtr( MinOutdoorTempSchedPtr ),
			MaxOutdoorTempSchedPtr( MaxOutdoorTempSchedPtr ),
			IndoorTempErrCount( IndoorTempErrCount ),
			SourceTempErrCount( SourceTempErrCount ),
			OutdoorTempErrCount( OutdoorTempErrCount ),
			IndoorTempErrIndex( IndoorTempErrIndex ),
			SourceTempErrIndex( SourceTempErrIndex ),
			OutdoorTempErrIndex( OutdoorTempErrIndex ),
			HybridControlType( HybridControlType ),
			HybridControlMasterNum( HybridControlMasterNum ),
			NumRefDoorConnections( NumRefDoorConnections ),
			EMSSimpleMixingOn( EMSSimpleMixingOn ),
			RefDoorMixFlag( RefDoorMixFlag ),
			EMSimpleMixingFlowRate( EMSimpleMixingFlowRate ),
			EMSRefDoorMixingOn( EMSRefDoorMixingOn ),
			EMSRefDoorFlowRate( EMSRefDoorFlowRate ),
			VolRefDoorFlowRate( VolRefDoorFlowRate ),
			OpenSchedPtr( OpenSchedPtr ),
			DoorHeight( DoorHeight ),
			DoorArea( DoorArea ),
			Protection( Protection ),
			MateZonePtr( MateZonePtr ),
			DoorMixingObjectName( DoorMixingObjectName ),
			DoorProtTypeName( DoorProtTypeName )
		{}

	};

	struct ZoneAirMassFlowConservation
	{
		// Members
		bool EnforceZoneMassBalance;     // flag to enforce zone air mass conservation
		int InfiltrationTreatment;       // determines how infiltration is treated for zone mass balance
		//Note, unique global object

		// Default Constructor
		ZoneAirMassFlowConservation() :
			EnforceZoneMassBalance( false ),
			InfiltrationTreatment( 0 )
		{}

		// Member Constructor
		ZoneAirMassFlowConservation(
			bool EnforceZoneMassBalance,
			int InfiltrationTreatment
			) :
			EnforceZoneMassBalance( EnforceZoneMassBalance ),
			InfiltrationTreatment( InfiltrationTreatment )
		{}
	};


	struct ZoneMassConservationData
	{
		// Members
		std::string Name;
		int ZonePtr;             // pointer to the mixing zone
		Real64 InMassFlowRate;   // zone total supply air mass flow rate, kg/s
		Real64 ExhMassFlowRate;  // zone exhaust total air mass flow rate, kg/s
		Real64 RetMassFlowRate;  // zone return air mass flow rate, kg/s
		Real64 MixingMassFlowRate;        // zone mixing air mass flow rate, kg/s
		Real64 MixingSourceMassFlowRate;  // Zone source mass flow rate for mixing zone, kg/s
		int NumSourceZonesMixingObject;   // number of zone mixing object references as a source zone
		int NumReceivingZonesMixingObject;  // number of zone mixing object references as a receiving zone
		bool IsOnlySourceZone; // true only if used only as a source zone in zone mixing object
		int InfiltrationPtr;             // pointer to infiltration object
		Real64 InfiltrationMassFlowRate;   // infiltration added to enforced source zone mass balance, kg/s
		int IncludeInfilToZoneMassBal;     // not self-balanced, include infiltration in zone air mass balance
		Array1D_int ZoneMixingSourcesPtr;     // source zones pointer
		Array1D_int ZoneMixingReceivingPtr;   // receiving zones pointer
		Array1D< Real64 > ZoneMixingReceivingFr; // receiving zones fraction
		//Note, this type dimensioned by number of zones

		// Default Constructor
		ZoneMassConservationData() :
			ZonePtr(0),
			InMassFlowRate(0.0),
			ExhMassFlowRate(0.0),
			RetMassFlowRate(0.0),
			MixingMassFlowRate(0.0),
			MixingSourceMassFlowRate(0.0),
			NumSourceZonesMixingObject(0),
			NumReceivingZonesMixingObject(0),
			IsOnlySourceZone(false),
			InfiltrationPtr(0),
			InfiltrationMassFlowRate(0.0),
			IncludeInfilToZoneMassBal(0)
		{}

		// Member Constructor
		ZoneMassConservationData(
			std::string const & Name,
			int const ZonePtr,
			Real64 const InMassFlowRate,
			Real64 const ExhMassFlowRate,
			Real64 const RetMassFlowRate,
			Real64 const MixingMassFlowRate,
			Real64 const MixingSourceMassFlowRate,
			int const NumSourceZonesMixingObject,
			int const NumReceivingZonesMixingObject,
			bool const IsOnlySourceZone,
			int const InfiltrationPtr,
			Real64 const InfiltrationMassFlowRate,
			int const IncludeInfilToZoneMassBal,
			Array1_int const & ZoneMixingSourcesPtr,
			Array1_int const & ZoneMixingReceivingPtr,
			Array1< Real64 > const & ZoneMixingReceivingFr
			) :
			Name(Name),
			ZonePtr(ZonePtr),
			InMassFlowRate(InMassFlowRate),
			ExhMassFlowRate(ExhMassFlowRate),
			RetMassFlowRate(RetMassFlowRate),
			MixingMassFlowRate(MixingMassFlowRate),
			MixingSourceMassFlowRate(MixingSourceMassFlowRate),
			NumSourceZonesMixingObject(NumSourceZonesMixingObject),
			NumReceivingZonesMixingObject(NumReceivingZonesMixingObject),
			IsOnlySourceZone(IsOnlySourceZone),
			InfiltrationPtr(InfiltrationPtr),
			InfiltrationMassFlowRate(InfiltrationMassFlowRate),
			IncludeInfilToZoneMassBal(IncludeInfilToZoneMassBal),
			ZoneMixingSourcesPtr(ZoneMixingSourcesPtr),
			ZoneMixingReceivingPtr(ZoneMixingReceivingPtr),
			ZoneMixingReceivingFr(ZoneMixingReceivingFr)
		{}
	};

	struct GenericComponentZoneIntGainStruct
	{
		// Members
		std::string CompObjectType; // device object class name
		std::string CompObjectName; // device user unique name
		int CompTypeOfNum; // type of internal gain device identifier
		Reference< Real64 > PtrConvectGainRate; // fortan POINTER to value of convection heat gain rate for device, watts
		Real64 ConvectGainRate; // current timestep value of convection heat gain rate for device, watts
		Reference< Real64 > PtrReturnAirConvGainRate; // fortan POINTER to value of return air convection heat gain rate for device, W
		Real64 ReturnAirConvGainRate; // urrent timestep value of return air convection heat gain rate for device, W
		Reference< Real64 > PtrRadiantGainRate; // fortan POINTER to value of thermal radiation heat gain rate for device, watts
		Real64 RadiantGainRate; // current timestep value of thermal radiation heat gain rate for device, watts
		Reference< Real64 > PtrLatentGainRate; // fortan POINTER to value of moisture gain rate for device, Watts
		Real64 LatentGainRate; // current timestep value of moisture gain rate for device, Watts
		Reference< Real64 > PtrReturnAirLatentGainRate; // fortan POINTER to value of return air moisture gain rate for device, Watts
		Real64 ReturnAirLatentGainRate; // current timestep value of return air moisture gain rate for device, Watts
		Reference< Real64 > PtrCarbonDioxideGainRate; // fortan POINTER to value of carbon dioxide gain rate for device
		Real64 CarbonDioxideGainRate; // current timestep value of carbon dioxide gain rate for device
		Reference< Real64 > PtrGenericContamGainRate; // fortan POINTER to value of generic contaminant gain rate for device
		Real64 GenericContamGainRate; // current timestep value of generic contaminant gain rate for device

		// Default Constructor
		GenericComponentZoneIntGainStruct() :
			CompTypeOfNum( 0 ),
			ConvectGainRate( 0.0 ), //Autodesk:Init Zero initializations for Real64 members added to fix use uninitialized: Such use probably is a logic bug that still needs fixing
			ReturnAirConvGainRate( 0.0 ),
			RadiantGainRate( 0.0 ),
			LatentGainRate( 0.0 ),
			ReturnAirLatentGainRate( 0.0 ),
			CarbonDioxideGainRate( 0.0 ),
			GenericContamGainRate( 0.0 )
		{}

		// Member Constructor
		GenericComponentZoneIntGainStruct(
			std::string const & CompObjectType, // device object class name
			std::string const & CompObjectName, // device user unique name
			int const CompTypeOfNum, // type of internal gain device identifier
			Reference< Real64 > const PtrConvectGainRate, // fortan POINTER to value of convection heat gain rate for device, watts
			Real64 const ConvectGainRate, // current timestep value of convection heat gain rate for device, watts
			Reference< Real64 > const PtrReturnAirConvGainRate, // fortan POINTER to value of return air convection heat gain rate for device, W
			Real64 const ReturnAirConvGainRate, // urrent timestep value of return air convection heat gain rate for device, W
			Reference< Real64 > const PtrRadiantGainRate, // fortan POINTER to value of thermal radiation heat gain rate for device, watts
			Real64 const RadiantGainRate, // current timestep value of thermal radiation heat gain rate for device, watts
			Reference< Real64 > const PtrLatentGainRate, // fortan POINTER to value of moisture gain rate for device, Watts
			Real64 const LatentGainRate, // current timestep value of moisture gain rate for device, Watts
			Reference< Real64 > const PtrReturnAirLatentGainRate, // fortan POINTER to value of return air moisture gain rate for device, Watts
			Real64 const ReturnAirLatentGainRate, // current timestep value of return air moisture gain rate for device, Watts
			Reference< Real64 > const PtrCarbonDioxideGainRate, // fortan POINTER to value of carbon dioxide gain rate for device
			Real64 const CarbonDioxideGainRate, // current timestep value of carbon dioxide gain rate for device
			Reference< Real64 > const PtrGenericContamGainRate, // fortan POINTER to value of generic contaminant gain rate for device
			Real64 const GenericContamGainRate // current timestep value of generic contaminant gain rate for device
		) :
			CompObjectType( CompObjectType ),
			CompObjectName( CompObjectName ),
			CompTypeOfNum( CompTypeOfNum ),
			PtrConvectGainRate( PtrConvectGainRate ),
			ConvectGainRate( ConvectGainRate ),
			PtrReturnAirConvGainRate( PtrReturnAirConvGainRate ),
			ReturnAirConvGainRate( ReturnAirConvGainRate ),
			PtrRadiantGainRate( PtrRadiantGainRate ),
			RadiantGainRate( RadiantGainRate ),
			PtrLatentGainRate( PtrLatentGainRate ),
			LatentGainRate( LatentGainRate ),
			PtrReturnAirLatentGainRate( PtrReturnAirLatentGainRate ),
			ReturnAirLatentGainRate( ReturnAirLatentGainRate ),
			PtrCarbonDioxideGainRate( PtrCarbonDioxideGainRate ),
			CarbonDioxideGainRate( CarbonDioxideGainRate ),
			PtrGenericContamGainRate( PtrGenericContamGainRate ),
			GenericContamGainRate( GenericContamGainRate )
		{}

	};

	struct ZoneSimData // Calculated data by Zone during each time step/hour
	{
		// Members
		Real64 NOFOCC; // Number of Occupants, zone total
		Real64 QOCTOT; // Total Energy from Occupants
		Real64 QOCSEN; // Sensible Energy from Occupants
		Real64 QOCCON; // ENERGY CONVECTED FROM OCCUPANTS (WH)
		Real64 QOCRAD; // ENERGY RADIATED FROM OCCUPANTS
		Real64 QOCLAT; // LATENT ENERGY FROM OCCUPANTS
		Real64 QLTTOT; // TOTAL ENERGY INTO LIGHTS (WH)
		Real64 QLTCON; // ENERGY CONVECTED TO SPACE AIR FROM LIGHTS
		Real64 QLTRAD; // ENERGY RADIATED TO SPACE FROM LIGHTS
		Real64 QLTCRA; // ENERGY CONVECTED TO RETURN AIR FROM LIGHTS
		Real64 QLTSW; // VISIBLE ENERGY FROM LIGHTS
		Real64 QEECON; // ENERGY CONVECTED FROM ELECTRIC EQUIPMENT
		Real64 QEERAD; // ENERCY RADIATED FROM ELECTRIC EQUIPMENT
		Real64 QEELost; // Energy from Electric Equipment (lost)
		Real64 QEELAT; // LATENT ENERGY FROM Electric Equipment
		Real64 QGECON; // ENERGY CONVECTED FROM GAS EQUIPMENT
		Real64 QGERAD; // ENERGY RADIATED FROM GAS EQUIPMENT
		Real64 QGELost; // Energy from Gas Equipment (lost)
		Real64 QGELAT; // LATENT ENERGY FROM Gas Equipment
		Real64 QOECON; // ENERGY CONVECTED FROM OTHER EQUIPMENT
		Real64 QOERAD; // ENERGY RADIATED FROM OTHER EQUIPMENT
		Real64 QOELost; // Energy from Other Equipment (lost)
		Real64 QOELAT; // LATENT ENERGY FROM Other Equipment
		Real64 QHWCON; // ENERGY CONVECTED FROM Hot Water EQUIPMENT
		Real64 QHWRAD; // ENERGY RADIATED FROM Hot Water EQUIPMENT
		Real64 QHWLost; // Energy from Hot Water Equipment (lost)
		Real64 QHWLAT; // LATENT ENERGY FROM Hot Water Equipment
		Real64 QSECON; // ENERGY CONVECTED FROM Steam EQUIPMENT
		Real64 QSERAD; // ENERGY RADIATED FROM Steam EQUIPMENT
		Real64 QSELost; // Energy from Steam Equipment (lost)
		Real64 QSELAT; // LATENT ENERGY FROM Steam Equipment
		Real64 QBBCON; // ENERGY CONVECTED FROM BASEBOARD HEATING
		Real64 QBBRAD; // ENERGY RADIATED FROM BASEBOARD HEATING
		int NumberOfDevices;
		int MaxNumberOfDevices;
		Array1D< GenericComponentZoneIntGainStruct > Device;

		// Default Constructor
		ZoneSimData() :
			NOFOCC( 0.0 ),
			QOCTOT( 0.0 ),
			QOCSEN( 0.0 ),
			QOCCON( 0.0 ),
			QOCRAD( 0.0 ),
			QOCLAT( 0.0 ),
			QLTTOT( 0.0 ),
			QLTCON( 0.0 ),
			QLTRAD( 0.0 ),
			QLTCRA( 0.0 ),
			QLTSW( 0.0 ),
			QEECON( 0.0 ),
			QEERAD( 0.0 ),
			QEELost( 0.0 ),
			QEELAT( 0.0 ),
			QGECON( 0.0 ),
			QGERAD( 0.0 ),
			QGELost( 0.0 ),
			QGELAT( 0.0 ),
			QOECON( 0.0 ),
			QOERAD( 0.0 ),
			QOELost( 0.0 ),
			QOELAT( 0.0 ),
			QHWCON( 0.0 ),
			QHWRAD( 0.0 ),
			QHWLost( 0.0 ),
			QHWLAT( 0.0 ),
			QSECON( 0.0 ),
			QSERAD( 0.0 ),
			QSELost( 0.0 ),
			QSELAT( 0.0 ),
			QBBCON( 0.0 ),
			QBBRAD( 0.0 ),
			NumberOfDevices( 0 ),
			MaxNumberOfDevices( 0 )
		{}

		// Member Constructor
		ZoneSimData(
			Real64 const NOFOCC, // Number of Occupants, zone total
			Real64 const QOCTOT, // Total Energy from Occupants
			Real64 const QOCSEN, // Sensible Energy from Occupants
			Real64 const QOCCON, // ENERGY CONVECTED FROM OCCUPANTS (WH)
			Real64 const QOCRAD, // ENERGY RADIATED FROM OCCUPANTS
			Real64 const QOCLAT, // LATENT ENERGY FROM OCCUPANTS
			Real64 const QLTTOT, // TOTAL ENERGY INTO LIGHTS (WH)
			Real64 const QLTCON, // ENERGY CONVECTED TO SPACE AIR FROM LIGHTS
			Real64 const QLTRAD, // ENERGY RADIATED TO SPACE FROM LIGHTS
			Real64 const QLTCRA, // ENERGY CONVECTED TO RETURN AIR FROM LIGHTS
			Real64 const QLTSW, // VISIBLE ENERGY FROM LIGHTS
			Real64 const QEECON, // ENERGY CONVECTED FROM ELECTRIC EQUIPMENT
			Real64 const QEERAD, // ENERCY RADIATED FROM ELECTRIC EQUIPMENT
			Real64 const QEELost, // Energy from Electric Equipment (lost)
			Real64 const QEELAT, // LATENT ENERGY FROM Electric Equipment
			Real64 const QGECON, // ENERGY CONVECTED FROM GAS EQUIPMENT
			Real64 const QGERAD, // ENERGY RADIATED FROM GAS EQUIPMENT
			Real64 const QGELost, // Energy from Gas Equipment (lost)
			Real64 const QGELAT, // LATENT ENERGY FROM Gas Equipment
			Real64 const QOECON, // ENERGY CONVECTED FROM OTHER EQUIPMENT
			Real64 const QOERAD, // ENERGY RADIATED FROM OTHER EQUIPMENT
			Real64 const QOELost, // Energy from Other Equipment (lost)
			Real64 const QOELAT, // LATENT ENERGY FROM Other Equipment
			Real64 const QHWCON, // ENERGY CONVECTED FROM Hot Water EQUIPMENT
			Real64 const QHWRAD, // ENERGY RADIATED FROM Hot Water EQUIPMENT
			Real64 const QHWLost, // Energy from Hot Water Equipment (lost)
			Real64 const QHWLAT, // LATENT ENERGY FROM Hot Water Equipment
			Real64 const QSECON, // ENERGY CONVECTED FROM Steam EQUIPMENT
			Real64 const QSERAD, // ENERGY RADIATED FROM Steam EQUIPMENT
			Real64 const QSELost, // Energy from Steam Equipment (lost)
			Real64 const QSELAT, // LATENT ENERGY FROM Steam Equipment
			Real64 const QBBCON, // ENERGY CONVECTED FROM BASEBOARD HEATING
			Real64 const QBBRAD, // ENERGY RADIATED FROM BASEBOARD HEATING
			int const NumberOfDevices,
			int const MaxNumberOfDevices,
			Array1< GenericComponentZoneIntGainStruct > const & Device
		) :
			NOFOCC( NOFOCC ),
			QOCTOT( QOCTOT ),
			QOCSEN( QOCSEN ),
			QOCCON( QOCCON ),
			QOCRAD( QOCRAD ),
			QOCLAT( QOCLAT ),
			QLTTOT( QLTTOT ),
			QLTCON( QLTCON ),
			QLTRAD( QLTRAD ),
			QLTCRA( QLTCRA ),
			QLTSW( QLTSW ),
			QEECON( QEECON ),
			QEERAD( QEERAD ),
			QEELost( QEELost ),
			QEELAT( QEELAT ),
			QGECON( QGECON ),
			QGERAD( QGERAD ),
			QGELost( QGELost ),
			QGELAT( QGELAT ),
			QOECON( QOECON ),
			QOERAD( QOERAD ),
			QOELost( QOELost ),
			QOELAT( QOELAT ),
			QHWCON( QHWCON ),
			QHWRAD( QHWRAD ),
			QHWLost( QHWLost ),
			QHWLAT( QHWLAT ),
			QSECON( QSECON ),
			QSERAD( QSERAD ),
			QSELost( QSELost ),
			QSELAT( QSELAT ),
			QBBCON( QBBCON ),
			QBBRAD( QBBRAD ),
			NumberOfDevices( NumberOfDevices ),
			MaxNumberOfDevices( MaxNumberOfDevices ),
			Device( Device )
		{}

	};

	struct WindowBlindProperties
	{
		// Members
		std::string Name;
		int MaterialNumber; // Material pointer for the blind
		// Input properties
		int SlatOrientation; // HORIZONTAL or VERTICAL
		int SlatAngleType; // FIXED or VARIABLE
		Real64 SlatWidth; // Slat width (m)
		Real64 SlatSeparation; // Slat separation (m)
		Real64 SlatThickness; // Slat thickness (m)
		Real64 SlatCrown; // the height of the slate (length from the chord to the curve)
		Real64 SlatAngle; // Slat angle (deg)
		Real64 MinSlatAngle; // Minimum slat angle for variable-angle slats (deg) (user input)
		Real64 MaxSlatAngle; // Maximum slat angle for variable-angle slats (deg) (user input)
		Real64 SlatConductivity; // Slat conductivity (W/m-K)
		// Solar slat properties
		Real64 SlatTransSolBeamDiff; // Slat solar beam-diffuse transmittance
		Real64 SlatFrontReflSolBeamDiff; // Slat front solar beam-diffuse reflectance
		Real64 SlatBackReflSolBeamDiff; // Slat back solar beam-diffuse reflectance
		Real64 SlatTransSolDiffDiff; // Slat solar diffuse-diffuse transmittance
		Real64 SlatFrontReflSolDiffDiff; // Slat front solar diffuse-diffuse reflectance
		Real64 SlatBackReflSolDiffDiff; // Slat back solar diffuse-diffuse reflectance
		// Visible slat properties
		Real64 SlatTransVisBeamDiff; // Slat visible beam-diffuse transmittance
		Real64 SlatFrontReflVisBeamDiff; // Slat front visible beam-diffuse reflectance
		Real64 SlatBackReflVisBeamDiff; // Slat back visible beam-diffuse reflectance
		Real64 SlatTransVisDiffDiff; // Slat visible diffuse-diffuse transmittance
		Real64 SlatFrontReflVisDiffDiff; // Slat front visible diffuse-diffuse reflectance
		Real64 SlatBackReflVisDiffDiff; // Slat back visible diffuse-diffuse reflectance
		// Long-wave (IR) slat properties
		Real64 SlatTransIR; // Slat IR transmittance
		Real64 SlatFrontEmissIR; // Slat front emissivity
		Real64 SlatBackEmissIR; // Slat back emissivity
		// Some characteristics for blind thermal calculation
		Real64 BlindToGlassDist; // Distance between window shade and adjacent glass (m)
		Real64 BlindTopOpeningMult; // Area of air-flow opening at top of blind, expressed as a fraction
		//  of the blind-to-glass opening area at the top of the blind
		Real64 BlindBottomOpeningMult; // Area of air-flow opening at bottom of blind, expressed as a fraction
		//  of the blind-to-glass opening area at the bottom of the blind
		Real64 BlindLeftOpeningMult; // Area of air-flow opening at left side of blind, expressed as a fraction
		//  of the blind-to-glass opening area at the left side of the blind
		Real64 BlindRightOpeningMult; // Area of air-flow opening at right side of blind, expressed as a fraction
		//  of the blind-to-glass opening area at the right side of the blind
		// Calculated blind properties
		// Blind solar properties
		Array2D< Real64 > SolFrontBeamBeamTrans; // Blind solar front beam-beam transmittance vs.
		// profile angle, slat angle
		Array2D< Real64 > SolFrontBeamBeamRefl; // Blind solar front beam-beam reflectance vs. profile angle,
		// slat angle (zero)
		Array2D< Real64 > SolBackBeamBeamTrans; // Blind solar back beam-beam transmittance vs. profile angle,
		// slat angle
		Array2D< Real64 > SolBackBeamBeamRefl; // Blind solar back beam-beam reflectance vs. profile angle,
		// slat angle (zero)
		Array2D< Real64 > SolFrontBeamDiffTrans; // Blind solar front beam-diffuse transmittance
		// vs. profile angle, slat angle
		Array2D< Real64 > SolFrontBeamDiffRefl; // Blind solar front beam-diffuse reflectance
		// vs. profile angle, slat angle
		Array2D< Real64 > SolBackBeamDiffTrans; // Blind solar back beam-diffuse transmittance
		// vs. profile angle, slat angle
		Array2D< Real64 > SolBackBeamDiffRefl; // Blind solar back beam-diffuse reflectance
		// vs. profile angle, slat angle
		Array1D< Real64 > SolFrontDiffDiffTrans; // Blind solar front diffuse-diffuse transmittance
		// vs. slat angle
		Array1D< Real64 > SolFrontDiffDiffTransGnd; // Blind ground solar front diffuse-diffuse transmittance
		// vs. slat angle
		Array1D< Real64 > SolFrontDiffDiffTransSky; // Blind sky solar front diffuse-diffuse transmittance
		// vs. slat angle
		Array1D< Real64 > SolFrontDiffDiffRefl; // Blind solar front diffuse-diffuse reflectance
		// vs. slat angle
		Array1D< Real64 > SolFrontDiffDiffReflGnd; // Blind ground solar front diffuse-diffuse reflectance
		// vs. slat angle
		Array1D< Real64 > SolFrontDiffDiffReflSky; // Blind sky solar front diffuse-diffuse reflectance
		// vs. slat angle
		Array1D< Real64 > SolBackDiffDiffTrans; // Blind solar back diffuse-diffuse transmittance
		// vs. slat angle
		Array1D< Real64 > SolBackDiffDiffRefl; // Blind solar back diffuse-diffuse reflectance
		// vs. slat angle
		Array2D< Real64 > SolFrontBeamAbs; // Blind solar front beam absorptance vs. slat angle
		Array2D< Real64 > SolBackBeamAbs; // Blind solar back beam absorptance vs. slat angle
		Array1D< Real64 > SolFrontDiffAbs; // Blind solar front diffuse absorptance vs. slat angle
		Array1D< Real64 > SolFrontDiffAbsGnd; // Blind ground solar front diffuse absorptance vs. slat angle
		Array1D< Real64 > SolFrontDiffAbsSky; // Blind sky solar front diffuse absorptance vs. slat angle
		Array1D< Real64 > SolBackDiffAbs; // Blind solar back diffuse absorptance vs. slat angle
		// Blind visible properties
		Array2D< Real64 > VisFrontBeamBeamTrans; // Blind visible front beam-beam transmittance
		// vs. profile angle, slat angle
		Array2D< Real64 > VisFrontBeamBeamRefl; // Blind visible front beam-beam reflectance
		// vs. profile angle, slat angle (zero)
		Array2D< Real64 > VisBackBeamBeamTrans; // Blind visible back beam-beam transmittance
		// vs. profile angle, slat angle
		Array2D< Real64 > VisBackBeamBeamRefl; // Blind visible back beam-beam reflectance
		// vs. profile angle, slat angle (zero)
		Array2D< Real64 > VisFrontBeamDiffTrans; // Blind visible front beam-diffuse transmittance
		// vs. profile angle, slat angle
		Array2D< Real64 > VisFrontBeamDiffRefl; // Blind visible front beam-diffuse reflectance
		// vs. profile angle, slat angle
		Array2D< Real64 > VisBackBeamDiffTrans; // Blind visible back beam-diffuse transmittance
		// vs. profile angle, slat angle
		Array2D< Real64 > VisBackBeamDiffRefl; // Blind visible back beam-diffuse reflectance
		// vs. profile angle, slat angle
		Array1D< Real64 > VisFrontDiffDiffTrans; // Blind visible front diffuse-diffuse transmittance
		// vs. slat angle
		Array1D< Real64 > VisFrontDiffDiffRefl; // Blind visible front diffuse-diffuse reflectance
		// vs. slat angle
		Array1D< Real64 > VisBackDiffDiffTrans; // Blind visible back diffuse-diffuse transmittance
		// vs. slat angle
		Array1D< Real64 > VisBackDiffDiffRefl; // Blind visible back diffuse-diffuse reflectance
		// vs. slat angle
		// Long-wave (IR) blind properties
		Array1D< Real64 > IRFrontTrans; // Blind IR front transmittance vs. slat angle
		Array1D< Real64 > IRFrontEmiss; // Blind IR front emissivity vs. slat angle
		Array1D< Real64 > IRBackTrans; // Blind IR back transmittance vs. slat angle
		Array1D< Real64 > IRBackEmiss; // Blind IR back emissivity vs. slat angle

		// Default Constructor
		WindowBlindProperties() :
			MaterialNumber( 0 ),
			SlatOrientation( 0 ),
			SlatAngleType( FixedSlats ),
			SlatWidth( 0.0 ),
			SlatSeparation( 0.0 ),
			SlatThickness( 0.0 ),
			SlatCrown( 0.0 ),
			SlatAngle( 0.0 ),
			MinSlatAngle( 0.0 ),
			MaxSlatAngle( 0.0 ),
			SlatConductivity( 0.0 ),
			SlatTransSolBeamDiff( 0.0 ),
			SlatFrontReflSolBeamDiff( 0.0 ),
			SlatBackReflSolBeamDiff( 0.0 ),
			SlatTransSolDiffDiff( 0.0 ),
			SlatFrontReflSolDiffDiff( 0.0 ),
			SlatBackReflSolDiffDiff( 0.0 ),
			SlatTransVisBeamDiff( 0.0 ),
			SlatFrontReflVisBeamDiff( 0.0 ),
			SlatBackReflVisBeamDiff( 0.0 ),
			SlatTransVisDiffDiff( 0.0 ),
			SlatFrontReflVisDiffDiff( 0.0 ),
			SlatBackReflVisDiffDiff( 0.0 ),
			SlatTransIR( 0.0 ),
			SlatFrontEmissIR( 0.0 ),
			SlatBackEmissIR( 0.0 ),
			BlindToGlassDist( 0.0 ),
			BlindTopOpeningMult( 0.0 ),
			BlindBottomOpeningMult( 0.0 ),
			BlindLeftOpeningMult( 0.0 ),
			BlindRightOpeningMult( 0.0 ),
			SolFrontBeamBeamTrans( MaxSlatAngs, 37, 0.0 ),
			SolFrontBeamBeamRefl( MaxSlatAngs, 37, 0.0 ),
			SolBackBeamBeamTrans( MaxSlatAngs, 37, 0.0 ),
			SolBackBeamBeamRefl( MaxSlatAngs, 37, 0.0 ),
			SolFrontBeamDiffTrans( MaxSlatAngs, 37, 0.0 ),
			SolFrontBeamDiffRefl( MaxSlatAngs, 37, 0.0 ),
			SolBackBeamDiffTrans( MaxSlatAngs, 37, 0.0 ),
			SolBackBeamDiffRefl( MaxSlatAngs, 37, 0.0 ),
			SolFrontDiffDiffTrans( MaxSlatAngs, 0.0 ),
			SolFrontDiffDiffTransGnd( MaxSlatAngs, 0.0 ),
			SolFrontDiffDiffTransSky( MaxSlatAngs, 0.0 ),
			SolFrontDiffDiffRefl( MaxSlatAngs, 0.0 ),
			SolFrontDiffDiffReflGnd( MaxSlatAngs, 0.0 ),
			SolFrontDiffDiffReflSky( MaxSlatAngs, 0.0 ),
			SolBackDiffDiffTrans( MaxSlatAngs, 0.0 ),
			SolBackDiffDiffRefl( MaxSlatAngs, 0.0 ),
			SolFrontBeamAbs( MaxSlatAngs, 37, 0.0 ),
			SolBackBeamAbs( MaxSlatAngs, 37, 0.0 ),
			SolFrontDiffAbs( MaxSlatAngs, 0.0 ),
			SolFrontDiffAbsGnd( MaxSlatAngs, 0.0 ),
			SolFrontDiffAbsSky( MaxSlatAngs, 0.0 ),
			SolBackDiffAbs( MaxSlatAngs, 0.0 ),
			VisFrontBeamBeamTrans( MaxSlatAngs, 37, 0.0 ),
			VisFrontBeamBeamRefl( MaxSlatAngs, 37, 0.0 ),
			VisBackBeamBeamTrans( MaxSlatAngs, 37, 0.0 ),
			VisBackBeamBeamRefl( MaxSlatAngs, 37, 0.0 ),
			VisFrontBeamDiffTrans( MaxSlatAngs, 37, 0.0 ),
			VisFrontBeamDiffRefl( MaxSlatAngs, 37, 0.0 ),
			VisBackBeamDiffTrans( MaxSlatAngs, 37, 0.0 ),
			VisBackBeamDiffRefl( MaxSlatAngs, 37, 0.0 ),
			VisFrontDiffDiffTrans( MaxSlatAngs, 0.0 ),
			VisFrontDiffDiffRefl( MaxSlatAngs, 0.0 ),
			VisBackDiffDiffTrans( MaxSlatAngs, 0.0 ),
			VisBackDiffDiffRefl( MaxSlatAngs, 0.0 ),
			IRFrontTrans( MaxSlatAngs, 0.0 ),
			IRFrontEmiss( MaxSlatAngs, 0.0 ),
			IRBackTrans( MaxSlatAngs, 0.0 ),
			IRBackEmiss( MaxSlatAngs, 0.0 )
		{}

		// Member Constructor
		WindowBlindProperties(
			std::string const & Name,
			int const MaterialNumber, // Material pointer for the blind
			int const SlatOrientation, // HORIZONTAL or VERTICAL
			int const SlatAngleType, // FIXED or VARIABLE
			Real64 const SlatWidth, // Slat width (m)
			Real64 const SlatSeparation, // Slat separation (m)
			Real64 const SlatThickness, // Slat thickness (m)
			Real64 const SlatCrown, // the height of the slate (length from the chord to the curve)
			Real64 const SlatAngle, // Slat angle (deg)
			Real64 const MinSlatAngle, // Minimum slat angle for variable-angle slats (deg) (user input)
			Real64 const MaxSlatAngle, // Maximum slat angle for variable-angle slats (deg) (user input)
			Real64 const SlatConductivity, // Slat conductivity (W/m-K)
			Real64 const SlatTransSolBeamDiff, // Slat solar beam-diffuse transmittance
			Real64 const SlatFrontReflSolBeamDiff, // Slat front solar beam-diffuse reflectance
			Real64 const SlatBackReflSolBeamDiff, // Slat back solar beam-diffuse reflectance
			Real64 const SlatTransSolDiffDiff, // Slat solar diffuse-diffuse transmittance
			Real64 const SlatFrontReflSolDiffDiff, // Slat front solar diffuse-diffuse reflectance
			Real64 const SlatBackReflSolDiffDiff, // Slat back solar diffuse-diffuse reflectance
			Real64 const SlatTransVisBeamDiff, // Slat visible beam-diffuse transmittance
			Real64 const SlatFrontReflVisBeamDiff, // Slat front visible beam-diffuse reflectance
			Real64 const SlatBackReflVisBeamDiff, // Slat back visible beam-diffuse reflectance
			Real64 const SlatTransVisDiffDiff, // Slat visible diffuse-diffuse transmittance
			Real64 const SlatFrontReflVisDiffDiff, // Slat front visible diffuse-diffuse reflectance
			Real64 const SlatBackReflVisDiffDiff, // Slat back visible diffuse-diffuse reflectance
			Real64 const SlatTransIR, // Slat IR transmittance
			Real64 const SlatFrontEmissIR, // Slat front emissivity
			Real64 const SlatBackEmissIR, // Slat back emissivity
			Real64 const BlindToGlassDist, // Distance between window shade and adjacent glass (m)
			Real64 const BlindTopOpeningMult, // Area of air-flow opening at top of blind, expressed as a fraction
			Real64 const BlindBottomOpeningMult, // Area of air-flow opening at bottom of blind, expressed as a fraction
			Real64 const BlindLeftOpeningMult, // Area of air-flow opening at left side of blind, expressed as a fraction
			Real64 const BlindRightOpeningMult, // Area of air-flow opening at right side of blind, expressed as a fraction
			Array2< Real64 > const & SolFrontBeamBeamTrans, // Blind solar front beam-beam transmittance vs.
			Array2< Real64 > const & SolFrontBeamBeamRefl, // Blind solar front beam-beam reflectance vs. profile angle,
			Array2< Real64 > const & SolBackBeamBeamTrans, // Blind solar back beam-beam transmittance vs. profile angle,
			Array2< Real64 > const & SolBackBeamBeamRefl, // Blind solar back beam-beam reflectance vs. profile angle,
			Array2< Real64 > const & SolFrontBeamDiffTrans, // Blind solar front beam-diffuse transmittance
			Array2< Real64 > const & SolFrontBeamDiffRefl, // Blind solar front beam-diffuse reflectance
			Array2< Real64 > const & SolBackBeamDiffTrans, // Blind solar back beam-diffuse transmittance
			Array2< Real64 > const & SolBackBeamDiffRefl, // Blind solar back beam-diffuse reflectance
			Array1< Real64 > const & SolFrontDiffDiffTrans, // Blind solar front diffuse-diffuse transmittance
			Array1< Real64 > const & SolFrontDiffDiffTransGnd, // Blind ground solar front diffuse-diffuse transmittance
			Array1< Real64 > const & SolFrontDiffDiffTransSky, // Blind sky solar front diffuse-diffuse transmittance
			Array1< Real64 > const & SolFrontDiffDiffRefl, // Blind solar front diffuse-diffuse reflectance
			Array1< Real64 > const & SolFrontDiffDiffReflGnd, // Blind ground solar front diffuse-diffuse reflectance
			Array1< Real64 > const & SolFrontDiffDiffReflSky, // Blind sky solar front diffuse-diffuse reflectance
			Array1< Real64 > const & SolBackDiffDiffTrans, // Blind solar back diffuse-diffuse transmittance
			Array1< Real64 > const & SolBackDiffDiffRefl, // Blind solar back diffuse-diffuse reflectance
			Array2< Real64 > const & SolFrontBeamAbs, // Blind solar front beam absorptance vs. slat angle
			Array2< Real64 > const & SolBackBeamAbs, // Blind solar back beam absorptance vs. slat angle
			Array1< Real64 > const & SolFrontDiffAbs, // Blind solar front diffuse absorptance vs. slat angle
			Array1< Real64 > const & SolFrontDiffAbsGnd, // Blind ground solar front diffuse absorptance vs. slat angle
			Array1< Real64 > const & SolFrontDiffAbsSky, // Blind sky solar front diffuse absorptance vs. slat angle
			Array1< Real64 > const & SolBackDiffAbs, // Blind solar back diffuse absorptance vs. slat angle
			Array2< Real64 > const & VisFrontBeamBeamTrans, // Blind visible front beam-beam transmittance
			Array2< Real64 > const & VisFrontBeamBeamRefl, // Blind visible front beam-beam reflectance
			Array2< Real64 > const & VisBackBeamBeamTrans, // Blind visible back beam-beam transmittance
			Array2< Real64 > const & VisBackBeamBeamRefl, // Blind visible back beam-beam reflectance
			Array2< Real64 > const & VisFrontBeamDiffTrans, // Blind visible front beam-diffuse transmittance
			Array2< Real64 > const & VisFrontBeamDiffRefl, // Blind visible front beam-diffuse reflectance
			Array2< Real64 > const & VisBackBeamDiffTrans, // Blind visible back beam-diffuse transmittance
			Array2< Real64 > const & VisBackBeamDiffRefl, // Blind visible back beam-diffuse reflectance
			Array1< Real64 > const & VisFrontDiffDiffTrans, // Blind visible front diffuse-diffuse transmittance
			Array1< Real64 > const & VisFrontDiffDiffRefl, // Blind visible front diffuse-diffuse reflectance
			Array1< Real64 > const & VisBackDiffDiffTrans, // Blind visible back diffuse-diffuse transmittance
			Array1< Real64 > const & VisBackDiffDiffRefl, // Blind visible back diffuse-diffuse reflectance
			Array1< Real64 > const & IRFrontTrans, // Blind IR front transmittance vs. slat angle
			Array1< Real64 > const & IRFrontEmiss, // Blind IR front emissivity vs. slat angle
			Array1< Real64 > const & IRBackTrans, // Blind IR back transmittance vs. slat angle
			Array1< Real64 > const & IRBackEmiss // Blind IR back emissivity vs. slat angle
		) :
			Name( Name ),
			MaterialNumber( MaterialNumber ),
			SlatOrientation( SlatOrientation ),
			SlatAngleType( SlatAngleType ),
			SlatWidth( SlatWidth ),
			SlatSeparation( SlatSeparation ),
			SlatThickness( SlatThickness ),
			SlatCrown( SlatCrown ),
			SlatAngle( SlatAngle ),
			MinSlatAngle( MinSlatAngle ),
			MaxSlatAngle( MaxSlatAngle ),
			SlatConductivity( SlatConductivity ),
			SlatTransSolBeamDiff( SlatTransSolBeamDiff ),
			SlatFrontReflSolBeamDiff( SlatFrontReflSolBeamDiff ),
			SlatBackReflSolBeamDiff( SlatBackReflSolBeamDiff ),
			SlatTransSolDiffDiff( SlatTransSolDiffDiff ),
			SlatFrontReflSolDiffDiff( SlatFrontReflSolDiffDiff ),
			SlatBackReflSolDiffDiff( SlatBackReflSolDiffDiff ),
			SlatTransVisBeamDiff( SlatTransVisBeamDiff ),
			SlatFrontReflVisBeamDiff( SlatFrontReflVisBeamDiff ),
			SlatBackReflVisBeamDiff( SlatBackReflVisBeamDiff ),
			SlatTransVisDiffDiff( SlatTransVisDiffDiff ),
			SlatFrontReflVisDiffDiff( SlatFrontReflVisDiffDiff ),
			SlatBackReflVisDiffDiff( SlatBackReflVisDiffDiff ),
			SlatTransIR( SlatTransIR ),
			SlatFrontEmissIR( SlatFrontEmissIR ),
			SlatBackEmissIR( SlatBackEmissIR ),
			BlindToGlassDist( BlindToGlassDist ),
			BlindTopOpeningMult( BlindTopOpeningMult ),
			BlindBottomOpeningMult( BlindBottomOpeningMult ),
			BlindLeftOpeningMult( BlindLeftOpeningMult ),
			BlindRightOpeningMult( BlindRightOpeningMult ),
			SolFrontBeamBeamTrans( MaxSlatAngs, 37, SolFrontBeamBeamTrans ),
			SolFrontBeamBeamRefl( MaxSlatAngs, 37, SolFrontBeamBeamRefl ),
			SolBackBeamBeamTrans( MaxSlatAngs, 37, SolBackBeamBeamTrans ),
			SolBackBeamBeamRefl( MaxSlatAngs, 37, SolBackBeamBeamRefl ),
			SolFrontBeamDiffTrans( MaxSlatAngs, 37, SolFrontBeamDiffTrans ),
			SolFrontBeamDiffRefl( MaxSlatAngs, 37, SolFrontBeamDiffRefl ),
			SolBackBeamDiffTrans( MaxSlatAngs, 37, SolBackBeamDiffTrans ),
			SolBackBeamDiffRefl( MaxSlatAngs, 37, SolBackBeamDiffRefl ),
			SolFrontDiffDiffTrans( MaxSlatAngs, SolFrontDiffDiffTrans ),
			SolFrontDiffDiffTransGnd( MaxSlatAngs, SolFrontDiffDiffTransGnd ),
			SolFrontDiffDiffTransSky( MaxSlatAngs, SolFrontDiffDiffTransSky ),
			SolFrontDiffDiffRefl( MaxSlatAngs, SolFrontDiffDiffRefl ),
			SolFrontDiffDiffReflGnd( MaxSlatAngs, SolFrontDiffDiffReflGnd ),
			SolFrontDiffDiffReflSky( MaxSlatAngs, SolFrontDiffDiffReflSky ),
			SolBackDiffDiffTrans( MaxSlatAngs, SolBackDiffDiffTrans ),
			SolBackDiffDiffRefl( MaxSlatAngs, SolBackDiffDiffRefl ),
			SolFrontBeamAbs( MaxSlatAngs, 37, SolFrontBeamAbs ),
			SolBackBeamAbs( MaxSlatAngs, 37, SolBackBeamAbs ),
			SolFrontDiffAbs( MaxSlatAngs, SolFrontDiffAbs ),
			SolFrontDiffAbsGnd( MaxSlatAngs, SolFrontDiffAbsGnd ),
			SolFrontDiffAbsSky( MaxSlatAngs, SolFrontDiffAbsSky ),
			SolBackDiffAbs( MaxSlatAngs, SolBackDiffAbs ),
			VisFrontBeamBeamTrans( MaxSlatAngs, 37, VisFrontBeamBeamTrans ),
			VisFrontBeamBeamRefl( MaxSlatAngs, 37, VisFrontBeamBeamRefl ),
			VisBackBeamBeamTrans( MaxSlatAngs, 37, VisBackBeamBeamTrans ),
			VisBackBeamBeamRefl( MaxSlatAngs, 37, VisBackBeamBeamRefl ),
			VisFrontBeamDiffTrans( MaxSlatAngs, 37, VisFrontBeamDiffTrans ),
			VisFrontBeamDiffRefl( MaxSlatAngs, 37, VisFrontBeamDiffRefl ),
			VisBackBeamDiffTrans( MaxSlatAngs, 37, VisBackBeamDiffTrans ),
			VisBackBeamDiffRefl( MaxSlatAngs, 37, VisBackBeamDiffRefl ),
			VisFrontDiffDiffTrans( MaxSlatAngs, VisFrontDiffDiffTrans ),
			VisFrontDiffDiffRefl( MaxSlatAngs, VisFrontDiffDiffRefl ),
			VisBackDiffDiffTrans( MaxSlatAngs, VisBackDiffDiffTrans ),
			VisBackDiffDiffRefl( MaxSlatAngs, VisBackDiffDiffRefl ),
			IRFrontTrans( MaxSlatAngs, IRFrontTrans ),
			IRFrontEmiss( MaxSlatAngs, IRFrontEmiss ),
			IRBackTrans( MaxSlatAngs, IRBackTrans ),
			IRBackEmiss( MaxSlatAngs, IRBackEmiss )
		{}

	};

	struct SurfaceScreenProperties
	{
		// Members
		int MaterialNumber; // Material pointer for the screen
		Real64 BmBmTrans; // Beam solar transmittance (dependent on sun angle)
		// (this value can include scattering if the user so chooses)
		Real64 BmBmTransBack; // Beam solar transmittance (dependent on sun angle) from back side of screen
		Real64 BmBmTransVis; // Visible solar transmittance (dependent on sun angle)
		// (this value can include visible scattering if the user so chooses)
		Real64 BmDifTrans; // Beam solar transmitted as diffuse radiation (dependent on sun angle)
		Real64 BmDifTransBack; // Beam solar transmitted as diffuse radiation (dependent on sun angle) from back side
		Real64 BmDifTransVis; // Visible solar transmitted as diffuse radiation (dependent on sun angle)
		// The following reflectance properties are dependent on sun angle:
		Real64 ReflectSolBeamFront; // Beam solar reflected as diffuse radiation when sun is in front of screen
		Real64 ReflectVisBeamFront; // Visible solar reflected as diffuse radiation when sun is in front of screen
		Real64 ReflectSolBeamBack; // Beam solar reflected as diffuse radiation when sun is in back of screen
		Real64 ReflectVisBeamBack; // Visible solar reflected as diffuse radiation when sun is in back of screen
		Real64 AbsorpSolarBeamFront; // Front surface solar beam absorptance
		Real64 AbsorpSolarBeamBack; // Back surface solar beam absorptance
		Real64 DifDifTrans; // Back surface diffuse solar transmitted
		Real64 DifDifTransVis; // Back surface diffuse visible solar transmitted
		Real64 DifScreenAbsorp; // Absorption of diffuse radiation
		Real64 DifReflect; // Back reflection of solar diffuse radiation
		Real64 DifReflectVis; // Back reflection of visible diffuse radiation
		Real64 ReflectScreen; // Screen assembly solar reflectance (user input adjusted for holes in screen)
		Real64 ReflectScreenVis; // Screen assembly visible reflectance (user input adjusted for holes in screen)
		Real64 ReflectCylinder; // Screen material solar reflectance (user input, does not account for holes in screen)
		Real64 ReflectCylinderVis; // Screen material visible reflectance (user input, does not account for holes in screen)
		Real64 ScreenDiameterToSpacingRatio; // ratio of screen material diameter to screen material spacing
		int ScreenBeamReflectanceAccounting; // user specified method of accounting for scattered solar beam

		// Default Constructor
		SurfaceScreenProperties() :
			MaterialNumber( 0 ),
			BmBmTrans( 0.0 ),
			BmBmTransBack( 0.0 ),
			BmBmTransVis( 0.0 ),
			BmDifTrans( 0.0 ),
			BmDifTransBack( 0.0 ),
			BmDifTransVis( 0.0 ),
			ReflectSolBeamFront( 0.0 ),
			ReflectVisBeamFront( 0.0 ),
			ReflectSolBeamBack( 0.0 ),
			ReflectVisBeamBack( 0.0 ),
			AbsorpSolarBeamFront( 0.0 ),
			AbsorpSolarBeamBack( 0.0 ),
			DifDifTrans( 0.0 ),
			DifDifTransVis( 0.0 ),
			DifScreenAbsorp( 0.0 ),
			DifReflect( 0.0 ),
			DifReflectVis( 0.0 ),
			ReflectScreen( 0.0 ),
			ReflectScreenVis( 0.0 ),
			ReflectCylinder( 0.0 ),
			ReflectCylinderVis( 0.0 ),
			ScreenDiameterToSpacingRatio( 0.0 ),
			ScreenBeamReflectanceAccounting( 0 )
		{}

		// Member Constructor
		SurfaceScreenProperties(
			int const MaterialNumber, // Material pointer for the screen
			Real64 const BmBmTrans, // Beam solar transmittance (dependent on sun angle)
			Real64 const BmBmTransBack, // Beam solar transmittance (dependent on sun angle) from back side of screen
			Real64 const BmBmTransVis, // Visible solar transmittance (dependent on sun angle)
			Real64 const BmDifTrans, // Beam solar transmitted as diffuse radiation (dependent on sun angle)
			Real64 const BmDifTransBack, // Beam solar transmitted as diffuse radiation (dependent on sun angle) from back side
			Real64 const BmDifTransVis, // Visible solar transmitted as diffuse radiation (dependent on sun angle)
			Real64 const ReflectSolBeamFront, // Beam solar reflected as diffuse radiation when sun is in front of screen
			Real64 const ReflectVisBeamFront, // Visible solar reflected as diffuse radiation when sun is in front of screen
			Real64 const ReflectSolBeamBack, // Beam solar reflected as diffuse radiation when sun is in back of screen
			Real64 const ReflectVisBeamBack, // Visible solar reflected as diffuse radiation when sun is in back of screen
			Real64 const AbsorpSolarBeamFront, // Front surface solar beam absorptance
			Real64 const AbsorpSolarBeamBack, // Back surface solar beam absorptance
			Real64 const DifDifTrans, // Back surface diffuse solar transmitted
			Real64 const DifDifTransVis, // Back surface diffuse visible solar transmitted
			Real64 const DifScreenAbsorp, // Absorption of diffuse radiation
			Real64 const DifReflect, // Back reflection of solar diffuse radiation
			Real64 const DifReflectVis, // Back reflection of visible diffuse radiation
			Real64 const ReflectScreen, // Screen assembly solar reflectance (user input adjusted for holes in screen)
			Real64 const ReflectScreenVis, // Screen assembly visible reflectance (user input adjusted for holes in screen)
			Real64 const ReflectCylinder, // Screen material solar reflectance (user input, does not account for holes in screen)
			Real64 const ReflectCylinderVis, // Screen material visible reflectance (user input, does not account for holes in screen)
			Real64 const ScreenDiameterToSpacingRatio, // ratio of screen material diameter to screen material spacing
			int const ScreenBeamReflectanceAccounting // user specified method of accounting for scattered solar beam
		) :
			MaterialNumber( MaterialNumber ),
			BmBmTrans( BmBmTrans ),
			BmBmTransBack( BmBmTransBack ),
			BmBmTransVis( BmBmTransVis ),
			BmDifTrans( BmDifTrans ),
			BmDifTransBack( BmDifTransBack ),
			BmDifTransVis( BmDifTransVis ),
			ReflectSolBeamFront( ReflectSolBeamFront ),
			ReflectVisBeamFront( ReflectVisBeamFront ),
			ReflectSolBeamBack( ReflectSolBeamBack ),
			ReflectVisBeamBack( ReflectVisBeamBack ),
			AbsorpSolarBeamFront( AbsorpSolarBeamFront ),
			AbsorpSolarBeamBack( AbsorpSolarBeamBack ),
			DifDifTrans( DifDifTrans ),
			DifDifTransVis( DifDifTransVis ),
			DifScreenAbsorp( DifScreenAbsorp ),
			DifReflect( DifReflect ),
			DifReflectVis( DifReflectVis ),
			ReflectScreen( ReflectScreen ),
			ReflectScreenVis( ReflectScreenVis ),
			ReflectCylinder( ReflectCylinder ),
			ReflectCylinderVis( ReflectCylinderVis ),
			ScreenDiameterToSpacingRatio( ScreenDiameterToSpacingRatio ),
			ScreenBeamReflectanceAccounting( ScreenBeamReflectanceAccounting )
		{}

	};

	struct ScreenTransData
	{
		// Members
		Array2D< Real64 > Trans;
		Array2D< Real64 > Scatt;

		// Default Constructor
		ScreenTransData()
		{}

		// Member Constructor
		ScreenTransData(
			Array2< Real64 > const & Trans,
			Array2< Real64 > const & Scatt
		) :
			Trans( Trans ),
			Scatt( Scatt )
		{}

	};

	struct ZoneCatEUseData
	{
		// Members
		Array1D< Real64 > EEConvected; // Category (0 to 25) Energy Convected from Electric Equipment
		Array1D< Real64 > EERadiated; // Category (0 to 25) Energy Radiated from Electric Equipment
		Array1D< Real64 > EELost; // Category (0 to 25) Energy from Electric Equipment (lost)
		Array1D< Real64 > EELatent; // Category (0 to 25) Latent Energy from Electric Equipment

		// Default Constructor
		ZoneCatEUseData() :
			EEConvected( {0,25}, 0.0 ),
			EERadiated( {0,25}, 0.0 ),
			EELost( {0,25}, 0.0 ),
			EELatent( {0,25}, 0.0 )
		{}

		// Member Constructor
		ZoneCatEUseData(
			Array1< Real64 > const & EEConvected, // Category (0 to 25) Energy Convected from Electric Equipment
			Array1< Real64 > const & EERadiated, // Category (0 to 25) Energy Radiated from Electric Equipment
			Array1< Real64 > const & EELost, // Category (0 to 25) Energy from Electric Equipment (lost)
			Array1< Real64 > const & EELatent // Category (0 to 25) Latent Energy from Electric Equipment
		) :
			EEConvected( {0,25}, EEConvected ),
			EERadiated( {0,25}, EERadiated ),
			EELost( {0,25}, EELost ),
			EELatent( {0,25}, EELatent )
		{}

	};

	struct RefrigCaseCreditData
	{
		// Members
		Real64 SenCaseCreditToZone; // Refrigerated display case sensible energy delivered to zone
		// includes refrigeration pipe and receiver heat exchange with zone
		Real64 LatCaseCreditToZone; // Refrigerated display case latent energy delivered to zone
		Real64 SenCaseCreditToHVAC; // Refrigerated display case sensible energy delivered to HVAC RA duct
		Real64 LatCaseCreditToHVAC; // Refrigerated display case latent energy delivered to HVAC RA duct

		// Default Constructor
		RefrigCaseCreditData() :
			SenCaseCreditToZone( 0.0 ),
			LatCaseCreditToZone( 0.0 ),
			SenCaseCreditToHVAC( 0.0 ),
			LatCaseCreditToHVAC( 0.0 )
		{}

		// Member Constructor
		RefrigCaseCreditData(
			Real64 const SenCaseCreditToZone, // Refrigerated display case sensible energy delivered to zone
			Real64 const LatCaseCreditToZone, // Refrigerated display case latent energy delivered to zone
			Real64 const SenCaseCreditToHVAC, // Refrigerated display case sensible energy delivered to HVAC RA duct
			Real64 const LatCaseCreditToHVAC // Refrigerated display case latent energy delivered to HVAC RA duct
		) :
			SenCaseCreditToZone( SenCaseCreditToZone ),
			LatCaseCreditToZone( LatCaseCreditToZone ),
			SenCaseCreditToHVAC( SenCaseCreditToHVAC ),
			LatCaseCreditToHVAC( LatCaseCreditToHVAC )
		{}

		// Reset to Zeros
		void
		reset()
		{
			SenCaseCreditToZone = 0.0;
			LatCaseCreditToZone = 0.0;
			SenCaseCreditToHVAC = 0.0;
			LatCaseCreditToHVAC = 0.0;
		}

	};

	struct HeatReclaimRefrigeratedRackData
	{
		// Members
		std::string Name; // Name of refrigerated rack
		std::string SourceType; // object type for refrigerated rack
		Real64 AvailCapacity; // Total available heat reclaim capacity
		Real64 UsedWaterHeater; // amount of avail used at plant water heater
		Real64 UsedHVACCoil; // amount of avail used at hvac coil

		// Default Constructor
		HeatReclaimRefrigeratedRackData() :
			AvailCapacity( 0.0 ),
			UsedWaterHeater( 0.0 ),
			UsedHVACCoil( 0.0 )
		{}

		// Member Constructor
		HeatReclaimRefrigeratedRackData(
			std::string const & Name, // Name of refrigerated rack
			std::string const & SourceType, // object type for refrigerated rack
			Real64 const AvailCapacity, // Total available heat reclaim capacity
			Real64 const UsedWaterHeater, // amount of avail used at plant water heater
			Real64 const UsedHVACCoil // amount of avail used at hvac coil
		) :
			Name( Name ),
			SourceType( SourceType ),
			AvailCapacity( AvailCapacity ),
			UsedWaterHeater( UsedWaterHeater ),
			UsedHVACCoil( UsedHVACCoil )
		{}

	};

	struct HeatReclaimRefrigCondenserData
	{
		// Members
		std::string Name; // Name of refrigeration system
		int SourceType; // object type for refrigeration system
		Real64 AvailCapacity; // Total available heat reclaim capacity
		Real64 AvailTemperature; // Temperature of heat reclaim source
		Real64 UsedWaterHeater; // amount of avail used at plant water heater
		Real64 UsedHVACCoil; // amount of avail used at hvac coil

		// Default Constructor
		HeatReclaimRefrigCondenserData() :
			SourceType( 0 ),
			AvailCapacity( 0.0 ),
			AvailTemperature( 0.0 ),
			UsedWaterHeater( 0.0 ),
			UsedHVACCoil( 0.0 )
		{}

		// Member Constructor
		HeatReclaimRefrigCondenserData(
			std::string const & Name, // Name of refrigeration system
			int const SourceType, // object type for refrigeration system
			Real64 const AvailCapacity, // Total available heat reclaim capacity
			Real64 const AvailTemperature, // Temperature of heat reclaim source
			Real64 const UsedWaterHeater, // amount of avail used at plant water heater
			Real64 const UsedHVACCoil // amount of avail used at hvac coil
		) :
			Name( Name ),
			SourceType( SourceType ),
			AvailCapacity( AvailCapacity ),
			AvailTemperature( AvailTemperature ),
			UsedWaterHeater( UsedWaterHeater ),
			UsedHVACCoil( UsedHVACCoil )
		{}

	};

	struct HeatReclaimDXCoilData
	{
		// Members
		std::string Name; // Name of DX Coil
		std::string SourceType; // SourceType for DX Coil
		Real64 AvailCapacity; // Total available heat reclaim capacity

		// Default Constructor
		HeatReclaimDXCoilData() :
			AvailCapacity( 0.0 )
		{}

		// Member Constructor
		HeatReclaimDXCoilData(
			std::string const & Name, // Name of DX Coil
			std::string const & SourceType, // SourceType for DX Coil
			Real64 const AvailCapacity // Total available heat reclaim capacity
		) :
			Name( Name ),
			SourceType( SourceType ),
			AvailCapacity( AvailCapacity )
		{}

	};

	struct AirReportVars
	{
		// Members
		Real64 MeanAirTemp; // Mean Air Temperature {C}
		Real64 OperativeTemp; // Average of Mean Air Temperature {C} and Mean Radiant Temperature {C}
		Real64 MeanAirHumRat; // Mean Air Humidity Ratio {kg/kg} (averaged over zone time step)
		Real64 MeanAirDewPointTemp; // Mean Air Dewpoint Temperature {C}
		Real64 ThermOperativeTemp; // Mix or MRT and MAT for Zone Control:Thermostatic:Operative Temperature {C}
		Real64 InfilHeatGain; // Heat Gain {J} due to infiltration
		Real64 InfilHeatLoss; // Heat Loss {J} due to infiltration
		Real64 InfilLatentGain; // Latent Gain {J} due to infiltration
		Real64 InfilLatentLoss; // Latent Loss {J} due to infiltration
		Real64 InfilTotalGain; // Total Gain {J} due to infiltration (sensible+latent)
		Real64 InfilTotalLoss; // Total Loss {J} due to infiltration (sensible+latent)
		Real64 InfilVolumeCurDensity; // Volume of Air {m3} due to infiltration at current zone air density
		Real64 InfilVolumeStdDensity; // Volume of Air {m3} due to infiltration at standard density (adjusted for elevation)
		Real64 InfilVdotCurDensity; // Volume flow rate of Air {m3/s} due to infiltration at current zone air density
		Real64 InfilVdotStdDensity; // Volume flow rate of Air {m3/s} due to infiltration standard density (adjusted elevation)
		Real64 InfilMass; // Mass of Air {kg} due to infiltration
		Real64 InfilMdot; // Mass flow rate of Air (kg/s) due to infiltration
		Real64 InfilAirChangeRate; // Infiltration air change rate {ach}
		Real64 VentilHeatLoss; // Heat Gain {J} due to ventilation
		Real64 VentilHeatGain; // Heat Loss {J} due to ventilation
		Real64 VentilLatentLoss; // Latent Gain {J} due to ventilation
		Real64 VentilLatentGain; // Latent Loss {J} due to ventilation
		Real64 VentilTotalLoss; // Total Gain {J} due to ventilation
		Real64 VentilTotalGain; // Total Loss {J} due to ventilation
		Real64 VentilVolumeCurDensity; // Volume of Air {m3} due to ventilation at current zone air density
		Real64 VentilVolumeStdDensity; // Volume of Air {m3} due to ventilation at standard density (adjusted for elevation)
		Real64 VentilVdotCurDensity; // Volume flow rate of Air {m3/s} due to ventilation at current zone air density
		Real64 VentilVdotStdDensity; // Volume flowr of Air {m3/s} due to ventilation at standard density (adjusted elevation)
		Real64 VentilMass; // Mass of Air {kg} due to ventilation
		Real64 VentilMdot; // Mass flow rate of Air {kg/s} due to ventilation
		Real64 VentilAirChangeRate; // Ventilation air change rate (ach)
		Real64 VentilFanElec; // Fan Electricity {W} due to ventilation
		Real64 VentilAirTemp; // Air Temp {C} of ventilation
		Real64 MixVolume; // Mixing volume of Air {m3}
		Real64 MixVdotCurDensity; // Mixing volume flow rate of Air {m3/s} at current zone air density
		Real64 MixVdotStdDensity; // Mixing volume flow rate of Air {m3/s} at standard density (adjusted for elevation)
		Real64 MixMass; // Mixing mass of air {kg}
		Real64 MixMdot; // Mixing mass flow rate of air {kg/s}
		Real64 MixHeatLoss; // Heat Gain {J} due to mixing and cross mixing and refrigeration door mixing
		Real64 MixHeatGain; // Heat Loss {J} due to mixing and cross mixing and refrigeration door mixing
		Real64 MixLatentLoss; // Latent Gain {J} due to mixing and cross mixing and refrigeration door mixing
		Real64 MixLatentGain; // Latent Loss {J} due to mixing and cross mixing and refrigeration door mixing
		Real64 MixTotalLoss; // Total Gain {J} due to mixing and cross mixing and refrigeration door mixing
		Real64 MixTotalGain; // Total Loss {J} due to mixing and cross mixing and refrigeration door mixing
		// air heat balance component load summary results
		Real64 SumIntGains; // Zone sum of convective internal gains
		Real64 SumHADTsurfs; // Zone sum of Hc*Area*(Tsurf - Tz)
		Real64 SumMCpDTzones; // zone sum of MassFlowRate*cp*(TremotZone - Tz) transfer air from other zone, Mixing
		Real64 SumMCpDtInfil; // Zone sum of MassFlowRate*Cp*(Tout - Tz) transfer from outside, ventil, earth tube
		Real64 SumMCpDTsystem; // Zone sum of air system MassFlowRate*Cp*(Tsup - Tz)
		Real64 SumNonAirSystem; // Zone sum of system convective gains, collected via NonAirSystemResponse
		Real64 CzdTdt; // Zone air energy storage term.
		Real64 imBalance; // put all terms in eq. 5 on RHS , should be zero
		// for ZoneAirBalance:OutdoorAir object Outputs only
		Real64 OABalanceHeatLoss; // Heat Gain {J} due to OA air balance
		Real64 OABalanceHeatGain; // Heat Loss {J} due to OA air balance
		Real64 OABalanceLatentLoss; // Latent Gain {J} due to OA air balance
		Real64 OABalanceLatentGain; // Latent Loss {J} due to OA air balance
		Real64 OABalanceTotalLoss; // Total Gain {J} due to OA air balance
		Real64 OABalanceTotalGain; // Total Loss {J} due to OA air balance
		Real64 OABalanceVolumeCurDensity; // Volume of Air {m3} due to OA air balance
		// at current zone air density
		Real64 OABalanceVolumeStdDensity; // Volume of Air {m3} due to OA air balance
		// at standard density (adjusted for elevation)
		Real64 OABalanceVdotCurDensity; // Volume flow rate of Air {m3/s} due to OA air balance
		// at current zone air density
		Real64 OABalanceVdotStdDensity; // Volume flow rate of Air {m3/s} due to OA air balance
		// at standard density (adjusted elevation)
		Real64 OABalanceMass; // Mass of Air {kg} due to OA air balance
		Real64 OABalanceMdot; // Mass flow rate of Air {kg/s} due to OA air balance
		Real64 OABalanceAirChangeRate; // OA air balance air change rate (ach)
		Real64 OABalanceFanElec; // Fan Electricity {W} due to OA air balance

		// Default Constructor
		AirReportVars() :
			MeanAirTemp( 0.0 ),
			OperativeTemp( 0.0 ),
			MeanAirHumRat( 0.0 ),
			MeanAirDewPointTemp( 0.0 ),
			ThermOperativeTemp( 0.0 ),
			InfilHeatGain( 0.0 ),
			InfilHeatLoss( 0.0 ),
			InfilLatentGain( 0.0 ),
			InfilLatentLoss( 0.0 ),
			InfilTotalGain( 0.0 ),
			InfilTotalLoss( 0.0 ),
			InfilVolumeCurDensity( 0.0 ),
			InfilVolumeStdDensity( 0.0 ),
			InfilVdotCurDensity( 0.0 ),
			InfilVdotStdDensity( 0.0 ),
			InfilMass( 0.0 ),
			InfilMdot( 0.0 ),
			InfilAirChangeRate( 0.0 ),
			VentilHeatLoss( 0.0 ),
			VentilHeatGain( 0.0 ),
			VentilLatentLoss( 0.0 ),
			VentilLatentGain( 0.0 ),
			VentilTotalLoss( 0.0 ),
			VentilTotalGain( 0.0 ),
			VentilVolumeCurDensity( 0.0 ),
			VentilVolumeStdDensity( 0.0 ),
			VentilVdotCurDensity( 0.0 ),
			VentilVdotStdDensity( 0.0 ),
			VentilMass( 0.0 ),
			VentilMdot( 0.0 ),
			VentilAirChangeRate( 0.0 ),
			VentilFanElec( 0.0 ),
			VentilAirTemp( 0.0 ),
			MixVolume( 0.0 ),
			MixVdotCurDensity( 0.0 ),
			MixVdotStdDensity( 0.0 ),
			MixMass( 0.0 ),
			MixMdot( 0.0 ),
			MixHeatLoss( 0.0 ),
			MixHeatGain( 0.0 ),
			MixLatentLoss( 0.0 ),
			MixLatentGain( 0.0 ),
			MixTotalLoss( 0.0 ),
			MixTotalGain( 0.0 ),
			SumIntGains( 0.0 ),
			SumHADTsurfs( 0.0 ),
			SumMCpDTzones( 0.0 ),
			SumMCpDtInfil( 0.0 ),
			SumMCpDTsystem( 0.0 ),
			SumNonAirSystem( 0.0 ),
			CzdTdt( 0.0 ),
			imBalance( 0.0 ),
			OABalanceHeatLoss( 0.0 ),
			OABalanceHeatGain( 0.0 ),
			OABalanceLatentLoss( 0.0 ),
			OABalanceLatentGain( 0.0 ),
			OABalanceTotalLoss( 0.0 ),
			OABalanceTotalGain( 0.0 ),
			OABalanceVolumeCurDensity( 0.0 ),
			OABalanceVolumeStdDensity( 0.0 ),
			OABalanceVdotCurDensity( 0.0 ),
			OABalanceVdotStdDensity( 0.0 ),
			OABalanceMass( 0.0 ),
			OABalanceMdot( 0.0 ),
			OABalanceAirChangeRate( 0.0 ),
			OABalanceFanElec( 0.0 )
		{}

		// Member Constructor
		AirReportVars(
			Real64 const MeanAirTemp, // Mean Air Temperature {C}
			Real64 const OperativeTemp, // Average of Mean Air Temperature {C} and Mean Radiant Temperature {C}
			Real64 const MeanAirHumRat, // Mean Air Humidity Ratio {kg/kg} (averaged over zone time step)
			Real64 const MeanAirDewPointTemp, // Mean Air Dewpoint Temperature {C}
			Real64 const ThermOperativeTemp, // Mix or MRT and MAT for Zone Control:Thermostatic:Operative Temperature {C}
			Real64 const InfilHeatGain, // Heat Gain {J} due to infiltration
			Real64 const InfilHeatLoss, // Heat Loss {J} due to infiltration
			Real64 const InfilLatentGain, // Latent Gain {J} due to infiltration
			Real64 const InfilLatentLoss, // Latent Loss {J} due to infiltration
			Real64 const InfilTotalGain, // Total Gain {J} due to infiltration (sensible+latent)
			Real64 const InfilTotalLoss, // Total Loss {J} due to infiltration (sensible+latent)
			Real64 const InfilVolumeCurDensity, // Volume of Air {m3} due to infiltration at current zone air density
			Real64 const InfilVolumeStdDensity, // Volume of Air {m3} due to infiltration at standard density (adjusted for elevation)
			Real64 const InfilVdotCurDensity, // Volume flow rate of Air {m3/s} due to infiltration at current zone air density
			Real64 const InfilVdotStdDensity, // Volume flow rate of Air {m3/s} due to infiltration standard density (adjusted elevation)
			Real64 const InfilMass, // Mass of Air {kg} due to infiltration
			Real64 const InfilMdot, // Mass flow rate of Air (kg/s) due to infiltration
			Real64 const InfilAirChangeRate, // Infiltration air change rate {ach}
			Real64 const VentilHeatLoss, // Heat Gain {J} due to ventilation
			Real64 const VentilHeatGain, // Heat Loss {J} due to ventilation
			Real64 const VentilLatentLoss, // Latent Gain {J} due to ventilation
			Real64 const VentilLatentGain, // Latent Loss {J} due to ventilation
			Real64 const VentilTotalLoss, // Total Gain {J} due to ventilation
			Real64 const VentilTotalGain, // Total Loss {J} due to ventilation
			Real64 const VentilVolumeCurDensity, // Volume of Air {m3} due to ventilation at current zone air density
			Real64 const VentilVolumeStdDensity, // Volume of Air {m3} due to ventilation at standard density (adjusted for elevation)
			Real64 const VentilVdotCurDensity, // Volume flow rate of Air {m3/s} due to ventilation at current zone air density
			Real64 const VentilVdotStdDensity, // Volume flowr of Air {m3/s} due to ventilation at standard density (adjusted elevation)
			Real64 const VentilMass, // Mass of Air {kg} due to ventilation
			Real64 const VentilMdot, // Mass flow rate of Air {kg/s} due to ventilation
			Real64 const VentilAirChangeRate, // Ventilation air change rate (ach)
			Real64 const VentilFanElec, // Fan Electricity {W} due to ventilation
			Real64 const VentilAirTemp, // Air Temp {C} of ventilation
			Real64 const MixVolume, // Mixing volume of Air {m3}
			Real64 const MixVdotCurDensity, // Mixing volume flow rate of Air {m3/s} at current zone air density
			Real64 const MixVdotStdDensity, // Mixing volume flow rate of Air {m3/s} at standard density (adjusted for elevation)
			Real64 const MixMass, // Mixing mass of air {kg}
			Real64 const MixMdot, // Mixing mass flow rate of air {kg/s}
			Real64 const MixHeatLoss, // Heat Gain {J} due to mixing and cross mixing and refrigeration door mixing
			Real64 const MixHeatGain, // Heat Loss {J} due to mixing and cross mixing and refrigeration door mixing
			Real64 const MixLatentLoss, // Latent Gain {J} due to mixing and cross mixing and refrigeration door mixing
			Real64 const MixLatentGain, // Latent Loss {J} due to mixing and cross mixing and refrigeration door mixing
			Real64 const MixTotalLoss, // Total Gain {J} due to mixing and cross mixing and refrigeration door mixing
			Real64 const MixTotalGain, // Total Loss {J} due to mixing and cross mixing and refrigeration door mixing
			Real64 const SumIntGains, // Zone sum of convective internal gains
			Real64 const SumHADTsurfs, // Zone sum of Hc*Area*(Tsurf - Tz)
			Real64 const SumMCpDTzones, // zone sum of MassFlowRate*cp*(TremotZone - Tz) transfer air from other zone, Mixing
			Real64 const SumMCpDtInfil, // Zone sum of MassFlowRate*Cp*(Tout - Tz) transfer from outside, ventil, earth tube
			Real64 const SumMCpDTsystem, // Zone sum of air system MassFlowRate*Cp*(Tsup - Tz)
			Real64 const SumNonAirSystem, // Zone sum of system convective gains, collected via NonAirSystemResponse
			Real64 const CzdTdt, // Zone air energy storage term.
			Real64 const imBalance, // put all terms in eq. 5 on RHS , should be zero
			Real64 const OABalanceHeatLoss, // Heat Gain {J} due to OA air balance
			Real64 const OABalanceHeatGain, // Heat Loss {J} due to OA air balance
			Real64 const OABalanceLatentLoss, // Latent Gain {J} due to OA air balance
			Real64 const OABalanceLatentGain, // Latent Loss {J} due to OA air balance
			Real64 const OABalanceTotalLoss, // Total Gain {J} due to OA air balance
			Real64 const OABalanceTotalGain, // Total Loss {J} due to OA air balance
			Real64 const OABalanceVolumeCurDensity, // Volume of Air {m3} due to OA air balance
			Real64 const OABalanceVolumeStdDensity, // Volume of Air {m3} due to OA air balance
			Real64 const OABalanceVdotCurDensity, // Volume flow rate of Air {m3/s} due to OA air balance
			Real64 const OABalanceVdotStdDensity, // Volume flow rate of Air {m3/s} due to OA air balance
			Real64 const OABalanceMass, // Mass of Air {kg} due to OA air balance
			Real64 const OABalanceMdot, // Mass flow rate of Air {kg/s} due to OA air balance
			Real64 const OABalanceAirChangeRate, // OA air balance air change rate (ach)
			Real64 const OABalanceFanElec // Fan Electricity {W} due to OA air balance
		) :
			MeanAirTemp( MeanAirTemp ),
			OperativeTemp( OperativeTemp ),
			MeanAirHumRat( MeanAirHumRat ),
			MeanAirDewPointTemp( MeanAirDewPointTemp ),
			ThermOperativeTemp( ThermOperativeTemp ),
			InfilHeatGain( InfilHeatGain ),
			InfilHeatLoss( InfilHeatLoss ),
			InfilLatentGain( InfilLatentGain ),
			InfilLatentLoss( InfilLatentLoss ),
			InfilTotalGain( InfilTotalGain ),
			InfilTotalLoss( InfilTotalLoss ),
			InfilVolumeCurDensity( InfilVolumeCurDensity ),
			InfilVolumeStdDensity( InfilVolumeStdDensity ),
			InfilVdotCurDensity( InfilVdotCurDensity ),
			InfilVdotStdDensity( InfilVdotStdDensity ),
			InfilMass( InfilMass ),
			InfilMdot( InfilMdot ),
			InfilAirChangeRate( InfilAirChangeRate ),
			VentilHeatLoss( VentilHeatLoss ),
			VentilHeatGain( VentilHeatGain ),
			VentilLatentLoss( VentilLatentLoss ),
			VentilLatentGain( VentilLatentGain ),
			VentilTotalLoss( VentilTotalLoss ),
			VentilTotalGain( VentilTotalGain ),
			VentilVolumeCurDensity( VentilVolumeCurDensity ),
			VentilVolumeStdDensity( VentilVolumeStdDensity ),
			VentilVdotCurDensity( VentilVdotCurDensity ),
			VentilVdotStdDensity( VentilVdotStdDensity ),
			VentilMass( VentilMass ),
			VentilMdot( VentilMdot ),
			VentilAirChangeRate( VentilAirChangeRate ),
			VentilFanElec( VentilFanElec ),
			VentilAirTemp( VentilAirTemp ),
			MixVolume( MixVolume ),
			MixVdotCurDensity( MixVdotCurDensity ),
			MixVdotStdDensity( MixVdotStdDensity ),
			MixMass( MixMass ),
			MixMdot( MixMdot ),
			MixHeatLoss( MixHeatLoss ),
			MixHeatGain( MixHeatGain ),
			MixLatentLoss( MixLatentLoss ),
			MixLatentGain( MixLatentGain ),
			MixTotalLoss( MixTotalLoss ),
			MixTotalGain( MixTotalGain ),
			SumIntGains( SumIntGains ),
			SumHADTsurfs( SumHADTsurfs ),
			SumMCpDTzones( SumMCpDTzones ),
			SumMCpDtInfil( SumMCpDtInfil ),
			SumMCpDTsystem( SumMCpDTsystem ),
			SumNonAirSystem( SumNonAirSystem ),
			CzdTdt( CzdTdt ),
			imBalance( imBalance ),
			OABalanceHeatLoss( OABalanceHeatLoss ),
			OABalanceHeatGain( OABalanceHeatGain ),
			OABalanceLatentLoss( OABalanceLatentLoss ),
			OABalanceLatentGain( OABalanceLatentGain ),
			OABalanceTotalLoss( OABalanceTotalLoss ),
			OABalanceTotalGain( OABalanceTotalGain ),
			OABalanceVolumeCurDensity( OABalanceVolumeCurDensity ),
			OABalanceVolumeStdDensity( OABalanceVolumeStdDensity ),
			OABalanceVdotCurDensity( OABalanceVdotCurDensity ),
			OABalanceVdotStdDensity( OABalanceVdotStdDensity ),
			OABalanceMass( OABalanceMass ),
			OABalanceMdot( OABalanceMdot ),
			OABalanceAirChangeRate( OABalanceAirChangeRate ),
			OABalanceFanElec( OABalanceFanElec )
		{}

	};

	struct ZonePreDefRepType
	{
		// Members
		bool isOccupied; // occupied during the current time step
		Real64 NumOccAccum; // number of occupants accumulating for entire simulation
		Real64 NumOccAccumTime; // time that the number of occupants is accumulating to compute average
		//  - zone time step
		Real64 TotTimeOcc; // time occuped (and the mechnical ventilation volume is accumulating)
		//  - system time step
		Real64 MechVentVolTotal; // volume for mechnical ventilation of outside air for entire simulation
		Real64 MechVentVolMin; // a large number since finding minimum volume
		Real64 InfilVolTotal; // volume for infiltration of outside air for entire simulation
		Real64 InfilVolMin; // a large number since finding minimum volume
		Real64 AFNInfilVolTotal; // volume for infiltration of outside air for entire simulation
		Real64 AFNInfilVolMin; // a large number since finding minimum volume
		Real64 SimpVentVolTotal; // volume for simple 'ZoneVentilation' of outside air for entire simulation
		Real64 SimpVentVolMin; // a large number since finding minimum volume
		// for Sensible Heat Gas Component Report
		//annual
		Real64 SHGSAnHvacHt; // hvac air heating
		Real64 SHGSAnHvacCl; // hvac air cooling
		Real64 SHGSAnSurfHt; // heated surface heating
		Real64 SHGSAnSurfCl; // cooled surface cooling
		Real64 SHGSAnPeoplAdd; // people additions
		Real64 SHGSAnLiteAdd; // lighing addition
		Real64 SHGSAnEquipAdd; // equipment addition
		Real64 SHGSAnWindAdd; // window addition
		Real64 SHGSAnIzaAdd; // inter zone air addition
		Real64 SHGSAnInfilAdd; // infiltration addition
		Real64 SHGSAnOtherAdd; // opaque surface and other addition
		Real64 SHGSAnEquipRem; // equipment removal
		Real64 SHGSAnWindRem; // window removal
		Real64 SHGSAnIzaRem; // inter-zone air removal
		Real64 SHGSAnInfilRem; // infiltration removal
		Real64 SHGSAnOtherRem; // opaque surface and other removal
		//peak cooling
		int clPtTimeStamp; // timestamp for the cooling peak
		Real64 clPeak; // cooling peak value (hvac air cooling + cooled surface)
		Real64 SHGSClHvacHt; // hvac air heating
		Real64 SHGSClHvacCl; // hvac air cooling
		Real64 SHGSClSurfHt; // heated surface heating
		Real64 SHGSClSurfCl; // cooled surface cooling
		Real64 SHGSClPeoplAdd; // people additions
		Real64 SHGSClLiteAdd; // lighing addition
		Real64 SHGSClEquipAdd; // equipment addition
		Real64 SHGSClWindAdd; // window addition
		Real64 SHGSClIzaAdd; // inter zone air addition
		Real64 SHGSClInfilAdd; // infiltration addition
		Real64 SHGSClOtherAdd; // opaque surface and other addition
		Real64 SHGSClEquipRem; // equipment removal
		Real64 SHGSClWindRem; // window removal
		Real64 SHGSClIzaRem; // inter-zone air removal
		Real64 SHGSClInfilRem; // infiltration removal
		Real64 SHGSClOtherRem; // opaque surface and other removal
		//peak heating
		int htPtTimeStamp; // timestamp for the heating peak
		Real64 htPeak; // heating peak value (hvac air heating + heated surface)
		Real64 SHGSHtHvacHt; // hvac air heating
		Real64 SHGSHtHvacCl; // hvac air cooling
		Real64 SHGSHtSurfHt; // heated surface heating
		Real64 SHGSHtSurfCl; // cooled surface cooling
		Real64 SHGSHtPeoplAdd; // people additions
		Real64 SHGSHtLiteAdd; // lighing addition
		Real64 SHGSHtEquipAdd; // equipment addition
		Real64 SHGSHtWindAdd; // window addition
		Real64 SHGSHtIzaAdd; // inter zone air addition
		Real64 SHGSHtInfilAdd; // infiltration addition
		Real64 SHGSHtOtherAdd; // opaque surface and other addition
		Real64 SHGSHtEquipRem; // equipment removal
		Real64 SHGSHtWindRem; // window removal
		Real64 SHGSHtIzaRem; // inter-zone air removal
		Real64 SHGSHtInfilRem; // infiltration removal
		Real64 SHGSHtOtherRem; // opaque surface and other removal

		// Default Constructor
		ZonePreDefRepType() :
			isOccupied( false ),
			NumOccAccum( 0.0 ),
			NumOccAccumTime( 0.0 ),
			TotTimeOcc( 0.0 ),
			MechVentVolTotal( 0.0 ),
			MechVentVolMin( 9.9e9 ),
			InfilVolTotal( 0.0 ),
			InfilVolMin( 9.9e9 ),
			AFNInfilVolTotal( 0.0 ),
			AFNInfilVolMin( 9.9e9 ),
			SimpVentVolTotal( 0.0 ),
			SimpVentVolMin( 9.9e9 ),
			SHGSAnHvacHt( 0.0 ),
			SHGSAnHvacCl( 0.0 ),
			SHGSAnSurfHt( 0.0 ),
			SHGSAnSurfCl( 0.0 ),
			SHGSAnPeoplAdd( 0.0 ),
			SHGSAnLiteAdd( 0.0 ),
			SHGSAnEquipAdd( 0.0 ),
			SHGSAnWindAdd( 0.0 ),
			SHGSAnIzaAdd( 0.0 ),
			SHGSAnInfilAdd( 0.0 ),
			SHGSAnOtherAdd( 0.0 ),
			SHGSAnEquipRem( 0.0 ),
			SHGSAnWindRem( 0.0 ),
			SHGSAnIzaRem( 0.0 ),
			SHGSAnInfilRem( 0.0 ),
			SHGSAnOtherRem( 0.0 ),
			clPtTimeStamp( 0 ),
			clPeak( 0.0 ),
			SHGSClHvacHt( 0.0 ),
			SHGSClHvacCl( 0.0 ),
			SHGSClSurfHt( 0.0 ),
			SHGSClSurfCl( 0.0 ),
			SHGSClPeoplAdd( 0.0 ),
			SHGSClLiteAdd( 0.0 ),
			SHGSClEquipAdd( 0.0 ),
			SHGSClWindAdd( 0.0 ),
			SHGSClIzaAdd( 0.0 ),
			SHGSClInfilAdd( 0.0 ),
			SHGSClOtherAdd( 0.0 ),
			SHGSClEquipRem( 0.0 ),
			SHGSClWindRem( 0.0 ),
			SHGSClIzaRem( 0.0 ),
			SHGSClInfilRem( 0.0 ),
			SHGSClOtherRem( 0.0 ),
			htPtTimeStamp( 0 ),
			htPeak( 0.0 ),
			SHGSHtHvacHt( 0.0 ),
			SHGSHtHvacCl( 0.0 ),
			SHGSHtSurfHt( 0.0 ),
			SHGSHtSurfCl( 0.0 ),
			SHGSHtPeoplAdd( 0.0 ),
			SHGSHtLiteAdd( 0.0 ),
			SHGSHtEquipAdd( 0.0 ),
			SHGSHtWindAdd( 0.0 ),
			SHGSHtIzaAdd( 0.0 ),
			SHGSHtInfilAdd( 0.0 ),
			SHGSHtOtherAdd( 0.0 ),
			SHGSHtEquipRem( 0.0 ),
			SHGSHtWindRem( 0.0 ),
			SHGSHtIzaRem( 0.0 ),
			SHGSHtInfilRem( 0.0 ),
			SHGSHtOtherRem( 0.0 )
		{}

		// Member Constructor
		ZonePreDefRepType(
			bool const isOccupied, // occupied during the current time step
			Real64 const NumOccAccum, // number of occupants accumulating for entire simulation
			Real64 const NumOccAccumTime, // time that the number of occupants is accumulating to compute average
			Real64 const TotTimeOcc, // time occuped (and the mechnical ventilation volume is accumulating)
			Real64 const MechVentVolTotal, // volume for mechnical ventilation of outside air for entire simulation
			Real64 const MechVentVolMin, // a large number since finding minimum volume
			Real64 const InfilVolTotal, // volume for infiltration of outside air for entire simulation
			Real64 const InfilVolMin, // a large number since finding minimum volume
			Real64 const AFNInfilVolTotal, // volume for infiltration of outside air for entire simulation
			Real64 const AFNInfilVolMin, // a large number since finding minimum volume
			Real64 const SimpVentVolTotal, // volume for simple 'ZoneVentilation' of outside air for entire simulation
			Real64 const SimpVentVolMin, // a large number since finding minimum volume
			Real64 const SHGSAnHvacHt, // hvac air heating
			Real64 const SHGSAnHvacCl, // hvac air cooling
			Real64 const SHGSAnSurfHt, // heated surface heating
			Real64 const SHGSAnSurfCl, // cooled surface cooling
			Real64 const SHGSAnPeoplAdd, // people additions
			Real64 const SHGSAnLiteAdd, // lighing addition
			Real64 const SHGSAnEquipAdd, // equipment addition
			Real64 const SHGSAnWindAdd, // window addition
			Real64 const SHGSAnIzaAdd, // inter zone air addition
			Real64 const SHGSAnInfilAdd, // infiltration addition
			Real64 const SHGSAnOtherAdd, // opaque surface and other addition
			Real64 const SHGSAnEquipRem, // equipment removal
			Real64 const SHGSAnWindRem, // window removal
			Real64 const SHGSAnIzaRem, // inter-zone air removal
			Real64 const SHGSAnInfilRem, // infiltration removal
			Real64 const SHGSAnOtherRem, // opaque surface and other removal
			int const clPtTimeStamp, // timestamp for the cooling peak
			Real64 const clPeak, // cooling peak value (hvac air cooling + cooled surface)
			Real64 const SHGSClHvacHt, // hvac air heating
			Real64 const SHGSClHvacCl, // hvac air cooling
			Real64 const SHGSClSurfHt, // heated surface heating
			Real64 const SHGSClSurfCl, // cooled surface cooling
			Real64 const SHGSClPeoplAdd, // people additions
			Real64 const SHGSClLiteAdd, // lighing addition
			Real64 const SHGSClEquipAdd, // equipment addition
			Real64 const SHGSClWindAdd, // window addition
			Real64 const SHGSClIzaAdd, // inter zone air addition
			Real64 const SHGSClInfilAdd, // infiltration addition
			Real64 const SHGSClOtherAdd, // opaque surface and other addition
			Real64 const SHGSClEquipRem, // equipment removal
			Real64 const SHGSClWindRem, // window removal
			Real64 const SHGSClIzaRem, // inter-zone air removal
			Real64 const SHGSClInfilRem, // infiltration removal
			Real64 const SHGSClOtherRem, // opaque surface and other removal
			int const htPtTimeStamp, // timestamp for the heating peak
			Real64 const htPeak, // heating peak value (hvac air heating + heated surface)
			Real64 const SHGSHtHvacHt, // hvac air heating
			Real64 const SHGSHtHvacCl, // hvac air cooling
			Real64 const SHGSHtSurfHt, // heated surface heating
			Real64 const SHGSHtSurfCl, // cooled surface cooling
			Real64 const SHGSHtPeoplAdd, // people additions
			Real64 const SHGSHtLiteAdd, // lighing addition
			Real64 const SHGSHtEquipAdd, // equipment addition
			Real64 const SHGSHtWindAdd, // window addition
			Real64 const SHGSHtIzaAdd, // inter zone air addition
			Real64 const SHGSHtInfilAdd, // infiltration addition
			Real64 const SHGSHtOtherAdd, // opaque surface and other addition
			Real64 const SHGSHtEquipRem, // equipment removal
			Real64 const SHGSHtWindRem, // window removal
			Real64 const SHGSHtIzaRem, // inter-zone air removal
			Real64 const SHGSHtInfilRem, // infiltration removal
			Real64 const SHGSHtOtherRem // opaque surface and other removal
		) :
			isOccupied( isOccupied ),
			NumOccAccum( NumOccAccum ),
			NumOccAccumTime( NumOccAccumTime ),
			TotTimeOcc( TotTimeOcc ),
			MechVentVolTotal( MechVentVolTotal ),
			MechVentVolMin( MechVentVolMin ),
			InfilVolTotal( InfilVolTotal ),
			InfilVolMin( InfilVolMin ),
			AFNInfilVolTotal( AFNInfilVolTotal ),
			AFNInfilVolMin( AFNInfilVolMin ),
			SimpVentVolTotal( SimpVentVolTotal ),
			SimpVentVolMin( SimpVentVolMin ),
			SHGSAnHvacHt( SHGSAnHvacHt ),
			SHGSAnHvacCl( SHGSAnHvacCl ),
			SHGSAnSurfHt( SHGSAnSurfHt ),
			SHGSAnSurfCl( SHGSAnSurfCl ),
			SHGSAnPeoplAdd( SHGSAnPeoplAdd ),
			SHGSAnLiteAdd( SHGSAnLiteAdd ),
			SHGSAnEquipAdd( SHGSAnEquipAdd ),
			SHGSAnWindAdd( SHGSAnWindAdd ),
			SHGSAnIzaAdd( SHGSAnIzaAdd ),
			SHGSAnInfilAdd( SHGSAnInfilAdd ),
			SHGSAnOtherAdd( SHGSAnOtherAdd ),
			SHGSAnEquipRem( SHGSAnEquipRem ),
			SHGSAnWindRem( SHGSAnWindRem ),
			SHGSAnIzaRem( SHGSAnIzaRem ),
			SHGSAnInfilRem( SHGSAnInfilRem ),
			SHGSAnOtherRem( SHGSAnOtherRem ),
			clPtTimeStamp( clPtTimeStamp ),
			clPeak( clPeak ),
			SHGSClHvacHt( SHGSClHvacHt ),
			SHGSClHvacCl( SHGSClHvacCl ),
			SHGSClSurfHt( SHGSClSurfHt ),
			SHGSClSurfCl( SHGSClSurfCl ),
			SHGSClPeoplAdd( SHGSClPeoplAdd ),
			SHGSClLiteAdd( SHGSClLiteAdd ),
			SHGSClEquipAdd( SHGSClEquipAdd ),
			SHGSClWindAdd( SHGSClWindAdd ),
			SHGSClIzaAdd( SHGSClIzaAdd ),
			SHGSClInfilAdd( SHGSClInfilAdd ),
			SHGSClOtherAdd( SHGSClOtherAdd ),
			SHGSClEquipRem( SHGSClEquipRem ),
			SHGSClWindRem( SHGSClWindRem ),
			SHGSClIzaRem( SHGSClIzaRem ),
			SHGSClInfilRem( SHGSClInfilRem ),
			SHGSClOtherRem( SHGSClOtherRem ),
			htPtTimeStamp( htPtTimeStamp ),
			htPeak( htPeak ),
			SHGSHtHvacHt( SHGSHtHvacHt ),
			SHGSHtHvacCl( SHGSHtHvacCl ),
			SHGSHtSurfHt( SHGSHtSurfHt ),
			SHGSHtSurfCl( SHGSHtSurfCl ),
			SHGSHtPeoplAdd( SHGSHtPeoplAdd ),
			SHGSHtLiteAdd( SHGSHtLiteAdd ),
			SHGSHtEquipAdd( SHGSHtEquipAdd ),
			SHGSHtWindAdd( SHGSHtWindAdd ),
			SHGSHtIzaAdd( SHGSHtIzaAdd ),
			SHGSHtInfilAdd( SHGSHtInfilAdd ),
			SHGSHtOtherAdd( SHGSHtOtherAdd ),
			SHGSHtEquipRem( SHGSHtEquipRem ),
			SHGSHtWindRem( SHGSHtWindRem ),
			SHGSHtIzaRem( SHGSHtIzaRem ),
			SHGSHtInfilRem( SHGSHtInfilRem ),
			SHGSHtOtherRem( SHGSHtOtherRem )
		{}

	};

	struct ZoneReportVars // Zone level.
	{
		// Members
		// People
		Real64 PeopleRadGain;
		Real64 PeopleConGain;
		Real64 PeopleSenGain;
		Real64 PeopleNumOcc;
		Real64 PeopleLatGain;
		Real64 PeopleTotGain;
		Real64 PeopleRadGainRate;
		Real64 PeopleConGainRate;
		Real64 PeopleSenGainRate;
		Real64 PeopleLatGainRate;
		Real64 PeopleTotGainRate;
		// Lights
		Real64 LtsPower;
		Real64 LtsElecConsump;
		Real64 LtsRadGain;
		Real64 LtsVisGain;
		Real64 LtsConGain;
		Real64 LtsRetAirGain;
		Real64 LtsTotGain;
		Real64 LtsRadGainRate;
		Real64 LtsVisGainRate;
		Real64 LtsConGainRate;
		Real64 LtsRetAirGainRate;
		Real64 LtsTotGainRate;
		// Baseboard Heat
		Real64 BaseHeatPower;
		Real64 BaseHeatElecCons;
		Real64 BaseHeatRadGain;
		Real64 BaseHeatConGain;
		Real64 BaseHeatTotGain;
		Real64 BaseHeatRadGainRate;
		Real64 BaseHeatConGainRate;
		Real64 BaseHeatTotGainRate;
		// Electric Equipment
		Real64 ElecPower;
		Real64 ElecConsump;
		Real64 ElecRadGain;
		Real64 ElecConGain;
		Real64 ElecLatGain;
		Real64 ElecLost;
		Real64 ElecTotGain;
		Real64 ElecRadGainRate;
		Real64 ElecConGainRate;
		Real64 ElecLatGainRate;
		Real64 ElecLostRate;
		Real64 ElecTotGainRate;
		// Gas Equipment
		Real64 GasPower;
		Real64 GasConsump;
		Real64 GasRadGain;
		Real64 GasConGain;
		Real64 GasLatGain;
		Real64 GasLost;
		Real64 GasTotGain;
		Real64 GasRadGainRate;
		Real64 GasConGainRate;
		Real64 GasLatGainRate;
		Real64 GasLostRate;
		Real64 GasTotGainRate;
		// Hot Water Equipment
		Real64 HWPower;
		Real64 HWConsump;
		Real64 HWRadGain;
		Real64 HWConGain;
		Real64 HWLatGain;
		Real64 HWLost;
		Real64 HWTotGain;
		Real64 HWRadGainRate;
		Real64 HWConGainRate;
		Real64 HWLatGainRate;
		Real64 HWLostRate;
		Real64 HWTotGainRate;
		// Steam Equipment
		Real64 SteamPower;
		Real64 SteamConsump;
		Real64 SteamRadGain;
		Real64 SteamConGain;
		Real64 SteamLatGain;
		Real64 SteamLost;
		Real64 SteamTotGain;
		Real64 SteamRadGainRate;
		Real64 SteamConGainRate;
		Real64 SteamLatGainRate;
		Real64 SteamLostRate;
		Real64 SteamTotGainRate;
		// Other Equipment
		Real64 OtherRadGain;
		Real64 OtherConGain;
		Real64 OtherLatGain;
		Real64 OtherLost;
		Real64 OtherTotGain;
		Real64 OtherRadGainRate;
		Real64 OtherConGainRate;
		Real64 OtherLatGainRate;
		Real64 OtherLostRate;
		Real64 OtherTotGainRate;
		// IT Equipment
		Real64 ITEqCPUPower; // Zone ITE CPU Electric Power [W]
		Real64 ITEqFanPower; // Zone ITE Fan Electric Power [W]
		Real64 ITEqUPSPower; // Zone ITE UPS Electric Power [W]
		Real64 ITEqCPUPowerAtDesign; // Zone ITE CPU Electric Power at Design Inlet Conditions [W]
		Real64 ITEqFanPowerAtDesign; // Zone ITE Fan Electric Power at Design Inlet Conditions [W]
		Real64 ITEqUPSGainRateToZone; // Zone ITE UPS Heat Gain toZone Rate [W] - convective gain
		Real64 ITEqConGainRateToZone; // Zone ITE Total Heat Gain toZone Rate [W] - convective gain - includes heat gain from UPS, plus CPU and Fans if room air model not used
		Real64 ITEqCPUConsumption; // Zone ITE CPU Electric Energy [J]
		Real64 ITEqFanConsumption; // Zone ITE Fan Electric Energy [J]
		Real64 ITEqUPSConsumption; // Zone ITE UPS Electric Energy [J]
		Real64 ITEqCPUEnergyAtDesign; // Zone ITE CPU Electric Energy at Design Inlet Conditions [J]
		Real64 ITEqFanEnergyAtDesign; // Zone ITE Fan Electric Energy at Design Inlet Conditions [J]
		Real64 ITEqUPSGainEnergyToZone; // Zone ITE UPS Heat Gain toZone Energy [J] - convective gain
		Real64 ITEqConGainEnergyToZone; // Zone ITE Total Heat Gain toZone Energy [J] - convective gain - includes heat gain from UPS, plus CPU and Fans if room air model not used
		Real64 ITEqAirVolFlowStdDensity; // Zone Air volume flow rate at standard density [m3/s]
		Real64 ITEqAirMassFlow; // Zone Air mass flow rate [kg/s]
		Real64 ITEqSHI; // Zone Supply Heat Index []
		Real64 ITEqTimeOutOfOperRange; // Zone ITE Air Inlet Operating Range Exceeded Time [hr]
		Real64 ITEqTimeAboveDryBulbT; // Zone ITE Air Inlet Dry-Bulb Temperature Above Operating Range Time [hr]
		Real64 ITEqTimeBelowDryBulbT; // Zone ITE Air Inlet Dry-Bulb Temperature Below Operating Range Time [hr]
		Real64 ITEqTimeAboveDewpointT; // Zone ITE Air Inlet Dewpoint Temperature Above Operating Range Time [hr]
		Real64 ITEqTimeBelowDewpointT; // Zone ITE Air Inlet Dewpoint Temperature Below Operating Range Time [hr]
		Real64 ITEqTimeAboveRH; // Zone ITE Air Inlet Relative Humidity Above Operating Range Time [hr]
		Real64 ITEqTimeBelowRH; // Zone ITE Air Inlet Relative Humidity Below Operating Range Time [hr]
		// Overall Zone Variables
		Real64 TotRadiantGain;
		Real64 TotVisHeatGain;
		Real64 TotConvectiveGain;
		Real64 TotLatentGain;
		Real64 TotTotalHeatGain;
		Real64 TotRadiantGainRate;
		Real64 TotVisHeatGainRate;
		Real64 TotConvectiveGainRate;
		Real64 TotLatentGainRate;
		Real64 TotTotalHeatGainRate;
		// Contaminant
		Real64 CO2Rate;
		Real64 GCRate;

		// Default Constructor
		ZoneReportVars() :
			PeopleRadGain( 0.0 ),
			PeopleConGain( 0.0 ),
			PeopleSenGain( 0.0 ),
			PeopleNumOcc( 0.0 ),
			PeopleLatGain( 0.0 ),
			PeopleTotGain( 0.0 ),
			PeopleRadGainRate( 0.0 ),
			PeopleConGainRate( 0.0 ),
			PeopleSenGainRate( 0.0 ),
			PeopleLatGainRate( 0.0 ),
			PeopleTotGainRate( 0.0 ),
			LtsPower( 0.0 ),
			LtsElecConsump( 0.0 ),
			LtsRadGain( 0.0 ),
			LtsVisGain( 0.0 ),
			LtsConGain( 0.0 ),
			LtsRetAirGain( 0.0 ),
			LtsTotGain( 0.0 ),
			LtsRadGainRate( 0.0 ),
			LtsVisGainRate( 0.0 ),
			LtsConGainRate( 0.0 ),
			LtsRetAirGainRate( 0.0 ),
			LtsTotGainRate( 0.0 ),
			BaseHeatPower( 0.0 ),
			BaseHeatElecCons( 0.0 ),
			BaseHeatRadGain( 0.0 ),
			BaseHeatConGain( 0.0 ),
			BaseHeatTotGain( 0.0 ),
			BaseHeatRadGainRate( 0.0 ),
			BaseHeatConGainRate( 0.0 ),
			BaseHeatTotGainRate( 0.0 ),
			ElecPower( 0.0 ),
			ElecConsump( 0.0 ),
			ElecRadGain( 0.0 ),
			ElecConGain( 0.0 ),
			ElecLatGain( 0.0 ),
			ElecLost( 0.0 ),
			ElecTotGain( 0.0 ),
			ElecRadGainRate( 0.0 ),
			ElecConGainRate( 0.0 ),
			ElecLatGainRate( 0.0 ),
			ElecLostRate( 0.0 ),
			ElecTotGainRate( 0.0 ),
			GasPower( 0.0 ),
			GasConsump( 0.0 ),
			GasRadGain( 0.0 ),
			GasConGain( 0.0 ),
			GasLatGain( 0.0 ),
			GasLost( 0.0 ),
			GasTotGain( 0.0 ),
			GasRadGainRate( 0.0 ),
			GasConGainRate( 0.0 ),
			GasLatGainRate( 0.0 ),
			GasLostRate( 0.0 ),
			GasTotGainRate( 0.0 ),
			HWPower( 0.0 ),
			HWConsump( 0.0 ),
			HWRadGain( 0.0 ),
			HWConGain( 0.0 ),
			HWLatGain( 0.0 ),
			HWLost( 0.0 ),
			HWTotGain( 0.0 ),
			HWRadGainRate( 0.0 ),
			HWConGainRate( 0.0 ),
			HWLatGainRate( 0.0 ),
			HWLostRate( 0.0 ),
			HWTotGainRate( 0.0 ),
			SteamPower( 0.0 ),
			SteamConsump( 0.0 ),
			SteamRadGain( 0.0 ),
			SteamConGain( 0.0 ),
			SteamLatGain( 0.0 ),
			SteamLost( 0.0 ),
			SteamTotGain( 0.0 ),
			SteamRadGainRate( 0.0 ),
			SteamConGainRate( 0.0 ),
			SteamLatGainRate( 0.0 ),
			SteamLostRate( 0.0 ),
			SteamTotGainRate( 0.0 ),
			OtherRadGain( 0.0 ),
			OtherConGain( 0.0 ),
			OtherLatGain( 0.0 ),
			OtherLost( 0.0 ),
			OtherTotGain( 0.0 ),
			OtherRadGainRate( 0.0 ),
			OtherConGainRate( 0.0 ),
			OtherLatGainRate( 0.0 ),
			OtherLostRate( 0.0 ),
			OtherTotGainRate( 0.0 ),
			ITEqCPUPower( 0.0 ),
			ITEqFanPower( 0.0 ),
			ITEqUPSPower( 0.0 ),
			ITEqCPUPowerAtDesign( 0.0 ),
			ITEqFanPowerAtDesign( 0.0 ),
			ITEqUPSGainRateToZone( 0.0 ),
			ITEqConGainRateToZone( 0.0 ),
			ITEqCPUConsumption( 0.0 ),
			ITEqFanConsumption( 0.0 ),
			ITEqUPSConsumption( 0.0 ),
			ITEqCPUEnergyAtDesign( 0.0 ),
			ITEqFanEnergyAtDesign( 0.0 ),
			ITEqUPSGainEnergyToZone( 0.0 ),
			ITEqConGainEnergyToZone( 0.0 ),
			ITEqAirVolFlowStdDensity( 0.0 ),
			ITEqAirMassFlow( 0.0 ),
			ITEqSHI( 0.0 ),
			ITEqTimeOutOfOperRange( 0.0 ),
			ITEqTimeAboveDryBulbT( 0.0 ),
			ITEqTimeBelowDryBulbT( 0.0 ),
			ITEqTimeAboveDewpointT( 0.0 ),
			ITEqTimeBelowDewpointT( 0.0 ),
			ITEqTimeAboveRH( 0.0 ),
			ITEqTimeBelowRH( 0.0 ),
			TotRadiantGain( 0.0 ),
			TotVisHeatGain( 0.0 ),
			TotConvectiveGain( 0.0 ),
			TotLatentGain( 0.0 ),
			TotTotalHeatGain( 0.0 ),
			TotRadiantGainRate( 0.0 ),
			TotVisHeatGainRate( 0.0 ),
			TotConvectiveGainRate( 0.0 ),
			TotLatentGainRate( 0.0 ),
			TotTotalHeatGainRate( 0.0 ),
			CO2Rate( 0.0 ),
			GCRate( 0.0 )
		{}

		// Member Constructor
		ZoneReportVars(
			Real64 const PeopleRadGain,
			Real64 const PeopleConGain,
			Real64 const PeopleSenGain,
			Real64 const PeopleNumOcc,
			Real64 const PeopleLatGain,
			Real64 const PeopleTotGain,
			Real64 const PeopleRadGainRate,
			Real64 const PeopleConGainRate,
			Real64 const PeopleSenGainRate,
			Real64 const PeopleLatGainRate,
			Real64 const PeopleTotGainRate,
			Real64 const LtsPower,
			Real64 const LtsElecConsump,
			Real64 const LtsRadGain,
			Real64 const LtsVisGain,
			Real64 const LtsConGain,
			Real64 const LtsRetAirGain,
			Real64 const LtsTotGain,
			Real64 const LtsRadGainRate,
			Real64 const LtsVisGainRate,
			Real64 const LtsConGainRate,
			Real64 const LtsRetAirGainRate,
			Real64 const LtsTotGainRate,
			Real64 const BaseHeatPower,
			Real64 const BaseHeatElecCons,
			Real64 const BaseHeatRadGain,
			Real64 const BaseHeatConGain,
			Real64 const BaseHeatTotGain,
			Real64 const BaseHeatRadGainRate,
			Real64 const BaseHeatConGainRate,
			Real64 const BaseHeatTotGainRate,
			Real64 const ElecPower,
			Real64 const ElecConsump,
			Real64 const ElecRadGain,
			Real64 const ElecConGain,
			Real64 const ElecLatGain,
			Real64 const ElecLost,
			Real64 const ElecTotGain,
			Real64 const ElecRadGainRate,
			Real64 const ElecConGainRate,
			Real64 const ElecLatGainRate,
			Real64 const ElecLostRate,
			Real64 const ElecTotGainRate,
			Real64 const GasPower,
			Real64 const GasConsump,
			Real64 const GasRadGain,
			Real64 const GasConGain,
			Real64 const GasLatGain,
			Real64 const GasLost,
			Real64 const GasTotGain,
			Real64 const GasRadGainRate,
			Real64 const GasConGainRate,
			Real64 const GasLatGainRate,
			Real64 const GasLostRate,
			Real64 const GasTotGainRate,
			Real64 const HWPower,
			Real64 const HWConsump,
			Real64 const HWRadGain,
			Real64 const HWConGain,
			Real64 const HWLatGain,
			Real64 const HWLost,
			Real64 const HWTotGain,
			Real64 const HWRadGainRate,
			Real64 const HWConGainRate,
			Real64 const HWLatGainRate,
			Real64 const HWLostRate,
			Real64 const HWTotGainRate,
			Real64 const SteamPower,
			Real64 const SteamConsump,
			Real64 const SteamRadGain,
			Real64 const SteamConGain,
			Real64 const SteamLatGain,
			Real64 const SteamLost,
			Real64 const SteamTotGain,
			Real64 const SteamRadGainRate,
			Real64 const SteamConGainRate,
			Real64 const SteamLatGainRate,
			Real64 const SteamLostRate,
			Real64 const SteamTotGainRate,
			Real64 const OtherRadGain,
			Real64 const OtherConGain,
			Real64 const OtherLatGain,
			Real64 const OtherLost,
			Real64 const OtherTotGain,
			Real64 const OtherRadGainRate,
			Real64 const OtherConGainRate,
			Real64 const OtherLatGainRate,
			Real64 const OtherLostRate,
			Real64 const OtherTotGainRate,
			Real64 const ITEqCPUPower, // Zone ITE CPU Electric Power [W]
			Real64 const ITEqFanPower, // Zone ITE Fan Electric Power [W]
			Real64 const ITEqUPSPower, // Zone ITE UPS Electric Power [W]
			Real64 const ITEqCPUPowerAtDesign, // Zone ITE CPU Electric Power at Design Inlet Conditions [W]
			Real64 const ITEqFanPowerAtDesign, // Zone ITE Fan Electric Power at Design Inlet Conditions [W]
			Real64 const ITEqUPSGainRateToZone, // Zone ITE UPS Heat Gain toZone Rate [W] - convective gain
			Real64 const ITEqConGainRateToZone, // Zone ITE Total Heat Gain toZone Rate [W] - convective gain - includes heat gain from UPS, plus CPU and Fans if room air model not used
			Real64 const ITEqCPUConsumption, // Zone ITE CPU Electric Energy [J]
			Real64 const ITEqFanConsumption, // Zone ITE Fan Electric Energy [J]
			Real64 const ITEqUPSConsumption, // Zone ITE UPS Electric Energy [J]
			Real64 const ITEqCPUEnergyAtDesign, // Zone ITE CPU Electric Energy at Design Inlet Conditions [J]
			Real64 const ITEqFanEnergyAtDesign, // Zone ITE Fan Electric Energy at Design Inlet Conditions [J]
			Real64 const ITEqUPSGainEnergyToZone, // Zone ITE UPS Heat Gain toZone Energy [J] - convective gain
			Real64 const ITEqConGainEnergyToZone, // Zone ITE Total Heat Gain toZone Energy [J] - convective gain - includes heat gain from UPS, plus CPU and Fans if room air model not used
			Real64 const ITEqAirVolFlowStdDensity, // Zone Air volume flow rate at standard density [m3/s]
			Real64 const ITEqAirMassFlow, // Zone Air mass flow rate [kg/s]
			Real64 const ITEqSHI, // Zone Supply Heat Index []
			Real64 const ITEqTimeOutOfOperRange, // Zone ITE Air Inlet Operating Range Exceeded Time [hr]
			Real64 const ITEqTimeAboveDryBulbT, // Zone ITE Air Inlet Dry-Bulb Temperature Above Operating Range Time [hr]
			Real64 const ITEqTimeBelowDryBulbT, // Zone ITE Air Inlet Dry-Bulb Temperature Below Operating Range Time [hr]
			Real64 const ITEqTimeAboveDewpointT, // Zone ITE Air Inlet Dewpoint Temperature Above Operating Range Time [hr]
			Real64 const ITEqTimeBelowDewpointT, // Zone ITE Air Inlet Dewpoint Temperature Below Operating Range Time [hr]
			Real64 const ITEqTimeAboveRH, // Zone ITE Air Inlet Relative Humidity Above Operating Range Time [hr]
			Real64 const ITEqTimeBelowRH, // Zone ITE Air Inlet Relative Humidity Below Operating Range Time [hr]
			Real64 const TotRadiantGain,
			Real64 const TotVisHeatGain,
			Real64 const TotConvectiveGain,
			Real64 const TotLatentGain,
			Real64 const TotTotalHeatGain,
			Real64 const TotRadiantGainRate,
			Real64 const TotVisHeatGainRate,
			Real64 const TotConvectiveGainRate,
			Real64 const TotLatentGainRate,
			Real64 const TotTotalHeatGainRate,
			Real64 const CO2Rate,
			Real64 const GCRate
		) :
			PeopleRadGain( PeopleRadGain ),
			PeopleConGain( PeopleConGain ),
			PeopleSenGain( PeopleSenGain ),
			PeopleNumOcc( PeopleNumOcc ),
			PeopleLatGain( PeopleLatGain ),
			PeopleTotGain( PeopleTotGain ),
			PeopleRadGainRate( PeopleRadGainRate ),
			PeopleConGainRate( PeopleConGainRate ),
			PeopleSenGainRate( PeopleSenGainRate ),
			PeopleLatGainRate( PeopleLatGainRate ),
			PeopleTotGainRate( PeopleTotGainRate ),
			LtsPower( LtsPower ),
			LtsElecConsump( LtsElecConsump ),
			LtsRadGain( LtsRadGain ),
			LtsVisGain( LtsVisGain ),
			LtsConGain( LtsConGain ),
			LtsRetAirGain( LtsRetAirGain ),
			LtsTotGain( LtsTotGain ),
			LtsRadGainRate( LtsRadGainRate ),
			LtsVisGainRate( LtsVisGainRate ),
			LtsConGainRate( LtsConGainRate ),
			LtsRetAirGainRate( LtsRetAirGainRate ),
			LtsTotGainRate( LtsTotGainRate ),
			BaseHeatPower( BaseHeatPower ),
			BaseHeatElecCons( BaseHeatElecCons ),
			BaseHeatRadGain( BaseHeatRadGain ),
			BaseHeatConGain( BaseHeatConGain ),
			BaseHeatTotGain( BaseHeatTotGain ),
			BaseHeatRadGainRate( BaseHeatRadGainRate ),
			BaseHeatConGainRate( BaseHeatConGainRate ),
			BaseHeatTotGainRate( BaseHeatTotGainRate ),
			ElecPower( ElecPower ),
			ElecConsump( ElecConsump ),
			ElecRadGain( ElecRadGain ),
			ElecConGain( ElecConGain ),
			ElecLatGain( ElecLatGain ),
			ElecLost( ElecLost ),
			ElecTotGain( ElecTotGain ),
			ElecRadGainRate( ElecRadGainRate ),
			ElecConGainRate( ElecConGainRate ),
			ElecLatGainRate( ElecLatGainRate ),
			ElecLostRate( ElecLostRate ),
			ElecTotGainRate( ElecTotGainRate ),
			GasPower( GasPower ),
			GasConsump( GasConsump ),
			GasRadGain( GasRadGain ),
			GasConGain( GasConGain ),
			GasLatGain( GasLatGain ),
			GasLost( GasLost ),
			GasTotGain( GasTotGain ),
			GasRadGainRate( GasRadGainRate ),
			GasConGainRate( GasConGainRate ),
			GasLatGainRate( GasLatGainRate ),
			GasLostRate( GasLostRate ),
			GasTotGainRate( GasTotGainRate ),
			HWPower( HWPower ),
			HWConsump( HWConsump ),
			HWRadGain( HWRadGain ),
			HWConGain( HWConGain ),
			HWLatGain( HWLatGain ),
			HWLost( HWLost ),
			HWTotGain( HWTotGain ),
			HWRadGainRate( HWRadGainRate ),
			HWConGainRate( HWConGainRate ),
			HWLatGainRate( HWLatGainRate ),
			HWLostRate( HWLostRate ),
			HWTotGainRate( HWTotGainRate ),
			SteamPower( SteamPower ),
			SteamConsump( SteamConsump ),
			SteamRadGain( SteamRadGain ),
			SteamConGain( SteamConGain ),
			SteamLatGain( SteamLatGain ),
			SteamLost( SteamLost ),
			SteamTotGain( SteamTotGain ),
			SteamRadGainRate( SteamRadGainRate ),
			SteamConGainRate( SteamConGainRate ),
			SteamLatGainRate( SteamLatGainRate ),
			SteamLostRate( SteamLostRate ),
			SteamTotGainRate( SteamTotGainRate ),
			OtherRadGain( OtherRadGain ),
			OtherConGain( OtherConGain ),
			OtherLatGain( OtherLatGain ),
			OtherLost( OtherLost ),
			OtherTotGain( OtherTotGain ),
			OtherRadGainRate( OtherRadGainRate ),
			OtherConGainRate( OtherConGainRate ),
			OtherLatGainRate( OtherLatGainRate ),
			OtherLostRate( OtherLostRate ),
			OtherTotGainRate( OtherTotGainRate ),
			ITEqCPUPower( ITEqCPUPower ),
			ITEqFanPower( ITEqFanPower ),
			ITEqUPSPower( ITEqUPSPower ),
			ITEqCPUPowerAtDesign( ITEqCPUPowerAtDesign ),
			ITEqFanPowerAtDesign( ITEqFanPowerAtDesign ),
			ITEqUPSGainRateToZone( ITEqUPSGainRateToZone ),
			ITEqConGainRateToZone( ITEqConGainRateToZone ),
			ITEqCPUConsumption( ITEqCPUConsumption ),
			ITEqFanConsumption( ITEqFanConsumption ),
			ITEqUPSConsumption( ITEqUPSConsumption ),
			ITEqCPUEnergyAtDesign( ITEqCPUEnergyAtDesign ),
			ITEqFanEnergyAtDesign( ITEqFanEnergyAtDesign ),
			ITEqUPSGainEnergyToZone( ITEqUPSGainEnergyToZone ),
			ITEqConGainEnergyToZone( ITEqConGainEnergyToZone ),
			ITEqAirVolFlowStdDensity( ITEqAirVolFlowStdDensity ),
			ITEqAirMassFlow( ITEqAirMassFlow ),
			ITEqSHI( ITEqSHI ),
			ITEqTimeOutOfOperRange( ITEqTimeOutOfOperRange ),
			ITEqTimeAboveDryBulbT( ITEqTimeAboveDryBulbT ),
			ITEqTimeBelowDryBulbT( ITEqTimeBelowDryBulbT ),
			ITEqTimeAboveDewpointT( ITEqTimeAboveDewpointT ),
			ITEqTimeBelowDewpointT( ITEqTimeBelowDewpointT ),
			ITEqTimeAboveRH( ITEqTimeAboveRH ),
			ITEqTimeBelowRH( ITEqTimeBelowRH ),
			TotRadiantGain( TotRadiantGain ),
			TotVisHeatGain( TotVisHeatGain ),
			TotConvectiveGain( TotConvectiveGain ),
			TotLatentGain( TotLatentGain ),
			TotTotalHeatGain( TotTotalHeatGain ),
			TotRadiantGainRate( TotRadiantGainRate ),
			TotVisHeatGainRate( TotVisHeatGainRate ),
			TotConvectiveGainRate( TotConvectiveGainRate ),
			TotLatentGainRate( TotLatentGainRate ),
			TotTotalHeatGainRate( TotTotalHeatGainRate ),
			CO2Rate( CO2Rate ),
			GCRate( GCRate )
		{}

	};

	// Object Data
	extern Array1D< ZonePreDefRepType > ZonePreDefRep;
	extern ZonePreDefRepType BuildingPreDefRep; //Autodesk:Note Removed explicit constructor that was missing some entries
	extern Array1D< ZoneSimData > ZoneIntGain;
	extern Array1D< MaterialProperties > Material;
	extern Array1D< GapSupportPillar > SupportPillar;
	extern Array1D< GapDeflectionState > DeflectionState;
	extern Array1D< ConstructionData > Construct;
	extern Array1D< SpectralDataProperties > SpectralData;
	extern Array1D< ZoneData > Zone;
	extern Array1D< ZoneListData > ZoneList;
	extern Array1D< ZoneGroupData > ZoneGroup;
	extern Array1D< PeopleData > People;
	extern Array1D< LightsData > Lights;
	extern Array1D< ZoneEquipData > ZoneElectric;
	extern Array1D< ZoneEquipData > ZoneGas;
	extern Array1D< ZoneEquipData > ZoneOtherEq;
	extern Array1D< ZoneEquipData > ZoneHWEq;
	extern Array1D< ZoneEquipData > ZoneSteamEq;
	extern Array1D< ITEquipData > ZoneITEq;
	extern Array1D< BBHeatData > ZoneBBHeat;
	extern Array1D< InfiltrationData > Infiltration;
	extern Array1D< VentilationData > Ventilation;
	extern Array1D< ZoneAirBalanceData > ZoneAirBalance;
	extern Array1D< MixingData > Mixing;
	extern Array1D< MixingData > CrossMixing;
	extern Array1D< MixingData > RefDoorMixing;
	extern Array1D< WindowBlindProperties > Blind;
	extern Array1D< WindowComplexShade > ComplexShade;
	extern Array1D< WindowThermalModelParams > WindowThermalModel;
	extern Array1D< SurfaceScreenProperties > SurfaceScreens;
	extern Array1D< ScreenTransData > ScreenTrans;
	extern Array1D< ZoneCatEUseData > ZoneIntEEuse;
	extern Array1D< RefrigCaseCreditData > RefrigCaseCredit;
	extern Array1D< HeatReclaimRefrigeratedRackData > HeatReclaimRefrigeratedRack;
	extern Array1D< HeatReclaimRefrigCondenserData > HeatReclaimRefrigCondenser;
	extern Array1D< HeatReclaimDXCoilData > HeatReclaimDXCoil;
	extern Array1D< AirReportVars > ZnAirRpt;
	extern Array1D< TCGlazingsType > TCGlazings;
	extern Array1D< ZoneEquipData > ZoneCO2Gen;
	extern Array1D< GlobalInternalGainMiscObject > PeopleObjects;
	extern Array1D< GlobalInternalGainMiscObject > LightsObjects;
	extern Array1D< GlobalInternalGainMiscObject > ZoneElectricObjects;
	extern Array1D< GlobalInternalGainMiscObject > ZoneGasObjects;
	extern Array1D< GlobalInternalGainMiscObject > HotWaterEqObjects;
	extern Array1D< GlobalInternalGainMiscObject > SteamEqObjects;
	extern Array1D< GlobalInternalGainMiscObject > OtherEqObjects;
	extern Array1D< GlobalInternalGainMiscObject > InfiltrationObjects;
	extern Array1D< GlobalInternalGainMiscObject > VentilationObjects;
	extern Array1D< ZoneReportVars > ZnRpt;
	extern Array1D< ZoneMassConservationData > MassConservation;
	extern ZoneAirMassFlowConservation ZoneAirMassFlow;

	// Functions

	// Clears the global data in DataHeatBalance.
	// Needed for unit tests, should not be normally called.
	void
	clear_state();

	void
	CheckAndSetConstructionProperties(
		int const ConstrNum, // Construction number to be set/checked
		bool & ErrorsFound // error flag that is set when certain errors have occurred
	);

	int
	AssignReverseConstructionNumber(
		int const ConstrNum, // Existing Construction number of first surface
		bool & ErrorsFound
	);

	void
	AddVariableSlatBlind(
		int const inBlindNumber, // current Blind Number/pointer to name
		int & outBlindNumber, // resultant Blind Number to pass back
		bool & errFlag // error flag should one be needed
	);

	void
	CalcScreenTransmittance(
		int const SurfaceNum,
		Optional< Real64 const > Phi = _, // Optional sun altitude relative to surface outward normal (radians)
		Optional< Real64 const > Theta = _, // Optional sun azimuth relative to surface outward normal (radians)
		Optional_int_const ScreenNumber = _ // Optional screen number
	);

	std::string
	DisplayMaterialRoughness( int const Roughness ); // Roughness String

	Real64
	ComputeNominalUwithConvCoeffs(
		int const numSurf, // index for Surface array.
		bool & isValid // returns true if result is valid
	);

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

} // DataHeatBalance

} // EnergyPlus

#endif
