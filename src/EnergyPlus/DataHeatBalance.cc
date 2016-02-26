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
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <DataHeatBalance.hh>
#include <DataEnvironment.hh>
#include <DataPrecisionGlobals.hh>
#include <General.hh>
#include <InputProcessor.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace DataHeatBalance {

	// MODULE INFORMATION:
	//       AUTHOR         Rick Strand
	//       DATE WRITTEN   August 1997 (rewritten)
	//       MODIFIED       Aug-Oct 1997 RKS (added derived types)
	//       MODIFIED       Feb 1999 (FW) Added visible radiation parameters,
	//                      WindowShadingControl type and SurfaceWindowCalc type
	//                      Sep 1999 (FW) Added/modified Window4 gas variables
	//                      Jul 2003 (CC) Added reference temperature variable for air models
	//                      Aug 2003 (FW) Added FractionReturnAirPlenTempCoeff1 and
	//                      FractionReturnAirPlenTempCoeff2 to Type LightsData
	//                      Nov 2003 (FW) Add FullExteriorWithRefl and FullInteriorExteriorWithRefl
	//                       as SolarDistribution values
	//                      Dec 2003 (PGE) Added Zone List and Zone Group; added SNLoad variables
	//                      August 2006 (COP) Added variable k coefficient and PCM enthalpy.
	//                      Dec 2006 (DJS-PSU) Added ecoroof material
	//                      Dec 2008 TH added new properties to MaterialProperties and
	//                              ConstructionData for thermochromic windows
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This module should contain the information that is needed to pass
	// from the Heat Balance Module and all of the Zone Initializations
	// such as ConductionTransferFunction, GlassCalculation,
	// SolarShading, etc. Modules.

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataGlobals::AutoCalculate;
	using DataGlobals::DegToRadians;
	using DataSurfaces::MaxSlatAngs;
	using namespace DataVectorTypes;
	using DataBSDFWindow::BSDFWindowInputStruct;
	using DataBSDFWindow::BSDFLayerAbsorpStruct;

	// Data
	// module should be available to other modules and routines.  Thus,
	// all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:

	// Parameters for the definition and limitation of arrays:
	int const MaxLayersInConstruct( 11 ); // Maximum number of layers allowed in a single construction
	int const MaxCTFTerms( 19 ); // Maximum number of CTF terms allowed to still allow stability //Note Sync with SurfaceGroundHeatExchanger::local::MaxCTFTerms
	int MaxSolidWinLayers( 0 ); // Maximum number of solid layers in a window construction
										// ** has to be big enough to hold no matter what window model
										//    each window model should validate layers individually
	int const MaxSpectralDataElements( 800 ); // Maximum number in Spectral Data arrays.

	// Parameters to indicate material group type for use with the Material
	// derived type (see below):

	int const RegularMaterial( 0 );
	int const Air( 1 );
	int const Shade( 2 );
	int const WindowGlass( 3 );
	int const WindowGas( 4 );
	int const WindowBlind( 5 );
	int const WindowGasMixture( 6 );
	int const Screen( 7 );
	int const EcoRoof( 8 );
	int const IRTMaterial( 9 );
	int const WindowSimpleGlazing( 10 );
	int const ComplexWindowShade( 11 );
	int const ComplexWindowGap( 12 );

	int const GlassEquivalentLayer( 13 );
	int const ShadeEquivalentLayer( 14 );
	int const DrapeEquivalentLayer( 15 );
	int const BlindEquivalentLayer( 16 );
	int const ScreenEquivalentLayer( 17 );
	int const GapEquivalentLayer( 18 );

	Array1D_string const cMaterialGroupType( {-1,18}, { "invalid", "Material/Material:NoMass", "Material:AirGap", "WindowMaterial:Shade", "WindowMaterial:Glazing*", "WindowMaterial:Gas", "WindowMaterial:Blind", "WindowMaterial:GasMixture", "WindowMaterial:Screen", "Material:RoofVegetation", "Material:InfraredTransparent", "WindowMaterial:SimpleGlazingSystem", "WindowMaterial:ComplexShade", "WindowMaterial:Gap", "WindowMaterial:Glazing:EquivalentLayer", "WindowMaterial:Shade:EquivalentLayer", "WindowMaterial:Drape:EquivalentLayer", "WindowMaterial:Blind:EquivalentLayer", "WindowMaterial:Screen:EquivalentLayer", "WindowMaterial:Gap:EquivalentLayer" } );

	// Parameters to indicate surface roughness for use with the Material
	// derived type (see below):

	int const VeryRough( 1 );
	int const Rough( 2 );
	int const MediumRough( 3 );
	int const MediumSmooth( 4 );
	int const Smooth( 5 );
	int const VerySmooth( 6 );

	// Parameters to indicate blind orientation for use with the Material
	// derived type (see below):

	int const Horizontal( 1 );
	int const Vertical( 2 );
	int const FixedSlats( 1 );
	int const VariableSlats( 2 );
	// Parameters for Interior and Exterior Solar Distribution

	int const MinimalShadowing( -1 ); // all incoming solar hits floor, no exterior shadowing except reveals
	int const FullExterior( 0 ); // all incoming solar hits floor, full exterior shadowing
	int const FullInteriorExterior( 1 ); // full interior solar distribution, full exterior solar shadowing
	int const FullExteriorWithRefl( 2 ); // all incoming solar hits floor, full exterior shadowing and reflections
	int const FullInteriorExteriorWithRefl( 3 ); // full interior solar distribution,
	// full exterior shadowing and reflections
	// Parameters to indicate the zone type for use with the Zone derived
	// type (see below--Zone%OfType):

	int const StandardZone( 1 );
	//INTEGER, PARAMETER :: PlenumZone = 2
	//INTEGER, PARAMETER :: SolarWallZone = 11  ! from old ZTYP, OSENV
	//INTEGER, PARAMETER :: RoofPondZone = 12   ! from old ZTYP, OSENV

	// Parameters to indicate the convection correlation being used for use with
	// InsideConvectionAlgo and OutsideConvectionAlgo

	int const ASHRAESimple( 1 );
	int const ASHRAETARP( 2 );
	int const CeilingDiffuser( 3 ); // Only valid for inside use
	int const TrombeWall( 4 ); // Only valid for inside use
	int const TarpHcOutside( 5 ); // Only valid for outside use
	int const MoWiTTHcOutside( 6 ); // Only valid for outside use
	int const DOE2HcOutside( 7 ); // Only valid for outside use
	int const BLASTHcOutside( 8 ); // Only valid for outside use
	int const AdaptiveConvectionAlgorithm( 9 );

	// Parameters for WarmupDays
	int const DefaultMaxNumberOfWarmupDays( 25 ); // Default maximum number of warmup days allowed
	int const DefaultMinNumberOfWarmupDays( 6 ); // Default minimum number of warmup days allowed

	// Parameters for Sky Radiance Distribution
	int const Isotropic( 0 );
	int const Anisotropic( 1 );

	// Parameters for HeatTransferAlgosUsed
	int const UseCTF( 1 );
	int const UseEMPD( 2 );
	int const UseCondFD( 5 );
	int const UseHAMT( 6 );

	// Parameters for ZoneAirSolutionAlgo
	int const Use3rdOrder( 0 );
	int const UseAnalyticalSolution( 1 );
	int const UseEulerMethod( 2 );

	// Parameter for MRT calculation type
	int const ZoneAveraged( 1 );
	int const SurfaceWeighted( 2 );
	int const AngleFactor( 3 );

	// Parameters for Ventilation
	int const NaturalVentilation( 0 );
	int const IntakeVentilation( 1 );
	int const ExhaustVentilation( 2 );
	int const BalancedVentilation( 3 );

	// Parameters for hybrid ventilation using Ventilation and Mixing objects
	int const HybridControlTypeIndiv( 0 );
	int const HybridControlTypeClose( 1 );
	int const HybridControlTypeGlobal( 2 );

	// System type, detailed refrigeration or refrigerated case rack
	int const RefrigSystemTypeDetailed( 1 );
	int const RefrigSystemTypeRack( 2 );

	// Refrigeration condenser type
	int const RefrigCondenserTypeAir( 1 );
	int const RefrigCondenserTypeEvap( 2 );
	int const RefrigCondenserTypeWater( 3 );
	int const RefrigCondenserTypeCascade( 4 );

	// Parameters for type of infiltration model
	int const InfiltrationDesignFlowRate( 1 );
	int const InfiltrationShermanGrimsrud( 2 );
	int const InfiltrationAIM2( 3 );

	// Parameters for type of ventilation model
	int const VentilationDesignFlowRate( 1 );
	int const VentilationWindAndStack( 2 );

	// Parameters for type of zone air balance model
	int const AirBalanceNone( 0 );
	int const AirBalanceQuadrature( 1 );

	// Parameter for source zone air flow mass balance infiltration treatment
	int const NoInfiltrationFlow( 0 );
	int const AddInfiltrationFlow( 1 );
	int const AdjustInfiltrationFlow( 2 );
	int const MixingSourceZonesOnly( 1 );
	int const AllZones( 2 );

	int const NumZoneIntGainDeviceTypes( 51 );
	Array1D_string const ZoneIntGainDeviceTypes( NumZoneIntGainDeviceTypes, { "PEOPLE", "LIGHTS", "ELECTRICEQUIPMENT", "GASEQUIPMENT", "HOTWATEREQUIPMENT", "STEAMEQUIPMENT", "OTHEREQUIPMENT", "ZONEBASEBOARD:OUTDOORTEMPERATURECONTROLLED", "ZONECONTAMINANTSOURCEANDSINK:CARBONDIOXIDE", "WATERUSE:EQUIPMENT", "DAYLIGHTINGDEVICE:TUBULAR", "WATERHEATER:MIXED", "WATERHEATER:STRATIFIED", "THERMALSTORAGE:CHILLEDWATER:MIXED", "THERMALSTORAGE:CHILLEDWATER:STRATIFIED", "GENERATOR:FUELCELL", "GENERATOR:MICROCHP", "ELECTRICLOADCENTER:TRANSFORMER", "ELECTRICLOADCENTER:INVERTER:SIMPLE", "ELECTRICLOADCENTER:INVERTER:FUNCTIONOFPOWER", "ELECTRICLOADCENTER:INVERTER:LOOKUPTABLE", "ELECTRICLOADCENTER:STORAGE:BATTERY", "ELECTRICLOADCENTER:STORAGE:SIMPLE", "PIPE:INDOOR", "REFRIGERATION:CASE", "REFRIGERATION:COMPRESSORRACK", "REFRIGERATION:SYSTEM:CONDENSER:AIRCOOLED", "REFRIGERATION:TRANSCRITICALSYSTEM:GASCOOLER:AIRCOOLED", "REFRIGERATION:SYSTEM:SUCTIONPIPE", "REFRIGERATION:TRANSCRITICALSYSTEM:SUCTIONPIPEMT", "REFRIGERATION:TRANSCRITICALSYSTEM:SUCTIONPIPELT", "REFRIGERATION:SECONDARYSYSTEM:RECEIVER", "REFRIGERATION:SECONDARYSYSTEM:PIPE", "REFRIGERATION:WALKIN", "PUMP:VARIABLESPEED", "PUMP:CONSTANTSPEED", "PUMP:VARIABLESPEED:CONDENSATE", "HEADEREDPUMPS:VARIABLESPEED", "HEADEREDPUMPS:CONSTANTSPEED", "ZONECONTAMINANTSOURCEANDSINK:GENERICCONTAMINANT", "PLANTCOMPONENT:USERDEFINED", "COIL:USERDEFINED", "ZONEHVAC:FORCEDAIR:USERDEFINED", "AIRTERMINAL:SINGLEDUCT:USERDEFINED", "COIL:COOLING:DX:SINGLESPEED:THERMALSTORAGE", "ELECTRICEQUIPMENT:ITE:AIRCOOLED", "COIL:COOLING:DX:SINGLESPEED", "COIL:HEATING:DX:SINGLESPEED", "COIL:COOLING:DX:TWOSPEED", "COIL:COOLING:DX:MULTISPEED", "COIL:HEATING:DX:MULTISPEED"} ); // 01 | 02 | 03 | 04 | 05 | 06 | 07 | 08 | 09 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25 | 26 | 27 | 28 | 29 | 30 | 31 | 32 | 33 | 34 | 35 | 36 | 37 | 38 | 39 | 40 | 41 | 42 | 43 | 44 | 45 | 46 | 47 | 48 | 49 | 50 | 51

	Array1D_string const ccZoneIntGainDeviceTypes( NumZoneIntGainDeviceTypes, { "People", "Lights", "ElectricEquipment", "GasEquipment", "HotWaterEquipment", "SteamEquipment", "OtherEquipment", "ZoneBaseboard:OutdoorTemperatureControlled", "ZoneContaminantSourceAndSink:CarbonDioxide", "WaterUse:Equipment", "DaylightingDevice:Tubular", "WaterHeater:Mixed", "WaterHeater:Stratified", "ThermalStorage:ChilledWater:Mixed", "ThermalStorage:ChilledWater:Stratified", "Generator:FuelCell", "Generator:MicroCHP", "ElectricLoadCenter:Transformer", "ElectricLoadCenter:Inverter:Simple", "ElectricLoadCenter:Inverter:FunctionOfPower", "ElectricLoadCenter:Inverter:LookUpTable", "ElectricLoadCenter:Storage:Battery", "ElectricLoadCenter:Storage:Simple", "Pipe:Indoor", "Refrigeration:Case", "Refrigeration:CompressorRack", "Refrigeration:System:Condenser:AirCooled", "Refrigeration:TranscriticalSystem:GasCooler:AirCooled", "Refrigeration:System:SuctionPipe", "Refrigeration:TranscriticalSystem:SuctionPipeMT", "Refrigeration:TranscriticalSystem:SuctionPipeLT", "Refrigeration:SecondarySystem:Receiver", "Refrigeration:SecondarySystem:Pipe", "Refrigeration:WalkIn", "Pump:VariableSpeed", "Pump:ConstantSpeed", "Pump:VariableSpeed:Condensate", "HeaderedPumps:VariableSpeed", "HeaderedPumps:ConstantSpeed", "ZoneContaminantSourceAndSink:GenericContaminant", "PlantComponent:UserDefined", "Coil:UserDefined", "ZoneHVAC:ForcedAir:UserDefined", "AirTerminal:SingleDuct:UserDefined", "Coil:Cooling:DX:SingleSpeed:ThermalStorage", "ElectricEquipment:ITE:AirCooled", "Coil:Cooling:DX:SingleSpeed", "Coil:Heating:DX:SingleSpeed", "Coil:Cooling:DX:TwoSpeed", "Coil:Cooling:DX:MultiSpeed", "Coil:Heating:DX:MultiSpeed"} ); // 01 | 02 | 03 | 04 | 05 | 06 | 07 | 08 | 09 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25 | 26 | 27 | 28 | 29 | 30 | 31 | 32 | 33 | 34 | 35 | 36 | 37 | 38 | 39 | 40 | 41 | 42 | 43 | 44 | 45 | 46 | 47 | 48 | 49 | 50 | 51

	int const IntGainTypeOf_People( 1 );
	int const IntGainTypeOf_Lights( 2 );
	int const IntGainTypeOf_ElectricEquipment( 3 );
	int const IntGainTypeOf_GasEquipment( 4 );
	int const IntGainTypeOf_HotWaterEquipment( 5 );
	int const IntGainTypeOf_SteamEquipment( 6 );
	int const IntGainTypeOf_OtherEquipment( 7 );
	int const IntGainTypeOf_ZoneBaseboardOutdoorTemperatureControlled( 8 );
	int const IntGainTypeOf_ZoneContaminantSourceAndSinkCarbonDioxide( 9 );
	int const IntGainTypeOf_WaterUseEquipment( 10 );
	int const IntGainTypeOf_DaylightingDeviceTubular( 11 );
	int const IntGainTypeOf_WaterHeaterMixed( 12 );
	int const IntGainTypeOf_WaterHeaterStratified( 13 );
	int const IntGainTypeOf_ThermalStorageChilledWaterMixed( 14 );
	int const IntGainTypeOf_ThermalStorageChilledWaterStratified( 15 );
	int const IntGainTypeOf_GeneratorFuelCell( 16 );
	int const IntGainTypeOf_GeneratorMicroCHP( 17 );
	int const IntGainTypeOf_ElectricLoadCenterTransformer( 18 );
	int const IntGainTypeOf_ElectricLoadCenterInverterSimple( 19 );
	int const IntGainTypeOf_ElectricLoadCenterInverterFunctionOfPower( 20 );
	int const IntGainTypeOf_ElectricLoadCenterInverterLookUpTable( 21 );
	int const IntGainTypeOf_ElectricLoadCenterStorageBattery( 22 );
	int const IntGainTypeOf_ElectricLoadCenterStorageSimple( 23 );
	int const IntGainTypeOf_PipeIndoor( 24 );
	int const IntGainTypeOf_RefrigerationCase( 25 );
	int const IntGainTypeOf_RefrigerationCompressorRack( 26 );
	int const IntGainTypeOf_RefrigerationSystemAirCooledCondenser( 27 );
	int const IntGainTypeOf_RefrigerationTransSysAirCooledGasCooler( 28 );
	int const IntGainTypeOf_RefrigerationSystemSuctionPipe( 29 );
	int const IntGainTypeOf_RefrigerationTransSysSuctionPipeMT( 30 );
	int const IntGainTypeOf_RefrigerationTransSysSuctionPipeLT( 31 );
	int const IntGainTypeOf_RefrigerationSecondaryReceiver( 32 );
	int const IntGainTypeOf_RefrigerationSecondaryPipe( 33 );
	int const IntGainTypeOf_RefrigerationWalkIn( 34 );
	int const IntGainTypeOf_Pump_VarSpeed( 35 );
	int const IntGainTypeOf_Pump_ConSpeed( 36 );
	int const IntGainTypeOf_Pump_Cond( 37 );
	int const IntGainTypeOf_PumpBank_VarSpeed( 38 );
	int const IntGainTypeOf_PumpBank_ConSpeed( 39 );
	int const IntGainTypeOf_ZoneContaminantSourceAndSinkGenericContam( 40 );
	int const IntGainTypeOf_PlantComponentUserDefined( 41 );
	int const IntGainTypeOf_CoilUserDefined( 42 );
	int const IntGainTypeOf_ZoneHVACForcedAirUserDefined( 43 );
	int const IntGainTypeOf_AirTerminalUserDefined( 44 );
	int const IntGainTypeOf_PackagedTESCoilTank( 45 );
	int const IntGainTypeOf_ElectricEquipmentITEAirCooled( 46 );
	int const IntGainTypeOf_SecCoolingDXCoilSingleSpeed( 47 );
	int const IntGainTypeOf_SecHeatingDXCoilSingleSpeed( 48 );
	int const IntGainTypeOf_SecCoolingDXCoilTwoSpeed( 49 );
	int const IntGainTypeOf_SecCoolingDXCoilMultiSpeed( 50 );
	int const IntGainTypeOf_SecHeatingDXCoilMultiSpeed( 51 );
	int const IntGainTypeOf_ElectricLoadCenterConverter( 52 );

	//Parameters for checking surface heat transfer models
	Real64 const HighDiffusivityThreshold( 1.e-5 ); // used to check if Material properties are out of line.
	Real64 const ThinMaterialLayerThreshold( 0.003 ); // 3 mm lower limit to expected material layers

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
	Real64 LowHConvLimit( 0.1 ); // Lowest allowed convection coefficient for detailed model
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
	Real64 HighHConvLimit( 1000.0 ); // upper limit for HConv, mostly used for user input limits in practics. !W/m2-K
	Real64 MaxAllowedDelTempCondFD( 0.002 ); // Convergence criteria for inside surface temperatures for CondFD

	std::string BuildingName; // Name of building
	Real64 BuildingAzimuth( 0.0 ); // North Axis of Building
	Real64 LoadsConvergTol( 0.0 ); // Tolerance value for Loads Convergence
	Real64 TempConvergTol( 0.0 ); // Tolerance value for Temperature Convergence
	int DefaultInsideConvectionAlgo( 1 ); // 1 = simple (ASHRAE); 2 = detailed (ASHRAE); 3 = ceiling diffuser;
	// 4 = trombe wall
	int DefaultOutsideConvectionAlgo( 1 ); // 1 = simple (ASHRAE); 2 = detailed; etc (BLAST, TARP, MOWITT, DOE-2)
	int SolarDistribution( 0 ); // Solar Distribution Algorithm
	int InsideSurfIterations( 0 ); // Counts inside surface iterations
	int OverallHeatTransferSolutionAlgo( UseCTF ); // UseCTF Solution, UseEMPD moisture solution, UseCondFD solution
	int NumberOfHeatTransferAlgosUsed( 1 );
	Array1D_int HeatTransferAlgosUsed;
	int MaxNumberOfWarmupDays( 25 ); // Maximum number of warmup days allowed
	int MinNumberOfWarmupDays( 6 ); // Minimum number of warmup days allowed
	Real64 CondFDRelaxFactor( 1.0 ); // Relaxation factor, for looping across all the surfaces.
	Real64 CondFDRelaxFactorInput( 1.0 ); // Relaxation factor, for looping across all the surfaces, user input value
	//LOGICAL ::  CondFDVariableProperties = .FALSE. ! if true, then variable conductivity or enthalpy in Cond FD.

	int ZoneAirSolutionAlgo( Use3rdOrder ); // ThirdOrderBackwardDifference, AnalyticalSolution, and EulerMethod
	Real64 BuildingRotationAppendixG( 0.0 ); // Building Rotation for Appendix G

	//END SiteData

	int NumOfZoneLists( 0 ); // Total number of zone lists
	int NumOfZoneGroups( 0 ); // Total number of zone groups
	int NumPeopleStatements( 0 ); // Number of People objects in input - possibly global assignments
	int NumLightsStatements( 0 ); // Number of Lights objects in input - possibly global assignments
	int NumZoneElectricStatements( 0 ); // Number of ZoneElectric objects in input - possibly global assignments
	int NumZoneGasStatements( 0 ); // Number of ZoneGas objects in input - possibly global assignments
	int NumInfiltrationStatements( 0 ); // Number of Design Flow Infiltration objects in input - possibly global assignments
	int NumVentilationStatements( 0 ); // Number of Design Flow Ventilation objects in input - possibly global assignments
	int NumHotWaterEqStatements( 0 ); // number of Hot Water Equipment objects in input. - possibly global assignments
	int NumSteamEqStatements( 0 ); // number of Steam Equipment objects in input. - possibly global assignments
	int NumOtherEqStatements( 0 ); // number of Other Equipment objects in input. - possibly global assignments
	int NumZoneITEqStatements( 0 ); // number of Other Equipment objects in input. - possibly global assignments
	int TotPeople( 0 ); // Total People Statements in input and extrapolated from global assignments
	int TotLights( 0 ); // Total Lights Statements in input and extrapolated from global assignments
	int TotElecEquip( 0 ); // Total Electric Equipment Statements in input and extrapolated from global assignments
	int TotGasEquip( 0 ); // Total Gas Equipment Statements in input
	int TotOthEquip( 0 ); // Total Other Equipment Statements in input
	int TotHWEquip( 0 ); // Total Hot Water Equipment Statements in input
	int TotStmEquip( 0 ); // Total Steam Equipment Statements in input
	int TotInfiltration( 0 ); // Total Infiltration Statements in input and extrapolated from global assignments
	int TotDesignFlowInfiltration( 0 ); // number of Design Flow rate ZoneInfiltration in input
	int TotShermGrimsInfiltration( 0 ); // number of Sherman Grimsrud (ZoneInfiltration:ResidentialBasic) in input
	int TotAIM2Infiltration( 0 ); // number of AIM2 (ZoneInfiltration:ResidentialEnhanced) in input
	int TotVentilation( 0 ); // Total Ventilation Statements in input
	int TotDesignFlowVentilation( 0 ); // number of Design Flow rate ZoneVentilation in input
	int TotWindAndStackVentilation( 0 ); // number of wind and stack open area ZoneVentilation in input
	int TotMixing( 0 ); // Total Mixing Statements in input
	int TotCrossMixing( 0 ); // Total Cross Mixing Statements in input
	int TotRefDoorMixing( 0 ); // Total RefrigerationDoor Mixing Statements in input
	int TotBBHeat( 0 ); // Total BBHeat Statements in input
	int TotMaterials( 0 ); // Total number of unique materials (layers) in this simulation
	int TotConstructs( 0 ); // Total number of unique constructions in this simulation
	int TotSpectralData( 0 ); // Total window glass spectral data sets
	int W5GlsMat( 0 ); // Window5 Glass Materials, specified by transmittance and front and back reflectance
	int W5GlsMatAlt( 0 ); // Window5 Glass Materials, specified by index of refraction and extinction coeff
	int W5GasMat( 0 ); // Window5 Single-Gas Materials
	int W5GasMatMixture( 0 ); // Window5 Gas Mixtures
	int W7SupportPillars( 0 ); // Complex fenestration support pillars
	int W7DeflectionStates( 0 ); // Complex fenestration deflection states
	int W7MaterialGaps( 0 ); // Complex fenestration material gaps
	int TotBlinds( 0 ); // Total number of blind materials
	int TotScreens( 0 ); // Total number of exterior window screen materials
	int TotTCGlazings( 0 ); // Number of TC glazing object - WindowMaterial:Glazing:Thermochromic found in the idf file
	int NumSurfaceScreens( 0 ); // Total number of screens on exterior windows
	int TotShades( 0 ); // Total number of shade materials
	int TotComplexShades( 0 ); // Total number of shading materials for complex fenestrations
	int TotComplexGaps( 0 ); // Total number of window gaps for complex fenestrations
	int TotSimpleWindow; // number of simple window systems.

	int W5GlsMatEQL( 0 ); // Window5 Single-Gas Materials for Equivalent Layer window model
	int TotShadesEQL( 0 ); // Total number of shade materials for Equivalent Layer window model
	int TotDrapesEQL( 0 ); // Total number of drape materials for Equivalent Layer window model
	int TotBlindsEQL( 0 ); // Total number of blind materials for Equivalent Layer window model
	int TotScreensEQL( 0 ); // Total number of exterior window screen materials for Equivalent Layer window model
	int W5GapMatEQL( 0 ); // Window5 Equivalent Layer Single-Gas Materials

	int TotZoneAirBalance( 0 ); // Total Zone Air Balance Statements in input
	int TotFrameDivider( 0 ); // Total number of window frame/divider objects
	int AirFlowFlag( 0 );
	int TotCO2Gen( 0 ); // Total CO2 source and sink statements in input
	bool CalcWindowRevealReflection( false ); // True if window reveal reflection is to be calculated
	// for at least one exterior window
	bool StormWinChangeThisDay( false ); // True if a storm window has been added or removed from any
	// window during the current day; can only be true for first
	// time step of the day.
	bool AdaptiveComfortRequested_CEN15251( false ); // true if people objects have adaptive comfort requests. CEN15251
	bool AdaptiveComfortRequested_ASH55( false ); // true if people objects have adaptive comfort requests. ASH55
	int NumRefrigeratedRacks( 0 ); // Total number of refrigerated case compressor racks in input
	int NumRefrigSystems( 0 ); // Total number of detailed refrigeration systems in input
	int NumRefrigCondensers( 0 ); // Total number of detailed refrigeration condensers in input
	int NumRefrigChillerSets( 0 ); // Total number of refrigerated warehouse coils in input
	Array1D< Real64 > SNLoadHeatEnergy;
	Array1D< Real64 > SNLoadCoolEnergy;
	Array1D< Real64 > SNLoadHeatRate;
	Array1D< Real64 > SNLoadCoolRate;
	Array1D< Real64 > SNLoadPredictedRate;
	Array1D< Real64 > SNLoadPredictedHSPRate; // Predicted load to heating setpoint (unmultiplied)
	Array1D< Real64 > SNLoadPredictedCSPRate; // Predicted load to cooling setpoint (unmultiplied)
	Array1D< Real64 > MoisturePredictedRate;

	Array1D< Real64 > ListSNLoadHeatEnergy;
	Array1D< Real64 > ListSNLoadCoolEnergy;
	Array1D< Real64 > ListSNLoadHeatRate;
	Array1D< Real64 > ListSNLoadCoolRate;

	Array1D< Real64 > GroupSNLoadHeatEnergy;
	Array1D< Real64 > GroupSNLoadCoolEnergy;
	Array1D< Real64 > GroupSNLoadHeatRate;
	Array1D< Real64 > GroupSNLoadCoolRate;

	Array1D< Real64 > MRT; // MEAN RADIANT TEMPERATURE (C)
	Array1D< Real64 > SUMAI; // 1 over the Sum of zone areas or 1/SumA
	Array1D< Real64 > ZoneTransSolar; // Exterior beam plus diffuse solar entering zone;
	//   sum of WinTransSolar for exterior windows in zone (W)
	Array1D< Real64 > ZoneWinHeatGain; // Heat gain to zone from all exterior windows (includes
	//   ZoneTransSolar); sum of WinHeatGain for exterior
	//   windows in zone (W)
	Array1D< Real64 > ZoneWinHeatGainRep; // = ZoneWinHeatGain when ZoneWinHeatGain >= 0
	Array1D< Real64 > ZoneWinHeatLossRep; // = -ZoneWinHeatGain when ZoneWinHeatGain < 0
	Array1D< Real64 > ZoneBmSolFrExtWinsRep; // Beam solar into zone from exterior windows [W]
	Array1D< Real64 > ZoneBmSolFrIntWinsRep; // Beam solar into zone from interior windows [W]
	Array1D< Real64 > InitialZoneDifSolReflW; // Initial diffuse solar in zone from ext and int windows
	// reflected from interior surfaces [W]
	Array1D< Real64 > ZoneDifSolFrExtWinsRep; // Diffuse solar into zone from exterior windows [W]
	Array1D< Real64 > ZoneDifSolFrIntWinsRep; // Diffuse solar into zone from interior windows [W]
	Array1D< Real64 > ZoneOpaqSurfInsFaceCond; // Zone inside face opaque surface conduction (W)
	Array1D< Real64 > ZoneOpaqSurfInsFaceCondGainRep; // = Zone inside face opaque surface conduction when >= 0
	Array1D< Real64 > ZoneOpaqSurfInsFaceCondLossRep; // = -Zone inside face opaque surface conduction when < 0
	Array1D< Real64 > ZoneOpaqSurfExtFaceCond; // Zone outside face opaque surface conduction (W)
	Array1D< Real64 > ZoneOpaqSurfExtFaceCondGainRep; // = Zone outside face opaque surface conduction when >= 0
	Array1D< Real64 > ZoneOpaqSurfExtFaceCondLossRep; // = -Zone outside face opaque surface conduction when < 0
	Array1D< Real64 > QRadThermInAbs; // Thermal radiation absorbed on inside surfaces
	Array2D< Real64 > QRadSWwinAbs; // Short wave radiation absorbed in window glass layers
	Array2D< Real64 > InitialDifSolwinAbs; // Initial diffuse solar absorbed in window glass layers
	// from inside(W/m2)
	Array1D< Real64 > QRadSWOutIncident; // Exterior beam plus diffuse solar incident on surface (W/m2)
	Array1D< Real64 > QRadSWOutIncidentBeam; // Exterior beam solar incident on surface (W/m2)
	Array1D< Real64 > BmIncInsSurfIntensRep; // Beam sol irrad from ext wins on inside of surface (W/m2)
	Array1D< Real64 > BmIncInsSurfAmountRep; // Beam sol amount from ext wins incident on inside of surface (W)
	Array1D< Real64 > IntBmIncInsSurfIntensRep; // Beam sol irrad from int wins on inside of surface (W/m2)
	Array1D< Real64 > IntBmIncInsSurfAmountRep; // Beam sol amount from int wins incident on inside of surface (W)
	Array1D< Real64 > QRadSWOutIncidentSkyDiffuse; // Exterior sky diffuse solar incident on surface (W/m2)
	Array1D< Real64 > QRadSWOutIncidentGndDiffuse; // Exterior ground diffuse solar incident on surface (W/m2)
	Array1D< Real64 > QRadSWOutIncBmToDiffReflGnd; // Exterior diffuse solar incident from beam to diffuse
	// reflection from ground (W/m2)
	Array1D< Real64 > QRadSWOutIncSkyDiffReflGnd; // Exterior diffuse solar incident from sky diffuse
	// reflection from ground (W/m2)
	Array1D< Real64 > QRadSWOutIncBmToBmReflObs; // Exterior beam solar incident from beam-to-beam
	// reflection from obstructions (W/m2)
	Array1D< Real64 > QRadSWOutIncBmToDiffReflObs; // Exterior diffuse solar incident from beam-to-diffuse
	// reflection from obstructions (W/m2)
	Array1D< Real64 > QRadSWOutIncSkyDiffReflObs; // Exterior diffuse solar incident from sky diffuse
	// reflection from obstructions (W/m2)
	Array1D< Real64 > CosIncidenceAngle; // Cosine of beam solar incidence angle (for reporting)
	Array1D_int BSDFBeamDirectionRep; // BSDF beam direction number for given complex fenestration state (for reporting) []
	Array1D< Real64 > BSDFBeamThetaRep; // BSDF beam Theta angle (for reporting) [rad]
	Array1D< Real64 > BSDFBeamPhiRep; // BSDF beam Phi angle (for reporting) [rad]

	Array1D< Real64 > QRadSWwinAbsTot; // Exterior beam plus diffuse solar absorbed in glass layers of window (W)
	Array2D< Real64 > QRadSWwinAbsLayer; // Exterior beam plus diffuse solar absorbed in glass layers of window (W)

	Array2D< Real64 > FenLaySurfTempFront; // Front surface temperatures of fenestration layers
	Array2D< Real64 > FenLaySurfTempBack; // Back surface temperatures of fenestration layers
	Array1D< Real64 > ZoneTransSolarEnergy; // Energy of ZoneTransSolar [J]
	Array1D< Real64 > ZoneWinHeatGainRepEnergy; // Energy of ZoneWinHeatGainRep [J]
	Array1D< Real64 > ZoneWinHeatLossRepEnergy; // Energy of ZoneWinHeatLossRep [J]
	Array1D< Real64 > ZoneBmSolFrExtWinsRepEnergy; // Energy of ZoneBmSolFrExtWinsRep [J]
	Array1D< Real64 > ZoneBmSolFrIntWinsRepEnergy; // Energy of ZoneBmSolFrIntWinsRep [J]
	Array1D< Real64 > ZoneDifSolFrExtWinsRepEnergy; // Energy of ZoneDifSolFrExtWinsRep [J]
	Array1D< Real64 > ZoneDifSolFrIntWinsRepEnergy; // Energy of ZoneDifSolFrIntWinsRep [J]
	Array1D< Real64 > ZnOpqSurfInsFaceCondGnRepEnrg; // Energy of ZoneOpaqSurfInsFaceCondGainRep [J]
	Array1D< Real64 > ZnOpqSurfInsFaceCondLsRepEnrg; // Energy of ZoneOpaqSurfInsFaceCondLossRep [J]
	Array1D< Real64 > ZnOpqSurfExtFaceCondGnRepEnrg; // Energy of ZoneOpaqSurfInsFaceCondGainRep [J]
	Array1D< Real64 > ZnOpqSurfExtFaceCondLsRepEnrg; // Energy of ZoneOpaqSurfInsFaceCondLossRep [J]
	Array1D< Real64 > BmIncInsSurfAmountRepEnergy; // energy of BmIncInsSurfAmountRep [J]
	Array1D< Real64 > IntBmIncInsSurfAmountRepEnergy; // energy of IntBmIncInsSurfAmountRep [J]
	Array1D< Real64 > QRadSWwinAbsTotEnergy; // Energy of QRadSWwinAbsTot [J]
	Array1D< Real64 > SWwinAbsTotalReport; // Report - Total interior/exterior shortwave
	//absorbed in all glass layers of window (W)
	Array1D< Real64 > InitialDifSolInAbsReport; // Report - Initial transmitted diffuse solar
	//absorbed on inside of surface (W)
	Array1D< Real64 > InitialDifSolInTransReport; // Report - Initial transmitted diffuse solar
	//transmitted out through inside of window surface (W)
	Array1D< Real64 > SWInAbsTotalReport; // Report - Total interior/exterior shortwave
	//absorbed on inside of surface (W)
	Array1D< Real64 > SWOutAbsTotalReport; // Report - Total exterior shortwave/solar
	//absorbed on outside of surface (W)
	Array1D< Real64 > SWOutAbsEnergyReport; // Report - Total exterior shortwave/solar
	//absorbed on outside of surface (j)

	Array1D< Real64 > NominalR; // Nominal R value of each material -- used in matching interzone surfaces
	Array1D< Real64 > NominalRforNominalUCalculation; // Nominal R values are summed to calculate NominalU values for constructions
	Array1D< Real64 > NominalU; // Nominal U value for each construction -- used in matching interzone surfaces

	// removed variables (these were all arrays):
	//REAL(r64), ALLOCATABLE, :: DifIncInsSurfIntensRep    !Diffuse sol irradiance from ext wins on inside of surface (W/m2)
	//REAL(r64), ALLOCATABLE, :: DifIncInsSurfAmountRep    !Diffuse sol amount from ext wins on inside of surface (W)
	//REAL(r64), ALLOCATABLE, :: IntDifIncInsSurfIntensRep    !Diffuse sol irradiance from int wins on inside of surface (W/m2)
	//REAL(r64), ALLOCATABLE, :: IntDifIncInsSurfAmountRep    !Diffuse sol amount from int wins on inside of surface (W)
	//REAL(r64), ALLOCATABLE, :: DifIncInsSurfAmountRepEnergy    !energy of DifIncInsSurfAmountRep [J]
	//REAL(r64), ALLOCATABLE, :: IntDifIncInsSurfAmountRepEnergy    !energy of IntDifIncInsSurfAmountRep [J]

	// Variables moved from HeatBalanceSurfaceManager and SolarShading
	// to avoid conflict with their use in WindowManager

	Array1D< Real64 > TempEffBulkAir; // air temperature adjacent to the surface used for
	// inside surface heat balances
	Array1D< Real64 > HConvIn; // INSIDE CONVECTION COEFFICIENT
	Array1D< Real64 > AnisoSkyMult; // Multiplier on exterior-surface sky view factor to
	// account for anisotropy of sky radiance; = 1.0 for
	// for isotropic sky

	// Moved from SolarShading to avoid conflicts in DaylightingDevices
	Array1D< Real64 > DifShdgRatioIsoSky; // Diffuse shading ratio (WithShdgIsoSky/WoShdgIsoSky)
	Array3D< Real64 > DifShdgRatioIsoSkyHRTS; // Diffuse shading ratio (WithShdgIsoSky/WoShdgIsoSky)
	Array1D< Real64 > curDifShdgRatioIsoSky; // Diffuse shading ratio (WithShdgIsoSky/WoShdgIsoSky)
	Array1D< Real64 > DifShdgRatioHoriz; // Horizon shading ratio (WithShdgHoriz/WoShdgHoriz)
	Array3D< Real64 > DifShdgRatioHorizHRTS; // Horizon shading ratio (WithShdgHoriz/WoShdgHoriz)
	Array1D< Real64 > WithShdgIsoSky; // Diffuse solar irradiance from sky on surface, with shading
	Array1D< Real64 > WoShdgIsoSky; // Diffuse solar from sky on surface, without shading
	Array1D< Real64 > WithShdgHoriz; // Diffuse solar irradiance from horizon portion of sky on surface,
	// with shading
	Array1D< Real64 > WoShdgHoriz; // Diffuse solar irradiance from horizon portion of sky on surface,
	// without shading
	Array1D< Real64 > MultIsoSky; // Contribution to eff sky view factor from isotropic sky
	Array1D< Real64 > MultCircumSolar; // Contribution to eff sky view factor from circumsolar brightening
	Array1D< Real64 > MultHorizonZenith; // Contribution to eff sky view factor from horizon or zenith brightening

	Array1D< Real64 > QS; // Zone short-wave flux density; used to calculate short-wave
	//     radiation absorbed on inside surfaces of zone
	Array1D< Real64 > QSLights; // Like QS, but Lights short-wave only.

	Array1D< Real64 > QSDifSol; // Like QS, but diffuse solar short-wave only.
	Array1D< Real64 > ITABSF; // FRACTION OF THERMAL FLUX ABSORBED (PER UNIT AREA)
	Array1D< Real64 > TMULT; // TMULT  - MULTIPLIER TO COMPUTE 'ITABSF'
	Array1D< Real64 > QL; // TOTAL THERMAL RADIATION ADDED TO ZONE
	Array2D< Real64 > SunlitFracHR; // Hourly fraction of heat transfer surface that is sunlit
	Array2D< Real64 > CosIncAngHR; // Hourly cosine of beam radiation incidence angle on surface
	Array3D< Real64 > SunlitFrac; // TimeStep fraction of heat transfer surface that is sunlit
	Array3D< Real64 > SunlitFracWithoutReveal; // For a window with reveal, the sunlit fraction
	// without shadowing by the reveal
	Array3D< Real64 > CosIncAng; // TimeStep cosine of beam radiation incidence angle on surface
	Array4D_int BackSurfaces; // For a given hour and timestep, a list of up to 20 surfaces receiving
	// beam solar radiation from a given exterior window
	Array4D< Real64 > OverlapAreas; // For a given hour and timestep, the areas of the exterior window sending
	// beam solar radiation to the surfaces listed in BackSurfaces
	//                       Air       Argon     Krypton   Xenon
	Array2D< Real64 > const GasCoeffsCon( 3, 10, reshape2< Real64, int >( { 2.873e-3, 2.285e-3, 9.443e-4, 4.538e-4, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 7.760e-5, 5.149e-5, 2.826e-5, 1.723e-5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 }, { 3, 10 } ) ); // Gas conductivity coefficients for gases in a mixture // Explicit reshape2 template args are work-around for VC++2013 bug

	//                       Air       Argon     Krypton   Xenon
	Array2D< Real64 > const GasCoeffsVis( 3, 10, reshape2< Real64, int >( { 3.723e-6, 3.379e-6, 2.213e-6, 1.069e-6, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 4.940e-8, 6.451e-8, 7.777e-8, 7.414e-8, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 }, { 3, 10 } ) ); // Gas viscosity coefficients for gases in a mixture // Explicit reshape2 template args are work-around for VC++2013 bug

	//                     Air       Argon     Krypton   Xenon
	Array2D< Real64 > const GasCoeffsCp( 3, 10, reshape2< Real64, int >( { 1002.737, 521.929, 248.091, 158.340, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.2324e-2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 }, { 3, 10 } ) ); // Gas specific heat coefficients for gases in a mixture // Explicit reshape2 template args are work-around for VC++2013 bug

	//                       Air       Argon     Krypton   Xenon
	Array1D< Real64 > const GasWght( 10, { 28.97, 39.948, 83.8, 131.3, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 } ); // Gas molecular weights for gases in a mixture

	Array1D< Real64 > const GasSpecificHeatRatio( 10, { 1.4, 1.67, 1.68, 1.66, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 } ); // Gas specific heat ratios.  Used for gasses in low pressure

	//Variables Dimensioned to Number of Zones
	Array1D< Real64 > MVFC; // Design Mixing Flow Rate [m3/s] (Cross Zone Mixing)
	Array1D< Real64 > MTC; // Control Temperature For Mixing [C] (Cross Zone Mixing)

	Real64 ZeroPointerVal( 0.0 );

	// SUBROUTINE SPECIFICATIONS FOR MODULE DataHeatBalance:

	// Object Data
	Array1D< ZonePreDefRepType > ZonePreDefRep;
	ZonePreDefRepType BuildingPreDefRep; //Autodesk:Note Removed explicit constructor that was missing some entries
	Array1D< ZoneSimData > ZoneIntGain;
	Array1D< MaterialProperties > Material;
	Array1D< GapSupportPillar > SupportPillar;
	Array1D< GapDeflectionState > DeflectionState;
	Array1D< ConstructionData > Construct;
	Array1D< SpectralDataProperties > SpectralData;
	Array1D< ZoneData > Zone;
	Array1D< ZoneListData > ZoneList;
	Array1D< ZoneGroupData > ZoneGroup;
	Array1D< PeopleData > People;
	Array1D< LightsData > Lights;
	Array1D< ZoneEquipData > ZoneElectric;
	Array1D< ZoneEquipData > ZoneGas;
	Array1D< ZoneEquipData > ZoneOtherEq;
	Array1D< ZoneEquipData > ZoneHWEq;
	Array1D< ZoneEquipData > ZoneSteamEq;
	Array1D< ITEquipData > ZoneITEq;
	Array1D< BBHeatData > ZoneBBHeat;
	Array1D< InfiltrationData > Infiltration;
	Array1D< VentilationData > Ventilation;
	Array1D< ZoneAirBalanceData > ZoneAirBalance;
	Array1D< MixingData > Mixing;
	Array1D< MixingData > CrossMixing;
	Array1D< MixingData > RefDoorMixing;
	Array1D< WindowBlindProperties > Blind;
	Array1D< WindowComplexShade > ComplexShade;
	Array1D< WindowThermalModelParams > WindowThermalModel;
	Array1D< SurfaceScreenProperties > SurfaceScreens;
	Array1D< ScreenTransData > ScreenTrans;
	Array1D< ZoneCatEUseData > ZoneIntEEuse;
	Array1D< RefrigCaseCreditData > RefrigCaseCredit;
	Array1D< HeatReclaimRefrigeratedRackData > HeatReclaimRefrigeratedRack;
	Array1D< HeatReclaimRefrigCondenserData > HeatReclaimRefrigCondenser;
	Array1D< HeatReclaimDXCoilData > HeatReclaimDXCoil;
	Array1D< AirReportVars > ZnAirRpt;
	Array1D< TCGlazingsType > TCGlazings;
	Array1D< ZoneEquipData > ZoneCO2Gen;
	Array1D< GlobalInternalGainMiscObject > PeopleObjects;
	Array1D< GlobalInternalGainMiscObject > LightsObjects;
	Array1D< GlobalInternalGainMiscObject > ZoneElectricObjects;
	Array1D< GlobalInternalGainMiscObject > ZoneGasObjects;
	Array1D< GlobalInternalGainMiscObject > HotWaterEqObjects;
	Array1D< GlobalInternalGainMiscObject > SteamEqObjects;
	Array1D< GlobalInternalGainMiscObject > OtherEqObjects;
	Array1D< GlobalInternalGainMiscObject > InfiltrationObjects;
	Array1D< GlobalInternalGainMiscObject > VentilationObjects;
	Array1D< ZoneReportVars > ZnRpt;
	Array1D< ZoneMassConservationData > MassConservation;
	ZoneAirMassFlowConservation ZoneAirMassFlow;

	// Functions

	// Clears the global data in DataHeatBalance.
	// Needed for unit tests, should not be normally called.
	void
	clear_state()
	{
		MaxSolidWinLayers = 0;
		LowHConvLimit = 0.1;
		HighHConvLimit = 1000.0;
		MaxAllowedDelTempCondFD = 0.002;
		BuildingName = std::string();
		BuildingAzimuth = 0.0;
		LoadsConvergTol = 0.0;
		TempConvergTol = 0.0;
		DefaultInsideConvectionAlgo = 1;
		DefaultOutsideConvectionAlgo = 1;
		SolarDistribution = 0;
		InsideSurfIterations = 0;
		OverallHeatTransferSolutionAlgo = UseCTF;
		NumberOfHeatTransferAlgosUsed = 1;
		HeatTransferAlgosUsed.deallocate();
		MaxNumberOfWarmupDays = 25;
		MinNumberOfWarmupDays = 6;
		CondFDRelaxFactor = 1.0;
		CondFDRelaxFactorInput = 1.0;
		ZoneAirSolutionAlgo = Use3rdOrder;
		BuildingRotationAppendixG = 0.0;
		NumOfZoneLists = 0;
		NumOfZoneGroups = 0;
		NumPeopleStatements = 0;
		NumLightsStatements = 0;
		NumZoneElectricStatements = 0;
		NumZoneGasStatements = 0;
		NumInfiltrationStatements = 0;
		NumVentilationStatements = 0;
		NumHotWaterEqStatements = 0;
		NumSteamEqStatements = 0;
		NumOtherEqStatements = 0;
		NumZoneITEqStatements = 0;
		TotPeople = 0;
		TotLights = 0;
		TotElecEquip = 0;
		TotGasEquip = 0;
		TotOthEquip = 0;
		TotHWEquip = 0;
		TotStmEquip = 0;
		TotInfiltration = 0;
		TotDesignFlowInfiltration = 0;
		TotShermGrimsInfiltration = 0;
		TotAIM2Infiltration = 0;
		TotVentilation = 0;
		TotDesignFlowVentilation = 0;
		TotWindAndStackVentilation = 0;
		TotMixing = 0;
		TotCrossMixing = 0;
		TotRefDoorMixing = 0;
		TotBBHeat = 0;
		TotMaterials = 0;
		TotConstructs = 0;
		TotSpectralData = 0;
		W5GlsMat = 0;
		W5GlsMatAlt = 0;
		W5GasMat = 0;
		W5GasMatMixture = 0;
		W7SupportPillars = 0;
		W7DeflectionStates = 0;
		W7MaterialGaps = 0;
		TotBlinds = 0;
		TotScreens = 0;
		TotTCGlazings = 0;
		NumSurfaceScreens = 0;
		TotShades = 0;
		TotComplexShades = 0;
		TotComplexGaps = 0;
		TotSimpleWindow = int();
		W5GlsMatEQL = 0;
		TotShadesEQL = 0;
		TotDrapesEQL = 0;
		TotBlindsEQL = 0;
		TotScreensEQL = 0;
		W5GapMatEQL = 0;
		TotZoneAirBalance = 0;
		TotFrameDivider = 0;
		AirFlowFlag = 0;
		TotCO2Gen = 0;
		CalcWindowRevealReflection = false;
		StormWinChangeThisDay = false;
		AdaptiveComfortRequested_CEN15251 = false;
		AdaptiveComfortRequested_ASH55 = false;
		NumRefrigeratedRacks = 0;
		NumRefrigSystems = 0;
		NumRefrigCondensers = 0;
		NumRefrigChillerSets = 0;
		SNLoadHeatEnergy.deallocate();
		SNLoadCoolEnergy.deallocate();
		SNLoadHeatRate.deallocate();
		SNLoadCoolRate.deallocate();
		SNLoadPredictedRate.deallocate();
		SNLoadPredictedHSPRate.deallocate();
		SNLoadPredictedCSPRate.deallocate();
		MoisturePredictedRate.deallocate();
		ListSNLoadHeatEnergy.deallocate();
		ListSNLoadCoolEnergy.deallocate();
		ListSNLoadHeatRate.deallocate();
		ListSNLoadCoolRate.deallocate();
		GroupSNLoadHeatEnergy.deallocate();
		GroupSNLoadCoolEnergy.deallocate();
		GroupSNLoadHeatRate.deallocate();
		GroupSNLoadCoolRate.deallocate();
		MRT.deallocate();
		SUMAI.deallocate();
		ZoneTransSolar.deallocate();
		ZoneWinHeatGain.deallocate();
		ZoneWinHeatGainRep.deallocate();
		ZoneWinHeatLossRep.deallocate();
		ZoneBmSolFrExtWinsRep.deallocate();
		ZoneBmSolFrIntWinsRep.deallocate();
		InitialZoneDifSolReflW.deallocate();
		ZoneDifSolFrExtWinsRep.deallocate();
		ZoneDifSolFrIntWinsRep.deallocate();
		ZoneOpaqSurfInsFaceCond.deallocate();
		ZoneOpaqSurfInsFaceCondGainRep.deallocate();
		ZoneOpaqSurfInsFaceCondLossRep.deallocate();
		ZoneOpaqSurfExtFaceCond.deallocate();
		ZoneOpaqSurfExtFaceCondGainRep.deallocate();
		ZoneOpaqSurfExtFaceCondLossRep.deallocate();
		QRadThermInAbs.deallocate();
		QRadSWwinAbs.deallocate();
		InitialDifSolwinAbs.deallocate();
		QRadSWOutIncident.deallocate();
		QRadSWOutIncidentBeam.deallocate();
		BmIncInsSurfIntensRep.deallocate();
		BmIncInsSurfAmountRep.deallocate();
		IntBmIncInsSurfIntensRep.deallocate();
		IntBmIncInsSurfAmountRep.deallocate();
		QRadSWOutIncidentSkyDiffuse.deallocate();
		QRadSWOutIncidentGndDiffuse.deallocate();
		QRadSWOutIncBmToDiffReflGnd.deallocate();
		QRadSWOutIncSkyDiffReflGnd.deallocate();
		QRadSWOutIncBmToBmReflObs.deallocate();
		QRadSWOutIncBmToDiffReflObs.deallocate();
		QRadSWOutIncSkyDiffReflObs.deallocate();
		CosIncidenceAngle.deallocate();
		BSDFBeamDirectionRep.deallocate();
		BSDFBeamThetaRep.deallocate();
		BSDFBeamPhiRep.deallocate();
		QRadSWwinAbsTot.deallocate();
		QRadSWwinAbsLayer.deallocate();
		FenLaySurfTempFront.deallocate();
		FenLaySurfTempBack.deallocate();
		ZoneTransSolarEnergy.deallocate();
		ZoneWinHeatGainRepEnergy.deallocate();
		ZoneWinHeatLossRepEnergy.deallocate();
		ZoneBmSolFrExtWinsRepEnergy.deallocate();
		ZoneBmSolFrIntWinsRepEnergy.deallocate();
		ZoneDifSolFrExtWinsRepEnergy.deallocate();
		ZoneDifSolFrIntWinsRepEnergy.deallocate();
		ZnOpqSurfInsFaceCondGnRepEnrg.deallocate();
		ZnOpqSurfInsFaceCondLsRepEnrg.deallocate();
		ZnOpqSurfExtFaceCondGnRepEnrg.deallocate();
		ZnOpqSurfExtFaceCondLsRepEnrg.deallocate();
		BmIncInsSurfAmountRepEnergy.deallocate();
		IntBmIncInsSurfAmountRepEnergy.deallocate();
		QRadSWwinAbsTotEnergy.deallocate();
		SWwinAbsTotalReport.deallocate();
		InitialDifSolInAbsReport.deallocate();
		InitialDifSolInTransReport.deallocate();
		SWInAbsTotalReport.deallocate();
		SWOutAbsTotalReport.deallocate();
		SWOutAbsEnergyReport.deallocate();
		NominalR.deallocate();
		NominalRforNominalUCalculation.deallocate();
		NominalU.deallocate();
		TempEffBulkAir.deallocate();
		HConvIn.deallocate();
		AnisoSkyMult.deallocate();
		DifShdgRatioIsoSky.deallocate();
		DifShdgRatioIsoSkyHRTS.deallocate();
		curDifShdgRatioIsoSky.deallocate();
		DifShdgRatioHoriz.deallocate();
		DifShdgRatioHorizHRTS.deallocate();
		WithShdgIsoSky.deallocate();
		WoShdgIsoSky.deallocate();
		WithShdgHoriz.deallocate();
		WoShdgHoriz.deallocate();
		MultIsoSky.deallocate();
		MultCircumSolar.deallocate();
		MultHorizonZenith.deallocate();
		QS.deallocate();
		QSLights.deallocate();
		QSDifSol.deallocate();
		ITABSF.deallocate();
		TMULT.deallocate();
		QL.deallocate();
		SunlitFracHR.deallocate();
		CosIncAngHR.deallocate();
		SunlitFrac.deallocate();
		SunlitFracWithoutReveal.deallocate();
		CosIncAng.deallocate();
		BackSurfaces.deallocate();
		OverlapAreas.deallocate();
		MVFC.deallocate();
		MTC.deallocate();
		ZeroPointerVal = 0.0;
		ZonePreDefRep.deallocate();
		BuildingPreDefRep = ZonePreDefRepType();
		ZoneIntGain.deallocate();
		Material.deallocate();
		SupportPillar.deallocate();
		DeflectionState.deallocate();
		Construct.deallocate();
		SpectralData.deallocate();
		Zone.deallocate();
		ZoneList.deallocate();
		ZoneGroup.deallocate();
		People.deallocate();
		Lights.deallocate();
		ZoneElectric.deallocate();
		ZoneGas.deallocate();
		ZoneOtherEq.deallocate();
		ZoneHWEq.deallocate();
		ZoneSteamEq.deallocate();
		ZoneITEq.deallocate();
		ZoneBBHeat.deallocate();
		Infiltration.deallocate();
		Ventilation.deallocate();
		ZoneAirBalance.deallocate();
		Mixing.deallocate();
		CrossMixing.deallocate();
		RefDoorMixing.deallocate();
		Blind.deallocate();
		ComplexShade.deallocate();
		WindowThermalModel.deallocate();
		SurfaceScreens.deallocate();
		ScreenTrans.deallocate();
		ZoneIntEEuse.deallocate();
		RefrigCaseCredit.deallocate();
		HeatReclaimRefrigeratedRack.deallocate();
		HeatReclaimRefrigCondenser.deallocate();
		HeatReclaimDXCoil.deallocate();
		ZnAirRpt.deallocate();
		TCGlazings.deallocate();
		ZoneCO2Gen.deallocate();
		PeopleObjects.deallocate();
		LightsObjects.deallocate();
		ZoneElectricObjects.deallocate();
		ZoneGasObjects.deallocate();
		HotWaterEqObjects.deallocate();
		SteamEqObjects.deallocate();
		OtherEqObjects.deallocate();
		InfiltrationObjects.deallocate();
		VentilationObjects.deallocate();
		ZnRpt.deallocate();
		MassConservation.deallocate();
		ZoneAirMassFlow = ZoneAirMassFlowConservation();
	}

	void
	ZoneData::SetOutBulbTempAt()
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Noel Keen (LBL)/Linda Lawrie
		//       DATE WRITTEN   August 2010
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Routine provides facility for doing bulk Set Temperature at Height.

		// Using/Aliasing
		using DataEnvironment::EarthRadius;
		using DataEnvironment::SiteTempGradient;
		using DataEnvironment::WeatherFileTempModCoeff;

		if ( SiteTempGradient == 0.0 ) {
			OutDryBulbTemp = DataEnvironment::OutDryBulbTemp;
			OutWetBulbTemp = DataEnvironment::OutWetBulbTemp;
		} else {
			// Base temperatures at Z = 0 (C)
			Real64 const BaseDryTemp( DataEnvironment::OutDryBulbTemp + WeatherFileTempModCoeff );
			Real64 const BaseWetTemp( DataEnvironment::OutWetBulbTemp + WeatherFileTempModCoeff );

			Real64 const Z( Centroid.z ); // Centroid value
			if ( Z <= 0.0 ) {
				OutDryBulbTemp = BaseDryTemp;
				OutWetBulbTemp = BaseWetTemp;
			} else {
				OutDryBulbTemp = BaseDryTemp - SiteTempGradient * EarthRadius * Z / ( EarthRadius + Z );
				OutWetBulbTemp = BaseWetTemp - SiteTempGradient * EarthRadius * Z / ( EarthRadius + Z );
			}
		}
	}

	void
	ZoneData::SetWindSpeedAt( Real64 const fac )
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   June 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Routine provides facility for doing bulk Set Windspeed at Height.

		// Using/Aliasing
		using DataEnvironment::SiteWindExp;

		if ( SiteWindExp == 0.0 ) {
			WindSpeed = DataEnvironment::WindSpeed;
		} else {
			Real64 const Z( Centroid.z ); // Centroid value
			if ( Z <= 0.0 ) {
				WindSpeed = 0.0;
			} else {
				//  [Met] - at meterological Station, Height of measurement is usually 10m above ground
				//  LocalWindSpeed = Windspeed [Met] * (Wind Boundary LayerThickness [Met]/Height [Met])**Wind Exponent[Met] &
				//                     * (Height above ground / Site Wind Boundary Layer Thickness) ** Site Wind Exponent
				WindSpeed = fac * std::pow( Z, SiteWindExp );
			}
		}
	}

	void
	SetZoneOutBulbTempAt()
	{
		for ( auto & zone : Zone ) {
			zone.SetOutBulbTempAt();
		}
	}

	void
	CheckZoneOutBulbTempAt() 
	{
		// Using/Aliasing
		using DataEnvironment::SetOutBulbTempAt_error;

		Real64 minBulb = 0.0;
		for ( auto & zone : Zone ) {
			minBulb = min( minBulb, zone.OutDryBulbTemp, zone.OutWetBulbTemp );
			if ( minBulb < -100.0 ) SetOutBulbTempAt_error( "Zone", zone.Centroid.z, zone.Name );
		}
	}

	void
	SetZoneWindSpeedAt()
	{
		// Using/Aliasing
		using DataEnvironment::SiteWindBLHeight;
		using DataEnvironment::SiteWindExp;
		using DataEnvironment::WeatherFileWindModCoeff;

		Real64 const fac( DataEnvironment::WindSpeed * WeatherFileWindModCoeff * std::pow( SiteWindBLHeight, -SiteWindExp ) );
		for ( auto & zone : Zone ) {
			zone.SetWindSpeedAt( fac );
		}
	}

	void
	CheckAndSetConstructionProperties(
		int const ConstrNum, // Construction number to be set/checked
		bool & ErrorsFound // error flag that is set when certain errors have occurred
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   December 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This routine checks some properties of entered constructions; sets some properties; and sets
		// an error flag for certain error conditions.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int InsideLayer; // Inside Layer of Construct; for window construct, layer no. of inside glass
		int MaterNum; // Counters to keep track of the material number for a layer
		int OutsideMaterNum; // Material "number" of the Outside layer
		int InsideMaterNum; // Material "number" of the Inside layer
		int Layer; // loop index for each of the construction layers
		int TotLayers; // Number of layers in a construction
		int TotGlassLayers; // Number of glass layers in a construction
		int TotShadeLayers; // Number of shade layers in a construction
		int TotGasLayers; // Number of gas layers in a construction
		bool WrongMaterialsMix; // True if window construction has a layer that is not glass, gas or shade
		bool WrongWindowLayering; // True if error in layering of a window construction
		int MaterNumNext; // Next material number in the layer sequence
		int IGas; // Index for gases in a mixture of gases in a window gap
		int LayNumSh; // Number of shade/blind layer in a construction
		int MatSh; // Material number of a shade/blind layer
		int MatGapL; // Material number of the gas layer to the left (outer side) of a shade/blind layer
		int MatGapR; // Material number of the gas layer to the right (innner side) of a shade/blind layer
		int BlNum; // Blind number
		bool ValidBGShadeBlindConst; // True if a valid window construction with between-glass shade/blind
		int GlassLayNum; // Glass layer number

		TotLayers = Construct( ConstrNum ).TotLayers;
		InsideLayer = TotLayers;
		if ( Construct( ConstrNum ).LayerPoint( InsideLayer ) <= 0 ) return; // Error condition
		if ( TotLayers == 0 ) return; // error condition, hopefully caught elsewhere

		//   window screen is not allowed on inside layer

		Construct( ConstrNum ).DayltPropPtr = 0;
		InsideMaterNum = Construct( ConstrNum ).LayerPoint( InsideLayer );
		if ( InsideMaterNum != 0 ) {
			Construct( ConstrNum ).InsideAbsorpVis = Material( InsideMaterNum ).AbsorpVisible;
			Construct( ConstrNum ).InsideAbsorpSolar = Material( InsideMaterNum ).AbsorpSolar;

			// Following line applies only to opaque surfaces; it is recalculated later for windows.
			Construct( ConstrNum ).ReflectVisDiffBack = 1.0 - Material( InsideMaterNum ).AbsorpVisible;
		}

		OutsideMaterNum = Construct( ConstrNum ).LayerPoint( 1 );
		if ( OutsideMaterNum != 0 ) {
			Construct( ConstrNum ).OutsideAbsorpVis = Material( OutsideMaterNum ).AbsorpVisible;
			Construct( ConstrNum ).OutsideAbsorpSolar = Material( OutsideMaterNum ).AbsorpSolar;
		}

		Construct( ConstrNum ).TotSolidLayers = 0;
		Construct( ConstrNum ).TotGlassLayers = 0;
		Construct( ConstrNum ).AbsDiffShade = 0.0;

		// Check if any layer is glass, gas, shade, screen or blind; if so it is considered a window construction for
		// purposes of error checking.

		Construct( ConstrNum ).TypeIsWindow = false;
		for ( Layer = 1; Layer <= TotLayers; ++Layer ) {
			MaterNum = Construct( ConstrNum ).LayerPoint( Layer );
			if ( MaterNum == 0 ) continue; // error -- has been caught will stop program later
			switch ( Material( MaterNum ).Group ) {
				case WindowGlass:
				case WindowGas:
				case WindowGasMixture:
				case Shade:
				case WindowBlind:
				case Screen:
				case WindowSimpleGlazing:
				case ComplexWindowShade:
				case ComplexWindowGap:
				case GlassEquivalentLayer:
				case ShadeEquivalentLayer:
				case DrapeEquivalentLayer:
				case ScreenEquivalentLayer:
				case BlindEquivalentLayer:
				case GapEquivalentLayer:
					Construct( ConstrNum ).TypeIsWindow = true;
			}
		}

		if ( InsideMaterNum == 0 ) return;
		if ( OutsideMaterNum == 0 ) return;

		if ( Construct( ConstrNum ).TypeIsWindow ) {

			Construct( ConstrNum ).NumCTFTerms = 0;
			Construct( ConstrNum ).NumHistories = 0;
			WrongMaterialsMix = false;
			WrongWindowLayering = false;
			for ( Layer = 1; Layer <= TotLayers; ++Layer ) {
				MaterNum = Construct( ConstrNum ).LayerPoint( Layer );
				if ( MaterNum == 0 ) continue; // error -- has been caught will stop program later
				switch ( Material( MaterNum ).Group ) {
					case WindowGlass:
					case WindowGas:
					case WindowGasMixture:
					case Shade:
					case WindowBlind:
					case Screen:
					case WindowSimpleGlazing:
					case ComplexWindowShade:
					case ComplexWindowGap:
					case GlassEquivalentLayer:
					case ShadeEquivalentLayer:
					case DrapeEquivalentLayer:
					case ScreenEquivalentLayer:
					case BlindEquivalentLayer:
					case GapEquivalentLayer:
						break; // everything is OK
					default:
						WrongMaterialsMix = true; // found a bad one
				}
			}

			if ( WrongMaterialsMix ) { //Illegal material for a window construction
				ShowSevereError( "Error: Window construction=" + Construct( ConstrNum ).Name + " has materials other than glass, gas, shade, screen, blind, complex shading, complex gap, or simple system." );
				ErrorsFound = true;
				// Do not check number of layers for BSDF type of window since that can be handled
			} else if ( ( TotLayers > 8 ) && ( ! Construct( ConstrNum ).WindowTypeBSDF ) && ( ! Construct( ConstrNum ).WindowTypeEQL ) ) { //Too many layers for a window construction
				ShowSevereError( "CheckAndSetConstructionProperties: Window construction=" + Construct( ConstrNum ).Name + " has too many layers (max of 8 allowed -- 4 glass + 3 gap + 1 shading device)." );
				ErrorsFound = true;

			} else if ( TotLayers == 1 ) {

				if ( Material( Construct( ConstrNum ).LayerPoint( 1 ) ).Group == Shade || Material( Construct( ConstrNum ).LayerPoint( 1 ) ).Group == WindowGas || Material( Construct( ConstrNum ).LayerPoint( 1 ) ).Group == WindowGasMixture || Material( Construct( ConstrNum ).LayerPoint( 1 ) ).Group == WindowBlind || Material( Construct( ConstrNum ).LayerPoint( 1 ) ).Group == Screen || Material( Construct( ConstrNum ).LayerPoint( 1 ) ).Group == ComplexWindowShade || Material( Construct( ConstrNum ).LayerPoint( 1 ) ).Group == ComplexWindowGap ) {
					ShowSevereError( "CheckAndSetConstructionProperties: The single-layer window construction=" + Construct( ConstrNum ).Name + " has a gas, complex gap, shade, complex shade, screen or blind material; it should be glass of simple glazing system." );
					ErrorsFound = true;
				}
			}

			// Find total glass layers, total shade/blind layers and total gas layers in a window construction

			TotGlassLayers = 0;
			TotShadeLayers = 0; // Includes shades, blinds, and screens
			TotGasLayers = 0;
			for ( Layer = 1; Layer <= TotLayers; ++Layer ) {
				MaterNum = Construct( ConstrNum ).LayerPoint( Layer );
				if ( MaterNum == 0 ) continue; // error -- has been caught will stop program later
				if ( Material( MaterNum ).Group == WindowGlass ) ++TotGlassLayers;
				if ( Material( MaterNum ).Group == WindowSimpleGlazing ) ++TotGlassLayers;
				if ( Material( MaterNum ).Group == Shade || Material( MaterNum ).Group == WindowBlind || Material( MaterNum ).Group == Screen || Material( MaterNum ).Group == ComplexWindowShade ) ++TotShadeLayers;
				if ( Material( MaterNum ).Group == WindowGas || Material( MaterNum ).Group == WindowGasMixture || Material( MaterNum ).Group == ComplexWindowGap ) ++TotGasLayers;
				if ( Layer < TotLayers ) {
					MaterNumNext = Construct( ConstrNum ).LayerPoint( Layer + 1 );
					// Adjacent layers of same type not allowed
					if ( MaterNumNext == 0 ) continue;
					if ( Material( MaterNum ).Group == Material( MaterNumNext ).Group ) WrongWindowLayering = true;
				}
			}

			// It is not necessary to check rest of BSDF window structure since that is performed inside TARCOG90 routine.
			// That routine also allow structures which are not allowed in rest of this routine
			if ( Construct( ConstrNum ).WindowTypeBSDF ) {
				Construct( ConstrNum ).TotGlassLayers = TotGlassLayers;
				Construct( ConstrNum ).TotSolidLayers = TotGlassLayers + TotShadeLayers;
				Construct( ConstrNum ).InsideAbsorpThermal = Material( Construct( ConstrNum ).LayerPoint( InsideLayer ) ).AbsorpThermalBack;
				Construct( ConstrNum ).OutsideAbsorpThermal = Material( Construct( ConstrNum ).LayerPoint( 1 ) ).AbsorpThermalFront;
				return;
			}

			if ( Construct( ConstrNum ).WindowTypeEQL ) {
				Construct( ConstrNum ).InsideAbsorpThermal = Material( Construct( ConstrNum ).LayerPoint( InsideLayer ) ).AbsorpThermalBack;
				Construct( ConstrNum ).OutsideAbsorpThermal = Material( Construct( ConstrNum ).LayerPoint( 1 ) ).AbsorpThermalFront;
				return;
			}

			if ( Material( Construct( ConstrNum ).LayerPoint( 1 ) ).Group == WindowGas || Material( Construct( ConstrNum ).LayerPoint( 1 ) ).Group == WindowGasMixture || Material( Construct( ConstrNum ).LayerPoint( TotLayers ) ).Group == WindowGas || Material( Construct( ConstrNum ).LayerPoint( TotLayers ) ).Group == WindowGasMixture ) WrongWindowLayering = true; // Gas cannot be first or last layer
			if ( TotShadeLayers > 1 ) WrongWindowLayering = true; //At most one shade, screen or blind allowed

			// If there is a diffusing glass layer no shade, screen or blind is allowed
			for ( Layer = 1; Layer <= TotLayers; ++Layer ) {
				MaterNum = Construct( ConstrNum ).LayerPoint( Layer );
				if ( MaterNum == 0 ) continue; // error -- has been caught will stop program later
				if ( Material( MaterNum ).SolarDiffusing && TotShadeLayers > 0 ) {
					ErrorsFound = true;
					ShowSevereError( "CheckAndSetConstructionProperties: Window construction=" + Construct( ConstrNum ).Name );
					ShowContinueError( "has diffusing glass=" + Material( MaterNum ).Name + " and a shade, screen or blind layer." );
					break;
				}
			}

			// If there is a diffusing glass layer it must be the innermost layer
			if ( TotGlassLayers > 1 ) {
				GlassLayNum = 0;
				for ( Layer = 1; Layer <= TotLayers; ++Layer ) {
					MaterNum = Construct( ConstrNum ).LayerPoint( Layer );
					if ( MaterNum == 0 ) continue; // error -- has been caught will stop program later
					if ( Material( MaterNum ).Group == WindowGlass ) {
						++GlassLayNum;
						if ( GlassLayNum < TotGlassLayers && Material( MaterNum ).SolarDiffusing ) {
							ErrorsFound = true;
							ShowSevereError( "CheckAndSetConstructionProperties: Window construction=" + Construct( ConstrNum ).Name );
							ShowContinueError( "has diffusing glass=" + Material( MaterNum ).Name + " that is not the innermost glass layer." );
						}
					}
				}
			}

			// interior window screen is not allowed. Check for invalid between-glass screen is checked below.
			if ( TotShadeLayers == 1 && Material( Construct( ConstrNum ).LayerPoint( TotLayers ) ).Group == Screen && TotLayers != 1 ) {
				WrongWindowLayering = true;
			}

			// Consistency checks for a construction with a between-glass shade or blind

			if ( TotShadeLayers == 1 && Material( Construct( ConstrNum ).LayerPoint( 1 ) ).Group != Shade && Material( Construct( ConstrNum ).LayerPoint( 1 ) ).Group != WindowBlind && Material( Construct( ConstrNum ).LayerPoint( 1 ) ).Group != Screen && Material( Construct( ConstrNum ).LayerPoint( TotLayers ) ).Group != Shade && Material( Construct( ConstrNum ).LayerPoint( TotLayers ) ).Group != WindowBlind && Material( Construct( ConstrNum ).LayerPoint( TotLayers ) ).Group != ComplexWindowShade && ! WrongWindowLayering ) {

				// This is a construction with a between-glass shade or blind

				if ( TotGlassLayers >= 4 ) {
					// Quadruple pane not allowed.
					WrongWindowLayering = true;
				} else if ( TotGlassLayers == 2 || TotGlassLayers == 3 ) {
					ValidBGShadeBlindConst = false;
					if ( TotGlassLayers == 2 ) {
						if ( TotLayers != 5 ) {
							WrongWindowLayering = true;
						} else {
							if ( Material( Construct( ConstrNum ).LayerPoint( 1 ) ).Group == WindowGlass && ( Material( Construct( ConstrNum ).LayerPoint( 2 ) ).Group == WindowGas || Material( Construct( ConstrNum ).LayerPoint( 2 ) ).Group == WindowGasMixture ) && ( ( Material( Construct( ConstrNum ).LayerPoint( 3 ) ).Group == Shade || Material( Construct( ConstrNum ).LayerPoint( 3 ) ).Group == WindowBlind ) && Material( Construct( ConstrNum ).LayerPoint( 3 ) ).Group != Screen ) && ( Material( Construct( ConstrNum ).LayerPoint( 4 ) ).Group == WindowGas || Material( Construct( ConstrNum ).LayerPoint( 4 ) ).Group == WindowGasMixture ) && Material( Construct( ConstrNum ).LayerPoint( 5 ) ).Group == WindowGlass ) ValidBGShadeBlindConst = true;
						}
					} else { // TotGlassLayers = 3
						if ( TotLayers != 7 ) {
							WrongWindowLayering = true;
						} else {
							if ( Material( Construct( ConstrNum ).LayerPoint( 1 ) ).Group == WindowGlass && ( Material( Construct( ConstrNum ).LayerPoint( 2 ) ).Group == WindowGas || Material( Construct( ConstrNum ).LayerPoint( 2 ) ).Group == WindowGasMixture ) && Material( Construct( ConstrNum ).LayerPoint( 3 ) ).Group == WindowGlass && ( Material( Construct( ConstrNum ).LayerPoint( 4 ) ).Group == WindowGas || Material( Construct( ConstrNum ).LayerPoint( 4 ) ).Group == WindowGasMixture ) && ( ( Material( Construct( ConstrNum ).LayerPoint( 5 ) ).Group == Shade || Material( Construct( ConstrNum ).LayerPoint( 5 ) ).Group == WindowBlind ) && Material( Construct( ConstrNum ).LayerPoint( 5 ) ).Group != Screen ) && ( Material( Construct( ConstrNum ).LayerPoint( 6 ) ).Group == WindowGas || Material( Construct( ConstrNum ).LayerPoint( 6 ) ).Group == WindowGasMixture ) && Material( Construct( ConstrNum ).LayerPoint( 7 ) ).Group == WindowGlass ) ValidBGShadeBlindConst = true;
						}
					} // End of check if TotGlassLayers = 2 or 3
					if ( ! ValidBGShadeBlindConst ) WrongWindowLayering = true;
					if ( ! WrongWindowLayering ) {
						LayNumSh = 2 * TotGlassLayers - 1;
						MatSh = Construct( ConstrNum ).LayerPoint( LayNumSh );
						// For double pane, shade/blind must be layer #3.
						// For triple pane, it must be layer #5 (i.e., between two inner panes).
						if ( Material( MatSh ).Group != Shade && Material( MatSh ).Group != WindowBlind ) WrongWindowLayering = true;
						if ( TotLayers != 2 * TotGlassLayers + 1 ) WrongWindowLayering = true;
						if ( ! WrongWindowLayering ) {
							// Gas on either side of a between-glass shade/blind must be the same
							MatGapL = Construct( ConstrNum ).LayerPoint( LayNumSh - 1 );
							MatGapR = Construct( ConstrNum ).LayerPoint( LayNumSh + 1 );
							for ( IGas = 1; IGas <= 5; ++IGas ) {
								if ( ( Material( MatGapL ).GasType( IGas ) != Material( MatGapR ).GasType( IGas ) ) || ( Material( MatGapL ).GasFract( IGas ) != Material( MatGapR ).GasFract( IGas ) ) ) WrongWindowLayering = true;
							}
							// Gap width on either side of a between-glass shade/blind must be the same
							if ( std::abs( Material( MatGapL ).Thickness - Material( MatGapR ).Thickness ) > 0.0005 ) WrongWindowLayering = true;
							if ( Material( MatSh ).Group == WindowBlind ) {
								BlNum = Material( MatSh ).BlindDataPtr;
								if ( BlNum > 0 ) {
									if ( ( Material( MatGapL ).Thickness + Material( MatGapR ).Thickness ) < Blind( BlNum ).SlatWidth ) {
										ErrorsFound = true;
										ShowSevereError( "CheckAndSetConstructionProperties: For window construction " + Construct( ConstrNum ).Name );
										ShowContinueError( "the slat width of the between-glass blind is greater than" );
										ShowContinueError( "the sum of the widths of the gas layers adjacent to the blind." );
									}
								} // End of check if BlNum > 0
							} // End of check if material is window blind
						} // End of check if WrongWindowLayering
					} // End of check if WrongWindowLayering
				} // End of check on total glass layers
			} // End of check if construction has between-glass shade/blind

			// Check Simple Windows,
			if ( Material( Construct( ConstrNum ).LayerPoint( 1 ) ).Group == WindowSimpleGlazing ) {
				if ( TotLayers > 1 ) {
					// check that none of the other layers are glazing or gas
					for ( Layer = 1; Layer <= TotLayers; ++Layer ) {
						MaterNum = Construct( ConstrNum ).LayerPoint( Layer );
						if ( MaterNum == 0 ) continue; // error -- has been caught will stop program later
						if ( Material( MaterNum ).Group == WindowGlass ) {
							ErrorsFound = true;
							ShowSevereError( "CheckAndSetConstructionProperties: Error in window construction " + Construct( ConstrNum ).Name + "--" );
							ShowContinueError( "For simple window constructions, no other glazing layers are allowed." );
						}
						if ( Material( MaterNum ).Group == WindowGas ) {
							ErrorsFound = true;
							ShowSevereError( "CheckAndSetConstructionProperties: Error in window construction " + Construct( ConstrNum ).Name + "--" );
							ShowContinueError( "For simple window constructions, no other gas layers are allowed." );
						}
					}
				}
			}

			if ( WrongWindowLayering ) {
				ShowSevereError( "CheckAndSetConstructionProperties: Error in window construction " + Construct( ConstrNum ).Name + "--" );
				ShowContinueError( "  For multi-layer window constructions the following rules apply:" );
				ShowContinueError( "    --The first and last layer must be a solid layer (glass or shade/screen/blind)," );
				ShowContinueError( "    --Adjacent glass layers must be separated by one and only one gas layer," );
				ShowContinueError( "    --Adjacent layers must not be of the same type," );
				ShowContinueError( "    --Only one shade/screen/blind layer is allowed," );
				ShowContinueError( "    --An exterior shade/screen/blind must be the first layer," );
				ShowContinueError( "    --An interior shade/blind must be the last layer," );
				ShowContinueError( "    --An interior screen is not allowed," );
				ShowContinueError( "    --For an exterior shade/screen/blind or interior shade/blind, there should not be a gas layer" );
				ShowContinueError( "    ----between the shade/screen/blind and adjacent glass," );
				ShowContinueError( "    --A between-glass screen is not allowed," );
				ShowContinueError( "    --A between-glass shade/blind is allowed only for double and triple glazing," );
				ShowContinueError( "    --A between-glass shade/blind must have adjacent gas layers of the same type and width," );
				ShowContinueError( "    --For triple glazing the between-glass shade/blind must be between the two inner glass layers," );
				ShowContinueError( "    --The slat width of a between-glass blind must be less than the sum of the widths" );
				ShowContinueError( "    ----of the gas layers adjacent to the blind." );
				ErrorsFound = true;
			}

			Construct( ConstrNum ).TotGlassLayers = TotGlassLayers;
			Construct( ConstrNum ).TotSolidLayers = TotGlassLayers + TotShadeLayers;

			// In following, InsideLayer is layer number of inside glass and InsideAbsorpThermal applies
			// only to inside glass; it is corrected later in InitGlassOpticalCalculations
			// if construction has inside shade or blind.
			if ( Material( Construct( ConstrNum ).LayerPoint( InsideLayer ) ).Group == Shade || Material( Construct( ConstrNum ).LayerPoint( InsideLayer ) ).Group == WindowBlind ) {
				--InsideLayer;
			}
			if ( InsideLayer > 0 ) {
				InsideMaterNum = Construct( ConstrNum ).LayerPoint( InsideLayer );
				Construct( ConstrNum ).InsideAbsorpThermal = Material( Construct( ConstrNum ).LayerPoint( InsideLayer ) ).AbsorpThermalBack;
			}
			if ( InsideMaterNum != 0 ) {
				Construct( ConstrNum ).InsideAbsorpVis = Material( InsideMaterNum ).AbsorpVisible;
				Construct( ConstrNum ).InsideAbsorpSolar = Material( InsideMaterNum ).AbsorpSolar;
			}

			if ( ( Material( Construct( ConstrNum ).LayerPoint( 1 ) ).Group == WindowGlass ) || ( Material( Construct( ConstrNum ).LayerPoint( 1 ) ).Group == WindowSimpleGlazing ) ) { //Glass
				Construct( ConstrNum ).OutsideAbsorpThermal = Material( Construct( ConstrNum ).LayerPoint( 1 ) ).AbsorpThermalFront;
			} else { //Exterior shade, blind or screen
				Construct( ConstrNum ).OutsideAbsorpThermal = Material( Construct( ConstrNum ).LayerPoint( 1 ) ).AbsorpThermal;
			}

		} else { //Opaque surface
			Construct( ConstrNum ).InsideAbsorpThermal = Material( Construct( ConstrNum ).LayerPoint( InsideLayer ) ).AbsorpThermal;
			Construct( ConstrNum ).OutsideAbsorpThermal = Material( Construct( ConstrNum ).LayerPoint( 1 ) ).AbsorpThermal;
		}

		Construct( ConstrNum ).OutsideRoughness = Material( Construct( ConstrNum ).LayerPoint( 1 ) ).Roughness;

		if ( Material( Construct( ConstrNum ).LayerPoint( 1 ) ).Group == Air ) {
			ShowSevereError( "CheckAndSetConstructionProperties: Outside Layer is Air for construction " + Construct( ConstrNum ).Name );
			ShowContinueError( "  Error in material " + Material( Construct( ConstrNum ).LayerPoint( 1 ) ).Name );
			ErrorsFound = true;
		}
		if ( InsideLayer > 0 ) {
			if ( Material( Construct( ConstrNum ).LayerPoint( InsideLayer ) ).Group == Air ) {
				ShowSevereError( "CheckAndSetConstructionProperties: Inside Layer is Air for construction " + Construct( ConstrNum ).Name );
				ShowContinueError( "  Error in material " + Material( Construct( ConstrNum ).LayerPoint( InsideLayer ) ).Name );
				ErrorsFound = true;
			}
		}

		if ( Material( Construct( ConstrNum ).LayerPoint( 1 ) ).Group == EcoRoof ) {
			Construct( ConstrNum ).TypeIsEcoRoof = true;
			//need to check EcoRoof is not non-outside layer
			for ( Layer = 2; Layer <= TotLayers; ++Layer ) {
				if ( Material( Construct( ConstrNum ).LayerPoint( Layer ) ).Group == EcoRoof ) {
					ShowSevereError( "CheckAndSetConstructionProperties: Interior Layer is EcoRoof for construction " + Construct( ConstrNum ).Name );
					ShowContinueError( "  Error in material " + Material( Construct( ConstrNum ).LayerPoint( Layer ) ).Name );
					ErrorsFound = true;
				}
			}
		}

		if ( Material( Construct( ConstrNum ).LayerPoint( 1 ) ).Group == IRTMaterial ) {
			Construct( ConstrNum ).TypeIsIRT = true;
			if ( Construct( ConstrNum ).TotLayers != 1 ) {
				ShowSevereError( "CheckAndSetConstructionProperties: Infrared Transparent (IRT) Construction is limited to 1 layer " + Construct( ConstrNum ).Name );
				ShowContinueError( "  Too many layers in referenced construction." );
				ErrorsFound = true;
			}
		}

	}

	int
	AssignReverseConstructionNumber(
		int const ConstrNum, // Existing Construction number of first surface
		bool & ErrorsFound
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   December 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// For interzone, unentered surfaces, we need to have "reverse" constructions
		// assigned to the created surfaces.  These need to be the reverse (outside to inside layer)
		// of existing surfaces.  Plus, there may be one already in the data structure so this is looked for as well.

		// METHODOLOGY EMPLOYED:
		// Create reverse layers.  Look in current constructions to see if match.  If no match, create a new one.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		int NewConstrNum; // Reverse Construction Number

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		static Array1D_int LayerPoint( MaxLayersInConstruct, 0 ); // Pointer array which refers back to
		int nLayer;
		int Loop;
		bool Found;

		if ( ConstrNum == 0 ) {
			// error caught elsewhere
			NewConstrNum = 0;
			return NewConstrNum;
		}

		Construct( ConstrNum ).IsUsed = true;
		nLayer = 0;
		LayerPoint = 0;
		for ( Loop = Construct( ConstrNum ).TotLayers; Loop >= 1; --Loop ) {
			++nLayer;
			LayerPoint( nLayer ) = Construct( ConstrNum ).LayerPoint( Loop );
		}

		// now, got thru and see if there is a match already....
		NewConstrNum = 0;
		for ( Loop = 1; Loop <= TotConstructs; ++Loop ) {
			Found = true;
			for ( nLayer = 1; nLayer <= MaxLayersInConstruct; ++nLayer ) {
				if ( Construct( Loop ).LayerPoint( nLayer ) != LayerPoint( nLayer ) ) {
					Found = false;
					break;
				}
			}
			if ( Found ) {
				NewConstrNum = Loop;
				break;
			}
		}

		// if need new one, bunch o stuff
		if ( NewConstrNum == 0 ) {
			++TotConstructs;
			Construct.redimension( TotConstructs );
			NominalRforNominalUCalculation.redimension( TotConstructs );
			NominalRforNominalUCalculation( TotConstructs ) = 0.0;
			NominalU.redimension( TotConstructs );
			NominalU( TotConstructs ) = 0.0;
			//  Put in new attributes
			NewConstrNum = TotConstructs;
			Construct( NewConstrNum ).IsUsed = true;
			Construct( TotConstructs ) = Construct( ConstrNum ); // preserve some of the attributes.
			// replace others...
			Construct( TotConstructs ).Name = "iz-" + Construct( ConstrNum ).Name;
			Construct( TotConstructs ).TotLayers = Construct( ConstrNum ).TotLayers;
			for ( nLayer = 1; nLayer <= MaxLayersInConstruct; ++nLayer ) {
				Construct( TotConstructs ).LayerPoint( nLayer ) = LayerPoint( nLayer );
				if ( LayerPoint( nLayer ) != 0 ) {
					NominalRforNominalUCalculation( TotConstructs ) += NominalR( LayerPoint( nLayer ) );
				}
			}

			// no error if zero -- that will have been caught with earlier construction
			// the following line was changed to fix CR7601
			if ( NominalRforNominalUCalculation( TotConstructs ) != 0.0 ) {
				NominalU( TotConstructs ) = 1.0 / NominalRforNominalUCalculation( TotConstructs );
			}

			CheckAndSetConstructionProperties( TotConstructs, ErrorsFound );

		}

		return NewConstrNum;

	}

	void
	AddVariableSlatBlind(
		int const inBlindNumber, // current Blind Number/pointer to name
		int & outBlindNumber, // resultant Blind Number to pass back
		bool & errFlag // error flag should one be needed
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   September 2009
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Window Blinds are presented as "fixed" slat blinds.  However for certain Window Shading Controls,
		// the program needs to set the property to "variable"/movable slats.  Since a blind could be in use
		// elsewhere with "fixed", a material needs to be added with variable properties -- having most of the
		// "fixed" properties in tact.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::RoundSigDigits;
		using InputProcessor::FindItemInList;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int Found;
		Real64 MinSlatAngGeom;
		Real64 MaxSlatAngGeom;

		// Object Data

		// maybe it's already there
		errFlag = false;
		Found = FindItemInList( "~" + Blind( inBlindNumber ).Name, Blind );
		if ( Found == 0 ) {
			// Add a new blind
			Blind.redimension( ++TotBlinds );
			Blind( TotBlinds ) = Blind( inBlindNumber );
			Blind( TotBlinds ).Name = "~" + Blind( inBlindNumber ).Name;
			outBlindNumber = TotBlinds;
			Blind( TotBlinds ).SlatAngleType = VariableSlats;

			// Minimum and maximum slat angles allowed by slat geometry
			if ( Blind( TotBlinds ).SlatWidth > Blind( TotBlinds ).SlatSeparation ) {
				MinSlatAngGeom = std::asin( Blind( TotBlinds ).SlatThickness / ( Blind( TotBlinds ).SlatThickness + Blind( TotBlinds ).SlatSeparation ) ) / DegToRadians;
			} else {
				MinSlatAngGeom = 0.0;
			}
			MaxSlatAngGeom = 180.0 - MinSlatAngGeom;

			// Error if maximum slat angle less than minimum

			if ( Blind( TotBlinds ).MaxSlatAngle < Blind( TotBlinds ).MinSlatAngle ) {
				errFlag = true;
				ShowSevereError( "WindowMaterial:Blind=\"" + Blind( inBlindNumber ).Name + "\", Illegal value combination." );
				ShowContinueError( "Minimum Slat Angle=[" + RoundSigDigits( Blind( TotBlinds ).MinSlatAngle, 1 ) + "], is greater than Maximum Slat Angle=[" + RoundSigDigits( Blind( TotBlinds ).MaxSlatAngle, 1 ) + "] deg." );
			}

			// Error if input slat angle not in input min/max range

			if ( Blind( TotBlinds ).MaxSlatAngle > Blind( TotBlinds ).MinSlatAngle && ( Blind( TotBlinds ).SlatAngle < Blind( TotBlinds ).MinSlatAngle || Blind( TotBlinds ).SlatAngle > Blind( TotBlinds ).MaxSlatAngle ) ) {
				errFlag = true;
				ShowSevereError( "WindowMaterial:Blind=\"" + Blind( inBlindNumber ).Name + "\", Illegal value combination." );
				ShowContinueError( "Slat Angle=[" + RoundSigDigits( Blind( TotBlinds ).SlatAngle, 1 ) + "] is outside of the input min/max range, min=[" + RoundSigDigits( Blind( TotBlinds ).MinSlatAngle, 1 ) + "], max=[" + RoundSigDigits( Blind( TotBlinds ).MaxSlatAngle, 1 ) + "] deg." );
			}

			// Warning if input minimum slat angle is less than that allowed by slat geometry

			if ( Blind( TotBlinds ).MinSlatAngle < MinSlatAngGeom ) {
				ShowWarningError( "WindowMaterial:Blind=\"" + Blind( inBlindNumber ).Name + "\", Illegal value combination." );
				ShowContinueError( "Minimum Slat Angle=[" + RoundSigDigits( Blind( TotBlinds ).MinSlatAngle, 1 ) + "] is less than the smallest allowed by slat dimensions and spacing, min=[" + RoundSigDigits( MinSlatAngGeom, 1 ) + "] deg." );
				ShowContinueError( "Minimum Slat Angle will be set to " + RoundSigDigits( MinSlatAngGeom, 1 ) + " deg." );
				Blind( TotBlinds ).MinSlatAngle = MinSlatAngGeom;
			}

			// Warning if input maximum slat angle is greater than that allowed by slat geometry

			if ( Blind( TotBlinds ).MaxSlatAngle > MaxSlatAngGeom ) {
				ShowWarningError( "WindowMaterial:Blind=\"" + Blind( inBlindNumber ).Name + "\", Illegal value combination." );
				ShowContinueError( "Maximum Slat Angle=[" + RoundSigDigits( Blind( TotBlinds ).MaxSlatAngle, 1 ) + "] is greater than the largest allowed by slat dimensions and spacing, [" + RoundSigDigits( MaxSlatAngGeom, 1 ) + "] deg." );
				ShowContinueError( "Maximum Slat Angle will be set to " + RoundSigDigits( MaxSlatAngGeom, 1 ) + " deg." );
				Blind( TotBlinds ).MaxSlatAngle = MaxSlatAngGeom;
			}
		} else {
			outBlindNumber = Found;
		}

	}

	void
	CalcScreenTransmittance(
		int const SurfaceNum,
		Optional< Real64 const > Phi, // Optional sun altitude relative to surface outward normal (radians)
		Optional< Real64 const > Theta, // Optional sun azimuth relative to surface outward normal (radians)
		Optional_int_const ScreenNumber // Optional screen number
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Raustad
		//       DATE WRITTEN   May 2006
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		//  Calculate transmittance of window screen given azimuth and altitude angle
		//  of sun and surface orientation.

		// METHODOLOGY EMPLOYED:
		//  Window screen solar beam transmittance varies as the sun moves across the sky
		//  due to the geometry of the screen material and the angle of incidence
		//  of the solar beam. Azimuth and altitude angle are calculated with respect
		//  to the surface outward normal. Solar beam reflectance and absorptance are also
		//  accounted for.

		//  CALLs to CalcScreenTransmittance are primarily based on surface index. A typical call is:
		//  CALL CalcScreenTransmittance(SurfaceNum)
		//  Since a single Material:WindowScreen object may be used for multiple windows, the
		//  screen's direct beam properties are calculated for the screen material attached to this surface.
		//  If a single Material:WindowScreen object is used for 3 windows then SurfaceScreens(3) is allocated.

		//  CALLs to CalcScreenTransmittance may be done by using the optional arguments as follows:
		//  CALLs to CalcScreenTransmittance at normal incidence are:
		//  CALL with a screen number and relative azimuth and altitude angles
		//  CALL CalcScreenTransmittance(0, Phi=0.0, Theta=0.0, ScreenNumber=ScNum)
		//   -OR-
		//  CALL same as above using the material structure
		//  CALL CalcScreenTransmittance(0, Phi=0.0, Theta=0.0, ScreenNumber=Material(MatShade)%ScreenDataPtr)
		//   -OR-
		//  CALL with the surface number and relative azimuth and altitude angles
		//  CALL CalcScreenTransmittance(SurfaceNum, Phi=0.0, Theta=0.0)

		//  CALL's passing the screen number without the relative azimuth and altitude angles is not allowed
		//  CALL CalcScreenTransmittance(0, ScreenNumber=ScNum) ! DO NOT use this syntax

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataGlobals::Pi;
		using DataGlobals::PiOvr2;
		using DataGlobals::DegToRadians;
		using DataSurfaces::Surface;
		using DataSurfaces::SurfaceWindow;
		using DataSurfaces::DoNotModel;
		using DataSurfaces::ModelAsDiffuse;
		using DataSurfaces::ModelAsDirectBeam;
		using DataEnvironment::SOLCOS;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:
		// The optional arguments Phi and Theta are used to integrate over a hemisphere and are passed as pairs
		// The optional argument ScreenNumber is used during CalcWindowScreenProperties to integrate over a quarter hemispere
		// "before" the surface # is known. Theta and Phi can be passed without ScreenNumber, but DO NOT pass ScreenNumber
		// without Theta and Phi.

		// FUNCTION PARAMETER DEFINITIONS:
		Real64 const Small( 1.E-9 ); // Small Number used to approximate zero

		// FUNCTION PARAMETER DEFINITIONS:
		int ScNum; // Index to screen data
		Real64 Tdirect; // Beam solar transmitted through screen (dependent on sun angle)
		Real64 Tscattered; // Beam solar reflected through screen (dependent on sun angle)
		Real64 TscatteredVis; // Visible beam solar reflected through screen (dependent on sun angle)
		Real64 SunAzimuth; // Solar azimuth angle from north (rad)
		Real64 SunAltitude; // Solar altitude angle from horizon (rad)
		Real64 SurfaceAzimuth; // Surface azimuth angle from north (rad)
		Real64 SurfaceTilt; // Surface tilt angle from vertical (rad)
		Real64 SunAzimuthToScreenNormal; // Relative solar azimuth (sun angle from screen normal, 0 to PiOvr2, rad)
		Real64 SunAltitudeToScreenNormal; // Relative solar altitude (sun angle from screen normal, -PiOvr2 to PiOvr2, rad)
		Real64 Beta; // Compliment of relative solar azimuth (rad)
		Real64 TransXDir; // Horizontal component of direct beam transmittance
		Real64 TransYDir; // Vertical component of direct beam transmittance
		Real64 Delta; // Intermediate variable used for Tscatter calculation (deg)
		Real64 DeltaMax; // Intermediate variable used for Tscatter calculation (deg)
		Real64 Tscattermax; // Maximum solar beam  scattered transmittance
		Real64 TscattermaxVis; // Maximum visible beam scattered transmittance
		Real64 ExponentInterior; // Exponent used in scattered transmittance calculation
		// when Delta < DeltaMax (0,0 to peak)
		Real64 ExponentExterior; // Exponent used in scattered transmittance calculation
		// when Delta > DeltaMax (peak to max)
		Real64 AlphaDblPrime; // Intermediate variables (used in Eng. Doc.)
		Real64 COSMu;
		Real64 Epsilon;
		Real64 Eta;
		Real64 MuPrime;
		Real64 Gamma;
		Real64 NormalAltitude; // Actual altitude angle of sun wrt surface outward normal (rad)
		Real64 NormalAzimuth; // Actual azimuth angle of sun wrt surface outward normal (rad)
		Real64 IncidentAngle; // Solar angle wrt surface outward normal to determine
		// if sun is in front of screen (rad)
		Real64 PeakToPlateauRatio; // Ratio of peak scattering to plateau at 0,0 incident angle
		Real64 PeakToPlateauRatioVis; // Ratio of peak visible scattering to plateau at 0,0 incident angle
		Real64 ReflectCyl; // Screen material reflectance
		Real64 ReflectCylVis; // Screen material visible reflectance

		// SurfaceScreens structure may be accessed using either the surface or screen index
		// The screen index is based on the number of Surface:HeatTransfer:Sub objects using any Material:WindowScreen object
		if ( present( ScreenNumber ) ) {
			ScNum = ScreenNumber;
			if ( ! present( Theta ) || ! present( Phi ) ) {
				ShowFatalError( "Syntax error, optional arguments Theta and Phi must be present when optional ScreenNumber is used." );
			}
		} else {
			ScNum = SurfaceWindow( SurfaceNum ).ScreenNumber;
		}

		if ( present( Theta ) ) {
			SunAzimuthToScreenNormal = std::abs( Theta );
			if ( SunAzimuthToScreenNormal > Pi ) {
				SunAzimuthToScreenNormal = 0.0;
			} else {
				if ( SunAzimuthToScreenNormal > PiOvr2 ) {
					SunAzimuthToScreenNormal = Pi - SunAzimuthToScreenNormal;
				}
			}
			NormalAzimuth = SunAzimuthToScreenNormal;
		} else {
			SunAzimuth = std::atan2( SOLCOS( 1 ), SOLCOS( 2 ) );
			if ( SunAzimuth < 0.0 ) SunAzimuth += 2.0 * Pi;
			SurfaceAzimuth = Surface( SurfaceNum ).Azimuth * DegToRadians;
			NormalAzimuth = SunAzimuth - SurfaceAzimuth;
			//   Calculate the transmittance whether sun is in front of or behind screen, place result in BmBmTrans or BmBmTransBack
			if ( std::abs( SunAzimuth - SurfaceAzimuth ) > PiOvr2 ) {
				SunAzimuthToScreenNormal = std::abs( SunAzimuth - SurfaceAzimuth ) - PiOvr2;
			} else {
				SunAzimuthToScreenNormal = std::abs( SunAzimuth - SurfaceAzimuth );
			}
		}

		if ( present( Phi ) ) {
			SunAltitudeToScreenNormal = std::abs( Phi );
			if ( SunAltitudeToScreenNormal > PiOvr2 ) {
				SunAltitudeToScreenNormal = Pi - SunAltitudeToScreenNormal;
			}
			SunAltitude = SunAltitudeToScreenNormal;
		} else {
			SunAltitude = ( PiOvr2 - std::acos( SOLCOS( 3 ) ) );
			SurfaceTilt = Surface( SurfaceNum ).Tilt * DegToRadians;
			SunAltitudeToScreenNormal = std::abs( SunAltitude + ( SurfaceTilt - PiOvr2 ) );
			if ( SunAltitudeToScreenNormal > PiOvr2 ) {
				SunAltitudeToScreenNormal -= PiOvr2;
			}
		}

		if ( SurfaceNum == 0 || ! present( ScreenNumber ) ) {
			NormalAltitude = SunAltitude;
		} else {
			NormalAltitude = SunAltitude + ( SurfaceTilt - PiOvr2 );
		}

		if ( NormalAltitude != 0.0 && NormalAzimuth != 0.0 ) {
			IncidentAngle = std::acos( std::sin( NormalAltitude ) / ( std::tan( NormalAzimuth ) * std::tan( NormalAltitude ) / std::sin( NormalAzimuth ) ) );
		} else if ( NormalAltitude != 0.0 && NormalAzimuth == 0.0 ) {
			IncidentAngle = NormalAltitude;
		} else if ( NormalAltitude == 0.0 && NormalAzimuth != 0.0 ) {
			IncidentAngle = NormalAzimuth;
		} else {
			IncidentAngle = 0.0;
		}

		// ratio of screen material diameter to screen material spacing
		Gamma = SurfaceScreens( ScNum ).ScreenDiameterToSpacingRatio;

		// ************************************************************************************************
		// * calculate transmittance of totally absorbing screen material (beam passing through open area)*
		// ************************************************************************************************

		// calculate compliment of relative solar azimuth
		Beta = PiOvr2 - SunAzimuthToScreenNormal;

		// Catch all divide by zero instances
		if ( Beta > Small ) {
			if ( std::abs( SunAltitudeToScreenNormal - PiOvr2 ) > Small ) {
				AlphaDblPrime = std::atan( std::tan( SunAltitudeToScreenNormal ) / std::cos( SunAzimuthToScreenNormal ) );
				TransYDir = 1.0 - Gamma * ( std::cos( AlphaDblPrime ) + std::sin( AlphaDblPrime ) * std::tan( SunAltitudeToScreenNormal ) * std::sqrt( 1.0 + pow_2( 1.0 / std::tan( Beta ) ) ) );
				TransYDir = max( 0.0, TransYDir );
			} else {
				TransYDir = 0.0;
			}
		} else {
			TransYDir = 0.0;
		}

		COSMu = std::sqrt( pow_2( std::cos( SunAltitudeToScreenNormal ) ) * pow_2( std::cos( SunAzimuthToScreenNormal ) ) + pow_2( std::sin( SunAltitudeToScreenNormal ) ) );
		if ( COSMu > Small ) {
			Epsilon = std::acos( std::cos( SunAltitudeToScreenNormal ) * std::cos( SunAzimuthToScreenNormal ) / COSMu );
			Eta = PiOvr2 - Epsilon;
			if ( std::cos( Epsilon ) != 0.0 ) {
				MuPrime = std::atan( std::tan( std::acos( COSMu ) ) / std::cos( Epsilon ) );
				if ( Eta != 0.0 ) {
					TransXDir = 1.0 - Gamma * ( std::cos( MuPrime ) + std::sin( MuPrime ) * std::tan( std::acos( COSMu ) ) * std::sqrt( 1.0 + pow_2( 1.0 / std::tan( Eta ) ) ) );
					TransXDir = max( 0.0, TransXDir );
				} else {
					TransXDir = 0.0;
				}
			} else {
				TransXDir = 0.0;
			}
		} else {
			TransXDir = 1.0 - Gamma;
		}
		Tdirect = max( 0.0, TransXDir * TransYDir );

		// *******************************************************************************
		// * calculate transmittance of scattered beam due to reflecting screen material *
		// *******************************************************************************

		ReflectCyl = SurfaceScreens( ScNum ).ReflectCylinder;
		ReflectCylVis = SurfaceScreens( ScNum ).ReflectCylinderVis;

		if ( std::abs( SunAzimuthToScreenNormal - PiOvr2 ) < Small || std::abs( SunAltitudeToScreenNormal - PiOvr2 ) < Small ) {
			Tscattered = 0.0;
			TscatteredVis = 0.0;
		} else {
			//   DeltaMax and Delta are in degrees
			DeltaMax = 89.7 - ( 10.0 * Gamma / 0.16 );
			Delta = std::sqrt( pow_2( SunAzimuthToScreenNormal / DegToRadians ) + pow_2( SunAltitudeToScreenNormal / DegToRadians ) );

			//   Use empirical model to determine maximum (peak) scattering
			Tscattermax = 0.0229 * Gamma + 0.2971 * ReflectCyl - 0.03624 * pow_2( Gamma ) + 0.04763 * pow_2( ReflectCyl ) - 0.44416 * Gamma * ReflectCyl;
			TscattermaxVis = 0.0229 * Gamma + 0.2971 * ReflectCylVis - 0.03624 * pow_2( Gamma ) + 0.04763 * pow_2( ReflectCylVis ) - 0.44416 * Gamma * ReflectCylVis;

			//   Vary slope of interior and exterior surface of scattering model
			ExponentInterior = -pow_2( Delta - DeltaMax ) / 600.0;
			ExponentExterior = -std::pow( std::abs( Delta - DeltaMax ), 2.5 ) / 600.0;

			//   Determine ratio of scattering at 0,0 incident angle to maximum (peak) scattering
			PeakToPlateauRatio = 1.0 / ( 0.2 * ( 1 - Gamma ) * ReflectCyl );
			PeakToPlateauRatioVis = 1.0 / ( 0.2 * ( 1 - Gamma ) * ReflectCylVis );

			if ( Delta > DeltaMax ) {
				//     Apply offset for plateau and use exterior exponential function to simulate actual scattering as a function of solar angles
				Tscattered = 0.2 * ( 1.0 - Gamma ) * ReflectCyl * Tscattermax * ( 1.0 + ( PeakToPlateauRatio - 1.0 ) * std::exp( ExponentExterior ) );
				TscatteredVis = 0.2 * ( 1.0 - Gamma ) * ReflectCylVis * TscattermaxVis * ( 1.0 + ( PeakToPlateauRatioVis - 1.0 ) * std::exp( ExponentExterior ) );
				//     Trim off offset if solar angle (delta) is greater than maximum (peak) scattering angle
				Tscattered -= ( 0.2 * ( 1.0 - Gamma ) * ReflectCyl * Tscattermax ) * max( 0.0, ( Delta - DeltaMax ) / ( 90.0 - DeltaMax ) );
				TscatteredVis -= ( 0.2 * ( 1.0 - Gamma ) * ReflectCylVis * TscattermaxVis ) * max( 0.0, ( Delta - DeltaMax ) / ( 90.0 - DeltaMax ) );
			} else {
				//     Apply offset for plateau and use interior exponential function to simulate actual scattering as a function of solar angles
				Tscattered = 0.2 * ( 1.0 - Gamma ) * ReflectCyl * Tscattermax * ( 1.0 + ( PeakToPlateauRatio - 1.0 ) * std::exp( ExponentInterior ) );
				TscatteredVis = 0.2 * ( 1.0 - Gamma ) * ReflectCylVis * TscattermaxVis * ( 1.0 + ( PeakToPlateauRatioVis - 1.0 ) * std::exp( ExponentInterior ) );
			}
		}
		Tscattered = max( 0.0, Tscattered );
		TscatteredVis = max( 0.0, TscatteredVis );

		if ( SurfaceScreens( ScNum ).ScreenBeamReflectanceAccounting == DoNotModel ) {
			if ( std::abs( IncidentAngle ) <= PiOvr2 ) {
				SurfaceScreens( ScNum ).BmBmTrans = Tdirect;
				SurfaceScreens( ScNum ).BmBmTransVis = Tdirect;
				SurfaceScreens( ScNum ).BmBmTransBack = 0.0;
			} else {
				SurfaceScreens( ScNum ).BmBmTrans = 0.0;
				SurfaceScreens( ScNum ).BmBmTransVis = 0.0;
				SurfaceScreens( ScNum ).BmBmTransBack = Tdirect;
			}
			Tscattered = 0.0;
			TscatteredVis = 0.0;
		} else if ( SurfaceScreens( ScNum ).ScreenBeamReflectanceAccounting == ModelAsDirectBeam ) {
			if ( std::abs( IncidentAngle ) <= PiOvr2 ) {
				SurfaceScreens( ScNum ).BmBmTrans = Tdirect + Tscattered;
				SurfaceScreens( ScNum ).BmBmTransVis = Tdirect + TscatteredVis;
				SurfaceScreens( ScNum ).BmBmTransBack = 0.0;
			} else {
				SurfaceScreens( ScNum ).BmBmTrans = 0.0;
				SurfaceScreens( ScNum ).BmBmTransVis = 0.0;
				SurfaceScreens( ScNum ).BmBmTransBack = Tdirect + Tscattered;
			}
			Tscattered = 0.0;
			TscatteredVis = 0.0;
		} else if ( SurfaceScreens( ScNum ).ScreenBeamReflectanceAccounting == ModelAsDiffuse ) {
			if ( std::abs( IncidentAngle ) <= PiOvr2 ) {
				SurfaceScreens( ScNum ).BmBmTrans = Tdirect;
				SurfaceScreens( ScNum ).BmBmTransVis = Tdirect;
				SurfaceScreens( ScNum ).BmBmTransBack = 0.0;
			} else {
				SurfaceScreens( ScNum ).BmBmTrans = 0.0;
				SurfaceScreens( ScNum ).BmBmTransVis = 0.0;
				SurfaceScreens( ScNum ).BmBmTransBack = Tdirect;
			}
		}

		if ( std::abs( IncidentAngle ) <= PiOvr2 ) {
			SurfaceScreens( ScNum ).BmDifTrans = Tscattered;
			SurfaceScreens( ScNum ).BmDifTransVis = TscatteredVis;
			SurfaceScreens( ScNum ).BmDifTransBack = 0.0;
			SurfaceScreens( ScNum ).ReflectSolBeamFront = max( 0.0, ReflectCyl * ( 1.0 - Tdirect ) - Tscattered );
			SurfaceScreens( ScNum ).ReflectVisBeamFront = max( 0.0, ReflectCylVis * ( 1.0 - Tdirect ) - TscatteredVis );
			SurfaceScreens( ScNum ).AbsorpSolarBeamFront = max( 0.0, ( 1.0 - Tdirect ) * ( 1.0 - ReflectCyl ) );
			SurfaceScreens( ScNum ).ReflectSolBeamBack = 0.0;
			SurfaceScreens( ScNum ).ReflectVisBeamBack = 0.0;
			SurfaceScreens( ScNum ).AbsorpSolarBeamBack = 0.0;
		} else {
			SurfaceScreens( ScNum ).BmDifTrans = 0.0;
			SurfaceScreens( ScNum ).BmDifTransVis = 0.0;
			SurfaceScreens( ScNum ).BmDifTransBack = Tscattered;
			SurfaceScreens( ScNum ).ReflectSolBeamBack = max( 0.0, ReflectCyl * ( 1.0 - Tdirect ) - Tscattered );
			SurfaceScreens( ScNum ).ReflectVisBeamBack = max( 0.0, ReflectCylVis * ( 1.0 - Tdirect ) - TscatteredVis );
			SurfaceScreens( ScNum ).AbsorpSolarBeamBack = max( 0.0, ( 1.0 - Tdirect ) * ( 1.0 - ReflectCyl ) );
			SurfaceScreens( ScNum ).ReflectSolBeamFront = 0.0;
			SurfaceScreens( ScNum ).ReflectVisBeamFront = 0.0;
			SurfaceScreens( ScNum ).AbsorpSolarBeamFront = 0.0;
		}

	}

	std::string
	DisplayMaterialRoughness( int const Roughness ) // Roughness String
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda K. Lawrie
		//       DATE WRITTEN   October 2005
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is given a roughness value and returns the character representation.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		std::string cRoughness; // Character representation of Roughness

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		//Select the correct Number for the associated ascii name for the roughness type
		{ auto const SELECT_CASE_var( Roughness );
		if ( SELECT_CASE_var == VeryRough ) {
			cRoughness = "VeryRough";
		} else if ( SELECT_CASE_var == Rough ) {
			cRoughness = "Rough";
		} else if ( SELECT_CASE_var == MediumRough ) {
			cRoughness = "MediumRough";
		} else if ( SELECT_CASE_var == MediumSmooth ) {
			cRoughness = "MediumSmooth";
		} else if ( SELECT_CASE_var == Smooth ) {
			cRoughness = "Smooth";
		} else if ( SELECT_CASE_var == VerySmooth ) {
			cRoughness = "VerySmooth";
		} else {
			cRoughness = "";
		}}

		return cRoughness;

	}

	Real64
	ComputeNominalUwithConvCoeffs(
		int const numSurf, // index for Surface array.
		bool & isValid // returns true if result is valid
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Jason Glazer
		//       DATE WRITTEN   September 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Calculate Nominal U-value with convection/film coefficients for reporting by
		// adding on prescribed R-values for interior and exterior convection coefficients
		// as found in ASHRAE 90.1-2004, Appendix A. Used in EIO and tabular reports.
		// ASHRAE 90.1-2004 Section A9.4.1 shows the following:
		//      R-value Condition
		//      All exterior conditions                        IP: 0.17  SI: 0.0299
		//      All semi-exterior surfaces                     IP: 0.46  SI: 0.0810
		//      Interior horizontal surfaces, heat flow up     IP: 0.61  SI: 0.1074
		//      Interior horizontal surfaces, heat flow down   IP: 0.92  SI: 0.1620
		//      Interior vertical surfaces                     IP: 0.68  SI: 0.1198
		// This section shows the same value in 90.1-2007 and 90.2-2010

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataSurfaces::Surface;
		using DataSurfaces::SurfaceClass_Wall;
		using DataSurfaces::SurfaceClass_Floor;
		using DataSurfaces::SurfaceClass_Roof;
		using DataSurfaces::ExternalEnvironment;
		using DataSurfaces::Ground;
		using DataSurfaces::GroundFCfactorMethod;
		using DataSurfaces::SurfaceClass_Door;

		// Return value
		Real64 NominalUwithConvCoeffs; // return value

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		Real64 insideFilm;
		Real64 outsideFilm;

		isValid = true;
		// exterior conditions
		{ auto const SELECT_CASE_var( Surface( numSurf ).ExtBoundCond );
		if ( SELECT_CASE_var == ExternalEnvironment ) {
			outsideFilm = 0.0299387; // All exterior conditions
		} else if ( ( SELECT_CASE_var == Ground ) || ( SELECT_CASE_var == GroundFCfactorMethod ) ) {
			outsideFilm = 0.0; // No outside film when underground
		} else {
			if ( Surface( numSurf ).ExtBoundCond > 0 ) { //interzone partition
				//use companion surface in adjacent zone
				{ auto const SELECT_CASE_var1( Surface( Surface( numSurf ).ExtBoundCond ).Class );
				if ( ( SELECT_CASE_var1 == SurfaceClass_Wall ) || ( SELECT_CASE_var1 == SurfaceClass_Door ) ) { // Interior:  vertical, still air, Rcin = 0.68 ft2-F-hr/BTU
					outsideFilm = 0.1197548;
				} else if ( SELECT_CASE_var1 == SurfaceClass_Floor ) { // Interior:  horizontal, still air, heat flow downward, Rcin = 0.92 ft2-F-hr/BTU
					outsideFilm = 0.1620212;
				} else if ( SELECT_CASE_var1 == SurfaceClass_Roof ) { // Interior:  horizontal, still air, heat flow upward, Rcin = 0.61 ft2-F-hr/BTU
					outsideFilm = 0.1074271;
				} else {
					outsideFilm = 0.0810106; // All semi-exterior surfaces
				}}
			} else {
				outsideFilm = 0.0810106; // All semi-exterior surfaces
			}
		}}
		// interior conditions
		if ( NominalU( Surface( numSurf ).Construction ) > 0.0 ) {
			{ auto const SELECT_CASE_var( Surface( numSurf ).Class );
			if ( ( SELECT_CASE_var == SurfaceClass_Wall ) || ( SELECT_CASE_var == SurfaceClass_Door ) ) { // Interior:  vertical, still air, Rcin = 0.68 ft2-F-hr/BTU
				insideFilm = 0.1197548;
			} else if ( SELECT_CASE_var == SurfaceClass_Floor ) { // Interior:  horizontal, still air, heat flow downward, Rcin = 0.92 ft2-F-hr/BTU
				insideFilm = 0.1620212;
			} else if ( SELECT_CASE_var == SurfaceClass_Roof ) { // Interior:  horizontal, still air, heat flow upward, Rcin = 0.61 ft2-F-hr/BTU
				insideFilm = 0.1074271;
			} else {
				insideFilm = 0.0;
				outsideFilm = 0.0;
			}}
			NominalUwithConvCoeffs = 1.0 / ( insideFilm + ( 1.0 / NominalU( Surface( numSurf ).Construction ) ) + outsideFilm );
		} else {
			isValid = false;
			NominalUwithConvCoeffs = NominalU( Surface( numSurf ).Construction );
		}

		return NominalUwithConvCoeffs;
	}

} // DataHeatBalance

} // EnergyPlus
