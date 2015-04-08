#ifndef SolarCollectors_hh_INCLUDED
#define SolarCollectors_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace SolarCollectors {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// Fluid Type Flags
	extern int const WATER;
	extern int const AIR;

	// Test Correlation Type Flags
	extern int const INLET;
	extern int const AVERAGE;
	extern int const OUTLET;

	// ICS Collector Type Flag
	extern int const ICSRectangularTank;
	//INTEGER, PARAMETER :: ICSProgressiveTube = 2

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE TYPE DECLARATIONS:

	extern Array1D_bool CheckEquipName;

	// MODULE VARIABLE DECLARATIONS:
	extern int NumOfParameters;
	extern int NumOfCollectors;

	extern Array1D< Real64 > TransSysSkyDiff; // transmittance of cover system for sky diffuse solar rad.
	extern Array1D< Real64 > TransSysGrnDiff; // transmittance of cover system for ground diffuse solar rad.
	extern Array1D< Real64 > RefSysSkyDiff; // reflectance of cover system for sky diffuse solar rad.
	extern Array1D< Real64 > RefSysGrnDiff; // reflectance of cover system for ground diffuse solar rad.

	// SUBROUTINE SPECIFICATIONS:

	// Types

	struct ParametersData
	{
		// Members
		std::string Name; // Name of solar collector parameters
		Real64 Area; // Gross area of collector (m2)
		int TestFluid; // Test fluid (only WATER for now)
		Real64 TestMassFlowRate; // Test volumetric flow rate (m3/s)
		int TestType; // Test correlation type (INLET | AVERAGE | OUTLET)
		Real64 eff0; // Coefficient 1 of efficiency equation (Y-intercept)
		Real64 eff1; // Coefficient 2 of efficiency equation (1st order)
		Real64 eff2; // Coefficient 3 of efficiency equation (2nd order)
		Real64 iam1; // Coefficient 2 of incident angle modifier (1st order)
		Real64 iam2; // Coefficient 3 of incident angle modifier (2nd order)
		int ICSType_Num; // ICS collector type
		Real64 Volume; // collector water net volume (m3)
		Real64 SideHeight; // collector side height (m)
		Real64 ThermalMass; // thermal mass of the absorber plate (J/m2C)
		Real64 ULossSide; // heat loss conductance for collector side (W/m2C)
		Real64 ULossBottom; // heat loss conductance for collector bottom (W/m2C)
		Real64 AspectRatio; // collector aspect ratio (dimensionless)
		int NumOfCovers; // number of transparent collector covers
		Real64 CoverSpacing; // collector cover spacings (m)
		Array1D< Real64 > RefractiveIndex; // refractive idex of inner and outer covers (dimensionless)
		Array1D< Real64 > ExtCoefTimesThickness; // extinction coefficient times thickness of covers (dimensionless)
		Array1D< Real64 > EmissOfCover; // emissivity of inner and outer covers (dimensionless)
		Real64 EmissOfAbsPlate; // emissivity Of absorber plate (dimensionless)
		Real64 AbsorOfAbsPlate; // absorptance of the absorber plate (dimensionless)

		// Default Constructor
		ParametersData() :
			Area( 0.0 ),
			TestFluid( WATER ),
			TestMassFlowRate( 0.0 ),
			TestType( INLET ),
			ICSType_Num( 0 ),
			Volume( 0.0 ),
			SideHeight( 0.0 ),
			ThermalMass( 0.0 ),
			ULossSide( 0.0 ),
			ULossBottom( 0.0 ),
			AspectRatio( 0.0 ),
			NumOfCovers( 0 ),
			CoverSpacing( 0.0 ),
			RefractiveIndex( 2, 0.0 ),
			ExtCoefTimesThickness( 2, 0.0 ),
			EmissOfCover( 2, 0.0 ),
			EmissOfAbsPlate( 0.0 ),
			AbsorOfAbsPlate( 0.0 )
		{}

		// Member Constructor
		ParametersData(
			std::string const & Name, // Name of solar collector parameters
			Real64 const Area, // Gross area of collector (m2)
			int const TestFluid, // Test fluid (only WATER for now)
			Real64 const TestMassFlowRate, // Test volumetric flow rate (m3/s)
			int const TestType, // Test correlation type (INLET | AVERAGE | OUTLET)
			Real64 const eff0, // Coefficient 1 of efficiency equation (Y-intercept)
			Real64 const eff1, // Coefficient 2 of efficiency equation (1st order)
			Real64 const eff2, // Coefficient 3 of efficiency equation (2nd order)
			Real64 const iam1, // Coefficient 2 of incident angle modifier (1st order)
			Real64 const iam2, // Coefficient 3 of incident angle modifier (2nd order)
			int const ICSType_Num, // ICS collector type
			Real64 const Volume, // collector water net volume (m3)
			Real64 const SideHeight, // collector side height (m)
			Real64 const ThermalMass, // thermal mass of the absorber plate (J/m2C)
			Real64 const ULossSide, // heat loss conductance for collector side (W/m2C)
			Real64 const ULossBottom, // heat loss conductance for collector bottom (W/m2C)
			Real64 const AspectRatio, // collector aspect ratio (dimensionless)
			int const NumOfCovers, // number of transparent collector covers
			Real64 const CoverSpacing, // collector cover spacings (m)
			Array1< Real64 > const & RefractiveIndex, // refractive idex of inner and outer covers (dimensionless)
			Array1< Real64 > const & ExtCoefTimesThickness, // extinction coefficient times thickness of covers (dimensionless)
			Array1< Real64 > const & EmissOfCover, // emissivity of inner and outer covers (dimensionless)
			Real64 const EmissOfAbsPlate, // emissivity Of absorber plate (dimensionless)
			Real64 const AbsorOfAbsPlate // absorptance of the absorber plate (dimensionless)
		) :
			Name( Name ),
			Area( Area ),
			TestFluid( TestFluid ),
			TestMassFlowRate( TestMassFlowRate ),
			TestType( TestType ),
			eff0( eff0 ),
			eff1( eff1 ),
			eff2( eff2 ),
			iam1( iam1 ),
			iam2( iam2 ),
			ICSType_Num( ICSType_Num ),
			Volume( Volume ),
			SideHeight( SideHeight ),
			ThermalMass( ThermalMass ),
			ULossSide( ULossSide ),
			ULossBottom( ULossBottom ),
			AspectRatio( AspectRatio ),
			NumOfCovers( NumOfCovers ),
			CoverSpacing( CoverSpacing ),
			RefractiveIndex( 2, RefractiveIndex ),
			ExtCoefTimesThickness( 2, ExtCoefTimesThickness ),
			EmissOfCover( 2, EmissOfCover ),
			EmissOfAbsPlate( EmissOfAbsPlate ),
			AbsorOfAbsPlate( AbsorOfAbsPlate )
		{}

	};

	struct CollectorData
	{
		// Members
		std::string Name; // Name of solar collector
		std::string BCType; // Boundary condition Type
		std::string BCName; // Boundary condition Name
		std::string OSCMName; // OtherSideConditionsModel
		int VentCavIndex; // index of ventilated cavity object
		int ICSType_Num; // ICS collector type number
		int TypeNum; // Plant Side Connection: 'TypeOf_Num' assigned in DataPlant !DSU
		int WLoopNum; // Water plant loop index number                      !DSU
		int WLoopSideNum; // Water plant loop side index                        !DSU
		int WLoopBranchNum; // Water plant loop branch index                      !DSU
		int WLoopCompNum; // Water plant loop component index                   !DSU
		bool Init; // Flag for initialization:  TRUE means do the init
		bool InitSizing; // Flag for initialization of plant sizing
		int Parameters; // Parameters object number
		int Surface; // Surface object number
		int InletNode; // Inlet node
		Real64 InletTemp; // Inlet temperature from plant (C)
		int OutletNode; // Outlet node
		Real64 OutletTemp; // Outlet temperature or stagnation temperature in the collector (C)
		Real64 MassFlowRate; // Mass flow rate through the collector (kg/s)
		Real64 MassFlowRateMax; // Maximum mass flow rate through the collector (kg/s)
		Real64 VolFlowRateMax; // Maximum volumetric flow rate through the collector (m3/s)
		int ErrIndex; // Error index for recurring error
		int IterErrIndex; // Error index for recurring error (iteration - did not converge)
		// Report variables
		Real64 IncidentAngleModifier; // Net incident angle modifier
		Real64 Efficiency; // Thermal efficiency of solar energy conversion
		Real64 Power; // Heat gain or loss to collector fluid (W)
		Real64 HeatGain; // Heat gain to collector fluid (W)
		Real64 HeatLoss; // Heat loss from collector fluid (W)
		Real64 Energy; // Energy gained (or lost) to collector fluid (J)
		// Report variables
		Real64 HeatRate; // Collector useful Heat gain rate [W]
		Real64 HeatEnergy; // Collector useful Heat gain energy [J]
		Real64 StoredHeatRate; // net heat gain or loss rate of the collector fluid [W]
		Real64 StoredHeatEnergy; // net heat gain or loss energy of the collector fluid [J]
		Real64 HeatGainRate; // Collector useful Heat gain rate [W]
		Real64 HeatGainEnergy; // Collector useful Heat gain energy (J)
		Real64 HeatLossRate; // collector useful heat loss rate [W]
		Real64 HeatLossEnergy; // Collector useful Heat loss energy [J]
		Real64 SkinHeatLossRate; // collector skin heat loss rate [W]
		Real64 CollHeatLossEnergy; // collector skin heat loss energy[J]
		Real64 TauAlpha; // Transmittance-absorptance product total radiation
		Real64 UTopLoss; // Over all top loss coefficient [W/m2.C]
		Real64 TempOfWater; // average temperature of the collector water [C]
		Real64 TempOfAbsPlate; // average temperature of the abs plate [C]
		Real64 TempOfInnerCover; // temperature of the collector inner cover [C]
		Real64 TempOfOuterCover; // temperature of the collector inner cover [C]
		// Data from elsewhere and calculated
		Real64 TauAlphaNormal; // Transmittance-absorptance product normal radiation
		Real64 TauAlphaSkyDiffuse; // Transmittance-absorptance product sky diffuse radiation
		Real64 TauAlphaGndDiffuse; // Transmittance-absorptance product grn diffuse radiation
		Real64 TauAlphaBeam; // Transmittance-absorptance product beam radiation
		Array1D< Real64 > CoversAbsSkyDiffuse; // sky diffuse solar absorptance of cover
		Array1D< Real64 > CoversAbsGndDiffuse; // ground diffuse solar absorptance of cover
		Array1D< Real64 > CoverAbs; // solar rad weighted covers absorptance
		Real64 TimeElapsed; // Fraction of the current hour that has elapsed (h)
		// Saved in order to identify the beginning of a new system time
		Real64 UbLoss; // Over all bottom loss coefficient [W/m2C]
		Real64 UsLoss; // Over all side loss coefficient [W/m2C]
		Real64 AreaRatio; // Side area to collector area ratio [-]
		Real64 RefSkyDiffInnerCover; // Sky diffuse refl of inner cover (cover 1)
		Real64 RefGrnDiffInnerCover; // ground diffuse refl of inner cover (cover 1)
		Real64 RefDiffInnerCover; // diffuse reflectance of the inner cover (cover 1) from bottom
		Real64 SavedTempOfWater; // water temp carried from time step to time step [C]
		Real64 SavedTempOfAbsPlate; // abs plate temp carried from time step to time step [C]
		Real64 SavedTempOfInnerCover; // inner cover temp carried from time step to time step [C]
		Real64 SavedTempOfOuterCover; // outer cover temp carried from time step to time step [C]
		Real64 SavedTempCollectorOSCM; // Temperature of collector back from OSCM at previous time step [C]
		Real64 Length; // characteristic length of the abs plate
		Real64 TiltR2V; // collector tilt angle from the vertical [degree]
		Real64 Tilt; // collector tilt angle from the horizontal [degree]
		Real64 CosTilt; // cosine of colector tilt angle [-]
		Real64 SinTilt; // sine of 1.8 times colector tilt angle [-]
		Real64 SideArea; // weighted collector side area (m2)
		Real64 Area; // collector area (m2)
		Real64 Volume; // collector net volume (m3)
		bool OSCM_ON; // Boundary condition is OSCM
		bool InitICS; // used to initialize ICS variables only

		// Default Constructor
		CollectorData() :
			VentCavIndex( 0 ),
			ICSType_Num( 0 ),
			WLoopNum( 0 ),
			WLoopSideNum( 0 ),
			WLoopBranchNum( 0 ),
			WLoopCompNum( 0 ),
			Init( true ),
			InitSizing( true ),
			Parameters( 0 ),
			Surface( 0 ),
			InletNode( 0 ),
			InletTemp( 0.0 ),
			OutletNode( 0 ),
			OutletTemp( 0.0 ),
			MassFlowRate( 0.0 ),
			MassFlowRateMax( 0.0 ),
			VolFlowRateMax( 0.0 ),
			ErrIndex( 0 ),
			IterErrIndex( 0 ),
			IncidentAngleModifier( 0.0 ),
			Efficiency( 0.0 ),
			Power( 0.0 ),
			HeatGain( 0.0 ),
			HeatLoss( 0.0 ),
			Energy( 0.0 ),
			HeatRate( 0.0 ),
			HeatEnergy( 0.0 ),
			StoredHeatRate( 0.0 ),
			StoredHeatEnergy( 0.0 ),
			HeatGainRate( 0.0 ),
			HeatGainEnergy( 0.0 ),
			HeatLossRate( 0.0 ),
			HeatLossEnergy( 0.0 ),
			SkinHeatLossRate( 0.0 ),
			CollHeatLossEnergy( 0.0 ),
			TauAlpha( 0.0 ),
			UTopLoss( 0.0 ),
			TempOfWater( 0.0 ),
			TempOfAbsPlate( 0.0 ),
			TempOfInnerCover( 0.0 ),
			TempOfOuterCover( 0.0 ),
			TauAlphaNormal( 0.0 ),
			TauAlphaSkyDiffuse( 0.0 ),
			TauAlphaGndDiffuse( 0.0 ),
			TauAlphaBeam( 0.0 ),
			CoversAbsSkyDiffuse( 2, 0.0 ),
			CoversAbsGndDiffuse( 2, 0.0 ),
			CoverAbs( 2, 0.0 ),
			TimeElapsed( 0.0 ),
			UbLoss( 0.0 ),
			UsLoss( 0.0 ),
			AreaRatio( 0.0 ),
			RefSkyDiffInnerCover( 0.0 ),
			RefGrnDiffInnerCover( 0.0 ),
			RefDiffInnerCover( 0.0 ),
			SavedTempOfWater( 0.0 ),
			SavedTempOfAbsPlate( 0.0 ),
			SavedTempOfInnerCover( 0.0 ),
			SavedTempOfOuterCover( 0.0 ),
			SavedTempCollectorOSCM( 0.0 ),
			Length( 1.0 ),
			TiltR2V( 0.0 ),
			Tilt( 0.0 ),
			CosTilt( 0.0 ),
			SinTilt( 0.0 ),
			SideArea( 0.0 ),
			Area( 0.0 ),
			Volume( 0.0 ),
			OSCM_ON( false ),
			InitICS( false )
		{}

		// Member Constructor
		CollectorData(
			std::string const & Name, // Name of solar collector
			std::string const & BCType, // Boundary condition Type
			std::string const & BCName, // Boundary condition Name
			std::string const & OSCMName, // OtherSideConditionsModel
			int const VentCavIndex, // index of ventilated cavity object
			int const ICSType_Num, // ICS collector type number
			int const TypeNum, // Plant Side Connection: 'TypeOf_Num' assigned in DataPlant !DSU
			int const WLoopNum, // Water plant loop index number                      !DSU
			int const WLoopSideNum, // Water plant loop side index                        !DSU
			int const WLoopBranchNum, // Water plant loop branch index                      !DSU
			int const WLoopCompNum, // Water plant loop component index                   !DSU
			bool const Init, // Flag for initialization:  TRUE means do the init
			bool const InitSizing, // Flag for initialization of plant sizing
			int const Parameters, // Parameters object number
			int const Surface, // Surface object number
			int const InletNode, // Inlet node
			Real64 const InletTemp, // Inlet temperature from plant (C)
			int const OutletNode, // Outlet node
			Real64 const OutletTemp, // Outlet temperature or stagnation temperature in the collector (C)
			Real64 const MassFlowRate, // Mass flow rate through the collector (kg/s)
			Real64 const MassFlowRateMax, // Maximum mass flow rate through the collector (kg/s)
			Real64 const VolFlowRateMax, // Maximum volumetric flow rate through the collector (m3/s)
			int const ErrIndex, // Error index for recurring error
			int const IterErrIndex, // Error index for recurring error (iteration - did not converge)
			Real64 const IncidentAngleModifier, // Net incident angle modifier
			Real64 const Efficiency, // Thermal efficiency of solar energy conversion
			Real64 const Power, // Heat gain or loss to collector fluid (W)
			Real64 const HeatGain, // Heat gain to collector fluid (W)
			Real64 const HeatLoss, // Heat loss from collector fluid (W)
			Real64 const Energy, // Energy gained (or lost) to collector fluid (J)
			Real64 const HeatRate, // Collector useful Heat gain rate [W]
			Real64 const HeatEnergy, // Collector useful Heat gain energy [J]
			Real64 const StoredHeatRate, // net heat gain or loss rate of the collector fluid [W]
			Real64 const StoredHeatEnergy, // net heat gain or loss energy of the collector fluid [J]
			Real64 const HeatGainRate, // Collector useful Heat gain rate [W]
			Real64 const HeatGainEnergy, // Collector useful Heat gain energy (J)
			Real64 const HeatLossRate, // collector useful heat loss rate [W]
			Real64 const HeatLossEnergy, // Collector useful Heat loss energy [J]
			Real64 const SkinHeatLossRate, // collector skin heat loss rate [W]
			Real64 const CollHeatLossEnergy, // collector skin heat loss energy[J]
			Real64 const TauAlpha, // Transmittance-absorptance product total radiation
			Real64 const UTopLoss, // Over all top loss coefficient [W/m2.C]
			Real64 const TempOfWater, // average temperature of the collector water [C]
			Real64 const TempOfAbsPlate, // average temperature of the abs plate [C]
			Real64 const TempOfInnerCover, // temperature of the collector inner cover [C]
			Real64 const TempOfOuterCover, // temperature of the collector inner cover [C]
			Real64 const TauAlphaNormal, // Transmittance-absorptance product normal radiation
			Real64 const TauAlphaSkyDiffuse, // Transmittance-absorptance product sky diffuse radiation
			Real64 const TauAlphaGndDiffuse, // Transmittance-absorptance product grn diffuse radiation
			Real64 const TauAlphaBeam, // Transmittance-absorptance product beam radiation
			Array1< Real64 > const & CoversAbsSkyDiffuse, // sky diffuse solar absorptance of cover
			Array1< Real64 > const & CoversAbsGndDiffuse, // ground diffuse solar absorptance of cover
			Array1< Real64 > const & CoverAbs, // solar rad weighted covers absorptance
			Real64 const TimeElapsed, // Fraction of the current hour that has elapsed (h)
			Real64 const UbLoss, // Over all bottom loss coefficient [W/m2C]
			Real64 const UsLoss, // Over all side loss coefficient [W/m2C]
			Real64 const AreaRatio, // Side area to collector area ratio [-]
			Real64 const RefSkyDiffInnerCover, // Sky diffuse refl of inner cover (cover 1)
			Real64 const RefGrnDiffInnerCover, // ground diffuse refl of inner cover (cover 1)
			Real64 const RefDiffInnerCover, // diffuse reflectance of the inner cover (cover 1) from bottom
			Real64 const SavedTempOfWater, // water temp carried from time step to time step [C]
			Real64 const SavedTempOfAbsPlate, // abs plate temp carried from time step to time step [C]
			Real64 const SavedTempOfInnerCover, // inner cover temp carried from time step to time step [C]
			Real64 const SavedTempOfOuterCover, // outer cover temp carried from time step to time step [C]
			Real64 const SavedTempCollectorOSCM, // Temperature of collector back from OSCM at previous time step [C]
			Real64 const Length, // characteristic length of the abs plate
			Real64 const TiltR2V, // collector tilt angle from the vertical [degree]
			Real64 const Tilt, // collector tilt angle from the horizontal [degree]
			Real64 const CosTilt, // cosine of colector tilt angle [-]
			Real64 const SinTilt, // sine of 1.8 times colector tilt angle [-]
			Real64 const SideArea, // weighted collector side area (m2)
			Real64 const Area, // collector area (m2)
			Real64 const Volume, // collector net volume (m3)
			bool const OSCM_ON, // Boundary condition is OSCM
			bool const InitICS // used to initialize ICS variables only
		) :
			Name( Name ),
			BCType( BCType ),
			BCName( BCName ),
			OSCMName( OSCMName ),
			VentCavIndex( VentCavIndex ),
			ICSType_Num( ICSType_Num ),
			TypeNum( TypeNum ),
			WLoopNum( WLoopNum ),
			WLoopSideNum( WLoopSideNum ),
			WLoopBranchNum( WLoopBranchNum ),
			WLoopCompNum( WLoopCompNum ),
			Init( Init ),
			InitSizing( InitSizing ),
			Parameters( Parameters ),
			Surface( Surface ),
			InletNode( InletNode ),
			InletTemp( InletTemp ),
			OutletNode( OutletNode ),
			OutletTemp( OutletTemp ),
			MassFlowRate( MassFlowRate ),
			MassFlowRateMax( MassFlowRateMax ),
			VolFlowRateMax( VolFlowRateMax ),
			ErrIndex( ErrIndex ),
			IterErrIndex( IterErrIndex ),
			IncidentAngleModifier( IncidentAngleModifier ),
			Efficiency( Efficiency ),
			Power( Power ),
			HeatGain( HeatGain ),
			HeatLoss( HeatLoss ),
			Energy( Energy ),
			HeatRate( HeatRate ),
			HeatEnergy( HeatEnergy ),
			StoredHeatRate( StoredHeatRate ),
			StoredHeatEnergy( StoredHeatEnergy ),
			HeatGainRate( HeatGainRate ),
			HeatGainEnergy( HeatGainEnergy ),
			HeatLossRate( HeatLossRate ),
			HeatLossEnergy( HeatLossEnergy ),
			SkinHeatLossRate( SkinHeatLossRate ),
			CollHeatLossEnergy( CollHeatLossEnergy ),
			TauAlpha( TauAlpha ),
			UTopLoss( UTopLoss ),
			TempOfWater( TempOfWater ),
			TempOfAbsPlate( TempOfAbsPlate ),
			TempOfInnerCover( TempOfInnerCover ),
			TempOfOuterCover( TempOfOuterCover ),
			TauAlphaNormal( TauAlphaNormal ),
			TauAlphaSkyDiffuse( TauAlphaSkyDiffuse ),
			TauAlphaGndDiffuse( TauAlphaGndDiffuse ),
			TauAlphaBeam( TauAlphaBeam ),
			CoversAbsSkyDiffuse( 2, CoversAbsSkyDiffuse ),
			CoversAbsGndDiffuse( 2, CoversAbsGndDiffuse ),
			CoverAbs( 2, CoverAbs ),
			TimeElapsed( TimeElapsed ),
			UbLoss( UbLoss ),
			UsLoss( UsLoss ),
			AreaRatio( AreaRatio ),
			RefSkyDiffInnerCover( RefSkyDiffInnerCover ),
			RefGrnDiffInnerCover( RefGrnDiffInnerCover ),
			RefDiffInnerCover( RefDiffInnerCover ),
			SavedTempOfWater( SavedTempOfWater ),
			SavedTempOfAbsPlate( SavedTempOfAbsPlate ),
			SavedTempOfInnerCover( SavedTempOfInnerCover ),
			SavedTempOfOuterCover( SavedTempOfOuterCover ),
			SavedTempCollectorOSCM( SavedTempCollectorOSCM ),
			Length( Length ),
			TiltR2V( TiltR2V ),
			Tilt( Tilt ),
			CosTilt( CosTilt ),
			SinTilt( SinTilt ),
			SideArea( SideArea ),
			Area( Area ),
			Volume( Volume ),
			OSCM_ON( OSCM_ON ),
			InitICS( InitICS )
		{}

	};

	// Object Data
	extern Array1D< ParametersData > Parameters;
	extern Array1D< CollectorData > Collector;

	// Functions

	void
	SimSolarCollector(
		int const EquipTypeNum,
		std::string const & CompName,
		int & CompIndex,
		bool const InitLoopEquip,
		bool const FirstHVACIteration
	);

	void
	GetSolarCollectorInput();

	void
	InitSolarCollector( int const CollectorNum );

	void
	CalcSolarCollector( int const CollectorNum );

	Real64
	IAM(
		int const ParamNum, // Collector parameters object number
		Real64 const IncidentAngle // Angle of incidence (radians)
	);

	void
	CalcICSSolarCollector( int const ColleNum );

	void
	ICSCollectorAnalyticalSoluton(
		int const ColleNum, // solar collector index
		Real64 const SecInTimeStep, // seconds in a time step
		Real64 const a1, // coefficient of ODE for Tp
		Real64 const a2, // coefficient of ODE for Tp
		Real64 const a3, // coefficient of ODE for Tp
		Real64 const b1, // coefficient of ODE for TW
		Real64 const b2, // coefficient of ODE for TW
		Real64 const b3, // coefficient of ODE for TW
		Real64 const TempAbsPlateOld, // absorber plate temperature at previous time step [C]
		Real64 const TempWaterOld, // collector water temperature at previous time step [C]
		Real64 & TempAbsPlate, // absorber plate temperature at current time step [C]
		Real64 & TempWater, // collector water temperature at current time step [C]
		bool const AbsorberPlateHasMass // flag for absober thermal mass
	);

	void
	CalcTransAbsorProduct(
		int const ColleNum, // Collector object number
		Real64 const IncidAngle // Angle of incidence (radians)
	);

	void
	CalcTransRefAbsOfCover(
		int const ColleNum, // Collector object number
		Real64 const IncidentAngle, // Angle of incidence (radians)
		Real64 & TransSys, // cover system solar transmittance
		Real64 & ReflSys, // cover system solar reflectance
		Real64 & AbsCover1, // Inner cover solar absorbtance
		Real64 & AbsCover2, // Outer cover solar absorbtance
		Optional_bool_const InOUTFlag = _, // flag for calc. diffuse solar refl of cover from inside out
		Optional< Real64 > RefSysDiffuse = _ // cover system solar reflectance from inner to outer cover
	);

	void
	CalcHeatTransCoeffAndCoverTemp( int const ColleNum ); // Collector object number

	Real64
	CalcConvCoeffBetweenPlates(
		Real64 const TempSurf1, // temperature of surface 1
		Real64 const TempSurf2, // temperature of surface 1
		Real64 const AirGap, // characteristic length [m]
		Real64 const CosTilt, // cosine of surface tilt angle relative to the horizontal
		Real64 const SinTilt // sine of surface tilt angle relative to the horizontal
	);

	Real64
	CalcConvCoeffAbsPlateAndWater(
		Real64 const TAbsorber, // temperature of absorber plate [C]
		Real64 const TWater, // temperature of water [C]
		Real64 const Lc, // characteristic length [m]
		Real64 const TiltR2V // collector tilt angle relative to the vertical [degree]
	);

	void
	UpdateSolarCollector( int const CollectorNum );

	void
	ReportSolarCollector( int const CollectorNum );

	void
	GetExtVentedCavityIndex(
		int const SurfacePtr,
		int & VentCavIndex
	);

	void
	GetExtVentedCavityTsColl(
		int const VentModNum,
		Real64 & TsColl
	);

	//     NOTICE

	//     Copyright © 1996-2014 The Board of Trustees of the University of Illinois
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

} // SolarCollectors

} // EnergyPlus

#endif
