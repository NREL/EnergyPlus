#ifndef DataComplexFenestration_hh_INCLUDED
#define DataComplexFenestration_hh_INCLUDED

// ObjexxFCL Headers

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace DataComplexFenestration {

	// Using/Aliasing

	// Data
	// Parameters for complex shade
	extern int const csVenetian;
	extern int const csWoven;
	extern int const csPerforated;
	extern int const csOtherShadingType;
	extern int const csBSDF;

	// Parameters for gas definitions
	extern int const GasCoeffsCustom;
	extern int const GasCoeffsAir;
	extern int const GasCoeffsArgon;
	extern int const GasCoeffsKrypton;
	extern int const GasCoeffsXenon;

	// Parameters for Thermal Algorithm
	//INTEGER, PARAMETER :: taTarcog = 0
	//INTEGER, PARAMETER :: taWinkelmann = 1

	// Parameters for calculation standard
	extern int const csISO15099;
	extern int const csEN673Declared;
	extern int const csEN673Design;

	// Parameters for thermal model
	extern int const tmISO15099;
	extern int const tmScaledCavityWidth;
	extern int const tmConvectiveScalarModel_NoSDThickness;
	extern int const tmConvectiveScalarModel_WithSDThickness;

	// Parameters for deflection model
	extern int const dmNoDeflection;
	extern int const dmTemperatureAndPressureInput;
	extern int const dmMeasuredDeflection;

	// Types

	struct GapSupportPillar
	{
		// Members
		std::string Name; // Name of support pillar
		Real64 Spacing; // Spacing between centers of support pillars (m)
		Real64 Radius; // Support pillar radius (m)

		// Default Constructor
		GapSupportPillar() :
			Spacing( 0.0 ),
			Radius( 0.0 )
		{}

		// Member Constructor
		GapSupportPillar(
			std::string const & Name, // Name of support pillar
			Real64 const Spacing, // Spacing between centers of support pillars (m)
			Real64 const Radius // Support pillar radius (m)
		) :
			Name( Name ),
			Spacing( Spacing ),
			Radius( Radius )
		{}

	};

	struct GapDeflectionState
	{
		// Members
		std::string Name; // Name of deflection state
		Real64 DeflectedThickness;

		// Default Constructor
		GapDeflectionState() :
			DeflectedThickness( 0.0 )
		{}

		// Member Constructor
		GapDeflectionState(
			std::string const & Name, // Name of deflection state
			Real64 const DeflectedThickness
		) :
			Name( Name ),
			DeflectedThickness( DeflectedThickness )
		{}

	};

	struct WindowComplexShade
	{
		// Members
		std::string Name; // Name for complex shade
		int LayerType; // Layer type (OtherShadingType, Venetian, Woven, Perforated)
		Real64 Thickness; // Layer thickness (m)
		Real64 Conductivity; // Layer conductivity (W/m2K)
		Real64 IRTransmittance; // IR Transmittance
		Real64 FrontEmissivity; // Emissivity of front suraface
		Real64 BackEmissivity; // Emissivity of back surface
		Real64 TopOpeningMultiplier; // Coverage percent for top opening (%)
		Real64 BottomOpeningMultiplier; // Coverage percent for bottom opening (%)
		Real64 LeftOpeningMultiplier; // Coverage percent for left opening (%)
		Real64 RightOpeningMultiplier; // Coverage percent for right opening (%)
		Real64 FrontOpeningMultiplier; // Coverage percent for front opening (%)
		Real64 SlatWidth; // Slat width (m)
		Real64 SlatSpacing; // Slat spacing (m)
		Real64 SlatThickness; // Slat thickness (m)
		Real64 SlatAngle; // Slat angle (deg)
		Real64 SlatConductivity; // Slat conductivity (W/m2K)
		Real64 SlatCurve; // Curvature radius of slat (if =0 then flat) (m)

		// Default Constructor
		WindowComplexShade() :
			LayerType( -1 ),
			Thickness( 0.0 ),
			Conductivity( 0.0 ),
			IRTransmittance( 0.0 ),
			FrontEmissivity( 0.0 ),
			BackEmissivity( 0.0 ),
			TopOpeningMultiplier( 0.0 ),
			BottomOpeningMultiplier( 0.0 ),
			LeftOpeningMultiplier( 0.0 ),
			RightOpeningMultiplier( 0.0 ),
			FrontOpeningMultiplier( 0.0 ),
			SlatWidth( 0.0 ),
			SlatSpacing( 0.0 ),
			SlatThickness( 0.0 ),
			SlatAngle( 0.0 ),
			SlatConductivity( 0.0 ),
			SlatCurve( 0.0 )
		{}

		// Member Constructor
		WindowComplexShade(
			std::string const & Name, // Name for complex shade
			int const LayerType, // Layer type (OtherShadingType, Venetian, Woven, Perforated)
			Real64 const Thickness, // Layer thickness (m)
			Real64 const Conductivity, // Layer conductivity (W/m2K)
			Real64 const IRTransmittance, // IR Transmittance
			Real64 const FrontEmissivity, // Emissivity of front suraface
			Real64 const BackEmissivity, // Emissivity of back surface
			Real64 const TopOpeningMultiplier, // Coverage percent for top opening (%)
			Real64 const BottomOpeningMultiplier, // Coverage percent for bottom opening (%)
			Real64 const LeftOpeningMultiplier, // Coverage percent for left opening (%)
			Real64 const RightOpeningMultiplier, // Coverage percent for right opening (%)
			Real64 const FrontOpeningMultiplier, // Coverage percent for front opening (%)
			Real64 const SlatWidth, // Slat width (m)
			Real64 const SlatSpacing, // Slat spacing (m)
			Real64 const SlatThickness, // Slat thickness (m)
			Real64 const SlatAngle, // Slat angle (deg)
			Real64 const SlatConductivity, // Slat conductivity (W/m2K)
			Real64 const SlatCurve // Curvature radius of slat (if =0 then flat) (m)
		) :
			Name( Name ),
			LayerType( LayerType ),
			Thickness( Thickness ),
			Conductivity( Conductivity ),
			IRTransmittance( IRTransmittance ),
			FrontEmissivity( FrontEmissivity ),
			BackEmissivity( BackEmissivity ),
			TopOpeningMultiplier( TopOpeningMultiplier ),
			BottomOpeningMultiplier( BottomOpeningMultiplier ),
			LeftOpeningMultiplier( LeftOpeningMultiplier ),
			RightOpeningMultiplier( RightOpeningMultiplier ),
			FrontOpeningMultiplier( FrontOpeningMultiplier ),
			SlatWidth( SlatWidth ),
			SlatSpacing( SlatSpacing ),
			SlatThickness( SlatThickness ),
			SlatAngle( SlatAngle ),
			SlatConductivity( SlatConductivity ),
			SlatCurve( SlatCurve )
		{}

	};

	struct WindowThermalModelParams
	{
		// Members
		std::string Name; // Window thermal model name
		int CalculationStandard; // Tarcog calculation standard
		int ThermalModel; // Tarcog thermal model
		Real64 SDScalar; // SDScalar coefficient
		int DeflectionModel; // Deflection model
		Real64 VacuumPressureLimit; // Pressure limit at which it will be considered vacuum gas state
		Real64 InitialTemperature; // Window(s) temperature in time of fabrication
		Real64 InitialPressure; // Window(s) pressure in time of fabrication

		// Default Constructor
		WindowThermalModelParams() :
			CalculationStandard( -1 ),
			ThermalModel( -1 ),
			SDScalar( 0.0 ),
			DeflectionModel( -1 ),
			VacuumPressureLimit( 0.0 ),
			InitialTemperature( 0.0 ),
			InitialPressure( 0.0 )
		{}

		// Member Constructor
		WindowThermalModelParams(
			std::string const & Name, // Window thermal model name
			int const CalculationStandard, // Tarcog calculation standard
			int const ThermalModel, // Tarcog thermal model
			Real64 const SDScalar, // SDScalar coefficient
			int const DeflectionModel, // Deflection model
			Real64 const VacuumPressureLimit, // Pressure limit at which it will be considered vacuum gas state
			Real64 const InitialTemperature, // Window(s) temperature in time of fabrication
			Real64 const InitialPressure // Window(s) pressure in time of fabrication
		) :
			Name( Name ),
			CalculationStandard( CalculationStandard ),
			ThermalModel( ThermalModel ),
			SDScalar( SDScalar ),
			DeflectionModel( DeflectionModel ),
			VacuumPressureLimit( VacuumPressureLimit ),
			InitialTemperature( InitialTemperature ),
			InitialPressure( InitialPressure )
		{}

	};

} // DataComplexFenestration

} // EnergyPlus

#endif
