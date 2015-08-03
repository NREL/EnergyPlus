#ifndef DataBranchAirLoopPlant_hh_INCLUDED
#define DataBranchAirLoopPlant_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace DataBranchAirLoopPlant {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// Parameters for tolerance
	extern Real64 const MassFlowTolerance; // minimum significant mass flow rate (kg/s)

	// Pressure Curve Type: None, pressure, or generic curve (if generic it will be a postive value which is the curve manager index)
	extern int const PressureCurve_Error;
	extern int const PressureCurve_None;
	extern int const PressureCurve_Pressure;
	extern int const PressureCurve_Generic;

	// Parameters for flow Control Types for branch flow resolution inside splitter/mixers
	extern int const ControlType_Unknown;
	extern int const ControlType_Active; // 'Active'
	extern int const ControlType_Passive; // 'Passive'
	extern int const ControlType_SeriesActive; // 'SeriesActive'
	extern int const ControlType_Bypass; // 'Bypass
	extern Array1D_string const cControlType;

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	extern int NumPressureCurves;

	// Types

	struct PlantPressureCurveData
	{
		// Members
		std::string Name;
		Real64 EquivDiameter; // - An effective diameter for calculation of Re & e/D [m]
		Real64 MinorLossCoeff; // - K factor                                          [-]
		Real64 EquivLength; // - An effective length to apply friction calculation [m]
		Real64 EquivRoughness; // - An effective roughness (e) to calculate e/D       [m]
		bool ConstantFPresent; // - Signal for if a constant value of f was entered
		Real64 ConstantF; // - Constant value of f (if applicable)               [-]
		bool EMSOverrideOn; // if TRUE, then EMS is calling to override curve value
		Real64 EMSOverrideCurveValue; // Value of curve result EMS is directing to use
		//  report variables.
		Real64 CurveOutput;
		Real64 CurveInput1; // - MassFlow                                         [kg/s]
		Real64 CurveInput2; // - Density                                          [kg/m3]
		Real64 CurveInput3; // - Velocity                                         [m/s]

		// Default Constructor
		PlantPressureCurveData() :
			EquivDiameter( 0.0 ),
			MinorLossCoeff( 0.0 ),
			EquivLength( 0.0 ),
			EquivRoughness( 0.0 ),
			ConstantFPresent( false ),
			ConstantF( 0.0 ),
			EMSOverrideOn( false ),
			EMSOverrideCurveValue( 0.0 ),
			CurveOutput( 0.0 ),
			CurveInput1( 0.0 ),
			CurveInput2( 0.0 ),
			CurveInput3( 0.0 )
		{}

		// Member Constructor
		PlantPressureCurveData(
			std::string const & Name,
			Real64 const EquivDiameter, // - An effective diameter for calculation of Re & e/D [m]
			Real64 const MinorLossCoeff, // - K factor                                          [-]
			Real64 const EquivLength, // - An effective length to apply friction calculation [m]
			Real64 const EquivRoughness, // - An effective roughness (e) to calculate e/D       [m]
			bool const ConstantFPresent, // - Signal for if a constant value of f was entered
			Real64 const ConstantF, // - Constant value of f (if applicable)               [-]
			bool const EMSOverrideOn, // if TRUE, then EMS is calling to override curve value
			Real64 const EMSOverrideCurveValue, // Value of curve result EMS is directing to use
			Real64 const CurveOutput,
			Real64 const CurveInput1, // - MassFlow                                         [kg/s]
			Real64 const CurveInput2, // - Density                                          [kg/m3]
			Real64 const CurveInput3 // - Velocity                                         [m/s]
		) :
			Name( Name ),
			EquivDiameter( EquivDiameter ),
			MinorLossCoeff( MinorLossCoeff ),
			EquivLength( EquivLength ),
			EquivRoughness( EquivRoughness ),
			ConstantFPresent( ConstantFPresent ),
			ConstantF( ConstantF ),
			EMSOverrideOn( EMSOverrideOn ),
			EMSOverrideCurveValue( EMSOverrideCurveValue ),
			CurveOutput( CurveOutput ),
			CurveInput1( CurveInput1 ),
			CurveInput2( CurveInput2 ),
			CurveInput3( CurveInput3 )
		{}

	};

	// Object Data
	extern Array1D< PlantPressureCurveData > PressureCurve;

} // DataBranchAirLoopPlant

} // EnergyPlus

#endif
