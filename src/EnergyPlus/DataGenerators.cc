// EnergyPlus Headers
#include <DataGenerators.hh>
#include <DataPrecisionGlobals.hh>

namespace EnergyPlus {

namespace DataGenerators {

	// MODULE INFORMATION:
	//       AUTHOR         B Griffith
	//       DATE WRITTEN   March 2005
	//       MODIFIED
	//       RE-ENGINEERED  July 2006 BG, generalized and added data for ICE/SE model micro CHP

	// PURPOSE OF THIS MODULE:
	// This data-only module is a repository for the variables that relate specifically
	// to the Fuel cell and Micro CHP modeling in EnergyPlus
	//  the data for the older BLAST generators are in those component's modules

	// METHODOLOGY EMPLOYED:

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:
	int const NormalizedCurveMode( 1 ); // mode where efficiency curves are modifier curves
	int const DirectCurveMode( 2 ); // mode where efficiency curves are direct

	int const ConstantRateSkinLoss( 1 ); // fixed rate mode for skin losses
	int const UADTSkinLoss( 2 ); // UAdelta T mode for skin losses
	int const QuadraticFuelNdotSkin( 3 ); // Quadratic function of fuel flow for skin losses

	int const QuadraticFuncofNdot( 1 ); // function of fuel rate mode for air flow
	int const ConstantStoicsAirRat( 2 ); // Constant air ratio in stoics with fuel constituents
	int const QuadraticFuncofPel( 3 ); // function of electric power mode

	int const NoRecoveryOnAirIntake( 101 ); // mode for controlling intake air heat recovery
	int const RecoverBurnInvertBatt( 102 ); // mode for controlling intake air heat recovery
	int const RecoverAuxiliaryBurner( 103 ); // mode for controlling intake air heat recovery
	int const RecoverInverterBatt( 104 ); // mode for controlling intake air heat recovery
	int const RecoverInverter( 105 ); // mode for controlling intake air heat recovery
	int const RecoverBattery( 106 ); // mode for controlling intake air heat recovery

	int const RegularAir( 1 );
	int const UserDefinedConstituents( 2 );

	int const FuelInTempFromNode( 1 );
	int const FuelInTempSchedule( 2 );

	int const WaterInReformMains( 21 );
	int const WaterInReformAirNode( 22 );
	int const WaterInReformWaterNode( 23 );
	int const WaterInReformSchedule( 24 );

	int const InverterEffConstant( 1 );
	int const InverterEffQuadratic( 2 );

	int const FixedEffectiveness( 11 ); // exhaust gas HX modeling mode
	int const LMTDempiricalUAeff( 12 ); // exhaust gas HX modeling mode
	int const LMTDfundementalUAeff( 13 ); // exhaust gas HX modeling mode
	int const Condensing( 14 ); // exhaust gas HX modeling mode

	int const SimpleEffConstraints( 21 ); // electrical storage modeling mode
	int const LeadAcidBatterySaupe( 22 ); // electrical storage modeling mode
	int const LeadAcidBatterManwellMcGowan( 23 ); // electrical storage modeling mode

	int const SurroundingZone( 31 );
	int const AirInletForFC( 32 );

	int const OpModeOff( 1 ); // CHP operating mode OFF
	int const OpModeStandby( 2 ); // CHP operating mode Stand By
	int const OpModeWarmUp( 3 ); // CHP operating mode Warm Up or start up
	int const OpModeNormal( 4 ); // CHP operating mode Normal
	int const OpModeCoolDown( 5 ); // CHP operating mode Cool down or shut down

	int const fuelModeGaseousConstituents( 301 );
	int const fuelModeGenericLiquid( 302 );

	Real64 const MinProductGasTemp( 100.0 ); // Minimum bound on search for product gas temps
	Real64 const MaxProductGasTemp( 2000.0 ); // Maximum bound on search for product gas temps

	int const NISTShomate( 41 );
	int const NASAPolynomial( 42 );

	Real64 const RinKJperMolpK( 0.0083145 ); // R is ideal gas constant (kJ/mol-K)
	Real64 const InitHRTemp( 50.0 ); // Initialization temperature for heat recovery water

	Real64 const ImBalanceTol( 0.00001 ); // used as fraction of electrical power at power module

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:

	int NumFuelConstit( 0 );
	int NumGeneratorFuelSups( 0 );
	int NumFuelCellGenerators( 0 ); // number of SOFC Generators specified in input
	int NumMicroCHPs( 0 );
	int NumMicroCHPParams( 0 ); // number of parameter sets for micro chp
	int NumGensWDynamics( 0 ); // number of dynamics controls for generators

	// Object Data
	Array1D< FCDataStruct > FuelCell; // dimension to number of machines
	Array1D< GasPropertyDataStruct > GasPhaseThermoChemistryData;
	Array1D< GeneratorFuelSupplyDataStruct > FuelSupply; // fuel supply (reused across various)
	Array1D< MicroCHPDataStruct > MicroCHP;
	Array1D< MicroCHPParamsNonNormalized > MicroCHPParamInput; // Used during get input then put into nested
	Array1D< GeneratorDynamicsManagerStruct > GeneratorDynamics;

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

} // DataGenerators

} // EnergyPlus
