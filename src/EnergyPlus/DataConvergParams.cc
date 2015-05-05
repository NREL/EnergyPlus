// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <DataConvergParams.hh>
#include <DataPrecisionGlobals.hh>

namespace EnergyPlus {

namespace DataConvergParams {

	// PURPOSE OF THIS MODULE:
	// This data-only module sets the parameters that control the convergence
	// of the HVAC simulation.

	// Using/Aliasing
	using namespace DataPrecisionGlobals;

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:

	// Note: Unless otherwise noted, the tolerance parameters listed below were chosen
	// to represent educated guesses at what the tolerances for individual physical
	// parameters should be.
	Real64 const HVACEnthalpyToler( 260.0 ); // Tolerance for enthalpy comparisons (in kJ/kgK)
	Real64 const HVACFlowRateToler( 0.01 ); // Tolerance for mass flow rate convergence (in kg/s) [~20 CFM]
	Real64 const HVACFlowRateSlopeToler( 0.001 ); // Slope tolerance for mass flow, kg/s/iteration
	Real64 const HVACFlowRateOscillationToler( 0.0000001 ); // tolerance for detecting duplicate flow rate in stack
	Real64 const HVACHumRatToler( 0.0001 ); // Tolerance for humidity ratio comparisons (kg water/kg dryair)
	Real64 const HVACHumRatSlopeToler( 0.00001 ); // Slope tolerance for humidity ratio, kg water/kg-dryair/iteration
	Real64 const HVACHumRatOscillationToler( 0.00000001 ); // tolerance for detecting duplicate humidity ratio in stack
	Real64 const HVACQualityToler( 0.01 ); // Tolerance for fluid quality comparisons (dimensionless)
	Real64 const HVACPressToler( 10.0 ); // Tolerance for pressure comparisons (in Pascals)
	Real64 const HVACTemperatureToler( 0.01 ); // Tolerance for temperature comparisons (in degrees C or K)
	Real64 const HVACTemperatureSlopeToler( 0.001 ); // Slope tolerance for Temperature, Deg C/iteration
	Real64 const HVACTemperatureOscillationToler( 0.000001 ); // tolerance for detecting duplicate temps in stack
	Real64 const HVACEnergyToler( 10.0 ); // Tolerance for Energy comparisons (in Watts W)
	// to be consistent, should be 20.d0 (BG Aug 2012)

	Real64 const HVACCpApprox( 1004.844 ); // Air Cp (20C,0.0Kg/Kg) Only for energy Tolerance Calculation
	// Only used to scale the answer for a more intuitive answer for comparison

	Real64 const PlantEnthalpyToler( 0.10 ); // Tolerance for enthalpy comparisons (in kJ/kgK)
	Real64 const PlantFlowRateToler( 0.001 ); // Tolerance for mass flow rate convergence (in kg/s) [~2 CFM]
	Real64 const PlantFlowRateOscillationToler( 0.0000001 );
	Real64 const PlantFlowRateSlopeToler( 0.0001 ); // Slope tolerance for mass flow, kg/s/iteration

	Real64 const PlantPressToler( 10.0 ); // Tolerance for pressure comparisons (in Pascals)
	Real64 const PlantTemperatureToler( 0.01 ); // Tolerance for temperature comparisons (in degrees C or K)
	Real64 const PlantTemperatureSlopeToler( 0.001 ); // Slope tolerance for Temperature, Deg C/iteration
	Real64 const PlantTemperatureOscillationToler( 0.000001 ); // tolerance for detecting duplicate temps in stack

	Real64 const PlantEnergyToler( 10.0 ); // Tolerance for Energy comparisons (in Watts W)

	Real64 const PlantCpApprox( 4180.0 ); // Approximate Cp used in Interface manager for
	// Energy Tolerance Calculation, used to scale the answer
	// for a more intuitive answer for comparison
	Real64 const PlantFlowFlowRateToler( 0.01 ); // Tolerance for mass flow rate convergence (in kg/s)
	Real64 const PlantLowFlowRateToler(0.000001); // Tolerance for low flow rate used for determining when
	//plant pumps can be shut down

	int const ConvergLogStackDepth( 10 );
	Array1D< Real64 > const ConvergLogStackARR( ConvergLogStackDepth, { 0.0, -1.0, -2.0, -3.0, -4.0, -5.0, -6.0, -7.0, -8.0, -9.0 } );
	Real64 const sum_ConvergLogStackARR( sum( ConvergLogStackARR ) );
	Real64 const square_sum_ConvergLogStackARR( pow_2( sum_ConvergLogStackARR ) );
	Real64 const sum_square_ConvergLogStackARR( sum( pow( ConvergLogStackARR, 2 ) ) );

	int const CalledFromAirSystemDemandSide( 100 );
	int const CalledFromAirSystemSupplySideDeck1( 101 );
	int const CalledFromAirSystemSupplySideDeck2( 102 );
	// DERIVED TYPE DEFINITIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:

	int AirLoopConvergFail( 0 );

	Real64 MinTimeStepSys( ( 1.0 / 60.0 ) ); // =1 minute
	Real64 MinTimeStepTol( 1.0e-4 ); // = min allowable for ABS(1.-TimeStepSys/(MinTimeStepSys))
	Real64 MaxZoneTempDiff( 0.3 ); // 0.3 C = (1% OF 300 C) = max allowable difference between
	//   zone air temp at Time=T and Time=T-1
	Real64 MinSysTimeRemaining( ( 1.0 / 3600.0 ) ); // = 1 second
	int MaxIter( 20 ); // maximum number of iterations allowed

	int MaxPlantSubIterations( 8 ); // Iteration Max for Plant Simulation sub iterations
	int MinPlantSubIterations( 2 ); // Iteration Min for Plant Simulation sub iterations

	// Object Data
	Array1D< HVACZoneInletConvergenceStruct > ZoneInletConvergence;
	Array1D< HVACAirLoopIterationConvergenceStruct > AirLoopConvergence;
	Array1D< PlantIterationConvergenceStruct > PlantConvergence;

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

} // DataConvergParams

} // EnergyPlus
