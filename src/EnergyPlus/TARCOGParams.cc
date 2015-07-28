// EnergyPlus Headers
#include <TARCOGParams.hh>
#include <DataPrecisionGlobals.hh>

namespace EnergyPlus {

namespace TARCOGParams {

	// MODULE INFORMATION:
	//       AUTHOR         Simon Vidanovic
	//       DATE WRITTEN   June/22/2010
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	//  Revision: 6.0.36  (June/22/2010)
	//   - Initial setup, extracted from TARCOG.fi
	//   - Moved Standards and MaxGas into GasParams module (gasses project)

	// PURPOSE OF THIS MODULE:
	// Module which contains common TARCOG parameters and constants

	// METHODOLOGY EMPLOYED:
	// <description>

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// USE STATEMENTS:

	// Using/Aliasing
	using namespace DataPrecisionGlobals;

	// Data
	//REAL(r64), parameter :: StefanBoltzmannConst    = 5.6697d-8     ! Stefan-Boltzman constant (5.6697e-8 [W/m^2K^4])
	//REAL(r64), parameter :: GravityConstant = 9.807d0
	Real64 const e( 2.718281828459 );
	//REAL(r64), parameter :: MaxHr = 100  ! used in iterations in case temperatures on surfaces reaches identical values
	Real64 const DeflectionRelaxation( 0.005 ); // Deflection relaxation parameter
	int const DeflectionMaxIterations( 400 ); // maximum number of deflection iterations
	Real64 const DeflectionErrorMargin( 0.01 ); // maximum temperature difference on layers for deflection iterations
	//pi is moved to gasses since it is used there now
	//REAL(r64), parameter :: pi       = 3.14159265358979323846d0

	int const maxpan( 100 ); // maximum number of monolithic glazing layers (100)
	//integer, parameter :: maxlay   = 1000         ! maximum number of layers (including laminates) (1000)
	int const maxlay( 100 ); // maximum number of layers (including laminates) (100)
	int const MaxGap( maxlay - 1 ); // maximum number of gaps (between layers)
	int const maxlay1( maxlay + 1 ); // maximum number of 'gaps', including in and out (maxlay+1)
	int const maxlay2( maxlay * 2 ); // maximum number of glass surfaces (maxlay*2)
	int const maxlay3( maxlay2 + 1 ); // maximum number of ? (maxlay2+1)
	int const maxlay4( maxlay * 4 ); // maximum number of ? (maxlay*4)
	int const maxslice( 100 ); // maximum nuber of slices (100)

	//integer, parameter :: MaxThetaArray = 200     ! maximum number for theta array

	// Debug flags
	int const noDebug( 0 );
	int const appendResultsToFile( 1 );
	int const resultsToNewFile( 2 );
	int const saveIntermediateResults( 3 ); // this will create new file

	int const minDebugFlag( 0 );
	int const maxDebugFlag( 3 );

	// to keep info that certain file is not open for writing
	int const statusClosed( 0 );

	//  Layer types:
	int const SPECULAR( 0 );
	int const VENETBLIND( 1 );
	int const WOVSHADE( 2 );
	int const PERFORATED( 3 );
	int const DIFFSHADE( 4 );
	int const BSDF( 5 );

	int const MinLayType( 0 );
	int const MaxLayType( 5 );

	//  Thermal models:
	int const THERM_MOD_ISO15099( 0 );
	int const THERM_MOD_SCW( 1 );
	int const THERM_MOD_CSM( 2 );

	int const MinThermalMode( 0 );
	int const MaxThermalMode( 2 );

	int const NO_SupportPillar( 0 );
	int const YES_SupportPillar( 1 );

	//Deflection parameters
	int const NO_DEFLECTION_CALCULATION( 0 );
	int const DEFLECTION_CALC_TEMPERATURE( 1 );
	int const DEFLECTION_CALC_GAP_WIDTHS( 2 );

	//definition of parameters for deflection sum.  These parameters define maximum number of loop to which sum
	//will perform. By equation, these numbers will go to infinite and some test showed that going to nmax and mmax
	//values would produce enough precision
	int const mmax( 5 ); // top m value for which "deflection sum" will be calculated
	int const nmax( 5 ); // top n value for which "deflection sum" will be calculated

	//  CalcForcedVentilation flag:
	//  0 = Skip forced ventilation calc
	//  1 = Allow forced ventilation calc
	int const CalcForcedVentilation( 0 );

	//  Calculation outcome
	int const CALC_UNKNOWN( 0 );
	int const CALC_OK( 1 );
	int const CALC_DIVERGE( 2 );
	int const CALC_OSC_OK( 3 );

	int const NumOfIterations( 100 );

	// Program will examine convergence parameter in each iteration.  That convergence parameter should decrease each time.
	// In case that is not happening program will tolerate certain number of tries before declare convergence
	// (or decrease relaxation parameter)
	int const NumOfTries( 5 );
	//integer, parameter :: NewtonIterations = 75 ! shows when to swith to Newton
	Real64 const RelaxationStart( 0.6 ); // Has to be between 0 and 1
	Real64 const RelaxationDecrease( 0.1 ); // Step for which relaxation parameter will decrease

	// Convergence parameters
	Real64 const tempCorrection( 1e-10 ); // used in case outside or inside temperature approaches tamb or troom
	Real64 const ConvergenceTolerance( 1e-2 ); // tolerance used within iterations

	// Airflow iterations
	Real64 const AirflowConvergenceTolerance( 1e-2 );
	Real64 const AirflowRelaxationParameter( 0.9 );

	Real64 const TemperatureQuessDiff( 1.0 ); // in case outside and inside temperatures are identical

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

} // TARCOGParams

} // EnergyPlus
