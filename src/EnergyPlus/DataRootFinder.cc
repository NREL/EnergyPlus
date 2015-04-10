// EnergyPlus Headers
#include <DataRootFinder.hh>
#include <DataPrecisionGlobals.hh>

namespace EnergyPlus {

namespace DataRootFinder {

	// MODULE INFORMATION:
	//       AUTHOR         Dimitri Curtil
	//       DATE WRITTEN   February 2006
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This data-only module is a repository for variables and types used by the
	// RootFinder module.

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS
	int const iSlopeNone( 0 ); // Undefined slope specification
	int const iSlopeIncreasing( 1 ); // For overall increasing function F(X) between min and max points
	int const iSlopeDecreasing( -1 ); // For overall decreasing function F(X) between min and max points

	// Error because the overall slope appears to be flat between the min and max points,
	// implying that the function might be singular over the interval:
	// F(XMin) == F(XMax)
	int const iStatusErrorSingular( -4 );
	// Error because the overall slope assumption is not observed at the min and max points:
	// - for an increasing function F(X), we expect F(XMin) < F(XMax)  otherwise error
	// - for a decreasing function F(X),  we expect F(XMin) > F(XMax)  otherwise error
	// Note that this error status does not detect strict monotonicity at points
	// between the min and max points.
	int const iStatusErrorSlope( -3 );
	// Error because the current candidate X does not lie within the current lower an upper points:
	// X < XLower or X > XUpper
	int const iStatusErrorBracket( -2 );
	// Error because the current candidate X does not lie within the min and max points:
	// X < XMin or X > XMax
	int const iStatusErrorRange( -1 );

	int const iStatusNone( 0 ); // Indeterminate error state (not converged), also default state
	int const iStatusOK( 1 ); // Unconstrained convergence achieved with root solution so that:
	// XMin < XRoot < XMax
	int const iStatusOKMin( 2 ); // Constrained convergence achieved with solution XRoot==XMin
	int const iStatusOKMax( 3 ); // Constrained convergence achieved with solution XRoot==XMax
	int const iStatusOKRoundOff( 4 ); // Reached requested tolerance in X variables although Y=F(X) does not
	// satisfy unconstrained convergence check

	int const iStatusWarningNonMonotonic( 10 ); // Error because F(X) is not strictly monotonic between the
	// lower and upper points
	int const iStatusWarningSingular( 11 ); // Error because F(X) == YLower or F(X) == YUpper

	int const iMethodNone( -1 ); // No solution method (used internally only when root finder is reset)
	int const iMethodBracket( 0 ); // Bracketting mode (used internally only to bracket root)
	int const iMethodBisection( 1 ); // Step performed using bisection method (aka interval halving)
	int const iMethodFalsePosition( 2 ); // Step performed using false position method (aka regula falsi)
	int const iMethodSecant( 3 ); // Step performed using secant method
	int const iMethodBrent( 4 ); // Step performed using Brent's method
	// Names for each solution method type
	Array1D_string const SolutionMethodTypes( {-1,4}, { "No solution method", "Bracketting method", "Bisection method", "False position method", "Secant method", "Brent method" } );

	// DERIVED TYPE DEFINITIONS
	// Type declaration for the numerical controls.

	// Type declaration for iterate tracking.

	// Type declaration for the root finder solution technique.

	// INTERFACE BLOCK SPECIFICATIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:
	// na

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

} // DataRootFinder

} // EnergyPlus
