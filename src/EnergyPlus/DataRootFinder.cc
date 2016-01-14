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

} // DataRootFinder

} // EnergyPlus
