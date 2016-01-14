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
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include <RootFinder.hh>
#include <DataGlobals.hh>
#include <DataPrecisionGlobals.hh>
#include <General.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace RootFinder {
	// Module containing the derivative-free, root finder routines for smooth,
	// "mostly" monotonic functions in one variable.
	// The algorithm is guaranteed to find the root if:
	// - it is bracketed between min/max bounds
	// - there is an odd number of roots between the min/max bounds
	// Note that there is an even number of roots between the min/max bounds then it is possible
	// that the algorithm will terminate with a min or max constrained solution instead of the
	// actual root.

	// MODULE INFORMATION:
	//       AUTHOR         Dimitri Curtil (LBNL)
	//       DATE WRITTEN   February 2006
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// To encapsulate the data and algorithms required to
	// find the root of a 1-dimensional function F(X) = Y .
	// At any moment during the solution process, the following condition is satisfied:
	// - For an overall increasing function F(X):
	//  MinPoint%X <= LowerPoint%X <= CurrentPoint%X <= UpperPoint%X <= MaxPoint%X
	//  MinPoint%Y <= LowerPoint%Y <= CurrentPoint%Y <= UpperPoint%Y <= MaxPoint%Y
	// - For an overall decreasing function F(X):
	//  MinPoint%X <= LowerPoint%X <= CurrentPoint%X <= UpperPoint%X <= MaxPoint%X
	//  MinPoint%Y >= LowerPoint%Y >= CurrentPoint%Y >= UpperPoint%Y >= MaxPoint%Y
	// Requirements for function F():
	// - smooth, continuous, deterministic function
	// - root XRoot bracketed within the min and max bounds, referred to as XMin, XMax
	// - XMin and XMax must remain constant during iterations
	// - If F(X) is increasing, set action to iSlopeIncreasing
	//   This implies that if X1 < X2, then F(X1) < F(X2) must be satisfied.
	// - If F(X) is decreasing, set action to iSlopeDecreasing
	//   This implies that if X1 < X2, then F(X1) > F(X2) must be satisfied.
	// Note that the residual function is allowed to be non-monotonic between the min and max
	// points as long as the slope specification is satisfied at the min and max points.
	// Unconstrained convergence conditions:
	// - ABS(F(XRoot)) <= ATolY
	// - ABS(X2-X1)    <= TolX * (ABS(X2)+ABS(X1))/2 + ATolX
	// Constrained convergence conditions:
	// For iSlopeIncreasing action:
	//   - YMin >= 0
	//   - YMax <= 0
	// For iSlopeDecreasing action:
	//   - YMin <= 0
	//   - YMax >= 0

	// METHODOLOGY EMPLOYED:
	// Implements an hybrid solution method consisting of:
	// - bisection method (aka interval halving)
	// - false position method (aka regula falsi)
	// - secant method
	// - Brent's method (inverse quadratic interpolation)
	// with safeguards against:
	// - out-of-range error
	// - singularity (i.e., locally singular Jacobian)
	// - bad slope
	// - round-off error
	// Typical usage of the root finder, assuming that :
	// - the function is called MyFunc()
	// - the function is strictly increasing between the min and max bounds
	// - the root is bracketed between XMin=0 and XMax=1
	// - the function is defined with the prototype real FUNCTION MyFunc( X )
	// - the solution method to use is the Brent's method
	// - the absolute tolerance for Y=MyFunc(X) is ATolY=1.0E-4
	// As a safeguard the iterative process stops after 50 iterations if the root has not been
	// located within the specified tolerance yet.
	// <BEGIN CODE SNIPPET>
	// TYPE(RootFinderDataType)  :: RF
	// INTEGER                   :: IterationCount
	// REAL(r64)                 :: X, Y
	// LOGICAL                   :: IsDoneFlag
	// CALL SetupRootFinder(   &
	//   RF,                   & ! RootFinderData
	//   iSlopeIncreasing,     & ! SlopeType
	//   iMethodBrent,         & ! MethodType
	//   1.0d-6,               & ! TolX
	//   1.0d-6,               & ! ATolX
	//   1.0d-4                & ! ATolY
	// )
	// CALL InitializeRootFinder(   &
	//   RF,                   & ! RootFinderData
	//   0.0d0,                & ! XMin
	//   1.0d0                 & ! XMax
	// )
	// IterationCount = 0
	// IsDoneFlag = .FALSE.
	// RF%XCandidate = 0.1d0 ! Sets X to initial value XInit
	// DO WHILE ( .NOT.IsDoneFlag )
	//   IterationCount = IterationCount+1
	//   IF ( IterationCount>50 ) THEN
	//     WRITE(*,*) 'Error: Too many iterations..."
	//     EXIT
	//   END IF
	//   ! Evaluate function with new root candidate
	//   X = RF%XCandidate
	//   Y = MyFunc( X )
	//   ! Update root finder data with new iterate
	//   CALL IterateRootFinder( RF, X, Y, IsDoneFlag )
	// END DO
	// ! Write root finder status description to standard output
	// CALL WriteRootFinderStatus( 6, RF )
	// <END CODE SNIPPET>

	// REFERENCES:
	// "Numerical Recipes in FORTRAN", Chapter 9 "Root Finding and Nonlinear Sets of Equations", pp.340-352
	// Used for formulas, but not for code.

	// OTHER NOTES:
	// na

	// USE STATEMENTS:
	// Use statements for data only modules
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataRootFinder;
	using namespace DataGlobals;
	using General::TrimSigDigits;

	// Data
	// MODULE PARAMETER DEFINITIONS
	// na

	// DERIVED TYPE DEFINITIONS
	// na

	//MODULE VARIABLE DECLARATIONS:
	// na

	// SUBROUTINE SPECIFICATIONS FOR THE MODULE

	// For a decreasing function,  MinPoint%Y > MaxPoint%Y

	// MODULE SUBROUTINES:
	//*************************************************************************

	// Functions

	void
	SetupRootFinder(
		RootFinderDataType & RootFinderData, // Data used by root finding algorithm
		int const SlopeType, // Either iSlopeIncreasing or iSlopeDecreasing
		int const MethodType, // Any of the iMethod<name> code but iMethodNone
		Real64 const TolX, // Relative tolerance for X variables
		Real64 const ATolX, // Absolute tolerance for X variables
		Real64 const ATolY // Absolute tolerance for Y variables
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dimitri Curtil (LBNL)
		//       DATE WRITTEN   February 2006
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine loads the numerical controls for the root finder.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		// FLOW:

		// Load assumed action for underlying function F(X)
		if ( SlopeType != iSlopeIncreasing && SlopeType != iSlopeDecreasing ) {
			ShowSevereError( "SetupRootFinder: Invalid function slope specification. Valid choices are:" );
			ShowContinueError( "SetupRootFinder: iSlopeIncreasing=" + TrimSigDigits( iSlopeIncreasing ) );
			ShowContinueError( "SetupRootFinder: iSlopeDecreasing=" + TrimSigDigits( iSlopeDecreasing ) );
			ShowFatalError( "SetupRootFinder: Preceding error causes program termination." );
		}
		RootFinderData.Controls.SlopeType = SlopeType;

		// Load solution method
		if ( MethodType != iMethodBisection && MethodType != iMethodFalsePosition && MethodType != iMethodSecant && MethodType != iMethodBrent ) {

			ShowSevereError( "SetupRootFinder: Invalid solution method specification. Valid choices are:" );
			ShowContinueError( "SetupRootFinder: iMethodBisection=" + TrimSigDigits( iMethodBisection ) );
			ShowContinueError( "SetupRootFinder: iMethodFalsePosition=" + TrimSigDigits( iMethodFalsePosition ) );
			ShowContinueError( "SetupRootFinder: iMethodSecant=" + TrimSigDigits( iMethodSecant ) );
			ShowContinueError( "SetupRootFinder: iMethodBrent=" + TrimSigDigits( iMethodBrent ) );
			ShowFatalError( "SetupRootFinder: Preceding error causes program termination." );

		}
		RootFinderData.Controls.MethodType = MethodType;

		// Load relative tolerance parameter for X variables
		if ( TolX < 0.0 ) {
			ShowFatalError( "SetupRootFinder: Invalid tolerance specification for X variables. TolX >= 0" );
		}
		RootFinderData.Controls.TolX = TolX;

		// Load absolute tolerance parameter for X variables
		if ( ATolX < 0.0 ) {
			ShowFatalError( "SetupRootFinder: Invalid absolute tolerance specification for X variables. ATolX >= 0" );
		}
		RootFinderData.Controls.ATolX = ATolX;

		// Load absolute tolerance parameter for Y variables
		if ( ATolY < 0.0 ) {
			ShowFatalError( "SetupRootFinder: Invalid absolute tolerance specification for Y variables. ATolY >= 0" );
		}
		RootFinderData.Controls.ATolY = ATolY;

		// Reset internal data for root finder with fictive min and max values
		ResetRootFinder( RootFinderData, constant_zero, constant_zero );

	}

	void
	ResetRootFinder(
		RootFinderDataType & RootFinderData, // Data used by root finding algorithm
		Real64 const XMin, // Minimum X value allowed
		Real64 const XMax // Maximum X value allowed
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dimitri Curtil (LBNL)
		//       DATE WRITTEN   February 2006
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine initializes the data for the root finder.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		// FLOW:

		// Reset min point
		RootFinderData.MinPoint.X = XMin;
		RootFinderData.MinPoint.Y = 0.0;
		RootFinderData.MinPoint.DefinedFlag = false;

		// Reset max point
		RootFinderData.MaxPoint.X = XMax;
		RootFinderData.MaxPoint.Y = 0.0;
		RootFinderData.MaxPoint.DefinedFlag = false;

		// Reset lower point
		RootFinderData.LowerPoint.X = 0.0;
		RootFinderData.LowerPoint.Y = 0.0;
		RootFinderData.LowerPoint.DefinedFlag = false;

		// Reset upper point
		RootFinderData.UpperPoint.X = 0.0;
		RootFinderData.UpperPoint.Y = 0.0;
		RootFinderData.UpperPoint.DefinedFlag = false;

		// Reset previous point
		RootFinderData.CurrentPoint.X = 0.0;
		RootFinderData.CurrentPoint.Y = 0.0;
		RootFinderData.CurrentPoint.DefinedFlag = false;

		// Reset iterate history with last 3 best points
		RootFinderData.NumHistory = 0;
		for ( auto & e : RootFinderData.History ) {
			e.X = e.Y = 0.0;
			e.DefinedFlag = false;
		}

		// Reset increments over successive iterationes
		RootFinderData.Increment.X = 0.0;
		RootFinderData.Increment.Y = 0.0;
		RootFinderData.Increment.DefinedFlag = false;

		RootFinderData.XCandidate = 0.0;

		// Reset default state
		RootFinderData.StatusFlag = iStatusNone;
		RootFinderData.CurrentMethodType = iMethodNone;
		RootFinderData.ConvergenceRate = -1.0;

	}

	void
	InitializeRootFinder(
		RootFinderDataType & RootFinderData, // Data used by root finding algorithm
		Real64 const XMin, // Minimum X value allowed
		Real64 const XMax // Maximum X value allowed
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dimitri Curtil (LBNL)
		//       DATE WRITTEN   February 2006
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine initializes the min and max for the root finder before
		// finding a new root.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		Real64 SavedXCandidate;
		Real64 XMinReset;
		// FLOW:

		XMinReset = XMin;
		if ( XMin > XMax ) {
			if ( XMax == 0.0 ) {
				XMinReset = XMax;
			} else {
				ShowFatalError( "InitializeRootFinder: Invalid min/max bounds XMin=" + TrimSigDigits( XMin, 6 ) + " must be smaller than XMax=" + TrimSigDigits( XMax, 6 ) );
			}
		}

		// First save current candidate value before it is overriden in ResetRootFinder()
		SavedXCandidate = RootFinderData.XCandidate;

		// Reset internal data for root finder with actual min and max values
		// NOTE: This resets the value of RootFinderData%XCandidate to zero
		ResetRootFinder( RootFinderData, XMinReset, XMax );

		// Enforce min/max constraints on previous candidate if available
		// NOTE: If XMin == XMax then this forces the value of XCandidateto the desired solution
		RootFinderData.XCandidate = min( RootFinderData.MaxPoint.X, max( SavedXCandidate, RootFinderData.MinPoint.X ) );

	}

	void
	IterateRootFinder(
		RootFinderDataType & RootFinderData, // Data used by root finding algorithm
		Real64 const X, // X value of current iterate
		Real64 const Y, // Y value of current iterate
		Optional_bool IsDoneFlag // If TRUE indicates that the iteration should be stopped
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dimitri Curtil (LBNL)
		//       DATE WRITTEN   March 2006
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine is the workhorse of the root finder framework.
		// It should be invoked with every new candidate (X,Y) until
		// convergence is achieved or abnormal termination is detected.
		// The subroutine performs the following tasks:
		// - it checks for convergence
		// - it updates the internal data with the current iterate (X,Y)
		// - it computes a new root candidate if not converged yet.
		// Sets IsDoneFlag to FALSE when iteration should continue with the new candidate value.
		// Sets IsDoneFlag to TRUE  when iteration should be stopped because convergence has been achieved
		// or because of a fatal error.
		// Note that only upon normal termination (iStatusOK<...> codes)
		// will the XCandidate value contain the root.
		// If the root has not been located yet the XCandidate value contains
		// the next candidate root to try.
		// Status                          IsDoneFlag          XCandidate
		// ========================================================================
		// iStatusErrorRange               TRUE                na
		// iStatusErrorSingular            TRUE                na
		// iStatusErrorSlope               TRUE                na
		// iStatusErrorBracket             TRUE                na
		// ------------------------------------------------------------------------
		// iStatusOKMin                    TRUE                MinPoint%X
		// iStatusOKMax                    TRUE                MaxPoint%X
		// iStatusOK                       TRUE                X
		// iStatusOKRoundOff               TRUE                X
		// ------------------------------------------------------------------------
		// iStatusNone                     FALSE               AdvanceRootFinder()
		// iStatusWarningNonMonotonic      FALSE               AdvanceRootFinder()
		// iStatusWarningSingular          FALSE               AdvanceRootFinder()

		// METHODOLOGY EMPLOYED:
		// The methodology reflects the same approach implemented in the subroutine CalcSimpleController()
		// whereby the iteration was exited by checking the following conditions in this specified
		// sequence:
		//   1. Check for singular function so that YMin /= YMax
		//   2. Check for slope condition for the residual function
		//      - increasing: YMin < YMax
		//      - decreasing: YMin > YMax
		//   3. Check for min constraint
		//      - increasing: YMin <= 0
		//      - decreasing: YMin >= 0
		//   4. Check for max constraint
		//      - increasing: YMax > 0
		//      - decreasing: YMax < 0
		//   5. Check unconstrained convergence
		// Note that the slope condition was not explicitly checked in the original implementation
		// in CalcSimpleController().
		// Finally, we also check the X increments between successive iterations to detect possible
		// cases whereby the allowed precision in the X space limits the precision attainable in
		// the Y space. This check is implemented in:
		// - CheckIncrementRoundOff()
		// - CheckBracketRoundOff()

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		// FLOW:

		// Reset status flag
		RootFinderData.StatusFlag = iStatusNone;

		// Check that MinPoint%X <= X <= MaxPoint%X
		if ( ! CheckMinMaxRange( RootFinderData, X ) ) {
			RootFinderData.StatusFlag = iStatusErrorRange;

			// Fatal error: No need to continue iterating
			IsDoneFlag = true;
			return;
		}

		// Update min/max support points with current iterate
		UpdateMinMax( RootFinderData, X, Y );

		//----------------------------------------------------------------------------
		// Check "global" singularity and bad slope conditions between min and
		// max points
		//----------------------------------------------------------------------------

		// NOTE: Performed before checking min and max constraints to mimic original implementation
		//       in ManagerControllers()
		if ( RootFinderData.MinPoint.DefinedFlag && RootFinderData.MaxPoint.DefinedFlag ) {

			// Check that min and max points are distinct
			if ( RootFinderData.MinPoint.X == RootFinderData.MaxPoint.X ) {
				RootFinderData.StatusFlag = iStatusOKMin;
				RootFinderData.XCandidate = RootFinderData.MinPoint.X;

				// Solution found: No need to continue iterating
				IsDoneFlag = true;
				return;
			}

			if ( RootFinderData.MinPoint.DefinedFlag ) {
				if ( CheckMinConstraint( RootFinderData ) ) {
					RootFinderData.StatusFlag = iStatusOKMin;
					RootFinderData.XCandidate = RootFinderData.MinPoint.X;

					// Solution found: No need to continue iterating
					IsDoneFlag = true;
					return;
				}
			}

			// Check singularity condition between min and max points
			if ( ! CheckNonSingularity( RootFinderData ) ) {
				RootFinderData.StatusFlag = iStatusErrorSingular;

				// Fatal error: No need to continue iterating
				IsDoneFlag = true;
				return;
			}

			// Check slope condition between min and max points
			if ( ! CheckSlope( RootFinderData ) ) {
				RootFinderData.StatusFlag = iStatusErrorSlope;

				// Fatal error: No need to continue iterating
				IsDoneFlag = true;
				return;
			}
		}

		//----------------------------------------------------------------------------
		// Check that F(X) is not min or max constrained
		//----------------------------------------------------------------------------

		// Check min constraint before max constraint to mimic original implementation
		// in ManagerControllers()
		if ( RootFinderData.MinPoint.DefinedFlag ) {
			if ( CheckMinConstraint( RootFinderData ) ) {
				RootFinderData.StatusFlag = iStatusOKMin;
				RootFinderData.XCandidate = RootFinderData.MinPoint.X;

				// Solution found: No need to continue iterating
				IsDoneFlag = true;
				return;
			}
		}

		// Min point should always be evaluated first to ensure that we return with the min
		// consrained solution even in cases where the residual function has inconsistent slope.
		// TODO: Force to evaluate min point before exiting with max constrained solution
		//       in order to be able to detect singularity and bad slope conditions.
		if ( RootFinderData.MaxPoint.DefinedFlag ) {
			if ( CheckMaxConstraint( RootFinderData ) ) {

				RootFinderData.StatusFlag = iStatusOKMax;
				RootFinderData.XCandidate = RootFinderData.MaxPoint.X;

				// Solution found: No need to continue iterating
				IsDoneFlag = true;
				return;
			}
		}

		//----------------------------------------------------------------------------
		// Check convergence of current iterate
		//----------------------------------------------------------------------------

		// Check unconstrained convergence after we are sure that the candidate X value lies
		// within the allowed min/max range
		if ( CheckRootFinderConvergence( RootFinderData, Y ) ) {
			RootFinderData.StatusFlag = iStatusOK;
			RootFinderData.XCandidate = X;

			// Update root finder internal data with current iterate (X,Y)
			UpdateRootFinder( RootFinderData, X, Y );

			// Solution found: No need to continue iterating
			IsDoneFlag = true;
			return;
		}

		// Check last as this was not done in the original implementation
		// Essentially we stop the iteration if:
		// - the increment beween successive iterates is smaller than the user-specified
		//   tolerance for the X variables.
		// - the distance between the lower and upper bounds is smaller than the user-specified
		//   tolerance for the X variables. (USING brackets from previous iteration)
		// BUG: Relaxed check to avoid detecting round-off in case 2 successive iterates are the same!
		//IF ( CheckIncrementRoundOff( RootFinderData, X ) ) THEN
		//  RootFinderData%StatusFlag = iStatusOKRoundOff
		//  RETURN
		//END IF
		if ( CheckBracketRoundOff( RootFinderData ) ) {
			RootFinderData.StatusFlag = iStatusOKRoundOff;
			RootFinderData.XCandidate = X;

			// Update root finder internal data with current iterate (X,Y)
			UpdateRootFinder( RootFinderData, X, Y );

			// Solution found: No need to continue iterating
			IsDoneFlag = true;
			return;
		}

		//----------------------------------------------------------------------------
		// If the current iterate lies within the current lower and upper brackets,
		// proceed with normal processing to identify the next root candidate:
		// - update lower/upper bracket with current iterate
		// - update history
		// - update increments across successive iterations
		// - update current point
		// - compute next candidate (see AdvanceRootFinder() ).
		//----------------------------------------------------------------------------

		// Check that current iterate is within the current lower and upper points
		if ( ! CheckLowerUpperBracket( RootFinderData, X ) ) {
			RootFinderData.StatusFlag = iStatusErrorBracket;

			// Fatal error: No need to continue iterating
			IsDoneFlag = true;
			return;
		}

		// Update root finder internal data with current iterate (X,Y)
		UpdateRootFinder( RootFinderData, X, Y );

		// Compute new root candidate and store value in in RootFinderData%XCandidate
		// - First attempt to bracket root within lower and upper points
		// - Then use whatever requested solution method in SetupRootFinder() to
		//   compute the next candidate.
		AdvanceRootFinder( RootFinderData );

		// Indicates that we should continue iterating with new candidate
		IsDoneFlag = false;

	}

	int
	CheckInternalConsistency( RootFinderDataType const & RootFinderData ) // Data used by root finding algorithm
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Dimitri Curtil (LBNL)
		//       DATE WRITTEN   March 2006
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function checks whether the lower and upper points (if defined)
		// determine a consistent interval bracketting the root.
		// Returns the status code accordingly.
		// This function does not modify the argument RooFinderData.
		// Only used internally for debugging.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		int CheckInternalConsistency;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		// FLOW:

		// Default initialization
		CheckInternalConsistency = iStatusNone;

		// Internal consistency check involving both support points
		if ( RootFinderData.LowerPoint.DefinedFlag && RootFinderData.UpperPoint.DefinedFlag ) {

			// Check that the existing lower and upper points do bracket the root
			if ( RootFinderData.LowerPoint.X > RootFinderData.UpperPoint.X ) {
				CheckInternalConsistency = iStatusErrorRange;
				return CheckInternalConsistency;
			}

			// Check for non-monotonicity between the existing lower and upper points
			{ auto const SELECT_CASE_var( RootFinderData.Controls.SlopeType );
			if ( SELECT_CASE_var == iSlopeIncreasing ) {
				// Y-value of lower point must be strictly smaller than Y-value of upper point
				if ( RootFinderData.LowerPoint.Y > RootFinderData.UpperPoint.Y ) {
					CheckInternalConsistency = iStatusWarningNonMonotonic;
					return CheckInternalConsistency;
				}

			} else if ( SELECT_CASE_var == iSlopeDecreasing ) {
				// Y-value of lower point must be strictly larger than Y-value of upper point
				if ( RootFinderData.LowerPoint.Y < RootFinderData.UpperPoint.Y ) {
					CheckInternalConsistency = iStatusWarningNonMonotonic;
					return CheckInternalConsistency;
				}

			} else {
				// Should never happen
				ShowSevereError( "CheckInternalConsistency: Invalid function slope specification. Valid choices are:" );
				ShowContinueError( "CheckInternalConsistency: iSlopeIncreasing=" + TrimSigDigits( iSlopeIncreasing ) );
				ShowContinueError( "CheckInternalConsistency: iSlopeDecreasing=" + TrimSigDigits( iSlopeDecreasing ) );
				ShowFatalError( "CheckInternalConsistency: Preceding error causes program termination." );
			}}

			// Check for in singularity with respect to the existing lower and upper points
			// Only check if the lower and upper points are distinct!
			if ( RootFinderData.UpperPoint.X > RootFinderData.LowerPoint.X ) {
				if ( RootFinderData.UpperPoint.Y == RootFinderData.LowerPoint.Y ) {
					CheckInternalConsistency = iStatusErrorSingular;
					return CheckInternalConsistency;
				}
			}

		}

		// Check min constraint for min point if already defined
		if ( RootFinderData.MinPoint.DefinedFlag ) {
			{ auto const SELECT_CASE_var( RootFinderData.Controls.SlopeType );
			if ( SELECT_CASE_var == iSlopeIncreasing ) {
				if ( RootFinderData.MinPoint.Y >= 0.0 ) {
					CheckInternalConsistency = iStatusOKMin;
					return CheckInternalConsistency;
				}

			} else if ( SELECT_CASE_var == iSlopeDecreasing ) {
				if ( RootFinderData.MinPoint.Y <= 0.0 ) {
					CheckInternalConsistency = iStatusOKMin;
					return CheckInternalConsistency;
				}

			} else {
				// Should never happen
				ShowSevereError( "CheckInternalConsistency: Invalid function slope specification. Valid choices are:" );
				ShowContinueError( "CheckInternalConsistency: iSlopeIncreasing=" + TrimSigDigits( iSlopeIncreasing ) );
				ShowContinueError( "CheckInternalConsistency: iSlopeDecreasing=" + TrimSigDigits( iSlopeDecreasing ) );
				ShowFatalError( "CheckInternalConsistency: Preceding error causes program termination." );
			}}
		}

		// Check max constraint for max point if already defined
		if ( RootFinderData.MaxPoint.DefinedFlag ) {
			{ auto const SELECT_CASE_var( RootFinderData.Controls.SlopeType );
			if ( SELECT_CASE_var == iSlopeIncreasing ) {
				if ( RootFinderData.MaxPoint.Y <= 0.0 ) {
					CheckInternalConsistency = iStatusOKMax;
					return CheckInternalConsistency;
				}

			} else if ( SELECT_CASE_var == iSlopeDecreasing ) {
				if ( RootFinderData.MaxPoint.Y >= 0.0 ) {
					CheckInternalConsistency = iStatusOKMax;
					return CheckInternalConsistency;
				}

			} else {
				// Should never happen
				ShowSevereError( "CheckInternalConsistency: Invalid function slope specification. Valid choices are:" );
				ShowContinueError( "CheckInternalConsistency: iSlopeIncreasing=" + TrimSigDigits( iSlopeIncreasing ) );
				ShowContinueError( "CheckInternalConsistency: iSlopeDecreasing=" + TrimSigDigits( iSlopeDecreasing ) );
				ShowFatalError( "CheckInternalConsistency: Preceding error causes program termination." );
			}}
		}

		return CheckInternalConsistency;
	}

	bool
	CheckRootFinderCandidate(
		RootFinderDataType const & RootFinderData, // Data used by root finding algorithm
		Real64 const X // X value for current iterate
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Dimitri Curtil (LBNL)
		//       DATE WRITTEN   February 2006
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function checks whether the root candidate X lies within the specified
		// min/max limits as well as within the current lower and upper brackets (if defined).
		// Returns TRUE if X value is a valid root candidate.
		// Returns FALSE otherwise.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		bool CheckRootFinderCandidate;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		// FLOW:

		if ( CheckMinMaxRange( RootFinderData, X ) && CheckLowerUpperBracket( RootFinderData, X ) ) {
			CheckRootFinderCandidate = true;
		} else {
			CheckRootFinderCandidate = false;
		}

		return CheckRootFinderCandidate;
	}

	bool
	CheckMinMaxRange(
		RootFinderDataType const & RootFinderData, // Data used by root finding algorithm
		Real64 const X // X value for current iterate
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Dimitri Curtil (LBNL)
		//       DATE WRITTEN   February 2006
		//       MODIFIED       Brent Griffith (NREL) added DefinedFlag traps
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function checks whether the current iterate X lies within the specified min/max limits
		// or not.
		// Returns TRUE if current iterate satisfies min/max constraints.
		// Returns FALSE if current iterate is out-of-range.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		bool CheckMinMaxRange;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		// FLOW:
		if ( RootFinderData.MinPoint.DefinedFlag ) { //DSU3 testing
			if ( X < RootFinderData.MinPoint.X ) {
				CheckMinMaxRange = false;
				return CheckMinMaxRange;
			}
		}

		if ( RootFinderData.MaxPoint.DefinedFlag ) { //DSU3 testing
			if ( X > RootFinderData.MaxPoint.X ) {
				CheckMinMaxRange = false;
				return CheckMinMaxRange;
			}
		}

		CheckMinMaxRange = true;

		return CheckMinMaxRange;
	}

	bool
	CheckLowerUpperBracket(
		RootFinderDataType const & RootFinderData, // Data used by root finding algorithm
		Real64 const X // X value for current iterate
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Dimitri Curtil (LBNL)
		//       DATE WRITTEN   February 2006
		//       MODIFIED       Brent Griffith, March 2010, changed to LowerPoint%X <= X <= UpperPoint%X
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function checks whether the current iterate X lies within the current lower
		// and upper points or not (if defined):
		//   LowerPoint%X < X < UpperPoint%X
		// Returns TRUE if current iterate lies within the lower/upper bracket.
		// Returns FALSE otherwise.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		bool CheckLowerUpperBracket;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		// FLOW:

		if ( RootFinderData.LowerPoint.DefinedFlag ) {
			if ( X < RootFinderData.LowerPoint.X ) { //DSU3 test with < instead of <=
				CheckLowerUpperBracket = false;
				return CheckLowerUpperBracket;
			}
		}

		if ( RootFinderData.UpperPoint.DefinedFlag ) {
			if ( X > RootFinderData.UpperPoint.X ) { //DSU3 test with > instead of >=
				CheckLowerUpperBracket = false;
				return CheckLowerUpperBracket;
			}
		}

		CheckLowerUpperBracket = true;

		return CheckLowerUpperBracket;
	}

	bool
	CheckSlope( RootFinderDataType const & RootFinderData ) // Data used by root finding algorithm
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Dimitri Curtil (LBNL)
		//       DATE WRITTEN   February 2006
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function checks whether the current iterate (X,Y) satisfies the slope
		// requirement between the min and max points.
		// Returns FALSE if the slope requirement is NOT satisfied.
		// Returns TRUE if the slope requirement is satisfied.
		// PRECONDITION:
		// - Function assumes that both the min and max points are defined.
		// POSTCONDITION:
		// - RootFinderData is NOT changed by this function.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		bool CheckSlope;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		// FLOW:

		// Check that the slope requirement is respected at the min and max points
		// Note that the singularity check takes care of RootFinderData%MinPoint%Y == RootFinderData%MaxPoint%Y
		// therefore we use strict comparison operators < and >.
		{ auto const SELECT_CASE_var( RootFinderData.Controls.SlopeType );
		if ( SELECT_CASE_var == iSlopeIncreasing ) {
			if ( RootFinderData.MinPoint.Y < RootFinderData.MaxPoint.Y ) {
				CheckSlope = true;
				return CheckSlope;
			}

		} else if ( SELECT_CASE_var == iSlopeDecreasing ) {
			if ( RootFinderData.MinPoint.Y > RootFinderData.MaxPoint.Y ) {
				CheckSlope = true;
				return CheckSlope;
			}

		} else {
			// Should never happen
			ShowSevereError( "CheckSlope: Invalid function slope specification. Valid choices are:" );
			ShowContinueError( "CheckSlope: iSlopeIncreasing=" + TrimSigDigits( iSlopeIncreasing ) );
			ShowContinueError( "CheckSlope: iSlopeDecreasing=" + TrimSigDigits( iSlopeDecreasing ) );
			ShowFatalError( "CheckSlope: Preceding error causes program termination." );

		}}

		CheckSlope = false;

		return CheckSlope;
	}

	bool
	CheckNonSingularity( RootFinderDataType const & RootFinderData ) // Data used by root finding algorithm
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Dimitri Curtil (LBNL)
		//       DATE WRITTEN   February 2006
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function checks whether the min and max points define a locally, singular
		// equation system. In a 1-dimensional system, "singularity" is detected if the
		// min and max points have the same y-values, thereby producing a zero slope
		// across the min/max range.
		// Returns TRUE if the function satisfies the non-singularity condition.
		// Returns FALSE otherwise (i.e., F(X) essentially displays a zero slope
		// between the min and max points) .
		// PRECONDITION:
		// - Function assumes that both the min and max points are defined.
		// POSTCONDITION:
		// - RootFinderData is NOT changed by this function.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		bool CheckNonSingularity;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// Safety factor used to detect a singular residual function between the min and max
		// points.
		// NOTE: Requesting exactly the same value is obtained by setting SafetyFactor = 0.0
		Real64 const SafetyFactor( 0.1 );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 DeltaY; // Difference between min and max Y-values
		Real64 ATolY; // Absolute tolerance used to detected equal min and max Y-values

		// FLOW:

		// Added this check based on an absolute tolerance test for y values to avoid incorrectly detecting
		// functions with bad slope due to numerical noise.
		// Typically, this takes care of situations where the controlled equipment in ManageControllers()
		// would be misdiagnosed as displaying the "wrong slope" instead of being treated as "singular"
		// (i.e. in inactive mode).
		DeltaY = std::abs( RootFinderData.MinPoint.Y - RootFinderData.MaxPoint.Y );
		ATolY = SafetyFactor * RootFinderData.Controls.ATolY;

		if ( std::abs( DeltaY ) <= ATolY ) {
			CheckNonSingularity = false;
		} else {
			CheckNonSingularity = true;
		}

		return CheckNonSingularity;
	}

	bool
	CheckMinConstraint( RootFinderDataType const & RootFinderData ) // Data used by root finding algorithm
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Dimitri Curtil (LBNL)
		//       DATE WRITTEN   February 2006
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function checks whether the min point satisfies the min constraint
		// condition or not.
		// PRECONDITION:
		// - Function assumes that the min point is defined.
		// POSTCONDITION:
		// - RootFinderData is NOT changed by this function.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		bool CheckMinConstraint;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		// FLOW:

		{ auto const SELECT_CASE_var( RootFinderData.Controls.SlopeType );
		if ( SELECT_CASE_var == iSlopeIncreasing ) {
			if ( RootFinderData.MinPoint.Y >= 0.0 ) {
				CheckMinConstraint = true;
				return CheckMinConstraint;
			}

		} else if ( SELECT_CASE_var == iSlopeDecreasing ) {
			if ( RootFinderData.MinPoint.Y <= 0.0 ) {
				CheckMinConstraint = true;
				return CheckMinConstraint;
			}

		} else {
			// Should never happen
			ShowSevereError( "CheckMinConstraint: Invalid function slope specification. Valid choices are:" );
			ShowContinueError( "CheckMinConstraint: iSlopeIncreasing=" + TrimSigDigits( iSlopeIncreasing ) );
			ShowContinueError( "CheckMinConstraint: iSlopeDecreasing=" + TrimSigDigits( iSlopeDecreasing ) );
			ShowFatalError( "CheckMinConstraint: Preceding error causes program termination." );
		}}

		CheckMinConstraint = false;

		return CheckMinConstraint;
	}

	bool
	CheckMaxConstraint( RootFinderDataType const & RootFinderData ) // Data used by root finding algorithm
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Dimitri Curtil (LBNL)
		//       DATE WRITTEN   February 2006
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function checks whether the max point satisfies the max constraint
		// condition or not.
		// PRECONDITION:
		// - Function assumes that the max point is defined.
		// POSTCONDITION:
		// - RootFinderData is NOT changed by this function.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		bool CheckMaxConstraint;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		// FLOW:

		// Check for max constrained convergence with respect to the new iterate (X,Y)
		{ auto const SELECT_CASE_var( RootFinderData.Controls.SlopeType );
		if ( SELECT_CASE_var == iSlopeIncreasing ) {
			if ( RootFinderData.MaxPoint.Y <= 0.0 ) {
				CheckMaxConstraint = true;
				return CheckMaxConstraint;
			}

		} else if ( SELECT_CASE_var == iSlopeDecreasing ) {
			if ( RootFinderData.MaxPoint.Y >= 0.0 ) {
				CheckMaxConstraint = true;
				return CheckMaxConstraint;
			}

		} else {
			// Should never happen
			ShowSevereError( "CheckMaxConstraint: Invalid function slope specification. Valid choices are:" );
			ShowContinueError( "CheckMaxConstraint: iSlopeIncreasing=" + TrimSigDigits( iSlopeIncreasing ) );
			ShowContinueError( "CheckMaxConstraint: iSlopeDecreasing=" + TrimSigDigits( iSlopeDecreasing ) );
			ShowFatalError( "CheckMaxConstraint: Preceding error causes program termination." );
		}}

		CheckMaxConstraint = false;

		return CheckMaxConstraint;
	}

	bool
	CheckRootFinderConvergence(
		RootFinderDataType const & RootFinderData, // Data used by root finding algorithm
		Real64 const Y // Y value for current iterate
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Dimitri Curtil (LBNL)
		//       DATE WRITTEN   February 2006
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function checks whether the current iterate (X,Y) satisfies the
		// unconstrained convergence criterion or not.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		bool CheckRootFinderConvergence;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		// FLOW:

		// Check for unconstrained convergence
		if ( std::abs( Y ) <= RootFinderData.Controls.ATolY ) {
			CheckRootFinderConvergence = true;
			return CheckRootFinderConvergence;
		}

		CheckRootFinderConvergence = false;

		return CheckRootFinderConvergence;
	}

	bool
	CheckIncrementRoundOff(
		RootFinderDataType const & RootFinderData, // Data used by root finding algorithm
		Real64 const X // X value for current iterate
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Dimitri Curtil (LBNL)
		//       DATE WRITTEN   February 2006
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function checks whether the current iterate X satisfies the
		// round-off criterion or not.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		bool CheckIncrementRoundOff;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 DeltaX; // Increment in X since last iterate
		Real64 TolX; // Tolerance to satisfy for X increment
		// FLOW:

		CheckIncrementRoundOff = false;
		// Check for round-off error in X increments since last iterate
		if ( RootFinderData.CurrentPoint.DefinedFlag ) {
			// TODO: Use typical value for X averaged over successive iterations
			TolX = RootFinderData.Controls.TolX * std::abs( X ) + RootFinderData.Controls.ATolX;
			DeltaX = X - RootFinderData.CurrentPoint.Y;

			if ( std::abs( DeltaX ) <= std::abs( TolX ) ) {
				CheckIncrementRoundOff = true;
				return CheckIncrementRoundOff;
			}
		}

		return CheckIncrementRoundOff;
	}

	bool
	CheckBracketRoundOff( RootFinderDataType const & RootFinderData ) // Data used by root finding algorithm
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Dimitri Curtil (LBNL)
		//       DATE WRITTEN   February 2006
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function checks whether the current lower and upper brackets satisfies
		// the round-off criterion or not.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		bool CheckBracketRoundOff;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 DeltaUL; // Distance between lower and upper points
		Real64 TypUL; // Typical value for values lying within lower/upper interval
		Real64 TolUL; // Tolerance to satisfy for lower-upper distance
		// FLOW:

		// Check for round-off error in Lower/Upper interval
		if ( RootFinderData.LowerPoint.DefinedFlag && RootFinderData.UpperPoint.DefinedFlag ) {
			DeltaUL = RootFinderData.UpperPoint.X - RootFinderData.LowerPoint.X;
			TypUL = ( std::abs( RootFinderData.UpperPoint.X ) + std::abs( RootFinderData.LowerPoint.X ) ) / 2.0;
			TolUL = RootFinderData.Controls.TolX * std::abs( TypUL ) + RootFinderData.Controls.ATolX;

			// Halve tolerance to reflect the fact that solution can be anywhere between the lower and upper points.
			if ( std::abs( DeltaUL ) <= 0.5 * std::abs( TolUL ) ) {
				CheckBracketRoundOff = true;
				return CheckBracketRoundOff;
			}
		}

		CheckBracketRoundOff = false;

		return CheckBracketRoundOff;
	}

	void
	UpdateMinMax(
		RootFinderDataType & RootFinderData, // Data used by root finding algorithm
		Real64 const X, // X value for current iterate
		Real64 const Y // Y value for current iterate, F(X)=Y
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dimitri Curtil (LBNL)
		//       DATE WRITTEN   February 2006
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine updates the min/max support points in the root finder data.

		// METHODOLOGY EMPLOYED:
		// PRECONDITION:
		// na
		// POSTCONDITION:
		// - RootFinderData%MinPoint possibly updated
		// - RootFinderData%MaxPoint possibly updated

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		// FLOW:

		// Update min support point
		if ( X == RootFinderData.MinPoint.X ) {
			RootFinderData.MinPoint.Y = Y;
			RootFinderData.MinPoint.DefinedFlag = true;
		}

		// Update max support point
		if ( X == RootFinderData.MaxPoint.X ) {
			RootFinderData.MaxPoint.Y = Y;
			RootFinderData.MaxPoint.DefinedFlag = true;
		}

	}

	void
	UpdateBracket(
		RootFinderDataType & RootFinderData, // Data used by root finding algorithm
		Real64 const X, // X value for current iterate
		Real64 const Y // Y value for current iterate, F(X)=Y
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dimitri Curtil (LBNL)
		//       DATE WRITTEN   March 2006
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine updates the lower/upper support points in the root finder data
		// with the current iterate (X,Y).

		// METHODOLOGY EMPLOYED:
		// PRECONDITION:
		// - The current iterate (X,Y) must satisfy:
		//   MinPoint%X <= LowerPoint%X < X < UpperPoint%X <= MaxPoint%X
		// - RootFinderData%StatusFlag == iStatusNone
		// POSTCONDITION:
		// - RootFinderData%LowerPoint possibly updated
		// - RootFinderData%UpperPoint possibly updated
		// - RootFinderData%StatusFlag possibly updated with:
		//   - iStatusWarningNonMonotonic
		//   - iStatusWarningSingular

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		// FLOW:

		{ auto const SELECT_CASE_var( RootFinderData.Controls.SlopeType );

		if ( SELECT_CASE_var == iSlopeIncreasing ) {
			// Update lower point
			if ( Y <= 0.0 ) {
				if ( ! RootFinderData.LowerPoint.DefinedFlag ) {
					RootFinderData.LowerPoint.DefinedFlag = true;
					RootFinderData.LowerPoint.X = X;
					RootFinderData.LowerPoint.Y = Y;
				} else {
					if ( X >= RootFinderData.LowerPoint.X ) {
						if ( Y == RootFinderData.LowerPoint.Y ) {
							RootFinderData.StatusFlag = iStatusWarningSingular;
						} else if ( Y < RootFinderData.LowerPoint.Y ) {
							RootFinderData.StatusFlag = iStatusWarningNonMonotonic;
						}
						// Update lower point with current iterate
						RootFinderData.LowerPoint.X = X;
						RootFinderData.LowerPoint.Y = Y;
					} else {
						// Should never happen if CheckLowerUpperBracket() is called before
						ShowSevereError( "UpdateBracket: Current iterate is smaller than the lower bracket." );
						ShowContinueError( "UpdateBracket: X=" + TrimSigDigits( X, 15 ) + ", Y=" + TrimSigDigits( Y, 15 ) );
						ShowContinueError( "UpdateBracket: XLower=" + TrimSigDigits( RootFinderData.LowerPoint.X, 15 ) + ", YLower=" + TrimSigDigits( RootFinderData.LowerPoint.Y, 15 ) );
						ShowFatalError( "UpdateBracket: Preceding error causes program termination." );
					}
				}

				// Update upper point
			} else {
				if ( ! RootFinderData.UpperPoint.DefinedFlag ) {
					RootFinderData.UpperPoint.DefinedFlag = true;
					RootFinderData.UpperPoint.X = X;
					RootFinderData.UpperPoint.Y = Y;
				} else {
					if ( X <= RootFinderData.UpperPoint.X ) {
						if ( Y == RootFinderData.UpperPoint.Y ) {
							RootFinderData.StatusFlag = iStatusWarningSingular;
						} else if ( Y > RootFinderData.UpperPoint.Y ) {
							RootFinderData.StatusFlag = iStatusWarningNonMonotonic;
						}
						// Update upper point with current iterate
						RootFinderData.UpperPoint.X = X;
						RootFinderData.UpperPoint.Y = Y;
					} else {
						// Should never happen if CheckLowerUpperBracket() is called before
						ShowSevereError( "UpdateBracket: Current iterate is greater than the upper bracket." );
						ShowContinueError( "UpdateBracket: X=" + TrimSigDigits( X, 15 ) + ", Y=" + TrimSigDigits( Y, 15 ) );
						ShowContinueError( "UpdateBracket: XUpper=" + TrimSigDigits( RootFinderData.UpperPoint.X, 15 ) + ", YUpper=" + TrimSigDigits( RootFinderData.UpperPoint.Y, 15 ) );
						ShowFatalError( "UpdateBracket: Preceding error causes program termination." );
					}
				}
			}

			// Monotone, decreasing function
		} else if ( SELECT_CASE_var == iSlopeDecreasing ) {
			// Update lower point
			if ( Y >= 0.0 ) {
				if ( ! RootFinderData.LowerPoint.DefinedFlag ) {
					RootFinderData.LowerPoint.DefinedFlag = true;
					RootFinderData.LowerPoint.X = X;
					RootFinderData.LowerPoint.Y = Y;
				} else {
					if ( X >= RootFinderData.LowerPoint.X ) {
						if ( Y == RootFinderData.LowerPoint.Y ) {
							RootFinderData.StatusFlag = iStatusWarningSingular;
						} else if ( Y > RootFinderData.LowerPoint.Y ) {
							RootFinderData.StatusFlag = iStatusWarningNonMonotonic;
						}
						// Update lower point with current iterate
						RootFinderData.LowerPoint.X = X;
						RootFinderData.LowerPoint.Y = Y;
					} else {
						// Should never happen if CheckLowerUpperBracket() is called before
						ShowSevereError( "UpdateBracket: Current iterate is smaller than the lower bracket." );
						ShowContinueError( "UpdateBracket: X=" + TrimSigDigits( X, 15 ) + ", Y=" + TrimSigDigits( Y, 15 ) );
						ShowContinueError( "UpdateBracket: XLower=" + TrimSigDigits( RootFinderData.LowerPoint.X, 15 ) + ", YLower=" + TrimSigDigits( RootFinderData.LowerPoint.Y, 15 ) );
						ShowFatalError( "UpdateBracket: Preceding error causes program termination." );
					}
				}

				// Update upper point
			} else {
				if ( ! RootFinderData.UpperPoint.DefinedFlag ) {
					RootFinderData.UpperPoint.DefinedFlag = true;
					RootFinderData.UpperPoint.X = X;
					RootFinderData.UpperPoint.Y = Y;
				} else {
					if ( X <= RootFinderData.UpperPoint.X ) {
						if ( Y == RootFinderData.UpperPoint.Y ) {
							RootFinderData.StatusFlag = iStatusWarningSingular;
						} else if ( Y < RootFinderData.UpperPoint.Y ) {
							RootFinderData.StatusFlag = iStatusWarningNonMonotonic;
						}
						// Update upper point with current iterate
						RootFinderData.UpperPoint.X = X;
						RootFinderData.UpperPoint.Y = Y;
					} else {
						// Should never happen if CheckLowerUpperBracket() is called before
						ShowSevereError( "UpdateBracket: Current iterate is greater than the upper bracket." );
						ShowContinueError( "UpdateBracket: X=" + TrimSigDigits( X, 15 ) + ", Y=" + TrimSigDigits( Y, 15 ) );
						ShowContinueError( "UpdateBracket: XUpper=" + TrimSigDigits( RootFinderData.UpperPoint.X, 15 ) + ", YUpper=" + TrimSigDigits( RootFinderData.UpperPoint.Y, 15 ) );
						ShowFatalError( "UpdateBracket: Preceding error causes program termination." );
					}
				}
			}

		} else {
			// Should never happen
			ShowSevereError( "UpdateBracket: Invalid function slope specification. Valid choices are:" );
			ShowContinueError( "UpdateBracket: iSlopeIncreasing=" + TrimSigDigits( iSlopeIncreasing ) );
			ShowContinueError( "UpdateBracket: iSlopeDecreasing=" + TrimSigDigits( iSlopeDecreasing ) );
			ShowFatalError( "UpdateBracket: Preceding error causes program termination." );

		}}

	}

	void
	UpdateHistory(
		RootFinderDataType & RootFinderData, // Data used by root finding algorithm
		Real64 const X, // X value for current iterate
		Real64 const Y // Y value for current iterate, F(X)=Y
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dimitri Curtil (LBNL)
		//       DATE WRITTEN   February 2006
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine updates the min/max support points in the root finder data.

		// METHODOLOGY EMPLOYED:
		// PRECONDITION:
		// - The current iterate (X,Y) must be a valid iterate:
		//   MinPoint%X <= LowerPoint%X < X < UpperPoint%X <= MaxPoint%X
		// POSTCONDITION:
		// - RootFinderData%History(:) updated with last 3 best iterates

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int NumHistory;

		// FLOW:

		// Update history with best iterates so that:
		//   ABS(History(1)%Y) <= ABS(History(2)%Y) <= ABS(History(3)%Y)
		// Note that the history points are sorted so that
		//   SIGN(History(1)%Y) = -SIGN(History(3)%Y)
		// to ensure that the history points bracket the candidate root.
		for ( auto & e : RootFinderData.History ) {
			e.X = e.Y = 0.0;
			e.DefinedFlag = false;
		}

		NumHistory = 0;
		if ( RootFinderData.LowerPoint.DefinedFlag ) {
			++NumHistory;
			RootFinderData.History( NumHistory ).DefinedFlag = RootFinderData.LowerPoint.DefinedFlag;
			RootFinderData.History( NumHistory ).X = RootFinderData.LowerPoint.X;
			RootFinderData.History( NumHistory ).Y = RootFinderData.LowerPoint.Y;
		}
		if ( RootFinderData.UpperPoint.DefinedFlag ) {
			++NumHistory;
			RootFinderData.History( NumHistory ).DefinedFlag = RootFinderData.UpperPoint.DefinedFlag;
			RootFinderData.History( NumHistory ).X = RootFinderData.UpperPoint.X;
			RootFinderData.History( NumHistory ).Y = RootFinderData.UpperPoint.Y;
		}
		++NumHistory;
		RootFinderData.History( NumHistory ).DefinedFlag = true;
		RootFinderData.History( NumHistory ).X = X;
		RootFinderData.History( NumHistory ).Y = Y;

		RootFinderData.NumHistory = NumHistory;
		SortHistory( NumHistory, RootFinderData.History );

	}

	void
	UpdateRootFinder(
		RootFinderDataType & RootFinderData, // Data used by root finding algorithm
		Real64 const X, // X value for current iterate
		Real64 const Y // Y value for current iterate, F(X)=Y
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dimitri Curtil (LBNL)
		//       DATE WRITTEN   February 2006
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine updates the root finder internal data to account for
		// the current iterate (X,Y):
		// - Lower / Upper support points
		// - Increments for successive iterates
		// - Convergence rate

		// METHODOLOGY EMPLOYED:
		// PRECONDITION:
		// - The current iterate (X,Y) must be a valid iterate:
		//   MinPoint%X <= LowerPoint%X < X < UpperPoint%X <= MaxPoint%X
		// - Invoke UpdateRootFinder() only if:
		//   - CheckRootFinderCandidate() returned TRUE
		//   - CheckNonSingularity() returned TRUE
		//   - CheckSlope() returned TRUE
		// POSTCONDITION:
		// - RootFinderData%LowerPoint possibly updated
		// - RootFinderData%UpperPoint possibly updated
		// - RootFinderData%CurrentPoint updated with current iterate (X,Y)
		// - RootFinderData%History(:) updated with last 3 best iterates
		// - RootFinderData%Increment updated
		// - RootFinderData%ConvergenceRate updated

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		// FLOW:

		// Update history with best iterates so that:
		//   ABS(History(1)%Y) <= ABS(History(2)%Y) <= ABS(History(3)%Y)
		// Note that we must update the history before updating the lower/upper points
		UpdateHistory( RootFinderData, X, Y );

		// Update lower and upper points
		UpdateBracket( RootFinderData, X, Y );

		// Update increments and convergence rate
		if ( RootFinderData.CurrentPoint.DefinedFlag ) {
			RootFinderData.Increment.DefinedFlag = true;
			RootFinderData.Increment.X = X - RootFinderData.CurrentPoint.X;
			RootFinderData.Increment.Y = Y - RootFinderData.CurrentPoint.Y;

			if ( std::abs( RootFinderData.CurrentPoint.Y ) > 0.0 ) {
				// NOTE: Should be smaller than one for convergent process
				RootFinderData.ConvergenceRate = std::abs( Y ) / std::abs( RootFinderData.CurrentPoint.Y );
			} else {
				// NOTE: Should never happen
				RootFinderData.ConvergenceRate = -1.0;
			}
		}

		// Finally update CurrentPoint (must be done last)
		RootFinderData.CurrentPoint.DefinedFlag = true;
		RootFinderData.CurrentPoint.X = X;
		RootFinderData.CurrentPoint.Y = Y;

	}

	void
	SortHistory(
		int const N, // Number of points to sort in history array
		Array1S< PointType > History // Array of PointType variables. At least N of them
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dimitri Curtil (LBNL)
		//       DATE WRITTEN   March 2006
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine orders the N points in the history array in increasing
		// order of ABS(Y) values.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int I;
		int J;
		Real64 XTemp;
		Real64 YTemp;
		// FLOW:

		// Nothing to do if only one point stored in history
		if ( N <= 1 ) {
			return;
		}

		for ( I = 1; I <= N - 1; ++I ) {
			for ( J = I + 1; J <= N; ++J ) {
				if ( History( J ).DefinedFlag ) {
					// Swap I and J elements
					if ( std::abs( History( J ).Y ) < std::abs( History( I ).Y ) ) {
						XTemp = History( I ).X;
						YTemp = History( I ).Y;
						History( I ).X = History( J ).X;
						History( I ).Y = History( J ).Y;
						History( J ).X = XTemp;
						History( J ).Y = YTemp;
					}
				}
			}
		}

	}

	void
	AdvanceRootFinder( RootFinderDataType & RootFinderData ) // Data used by root finding algorithm
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dimitri Curtil (LBNL)
		//       DATE WRITTEN   February 2006
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine computes the next candidate value based on the information available so far.
		// Stores new value into RootFinderData%XCandidate
		// PRECONDITION:
		// na
		// POSTCONDITION:
		// - LowerPoint%X < XCandidate < UpperPoint%X
		// - RootFinderData%CurrentMethodType update with current solution method.

		// METHODOLOGY EMPLOYED:
		// The subroutine first attempts to bracket the root within a lower and upper point.
		// Once it is bracketed, then we use the specified solution methods (Bisection,
		// False position, Secant and Brent) to compute the next candidate.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		static Real64 XNext( 0.0 );

		// FLOW:

		//----------------------------------------------------------------------------
		// First attempt to bracket root between a lower point and an upper point.
		//----------------------------------------------------------------------------

		// Detect the lower bracket
		if ( ! RootFinderData.LowerPoint.DefinedFlag ) {
			RootFinderData.CurrentMethodType = iMethodBracket;
			// If we have 2 points already, try to detect lower point using the Secant formula
			if ( BracketRoot( RootFinderData, XNext ) ) {
				RootFinderData.XCandidate = XNext;
			} else {
				if ( ! RootFinderData.MinPoint.DefinedFlag ) {
					RootFinderData.XCandidate = RootFinderData.MinPoint.X;
				} else {
					// Should never happen
					ShowFatalError( "AdvanceRootFinder: Cannot find lower bracket." );
				}
			}

			// Detect the upper bracket
		} else if ( ! RootFinderData.UpperPoint.DefinedFlag ) {
			RootFinderData.CurrentMethodType = iMethodBracket;
			// If we have 2 points already, try to detect upper point using the Secant formula
			if ( BracketRoot( RootFinderData, XNext ) ) {
				RootFinderData.XCandidate = XNext;
			} else {
				if ( ! RootFinderData.MaxPoint.DefinedFlag ) {
					RootFinderData.XCandidate = RootFinderData.MaxPoint.X;
				} else {
					// Should never happen
					ShowFatalError( "AdvanceRootFinder: Cannot find upper bracket." );
				}
			}

			//----------------------------------------------------------------------------
			// Root finding can start ...
			// Assumptions:
			// - the lower and upper support points are defined.
			// - the increments are defined (at least 2 history points are available)
			//----------------------------------------------------------------------------
		} else {
			{ auto const SELECT_CASE_var( RootFinderData.StatusFlag );
			if ( SELECT_CASE_var == iStatusOKRoundOff ) {
				// Should never happen if we exit the root finder upon detecting round-off condition
				RootFinderData.XCandidate = BisectionMethod( RootFinderData );

			} else if ( ( SELECT_CASE_var == iStatusWarningSingular ) || ( SELECT_CASE_var == iStatusWarningNonMonotonic ) ) {
				// Following local singularity or non-monotonicity warnings we attempt
				// to recover with the false position method to avoid running into trouble
				// because the latest iterate did nt produce any improvement compared to
				// the previous lower and upper brackets.
				RootFinderData.XCandidate = FalsePositionMethod( RootFinderData );

			} else {
				// Assuming that the root is bracketed between the lower and upper points,
				// we execute the requested solution method to produce the next candidate value
				// for the root.
				{ auto const SELECT_CASE_var1( RootFinderData.Controls.MethodType );
				if ( SELECT_CASE_var1 == iMethodBisection ) {
					// Bisection method (aka interval halving)
					RootFinderData.XCandidate = BisectionMethod( RootFinderData );
				} else if ( SELECT_CASE_var1 == iMethodFalsePosition ) {
					// False position method (aka regula falsi)
					RootFinderData.XCandidate = FalsePositionMethod( RootFinderData );
				} else if ( SELECT_CASE_var1 == iMethodSecant ) {
					// Secant method
					RootFinderData.XCandidate = SecantMethod( RootFinderData );
				} else if ( SELECT_CASE_var1 == iMethodBrent ) {
					// Brent method
					RootFinderData.XCandidate = BrentMethod( RootFinderData );
				} else {
					ShowSevereError( "AdvanceRootFinder: Invalid solution method specification. Valid choices are:" );
					ShowContinueError( "AdvanceRootFinder: iMethodBisection=" + TrimSigDigits( iMethodBisection ) );
					ShowContinueError( "AdvanceRootFinder: iMethodFalsePosition=" + TrimSigDigits( iMethodFalsePosition ) );
					ShowContinueError( "AdvanceRootFinder: iMethodSecant=" + TrimSigDigits( iMethodSecant ) );
					ShowContinueError( "AdvanceRootFinder: iMethodBrent=" + TrimSigDigits( iMethodBrent ) );
					ShowFatalError( "AdvanceRootFinder: Preceding error causes program termination." );
				}}
			}}
		}

	}

	bool
	BracketRoot(
		RootFinderDataType const & RootFinderData, // Data used by root finding algorithm
		Real64 & XNext // Next value
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Dimitri Curtil (LBNL)
		//       DATE WRITTEN   February 2006
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function attempts to compute a new point that will bracket the root
		// using the secant formula to take advantage of the slope between the last 2
		// iterates.
		// Returns TRUE if successfully computed a new bracket in XNext.
		// Else returns FASLE and does not update the XNext argument.
		// Should only be used while in braketing mode (iMethodBracket).
		// When the lower and upper brackets are detected then the FUNCTION SecantMethod
		// should be used instead.
		// PRECONDITION:
		// na
		// POSTCONDITION:
		// - MinPoint%X <= XNext <= MaxPoint%X
		// - LowerPoint%X < XNext < UpperPoint%X

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		bool BracketRoot;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		// FLOW:

		// Cannot use Secant method unless there are at least 2 points
		// Also do not use Secant method more than once, i.e. NumHistory==3, in order to avoid
		// the pathological case whereby the secant method always comes up short of bracketting
		// the root because the function slope flattens as we come closer to either min/max point.
		if ( RootFinderData.NumHistory != 2 ) {
			BracketRoot = false;
			return BracketRoot;
		}

		// Should not use Secant method if the last 2 points produced a warning
		if ( RootFinderData.StatusFlag == iStatusWarningSingular || RootFinderData.StatusFlag == iStatusWarningNonMonotonic ) {
			BracketRoot = false;
			return BracketRoot;
		}

		// Try to compute next root candidate using Secant formula
		if ( SecantFormula( RootFinderData, XNext ) ) {

			// Check that next candidate is consistent with min/max constraints and lower/upper brackets
			if ( CheckRootFinderCandidate( RootFinderData, XNext ) ) {
				BracketRoot = true;
				return BracketRoot;
			}
		}

		BracketRoot = false;

		return BracketRoot;
	}

	Real64
	BisectionMethod( RootFinderDataType & RootFinderData ) // Data used by root finding algorithm
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Dimitri Curtil (LBNL)
		//       DATE WRITTEN   February 2006
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function computes the next iterate using the bisection method (aka interval halving).
		// Convergence rate is at best linear.
		// PRECONDITION:
		// Lower and upper points must be defined and distinct.
		// POSTCONDITION:
		// - LowerPoint%X < XCandidate < UpperPoint%X
		// - RootFinderData%CurrentMethodType update with current solution method.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		Real64 BisectionMethod;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		// FLOW:
		RootFinderData.CurrentMethodType = iMethodBisection;
		BisectionMethod = ( RootFinderData.LowerPoint.X + RootFinderData.UpperPoint.X ) / 2.0;

		return BisectionMethod;
	}

	Real64
	FalsePositionMethod( RootFinderDataType & RootFinderData ) // Data used by root finding algorithm
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Dimitri Curtil (LBNL)
		//       DATE WRITTEN   February 2006
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function computes the next iterate using the false position method (aka regula falsi).
		// If new iterate does not lie within the lower and upper points then
		// the Bisection method is used instead.
		// Convergence rate is at best superlinear.
		// PRECONDITION:
		// Lower and upper points must be defined and distinct.
		// POSTCONDITION:
		// - LowerPoint%X < XCandidate < UpperPoint%X
		// - RootFinderData%CurrentMethodType update with current solution method.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		Real64 FalsePositionMethod;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 XCandidate;
		Real64 Num;
		Real64 Den;

		// FLOW:

		Num = RootFinderData.UpperPoint.X - RootFinderData.LowerPoint.X;
		Den = RootFinderData.UpperPoint.Y - RootFinderData.LowerPoint.Y;

		if ( Den != 0.0 ) {
			// False position method
			RootFinderData.CurrentMethodType = iMethodFalsePosition;
			XCandidate = RootFinderData.LowerPoint.X - RootFinderData.LowerPoint.Y * Num / Den;

			// Check that new candidate is within range and brackets
			if ( ! CheckRootFinderCandidate( RootFinderData, XCandidate ) ) {
				// Recovery method
				XCandidate = BisectionMethod( RootFinderData );
			}
		} else {
			// Recovery method
			XCandidate = BisectionMethod( RootFinderData );
		}

		FalsePositionMethod = XCandidate;
		return FalsePositionMethod;
	}

	Real64
	SecantMethod( RootFinderDataType & RootFinderData ) // Data used by root finding algorithm
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Dimitri Curtil (LBNL)
		//       DATE WRITTEN   February 2006
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function computes the next iterate using the secant method.
		// If new iterate does not lie within the lower and upper points then
		// the false position method is used instead.
		// Convergence rate is at best superlinear.
		// PRECONDITION:
		// There must be at least 2 history points so that RootFinderData%Increment is defined.
		// See FUNCTION SecantFormula.
		// POSTCONDITION:
		// - LowerPoint%X < XCandidate < UpperPoint%X
		// - RootFinderData%CurrentMethodType update with current solution method.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		Real64 SecantMethod;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 XCandidate;

		// FLOW:

		// Recover with false position
		if ( SecantFormula( RootFinderData, XCandidate ) ) {
			// Secant method
			RootFinderData.CurrentMethodType = iMethodSecant;

			// Check that new candidate is within range and brackets
			if ( ! CheckRootFinderCandidate( RootFinderData, XCandidate ) ) {
				// Recovery method
				XCandidate = FalsePositionMethod( RootFinderData );
			}
		} else {
			// Recovery method
			XCandidate = FalsePositionMethod( RootFinderData );
		}

		SecantMethod = XCandidate;
		return SecantMethod;
	}

	bool
	SecantFormula(
		RootFinderDataType const & RootFinderData, // Data used by root finding algorithm
		Real64 & XNext // Result from Secant formula if possible to compute
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Dimitri Curtil (LBNL)
		//       DATE WRITTEN   April 2006
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function computes the next iterate using the secant formula.
		// If the new iterate cannot be computed the function returns FALSE, else TRUE.
		// Convergence rate is at best superlinear.
		// PRECONDITION:
		// There must be at least 2 history points so that RootFinderData%Increment is defined.
		// POSTCONDITION:
		// XNext contains the result from applying the Secant formula.
		// If XNext could not be computed then leave XNext unchanged.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		bool SecantFormula;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 Num;
		Real64 Den;

		// FLOW:

		Num = RootFinderData.Increment.X;
		Den = RootFinderData.Increment.Y;

		// Cannot use secant with infinite slope (Den==0).
		// Cannot use secant with null slope (Num==0).
		if ( Den != 0.0 && Num != 0.0 ) {
			XNext = RootFinderData.CurrentPoint.X - RootFinderData.CurrentPoint.Y * Num / Den;
			SecantFormula = true;
		} else {
			SecantFormula = false;
		}

		return SecantFormula;
	}

	Real64
	BrentMethod( RootFinderDataType & RootFinderData ) // Data used by root finding algorithm
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         Dimitri Curtil (LBNL)
		//       DATE WRITTEN   March 2006
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function computes the next iterate using the Brent's method.
		// If new iterate does not lie within the lower and upper points then
		// the secant method is used instead.
		// Convergence rate is at best quadratic.
		// PRECONDITION:
		// Lower and upper points must be defined and distinct.
		// POSTCONDITION:
		// - LowerPoint%X < XCandidate < UpperPoint%X
		// - RootFinderData%CurrentMethodType update with current solution method.

		// METHODOLOGY EMPLOYED:
		// Inverse quadratic interpolation using the last 3 best iterates.
		// The next root estimate is x = B + P/Q whereby B is the current best estimate
		// of the root.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		Real64 BrentMethod;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 XCandidate;
		Real64 A;
		Real64 FA;
		Real64 B;
		Real64 FB;
		Real64 C;
		Real64 FC;
		Real64 R;
		Real64 S;
		Real64 T;
		Real64 P;
		Real64 Q;
		// FLOW:

		// Only attempt Brent's method if enough history points are available
		// and if the root finder is converging (not diverging) since the previous
		// iterate.
		// We assume that;
		// - the root is bracketed between the lower and upper points (see AdvanceRootFinder() ).
		// - there are at least 3 history points
		if ( RootFinderData.NumHistory == 3 ) {

			A = RootFinderData.History( 2 ).X;
			FA = RootFinderData.History( 2 ).Y;
			B = RootFinderData.History( 1 ).X;
			FB = RootFinderData.History( 1 ).Y;
			C = RootFinderData.History( 3 ).X;
			FC = RootFinderData.History( 3 ).Y;

			// Should never happen if CheckRootFinderConvergence() is invoked prior to this subroutine
			if ( FC == 0.0 ) {
				BrentMethod = C;
				return BrentMethod;
				// Should never happen if CheckRootFinderConvergence() is invoked prior to this subroutine
			} else if ( FA == 0.0 ) {
				BrentMethod = A;
				return BrentMethod;
			} else {
				R = FB / FC;
				S = FB / FA;
				T = FA / FC;

				P = S * ( T * ( R - T ) * ( C - B ) - ( 1.0 - R ) * ( B - A ) );
				Q = ( T - 1.0 ) * ( R - 1.0 ) * ( S - 1.0 );

				// Only accept correction if it is small enough (75% of previous increment)
				if ( std::abs( P ) <= 0.75 * std::abs( Q * RootFinderData.Increment.X ) ) {
					RootFinderData.CurrentMethodType = iMethodBrent;
					XCandidate = B + P / Q;

					// Check that new candidate is within range and brackets
					if ( ! CheckRootFinderCandidate( RootFinderData, XCandidate ) ) {
						// Recovery method
						XCandidate = FalsePositionMethod( RootFinderData );
					}
				} else {
					// Recover from bad correction with bisection
					// Biscetion produced the best numerical performance in testing compared to
					// - Secant
					// - False position (very slow recovery)
					XCandidate = BisectionMethod( RootFinderData );
				}
			}
		} else {
			// Not enough history to try Brent's method yet: use Secant's method
			XCandidate = SecantMethod( RootFinderData );
		}

		BrentMethod = XCandidate;
		return BrentMethod;
	}

	void
	WriteRootFinderTraceHeader( int const TraceFileUnit ) // Unit for trace file
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dimitri Curtil (LBNL)
		//       DATE WRITTEN   March 2006
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine writes the header for the trace file to the specified
		// file unit using CSV formatting.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		// FLOW:

		//'MinPoint%DefinedFlag', ',', &
		//'LowerPoint%DefinedFlag', ',', &
		//'UpperPoint%DefinedFlag', ',', &
		//'MaxPoint%DefinedFlag', ',', &
		//'History(1)%DefinedFlag', ',', &
		//'History(2)%DefinedFlag', ',', &
		//'History(3)%DefinedFlag', ',', &
		{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( TraceFileUnit, "(20(A,A))", flags ) << "Status" << ',' << "Method" << ',' << "CurrentPoint%X" << ',' << "CurrentPoint%Y" << ',' << "XCandidate" << ',' << "ConvergenceRate" << ',' << "MinPoint%X" << ',' << "MinPoint%Y" << ',' << "LowerPoint%X" << ',' << "LowerPoint%Y" << ',' << "UpperPoint%X" << ',' << "UpperPoint%Y" << ',' << "MaxPoint%X" << ',' << "MaxPoint%Y" << ',' << "History(1)%X" << ',' << "History(1)%Y" << ',' << "History(2)%X" << ',' << "History(2)%Y" << ',' << "History(3)%X" << ',' << "History(3)%Y" << ','; }

	}

	void
	WriteRootFinderTrace(
		int const TraceFileUnit, // Unit for trace file
		RootFinderDataType const & RootFinderData // Data used by root finding algorithm
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dimitri Curtil (LBNL)
		//       DATE WRITTEN   March 2006
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine writes the current state of the root finder data to the trace file
		// unit using CSV formatting.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::TrimSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		// FLOW:

		{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( TraceFileUnit, "(2(A,A))", flags ) << TrimSigDigits( RootFinderData.StatusFlag ) << ',' << TrimSigDigits( RootFinderData.CurrentMethodType ) << ','; }

		// Only show current point if defined.
		WritePoint( TraceFileUnit, RootFinderData.CurrentPoint, false );

		{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( TraceFileUnit, "(2(F20.10,A))", flags ) << RootFinderData.XCandidate << ',' << RootFinderData.ConvergenceRate << ','; }

		// Always show min and max points.
		// Only show lower and upper points if defined.
		WritePoint( TraceFileUnit, RootFinderData.MinPoint, true );
		WritePoint( TraceFileUnit, RootFinderData.LowerPoint, false );
		WritePoint( TraceFileUnit, RootFinderData.UpperPoint, false );
		WritePoint( TraceFileUnit, RootFinderData.MaxPoint, true );
		// Only show history points if defined.
		WritePoint( TraceFileUnit, RootFinderData.History( 1 ), false );
		WritePoint( TraceFileUnit, RootFinderData.History( 2 ), false );
		WritePoint( TraceFileUnit, RootFinderData.History( 3 ), false );

	}

	void
	WritePoint(
		int const TraceFileUnit, // Unit for trace file
		PointType const & PointData, // Point data structure
		bool const ShowXValue
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dimitri Curtil (LBNL)
		//       DATE WRITTEN   March 2006
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine writes the current point data to the trace file
		// unit using CSV formatting.
		// If not defined writes an empty string instead.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// If set to TRUE, ten always show the X value even if not defined

		// SUBROUTINE PARAMETER DEFINITIONS:
		static std::string NoValue; // String used whenever the value is not available

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		// FLOW:

		if ( PointData.DefinedFlag ) {
			{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( TraceFileUnit, "(2(F20.10,A))", flags ) << PointData.X << ',' << PointData.Y << ','; }
		} else {
			if ( ShowXValue ) {
				{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( TraceFileUnit, "(1(F20.10,A),1(A,A))", flags ) << PointData.X << ',' << NoValue << ','; }
			} else {
				{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( TraceFileUnit, "(2(A,A))", flags ) << NoValue << ',' << NoValue << ','; }
			}
		}

	}

	void
	DebugRootFinder(
		int const FileUnit, // File unit where to write debugging info
		RootFinderDataType const & RootFinderData // Data used by root finding algorithm
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dimitri Curtil (LBNL)
		//       DATE WRITTEN   April 2006
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine writes the current min/max range and lower/upper bracket to
		// the standard output file.
		// Used only for debugging.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static gio::Fmt fmtLD( "*" );
		static gio::Fmt fmtA( "(A)" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		// FLOW:

		// UNIT=0 should correspond to the standard output file (screen).
		{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( FileUnit, fmtA, flags ) << "Current = "; }
		WritePoint( FileUnit, RootFinderData.CurrentPoint, true );
		gio::write( FileUnit, fmtLD );

		{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( FileUnit, fmtA, flags ) << "Min     = "; }
		WritePoint( FileUnit, RootFinderData.MinPoint, true );
		gio::write( FileUnit, fmtLD );

		{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( FileUnit, fmtA, flags ) << "Lower   = "; }
		WritePoint( FileUnit, RootFinderData.LowerPoint, false );
		gio::write( FileUnit, fmtLD );

		{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( FileUnit, fmtA, flags ) << "Upper   = "; }
		WritePoint( FileUnit, RootFinderData.UpperPoint, false );
		gio::write( FileUnit, fmtLD );

		{ IOFlags flags; flags.ADVANCE( "No" ); gio::write( FileUnit, fmtA, flags ) << "Max     = "; }
		WritePoint( FileUnit, RootFinderData.MaxPoint, true );
		gio::write( FileUnit, fmtLD );

	}

	void
	WriteRootFinderStatus(
		int const FileUnit, // File unit where to write the status description
		RootFinderDataType const & RootFinderData // Data used by root finding algorithm
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dimitri Curtil (LBNL)
		//       DATE WRITTEN   May 2006
		//       MODIFIED
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static gio::Fmt fmtA( "(A)" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		// FLOW:

		{ auto const SELECT_CASE_var( RootFinderData.StatusFlag );
		if ( SELECT_CASE_var == iStatusOK ) {
			gio::write( FileUnit, fmtA ) << "Found unconstrained root";
		} else if ( SELECT_CASE_var == iStatusOKMin ) {
			gio::write( FileUnit, fmtA ) << "Found min constrained root";
		} else if ( SELECT_CASE_var == iStatusOKMax ) {
			gio::write( FileUnit, fmtA ) << "Found max constrained root";
		} else if ( SELECT_CASE_var == iStatusOKRoundOff ) {
			gio::write( FileUnit, fmtA ) << "Detected round-off convergence in bracket";

		} else if ( SELECT_CASE_var == iStatusWarningSingular ) {
			gio::write( FileUnit, fmtA ) << "Detected singularity warning";
		} else if ( SELECT_CASE_var == iStatusWarningNonMonotonic ) {
			gio::write( FileUnit, fmtA ) << "Detected non-monotonicity warning";

		} else if ( SELECT_CASE_var == iStatusErrorRange ) {
			gio::write( FileUnit, fmtA ) << "Detected out-of-range error";
		} else if ( SELECT_CASE_var == iStatusErrorBracket ) {
			gio::write( FileUnit, fmtA ) << "Detected bracket error";
		} else if ( SELECT_CASE_var == iStatusErrorSlope ) {
			gio::write( FileUnit, fmtA ) << "Detected slope error";
		} else if ( SELECT_CASE_var == iStatusErrorSingular ) {
			gio::write( FileUnit, fmtA ) << "Detected singularity error";

		} else {
			gio::write( FileUnit, fmtA ) << "Detected bad root finder status";
		}}

	}

} // RootFinder

} // EnergyPlus
