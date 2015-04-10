#ifndef RootFinder_hh_INCLUDED
#define RootFinder_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1S.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataRootFinder.hh>

namespace EnergyPlus {

namespace RootFinder {

	// Using/Aliasing
	using DataRootFinder::PointType;
	using DataRootFinder::RootFinderDataType;

	// Data
	// MODULE PARAMETER DEFINITIONS
	// na

	// DERIVED TYPE DEFINITIONS
	// na

	//MODULE VARIABLE DECLARATIONS:
	// na

	// SUBROUTINE SPECIFICATIONS FOR THE MODULE

	// For a decreasing function,  MinPoint%Y > MaxPoint%Y

	// Functions

	void
	SetupRootFinder(
		RootFinderDataType & RootFinderData, // Data used by root finding algorithm
		int const SlopeType, // Either iSlopeIncreasing or iSlopeDecreasing
		int const MethodType, // Any of the iMethod<name> code but iMethodNone
		Real64 const TolX, // Relative tolerance for X variables
		Real64 const ATolX, // Absolute tolerance for X variables
		Real64 const ATolY // Absolute tolerance for Y variables
	);

	void
	ResetRootFinder(
		RootFinderDataType & RootFinderData, // Data used by root finding algorithm
		Real64 const XMin, // Minimum X value allowed
		Real64 const XMax // Maximum X value allowed
	);

	void
	InitializeRootFinder(
		RootFinderDataType & RootFinderData, // Data used by root finding algorithm
		Real64 const XMin, // Minimum X value allowed
		Real64 const XMax // Maximum X value allowed
	);

	void
	IterateRootFinder(
		RootFinderDataType & RootFinderData, // Data used by root finding algorithm
		Real64 const X, // X value of current iterate
		Real64 const Y, // Y value of current iterate
		Optional_bool IsDoneFlag = _ // If TRUE indicates that the iteration should be stopped
	);

	int
	CheckInternalConsistency( RootFinderDataType const & RootFinderData ); // Data used by root finding algorithm

	bool
	CheckRootFinderCandidate(
		RootFinderDataType const & RootFinderData, // Data used by root finding algorithm
		Real64 const X // X value for current iterate
	);

	bool
	CheckMinMaxRange(
		RootFinderDataType const & RootFinderData, // Data used by root finding algorithm
		Real64 const X // X value for current iterate
	);

	bool
	CheckLowerUpperBracket(
		RootFinderDataType const & RootFinderData, // Data used by root finding algorithm
		Real64 const X // X value for current iterate
	);

	bool
	CheckSlope( RootFinderDataType const & RootFinderData ); // Data used by root finding algorithm

	bool
	CheckNonSingularity( RootFinderDataType const & RootFinderData ); // Data used by root finding algorithm

	bool
	CheckMinConstraint( RootFinderDataType const & RootFinderData ); // Data used by root finding algorithm

	bool
	CheckMaxConstraint( RootFinderDataType const & RootFinderData ); // Data used by root finding algorithm

	bool
	CheckRootFinderConvergence(
		RootFinderDataType const & RootFinderData, // Data used by root finding algorithm
		Real64 const Y // Y value for current iterate
	);

	bool
	CheckIncrementRoundOff(
		RootFinderDataType const & RootFinderData, // Data used by root finding algorithm
		Real64 const X // X value for current iterate
	);

	bool
	CheckBracketRoundOff( RootFinderDataType const & RootFinderData ); // Data used by root finding algorithm

	void
	UpdateMinMax(
		RootFinderDataType & RootFinderData, // Data used by root finding algorithm
		Real64 const X, // X value for current iterate
		Real64 const Y // Y value for current iterate, F(X)=Y
	);

	void
	UpdateBracket(
		RootFinderDataType & RootFinderData, // Data used by root finding algorithm
		Real64 const X, // X value for current iterate
		Real64 const Y // Y value for current iterate, F(X)=Y
	);

	void
	UpdateHistory(
		RootFinderDataType & RootFinderData, // Data used by root finding algorithm
		Real64 const X, // X value for current iterate
		Real64 const Y // Y value for current iterate, F(X)=Y
	);

	void
	UpdateRootFinder(
		RootFinderDataType & RootFinderData, // Data used by root finding algorithm
		Real64 const X, // X value for current iterate
		Real64 const Y // Y value for current iterate, F(X)=Y
	);

	void
	SortHistory(
		int const N, // Number of points to sort in history array
		Array1S< PointType > History // Array of PointType variables. At least N of them
	);

	void
	AdvanceRootFinder( RootFinderDataType & RootFinderData ); // Data used by root finding algorithm

	bool
	BracketRoot(
		RootFinderDataType const & RootFinderData, // Data used by root finding algorithm
		Real64 & XNext // Next value
	);

	Real64
	BisectionMethod( RootFinderDataType & RootFinderData ); // Data used by root finding algorithm

	Real64
	FalsePositionMethod( RootFinderDataType & RootFinderData ); // Data used by root finding algorithm

	Real64
	SecantMethod( RootFinderDataType & RootFinderData ); // Data used by root finding algorithm

	bool
	SecantFormula(
		RootFinderDataType const & RootFinderData, // Data used by root finding algorithm
		Real64 & XNext // Result from Secant formula if possible to compute
	);

	Real64
	BrentMethod( RootFinderDataType & RootFinderData ); // Data used by root finding algorithm

	void
	WriteRootFinderTraceHeader( int const TraceFileUnit ); // Unit for trace file

	void
	WriteRootFinderTrace(
		int const TraceFileUnit, // Unit for trace file
		RootFinderDataType const & RootFinderData // Data used by root finding algorithm
	);

	void
	WritePoint(
		int const TraceFileUnit, // Unit for trace file
		PointType const & PointData, // Point data structure
		bool const ShowXValue
	);

	void
	DebugRootFinder(
		int const FileUnit, // File unit where to write debugging info
		RootFinderDataType const & RootFinderData // Data used by root finding algorithm
	);

	void
	WriteRootFinderStatus(
		int const FileUnit, // File unit where to write the status description
		RootFinderDataType const & RootFinderData // Data used by root finding algorithm
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

} // RootFinder

} // EnergyPlus

#endif
