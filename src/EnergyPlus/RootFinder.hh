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

} // RootFinder

} // EnergyPlus

#endif
