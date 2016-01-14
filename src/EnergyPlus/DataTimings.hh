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

#ifndef DataTimings_hh_INCLUDED
#define DataTimings_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

#ifdef EP_NO_Timings
#undef EP_Timings
#endif

namespace DataTimings {

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:
	extern int const MaxTimingStringLength; // string length for timing string array

	// DERIVED TYPE DEFINITIONS

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// MODULE VARIABLE DECLARATIONS:
	extern int NumTimingElements;
	extern int MaxTimingElements;
	extern Real64 dailyWeatherTime;
	extern Real64 dailyExteriorEnergyUseTime;
	extern Real64 dailyHeatBalanceTime;
	extern Real64 hbdailyInit;
	extern Real64 hbdailyOutSurf;
	extern Real64 hbdailyInSurf;
	extern Real64 hbdailyHVAC;
	extern Real64 hbdailyRep;
	extern Real64 clockrate;
	extern bool lprocessingInputTiming;
	extern bool lmanageSimulationTiming;
	extern bool lcloseoutReportingTiming;

	// Following for calls to routines
#ifdef EP_Count_Calls
	extern int NumShadow_Calls;
	extern int NumShadowAtTS_Calls;
	extern int NumClipPoly_Calls;
	extern int NumInitSolar_Calls;
	extern int NumAnisoSky_Calls;
	extern int NumDetPolyOverlap_Calls;
	extern int NumCalcPerSolBeam_Calls;
	extern int NumDetShadowCombs_Calls;
	extern int NumIntSolarDist_Calls;
	extern int NumIntRadExchange_Calls;
	extern int NumIntRadExchangeZ_Calls;
	extern int NumIntRadExchangeMain_Calls;
	extern int NumIntRadExchangeOSurf_Calls;
	extern int NumIntRadExchangeISurf_Calls;
	extern int NumMaxInsideSurfIterations;
	extern int NumCalcScriptF_Calls;
#endif

	// Types

	struct timings
	{
		// Members
		std::string Element;
		Real64 rstartTime;
		Real64 currentTimeSum;
		int calls;

		// Default Constructor
		timings() :
			rstartTime( 0.0 ),
			currentTimeSum( 0.0 ),
			calls( 0 )
		{}

	};

	// Object Data
	extern Array1D< timings > Timing;

	// Functions

	void
	epStartTime( std::string const & ctimingElementstring );

	void
	epStopTime(
		std::string const & ctimingElementstring,
		Optional_bool_const printit = _, // true if it should be printed here.
		Optional_string_const wprint = _ // only needed (and assumed, if printit is true)
	);

	void
	epSummaryTimes( Real64 & TimeUsed_CPUTime );

	Real64
	epGetTimeUsed( std::string const & ctimingElementstring );

	Real64
	epGetTimeUsedperCall( std::string const & ctimingElementstring );

	Real64
	eptime();

	Real64
	epElapsedTime();

} // DataTimings

} // EnergyPlus

#endif
