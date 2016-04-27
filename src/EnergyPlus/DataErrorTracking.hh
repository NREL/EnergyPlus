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

#ifndef DataErrorTracking_hh_INCLUDED
#define DataErrorTracking_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace DataErrorTracking {

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:
	extern int const SearchCounts;
	extern Array1D_string const MessageSearch;
	extern Array1D_string const Summaries;
	// in below -- simple line end <CR>.  End of Whole message <CRE>
	extern std::string const MoreDetails_1; // InterZone Surface Areas -- mismatch
	extern std::string const MoreDetails_2; // Interzone surfaces - different zones
	extern std::string const MoreDetails_3; // Node Connection Errors
	extern std::string const MoreDetails_4; // InterZone Surface Azimuths -- mismatch
	extern std::string const MoreDetails_5; // InterZone Surface Tilts -- mismatch
	extern std::string const MoreDetails_6; // Likely non-planar surfaces
	extern std::string const MoreDetails_7; // Deprecated Features or Key Values
	extern std::string const MoreDetails_8; // Incorrect Floor Tilt
	extern std::string const MoreDetails_9; // Incorrect Roof/Ceiling Tilt
	extern std::string const MoreDetails_10; // Incomplete View factors
	extern std::string const MoreDetails_11; // Unbalanced exhaust air flow
	extern std::string const MoreDetails_12; // Loads Initialization did not Converge
	extern std::string const MoreDetails_13; // CalcDaylightMapPoints: Window
	extern std::string const MoreDetails_14; // Zone Air Heat Balance Warnings
	extern std::string const MoreDetails_15; // Occupant density is extremely high
	extern std::string const MoreDetails_16; // Temperature (low) out of bounds AND Temperature (high) out of bounds
	extern std::string const MoreDetails_18; // Nominally unused constructions
	extern std::string const MoreDetails_19; // InfraredTransparent constructions in non-interzone surfaces
	extern std::string const MoreDetails_20; // No reporting elements requested
	extern Array1D_string const MoreDetails; // Details 16 applies to both temperature out of bounds | errors.

	extern int const MaxRecurringErrorMsgLength; // Maximum error message length for recurring error messages

	// DERIVED TYPE DEFINITIONS

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// MODULE VARIABLE DECLARATIONS:
	extern Array1D_int MatchCounts;
	extern bool AbortProcessing; // Flag used to if currently in "abort processing"
	extern int NumRecurringErrors; // Number of stored recurring error messages
	extern int TotalSevereErrors; // Counter
	extern int TotalWarningErrors; // Counter
	extern int TotalSevereErrorsDuringWarmup; // Counter
	extern int TotalWarningErrorsDuringWarmup; // Counter
	extern int TotalSevereErrorsDuringSizing; // Counter
	extern int TotalWarningErrorsDuringSizing; // Counter
	extern int TotalMultipliedWindows; // Counter
	extern int TotalCoincidentVertices; // Counter
	extern int TotalDegenerateSurfaces; // Counter
	extern int TotalReceivingNonConvexSurfaces; // Counter
	extern int TotalCastingNonConvexSurfaces; // Counter
	extern int TotalRoomAirPatternTooLow; // Counter
	extern int TotalRoomAirPatternTooHigh; // Counter
	extern bool AskForConnectionsReport; // Flag used to tell when connections should be reported
	extern bool AskForSurfacesReport; // Flag used to tell when surfaces should be reported
	extern bool AskForPlantCheckOnAbort; // flag used to tell if plant structure can be checked
	extern bool ExitDuringSimulations; // flag used to tell if program is in simulation mode when fatal occurs
	extern std::string LastSevereError;

	// Types

	struct RecurringErrorData
	{
		// Members
		std::string Message; // Message to be written to "error file" at end of simulation
		int Count; // Count of total times this recurring error message has been called
		int WarmupCount; // Count of times this recurring error message has been called during warmup
		int SizingCount; // Count of times this recurring error message has been called during sizing
		Real64 MaxValue; // Max of the values passed for this recurring error message
		Real64 MinValue; // Min of the values passed for this recurring error message
		Real64 SumValue; // Sum of the values passed for this recurring error message
		std::string MaxUnits; // units for Max values
		std::string MinUnits; // units for Min values
		std::string SumUnits; // units for Sum values
		bool ReportMax; // Flag to report max value
		bool ReportMin; // Flag to report min value
		bool ReportSum; // Flag to report sum value

		// Default Constructor
		RecurringErrorData() :
			Count( 0 ),
			WarmupCount( 0 ),
			SizingCount( 0 ),
			MaxValue( 0.0 ),
			MinValue( 0.0 ),
			SumValue( 0.0 ),
			ReportMax( false ),
			ReportMin( false ),
			ReportSum( false )
		{}

	};

	// Object Data
	extern Array1D< RecurringErrorData > RecurringErrors;

} // DataErrorTracking

} // EnergyPlus

#endif
