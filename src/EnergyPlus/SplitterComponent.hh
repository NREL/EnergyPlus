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

#ifndef SplitterComponent_hh_INCLUDED
#define SplitterComponent_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace SplitterComponent {

	// Using/Aliasing

	// Data
	// MODULE PARAMETERS:

	// MODULE PARAMETER DEFINITIONS
	// na

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	extern bool GetSplitterInputFlag;
	// Public because Used by SimAirServingZones and the Direct Air Unit
	extern int NumSplitters; // The Number of Splitters found in the Input
	extern Array1D_bool CheckEquipName;

	// Subroutine Specifications for the Module
	// Driver/Manager Routines

	// Get Input routines for module

	// Initialization routines for module

	// Algorithms for the module

	// Update routine to check convergence and update nodes

	// Reporting routines for module

	// Types

	struct SplitterConditions // public because USEd by SimAirServingZones and the Direct Air Unit
	{
		// Members
		std::string SplitterName; // Name of the Splitter
		Real64 InletTemp;
		Real64 InletHumRat;
		Real64 InletEnthalpy;
		Real64 InletPressure;
		int InletNode;
		Real64 InletMassFlowRate; // MassFlow through the Splitter being Simulated [kg/Sec]
		Real64 InletMassFlowRateMaxAvail; // Max Avail MassFlow through the Splitter being Simulated [kg/Sec]
		Real64 InletMassFlowRateMinAvail; // Min Avail MassFlow through the Splitter being Simulated [kg/Sec]
		int NumOutletNodes;
		Array1D_int OutletNode;
		Array1D< Real64 > OutletMassFlowRate;
		Array1D< Real64 > OutletMassFlowRateMaxAvail;
		Array1D< Real64 > OutletMassFlowRateMinAvail;
		Array1D< Real64 > OutletTemp;
		Array1D< Real64 > OutletHumRat;
		Array1D< Real64 > OutletEnthalpy;
		Array1D< Real64 > OutletPressure;

		// Default Constructor
		SplitterConditions() :
			InletTemp( 0.0 ),
			InletHumRat( 0.0 ),
			InletEnthalpy( 0.0 ),
			InletPressure( 0.0 ),
			InletNode( 0 ),
			InletMassFlowRate( 0.0 ),
			InletMassFlowRateMaxAvail( 0.0 ),
			InletMassFlowRateMinAvail( 0.0 ),
			NumOutletNodes( 0 )
		{}

	};

	// Object Data
	extern Array1D< SplitterConditions > SplitterCond;

	// Functions
	void
	clear_state();

	void
	SimAirLoopSplitter(
		std::string const & CompName,
		bool const FirstHVACIteration,
		bool const FirstCall,
		bool & SplitterInletChanged,
		int & CompIndex
	);

	//*******************************

	// Get Input Section of the Module
	//******************************************************************************

	void
	GetSplitterInput();

	// End of Get Input subroutines for the HB Module
	//******************************************************************************

	// Beginning Initialization Section of the Module
	//******************************************************************************

	void
	InitAirLoopSplitter(
		int const SplitterNum,
		bool const FirstHVACIteration,
		bool const FirstCall
	);

	// End Initialization Section of the Module
	//******************************************************************************

	// Begin Algorithm Section of the Module
	//******************************************************************************

	void
	CalcAirLoopSplitter(
		int const SplitterNum,
		bool const FirstCall
	);

	// End Algorithm Section of the Module
	// *****************************************************************************

	// Beginning of Update subroutines for the Splitter Module
	// *****************************************************************************

	void
	UpdateSplitter(
		int const SplitterNum,
		bool & SplitterInletChanged,
		bool const FirstCall
	);

	//        End of Update subroutines for the Splitter Module
	// *****************************************************************************

	// Beginning of Reporting subroutines for the Splitter Module
	// *****************************************************************************

	void
	ReportSplitter( int const SplitterNum );

	int
	GetSplitterOutletNumber(
		std::string const & SplitterName, // must match Splitter names for the Splitter type
		int const SplitterNum, // Index of Splitters
		bool & ErrorsFound // set to true if problem
	);

	Array1D_int
	GetSplitterNodeNumbers(
		std::string const & SplitterName, // must match Splitter names for the Splitter type
		int const SplitterNum, // Index of Splitters
		bool & ErrorsFound // set to true if problem
	);

	//        End of Reporting subroutines for the Splitter Module
	// *****************************************************************************

} // SplitterComponent

} // EnergyPlus

#endif
