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

#ifndef SizingManager_hh_INCLUDED
#define SizingManager_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace SizingManager {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS: none

	// DERIVED TYPE DEFINITIONS: none

	// INTERFACE BLOCK SPECIFICATIONS: none

	// MODULE VARIABLE DECLARATIONS:
	extern int NumAirLoops;

	// SUBROUTINE SPECIFICATIONS FOR MODULE SimulationManager

	// Types

	struct ZoneListData
	{
		// Members
		std::string Name;
		int NumOfZones;
		Array1D_int Zones;

		// Default Constructor
		ZoneListData() :
			NumOfZones( 0 )
		{}

	};

	// Functions
	void
	clear_state();

	void
	ManageSizing();

	void
	GetOARequirements();

	void
	ProcessInputOARequirements(
		std::string const & cCurrentModuleObject,
		int const OAIndex,
		Array1_string const & cAlphaArgs,
		int & NumAlphas,
		Array1< Real64 > const & rNumericArgs,
		int & NumNumbers,
		Array1_bool const & lNumericFieldBlanks, //Unused
		Array1_bool const & lAlphaFieldBlanks,
		Array1_string const & cAlphaFieldNames,
		Array1_string const & cNumericFieldNames, //Unused
		bool & ErrorsFound // If errors found in input
	);

	void
	GetZoneAirDistribution();

	void
	GetZoneHVACSizing();

	void
	GetSizingParams();

	void
	GetZoneSizingInput();

	void
	GetZoneAndZoneListNames(
		bool & ErrorsFound,
		int & NumZones,
		Array1D_string & ZoneNames,
		int & NumZoneLists,
		Array1D< ZoneListData > & ZoneListNames
	);

	void
	GetSystemSizingInput();

	void
	GetPlantSizingInput();

	void
	SetupZoneSizing( bool & ErrorsFound );

	void
	ReportZoneSizing(
		std::string const & ZoneName, // the name of the zone
		std::string const & LoadType, // the description of the input variable
		Real64 const CalcDesLoad, // the value from the sizing calculation [W]
		Real64 const UserDesLoad, // the value from the sizing calculation modified by user input [W]
		Real64 const CalcDesFlow, // calculated design air flow rate [m3/s]
		Real64 const UserDesFlow, // user input or modified design air flow rate [m3/s]
		std::string const & DesDayName, // the name of the design day that produced the peak
		std::string const & PeakHrMin, // time stamp of the peak
		Real64 const PeakTemp, // temperature at peak [C]
		Real64 const PeakHumRat, // humidity ratio at peak [kg water/kg dry air]
		Real64 const FloorArea, // zone floor area [m2]
		Real64 const TotOccs, // design number of occupants for the zone
		Real64 const MinOAVolFlow, // zone design minimum outside air flow rate [m3/s]
		Real64 const DOASHeatAddRate // zone design heat addition rate from the DOAS [W]
	);

	void
	ReportSysSizing(
		std::string const & SysName, // the name of the zone
		std::string const & VarDesc, // the description of the input variable
		Real64 const VarValue // the value from the sizing calculation
	);

} // SizingManager

} // EnergyPlus

#endif
