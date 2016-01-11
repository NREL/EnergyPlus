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

#ifndef InternalHeatGains_hh_INCLUDED
#define InternalHeatGains_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1S.hh>
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace InternalHeatGains {

	// Data
	// MODULE PARAMETER DEFINITIONS:

	extern bool GetInternalHeatGainsInputFlag; // Controls the GET routine calling (limited to first time)

	// SUBROUTINE SPECIFICATIONS FOR MODULE InternalHeatGains
	//PUBLIC  SumInternalConvectionGainsByIndices
	//PUBLIC SumReturnAirConvectionGainsByIndices
	//PUBLIC  SumInternalRadiationGainsByIndices
	//PUBLIC  SumInternalLatentGainsByIndices
	//PUBLIC
	//PUBLIC  SumInternalCO2GainsByIndices
	//PUBLIC  GetInternalGainDeviceIndex

	// Functions
	void
	clear_state();

	void
	ManageInternalHeatGains( Optional_bool_const InitOnly = _ ); // when true, just calls the get input, if appropriate and returns.

	void
	GetInternalHeatGainsInput();

	void
	InitInternalHeatGains();

	void
	CalcZoneITEq();

	void
	ReportInternalHeatGains();

	Real64
	GetDesignLightingLevelForZone( int const WhichZone ); // name of zone

	void
	CheckLightsReplaceableMinMaxForZone( int const WhichZone ); // Zone Number

	void
	UpdateInternalGainValues(
		Optional_bool_const SuppressRadiationUpdate = _,
		Optional_bool_const SumLatentGains = _
	);

	void
	SumAllInternalConvectionGains(
		int const ZoneNum, // zone index pointer for which zone to sum gains for
		Real64 & SumConvGainRate
	);

	void
	SumInternalConvectionGainsByTypes(
		int const ZoneNum, // zone index pointer for which zone to sum gains for
		Array1S_int const GainTypeARR, // variable length 1-d array of integer valued gain types
		Real64 & SumConvGainRate
	);

	void
	GetInternalGainDeviceIndex(
		int const ZoneNum, // zone index pointer for which zone to sum gains for
		int const IntGainTypeOfNum, // zone internal gain type number
		std::string const & IntGainName, // Internal gain name
		int & DeviceIndex, // Device index
		bool & ErrorFound
	);

	void
	SumInternalConvectionGainsByIndices(
		int const ZoneNum, // zone index pointer for which zone to sum gains for
		Array1S_int const DeviceIndexARR, // variable length 1-d array of integer device index pointers to include in summation
		Array1A< Real64 > const FractionARR, // array of fractional multipliers to apply to devices
		Real64 & SumConvGainRate
	);

	void
	SumInternalLatentGainsByIndices(
		int const ZoneNum, // zone index pointer for which zone to sum gains for
		Array1S_int const DeviceIndexARR, // variable length 1-d array of integer device index pointers to include in summation
		Array1A< Real64 > const FractionARR, // array of fractional multipliers to apply to devices
		Real64 & SumLatentGainRate
	);

	void
	SumReturnAirConvectionGainsByIndices(
		int const ZoneNum, // zone index pointer for which zone to sum gains for
		Array1S_int const DeviceIndexARR, // variable length 1-d array of integer device index pointers to include in summation
		Array1A< Real64 > const FractionARR, // array of fractional multipliers to apply to devices
		Real64 & SumReturnAirGainRate
	);

	void
	SumAllReturnAirConvectionGains(
		int const ZoneNum, // zone index pointer for which zone to sum gains for
		Real64 & SumReturnAirGainRate
	);

	void
	SumReturnAirConvectionGainsByTypes(
		int const ZoneNum, // zone index pointer for which zone to sum gains for
		Array1S_int const GainTypeARR, // variable length 1-d array of integer valued gain types
		Real64 & SumReturnAirGainRate
	);

	void
	SumAllInternalRadiationGains(
		int const ZoneNum, // zone index pointer for which zone to sum gains for
		Real64 & SumRadGainRate
	);

	void
	SumInternalRadiationGainsByTypes(
		int const ZoneNum, // zone index pointer for which zone to sum gains for
		Array1S_int const GainTypeARR, // variable length 1-d array of integer valued gain types
		Real64 & SumRadiationGainRate
	);

	void
	SumAllInternalLatentGains(
		int const ZoneNum, // zone index pointer for which zone to sum gains for
		Real64 & SumLatentGainRate
	);

	void
	SumInternalLatentGainsByTypes(
		int const ZoneNum, // zone index pointer for which zone to sum gains for
		Array1S_int const GainTypeARR, // variable length 1-d array of integer valued gain types
		Real64 & SumLatentGainRate
	);

	void
	SumAllReturnAirLatentGains(
		int const ZoneNum, // zone index pointer for which zone to sum gains for
		Real64 & SumRetAirLatentGainRate
	);

	void
	SumAllInternalCO2Gains(
		int const ZoneNum, // zone index pointer for which zone to sum gains for
		Real64 & SumCO2GainRate
	);

	void
	SumInternalCO2GainsByTypes(
		int const ZoneNum, // zone index pointer for which zone to sum gains for
		Array1S_int const GainTypeARR, // variable length 1-d array of integer valued gain types
		Real64 & SumCO2GainRate
	);

	void
	SumAllInternalGenericContamGains(
		int const ZoneNum, // zone index pointer for which zone to sum gains for
		Real64 & SumGCGainRate
	);

	void
	GatherComponentLoadsIntGain();

} // InternalHeatGains

} // EnergyPlus

#endif
