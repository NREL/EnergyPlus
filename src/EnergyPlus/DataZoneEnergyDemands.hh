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

#ifndef DataZoneEnergyDemands_hh_INCLUDED
#define DataZoneEnergyDemands_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace DataZoneEnergyDemands {

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE VARIABLE DECLARATIONS:

	extern Array1D_bool DeadBandOrSetback; // true if zone temperature is in the thermostat deadband
	// before any heating / cooling done
	extern Array1D_bool Setback; // true if zone temperature has increased
	// from previous setting
	extern Array1D_bool CurDeadBandOrSetback; // same as above except updated after each piece of zone equipment
	// in a zone is simulated

	// Types

	struct ZoneSystemDemandData // Sensible cooling/heating loads to be met (watts)
	{
		// Members
		Real64 RemainingOutputRequired;
		Real64 TotalOutputRequired;
		Real64 OutputRequiredToHeatingSP; // Load required to meet heating setpoint (>0 is a heating load)
		Real64 OutputRequiredToCoolingSP; // Load required to meet cooling setpoint (<0 is a cooling load)
		Real64 RemainingOutputReqToHeatSP; // Remaining load required to meet heating setpoint (>0 is a heating load)
		Real64 RemainingOutputReqToCoolSP; // Remaining load required to meet cooling setpoint (<0 is a cooling load)
		int NumZoneEquipment; // count of zone equipment for this zone, from ZoneHVAC:EquipmentList
		Array1D< Real64 > SequencedOutputRequired;
		Array1D< Real64 > SequencedOutputRequiredToHeatingSP; // load required to meet heating setpoint by sequence
		Array1D< Real64 > SequencedOutputRequiredToCoolingSP; // load required to meet cooling setpoint by sequence
		Real64 SupplyAirAdjustFactor; // supply air adjustment factor due to the cap of
		// zone maximum outdoor air fraction
		int StageNum; // The stage number when staged thermostate is used:
		// 0 no load, >0 Heating stage, <0 Cooling stage

		// Default Constructor
		ZoneSystemDemandData() :
			RemainingOutputRequired( 0.0 ),
			TotalOutputRequired( 0.0 ),
			OutputRequiredToHeatingSP( 0.0 ),
			OutputRequiredToCoolingSP( 0.0 ),
			RemainingOutputReqToHeatSP( 0.0 ),
			RemainingOutputReqToCoolSP( 0.0 ),
			NumZoneEquipment( 0 ),
			SupplyAirAdjustFactor( 1.0 ),
			StageNum( 0 )
		{}

	};

	struct ZoneSystemMoistureDemand // Humidification/dehumidification loads to be met (kg water per second)
	{
		// Members
		Real64 RemainingOutputRequired;
		Real64 TotalOutputRequired;
		Real64 OutputRequiredToHumidifyingSP; // Load required to meet humidifying setpoint (>0 = a humidify load)
		Real64 OutputRequiredToDehumidifyingSP; // Load required to meet dehumidifying setpoint (<0 = a dehumidify load)
		Real64 RemainingOutputReqToHumidSP; // Remaining load required to meet humidifying setpoint
		// (>0 is a humidify load)
		Real64 RemainingOutputReqToDehumidSP; // Remaining load required to meet dehumidifying setpoint
		// (<0 is a dehumidify load)
		int NumZoneEquipment; // count of zone equipment for this zone, from ZoneHVAC:EquipmentList
		Array1D< Real64 > SequencedOutputRequired;
		Array1D< Real64 > SequencedOutputRequiredToHumidSP; // load required to meet humidify setpoint by sequence
		Array1D< Real64 > SequencedOutputRequiredToDehumidSP; // load required to meet dehumidify setpoint by sequenc

		// Default Constructor
		ZoneSystemMoistureDemand() :
			RemainingOutputRequired( 0.0 ),
			TotalOutputRequired( 0.0 ),
			OutputRequiredToHumidifyingSP( 0.0 ),
			OutputRequiredToDehumidifyingSP( 0.0 ),
			RemainingOutputReqToHumidSP( 0.0 ),
			RemainingOutputReqToDehumidSP( 0.0 ),
			NumZoneEquipment( 0 )
		{}

	};

	// Object Data
	extern Array1D< ZoneSystemDemandData > ZoneSysEnergyDemand;
	extern Array1D< ZoneSystemMoistureDemand > ZoneSysMoistureDemand;

	void
	clear_state();

} // DataZoneEnergyDemands

} // EnergyPlus

#endif
