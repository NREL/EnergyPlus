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
#include <NonZoneEquipmentManager.hh>
#include <DataGlobals.hh>
#include <InputProcessor.hh>
#include <WaterThermalTanks.hh>
#include <WaterUse.hh>

namespace EnergyPlus {

namespace NonZoneEquipmentManager {

	// MODULE INFORMATION:
	//       AUTHOR         Peter Graham Ellis
	//       DATE WRITTEN   January 2004
	//       MODIFIED       Hudson, ORNL July 2007
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:

	// METHODOLOGY EMPLOYED: na

	// REFERENCES: na
	// OTHER NOTES: na
	// USE STATEMENTS: na

	// Data
	// MODULE PARAMETER DEFINITIONS: na
	// MODULE VARIABLE DECLARATIONS: na

	// SUBROUTINE SPECIFICATIONS:

	// MODULE SUBROUTINES:

	// Functions

	void
	ManageNonZoneEquipment(
		bool const FirstHVACIteration,
		bool & SimNonZoneEquipment // Simulation convergence flag
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Dan Fisher
		//       DATE WRITTEN   Sept. 2000
		//       RE-ENGINEERED  Richard Liesen
		//       DATE MODIFIED  February 2003
		//       MODIFIED       Hudson, ORNL July 2007
		//       MODIFIED       B. Grifffith, NREL, April 2008,
		//                      added calls for just heat recovery part of chillers
		//       MODIFIED       Removed much for plant upgrade, 2011

		// PURPOSE OF THIS SUBROUTINE:
		// This routine checks the input file for any non-zone equipment objects and gets their input.
		// Zone equipment objects are generally triggered to "get input" when they are called for simulation
		// by the ZoneEquipmentManager because they are referenced by a Zone Equipment List.  In the case of
		// the NonZoneEquipmentManager, it does not yet have a list of non-zone equipment, so it must make
		// one here before it knows what to call for simulation.

		// METHODOLOGY EMPLOYED: na

		// REFERENCES: na

		// Using/Aliasing
		using DataGlobals::ZoneSizingCalc;
		using InputProcessor::GetNumObjectsFound;
		using WaterThermalTanks::SimulateWaterHeaterStandAlone;
		using WaterUse::SimulateWaterUse;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		int WaterHeaterNum; // Water heater object number
		static int NumOfWaterHeater;
		static bool CountNonZoneEquip( true );

		// FLOW:
		if ( CountNonZoneEquip ) {
			NumOfWaterHeater = GetNumObjectsFound( "WaterHeater:Mixed" ) + GetNumObjectsFound( "WaterHeater:Stratified" );
			CountNonZoneEquip = false;
		}

		SimulateWaterUse( FirstHVACIteration ); // simulate non-plant loop water use.

		if ( ! ZoneSizingCalc ) {
			for ( WaterHeaterNum = 1; WaterHeaterNum <= NumOfWaterHeater; ++WaterHeaterNum ) {
				SimulateWaterHeaterStandAlone( WaterHeaterNum, FirstHVACIteration );
			}
		}

		if ( FirstHVACIteration ) {
			SimNonZoneEquipment = true;
		} else {
			SimNonZoneEquipment = false;
		}

	}

} // NonZoneEquipmentManager

} // EnergyPlus
