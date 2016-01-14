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
#include <DataZoneControls.hh>
#include <DataPrecisionGlobals.hh>

namespace EnergyPlus {

namespace DataZoneControls {

	// Module containing the routines dealing with the zone controls.

	// MODULE INFORMATION:
	//       AUTHOR         Linda Lawrie
	//       DATE WRITTEN   October 2007
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This module has the data and structures for various types of controls
	// (humidity, temperature, comfort) within the zones.  This data was formerly
	// public data in ZoneTempPredictorCorrector.

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	// <use statements for access to subroutines in other modules>

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// na

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	int NumTempControlledZones( 0 );
	int NumHumidityControlZones( 0 );
	int NumComfortControlledZones( 0 );
	int NumTStatStatements( 0 );
	int NumComfortTStatStatements( 0 );
	int NumOpTempControlledZones( 0 ); // number of zones with operative temp control
	int NumTempAndHumidityControlledZones( 0 ); // number of zones with over cool control
	bool AnyOpTempControl( false ); // flag set true if any zones have op temp control
	bool AnyZoneTempAndHumidityControl( false ); // flag set true if any zones have over cool control
	Array1D_bool StageZoneLogic; // Logical array, A zone with staged thermostat = .TRUE.
	Array1D< Real64 > OccRoomTSetPointHeat; // occupied heating set point for optimum start period
	Array1D< Real64 > OccRoomTSetPointCool; // occupied cooling set point for optimum start period
	bool GetZoneAirStatsInputFlag( true ); // True when need to get input

	// Object Data
	Array1D< ZoneHumidityControls > HumidityControlZone;
	Array1D< ZoneTempControls > TempControlledZone;
	Array1D< ZoneComfortControls > ComfortControlledZone;
	Array1D< TStatObject > TStatObjects;
	Array1D< TStatObject > ComfortTStatObjects;
	Array1D< TStatObject > StagedTStatObjects;
	Array1D< ZoneStagedControls > StageControlledZone;

	// Clears the global data in DataZoneControls.
	// Needed for unit tests, should not be normally called.
	void
	clear_state()
	{
		NumTempControlledZones = 0;
		NumHumidityControlZones = 0;
		NumComfortControlledZones = 0;
		NumTStatStatements = 0;
		NumComfortTStatStatements = 0;
		NumOpTempControlledZones = 0; // number of zones with operative temp control
		NumTempAndHumidityControlledZones = 0; // number of zones with over cool control
		AnyOpTempControl = false; // flag set true if any zones have op temp control
		AnyZoneTempAndHumidityControl = false; // flag set true if any zones have over cool control
		GetZoneAirStatsInputFlag = true; // True when need to get input
		StageZoneLogic.deallocate();
		OccRoomTSetPointHeat.deallocate();
		OccRoomTSetPointCool.deallocate();
		HumidityControlZone.deallocate();
		TempControlledZone.deallocate();
		ComfortControlledZone.deallocate();
		TStatObjects.deallocate();
		ComfortTStatObjects.deallocate();
		StagedTStatObjects.deallocate();
		StageControlledZone.deallocate();
	}

} // DataZoneControls

} // EnergyPlus
