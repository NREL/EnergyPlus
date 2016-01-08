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
#include <DataAirLoop.hh>
#include <DataPrecisionGlobals.hh>

namespace EnergyPlus {

namespace DataAirLoop {

	// MODULE INFORMATION:
	//       AUTHOR         Fred Buhl
	//       DATE WRITTEN   November 2003
	//       MODIFIED       L. Gu, Jan. 24, 2007. Add more variables to get information on OnOff fan operation
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This data-only module contains type definitions and variables
	// associated with HVAC air loops (AirLoopHVAC objects).

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:

	// DERIVED TYPE DEFINITIONS:

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// MODULE VARIABLE DECLARATIONS:

	int NumOASystems( 0 ); // Number of Outdoor Air Systems
	int LoopFanOperationMode( 0 ); // OnOff fan operation mode
	Real64 LoopSystemOnMassFlowrate( 0.0 ); // Loop mass flow rate during on cycle using an OnOff fan
	Real64 LoopSystemOffMassFlowrate( 0.0 ); // Loop mass flow rate during off cycle using an OnOff fan
	Real64 LoopOnOffFanPartLoadRatio( 0.0 ); // OnOff fan part load ratio
	Real64 LoopHeatingCoilMaxRTF( 0.0 ); // Maximum run time fraction for electric or gas heating coil in an HVAC Air Loop
	Real64 LoopOnOffFanRTF( 0.0 ); // OnOff fan run time fraction in an HVAC Air Loop
	Real64 LoopDXCoilRTF( 0.0 ); // OnOff fan run time fraction in an HVAC Air Loop
	Real64 LoopCompCycRatio( 0.0 ); // Loop compressor cycling ratio for multispeed heat pump
	bool AirLoopInputsFilled( false ); // Set to TRUE after first pass through air loop

	// Object Data
	Array1D< AirLoopZoneEquipConnectData > AirToZoneNodeInfo;
	Array1D< AirLoopOutsideAirConnectData > AirToOANodeInfo;
	Array1D< DefinePriAirSysAvailMgrs > PriAirSysAvailMgr;
	Array1D< AirLooptoZoneData > AirLoopZoneInfo;
	Array1D< AirLoopControlData > AirLoopControlInfo;
	Array1D< AirLoopFlowData > AirLoopFlow;
	Array1D< OAControllerData > OAControllerInfo;
	Array1D< OutsideAirSysProps > OutsideAirSys;

	// Clears the global data in DataAirLoop.
	// Needed for unit tests, should not be normally called.
	void
	clear_state()
	{
		NumOASystems = 0;
		LoopFanOperationMode = 0;
		LoopSystemOnMassFlowrate = 0.0;
		LoopSystemOffMassFlowrate = 0.0;
		LoopOnOffFanPartLoadRatio = 0.0;
		LoopHeatingCoilMaxRTF = 0.0;
		LoopOnOffFanRTF = 0.0;
		LoopDXCoilRTF = 0.0;
		LoopCompCycRatio = 0.0;
		AirLoopInputsFilled = false;
		AirToZoneNodeInfo.deallocate();
		AirToOANodeInfo.deallocate();
		PriAirSysAvailMgr.deallocate();
		AirLoopZoneInfo.deallocate();
		AirLoopControlInfo.deallocate();
		AirLoopFlow.deallocate();
		OAControllerInfo.deallocate();
		OutsideAirSys.deallocate();
	}

} // DataAirLoop

} // EnergyPlus
