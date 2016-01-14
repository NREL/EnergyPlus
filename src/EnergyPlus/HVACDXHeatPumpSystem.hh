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

#ifndef HVACDXHeatPumpSystem_hh_INCLUDED
#define HVACDXHeatPumpSystem_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace HVACDXHeatPumpSystem {

	// Using/Aliasing

	// Data
	//MODULE PARAMETER DEFINITIONS
	extern Real64 const MinAirMassFlow;
	// Compressor operation
	extern int const On; // normal compressor operation
	extern int const Off; // signal DXCoil that compressor shouldn't run

	// DERIVED TYPE DEFINITIONS

	//MODULE VARIABLE DECLARATIONS:
	extern int NumDXHeatPumpSystems; // The Number of DXHeatPumpSystems found in the Input
	extern bool EconomizerFlag; // holds air loop economizer status

	// Make this type allocatable
	extern Array1D_bool CheckEquipName;

	// Subroutine Specifications for the Module
	// Driver/Manager Routines

	// Get Input routines for module

	// Update routine to check convergence and update nodes

	// Types

	struct DXHeatPumpSystemStruct
	{
		// Members
		std::string DXHeatPumpSystemType; // Type of DXHeatingSystem
		std::string Name; // Name of the DXHeatingSystem
		int SchedPtr;
		std::string HeatPumpCoilType;
		int HeatPumpCoilType_Num;
		std::string HeatPumpCoilName;
		int HeatPumpCoilIndex;
		int DXHeatPumpCoilInletNodeNum;
		int DXHeatPumpCoilOutletNodeNum;
		int DXSystemControlNodeNum; // the node number of the node with the set point
		Real64 DesiredOutletTemp; // the temperature at the unit outlet node needed
		// to meet the supply air set point.
		Real64 PartLoadFrac; // part load fraction for current time step (single speed)
		Real64 SpeedRatio; // current compressor speed ratio (variable speed)
		Real64 CycRatio; // cycling part load ratio (variable speed)
		int FanOpMode; // Fan operating mode (see parameter above)
		// Warning message variables
		int DXCoilSensPLRIter; // used in DXCoil calculations
		int DXCoilSensPLRIterIndex; // used in DXCoil calculations
		int DXCoilSensPLRFail; // used in DXCoil calculations
		int DXCoilSensPLRFailIndex; // used in DXCoil calculations
		// When the Dx system is a part of Outdoor Air Unit
		Real64 OAUnitSetTemp; // set
		// variable-speed coil
		int SpeedNum; // select speed number for variable-speed coil

		// Default Constructor
		DXHeatPumpSystemStruct() :
			SchedPtr( 0 ),
			HeatPumpCoilType_Num( 0 ),
			HeatPumpCoilIndex( 0 ),
			DXHeatPumpCoilInletNodeNum( 0 ),
			DXHeatPumpCoilOutletNodeNum( 0 ),
			DXSystemControlNodeNum( 0 ),
			DesiredOutletTemp( 0.0 ),
			PartLoadFrac( 0.0 ),
			SpeedRatio( 0.0 ),
			CycRatio( 0.0 ),
			FanOpMode( 0 ),
			DXCoilSensPLRIter( 0 ),
			DXCoilSensPLRIterIndex( 0 ),
			DXCoilSensPLRFail( 0 ),
			DXCoilSensPLRFailIndex( 0 ),
			OAUnitSetTemp( 0.0 ),
			SpeedNum( 0 )
		{}

	};

	// Object Data
	extern Array1D< DXHeatPumpSystemStruct > DXHeatPumpSystem;

	// Functions

	void
	clear_state();

	void
	SimDXHeatPumpSystem(
		std::string const & DXHeatPumpSystemName, // Name of DXSystem:Airloop object
		bool const FirstHVACIteration, // True when first HVAC iteration
		int const AirLoopNum, // Primary air loop number
		int & CompIndex, // Index to CoilSystem:Heating:DX object
		Optional_int_const OAUnitNum = _, // If the system is an equipment of OutdoorAirUnit
		Optional< Real64 const > OAUCoilOutTemp = _, // the coil inlet temperature of OutdoorAirUnit
		Optional< Real64 > QTotOut = _ // the total cooling output of unit
	);

	// Get Input Section of the Module
	//******************************************************************************

	void
	GetDXHeatPumpSystemInput();

	// End of Get Input subroutines for the Module
	//******************************************************************************

	// Beginning of Initialization subroutines for the Module
	// *****************************************************************************

	void
	InitDXHeatPumpSystem(
		int const DXSystemNum, // number of the current DX Sys being simulated
		int const AirLoopNum, // number of the current air loop being simulated
		Optional_int_const OAUnitNum = _, // number of the current outdoor air unit being simulated
		Optional< Real64 const > OAUCoilOutTemp = _ // the coil inlet temperature of OutdoorAirUnit
	);

	// End of Initialization subroutines for the Module
	// *****************************************************************************

	// Beginning of Calculation subroutines for the DXCoolingSystem Module
	// *****************************************************************************

	void
	ControlDXHeatingSystem(
		int const DXSystemNum, // index to DXSystem
		bool const FirstHVACIteration // First HVAC iteration flag
	);

	Real64
	DXHeatingCoilResidual(
		Real64 const PartLoadFrac, // Compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	);

	//******************************************************************************

	Real64
	VSCoilCyclingResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	);

	//******************************************************************************

	Real64
	VSCoilSpeedResidual(
		Real64 const SpeedRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	);

} // HVACDXHeatPumpSystem

} // EnergyPlus

#endif
