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

#ifndef HVACDXSystem_hh_INCLUDED
#define HVACDXSystem_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

// note that there are two modules in this file

//  HVACDXSystem is for cooling DX coils

//  HVACDXHeatPumpSystem is for heating DX coils

namespace HVACDXSystem {

	// Using/Aliasing

	// Data
	//MODULE PARAMETER DEFINITIONS
	extern Real64 const MinAirMassFlow;
	// Compressor operation
	extern int const On; // normal compressor operation
	extern int const Off; // signal DXCoil that compressor shouldn't run
	// Dehumidification control modes (DehumidControlMode)
	extern int const DehumidControl_None;
	extern int const DehumidControl_Multimode;
	extern int const DehumidControl_CoolReheat;
	extern bool GetInputFlag; // Flag to get input only once

	//packaged TES modes
	extern int const OffMode;
	extern int const CoolingOnlyMode;
	extern int const CoolingAndChargeMode;
	extern int const CoolingAndDischargeMode;
	extern int const ChargeOnlyMode;
	extern int const DischargeOnlyMode;

	// DERIVED TYPE DEFINITIONS

	//MODULE VARIABLE DECLARATIONS:
	extern int NumDXSystem; // The Number of DXCoolingSystems found in the Input
	extern bool EconomizerFlag; // holds air loop economizer status

	// Make this type allocatable
	extern Array1D_bool CheckEquipName;

	// Subroutine Specifications for the Module
	// Driver/Manager Routines

	// Get Input routines for module

	// Update routine to check convergence and update nodes

	// Types

	struct DXCoolingConditions
	{
		// Members
		std::string DXCoolingSystemType; // Type of DXCoolingSystem
		std::string Name; // Name of the DXCoolingSystem
		int SchedPtr;
		std::string CoolingCoilType;
		int CoolingCoilType_Num;
		std::string CoolingCoilName;
		int CoolingCoilIndex;
		int DXCoolingCoilInletNodeNum;
		int DXCoolingCoilOutletNodeNum;
		int DXSystemControlNodeNum; // the node number of the node with the setpoint
		Real64 DesiredOutletTemp; // the temperature at the unit outlet node needed
		// to meet the supply air setpoint.
		Real64 DesiredOutletHumRat; // the humidity ratio at the unit outlet node needed
		// to meet the supply air setpoint.
		Real64 PartLoadFrac; // part load fraction for current time step (single speed)
		Real64 SpeedRatio; // current compressor speed ratio (variable speed)
		Real64 CycRatio; // cycling part load ratio (variable speed)
		bool RunOnSensibleLoad; // logical determines if this system will run to
		// meet a sensible load - for future use
		bool RunOnLatentLoad; // logical determines if this system will run to
		// meet a latent-only load - for future use
		int DehumidControlType; // Dehumidification control type (currently only for multimode coil)
		int DehumidificationMode; // Dehumidification mode for multimode coil,
		// 0=normal, 1+=enhanced dehumidification mode
		int FanOpMode; // Fan operating mode (see parameter above)
		// Warning message variables
		int HXAssistedSensPLRIter; // used in HX Assisted calculations
		int HXAssistedSensPLRIterIndex; // used in HX Assisted calculations
		int HXAssistedSensPLRFail; // used in HX Assisted calculations
		int HXAssistedSensPLRFailIndex; // used in HX Assisted calculations
		int HXAssistedSensPLRFail2; // used in HX Assisted calculations
		int HXAssistedSensPLRFailIndex2; // used in HX Assisted calculations
		int HXAssistedLatPLRIter; // used in HX Assisted calculations
		int HXAssistedLatPLRIterIndex; // used in HX Assisted calculations
		int HXAssistedLatPLRFail; // used in HX Assisted calculations
		int HXAssistedLatPLRFailIndex; // used in HX Assisted calculations
		int HXAssistedCRLatPLRIter; // used in HX Assisted calculations
		int HXAssistedCRLatPLRIterIndex; // used in HX Assisted calculations
		int HXAssistedCRLatPLRFail; // used in HX Assisted calculations
		int HXAssistedCRLatPLRFailIndex; // used in HX Assisted calculations
		int HXAssistedCRLatPLRFail2; // used in HX Assisted calculations
		int HXAssistedCRLatPLRFailIndex2; // used in HX Assisted calculations
		int DXCoilSensPLRIter; // used in DXCoil calculations
		int DXCoilSensPLRIterIndex; // used in DXCoil calculations
		int DXCoilSensPLRFail; // used in DXCoil calculations
		int DXCoilSensPLRFailIndex; // used in DXCoil calculations
		int DXCoilLatPLRIter; // used in DXCoil calculations
		int DXCoilLatPLRIterIndex; // used in DXCoil calculations
		int DXCoilLatPLRFail; // used in DXCoil calculations
		int DXCoilLatPLRFailIndex; // used in DXCoil calculations
		int MSpdSensPLRIter; // used in MultiSpeed calculations
		int MSpdSensPLRIterIndex; // used in MultiSpeed calculations
		int MSpdCycSensPLRIter; // used in MultiSpeed calculations
		int MSpdCycSensPLRIterIndex; // used in MultiSpeed calculations
		int MSpdLatPLRIter; // used in MultiSpeed calculations
		int MSpdLatPLRIterIndex; // used in MultiSpeed calculations
		int MSpdCycLatPLRIter; // used in MultiSpeed calculations
		int MSpdCycLatPLRIterIndex; // used in MultiSpeed calculations
		int MModeSensPLRIter; // used in MultiMode calculations
		int MModeSensPLRIterIndex; // used in MultiMode calculations
		int MModeLatPLRIter; // used in MultiMode calculations
		int MModeLatPLRIterIndex; // used in MultiMode calculations
		int MModeLatPLRIter2; // used in MultiMode calculations
		int MModeLatPLRIterIndex2; // used in MultiMode calculations
		// When the Dx system is a part of Outdoor Air Unit
		Real64 OAUnitSetTemp; // set
		// DOAS DX Cooling coil
		bool ISHundredPercentDOASDXCoil; // logical determines if this system will run as 100% DOAS
		// DX Coil, false is regular DX coil
		Real64 DOASDXCoolingCoilMinTout; // DOAS DX Cooling coil outlet air minimum temperature
		int FrostControlStatus; // DOAS coil system frost control status
		// variable-speed coil
		int SpeedNum; // select speed number for variable-speed coil
		// Packaged thermal energy storage coil
		int TESOpMode;

		// Default Constructor
		DXCoolingConditions() :
			SchedPtr( 0 ),
			CoolingCoilType_Num( 0 ),
			CoolingCoilIndex( 0 ),
			DXCoolingCoilInletNodeNum( 0 ),
			DXCoolingCoilOutletNodeNum( 0 ),
			DXSystemControlNodeNum( 0 ),
			DesiredOutletTemp( 0.0 ),
			DesiredOutletHumRat( 1.0 ),
			PartLoadFrac( 0.0 ),
			SpeedRatio( 0.0 ),
			CycRatio( 0.0 ),
			RunOnSensibleLoad( true ),
			RunOnLatentLoad( false ),
			DehumidControlType( 0 ),
			DehumidificationMode( 0 ),
			FanOpMode( 0 ),
			HXAssistedSensPLRIter( 0 ),
			HXAssistedSensPLRIterIndex( 0 ),
			HXAssistedSensPLRFail( 0 ),
			HXAssistedSensPLRFailIndex( 0 ),
			HXAssistedSensPLRFail2( 0 ),
			HXAssistedSensPLRFailIndex2( 0 ),
			HXAssistedLatPLRIter( 0 ),
			HXAssistedLatPLRIterIndex( 0 ),
			HXAssistedLatPLRFail( 0 ),
			HXAssistedLatPLRFailIndex( 0 ),
			HXAssistedCRLatPLRIter( 0 ),
			HXAssistedCRLatPLRIterIndex( 0 ),
			HXAssistedCRLatPLRFail( 0 ),
			HXAssistedCRLatPLRFailIndex( 0 ),
			HXAssistedCRLatPLRFail2( 0 ),
			HXAssistedCRLatPLRFailIndex2( 0 ),
			DXCoilSensPLRIter( 0 ),
			DXCoilSensPLRIterIndex( 0 ),
			DXCoilSensPLRFail( 0 ),
			DXCoilSensPLRFailIndex( 0 ),
			DXCoilLatPLRIter( 0 ),
			DXCoilLatPLRIterIndex( 0 ),
			DXCoilLatPLRFail( 0 ),
			DXCoilLatPLRFailIndex( 0 ),
			MSpdSensPLRIter( 0 ),
			MSpdSensPLRIterIndex( 0 ),
			MSpdCycSensPLRIter( 0 ),
			MSpdCycSensPLRIterIndex( 0 ),
			MSpdLatPLRIter( 0 ),
			MSpdLatPLRIterIndex( 0 ),
			MSpdCycLatPLRIter( 0 ),
			MSpdCycLatPLRIterIndex( 0 ),
			MModeSensPLRIter( 0 ),
			MModeSensPLRIterIndex( 0 ),
			MModeLatPLRIter( 0 ),
			MModeLatPLRIterIndex( 0 ),
			MModeLatPLRIter2( 0 ),
			MModeLatPLRIterIndex2( 0 ),
			OAUnitSetTemp( 0.0 ),
			ISHundredPercentDOASDXCoil( false ),
			DOASDXCoolingCoilMinTout( 0.0 ),
			FrostControlStatus( 0 ),
			SpeedNum( 0 ),
			TESOpMode( 0 )
		{}

	};

	// Object Data
	extern Array1D< DXCoolingConditions > DXCoolingSystem;

	// Functions

	void
	clear_state();

	void
	SimDXCoolingSystem(
		std::string const & DXCoolingSystemName, // Name of DXSystem:Airloop object
		bool const FirstHVACIteration, // True when first HVAC iteration
		int const AirLoopNum, // Primary air loop number
		int & CompIndex, // Index to DXSystem:Airloop object
		Optional_int_const OAUnitNum = _, // If the system is an equipment of OutdoorAirUnit
		Optional< Real64 const > OAUCoilOutTemp = _, // the coil inlet temperature of OutdoorAirUnit
		Optional< Real64 > QTotOut = _ // the total cooling output of unit
	);

	// Get Input Section of the Module
	//******************************************************************************

	void
	GetDXCoolingSystemInput();

	// End of Get Input subroutines for the Module
	//******************************************************************************

	// Beginning of Initialization subroutines for the Module
	// *****************************************************************************

	void
	InitDXCoolingSystem(
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
	ControlDXSystem(
		int const DXSystemNum, // index to DXSystem
		bool const FirstHVACIteration, // First HVAC iteration flag
		bool & HXUnitOn // flag to enable heat exchanger heat recovery
	);

	Real64
	DXCoilVarSpeedResidual(
		Real64 const SpeedRatio, // compressor speed ratio (1.0 is max, 0.0 is min)
		Array1< Real64 > const & Par // par(1) = DX coil number
	);

	Real64
	DXCoilVarSpeedHumRatResidual(
		Real64 const SpeedRatio, // compressor speed ratio (1.0 is max, 0.0 is min)
		Array1< Real64 > const & Par // par(1) = DX coil number
	);

	Real64
	DXCoilCyclingResidual(
		Real64 const CycRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	);

	Real64
	DXCoilCyclingHumRatResidual(
		Real64 const CycRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	);

	Real64
	DOE2DXCoilResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	);

	Real64
	DOE2DXCoilHumRatResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	);

	Real64
	MultiModeDXCoilResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	);

	Real64
	MultiModeDXCoilHumRatResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	);

	Real64
	HXAssistedCoolCoilTempResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	);

	Real64
	HXAssistedCoolCoilHRResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	);

	Real64
	TESCoilResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	);

	Real64
	TESCoilHumRatResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	);

	void
	FrostControlSetPointLimit(
		int const DXSystemNum, // dx cooling coil system index
		Real64 & TempSetPoint, // temperature setpoint of the sensor node
		Real64 & HumRatSetPoint, // humidity ratio setpoint of the sensor node
		Real64 const BaroPress, // baromtric pressure, Pa [N/m^2]
		Real64 const TfrostControl, // minimum temperature limit for forst control
		int const ControlMode // temperature or humidity control mode
	);

	void
	CheckDXCoolingCoilInOASysExists( std::string const & DXCoilSysName );

	void
	GetCoolingCoilTypeNameAndIndex(
		std::string const & DXCoilSysName,
		int & CoolCoilType,
		int & CoolCoilIndex,
		std::string & CoolCoilName,
		bool & ErrFound
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

	Real64
	VSCoilCyclingHumResidual(
		Real64 const PartLoadRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	);

	//******************************************************************************

	Real64
	VSCoilSpeedHumResidual(
		Real64 const SpeedRatio, // compressor cycling ratio (1.0 is continuous, 0.0 is off)
		Array1< Real64 > const & Par // par(1) = DX coil number
	);

	//        End of Calculation subroutines for the DXCoolingSystem Module
	// *****************************************************************************

} // HVACDXSystem

} // EnergyPlus

#endif
