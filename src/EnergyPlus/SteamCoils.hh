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

#ifndef SteamCoils_hh_INCLUDED
#define SteamCoils_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace SteamCoils {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS:
	extern int const SteamCoil_AirHeating;
	extern int const TemperatureSetPointControl;
	extern int const ZoneLoadControl;

	// DERIVED TYPE DEFINITIONS

	// INTERFACE DEFINITIONS
	// MODULE VARIABLE DECLARATIONS:
	extern int SteamIndex;
	extern int NumSteamCoils; // The Number of SteamCoils found in the Input
	extern Array1D_bool MySizeFlag;
	extern Array1D_bool CoilWarningOnceFlag;
	extern Array1D_bool CheckEquipName;
	extern bool GetSteamCoilsInputFlag; // Flag set to make sure you get input once

	// Subroutine Specifications for the Module
	// Driver/Manager Routines

	// Get Input routines for module

	// Initialization routines for module

	// Algorithms for the module

	// Update routine to check convergence and update nodes

	// Reporting routines for module

	// Utility routines for module

	// Types

	struct SteamCoilEquipConditions
	{
		// Members
		std::string Name; // Name of the SteamCoil
		std::string SteamCoilTypeA; // Type of SteamCoil ie. Heating or Cooling
		int SteamCoilType; // Type of SteamCoil ie. Heating or Cooling
		int SteamCoilModel; // Type of SteamCoil ie. Simple, Detailed, etc.
		int SteamCoilType_Num;
		std::string Schedule; // SteamCoil Operation Schedule
		int SchedPtr; // Pointer to the correct schedule
		Real64 InletAirMassFlowRate; // MassFlow through the SteamCoil being Simulated [kg/s]
		Real64 OutletAirMassFlowRate; // MassFlow throught the SteamCoil being Simulated[kg/s]
		Real64 InletAirTemp; // Inlet Air Temperature Operating Condition [C]
		Real64 OutletAirTemp; // Outlet Air Temperature Operating Condition [C]
		Real64 InletAirHumRat; // Inlet Air Humidity Ratio Operating Condition
		Real64 OutletAirHumRat; // Outlet Air Humidity Ratio Calculated Condition
		Real64 InletAirEnthalpy; // Inlet Air enthalpy [J/kg]
		Real64 OutletAirEnthalpy; // Outlet Air enthalpy [J/kg]
		Real64 TotSteamCoilLoad; // Total Load on the Coil [W]
		Real64 SenSteamCoilLoad; // Sensible Load on the Coil [W]
		Real64 TotSteamHeatingCoilEnergy; // Total Heating Coil energy of the Coil [J]
		Real64 TotSteamCoolingCoilEnergy; // Total Cooling Coil energy of the Coil [J]
		Real64 SenSteamCoolingCoilEnergy; // Sensible Cooling Coil energy of the Coil [J]
		Real64 TotSteamHeatingCoilRate; // Total Heating Coil Rate on the Coil [W]
		Real64 LoopLoss; // Loss in loop due to cond return to atm pressure
		Real64 TotSteamCoolingCoilRate; // Total Cooling Coil Rate on the Coil [W]
		Real64 SenSteamCoolingCoilRate; // Sensible Cooling Coil Rate on the Coil [W]
		Real64 LeavingRelHum; // Simple Coil Latent Model requires User input for leaving RH
		Real64 DesiredOutletTemp; // Temp desired at the outlet (C)
		Real64 DesiredOutletHumRat; // Humudity Ratio desired at outlet (C)
		Real64 InletSteamTemp; // Inlet Steam Temperature [C]
		Real64 OutletSteamTemp; // Outlet Steam Temperature [C]
		Real64 InletSteamMassFlowRate; // Inlet Steam Mass Flow Rate [Kg/s]
		Real64 OutletSteamMassFlowRate; // Outlet Steam Mass Flow Rate [Kg/s]
		Real64 MaxSteamVolFlowRate; // Maximum water Volume flow rate [m3/s]
		Real64 MaxSteamMassFlowRate; // Maximum water mass flow rate [Kg/s]
		Real64 InletSteamEnthalpy; // Inlet Water Enthalpy (J/Kg)
		Real64 OutletWaterEnthalpy; // Outlet Water Enthalpy (J/kg)
		Real64 InletSteamPress; // Pressure at steam inlet (Pa)
		Real64 InletSteamQuality; // Quality of steam at inlet
		Real64 OutletSteamQuality; // Quality of steam at outlet
		Real64 DegOfSubcooling;
		Real64 LoopSubcoolReturn;
		int AirInletNodeNum; // Inlet node number at air side
		int AirOutletNodeNum; // Outlet node number at air side
		int SteamInletNodeNum; // SteamInletNodeNum
		int SteamOutletNodeNum; // SteamOutletNodeNum
		int TempSetPointNodeNum; // If applicable : node number that the temp setpoint exists.
		int TypeOfCoil; // Control of Coil , temperature or Zone load
		int FluidIndex; // Fluid index for FluidProperties (Steam)
		int LoopNum; // index for plant loop with steam coil
		int LoopSide; // index for plant loop side for steam coil
		int BranchNum; // index for plant branch for steam coil
		int CompNum; // index for plant component for steam coil
		int Coil_PlantTypeNum; // plant level index for coil type
		Real64 OperatingCapacity; // capacity of steam coil at operating conditions (W)
		bool DesiccantRegenerationCoil; // true if it is a regeneration air heating coil defined in Desiccant Dehumidifier system
		int DesiccantDehumNum; // index to desiccant dehumidifier object

		// Default Constructor
		SteamCoilEquipConditions() :
			SteamCoilType( 0 ),
			SteamCoilModel( 0 ),
			SteamCoilType_Num( 0 ),
			SchedPtr( 0 ),
			InletAirMassFlowRate( 0.0 ),
			OutletAirMassFlowRate( 0.0 ),
			InletAirTemp( 0.0 ),
			OutletAirTemp( 0.0 ),
			InletAirHumRat( 0.0 ),
			OutletAirHumRat( 0.0 ),
			InletAirEnthalpy( 0.0 ),
			OutletAirEnthalpy( 0.0 ),
			TotSteamCoilLoad( 0.0 ),
			SenSteamCoilLoad( 0.0 ),
			TotSteamHeatingCoilEnergy( 0.0 ),
			TotSteamCoolingCoilEnergy( 0.0 ),
			SenSteamCoolingCoilEnergy( 0.0 ),
			TotSteamHeatingCoilRate( 0.0 ),
			LoopLoss( 0.0 ),
			TotSteamCoolingCoilRate( 0.0 ),
			SenSteamCoolingCoilRate( 0.0 ),
			LeavingRelHum( 0.0 ),
			DesiredOutletTemp( 0.0 ),
			DesiredOutletHumRat( 0.0 ),
			InletSteamTemp( 0.0 ),
			OutletSteamTemp( 0.0 ),
			InletSteamMassFlowRate( 0.0 ),
			OutletSteamMassFlowRate( 0.0 ),
			MaxSteamVolFlowRate( 0.0 ),
			MaxSteamMassFlowRate( 0.0 ),
			InletSteamEnthalpy( 0.0 ),
			OutletWaterEnthalpy( 0.0 ),
			InletSteamPress( 0.0 ),
			InletSteamQuality( 0.0 ),
			OutletSteamQuality( 0.0 ),
			DegOfSubcooling( 0.0 ),
			LoopSubcoolReturn( 0.0 ),
			AirInletNodeNum( 0 ),
			AirOutletNodeNum( 0 ),
			SteamInletNodeNum( 0 ),
			SteamOutletNodeNum( 0 ),
			TempSetPointNodeNum( 0 ),
			TypeOfCoil( 0 ),
			FluidIndex( 0 ),
			LoopNum( 0 ),
			LoopSide( 0 ),
			BranchNum( 0 ),
			CompNum( 0 ),
			Coil_PlantTypeNum( 0 ),
			OperatingCapacity( 0.0 ),
			DesiccantRegenerationCoil( false ),
			DesiccantDehumNum( 0 )
		{}

	};

	// Object Data
	extern Array1D< SteamCoilEquipConditions > SteamCoil;

	// Functions

	void
	SimulateSteamCoilComponents(
		std::string const & CompName,
		bool const FirstHVACIteration,
		int & CompIndex,
		Optional< Real64 const > QCoilReq = _, // coil load to be met
		Optional< Real64 > QCoilActual = _, // coil load actually delivered returned to calling component
		Optional_int_const FanOpMode = _,
		Optional< Real64 const > PartLoadRatio = _
	);

	// Get Input Section of the Module

	void
	GetSteamCoilInput();

	// End of Get Input subroutines for the HB Module

	// Beginning Initialization Section of the Module

	void
	InitSteamCoil(
		int const CoilNum,
		bool const FirstHVACIteration
	);

	void
	SizeSteamCoil( int const CoilNum );

	// End Initialization Section of the Module

	// Begin Algorithm Section of the Module

	void
	CalcSteamAirCoil(
		int const CoilNum,
		Real64 const QCoilRequested, // requested coil load
		Real64 & QCoilActual, // coil load actually delivered
		int const FanOpMode, // fan operating mode
		Real64 const PartLoadRatio // part-load ratio of heating coil
	);

	// Beginning of Update subroutines for the SteamCoil Module

	void
	UpdateSteamCoil( int const CoilNum );

	// End of Update subroutines for the SteamCoil Module

	// Beginning of Reporting subroutines for the SteamCoil Module

	void
	ReportSteamCoil( int const CoilNum );

	// End of Reporting subroutines for the SteamCoil Module

	// Utility subroutines for the SteamCoil Module

	int
	GetSteamCoilIndex(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	void
	CheckSteamCoilSchedule(
		std::string const & CompType,
		std::string const & CompName,
		Real64 & Value,
		int & CompIndex
	);

	Real64
	GetCoilMaxWaterFlowRate(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	Real64
	GetCoilMaxSteamFlowRate(
		int const CoilIndex, // must match coil types in this module
		bool & ErrorsFound // set to true if problem
	);

	int
	GetCoilAirInletNode(
		int const CoilIndex, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	int
	GetCoilAirOutletNode(
		int const CoilIndex, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	int
	GetCoilAirOutletNode(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	int
	GetCoilSteamInletNode(
		int const CoilIndex, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	int
	GetCoilSteamInletNode(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	int
	GetCoilSteamOutletNode(
		int const CoilIndex, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	int
	GetCoilSteamOutletNode(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	Real64
	GetCoilCapacity(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	int
	GetTypeOfCoil(
		int const CoilIndex, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	int
	GetSteamCoilControlNodeNum(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorFlag // set to true if problem
	);

	int
	GetSteamCoilAvailScheduleIndex(
		std::string const & CoilType, // must match coil types in this module
		std::string const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	//// register that a coil is used as a regeneration air heating coil in
	//// desiccant dehumidification system
	//void
	//SetSteamCoilAsDesicRegenCoil(
	//	std::string const & CoilType, // must match coil types in this module
	//	std::string const & CoilName, // must match coil names for the coil type
	//	int & DesiccantDehumIndex, // index of desiccant dehumidifier
	//	bool & ErrorsFound // set to true if problem
	//);

	// sets data to a coil that is used as a regeneration air heating coil in
	// desiccant dehumidification system
	void
	SetSteamCoilData(
		int const CoilNum, // index of hot steam heating Coil
		bool & ErrorsFound, // Set to true if certain errors found
		Optional_bool DesiccantRegenerationCoil = _, // Flag that this coil is used as regeneration air heating coil
		Optional_int DesiccantDehumIndex = _ // Index for the desiccant dehum system where this caoil is used 
	);

	// End of Utility subroutines for the SteamCoil Module

} // SteamCoils

} // EnergyPlus

#endif
