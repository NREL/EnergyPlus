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

#ifndef Humidifiers_hh_INCLUDED
#define Humidifiers_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace Humidifiers {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS
	extern int const Humidifier_Steam_Electric;
	extern int const Humidifier_Steam_Gas;

	extern Array1D_string const HumidifierType;

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	extern int NumHumidifiers; // number of humidifiers of all types
	extern int NumElecSteamHums; // number of electric steam humidifiers
	extern int NumGasSteamHums; // number of gas steam humidifiers
	extern Array1D_bool CheckEquipName;

	// Humidifier normalized thermal efficiency curve types
	extern int const Linear;
	extern int const Quadratic;
	extern int const Cubic;
	extern int const FixedInletWaterTemperature;
	extern int const VariableInletWaterTemperature;

	// SUBROUTINE SPECIFICATIONS FOR MODULE

	// Types

	class HumidifierData
	{

	private:
	public:

		// Members
		std::string Name; // unique name of component
		//    CHARACTER(len=MaxNameLength) :: HumType           =' ' ! Type of humidifier
		int HumType_Code; // Pointer to Humidifier in list of humidifiers
		int EquipIndex; // Pointer to Humidifier in list of humidifiers
		std::string Sched; // name of availability schedule
		int SchedPtr; // index of availability schedule
		Real64 NomCapVol; // nominal capacity [m3/s of water]
		Real64 NomCap; // nominal capacity [kg/s of water]
		Real64 NomPower; // power consumption at full output [watts]
		Real64 ThermalEffRated; // rated thermal efficiency of the gas fired humidifier [-]
		Real64 CurMakeupWaterTemp; // makeup water temperature from main water [C]
		int EfficiencyCurvePtr; // index to efficiency curve
		int EfficiencyCurveType; // type of efficiency curve
		int InletWaterTempOption; // type inlet water temperature fixed or variable
		Real64 FanPower; // nominal fan power [watts]
		Real64 StandbyPower; // standby power consumption [watts]
		int AirInNode; // air inlet node of humidifier
		int AirOutNode; // air outlet node of humidifier
		Real64 AirInTemp; // inlet air temperature [C]
		Real64 AirInHumRat; // inlet air humidity ratio [kg water / kg air]
		Real64 AirInEnthalpy; // inlet air specific enthalpy [J/kg]
		Real64 AirInMassFlowRate; // inlet air mass flow rate [kg/s]
		Real64 AirOutTemp; // outlet air temperature [C]
		Real64 AirOutHumRat; // outlet air humidity ratio [kg water / kg air]
		Real64 AirOutEnthalpy; // outlet air specific enthalpy [J/kg]
		Real64 AirOutMassFlowRate; // outlet air mass flow rate [kg/s]
		Real64 HumRatSet; // humidity ratio setpoint [kg water / kg air]
		Real64 WaterAdd; // water output (and consumption) [kg/s]
		Real64 ElecUseEnergy; // electricity consumption [J]
		Real64 ElecUseRate; // electricity consumption [W]
		Real64 WaterCons; // water consumption in cubic meters
		Real64 WaterConsRate; // water consumption rate in m3/s
		bool SuppliedByWaterSystem; // true means there is storage tank, otherwise mains
		int WaterTankID; // index pointer to water storage tank
		int WaterTankDemandARRID; // index pointer to WaterStorage Demand arrays.
		Real64 TankSupplyVdot;
		Real64 TankSupplyVol;
		Real64 StarvedSupplyVdot;
		Real64 StarvedSupplyVol;
		int TankSupplyID; // index pointer to WaterStorage supply arrays.
		bool MySizeFlag;
		bool MyEnvrnFlag;
		bool MySetPointCheckFlag;
		// report variables for gas humidifier
		Real64 ThermalEff; // current actual thermal efficiency gas humidifier [-]
		Real64 GasUseRate; // gas consumption rate [W]
		Real64 GasUseEnergy; // gas energy consumption [J]
		Real64 AuxElecUseRate; // auxiliary electric power input [W]
		Real64 AuxElecUseEnergy; //  auxiliary electric energy consumption [J]'

		// Default Constructor
		HumidifierData() :
			HumType_Code( 0 ),
			EquipIndex( 0 ),
			SchedPtr( 0 ),
			NomCapVol( 0.0 ),
			NomCap( 0.0 ),
			NomPower( 0.0 ),
			ThermalEffRated( 1.0 ),
			CurMakeupWaterTemp( 0.0 ),
			EfficiencyCurvePtr( 0 ),
			EfficiencyCurveType( 0 ),
			InletWaterTempOption( 0 ),
			FanPower( 0.0 ),
			StandbyPower( 0.0 ),
			AirInNode( 0 ),
			AirOutNode( 0 ),
			AirInTemp( 0.0 ),
			AirInHumRat( 0.0 ),
			AirInEnthalpy( 0.0 ),
			AirInMassFlowRate( 0.0 ),
			AirOutTemp( 0.0 ),
			AirOutHumRat( 0.0 ),
			AirOutEnthalpy( 0.0 ),
			AirOutMassFlowRate( 0.0 ),
			HumRatSet( 0.0 ),
			WaterAdd( 0.0 ),
			ElecUseEnergy( 0.0 ),
			ElecUseRate( 0.0 ),
			WaterCons( 0.0 ),
			WaterConsRate( 0.0 ),
			SuppliedByWaterSystem( false ),
			WaterTankID( 0 ),
			WaterTankDemandARRID( 0 ),
			TankSupplyVdot( 0.0 ),
			TankSupplyVol( 0.0 ),
			StarvedSupplyVdot( 0.0 ),
			StarvedSupplyVol( 0.0 ),
			TankSupplyID( 0 ),
			MySizeFlag( true ),
			MyEnvrnFlag( true ),
			MySetPointCheckFlag( true ),
			ThermalEff( 0.0 ),
			GasUseRate( 0.0 ),
			GasUseEnergy( 0.0 ),
			AuxElecUseRate( 0.0 ),
			AuxElecUseEnergy( 0.0 )
		{}

		void
		InitHumidifier(); // number of the current humidifier being simulated

		void
		SizeHumidifier(); // number of the current humidifier being sized

		void
		ControlHumidifier(
			Real64 & WaterAddNeeded // moisture addition rate needed to meet minimum humidity ratio setpoint [kg/s]
		);

		void
		CalcElecSteamHumidifier(
			Real64 const WaterAddNeeded // moisture addition rate set by controller [kg/s]
		);

		void
		CalcGasSteamHumidifier(
			Real64 const WaterAddNeeded // moisture addition rate set by controller [kg/s]
		);

		void
		UpdateReportWaterSystem(); // number of the current humidifier being simulated

		void
		UpdateHumidifier(); // number of the current humidifier being simulated

		void
		ReportHumidifier(); // number of the current humidifier being simulated

	};

	// Object Data
	extern Array1D< HumidifierData > Humidifier;

	// Functions

	// Clears the global data in Humidifiers.
	// Needed for unit tests, should not be normally called.
	void
	clear_state();

	void
	SimHumidifier(
		std::string const & CompName, // name of the humidifier unit
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		int & CompIndex // Pointer to Humidifier Unit
	);

	void
	GetHumidifierInput();

} // Humidifiers

} // EnergyPlus

#endif
