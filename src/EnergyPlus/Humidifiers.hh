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


	//     NOTICE

	//     Copyright (c) 1996-2014 The Board of Trustees of the University of Illinois
	//     and The Regents of the University of California through Ernest Orlando Lawrence
	//     Berkeley National Laboratory.  All rights reserved.

	//     Portions of the EnergyPlus software package have been developed and copyrighted
	//     by other individuals, companies and institutions.  These portions have been
	//     incorporated into the EnergyPlus software package under license.   For a complete
	//     list of contributors, see "Notice" located in main.cc.

	//     NOTICE: The U.S. Government is granted for itself and others acting on its
	//     behalf a paid-up, nonexclusive, irrevocable, worldwide license in this data to
	//     reproduce, prepare derivative works, and perform publicly and display publicly.
	//     Beginning five (5) years after permission to assert copyright is granted,
	//     subject to two possible five year renewals, the U.S. Government is granted for
	//     itself and others acting on its behalf a paid-up, non-exclusive, irrevocable
	//     worldwide license in this data to reproduce, prepare derivative works,
	//     distribute copies to the public, perform publicly and display publicly, and to
	//     permit others to do so.

	//     TRADEMARKS: EnergyPlus is a trademark of the US Department of Energy.

} // Humidifiers

} // EnergyPlus

#endif
