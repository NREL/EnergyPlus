#ifndef Humidifiers_hh_INCLUDED
#define Humidifiers_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace Humidifiers {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS
	extern int const Humidifier_Steam_Electric;

	extern FArray1D_string const HumidifierType;

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	extern int NumHumidifiers; // number of humidifiers of all types
	extern int NumElecSteamHums; // number of electric steam humidifiers
	extern FArray1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE

	// Types

	struct HumidifierData
	{
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

		// Default Constructor
		HumidifierData() :
			HumType_Code( 0 ),
			EquipIndex( 0 ),
			SchedPtr( 0 ),
			NomCapVol( 0.0 ),
			NomCap( 0.0 ),
			NomPower( 0.0 ),
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
			StarvedSupplyVol( 0.0 )
		{}

		// Member Constructor
		HumidifierData(
			std::string const & Name, // unique name of component
			int const HumType_Code, // Pointer to Humidifier in list of humidifiers
			int const EquipIndex, // Pointer to Humidifier in list of humidifiers
			std::string const & Sched, // name of availability schedule
			int const SchedPtr, // index of availability schedule
			Real64 const NomCapVol, // nominal capacity [m3/s of water]
			Real64 const NomCap, // nominal capacity [kg/s of water]
			Real64 const NomPower, // power consumption at full output [watts]
			Real64 const FanPower, // nominal fan power [watts]
			Real64 const StandbyPower, // standby power consumption [watts]
			int const AirInNode, // air inlet node of humidifier
			int const AirOutNode, // air outlet node of humidifier
			Real64 const AirInTemp, // inlet air temperature [C]
			Real64 const AirInHumRat, // inlet air humidity ratio [kg water / kg air]
			Real64 const AirInEnthalpy, // inlet air specific enthalpy [J/kg]
			Real64 const AirInMassFlowRate, // inlet air mass flow rate [kg/s]
			Real64 const AirOutTemp, // outlet air temperature [C]
			Real64 const AirOutHumRat, // outlet air humidity ratio [kg water / kg air]
			Real64 const AirOutEnthalpy, // outlet air specific enthalpy [J/kg]
			Real64 const AirOutMassFlowRate, // outlet air mass flow rate [kg/s]
			Real64 const HumRatSet, // humidity ratio setpoint [kg water / kg air]
			Real64 const WaterAdd, // water output (and consumption) [kg/s]
			Real64 const ElecUseEnergy, // electricity consumption [J]
			Real64 const ElecUseRate, // electricity consumption [W]
			Real64 const WaterCons, // water consumption in cubic meters
			Real64 const WaterConsRate, // water consumption rate in m3/s
			bool const SuppliedByWaterSystem, // true means there is storage tank, otherwise mains
			int const WaterTankID, // index pointer to water storage tank
			int const WaterTankDemandARRID, // index pointer to WaterStorage Demand arrays.
			Real64 const TankSupplyVdot,
			Real64 const TankSupplyVol,
			Real64 const StarvedSupplyVdot,
			Real64 const StarvedSupplyVol
		) :
			Name( Name ),
			HumType_Code( HumType_Code ),
			EquipIndex( EquipIndex ),
			Sched( Sched ),
			SchedPtr( SchedPtr ),
			NomCapVol( NomCapVol ),
			NomCap( NomCap ),
			NomPower( NomPower ),
			FanPower( FanPower ),
			StandbyPower( StandbyPower ),
			AirInNode( AirInNode ),
			AirOutNode( AirOutNode ),
			AirInTemp( AirInTemp ),
			AirInHumRat( AirInHumRat ),
			AirInEnthalpy( AirInEnthalpy ),
			AirInMassFlowRate( AirInMassFlowRate ),
			AirOutTemp( AirOutTemp ),
			AirOutHumRat( AirOutHumRat ),
			AirOutEnthalpy( AirOutEnthalpy ),
			AirOutMassFlowRate( AirOutMassFlowRate ),
			HumRatSet( HumRatSet ),
			WaterAdd( WaterAdd ),
			ElecUseEnergy( ElecUseEnergy ),
			ElecUseRate( ElecUseRate ),
			WaterCons( WaterCons ),
			WaterConsRate( WaterConsRate ),
			SuppliedByWaterSystem( SuppliedByWaterSystem ),
			WaterTankID( WaterTankID ),
			WaterTankDemandARRID( WaterTankDemandARRID ),
			TankSupplyVdot( TankSupplyVdot ),
			TankSupplyVol( TankSupplyVol ),
			StarvedSupplyVdot( StarvedSupplyVdot ),
			StarvedSupplyVol( StarvedSupplyVol )
		{}

	};

	// Object Data
	extern FArray1D< HumidifierData > Humidifier;

	// Functions

	void
	SimHumidifier(
		std::string const & CompName, // name of the humidifier unit
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		int & CompIndex // Pointer to Humidifier Unit
	);

	void
	GetHumidifierInput();

	void
	InitHumidifier( int const HumNum ); // number of the current humidifier being simulated

	void
	SizeHumidifier( int const HumNum ); // number of the current humidifier being sized

	void
	ControlHumidifier(
		int const HumNum, // number of the current humidifier being simulated
		Real64 & WaterAddNeeded // moisture addition rate needed to meet minimum humidity ratio setpoint [kg/s]
	);

	void
	CalcElecSteamHumidifier(
		int const HumNum, // number of the current humidifier being simulated
		Real64 const WaterAddNeeded // moisture addition rate set by controller [kg/s]
	);

	void
	UpdateReportWaterSystem( int const HumNum ); // number of the current humidifier being simulated

	void
	UpdateHumidifier( int const HumNum ); // number of the current humidifier being simulated

	void
	ReportHumidifier( int const HumNum ); // number of the current humidifier being simulated

	//     NOTICE

	//     Copyright © 1996-2014 The Board of Trustees of the University of Illinois
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
