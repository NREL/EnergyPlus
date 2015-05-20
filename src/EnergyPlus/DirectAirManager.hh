#ifndef DirectAirManager_hh_INCLUDED
#define DirectAirManager_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace DirectAirManager {

	// Using/Aliasing

	// Data
	//MODULE PARAMETER DEFINITIONS:

	//Type declarations in DirectAir module

	//MODULE VARIABLE DECLARATIONS:
	extern int NumDirectAir;
	extern Array1D_bool CheckEquipName;

	//SUBROUTINE SPECIFICATIONS FOR MODULE AirLoopSplitter

	// Types

	struct DirectAirProps
	{
		// Members
		// Input Data
		std::string cObjectName;
		std::string EquipID;
		std::string Schedule;
		int ZoneSupplyAirNode;
		int SchedPtr;
		Real64 MaxAirVolFlowRate; // Max Specified Volume Flow Rate of Sys [m3/sec]
		Real64 AirMassFlowRateMax; // Max mass flow [kg/sec]
		Real64 InitMaxAvailMassFlow; // The Initial max mass Flow to set the Control Flow Fraction
		Real64 AirMassFlowFraction;
		int ZoneEquipAirInletNode;
		// Simulation Data
		Real64 SensOutputProvided;
		bool EMSOverrideAirFlow; // if true, EMS is calling to override flow rate
		Real64 EMSMassFlowRateValue; // value EMS is directing to use for flow rate [kg/s]
		//Reporting Variables
		Real64 HeatRate;
		Real64 CoolRate;
		Real64 HeatEnergy;
		Real64 CoolEnergy;

		// Default Constructor
		DirectAirProps() :
			ZoneSupplyAirNode( 0 ),
			SchedPtr( 0 ),
			MaxAirVolFlowRate( 0.0 ),
			AirMassFlowRateMax( 0.0 ),
			InitMaxAvailMassFlow( 0.0 ),
			AirMassFlowFraction( 0.0 ),
			ZoneEquipAirInletNode( 0 ),
			SensOutputProvided( 0.0 ),
			EMSOverrideAirFlow( false ),
			EMSMassFlowRateValue( 0.0 ),
			HeatRate( 0.0 ),
			CoolRate( 0.0 ),
			HeatEnergy( 0.0 ),
			CoolEnergy( 0.0 )
		{}

		// Member Constructor
		DirectAirProps(
			std::string const & cObjectName,
			std::string const & EquipID,
			std::string const & Schedule,
			int const ZoneSupplyAirNode,
			int const SchedPtr,
			Real64 const MaxAirVolFlowRate, // Max Specified Volume Flow Rate of Sys [m3/sec]
			Real64 const AirMassFlowRateMax, // Max mass flow [kg/sec]
			Real64 const InitMaxAvailMassFlow, // The Initial max mass Flow to set the Control Flow Fraction
			Real64 const AirMassFlowFraction,
			int const ZoneEquipAirInletNode,
			Real64 const SensOutputProvided,
			bool const EMSOverrideAirFlow, // if true, EMS is calling to override flow rate
			Real64 const EMSMassFlowRateValue, // value EMS is directing to use for flow rate [kg/s]
			Real64 const HeatRate,
			Real64 const CoolRate,
			Real64 const HeatEnergy,
			Real64 const CoolEnergy
		) :
			cObjectName( cObjectName ),
			EquipID( EquipID ),
			Schedule( Schedule ),
			ZoneSupplyAirNode( ZoneSupplyAirNode ),
			SchedPtr( SchedPtr ),
			MaxAirVolFlowRate( MaxAirVolFlowRate ),
			AirMassFlowRateMax( AirMassFlowRateMax ),
			InitMaxAvailMassFlow( InitMaxAvailMassFlow ),
			AirMassFlowFraction( AirMassFlowFraction ),
			ZoneEquipAirInletNode( ZoneEquipAirInletNode ),
			SensOutputProvided( SensOutputProvided ),
			EMSOverrideAirFlow( EMSOverrideAirFlow ),
			EMSMassFlowRateValue( EMSMassFlowRateValue ),
			HeatRate( HeatRate ),
			CoolRate( CoolRate ),
			HeatEnergy( HeatEnergy ),
			CoolEnergy( CoolEnergy )
		{}

	};

	// Object Data
	extern Array1D< DirectAirProps > DirectAir;

	// Functions

	void
	SimDirectAir(
		std::string const & EquipName,
		int const ControlledZoneNum,
		bool const FirstHVACIteration,
		Real64 & SensOutputProvided,
		Real64 & LatOutputProvided, // Latent output provided (kg/s), dehumidification = negative
		int & CompIndex
	);

	void
	GetDirectAirInput();

	// Beginning Initialization Section of the Module
	//******************************************************************************

	void
	InitDirectAir(
		int const DirectAirNum,
		bool const FirstHVACIteration
	);

	void
	SizeDirectAir( int const DirectAirNum );

	// End Initialization Section of the Module
	//******************************************************************************

	void
	CalcDirectAir(
		int const DirectAirNum,
		int const ControlledZoneNum,
		Real64 & SensOutputProvided,
		Real64 & LatOutputProvided // Latent output provided, kg/s, dehumidification = negative
	);

	void
	ReportDirectAir( int & DirectAirNum );

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

} // DirectAirManager

} // EnergyPlus

#endif
