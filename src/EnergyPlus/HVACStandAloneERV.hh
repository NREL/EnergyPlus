#ifndef HVACStandAloneERV_hh_INCLUDED
#define HVACStandAloneERV_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace HVACStandAloneERV {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS

	extern int const ControllerSimple;
	extern int const ControllerOutsideAir;
	extern int const ControllerStandAloneERV;

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:

	extern int NumStandAloneERVs; // Total number of stand alone ERVs defined in the idf

	extern Array1D_bool MySizeFlag;
	extern Array1D_bool CheckEquipName;
	extern bool GetERVInputFlag; // First time, input is "gotten"

	// SUBROUTINE SPECIFICATIONS FOR MODULE

	// Driver/Manager Routine

	// Algorithms/Calculation routine for the module

	// Get Input routine for module

	// Sizing routine for the module

	// Initialization routine for module

	// Utility routines for module

	// Types

	struct StandAloneERVData
	{
		// Members
		// input data
		std::string Name; // name of the stand alone ERV unit
		std::string UnitType; // ZoneHVAC:EnergyRecoveryVentilator
		int SchedPtr; // pointer to availability schedule
		std::string HeatExchangerName; // name of the heat exchanger within the ERV unit
		int HeatExchangerIndex; // Pointer to heat exchanger
		int HeatExchangerTypeNum; // Parameter equivalent of HX object type
		int SupplyAirInletNode; // supply air inlet node for the stand alone ERV
		int SupplyAirOutletNode; // supply air outlet node for the stand alone ERV
		std::string SupplyAirFanName; // fan name in the supply air stream of the ERV
		int SupplyAirFanIndex; // index to supply air fan
		int SupplyAirFanSchPtr; // index to supply air fan schedule
		int SupplyAirFanType_Num; // parameter equivalent of fan type
		int ExhaustAirInletNode; // exhaust air inlet node for the stand alone ERV
		int ExhaustAirOutletNode; // exhaust air outlet node for the stand alone ERV
		std::string ExhaustAirFanName; // fan name in exhaust air stream of the ERV
		int ExhaustAirFanIndex; // index to exhaust air fan
		int ExhaustAirFanSchPtr; // index to exhaust air fan schedule
		int ExhaustAirFanType_Num; // paramter equivalent of fan type
		Real64 SupplyAirVolFlow; // volumetric flow rate through the supply side of the ERV
		Real64 ExhaustAirVolFlow; // volumetric flow rate through the exhaust side of the ERV
		std::string ControllerName; // name of the controller for the stand alone ERV
		bool ControllerNameDefined; // controller for the stand alone ERV is defined
		int ControlledZoneNum; // index to controlled zone for stand alone ERV
		int ControllerIndex; // Pointer for updates by routines this module calls.
		Real64 MaxSupAirMassFlow; // air mass flow rate through the supply side of the ERV
		Real64 MaxExhAirMassFlow; // air mass flow rate through the exhaust side of the ERV
		Real64 HighRHOAFlowRatio; // ratio of outside air flow to max outside air flow
		Real64 DesignSAFanVolFlowRate; // SA fan volumetric flow rate
		Real64 DesignEAFanVolFlowRate; // EA fan volumetric flow rate
		Real64 DesignHXVolFlowRate; // HX (heat exchanger) volumetric flow rate
		Real64 DesignSAFanMassFlowRate; // SA fan mass flow rate
		Real64 DesignEAFanMassFlowRate; // EA fan mass flow rate
		Real64 AirVolFlowPerFloorArea; // Air flow rate per unit floor area, used for autosizing
		Real64 AirVolFlowPerOccupant; // Air flow rate per occupant, used for autosizing
		int EconomizerOASchedPtr; // schedule to modify outdoor air
		bool FlowError; // used for one-time warning message for flow imbalance (Init)
		int AvailStatus;
		std::string AvailManagerListName; // Name of an availability manager list object
		// report variables
		Real64 ElecUseRate; // total electric use rate (power) for supply/exhaust fans & generic HX parasitics [W]
		Real64 ElecUseEnergy; // electric energy use for supply fan, exhaust fan, and generic HX parasitics [J]
		Real64 SensCoolingEnergy; // sensible cooling energy delivered by the ERV supply air to the zone [J]
		Real64 SensCoolingRate; // rate of sensible cooling delivered to the zone [W]
		Real64 LatCoolingEnergy; // latent cooling energy delivered by the ERV supply air to the zone [J]
		Real64 LatCoolingRate; // rate of latent cooling delivered to the zone [W]
		Real64 TotCoolingEnergy; // total cooling energy delivered by the ERV supply air to the zone [J]
		Real64 TotCoolingRate; // rate of total cooling delivered to the zone [W]
		Real64 SensHeatingEnergy; // sensible heating energy delivered by the ERV supply air to the zone [J]
		Real64 SensHeatingRate; // rate of sensible heating delivered to the zone [W]
		Real64 LatHeatingEnergy; // latent heating energy delivered by the ERV supply air to the zone [J]
		Real64 LatHeatingRate; // rate of latent heating delivered to the zone [W]
		Real64 TotHeatingEnergy; // total heating energy delivered by the ERV supply air to the zone [J]
		Real64 TotHeatingRate; // rate of total heating delivered to the zone [W]

		// Default Constructor
		StandAloneERVData() :
			SchedPtr( 0 ),
			HeatExchangerIndex( 0 ),
			HeatExchangerTypeNum( 0 ),
			SupplyAirInletNode( 0 ),
			SupplyAirOutletNode( 0 ),
			SupplyAirFanIndex( 0 ),
			SupplyAirFanSchPtr( 0 ),
			SupplyAirFanType_Num( 0 ),
			ExhaustAirInletNode( 0 ),
			ExhaustAirOutletNode( 0 ),
			ExhaustAirFanIndex( 0 ),
			ExhaustAirFanSchPtr( 0 ),
			ExhaustAirFanType_Num( 0 ),
			SupplyAirVolFlow( 0.0 ),
			ExhaustAirVolFlow( 0.0 ),
			ControllerNameDefined( true ),
			ControlledZoneNum( 0 ),
			ControllerIndex( 0 ),
			MaxSupAirMassFlow( 0.0 ),
			MaxExhAirMassFlow( 0.0 ),
			HighRHOAFlowRatio( 1.0 ),
			DesignSAFanVolFlowRate( 0.0 ),
			DesignEAFanVolFlowRate( 0.0 ),
			DesignHXVolFlowRate( 0.0 ),
			DesignSAFanMassFlowRate( 0.0 ),
			DesignEAFanMassFlowRate( 0.0 ),
			AirVolFlowPerFloorArea( 0.0 ),
			AirVolFlowPerOccupant( 0.0 ),
			EconomizerOASchedPtr( 0 ),
			FlowError( true ),
			AvailStatus( 0 ),
			ElecUseRate( 0.0 ),
			ElecUseEnergy( 0.0 ),
			SensCoolingEnergy( 0.0 ),
			SensCoolingRate( 0.0 ),
			LatCoolingEnergy( 0.0 ),
			LatCoolingRate( 0.0 ),
			TotCoolingEnergy( 0.0 ),
			TotCoolingRate( 0.0 ),
			SensHeatingEnergy( 0.0 ),
			SensHeatingRate( 0.0 ),
			LatHeatingEnergy( 0.0 ),
			LatHeatingRate( 0.0 ),
			TotHeatingEnergy( 0.0 ),
			TotHeatingRate( 0.0 )
		{}

		// Member Constructor
		StandAloneERVData(
			std::string const & Name, // name of the stand alone ERV unit
			std::string const & UnitType, // ZoneHVAC:EnergyRecoveryVentilator
			int const SchedPtr, // pointer to availability schedule
			std::string const & HeatExchangerName, // name of the heat exchanger within the ERV unit
			int const HeatExchangerIndex, // Pointer to heat exchanger
			int const HeatExchangerTypeNum, // Parameter equivalent of HX object type
			int const SupplyAirInletNode, // supply air inlet node for the stand alone ERV
			int const SupplyAirOutletNode, // supply air outlet node for the stand alone ERV
			std::string const & SupplyAirFanName, // fan name in the supply air stream of the ERV
			int const SupplyAirFanIndex, // index to supply air fan
			int const SupplyAirFanSchPtr, // index to supply air fan schedule
			int const SupplyAirFanType_Num, // parameter equivalent of fan type
			int const ExhaustAirInletNode, // exhaust air inlet node for the stand alone ERV
			int const ExhaustAirOutletNode, // exhaust air outlet node for the stand alone ERV
			std::string const & ExhaustAirFanName, // fan name in exhaust air stream of the ERV
			int const ExhaustAirFanIndex, // index to exhaust air fan
			int const ExhaustAirFanSchPtr, // index to exhaust air fan schedule
			int const ExhaustAirFanType_Num, // paramter equivalent of fan type
			Real64 const SupplyAirVolFlow, // volumetric flow rate through the supply side of the ERV
			Real64 const ExhaustAirVolFlow, // volumetric flow rate through the exhaust side of the ERV
			std::string const & ControllerName, // name of the controller for the stand alone ERV
			bool const ControllerNameDefined, // controller for the stand alone ERV is defined
			int const ControlledZoneNum, // index to controlled zone for stand alone ERV
			int const ControllerIndex, // Pointer for updates by routines this module calls.
			Real64 const MaxSupAirMassFlow, // air mass flow rate through the supply side of the ERV
			Real64 const MaxExhAirMassFlow, // air mass flow rate through the exhaust side of the ERV
			Real64 const HighRHOAFlowRatio, // ratio of outside air flow to max outside air flow
			Real64 const DesignSAFanVolFlowRate, // SA fan volumetric flow rate
			Real64 const DesignEAFanVolFlowRate, // EA fan volumetric flow rate
			Real64 const DesignHXVolFlowRate, // HX (heat exchanger) volumetric flow rate
			Real64 const DesignSAFanMassFlowRate, // SA fan mass flow rate
			Real64 const DesignEAFanMassFlowRate, // EA fan mass flow rate
			Real64 const AirVolFlowPerFloorArea, // Air flow rate per unit floor area, used for autosizing
			Real64 const AirVolFlowPerOccupant, // Air flow rate per occupant, used for autosizing
			int const EconomizerOASchedPtr, // schedule to modify outdoor air
			bool const FlowError, // used for one-time warning message for flow imbalance (Init)
			int const AvailStatus,
			std::string const & AvailManagerListName, // Name of an availability manager list object
			Real64 const ElecUseRate, // total electric use rate (power) for supply/exhaust fans & generic HX parasitics [W]
			Real64 const ElecUseEnergy, // electric energy use for supply fan, exhaust fan, and generic HX parasitics [J]
			Real64 const SensCoolingEnergy, // sensible cooling energy delivered by the ERV supply air to the zone [J]
			Real64 const SensCoolingRate, // rate of sensible cooling delivered to the zone [W]
			Real64 const LatCoolingEnergy, // latent cooling energy delivered by the ERV supply air to the zone [J]
			Real64 const LatCoolingRate, // rate of latent cooling delivered to the zone [W]
			Real64 const TotCoolingEnergy, // total cooling energy delivered by the ERV supply air to the zone [J]
			Real64 const TotCoolingRate, // rate of total cooling delivered to the zone [W]
			Real64 const SensHeatingEnergy, // sensible heating energy delivered by the ERV supply air to the zone [J]
			Real64 const SensHeatingRate, // rate of sensible heating delivered to the zone [W]
			Real64 const LatHeatingEnergy, // latent heating energy delivered by the ERV supply air to the zone [J]
			Real64 const LatHeatingRate, // rate of latent heating delivered to the zone [W]
			Real64 const TotHeatingEnergy, // total heating energy delivered by the ERV supply air to the zone [J]
			Real64 const TotHeatingRate // rate of total heating delivered to the zone [W]
		) :
			Name( Name ),
			UnitType( UnitType ),
			SchedPtr( SchedPtr ),
			HeatExchangerName( HeatExchangerName ),
			HeatExchangerIndex( HeatExchangerIndex ),
			HeatExchangerTypeNum( HeatExchangerTypeNum ),
			SupplyAirInletNode( SupplyAirInletNode ),
			SupplyAirOutletNode( SupplyAirOutletNode ),
			SupplyAirFanName( SupplyAirFanName ),
			SupplyAirFanIndex( SupplyAirFanIndex ),
			SupplyAirFanSchPtr( SupplyAirFanSchPtr ),
			SupplyAirFanType_Num( SupplyAirFanType_Num ),
			ExhaustAirInletNode( ExhaustAirInletNode ),
			ExhaustAirOutletNode( ExhaustAirOutletNode ),
			ExhaustAirFanName( ExhaustAirFanName ),
			ExhaustAirFanIndex( ExhaustAirFanIndex ),
			ExhaustAirFanSchPtr( ExhaustAirFanSchPtr ),
			ExhaustAirFanType_Num( ExhaustAirFanType_Num ),
			SupplyAirVolFlow( SupplyAirVolFlow ),
			ExhaustAirVolFlow( ExhaustAirVolFlow ),
			ControllerName( ControllerName ),
			ControllerNameDefined( ControllerNameDefined ),
			ControlledZoneNum( ControlledZoneNum ),
			ControllerIndex( ControllerIndex ),
			MaxSupAirMassFlow( MaxSupAirMassFlow ),
			MaxExhAirMassFlow( MaxExhAirMassFlow ),
			HighRHOAFlowRatio( HighRHOAFlowRatio ),
			DesignSAFanVolFlowRate( DesignSAFanVolFlowRate ),
			DesignEAFanVolFlowRate( DesignEAFanVolFlowRate ),
			DesignHXVolFlowRate( DesignHXVolFlowRate ),
			DesignSAFanMassFlowRate( DesignSAFanMassFlowRate ),
			DesignEAFanMassFlowRate( DesignEAFanMassFlowRate ),
			AirVolFlowPerFloorArea( AirVolFlowPerFloorArea ),
			AirVolFlowPerOccupant( AirVolFlowPerOccupant ),
			EconomizerOASchedPtr( EconomizerOASchedPtr ),
			FlowError( FlowError ),
			AvailStatus( AvailStatus ),
			AvailManagerListName( AvailManagerListName ),
			ElecUseRate( ElecUseRate ),
			ElecUseEnergy( ElecUseEnergy ),
			SensCoolingEnergy( SensCoolingEnergy ),
			SensCoolingRate( SensCoolingRate ),
			LatCoolingEnergy( LatCoolingEnergy ),
			LatCoolingRate( LatCoolingRate ),
			TotCoolingEnergy( TotCoolingEnergy ),
			TotCoolingRate( TotCoolingRate ),
			SensHeatingEnergy( SensHeatingEnergy ),
			SensHeatingRate( SensHeatingRate ),
			LatHeatingEnergy( LatHeatingEnergy ),
			LatHeatingRate( LatHeatingRate ),
			TotHeatingEnergy( TotHeatingEnergy ),
			TotHeatingRate( TotHeatingRate )
		{}

	};

	// Object Data
	extern Array1D< StandAloneERVData > StandAloneERV;

	// Functions

	void
	SimStandAloneERV(
		std::string const & CompName, // name of the Stand Alone ERV unit
		int const ZoneNum, // number of zone being served unused1208
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 & SensLoadMet, // net sensible load supplied by the ERV unit to the zone (W)
		Real64 & LatLoadMet, // net latent load supplied by ERV unit to the zone (kg/s),
		int & CompIndex // pointer to correct component
	);

	void
	GetStandAloneERV();

	void
	InitStandAloneERV(
		int const StandAloneERVNum, // number of the current Stand Alone ERV unit being simulated
		int const ZoneNum, // number of zone being served unused1208
		bool const FirstHVACIteration // TRUE if first HVAC iteration
	);

	void
	SizeStandAloneERV( int const StandAloneERVNum );

	void
	CalcStandAloneERV(
		int const StandAloneERVNum, // Unit index in ERV data structure
		bool const FirstHVACIteration, // flag for 1st HVAC iteration in the time step
		Real64 & SensLoadMet, // sensible zone load met by unit (W)
		Real64 & LatentMassLoadMet // latent zone load met by unit (kg/s), dehumid = negative
	);

	void
	ReportStandAloneERV( int const StandAloneERVNum ); // number of the current Stand Alone ERV being simulated

	//        End of Reporting subroutines for the Module

	//        Utility subroutines/functions for the HeatingCoil Module

	Real64
	GetSupplyAirFlowRate(
		std::string const & ERVType, // must be "ZoneHVAC:EnergyRecoveryVentilator"
		std::string const & ERVCtrlName, // must match a controller name in the ERV data structure
		bool & ErrorsFound // set to true if problem
	);

	int
	GetSupplyAirInletNode(
		std::string const & ERVType, // must be "ZoneHVAC:EnergyRecoveryVentilator"
		std::string const & ERVCtrlName, // must match a controller name in the ERV data structure
		bool & ErrorsFound // set to true if problem
	);

	int
	GetExhaustAirInletNode(
		std::string const & ERVType, // must be "ZoneHVAC:EnergyRecoveryVentilator"
		std::string const & ERVCtrlName, // must match a controller name in the ERV data structure
		bool & ErrorsFound // set to true if problem
	);

	int
	GetStandAloneERVOutAirNode( int const StandAloneERVNum );

	int
	GetStandAloneERVZoneInletAirNode( int const StandAloneERVNum );

	int
	GetStandAloneERVReturnAirNode( int const StandAloneERVNum );

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

} // HVACStandAloneERV

} // EnergyPlus

#endif
