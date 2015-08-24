#ifndef DataZoneEnergyDemands_hh_INCLUDED
#define DataZoneEnergyDemands_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace DataZoneEnergyDemands {

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE VARIABLE DECLARATIONS:

	extern Array1D_bool DeadBandOrSetback; // true if zone temperature is in the thermostat deadband
	// before any heating / cooling done
	extern Array1D_bool Setback; // true if zone temperature has increased
	// from previous setting
	extern Array1D_bool CurDeadBandOrSetback; // same as above except updated after each piece of zone equipment
	// in a zone is simulated

	// Types

	struct ZoneSystemDemandData // Sensible cooling/heating loads to be met (watts)
	{
		// Members
		Real64 RemainingOutputRequired;
		Real64 TotalOutputRequired;
		Real64 OutputRequiredToHeatingSP; // Load required to meet heating setpoint (>0 is a heating load)
		Real64 OutputRequiredToCoolingSP; // Load required to meet cooling setpoint (<0 is a cooling load)
		Real64 RemainingOutputReqToHeatSP; // Remaining load required to meet heating setpoint (>0 is a heating load)
		Real64 RemainingOutputReqToCoolSP; // Remaining load required to meet cooling setpoint (<0 is a cooling load)
		int NumZoneEquipment; // count of zone equipment for this zone, from ZoneHVAC:EquipmentList
		Array1D< Real64 > SequencedOutputRequired;
		Array1D< Real64 > SequencedOutputRequiredToHeatingSP; // load required to meet heating setpoint by sequence
		Array1D< Real64 > SequencedOutputRequiredToCoolingSP; // load required to meet cooling setpoint by sequence
		Real64 SupplyAirAdjustFactor; // supply air adjustment factor due to the cap of
		// zone maximum outdoor air fraction
		int StageNum; // The stage number when staged thermostate is used:
		// 0 no load, >0 Heating stage, <0 Cooling stage

		// Default Constructor
		ZoneSystemDemandData() :
			RemainingOutputRequired( 0.0 ),
			TotalOutputRequired( 0.0 ),
			OutputRequiredToHeatingSP( 0.0 ),
			OutputRequiredToCoolingSP( 0.0 ),
			RemainingOutputReqToHeatSP( 0.0 ),
			RemainingOutputReqToCoolSP( 0.0 ),
			NumZoneEquipment( 0 ),
			SupplyAirAdjustFactor( 1.0 ),
			StageNum( 0 )
		{}

		// Member Constructor
		ZoneSystemDemandData(
			Real64 const RemainingOutputRequired,
			Real64 const TotalOutputRequired,
			Real64 const OutputRequiredToHeatingSP, // Load required to meet heating setpoint (>0 is a heating load)
			Real64 const OutputRequiredToCoolingSP, // Load required to meet cooling setpoint (<0 is a cooling load)
			Real64 const RemainingOutputReqToHeatSP, // Remaining load required to meet heating setpoint (>0 is a heating load)
			Real64 const RemainingOutputReqToCoolSP, // Remaining load required to meet cooling setpoint (<0 is a cooling load)
			int const NumZoneEquipment, // count of zone equipment for this zone, from ZoneHVAC:EquipmentList
			Array1< Real64 > const & SequencedOutputRequired,
			Array1< Real64 > const & SequencedOutputRequiredToHeatingSP, // load required to meet heating setpoint by sequence
			Array1< Real64 > const & SequencedOutputRequiredToCoolingSP, // load required to meet cooling setpoint by sequence
			Real64 const SupplyAirAdjustFactor, // supply air adjustment factor due to the cap of
			int const StageNum // The stage number when staged thermostate is used:
		) :
			RemainingOutputRequired( RemainingOutputRequired ),
			TotalOutputRequired( TotalOutputRequired ),
			OutputRequiredToHeatingSP( OutputRequiredToHeatingSP ),
			OutputRequiredToCoolingSP( OutputRequiredToCoolingSP ),
			RemainingOutputReqToHeatSP( RemainingOutputReqToHeatSP ),
			RemainingOutputReqToCoolSP( RemainingOutputReqToCoolSP ),
			NumZoneEquipment( NumZoneEquipment ),
			SequencedOutputRequired( SequencedOutputRequired ),
			SequencedOutputRequiredToHeatingSP( SequencedOutputRequiredToHeatingSP ),
			SequencedOutputRequiredToCoolingSP( SequencedOutputRequiredToCoolingSP ),
			SupplyAirAdjustFactor( SupplyAirAdjustFactor ),
			StageNum( StageNum )
		{}

	};

	struct ZoneSystemMoistureDemand // Humidification/dehumidification loads to be met (kg water per second)
	{
		// Members
		Real64 RemainingOutputRequired;
		Real64 TotalOutputRequired;
		Real64 OutputRequiredToHumidifyingSP; // Load required to meet humidifying setpoint (>0 = a humidify load)
		Real64 OutputRequiredToDehumidifyingSP; // Load required to meet dehumidifying setpoint (<0 = a dehumidify load)
		Real64 RemainingOutputReqToHumidSP; // Remaining load required to meet humidifying setpoint
		// (>0 is a humidify load)
		Real64 RemainingOutputReqToDehumidSP; // Remaining load required to meet dehumidifying setpoint
		// (<0 is a dehumidify load)
		int NumZoneEquipment; // count of zone equipment for this zone, from ZoneHVAC:EquipmentList
		Array1D< Real64 > SequencedOutputRequired;
		Array1D< Real64 > SequencedOutputRequiredToHumidSP; // load required to meet humidify setpoint by sequence
		Array1D< Real64 > SequencedOutputRequiredToDehumidSP; // load required to meet dehumidify setpoint by sequenc

		// Default Constructor
		ZoneSystemMoistureDemand() :
			RemainingOutputRequired( 0.0 ),
			TotalOutputRequired( 0.0 ),
			OutputRequiredToHumidifyingSP( 0.0 ),
			OutputRequiredToDehumidifyingSP( 0.0 ),
			RemainingOutputReqToHumidSP( 0.0 ),
			RemainingOutputReqToDehumidSP( 0.0 ),
			NumZoneEquipment( 0 )
		{}

		// Member Constructor
		ZoneSystemMoistureDemand(
			Real64 const RemainingOutputRequired,
			Real64 const TotalOutputRequired,
			Real64 const OutputRequiredToHumidifyingSP, // Load required to meet humidifying setpoint (>0 = a humidify load)
			Real64 const OutputRequiredToDehumidifyingSP, // Load required to meet dehumidifying setpoint (<0 = a dehumidify load)
			Real64 const RemainingOutputReqToHumidSP, // Remaining load required to meet humidifying setpoint
			Real64 const RemainingOutputReqToDehumidSP, // Remaining load required to meet dehumidifying setpoint
			int const NumZoneEquipment, // count of zone equipment for this zone, from ZoneHVAC:EquipmentList
			Array1< Real64 > const & SequencedOutputRequired,
			Array1< Real64 > const & SequencedOutputRequiredToHumidSP, // load required to meet humidify setpoint by sequence
			Array1< Real64 > const & SequencedOutputRequiredToDehumidSP // load required to meet dehumidify setpoint by sequenc
		) :
			RemainingOutputRequired( RemainingOutputRequired ),
			TotalOutputRequired( TotalOutputRequired ),
			OutputRequiredToHumidifyingSP( OutputRequiredToHumidifyingSP ),
			OutputRequiredToDehumidifyingSP( OutputRequiredToDehumidifyingSP ),
			RemainingOutputReqToHumidSP( RemainingOutputReqToHumidSP ),
			RemainingOutputReqToDehumidSP( RemainingOutputReqToDehumidSP ),
			NumZoneEquipment( NumZoneEquipment ),
			SequencedOutputRequired( SequencedOutputRequired ),
			SequencedOutputRequiredToHumidSP( SequencedOutputRequiredToHumidSP ),
			SequencedOutputRequiredToDehumidSP( SequencedOutputRequiredToDehumidSP )
		{}

	};

	// Object Data
	extern Array1D< ZoneSystemDemandData > ZoneSysEnergyDemand;
	extern Array1D< ZoneSystemMoistureDemand > ZoneSysMoistureDemand;

} // DataZoneEnergyDemands

} // EnergyPlus

#endif
