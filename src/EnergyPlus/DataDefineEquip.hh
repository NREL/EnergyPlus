#ifndef DataDefineEquip_hh_INCLUDED
#define DataDefineEquip_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace DataDefineEquip {

	// Using/Aliasing

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	//MODULE PARAMETER DEFINITIONS
	extern int const MaxZoneAirComponents;
	//INTEGER, PARAMETER :: MaxZoneAirControls = 4
	// Equipment Types covered by ZoneAirLoopEquipment:
	extern int const DualDuctConstVolume;
	extern int const DualDuctVAV;
	extern int const SingleDuctVAVReheat;
	extern int const SingleDuctConstVolReheat;
	extern int const SingleDuctVAVNoReheat;
	extern int const SingleDuct_SeriesPIU_Reheat;
	extern int const SingleDuct_ParallelPIU_Reheat;
	extern int const SingleDuct_ConstVol_4PipeInduc;
	extern int const SingleDuctVAVReheatVSFan;
	extern int const SingleDuctCBVAVReheat;
	extern int const SingleDuctCBVAVNoReheat;
	extern int const SingleDuctConstVolCooledBeam;
	extern int const DualDuctVAVOutdoorAir;
	extern int const SingleDuctUserDefined;
	extern int const SingleDuctInletATMixer;
	extern int const SingleDuctSupplyATMixer;

	// DERIVED TYPE DEFINITIONS

	//MODULE VARIABLE DECLARATIONS:
	// components of air system
	extern int NumAirDistUnits;

	// Types

	struct ZoneAirEquip
	{
		// Members
		std::string Name; // Name or identifier of this piece of equipment
		int OutletNodeNum; // index of outlet node
		int NumComponents; // number of subcomponents (=1)
		int NumControls; // number of controls (not used; =0)
		Array1D_string EquipType; // Pointer indentifying type of subcomponent
		Array1D_int EquipType_Num;
		Array1D_string EquipName; // name of subcomponent
		Array1D_int EquipIndex;
		Real64 UpStreamLeakFrac; // upstream nominal leakage fraction
		Real64 DownStreamLeakFrac; // downstream constant leakage fraction
		Real64 MassFlowRateUpStrLk; // current air mass flow rate of the upstream leak [kg/s]
		Real64 MassFlowRateDnStrLk; // current air mass flow rate of the downstream leak [kg/s]
		Real64 MassFlowRateTU; // current air mass flow rate tjrough the terminal unit [kg/s]
		Real64 MassFlowRateZSup; // current air mass flow rate of zone supply air [kg/s]
		Real64 MassFlowRateSup; // current air mass flow rate of supply air upstream of upstream leak [kg/s]
		Real64 MaxAvailDelta; // change in max avail mass low rate due to leaks [kg/s]
		Real64 MinAvailDelta; // change in min avail mass low rate due to leaks [kg/s]
		int InletNodeNum; // index of inlet node
		int ZoneEqNum; // index of zone equipment object for this terminal unit
		Real64 LeakLoadMult; // zome load multiplier to adjust for downstream leak
		bool UpStreamLeak; // if true, there is an upstream leak
		bool DownStreamLeak; // if true, there is an downstream leak

		// Default Constructor
		ZoneAirEquip() :
			OutletNodeNum( 0 ),
			NumComponents( 0 ),
			NumControls( 0 ),
			EquipType( MaxZoneAirComponents ),
			EquipType_Num( MaxZoneAirComponents, 0 ),
			EquipName( MaxZoneAirComponents ),
			EquipIndex( MaxZoneAirComponents, 0 ),
			UpStreamLeakFrac( 0.0 ),
			DownStreamLeakFrac( 0.0 ),
			MassFlowRateUpStrLk( 0.0 ),
			MassFlowRateDnStrLk( 0.0 ),
			MassFlowRateTU( 0.0 ),
			MassFlowRateZSup( 0.0 ),
			MassFlowRateSup( 0.0 ),
			MaxAvailDelta( 0.0 ),
			MinAvailDelta( 0.0 ),
			InletNodeNum( 0 ),
			ZoneEqNum( 0 ),
			LeakLoadMult( 0.0 ),
			UpStreamLeak( false ),
			DownStreamLeak( false )
		{}

		// Member Constructor
		ZoneAirEquip(
			std::string const & Name, // Name or identifier of this piece of equipment
			int const OutletNodeNum, // index of outlet node
			int const NumComponents, // number of subcomponents (=1)
			int const NumControls, // number of controls (not used; =0)
			Array1_string const & EquipType, // Pointer indentifying type of subcomponent
			Array1_int const & EquipType_Num,
			Array1_string const & EquipName, // name of subcomponent
			Array1_int const & EquipIndex,
			Real64 const UpStreamLeakFrac, // upstream nominal leakage fraction
			Real64 const DownStreamLeakFrac, // downstream constant leakage fraction
			Real64 const MassFlowRateUpStrLk, // current air mass flow rate of the upstream leak [kg/s]
			Real64 const MassFlowRateDnStrLk, // current air mass flow rate of the downstream leak [kg/s]
			Real64 const MassFlowRateTU, // current air mass flow rate tjrough the terminal unit [kg/s]
			Real64 const MassFlowRateZSup, // current air mass flow rate of zone supply air [kg/s]
			Real64 const MassFlowRateSup, // current air mass flow rate of supply air upstream of upstream leak [kg/s]
			Real64 const MaxAvailDelta, // change in max avail mass low rate due to leaks [kg/s]
			Real64 const MinAvailDelta, // change in min avail mass low rate due to leaks [kg/s]
			int const InletNodeNum, // index of inlet node
			int const ZoneEqNum, // index of zone equipment object for this terminal unit
			Real64 const LeakLoadMult, // zome load multiplier to adjust for downstream leak
			bool const UpStreamLeak, // if true, there is an upstream leak
			bool const DownStreamLeak // if true, there is an downstream leak
		) :
			Name( Name ),
			OutletNodeNum( OutletNodeNum ),
			NumComponents( NumComponents ),
			NumControls( NumControls ),
			EquipType( MaxZoneAirComponents, EquipType ),
			EquipType_Num( MaxZoneAirComponents, EquipType_Num ),
			EquipName( MaxZoneAirComponents, EquipName ),
			EquipIndex( MaxZoneAirComponents, EquipIndex ),
			UpStreamLeakFrac( UpStreamLeakFrac ),
			DownStreamLeakFrac( DownStreamLeakFrac ),
			MassFlowRateUpStrLk( MassFlowRateUpStrLk ),
			MassFlowRateDnStrLk( MassFlowRateDnStrLk ),
			MassFlowRateTU( MassFlowRateTU ),
			MassFlowRateZSup( MassFlowRateZSup ),
			MassFlowRateSup( MassFlowRateSup ),
			MaxAvailDelta( MaxAvailDelta ),
			MinAvailDelta( MinAvailDelta ),
			InletNodeNum( InletNodeNum ),
			ZoneEqNum( ZoneEqNum ),
			LeakLoadMult( LeakLoadMult ),
			UpStreamLeak( UpStreamLeak ),
			DownStreamLeak( DownStreamLeak )
		{}

	};

	// Object Data
	extern Array1D< ZoneAirEquip > AirDistUnit; // Used to specify zone related

} // DataDefineEquip

} // EnergyPlus

#endif
