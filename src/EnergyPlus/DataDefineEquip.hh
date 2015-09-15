#ifndef DataDefineEquip_hh_INCLUDED
#define DataDefineEquip_hh_INCLUDED

// C++ Headers
#include <memory>
// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>
#include <AirTerminalUnit.hh>

namespace EnergyPlus {

namespace DataDefineEquip {

	// Using/Aliasing

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	//MODULE PARAMETER DEFINITIONS
	extern int const MaxZoneAirComponents;

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
	extern int const SingleDuctConstVolFourPipeBeam;

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
		///// Note use of shared_ptr here is not a good pattern, not to be replicated without further discussion.
		std::shared_ptr< AirTerminalUnit > airTerminalPtr;
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
		int ZoneNum; // index of the zone object for this terminal unit
	 	bool AccountForDOAS; // if true user has asked for DOAS
		Real64 HeatRate; // [W]
		Real64 CoolRate; // [W]
		Real64 HeatGain; // [J]
		Real64 CoolGain; // [J]

		// Default Constructor
		ZoneAirEquip() :
			OutletNodeNum( 0 ),
			NumComponents( 0 ),
			NumControls( 0 ),
			EquipType( MaxZoneAirComponents ),
			EquipType_Num( MaxZoneAirComponents, 0 ),
			airTerminalPtr( nullptr ),
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
			DownStreamLeak( false ),
			ZoneNum( 0 ),
			AccountForDOAS( false ),
			HeatRate( 0.0 ),
			CoolRate( 0.0 ),
			HeatGain( 0.0 ),
			CoolGain( 0.0 )
		{}

	};

	// Object Data
	extern Array1D< ZoneAirEquip > AirDistUnit; // Used to specify zone related

	// Clears the global data in DataDefineEquip.
	// Needed for unit tests, should not be normally called.
	void
	clear_state();

} // DataDefineEquip

} // EnergyPlus

#endif
