#ifndef AirTerminalUnit_hh_INCLUDED
#define AirTerminalUnit_hh_INCLUDED

// C++ Headers
#include <string>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

// types of air terminal units, refactored from old DataDefineEquip
enum AirTerminalUnitType {
	notYetDetermined,
	dualDuctConstVolume,
	dualDuctVAV,
	singleDuctVAVReheat,
	singleDuctConstVolReheat,
	singleDuctVAVNoReheat,
	singleDuct_SeriesPIU_Reheat,
	singleDuct_ParallelPIU_Reheat,
	singleDuct_ConstVol_4PipeInduc,
	singleDuctVAVReheatVSFan,
	singleDuctCBVAVReheat,
	singleDuctCBVAVNoReheat,
	singleDuctConstVolCooledBeam,
	dualDuctVAVOutdoorAir,
	singleDuctUserDefined,
	singleDuctInletATMixer,
	singleDuctSupplyATMixer,
	singleDuctConstVolFourPipeBeam
};

// base class for all air distribution units.  zone air terminals for connecting to central air handlers
class AirTerminalUnit
{

protected: // Creation

	// Default Constructor
	AirTerminalUnit() :
		terminalType( notYetDetermined ),
		aDUNum( 0 ),
		airAvailSchedNum( 0 ),
		airAvailable( false ),
		vDotDesignPrimAir( 0.0 ),
		vDotDesignPrimAirWasAutosized( false ),
		mDotDesignPrimAir( 0.0 ),
		airInNodeNum( 0 ),
		airOutNodeNum( 0 ),
		zoneIndex( 0 ),
		zoneNodeIndex( 0 )
	{}

	// Copy Constructor
	AirTerminalUnit( AirTerminalUnit const & ) = default;

	// Move Constructor
#if !defined(_MSC_VER) || defined(__INTEL_COMPILER) || (_MSC_VER>=1900)
	AirTerminalUnit( AirTerminalUnit && ) = default;
#endif

public: // Creation

	// Destructor
	virtual
	~AirTerminalUnit()
	{}

protected: // Assignment

	// Copy Assignment
//	operator =( AirTerminalUnit const & ) = default;

	// Move Assignment
//#if !defined(_MSC_VER) || defined(__INTEL_COMPILER) || (_MSC_VER>=1900)
//	operator =( AirTerminalUnit && ) = default;
//#endif

public: // Methods		REMOVE ANY OF THESE THAT AREN'T COMMON (WITH SAME ARGS) TO ALL SUB-TYPES

	// for unit tests
	virtual
	void
	clear_state() = 0;

	virtual
	void
	simulate(
		bool const FirstHVACIteration, // TRUE if first HVAC iteration in time step
		Real64 & NonAirSysOutput // convective cooling by the beam system [W]
	) = 0;



public: // Data

	AirTerminalUnitType terminalType; // Type of air distribution unit  //Legacy For use during transition to OO
	std::string name; // name of unit
	std::string unitType; // type of unit = e.g. AirTerminal:SingleDuct:ConstantVolume:FourPipeBeam
	int aDUNum; // index of this unit in the corresponding air distribution unit structure
	int airAvailSchedNum; // index to schedule for pimary air availability
	bool airAvailable; // true if primary air is available
	Real64 vDotDesignPrimAir; // Design primary air volume flow rate m3/s (autosizable)
	bool vDotDesignPrimAirWasAutosized; // true if user input for design air flow was autsized on input
	Real64 mDotDesignPrimAir; // Design primary air mass flow rate kg/s
	int airInNodeNum; // unit air inlet system node number, air enters into air terminal unit
	int airOutNodeNum; // unit air outlet system node number, air enters into zone from air terminal
	int zoneIndex; // zone index for this air terminal unit
	int zoneNodeIndex; // index in node structure for the zone node for this air terminal
}; // AirTerminalUnit

} // EnergyPlus

#endif // AirTerminalUnit_hh_INCLUDED
