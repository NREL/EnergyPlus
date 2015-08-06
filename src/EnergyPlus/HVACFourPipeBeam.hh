#ifndef HVACFourPipeBeam_hh_INCLUDED
#define HVACFourPipeBeam_hh_INCLUDED

#include <memory>
// ObjexxFCL Headers
#include <ObjexxFCL/Array1.hh>

// EnergyPlus Headers
#include <AirTerminalUnit.hh>
#include <EnergyPlus.hh>
#include <DataGlobals.hh>
#include <GlobalNames.hh>
#include <PlantLocation.hh> // this is from https://github.com/NREL/EnergyPlus/pull/4945  assuming it goes in other wise maybe the same from using DataPlant

namespace EnergyPlus {

namespace FourPipeBeam {

class HVACFourPipeBeam : public AirTerminalUnit
{

public: // Creation

	// Default Constructor
	HVACFourPipeBeam() :
		coolingAvailSchedNum( 0 ),
		coolingAvailable( false ),
		heatingAvailSchedNum( 0 ),
		heatingAvailable( false ),
		totBeamLength( 0.0 ),
		totBeamLengthWasAutosized( false ),
		vDotNormRatedPrimAir( 0.0 ),
		mDotNormRatedPrimAir( 0.0 ),
		beamCoolingPresent(  false ),
		vDotDesignCW( 0.0 ),
		vDotDesignCWWasAutosized( false ),
		mDotDesignCW( 0.0 ),
		qDotNormRatedCooling( 0.0 ),
		deltaTempRatedCooling( 0.0 ),
		vDotNormRatedCW( 0.0 ),
		mDotNormRatedCW( 0.0 ),
		modCoolingQdotDeltaTFuncNum( 0 ),
		modCoolingQdotAirFlowFuncNum( 0 ),
		modCoolingQdotCWFlowFuncNum( 0 ),
		mDotCW( 0.0 ),
		cWTempIn( 0.0 ),
		cWTempOut( 0.0 ),
		cWTempOutErrorCount( 0 ),
		cWInNodeNum( 0 ),
		cWOutNodeNum( 0 ),
		cWLocation( PlantLocation( 0, 0, 0, 0 ) ),
		beamHeatingPresent( false ),
		vDotDesignHW( 0.0 ),
		vDotDesignHWWasAutosized( false ),
		mDotDesignHW( 0.0 ),
		qDotNormRatedHeating( 0.0 ),
		deltaTempRatedHeating( 0.0 ),
		vDotNormRatedHW( 0.0 ),
		mDotNormRatedHW( 0.0 ),
		modHeatingQdotDeltaTFuncNum( 0 ),
		modHeatingQdotAirFlowFuncNum( 0 ),
		modHeatingQdotHWFlowFuncNum( 0 ),
		mDotHW( 0.0 ),
		hWTempIn( 0.0 ),
		hWTempOut( 0.0 ),
		hWTempOutErrorCount( 0 ),
		hWInNodeNum( 0 ),
		hWOutNodeNum( 0 ),
		hWLocation( PlantLocation( 0, 0, 0, 0 ) ),
		beamCoolingEnergy( 0.0 ),
		beamCoolingRate( 0.0 ),
		beamHeatingEnergy( 0.0 ),
		beamHeatingRate( 0.0 ),
		supAirCoolingEnergy( 0.0 ),
		supAirCoolingRate( 0.0 ),
		supAirHeatingEnergy( 0.0 ),
		supAirHeatingRate( 0.0 ),
		primAirFlow( 0.0 ),
		myEnvrnFlag( true ),
		mySizeFlag( true ),
		plantLoopScanFlag( true ),
		zoneEquipmentListChecked( false ),
		tDBZoneAirTemp( 0.0 ),
		tDBSystemAir( 0.0 ),
		mDotSystemAir( 0.0 ),
		cpZoneAir( 0.0 ),
		cpSystemAir( 0.0 ),
		qDotSystemAir( 0.0 ),
		qDotBeamCoolingMax( 0.0 ),
		qDotBeamHeatingMax( 0.0 ),
		qDotTotalDelivered( 0.0 ),
		qDotBeamCooling( 0.0 ),
		qDotBeamHeating( 0.0 ),
		qDotZoneReq( 0.0 ),
		qDotBeamReq( 0.0 ),
		qDotZoneToHeatSetPt( 0.0 ),
		qDotZoneToCoolSetPt( 0.0 )
	{}

	// Destructor
	virtual
	~HVACFourPipeBeam()
	{}

public: // Methods		MARK ANY THAT DON'T ALTER STATE const !!!

	static std::shared_ptr< AirTerminalUnit >
	fourPipeBeamFactory(
		int objectType,
		std::string objectName
	);

	void
	init(
		bool const FirstHVACIteration // TRUE if first air loop solution this HVAC step         MAYBE THIS SHOULD HAVE A DEFAULT ARG OF = false
	);

	void
	set_size();

	Real64
	residualSizing(
		Real64 const airFlow // primary supply air flow rate in kg/s
	);

	void
	control(
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 & NonAirSysOutput // convective cooling by the beam system [W]
	);

	void
	calc();

	Real64
	residualCooling(
		Real64 const cWaterFlow // chilled water flow rate in kg/s
	);

	Real64
	residualHeating(
		Real64 const hWaterFlow // hot water flow rate in kg/s
	);

	void
	simulate(
		bool const FirstHVACIteration, // TRUE if first HVAC iteration in time step
		int const ZoneNum, // index of zone served by the unit
		int const ZoneNodeNum, // zone node number of zone served by the unit
		Real64 & NonAirSysOutput // convective cooling by the beam system [W]
	);

	void
	update() const;

	void
	report() ;

public: // Data

	int coolingAvailSchedNum; // index to schedule for cooling availability
	bool coolingAvailable; // true if beam cooling is available
	int heatingAvailSchedNum; // index to schedule for heating availability
	bool heatingAvailable; // true if beam heating is available

	Real64 totBeamLength; // length of all the beams in the zone (autosizable) (m)
	bool totBeamLengthWasAutosized; // true if beam length was autosized on input
	Real64 vDotNormRatedPrimAir; // normalized primary air volume flow rate at rating point (m3/s-m)
	Real64 mDotNormRatedPrimAir; // normalized primary air mass flow rate at rating point (kg/s-m)
	// cooling
	bool beamCoolingPresent;  // true if chilled water system is connected to beam
	Real64 vDotDesignCW; // Design chilled water volume flow rate (autosizable) (m3/s)
	bool vDotDesignCWWasAutosized; // true if use input for chilled water flow was autosized
	Real64 mDotDesignCW; // Design chilled water mass flow rate (kg/s)
	Real64 qDotNormRatedCooling; // normalized cooling capacity at rating point (W/m)
	Real64 deltaTempRatedCooling; // temperature difference between zone air and entering chilled water at rating point (delta C)
	Real64 vDotNormRatedCW; // normalized chilled water volume flow rate at rating point (m3/s-m)
	Real64 mDotNormRatedCW; // normalized chilled water mass flow rate at rating point (kg/s-m)
	int modCoolingQdotDeltaTFuncNum; // index to curve or table modifying cooling capacity as a function of delta T ratio
	int modCoolingQdotAirFlowFuncNum; // index to curve or table modifying cooling capacity as a function of air flow ratio
	int modCoolingQdotCWFlowFuncNum; // index to curve or table modifying cooling capacity as a function of chilled water flow ratio
	Real64 mDotCW; // current chilled water mass flow rate (kg/s)
	Real64 cWTempIn; // current inlet chilled water temperature [C]
	Real64 cWTempOut; // current outlet chilled water temperature [C]
	int cWTempOutErrorCount; // counter for recurring errors in chilled water outlet temperature
	int cWInNodeNum; // chilled water inlet node
	int cWOutNodeNum; // chilled water outlet nod
	PlantLocation cWLocation; // chilled water plant loop location
	//heating
	bool beamHeatingPresent; // true if hot water system is connected to beam
	Real64 vDotDesignHW; // Design hot water volume flow rate (autosizable) (m3/s)
	bool vDotDesignHWWasAutosized; // true if user input for hot water flow was autosized
	Real64 mDotDesignHW; // Design hot water mass flow rate (kg/s)
	Real64 qDotNormRatedHeating; // normalized heating capacity at rating point (W/m)
	Real64 deltaTempRatedHeating; // temperature difference between zone air and entering hot water at rating point (delta C)
	Real64 vDotNormRatedHW; // normalized hot water volume flow rate at rating point (m3/s-m)
	Real64 mDotNormRatedHW; // normalized hot water mass flow rate at rating point (kg/s-m)
	int modHeatingQdotDeltaTFuncNum; // index to curve or table modifying heating capacity as a function of delta T ratio
	int modHeatingQdotAirFlowFuncNum; // index to curve or table modifying heating capacity as a function of air flow ratio
	int modHeatingQdotHWFlowFuncNum; // index to curve or table modifying heating capacity as a function of chilled water flow ratio
	Real64 mDotHW; // current hot water mass flow rate (kg/s)
	Real64 hWTempIn; // current inlet hot water temperature (C)
	Real64 hWTempOut; // current outlet hot water temperature (C)
	int hWTempOutErrorCount;// counter for recurring errors in hot water outlet temperature
	int hWInNodeNum; // hot water inlet node
	int hWOutNodeNum; // hot water outlet node
	PlantLocation hWLocation; // hot water connection location structure

	// output variables
	Real64 beamCoolingEnergy; // beam sensible cooling energy of all beams in the zone [J]
	Real64 beamCoolingRate; // beam sensible cooling rate of all beams in the zone (positive convention) [W]
	Real64 beamHeatingEnergy; // beam heating energy of all beams in the zone [J]
	Real64 beamHeatingRate; // beam heating rate of all beams in the zone [W]
	Real64 supAirCoolingEnergy; // Total cooling energy from supply air [J]
	Real64 supAirCoolingRate; // Total cooling rate from supply air [W]
	Real64 supAirHeatingEnergy; // Total cooling energy from supply air [J]
	Real64 supAirHeatingRate; // Total cooling rate from supply air [W]
	Real64 primAirFlow; // supply air flow per zone at standard elevation-adjusted density [m3/s]

private: // data

	bool myEnvrnFlag; // control when to re initialize for new environment period
	bool mySizeFlag; // control when to run sizing method
	bool plantLoopScanFlag; // control when to look up plant locations for water connections
	bool zoneEquipmentListChecked; // control when to check zone equipment list was input correctly


	Real64 tDBZoneAirTemp;
	Real64 tDBSystemAir;
	Real64 mDotSystemAir;
	Real64 cpZoneAir;
	Real64 cpSystemAir;
	Real64 qDotSystemAir;
	Real64 qDotBeamCoolingMax;
	Real64 qDotBeamHeatingMax;
	Real64 qDotTotalDelivered;
	Real64 qDotBeamCooling;
	Real64 qDotBeamHeating;
	Real64 qDotZoneReq;
	Real64 qDotBeamReq;
	Real64 qDotZoneToHeatSetPt;
	Real64 qDotZoneToCoolSetPt;

}; // HVACFourPipeBeam



extern Array1D< std::shared_ptr< HVACFourPipeBeam > > FourPipeBeams; // dimension to number of machines


} // FourPipeBeam

	// NOTICE

	// Copyright (c) 1996-2014 The Board of Trustees of the University of Illinois
	// and The Regents of the University of California through Ernest Orlando Lawrence
	// Berkeley National Laboratory.  All rights reserved.

	// Portions of the EnergyPlus software package have been developed and copyrighted
	// by other individuals, companies and institutions.  These portions have been
	// incorporated into the EnergyPlus software package under license.   For a complete
	// list of contributors, see "Notice" located in main.cc.

	// NOTICE: The U.S. Government is granted for itself and others acting on its
	// behalf a paid-up, nonexclusive, irrevocable, worldwide license in this data to
	// reproduce, prepare derivative works, and perform publicly and display publicly.
	// Beginning five (5) years after permission to assert copyright is granted,
	// subject to two possible five year renewals, the U.S. Government is granted for
	// itself and others acting on its behalf a paid-up, non-exclusive, irrevocable
	// worldwide license in this data to reproduce, prepare derivative works,
	// distribute copies to the public, perform publicly and display publicly, and to
	// permit others to do so.

	// TRADEMARKS: EnergyPlus is a trademark of the US Department of Energy.

} // EnergyPlus

#endif // HVACFourPipeBeam_hh_INCLUDED
