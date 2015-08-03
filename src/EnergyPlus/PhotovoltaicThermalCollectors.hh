#ifndef PhotovoltaicThermalCollectors_hh_INCLUDED
#define PhotovoltaicThermalCollectors_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace PhotovoltaicThermalCollectors {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS:
	extern int const SimplePVTmodel;
	extern int const LayerByLayerPVTmodel;

	extern int const ScheduledThermEffic; // mode for thermal efficiency is to use schedule
	extern int const FixedThermEffic; // mode for thermal efficiency is to use fixed value

	extern int const LiquidWorkingFluid;
	extern int const AirWorkingFluid;

	extern int const CalledFromPlantLoopEquipMgr;
	extern int const CalledFromOutsideAirSystem;

	extern Real64 const SimplePVTWaterSizeFactor; // [ m3/s/m2 ] average of collectors in SolarCollectors.idf

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	extern Array1D_bool CheckEquipName;
	extern int NumPVT; // count of all types of PVT in input file
	extern int NumSimplePVTPerform; // count of simple PVT performance objects in input file

	// SUBROUTINE SPECIFICATIONS FOR MODULE:
	// Driver/Manager Routines

	// Utility routines for module
	// these would be public such as:
	//PUBLIC  GetPVTIncidentSolarForInternalPVLayer
	//PUBLIC  GetPVTCellTemp

	// Types

	struct SimplePVTModelStruct
	{
		// Members
		std::string Name;
		Real64 ThermalActiveFract; // fraction of surface area with active thermal collection
		int ThermEfficMode; // setting for how therm effic is determined
		Real64 ThermEffic; // fixed or current Therm efficiency
		int ThermEffSchedNum; // pointer to schedule for therm effic (if any)
		Real64 SurfEmissivity; // surface emittance in long wave IR
		Real64 LastCollectorTemp; // store previous temperature
		Real64 CollectorTemp; // average solar collector temp.

		// Default Constructor
		SimplePVTModelStruct() :
			ThermalActiveFract( 0.0 ),
			ThermEfficMode( 0 ),
			ThermEffic( 0.0 ),
			ThermEffSchedNum( 0 ),
			SurfEmissivity( 0.0 ),
			LastCollectorTemp( 0.0 ),
			CollectorTemp( 0.0 )
		{}
	};

	struct PVTReportStruct
	{
		// Members
		Real64 ThermEfficiency; // Thermal efficiency of solar energy conversion
		Real64 ThermPower; // Heat gain or loss to collector fluid (W)
		Real64 ThermHeatGain; // Heat gain to collector fluid (W)
		Real64 ThermHeatLoss; // Heat loss from collector fluid (W)
		Real64 ThermEnergy; // Energy gained (or lost) to collector fluid (J)
		Real64 MdotWorkFluid; // working fluid mass flow rate (kg/s)
		Real64 TinletWorkFluid; // working fluid inlet temp (C)
		Real64 ToutletWorkFluid; // working fluid outlet temp (C)
		Real64 BypassStatus; // 0 = no bypass, 1=full bypass

		// Default Constructor
		PVTReportStruct() :
			ThermEfficiency( 0.0 ),
			ThermPower( 0.0 ),
			ThermHeatGain( 0.0 ),
			ThermHeatLoss( 0.0 ),
			ThermEnergy( 0.0 ),
			MdotWorkFluid( 0.0 ),
			TinletWorkFluid( 0.0 ),
			ToutletWorkFluid( 0.0 ),
			BypassStatus( 0.0 )
		{}
	};

	struct PVTCollectorStruct
	{
		// Members
		// input
		std::string Name; // Name of PVT collector
		int TypeNum; // Plant Side Connection: 'TypeOf_Num' assigned in DataPlant  !DSU
		int WLoopNum; // Water plant loop index number                      !DSU
		int WLoopSideNum; // Water plant loop side index                        !DSU
		int WLoopBranchNum; // Water plant loop branch index                      !DSU
		int WLoopCompNum; // Water plant loop component index                   !DSU
		bool EnvrnInit; // manage begin environmen inits
		bool SizingInit; // manage when sizing is complete
		std::string PVTModelName; // Name of PVT performance object
		int PVTModelType; // model type indicator, only simple avail now
		int SurfNum; // surface index
		std::string PVname; // named Generator:Photovoltaic object
		int PVnum; // PV index
		bool PVfound; // init, need to delay get input until PV gotten
		// INTEGER                      :: PlantLoopNum       = 0  ! needed for sizing and control
		// INTEGER                      :: PlantLoopSide      = 0  ! needed for sizing, demand vs. supply sided
		SimplePVTModelStruct Simple; // performance data structure.
		int WorkingFluidType;
		int PlantInletNodeNum;
		int PlantOutletNodeNum;
		int HVACInletNodeNum;
		int HVACOutletNodeNum;
		Real64 DesignVolFlowRate;
		bool DesignVolFlowRateWasAutoSized; // true if design volume flow rate was autosize on input
		Real64 MaxMassFlowRate;
		Real64 MassFlowRate; // DSU
		Real64 AreaCol;
		bool BypassDamperOff;
		bool CoolingUseful;
		bool HeatingUseful;
		PVTReportStruct Report;

		// Default Constructor
		PVTCollectorStruct() :
			WLoopNum( 0 ),
			WLoopSideNum( 0 ),
			WLoopBranchNum( 0 ),
			WLoopCompNum( 0 ),
			EnvrnInit( true ),
			SizingInit( true ),
			PVTModelType( 0 ),
			SurfNum( 0 ),
			PVnum( 0 ),
			PVfound( false ),
			WorkingFluidType( 0 ),
			PlantInletNodeNum( 0 ),
			PlantOutletNodeNum( 0 ),
			HVACInletNodeNum( 0 ),
			HVACOutletNodeNum( 0 ),
			DesignVolFlowRate( 0.0 ),
			DesignVolFlowRateWasAutoSized( false ),
			MaxMassFlowRate( 0.0 ),
			MassFlowRate( 0.0 ),
			AreaCol( 0.0 ),
			BypassDamperOff( true ),
			CoolingUseful( false ),
			HeatingUseful( false )
		{}
	};

	// Object Data
	extern Array1D< PVTCollectorStruct > PVT;

	// Functions

	void
	SimPVTcollectors(
		int & PVTnum, // index to PVT array.
		bool const FirstHVACIteration,
		int const CalledFrom,
		Optional_string_const PVTName = _,
		Optional_bool_const InitLoopEquip = _
	);

	void
	GetPVTcollectorsInput();

	void
	InitPVTcollectors(
		int const PVTnum,
		bool const FirstHVACIteration
	);

	void
	SizePVT( int const PVTnum );

	void
	ControlPVTcollector( int const PVTnum );

	void
	CalcPVTcollectors( int const PVTnum );

	void
	UpdatePVTcollectors( int const PVTnum );

	void
	GetPVTThermalPowerProduction(
		int const PVindex, // index of PV generator (not PVT collector)
		Real64 & ThermalPower,
		Real64 & ThermalEnergy
	);

	//=====================  Utility/Other routines for module.
	// Insert as appropriate

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

} // PhotovoltaicThermalCollectors

} // EnergyPlus

#endif
