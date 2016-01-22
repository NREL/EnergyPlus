#ifndef ChilledCeilingPanelSimple_hh_INCLUDED
#define ChilledCeilingPanelSimple_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace CoolingPanelSimple {

	// Using/Aliasing

	// Data
	//MODULE PARAMETER DEFINITIONS

	extern std::string const cCMO_CoolingPanel_Simple;
	// Control types:
	extern int const MATControl; // Controls system using mean air temperature
	extern int const MRTControl; // Controls system using mean radiant temperature
	extern int const OperativeControl; // Controls system using operative temperature
	extern int const ODBControl; // Controls system using outside air dry-bulb temperature
	extern int const OWBControl; // Controls system using outside air wet-bulb temperature
	extern int const ZoneTotalLoadControl; // Controls system using remaining zone total load
	extern int const ZoneConvectiveLoadControl; // Controls system using remaining zone convective load
	// Condensation control types:
	extern int const CondCtrlNone; // Condensation control--none, so system never shuts down
	extern int const CondCtrlSimpleOff; // Condensation control--simple off, system shuts off when condensation predicted
	extern int const CondCtrlVariedOff; // Condensation control--variable off, system modulates to keep running if possible

	// DERIVED TYPE DEFINITIONS

	//MODULE VARIABLE DECLARATIONS:
	extern int NumCoolingPanels;
	extern Array1D< Real64 > CoolingPanelSource; // Need to keep the last value in case we are still iterating
	extern Array1D< Real64 > CoolingPanelSrcAvg; // Need to keep the last value in case we are still iterating
	extern Array1D< Real64 > ZeroSourceSumHATsurf; // Equal to the SumHATsurf for all the walls in a zone with no source

	// Record keeping variables used to calculate CoolingPanelSrcAvg locally
	extern Array1D< Real64 > LastCoolingPanelSrc; // Need to keep the last value in case we are still iterating
	extern Array1D< Real64 > LastSysTimeElapsed; // Need to keep the last value in case we are still iterating
	extern Array1D< Real64 > LastTimeStepSys; // Need to keep the last value in case we are still iterating
	extern Array1D_bool CheckEquipName;
	extern Array1D_bool SetLoopIndexFlag; // get loop number flag

	//SUBROUTINE SPECIFICATIONS FOR MODULE Simple Chilled Ceiling Panel

	// Types

	struct CoolingPanelParams
	{
		// Members
		std::string EquipID;
		int EquipType;
		std::string Schedule;
		Array1D_string SurfaceName;
		Array1D_int SurfacePtr;
		int ZonePtr;
		int SchedPtr;
		int WaterInletNode;
		int WaterOutletNode;
		int TotSurfToDistrib;
		int ControlCompTypeNum;
		int CompErrIndex;
		int ControlType;
		std::string ColdSetptSched;
		int ColdSetptSchedPtr;
		int CondCtrlType;
		Real64 CondDewPtDeltaT;
		int CondErrIndex;
		Real64 ColdThrottlRange;
		Real64 RatedWaterTemp;
		Real64 RatedCapacity;
		Real64 UA;
		Real64 Offset;
		Real64 WaterMassFlowRate;
		Real64 WaterMassFlowRateMax;
		Real64 RatedWaterFlowRate;
		Real64 WaterVolFlowRateMax;
		Real64 WaterInletTempStd;
		Real64 WaterInletTemp;
		Real64 WaterInletEnthalpy;
		Real64 WaterOutletTempStd;
		Real64 WaterOutletTemp;
		Real64 WaterOutletEnthalpy;
		Real64 RatedZoneAirTemp;
		Real64 FracRadiant;
		Real64 FracConvect;
		Real64 FracDistribPerson;
		Array1D< Real64 > FracDistribToSurf;
		Real64 TotPower;
		Real64 Power;
		Real64 ConvPower;
		Real64 RadPower;
		Real64 TotEnergy;
		Real64 Energy;
		Real64 ConvEnergy;
		Real64 RadEnergy;
		int LoopNum; // plant loop index
		int LoopSideNum; // plant loop side index
		int BranchNum; // plant loop branch index
		int CompNum; // plant loop component index
		int CoolingPanelLoadReSimIndex;
		int CoolingPanelMassFlowReSimIndex;
		int CoolingPanelInletTempFlowReSimIndex;

		// Default Constructor
		CoolingPanelParams() :
			EquipType( 0 ),
			ZonePtr( 0 ),
			SchedPtr( 0 ),
			WaterInletNode( 0 ),
			WaterOutletNode( 0 ),
			TotSurfToDistrib( 0 ),
			ControlCompTypeNum( 0 ),
			CompErrIndex( 0 ),
			ControlType( 0 ),
			ColdSetptSchedPtr( 0 ),
			CondCtrlType( 0 ),
			CondDewPtDeltaT( 0.0 ),
			CondErrIndex( 0 ),
			ColdThrottlRange( 0.0 ),
			RatedWaterTemp( 0.0 ),
			RatedCapacity( 0.0 ),
			UA( 0.0 ),
			Offset( 0.0 ),
			WaterMassFlowRate( 0.0 ),
			WaterMassFlowRateMax( 0.0 ),
			RatedWaterFlowRate( 0.0 ),
			WaterVolFlowRateMax( 0.0 ),
			WaterInletTempStd( 0.0 ),
			WaterInletTemp( 0.0 ),
			WaterInletEnthalpy( 0.0 ),
			WaterOutletTempStd( 0.0 ),
			WaterOutletTemp( 0.0 ),
			WaterOutletEnthalpy( 0.0 ),
			RatedZoneAirTemp( 0.0 ),
			FracRadiant( 0.0 ),
			FracConvect( 0.0 ),
			FracDistribPerson( 0.0 ),
			TotPower( 0.0 ),
			Power( 0.0 ),
			ConvPower( 0.0 ),
			RadPower( 0.0 ),
			TotEnergy( 0.0 ),
			Energy( 0.0 ),
			ConvEnergy( 0.0 ),
			RadEnergy( 0.0 ),
			LoopNum( 0 ),
			LoopSideNum( 0 ),
			BranchNum( 0 ),
			CompNum( 0 ),
			CoolingPanelLoadReSimIndex( 0 ),
			CoolingPanelMassFlowReSimIndex( 0 ),
			CoolingPanelInletTempFlowReSimIndex( 0 )
		{}

	};

	// Object Data
	extern Array1D< CoolingPanelParams > CoolingPanel;

	// Functions

	void
	SimCoolingPanel(
		std::string const & EquipName,
		int const ActualZoneNum,
		int const ControlledZoneNum,
		bool const FirstHVACIteration,
		Real64 & PowerMet,
		int & CompIndex
	);

	void
	GetCoolingPanelInput();

	void
	InitCoolingPanel(
		int const CoolingPanelNum,
		int const ControlledZoneNumSub,
		bool const FirstHVACIteration
	);

	void
	CalcCoolingPanel(
		int & CoolingPanelNum
	);

	void
	UpdateCoolingPanel( int const CoolingPanelNum );

	void
	UpdateCoolingPanelSourceValAvg( bool & CoolingPanelSysOn ); // .TRUE. if the radiant system has run this zone time step

	void
	DistributeCoolingPanelRadGains();

	void
	ReportCoolingPanel( int const CoolingPanelNum );

	Real64
	SumHATsurf( int const ZoneNum ); // Zone number

	//*****************************************************************************************
	//     NOTICE

	//     Copyright � 1996-2014 The Board of Trustees of the University of Illinois
	//     and The Regents of the University of California through Ernest Orlando Lawrence
	//     Berkeley National Laboratory.  All rights reserved.

	//     Portions of the EnergyPlus software package have been developed and copyrighted
	//     by other individuals, companies and institutions.  These portions have been
	//     incorporated into the EnergyPlus software package under license.   For a complete
	//     list of contributors, see "Notice" located in EnergyPlus.f90.

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

} // CoolingPanelSimple

} // EnergyPlus

#endif
