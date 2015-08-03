#ifndef ZoneTempPredictorCorrector_hh_INCLUDED
#define ZoneTempPredictorCorrector_hh_INCLUDED

// C++ Headers
#include <vector>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array2D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace ZoneTempPredictorCorrector {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// Controls for PredictorCorrector
	//INTEGER, PUBLIC, PARAMETER :: iGetZoneSetPoints             = 1
	//INTEGER, PUBLIC, PARAMETER :: iPredictStep                  = 2
	//INTEGER, PUBLIC, PARAMETER :: iCorrectStep                  = 3
	//INTEGER, PUBLIC, PARAMETER :: iRevertZoneTimestepHistories  = 4
	//INTEGER, PUBLIC, PARAMETER :: iPushZoneTimestepHistories    = 5
	//INTEGER, PUBLIC, PARAMETER :: iPushSystemTimestepHistories  = 6

	extern Array1D_string const ValidControlTypes;

	extern Array1D_string const ValidComfortControlTypes;

	extern Array1D_string const cZControlTypes;

	extern int const iZC_TStat;
	extern int const iZC_TCTStat;
	extern int const iZC_OTTStat;
	extern int const iZC_HStat;
	extern int const iZC_TandHStat;
	extern int const iZC_StagedDual;
	extern Array1D_int const iZControlTypes;

	extern int const SglHeatSetPoint;
	extern int const SglCoolSetPoint;
	extern int const SglHCSetPoint;
	extern int const DualSetPoint;
	extern int const SglHeatSetPointFanger;
	extern int const SglCoolSetPointFanger;
	extern int const SglHCSetPointFanger;
	extern int const DualSetPointFanger;
	extern int const SglHeatSetPointPierce;
	extern int const SglCoolSetPointPierce;
	extern int const SglHCSetPointPierce;
	extern int const DualSetPointPierce;
	extern int const SglHeatSetPointKSU;
	extern int const SglCoolSetPointKSU;
	extern int const SglHCSetPointKSU;
	extern int const DualSetPointKSU;

	// Average method parameter with multiple people objects in a zone
	extern int const AverageMethodNum_NO; // No multiple people objects
	extern int const AverageMethodNum_SPE; // Specific people object
	extern int const AverageMethodNum_OBJ; // People object average
	extern int const AverageMethodNum_PEO; // People number average

	// DERIVED TYPE DEFINITIONS:

	// INTERFACE BLOCK SPECIFICATIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:

	extern int NumSingleTempHeatingControls;
	extern int NumSingleTempCoolingControls;
	extern int NumSingleTempHeatCoolControls;
	extern int NumDualTempHeatCoolControls;

	// Number of Thermal comfort control types
	extern int NumSingleFangerHeatingControls;
	extern int NumSingleFangerCoolingControls;
	extern int NumSingleFangerHeatCoolControls;
	extern int NumDualFangerHeatCoolControls;

	// Number of zone with staged controlled objects
	extern int NumStageCtrZone;

	extern Array1D< Real64 > ZoneSetPointLast;
	extern Array1D< Real64 > TempIndZnLd;
	extern Array1D< Real64 > TempDepZnLd;
	extern Array1D< Real64 > ZoneAirRelHum; // Zone relative humidity in percent

	// Zone temperature history - used only for oscillation test
	extern Array2D< Real64 > ZoneTempHist;
	extern Array1D< Real64 > ZoneTempOscillate;
	extern Real64 AnyZoneTempOscillate;

	// SUBROUTINE SPECIFICATIONS:

	// Types

	struct ZoneTempControlType
	{
		// Members
		std::string Name; // Name of the zone
		std::string TempSchedName; // Name of the schedule which determines the zone temp setpoint
		int TempSchedIndex;
		std::string HeatTempSetptSchedName;
		int HeatTempSchedIndex;
		std::string CoolTempSetptSchedName;
		int CoolTempSchedIndex;

		// Default Constructor
		ZoneTempControlType() :
			TempSchedIndex( 0 ),
			HeatTempSchedIndex( 0 ),
			CoolTempSchedIndex( 0 )
		{}

		// Member Constructor
		ZoneTempControlType(
			std::string const & Name, // Name of the zone
			std::string const & TempSchedName, // Name of the schedule which determines the zone temp setpoint
			int const TempSchedIndex,
			std::string const & HeatTempSetptSchedName,
			int const HeatTempSchedIndex,
			std::string const & CoolTempSetptSchedName,
			int const CoolTempSchedIndex
		) :
			Name( Name ),
			TempSchedName( TempSchedName ),
			TempSchedIndex( TempSchedIndex ),
			HeatTempSetptSchedName( HeatTempSetptSchedName ),
			HeatTempSchedIndex( HeatTempSchedIndex ),
			CoolTempSetptSchedName( CoolTempSetptSchedName ),
			CoolTempSchedIndex( CoolTempSchedIndex )
		{}

	};

	struct ZoneComfortFangerControlType
	{
		// Members
		std::string Name; // Name of the zone
		std::string PMVSchedName; // Name of the schedule which determines the zone temp setpoint
		int PMVSchedIndex; // Index to PMV dual set point schedule
		std::string HeatPMVSetptSchedName; // Name of PMV heating set point schedule
		int HeatPMVSchedIndex; // Index to PMV heating set point schedule
		std::string CoolPMVSetptSchedName; // Name of PMV cooling set point schedule
		int CoolPMVSchedIndex; // INdex to PMV cooling set point schedule

		// Default Constructor
		ZoneComfortFangerControlType() :
			PMVSchedIndex( 0 ),
			HeatPMVSchedIndex( 0 ),
			CoolPMVSchedIndex( 0 )
		{}

		// Member Constructor
		ZoneComfortFangerControlType(
			std::string const & Name, // Name of the zone
			std::string const & PMVSchedName, // Name of the schedule which determines the zone temp setpoint
			int const PMVSchedIndex, // Index to PMV dual set point schedule
			std::string const & HeatPMVSetptSchedName, // Name of PMV heating set point schedule
			int const HeatPMVSchedIndex, // Index to PMV heating set point schedule
			std::string const & CoolPMVSetptSchedName, // Name of PMV cooling set point schedule
			int const CoolPMVSchedIndex // INdex to PMV cooling set point schedule
		) :
			Name( Name ),
			PMVSchedName( PMVSchedName ),
			PMVSchedIndex( PMVSchedIndex ),
			HeatPMVSetptSchedName( HeatPMVSetptSchedName ),
			HeatPMVSchedIndex( HeatPMVSchedIndex ),
			CoolPMVSetptSchedName( CoolPMVSetptSchedName ),
			CoolPMVSchedIndex( CoolPMVSchedIndex )
		{}

	};

	// Object Data
	extern Array1D< ZoneTempControlType > SetPointSingleHeating;
	extern Array1D< ZoneTempControlType > SetPointSingleCooling;
	extern Array1D< ZoneTempControlType > SetPointSingleHeatCool;
	extern Array1D< ZoneTempControlType > SetPointDualHeatCool;
	extern Array1D< ZoneComfortFangerControlType > SetPointSingleHeatingFanger;
	extern Array1D< ZoneComfortFangerControlType > SetPointSingleCoolingFanger;
	extern Array1D< ZoneComfortFangerControlType > SetPointSingleHeatCoolFanger;
	extern Array1D< ZoneComfortFangerControlType > SetPointDualHeatCoolFanger;

	// Functions

	void
	ManageZoneAirUpdates(
		int const UpdateType, // Can be iGetZoneSetPoints, iPredictStep, iCorrectStep
		Real64 & ZoneTempChange, // Temp change in zone air btw previous and current timestep
		bool const ShortenTimeStepSys,
		bool const UseZoneTimeStepHistory, // if true then use zone timestep history, if false use system time step
		Real64 const PriorTimeStep // the old value for timestep length is passed for possible use in interpolating
	);

	void
	GetZoneAirSetPoints();

	void
	InitZoneAirSetPoints();

	void
	PredictSystemLoads(
		bool const ShortenTimeStepSys,
		bool const UseZoneTimeStepHistory, // if true then use zone timestep history, if false use system time step
		Real64 const PriorTimeStep // the old value for timestep length is passed for possible use in interpolating
	);

	void
	CalcZoneAirTempSetPoints();

	void
	CalcPredictedSystemLoad( int const ZoneNum );

	void
	CalcPredictedHumidityRatio( int const ZoneNum );

	void
	CorrectZoneAirTemp(
		Real64 & ZoneTempChange, // Temperature change in zone air between previous and current timestep
		bool const ShortenTimeStepSys,
		bool const UseZoneTimeStepHistory, // if true then use zone timestep history, if false use system time step history
		Real64 const PriorTimeStep // the old value for timestep length is passed for possible use in interpolating
	);

	void
	PushZoneTimestepHistories();

	void
	PushSystemTimestepHistories();

	void
	RevertZoneTimestepHistories();

	void
	CorrectZoneHumRat(
		int const ZoneNum,
		std::vector< int > const & controlledZoneEquipConfigNums // Precomputed controlled equip nums
	);

	void
	DownInterpolate4HistoryValues(
		Real64 const OldTimeStep,
		Real64 const NewTimeStep,
		Real64 & oldVal0,
		Real64 & oldVal1,
		Real64 & oldVal2,
		Real64 & oldVal3,
		Real64 & oldVal4,
		Real64 & newVal0,
		Real64 & newVal1,
		Real64 & newVal2,
		Real64 & newVal3, // unused 1208
		Real64 & newVal4 // unused 1208
	);

	void
	CalcZoneSums(
		int const ZoneNum, // Zone number
		Real64 & SumIntGain, // Zone sum of convective internal gains
		Real64 & SumHA, // Zone sum of Hc*Area
		Real64 & SumHATsurf, // Zone sum of Hc*Area*Tsurf
		Real64 & SumHATref, // Zone sum of Hc*Area*Tref, for ceiling diffuser convection correlation
		Real64 & SumMCp, // Zone sum of MassFlowRate*Cp
		Real64 & SumMCpT, // Zone sum of MassFlowRate*Cp*T
		Real64 & SumSysMCp, // Zone sum of air system MassFlowRate*Cp
		Real64 & SumSysMCpT, // Zone sum of air system MassFlowRate*Cp*T
		std::vector< int > const & controlledZoneEquipConfigNums // Precomputed controlled equip nums
	);

	void
	CalcZoneComponentLoadSums(
		int const ZoneNum, // Zone number
		Real64 const TempDepCoef, // Dependent coefficient
		Real64 const TempIndCoef, // Independent coefficient
		Real64 & SumIntGains, // Zone sum of convective internal gains
		Real64 & SumHADTsurfs, // Zone sum of Hc*Area*(Tsurf - Tz)
		Real64 & SumMCpDTzones, // zone sum of MassFlowRate*cp*(TremotZone - Tz) transfer air from other zone, Mixing
		Real64 & SumMCpDtInfil, // Zone sum of MassFlowRate*Cp*(Tout - Tz) transfer from outside, ventil, earth tube
		Real64 & SumMCpDTsystem, // Zone sum of air system MassFlowRate*Cp*(Tsup - Tz)
		Real64 & SumNonAirSystem, // Zone sum of non air system convective heat gains
		Real64 & CzdTdt, // Zone air energy storage term.
		Real64 & imBalance, // put all terms in eq. 5 on RHS , should be zero
		std::vector< int > const & controlledZoneEquipConfigNums // Precomputed controlled equip nums
	);

	bool
	VerifyThermostatInZone( std::string const & ZoneName ); // Zone to verify

	bool
	VerifyControlledZoneForThermostat( std::string const & ZoneName ); // Zone to verify

	void
	DetectOscillatingZoneTemp();

	void
	AdjustAirSetPointsforOpTempCntrl(
		int const TempControlledZoneID,
		int const ActualZoneNum,
		Real64 & ZoneAirSetPoint
	);

	void
	CalcZoneAirComfortSetPoints();

	void
	GetComfortSetPoints(
		int const PeopleNum,
		int const ComfortControlNum,
		Real64 const PMVSet,
		Real64 & Tset // drybulb setpoint temperature for a given PMV value
	);

	Real64
	PMVResidual(
		Real64 const Tset,
		Array1< Real64 > const & Par // par(1) = PMV set point
	);

	void
	AdjustCoolingSetPointforTempAndHumidityControl(
		int const TempControlledZoneID,
		int const ActualZoneNum // controlled zone actual zone number
	);

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

} // ZoneTempPredictorCorrector

} // EnergyPlus

#endif
