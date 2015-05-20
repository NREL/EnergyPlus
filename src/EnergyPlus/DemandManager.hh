#ifndef DemandManager_hh_INCLUDED
#define DemandManager_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace DemandManager {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS:
	extern int const ManagerTypeExtLights;
	extern int const ManagerTypeLights;
	extern int const ManagerTypeElecEquip;
	extern int const ManagerTypeThermostats;

	extern int const ManagerPrioritySequential;
	extern int const ManagerPriorityOptimal;
	extern int const ManagerPriorityAll;

	extern int const ManagerLimitOff;
	extern int const ManagerLimitFixed;
	extern int const ManagerLimitVariable;

	extern int const ManagerSelectionAll;
	extern int const ManagerSelectionMany;
	extern int const ManagerSelectionOne;

	extern int const CheckCanReduce;
	extern int const SetLimit;
	extern int const ClearLimit;

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE TYPE DECLARATIONS:

	// MODULE VARIABLE DECLARATIONS:
	extern int NumDemandManagerList;
	extern int NumDemandMgr;
	extern int DemandManagerExtIterations;
	extern int DemandManagerHBIterations;
	extern int DemandManagerHVACIterations;
	extern bool GetInput; // Flag to prevent input from being read multiple times

	// SUBROUTINE SPECIFICATIONS:

	// Types

	struct DemandManagerListData
	{
		// Members
		std::string Name; // Name of DEMAND MANAGER LIST
		int Meter; // Index to meter to demand limit
		int LimitSchedule; // Schedule index for demand limit
		Real64 SafetyFraction; // Multiplier applied to demand limit schedule
		int BillingSchedule; // Schedule index for billing month periods
		Real64 BillingPeriod; // Current billing period value
		int PeakSchedule; // Schedule index for billing month periods
		int AveragingWindow; // Number of timesteps for averaging demand window
		Array1D< Real64 > History; // Demand window history
		int ManagerPriority; // Indicator for priority (SEQUENTIAL, OPTIMAL, ALL)
		int NumOfManager; // Number of DEMAND MANAGERs
		Array1D_int Manager; // Indexes for DEMAND MANAGERs
		Real64 MeterDemand; // Meter demand at this timestep
		Real64 AverageDemand; // Current demand over the demand window
		Real64 PeakDemand; // Peak demand in the billing month so far
		Real64 ScheduledLimit; // Scheduled demand limit
		Real64 DemandLimit; // Scheduled demand limit * Safety Fraction
		Real64 AvoidedDemand; // Demand avoided by active DEMAND MANAGERs
		Real64 OverLimit; // Amount that demand limit is exceeded
		Real64 OverLimitDuration; // Number of hours that demand limit is exceeded

		// Default Constructor
		DemandManagerListData() :
			Meter( 0 ),
			LimitSchedule( 0 ),
			SafetyFraction( 1.0 ),
			BillingSchedule( 0 ),
			BillingPeriod( 0.0 ),
			PeakSchedule( 0 ),
			AveragingWindow( 1 ),
			ManagerPriority( 0 ),
			MeterDemand( 0.0 ),
			AverageDemand( 0.0 ),
			PeakDemand( 0.0 ),
			ScheduledLimit( 0.0 ),
			DemandLimit( 0.0 ),
			AvoidedDemand( 0.0 ),
			OverLimit( 0.0 ),
			OverLimitDuration( 0.0 )
		{}

		// Member Constructor
		DemandManagerListData(
			std::string const & Name, // Name of DEMAND MANAGER LIST
			int const Meter, // Index to meter to demand limit
			int const LimitSchedule, // Schedule index for demand limit
			Real64 const SafetyFraction, // Multiplier applied to demand limit schedule
			int const BillingSchedule, // Schedule index for billing month periods
			Real64 const BillingPeriod, // Current billing period value
			int const PeakSchedule, // Schedule index for billing month periods
			int const AveragingWindow, // Number of timesteps for averaging demand window
			Array1< Real64 > const & History, // Demand window history
			int const ManagerPriority, // Indicator for priority (SEQUENTIAL, OPTIMAL, ALL)
			int const NumOfManager, // Number of DEMAND MANAGERs
			Array1_int const & Manager, // Indexes for DEMAND MANAGERs
			Real64 const MeterDemand, // Meter demand at this timestep
			Real64 const AverageDemand, // Current demand over the demand window
			Real64 const PeakDemand, // Peak demand in the billing month so far
			Real64 const ScheduledLimit, // Scheduled demand limit
			Real64 const DemandLimit, // Scheduled demand limit * Safety Fraction
			Real64 const AvoidedDemand, // Demand avoided by active DEMAND MANAGERs
			Real64 const OverLimit, // Amount that demand limit is exceeded
			Real64 const OverLimitDuration // Number of hours that demand limit is exceeded
		) :
			Name( Name ),
			Meter( Meter ),
			LimitSchedule( LimitSchedule ),
			SafetyFraction( SafetyFraction ),
			BillingSchedule( BillingSchedule ),
			BillingPeriod( BillingPeriod ),
			PeakSchedule( PeakSchedule ),
			AveragingWindow( AveragingWindow ),
			History( History ),
			ManagerPriority( ManagerPriority ),
			NumOfManager( NumOfManager ),
			Manager( Manager ),
			MeterDemand( MeterDemand ),
			AverageDemand( AverageDemand ),
			PeakDemand( PeakDemand ),
			ScheduledLimit( ScheduledLimit ),
			DemandLimit( DemandLimit ),
			AvoidedDemand( AvoidedDemand ),
			OverLimit( OverLimit ),
			OverLimitDuration( OverLimitDuration )
		{}

	};

	struct DemandManagerData
	{
		// Members
		std::string Name; // Name of DEMAND MANAGER
		int Type; // Type of DEMAND MANAGER (:LIGHTS, :ELECTRICEQUIPMENT, etc.)
		int DemandManagerList; // Reference to parent DEMAND MANAGER LIST for error checking
		bool CanReduceDemand; // Flag to indicate whether manager can reduce demand
		int AvailSchedule; // Schedule index pointer for Availability Schedule
		bool Available; // Availability flag
		bool Activate; // Flag to activate the manager
		bool Active; // Flag to indicate that the manager is active
		int LimitControl;
		int SelectionControl;
		int LimitDuration; // Minimum duration of demand manager activity (min)
		int ElapsedTime; // Elapsed time for the demand manager activity (min)
		int RotationDuration; // Rotation duration (min)
		int ElapsedRotationTime; // Elapsed time for the current rotation (min)
		int RotatedLoadNum; // Index for rotated load
		Real64 LowerLimit; // Lowest demand limit as fraction of design level
		// Lowest heating setpoint for thermostats
		Real64 UpperLimit; // Not used for demand limit
		// Highest cooling setpoint for thermostats
		int NumOfLoads; // Number of load objects
		Array1D_int Load; // Pointers to load objects

		// Default Constructor
		DemandManagerData() :
			Type( 0 ),
			DemandManagerList( 0 ),
			CanReduceDemand( false ),
			AvailSchedule( 0 ),
			Available( false ),
			Activate( false ),
			Active( false ),
			LimitControl( 0 ),
			SelectionControl( 0 ),
			LimitDuration( 0 ),
			ElapsedTime( 0 ),
			RotationDuration( 0 ),
			ElapsedRotationTime( 0 ),
			RotatedLoadNum( 0 ),
			LowerLimit( 0.0 ),
			UpperLimit( 0.0 ),
			NumOfLoads( 0 )
		{}

		// Member Constructor
		DemandManagerData(
			std::string const & Name, // Name of DEMAND MANAGER
			int const Type, // Type of DEMAND MANAGER (:LIGHTS, :ELECTRICEQUIPMENT, etc.)
			int const DemandManagerList, // Reference to parent DEMAND MANAGER LIST for error checking
			bool const CanReduceDemand, // Flag to indicate whether manager can reduce demand
			int const AvailSchedule, // Schedule index pointer for Availability Schedule
			bool const Available, // Availability flag
			bool const Activate, // Flag to activate the manager
			bool const Active, // Flag to indicate that the manager is active
			int const LimitControl,
			int const SelectionControl,
			int const LimitDuration, // Minimum duration of demand manager activity (min)
			int const ElapsedTime, // Elapsed time for the demand manager activity (min)
			int const RotationDuration, // Rotation duration (min)
			int const ElapsedRotationTime, // Elapsed time for the current rotation (min)
			int const RotatedLoadNum, // Index for rotated load
			Real64 const LowerLimit, // Lowest demand limit as fraction of design level
			Real64 const UpperLimit, // Not used for demand limit
			int const NumOfLoads, // Number of load objects
			Array1_int const & Load // Pointers to load objects
		) :
			Name( Name ),
			Type( Type ),
			DemandManagerList( DemandManagerList ),
			CanReduceDemand( CanReduceDemand ),
			AvailSchedule( AvailSchedule ),
			Available( Available ),
			Activate( Activate ),
			Active( Active ),
			LimitControl( LimitControl ),
			SelectionControl( SelectionControl ),
			LimitDuration( LimitDuration ),
			ElapsedTime( ElapsedTime ),
			RotationDuration( RotationDuration ),
			ElapsedRotationTime( ElapsedRotationTime ),
			RotatedLoadNum( RotatedLoadNum ),
			LowerLimit( LowerLimit ),
			UpperLimit( UpperLimit ),
			NumOfLoads( NumOfLoads ),
			Load( Load )
		{}

	};

	// Object Data
	extern Array1D< DemandManagerListData > DemandManagerList;
	extern Array1D< DemandManagerData > DemandMgr;

	// Functions

	void
	ManageDemand();

	void
	SimulateDemandManagerList(
		int const ListNum,
		bool & ResimExt, // Flag to resimulate the exterior energy use simulation
		bool & ResimHB, // Flag to resimulate the heat balance simulation (including HVAC)
		bool & ResimHVAC // Flag to resimulate the HVAC simulation
	);

	void
	GetDemandManagerListInput();

	void
	GetDemandManagerInput();

	void
	SurveyDemandManagers();

	void
	ActivateDemandManagers();

	void
	UpdateDemandManagers();

	void
	ReportDemandManagerList( int const ListNum );

	void
	LoadInterface(
		int const Action,
		int const MgrNum,
		int const LoadPtr,
		bool & CanReduceDemand
	);

	void
	InitDemandManagers();

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

} // DemandManager

} // EnergyPlus

#endif
