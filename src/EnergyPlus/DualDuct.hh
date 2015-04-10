#ifndef DualDuct_hh_INCLUDED
#define DualDuct_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace DualDuct {

	// Using/Aliasing

	// Data
	//MODULE PARAMETER DEFINITIONS
	extern int const DualDuct_ConstantVolume;
	extern int const DualDuct_VariableVolume;
	extern int const DualDuct_OutdoorAir;
	extern std::string const cCMO_DDConstantVolume;
	extern std::string const cCMO_DDVariableVolume;
	extern std::string const cCMO_DDVarVolOA;

	extern int const DD_OA_ConstantOAMode;
	extern int const DD_OA_ScheduleOAMode;
	extern int const DD_OA_DynamicOAMode;

	extern int const PerPersonModeNotSet;
	extern int const PerPersonDCVByCurrentLevel;
	extern int const PerPersonByDesignLevel;

	// DERIVED TYPE DEFINITIONS

	//MODULE VARIABLE DECLARATIONS:
	extern Array1D_bool CheckEquipName;

	extern int NumDampers; // The Number of Dampers found in the Input
	extern int NumDualDuctConstVolDampers;
	extern int NumDualDuctVarVolDampers;
	extern int NumDualDuctVarVolOA;
	extern Real64 MassFlowSetToler;
	extern bool GetDualDuctInputFlag; // Flag set to make sure you get input once

	// Subroutine Specifications for the Module
	// Driver/Manager Routines

	// Get Input routines for module

	// Initialization routines for module

	// Algorithms for the module

	// Update routine to check convergence and update nodes

	// Reporting routines for module

	// Types

	struct DamperDesignParams
	{
		// Members
		std::string DamperName; // Name of the Damper
		//  CHARACTER(len=MaxNameLength) :: DamperType  = ' ' ! Type of Damper ie. VAV, Mixing, Inducing, etc.
		int DamperType; // Type of Damper ie. VAV, Mixing, Inducing, etc.
		std::string Schedule; // Damper Operation Schedule
		int SchedPtr; // Pointer to the correct schedule
		Real64 MaxAirVolFlowRate; // Max Specified Volume Flow Rate of Damper [m3/sec]
		Real64 MaxAirMassFlowRate; // Max Specified MAss Flow Rate of Damper [kg/s]
		int InletNodeNum;
		int HotAirInletNodeNum;
		int ColdAirInletNodeNum;
		int OutletNodeNum;
		Real64 ZoneMinAirFrac;
		Real64 ColdAirDamperPosition;
		Real64 HotAirDamperPosition;
		int OAInletNodeNum; // Alternate Node for VAV:OutdoorAir for Outdoor Air
		int RecircAirInletNodeNum; // Alternate Node for VAV:OutdoorAir for Recirc Air
		bool RecircIsUsed; // if true. then not using recirc duct, which is okay
		Real64 DesignOAFlowRate; // Terminal Outdoor Air Design Flow Rate for VAV:OutdoorAir, m3/s
		Real64 DesignRecircFlowRate; // Terminal Recirc Air Design Flow Rate for VAV:OutdoorAir, m3/s
		int OAControlMode; // Choice of scheduled, constant, or dynamic for VAV:OutdoorAir
		Real64 RecircAirDamperPosition; // Alternate Damper Pos Output for VAV:OutdoorAir for Recirc Air
		Real64 OADamperPosition; // Alternate Damper Pos Output for VAV:OutdoorAir for Recirc Air
		Real64 OAFraction; // Outdoor Air Fraction for VAV:OutdoorAir
		int ADUNum; // index of corresponding air distribution unit
		int CtrlZoneNum; // Pointer to CtrlZone data structure
		int ActualZoneNum; // Pointer to Zone data Structure
		Real64 OutdoorAirFlowRate; // report variable for TU outdoor air flow rate
		bool NoOAFlowInputFromUser; // avoids OA calculation if no input specified by user
		int OARequirementsPtr; // - Index to DesignSpecification:OutdoorAir object
		int OAPerPersonMode; // mode for how per person rates are determined, DCV or design.
		Real64 OAPerPersonByDesignLevel; // store sum of people and per person rate, constant, m3/s
		int AirLoopNum;

		// Default Constructor
		DamperDesignParams() :
			DamperType( 0 ),
			SchedPtr( 0 ),
			MaxAirVolFlowRate( 0.0 ),
			MaxAirMassFlowRate( 0.0 ),
			InletNodeNum( 0 ),
			HotAirInletNodeNum( 0 ),
			ColdAirInletNodeNum( 0 ),
			OutletNodeNum( 0 ),
			ZoneMinAirFrac( 0.0 ),
			ColdAirDamperPosition( 0.0 ),
			HotAirDamperPosition( 0.0 ),
			OAInletNodeNum( 0 ),
			RecircAirInletNodeNum( 0 ),
			RecircIsUsed( true ),
			DesignOAFlowRate( 0.0 ),
			DesignRecircFlowRate( 0.0 ),
			OAControlMode( 0 ),
			RecircAirDamperPosition( 0.0 ),
			OADamperPosition( 0.0 ),
			OAFraction( 0.0 ),
			ADUNum( 0 ),
			CtrlZoneNum( 0 ),
			ActualZoneNum( 0 ),
			OutdoorAirFlowRate( 0.0 ),
			NoOAFlowInputFromUser( true ),
			OARequirementsPtr( 0 ),
			OAPerPersonMode( PerPersonModeNotSet ),
			OAPerPersonByDesignLevel( 0.0 ),
			AirLoopNum( 0 )
		{}

		// Member Constructor
		DamperDesignParams(
			std::string const & DamperName, // Name of the Damper
			int const DamperType, // Type of Damper ie. VAV, Mixing, Inducing, etc.
			std::string const & Schedule, // Damper Operation Schedule
			int const SchedPtr, // Pointer to the correct schedule
			Real64 const MaxAirVolFlowRate, // Max Specified Volume Flow Rate of Damper [m3/sec]
			Real64 const MaxAirMassFlowRate, // Max Specified MAss Flow Rate of Damper [kg/s]
			int const InletNodeNum,
			int const HotAirInletNodeNum,
			int const ColdAirInletNodeNum,
			int const OutletNodeNum,
			Real64 const ZoneMinAirFrac,
			Real64 const ColdAirDamperPosition,
			Real64 const HotAirDamperPosition,
			int const OAInletNodeNum, // Alternate Node for VAV:OutdoorAir for Outdoor Air
			int const RecircAirInletNodeNum, // Alternate Node for VAV:OutdoorAir for Recirc Air
			bool const RecircIsUsed, // if true. then not using recirc duct, which is okay
			Real64 const DesignOAFlowRate, // Terminal Outdoor Air Design Flow Rate for VAV:OutdoorAir, m3/s
			Real64 const DesignRecircFlowRate, // Terminal Recirc Air Design Flow Rate for VAV:OutdoorAir, m3/s
			int const OAControlMode, // Choice of scheduled, constant, or dynamic for VAV:OutdoorAir
			Real64 const RecircAirDamperPosition, // Alternate Damper Pos Output for VAV:OutdoorAir for Recirc Air
			Real64 const OADamperPosition, // Alternate Damper Pos Output for VAV:OutdoorAir for Recirc Air
			Real64 const OAFraction, // Outdoor Air Fraction for VAV:OutdoorAir
			int const ADUNum, // index of corresponding air distribution unit
			int const CtrlZoneNum, // Pointer to CtrlZone data structure
			int const ActualZoneNum, // Pointer to Zone data Structure
			Real64 const OutdoorAirFlowRate, // report variable for TU outdoor air flow rate
			bool const NoOAFlowInputFromUser, // avoids OA calculation if no input specified by user
			int const OARequirementsPtr, // - Index to DesignSpecification:OutdoorAir object
			int const OAPerPersonMode, // mode for how per person rates are determined, DCV or design.
			Real64 const OAPerPersonByDesignLevel, // store sum of people and per person rate, constant, m3/s
			int const AirLoopNum
		) :
			DamperName( DamperName ),
			DamperType( DamperType ),
			Schedule( Schedule ),
			SchedPtr( SchedPtr ),
			MaxAirVolFlowRate( MaxAirVolFlowRate ),
			MaxAirMassFlowRate( MaxAirMassFlowRate ),
			InletNodeNum( InletNodeNum ),
			HotAirInletNodeNum( HotAirInletNodeNum ),
			ColdAirInletNodeNum( ColdAirInletNodeNum ),
			OutletNodeNum( OutletNodeNum ),
			ZoneMinAirFrac( ZoneMinAirFrac ),
			ColdAirDamperPosition( ColdAirDamperPosition ),
			HotAirDamperPosition( HotAirDamperPosition ),
			OAInletNodeNum( OAInletNodeNum ),
			RecircAirInletNodeNum( RecircAirInletNodeNum ),
			RecircIsUsed( RecircIsUsed ),
			DesignOAFlowRate( DesignOAFlowRate ),
			DesignRecircFlowRate( DesignRecircFlowRate ),
			OAControlMode( OAControlMode ),
			RecircAirDamperPosition( RecircAirDamperPosition ),
			OADamperPosition( OADamperPosition ),
			OAFraction( OAFraction ),
			ADUNum( ADUNum ),
			CtrlZoneNum( CtrlZoneNum ),
			ActualZoneNum( ActualZoneNum ),
			OutdoorAirFlowRate( OutdoorAirFlowRate ),
			NoOAFlowInputFromUser( NoOAFlowInputFromUser ),
			OARequirementsPtr( OARequirementsPtr ),
			OAPerPersonMode( OAPerPersonMode ),
			OAPerPersonByDesignLevel( OAPerPersonByDesignLevel ),
			AirLoopNum( AirLoopNum )
		{}

	};

	struct DamperFlowConditions
	{
		// Members
		Real64 AirMassFlowRate; // MassFlow through the Damper being Simulated [kg/Sec]
		Real64 AirMassFlowRateMaxAvail; // MassFlow through the Damper being Simulated [kg/Sec]
		Real64 AirMassFlowRateMinAvail; // MassFlow through the Damper being Simulated [kg/Sec]
		Real64 AirMassFlowRateMax; // Max Mass Flow Rate or the Design Mass Flow Rate
		Real64 AirTemp;
		Real64 AirHumRat;
		Real64 AirEnthalpy;
		Real64 AirMassFlowRateHist1; // flow history back 1 iteration kg/s
		Real64 AirMassFlowRateHist2; // flow history back 2 iteration kg/s
		Real64 AirMassFlowRateHist3; // flow history back 3 iteration kg/s
		Real64 AirMassFlowDiffMag; // flow difference scale, kg/s

		// Default Constructor
		DamperFlowConditions() :
			AirMassFlowRate( 0.0 ),
			AirMassFlowRateMaxAvail( 0.0 ),
			AirMassFlowRateMinAvail( 0.0 ),
			AirMassFlowRateMax( 0.0 ),
			AirTemp( 0.0 ),
			AirHumRat( 0.0 ),
			AirEnthalpy( 0.0 ),
			AirMassFlowRateHist1( 0.0 ),
			AirMassFlowRateHist2( 0.0 ),
			AirMassFlowRateHist3( 0.0 ),
			AirMassFlowDiffMag( 0.0 )
		{}

		// Member Constructor
		DamperFlowConditions(
			Real64 const AirMassFlowRate, // MassFlow through the Damper being Simulated [kg/Sec]
			Real64 const AirMassFlowRateMaxAvail, // MassFlow through the Damper being Simulated [kg/Sec]
			Real64 const AirMassFlowRateMinAvail, // MassFlow through the Damper being Simulated [kg/Sec]
			Real64 const AirMassFlowRateMax, // Max Mass Flow Rate or the Design Mass Flow Rate
			Real64 const AirTemp,
			Real64 const AirHumRat,
			Real64 const AirEnthalpy,
			Real64 const AirMassFlowRateHist1, // flow history back 1 iteration kg/s
			Real64 const AirMassFlowRateHist2, // flow history back 2 iteration kg/s
			Real64 const AirMassFlowRateHist3, // flow history back 3 iteration kg/s
			Real64 const AirMassFlowDiffMag // flow difference scale, kg/s
		) :
			AirMassFlowRate( AirMassFlowRate ),
			AirMassFlowRateMaxAvail( AirMassFlowRateMaxAvail ),
			AirMassFlowRateMinAvail( AirMassFlowRateMinAvail ),
			AirMassFlowRateMax( AirMassFlowRateMax ),
			AirTemp( AirTemp ),
			AirHumRat( AirHumRat ),
			AirEnthalpy( AirEnthalpy ),
			AirMassFlowRateHist1( AirMassFlowRateHist1 ),
			AirMassFlowRateHist2( AirMassFlowRateHist2 ),
			AirMassFlowRateHist3( AirMassFlowRateHist3 ),
			AirMassFlowDiffMag( AirMassFlowDiffMag )
		{}

	};

	// Object Data
	extern Array1D< DamperDesignParams > Damper;
	extern Array1D< DamperFlowConditions > DamperInlet;
	extern Array1D< DamperFlowConditions > DamperHotAirInlet;
	extern Array1D< DamperFlowConditions > DamperColdAirInlet;
	extern Array1D< DamperFlowConditions > DamperOutlet;
	extern Array1D< DamperFlowConditions > DamperOAInlet; // VAV:OutdoorAir Outdoor Air Inlet
	extern Array1D< DamperFlowConditions > DamperRecircAirInlet; // VAV:OutdoorAir Recirculated Air Inlet

	// Functions

	void
	SimulateDualDuct(
		std::string const & CompName,
		bool const FirstHVACIteration,
		int const ZoneNum,
		int const ZoneNodeNum,
		int & CompIndex
	);

	// Get Input Section of the Module
	//******************************************************************************

	void
	GetDualDuctInput();

	// End of Get Input subroutines for the Module
	//******************************************************************************

	// Beginning Initialization Section of the Module
	//******************************************************************************

	void
	InitDualDuct(
		int const DamperNum,
		bool const FirstHVACIteration
	);

	void
	SizeDualDuct( int const DamperNum );

	// End Initialization Section of the Module
	//******************************************************************************

	// Begin Algorithm Section of the Module
	//******************************************************************************

	void
	SimDualDuctConstVol(
		int const DamperNum,
		int const ZoneNum,
		int const ZoneNodeNum
	);

	void
	SimDualDuctVarVol(
		int const DamperNum,
		int const ZoneNum,
		int const ZoneNodeNum
	);

	void
	SimDualDuctVAVOutdoorAir(
		int const DamperNum,
		int const ZoneNum,
		int const ZoneNodeNum
	);

	void
	CalcOAMassFlow(
		int const DamperNum, // index to terminal unit
		Real64 & SAMassFlow, // outside air based on optional user input
		Real64 & AirLoopOAFrac // outside air based on optional user input
	);

	void
	CalcOAOnlyMassFlow(
		int const DamperNum, // index to terminal unit
		Real64 & OAMassFlow, // outside air flow from user input kg/s
		Optional< Real64 > MaxOAVolFlow = _ // design level for outside air m3/s
	);

	// End Algorithm Section of the Module
	// *****************************************************************************

	// Beginning of Update subroutines for the Damper Module
	// *****************************************************************************

	void
	UpdateDualDuct( int const DamperNum );

	//        End of Update subroutines for the Damper Module
	// *****************************************************************************

	// Beginning of Reporting subroutines for the Damper Module
	// *****************************************************************************

	void
	ReportDualDuct( int const DamperNum ); // unused1208

	void
	ReportDualDuctConnections();

	void
	GetDualDuctOutdoorAirRecircUse(
		std::string const & CompTypeName,
		std::string const & CompName,
		bool & RecircIsUsed
	);

	//        End of Reporting subroutines for the Damper Module
	// *****************************************************************************

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

} // DualDuct

} // EnergyPlus

#endif
