#ifndef SystemReports_hh_INCLUDED
#define SystemReports_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace SystemReports {

	// Data
	// MODULE PARAMETER DEFINITIONS:
	extern int const NoHeatNoCool;
	extern int const CoolingOnly;
	extern int const HeatingOnly;
	extern int const HeatAndCool;
	extern int const MaxSetBackCount;

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	//Ventilation Report Variables
	extern FArray1D< Real64 > MaxCoolingLoadMetByVent;
	extern FArray1D< Real64 > MaxCoolingLoadAddedByVent;
	extern FArray1D< Real64 > MaxOvercoolingByVent;
	extern FArray1D< Real64 > MaxHeatingLoadMetByVent;
	extern FArray1D< Real64 > MaxHeatingLoadAddedByVent;
	extern FArray1D< Real64 > MaxOverheatingByVent;
	extern FArray1D< Real64 > MaxNoLoadHeatingByVent;
	extern FArray1D< Real64 > MaxNoLoadCoolingByVent;

	extern FArray1D< Real64 > RemMaxCoolingLoadMetByVent;
	extern FArray1D< Real64 > RemMaxCoolingLoadAddedByVent;
	extern FArray1D< Real64 > RemMaxOvercoolingByVent;
	extern FArray1D< Real64 > RemMaxHeatingLoadMetByVent;
	extern FArray1D< Real64 > RemMaxHeatingLoadAddedByVent;
	extern FArray1D< Real64 > RemMaxOverheatingByVent;
	extern FArray1D< Real64 > RemMaxNoLoadHeatingByVent;
	extern FArray1D< Real64 > RemMaxNoLoadCoolingByVent;

	extern FArray1D< Real64 > LastMaxCoolingLoadMetByVent;
	extern FArray1D< Real64 > LastMaxCoolingLoadAddedByVent;
	extern FArray1D< Real64 > LastMaxOvercoolingByVent;
	extern FArray1D< Real64 > LastMaxHeatingLoadMetByVent;
	extern FArray1D< Real64 > LastMaxHeatingLoadAddedByVent;
	extern FArray1D< Real64 > LastMaxOverheatingByVent;
	extern FArray1D< Real64 > LastMaxNoLoadHeatingByVent;
	extern FArray1D< Real64 > LastMaxNoLoadCoolingByVent;

	extern FArray1D< Real64 > SysTotZoneLoadHTNG;
	extern FArray1D< Real64 > SysTotZoneLoadCLNG;
	extern FArray1D< Real64 > SysOALoadHTNG;
	extern FArray1D< Real64 > SysOALoadCLNG;
	extern FArray1D< Real64 > SysTotHTNG;
	extern FArray1D< Real64 > SysTotCLNG;

	extern FArray1D< Real64 > SysTotH2OHOT;
	extern FArray1D< Real64 > SysTotH2OCOLD;
	extern FArray1D< Real64 > SysTotElec;
	extern FArray1D< Real64 > SysTotGas;
	extern FArray1D< Real64 > SysTotSteam;

	extern FArray1D< Real64 > SysHumidHTNG;
	extern FArray1D< Real64 > SysHumidElec;
	extern FArray1D< Real64 > SysEvapCLNG;
	extern FArray1D< Real64 > SysEvapElec;
	extern FArray1D< Real64 > SysHeatExHTNG;
	extern FArray1D< Real64 > SysHeatExCLNG;
	extern FArray1D< Real64 > DesDehumidCLNG;
	extern FArray1D< Real64 > DesDehumidElec;
	extern FArray1D< Real64 > SysSolarCollectHeating;
	extern FArray1D< Real64 > SysSolarCollectCooling;
	extern FArray1D< Real64 > SysUserDefinedTerminalHeating;
	extern FArray1D< Real64 > SysUserDefinedTerminalCooling;

	extern FArray1D< Real64 > SysFANCompHTNG;
	extern FArray1D< Real64 > SysFANCompElec;
	extern FArray1D< Real64 > SysCCCompCLNG;
	extern FArray1D< Real64 > SysCCCompH2OCOLD;
	extern FArray1D< Real64 > SysCCCompElec;
	extern FArray1D< Real64 > SysHCCompH2OHOT;
	extern FArray1D< Real64 > SysHCCompElec;
	extern FArray1D< Real64 > SysHCCompElecRes;
	extern FArray1D< Real64 > SysHCCompHTNG;
	extern FArray1D< Real64 > SysHCCompGas;
	extern FArray1D< Real64 > SysHCCompSteam;
	extern FArray1D< Real64 > SysDomesticH20;

	extern FArray1D< Real64 > ZoneOAMassFlow; // zone mech vent mass flow rate {kg/s}
	extern FArray1D< Real64 > ZoneOAMass; // zone mech vent total mass for time {kg}
	extern FArray1D< Real64 > ZoneOAVolFlowStdRho; // zone mech vent volume flow rate at standard density {m3/s}
	extern FArray1D< Real64 > ZoneOAVolStdRho; // zone mech vent total volume OA at standard density {m3/s}
	extern FArray1D< Real64 > ZoneOAVolFlowCrntRho; // zone mech vent volume flow rate at current density {m3/s}
	extern FArray1D< Real64 > ZoneOAVolCrntRho; // zone mech vent total volume OA at current density {m3/s}
	extern FArray1D< Real64 > ZoneMechACH; // zone mech vent air changes per hour {ACH}

	extern bool AirLoopLoadsReportEnabled;
	extern bool VentLoadsReportEnabled;
	extern bool VentEnergyReportEnabled;
	extern bool VentReportStructureCreated;
	extern int TotalLoopConnects; // Total number of loop connections
	extern int MaxLoopArraySize;
	extern int MaxCompArraySize;
	extern int DBFlag;

	extern FArray1D_int SetBackCounter;
	extern FArray1D_int HeatCoolFlag;
	extern FArray1D_int FirstHeatCoolFlag;
	extern FArray1D_int FirstHeatCoolHour;
	extern FArray1D_int LastHeatCoolFlag;
	extern FArray1D_int LastHeatCoolHour;
	extern FArray1D_bool AirLoopCalcDone;
	extern FArray1D_bool NoLoadFlag;
	extern FArray1D_bool UnmetLoadFlag;

	// SUBROUTINE SPECIFICATIONS FOR MODULE SystemReports

	//Reporting Initialization

	// Reporting routines for module

	// Types

	struct Energy
	{
		// Members
		Real64 TotDemand;
		Real64 Elec;
		Real64 Gas;
		Real64 Purch;
		Real64 Other;

		// Default Constructor
		Energy() :
			TotDemand( 0.0 ),
			Elec( 0.0 ),
			Gas( 0.0 ),
			Purch( 0.0 ),
			Other( 0.0 )
		{}

		// Member Constructor
		Energy(
			Real64 const TotDemand,
			Real64 const Elec,
			Real64 const Gas,
			Real64 const Purch,
			Real64 const Other
		) :
			TotDemand( TotDemand ),
			Elec( Elec ),
			Gas( Gas ),
			Purch( Purch ),
			Other( Other )
		{}

	};

	struct CoilType
	{
		// Members
		Energy DecreasedCC; // LoadMetByVent
		Energy DecreasedHC; // LoadMetByVent
		Energy IncreasedCC; // LoadIncreasedVent
		Energy IncreasedHC; // LoadAddedByVent
		Energy ReducedByCC; // LoadAddedByVent
		Energy ReducedByHC; // LoadAddedByVent

		// Default Constructor
		CoilType()
		{}

		// Member Constructor
		CoilType(
			Energy const & DecreasedCC, // LoadMetByVent
			Energy const & DecreasedHC, // LoadMetByVent
			Energy const & IncreasedCC, // LoadIncreasedVent
			Energy const & IncreasedHC, // LoadAddedByVent
			Energy const & ReducedByCC, // LoadAddedByVent
			Energy const & ReducedByHC // LoadAddedByVent
		) :
			DecreasedCC( DecreasedCC ),
			DecreasedHC( DecreasedHC ),
			IncreasedCC( IncreasedCC ),
			IncreasedHC( IncreasedHC ),
			ReducedByCC( ReducedByCC ),
			ReducedByHC( ReducedByHC )
		{}

	};

	struct SummarizeLoads
	{
		// Members
		CoilType Load; // LoadMetByVent
		CoilType NoLoad; // LoadMetByVentNoLoad
		CoilType ExcessLoad; // LoadAddedByVentOvercool
		CoilType PotentialSavings; // LoadAddedByVentCoolLost
		CoilType PotentialCost; // LoadAddedByVentHeatLost

		// Default Constructor
		SummarizeLoads()
		{}

		// Member Constructor
		SummarizeLoads(
			CoilType const & Load, // LoadMetByVent
			CoilType const & NoLoad, // LoadMetByVentNoLoad
			CoilType const & ExcessLoad, // LoadAddedByVentOvercool
			CoilType const & PotentialSavings, // LoadAddedByVentCoolLost
			CoilType const & PotentialCost // LoadAddedByVentHeatLost
		) :
			Load( Load ),
			NoLoad( NoLoad ),
			ExcessLoad( ExcessLoad ),
			PotentialSavings( PotentialSavings ),
			PotentialCost( PotentialCost )
		{}

	};

	// Object Data
	extern FArray1D< SummarizeLoads > Vent;

	// Functions

	void
	InitEnergyReports();

	void
	FindFirstLastPtr(
		int & LoopType,
		int & LoopNum,
		int & ArrayCount,
		int & LoopCount,
		bool & ConnectionFlag
	);

	void
	UpdateZoneCompPtrArray(
		int & Idx,
		int const ListNum,
		int const AirDistUnitNum,
		int const PlantLoopType,
		int const PlantLoop,
		int const PlantBranch,
		int const PlantComp
	);

	void
	UpdateZoneSubCompPtrArray(
		int & Idx,
		int const ListNum,
		int const AirDistUnitNum,
		int const SubCompNum,
		int const PlantLoopType,
		int const PlantLoop,
		int const PlantBranch,
		int const PlantComp
	);

	void
	UpdateZoneSubSubCompPtrArray(
		int & Idx,
		int const ListNum,
		int const AirDistUnitNum,
		int const SubCompNum,
		int const SubSubCompNum,
		int const PlantLoopType,
		int const PlantLoop,
		int const PlantBranch,
		int const PlantComp
	);

	void
	UpdateAirSysCompPtrArray(
		int & Idx,
		int const AirLoopNum,
		int const BranchNum,
		int const CompNum,
		int const PlantLoopType,
		int const PlantLoop,
		int const PlantBranch,
		int const PlantComp
	);

	void
	UpdateAirSysSubCompPtrArray(
		int & Idx,
		int const AirLoopNum,
		int const BranchNum,
		int const CompNum,
		int const SubCompNum,
		int const PlantLoopType,
		int const PlantLoop,
		int const PlantBranch,
		int const PlantComp
	);

	void
	UpdateAirSysSubSubCompPtrArray(
		int & Idx,
		int const AirLoopNum,
		int const BranchNum,
		int const CompNum,
		int const SubCompNum,
		int const SubSubCompNum,
		int const PlantLoopType,
		int const PlantLoop,
		int const PlantBranch,
		int const PlantComp
	);

	void
	AllocateAndSetUpVentReports();

	void
	CreateEnergyReportStructure();

	// End Initialization Section of the Module
	//******************************************************************************

	// Beginning of Reporting subroutines for the SimAir Module
	// *****************************************************************************

	void
	ReportSystemEnergyUse();

	void
	CalcSystemEnergyUse(
		bool const CompLoadFlag,
		int const AirLoopNum,
		std::string const & CompType,
		int const EnergyType,
		Real64 const CompLoad,
		Real64 const CompEnergy
	);

	void
	ReportMaxVentilationLoads();

	void
	MatchPlantSys(
		int const AirLoopNum, // counter for zone air distribution inlets
		int const BranchNum // counter for zone air distribution inlets
	);

	void
	FindDemandSideMatch(
		std::string const & CompType, // Inlet node of the component to find the match of
		std::string const & CompName, // Outlet node of the component to find the match of
		bool & MatchFound, // Set to .TRUE. when a match is found
		int & MatchLoopType, // Loop number of the match
		int & MatchLoop, // Loop number of the match
		int & MatchBranch, // Branch number of the match
		int & MatchComp // Component number of the match
	);

	void
	ReportAirLoopConnections();

	//        End of Reporting subroutines for the SimAir Module
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

} // SystemReports

} // EnergyPlus

#endif
