#ifndef HeatBalanceManager_hh_INCLUDED
#define HeatBalanceManager_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array2D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace HeatBalanceManager {

	// Data
	// MODULE PARAMETER DEFINITIONS

	extern Array1D_string const PassFail;

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:

	//Real Variables for the Heat Balance Simulation
	//Variables used to determine warmup convergence
	extern Array1D< Real64 > MaxCoolLoadPrevDay; // Max cooling load from the previous day
	extern Array1D< Real64 > MaxCoolLoadZone; // Maximum zone cooling load from the current day
	extern Array1D< Real64 > MaxHeatLoadPrevDay; // Max heating load from the previous day
	extern Array1D< Real64 > MaxHeatLoadZone; // Maximum zone heating load from the current day
	extern Array1D< Real64 > MaxTempPrevDay; // Max temperature from the previous day
	extern Array1D< Real64 > MaxTempZone; // Maximum zone temperature from the current day
	extern Array1D< Real64 > MinTempPrevDay; // Min temperature from the previous day
	extern Array1D< Real64 > MinTempZone; // Minimum zone temperature from the current day

	//Variables used to report difference in temperature and load from the last two warmup days
	extern Array1D< Real64 > WarmupTempDiff; // Temperature difference between the last two warmup days
	extern Array1D< Real64 > WarmupLoadDiff; // Zone load differences between the last two warmup days
	extern Array1D< Real64 > TempZoneSecPrevDay; // Zone air temperature from the second last warmup day
	extern Array1D< Real64 > LoadZoneSecPrevDay; // Zone load from the second last warmup day
	extern Array1D< Real64 > TempZonePrevDay; // Zone air temperature from the previous day
	extern Array1D< Real64 > LoadZonePrevDay; // Zone load from the previuos day
	extern Array1D< Real64 > TempZone; // Zone air temperature from the current warmup day
	extern Array1D< Real64 > LoadZone; // Zone load from the current warmup day

	extern Array2D< Real64 > TempZoneRpt; // Zone air temperature to report (average over all warmup days)
	extern Array1D< Real64 > TempZoneRptStdDev; // Zone air temperature to report (std dev over all warmup days)
	extern Array2D< Real64 > LoadZoneRpt; // Zone load to report (average over all warmup days)
	extern Array1D< Real64 > LoadZoneRptStdDev; // Zone load to report (std dev over all warmup days)
	extern Array2D< Real64 > MaxLoadZoneRpt; // Maximum zone load for reporting calcs
	extern int CountWarmupDayPoints; // Count of warmup timesteps (to achieve warmup)

	extern std::string CurrentModuleObject; // to assist in getting input

	// Subroutine Specifications for the Heat Balance Module
	// Driver Routines

	// Input reader routines for the module

	// Initialization routines for module

	// Record Keeping/Utility Routines for Module

	// Reporting routines for module

	// Types

	struct WarmupConvergence
	{
		// Members
		Array1D_int PassFlag; // one flag (1=Fail), (2=Pass) for each of the 4 conditions of convergence from
		// warmup (PassFlag(1)=Max Temp, PassFlag(2)=Min Temp, PassFlag(3)=Max Heat Load
		// PassFlag(4)=Max Cool Load)
		// Following are stored test values for temperature and loads convergence
		Real64 TestMaxTempValue; // Max Temperature convergence value=ABS(MaxTempPrevDay(ZoneNum)-MaxTempZone(ZoneNum))
		Real64 TestMinTempValue; // Min Temperature convergence value=ABS(MinTempPrevDay(ZoneNum)-MinTempZone(ZoneNum))
		Real64 TestMaxHeatLoadValue; // Max Heat Load convergence value=
		//  ABS((MaxHeatLoadZone(ZoneNum)-MaxHeatLoadPrevDay(ZoneNum))/MaxHeatLoadZone(ZoneNum))
		Real64 TestMaxCoolLoadValue; // Max Cool Load convergence value=
		//  ABS((MaxCoolLoadZone(ZoneNum)-MaxCoolLoadPrevDay(ZoneNum))/MaxCoolLoadZone(ZoneNum))

		// Default Constructor
		WarmupConvergence() :
			PassFlag( 4, 2 ),
			TestMaxTempValue( 0.0 ),
			TestMinTempValue( 0.0 ),
			TestMaxHeatLoadValue( 0.0 ),
			TestMaxCoolLoadValue( 0.0 )
		{}

		// Member Constructor
		WarmupConvergence(
			Array1_int const & PassFlag, // one flag (1=Fail), (2=Pass) for each of the 4 conditions of convergence from
			Real64 const TestMaxTempValue, // Max Temperature convergence value=ABS(MaxTempPrevDay(ZoneNum)-MaxTempZone(ZoneNum))
			Real64 const TestMinTempValue, // Min Temperature convergence value=ABS(MinTempPrevDay(ZoneNum)-MinTempZone(ZoneNum))
			Real64 const TestMaxHeatLoadValue, // Max Heat Load convergence value=
			Real64 const TestMaxCoolLoadValue // Max Cool Load convergence value=
		) :
			PassFlag( 4, PassFlag ),
			TestMaxTempValue( TestMaxTempValue ),
			TestMinTempValue( TestMinTempValue ),
			TestMaxHeatLoadValue( TestMaxHeatLoadValue ),
			TestMaxCoolLoadValue( TestMaxCoolLoadValue )
		{}

	};

	// Object Data
	extern Array1D< WarmupConvergence > WarmupConvergenceValues;

	// Functions

	// Clears the global data in HeatBalanceManager.
	// Needed for unit tests, should not be normally called.
	void
	clear_state();

	void
	ManageHeatBalance();

	// Get Input Section of the Module
	//******************************************************************************

	void
	GetHeatBalanceInput();

	void
	CheckUsedConstructions( bool & ErrorsFound );

	bool
	CheckValidSimulationObjects();

	void
	SetPreConstructionInputParameters();

	void
	GetProjectControlData( bool & ErrorsFound ); // Set to true if errors detected during getting data

	void
	GetSiteAtmosphereData( bool & ErrorsFound );

	void
	GetMaterialData( bool & ErrorsFound ); // set to true if errors found in input

	void
	GetWindowGlassSpectralData( bool & ErrorsFound ); // set to true if errors found in input

	void
	ValidateMaterialRoughness(
		int const MaterNum, // Which Material number being validated.
		std::string const & Roughness, // Roughness String
		bool & ErrorsFound // If errors found
	);

	void
	GetConstructData( bool & ErrorsFound ); // If errors found in input

	void
	GetBuildingData( bool & ErrorsFound ); // If errors found in input

	void
	GetZoneData( bool & ErrorsFound ); // If errors found in input

	void
	ProcessZoneData(
		std::string const & cCurrentModuleObject,
		int const ZoneLoop,
		Array1_string const & cAlphaArgs,
		int & NumAlphas,
		Array1< Real64 > const & rNumericArgs,
		int & NumNumbers,
		Array1_bool const & lNumericFieldBlanks, //Unused
		Array1_bool const & lAlphaFieldBlanks,
		Array1_string const & cAlphaFieldNames,
		Array1_string const & cNumericFieldNames, //Unused
		bool & ErrorsFound // If errors found in input
	);

	// End of Get Input subroutines for the HB Module
	//******************************************************************************

	// Beginning Initialization Section of the Module
	//******************************************************************************

	void
	InitHeatBalance();

	void
	AllocateHeatBalArrays();

	// End Initialization Section of the Module
	//******************************************************************************

	// Beginning of Record Keeping subroutines for the HB Module
	// *****************************************************************************

	void
	RecKeepHeatBalance();

	void
	CheckWarmupConvergence();

	void
	ReportWarmupConvergence();

	//        End of Record Keeping subroutines for the HB Module
	// *****************************************************************************

	// Beginning of Reporting subroutines for the HB Module
	// *****************************************************************************

	void
	ReportHeatBalance();

	//        End of Reporting subroutines for the HB Module

	void
	GetFrameAndDividerData( bool & ErrorsFound ); // set to true if errors found in input

	void
	SearchWindow5DataFile(
		std::string const & DesiredFileName, // File name that contains the Window5 constructions.
		std::string const & DesiredConstructionName, // Name that will be searched for in the Window5 data file
		bool & ConstructionFound, // True if DesiredConstructionName is in the Window5 data file
		bool & EOFonFile, // True if EOF during file read
		bool & ErrorsFound // True if there is a problem with the entry requested from the data file
	);

	void
	SetStormWindowControl();

	void
	CreateFCfactorConstructions(
		int & ConstrNum, // Counter for Constructions
		bool & ErrorsFound // If errors found in input
	);

	void
	GetScheduledSurfaceGains( bool & ErrorsFound ); // If errors found in input

	void
	CheckScheduledSurfaceGains( int const ZoneNum ); // Zone number for which error check will be performed

	void
	CreateTCConstructions( bool & ErrorsFound ); // If errors found in input

	void
	SetupSimpleWindowGlazingSystem( int & MaterNum );

	void
	SetupComplexFenestrationMaterialInput(
		int & MaterNum, // num of material items thus far
		bool & ErrorsFound
	);

	void
	SetupComplexFenestrationStateInput(
		int & ConstrNum, // num of construction items thus far
		bool & ErrorsFound
	);

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

} // HeatBalanceManager

} // EnergyPlus

#endif
