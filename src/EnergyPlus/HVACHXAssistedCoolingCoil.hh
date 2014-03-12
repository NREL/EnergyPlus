#ifndef HVACHXAssistedCoolingCoil_hh_INCLUDED
#define HVACHXAssistedCoolingCoil_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1D.hh>
#include <ObjexxFCL/Fstring.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace HVACHXAssistedCoolingCoil {

	// Using/Aliasing
	using DataGlobals::MaxNameLength;

	// Data
	// MODULE PARAMETER DEFINITIONS
	// Compressor operation
	extern int const On; // normal compressor operation
	extern int const Off; // signal DXCoil that compressor shouldn't run

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	extern int TotalNumHXAssistedCoils; // The total number of HXAssistedCoolingCoil compound objects
	extern FArray1D< Real64 > HXAssistedCoilOutletTemp; // Outlet temperature from this compound object
	extern FArray1D< Real64 > HXAssistedCoilOutletHumRat; // Outlet humidity ratio from this compound object
	// PUBLIC so others can access this information
	extern bool GetCoilsInputFlag; // Flag to allow input data to be retrieved from idf on first call to this subroutine
	extern FArray1D_bool CheckEquipName;

	// Subroutine Specifications for the Module
	// Driver/Manager Routines

	// Get Input routines for module

	// Initialization routines for module

	// Calculation algorithms for the module
	// Update routine to update output node information
	//PRIVATE UpdateHXAssistedCoolingCoil
	// Not required.  All updates done by the individual components
	// (cooling coil and air-to-air heat exchanger)

	// Reporting routines for module
	//PRIVATE ReportHXAssistedCoolingCoil
	// No reporting variables for this compound component

	// Utility routines for module

	// Types

	struct HXAssistedCoilParameters
	{
		// Members
		Fstring HXAssistedCoilType; // Type of HXAssistedCoolingCoil
		int HXAssistedCoilType_Num; // Numeric equivalent for hx assisted coil
		Fstring Name; // Name of the HXAssistedCoolingCoil
		Fstring CoolingCoilType; // Cooling coil type must be DetailedFlatCooling
		//  or Coil:DX:CoolingBypassFactorEmpirical
		int CoolingCoilType_Num; // Numeric Equivalent for cooling coil
		Fstring CoolingCoilName; // Cooling coil name
		int CoolingCoilIndex;
		Fstring HeatExchangerType; // Heat Exchanger type must be HeatExchanger:AirToAir:FlatPlate,
		// HeatExchanger:AirToAir:SensibleAndLatent or
		// HeatExchanger:Desiccant:BalancedFlow
		int HeatExchangerType_Num; // Numeric Equivalent for heat exchanger
		Fstring HeatExchangerName; // Heat Exchanger name
		int HeatExchangerIndex; // Heat Exchanger index
		int HXAssistedCoilInletNodeNum; // Inlet node to HXAssistedCoolingCoil compound object
		int HXAssistedCoilOutletNodeNum; // Outlet node to HXAssistedCoolingCoil compound object
		int HXExhaustAirInletNodeNum; // Inlet node number for air-to-air heat exchanger
		Real64 MassFlowRate; // Mass flow rate through HXAssistedCoolingCoil compound object
		int MaxIterCounter; // used in warning messages
		int MaxIterIndex; // used in warning messages

		// Default Constructor
		HXAssistedCoilParameters() :
			HXAssistedCoilType( MaxNameLength ),
			HXAssistedCoilType_Num( 0 ),
			Name( MaxNameLength ),
			CoolingCoilType( MaxNameLength ),
			CoolingCoilType_Num( 0 ),
			CoolingCoilName( MaxNameLength ),
			CoolingCoilIndex( 0 ),
			HeatExchangerType( MaxNameLength ),
			HeatExchangerType_Num( 0 ),
			HeatExchangerName( MaxNameLength ),
			HeatExchangerIndex( 0 ),
			HXAssistedCoilInletNodeNum( 0 ),
			HXAssistedCoilOutletNodeNum( 0 ),
			HXExhaustAirInletNodeNum( 0 ),
			MassFlowRate( 0.0 ),
			MaxIterCounter( 0 ),
			MaxIterIndex( 0 )
		{}

		// Member Constructor
		HXAssistedCoilParameters(
			Fstring const & HXAssistedCoilType, // Type of HXAssistedCoolingCoil
			int const HXAssistedCoilType_Num, // Numeric equivalent for hx assisted coil
			Fstring const & Name, // Name of the HXAssistedCoolingCoil
			Fstring const & CoolingCoilType, // Cooling coil type must be DetailedFlatCooling
			int const CoolingCoilType_Num, // Numeric Equivalent for cooling coil
			Fstring const & CoolingCoilName, // Cooling coil name
			int const CoolingCoilIndex,
			Fstring const & HeatExchangerType, // Heat Exchanger type must be HeatExchanger:AirToAir:FlatPlate,
			int const HeatExchangerType_Num, // Numeric Equivalent for heat exchanger
			Fstring const & HeatExchangerName, // Heat Exchanger name
			int const HeatExchangerIndex, // Heat Exchanger index
			int const HXAssistedCoilInletNodeNum, // Inlet node to HXAssistedCoolingCoil compound object
			int const HXAssistedCoilOutletNodeNum, // Outlet node to HXAssistedCoolingCoil compound object
			int const HXExhaustAirInletNodeNum, // Inlet node number for air-to-air heat exchanger
			Real64 const MassFlowRate, // Mass flow rate through HXAssistedCoolingCoil compound object
			int const MaxIterCounter, // used in warning messages
			int const MaxIterIndex // used in warning messages
		) :
			HXAssistedCoilType( MaxNameLength, HXAssistedCoilType ),
			HXAssistedCoilType_Num( HXAssistedCoilType_Num ),
			Name( MaxNameLength, Name ),
			CoolingCoilType( MaxNameLength, CoolingCoilType ),
			CoolingCoilType_Num( CoolingCoilType_Num ),
			CoolingCoilName( MaxNameLength, CoolingCoilName ),
			CoolingCoilIndex( CoolingCoilIndex ),
			HeatExchangerType( MaxNameLength, HeatExchangerType ),
			HeatExchangerType_Num( HeatExchangerType_Num ),
			HeatExchangerName( MaxNameLength, HeatExchangerName ),
			HeatExchangerIndex( HeatExchangerIndex ),
			HXAssistedCoilInletNodeNum( HXAssistedCoilInletNodeNum ),
			HXAssistedCoilOutletNodeNum( HXAssistedCoilOutletNodeNum ),
			HXExhaustAirInletNodeNum( HXExhaustAirInletNodeNum ),
			MassFlowRate( MassFlowRate ),
			MaxIterCounter( MaxIterCounter ),
			MaxIterIndex( MaxIterIndex )
		{}

	};

	// Object Data
	extern FArray1D< HXAssistedCoilParameters > HXAssistedCoil;

	// Functions

	void
	SimHXAssistedCoolingCoil(
		Fstring const & HXAssistedCoilName, // Name of HXAssistedCoolingCoil
		bool const FirstHVACIteration, // FirstHVACIteration flag
		int const CompOp, // compressor operation; 1=on, 0=off
		Real64 const PartLoadRatio, // Part load ratio of Coil:DX:CoolingBypassFactorEmpirical
		int & CompIndex,
		int const FanOpMode, // Allows the parent object to control fan operation
		Optional_bool_const HXUnitEnable = _, // flag to enable heat exchanger heat recovery
		Optional< Real64 const > OnOffAFR = _, // Ratio of compressor ON air mass flow rate to AVERAGE over time step
		Optional_bool_const EconomizerFlag = _, // OA sys or air loop economizer status
		Optional< Real64 > QTotOut = _ // the total cooling output of unit
	);

	// Get Input Section of the Module
	//******************************************************************************

	void
	GetHXAssistedCoolingCoilInput();

	// End of Get Input subroutines for this Module
	//******************************************************************************

	// Beginning Initialization Section of the Module
	//******************************************************************************

	void
	InitHXAssistedCoolingCoil( int const HXAssistedCoilNum ); // index for HXAssistedCoolingCoil

	// End Initialization Section of the Module
	//******************************************************************************

	void
	CalcHXAssistedCoolingCoil(
		int const HXAssistedCoilNum, // Index number for HXAssistedCoolingCoil
		bool const FirstHVACIteration, // FirstHVACIteration flag
		int const CompOp, // compressor operation; 1=on, 0=off
		Real64 const PartLoadRatio, // Cooling coil part load ratio
		bool const HXUnitOn, // Flag to enable heat exchanger
		int const FanOpMode, // Allows parent object to control fan operation
		Optional< Real64 const > OnOffAirFlow = _, // Ratio of compressor ON air mass flow to AVERAGE over time step
		Optional_bool_const EconomizerFlag = _ // OA (or airloop) econommizer status
	);

	//        End of Reporting subroutines for the HXAssistedCoil Module
	// *****************************************************************************

	void
	GetHXDXCoilIndex(
		Fstring const & HXDXCoilName,
		int & HXDXCoilIndex,
		bool & ErrorsFound,
		Optional_Fstring_const CurrentModuleObject = _
	);

	void
	CheckHXAssistedCoolingCoilSchedule(
		Fstring const & CompType, // unused1208
		Fstring const & CompName,
		Real64 & Value,
		int & CompIndex
	);

	Real64
	GetCoilCapacity(
		Fstring const & CoilType, // must match coil types in this module
		Fstring const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	int
	GetCoilGroupTypeNum(
		Fstring const & CoilType, // must match coil types in this module
		Fstring const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound, // set to true if problem
		Optional_bool_const PrintWarning = _ // prints warning message if true
	);

	int
	GetCoilObjectTypeNum(
		Fstring const & CoilType, // must match coil types in this module
		Fstring const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound, // set to true if problem
		Optional_bool_const PrintWarning = _ // prints warning message if true
	);

	int
	GetCoilInletNode(
		Fstring const & CoilType, // must match coil types in this module
		Fstring const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	int
	GetCoilWaterInletNode(
		Fstring const & CoilType, // must match coil types in this module
		Fstring const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	int
	GetCoilOutletNode(
		Fstring const & CoilType, // must match coil types in this module
		Fstring const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	Fstring
	GetHXDXCoilName(
		Fstring const & CoilType, // must match coil types in this module
		Fstring const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	int
	GetActualDXCoilIndex(
		Fstring const & CoilType, // must match coil types in this module
		Fstring const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	Fstring
	GetHXCoilType(
		Fstring const & CoilType, // must match coil types in this module
		Fstring const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	void
	GetHXCoilTypeAndName(
		Fstring const & CoilType, // must match coil types in this module
		Fstring const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound, // set to true if problem
		Fstring & CoolingCoilType, // returned type of cooling coil
		Fstring & CoolingCoilName // returned name of cooling coil
	);

	Real64
	GetCoilMaxWaterFlowRate(
		Fstring const & CoilType, // must match coil types in this module
		Fstring const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	Real64
	GetHXCoilAirFlowRate(
		Fstring const & CoilType, // must match coil types in this module
		Fstring const & CoilName, // must match coil names for the coil type
		bool & ErrorsFound // set to true if problem
	);

	bool
	VerifyHeatExchangerParent(
		Fstring const & HXType, // must match coil types in this module
		Fstring const & HXName // must match coil names for the coil type
	);

	//        End of Utility subroutines for the HXAssistedCoil Module
	// *****************************************************************************

	//     NOTICE

	//     NOTICE

	//     Copyright © 1996-2014 The Board of Trustees of the University of Illinois
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

} // HVACHXAssistedCoolingCoil

} // EnergyPlus

#endif
