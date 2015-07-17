#ifndef SplitterComponent_hh_INCLUDED
#define SplitterComponent_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace SplitterComponent {

	// Using/Aliasing

	// Data
	// MODULE PARAMETERS:

	// MODULE PARAMETER DEFINITIONS
	// na

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	extern bool GetSplitterInputFlag;
	// Public because Used by SimAirServingZones and the Direct Air Unit
	extern int NumSplitters; // The Number of Splitters found in the Input
	extern Array1D_bool CheckEquipName;

	// Subroutine Specifications for the Module
	// Driver/Manager Routines

	// Get Input routines for module

	// Initialization routines for module

	// Algorithms for the module

	// Update routine to check convergence and update nodes

	// Reporting routines for module

	// Types

	struct SplitterConditions // public because USEd by SimAirServingZones and the Direct Air Unit
	{
		// Members
		std::string SplitterName; // Name of the Splitter
		Real64 InletTemp;
		Real64 InletHumRat;
		Real64 InletEnthalpy;
		Real64 InletPressure;
		int InletNode;
		Real64 InletMassFlowRate; // MassFlow through the Splitter being Simulated [kg/Sec]
		Real64 InletMassFlowRateMaxAvail; // Max Avail MassFlow through the Splitter being Simulated [kg/Sec]
		Real64 InletMassFlowRateMinAvail; // Min Avail MassFlow through the Splitter being Simulated [kg/Sec]
		int NumOutletNodes;
		Array1D_int OutletNode;
		Array1D< Real64 > OutletMassFlowRate;
		Array1D< Real64 > OutletMassFlowRateMaxAvail;
		Array1D< Real64 > OutletMassFlowRateMinAvail;
		Array1D< Real64 > OutletTemp;
		Array1D< Real64 > OutletHumRat;
		Array1D< Real64 > OutletEnthalpy;
		Array1D< Real64 > OutletPressure;

		// Default Constructor
		SplitterConditions() :
			InletTemp( 0.0 ),
			InletHumRat( 0.0 ),
			InletEnthalpy( 0.0 ),
			InletPressure( 0.0 ),
			InletNode( 0 ),
			InletMassFlowRate( 0.0 ),
			InletMassFlowRateMaxAvail( 0.0 ),
			InletMassFlowRateMinAvail( 0.0 ),
			NumOutletNodes( 0 )
		{}

		// Member Constructor
		SplitterConditions(
			std::string const & SplitterName, // Name of the Splitter
			Real64 const InletTemp,
			Real64 const InletHumRat,
			Real64 const InletEnthalpy,
			Real64 const InletPressure,
			int const InletNode,
			Real64 const InletMassFlowRate, // MassFlow through the Splitter being Simulated [kg/Sec]
			Real64 const InletMassFlowRateMaxAvail, // Max Avail MassFlow through the Splitter being Simulated [kg/Sec]
			Real64 const InletMassFlowRateMinAvail, // Min Avail MassFlow through the Splitter being Simulated [kg/Sec]
			int const NumOutletNodes,
			Array1_int const & OutletNode,
			Array1< Real64 > const & OutletMassFlowRate,
			Array1< Real64 > const & OutletMassFlowRateMaxAvail,
			Array1< Real64 > const & OutletMassFlowRateMinAvail,
			Array1< Real64 > const & OutletTemp,
			Array1< Real64 > const & OutletHumRat,
			Array1< Real64 > const & OutletEnthalpy,
			Array1< Real64 > const & OutletPressure
		) :
			SplitterName( SplitterName ),
			InletTemp( InletTemp ),
			InletHumRat( InletHumRat ),
			InletEnthalpy( InletEnthalpy ),
			InletPressure( InletPressure ),
			InletNode( InletNode ),
			InletMassFlowRate( InletMassFlowRate ),
			InletMassFlowRateMaxAvail( InletMassFlowRateMaxAvail ),
			InletMassFlowRateMinAvail( InletMassFlowRateMinAvail ),
			NumOutletNodes( NumOutletNodes ),
			OutletNode( OutletNode ),
			OutletMassFlowRate( OutletMassFlowRate ),
			OutletMassFlowRateMaxAvail( OutletMassFlowRateMaxAvail ),
			OutletMassFlowRateMinAvail( OutletMassFlowRateMinAvail ),
			OutletTemp( OutletTemp ),
			OutletHumRat( OutletHumRat ),
			OutletEnthalpy( OutletEnthalpy ),
			OutletPressure( OutletPressure )
		{}

	};

	// Object Data
	extern Array1D< SplitterConditions > SplitterCond;

	// Functions

	void
	SimAirLoopSplitter(
		std::string const & CompName,
		bool const FirstHVACIteration,
		bool const FirstCall,
		bool & SplitterInletChanged,
		int & CompIndex
	);

	//*******************************

	// Get Input Section of the Module
	//******************************************************************************

	void
	GetSplitterInput();

	// End of Get Input subroutines for the HB Module
	//******************************************************************************

	// Beginning Initialization Section of the Module
	//******************************************************************************

	void
	InitAirLoopSplitter(
		int const SplitterNum,
		bool const FirstHVACIteration,
		bool const FirstCall
	);

	// End Initialization Section of the Module
	//******************************************************************************

	// Begin Algorithm Section of the Module
	//******************************************************************************

	void
	CalcAirLoopSplitter(
		int const SplitterNum,
		bool const FirstCall
	);

	// End Algorithm Section of the Module
	// *****************************************************************************

	// Beginning of Update subroutines for the Splitter Module
	// *****************************************************************************

	void
	UpdateSplitter(
		int const SplitterNum,
		bool & SplitterInletChanged,
		bool const FirstCall
	);

	//        End of Update subroutines for the Splitter Module
	// *****************************************************************************

	// Beginning of Reporting subroutines for the Splitter Module
	// *****************************************************************************

	void
	ReportSplitter( int const SplitterNum );

	int
	GetSplitterOutletNumber(
		std::string const & SplitterName, // must match Splitter names for the Splitter type
		int const SplitterNum, // Index of Splitters
		bool & ErrorsFound // set to true if problem
	);

	Array1D_int
	GetSplitterNodeNumbers(
		std::string const & SplitterName, // must match Splitter names for the Splitter type
		int const SplitterNum, // Index of Splitters
		bool & ErrorsFound // set to true if problem
	);

	//        End of Reporting subroutines for the Splitter Module
	// *****************************************************************************

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

} // SplitterComponent

} // EnergyPlus

#endif
