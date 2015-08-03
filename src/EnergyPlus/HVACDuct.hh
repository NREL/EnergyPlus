#ifndef HVACDuct_hh_INCLUDED
#define HVACDuct_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace HVACDuct {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// na

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	extern int NumDucts;
	extern Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE HVACDuct:

	// <name Public routines, optionally name Private routines within this module>

	// Types

	struct DuctData
	{
		// Members
		std::string Name; // duct unique name
		int InletNodeNum; // inlet node number
		int OutletNodeNum; // outlet node number

		// Default Constructor
		DuctData() :
			InletNodeNum( 0 ),
			OutletNodeNum( 0 )
		{}

		// Member Constructor
		DuctData(
			std::string const & Name, // duct unique name
			int const InletNodeNum, // inlet node number
			int const OutletNodeNum // outlet node number
		) :
			Name( Name ),
			InletNodeNum( InletNodeNum ),
			OutletNodeNum( OutletNodeNum )
		{}

	};

	// Object Data
	extern Array1D< DuctData > Duct;

	// Functions

	void
	SimDuct(
		std::string const & CompName, // name of the duct component
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep !unused1208
		int & CompIndex // index of duct component
	);

	void
	GetDuctInput();

	void
	InitDuct( int const DuctNum ); // number of the current duct being simulated

	void
	CalcDuct( int const DuctNum ); // number of the current duct being simulated !unused1208

	void
	UpdateDuct( int const DuctNum ); // number of the current duct being simulated

	void
	ReportDuct( int const DuctNum ); // number of the current duct being simulated !unused1208

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

} // HVACDuct

} // EnergyPlus

#endif
