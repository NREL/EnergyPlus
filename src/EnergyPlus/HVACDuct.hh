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

} // HVACDuct

} // EnergyPlus

#endif
