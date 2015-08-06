#ifndef ReportSizingManager_hh_INCLUDED
#define ReportSizingManager_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace ReportSizingManager {

	// Functions

	void
	ReportSizingOutput(
		std::string const & CompType, // the type of the component
		std::string const & CompName, // the name of the component
		std::string const & VarDesc, // the description of the input variable
		Real64 const VarValue, // the value from the sizing calculation
		Optional_string_const UsrDesc = _, // the description of a user-specified variable
		Optional< Real64 const > UsrValue = _ // the value from the user for the desc item
	);


	void
	RequestSizing(
		std::string const & CompType, // type of component
		std::string const & CompName, // name of component
		int const SizingType, // integerized type of sizing requested (see DataHVACGlobals, e.g. CoolingCapacitySizing)
		std::string const & SizingString, // string containing info for eio report
		Real64 & SizingResult, // result of the sizing procedure
		bool const PrintWarningFlag, // TRUE when requesting output (eio) reporting
		std::string const & CallingRoutine // name of calling rotuine for warning messages
	);

	void
	GetCoilDesFlowT(
		int SysNum, // central air system index
		Real64 CpAir, // specific heat to be used in calculations [J/kgC]
		Real64 & DesFlow, // returned design mass flow [kg/s]
		Real64 & DesExitTemp // returned design coil exit temperature [kg/s]
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

} // ReportSizingManager

} // EnergyPlus

#endif
