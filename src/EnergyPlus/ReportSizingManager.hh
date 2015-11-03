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

} // ReportSizingManager

} // EnergyPlus

#endif
