#ifndef DataOutputs_hh_INCLUDED
#define DataOutputs_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace DataOutputs {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS:
	extern int const NumMonthlyReports;
	extern Array1D_string const MonthlyNamedReports;

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	extern int MaxConsideredOutputVariables; // Max Array size for OutputVariable pre-scanned
	extern int NumConsideredOutputVariables; // Number of variables - pre-scanned, allowed for output
	extern int iNumberOfRecords; // Number of records in input
	extern int iNumberOfDefaultedFields; // number of defaulted fields
	extern int iTotalFieldsWithDefaults; // number of fields that can be defaulted
	extern int iNumberOfAutoSizedFields; // number of autosized fields
	extern int iTotalAutoSizableFields; // number of fields that can be autosized
	extern int iNumberOfAutoCalcedFields; // number of autocalculated fields
	extern int iTotalAutoCalculatableFields; // number of fields that can be autocalculated

	// Types

	struct OutputReportingVariables // Linked list of variables and keys
	{
		// Members
		std::string Key; // could be a key or "*"  (upper case)
		std::string VarName; // variable name (upper case)
		int Previous; // Pointer to Previous of same variable name
		int Next; // Pointer to Next of same variable name

		// Default Constructor
		OutputReportingVariables() :
			Previous( 0 ),
			Next( 0 )
		{}

		// Member Constructor
		OutputReportingVariables(
			std::string const & Key, // could be a key or "*"  (upper case)
			std::string const & VarName, // variable name (upper case)
			int const Previous, // Pointer to Previous of same variable name
			int const Next // Pointer to Next of same variable name
		) :
			Key( Key ),
			VarName( VarName ),
			Previous( Previous ),
			Next( Next )
		{}

	};

	// Object Data
	extern Array1D< OutputReportingVariables > OutputVariablesForSimulation;

	// Functions

	// Clears the global data in DataOutputs.
	// Needed for unit tests, should not be normally called.
	void
	clear_state();

	bool
	FindItemInVariableList(
		std::string const & KeyedValue,
		std::string const & VariableName
	);

} // DataOutputs

} // EnergyPlus

#endif
