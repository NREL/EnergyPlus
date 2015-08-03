#ifndef DataErrorTracking_hh_INCLUDED
#define DataErrorTracking_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace DataErrorTracking {

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:
	extern int const SearchCounts;
	extern Array1D_string const MessageSearch;
	extern Array1D_string const Summaries;
	// in below -- simple line end <CR>.  End of Whole message <CRE>
	extern std::string const MoreDetails_1; // InterZone Surface Areas -- mismatch
	extern std::string const MoreDetails_2; // Interzone surfaces - different zones
	extern std::string const MoreDetails_3; // Node Connection Errors
	extern std::string const MoreDetails_4; // InterZone Surface Azimuths -- mismatch
	extern std::string const MoreDetails_5; // InterZone Surface Tilts -- mismatch
	extern std::string const MoreDetails_6; // Likely non-planar surfaces
	extern std::string const MoreDetails_7; // Deprecated Features or Key Values
	extern std::string const MoreDetails_8; // Incorrect Floor Tilt
	extern std::string const MoreDetails_9; // Incorrect Roof/Ceiling Tilt
	extern std::string const MoreDetails_10; // Incomplete View factors
	extern std::string const MoreDetails_11; // Unbalanced exhaust air flow
	extern std::string const MoreDetails_12; // Loads Initialization did not Converge
	extern std::string const MoreDetails_13; // CalcDaylightMapPoints: Window
	extern std::string const MoreDetails_14; // Zone Air Heat Balance Warnings
	extern std::string const MoreDetails_15; // Occupant density is extremely high
	extern std::string const MoreDetails_16; // Temperature (low) out of bounds AND Temperature (high) out of bounds
	extern std::string const MoreDetails_18; // Nominally unused constructions
	extern std::string const MoreDetails_19; // InfraredTransparent constructions in non-interzone surfaces
	extern std::string const MoreDetails_20; // No reporting elements requested
	extern Array1D_string const MoreDetails; // Details 16 applies to both temperature out of bounds | errors.

	extern int const MaxRecurringErrorMsgLength; // Maximum error message length for recurring error messages

	// DERIVED TYPE DEFINITIONS

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// MODULE VARIABLE DECLARATIONS:
	extern Array1D_int MatchCounts;
	extern bool AbortProcessing; // Flag used to if currently in "abort processing"
	extern int NumRecurringErrors; // Number of stored recurring error messages
	extern int TotalSevereErrors; // Counter
	extern int TotalWarningErrors; // Counter
	extern int TotalSevereErrorsDuringWarmup; // Counter
	extern int TotalWarningErrorsDuringWarmup; // Counter
	extern int TotalSevereErrorsDuringSizing; // Counter
	extern int TotalWarningErrorsDuringSizing; // Counter
	extern int TotalMultipliedWindows; // Counter
	extern int TotalCoincidentVertices; // Counter
	extern int TotalDegenerateSurfaces; // Counter
	extern int TotalReceivingNonConvexSurfaces; // Counter
	extern int TotalCastingNonConvexSurfaces; // Counter
	extern int TotalRoomAirPatternTooLow; // Counter
	extern int TotalRoomAirPatternTooHigh; // Counter
	extern bool AskForConnectionsReport; // Flag used to tell when connections should be reported
	extern bool AskForSurfacesReport; // Flag used to tell when surfaces should be reported
	extern bool AskForPlantCheckOnAbort; // flag used to tell if plant structure can be checked
	extern bool ExitDuringSimulations; // flag used to tell if program is in simulation mode when fatal occurs
	extern std::string LastSevereError;

	// Types

	struct RecurringErrorData
	{
		// Members
		std::string Message; // Message to be written to "error file" at end of simulation
		int Count; // Count of total times this recurring error message has been called
		int WarmupCount; // Count of times this recurring error message has been called during warmup
		int SizingCount; // Count of times this recurring error message has been called during sizing
		Real64 MaxValue; // Max of the values passed for this recurring error message
		Real64 MinValue; // Min of the values passed for this recurring error message
		Real64 SumValue; // Sum of the values passed for this recurring error message
		std::string MaxUnits; // units for Max values
		std::string MinUnits; // units for Min values
		std::string SumUnits; // units for Sum values
		bool ReportMax; // Flag to report max value
		bool ReportMin; // Flag to report min value
		bool ReportSum; // Flag to report sum value

		// Default Constructor
		RecurringErrorData() :
			Count( 0 ),
			WarmupCount( 0 ),
			SizingCount( 0 ),
			MaxValue( 0.0 ),
			MinValue( 0.0 ),
			SumValue( 0.0 ),
			ReportMax( false ),
			ReportMin( false ),
			ReportSum( false )
		{}

		// Member Constructor
		RecurringErrorData(
			std::string const & Message, // Message to be written to "error file" at end of simulation
			int const Count, // Count of total times this recurring error message has been called
			int const WarmupCount, // Count of times this recurring error message has been called during warmup
			int const SizingCount, // Count of times this recurring error message has been called during sizing
			Real64 const MaxValue, // Max of the values passed for this recurring error message
			Real64 const MinValue, // Min of the values passed for this recurring error message
			Real64 const SumValue, // Sum of the values passed for this recurring error message
			std::string const & MaxUnits, // units for Max values
			std::string const & MinUnits, // units for Min values
			std::string const & SumUnits, // units for Sum values
			bool const ReportMax, // Flag to report max value
			bool const ReportMin, // Flag to report min value
			bool const ReportSum // Flag to report sum value
		) :
			Message( Message ),
			Count( Count ),
			WarmupCount( WarmupCount ),
			SizingCount( SizingCount ),
			MaxValue( MaxValue ),
			MinValue( MinValue ),
			SumValue( SumValue ),
			MaxUnits( MaxUnits ),
			MinUnits( MinUnits ),
			SumUnits( SumUnits ),
			ReportMax( ReportMax ),
			ReportMin( ReportMin ),
			ReportSum( ReportSum )
		{}

	};

	// Object Data
	extern Array1D< RecurringErrorData > RecurringErrors;

} // DataErrorTracking

} // EnergyPlus

#endif
