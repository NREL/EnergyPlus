#ifndef SizingManager_hh_INCLUDED
#define SizingManager_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace SizingManager {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS: none

	// DERIVED TYPE DEFINITIONS: none

	// INTERFACE BLOCK SPECIFICATIONS: none

	// MODULE VARIABLE DECLARATIONS:
	extern int NumAirLoops;

	// SUBROUTINE SPECIFICATIONS FOR MODULE SimulationManager

	// Types

	struct ZoneListData
	{
		// Members
		std::string Name;
		int NumOfZones;
		Array1D_int Zones;

		// Default Constructor
		ZoneListData() :
			NumOfZones( 0 )
		{}

		// Member Constructor
		ZoneListData(
			std::string const & Name,
			int const NumOfZones,
			Array1_int const & Zones
		) :
			Name( Name ),
			NumOfZones( NumOfZones ),
			Zones( Zones )
		{}

	};

	// Functions
	void
	clear_state();

	void
	ManageSizing();

	void
	GetOARequirements();

	void
	ProcessInputOARequirements(
		std::string const & cCurrentModuleObject,
		int const OAIndex,
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

	void
	GetZoneAirDistribution();

	void
	GetZoneHVACSizing();

	void
	GetSizingParams();

	void
	GetZoneSizingInput();

	void
	GetZoneAndZoneListNames(
		bool & ErrorsFound,
		int & NumZones,
		Array1D_string & ZoneNames,
		int & NumZoneLists,
		Array1D< ZoneListData > & ZoneListNames
	);

	void
	GetSystemSizingInput();

	void
	GetPlantSizingInput();

	void
	SetupZoneSizing( bool & ErrorsFound );

	void
	ReportZoneSizing(
		std::string const & ZoneName, // the name of the zone
		std::string const & LoadType, // the description of the input variable
		Real64 const CalcDesLoad, // the value from the sizing calculation [W]
		Real64 const UserDesLoad, // the value from the sizing calculation modified by user input [W]
		Real64 const CalcDesFlow, // calculated design air flow rate [m3/s]
		Real64 const UserDesFlow, // user input or modified design air flow rate [m3/s]
		std::string const & DesDayName, // the name of the design day that produced the peak
		std::string const & PeakHrMin, // time stamp of the peak
		Real64 const PeakTemp, // temperature at peak [C]
		Real64 const PeakHumRat, // humidity ratio at peak [kg water/kg dry air]
		Real64 const FloorArea, // zone floor area [m2]
		Real64 const TotOccs, // design number of occupants for the zone
		Real64 const MinOAVolFlow, // zone design minimum outside air flow rate [m3/s]
		Real64 const DOASHeatAddRate // zone design heat addition rate from the DOAS [W]
	);

	void
	ReportSysSizing(
		std::string const & SysName, // the name of the zone
		std::string const & VarDesc, // the description of the input variable
		Real64 const VarValue // the value from the sizing calculation
	);

} // SizingManager

} // EnergyPlus

#endif
