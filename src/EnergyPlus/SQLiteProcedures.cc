// ObjexxFCL Headers

// EnergyPlus Headers
#include <SQLiteProcedures.hh>
#include <DataGlobals.hh>
#include <DataPrecisionGlobals.hh>
#include <InputProcessor.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace SQLiteProcedures {

	// Note most of the procedures below are stubs -- they have no function other than to satisfy compiler requirements

	// Using/Aliasing
	using namespace DataPrecisionGlobals;

	// Data
	int const MaxMessageSize( 4096 );
	bool WriteOutputToSQLite( false );
	bool WriteTabularDataToSQLite( false );

	int SQLdbTimeIndex( 0 );

	// Functions

	void
	SQLiteBegin()
	{

		// Locals
		int result;

	}

	void
	SQLiteCommit()
	{

		// Locals
		int result;

	}

	void
	CreateSQLiteDatabase()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   September 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine determines if there is a request for SQLite data and fatals if there is.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using InputProcessor::GetNumObjectsFound;

		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

		if ( GetNumObjectsFound( "Output:SQLite" ) > 0 ) {
			ShowSevereError( "SQLite is not available in this version" );
			ShowContinueError( "Request for SQLite output will be ignored" );
			WriteOutputToSQLite = false;
		} else {
			WriteOutputToSQLite = false;
		}

	}

	void
	CreateSQLiteReportVariableDictionaryRecord(
		int const reportVariableReportID,
		int const storeTypeIndex,
		std::string const & indexGroup,
		std::string const & keyedValueString,
		std::string const & variableName,
		int const indexType,
		std::string const & units,
		int const reportingFreq,
		Optional_string_const ScheduleName
	)
	{

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

	}

	void
	CreateSQLiteReportVariableDataRecord(
		int const recordIndex,
		int const timeIndex,
		Real64 const value,
		Optional_int_const reportingInterval,
		Optional< Real64 const > minValue,
		Optional_int_const minValueDate,
		Optional< Real64 const > maxValue,
		Optional_int_const maxValueDate,
		Optional_int_const minutesPerTimeStep
	)
	{

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

	}

	int
	CreateSQLiteTimeIndexRecord(
		int const reportingInterval,
		int const recordIndex,
		int const CumlativeSimulationDays,
		Optional_int_const Month,
		Optional_int_const DayOfMonth,
		Optional_int_const Hour,
		Optional< Real64 const > EndMinute,
		Optional< Real64 const > StartMinute,
		Optional_int_const DST,
		Optional_string_const DayType
	)
	{

		// Return value
		int CreateSQLiteTimeIndexRecord;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:
		// See Module Parameter Definitons for LocalReportEach, LocalReportTimeStep, LocalReportHourly, etc.

		CreateSQLiteTimeIndexRecord = -1;

		return CreateSQLiteTimeIndexRecord;
	}

	void
	CreateSQLiteZoneTable()
	{

	}

	void
	CreateSQLiteNominalLightingTable()
	{

	}

	void
	CreateSQLiteNominalPeopleTable()
	{

	}

	void
	CreateSQLiteNominalElectricEquipmentTable()
	{

	}

	void
	CreateSQLiteNominalGasEquipmentTable()
	{

	}

	void
	CreateSQLiteNominalSteamEquipmentTable()
	{

	}

	void
	CreateSQLiteNominalHotWaterEquipmentTable()
	{

	}

	void
	CreateSQLiteNominalOtherEquipmentTable()
	{

	}

	void
	CreateSQLiteNominalBaseboardHeatTable()
	{

	}

	void
	CreateSQLiteSurfacesTable()
	{

	}

	void
	CreateSQLiteConstructionsTable()
	{

	}

	void
	CreateSQLiteMaterialsTable()
	{

	}

	void
	CreateSQLiteZoneListTable()
	{

	}

	void
	CreateSQLiteZoneGroupTable()
	{

	}

	void
	CreateSQLiteInfiltrationTable()
	{

	}

	void
	CreateSQLiteVentilationTable()
	{

	}

	void
	AddSQLiteZoneSizingRecord(
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
		Real64 const MinOAVolFlow // zone design minimum outside air flow rate [m3/s]
	)
	{

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

	}

	void
	AddSQLiteSystemSizingRecord(
		std::string const & SysName, // the name of the system
		std::string const & VarDesc, // the description of the input variable
		Real64 const VarValue // the value from the sizing calculation
	)
	{

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

	}

	void
	AddSQLiteComponentSizingRecord(
		std::string const & CompType, // the type of the component
		std::string const & CompName, // the name of the component
		std::string const & VarDesc, // the description of the input variable
		Real64 const VarValue // the value from the sizing calculation
	)
	{

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

	}

	void
	CreateSQLiteRoomAirModelTable()
	{

	}

	void
	CreateSQLiteMeterDictionaryRecord(
		int const meterReportID,
		int const storeTypeIndex,
		std::string const & indexGroup,
		std::string const & keyedValueString,
		std::string const & variableName,
		int const indexType,
		std::string const & units,
		int const reportingFreq,
		Optional_string_const ScheduleName
	)
	{

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

	}

	void
	CreateSQLiteMeterRecord(
		int const recordIndex,
		int const timeIndex,
		Real64 const value,
		Optional_int_const reportingInterval,
		Optional< Real64 const > minValue,
		Optional_int_const minValueDate,
		Optional< Real64 const > maxValue,
		Optional_int_const maxValueDate,
		Optional_int_const minutesPerTimeStep
	)
	{

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

	}

	void
	SQLiteWriteMessageMacro( std::string const & message )
	{

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

	}

	void
	CreateZoneExtendedOutput()
	{

	}

	void
	CreateSQLiteDaylightMapTitle(
		int const mapNum,
		std::string const & mapName,
		std::string const & environmentName,
		int const zone,
		std::string const & refPt1,
		std::string const & refPt2,
		Real64 const zCoord
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Greg Stark
		//       DATE WRITTEN   Sept 2008
		//       MODIFIED       April 2010, Kyle Benne, Added zCoord
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:

		// METHODOLOGY EMPLOYED:
		// Standard SQL92 queries and commands via the Fortran SQLite3 API

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

	}

	void
	CreateSQLiteDaylightMap(
		int const mapNum,
		int const month,
		int const dayOfMonth,
		int const hourOfDay,
		int const nX,
		FArray1S< Real64 > const x,
		int const nY,
		FArray1S< Real64 > const y,
		FArray2S< Real64 > const illuminance
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Greg Stark
		//       DATE WRITTEN   Sept 2008
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:

		// METHODOLOGY EMPLOYED:
		// Standard SQL92 queries and commands via the Fortran SQLite3 API

		// REFERENCES:
		// na

		// USE STATEMENTS:

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

	}

	void
	CreateSQLiteTabularDataRecords(
		FArray2S_string const body, // row,column
		FArray1S_string const rowLabels,
		FArray1S_string const columnLabels,
		std::string const & ReportName,
		std::string const & ReportForString,
		std::string const & TableName
	)
	{

		// Argument array dimensioning

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

	}

	void
	InitializeIndexes()
	{

		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

	}

	void
	InitializeTabularDataTable()
	{

		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

	}

	void
	InitializeTabularDataView()
	{

	}

	void
	CreateSQLiteSimulationsRecord( int const ID )
	{

		// Using/Aliasing

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

	}

	void
	CreateSQLiteEnvironmentPeriodRecord()
	{

	}

	void
	CreateSQLiteErrorRecord(
		int const simulationIndex,
		int const errorType,
		std::string const & errorMessage,
		int const cnt
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Kyle Benne
		//       DATE WRITTEN   August 2010
		//       RE-ENGINEERED  na
		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine writes the error data to the Errors table
		// METHODOLOGY EMPLOYED:
		// Standard SQL92 queries and commands via the Fortran SQLite3 API
		// REFERENCES:
		// na
		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

	}

	void
	UpdateSQLiteErrorRecord( std::string const & errorMessage )
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Kyle Benne
		//       DATE WRITTEN   August 2010
		//       RE-ENGINEERED  na
		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine updates error records in the Errors table.
		// This is used to append text to an error that continues on
		// to the next line.  The errorMessage is always appended to the
		// last record inserted into the Errors table.
		// METHODOLOGY EMPLOYED:
		// Standard SQL92 queries and commands via the Fortran SQLite3 API
		// REFERENCES:
		// na
		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

	}

	void
	UpdateSQLiteSimulationRecord(
		bool const completed,
		bool const completedSuccessfully
	)
	{
		// SUBROUTINE INFORMATION:
		//       AUTHOR         Kyle Benne
		//       DATE WRITTEN   August 2010
		//       RE-ENGINEERED  na
		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine updates simulation records in the Simulations table.
		// A simulation record is first inserted as
		// completed = false and
		// completedSuccessfully = false
		// This subroutine updates those records.
		// METHODOLOGY EMPLOYED:
		// Standard SQL92 queries and commands via the Fortran SQLite3 API
		// REFERENCES:
		// na
		// USE STATEMENTS:

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

	}

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

	//     Copyright © 2008 Building Synergies, LLC.  All rights reserved.

} // SQLiteProcedures

} // EnergyPlus
