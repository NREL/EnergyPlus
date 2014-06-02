#ifndef SQLiteProcedures_hh_INCLUDED
#define SQLiteProcedures_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1S.hh>
#include <ObjexxFCL/FArray2S.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

#include <stdio.h>

namespace EnergyPlus {

class SQLiteOutput
{
	public:

	// Open the DB and prepare for writing data
	// Create all of the tables using the same approach as the legacy CreateSQLiteDatabase function
	// KSB: Should this throw if the DB cannot be opened?
	SQLiteOutput();
	virtual ~SQLiteOutput();

	// Begin a transaction
	// KSB: consider making this private?
	void
	SQLiteBegin();

	// Commit a transaction
	// KSB: consider making this private?
	void
	SQLiteCommit();

	// These are derived from the legacy functions 
	// moved here as class methods.

	void
	createSQLiteReportVariableDictionaryRecord(
		int const reportVariableReportID,
		int const storeTypeIndex,
		std::string const & indexGroup,
		std::string const & keyedValueString,
		std::string const & variableName,
		int const indexType,
		std::string const & units,
		int const reportingFreq,
		Optional_string_const ScheduleName = _
	);

	void
	createSQLiteReportVariableDataRecord(
		int const recordIndex,
		int const timeIndex,
		Real64 const value,
		Optional_int_const reportingInterval = _,
		Optional< Real64 const > minValue = _,
		Optional_int_const minValueDate = _,
		Optional< Real64 const > maxValue = _,
		Optional_int_const maxValueDate = _,
		Optional_int_const minutesPerTimeStep = _
	);

	int
	CreateSQLiteTimeIndexRecord(
		int const reportingInterval,
		int const recordIndex,
		int const CumlativeSimulationDays,
		Optional_int_const Month = _,
		Optional_int_const DayOfMonth = _,
		Optional_int_const Hour = _,
		Optional< Real64 const > EndMinute = _,
		Optional< Real64 const > StartMinute = _,
		Optional_int_const DST = _,
		Optional_string_const DayType = _
	);

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
	);

	void
	AddSQLiteSystemSizingRecord(
		std::string const & SysName, // the name of the system
		std::string const & VarDesc, // the description of the input variable
		Real64 const VarValue // the value from the sizing calculation
	);

	void
	AddSQLiteComponentSizingRecord(
		std::string const & CompType, // the type of the component
		std::string const & CompName, // the name of the component
		std::string const & VarDesc, // the description of the input variable
		Real64 const VarValue // the value from the sizing calculation
	);

	void
	CreateSQLiteRoomAirModelTable();

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
		Optional_string_const ScheduleName = _
	);

	void
	CreateSQLiteMeterRecord(
		int const recordIndex,
		int const timeIndex,
		Real64 const value,
		Optional_int_const reportingInterval = _,
		Optional< Real64 const > minValue = _,
		Optional_int_const minValueDate = _,
		Optional< Real64 const > maxValue = _,
		Optional_int_const maxValueDate = _,
		Optional_int_const minutesPerTimeStep = _
	);

	void
	SQLiteWriteMessageMacro( std::string const & message );

	void
	CreateSQLiteDaylightMapTitle(
		int const mapNum,
		std::string const & mapName,
		std::string const & environmentName,
		int const zone,
		std::string const & refPt1,
		std::string const & refPt2,
		Real64 const zCoord
	);

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
	);

	void
	CreateSQLiteTabularDataRecords(
		FArray2S_string const body, // row,column
		FArray1S_string const rowLabels,
		FArray1S_string const columnLabels,
		std::string const & ReportName,
		std::string const & ReportForString,
		std::string const & TableName
	);

	void
	CreateSQLiteSimulationsRecord( int const ID );

	void
	CreateSQLiteErrorRecord(
		int const simulationIndex,
		int const errorType,
		std::string const & errorMessage,
		int const cnt
	);

	void
	UpdateSQLiteErrorRecord( std::string const & errorMessage );

	void
	UpdateSQLiteSimulationRecord(
		bool const completed,
		bool const completedSuccessfully
	);

	private:

	int SQLiteExecuteCommand(const std::string & commandBuffer);

	int const m_maxMessageSize;
	bool const m_writeOutputToSQLite;
	bool const m_writeTabularDataToSQLite;
	int m_sqlDBTimeIndex;

	// KSB: I don't care for bringing in stdio.
	// Perhaps this class should be impled to keep the includes clean and hide all of this private junk?

	FILE * m_outputFile;
};

// KSB: These are the old SQLiteProcedures, 
// the above new class will replace these.

namespace SQLiteProcedures {

	// Data
	extern int const MaxMessageSize;
	extern bool WriteOutputToSQLite;
	extern bool WriteTabularDataToSQLite;

	extern int SQLdbTimeIndex;

	// Functions

	void
	SQLiteBegin();

	void
	SQLiteCommit();

	void
	CreateSQLiteDatabase();

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
		Optional_string_const ScheduleName = _
	);

	void
	CreateSQLiteReportVariableDataRecord(
		int const recordIndex,
		int const timeIndex,
		Real64 const value,
		Optional_int_const reportingInterval = _,
		Optional< Real64 const > minValue = _,
		Optional_int_const minValueDate = _,
		Optional< Real64 const > maxValue = _,
		Optional_int_const maxValueDate = _,
		Optional_int_const minutesPerTimeStep = _
	);

	int
	CreateSQLiteTimeIndexRecord(
		int const reportingInterval,
		int const recordIndex,
		int const CumlativeSimulationDays,
		Optional_int_const Month = _,
		Optional_int_const DayOfMonth = _,
		Optional_int_const Hour = _,
		Optional< Real64 const > EndMinute = _,
		Optional< Real64 const > StartMinute = _,
		Optional_int_const DST = _,
		Optional_string_const DayType = _
	);

	void
	CreateSQLiteZoneTable();

	void
	CreateSQLiteNominalLightingTable();

	void
	CreateSQLiteNominalPeopleTable();

	void
	CreateSQLiteNominalElectricEquipmentTable();

	void
	CreateSQLiteNominalGasEquipmentTable();

	void
	CreateSQLiteNominalSteamEquipmentTable();

	void
	CreateSQLiteNominalHotWaterEquipmentTable();

	void
	CreateSQLiteNominalOtherEquipmentTable();

	void
	CreateSQLiteNominalBaseboardHeatTable();

	void
	CreateSQLiteSurfacesTable();

	void
	CreateSQLiteConstructionsTable();

	void
	CreateSQLiteMaterialsTable();

	void
	CreateSQLiteZoneListTable();

	void
	CreateSQLiteZoneGroupTable();

	void
	CreateSQLiteInfiltrationTable();

	void
	CreateSQLiteVentilationTable();

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
	);

	void
	AddSQLiteSystemSizingRecord(
		std::string const & SysName, // the name of the system
		std::string const & VarDesc, // the description of the input variable
		Real64 const VarValue // the value from the sizing calculation
	);

	void
	AddSQLiteComponentSizingRecord(
		std::string const & CompType, // the type of the component
		std::string const & CompName, // the name of the component
		std::string const & VarDesc, // the description of the input variable
		Real64 const VarValue // the value from the sizing calculation
	);

	void
	CreateSQLiteRoomAirModelTable();

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
		Optional_string_const ScheduleName = _
	);

	void
	CreateSQLiteMeterRecord(
		int const recordIndex,
		int const timeIndex,
		Real64 const value,
		Optional_int_const reportingInterval = _,
		Optional< Real64 const > minValue = _,
		Optional_int_const minValueDate = _,
		Optional< Real64 const > maxValue = _,
		Optional_int_const maxValueDate = _,
		Optional_int_const minutesPerTimeStep = _
	);

	void
	SQLiteWriteMessageMacro( std::string const & message );

	void
	CreateZoneExtendedOutput();

	void
	CreateSQLiteDaylightMapTitle(
		int const mapNum,
		std::string const & mapName,
		std::string const & environmentName,
		int const zone,
		std::string const & refPt1,
		std::string const & refPt2,
		Real64 const zCoord
	);

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
	);

	void
	CreateSQLiteTabularDataRecords(
		FArray2S_string const body, // row,column
		FArray1S_string const rowLabels,
		FArray1S_string const columnLabels,
		std::string const & ReportName,
		std::string const & ReportForString,
		std::string const & TableName
	);

	void
	InitializeIndexes();

	void
	InitializeTabularDataTable();

	void
	InitializeTabularDataView();

	void
	CreateSQLiteSimulationsRecord( int const ID );

	void
	CreateSQLiteEnvironmentPeriodRecord();

	void
	CreateSQLiteErrorRecord(
		int const simulationIndex,
		int const errorType,
		std::string const & errorMessage,
		int const cnt
	);

	void
	UpdateSQLiteErrorRecord( std::string const & errorMessage );

	void
	UpdateSQLiteSimulationRecord(
		bool const completed,
		bool const completedSuccessfully
	);

	//// Consider making a class and making these private
	//int SQLiteExecuteCommand (const std::string & commandBuffer)
	//{
	//	char *zErrMsg = 0;
	//	int rc = -1;

	//	if (outputFile == NULL)
	//		fprintf(stderr, "SQLite3 message, can't open error file: sqlite.err\n");
	//	else
	//	{
	//		rc = sqlite3_exec(db, commandBuffer, callback, 0, &zErrMsg);
	//		if( rc != SQLITE_OK ){
	//			fprintf(outputFile, "SQLite3 message, error: %s\n", zErrMsg);
	//			sqlite3_free(zErrMsg);
	//		}
	//	}
	//	return rc;
	//}

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

#endif
