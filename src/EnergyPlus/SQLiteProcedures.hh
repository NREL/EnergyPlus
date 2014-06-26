#ifndef SQLiteProcedures_hh_INCLUDED
#define SQLiteProcedures_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1D.hh>
#include <ObjexxFCL/FArray2D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

#include <sqlite3.h>

#include <fstream>
#include <memory>

namespace EnergyPlus {

class SQLite {
public:

	// Open the DB and prepare for writing data
	// Create all of the tables on construction
	SQLite();

	// Close database and free prepared statements
	virtual ~SQLite();

	bool writeOutputToSQLite() const;
	bool writeTabularDataToSQLite() const;

	// Begin a transaction
	void sqliteBegin();

	// Commit a transaction
	void sqliteCommit();

	void createSQLiteReportVariableDictionaryRecord(
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

	void createSQLiteReportVariableDataRecord(
		int const recordIndex,
		Real64 const value,
		Optional_int_const reportingInterval = _,
		Optional< Real64 const > minValue = _,
		Optional_int_const minValueDate = _,
		Optional< Real64 const > maxValue = _,
		Optional_int_const maxValueDate = _,
		Optional_int_const minutesPerTimeStep = _
	);

	void createSQLiteTimeIndexRecord(
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

	void addSQLiteZoneSizingRecord(
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

	void addSQLiteSystemSizingRecord(
		std::string const & SysName, // the name of the system
		std::string const & VarDesc, // the description of the input variable
		Real64 const VarValue // the value from the sizing calculation
	);

	void addSQLiteComponentSizingRecord(
		std::string const & CompType, // the type of the component
		std::string const & CompName, // the name of the component
		std::string const & VarDesc, // the description of the input variable
		Real64 const VarValue // the value from the sizing calculation
	);

	void createSQLiteRoomAirModelTable();

	void createSQLiteMeterDictionaryRecord(
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

	void createSQLiteMeterRecord(
		int const recordIndex,
		Real64 const value,
		Optional_int_const reportingInterval = _,
		Optional< Real64 const > minValue = _,
		Optional_int_const minValueDate = _,
		Optional< Real64 const > maxValue = _,
		Optional_int_const maxValueDate = _,
		Optional_int_const minutesPerTimeStep = _
	);

	void createSQLiteDaylightMapTitle(
		int const mapNum,
		std::string const & mapName,
		std::string const & environmentName,
		int const zone,
		std::string const & refPt1,
		std::string const & refPt2,
		Real64 const zCoord
	);

	void createSQLiteDaylightMap(
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

	void createSQLiteTabularDataRecords(
		FArray2D_string const & body, // row,column
		FArray1D_string const & rowLabels,
		FArray1D_string const & columnLabels,
		std::string const & ReportName,
		std::string const & ReportForString,
		std::string const & TableName
	);

	void createSQLiteSimulationsRecord( int const ID );

	void createSQLiteErrorRecord(
		int const simulationIndex,
		int const errorType,
		std::string const & errorMessage,
		int const cnt
	);

	void updateSQLiteErrorRecord( std::string const & errorMessage );

	void updateSQLiteSimulationRecord(
		bool const completed,
		bool const completedSuccessfully
	);

	void createSQLiteEnvironmentPeriodRecord();

	void sqliteWriteMessage(const std::string & message);

	void createZoneExtendedOutput();

	void initializeIndexes();

private:

	void createSQLiteZoneTable();
	void createSQLiteNominalLightingTable();
	void createSQLiteNominalPeopleTable();
	void createSQLiteNominalElectricEquipmentTable();
	void createSQLiteNominalGasEquipmentTable();
	void createSQLiteNominalSteamEquipmentTable();
	void createSQLiteNominalHotWaterEquipmentTable();
	void createSQLiteNominalOtherEquipmentTable();
	void createSQLiteNominalBaseboardHeatTable();
	void createSQLiteInfiltrationTable();
	void createSQLiteVentilationTable();
	void createSQLiteSurfacesTable();
	void createSQLiteConstructionsTable();
	void createSQLiteMaterialsTable();
	void createSQLiteZoneListTable();
	void createSQLiteZoneGroupTable();
	void createSQLiteSchedulesTable();

	int sqliteExecuteCommand(const std::string & commandBuffer);
	int sqlitePrepareStatement(sqlite3_stmt* & stmt, const std::string & stmtBuffer);

	int sqliteBindText(sqlite3_stmt * stmt, const int stmtInsertLocationIndex, const std::string & textBuffer);
	int sqliteBindInteger(sqlite3_stmt * stmt, const int stmtInsertLocationIndex, const int intToInsert);
	int sqliteBindDouble(sqlite3_stmt * stmt, const int stmtInsertLocationIndex, const double doubleToInsert);
	int sqliteBindNULL(sqlite3_stmt * stmt, const int stmtInsertLocationIndex);
	int sqliteBindLogical(sqlite3_stmt * stmt, const int stmtInsertLocationIndex, const bool valueToInsert);

	int sqliteStepCommand(sqlite3_stmt * stmt);
	int sqliteResetCommand(sqlite3_stmt * stmt);
	int sqliteClearBindings(sqlite3_stmt * stmt);
	int sqliteFinalizeCommand(sqlite3_stmt * stmt);

	int createSQLiteStringTableRecord(std::string const & stringValue,std::string const & stringType);

	static std::string storageType(const int storageTypeIndex);
	static std::string timestepTypeName(const int timestepType);
	static std::string reportingFreqName(const int reportingFreqIndex);

	static void adjustReportingHourAndMinutes(int & hour, int & minutes);
	// Given combinedString, parse out units and description.
	// Example: Given combinedString "Total Energy [GJ]", return "Total Energy"
	// in description and "GJ" in units.
	static void parseUnitsAndDescription(const std::string & combinedString, std::string & units, std::string & description);

	static int logicalToInteger(const bool value);

	void initializeReportVariableDataDictionaryTable();
	void initializeReportVariableDataTables();
	void initializeReportMeterDataDictionaryTable();
	void initializeReportMeterDataTables();
	void initializeTimeIndicesTable();
	void initializeZoneInfoTable();
	void initializeNominalPeopleTable();
	void initializeNominalLightingTable();
	void initializeNominalElectricEquipmentTable();
	void initializeNominalGasEquipmentTable();
	void initializeNominalSteamEquipmentTable();
	void initializeNominalHotWaterEquipmentTable();
	void initializeNominalOtherEquipmentTable();
	void initializeNominalBaseboardHeatTable();
	void initializeSurfacesTable();
	void initializeConstructionsTables();
	void initializeMaterialsTable();
	void initializeZoneListTable();
	void initializeZoneGroupTable();
	void initializeNominalInfiltrationTable();
	void initializeNominalVentilationTable();
	void initializeZoneSizingTable();
	void initializeSystemSizingTable();
	void initializeComponentSizingTable();
	void initializeRoomAirModelTable();
	void initializeSchedulesTable();
	void initializeDaylightMapTables();
	void initializeViews();
	void initializeSimulationsTable();
	void initializeEnvironmentPeriodsTable();
	void initializeErrorsTable();
	void initializeTabularDataTable();
	void initializeTabularDataView();

	bool m_writeOutputToSQLite;
	bool m_writeTabularDataToSQLite;
	int m_sqlDBTimeIndex;
	std::ofstream m_errorStream;
	sqlite3 * m_db;
	std::string m_dbName;
	sqlite3_stmt * m_reportVariableDataInsertStmt;
	sqlite3_stmt * m_reportVariableExtendedDataInsertStmt;
	sqlite3_stmt * m_timeIndexInsertStmt;
	sqlite3_stmt * m_reportVariableDictionaryInsertStmt;
	sqlite3_stmt * m_zoneInfoInsertStmt;
	sqlite3_stmt * m_nominalLightingInsertStmt;
	sqlite3_stmt * m_nominalElectricEquipmentInsertStmt;
	sqlite3_stmt * m_nominalGasEquipmentInsertStmt;
	sqlite3_stmt * m_nominalSteamEquipmentInsertStmt;
	sqlite3_stmt * m_nominalHotWaterEquipmentInsertStmt;
	sqlite3_stmt * m_nominalOtherEquipmentInsertStmt;
	sqlite3_stmt * m_nominalBaseboardHeatInsertStmt;
	sqlite3_stmt * m_surfaceInsertStmt;
	sqlite3_stmt * m_constructionInsertStmt;
	sqlite3_stmt * m_constructionLayerInsertStmt;
	sqlite3_stmt * m_materialInsertStmt;
	sqlite3_stmt * m_zoneListInsertStmt;
	sqlite3_stmt * m_zoneGroupInsertStmt;
	sqlite3_stmt * m_infiltrationInsertStmt;
	sqlite3_stmt * m_ventilationInsertStmt;
	sqlite3_stmt * m_nominalPeopleInsertStmt;
	sqlite3_stmt * m_zoneSizingInsertStmt;
	sqlite3_stmt * m_systemSizingInsertStmt;
	sqlite3_stmt * m_componentSizingInsertStmt;
	sqlite3_stmt * m_roomAirModelInsertStmt;
	sqlite3_stmt * m_groundTemperatureInsertStmt;
	sqlite3_stmt * m_weatherFileInsertStmt;
	sqlite3_stmt * m_meterDictionaryInsertStmt;
	sqlite3_stmt * m_reportMeterDataInsertStmt;
	sqlite3_stmt * m_meterExtendedDataInsertStmt;
	sqlite3_stmt * m_scheduleInsertStmt;
	sqlite3_stmt * m_daylightMapTitleInsertStmt;
	sqlite3_stmt * m_daylightMapHorlyTitleInsertStmt;
	sqlite3_stmt * m_daylightMapHorlyDataInsertStmt;
	sqlite3_stmt * m_environmentPeriodInsertStmt;
	sqlite3_stmt * m_simulationsInsertStmt;
	sqlite3_stmt * m_tabularDataInsertStmt;
	sqlite3_stmt * m_stringsInsertStmt;
	sqlite3_stmt * m_stringsLookUpStmt;
	sqlite3_stmt * m_errorInsertStmt;
	sqlite3_stmt * m_errorUpdateStmt;
	sqlite3_stmt * m_simulationUpdateStmt;

	static const int LocalReportEach;      //  Write out each time UpdatedataandLocalReport is called
	static const int LocalReportTimeStep;  //  Write out at 'EndTimeStepFlag'
	static const int LocalReportHourly;    //  Write out at 'EndHourFlag'
	static const int LocalReportDaily;     //  Write out at 'EndDayFlag'
	static const int LocalReportMonthly;   //  Write out at end of month (must be determined)
	static const int LocalReportSim;       //  Write out once per environment 'EndEnvrnFlag'
	static const std::string ReportNameId;         // These should be integers.  Why is this?
	static const std::string ReportForStringId;
	static const std::string TableNameId;
	static const std::string RowNameId;
	static const std::string ColumnNameId;
	static const std::string UnitsId;
};

extern std::unique_ptr<SQLite> sqlite;

} // EnergyPlus

#endif
