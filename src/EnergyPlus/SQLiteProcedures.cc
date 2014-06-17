// ObjexxFCL Headers

// EnergyPlus Headers
#include "SQLiteProcedures.hh"
#include "DataGlobals.hh"
#include "DataStringGlobals.hh"
#include "DataEnvironment.hh"
#include "DataPrecisionGlobals.hh"
#include "DataRoomAirModel.hh"
#include "InputProcessor.hh"
#include "UtilityRoutines.hh"
#include "General.hh"

#include <sstream>

namespace EnergyPlus {

const int SQLite::LocalReportEach     = -1;   // Write out each time UpdatedataandLocalReport is called
const int SQLite::LocalReportTimeStep =  0;   // Write out at 'EndTimeStepFlag'
const int SQLite::LocalReportHourly   =  1;   // Write out at 'EndHourFlag'
const int SQLite::LocalReportDaily    =  2;   // Write out at 'EndDayFlag'
const int SQLite::LocalReportMonthly  =  3;   // Write out at end of month (must be determined)
const int SQLite::LocalReportSim      =  4;   // Write out once per environment 'EndEnvrnFlag'
const std::string SQLite::ReportNameId        = "1";  // These should be integers.  Why is this?
const std::string SQLite::ReportForStringId   = "2";
const std::string SQLite::TableNameId         = "3";
const std::string SQLite::RowNameId           = "4";
const std::string SQLite::ColumnNameId        = "5";
const std::string SQLite::UnitsId             = "6";

SQLite::SQLite()
	: 
	m_writeOutputToSQLite(true),
	m_writeTabularDataToSQLite(true),
	m_sqlDBTimeIndex(0),
	m_errorStream("sqlite.err", std::ofstream::out | std::ofstream::trunc),
	m_db(nullptr),
	m_dbName("eplusout.sql"),
	m_reportVariableDataInsertStmt(nullptr),
	m_reportVariableExtendedDataInsertStmt(nullptr),
	m_timeIndexInsertStmt(nullptr),
	m_reportVariableDictionaryInsertStmt(nullptr),
	m_zoneInfoInsertStmt(nullptr),
	m_nominalLightingInsertStmt(nullptr),
	m_nominalElectricEquipmentInsertStmt(nullptr),
	m_nominalGasEquipmentInsertStmt(nullptr),
	m_nominalSteamEquipmentInsertStmt(nullptr),
	m_nominalHotWaterEquipmentInsertStmt(nullptr),
	m_nominalOtherEquipmentInsertStmt(nullptr),
	m_nominalBaseboardHeatInsertStmt(nullptr),
	m_surfaceInsertStmt(nullptr),
	m_constructionInsertStmt(nullptr),
	m_constructionLayerInsertStmt(nullptr),
	m_materialInsertStmt(nullptr),
	m_zoneListInsertStmt(nullptr),
	m_zoneGroupInsertStmt(nullptr),
	m_infiltrationInsertStmt(nullptr),
	m_ventilationInsertStmt(nullptr),
	m_nominalPeopleInsertStmt(nullptr),
	m_zoneSizingInsertStmt(nullptr),
	m_systemSizingInsertStmt(nullptr),
	m_componentSizingInsertStmt(nullptr),
	m_roomAirModelInsertStmt(nullptr),
	m_groundTemperatureInsertStmt(nullptr),
	m_weatherFileInsertStmt(nullptr),
	m_meterDictionaryInsertStmt(nullptr),
	m_reportMeterDataInsertStmt(nullptr),
	m_meterExtendedDataInsertStmt(nullptr),
	m_scheduleInsertStmt(nullptr),
	m_daylightMapTitleInsertStmt(nullptr),
	m_daylightMapHorlyTitleInsertStmt(nullptr),
	m_daylightMapHorlyDataInsertStmt(nullptr),
	m_environmentPeriodInsertStmt(nullptr),
	m_simulationsInsertStmt(nullptr),
	m_tabularDataInsertStmt(nullptr),
	m_stringsInsertStmt(nullptr),
	m_stringsLookUpStmt(nullptr),
	m_errorInsertStmt(nullptr),
	m_errorUpdateStmt(nullptr),
	m_simulationUpdateStmt(nullptr)
	//m_variableExtendedDataIndex(0),
	//m_variableOID(0)
{
	int rc = -1;
	bool ok = true;

	// Test if we can write to the sqlite error file
	//  Does there need to be a seperate sqlite.err file at all?  Consider using eplusout.err  
	if( m_errorStream.is_open() )
	{
		m_errorStream << "SQLite3 message, sqlite.err open for processing!" << std::endl;
	}
	else 
	{
		ok = false;
	}

		// Test if we can create a new file named m_dbName
	if( ok )
	{
		std::ofstream test(m_dbName, std::ofstream::out | std::ofstream::trunc);
		if( test.is_open() )
		{
			test.close();
		}
		else
		{
			ok = false;
		}
	}

	// Test if we can write to the database
	// If we can't then there are probably locks on the database
	if( ok )
	{
		sqlite3_open_v2(m_dbName.c_str(), &m_db, SQLITE_OPEN_READWRITE, nullptr);
		char * zErrMsg = nullptr;
		rc = sqlite3_exec(m_db, "CREATE TABLE Test(x INTEGER PRIMARY KEY)", nullptr, 0, &zErrMsg);
		sqlite3_close(m_db);
		if( rc )
		{
			m_errorStream << "SQLite3 message, can't get exclusive lock on existing database: " << sqlite3_errmsg(m_db) << std::endl;
			ok = false;
		}
		else
		{
			// Remmove test db
			rc = remove( m_dbName.c_str() );
			if( rc )
			{
				m_errorStream << "SQLite3 message, can't remove old database: " << sqlite3_errmsg(m_db) << std::endl;
				ok = false;
			}
		}
		sqlite3_free(zErrMsg);
	}

	if( ok )
	{
		// Now open the output db for the duration of the simulation
		rc = sqlite3_open_v2(m_dbName.c_str(), &m_db, SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE, nullptr);
		if( rc )
		{
			m_errorStream << "SQLite3 message, can't open new database: " << sqlite3_errmsg(m_db) << std::endl;
			sqlite3_close(m_db);
			ok = false;
		}
	}
}

SQLite::~SQLite()
{
	m_errorStream.close();
}

int SQLite::sqliteExecuteCommand(const std::string & commandBuffer)
{
	char *zErrMsg = 0;
	int rc = -1;

	rc = sqlite3_exec(m_db, commandBuffer.c_str(), NULL, 0, &zErrMsg);
	if( rc != SQLITE_OK )
	{
		m_errorStream << zErrMsg;
	}
	sqlite3_free(zErrMsg);

	return rc;
}

int SQLite::sqlitePrepareStatement(sqlite3_stmt * stmt, const std::string & stmtBuffer)
{
	int rc = -1;

	rc = sqlite3_prepare_v2(m_db, stmtBuffer.c_str(), -1, &stmt, nullptr);
	if( rc != SQLITE_OK )
	{
		m_errorStream << "SQLite3 message, sqlite3_prepare_v2 message: " << sqlite3_errmsg(m_db) << std::endl;
	}

	return rc;
}

void SQLite::initializeReportVariableDataDictionaryTable()
{
	const std::string newTableSQL = 
	"CREATE TABLE ReportVariableDataDictionary("
	"ReportVariableDataDictionaryIndex INTEGER PRIMARY KEY, "
	"VariableType TEXT, "
	"IndexGroup TEXT, "
	"TimestepType TEXT, "
	"KeyValue TEXT, "
	"VariableName TEXT, "
	"ReportingFrequency TEXT, "
	"ScheduleName TEXT, "
	"VariableUnits TEXT);";

	sqliteExecuteCommand(newTableSQL);

	const std::string preparedSQL = 
	"INSERT INTO ReportVariableDataDictionary ("
	"ReportVariableDataDictionaryIndex, "
	"VariableType, "
	"IndexGroup, "
	"TimestepType, "
	"KeyValue, "
	"VariableName, "
	"ReportingFrequency, "
	"ScheduleName, "
	"VariableUnits) "
	"VALUES(?,?,?,?,?,?,?,?,?);";

	sqlitePrepareStatement(m_reportVariableDictionaryInsertStmt,preparedSQL);
}

void SQLite::initializeReportVariableDataTables()
{
	const std::string reportVariableDataTableSQL =
	"CREATE TABLE ReportVariableData ("
	"TimeIndex INTEGER, "
	"ReportVariableDataDictionaryIndex INTEGER, "
	"VariableValue REAL, "
	"ReportVariableExtendedDataIndex INTEGER);";

	sqliteExecuteCommand(reportVariableDataTableSQL);

	const std::string reportVariableDataInsertSQL = 
	"INSERT INTO ReportVariableData ("
	"TimeIndex, "
	"ReportVariableDataDictionaryIndex, "
	"VariableValue, "
	"ReportVariableExtendedDataIndex) "
	"VALUES(?,?,?,?);";

	sqlitePrepareStatement(m_reportVariableDataInsertStmt,reportVariableDataInsertSQL);

	const std::string reportVariableExtendedDataTableSQL = 
	"CREATE TABLE ReportVariableExtendedData ("
	"ReportVariableExtendedDataIndex INTEGER PRIMARY KEY, "
	"MaxValue REAL, "
	"MaxMonth INTEGER, "
	"MaxDay INTEGER, "
	"MaxHour INTEGER, "
	"MaxStartMinute INTEGER, "
	"MaxMinute INTEGER, "
	"MinValue REAL, "
	"MinMonth INTEGER, "
	"MinDay INTEGER, "
	"MinHour INTEGER, "
	"MinStartMinute INTEGER, "
	"MinMinute INTEGER);";

	sqliteExecuteCommand(reportVariableExtendedDataTableSQL);

	const std::string reportVariableExtendedDataInsertSQL = 
	"INSERT INTO ReportVariableExtendedData ("
	"ReportVariableExtendedDataIndex, "
	"MaxValue, "
	"MaxMonth, "
	"MaxDay, "
	"MaxHour, "
	"MaxStartMinute, "
	"MaxMinute, "
	"MinValue, "
	"MinMonth, "
	"MinDay, "
	"MinHour, "
	"MinStartMinute, "
	"MinMinute) "
	"VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?);";

	sqlitePrepareStatement(m_reportVariableExtendedDataInsertStmt,reportVariableExtendedDataInsertSQL);
}

void SQLite::initializeReportMeterDataDictionaryTable()
{
	const std::string reportMeterDataDictionaryTableSQL =
	"CREATE TABLE ReportMeterDataDictionary (ReportMeterDataDictionaryIndex INTEGER PRIMARY KEY, "
	"VariableType TEXT, IndexGroup TEXT, TimestepType TEXT, KeyValue TEXT, "
	"VariableName TEXT, ReportingFrequency TEXT, ScheduleName TEXT, VariableUnits TEXT);";

	sqliteExecuteCommand(reportMeterDataDictionaryTableSQL);

	const std::string meterDictionaryInsertSQL = 
	"INSERT INTO ReportMeterDataDictionary (ReportMeterDataDictionaryIndex, VariableType, IndexGroup, "
	"timestepType, KeyValue, VariableName, ReportingFrequency, ScheduleName, VariableUnits) "
	"VALUES(?,?,?,?,?,?,?,?,?);";

	sqlitePrepareStatement(m_meterDictionaryInsertStmt,meterDictionaryInsertSQL);
}

void SQLite::initializeReportMeterDataTables()
{
	const std::string reportMeterDataTableSQL = 
	"CREATE TABLE ReportMeterData(TimeIndex INTEGER, ReportMeterDataDictionaryIndex INTEGER, VariableValue REAL, "
	"ReportVariableExtendedDataIndex INTEGER);";

	sqliteExecuteCommand(reportMeterDataTableSQL);

	const std::string reportMeterDataInsertSQL = 
	"INSERT INTO ReportMeterData VALUES(?,?,?,?);";

	sqlitePrepareStatement(m_reportMeterDataInsertStmt,reportMeterDataInsertSQL);

	const std::string reportMeterExtendedDataTableSQL =
	"CREATE TABLE ReportMeterExtendedData (ReportMeterExtendedDataIndex INTEGER PRIMARY KEY, "
	"MaxValue REAL, MaxMonth INTEGER, MaxDay INTEGER, MaxHour INTEGER, MaxStartMinute INTEGER, "
	"MaxMinute INTEGER, MinValue REAL, MinMonth INTEGER, MinDay INTEGER, MinHour INTEGER, "
	"MinStartMinute INTEGER, MinMinute INTEGER);";

	sqliteExecuteCommand(reportMeterExtendedDataTableSQL);

	const std::string meterExtendedDataInsertSQL = 
	"INSERT INTO ReportMeterExtendedData VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?);";

	sqlitePrepareStatement(m_meterExtendedDataInsertStmt,meterExtendedDataInsertSQL);
}

void SQLite::initializeTimeIndicesTable()
{
	const std::string timeTableSQL = 
	"CREATE TABLE Time ("
	"TimeIndex INTEGER PRIMARY KEY, "
	"Month INTEGER, "
	"Day INTEGER, "
	"Hour INTEGER, "
	"Minute INTEGER, "
	"Dst INTEGER, "
	"Interval INTEGER, "
	"IntervalType INTEGER, "
	"SimulationDays INTEGER, "
	"DayType TEXT, "
	"EnvironmentPeriodIndex INTEGER, "
	"WarmupFlag INTEGER);";

	sqliteExecuteCommand(timeTableSQL);

	const std::string timeIndexInsertSQL = 
	"INSERT INTO Time ("
	"TimeIndex, "
	"Month, "
	"Day, "
	"Hour, "
	"Minute, "
	"DST, "
	"Interval, "
	"IntervalType, "
	"SimulationDays, "
	"DayType, "
	"EnvironmentPeriodIndex, "
	"WarmupFlag) "
	"VALUES(?,?,?,?,?,?,?,?,?,?,?,?);";

	sqlitePrepareStatement(m_timeIndexInsertStmt,timeIndexInsertSQL);
}

void SQLite::initializeZoneInfoTable()
{
	const std::string zonesTableSQL = 
	"CREATE TABLE Zones ("
	"ZoneIndex INTEGER PRIMARY KEY, "
	"ZoneName TEXT, "
	"RelNorth REAL, "
	"OriginX REAL, "
	"OriginY REAL, "
	"OriginZ REAL, "
	"CentroidX REAL, "
	"CentroidY REAL, "
	"CentroidZ REAL, "
	"OfType INTEGER, "
	"Multiplier REAL, "
	"ListMultiplier REAL, "
	"MinimumX REAL, "
	"MaximumX REAL, "
	"MinimumY REAL, "
	"MaximumY REAL, "
	"MinimumZ REAL, "
	"MaximumZ REAL, "
	"CeilingHeight REAL, "
	"Volume REAL, "
	"InsideConvectionAlgo INTEGER, "
	"OutsideConvectionAlgo INTEGER, "
	"FloorArea REAL, "
	"ExtGrossWallArea REAL, "
	"ExtNetWallArea REAL, "
	"ExtWindowArea REAL, "
	"IsPartOfTotalArea INTEGER);";

	sqliteExecuteCommand(zonesTableSQL);

	const std::string zoneInfoInsertSQL = 
	"INSERT INTO Zones ("
	"ZoneIndex, "
	"ZoneName, "
	"RelNorth, "
	"OriginX, "
	"OriginY, "

	"OriginZ, "
	"CentroidX, "
	"CentroidY, "
	"CentroidZ, "
	"OfType, "

	"Multiplier, "
	"ListMultiplier, "
	"MinimumX, "
	"MaximumX, "
	"MinimumY, "

	"MaximumY, "
	"MinimumZ, "
	"MaximumZ, "
	"CeilingHeight, "
	"Volume, "

	"InsideConvectionAlgo, "
	"OutsideConvectionAlgo, "
	"FloorArea, "
	"ExtGrossWallArea, "
	"ExtNetWallArea, "

	"ExtWindowArea, "
	"IsPartOfTotalArea) "
	"VALUES (?,?,?,?,?, ?,?,?,?,?, ?,?,?,?,?, ?,?,?,?,?, ?,?,?,?,?, ?,?);";

	sqlitePrepareStatement(m_zoneInfoInsertStmt,zoneInfoInsertSQL);
}

void SQLite::initializeNominalPeopleTable()
{
	const std::string nominalPeopleTableSQL = 
	"CREATE TABLE NominalPeople (NominalPeopleIndex INTEGER PRIMARY KEY, "
	"ObjectName TEXT, ZoneIndex INTEGER,"
	"NumberOfPeople INTEGER, NumberOfPeopleScheduleIndex INTEGER, ActivityScheduleIndex INTEGER, FractionRadiant REAL, "
	"FractionConvected REAL, WorkEfficiencyScheduleIndex INTEGER, ClothingEfficiencyScheduleIndex INTEGER, "
	"AirVelocityScheduleIndex INTEGER, Fanger INTEGER, Pierce INTEGER, KSU INTEGER, "
	"MRTCalcType INTEGER, SurfaceIndex INTEGER, "
	"AngleFactorListName TEXT, AngleFactorList INTEGER, UserSpecifeidSensibleFraction REAL, Show55Warning INTEGER"
	");";

	sqliteExecuteCommand(nominalPeopleTableSQL);

	const std::string nominalPeopleInsertSQL =
	"INSERT INTO NominalPeople VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);";

	sqlitePrepareStatement(m_nominalPeopleInsertStmt,nominalPeopleInsertSQL);
}

void SQLite::initializeNominalLightingTable()
{
	const std::string nominalLightingTableSQL = 
	"CREATE TABLE NominalLighting (NominalLightingIndex INTEGER PRIMARY KEY, "
	"ObjectName TEXT, "
	"ZoneIndex INTEGER, ScheduleIndex INTEGER, DesignLevel REAL, FractionReturnAir REAL, FractionRadiant REAL, "
	"FractionShortWave REAL, FractionReplaceable REAL, FractionConvected REAL, EndUseSubcategory TEXT);";

	sqliteExecuteCommand(nominalLightingTableSQL);

	const std::string nominalLightingInsertSQL = 
	"INSERT INTO NominalLighting VALUES(?,?,?,?,?,?,?,?,?,?,?);";

	sqlitePrepareStatement(m_nominalLightingInsertStmt,nominalLightingInsertSQL);
}

void SQLite::initializeNominalElectricEquipmentTable()
{
	const std::string nominalElectricEquipmentTableSQL =
	"CREATE TABLE NominalElectricEquipment ("
	"NominalElectricEquipmentIndex INTEGER PRIMARY KEY, "
	"ObjectName TEXT, "
	"ZoneIndex INTEGER, ScheduleIndex INTEGER, DesignLevel REAL, "
	"FractionLatent REAL, FractionRadiant REAL, FractionLost REAL, "
	"FractionConvected REAL, EndUseSubcategory TEXT);";

	sqliteExecuteCommand(nominalElectricEquipmentTableSQL);

	const std::string nominalElectricEquipmentInsertSQL = 
	"INSERT INTO NominalElectricEquipment VALUES(?,?,?,?,?,?,?,?,?,?);";

	sqlitePrepareStatement(m_nominalElectricEquipmentInsertStmt,nominalElectricEquipmentInsertSQL);
}

void SQLite::initializeNominalGasEquipmentTable()
{
	const std::string nominalGasEquipmentTableSQL =
	"CREATE TABLE NominalGasEquipment(NominalGasEquipmentIndex INTEGER PRIMARY KEY, "
	"ObjectName TEXT, "
	"ZoneIndex INTEGER, ScheduleIndex INTEGER, "
	"DesignLevel REAL, FractionLatent REAL, FractionRadiant REAL, FractionLost REAL, "
	"FractionConvected REAL, EndUseSubcategory TEXT);";

	sqliteExecuteCommand(nominalGasEquipmentTableSQL);

	const std::string nominalGasEquipmentInsertSQL =
	"INSERT INTO NominalGasEquipment VALUES(?,?,?,?,?,?,?,?,?,?);";

	sqlitePrepareStatement(m_nominalGasEquipmentInsertStmt,nominalGasEquipmentInsertSQL);
}

void SQLite::initializeNominalSteamEquipmentTable()
{
	const std::string nominalSteamEquipmentTableSQL = 
	"CREATE TABLE NominalSteamEquipment(NominalSteamEquipmentIndex INTEGER PRIMARY KEY, "
	"ObjectName TEXT, "
	"ZoneIndex INTEGER, ScheduleIndex INTEGER, DesignLevel REAL, "
	"FractionLatent REAL, FractionRadiant REAL, FractionLost REAL, "
	"FractionConvected REAL, EndUseSubcategory TEXT);";

	sqliteExecuteCommand(nominalSteamEquipmentTableSQL);

	const std::string nominalSteamEquipmentInsertSQL = 
	"INSERT INTO NominalSteamEquipment VALUES(?,?,?,?,?,?,?,?,?,?);";

	sqlitePrepareStatement(m_nominalSteamEquipmentInsertStmt,nominalSteamEquipmentInsertSQL);
}

void SQLite::initializeNominalHotWaterEquipmentTable()
{
	const std::string nominalHotWaterEquipmentTableSQL = 
	"CREATE TABLE NominalHotWaterEquipment("
	"NominalHotWaterEquipmentIndex INTEGER PRIMARY KEY, "
	"ObjectName TEXT, "
	"ZoneIndex INTEGER, SchedNo INTEGER, DesignLevel REAL, FractionLatent REAL, FractionRadiant REAL, FractionLost REAL, "
	"FractionConvected REAL, EndUseSubcategory TEXT);";

	sqliteExecuteCommand(nominalHotWaterEquipmentTableSQL);

	const std::string nominalHotWaterEquipmentInsertSQL = 
	"INSERT INTO NominalHotWaterEquipment VALUES(?,?,?,?,?,?,?,?,?,?);";

	sqlitePrepareStatement(m_nominalHotWaterEquipmentInsertStmt,nominalHotWaterEquipmentInsertSQL);
}

void SQLite::initializeNominalOtherEquipmentTable()
{
	const std::string nominalOtherEquipmentTableSQL = 
	"CREATE TABLE NominalOtherEquipment(NominalOtherEquipmentIndex INTEGER PRIMARY KEY, "
	"ObjectName TEXT, "
	"ZoneIndex INTEGER, ScheduleIndex INTEGER, DesignLevel REAL, FractionLatent REAL, "
	"FractionRadiant REAL, FractionLost REAL, "
	"FractionConvected REAL, EndUseSubcategory TEXT);";

	sqliteExecuteCommand(nominalOtherEquipmentTableSQL);

	const std::string nominalOtherEquipmentInsertSQL =
	"INSERT INTO NominalOtherEquipment VALUES(?,?,?,?,?,?,?,?,?,?);";

	sqlitePrepareStatement(m_nominalHotWaterEquipmentInsertStmt,nominalOtherEquipmentInsertSQL);
}

void SQLite::initializeNominalBaseboardHeatTable()
{
	const std::string nominalBaseboardHeatersTableSQL =
	"CREATE TABLE NominalBaseboardHeaters (NominalBaseboardHeaterIndex INTEGER PRIMARY KEY, "
	"ObjectName TEXT, "
	"ZoneIndex INTEGER, ScheduleIndex INTEGER, CapatLowTemperature REAL, LowTemperature REAL, CapatHighTemperature REAL, "
	"HighTemperature REAL, FractionRadiant REAL, FractionConvected REAL, EndUseSubcategory TEXT);";

	sqliteExecuteCommand(nominalBaseboardHeatersTableSQL);

	const std::string nominalBaseboardHeatInsertSQL =
	"INSERT INTO NominalBaseboardHeaters VALUES(?,?,?,?,?,?,?,?,?,?,?);";

	sqlitePrepareStatement(m_nominalBaseboardHeatInsertStmt,nominalBaseboardHeatersTableSQL);
}

void SQLite::initializeSurfacesTable()
{
	const std::string surfacesTableSQL =
	"CREATE TABLE Surfaces (SurfaceIndex INTEGER PRIMARY KEY, "
	"SurfaceName, ConstructionIndex INTEGER, "
	"ClassName TEXT, Area REAL, GrossArea REAL, Perimeter REAL, "
	"Azimuth REAL, Height REAL, Reveal REAL, "
	"Shape INTEGER, Sides INTEGER, Tilt REAL, Width REAL, HeatTransferSurf INTEGER, "
	"BaseSurfaceIndex INTEGER, ZoneIndex INTEGER, ExtBoundCond INTEGER,  "
	"ExtSolar INTEGER, ExtWind INTEGER"
	");";

	sqliteExecuteCommand(surfacesTableSQL);

	const std::string surfaceInsertSQL = 
	"INSERT INTO Surfaces VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);";

	sqlitePrepareStatement(m_surfaceInsertStmt,surfaceInsertSQL);
}

void SQLite::initializeConstructionsTables()
{
	const std::string constructionsTableSQL = 
	"CREATE TABLE Constructions (ConstructionIndex INTEGER PRIMARY KEY, Name TEXT, TotalLayers INTEGER, "
	"TotalSolidLayers INTEGER, TotalGlassLayers INTEGER, InsideAbsorpVis REAL, OutsideAbsorpVis REAL,"
	" InsideAbsorpSolar REAL, OutsideAbsorpSolar REAL, InsideAbsorpThermal REAL, OutsideAbsorpThermal REAL, "
	"OutsideRoughness INTEGER, TypeIsWindow INTEGER, Uvalue REAL"
	");";

	sqliteExecuteCommand(constructionsTableSQL);

	const std::string constructionInsertSQL = 
	"INSERT INTO Constructions VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?);";

	sqlitePrepareStatement(m_constructionInsertStmt,constructionsTableSQL);

	const std::string constructionLayersTableSQL =
	"CREATE TABLE ConstructionLayers (ConstructionIndex INTEGER, "
	"LayerIndex INTEGER, MaterialIndex INTEGER);";

	sqliteExecuteCommand(constructionLayersTableSQL);

	const std::string constructionLayerInsertSQL = 
  "INSERT INTO ConstructionLayers VALUES(?,?,?);";

	sqlitePrepareStatement(m_constructionLayerInsertStmt,constructionLayersTableSQL);
}

void SQLite::initializeMaterialsTable()
{
	const std::string materialsTableSQL =
	"CREATE TABLE Materials (MaterialIndex INTEGER PRIMARY KEY, Name TEXT, MaterialType INTEGER, "
	"Roughness INTEGER, "
	"Conductivity REAL, Density REAL, IsoMoistCap REAL, Porosity REAL, Resistance REAL, "
	"ROnly INTEGER, SpecHeat REAL, ThermGradCoef REAL, Thickness REAL, VaporDiffus"
	");";

	sqliteExecuteCommand(materialsTableSQL);

	const std::string materialInsertSQL =
	"INSERT INTO Materials VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?);";

	sqlitePrepareStatement(m_materialInsertStmt,materialInsertSQL);
}

void SQLite::initializeZoneListTable()
{
	const std::string zoneListsTableSQL =
	"CREATE TABLE ZoneLists (ZoneListIndex INTEGER PRIMARY KEY, Name TEXT, "
	"ZoneIndex INTEGER);";

	sqliteExecuteCommand(zoneListsTableSQL);

	const std::string zoneListInsertSQL = 
	"INSERT INTO ZoneLists VALUES(?,?,?);";

	sqlitePrepareStatement(m_zoneListInsertStmt,zoneListInsertSQL);
}

void SQLite::initializeZoneGroupTable()
{
	const std::string zoneGroupsTableSQL =
	"CREATE TABLE ZoneGroups (ZoneGroupIndex INTEGER PRIMARY KEY, ZoneListName TEXT, ZoneListMultiplier INTEGER);";

	sqliteExecuteCommand(zoneGroupsTableSQL);

	const std::string zoneGroupInsertSQL = 
	"INSERT INTO ZoneGroups VALUES(?,?,?);";

	sqlitePrepareStatement(m_zoneGroupInsertStmt,zoneGroupInsertSQL);
}

void SQLite::initializeNominalInfiltrationTable()
{
	const std::string nominalInfiltrationTableSQL =
	"CREATE TABLE NominalInfiltration (NominalInfiltrationIndex INTEGER PRIMARY KEY, "
	"ObjectName TEXT, "
	"ZoneIndex INTEGER, ScheduleIndex INTEGER, DesignLevel REAL);";

	sqliteExecuteCommand(nominalInfiltrationTableSQL);

	const std::string infiltrationInsertSQL =  
	"INSERT INTO NominalInfiltration (NominalInfiltrationIndex, ObjectName, ZoneIndex, ScheduleIndex, DesignLevel)"
	"VALUES (?,?,?,?,?);";

	sqlitePrepareStatement(m_infiltrationInsertStmt,infiltrationInsertSQL);
}

void SQLite::initializeNominalVentilationTable()
{
	const std::string nominalVentilationTableSQL =
	"CREATE TABLE NominalVentilation (NominalVentilationIndex INTEGER PRIMARY KEY, "
	"ObjectName TEXT, "
	"ZoneIndex INTEGER, ScheduleIndex INTEGER, DesignLevel REAL);";

	sqliteExecuteCommand(nominalVentilationTableSQL);

	const std::string ventilationInsertSQL = 
	"INSERT INTO NominalVentilation VALUES(?,?,?,?,?);";

	sqlitePrepareStatement(m_ventilationInsertStmt,ventilationInsertSQL);
}

void SQLite::initializeZoneSizingTable()
{
	const std::string zoneSizesTableSQL =
	"CREATE TABLE ZoneSizes (ZoneName TEXT, LoadType TEXT, "
	"CalcDesLoad REAL, UserDesLoad REAL, CalcDesFlow REAL, UserDesFlow REAL, DesDayName TEXT, PeakHrMin TEXT, "
	"PeakTemp REAL, PeakHumRat REAL, CalcOutsideAirFlow REAL"
	");";

	sqliteExecuteCommand(zoneSizesTableSQL);

	const std::string zoneSizingInsertSQL =
	"INSERT INTO ZoneSizes VALUES(?,?,?,?,?,?,?,?,?,?,?);";

	sqlitePrepareStatement(m_zoneSizingInsertStmt,zoneSizingInsertSQL);
}

void SQLite::initializeSystemSizingTable()
{
	const std::string systemSizesTableSQL =
	"CREATE TABLE SystemSizes (SystemName TEXT, Description TEXT, Value REAL, Units TEXT);";

	sqliteExecuteCommand(systemSizesTableSQL);

	const std::string systemSizingInsertSQL =
	"INSERT INTO SystemSizes VALUES(?,?,?,?);";

	sqlitePrepareStatement(m_systemSizingInsertStmt,systemSizingInsertSQL);
}

void SQLite::initializeComponentSizingTable()
{
	const std::string componentSizesTableSQL =
	"CREATE TABLE ComponentSizes (CompType TEXT, CompName TEXT, "
	"Description TEXT, Value REAL, Units TEXT);";

	sqliteExecuteCommand(componentSizesTableSQL);

	const std::string componentSizingInsertSQL =
	"INSERT INTO ComponentSizes VALUES (?,?,?,?,?);";

	sqlitePrepareStatement(m_componentSizingInsertStmt,componentSizingInsertSQL);
}

void SQLite::initializeRoomAirModelTable()
{
	const std::string roomAirModelsTableSQL =
	"CREATE TABLE RoomAirModels (ZoneIndex INTEGER PRIMARY KEY, AirModelName TEXT, AirModelType INTEGER, "
	"TempCoupleScheme INTEGER, SimAirModel INTEGER);";

	sqliteExecuteCommand(roomAirModelsTableSQL);

	const std::string roomAirModelInsertSQL =
	"INSERT INTO RoomAirModels VALUES(?,?,?,?,?);";

	sqlitePrepareStatement(m_roomAirModelInsertStmt,roomAirModelInsertSQL);
}

void SQLite::initializeSchedulesTable()
{
	const std::string scheduleTableSQL =
	"CREATE TABLE Schedules (ScheduleIndex INTEGER PRIMARY KEY, ScheduleName TEXT, "
	"ScheduleType TEXT, ScheduleMinimum REAL, ScheduleMaximum REAL);";

	sqliteExecuteCommand(scheduleTableSQL);

	const std::string scheduleInsertSQL =
	"INSERT INTO Schedules VALUES(?,?,?,?,?);";

	sqlitePrepareStatement(m_scheduleInsertStmt,scheduleInsertSQL);
}

void SQLite::initializeDaylightMapTables()
{
	const std::string daylightMapsTableSQL =
	"CREATE TABLE DaylightMaps (MapNumber INTEGER PRIMARY KEY, MapName TEXT, "
	"Environment TEXT, Zone INTEGER, ReferencePt1 TEXT, ReferencePt2 TEXT, Z REAL);";

	sqliteExecuteCommand(daylightMapsTableSQL);

	const std::string daylightMapTitleInsertSQL =
	"INSERT INTO DaylightMaps VALUES(?,?,?,?,?,?,?);";

	sqlitePrepareStatement(m_daylightMapHorlyTitleInsertStmt,daylightMapTitleInsertSQL);

	const std::string daylightMapHourlyReportsTableSQL =
	"CREATE TABLE DaylightMapHourlyReports (HourlyReportIndex INTEGER PRIMARY KEY, "
	"MapNumber INTEGER, Month INTEGER, DayOfMonth INTEGER, Hour INTEGER);";

	sqliteExecuteCommand(daylightMapHourlyReportsTableSQL);

	const std::string daylightMapHorlyTitleInsertSQL =
	"INSERT INTO DaylightMapHourlyReports VALUES(?,?,?,?,?);";

	sqlitePrepareStatement(m_daylightMapHorlyTitleInsertStmt,daylightMapHorlyTitleInsertSQL);

	const std::string daylightMapHourlyDataTableSQL =
	"CREATE TABLE DaylightMapHourlyData (HourlyReportIndex INTEGER, "
	"X REAL, Y REAL, Illuminance REAL);";

	sqliteExecuteCommand(daylightMapHourlyDataTableSQL);

	const std::string daylightMapHorlyDataInsertSQL =
	"INSERT INTO DaylightMapHourlyData VALUES(?,?,?,?);";

	sqlitePrepareStatement(m_daylightMapHorlyDataInsertStmt,daylightMapHorlyDataInsertSQL);
}

void SQLite::initializeViews()
{
	const std::string reportVariableWithTimeViewSQL =
	"CREATE VIEW ReportVariableWithTime "
	"AS "
	"SELECT ReportVariableData.*, Time.*, ReportVariableDataDictionary.*, ReportVariableExtendedData.* "
	"FROM "
	"ReportVariableData LEFT OUTER JOIN ReportVariableExtendedData "
	"INNER JOIN Time "
	"INNER JOIN ReportVariableDataDictionary "
	"ON "
	"(ReportVariableData.ReportVariableExtendedDataIndex "
	"= ReportVariableExtendedData.ReportVariableExtendedDataIndex) "
	"AND "
	"(ReportVariableData.TimeIndex = Time.TimeIndex) "
	"AND "
	"(ReportVariableDataDictionary.ReportVariableDataDictionaryIndex "
	"= ReportVariableData.ReportVariableDataDictionaryIndex);";

	sqliteExecuteCommand(reportVariableWithTimeViewSQL);
}

void SQLite::initializeSimulationsTable()
{
	const std::string simulationsTableSQL =
	"CREATE TABLE Simulations (SimulationIndex INTEGER PRIMARY KEY, "
	"EnergyPlusVersion TEXT, TimeStamp TEXT, NumTimestepsPerHour INTEGER, Completed BOOL, "
	"CompletedSuccessfully BOOL);";

	sqliteExecuteCommand(simulationsTableSQL);

	const std::string simulationsInsertSQL = 
	"INSERT INTO Simulations "
	"VALUES(?,?,?,?,'FALSE','FALSE');";

	sqlitePrepareStatement(m_simulationsInsertStmt,simulationsInsertSQL);

	const std::string simulationUpdateSQL =
	"UPDATE Simulations SET "
	"Completed = ?, CompletedSuccessfully = ? "
	"WHERE SimulationIndex = (SELECT count(*) FROM Simulations)";

	sqlitePrepareStatement(m_simulationUpdateStmt,simulationUpdateSQL);
}

void SQLite::initializeErrorsTable()
{
	const std::string errorsTableSQL =
	"CREATE TABLE Errors (ErrorIndex INTEGER PRIMARY KEY, SimulationIndex INTEGER, "
	"ErrorType INTEGER, ErrorMessage TEXT, Count INTEGER);";

	sqliteExecuteCommand(errorsTableSQL);

	const std::string errorInsertSQL =
	"INSERT INTO Errors VALUES(?,?,?,?,?);";

	sqlitePrepareStatement(m_errorInsertStmt,errorInsertSQL);

	const std::string errorUpdateSQL =
	"UPDATE Errors SET "
	"ErrorMessage = ErrorMessage || ? WHERE ErrorIndex = (SELECT count(*) FROM Errors)";

	sqlitePrepareStatement(m_errorUpdateStmt,errorUpdateSQL);
}

void SQLite::initializeEnvironmentPeriodsTable()
{
	const std::string environmentPeriodsTableSQL =
	"CREATE TABLE EnvironmentPeriods (EnvironmentPeriodIndex INTEGER PRIMARY KEY, "
	"SimulationIndex INTEGER, EnvironmentName TEXT, EnvironmentType INTEGER);";

	sqliteExecuteCommand(environmentPeriodsTableSQL);

	const std::string environmentPeriodInsertSQL =
	"INSERT INTO EnvironmentPeriods VALUES(?,?,?,?);";

	sqlitePrepareStatement(m_environmentPeriodInsertStmt,environmentPeriodInsertSQL);
}

int SQLite::sqliteBindText(sqlite3_stmt * stmt, const int stmtInsertLocationIndex, const std::string & textBuffer)
{
	int rc = -1;

	rc = sqlite3_bind_text(stmt, stmtInsertLocationIndex, textBuffer.c_str(), -1, SQLITE_TRANSIENT);
	if( rc != SQLITE_OK )
	{
		m_errorStream << "SQLite3 message, sqlite3_bind_text message: " << sqlite3_errmsg(m_db) << std::endl;
	}

	return rc;
}

int SQLite::sqliteBindInteger(sqlite3_stmt * stmt, const int stmtInsertLocationIndex, const int intToInsert)
{
	int rc = -1;

	rc = sqlite3_bind_int(stmt, stmtInsertLocationIndex, intToInsert);
	if( rc != SQLITE_OK )
	{
		m_errorStream << "SQLite3 message, sqlite3_bind_int message: " << sqlite3_errmsg(m_db) << std::endl;
	}

	return rc;
}

int SQLite::sqliteBindDouble(sqlite3_stmt * stmt, const int stmtInsertLocationIndex, const double doubleToInsert)
{
	int rc = -1;

	rc = sqlite3_bind_double(stmt, stmtInsertLocationIndex, doubleToInsert);
	if( rc != SQLITE_OK )
	{
	    m_errorStream << "SQLite3 message, sqlite3_bind_double message: " << sqlite3_errmsg(m_db) << std::endl;
	}

	return rc;
}

int SQLite::sqliteBindNULL(sqlite3_stmt * stmt, const int stmtInsertLocationIndex)
{
	int rc = -1;

	rc = sqlite3_bind_null(stmt, stmtInsertLocationIndex);
	if( rc != SQLITE_OK )
	{
		m_errorStream << "SQLite3 message, sqlite3_bind_null message: " << sqlite3_errmsg(m_db) << std::endl;
	}

	return rc;
}

int SQLite::sqliteBindLogical(sqlite3_stmt * stmt, const int stmtInsertLocationIndex, const bool valueToInsert)
{
	return sqliteBindInteger(stmt,stmtInsertLocationIndex,logicalToInteger(valueToInsert));
}

int SQLite::sqliteStepCommand(sqlite3_stmt * stmt)
{
	int rc = -1;

	rc = sqlite3_step(stmt);
	switch(rc)
	{
		case SQLITE_DONE:
		case SQLITE_OK:
		case SQLITE_ROW:
			break;

		default:
			m_errorStream << "SQLite3 message, sqlite3_step message: " << sqlite3_errmsg(m_db) << std::endl;
			break;
	}

	return rc;
}

int SQLite::sqliteResetCommand(sqlite3_stmt * stmt)
{
	return sqlite3_reset(stmt);
}

int SQLite::sqliteClearBindings(sqlite3_stmt * stmt)
{
	return sqlite3_clear_bindings(stmt);
}

int SQLite::sqliteFinalizeCommand(sqlite3_stmt * stmt)
{
	return sqlite3_finalize(stmt);
}

void SQLite::sqliteWriteMessage(const std::string & message)
{
	m_errorStream << "SQLite3 message, " << message << std::endl;
}

std::string SQLite::storageType(const int storageTypeIndex) const
{
	std::string result;

	switch(storageTypeIndex)
	{
		case 1:
			result = "Avg";
			break;
		case 2:
			result = "Sum";
			break;
		default:
			result = "Unknown!!!";
	}

	return result;
}

std::string SQLite::timestepTypeName(const int timestepType) const
{
	std::string result;

	switch(timestepType)
	{
		case 1:
			result = "HVAC System";
			break;
		case 2:
			result = "Zone";
			break;
		default:
			result = "Unknown!!!";
	}

	return result;
}

std::string SQLite::reportingFreqName(const int reportingFreqIndex) const
{
	std::string result;

	switch(reportingFreqIndex)
	{
		case LocalReportEach:
			result = "HVAC System Timestep";
			break;
		case LocalReportTimeStep:
			result = "Zone Timestep";
			break;
		case LocalReportHourly:
			result = "Hourly";
			break;
		case LocalReportDaily:
			result = "Daily";
			break;
		case LocalReportMonthly:
			result = "Monthly";
			break;
		case LocalReportSim:
			result = "Run Period";
			break;
		default:
			result = "Unknown!!!";
			break;
	}

	return result;
}

void SQLite::adjustReportingHourAndMinutes(int & hour, int & minutes)
{
	switch(minutes)
	{
		case 60:
			minutes = 0;
			break;
		default:
			hour = hour - 1;
	}
}

void SQLite::parseUnitsAndDescription(const std::string & combinedString, std::string & units, std::string & description)
{
  std::size_t leftPos = combinedString.find("[");
  std::size_t rightPos = combinedString.find("]");

  units = combinedString.substr(leftPos + 1,rightPos - leftPos - 1);
  description = combinedString.substr(leftPos - 1);
}

int SQLite::logicalToInteger(const bool value)
{
	return value ? 1 : 0;
}

void SQLite::createSQLiteReportVariableDictionaryRecord(
	int const reportVariableReportID,
	int const storeTypeIndex,
	std::string const & indexGroup,
	std::string const & keyedValueString,
	std::string const & variableName,
	int const indexType,
	std::string const & units,
	int const reportingFreq,
	Optional_string_const scheduleName
)
{
	sqliteBindInteger(m_reportVariableDictionaryInsertStmt, 1, reportVariableReportID);
	sqliteBindText(m_reportVariableDictionaryInsertStmt, 2, storageType(storeTypeIndex));
	sqliteBindText(m_reportVariableDictionaryInsertStmt, 3, indexGroup);
	sqliteBindText(m_reportVariableDictionaryInsertStmt, 4, timestepTypeName(indexType));
	sqliteBindText(m_reportVariableDictionaryInsertStmt, 5, keyedValueString);
	sqliteBindText(m_reportVariableDictionaryInsertStmt, 6, variableName);
	sqliteBindText(m_reportVariableDictionaryInsertStmt, 7, reportingFreqName(reportingFreq));

	if(scheduleName.present())
	{
		sqliteBindText(m_reportVariableDictionaryInsertStmt, 8, scheduleName());
	}
	else
	{
		sqliteBindNULL(m_reportVariableDictionaryInsertStmt, 8);
	}
	
	sqliteBindText(m_reportVariableDictionaryInsertStmt, 9, units);
	
	sqliteStepCommand(m_reportVariableDictionaryInsertStmt);
	sqliteResetCommand(m_reportVariableDictionaryInsertStmt);
}

void
SQLite::createSQLiteReportVariableDataRecord(
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
	int result;

	int minMonth;
	int minDay;
	int minHour;
	int minMinute;
	int maxMonth;
	int maxDay;
	int maxHour;
	int maxMinute;

	static int oid = 0;
	static int extendedDataIndex = 0;

	++oid;

	sqliteBindInteger(m_reportMeterDataInsertStmt, 1, timeIndex);
	sqliteBindInteger(m_reportMeterDataInsertStmt, 2, recordIndex);
	sqliteBindDouble(m_reportMeterDataInsertStmt, 3, value);
	sqliteBindInteger(m_reportMeterDataInsertStmt, 4, oid);
	
	if(reportingInterval.present())
	{
		General::DecodeMonDayHrMin(minValueDate, minMonth, minDay, minHour, minMinute);
		General::DecodeMonDayHrMin(maxValueDate, maxMonth, maxDay, maxHour, maxMinute);

		adjustReportingHourAndMinutes(minHour, minMinute);
		adjustReportingHourAndMinutes(maxHour, maxMinute);

		++extendedDataIndex;

		switch(reportingInterval())
		{
			case LocalReportHourly:
			case LocalReportDaily:
			case LocalReportMonthly:
			case LocalReportSim:
				sqliteBindInteger(m_meterExtendedDataInsertStmt, 1, extendedDataIndex);
				sqliteBindDouble(m_meterExtendedDataInsertStmt, 2, maxValue);
				sqliteBindInteger(m_meterExtendedDataInsertStmt, 3, maxMonth);
				sqliteBindInteger(m_meterExtendedDataInsertStmt, 4, maxDay);
				sqliteBindInteger(m_meterExtendedDataInsertStmt, 5, maxHour);
				sqliteBindInteger(m_meterExtendedDataInsertStmt, 6, maxMinute - minutesPerTimeStep + 1);
				sqliteBindInteger(m_meterExtendedDataInsertStmt, 7, maxMinute);
				
				sqliteBindDouble(m_meterExtendedDataInsertStmt, 8, minValue);
				sqliteBindInteger(m_meterExtendedDataInsertStmt, 9, minMonth);
				sqliteBindInteger(m_meterExtendedDataInsertStmt, 10, minDay);
				sqliteBindInteger(m_meterExtendedDataInsertStmt, 11, minHour);
				sqliteBindInteger(m_meterExtendedDataInsertStmt, 12, minMinute - minutesPerTimeStep + 1);
				sqliteBindInteger(m_meterExtendedDataInsertStmt, 13, minMinute);
				
				sqliteStepCommand(m_meterExtendedDataInsertStmt);
				sqliteResetCommand(m_meterExtendedDataInsertStmt);
				break;

			case LocalReportTimeStep:
				--extendedDataIndex; // Reset the data index to account for the error
				sqliteBindNULL(m_reportVariableDataInsertStmt, 4);
				break;

			default:
				--extendedDataIndex; // Reset the data index to account for the error
				std::stringstream ss;
				ss << "Illegal reportingInterval passed to CreateSQLiteMeterRecord: " << reportingInterval;
				sqliteWriteMessage(ss.str());
		}
	}

	sqliteStepCommand(m_reportMeterDataInsertStmt);
	sqliteResetCommand(m_reportMeterDataInsertStmt);
}

int
SQLite::CreateSQLiteTimeIndexRecord(
	int const reportingInterval,
	int const recordIndex,
	int const cumlativeSimulationDays,
	Optional_int_const month, // Why are these optional
	Optional_int_const dayOfMonth, // Why are these optional
	Optional_int_const hour, // Why are these optional
	Optional< Real64 const > endMinute, // Why are these optional
	Optional< Real64 const > startMinute, // Why are these optional
	Optional_int_const dst, // Why are these optional
	Optional_string_const dayType // Why are these optional
)
{
	int iOut = -1;

	// These should be required, the function does not work without them
	if( ! ( month.present() && 
					dayType.present() && 
					hour.present() && 
					endMinute.present() && 
					startMinute.present() &&
					dst.present() &&
					dayType.present() ) )
	{
		sqliteWriteMessage("insufficient data passed to CreateSQLiteTimeIndexRecord");
		return iOut; // Error Condition
	}

	int intEndMinute = 60;
	int intStartMinute = 0;
	int intervalInMinutes = 60;
	int t_hour = hour();

	const std::vector<int> lastDayOfMonth = {31,28,31,30,31,30,31,31,30,31,30,31};

	switch(reportingInterval)
	{
		case LocalReportEach:
		case LocalReportTimeStep:
			++m_sqlDBTimeIndex;

			intEndMinute = static_cast<int>(endMinute() + 0.5);
			intStartMinute = static_cast<int>(startMinute() + 0.5);
			adjustReportingHourAndMinutes(t_hour, intEndMinute);
			intervalInMinutes = intEndMinute - intStartMinute;

			sqliteBindInteger(m_timeIndexInsertStmt, 1, m_sqlDBTimeIndex);
			sqliteBindInteger(m_timeIndexInsertStmt, 2, month());
			sqliteBindInteger(m_timeIndexInsertStmt, 3, dayOfMonth());
			sqliteBindInteger(m_timeIndexInsertStmt, 4, t_hour);
			sqliteBindInteger(m_timeIndexInsertStmt, 5, intEndMinute);
			sqliteBindInteger(m_timeIndexInsertStmt, 6, dst());
			sqliteBindInteger(m_timeIndexInsertStmt, 7, intervalInMinutes);
			sqliteBindInteger(m_timeIndexInsertStmt, 8, reportingInterval);
			sqliteBindInteger(m_timeIndexInsertStmt, 9, cumlativeSimulationDays);
			sqliteBindText(m_timeIndexInsertStmt, 10, dayType());
			sqliteBindInteger(m_timeIndexInsertStmt, 11, DataEnvironment::CurEnvirNum);
			if(DataGlobals::WarmupFlag)
			{
				sqliteBindInteger(m_timeIndexInsertStmt, 12, 1);
			}
			else
			{
				sqliteBindInteger(m_timeIndexInsertStmt, 12, 0);
			}

			sqliteStepCommand(m_timeIndexInsertStmt);
			sqliteResetCommand(m_timeIndexInsertStmt);

			iOut = m_sqlDBTimeIndex;
			break;
		case LocalReportHourly:
			++m_sqlDBTimeIndex;

			sqliteBindInteger(m_timeIndexInsertStmt, 1, m_sqlDBTimeIndex);
			sqliteBindInteger(m_timeIndexInsertStmt, 2, month());
			sqliteBindInteger(m_timeIndexInsertStmt, 3, dayOfMonth());
			sqliteBindInteger(m_timeIndexInsertStmt, 4, t_hour);
			sqliteBindInteger(m_timeIndexInsertStmt, 5, 0);
			sqliteBindInteger(m_timeIndexInsertStmt, 6, dst());
			sqliteBindInteger(m_timeIndexInsertStmt, 7, intervalInMinutes);
			sqliteBindInteger(m_timeIndexInsertStmt, 8, reportingInterval);
			sqliteBindInteger(m_timeIndexInsertStmt, 9, cumlativeSimulationDays);
			sqliteBindText(m_timeIndexInsertStmt, 10, dayType());
			sqliteBindInteger(m_timeIndexInsertStmt, 11, DataEnvironment::CurEnvirNum);

			sqliteStepCommand(m_timeIndexInsertStmt);
			sqliteResetCommand(m_timeIndexInsertStmt);

			iOut = m_sqlDBTimeIndex;
			break;
		case LocalReportDaily:
			++m_sqlDBTimeIndex;

			intervalInMinutes = 60*24;
			sqliteBindInteger(m_timeIndexInsertStmt, 1, m_sqlDBTimeIndex);
			sqliteBindInteger(m_timeIndexInsertStmt, 2, month());
			sqliteBindInteger(m_timeIndexInsertStmt, 3, dayOfMonth());
			sqliteBindInteger(m_timeIndexInsertStmt, 4, 24);
			sqliteBindInteger(m_timeIndexInsertStmt, 5, 0);
			sqliteBindInteger(m_timeIndexInsertStmt, 6, dst());
			sqliteBindInteger(m_timeIndexInsertStmt, 7, intervalInMinutes);
			sqliteBindInteger(m_timeIndexInsertStmt, 8, reportingInterval);
			sqliteBindInteger(m_timeIndexInsertStmt, 9, cumlativeSimulationDays);
			sqliteBindText(m_timeIndexInsertStmt, 10, dayType());
			sqliteBindInteger(m_timeIndexInsertStmt, 11, DataEnvironment::CurEnvirNum);

			sqliteStepCommand(m_timeIndexInsertStmt);
			sqliteResetCommand(m_timeIndexInsertStmt);

			iOut = m_sqlDBTimeIndex;
			break;
		case LocalReportMonthly:
			++m_sqlDBTimeIndex;

			intervalInMinutes = 60*24*lastDayOfMonth[month()];
			sqliteBindInteger(m_timeIndexInsertStmt, 1, m_sqlDBTimeIndex);
			sqliteBindInteger(m_timeIndexInsertStmt, 2, month());
			sqliteBindInteger(m_timeIndexInsertStmt, 3, lastDayOfMonth[month()]);
			sqliteBindInteger(m_timeIndexInsertStmt, 4, 24);
			sqliteBindInteger(m_timeIndexInsertStmt, 5, 0);
			sqliteBindNULL(m_timeIndexInsertStmt, 6);
			sqliteBindInteger(m_timeIndexInsertStmt, 7, intervalInMinutes);
			sqliteBindInteger(m_timeIndexInsertStmt, 8, reportingInterval);
			sqliteBindInteger(m_timeIndexInsertStmt, 9, cumlativeSimulationDays);
			sqliteBindNULL(m_timeIndexInsertStmt, 10);
			sqliteBindInteger(m_timeIndexInsertStmt, 11, DataEnvironment::CurEnvirNum);

			sqliteStepCommand(m_timeIndexInsertStmt);
			sqliteResetCommand(m_timeIndexInsertStmt);

			iOut = m_sqlDBTimeIndex;
			break;
		case LocalReportSim:
			++m_sqlDBTimeIndex;

			intervalInMinutes = 60*24*cumlativeSimulationDays;
			sqliteBindInteger(m_timeIndexInsertStmt, 1, m_sqlDBTimeIndex);
			sqliteBindNULL(m_timeIndexInsertStmt, 2);
			sqliteBindNULL(m_timeIndexInsertStmt, 3);
			sqliteBindNULL(m_timeIndexInsertStmt, 4);
			sqliteBindNULL(m_timeIndexInsertStmt, 5);
			sqliteBindNULL(m_timeIndexInsertStmt, 6);
			sqliteBindInteger(m_timeIndexInsertStmt, 7, intervalInMinutes);
			sqliteBindInteger(m_timeIndexInsertStmt, 8, reportingInterval);
			sqliteBindInteger(m_timeIndexInsertStmt, 9, cumlativeSimulationDays);
			sqliteBindNULL(m_timeIndexInsertStmt, 10);
			sqliteBindInteger(m_timeIndexInsertStmt, 11, DataEnvironment::CurEnvirNum);

			sqliteStepCommand (m_timeIndexInsertStmt);
			sqliteResetCommand (m_timeIndexInsertStmt);

			iOut = m_sqlDBTimeIndex;
			break;
		default:
			std::stringstream ss;
			ss << "Illegal reportingInterval passed to CreateSQLiteTimeIndexRecord: " << reportingInterval;
			sqliteWriteMessage(ss.str());
			iOut = -1;  // this is an error condition
	}

	return iOut;
}

void
SQLite::AddSQLiteZoneSizingRecord(
	std::string const & zoneName, // the name of the zone
	std::string const & loadType, // the description of the input variable
	Real64 const calcDesLoad, // the value from the sizing calculation [W]
	Real64 const userDesLoad, // the value from the sizing calculation modified by user input [W]
	Real64 const calcDesFlow, // calculated design air flow rate [m3/s]
	Real64 const userDesFlow, // user input or modified design air flow rate [m3/s]
	std::string const & desDayName, // the name of the design day that produced the peak
	std::string const & peakHrMin, // time stamp of the peak
	Real64 const peakTemp, // temperature at peak [C]
	Real64 const peakHumRat, // humidity ratio at peak [kg water/kg dry air]
	Real64 const minOAVolFlow // zone design minimum outside air flow rate [m3/s]
)
{
	sqliteBindText(m_zoneSizingInsertStmt, 1, zoneName);
	sqliteBindText(m_zoneSizingInsertStmt, 2, loadType);

	sqliteBindDouble(m_zoneSizingInsertStmt, 3, calcDesLoad);
	sqliteBindDouble(m_zoneSizingInsertStmt, 4, userDesLoad);
	sqliteBindDouble(m_zoneSizingInsertStmt, 5, calcDesFlow);
	sqliteBindDouble(m_zoneSizingInsertStmt, 6, userDesFlow);

	sqliteBindText(m_zoneSizingInsertStmt, 7, desDayName);
	sqliteBindText(m_zoneSizingInsertStmt, 8, peakHrMin);

	sqliteBindDouble(m_zoneSizingInsertStmt, 9, peakTemp);
	sqliteBindDouble(m_zoneSizingInsertStmt, 10, peakHumRat);
	sqliteBindDouble(m_zoneSizingInsertStmt, 11, minOAVolFlow);

	sqliteStepCommand(m_zoneSizingInsertStmt);
	sqliteResetCommand(m_zoneSizingInsertStmt);
}

void
SQLite::AddSQLiteSystemSizingRecord(
	std::string const & sysName, // the name of the system
	std::string const & varDesc, // the description of the input variable
	Real64 const varValue // the value from the sizing calculation
)
{
	std::string description;
	std::string units;

	parseUnitsAndDescription(varDesc,units,description);

	sqliteBindText(m_systemSizingInsertStmt, 1, sysName);
	sqliteBindText(m_systemSizingInsertStmt, 2, description);
	sqliteBindDouble(m_systemSizingInsertStmt, 3, varValue);
	sqliteBindText(m_systemSizingInsertStmt, 4, units);

	sqliteStepCommand(m_systemSizingInsertStmt);
	sqliteResetCommand(m_systemSizingInsertStmt);
}

void
SQLite::AddSQLiteComponentSizingRecord(
	std::string const & compType, // the type of the component
	std::string const & compName, // the name of the component
	std::string const & varDesc, // the description of the input variable
	Real64 const varValue // the value from the sizing calculation
)
{
	std::string description;
	std::string units;

	parseUnitsAndDescription(varDesc,units,description);

	sqliteBindText(m_componentSizingInsertStmt, 1, compType);
	sqliteBindText(m_componentSizingInsertStmt, 2, compName);
	sqliteBindText(m_componentSizingInsertStmt, 3, description);
	sqliteBindDouble(m_componentSizingInsertStmt, 4, varValue);
	sqliteBindText(m_componentSizingInsertStmt, 5, units);

	sqliteStepCommand(m_componentSizingInsertStmt);
	sqliteResetCommand(m_componentSizingInsertStmt);
}

void
SQLite::CreateSQLiteRoomAirModelTable()
{
	for(int zoneNum = 1; zoneNum <= DataGlobals::NumOfZones; ++zoneNum)
	{
		sqliteBindInteger(m_roomAirModelInsertStmt, 1, zoneNum);
		sqliteBindText(m_roomAirModelInsertStmt, 2, DataRoomAirModel::AirModel(zoneNum).AirModelName);
		sqliteBindInteger(m_roomAirModelInsertStmt, 3, DataRoomAirModel::AirModel(zoneNum).AirModelType);
		sqliteBindInteger(m_roomAirModelInsertStmt, 4, DataRoomAirModel::AirModel(zoneNum).TempCoupleScheme);
		sqliteBindLogical(m_roomAirModelInsertStmt, 5, DataRoomAirModel::AirModel(zoneNum).SimAirModel);

		sqliteStepCommand(m_roomAirModelInsertStmt);
		sqliteResetCommand(m_roomAirModelInsertStmt);
	}
}

void
SQLite::CreateSQLiteMeterDictionaryRecord(
	int const meterReportID,
	int const storeTypeIndex,
	std::string const & indexGroup,
	std::string const & keyedValueString,
	std::string const & variableName,
	int const indexType,
	std::string const & units,
	int const reportingFreq,
	Optional_string_const scheduleName
)
{
	sqliteBindInteger(m_meterDictionaryInsertStmt, 1, meterReportID);
	sqliteBindText(m_meterDictionaryInsertStmt, 2, storageType(storeTypeIndex));
	sqliteBindText(m_meterDictionaryInsertStmt, 3, indexGroup);
	sqliteBindText(m_meterDictionaryInsertStmt, 4, timestepTypeName(indexType));
	sqliteBindText(m_meterDictionaryInsertStmt, 5, keyedValueString);
	sqliteBindText(m_meterDictionaryInsertStmt, 6, variableName);
	sqliteBindText(m_meterDictionaryInsertStmt, 7, reportingFreqName(reportingFreq));

	if(scheduleName.present())
	{
		sqliteBindText(m_meterDictionaryInsertStmt, 8, scheduleName());
	}
	else
	{
		sqliteBindNULL(m_meterDictionaryInsertStmt, 8);
	}

	sqliteBindText(m_meterDictionaryInsertStmt, 9, units);

	sqliteStepCommand(m_meterDictionaryInsertStmt);
	sqliteResetCommand(m_meterDictionaryInsertStmt);
}

void
SQLite::CreateSQLiteMeterRecord(
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
	static int extendedDataIndex = 0;
	static int oid = 0;

	++oid;

	sqliteBindInteger(m_reportMeterDataInsertStmt, 1, timeIndex);
	sqliteBindInteger(m_reportMeterDataInsertStmt, 2, recordIndex);
	sqliteBindDouble(m_reportMeterDataInsertStmt, 3, value);
	sqliteBindInteger(m_reportMeterDataInsertStmt, 4, oid);

	if(reportingInterval.present() && minValue.present() && minValueDate.present()
		&& maxValue.present() && maxValueDate.present() && minutesPerTimeStep.present() )
	{
		int minMonth;
		int minDay;
		int minHour;
		int minMinute;
		int maxMonth;
		int maxDay;
		int maxHour;
		int maxMinute;

		General::DecodeMonDayHrMin(minValueDate, minMonth, minDay, minHour, minMinute);
		General::DecodeMonDayHrMin(maxValueDate, maxMonth, maxDay, maxHour, maxMinute);

		++extendedDataIndex;

		switch(reportingInterval())
		{
			case LocalReportHourly:
			case LocalReportDaily:
			case LocalReportMonthly:
			case LocalReportSim:
				sqliteBindInteger(m_meterExtendedDataInsertStmt, 1, extendedDataIndex);
				sqliteBindDouble(m_meterExtendedDataInsertStmt, 2, maxValue);
				sqliteBindInteger(m_meterExtendedDataInsertStmt, 3, maxMonth);
				sqliteBindInteger(m_meterExtendedDataInsertStmt, 4, maxDay);
				sqliteBindInteger(m_meterExtendedDataInsertStmt, 5, maxHour);
				sqliteBindInteger(m_meterExtendedDataInsertStmt, 6, maxMinute - minutesPerTimeStep + 1);
				sqliteBindInteger(m_meterExtendedDataInsertStmt, 7, maxMinute);
				
				sqliteBindDouble(m_meterExtendedDataInsertStmt, 8, minValue);
				sqliteBindInteger(m_meterExtendedDataInsertStmt, 9, minMonth);
				sqliteBindInteger(m_meterExtendedDataInsertStmt, 10, minDay);
				sqliteBindInteger(m_meterExtendedDataInsertStmt, 11, minHour);
				sqliteBindInteger(m_meterExtendedDataInsertStmt, 12, minMinute - minutesPerTimeStep + 1);
				sqliteBindInteger(m_meterExtendedDataInsertStmt, 13, minMinute);
				
				sqliteStepCommand(m_meterExtendedDataInsertStmt);
				sqliteResetCommand(m_meterExtendedDataInsertStmt);
				break;
			default:
				--extendedDataIndex; // Reset the data index to account for the error
				std::stringstream ss;
				ss << "Illegal reportingInterval passed to CreateSQLiteMeterRecord: " << reportingInterval;
				sqliteWriteMessage(ss.str());
		}
	}

	sqliteStepCommand(m_reportMeterDataInsertStmt);
	sqliteResetCommand(m_reportMeterDataInsertStmt);
}

void
SQLite::CreateSQLiteDaylightMapTitle(
	int const mapNum,
	std::string const & mapName,
	std::string const & environmentName,
	int const zone,
	std::string const & refPt1,
	std::string const & refPt2,
	Real64 const zCoord
)
{
	sqliteBindInteger(m_daylightMapTitleInsertStmt, 1, mapNum);
	sqliteBindText(m_daylightMapTitleInsertStmt, 2, mapName);
	sqliteBindText(m_daylightMapTitleInsertStmt, 3, environmentName);
	sqliteBindInteger(m_daylightMapTitleInsertStmt, 4, zone);
	sqliteBindText(m_daylightMapTitleInsertStmt, 5, refPt1);
	sqliteBindText(m_daylightMapTitleInsertStmt, 6, refPt2);
	sqliteBindDouble(m_daylightMapTitleInsertStmt, 7, zCoord);

	sqliteStepCommand(m_daylightMapTitleInsertStmt);
	sqliteResetCommand(m_daylightMapTitleInsertStmt);
}

void
SQLite::CreateSQLiteDaylightMap(
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
	static int hourlyReportIndex = 1;

	sqliteBindInteger(m_daylightMapHorlyTitleInsertStmt, 1, hourlyReportIndex);
	sqliteBindInteger(m_daylightMapHorlyTitleInsertStmt, 2, mapNum);
	sqliteBindInteger(m_daylightMapHorlyTitleInsertStmt, 3, month);
	sqliteBindInteger(m_daylightMapHorlyTitleInsertStmt, 4, dayOfMonth);
	sqliteBindInteger(m_daylightMapHorlyTitleInsertStmt, 5, hourOfDay);

	sqliteStepCommand(m_daylightMapHorlyTitleInsertStmt);
	sqliteResetCommand(m_daylightMapHorlyTitleInsertStmt);

	for(int yIndex = 1; yIndex <= nY; ++yIndex)
	{
		for(int xIndex = 1; xIndex <= nX; ++xIndex)
		{
			sqliteBindInteger(m_daylightMapHorlyDataInsertStmt, 1, hourlyReportIndex);
			sqliteBindDouble(m_daylightMapHorlyDataInsertStmt, 2, x(xIndex));
			sqliteBindDouble(m_daylightMapHorlyDataInsertStmt, 3, y(yIndex));
			sqliteBindDouble(m_daylightMapHorlyDataInsertStmt, 4, illuminance(xIndex, yIndex));

			sqliteStepCommand(m_daylightMapHorlyDataInsertStmt);
			sqliteResetCommand(m_daylightMapHorlyDataInsertStmt);
		}
	}

	++hourlyReportIndex;
}

void
SQLite::CreateSQLiteTabularDataRecords(
	FArray2D_string const & body, // row,column
	FArray1D_string const & rowLabels,
	FArray1D_string const & columnLabels,
	std::string const & reportName,
	std::string const & reportForString,
	std::string const & tableName
)
{
	if(m_writeTabularDataToSQLite)
	{
		size_t sizeColumnLabels = columnLabels.size();
		size_t sizeRowLabels = rowLabels.size();

		for(size_t iRow = 1, k = body.index(1,1); iRow <= sizeRowLabels; ++iRow)
		{
			std::string rowLabel = rowLabels[iRow];
			std::string rowUnits;
			std::string rowDescription;
			parseUnitsAndDescription(rowLabel,rowUnits,rowDescription);

			for(size_t iCol = 1; iCol <= sizeColumnLabels; ++iCol)
			{
				std::string colLabel = columnLabels[iCol];
				std::string colUnits;
				std::string colDescription;
				parseUnitsAndDescription(colLabel,colUnits,colDescription);

				std::string units;
				if( colUnits.empty() )
				{
					units = rowUnits;
				}
				else
				{
					units = colUnits;
				}

				int reportNameIndex = createSQLiteStringTableRecord(reportName,ReportNameId);
				int reportForStringIndex = createSQLiteStringTableRecord(reportForString,ReportForStringId);
				int tableNameIndex = createSQLiteStringTableRecord(tableName,TableNameId);
				int rowLabelIndex = createSQLiteStringTableRecord(rowLabel,RowNameId);
				int columnLabelIndex = createSQLiteStringTableRecord(colLabel,ColumnNameId);
				int unitsIndex = createSQLiteStringTableRecord(units,UnitsId);

				sqliteBindInteger(m_tabularDataInsertStmt,1,reportNameIndex);
				sqliteBindInteger(m_tabularDataInsertStmt,2,reportForStringIndex);
				sqliteBindInteger(m_tabularDataInsertStmt,3,tableNameIndex);
				sqliteBindInteger(m_tabularDataInsertStmt,4,1);
				sqliteBindInteger(m_tabularDataInsertStmt,5,rowLabelIndex);
				sqliteBindInteger(m_tabularDataInsertStmt,6,columnLabelIndex);
				sqliteBindInteger(m_tabularDataInsertStmt,7,iRow);
				sqliteBindInteger(m_tabularDataInsertStmt,8,iCol);
				sqliteBindText(m_tabularDataInsertStmt,9,body[k]);
				sqliteBindInteger(m_tabularDataInsertStmt,10,unitsIndex);

				++k;

				sqliteStepCommand(m_tabularDataInsertStmt);
				sqliteResetCommand(m_tabularDataInsertStmt);
				sqliteClearBindings(m_tabularDataInsertStmt);
			}
		}
	}
}

int SQLite::createSQLiteStringTableRecord(std::string const & stringValue,std::string const & stringType)
{
	int iOut = 0;
	int errcode;

	errcode = sqliteBindText(m_stringsLookUpStmt, 1, stringType);
	errcode = sqliteBindText(m_stringsLookUpStmt, 2, stringValue);
	errcode = sqliteStepCommand(m_stringsLookUpStmt);

	if(errcode == SQLITE_ROW)
	{
		// If the stringKey is already in the database we just return its ID
		iOut = sqlite3_column_int(m_stringsLookUpStmt,0);
	}
	else
	{
		// If the stringKey is not already in the database we create a new record
		// using the next available ID

		errcode = sqliteBindText(m_stringsInsertStmt, 1, stringType);
		errcode = sqliteBindText(m_stringsInsertStmt, 2, stringValue);
		errcode = sqliteStepCommand(m_stringsInsertStmt);
		errcode = sqliteResetCommand(m_stringsInsertStmt);
		errcode = sqliteClearBindings(m_stringsInsertStmt);
		
		errcode = sqliteResetCommand(m_stringsLookUpStmt);
		errcode = sqliteStepCommand(m_stringsLookUpStmt);
		iOut = sqlite3_column_int(m_stringsLookUpStmt,0);
	}

	errcode = sqliteResetCommand(m_stringsLookUpStmt);
	errcode = sqliteClearBindings(m_stringsLookUpStmt);

	return iOut;
}

void
SQLite::CreateSQLiteSimulationsRecord( int const id )
{
	sqliteBindInteger(m_simulationsInsertStmt, 1, id);
	sqliteBindText(m_simulationsInsertStmt, 2, DataStringGlobals::VerString);
	sqliteBindText(m_simulationsInsertStmt, 3, DataStringGlobals::CurrentDateTime);
	sqliteBindInteger(m_simulationsInsertStmt, 4, DataGlobals::NumOfTimeStepInHour);

	sqliteStepCommand(m_simulationsInsertStmt);
	sqliteResetCommand(m_simulationsInsertStmt);
}

void
SQLite::CreateSQLiteErrorRecord(
	int const simulationIndex,
	int const errorType,
	std::string const & errorMessage,
	int const cnt
)
{
	static int errorIndex = 0;

	++errorIndex;

	sqliteBindInteger(m_errorInsertStmt, 1, errorIndex);
	sqliteBindInteger(m_errorInsertStmt, 2, simulationIndex);
	sqliteBindInteger(m_errorInsertStmt, 3, errorType);
	sqliteBindText(m_errorInsertStmt, 4, errorMessage);
	sqliteBindInteger(m_errorInsertStmt, 5, cnt);

	sqliteStepCommand(m_errorInsertStmt);
	sqliteResetCommand(m_errorInsertStmt);
}

void
SQLite::UpdateSQLiteErrorRecord( std::string const & errorMessage )
{
	sqliteBindText(m_errorUpdateStmt, 1, "  " + errorMessage);

	sqliteStepCommand(m_errorUpdateStmt);
	sqliteResetCommand(m_errorUpdateStmt);
}

void
SQLite::UpdateSQLiteSimulationRecord(
	bool const completed,
	bool const completedSuccessfully
)
{
	sqliteBindLogical(m_simulationUpdateStmt, 1, completed);
	sqliteBindLogical(m_simulationUpdateStmt, 2, completedSuccessfully);

	sqliteStepCommand(m_simulationUpdateStmt);
	sqliteResetCommand(m_simulationUpdateStmt);
}

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
