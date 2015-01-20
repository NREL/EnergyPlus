// EnergyPlus::SQLite Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/SQLiteProcedures.hh>

using namespace EnergyPlus;
using namespace ObjexxFCL;

namespace EnergyPlus {

	class SQLiteFixture : public testing::Test
	{

	protected:
		static void SetUpTestCase() {
			ASSERT_NO_THROW(sqlite = std::unique_ptr<SQLite>(new SQLite( true, true )));
			ASSERT_TRUE(sqlite->writeOutputToSQLite());
			ASSERT_TRUE(sqlite->writeTabularDataToSQLite());
			sqlite->sqliteExecuteCommand("PRAGMA foreign_keys = ON;");
		}

		static void TearDownTestCase() {
			// don't know if this is needed...
			sqlite.reset();
			sqlite = nullptr;
		}

		virtual void SetUp() { }

		virtual void TearDown() { }

		std::string storageType( const int storageTypeIndex ) {
			return sqlite->storageType( storageTypeIndex );
		}

		std::string timestepTypeName( const int timestepType ) {
			return sqlite->timestepTypeName( timestepType );
		}

		std::string reportingFreqName( const int reportingFreqIndex ) {
			return sqlite->reportingFreqName( reportingFreqIndex );
		}

	public:
		// static std::unique_ptr<SQLite> sqlite;

		std::string columnText(const unsigned char* column) {
			return std::string(reinterpret_cast<const char*>(column));
		}

		int executeSQL(const std::string & query) {
			return sqlite3_exec(sqlite->m_db, query.c_str(), NULL, 0, nullptr);
		}

		std::pair<bool, int> executeAndReturnFirstInt( const std::string & statement, const int column ) {
			bool success = false;
			int result = 0;

			sqlite3_stmt* sqlStmtPtr;
			int code = sqlite3_prepare_v2(sqlite->m_db, statement.c_str(), -1, &sqlStmtPtr, nullptr);
			code = sqlite3_step(sqlStmtPtr);
			if (code == SQLITE_ROW) {
				result = sqlite3_column_int(sqlStmtPtr, column);
				success = true;
			}
			sqlite3_finalize(sqlStmtPtr);

			return std::make_pair( success, result );
		}

		bool indexExists( const std::string& indexName ) {
			sqlite3_stmt* sqlStmtPtr;
			std::string sql("pragma index_info(" + indexName + ");");
			bool success = false;
			int rc = sqlite3_prepare_v2(sqlite->m_db, sql.c_str(), -1, &sqlStmtPtr, nullptr);
			if ( SQLITE_OK != rc ) {
				sqlite3_finalize(sqlStmtPtr);
				return success;
			}
			if ( SQLITE_ROW == sqlite3_step( sqlStmtPtr ) ) {
				success = true;
			}
			sqlite3_finalize(sqlStmtPtr);
			return success;
		}

		int columnCount( const std::string& tableName ) {
			sqlite3_stmt* sqlStmtPtr;
			std::string sql("pragma table_info(" + tableName + ");");
			int rc = sqlite3_prepare_v2(sqlite->m_db, sql.c_str(), -1, &sqlStmtPtr, nullptr);
			if ( SQLITE_OK != rc ) {
				sqlite3_finalize(sqlStmtPtr);
				return -1;
			}
			int rowCount = 0;
			while ( SQLITE_ROW == sqlite3_step( sqlStmtPtr ) ) {
				rowCount++;
			}
			sqlite3_finalize(sqlStmtPtr);
			return rowCount;
		}

		std::vector<std::vector<std::string>> queryResult( const std::string& statement, const std::string& tableName ) {
			std::vector < std::vector < std::string > > queryVector;

			int rowCount = columnCount( tableName );
			if( rowCount < 1 ) return queryVector;

			sqlite3_stmt* sqlStmtPtr;

			int code = sqlite3_prepare_v2(sqlite->m_db, statement.c_str(), -1, &sqlStmtPtr, nullptr);
			while ( SQLITE_ROW == sqlite3_step( sqlStmtPtr ) ) {
				std::vector < std::string > valueVector;
				for( int i = 0; i < rowCount; ++i ) {
					auto sqlite_value = sqlite3_column_text(sqlStmtPtr, i);
					if (nullptr == sqlite_value) {
						valueVector.push_back("");
					} else {
						valueVector.push_back(columnText(sqlite_value));
					}
				}
				queryVector.push_back(valueVector);
			}
			sqlite3_finalize(sqlStmtPtr);
			return queryVector;
		}
	};


	TEST_F( SQLiteFixture, createSQLiteSimulationsRecord ) {
		sqlite->sqliteBegin();
		sqlite->createSQLiteSimulationsRecord( 1, "EnergyPlus Version", "Current Time" );
		auto result = queryResult("SELECT * FROM Simulations;", "Simulations");
		sqlite->sqliteCommit();

		ASSERT_EQ(1, result.size());
		std::vector<std::string> testResult0 {"1", "EnergyPlus Version", "Current Time", "", "FALSE", "FALSE"};
		EXPECT_EQ(testResult0, result[0]);

		auto countResult = executeAndReturnFirstInt( "SELECT count(*) FROM Simulations;", 0 );
		EXPECT_TRUE(countResult.first);
		EXPECT_EQ(1, countResult.second);

		countResult = executeAndReturnFirstInt( "SELECT count(*) FROM Simulations WHERE SimulationIndex = 1;", 0 );
		EXPECT_TRUE(countResult.first);
		EXPECT_EQ(1, countResult.second);

		countResult = executeAndReturnFirstInt( "SELECT count(*) FROM Simulations WHERE SimulationIndex = 2;", 0 );
		EXPECT_TRUE(countResult.first);
		EXPECT_EQ(0, countResult.second);
	}

	TEST_F( SQLiteFixture, createSQLiteEnvironmentPeriodRecord ) {
		sqlite->sqliteBegin();
		// There needs to be a simulation record otherwise the foreign key constraint will fail
		sqlite->createSQLiteSimulationsRecord( 1, "EnergyPlus Version", "Current Time" );
		sqlite->createSQLiteEnvironmentPeriodRecord( 1, "CHICAGO ANN HTG 99.6% CONDNS DB", 1 );
		sqlite->createSQLiteEnvironmentPeriodRecord( 2, "CHICAGO ANN CLG .4% CONDNS WB=>MDB", 1, 1 );
		sqlite->createSQLiteEnvironmentPeriodRecord( 3, "CHICAGO ANN HTG 99.6% CONDNS DB", 2 );
		sqlite->createSQLiteEnvironmentPeriodRecord( 4, "CHICAGO ANN CLG .4% CONDNS WB=>MDB", 3, 1 );
		auto result = queryResult("SELECT * FROM EnvironmentPeriods;", "EnvironmentPeriods");
		sqlite->sqliteCommit();

		ASSERT_EQ(4, result.size());
		std::vector<std::string> testResult0 {"1", "1", "CHICAGO ANN HTG 99.6% CONDNS DB", "1"};
		std::vector<std::string> testResult1 {"2", "1", "CHICAGO ANN CLG .4% CONDNS WB=>MDB", "1"};
		std::vector<std::string> testResult2 {"3", "1", "CHICAGO ANN HTG 99.6% CONDNS DB", "2"};
		std::vector<std::string> testResult3 {"4", "1", "CHICAGO ANN CLG .4% CONDNS WB=>MDB", "3"};
		EXPECT_EQ(testResult0, result[0]);
		EXPECT_EQ(testResult1, result[1]);
		EXPECT_EQ(testResult2, result[2]);
		EXPECT_EQ(testResult3, result[3]);

		sqlite->sqliteBegin();
		// This should fail to insert due to foreign key constraint
		sqlite->createSQLiteEnvironmentPeriodRecord( 5, "CHICAGO ANN HTG 99.6% CONDNS DB", 1, 2 );
		// This should fail to insert due to duplicate primary key
		sqlite->createSQLiteEnvironmentPeriodRecord( 4, "CHICAGO ANN CLG .4% CONDNS WB=>MDB", 1, 1 );
		result = queryResult("SELECT * FROM EnvironmentPeriods;", "EnvironmentPeriods");
		sqlite->sqliteCommit();

		EXPECT_EQ(4, result.size());
	}

	TEST_F( SQLiteFixture, createSQLiteReportDictionaryRecord )
	{
		sqlite->sqliteBegin();
		sqlite->createSQLiteReportDictionaryRecord( 1, 1, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false, _ );
		sqlite->createSQLiteReportDictionaryRecord( 2, 2, "Facility:Electricity", "", "Facility:Electricity", 1, "J", 1, true, _ );
		sqlite->createSQLiteReportDictionaryRecord( 3, 2, "Facility:Electricity", "", "Facility:Electricity", 1, "J", 3, true, _ );
		sqlite->createSQLiteReportDictionaryRecord( 4, 1, "HVAC", "", "AHU-1", 2, "", 1, false, _ );
		sqlite->createSQLiteReportDictionaryRecord( 5, 1, "HVAC", "", "AHU-1", 2, "", 1, false, "test schedule" );
		auto result = queryResult("SELECT * FROM ReportDataDictionary;", "ReportDataDictionary");
		sqlite->sqliteCommit();

		ASSERT_EQ(5, result.size());
		std::vector<std::string> testResult0 {"1", "0", "Avg", "Zone", "HVAC System", "Environment", "Site Outdoor Air Drybulb Temperature", "Hourly", "", "C"};
		std::vector<std::string> testResult1 {"2", "1", "Sum", "Facility:Electricity", "HVAC System", "", "Facility:Electricity", "Hourly", "", "J"};
		std::vector<std::string> testResult2 {"3", "1", "Sum", "Facility:Electricity", "HVAC System", "", "Facility:Electricity", "Monthly", "", "J"};
		std::vector<std::string> testResult3 {"4", "0", "Avg", "HVAC", "Zone", "", "AHU-1", "Hourly", "", ""};
		std::vector<std::string> testResult4 {"5", "0", "Avg", "HVAC", "Zone", "", "AHU-1", "Hourly", "test schedule", ""};
		EXPECT_EQ(testResult0, result[0]);
		EXPECT_EQ(testResult1, result[1]);
		EXPECT_EQ(testResult2, result[2]);
		EXPECT_EQ(testResult3, result[3]);
		EXPECT_EQ(testResult4, result[4]);

		sqlite->sqliteBegin();
		sqlite->createSQLiteReportDictionaryRecord( 6, 3, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false, _ );
		sqlite->createSQLiteReportDictionaryRecord( 7, 2, "Facility:Electricity", "", "Facility:Electricity", 3, "J", 1, true, _ );
		sqlite->createSQLiteReportDictionaryRecord( 8, 2, "Facility:Electricity", "", "Facility:Electricity", 1, "J", 7, true, _ );
		sqlite->createSQLiteReportDictionaryRecord( 9, 1, "HVAC", "", "AHU-1", 2, "", -2, false, _ );
		result = queryResult("SELECT * FROM ReportDataDictionary;", "ReportDataDictionary");
		sqlite->sqliteCommit();

		ASSERT_EQ(9, result.size());
		std::vector<std::string> testResult5 {"6", "0", "Unknown!!!", "Zone", "HVAC System", "Environment", "Site Outdoor Air Drybulb Temperature", "Hourly", "", "C"};
		std::vector<std::string> testResult6 {"7", "1", "Sum", "Facility:Electricity", "Unknown!!!", "", "Facility:Electricity", "Hourly", "", "J"};
		std::vector<std::string> testResult7 {"8", "1", "Sum", "Facility:Electricity", "HVAC System", "", "Facility:Electricity", "Unknown!!!", "", "J"};
		std::vector<std::string> testResult8 {"9", "0", "Avg", "HVAC", "Zone", "", "AHU-1", "Unknown!!!", "", ""};
		EXPECT_EQ(testResult5, result[5]);
		EXPECT_EQ(testResult6, result[6]);
		EXPECT_EQ(testResult7, result[7]);
		EXPECT_EQ(testResult8, result[8]);

		sqlite->sqliteBegin();
		// This should fail to insert due to duplicate primary key
		sqlite->createSQLiteReportDictionaryRecord( 9, 3, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false, _ );
		result = queryResult("SELECT * FROM ReportDataDictionary;", "ReportDataDictionary");
		sqlite->sqliteCommit();
		EXPECT_EQ(9, result.size());
	}

	TEST_F( SQLiteFixture, initializeIndexes ) {
		sqlite->sqliteBegin();
		sqlite->initializeIndexes();
		sqlite->sqliteCommit();

		EXPECT_TRUE(indexExists("rddMTR"));
		EXPECT_TRUE(indexExists("redRD"));
		EXPECT_FALSE(indexExists("dmhdHRI"));
		EXPECT_FALSE(indexExists("dmhrMNI"));
		EXPECT_FALSE(indexExists("tdI"));
	}

	TEST_F( SQLiteFixture, privateMethods ) {
		// test storageType
		EXPECT_EQ( "Avg", storageType( 1 ) );
		EXPECT_EQ( "Sum", storageType( 2 ) );
		EXPECT_EQ( "Unknown!!!", storageType( 3 ) );
		EXPECT_EQ( "Unknown!!!", storageType( -1 ) );

		// test timestepTypeName
		EXPECT_EQ( "HVAC System", timestepTypeName( 1 ) );
		EXPECT_EQ( "Zone", timestepTypeName( 2 ) );
		EXPECT_EQ( "Unknown!!!", timestepTypeName( 3 ) );
		EXPECT_EQ( "Unknown!!!", timestepTypeName( -1 ) );

		// test reportingFreqName
		EXPECT_EQ( "HVAC System Timestep", reportingFreqName( -1 ) );
		EXPECT_EQ( "Zone Timestep", reportingFreqName( 0 ) );
		EXPECT_EQ( "Hourly", reportingFreqName( 1 ) );
		EXPECT_EQ( "Daily", reportingFreqName( 2 ) );
		EXPECT_EQ( "Monthly", reportingFreqName( 3 ) );
		EXPECT_EQ( "Run Period", reportingFreqName( 4 ) );
		EXPECT_EQ( "Unknown!!!", reportingFreqName( 5 ) );
		EXPECT_EQ( "Unknown!!!", reportingFreqName( -2 ) );
	}

	// // Begin a transaction
	// void sqliteBegin();

	// // Commit a transaction
	// void sqliteCommit();

	// void createSQLiteReportDictionaryRecord(
	// 	int const reportVariableReportID,
	// 	int const storeTypeIndex,
	// 	std::string const & indexGroup,
	// 	std::string const & keyedValueString,
	// 	std::string const & variableName,
	// 	int const indexType,
	// 	std::string const & units,
	// 	int const reportingFreq,
	// 	bool isMeter,
	// 	Optional_string_const ScheduleName = _
	// );

	// void createSQLiteReportDataRecord(
	// 	int const recordIndex,
	// 	Real64 const value,
	// 	Optional_int_const reportingInterval = _,
	// 	Optional< Real64 const > minValue = _,
	// 	Optional_int_const minValueDate = _,
	// 	Optional< Real64 const > maxValue = _,
	// 	Optional_int_const maxValueDate = _,
	// 	Optional_int_const minutesPerTimeStep = _
	// );

	// void createSQLiteTimeIndexRecord(
	// 	int const reportingInterval,
	// 	int const recordIndex,
	// 	int const CumlativeSimulationDays,
	// 	Optional_int_const Month = _,
	// 	Optional_int_const DayOfMonth = _,
	// 	Optional_int_const Hour = _,
	// 	Optional< Real64 const > EndMinute = _,
	// 	Optional< Real64 const > StartMinute = _,
	// 	Optional_int_const DST = _,
	// 	Optional_string_const DayType = _
	// );

	// void addSQLiteZoneSizingRecord(
	// 	std::string const & ZoneName, // the name of the zone
	// 	std::string const & LoadType, // the description of the input variable
	// 	Real64 const CalcDesLoad, // the value from the sizing calculation [W]
	// 	Real64 const UserDesLoad, // the value from the sizing calculation modified by user input [W]
	// 	Real64 const CalcDesFlow, // calculated design air flow rate [m3/s]
	// 	Real64 const UserDesFlow, // user input or modified design air flow rate [m3/s]
	// 	std::string const & DesDayName, // the name of the design day that produced the peak
	// 	std::string const & PeakHrMin, // time stamp of the peak
	// 	Real64 const PeakTemp, // temperature at peak [C]
	// 	Real64 const PeakHumRat, // humidity ratio at peak [kg water/kg dry air]
	// 	Real64 const MinOAVolFlow // zone design minimum outside air flow rate [m3/s]
	// );

	// void addSQLiteSystemSizingRecord(
	// 	std::string const & SysName, // the name of the system
	// 	std::string const & VarDesc, // the description of the input variable
	// 	Real64 const VarValue // the value from the sizing calculation
	// );

	// void addSQLiteComponentSizingRecord(
	// 	std::string const & CompType, // the type of the component
	// 	std::string const & CompName, // the name of the component
	// 	std::string const & VarDesc, // the description of the input variable
	// 	Real64 const VarValue // the value from the sizing calculation
	// );

	// void createSQLiteRoomAirModelTable();

	// void createSQLiteDaylightMapTitle(
	// 	int const mapNum,
	// 	std::string const & mapName,
	// 	std::string const & environmentName,
	// 	int const zone,
	// 	std::string const & refPt1,
	// 	std::string const & refPt2,
	// 	Real64 const zCoord
	// );

	// void createSQLiteDaylightMap(
	// 	int const mapNum,
	// 	int const month,
	// 	int const dayOfMonth,
	// 	int const hourOfDay,
	// 	int const nX,
	// 	FArray1S< Real64 > const & x,
	// 	int const nY,
	// 	FArray1S< Real64 > const & y,
	// 	FArray2S< Real64 > const & illuminance
	// );

	// void createSQLiteTabularDataRecords(
	// 	FArray2D_string const & body, // row,column
	// 	FArray1D_string const & rowLabels,
	// 	FArray1D_string const & columnLabels,
	// 	std::string const & ReportName,
	// 	std::string const & ReportForString,
	// 	std::string const & TableName
	// );

	// void createSQLiteSimulationsRecord( int const ID );

	// void createSQLiteErrorRecord(
	// 	int const simulationIndex,
	// 	int const errorType,
	// 	std::string const & errorMessage,
	// 	int const cnt
	// );

	// void updateSQLiteErrorRecord( std::string const & errorMessage );

	// void updateSQLiteSimulationRecord(
	// 	bool const completed,
	// 	bool const completedSuccessfully
	// );

	// void updateSQLiteSimulationRecord(
	// 	int const id
	// );

	// void sqliteWriteMessage(const std::string & message);

	// void createZoneExtendedOutput();
}
