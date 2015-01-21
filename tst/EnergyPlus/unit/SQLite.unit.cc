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
		static void SetUpTestCase() { }

		static void TearDownTestCase() { }

		std::unique_ptr<SQLite> sqlite_test;
		std::ostringstream ss;

		virtual void SetUp() {
			ASSERT_NO_THROW(sqlite_test = std::unique_ptr<SQLite>(new SQLite( ss, true, true, ":memory:" )));
			ASSERT_TRUE(sqlite_test->writeOutputToSQLite());
			ASSERT_TRUE(sqlite_test->writeTabularDataToSQLite());
			sqlite_test->sqliteExecuteCommand("PRAGMA foreign_keys = ON;");
		}

		virtual void TearDown() {
			// don't know if this is needed...
			// sqlite3_close(m_db);
			sqlite_test.reset();
			sqlite_test = nullptr;
		}

		std::string storageType( const int storageTypeIndex ) {
			return sqlite_test->storageType( storageTypeIndex );
		}

		std::string timestepTypeName( const int timestepType ) {
			return sqlite_test->timestepTypeName( timestepType );
		}

		std::string reportingFreqName( const int reportingFreqIndex ) {
			return sqlite_test->reportingFreqName( reportingFreqIndex );
		}

		std::string columnText(const unsigned char* column) {
			return std::string(reinterpret_cast<const char*>(column));
		}

		// int executeSQL(const std::string & query) {
		// 	return sqlite3_exec(sqlite->m_db, query.c_str(), NULL, 0, nullptr);
		// }

		// std::pair<bool, int> executeAndReturnFirstInt( const std::string & statement, const int column ) {
		// 	bool success = false;
		// 	int result = 0;

		// 	sqlite3_stmt* sqlStmtPtr;
		// 	int code = sqlite3_prepare_v2(sqlite->m_db, statement.c_str(), -1, &sqlStmtPtr, nullptr);
		// 	code = sqlite3_step(sqlStmtPtr);
		// 	if (code == SQLITE_ROW) {
		// 		result = sqlite3_column_int(sqlStmtPtr, column);
		// 		success = true;
		// 	}
		// 	sqlite3_finalize(sqlStmtPtr);

		// 	return std::make_pair( success, result );
		// }

		bool indexExists( const std::string& indexName ) {
			sqlite3_stmt* sqlStmtPtr;
			std::string sql("pragma index_info(" + indexName + ");");
			bool success = false;
			int rc = sqlite3_prepare_v2(sqlite_test->m_db, sql.c_str(), -1, &sqlStmtPtr, nullptr);
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
			int rc = sqlite3_prepare_v2(sqlite_test->m_db, sql.c_str(), -1, &sqlStmtPtr, nullptr);
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

			int code = sqlite3_prepare_v2(sqlite_test->m_db, statement.c_str(), -1, &sqlStmtPtr, nullptr);
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

	TEST_F( SQLiteFixture, sqliteWriteMessage ) {
		sqlite_test->sqliteWriteMessage("");
		EXPECT_EQ("SQLite3 message, sqlite.err open for processing!\nSQLite3 message, \n", ss.str());
		sqlite_test->sqliteWriteMessage("test message");
		EXPECT_EQ("SQLite3 message, sqlite.err open for processing!\nSQLite3 message, \nSQLite3 message, test message\n", ss.str());
	}

	TEST_F( SQLiteFixture, initializeIndexes ) {
		sqlite_test->sqliteBegin();
		sqlite_test->initializeIndexes();
		sqlite_test->sqliteCommit();

		EXPECT_TRUE(indexExists("rddMTR"));
		EXPECT_TRUE(indexExists("redRD"));
		EXPECT_FALSE(indexExists("dmhdHRI"));
		EXPECT_FALSE(indexExists("dmhrMNI"));
		EXPECT_FALSE(indexExists("tdI"));
	}

	TEST_F( SQLiteFixture, simulationRecords ) {
		sqlite_test->sqliteBegin();
		// There needs to be a simulation record otherwise updateSQLiteSimulationRecord will fail
		sqlite_test->createSQLiteSimulationsRecord( 1, "EnergyPlus Version", "Current Time" );
		sqlite_test->createSQLiteSimulationsRecord( 2, "EnergyPlus Version", "Current Time" );
		sqlite_test->createSQLiteSimulationsRecord( 3, "EnergyPlus Version", "Current Time" );
		sqlite_test->updateSQLiteSimulationRecord( 1, 6 );
		sqlite_test->updateSQLiteSimulationRecord( true, false, 2 );
		sqlite_test->updateSQLiteSimulationRecord( true, true, 3 );
		auto result = queryResult("SELECT * FROM Simulations;", "Simulations");
		sqlite_test->sqliteCommit();

		ASSERT_EQ(3, result.size());
		std::vector<std::string> testResult0 {"1", "EnergyPlus Version", "Current Time", "6", "FALSE", "FALSE"};
		std::vector<std::string> testResult1 {"2", "EnergyPlus Version", "Current Time", "", "1", "0"};
		std::vector<std::string> testResult2 {"3", "EnergyPlus Version", "Current Time", "", "1", "1"};
		EXPECT_EQ(testResult0, result[0]);
		EXPECT_EQ(testResult1, result[1]);
		EXPECT_EQ(testResult2, result[2]);

		sqlite_test->sqliteBegin();
		sqlite_test->updateSQLiteSimulationRecord( true, true );
		result = queryResult("SELECT * FROM Simulations;", "Simulations");
		sqlite_test->sqliteCommit();

		ASSERT_EQ(3, result.size());
		std::vector<std::string> testResult3 {"1", "EnergyPlus Version", "Current Time", "6", "1", "1"};
		EXPECT_EQ(testResult3, result[0]);
	}

	TEST_F( SQLiteFixture, createSQLiteEnvironmentPeriodRecord ) {
		sqlite_test->sqliteBegin();
		// There needs to be a simulation record otherwise the foreign key constraint will fail
		sqlite_test->createSQLiteSimulationsRecord( 1, "EnergyPlus Version", "Current Time" );
		sqlite_test->createSQLiteEnvironmentPeriodRecord( 1, "CHICAGO ANN HTG 99.6% CONDNS DB", 1 );
		sqlite_test->createSQLiteEnvironmentPeriodRecord( 2, "CHICAGO ANN CLG .4% CONDNS WB=>MDB", 1, 1 );
		sqlite_test->createSQLiteEnvironmentPeriodRecord( 3, "CHICAGO ANN HTG 99.6% CONDNS DB", 2 );
		sqlite_test->createSQLiteEnvironmentPeriodRecord( 4, "CHICAGO ANN CLG .4% CONDNS WB=>MDB", 3, 1 );
		auto result = queryResult("SELECT * FROM EnvironmentPeriods;", "EnvironmentPeriods");
		sqlite_test->sqliteCommit();

		ASSERT_EQ(4, result.size());
		std::vector<std::string> testResult0 {"1", "1", "CHICAGO ANN HTG 99.6% CONDNS DB", "1"};
		std::vector<std::string> testResult1 {"2", "1", "CHICAGO ANN CLG .4% CONDNS WB=>MDB", "1"};
		std::vector<std::string> testResult2 {"3", "1", "CHICAGO ANN HTG 99.6% CONDNS DB", "2"};
		std::vector<std::string> testResult3 {"4", "1", "CHICAGO ANN CLG .4% CONDNS WB=>MDB", "3"};
		EXPECT_EQ(testResult0, result[0]);
		EXPECT_EQ(testResult1, result[1]);
		EXPECT_EQ(testResult2, result[2]);
		EXPECT_EQ(testResult3, result[3]);

		sqlite_test->sqliteBegin();
		// This should fail to insert due to foreign key constraint
		sqlite_test->createSQLiteEnvironmentPeriodRecord( 5, "CHICAGO ANN HTG 99.6% CONDNS DB", 1, 100 );
		// This should fail to insert due to duplicate primary key
		sqlite_test->createSQLiteEnvironmentPeriodRecord( 4, "CHICAGO ANN CLG .4% CONDNS WB=>MDB", 1, 1 );
		result = queryResult("SELECT * FROM EnvironmentPeriods;", "EnvironmentPeriods");
		sqlite_test->sqliteCommit();

		EXPECT_EQ(4, result.size());
	}

	TEST_F( SQLiteFixture, errorRecords ) {
		sqlite_test->sqliteBegin();
		// There needs to be a simulation record otherwise the foreign key constraint will fail
		sqlite_test->createSQLiteSimulationsRecord( 1, "EnergyPlus Version", "Current Time" );
		sqlite_test->createSQLiteErrorRecord( 1, 0, "CheckUsedConstructions: There are 2 nominally unused constructions in input.", 1 );
		auto result = queryResult("SELECT * FROM Errors;", "Errors");
		sqlite_test->sqliteCommit();

		ASSERT_EQ(1, result.size());
		std::vector<std::string> testResult0 {"1", "1", "0", "CheckUsedConstructions: There are 2 nominally unused constructions in input.", "1"};
		EXPECT_EQ(testResult0, result[0]);

		sqlite_test->sqliteBegin();
		// updateSQLiteErrorRecord appends the message to the current error message of the last error in the table
		sqlite_test->updateSQLiteErrorRecord( "New error message" );
		result = queryResult("SELECT * FROM Errors;", "Errors");
		sqlite_test->sqliteCommit();

		ASSERT_EQ(1, result.size());
		std::vector<std::string> testResult1 {"1", "1", "0", "CheckUsedConstructions: There are 2 nominally unused constructions in input.  New error message", "1"};
		EXPECT_EQ(testResult1, result[0]);

		sqlite_test->sqliteBegin();
		sqlite_test->createSQLiteErrorRecord( 1, 0, "CheckUsedConstructions: There are 2 nominally unused constructions in input.", 1 );
		sqlite_test->createSQLiteErrorRecord( 1, 0, "This should be changed.", 1 );
		sqlite_test->updateSQLiteErrorRecord( "Changed error message." );
		result = queryResult("SELECT * FROM Errors;", "Errors");
		sqlite_test->sqliteCommit();

		ASSERT_EQ(3, result.size());
		std::vector<std::string> testResult2 {"1", "1", "0", "CheckUsedConstructions: There are 2 nominally unused constructions in input.  New error message", "1"};
		std::vector<std::string> testResult3 {"2", "1", "0", "CheckUsedConstructions: There are 2 nominally unused constructions in input.", "1"};
		std::vector<std::string> testResult4 {"3", "1", "0", "This should be changed.  Changed error message.", "1"};
		EXPECT_EQ(testResult2, result[0]);
		EXPECT_EQ(testResult3, result[1]);
		EXPECT_EQ(testResult4, result[2]);

		sqlite_test->sqliteBegin();
		// This should fail to insert due to foreign key constraint
		sqlite_test->createSQLiteErrorRecord( 100, 0, "CheckUsedConstructions: There are 2 nominally unused constructions in input.", 1 );
		result = queryResult("SELECT * FROM Errors;", "Errors");
		sqlite_test->sqliteCommit();

		EXPECT_EQ(3, result.size());
	}

	TEST_F( SQLiteFixture, createSQLiteReportDictionaryRecord )
	{
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

		sqlite_test->sqliteBegin();
		sqlite_test->createSQLiteReportDictionaryRecord( 1, 1, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false, _ );
		sqlite_test->createSQLiteReportDictionaryRecord( 2, 2, "Facility:Electricity", "", "Facility:Electricity", 1, "J", 1, true, _ );
		sqlite_test->createSQLiteReportDictionaryRecord( 3, 2, "Facility:Electricity", "", "Facility:Electricity", 1, "J", 3, true, _ );
		sqlite_test->createSQLiteReportDictionaryRecord( 4, 1, "HVAC", "", "AHU-1", 2, "", 1, false, _ );
		sqlite_test->createSQLiteReportDictionaryRecord( 5, 1, "HVAC", "", "AHU-1", 2, "", 1, false, "test schedule" );
		auto result = queryResult("SELECT * FROM ReportDataDictionary;", "ReportDataDictionary");
		sqlite_test->sqliteCommit();

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

		sqlite_test->sqliteBegin();
		sqlite_test->createSQLiteReportDictionaryRecord( 6, 3, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false, _ );
		sqlite_test->createSQLiteReportDictionaryRecord( 7, 2, "Facility:Electricity", "", "Facility:Electricity", 3, "J", 1, true, _ );
		sqlite_test->createSQLiteReportDictionaryRecord( 8, 2, "Facility:Electricity", "", "Facility:Electricity", 1, "J", 7, true, _ );
		sqlite_test->createSQLiteReportDictionaryRecord( 9, 1, "HVAC", "", "AHU-1", 2, "", -2, false, _ );
		result = queryResult("SELECT * FROM ReportDataDictionary;", "ReportDataDictionary");
		sqlite_test->sqliteCommit();

		ASSERT_EQ(9, result.size());
		std::vector<std::string> testResult5 {"6", "0", "Unknown!!!", "Zone", "HVAC System", "Environment", "Site Outdoor Air Drybulb Temperature", "Hourly", "", "C"};
		std::vector<std::string> testResult6 {"7", "1", "Sum", "Facility:Electricity", "Unknown!!!", "", "Facility:Electricity", "Hourly", "", "J"};
		std::vector<std::string> testResult7 {"8", "1", "Sum", "Facility:Electricity", "HVAC System", "", "Facility:Electricity", "Unknown!!!", "", "J"};
		std::vector<std::string> testResult8 {"9", "0", "Avg", "HVAC", "Zone", "", "AHU-1", "Unknown!!!", "", ""};
		EXPECT_EQ(testResult5, result[5]);
		EXPECT_EQ(testResult6, result[6]);
		EXPECT_EQ(testResult7, result[7]);
		EXPECT_EQ(testResult8, result[8]);

		sqlite_test->sqliteBegin();
		// This should fail to insert due to duplicate primary key
		sqlite_test->createSQLiteReportDictionaryRecord( 9, 3, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false, _ );
		result = queryResult("SELECT * FROM ReportDataDictionary;", "ReportDataDictionary");
		sqlite_test->sqliteCommit();
		EXPECT_EQ(9, result.size());
	}

	TEST_F( SQLiteFixture, createSQLiteTimeIndexRecord ) {
		// void createSQLiteTimeIndexRecord(
		// 	int const reportingInterval,
		// 	int const recordIndex,
		// 	int const CumlativeSimulationDays,
		//	int const curEnvirNum,
		// 	Optional_int_const Month = _,
		// 	Optional_int_const DayOfMonth = _,
		// 	Optional_int_const Hour = _,
		// 	Optional< Real64 const > EndMinute = _,
		// 	Optional< Real64 const > StartMinute = _,
		// 	Optional_int_const DST = _,
		// 	Optional_string_const DayType = _,
		//	bool const warmupFlag = false
		// );

		sqlite_test->sqliteBegin();
		sqlite_test->createSQLiteTimeIndexRecord( 4, 1, 1, 0 );
		sqlite_test->createSQLiteTimeIndexRecord( 3, 1, 1, 0, 1 );
		sqlite_test->createSQLiteTimeIndexRecord( 2, 1, 1, 0, 1, 1, 1, _, _, 0, "WinterDesignDay" );
		sqlite_test->createSQLiteTimeIndexRecord( 1, 1, 1, 0, 1, 2, 2, _, _, 0, "SummerDesignDay" );
		sqlite_test->createSQLiteTimeIndexRecord( 0, 1, 1, 0, 1, 1, 1, 60, 0, 0, "WinterDesignDay" );
		sqlite_test->createSQLiteTimeIndexRecord( -1, 1, 1, 0, 1, 2, 2, 60, 0, 0, "SummerDesignDay" );
		sqlite_test->createSQLiteTimeIndexRecord( -1, 1, 1, 1, 1, 3, 3, 60, 0, 0, "SummerDesignDay", true );
		auto result = queryResult("SELECT * FROM Time;", "Time");
		sqlite_test->sqliteCommit();

		ASSERT_EQ(7, result.size());
		// some of these are odd.........
		std::vector<std::string> testResult0 {"1", "", "", "", "", "", "1440", "4", "1", "", "0", ""};
		std::vector<std::string> testResult1 {"2", "1", "31", "24", "0", "", "44640", "3", "1", "", "0", ""};
		std::vector<std::string> testResult2 {"3", "1", "1", "24", "0", "0", "1440", "2", "1", "WinterDesignDay", "0", ""};
		std::vector<std::string> testResult3 {"4", "1", "2", "2", "0", "0", "60", "1", "1", "SummerDesignDay", "0", ""};
		std::vector<std::string> testResult4 {"5", "1", "1", "1", "0", "0", "60", "0", "1", "WinterDesignDay", "0", "0"};
		std::vector<std::string> testResult5 {"6", "1", "2", "2", "0", "0", "60", "-1", "1", "SummerDesignDay", "0", "0"};
		std::vector<std::string> testResult6 {"7", "1", "3", "3", "0", "0", "60", "-1", "1", "SummerDesignDay", "1", "1"};
		EXPECT_EQ(testResult0, result[0]);
		EXPECT_EQ(testResult1, result[1]);
		EXPECT_EQ(testResult2, result[2]);
		EXPECT_EQ(testResult3, result[3]);
		EXPECT_EQ(testResult4, result[4]);
		EXPECT_EQ(testResult5, result[5]);
		EXPECT_EQ(testResult6, result[6]);
	}

	TEST_F( SQLiteFixture, createSQLiteReportDataRecord ) {
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

		sqlite_test->sqliteBegin();
		sqlite_test->createSQLiteTimeIndexRecord( 4, 1, 1, 0 );
		sqlite_test->createSQLiteReportDictionaryRecord( 1, 1, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false, _ );
		sqlite_test->createSQLiteReportDataRecord( 1, 999.9 );
		auto result = queryResult("SELECT * FROM ReportData;", "ReportData");
		sqlite_test->sqliteCommit();

		ASSERT_EQ(1, result.size());
	}

	TEST_F( SQLiteFixture, addSQLiteZoneSizingRecord ) {
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

		sqlite_test->sqliteBegin();
		sqlite_test->addSQLiteZoneSizingRecord( "FLOOR 1 IT HALL", "Cooling", 175, 262, 0.013, 0.019, "CHICAGO ANN CLG .4% CONDNS WB=>MDB", "7/21 06:00:00", 20.7, 0.0157, 0.0033 );
		auto result = queryResult("SELECT * FROM ZoneSizes;", "ZoneSizes");
		sqlite_test->sqliteCommit();

		ASSERT_EQ(1, result.size());
		std::vector<std::string> testResult0 {"1", "FLOOR 1 IT HALL", "Cooling", "175.0", "262.0", "0.013", "0.019", "CHICAGO ANN CLG .4% CONDNS WB=>MDB", "7/21 06:00:00", "20.7", "0.0157", "0.0033"};
		EXPECT_EQ(testResult0, result[0]);
	}

	TEST_F( SQLiteFixture, addSQLiteSystemSizingRecord ) {
		// void addSQLiteSystemSizingRecord(
		// 	std::string const & SysName, // the name of the system
		// 	std::string const & VarDesc, // the description of the input variable
		// 	Real64 const VarValue // the value from the sizing calculation
		// );

		sqlite_test->sqliteBegin();
		sqlite_test->addSQLiteSystemSizingRecord( "VAV_1", "Calculated Cooling Design Air Flow Rate [m3/s]", 6.37 );
		sqlite_test->addSQLiteSystemSizingRecord( "VAV_2", "User Cooling Design Air Flow Rate", 5.1 );
		auto result = queryResult("SELECT * FROM SystemSizes;", "SystemSizes");
		sqlite_test->sqliteCommit();

		ASSERT_EQ(2, result.size());
		std::vector<std::string> testResult0 {"1", "VAV_1", "Calculated Cooling Design Air Flow Rate", "6.37", "m3/s"};
		std::vector<std::string> testResult1 {"2", "VAV_2", "User Cooling Design Air Flow Rate", "5.1", ""};
		EXPECT_EQ(testResult0, result[0]);
		EXPECT_EQ(testResult1, result[1]);
	}

	TEST_F( SQLiteFixture, addSQLiteComponentSizingRecord ) {
		// void addSQLiteComponentSizingRecord(
		// 	std::string const & CompType, // the type of the component
		// 	std::string const & CompName, // the name of the component
		// 	std::string const & VarDesc, // the description of the input variable
		// 	Real64 const VarValue // the value from the sizing calculation
		// );

		sqlite_test->sqliteBegin();
		sqlite_test->addSQLiteComponentSizingRecord( "AirTerminal:SingleDuct:VAV:Reheat", "CORE_BOTTOM VAV BOX COMPONENT", "Design Size Maximum Air Flow Rate [m3/s]", 3.23 );
		sqlite_test->addSQLiteComponentSizingRecord( "Coil:Heating:Electric", "CORE_BOTTOM VAV BOX REHEAT COIL", "Design Size Nominal Capacity", 38689.18 );
		auto result = queryResult("SELECT * FROM ComponentSizes;", "ComponentSizes");
		sqlite_test->sqliteCommit();

		ASSERT_EQ(2, result.size());
		std::vector<std::string> testResult0 {"1", "AirTerminal:SingleDuct:VAV:Reheat", "CORE_BOTTOM VAV BOX COMPONENT", "Design Size Maximum Air Flow Rate", "3.23", "m3/s"};
		std::vector<std::string> testResult1 {"2", "Coil:Heating:Electric", "CORE_BOTTOM VAV BOX REHEAT COIL", "Design Size Nominal Capacity", "38689.18", ""};
		EXPECT_EQ(testResult0, result[0]);
		EXPECT_EQ(testResult1, result[1]);
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

	// void createZoneExtendedOutput();
}
