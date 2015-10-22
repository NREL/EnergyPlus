#ifndef SQLiteFixture_hh_INCLUDED
#define SQLiteFixture_hh_INCLUDED

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "EnergyPlusFixture.hh"
#include <EnergyPlus/SQLiteProcedures.hh>

namespace EnergyPlus {

	class SQLiteFixture : public EnergyPlusFixture
	{

	protected:
		static void SetUpTestCase() { }
		static void TearDownTestCase() { }

		std::unique_ptr<SQLite> sqlite_test;
		std::shared_ptr<std::ostringstream> ss;

		virtual void SetUp() {
			EnergyPlusFixture::SetUp();  // Sets up the base fixture first.
			ss = std::make_shared<std::ostringstream>();
			ASSERT_NO_THROW(sqlite_test = std::unique_ptr<SQLite>(new SQLite( ss, ":memory:", "std::ostringstream", true, true )));
			ASSERT_TRUE(sqlite_test->writeOutputToSQLite());
			ASSERT_TRUE(sqlite_test->writeTabularDataToSQLite());
			sqlite_test->sqliteExecuteCommand("PRAGMA foreign_keys = ON;");
			EXPECT_EQ("SQLite3 message, std::ostringstream open for processing!\n", ss->str());
			ss->str(std::string());
		}

		virtual void TearDown() {
			EnergyPlusFixture::TearDown();  // Remember to tear down the base fixture after cleaning up derived fixture!
		}

		void resetDatabase() {
			ss = std::make_shared<std::ostringstream>();
			ASSERT_NO_THROW(sqlite_test = std::unique_ptr<SQLite>(new SQLite( ss, ":memory:", "std::ostringstream", true, true )));
			ASSERT_TRUE(sqlite_test->writeOutputToSQLite());
			ASSERT_TRUE(sqlite_test->writeTabularDataToSQLite());
			sqlite_test->sqliteExecuteCommand("PRAGMA foreign_keys = ON;");
			EXPECT_EQ("SQLite3 message, std::ostringstream open for processing!\n", ss->str());
			ss->str(std::string());
		}

		// Need to move unit test sqlite_test unique_ptr to EnergyPlus sqlite unique_ptr and back again so usage of the sqlite bindings in
		// EnergyPlus code can be queried against in unit tests.
		// These do not change non-const rvalue references...
		void functionUsingSQLite( std::function<void()> func ) {
			EnergyPlus::sqlite = std::move( sqlite_test );
			func();
			sqlite_test = std::move( EnergyPlus::sqlite );
		}

		// This is causing C2668 'EnergyPlus::SQLiteFixture::functionUsingSQLite' : ambiguous call to overloaded function on Windows CI
		// Maybe Visual Studio 2015 will fix this and catch up to clang/gcc..
		// int functionUsingSQLite( std::function<int()> func ) {
		// 	EnergyPlus::sqlite = std::move( sqlite_test );
		// 	int value = func();
		// 	sqlite_test = std::move( EnergyPlus::sqlite );
		// 	return value;
		// }

		// This is causing C2668 'EnergyPlus::SQLiteFixture::functionUsingSQLite' : ambiguous call to overloaded function on Windows CI
		// Maybe Visual Studio 2015 will fix this and catch up to clang/gcc..
		// template <typename T>
		// T functionUsingSQLite( std::function<T()> func ) {
		// 	EnergyPlus::sqlite = std::move( sqlite_test );
		// 	T value = func();
		// 	sqlite_test = std::move( EnergyPlus::sqlite );
		// 	return value;
		// }

		std::string storageType( const int storageTypeIndex ) {
			return sqlite_test->storageType( storageTypeIndex );
		}

		std::string timestepTypeName( const int timestepType ) {
			return sqlite_test->timestepTypeName( timestepType );
		}

		std::string reportingFreqName( const int reportingFreqIndex ) {
			return sqlite_test->reportingFreqName( reportingFreqIndex );
		}

		std::string columnText( const unsigned char* column ) {
			return std::string( reinterpret_cast<const char*>( column ) );
		}

		int logicalToInteger( const bool value ) {
			return sqlite_test->logicalToInteger(value);
		}

		void adjustReportingHourAndMinutes( int & hour, int & minutes ) {
			sqlite_test->adjustReportingHourAndMinutes( hour, minutes );
		}

		bool indexExists( const std::string& indexName ) {
			sqlite3_stmt* sqlStmtPtr;
			std::string sql("pragma index_info(" + indexName + ");");
			bool success = false;
			int rc = sqlite3_prepare_v2(sqlite_test->m_db.get(), sql.c_str(), -1, &sqlStmtPtr, nullptr);
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
			int rc = sqlite3_prepare_v2(sqlite_test->m_db.get(), sql.c_str(), -1, &sqlStmtPtr, nullptr);
			if ( SQLITE_OK != rc ) {
				sqlite3_finalize(sqlStmtPtr);
				return -1;
			}
			int rowCount = 0;
			while ( SQLITE_ROW == sqlite3_step( sqlStmtPtr ) ) {
				++rowCount;
			}
			sqlite3_finalize(sqlStmtPtr);
			return rowCount;
		}

		std::vector < std::vector < std::string > > queryResult( const std::string& statement, const std::string& tableName ) {
			std::vector < std::vector < std::string > > queryVector;

			int rowCount = columnCount( tableName );
			if ( rowCount < 1 ) return queryVector;

			sqlite3_stmt* sqlStmtPtr;

			sqlite3_prepare_v2(sqlite_test->m_db.get(), statement.c_str(), -1, &sqlStmtPtr, nullptr);
			while ( SQLITE_ROW == sqlite3_step( sqlStmtPtr ) ) {
				std::vector < std::string > valueVector;
				for ( int i = 0; i < rowCount; ++i ) {
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

}

#endif
