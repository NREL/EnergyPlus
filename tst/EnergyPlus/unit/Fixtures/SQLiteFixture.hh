// EnergyPlus, Copyright (c) 1996-2016, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
//
// If you have questions about your rights to use or distribute this software, please contact
// Berkeley Lab's Innovation & Partnerships Office at IPO@lbl.gov.
//
// NOTICE: This Software was developed under funding from the U.S. Department of Energy and the
// U.S. Government consequently retains certain rights. As such, the U.S. Government has been
// granted for itself and others acting on its behalf a paid-up, nonexclusive, irrevocable,
// worldwide license in the Software to reproduce, distribute copies to the public, prepare
// derivative works, and perform publicly and display publicly, and to permit others to do so.
//
// Redistribution and use in source and binary forms, with or without modification, are permitted
// provided that the following conditions are met:
//
// (1) Redistributions of source code must retain the above copyright notice, this list of
//     conditions and the following disclaimer.
//
// (2) Redistributions in binary form must reproduce the above copyright notice, this list of
//     conditions and the following disclaimer in the documentation and/or other materials
//     provided with the distribution.
//
// (3) Neither the name of the University of California, Lawrence Berkeley National Laboratory,
//     the University of Illinois, U.S. Dept. of Energy nor the names of its contributors may be
//     used to endorse or promote products derived from this software without specific prior
//     written permission.
//
// (4) Use of EnergyPlus(TM) Name. If Licensee (i) distributes the software in stand-alone form
//     without changes from the version obtained under this License, or (ii) Licensee makes a
//     reference solely to the software portion of its product, Licensee must refer to the
//     software as "EnergyPlus version X" software, where "X" is the version number Licensee
//     obtained under this License and may not use a different name for the software. Except as
//     specifically required in this Section (4), Licensee shall not use in a company name, a
//     product name, in advertising, publicity, or other promotional activities any name, trade
//     name, trademark, logo, or other designation of "EnergyPlus", "E+", "e+" or confusingly
//     similar designation, without Lawrence Berkeley National Laboratory's prior written consent.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
// AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
// OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
//
// You are under no obligation whatsoever to provide any bug fixes, patches, or upgrades to the
// features, functionality or performance of the source code ("Enhancements") to anyone; however,
// if you choose to make your Enhancements available either publicly, or directly to Lawrence
// Berkeley National Laboratory, without imposing a separate written license agreement for such
// Enhancements, then you hereby grant the following license: a non-exclusive, royalty-free
// perpetual license to install, use, modify, prepare derivative works, incorporate into other
// computer software, distribute, and sublicense such enhancements or derivative works thereof,
// in binary and source code form.

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
