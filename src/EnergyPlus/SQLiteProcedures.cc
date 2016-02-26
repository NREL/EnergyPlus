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

// ObjexxFCL Headers

// EnergyPlus Headers
#include "CommandLineInterface.hh"
#include "SQLiteProcedures.hh"
#include "DataGlobals.hh"
#include "DataStringGlobals.hh"
#include "DataEnvironment.hh"
#include "DataHeatBalance.hh"
#include "DataPrecisionGlobals.hh"
#include "DataRoomAirModel.hh"
#include "InputProcessor.hh"
#include "UtilityRoutines.hh"
#include "General.hh"
#include "ScheduleManager.hh"
#include "DataSystemVariables.hh"

#include <sstream>
#include <stdexcept>

namespace EnergyPlus {

const int SQLite::LocalReportEach     = -1;   // Write out each time UpdatedataandLocalReport is called
const int SQLite::LocalReportTimeStep =  0;   // Write out at 'EndTimeStepFlag'
const int SQLite::LocalReportHourly   =  1;   // Write out at 'EndHourFlag'
const int SQLite::LocalReportDaily    =  2;   // Write out at 'EndDayFlag'
const int SQLite::LocalReportMonthly  =  3;   // Write out at end of month (must be determined)
const int SQLite::LocalReportSim      =  4;   // Write out once per environment 'EndEnvrnFlag'
const int SQLite::ReportNameId        =  1;
const int SQLite::ReportForStringId   =  2;
const int SQLite::TableNameId         =  3;
const int SQLite::RowNameId           =  4;
const int SQLite::ColumnNameId        =  5;
const int SQLite::UnitsId             =  6;

std::unique_ptr<SQLite> sqlite;

std::unique_ptr<SQLite> CreateSQLiteDatabase()
{
	try {
		int numberOfSQLiteObjects = InputProcessor::GetNumObjectsFound("Output:SQLite");
		bool writeOutputToSQLite = false;
		bool writeTabularDataToSQLite = false;

		if ( numberOfSQLiteObjects == 1 ) {
			Array1D_string alphas(5);
			int numAlphas;
			Array1D< Real64 > numbers(2);
			int numNumbers;
			int status;

			InputProcessor::GetObjectItem("Output:SQLite",1,alphas,numAlphas,numbers,numNumbers,status);
			if ( numAlphas > 0 ) {
				std::string option = alphas(1);
				if ( InputProcessor::SameString(option,"SimpleAndTabular") ) {
					writeTabularDataToSQLite = true;
					writeOutputToSQLite = true;
				} else if ( InputProcessor::SameString(option,"Simple") ) {
					writeOutputToSQLite = true;
				}
			}
		}
		std::shared_ptr<std::ofstream> errorStream = std::make_shared<std::ofstream>( DataStringGlobals::outputSqliteErrFileName, std::ofstream::out | std::ofstream::trunc );
		return std::unique_ptr<SQLite>(new SQLite( errorStream, DataStringGlobals::outputSqlFileName, DataStringGlobals::outputSqliteErrFileName, writeOutputToSQLite, writeTabularDataToSQLite ));
	} catch( const std::runtime_error& error ) {
		ShowFatalError(error.what());
		return nullptr;
	}
}

void CreateSQLiteZoneExtendedOutput()
{
	if ( sqlite && sqlite->writeOutputToSQLite() ) {
		for ( int zoneNum = 1; zoneNum <= DataGlobals::NumOfZones; ++zoneNum) {
			sqlite->addZoneData( zoneNum, DataHeatBalance::Zone(zoneNum) );
		}
		for (int listNum = 1; listNum <= DataHeatBalance::NumOfZoneLists; ++listNum) {
			sqlite->addZoneListData( listNum, DataHeatBalance::ZoneList(listNum) );
		}
		for (int groupNum = 1; groupNum <= DataHeatBalance::NumOfZoneGroups; ++groupNum) {
			sqlite->addZoneGroupData( groupNum, DataHeatBalance::ZoneGroup(groupNum) );
		}
		for ( int scheduleNumber = 1, numberOfSchedules = ScheduleManager::GetNumberOfSchedules(); scheduleNumber <= numberOfSchedules; ++scheduleNumber) {
			sqlite->addScheduleData( scheduleNumber, ScheduleManager::GetScheduleName(scheduleNumber), ScheduleManager::GetScheduleType(scheduleNumber),
									 ScheduleManager::GetScheduleMinValue(scheduleNumber), ScheduleManager::GetScheduleMaxValue(scheduleNumber) );
		}
		for ( int surfaceNumber = 1; surfaceNumber <= DataSurfaces::TotSurfaces; ++surfaceNumber ) {
			auto const & surface = DataSurfaces::Surface(surfaceNumber);
			sqlite->addSurfaceData( surfaceNumber, surface, DataSurfaces::cSurfaceClass(surface.Class) );
		}
		for (int materialNum = 1; materialNum <= DataHeatBalance::TotMaterials; ++materialNum) {
			sqlite->addMaterialData( materialNum, DataHeatBalance::Material(materialNum) );
		}
		for (int constructNum = 1; constructNum <= DataHeatBalance::TotConstructs; ++constructNum) {
			auto const & construction = DataHeatBalance::Construct(constructNum);
			if (construction.TotGlassLayers == 0) {
				sqlite->addConstructionData( constructNum, construction, construction.UValue );
			} else {
				sqlite->addConstructionData( constructNum, construction, DataHeatBalance::NominalU(constructNum) );
			}
		}
		for (int lightNum = 1; lightNum <= DataHeatBalance::TotLights; ++lightNum) {
			sqlite->addNominalLightingData( lightNum, DataHeatBalance::Lights(lightNum) );
		}
		for (int peopleNum = 1; peopleNum <= DataHeatBalance::TotPeople; ++peopleNum) {
			sqlite->addNominalPeopleData( peopleNum, DataHeatBalance::People(peopleNum) );
		}
		for (int elecEquipNum = 1; elecEquipNum <= DataHeatBalance::TotElecEquip; ++elecEquipNum) {
			sqlite->addNominalElectricEquipmentData( elecEquipNum, DataHeatBalance::ZoneElectric(elecEquipNum) );
		}
		for (int gasEquipNum = 1; gasEquipNum <= DataHeatBalance::TotGasEquip; ++gasEquipNum) {
			sqlite->addNominalGasEquipmentData( gasEquipNum, DataHeatBalance::ZoneGas(gasEquipNum) );
		}
		for (int steamEquipNum = 1; steamEquipNum <= DataHeatBalance::TotStmEquip; ++steamEquipNum) {
			sqlite->addNominalSteamEquipmentData( steamEquipNum, DataHeatBalance::ZoneSteamEq(steamEquipNum) );
		}
		for (int hWEquipNum = 1; hWEquipNum <= DataHeatBalance::TotHWEquip; ++hWEquipNum) {
			sqlite->addNominalHotWaterEquipmentData( hWEquipNum, DataHeatBalance::ZoneHWEq(hWEquipNum) );
		}
		for (int otherEquipNum = 1; otherEquipNum <= DataHeatBalance::TotOthEquip; ++otherEquipNum) {
			sqlite->addNominalOtherEquipmentData( otherEquipNum, DataHeatBalance::ZoneOtherEq(otherEquipNum) );
		}
		for (int bBHeatNum = 1; bBHeatNum <= DataHeatBalance::TotBBHeat; ++bBHeatNum) {
			sqlite->addNominalBaseboardData( bBHeatNum, DataHeatBalance::ZoneBBHeat(bBHeatNum) );
		}
		for (int infilNum = 1; infilNum <= DataHeatBalance::TotInfiltration; ++infilNum) {
			sqlite->addInfiltrationData( infilNum, DataHeatBalance::Infiltration(infilNum) );
		}
		for (int ventNum = 1; ventNum <= DataHeatBalance::TotVentilation; ++ventNum) {
			sqlite->addVentilationData( ventNum, DataHeatBalance::Ventilation(ventNum) );
		}
		for (int zoneNum = 1; zoneNum <= DataGlobals::NumOfZones; ++zoneNum) {
			sqlite->addRoomAirModelData( zoneNum, DataRoomAirModel::AirModel(zoneNum) );
		}

		sqlite->createZoneExtendedOutput();
	}
}

SQLite::SQLite( std::shared_ptr<std::ostream> errorStream, std::string const & dbName, std::string const & errorFileName, bool writeOutputToSQLite, bool writeTabularDataToSQLite )
	:
	SQLiteProcedures(errorStream, writeOutputToSQLite, dbName, errorFileName),
	m_writeTabularDataToSQLite(writeTabularDataToSQLite),
	m_sqlDBTimeIndex(0),
	m_reportDataInsertStmt(nullptr),
	m_reportExtendedDataInsertStmt(nullptr),
	m_reportDictionaryInsertStmt(nullptr),
	m_timeIndexInsertStmt(nullptr),
	m_zoneInfoInsertStmt(nullptr),
	m_zoneInfoZoneListInsertStmt(nullptr),
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
	m_scheduleInsertStmt(nullptr),
	m_daylightMapTitleInsertStmt(nullptr),
	m_daylightMapHourlyTitleInsertStmt(nullptr),
	m_daylightMapHourlyDataInsertStmt(nullptr),
	m_environmentPeriodInsertStmt(nullptr),
	m_simulationsInsertStmt(nullptr),
	m_tabularDataInsertStmt(nullptr),
	m_stringsInsertStmt(nullptr),
	m_stringsLookUpStmt(nullptr),
	m_errorInsertStmt(nullptr),
	m_errorUpdateStmt(nullptr),
	m_simulationUpdateStmt(nullptr),
	m_simulationDataUpdateStmt(nullptr)
{
	if ( m_writeOutputToSQLite ) {
		sqliteExecuteCommand("PRAGMA locking_mode = EXCLUSIVE;");
		sqliteExecuteCommand("PRAGMA journal_mode = OFF;");
		sqliteExecuteCommand("PRAGMA synchronous = OFF;");
		// Turn this to ON for Foreign Key constraints.
		// This must be turned ON for every connection
		// Currently, inserting into daylighting tables does not work with this ON. The ZoneIndex referenced by DaylightMaps does not exist in
		// the database at the time data is inserted.
		sqliteExecuteCommand("PRAGMA foreign_keys = OFF;");

		initializeSimulationsTable();
		initializeEnvironmentPeriodsTable();
		initializeErrorsTable();
		initializeTimeIndicesTable();
		initializeZoneInfoTable();
		initializeZoneListTable();
		initializeZoneGroupTable();
		initializeZoneInfoZoneListTable();
		initializeSchedulesTable();
		initializeMaterialsTable();
		initializeConstructionsTables();
		initializeSurfacesTable();
		initializeReportDataDictionaryTable();
		initializeReportDataTables();
		initializeNominalPeopleTable();
		initializeNominalLightingTable();
		initializeNominalElectricEquipmentTable();
		initializeNominalGasEquipmentTable();
		initializeNominalSteamEquipmentTable();
		initializeNominalHotWaterEquipmentTable();
		initializeNominalOtherEquipmentTable();
		initializeNominalBaseboardHeatTable();
		initializeNominalInfiltrationTable();
		initializeNominalVentilationTable();
		initializeZoneSizingTable();
		initializeSystemSizingTable();
		initializeComponentSizingTable();
		initializeRoomAirModelTable();
		initializeDaylightMapTables();
		initializeViews();

		if (m_writeTabularDataToSQLite) {
			initializeTabularDataTable();
			initializeTabularDataView();
		}
	}
}

SQLite::~SQLite()
{
	sqlite3_finalize(m_reportDataInsertStmt);
	sqlite3_finalize(m_reportExtendedDataInsertStmt);
	sqlite3_finalize(m_reportDictionaryInsertStmt);
	sqlite3_finalize(m_timeIndexInsertStmt);
	sqlite3_finalize(m_zoneInfoInsertStmt);
	sqlite3_finalize(m_zoneListInsertStmt);
	sqlite3_finalize(m_zoneGroupInsertStmt);
	sqlite3_finalize(m_zoneInfoZoneListInsertStmt);
	sqlite3_finalize(m_nominalLightingInsertStmt);
	sqlite3_finalize(m_nominalElectricEquipmentInsertStmt);
	sqlite3_finalize(m_nominalGasEquipmentInsertStmt);
	sqlite3_finalize(m_nominalSteamEquipmentInsertStmt);
	sqlite3_finalize(m_nominalHotWaterEquipmentInsertStmt);
	sqlite3_finalize(m_nominalOtherEquipmentInsertStmt);
	sqlite3_finalize(m_nominalBaseboardHeatInsertStmt);
	sqlite3_finalize(m_surfaceInsertStmt);
	sqlite3_finalize(m_constructionInsertStmt);
	sqlite3_finalize(m_constructionLayerInsertStmt);
	sqlite3_finalize(m_materialInsertStmt);
	sqlite3_finalize(m_infiltrationInsertStmt);
	sqlite3_finalize(m_ventilationInsertStmt);
	sqlite3_finalize(m_nominalPeopleInsertStmt);
	sqlite3_finalize(m_zoneSizingInsertStmt);
	sqlite3_finalize(m_systemSizingInsertStmt);
	sqlite3_finalize(m_componentSizingInsertStmt);
	sqlite3_finalize(m_roomAirModelInsertStmt);
	sqlite3_finalize(m_groundTemperatureInsertStmt);
	sqlite3_finalize(m_weatherFileInsertStmt);
	sqlite3_finalize(m_scheduleInsertStmt);
	sqlite3_finalize(m_daylightMapTitleInsertStmt);
	sqlite3_finalize(m_daylightMapHourlyTitleInsertStmt);
	sqlite3_finalize(m_daylightMapHourlyDataInsertStmt);
	sqlite3_finalize(m_environmentPeriodInsertStmt);
	sqlite3_finalize(m_simulationsInsertStmt);
	sqlite3_finalize(m_tabularDataInsertStmt);
	sqlite3_finalize(m_stringsInsertStmt);
	sqlite3_finalize(m_stringsLookUpStmt);
	sqlite3_finalize(m_errorInsertStmt);
	sqlite3_finalize(m_errorUpdateStmt);
	sqlite3_finalize(m_simulationUpdateStmt);
	sqlite3_finalize(m_simulationDataUpdateStmt);
}

bool SQLite::writeOutputToSQLite() const
{
	return m_writeOutputToSQLite;
}

bool SQLite::writeTabularDataToSQLite() const
{
	return m_writeTabularDataToSQLite;
}

void SQLite::sqliteBegin()
{
	if ( m_writeOutputToSQLite ) {
		sqliteExecuteCommand("BEGIN;");
	}
}

void SQLite::sqliteCommit()
{
	if ( m_writeOutputToSQLite ) {
		sqliteExecuteCommand("COMMIT;");
	}
}

void SQLite::sqliteWriteMessage(const std::string & message)
{
	if ( m_writeOutputToSQLite ) {
		*m_errorStream << "SQLite3 message, " << message << std::endl;
	}
}

void SQLite::initializeReportDataDictionaryTable()
{
	const std::string newTableSQL =
		"CREATE TABLE ReportDataDictionary("
		"ReportDataDictionaryIndex INTEGER PRIMARY KEY, "
		"IsMeter INTEGER, "
		"Type TEXT, "
		"IndexGroup TEXT, "
		"TimestepType TEXT, "
		"KeyValue TEXT, "
		"Name TEXT, "
		"ReportingFrequency TEXT, "
		"ScheduleName TEXT, "
		"Units TEXT);";

	sqliteExecuteCommand(newTableSQL);

	const std::string preparedSQL =
		"INSERT INTO ReportDataDictionary ("
		"ReportDataDictionaryIndex, "
		"IsMeter, "
		"Type, "
		"IndexGroup, "
		"TimestepType, "
		"KeyValue, "
		"Name, "
		"ReportingFrequency, "
		"ScheduleName, "
		"Units) "
		"VALUES(?,?,?,?,?,?,?,?,?,?);";

	sqlitePrepareStatement(m_reportDictionaryInsertStmt,preparedSQL);
}

void SQLite::initializeReportDataTables()
{
	const std::string reportDataTableSQL =
		"CREATE TABLE ReportData ("
		"ReportDataIndex INTEGER PRIMARY KEY, "
		"TimeIndex INTEGER, "
		"ReportDataDictionaryIndex INTEGER, "
		"Value REAL, "
		"FOREIGN KEY(TimeIndex) REFERENCES Time(TimeIndex) "
		"ON DELETE CASCADE ON UPDATE CASCADE "
		"FOREIGN KEY(ReportDataDictionaryIndex) REFERENCES ReportDataDictionary(ReportDataDictionaryIndex) "
		"ON DELETE CASCADE ON UPDATE CASCADE "
		");";

	sqliteExecuteCommand(reportDataTableSQL);

	const std::string reportDataInsertSQL =
		"INSERT INTO ReportData ("
		"ReportDataIndex, "
		"TimeIndex, "
		"ReportDataDictionaryIndex, "
		"Value) "
		"VALUES(?,?,?,?);";

	sqlitePrepareStatement(m_reportDataInsertStmt,reportDataInsertSQL);

	const std::string reportExtendedDataTableSQL =
		"CREATE TABLE ReportExtendedData ("
		"ReportExtendedDataIndex INTEGER PRIMARY KEY, "
		"ReportDataIndex INTEGER, "
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
		"MinMinute INTEGER, "
		"FOREIGN KEY(ReportDataIndex) REFERENCES ReportData(ReportDataIndex) "
		"ON DELETE CASCADE ON UPDATE CASCADE "
		");";

	sqliteExecuteCommand(reportExtendedDataTableSQL);

	const std::string reportExtendedDataInsertSQL =
		"INSERT INTO ReportExtendedData ("
		"ReportExtendedDataIndex, "
		"ReportDataIndex, "
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
		"VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?);";

	sqlitePrepareStatement(m_reportExtendedDataInsertStmt,reportExtendedDataInsertSQL);
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

void SQLite::initializeZoneInfoZoneListTable()
{
	const std::string zoneInfoZoneListTableSQL =
		"CREATE TABLE ZoneInfoZoneLists ("
		"ZoneListIndex INTEGER NOT NULL, "
		"ZoneIndex INTEGER NOT NULL, "
		"PRIMARY KEY(ZoneListIndex, ZoneIndex), "
		"FOREIGN KEY(ZoneListIndex) REFERENCES ZoneLists(ZoneListIndex) "
		"ON DELETE CASCADE ON UPDATE CASCADE, "
		"FOREIGN KEY(ZoneIndex) REFERENCES Zones(ZoneIndex) "
		"ON DELETE CASCADE ON UPDATE CASCADE "
		");";

	sqliteExecuteCommand(zoneInfoZoneListTableSQL);

	const std::string zoneInfoZoneListInsertSQL =
		"INSERT INTO ZoneInfoZoneLists ("
		"ZoneListIndex, "
		"ZoneIndex) "
		"VALUES (?,?);";

	sqlitePrepareStatement(m_zoneInfoZoneListInsertStmt,zoneInfoZoneListInsertSQL);
}

void SQLite::initializeNominalPeopleTable()
{
	const std::string nominalPeopleTableSQL =
		"CREATE TABLE NominalPeople ( "
		"NominalPeopleIndex INTEGER PRIMARY KEY, ObjectName TEXT, ZoneIndex INTEGER,"
		"NumberOfPeople INTEGER, NumberOfPeopleScheduleIndex INTEGER, ActivityScheduleIndex INTEGER, FractionRadiant REAL, "
		"FractionConvected REAL, WorkEfficiencyScheduleIndex INTEGER, ClothingEfficiencyScheduleIndex INTEGER, "
		"AirVelocityScheduleIndex INTEGER, Fanger INTEGER, Pierce INTEGER, KSU INTEGER, "
		"MRTCalcType INTEGER, SurfaceIndex INTEGER, "
		"AngleFactorListName TEXT, AngleFactorList INTEGER, UserSpecifeidSensibleFraction REAL, Show55Warning INTEGER, "
		"FOREIGN KEY(ZoneIndex) REFERENCES Zones(ZoneIndex) "
		"ON DELETE CASCADE ON UPDATE CASCADE, "
		"FOREIGN KEY(NumberOfPeopleScheduleIndex) REFERENCES Schedules(ScheduleIndex) "
		"ON UPDATE CASCADE, "
		"FOREIGN KEY(ActivityScheduleIndex) REFERENCES Schedules(ScheduleIndex) "
		"ON UPDATE CASCADE, "
		"FOREIGN KEY(WorkEfficiencyScheduleIndex) REFERENCES Schedules(ScheduleIndex) "
		"ON UPDATE CASCADE, "
		"FOREIGN KEY(ClothingEfficiencyScheduleIndex) REFERENCES Schedules(ScheduleIndex) "
		"ON UPDATE CASCADE, "
		"FOREIGN KEY(AirVelocityScheduleIndex) REFERENCES Schedules(ScheduleIndex) "
		"ON UPDATE CASCADE, "
		"FOREIGN KEY(SurfaceIndex) REFERENCES Surfaces(SurfaceIndex) "
		"ON UPDATE CASCADE "
		");";

	sqliteExecuteCommand(nominalPeopleTableSQL);

	const std::string nominalPeopleInsertSQL =
		"INSERT INTO NominalPeople VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);";

	sqlitePrepareStatement(m_nominalPeopleInsertStmt,nominalPeopleInsertSQL);
}

void SQLite::initializeNominalLightingTable()
{
	const std::string nominalLightingTableSQL =
		"CREATE TABLE NominalLighting ( "
		"NominalLightingIndex INTEGER PRIMARY KEY, ObjectName TEXT, "
		"ZoneIndex INTEGER, ScheduleIndex INTEGER, DesignLevel REAL, FractionReturnAir REAL, FractionRadiant REAL, "
		"FractionShortWave REAL, FractionReplaceable REAL, FractionConvected REAL, EndUseSubcategory TEXT, "
		"FOREIGN KEY(ZoneIndex) REFERENCES Zones(ZoneIndex) "
		"ON DELETE CASCADE ON UPDATE CASCADE, "
		"FOREIGN KEY(ScheduleIndex) REFERENCES Schedules(ScheduleIndex) "
		"ON UPDATE CASCADE "
		");";

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
		"FractionConvected REAL, EndUseSubcategory TEXT, "
		"FOREIGN KEY(ZoneIndex) REFERENCES Zones(ZoneIndex) "
		"ON DELETE CASCADE ON UPDATE CASCADE, "
		"FOREIGN KEY(ScheduleIndex) REFERENCES Schedules(ScheduleIndex) "
		"ON UPDATE CASCADE "
		");";

	sqliteExecuteCommand(nominalElectricEquipmentTableSQL);

	const std::string nominalElectricEquipmentInsertSQL =
		"INSERT INTO NominalElectricEquipment VALUES(?,?,?,?,?,?,?,?,?,?);";

	sqlitePrepareStatement(m_nominalElectricEquipmentInsertStmt,nominalElectricEquipmentInsertSQL);
}

void SQLite::initializeNominalGasEquipmentTable()
{
	const std::string nominalGasEquipmentTableSQL =
		"CREATE TABLE NominalGasEquipment( "
		"NominalGasEquipmentIndex INTEGER PRIMARY KEY, ObjectName TEXT, "
		"ZoneIndex INTEGER, ScheduleIndex INTEGER, "
		"DesignLevel REAL, FractionLatent REAL, FractionRadiant REAL, FractionLost REAL, "
		"FractionConvected REAL, EndUseSubcategory TEXT, "
		"FOREIGN KEY(ZoneIndex) REFERENCES Zones(ZoneIndex) "
		"ON DELETE CASCADE ON UPDATE CASCADE, "
		"FOREIGN KEY(ScheduleIndex) REFERENCES Schedules(ScheduleIndex) "
		"ON UPDATE CASCADE "
		");";

	sqliteExecuteCommand(nominalGasEquipmentTableSQL);

	const std::string nominalGasEquipmentInsertSQL =
		"INSERT INTO NominalGasEquipment VALUES(?,?,?,?,?,?,?,?,?,?);";

	sqlitePrepareStatement(m_nominalGasEquipmentInsertStmt,nominalGasEquipmentInsertSQL);
}

void SQLite::initializeNominalSteamEquipmentTable()
{
	const std::string nominalSteamEquipmentTableSQL =
		"CREATE TABLE NominalSteamEquipment( "
		"NominalSteamEquipmentIndex INTEGER PRIMARY KEY, ObjectName TEXT, "
		"ZoneIndex INTEGER, ScheduleIndex INTEGER, DesignLevel REAL, "
		"FractionLatent REAL, FractionRadiant REAL, FractionLost REAL, "
		"FractionConvected REAL, EndUseSubcategory TEXT, "
		"FOREIGN KEY(ZoneIndex) REFERENCES Zones(ZoneIndex) "
		"ON DELETE CASCADE ON UPDATE CASCADE, "
		"FOREIGN KEY(ScheduleIndex) REFERENCES Schedules(ScheduleIndex) "
		"ON UPDATE CASCADE "
		");";

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
		"FractionConvected REAL, EndUseSubcategory TEXT, "
		"FOREIGN KEY(ZoneIndex) REFERENCES Zones(ZoneIndex) "
		"ON DELETE CASCADE ON UPDATE CASCADE, "
		"FOREIGN KEY(SchedNo) REFERENCES Schedules(ScheduleIndex) "
		"ON UPDATE CASCADE "
		");";

	sqliteExecuteCommand(nominalHotWaterEquipmentTableSQL);

	const std::string nominalHotWaterEquipmentInsertSQL =
		"INSERT INTO NominalHotWaterEquipment VALUES(?,?,?,?,?,?,?,?,?,?);";

	sqlitePrepareStatement(m_nominalHotWaterEquipmentInsertStmt,nominalHotWaterEquipmentInsertSQL);
}

void SQLite::initializeNominalOtherEquipmentTable()
{
	const std::string nominalOtherEquipmentTableSQL =
		"CREATE TABLE NominalOtherEquipment( "
		"NominalOtherEquipmentIndex INTEGER PRIMARY KEY, ObjectName TEXT, "
		"ZoneIndex INTEGER, ScheduleIndex INTEGER, DesignLevel REAL, FractionLatent REAL, "
		"FractionRadiant REAL, FractionLost REAL, "
		"FractionConvected REAL, EndUseSubcategory TEXT, "
		"FOREIGN KEY(ZoneIndex) REFERENCES Zones(ZoneIndex) "
		"ON DELETE CASCADE ON UPDATE CASCADE, "
		"FOREIGN KEY(ScheduleIndex) REFERENCES Schedules(ScheduleIndex) "
		"ON UPDATE CASCADE "
		");";

	sqliteExecuteCommand(nominalOtherEquipmentTableSQL);

	const std::string nominalOtherEquipmentInsertSQL =
		"INSERT INTO NominalOtherEquipment VALUES(?,?,?,?,?,?,?,?,?,?);";

	sqlitePrepareStatement(m_nominalOtherEquipmentInsertStmt,nominalOtherEquipmentInsertSQL);
}

void SQLite::initializeNominalBaseboardHeatTable()
{
	const std::string nominalBaseboardHeatersTableSQL =
		"CREATE TABLE NominalBaseboardHeaters ( "
		"NominalBaseboardHeaterIndex INTEGER PRIMARY KEY, ObjectName TEXT, "
		"ZoneIndex INTEGER, ScheduleIndex INTEGER, CapatLowTemperature REAL, LowTemperature REAL, CapatHighTemperature REAL, "
		"HighTemperature REAL, FractionRadiant REAL, FractionConvected REAL, EndUseSubcategory TEXT, "
		"FOREIGN KEY(ZoneIndex) REFERENCES Zones(ZoneIndex) "
		"ON DELETE CASCADE ON UPDATE CASCADE, "
		"FOREIGN KEY(ScheduleIndex) REFERENCES Schedules(ScheduleIndex) "
		"ON UPDATE CASCADE "
		");";

	sqliteExecuteCommand(nominalBaseboardHeatersTableSQL);

	const std::string nominalBaseboardHeatInsertSQL =
		"INSERT INTO NominalBaseboardHeaters VALUES(?,?,?,?,?,?,?,?,?,?,?);";

	sqlitePrepareStatement(m_nominalBaseboardHeatInsertStmt,nominalBaseboardHeatInsertSQL);
}

void SQLite::initializeSurfacesTable()
{
	const std::string surfacesTableSQL =
		"CREATE TABLE Surfaces ( "
		"SurfaceIndex INTEGER PRIMARY KEY, SurfaceName TEXT, ConstructionIndex INTEGER, "
		"ClassName TEXT, Area REAL, GrossArea REAL, Perimeter REAL, "
		"Azimuth REAL, Height REAL, Reveal REAL, "
		"Shape INTEGER, Sides INTEGER, Tilt REAL, Width REAL, HeatTransferSurf INTEGER, "
		"BaseSurfaceIndex INTEGER, ZoneIndex INTEGER, ExtBoundCond INTEGER,  "
		"ExtSolar INTEGER, ExtWind INTEGER, "
		"FOREIGN KEY(ConstructionIndex) REFERENCES Constructions(ConstructionIndex) "
		"ON UPDATE CASCADE, "
		"FOREIGN KEY(BaseSurfaceIndex) REFERENCES Surfaces(SurfaceIndex) "
		"ON UPDATE CASCADE, "
		"FOREIGN KEY(ZoneIndex) REFERENCES Zones(ZoneIndex) "
		"ON DELETE CASCADE ON UPDATE CASCADE "
		");";

	sqliteExecuteCommand(surfacesTableSQL);

	const std::string surfaceInsertSQL =
		"INSERT INTO Surfaces VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);";

	sqlitePrepareStatement(m_surfaceInsertStmt,surfaceInsertSQL);
}

void SQLite::initializeConstructionsTables()
{
	const std::string constructionsTableSQL =
		"CREATE TABLE Constructions ( "
		"ConstructionIndex INTEGER PRIMARY KEY, Name TEXT, TotalLayers INTEGER, "
		"TotalSolidLayers INTEGER, TotalGlassLayers INTEGER, InsideAbsorpVis REAL, OutsideAbsorpVis REAL, "
		"InsideAbsorpSolar REAL, OutsideAbsorpSolar REAL, InsideAbsorpThermal REAL, OutsideAbsorpThermal REAL, "
		"OutsideRoughness INTEGER, TypeIsWindow INTEGER, Uvalue REAL"
		");";

	sqliteExecuteCommand(constructionsTableSQL);

	const std::string constructionInsertSQL =
		"INSERT INTO Constructions VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?);";

	sqlitePrepareStatement(m_constructionInsertStmt,constructionInsertSQL);

	const std::string constructionLayersTableSQL =
		"CREATE TABLE ConstructionLayers ( "
		"ConstructionLayersIndex INTEGER PRIMARY KEY, "
		"ConstructionIndex INTEGER, LayerIndex INTEGER, MaterialIndex INTEGER, "
		"FOREIGN KEY(ConstructionIndex) REFERENCES Constructions(ConstructionIndex) "
		"ON DELETE CASCADE ON UPDATE CASCADE, "
		"FOREIGN KEY(MaterialIndex) REFERENCES Materials(MaterialIndex) "
		"ON UPDATE CASCADE "
		");";

	sqliteExecuteCommand(constructionLayersTableSQL);

	const std::string constructionLayerInsertSQL =
		"INSERT INTO ConstructionLayers(ConstructionIndex, LayerIndex, MaterialIndex) VALUES(?,?,?);";

	sqlitePrepareStatement(m_constructionLayerInsertStmt,constructionLayerInsertSQL);
}

void SQLite::initializeMaterialsTable()
{
	const std::string materialsTableSQL =
		"CREATE TABLE Materials ( "
		"MaterialIndex INTEGER PRIMARY KEY, "
		"Name TEXT, MaterialType INTEGER, Roughness INTEGER, "
		"Conductivity REAL, Density REAL, IsoMoistCap REAL, Porosity REAL, Resistance REAL, "
		"ROnly INTEGER, SpecHeat REAL, ThermGradCoef REAL, Thickness REAL, VaporDiffus REAL "
		");";

	sqliteExecuteCommand(materialsTableSQL);

	const std::string materialInsertSQL =
		"INSERT INTO Materials VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?);";

	sqlitePrepareStatement(m_materialInsertStmt,materialInsertSQL);
}

void SQLite::initializeZoneListTable()
{
	const std::string zoneListsTableSQL =
		"CREATE TABLE ZoneLists ( "
		"ZoneListIndex INTEGER PRIMARY KEY, Name TEXT);";

	sqliteExecuteCommand(zoneListsTableSQL);

	const std::string zoneListInsertSQL =
		"INSERT INTO ZoneLists VALUES(?,?);";

	sqlitePrepareStatement(m_zoneListInsertStmt,zoneListInsertSQL);
}

void SQLite::initializeZoneGroupTable()
{
	const std::string zoneGroupsTableSQL =
		"CREATE TABLE ZoneGroups ( "
		"ZoneGroupIndex INTEGER PRIMARY KEY, "
		"ZoneGroupName TEXT, "
		"ZoneListIndex INTEGER, "
		"ZoneListMultiplier INTEGER, "
		"FOREIGN KEY(ZoneListIndex) REFERENCES ZoneLists(ZoneListIndex) "
		"ON UPDATE CASCADE "
		");";

	sqliteExecuteCommand(zoneGroupsTableSQL);

	const std::string zoneGroupInsertSQL =
		"INSERT INTO ZoneGroups VALUES(?,?,?,?);";

	sqlitePrepareStatement(m_zoneGroupInsertStmt,zoneGroupInsertSQL);
}

void SQLite::initializeNominalInfiltrationTable()
{
	const std::string nominalInfiltrationTableSQL =
		"CREATE TABLE NominalInfiltration ( "
		"NominalInfiltrationIndex INTEGER PRIMARY KEY, "
		"ObjectName TEXT, "
		"ZoneIndex INTEGER, ScheduleIndex INTEGER, DesignLevel REAL, "
		"FOREIGN KEY(ZoneIndex) REFERENCES Zones(ZoneIndex) "
		"ON DELETE CASCADE ON UPDATE CASCADE, "
		"FOREIGN KEY(ScheduleIndex) REFERENCES Schedules(ScheduleIndex) "
		"ON UPDATE CASCADE "
		");";

	sqliteExecuteCommand(nominalInfiltrationTableSQL);

	const std::string infiltrationInsertSQL =
		"INSERT INTO NominalInfiltration (NominalInfiltrationIndex, ObjectName, ZoneIndex, ScheduleIndex, DesignLevel)"
		"VALUES (?,?,?,?,?);";

	sqlitePrepareStatement(m_infiltrationInsertStmt,infiltrationInsertSQL);
}

void SQLite::initializeNominalVentilationTable()
{
	const std::string nominalVentilationTableSQL =
		"CREATE TABLE NominalVentilation ( "
		"NominalVentilationIndex INTEGER PRIMARY KEY, "
		"ObjectName TEXT, "
		"ZoneIndex INTEGER, ScheduleIndex INTEGER, DesignLevel REAL, "
		"FOREIGN KEY(ZoneIndex) REFERENCES Zones(ZoneIndex) "
		"ON DELETE CASCADE ON UPDATE CASCADE, "
		"FOREIGN KEY(ScheduleIndex) REFERENCES Schedules(ScheduleIndex) "
		"ON UPDATE CASCADE "
		");";

	sqliteExecuteCommand(nominalVentilationTableSQL);

	const std::string ventilationInsertSQL =
		"INSERT INTO NominalVentilation VALUES(?,?,?,?,?);";

	sqlitePrepareStatement(m_ventilationInsertStmt,ventilationInsertSQL);
}

void SQLite::initializeZoneSizingTable()
{
	const std::string zoneSizesTableSQL =
		"CREATE TABLE ZoneSizes ( "
		"ZoneSizesIndex INTEGER PRIMARY KEY, ZoneName TEXT, LoadType TEXT, "
		"CalcDesLoad REAL, UserDesLoad REAL, CalcDesFlow REAL, UserDesFlow REAL, DesDayName TEXT, PeakHrMin TEXT, "
		"PeakTemp REAL, PeakHumRat REAL, CalcOutsideAirFlow REAL, DOASHeatAddRate REAL"
		");";

	sqliteExecuteCommand(zoneSizesTableSQL);

	const std::string zoneSizingInsertSQL =
		"INSERT INTO ZoneSizes VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?);";

	sqlitePrepareStatement(m_zoneSizingInsertStmt,zoneSizingInsertSQL);
}

void SQLite::initializeSystemSizingTable()
{
	const std::string systemSizesTableSQL =
		"CREATE TABLE SystemSizes (SystemSizesIndex INTEGER PRIMARY KEY, SystemName TEXT, Description TEXT, Value REAL, Units TEXT);";

	sqliteExecuteCommand(systemSizesTableSQL);

	const std::string systemSizingInsertSQL =
		"INSERT INTO SystemSizes VALUES(?,?,?,?,?);";

	sqlitePrepareStatement(m_systemSizingInsertStmt,systemSizingInsertSQL);
}

void SQLite::initializeComponentSizingTable()
{
	const std::string componentSizesTableSQL =
		"CREATE TABLE ComponentSizes (ComponentSizesIndex INTEGER PRIMARY KEY, "
		"CompType TEXT, CompName TEXT, Description TEXT, Value REAL, Units TEXT);";

	sqliteExecuteCommand(componentSizesTableSQL);

	const std::string componentSizingInsertSQL =
		"INSERT INTO ComponentSizes VALUES (?,?,?,?,?,?);";

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
		"CREATE TABLE DaylightMaps ( "
		"MapNumber INTEGER PRIMARY KEY, MapName TEXT, "
		"Environment TEXT, Zone INTEGER, ReferencePt1 TEXT, ReferencePt2 TEXT, Z REAL, "
		"FOREIGN KEY(Zone) REFERENCES Zones(ZoneIndex) "
		"ON DELETE CASCADE ON UPDATE CASCADE "
		");";

	sqliteExecuteCommand(daylightMapsTableSQL);

	const std::string daylightMapTitleInsertSQL =
		"INSERT INTO DaylightMaps VALUES(?,?,?,?,?,?,?);";

	sqlitePrepareStatement(m_daylightMapTitleInsertStmt,daylightMapTitleInsertSQL);

	const std::string daylightMapHourlyReportsTableSQL =
		"CREATE TABLE DaylightMapHourlyReports ( "
		"HourlyReportIndex INTEGER PRIMARY KEY, "
		"MapNumber INTEGER, Month INTEGER, DayOfMonth INTEGER, Hour INTEGER, "
		"FOREIGN KEY(MapNumber) REFERENCES DaylightMaps(MapNumber) "
		"ON DELETE CASCADE ON UPDATE CASCADE "
		");";

	sqliteExecuteCommand(daylightMapHourlyReportsTableSQL);

	const std::string daylightMapHourlyTitleInsertSQL =
		"INSERT INTO DaylightMapHourlyReports VALUES(?,?,?,?,?);";

	sqlitePrepareStatement(m_daylightMapHourlyTitleInsertStmt,daylightMapHourlyTitleInsertSQL);

	const std::string daylightMapHourlyDataTableSQL =
		"CREATE TABLE DaylightMapHourlyData ( "
		"HourlyDataIndex INTEGER PRIMARY KEY, HourlyReportIndex INTEGER, "
		"X REAL, Y REAL, Illuminance REAL, "
		"FOREIGN KEY(HourlyReportIndex) REFERENCES DaylightMapHourlyReports(HourlyReportIndex) "
		"ON DELETE CASCADE ON UPDATE CASCADE "
		");";

	sqliteExecuteCommand(daylightMapHourlyDataTableSQL);

	const std::string daylightMapHourlyDataInsertSQL =
		"INSERT INTO DaylightMapHourlyData VALUES(?,?,?,?,?);";

	sqlitePrepareStatement(m_daylightMapHourlyDataInsertStmt,daylightMapHourlyDataInsertSQL);
}

void SQLite::initializeViews()
{
	const std::string reportVariableWithTimeViewSQL =
		"CREATE VIEW ReportVariableWithTime AS "
		"SELECT rd.ReportDataIndex, rd.TimeIndex, rd.ReportDataDictionaryIndex, red.ReportExtendedDataIndex, rd.Value, "
		"t.Month, t.Day, t.Hour, t.Minute, t.Dst, t.Interval, t.IntervalType, t.SimulationDays, t.DayType, t.EnvironmentPeriodIndex, t.WarmupFlag, "
		"rdd.IsMeter, rdd.Type, rdd.IndexGroup, rdd.TimestepType, rdd.KeyValue, rdd.Name, rdd.ReportingFrequency, rdd.ScheduleName, rdd.Units, "
		"red.MaxValue, red.MaxMonth, red.MaxDay, red.MaxStartMinute, red.MaxMinute, red.MinValue, red.MinMonth, red.MinDay, red.MinStartMinute, red.MinMinute "
		"FROM ReportData As rd "
		"INNER JOIN ReportDataDictionary As rdd "
		"ON rd.ReportDataDictionaryIndex = rdd.ReportDataDictionaryIndex "
		"LEFT OUTER JOIN ReportExtendedData As red "
		"ON rd.ReportDataIndex = red.ReportDataIndex "
		"INNER JOIN Time As t "
		"ON rd.TimeIndex = t.TimeIndex;";

	sqliteExecuteCommand(reportVariableWithTimeViewSQL);

	const std::string reportVariableDataViewSQL =
		"CREATE VIEW ReportVariableData AS "
		"SELECT rd.ReportDataIndex As rowid, rd.TimeIndex, rd.ReportDataDictionaryIndex As ReportVariableDataDictionaryIndex, "
		"rd.Value As VariableValue, red.ReportExtendedDataIndex As ReportVariableExtendedDataIndex "
		"FROM ReportData As rd "
		"LEFT OUTER JOIN ReportExtendedData As red "
		"ON rd.ReportDataIndex = red.ReportDataIndex;";

	sqliteExecuteCommand(reportVariableDataViewSQL);

	const std::string reportVariableDataDictionaryViewSQL =
		"CREATE VIEW ReportVariableDataDictionary AS "
		"SELECT rdd.ReportDataDictionaryIndex As ReportVariableDataDictionaryIndex, rdd.Type As VariableType, rdd.IndexGroup, rdd.TimestepType, "
		"rdd.KeyValue, rdd.Name As VariableName, rdd.ReportingFrequency, rdd.ScheduleName, rdd.Units As VariableUnits "
		"FROM ReportDataDictionary As rdd;";

	sqliteExecuteCommand(reportVariableDataDictionaryViewSQL);

	const std::string reportVariableExtendedDataViewSQL =
		"CREATE VIEW ReportVariableExtendedData AS "
		"SELECT red.ReportExtendedDataIndex As ReportVariableExtendedDataIndex, red.MaxValue, red.MaxMonth, red.MaxDay, "
		"red.MaxStartMinute, red.MaxMinute, red.MinValue, red.MinMonth, red.MinDay, red.MinStartMinute, red.MinMinute "
		"FROM ReportExtendedData As red;";

	sqliteExecuteCommand(reportVariableExtendedDataViewSQL);

	const std::string reportMeterDataViewSQL =
		"CREATE VIEW ReportMeterData AS "
		"SELECT rd.ReportDataIndex As rowid, rd.TimeIndex, rd.ReportDataDictionaryIndex As ReportMeterDataDictionaryIndex, "
		"rd.Value As VariableValue, red.ReportExtendedDataIndex As ReportVariableExtendedDataIndex "
		"FROM ReportData As rd "
		"LEFT OUTER JOIN ReportExtendedData As red "
		"ON rd.ReportDataIndex = red.ReportDataIndex "
		"INNER JOIN ReportDataDictionary As rdd "
		"ON rd.ReportDataDictionaryIndex = rdd.ReportDataDictionaryIndex "
		"WHERE rdd.IsMeter = 1;";

	sqliteExecuteCommand(reportMeterDataViewSQL);

	const std::string reportMeterDataDictionaryViewSQL =
		"CREATE VIEW ReportMeterDataDictionary AS "
		"SELECT rdd.ReportDataDictionaryIndex As ReportMeterDataDictionaryIndex, rdd.Type As VariableType, rdd.IndexGroup, rdd.TimestepType, "
		"rdd.KeyValue, rdd.Name As VariableName, rdd.ReportingFrequency, rdd.ScheduleName, rdd.Units As VariableUnits "
		"FROM ReportDataDictionary As rdd "
		"WHERE rdd.IsMeter = 1;";

	sqliteExecuteCommand(reportMeterDataDictionaryViewSQL);

	const std::string reportMeterExtendedDataViewSQL =
		"CREATE VIEW ReportMeterExtendedData AS "
		"SELECT red.ReportExtendedDataIndex As ReportMeterExtendedDataIndex, red.MaxValue, red.MaxMonth, red.MaxDay, "
		"red.MaxStartMinute, red.MaxMinute, red.MinValue, red.MinMonth, red.MinDay, red.MinStartMinute, red.MinMinute "
		"FROM ReportExtendedData As red "
		"LEFT OUTER JOIN ReportData As rd "
		"ON rd.ReportDataIndex = red.ReportDataIndex "
		"INNER JOIN ReportDataDictionary As rdd "
		"ON rd.ReportDataDictionaryIndex = rdd.ReportDataDictionaryIndex "
		"WHERE rdd.IsMeter = 1;";

	sqliteExecuteCommand(reportMeterExtendedDataViewSQL);
}

void SQLite::initializeSimulationsTable()
{
	const std::string simulationsTableSQL =
		"CREATE TABLE Simulations (SimulationIndex INTEGER PRIMARY KEY, "
		"EnergyPlusVersion TEXT, TimeStamp TEXT, NumTimestepsPerHour INTEGER, Completed BOOL, "
		"CompletedSuccessfully BOOL);";

	sqliteExecuteCommand(simulationsTableSQL);

	const std::string simulationsInsertSQL =
		"INSERT INTO Simulations(SimulationIndex, EnergyPlusVersion, TimeStamp, Completed, CompletedSuccessfully) "
		"VALUES(?,?,?,'FALSE','FALSE');";

	sqlitePrepareStatement(m_simulationsInsertStmt,simulationsInsertSQL);

	const std::string simulationUpdateSQL =
		"UPDATE Simulations SET "
		"Completed = ?, CompletedSuccessfully = ? "
		"WHERE SimulationIndex = ?";

	sqlitePrepareStatement(m_simulationUpdateStmt,simulationUpdateSQL);

	const std::string simulationDataUpdateSQL =
		"UPDATE Simulations SET "
		"NumTimestepsPerHour = ? "
		"WHERE SimulationIndex = ?";

	sqlitePrepareStatement(m_simulationDataUpdateStmt,simulationDataUpdateSQL);
}

void SQLite::initializeErrorsTable()
{
	const std::string errorsTableSQL =
		"CREATE TABLE Errors ( "
		"ErrorIndex INTEGER PRIMARY KEY, SimulationIndex INTEGER, "
		"ErrorType INTEGER, ErrorMessage TEXT, Count INTEGER, "
		"FOREIGN KEY(SimulationIndex) REFERENCES Simulations(SimulationIndex) "
		"ON DELETE CASCADE ON UPDATE CASCADE "
		");";

	sqliteExecuteCommand(errorsTableSQL);

	const std::string errorInsertSQL =
		"INSERT INTO Errors VALUES(?,?,?,?,?);";

	sqlitePrepareStatement(m_errorInsertStmt,errorInsertSQL);

	const std::string errorUpdateSQL =
		"UPDATE Errors SET "
		"ErrorMessage = ErrorMessage || ? WHERE ErrorIndex = (SELECT ErrorIndex FROM Errors ORDER BY ErrorIndex DESC LIMIT 1)";

	sqlitePrepareStatement(m_errorUpdateStmt,errorUpdateSQL);
}

void SQLite::initializeEnvironmentPeriodsTable()
{
	const std::string environmentPeriodsTableSQL =
		"CREATE TABLE EnvironmentPeriods ( "
		"EnvironmentPeriodIndex INTEGER PRIMARY KEY, "
		"SimulationIndex INTEGER, EnvironmentName TEXT, EnvironmentType INTEGER, "
		"FOREIGN KEY(SimulationIndex) REFERENCES Simulations(SimulationIndex) "
		"ON DELETE CASCADE ON UPDATE CASCADE "
		");";

	sqliteExecuteCommand(environmentPeriodsTableSQL);

	const std::string environmentPeriodInsertSQL =
		"INSERT INTO EnvironmentPeriods VALUES(?,?,?,?);";

	sqlitePrepareStatement(m_environmentPeriodInsertStmt,environmentPeriodInsertSQL);
}

void SQLite::initializeTabularDataTable()
{
	const std::string sql =
		"CREATE TABLE StringTypes ( "
		"StringTypeIndex INTEGER PRIMARY KEY, "
		"Value TEXT"
		");";

	sqliteExecuteCommand(sql);

	sqliteExecuteCommand("INSERT INTO StringTypes VALUES(" + std::to_string(ReportNameId) + ",'ReportName');");
	sqliteExecuteCommand("INSERT INTO StringTypes VALUES(" + std::to_string(ReportForStringId) + ",'ReportForString');");
	sqliteExecuteCommand("INSERT INTO StringTypes VALUES(" + std::to_string(TableNameId) + ",'TableName');");
	sqliteExecuteCommand("INSERT INTO StringTypes VALUES(" + std::to_string(RowNameId) + ",'RowName');");
	sqliteExecuteCommand("INSERT INTO StringTypes VALUES(" + std::to_string(ColumnNameId) + ",'ColumnName');");
	sqliteExecuteCommand("INSERT INTO StringTypes VALUES(" + std::to_string(UnitsId) + ",'Units');");

	const std::string sql2 =
		"CREATE TABLE Strings ( "
		"StringIndex INTEGER PRIMARY KEY, "
		"StringTypeIndex INTEGER, "
		"Value TEXT, "
		"UNIQUE(StringTypeIndex, Value), "
		"FOREIGN KEY(StringTypeIndex) REFERENCES StringTypes(StringTypeIndex) "
		"ON UPDATE CASCADE "
		");";

	sqliteExecuteCommand(sql2);

	const std::string sql3 = "INSERT INTO Strings (StringIndex,StringTypeIndex,Value) VALUES(?,?,?);";

	sqlitePrepareStatement(m_stringsInsertStmt,sql3);

	const std::string sql4 = "SELECT StringIndex FROM Strings WHERE StringTypeIndex=? AND Value=?;";

	sqlitePrepareStatement(m_stringsLookUpStmt,sql4);

	const std::string sql5 =
		"CREATE TABLE TabularData ( "
		"TabularDataIndex INTEGER PRIMARY KEY, "
		"ReportNameIndex INTEGER, "
		"ReportForStringIndex INTEGER, "
		"TableNameIndex INTEGER, "
		"RowNameIndex INTEGER, "
		"ColumnNameIndex INTEGER, "
		"UnitsIndex INTEGER, "
		"SimulationIndex INTEGER, "
		"RowId INTEGER, "
		"ColumnId INTEGER, "
		"Value TEXT "
		"Value TEXT, "
		"FOREIGN KEY(ReportNameIndex) REFERENCES Strings(StringIndex) "
		"ON UPDATE CASCADE "
		"FOREIGN KEY(ReportForStringIndex) REFERENCES Strings(StringIndex) "
		"ON UPDATE CASCADE "
		"FOREIGN KEY(TableNameIndex) REFERENCES Strings(StringIndex) "
		"ON UPDATE CASCADE "
		"FOREIGN KEY(RowNameIndex) REFERENCES Strings(StringIndex) "
		"ON UPDATE CASCADE "
		"FOREIGN KEY(ColumnNameIndex) REFERENCES Strings(StringIndex) "
		"ON UPDATE CASCADE "
		"FOREIGN KEY(UnitsIndex) REFERENCES Strings(StringIndex) "
		"ON UPDATE CASCADE "
		"FOREIGN KEY(SimulationIndex) REFERENCES Simulations(SimulationIndex) "
		"ON DELETE CASCADE ON UPDATE CASCADE "
		");";

	sqliteExecuteCommand(sql5);

	const std::string sql6 = "INSERT INTO TabularData VALUES(?,?,?,?,?,?,?,?,?,?,?);";

	sqlitePrepareStatement(m_tabularDataInsertStmt,sql6);
}

void SQLite::initializeTabularDataView()
{
	const std::string sql = "CREATE VIEW TabularDataWithStrings AS SELECT "
							"td.TabularDataIndex, "
							"td.Value As Value, "
							"reportn.Value As ReportName, "
							"fs.Value As ReportForString, "
							"tn.Value As TableName, "
							"rn.Value As RowName, "
							"cn.Value As ColumnName, "
							"u.Value As Units "
							"FROM TabularData As td "
							"INNER JOIN Strings As reportn ON reportn.StringIndex=td.ReportNameIndex "
							"INNER JOIN Strings As fs ON fs.StringIndex=td.ReportForStringIndex "
							"INNER JOIN Strings As tn ON tn.StringIndex=td.TableNameIndex "
							"INNER JOIN Strings As rn ON rn.StringIndex=td.RowNameIndex "
							"INNER JOIN Strings As cn ON cn.StringIndex=td.ColumnNameIndex "
							"INNER JOIN Strings As u ON u.StringIndex=td.UnitsIndex;";

	sqliteExecuteCommand(sql);
}

void SQLite::initializeIndexes()
{
	if ( m_writeOutputToSQLite ) {
		sqliteExecuteCommand("CREATE INDEX rddMTR ON ReportDataDictionary (IsMeter);");
		sqliteExecuteCommand("CREATE INDEX redRD ON ReportExtendedData (ReportDataIndex);");

		// These following indexes could potentially be used by sqlite, but for a narrow range of queries
		// There are no built in views that use these indexes in their queries.
		// sqliteExecuteCommand("CREATE INDEX dmhdHRI ON DaylightMapHourlyData (HourlyReportIndex);");
		// sqliteExecuteCommand("CREATE INDEX dmhrMNI ON DaylightMapHourlyReports (MapNumber);");

		// This following index is used by sqlite, but doesn't seem to increase performance in my testing
		// sqliteExecuteCommand("CREATE INDEX tdI ON TabularData (ReportNameIndex, ReportForStringIndex, TableNameIndex, RowNameIndex, ColumnNameIndex, UnitsIndex, Value);");
	}
}

std::string SQLite::storageType(const int storageTypeIndex)
{
	std::string result;

	switch(storageTypeIndex) {
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

std::string SQLite::timestepTypeName(const int timestepType)
{
	std::string result;

	switch(timestepType) {
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

std::string SQLite::reportingFreqName(const int reportingFreqIndex)
{
	std::string result;

	switch(reportingFreqIndex) {
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
	switch(minutes) {
	case 60:
		minutes = 0;
		break;
	default:
		--hour;
	}
}

void SQLite::parseUnitsAndDescription(const std::string & combinedString, std::string & units, std::string & description)
{
	std::size_t leftPos = combinedString.find("[");
	std::size_t rightPos = combinedString.find("]");

	if ( (leftPos < rightPos) && (leftPos != std::string::npos) && (rightPos != std::string::npos) ) {
		units = combinedString.substr(leftPos + 1,rightPos - leftPos - 1);
		description = combinedString.substr(0,leftPos - 1);
	} else {
		units = "";
		description = combinedString;
	}
}

int SQLite::logicalToInteger(const bool value)
{
	return value ? 1 : 0;
}

void SQLite::createSQLiteReportDictionaryRecord (
	int const reportVariableReportID,
	int const storeTypeIndex,
	std::string const & indexGroup,
	std::string const & keyedValueString,
	std::string const & variableName,
	int const indexType,
	std::string const & units,
	int const reportingFreq,
	bool isMeter,
	Optional_string_const scheduleName
)
{
	if ( m_writeOutputToSQLite ) {
		sqliteBindInteger(m_reportDictionaryInsertStmt, 1, reportVariableReportID);
		sqliteBindLogical(m_reportDictionaryInsertStmt, 2, isMeter);
		sqliteBindText(m_reportDictionaryInsertStmt, 3, storageType(storeTypeIndex));
		sqliteBindText(m_reportDictionaryInsertStmt, 4, indexGroup);
		sqliteBindText(m_reportDictionaryInsertStmt, 5, timestepTypeName(indexType));
		sqliteBindText(m_reportDictionaryInsertStmt, 6, keyedValueString);
		sqliteBindText(m_reportDictionaryInsertStmt, 7, variableName);
		sqliteBindText(m_reportDictionaryInsertStmt, 8, reportingFreqName(reportingFreq));

		if ( scheduleName.present() ) {
			sqliteBindText(m_reportDictionaryInsertStmt, 9, scheduleName());
		} else {
			sqliteBindNULL(m_reportDictionaryInsertStmt, 9);
		}

		sqliteBindText(m_reportDictionaryInsertStmt, 10, units);

		sqliteStepCommand(m_reportDictionaryInsertStmt);
		sqliteResetCommand(m_reportDictionaryInsertStmt);
	}
}

void SQLite::createSQLiteReportDataRecord(
	int const recordIndex,
	Real64 const value,
	Optional_int_const reportingInterval,
	Optional< Real64 const > minValue,
	Optional_int_const minValueDate,
	Optional< Real64 const > maxValue,
	Optional_int_const maxValueDate,
	Optional_int_const minutesPerTimeStep
)
{
	if ( m_writeOutputToSQLite ) {
		++m_dataIndex;

		sqliteBindInteger(m_reportDataInsertStmt, 1, m_dataIndex);
		sqliteBindForeignKey(m_reportDataInsertStmt, 2, m_sqlDBTimeIndex);
		sqliteBindForeignKey(m_reportDataInsertStmt, 3, recordIndex);
		sqliteBindDouble(m_reportDataInsertStmt, 4, value);

		sqliteStepCommand(m_reportDataInsertStmt);
		sqliteResetCommand(m_reportDataInsertStmt);

		if (reportingInterval.present() && minValueDate != 0 && maxValueDate != 0) {
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

			adjustReportingHourAndMinutes(minHour, minMinute);
			adjustReportingHourAndMinutes(maxHour, maxMinute);

			++m_extendedDataIndex;

			if ( minutesPerTimeStep.present() ) { // This is for data created by a 'Report Meter' statement
				switch(reportingInterval()) {
				case LocalReportHourly:
				case LocalReportDaily:
				case LocalReportMonthly:
				case LocalReportSim:
					sqliteBindInteger(m_reportExtendedDataInsertStmt, 1, m_extendedDataIndex);
					sqliteBindForeignKey(m_reportExtendedDataInsertStmt, 2, m_dataIndex);

					sqliteBindDouble(m_reportExtendedDataInsertStmt, 3, maxValue);
					sqliteBindInteger(m_reportExtendedDataInsertStmt, 4, maxMonth);
					sqliteBindInteger(m_reportExtendedDataInsertStmt, 5, maxDay);
					sqliteBindInteger(m_reportExtendedDataInsertStmt, 6, maxHour);
					sqliteBindInteger(m_reportExtendedDataInsertStmt, 7, maxMinute - minutesPerTimeStep + 1);
					sqliteBindInteger(m_reportExtendedDataInsertStmt, 8, maxMinute);

					sqliteBindDouble(m_reportExtendedDataInsertStmt, 9, minValue);
					sqliteBindInteger(m_reportExtendedDataInsertStmt, 10, minMonth);
					sqliteBindInteger(m_reportExtendedDataInsertStmt, 11, minDay);
					sqliteBindInteger(m_reportExtendedDataInsertStmt, 12, minHour);
					sqliteBindInteger(m_reportExtendedDataInsertStmt, 13, minMinute - minutesPerTimeStep + 1);
					sqliteBindInteger(m_reportExtendedDataInsertStmt, 14, minMinute);

					sqliteStepCommand(m_reportExtendedDataInsertStmt);
					sqliteResetCommand(m_reportExtendedDataInsertStmt);
					break;

				case LocalReportTimeStep:
					--m_extendedDataIndex; // Reset the data index to account for the error
					break;

				default:
					--m_extendedDataIndex; // Reset the data index to account for the error
					std::stringstream ss;
					ss << "Illegal reportingInterval passed to CreateSQLiteMeterRecord: " << reportingInterval;
					sqliteWriteMessage(ss.str());
				}
			} else { // This is for data created by a 'Report Variable' statement
				switch(reportingInterval()) {
				case LocalReportDaily:
				case LocalReportMonthly:
				case LocalReportSim:
					sqliteBindInteger(m_reportExtendedDataInsertStmt, 1, m_extendedDataIndex);
					sqliteBindForeignKey(m_reportExtendedDataInsertStmt, 2, m_dataIndex);

					sqliteBindDouble(m_reportExtendedDataInsertStmt, 3, maxValue);
					sqliteBindInteger(m_reportExtendedDataInsertStmt, 4, maxMonth);
					sqliteBindInteger(m_reportExtendedDataInsertStmt, 5, maxDay);
					sqliteBindInteger(m_reportExtendedDataInsertStmt, 6, maxHour);
					sqliteBindNULL(m_reportExtendedDataInsertStmt, 7);
					sqliteBindInteger(m_reportExtendedDataInsertStmt, 8, maxMinute);

					sqliteBindDouble(m_reportExtendedDataInsertStmt, 9, minValue);
					sqliteBindInteger(m_reportExtendedDataInsertStmt, 10, minMonth);
					sqliteBindInteger(m_reportExtendedDataInsertStmt, 11, minDay);
					sqliteBindInteger(m_reportExtendedDataInsertStmt, 12, minHour);
					sqliteBindNULL(m_reportExtendedDataInsertStmt, 13);
					sqliteBindInteger(m_reportExtendedDataInsertStmt, 14, minMinute);

					sqliteStepCommand(m_reportExtendedDataInsertStmt);
					sqliteResetCommand(m_reportExtendedDataInsertStmt);
					break;

				default:
					--m_extendedDataIndex; // Reset the data index to account for the error
					std::stringstream ss;
					ss << "Illegal reportingInterval passed to CreateSQLiteMeterRecord: " << reportingInterval;
					sqliteWriteMessage(ss.str());
				}
			}
		}
	}
}

void SQLite::createSQLiteTimeIndexRecord(
	int const reportingInterval,
	int const EP_UNUSED( recordIndex ),
	int const cumlativeSimulationDays,
	int const curEnvirNum,
	Optional_int_const month,
	Optional_int_const dayOfMonth,
	Optional_int_const hour,
	Optional< Real64 const > endMinute,
	Optional< Real64 const > startMinute,
	Optional_int_const dst,
	Optional_string_const dayType,
	bool const warmupFlag
)
{
	if ( m_writeOutputToSQLite ) {
		int intStartMinute = 0;
		int intervalInMinutes = 60;

		static const std::vector<int> lastDayOfMonth = {31,28,31,30,31,30,31,31,30,31,30,31};

		switch(reportingInterval) {
		case LocalReportEach:
		case LocalReportTimeStep: {
			if ( !month.present() || !dayOfMonth.present() || !hour.present() ||
					!endMinute.present() || !startMinute.present() || !dst.present() || !dayType.present() ) {
				sqliteWriteMessage("Empty month, dayOfMonth, hour, endMinute, startMinute, dst, or dayType passed to CreateSQLiteTimeIndexRecord");
				break;
			}
			++m_sqlDBTimeIndex;

			int intEndMinute = static_cast<int>(endMinute() + 0.5);
			intStartMinute = static_cast<int>(startMinute() + 0.5);
			int t_hour = hour();
			intervalInMinutes = intEndMinute - intStartMinute;
			adjustReportingHourAndMinutes(t_hour, intEndMinute);

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
			sqliteBindInteger(m_timeIndexInsertStmt, 11, curEnvirNum);
			sqliteBindLogical(m_timeIndexInsertStmt, 12, warmupFlag);

			sqliteStepCommand(m_timeIndexInsertStmt);
			sqliteResetCommand(m_timeIndexInsertStmt);

			break;
		}
		case LocalReportHourly: {
			if ( !month.present() || !dayOfMonth.present() || !hour.present() || !dst.present() || !dayType.present() ) {
				sqliteWriteMessage("Empty month, dayOfMonth, hour, dst, or dayType passed to CreateSQLiteTimeIndexRecord");
				break;
			}
			++m_sqlDBTimeIndex;

			sqliteBindInteger(m_timeIndexInsertStmt, 1, m_sqlDBTimeIndex);
			sqliteBindInteger(m_timeIndexInsertStmt, 2, month());
			sqliteBindInteger(m_timeIndexInsertStmt, 3, dayOfMonth());
			sqliteBindInteger(m_timeIndexInsertStmt, 4, hour());
			sqliteBindInteger(m_timeIndexInsertStmt, 5, 0);
			sqliteBindInteger(m_timeIndexInsertStmt, 6, dst());
			sqliteBindInteger(m_timeIndexInsertStmt, 7, intervalInMinutes);
			sqliteBindInteger(m_timeIndexInsertStmt, 8, reportingInterval);
			sqliteBindInteger(m_timeIndexInsertStmt, 9, cumlativeSimulationDays);
			sqliteBindText(m_timeIndexInsertStmt, 10, dayType());
			sqliteBindInteger(m_timeIndexInsertStmt, 11, curEnvirNum);

			sqliteStepCommand(m_timeIndexInsertStmt);
			sqliteResetCommand(m_timeIndexInsertStmt);

			break;
		}
		case LocalReportDaily: {
			if ( !month.present() || !dayOfMonth.present() || !dst.present() || !dayType.present() ) {
				sqliteWriteMessage("Empty month, dayOfMonth, dst, or dayType passed to CreateSQLiteTimeIndexRecord");
				break;
			}
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
			sqliteBindInteger(m_timeIndexInsertStmt, 11, curEnvirNum);

			sqliteStepCommand(m_timeIndexInsertStmt);
			sqliteResetCommand(m_timeIndexInsertStmt);

			break;
		}
		case LocalReportMonthly: {
			if ( !month.present() ) {
				sqliteWriteMessage("Empty month passed to CreateSQLiteTimeIndexRecord");
				break;
			}
			++m_sqlDBTimeIndex;

			intervalInMinutes = 60*24*lastDayOfMonth[month() - 1];
			sqliteBindInteger(m_timeIndexInsertStmt, 1, m_sqlDBTimeIndex);
			sqliteBindInteger(m_timeIndexInsertStmt, 2, month());
			sqliteBindInteger(m_timeIndexInsertStmt, 3, lastDayOfMonth[month() - 1]);
			sqliteBindInteger(m_timeIndexInsertStmt, 4, 24);
			sqliteBindInteger(m_timeIndexInsertStmt, 5, 0);
			sqliteBindNULL(m_timeIndexInsertStmt, 6);
			sqliteBindInteger(m_timeIndexInsertStmt, 7, intervalInMinutes);
			sqliteBindInteger(m_timeIndexInsertStmt, 8, reportingInterval);
			sqliteBindInteger(m_timeIndexInsertStmt, 9, cumlativeSimulationDays);
			sqliteBindNULL(m_timeIndexInsertStmt, 10);
			sqliteBindInteger(m_timeIndexInsertStmt, 11, curEnvirNum);

			sqliteStepCommand(m_timeIndexInsertStmt);
			sqliteResetCommand(m_timeIndexInsertStmt);

			break;
		}
		case LocalReportSim: {
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
			sqliteBindInteger(m_timeIndexInsertStmt, 11, curEnvirNum);

			sqliteStepCommand (m_timeIndexInsertStmt);
			sqliteResetCommand (m_timeIndexInsertStmt);

			break;
		}
		default: {
			std::stringstream ss;
			ss << "Illegal reportingInterval passed to CreateSQLiteTimeIndexRecord: " << reportingInterval;
			sqliteWriteMessage(ss.str());
		}
		}
	}
}

void SQLite::addSQLiteZoneSizingRecord(
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
	Real64 const minOAVolFlow, // zone design minimum outside air flow rate [m3/s]
	Real64 const DOASHeatAddRate // zone design heat addition rate from the DOAS [W]
)
{
	if ( m_writeOutputToSQLite ) {
		++m_zoneSizingIndex;
		sqliteBindInteger(m_zoneSizingInsertStmt, 1, m_zoneSizingIndex);
		sqliteBindText(m_zoneSizingInsertStmt, 2, zoneName);
		sqliteBindText(m_zoneSizingInsertStmt, 3, loadType);

		sqliteBindDouble(m_zoneSizingInsertStmt, 4, calcDesLoad);
		sqliteBindDouble(m_zoneSizingInsertStmt, 5, userDesLoad);
		sqliteBindDouble(m_zoneSizingInsertStmt, 6, calcDesFlow);
		sqliteBindDouble(m_zoneSizingInsertStmt, 7, userDesFlow);

		sqliteBindText(m_zoneSizingInsertStmt, 8, desDayName);
		sqliteBindText(m_zoneSizingInsertStmt, 9, peakHrMin);

		sqliteBindDouble(m_zoneSizingInsertStmt, 10, peakTemp);
		sqliteBindDouble(m_zoneSizingInsertStmt, 11, peakHumRat);
		sqliteBindDouble(m_zoneSizingInsertStmt, 12, minOAVolFlow);
		sqliteBindDouble( m_zoneSizingInsertStmt, 13, DOASHeatAddRate );

		sqliteStepCommand(m_zoneSizingInsertStmt);
		sqliteResetCommand(m_zoneSizingInsertStmt);
	}
}

void SQLite::addSQLiteSystemSizingRecord(
	std::string const & sysName, // the name of the system
	std::string const & varDesc, // the description of the input variable
	Real64 const varValue // the value from the sizing calculation
)
{
	if ( m_writeOutputToSQLite ) {
		++m_systemSizingIndex;
		std::string description;
		std::string units;

		parseUnitsAndDescription(varDesc,units,description);

		sqliteBindInteger(m_systemSizingInsertStmt, 1, m_systemSizingIndex);
		sqliteBindText(m_systemSizingInsertStmt, 2, sysName);
		sqliteBindText(m_systemSizingInsertStmt, 3, description);
		sqliteBindDouble(m_systemSizingInsertStmt, 4, varValue);
		sqliteBindText(m_systemSizingInsertStmt, 5, units);

		sqliteStepCommand(m_systemSizingInsertStmt);
		sqliteResetCommand(m_systemSizingInsertStmt);
	}
}

void SQLite::addSQLiteComponentSizingRecord(
	std::string const & compType, // the type of the component
	std::string const & compName, // the name of the component
	std::string const & varDesc, // the description of the input variable
	Real64 const varValue // the value from the sizing calculation
)
{
	if ( m_writeOutputToSQLite ) {
		++m_componentSizingIndex;

		std::string description;
		std::string units;

		parseUnitsAndDescription(varDesc,units,description);

		sqliteBindInteger(m_componentSizingInsertStmt, 1, m_componentSizingIndex);
		sqliteBindText(m_componentSizingInsertStmt, 2, compType);
		sqliteBindText(m_componentSizingInsertStmt, 3, compName);
		sqliteBindText(m_componentSizingInsertStmt, 4, description);
		sqliteBindDouble(m_componentSizingInsertStmt, 5, varValue);
		sqliteBindText(m_componentSizingInsertStmt, 6, units);

		sqliteStepCommand(m_componentSizingInsertStmt);
		sqliteResetCommand(m_componentSizingInsertStmt);
	}
}

void SQLite::createSQLiteDaylightMapTitle(
	int const mapNum,
	std::string const & mapName,
	std::string const & environmentName,
	int const zone,
	std::string const & refPt1,
	std::string const & refPt2,
	Real64 const zCoord
)
{
	if ( m_writeOutputToSQLite ) {
		// for some reason it is adding extra mapNumbers that are getting UNIQUE constraint ignored.
		// Might need to look into it, basically I think something is getting double inserted (12/06/14)
		sqliteBindInteger(m_daylightMapTitleInsertStmt, 1, mapNum);
		sqliteBindText(m_daylightMapTitleInsertStmt, 2, mapName);
		sqliteBindText(m_daylightMapTitleInsertStmt, 3, environmentName);
		sqliteBindForeignKey(m_daylightMapTitleInsertStmt, 4, zone);
		sqliteBindText(m_daylightMapTitleInsertStmt, 5, refPt1);
		sqliteBindText(m_daylightMapTitleInsertStmt, 6, refPt2);
		sqliteBindDouble(m_daylightMapTitleInsertStmt, 7, zCoord);

		sqliteStepCommand(m_daylightMapTitleInsertStmt);
		sqliteResetCommand(m_daylightMapTitleInsertStmt);
	}
}

void SQLite::createSQLiteDaylightMap(
	int const mapNum,
	int const month,
	int const dayOfMonth,
	int const hourOfDay,
	int const nX,
	Array1< Real64 > const & x,
	int const nY,
	Array1< Real64 > const & y,
	Array2< Real64 > const & illuminance
)
{
	if ( m_writeOutputToSQLite ) {
		++m_hourlyReportIndex;
		sqliteBindInteger(m_daylightMapHourlyTitleInsertStmt, 1, m_hourlyReportIndex);
		sqliteBindForeignKey(m_daylightMapHourlyTitleInsertStmt, 2, mapNum);
		sqliteBindInteger(m_daylightMapHourlyTitleInsertStmt, 3, month);
		sqliteBindInteger(m_daylightMapHourlyTitleInsertStmt, 4, dayOfMonth);
		sqliteBindInteger(m_daylightMapHourlyTitleInsertStmt, 5, hourOfDay);

		sqliteStepCommand(m_daylightMapHourlyTitleInsertStmt);
		sqliteResetCommand(m_daylightMapHourlyTitleInsertStmt);

		for ( int yIndex = 1; yIndex <= nY; ++yIndex ) {
			for ( int xIndex = 1; xIndex <= nX; ++xIndex ) {
				++m_hourlyDataIndex;
				sqliteBindInteger(m_daylightMapHourlyDataInsertStmt, 1, m_hourlyDataIndex);
				sqliteBindForeignKey(m_daylightMapHourlyDataInsertStmt, 2, m_hourlyReportIndex);
				sqliteBindDouble(m_daylightMapHourlyDataInsertStmt, 3, x(xIndex));
				sqliteBindDouble(m_daylightMapHourlyDataInsertStmt, 4, y(yIndex));
				sqliteBindDouble(m_daylightMapHourlyDataInsertStmt, 5, illuminance(xIndex, yIndex));

				sqliteStepCommand(m_daylightMapHourlyDataInsertStmt);
				sqliteResetCommand(m_daylightMapHourlyDataInsertStmt);
			}
		}
	}
}

void SQLite::createSQLiteTabularDataRecords(
	Array2D_string const & body, // html table row, html table column
	Array1D_string const & rowLabels,
	Array1D_string const & columnLabels,
	std::string const & reportName,
	std::string const & reportForString,
	std::string const & tableName
)
{
	if ( m_writeTabularDataToSQLite ) {
		size_t sizeColumnLabels = columnLabels.size();
		size_t sizeRowLabels = rowLabels.size();

		int const reportNameIndex = createSQLiteStringTableRecord(reportName, ReportNameId);
		int const reportForStringIndex = createSQLiteStringTableRecord(reportForString, ReportForStringId);
		int const tableNameIndex = createSQLiteStringTableRecord(tableName, TableNameId);
		int unitsIndex;

		for ( size_t iCol = 0, k = body.index(1,1); iCol < sizeColumnLabels; ++iCol ) {
			std::string colUnits;
			std::string colDescription;
			parseUnitsAndDescription(columnLabels[iCol], colUnits, colDescription);

			int const columnLabelIndex = createSQLiteStringTableRecord(colDescription, ColumnNameId);

			if ( !colUnits.empty() ) {
				unitsIndex = createSQLiteStringTableRecord(colUnits, UnitsId);
			}

			for ( size_t iRow = 0; iRow < sizeRowLabels; ++iRow ) {
				++m_tabularDataIndex;
				std::string rowUnits;
				std::string rowDescription;
				parseUnitsAndDescription(rowLabels[iRow], rowUnits, rowDescription);

				int const rowLabelIndex = createSQLiteStringTableRecord(rowDescription, RowNameId);

				if ( colUnits.empty() ) {
					unitsIndex = createSQLiteStringTableRecord(rowUnits, UnitsId);
				}

				sqliteBindInteger(m_tabularDataInsertStmt,1,m_tabularDataIndex);
				sqliteBindForeignKey(m_tabularDataInsertStmt,2,reportNameIndex);
				sqliteBindForeignKey(m_tabularDataInsertStmt,3,reportForStringIndex);
				sqliteBindForeignKey(m_tabularDataInsertStmt,4,tableNameIndex);
				sqliteBindForeignKey(m_tabularDataInsertStmt,5,rowLabelIndex);
				sqliteBindForeignKey(m_tabularDataInsertStmt,6,columnLabelIndex);
				sqliteBindForeignKey(m_tabularDataInsertStmt,7,unitsIndex);
				sqliteBindForeignKey(m_tabularDataInsertStmt,8,1);
				sqliteBindInteger(m_tabularDataInsertStmt,9,iRow);
				sqliteBindInteger(m_tabularDataInsertStmt,10,iCol);
				sqliteBindText(m_tabularDataInsertStmt,11,body[k]);

				sqliteStepCommand(m_tabularDataInsertStmt);
				sqliteResetCommand(m_tabularDataInsertStmt);

				++k;
			}
		}
	}
}

int SQLite::createSQLiteStringTableRecord(std::string const & stringValue, int const stringType)
{
	int rowId = -1;
	if ( m_writeOutputToSQLite ) {

		auto ret = m_tabularStrings.emplace( make_pair(stringValue, stringType), 0 );

		if ( !ret.second ) {
			rowId = ret.first->second;
		} else {
			sqliteBindInteger(m_stringsInsertStmt, 1, m_stringIndex);
			sqliteBindForeignKey(m_stringsInsertStmt, 2, stringType);
			sqliteBindText(m_stringsInsertStmt, 3, stringValue);

			int errorcode = sqliteStepCommand(m_stringsInsertStmt);
			sqliteResetCommand(m_stringsInsertStmt);

			if ( errorcode != SQLITE_CONSTRAINT ) {
				rowId = m_stringIndex++;
			} else {
				sqliteBindInteger(m_stringsLookUpStmt, 1, stringType);
				sqliteBindText(m_stringsLookUpStmt, 2, stringValue);
				sqliteStepCommand(m_stringsLookUpStmt);
				rowId = sqlite3_column_int(m_stringsLookUpStmt,0);
				sqliteResetCommand(m_stringsLookUpStmt);
			}
			ret.first->second = rowId;
		}
	}
	return rowId;
}

void SQLite::createSQLiteSimulationsRecord( int const id, const std::string& verString, const std::string& currentDateTime )
{
	if ( m_writeOutputToSQLite ) {
		sqliteBindInteger(m_simulationsInsertStmt, 1, id);
		sqliteBindText(m_simulationsInsertStmt, 2, verString);
		sqliteBindText(m_simulationsInsertStmt, 3, currentDateTime);

		sqliteStepCommand(m_simulationsInsertStmt);
		sqliteResetCommand(m_simulationsInsertStmt);
	}
}

void SQLite::createSQLiteErrorRecord(
	int const simulationIndex,
	int const errorType,
	std::string const & errorMessage,
	int const cnt
)
{
	if ( m_writeOutputToSQLite ) {
		++m_errorIndex;

		sqliteBindInteger(m_errorInsertStmt, 1, m_errorIndex);
		sqliteBindForeignKey(m_errorInsertStmt, 2, simulationIndex);
		sqliteBindInteger(m_errorInsertStmt, 3, errorType);
		sqliteBindText(m_errorInsertStmt, 4, errorMessage);
		sqliteBindInteger(m_errorInsertStmt, 5, cnt);

		sqliteStepCommand(m_errorInsertStmt);
		sqliteResetCommand(m_errorInsertStmt);
	}
}

void SQLite::updateSQLiteErrorRecord( std::string const & errorMessage )
{
	if ( m_writeOutputToSQLite ) {
		sqliteBindText(m_errorUpdateStmt, 1, "  " + errorMessage);

		sqliteStepCommand(m_errorUpdateStmt);
		sqliteResetCommand(m_errorUpdateStmt);
	}
}

void SQLite::updateSQLiteSimulationRecord( int const id, int const numOfTimeStepInHour )
{
	if ( m_writeOutputToSQLite ) {
		sqliteBindInteger(m_simulationDataUpdateStmt, 1, numOfTimeStepInHour);
		sqliteBindForeignKey(m_simulationDataUpdateStmt, 2, id);

		sqliteStepCommand(m_simulationDataUpdateStmt);
		sqliteResetCommand(m_simulationDataUpdateStmt);
	}
}

void SQLite::updateSQLiteSimulationRecord( bool const completed, bool const completedSuccessfully, int const id )
{
	if ( m_writeOutputToSQLite ) {
		sqliteBindLogical(m_simulationUpdateStmt, 1, completed);
		sqliteBindLogical(m_simulationUpdateStmt, 2, completedSuccessfully);
		sqliteBindForeignKey(m_simulationUpdateStmt, 3, id); // seems to always be 1, SimulationManager::ManageSimulation()

		sqliteStepCommand(m_simulationUpdateStmt);
		sqliteResetCommand(m_simulationUpdateStmt);
	}
}

void SQLite::createZoneExtendedOutput()
{
	if ( m_writeOutputToSQLite ) {
		for (auto const & zone : zones) {
			zone->insertIntoSQLite( m_zoneInfoInsertStmt );
		}
		for (auto const & zoneList : zoneLists) {
			zoneList->insertIntoSQLite( m_zoneListInsertStmt, m_zoneInfoZoneListInsertStmt );
		}
		for (auto const & zoneGroup : zoneGroups) {
			zoneGroup->insertIntoSQLite( m_zoneGroupInsertStmt );
		}
		for (auto const & schedule : schedules) {
			schedule->insertIntoSQLite( m_scheduleInsertStmt );
		}
		for (auto const & material : materials) {
			material->insertIntoSQLite( m_materialInsertStmt );
		}
		for (auto const & construction : constructions) {
			construction->insertIntoSQLite( m_constructionInsertStmt, m_constructionLayerInsertStmt );
		}
		for (auto const & surface : surfaces) {
			surface->insertIntoSQLite( m_surfaceInsertStmt );
		}
		for (auto const & nominalLighting : nominalLightings) {
			nominalLighting->insertIntoSQLite( m_nominalLightingInsertStmt );
		}
		for (auto const & nominalPeople : nominalPeoples) {
			nominalPeople->insertIntoSQLite( m_nominalPeopleInsertStmt );
		}
		for (auto const & nominalElectricEquipment : nominalElectricEquipments) {
			nominalElectricEquipment->insertIntoSQLite( m_nominalElectricEquipmentInsertStmt );
		}
		for (auto const & nominalGasEquipment : nominalGasEquipments) {
			nominalGasEquipment->insertIntoSQLite( m_nominalGasEquipmentInsertStmt );
		}
		for (auto const & nominalSteamEquipment : nominalSteamEquipments) {
			nominalSteamEquipment->insertIntoSQLite( m_nominalSteamEquipmentInsertStmt );
		}
		for (auto const & nominalHotWaterEquipment : nominalHotWaterEquipments) {
			nominalHotWaterEquipment->insertIntoSQLite( m_nominalHotWaterEquipmentInsertStmt );
		}
		for (auto const & nominalOtherEquipment : nominalOtherEquipments) {
			nominalOtherEquipment->insertIntoSQLite( m_nominalOtherEquipmentInsertStmt );
		}
		for (auto const & nominalBaseboardHeat : nominalBaseboardHeats) {
			nominalBaseboardHeat->insertIntoSQLite( m_nominalBaseboardHeatInsertStmt );
		}
		for (auto const & infiltration : infiltrations ) {
			infiltration->insertIntoSQLite( m_infiltrationInsertStmt );
		}
		for (auto const & ventilation : ventilations) {
			ventilation->insertIntoSQLite( m_ventilationInsertStmt );
		}
		for (auto const & roomAirModel : roomAirModels) {
			roomAirModel->insertIntoSQLite( m_roomAirModelInsertStmt );
		}
	}
}

void SQLite::createSQLiteEnvironmentPeriodRecord( const int curEnvirNum, const std::string& environmentName, const int kindOfSim, const int simulationIndex )
{
	if ( m_writeOutputToSQLite ) {
		sqliteBindInteger(m_environmentPeriodInsertStmt, 1, curEnvirNum);
		sqliteBindForeignKey(m_environmentPeriodInsertStmt, 2, simulationIndex);
		sqliteBindText(m_environmentPeriodInsertStmt, 3, environmentName);
		sqliteBindInteger(m_environmentPeriodInsertStmt, 4, kindOfSim);

		sqliteStepCommand(m_environmentPeriodInsertStmt);
		sqliteResetCommand(m_environmentPeriodInsertStmt);
	}
}

void SQLite::addScheduleData( int const number, std::string const & name, std::string const & type, double const minValue, double const maxValue )
{
	schedules.push_back(
		std::unique_ptr<Schedule>(new Schedule(m_errorStream, m_db, number, name, type, minValue, maxValue))
	);
}

void SQLite::addZoneData( int const number, DataHeatBalance::ZoneData const & zoneData )
{
	zones.push_back(
		std::unique_ptr<Zone>(new Zone(m_errorStream, m_db, number, zoneData))
	);
}

void SQLite::addZoneListData( int const number, DataHeatBalance::ZoneListData const & zoneListData )
{
	zoneLists.push_back(
		std::unique_ptr<ZoneList>(new ZoneList(m_errorStream, m_db, number, zoneListData))
	);
}

void SQLite::addSurfaceData( int const number, DataSurfaces::SurfaceData const & surfaceData, std::string const & surfaceClass )
{
	surfaces.push_back(
		std::unique_ptr<Surface>(new Surface(m_errorStream, m_db, number, surfaceData, surfaceClass))
	);
}

void SQLite::addZoneGroupData( int const number, DataHeatBalance::ZoneGroupData const & zoneGroupData )
{
	zoneGroups.push_back(
		std::unique_ptr<ZoneGroup>(new ZoneGroup(m_errorStream, m_db, number, zoneGroupData))
	);
}

void SQLite::addMaterialData( int const number, DataHeatBalance::MaterialProperties const & materialData )
{
	materials.push_back(
		std::unique_ptr<Material>(new Material(m_errorStream, m_db, number, materialData))
	);
}
void SQLite::addConstructionData( int const number, DataHeatBalance::ConstructionData const & constructionData, double const & constructionUValue )
{
	constructions.push_back(
		std::unique_ptr<Construction>(new Construction(m_errorStream, m_db, number, constructionData, constructionUValue))
	);
}
void SQLite::addNominalLightingData( int const number, DataHeatBalance::LightsData const & nominalLightingData )
{
	nominalLightings.push_back(
		std::unique_ptr<NominalLighting>(new NominalLighting(m_errorStream, m_db, number, nominalLightingData))
	);
}
void SQLite::addNominalPeopleData( int const number, DataHeatBalance::PeopleData const & nominalPeopleData )
{
	nominalPeoples.push_back(
		std::unique_ptr<NominalPeople>(new NominalPeople(m_errorStream, m_db, number, nominalPeopleData))
	);
}
void SQLite::addNominalElectricEquipmentData( int const number, DataHeatBalance::ZoneEquipData const & nominalElectricEquipmentData )
{
	nominalElectricEquipments.push_back(
		std::unique_ptr<NominalElectricEquipment>(new NominalElectricEquipment(m_errorStream, m_db, number, nominalElectricEquipmentData))
	);
}
void SQLite::addNominalGasEquipmentData( int const number, DataHeatBalance::ZoneEquipData const & nominalGasEquipmentData )
{
	nominalGasEquipments.push_back(
		std::unique_ptr<NominalGasEquipment>(new NominalGasEquipment(m_errorStream, m_db, number, nominalGasEquipmentData))
	);
}
void SQLite::addNominalSteamEquipmentData( int const number, DataHeatBalance::ZoneEquipData const & nominalSteamEquipmentData )
{
	nominalSteamEquipments.push_back(
		std::unique_ptr<NominalSteamEquipment>(new NominalSteamEquipment(m_errorStream, m_db, number, nominalSteamEquipmentData))
	);
}
void SQLite::addNominalHotWaterEquipmentData( int const number, DataHeatBalance::ZoneEquipData const & nominalHotWaterEquipmentData )
{
	nominalHotWaterEquipments.push_back(
		std::unique_ptr<NominalHotWaterEquipment>(new NominalHotWaterEquipment(m_errorStream, m_db, number, nominalHotWaterEquipmentData))
	);
}
void SQLite::addNominalOtherEquipmentData( int const number, DataHeatBalance::ZoneEquipData const & nominalOtherEquipmentData )
{
	nominalOtherEquipments.push_back(
		std::unique_ptr<NominalOtherEquipment>(new NominalOtherEquipment(m_errorStream, m_db, number, nominalOtherEquipmentData))
	);
}
void SQLite::addNominalBaseboardData( int const number, DataHeatBalance::BBHeatData const & nominalBaseboardData )
{
	nominalBaseboardHeats.push_back(
		std::unique_ptr<NominalBaseboardHeat>(new NominalBaseboardHeat(m_errorStream, m_db, number, nominalBaseboardData))
	);
}
void SQLite::addInfiltrationData( int const number, DataHeatBalance::InfiltrationData const & infiltrationData )
{
	infiltrations.push_back(
		std::unique_ptr<Infiltration>(new Infiltration(m_errorStream, m_db, number, infiltrationData))
	);
}
void SQLite::addVentilationData( int const number, DataHeatBalance::VentilationData const & ventilationData )
{
	ventilations.push_back(
		std::unique_ptr<Ventilation>(new Ventilation(m_errorStream, m_db, number, ventilationData))
	);
}
void SQLite::addRoomAirModelData( int const number, DataRoomAirModel::AirModelData const & roomAirModelData )
{
	roomAirModels.push_back(
		std::unique_ptr<RoomAirModel>(new RoomAirModel(m_errorStream, m_db, number, roomAirModelData))
	);
}

bool SQLite::ZoneGroup::insertIntoSQLite ( sqlite3_stmt * insertStmt )
{
	sqliteBindInteger(insertStmt, 1, number);
	sqliteBindText(insertStmt, 2, name);
	sqliteBindForeignKey(insertStmt, 3, zoneList);
	sqliteBindInteger(insertStmt, 4, multiplier);

	int rc = sqliteStepCommand(insertStmt);
	bool validInsert = sqliteStepValidity( rc );
	sqliteResetCommand(insertStmt);
	return validInsert;
}
bool SQLite::Material::insertIntoSQLite ( sqlite3_stmt * insertStmt )
{
	sqliteBindInteger(insertStmt, 1, number);
	sqliteBindText(insertStmt, 2, name);
	sqliteBindInteger(insertStmt, 3, group);
	sqliteBindInteger(insertStmt, 4, roughness);
	sqliteBindDouble(insertStmt, 5, conductivity);
	sqliteBindDouble(insertStmt, 6, density);
	sqliteBindDouble(insertStmt, 7, isoMoistCap);
	sqliteBindDouble(insertStmt, 8, porosity);
	sqliteBindDouble(insertStmt, 9, resistance);
	sqliteBindLogical(insertStmt, 10, rOnly);
	sqliteBindDouble(insertStmt, 11, specHeat);
	sqliteBindDouble(insertStmt, 12, thermGradCoef);
	sqliteBindDouble(insertStmt, 13, thickness);
	sqliteBindDouble(insertStmt, 14, vaporDiffus);

	int rc = sqliteStepCommand(insertStmt);
	bool validInsert = sqliteStepValidity( rc );
	sqliteResetCommand(insertStmt);
	return validInsert;
}
bool SQLite::Construction::insertIntoSQLite ( sqlite3_stmt * insertStmt )
{
	sqliteBindInteger(insertStmt, 1, number);
	sqliteBindText(insertStmt, 2, name);
	sqliteBindInteger(insertStmt, 3, totLayers);
	sqliteBindInteger(insertStmt, 4, totSolidLayers);
	sqliteBindInteger(insertStmt, 5, totGlassLayers);
	sqliteBindDouble(insertStmt, 6, insideAbsorpVis);
	sqliteBindDouble(insertStmt, 7, outsideAbsorpVis);
	sqliteBindDouble(insertStmt, 8, insideAbsorpSolar);
	sqliteBindDouble(insertStmt, 9, outsideAbsorpSolar);
	sqliteBindDouble(insertStmt, 10, insideAbsorpThermal);
	sqliteBindDouble(insertStmt, 11, outsideAbsorpThermal);
	sqliteBindInteger(insertStmt, 12, outsideRoughness);
	sqliteBindLogical(insertStmt, 13, typeIsWindow);
	sqliteBindDouble(insertStmt, 14, uValue);

	int rc = sqliteStepCommand(insertStmt);
	bool validInsert = sqliteStepValidity( rc );
	sqliteResetCommand(insertStmt);
	return validInsert;
}
bool SQLite::Construction::insertIntoSQLite ( sqlite3_stmt * insertStmt, sqlite3_stmt * subInsertStmt )
{
	bool constructionInsertValid = insertIntoSQLite( insertStmt );
	if ( !constructionInsertValid ) return false;

	bool valid = true;
	for (auto const & constructionLayer : constructionLayers) {
		bool validInsert = constructionLayer->insertIntoSQLite( subInsertStmt );
		if ( valid && !validInsert ) valid = false;
	}
	return valid;
}
bool SQLite::Construction::ConstructionLayer::insertIntoSQLite( sqlite3_stmt * insertStmt )
{
	sqliteBindForeignKey(insertStmt, 1, constructNumber);
	sqliteBindInteger(insertStmt, 2, layerNumber);
	sqliteBindForeignKey(insertStmt, 3, layerPoint);

	int rc = sqliteStepCommand(insertStmt);
	bool validInsert = sqliteStepValidity( rc );
	sqliteResetCommand(insertStmt);
	return validInsert;
}
bool SQLite::NominalLighting::insertIntoSQLite ( sqlite3_stmt * insertStmt )
{
	sqliteBindInteger(insertStmt, 1, number);
	sqliteBindText(insertStmt, 2, name);
	sqliteBindForeignKey(insertStmt, 3, zonePtr);
	sqliteBindForeignKey(insertStmt, 4, schedulePtr);
	sqliteBindDouble(insertStmt, 5, designLevel);
	sqliteBindDouble(insertStmt, 6, fractionReturnAir);
	sqliteBindDouble(insertStmt, 7, fractionRadiant);
	sqliteBindDouble(insertStmt, 8, fractionShortWave);
	sqliteBindDouble(insertStmt, 9, fractionReplaceable);
	sqliteBindDouble(insertStmt, 10, fractionConvected);
	sqliteBindText(insertStmt, 11, endUseSubcategory);

	int rc = sqliteStepCommand(insertStmt);
	bool validInsert = sqliteStepValidity( rc );
	sqliteResetCommand(insertStmt);
	return validInsert;
}
bool SQLite::NominalPeople::insertIntoSQLite ( sqlite3_stmt * insertStmt )
{
	sqliteBindInteger(insertStmt, 1, number);
	sqliteBindText(insertStmt, 2, name);
	sqliteBindForeignKey(insertStmt, 3, zonePtr);
	sqliteBindDouble(insertStmt, 4, numberOfPeople);
	sqliteBindForeignKey(insertStmt, 5, numberOfPeoplePtr);
	sqliteBindForeignKey(insertStmt, 6, activityLevelPtr);
	sqliteBindDouble(insertStmt, 7, fractionRadiant);
	sqliteBindDouble(insertStmt, 8, fractionConvected);
	sqliteBindForeignKey(insertStmt, 9, workEffPtr);
	sqliteBindForeignKey(insertStmt, 10, clothingPtr);
	sqliteBindForeignKey(insertStmt, 11, airVelocityPtr);
	sqliteBindLogical(insertStmt, 12, fanger);
	sqliteBindLogical(insertStmt, 13, pierce);
	sqliteBindLogical(insertStmt, 14, ksu);
	sqliteBindInteger(insertStmt, 15, mrtCalcType);
	sqliteBindForeignKey(insertStmt, 16, surfacePtr);
	sqliteBindText(insertStmt, 17, angleFactorListName);
	sqliteBindInteger(insertStmt, 18, angleFactorListPtr);
	sqliteBindDouble(insertStmt, 19, userSpecSensFrac);
	sqliteBindLogical(insertStmt, 20, show55Warning);

	int rc = sqliteStepCommand(insertStmt);
	bool validInsert = sqliteStepValidity( rc );
	sqliteResetCommand(insertStmt);
	return validInsert;
}
bool SQLite::NominalElectricEquipment::insertIntoSQLite ( sqlite3_stmt * insertStmt )
{
	sqliteBindInteger(insertStmt, 1, number);
	sqliteBindText(insertStmt, 2, name);
	sqliteBindForeignKey(insertStmt, 3, zonePtr);
	sqliteBindForeignKey(insertStmt, 4, schedulePtr);
	sqliteBindDouble(insertStmt, 5, designLevel);
	sqliteBindDouble(insertStmt, 6, fractionLatent);
	sqliteBindDouble(insertStmt, 7, fractionRadiant);
	sqliteBindDouble(insertStmt, 8, fractionLost);
	sqliteBindDouble(insertStmt, 9, fractionConvected);
	sqliteBindText(insertStmt, 10, endUseSubcategory);

	int rc = sqliteStepCommand(insertStmt);
	bool validInsert = sqliteStepValidity( rc );
	sqliteResetCommand(insertStmt);
	return validInsert;
}
bool SQLite::NominalGasEquipment::insertIntoSQLite ( sqlite3_stmt * insertStmt )
{
	sqliteBindInteger(insertStmt, 1, number);
	sqliteBindText(insertStmt, 2, name);
	sqliteBindForeignKey(insertStmt, 3, zonePtr);
	sqliteBindForeignKey(insertStmt, 4, schedulePtr);
	sqliteBindDouble(insertStmt, 5, designLevel);
	sqliteBindDouble(insertStmt, 6, fractionLatent);
	sqliteBindDouble(insertStmt, 7, fractionRadiant);
	sqliteBindDouble(insertStmt, 8, fractionLost);
	sqliteBindDouble(insertStmt, 9, fractionConvected);
	sqliteBindText(insertStmt, 10, endUseSubcategory);

	int rc = sqliteStepCommand(insertStmt);
	bool validInsert = sqliteStepValidity( rc );
	sqliteResetCommand(insertStmt);
	return validInsert;
}
bool SQLite::NominalSteamEquipment::insertIntoSQLite ( sqlite3_stmt * insertStmt )
{
	sqliteBindInteger(insertStmt, 1, number);
	sqliteBindText(insertStmt, 2, name);
	sqliteBindForeignKey(insertStmt, 3, zonePtr);
	sqliteBindForeignKey(insertStmt, 4, schedulePtr);
	sqliteBindDouble(insertStmt, 5, designLevel);
	sqliteBindDouble(insertStmt, 6, fractionLatent);
	sqliteBindDouble(insertStmt, 7, fractionRadiant);
	sqliteBindDouble(insertStmt, 8, fractionLost);
	sqliteBindDouble(insertStmt, 9, fractionConvected);
	sqliteBindText(insertStmt, 10, endUseSubcategory);

	int rc = sqliteStepCommand(insertStmt);
	bool validInsert = sqliteStepValidity( rc );
	sqliteResetCommand(insertStmt);
	return validInsert;
}
bool SQLite::NominalHotWaterEquipment::insertIntoSQLite ( sqlite3_stmt * insertStmt )
{
	sqliteBindInteger(insertStmt, 1, number);
	sqliteBindText(insertStmt, 2, name);
	sqliteBindForeignKey(insertStmt, 3, zonePtr);
	sqliteBindForeignKey(insertStmt, 4, schedulePtr);
	sqliteBindDouble(insertStmt, 5, designLevel);
	sqliteBindDouble(insertStmt, 6, fractionLatent);
	sqliteBindDouble(insertStmt, 7, fractionRadiant);
	sqliteBindDouble(insertStmt, 8, fractionLost);
	sqliteBindDouble(insertStmt, 9, fractionConvected);
	sqliteBindText(insertStmt, 10, endUseSubcategory);

	int rc = sqliteStepCommand(insertStmt);
	bool validInsert = sqliteStepValidity( rc );
	sqliteResetCommand(insertStmt);
	return validInsert;
}
bool SQLite::NominalOtherEquipment::insertIntoSQLite ( sqlite3_stmt * insertStmt )
{
	sqliteBindInteger(insertStmt, 1, number);
	sqliteBindText(insertStmt, 2, name);
	sqliteBindForeignKey(insertStmt, 3, zonePtr);
	sqliteBindForeignKey(insertStmt, 4, schedulePtr);
	sqliteBindDouble(insertStmt, 5, designLevel);
	sqliteBindDouble(insertStmt, 6, fractionLatent);
	sqliteBindDouble(insertStmt, 7, fractionRadiant);
	sqliteBindDouble(insertStmt, 8, fractionLost);
	sqliteBindDouble(insertStmt, 9, fractionConvected);
	sqliteBindText(insertStmt, 10, endUseSubcategory);

	int rc = sqliteStepCommand(insertStmt);
	bool validInsert = sqliteStepValidity( rc );
	sqliteResetCommand(insertStmt);
	return validInsert;
}
bool SQLite::NominalBaseboardHeat::insertIntoSQLite ( sqlite3_stmt * insertStmt )
{
	sqliteBindInteger(insertStmt, 1, number);
	sqliteBindText(insertStmt, 2, name);
	sqliteBindForeignKey(insertStmt, 3, zonePtr);
	sqliteBindForeignKey(insertStmt, 4, schedPtr);
	sqliteBindDouble(insertStmt, 5, capatLowTemperature);
	sqliteBindDouble(insertStmt, 6, lowTemperature);
	sqliteBindDouble(insertStmt, 7, capatHighTemperature);
	sqliteBindDouble(insertStmt, 8, highTemperature);
	sqliteBindDouble(insertStmt, 9, fractionRadiant);
	sqliteBindDouble(insertStmt, 10, fractionConvected);
	sqliteBindText(insertStmt, 11, endUseSubcategory);

	int rc = sqliteStepCommand(insertStmt);
	bool validInsert = sqliteStepValidity( rc );
	sqliteResetCommand(insertStmt);
	return validInsert;
}
bool SQLite::Infiltration::insertIntoSQLite ( sqlite3_stmt * insertStmt )
{
	sqliteBindInteger(insertStmt, 1, number);
	sqliteBindText(insertStmt, 2, name);
	sqliteBindForeignKey(insertStmt, 3, zonePtr);
	sqliteBindForeignKey(insertStmt, 4, schedPtr);
	sqliteBindDouble(insertStmt, 5, designLevel);

	int rc = sqliteStepCommand(insertStmt);
	bool validInsert = sqliteStepValidity( rc );
	sqliteResetCommand(insertStmt);
	return validInsert;
}
bool SQLite::Ventilation::insertIntoSQLite ( sqlite3_stmt * insertStmt )
{
	sqliteBindInteger(insertStmt, 1, number);
	sqliteBindText(insertStmt, 2, name);
	sqliteBindForeignKey(insertStmt, 3, zonePtr);
	sqliteBindForeignKey(insertStmt, 4, schedPtr);
	sqliteBindDouble(insertStmt, 5, designLevel);

	int rc = sqliteStepCommand(insertStmt);
	bool validInsert = sqliteStepValidity( rc );
	sqliteResetCommand(insertStmt);
	return validInsert;
}
bool SQLite::RoomAirModel::insertIntoSQLite ( sqlite3_stmt * insertStmt )
{
	sqliteBindInteger(insertStmt, 1, number);
	sqliteBindText(insertStmt, 2, airModelName);
	sqliteBindInteger(insertStmt, 3, airModelType);
	sqliteBindInteger(insertStmt, 4, tempCoupleScheme);
	sqliteBindLogical(insertStmt, 5, simAirModel);

	int rc = sqliteStepCommand(insertStmt);
	bool validInsert = sqliteStepValidity( rc );
	sqliteResetCommand(insertStmt);
	return validInsert;
}

bool SQLite::Surface::insertIntoSQLite( sqlite3_stmt * insertStmt )
{
	sqliteBindInteger(insertStmt, 1, number);
	sqliteBindText(insertStmt, 2, name);
	sqliteBindForeignKey(insertStmt, 3, construction);
	sqliteBindText(insertStmt, 4, surfaceClass);
	sqliteBindDouble(insertStmt, 5, area);
	sqliteBindDouble(insertStmt, 6, grossArea);
	sqliteBindDouble(insertStmt, 7, perimeter);
	sqliteBindDouble(insertStmt, 8, azimuth);
	sqliteBindDouble(insertStmt, 9, height);
	sqliteBindDouble(insertStmt, 10, reveal);
	sqliteBindInteger(insertStmt, 11, shape);
	sqliteBindInteger(insertStmt, 12, sides);
	sqliteBindDouble(insertStmt, 13, tilt);
	sqliteBindDouble(insertStmt, 14, width);
	sqliteBindLogical(insertStmt, 15, heatTransSurf);
	sqliteBindForeignKey(insertStmt, 16, baseSurf);
	sqliteBindForeignKey(insertStmt, 17, zone);
	sqliteBindInteger(insertStmt, 18, extBoundCond);
	sqliteBindLogical(insertStmt, 19, extSolar);
	sqliteBindLogical(insertStmt, 20, extWind);

	int rc = sqliteStepCommand(insertStmt);
	bool validInsert = sqliteStepValidity( rc );
	sqliteResetCommand(insertStmt);
	return validInsert;
}

bool SQLite::ZoneList::insertIntoSQLite( sqlite3_stmt * insertStmt )
{
	sqliteBindInteger(insertStmt, 1, number);
	sqliteBindText(insertStmt, 2, name);

	int rc = sqliteStepCommand(insertStmt);
	bool validInsert = sqliteStepValidity( rc );
	sqliteResetCommand(insertStmt);
	return validInsert;
}

bool SQLite::ZoneList::insertIntoSQLite( sqlite3_stmt * insertStmt, sqlite3_stmt * subInsertStmt )
{
	bool zoneListInsertValid = insertIntoSQLite( insertStmt );
	if ( !zoneListInsertValid ) return false;
	bool valid = true;
	for ( size_t i = 1; i <= zones.size(); ++i ) {
		sqliteBindForeignKey(subInsertStmt, 1, number);
		sqliteBindForeignKey(subInsertStmt, 2, zones( i ));
		int rc = sqliteStepCommand(subInsertStmt);
		bool validInsert = sqliteStepValidity( rc );
		sqliteResetCommand(subInsertStmt);
		if ( valid && !validInsert ) valid = false;
	}
	return valid;
}

bool SQLite::Schedule::insertIntoSQLite( sqlite3_stmt * insertStmt )
{
	sqliteBindInteger(insertStmt, 1, number);
	sqliteBindText(insertStmt, 2, name);
	sqliteBindText(insertStmt, 3, type);
	sqliteBindDouble(insertStmt, 4, minValue);
	sqliteBindDouble(insertStmt, 5, maxValue);

	int rc = sqliteStepCommand(insertStmt);
	bool validInsert = sqliteStepValidity( rc );
	sqliteResetCommand(insertStmt);
	return validInsert;
}

bool SQLite::Zone::insertIntoSQLite( sqlite3_stmt * insertStmt )
{
	sqliteBindInteger(insertStmt, 1, number);
	sqliteBindText(insertStmt, 2, name);
	sqliteBindDouble(insertStmt, 3, relNorth);
	sqliteBindDouble(insertStmt, 4, originX);
	sqliteBindDouble(insertStmt, 5, originY);
	sqliteBindDouble(insertStmt, 6, originZ);
	sqliteBindDouble(insertStmt, 7, centroidX);
	sqliteBindDouble(insertStmt, 8, centroidY);
	sqliteBindDouble(insertStmt, 9, centroidZ);
	sqliteBindInteger(insertStmt, 10, ofType);
	sqliteBindInteger(insertStmt, 11, multiplier);
	sqliteBindInteger(insertStmt, 12, listMultiplier);
	sqliteBindDouble(insertStmt, 13, minimumX);
	sqliteBindDouble(insertStmt, 14, maximumX);
	sqliteBindDouble(insertStmt, 15, minimumY);
	sqliteBindDouble(insertStmt, 16, maximumY);
	sqliteBindDouble(insertStmt, 17, minimumZ);
	sqliteBindDouble(insertStmt, 18, maximumZ);
	sqliteBindDouble(insertStmt, 19, ceilingHeight);
	sqliteBindDouble(insertStmt, 20, volume);
	sqliteBindInteger(insertStmt, 21, insideConvectionAlgo);
	sqliteBindInteger(insertStmt, 22, outsideConvectionAlgo);
	sqliteBindDouble(insertStmt, 23, floorArea);
	sqliteBindDouble(insertStmt, 24, extGrossWallArea);
	sqliteBindDouble(insertStmt, 25, extNetWallArea);
	sqliteBindDouble(insertStmt, 26, extWindowArea);
	sqliteBindLogical(insertStmt, 27, isPartOfTotalArea);

	int rc = sqliteStepCommand(insertStmt);
	bool validInsert = sqliteStepValidity( rc );
	sqliteResetCommand(insertStmt);
	return validInsert;
}

SQLite::SQLiteData::SQLiteData( std::shared_ptr<std::ostream> const & errorStream, std::shared_ptr<sqlite3> const & db )
	:
	SQLiteProcedures( errorStream, db )
{}

SQLiteProcedures::SQLiteProcedures( std::shared_ptr<std::ostream> const & errorStream, std::shared_ptr<sqlite3> const & db )
	:
	m_writeOutputToSQLite(true),
	m_errorStream(errorStream),
	m_connection(nullptr),
	m_db(db)
{}

SQLiteProcedures::SQLiteProcedures( std::shared_ptr<std::ostream> const & errorStream, bool writeOutputToSQLite, std::string const & dbName, std::string const & errorFileName )
	:
	m_writeOutputToSQLite(writeOutputToSQLite),
	m_errorStream(errorStream),
	m_connection(nullptr)
{
	if ( m_writeOutputToSQLite ) {
		int rc = -1;
		bool ok = true;

		// Test if we can write to the sqlite error file
		//  Does there need to be a seperate sqlite.err file at all?  Consider using eplusout.err
		if ( m_errorStream ) {
			*m_errorStream << "SQLite3 message, " << errorFileName << " open for processing!" << std::endl;
		} else {
			ok = false;
		}

		// Test if we can create a new file named dbName
		if ( ok && dbName != ":memory:" ) {
			std::ofstream test(dbName, std::ofstream::out | std::ofstream::trunc);
			if ( test.is_open() ) {
				test.close();
			} else {
				ok = false;
			}
		}

		// Test if we can write to the database
		// If we can't then there are probably locks on the database
		if ( ok ) {
			sqlite3_open_v2(dbName.c_str(), &m_connection, SQLITE_OPEN_READWRITE, nullptr);
			char * zErrMsg = nullptr;
			rc = sqlite3_exec(m_connection, "CREATE TABLE Test(x INTEGER PRIMARY KEY)", nullptr, 0, &zErrMsg);
			sqlite3_close(m_connection);
			if ( rc ) {
				*m_errorStream << "SQLite3 message, can't get exclusive lock on existing database: " << sqlite3_errmsg(m_connection) << std::endl;
				ok = false;
			} else {
				if (dbName != ":memory:") {
					// Remove test db
					rc = remove( dbName.c_str() );
					if ( rc ) {
						*m_errorStream << "SQLite3 message, can't remove old database: " << sqlite3_errmsg(m_connection) << std::endl;
						ok = false;
					}
				}
			}
			sqlite3_free(zErrMsg);
		}

		if ( ok ) {
			// Now open the output db for the duration of the simulation
			rc = sqlite3_open_v2(dbName.c_str(), &m_connection, SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE, nullptr);
			m_db = std::shared_ptr<sqlite3>(m_connection, sqlite3_close);
			if ( rc ) {
				*m_errorStream << "SQLite3 message, can't open new database: " << sqlite3_errmsg(m_connection) << std::endl;
				ok = false;
			}
		}

		if ( !ok ) {
			throw std::runtime_error("The SQLite database failed to open.");
		}
	}
}

int SQLiteProcedures::sqliteExecuteCommand(const std::string & commandBuffer)
{
	char *zErrMsg = 0;

	int rc = sqlite3_exec(m_db.get(), commandBuffer.c_str(), NULL, 0, &zErrMsg);
	if ( rc != SQLITE_OK ) {
		*m_errorStream << zErrMsg;
	}
	sqlite3_free(zErrMsg);

	return rc;
}

int SQLiteProcedures::sqlitePrepareStatement(sqlite3_stmt* & stmt, const std::string & stmtBuffer)
{
	int rc = sqlite3_prepare_v2(m_db.get(), stmtBuffer.c_str(), -1, &stmt, nullptr);
	if ( rc != SQLITE_OK ) {
		*m_errorStream << "SQLite3 message, sqlite3_prepare_v2 message: " << stmtBuffer << std::endl;
	}

	return rc;
}

int SQLiteProcedures::sqliteBindText(sqlite3_stmt * stmt, const int stmtInsertLocationIndex, const std::string & textBuffer)
{
	int rc = sqlite3_bind_text(stmt, stmtInsertLocationIndex, textBuffer.c_str(), -1, SQLITE_TRANSIENT);
	if ( rc != SQLITE_OK ) {
		*m_errorStream << "SQLite3 message, sqlite3_bind_text failed: " << textBuffer << std::endl;
	}

	return rc;
}

int SQLiteProcedures::sqliteBindInteger(sqlite3_stmt * stmt, const int stmtInsertLocationIndex, const int intToInsert)
{
	int rc = sqlite3_bind_int(stmt, stmtInsertLocationIndex, intToInsert);
	if ( rc != SQLITE_OK ) {
		*m_errorStream << "SQLite3 message, sqlite3_bind_int failed: " << intToInsert << std::endl;
	}

	return rc;
}

int SQLiteProcedures::sqliteBindDouble(sqlite3_stmt * stmt, const int stmtInsertLocationIndex, const double doubleToInsert)
{
	int rc = sqlite3_bind_double(stmt, stmtInsertLocationIndex, doubleToInsert);
	if ( rc != SQLITE_OK ) {
		*m_errorStream << "SQLite3 message, sqlite3_bind_double failed: " << doubleToInsert << std::endl;
	}

	return rc;
}

int SQLiteProcedures::sqliteBindNULL(sqlite3_stmt * stmt, const int stmtInsertLocationIndex)
{
	int rc = sqlite3_bind_null(stmt, stmtInsertLocationIndex);
	if ( rc != SQLITE_OK ) {
		*m_errorStream << "SQLite3 message, sqlite3_bind_null failed" << std::endl;
	}

	return rc;
}

int SQLiteProcedures::sqliteBindForeignKey(sqlite3_stmt * stmt, const int stmtInsertLocationIndex, const int intToInsert)
{
	int rc = -1;
	if ( intToInsert > 0 ) {
		rc = sqlite3_bind_int(stmt, stmtInsertLocationIndex, intToInsert);
	} else {
		rc = sqlite3_bind_null(stmt, stmtInsertLocationIndex);
	}
	if ( rc != SQLITE_OK ) {
		*m_errorStream << "SQLite3 message, sqliteBindForeignKey failed: " << intToInsert << std::endl;
	}

	return rc;
}

int SQLiteProcedures::sqliteBindLogical(sqlite3_stmt * stmt, const int stmtInsertLocationIndex, const bool valueToInsert)
{
	return sqliteBindInteger(stmt,stmtInsertLocationIndex, valueToInsert ? 1 : 0);
}

bool SQLiteProcedures::sqliteStepValidity( int const rc )
{
	bool isValid = false;
	switch(rc) {
	case SQLITE_DONE:
	case SQLITE_OK:
	case SQLITE_ROW:
		isValid = true;
		break;
	default:
		break;
	}
	return isValid;
}

int SQLiteProcedures::sqliteStepCommand(sqlite3_stmt * stmt)
{
	int rc = sqlite3_step(stmt);
	switch(rc) {
	case SQLITE_DONE:
	case SQLITE_OK:
	case SQLITE_ROW:
		break;
	default:
		*m_errorStream << "SQLite3 message, sqlite3_step message: " << sqlite3_errmsg(m_db.get()) << std::endl;
		break;
	}

	return rc;
}

int SQLiteProcedures::sqliteResetCommand(sqlite3_stmt * stmt)
{
	return sqlite3_reset(stmt);
}

// int SQLiteProcedures::sqliteClearBindings(sqlite3_stmt * stmt)
// {
// 	return sqlite3_clear_bindings(stmt);
// }

// int SQLiteProcedures::sqliteFinalizeCommand(sqlite3_stmt * stmt)
// {
// 	return sqlite3_finalize(stmt);
// }

} // EnergyPlus
