#ifndef SQLiteProcedures_hh_INCLUDED
#define SQLiteProcedures_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1D.hh>
#include <ObjexxFCL/FArray2D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include "DataHeatBalance.hh"
#include "DataRoomAirModel.hh"

#include <sqlite3.h>

#include <fstream>
#include <iostream>
#include <memory>
#include <map>

namespace EnergyPlus {

class SQLiteProcedures
{
protected:
	SQLiteProcedures( std::ostream & errorStream, std::shared_ptr<sqlite3> & db );
	SQLiteProcedures( std::ostream & errorStream, bool writeOutputToSQLite, std::string const & dbName );
	~SQLiteProcedures();

	int sqliteExecuteCommand(const std::string & commandBuffer);
	int sqlitePrepareStatement(sqlite3_stmt* & stmt, const std::string & stmtBuffer);

	int sqliteBindText(sqlite3_stmt * stmt, const int stmtInsertLocationIndex, const std::string & textBuffer);
	int sqliteBindInteger(sqlite3_stmt * stmt, const int stmtInsertLocationIndex, const int intToInsert);
	int sqliteBindDouble(sqlite3_stmt * stmt, const int stmtInsertLocationIndex, const double doubleToInsert);
	int sqliteBindNULL(sqlite3_stmt * stmt, const int stmtInsertLocationIndex);
	int sqliteBindLogical(sqlite3_stmt * stmt, const int stmtInsertLocationIndex, const bool valueToInsert);

	// This assumes a Foreign Key must be greater than 0 to be a valid Foreign Key, otherwise it sets the field to NULL.
	int sqliteBindForeignKey(sqlite3_stmt * stmt, const int stmtInsertLocationIndex, const int intToInsert);

	bool sqliteStepValidity( int const rc );
	int sqliteStepCommand(sqlite3_stmt * stmt);
	int sqliteResetCommand(sqlite3_stmt * stmt);
	int sqliteClearBindings(sqlite3_stmt * stmt);
	int sqliteFinalizeCommand(sqlite3_stmt * stmt);

	bool m_writeOutputToSQLite;
	std::ostream & m_errorStream;
	sqlite3 * m_connection;
	std::shared_ptr<sqlite3> m_db;
	std::string m_dbName;
};

class SQLite : SQLiteProcedures {
public:
	// Friend SQLiteFixture which is the gtest fixture class for testing SQLite
	// This allows for testing of private methods in SQLite
	friend class SQLiteFixture;

	void addScheduleData( int const number, std::string const & name, std::string const & type, double const & minValue, double const & maxValue );
	void addZoneData( int const number, DataHeatBalance::ZoneData const & zoneData );
	void addZoneListData( int const number, DataHeatBalance::ZoneListData const & zoneListData );
	void addSurfaceData( int const number, DataSurfaces::SurfaceData const & surfaceData, std::string const & surfaceClass );
	void addZoneGroupData( int const number, DataHeatBalance::ZoneGroupData const & zoneGroupData );
	void addMaterialData( int const number, DataHeatBalance::MaterialProperties const & materialData );
	void addConstructionData( int const number, DataHeatBalance::ConstructionData const & constructionData );
	void addNominalLightingData( int const number, DataHeatBalance::LightsData const & nominalLightingData );
	void addNominalPeopleData( int const number, DataHeatBalance::PeopleData const & nominalPeopleData );
	void addNominalElectricEquipmentData( int const number, DataHeatBalance::ZoneEquipData const & nominalElectricEquipmentData );
	void addNominalGasEquipmentData( int const number, DataHeatBalance::ZoneEquipData const & nominalGasEquipmentData );
	void addNominalSteamEquipmentData( int const number, DataHeatBalance::ZoneEquipData const & nominalSteamEquipmentData );
	void addNominalHotWaterEquipmentData( int const number, DataHeatBalance::ZoneEquipData const & nominalHotWaterEquipmentData );
	void addNominalOtherEquipmentData( int const number, DataHeatBalance::ZoneEquipData const & nominalOtherEquipmentData );
	void addNominalBaseboardData( int const number, DataHeatBalance::BBHeatData const & nominalBaseboardData );
	void addInfiltrationData( int const number, DataHeatBalance::InfiltrationData const & infiltrationData );
	void addVentilationData( int const number, DataHeatBalance::VentilationData const & ventilationData );
	void addRoomAirModelData( int const number, DataRoomAirModel::AirModelData const & roomAirModelData );

	// class ZoneExtendedOutput {
	// public:
	// 	ZoneExtendedOutput( std::ostream & errorStream, std::shared_ptr<sqlite3> & db );

	// 	void addFromGlobalData();

	// private:
	// 	std::ostream & m_errorStream;
	// 	std::shared_ptr<sqlite3> m_db;

	// 	class Schedule;
	// 	class Zone;

	// 	std::vector< std::unique_ptr<SQLite::ZoneExtendedOutput::Zone> > zones;
	// 	std::vector< std::unique_ptr<SQLite::ZoneExtendedOutput::Schedule> > schedules;

	// 	void addScheduleData( int const & number, std::string const & name, std::string const & type, double const & minValue, double const & maxValue );
	// 	void addZoneData( int const & number, DataHeatBalance::ZoneData const & zoneData );

	// 	class SQLiteData : public SQLiteProcedures
	// 	{
	// 		protected:
	// 			SQLiteData( std::ostream & errorStream, std::shared_ptr<sqlite3> & db );
	// 			virtual bool insertIntoSQLite( sqlite3_stmt * insertStmt ) = 0;
	// 	};

	// 	class Schedule : SQLiteData
	// 	{
	// 		public:
	// 			Schedule( std::ostream & errorStream, std::shared_ptr<sqlite3> & db, int const & scheduleNumber, std::string const & scheduleName, 
	// 					std::string const & scheduleType, double const & scheduleMinValue, double const & scheduleMaxValue ) :
	// 				SQLiteData( errorStream, db ),
	// 				number( scheduleNumber ),
	// 				name( scheduleName ),
	// 				type( scheduleType ),
	// 				minValue( scheduleMinValue ),
	// 				maxValue( scheduleMaxValue )
	// 			{}

	// 		protected:
	// 			virtual bool insertIntoSQLite( sqlite3_stmt * insertStmt );

	// 		private:
	// 			int const & number;
	// 			std::string const & name;
	// 			std::string const & type;
	// 			double const & minValue;
	// 			double const & maxValue;
	// 	};

	// 	class Zone : SQLiteData
	// 	{
	// 		public:
	// 			Zone( std::ostream & errorStream, std::shared_ptr<sqlite3> & db, int const & zoneNumber, DataHeatBalance::ZoneData const & zoneData ) :
	// 				SQLiteData( errorStream, db ),
	// 				number( zoneNumber ),
	// 				name( zoneData.Name ),
	// 				relNorth( zoneData.RelNorth ),
	// 				originX( zoneData.OriginX ),
	// 				originY( zoneData.OriginY ),
	// 				originZ( zoneData.OriginZ ),
	// 				centroidX( zoneData.Centroid.x ),
	// 				centroidY( zoneData.Centroid.y ),
	// 				centroidZ( zoneData.Centroid.z ),
	// 				ofType( zoneData.OfType ),
	// 				multiplier( zoneData.Multiplier ),
	// 				listMultiplier( zoneData.ListMultiplier ),
	// 				minimumX( zoneData.MinimumX ),
	// 				maximumX( zoneData.MaximumX ),
	// 				minimumY( zoneData.MinimumY ),
	// 				maximumY( zoneData.MaximumY ),
	// 				minimumZ( zoneData.MinimumZ ),
	// 				maximumZ( zoneData.MaximumZ ),
	// 				ceilingHeight( zoneData.CeilingHeight ),
	// 				volume( zoneData.Volume ),
	// 				insideConvectionAlgo( zoneData.InsideConvectionAlgo ),
	// 				outsideConvectionAlgo( zoneData.OutsideConvectionAlgo ),
	// 				floorArea( zoneData.FloorArea ),
	// 				extGrossWallArea( zoneData.ExtGrossWallArea ),
	// 				extNetWallArea( zoneData.ExtNetWallArea ),
	// 				extWindowArea( zoneData.ExtWindowArea ),
	// 				isPartOfTotalArea( zoneData.isPartOfTotalArea )
	// 			{}

	// 		protected:
	// 			virtual bool insertIntoSQLite( sqlite3_stmt * insertStmt );

	// 		private:
	// 			int const & number;
	// 			std::string const & name;
	// 			double const & relNorth;
	// 			double const & originX;
	// 			double const & originY;
	// 			double const & originZ;
	// 			double const & centroidX;
	// 			double const & centroidY;
	// 			double const & centroidZ;
	// 			int const & ofType;
	// 			int const & multiplier;
	// 			int const & listMultiplier;
	// 			double const & minimumX;
	// 			double const & maximumX;
	// 			double const & minimumY;
	// 			double const & maximumY;
	// 			double const & minimumZ;
	// 			double const & maximumZ;
	// 			double const & ceilingHeight;
	// 			double const & volume;
	// 			int const & insideConvectionAlgo;
	// 			int const & outsideConvectionAlgo;
	// 			double const & floorArea;
	// 			double const & extGrossWallArea;
	// 			double const & extNetWallArea;
	// 			double const & extWindowArea;
	// 			bool const & isPartOfTotalArea;
	// 	};
	// };

	// class SQLiteData : public SQLiteProcedures
	// {
	// 	protected:
	// 		SQLiteData( std::ostream & errorStream, std::shared_ptr<sqlite3> & db );
	// 		virtual bool insertIntoSQLite( sqlite3_stmt * insertStmt ) = 0;
	// };

	// class Schedule : SQLiteData
	// {
	// 	public:
	// 		Schedule( std::ostream & errorStream, std::shared_ptr<sqlite3> & db, int & scheduleNumber, std::string && scheduleName, 
	// 				std::string && scheduleType, double && scheduleMinValue, double && scheduleMaxValue ) :
	// 			SQLiteData( errorStream, db ),
	// 			number(& scheduleNumber),
	// 			name(& scheduleName),
	// 			type(& scheduleType),
	// 			minValue(& scheduleMinValue),
	// 			maxValue(& scheduleMaxValue)
	// 		{}

	// 	protected:
	// 		virtual bool insertIntoSQLite( sqlite3_stmt * insertStmt );

	// 	private:
	// 		int * const number;
	// 		std::string * const name;
	// 		std::string * const type;
	// 		double * const minValue;
	// 		double * const maxValue;
	// };

	// class Zone : SQLiteData
	// {
	// 	public:
	// 		Zone( std::ostream & errorStream, std::shared_ptr<sqlite3> & db, int & zoneNumber, DataHeatBalance::ZoneData & zoneData ) :
	// 			SQLiteData( errorStream, db ),
	// 			number( & zoneNumber ),
	// 			name( & zoneData.Name ),
	// 			relNorth( & zoneData.RelNorth ),
	// 			originX( & zoneData.OriginX ),
	// 			originY( & zoneData.OriginY ),
	// 			originZ( & zoneData.OriginZ ),
	// 			centroidX( & zoneData.Centroid.x ),
	// 			centroidY( & zoneData.Centroid.y ),
	// 			centroidZ( & zoneData.Centroid.z ),
	// 			ofType( & zoneData.OfType ),
	// 			multiplier( & zoneData.Multiplier ),
	// 			listMultiplier( & zoneData.ListMultiplier ),
	// 			minimumX( & zoneData.MinimumX ),
	// 			maximumX( & zoneData.MaximumX ),
	// 			minimumY( & zoneData.MinimumY ),
	// 			maximumY( & zoneData.MaximumY ),
	// 			minimumZ( & zoneData.MinimumZ ),
	// 			maximumZ( & zoneData.MaximumZ ),
	// 			ceilingHeight( & zoneData.CeilingHeight ),
	// 			volume( & zoneData.Volume ),
	// 			insideConvectionAlgo( & zoneData.InsideConvectionAlgo ),
	// 			outsideConvectionAlgo( & zoneData.OutsideConvectionAlgo ),
	// 			floorArea( & zoneData.FloorArea ),
	// 			extGrossWallArea( & zoneData.ExtGrossWallArea ),
	// 			extNetWallArea( & zoneData.ExtNetWallArea ),
	// 			extWindowArea( & zoneData.ExtWindowArea ),
	// 			isPartOfTotalArea( & zoneData.isPartOfTotalArea )
	// 		{}

	// 	protected:
	// 		virtual bool insertIntoSQLite( sqlite3_stmt * insertStmt );

	// 	private:
	// 		int * const number;
	// 		std::string * const name;
	// 		double * const relNorth;
	// 		double * const originX;
	// 		double * const originY;
	// 		double * const originZ;
	// 		double * const centroidX;
	// 		double * const centroidY;
	// 		double * const centroidZ;
	// 		int * const ofType;
	// 		int * const multiplier;
	// 		int * const listMultiplier;
	// 		double * const minimumX;
	// 		double * const maximumX;
	// 		double * const minimumY;
	// 		double * const maximumY;
	// 		double * const minimumZ;
	// 		double * const maximumZ;
	// 		double * const ceilingHeight;
	// 		double * const volume;
	// 		int * const insideConvectionAlgo;
	// 		int * const outsideConvectionAlgo;
	// 		double * const floorArea;
	// 		double * const extGrossWallArea;
	// 		double * const extNetWallArea;
	// 		double * const extWindowArea;
	// 		bool * const isPartOfTotalArea;
	// };

	// static enum class ZoneExtendedOutput { ZoneTable, ZoneListTable, ZoneGroupTable, SchedulesTable, MaterialsTable, ConstructionTable, 
	// 	SurfacesTable, NominalLightingTable, NominalPeopleTable, NominalElectricEquipmentTable, NominalGasEquipmentTable, 
	// 	NominalHotWaterEquipmentTable, NominalOtherEquipmentTable, NominalBaseboardHeatTable, InfiltrationTable, VentilationTable, 
	// 	RoomAirModelTable };

	// typedef std::tuple< std::vector< std::unique_ptr<SQLite::Zone> >, std::vector< std::unique_ptr<SQLite::Schedule> > > ZoneExtendedOutput;

	// typedef std::tuple< std::map<int, DataHeatBalance::ZoneData>, std::map<int, DataHeatBalance::ZoneList>, std::map<int, DataHeatBalance::ZoneGroup>, 
	// std::map<int, std::tuple<std::string, std::string, double, double> >, std::map<int, DataHeatBalance::Material>, std::map<int, DataHeatBalance::ZoneData>, 
	// std::map<int, DataHeatBalance::ZoneData>, 
	// std::map<int, DataHeatBalance::ZoneData>, std::map<int, DataHeatBalance::ZoneData>, std::map<int, DataHeatBalance::ZoneData>, std::map<int, DataHeatBalance::ZoneData>, 
	// std::map<int, DataHeatBalance::ZoneData>, std::map<int, DataHeatBalance::ZoneData>, std::map<int, DataHeatBalance::ZoneData>, std::map<int, DataHeatBalance::ZoneData>, 
	// std::map<int, DataHeatBalance::ZoneData>, std::map<int, DataHeatBalance::ZoneData>, std::map<int, DataHeatBalance::ZoneData> > ZoneExtendedOutput;

	// Open the DB and prepare for writing data
	// Create all of the tables on construction
	SQLite( std::ostream & errorStream, bool writeOutputToSQLite = false, bool writeTabularDataToSQLite = false, std::string const & dbName = "eplusout.sql" );

	// Close database and free prepared statements
	virtual ~SQLite();

	bool writeOutputToSQLite() const;
	bool writeTabularDataToSQLite() const;

	// Begin a transaction
	void sqliteBegin();

	// Commit a transaction
	void sqliteCommit();

	void createSQLiteReportDictionaryRecord(
		int const reportVariableReportID,
		int const storeTypeIndex,
		std::string const & indexGroup,
		std::string const & keyedValueString,
		std::string const & variableName,
		int const indexType,
		std::string const & units,
		int const reportingFreq,
		bool isMeter,
		Optional_string_const ScheduleName = _
	);

	void createSQLiteReportDataRecord(
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
		int const curEnvirNum,
		Optional_int_const Month = _,
		Optional_int_const DayOfMonth = _,
		Optional_int_const Hour = _,
		Optional< Real64 const > EndMinute = _,
		Optional< Real64 const > StartMinute = _,
		Optional_int_const DST = _,
		Optional_string_const DayType = _,
		bool const warmupFlag = false
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

	void createSQLiteSimulationsRecord( int const ID, const std::string& verString, const std::string& currentDateTime );

	void createSQLiteErrorRecord(
		int const simulationIndex,
		int const errorType,
		std::string const & errorMessage,
		int const cnt
	);

	void updateSQLiteErrorRecord( std::string const & errorMessage );

	void updateSQLiteSimulationRecord( bool const completed, bool const completedSuccessfully, int const id = 1 );

	void updateSQLiteSimulationRecord( int const id, int const numOfTimeStepInHour );

	void createSQLiteEnvironmentPeriodRecord( const int curEnvirNum, const std::string& environmentName, const int kindOfSim, const int simulationIndex = 1 );

	void sqliteWriteMessage(const std::string & message);

	void createZoneExtendedOutput();

	void initializeIndexes();

private:

	void createSQLiteZoneTable( std::map<int, DataHeatBalance::ZoneData> const & zones );
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
	void createSQLiteRoomAirModelTable();

	// int sqliteExecuteCommand(const std::string & commandBuffer);
	// int sqlitePrepareStatement(sqlite3_stmt* & stmt, const std::string & stmtBuffer);

	// int sqliteBindText(sqlite3_stmt * stmt, const int stmtInsertLocationIndex, const std::string & textBuffer);
	// int sqliteBindInteger(sqlite3_stmt * stmt, const int stmtInsertLocationIndex, const int intToInsert);
	// int sqliteBindDouble(sqlite3_stmt * stmt, const int stmtInsertLocationIndex, const double doubleToInsert);
	// int sqliteBindNULL(sqlite3_stmt * stmt, const int stmtInsertLocationIndex);
	// int sqliteBindLogical(sqlite3_stmt * stmt, const int stmtInsertLocationIndex, const bool valueToInsert);

	// // This assumes a Foreign Key must be greater than 0 to be a valid Foreign Key, otherwise it sets the field to NULL.
	// int sqliteBindForeignKey(sqlite3_stmt * stmt, const int stmtInsertLocationIndex, const int intToInsert);

	// int sqliteStepCommand(sqlite3_stmt * stmt);
	// int sqliteResetCommand(sqlite3_stmt * stmt);
	// int sqliteClearBindings(sqlite3_stmt * stmt);
	// int sqliteFinalizeCommand(sqlite3_stmt * stmt);

	int createSQLiteStringTableRecord(std::string const & stringValue, int const stringType);

	static std::string storageType(const int storageTypeIndex);
	static std::string timestepTypeName(const int timestepType);
	static std::string reportingFreqName(const int reportingFreqIndex);

	static void adjustReportingHourAndMinutes(int & hour, int & minutes);
	// Given combinedString, parse out units and description.
	// Example: Given combinedString "Total Energy [GJ]", return "Total Energy"
	// in description and "GJ" in units.
	static void parseUnitsAndDescription(const std::string & combinedString, std::string & units, std::string & description);

	static int logicalToInteger(const bool value);

	void initializeReportDataDictionaryTable();
	void initializeReportDataTables();
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

	// bool m_writeOutputToSQLite;
	bool m_writeTabularDataToSQLite;
	int m_sqlDBTimeIndex;
	// std::ostream & m_errorStream;
	// sqlite3 * m_db;
	// std::string m_dbName;
	sqlite3_stmt * m_reportDataInsertStmt;
	sqlite3_stmt * m_reportExtendedDataInsertStmt;
	sqlite3_stmt * m_reportDictionaryInsertStmt;
	sqlite3_stmt * m_timeIndexInsertStmt;
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
	sqlite3_stmt * m_scheduleInsertStmt;
	sqlite3_stmt * m_daylightMapTitleInsertStmt;
	sqlite3_stmt * m_daylightMapHourlyTitleInsertStmt;
	sqlite3_stmt * m_daylightMapHourlyDataInsertStmt;
	sqlite3_stmt * m_environmentPeriodInsertStmt;
	sqlite3_stmt * m_simulationsInsertStmt;
	sqlite3_stmt * m_tabularDataInsertStmt;
	sqlite3_stmt * m_stringsInsertStmt;
	sqlite3_stmt * m_stringsLookUpStmt;
	sqlite3_stmt * m_errorInsertStmt;
	sqlite3_stmt * m_errorUpdateStmt;
	sqlite3_stmt * m_simulationUpdateStmt;
	sqlite3_stmt * m_simulationDataUpdateStmt;

	static const int LocalReportEach;      //  Write out each time UpdatedataandLocalReport is called
	static const int LocalReportTimeStep;  //  Write out at 'EndTimeStepFlag'
	static const int LocalReportHourly;    //  Write out at 'EndHourFlag'
	static const int LocalReportDaily;     //  Write out at 'EndDayFlag'
	static const int LocalReportMonthly;   //  Write out at end of month (must be determined)
	static const int LocalReportSim;       //  Write out once per environment 'EndEnvrnFlag'
	static const int ReportNameId;
	static const int ReportForStringId;
	static const int TableNameId;
	static const int RowNameId;
	static const int ColumnNameId;
	static const int UnitsId;

	class SQLiteData : public SQLiteProcedures
	{
		protected:
			SQLiteData( std::ostream & errorStream, std::shared_ptr<sqlite3> & db );
			// ~SQLiteData();
			virtual bool insertIntoSQLite( sqlite3_stmt * insertStmt ) = 0;
			// virtual void initializeTable() = 0;
			// sqlite3_stmt * m_insertStmt;
	};

	class Schedule : SQLiteData
	{
		public:
			Schedule( std::ostream & errorStream, std::shared_ptr<sqlite3> & db, int const scheduleNumber, std::string const & scheduleName, 
					std::string const & scheduleType, double const & scheduleMinValue, double const & scheduleMaxValue ) :
				SQLiteData( errorStream, db ),
				number( scheduleNumber ),
				name( scheduleName ),
				type( scheduleType ),
				minValue( scheduleMinValue ),
				maxValue( scheduleMaxValue )
			{}

		// protected:
			virtual bool insertIntoSQLite( sqlite3_stmt * insertStmt );
			// virtual void initializeTable();

		private:
			// sqlite3_stmt * m_insertStmt;
			// std::unique_ptr<sqlite3_stmt> insertStmt(m_insertStmt, sqlite3_finalize);
			// sqlite3 * m_insertStmt;
			// std::unique_ptr<sqlite3_stmt> m_insert;
			int const number;
			std::string const & name;
			std::string const & type;
			double const & minValue;
			double const & maxValue;
	};

	class Surface : SQLiteData
	{
		public:
			Surface( std::ostream & errorStream, std::shared_ptr<sqlite3> & db, int const surfaceNumber, DataSurfaces::SurfaceData const & surfaceData, std::string const & surfaceClass ) :
				SQLiteData( errorStream, db ),
				number( surfaceNumber ),
				name( surfaceData.Name ),
				construction( surfaceData.Construction ),
				surfaceClass( surfaceClass ),
				area( surfaceData.Area ),
				grossArea( surfaceData.GrossArea ),
				perimeter( surfaceData.Perimeter ),
				azimuth( surfaceData.Azimuth ),
				height( surfaceData.Height ),
				reveal( surfaceData.Reveal ),
				shape( surfaceData.Shape ),
				sides( surfaceData.Sides ),
				tilt( surfaceData.Tilt ),
				width( surfaceData.Width ),
				heatTransSurf( surfaceData.HeatTransSurf ),
				baseSurf( surfaceData.BaseSurf ),
				zone( surfaceData.Zone ),
				extBoundCond( surfaceData.ExtBoundCond ),
				extSolar( surfaceData.ExtSolar ),
				extWind( surfaceData.ExtWind )
			{}

		// protected:
			virtual bool insertIntoSQLite( sqlite3_stmt * insertStmt );

		private:
			int const number;
			std::string const & name;
			int const & construction;
			std::string const & surfaceClass;
			double const & area;
			double const & grossArea;
			double const & perimeter;
			double const & azimuth;
			double const & height;
			double const & reveal;
			int const & shape;
			int const & sides;
			double const & tilt;
			double const & width;
			bool const & heatTransSurf;
			int const & baseSurf;
			int const & zone;
			int const & extBoundCond;
			bool const & extSolar;
			bool const & extWind;
	};

	class Zone : SQLiteData
	{
		public:
			Zone( std::ostream & errorStream, std::shared_ptr<sqlite3> & db, int const zoneNumber, DataHeatBalance::ZoneData const & zoneData ) :
				SQLiteData( errorStream, db ),
				number( zoneNumber ),
				name( zoneData.Name ),
				relNorth( zoneData.RelNorth ),
				originX( zoneData.OriginX ),
				originY( zoneData.OriginY ),
				originZ( zoneData.OriginZ ),
				centroidX( zoneData.Centroid.x ),
				centroidY( zoneData.Centroid.y ),
				centroidZ( zoneData.Centroid.z ),
				ofType( zoneData.OfType ),
				multiplier( zoneData.Multiplier ),
				listMultiplier( zoneData.ListMultiplier ),
				minimumX( zoneData.MinimumX ),
				maximumX( zoneData.MaximumX ),
				minimumY( zoneData.MinimumY ),
				maximumY( zoneData.MaximumY ),
				minimumZ( zoneData.MinimumZ ),
				maximumZ( zoneData.MaximumZ ),
				ceilingHeight( zoneData.CeilingHeight ),
				volume( zoneData.Volume ),
				insideConvectionAlgo( zoneData.InsideConvectionAlgo ),
				outsideConvectionAlgo( zoneData.OutsideConvectionAlgo ),
				floorArea( zoneData.FloorArea ),
				extGrossWallArea( zoneData.ExtGrossWallArea ),
				extNetWallArea( zoneData.ExtNetWallArea ),
				extWindowArea( zoneData.ExtWindowArea ),
				isPartOfTotalArea( zoneData.isPartOfTotalArea )
			{}

		// protected:
			virtual bool insertIntoSQLite( sqlite3_stmt * insertStmt );
			// virtual void initializeTable();

		private:
			int const number;
			std::string const & name;
			double const & relNorth;
			double const & originX;
			double const & originY;
			double const & originZ;
			double const & centroidX;
			double const & centroidY;
			double const & centroidZ;
			int const & ofType;
			int const & multiplier;
			int const & listMultiplier;
			double const & minimumX;
			double const & maximumX;
			double const & minimumY;
			double const & maximumY;
			double const & minimumZ;
			double const & maximumZ;
			double const & ceilingHeight;
			double const & volume;
			int const & insideConvectionAlgo;
			int const & outsideConvectionAlgo;
			double const & floorArea;
			double const & extGrossWallArea;
			double const & extNetWallArea;
			double const & extWindowArea;
			bool const & isPartOfTotalArea;
	};

	class ZoneList : SQLiteData
	{
		public:
			ZoneList( std::ostream & errorStream, std::shared_ptr<sqlite3> & db, int const zoneListNumber, DataHeatBalance::ZoneListData const & zoneListData ) :
				SQLiteData( errorStream, db ),
				number( zoneListNumber ),
				name( zoneListData.Name )
				// zoneId( zoneListData.Zone(zoneListNumber) )
			{}

		// protected:
			virtual bool insertIntoSQLite( sqlite3_stmt * insertStmt );
			// virtual void initializeTable();

		private:
			int const number;
			std::string const & name;
			// double const & zoneId;
	};

	class ZoneGroup : SQLiteData
	{
		public:
			ZoneGroup( std::ostream & errorStream, std::shared_ptr<sqlite3> & db, int const zoneGroupNumber, DataHeatBalance::ZoneGroupData const & zoneGroupData ) :
				SQLiteData( errorStream, db ),
				number( zoneGroupNumber ),
				name( zoneGroupData.Name )
			{}

			virtual bool insertIntoSQLite( sqlite3_stmt * insertStmt );

		private:
			int const number;
			std::string const & name;
	};

	class Material : SQLiteData
	{
		public:
			Material( std::ostream & errorStream, std::shared_ptr<sqlite3> & db, int const materialNumber, DataHeatBalance::MaterialProperties const & materialData ) :
				SQLiteData( errorStream, db ),
				number( materialNumber ),
				name( materialData.Name )
			{}

			virtual bool insertIntoSQLite( sqlite3_stmt * insertStmt );

		private:
			int const number;
			std::string const & name;
	};

	class Construction : SQLiteData
	{
		public:
			Construction( std::ostream & errorStream, std::shared_ptr<sqlite3> & db, int const constructionNumber, DataHeatBalance::ConstructionData const & constructionData ) :
				SQLiteData( errorStream, db ),
				number( constructionNumber ),
				name( constructionData.Name )
			{}

			virtual bool insertIntoSQLite( sqlite3_stmt * insertStmt );

		private:
			int const number;
			std::string const & name;
	};

	class NominalLighting : SQLiteData
	{
		public:
			NominalLighting( std::ostream & errorStream, std::shared_ptr<sqlite3> & db, int const nominalLightingNumber, DataHeatBalance::LightsData const & nominalLightingData ) :
				SQLiteData( errorStream, db ),
				number( nominalLightingNumber ),
				name( nominalLightingData.Name )
			{}

			virtual bool insertIntoSQLite( sqlite3_stmt * insertStmt );

		private:
			int const number;
			std::string const & name;
	};

	class NominalPeople : SQLiteData
	{
		public:
			NominalPeople( std::ostream & errorStream, std::shared_ptr<sqlite3> & db, int const nominalPeopleNumber, DataHeatBalance::PeopleData const & nominalPeopleData ) :
				SQLiteData( errorStream, db ),
				number( nominalPeopleNumber ),
				name( nominalPeopleData.Name )
			{}

			virtual bool insertIntoSQLite( sqlite3_stmt * insertStmt );

		private:
			int const number;
			std::string const & name;
	};

	class NominalElectricEquipment : SQLiteData
	{
		public:
			NominalElectricEquipment( std::ostream & errorStream, std::shared_ptr<sqlite3> & db, int const nominalElectricEquipmentNumber, DataHeatBalance::ZoneEquipData const & nominalElectricEquipmentData ) :
				SQLiteData( errorStream, db ),
				number( nominalElectricEquipmentNumber ),
				name( nominalElectricEquipmentData.Name )
			{}

			virtual bool insertIntoSQLite( sqlite3_stmt * insertStmt );

		private:
			int const number;
			std::string const & name;
	};

	class NominalGasEquipment : SQLiteData
	{
		public:
			NominalGasEquipment( std::ostream & errorStream, std::shared_ptr<sqlite3> & db, int const nominalGasEquipmentNumber, DataHeatBalance::ZoneEquipData const & nominalGasEquipmentData ) :
				SQLiteData( errorStream, db ),
				number( nominalGasEquipmentNumber ),
				name( nominalGasEquipmentData.Name )
			{}

			virtual bool insertIntoSQLite( sqlite3_stmt * insertStmt );

		private:
			int const number;
			std::string const & name;
	};

	class NominalSteamEquipment : SQLiteData
	{
		public:
			NominalSteamEquipment( std::ostream & errorStream, std::shared_ptr<sqlite3> & db, int const nominalSteamEquipmentNumber, DataHeatBalance::ZoneEquipData const & nominalSteamEquipmentData ) :
				SQLiteData( errorStream, db ),
				number( nominalSteamEquipmentNumber ),
				name( nominalSteamEquipmentData.Name )
			{}

			virtual bool insertIntoSQLite( sqlite3_stmt * insertStmt );

		private:
			int const number;
			std::string const & name;
	};

	class NominalHotWaterEquipment : SQLiteData
	{
		public:
			NominalHotWaterEquipment( std::ostream & errorStream, std::shared_ptr<sqlite3> & db, int const nominalHotWaterEquipmentNumber, DataHeatBalance::ZoneEquipData const & nominalHotWaterEquipmentData ) :
				SQLiteData( errorStream, db ),
				number( nominalHotWaterEquipmentNumber ),
				name( nominalHotWaterEquipmentData.Name )
			{}

			virtual bool insertIntoSQLite( sqlite3_stmt * insertStmt );

		private:
			int const number;
			std::string const & name;
	};

	class NominalOtherEquipment : SQLiteData
	{
		public:
			NominalOtherEquipment( std::ostream & errorStream, std::shared_ptr<sqlite3> & db, int const nominalOtherEquipmentNumber, DataHeatBalance::ZoneEquipData const & nominalOtherEquipmentData ) :
				SQLiteData( errorStream, db ),
				number( nominalOtherEquipmentNumber ),
				name( nominalOtherEquipmentData.Name )
			{}

			virtual bool insertIntoSQLite( sqlite3_stmt * insertStmt );

		private:
			int const number;
			std::string const & name;
	};

	class NominalBaseboardHeat : SQLiteData
	{
		public:
			NominalBaseboardHeat( std::ostream & errorStream, std::shared_ptr<sqlite3> & db, int const nominalBaseboardHeatNumber, DataHeatBalance::BBHeatData const & nominalBaseboardHeatData ) :
				SQLiteData( errorStream, db ),
				number( nominalBaseboardHeatNumber ),
				name( nominalBaseboardHeatData.Name )
			{}

			virtual bool insertIntoSQLite( sqlite3_stmt * insertStmt );

		private:
			int const number;
			std::string const & name;
	};

	class Infiltration : SQLiteData
	{
		public:
			Infiltration( std::ostream & errorStream, std::shared_ptr<sqlite3> & db, int const infiltrationNumber, DataHeatBalance::InfiltrationData const & infiltrationData ) :
				SQLiteData( errorStream, db ),
				number( infiltrationNumber ),
				name( infiltrationData.Name )
			{}

			virtual bool insertIntoSQLite( sqlite3_stmt * insertStmt );

		private:
			int const number;
			std::string const & name;
	};

	class Ventilation : SQLiteData
	{
		public:
			Ventilation( std::ostream & errorStream, std::shared_ptr<sqlite3> & db, int const ventilationNumber, DataHeatBalance::VentilationData const & ventilationData ) :
				SQLiteData( errorStream, db ),
				number( ventilationNumber ),
				name( ventilationData.Name )
			{}

			virtual bool insertIntoSQLite( sqlite3_stmt * insertStmt );

		private:
			int const number;
			std::string const & name;
	};

	class RoomAirModel : SQLiteData
	{
		public:
			RoomAirModel( std::ostream & errorStream, std::shared_ptr<sqlite3> & db, int const roomAirModelNumber, DataRoomAirModel::AirModelData const & roomAirModelData ) :
				SQLiteData( errorStream, db ),
				number( roomAirModelNumber )
				// name( roomAirModelData.Name )
			{}

			virtual bool insertIntoSQLite( sqlite3_stmt * insertStmt );

		private:
			int const number;
			// std::string const & name;
	};

	std::vector< std::unique_ptr<SQLite::Zone> > zones;
	std::vector< std::unique_ptr<SQLite::ZoneList> > zoneLists;
	std::vector< std::unique_ptr<SQLite::ZoneGroup> > zoneGroups;
	std::vector< std::unique_ptr<SQLite::Schedule> > schedules;
	std::vector< std::unique_ptr<SQLite::Surface> > surfaces;
	std::vector< std::unique_ptr<SQLite::Material> > materials;
	std::vector< std::unique_ptr<SQLite::Construction> > constructions;
	std::vector< std::unique_ptr<SQLite::NominalLighting> > nominalLightings;
	std::vector< std::unique_ptr<SQLite::NominalPeople> > nominalPeoples;
	std::vector< std::unique_ptr<SQLite::NominalElectricEquipment> > nominalElectricEquipments;
	std::vector< std::unique_ptr<SQLite::NominalGasEquipment> > nominalGasEquipments;
	std::vector< std::unique_ptr<SQLite::NominalSteamEquipment> > nominalSteamEquipments;
	std::vector< std::unique_ptr<SQLite::NominalHotWaterEquipment> > nominalHotWaterEquipments;
	std::vector< std::unique_ptr<SQLite::NominalOtherEquipment> > nominalOtherEquipments;
	std::vector< std::unique_ptr<SQLite::NominalBaseboardHeat> > nominalBaseboardHeats;
	std::vector< std::unique_ptr<SQLite::Infiltration> > infiltrations;
	std::vector< std::unique_ptr<SQLite::Ventilation> > ventilations;
	std::vector< std::unique_ptr<SQLite::RoomAirModel> > roomAirModels;
};

extern std::unique_ptr<SQLite> sqlite;

std::unique_ptr<SQLite> CreateSQLiteDatabase();

void CreateSQLiteZoneExtendedOutput();

} // EnergyPlus

#endif
