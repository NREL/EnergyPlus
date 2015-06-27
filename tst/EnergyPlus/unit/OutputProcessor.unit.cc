// EnergyPlus::OutputProcessor Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/SQLiteFixture.hh"
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <ObjexxFCL/gio.hh>

#include <map>

using namespace EnergyPlus;
using namespace EnergyPlus::OutputProcessor;
using namespace ObjexxFCL;
using namespace DataGlobals;

TEST( OutputProcessor, TestGetMeteredVariables )
{
	ShowMessage( "Begin Test: OutputProcessor, TestGetMeteredVariables" );

	int const NumVariables = 2;
	Array1D_int VarIndexes( NumVariables ); // Variable Numbers
	Array1D_int VarTypes( NumVariables ); // Variable Types (1=integer, 2=real, 3=meter)
	Array1D_int IndexTypes( NumVariables ); // Variable Index Types (1=Zone,2=HVAC)
	Array1D_string UnitsStrings( NumVariables ); // UnitsStrings for each variable
	Array1D_int ResourceTypes( NumVariables ); // ResourceTypes for each variable
	Array1D_string EndUses( NumVariables ); // EndUses for each variable
	Array1D_string Groups( NumVariables ); // Groups for each variable
	Array1D_string Names( NumVariables ); // Variable Names for each variable
	Reference< RealVariables > RVar;

	std::string TypeOfComp = "ZONEHVAC:TERMINALUNIT:VARIABLEREFRIGERANTFLOW";
	std::string NameOfComp = "FC-5-1B";

	int NumFound;

	GetMeteredVariables( TypeOfComp, NameOfComp, VarIndexes, VarTypes, IndexTypes, UnitsStrings, ResourceTypes, EndUses, Groups, Names, NumFound );

	EXPECT_EQ( 0, NumFound );

	NumOfRVariable = 2;
	RVariableTypes.allocate( NumOfRVariable );
	NameOfComp = "OUTSIDELIGHTS";
	RVar.allocate();

	RVar().MeterArrayPtr = 1;
	RVariableTypes( 1 ).KeyNameOnlyUC = NameOfComp;
	RVariableTypes( 1 ).VarPtr = RVar;
	VarMeterArrays.allocate( 1 );

	VarMeterArrays( 1 ).NumOnMeters = 1;
	VarMeterArrays( 1 ).OnMeters( 1 ) = 1;

	EnergyMeters.allocate( 10 );
	EnergyMeters( 1 ).ResourceType = NameOfComp;

	GetMeteredVariables( TypeOfComp, NameOfComp, VarIndexes, VarTypes, IndexTypes, UnitsStrings, ResourceTypes, EndUses, Groups, Names, NumFound );
	EXPECT_EQ( 1 , NumFound );

	// Clean up
	RVariableTypes.deallocate();
	RVar.deallocate();
	VarMeterArrays.deallocate();
	EnergyMeters.deallocate();
}

TEST_F( SQLiteFixture, reportTSMeters_PrintESOTimeStamp )
{
	ShowMessage( "Begin Test: OutputProcessor, reportTSMeters_PrintESOTimeStamp" );

	std::unique_ptr<std::ostringstream> eso_strm(new std::ostringstream);
	std::unique_ptr<std::ostringstream> mtr_strm(new std::ostringstream);

	DataGlobals::eso_stream = eso_strm.get();
	DataGlobals::mtr_stream = mtr_strm.get();

	sqlite_test->createSQLiteReportDictionaryRecord( 1, 1, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false, _ );
	sqlite_test->createSQLiteReportDictionaryRecord( 2, 2, "Facility:Electricity", "", "Facility:Electricity", 1, "J", 1, true, _ );

	NumEnergyMeters = 2;
	EnergyMeters.allocate( NumEnergyMeters );
	EnergyMeters( 1 ).CurTSValue = 999.9;
	EnergyMeters( 1 ).TSValue = 999.9;
	EnergyMeters( 1 ).RptTS = true;
	EnergyMeters( 1 ).RptAccTS = false;
	EnergyMeters( 1 ).RptTSFO = false;
	EnergyMeters( 1 ).RptAccTSFO = false;
	EnergyMeters( 1 ).TSRptNum = 1;
	EnergyMeters( 1 ).TSRptNumChr = "1";
	EnergyMeters( 1 ).TSAccRptNum = 1;
	EnergyMeters( 1 ).SMValue = 999.9;

	EnergyMeters( 2 ).CurTSValue = 9999.9;
	EnergyMeters( 2 ).TSValue = 9999.9;
	EnergyMeters( 2 ).RptTS = true;
	EnergyMeters( 2 ).RptAccTS = false;
	EnergyMeters( 2 ).RptTSFO = false;
	EnergyMeters( 2 ).RptAccTSFO = false;
	EnergyMeters( 2 ).TSRptNum = 2;
	EnergyMeters( 2 ).TSRptNumChr = "2";
	EnergyMeters( 2 ).TSAccRptNum = 2;
	EnergyMeters( 2 ).SMValue = 9999.9;

	TimeStepStampReportNbr = 1;
	TimeStepStampReportChr = "1";
	DayOfSim = 1;
	DayOfSimChr = "1";
	HourOfDay = 1;
	DataEnvironment::Month = 12;
	DataEnvironment::DayOfMonth = 21;
	DataEnvironment::DSTIndicator = 0;
	DataEnvironment::DayOfWeek = 2;
	DataEnvironment::HolidayIndex = 3;
	int EndMinute = 10;
	int StartMinute = 0;
	bool PrintESOTimeStamp = true;

	EnergyPlus::sqlite = std::move( sqlite_test );

	ReportTSMeters( StartMinute, EndMinute, PrintESOTimeStamp );

	sqlite_test = std::move( EnergyPlus::sqlite );

	auto result = queryResult("SELECT * FROM Time;", "Time");

	ASSERT_EQ(1ul, result.size());

	std::vector<std::string> testResult0 {"1", "12", "21", "0", "10", "0", "10", "-1", "1", "WinterDesignDay", "0", "0"};
	EXPECT_EQ( testResult0, result[0] );
	EXPECT_EQ( delimitedString( { "1,1,12,21, 0, 1, 0.00,10.00,WinterDesignDay", "1,999.9", "2,9999.9" } ), mtr_strm->str() );
	EXPECT_EQ( delimitedString( { "1,1,12,21, 0, 1, 0.00,10.00,WinterDesignDay", "1,999.9", "2,9999.9" } ), eso_strm->str() );

	auto reportDataResults = queryResult("SELECT * FROM ReportData;", "ReportData");
	auto reportExtendedDataResults = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

	std::vector< std::vector<std::string> > reportData(
	{
		{"1", "1", "1", "999.9"},
		{"2", "1", "2", "9999.9"}
	});

	std::vector< std::vector<std::string> > reportExtendedData({});

	ASSERT_EQ( reportData.size(), reportDataResults.size() );
	for (size_t i = 0; i < reportDataResults.size(); ++i)
	{
		EXPECT_EQ( reportData[i], reportDataResults[i] );
	}

	ASSERT_EQ( reportExtendedData.size(), reportExtendedDataResults.size() );

	EnergyMeters.deallocate();
}

TEST_F( SQLiteFixture, reportTSMeters )
{
	ShowMessage( "Begin Test: OutputProcessor, reportTSMeters" );

	std::unique_ptr<std::ostringstream> eso_strm(new std::ostringstream);
	std::unique_ptr<std::ostringstream> mtr_strm(new std::ostringstream);

	DataGlobals::eso_stream = eso_strm.get();
	DataGlobals::mtr_stream = mtr_strm.get();

	sqlite_test->createSQLiteReportDictionaryRecord( 1, 1, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false, _ );
	sqlite_test->createSQLiteReportDictionaryRecord( 2, 2, "Facility:Electricity", "", "Facility:Electricity", 1, "J", 1, true, _ );

	NumEnergyMeters = 2;
	EnergyMeters.allocate( NumEnergyMeters );
	EnergyMeters( 1 ).CurTSValue = 999.9;
	EnergyMeters( 1 ).TSValue = 999.9;
	EnergyMeters( 1 ).RptTS = true;
	EnergyMeters( 1 ).RptAccTS = false;
	EnergyMeters( 1 ).RptTSFO = false;
	EnergyMeters( 1 ).RptAccTSFO = false;
	EnergyMeters( 1 ).TSRptNum = 1;
	EnergyMeters( 1 ).TSRptNumChr = "1";
	EnergyMeters( 1 ).TSAccRptNum = 1;
	EnergyMeters( 1 ).SMValue = 999.9;

	EnergyMeters( 2 ).CurTSValue = 9999.9;
	EnergyMeters( 2 ).TSValue = 9999.9;
	EnergyMeters( 2 ).RptTS = true;
	EnergyMeters( 2 ).RptAccTS = false;
	EnergyMeters( 2 ).RptTSFO = false;
	EnergyMeters( 2 ).RptAccTSFO = false;
	EnergyMeters( 2 ).TSRptNum = 2;
	EnergyMeters( 2 ).TSRptNumChr = "2";
	EnergyMeters( 2 ).TSAccRptNum = 2;
	EnergyMeters( 2 ).SMValue = 9999.9;

	TimeStepStampReportNbr = 1;
	TimeStepStampReportChr = "1";
	DayOfSim = 1;
	DayOfSimChr = "1";
	HourOfDay = 1;
	DataEnvironment::Month = 12;
	DataEnvironment::DayOfMonth = 21;
	DataEnvironment::DSTIndicator = 0;
	DataEnvironment::DayOfWeek = 2;
	DataEnvironment::HolidayIndex = 3;
	int EndMinute = 10;
	int StartMinute = 0;
	bool PrintESOTimeStamp = false;

	EnergyPlus::sqlite = std::move( sqlite_test );

	ReportTSMeters( StartMinute, EndMinute, PrintESOTimeStamp );

	sqlite_test = std::move( EnergyPlus::sqlite );

	auto result = queryResult("SELECT * FROM Time;", "Time");

	ASSERT_EQ(1ul, result.size());

	std::vector<std::string> testResult0 {"1", "12", "21", "0", "10", "0", "10", "-1", "1", "WinterDesignDay", "0", "0"};
	EXPECT_EQ( testResult0, result[0] );
	EXPECT_EQ( delimitedString( { "1,1,12,21, 0, 1, 0.00,10.00,WinterDesignDay", "1,999.9", "2,9999.9" } ), mtr_strm->str() );
	EXPECT_EQ( delimitedString( { "1,999.9", "2,9999.9" } ), eso_strm->str() );

	auto reportDataResults = queryResult("SELECT * FROM ReportData;", "ReportData");
	auto reportExtendedDataResults = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

	std::vector< std::vector<std::string> > reportData(
	{
		{"1", "1", "1", "999.9"},
		{"2", "1", "2", "9999.9"}
	});

	std::vector< std::vector<std::string> > reportExtendedData({});

	ASSERT_EQ( reportData.size(), reportDataResults.size() );
	for (size_t i = 0; i < reportDataResults.size(); ++i)
	{
		EXPECT_EQ( reportData[i], reportDataResults[i] );
	}

	ASSERT_EQ( reportExtendedData.size(), reportExtendedDataResults.size() );

	EnergyMeters.deallocate();
}

TEST_F( SQLiteFixture, reportHRMeters )
{
	ShowMessage( "Begin Test: OutputProcessor, reportHRMeters" );

	std::unique_ptr<std::ostringstream> mtr_strm(new std::ostringstream);

	DataGlobals::mtr_stream = mtr_strm.get();

	sqlite_test->createSQLiteReportDictionaryRecord( 1, 1, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false, _ );
	sqlite_test->createSQLiteReportDictionaryRecord( 2, 2, "Facility:Electricity", "", "Facility:Electricity", 1, "J", 1, true, _ );

	NumEnergyMeters = 2;
	EnergyMeters.allocate( NumEnergyMeters );
	EnergyMeters( 1 ).RptHR = true;
	EnergyMeters( 1 ).RptHRFO = true;
	EnergyMeters( 1 ).RptAccHR = false;
	EnergyMeters( 1 ).RptAccHRFO = false;
	EnergyMeters( 1 ).HRRptNum = 1;
	EnergyMeters( 1 ).HRRptNumChr = "1";
	EnergyMeters( 1 ).HRValue = 999.9;
	EnergyMeters( 1 ).HRAccRptNum = 1;
	EnergyMeters( 1 ).SMValue = 999.9;

	EnergyMeters( 2 ).RptHR = true;
	EnergyMeters( 2 ).RptHRFO = true;
	EnergyMeters( 2 ).RptAccHR = false;
	EnergyMeters( 2 ).RptAccHRFO = false;
	EnergyMeters( 2 ).HRRptNum = 2;
	EnergyMeters( 2 ).HRRptNumChr = "2";
	EnergyMeters( 2 ).HRValue = 9999.9;
	EnergyMeters( 2 ).HRAccRptNum = 2;
	EnergyMeters( 2 ).SMValue = 9999.9;

	TimeStepStampReportNbr = 1;
	TimeStepStampReportChr = "1";
	DayOfSim = 1;
	DayOfSimChr = "1";
	HourOfDay = 1;
	DataEnvironment::Month = 12;
	DataEnvironment::DayOfMonth = 21;
	DataEnvironment::DSTIndicator = 0;
	DataEnvironment::DayOfWeek = 2;
	DataEnvironment::HolidayIndex = 3;

	EnergyPlus::sqlite = std::move( sqlite_test );

	ReportHRMeters();

	sqlite_test = std::move( EnergyPlus::sqlite );

	auto result = queryResult("SELECT * FROM Time;", "Time");

	ASSERT_EQ(1ul, result.size());

	std::vector<std::string> testResult0 {"1", "12", "21", "1", "0", "0", "60", "1", "1", "WinterDesignDay", "0", ""};
	EXPECT_EQ( testResult0, result[0] );
	EXPECT_EQ( delimitedString( { "1,1,12,21, 0, 1, 0.00,60.00,WinterDesignDay", "1,999.9", "2,9999.9" } ), mtr_strm->str() );

	auto reportDataResults = queryResult("SELECT * FROM ReportData;", "ReportData");
	auto reportExtendedDataResults = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

	std::vector< std::vector<std::string> > reportData(
	{
		{"1", "1", "1", "999.9"},
		{"2", "1", "2", "9999.9"}
	});

	std::vector< std::vector<std::string> > reportExtendedData({});

	ASSERT_EQ( reportData.size(), reportDataResults.size() );
	for (size_t i = 0; i < reportDataResults.size(); ++i)
	{
		EXPECT_EQ( reportData[i], reportDataResults[i] );
	}

	ASSERT_EQ( reportExtendedData.size(), reportExtendedDataResults.size() );

	EnergyMeters.deallocate();
}

TEST_F( SQLiteFixture, reportDYMeters )
{
	ShowMessage( "Begin Test: OutputProcessor, reportDYMeters" );

	std::unique_ptr<std::ostringstream> mtr_strm(new std::ostringstream);
	std::unique_ptr<std::ostringstream> compare_text(new std::ostringstream);

	DataGlobals::mtr_stream = mtr_strm.get();

	sqlite_test->createSQLiteReportDictionaryRecord( 1, 1, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false, _ );
	sqlite_test->createSQLiteReportDictionaryRecord( 2, 2, "Facility:Electricity", "", "Facility:Electricity", 1, "J", 1, true, _ );

	NumEnergyMeters = 2;
	EnergyMeters.allocate( NumEnergyMeters );
	EnergyMeters( 1 ).RptDY = true;
	EnergyMeters( 1 ).RptDYFO = true;
	EnergyMeters( 1 ).RptAccDY = false;
	EnergyMeters( 1 ).RptAccDYFO = false;
	EnergyMeters( 1 ).DYRptNum = 1;
	EnergyMeters( 1 ).DYRptNumChr = "1";
	EnergyMeters( 1 ).DYValue = 999.9;
	EnergyMeters( 1 ).DYAccRptNum = 1;
	EnergyMeters( 1 ).SMValue = 999.9;
	EnergyMeters( 1 ).DYMaxVal = 4283136.2524843821;
	EnergyMeters( 1 ).DYMaxValDate = 12210160;
	EnergyMeters( 1 ).DYMinVal = 4283136.2516839253;
	EnergyMeters( 1 ).DYMinValDate = 12210110;

	EnergyMeters( 2 ).RptDY = true;
	EnergyMeters( 2 ).RptDYFO = true;
	EnergyMeters( 2 ).RptAccDY = false;
	EnergyMeters( 2 ).RptAccDYFO = false;
	EnergyMeters( 2 ).DYRptNum = 2;
	EnergyMeters( 2 ).DYRptNumChr = "2";
	EnergyMeters( 2 ).DYValue = 9999.9;
	EnergyMeters( 2 ).DYAccRptNum = 2;
	EnergyMeters( 2 ).SMValue = 9999.9;
	EnergyMeters( 2 ).DYMaxVal = 4283136.2524843821;
	EnergyMeters( 2 ).DYMaxValDate = 12210160;
	EnergyMeters( 2 ).DYMinVal = 4283136.2516839253;
	EnergyMeters( 2 ).DYMinValDate = 12210110;

	DailyStampReportNbr = 1;
	DailyStampReportChr = "1";
	DayOfSim = 1;
	DayOfSimChr = "1";
	HourOfDay = 1;
	DataEnvironment::Month = 12;
	DataEnvironment::DayOfMonth = 21;
	DataEnvironment::DSTIndicator = 0;
	DataEnvironment::DayOfWeek = 2;
	DataEnvironment::HolidayIndex = 3;

	EnergyPlus::sqlite = std::move( sqlite_test );

	ReportDYMeters();

	sqlite_test = std::move( EnergyPlus::sqlite );

	auto result = queryResult("SELECT * FROM Time;", "Time");

	ASSERT_EQ(1ul, result.size());

	std::vector<std::string> testResult0 {"1", "12", "21", "24", "0", "0", "1440", "2", "1", "WinterDesignDay", "0", ""};
	EXPECT_EQ( testResult0, result[0] );
	EXPECT_EQ( delimitedString( { "1,1,12,21, 0,WinterDesignDay", "1,999.9,4283136.25168393, 1,10,4283136.25248438, 1,60", "2,9999.9,4283136.25168393, 1,10,4283136.25248438, 1,60" } ), mtr_strm->str() );

	auto reportDataResults = queryResult("SELECT * FROM ReportData;", "ReportData");
	auto reportExtendedDataResults = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

	std::vector< std::vector<std::string> > reportData(
	{
		{"1", "1", "1", "999.9"},
		{"2", "1", "2", "9999.9"}
	});

	std::vector< std::vector<std::string> > reportExtendedData(
	{
		{"1","1","4283136.25248438","12","21","1","1","0","4283136.25168393","12","21","0","11","10"},
		{"2","2","4283136.25248438","12","21","1","1","0","4283136.25168393","12","21","0","11","10"}
	});

	ASSERT_EQ( reportData.size(), reportDataResults.size() );
	for (size_t i = 0; i < reportDataResults.size(); ++i)
	{
		EXPECT_EQ( reportData[i], reportDataResults[i] );
	}

	ASSERT_EQ( reportExtendedData.size(), reportExtendedDataResults.size() );
	for (size_t i = 0; i < reportExtendedDataResults.size(); ++i)
	{
		EXPECT_EQ( reportExtendedData[i], reportExtendedDataResults[i] );
	}

	EnergyMeters.deallocate();
}

TEST_F( SQLiteFixture, reportMNMeters )
{
	ShowMessage( "Begin Test: OutputProcessor, reportMNMeters" );

	std::unique_ptr<std::ostringstream> mtr_strm(new std::ostringstream);

	DataGlobals::mtr_stream = mtr_strm.get();

	sqlite_test->createSQLiteReportDictionaryRecord( 1, 1, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false, _ );
	sqlite_test->createSQLiteReportDictionaryRecord( 2, 2, "Facility:Electricity", "", "Facility:Electricity", 1, "J", 1, true, _ );

	NumEnergyMeters = 2;
	EnergyMeters.allocate( NumEnergyMeters );
	EnergyMeters( 1 ).RptMN = true;
	EnergyMeters( 1 ).RptMNFO = true;
	EnergyMeters( 1 ).RptAccMN = false;
	EnergyMeters( 1 ).RptAccMNFO = false;
	EnergyMeters( 1 ).MNRptNum = 1;
	EnergyMeters( 1 ).MNRptNumChr = "1";
	EnergyMeters( 1 ).MNValue = 999.9;
	EnergyMeters( 1 ).MNAccRptNum = 1;
	EnergyMeters( 1 ).SMValue = 999.9;
	EnergyMeters( 1 ).MNMaxVal = 4283136.2524843821;
	EnergyMeters( 1 ).MNMaxValDate = 12210160;
	EnergyMeters( 1 ).MNMinVal = 4283136.2516839253;
	EnergyMeters( 1 ).MNMinValDate = 12210110;

	EnergyMeters( 2 ).RptMN = true;
	EnergyMeters( 2 ).RptMNFO = true;
	EnergyMeters( 2 ).RptAccMN = false;
	EnergyMeters( 2 ).RptAccMNFO = false;
	EnergyMeters( 2 ).MNRptNum = 2;
	EnergyMeters( 2 ).MNRptNumChr = "2";
	EnergyMeters( 2 ).MNValue = 9999.9;
	EnergyMeters( 2 ).MNAccRptNum = 2;
	EnergyMeters( 2 ).SMValue = 9999.9;
	EnergyMeters( 2 ).MNMaxVal = 4283136.2524843821;
	EnergyMeters( 2 ).MNMaxValDate = 12210160;
	EnergyMeters( 2 ).MNMinVal = 4283136.2516839253;
	EnergyMeters( 2 ).MNMinValDate = 12210110;

	MonthlyStampReportNbr = 1;
	MonthlyStampReportChr = "1";
	DayOfSim = 1;
	DayOfSimChr = "1";
	HourOfDay = 1;
	DataEnvironment::Month = 12;
	DataEnvironment::DayOfMonth = 21;
	DataEnvironment::DSTIndicator = 0;
	DataEnvironment::DayOfWeek = 2;
	DataEnvironment::HolidayIndex = 3;

	EnergyPlus::sqlite = std::move( sqlite_test );

	ReportMNMeters();

	sqlite_test = std::move( EnergyPlus::sqlite );

	auto result = queryResult("SELECT * FROM Time;", "Time");

	ASSERT_EQ(1ul, result.size());

	std::vector<std::string> testResult0 {"1", "12", "31", "24", "0", "", "44640", "3", "1", "", "0", ""};
	EXPECT_EQ( testResult0, result[0] );
	EXPECT_EQ( delimitedString( { "1,1,12", "1,999.9,4283136.25168393,21, 1,10,4283136.25248438,21, 1,60", "2,9999.9,4283136.25168393,21, 1,10,4283136.25248438,21, 1,60" } ), mtr_strm->str() );

	auto reportDataResults = queryResult("SELECT * FROM ReportData;", "ReportData");
	auto reportExtendedDataResults = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

	std::vector< std::vector<std::string> > reportData(
	{
		{"1", "1", "1", "999.9"},
		{"2", "1", "2", "9999.9"}
	});

	std::vector< std::vector<std::string> > reportExtendedData(
	{
		{"1","1","4283136.25248438","12","21","1","1","0","4283136.25168393","12","21","0","11","10"},
		{"2","2","4283136.25248438","12","21","1","1","0","4283136.25168393","12","21","0","11","10"}
	});

	ASSERT_EQ( reportData.size(), reportDataResults.size() );
	for (size_t i = 0; i < reportDataResults.size(); ++i)
	{
		EXPECT_EQ( reportData[i], reportDataResults[i] );
	}

	ASSERT_EQ( reportExtendedData.size(), reportExtendedDataResults.size() );
	for (size_t i = 0; i < reportExtendedDataResults.size(); ++i)
	{
		EXPECT_EQ( reportExtendedData[i], reportExtendedDataResults[i] );
	}

	EnergyMeters.deallocate();
}

TEST_F( SQLiteFixture, reportSMMeters )
{
	ShowMessage( "Begin Test: OutputProcessor, reportSMMeters" );

	std::unique_ptr<std::ostringstream> mtr_strm(new std::ostringstream);

	DataGlobals::mtr_stream = mtr_strm.get();

	sqlite_test->createSQLiteReportDictionaryRecord( 1, 1, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false, _ );
	sqlite_test->createSQLiteReportDictionaryRecord( 2, 2, "Facility:Electricity", "", "Facility:Electricity", 1, "J", 1, true, _ );

	NumEnergyMeters = 2;
	EnergyMeters.allocate( NumEnergyMeters );
	EnergyMeters( 1 ).RptSM = true;
	EnergyMeters( 1 ).RptSMFO = true;
	EnergyMeters( 1 ).RptAccSM = false;
	EnergyMeters( 1 ).RptAccSMFO = false;
	EnergyMeters( 1 ).SMRptNum = 1;
	EnergyMeters( 1 ).SMRptNumChr = "1";
	EnergyMeters( 1 ).SMValue = 999.9;
	EnergyMeters( 1 ).SMAccRptNum = 1;
	EnergyMeters( 1 ).SMValue = 999.9;
	EnergyMeters( 1 ).SMMaxVal = 4283136.2524843821;
	EnergyMeters( 1 ).SMMaxValDate = 12210160;
	EnergyMeters( 1 ).SMMinVal = 4283136.2516839253;
	EnergyMeters( 1 ).SMMinValDate = 12210110;

	EnergyMeters( 2 ).RptSM = true;
	EnergyMeters( 2 ).RptSMFO = true;
	EnergyMeters( 2 ).RptAccSM = false;
	EnergyMeters( 2 ).RptAccSMFO = false;
	EnergyMeters( 2 ).SMRptNum = 2;
	EnergyMeters( 2 ).SMRptNumChr = "2";
	EnergyMeters( 2 ).SMValue = 9999.9;
	EnergyMeters( 2 ).SMAccRptNum = 2;
	EnergyMeters( 2 ).SMValue = 9999.9;
	EnergyMeters( 2 ).SMMaxVal = 4283136.2524843821;
	EnergyMeters( 2 ).SMMaxValDate = 12210160;
	EnergyMeters( 2 ).SMMinVal = 4283136.2516839253;
	EnergyMeters( 2 ).SMMinValDate = 12210110;

	RunPeriodStampReportNbr = 1;
	RunPeriodStampReportChr = "1";
	DayOfSim = 1;
	DayOfSimChr = "1";
	HourOfDay = 1;
	DataEnvironment::Month = 12;
	DataEnvironment::DayOfMonth = 21;
	DataEnvironment::DSTIndicator = 0;
	DataEnvironment::DayOfWeek = 2;
	DataEnvironment::HolidayIndex = 3;

	EnergyPlus::sqlite = std::move( sqlite_test );

	ReportSMMeters();

	sqlite_test = std::move( EnergyPlus::sqlite );

	auto result = queryResult("SELECT * FROM Time;", "Time");

	ASSERT_EQ(1ul, result.size());

	std::vector<std::string> testResult0 {"1", "", "", "", "", "", "1440", "4", "1", "", "0", ""};
	EXPECT_EQ( testResult0, result[0] );
	EXPECT_EQ( delimitedString( { "1,1", "1,999.9,4283136.25168393,12,21, 1,10,4283136.25248438,12,21, 1,60", "2,9999.9,4283136.25168393,12,21, 1,10,4283136.25248438,12,21, 1,60" } ), mtr_strm->str() );

	auto reportDataResults = queryResult("SELECT * FROM ReportData;", "ReportData");
	auto reportExtendedDataResults = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

	std::vector< std::vector<std::string> > reportData(
	{
		{"1", "1", "1", "999.9"},
		{"2", "1", "2", "9999.9"}
	});

	std::vector< std::vector<std::string> > reportExtendedData(
	{
		{"1","1","4283136.25248438","12","21","1","1","0","4283136.25168393","12","21","0","11","10"},
		{"2","2","4283136.25248438","12","21","1","1","0","4283136.25168393","12","21","0","11","10"}
	});

	ASSERT_EQ( reportData.size(), reportDataResults.size() );
	for (size_t i = 0; i < reportDataResults.size(); ++i)
	{
		EXPECT_EQ( reportData[i], reportDataResults[i] );
	}

	ASSERT_EQ( reportExtendedData.size(), reportExtendedDataResults.size() );
	for (size_t i = 0; i < reportExtendedDataResults.size(); ++i)
	{
		EXPECT_EQ( reportExtendedData[i], reportExtendedDataResults[i] );
	}

	EnergyMeters.deallocate();
}

TEST_F( SQLiteFixture, writeTimeStampFormatData )
{
	ShowMessage( "Begin Test: OutputProcessor, writeTimeStampFormatData" );

	std::unique_ptr<std::ostringstream> mtr_stream(new std::ostringstream);

	int TimeStepStampReportNbr = 1;
	std::string TimeStepStampReportChr = "1";

	int DailyStampReportNbr = 1;
	std::string DailyStampReportChr = "1";

	int MonthlyStampReportNbr = 1;
	std::string MonthlyStampReportChr = "1";

	int RunPeriodStampReportNbr = 1;
	std::string RunPeriodStampReportChr = "1";

	int DayOfSim = 1;
	std::string DayOfSimChr = "1";
	bool PrintTimeStamp = true;
	int Month = 12;
	int DayOfMonth = 21;
	int HourOfDay = 1;
	int EndMinute = 10;
	int StartMinute = 0;
	int DSTIndicator = 0;
	int CurDayType = 10;

	EnergyPlus::sqlite = std::move( sqlite_test );

	// TSMeter
	WriteTimeStampFormatData( mtr_stream.get(), ReportTimeStep, TimeStepStampReportNbr, TimeStepStampReportChr, DayOfSim, 
		DayOfSimChr, PrintTimeStamp, Month, DayOfMonth, HourOfDay, EndMinute, 
		StartMinute, DSTIndicator, DayTypes( CurDayType ) );

	EXPECT_EQ( delimitedString( { "1,1,12,21, 0, 1, 0.00,10.00,WinterDesignDay" } ), mtr_stream->str() );
	mtr_stream->str(std::string());

	// TSMeter
	WriteTimeStampFormatData( mtr_stream.get(), ReportEach, TimeStepStampReportNbr, TimeStepStampReportChr, DayOfSim, 
		DayOfSimChr, PrintTimeStamp, Month, DayOfMonth, HourOfDay, EndMinute, 
		StartMinute, DSTIndicator, DayTypes( CurDayType ) );

	EXPECT_EQ( delimitedString( { "1,1,12,21, 0, 1, 0.00,10.00,WinterDesignDay" } ), mtr_stream->str() );
	mtr_stream->str(std::string());

	// HRMeter
	WriteTimeStampFormatData( mtr_stream.get(), ReportHourly, TimeStepStampReportNbr, TimeStepStampReportChr, DayOfSim, 
		DayOfSimChr, PrintTimeStamp, Month, DayOfMonth, HourOfDay, _, _, DSTIndicator, DayTypes( CurDayType ) );

	EXPECT_EQ( delimitedString( { "1,1,12,21, 0, 1, 0.00,60.00,WinterDesignDay" } ), mtr_stream->str() );
	mtr_stream->str(std::string());

	// DYMeter
	WriteTimeStampFormatData( mtr_stream.get(), ReportDaily, DailyStampReportNbr, DailyStampReportChr, DayOfSim, DayOfSimChr, 
		PrintTimeStamp, Month, DayOfMonth, _, _, _, DSTIndicator, DayTypes( CurDayType ) );

	EXPECT_EQ( delimitedString( { "1,1,12,21, 0,WinterDesignDay" } ), mtr_stream->str() );
	mtr_stream->str(std::string());

	// MNMeter
	WriteTimeStampFormatData( mtr_stream.get(), ReportMonthly, MonthlyStampReportNbr, MonthlyStampReportChr, DayOfSim, DayOfSimChr, 
		PrintTimeStamp, Month );

	EXPECT_EQ( delimitedString( { "1,1,12" } ), mtr_stream->str() );
	mtr_stream->str(std::string());

	// SMMeter
	WriteTimeStampFormatData( mtr_stream.get(), ReportSim, RunPeriodStampReportNbr, RunPeriodStampReportChr, DayOfSim, DayOfSimChr, PrintTimeStamp );

	EXPECT_EQ( delimitedString( { "1,1" } ), mtr_stream->str() );
	mtr_stream->str(std::string());

	// Bad input
	WriteTimeStampFormatData( mtr_stream.get(), 999, RunPeriodStampReportNbr, RunPeriodStampReportChr, DayOfSim, DayOfSimChr, PrintTimeStamp );

	sqlite_test = std::move( EnergyPlus::sqlite );

	EXPECT_EQ("SQLite3 message, Illegal reportingInterval passed to WriteTimeStampFormatData: 999\n", ss->str());
	ss->str(std::string());

	auto timeResults = queryResult("SELECT * FROM Time;", "Time");

	std::vector< std::vector<std::string> > timeData(
	{
		{"1", "12", "21", "0", "10", "0", "10", "0", "1", "WinterDesignDay", "0", "0"},
		{"2", "12", "21", "0", "10", "0", "10", "-1", "1", "WinterDesignDay", "0", "0"},
		{"3", "12", "21", "1", "0", "0", "60", "1", "1", "WinterDesignDay", "0", "0"},
		{"4", "12", "21", "24", "0", "0", "1440", "2", "1", "WinterDesignDay", "0", "0"},
		{"5", "12", "31", "24", "0", "", "44640", "3", "1", "", "0", "0"},
		{"6", "", "", "", "", "", "1440", "4", "1", "", "0", "0"}
	});

	ASSERT_EQ( timeData.size(), timeResults.size() );
	for (size_t i = 0; i < timeResults.size(); ++i)
	{
		EXPECT_EQ( timeData[i], timeResults[i] );
	}

}

TEST_F( SQLiteFixture, writeReportMeterData )
{
	ShowMessage( "Begin Test: OutputProcessor, writeReportMeterData" );

	std::unique_ptr<std::ostringstream> eso_strm(new std::ostringstream);
	std::unique_ptr<std::ostringstream> mtr_strm(new std::ostringstream);

	DataGlobals::eso_stream = eso_strm.get();
	DataGlobals::mtr_stream = mtr_strm.get();

	DataGlobals::MinutesPerTimeStep = 10;

	sqlite_test->createSQLiteTimeIndexRecord( 4, 1, 1, 0 );
	sqlite_test->createSQLiteReportDictionaryRecord( 1, 1, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false, _ );

	EnergyPlus::sqlite = std::move( sqlite_test );

	WriteReportMeterData( 1, "1", 999.9, ReportTimeStep, 0.0, 0, 0.0, 0, false );
	EXPECT_EQ( delimitedString( { "1,999.9" } ), eso_strm->str() );
	EXPECT_EQ( delimitedString( { "1,999.9" } ), mtr_strm->str() );
	mtr_strm->str(std::string());
	eso_strm->str(std::string());

	WriteReportMeterData( 1, "1", 999.9, ReportEach, 0.0, 0, 0.0, 0, false );
	EXPECT_EQ( delimitedString( { "1,999.9" } ), eso_strm->str() );
	EXPECT_EQ( delimitedString( { "1,999.9" } ), mtr_strm->str() );
	mtr_strm->str(std::string());
	eso_strm->str(std::string());

	WriteReportMeterData( 1, "1", 616771620.98702729, ReportHourly, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460, false );
	EXPECT_EQ( delimitedString( { "1,616771620.987027" } ), eso_strm->str() );
	EXPECT_EQ( delimitedString( { "1,616771620.987027" } ), mtr_strm->str() );
	mtr_strm->str(std::string());
	eso_strm->str(std::string());

	WriteReportMeterData( 1, "1", 616771620.98702729, ReportDaily, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460, false );
	EXPECT_EQ( delimitedString( { "1,616771620.987027,4283136.25168393, 1,10,4283136.25872118,24,60" } ), eso_strm->str() );
	EXPECT_EQ( delimitedString( { "1,616771620.987027,4283136.25168393, 1,10,4283136.25872118,24,60" } ), mtr_strm->str() );
	mtr_strm->str(std::string());
	eso_strm->str(std::string());

	WriteReportMeterData( 1, "1", 616771620.98702729, ReportMonthly, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460, false );
	EXPECT_EQ( delimitedString( { "1,616771620.987027,4283136.25168393,21, 1,10,4283136.25872118,21,24,60" } ), eso_strm->str() );
	EXPECT_EQ( delimitedString( { "1,616771620.987027,4283136.25168393,21, 1,10,4283136.25872118,21,24,60" } ), mtr_strm->str() );
	mtr_strm->str(std::string());
	eso_strm->str(std::string());

	WriteReportMeterData( 1, "1", 616771620.98702729, ReportSim, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460, false );
	EXPECT_EQ( delimitedString( { "1,616771620.987027,4283136.25168393,12,21, 1,10,4283136.25872118,12,21,24,60" } ), eso_strm->str() );
	EXPECT_EQ( delimitedString( { "1,616771620.987027,4283136.25168393,12,21, 1,10,4283136.25872118,12,21,24,60" } ), mtr_strm->str() );
	mtr_strm->str(std::string());
	eso_strm->str(std::string());

	WriteReportMeterData( 1, "1", 616771620.98702729, ReportTimeStep, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460, true );
	EXPECT_EQ( delimitedString( { "1,616771620.987027" } ), mtr_strm->str() );
	mtr_strm->str(std::string());

	WriteReportMeterData( 1, "1", 616771620.98702729, ReportEach, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460, true );
	EXPECT_EQ( delimitedString( { "1,616771620.987027" } ), mtr_strm->str() );
	mtr_strm->str(std::string());

	WriteReportMeterData( 1, "1", 616771620.98702729, ReportHourly, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460, true );
	EXPECT_EQ( delimitedString( { "1,616771620.987027" } ), mtr_strm->str() );
	mtr_strm->str(std::string());

	WriteReportMeterData( 1, "1", 616771620.98702729, ReportDaily, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460, true );
	EXPECT_EQ( delimitedString( { "1,616771620.987027,4283136.25168393, 1,10,4283136.25872118,24,60" } ), mtr_strm->str() );
	mtr_strm->str(std::string());

	WriteReportMeterData( 1, "1", 616771620.98702729, ReportMonthly, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460, true );
	EXPECT_EQ( delimitedString( { "1,616771620.987027,4283136.25168393,21, 1,10,4283136.25872118,21,24,60" } ), mtr_strm->str() );
	mtr_strm->str(std::string());

	WriteReportMeterData( 1, "1", 616771620.98702729, ReportSim, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460, true );
	EXPECT_EQ( delimitedString( { "1,616771620.987027,4283136.25168393,12,21, 1,10,4283136.25872118,12,21,24,60" } ), mtr_strm->str() );
	mtr_strm->str(std::string());

	WriteReportMeterData( 1, "1", 0, ReportTimeStep, 0.0, 0, 0.0, 0, false );
	EXPECT_EQ( delimitedString( { "1,0.0" } ), eso_strm->str() );
	EXPECT_EQ( delimitedString( { "1,0.0" } ), mtr_strm->str() );
	mtr_strm->str(std::string());
	eso_strm->str(std::string());

	sqlite_test = std::move( EnergyPlus::sqlite );

	auto reportDataResults = queryResult("SELECT * FROM ReportData;", "ReportData");
	auto reportExtendedDataResults = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

	std::vector< std::vector<std::string> > reportData(
	{
		{"1", "1", "1", "999.9"},
		{"2", "1", "1", "999.9"},
		{"3", "1", "1", "616771620.987027"},
		{"4", "1", "1", "616771620.987027"},
		{"5", "1", "1", "616771620.987027"},
		{"6", "1", "1", "616771620.987027"},
		{"7", "1", "1", "616771620.987027"},
		{"8", "1", "1", "616771620.987027"},
		{"9", "1", "1", "616771620.987027"},
		{"10", "1", "1", "616771620.987027"},
		{"11", "1", "1", "616771620.987027"},
		{"12", "1", "1", "616771620.987027"},
		{"13", "1", "1", "0.0"}
	});

	std::vector< std::vector<std::string> > reportExtendedData(
	{
		{"1","3","4283136.25872118","12","21","24","-9","0","4283136.25168393","12","21","0","1","10"},
		{"2","4","4283136.25872118","12","21","24","-9","0","4283136.25168393","12","21","0","1","10"},
		{"3","5","4283136.25872118","12","21","24","-9","0","4283136.25168393","12","21","0","1","10"},
		{"4","6","4283136.25872118","12","21","24","-9","0","4283136.25168393","12","21","0","1","10"},
		{"5","9","4283136.25872118","12","21","24","-9","0","4283136.25168393","12","21","0","1","10"},
		{"6","10","4283136.25872118","12","21","24","-9","0","4283136.25168393","12","21","0","1","10"},
		{"7","11","4283136.25872118","12","21","24","-9","0","4283136.25168393","12","21","0","1","10"},
		{"8","12","4283136.25872118","12","21","24","-9","0","4283136.25168393","12","21","0","1","10"}
	});

	ASSERT_EQ( reportData.size(), reportDataResults.size() );
	for (size_t i = 0; i < reportDataResults.size(); ++i)
	{
		EXPECT_EQ( reportData[i], reportDataResults[i] );
	}

	ASSERT_EQ( reportExtendedData.size(), reportExtendedDataResults.size() );
	for (size_t i = 0; i < reportExtendedDataResults.size(); ++i)
	{
		EXPECT_EQ( reportExtendedData[i], reportExtendedDataResults[i] );
	}

}

TEST_F( SQLiteFixture, writeReportRealData )
{
	ShowMessage( "Begin Test: OutputProcessor, writeReportRealData" );

	std::unique_ptr<std::ostringstream> eso_strm(new std::ostringstream);

	DataGlobals::eso_stream = eso_strm.get();

	sqlite_test->createSQLiteTimeIndexRecord( 4, 1, 1, 0 );
	sqlite_test->createSQLiteReportDictionaryRecord( 1, 1, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false, _ );

	EnergyPlus::sqlite = std::move( sqlite_test );

	WriteReportRealData( 1, "1", 999.9, 2, 1, ReportTimeStep, 0.0, 0, 0.0, 0 );
	EXPECT_EQ( delimitedString( { "1,999.9" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteReportRealData( 1, "1", 999.9, 2, 1, ReportEach, 0.0, 0, 0.0, 0 );
	EXPECT_EQ( delimitedString( { "1,999.9" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteReportRealData( 1, "1", 999.9, 2, 1, ReportHourly, 0.0, 0, 0.0, 0 );
	EXPECT_EQ( delimitedString( { "1,999.9" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteReportRealData( 1, "1", 616771620.98702729, 2, 1, ReportDaily, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460 );
	EXPECT_EQ( delimitedString( { "1,616771620.987027,4283136.25168393, 1,10,4283136.25872118,24,60" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteReportRealData( 1, "1", 616771620.98702729, 2, 1, ReportMonthly, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460 );
	EXPECT_EQ( delimitedString( { "1,616771620.987027,4283136.25168393,21, 1,10,4283136.25872118,21,24,60" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteReportRealData( 1, "1", 616771620.98702729, 2, 1, ReportSim, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460 );
	EXPECT_EQ( delimitedString( { "1,616771620.987027,4283136.25168393,12,21, 1,10,4283136.25872118,12,21,24,60" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteReportRealData( 1, "1", 616771620.98702729, 1, 10, ReportTimeStep, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460 );
	EXPECT_EQ( delimitedString( { "1,61677162.0987027" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteReportRealData( 1, "1", 616771620.98702729, 1, 10, ReportEach, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460 );
	EXPECT_EQ( delimitedString( { "1,61677162.0987027" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteReportRealData( 1, "1", 616771620.98702729, 1, 10, ReportHourly, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460 );
	EXPECT_EQ( delimitedString( { "1,61677162.0987027" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteReportRealData( 1, "1", 616771620.98702729, 1, 10, ReportDaily, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460 );
	EXPECT_EQ( delimitedString( { "1,61677162.0987027,4283136.25168393, 1,10,4283136.25872118,24,60" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteReportRealData( 1, "1", 616771620.98702729, 1, 10, ReportMonthly, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460 );
	EXPECT_EQ( delimitedString( { "1,61677162.0987027,4283136.25168393,21, 1,10,4283136.25872118,21,24,60" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteReportRealData( 1, "1", 616771620.98702729, 1, 10, ReportSim, 4283136.2516839253, 12210110, 4283136.2587211775, 12212460 );
	EXPECT_EQ( delimitedString( { "1,61677162.0987027,4283136.25168393,12,21, 1,10,4283136.25872118,12,21,24,60" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteReportRealData( 1, "1", 0, 2, 1, ReportTimeStep, 0.0, 0, 0.0, 0 );
	EXPECT_EQ( delimitedString( { "1,0.0" } ), eso_strm->str() );
	eso_strm->str(std::string());

	sqlite_test = std::move( EnergyPlus::sqlite );


	auto reportDataResults = queryResult("SELECT * FROM ReportData;", "ReportData");
	auto reportExtendedDataResults = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

	std::vector< std::vector<std::string> > reportData(
	{
		{"1", "1", "1", "999.9"},
		{"2", "1", "1", "999.9"},
		{"3", "1", "1", "999.9"},
		{"4", "1", "1", "616771620.987027"},
		{"5", "1", "1", "616771620.987027"},
		{"6", "1", "1", "616771620.987027"},
		{"7", "1", "1", "61677162.0987027"},
		{"8", "1", "1", "61677162.0987027"},
		{"9", "1", "1", "61677162.0987027"},
		{"10", "1", "1", "61677162.0987027"},
		{"11", "1", "1", "61677162.0987027"},
		{"12", "1", "1", "61677162.0987027"},
		{"13", "1", "1", "0.0"}
	});

	std::vector< std::vector<std::string> > reportExtendedData(
	{
		{"1","4","4283136.25872118","12","21","24","","0","4283136.25168393","12","21","0","","10"},
		{"2","5","4283136.25872118","12","21","24","","0","4283136.25168393","12","21","0","","10"},
		{"3","6","4283136.25872118","12","21","24","","0","4283136.25168393","12","21","0","","10"},
		{"4","10","4283136.25872118","12","21","24","","0","4283136.25168393","12","21","0","","10"},
		{"5","11","4283136.25872118","12","21","24","","0","4283136.25168393","12","21","0","","10"},
		{"6","12","4283136.25872118","12","21","24","","0","4283136.25168393","12","21","0","","10"}
	});

	ASSERT_EQ( reportData.size(), reportDataResults.size() );
	for (size_t i = 0; i < reportDataResults.size(); ++i)
	{
		EXPECT_EQ( reportData[i], reportDataResults[i] );
	}

	ASSERT_EQ( reportExtendedData.size(), reportExtendedDataResults.size() );
	for (size_t i = 0; i < reportExtendedDataResults.size(); ++i)
	{
		EXPECT_EQ( reportExtendedData[i], reportExtendedDataResults[i] );
	}

}

TEST_F( SQLiteFixture, writeReportIntegerData )
{
	ShowMessage( "Begin Test: OutputProcessor, writeReportIntegerData" );

	std::unique_ptr<std::ostringstream> eso_strm(new std::ostringstream);

	DataGlobals::eso_stream = eso_strm.get();

	sqlite_test->createSQLiteTimeIndexRecord( 4, 1, 1, 0 );
	sqlite_test->createSQLiteReportDictionaryRecord( 1, 1, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false, _ );

	EnergyPlus::sqlite = std::move( sqlite_test );

	WriteReportIntegerData( 1, "1", 999.9, 2, 1, ReportTimeStep, 0, 0, 0, 0 );
	EXPECT_EQ( delimitedString( { "1,999.9" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteReportIntegerData( 1, "1", 999.9, 2, 1, ReportEach, 0, 0, 0, 0 );
	EXPECT_EQ( delimitedString( { "1,999.9" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteReportIntegerData( 1, "1", 999.9, 2, 1, ReportHourly, 0, 0, 0, 0 );
	EXPECT_EQ( delimitedString( { "1,999.9" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteReportIntegerData( 1, "1", 616771620.98702729, 2, 1, ReportDaily, 4283136, 12210110, 4283196, 12212460 );
	EXPECT_EQ( delimitedString( { "1,616771620.987027,4283136, 1,10,4283196,24,60" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteReportIntegerData( 1, "1", 616771620.98702729, 2, 1, ReportMonthly, 4283136, 12210110, 4283196, 12212460 );
	EXPECT_EQ( delimitedString( { "1,616771620.987027,4283136,21, 1,10,4283196,21,24,60" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteReportIntegerData( 1, "1", 616771620.98702729, 2, 1, ReportSim, 4283136, 12210110, 4283196, 12212460 );
	EXPECT_EQ( delimitedString( { "1,616771620.987027,4283136,12,21, 1,10,4283196,12,21,24,60" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteReportIntegerData( 1, "1", 616771620.98702729, 1, 10, ReportTimeStep, 4283136, 12210110, 4283196, 12212460 );
	EXPECT_EQ( delimitedString( { "1,61677162.0987027" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteReportIntegerData( 1, "1", 616771620.98702729, 1, 10, ReportEach, 4283136, 12210110, 4283196, 12212460 );
	EXPECT_EQ( delimitedString( { "1,61677162.0987027" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteReportIntegerData( 1, "1", 616771620.98702729, 1, 10, ReportHourly, 4283136, 12210110, 4283196, 12212460 );
	EXPECT_EQ( delimitedString( { "1,61677162.0987027" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteReportIntegerData( 1, "1", 616771620.98702729, 1, 10, ReportDaily, 4283136, 12210110, 4283196, 12212460 );
	EXPECT_EQ( delimitedString( { "1,61677162.0987027,4283136, 1,10,4283196,24,60" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteReportIntegerData( 1, "1", 616771620.98702729, 1, 10, ReportMonthly, 4283136, 12210110, 4283196, 12212460 );
	EXPECT_EQ( delimitedString( { "1,61677162.0987027,4283136,21, 1,10,4283196,21,24,60" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteReportIntegerData( 1, "1", 616771620.98702729, 1, 10, ReportSim, 4283136, 12210110, 4283196, 12212460 );
	EXPECT_EQ( delimitedString( { "1,61677162.0987027,4283136,12,21, 1,10,4283196,12,21,24,60" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteReportIntegerData( 1, "1", 0, 2, 1, ReportTimeStep, 0, 0, 0, 0 );
	EXPECT_EQ( delimitedString( { "1,0.0" } ), eso_strm->str() );
	eso_strm->str(std::string());

	sqlite_test = std::move( EnergyPlus::sqlite );

	auto reportDataResults = queryResult("SELECT * FROM ReportData;", "ReportData");
	auto reportExtendedDataResults = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

	std::vector< std::vector<std::string> > reportData(
	{
		{"1", "1", "1", "999.9"},
		{"2", "1", "1", "999.9"},
		{"3", "1", "1", "999.9"},
		{"4", "1", "1", "616771620.987027"},
		{"5", "1", "1", "616771620.987027"},
		{"6", "1", "1", "616771620.987027"},
		{"7", "1", "1", "61677162.0987027"},
		{"8", "1", "1", "61677162.0987027"},
		{"9", "1", "1", "61677162.0987027"},
		{"10", "1", "1", "61677162.0987027"},
		{"11", "1", "1", "61677162.0987027"},
		{"12", "1", "1", "61677162.0987027"},
		{"13", "1", "1", "0.0"}
	});

	std::vector< std::vector<std::string> > reportExtendedData(
	{
		{"1","4","4283196.0","12","21","24","","0","4283136.0","12","21","0","","10"},
		{"2","5","4283196.0","12","21","24","","0","4283136.0","12","21","0","","10"},
		{"3","6","4283196.0","12","21","24","","0","4283136.0","12","21","0","","10"},
		{"4","10","4283196.0","12","21","24","","0","4283136.0","12","21","0","","10"},
		{"5","11","4283196.0","12","21","24","","0","4283136.0","12","21","0","","10"},
		{"6","12","4283196.0","12","21","24","","0","4283136.0","12","21","0","","10"}
	});

	ASSERT_EQ( reportData.size(), reportDataResults.size() );
	for (size_t i = 0; i < reportDataResults.size(); ++i)
	{
		EXPECT_EQ( reportData[i], reportDataResults[i] );
	}

	ASSERT_EQ( reportExtendedData.size(), reportExtendedDataResults.size() );
	for (size_t i = 0; i < reportExtendedDataResults.size(); ++i)
	{
		EXPECT_EQ( reportExtendedData[i], reportExtendedDataResults[i] );
	}

}

TEST_F( SQLiteFixture, writeIntegerData )
{
	ShowMessage( "Begin Test: SQLiteFixture, writeIntegerData" );

	std::unique_ptr<std::ostringstream> eso_strm(new std::ostringstream);

	DataGlobals::eso_stream = eso_strm.get();

	sqlite_test->createSQLiteTimeIndexRecord( 4, 1, 1, 0 );
	sqlite_test->createSQLiteReportDictionaryRecord( 1, 1, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false, _ );

	EnergyPlus::sqlite = std::move( sqlite_test );

	WriteIntegerData( 1, "1", 999 );
	EXPECT_EQ( delimitedString( { "1,999" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteIntegerData( 1, "1", 0 );
	EXPECT_EQ( delimitedString( { "1,0" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteIntegerData( 1, "1", -999 );
	EXPECT_EQ( delimitedString( { "1,-999" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteIntegerData( 1, "1", _, 999.9 );
	EXPECT_EQ( delimitedString( { "1,999.9" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteIntegerData( 1, "1", _, 0.0 );
	EXPECT_EQ( delimitedString( { "1,0.0" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteIntegerData( 1, "1", _, -999.9 );
	EXPECT_EQ( delimitedString( { "1,-999.9" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteIntegerData( 1, "1", 999, 999.9 );
	EXPECT_EQ( delimitedString( { "1,999.9" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteIntegerData( 1, "1", 0, 0.0 );
	EXPECT_EQ( delimitedString( { "1,0.0" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteIntegerData( 1, "1", -999, -999.9 );
	EXPECT_EQ( delimitedString( { "1,-999.9" } ), eso_strm->str() );
	eso_strm->str(std::string());

	sqlite_test = std::move( EnergyPlus::sqlite );

	auto reportDataResults = queryResult("SELECT * FROM ReportData;", "ReportData");
	auto reportExtendedDataResults = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

	std::vector< std::vector<std::string> > reportData(
	{
		{"1", "1", "1", "999.0"},
		{"2", "1", "1", "0.0"},
		{"3", "1", "1", "-999.0"},
		{"4", "1", "1", "999.9"},
		{"5", "1", "1", "0.0"},
		{"6", "1", "1", "-999.9"},
		{"7", "1", "1", "999.9"},
		{"8", "1", "1", "0.0"},
		{"9", "1", "1", "-999.9"}
	});

	std::vector< std::vector<std::string> > reportExtendedData({});

	ASSERT_EQ( reportData.size(), reportDataResults.size() );
	for (size_t i = 0; i < reportDataResults.size(); ++i)
	{
		EXPECT_EQ( reportData[i], reportDataResults[i] );
	}

	ASSERT_EQ( reportExtendedData.size(), reportExtendedDataResults.size() );

}

TEST_F( SQLiteFixture, getStandardMeterResourceType )
{
	ShowMessage( "Begin Test: SQLiteFixture, getStandardMeterResourceType" );

	std::map< std::string, std::string > const resource_map = {
		{ "ELECTRICITY", "Electricity" },
		{ "ELECTRIC", "Electricity" },
		{ "ELEC", "Electricity" },
		{ "GAS", "Gas" },
		{ "NATURALGAS", "Gas" },
		{ "NATURAL GAS", "Gas" },
		{ "GASOLINE", "Gasoline" },
		{ "DIESEL", "Diesel" },
		{ "COAL", "Coal" },
		{ "FUEL OIL #1", "FuelOil#1" },
		{ "FUELOIL#1", "FuelOil#1" },
		{ "FUEL OIL", "FuelOil#1" },
		{ "DISTILLATE OIL", "FuelOil#1" },
		{ "FUEL OIL #2", "FuelOil#2" },
		{ "FUELOIL#2", "FuelOil#2" },
		{ "RESIDUAL OIL", "FuelOil#2" },
		{ "PROPANE", "Propane" },
		{ "LPG", "Propane" },
		{ "PROPANEGAS", "Propane" },
		{ "PROPANE GAS", "Propane" },
		{ "WATER", "Water" },
		{ "H2O", "Water" },
		{ "ONSITEWATER", "OnSiteWater" },
		{ "WATERPRODUCED", "OnSiteWater" },
		{ "ONSITE WATER", "OnSiteWater" },
		{ "MAINSWATER", "MainsWater" },
		{ "WATERSUPPLY", "MainsWater" },
		{ "RAINWATER", "RainWater" },
		{ "PRECIPITATION", "RainWater" },
		{ "WELLWATER", "WellWater" },
		{ "GROUNDWATER", "WellWater" },
		{ "CONDENSATE", "Condensate" },
		{ "ENERGYTRANSFER", "EnergyTransfer" },
		{ "ENERGYXFER", "EnergyTransfer" },
		{ "XFER", "EnergyTransfer" },
		{ "STEAM", "Steam" },
		{ "DISTRICTCOOLING", "DistrictCooling" },
		{ "DISTRICTHEATING", "DistrictHeating" },
		{ "ELECTRICITYPRODUCED", "ElectricityProduced" },
		{ "ELECTRICITYPURCHASED", "ElectricityPurchased" },
		{ "ELECTRICITYSURPLUSSOLD", "ElectricitySurplusSold" },
		{ "ELECTRICITYNET", "ElectricityNet" },
		{ "SOLARWATER", "SolarWater" },
		{ "SOLARAIR", "SolarAir" },
		{ "SO2", "SO2" },
		{ "NOX", "NOx" },
		{ "N2O", "N2O" },
		{ "PM", "PM" },
		{ "PM2.5", "PM2.5" },
		{ "PM10", "PM10" },
		{ "CO", "CO" },
		{ "CO2", "CO2" },
		{ "CH4", "CH4" },
		{ "NH3", "NH3" },
		{ "NMVOC", "NMVOC" },
		{ "HG", "Hg" },
		{ "PB", "Pb" },
		{ "NUCLEAR HIGH", "Nuclear High" },
		{ "NUCLEAR LOW", "Nuclear Low" },
		{ "WATERENVIRONMENTALFACTORS", "WaterEnvironmentalFactors" },
		{ "CARBON EQUIVALENT", "Carbon Equivalent" },
		{ "SOURCE", "Source" },
		{ "PLANTLOOPHEATINGDEMAND", "PlantLoopHeatingDemand" },
		{ "PLANTLOOPCOOLINGDEMAND", "PlantLoopCoolingDemand" },
		{ "GENERIC", "Generic" },
		{ "OTHERFUEL1", "OtherFuel1" },
		{ "OTHERFUEL2", "OtherFuel2" }
	};

	std::string out_resource_type;
	bool error_found = false;

	for( auto const & meterType : resource_map ) {
		GetStandardMeterResourceType( out_resource_type, meterType.first, error_found );
		EXPECT_EQ( meterType.second, out_resource_type );
		EXPECT_FALSE( error_found );
	}

	sqlite_test->createSQLiteSimulationsRecord( 1, "EnergyPlus Version", "Current Time" );

	auto const meterType = "BAD INPUT";
	out_resource_type = "BAD INPUT";

	EnergyPlus::sqlite = std::move( sqlite_test );
	GetStandardMeterResourceType( out_resource_type, meterType, error_found );
	sqlite_test = std::move( EnergyPlus::sqlite );

	EXPECT_EQ( meterType, out_resource_type );
	EXPECT_TRUE( error_found );

	auto errorData = queryResult("SELECT * FROM Errors;", "Errors");

	ASSERT_EQ(1ul, errorData.size());
	std::vector<std::string> errorData0 {"1", "1", "1", "GetStandardMeterResourceType: Illegal OutResourceType (for Meters) Entered=BAD INPUT", "1"};
	EXPECT_EQ(errorData0, errorData[0]);

}

TEST( OutputProcessor, determineIndexGroupKeyFromMeterName )
{
	ShowMessage( "Begin Test: OutputProcessor, determineIndexGroupKeyFromMeterName" );

	std::map< std::string, int > const resource_map = {
		{ "Electricity:Facility", 100 },
		{ "Gas:Facility", 101 },
		{ "DistricHeating:Facility", 102 },
		{ "DistricCooling:Facility", 103 },
		{ "ElectricityNet:Facility", 104 },
		{ "Electricity:Building", 201 },
		{ "Gas:Building", 202 },
		{ "Electricity:HVAC", 301 },
		{ "InteriorLights:Electricity", 401 },
		{ "InteriorLights:Electricity:Zone", 501 },
		{ "BAD INPUT", -11 }
	};

	for( auto const & indexGroup : resource_map ) {
		EXPECT_EQ( indexGroup.second, DetermineIndexGroupKeyFromMeterName( indexGroup.first ) ) << "where meterName is " << indexGroup.first;
	}
}

TEST_F( SQLiteFixture, validateIndexType )
{
	ShowMessage( "Begin Test: SQLiteFixture, validateIndexType" );

	std::map< std::string, int > const resource_map = {
		{ "ZONE", 1 },
		{ "HEATBALANCE", 1 },
		{ "HEAT BALANCE", 1 },
		{ "HVAC", 2 },
		{ "SYSTEM", 2 },
		{ "PLANT", 2 }
	};

	auto const calledFrom = "UnitTest";

	for( auto const & indexGroup : resource_map ) {
		EXPECT_EQ( indexGroup.second, ValidateIndexType( indexGroup.first, calledFrom ) ) << "where indexTypeKey is " << indexGroup.first;
	}

	// Can't test a bad input because it fatal errors and kills the whole unit test framework.

	// auto const indexTypeKey = "BAD INPUT";

	// EnergyPlus::sqlite = std::move( sqlite_test );
	// auto index = ValidateIndexType( indexTypeKey, calledFrom );
	// sqlite_test = std::move( EnergyPlus::sqlite );

	// EXPECT_EQ( 0, index );

	// auto errorData = queryResult("SELECT * FROM Errors;", "Errors");

	// ASSERT_EQ(1ul, errorData.size());
	// std::vector<std::string> errorData0 {"1", "1", "1", "OutputProcessor/ValidateIndexType: Invalid Index Key passed to ValidateIndexType=BAD INPUT", "1"};
	// EXPECT_EQ(errorData0, errorData[0]);

}

TEST( OutputProcessor, standardIndexTypeKey )
{
	ShowMessage( "Begin Test: OutputProcessor, standardIndexTypeKey" );

	EXPECT_EQ( "Zone", StandardIndexTypeKey( 1 ) );
	EXPECT_EQ( "HVAC", StandardIndexTypeKey( 2 ) );
	EXPECT_EQ( "UNKW", StandardIndexTypeKey( 0 ) );
	EXPECT_EQ( "UNKW", StandardIndexTypeKey( -1 ) );
	EXPECT_EQ( "UNKW", StandardIndexTypeKey( 3 ) );

}

TEST_F( SQLiteFixture, validateVariableType )
{
	ShowMessage( "Begin Test: SQLiteFixture, validateVariableType" );

	std::map< std::string, int > const resource_map = {
		{ "STATE", 1 },
		{ "AVERAGE", 1 },
		{ "AVERAGED", 1 },
		{ "NON STATE", 2 },
		{ "NONSTATE", 2 },
		{ "SUM", 2 },
		{ "SUMMED", 2 }
	};

	for( auto const & variableType : resource_map ) {
		EXPECT_EQ( variableType.second, ValidateVariableType( variableType.first ) ) << "where variableTypeKey is " << variableType.first;
	}

	sqlite_test->createSQLiteSimulationsRecord( 1, "EnergyPlus Version", "Current Time" );

	auto const variableTypeKey = "BAD INPUT";

	EnergyPlus::sqlite = std::move( sqlite_test );
	auto index = ValidateVariableType( variableTypeKey );
	sqlite_test = std::move( EnergyPlus::sqlite );

	EXPECT_EQ( 0, index );

	auto errorData = queryResult("SELECT * FROM Errors;", "Errors");

	ASSERT_EQ(1ul, errorData.size());
	std::vector<std::string> errorData0 {"1", "1", "1", "Invalid variable type requested=BAD INPUT", "1"};
	EXPECT_EQ(errorData0, errorData[0]);

}

TEST( OutputProcessor, standardVariableTypeKey )
{
	ShowMessage( "Begin Test: OutputProcessor, standardVariableTypeKey" );

	EXPECT_EQ( "Average", StandardVariableTypeKey( 1 ) );
	EXPECT_EQ( "Sum", StandardVariableTypeKey( 2 ) );
	EXPECT_EQ( "Unknown", StandardVariableTypeKey( 0 ) );
	EXPECT_EQ( "Unknown", StandardVariableTypeKey( -1 ) );
	EXPECT_EQ( "Unknown", StandardVariableTypeKey( 3 ) );

}

TEST_F(  SQLiteFixture, determineMeterIPUnits ) 
{
	ShowMessage( "Begin Test: SQLiteFixture, determineMeterIPUnits" );

	// int & CodeForIPUnits, // Output Code for IP Units
	// std::string const & ResourceType, // Resource Type
	// std::string const & MtrUnits, // Meter units
	// bool & ErrorsFound // true if errors found during subroutine

	int ipUnits = -999999;
	bool errorFound = false;

	DetermineMeterIPUnits( ipUnits, "ELEC", "J", errorFound );
	EXPECT_EQ( RT_IPUnits_Electricity, ipUnits );
	EXPECT_FALSE( errorFound );

	DetermineMeterIPUnits( ipUnits, "GAS", "J", errorFound );
	EXPECT_EQ( RT_IPUnits_Gas, ipUnits );
	EXPECT_FALSE( errorFound );

	DetermineMeterIPUnits( ipUnits, "COOL", "J", errorFound );
	EXPECT_EQ( RT_IPUnits_Cooling, ipUnits );
	EXPECT_FALSE( errorFound );

	DetermineMeterIPUnits( ipUnits, "WATER", "m3", errorFound );
	EXPECT_EQ( RT_IPUnits_Water, ipUnits );
	EXPECT_FALSE( errorFound );

	DetermineMeterIPUnits( ipUnits, "OTHER", "m3", errorFound );
	EXPECT_EQ( RT_IPUnits_OtherM3, ipUnits );
	EXPECT_FALSE( errorFound );

	DetermineMeterIPUnits( ipUnits, "OTHER", "kg", errorFound );
	EXPECT_EQ( RT_IPUnits_OtherKG, ipUnits );
	EXPECT_FALSE( errorFound );

	DetermineMeterIPUnits( ipUnits, "OTHER", "L", errorFound );
	EXPECT_EQ( RT_IPUnits_OtherL, ipUnits );
	EXPECT_FALSE( errorFound );

	sqlite_test->createSQLiteSimulationsRecord( 1, "EnergyPlus Version", "Current Time" );

	EnergyPlus::sqlite = std::move( sqlite_test );

	ipUnits = -999999;
	DetermineMeterIPUnits( ipUnits, "UNKONWN", "badUnits", errorFound );
	EXPECT_EQ( RT_IPUnits_OtherJ, ipUnits );
	EXPECT_TRUE( errorFound );

	ipUnits = -999999;
	DetermineMeterIPUnits( ipUnits, "ELEC", "kWh", errorFound );
	EXPECT_EQ( RT_IPUnits_Electricity, ipUnits );
	EXPECT_TRUE( errorFound );

	sqlite_test = std::move( EnergyPlus::sqlite );

	auto errorData = queryResult("SELECT * FROM Errors;", "Errors");

	ASSERT_EQ(2ul, errorData.size());
	std::vector<std::string> errorData0 {"1", "1", "0", "DetermineMeterIPUnits: Meter units not recognized for IP Units conversion=[badUnits].", "1"};
	std::vector<std::string> errorData1 {"2", "1", "0", "DetermineMeterIPUnits: Meter units not recognized for IP Units conversion=[kWh].", "1"};
	EXPECT_EQ(errorData0, errorData[0]);
	EXPECT_EQ(errorData1, errorData[1]);

}

TEST( OutputProcessor, dateToStringWithMonth )
{
	ShowMessage( "Begin Test: OutputProcessor, dateToStringWithMonth" );

	EXPECT_EQ( "01-JAN-00:01", DateToStringWithMonth( 1010101 ) );
	EXPECT_EQ( "01-JAN-00:00", DateToStringWithMonth( 1010100 ) );
	EXPECT_EQ( "01-FEB-01:00", DateToStringWithMonth( 2010160 ) );
	EXPECT_EQ( "20-MAR-01:59", DateToStringWithMonth( 3200259 ) );
	EXPECT_EQ( "13-APR-23:59", DateToStringWithMonth( 4132459 ) );
	EXPECT_EQ( "15-MAY-20:30", DateToStringWithMonth( 5152130 ) );
	EXPECT_EQ( "19-JUN-12:10", DateToStringWithMonth( 6191310 ) );
	EXPECT_EQ( "25-JUL-19:40", DateToStringWithMonth( 7252040 ) );
	EXPECT_EQ( "05-AUG-06:22", DateToStringWithMonth( 8050722 ) );
	EXPECT_EQ( "03-SEP-09:50", DateToStringWithMonth( 9031050 ) );
	EXPECT_EQ( "27-OCT-03:31", DateToStringWithMonth( 10270431 ) );
	EXPECT_EQ( "08-NOV-22:28", DateToStringWithMonth( 11082328 ) );
	EXPECT_EQ( "21-DEC-00:10", DateToStringWithMonth( 12210110 ) );

	EXPECT_EQ( "-", DateToStringWithMonth( 0 ) );
	EXPECT_EQ( "-", DateToStringWithMonth( -9999 ) );
	EXPECT_EQ( "-", DateToStringWithMonth( -12210110 ) );
	EXPECT_EQ( "-", DateToStringWithMonth( 13082328 ) );
	EXPECT_EQ( "-", DateToStringWithMonth( 10100 ) );
	EXPECT_EQ( "-", DateToStringWithMonth( 1000101 ) );
	EXPECT_EQ( "-", DateToStringWithMonth( 990101 ) );
	EXPECT_EQ( "-", DateToStringWithMonth( 1010001 ) );
	EXPECT_EQ( "-", DateToStringWithMonth( 1009901 ) );
	EXPECT_EQ( "-", DateToStringWithMonth( 1010099 ) );
	EXPECT_EQ( "-", DateToStringWithMonth( 1320100 ) );
	EXPECT_EQ( "-", DateToStringWithMonth( 1012500 ) );

}

TEST_F( SQLiteFixture, writeMeterDictionaryItem )
{
	ShowMessage( "Begin Test: SQLiteFixture, writeMeterDictionaryItem" );

	std::unique_ptr<std::ostringstream> eso_strm(new std::ostringstream);
	std::unique_ptr<std::ostringstream> mtr_strm(new std::ostringstream);

	DataGlobals::eso_stream = eso_strm.get();
	DataGlobals::mtr_stream = mtr_strm.get();

	InitializeOutput();

	sqlite_test->createSQLiteTimeIndexRecord( 4, 1, 1, 0 );

	EnergyPlus::sqlite = std::move( sqlite_test );

	WriteMeterDictionaryItem( ReportTimeStep, 1, 1, -999, "indexGroup", "1", "meterName", "meterUnits", false, false );
	EXPECT_EQ( delimitedString( { "1,1,meterName [meterUnits] !TimeStep" } ), eso_strm->str() );
	EXPECT_EQ( delimitedString( { "1,1,meterName [meterUnits] !TimeStep" } ), mtr_strm->str() );
	mtr_strm->str(std::string());
	eso_strm->str(std::string());

	WriteMeterDictionaryItem( ReportTimeStep, 2, 2, -999, "indexGroup", "2", "meterName", "meterUnits", false, false );
	EXPECT_EQ( delimitedString( { "2,1,meterName [meterUnits] !TimeStep" } ), eso_strm->str() );
	EXPECT_EQ( delimitedString( { "2,1,meterName [meterUnits] !TimeStep" } ), mtr_strm->str() );
	mtr_strm->str(std::string());
	eso_strm->str(std::string());

	WriteMeterDictionaryItem( ReportTimeStep, 1, 3, -999, "indexGroup", "3", "meterName", "meterUnits", true, false );
	EXPECT_EQ( delimitedString( { "3,1,Cumulative meterName [meterUnits] !TimeStep" } ), eso_strm->str() );
	EXPECT_EQ( delimitedString( { "3,1,Cumulative meterName [meterUnits] !TimeStep" } ), mtr_strm->str() );
	mtr_strm->str(std::string());
	eso_strm->str(std::string());

	WriteMeterDictionaryItem( ReportTimeStep, 1, 4, -999, "indexGroup", "4", "meterName", "meterUnits", false, true );
	EXPECT_EQ( delimitedString( { "4,1,meterName [meterUnits] !TimeStep" } ), mtr_strm->str() );
	mtr_strm->str(std::string());

	WriteMeterDictionaryItem( ReportTimeStep, 1, 5, -999, "indexGroup", "5", "meterName", "meterUnits", true, true );
	EXPECT_EQ( delimitedString( { "5,1,Cumulative meterName [meterUnits] !TimeStep" } ), mtr_strm->str() );
	mtr_strm->str(std::string());

	WriteMeterDictionaryItem( ReportEach, 1, 6, -999, "indexGroup", "6", "meterName", "meterUnits", false, false );
	EXPECT_EQ( delimitedString( { "6,1,meterName [meterUnits] !Each Call" } ), eso_strm->str() );
	EXPECT_EQ( delimitedString( { "6,1,meterName [meterUnits] !Each Call" } ), mtr_strm->str() );
	mtr_strm->str(std::string());
	eso_strm->str(std::string());

	WriteMeterDictionaryItem( ReportEach, 2, 7, -999, "indexGroup", "7", "meterName", "meterUnits", false, false );
	EXPECT_EQ( delimitedString( { "7,1,meterName [meterUnits] !Each Call" } ), eso_strm->str() );
	EXPECT_EQ( delimitedString( { "7,1,meterName [meterUnits] !Each Call" } ), mtr_strm->str() );
	mtr_strm->str(std::string());
	eso_strm->str(std::string());

	WriteMeterDictionaryItem( ReportEach, 1, 8, -999, "indexGroup", "8", "meterName", "meterUnits", true, false );
	EXPECT_EQ( delimitedString( { "8,1,Cumulative meterName [meterUnits] !Each Call" } ), eso_strm->str() );
	EXPECT_EQ( delimitedString( { "8,1,Cumulative meterName [meterUnits] !Each Call" } ), mtr_strm->str() );
	mtr_strm->str(std::string());
	eso_strm->str(std::string());

	WriteMeterDictionaryItem( ReportEach, 1, 9, -999, "indexGroup", "9", "meterName", "meterUnits", false, true );
	EXPECT_EQ( delimitedString( { "9,1,meterName [meterUnits] !Each Call" } ), mtr_strm->str() );
	mtr_strm->str(std::string());

	WriteMeterDictionaryItem( ReportEach, 1, 10, -999, "indexGroup", "10", "meterName", "meterUnits", true, true );
	EXPECT_EQ( delimitedString( { "10,1,Cumulative meterName [meterUnits] !Each Call" } ), mtr_strm->str() );
	mtr_strm->str(std::string());

	WriteMeterDictionaryItem( ReportHourly, 1, 11, -999, "indexGroup", "11", "meterName", "meterUnits", false, false );
	EXPECT_EQ( delimitedString( { "11,1,meterName [meterUnits] !Hourly" } ), eso_strm->str() );
	EXPECT_EQ( delimitedString( { "11,1,meterName [meterUnits] !Hourly" } ), mtr_strm->str() );
	mtr_strm->str(std::string());
	eso_strm->str(std::string());

	WriteMeterDictionaryItem( ReportHourly, 2, 12, -999, "indexGroup", "12", "meterName", "meterUnits", false, false );
	EXPECT_EQ( delimitedString( { "12,1,meterName [meterUnits] !Hourly" } ), eso_strm->str() );
	EXPECT_EQ( delimitedString( { "12,1,meterName [meterUnits] !Hourly" } ), mtr_strm->str() );
	mtr_strm->str(std::string());
	eso_strm->str(std::string());

	WriteMeterDictionaryItem( ReportHourly, 1, 13, -999, "indexGroup", "13", "meterName", "meterUnits", true, false );
	EXPECT_EQ( delimitedString( { "13,1,Cumulative meterName [meterUnits] !Hourly" } ), eso_strm->str() );
	EXPECT_EQ( delimitedString( { "13,1,Cumulative meterName [meterUnits] !Hourly" } ), mtr_strm->str() );
	mtr_strm->str(std::string());
	eso_strm->str(std::string());

	WriteMeterDictionaryItem( ReportHourly, 1, 14, -999, "indexGroup", "14", "meterName", "meterUnits", false, true );
	EXPECT_EQ( delimitedString( { "14,1,meterName [meterUnits] !Hourly" } ), mtr_strm->str() );
	mtr_strm->str(std::string());

	WriteMeterDictionaryItem( ReportHourly, 1, 15, -999, "indexGroup", "15", "meterName", "meterUnits", true, true );
	EXPECT_EQ( delimitedString( { "15,1,Cumulative meterName [meterUnits] !Hourly" } ), mtr_strm->str() );
	mtr_strm->str(std::string());

	WriteMeterDictionaryItem( ReportDaily, 1, 16, -999, "indexGroup", "16", "meterName", "meterUnits", false, false );
	EXPECT_EQ( delimitedString( { "16,7,meterName [meterUnits] !Daily [Value,Min,Hour,Minute,Max,Hour,Minute]" } ), eso_strm->str() );
	EXPECT_EQ( delimitedString( { "16,7,meterName [meterUnits] !Daily [Value,Min,Hour,Minute,Max,Hour,Minute]" } ), mtr_strm->str() );
	mtr_strm->str(std::string());
	eso_strm->str(std::string());

	WriteMeterDictionaryItem( ReportDaily, 2, 17, -999, "indexGroup", "17", "meterName", "meterUnits", false, false );
	EXPECT_EQ( delimitedString( { "17,7,meterName [meterUnits] !Daily  [Value,Min,Hour,Minute,Max,Hour,Minute]" } ), eso_strm->str() );
	EXPECT_EQ( delimitedString( { "17,7,meterName [meterUnits] !Daily  [Value,Min,Hour,Minute,Max,Hour,Minute]" } ), mtr_strm->str() );
	mtr_strm->str(std::string());
	eso_strm->str(std::string());

	WriteMeterDictionaryItem( ReportDaily, 1, 18, -999, "indexGroup", "18", "meterName", "meterUnits", true, false );
	EXPECT_EQ( delimitedString( { "18,1,Cumulative meterName [meterUnits] !Daily " } ), eso_strm->str() );
	EXPECT_EQ( delimitedString( { "18,1,Cumulative meterName [meterUnits] !Daily " } ), mtr_strm->str() );
	mtr_strm->str(std::string());
	eso_strm->str(std::string());

	WriteMeterDictionaryItem( ReportDaily, 1, 19, -999, "indexGroup", "19", "meterName", "meterUnits", false, true );
	EXPECT_EQ( delimitedString( { "19,7,meterName [meterUnits] !Daily [Value,Min,Hour,Minute,Max,Hour,Minute]" } ), mtr_strm->str() );
	mtr_strm->str(std::string());

	WriteMeterDictionaryItem( ReportDaily, 1, 20, -999, "indexGroup", "20", "meterName", "meterUnits", true, true );
	EXPECT_EQ( delimitedString( { "20,1,Cumulative meterName [meterUnits] !Daily " } ), mtr_strm->str() );
	mtr_strm->str(std::string());

	WriteMeterDictionaryItem( ReportMonthly, 1, 21, -999, "indexGroup", "21", "meterName", "meterUnits", false, false );
	EXPECT_EQ( delimitedString( { "21,9,meterName [meterUnits] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]" } ), eso_strm->str() );
	EXPECT_EQ( delimitedString( { "21,9,meterName [meterUnits] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]" } ), mtr_strm->str() );
	mtr_strm->str(std::string());
	eso_strm->str(std::string());

	WriteMeterDictionaryItem( ReportMonthly, 2, 22, -999, "indexGroup", "22", "meterName", "meterUnits", false, false );
	EXPECT_EQ( delimitedString( { "22,9,meterName [meterUnits] !Monthly  [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]" } ), eso_strm->str() );
	EXPECT_EQ( delimitedString( { "22,9,meterName [meterUnits] !Monthly  [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]" } ), mtr_strm->str() );
	mtr_strm->str(std::string());
	eso_strm->str(std::string());

	WriteMeterDictionaryItem( ReportMonthly, 1, 23, -999, "indexGroup", "23", "meterName", "meterUnits", true, false );
	EXPECT_EQ( delimitedString( { "23,1,Cumulative meterName [meterUnits] !Monthly " } ), eso_strm->str() );
	EXPECT_EQ( delimitedString( { "23,1,Cumulative meterName [meterUnits] !Monthly " } ), mtr_strm->str() );
	mtr_strm->str(std::string());
	eso_strm->str(std::string());

	WriteMeterDictionaryItem( ReportMonthly, 1, 24, -999, "indexGroup", "24", "meterName", "meterUnits", false, true );
	EXPECT_EQ( delimitedString( { "24,9,meterName [meterUnits] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]" } ), mtr_strm->str() );
	mtr_strm->str(std::string());

	WriteMeterDictionaryItem( ReportMonthly, 1, 25, -999, "indexGroup", "25", "meterName", "meterUnits", true, true );
	EXPECT_EQ( delimitedString( { "25,1,Cumulative meterName [meterUnits] !Monthly " } ), mtr_strm->str() );
	mtr_strm->str(std::string());

	WriteMeterDictionaryItem( ReportSim, 1, 26, -999, "indexGroup", "26", "meterName", "meterUnits", false, false );
	EXPECT_EQ( delimitedString( { "26,11,meterName [meterUnits] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]" } ), eso_strm->str() );
	EXPECT_EQ( delimitedString( { "26,11,meterName [meterUnits] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]" } ), mtr_strm->str() );
	mtr_strm->str(std::string());
	eso_strm->str(std::string());

	WriteMeterDictionaryItem( ReportSim, 2, 27, -999, "indexGroup", "27", "meterName", "meterUnits", false, false );
	EXPECT_EQ( delimitedString( { "27,11,meterName [meterUnits] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]" } ), eso_strm->str() );
	EXPECT_EQ( delimitedString( { "27,11,meterName [meterUnits] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]" } ), mtr_strm->str() );
	mtr_strm->str(std::string());
	eso_strm->str(std::string());

	WriteMeterDictionaryItem( ReportSim, 1, 28, -999, "indexGroup", "28", "meterName", "meterUnits", true, false );
	EXPECT_EQ( delimitedString( { "28,1,Cumulative meterName [meterUnits] !RunPeriod " } ), eso_strm->str() );
	EXPECT_EQ( delimitedString( { "28,1,Cumulative meterName [meterUnits] !RunPeriod " } ), mtr_strm->str() );
	mtr_strm->str(std::string());
	eso_strm->str(std::string());

	WriteMeterDictionaryItem( ReportSim, 1, 29, -999, "indexGroup", "29", "meterName", "meterUnits", false, true );
	EXPECT_EQ( delimitedString( { "29,11,meterName [meterUnits] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]" } ), mtr_strm->str() );
	mtr_strm->str(std::string());

	WriteMeterDictionaryItem( ReportSim, 1, 30, -999, "indexGroup", "30", "meterName", "meterUnits", true, true );
	EXPECT_EQ( delimitedString( { "30,1,Cumulative meterName [meterUnits] !RunPeriod " } ), mtr_strm->str() );
	mtr_strm->str(std::string());

	sqlite_test = std::move( EnergyPlus::sqlite );

	auto reportDataDictionaryResults = queryResult("SELECT * FROM ReportDataDictionary;", "ReportDataDictionary");

	std::vector< std::vector<std::string> > reportDataDictionary(
	{
		{"1", "1", "Avg", "indexGroup", "HVAC System", "", "meterName", "Zone Timestep", "", "meterUnits"},
		{"2", "1", "Sum", "indexGroup", "HVAC System", "", "meterName", "Zone Timestep", "", "meterUnits"},
		{"3", "1", "Avg", "indexGroup", "HVAC System", "Cumulative ", "meterName", "Zone Timestep", "", "meterUnits"},
		{"4", "1", "Avg", "indexGroup", "HVAC System", "", "meterName", "Zone Timestep", "", "meterUnits"},
		{"5", "1", "Avg", "indexGroup", "HVAC System", "Cumulative ", "meterName", "Zone Timestep", "", "meterUnits"},
		{"6", "1", "Avg", "indexGroup", "HVAC System", "", "meterName", "HVAC System Timestep", "", "meterUnits"},
		{"7", "1", "Sum", "indexGroup", "HVAC System", "", "meterName", "HVAC System Timestep", "", "meterUnits"},
		{"8", "1", "Avg", "indexGroup", "HVAC System", "Cumulative ", "meterName", "HVAC System Timestep", "", "meterUnits"},
		{"9", "1", "Avg", "indexGroup", "HVAC System", "", "meterName", "HVAC System Timestep", "", "meterUnits"},
		{"10", "1", "Avg", "indexGroup", "HVAC System", "Cumulative ", "meterName", "HVAC System Timestep", "", "meterUnits"},
		{"11", "1", "Avg", "indexGroup", "HVAC System", "", "meterName", "Hourly", "", "meterUnits"},
		{"12", "1", "Sum", "indexGroup", "HVAC System", "", "meterName", "Hourly", "", "meterUnits"},
		{"13", "1", "Avg", "indexGroup", "HVAC System", "Cumulative ", "meterName", "Hourly", "", "meterUnits"},
		{"14", "1", "Avg", "indexGroup", "HVAC System", "", "meterName", "Hourly", "", "meterUnits"},
		{"15", "1", "Avg", "indexGroup", "HVAC System", "Cumulative ", "meterName", "Hourly", "", "meterUnits"},
		{"16", "1", "Avg", "indexGroup", "HVAC System", "", "meterName", "Daily", "", "meterUnits"},
		{"17", "1", "Sum", "indexGroup", "HVAC System", "", "meterName", "Daily", "", "meterUnits"},
		{"18", "1", "Avg", "indexGroup", "HVAC System", "Cumulative ", "meterName", "Daily", "", "meterUnits"},
		{"19", "1", "Avg", "indexGroup", "HVAC System", "", "meterName", "Daily", "", "meterUnits"},
		{"20", "1", "Avg", "indexGroup", "HVAC System", "Cumulative ", "meterName", "Daily", "", "meterUnits"},
		{"21", "1", "Avg", "indexGroup", "HVAC System", "", "meterName", "Monthly", "", "meterUnits"},
		{"22", "1", "Sum", "indexGroup", "HVAC System", "", "meterName", "Monthly", "", "meterUnits"},
		{"23", "1", "Avg", "indexGroup", "HVAC System", "Cumulative ", "meterName", "Monthly", "", "meterUnits"},
		{"24", "1", "Avg", "indexGroup", "HVAC System", "", "meterName", "Monthly", "", "meterUnits"},
		{"25", "1", "Avg", "indexGroup", "HVAC System", "Cumulative ", "meterName", "Monthly", "", "meterUnits"},
		{"26", "1", "Avg", "indexGroup", "HVAC System", "", "meterName", "Run Period", "", "meterUnits"},
		{"27", "1", "Sum", "indexGroup", "HVAC System", "", "meterName", "Run Period", "", "meterUnits"},
		{"28", "1", "Avg", "indexGroup", "HVAC System", "Cumulative ", "meterName", "Run Period", "", "meterUnits"},
		{"29", "1", "Avg", "indexGroup", "HVAC System", "", "meterName", "Run Period", "", "meterUnits"},
		{"30", "1", "Avg", "indexGroup", "HVAC System", "Cumulative ", "meterName", "Run Period", "", "meterUnits"}
	});
	ASSERT_EQ( reportDataDictionary.size(), reportDataDictionaryResults.size() );
	for (size_t i = 0; i < reportDataDictionaryResults.size(); ++i)
	{
		EXPECT_EQ( reportDataDictionary[i], reportDataDictionaryResults[i] );
	}

	// This should be all necessary clean up...
	{ IOFlags flags; flags.DISPOSE( "DELETE" ); gio::close( OutputFileMeterDetails, flags ); }
	RVariableTypes.deallocate();
	IVariableTypes.deallocate();
	ReportList.deallocate();
	EndUseCategory.deallocate();

}

TEST_F( SQLiteFixture, writeReportVariableDictionaryItem )
{
	ShowMessage( "Begin Test: SQLiteFixture, writeReportVariableDictionaryItem" );

	std::unique_ptr<std::ostringstream> eso_strm(new std::ostringstream);

	DataGlobals::eso_stream = eso_strm.get();

	InitializeOutput();

	sqlite_test->createSQLiteTimeIndexRecord( 4, 1, 1, 0 );

	EnergyPlus::sqlite = std::move( sqlite_test );

	WriteReportVariableDictionaryItem( ReportTimeStep, 1, 1, -999, "indexGroup", "1", "keyedValue", "variableName", 1, "variableUnits" );
	EXPECT_EQ( delimitedString( { "1,1,keyedValue,variableName [variableUnits] !TimeStep" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteReportVariableDictionaryItem( ReportTimeStep, 2, 2, -999, "indexGroup", "2", "keyedValue", "variableName", 1, "variableUnits" );
	EXPECT_EQ( delimitedString( { "2,1,keyedValue,variableName [variableUnits] !TimeStep" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteReportVariableDictionaryItem( ReportTimeStep, 1, 3, -999, "indexGroup", "3", "keyedValue", "variableName", 1, "variableUnits", "scheduleName" );
	EXPECT_EQ( delimitedString( { "3,1,keyedValue,variableName [variableUnits] !TimeStep,scheduleName" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteReportVariableDictionaryItem( ReportTimeStep, 1, 4, -999, "indexGroup", "4", "keyedValue", "variableName", 2, "variableUnits" );
	EXPECT_EQ( delimitedString( { "4,1,keyedValue,variableName [variableUnits] !TimeStep" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteReportVariableDictionaryItem( ReportTimeStep, 1, 5, -999, "indexGroup", "5", "keyedValue", "variableName", 3, "variableUnits" );
	EXPECT_EQ( delimitedString( { "5,1,keyedValue,variableName [variableUnits] !TimeStep" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteReportVariableDictionaryItem( ReportEach, 1, 6, -999, "indexGroup", "6", "keyedValue", "variableName", 1, "variableUnits" );
	EXPECT_EQ( delimitedString( { "6,1,keyedValue,variableName [variableUnits] !Each Call" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteReportVariableDictionaryItem( ReportEach, 2, 7, -999, "indexGroup", "7", "keyedValue", "variableName", 1, "variableUnits" );
	EXPECT_EQ( delimitedString( { "7,1,keyedValue,variableName [variableUnits] !Each Call" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteReportVariableDictionaryItem( ReportEach, 1, 8, -999, "indexGroup", "8", "keyedValue", "variableName", 1, "variableUnits", "scheduleName" );
	EXPECT_EQ( delimitedString( { "8,1,keyedValue,variableName [variableUnits] !Each Call,scheduleName" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteReportVariableDictionaryItem( ReportEach, 1, 9, -999, "indexGroup", "9", "keyedValue", "variableName", 2, "variableUnits" );
	EXPECT_EQ( delimitedString( { "9,1,keyedValue,variableName [variableUnits] !Each Call" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteReportVariableDictionaryItem( ReportEach, 1, 10, -999, "indexGroup", "10", "keyedValue", "variableName", 3, "variableUnits" );
	EXPECT_EQ( delimitedString( { "10,1,keyedValue,variableName [variableUnits] !Each Call" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteReportVariableDictionaryItem( ReportHourly, 1, 11, -999, "indexGroup", "11", "keyedValue", "variableName", 1, "variableUnits" );
	EXPECT_TRUE( TrackingHourlyVariables );
	TrackingHourlyVariables = false;
	EXPECT_EQ( delimitedString( { "11,1,keyedValue,variableName [variableUnits] !Hourly" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteReportVariableDictionaryItem( ReportHourly, 2, 12, -999, "indexGroup", "12", "keyedValue", "variableName", 1, "variableUnits" );
	EXPECT_TRUE( TrackingHourlyVariables );
	TrackingHourlyVariables = false;
	EXPECT_EQ( delimitedString( { "12,1,keyedValue,variableName [variableUnits] !Hourly" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteReportVariableDictionaryItem( ReportHourly, 1, 13, -999, "indexGroup", "13", "keyedValue", "variableName", 1, "variableUnits", "scheduleName" );
	EXPECT_TRUE( TrackingHourlyVariables );
	TrackingHourlyVariables = false;
	EXPECT_EQ( delimitedString( { "13,1,keyedValue,variableName [variableUnits] !Hourly,scheduleName" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteReportVariableDictionaryItem( ReportHourly, 1, 14, -999, "indexGroup", "14", "keyedValue", "variableName", 2, "variableUnits" );
	EXPECT_TRUE( TrackingHourlyVariables );
	TrackingHourlyVariables = false;
	EXPECT_EQ( delimitedString( { "14,1,keyedValue,variableName [variableUnits] !Hourly" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteReportVariableDictionaryItem( ReportHourly, 1, 15, -999, "indexGroup", "15", "keyedValue", "variableName", 3, "variableUnits" );
	EXPECT_TRUE( TrackingHourlyVariables );
	TrackingHourlyVariables = false;
	EXPECT_EQ( delimitedString( { "15,1,keyedValue,variableName [variableUnits] !Hourly" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteReportVariableDictionaryItem( ReportDaily, 1, 16, -999, "indexGroup", "16", "keyedValue", "variableName", 1, "variableUnits" );
	EXPECT_TRUE( TrackingDailyVariables );
	TrackingDailyVariables = false;
	EXPECT_EQ( delimitedString( { "16,7,keyedValue,variableName [variableUnits] !Daily [Value,Min,Hour,Minute,Max,Hour,Minute]" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteReportVariableDictionaryItem( ReportDaily, 2, 17, -999, "indexGroup", "17", "keyedValue", "variableName", 1, "variableUnits" );
	EXPECT_TRUE( TrackingDailyVariables );
	TrackingDailyVariables = false;
	EXPECT_EQ( delimitedString( { "17,7,keyedValue,variableName [variableUnits] !Daily  [Value,Min,Hour,Minute,Max,Hour,Minute]" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteReportVariableDictionaryItem( ReportDaily, 1, 18, -999, "indexGroup", "18", "keyedValue", "variableName", 1, "variableUnits", "scheduleName" );
	EXPECT_TRUE( TrackingDailyVariables );
	TrackingDailyVariables = false;
	EXPECT_EQ( delimitedString( { "18,7,keyedValue,variableName [variableUnits] !Daily [Value,Min,Hour,Minute,Max,Hour,Minute],scheduleName" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteReportVariableDictionaryItem( ReportDaily, 1, 19, -999, "indexGroup", "19", "keyedValue", "variableName", 2, "variableUnits" );
	EXPECT_TRUE( TrackingDailyVariables );
	TrackingDailyVariables = false;
	EXPECT_EQ( delimitedString( { "19,7,keyedValue,variableName [variableUnits] !Daily [Value,Min,Hour,Minute,Max,Hour,Minute]" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteReportVariableDictionaryItem( ReportDaily, 1, 20, -999, "indexGroup", "20", "keyedValue", "variableName", 3, "variableUnits" );
	EXPECT_TRUE( TrackingDailyVariables );
	TrackingDailyVariables = false;
	EXPECT_EQ( delimitedString( { "20,7,keyedValue,variableName [variableUnits] !Daily [Value,Min,Hour,Minute,Max,Hour,Minute]" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteReportVariableDictionaryItem( ReportMonthly, 1, 21, -999, "indexGroup", "21", "keyedValue", "variableName", 1, "variableUnits" );
	EXPECT_TRUE( TrackingMonthlyVariables );
	TrackingMonthlyVariables = false;
	EXPECT_EQ( delimitedString( { "21,9,keyedValue,variableName [variableUnits] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteReportVariableDictionaryItem( ReportMonthly, 2, 22, -999, "indexGroup", "22", "keyedValue", "variableName", 1, "variableUnits" );
	EXPECT_TRUE( TrackingMonthlyVariables );
	TrackingMonthlyVariables = false;
	EXPECT_EQ( delimitedString( { "22,9,keyedValue,variableName [variableUnits] !Monthly  [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteReportVariableDictionaryItem( ReportMonthly, 1, 23, -999, "indexGroup", "23", "keyedValue", "variableName", 1, "variableUnits", "scheduleName" );
	EXPECT_TRUE( TrackingMonthlyVariables );
	TrackingMonthlyVariables = false;
	EXPECT_EQ( delimitedString( { "23,9,keyedValue,variableName [variableUnits] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute],scheduleName" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteReportVariableDictionaryItem( ReportMonthly, 1, 24, -999, "indexGroup", "24", "keyedValue", "variableName", 2, "variableUnits" );
	EXPECT_TRUE( TrackingMonthlyVariables );
	TrackingMonthlyVariables = false;
	EXPECT_EQ( delimitedString( { "24,9,keyedValue,variableName [variableUnits] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteReportVariableDictionaryItem( ReportMonthly, 1, 25, -999, "indexGroup", "25", "keyedValue", "variableName", 3, "variableUnits" );
	EXPECT_TRUE( TrackingMonthlyVariables );
	TrackingMonthlyVariables = false;
	EXPECT_EQ( delimitedString( { "25,9,keyedValue,variableName [variableUnits] !Monthly [Value,Min,Day,Hour,Minute,Max,Day,Hour,Minute]" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteReportVariableDictionaryItem( ReportSim, 1, 26, -999, "indexGroup", "26", "keyedValue", "variableName", 1, "variableUnits" );
	EXPECT_TRUE( TrackingRunPeriodVariables );
	TrackingRunPeriodVariables = false;
	EXPECT_EQ( delimitedString( { "26,11,keyedValue,variableName [variableUnits] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteReportVariableDictionaryItem( ReportSim, 2, 27, -999, "indexGroup", "27", "keyedValue", "variableName", 1, "variableUnits" );
	EXPECT_TRUE( TrackingRunPeriodVariables );
	TrackingRunPeriodVariables = false;
	EXPECT_EQ( delimitedString( { "27,11,keyedValue,variableName [variableUnits] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteReportVariableDictionaryItem( ReportSim, 1, 28, -999, "indexGroup", "28", "keyedValue", "variableName", 1, "variableUnits", "scheduleName" );
	EXPECT_TRUE( TrackingRunPeriodVariables );
	TrackingRunPeriodVariables = false;
	EXPECT_EQ( delimitedString( { "28,11,keyedValue,variableName [variableUnits] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute],scheduleName" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteReportVariableDictionaryItem( ReportSim, 1, 29, -999, "indexGroup", "29", "keyedValue", "variableName", 2, "variableUnits" );
	EXPECT_TRUE( TrackingRunPeriodVariables );
	TrackingRunPeriodVariables = false;
	EXPECT_EQ( delimitedString( { "29,11,keyedValue,variableName [variableUnits] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteReportVariableDictionaryItem( ReportSim, 1, 30, -999, "indexGroup", "30", "keyedValue", "variableName", 3, "variableUnits" );
	EXPECT_TRUE( TrackingRunPeriodVariables );
	TrackingRunPeriodVariables = false;
	EXPECT_EQ( delimitedString( { "30,11,keyedValue,variableName [variableUnits] !RunPeriod [Value,Min,Month,Day,Hour,Minute,Max,Month,Day,Hour,Minute]" } ), eso_strm->str() );
	eso_strm->str(std::string());

	sqlite_test = std::move( EnergyPlus::sqlite );

	auto reportDataDictionaryResults = queryResult("SELECT * FROM ReportDataDictionary;", "ReportDataDictionary");

	std::vector< std::vector<std::string> > reportDataDictionary(
	{
		{"1", "0", "Avg", "indexGroup", "HVAC System", "keyedValue", "variableName", "Zone Timestep", "", "variableUnits"},
		{"2", "0", "Sum", "indexGroup", "HVAC System", "keyedValue", "variableName", "Zone Timestep", "", "variableUnits"},
		{"3", "0", "Avg", "indexGroup", "HVAC System", "keyedValue", "variableName", "Zone Timestep", "scheduleName", "variableUnits"},
		{"4", "0", "Avg", "indexGroup", "Zone", "keyedValue", "variableName", "Zone Timestep", "", "variableUnits"},
		{"5", "0", "Avg", "indexGroup", "Unknown!!!", "keyedValue", "variableName", "Zone Timestep", "", "variableUnits"},
		{"6", "0", "Avg", "indexGroup", "HVAC System", "keyedValue", "variableName", "HVAC System Timestep", "", "variableUnits"},
		{"7", "0", "Sum", "indexGroup", "HVAC System", "keyedValue", "variableName", "HVAC System Timestep", "", "variableUnits"},
		{"8", "0", "Avg", "indexGroup", "HVAC System", "keyedValue", "variableName", "HVAC System Timestep", "scheduleName", "variableUnits"},
		{"9", "0", "Avg", "indexGroup", "Zone", "keyedValue", "variableName", "HVAC System Timestep", "", "variableUnits"},
		{"10", "0", "Avg", "indexGroup", "Unknown!!!", "keyedValue", "variableName", "HVAC System Timestep", "", "variableUnits"},
		{"11", "0", "Avg", "indexGroup", "HVAC System", "keyedValue", "variableName", "Hourly", "", "variableUnits"},
		{"12", "0", "Sum", "indexGroup", "HVAC System", "keyedValue", "variableName", "Hourly", "", "variableUnits"},
		{"13", "0", "Avg", "indexGroup", "HVAC System", "keyedValue", "variableName", "Hourly", "scheduleName", "variableUnits"},
		{"14", "0", "Avg", "indexGroup", "Zone", "keyedValue", "variableName", "Hourly", "", "variableUnits"},
		{"15", "0", "Avg", "indexGroup", "Unknown!!!", "keyedValue", "variableName", "Hourly", "", "variableUnits"},
		{"16", "0", "Avg", "indexGroup", "HVAC System", "keyedValue", "variableName", "Daily", "", "variableUnits"},
		{"17", "0", "Sum", "indexGroup", "HVAC System", "keyedValue", "variableName", "Daily", "", "variableUnits"},
		{"18", "0", "Avg", "indexGroup", "HVAC System", "keyedValue", "variableName", "Daily", "scheduleName", "variableUnits"},
		{"19", "0", "Avg", "indexGroup", "Zone", "keyedValue", "variableName", "Daily", "", "variableUnits"},
		{"20", "0", "Avg", "indexGroup", "Unknown!!!", "keyedValue", "variableName", "Daily", "", "variableUnits"},
		{"21", "0", "Avg", "indexGroup", "HVAC System", "keyedValue", "variableName", "Monthly", "", "variableUnits"},
		{"22", "0", "Sum", "indexGroup", "HVAC System", "keyedValue", "variableName", "Monthly", "", "variableUnits"},
		{"23", "0", "Avg", "indexGroup", "HVAC System", "keyedValue", "variableName", "Monthly", "scheduleName", "variableUnits"},
		{"24", "0", "Avg", "indexGroup", "Zone", "keyedValue", "variableName", "Monthly", "", "variableUnits"},
		{"25", "0", "Avg", "indexGroup", "Unknown!!!", "keyedValue", "variableName", "Monthly", "", "variableUnits"},
		{"26", "0", "Avg", "indexGroup", "HVAC System", "keyedValue", "variableName", "Run Period", "", "variableUnits"},
		{"27", "0", "Sum", "indexGroup", "HVAC System", "keyedValue", "variableName", "Run Period", "", "variableUnits"},
		{"28", "0", "Avg", "indexGroup", "HVAC System", "keyedValue", "variableName", "Run Period", "scheduleName", "variableUnits"},
		{"29", "0", "Avg", "indexGroup", "Zone", "keyedValue", "variableName", "Run Period", "", "variableUnits"},
		{"30", "0", "Avg", "indexGroup", "Unknown!!!", "keyedValue", "variableName", "Run Period", "", "variableUnits"}
	});
	ASSERT_EQ( reportDataDictionary.size(), reportDataDictionaryResults.size() );
	for (size_t i = 0; i < reportDataDictionaryResults.size(); ++i)
	{
		EXPECT_EQ( reportDataDictionary[i], reportDataDictionaryResults[i] );
	}

	// This should be all necessary clean up...
	{ IOFlags flags; flags.DISPOSE( "DELETE" ); gio::close( OutputFileMeterDetails, flags ); }
	RVariableTypes.deallocate();
	IVariableTypes.deallocate();
	ReportList.deallocate();
	EndUseCategory.deallocate();

}

TEST_F( SQLiteFixture, writeCumulativeReportMeterData )
{
	ShowMessage( "Begin Test: OutputProcessor, writeCumulativeReportMeterData" );

	std::unique_ptr<std::ostringstream> eso_strm(new std::ostringstream);
	std::unique_ptr<std::ostringstream> mtr_strm(new std::ostringstream);

	DataGlobals::eso_stream = eso_strm.get();
	DataGlobals::mtr_stream = mtr_strm.get();

	sqlite_test->createSQLiteTimeIndexRecord( 4, 1, 1, 0 );
	sqlite_test->createSQLiteReportDictionaryRecord( 1, 1, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false, _ );

	EnergyPlus::sqlite = std::move( sqlite_test );

	WriteCumulativeReportMeterData( 1, "1", 616771620.98702729, true );
	EXPECT_EQ( delimitedString( { "1,616771620.987027" } ), mtr_strm->str() );
	mtr_strm->str(std::string());

	WriteCumulativeReportMeterData( 1, "1", 616771620.98702729, false );
	EXPECT_EQ( delimitedString( { "1,616771620.987027" } ), eso_strm->str() );
	EXPECT_EQ( delimitedString( { "1,616771620.987027" } ), mtr_strm->str() );
	eso_strm->str(std::string());
	mtr_strm->str(std::string());

	WriteCumulativeReportMeterData( 1, "1", 0, true );
	EXPECT_EQ( delimitedString( { "1,0.0" } ), mtr_strm->str() );
	mtr_strm->str(std::string());

	WriteCumulativeReportMeterData( 1, "1", 0, false );
	EXPECT_EQ( delimitedString( { "1,0.0" } ), eso_strm->str() );
	EXPECT_EQ( delimitedString( { "1,0.0" } ), mtr_strm->str() );
	eso_strm->str(std::string());
	mtr_strm->str(std::string());

	sqlite_test = std::move( EnergyPlus::sqlite );

	auto reportData = queryResult("SELECT * FROM ReportData;", "ReportData");
	auto reportExtendedData = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

	ASSERT_EQ(4ul, reportData.size());
	std::vector<std::string> reportData0 {"1", "1", "1", "616771620.987027"};
	std::vector<std::string> reportData1 {"2", "1", "1", "616771620.987027"};
	std::vector<std::string> reportData2 {"3", "1", "1", "0.0"};
	std::vector<std::string> reportData3 {"4", "1", "1", "0.0"};
	EXPECT_EQ(reportData0, reportData[0]);
	EXPECT_EQ(reportData1, reportData[1]);
	EXPECT_EQ(reportData2, reportData[2]);
	EXPECT_EQ(reportData3, reportData[3]);

	ASSERT_EQ(0ul, reportExtendedData.size());

}

TEST_F( SQLiteFixture, writeRealData )
{
	ShowMessage( "Begin Test: OutputProcessor, writeRealData" );

	std::unique_ptr<std::ostringstream> eso_strm(new std::ostringstream);

	DataGlobals::eso_stream = eso_strm.get();

	sqlite_test->createSQLiteTimeIndexRecord( 4, 1, 1, 0 );
	sqlite_test->createSQLiteReportDictionaryRecord( 1, 1, "Zone", "Environment", "Site Outdoor Air Drybulb Temperature", 1, "C", 1, false, _ );

	EnergyPlus::sqlite = std::move( sqlite_test );

	WriteRealData( 1, "1", 0 );
	EXPECT_EQ( delimitedString( { "1,0.0" } ), eso_strm->str() ) << "This might fail due to floating point, revisit if happens regularly.";
	eso_strm->str(std::string());

	WriteRealData( 1, "1", 0.1 );
	EXPECT_EQ( delimitedString( { "1,0.1" } ), eso_strm->str() ) << "This might fail due to floating point, revisit if happens regularly.";
	eso_strm->str(std::string());

	WriteRealData( 1, "1", -0.1 );
	EXPECT_EQ( delimitedString( { "1,-0.1" } ), eso_strm->str() ) << "This might fail due to floating point, revisit if happens regularly.";
	eso_strm->str(std::string());

	WriteRealData( 1, "1", 1.0e-2 );
	EXPECT_EQ( delimitedString( { "1,1.000000000000000E-02" } ), eso_strm->str() ) << "This might fail due to floating point, revisit if happens regularly.";
	eso_strm->str(std::string());

	WriteRealData( 1, "1", 1.0e-3 );
	EXPECT_EQ( delimitedString( { "1,1.000000000000000E-03" } ), eso_strm->str() ) << "This might fail due to floating point, revisit if happens regularly.";
	eso_strm->str(std::string());

	WriteRealData( 1, "1", 1.0e-4 );
	EXPECT_EQ( delimitedString( { "1,1.000000000000000E-04" } ), eso_strm->str() ) << "This might fail due to floating point, revisit if happens regularly.";
	eso_strm->str(std::string());

	WriteRealData( 1, "1", 1.0e-5 );
	EXPECT_EQ( delimitedString( { "1,1.000000000000000E-05" } ), eso_strm->str() ) << "This might fail due to floating point, revisit if happens regularly.";
	eso_strm->str(std::string());

	WriteRealData( 1, "1", 1.0e-6 );
	EXPECT_EQ( delimitedString( { "1,1.000000000000000E-06" } ), eso_strm->str() ) << "This might fail due to floating point, revisit if happens regularly.";
	eso_strm->str(std::string());

	WriteRealData( 1, "1", 1.0e-7 );
	EXPECT_EQ( delimitedString( { "1,1.000000000000000E-07" } ), eso_strm->str() ) << "This might fail due to floating point, revisit if happens regularly.";
	eso_strm->str(std::string());

	WriteRealData( 1, "1", 1.0e-8 );
	EXPECT_EQ( delimitedString( { "1,1.000000000000000E-08" } ), eso_strm->str() ) << "This might fail due to floating point, revisit if happens regularly.";
	eso_strm->str(std::string());

	WriteRealData( 1, "1", 1.0e-9 );
	EXPECT_EQ( delimitedString( { "1,1.000000000000000E-09" } ), eso_strm->str() ) << "This might fail due to floating point, revisit if happens regularly.";
	eso_strm->str(std::string());

	WriteRealData( 1, "1", 1.0e-10 );
	EXPECT_EQ( delimitedString( { "1,1.000000000000000E-10" } ), eso_strm->str() ) << "This might fail due to floating point, revisit if happens regularly.";
	eso_strm->str(std::string());

	WriteRealData( 1, "1", 1.0e-11 );
	// this seems to always be low... not 1.0e-11
	EXPECT_EQ( delimitedString( { "1,9.999999999999999E-12" } ), eso_strm->str() ) << "This might fail due to floating point, revisit if happens regularly.";
	eso_strm->str(std::string());

	WriteRealData( 1, "1", 1.0e-12 );
	EXPECT_EQ( delimitedString( { "1,1.000000000000000E-12" } ), eso_strm->str() ) << "This might fail due to floating point, revisit if happens regularly.";
	eso_strm->str(std::string());

	WriteRealData( 1, "1", 1.0e-13 );
	EXPECT_EQ( delimitedString( { "1,1.000000000000000E-13" } ), eso_strm->str() ) << "This might fail due to floating point, revisit if happens regularly.";
	eso_strm->str(std::string());

	WriteRealData( 1, "1", 1.0e-14 );
	EXPECT_EQ( delimitedString( { "1,1.000000000000000E-14" } ), eso_strm->str() ) << "This might fail due to floating point, revisit if happens regularly.";
	eso_strm->str(std::string());

	WriteRealData( 1, "1", 1.0e-15 );
	EXPECT_EQ( delimitedString( { "1,1.000000000000000E-15" } ), eso_strm->str() ) << "This might fail due to floating point, revisit if happens regularly.";
	eso_strm->str(std::string());

	WriteRealData( 1, "1", 1.0e-16 );
	EXPECT_EQ( delimitedString( { "1,1.000000000000000E-16" } ), eso_strm->str() ) << "This might fail due to floating point, revisit if happens regularly.";
	eso_strm->str(std::string());

	WriteRealData( 1, "1", -1.0e-16 );
	EXPECT_EQ( delimitedString( { "1,-1.000000000000000E-16" } ), eso_strm->str() ) << "This might fail due to floating point, revisit if happens regularly.";
	eso_strm->str(std::string());

	WriteRealData( 1, "1", 1.0e-19 );
	EXPECT_EQ( delimitedString( { "1,1.000000000000000E-19" } ), eso_strm->str() ) << "This might fail due to floating point, revisit if happens regularly.";
	eso_strm->str(std::string());

	WriteRealData( 1, "1", 0.5 );
	EXPECT_EQ( delimitedString( { "1,0.5" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteRealData( 1, "1", 1.0 );
	EXPECT_EQ( delimitedString( { "1,1." } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteRealData( 1, "1", 10.0 );
	EXPECT_EQ( delimitedString( { "1,10." } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteRealData( 1, "1", 1.0e2 );
	EXPECT_EQ( delimitedString( { "1,100." } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteRealData( 1, "1", 1.0e3 );
	EXPECT_EQ( delimitedString( { "1,1000." } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteRealData( 1, "1", 1.0e4 );
	EXPECT_EQ( delimitedString( { "1,10000." } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteRealData( 1, "1", 1.0e5 );
	EXPECT_EQ( delimitedString( { "1,100000." } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteRealData( 1, "1", 1.0e6 );
	EXPECT_EQ( delimitedString( { "1,1000000." } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteRealData( 1, "1", 1.0e7 );
	EXPECT_EQ( delimitedString( { "1,10000000." } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteRealData( 1, "1", 1.0e8 );
	EXPECT_EQ( delimitedString( { "1,100000000." } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteRealData( 1, "1", 1.0e9 );
	EXPECT_EQ( delimitedString( { "1,1000000000." } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteRealData( 1, "1", 1.0e10 );
	EXPECT_EQ( delimitedString( { "1,10000000000." } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteRealData( 1, "1", 1.0e11 );
	EXPECT_EQ( delimitedString( { "1,100000000000." } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteRealData( 1, "1", 1.0e12 );
	EXPECT_EQ( delimitedString( { "1,1000000000000." } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteRealData( 1, "1", 1.0e13 );
	EXPECT_EQ( delimitedString( { "1,10000000000000." } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteRealData( 1, "1", 1.0e14 );
	EXPECT_EQ( delimitedString( { "1,100000000000000" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteRealData( 1, "1", 1.0e15 );
	EXPECT_EQ( delimitedString( { "1,1000000000000000" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteRealData( 1, "1", 1.0e16 );
	EXPECT_EQ( delimitedString( { "1,10000000000000000" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteRealData( 1, "1", 1.0e17 );
	EXPECT_EQ( delimitedString( { "1,1.000000000000000E+17" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteRealData( 1, "1", -1.0e16 );
	EXPECT_EQ( delimitedString( { "1,-10000000000000000" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteRealData( 1, "1", -1.0e17 );
	EXPECT_EQ( delimitedString( { "1,-1.000000000000000E+17" } ), eso_strm->str() );
	eso_strm->str(std::string());

	WriteRealData( 1, "1", 1.0e25 );
	EXPECT_EQ( delimitedString( { "1,1.000000000000000E+25" } ), eso_strm->str() );
	eso_strm->str(std::string());

	sqlite_test = std::move( EnergyPlus::sqlite );

	auto reportDataResults = queryResult("SELECT * FROM ReportData;", "ReportData");
	auto reportExtendedDataResults = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

	std::vector< std::vector<std::string> > reportData(
	{
		{ "1", "1", "1", "0.0" },
		{ "2", "1", "1", "0.1" },
		{ "3", "1", "1", "-0.1" },
		{ "4", "1", "1", "0.01" },
		{ "5", "1", "1", "0.001" },
		{ "6", "1", "1", "0.0001" },
		{ "7", "1", "1", "1.0e-05" },
		{ "8", "1", "1", "1.0e-06" },
		{ "9", "1", "1", "1.0e-07" },
		{ "10", "1", "1", "1.0e-08" },
		{ "11", "1", "1", "1.0e-09" },
		{ "12", "1", "1", "1.0e-10" },
		{ "13", "1", "1", "1.0e-11" },
		{ "14", "1", "1", "1.0e-12" },
		{ "15", "1", "1", "1.0e-13" },
		{ "16", "1", "1", "1.0e-14" },
		{ "17", "1", "1", "1.0e-15" },
		{ "18", "1", "1", "1.0e-16" },
		{ "19", "1", "1", "-1.0e-16" },
		{ "20", "1", "1", "1.0e-19" },
		{ "21", "1", "1", "0.5" },
		{ "22", "1", "1", "1.0" },
		{ "23", "1", "1", "10.0" },
		{ "24", "1", "1", "100.0" },
		{ "25", "1", "1", "1000.0" },
		{ "26", "1", "1", "10000.0" },
		{ "27", "1", "1", "100000.0" },
		{ "28", "1", "1", "1000000.0" },
		{ "29", "1", "1", "10000000.0" },
		{ "30", "1", "1", "100000000.0" },
		{ "31", "1", "1", "1000000000.0" },
		{ "32", "1", "1", "10000000000.0" },
		{ "33", "1", "1", "100000000000.0" },
		{ "34", "1", "1", "1000000000000.0" },
		{ "35", "1", "1", "10000000000000.0" },
		{ "36", "1", "1", "100000000000000.0" },
		{ "37", "1", "1", "1.0e+15" },
		{ "38", "1", "1", "1.0e+16" },
		{ "39", "1", "1", "1.0e+17" },
		{ "40", "1", "1", "-1.0e+16" },
		{ "41", "1", "1", "-1.0e+17" },
		{ "42", "1", "1", "1.0e+25" }
	});

	std::vector< std::vector<std::string> > reportExtendedData({});

	ASSERT_EQ( reportData.size(), reportDataResults.size() );
	for (size_t i = 0; i < reportDataResults.size(); ++i)
	{
		EXPECT_EQ( reportData[i], reportDataResults[i] );
	}

	ASSERT_EQ( reportExtendedData.size(), reportExtendedDataResults.size() );

}
