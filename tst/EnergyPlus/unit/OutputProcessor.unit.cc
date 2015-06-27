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

	auto reportData = queryResult("SELECT * FROM ReportData;", "ReportData");
	auto reportExtendedData = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

	ASSERT_EQ(2ul, reportData.size());
	std::vector<std::string> reportData0 {"1", "1", "1", "999.9"};
	std::vector<std::string> reportData1 {"2", "1", "2", "9999.9"};
	EXPECT_EQ(reportData0, reportData[0]);
	EXPECT_EQ(reportData1, reportData[1]);

	ASSERT_EQ(0ul, reportExtendedData.size());

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

	auto reportData = queryResult("SELECT * FROM ReportData;", "ReportData");
	auto reportExtendedData = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

	ASSERT_EQ(2ul, reportData.size());
	std::vector<std::string> reportData0 {"1", "1", "1", "999.9"};
	std::vector<std::string> reportData1 {"2", "1", "2", "9999.9"};
	EXPECT_EQ(reportData0, reportData[0]);
	EXPECT_EQ(reportData1, reportData[1]);

	ASSERT_EQ(0ul, reportExtendedData.size());

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

	auto reportData = queryResult("SELECT * FROM ReportData;", "ReportData");
	auto reportExtendedData = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

	ASSERT_EQ(2ul, reportData.size());
	std::vector<std::string> reportData0 {"1", "1", "1", "999.9"};
	std::vector<std::string> reportData1 {"2", "1", "2", "9999.9"};
	EXPECT_EQ(reportData0, reportData[0]);
	EXPECT_EQ(reportData1, reportData[1]);

	ASSERT_EQ(0ul, reportExtendedData.size());

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

	auto reportData = queryResult("SELECT * FROM ReportData;", "ReportData");
	auto reportExtendedData = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

	ASSERT_EQ(2ul, reportData.size());
	std::vector<std::string> reportData0 {"1", "1", "1", "999.9"};
	std::vector<std::string> reportData1 {"2", "1", "2", "9999.9"};
	EXPECT_EQ(reportData0, reportData[0]);
	EXPECT_EQ(reportData1, reportData[1]);

	ASSERT_EQ(2ul, reportExtendedData.size());
	std::vector<std::string> reportExtendedData0 {"1","1","4283136.25248438","12","21","1","1","0","4283136.25168393","12","21","0","11","10"};
	std::vector<std::string> reportExtendedData1 {"2","2","4283136.25248438","12","21","1","1","0","4283136.25168393","12","21","0","11","10"};
	EXPECT_EQ(reportExtendedData0, reportExtendedData[0]);
	EXPECT_EQ(reportExtendedData1, reportExtendedData[1]);

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

	auto reportData = queryResult("SELECT * FROM ReportData;", "ReportData");
	auto reportExtendedData = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

	ASSERT_EQ(2ul, reportData.size());
	std::vector<std::string> reportData0 {"1", "1", "1", "999.9"};
	std::vector<std::string> reportData1 {"2", "1", "2", "9999.9"};
	EXPECT_EQ(reportData0, reportData[0]);
	EXPECT_EQ(reportData1, reportData[1]);

	ASSERT_EQ(2ul, reportExtendedData.size());
	std::vector<std::string> reportExtendedData0 {"1","1","4283136.25248438","12","21","1","1","0","4283136.25168393","12","21","0","11","10"};
	std::vector<std::string> reportExtendedData1 {"2","2","4283136.25248438","12","21","1","1","0","4283136.25168393","12","21","0","11","10"};
	EXPECT_EQ(reportExtendedData0, reportExtendedData[0]);
	EXPECT_EQ(reportExtendedData1, reportExtendedData[1]);

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

	auto reportData = queryResult("SELECT * FROM ReportData;", "ReportData");
	auto reportExtendedData = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

	ASSERT_EQ(2ul, reportData.size());
	std::vector<std::string> reportData0 {"1", "1", "1", "999.9"};
	std::vector<std::string> reportData1 {"2", "1", "2", "9999.9"};
	EXPECT_EQ(reportData0, reportData[0]);
	EXPECT_EQ(reportData1, reportData[1]);

	ASSERT_EQ(2ul, reportExtendedData.size());
	std::vector<std::string> reportExtendedData0 {"1","1","4283136.25248438","12","21","1","1","0","4283136.25168393","12","21","0","11","10"};
	std::vector<std::string> reportExtendedData1 {"2","2","4283136.25248438","12","21","1","1","0","4283136.25168393","12","21","0","11","10"};
	EXPECT_EQ(reportExtendedData0, reportExtendedData[0]);
	EXPECT_EQ(reportExtendedData1, reportExtendedData[1]);

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

	auto result = queryResult("SELECT * FROM Time;", "Time");

	ASSERT_EQ(6ul, result.size());

	std::vector<std::string> testResult0 {"1", "12", "21", "0", "10", "0", "10", "0", "1", "WinterDesignDay", "0", "0"};
	std::vector<std::string> testResult1 {"2", "12", "21", "0", "10", "0", "10", "-1", "1", "WinterDesignDay", "0", "0"};
	std::vector<std::string> testResult2 {"3", "12", "21", "1", "0", "0", "60", "1", "1", "WinterDesignDay", "0", "0"};
	std::vector<std::string> testResult3 {"4", "12", "21", "24", "0", "0", "1440", "2", "1", "WinterDesignDay", "0", "0"};
	std::vector<std::string> testResult4 {"5", "12", "31", "24", "0", "", "44640", "3", "1", "", "0", "0"};
	std::vector<std::string> testResult5 {"6", "", "", "", "", "", "1440", "4", "1", "", "0", "0"};
	EXPECT_EQ( testResult0, result[0] );
	EXPECT_EQ( testResult1, result[1] );
	EXPECT_EQ( testResult2, result[2] );
	EXPECT_EQ( testResult3, result[3] );
	EXPECT_EQ( testResult4, result[4] );
	EXPECT_EQ( testResult5, result[5] );

}

TEST_F( SQLiteFixture, writeReportMeterData )
{
	ShowMessage( "Begin Test: OutputProcessor, writeReportMeterData" );

	std::unique_ptr<std::ostringstream> eso_strm(new std::ostringstream);
	std::unique_ptr<std::ostringstream> mtr_strm(new std::ostringstream);

	DataGlobals::eso_stream = eso_strm.get();
	DataGlobals::mtr_stream = mtr_strm.get();

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

	WriteReportMeterData( 1, "1", 999.9, ReportHourly, 0.0, 0, 0.0, 0, false );
	EXPECT_EQ( delimitedString( { "1,999.9" } ), eso_strm->str() );
	EXPECT_EQ( delimitedString( { "1,999.9" } ), mtr_strm->str() );
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

	sqlite_test = std::move( EnergyPlus::sqlite );

	auto reportData = queryResult("SELECT * FROM ReportData;", "ReportData");
	auto reportExtendedData = queryResult("SELECT * FROM ReportExtendedData;", "ReportExtendedData");

	ASSERT_EQ(12ul, reportData.size());
	std::vector<std::string> reportData0 {"1", "1", "1", "999.9"};
	std::vector<std::string> reportData1 {"2", "1", "1", "999.9"};
	std::vector<std::string> reportData2 {"3", "1", "1", "999.9"};
	std::vector<std::string> reportData3 {"4", "1", "1", "616771620.987027"};
	std::vector<std::string> reportData4 {"5", "1", "1", "616771620.987027"};
	std::vector<std::string> reportData5 {"6", "1", "1", "616771620.987027"};
	std::vector<std::string> reportData6 {"7", "1", "1", "616771620.987027"};
	std::vector<std::string> reportData7 {"8", "1", "1", "616771620.987027"};
	std::vector<std::string> reportData8 {"9", "1", "1", "616771620.987027"};
	std::vector<std::string> reportData9 {"10", "1", "1", "616771620.987027"};
	std::vector<std::string> reportData10 {"11", "1", "1", "616771620.987027"};
	std::vector<std::string> reportData11 {"12", "1", "1", "616771620.987027"};
	EXPECT_EQ(reportData0, reportData[0]);
	EXPECT_EQ(reportData1, reportData[1]);
	EXPECT_EQ(reportData2, reportData[2]);
	EXPECT_EQ(reportData3, reportData[3]);
	EXPECT_EQ(reportData4, reportData[4]);
	EXPECT_EQ(reportData5, reportData[5]);
	EXPECT_EQ(reportData6, reportData[6]);
	EXPECT_EQ(reportData7, reportData[7]);
	EXPECT_EQ(reportData8, reportData[8]);
	EXPECT_EQ(reportData9, reportData[9]);
	EXPECT_EQ(reportData10, reportData[10]);
	EXPECT_EQ(reportData11, reportData[11]);

	ASSERT_EQ(7ul, reportExtendedData.size());

	std::vector<std::string> reportExtendedData0 {"1","4","4283136.25872118","12","21","24","1","0","4283136.25168393","12","21","0","11","10"};
	std::vector<std::string> reportExtendedData1 {"2","5","4283136.25872118","12","21","24","1","0","4283136.25168393","12","21","0","11","10"};
	std::vector<std::string> reportExtendedData2 {"3","6","4283136.25872118","12","21","24","1","0","4283136.25168393","12","21","0","11","10"};
	std::vector<std::string> reportExtendedData3 {"4","9","4283136.25872118","12","21","24","1","0","4283136.25168393","12","21","0","11","10"};
	std::vector<std::string> reportExtendedData4 {"5","10","4283136.25872118","12","21","24","1","0","4283136.25168393","12","21","0","11","10"};
	std::vector<std::string> reportExtendedData5 {"6","11","4283136.25872118","12","21","24","1","0","4283136.25168393","12","21","0","11","10"};
	std::vector<std::string> reportExtendedData6 {"7","12","4283136.25872118","12","21","24","1","0","4283136.25168393","12","21","0","11","10"};
	EXPECT_EQ(reportExtendedData0, reportExtendedData[0]);
	EXPECT_EQ(reportExtendedData1, reportExtendedData[1]);
	EXPECT_EQ(reportExtendedData2, reportExtendedData[2]);
	EXPECT_EQ(reportExtendedData3, reportExtendedData[3]);
	EXPECT_EQ(reportExtendedData4, reportExtendedData[4]);
	EXPECT_EQ(reportExtendedData5, reportExtendedData[5]);
	EXPECT_EQ(reportExtendedData6, reportExtendedData[6]);

}

TEST_F ( SQLiteFixture, getStandardMeterResourceType )
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

	sqlite_test->createSQLiteSimulationsRecord( 1, "EnergyPlus Version", "Current Time" );

	std::string out_resource_type;
	bool error_found = false;

	for( auto const & meterType : resource_map ) {
		GetStandardMeterResourceType( out_resource_type, meterType.first, error_found );
		EXPECT_EQ( meterType.second, out_resource_type );
		EXPECT_FALSE( error_found );
	}

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

TEST_F ( SQLiteFixture, writeMeterDictionaryItem )
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

	auto reportDataDictionary = queryResult("SELECT * FROM ReportDataDictionary;", "ReportDataDictionary");

	ASSERT_EQ(30ul, reportDataDictionary.size());
	std::vector<std::string> reportDataDictionary0 {"1", "1", "Avg", "indexGroup", "HVAC System", "", "meterName", "Zone Timestep", "", "meterUnits"};
	std::vector<std::string> reportDataDictionary1 {"2", "1", "Sum", "indexGroup", "HVAC System", "", "meterName", "Zone Timestep", "", "meterUnits"};
	std::vector<std::string> reportDataDictionary2 {"3", "1", "Avg", "indexGroup", "HVAC System", "Cumulative ", "meterName", "Zone Timestep", "", "meterUnits"};
	std::vector<std::string> reportDataDictionary3 {"4", "1", "Avg", "indexGroup", "HVAC System", "", "meterName", "Zone Timestep", "", "meterUnits"};
	std::vector<std::string> reportDataDictionary4 {"5", "1", "Avg", "indexGroup", "HVAC System", "Cumulative ", "meterName", "Zone Timestep", "", "meterUnits"};
	std::vector<std::string> reportDataDictionary5 {"6", "1", "Avg", "indexGroup", "HVAC System", "", "meterName", "HVAC System Timestep", "", "meterUnits"};
	std::vector<std::string> reportDataDictionary6 {"7", "1", "Sum", "indexGroup", "HVAC System", "", "meterName", "HVAC System Timestep", "", "meterUnits"};
	std::vector<std::string> reportDataDictionary7 {"8", "1", "Avg", "indexGroup", "HVAC System", "Cumulative ", "meterName", "HVAC System Timestep", "", "meterUnits"};
	std::vector<std::string> reportDataDictionary8 {"9", "1", "Avg", "indexGroup", "HVAC System", "", "meterName", "HVAC System Timestep", "", "meterUnits"};
	std::vector<std::string> reportDataDictionary9 {"10", "1", "Avg", "indexGroup", "HVAC System", "Cumulative ", "meterName", "HVAC System Timestep", "", "meterUnits"};
	std::vector<std::string> reportDataDictionary10 {"11", "1", "Avg", "indexGroup", "HVAC System", "", "meterName", "Hourly", "", "meterUnits"};
	std::vector<std::string> reportDataDictionary11 {"12", "1", "Sum", "indexGroup", "HVAC System", "", "meterName", "Hourly", "", "meterUnits"};
	std::vector<std::string> reportDataDictionary12 {"13", "1", "Avg", "indexGroup", "HVAC System", "Cumulative ", "meterName", "Hourly", "", "meterUnits"};
	std::vector<std::string> reportDataDictionary13 {"14", "1", "Avg", "indexGroup", "HVAC System", "", "meterName", "Hourly", "", "meterUnits"};
	std::vector<std::string> reportDataDictionary14 {"15", "1", "Avg", "indexGroup", "HVAC System", "Cumulative ", "meterName", "Hourly", "", "meterUnits"};
	std::vector<std::string> reportDataDictionary15 {"16", "1", "Avg", "indexGroup", "HVAC System", "", "meterName", "Daily", "", "meterUnits"};
	std::vector<std::string> reportDataDictionary16 {"17", "1", "Sum", "indexGroup", "HVAC System", "", "meterName", "Daily", "", "meterUnits"};
	std::vector<std::string> reportDataDictionary17 {"18", "1", "Avg", "indexGroup", "HVAC System", "Cumulative ", "meterName", "Daily", "", "meterUnits"};
	std::vector<std::string> reportDataDictionary18 {"19", "1", "Avg", "indexGroup", "HVAC System", "", "meterName", "Daily", "", "meterUnits"};
	std::vector<std::string> reportDataDictionary19 {"20", "1", "Avg", "indexGroup", "HVAC System", "Cumulative ", "meterName", "Daily", "", "meterUnits"};
	std::vector<std::string> reportDataDictionary20 {"21", "1", "Avg", "indexGroup", "HVAC System", "", "meterName", "Monthly", "", "meterUnits"};
	std::vector<std::string> reportDataDictionary21 {"22", "1", "Sum", "indexGroup", "HVAC System", "", "meterName", "Monthly", "", "meterUnits"};
	std::vector<std::string> reportDataDictionary22 {"23", "1", "Avg", "indexGroup", "HVAC System", "Cumulative ", "meterName", "Monthly", "", "meterUnits"};
	std::vector<std::string> reportDataDictionary23 {"24", "1", "Avg", "indexGroup", "HVAC System", "", "meterName", "Monthly", "", "meterUnits"};
	std::vector<std::string> reportDataDictionary24 {"25", "1", "Avg", "indexGroup", "HVAC System", "Cumulative ", "meterName", "Monthly", "", "meterUnits"};
	std::vector<std::string> reportDataDictionary25 {"26", "1", "Avg", "indexGroup", "HVAC System", "", "meterName", "Run Period", "", "meterUnits"};
	std::vector<std::string> reportDataDictionary26 {"27", "1", "Sum", "indexGroup", "HVAC System", "", "meterName", "Run Period", "", "meterUnits"};
	std::vector<std::string> reportDataDictionary27 {"28", "1", "Avg", "indexGroup", "HVAC System", "Cumulative ", "meterName", "Run Period", "", "meterUnits"};
	std::vector<std::string> reportDataDictionary28 {"29", "1", "Avg", "indexGroup", "HVAC System", "", "meterName", "Run Period", "", "meterUnits"};
	std::vector<std::string> reportDataDictionary29 {"30", "1", "Avg", "indexGroup", "HVAC System", "Cumulative ", "meterName", "Run Period", "", "meterUnits"};
	EXPECT_EQ(reportDataDictionary0, reportDataDictionary[0]);
	EXPECT_EQ(reportDataDictionary1, reportDataDictionary[1]);
	EXPECT_EQ(reportDataDictionary2, reportDataDictionary[2]);
	EXPECT_EQ(reportDataDictionary3, reportDataDictionary[3]);
	EXPECT_EQ(reportDataDictionary4, reportDataDictionary[4]);
	EXPECT_EQ(reportDataDictionary5, reportDataDictionary[5]);
	EXPECT_EQ(reportDataDictionary6, reportDataDictionary[6]);
	EXPECT_EQ(reportDataDictionary7, reportDataDictionary[7]);
	EXPECT_EQ(reportDataDictionary8, reportDataDictionary[8]);
	EXPECT_EQ(reportDataDictionary9, reportDataDictionary[9]);
	EXPECT_EQ(reportDataDictionary10, reportDataDictionary[10]);
	EXPECT_EQ(reportDataDictionary11, reportDataDictionary[11]);
	EXPECT_EQ(reportDataDictionary12, reportDataDictionary[12]);
	EXPECT_EQ(reportDataDictionary13, reportDataDictionary[13]);
	EXPECT_EQ(reportDataDictionary14, reportDataDictionary[14]);
	EXPECT_EQ(reportDataDictionary15, reportDataDictionary[15]);
	EXPECT_EQ(reportDataDictionary16, reportDataDictionary[16]);
	EXPECT_EQ(reportDataDictionary17, reportDataDictionary[17]);
	EXPECT_EQ(reportDataDictionary18, reportDataDictionary[18]);
	EXPECT_EQ(reportDataDictionary19, reportDataDictionary[19]);
	EXPECT_EQ(reportDataDictionary20, reportDataDictionary[20]);
	EXPECT_EQ(reportDataDictionary21, reportDataDictionary[21]);
	EXPECT_EQ(reportDataDictionary22, reportDataDictionary[22]);
	EXPECT_EQ(reportDataDictionary23, reportDataDictionary[23]);
	EXPECT_EQ(reportDataDictionary24, reportDataDictionary[24]);
	EXPECT_EQ(reportDataDictionary25, reportDataDictionary[25]);
	EXPECT_EQ(reportDataDictionary26, reportDataDictionary[26]);
	EXPECT_EQ(reportDataDictionary27, reportDataDictionary[27]);
	EXPECT_EQ(reportDataDictionary28, reportDataDictionary[28]);
	EXPECT_EQ(reportDataDictionary29, reportDataDictionary[29]);

	// This should be all necessary clean up...
	{ IOFlags flags; flags.DISPOSE( "DELETE" ); gio::close( OutputFileMeterDetails, flags ); }
	RVariableTypes.deallocate();
	IVariableTypes.deallocate();
	ReportList.deallocate();
	EndUseCategory.deallocate();

}
