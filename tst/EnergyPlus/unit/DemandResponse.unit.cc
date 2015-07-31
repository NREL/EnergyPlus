// EnergyPlus::DemandManager ventilation test

// Google test headers
#include <gtest/gtest.h>

#include <UtilityRoutines.hh>
#include <DataGlobals.hh>
#include <InputProcessor.hh>
#include <DataIPShortCuts.hh>
#include <MixedAir.hh>
#include <DemandManager.hh>

using namespace EnergyPlus;
using namespace ObjexxFCL;
using namespace EnergyPlus::DataGlobals;
using namespace EnergyPlus::InputProcessor;
using namespace EnergyPlus::DataIPShortCuts;
using namespace EnergyPlus::MixedAir;
using namespace EnergyPlus::DemandManager;

TEST(DemandResponseTest, Test1)
{
	ShowMessage("Begin Test: DemandResponseTest, Input Data Test");

	// Test input processing for DemandManager:Ventilation

	int NumOfOAControllers( 1 );
	int NumIDDAlphas( 14 );
	int NumIDDNumbers( 5 );
	int NumIDDNumeric( 14 );
	int NumIDFAlphas( 5 );
	int NumIDFNumbers( 5 );

	OAController.allocate( NumOfOAControllers );

	OAController(1).Name = "OA Controller 1";
	NumOAControllers = 1;

	NumObjectDefs = 1;
	ListOfObjects.allocate( NumObjectDefs );
	ListOfObjects( NumObjectDefs ) = "DEMANDMANAGER:VENTILATION";
	iListOfObjects.allocate( NumObjectDefs );
	iListOfObjects( NumObjectDefs ) = 1;
	ObjectStartRecord.allocate( NumObjectDefs );
	ObjectStartRecord( NumObjectDefs ) = 1;
	ObjectGotCount.allocate( NumObjectDefs );
	IDFRecordsGotten.allocate( NumObjectDefs );
	IDFRecordsGotten( NumObjectDefs ) = false;

	// Define IDD object for DemandManager:Ventilation
	ObjectDef.allocate( NumObjectDefs );
	ObjectDef( 1 ).NumFound = 1;
	ObjectDef( 1 ).NumParams = NumIDDAlphas + NumIDDNumbers;
	ObjectDef( 1 ).NumAlpha = NumIDDAlphas;
	ObjectDef( 1 ).NumNumeric = NumIDFNumbers;
	ObjectDef( 1 ).AlphFieldChks.allocate( NumIDDAlphas );
	ObjectDef( 1 ).AlphFieldChks = " ";
	ObjectDef( 1 ).NumRangeChks.allocate( NumIDDNumbers );

	// Define IDF object for DemandManager:Ventilation
	NumIDFRecords = 1;
	IDFRecords.allocate( NumIDFRecords );
	IDFRecords( 1 ).Name = ListOfObjects( 1 );
	IDFRecords( 1 ).NumNumbers = NumIDFNumbers;
	IDFRecords( 1 ).NumAlphas = NumIDFAlphas;
	IDFRecords( 1 ).ObjectDefPtr = ObjectDef( 1 ).NumFound;
	IDFRecords( 1 ).Alphas.allocate( NumIDFAlphas );
	IDFRecords( 1 ).Alphas( 1 ) = "Ventilation Manager";
	IDFRecords( 1 ).Alphas( 2 ) = "";
	IDFRecords( 1 ).Alphas( 3 ) = "FIXEDRATE";
	IDFRecords( 1 ).Alphas( 4 ) = "ALL";
	IDFRecords( 1 ).Alphas( 5 ) = "OA Controller 1";
	IDFRecords( 1 ).AlphBlank.allocate( NumIDFAlphas );
	IDFRecords( 1 ).AlphBlank( 1 ) = false;
	IDFRecords( 1 ).AlphBlank( 2 ) = true;
	IDFRecords( 1 ).AlphBlank( 3 ) = false;
	IDFRecords( 1 ).AlphBlank( 4 ) = false;
	IDFRecords( 1 ).AlphBlank( 5 ) = false;
	IDFRecords( 1 ).Numbers.allocate( NumIDFNumbers );
	IDFRecords( 1 ).Numbers( 1 ) = 60;
	IDFRecords( 1 ).Numbers( 2 ) = 0.2;
	IDFRecords( 1 ).NumBlank.allocate( NumIDFNumbers );
	IDFRecords( 1 ).NumBlank = true;
	IDFRecords( 1 ).NumBlank( 1 ) = false;
	IDFRecords( 1 ).NumBlank( 2 ) = false;

	// Set up construction input objects
	lNumericFieldBlanks.allocate( NumIDDNumbers );
	lAlphaFieldBlanks.allocate( NumIDDAlphas );
	cAlphaFieldNames.allocate( NumIDDAlphas );
	cNumericFieldNames.allocate( NumIDDNumbers );
	cAlphaArgs.allocate( NumIDDAlphas );
	rNumericArgs.allocate( NumIDDNumbers );
	lNumericFieldBlanks = false;
	lAlphaFieldBlanks = false;
	cAlphaFieldNames = " ";
	cNumericFieldNames = " ";
	cAlphaArgs = " ";
	rNumericArgs = 0.0;
	MaxAlphaArgsFound = NumIDDAlphas;
	MaxNumericArgsFound = NumIDDNumeric;

	GetDemandManagerInput();

	EXPECT_EQ( ScheduleAlwaysOn, DemandMgr( 1 ).AvailSchedule );
	EXPECT_EQ( ManagerLimitFixed, DemandMgr( 1 ).LimitControl );
	EXPECT_DOUBLE_EQ( 60.0, DemandMgr( 1 ).LimitDuration );
	EXPECT_DOUBLE_EQ( 0.2, DemandMgr( 1 ).FixedRate );
	EXPECT_EQ( ManagerSelectionAll, DemandMgr( 1 ).SelectionControl );
	EXPECT_EQ( 1, DemandMgr( 1 ).NumOfLoads );

	iListOfObjects.deallocate();
	ObjectStartRecord.deallocate();
	ObjectGotCount.deallocate();
	IDFRecordsGotten.deallocate();
	ObjectDef(1).AlphFieldChks.deallocate();
	ObjectDef(1).NumRangeChks.deallocate();
	ObjectDef.deallocate();
	IDFRecords(1).Alphas.deallocate();
	IDFRecords(1).AlphBlank.deallocate();
	IDFRecords(1).Numbers.deallocate();
	IDFRecords(1).NumBlank.deallocate();
	IDFRecords.deallocate();
	lNumericFieldBlanks.deallocate();
	lAlphaFieldBlanks.deallocate();
	cAlphaFieldNames.deallocate();
	cNumericFieldNames.deallocate();
	cAlphaArgs.deallocate();
	rNumericArgs.deallocate();

	DemandMgr.deallocate();

	OAController.deallocate();
}
