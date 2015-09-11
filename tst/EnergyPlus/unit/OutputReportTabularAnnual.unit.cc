// EnergyPlus::OutputReportTabular Unit Tests

// Google Test Headers
#include <gtest/gtest.h>
// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
// EnergyPlus Headers
#include <EnergyPlus/OutputReportData.hh>
#include <EnergyPlus/OutputReportTabularAnnual.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/OutputProcessor.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace EnergyPlus::OutputReportTabularAnnual;
using namespace ObjexxFCL;

TEST_F( EnergyPlusFixture, OutputReportTabularAnnual_GetInput )
{
	std::string const idf_objects = delimited_string( {
		"Version,8.3;",
		"Output:Table:Annual,",
		"Space Gains Annual Report, !- Name",
		"Filter1, !- Filter",
		"Schedule2, !- Schedule Name",
		"Zone People Total Heating Energy, !- Variable or Meter 1 Name",
		"SumOrAverage, !- Aggregation Type for Variable or Meter 1",
		"4, !- field Digits After Decimal 1",
		"Zone Lights Total Heating Energy, !- Variable or Meter 2 Name",
		"hoursNonZero, !- Aggregation Type for Variable or Meter 2",
		", !- field Digits After Decimal 2",
		"Zone Electric Equipment Total Heating Energy; !- Variable or Meter 3 Name",
	} );

	ASSERT_FALSE( process_idf( idf_objects ) );

	DataGlobals::DoWeathSim = true;

	GetInputTabularAnnual();

	EXPECT_EQ( OutputReportTabularAnnual::annualTables.size(), 1u );

	std::vector<AnnualTable>::iterator firstTable = OutputReportTabularAnnual::annualTables.begin();

	std::vector<std::string> tableParams = firstTable->inspectTable();

	EXPECT_EQ( tableParams[0] , "SPACE GAINS ANNUAL REPORT" ); // m_name
	EXPECT_EQ( tableParams[1], "FILTER1" ); //  m_filter
	EXPECT_EQ( tableParams[2], "SCHEDULE2" ); //  m_scheduleName

	std::vector<std::string> fieldSetParams = firstTable->inspectTableFieldSets(0);
	EXPECT_EQ( fieldSetParams[0], "ZONE PEOPLE TOTAL HEATING ENERGY");
	EXPECT_EQ( fieldSetParams[3], "4" ); // m_showDigits
	EXPECT_EQ( fieldSetParams[8], "0" ); // m_aggregate - 0 is sumOrAvg

	fieldSetParams = firstTable->inspectTableFieldSets( 1 );
	EXPECT_EQ( fieldSetParams[3], "2" ); // m_showDigits (2 is the default if no value provided)
	EXPECT_EQ( fieldSetParams[8], "3" ); // m_aggregate - 3 is hoursNonZero

	fieldSetParams = firstTable->inspectTableFieldSets( 2 );
	EXPECT_EQ( fieldSetParams[8], "0" ); // m_aggregate - 0 is sumOrAvg is default if not included in idf input object


}


TEST_F( EnergyPlusFixture, OutputReportTabularAnnual_SetupGathering )
{
	std::string const idf_objects = delimited_string( {
		"Version,8.3;",
		"Output:Table:Annual,",
		"Space Gains Annual Report, !- Name",
		", !- Filter",
		", !- Schedule Name",
		"Exterior Lights Electric Energy, !- Variable or Meter 1 Name",
		"SumOrAverage, !- Aggregation Type for Variable or Meter 1",
		"4, !- field Digits After Decimal 1",
		"Exterior Lights Electric Power, !- Variable or Meter 2 Name",
		"hoursNonZero, !- Aggregation Type for Variable or Meter 2",
		", !- field Digits After Decimal 2",
		"Zone Electric Equipment Total Heating Energy; !- Variable or Meter 3 Name",
	} );

	ASSERT_FALSE( process_idf( idf_objects ) );

	Real64 extLitPow;
	Real64 extLitUse;

	SetupOutputVariable( "Exterior Lights Electric Energy [J]", extLitUse, "Zone", "Sum", "Lite1", _, "Electricity", "Exterior Lights", "General" );
	SetupOutputVariable( "Exterior Lights Electric Energy [J]", extLitUse, "Zone", "Sum", "Lite2", _, "Electricity", "Exterior Lights", "General" );
	SetupOutputVariable( "Exterior Lights Electric Energy [J]", extLitUse, "Zone", "Sum", "Lite3", _, "Electricity", "Exterior Lights", "General" );
	SetupOutputVariable( "Exterior Lights Electric Power [W]", extLitPow, "Zone", "Average", "Lite1" );
	SetupOutputVariable( "Exterior Lights Electric Power [W]", extLitPow, "Zone", "Average", "Lite2" );
	SetupOutputVariable( "Exterior Lights Electric Power [W]", extLitPow, "Zone", "Average", "Lite3" );

	DataGlobals::DoWeathSim = true;

	GetInputTabularAnnual(); // this also calls setupGathering
	EXPECT_EQ( OutputReportTabularAnnual::annualTables.size(), 1u );

	std::vector<AnnualTable>::iterator firstTable = OutputReportTabularAnnual::annualTables.begin();
	std::vector<std::string> fieldSetParams = firstTable->inspectTableFieldSets( 0 );

	EXPECT_EQ( fieldSetParams[0], "EXTERIOR LIGHTS ELECTRIC ENERGY" );
	EXPECT_EQ( fieldSetParams[2], "J" ); // m_varUnits
	EXPECT_EQ( fieldSetParams[4], "2" ); // m_typeOfVar
	EXPECT_EQ( fieldSetParams[5], "3" ); // m_keyCount
	EXPECT_EQ( fieldSetParams[6], "2" ); // m_varAvgSum
	EXPECT_EQ( fieldSetParams[7], "1" ); // m_varStepType

}

TEST_F( EnergyPlusFixture, OutputReportTabularAnnual_GatherResults )
{
	std::string const idf_objects = delimited_string( {
		"Version,8.3;",
		"Output:Table:Annual,",
		"Space Gains Annual Report, !- Name",
		", !- Filter",
		", !- Schedule Name",
		"Exterior Lights Electric Energy, !- Variable or Meter 1 Name",
		"SumOrAverage, !- Aggregation Type for Variable or Meter 1",
		"4, !- field Digits After Decimal 1",
		"Exterior Lights Electric Power, !- Variable or Meter 2 Name",
		"Maximum, !- Aggregation Type for Variable or Meter 2",
		", !- field Digits After Decimal 2",
		"Zone Electric Equipment Total Heating Energy; !- Variable or Meter 3 Name",
	} );

	ASSERT_FALSE( process_idf( idf_objects ) );

	Real64 extLitPow;
	Real64 extLitUse;

	SetupOutputVariable( "Exterior Lights Electric Energy [J]", extLitUse, "Zone", "Sum", "Lite1", _, "Electricity", "Exterior Lights", "General" );
	SetupOutputVariable( "Exterior Lights Electric Energy [J]", extLitUse, "Zone", "Sum", "Lite2", _, "Electricity", "Exterior Lights", "General" );
	SetupOutputVariable( "Exterior Lights Electric Energy [J]", extLitUse, "Zone", "Sum", "Lite3", _, "Electricity", "Exterior Lights", "General" );
	SetupOutputVariable( "Exterior Lights Electric Power [W]", extLitPow, "Zone", "Average", "Lite1" );
	SetupOutputVariable( "Exterior Lights Electric Power [W]", extLitPow, "Zone", "Average", "Lite2" );
	SetupOutputVariable( "Exterior Lights Electric Power [W]", extLitPow, "Zone", "Average", "Lite3" );

	DataGlobals::DoWeathSim = true;
	DataGlobals::TimeStepZone = 0.25;

	GetInputTabularAnnual();
	EXPECT_EQ( OutputReportTabularAnnual::annualTables.size(), 1u );

	extLitPow = 2.01;
	extLitUse = 1.01;

	//UpdateDataandReport( 1 ); not sure if this is needed
	GatherAnnualResultsForTimeStep( 1 );

	// STOPPPED HERE. NOT SEEING THE POWER VARIABLE SHOWING UP

	std::vector<AnnualTable>::iterator firstTable = OutputReportTabularAnnual::annualTables.begin();
	std::vector<std::string> fieldSetParams = firstTable->inspectTableFieldSets( 0 );

}



