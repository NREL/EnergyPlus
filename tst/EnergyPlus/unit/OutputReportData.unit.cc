// EnergyPlus::OutputReportTabular Unit Tests

// Google Test Headers
#include <gtest/gtest.h>
// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
// EnergyPlus Headers
#include <EnergyPlus/OutputReportData.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/OutputProcessor.hh>

using namespace EnergyPlus;
using namespace ObjexxFCL;
using namespace OutputProcessor;

TEST( OutputReportDataTest, AnnualFieldSetConstructor )
{
	ShowMessage( "Begin Test: OutputReportDataTest, AnnualFieldSetConstructor" );


	std::string varNameTest = "TestReport";
	AnnualFieldSet::AggregationKind kindOfAggregationTest = AnnualFieldSet::AggregationKind::sumOrAvg;
	int numDigitsShownTest = 3;
	AnnualFieldSet fldStTest = AnnualFieldSet( varNameTest, kindOfAggregationTest, numDigitsShownTest );
	EXPECT_EQ( fldStTest.m_variMeter, varNameTest );
	EXPECT_EQ( fldStTest.m_aggregate, kindOfAggregationTest );
	EXPECT_EQ( fldStTest.m_showDigits, numDigitsShownTest );
}


TEST( OutputReportDataTest, getVariableKeys )
{
	ShowMessage( "Begin Test: OutputReportDataTest, getVariableKeys" );

	std::string varNameTest = "TestReport";
	AnnualFieldSet::AggregationKind kindOfAggregationTest = AnnualFieldSet::AggregationKind::sumOrAvg;
	int numDigitsShownTest = 3;
	AnnualFieldSet fldStTest = AnnualFieldSet( varNameTest, kindOfAggregationTest, numDigitsShownTest );

	Real64 extLitPow;
	Real64 extLitUse;

	SetupOutputVariable( "Exterior Lights Electric Energy [J]", extLitUse, "Zone", "Sum", "Lite1", _, "Electricity", "Exterior Lights", "General" );
	SetupOutputVariable( "Exterior Lights Electric Energy [J]", extLitUse, "Zone", "Sum", "Lite2", _, "Electricity", "Exterior Lights", "General" );
	SetupOutputVariable( "Exterior Lights Electric Energy [J]", extLitUse, "Zone", "Sum", "Lite3", _, "Electricity", "Exterior Lights", "General" );
	SetupOutputVariable( "Exterior Lights Electric Power [W]", extLitPow, "Zone", "Average", "Lite1" );
	SetupOutputVariable( "Exterior Lights Electric Power [W]", extLitPow, "Zone", "Average", "Lite2" );
	SetupOutputVariable( "Exterior Lights Electric Power [W]", extLitPow, "Zone", "Average", "Lite3" );

	int keyCount = 0;
	int typeVar = 0;
	int avgSumVar = 0;
	int stepTypeVar = 0;
	std::string unitsVar = "";

	fldStTest.m_variMeter = "EXTERIOR LIGHTS ELECTRIC ENERGY";
	keyCount = fldStTest.getVariableKeyCountandTypeFromFldSt( typeVar, avgSumVar, stepTypeVar, unitsVar );
	EXPECT_EQ( keyCount, 3 );

	fldStTest.getVariableKeysFromFldSt( typeVar, keyCount, fldStTest.m_namesOfKeys, fldStTest.m_indexesForKeyVar );

	EXPECT_EQ( fldStTest.m_namesOfKeys[0], "LITE1" );
	EXPECT_EQ( fldStTest.m_namesOfKeys[1], "LITE2" );
	EXPECT_EQ( fldStTest.m_namesOfKeys[2], "LITE3" );


}
