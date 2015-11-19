// C++ Headers
#include <fstream>
#include <string>
#include <vector>
#include <sstream>

// Google Test Headers
#include <gtest/gtest.h>

// Fixtures, etc.
#include <Fixtures/EnergyPlusFixture.hh>
#include <ConfiguredFunctions.hh>

namespace EnergyPlus {

	typedef EnergyPlusFixture DataSetFixture;

	std::vector<std::string> getAllLinesInFile( std::string filePath ) {
		std::ifstream infile(filePath);
		std::vector<std::string> lines;
		std::string line;
		while ( std::getline( infile, line ) )
		{
			lines.push_back( line );
		}
		return lines;
	}

	TEST_F( DataSetFixture, AirCooledChiller ) {
		ASSERT_FALSE( process_idf( delimited_string( getAllLinesInFile( configured_source_directory() + "/datasets/AirCooledChiller.idf" ) ) ) );
	}
	TEST_F( DataSetFixture, ASHRAE_2005_HOF_Materials ) {
		ASSERT_FALSE( process_idf( delimited_string( getAllLinesInFile( configured_source_directory() + "/datasets/ASHRAE_2005_HOF_Materials.idf" ) ) ) );
	}
	TEST_F( DataSetFixture, Boilers ) {
		ASSERT_FALSE( process_idf( delimited_string( getAllLinesInFile( configured_source_directory() + "/datasets/Boilers.idf" ) ) ) );
	}
	TEST_F( DataSetFixture, California_Title_24_2008 ) {
		ASSERT_FALSE( process_idf( delimited_string( getAllLinesInFile( configured_source_directory() + "/datasets/California_Title_24-2008.idf" ) ) ) );
	}
	TEST_F( DataSetFixture, Chillers ) {
		ASSERT_FALSE( process_idf( delimited_string( getAllLinesInFile( configured_source_directory() + "/datasets/Chillers.idf" ) ) ) );
	}
	TEST_F( DataSetFixture, CompositeWallConstructions ) {
		ASSERT_FALSE( process_idf( delimited_string( getAllLinesInFile( configured_source_directory() + "/datasets/CompositeWallConstructions.idf" ) ) ) );
	}
	TEST_F( DataSetFixture, DXCoolingCoil ) {
		ASSERT_FALSE( process_idf( delimited_string( getAllLinesInFile( configured_source_directory() + "/datasets/DXCoolingCoil.idf" ) ) ) );
	}
	TEST_F( DataSetFixture, ElectricGenerators ) {
		ASSERT_FALSE( process_idf( delimited_string( getAllLinesInFile( configured_source_directory() + "/datasets/ElectricGenerators.idf" ) ) ) );
	}
	TEST_F( DataSetFixture, ElectricityUSAEnvironmentalImpactFactors ) {
		ASSERT_FALSE( process_idf( delimited_string( getAllLinesInFile( configured_source_directory() + "/datasets/ElectricityUSAEnvironmentalImpactFactors.idf" ) ) ) );
	}
	TEST_F( DataSetFixture, ElectronicEnthalpyEconomizerCurves ) {
		ASSERT_FALSE( process_idf( delimited_string( getAllLinesInFile( configured_source_directory() + "/datasets/ElectronicEnthalpyEconomizerCurves.idf" ) ) ) );
	}
	TEST_F( DataSetFixture, ExhaustFiredChiller ) {
		ASSERT_FALSE( process_idf( delimited_string( getAllLinesInFile( configured_source_directory() + "/datasets/ExhaustFiredChiller.idf" ) ) ) );
	}
	TEST_F( DataSetFixture, FluidPropertiesRefData ) {
		ASSERT_FALSE( process_idf( delimited_string( getAllLinesInFile( configured_source_directory() + "/datasets/FluidPropertiesRefData.idf" ) ) ) );
	}
	TEST_F( DataSetFixture, FossilFuelEnvironmentalImpactFactors ) {
		ASSERT_FALSE( process_idf( delimited_string( getAllLinesInFile( configured_source_directory() + "/datasets/FossilFuelEnvironmentalImpactFactors.idf" ) ) ) );
	}
	TEST_F( DataSetFixture, GLHERefData ) {
		ASSERT_FALSE( process_idf( delimited_string( getAllLinesInFile( configured_source_directory() + "/datasets/GLHERefData.idf" ) ) ) );
	}
	TEST_F( DataSetFixture, GlycolPropertiesRefData ) {
		ASSERT_FALSE( process_idf( delimited_string( getAllLinesInFile( configured_source_directory() + "/datasets/GlycolPropertiesRefData.idf" ) ) ) );
	}
	TEST_F( DataSetFixture, LCCusePriceEscalationDataSet2010 ) {
		ASSERT_FALSE( process_idf( delimited_string( getAllLinesInFile( configured_source_directory() + "/datasets/LCCusePriceEscalationDataSet2010.idf" ) ) ) );
	}
	TEST_F( DataSetFixture, LCCusePriceEscalationDataSet2011 ) {
		ASSERT_FALSE( process_idf( delimited_string( getAllLinesInFile( configured_source_directory() + "/datasets/LCCusePriceEscalationDataSet2011.idf" ) ) ) );
	}
	TEST_F( DataSetFixture, LCCusePriceEscalationDataSet2012 ) {
		ASSERT_FALSE( process_idf( delimited_string( getAllLinesInFile( configured_source_directory() + "/datasets/LCCusePriceEscalationDataSet2012.idf" ) ) ) );
	}
	TEST_F( DataSetFixture, LCCusePriceEscalationDataSet2013 ) {
		ASSERT_FALSE( process_idf( delimited_string( getAllLinesInFile( configured_source_directory() + "/datasets/LCCusePriceEscalationDataSet2013.idf" ) ) ) );
	}
	TEST_F( DataSetFixture, LCCusePriceEscalationDataSet2014 ) {
		ASSERT_FALSE( process_idf( delimited_string( getAllLinesInFile( configured_source_directory() + "/datasets/LCCusePriceEscalationDataSet2014.idf" ) ) ) );
	}
	TEST_F( DataSetFixture, MoistureMaterials ) {
		ASSERT_FALSE( process_idf( delimited_string( getAllLinesInFile( configured_source_directory() + "/datasets/MoistureMaterials.idf" ) ) ) );
	}
	TEST_F( DataSetFixture, PerfCurves ) {
		ASSERT_FALSE( process_idf( delimited_string( getAllLinesInFile( configured_source_directory() + "/datasets/PerfCurves.idf" ) ) ) );
	}
	TEST_F( DataSetFixture, PrecipitationSchedulesUSA ) {
		ASSERT_FALSE( process_idf( delimited_string( getAllLinesInFile( configured_source_directory() + "/datasets/PrecipitationSchedulesUSA.idf" ) ) ) );
	}
	TEST_F( DataSetFixture, RefrigerationCasesDataSet ) {
		ASSERT_FALSE( process_idf( delimited_string( getAllLinesInFile( configured_source_directory() + "/datasets/RefrigerationCasesDataSet.idf" ) ) ) );
	}
	TEST_F( DataSetFixture, RefrigerationCompressorCurves ) {
		ASSERT_FALSE( process_idf( delimited_string( getAllLinesInFile( configured_source_directory() + "/datasets/RefrigerationCompressorCurves.idf" ) ) ) );
	}
	TEST_F( DataSetFixture, RefrigerationCompressorCurvesNeedFix ) {
		ASSERT_FALSE( process_idf( delimited_string( getAllLinesInFile( configured_source_directory() + "/datasets/RefrigerationCompressorCurvesNeedFix.idf" ) ) ) );
	}
	TEST_F( DataSetFixture, RefrigerationCompressorCurvesOK ) {
		ASSERT_FALSE( process_idf( delimited_string( getAllLinesInFile( configured_source_directory() + "/datasets/RefrigerationCompressorCurvesOK.idf" ) ) ) );
	}
	TEST_F( DataSetFixture, ResidentialACsAndHPsPerfCurves ) {
		ASSERT_FALSE( process_idf( delimited_string( getAllLinesInFile( configured_source_directory() + "/datasets/ResidentialACsAndHPsPerfCurves.idf" ) ) ) );
	}
	TEST_F( DataSetFixture, RooftopPackagedHeatPump ) {
		ASSERT_FALSE( process_idf( delimited_string( getAllLinesInFile( configured_source_directory() + "/datasets/RooftopPackagedHeatPump.idf" ) ) ) );
	}
	TEST_F( DataSetFixture, SandiaPVdata ) {
		ASSERT_FALSE( process_idf( delimited_string( getAllLinesInFile( configured_source_directory() + "/datasets/SandiaPVdata.idf" ) ) ) );
	}
	TEST_F( DataSetFixture, Schedules ) {
		ASSERT_FALSE( process_idf( delimited_string( getAllLinesInFile( configured_source_directory() + "/datasets/Schedules.idf" ) ) ) );
	}
	TEST_F( DataSetFixture, SolarCollectors ) {
		ASSERT_FALSE( process_idf( delimited_string( getAllLinesInFile( configured_source_directory() + "/datasets/SolarCollectors.idf" ) ) ) );
	}
	TEST_F( DataSetFixture, StandardReports ) {
		ASSERT_FALSE( process_idf( delimited_string( getAllLinesInFile( configured_source_directory() + "/datasets/StandardReports.idf" ) ) ) );
	}
	TEST_F( DataSetFixture, SurfaceColorSchemes ) {
		ASSERT_FALSE( process_idf( delimited_string( getAllLinesInFile( configured_source_directory() + "/datasets/SurfaceColorSchemes.idf" ) ) ) );
	}
	TEST_F( DataSetFixture, USHolidays_DST ) {
		ASSERT_FALSE( process_idf( delimited_string( getAllLinesInFile( configured_source_directory() + "/datasets/USHolidays-DST.idf" ) ) ) );
	}
	TEST_F( DataSetFixture, WindowBlindMaterials ) {
		ASSERT_FALSE( process_idf( delimited_string( getAllLinesInFile( configured_source_directory() + "/datasets/WindowBlindMaterials.idf" ) ) ) );
	}
	TEST_F( DataSetFixture, WindowConstructs ) {
		ASSERT_FALSE( process_idf( delimited_string( getAllLinesInFile( configured_source_directory() + "/datasets/WindowConstructs.idf" ) ) ) );
	}
	TEST_F( DataSetFixture, WindowGasMaterials ) {
		ASSERT_FALSE( process_idf( delimited_string( getAllLinesInFile( configured_source_directory() + "/datasets/WindowGasMaterials.idf" ) ) ) );
	}
	TEST_F( DataSetFixture, WindowGlassMaterials ) {
		ASSERT_FALSE( process_idf( delimited_string( getAllLinesInFile( configured_source_directory() + "/datasets/WindowGlassMaterials.idf" ) ) ) );
	}
	TEST_F( DataSetFixture, WindowScreenMaterials ) {
		ASSERT_FALSE( process_idf( delimited_string( getAllLinesInFile( configured_source_directory() + "/datasets/WindowScreenMaterials.idf" ) ) ) );
	}
	TEST_F( DataSetFixture, WindowShadeMaterials ) {
		ASSERT_FALSE( process_idf( delimited_string( getAllLinesInFile( configured_source_directory() + "/datasets/WindowShadeMaterials.idf" ) ) ) );
	}

}
