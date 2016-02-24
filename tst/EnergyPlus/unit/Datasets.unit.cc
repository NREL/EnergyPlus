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
