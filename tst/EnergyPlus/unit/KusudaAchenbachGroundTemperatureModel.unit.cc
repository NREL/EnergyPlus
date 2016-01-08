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

// EnergyPlus::GroundTemperatureModels Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "EnergyPlus/DataIPShortCuts.hh"
#include "Fixtures/EnergyPlusFixture.hh"
#include "EnergyPlus/GroundTemperatureModeling/GroundTemperatureModelManager.hh"
#include "EnergyPlus/GroundTemperatureModeling/KusudaAchenbachGroundTemperatureModel.hh"

using namespace EnergyPlus;
using namespace EnergyPlus::GroundTemperatureManager;
 
TEST_F( EnergyPlusFixture, KusudaAchenbachGroundTempModelTest1 )
{
	std::string const idf_objects = delimited_string({
		"Version,8.4;",
		"Site:GroundTemperature:Undisturbed:KusudaAchenbach,",
		"	Test,	!- Name of ground temperature object",
		"	1.08,		!- Soil Thermal Conductivity",
		"	980,		!- Soil Density",
		"	2570,		!- Soil Specific Heat",
		"	15.0,		!- Average Surface Temperature",
		"	5.0,		!- Average Amplitude of Surface Temperature",
		"	1;			!- Phase Shift of Minimum Surface Temperature",
	});

	ASSERT_FALSE( process_idf( idf_objects ) );
	
	std::string const CurrentModuleObject = CurrentModuleObjects( objectType_KusudaGroundTemp );

	auto thisModel = GetGroundTempModelAndInit( CurrentModuleObject, "TEST" );

	EXPECT_NEAR( 10.0, thisModel->getGroundTempAtTimeInSeconds( 0.0, 0.0 ), 0.01 );		// Jan 1
	EXPECT_NEAR( 20.0, thisModel->getGroundTempAtTimeInSeconds( 0.0, 15768000 ), 0.01 );	// June 1
	EXPECT_NEAR( 10.0, thisModel->getGroundTempAtTimeInSeconds( 0.0, 31449600 ), 0.01 );	// Dec 30
	EXPECT_NEAR( 15.0, thisModel->getGroundTempAtTimeInSeconds( 100.0, 0.0 ), 0.01 );		// Very deep

	EXPECT_NEAR( 10.15, thisModel->getGroundTempAtTimeInMonths( 0.0, 1 ), 0.01 );			// January
	EXPECT_NEAR( 19.75, thisModel->getGroundTempAtTimeInMonths( 0.0, 6 ), 0.01 );			// June

}

TEST_F( EnergyPlusFixture, KusudaAchenbachGroundTempModelTest2 ) // lNumericFieldBlanks not working correctly for this test
{
	std::string const idf_objects = delimited_string({
		"Version,8.4;",
		"Site:GroundTemperature:Undisturbed:KusudaAchenbach,",
		"	Test,	!- Name of ground temperature object",
		"	1.08,		!- Soil Thermal Conductivity",
		"	980,		!- Soil Density",
		"	2570,		!- Soil Specific Heat",
		"	,			!- Average Surface Temperature",
		"	,			!- Average Amplitude of Surface Temperature",
		"	;			!- Phase Shift of Minimum Surface Temperature",
		"Site:GroundTemperature:Shallow,",
		"	16.00,	!- January",
		"	15.00,	!- February",
		"	16.00,	!- March",
		"	17.00,	!- April",
		"	18.00,	!- May",
		"	19.00,	!- June",
		"	20.00,	!- July",
		"	21.00,	!- August",
		"	20.00,	!- Septeber",
		"	19.00,	!- October",
		"	18.00,	!- November",
		"	17.00;	!- December",
	});

	ASSERT_FALSE( process_idf( idf_objects ) );
	
	std::string const CurrentModuleObject = CurrentModuleObjects( objectType_KusudaGroundTemp );

	auto thisModel = GetGroundTempModelAndInit( CurrentModuleObject, "TEST" );

	EXPECT_NEAR( 16.46, thisModel->getGroundTempAtTimeInSeconds( 0.0, 0.0 ), 0.01 );		// Jan 1
	EXPECT_NEAR( 17.17, thisModel->getGroundTempAtTimeInSeconds( 0.0, 11664000 ), 0.01 );	// May 15
	EXPECT_NEAR( 20.12, thisModel->getGroundTempAtTimeInSeconds( 0.0, 24883200 ), 0.01 );	// Oct 15
	EXPECT_NEAR( 16.46, thisModel->getGroundTempAtTimeInSeconds( 0.0, 31536000 ), 0.01 );	// Dec 31

	EXPECT_NEAR( 18.0, thisModel->getGroundTempAtTimeInSeconds( 100.0, 24883200 ), 0.01 );	// Oct 15--deep
}
