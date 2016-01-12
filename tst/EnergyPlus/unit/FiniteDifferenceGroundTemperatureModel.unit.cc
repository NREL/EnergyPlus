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
#include "EnergyPlus/DataGlobals.hh"
#include "EnergyPlus/DataIPShortCuts.hh"
#include "Fixtures/EnergyPlusFixture.hh"
#include "EnergyPlus/GroundTemperatureModeling/FiniteDifferenceGroundTemperatureModel.hh"
#include "EnergyPlus/GroundTemperatureModeling/GroundTemperatureModelManager.hh"
#include "EnergyPlus/WeatherManager.hh"

using namespace EnergyPlus;
using namespace EnergyPlus::GroundTemperatureManager;
 
TEST_F( EnergyPlusFixture, FiniteDiffGroundTempModelTest )
{

	using DataGlobals::Pi;
	using WeatherManager::NumDaysInYear;
	using namespace DataIPShortCuts;

	std::shared_ptr< FiniteDiffGroundTempsModel > thisModel( new FiniteDiffGroundTempsModel() );

	thisModel->objectType = objectType_FiniteDiffGroundTemp;
	thisModel->objectName = "Test";
	thisModel->baseConductivity = 1.08;
	thisModel->baseDensity = 962.0;
	thisModel->baseSpecificHeat = 2576.0;
	thisModel->waterContent = 30.0 / 100.0;
	thisModel->saturatedWaterContent = 50.0 / 100.0;
	thisModel->evapotransCoeff = 0.408;

	EXPECT_NEAR( 2.0, thisModel->interpolate( 2.0, 3.0, 1.0, 3.0, 1.0 ), 0.0000001 );

	thisModel->developMesh();

	// Setting weather data manually here
	thisModel->weatherDataArray.dimension( NumDaysInYear );

	Real64 drybulb_minTemp = 5;
	Real64 drybulb_amp = 10;
	Real64 relHum_const = 0.5;
	Real64 windSpeed_const = 3.0;
	Real64 solar_min = 100;
	Real64 solar_amp = 100;

	for ( int day = 1; day <= NumDaysInYear; ++day ) {
		auto & tdwd = thisModel->weatherDataArray( day ); // "This day weather data"

		Real64 theta = 2 * Pi * day / NumDaysInYear;
		Real64 omega = 2 * Pi * 130 / NumDaysInYear; // Shifts min to around the end of Jan

		tdwd.dryBulbTemp = drybulb_amp * std::sin( theta - omega ) + ( drybulb_minTemp + drybulb_amp );
		tdwd.relativeHumidity = relHum_const;
		tdwd.windSpeed = windSpeed_const;
		tdwd.horizontalRadiation = solar_amp * std::sin( theta - omega ) + ( solar_min + solar_amp );;
		tdwd.airDensity = 1.2;
	}

	thisModel->annualAveAirTemp = 15.0;
	thisModel->maxDailyAirTemp = 25.0;
	thisModel->minDailyAirTemp = 5.0;
	thisModel->dayOfMinDailyAirTemp = 30;

	thisModel->performSimulation();

	EXPECT_NEAR( 4.51, thisModel->getGroundTempAtTimeInMonths( 0.0, 1 ), 0.01 );
	EXPECT_NEAR( 19.14, thisModel->getGroundTempAtTimeInMonths( 0.0, 6 ), 0.01 );
	EXPECT_NEAR( 7.96, thisModel->getGroundTempAtTimeInMonths( 0.0, 12 ), 0.01 );
	EXPECT_NEAR( 3.46, thisModel->getGroundTempAtTimeInMonths( 0.0, 14 ), 0.01 );

	EXPECT_NEAR( 14.36, thisModel->getGroundTempAtTimeInMonths( 3.0, 1 ), 0.01 );
	EXPECT_NEAR( 11.78, thisModel->getGroundTempAtTimeInMonths( 3.0, 6 ), 0.01 );
	EXPECT_NEAR( 15.57, thisModel->getGroundTempAtTimeInMonths( 3.0, 12 ), 0.01 );

	EXPECT_NEAR( 14.58, thisModel->getGroundTempAtTimeInMonths( 25.0, 1 ), 0.01 );
	EXPECT_NEAR( 14.55, thisModel->getGroundTempAtTimeInMonths( 25.0, 6 ), 0.01 );
	EXPECT_NEAR( 14.53, thisModel->getGroundTempAtTimeInMonths( 25.0, 12 ), 0.01 );

	EXPECT_NEAR( 5.04, thisModel->getGroundTempAtTimeInSeconds( 0.0, 0.0 ), 0.01 ); 
	EXPECT_NEAR( 19.28, thisModel->getGroundTempAtTimeInSeconds( 0.0, 14342400 ), 0.01 );
	EXPECT_NEAR( 7.32, thisModel->getGroundTempAtTimeInSeconds( 0.0, 30153600), 0.01 );
	EXPECT_NEAR( 3.53, thisModel->getGroundTempAtTimeInSeconds( 0.0, 35510400 ), 0.01 );

	EXPECT_NEAR( 14.36, thisModel->getGroundTempAtTimeInSeconds( 3.0, 1296000 ), 0.01 );
	EXPECT_NEAR( 11.80, thisModel->getGroundTempAtTimeInSeconds( 3.0, 14342400 ), 0.01 );
	EXPECT_NEAR( 15.46, thisModel->getGroundTempAtTimeInSeconds( 3.0, 30153600 ), 0.01 );

	EXPECT_NEAR( 14.52, thisModel->getGroundTempAtTimeInSeconds( 25.0, 0.0 ), 0.01 );
	EXPECT_NEAR( 14.55, thisModel->getGroundTempAtTimeInSeconds( 25.0, 14342400 ), 0.01 );
	EXPECT_NEAR( 14.52, thisModel->getGroundTempAtTimeInSeconds( 25.0, 30153600 ), 0.01 );
}
