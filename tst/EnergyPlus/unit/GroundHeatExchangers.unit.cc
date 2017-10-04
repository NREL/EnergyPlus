// EnergyPlus, Copyright (c) 1996-2017, The Board of Trustees of the University of Illinois,
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy), Oak Ridge
// National Laboratory, managed by UT-Battelle, Alliance for Sustainable Energy, LLC, and other
// contributors. All rights reserved.
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
//     similar designation, without the U.S. Department of Energy's prior written consent.
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

// EnergyPlus::GroundHeatExchangers Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/GroundHeatExchangers.hh>
#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace EnergyPlus::GroundHeatExchangers;
using namespace EnergyPlus::DataGlobals;
using namespace EnergyPlus::DataPlant;
using namespace EnergyPlus::DataLoopNode;


TEST_F( EnergyPlusFixture, GroundHeatExchangerTest_Interpolate )
{

	// Initialization
	GLHESlinky thisGLHE;
	Real64 thisLNTTS;
	Real64 thisGFunc;

	int NPairs = 2;

	std::shared_ptr< GLHEResponseFactorsStruct > thisRF( new GLHEResponseFactorsStruct );
	thisGLHE.myRespFactors = thisRF;

	thisGLHE.myRespFactors->LNTTS.allocate( NPairs );
	thisGLHE.myRespFactors->GFNC.allocate( NPairs );

	thisGLHE.myRespFactors->LNTTS( 1 ) = 0.0;
	thisGLHE.myRespFactors->LNTTS( 2 ) = 5.0;
	thisGLHE.myRespFactors->GFNC( 1 ) = 0.0;
	thisGLHE.myRespFactors->GFNC( 2 ) = 5.0;

	// Case when extrapolating beyond lower bound
	thisLNTTS = -1.0;
	thisGFunc = thisGLHE.interpGFunc( thisLNTTS );
	EXPECT_DOUBLE_EQ( -1.0, thisGFunc );

	// Case when extrapolating beyond upper bound
	thisLNTTS = 6.0;
	thisGFunc = thisGLHE.interpGFunc( thisLNTTS );
	EXPECT_DOUBLE_EQ( 6.0 , thisGFunc );

	// Case when we're actually interpolating
	thisLNTTS = 2.5;
	thisGFunc = thisGLHE.interpGFunc( thisLNTTS );
	EXPECT_DOUBLE_EQ( 2.5, thisGFunc );
}

TEST_F( EnergyPlusFixture, GroundHeatExchangerTest_Slinky_GetGFunc )
{

	// Initialization
	GLHESlinky thisGLHE;
	Real64 thisGFunc;
	Real64 time;

	int NPairs = 2;

	std::shared_ptr< GLHEResponseFactorsStruct > thisRF( new GLHEResponseFactorsStruct );
	thisGLHE.myRespFactors = thisRF;

	thisGLHE.myRespFactors->LNTTS.allocate( NPairs );
	thisGLHE.myRespFactors->GFNC.allocate( NPairs );

	thisGLHE.myRespFactors->LNTTS( 1 ) = 0.0;
	thisGLHE.myRespFactors->LNTTS( 2 ) = 5.0;
	thisGLHE.myRespFactors->GFNC( 1 ) = 0.0;
	thisGLHE.myRespFactors->GFNC( 2 ) = 5.0;

	time = std::pow( 10.0, 2.5 );

	thisGFunc = thisGLHE.getGFunc( time );

	EXPECT_EQ( 2.5, thisGFunc );
}

TEST_F( EnergyPlusFixture, GroundHeatExchangerTest_System_GetGFunc )
{

	// Initialization
	GLHEVert thisGLHE;
	Real64 thisGFunc;
	Real64 time;

	int NPairs = 2;

	std::shared_ptr< GLHEResponseFactorsStruct > thisRF( new GLHEResponseFactorsStruct );
	thisGLHE.myRespFactors = thisRF;

	thisGLHE.myRespFactors->LNTTS.allocate( NPairs );
	thisGLHE.myRespFactors->GFNC.allocate( NPairs );

	thisGLHE.myRespFactors->LNTTS( 1 ) = 0.0;
	thisGLHE.myRespFactors->LNTTS( 2 ) = 5.0;
	thisGLHE.myRespFactors->GFNC( 1 ) = 0.0;
	thisGLHE.myRespFactors->GFNC( 2 ) = 5.0;

	time = std::pow( 2.7182818284590452353602874, 2.5 );

	thisGLHE.bhLength = 1.0;
	thisGLHE.bhRadius = 1.0;

	// Situation when correction is not applied
	thisGLHE.myRespFactors->gRefRatio = 1.0;
	thisGFunc = thisGLHE.getGFunc( time );
	EXPECT_DOUBLE_EQ( 2.5, thisGFunc );

	// Situation when correction is applied
	thisGLHE.myRespFactors->gRefRatio = 2.0;
	thisGFunc = thisGLHE.getGFunc( time );
	EXPECT_NEAR( 2.5 + 0.6931, thisGFunc, 0.0001);

}

TEST_F( EnergyPlusFixture, GroundHeatExchangerTest_Slinky_CalcHXResistance )
{
	// Initializations
	GLHESlinky thisGLHE;

	PlantLoop.allocate( 1 );
	thisGLHE.loopNum = 1;

	PlantLoop( thisGLHE.loopNum ).FluidName = "WATER";
	PlantLoop( thisGLHE.loopNum ).FluidIndex = 1;

	thisGLHE.inletTemp = 5.0;
	thisGLHE.massFlowRate = 0.01;
	thisGLHE.numTrenches = 1;
	thisGLHE.pipe.outDia = 0.02667;
	thisGLHE.pipe.thickness = 0.004;
	thisGLHE.pipe.k = 0.4;

	// Re < 2300 mass flow rate
	thisGLHE.calcHXResistance();
	EXPECT_NEAR( 0.13487, thisGLHE.HXResistance, 0.0001 );

	// 4000 > Re > 2300 mass flow rate
	thisGLHE.massFlowRate = 0.07;
	thisGLHE.calcHXResistance();
	EXPECT_NEAR( 0.08582, thisGLHE.HXResistance, 0.0001 );

	// Re > 4000 mass flow rate
	thisGLHE.massFlowRate = 0.1;
	thisGLHE.calcHXResistance();
	EXPECT_NEAR( 0.077185, thisGLHE.HXResistance, 0.0001 );

	// Zero mass flow rate
	thisGLHE.massFlowRate = 0.0;
	thisGLHE.calcHXResistance();
	EXPECT_NEAR( 0.07094, thisGLHE.HXResistance, 0.0001 );
}

TEST_F( EnergyPlusFixture, GroundHeatExchangerTest_System_CalcHXResistance )
{

	// Initializations
	GLHEVert thisGLHE;

	std::shared_ptr< GLHEResponseFactorsStruct > thisRF( new GLHEResponseFactorsStruct );
	thisGLHE.myRespFactors = thisRF;

	PlantLoop.allocate( 1 );
	thisGLHE.loopNum = 1;

	PlantLoop( thisGLHE.loopNum ).FluidName = "WATER";
	PlantLoop( thisGLHE.loopNum ).FluidIndex = 1;

	thisGLHE.inletTemp = 5.0;
	thisGLHE.massFlowRate = 0.01;
	thisGLHE.myRespFactors->numBoreholes = 1;
	thisGLHE.pipe.outDia = 0.02667;
	thisGLHE.pipe.thickness = 0.004;
	thisGLHE.pipe.k = 0.4;
	thisGLHE.bhRadius = 0.1;
	thisGLHE.grout.k = 1.0;

	// Re < 2300 mass flow rate; 0.0 <= distanceRatio <= 2.5 correction factor
	thisGLHE.calcHXResistance();
	EXPECT_NEAR( 0.49421, thisGLHE.HXResistance, 0.0001 );

	// Re < 2300 mass flow rate; 0.25 < distanceRatio < 0.5 correction factor
	thisGLHE.bhUTubeDist = 0.05;
	thisGLHE.calcHXResistance();
	EXPECT_NEAR( 0.46859, thisGLHE.HXResistance, 0.0001 );

	// Re < 2300 mass flow rate; 0.5 <= distanceRatio < 0.75 correction factor
	thisGLHE.bhUTubeDist = 0.087;
	thisGLHE.calcHXResistance();
	EXPECT_NEAR( 0.32891, thisGLHE.HXResistance, 0.0001 );

	// 4000 > Re > 2300 mass flow rate; all other distance ratios correction factor
	thisGLHE.bhUTubeDist = 0.12;
	thisGLHE.massFlowRate = 0.07;
	thisGLHE.calcHXResistance();
	EXPECT_NEAR( 0.18391, thisGLHE.HXResistance, 0.0001 );

	// Re > 4000 mass flow rate; all other distance ratios correction factor
	thisGLHE.massFlowRate = 0.1;
	thisGLHE.calcHXResistance();
	EXPECT_NEAR( 0.17526, thisGLHE.HXResistance, 0.0001 );

	// Zero mass flow rate; distance ratio > 0.75 correction factor
	thisGLHE.massFlowRate = 0.0;
	thisGLHE.calcHXResistance();
	EXPECT_NEAR( 0.16903, thisGLHE.HXResistance, 0.0001 );
}

TEST_F( EnergyPlusFixture, GroundHeatExchangerTest_Slinky_CalcGroundHeatExchanger )
{

	// Initializations
	GLHESlinky thisGLHE;

	std::shared_ptr< GLHEResponseFactorsStruct > thisRF( new GLHEResponseFactorsStruct );
	thisGLHE.myRespFactors = thisRF;

	thisGLHE.numCoils = 100;
	thisGLHE.numTrenches = 2;
	thisGLHE.maxSimYears = 10;
	thisGLHE.coilPitch = 0.4;
	thisGLHE.coilDepth = 1.5;
	thisGLHE.coilDiameter = 0.8;
	thisGLHE.pipe.outDia = 0.034;
	thisGLHE.trenchSpacing = 3.0;
	thisGLHE.soil.diffusivity = 3.0e-007;
	thisGLHE.AGG = 192;
	thisGLHE.SubAGG = 15;

	// Horizontal G-Functions
	thisGLHE.calcGFunctions();
	EXPECT_NEAR( 19.08237, thisGLHE.myRespFactors->GFNC( 28 ), 0.0001 );

	// Vertical G-Functions
	thisGLHE.verticalConfig = true;
	thisGLHE.calcGFunctions();
	EXPECT_NEAR( 18.91819, thisGLHE.myRespFactors->GFNC( 28 ), 0.0001 );

}

TEST_F( EnergyPlusFixture, GroundHeatExchangerTest_System_Properties_IDF_Check )
{
	std::string const idf_objects = delimited_string({
		"GroundHeatExchanger:Vertical:Properties,",
		"	GHE-1 Props,        !- Name",
		"	1,                  !- Depth of Top of Borehole {m}",
		"	110,                !- Borehole Length {m}",
		"	0.109982,           !- Borehole Diameter {m}",
		"	0.744,              !- Grout Thermal Conductivity {W/m-K}",
		"	3.90E+06,           !- Grout Thermal Heat Capacity {J/m3-K}",
		"	0.389,              !- Pipe Thermal Conductivity {W/m-K}",
		"	0.0267,             !- Pipe Outer Diameter {m}",
		"	0.00243,            !- Pipe Thickness {m}",
		"	0.01887;            !- U-Tube Distance {m}"
	});

	ASSERT_FALSE( process_idf( idf_objects ) );

	GetGroundHeatExchangerInput();

	EXPECT_EQ( 1, vertPropsVector.size() );

	auto & thisProp( vertPropsVector[0] );

	EXPECT_EQ( "GHE-1 PROPS", thisProp->name );
	EXPECT_EQ( 1, thisProp->bhTopDepth );
	EXPECT_EQ( 110, thisProp->bhLength );
	EXPECT_EQ( 0.109982, thisProp->bhDiameter );
	EXPECT_EQ( 0.744, thisProp->grout.k );
	EXPECT_EQ( 3.90E+06, thisProp->grout.rhoCp );
	EXPECT_EQ( 0.389, thisProp->pipe.k );
	EXPECT_EQ( 0.0267, thisProp->pipe.outDia );
	EXPECT_EQ( 0.00243, thisProp->pipe.thickness );
	EXPECT_EQ( 0.01887, thisProp->bhUTubeDist );

}

TEST_F( EnergyPlusFixture, GroundHeatExchangerTest_System_Resp_Factors_IDF_Check )
{
	std::string const idf_objects = delimited_string({
		"GroundHeatExchanger:Vertical:Properties,",
		"	GHE-1 Props,        !- Name",
		"	1,                  !- Depth of Top of Borehole {m}",
		"	110,                !- Borehole Length {m}",
		"	0.109982,           !- Borehole Diameter {m}",
		"	0.744,              !- Grout Thermal Conductivity {W/m-K}",
		"	3.90E+06,           !- Grout Thermal Heat Capacity {J/m3-K}",
		"	0.389,              !- Pipe Thermal Conductivity {W/m-K}",
		"	0.0267,             !- Pipe Outer Diameter {m}",
		"	0.00243,            !- Pipe Thickness {m}",
		"	0.01887;            !- U-Tube Distance {m}",

		"GroundHeatExchanger:ResponseFactors,",
		"	GHE-1 g-functions,       !- Name",
		"	GHE-1 Props,             !- GHE Properties",
		"	4,                       !- Number of Boreholes",
		"	0.00043,                 !- G-Function Reference Ratio {dimensionless}",
		"	-15.585075,              !- G-Function Ln(T/Ts) Value 1",
		"	-2.672011,               !- G-Function G Value 1",
		"	-15.440481,              !- G-Function Ln(T/Ts) Value 2",
		"	-2.575897,               !- G-Function G Value 2",
		"	-15.295888,              !- G-Function Ln(T/Ts) Value 3",
		"	-2.476279,               !- G-Function G Value 3",
		"	-15.151295,              !- G-Function Ln(T/Ts) Value 4",
		"	-2.372609,               !- G-Function G Value 4",
		"	-15.006701,              !- G-Function Ln(T/Ts) Value 5",
		"	-2.264564,               !- G-Function G Value 5",
		"	-14.862108,              !- G-Function Ln(T/Ts) Value 6",
		"	-2.151959,               !- G-Function G Value 6",
		"	-14.717515,              !- G-Function Ln(T/Ts) Value 7",
		"	-2.034708,               !- G-Function G Value 7",
		"	-14.572921,              !- G-Function Ln(T/Ts) Value 8",
		"	-1.912801,               !- G-Function G Value 8",
		"	-14.428328,              !- G-Function Ln(T/Ts) Value 9",
		"	-1.786299,               !- G-Function G Value 9",
		"	-14.283734,              !- G-Function Ln(T/Ts) Value 10",
		"	-1.655324,               !- G-Function G Value 10",
		"	-14.139141,              !- G-Function Ln(T/Ts) Value 11",
		"	-1.520066,               !- G-Function G Value 11",
		"	-13.994548,              !- G-Function Ln(T/Ts) Value 12",
		"	-1.380782,               !- G-Function G Value 12",
		"	-13.849954,              !- G-Function Ln(T/Ts) Value 13",
		"	-1.237813,               !- G-Function G Value 13",
		"	-13.705361,              !- G-Function Ln(T/Ts) Value 14",
		"	-1.091594,               !- G-Function G Value 14",
		"	-13.560768,              !- G-Function Ln(T/Ts) Value 15",
		"	-0.942670,               !- G-Function G Value 15",
		"	-13.416174,              !- G-Function Ln(T/Ts) Value 16",
		"	-0.791704,               !- G-Function G Value 16",
		"	-13.271581,              !- G-Function Ln(T/Ts) Value 17",
		"	-0.639479,               !- G-Function G Value 17",
		"	-13.126988,              !- G-Function Ln(T/Ts) Value 18",
		"	-0.486879,               !- G-Function G Value 18",
		"	-12.982394,              !- G-Function Ln(T/Ts) Value 19",
		"	-0.334866,               !- G-Function G Value 19",
		"	-12.837801,              !- G-Function Ln(T/Ts) Value 20",
		"	-0.184431,               !- G-Function G Value 20",
		"	-12.693207,              !- G-Function Ln(T/Ts) Value 21",
		"	-0.036546,               !- G-Function G Value 21",
		"	-12.548614,              !- G-Function Ln(T/Ts) Value 22",
		"	0.107892,                !- G-Function G Value 22",
		"	-12.404021,              !- G-Function Ln(T/Ts) Value 23",
		"	0.248115,                !- G-Function G Value 23",
		"	-12.259427,              !- G-Function Ln(T/Ts) Value 24",
		"	0.383520,                !- G-Function G Value 24",
		"	-12.114834,              !- G-Function Ln(T/Ts) Value 25",
		"	0.513700,                !- G-Function G Value 25",
		"	-11.970241,              !- G-Function Ln(T/Ts) Value 26",
		"	0.638450,                !- G-Function G Value 26",
		"	-11.825647,              !- G-Function Ln(T/Ts) Value 27",
		"	0.757758,                !- G-Function G Value 27",
		"	-11.681054,              !- G-Function Ln(T/Ts) Value 28",
		"	0.871780,                !- G-Function G Value 28",
		"	-11.536461,              !- G-Function Ln(T/Ts) Value 29",
		"	0.980805,                !- G-Function G Value 29",
		"	-11.391867,              !- G-Function Ln(T/Ts) Value 30",
		"	1.085218,                !- G-Function G Value 30",
		"	-11.247274,              !- G-Function Ln(T/Ts) Value 31",
		"	1.185457,                !- G-Function G Value 31",
		"	-11.102680,              !- G-Function Ln(T/Ts) Value 32",
		"	1.281980,                !- G-Function G Value 32",
		"	-10.958087,              !- G-Function Ln(T/Ts) Value 33",
		"	1.375237,                !- G-Function G Value 33",
		"	-10.813494,              !- G-Function Ln(T/Ts) Value 34",
		"	1.465651,                !- G-Function G Value 34",
		"	-10.668900,              !- G-Function Ln(T/Ts) Value 35",
		"	1.553606,                !- G-Function G Value 35",
		"	-10.524307,              !- G-Function Ln(T/Ts) Value 36",
		"	1.639445,                !- G-Function G Value 36",
		"	-10.379714,              !- G-Function Ln(T/Ts) Value 37",
		"	1.723466,                !- G-Function G Value 37",
		"	-10.235120,              !- G-Function Ln(T/Ts) Value 38",
		"	1.805924,                !- G-Function G Value 38",
		"	-10.090527,              !- G-Function Ln(T/Ts) Value 39",
		"	1.887041,                !- G-Function G Value 39",
		"	-9.945934,               !- G-Function Ln(T/Ts) Value 40",
		"	1.967002,                !- G-Function G Value 40",
		"	-9.801340,               !- G-Function Ln(T/Ts) Value 41",
		"	2.045967,                !- G-Function G Value 41",
		"	-9.656747,               !- G-Function Ln(T/Ts) Value 42",
		"	2.124073,                !- G-Function G Value 42",
		"	-9.512154,               !- G-Function Ln(T/Ts) Value 43",
		"	2.201436,                !- G-Function G Value 43",
		"	-9.367560,               !- G-Function Ln(T/Ts) Value 44",
		"	2.278154,                !- G-Function G Value 44",
		"	-9.222967,               !- G-Function Ln(T/Ts) Value 45",
		"	2.354312,                !- G-Function G Value 45",
		"	-9.078373,               !- G-Function Ln(T/Ts) Value 46",
		"	2.429984,                !- G-Function G Value 46",
		"	-8.933780,               !- G-Function Ln(T/Ts) Value 47",
		"	2.505232,                !- G-Function G Value 47",
		"	-8.789187,               !- G-Function Ln(T/Ts) Value 48",
		"	2.580112,                !- G-Function G Value 48",
		"	-8.644593,               !- G-Function Ln(T/Ts) Value 49",
		"	2.654669,                !- G-Function G Value 49",
		"	-8.500000,               !- G-Function Ln(T/Ts) Value 50",
		"	2.830857,                !- G-Function G Value 50",
		"	-7.800000,               !- G-Function Ln(T/Ts) Value 51",
		"	3.176174,                !- G-Function G Value 51",
		"	-7.200000,               !- G-Function Ln(T/Ts) Value 52",
		"	3.484017,                !- G-Function G Value 52",
		"	-6.500000,               !- G-Function Ln(T/Ts) Value 53",
		"	3.887770,                !- G-Function G Value 53",
		"	-5.900000,               !- G-Function Ln(T/Ts) Value 54",
		"	4.311301,                !- G-Function G Value 54",
		"	-5.200000,               !- G-Function Ln(T/Ts) Value 55",
		"	4.928223,                !- G-Function G Value 55",
		"	-4.500000,               !- G-Function Ln(T/Ts) Value 56",
		"	5.696283,                !- G-Function G Value 56",
		"	-3.963000,               !- G-Function Ln(T/Ts) Value 57",
		"	6.361422,                !- G-Function G Value 57",
		"	-3.270000,               !- G-Function Ln(T/Ts) Value 58",
		"	7.375959,                !- G-Function G Value 58",
		"	-2.864000,               !- G-Function Ln(T/Ts) Value 59",
		"	7.994729,                !- G-Function G Value 59",
		"	-2.577000,               !- G-Function Ln(T/Ts) Value 60",
		"	8.438474,                !- G-Function G Value 60",
		"	-2.171000,               !- G-Function Ln(T/Ts) Value 61",
		"	9.059916,                !- G-Function G Value 61",
		"	-1.884000,               !- G-Function Ln(T/Ts) Value 62",
		"	9.492228,                !- G-Function G Value 62",
		"	-1.191000,               !- G-Function Ln(T/Ts) Value 63",
		"	10.444276,               !- G-Function G Value 63",
		"	-0.497000,               !- G-Function Ln(T/Ts) Value 64",
		"	11.292233,               !- G-Function G Value 64",
		"	-0.274000,               !- G-Function Ln(T/Ts) Value 65",
		"	11.525537,               !- G-Function G Value 65",
		"	-0.051000,               !- G-Function Ln(T/Ts) Value 66",
		"	11.735157,               !- G-Function G Value 66",
		"	0.196000,                !- G-Function Ln(T/Ts) Value 67",
		"	11.942392,               !- G-Function G Value 67",
		"	0.419000,                !- G-Function Ln(T/Ts) Value 68",
		"	12.103282,               !- G-Function G Value 68",
		"	0.642000,                !- G-Function Ln(T/Ts) Value 69",
		"	12.243398,               !- G-Function G Value 69",
		"	0.873000,                !- G-Function Ln(T/Ts) Value 70",
		"	12.365217,               !- G-Function G Value 70",
		"	1.112000,                !- G-Function Ln(T/Ts) Value 71",
		"	12.469007,               !- G-Function G Value 71",
		"	1.335000,                !- G-Function Ln(T/Ts) Value 72",
		"	12.547123,               !- G-Function G Value 72",
		"	1.679000,                !- G-Function Ln(T/Ts) Value 73",
		"	12.637890,               !- G-Function G Value 73",
		"	2.028000,                !- G-Function Ln(T/Ts) Value 74",
		"	12.699245,               !- G-Function G Value 74",
		"	2.275000,                !- G-Function Ln(T/Ts) Value 75",
		"	12.729288,               !- G-Function G Value 75",
		"	3.003000,                !- G-Function Ln(T/Ts) Value 76",
		"	12.778359;               !- G-Function G Value 76"
	});

	ASSERT_FALSE( process_idf( idf_objects ) );

	GetGroundHeatExchangerInput();

	EXPECT_EQ( 1, responseFactorsVector.size() );

	auto & thisRF( responseFactorsVector[0] );

	EXPECT_EQ( "GHE-1 G-FUNCTIONS", thisRF->name );
	EXPECT_EQ( "GHE-1 PROPS", thisRF->props->name );
	EXPECT_EQ( 4, thisRF->numBoreholes );
	EXPECT_EQ( 0.00043, thisRF->gRefRatio );
	EXPECT_EQ( 76, thisRF->numGFuncPairs );
	EXPECT_EQ( -15.585075, thisRF->LNTTS( 1 ) );
	EXPECT_EQ( -2.672011, thisRF->GFNC( 1 ) );
	EXPECT_EQ( 3.003000, thisRF->LNTTS( 76 ) );
	EXPECT_EQ( 12.778359, thisRF->GFNC( 76 ) );
}

TEST_F( EnergyPlusFixture, GroundHeatExchangerTest_System_Vertical_Array_IDF_Check )
{
	std::string const idf_objects = delimited_string({
		"GroundHeatExchanger:Vertical:Properties,",
		"	GHE-1 Props,        !- Name",
		"	1,                  !- Depth of Top of Borehole {m}",
		"	110,                !- Borehole Length {m}",
		"	0.109982,           !- Borehole Diameter {m}",
		"	0.744,              !- Grout Thermal Conductivity {W/m-K}",
		"	3.90E+06,           !- Grout Thermal Heat Capacity {J/m3-K}",
		"	0.389,              !- Pipe Thermal Conductivity {W/m-K}",
		"	0.0267,             !- Pipe Outer Diameter {m}",
		"	0.00243,            !- Pipe Thickness {m}",
		"	0.01887;            !- U-Tube Distance {m}",

		"GroundHeatExchanger:Vertical:Array,",
		"	GHE-Array,          !- Name",
		"	GHE-1 Props,        !- GHE Properties",
		"	2,                  !- Number of Boreholes in X Direction",
		"	2,                  !- Number of Boreholes in Y Direction",
		"	2;                  !- Borehole Spacing {m}",
	});

	ASSERT_FALSE( process_idf( idf_objects ) );

	GetGroundHeatExchangerInput();

	EXPECT_EQ( 1, vertArraysVector.size() );

	auto & thisArray( vertArraysVector[0] );

	EXPECT_EQ( "GHE-ARRAY", thisArray->name );
	EXPECT_EQ( "GHE-1 PROPS", thisArray->props->name );
	EXPECT_EQ( 2 , thisArray->numBHinXDirection );
	EXPECT_EQ( 2 , thisArray->numBHinYDirection );
}

TEST_F( EnergyPlusFixture, GroundHeatExchangerTest_System_Given_Response_Factors_IDF_Check )
{
	std::string const idf_objects = delimited_string({
		"Site:GroundTemperature:Undisturbed:KusudaAchenbach,",
		"	KATemps,                 !- Name",
		"	1.8,                     !- Soil Thermal Conductivity {W/m-K}",
		"	920,                     !- Soil Density {kg/m3}",
		"	2200,                    !- Soil Specific Heat {J/kg-K}",
		"	15.5,                    !- Average Soil Surface Temperature {C}",
		"	3.2,                     !- Average Amplitude of Surface Temperature {deltaC}",
		"	8;                       !- Phase Shift of Minimum Surface Temperature {days}",

		"GroundHeatExchanger:Vertical:Properties,",
		"	GHE-1 Props,        !- Name",
		"	1,                  !- Depth of Top of Borehole {m}",
		"	110,                !- Borehole Length {m}",
		"	0.109982,           !- Borehole Diameter {m}",
		"	0.744,              !- Grout Thermal Conductivity {W/m-K}",
		"	3.90E+06,           !- Grout Thermal Heat Capacity {J/m3-K}",
		"	0.389,              !- Pipe Thermal Conductivity {W/m-K}",
		"	0.0267,             !- Pipe Outer Diameter {m}",
		"	0.00243,            !- Pipe Thickness {m}",
		"	0.01887;            !- U-Tube Distance {m}",

		"GroundHeatExchanger:ResponseFactors,",
		"	GHE-1 g-functions,       !- Name",
		"	GHE-1 Props,             !- GHE Properties",
		"	4,                       !- Number of Boreholes",
		"	0.00043,                 !- G-Function Reference Ratio {dimensionless}",
		"	-15.585075,              !- G-Function Ln(T/Ts) Value 1",
		"	-2.672011,               !- G-Function G Value 1",
		"	-15.440481,              !- G-Function Ln(T/Ts) Value 2",
		"	-2.575897,               !- G-Function G Value 2",
		"	-15.295888,              !- G-Function Ln(T/Ts) Value 3",
		"	-2.476279,               !- G-Function G Value 3",
		"	-15.151295,              !- G-Function Ln(T/Ts) Value 4",
		"	-2.372609,               !- G-Function G Value 4",
		"	-15.006701,              !- G-Function Ln(T/Ts) Value 5",
		"	-2.264564,               !- G-Function G Value 5",
		"	-14.862108,              !- G-Function Ln(T/Ts) Value 6",
		"	-2.151959,               !- G-Function G Value 6",
		"	-14.717515,              !- G-Function Ln(T/Ts) Value 7",
		"	-2.034708,               !- G-Function G Value 7",
		"	-14.572921,              !- G-Function Ln(T/Ts) Value 8",
		"	-1.912801,               !- G-Function G Value 8",
		"	-14.428328,              !- G-Function Ln(T/Ts) Value 9",
		"	-1.786299,               !- G-Function G Value 9",
		"	-14.283734,              !- G-Function Ln(T/Ts) Value 10",
		"	-1.655324,               !- G-Function G Value 10",
		"	-14.139141,              !- G-Function Ln(T/Ts) Value 11",
		"	-1.520066,               !- G-Function G Value 11",
		"	-13.994548,              !- G-Function Ln(T/Ts) Value 12",
		"	-1.380782,               !- G-Function G Value 12",
		"	-13.849954,              !- G-Function Ln(T/Ts) Value 13",
		"	-1.237813,               !- G-Function G Value 13",
		"	-13.705361,              !- G-Function Ln(T/Ts) Value 14",
		"	-1.091594,               !- G-Function G Value 14",
		"	-13.560768,              !- G-Function Ln(T/Ts) Value 15",
		"	-0.942670,               !- G-Function G Value 15",
		"	-13.416174,              !- G-Function Ln(T/Ts) Value 16",
		"	-0.791704,               !- G-Function G Value 16",
		"	-13.271581,              !- G-Function Ln(T/Ts) Value 17",
		"	-0.639479,               !- G-Function G Value 17",
		"	-13.126988,              !- G-Function Ln(T/Ts) Value 18",
		"	-0.486879,               !- G-Function G Value 18",
		"	-12.982394,              !- G-Function Ln(T/Ts) Value 19",
		"	-0.334866,               !- G-Function G Value 19",
		"	-12.837801,              !- G-Function Ln(T/Ts) Value 20",
		"	-0.184431,               !- G-Function G Value 20",
		"	-12.693207,              !- G-Function Ln(T/Ts) Value 21",
		"	-0.036546,               !- G-Function G Value 21",
		"	-12.548614,              !- G-Function Ln(T/Ts) Value 22",
		"	0.107892,                !- G-Function G Value 22",
		"	-12.404021,              !- G-Function Ln(T/Ts) Value 23",
		"	0.248115,                !- G-Function G Value 23",
		"	-12.259427,              !- G-Function Ln(T/Ts) Value 24",
		"	0.383520,                !- G-Function G Value 24",
		"	-12.114834,              !- G-Function Ln(T/Ts) Value 25",
		"	0.513700,                !- G-Function G Value 25",
		"	-11.970241,              !- G-Function Ln(T/Ts) Value 26",
		"	0.638450,                !- G-Function G Value 26",
		"	-11.825647,              !- G-Function Ln(T/Ts) Value 27",
		"	0.757758,                !- G-Function G Value 27",
		"	-11.681054,              !- G-Function Ln(T/Ts) Value 28",
		"	0.871780,                !- G-Function G Value 28",
		"	-11.536461,              !- G-Function Ln(T/Ts) Value 29",
		"	0.980805,                !- G-Function G Value 29",
		"	-11.391867,              !- G-Function Ln(T/Ts) Value 30",
		"	1.085218,                !- G-Function G Value 30",
		"	-11.247274,              !- G-Function Ln(T/Ts) Value 31",
		"	1.185457,                !- G-Function G Value 31",
		"	-11.102680,              !- G-Function Ln(T/Ts) Value 32",
		"	1.281980,                !- G-Function G Value 32",
		"	-10.958087,              !- G-Function Ln(T/Ts) Value 33",
		"	1.375237,                !- G-Function G Value 33",
		"	-10.813494,              !- G-Function Ln(T/Ts) Value 34",
		"	1.465651,                !- G-Function G Value 34",
		"	-10.668900,              !- G-Function Ln(T/Ts) Value 35",
		"	1.553606,                !- G-Function G Value 35",
		"	-10.524307,              !- G-Function Ln(T/Ts) Value 36",
		"	1.639445,                !- G-Function G Value 36",
		"	-10.379714,              !- G-Function Ln(T/Ts) Value 37",
		"	1.723466,                !- G-Function G Value 37",
		"	-10.235120,              !- G-Function Ln(T/Ts) Value 38",
		"	1.805924,                !- G-Function G Value 38",
		"	-10.090527,              !- G-Function Ln(T/Ts) Value 39",
		"	1.887041,                !- G-Function G Value 39",
		"	-9.945934,               !- G-Function Ln(T/Ts) Value 40",
		"	1.967002,                !- G-Function G Value 40",
		"	-9.801340,               !- G-Function Ln(T/Ts) Value 41",
		"	2.045967,                !- G-Function G Value 41",
		"	-9.656747,               !- G-Function Ln(T/Ts) Value 42",
		"	2.124073,                !- G-Function G Value 42",
		"	-9.512154,               !- G-Function Ln(T/Ts) Value 43",
		"	2.201436,                !- G-Function G Value 43",
		"	-9.367560,               !- G-Function Ln(T/Ts) Value 44",
		"	2.278154,                !- G-Function G Value 44",
		"	-9.222967,               !- G-Function Ln(T/Ts) Value 45",
		"	2.354312,                !- G-Function G Value 45",
		"	-9.078373,               !- G-Function Ln(T/Ts) Value 46",
		"	2.429984,                !- G-Function G Value 46",
		"	-8.933780,               !- G-Function Ln(T/Ts) Value 47",
		"	2.505232,                !- G-Function G Value 47",
		"	-8.789187,               !- G-Function Ln(T/Ts) Value 48",
		"	2.580112,                !- G-Function G Value 48",
		"	-8.644593,               !- G-Function Ln(T/Ts) Value 49",
		"	2.654669,                !- G-Function G Value 49",
		"	-8.500000,               !- G-Function Ln(T/Ts) Value 50",
		"	2.830857,                !- G-Function G Value 50",
		"	-7.800000,               !- G-Function Ln(T/Ts) Value 51",
		"	3.176174,                !- G-Function G Value 51",
		"	-7.200000,               !- G-Function Ln(T/Ts) Value 52",
		"	3.484017,                !- G-Function G Value 52",
		"	-6.500000,               !- G-Function Ln(T/Ts) Value 53",
		"	3.887770,                !- G-Function G Value 53",
		"	-5.900000,               !- G-Function Ln(T/Ts) Value 54",
		"	4.311301,                !- G-Function G Value 54",
		"	-5.200000,               !- G-Function Ln(T/Ts) Value 55",
		"	4.928223,                !- G-Function G Value 55",
		"	-4.500000,               !- G-Function Ln(T/Ts) Value 56",
		"	5.696283,                !- G-Function G Value 56",
		"	-3.963000,               !- G-Function Ln(T/Ts) Value 57",
		"	6.361422,                !- G-Function G Value 57",
		"	-3.270000,               !- G-Function Ln(T/Ts) Value 58",
		"	7.375959,                !- G-Function G Value 58",
		"	-2.864000,               !- G-Function Ln(T/Ts) Value 59",
		"	7.994729,                !- G-Function G Value 59",
		"	-2.577000,               !- G-Function Ln(T/Ts) Value 60",
		"	8.438474,                !- G-Function G Value 60",
		"	-2.171000,               !- G-Function Ln(T/Ts) Value 61",
		"	9.059916,                !- G-Function G Value 61",
		"	-1.884000,               !- G-Function Ln(T/Ts) Value 62",
		"	9.492228,                !- G-Function G Value 62",
		"	-1.191000,               !- G-Function Ln(T/Ts) Value 63",
		"	10.444276,               !- G-Function G Value 63",
		"	-0.497000,               !- G-Function Ln(T/Ts) Value 64",
		"	11.292233,               !- G-Function G Value 64",
		"	-0.274000,               !- G-Function Ln(T/Ts) Value 65",
		"	11.525537,               !- G-Function G Value 65",
		"	-0.051000,               !- G-Function Ln(T/Ts) Value 66",
		"	11.735157,               !- G-Function G Value 66",
		"	0.196000,                !- G-Function Ln(T/Ts) Value 67",
		"	11.942392,               !- G-Function G Value 67",
		"	0.419000,                !- G-Function Ln(T/Ts) Value 68",
		"	12.103282,               !- G-Function G Value 68",
		"	0.642000,                !- G-Function Ln(T/Ts) Value 69",
		"	12.243398,               !- G-Function G Value 69",
		"	0.873000,                !- G-Function Ln(T/Ts) Value 70",
		"	12.365217,               !- G-Function G Value 70",
		"	1.112000,                !- G-Function Ln(T/Ts) Value 71",
		"	12.469007,               !- G-Function G Value 71",
		"	1.335000,                !- G-Function Ln(T/Ts) Value 72",
		"	12.547123,               !- G-Function G Value 72",
		"	1.679000,                !- G-Function Ln(T/Ts) Value 73",
		"	12.637890,               !- G-Function G Value 73",
		"	2.028000,                !- G-Function Ln(T/Ts) Value 74",
		"	12.699245,               !- G-Function G Value 74",
		"	2.275000,                !- G-Function Ln(T/Ts) Value 75",
		"	12.729288,               !- G-Function G Value 75",
		"	3.003000,                !- G-Function Ln(T/Ts) Value 76",
		"	12.778359;               !- G-Function G Value 76",

		"GroundHeatExchanger:System,",
		"	Vertical GHE 1x4 Std,  !- Name",
		"	GHLE Inlet,         !- Inlet Node Name",
		"	GHLE Outlet,        !- Outlet Node Name",
		"	0.0007571,          !- Design Flow Rate {m3/s}",
		"	Site:GroundTemperature:Undisturbed:KusudaAchenbach,  !- Undisturbed Ground Temperature Model Type",
		"	KATemps,            !- Undisturbed Ground Temperature Model Name",
		"	2.423,              !- Ground Thermal Conductivity {W/m-K}",
		"	2.343E+06,          !- Ground Thermal Heat Capacity {J/m3-K}",
		"	GHE-1 g-functions;  !- Response Factors Object Name",
	});

	ASSERT_FALSE( process_idf( idf_objects ) );

	GetGroundHeatExchangerInput();

	EXPECT_EQ( 1, vertPropsVector.size() );
	EXPECT_EQ( 1, responseFactorsVector.size() );
	EXPECT_EQ( 1, verticalGLHE.size() );

	auto & thisProp( vertPropsVector[0] );
	auto & thisRF( responseFactorsVector[0] );
	auto & thisGLHE( verticalGLHE[0] );

	EXPECT_EQ( "VERTICAL GHE 1X4 STD", thisGLHE.name );
	EXPECT_EQ( true, thisGLHE.available );
	EXPECT_EQ( true, thisGLHE.on );
	EXPECT_EQ( 2.423, thisGLHE.soil.k );
	EXPECT_EQ( 2.343E6, thisGLHE.soil.rhoCp );
	EXPECT_EQ( GetResponseFactor( thisRF->name ), thisGLHE.myRespFactors );
	EXPECT_EQ( 0.109982, thisGLHE.bhDiameter );
	EXPECT_EQ( 0.109982 / 2, thisGLHE.bhRadius );
	EXPECT_EQ( 110, thisGLHE.bhLength );
	EXPECT_EQ( 0.01887, thisGLHE.bhUTubeDist );
	EXPECT_EQ( 0, thisGLHE.myRespFactors->maxSimYears );
	EXPECT_EQ( 440, thisGLHE.totalTubeLength );
	EXPECT_EQ( thisGLHE.soil.k / thisGLHE.soil.rhoCp, thisGLHE.soil.diffusivity );
}

TEST_F( EnergyPlusFixture, GroundHeatExchangerTest_System_Given_Array_IDF_Check )
{
	std::string const idf_objects = delimited_string( {
		"Site:GroundTemperature:Undisturbed:KusudaAchenbach,",
		"	KATemps,                 !- Name",
		"	1.8,                     !- Soil Thermal Conductivity {W/m-K}",
		"	920,                     !- Soil Density {kg/m3}",
		"	2200,                    !- Soil Specific Heat {J/kg-K}",
		"	15.5,                    !- Average Soil Surface Temperature {C}",
		"	3.2,                     !- Average Amplitude of Surface Temperature {deltaC}",
		"	8;                       !- Phase Shift of Minimum Surface Temperature {days}",

		"GroundHeatExchanger:Vertical:Properties,",
		"	GHE-1 Props,        !- Name",
		"	1,                  !- Depth of Top of Borehole {m}",
		"	110,                !- Borehole Length {m}",
		"	0.109982,           !- Borehole Diameter {m}",
		"	0.744,              !- Grout Thermal Conductivity {W/m-K}",
		"	3.90E+06,           !- Grout Thermal Heat Capacity {J/m3-K}",
		"	0.389,              !- Pipe Thermal Conductivity {W/m-K}",
		"	0.0267,             !- Pipe Outer Diameter {m}",
		"	0.00243,            !- Pipe Thickness {m}",
		"	0.01887;            !- U-Tube Distance {m}",

		"GroundHeatExchanger:Vertical:Array,",
		"	GHE-Array,          !- Name",
		"	GHE-1 Props,        !- GHE Properties",
		"	2,                  !- Number of Boreholes in X Direction",
		"	2,                  !- Number of Boreholes in Y Direction",
		"	2;                  !- Borehole Spacing {m}",

		"GroundHeatExchanger:System,",
		"	Vertical GHE 1x4 Std,  !- Name",
		"	GHLE Inlet,         !- Inlet Node Name",
		"	GHLE Outlet,        !- Outlet Node Name",
		"	0.0007571,          !- Design Flow Rate {m3/s}",
		"	Site:GroundTemperature:Undisturbed:KusudaAchenbach,  !- Undisturbed Ground Temperature Model Type",
		"	KATemps,            !- Undisturbed Ground Temperature Model Name",
		"	2.423,              !- Ground Thermal Conductivity {W/m-K}",
		"	2.343E+06,          !- Ground Thermal Heat Capacity {J/m3-K}",
		"	,					!- Response Factors Object Name",
		"	GHE-Array;			!- !- GHE Array Object Name"
	});

	ASSERT_FALSE( process_idf( idf_objects ) );

	GetGroundHeatExchangerInput();

	EXPECT_EQ( 1, vertPropsVector.size() );
	EXPECT_EQ( 1, vertArraysVector.size() );
	EXPECT_EQ( 1, verticalGLHE.size() );

	auto & thisProp( vertPropsVector[0] );
	auto & thisArray( vertArraysVector[0] );
	auto & thisGLHE( verticalGLHE[0] );

	EXPECT_EQ( "VERTICAL GHE 1X4 STD", thisGLHE.name );
	EXPECT_EQ( true, thisGLHE.available );
	EXPECT_EQ( true, thisGLHE.on );
	EXPECT_EQ( 2.423, thisGLHE.soil.k );
	EXPECT_EQ( 2.343E6, thisGLHE.soil.rhoCp );
	EXPECT_EQ( GetResponseFactor( thisArray->name ), thisGLHE.myRespFactors );
	EXPECT_EQ( 0.109982, thisGLHE.bhDiameter );
	EXPECT_EQ( 0.109982 / 2, thisGLHE.bhRadius );
	EXPECT_EQ( 110, thisGLHE.bhLength );
	EXPECT_EQ( 0.01887, thisGLHE.bhUTubeDist );
	EXPECT_EQ( 0, thisGLHE.myRespFactors->maxSimYears );
	EXPECT_EQ( 440, thisGLHE.totalTubeLength );
	EXPECT_EQ( thisGLHE.soil.k / thisGLHE.soil.rhoCp, thisGLHE.soil.diffusivity );
}

TEST_F( EnergyPlusFixture, GroundHeatExchangerTest_System_Given_Single_BHs_IDF_Check )
{
	std::string const idf_objects = delimited_string( {
		"Site:GroundTemperature:Undisturbed:KusudaAchenbach,",
		"	KATemps,                 !- Name",
		"	1.8,                     !- Soil Thermal Conductivity {W/m-K}",
		"	920,                     !- Soil Density {kg/m3}",
		"	2200,                    !- Soil Specific Heat {J/kg-K}",
		"	15.5,                    !- Average Soil Surface Temperature {C}",
		"	3.2,                     !- Average Amplitude of Surface Temperature {deltaC}",
		"	8;                       !- Phase Shift of Minimum Surface Temperature {days}",

		"GroundHeatExchanger:Vertical:Properties,",
		"	GHE-1 Props,        !- Name",
		"	1,                  !- Depth of Top of Borehole {m}",
		"	110,                !- Borehole Length {m}",
		"	0.109982,           !- Borehole Diameter {m}",
		"	0.744,              !- Grout Thermal Conductivity {W/m-K}",
		"	3.90E+06,           !- Grout Thermal Heat Capacity {J/m3-K}",
		"	0.389,              !- Pipe Thermal Conductivity {W/m-K}",
		"	0.0267,             !- Pipe Outer Diameter {m}",
		"	0.00243,            !- Pipe Thickness {m}",
		"	0.01887;            !- U-Tube Distance {m}",

		"GroundHeatExchanger:Vertical:Single,",
		"	GHE-1,              !- Name",
		"	GHE-1 Props,        !- GHE Properties",
		"	0,                  !- X Location {m}",
		"	0;                  !- Y Location {m}",

		"GroundHeatExchanger:Vertical:Single,",
		"	GHE-2,              !- Name",
		"	GHE-1 Props,        !- GHE Properties",
		"	5.5,                !- X Location {m}",
		"	0;                  !- Y Location {m}",

		"GroundHeatExchanger:Vertical:Single,",
		"	GHE-3,              !- Name",
		"	GHE-1 Props,        !- GHE Properties",
		"	0,                  !- X Location {m}",
		"	5.5;                !- Y Location {m}",

		"GroundHeatExchanger:Vertical:Single,",
		"	GHE-4,              !- Name",
		"	GHE-1 Props,        !- GHE Properties",
		"	5.5,                !- X Location {m}",
		"	5.5;                !- Y Location {m}",

		"GroundHeatExchanger:System,",
		"	Vertical GHE 1x4 Std,  !- Name",
		"	GHLE Inlet,         !- Inlet Node Name",
		"	GHLE Outlet,        !- Outlet Node Name",
		"	0.0007571,          !- Design Flow Rate {m3/s}",
		"	Site:GroundTemperature:Undisturbed:KusudaAchenbach,  !- Undisturbed Ground Temperature Model Type",
		"	KATemps,            !- Undisturbed Ground Temperature Model Name",
		"	2.423,              !- Ground Thermal Conductivity {W/m-K}",
		"	2.343E+06,          !- Ground Thermal Heat Capacity {J/m3-K}",
		"	,					!- Response Factors Object Name",
		"	,                   !- GHE Array Object Name",
		"	GHE-1,              !- GHE Borehole Definition 1",
		"	GHE-2,              !- GHE Borehole Definition 2",
		"	GHE-3,              !- GHE Borehole Definition 3",
		"	GHE-4;              !- GHE Borehole Definition 4"
	});

	ASSERT_FALSE( process_idf( idf_objects ) );

	GetGroundHeatExchangerInput();

	EXPECT_EQ( 2, vertPropsVector.size() );
	EXPECT_EQ( 4, singleBoreholesVector.size() );
	EXPECT_EQ( 1, verticalGLHE.size() );

	auto & thisProp( vertPropsVector[0] );
	auto & thisGLHE( verticalGLHE[0] );

	EXPECT_EQ( "VERTICAL GHE 1X4 STD", thisGLHE.name );
	EXPECT_EQ( true, thisGLHE.available );
	EXPECT_EQ( true, thisGLHE.on );
	EXPECT_EQ( 2.423, thisGLHE.soil.k );
	EXPECT_EQ( 2.343E6, thisGLHE.soil.rhoCp );
	EXPECT_EQ( 0.109982, thisGLHE.bhDiameter );
	EXPECT_EQ( 0.109982 / 2, thisGLHE.bhRadius );
	EXPECT_EQ( 110, thisGLHE.bhLength );
	EXPECT_EQ( 0.01887, thisGLHE.bhUTubeDist );
	EXPECT_EQ( 0, thisGLHE.myRespFactors->maxSimYears );
	EXPECT_EQ( 440, thisGLHE.totalTubeLength );
	EXPECT_EQ( thisGLHE.soil.k / thisGLHE.soil.rhoCp, thisGLHE.soil.diffusivity );
}
