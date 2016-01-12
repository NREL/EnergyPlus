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

	thisGLHE.NPairs = 2;

	thisGLHE.LNTTS.allocate( thisGLHE.NPairs );
	thisGLHE.GFNC.allocate( thisGLHE.NPairs );

	thisGLHE.LNTTS( 1 ) = 0.0;
	thisGLHE.LNTTS( 2 ) = 5.0;
	thisGLHE.GFNC( 1 ) = 0.0;
	thisGLHE.GFNC( 2 ) = 5.0;
	
	// Case when extrapolating beyond lower bound
	thisLNTTS = -1.0;
	thisGFunc = thisGLHE.interpGFunc( thisLNTTS );
	EXPECT_DOUBLE_EQ( -1.0, thisGFunc );

	// Case when extrapolating beyond opper bound
	thisLNTTS = 6.0;
	thisGFunc = thisGLHE.interpGFunc( thisLNTTS );
	EXPECT_DOUBLE_EQ( 6.0 , thisGFunc );

	// Case when we're actually interpolating
	thisLNTTS = 2.5;
	thisGFunc = thisGLHE.interpGFunc( thisLNTTS );
	EXPECT_DOUBLE_EQ( 2.5, thisGFunc );
}

TEST_F( EnergyPlusFixture, SlinkyGroundHeatExchangerTest_GetGFunc )
{

	// Initialization
	GLHESlinky thisGLHE;
	Real64 thisGFunc;
	Real64 time;
	
	thisGLHE.NPairs = 2;

	thisGLHE.LNTTS.allocate( thisGLHE.NPairs );
	thisGLHE.GFNC.allocate( thisGLHE.NPairs );

	thisGLHE.LNTTS( 1 ) = 0.0;
	thisGLHE.LNTTS( 2 ) = 5.0;
	thisGLHE.GFNC( 1 ) = 0.0;
	thisGLHE.GFNC( 2 ) = 5.0;

	time = std::pow( 10.0, 2.5 );

	thisGFunc = thisGLHE.getGFunc( time );

	EXPECT_EQ( 2.5, thisGFunc );
}

TEST_F( EnergyPlusFixture, VerticalGroundHeatExchangerTest_GetGFunc )
{

	// Initialization
	GLHEVert thisGLHE;
	Real64 thisGFunc;
	Real64 time;
	
	thisGLHE.NPairs = 2;

	thisGLHE.LNTTS.allocate( thisGLHE.NPairs );
	thisGLHE.GFNC.allocate( thisGLHE.NPairs );

	thisGLHE.LNTTS( 1 ) = 0.0;
	thisGLHE.LNTTS( 2 ) = 5.0;
	thisGLHE.GFNC( 1 ) = 0.0;
	thisGLHE.GFNC( 2 ) = 5.0;

	time = std::pow( 2.7182818284590452353602874, 2.5 );

	thisGLHE.boreholeLength = 1.0;
	thisGLHE.boreholeRadius = 1.0;

	// Situation when correction is not applied
	thisGLHE.gReferenceRatio = 1.0;
	thisGFunc = thisGLHE.getGFunc( time );
	EXPECT_DOUBLE_EQ( 2.5, thisGFunc );

	//Situation when correction is applied
	thisGLHE.gReferenceRatio = 2.0;
	thisGFunc = thisGLHE.getGFunc( time );
	EXPECT_NEAR( 2.5 + 0.6931, thisGFunc, 0.0001);

}

TEST_F( EnergyPlusFixture, SlinkyGroundHeatExchangerTest_CalcHXResistance )
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
	thisGLHE.pipeOutDia = 0.02667;
	thisGLHE.pipeThick = 0.004;
	thisGLHE.kPipe = 0.4;

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

TEST_F( EnergyPlusFixture, VerticalGroundHeatExchangerTest_CalcHXResistance )
{

	// Initializations
	GLHEVert thisGLHE;

	PlantLoop.allocate( 1 );
	thisGLHE.loopNum = 1;

	PlantLoop( thisGLHE.loopNum ).FluidName = "WATER";
	PlantLoop( thisGLHE.loopNum ).FluidIndex = 1;
	
	thisGLHE.inletTemp = 5.0;
	thisGLHE.massFlowRate = 0.01;
	thisGLHE.numBoreholes = 1;
	thisGLHE.pipeOutDia = 0.02667;
	thisGLHE.pipeThick = 0.004;
	thisGLHE.kPipe = 0.4;
	thisGLHE.boreholeRadius = 0.1;
	thisGLHE.kGrout = 1.0;

	// Re < 2300 mass flow rate; 0.0 <= distanceRatio <= 2.5 correction factor
	thisGLHE.calcHXResistance();
	EXPECT_NEAR( 0.49421, thisGLHE.HXResistance, 0.0001 );

	// Re < 2300 mass flow rate; 0.25 < distanceRatio < 0.5 correction factor
	thisGLHE.UtubeDist = 0.05;
	thisGLHE.calcHXResistance();
	EXPECT_NEAR( 0.46859, thisGLHE.HXResistance, 0.0001 );

	// Re < 2300 mass flow rate; 0.5 <= distanceRatio < 0.75 correction factor
	thisGLHE.UtubeDist = 0.087;
	thisGLHE.calcHXResistance();
	EXPECT_NEAR( 0.32891, thisGLHE.HXResistance, 0.0001 );

	// 4000 > Re > 2300 mass flow rate; all other distance ratios correction factor
	thisGLHE.UtubeDist = 0.12;
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

TEST_F( EnergyPlusFixture, SlinkyGroundHeatExchangerTest_CalcGroundHeatExchanger )
{

	// Initializations
	GLHESlinky thisGLHE;

	thisGLHE.numCoils = 100;
	thisGLHE.numTrenches = 2;
	thisGLHE.maxSimYears = 10;
	thisGLHE.coilPitch = 0.4;
	thisGLHE.coilDepth = 1.5;
	thisGLHE.coilDiameter = 0.8;
	thisGLHE.pipeOutDia = 0.034;
	thisGLHE.trenchSpacing = 3.0;
	thisGLHE.diffusivityGround = 3.0e-007;
	thisGLHE.AGG = 192;
	thisGLHE.SubAGG = 15;

	// Horizontal G-Functions
	thisGLHE.calcGFunctions();
	EXPECT_NEAR( 19.08237, thisGLHE.GFNC( 28 ), 0.0001 );

	// Vertical G-Functions
	thisGLHE.verticalConfig = true;
	thisGLHE.calcGFunctions();
	EXPECT_NEAR( 18.91819, thisGLHE.GFNC( 28 ), 0.0001 );

}

TEST_F( EnergyPlusFixture, VerticalGLHEBadIDF_1 ) 
{
	std::string const idf_objects = delimited_string({
		"Version,8.4;",
		"GroundHeatExchanger:Vertical,",
		"Vertical GHE JL2015,     !- Name",
		"GHEV Borehole Inlet Node,!- Inlet Node Name",
		"GHEV Borehole Outlet Node,  !- Outlet Node Name",
		"0.000303,                !- Design Flow Rate {m3/s}",
		"1,                       !- Number of Bore Holes",
		"160,                     !- Bore Hole Length {m}",
		"0.05715,                 !- Bore Hole Radius {m}",
		"2.493,                   !- Ground Thermal Conductivity {W/m-K}",
		"2495700,                 !- Ground Thermal Heat Capacity {J/m3-K}",
		"8,                       !- Ground Temperature {C}",
		"0.744,                   !- Grout Thermal Conductivity {W/m-K}",
		"0.389,                   !- Pipe Thermal Conductivity {W/m-K}",
		"0.0267,                  !- Pipe Out Diameter {m}",
		"0.0254,                  !- U-Tube Distance {m}",
		"0.00243,                 !- Pipe Thickness {m}",
		"0.01,                    !- Maximum Length of Simulation {years}",
		"0.0005,                  !- G-Function Reference Ratio {dimensionless}",
		"0.1,                     !- Number of Data Pairs of the G Function ---CANNOT BE LESS THAN 1",
		"-4.5,                    !- G-Function Ln(T/Ts) Value 1",
		"4.7,                     !- G-Function G Value 1",
		"-4,                      !- G-Function Ln(T/Ts) Value 2",
		"4.85,                    !- G-Function G Value 2",
		"-3.5,                    !- G-Function Ln(T/Ts) Value 3",
		"5.1,                     !- G-Function G Value 3",
		"-3,                      !- G-Function Ln(T/Ts) Value 4",
		"5.3,                     !- G-Function G Value 4",
		"-2.5,                    !- G-Function Ln(T/Ts) Value 5",
		"5.56,                    !- G-Function G Value 5",
		"-2,                      !- G-Function Ln(T/Ts) Value 6",
		"5.76,                    !- G-Function G Value 6",
		"-1.5,                    !- G-Function Ln(T/Ts) Value 7",
		"5.97,                    !- G-Function G Value 7",
		"-1,                      !- G-Function Ln(T/Ts) Value 8",
		"6.19,                    !- G-Function G Value 8",
		"-.5,                     !- G-Function Ln(T/Ts) Value 9",
		"6.31,                    !- G-Function G Value 9",
		"0,                       !- G-Function Ln(T/Ts) Value 10",
		"6.42,                    !- G-Function G Value 10",
		"0.5,                     !- G-Function Ln(T/Ts) Value 11",
		"6.56,                    !- G-Function G Value 11",
		"1,                       !- G-Function Ln(T/Ts) Value 12",
		"6.61,                    !- G-Function G Value 12",
		"1.5,                     !- G-Function Ln(T/Ts) Value 13",
		"6.66,                    !- G-Function G Value 13",
		"2,                       !- G-Function Ln(T/Ts) Value 14",
		"6.7,                     !- G-Function G Value 14",
		"2.5,                     !- G-Function Ln(T/Ts) Value 15",
		"6.72,                    !- G-Function G Value 15",
		"3,                       !- G-Function Ln(T/Ts) Value 16",
		"6.73;                    !- G-Function G Value 16",
	});

	EXPECT_TRUE( process_idf( idf_objects, false ) );

	EXPECT_ANY_THROW( GetGroundHeatExchangerInput() );

}

TEST_F( EnergyPlusFixture, VerticalGLHEBadIDF_2 )
{
		std::string const idf_objects = delimited_string({
		"Version,8.4;",
		"GroundHeatExchanger:Vertical,",
		"Vertical GHE JL2015,     !- Name",
		"GHEV Borehole Inlet Node,!- Inlet Node Name",
		"GHEV Borehole Outlet Node,  !- Outlet Node Name",
		"0.000303,                !- Design Flow Rate {m3/s}",
		"1,                       !- Number of Bore Holes",
		"160,                     !- Bore Hole Length {m}",
		"0.05715,                 !- Bore Hole Radius {m}",
		"2.493,                   !- Ground Thermal Conductivity {W/m-K}",
		"2495700,                 !- Ground Thermal Heat Capacity {J/m3-K}",
		"8,                       !- Ground Temperature {C}",
		"0.744,                   !- Grout Thermal Conductivity {W/m-K}",
		"0.389,                   !- Pipe Thermal Conductivity {W/m-K}",
		"0.0267,                  !- Pipe Out Diameter {m}",
		"0.0254,                  !- U-Tube Distance {m}",
		"0.00243,                 !- Pipe Thickness {m}",
		"5,                       !- Maximum Length of Simulation {years}",
		"0.0005,                  !- G-Function Reference Ratio {dimensionless}",
		"16,                      !- Number of Data Pairs of the G Function",
		"-4.5,                    !- G-Function Ln(T/Ts) Value 1",
		"4.7,                     !- G-Function G Value 1",
		"-4,                      !- G-Function Ln(T/Ts) Value 2",
		"4.85,                    !- G-Function G Value 2",
		"-3.5,                    !- G-Function Ln(T/Ts) Value 3",
		"5.1,                     !- G-Function G Value 3",
		"-3,                      !- G-Function Ln(T/Ts) Value 4",
		"5.3,                     !- G-Function G Value 4",
		"-2.5,                    !- G-Function Ln(T/Ts) Value 5",
		"5.56,                    !- G-Function G Value 5",
		"-2,                      !- G-Function Ln(T/Ts) Value 6",
		"5.76,                    !- G-Function G Value 6",
		"-1.5,                    !- G-Function Ln(T/Ts) Value 7",
		"5.97,                    !- G-Function G Value 7",
		"-1,                      !- G-Function Ln(T/Ts) Value 8",
		"6.19,                    !- G-Function G Value 8",
		"-.5,                     !- G-Function Ln(T/Ts) Value 9",
		"6.31,                    !- G-Function G Value 9",
		"0,                       !- G-Function Ln(T/Ts) Value 10",
		"6.42,                    !- G-Function G Value 10",
		"0.5,                     !- G-Function Ln(T/Ts) Value 11",
		"6.56,                    !- G-Function G Value 11",
		"1,                       !- G-Function Ln(T/Ts) Value 12",
		"6.61,                    !- G-Function G Value 12",
		"1.5,                     !- G-Function Ln(T/Ts) Value 13",
		"6.66,                    !- G-Function G Value 13",
		"2,                       !- G-Function Ln(T/Ts) Value 14",
		"6.7,                     !- G-Function G Value 14",
		"2.5,                     !- G-Function Ln(T/Ts) Value 15",
		"6.72,                    !- G-Function G Value 15",
		"3,                       !- G-Function Ln(T/Ts) Value 16",
		"6.73,                    !- G-Function G Value 16",
		"3.5;                     !- G-Function Ln(T/Ts) Value 17 ---EXTRA UNBALANCED FIELD---",
	});

	EXPECT_FALSE( process_idf( idf_objects, false ) );

	EXPECT_ANY_THROW( GetGroundHeatExchangerInput() );

}
