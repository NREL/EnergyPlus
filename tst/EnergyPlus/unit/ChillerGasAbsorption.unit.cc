// EnergyPlus, Copyright (c) 1996-2018, The Board of Trustees of the University of Illinois,
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

// Google Test Headers
#include <gtest/gtest.h>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <DataGlobals.hh>
#include <ChillerGasAbsorption.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace EnergyPlus::ChillerGasAbsorption;

TEST_F( EnergyPlusFixture, GasAbsorption_GetInput_Test )
{
	std::string const idf_objects = delimited_string( {
		"  ChillerHeater:Absorption:DirectFired,                                                                      ",
		"    Big Chiller,             !- Name                                                                         ",
		"    100000,                  !- Nominal Cooling Capacity {W}                                                 ",
		"    0.8,                     !- Heating to Cooling Capacity Ratio                                            ",
		"    0.97,                    !- Fuel Input to Cooling Output Ratio                                           ",
		"    1.25,                    !- Fuel Input to Heating Output Ratio                                           ",
		"    0.01,                    !- Electric Input to Cooling Output Ratio                                       ",
		"    0.005,                   !- Electric Input to Heating Output Ratio                                       ",
		"    Big Chiller Inlet Node,  !- Chilled Water Inlet Node Name                                                ",
		"    Big Chiller Outlet Node, !- Chilled Water Outlet Node Name                                               ",
		"    Big Chiller Condenser Inlet Node,  !- Condenser Inlet Node Name                                          ",
		"    Big Chiller Condenser Outlet Node,  !- Condenser Outlet Node Name                                        ",
		"    Purchased Heat Inlet Node,  !- Hot Water Inlet Node Name                                                 ",
		"    Purchased Heat Outlet Node,  !- Hot Water Outlet Node Name                                               ",
		"    0.000001,                !- Minimum Part Load Ratio                                                      ",
		"    1.0,                     !- Maximum Part Load Ratio                                                      ",
		"    0.6,                     !- Optimum Part Load Ratio                                                      ",
		"    29,                      !- Design Entering Condenser Water Temperature {C}                              ",
		"    7,                       !- Design Leaving Chilled Water Temperature {C}                                 ",
		"    0.0011,                  !- Design Chilled Water Flow Rate {m3/s}                                        ",
		"    0.0011,                  !- Design Condenser Water Flow Rate {m3/s}                                      ",
		"    0.0043,                  !- Design Hot Water Flow Rate {m3/s}                                            ",
		"    GasAbsFlatBiQuad,        !- Cooling Capacity Function of Temperature Curve Name                          ",
		"    GasAbsFlatBiQuad,        !- Fuel Input to Cooling Output Ratio Function of Temperature Curve Name        ",
		"    GasAbsLinearQuad,        !- Fuel Input to Cooling Output Ratio Function of Part Load Ratio Curve Name    ",
		"    GasAbsFlatBiQuad,        !- Electric Input to Cooling Output Ratio Function of Temperature Curve Name    ",
		"    GasAbsFlatQuad,          !- Electric Input to Cooling Output Ratio Function of Part Load Ratio Curve Name",
		"    GasAbsInvLinearQuad,     !- Heating Capacity Function of Cooling Capacity Curve Name                     ",
		"    GasAbsLinearQuad,        !- Fuel Input to Heat Output Ratio During Heating Only Operation Curve Name     ",
		"    EnteringCondenser,       !- Temperature Curve Input Variable                                             ",
		"    AirCooled,               !- Condenser Type                                                               ",
		"    2,                       !- Chilled Water Temperature Lower Limit {C}                                    ",
		"    0,                       !- Fuel Higher Heating Value {kJ/kg}                                            ",
		"    NaturalGas,              !- Fuel Type                                                                    ",
		"    ;                        !- Sizing Factor                                                                ",
		"                                                                                                             ",
		"  Curve:Biquadratic,                                                                                         ",
		"    GasAbsFlatBiQuad,        !- Name                                                                         ",
		"    1.000000000,             !- Coefficient1 Constant                                                        ",
		"    0.000000000,             !- Coefficient2 x                                                               ",
		"    0.000000000,             !- Coefficient3 x**2                                                            ",
		"    0.000000000,             !- Coefficient4 y                                                               ",
		"    0.000000000,             !- Coefficient5 y**2                                                            ",
		"    0.000000000,             !- Coefficient6 x*y                                                             ",
		"    0.,                      !- Minimum Value of x                                                           ",
		"    50.,                     !- Maximum Value of x                                                           ",
		"    0.,                      !- Minimum Value of y                                                           ",
		"    50.;                     !- Maximum Value of y                                                           ",
		"                                                                                                             ",
		"  Curve:Quadratic,                                                                                           ",
		"    GasAbsFlatQuad,          !- Name                                                                         ",
		"    1.000000000,             !- Coefficient1 Constant                                                        ",
		"    0.000000000,             !- Coefficient2 x                                                               ",
		"    0.000000000,             !- Coefficient3 x**2                                                            ",
		"    0.,                      !- Minimum Value of x                                                           ",
		"    50.;                     !- Maximum Value of x                                                           ",
		"                                                                                                             ",
		"  Curve:Quadratic,                                                                                           ",
		"    GasAbsLinearQuad,        !- Name                                                                         ",
		"    0.000000000,             !- Coefficient1 Constant                                                        ",
		"    1.000000000,             !- Coefficient2 x                                                               ",
		"    0.000000000,             !- Coefficient3 x**2                                                            ",
		"    0.,                      !- Minimum Value of x                                                           ",
		"    50.;                     !- Maximum Value of x                                                           ",
		"                                                                                                             ",
		"  Curve:Quadratic,                                                                                           ",
		"    GasAbsInvLinearQuad,     !- Name                                                                         ",
		"    1.000000000,             !- Coefficient1 Constant                                                        ",
		"    -1.000000000,            !- Coefficient2 x                                                               ",
		"    0.000000000,             !- Coefficient3 x**2                                                            ",
		"    0.,                      !- Minimum Value of x                                                           ",
		"    50.;                     !- Maximum Value of x                                                           ",

	} );

	ASSERT_FALSE( process_idf( idf_objects ) );

	GetGasAbsorberInput();

	EXPECT_EQ( 1, NumGasAbsorbers );
	EXPECT_EQ( "BIG CHILLER", GasAbsorber( 1 ).Name );
	EXPECT_EQ( 100000., GasAbsorber( 1 ).NomCoolingCap );
	EXPECT_EQ( 0.8, GasAbsorber( 1 ).NomHeatCoolRatio );

	EXPECT_EQ( 0.97, GasAbsorber( 1 ).FuelCoolRatio );
	EXPECT_EQ( 1.25, GasAbsorber( 1 ).FuelHeatRatio );
	EXPECT_EQ( 0.01, GasAbsorber( 1 ).ElecCoolRatio );
	EXPECT_EQ( 0.005, GasAbsorber( 1 ).ElecHeatRatio );

	EXPECT_EQ( 0.000001, GasAbsorber( 1 ).MinPartLoadRat );
	EXPECT_EQ( 1.0, GasAbsorber( 1 ).MaxPartLoadRat );
	EXPECT_EQ( 0.6, GasAbsorber( 1 ).OptPartLoadRat );

	EXPECT_EQ( 29., GasAbsorber( 1 ).TempDesCondReturn );
	EXPECT_EQ( 7., GasAbsorber( 1 ).TempDesCHWSupply );
	EXPECT_EQ( 0.0011, GasAbsorber( 1 ).EvapVolFlowRate );
	EXPECT_EQ( 0.0043, GasAbsorber( 1 ).HeatVolFlowRate );

	EXPECT_TRUE( GasAbsorber( 1 ).isEnterCondensTemp );
	EXPECT_FALSE( GasAbsorber( 1 ).isWaterCooled );

	EXPECT_EQ( 2., GasAbsorber( 1 ).CHWLowLimitTemp );
	EXPECT_EQ( "Gas", GasAbsorber( 1 ).FuelType );

}





