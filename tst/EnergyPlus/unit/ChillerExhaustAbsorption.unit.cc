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
#include <ChillerExhaustAbsorption.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace EnergyPlus::ChillerExhaustAbsorption;

TEST_F( EnergyPlusFixture, ExhAbsorption_GetInput_Test )
{
	std::string const idf_objects = delimited_string( {
		"  ChillerHeater:Absorption:DoubleEffect,                                                                     ",
		"    Exh Chiller,             !- Name                                                                         ",
		"    100000,                  !- Nominal Cooling Capacity {W}                                                 ",
		"    0.8,                     !- Heating to Cooling Capacity Ratio                                            ",
		"    0.97,                    !- Thermal Energy Input to Cooling Output Ratio                                 ",
		"    1.25,                    !- Thermal Energy Input to Heating Output Ratio                                 ",
		"    0.01,                    !- Electric Input to Cooling Output Ratio                                       ",
		"    0.005,                   !- Electric Input to Heating Output Ratio                                       ",
		"    Exh Chiller Inlet Node,  !- Chilled Water Inlet Node Name                                                ",
		"    Exh Chiller Outlet Node, !- Chilled Water Outlet Node Name                                               ",
		"    Exh Chiller Condenser Inlet Node,  !- Condenser Inlet Node Name                                          ",
		"    Exh Chiller Condenser Outlet Node,  !- Condenser Outlet Node Name                                        ",
		"    Exh Chiller Heating Inlet Node,  !- Hot Water Inlet Node Name                                            ",
		"    Exh Chiller Heating Outlet Node,  !- Hot Water Outlet Node Name                                          ",
		"    0.000001,                !- Minimum Part Load Ratio                                                      ",
		"    1.0,                     !- Maximum Part Load Ratio                                                      ",
		"    0.6,                     !- Optimum Part Load Ratio                                                      ",
		"    29,                      !- Design Entering Condenser Water Temperature {C}                              ",
		"    7,                       !- Design Leaving Chilled Water Temperature {C}                                 ",
		"    0.0011,                  !- Design Chilled Water Flow Rate {m3/s}                                        ",
		"    0.0011,                  !- Design Condenser Water Flow Rate {m3/s}                                      ",
		"    0.0043,                  !- Design Hot Water Flow Rate {m3/s}                                            ",
		"    ExhAbsorb_CapFt,         !- Cooling Capacity Function of Temperature Curve Name                          ",
		"    ExhAbsorb_EIRFt,         !- Fuel Input to Cooling Output Ratio Function of Temperature Curve Name        ",
		"    ExhAbsorb_PLR,           !- Fuel Input to Cooling Output Ratio Function of Part Load Ratio Curve Name    ",
		"    ExhAbsFlatBiQuad,        !- Electric Input to Cooling Output Ratio Function of Temperature Curve Name    ",
		"    ExhAbsFlatQuad,          !- Electric Input to Cooling Output Ratio Function of Part Load Ratio Curve Name",
		"    ExhAbsInvLinearQuad,     !- Heating Capacity Function of Cooling Capacity Curve Name                     ",
		"    ExhAbsLinearQuad,        !- Fuel Input to Heat Output Ratio During Heating Only Operation Curve Name     ",
		"    EnteringCondenser,       !- Temperature Curve Input Variable                                             ",
		"    AirCooled,             !- Condenser Type                                                                 ",
		"    2,                       !- Chilled Water Temperature Lower Limit {C}                                    ",
		"    Generator:MicroTurbine,  !- Exhaust Source Object Type                                                   ",
		"    Capstone C65,            !- Exhaust Source Object Name                                                   ",
		"    ;                        !- Sizing Factor                                                                ",
		"                                                                                                             ",
		"OutdoorAir:Node,                                                                 ",
		"  Exh Chiller Condenser Inlet Node,                                  !- Name     ",
		"  -1;                                                      !- Height Above Ground",
		"                                                                                 ",
		"  CURVE:BIQUADRATIC,                                                                                         ",
		"    ExhAbsorb_CapFt,         !- Name                                                                         ",
		"    -0.115131E+01,           !- Coefficient1 Constant                                                        ",
		"    -0.801316E-01,           !- Coefficient2 x                                                               ",
		"    -0.945353E-02,           !- Coefficient3 x**2                                                            ",
		"    0.209867E+00,            !- Coefficient4 y                                                               ",
		"    -0.567055E-02,           !- Coefficient5 y**2                                                            ",
		"    0.943605E-02,            !- Coefficient6 x*y                                                             ",
		"    4.44444,                 !- Minimum Value of x                                                           ",
		"    8.88889,                 !- Maximum Value of x                                                           ",
		"    21.11111,                !- Minimum Value of y                                                           ",
		"    35.00000;                !- Maximum Value of y                                                           ",
		"                                                                                                             ",
		"  CURVE:BIQUADRATIC,                                                                                         ",
		"    ExhAbsorb_EIRFt,         !- Name                                                                         ",
		"    0.131195E+01,            !- Coefficient1 Constant                                                        ",
		"    -0.159283E-01,           !- Coefficient2 x                                                               ",
		"    0.773725E-03,            !- Coefficient3 x**2                                                            ",
		"    -0.196279E-01,           !- Coefficient4 y                                                               ",
		"    0.378351E-03,            !- Coefficient5 y**2                                                            ",
		"    0.558356E-04,            !- Coefficient6 x*y                                                             ",
		"    4.44444,                 !- Minimum Value of x                                                           ",
		"    8.88889,                 !- Maximum Value of x                                                           ",
		"    21.11111,                !- Minimum Value of y                                                           ",
		"    35.00000;                !- Maximum Value of y                                                           ",
		"                                                                                                             ",
		"  Curve:Biquadratic,                                                                                         ",
		"    ExhAbsFlatBiQuad,        !- Name                                                                         ",
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
		"    ExhAbsLinearQuad,        !- Name                                                                         ",
		"    0.000000000,             !- Coefficient1 Constant                                                        ",
		"    1.000000000,             !- Coefficient2 x                                                               ",
		"    0.000000000,             !- Coefficient3 x**2                                                            ",
		"    0.,                      !- Minimum Value of x                                                           ",
		"    50.;                     !- Maximum Value of x                                                           ",
		"                                                                                                             ",
		"  Curve:Quadratic,                                                                                           ",
		"    ExhAbsInvLinearQuad,     !- Name                                                                         ",
		"    1.000000000,             !- Coefficient1 Constant                                                        ",
		"    -1.000000000,            !- Coefficient2 x                                                               ",
		"    0.000000000,             !- Coefficient3 x**2                                                            ",
		"    0.,                      !- Minimum Value of x                                                           ",
		"    50.;                     !- Maximum Value of x                                                           ",
		"                                                                                                             ",
		"  Curve:QUADRATIC,                                                                                           ",
		"    ExhAbsorb_PLR,           !- Name                                                                         ",
		"    0.026280035,             !- Coefficient1 Constant                                                        ",
		"    0.678066088,             !- Coefficient2 x                                                               ",
		"    0.273905867,             !- Coefficient3 x**2                                                            ",
		"    0.0,                     !- Minimum Value of x                                                           ",
		"    1.0;                     !- Maximum Value of x                                                           ",
		"                                                                                                             ",
		"  Curve:Quadratic,                                                                                           ",
		"    ExhAbsFlatQuad,          !- Name                                                                         ",
		"    1.000000000,             !- Coefficient1 Constant                                                        ",
		"    0.000000000,             !- Coefficient2 x                                                               ",
		"    0.000000000,             !- Coefficient3 x**2                                                            ",
		"    0.,                      !- Minimum Value of x                                                           ",
		"    50.;                     !- Maximum Value of x                                                           ",
		"                                                                                                              ",
		"  Generator:MicroTurbine,                                                                                     ",
		"    Capstone C65,            !- Name                                                                          ",
		"    65000,                   !- Reference Electrical Power Output {W}                                         ",
		"    29900,                   !- Minimum Full Load Electrical Power Output {W}                                 ",
		"    65000,                   !- Maximum Full Load Electrical Power Output {W}                                 ",
		"    0.29,                    !- Reference Electrical Efficiency Using Lower Heating Value                     ",
		"    15.0,                    !- Reference Combustion Air Inlet Temperature {C}                                ",
		"    0.00638,                 !- Reference Combustion Air Inlet Humidity Ratio {kgWater/kgDryAir}              ",
		"    0.0,                     !- Reference Elevation {m}                                                       ",
		"    Capstone C65 Power_vs_Temp_Elev,  !- Electrical Power Function of Temperature and Elevation Curve Name    ",
		"    Capstone C65 Efficiency_vs_Temp,  !- Electrical Efficiency Function of Temperature Curve Name             ",
		"    Capstone C65 Efficiency_vs_PLR,  !- Electrical Efficiency Function of Part Load Ratio Curve Name          ",
		"    NaturalGas,              !- Fuel Type                                                                     ",
		"    50000,                   !- Fuel Higher Heating Value {kJ/kg}                                             ",
		"    45450,                   !- Fuel Lower Heating Value {kJ/kg}                                              ",
		"    300,                     !- Standby Power {W}                                                             ",
		"    4500,                    !- Ancillary Power {W}                                                           ",
		"    ,                        !- Ancillary Power Function of Fuel Input Curve Name                             ",
		"    ,                        !- Heat Recovery Water Inlet Node Name                                           ",
		"    ,                        !- Heat Recovery Water Outlet Node Name                                          ",
		"    ,                        !- Reference Thermal Efficiency Using Lower Heat Value                           ",
		"    ,                        !- Reference Inlet Water Temperature {C}                                         ",
		"    ,                        !- Heat Recovery Water Flow Operating Mode                                       ",
		"    ,                        !- Reference Heat Recovery Water Flow Rate {m3/s}                                ",
		"    ,                        !- Heat Recovery Water Flow Rate Function of Temperature and Power Curve Name    ",
		"    ,                        !- Thermal Efficiency Function of Temperature and Elevation Curve Name           ",
		"    ,                        !- Heat Recovery Rate Function of Part Load Ratio Curve Name                     ",
		"    ,                        !- Heat Recovery Rate Function of Inlet Water Temperature Curve Name             ",
		"    ,                        !- Heat Recovery Rate Function of Water Flow Rate Curve Name                     ",
		"    ,                        !- Minimum Heat Recovery Water Flow Rate {m3/s}                                  ",
		"    ,                        !- Maximum Heat Recovery Water Flow Rate {m3/s}                                  ",
		"    ,                        !- Maximum Heat Recovery Water Temperature {C}                                   ",
		"    Capstone C65 Combustion Air Inlet Node,  !- Combustion Air Inlet Node Name                                ",
		"    Capstone C65 Combustion Air Outlet Node,  !- Combustion Air Outlet Node Name                              ",
		"    0.6,                     !- Reference Exhaust Air Mass Flow Rate {kg/s}                                   ",
		"    Capstone C65 ExhFlowRate_vs_Inlet_Temp,  !- Exhaust Air Flow Rate Function of Temperature Curve Name      ",
		"    Capstone C65 ExhFlowRate_vs_PLR,  !- Exhaust Air Flow Rate Function of Part Load Ratio Curve Name         ",
		"    350,                     !- Nominal Exhaust Air Outlet Temperature                                        ",
		"    Capstone C65 ExhTemp_vs_Inlet_Temp,  !- Exhaust Air Temperature Function of Temperature Curve Name        ",
		"    Capstone C65 ExhTemp_vs_PLR;  !- Exhaust Air Temperature Function of Part Load Ratio Curve Name           ",
		"                                                                                                              ",
		"  OutdoorAir:Node,                                                                ",
		"    Capstone C65 Combustion Air Inlet Node,  !- Name                              ",
		"    -1;                      !- Height Above Ground {m}                           ",
		"                                                                                  ",
		"  Curve:Quadratic,                                                                                            ",
		"    Capstone C65 ExhTemp_vs_Inlet_Temp,  !- Name                                                              ",
		"    1.0,                     !- Coefficient1 Constant                                                         ",
		"    0.0,                     !- Coefficient2 x                                                                ",
		"    0.0,                     !- Coefficient3 x**2                                                             ",
		"    -20.,                    !- Minimum Value of x                                                            ",
		"    50.;                     !- Maximum Value of x                                                            ",
		"                                                                                                              ",
		"  Curve:Quadratic,                                                                                            ",
		"    Capstone C65 ExhTemp_vs_PLR,  !- Name                                                                     ",
		"    1.0,                     !- Coefficient1 Constant                                                         ",
		"    0.0,                     !- Coefficient2 x                                                                ",
		"    0.0,                     !- Coefficient3 x**2                                                             ",
		"    0.03,                    !- Minimum Value of x                                                            ",
		"    1.;                      !- Maximum Value of x                                                            ",
		"                                                                                                              ",
		"  Curve:Quadratic,                                                                                            ",
		"    Capstone C65 ExhFlowRate_vs_PLR,  !- Name                                                                 ",
		"    1.0,                     !- Coefficient1 Constant                                                         ",
		"    0.0,                     !- Coefficient2 x                                                                ",
		"    0.0,                     !- Coefficient3 x**2                                                             ",
		"    0.03,                    !- Minimum Value of x                                                            ",
		"    1.;                      !- Maximum Value of x                                                            ",
		"                                                                                                              ",
		"  Curve:Cubic,                                                                                                ",
		"    Capstone C65 ExhFlowRate_vs_Inlet_Temp,  !- Name                                                          ",
		"    1.0,                     !- Coefficient1 Constant                                                         ",
		"    0.0,                     !- Coefficient2 x                                                                ",
		"    0.0,                     !- Coefficient3 x**2                                                             ",
		"    0.0,                     !- Coefficient4 x**3                                                             ",
		"    -20.,                    !- Minimum Value of x                                                            ",
		"    50.;                     !- Maximum Value of x                                                            ",
		"                                                                                                              ",
		"  Curve:Cubic,                                                                                                ",
		"    Capstone C65 Efficiency_vs_Temp,  !- Name                                                                 ",
		"    1.0402217,               !- Coefficient1 Constant                                                         ",
		"    -0.0017314,              !- Coefficient2 x                                                                ",
		"    -6.497040E-05,           !- Coefficient3 x**2                                                             ",
		"    5.133175E-07,            !- Coefficient4 x**3                                                             ",
		"    -20.0,                   !- Minimum Value of x                                                            ",
		"    50.0,                    !- Maximum Value of x                                                            ",
		"    ,                        !- Minimum Curve Output                                                          ",
		"    ,                        !- Maximum Curve Output                                                          ",
		"    Temperature,             !- Input Unit Type for X                                                         ",
		"    Dimensionless;           !- Output Unit Type                                                              ",
		"                                                                                                              ",
		"  Curve:Cubic,                                                                                                ",
		"    Capstone C65 Efficiency_vs_PLR,  !- Name                                                                  ",
		"    0.215290,                !- Coefficient1 Constant                                                         ",
		"    2.561463,                !- Coefficient2 x                                                                ",
		"    -3.24613,                !- Coefficient3 x**2                                                             ",
		"    1.497306,                !- Coefficient4 x**3                                                             ",
		"    0.03,                    !- Minimum Value of x                                                            ",
		"    1.0;                     !- Maximum Value of x                                                            ",
		"                                                                                                              ",
		"  Curve:Biquadratic,                                                                                          ",
		"    Capstone C65 Power_vs_Temp_Elev,  !- Name                                                                 ",
		"    1.2027697,               !- Coefficient1 Constant                                                         ",
		"    -9.671305E-03,           !- Coefficient2 x                                                                ",
		"    -4.860793E-06,           !- Coefficient3 x**2                                                             ",
		"    -1.542394E-04,           !- Coefficient4 y                                                                ",
		"    9.111418E-09,            !- Coefficient5 y**2                                                             ",
		"    8.797885E-07,            !- Coefficient6 x*y                                                              ",
		"    -17.8,                   !- Minimum Value of x                                                            ",
		"    50.0,                    !- Maximum Value of x                                                            ",
		"    0.0,                     !- Minimum Value of y                                                            ",
		"    3050.,                   !- Maximum Value of y                                                            ",
		"    ,                        !- Minimum Curve Output                                                          ",
		"    ,                        !- Maximum Curve Output                                                          ",
		"    Temperature,             !- Input Unit Type for X                                                         ",
		"    Distance,                !- Input Unit Type for Y                                                         ",
		"    Dimensionless;           !- Output Unit Type                                                              ",

	} );

	ASSERT_TRUE( process_idf( idf_objects ) );
	compare_err_stream( "" );

	GetExhaustAbsorberInput();

	compare_err_stream( "" );

	EXPECT_EQ( 1, NumExhaustAbsorbers );
	EXPECT_EQ( "EXH CHILLER", ExhaustAbsorber( 1 ).Name );

	EXPECT_EQ( 100000., ExhaustAbsorber( 1 ).NomCoolingCap );
	EXPECT_EQ( 0.8, ExhaustAbsorber( 1 ).NomHeatCoolRatio );
	EXPECT_EQ( 0.97, ExhaustAbsorber( 1 ).ThermalEnergyCoolRatio );
	EXPECT_EQ( 1.25, ExhaustAbsorber( 1 ).ThermalEnergyHeatRatio );
	EXPECT_EQ( 0.01, ExhaustAbsorber( 1 ).ElecCoolRatio );
	EXPECT_EQ( 0.005, ExhaustAbsorber( 1 ).ElecHeatRatio );

	EXPECT_EQ( 0.000001, ExhaustAbsorber( 1 ).MinPartLoadRat );
	EXPECT_EQ( 1.0, ExhaustAbsorber( 1 ).MaxPartLoadRat );
	EXPECT_EQ( 0.6, ExhaustAbsorber( 1 ).OptPartLoadRat );

	EXPECT_EQ( 29., ExhaustAbsorber( 1 ).TempDesCondReturn );
	EXPECT_EQ( 7., ExhaustAbsorber( 1 ).TempDesCHWSupply );
	EXPECT_EQ( 0.0011, ExhaustAbsorber( 1 ).EvapVolFlowRate );
	EXPECT_EQ( 0.0043, ExhaustAbsorber( 1 ).HeatVolFlowRate );

	EXPECT_TRUE( ExhaustAbsorber( 1 ).isEnterCondensTemp );
	EXPECT_FALSE( ExhaustAbsorber( 1 ).isWaterCooled );
	EXPECT_EQ( 2., ExhaustAbsorber( 1 ).CHWLowLimitTemp );

}



