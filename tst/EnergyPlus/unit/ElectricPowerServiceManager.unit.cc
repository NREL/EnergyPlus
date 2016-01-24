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

// EnergyPlus::ElectricPowerServiceManager Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// C++ Headers
#include <vector>
#include <memory>

// EnergyPlus Headers
#include <EnergyPlus/ExteriorEnergyUse.hh>
#include <EnergyPlus/ElectricPowerServiceManager.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>


#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace EnergyPlus::CurveManager;
using namespace ObjexxFCL;
using namespace DataGlobals;

TEST_F( EnergyPlusFixture, ManageElectricPowerTest_BatteryDischargeTest )
{

	std::string const idf_objects = delimited_string( { 
	"Version,8.4;",
  "ElectricLoadCenter:Distribution,",
"    PV Array Load Center,    !- Name",
"    Generator List,          !- Generator List Name",
"    TrackElectrical,         !- Generator Operation Scheme Type",
"    0,                       !- Demand Limit Scheme Purchased Electric Demand Limit {W}",
"    ,                        !- Track Schedule Name Scheme Schedule Name",
"    ,                        !- Track Meter Scheme Meter Name",
"    DirectCurrentWithInverterDCStorage,  !- Electrical Buss Type",
"    PV Inverter,             !- Inverter Object Name",
"    Kibam;                   !- Electrical Storage Object Name",

"  Curve:DoubleExponentialDecay,",
"    Doubleexponential,       !- Name",
"    1380,                    !- Coefficient1 C1",
"    6834,                    !- Coefficient2 C2",
"    -8.75,                   !- Coefficient3 C3",
"    6747,                    !- Coefficient3 C4",
"    -6.22,                   !- Coefficient3 C5",
"    0,                       !- Minimum Value of x",
"    1,                       !- Maximum Value of x",
"    ,                        !- Minimum Curve Output",
"    ,                        !- Maximum Curve Output",
"    Dimensionless,           !- Input Unit Type for x",
"    Dimensionless;           !- Output Unit Type",

"  ElectricLoadCenter:Storage:Battery,",
"    Kibam,                   !- Name",
"    ALWAYS_ON,               !- Availability Schedule Name",
"    ,                        !- Zone Name",
"    0,                       !- Radiative Fraction",
"    10,                      !- Number of Battery Modules in Parallel",
"    10,                      !- Number of Battery Modules in Series",
"    86.1,                    !- Maximum Module Capacity {Ah}",
"    0.7,                     !- Initial Fractional State of Charge",
"    0.37,                    !- Fraction of Available Charge Capacity",
"    0.5874,                  !- Change Rate from Bound Charge to Available Charge {1/hr}",
"    12.6,                    !- Fully Charged Module Open Circuit Voltage {V}",
"    12.4,                    !- Fully Discharged Module Open Circuit Voltage {V}",
"    charging,                !- Voltage Change Curve Name for Charging",
"    discharging,             !- Voltage Change Curve Name for Discharging",
"    0.054,                   !- Module Internal Electrical Resistance {ohms}",
"    100,                     !- Maximum Module Discharging Current {A}",
"    10,                      !- Module Cut-off Voltage {V}",
"    1,                       !- Module Charge Rate Limit",
"    Yes,                     !- Battery Life Calculation",
"    5,                       !- Number of Cycle Bins",
"    Doubleexponential;       !- Battery Life Curve Name",

"  Curve:RectangularHyperbola2,",
"    charging,                !- Name",
"    -.2765,                  !- Coefficient1 C1",
"    -93.27,                  !- Coefficient2 C2",
"    0.0068,                  !- Coefficient3 C3",
"    0,                       !- Minimum Value of x",
"    1,                       !- Maximum Value of x",
"    -100,                    !- Minimum Curve Output",
"    100,                     !- Maximum Curve Output",
"    Dimensionless,           !- Input Unit Type for x",
"    Dimensionless;           !- Output Unit Type",

"  Curve:RectangularHyperbola2,",
"    discharging,             !- Name",
"    0.0899,                  !- Coefficient1 C1",
"    -98.24,                  !- Coefficient2 C2",
"    -.0082,                  !- Coefficient3 C3",
"    0,                       !- Minimum Value of x",
"    1,                       !- Maximum Value of x",
"    -100,                    !- Minimum Curve Output",
"    100,                     !- Maximum Curve Output",
"    Dimensionless,           !- Input Unit Type for x",
"    Dimensionless;           !- Output Unit Type",

"  ElectricLoadCenter:Inverter:LookUpTable,",
"    PV Inverter,             !- Name",
"    ALWAYS_ON,               !- Availability Schedule Name",
"    ,                        !- Zone Name",
"    0.25,                    !- Radiative Fraction",
"    14000,                   !- Rated Maximum Continuous Output Power {W}",
"    200.0,                   !- Night Tare Loss Power {W}",
"    368,                     !- Nominal Voltage Input {V}",
"    0.839,                   !- Efficiency at 10% Power and Nominal Voltage",
"    0.897,                   !- Efficiency at 20% Power and Nominal Voltage",
"    0.916,                   !- Efficiency at 30% Power and Nominal Voltage",
"    0.931,                   !- Efficiency at 50% Power and Nominal Voltage",
"    0.934,                   !- Efficiency at 75% Power and Nominal Voltage",
"    0.930;                   !- Efficiency at 100% Power and Nominal Voltage",

"  ElectricLoadCenter:Generators,",
"    Generator List,          !- Name",
"    PV:ZN_1_FLR_1_SEC_1_Ceiling,  !- Generator 1 Name",
"    Generator:Photovoltaic,  !- Generator 1 Object Type",
"    9000.0,                  !- Generator 1 Rated Electric Power Output {W}",
"    ALWAYS_ON,               !- Generator 1 Availability Schedule Name",
"    ;                        !- Generator 1 Rated Thermal to Electrical Power Ratio",

"  Generator:Photovoltaic,",
"    PV:ZN_1_FLR_1_SEC_1_Ceiling,  !- Name",
"    ZN_1_FLR_1_SEC_1_Ceiling,!- Surface Name",
"    PhotovoltaicPerformance:Simple,  !- Photovoltaic Performance Object Type",
"    20percentEffPVhalfArea,  !- Module Performance Name",
"    Decoupled,               !- Heat Transfer Integration Mode",
"    1.0,                     !- Number of Series Strings in Parallel {dimensionless}",
"    1.0;                     !- Number of Modules in Series {dimensionless}",

"  PhotovoltaicPerformance:Simple,",
"    20percentEffPVhalfArea,  !- Name",
"    0.5,                     !- Fraction of Surface Area with Active Solar Cells {dimensionless}",
"    Fixed,                   !- Conversion Efficiency Input Mode",
"    0.20,                    !- Value for Cell Efficiency if Fixed",
"    ;                        !- Efficiency Schedule Name",

"  Schedule:Compact,",
"    ALWAYS_ON,               !- Name",
"    On/Off,                  !- Schedule Type Limits Name",
"    Through: 12/31,          !- Field 1",
"    For: AllDays,            !- Field 2",
"    Until: 24:00,1;          !- Field 3",
	} );

	ASSERT_FALSE( process_idf( idf_objects ) );

	ElectricPowerService::createFacilityElectricPowerServiceObject();
	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs.emplace_back( std::make_unique < ElectricPowerService::ElectPowerLoadCenter > ( 1 ) );

	int CurveNum1 = 1;
	Real64 k = 0.5874;
	Real64 c = 0.37;
	Real64 qmax = 86.1;
	Real64 E0c = 12.6;
	Real64 InternalR = 0.054;

	Real64 I0 = 0.159;
	Real64 T0 = 537.9;
	Real64 Volt = 12.59;
	Real64 Pw = 2.0;
	Real64 q0 = 60.2;

	EXPECT_TRUE( ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->storageObj->determineCurrentForBatteryDischarge( I0, T0, Volt, Pw, q0, CurveNum1, k, c, qmax, E0c, InternalR ) );

	I0 = -222.7;
	T0 = -0.145;
	Volt = 24.54;
	Pw = 48000;
	q0 = 0;

	EXPECT_FALSE( ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->storageObj->determineCurrentForBatteryDischarge( I0, T0, Volt, Pw, q0, CurveNum1, k, c, qmax, E0c, InternalR ) );

}

TEST_F( EnergyPlusFixture, ManageElectricPowerTest_UpdateLoadCenterRecords_Case1 )
{

	std::string const idf_objects = delimited_string( { 
	"Version,8.4;",

"  ElectricLoadCenter:Distribution,",
"    Test Load Center,    !- Name",
"    Test Generator List,          !- Generator List Name",
"    TrackElectrical,         !- Generator Operation Scheme Type",
"    0,                       !- Demand Limit Scheme Purchased Electric Demand Limit {W}",
"    ,                        !- Track Schedule Name Scheme Schedule Name",
"    ,                        !- Track Meter Scheme Meter Name",
"    AlternatingCurrent,                  !- Electrical Buss Type",
"    ,                        !- Inverter Object Name",
"    ;                        !- Electrical Storage Object Name",

"  ElectricLoadCenter:Generators,",
"    Test Generator List,          !- Name",
"    Test Gen 1,  !- Generator 1 Name",
"    Generator:InternalCombustionEngine,  !- Generator 1 Object Type",
"    1000.0,                  !- Generator 1 Rated Electric Power Output {W}",
"    ALWAYS_ON,               !- Generator 1 Availability Schedule Name",
"    ,                        !- Generator 1 Rated Thermal to Electrical Power Ratio",
"    Test Gen 2,  !- Generator 2 Name",
"    Generator:WindTurbine,  !- Generator 2 Object Type",
"    2000.0,                  !- Generator 2 Rated Electric Power Output {W}",
"    ALWAYS_ON,               !- Generator 2 Availability Schedule Name",
"    ;                        !- Generator 2 Rated Thermal to Electrical Power Ratio",


"  Schedule:Compact,",
"    ALWAYS_ON,               !- Name",
"    On/Off,                  !- Schedule Type Limits Name",
"    Through: 12/31,          !- Field 1",
"    For: AllDays,            !- Field 2",
"    Until: 24:00,1;          !- Field 3",
	} );

	ASSERT_FALSE( process_idf( idf_objects ) );

	ElectricPowerService::createFacilityElectricPowerServiceObject();
	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs.emplace_back( new ElectricPowerService::ElectPowerLoadCenter ( 1 ) );


	// Case 1 ACBuss - Generators 1000+2000=3000, thermal 500+750=1250
	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->bussType = ElectricPowerService::ElectPowerLoadCenter::aCBuss;

	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->elecGenCntrlObj[ 0 ]->electProdRate = 1000.0;
//	ElecLoadCenter( LoadCenterNum ).ElecGen( 1 ).ElectProdRate = 1000.0;
	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->elecGenCntrlObj[ 1 ]->electProdRate = 2000.0;
//	ElecLoadCenter( LoadCenterNum ).ElecGen( 2 ).ElectProdRate = 2000.0;
	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->elecGenCntrlObj[ 0 ]->electricityProd = 1000.0*3600.0;
	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->elecGenCntrlObj[ 1 ]->electricityProd = 2000.0*3600.0;
	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->elecGenCntrlObj[ 0 ]->thermalProdRate = 500.0;
	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->elecGenCntrlObj[ 1 ]->thermalProdRate = 750.0;
	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->elecGenCntrlObj[ 0 ]->thermalProd     = 500.0*3600.0;
	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->elecGenCntrlObj[ 1 ]->thermalProd     = 750.0*3600.0;
	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->updateLoadCenterRecords();

	EXPECT_NEAR( ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->electProdRate , 3000.0, 0.1);
	EXPECT_NEAR( ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->electricityProd, 3000.0*3600.0, 0.1 );
	EXPECT_NEAR( ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->thermalProdRate, 1250.0, 0.1 );
	EXPECT_NEAR( ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->thermalProd, 1250.0*3600.0, 0.1 );

}

TEST_F( EnergyPlusFixture, ManageElectricPowerTest_UpdateLoadCenterRecords_Case2 )
{

	std::string const idf_objects = delimited_string( { 
	"Version,8.4;",

"  ElectricLoadCenter:Distribution,",
"    Test Load Center,        !- Name",
"    Test Generator List,     !- Generator List Name",
"    TrackElectrical,         !- Generator Operation Scheme Type",
"    0,                       !- Demand Limit Scheme Purchased Electric Demand Limit {W}",
"    ,                        !- Track Schedule Name Scheme Schedule Name",
"    ,                        !- Track Meter Scheme Meter Name",
"    AlternatingCurrentWithStorage,                  !- Electrical Buss Type",
"     ,                        !- Inverter Object Name",
"    Test Storage Bank;       !- Electrical Storage Object Name",

"  ElectricLoadCenter:Storage:Simple,",
"    Test Storage Bank,",
"    ALWAYS_ON, !- availability schedule",
"    , !- zone name"   ,
"    , !- radiative fraction",
"    1.0 , !- Nominal Energetic Efficiency for Charging",
"    1.0,  !- Nominal Discharging Energetic efficiency",
"    1.0E9, !- Maximum storage capacity",
"    5000.0, !- Maximum Power for Discharging",
"    5000.0, !- Maximum Power for Charging",
"    1.0E9; !- initial stat of charge",

"  ElectricLoadCenter:Generators,",
"    Test Generator List,     !- Name",
"    Test Gen 1,              !- Generator 1 Name",
"    Generator:InternalCombustionEngine,  !- Generator 1 Object Type",
"    1000.0,                  !- Generator 1 Rated Electric Power Output {W}",
"    ALWAYS_ON,               !- Generator 1 Availability Schedule Name",
"    ,                        !- Generator 1 Rated Thermal to Electrical Power Ratio",
"    Test Gen 2,              !- Generator 2 Name",
"    Generator:WindTurbine,   !- Generator 2 Object Type",
"    2000.0,                  !- Generator 2 Rated Electric Power Output {W}",
"    ALWAYS_ON,               !- Generator 2 Availability Schedule Name",
"    ;                        !- Generator 2 Rated Thermal to Electrical Power Ratio",

"  Schedule:Compact,",
"    ALWAYS_ON,               !- Name",
"    On/Off,                  !- Schedule Type Limits Name",
"    Through: 12/31,          !- Field 1",
"    For: AllDays,            !- Field 2",
"    Until: 24:00,1;          !- Field 3",
	} );

	ASSERT_FALSE( process_idf( idf_objects ) );

	ElectricPowerService::createFacilityElectricPowerServiceObject();
	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs.emplace_back( new ElectricPowerService::ElectPowerLoadCenter ( 1 ) );


	// Case 2 ACBussStorage - Generators 1000+2000=3000, Storage 200-150=50
	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->bussType = ElectricPowerService::ElectPowerLoadCenter::aCBussStorage;
//	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->storagePresent
	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->storageObj = std::unique_ptr < ElectricPowerService::ElectricStorage >( new ElectricPowerService::ElectricStorage (  "TEST STORAGE BANK"  ) );

	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->elecGenCntrlObj[ 0 ]->electProdRate = 1000.0;

	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->elecGenCntrlObj[ 1 ]->electProdRate = 2000.0;

	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->elecGenCntrlObj[ 0 ]->electricityProd = 1000.0*3600.0;
	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->elecGenCntrlObj[ 1 ]->electricityProd = 2000.0*3600.0;
	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->elecGenCntrlObj[ 0 ]->thermalProdRate = 500.0;
	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->elecGenCntrlObj[ 1 ]->thermalProdRate = 750.0;
	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->elecGenCntrlObj[ 0 ]->thermalProd     = 500.0*3600.0;
	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->elecGenCntrlObj[ 1 ]->thermalProd     = 750.0*3600.0;

	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->storageObj->drawnPower   = 200.0;
	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->storageObj->storedPower  = 150.0;
	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->storageObj->drawnEnergy  = 200.0*3600.0;
	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->storageObj->storedEnergy = 150.0*3600.0;

	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->updateLoadCenterRecords();

	EXPECT_NEAR( ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->electProdRate, 3050.0, 0.1 );
	EXPECT_NEAR( ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->electricityProd, 3050.0*3600.0, 0.1 );

	}


TEST_F( EnergyPlusFixture, ManageElectricPowerTest_UpdateLoadCenterRecords_Case3 )
{

	std::string const idf_objects = delimited_string( { 
	"Version,8.4;",

"  ElectricLoadCenter:Distribution,",
"    Test Load Center,        !- Name",
"    Test Generator List,     !- Generator List Name",
"    TrackElectrical,         !- Generator Operation Scheme Type",
"    0,                       !- Demand Limit Scheme Purchased Electric Demand Limit {W}",
"    ,                        !- Track Schedule Name Scheme Schedule Name",
"    ,                        !- Track Meter Scheme Meter Name",
"    DirectCurrentWithInverter,                  !- Electrical Buss Type",
"    Test Inverter ,                        !- Inverter Object Name",
"    ;       !- Electrical Storage Object Name",

"  ElectricLoadCenter:Inverter:Simple,",
"    Test Inverter,",
"    ALWAYS_ON, !- availability schedule",
"    , !- zone name"   ,
"    , !- radiative fraction",
"    1.0 ; !- Inverter efficiency",


"  ElectricLoadCenter:Generators,",
"    Test Generator List,     !- Name",
"    Test Gen 1,              !- Generator 1 Name",
"    Generator:InternalCombustionEngine,  !- Generator 1 Object Type",
"    1000.0,                  !- Generator 1 Rated Electric Power Output {W}",
"    ALWAYS_ON,               !- Generator 1 Availability Schedule Name",
"    ,                        !- Generator 1 Rated Thermal to Electrical Power Ratio",
"    Test Gen 2,              !- Generator 2 Name",
"    Generator:WindTurbine,   !- Generator 2 Object Type",
"    2000.0,                  !- Generator 2 Rated Electric Power Output {W}",
"    ALWAYS_ON,               !- Generator 2 Availability Schedule Name",
"    ;                        !- Generator 2 Rated Thermal to Electrical Power Ratio",

"  Schedule:Compact,",
"    ALWAYS_ON,               !- Name",
"    On/Off,                  !- Schedule Type Limits Name",
"    Through: 12/31,          !- Field 1",
"    For: AllDays,            !- Field 2",
"    Until: 24:00,1;          !- Field 3",
	} );

	ASSERT_FALSE( process_idf( idf_objects ) );

	ElectricPowerService::createFacilityElectricPowerServiceObject();
	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs.emplace_back( new ElectricPowerService::ElectPowerLoadCenter ( 1 ) );


	// Case 3 DCBussInverter   Inverter = 5000,
	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->bussType = ElectricPowerService::ElectPowerLoadCenter::dCBussInverter;
	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->inverterObj = std::unique_ptr < ElectricPowerService::DCtoACInverter >( new ElectricPowerService::DCtoACInverter( "TEST INVERTER") );
	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->inverterPresent = true;

	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->inverterObj->aCPowerOut  = 5000.0;
	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->inverterObj->aCEnergyOut = 5000.0*3600.0;
	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->updateLoadCenterRecords();

	EXPECT_NEAR( ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->electProdRate,   5000.0, 0.1 );
	EXPECT_NEAR( ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->electricityProd, 5000.0*3600.0, 0.1 );
}

TEST_F( EnergyPlusFixture, ManageElectricPowerTest_UpdateLoadCenterRecords_Case4 )
{

	std::string const idf_objects = delimited_string( { 
	"Version,8.4;",

"  ElectricLoadCenter:Distribution,",
"    Test Load Center,        !- Name",
"    Test Generator List,     !- Generator List Name",
"    TrackElectrical,         !- Generator Operation Scheme Type",
"    0,                       !- Demand Limit Scheme Purchased Electric Demand Limit {W}",
"    ,                        !- Track Schedule Name Scheme Schedule Name",
"    ,                        !- Track Meter Scheme Meter Name",
"    DirectCurrentWithInverterDCStorage,                  !- Electrical Buss Type",
"    Test Inverter ,                        !- Inverter Object Name",
"    Test Storage Bank;       !- Electrical Storage Object Name",

"  ElectricLoadCenter:Inverter:Simple,",
"    Test Inverter,",
"    ALWAYS_ON, !- availability schedule",
"    , !- zone name"   ,
"    , !- radiative fraction",
"    1.0 ; !- Inverter efficiency",

"  ElectricLoadCenter:Storage:Simple,",
"    Test Storage Bank,",
"    ALWAYS_ON, !- availability schedule",
"    , !- zone name"   ,
"    , !- radiative fraction",
"    1.0 , !- Nominal Energetic Efficiency for Charging",
"    1.0,  !- Nominal Discharging Energetic efficiency",
"    1.0E9, !- Maximum storage capacity",
"    5000.0, !- Maximum Power for Discharging",
"    5000.0, !- Maximum Power for Charging",
"    1.0E9; !- initial stat of charge",

"  ElectricLoadCenter:Generators,",
"    Test Generator List,     !- Name",
"    Test Gen 1,              !- Generator 1 Name",
"    Generator:InternalCombustionEngine,  !- Generator 1 Object Type",
"    1000.0,                  !- Generator 1 Rated Electric Power Output {W}",
"    ALWAYS_ON,               !- Generator 1 Availability Schedule Name",
"    ,                        !- Generator 1 Rated Thermal to Electrical Power Ratio",
"    Test Gen 2,              !- Generator 2 Name",
"    Generator:WindTurbine,   !- Generator 2 Object Type",
"    2000.0,                  !- Generator 2 Rated Electric Power Output {W}",
"    ALWAYS_ON,               !- Generator 2 Availability Schedule Name",
"    ;                        !- Generator 2 Rated Thermal to Electrical Power Ratio",

"  Schedule:Compact,",
"    ALWAYS_ON,               !- Name",
"    On/Off,                  !- Schedule Type Limits Name",
"    Through: 12/31,          !- Field 1",
"    For: AllDays,            !- Field 2",
"    Until: 24:00,1;          !- Field 3",
	} );

	ASSERT_FALSE( process_idf( idf_objects ) );

	ElectricPowerService::createFacilityElectricPowerServiceObject();
	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs.emplace_back( new ElectricPowerService::ElectPowerLoadCenter ( 1 ) );


	// Case 4 DCBussInverterDCStorage    Inverter = 5000,
	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->bussType = ElectricPowerService::ElectPowerLoadCenter::dCBussInverterDCStorage ;

	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->inverterObj = std::unique_ptr < ElectricPowerService::DCtoACInverter >( new ElectricPowerService::DCtoACInverter( "TEST INVERTER") );
	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->inverterPresent = true;

	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->inverterObj->aCPowerOut  = 5000.0;
	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->inverterObj->aCEnergyOut = 5000.0*3600.0;
	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->updateLoadCenterRecords();

	EXPECT_NEAR( ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->electProdRate,   5000.0, 0.1 );
	EXPECT_NEAR( ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->electricityProd, 5000.0*3600.0, 0.1 );
}

TEST_F( EnergyPlusFixture, ManageElectricPowerTest_UpdateLoadCenterRecords_Case5 )
{

	std::string const idf_objects = delimited_string( { 
	"Version,8.4;",

"  ElectricLoadCenter:Distribution,",
"    Test Load Center,        !- Name",
"    Test Generator List,     !- Generator List Name",
"    TrackElectrical,         !- Generator Operation Scheme Type",
"    0,                       !- Demand Limit Scheme Purchased Electric Demand Limit {W}",
"    ,                        !- Track Schedule Name Scheme Schedule Name",
"    ,                        !- Track Meter Scheme Meter Name",
"    DirectCurrentWithInverterACStorage,                  !- Electrical Buss Type",
"    Test Inverter ,                        !- Inverter Object Name",
"    Test Storage Bank;       !- Electrical Storage Object Name",

"  ElectricLoadCenter:Inverter:Simple,",
"    Test Inverter,",
"    ALWAYS_ON, !- availability schedule",
"    , !- zone name"   ,
"    , !- radiative fraction",
"    1.0 ; !- Inverter efficiency",

"  ElectricLoadCenter:Storage:Simple,",
"    Test Storage Bank,",
"    ALWAYS_ON, !- availability schedule",
"    , !- zone name"   ,
"    , !- radiative fraction",
"    1.0 , !- Nominal Energetic Efficiency for Charging",
"    1.0,  !- Nominal Discharging Energetic efficiency",
"    1.0E9, !- Maximum storage capacity",
"    5000.0, !- Maximum Power for Discharging",
"    5000.0, !- Maximum Power for Charging",
"    1.0E9; !- initial stat of charge",

"  ElectricLoadCenter:Generators,",
"    Test Generator List,     !- Name",
"    Test Gen 1,              !- Generator 1 Name",
"    Generator:InternalCombustionEngine,  !- Generator 1 Object Type",
"    1000.0,                  !- Generator 1 Rated Electric Power Output {W}",
"    ALWAYS_ON,               !- Generator 1 Availability Schedule Name",
"    ,                        !- Generator 1 Rated Thermal to Electrical Power Ratio",
"    Test Gen 2,              !- Generator 2 Name",
"    Generator:WindTurbine,   !- Generator 2 Object Type",
"    2000.0,                  !- Generator 2 Rated Electric Power Output {W}",
"    ALWAYS_ON,               !- Generator 2 Availability Schedule Name",
"    ;                        !- Generator 2 Rated Thermal to Electrical Power Ratio",

"  Schedule:Compact,",
"    ALWAYS_ON,               !- Name",
"    On/Off,                  !- Schedule Type Limits Name",
"    Through: 12/31,          !- Field 1",
"    For: AllDays,            !- Field 2",
"    Until: 24:00,1;          !- Field 3",
	} );

	ASSERT_FALSE( process_idf( idf_objects ) );

	ElectricPowerService::createFacilityElectricPowerServiceObject();
	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs.emplace_back( new ElectricPowerService::ElectPowerLoadCenter ( 1 ) );


	// Case 5 DCBussInverterACStorage     Inverter = 5000, , Storage 200-150=50, thermal should still be same as Case 1
	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->bussType =  ( ElectricPowerService::ElectPowerLoadCenter::dCBussInverterACStorage );
	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->inverterObj = std::unique_ptr < ElectricPowerService::DCtoACInverter >( new ElectricPowerService::DCtoACInverter( "TEST INVERTER") );
	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->inverterPresent = true;
	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->storageObj = std::unique_ptr < ElectricPowerService::ElectricStorage >( new ElectricPowerService::ElectricStorage (  "TEST STORAGE BANK"  ) );
	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->storageObj->drawnPower   = 200.0;
	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->storageObj->storedPower  = 150.0;
	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->storageObj->drawnEnergy  = 200.0*3600.0;
	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->storageObj->storedEnergy = 150.0*3600.0;
	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->elecGenCntrlObj[ 0 ]->thermalProdRate = 500.0;
	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->elecGenCntrlObj[ 1 ]->thermalProdRate = 750.0;
	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->elecGenCntrlObj[ 0 ]->thermalProd     = 500.0*3600.0;
	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->elecGenCntrlObj[ 1 ]->thermalProd     = 750.0*3600.0;
	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->inverterObj->aCPowerOut  = 5000.0;
	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->inverterObj->aCEnergyOut = 5000.0*3600.0;
	ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->updateLoadCenterRecords();

	EXPECT_NEAR( ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->electProdRate,   5050.0, 0.1 );
	EXPECT_NEAR( ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->electricityProd, 5050.0*3600.0, 0.1 );
	EXPECT_NEAR( ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->thermalProdRate, 1250.0, 0.1 );
	EXPECT_NEAR( ElectricPowerService::facilityElectricServiceObj->elecLoadCenterObjs[ 0 ]->thermalProd, 1250.0*3600.0, 0.1 );


}

