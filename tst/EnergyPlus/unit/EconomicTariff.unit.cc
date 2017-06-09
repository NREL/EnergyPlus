// EnergyPlus, Copyright (c) 1996-2017, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
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
#include <OutputProcessor.hh>
#include <OutputReportPredefined.hh>
#include <EconomicTariff.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace EnergyPlus::EconomicTariff;
using namespace EnergyPlus::OutputProcessor;
using namespace EnergyPlus::OutputReportPredefined;

TEST_F( EnergyPlusFixture, EconomicTariff_GetInput_Test)
{
	std::string const idf_objects = delimited_string( {
		"  UtilityCost:Tariff,                                                       ",
		"    ExampleFmc,              !- Name                                        ",
		"    ElectricityPurchased:Facility,  !- Output Meter Name                    ",
		"    kWh,                     !- Conversion Factor Choice                    ",
		"    ,                        !- Energy Conversion Factor                    ",
		"    ,                        !- Demand Conversion Factor                    ",
		"    TimeOfDaySchedule-Fmc,   !- Time of Use Period Schedule Name            ",
		"    TwoSeasonSchedule-Fmc,   !- Season Schedule Name                        ",
		"    ,                        !- Month Schedule Name                         ",
		"    ,                        !- Demand Window Length                        ",
		"    37.75;                   !- Monthly Charge or Variable Name             ",
		"                                                                            ",
		"  UtilityCost:Charge:Simple,                                                ",
		"    SummerOnPeak,            !- Name                                        ",
		"    ExampleFmc,              !- Tariff Name                                 ",
		"    peakEnergy,              !- Source Variable                             ",
		"    summer,                  !- Season                                      ",
		"    EnergyCharges,           !- Category Variable Name                      ",
		"    0.14009;                 !- Cost per Unit Value or Variable Name        ",
		"                                                                            ",
		"  UtilityCost:Charge:Simple,                                                ",
		"    SummerOffPeak,           !- Name                                        ",
		"    ExampleFmc,              !- Tariff Name                                 ",
		"    offPeakEnergy,           !- Source Variable                             ",
		"    summer,                  !- Season                                      ",
		"    EnergyCharges,           !- Category Variable Name                      ",
		"    0.06312;                 !- Cost per Unit Value or Variable Name        ",
		"                                                                            ",
		"  UtilityCost:Charge:Block,                                                 ",
		"    WinterOnPeak,            !- Name                                        ",
		"    ExampleFmc,              !- Tariff Name                                 ",
		"    peakEnergy,              !- Source Variable                             ",
		"    winter,                  !- Season                                      ",
		"    EnergyCharges,           !- Category Variable Name                      ",
		"    ,                        !- Remaining Into Variable                     ",
		"    ,                        !- Block Size Multiplier Value or Variable Name",
		"    650,                     !- Block Size 1 Value or Variable Name         ",
		"    0.04385,                 !- Block 1 Cost per Unit Value or Variable Name",
		"    350,                     !- Block Size 2 Value or Variable Name         ",
		"    0.03763,                 !- Block 2 Cost per Unit Value or Variable Name",
		"    remaining,               !- Block Size 3 Value or Variable Name         ",
		"    0.03704;                 !- Block 3 Cost per Unit Value or Variable Name",
		"                                                                            ",
		"  UtilityCost:Charge:Simple,                                                ",
		"    WinterOffPeak,           !- Name                                        ",
		"    ExampleFmc,              !- Tariff Name                                 ",
		"    offPeakEnergy,           !- Source Variable                             ",
		"    winter,                  !- Season                                      ",
		"    EnergyCharges,           !- Category Variable Name                      ",
		"    0.02420;                 !- Cost per Unit Value or Variable Name        ",
		"                                                                            ",
		"  UtilityCost:Qualify,                                                      ",
		"    MinDemand,               !- Name                                        ",
		"    ExampleFmc,              !- Tariff Name                                 ",
		"    TotalDemand,             !- Variable Name                               ",
		"    Minimum,                 !- Qualify Type                                ",
		"    12,                      !- Threshold Value or Variable Name            ",
		"    Annual,                  !- Season                                      ",
		"    Count,                   !- Threshold Test                              ",
		"    2;                       !- Number of Months                            ",
		"                                                                            ",
		"  UtilityCost:Computation,                                                  ",
		"    ManualExample,           !- Name                                        ",
		"    ExampleFmc,              !- Tariff Name                                 ",
		"    SumEneCharges SUM SUMMERONPEAK SUMMEROFFPEAK,  !- Compute Step 1        ",
		"    WinEneCharges SUM WINTERONPEAK WINTEROFFPEAK,  !- Compute Step 2        ",
		"    EnergyCharges SUM SumEneCharges WinEneCharges,  !- Compute Step 3       ",
		"    Basis SUM EnergyCharges DemandCharges ServiceCharges,  !- Compute Step 4",
		"    Subtotal SUM Basis Adjustment Surcharge,  !- Compute Step 5             ",
		"    Total SUM Subtotal Taxes;!- Compute Step 6                              ",
		"                                                                            ",
		"  UtilityCost:Ratchet,                                                      ",
		"    BillingDemand1,          !- Name                                        ",
		"    ExampleFmc,              !- Tariff Name                                 ",
		"    TotalDemand,             !- Baseline Source Variable                    ",
		"    TotalDemand,             !- Adjustment Source Variable                  ",
		"    Summer,                  !- Season From                                 ",
		"    Annual,                  !- Season To                                   ",
		"    0.80,                    !- Multiplier Value or Variable Name           ",
		"    0;                       !- Offset Value or Variable Name               ",
		"                                                                            ",
		"  Schedule:Compact,                                                         ",
		"    TwoSeasonSchedule-Fmc,   !- Name                                        ",
		"    number,                  !- Schedule Type Limits Name                   ",
		"    Through: 5/31,           !- Field 1                                     ",
		"    For: AllDays,            !- Field 2                                     ",
		"    Until: 24:00,1,          !- Field 3                                     ",
		"    Through: 9/30,           !- Field 5                                     ",
		"    For: AllDays,            !- Field 6                                     ",
		"    Until: 24:00,3,          !- Field 7                                     ",
		"    Through: 12/31,          !- Field 9                                     ",
		"    For: AllDays,            !- Field 10                                    ",
		"    Until: 24:00,1;          !- Field 11                                    ",
		"                                                                            ",
		"  Schedule:Compact,                                                         ",
		"    TimeOfDaySchedule-Fmc,   !- Name                                        ",
		"    number,                  !- Schedule Type Limits Name                   ",
		"    Through: 5/31,           !- Field 1                                     ",
		"    For: AllDays,            !- Field 2                                     ",
		"    Until: 15:00,3,          !- Field 3                                     ",
		"    Until: 22:00,1,          !- Field 5                                     ",
		"    Until: 24:00,3,          !- Field 7                                     ",
		"    Through: 9/30,           !- Field 9                                     ",
		"    For: AllDays,            !- Field 10                                    ",
		"    Until: 10:00,3,          !- Field 11                                    ",
		"    Until: 19:00,1,          !- Field 13                                    ",
		"    Until: 24:00,3,          !- Field 15                                    ",
		"    Through: 12/31,          !- Field 17                                    ",
		"    For: AllDays,            !- Field 18                                    ",
		"    Until: 15:00,3,          !- Field 19                                    ",
		"    Until: 22:00,1,          !- Field 21                                    ",
		"    Until: 24:00,3;          !- Field 23                                    ",
		"                                                                            ",

	} );

	ASSERT_FALSE( process_idf( idf_objects ) );

	UpdateUtilityBills();

	// tariff
	EXPECT_EQ( 1, numTariff );
	EXPECT_EQ( "EXAMPLEFMC", tariff( 1 ).tariffName );
	EXPECT_EQ( conversionKWH, tariff( 1 ).convChoice );
	EXPECT_EQ( 37.75, tariff( 1 ).monthChgVal );

	// qualify
	EXPECT_EQ( 1, numQualify );
	EXPECT_FALSE( qualify( 1 ).isMaximum );
	EXPECT_EQ( 12 , qualify( 1 ).thresholdVal );
	EXPECT_EQ( seasonAnnual, qualify( 1 ).season );
	EXPECT_FALSE( qualify( 1 ).isConsecutive );
	EXPECT_EQ( 2, qualify( 1 ).numberOfMonths );

	// ChargeSimple
	EXPECT_EQ( 3, numChargeSimple );
	EXPECT_EQ( seasonWinter, chargeSimple( 3 ).season );
	EXPECT_EQ( 0.02420, chargeSimple( 3 ).costPerVal );

	// ChargeBlock
	EXPECT_EQ( 1, numChargeBlock );
	EXPECT_EQ( seasonWinter, chargeBlock( 1 ).season );
	EXPECT_EQ( 3, chargeBlock( 1 ).numBlk );
	EXPECT_EQ( 350, chargeBlock( 1 ).blkSzVal( 2 ) );
	EXPECT_EQ( 0.03763, chargeBlock( 1 ).blkCostVal( 2 ) );

	// Ratchet
	EXPECT_EQ( 1, numRatchet );
	EXPECT_EQ( seasonSummer, ratchet( 1 ).seasonFrom );
	EXPECT_EQ( seasonAnnual , ratchet( 1 ).seasonTo );
	EXPECT_EQ( 0.80, ratchet( 1 ).multiplierVal );
	EXPECT_EQ( 0.0, ratchet( 1 ).offsetVal );

	// Computation
	EXPECT_EQ( 1, numComputation );


}

TEST_F( EnergyPlusFixture, EconomicTariff_WaterInput_Test)
{
	std::string const idf_objects = delimited_string( {
		"  UtilityCost:Tariff,                                                       ",
		"    ExampleWaterTariff,      !- Name                                        ",
		"    Water:Facility,          !- Output Meter Name                           ",
		"    ,                        !- Conversion Factor Choice                    ",
		"    ,                        !- Energy Conversion Factor                    ",
		"    ,                        !- Demand Conversion Factor                    ",
		"    ,                        !- Time of Use Period Schedule Name            ",
		"    ,                        !- Season Schedule Name                        ",
		"    ,                        !- Month Schedule Name                         ",
		"    ,                        !- Demand Window Length                        ",
		"    10;                      !- Monthly Charge or Variable Name             ",
		"                                                                            ",
		"  UtilityCost:Charge:Simple,                                                ",
		"    FlatWaterChargePerm3,    !- Name                                        ",
		"    ExampleWaterTariff,      !- Tariff Name                                 ",
		"    totalEnergy,             !- Source Variable                             ",
		"    Annual,                  !- Season                                      ",
		"    EnergyCharges,           !- Category Variable Name                      ",
		"    3.3076;                  !- Cost per Unit Value or Variable Name        ",
	} );

	ASSERT_FALSE( process_idf( idf_objects ) );

	// Create a water meter
	NumEnergyMeters = 1;
	EnergyMeters.allocate( NumEnergyMeters );
	EnergyMeters( 1 ).Name = "WATER:FACILITY";
	EnergyMeters( 1 ).ResourceType = "WATER";

	UpdateUtilityBills();

	// tariff
	EXPECT_EQ( 1, numTariff );
	EXPECT_EQ( "EXAMPLEWATERTARIFF", tariff( 1 ).tariffName );

	// Check that it correctly defaults the conversion factor
	EXPECT_EQ( kindMeterWater, tariff( 1 ).kindWaterMtr );
	EXPECT_EQ( conversionUSERDEF, tariff( 1 ).convChoice );
	EXPECT_EQ( 1, tariff( 1 ).energyConv );
	EXPECT_EQ( 1, tariff( 1 ).demandConv );
	EXPECT_EQ( 10, tariff( 1 ).monthChgVal );

}


TEST_F( EnergyPlusFixture, EconomicTariff_LEEDtariffReporting_Test )
{
	NumEnergyMeters = 4;
	EnergyMeters.allocate( NumEnergyMeters );
	EnergyMeters( 1 ).Name = "ELECTRICITY:FACILITY";
	EnergyMeters( 2 ).Name = "GAS:FACILITY";
	EnergyMeters( 3 ).Name = "DISTRICTCOOLING:FACILITY";
	EnergyMeters( 4 ).Name = "DISTRICTHEATING:FACILITY";

	numTariff = 4;
	tariff.allocate( numTariff );
	tariff( 1 ).tariffName = "SecondaryGeneralUnit";
	tariff( 1 ).isSelected = true;
	tariff( 1 ).totalAnnualCost = 4151.45;
	tariff( 1 ).totalAnnualEnergy = 4855.21;
	tariff( 1 ).kindElectricMtr = 3;
	tariff( 1 ).reportMeterIndx = 1;

	tariff( 2 ).tariffName = "SmallCGUnit";
	tariff( 2 ).isSelected = true;
	tariff( 2 ).totalAnnualCost = 415.56;
	tariff( 2 ).totalAnnualEnergy = 0.00;
	tariff( 2 ).reportMeterIndx = 2;

	tariff( 3 ).tariffName = "DistrictCoolingUnit";
	tariff( 3 ).isSelected = true;
	tariff( 3 ).totalAnnualCost = 55.22;
	tariff( 3 ).totalAnnualEnergy = 8.64;
	tariff( 3 ).reportMeterIndx = 3;

	tariff( 4 ).tariffName = "DistrictHeatingUnit";
	tariff( 4 ).isSelected = true;
	tariff( 4 ).totalAnnualCost = 15.98;
	tariff( 4 ).totalAnnualEnergy = 1.47;
	tariff( 4 ).reportMeterIndx = 4;

	SetPredefinedTables(); // need to setup the predefined table entry numbers

	LEEDtariffReporting();

	EXPECT_EQ( "SecondaryGeneralUnit", RetrievePreDefTableEntry( pdchLeedEtsRtNm, "Electricity" ) );
	EXPECT_EQ( "SmallCGUnit", RetrievePreDefTableEntry( pdchLeedEtsRtNm, "Natural Gas" ) );
	EXPECT_EQ( "DistrictCoolingUnit", RetrievePreDefTableEntry( pdchLeedEtsRtNm, "District Cooling" ) );
	EXPECT_EQ( "DistrictHeatingUnit", RetrievePreDefTableEntry( pdchLeedEtsRtNm, "District Heating" ) );

	EXPECT_EQ( "0.855", RetrievePreDefTableEntry( pdchLeedEtsVirt, "Electricity" ) );
	EXPECT_EQ( "6.391", RetrievePreDefTableEntry( pdchLeedEtsVirt, "District Cooling" ) );
	EXPECT_EQ( "10.871", RetrievePreDefTableEntry( pdchLeedEtsVirt, "District Heating" ) );

}


