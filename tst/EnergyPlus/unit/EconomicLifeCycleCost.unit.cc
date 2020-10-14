// EnergyPlus, Copyright (c) 1996-2020, The Board of Trustees of the University of Illinois,
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

// EnergyPlus::WindowManager unit tests

// C++ Headers
#include <iostream>

// Google Test Headers
#include <gtest/gtest.h>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataGlobalConstants.hh>
#include <EnergyPlus/EconomicTariff.hh>
#include <EnergyPlus/EconomicLifeCycleCost.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace EnergyPlus::EconomicLifeCycleCost;

using namespace EnergyPlus::DataGlobalConstants;
using namespace EnergyPlus::EconomicTariff;

TEST_F(EnergyPlusFixture, EconomicLifeCycleCost_GetInput)
{

    std::string const idf_objects = delimited_string({
        "  LifeCycleCost:Parameters,                                           ",
        "    TypicalLCC,              !- Name                                  ",
        "    EndOfYear,               !- Discounting Convention                ",
        "    ConstantDollar,          !- Inflation Approach                    ",
        "    0.03,                    !- Real Discount Rate                    ",
        "    ,                        !- Nominal Discount Rate                 ",
        "    ,                        !- Inflation                             ",
        "    January,                 !- Base Date Month                       ",
        "    2012,                    !- Base Date Year                        ",
        "    January,                 !- Service Date Month                    ",
        "    2014,                    !- Service Date Year                     ",
        "    22,                      !- Length of Study Period in Years       ",
        "    0,                       !- Tax rate                              ",
        "    ;                        !- Depreciation Method                   ",
        "                                                                      ",
        "  LifeCycleCost:NonrecurringCost,                                     ",
        "    Phase One Investment,    !- Name                                  ",
        "    Construction,            !- Category                              ",
        "    51500,                   !- Cost                                  ",
        "    BasePeriod,              !- Start of Costs                        ",
        "    0,                       !- Years from Start                      ",
        "    0;                       !- Months from Start                     ",
        "                                                                      ",
        "  LifeCycleCost:NonrecurringCost,                                     ",
        "    Phase Two Investment,    !- Name                                  ",
        "    Construction,            !- Category                              ",
        "    51500,                   !- Cost                                  ",
        "    BasePeriod,              !- Start of Costs                        ",
        "    1,                       !- Years from Start                      ",
        "    0;                       !- Months from Start                     ",
        "                                                                      ",
        "  LifeCycleCost:NonrecurringCost,                                     ",
        "    FanReplacement,          !- Name                                  ",
        "    OtherCapital,            !- Category                              ",
        "    12000,                   !- Cost                                  ",
        "    BasePeriod,              !- Start of Costs                        ",
        "    13,                      !- Years from Start                      ",
        "    0;                       !- Months from Start                     ",
        "                                                                      ",
        "  LifeCycleCost:NonrecurringCost,                                     ",
        "    PlantReplacement,        !- Name                                  ",
        "    OtherCapital,            !- Category                              ",
        "    60000,                   !- Cost                                  ",
        "    BasePeriod,              !- Start of Costs                        ",
        "    16,                      !- Years from Start                      ",
        "    0;                       !- Months from Start                     ",
        "                                                                      ",
        "  LifeCycleCost:NonrecurringCost,                                     ",
        "    ResidualValue,           !- Name                                  ",
        "    Salvage,                 !- Category                              ",
        "    -20000,                  !- Cost                                  ",
        "    BasePeriod,              !- Start of Costs                        ",
        "    21,                      !- Years from Start                      ",
        "    0;                       !- Months from Start                     ",
        "                                                                      ",
        "  LifeCycleCost:RecurringCosts,                                       ",
        "    AnnualMaint,             !- Name                                  ",
        "    Maintenance,             !- Category                              ",
        "    7000,                    !- Cost                                  ",
        "    ServicePeriod,           !- Start of Costs                        ",
        "    0,                       !- Years from Start                      ",
        "    0,                       !- Months from Start                     ",
        "    1,                       !- Repeat Period Years                   ",
        "    0,                       !- Repeat Period Months                  ",
        "    0;                       !- Annual escalation rate                ",
        "                                                                      ",
        "  LifeCycleCost:UsePriceEscalation,                                   ",
        "    MidWest  Commercial-Electricity,  !- Name                         ",
        "    ElectricityPurchased,    !- Resource                              ",
        "    2012,                    !- Escalation Start Year                 ",
        "    January,                 !- Escalation Start Month                ",
        "    1.0072,                  !- Year 1 Escalation                     ",
        "    1.0148,                  !- Year 2 Escalation                     ",
        "    1.0315,                  !- Year 3 Escalation                     ",
        "    1.0493,                  !- Year 4 Escalation                     ",
        "    1.0505,                  !- Year 5 Escalation                     ",
        "    1.0451,                  !- Year 6 Escalation                     ",
        "    1.0429,                  !- Year 7 Escalation                     ",
        "    1.0410,                  !- Year 8 Escalation                     ",
        "    1.0406,                  !- Year 9 Escalation                     ",
        "    1.0444,                  !- Year 10 Escalation                    ",
        "    1.0505,                  !- Year 11 Escalation                    ",
        "    1.0535,                  !- Year 12 Escalation                    ",
        "    1.0524,                  !- Year 13 Escalation                    ",
        "    1.0478,                  !- Year 14 Escalation                    ",
        "    1.0429,                  !- Year 15 Escalation                    ",
        "    1.0391,                  !- Year 16 Escalation                    ",
        "    1.0372,                  !- Year 17 Escalation                    ",
        "    1.0360,                  !- Year 18 Escalation                    ",
        "    1.0341,                  !- Year 19 Escalation                    ",
        "    1.0319,                  !- Year 20 Escalation                    ",
        "    1.0288,                  !- Year 21 Escalation                    ",
        "    1.0341,                  !- Year 22 Escalation                    ",
        "    1.0425,                  !- Year 23 Escalation                    ",
        "    1.0508,                  !- Year 24 Escalation                    ",
        "    1.0569,                  !- Year 25 Escalation                    ",
        "    1.0626,                  !- Year 26 Escalation                    ",
        "    1.0694,                  !- Year 27 Escalation                    ",
        "    1.0721,                  !- Year 28 Escalation                    ",
        "    1.0744,                  !- Year 29 Escalation                    ",
        "    1.0774;                  !- Year 30 Escalation                    ",
        "                                                                      ",
        "  LifeCycleCost:UsePriceEscalation,                                   ",
        "    MidWest  Commercial-Electricity,  !- Name                         ",
        "    ElectricitySurplusSold,  !- Resource                              ",
        "    2012,                    !- Escalation Start Year                 ",
        "    January,                 !- Escalation Start Month                ",
        "    1.0072,                  !- Year 1 Escalation                     ",
        "    1.0148,                  !- Year 2 Escalation                     ",
        "    1.0315,                  !- Year 3 Escalation                     ",
        "    1.0493,                  !- Year 4 Escalation                     ",
        "    1.0505,                  !- Year 5 Escalation                     ",
        "    1.0451,                  !- Year 6 Escalation                     ",
        "    1.0429,                  !- Year 7 Escalation                     ",
        "    1.0410,                  !- Year 8 Escalation                     ",
        "    1.0406,                  !- Year 9 Escalation                     ",
        "    1.0444,                  !- Year 10 Escalation                    ",
        "    1.0505,                  !- Year 11 Escalation                    ",
        "    1.0535,                  !- Year 12 Escalation                    ",
        "    1.0524,                  !- Year 13 Escalation                    ",
        "    1.0478,                  !- Year 14 Escalation                    ",
        "    1.0429,                  !- Year 15 Escalation                    ",
        "    1.0391,                  !- Year 16 Escalation                    ",
        "    1.0372,                  !- Year 17 Escalation                    ",
        "    1.0360,                  !- Year 18 Escalation                    ",
        "    1.0341,                  !- Year 19 Escalation                    ",
        "    1.0319,                  !- Year 20 Escalation                    ",
        "    1.0288,                  !- Year 21 Escalation                    ",
        "    1.0341,                  !- Year 22 Escalation                    ",
        "    1.0425,                  !- Year 23 Escalation                    ",
        "    1.0508,                  !- Year 24 Escalation                    ",
        "    1.0569,                  !- Year 25 Escalation                    ",
        "    1.0626,                  !- Year 26 Escalation                    ",
        "    1.0694,                  !- Year 27 Escalation                    ",
        "    1.0721,                  !- Year 28 Escalation                    ",
        "    1.0744,                  !- Year 29 Escalation                    ",
        "    1.0774;                  !- Year 30 Escalation                    ",
        "                                                                      ",
        "  LifeCycleCost:UsePriceEscalation,                                   ",
        "    MidWest  Commercial-Natural gas,  !- Name                         ",
        "    NaturalGas,              !- Resource                              ",
        "    2012,                    !- Escalation Start Year                 ",
        "    January,                 !- Escalation Start Month                ",
        "    0.9950,                  !- Year 1 Escalation                     ",
        "    0.9711,                  !- Year 2 Escalation                     ",
        "    0.9774,                  !- Year 3 Escalation                     ",
        "    0.9849,                  !- Year 4 Escalation                     ",
        "    0.9899,                  !- Year 5 Escalation                     ",
        "    1.0000,                  !- Year 6 Escalation                     ",
        "    1.0113,                  !- Year 7 Escalation                     ",
        "    1.0289,                  !- Year 8 Escalation                     ",
        "    1.0616,                  !- Year 9 Escalation                     ",
        "    1.1031,                  !- Year 10 Escalation                    ",
        "    1.1321,                  !- Year 11 Escalation                    ",
        "    1.1484,                  !- Year 12 Escalation                    ",
        "    1.1623,                  !- Year 13 Escalation                    ",
        "    1.1786,                  !- Year 14 Escalation                    ",
        "    1.1925,                  !- Year 15 Escalation                    ",
        "    1.2025,                  !- Year 16 Escalation                    ",
        "    1.2138,                  !- Year 17 Escalation                    ",
        "    1.2289,                  !- Year 18 Escalation                    ",
        "    1.2453,                  !- Year 19 Escalation                    ",
        "    1.2642,                  !- Year 20 Escalation                    ",
        "    1.2818,                  !- Year 21 Escalation                    ",
        "    1.3182,                  !- Year 22 Escalation                    ",
        "    1.3560,                  !- Year 23 Escalation                    ",
        "    1.3899,                  !- Year 24 Escalation                    ",
        "    1.4151,                  !- Year 25 Escalation                    ",
        "    1.4491,                  !- Year 26 Escalation                    ",
        "    1.4881,                  !- Year 27 Escalation                    ",
        "    1.4818,                  !- Year 28 Escalation                    ",
        "    1.4931,                  !- Year 29 Escalation                    ",
        "    1.5145;                  !- Year 30 Escalation                    ",
        "                                                                      ",
        "  LifeCycleCost:UseAdjustment,                                        ",
        "    NoElectricUseAdjustment, !- Name                                  ",
        "    ElectricityPurchased,    !- Resource                              ",
        "    1.0;                     !- Year 1 Multiplier                     ",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    GetInputForLifeCycleCost(state);

    EXPECT_EQ(disConvEndOfYear, discountConvension);
    EXPECT_EQ(inflAppConstantDollar, inflationApproach);
    EXPECT_EQ(0.03, realDiscountRate);
    EXPECT_EQ(1, baseDateMonth);
    EXPECT_EQ(2012, baseDateYear);
    EXPECT_EQ(22 * 12, lengthStudyTotalMonths);

    EXPECT_EQ(5, numNonrecurringCost);
    EXPECT_EQ("RESIDUALVALUE", NonrecurringCost(5).name);
    EXPECT_EQ(costCatSalvage, NonrecurringCost(5).category);
    EXPECT_EQ(startBasePeriod, NonrecurringCost(5).startOfCosts);
    EXPECT_EQ(-20000., NonrecurringCost(5).cost);

    EXPECT_EQ(1, numRecurringCosts);
    EXPECT_EQ("ANNUALMAINT", RecurringCosts(1).name);
    EXPECT_EQ(costCatMaintenance, RecurringCosts(1).category);
    EXPECT_EQ(7000., RecurringCosts(1).cost);
    EXPECT_EQ(startServicePeriod, RecurringCosts(1).startOfCosts);
    EXPECT_EQ(1, RecurringCosts(1).repeatPeriodYears);

    EXPECT_EQ(3, numUsePriceEscalation);
    EXPECT_EQ("MIDWEST  COMMERCIAL-NATURAL GAS", UsePriceEscalation(3).name);
    EXPECT_EQ(2012, UsePriceEscalation(3).escalationStartYear);
    EXPECT_EQ(1.1321, UsePriceEscalation(3).Escalation(11));
    EXPECT_EQ(1.2818, UsePriceEscalation(3).Escalation(21));

    EXPECT_EQ(1, numUseAdjustment);
    EXPECT_EQ("NOELECTRICUSEADJUSTMENT", UseAdjustment(1).name);
    EXPECT_EQ(1.0, UseAdjustment(1).Adjustment(1));
}

TEST_F(EnergyPlusFixture, EconomicLifeCycleCost_ProcessMaxInput)
{

    std::string const idf_objects = delimited_string({
        "  LifeCycleCost:Parameters,                                           ",
        "    TypicalLCC,              !- Name                                  ",
        "    EndOfYear,               !- Discounting Convention                ",
        "    ConstantDollar,          !- Inflation Approach                    ",
        "    0.03,                    !- Real Discount Rate                    ",
        "    ,                        !- Nominal Discount Rate                 ",
        "    ,                        !- Inflation                             ",
        "    January,                 !- Base Date Month                       ",
        "    2012,                    !- Base Date Year                        ",
        "    January,                 !- Service Date Month                    ",
        "    2014,                    !- Service Date Year                     ",
        "    100,                     !- Length of Study Period in Years       ",
        "    0,                       !- Tax rate                              ",
        "    ;                        !- Depreciation Method                   ",
        "                                                                      ",
        "  LifeCycleCost:NonrecurringCost,                                     ",
        "    FanReplacement,          !- Name                                  ",
        "    OtherCapital,            !- Category                              ",
        "    12000,                   !- Cost                                  ",
        "    BasePeriod,              !- Start of Costs                        ",
        "    13,                      !- Years from Start                      ",
        "    0;                       !- Months from Start                     ",
        "                                                                      ",
        "  LifeCycleCost:RecurringCosts,                                       ",
        "    AnnualMaint,             !- Name                                  ",
        "    Maintenance,             !- Category                              ",
        "    7000,                    !- Cost                                  ",
        "    ServicePeriod,           !- Start of Costs                        ",
        "    0,                       !- Years from Start                      ",
        "    0,                       !- Months from Start                     ",
        "    1,                       !- Repeat Period Years                   ",
        "    0,                       !- Repeat Period Months                  ",
        "    0;                       !- Annual escalation rate                ",
        "                                                                      ",
        "  LifeCycleCost:UsePriceEscalation,                                   ",
        "    MidWest  Commercial-Electricity,  !- Name                         ",
        "    ElectricityPurchased,    !- Resource                              ",
        "    2012,                    !- Escalation Start Year                 ",
        "    January,                 !- Escalation Start Month                ",
        "    1.007, 1.014, 1.031, 1.049, 1.050, 1.045, 1.042, 1.041, 1.040, 1.044, !- Year 1-10 Escalation ",
        "    1.008, 1.014, 1.031, 1.049, 1.050, 1.045, 1.042, 1.041, 1.040, 1.044, !- Year 11-20 Escalation ",
        "    1.009, 1.014, 1.031, 1.049, 1.050, 1.045, 1.042, 1.041, 1.040, 1.044, !- Year 21-30 Escalation ",
        "    1.009, 1.014, 1.031, 1.049, 1.050, 1.045, 1.042, 1.041, 1.040, 1.044, !- Year 31-40 Escalation ",
        "    1.009, 1.014, 1.031, 1.049, 1.050, 1.045, 1.042, 1.041, 1.040, 1.044, !- Year 41-50 Escalation ",
        "    1.009, 1.014, 1.031, 1.049, 1.050, 1.045, 1.042, 1.041, 1.040, 1.044, !- Year 51-60 Escalation ",
        "    1.009, 1.014, 1.031, 1.049, 1.050, 1.045, 1.042, 1.041, 1.040, 1.044, !- Year 61-70 Escalation ",
        "    1.009, 1.014, 1.031, 1.049, 1.050, 1.045, 1.042, 1.041, 1.040, 1.044, !- Year 71-80 Escalation ",
        "    1.009, 1.014, 1.031, 1.049, 1.050, 1.045, 1.042, 1.041, 1.040, 1.044, !- Year 81-90 Escalation ",
        "    1.007, 1.014, 1.031, 1.049, 1.050, 1.045, 1.042, 1.041, 1.099, 1.100; !- Year 91-100 Escalation ",
        "                                                                      ",
        "  LifeCycleCost:UsePriceEscalation,                                   ",
        "    MidWest  Commercial-Electricity,  !- Name                         ",
        "    ElectricitySurplusSold,  !- Resource                              ",
        "    2012,                    !- Escalation Start Year                 ",
        "    January,                 !- Escalation Start Month                ",
        "    1.007, 1.014, 1.031, 1.049, 1.050, 1.045, 1.042, 1.041, 1.040, 1.044, !- Year 1-10 Escalation ",
        "    1.008, 1.014, 1.031, 1.049, 1.050, 1.045, 1.042, 1.041, 1.040, 1.044, !- Year 11-20 Escalation ",
        "    1.009, 1.014, 1.031, 1.049, 1.050, 1.045, 1.042, 1.041, 1.040, 1.044, !- Year 21-30 Escalation ",
        "    1.009, 1.014, 1.031, 1.049, 1.050, 1.045, 1.042, 1.041, 1.040, 1.044, !- Year 31-40 Escalation ",
        "    1.009, 1.014, 1.031, 1.049, 1.050, 1.045, 1.042, 1.041, 1.040, 1.044, !- Year 41-50 Escalation ",
        "    1.009, 1.014, 1.031, 1.049, 1.050, 1.045, 1.042, 1.041, 1.040, 1.044, !- Year 51-60 Escalation ",
        "    1.009, 1.014, 1.031, 1.049, 1.050, 1.045, 1.042, 1.041, 1.040, 1.044, !- Year 61-70 Escalation ",
        "    1.009, 1.014, 1.031, 1.049, 1.050, 1.045, 1.042, 1.041, 1.040, 1.044, !- Year 71-80 Escalation ",
        "    1.009, 1.014, 1.031, 1.049, 1.050, 1.045, 1.042, 1.041, 1.040, 1.044, !- Year 81-90 Escalation ",
        "    1.007, 1.014, 1.031, 1.049, 1.050, 1.045, 1.042, 1.041, 1.099, 1.100; !- Year 91-100 Escalation ",
        "                                                                      ",
        "  LifeCycleCost:UsePriceEscalation,                                   ",
        "    MidWest  Commercial-Natural gas,  !- Name                         ",
        "    NaturalGas,              !- Resource                              ",
        "    2012,                    !- Escalation Start Year                 ",
        "    January,                 !- Escalation Start Month                ",
        "    1.007, 1.014, 1.031, 1.049, 1.050, 1.045, 1.042, 1.041, 1.040, 1.044, !- Year 1-10 Escalation ",
        "    1.008, 1.014, 1.031, 1.049, 1.050, 1.045, 1.042, 1.041, 1.040, 1.044, !- Year 11-20 Escalation ",
        "    1.009, 1.014, 1.031, 1.049, 1.050, 1.045, 1.042, 1.041, 1.040, 1.044, !- Year 21-30 Escalation ",
        "    1.009, 1.014, 1.031, 1.049, 1.050, 1.045, 1.042, 1.041, 1.040, 1.044, !- Year 31-40 Escalation ",
        "    1.009, 1.014, 1.031, 1.049, 1.050, 1.045, 1.042, 1.041, 1.040, 1.044, !- Year 41-50 Escalation ",
        "    1.009, 1.014, 1.031, 1.049, 1.050, 1.045, 1.042, 1.041, 1.040, 1.044, !- Year 51-60 Escalation ",
        "    1.009, 1.014, 1.031, 1.049, 1.050, 1.045, 1.042, 1.041, 1.040, 1.044, !- Year 61-70 Escalation ",
        "    1.009, 1.014, 1.031, 1.049, 1.050, 1.045, 1.042, 1.041, 1.040, 1.044, !- Year 71-80 Escalation ",
        "    1.009, 1.014, 1.031, 1.049, 1.050, 1.045, 1.042, 1.041, 1.040, 1.044, !- Year 81-90 Escalation ",
        "    1.007, 1.014, 1.031, 1.049, 1.050, 1.045, 1.042, 1.041, 1.099, 1.100; !- Year 91-100 Escalation ",
        "                                                                      ",
        "  LifeCycleCost:UseAdjustment,                                        ",
        "    NoElectricUseAdjustment, !- Name                                  ",
        "    ElectricityPurchased,    !- Resource                              ",
        "    1.007, 1.014, 1.031, 1.049, 1.050, 1.045, 1.042, 1.041, 1.040, 1.044, !- Year 1-10 Escalation ",
        "    1.008, 1.014, 1.031, 1.049, 1.050, 1.045, 1.042, 1.041, 1.040, 1.044, !- Year 11-20 Escalation ",
        "    1.009, 1.014, 1.031, 1.049, 1.050, 1.045, 1.042, 1.041, 1.040, 1.044, !- Year 21-30 Escalation ",
        "    1.009, 1.014, 1.031, 1.049, 1.050, 1.045, 1.042, 1.041, 1.040, 1.044, !- Year 31-40 Escalation ",
        "    1.009, 1.014, 1.031, 1.049, 1.050, 1.045, 1.042, 1.041, 1.040, 1.044, !- Year 41-50 Escalation ",
        "    1.009, 1.014, 1.031, 1.049, 1.050, 1.045, 1.042, 1.041, 1.040, 1.044, !- Year 51-60 Escalation ",
        "    1.009, 1.014, 1.031, 1.049, 1.050, 1.045, 1.042, 1.041, 1.040, 1.044, !- Year 61-70 Escalation ",
        "    1.009, 1.014, 1.031, 1.049, 1.050, 1.045, 1.042, 1.041, 1.040, 1.044, !- Year 71-80 Escalation ",
        "    1.009, 1.014, 1.031, 1.049, 1.050, 1.045, 1.042, 1.041, 1.040, 1.044, !- Year 81-90 Escalation ",
        "    1.007, 1.014, 1.031, 1.049, 1.050, 1.045, 1.042, 1.041, 1.099, 1.100; !- Year 91-100 Escalation ",

    });

    ASSERT_TRUE(process_idf(idf_objects));

    GetInputForLifeCycleCost(state);

    EXPECT_EQ(disConvEndOfYear, discountConvension);
    EXPECT_EQ(inflAppConstantDollar, inflationApproach);
    EXPECT_EQ(0.03, realDiscountRate);
    EXPECT_EQ(1, baseDateMonth);
    EXPECT_EQ(2012, baseDateYear);
    EXPECT_EQ(100 * 12, lengthStudyTotalMonths);

    EXPECT_EQ(3, numUsePriceEscalation);
    EXPECT_EQ("MIDWEST  COMMERCIAL-NATURAL GAS", UsePriceEscalation(3).name);
    EXPECT_EQ(2012, UsePriceEscalation(3).escalationStartYear);
    EXPECT_EQ(1.007, UsePriceEscalation(3).Escalation(1));
    EXPECT_EQ(1.008, UsePriceEscalation(3).Escalation(11));
    EXPECT_EQ(1.009, UsePriceEscalation(3).Escalation(21));
    EXPECT_EQ(1.099, UsePriceEscalation(3).Escalation(99));
    EXPECT_EQ(1.100, UsePriceEscalation(3).Escalation(100));

    EXPECT_EQ(1, numUseAdjustment);
    EXPECT_EQ("NOELECTRICUSEADJUSTMENT", UseAdjustment(1).name);
    EXPECT_EQ(1.007, UseAdjustment(1).Adjustment(1));
    EXPECT_EQ(1.008, UseAdjustment(1).Adjustment(11));
    EXPECT_EQ(1.009, UseAdjustment(1).Adjustment(21));
    EXPECT_EQ(1.099, UseAdjustment(1).Adjustment(99));
    EXPECT_EQ(1.100, UseAdjustment(1).Adjustment(100));
}

TEST_F(EnergyPlusFixture, EconomicLifeCycleCost_ComputeEscalatedEnergyCosts)
{
    lengthStudyYears = 5;

    numCashFlow = 1;
    CashFlow.allocate(numCashFlow);
    CashFlow(1).pvKind = pvkEnergy;
    CashFlow(1).Resource = 1001;
    CashFlow(1).yrAmount.allocate(lengthStudyYears);
    CashFlow(1).yrAmount(1) = 100;
    CashFlow(1).yrAmount(2) = 110;
    CashFlow(1).yrAmount(3) = 120;
    CashFlow(1).yrAmount(4) = 130;
    CashFlow(1).yrAmount(5) = 140;

    numResourcesUsed = 1;

    EscalatedEnergy.allocate(lengthStudyYears, NumOfResourceTypes);
    EscalatedEnergy = 0.0;
    EscalatedTotEnergy.allocate(lengthStudyYears);
    EscalatedTotEnergy = 0.0;

    ComputeEscalatedEnergyCosts();
    EXPECT_NEAR(EscalatedEnergy(1, 1), 100., 0.001);
    EXPECT_NEAR(EscalatedEnergy(2, 1), 110., 0.001);
    EXPECT_NEAR(EscalatedEnergy(3, 1), 120., 0.001);
    EXPECT_NEAR(EscalatedEnergy(4, 1), 130., 0.001);
    EXPECT_NEAR(EscalatedEnergy(5, 1), 140., 0.001);

    EXPECT_NEAR(EscalatedTotEnergy(1), 100., 0.001);
    EXPECT_NEAR(EscalatedTotEnergy(2), 110., 0.001);
    EXPECT_NEAR(EscalatedTotEnergy(3), 120., 0.001);
    EXPECT_NEAR(EscalatedTotEnergy(4), 130., 0.001);
    EXPECT_NEAR(EscalatedTotEnergy(5), 140., 0.001);

    numUsePriceEscalation = 1;
    UsePriceEscalation.allocate(numUsePriceEscalation);
    UsePriceEscalation(1).resource = 1001;
    UsePriceEscalation(1).Escalation.allocate(lengthStudyYears);
    UsePriceEscalation(1).Escalation(1) = 1.03;
    UsePriceEscalation(1).Escalation(2) = 1.05;
    UsePriceEscalation(1).Escalation(3) = 1.07;
    UsePriceEscalation(1).Escalation(4) = 1.11;
    UsePriceEscalation(1).Escalation(5) = 1.15;

    //reset this variable to zero
    EscalatedTotEnergy = 0.0;

    ComputeEscalatedEnergyCosts();
    EXPECT_NEAR(EscalatedEnergy(1, 1), 103.0, 0.001);
    EXPECT_NEAR(EscalatedEnergy(2, 1), 115.5, 0.001);
    EXPECT_NEAR(EscalatedEnergy(3, 1), 128.4, 0.001);
    EXPECT_NEAR(EscalatedEnergy(4, 1), 144.3, 0.001);
    EXPECT_NEAR(EscalatedEnergy(5, 1), 161.0, 0.001);

    EXPECT_NEAR(EscalatedTotEnergy(1), 103., 0.001);
    EXPECT_NEAR(EscalatedTotEnergy(2), 115.5, 0.001);
    EXPECT_NEAR(EscalatedTotEnergy(3), 128.4, 0.001);
    EXPECT_NEAR(EscalatedTotEnergy(4), 144.3, 0.001);
    EXPECT_NEAR(EscalatedTotEnergy(5), 161.0, 0.001);

}

TEST_F(EnergyPlusFixture, EconomicLifeCycleCost_MonthToMonthNumber)
{
    EXPECT_EQ(1, MonthToMonthNumber("January",1));
    EXPECT_EQ(2, MonthToMonthNumber("February",1));
    EXPECT_EQ(3, MonthToMonthNumber("March",1));
    EXPECT_EQ(4, MonthToMonthNumber("April",1));
    EXPECT_EQ(5, MonthToMonthNumber("May",1));
    EXPECT_EQ(6, MonthToMonthNumber("June",1));
    EXPECT_EQ(7, MonthToMonthNumber("July",1));
    EXPECT_EQ(8, MonthToMonthNumber("August",1));
    EXPECT_EQ(9, MonthToMonthNumber("September",1));
    EXPECT_EQ(10, MonthToMonthNumber("October",1));
    EXPECT_EQ(11, MonthToMonthNumber("November",1));
    EXPECT_EQ(12, MonthToMonthNumber("December",1));
    EXPECT_EQ(99, MonthToMonthNumber("Hexember",99));
}


TEST_F(EnergyPlusFixture, EconomicLifeCycleCost_ExpressAsCashFlows)
{
    baseDateYear = 2020;
    baseDateMonth = 1;

    serviceDateYear = 2023;
    serviceDateMonth = 1;

    lengthStudyYears = 5;
    lengthStudyTotalMonths = lengthStudyYears * 12;


    numTariff = 1;
    tariff.allocate(1);
    tariff(1).isSelected = true;
    tariff(1).resourceNum = 1001;
    tariff(1).ptTotal = 1;
    econVar.allocate(1);
    econVar(1).values.allocate(12);
    econVar(1).values(1) = 101.;
    econVar(1).values(2) = 102.;
    econVar(1).values(3) = 103.;
    econVar(1).values(4) = 104.;
    econVar(1).values(5) = 105.;
    econVar(1).values(6) = 106.;
    econVar(1).values(7) = 107.;
    econVar(1).values(8) = 108.;
    econVar(1).values(9) = 109.;
    econVar(1).values(10) = 110.;
    econVar(1).values(11) = 111.;
    econVar(1).values(12) = 112.;

    numNonrecurringCost = 1;
    NonrecurringCost.allocate(1);
    NonrecurringCost(1).name = "MiscConstruction";
    NonrecurringCost(1).name = "MiscConstruction";
    NonrecurringCost(1).category = costCatConstruction;
    NonrecurringCost(1).cost = 123456.;
    NonrecurringCost(1).startOfCosts = startServicePeriod;
    NonrecurringCost(1).totalMonthsFromStart = 10;

    ExpressAsCashFlows(state);

    EXPECT_NEAR(CashFlow(17).mnAmount(47), 123456., 0.001);  // 36 months plus 10 months plus one month

    // first year
    EXPECT_NEAR(CashFlow(18).mnAmount(37), 101., 0.001);
    EXPECT_NEAR(CashFlow(18).mnAmount(38), 102., 0.001);
    EXPECT_NEAR(CashFlow(18).mnAmount(39), 103., 0.001);
    EXPECT_NEAR(CashFlow(18).mnAmount(40), 104., 0.001);
    EXPECT_NEAR(CashFlow(18).mnAmount(41), 105., 0.001);
    EXPECT_NEAR(CashFlow(18).mnAmount(42), 106., 0.001);
    EXPECT_NEAR(CashFlow(18).mnAmount(43), 107., 0.001);
    EXPECT_NEAR(CashFlow(18).mnAmount(44), 108., 0.001);
    EXPECT_NEAR(CashFlow(18).mnAmount(45), 109., 0.001);
    EXPECT_NEAR(CashFlow(18).mnAmount(46), 110., 0.001);
    EXPECT_NEAR(CashFlow(18).mnAmount(47), 111., 0.001);
    EXPECT_NEAR(CashFlow(18).mnAmount(48), 112., 0.001);
    // second  year
    EXPECT_NEAR(CashFlow(18).mnAmount(49), 101., 0.001);
    EXPECT_NEAR(CashFlow(18).mnAmount(50), 102., 0.001);
    EXPECT_NEAR(CashFlow(18).mnAmount(51), 103., 0.001);
    EXPECT_NEAR(CashFlow(18).mnAmount(52), 104., 0.001);
    EXPECT_NEAR(CashFlow(18).mnAmount(53), 105., 0.001);
    EXPECT_NEAR(CashFlow(18).mnAmount(54), 106., 0.001);
    EXPECT_NEAR(CashFlow(18).mnAmount(55), 107., 0.001);
    EXPECT_NEAR(CashFlow(18).mnAmount(56), 108., 0.001);
    EXPECT_NEAR(CashFlow(18).mnAmount(57), 109., 0.001);
    EXPECT_NEAR(CashFlow(18).mnAmount(58), 110., 0.001);
    EXPECT_NEAR(CashFlow(18).mnAmount(59), 111., 0.001);
    EXPECT_NEAR(CashFlow(18).mnAmount(60), 112., 0.001);

    EXPECT_NEAR(CashFlow(18).yrAmount(4), 1278., 0.001);
    EXPECT_NEAR(CashFlow(18).yrAmount(5), 1278., 0.001);

    EXPECT_NEAR(CashFlow(costCatEnergy).yrAmount(4), 1278., 0.001);
    EXPECT_NEAR(CashFlow(costCatEnergy).yrAmount(5), 1278., 0.001);

    EXPECT_NEAR(CashFlow(costCatTotEnergy).yrAmount(4), 1278., 0.001);
    EXPECT_NEAR(CashFlow(costCatTotEnergy).yrAmount(5), 1278., 0.001);

    EXPECT_NEAR(CashFlow(costCatConstruction).yrAmount(4), 123456, 0.001);
    EXPECT_NEAR(CashFlow(costCatTotCaptl).yrAmount(4), 123456, 0.001);

    EXPECT_NEAR(CashFlow(costCatTotGrand).yrAmount(4), 1278. + 123456., 0.001);
    EXPECT_NEAR(CashFlow(costCatTotGrand).yrAmount(5), 1278., 0.001);
}

