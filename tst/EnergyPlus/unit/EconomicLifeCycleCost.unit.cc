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

// EnergyPlus::WindowManager unit tests

// C++ Headers
#include <iostream>

// Google Test Headers
#include <gtest/gtest.h>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <DataGlobals.hh>
#include <EconomicLifeCycleCost.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace EnergyPlus::EconomicLifeCycleCost;

TEST_F(EnergyPlusFixture, EconomicLifeCycleCost_GetInput)
{

    std::string const idf_objects = delimited_string({
        "Version,8.5;",
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

    GetInputForLifeCycleCost();

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
