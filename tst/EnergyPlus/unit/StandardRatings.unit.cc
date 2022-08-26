// EnergyPlus, Copyright (c) 1996-2022, The Board of Trustees of the University of Illinois,
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

// EnergyPlus::StandardRatings unit tests

// Google test headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/ChillerElectricEIR.hh>
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/DXCoils.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/IOFiles.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/StandardRatings.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::StandardRatings;
using namespace EnergyPlus::CurveManager;
using namespace EnergyPlus::DataHVACGlobals;
using namespace EnergyPlus::DXCoils;
using namespace EnergyPlus::ChillerElectricEIR;

namespace EnergyPlus {

TEST_F(EnergyPlusFixture, SingleSpeedHeatingCoilCurveTest)
{
    // Test that the standard ratings calculation with negative curve value

    using Psychrometrics::PsyRhoAirFnPbTdbW;
    using StandardRatings::SingleSpeedDXHeatingCoilStandardRatings;

    // Set up heating coil and curves.
    int DXCoilNum;
    state->dataDXCoils->NumDXCoils = 1;
    DXCoilNum = 1;
    state->dataDXCoils->DXCoil.allocate(state->dataDXCoils->NumDXCoils);
    state->dataDXCoils->DXCoilNumericFields.allocate(1);
    state->dataDXCoils->DXCoilOutletTemp.allocate(state->dataDXCoils->NumDXCoils);
    state->dataDXCoils->DXCoilOutletHumRat.allocate(state->dataDXCoils->NumDXCoils);
    state->dataDXCoils->DXCoilFanOpMode.allocate(state->dataDXCoils->NumDXCoils);
    state->dataDXCoils->DXCoilPartLoadRatio.allocate(state->dataDXCoils->NumDXCoils);
    state->dataDXCoils->DXCoilTotalHeating.allocate(state->dataDXCoils->NumDXCoils);
    state->dataDXCoils->DXCoilHeatInletAirDBTemp.allocate(state->dataDXCoils->NumDXCoils);
    state->dataDXCoils->DXCoilHeatInletAirWBTemp.allocate(state->dataDXCoils->NumDXCoils);
    DXCoilData &Coil = state->dataDXCoils->DXCoil(DXCoilNum);

    Coil.Name = "DX Single Speed Heating Coil";
    Coil.DXCoilType = "Coil:Heating:DX:SingleSpeed";
    Coil.DXCoilType_Num = CoilDX_HeatingEmpirical;
    Coil.SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
    Coil.RatedSHR(1) = 1.0;
    Coil.RatedTotCap(1) = 1600.0;
    Coil.RatedCOP(1) = 4.0;
    Coil.RatedEIR(1) = 1 / Coil.RatedCOP(1);
    Coil.RatedAirVolFlowRate(1) = 0.50;
    Coil.RatedAirMassFlowRate(1) =
        Coil.RatedAirVolFlowRate(1) * PsyRhoAirFnPbTdbW(*state, state->dataEnvrn->StdBaroPress, 21.11, 0.00881, "InitDXCoil");
    Coil.FanPowerPerEvapAirFlowRate(1) = 773.3;
    Coil.FanPowerPerEvapAirFlowRate_2023(1) = 934.4;
    Coil.MinOATCompressor = -10.0;
    Coil.CrankcaseHeaterCapacity = 0.0;
    Coil.MaxOATDefrost = 0.0;
    Coil.DefrostStrategy = StandardRatings::DefrostStrat::Resistive;
    Coil.DefrostControl = StandardRatings::HPdefrostControl::Invalid; // timed defrost control type
    Coil.DefrostTime = 0.058333;
    Coil.DefrostCapacity = 1000;
    Coil.PLRImpact = false;
    Coil.FuelType = "Electricity";
    Coil.RegionNum = 4;
    Coil.OATempCompressorOn = -5.0;
    Coil.OATempCompressorOnOffBlank = "true";
    state->dataCurveManager->NumCurves = 5;
    state->dataCurveManager->PerfCurve.allocate(state->dataCurveManager->NumCurves);
    PerformanceCurveData *pCurve;

    int constexpr nCapfT = 1;
    pCurve = &state->dataCurveManager->PerfCurve(nCapfT);
    pCurve->curveType = CurveType::Cubic;
    pCurve->NumDims = 1;
    pCurve->Name = "PTHPHeatingCAPFT";
    pCurve->Coeff1 = 0.876825;
    pCurve->Coeff2 = -0.002955;
    pCurve->Coeff3 = 5.8e-005;
    pCurve->Coeff4 = 0.025335;
    pCurve->Var1Min = -5;
    pCurve->Var1Max = 25;

    Coil.CCapFTemp(1) = nCapfT;

    int constexpr nCapfFF = 2;
    pCurve = &state->dataCurveManager->PerfCurve(nCapfFF);
    pCurve->curveType = CurveType::Quadratic;
    pCurve->NumDims = 1;
    pCurve->Name = "HPHeatCapfFF";
    pCurve->Coeff1 = 1;
    pCurve->Coeff2 = 0;
    pCurve->Coeff3 = 0;
    pCurve->Var1Min = 0;
    pCurve->Var1Max = 2;
    pCurve->CurveMin = 0;
    pCurve->CurveMax = 2;
    Coil.CCapFFlow(1) = nCapfFF;

    int constexpr nEIRfT = 3;
    pCurve = &state->dataCurveManager->PerfCurve(nEIRfT);
    pCurve->curveType = CurveType::Cubic;
    pCurve->NumDims = 1;
    pCurve->Name = "PTHPHeatingEIRFT";
    pCurve->Coeff1 = 0.704658;
    pCurve->Coeff2 = 0.008767;
    pCurve->Coeff3 = 0.000625;
    pCurve->Coeff4 = -0.009037;
    pCurve->Var1Min = -5;
    pCurve->Var1Max = 25;
    Coil.EIRFTemp(1) = nEIRfT;

    int constexpr nEIRfFF = 4;
    pCurve = &state->dataCurveManager->PerfCurve(nEIRfFF);
    pCurve->curveType = CurveType::Quadratic;
    pCurve->NumDims = 1;
    pCurve->Name = "HPHeatEIRfFF";
    pCurve->Coeff1 = 1;
    pCurve->Coeff2 = 0;
    pCurve->Coeff3 = 0;
    pCurve->Var1Min = 0;
    pCurve->Var1Max = 2;
    pCurve->CurveMin = 0;
    pCurve->CurveMax = 2;
    Coil.EIRFFlow(1) = nEIRfFF;

    int constexpr nPLFfPLR = 5;
    pCurve = &state->dataCurveManager->PerfCurve(nPLFfPLR);
    pCurve->curveType = CurveType::Quadratic;
    pCurve->NumDims = 1;
    pCurve->Name = "HPHeatPLFfPLR";
    pCurve->Coeff1 = 1;
    pCurve->Coeff2 = 0;
    pCurve->Coeff3 = 0;
    pCurve->Var1Min = 0;
    pCurve->Var1Max = 1;
    pCurve->CurveMin = 0.7;
    pCurve->CurveMax = 1;
    Coil.PLFFPLR(1) = nPLFfPLR;

    for (int CurveNum = 1; CurveNum <= state->dataCurveManager->NumCurves; ++CurveNum) {
        PerformanceCurveData &rCurve = state->dataCurveManager->PerfCurve(CurveNum);
        if (rCurve.curveType == CurveType::Cubic) {
            rCurve.ObjectType = "Curve:Cubic";
        } else if (rCurve.curveType == CurveType::Quadratic) {
            rCurve.ObjectType = "Curve:Quadratic";
        }
        rCurve.InterpolationType = InterpType::EvaluateCurveToLimits;
    }
    Real64 NetHeatingCapRatedHighTemp;
    Real64 NetHeatingCapRatedLowTemp;
    Real64 HSPF;
    // for HSPF2 2023
    Real64 NetHeatingCapRatedHighTemp_2023;
    Real64 NetHeatingCapRatedLowTemp_2023;
    Real64 HSPF_2023;
    std::map<std::string, Real64> StandardRatingsResults;
    StandardRatingsResults = SingleSpeedDXHeatingCoilStandardRatings(*state,
                                                                     Coil.RatedTotCap(1),
                                                                     Coil.RatedCOP(1),
                                                                     Coil.CCapFFlow(1),
                                                                     Coil.CCapFTemp(1),
                                                                     Coil.EIRFFlow(1),
                                                                     Coil.EIRFTemp(1),
                                                                     Coil.RatedAirVolFlowRate(1),
                                                                     Coil.FanPowerPerEvapAirFlowRate(1),
                                                                     Coil.FanPowerPerEvapAirFlowRate_2023(1),
                                                                     Coil.RegionNum,
                                                                     Coil.MinOATCompressor,
                                                                     Coil.OATempCompressorOn,
                                                                     Coil.OATempCompressorOnOffBlank,
                                                                     Coil.DefrostControl);

    NetHeatingCapRatedHighTemp = StandardRatingsResults["NetHeatingCapRated"];
    NetHeatingCapRatedLowTemp = StandardRatingsResults["NetHeatingCapH3Test"];
    HSPF = StandardRatingsResults["HSPF"];
    NetHeatingCapRatedHighTemp_2023 = StandardRatingsResults["NetHeatingCapRated_2023"];
    NetHeatingCapRatedLowTemp_2023 = StandardRatingsResults["NetHeatingCapH3Test_2023"];
    HSPF_2023 = StandardRatingsResults["HSPF2_2023"];
    ASSERT_TRUE(HSPF == HSPF_2023); // 0.0 for a negative Curve

    // evaluate capacity curves
    Real64 TotCapTempModFacRated = CurveValue(*state, Coil.CCapFTemp(1), StandardRatings::HeatingOutdoorCoilInletAirDBTempRated);
    Real64 TotCapFlowModFac = CurveValue(*state, Coil.CCapFFlow(1), 1.0);
    Real64 NetHeatingCapRated =
        Coil.RatedTotCap(1) * TotCapTempModFacRated * TotCapFlowModFac + Coil.RatedAirVolFlowRate(1) * Coil.FanPowerPerEvapAirFlowRate(1);
    // check curve values and heating capacity
    EXPECT_GT(TotCapTempModFacRated, 0.0);
    EXPECT_DOUBLE_EQ(TotCapFlowModFac, 1.0);
    EXPECT_DOUBLE_EQ(NetHeatingCapRatedHighTemp, NetHeatingCapRated);
    // evaluate capacity curves
    Real64 CapTempModFacH2Test = CurveValue(*state, Coil.CCapFTemp(1), StandardRatings::HeatingOutdoorCoilInletAirDBTempH2Test);
    EXPECT_GT(CapTempModFacH2Test, 0.0);
    Real64 CapTempModFacH3Test = CurveValue(*state, Coil.CCapFTemp(1), StandardRatings::HeatingOutdoorCoilInletAirDBTempH3Test);
    // if CapTempModFacH3Test curves value is less than zero, NetHeatingCapRatedLowTemp is set to zero
    EXPECT_LT(CapTempModFacH3Test, 0.0);

    // evaluate capacity curves | HSPF2
    Real64 NetHeatingCapRated2023 =
        Coil.RatedTotCap(1) * TotCapTempModFacRated * TotCapFlowModFac + Coil.RatedAirVolFlowRate(1) * Coil.FanPowerPerEvapAirFlowRate_2023(1);
    EXPECT_DOUBLE_EQ(NetHeatingCapRatedHighTemp_2023, NetHeatingCapRated2023);

    // check heating capacity at low temperature
    EXPECT_DOUBLE_EQ(NetHeatingCapRatedLowTemp, 0.0);
    // evaluate EIR curves
    Real64 EIRTempModFacRated = CurveValue(*state, Coil.EIRFTemp(1), StandardRatings::HeatingOutdoorCoilInletAirDBTempRated);
    Real64 EIRTempModFacH2Test = CurveValue(*state, Coil.EIRFTemp(1), StandardRatings::HeatingOutdoorCoilInletAirDBTempH2Test);
    Real64 EIRTempModFacH3Test = CurveValue(*state, Coil.EIRFTemp(1), StandardRatings::HeatingOutdoorCoilInletAirDBTempH3Test);
    // check EIR curve value
    EXPECT_LT(EIRTempModFacRated, 0.0);
    EXPECT_GT(EIRTempModFacH2Test, 0.0);
    EXPECT_GT(EIRTempModFacH3Test, 0.0);
    // if one of the CAP or EIR curves value is less than zero, then HSPF is set to zero
    EXPECT_DOUBLE_EQ(HSPF, 0.0);

    // check heating capacity at low temperature | HSPF2
    EXPECT_DOUBLE_EQ(NetHeatingCapRatedLowTemp_2023, 0.0);
    // if one of the CAP or EIR curves value is less than zero, then HSPF2/HSPF_2023 is set to zero
    EXPECT_DOUBLE_EQ(HSPF_2023, 0.0);
}

TEST_F(EnergyPlusFixture, SingleSpeedHeatingCoilCurveTest_PositiveCurve)
{
    // Test that the standard ratings calculation with positive curve value

    using Psychrometrics::PsyRhoAirFnPbTdbW;
    using StandardRatings::SingleSpeedDXHeatingCoilStandardRatings;

    // Set up heating coil and curves.
    int DXCoilNum;
    state->dataDXCoils->NumDXCoils = 1;
    DXCoilNum = 1;
    state->dataDXCoils->DXCoil.allocate(state->dataDXCoils->NumDXCoils);
    state->dataDXCoils->DXCoilNumericFields.allocate(1);
    state->dataDXCoils->DXCoilOutletTemp.allocate(state->dataDXCoils->NumDXCoils);
    state->dataDXCoils->DXCoilOutletHumRat.allocate(state->dataDXCoils->NumDXCoils);
    state->dataDXCoils->DXCoilFanOpMode.allocate(state->dataDXCoils->NumDXCoils);
    state->dataDXCoils->DXCoilPartLoadRatio.allocate(state->dataDXCoils->NumDXCoils);
    state->dataDXCoils->DXCoilTotalHeating.allocate(state->dataDXCoils->NumDXCoils);
    state->dataDXCoils->DXCoilHeatInletAirDBTemp.allocate(state->dataDXCoils->NumDXCoils);
    state->dataDXCoils->DXCoilHeatInletAirWBTemp.allocate(state->dataDXCoils->NumDXCoils);
    DXCoilData &Coil = state->dataDXCoils->DXCoil(DXCoilNum);

    Coil.Name = "DX Single Speed Heating Coil";
    Coil.DXCoilType = "Coil:Heating:DX:SingleSpeed";
    Coil.DXCoilType_Num = CoilDX_HeatingEmpirical;
    Coil.SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
    Coil.RatedSHR(1) = 1.0;
    Coil.RatedTotCap(1) = 1600.0;
    Coil.RatedCOP(1) = 4.0;
    Coil.RatedEIR(1) = 1 / Coil.RatedCOP(1);
    Coil.RatedAirVolFlowRate(1) = 0.50;
    Coil.RatedAirMassFlowRate(1) =
        Coil.RatedAirVolFlowRate(1) * PsyRhoAirFnPbTdbW(*state, state->dataEnvrn->StdBaroPress, 21.11, 0.00881, "InitDXCoil");
    Coil.FanPowerPerEvapAirFlowRate(1) = 773.3;
    Coil.FanPowerPerEvapAirFlowRate_2023(1) = 934.4;
    Coil.MinOATCompressor = -10.0;
    Coil.CrankcaseHeaterCapacity = 0.0;
    Coil.MaxOATDefrost = 0.0;
    Coil.DefrostStrategy = StandardRatings::DefrostStrat::Resistive;
    Coil.DefrostControl = StandardRatings::HPdefrostControl::Invalid; // timed defrost control type
    Coil.DefrostTime = 0.058333;
    Coil.DefrostCapacity = 1000;
    Coil.PLRImpact = false;
    Coil.FuelType = "Electricity";
    Coil.RegionNum = 4;
    Coil.OATempCompressorOn = -5.0;
    Coil.OATempCompressorOnOffBlank = "true";
    state->dataCurveManager->NumCurves = 5;
    state->dataCurveManager->PerfCurve.allocate(state->dataCurveManager->NumCurves);
    PerformanceCurveData *pCurve;

    int constexpr nCapfT = 1;
    pCurve = &state->dataCurveManager->PerfCurve(nCapfT);
    pCurve->curveType = CurveType::Cubic;
    pCurve->NumDims = 1;
    pCurve->Name = "PTHPHeatingCAPFT";
    pCurve->Coeff1 = 0.876825;
    pCurve->Coeff2 = 0.002955; // previously -ve
    pCurve->Coeff3 = 5.8e-005;
    pCurve->Coeff4 = 0.025335;
    pCurve->Var1Min = 5; // previously -ve
    pCurve->Var1Max = 25;

    Coil.CCapFTemp(1) = nCapfT;

    int constexpr nCapfFF = 2;
    pCurve = &state->dataCurveManager->PerfCurve(nCapfFF);
    pCurve->curveType = CurveType::Quadratic;
    pCurve->NumDims = 1;
    pCurve->Name = "HPHeatCapfFF";
    pCurve->Coeff1 = 1;
    pCurve->Coeff2 = 0;
    pCurve->Coeff3 = 0;
    pCurve->Var1Min = 0;
    pCurve->Var1Max = 2;
    pCurve->CurveMin = 0;
    pCurve->CurveMax = 2;
    Coil.CCapFFlow(1) = nCapfFF;

    int constexpr nEIRfT = 3;
    pCurve = &state->dataCurveManager->PerfCurve(nEIRfT);
    pCurve->curveType = CurveType::Cubic;
    pCurve->NumDims = 1;
    pCurve->Name = "PTHPHeatingEIRFT";
    pCurve->Coeff1 = 0.704658;
    pCurve->Coeff2 = 0.008767;
    pCurve->Coeff3 = 0.000625;
    pCurve->Coeff4 = 0.009037; // previously -ve
    pCurve->Var1Min = 5;       // previously -ve
    pCurve->Var1Max = 25;
    Coil.EIRFTemp(1) = nEIRfT;

    int constexpr nEIRfFF = 4;
    pCurve = &state->dataCurveManager->PerfCurve(nEIRfFF);
    pCurve->curveType = CurveType::Quadratic;
    pCurve->NumDims = 1;
    pCurve->Name = "HPHeatEIRfFF";
    pCurve->Coeff1 = 1;
    pCurve->Coeff2 = 0;
    pCurve->Coeff3 = 0;
    pCurve->Var1Min = 0;
    pCurve->Var1Max = 2;
    pCurve->CurveMin = 0;
    pCurve->CurveMax = 2;
    Coil.EIRFFlow(1) = nEIRfFF;

    int constexpr nPLFfPLR = 5;
    pCurve = &state->dataCurveManager->PerfCurve(nPLFfPLR);
    pCurve->curveType = CurveType::Quadratic;
    pCurve->NumDims = 1;
    pCurve->Name = "HPHeatPLFfPLR";
    pCurve->Coeff1 = 1;
    pCurve->Coeff2 = 0;
    pCurve->Coeff3 = 0;
    pCurve->Var1Min = 0;
    pCurve->Var1Max = 1;
    pCurve->CurveMin = 0.7;
    pCurve->CurveMax = 1;
    Coil.PLFFPLR(1) = nPLFfPLR;

    for (int CurveNum = 1; CurveNum <= state->dataCurveManager->NumCurves; ++CurveNum) {
        PerformanceCurveData &rCurve = state->dataCurveManager->PerfCurve(CurveNum);
        if (rCurve.curveType == CurveType::Cubic) {
            rCurve.ObjectType = "Curve:Cubic";
        } else if (rCurve.curveType == CurveType::Quadratic) {
            rCurve.ObjectType = "Curve:Quadratic";
        }
        rCurve.InterpolationType = InterpType::EvaluateCurveToLimits;
    }
    Real64 NetHeatingCapRatedHighTemp;
    Real64 NetHeatingCapRatedLowTemp;
    Real64 HSPF;
    // for HSPF2 2023
    Real64 NetHeatingCapRatedHighTemp_2023;
    Real64 NetHeatingCapRatedLowTemp_2023;
    Real64 HSPF_2023;
    std::map<std::string, Real64> StandardRatingsResults;
    StandardRatingsResults = SingleSpeedDXHeatingCoilStandardRatings(*state,
                                                                     Coil.RatedTotCap(1),
                                                                     Coil.RatedCOP(1),
                                                                     Coil.CCapFFlow(1),
                                                                     Coil.CCapFTemp(1),
                                                                     Coil.EIRFFlow(1),
                                                                     Coil.EIRFTemp(1),
                                                                     Coil.RatedAirVolFlowRate(1),
                                                                     Coil.FanPowerPerEvapAirFlowRate(1),
                                                                     Coil.FanPowerPerEvapAirFlowRate_2023(1),
                                                                     Coil.RegionNum,
                                                                     Coil.MinOATCompressor,
                                                                     Coil.OATempCompressorOn,
                                                                     Coil.OATempCompressorOnOffBlank,
                                                                     Coil.DefrostControl);

    NetHeatingCapRatedHighTemp = StandardRatingsResults["NetHeatingCapRated"];
    NetHeatingCapRatedLowTemp = StandardRatingsResults["NetHeatingCapH3Test"];
    HSPF = StandardRatingsResults["HSPF"];

    NetHeatingCapRatedHighTemp_2023 = StandardRatingsResults["NetHeatingCapRated_2023"];
    NetHeatingCapRatedLowTemp_2023 = StandardRatingsResults["NetHeatingCapH3Test_2023"];
    HSPF_2023 = StandardRatingsResults["HSPF_2023"];
    // ASSERT_TRUE(HSPF < HSPF_2023);
    //??TODO: further extension of this test
    // evaluate capacity curves
    // Real64 TotCapTempModFacRated = CurveValue(*state, Coil.CCapFTemp(1), StandardRatings::HeatingOutdoorCoilInletAirDBTempRated);
    // Real64 TotCapFlowModFac = CurveValue(*state, Coil.CCapFFlow(1), 1.0);
    // Real64 NetHeatingCapRated =
    //    Coil.RatedTotCap(1) * TotCapTempModFacRated * TotCapFlowModFac + Coil.RatedAirVolFlowRate(1) * Coil.FanPowerPerEvapAirFlowRate(1);
    //// check curve values and heating capacity
    // EXPECT_GT(TotCapTempModFacRated, 0.0);
    // EXPECT_DOUBLE_EQ(TotCapFlowModFac, 1.0);
    // EXPECT_DOUBLE_EQ(NetHeatingCapRatedHighTemp, NetHeatingCapRated);
    //// evaluate capacity curves
    // Real64 CapTempModFacH2Test = CurveValue(*state, Coil.CCapFTemp(1), StandardRatings::HeatingOutdoorCoilInletAirDBTempH2Test);
    // EXPECT_GT(CapTempModFacH2Test, 0.0);
    // Real64 CapTempModFacH3Test = CurveValue(*state, Coil.CCapFTemp(1), StandardRatings::HeatingOutdoorCoilInletAirDBTempH3Test);
    //// if CapTempModFacH3Test curves value is less than zero, NetHeatingCapRatedLowTemp is set to zero
    // EXPECT_LT(CapTempModFacH3Test, 0.0);

    //// check heating capacity at low temperature
    // EXPECT_DOUBLE_EQ(NetHeatingCapRatedLowTemp, 0.0);
    //// evaluate EIR curves
    // Real64 EIRTempModFacRated = CurveValue(*state, Coil.EIRFTemp(1), StandardRatings::HeatingOutdoorCoilInletAirDBTempRated);
    // Real64 EIRTempModFacH2Test = CurveValue(*state, Coil.EIRFTemp(1), StandardRatings::HeatingOutdoorCoilInletAirDBTempH2Test);
    // Real64 EIRTempModFacH3Test = CurveValue(*state, Coil.EIRFTemp(1), StandardRatings::HeatingOutdoorCoilInletAirDBTempH3Test);
    //// check EIR curve value
    // EXPECT_LT(EIRTempModFacRated, 0.0);
    // EXPECT_GT(EIRTempModFacH2Test, 0.0);
    // EXPECT_GT(EIRTempModFacH3Test, 0.0);
    //// if one of the CAP or EIR curves value is less than zero, then HSPF is set to zero
    // EXPECT_DOUBLE_EQ(HSPF, 0.0);
}

TEST_F(EnergyPlusFixture, SingleSpeedHeatingCoilCurveTest2023)
{
    // Test that the standard ratings calculation with negative curve value

    using Psychrometrics::PsyRhoAirFnPbTdbW;
    using StandardRatings::SingleSpeedDXHeatingCoilStandardRatings;

    // Set up heating coil and curves.
    int DXCoilNum;
    state->dataDXCoils->NumDXCoils = 1;
    DXCoilNum = 1;
    state->dataDXCoils->DXCoil.allocate(state->dataDXCoils->NumDXCoils);
    state->dataDXCoils->DXCoilNumericFields.allocate(1);
    state->dataDXCoils->DXCoilOutletTemp.allocate(state->dataDXCoils->NumDXCoils);
    state->dataDXCoils->DXCoilOutletHumRat.allocate(state->dataDXCoils->NumDXCoils);
    state->dataDXCoils->DXCoilFanOpMode.allocate(state->dataDXCoils->NumDXCoils);
    state->dataDXCoils->DXCoilPartLoadRatio.allocate(state->dataDXCoils->NumDXCoils);
    state->dataDXCoils->DXCoilTotalHeating.allocate(state->dataDXCoils->NumDXCoils);
    state->dataDXCoils->DXCoilHeatInletAirDBTemp.allocate(state->dataDXCoils->NumDXCoils);
    state->dataDXCoils->DXCoilHeatInletAirWBTemp.allocate(state->dataDXCoils->NumDXCoils);
    DXCoilData &Coil = state->dataDXCoils->DXCoil(DXCoilNum);

    Coil.Name = "HeatingCoilDXSingleSpeedAutosize";
    Coil.DXCoilType = "Coil:Heating:DX:SingleSpeed";
    Coil.DXCoilType_Num = CoilDX_HeatingEmpirical;
    Coil.SchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
    Coil.RatedSHR(1) = 1.0;
    Coil.RatedTotCap(1) = 1600.0;
    Coil.RatedCOP(1) = 3.8; //
    Coil.RatedEIR(1) = 1 / Coil.RatedCOP(1);
    Coil.RatedAirVolFlowRate(1) = 0.50;
    Coil.RatedAirMassFlowRate(1) =
        Coil.RatedAirVolFlowRate(1) * PsyRhoAirFnPbTdbW(*state, state->dataEnvrn->StdBaroPress, 21.11, 0.00881, "InitDXCoil");
    Coil.FanPowerPerEvapAirFlowRate(1) = 773.3;
    Coil.FanPowerPerEvapAirFlowRate_2023(1) = 934.4;                 //
    Coil.MinOATCompressor = -5;                                      //
    Coil.CrankcaseHeaterCapacity = 200;                              //
    Coil.MaxOATDefrost = 5;                                          //
    Coil.DefrostStrategy = StandardRatings::DefrostStrat::Resistive; //
    Coil.DefrostControl = StandardRatings::HPdefrostControl::Timed;  // timed defrost control type
    Coil.DefrostTime = 0.167;                                        //
    Coil.DefrostCapacity = 20000;                                    //
    Coil.PLRImpact = false;
    Coil.FuelType = "Electricity";
    Coil.RegionNum = 4; //
    Coil.OATempCompressorOn = -5.0;
    Coil.OATempCompressorOnOffBlank = "true";
    state->dataCurveManager->NumCurves = 5;
    state->dataCurveManager->PerfCurve.allocate(state->dataCurveManager->NumCurves);
    PerformanceCurveData *pCurve;

    int constexpr nCapfT = 1;
    pCurve = &state->dataCurveManager->PerfCurve(nCapfT);
    pCurve->curveType = CurveType::Cubic;
    pCurve->NumDims = 1;
    pCurve->Name = "PTHPHeatingCAPFT"; // Simpl_HPACHeatCapFT_Cubic
    pCurve->Coeff1 = 0.759;
    pCurve->Coeff2 = 0.028;
    pCurve->Coeff3 = 0;
    pCurve->Coeff4 = 0;
    pCurve->Var1Min = -20;
    pCurve->Var1Max = 20;

    Coil.CCapFTemp(1) = nCapfT;

    int constexpr nCapfFF = 2;
    pCurve = &state->dataCurveManager->PerfCurve(nCapfFF);
    pCurve->curveType = CurveType::Cubic;
    pCurve->NumDims = 1;
    pCurve->Name = "HPHeatCapfFF"; // Simpl_HPACHeatCapFFF_Cubic
    pCurve->Coeff1 = 0.84;
    pCurve->Coeff2 = 0.16;
    pCurve->Coeff3 = 0;
    pCurve->Coeff4 = 0;
    pCurve->Var1Min = 0.5;
    pCurve->Var1Max = 1.5;
    Coil.CCapFFlow(1) = nCapfFF;

    int constexpr nEIRfT = 3;
    pCurve = &state->dataCurveManager->PerfCurve(nEIRfT);
    pCurve->curveType = CurveType::BiQuadratic;
    pCurve->NumDims = 1;
    pCurve->Name = "PTHPHeatingEIRFT"; // Simpl_HPACEIRFT_Biquadratic
    pCurve->Coeff1 = 0.342;
    pCurve->Coeff2 = 0.035;
    pCurve->Coeff3 = -0.001;
    pCurve->Coeff4 = 0.005;
    pCurve->Coeff5 = 0;
    pCurve->Coeff6 = -0.001;
    pCurve->Var1Min = 12.778;
    pCurve->Var1Max = 23.889;
    pCurve->Var2Min = 18;
    pCurve->Var2Max = 46.111;
    Coil.EIRFTemp(1) = nEIRfT;

    int constexpr nEIRfFF = 4;
    pCurve = &state->dataCurveManager->PerfCurve(nEIRfFF);
    pCurve->curveType = CurveType::Cubic;
    pCurve->NumDims = 1;
    pCurve->Name = "HPHeatEIRfFF"; // Simpl_HPACHeatEIRFT_Cubic
    pCurve->Coeff1 = 1.192;
    pCurve->Coeff2 = -0.03;
    pCurve->Coeff3 = 0.001;
    pCurve->Coeff4 = 0;
    pCurve->Var1Min = -20;
    pCurve->Var1Max = 20;
    pCurve->CurveMin = -20;
    pCurve->CurveMax = 20;
    Coil.EIRFFlow(1) = nEIRfFF;

    int constexpr nPLFfPLR = 5;
    pCurve = &state->dataCurveManager->PerfCurve(nPLFfPLR);
    pCurve->curveType = CurveType::Quadratic;
    pCurve->NumDims = 1;
    pCurve->Name = "HPHeatPLFfPLR"; // Simpl_HPACCOOLPLFFPLR_Quadratic
    pCurve->Coeff1 = 0.75;
    pCurve->Coeff2 = 0.25;
    pCurve->Coeff3 = 0;
    pCurve->Var1Min = 0;
    pCurve->Var1Max = 1;
    pCurve->CurveMin = 0;
    pCurve->CurveMax = 1;
    Coil.PLFFPLR(1) = nPLFfPLR;

    for (int CurveNum = 1; CurveNum <= state->dataCurveManager->NumCurves; ++CurveNum) {
        PerformanceCurveData &rCurve = state->dataCurveManager->PerfCurve(CurveNum);
        if (rCurve.curveType == CurveType::Cubic) {
            rCurve.ObjectType = "Curve:Cubic";
        } else if (rCurve.curveType == CurveType::Quadratic) {
            rCurve.ObjectType = "Curve:Quadratic";
        }
        rCurve.InterpolationType = InterpType::EvaluateCurveToLimits;
    }
    Real64 NetHeatingCapRatedHighTemp;
    Real64 NetHeatingCapRatedLowTemp;
    Real64 HSPF;
    // for HSPF2 2023
    Real64 NetHeatingCapRatedHighTemp_2023;
    Real64 NetHeatingCapRatedLowTemp_2023;
    Real64 HSPF_2023;
    std::map<std::string, Real64> StandardRatingsResults;
    StandardRatingsResults = SingleSpeedDXHeatingCoilStandardRatings(*state,
                                                                     Coil.RatedTotCap(1),
                                                                     Coil.RatedCOP(1),
                                                                     Coil.CCapFFlow(1),
                                                                     Coil.CCapFTemp(1),
                                                                     Coil.EIRFFlow(1),
                                                                     Coil.EIRFTemp(1),
                                                                     Coil.RatedAirVolFlowRate(1),
                                                                     Coil.FanPowerPerEvapAirFlowRate(1),
                                                                     Coil.FanPowerPerEvapAirFlowRate_2023(1),
                                                                     Coil.RegionNum,
                                                                     Coil.MinOATCompressor,
                                                                     Coil.OATempCompressorOn,
                                                                     Coil.OATempCompressorOnOffBlank,
                                                                     Coil.DefrostControl);

    NetHeatingCapRatedHighTemp = StandardRatingsResults["NetHeatingCapRated"];
    NetHeatingCapRatedLowTemp = StandardRatingsResults["NetHeatingCapH3Test"];
    HSPF = StandardRatingsResults["HSPF"];

    NetHeatingCapRatedHighTemp_2023 = StandardRatingsResults["NetHeatingCapRated_2023"];
    NetHeatingCapRatedLowTemp_2023 = StandardRatingsResults["NetHeatingCapH3Test_2023"];
    HSPF_2023 = StandardRatingsResults["HSPF2_2023"];
    // ASSERT_TRUE(HSPF < HSPF_2023);
    //??TODO: further extension of this test
    //// evaluate capacity curves
    // Real64 TotCapTempModFacRated = CurveValue(*state, Coil.CCapFTemp(1), StandardRatings::HeatingOutdoorCoilInletAirDBTempRated);
    // Real64 TotCapFlowModFac = CurveValue(*state, Coil.CCapFFlow(1), 1.0);
    // Real64 NetHeatingCapRated =
    //    Coil.RatedTotCap(1) * TotCapTempModFacRated * TotCapFlowModFac + Coil.RatedAirVolFlowRate(1) * Coil.FanPowerPerEvapAirFlowRate(1);
    //// check curve values and heating capacity
    // EXPECT_GT(TotCapTempModFacRated, 0.0);
    // EXPECT_DOUBLE_EQ(TotCapFlowModFac, 1.0);
    // EXPECT_DOUBLE_EQ(NetHeatingCapRatedHighTemp, NetHeatingCapRated);
    //// evaluate capacity curves
    // Real64 CapTempModFacH2Test = CurveValue(*state, Coil.CCapFTemp(1), StandardRatings::HeatingOutdoorCoilInletAirDBTempH2Test);
    // EXPECT_GT(CapTempModFacH2Test, 0.0);
    // Real64 CapTempModFacH3Test = CurveValue(*state, Coil.CCapFTemp(1), StandardRatings::HeatingOutdoorCoilInletAirDBTempH3Test);
    //// if CapTempModFacH3Test curves value is less than zero, NetHeatingCapRatedLowTemp is set to zero
    // EXPECT_LT(CapTempModFacH3Test, 0.0);

    //// check heating capacity at low temperature
    // EXPECT_DOUBLE_EQ(NetHeatingCapRatedLowTemp, 0.0);
    //// evaluate EIR curves
    // Real64 EIRTempModFacRated = CurveValue(*state, Coil.EIRFTemp(1), StandardRatings::HeatingOutdoorCoilInletAirDBTempRated);
    // Real64 EIRTempModFacH2Test = CurveValue(*state, Coil.EIRFTemp(1), StandardRatings::HeatingOutdoorCoilInletAirDBTempH2Test);
    // Real64 EIRTempModFacH3Test = CurveValue(*state, Coil.EIRFTemp(1), StandardRatings::HeatingOutdoorCoilInletAirDBTempH3Test);
    //// check EIR curve value
    // EXPECT_LT(EIRTempModFacRated, 0.0);
    // EXPECT_GT(EIRTempModFacH2Test, 0.0);
    // EXPECT_GT(EIRTempModFacH3Test, 0.0);
    //// if one of the CAP or EIR curves value is less than zero, then HSPF is set to zero
    // EXPECT_DOUBLE_EQ(HSPF, 0.0);
}

TEST_F(EnergyPlusFixture, SingleSpeedHeatingCurveTest2023_II)
{
    std::string const idf_objects1 = delimited_string({
        "Coil:Heating:DX:SingleSpeed,",
        "  Heat Pump 1 HP Heating Coil,                             !- Name",
        "  ,                                                        !- Availability Schedule Name",
        "  9000.0,                                                  !- Rated Total Heating Capacity {W}",
        "  2.75,                                                    !- Rated COP",
        "  0.5,                                                     !- Rated Air Flow Rate {m3/s}",
        "  773.3,                                                   !- 2017 Rated Supply Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "  934.4,                                                   !- 2023 Rated Supply Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "  Heat Pump 1 Cooling Coil Outlet,                         !- Air Inlet Node Name",
        "  Heat Pump 1 Heating Coil Outlet,                         !- Air Outlet Node Name",
        "  Heat Pump 1 HP Heating Coil Cap-FT,                      !- Total Heating Capacity Function of Temperature Curve Name",
        "  Heat Pump 1 HP Heating Coil Cap-FF,                      !- Total Heating Capacity Function of Flow Fraction Curve Name",
        "  Heat Pump 1 HP Heating Coil EIR-FT,                      !- Energy Input Ratio Function of Temperature Curve Name",
        "  Heat Pump 1 HP Heating Coil EIR-FF,                      !- Energy Input Ratio Function of Flow Fraction Curve Name",
        "  Heat Pump 1 HP Heating Coil PLF,                         !- Part Load Fraction Correlation Curve Name",
        "  Heat Pump 1 HP Heating Coil DefrEIR-FT,                  !- Defrost Energy Input Ratio Function of Temperature Curve Name",
        "  -8,                                                      !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
        "  ,                                                        !- Outdoor Dry-Bulb Temperature to Turn On Compressor",
        "  5,                                                       !- Maximum Outdoor Dry-Bulb Temperature for Defrost Operation {C}",
        "  0,                                                       !- Crankcase Heater Capacity {W}",
        "  0,                                                       !- Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation {C}",
        "  ReverseCycle,                                            !- Defrost Strategy",
        "  Timed,                                                   !- Defrost Control",
        "  0.058333,                                                !- Defrost Time Period Fraction",
        "  10.0;                                                    !- Resistive Defrost Heater Capacity {W}",

        "Curve:Cubic,",
        "  Heat Pump 1 HP Heating Coil Cap-FT,                      !- Name",
        "  0.758746,                                                !- Coefficient1 Constant",
        "  0.027626,                                                !- Coefficient2 x",
        "  0.000148716,                                             !- Coefficient3 x**2",
        "  0.0000034992,                                            !- Coefficient4 x**3",
        "  -20.0,                                                   !- Minimum Value of x",
        "  20.0;                                                    !- Maximum Value of x",

        "Curve:Cubic,",
        "  Heat Pump 1 HP Heating Coil Cap-FF,                      !- Name",
        "  0.84,                                                    !- Coefficient1 Constant",
        "  0.16,                                                    !- Coefficient2 x",
        "  0.0,                                                     !- Coefficient3 x**2",
        "  0.0,                                                     !- Coefficient4 x**3",
        "  0.5,                                                     !- Minimum Value of x",
        "  1.5;                                                     !- Maximum Value of x",

        "Curve:Cubic,",
        "  Heat Pump 1 HP Heating Coil EIR-FT,                      !- Name",
        "  1.19248,                                                 !- Coefficient1 Constant",
        "  -0.0300438,                                              !- Coefficient2 x",
        "  0.00103745,                                              !- Coefficient3 x**2",
        "  -0.000023328,                                            !- Coefficient4 x**3",
        "  -20.0,                                                   !- Minimum Value of x",
        "  20.0;                                                    !- Maximum Value of x",

        "Curve:Quadratic,",
        "  Heat Pump 1 HP Heating Coil EIR-FF,                      !- Name",
        "  1.3824,                                                  !- Coefficient1 Constant",
        "  -0.4336,                                                 !- Coefficient2 x",
        "  0.0512,                                                  !- Coefficient3 x**2",
        "  0.0,                                                     !- Minimum Value of x",
        "  1.0;                                                     !- Maximum Value of x",

        "Curve:Quadratic,",
        "  Heat Pump 1 HP Heating Coil PLF,                         !- Name",
        "  0.75,                                                    !- Coefficient1 Constant",
        "  0.25,                                                    !- Coefficient2 x",
        "  0.0,                                                     !- Coefficient3 x**2",
        "  0.0,                                                     !- Minimum Value of x",
        "  1.0;                                                     !- Maximum Value of x",

        "Curve:Biquadratic,",
        "  Heat Pump 1 HP Heating Coil DefrEIR-FT,                  !- Name",
        "  1,                                                       !- Coefficient1 Constant",
        "  0,                                                       !- Coefficient2 x",
        "  0,                                                       !- Coefficient3 x**2",
        "  0,                                                       !- Coefficient4 y",
        "  0,                                                       !- Coefficient5 y**2",
        "  0,                                                       !- Coefficient6 x*y",
        "  0,                                                       !- Minimum Value of x",
        "  50,                                                      !- Maximum Value of x",
        "  0,                                                       !- Minimum Value of y",
        "  50;                                                      !- Maximum Value of y",
    });

    ASSERT_TRUE(process_idf(idf_objects1));

    GetDXCoils(*state);

    auto &Coil(state->dataDXCoils->DXCoil(1));

    Real64 NetHeatingCapRatedHighTemp;
    Real64 NetHeatingCapRatedLowTemp;
    Real64 HSPF;
    // for HSPF2 2023
    Real64 NetHeatingCapRatedHighTemp_2023;
    Real64 NetHeatingCapRatedLowTemp_2023;
    Real64 HSPF_2023;
    std::map<std::string, Real64> StandardRatingsResults;
    StandardRatingsResults = SingleSpeedDXHeatingCoilStandardRatings(*state,
                                                                     Coil.RatedTotCap(1),
                                                                     Coil.RatedCOP(1),
                                                                     Coil.CCapFFlow(1),
                                                                     Coil.CCapFTemp(1),
                                                                     Coil.EIRFFlow(1),
                                                                     Coil.EIRFTemp(1),
                                                                     Coil.RatedAirVolFlowRate(1),
                                                                     Coil.FanPowerPerEvapAirFlowRate(1),
                                                                     Coil.FanPowerPerEvapAirFlowRate_2023(1),
                                                                     Coil.RegionNum,
                                                                     Coil.MinOATCompressor,
                                                                     Coil.OATempCompressorOn,
                                                                     Coil.OATempCompressorOnOffBlank,
                                                                     Coil.DefrostControl);

    NetHeatingCapRatedHighTemp = StandardRatingsResults["NetHeatingCapRated"];
    NetHeatingCapRatedLowTemp = StandardRatingsResults["NetHeatingCapH3Test"];
    HSPF = StandardRatingsResults["HSPF"];

    NetHeatingCapRatedHighTemp_2023 = StandardRatingsResults["NetHeatingCapRated_2023"];
    NetHeatingCapRatedLowTemp_2023 = StandardRatingsResults["NetHeatingCapH3Test_2023"];
    HSPF_2023 = StandardRatingsResults["HSPF2_2023"];
}

TEST_F(EnergyPlusFixture, MultiSpeedHeatingCoil_HSPFValueTest_2Speed)
{

    std::string const idf_objects1 = delimited_string({
        " Coil:Heating:DX:MultiSpeed,",
        "   ashp htg coil,                          !- Name",
        "   ,                                       !- Availability Schedule Name",
        "   ashp unitary system Cooling Coil - Heating Coil Node, !- Air Inlet Node Name",
        "   ashp unitary system Heating Coil - Supplemental Coil Node, !- Air Outlet Node Name",
        "   -17.7777777777778,                      !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
        "   ,                                       !- Outdoor Dry-Bulb Temperature to Turn On Compressor {C}",
        "   50,                                     !- Crankcase Heater Capacity {W}",
        "   10,                                     !- Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation {C}",
        "   DefrostEIR,                             !- Defrost Energy Input Ratio Function of Temperature Curve Name",
        "   4.44444444444444,                       !- Maximum Outdoor Dry-Bulb Temperature for Defrost Operation {C}",
        "   ReverseCycle,                           !- Defrost Strategy",
        "   OnDemand,                               !- Defrost Control",
        "   0.058333,                               !- Defrost Time Period Fraction",
        "   AutoSize,                               !- Resistive Defrost Heater Capacity {W}",
        "   No,                                     !- Apply Part Load Fraction to Speeds Greater than 1",
        "   Electricity,                            !- Fuel Type",
        "   4,                                      !- Region number for Calculating HSPF",
        "   2,                                      !- Number of Speeds",
        "   10128.5361851424,                       !- Speed Gross Rated Heating Capacity 1 {W}",
        "   4.4518131589158,                        !- Speed Gross Rated Heating COP 1 {W/W}",
        "   0.531903646383625,                      !- Speed Rated Air Flow Rate 1 {m3/s}",
        "   773.3,                                  !- 2017 Speed 1 Rated Supply Air Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "   934.3,                                  !- 2023 Speed 1 Rated Supply Air Fan Power Per Volume Flow Rate {W/(m3/s)}", //??
        "   HP_Heat-Cap-fT1,                        !- Speed Heating Capacity Function of Temperature Curve Name 1",
        "   HP_Heat-CAP-fFF1,                       !- Speed Heating Capacity Function of Flow Fraction Curve Name 1",
        "   HP_Heat-EIR-fT1,                        !- Speed Energy Input Ratio Function of Temperature Curve Name 1",
        "   HP_Heat-EIR-fFF1,                       !- Speed Energy Input Ratio Function of Flow Fraction Curve Name 1",
        "   HP_Heat-PLF-fPLR1,                      !- Speed Part Load Fraction Correlation Curve Name 1",
        "   0.2,                                    !- Speed Rated Waste Heat Fraction of Power Input 1 {dimensionless}",
        "   ConstantBiquadratic,                    !- Speed Waste Heat Function of Temperature Curve Name 1",
        "   14067.4113682534,                       !- Speed Gross Rated Heating Capacity 2 {W}",
        "   3.9871749697327,                        !- Speed Gross Rated Heating COP 2 {W/W}",
        "   0.664879557979531,                      !- Speed Rated Air Flow Rate 2 {m3/s}",
        "   773.3,                                  !- 2017 Speed 2 Rated Supply Air Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "   934.3,                                  !- 2023 Speed 2 Rated Supply Air Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "   HP_Heat-Cap-fT2,                        !- Speed Heating Capacity Function of Temperature Curve Name 2",
        "   HP_Heat-CAP-fFF2,                       !- Speed Heating Capacity Function of Flow Fraction Curve Name 2",
        "   HP_Heat-EIR-fT2,                        !- Speed Energy Input Ratio Function of Temperature Curve Name 2",
        "   HP_Heat-EIR-fFF2,                       !- Speed Energy Input Ratio Function of Flow Fraction Curve Name 2",
        "   HP_Heat-PLF-fPLR2,                      !- Speed Part Load Fraction Correlation Curve Name 2",
        "   0.2,                                    !- Speed Rated Waste Heat Fraction of Power Input 2 {dimensionless}",
        "   ConstantBiquadratic;                    !- Speed Waste Heat Function of Temperature Curve Name 2",

        " Curve:Biquadratic,",
        "   DefrostEIR,                             !- Name",
        "   0.1528,                                 !- Coefficient1 Constant",
        "   0,                                      !- Coefficient2 x",
        "   0,                                      !- Coefficient3 x**2",
        "   0,                                      !- Coefficient4 y",
        "   0,                                      !- Coefficient5 y**2",
        "   0,                                      !- Coefficient6 x*y",
        "   -100,                                   !- Minimum Value of x {BasedOnField A2}",
        "   100,                                    !- Maximum Value of x {BasedOnField A2}",
        "   -100,                                   !- Minimum Value of y {BasedOnField A3}",
        "   100;                                    !- Maximum Value of y {BasedOnField A3}",

        " Curve:Biquadratic,",
        "   HP_Heat-Cap-fT1,                        !- Name",
        "   0.84077409,                             !- Coefficient1 Constant",
        "   -0.0014336586,                          !- Coefficient2 x",
        "   -0.000150336,                           !- Coefficient3 x**2",
        "   0.029628603,                            !- Coefficient4 y",
        "   0.000161676,                            !- Coefficient5 y**2",
        "   -2.349e-005,                            !- Coefficient6 x*y",
        "   -100,                                   !- Minimum Value of x {BasedOnField A2}",
        "   100,                                    !- Maximum Value of x {BasedOnField A2}",
        "   -100,                                   !- Minimum Value of y {BasedOnField A3}",
        "   100;                                    !- Maximum Value of y {BasedOnField A3}",

        " Curve:Quadratic,",
        "   HP_Heat-CAP-fFF1,                       !- Name",
        "   0.741466907,                            !- Coefficient1 Constant",
        "   0.378645444,                            !- Coefficient2 x",
        "   -0.119754733,                           !- Coefficient3 x**2",
        "   0,                                      !- Minimum Value of x {BasedOnField A2}",
        "   2,                                      !- Maximum Value of x {BasedOnField A2}",
        "   0,                                      !- Minimum Curve Output {BasedOnField A3}",
        "   2;                                      !- Maximum Curve Output {BasedOnField A3}",

        " Curve:Biquadratic,",
        "   HP_Heat-EIR-fT1,                        !- Name",
        "   0.539472334,                            !- Coefficient1 Constant",
        "   0.0165103146,                           !- Coefficient2 x",
        "   0.00083874528,                          !- Coefficient3 x**2",
        "   -0.00403234020000001,                   !- Coefficient4 y",
        "   0.00142404156,                          !- Coefficient5 y**2",
        "   -0.00211806252,                         !- Coefficient6 x*y",
        "   -100,                                   !- Minimum Value of x {BasedOnField A2}",
        "   100,                                    !- Maximum Value of x {BasedOnField A2}",
        "   -100,                                   !- Minimum Value of y {BasedOnField A3}",
        "   100;                                    !- Maximum Value of y {BasedOnField A3}",

        " Curve:Quadratic,",
        "   HP_Heat-EIR-fFF1,                       !- Name",
        "   2.153618211,                            !- Coefficient1 Constant",
        "   -1.737190609,                           !- Coefficient2 x",
        "   0.584269478,                            !- Coefficient3 x**2",
        "   0,                                      !- Minimum Value of x {BasedOnField A2}",
        "   2,                                      !- Maximum Value of x {BasedOnField A2}",
        "   0,                                      !- Minimum Curve Output {BasedOnField A3}",
        "   2;                                      !- Maximum Curve Output {BasedOnField A3}",

        " Curve:Quadratic,",
        "   HP_Heat-PLF-fPLR1,                      !- Name",
        "   0.89,                                   !- Coefficient1 Constant",
        "   0.11,                                   !- Coefficient2 x",
        "   0,                                      !- Coefficient3 x**2",
        "   0,                                      !- Minimum Value of x {BasedOnField A2}",
        "   1,                                      !- Maximum Value of x {BasedOnField A2}",
        "   0.7,                                    !- Minimum Curve Output {BasedOnField A3}",
        "   1;                                      !- Maximum Curve Output {BasedOnField A3}",

        " Curve:Biquadratic,",
        "   ConstantBiquadratic,                    !- Name",
        "   1,                                      !- Coefficient1 Constant",
        "   0,                                      !- Coefficient2 x",
        "   0,                                      !- Coefficient3 x**2",
        "   0,                                      !- Coefficient4 y",
        "   0,                                      !- Coefficient5 y**2",
        "   0,                                      !- Coefficient6 x*y",
        "   -100,                                   !- Minimum Value of x {BasedOnField A2}",
        "   100,                                    !- Maximum Value of x {BasedOnField A2}",
        "   -100,                                   !- Minimum Value of y {BasedOnField A3}",
        "   100;                                    !- Maximum Value of y {BasedOnField A3}",

        " Curve:Biquadratic,",
        "   HP_Heat-Cap-fT2,                        !- Name",
        "   0.831506971,                            !- Coefficient1 Constant",
        "   0.0018392166,                           !- Coefficient2 x",
        "   -0.000187596,                           !- Coefficient3 x**2",
        "   0.0266002056,                           !- Coefficient4 y",
        "   0.000191484,                            !- Coefficient5 y**2",
        "   -6.5772e-005,                           !- Coefficient6 x*y",
        "   -100,                                   !- Minimum Value of x {BasedOnField A2}",
        "   100,                                    !- Maximum Value of x {BasedOnField A2}",
        "   -100,                                   !- Minimum Value of y {BasedOnField A3}",
        "   100;                                    !- Maximum Value of y {BasedOnField A3}",

        " Curve:Quadratic,",
        "   HP_Heat-CAP-fFF2,                       !- Name",
        "   0.76634609,                             !- Coefficient1 Constant",
        "   0.32840943,                             !- Coefficient2 x",
        "   -0.094701495,                           !- Coefficient3 x**2",
        "   0,                                      !- Minimum Value of x {BasedOnField A2}",
        "   2,                                      !- Maximum Value of x {BasedOnField A2}",
        "   0,                                      !- Minimum Curve Output {BasedOnField A3}",
        "   2;                                      !- Maximum Curve Output {BasedOnField A3}",

        " Curve:Biquadratic,",
        "   HP_Heat-EIR-fT2,                        !- Name",
        "   0.787746797,                            !- Coefficient1 Constant",
        "   -0.000652314599999999,                  !- Coefficient2 x",
        "   0.00078866784,                          !- Coefficient3 x**2",
        "   -0.0023209056,                          !- Coefficient4 y",
        "   0.00074760408,                          !- Coefficient5 y**2",
        "   -0.00109173096,                         !- Coefficient6 x*y",
        "   -100,                                   !- Minimum Value of x {BasedOnField A2}",
        "   100,                                    !- Maximum Value of x {BasedOnField A2}",
        "   -100,                                   !- Minimum Value of y {BasedOnField A3}",
        "   100;                                    !- Maximum Value of y {BasedOnField A3}",

        " Curve:Quadratic,",
        "   HP_Heat-EIR-fFF2,                       !- Name",
        "   2.001041353,                            !- Coefficient1 Constant",
        "   -1.58869128,                            !- Coefficient2 x",
        "   0.587593517,                            !- Coefficient3 x**2",
        "   0,                                      !- Minimum Value of x {BasedOnField A2}",
        "   2,                                      !- Maximum Value of x {BasedOnField A2}",
        "   0,                                      !- Minimum Curve Output {BasedOnField A3}",
        "   2;                                      !- Maximum Curve Output {BasedOnField A3}",

        " Curve:Quadratic,",
        "   HP_Heat-PLF-fPLR2,                      !- Name",
        "   0.89,                                   !- Coefficient1 Constant",
        "   0.11,                                   !- Coefficient2 x",
        "   0,                                      !- Coefficient3 x**2",
        "   0,                                      !- Minimum Value of x {BasedOnField A2}",
        "   1,                                      !- Maximum Value of x {BasedOnField A2}",
        "   0.7,                                    !- Minimum Curve Output {BasedOnField A3}",
        "   1;                                      !- Maximum Curve Output {BasedOnField A3}",
    });

    ASSERT_TRUE(process_idf(idf_objects1));

    GetDXCoils(*state);

    auto &Coil(state->dataDXCoils->DXCoil(1));

    std::map<std::string, Real64> StandardRatingsResult;

    Real64 NetHeatingCapRatedHighTemp(0.0);
    Real64 NetHeatingCapRatedLowTemp(0.0);
    Real64 HSPF(0.0);

    Real64 NetHeatingCapRatedHighTemp_2023(0.0);
    Real64 NetHeatingCapRatedLowTemp_2023(0.0);
    Real64 HSPF2_2023(0.0);

    StandardRatingsResult = MultiSpeedDXHeatingCoilStandardRatings(*state,
                                                                   Coil.Name,          // DXCoilName
                                                                   Coil.DXCoilType,    // DXCoilType,
                                                                   Coil.MSCCapFTemp,   // CapFTempCurveIndex,
                                                                   Coil.MSCCapFFlow,   // CapFFlowCurveIndex,
                                                                   Coil.MSEIRFTemp,    // EIRFTempCurveIndex,
                                                                   Coil.MSEIRFFlow,    // EIRFFlowCurveIndex,
                                                                   Coil.MSPLFFPLR,     // PLFFPLRCurveIndex,
                                                                   Coil.MSRatedTotCap, // RatedTotalCapacity,
                                                                   Coil.MSRatedCOP,
                                                                   Coil.MSRatedAirVolFlowRate,
                                                                   Coil.MSFanPowerPerEvapAirFlowRate,      // FanPowerPerEvapAirFlowRateFromInput,
                                                                   Coil.MSFanPowerPerEvapAirFlowRate_2023, // FanPowerPerEvapAirFlowRateFromInput,
                                                                   Coil.NumOfSpeeds,
                                                                   Coil.RegionNum,
                                                                   Coil.MinOATCompressor,
                                                                   Coil.OATempCompressorOn,
                                                                   Coil.OATempCompressorOnOffBlank,
                                                                   Coil.DefrostControl);

    NetHeatingCapRatedHighTemp = StandardRatingsResult["NetHeatingCapRatedHighTemp"];
    NetHeatingCapRatedLowTemp = StandardRatingsResult["NetHeatingCapRatedLowTemp"];
    HSPF = StandardRatingsResult["HSPF"];

    NetHeatingCapRatedHighTemp_2023 = StandardRatingsResult["NetHeatingCapRatedHighTemp_2023"];
    NetHeatingCapRatedLowTemp_2023 = StandardRatingsResult["NetHeatingCapRatedLowTemp_2023"];
    HSPF2_2023 = StandardRatingsResult["HSPF2_2023"];

    ASSERT_TRUE(HSPF != 0.0);
    ASSERT_TRUE(NetHeatingCapRatedHighTemp != 0.0);
    ASSERT_TRUE(NetHeatingCapRatedLowTemp != 0.0);
    EXPECT_NEAR(1.8604449198065671, HSPF, 0.01);                       // 1.8378611856963720 at 934.3 W/(m3/s)
    EXPECT_NEAR(14723.494682539813, NetHeatingCapRatedHighTemp, 0.01); // 14830.540291374517 at 934.3 W/(m3/s)
    EXPECT_NEAR(8814.4702147982516, NetHeatingCapRatedLowTemp, 0.01);  // 8921.5158236329553 at 934.3 W/(m3/s)
    EXPECT_NEAR(6.35, HSPF * StandardRatings::ConvFromSIToIP, 0.01);   // //6.3481015667753340 at 773.3 W/(m3/s)

    ASSERT_TRUE(HSPF2_2023 != 0.0);                      // 1.6064633535795425
    ASSERT_TRUE(NetHeatingCapRatedHighTemp_2023 != 0.0); // 14830.540291374517
    ASSERT_TRUE(NetHeatingCapRatedLowTemp_2023 != 0.0);  // 6974.9677431654618

    ASSERT_TRUE(HSPF != HSPF2_2023);
}

TEST_F(EnergyPlusFixture, ChillerIPLVTestAirCooled)
{

    using StandardRatings::CalcChillerIPLV;

    // Setup an air-cooled Chiller:Electric:EIR chiller
    state->dataChillerElectricEIR->ElectricEIRChiller.allocate(1);
    state->dataChillerElectricEIR->ElectricEIRChiller(1).Name = "Air Cooled Chiller";
    state->dataChillerElectricEIR->ElectricEIRChiller(1).RefCap = 216000;           // W
    state->dataChillerElectricEIR->ElectricEIRChiller(1).RefCOP = 2.81673861898309; // W/W
    state->dataChillerElectricEIR->ElectricEIRChiller(1).CondenserType = DataPlant::CondenserType::AirCooled;
    state->dataChillerElectricEIR->ElectricEIRChiller(1).MinUnloadRat = 0.15;

    int CurveNum;
    state->dataCurveManager->NumCurves = 3;
    state->dataCurveManager->PerfCurve.allocate(state->dataCurveManager->NumCurves);

    // Cap=f(T)
    CurveNum = 1;
    state->dataCurveManager->PerfCurve(CurveNum).curveType = CurveType::BiQuadratic;
    state->dataCurveManager->PerfCurve(CurveNum).NumDims = 2;
    state->dataCurveManager->PerfCurve(CurveNum).ObjectType = "Curve:BiQuadratic";
    state->dataCurveManager->PerfCurve(CurveNum).InterpolationType = InterpType::EvaluateCurveToLimits;
    state->dataCurveManager->PerfCurve(CurveNum).Name = "AirCooledChillerScrewCmpCapfT";
    state->dataCurveManager->PerfCurve(CurveNum).Coeff1 = 0.98898813;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff2 = 0.036832851;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff3 = 0.000174006;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff4 = -0.000275634;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff5 = -0.000143667;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff6 = -0.000246286;
    state->dataCurveManager->PerfCurve(CurveNum).Var1Min = 4.44;
    state->dataCurveManager->PerfCurve(CurveNum).Var1Max = 10;
    state->dataCurveManager->PerfCurve(CurveNum).Var2Min = 23.89;
    state->dataCurveManager->PerfCurve(CurveNum).Var2Max = 46.11;
    state->dataChillerElectricEIR->ElectricEIRChiller(1).ChillerCapFTIndex = 1;

    // EIR=f(T)
    CurveNum = 2;
    state->dataCurveManager->PerfCurve(CurveNum).curveType = CurveType::BiQuadratic;
    state->dataCurveManager->PerfCurve(CurveNum).NumDims = 2;
    state->dataCurveManager->PerfCurve(CurveNum).ObjectType = "Curve:BiQuadratic";
    state->dataCurveManager->PerfCurve(CurveNum).InterpolationType = InterpType::EvaluateCurveToLimits;
    state->dataCurveManager->PerfCurve(CurveNum).Name = "AirCooledChillerScrewCmpEIRfT";
    state->dataCurveManager->PerfCurve(CurveNum).Coeff1 = 0.814058418;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff2 = 0.002335553;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff3 = 0.000817786;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff4 = -0.017129784;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff5 = 0.000773288;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff6 = -0.000922024;
    state->dataCurveManager->PerfCurve(CurveNum).Var1Min = 4.44;
    state->dataCurveManager->PerfCurve(CurveNum).Var1Max = 10;
    state->dataCurveManager->PerfCurve(CurveNum).Var2Min = 10;
    state->dataCurveManager->PerfCurve(CurveNum).Var2Max = 46.11;
    state->dataChillerElectricEIR->ElectricEIRChiller(1).ChillerEIRFTIndex = 2;

    // EIR=f(PLR)
    CurveNum = 3;
    state->dataCurveManager->PerfCurve(CurveNum).curveType = CurveType::Cubic;
    state->dataCurveManager->PerfCurve(CurveNum).NumDims = 1;
    state->dataCurveManager->PerfCurve(CurveNum).ObjectType = "Curve:Cubic";
    state->dataCurveManager->PerfCurve(CurveNum).InterpolationType = InterpType::EvaluateCurveToLimits;
    state->dataCurveManager->PerfCurve(CurveNum).Name = "AirCooledChillerScrewCmpEIRfPLR";
    state->dataCurveManager->PerfCurve(CurveNum).Coeff1 = -0.08117804;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff2 = 1.433532026;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff3 = -0.762289434;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff4 = 0.412199944;
    state->dataCurveManager->PerfCurve(CurveNum).Var1Min = 0;
    state->dataCurveManager->PerfCurve(CurveNum).Var1Max = 1;
    state->dataChillerElectricEIR->ElectricEIRChiller(1).ChillerEIRFPLRIndex = 3;

    Real64 IPLV;
    CalcChillerIPLV(*state,
                    state->dataChillerElectricEIR->ElectricEIRChiller(1).Name,
                    DataPlant::PlantEquipmentType::Chiller_ElectricEIR,
                    state->dataChillerElectricEIR->ElectricEIRChiller(1).RefCap,
                    state->dataChillerElectricEIR->ElectricEIRChiller(1).RefCOP,
                    state->dataChillerElectricEIR->ElectricEIRChiller(1).CondenserType,
                    state->dataChillerElectricEIR->ElectricEIRChiller(1).ChillerCapFTIndex,
                    state->dataChillerElectricEIR->ElectricEIRChiller(1).ChillerEIRFTIndex,
                    state->dataChillerElectricEIR->ElectricEIRChiller(1).ChillerEIRFPLRIndex,
                    state->dataChillerElectricEIR->ElectricEIRChiller(1).MinUnloadRat,
                    IPLV,
                    Optional<const Real64>(),
                    ObjexxFCL::Optional_int_const(),
                    Optional<const Real64>());

    EXPECT_DOUBLE_EQ(round(IPLV * 100) / 100, 3.87); // 13.20 IPLV
}

TEST_F(EnergyPlusFixture, ChillerIPLVTestWaterCooled)
{

    using StandardRatings::CalcChillerIPLV;

    // Setup a water-cooled Chiller:Electric:EIR chiller with reference conditions being at non-rated conditions
    state->dataChillerElectricEIR->ElectricEIRChiller.allocate(1);
    state->dataChillerElectricEIR->ElectricEIRChiller(1).Name = "ElectricEIRChiller McQuay WSC 471kW/5.89COP/Vanes";
    state->dataChillerElectricEIR->ElectricEIRChiller(1).RefCap = 471200; // W
    state->dataChillerElectricEIR->ElectricEIRChiller(1).RefCOP = 5.89;   // W/W
    state->dataChillerElectricEIR->ElectricEIRChiller(1).CondenserType = DataPlant::CondenserType::WaterCooled;
    state->dataChillerElectricEIR->ElectricEIRChiller(1).MinUnloadRat = 0.10;
    state->dataChillerElectricEIR->ElectricEIRChiller(1).MaxPartLoadRat = 1.15;

    int CurveNum;
    state->dataCurveManager->NumCurves = 3;
    state->dataCurveManager->PerfCurve.allocate(state->dataCurveManager->NumCurves);

    state->dataCurveManager->NumCurves = 3;
    state->dataCurveManager->PerfCurve.allocate(state->dataCurveManager->NumCurves);

    // Cap=f(T)
    CurveNum = 1;
    state->dataCurveManager->PerfCurve(CurveNum).curveType = CurveType::BiQuadratic;
    state->dataCurveManager->PerfCurve(CurveNum).NumDims = 2;
    state->dataCurveManager->PerfCurve(CurveNum).ObjectType = "Curve:BiQuadratic";
    state->dataCurveManager->PerfCurve(CurveNum).InterpolationType = InterpType::EvaluateCurveToLimits;
    state->dataCurveManager->PerfCurve(CurveNum).Name = "ElectricEIRChiller McQuay WSC 471kW/5.89COP/Vanes CAPFT";
    state->dataCurveManager->PerfCurve(CurveNum).Coeff1 = 2.521130E-01;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff2 = 1.324053E-02;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff3 = -8.637329E-03;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff4 = 8.581056E-02;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff5 = -4.261176E-03;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff6 = 8.661899E-03;
    state->dataCurveManager->PerfCurve(CurveNum).Var1Min = 7.22;
    state->dataCurveManager->PerfCurve(CurveNum).Var1Max = 12.78;
    state->dataCurveManager->PerfCurve(CurveNum).Var2Min = 12.78;
    state->dataCurveManager->PerfCurve(CurveNum).Var2Max = 26.67;
    state->dataChillerElectricEIR->ElectricEIRChiller(1).ChillerCapFTIndex = 1;

    // EIR=f(T)
    CurveNum = 2;
    state->dataCurveManager->PerfCurve(CurveNum).curveType = CurveType::BiQuadratic;
    state->dataCurveManager->PerfCurve(CurveNum).NumDims = 2;
    state->dataCurveManager->PerfCurve(CurveNum).ObjectType = "Curve:BiQuadratic";
    state->dataCurveManager->PerfCurve(CurveNum).InterpolationType = InterpType::EvaluateCurveToLimits;
    state->dataCurveManager->PerfCurve(CurveNum).Name = "ElectricEIRChiller McQuay WSC 471kW/5.89COP/Vanes EIRFT";
    state->dataCurveManager->PerfCurve(CurveNum).Coeff1 = 4.475238E-01;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff2 = -2.588210E-02;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff3 = -1.459053E-03;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff4 = 4.342595E-02;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff5 = -1.000651E-03;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff6 = 1.920106E-03;
    state->dataCurveManager->PerfCurve(CurveNum).Var1Min = 7.22;
    state->dataCurveManager->PerfCurve(CurveNum).Var1Max = 12.78;
    state->dataCurveManager->PerfCurve(CurveNum).Var2Min = 12.78;
    state->dataCurveManager->PerfCurve(CurveNum).Var2Max = 26.67;
    state->dataChillerElectricEIR->ElectricEIRChiller(1).ChillerEIRFTIndex = 2;

    // EIR=f(PLR)
    CurveNum = 3;
    state->dataCurveManager->PerfCurve(CurveNum).curveType = CurveType::Cubic;
    state->dataCurveManager->PerfCurve(CurveNum).NumDims = 1;
    state->dataCurveManager->PerfCurve(CurveNum).ObjectType = "Curve:Quadratic";
    state->dataCurveManager->PerfCurve(CurveNum).InterpolationType = InterpType::EvaluateCurveToLimits;
    state->dataCurveManager->PerfCurve(CurveNum).Name = "ElectricEIRChiller McQuay WSC 471kW/5.89COP/Vanes EIRFPLR";
    state->dataCurveManager->PerfCurve(CurveNum).Coeff1 = 2.778889E-01;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff2 = 2.338363E-01;
    state->dataCurveManager->PerfCurve(CurveNum).Coeff3 = 4.883748E-01;
    state->dataCurveManager->PerfCurve(CurveNum).Var1Min = 0;
    state->dataCurveManager->PerfCurve(CurveNum).Var1Max = 1.15;
    state->dataChillerElectricEIR->ElectricEIRChiller(1).ChillerEIRFPLRIndex = 3;

    Real64 IPLV;
    CalcChillerIPLV(*state,
                    state->dataChillerElectricEIR->ElectricEIRChiller(1).Name,
                    DataPlant::PlantEquipmentType::Chiller_ElectricEIR,
                    state->dataChillerElectricEIR->ElectricEIRChiller(1).RefCap,
                    state->dataChillerElectricEIR->ElectricEIRChiller(1).RefCOP,
                    state->dataChillerElectricEIR->ElectricEIRChiller(1).CondenserType,
                    state->dataChillerElectricEIR->ElectricEIRChiller(1).ChillerCapFTIndex,
                    state->dataChillerElectricEIR->ElectricEIRChiller(1).ChillerEIRFTIndex,
                    state->dataChillerElectricEIR->ElectricEIRChiller(1).ChillerEIRFPLRIndex,
                    state->dataChillerElectricEIR->ElectricEIRChiller(1).MinUnloadRat,
                    IPLV,
                    Optional<const Real64>(),
                    ObjexxFCL::Optional_int_const(),
                    Optional<const Real64>());

    EXPECT_DOUBLE_EQ(round(IPLV * 100) / 100, 5.44); // 18.56 IPLV
}

TEST_F(EnergyPlusFixture, SingleSpeedCoolingCoil_SEERValueTest)
{
    // Tests SEER Value calculation for single speed DX cooling coil
    // using user's PLF curve and AHRI default PLF curve

    std::string const idf_objects = delimited_string({
        "   Coil:Cooling:DX:SingleSpeed,",
        "     Heat Pump ACDXCoil,      !- Name",
        "     ,                        !- Availability Schedule Name",
        "     autosize,                !- Gross Rated Total Cooling Capacity {W}",
        "     autosize,                !- Gross Rated Sensible Heat Ratio",
        "     4.0,                     !- Gross Rated Cooling COP {W/W}",
        "     autosize,                !- Rated Air Flow Rate {m3/s}",
        "     495.0,                   !- 2017 Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "     495.0,                   !- 2023 Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "     DX Cooling Coil Air Inlet Node,  !- Air Inlet Node Name",
        "     Heating Coil Air Inlet Node,  !- Air Outlet Node Name",
        "     HPACCoolCapFT,           !- Total Cooling Capacity Function of Temperature Curve Name",
        "     HPACCoolCapFFF,          !- Total Cooling Capacity Function of Flow Fraction Curve Name",
        "     HPACCOOLEIRFT,           !- Energy Input Ratio Function of Temperature Curve Name",
        "     HPACCOOLEIRFFF,          !- Energy Input Ratio Function of Flow Fraction Curve Name",
        "     HPACCOOLPLFFPLR;         !- Part Load Fraction Correlation Curve Name",

        "   Curve:Biquadratic,",
        "     HPACCoolCapFT,           !- Name",
        "     0.766956,                !- Coefficient1 Constant",
        "     0.0107756,               !- Coefficient2 x",
        "    -0.0000414703,            !- Coefficient3 x**2",
        "     0.00134961,              !- Coefficient4 y",
        "    -0.000261144,             !- Coefficient5 y**2",
        "     0.000457488,             !- Coefficient6 x*y",
        "     12.77778,                !- Minimum Value of x",
        "     23.88889,                !- Maximum Value of x",
        "     18.0,                    !- Minimum Value of y",
        "     46.11111,                !- Maximum Value of y",
        "     ,                        !- Minimum Curve Output",
        "     ,                        !- Maximum Curve Output",
        "     Temperature,             !- Input Unit Type for X",
        "     Temperature,             !- Input Unit Type for Y",
        "     Dimensionless;           !- Output Unit Type",

        "   Curve:Biquadratic,",
        "     HPACCOOLEIRFT,           !- Name",
        "     0.297145,                !- Coefficient1 Constant",
        "     0.0430933,               !- Coefficient2 x",
        "    -0.000748766,             !- Coefficient3 x**2",
        "     0.00597727,              !- Coefficient4 y",
        "     0.000482112,             !- Coefficient5 y**2",
        "    -0.000956448,             !- Coefficient6 x*y",
        "     12.77778,                !- Minimum Value of x",
        "     23.88889,                !- Maximum Value of x",
        "     18.0,                    !- Minimum Value of y",
        "     46.11111,                !- Maximum Value of y",
        "     ,                        !- Minimum Curve Output",
        "     ,                        !- Maximum Curve Output",
        "     Temperature,             !- Input Unit Type for X",
        "     Temperature,             !- Input Unit Type for Y",
        "     Dimensionless;           !- Output Unit Type",

        "   Curve:Cubic,",
        "     HPACHeatCapFT,           !- Name",
        "     0.758746,                !- Coefficient1 Constant",
        "     0.027626,                !- Coefficient2 x",
        "     0.000148716,             !- Coefficient3 x**2",
        "     0.0000034992,            !- Coefficient4 x**3",
        "     -20.0,                   !- Minimum Value of x",
        "     20.0,                    !- Maximum Value of x",
        "     ,                        !- Minimum Curve Output",
        "     ,                        !- Maximum Curve Output",
        "     Temperature,             !- Input Unit Type for X",
        "     Dimensionless;           !- Output Unit Type",

        "   Curve:Cubic,",
        "     HPACHeatCapFFF,          !- Name",
        "     0.84,                    !- Coefficient1 Constant",
        "     0.16,                    !- Coefficient2 x",
        "     0.0,                     !- Coefficient3 x**2",
        "     0.0,                     !- Coefficient4 x**3",
        "     0.5,                     !- Minimum Value of x",
        "     1.5;                     !- Maximum Value of x",

        "   Curve:Cubic,",
        "     HPACHeatEIRFT,           !- Name",
        "     1.19248,                 !- Coefficient1 Constant",
        "    -0.0300438,               !- Coefficient2 x",
        "     0.00103745,              !- Coefficient3 x**2",
        "    -0.000023328,             !- Coefficient4 x**3",
        "    -20.0,                    !- Minimum Value of x",
        "     20.0,                    !- Maximum Value of x",
        "     ,                        !- Minimum Curve Output",
        "     ,                        !- Maximum Curve Output",
        "     Temperature,             !- Input Unit Type for X",
        "     Dimensionless;           !- Output Unit Type",

        "   Curve:Quadratic,",
        "     HPACCoolCapFFF,          !- Name",
        "     0.8,                     !- Coefficient1 Constant",
        "     0.2,                     !- Coefficient2 x",
        "     0.0,                     !- Coefficient3 x**2",
        "     0.5,                     !- Minimum Value of x",
        "     1.5;                     !- Maximum Value of x",

        "   Curve:Quadratic,",
        "     HPACCOOLEIRFFF,          !- Name",
        "     1.156,                   !- Coefficient1 Constant",
        "    -0.1816,                  !- Coefficient2 x",
        "     0.0256,                  !- Coefficient3 x**2",
        "     0.5,                     !- Minimum Value of x",
        "     1.5;                     !- Maximum Value of x",

        "   Curve:Linear,",
        "     HPACCOOLPLFFPLR,         !- Name",
        "     0.90,                    !- Coefficient1 Constant",
        "     0.10,                    !- Coefficient2 x",
        "     0.0,                     !- Minimum Value of x",
        "     1.0;                     !- Maximum Value of x",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    GetDXCoils(*state);

    auto &thisCoil(state->dataDXCoils->DXCoil(1));
    auto &thisCoolPLFfPLR(state->dataCurveManager->PerfCurve(thisCoil.PLFFPLR(1)));
    // ckeck user PLF curve coefficients
    //?? Default PLF cofficients source ?
    EXPECT_EQ(0.90, thisCoolPLFfPLR.Coeff1);
    EXPECT_EQ(0.10, thisCoolPLFfPLR.Coeff2);
    EXPECT_EQ(0.0, thisCoolPLFfPLR.Var1Min);
    EXPECT_EQ(1.0, thisCoolPLFfPLR.Var1Max);
    Real64 minEIRfLowPLRXInput(0.0);
    Real64 maxEIRfLowPLRXInput(0.0);
    // check user PLF curve PLR limits
    CurveManager::GetCurveMinMaxValues(*state, thisCoil.PLFFPLR(1), minEIRfLowPLRXInput, maxEIRfLowPLRXInput);
    EXPECT_EQ(0.0, minEIRfLowPLRXInput);
    EXPECT_EQ(1.0, maxEIRfLowPLRXInput);

    // Test 1: user PLF curve is different from the AHRI Std 210/240-2008 default PLF Curve
    Array1D<Real64> NetCoolingCapRated(1);
    Real64 SEER_User(0.0);
    Real64 SEER_Standard(0.0);
    Real64 SEER2_User(0.0);
    Real64 SEER2_Standard(0.0);
    Real64 EER(0.0);
    Real64 IEER(0.0);
    NetCoolingCapRated = 0.0;
    // set rated capacity and flow rate
    //?? Did these come from 2008 standard Test condition or
    thisCoil.RatedTotCap(1) = 25000.0;
    thisCoil.RatedAirVolFlowRate(1) = 1.300;

    EXPECT_EQ(495.0, thisCoil.FanPowerPerEvapAirFlowRate_2023(1));
    std::map<std::string, Real64> StandarRatingResults;
    // calculate standard ratings
    StandarRatingResults = SingleSpeedDXCoolingCoilStandardRatings(*state,
                                                                   thisCoil.Name,
                                                                   thisCoil.DXCoilType,
                                                                   thisCoil.CCapFTemp(1),
                                                                   thisCoil.CCapFFlow(1),
                                                                   thisCoil.EIRFTemp(1),
                                                                   thisCoil.EIRFFlow(1),
                                                                   thisCoil.PLFFPLR(1),
                                                                   thisCoil.RatedTotCap(1),
                                                                   thisCoil.RatedCOP(1),
                                                                   thisCoil.RatedAirVolFlowRate(1),
                                                                   thisCoil.FanPowerPerEvapAirFlowRate(1),
                                                                   thisCoil.FanPowerPerEvapAirFlowRate_2023(1) //,
                                                                                                               // NetCoolingCapRated(1),
                                                                                                               // SEER_User,     // 3.81
                                                                                                               // SEER_Standard, // 3.51
                                                                                                               // EER,
                                                                                                               // IEER,
                                                                                                               // SEER2_User,      // 3.81
                                                                                                               // SEER2_Standard // 3.61
    );

    SEER_User = StandarRatingResults["SEER_User"];
    SEER_Standard = StandarRatingResults["SEER_Standard"];
    SEER2_User = StandarRatingResults["SEER2_User"];
    SEER2_Standard = StandarRatingResults["SEER2_Standard"];
    // check SEER values calculated using user PLF and default PLF curve
    EXPECT_NEAR(13.00, SEER_User * StandardRatings::ConvFromSIToIP, 0.01);
    EXPECT_NEAR(11.98, SEER_Standard * StandardRatings::ConvFromSIToIP, 0.01);
    // auto a = SEER2_User * StandardRatings::ConvFromSIToIP;
    // auto b = SEER2_Standard * StandardRatings::ConvFromSIToIP;
    EXPECT_NEAR(13.00, SEER2_User * StandardRatings::ConvFromSIToIP, 0.01);
    EXPECT_NEAR(12.32, SEER2_Standard * StandardRatings::ConvFromSIToIP, 0.01);

    // Test 2: user PLF curve is the same as the AHRI Std 210/240-2008 default PLF Curve
    // reset the user PLF curve to the AHRI Std 210/240-2008 default PLF curve
    // AHRI Std 210/240-2008 default PLF curve is linear equation, PLF = a + b * PLR
    thisCoolPLFfPLR.Coeff1 = 0.75; // = a 0.8
    thisCoolPLFfPLR.Coeff2 = 0.25; // = b 0.2
    thisCoolPLFfPLR.Var1Min = 0.0; // PLR minimum value allowed by the PLF curve
    thisCoolPLFfPLR.Var1Max = 1.0; // PLR maximum value allowed by the PLF curve
    // reset output variables
    SEER_User = 0.0;
    SEER_Standard = 0.0;
    SEER2_User = 0.0;
    SEER2_Standard = 0.0;
    EER = 0.0;
    IEER = 0.0;
    NetCoolingCapRated = 0.0;
    StandarRatingResults.clear();
    // rerun the standard ratings calculation
    StandarRatingResults = SingleSpeedDXCoolingCoilStandardRatings(*state,
                                                                   thisCoil.Name,
                                                                   thisCoil.DXCoilType,
                                                                   thisCoil.CCapFTemp(1),
                                                                   thisCoil.CCapFFlow(1),
                                                                   thisCoil.EIRFTemp(1),
                                                                   thisCoil.EIRFFlow(1),
                                                                   thisCoil.PLFFPLR(1),
                                                                   thisCoil.RatedTotCap(1),
                                                                   thisCoil.RatedCOP(1),
                                                                   thisCoil.RatedAirVolFlowRate(1),
                                                                   thisCoil.FanPowerPerEvapAirFlowRate(1),
                                                                   thisCoil.FanPowerPerEvapAirFlowRate_2023(1) //,
                                                                                                               // NetCoolingCapRated(1),
                                                                                                               // SEER_User,     // 3.51
                                                                                                               // SEER_Standard, // 3.51
                                                                                                               // EER,
                                                                                                               // IEER,
                                                                                                               // SEER2_User,    // 3.51
                                                                                                               // SEER2_Standard // 3.61
    );
    SEER_User = StandarRatingResults["SEER_User"];
    SEER_Standard = StandarRatingResults["SEER_Standard"];
    SEER2_User = StandarRatingResults["SEER2_User"];
    SEER2_Standard = StandarRatingResults["SEER2_Standard"];
    // SEER and SEER_Default must match for the same PLF curve
    EXPECT_DOUBLE_EQ(SEER_User, SEER_Standard);
    EXPECT_NEAR(11.98, SEER_User * StandardRatings::ConvFromSIToIP, 0.01);
    EXPECT_NEAR(11.98, SEER_Standard * StandardRatings::ConvFromSIToIP, 0.01);

    // Test 3: user PLF curve is the same as the AHRI Std 210/240-2023 default PLF Curve
    // reset the user PLF curve to the AHRI Std 210/240-2008 default PLF curve
    // AHRI Std 210/240-2023 default PLF curve is linear equation, PLF = a + b * PLR
    thisCoolPLFfPLR.Coeff1 = 0.80; //
    thisCoolPLFfPLR.Coeff2 = 0.20; //
    thisCoolPLFfPLR.Var1Min = 0.0; // PLR minimum value allowed by the PLF curve
    thisCoolPLFfPLR.Var1Max = 1.0; // PLR maximum value allowed by the PLF curve
    // reset output variables
    SEER_User = 0.0;
    SEER_Standard = 0.0;
    SEER2_User = 0.0;
    SEER2_Standard = 0.0;
    EER = 0.0;
    IEER = 0.0;
    NetCoolingCapRated = 0.0;
    StandarRatingResults.clear();
    // rerun the standard ratings calculation for PLF Curve (AHRI 2023)
    StandarRatingResults = SingleSpeedDXCoolingCoilStandardRatings(*state,
                                                                   thisCoil.Name,
                                                                   thisCoil.DXCoilType,
                                                                   thisCoil.CCapFTemp(1),
                                                                   thisCoil.CCapFFlow(1),
                                                                   thisCoil.EIRFTemp(1),
                                                                   thisCoil.EIRFFlow(1),
                                                                   thisCoil.PLFFPLR(1),
                                                                   thisCoil.RatedTotCap(1),
                                                                   thisCoil.RatedCOP(1),
                                                                   thisCoil.RatedAirVolFlowRate(1),
                                                                   thisCoil.FanPowerPerEvapAirFlowRate(1),
                                                                   thisCoil.FanPowerPerEvapAirFlowRate_2023(1) //,
                                                                                                               // NetCoolingCapRated(1),
                                                                                                               // SEER_User,
                                                                                                               // SEER_Standard,
                                                                                                               // EER,
                                                                                                               // IEER,
                                                                                                               // SEER2_User,    // 3.61
                                                                                                               // SEER2_Standard // 3.61
    );
    SEER_User = StandarRatingResults["SEER_User"];
    SEER_Standard = StandarRatingResults["SEER_Standard"];
    SEER2_User = StandarRatingResults["SEER2_User"];
    SEER2_Standard = StandarRatingResults["SEER2_Standard"];

    // SEER2_USER and SEER2_Default/Standard must match for the same PLF curve
    // auto c = SEER2_User * StandardRatings::ConvFromSIToIP;
    // auto d = SEER2_Standard * StandardRatings::ConvFromSIToIP;
    EXPECT_DOUBLE_EQ(SEER2_User, SEER2_Standard);
    EXPECT_NEAR(12.32, SEER2_User * StandardRatings::ConvFromSIToIP, 0.01);
    EXPECT_NEAR(12.32, SEER2_Standard * StandardRatings::ConvFromSIToIP, 0.01);

    // Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)} for both 2017 & 2023
    // are same (495) so the NetCoolingCapacity, EER and IEER for both 2017 & 2023 standard
    // should match
    EXPECT_DOUBLE_EQ(StandarRatingResults["EER_2023"], StandarRatingResults["EER"]);
    // EXPECT_DOUBLE_EQ(StandarRatingResults["IEER_2023"], StandarRatingResults["IEER"]);
    EXPECT_DOUBLE_EQ(StandarRatingResults["NetCoolingCapRated2023"], StandarRatingResults["NetCoolingCapRated"]);
    EXPECT_NEAR(3.53, StandarRatingResults["EER"], 0.01);
    EXPECT_NEAR(3.53, StandarRatingResults["EER_2023"], 0.01);
    EXPECT_NEAR(3.65, StandarRatingResults["IEER"], 0.01);
    // EXPECT_NEAR(3.65, StandarRatingResults["IEER_2023"], 0.01);
    EXPECT_NEAR(24340.78, StandarRatingResults["NetCoolingCapRated"], 0.01);
    EXPECT_NEAR(24340.78, StandarRatingResults["NetCoolingCapRated2023"], 0.01);
}

TEST_F(EnergyPlusFixture, MultiSpeedCoolingCoil_SEERValueTest)
{
    // Tests SEER Value calculation for multi speed DX cooling coil
    // using user's PLF curve and AHRI default PLF curve

    std::string const idf_objects = delimited_string({
        "   Coil:Cooling:DX:MultiSpeed,",
        "     MS DX Cooling Coil,      !- Name",
        "     ,                        !- Availability Schedule Name",
        "     MSDXClgCoil AirInletNode,!- Air Inlet Node Name",
        "     MSDXHtgCoil AirInletNode,!- Air Outlet Node Name",
        "     Condenser Air Inlet Node,!- Condenser Air Inlet Node Name",
        "     AirCooled,               !- Condenser Type",
        "     -8.0,                    !- Minimum Outdoor Dry-Bulb Temperature for Compressor Operation {C}",
        "     ,                        !- Supply Water Storage Tank Name",
        "     ,                        !- Condensate Collection Water Storage Tank Name",
        "     No,                      !- Apply Part Load Fraction to Speeds Greater than 1",
        "     No,                      !- Apply Latent Degradation to Speeds Greater than 1",
        "     200.0,                   !- Crankcase Heater Capacity {W}",
        "     10.0,                    !- Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation {C}",
        "     ,                        !- Basin Heater Capacity {W/K}",
        "     ,                        !- Basin Heater Setpoint Temperature {C}",
        "     ,                        !- Basin Heater Operating Schedule Name",
        "     NaturalGas,              !- Fuel Type",
        "     4,                       !- Number of Speeds",
        "     7500,                    !- Speed 1 Gross Rated Total Cooling Capacity {W}",
        "     0.75,                    !- Speed 1 Gross Rated Sensible Heat Ratio",
        "     4.0,                     !- Speed 1 Gross Rated Cooling COP {W/W}",
        "     0.40,                    !- Speed 1 Rated Air Flow Rate {m3/s}",
        "     453.3,                   !- 2017 Speed 1 Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "     453.3,                   !- 2023 Speed 1 Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}", //??TBD:BPS
        "     HPACCoolCapFT Speed,     !- Speed 1 Total Cooling Capacity Function of Temperature Curve Name",
        "     HPACCoolCapFF Speed,     !- Speed 1 Total Cooling Capacity Function of Flow Fraction Curve Name",
        "     HPACCOOLEIRFT Speed,     !- Speed 1 Energy Input Ratio Function of Temperature Curve Name",
        "     HPACCOOLEIRFF Speed,     !- Speed 1 Energy Input Ratio Function of Flow Fraction Curve Name",
        "     HPACCOOLPLFFPLR Speed,   !- Speed 1 Part Load Fraction Correlation Curve Name",
        "     1000.0,                  !- Speed 1 Nominal Time for Condensate Removal to Begin {s}",
        "     1.5,                     !- Speed 1 Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity {dimensionless}",
        "     3.0,                     !- Speed 1 Maximum Cycling Rate {cycles/hr}",
        "     45.0,                    !- Speed 1 Latent Capacity Time Constant {s}",
        "     0.2,                     !- Speed 1 Rated Waste Heat Fraction of Power Input {dimensionless}",
        "     HAPCCoolWHFT Speed,      !- Speed 1 Waste Heat Function of Temperature Curve Name",
        "     0.9,                     !- Speed 1 Evaporative Condenser Effectiveness {dimensionless}",
        "     0.05,                    !- Speed 1 Evaporative Condenser Air Flow Rate {m3/s}",
        "     50,                      !- Speed 1 Rated Evaporative Condenser Pump Power Consumption {W}",
        "     17500,                   !- Speed 2 Gross Rated Total Cooling Capacity {W}",
        "     0.75,                    !- Speed 2 Gross Rated Sensible Heat Ratio",
        "     4.0,                     !- Speed 2 Gross Rated Cooling COP {W/W}",
        "     0.85,                    !- Speed 2 Rated Air Flow Rate {m3/s}",
        "     523.3,                   !- 2017 Speed 2 Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "     523.3,                   !- 2023 Speed 2 Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}", //??TBD:BPS
        "     HPACCoolCapFT Speed,     !- Speed 2 Total Cooling Capacity Function of Temperature Curve Name",
        "     HPACCoolCapFF Speed,     !- Speed 2 Total Cooling Capacity Function of Flow Fraction Curve Name",
        "     HPACCOOLEIRFT Speed,     !- Speed 2 Energy Input Ratio Function of Temperature Curve Name",
        "     HPACCOOLEIRFF Speed,     !- Speed 2 Energy Input Ratio Function of Flow Fraction Curve Name",
        "     HPACCOOLPLFFPLR Speed,   !- Speed 2 Part Load Fraction Correlation Curve Name",
        "     1000.0,                  !- Speed 2 Nominal Time for Condensate Removal to Begin {s}",
        "     1.5,                     !- Speed 2 Ratio of Initial Moisture Evaporation Rate and steady state Latent Capacity {dimensionless}",
        "     3.0,                     !- Speed 2 Maximum Cycling Rate {cycles/hr}",
        "     45.0,                    !- Speed 2 Latent Capacity Time Constant {s}",
        "     0.2,                     !- Speed 2 Rated Waste Heat Fraction of Power Input {dimensionless}",
        "     HAPCCoolWHFT Speed,      !- Speed 2 Waste Heat Function of Temperature Curve Name",
        "     0.9,                     !- Speed 2 Evaporative Condenser Effectiveness {dimensionless}",
        "     0.1,                     !- Speed 2 Evaporative Condenser Air Flow Rate {m3/s}",
        "     60,                      !- Speed 2 Rated Evaporative Condenser Pump Power Consumption {W}",
        "     25500,                   !- Speed 3 Gross Rated Total Cooling Capacity {W}",
        "     0.75,                    !- Speed 3 Gross Rated Sensible Heat Ratio",
        "     4.0,                     !- Speed 3 Gross Rated Cooling COP {W/W}",
        "     1.25,                    !- Speed 3 Rated Air Flow Rate {m3/s}",
        "     573.3,                   !- 2017 Speed 3 Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "     573.3,                   !- 2023 Speed 3 Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}", //??TBD:BPS
        "     HPACCoolCapFT Speed,     !- Speed 3 Total Cooling Capacity Function of Temperature Curve Name",
        "     HPACCoolCapFF Speed,     !- Speed 3 Total Cooling Capacity Function of Flow Fraction Curve Name",
        "     HPACCOOLEIRFT Speed,     !- Speed 3 Energy Input Ratio Function of Temperature Curve Name",
        "     HPACCOOLEIRFF Speed,     !- Speed 3 Energy Input Ratio Function of Flow Fraction Curve Name",
        "     HPACCOOLPLFFPLR Speed,   !- Speed 3 Part Load Fraction Correlation Curve Name",
        "     1000.0,                  !- Speed 3 Nominal Time for Condensate Removal to Begin {s}",
        "     1.5,                     !- Speed 3 Ratio of Initial Moisture Evaporation Rate and steady state Latent Capacity {dimensionless}",
        "     3.0,                     !- Speed 3 Maximum Cycling Rate {cycles/hr}",
        "     45.0,                    !- Speed 3 Latent Capacity Time Constant {s}",
        "     0.2,                     !- Speed 3 Rated Waste Heat Fraction of Power Input {dimensionless}",
        "     HAPCCoolWHFT Speed,      !- Speed 3 Waste Heat Function of Temperature Curve Name",
        "     0.9,                     !- Speed 3 Evaporative Condenser Effectiveness {dimensionless}",
        "     0.2,                     !- Speed 3 Evaporative Condenser Air Flow Rate {m3/s}",
        "     80,                      !- Speed 3 Rated Evaporative Condenser Pump Power Consumption {W}",
        "     35500,                   !- Speed 4 Gross Rated Total Cooling Capacity {W}",
        "     0.75,                    !- Speed 4 Gross Rated Sensible Heat Ratio",
        "     4.0,                     !- Speed 4 Gross Rated Cooling COP {W/W}",
        "     1.75,                    !- Speed 4 Rated Air Flow Rate {m3/s}",
        "     673.3,                   !- 2017 Speed 4 Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}",
        "     673.3,                   !- 2023 Speed 4 Rated Evaporator Fan Power Per Volume Flow Rate {W/(m3/s)}", //??TBD:BPS
        "     HPACCoolCapFT Speed,     !- Speed 4 Total Cooling Capacity Function of Temperature Curve Name",
        "     HPACCoolCapFF Speed,     !- Speed 4 Total Cooling Capacity Function of Flow Fraction Curve Name",
        "     HPACCOOLEIRFT Speed,     !- Speed 4 Energy Input Ratio Function of Temperature Curve Name",
        "     HPACCOOLEIRFF Speed,     !- Speed 4 Energy Input Ratio Function of Flow Fraction Curve Name",
        "     HPACCOOLPLFFPLR Speed,   !- Speed 4 Part Load Fraction Correlation Curve Name",
        "     1000.0,                  !- Speed 4 Nominal Time for Condensate Removal to Begin {s}",
        "     1.5,                     !- Speed 4 Ratio of Initial Moisture Evaporation Rate and steady state Latent Capacity {dimensionless}",
        "     4.0,                     !- Speed 4 Maximum Cycling Rate {cycles/hr}",
        "     45.0,                    !- Speed 4 Latent Capacity Time Constant {s}",
        "     0.2,                     !- Speed 4 Rated Waste Heat Fraction of Power Input {dimensionless}",
        "     HAPCCoolWHFT Speed,      !- Speed 4 Waste Heat Function of Temperature Curve Name",
        "     0.9,                     !- Speed 4 Evaporative Condenser Effectiveness {dimensionless}",
        "     0.3,                     !- Speed 4 Evaporative Condenser Air Flow Rate {m3/s}",
        "     100;                     !- Speed 4 Rated Evaporative Condenser Pump Power Consumption {W}",

        "   OutdoorAir:Node,",
        "     Outdoor Condenser Air Node,  !- Name",
        "     1.0;                     !- Height Above Ground {m}",

        "   Curve:Biquadratic,",
        "     HPACCoolCapFT Speed,     !- Name",
        "     0.476428E+00,            !- Coefficient1 Constant",
        "     0.401147E-01,            !- Coefficient2 x",
        "     0.226411E-03,            !- Coefficient3 x**2",
        "    -0.827136E-03,            !- Coefficient4 y",
        "    -0.732240E-05,            !- Coefficient5 y**2",
        "    -0.446278E-03,            !- Coefficient6 x*y",
        "     12.77778,                !- Minimum Value of x",
        "     23.88889,                !- Maximum Value of x",
        "     23.88889,                !- Minimum Value of y",
        "     46.11111,                !- Maximum Value of y",
        "     ,                        !- Minimum Curve Output",
        "     ,                        !- Maximum Curve Output",
        "     Temperature,             !- Input Unit Type for X",
        "     Temperature,             !- Input Unit Type for Y",
        "     Dimensionless;           !- Output Unit Type",

        "   Curve:Cubic,",
        "     HPACCoolCapFF Speed,     !- Name",
        "     0.47278589,              !- Coefficient1 Constant",
        "     1.2433415,               !- Coefficient2 x",
        "    -1.0387055,               !- Coefficient3 x**2",
        "     0.32257813,              !- Coefficient4 x**3",
        "     0.5,                     !- Minimum Value of x",
        "     1.5;                     !- Maximum Value of x",

        "   Curve:Biquadratic,",
        "     HPACCOOLEIRFT Speed,     !- Name",
        "     0.632475E+00,            !- Coefficient1 Constant",
        "    -0.121321E-01,            !- Coefficient2 x",
        "     0.507773E-03,            !- Coefficient3 x**2",
        "     0.155377E-01,            !- Coefficient4 y",
        "     0.272840E-03,            !- Coefficient5 y**2",
        "    -0.679201E-03,            !- Coefficient6 x*y",
        "     12.77778,                !- Minimum Value of x",
        "     23.88889,                !- Maximum Value of x",
        "     23.88889,                !- Minimum Value of y",
        "     46.11111,                !- Maximum Value of y",
        "     ,                        !- Minimum Curve Output",
        "     ,                        !- Maximum Curve Output",
        "     Temperature,             !- Input Unit Type for X",
        "     Temperature,             !- Input Unit Type for Y",
        "     Dimensionless;           !- Output Unit Type",

        "   Curve:Cubic,",
        "     HPACCOOLEIRFF Speed,     !- Name",
        "     0.47278589,              !- Coefficient1 Constant",
        "     1.2433415,               !- Coefficient2 x",
        "    -1.0387055,               !- Coefficient3 x**2",
        "     0.32257813,              !- Coefficient4 x**3",
        "     0.5,                     !- Minimum Value of x",
        "     1.5;                     !- Maximum Value of x",

        "   Curve:Linear,",
        "     HPACCOOLPLFFPLR Speed,   !- Name",
        "     0.90,                    !- Coefficient1 Constant",
        "     0.10,                    !- Coefficient2 x",
        "     0.0,                     !- Minimum Value of x",
        "     1.0;                     !- Maximum Value of x",

        "   Curve:Biquadratic,",
        "     HAPCCoolWHFT Speed,      !- Name",
        "     1.0,                     !- Coefficient1 Constant",
        "     0.0,                     !- Coefficient2 x",
        "     0.0,                     !- Coefficient3 x**2",
        "     0.0,                     !- Coefficient4 y",
        "     0.0,                     !- Coefficient5 y**2",
        "     0.0,                     !- Coefficient6 x*y",
        "     0,                       !- Minimum Value of x",
        "     50,                      !- Maximum Value of x",
        "     0,                       !- Minimum Value of y",
        "     50,                      !- Maximum Value of y",
        "     ,                        !- Minimum Curve Output",
        "     ,                        !- Maximum Curve Output",
        "     Temperature,             !- Input Unit Type for X",
        "     Temperature,             !- Input Unit Type for Y",
        "     Dimensionless;           !- Output Unit Type",
    });

    ASSERT_TRUE(process_idf(idf_objects));

    GetDXCoils(*state);

    auto &thisCoil(state->dataDXCoils->DXCoil(1));
    auto &thisCoolPLFfPLR(state->dataCurveManager->PerfCurve(thisCoil.MSPLFFPLR(1)));
    // ckeck user PLF curve coefficients
    EXPECT_EQ(0.90, thisCoolPLFfPLR.Coeff1);
    EXPECT_EQ(0.10, thisCoolPLFfPLR.Coeff2);
    EXPECT_EQ(0.0, thisCoolPLFfPLR.Var1Min);
    EXPECT_EQ(1.0, thisCoolPLFfPLR.Var1Max);
    Real64 minEIRfLowPLRXInput(0.0);
    Real64 maxEIRfLowPLRXInput(0.0);
    // check user PLF curve PLR limits
    CurveManager::GetCurveMinMaxValues(*state, thisCoil.MSPLFFPLR(1), minEIRfLowPLRXInput, maxEIRfLowPLRXInput);
    EXPECT_EQ(0.0, minEIRfLowPLRXInput);
    EXPECT_EQ(1.0, maxEIRfLowPLRXInput);

    // Test 1: user PLF curve is different from the AHRI Std 210/240-2008 default PLF Curve
    std::map<std::string, Real64> StandardRatingsResult;
    Array1D<Real64> NetCoolingCapRated(thisCoil.NumOfSpeeds);
    Real64 SEER_User(0.0);
    Real64 SEER_Standard(0.0);

    Array1D<Real64> NetCoolingCapRated_2023(thisCoil.NumOfSpeeds);
    Real64 SEER2_User(0.0);
    Real64 SEER2_Standard(0.0);
    // calculate standard ratings for multispeed DX cooling coil
    StandardRatingsResult = MultiSpeedDXCoolingCoilStandardRatings(*state,
                                                                   thisCoil.MSCCapFTemp,
                                                                   thisCoil.MSCCapFFlow,
                                                                   thisCoil.MSEIRFTemp,
                                                                   thisCoil.MSEIRFFlow,
                                                                   thisCoil.MSPLFFPLR,
                                                                   thisCoil.MSRatedTotCap,
                                                                   thisCoil.MSRatedCOP,
                                                                   thisCoil.MSRatedAirVolFlowRate,
                                                                   thisCoil.MSFanPowerPerEvapAirFlowRate,
                                                                   thisCoil.MSFanPowerPerEvapAirFlowRate_2023,
                                                                   thisCoil.NumOfSpeeds);
    NetCoolingCapRated(thisCoil.NumOfSpeeds) = StandardRatingsResult["NetCoolingCapRatedMaxSpeed"];
    SEER_User = StandardRatingsResult["SEER_User"];
    SEER_Standard = StandardRatingsResult["SEER_Standard"];

    NetCoolingCapRated_2023(thisCoil.NumOfSpeeds) = StandardRatingsResult["NetCoolingCapRatedMaxSpeed2023"];
    SEER2_User = StandardRatingsResult["SEER2_User"];
    SEER2_Standard = StandardRatingsResult["SEER2_Standard"];
    // check SEER values calculated using user PLF and default PLF curve
    EXPECT_NEAR(4.09, SEER_User, 0.01);
    EXPECT_NEAR(4.06, SEER_Standard, 0.01);
    EXPECT_NEAR(34330.75, NetCoolingCapRated(thisCoil.NumOfSpeeds), 0.01);
    EXPECT_NEAR(13.95, SEER_User * StandardRatings::ConvFromSIToIP, 0.01);
    EXPECT_NEAR(13.86, SEER_Standard * StandardRatings::ConvFromSIToIP, 0.01);
    // check SEER values calculated using user PLF and default PLF curve | AHRI Std. 2023
    EXPECT_NEAR(4.00, SEER2_User, 0.01);
    EXPECT_NEAR(3.99, SEER2_Standard, 0.01);
    EXPECT_NEAR(34330.75, NetCoolingCapRated_2023(thisCoil.NumOfSpeeds), 0.01);
    EXPECT_NEAR(13.66, SEER2_User * StandardRatings::ConvFromSIToIP, 0.01);
    EXPECT_NEAR(13.64, SEER2_Standard * StandardRatings::ConvFromSIToIP, 0.01);

    // Test 2: user PLF curve is the same as the AHRI Std 210/240-2008 default PLF Curve
    // reset the user PLF curve to the AHRI Std 210/240-2008 default PLF curve
    // AHRI Std 210/240-2008 default PLF curve is linear equation, PLF = a + b * PLR
    thisCoolPLFfPLR.Coeff1 = 0.75; // = a
    thisCoolPLFfPLR.Coeff2 = 0.25; // = b
    thisCoolPLFfPLR.Var1Min = 0.0; // PLR minimum value allowed by the PLF curve
    thisCoolPLFfPLR.Var1Max = 1.0; // PLR maximum value allowed by the PLF curve
    // reset output variables
    SEER_User = 0.0;
    SEER_Standard = 0.0;
    NetCoolingCapRated(thisCoil.NumOfSpeeds) = 0.0;

    SEER2_User = 0.0;
    SEER2_Standard = 0.0;

    NetCoolingCapRated_2023(thisCoil.NumOfSpeeds) = 0.0;
    StandardRatingsResult.clear();
    // rerun the standard ratings calculation
    StandardRatingsResult = MultiSpeedDXCoolingCoilStandardRatings(*state,
                                                                   thisCoil.MSCCapFTemp,
                                                                   thisCoil.MSCCapFFlow,
                                                                   thisCoil.MSEIRFTemp,
                                                                   thisCoil.MSEIRFFlow,
                                                                   thisCoil.MSPLFFPLR,
                                                                   thisCoil.MSRatedTotCap,
                                                                   thisCoil.MSRatedCOP,
                                                                   thisCoil.MSRatedAirVolFlowRate,
                                                                   thisCoil.MSFanPowerPerEvapAirFlowRate,
                                                                   thisCoil.MSFanPowerPerEvapAirFlowRate_2023,
                                                                   thisCoil.NumOfSpeeds);
    NetCoolingCapRated(thisCoil.NumOfSpeeds) = StandardRatingsResult["NetCoolingCapRatedMaxSpeed"];
    SEER_User = StandardRatingsResult["SEER_User"];
    SEER_Standard = StandardRatingsResult["SEER_Standard"];

    NetCoolingCapRated_2023(thisCoil.NumOfSpeeds) = StandardRatingsResult["NetCoolingCapRatedMaxSpeed2023"];
    SEER2_User = StandardRatingsResult["SEER2_User"];
    SEER2_Standard = StandardRatingsResult["SEER2_Standard"];
    // SEER and SEER_Default must match for the same PLF curve
    EXPECT_NEAR(4.06, SEER_User, 0.01);
    EXPECT_NEAR(4.06, SEER_Standard, 0.01);
    EXPECT_NEAR(34330.75, NetCoolingCapRated(thisCoil.NumOfSpeeds), 0.01);
    EXPECT_DOUBLE_EQ(SEER_User, SEER_Standard);
    EXPECT_NEAR(13.86, SEER_User * StandardRatings::ConvFromSIToIP, 0.01);
    EXPECT_NEAR(13.86, SEER_Standard * StandardRatings::ConvFromSIToIP, 0.01);
    // check SEER values calculated using user PLF and default PLF curve | AHRI Std. 2023
    EXPECT_NEAR(3.99, SEER2_User, 0.01);
    EXPECT_NEAR(3.99, SEER2_Standard, 0.01);
    EXPECT_NEAR(34330.75, NetCoolingCapRated_2023(thisCoil.NumOfSpeeds), 0.01);
    EXPECT_NEAR(13.61, SEER2_User * StandardRatings::ConvFromSIToIP, 0.01);
    EXPECT_NEAR(13.64, SEER2_Standard * StandardRatings::ConvFromSIToIP, 0.01);

    // Test 3: user PLF curve is the same as the AHRI Std 210/240-2023 default PLF Curve
    // reset the user PLF curve to the AHRI Std 210/240-2023 default PLF curve
    // AHRI Std 210/240-2023 default PLF curve is linear equation, PLF = a + b * PLR
    thisCoolPLFfPLR.Coeff1 = 0.80; // = a
    thisCoolPLFfPLR.Coeff2 = 0.20; // = b
    thisCoolPLFfPLR.Var1Min = 0.0; // PLR minimum value allowed by the PLF curve
    thisCoolPLFfPLR.Var1Max = 1.0; // PLR maximum value allowed by the PLF curve
    // reset output variables
    SEER_User = 0.0;
    SEER_Standard = 0.0;
    NetCoolingCapRated(thisCoil.NumOfSpeeds) = 0.0;

    SEER2_User = 0.0;
    SEER2_Standard = 0.0;
    NetCoolingCapRated_2023(thisCoil.NumOfSpeeds) = 0.0;
    StandardRatingsResult.clear();
    // rerun the standard ratings calculation
    StandardRatingsResult = MultiSpeedDXCoolingCoilStandardRatings(*state,
                                                                   thisCoil.MSCCapFTemp,
                                                                   thisCoil.MSCCapFFlow,
                                                                   thisCoil.MSEIRFTemp,
                                                                   thisCoil.MSEIRFFlow,
                                                                   thisCoil.MSPLFFPLR,
                                                                   thisCoil.MSRatedTotCap,
                                                                   thisCoil.MSRatedCOP,
                                                                   thisCoil.MSRatedAirVolFlowRate,
                                                                   thisCoil.MSFanPowerPerEvapAirFlowRate,
                                                                   thisCoil.MSFanPowerPerEvapAirFlowRate_2023,
                                                                   thisCoil.NumOfSpeeds);
    NetCoolingCapRated(thisCoil.NumOfSpeeds) = StandardRatingsResult["NetCoolingCapRatedMaxSpeed"];
    SEER_User = StandardRatingsResult["SEER_User"];
    SEER_Standard = StandardRatingsResult["SEER_Standard"];

    NetCoolingCapRated_2023(thisCoil.NumOfSpeeds) = StandardRatingsResult["NetCoolingCapRatedMaxSpeed2023"];
    SEER2_User = StandardRatingsResult["SEER2_User"];
    SEER2_Standard = StandardRatingsResult["SEER2_Standard"];

    //  check SEER values calculated using user PLF and default PLF curve
    EXPECT_NEAR(4.07, SEER_User, 0.01);
    EXPECT_NEAR(4.06, SEER_Standard, 0.01);
    EXPECT_NEAR(34330.75, NetCoolingCapRated(thisCoil.NumOfSpeeds), 0.01);
    EXPECT_NEAR(13.88, SEER_User * StandardRatings::ConvFromSIToIP, 0.01);
    EXPECT_NEAR(13.86, SEER_Standard * StandardRatings::ConvFromSIToIP, 0.01);
    // SEER2 and SEER2_Default must match for the same PLF curve | AHRI Std. 2023
    EXPECT_NEAR(3.99, SEER2_User, 0.01);     // 3.995
    EXPECT_NEAR(3.99, SEER2_Standard, 0.01); // 3.999
    EXPECT_NEAR(34330.75, NetCoolingCapRated_2023(thisCoil.NumOfSpeeds), 0.01);
    EXPECT_NEAR(SEER2_User, SEER2_Standard, 0.01);
    // EXPECT_DOUBLE_EQ(SEER2_User, SEER2_Standard);
    EXPECT_NEAR(13.63, SEER2_User * StandardRatings::ConvFromSIToIP, 0.01);
    EXPECT_NEAR(13.64, SEER2_Standard * StandardRatings::ConvFromSIToIP, 0.01);

    // SEER and SEER_Default must match for the same PLF curve
}

} // namespace EnergyPlus
