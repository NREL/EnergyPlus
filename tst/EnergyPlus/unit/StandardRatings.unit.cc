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

// EnergyPlus::StandardRatings unit tests

// Google test headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/DXCoils.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/StandardRatings.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::StandardRatings;
using namespace EnergyPlus::CurveManager;
using namespace EnergyPlus::DataHVACGlobals;
using namespace EnergyPlus::DXCoils;

namespace EnergyPlus {

TEST_F(EnergyPlusFixture, SingleSpeedHeatingCoilCurveTest)
{
    // Test that the standard ratings calculation with negative curve value

    using CurveManager::Cubic;
    using CurveManager::NumCurves;
    using CurveManager::Quadratic;
    using DXCoils::DXCoil;
    using Psychrometrics::PsyRhoAirFnPbTdbW;
    using StandardRatings::SingleSpeedDXHeatingCoilStandardRatings;

    // Set up heating coil and curves.
    int DXCoilNum;
    NumDXCoils = 1;
    DXCoilNum = 1;
    DXCoil.allocate(NumDXCoils);
    DXCoilNumericFields.allocate(1);
    DXCoilOutletTemp.allocate(NumDXCoils);
    DXCoilOutletHumRat.allocate(NumDXCoils);
    DXCoilFanOpMode.allocate(NumDXCoils);
    DXCoilPartLoadRatio.allocate(NumDXCoils);
    DXCoilTotalHeating.allocate(NumDXCoils);
    DXCoilHeatInletAirDBTemp.allocate(NumDXCoils);
    DXCoilHeatInletAirWBTemp.allocate(NumDXCoils);
    DXCoilData &Coil = DXCoil(DXCoilNum);

    Coil.Name = "DX Single Speed Heating Coil";
    Coil.DXCoilType = "Coil:Heating:DX:SingleSpeed";
    Coil.DXCoilType_Num = CoilDX_HeatingEmpirical;
    Coil.SchedPtr = DataGlobals::ScheduleAlwaysOn;
    Coil.RatedSHR(1) = 1.0;
    Coil.RatedTotCap(1) = 1600.0;
    Coil.RatedCOP(1) = 4.0;
    Coil.RatedEIR(1) = 1 / Coil.RatedCOP(1);
    Coil.RatedAirVolFlowRate(1) = 0.50;
    Coil.RatedAirMassFlowRate(1) =
        Coil.RatedAirVolFlowRate(1) * PsyRhoAirFnPbTdbW(EnergyPlus::DataEnvironment::StdBaroPress, 21.11, 0.00881, "InitDXCoil");
    Coil.FanPowerPerEvapAirFlowRate(1) = 773.3;
    Coil.MinOATCompressor = -10.0;
    Coil.CrankcaseHeaterCapacity = 0.0;
    Coil.MaxOATDefrost = 0.0;
    Coil.DefrostStrategy = Resistive;
    Coil.DefrostControl = 0; // timed defrost control type
    Coil.DefrostTime = 0.058333;
    Coil.DefrostCapacity = 1000;
    Coil.PLRImpact = false;
    Coil.FuelType = FuelTypeElectricity;
    Coil.RegionNum = 4;
    Coil.OATempCompressorOn = -5.0;
    Coil.OATempCompressorOnOffBlank = "true";
    NumCurves = 5;
    PerfCurve.allocate(NumCurves);
    PerfomanceCurveData *pCurve;

    int const nCapfT = 1;
    pCurve = &PerfCurve(nCapfT);
    pCurve->CurveType = Cubic;
    pCurve->NumDims = 1;
    pCurve->Name = "PTHPHeatingCAPFT";
    pCurve->Coeff1 = 0.876825;
    pCurve->Coeff2 = -0.002955;
    pCurve->Coeff3 = 5.8e-005;
    pCurve->Coeff4 = 0.025335;
    pCurve->Var1Min = -5;
    pCurve->Var1Max = 25;

    Coil.CCapFTemp(1) = nCapfT;

    int const nCapfFF = 2;
    pCurve = &PerfCurve(nCapfFF);
    pCurve->CurveType = Quadratic;
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

    int const nEIRfT = 3;
    pCurve = &PerfCurve(nEIRfT);
    pCurve->CurveType = Cubic;
    pCurve->NumDims = 1;
    pCurve->Name = "PTHPHeatingEIRFT";
    pCurve->Coeff1 = 0.704658;
    pCurve->Coeff2 = 0.008767;
    pCurve->Coeff3 = 0.000625;
    pCurve->Coeff4 = -0.009037;
    pCurve->Var1Min = -5;
    pCurve->Var1Max = 25;
    Coil.EIRFTemp(1) = nEIRfT;

    int const nEIRfFF = 4;
    pCurve = &PerfCurve(nEIRfFF);
    pCurve->CurveType = Quadratic;
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

    int const nPLFfPLR = 5;
    pCurve = &PerfCurve(nPLFfPLR);
    pCurve->CurveType = Quadratic;
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

    for (int CurveNum = 1; CurveNum <= NumCurves; ++CurveNum) {
        PerfomanceCurveData &rCurve = PerfCurve(CurveNum);
        if (rCurve.CurveType == Cubic) {
            rCurve.ObjectType = "Curve:Cubic";
        } else if (rCurve.CurveType == Quadratic) {
            rCurve.ObjectType = "Curve:Quadratic";
        }
        rCurve.InterpolationType = EvaluateCurveToLimits;
    }
    Real64 NetHeatingCapRatedHighTemp;
    Real64 NetHeatingCapRatedLowTemp;
    Real64 HSPF;

    SingleSpeedDXHeatingCoilStandardRatings(Coil.RatedTotCap(1), Coil.RatedCOP(1), Coil.CCapFFlow(1), Coil.CCapFTemp(1), Coil.EIRFFlow(1),
                                            Coil.EIRFTemp(1), Coil.RatedAirVolFlowRate(1), Coil.FanPowerPerEvapAirFlowRate(1),
                                            NetHeatingCapRatedHighTemp, NetHeatingCapRatedLowTemp, HSPF, Coil.RegionNum, Coil.MinOATCompressor,
                                            Coil.OATempCompressorOn, Coil.OATempCompressorOnOffBlank, Coil.DefrostControl);

    // evaluate capacity curves
    Real64 TotCapTempModFacRated = CurveValue(Coil.CCapFTemp(1), StandardRatings::HeatingOutdoorCoilInletAirDBTempRated);
    Real64 TotCapFlowModFac = CurveValue(Coil.CCapFFlow(1), 1.0);
    Real64 NetHeatingCapRated =
        Coil.RatedTotCap(1) * TotCapTempModFacRated * TotCapFlowModFac + Coil.RatedAirVolFlowRate(1) * Coil.FanPowerPerEvapAirFlowRate(1);
    // check curve values and heating capacity
    EXPECT_GT(TotCapTempModFacRated, 0.0);
    EXPECT_DOUBLE_EQ(TotCapFlowModFac, 1.0);
    EXPECT_DOUBLE_EQ(NetHeatingCapRatedHighTemp, NetHeatingCapRated);
    // evaluate capacity curves
    Real64 CapTempModFacH2Test = CurveValue(Coil.CCapFTemp(1), StandardRatings::HeatingOutdoorCoilInletAirDBTempH2Test);
    EXPECT_GT(CapTempModFacH2Test, 0.0);
    Real64 CapTempModFacH3Test = CurveValue(Coil.CCapFTemp(1), StandardRatings::HeatingOutdoorCoilInletAirDBTempH3Test);
    // if CapTempModFacH3Test curves value is less than zero, NetHeatingCapRatedLowTemp is set to zero
    EXPECT_LT(CapTempModFacH3Test, 0.0);

    // check heating capacity at low temperature
    EXPECT_DOUBLE_EQ(NetHeatingCapRatedLowTemp, 0.0);
    // evaluate EIR curves
    Real64 EIRTempModFacRated = CurveValue(Coil.EIRFTemp(1), StandardRatings::HeatingOutdoorCoilInletAirDBTempRated);
    Real64 EIRTempModFacH2Test = CurveValue(Coil.EIRFTemp(1), StandardRatings::HeatingOutdoorCoilInletAirDBTempH2Test);
    Real64 EIRTempModFacH3Test = CurveValue(Coil.EIRFTemp(1), StandardRatings::HeatingOutdoorCoilInletAirDBTempH3Test);
    // check EIR curve value
    EXPECT_LT(EIRTempModFacRated, 0.0);
    EXPECT_GT(EIRTempModFacH2Test, 0.0);
    EXPECT_GT(EIRTempModFacH3Test, 0.0);
    // if one of the CAP or EIR curves value is less than zero, then HSPF is set to zero
    EXPECT_DOUBLE_EQ(HSPF, 0.0);
}
} // namespace EnergyPlus
