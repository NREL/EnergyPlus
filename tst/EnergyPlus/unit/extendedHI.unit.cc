// EnergyPlus, Copyright (c) 1996-2024, The Board of Trustees of the University of Illinois,
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

#include <algorithm>
#include <cmath>
#include <iostream>
#include <vector>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/HVACSystemRootFindingAlgorithm.hh>
#include <EnergyPlus/extendedHI.hh>

using namespace EnergyPlus;

TEST_F(EnergyPlusFixture, extendedHI_pvstar)
{
    Real64 tol = 1e-8;
    std::vector<double> T_values = {200, 210, 220, 230, 240, 250, 260, 270, 280, 290, 300, 310, 320, 330, 340, 350, 360, 370};
    std::vector<double> result = {0.16315953136381425,
                                  0.7045737646159884,
                                  2.6639212639290717,
                                  8.972721335550697,
                                  27.315394188834492,
                                  76.0747215114313,
                                  195.83100375810935,
                                  470.0335224802147,
                                  991.9254222637857,
                                  1920.6801555359402,
                                  3538.9408236895683,
                                  6235.887915937109,
                                  10554.04916628072,
                                  17222.314773780665,
                                  27187.715714871047,
                                  41643.766112228834,
                                  62053.2640569111,
                                  90163.72448626769};
    for (size_t i = 0; i < T_values.size(); ++i) {
        EXPECT_NEAR(extendedHI::pvstar(T_values[i]), result[i], tol);
    }
}

TEST_F(EnergyPlusFixture, extendedHI_Le)
{
    Real64 tol = 1e-8;
    std::vector<double> T_values = {200, 210, 220, 230, 240, 250, 260, 270, 280, 290, 300, 310, 320, 330, 340, 350, 360, 370};
    std::vector<double> result = {2663805.16,
                                  2641405.16,
                                  2619005.16,
                                  2596605.16,
                                  2574205.16,
                                  2551805.16,
                                  2529405.16,
                                  2507005.16,
                                  2484605.16,
                                  2462205.16,
                                  2439805.16,
                                  2417405.16,
                                  2395005.16,
                                  2372605.16,
                                  2350205.16,
                                  2327805.16,
                                  2305405.16,
                                  2283005.16};
    for (size_t i = 0; i < T_values.size(); ++i) {
        EXPECT_NEAR(extendedHI::Le(T_values[i]), result[i], tol);
    }
}

TEST_F(EnergyPlusFixture, extendedHI_Qv)
{
    Real64 tol = 1e-8;
    std::vector<double> T_values = {200, 210, 220, 230, 240, 250, 260, 270, 280, 290, 300, 310, 320, 330, 340, 350, 360, 370};
    std::vector<double> P_values = {1, 10, 100, 1000, 10000};
    std::vector<std::vector<double>> result = {{49.94618970804489, 49.91176798964203, 49.56755080561335, 46.125378965326625, 11.703660562459378},
                                               {47.3566427480449, 47.32222102964202, 46.97800384561336, 43.53583200532663, 9.114113602459378},
                                               {44.76709578804489, 44.73267406964202, 44.38845688561335, 40.94628504532663, 6.5245666424593765},
                                               {42.17754882804489, 42.14312710964202, 41.79890992561335, 38.356738085326626, 3.9350196824593775},
                                               {39.58800186804489, 39.55358014964203, 39.20936296561336, 35.76719112532663, 1.3454727224593785},
                                               {36.998454908044884, 36.96403318964203, 36.61981600561335, 33.177644165326626, -1.2440742375406237},
                                               {34.4089079480449, 34.374486229642024, 34.03026904561336, 30.58809720532663, -3.8336211975406225},
                                               {31.819360988044888, 31.784939269642024, 31.440722085613352, 27.99855024532663, -6.423168157540623},
                                               {29.229814028044892, 29.195392309642024, 28.851175125613352, 25.409003285326627, -9.012715117540624},
                                               {26.640267068044892, 26.605845349642024, 26.261628165613356, 22.819456325326627, -11.602262077540622},
                                               {24.05072010804489, 24.016298389642024, 23.672081205613352, 20.229909365326627, -14.191809037540622},
                                               {21.461173148044892, 21.426751429642025, 21.082534245613353, 17.640362405326627, -16.781355997540622},
                                               {18.871626188044893, 18.837204469642025, 18.492987285613356, 15.050815445326627, -19.370902957540622},
                                               {16.28207922804489, 16.247657509642025, 15.903440325613353, 12.461268485326626, -21.960449917540625},
                                               {13.692532268044891, 13.658110549642025, 13.313893365613353, 9.871721525326627, -24.549996877540625},
                                               {11.102985308044891, 11.068563589642023, 10.724346405613353, 7.282174565326627, -27.139543837540625},
                                               {8.51343834804489, 8.479016629642024, 8.134799445613353, 4.692627605326626, -29.729090797540625},
                                               {5.923891388044891, 5.889469669642025, 5.545252485613354, 2.1030806453266275, -32.31863775754062}};
    for (size_t i = 0; i < T_values.size(); ++i) {
        for (size_t j = 0; j < P_values.size(); ++j) {
            EXPECT_NEAR(extendedHI::Qv(T_values[i], P_values[j]), result[i][j], tol);
        }
    }
}

TEST_F(EnergyPlusFixture, extendedHI_Zs)
{
    Real64 tol = 1e-8;
    std::vector<double> Rs_values = {0.0387, 0.5, 1, 1.2};
    std::vector<double> result = {52.1, 18750000.0, 600000000.0, 1492991999.9999998};
    for (size_t i = 0; i < Rs_values.size(); ++i) {
        EXPECT_NEAR(extendedHI::Zs(Rs_values[i]), result[i], tol);
    }
}

TEST_F(EnergyPlusFixture, extendedHI_Ra)
{
    Real64 tol = 1e-8;
    std::vector<double> Ts_values = {240, 260, 280, 300, 320, 340, 360};
    std::vector<double> Ta_values = {240, 260, 280, 300, 320, 340, 360};
    std::vector<std::vector<double>> result = {{0.05003742549640303,
                                                0.04919686900951711,
                                                0.0482949471609349,
                                                0.04733482621494337,
                                                0.046320477644314524,
                                                0.04525658366932734,
                                                0.04414842527373929},
                                               {0.04919686900951711,
                                                0.04834034953171599,
                                                0.04742549966979792,
                                                0.04645568018096253,
                                                0.04543499562267212,
                                                0.044368197070707924,
                                                0.04326056977845574},
                                               {0.0482949471609349,
                                                0.04742549966979792,
                                                0.04650092325775173,
                                                0.04552473014716025,
                                                0.04450111356275697,
                                                0.04343484913155035,
                                                0.04233118361901477},
                                               {0.04733482621494337,
                                                0.04645568018096253,
                                                0.04552473014716025,
                                                0.04454559620854969,
                                                0.04352251644673171,
                                                0.04246024843823163,
                                                0.041363960371290795},
                                               {0.046320477644314524,
                                                0.04543499562267212,
                                                0.04450111356275697,
                                                0.04352251644673171,
                                                0.04250344511572377,
                                                0.04144859919286657,
                                                0.040363031720372144},
                                               {0.04525658366932734,
                                                0.044368197070707924,
                                                0.04343484913155035,
                                                0.04246024843823163,
                                                0.04144859919286657,
                                                0.04040450670241235,
                                                0.03933287648842351},
                                               {0.04414842527373929,
                                                0.04326056977845574,
                                                0.04233118361901477,
                                                0.041363960371290795,
                                                0.040363031720372144,
                                                0.03933287648842351,
                                                0.03827822497025564}};
    for (size_t i = 0; i < Ts_values.size(); ++i) {
        for (size_t j = 0; j < Ta_values.size(); ++j) {
            EXPECT_NEAR(extendedHI::Ra(Ts_values[i], Ta_values[j]), result[i][j], tol);
        }
    }
}

TEST_F(EnergyPlusFixture, extendedHI_Ra_bar)
{
    Real64 tol = 1e-8;
    std::vector<double> Tf_values = {240, 260, 280, 300, 320, 340, 360};
    std::vector<double> Ta_values = {240, 260, 280, 300, 320, 340, 360};
    std::vector<std::vector<double>> result = {{0.07141547425826913,
                                                0.0698327896993347,
                                                0.06815365152671275,
                                                0.06638750352391155,
                                                0.0645450734750961,
                                                0.06263806274737325,
                                                0.060678819043721195},
                                               {0.0698327896993347,
                                                0.06823771141282657,
                                                0.06655336747218435,
                                                0.06478927443631394,
                                                0.06295606908639516,
                                                0.06106520744639883,
                                                0.0591286533968074},
                                               {0.06815365152671275,
                                                0.06655336747218435,
                                                0.06487108590376367,
                                                0.06311628619901372,
                                                0.061299411560035544,
                                                0.0594315809828658,
                                                0.05752429636310142},
                                               {0.06638750352391155,
                                                0.06478927443631394,
                                                0.06311628619901372,
                                                0.06137787932930683,
                                                0.0595842117328534,
                                                0.057745986422442296,
                                                0.05587417899074831},
                                               {0.0645450734750961,
                                                0.06295606908639516,
                                                0.061299411560035544,
                                                0.0595842117328534,
                                                0.05782026331305135,
                                                0.05601778840676245,
                                                0.05418718649514485},
                                               {0.06263806274737325,
                                                0.06106520744639883,
                                                0.0594315809828658,
                                                0.057745986422442296,
                                                0.05601778840676245,
                                                0.054256677818840554,
                                                0.05247244272030079},
                                               {0.060678819043721195,
                                                0.0591286533968074,
                                                0.05752429636310142,
                                                0.05587417899074831,
                                                0.05418718649514485,
                                                0.05247244272030079,
                                                0.0507391028959455}};
    for (size_t i = 0; i < Tf_values.size(); ++i) {
        for (size_t j = 0; j < Ta_values.size(); ++j) {
            EXPECT_NEAR(extendedHI::Ra_bar(Tf_values[i], Ta_values[j]), result[i][j], tol);
        }
    }
}

TEST_F(EnergyPlusFixture, extendedHI_Ra_un)
{
    Real64 tol = 1e-8;
    std::vector<double> Tf_values = {240, 260, 280, 300, 320, 340, 360};
    std::vector<double> Ta_values = {240, 260, 280, 300, 320, 340, 360};
    std::vector<std::vector<double>> result = {{0.06787493202819687,
                                                0.06642598354057656,
                                                0.06488609100341329,
                                                0.06326346252950685,
                                                0.06156752777670731,
                                                0.05980867097828581,
                                                0.05799794522838118},
                                               {0.06642598354057656,
                                                0.06496324482301812,
                                                0.06341597668917175,
                                                0.0617925013672891,
                                                0.06010221781318763,
                                                0.05835534085968962,
                                                0.056562627404263835},
                                               {0.06488609100341329,
                                                0.06341597668917175,
                                                0.06186785837632265,
                                                0.06025007445646361,
                                                0.0585719003382978,
                                                0.056843296943746194,
                                                0.05507465133709833},
                                               {0.06326346252950685,
                                                0.0617925013672891,
                                                0.06025007445646361,
                                                0.05864444445695526,
                                                0.056984678844683855,
                                                0.055280410417843366,
                                                0.05354159428335702},
                                               {0.06156752777670731,
                                                0.06010221781318763,
                                                0.0585719003382978,
                                                0.056984678844683855,
                                                0.05534933885591997,
                                                0.05367512249775102,
                                                0.05197150313175455},
                                               {0.05980867097828581,
                                                0.05835534085968962,
                                                0.056843296943746194,
                                                0.055280410417843366,
                                                0.05367512249775102,
                                                0.05203623450490319,
                                                0.0503727008434497},
                                               {0.05799794522838118,
                                                0.056562627404263835,
                                                0.05507465133709833,
                                                0.05354159428335702,
                                                0.05197150313175455,
                                                0.0503727008434497,
                                                0.04875359793440157}};
    for (size_t i = 0; i < Tf_values.size(); ++i) {
        for (size_t j = 0; j < Ta_values.size(); ++j) {
            EXPECT_NEAR(extendedHI::Ra_un(Tf_values[i], Ta_values[j]), result[i][j], tol);
        }
    }
}

TEST_F(EnergyPlusFixture, extendedHI_find_eqvar)
{
    Real64 tol = 1e-5;
    std::vector<double> Ta_values = {240, 260, 280, 300, 320, 340, 360};
    std::vector<double> RH_values = {0, 0.2, 0.4, 0.6, 0.8, 1.0};
    std::vector<std::vector<std::string>> result_0 = {{"Rf", "Rf", "Rf", "Rf", "Rf", "Rf"},
                                                      {"Rf", "Rf", "Rf", "Rf", "Rf", "Rf"},
                                                      {"Rf", "Rf", "Rf", "Rf", "Rf", "Rf"},
                                                      {"Rf", "Rs", "Rs", "Rs", "Rs", "Rs"},
                                                      {"Rs", "Rs", "dTcdt", "dTcdt", "dTcdt", "dTcdt"},
                                                      {"Rs", "dTcdt", "dTcdt", "dTcdt", "dTcdt", "dTcdt"},
                                                      {"Rs*", "dTcdt", "dTcdt", "dTcdt", "dTcdt", "dTcdt"}};
    Real64 result_1 = 0.84;
    std::vector<std::vector<double>> result_2 = {
        {33.04275493601435, 32.695633042229204, 32.355420488419746, 32.02191338684983, 31.694909211003484, 31.374226422935042},
        {1.5604330579759274, 1.546333295029165, 1.5323814066211647, 1.518575393945302, 1.504913229554858, 1.491392978806081},
        {0.4424813164108615, 0.42287513206662247, 0.40403359558586455, 0.3859354349292241, 0.3685598262740109, 0.35188630624297995},
        {0.011072874985460073, 0.0, 0.0, 0.0, 0.0, 0.0},
        {0.0, 0.0, 0.0, 0.0, 0.0, 0.0},
        {0.0, 0.0, 0.0, 0.0, 0.0, 0.0},
        {0.0, 0.0, 0.0, 0.0, 0.0, 0.0}};
    std::vector<std::vector<double>> result_3 = {
        {0.0387, 0.0387, 0.0387, 0.0387, 0.0387, 0.0387},
        {0.0387, 0.0387, 0.0387, 0.0387, 0.0387, 0.0387},
        {0.0387, 0.0387, 0.0387, 0.0387, 0.0387, 0.0387},
        {0.0387, 0.038212662537193985, 0.03685000138053738, 0.03532725311254958, 0.033563489978529426, 0.031401997100220014},
        {0.026992716499180645, 0.02259372764887033, 0.0, 0.0, 0.0, 0.0},
        {0.02072684837877413, 0.0, 0.0, 0.0, 0.0, 0.0},
        {0.0014681498999064508, 0.0, 0.0, 0.0, 0.0, 0.0}};
    std::vector<std::vector<double>> result_4 = {
        {0.0, 0.0, 0.0, 0.0, 0.0, 0.0},
        {0.0, 0.0, 0.0, 0.0, 0.0, 0.0},
        {0.0, 0.0, 0.0, 0.0, 0.0, 0.0},
        {0.0, 0.0, 0.0, 0.0, 0.0, 0.0},
        {0.0, 0.0, 0.00048496625767469157, 0.003383747126335647, 0.006282527994996603, 0.00918130886365756},
        {0.0, 0.004663667586060751, 0.012131060331269492, 0.019598453076478232, 0.02706584582168697, 0.034533238566895705},
        {0.0, 0.016910954386734945, 0.03395453755108459, 0.05099812071543423, 0.06804170387978387, 0.0850852870441335}};

    for (size_t i = 0; i < Ta_values.size(); ++i) {
        for (size_t j = 0; j < RH_values.size(); ++j) {
            auto const output = extendedHI::find_eqvar(*state, Ta_values[i], RH_values[j]);
            EXPECT_EQ(std::get<0>(output), result_0[i][j]);
            EXPECT_EQ(std::get<1>(output), result_1);
            EXPECT_NEAR(std::get<2>(output), result_2[i][j], tol);
            EXPECT_NEAR(std::get<3>(output), result_3[i][j], tol);
            EXPECT_NEAR(std::get<4>(output), result_4[i][j], tol);
        }
    }
}

TEST_F(EnergyPlusFixture, extendedHI_find_T)
{
    Real64 tol = 1e-7;
    state->dataRootFinder->HVACSystemRootFinding.HVACSystemRootSolver = HVACSystemRootSolverAlgorithm::Bisection;
    std::vector<double> Rf_values = {30, 32, 34, 36, 38};
    std::vector<double> result_0_rf = {240.06754042464308, 239.971123727737, 239.88581076147966, 239.8097881616559, 239.7416166716721};
    for (size_t i = 0; i < Rf_values.size(); ++i) {
        auto const output = extendedHI::find_T(*state, "Rf", Rf_values[i]);
        EXPECT_EQ(std::get<1>(output), "II");
        EXPECT_NEAR(std::get<0>(output), result_0_rf[i], tol);
    }
    std::vector<double> Rs_values = {0.01, 0.02, 0.03};
    std::vector<double> result_0_rs = {337.8696502133971, 329.7586998442421, 307.4815719091566};
    tol = 1e-4;
    for (size_t i = 0; i < Rs_values.size(); ++i) {
        auto const output = extendedHI::find_T(*state, "Rs", Rs_values[i]);
        EXPECT_EQ(std::get<1>(output), "IV");
        EXPECT_NEAR(std::get<0>(output), result_0_rs[i], tol);
    }
    // fixme: this one has large diff, 347 vs 350, because of the difference in the root solvers between EnergyPlus and the heatindex.py code by Lu
    // and Romps.
    // auto const output = extendedHI::find_T(*state, "Rs", 349.99999999359716); EXPECT_NEAR(std::get<0>(extendedHI::find_T(*state, "Rs",
    // 0.0), result_0_rs[i], tol);
}

TEST_F(EnergyPlusFixture, extendedHI_heatindex)
{

    // fixme: not finished
    state->dataRootFinder->HVACSystemRootFinding.HVACSystemRootSolver = HVACSystemRootSolverAlgorithm::Bisection;
    std::vector<std::vector<double>> HI_values = {{199.9994020652, 199.9997010342, 200.0000000021},
                                                  {209.9975943902, 209.9987971085, 209.9999998068},
                                                  {219.9915822029, 219.9957912306, 219.9999999912},
                                                  {229.9739691979, 229.9869861009, 230.0000001850},
                                                  {239.9253828022, 239.9626700074, 240.0000000003},
                                                  {249.7676757244, 249.8837049107, 250.0000000037},
                                                  {259.3735990024, 259.6864068902, 259.9999999944},
                                                  {268.5453870455, 269.2745889562, 270.0000002224},
                                                  {277.2234200026, 278.6369451963, 280.0000000091},
                                                  {285.7510545370, 288.2813660100, 290.7860610129},
                                                  {297.5737503539, 300.2922595865, 305.3947127590},
                                                  {305.5549530893, 318.6225524695, 359.9063248191},
                                                  {313.0298872791, 359.0538750602, 407.5345212438},
                                                  {320.5088548469, 398.5759733823, 464.9949352940},
                                                  {328.0358006469, 445.8599463105, 530.5524786708},
                                                  {333.2806160592, 500.0421800191, 601.9518435268},
                                                  {343.6312984164, 559.6640227151, 677.2462089759},
                                                  {354.1825692377, 623.1960299857, 755.0832658147}};

    std::vector<double> T_values = {200, 210, 220, 230, 240, 250, 260, 270, 280, 290, 300, 310, 320, 330, 340, 350, 360, 370};
    std::vector<double> RH_values = {0, 0.5, 1};

    Real64 tol = 1e-4;

    // fixme, this one has issue: extendedHI::heatindex(*state, 310, 0.5, false);
    for (size_t i = 0; i < T_values.size(); ++i) {
        for (size_t j = 0; j < RH_values.size(); ++j) {
            Real64 HI = HI_values[i][j];
            fmt::print("T={}, RH={}, HI={}\n", T_values[i], RH_values[j], extendedHI::heatindex(*state, T_values[i], RH_values[j], true));
            EXPECT_NEAR(extendedHI::heatindex(*state, T_values[i], RH_values[j], false), HI, tol);
        }
    }
}

// helper: calculating HI the old way
Real64 calcHI(Real64 ZoneTF, Real64 ZoneRH)
{
    Real64 HI;
    Real64 constexpr c1 = -42.379;
    Real64 constexpr c2 = 2.04901523;
    Real64 constexpr c3 = 10.14333127;
    Real64 constexpr c4 = -.22475541;
    Real64 constexpr c5 = -.00683783;
    Real64 constexpr c6 = -.05481717;
    Real64 constexpr c7 = .00122874;
    Real64 constexpr c8 = .00085282;
    Real64 constexpr c9 = -.00000199;
    if (ZoneTF < 80) {
        HI = 0.5 * (ZoneTF + 61.0 + (ZoneTF - 68.0) * 1.2 + (ZoneRH * 0.094));
    } else {
        HI = c1 + c2 * ZoneTF + c3 * ZoneRH + c4 * ZoneTF * ZoneRH + c5 * ZoneTF * ZoneTF + c6 * ZoneRH * ZoneRH + c7 * ZoneTF * ZoneTF * ZoneRH +
             c8 * ZoneTF * ZoneRH * ZoneRH + c9 * ZoneTF * ZoneTF * ZoneRH * ZoneRH;
        if (ZoneRH < 13 && ZoneTF < 112) {
            HI -= (13 - ZoneRH) / 4 * std::sqrt((17 - abs(ZoneTF - 95)) / 17);
        } else if (ZoneRH > 85 && ZoneTF < 87) {
            HI += (ZoneRH - 85) / 10 * (87 - ZoneTF) / 5;
        }
    }
    HI = (HI - 32.0) * (5.0 / 9.0);
    return HI;
}

constexpr Real64 Fahrenheit2Celsius(Real64 F)
{
    return (F - 32.0) * 5.0 / 9.0;
}

// compare different the old and new HI method
TEST_F(EnergyPlusFixture, extendedHI_heatindex_compare)
{
    //    uncomment the following to write out outputs comparing the old HI and the extended HI
    //    for (int T = 80; T < 112; T += 2) {
    //        for (int RH_percent = 40; RH_percent < 105; RH_percent += 5) {
    //            fmt::print("{},{},{},{}\n", T, Fahrenheit2Celsius(T) + 273.15, RH_percent, calcHI(T * 1.0, RH_percent) + 273.15);
    //        }
    //    }
    //
    //    fmt::print("--------------------------extended HI------------------------------\n");
    //
    //    for (int T = 80; T < 112; T += 2) {
    //        for (int RH_percent = 40; RH_percent < 105; RH_percent += 5) {
    //            fmt::print("{},{},{},{}\n",
    //                       T,
    //                       Fahrenheit2Celsius(T) + 273.15,
    //                       RH_percent,
    //                       extendedHI::heatindex(*state, Fahrenheit2Celsius(T) + 273.15, RH_percent / 100.0, false));
    //        }
    //    }

    std::vector<std::vector<Real64>> HI_values = {{299.50263955106493,
                                                   299.776740099187,
                                                   300.0697403535014,
                                                   300.3840374486754,
                                                   300.7224627089454,
                                                   301.0883855406428,
                                                   301.48585129820276,
                                                   301.91976206202526,
                                                   302.39612058678176,
                                                   302.9223619942786,
                                                   303.50781721121166,
                                                   304.164366737823,
                                                   304.90738965163473},
                                                  {300.7371586287627,
                                                   301.08082855993416,
                                                   301.45165138470475,
                                                   301.85347046062816,
                                                   302.29090810229536,
                                                   302.7695771184517,
                                                   303.2963640097296,
                                                   303.8798186200438,
                                                   304.5306942932075,
                                                   305.2627171872882,
                                                   306.0937017010292,
                                                   307.0472131000133,
                                                   308.15511379914824},
                                                  {302.02736771141645,
                                                   302.4561810697196,
                                                   302.92349559196737,
                                                   303.4353941195877,
                                                   303.9993409969611,
                                                   304.62460199545603,
                                                   305.322829571669,
                                                   306.10889266303275,
                                                   307.00207884714473,
                                                   308.027883155155,
                                                   309.2207373859128,
                                                   313.0628398671979,
                                                   321.5848247316899},
                                                  {303.3841495186789,
                                                   303.9170689758612,
                                                   304.50409744807985,
                                                   305.1547874516109,
                                                   305.88112467259634,
                                                   306.69836198852863,
                                                   307.62622559734154,
                                                   308.69069155014586,
                                                   309.9266883142991,
                                                   311.38231908960734,
                                                   318.54246110946406,
                                                   328.0043066147482,
                                                   333.3393972698832},
                                                  {304.8199354641838,
                                                   305.48032203514595,
                                                   306.21634121227544,
                                                   307.0429394330131,
                                                   307.97934497182723,
                                                   309.05072723689955,
                                                   310.29069143289234,
                                                   311.7451353772776,
                                                   313.95530016103294,
                                                   323.83440227771644,
                                                   332.4701096833451,
                                                   334.90678639907856,
                                                   337.31894975469913},
                                                  {306.3492841011612,
                                                   307.16624582593795,
                                                   308.0886627669679,
                                                   309.13992447021883,
                                                   310.35097800020594,
                                                   311.7636495939223,
                                                   313.43586634553503,
                                                   317.6494802389061,
                                                   328.3628307696199,
                                                   333.74059265770484,
                                                   336.3222867687,
                                                   338.8761290133698,
                                                   341.4020377426641},
                                                  {307.98957814869937,
                                                   308.99980285495985,
                                                   310.15708861581516,
                                                   311.49815879471134,
                                                   313.0731694836868,
                                                   314.95241500844713,
                                                   320.7787010347238,
                                                   331.8317274673609,
                                                   334.83416476810817,
                                                   337.5699159357464,
                                                   340.2740648871986,
                                                   342.94652084296104,
                                                   345.6054543473874},
                                                  {309.7619108116487,
                                                   311.0122043680167,
                                                   312.46811247605365,
                                                   314.187658362207,
                                                   316.2529625179013,
                                                   323.0801825976232,
                                                   332.79726637585554,
                                                   335.7326857879525,
                                                   338.63242015999276,
                                                   341.4963439834537,
                                                   344.32435625989456,
                                                   347.16610411327565,
                                                   349.9882094984059},
                                                  {311.6922567837173,
                                                   313.2431093422929,
                                                   315.0828601612011,
                                                   317.30399334745016,
                                                   324.3992667720886,
                                                   333.3013983833371,
                                                   336.4164381631417,
                                                   339.4910959439585,
                                                   342.5252268166514,
                                                   345.53482289920794,
                                                   348.55059671303025,
                                                   351.53749776567565,
                                                   354.49586372385966},
                                                  {313.81305196962785,
                                                   315.74373047973495,
                                                   318.0832217884017,
                                                   324.6363173221471,
                                                   333.556578377611,
                                                   336.86389599752147,
                                                   340.12557708716486,
                                                   343.3414515358163,
                                                   346.5464459863142,
                                                   349.74051965837134,
                                                   352.90212786378106,
                                                   356.0316775160027,
                                                   359.1295787363197},
                                                  {316.1653665843187,
                                                   318.58128181367647,
                                                   323.6855917231878,
                                                   333.5378166526789,
                                                   337.0514521509176,
                                                   340.51358119759243,
                                                   343.9240011043148,
                                                   347.3324049610528,
                                                   350.715991469624,
                                                   354.063080122869,
                                                   357.3741601619986,
                                                   360.6497245162609,
                                                   363.89026874647243},
                                                  {318.80192411888856,
                                                   321.8453275115462,
                                                   333.2175307610305,
                                                   336.9530971109634,
                                                   340.63061076856684,
                                                   344.2498254548991,
                                                   347.8701384467422,
                                                   351.4554844770464,
                                                   354.99980266642524,
                                                   358.5036799122463,
                                                   361.96770739537897,
                                                   365.39247917797184,
                                                   368.7785908780643},
                                                  {321.79129250056576,
                                                   332.5651462195674,
                                                   336.5400431206217,
                                                   340.4496004333487,
                                                   344.2935131903505,
                                                   348.13508995372104,
                                                   351.9356167348451,
                                                   355.6900213044719,
                                                   359.3990061376826,
                                                   363.0632788382354,
                                                   366.6835501792957,
                                                   370.2605324515025,
                                                   373.7949378686608},
                                                  {329.9497329717269,
                                                   335.7802855706541,
                                                   339.9405041738646,
                                                   344.0269053383963,
                                                   348.1004405542626,
                                                   352.130907458195,
                                                   356.1095161383855,
                                                   360.03710570017574,
                                                   363.9145213572192,
                                                   367.74261204962386,
                                                   371.5222280813032,
                                                   375.2542191799148,
                                                   378.9394325375906},
                                                  {334.63809705979656,
                                                   339.06981446722057,
                                                   343.41864144138526,
                                                   347.7367934017093,
                                                   352.013488568773,
                                                   356.2318584616878,
                                                   360.3929031532607,
                                                   364.49763041891856,
                                                   368.54705243284116,
                                                   372.5421826945967,
                                                   376.48403349361615,
                                                   380.3736132776248,
                                                   384.2119247702067},
                                                  {337.80000485421624,
                                                   342.43363185960334,
                                                   347.0118012771127,
                                                   351.55276412755484,
                                                   356.0280998123926,
                                                   360.438997685269,
                                                   364.78665706905304,
                                                   369.07228278025286,
                                                   373.2970810952247,
                                                   377.4622562734294,
                                                   381.56900731119094,
                                                   385.6185252717114,
                                                   389.61199074954493}};

    std::vector<Real64> T_values;
    for (int i = 0; i < 16; ++i) {
        T_values.push_back(80 + 2 * i);
    }
    std::vector<Real64> RH_values;
    for (int i = 0; i < 13; ++i) {
        RH_values.push_back(40 + 5 * i);
    }

    Real64 extended;
    for (size_t i = 0; i < T_values.size(); ++i) {
        for (size_t j = 0; j < RH_values.size(); ++j) {
            extended = extendedHI::heatindex(*state, Fahrenheit2Celsius(T_values[i]) + 273.15, RH_values[j] / 100.0, false);
            EXPECT_NEAR(HI_values[i][j], extended, 1e-4);
        }
    }
}
