// EnergyPlus, Copyright (c) 1996-2021, The Board of Trustees of the University of Illinois,
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

#ifndef Psychrometrics_hh_INCLUDED
#define Psychrometrics_hh_INCLUDED

// C++ Headers
#include <cassert>
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/EnergyPlus.hh>
#include <EnergyPlus/PsychCacheData.hh>
#include <EnergyPlus/UtilityRoutines.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

#ifdef EP_nocache_Psychrometrics
#undef EP_cache_PsyTwbFnTdbWPb
#undef EP_cache_PsyPsatFnTemp
#undef EP_cache_PsyTsatFnPb
#undef EP_cache_PsyTsatFnHPb
#else
#define EP_cache_PsyTwbFnTdbWPb
#define EP_cache_PsyPsatFnTemp
#define EP_cache_PsyTsatFnPb
#define EP_cache_PsyTsatFnHPb
#endif
// Adapted from: https://www.fluentcpp.com/2019/08/30/how-to-disable-a-warning-in-cpp/
// clang-format off
#if defined(_MSC_VER)
#define DISABLE_WARNING_PUSH __pragma(warning(push))
#define DISABLE_WARNING_POP __pragma(warning(pop))
#define DISABLE_WARNING(warningNumber) __pragma(warning(disable : warningNumber))

// purposfully doing nothing here - does MSVC not have a strict-aliasing warning?
#define DISABLE_WARNING_STRICT_ALIASING
#elif defined(__GNUC__) || defined(__clang__)
#define DO_PRAGMA(X) _Pragma(#X)
#define DISABLE_WARNING_PUSH DO_PRAGMA(GCC diagnostic push)
#define DISABLE_WARNING_POP DO_PRAGMA(GCC diagnostic pop)
#define DISABLE_WARNING(warningName) DO_PRAGMA(GCC diagnostic ignored #warningName)

#define DISABLE_WARNING_STRICT_ALIASING DISABLE_WARNING(-Wstrict-aliasing)
#endif
// clang-format on

namespace Psychrometrics {

    // Data
    // MODULE PARAMETER DEFINITIONS:
    // call for recurring errors
    constexpr int iPsyTdpFnTdbTwbPb = 1;
    constexpr int iPsyRhFnTdbWPb = 2;
    constexpr int iPsyTwbFnTdbWPb = 3;
    constexpr int iPsyTwbFnTdbWPb2 = 14;
    constexpr int iPsyTwbFnTdbWPb3 = 15; // convergence
    constexpr int iPsyVFnTdbWPb = 4;
    constexpr int iPsyWFnTdpPb = 5;
    constexpr int iPsyWFnTdbH = 6;
    constexpr int iPsyWFnTdbTwbPb = 7;
    constexpr int iPsyWFnTdbTwbPb2 = 16;
    constexpr int iPsyWFnTdbRhPb = 8;
    constexpr int iPsyPsatFnTemp = 9;
    constexpr int iPsyTsatFnHPb = 10;
    constexpr int iPsyTsatFnPb = 11;
    constexpr int iPsyTsatFnPb2 = 17; // iterations
    constexpr int iPsyRhFnTdbRhov = 12;
    constexpr int iPsyRhFnTdbRhovLBnd0C = 13;
    constexpr int iPsyTwbFnTdbWPb_cache = 18;
    constexpr int iPsyPsatFnTemp_cache = 19;
    // sample bin =64 Pa; sample size =318
    // constexpr std::array<Real64, 318> tsat_fn_pb_x = {}; // pressure
    // constexpr std::array<Real64, 318> tsat_fn_pb_y = {}; //tsat
    // constexpr std::array<Real64, 318> tsat_fn_pb_d2y = {};
    // sample bin size =128 Pa; sample size =161
    // constexpr std::array<Real64, 161> tsat_fn_pb_x = {}; // pressure
    // constexpr std::array<Real64, 161> tsat_fn_pb_y = {}; // tsat
    // constexpr std::array<Real64, 161> tsat_fn_pb_d2y = {};
    // sample bin size =256 Pa; sample size =82
    // constexpr std::array<Real64, 82> tsat_fn_pb_x = {}; // pressure
    // constexpr std::array<Real64, 82> tsat_fn_pb_y = {}; // tsat
    // constexpr std::array<Real64, 82> tsat_fn_pb_d2y = {};
    // sample bin size =512 Pa; sample size =52 (two sample sets)
    constexpr std::array<Real64, 52> tsat_fn_pb_x = {0,      512,    1024,   1536,   2048,   2560,   3072,   3584,   4096,   4608,   5120,
                                                     5632,   6144,   6656,   7168,   7680,   8192,   8704,   9216,   9728,   10240,  10752,
                                                     95744,  96256,  96768,  97280,  97792,  98304,  98816,  99328,  99840,  100352, 100864,
                                                     101376, 101888, 102400, 102912, 103424, 103936, 104448, 104960, 105472, 105984, 106496,
                                                     107008, 107520, 108032, 108544, 109056, 109568, 110080, 110592};
    constexpr std::array<Real64, 52> tsat_fn_pb_y = {
        -100,        -2.132789207, 7.317464071, 13.38541367, 17.87411665, 21.46712703, 24.4784194,  27.07951029, 29.37480614,
        31.43278471, 33.30089772,  35.01342775, 36.59599334, 38.06825698, 39.44565725, 40.74054192, 41.96295168, 43.12116511,
        44.22209605, 45.27156964,  46.27455279, 47.23530984, 98.39463599, 98.54266935, 98.69005,    98.83678649, 98.98288655,
        99.12834992, 99.2731896,   99.41740889, 99.56101346, 99.70400948, 99.84640288, 99.98819959, 100.1294041, 100.2700192,
        100.4100569, 100.5495203,  100.6884104, 100.8267359, 100.9645009, 101.101711,  101.2383738, 101.3744899, 101.5100639,
        101.6451069, 101.7796152,  101.9136007, 102.0470595, 102.180006,  102.3124366, 102.4443601};
    constexpr std::array<Real64, 52> tsat_fn_pb_d2y = {
        0.000310208,  -0.000620417, 0.000147756,  -4.80202E-05, 0.000008179, -5.1967E-06, -7.069E-07, -1.3646E-06,  -8.338E-07,
        -0.000000732, -0.000000584, -4.932E-07,   -4.178E-07,   -0.00000036, -3.133E-07,  -2.754E-07, -0.000000244, -2.178E-07,
        -1.958E-07,   -1.769E-07,   -1.607E-07,   -1.467E-07,   -2.5E-09,    -2.5E-09,    -2.5E-09,   -2.4E-09,     -2.4E-09,
        -2.4E-09,     -2.4E-09,     -2.3E-09,     -2.3E-09,     -2.3E-09,    -2.3E-09,    -2.3E-09,   -2.3E-09,     -2.2E-09,
        -2.2E-09,     -2.2E-09,     -2.1E-09,     -2.1E-09,     -2.1E-09,    -2.1E-09,    -2.1E-09,   -2.1E-09,     -0.000000002,
        -2.1E-09,     -0.000000002, -0.000000002, -0.000000002, -1.8E-09,    -2.8E-09,    1.4E-09};
    // sample bin size =512 Pa; sample size =217 (continous)
    // constexpr std::array<Real64, 217> tsat_fn_pb_x = {
    //    0,      512,    1024,   1536,   2048,   2560,   3072,   3584,   4096,   4608,   5120,   5632,   6144,   6656,   7168,   7680,   8192,
    //    8704,   9216,   9728,   10240,  10752,  11264,  11776,  12288,  12800,  13312,  13824,  14336,  14848,  15360,  15872,  16384,  16896,
    //    17408,  17920,  18432,  18944,  19456,  19968,  20480,  20992,  21504,  22016,  22528,  23040,  23552,  24064,  24576,  25088,  25600,
    //    26112,  26624,  27136,  27648,  28160,  28672,  29184,  29696,  30208,  30720,  31232,  31744,  32256,  32768,  33280,  33792,  34304,
    //    34816,  35328,  35840,  36352,  36864,  37376,  37888,  38400,  38912,  39424,  39936,  40448,  40960,  41472,  41984,  42496,  43008,
    //    43520,  44032,  44544,  45056,  45568,  46080,  46592,  47104,  47616,  48128,  48640,  49152,  49664,  50176,  50688,  51200,  51712,
    //    52224,  52736,  53248,  53760,  54272,  54784,  55296,  55808,  56320,  56832,  57344,  57856,  58368,  58880,  59392,  59904,  60416,
    //    60928,  61440,  61952,  62464,  62976,  63488,  64000,  64512,  65024,  65536,  66048,  66560,  67072,  67584,  68096,  68608,  69120,
    //    69632,  70144,  70656,  71168,  71680,  72192,  72704,  73216,  73728,  74240,  74752,  75264,  75776,  76288,  76800,  77312,  77824,
    //    78336,  78848,  79360,  79872,  80384,  80896,  81408,  81920,  82432,  82944,  83456,  83968,  84480,  84992,  85504,  86016,  86528,
    //    87040,  87552,  88064,  88576,  89088,  89600,  90112,  90624,  91136,  91648,  92160,  92672,  93184,  93696,  94208,  94720,  95232,
    //    95744,  96256,  96768,  97280,  97792,  98304,  98816,  99328,  99840,  100352, 100864, 101376, 101888, 102400, 102912, 103424, 103936,
    //    104448, 104960, 105472, 105984, 106496, 107008, 107520, 108032, 108544, 109056, 109568, 110080, 110592}; // pressure

    // constexpr std::array<Real64, 217> tsat_fn_pb_y = {
    //    -100,        -2.132789207, 7.317464071, 13.38541367, 17.87411665, 21.46712703, 24.4784194,  27.07951029, 29.37480614, 31.43278471,
    //    33.30089772, 35.01342775,  36.59599334, 38.06825698, 39.44565725, 40.74054192, 41.96295168, 43.12116511, 44.22209605, 45.27156964,
    //    46.27455279, 47.23530984,  48.15753313, 49.04443945, 49.89885468, 50.72326533, 51.51988223, 52.29067211, 53.03739768, 53.76164079,
    //    54.46483065, 55.14826341,  55.8131092,  56.46044318, 57.09124313, 57.70640793, 58.30675724, 58.89305587, 59.46600373, 60.02624654,
    //    60.57438886, 61.11098811,  61.6365656,  62.15160576, 62.65655972, 63.15185221, 63.63787789, 64.11501608, 64.5836074,  65.04398502,
    //    65.49645605, 65.94131694,  66.37884177, 66.80929006, 67.23291261, 67.64993655, 68.06059149, 68.46508104, 68.86360503, 69.25636202,
    //    69.64352333, 70.02526447,  70.40175094, 70.77314,    71.13958183, 71.50121362, 71.85817753, 72.21060503, 72.5586171,  72.90233649,
    //    73.24187731, 73.57735287,  73.90886374, 74.23651553, 74.56040515, 74.88062483, 75.19726977, 75.51042254, 75.82016525, 76.12658418,
    //    76.42975531, 76.72975292,  77.02664946, 77.32051481, 77.6114198,  77.89942204, 78.18459127, 78.46698385, 78.746663,   79.02368438,
    //    79.29810401, 79.56997224,  79.8393431,  80.10626466, 80.37079099, 80.63296434, 80.89282813, 81.15043579, 81.40582266, 81.65902927,
    //    81.91010465, 82.15908193,  82.40600156, 82.65090102, 82.89381203, 83.13477865, 83.37382643, 83.61099411, 83.84631261, 84.07981307,
    //    84.31152889, 84.54148605,  84.76971427, 84.99624746, 85.22110697, 85.44432122, 85.66592225, 85.88592674, 86.1043677,  86.32126306,
    //    86.53664208, 86.75052605,  86.96294,    87.17390292, 87.38343463, 87.59156283, 87.79830413, 88.00367984, 88.2077092,  88.41041472,
    //    88.61180925, 88.81191947,  89.01075422, 89.20834007, 89.40468873, 89.59981659, 89.7937454,  89.98648709, 90.17806217, 90.36848137,
    //    90.55776192, 90.74592103,  90.93297058, 91.11892527, 91.30379877, 91.48760533, 91.67036098, 91.85207509, 92.03276627, 92.21243853,
    //    92.39111284, 92.56880174,  92.74550864, 92.92125288, 93.09604538, 93.26989246, 93.44281016, 93.61480953, 93.78590211, 93.95609091,
    //    94.12539575, 94.29382356,  94.46138296, 94.6280858,  94.79393861, 94.95895446, 95.12314186, 95.28651202, 95.449067,   95.61082454,
    //    95.77178414, 95.93196106,  96.09136342, 96.24999892, 96.40787474, 96.5649979,  96.72137804, 96.87702132, 97.03194067, 97.18613698,
    //    97.33962005, 97.49239649,  97.64447612, 97.79586097, 97.94656357, 98.09659062, 98.24594464, 98.39463599, 98.54266935, 98.69005,
    //    98.83678649, 98.98288655,  99.12834992, 99.2731896,  99.41740889, 99.56101346, 99.70400948, 99.84640288, 99.98819959, 100.1294041,
    //    100.2700192, 100.4100569,  100.5495203, 100.6884104, 100.8267359, 100.9645009, 101.101711,  101.2383738, 101.3744899, 101.5100639,
    //    101.6451069, 101.7796152,  101.9136007, 102.0470595, 102.180006,  102.3124366, 102.4443601
    //    }; // tsat
    // constexpr std::array<Real64, 217> tsat_fn_pb_d2y = {
    //    0.000310208,  -0.000620417, 0.000147756,  -4.80E-05,    0.000008179,  -5.20E-06,    -7.07E-07,    -1.36E-06,    -8.34E-07,    -0.000000732,
    //    -0.000000584, -4.93E-07,    -4.18E-07,    -0.00000036,  -3.13E-07,    -2.75E-07,    -0.000000244, -2.18E-07,    -1.96E-07,    -1.77E-07,
    //    -1.61E-07,    -1.47E-07,    -1.35E-07,    -1.24E-07,    -1.14E-07,    -1.06E-07,    -9.84E-08,    -9.17E-08,    -8.57E-08,    -8.02E-08,
    //    -7.53E-08,    -7.09E-08,    -6.67E-08,    -0.000000063, -5.96E-08,    -5.65E-08,    -5.36E-08,    -5.09E-08,    -4.84E-08,    -4.61E-08,
    //    -0.000000044, -0.000000042, -4.02E-08,    -3.85E-08,    -3.68E-08,    -3.54E-08,    -3.39E-08,    -3.26E-08,    -3.13E-08,    -3.02E-08,
    //    -0.000000029, -0.000000028, -0.000000027, -0.000000026, -2.52E-08,    -2.43E-08,    -2.35E-08,    -2.28E-08,    -0.000000022, -2.14E-08,
    //    -2.07E-08,    -0.00000002,  -1.94E-08,    -1.89E-08,    -1.84E-08,    -1.78E-08,    -1.73E-08,    -1.69E-08,    -1.64E-08,    -1.59E-08,
    //    -1.55E-08,    -1.51E-08,    -1.47E-08,    -1.43E-08,    -0.000000014, -1.36E-08,    -1.33E-08,    -0.000000013, -1.27E-08,    -1.24E-08,
    //    -1.21E-08,    -1.18E-08,    -1.16E-08,    -1.13E-08,    -1.11E-08,    -1.08E-08,    -1.06E-08,    -1.03E-08,    -1.01E-08,    -9.90E-09,
    //    -9.70E-09,    -9.50E-09,    -9.40E-09,    -9.10E-09,    -0.000000009, -8.80E-09,    -8.60E-09,    -8.50E-09,    -8.30E-09,    -8.10E-09,
    //    -0.000000008, -7.80E-09,    -7.70E-09,    -7.60E-09,    -7.40E-09,    -7.30E-09,    -7.20E-09,    -7.10E-09,    -6.90E-09,    -6.80E-09,
    //    -6.70E-09,    -6.60E-09,    -6.50E-09,    -6.40E-09,    -6.30E-09,    -6.10E-09,    -6.10E-09,    -5.90E-09,    -5.90E-09,    -5.80E-09,
    //    -5.70E-09,    -5.60E-09,    -5.50E-09,    -5.50E-09,    -5.30E-09,    -5.30E-09,    -5.20E-09,    -5.10E-09,    -0.000000005, -0.000000005,
    //    -4.90E-09,    -4.90E-09,    -4.70E-09,    -4.70E-09,    -4.70E-09,    -4.60E-09,    -4.50E-09,    -4.40E-09,    -4.40E-09,    -4.30E-09,
    //    -4.30E-09,    -4.20E-09,    -4.20E-09,    -4.10E-09,    -4.10E-09,    -0.000000004, -0.000000004, -3.90E-09,    -3.90E-09,    -3.80E-09,
    //    -3.70E-09,    -3.80E-09,    -3.70E-09,    -3.60E-09,    -3.60E-09,    -3.50E-09,    -3.50E-09,    -3.40E-09,    -3.50E-09,    -3.40E-09,
    //    -3.40E-09,    -3.30E-09,    -3.30E-09,    -3.30E-09,    -3.20E-09,    -3.20E-09,    -3.10E-09,    -3.10E-09,    -0.000000003, -3.10E-09,
    //    -0.000000003, -0.000000003, -2.90E-09,    -2.90E-09,    -2.90E-09,    -2.80E-09,    -2.80E-09,    -2.70E-09,    -2.80E-09,    -2.70E-09,
    //    -2.70E-09,    -2.60E-09,    -2.70E-09,    -2.60E-09,    -2.60E-09,    -2.60E-09,    -2.50E-09,    -2.50E-09,    -2.50E-09,    -2.50E-09,
    //    -2.40E-09,    -2.40E-09,    -2.40E-09,    -2.40E-09,    -2.30E-09,    -2.30E-09,    -2.30E-09,    -2.30E-09,    -2.30E-09,    -2.30E-09,
    //    -2.20E-09,    -2.20E-09,    -2.20E-09,    -2.10E-09,    -2.10E-09,    -2.10E-09,    -2.10E-09,    -2.10E-09,    -2.10E-09,    -0.000000002,
    //    -2.10E-09,    -0.000000002, -0.000000002, -0.000000002, -1.80E-09,    -2.80E-09,    1.40E-09};

#ifdef EP_psych_stats
    extern Array1D_string const PsyRoutineNames; // 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 - HR | 15 - max iter | 16 - HR | 17 -
                                                 // max iter | 18 - PsyTwbFnTdbWPb_raw (raw calc) | 19 - PsyPsatFnTemp_raw (raw calc)

    extern Array1D_bool const PsyReportIt; // PsyTdpFnTdbTwbPb     1 | PsyRhFnTdbWPb        2 | PsyTwbFnTdbWPb       3 | PsyVFnTdbWPb         4 |
                                           // PsyWFnTdpPb          5 | PsyWFnTdbH           6 | PsyWFnTdbTwbPb       7 | PsyWFnTdbRhPb        8 |
                                           // PsyPsatFnTemp        9 | PsyTsatFnHPb         10 | PsyTsatFnPb          11 | PsyRhFnTdbRhov       12 |
                                           // PsyRhFnTdbRhovLBnd0C 13 | PsyTwbFnTdbWPb       14 - HR | PsyTwbFnTdbWPb       15 - max iter |
                                           // PsyWFnTdbTwbPb       16 - HR | PsyTsatFnPb          17 - max iter | PsyTwbFnTdbWPb_cache 18 -
                                           // PsyTwbFnTdbWPb_raw (raw calc) | PsyPsatFnTemp_cache  19 - PsyPsatFnTemp_raw (raw calc)
#endif

#ifndef EP_psych_errors
#endif

#ifdef EP_cache_PsyTwbFnTdbWPb
    constexpr int twbcache_size = 1024 * 1024;
    constexpr int twbprecision_bits = 20;
#endif
#ifdef EP_cache_PsyPsatFnTemp
    constexpr int psatcache_size = 1024 * 1024;
    constexpr int psatprecision_bits = 24; // 28  //24  //32
    constexpr Int64 psatcache_mask = psatcache_size - 1;
#endif
#ifdef EP_cache_PsyTsatFnPb
    constexpr int tsatcache_size = 1024 * 1024;
    constexpr int tsatprecision_bits = 24;
    constexpr Int64 tsatcache_mask = tsatcache_size - 1;
#endif
#ifdef EP_cache_PsyTsatFnHPb
    constexpr int tsat_hbp_cache_size = 1024 * 1024;
    constexpr int tsat_hbp_precision_bits = 28;
#endif

    void InitializePsychRoutines(EnergyPlusData &state);

    void ShowPsychrometricSummary(EnergyPlusData &state, InputOutputFile &auditFile);

#ifdef EP_psych_errors
    void PsyRhoAirFnPbTdbW_error(EnergyPlusData &state,
                                 Real64 const pb,                       // barometric pressure (Pascals)
                                 Real64 const tdb,                      // dry bulb temperature (Celsius)
                                 Real64 const dw,                       // humidity ratio (kgWater/kgDryAir)
                                 Real64 const rhoair,                   // density of air
                                 std::string_view const CalledFrom = "" // routine this function was called from (error messages) !unused1208
    );
#endif

    inline Real64 PsyRhoAirFnPbTdbW(
        [[maybe_unused]] EnergyPlusData &state,
        Real64 const pb,                                        // barometric pressure (Pascals)
        Real64 const tdb,                                       // dry bulb temperature (Celsius)
        Real64 const dw,                                        // humidity ratio (kgWater/kgDryAir)
        [[maybe_unused]] std::string_view const CalledFrom = "" // routine this function was called from (error messages) !unused1208
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         G. S. Wright
        //       DATE WRITTEN   June 2, 1994
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function provides density of air as a function of barometric
        // pressure, dry bulb temperature, and humidity ratio.

        // METHODOLOGY EMPLOYED:
        // ideal gas law
        //    universal gas const for air 287 J/(kg K)
        //    air/water molecular mass ratio 28.9645/18.01534

        // REFERENCES:
        // Wylan & Sontag, Fundamentals of Classical Thermodynamics.
        // ASHRAE handbook 1985 Fundamentals, Ch. 6, eqn. (6),(26)

        Real64 const rhoair(pb / (287.0 * (tdb + DataGlobalConstants::KelvinConv) * (1.0 + 1.6077687 * max(dw, 1.0e-5))));
#ifdef EP_psych_errors
        if (rhoair < 0.0) PsyRhoAirFnPbTdbW_error(state, pb, tdb, dw, rhoair, CalledFrom);
#endif
        return rhoair;
    }

    constexpr Real64 PsyRhoAirFnPbTdbW(Real64 const pb,  // barometric pressure (Pascals)
                                       Real64 const tdb, // dry bulb temperature (Celsius)
                                       Real64 const dw   // humidity ratio (kgWater/kgDryAir)
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         G. S. Wright
        //       DATE WRITTEN   June 2, 1994
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function provides density of air as a function of barometric
        // pressure, dry bulb temperature, and humidity ratio.

        // METHODOLOGY EMPLOYED:
        // ideal gas law
        //    universal gas const for air 287 J/(kg K)
        //    air/water molecular mass ratio 28.9645/18.01534

        // REFERENCES:
        // Wylan & Sontag, Fundamentals of Classical Thermodynamics.
        // ASHRAE handbook 1985 Fundamentals, Ch. 6, eqn. (6),(26)

        return (pb / (287.0 * (tdb + DataGlobalConstants::KelvinConv) * (1.0 + 1.6077687 * std::max(dw, 1.0e-5))));
    }

    inline Real64 PsyRhoAirFnPbTdbW_fast([[maybe_unused]] EnergyPlusData &state,
                                         Real64 const pb,  // barometric pressure (Pascals)
                                         Real64 const tdb, // dry bulb temperature (Celsius)
                                         Real64 const dw   // humidity ratio (kgWater/kgDryAir)
    )
    {
        // Faster version with humidity ratio already adjusted
        assert(dw >= 1.0e-5);
        Real64 const rhoair(pb / (287.0 * (tdb + DataGlobalConstants::KelvinConv) * (1.0 + 1.6077687 * dw)));
#ifdef EP_psych_errors
        if (rhoair < 0.0) PsyRhoAirFnPbTdbW_error(state, pb, tdb, dw, rhoair);
#endif
        return rhoair;
    }

    inline Real64 PsyHfgAirFnWTdb([[maybe_unused]] Real64 const w, // humidity ratio {kgWater/kgDryAir} !unused1208
                                  Real64 const T                   // input temperature {Celsius}
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Richard Liesen
        //       DATE WRITTEN   May, 2001
        //       MODIFIED       June, 2002
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function provides latent energy of air as function of humidity ratio and temperature.

        // METHODOLOGY EMPLOYED:
        // calculates hg and then hf and the difference is Hfg.

        // REFERENCES:
        // see ASHRAE Fundamentals Psychrometric Chapter
        // USAGE:  hfg = PsyHfgAirFnWTdb(w,T)

        // Return value
        // result => heat of vaporization for moist air {J/kg}

        // This formulation currently does not use W since it returns results that are in J/kg and the
        //  amount of energy is on a per unit of moisture basis.

        Real64 const Temperature(max(T, 0.0));                               // input temperature {Celsius} - corrected for >= 0C
        return (2500940.0 + 1858.95 * Temperature) - (4180.0 * Temperature); // enthalpy of the gas - enthalpy of the fluid
    }

    inline Real64 PsyHgAirFnWTdb([[maybe_unused]] Real64 const w, // humidity ratio {kgWater/kgDryAir} !unused1208
                                 Real64 const T                   // input temperature {Celsius}
    )
    {

        // FUNCTION INFORMATION:
        //       AUTHOR         Richard Liesen
        //       DATE WRITTEN   May, 2001
        //       MODIFIED       June, 2002
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function provides latent energy of the moisture as a gas in the air as
        // function of humidity ratio and temperature.

        // REFERENCES:
        // see ASHRAE Fundamentals Psychrometric Chapter
        // USAGE:  hg = PsyHgAirFnWTdb(w,T)

        // This formulation currently does not use W since it returns results that are in J/kg and the
        //  amount of energy is on a per unit of moisture basis.

        return 2500940.0 + 1858.95 * T; // enthalpy of the gas {units?}
    }

    inline Real64 PsyHFnTdbW(Real64 const TDB, // dry-bulb temperature {C}
                             Real64 const dW   // humidity ratio
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         George Shih
        //       DATE WRITTEN   May 1976
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function calculates the enthalpy {J/kg} from dry-bulb temperature and humidity ratio.

        // REFERENCES:
        // ASHRAE HANDBOOK OF FUNDAMENTALS, 1972, P100, EQN 32

        // calculate enthalpy
        return 1.00484e3 * TDB + max(dW, 1.0e-5) * (2.50094e6 + 1.85895e3 * TDB); // enthalpy {J/kg}
    }

    inline Real64 PsyHFnTdbW_fast(Real64 const TDB, // dry-bulb temperature {C}
                                  Real64 const dW   // humidity ratio
    )
    {
        // Faster version with humidity ratio already adjusted
        assert(dW >= 1.0e-5);

        // calculate enthalpy
        return 1.00484e3 * TDB + dW * (2.50094e6 + 1.85895e3 * TDB); // enthalpy {J/kg}
    }

    inline Real64 PsyCpAirFnW(Real64 const dw // humidity ratio {kgWater/kgDryAir}
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         J. C. VanderZee
        //       DATE WRITTEN   Feb. 1994
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function provides the heat capacity of air {J/kg-C} as function of humidity ratio.

        // METHODOLOGY EMPLOYED:
        // take numerical derivative of PsyHFnTdbW function

        // REFERENCES:
        // see PsyHFnTdbW ref. to ASHRAE Fundamentals
        // USAGE:  cpa = PsyCpAirFnW(w)

        // Static locals
        static Real64 dwSave(-100.0);
        static Real64 cpaSave(-100.0);

        // check if last call had the same input and if it did just use the saved output
        if (dwSave == dw) return cpaSave;

        // compute heat capacity of air
        Real64 const w(max(dw, 1.0e-5));
        Real64 const cpa((1.00484e3 + w * 1.85895e3)); // result => heat capacity of moist air {J/kg-C}

        // save values for next call
        dwSave = dw;
        cpaSave = cpa;

        return cpa;
    }

    inline Real64 PsyCpAirFnW_fast(Real64 const dw // humidity ratio {kgWater/kgDryAir}
    )
    {
        // Faster version with humidity ratio already adjusted
        assert(dw >= 1.0e-5);

        // Static locals
        static Real64 dwSave(-100.0);
        static Real64 cpaSave(-100.0);

        // check if last call had the same input and if it did just use the saved output
        if (dwSave == dw) return cpaSave;

        // compute heat capacity of air
        Real64 const cpa((1.00484e3 + dw * 1.85895e3)); // result => heat capacity of moist air {J/kg-C}

        // save values for next call
        dwSave = dw;
        cpaSave = cpa;

        return cpa;
    }

    inline Real64 PsyTdbFnHW(Real64 const H, // enthalpy {J/kg}
                             Real64 const dW // humidity ratio
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         J. C. VanderZee
        //       DATE WRITTEN   Feb. 1994
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function provides air temperature from enthalpy and humidity ratio.

        // REFERENCES:
        // ASHRAE HANDBOOK OF FUNDAMENTALS, 1972, P100, EQN 32
        //   by inverting function PsyHFnTdbW

        Real64 const W(max(dW, 1.0e-5));                          // humidity ratio
        return (H - 2.50094e6 * W) / (1.00484e3 + 1.85895e3 * W); // result=> dry-bulb temperature {C}
    }

    inline Real64 PsyRhovFnTdbRhLBnd0C(Real64 const Tdb, // dry-bulb temperature {C}
                                       Real64 const RH   // relative humidity value (0.0-1.0)
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         R. J. Liesen
        //       DATE WRITTEN   July 2000
        //       MODIFIED       Name change to signify derivation and temperatures were used
        //                      with 0C as minimum; LKL January 2008
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function provides the Vapor Density in air as a
        // function of dry bulb temperature, and Relative Humidity.

        // METHODOLOGY EMPLOYED:
        // ideal gas law
        // Universal gas const for water vapor 461.52 J/(kg K)

        // REFERENCES:
        // ASHRAE handbook 1993 Fundamentals,

        return RH / (461.52 * (Tdb + DataGlobalConstants::KelvinConv)) *
               std::exp(23.7093 - 4111.0 / ((Tdb + DataGlobalConstants::KelvinConv) - 35.45)); // Vapor density in air
    }

    inline Real64 PsyRhovFnTdbWPb(Real64 const Tdb, // dry-bulb temperature {C}
                                  Real64 const dW,  // humidity ratio
                                  Real64 const PB   // Barometric Pressure {Pascals}
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         R. J. Liesen
        //       DATE WRITTEN   July 2000
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function provides the Vapor Density in air as a
        // function of dry bulb temperature, Humidity Ratio, and Barometric Pressure.

        // METHODOLOGY EMPLOYED:
        // ideal gas law
        // Universal gas const for water vapor 461.52 J/(kg K)

        // REFERENCES:
        // ASHRAE handbook 1993 Fundamentals,

        Real64 const W(max(dW, 1.0e-5)); // humidity ratio
        return W * PB / (461.52 * (Tdb + DataGlobalConstants::KelvinConv) * (W + 0.62198));
    }

    inline Real64 PsyRhovFnTdbWPb_fast(Real64 const Tdb, // dry-bulb temperature {C}
                                       Real64 const dW,  // humidity ratio
                                       Real64 const PB   // Barometric Pressure {Pascals}
    )
    {
        // Faster version with humidity ratio already adjusted
        assert(dW >= 1.0e-5);
        return dW * PB / (461.52 * (Tdb + DataGlobalConstants::KelvinConv) * (dW + 0.62198));
    }

#ifdef EP_psych_errors
    void PsyRhFnTdbRhovLBnd0C_error(EnergyPlusData &state,
                                    Real64 const Tdb,                 // dry-bulb temperature {C}
                                    Real64 const Rhovapor,            // vapor density in air {kg/m3}
                                    Real64 const RHValue,             // relative humidity value (0.0-1.0)
                                    std::string_view const CalledFrom // routine this function was called from (error messages)
    );
#endif

    inline Real64
        PsyRhFnTdbRhovLBnd0C([[maybe_unused]] EnergyPlusData &state,
                             Real64 const Tdb,                                       // dry-bulb temperature {C}
                             Real64 const Rhovapor,                                  // vapor density in air {kg/m3}
                             [[maybe_unused]] std::string_view const CalledFrom = "" // routine this function was called from (error messages)
        )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         R. J. Liesen
        //       DATE WRITTEN   July 2000
        //       MODIFIED       Name change to signify derivation and temperatures were used
        //                      with 0C as minimum; LKL January 2008
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function provides the Relative Humidity in air as a
        // function of dry bulb temperature and Vapor Density.

        // METHODOLOGY EMPLOYED:
        // ideal gas law
        // Universal gas const for water vapor 461.52 J/(kg K)

        // REFERENCES:
        // ASHRAE handbook 1993 Fundamentals,

#ifdef EP_psych_stats
        ++state.dataPsychCache->NumTimesCalled(iPsyRhFnTdbRhovLBnd0C);
#endif

        Real64 const RHValue(Rhovapor > 0.0 ? Rhovapor * 461.52 * (Tdb + DataGlobalConstants::KelvinConv) *
                                                  std::exp(-23.7093 + 4111.0 / ((Tdb + DataGlobalConstants::KelvinConv) - 35.45))
                                            : 0.0);

        if ((RHValue < 0.0) || (RHValue > 1.0)) {
#ifdef EP_psych_errors
            if ((RHValue < -0.05) || (RHValue > 1.01)) {
                PsyRhFnTdbRhovLBnd0C_error(state, Tdb, Rhovapor, RHValue, CalledFrom);
            }
#endif
            return min(max(RHValue, 0.01), 1.0);
        } else {
            return RHValue;
        }
    }

#ifdef EP_cache_PsyTwbFnTdbWPb

    Real64 PsyTwbFnTdbWPb(EnergyPlusData &state,
                          Real64 const Tdb,                      // dry-bulb temperature {C}
                          Real64 const W,                        // humidity ratio
                          Real64 const Pb,                       // barometric pressure {Pascals}
                          std::string_view const CalledFrom = "" // routine this function was called from (error messages)
    );

    Real64 PsyTwbFnTdbWPb_raw(EnergyPlusData &state,
                              Real64 const TDB,                      // dry-bulb temperature {C}
                              Real64 const dW,                       // humidity ratio
                              Real64 const Patm,                     // barometric pressure {Pascals}
                              std::string_view const CalledFrom = "" // routine this function was called from (error messages)
    );

#else

    Real64 PsyTwbFnTdbWPb(EnergyPlusData &state,
                          Real64 const TDB,                      // dry-bulb temperature {C}
                          Real64 const dW,                       // humidity ratio
                          Real64 const Patm,                     // barometric pressure {Pascals}
                          std::string_view const CalledFrom = "" // routine this function was called from (error messages)
    );

#endif

#ifdef EP_psych_errors
    void PsyVFnTdbWPb_error(EnergyPlusData &state,
                            Real64 const TDB,                 // dry-bulb temperature {C}
                            Real64 const w,                   // humidity ratio
                            Real64 const PB,                  // barometric pressure {Pascals}
                            Real64 const V,                   // specific volume {m3/kg}
                            std::string_view const CalledFrom // routine this function was called from (error messages)
    );
#endif

    inline Real64 PsyVFnTdbWPb([[maybe_unused]] EnergyPlusData &state,
                               Real64 const TDB,                                       // dry-bulb temperature {C}
                               Real64 const dW,                                        // humidity ratio
                               Real64 const PB,                                        // barometric pressure {Pascals}
                               [[maybe_unused]] std::string_view const CalledFrom = "" // routine this function was called from (error messages)
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         George Shih
        //       DATE WRITTEN   May 1976
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function provides the specific volume from dry-bulb temperature,
        // humidity ratio and barometric pressure.

        // REFERENCES:
        // ASHRAE HANDBOOK OF FUNDAMENTALS, 1972, P99, EQN 28

#ifdef EP_psych_stats
        ++state.dataPsychCache->NumTimesCalled(iPsyVFnTdbWPb);
#endif

        Real64 const w(max(dW, 1.0e-5));                                           // humidity ratio
        Real64 const V(1.59473e2 * (1.0 + 1.6078 * w) * (1.8 * TDB + 492.0) / PB); // specific volume {m3/kg}

        // Validity test
        if (V < 0.0) {
#ifdef EP_psych_errors
            if (V <= -0.01) PsyVFnTdbWPb_error(state, TDB, w, PB, V, CalledFrom);
#endif
            return 0.83; // Fix Was inside the ifdef
        } else {
            return V;
        }
    }

#ifdef EP_psych_errors
    void PsyWFnTdbH_error(EnergyPlusData &state,
                          Real64 const TDB,                 // dry-bulb temperature {C}
                          Real64 const H,                   // enthalpy {J/kg}
                          Real64 const W,                   // humidity ratio
                          std::string_view const CalledFrom // routine this function was called from (error messages)
    );
#endif

    inline Real64 PsyWFnTdbH([[maybe_unused]] EnergyPlusData &state,
                             Real64 const TDB,                                        // dry-bulb temperature {C}
                             Real64 const H,                                          // enthalpy {J/kg}
                             [[maybe_unused]] std::string_view const CalledFrom = "", // routine this function was called from (error messages)
                             [[maybe_unused]] bool const SuppressWarnings = false     // if calling function is calculating an intermediate state
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         George Shih
        //       DATE WRITTEN   May 1976
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function provides the humidity ratio from dry-bulb temperature
        // and enthalpy.

        // REFERENCES:
        // ASHRAE HANDBOOK OF FUNDAMENTALS, 1972, P100, EQN 32

#ifdef EP_psych_stats
        ++state.dataPsychCache->NumTimesCalled(iPsyWFnTdbH);
#endif

        Real64 const W((H - 1.00484e3 * TDB) / (2.50094e6 + 1.85895e3 * TDB)); // humidity ratio

        // Validity test
        if (W < 0.0) {
#ifdef EP_psych_errors
            if ((W <= -0.0001) && (!SuppressWarnings)) PsyWFnTdbH_error(state, TDB, H, W, CalledFrom);
#endif
            return 1.0e-5;
        } else {
            return W;
        }
    }

#ifdef EP_cache_PsyPsatFnTemp

    Real64 PsyPsatFnTemp_raw(EnergyPlusData &state,
                             Real64 const T,                        // dry-bulb temperature {C}
                             std::string_view const CalledFrom = "" // routine this function was called from (error messages)
    );

    // we are disabling these warnings on Windows because the cache value lookups are using 64bit integers,
    // but the () and [] operator overloads for Array1D (which stores the cache) only uses 32bit lookups
    // this seems ... very bad. This problem will be fixed when we get rid of Array1D
    // at which time this warning disable should be removed.
#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable : 4244)
#endif

    inline Real64 PsyPsatFnTemp(EnergyPlusData &state,
                                Real64 const T,                        // dry-bulb temperature {C}
                                std::string_view const CalledFrom = "" // routine this function was called from (error messages)
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Linda Lawrie
        //       DATE WRITTEN   March 2013
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // Provide a "cache" of results for the given argument (T) and pressure (Pascal) output result.

        // METHODOLOGY EMPLOYED:
        // Use grid shifting and masking to provide hash into the cache. Use Equivalence to
        // make Fortran ignore "types".

        // FUNCTION PARAMETER DEFINITIONS:
        //  integer(i64), parameter :: Grid_Mask=NOT(ISHFT(1_i64, Grid_Shift)-1)
        std::uint64_t constexpr Grid_Shift = 64 - 12 - psatprecision_bits;

#ifdef EP_psych_stats
        ++state.dataPsychCache->NumTimesCalled(iPsyPsatFnTemp_cache);
#endif

        DISABLE_WARNING_PUSH
        DISABLE_WARNING_STRICT_ALIASING
        Int64 Tdb_tag(*reinterpret_cast<Int64 const *>(&T) >> Grid_Shift);
        DISABLE_WARNING_POP
        Int64 const hash(Tdb_tag & psatcache_mask);
        auto &cPsat(state.dataPsychCache->cached_Psat(hash));

        if (cPsat.iTdb != Tdb_tag) {
            cPsat.iTdb = Tdb_tag;
            Tdb_tag <<= Grid_Shift;
            DISABLE_WARNING_PUSH
            DISABLE_WARNING_STRICT_ALIASING
            Real64 Tdb_tag_r = *reinterpret_cast<Real64 const *>(&Tdb_tag);
            DISABLE_WARNING_POP
            cPsat.Psat = PsyPsatFnTemp_raw(state, Tdb_tag_r, CalledFrom);
        }

        return cPsat.Psat; // saturation pressure {Pascals}
    }

#else

    Real64 PsyPsatFnTemp(EnergyPlusData &state,
                         Real64 const T,                        // dry-bulb temperature {C}
                         std::string_view const CalledFrom = "" // routine this function was called from (error messages)
    );

#endif

#ifdef EP_cache_PsyTsatFnHPb
    Real64 PsyTsatFnHPb_raw(EnergyPlusData &state,
                            Real64 const H,                        // enthalpy {J/kg}
                            Real64 const PB,                       // barometric pressure {Pascals}
                            std::string_view const CalledFrom = "" // routine this function was called from (error messages)
    );
    inline Real64 PsyTsatFnHPb(EnergyPlusData &state,
                               Real64 const H,
                               Real64 const Pb,                       // barometric pressure {Pascals}
                               std::string_view const CalledFrom = "" // routine this function was called from (error messages)
    )
    {

        Real64 Tsat_result; // result=> Sat-Temp {C}

        std::uint64_t constexpr Grid_Shift = 64 - 12 - tsat_hbp_precision_bits;

        // INTERFACE BLOCK SPECIFICATIONS:
        // na

        // DERIVED TYPE DEFINITIONS:
        // na

        // FUNCTION LOCAL VARIABLE DECLARATIONS:

#ifdef EP_psych_stats
        ++state.dataPsychCache->NumTimesCalled(iPsyTwbFnTdbWPb_cache);
#endif
        DISABLE_WARNING_PUSH
        DISABLE_WARNING_STRICT_ALIASING
        Int64 H_tag = *reinterpret_cast<Int64 const *>(&H) >> Grid_Shift;
        Int64 Pb_tag = *reinterpret_cast<Int64 const *>(&Pb) >> Grid_Shift;
        DISABLE_WARNING_POP
        Int64 hash = (H_tag ^ Pb_tag) & Int64(tsat_hbp_cache_size - 1);
        auto &cached_Tsat_HPb = state.dataPsychCache->cached_Tsat_HPb;
        if (cached_Tsat_HPb(hash).iH != H_tag || cached_Tsat_HPb(hash).iPb != Pb_tag) {
            cached_Tsat_HPb(hash).iH = H_tag;
            cached_Tsat_HPb(hash).iPb = Pb_tag;
            cached_Tsat_HPb(hash).Tsat = PsyTsatFnHPb_raw(state, H, Pb, CalledFrom);
        }

        Tsat_result = cached_Tsat_HPb(hash).Tsat;

        return Tsat_result;
    }

#else

    Real64 PsyTsatFnHPb(EnergyPlusData &state,
                        Real64 const H,                        // enthalpy {J/kg}
                        Real64 const PB,                       // barometric pressure {Pascals}
                        std::string_view const CalledFrom = "" // routine this function was called from (error messages)
    );

#endif

    inline Real64 PsyRhovFnTdbRh(EnergyPlusData &state,
                                 Real64 const Tdb,                      // dry-bulb temperature {C}
                                 Real64 const RH,                       // relative humidity value (0.0-1.0)
                                 std::string_view const CalledFrom = "" // routine this function was called from (error messages)
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         R. J. Liesen
        //       DATE WRITTEN   July 2000
        //       MODIFIED       Change temperature range applied (determine pws); Aug 2007; LKL
        //                      Function is continuous over temperature spectrum
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function provides the Vapor Density in air as a
        // function of dry bulb temperature, and Relative Humidity.

        // METHODOLOGY EMPLOYED:
        // ideal gas law
        // Universal gas const for water vapor 461.52 J/(kg K)

        // REFERENCES:
        // ASHRAE handbook 1993 Fundamentals, ??
        // Used values from Table 2, HOF 2005, Chapter 6, to verify that these values match (at saturation)
        // values from PsyRhFnTdbWPb

        return (PsyPsatFnTemp(state, Tdb, CalledFrom) * RH) / (461.52 * (Tdb + DataGlobalConstants::KelvinConv)); // Vapor density in air
    }

#ifdef EP_psych_errors
    void PsyRhFnTdbRhov_error(EnergyPlusData &state,
                              Real64 const Tdb,                      // dry-bulb temperature {C}
                              Real64 const Rhovapor,                 // vapor density in air {kg/m3}
                              Real64 const RHValue,                  // relative humidity
                              std::string_view const CalledFrom = "" // routine this function was called from (error messages)
    );
#endif

    inline Real64 PsyRhFnTdbRhov(EnergyPlusData &state,
                                 Real64 const Tdb,                                       // dry-bulb temperature {C}
                                 Real64 const Rhovapor,                                  // vapor density in air {kg/m3}
                                 [[maybe_unused]] std::string_view const CalledFrom = "" // routine this function was called from (error messages)
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         R. J. Liesen
        //       DATE WRITTEN   July 2000
        //       MODIFIED       Change temperature range applied (determine pws); Aug 2007; LKL
        //                      Function is continuous over temperature spectrum
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function provides the Relative Humidity in air as a
        // function of dry bulb temperature and Vapor Density.

        // METHODOLOGY EMPLOYED:
        // ideal gas law
        // Universal gas const for water vapor 461.52 J/(kg K)

        // REFERENCES:
        // ASHRAE handbook 1993 Fundamentals,
        // Used values from Table 2, HOF 2005, Chapter 6, to verify that these values match (at saturation)
        // values from PsyRhFnTdbWPb

        // FUNCTION PARAMETER DEFINITIONS:
        static std::string const RoutineName("PsyRhFnTdbRhov");

#ifdef EP_psych_stats
        ++state.dataPsychCache->NumTimesCalled(iPsyRhFnTdbRhov);
#endif

        Real64 const RHValue(Rhovapor > 0.0 ? Rhovapor * 461.52 * (Tdb + DataGlobalConstants::KelvinConv) / PsyPsatFnTemp(state, Tdb, RoutineName)
                                            : 0.0);

        if ((RHValue < 0.0) || (RHValue > 1.0)) {
#ifdef EP_psych_errors
            if ((RHValue < -0.05) || (RHValue > 1.01)) {
                PsyRhFnTdbRhov_error(state, Tdb, Rhovapor, RHValue, CalledFrom);
            }
#endif
            return min(max(RHValue, 0.01), 1.0);
        } else {
            return RHValue;
        }
    }

#ifdef EP_psych_errors
    void PsyRhFnTdbWPb_error(EnergyPlusData &state,
                             Real64 const TDB,                 // dry-bulb temperature {C}
                             Real64 const W,                   // humidity ratio
                             Real64 const RHValue,             // relative humidity (0.0-1.0)
                             std::string_view const CalledFrom // routine this function was called from (error messages)
    );
#endif

    inline Real64 PsyRhFnTdbWPb(EnergyPlusData &state,
                                Real64 const TDB,                      // dry-bulb temperature {C}
                                Real64 const dW,                       // humidity ratio
                                Real64 const PB,                       // barometric pressure {Pascals}
                                std::string_view const CalledFrom = "" // routine this function was called from (error messages)
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         Richard J. Liesen
        //       DATE WRITTEN   Nov 1988
        //       MODIFIED       Aug 1989, Michael J. Witte
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function provides the relative humidity value (0.0-1.0) as a result of
        // dry-bulb temperature, humidity ratio and barometric pressure.

        // REFERENCES:
        // ASHRAE HANDBOOK FUNDAMENTALS 1985, P6.12, EQN 10,21,23

        // FUNCTION PARAMETER DEFINITIONS:
        static std::string const RoutineName("PsyRhFnTdbWPb");

#ifdef EP_psych_stats
        ++state.dataPsychCache->NumTimesCalled(iPsyRhFnTdbWPb);
#endif

        Real64 const PWS(PsyPsatFnTemp(state, TDB, (CalledFrom.empty() ? RoutineName : CalledFrom))); // Pressure -- saturated for pure water

        // Find Degree Of Saturation
        Real64 const W(max(dW, 1.0e-5));                  // humidity ratio
        Real64 const U(W / (0.62198 * PWS / (PB - PWS))); // Degree of Saturation

        // Calculate The Relative Humidity
        Real64 const RHValue(U / (1.0 - (1.0 - U) * (PWS / PB)));

        // Validity test
        if ((RHValue < 0.0) || (RHValue > 1.0)) {
#ifdef EP_psych_errors
            if ((RHValue < -0.05) || (RHValue > 1.01)) {
                PsyRhFnTdbWPb_error(state, TDB, W, RHValue, CalledFrom);
            }
#endif
            return min(max(RHValue, 0.01), 1.0);
        } else {
            return RHValue;
        }
    }

#ifdef EP_psych_errors
    void PsyWFnTdpPb_error(EnergyPlusData &state,
                           Real64 const TDP,                 // dew-point temperature {C}
                           Real64 const PB,                  // barometric pressure {Pascals}
                           Real64 const W,                   // humidity ratio
                           Real64 const DeltaT,              // Reduced temperature difference of dew point
                           std::string_view const CalledFrom // routine this function was called from (error messages)
    );
#endif

    inline Real64 PsyWFnTdpPb(EnergyPlusData &state,
                              Real64 const TDP,                      // dew-point temperature {C}
                              Real64 const PB,                       // barometric pressure {Pascals}
                              std::string_view const CalledFrom = "" // routine this function was called from (error messages)
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         George Shih
        //       DATE WRITTEN   May 1976
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function provides the humidity ratio from dew-point temperature
        // and barometric pressure.

        // REFERENCES:
        // ASHRAE HANDBOOK OF FUNDAMENTALS, 1972, P99, EQN 22

        // FUNCTION PARAMETER DEFINITIONS:
        static std::string const RoutineName("PsyWFnTdpPb");

#ifdef EP_psych_stats
        ++state.dataPsychCache->NumTimesCalled(iPsyWFnTdpPb);
#endif

        Real64 const PDEW(
            PsyPsatFnTemp(state, TDP, (CalledFrom.empty() ? RoutineName : CalledFrom))); // saturation pressure at dew-point temperature {Pascals}
        Real64 const W(PDEW * 0.62198 / (PB - PDEW));                                    // humidity ratio

        // Validity test
        if (W < 0.0) {
            Real64 DeltaT = 0.0;
            Real64 PDEW1 = PDEW;
            while (PDEW1 >= PB) {
                DeltaT++;
                PDEW1 = PsyPsatFnTemp(state,
                                      TDP - DeltaT,
                                      (CalledFrom.empty() ? RoutineName : CalledFrom)); // saturation pressure at dew-point temperature {Pascals}
            }
            Real64 W1 = PDEW1 * 0.62198 / (PB - PDEW1);
#ifdef EP_psych_errors
            if (W <= -0.0001) {
                PsyWFnTdpPb_error(state, TDP, PB, W1, DeltaT, CalledFrom);
            }
#endif
            return W1;
        } else {
            return W;
        }
    }

#ifdef EP_psych_errors
    void PsyWFnTdbRhPb_error(EnergyPlusData &state,
                             Real64 const TDB,                 // dry-bulb temperature {C}
                             Real64 const RH,                  // relative humidity value (0.0-1.0)
                             Real64 const PB,                  // barometric pressure {Pascals}
                             Real64 const W,                   // humidity ratio
                             std::string_view const CalledFrom // routine this function was called from (error messages)
    );
#endif

    inline Real64 PsyWFnTdbRhPb(EnergyPlusData &state,
                                Real64 const TDB,                      // dry-bulb temperature {C}
                                Real64 const RH,                       // relative humidity value (0.0-1.0)
                                Real64 const PB,                       // barometric pressure {Pascals}
                                std::string_view const CalledFrom = "" // routine this function was called from (error messages)
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         George Shih
        //       DATE WRITTEN   May 1976
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function provides the humidity ratio from dry-bulb temperature,
        // relative humidty (value) and barometric pressure.

        // REFERENCES:
        // ASHRAE HANDBOOK OF FUNDAMENTALS, 1972, P99, EQN 22

        // FUNCTION PARAMETER DEFINITIONS:
        static std::string const RoutineName("PsyWFnTdbRhPb");

#ifdef EP_psych_stats
        ++state.dataPsychCache->NumTimesCalled(iPsyWFnTdbRhPb);
#endif

        Real64 const PDEW(RH *
                          PsyPsatFnTemp(state, TDB, (CalledFrom.empty() ? RoutineName : CalledFrom))); // Pressure at dew-point temperature {Pascals}

        // Numeric error check when the temperature and RH values cause Pdew to equal or exceed
        // barometric pressure which is physically impossible. An approach limit of 1000 pascals
        // was chosen to keep the numerics stable as the denominator approaches 0.
        Real64 const W(PDEW * 0.62198 / max(PB - PDEW, 1000.0)); // humidity ratio
        // THIS EQUATION IN SI UNIT IS FROM ASHRAE HANDBOOK OF FUNDAMENTALS PAGE 99  EQUATION 22

        // Validity test
        if (W < 1.0e-5) {
#ifdef EP_psych_errors
            if (W <= -0.0001) PsyWFnTdbRhPb_error(state, TDB, RH, PB, W, CalledFrom);
#endif
            return 1.0e-5;
        } else {
            return W;
        }
    }

#ifdef EP_psych_errors

    void PsyWFnTdbTwbPb_temperature_error(EnergyPlusData &state,
                                          Real64 const TDB,                 // dry-bulb temperature {C}
                                          Real64 const TWB,                 // wet-bulb temperature {C}
                                          Real64 const PB,                  // barometric pressure {Pascals}
                                          std::string_view const CalledFrom // routine this function was called from (error messages)
    );

    void PsyWFnTdbTwbPb_humidity_error(EnergyPlusData &state,
                                       Real64 const TDB,                 // dry-bulb temperature {C}
                                       Real64 const TWB,                 // wet-bulb temperature {C}
                                       Real64 const PB,                  // barometric pressure {Pascals}
                                       Real64 const W,                   // humidity ratio
                                       std::string_view const CalledFrom // routine this function was called from (error messages)
    );

#endif

    inline Real64 PsyWFnTdbTwbPb(EnergyPlusData &state,
                                 Real64 const TDB,                      // dry-bulb temperature {C}
                                 Real64 const TWBin,                    // wet-bulb temperature {C}
                                 Real64 const PB,                       // barometric pressure {Pascals}
                                 std::string_view const CalledFrom = "" // routine this function was called from (error messages)
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         George Shih
        //       DATE WRITTEN   May 1976
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function provides the humidity ratio from dry-bulb temperature,
        // wet-bulb temperature and barometric pressure.

        // REFERENCES:
        // ASHRAE HANDBOOK OF FUNDAMENTALS, 1972, P99, EQ 22,35

        // FUNCTION PARAMETER DEFINITIONS:
        static std::string const RoutineName("PsyWFnTdbTwbPb");

#ifdef EP_psych_stats
        ++state.dataPsychCache->NumTimesCalled(iPsyWFnTdbTwbPb);
#endif

        Real64 TWB(TWBin); // test wet-bulb temperature

        // Validity check
        if (TWB > TDB) {
#ifdef EP_psych_errors
            if (TWB > TDB + 0.01) PsyWFnTdbTwbPb_temperature_error(state, TDB, TWB, PB, CalledFrom);
#endif
            TWB = TDB;
        }

        // Calculation
        Real64 const PWET(PsyPsatFnTemp(state, TWB, (CalledFrom.empty() ? RoutineName : CalledFrom))); // Pressure at wet-bulb temperature {Pascals}
        Real64 const WET(0.62198 * PWET / (PB - PWET));                                                // Humidity ratio at wet-bulb temperature
        Real64 const W(((2501.0 - 2.381 * TWB) * WET - (TDB - TWB)) / (2501.0 + 1.805 * TDB - 4.186 * TWB)); // humidity ratio

        // Validity check
        if (W < 0.0) {
#ifdef EP_psych_errors
            PsyWFnTdbTwbPb_humidity_error(state, TDB, TWB, PB, W, CalledFrom);
#endif
            return PsyWFnTdbRhPb(state, TDB, 0.0001, PB, CalledFrom);
        } else {
            return W;
        }
    }

    inline Real64 PsyHFnTdbRhPb(EnergyPlusData &state,
                                Real64 const TDB,                      // dry-bulb temperature {C}
                                Real64 const RH,                       // relative humidity value (0.0 - 1.0)
                                Real64 const PB,                       // barometric pressure (N/M**2) {Pascals}
                                std::string_view const CalledFrom = "" // routine this function was called from (error messages)
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         J. C. VanderZee
        //       DATE WRITTEN   Feb. 1994
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function provides air enthalpy from temperature and relative humidity.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // ASHRAE HANDBOOK OF FUNDAMENTALS, 1972, P100, EQN 32
        //   by using functions PsyWFnTdbRhPb and PsyHFnTdbW

        return PsyHFnTdbW(TDB, max(PsyWFnTdbRhPb(state, TDB, RH, PB, CalledFrom), 1.0e-5)); // enthalpy {J/kg}
    }

#ifdef EP_cache_PsyTsatFnPb

    Real64 PsyTsatFnPb_raw(EnergyPlusData &state,
                           Real64 const Press,                    // barometric pressure {Pascals}
                           std::string_view const CalledFrom = "" // routine this function was called from (error messages)
    );

    inline Real64 PsyTsatFnPb(EnergyPlusData &state,
                              Real64 const Press,                    // barometric pressure {Pascals}
                              std::string_view const CalledFrom = "" // routine this function was called from (error messages)
    )
    {

        std::uint64_t constexpr Grid_Shift = 64 - 12 - tsatprecision_bits;
        DISABLE_WARNING_PUSH
        DISABLE_WARNING_STRICT_ALIASING
        Int64 const Pb_tag(*reinterpret_cast<Int64 const *>(&Press) >> Grid_Shift);
        DISABLE_WARNING_POP

        Int64 const hash(Pb_tag & tsatcache_mask);
        auto &cTsat(state.dataPsychCache->cached_Tsat(hash));
        if (cTsat.iPb != Pb_tag) {
            cTsat.iPb = Pb_tag;
            cTsat.Tsat = PsyTsatFnPb_raw(state, Press, CalledFrom);
        }

        return cTsat.Tsat; // saturation temperature
    }

#ifdef _MSC_VER
#pragma warning(pop)
#endif

#else
    Real64 PsyTsatFnPb(EnergyPlusData &state,
                       Real64 const Press,                    // barometric pressure {Pascals}
                       std::string_view const CalledFrom = "" // routine this function was called from (error messages)
    );
#endif

    inline Real64 PsyTdpFnWPb(EnergyPlusData &state,
                              Real64 const W,                        // humidity ratio
                              Real64 const PB,                       // barometric pressure (N/M**2) {Pascals}
                              std::string_view const CalledFrom = "" // routine this function was called from (error messages)
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         George Shih
        //       DATE WRITTEN   May 1976
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function calculates the dew-point temperature {C} from humidity ratio and pressure.

        // METHODOLOGY EMPLOYED:
        // na

        // REFERENCES:
        // ASHRAE HANDBOOK OF FUNDAMENTALS, 1972, P.99, EQN 22

        Real64 const W0(max(W, 1.0e-5));             // limited humidity ratio
        Real64 const PDEW(PB * W0 / (0.62198 + W0)); // pressure at dew point temperature
        return PsyTsatFnPb(state, PDEW, CalledFrom);
    }

#ifdef EP_psych_errors
    void PsyTdpFnTdbTwbPb_error(EnergyPlusData &state,
                                Real64 const TDB,                 // dry-bulb temperature {C}
                                Real64 const TWB,                 // wet-bulb temperature {C}
                                Real64 const PB,                  // barometric pressure (N/M**2) {Pascals}
                                Real64 const W,                   // humidity ratio
                                Real64 const TDP,                 // dew-point temperature {C}
                                std::string_view const CalledFrom // routine this function was called from (error messages)
    );
#endif

    inline Real64 PsyTdpFnTdbTwbPb(EnergyPlusData &state,
                                   Real64 const TDB,                      // dry-bulb temperature {C}
                                   Real64 const TWB,                      // wet-bulb temperature {C}
                                   Real64 const PB,                       // barometric pressure (N/M**2) {Pascals}
                                   std::string_view const CalledFrom = "" // routine this function was called from (error messages)
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         George Shih
        //       DATE WRITTEN   May 1976
        //       MODIFIED       na
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS FUNCTION:
        // This function calculates the dew-point temperature {C} from dry-bulb, wet-bulb and pressure.

#ifdef EP_psych_stats
        ++state.dataPsychCache->NumTimesCalled(iPsyTdpFnTdbTwbPb);
#endif

        Real64 const W(max(PsyWFnTdbTwbPb(state, TDB, TWB, PB, CalledFrom), 1.0e-5));
        Real64 const TDP(PsyTdpFnWPb(state, W, PB, CalledFrom));

        if (TDP > TWB) {
#ifdef EP_psych_errors
            if (TDP > TWB + 0.1) PsyTdpFnTdbTwbPb_error(state, TDB, TWB, PB, W, TDP, CalledFrom);
#endif
            return TWB;
        } else {
            return TDP;
        }
    }

    inline Real64 F6(Real64 const X, Real64 const A0, Real64 const A1, Real64 const A2, Real64 const A3, Real64 const A4, Real64 const A5)
    {
        return A0 + X * (A1 + X * (A2 + X * (A3 + X * (A4 + X * A5))));
    }

    inline Real64
    F7(Real64 const X, Real64 const A0, Real64 const A1, Real64 const A2, Real64 const A3, Real64 const A4, Real64 const A5, Real64 const A6)
    {
        return (A0 + X * (A1 + X * (A2 + X * (A3 + X * (A4 + X * (A5 + X * A6)))))) / 1.0E10;
    }

    inline Real64 CPCW([[maybe_unused]] Real64 const Temperature // unused1208
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         RUSSELL D. TAYLOR
        //       DATE WRITTEN   April 1992

        // PURPOSE OF THIS FUNCTION:
        // This function provides the specific heat of chilled water. CPCW (J/Kg/k)

        return 4180.0;
    }

    inline Real64 CPHW([[maybe_unused]] Real64 const Temperature // unused1208
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         RUSSELL D. TAYLOR
        //       DATE WRITTEN   April 1992

        // PURPOSE OF THIS FUNCTION:
        // This function provides the specific heat of hot water. CPHW (J/Kg/k)

        return 4180.0;
    }

    inline Real64 RhoH2O(Real64 const TB // Dry bulb temperature. {C}
    )
    {
        // FUNCTION INFORMATION:
        //       AUTHOR         SIGSTEINN P. GRETARSSON
        //       DATE WRITTEN   April 1992

        // PURPOSE OF THIS FUNCTION:
        // This function provides the density of water at a specific temperature.

        // METHODOLOGY EMPLOYED:
        //     Density of water [kg/m3]
        //     (RANGE: KelvinConv - 423.15 DEG. K) (convert to C first)

        return 1000.1207 + 8.3215874e-04 * TB - 4.929976e-03 * pow_2(TB) + 8.4791863e-06 * pow_3(TB);
    }

    inline Real64 PsyDeltaHSenFnTdb2Tdb1W(Real64 const TDB2, // dry-bulb temperature at state 1 {C}
                                          Real64 const TDB1, // dry-bulb temperature at state 2 {C}
                                          Real64 const W     // humidity ratio (at zone air node or Wmin)
    )
    {
        // When called for zone equipment flow entering a zone (from CalcZoneSensibleLatentOutput or CalcZoneSensibleOutput):
        // returns sensible enthalpy difference between equipment supply air (TDB2) and zone air (TDB1) evaluated
        // using the zone air node humidity ratio. This enthalpy difference multiplied by supply
        // air mass flow rate yields the sensible heat transfer rate in Watts.
        // positive value is heating, negative value is cooling

        // When called across a component (from PsyDeltaHSenFnTdb2W2Tdb1W1 by CalcComponentSensibleLatentOutput):
        // returns sensible enthalpy difference between state 1 (TDB1) and state 2 (TDB2) using the minimum
        // humidity ratio from states 1 and 2. This enthalpy difference multiplied by supply air mass flow
        // rate yields the sensible heat transfer rate in Watts.
        // positive value is heating, negative value is cooling

        // the following two functions for calculating enthalpy difference are equivalent:
        // PsyDeltaHSenFnTdb2Tdb1W() = PsyHFnTdbW(TDB2, W) - PsyHFnTdbW(TDB1, W)
        // PsyDeltaHSenFnTdb2Tdb1W() function was derived by simplifying the expression above
        // The constant coefficients come from the equation for moist air enthalpy, PsyHFnTdbW()

        return (1.00484e3 + max(1.0e-5, W) * 1.85895e3) * (TDB2 - TDB1);
    }

    inline Real64 PsyDeltaHSenFnTdb2W2Tdb1W1(Real64 const TDB2, // dry-bulb temperature at state 2 {C}
                                             Real64 const W2,   // humidity ratio at state 2
                                             Real64 const TDB1, // dry-bulb temperature at state 1 {C}
                                             Real64 const W1    // humidity ratio at state 1
    )
    {
        // returns sensible enthalpy difference of moist air going from state 1 to state 2 (e.g across coils)
        // using the minimum humidity ratio state points 1 and 2. This enthalpy difference multiplied by
        // supply air mass flow rate yields sensible heat transfer rate across coils in Watts
        // positive value is heating, negative value is cooling

        // the following two functions for calculating enthalpy difference are equivalent:
        // PsyDeltaHSenFnTdb2W2Tdb1W1() = PsyHFnTdbW(TDB2, min(W1, W2)) - PsyHFnTdbW(TDB1, min(W1,W2))
        // PsyDeltaHSenFnTdb2W2Tdb1W1() function was derived by simplifying the above expression
        // The constant coefficients came from the equation for moist air enthalpy, PsyHFnTdbW()

        Real64 const Wmin = min(W1, W2);
        return PsyDeltaHSenFnTdb2Tdb1W(TDB2, TDB1, Wmin);
    }

    Real64 linearint(EnergyPlusData &state,
                     int const n,      // sample data size
                     const Real64 &x); // given value of x
    Real64 CSplineint(EnergyPlusData &state,
                      int const n,      // sample data size
                      const Real64 &x); // given value of x

} // namespace Psychrometrics

struct PsychrometricsData : BaseGlobalStruct
{
    Real64 iconvTol = 0.0001;
    Real64 last_Patm = -99999.0;  // barometric pressure {Pascals}  (last)
    Real64 last_tBoil = -99999.0; // Boiling temperature of water at given pressure (last)
    Real64 Press_Save = -99999.0;
    Real64 tSat_Save = -99999.0;
    Array1D_int iPsyErrIndex = Array1D_int(EnergyPlus::NumPsychMonitors, 0); // Number of times error occurred
    std::string String;
    bool ReportErrors = true;
    Array1D<Real64> x_sample = {100,  200,  300,  400,  500,  600,  700,  800,  900,  1000,  1100,  1200,  1300,  1400,  1500, 1600, 1700, 1800,
                                1900, 2000, 2100, 2200, 2300, 2400, 2500, 2600, 2700, 2800,  2900,  3000,  3100,  3200,  3300, 3400, 3500, 3600,
                                3700, 3800, 3900, 4000, 4100, 4200, 4300, 4400, 4500, 4600,  4700,  4800,  4900,  5000,  5100, 5200, 5300, 5400,
                                5500, 5600, 5700, 5800, 5900, 6000, 6100, 6200, 6300, 6400,  6500,  6600,  6700,  6800,  6900, 7000, 7100, 7200,
                                7300, 7400, 7500, 7600, 7700, 7800, 7900, 8000, 8100, 8200,  8300,  8400,  8500,  8600,  8700, 8800, 8900, 9000,
                                9100, 9200, 9300, 9400, 9500, 9600, 9700, 9800, 9900, 10000, 10100, 10200, 10300, 10400, 10500}; //(pressure)
    Array1D<Real64> y_sample = {-20.3339, -12.9181, -8.37385, -5.05147, -2.41598, -0.223487, 1.88153, 3.76257, 5.44597, 6.97148, 8.36777, 9.65627,
                                10.8534,  11.972,   13.0223,  14.0128,  14.9503,  15.8406,   16.6885, 17.4981, 18.2729, 19.0161, 19.7302, 20.4176,
                                21.0805,  21.7205,  22.3394,  22.9385,  23.5193,  24.0828,   24.6301, 25.1622, 25.6801, 26.1844, 26.676,  27.1554,
                                27.6234,  28.0806,  28.5273,  28.9642,  29.3917,  29.8103,   30.2202, 30.622,  31.0158, 31.4022, 31.7813, 32.1535,
                                32.519,   32.8781,  33.231,   33.578,   33.9192,  34.2549,   34.5853, 34.9104, 35.2306, 35.546,  35.8567, 36.1628,
                                36.4646,  36.7621,  37.0555,  37.3449,  37.6304,  37.9121,   38.1902, 38.4647, 38.7357, 39.0033, 39.2677, 39.5289,
                                39.787,   40.042,   40.294,   40.5432,  40.7896,  41.0332,   41.2742, 41.5125, 41.7483, 41.9815, 42.2124, 42.4408,
                                42.6669,  42.8907,  43.1123,  43.3318,  43.549,   43.7642,   43.9774, 44.1885, 44.3977, 44.6049, 44.8102, 45.0137,
                                45.2154,  45.4153,  45.6135,  45.8099,  46.0047,  46.1978,   46.3892, 46.5791, 46.7675}; //(tsat)
    Array1D<Real64> d2y = {
        0.000239945, -0.00047989, -4.33E-05, -8.02E-05, -4.80E-05, 6.58E-06,  -3.08E-05, -1.80E-05, -1.60E-05, -1.27E-05, -1.07E-05, -9.07E-06,
        -7.81E-06,   -6.79E-06,   -5.96E-06, -5.28E-06, -4.71E-06, -4.23E-06, -3.82E-06, -3.47E-06, -3.14E-06, -2.97E-06, -2.37E-06, -3.54E-06,
        1.77E-06,    1.46E-06,    -2.93E-06, -1.58E-06, -1.79E-06, -1.60E-06, -1.52E-06, -1.43E-06, -1.35E-06, -1.28E-06, -1.21E-06, -1.15E-06,
        -1.09E-06,   -1.03E-06,   -9.86E-07, -9.40E-07, -8.97E-07, -8.57E-07, -8.20E-07, -7.86E-07, -7.53E-07, -7.23E-07, -6.94E-07, -6.67E-07,
        -6.41E-07,   -6.18E-07,   -5.95E-07, -5.74E-07, -5.53E-07, -5.35E-07, -5.16E-07, -4.99E-07, -4.83E-07, -4.67E-07, -4.53E-07, -4.38E-07,
        -4.26E-07,   -4.12E-07,   -4.00E-07, -3.88E-07, -3.77E-07, -3.67E-07, -3.56E-07, -3.46E-07, -3.38E-07, -3.28E-07, -3.20E-07, -3.11E-07,
        -3.04E-07,   -2.96E-07,   -2.88E-07, -2.81E-07, -2.75E-07, -2.68E-07, -2.62E-07, -2.56E-07, -2.49E-07, -2.44E-07, -2.39E-07, -2.33E-07,
        -2.28E-07,   -2.23E-07,   -2.18E-07, -2.14E-07, -2.10E-07, -2.03E-07, -2.07E-07, -1.73E-07, -2.81E-07, 1.41E-07,  1.33E-07,  -2.67E-07,
        -1.56E-07,   -1.80E-07,   -1.70E-07, -1.69E-07, -1.64E-07, -1.67E-07, -1.40E-07, -2.28E-07, 1.14E-07};

    void clear_state() override
    {
        iPsyErrIndex = Array1D_int(EnergyPlus::NumPsychMonitors, 0);
        iconvTol = 0.0001;
        last_Patm = -99999.0;  // barometric pressure {Pascals}  (last)
        last_tBoil = -99999.0; // Boiling temperature of water at given pressure (last)
        Press_Save = -99999.0;
        tSat_Save = -99999.0;
        String = "";
        ReportErrors = true;
        x_sample = {100,  200,  300,  400,  500,  600,  700,  800,  900,  1000,  1100,  1200,  1300,  1400,  1500, 1600, 1700, 1800,
                    1900, 2000, 2100, 2200, 2300, 2400, 2500, 2600, 2700, 2800,  2900,  3000,  3100,  3200,  3300, 3400, 3500, 3600,
                    3700, 3800, 3900, 4000, 4100, 4200, 4300, 4400, 4500, 4600,  4700,  4800,  4900,  5000,  5100, 5200, 5300, 5400,
                    5500, 5600, 5700, 5800, 5900, 6000, 6100, 6200, 6300, 6400,  6500,  6600,  6700,  6800,  6900, 7000, 7100, 7200,
                    7300, 7400, 7500, 7600, 7700, 7800, 7900, 8000, 8100, 8200,  8300,  8400,  8500,  8600,  8700, 8800, 8900, 9000,
                    9100, 9200, 9300, 9400, 9500, 9600, 9700, 9800, 9900, 10000, 10100, 10200, 10300, 10400, 10500}; //(pressure)
        y_sample = {-20.3339, -12.9181, -8.37385, -5.05147, -2.41598, -0.223487, 1.88153, 3.76257, 5.44597, 6.97148, 8.36777, 9.65627,
                    10.8534,  11.972,   13.0223,  14.0128,  14.9503,  15.8406,   16.6885, 17.4981, 18.2729, 19.0161, 19.7302, 20.4176,
                    21.0805,  21.7205,  22.3394,  22.9385,  23.5193,  24.0828,   24.6301, 25.1622, 25.6801, 26.1844, 26.676,  27.1554,
                    27.6234,  28.0806,  28.5273,  28.9642,  29.3917,  29.8103,   30.2202, 30.622,  31.0158, 31.4022, 31.7813, 32.1535,
                    32.519,   32.8781,  33.231,   33.578,   33.9192,  34.2549,   34.5853, 34.9104, 35.2306, 35.546,  35.8567, 36.1628,
                    36.4646,  36.7621,  37.0555,  37.3449,  37.6304,  37.9121,   38.1902, 38.4647, 38.7357, 39.0033, 39.2677, 39.5289,
                    39.787,   40.042,   40.294,   40.5432,  40.7896,  41.0332,   41.2742, 41.5125, 41.7483, 41.9815, 42.2124, 42.4408,
                    42.6669,  42.8907,  43.1123,  43.3318,  43.549,   43.7642,   43.9774, 44.1885, 44.3977, 44.6049, 44.8102, 45.0137,
                    45.2154,  45.4153,  45.6135,  45.8099,  46.0047,  46.1978,   46.3892, 46.5791, 46.7675}; //(tsat)
        d2y = {0.000239945, -0.00047989, -4.33E-05, -8.02E-05, -4.80E-05, 6.58E-06,  -3.08E-05, -1.80E-05, -1.60E-05, -1.27E-05, -1.07E-05, -9.07E-06,
               -7.81E-06,   -6.79E-06,   -5.96E-06, -5.28E-06, -4.71E-06, -4.23E-06, -3.82E-06, -3.47E-06, -3.14E-06, -2.97E-06, -2.37E-06, -3.54E-06,
               1.77E-06,    1.46E-06,    -2.93E-06, -1.58E-06, -1.79E-06, -1.60E-06, -1.52E-06, -1.43E-06, -1.35E-06, -1.28E-06, -1.21E-06, -1.15E-06,
               -1.09E-06,   -1.03E-06,   -9.86E-07, -9.40E-07, -8.97E-07, -8.57E-07, -8.20E-07, -7.86E-07, -7.53E-07, -7.23E-07, -6.94E-07, -6.67E-07,
               -6.41E-07,   -6.18E-07,   -5.95E-07, -5.74E-07, -5.53E-07, -5.35E-07, -5.16E-07, -4.99E-07, -4.83E-07, -4.67E-07, -4.53E-07, -4.38E-07,
               -4.26E-07,   -4.12E-07,   -4.00E-07, -3.88E-07, -3.77E-07, -3.67E-07, -3.56E-07, -3.46E-07, -3.38E-07, -3.28E-07, -3.20E-07, -3.11E-07,
               -3.04E-07,   -2.96E-07,   -2.88E-07, -2.81E-07, -2.75E-07, -2.68E-07, -2.62E-07, -2.56E-07, -2.49E-07, -2.44E-07, -2.39E-07, -2.33E-07,
               -2.28E-07,   -2.23E-07,   -2.18E-07, -2.14E-07, -2.10E-07, -2.03E-07, -2.07E-07, -1.73E-07, -2.81E-07, 1.41E-07,  1.33E-07,  -2.67E-07,
               -1.56E-07,   -1.80E-07,   -1.70E-07, -1.69E-07, -1.64E-07, -1.67E-07, -1.40E-07, -2.28E-07, 1.14E-07};
    }
};

} // namespace EnergyPlus

#endif
