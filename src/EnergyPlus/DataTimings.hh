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

#ifndef DataTimings_hh_INCLUDED
#define DataTimings_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

#ifdef EP_NO_Timings
#undef EP_Timings
#endif

namespace DataTimings {

    // Data
    // -only module should be available to other modules and routines.
    // Thus, all variables in this module must be PUBLIC.

    // MODULE PARAMETER DEFINITIONS:
    int constexpr MaxTimingStringLength()
    {
        return 250;
    } // string length for timing string array

    // Following for calls to routines

    // Types

    struct timings
    {
        // Members
        std::string Element;
        Real64 rstartTime;
        Real64 currentTimeSum;
        int calls;

        // Default Constructor
        timings() : rstartTime(0.0), currentTimeSum(0.0), calls(0)
        {
        }
    };

    // Functions

    void epStartTime(std::string const &ctimingElementstring);

    void epStopTime(std::string const &ctimingElementstring,
                    Optional_bool_const printit = _, // true if it should be printed here.
                    Optional_string_const wprint = _ // only needed (and assumed, if printit is true)
    );

    void epSummaryTimes(Real64 &TimeUsed_CPUTime);

    Real64 epGetTimeUsed(EnergyPlusData &state, std::string const &ctimingElementstring);

    Real64 epGetTimeUsedperCall(EnergyPlusData &state, std::string const &ctimingElementstring);

    Real64 eptime(EnergyPlusData &state);

    Real64 epElapsedTime();

} // namespace DataTimings

struct DataTimingsData : BaseGlobalStruct
{

    int NumTimingElements = 0;
    int MaxTimingElements = 0;
    Real64 dailyWeatherTime = 0.0;
    Real64 dailyExteriorEnergyUseTime = 0.0;
    Real64 dailyHeatBalanceTime = 0.0;
    Real64 hbdailyInit = 0.0;
    Real64 hbdailyOutSurf = 0.0;
    Real64 hbdailyInSurf = 0.0;
    Real64 hbdailyHVAC = 0.0;
    Real64 hbdailyRep = 0.0;
    Real64 clockrate = 0.0;
    bool lprocessingInputTiming = false;
    bool lmanageSimulationTiming = false;
    bool lcloseoutReportingTiming = false;
    Array1D<DataTimings::timings> Timing;

    // Following for calls to routines
#ifdef EP_Count_Calls
    int NumShadow_Calls = 0;
    int NumShadowAtTS_Calls = 0;
    int NumClipPoly_Calls = 0;
    int NumInitSolar_Calls = 0;
    int NumAnisoSky_Calls = 0;
    int NumDetPolyOverlap_Calls = 0;
    int NumCalcPerSolBeam_Calls = 0;
    int NumDetShadowCombs_Calls = 0;
    int NumIntSolarDist_Calls = 0;
    int NumIntRadExchange_Calls = 0;
    int NumIntRadExchangeZ_Calls = 0;
    int NumIntRadExchangeMain_Calls = 0;
    int NumIntRadExchangeOSurf_Calls = 0;
    int NumIntRadExchangeISurf_Calls = 0;
    int NumMaxInsideSurfIterations = 0;
    int NumCalcScriptF_Calls = 0;
#endif

    void clear_state() override
    {
        this->NumTimingElements = 0;
        this->MaxTimingElements = 0;
        this->dailyWeatherTime = 0.0;
        this->dailyExteriorEnergyUseTime = 0.0;
        this->dailyHeatBalanceTime = 0.0;
        this->hbdailyInit = 0.0;
        this->hbdailyOutSurf = 0.0;
        this->hbdailyInSurf = 0.0;
        this->hbdailyHVAC = 0.0;
        this->hbdailyRep = 0.0;
        this->clockrate = 0.0;
        this->lprocessingInputTiming = false;
        this->lmanageSimulationTiming = false;
        this->lcloseoutReportingTiming = false;
        this->Timing.deallocate();

#ifdef EP_Count_Calls
        this->NumShadow_Calls = 0;
        this->NumShadowAtTS_Calls = 0;
        this->NumClipPoly_Calls = 0;
        this->NumInitSolar_Calls = 0;
        this->NumAnisoSky_Calls = 0;
        this->NumDetPolyOverlap_Calls = 0;
        this->NumCalcPerSolBeam_Calls = 0;
        this->NumDetShadowCombs_Calls = 0;
        this->NumIntSolarDist_Calls = 0;
        this->NumIntRadExchange_Calls = 0;
        this->NumIntRadExchangeZ_Calls = 0;
        this->NumIntRadExchangeMain_Calls = 0;
        this->NumIntRadExchangeOSurf_Calls = 0;
        this->NumIntRadExchangeISurf_Calls = 0;
        this->NumMaxInsideSurfIterations = 0;
        this->NumCalcScriptF_Calls = 0;
#endif
    }
};

} // namespace EnergyPlus

#endif
