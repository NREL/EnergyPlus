// EnergyPlus, Copyright (c) 1996-2019, The Board of Trustees of the University of Illinois,
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

#ifndef EnergyPlusAPIRuntime_h_INCLUDED
#define EnergyPlusAPIRuntime_h_INCLUDED

#include <EnergyPlus/api/EnergyPlusAPI.hh>
#include <EnergyPlus/TypeDefs.h>

#ifdef __cplusplus

#include <functional>

ENERGYPLUSLIB_API void callbackBeginNewEnvironment(std::function<void ()> f);
ENERGYPLUSLIB_API void callbackAfterNewEnvironmentWarmupComplete(std::function<void ()> f);
ENERGYPLUSLIB_API void callbackBeginZoneTimeStepBeforeInitHeatBalance(std::function<void ()> f);
ENERGYPLUSLIB_API void callbackBeginZoneTimeStepAfterInitHeatBalance(std::function<void ()> f);
ENERGYPLUSLIB_API void callbackBeginTimeStepBeforePredictor(std::function<void ()> f);
ENERGYPLUSLIB_API void callbackAfterPredictorBeforeHVACManagers(std::function<void ()> f);
ENERGYPLUSLIB_API void callbackAfterPredictorAfterHVACManagers(std::function<void ()> f);
ENERGYPLUSLIB_API void callbackInsideSystemIterationLoop(std::function<void ()> f);
ENERGYPLUSLIB_API void callbackEndOfZoneTimeStepBeforeZoneReporting(std::function<void ()> f);
ENERGYPLUSLIB_API void callbackEndOfZoneTimeStepAfterZoneReporting(std::function<void ()> f);
ENERGYPLUSLIB_API void callbackEndOfSystemTimeStepBeforeHVACReporting(std::function<void ()> f);
ENERGYPLUSLIB_API void callbackEndOfSystemTimeStepAfterHVACReporting(std::function<void ()> f);
ENERGYPLUSLIB_API void callbackEndOfZoneSizing(std::function<void ()> f);
ENERGYPLUSLIB_API void callbackEndOfSystemSizing(std::function<void ()> f);
ENERGYPLUSLIB_API void callbackEndOfAfterComponentGetInput(std::function<void ()> f);
ENERGYPLUSLIB_API void callbackUserDefinedComponentModel(std::function<void ()> f);
ENERGYPLUSLIB_API void callbackUnitarySystemSizing(std::function<void ()> f);

extern "C" {

#endif

ENERGYPLUSLIB_API void cClearAllStates();
ENERGYPLUSLIB_API void cRuntimeNoOp();

// Program level functions
ENERGYPLUSLIB_API int energyplus(int argc, const char *argv[]);

// There are a few things here to provide a way for plugins to issue messages
// I don't see how these are really useful in API applications, but I'm not going to
// try to isolate them.  Also, we don't have a fatal, because the plugin can just
// return 1 from main() to signal that E+ should clean up and fatal out.
ENERGYPLUSLIB_API void issueWarning(const char * message); // Issue the warning text to the err file
ENERGYPLUSLIB_API void issueSevere(const char * message); // Issue the severe text to the err file
ENERGYPLUSLIB_API void issueText(const char * message); // Issue additional supporting text to the err file

// We can also provide a way for EnergyPlus to callback during an API run
// These are not useful in Python Plugin applications, just during API calls
ENERGYPLUSLIB_API void registerProgressCallback(void (*f)(int const));
ENERGYPLUSLIB_API void registerStdOutCallback(void (*f)(const char * message));

ENERGYPLUSLIB_API void callbackBeginNewEnvironment(void (*f)());
ENERGYPLUSLIB_API void callbackAfterNewEnvironmentWarmupComplete(void (*f)());
ENERGYPLUSLIB_API void callbackBeginZoneTimeStepBeforeInitHeatBalance(void (*f)());
ENERGYPLUSLIB_API void callbackBeginZoneTimeStepAfterInitHeatBalance(void (*f)());
ENERGYPLUSLIB_API void callbackBeginTimeStepBeforePredictor(void (*f)());
ENERGYPLUSLIB_API void callbackAfterPredictorBeforeHVACManagers(void (*f)());
ENERGYPLUSLIB_API void callbackAfterPredictorAfterHVACManagers(void (*f)());
ENERGYPLUSLIB_API void callbackInsideSystemIterationLoop(void (*f)());
ENERGYPLUSLIB_API void callbackEndOfZoneTimeStepBeforeZoneReporting(void (*f)());
ENERGYPLUSLIB_API void callbackEndOfZoneTimeStepAfterZoneReporting(void (*f)());
ENERGYPLUSLIB_API void callbackEndOfSystemTimeStepBeforeHVACReporting(void (*f)());
ENERGYPLUSLIB_API void callbackEndOfSystemTimeStepAfterHVACReporting(void (*f)());
ENERGYPLUSLIB_API void callbackEndOfZoneSizing(void (*f)());
ENERGYPLUSLIB_API void callbackEndOfSystemSizing(void (*f)());
ENERGYPLUSLIB_API void callbackEndOfAfterComponentGetInput(void (*f)());
ENERGYPLUSLIB_API void callbackUserDefinedComponentModel(void (*f)());
ENERGYPLUSLIB_API void callbackUnitarySystemSizing(void (*f)());

#ifdef __cplusplus
}

#endif

#endif // EnergyPlusAPIRuntime_h_INCLUDED
