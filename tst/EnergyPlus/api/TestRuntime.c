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

#include <EnergyPlus/api/runtime.h>
#include <EnergyPlus/api/state.h>
#include <stdio.h>
#include <string.h>

#ifdef _WIN32
#define __PRETTY_FUNCTION__ __FUNCSIG__
#endif

int numWarnings = 0;
int oneTimeHalfway = 0;

void BeginNewEnvironmentHandler(EnergyPlusState state)
{
    printf("CALLBACK: %s\n", __PRETTY_FUNCTION__);
    issueWarning(state, "Fake Warning at new environment");
    issueSevere(state, "Fake Severe at new environment");
    issueText(state, "Just some text at the new environment");
}
void AfterNewEnvironmentWarmupCompleteHandler(EnergyPlusState state)
{
    printf("CALLBACK: %s\n", __PRETTY_FUNCTION__);
}
void BeginZoneTimeStepBeforeInitHeatBalanceHandler(EnergyPlusState state)
{
    printf("CALLBACK: %s\n", __PRETTY_FUNCTION__);
}
void BeginZoneTimeStepAfterInitHeatBalanceHandler(EnergyPlusState state)
{
    printf("CALLBACK: %s\n", __PRETTY_FUNCTION__);
}
void BeginTimeStepBeforePredictorHandler(EnergyPlusState state)
{
    printf("CALLBACK: %s\n", __PRETTY_FUNCTION__);
}
void AfterPredictorBeforeHVACManagersHandler(EnergyPlusState state)
{
    printf("CALLBACK: %s\n", __PRETTY_FUNCTION__);
}
void AfterPredictorAfterHVACManagersHandler(EnergyPlusState state)
{
    printf("CALLBACK: %s\n", __PRETTY_FUNCTION__);
}
void InsideSystemIterationLoopHandler(EnergyPlusState state)
{
    printf("CALLBACK: %s\n", __PRETTY_FUNCTION__);
}
void EndOfZoneTimeStepBeforeZoneReportingHandler(EnergyPlusState state)
{
    printf("CALLBACK: %s\n", __PRETTY_FUNCTION__);
}
void EndOfZoneTimeStepAfterZoneReportingHandler(EnergyPlusState state)
{
    printf("CALLBACK: %s\n", __PRETTY_FUNCTION__);
}
void EndOfSystemTimeStepBeforeHVACReportingHandler(EnergyPlusState state)
{
    printf("CALLBACK: %s\n", __PRETTY_FUNCTION__);
}
void EndOfSystemTimeStepAfterHVACReportingHandler(EnergyPlusState state)
{
    printf("CALLBACK: %s\n", __PRETTY_FUNCTION__);
}
void EndOfZoneSizingHandler(EnergyPlusState state)
{
    printf("CALLBACK: %s\n", __PRETTY_FUNCTION__);
}
void EndOfSystemSizingHandler(EnergyPlusState state)
{
    printf("CALLBACK: %s\n", __PRETTY_FUNCTION__);
}
void EndOfAfterComponentGetInputHandler(EnergyPlusState state)
{
    printf("CALLBACK: %s\n", __PRETTY_FUNCTION__);
}
void UnitarySystemSizingHandler(EnergyPlusState state)
{
    printf("CALLBACK: %s\n", __PRETTY_FUNCTION__);
}
void stdOutHandler(const char *message)
{
    printf("CAPTURED STDOUT: %s\n", message);
}

void newEnvrnHandler(EnergyPlusState state)
{
    printf("Starting a new environment\n");
}

void progressHandler(EnergyPlusState state, int const progress)
{
    if (oneTimeHalfway == 0 && progress > 50) {
        printf("Were halfway there!\n");
        oneTimeHalfway = 1;
    }
}

void errorHandler(EnergyPlusState state, int level, const char *message)
{
    char *warning = strstr(message, "Warning");
    if (warning) {
        numWarnings++;
    }
}

void externalHVAC(EnergyPlusState state)
{
    printf("External HVAC called\n");
}

int main(int argc, const char *argv[])
{
    //    callbackBeginNewEnvironment(BeginNewEnvironmentHandler);
    //    callbackAfterNewEnvironmentWarmupComplete(AfterNewEnvironmentWarmupCompleteHandler);
    //    callbackBeginZoneTimeStepBeforeInitHeatBalance(state, BeginZoneTimeStepBeforeInitHeatBalanceHandler);
    //    callbackBeginZoneTimeStepAfterInitHeatBalance(state, BeginZoneTimeStepAfterInitHeatBalanceHandler);
    //    callbackBeginTimeStepBeforePredictor(BeginTimeStepBeforePredictorHandler);
    //    callbackAfterPredictorBeforeHVACManagers(AfterPredictorBeforeHVACManagersHandler);
    //    callbackAfterPredictorAfterHVACManagers(AfterPredictorAfterHVACManagersHandler);
    //    callbackInsideSystemIterationLoop(InsideSystemIterationLoopHandler);
    //    callbackEndOfZoneTimeStepBeforeZoneReporting(EndOfZoneTimeStepBeforeZoneReportingHandler);
    //    callbackEndOfZoneTimeStepAfterZoneReporting(EndOfZoneTimeStepAfterZoneReportingHandler);
    //    callbackEndOfSystemTimeStepBeforeHVACReporting(EndOfSystemTimeStepBeforeHVACReportingHandler);
    //    callbackEndOfSystemTimeStepAfterHVACReporting(EndOfSystemTimeStepAfterHVACReportingHandler);
    //    callbackEndOfZoneSizing(EndOfZoneSizingHandler);
    //    callbackEndOfSystemSizing(EndOfSystemSizingHandler);
    //    callbackEndOfAfterComponentGetInput(EndOfAfterComponentGetInputHandler);
    //    callbackUnitarySystemSizing(UnitarySystemSizingHandler);
    //    registerProgressCallback(progressHandler);
    //    registerErrorCallback(errorHandler);
    EnergyPlusState state = stateNew();
    energyplus(state, argc, argv);
    if (numWarnings > 0) {
        printf("There were %d warnings!\n", numWarnings);
        numWarnings = 0;
    }
    oneTimeHalfway = 0;
    // reset and run again
    stateReset(state); // note previous callbacks are cleared here
    callbackAfterNewEnvironmentWarmupComplete(state, newEnvrnHandler);
    registerStdOutCallback(state, stdOutHandler);
    energyplus(state, argc, argv);
    if (numWarnings > 0) {
        printf("There were %d warnings!\n", numWarnings);
        numWarnings = 0;
    }
    // reset and run a test with an external hvac manager
    stateReset(state);
    registerExternalHVACManager(state, externalHVAC);
    energyplus(state, argc, argv);
    // create a new state and run it with console output muted
    printf("Running EnergyPlus with Console Output Muted...\n");
    EnergyPlusState state2 = stateNew();
    setConsoleOutputState(state2, 0);
    energyplus(state2, argc, argv);
    printf("...and it is done.");
    return 0;
}
