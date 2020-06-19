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

#include <EnergyPlus/api/EnergyPlusPgm.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/PluginManager.hh>
#include <EnergyPlus/api/runtime.h>
#include <EnergyPlus/StateManagement.hh>
#include <EnergyPlus/UtilityRoutines.hh>

void cClearAllStates() {
    EnergyPlus::EnergyPlusData state;   //THIS IS TEMPORARY
    EnergyPlus::clearThisState(state);
    EnergyPlus::clearAllStates(state.outputFiles);
}

int energyplus(int argc, const char *argv[]) {
//    argv[0] = "energyplus";
//    argv[1] = "-d";
//    argv[2] = workingPath.string().c_str();
//    argv[3] = "-w";
//    argv[4] = epcomp->weatherFilePath.c_str();
//    argv[5] = "-i";
//    argv[6] = epcomp->iddPath.c_str();
//    argv[7] = epcomp->idfInputPath.c_str();

    EnergyPlus::EnergyPlusData state;
    return runEnergyPlusAsLibrary(state, argc, argv);
}

void stopSimulation() {
  EnergyPlus::DataGlobals::stopSimulation = true;
}

void issueWarning(const char * message) {
    EnergyPlus::ShowWarningError(message);
}
void issueSevere(const char * message) {
    EnergyPlus::ShowSevereError(message);
}
void issueText(const char * message) {
    EnergyPlus::ShowContinueError(message);
}

void registerProgressCallback(void (*f)(int const)) {
    EnergyPlus::DataGlobals::progressCallback = f;
}

void registerStdOutCallback(void (*f)(const char *)) {
    const auto stdf = [f](const std::string & message) {
        f(message.c_str());
    };
    registerStdOutCallback(stdf);
}

void registerStdOutCallback(std::function<void (const std::string &)> f) {
    EnergyPlus::DataGlobals::messageCallback = f;
}

void callbackBeginNewEnvironment(std::function<void ()> f) {
    EnergyPlus::PluginManagement::registerNewCallback(EnergyPlus::DataGlobals::emsCallFromBeginNewEvironment, f);
}

void callbackBeginNewEnvironment(void (*f)()) {
    callbackBeginNewEnvironment(std::function<void ()>(f));
}

void callbackAfterNewEnvironmentWarmupComplete(std::function<void ()> f) {
    EnergyPlus::PluginManagement::registerNewCallback(EnergyPlus::DataGlobals::emsCallFromBeginNewEvironmentAfterWarmUp, f);
}

void callbackAfterNewEnvironmentWarmupComplete(void (*f)()) {
  callbackAfterNewEnvironmentWarmupComplete(std::function<void ()>(f));
}

void callbackBeginZoneTimeStepBeforeInitHeatBalance(std::function<void ()> f) {
    EnergyPlus::PluginManagement::registerNewCallback(EnergyPlus::DataGlobals::emsCallFromBeginZoneTimestepBeforeInitHeatBalance, f);
}

void callbackBeginZoneTimeStepBeforeInitHeatBalance(void (*f)()) {
    callbackBeginZoneTimeStepBeforeInitHeatBalance(std::function<void ()>(f));
}

void callbackBeginZoneTimeStepAfterInitHeatBalance(std::function<void ()> f) {
    EnergyPlus::PluginManagement::registerNewCallback(EnergyPlus::DataGlobals::emsCallFromBeginZoneTimestepAfterInitHeatBalance, f);
}

void callbackBeginZoneTimeStepAfterInitHeatBalance(void (*f)()) {
    callbackBeginZoneTimeStepAfterInitHeatBalance(std::function<void ()>(f));
}

void callbackBeginTimeStepBeforePredictor(std::function<void ()> f) {
    EnergyPlus::PluginManagement::registerNewCallback(EnergyPlus::DataGlobals::emsCallFromBeginTimestepBeforePredictor, f);
}

void callbackBeginTimeStepBeforePredictor(void (*f)()) {
    callbackBeginTimeStepBeforePredictor(std::function<void ()>(f));
}

void callbackAfterPredictorBeforeHVACManagers(std::function<void ()> f) {
    EnergyPlus::PluginManagement::registerNewCallback(EnergyPlus::DataGlobals::emsCallFromBeforeHVACManagers, f);
}

void callbackAfterPredictorBeforeHVACManagers(void (*f)()) {
    callbackAfterPredictorBeforeHVACManagers(std::function<void ()>(f));
}

void callbackAfterPredictorAfterHVACManagers(std::function<void ()> f) {
    EnergyPlus::PluginManagement::registerNewCallback(EnergyPlus::DataGlobals::emsCallFromAfterHVACManagers, f);
}

void callbackAfterPredictorAfterHVACManagers(void (*f)()) {
    callbackAfterPredictorAfterHVACManagers(std::function<void ()>(f));
}

void callbackInsideSystemIterationLoop(std::function<void ()> f) {
    EnergyPlus::PluginManagement::registerNewCallback(EnergyPlus::DataGlobals::emsCallFromHVACIterationLoop, f);
}

void callbackInsideSystemIterationLoop(void (*f)()) {
    callbackInsideSystemIterationLoop(std::function<void ()>(f));
}

void callbackEndOfZoneTimeStepBeforeZoneReporting(std::function<void ()> f) {
    EnergyPlus::PluginManagement::registerNewCallback(EnergyPlus::DataGlobals::emsCallFromEndZoneTimestepBeforeZoneReporting, f);
}

void callbackEndOfZoneTimeStepBeforeZoneReporting(void (*f)()) {
    callbackEndOfZoneTimeStepBeforeZoneReporting(std::function<void ()>(f));
}

void callbackEndOfZoneTimeStepAfterZoneReporting(std::function<void ()> f) {
    EnergyPlus::PluginManagement::registerNewCallback(EnergyPlus::DataGlobals::emsCallFromEndZoneTimestepAfterZoneReporting, f);
}

void callbackEndOfZoneTimeStepAfterZoneReporting(void (*f)()) {
    callbackEndOfZoneTimeStepAfterZoneReporting(std::function<void ()>(f));
}

void callbackEndOfSystemTimeStepBeforeHVACReporting(std::function<void ()> f) {
    EnergyPlus::PluginManagement::registerNewCallback(EnergyPlus::DataGlobals::emsCallFromEndSystemTimestepBeforeHVACReporting, f);
}

void callbackEndOfSystemTimeStepBeforeHVACReporting(void (*f)()) {
    callbackEndOfSystemTimeStepBeforeHVACReporting(std::function<void ()>(f));
}

void callbackEndOfSystemTimeStepAfterHVACReporting(std::function<void ()> f) {
    EnergyPlus::PluginManagement::registerNewCallback(EnergyPlus::DataGlobals::emsCallFromEndSystemTimestepAfterHVACReporting, f);
}

void callbackEndOfSystemTimeStepAfterHVACReporting(void (*f)()) {
    callbackEndOfSystemTimeStepAfterHVACReporting(std::function<void ()>(f));
}

void callbackEndOfZoneSizing(std::function<void ()> f) {
    EnergyPlus::PluginManagement::registerNewCallback(EnergyPlus::DataGlobals::emsCallFromZoneSizing, f);
}

void callbackEndOfZoneSizing(void (*f)()) {
    callbackEndOfZoneSizing(std::function<void ()>(f));
}

void callbackEndOfSystemSizing(std::function<void ()> f) {
    EnergyPlus::PluginManagement::registerNewCallback(EnergyPlus::DataGlobals::emsCallFromSystemSizing, f);
}

void callbackEndOfSystemSizing(void (*f)()) {
    callbackEndOfSystemSizing(std::function<void ()>(f));
}

void callbackEndOfAfterComponentGetInput(std::function<void ()> f) {
    EnergyPlus::PluginManagement::registerNewCallback(EnergyPlus::DataGlobals::emsCallFromComponentGetInput, f);
}

void callbackEndOfAfterComponentGetInput(void (*f)()) {
    callbackEndOfAfterComponentGetInput(std::function<void ()>(f));
}

//void callbackUserDefinedComponentModel(std::function<void ()> f) {
//    EnergyPlus::PluginManagement::registerNewCallback(EnergyPlus::DataGlobals::emsCallFromUserDefinedComponentModel, f);
//}
//
//void callbackUserDefinedComponentModel(void (*f)()) {
//    callbackUserDefinedComponentModel(std::function<void ()>(f));
//}

void callbackUnitarySystemSizing(std::function<void ()> f) {
    EnergyPlus::PluginManagement::registerNewCallback(EnergyPlus::DataGlobals::emsCallFromUnitarySystemSizing, f);
}

void callbackUnitarySystemSizing(void (*f)()) {
    callbackUnitarySystemSizing(std::function<void ()>(f));
}

