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
#include <EnergyPlus/Data/CommonIncludes.hh>
#include <EnergyPlus/PluginManager.hh>
#include <EnergyPlus/api/runtime.h>
#include <EnergyPlus/UtilityRoutines.hh>
#include <utility>

int energyplus(EnergyPlusState state, int argc, const char *argv[]) {
//    argv[0] = "energyplus";
//    argv[1] = "-d";
//    argv[2] = workingPath.string().c_str();
//    argv[3] = "-w";
//    argv[4] = epcomp->weatherFilePath.c_str();
//    argv[5] = "-i";
//    argv[6] = epcomp->iddPath.c_str();
//    argv[7] = epcomp->idfInputPath.c_str();
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    return runEnergyPlusAsLibrary(*thisState, argc, argv);
}

void stopSimulation(EnergyPlusState state) {
    auto thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    thisState->dataGlobal->stopSimulation = true;
}

void issueWarning(EnergyPlusState, const char * message) {
    EnergyPlus::ShowWarningError(message);
}
void issueSevere(EnergyPlusState, const char * message) {
    EnergyPlus::ShowSevereError(message);
}
void issueText(EnergyPlusState, const char * message) {
    EnergyPlus::ShowContinueError(message);
}

void registerProgressCallback(EnergyPlusState, void (*f)(int const)) {
    EnergyPlus::DataGlobals::progressCallback = f;
}

void registerStdOutCallback(EnergyPlusState EP_UNUSED(state), std::function<void (const std::string &)> f) {
    EnergyPlus::DataGlobals::messageCallback = f;
}

void registerStdOutCallback(EnergyPlusState state, void (*f)(const char *)) {
    const auto stdf = [f](const std::string & message) {
        f(message.c_str());
    };
    registerStdOutCallback(state, std::function<void (const std::string &)>(stdf));
}

void registerExternalHVACManager(EnergyPlusState state, std::function<void (EnergyPlusState)> f) {
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    thisState->dataGlobal->externalHVACManager = f;
}

void registerExternalHVACManager(EnergyPlusState state, void (*f)(EnergyPlusState)) {
    registerExternalHVACManager(state, std::function<void (EnergyPlusState)>(f));
}

void callbackBeginNewEnvironment(EnergyPlusState state, std::function<void (EnergyPlusState)> const &f) {
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    EnergyPlus::PluginManagement::registerNewCallback(*thisState, EnergyPlus::DataGlobals::emsCallFromBeginNewEvironment, f);
}

void callbackBeginNewEnvironment(EnergyPlusState state, void (*f)(EnergyPlusState)) {
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    EnergyPlus::PluginManagement::registerNewCallback(*thisState, EnergyPlus::DataGlobals::emsCallFromBeginNewEvironment, f);
}

void callbackBeginZoneTimestepBeforeSetCurrentWeather(EnergyPlusState state, std::function<void(EnergyPlusState)> const& f) {
    auto* thisState = reinterpret_cast<EnergyPlus::EnergyPlusData*>(state);
    EnergyPlus::PluginManagement::registerNewCallback(*thisState, EnergyPlus::DataGlobals::emsCallFromBeginZoneTimestepBeforeSetCurrentWeather, f);
}

void callbackBeginZoneTimestepBeforeSetCurrentWeather(EnergyPlusState state, void (*f)(EnergyPlusState)) {
    auto* thisState = reinterpret_cast<EnergyPlus::EnergyPlusData*>(state);
    EnergyPlus::PluginManagement::registerNewCallback(*thisState, EnergyPlus::DataGlobals::emsCallFromBeginZoneTimestepBeforeSetCurrentWeather, f);
}

void callbackAfterNewEnvironmentWarmupComplete(EnergyPlusState state, std::function<void (EnergyPlusState)> const &f) {
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    EnergyPlus::PluginManagement::registerNewCallback(*thisState, EnergyPlus::DataGlobals::emsCallFromBeginNewEvironmentAfterWarmUp, f);
}

void callbackAfterNewEnvironmentWarmupComplete(EnergyPlusState state, void (*f)(EnergyPlusState)) {
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    EnergyPlus::PluginManagement::registerNewCallback(*thisState, EnergyPlus::DataGlobals::emsCallFromBeginNewEvironmentAfterWarmUp, f);
}

void callbackBeginZoneTimeStepBeforeInitHeatBalance(EnergyPlusState state, std::function<void (EnergyPlusState)> const &f) {
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    EnergyPlus::PluginManagement::registerNewCallback(*thisState, EnergyPlus::DataGlobals::emsCallFromBeginZoneTimestepBeforeInitHeatBalance, f);
}

void callbackBeginZoneTimeStepBeforeInitHeatBalance(EnergyPlusState state, void (*f)(EnergyPlusState)) {
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    EnergyPlus::PluginManagement::registerNewCallback(*thisState, EnergyPlus::DataGlobals::emsCallFromBeginZoneTimestepBeforeInitHeatBalance, f);
}

void callbackBeginZoneTimeStepAfterInitHeatBalance(EnergyPlusState state, std::function<void (EnergyPlusState)> const &f) {
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    EnergyPlus::PluginManagement::registerNewCallback(*thisState, EnergyPlus::DataGlobals::emsCallFromBeginZoneTimestepAfterInitHeatBalance, f);
}

void callbackBeginZoneTimeStepAfterInitHeatBalance(EnergyPlusState state, void (*f)(EnergyPlusState)) {
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    EnergyPlus::PluginManagement::registerNewCallback(*thisState, EnergyPlus::DataGlobals::emsCallFromBeginZoneTimestepAfterInitHeatBalance, f);
}

void callbackBeginTimeStepBeforePredictor(EnergyPlusState state, std::function<void (EnergyPlusState)> const &f) {
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    EnergyPlus::PluginManagement::registerNewCallback(*thisState, EnergyPlus::DataGlobals::emsCallFromBeginTimestepBeforePredictor, f);
}

void callbackBeginTimeStepBeforePredictor(EnergyPlusState state, void (*f)(EnergyPlusState)) {
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    EnergyPlus::PluginManagement::registerNewCallback(*thisState, EnergyPlus::DataGlobals::emsCallFromBeginTimestepBeforePredictor, f);
}

void callbackAfterPredictorBeforeHVACManagers(EnergyPlusState state, std::function<void (EnergyPlusState)> const &f) {
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    EnergyPlus::PluginManagement::registerNewCallback(*thisState, EnergyPlus::DataGlobals::emsCallFromBeforeHVACManagers, f);
}

void callbackAfterPredictorBeforeHVACManagers(EnergyPlusState state, void (*f)(EnergyPlusState)) {
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    EnergyPlus::PluginManagement::registerNewCallback(*thisState, EnergyPlus::DataGlobals::emsCallFromBeforeHVACManagers, f);
}

void callbackAfterPredictorAfterHVACManagers(EnergyPlusState state, std::function<void (EnergyPlusState)> const &f) {
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    EnergyPlus::PluginManagement::registerNewCallback(*thisState, EnergyPlus::DataGlobals::emsCallFromAfterHVACManagers, f);
}

void callbackAfterPredictorAfterHVACManagers(EnergyPlusState state, void (*f)(EnergyPlusState)) {
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    EnergyPlus::PluginManagement::registerNewCallback(*thisState, EnergyPlus::DataGlobals::emsCallFromAfterHVACManagers, f);
}

void callbackInsideSystemIterationLoop(EnergyPlusState state, std::function<void (EnergyPlusState)> const &f) {
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    EnergyPlus::PluginManagement::registerNewCallback(*thisState, EnergyPlus::DataGlobals::emsCallFromHVACIterationLoop, f);
}

void callbackInsideSystemIterationLoop(EnergyPlusState state, void (*f)(EnergyPlusState)) {
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    EnergyPlus::PluginManagement::registerNewCallback(*thisState, EnergyPlus::DataGlobals::emsCallFromHVACIterationLoop, f);
}

void callbackEndOfZoneTimeStepBeforeZoneReporting(EnergyPlusState state, std::function<void (EnergyPlusState)> const &f) {
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    EnergyPlus::PluginManagement::registerNewCallback(*thisState, EnergyPlus::DataGlobals::emsCallFromEndZoneTimestepBeforeZoneReporting, f);
}

void callbackEndOfZoneTimeStepBeforeZoneReporting(EnergyPlusState state, void (*f)(EnergyPlusState)) {
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    EnergyPlus::PluginManagement::registerNewCallback(*thisState, EnergyPlus::DataGlobals::emsCallFromEndZoneTimestepBeforeZoneReporting, f);
}

void callbackEndOfZoneTimeStepAfterZoneReporting(EnergyPlusState state, std::function<void (EnergyPlusState)> const &f) {
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    EnergyPlus::PluginManagement::registerNewCallback(*thisState, EnergyPlus::DataGlobals::emsCallFromEndZoneTimestepAfterZoneReporting, f);
}

void callbackEndOfZoneTimeStepAfterZoneReporting(EnergyPlusState state, void (*f)(EnergyPlusState)) {
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    EnergyPlus::PluginManagement::registerNewCallback(*thisState, EnergyPlus::DataGlobals::emsCallFromEndZoneTimestepAfterZoneReporting, f);
}

void callbackEndOfSystemTimeStepBeforeHVACReporting(EnergyPlusState state, std::function<void (EnergyPlusState)> const &f) {
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    EnergyPlus::PluginManagement::registerNewCallback(*thisState, EnergyPlus::DataGlobals::emsCallFromEndSystemTimestepBeforeHVACReporting, f);
}

void callbackEndOfSystemTimeStepBeforeHVACReporting(EnergyPlusState state, void (*f)(EnergyPlusState)) {
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    EnergyPlus::PluginManagement::registerNewCallback(*thisState, EnergyPlus::DataGlobals::emsCallFromEndSystemTimestepBeforeHVACReporting, f);
}

void callbackEndOfSystemTimeStepAfterHVACReporting(EnergyPlusState state, std::function<void (EnergyPlusState)> const &f) {
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    EnergyPlus::PluginManagement::registerNewCallback(*thisState, EnergyPlus::DataGlobals::emsCallFromEndSystemTimestepAfterHVACReporting, f);
}

void callbackEndOfSystemTimeStepAfterHVACReporting(EnergyPlusState state, void (*f)(EnergyPlusState)) {
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    EnergyPlus::PluginManagement::registerNewCallback(*thisState, EnergyPlus::DataGlobals::emsCallFromEndSystemTimestepAfterHVACReporting, f);
}

void callbackEndOfZoneSizing(EnergyPlusState state, std::function<void (EnergyPlusState)> const &f) {
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    EnergyPlus::PluginManagement::registerNewCallback(*thisState, EnergyPlus::DataGlobals::emsCallFromZoneSizing, f);
}

void callbackEndOfZoneSizing(EnergyPlusState state, void (*f)(EnergyPlusState)) {
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    EnergyPlus::PluginManagement::registerNewCallback(*thisState, EnergyPlus::DataGlobals::emsCallFromZoneSizing, f);
}

void callbackEndOfSystemSizing(EnergyPlusState state, std::function<void (EnergyPlusState)> const &f) {
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    EnergyPlus::PluginManagement::registerNewCallback(*thisState, EnergyPlus::DataGlobals::emsCallFromSystemSizing, f);
}

void callbackEndOfSystemSizing(EnergyPlusState state, void (*f)(EnergyPlusState)) {
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    EnergyPlus::PluginManagement::registerNewCallback(*thisState, EnergyPlus::DataGlobals::emsCallFromSystemSizing, f);
}

void callbackEndOfAfterComponentGetInput(EnergyPlusState state, std::function<void (EnergyPlusState)> const &f) {
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    EnergyPlus::PluginManagement::registerNewCallback(*thisState, EnergyPlus::DataGlobals::emsCallFromComponentGetInput, f);
}

void callbackEndOfAfterComponentGetInput(EnergyPlusState state, void (*f)(EnergyPlusState)) {
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    EnergyPlus::PluginManagement::registerNewCallback(*thisState, EnergyPlus::DataGlobals::emsCallFromComponentGetInput, f);
}

//void callbackUserDefinedComponentModel(EnergyPlusState state, std::function<void ()> f) {
//    EnergyPlus::PluginManagement::registerNewCallback(EnergyPlus::DataGlobals::emsCallFromUserDefinedComponentModel, f);
//}
//
//void callbackUserDefinedComponentModel(EnergyPlusState state, void (*f)()) {
//    callbackUserDefinedComponentModel(std::function<void ()>(f));
//}

void callbackUnitarySystemSizing(EnergyPlusState state, std::function<void (EnergyPlusState)> const &f) {
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    EnergyPlus::PluginManagement::registerNewCallback(*thisState, EnergyPlus::DataGlobals::emsCallFromUnitarySystemSizing, f);
}

void callbackUnitarySystemSizing(EnergyPlusState state, void (*f)(EnergyPlusState)) {
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    EnergyPlus::PluginManagement::registerNewCallback(*thisState, EnergyPlus::DataGlobals::emsCallFromUnitarySystemSizing, f);
}
