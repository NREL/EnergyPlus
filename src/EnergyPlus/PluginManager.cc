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

#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/PluginManager.hh>

namespace EnergyPlus {
namespace PluginManager {

std::vector<void (*)()> callbacksCallFromBeginNewEvironment;
std::vector<void (*)()> callbacksCallFromZoneSizing;
std::vector<void (*)()> callbacksCallFromSystemSizing;
std::vector<void (*)()> callbacksCallFromBeginTimestepBeforePredictor;
std::vector<void (*)()> callbacksCallFromEndSystemTimestepBeforeHVACReporting;
std::vector<void (*)()> callbacksCallFromEndSystemTimestepAfterHVACReporting;
std::vector<void (*)()> callbacksCallFromBeforeHVACManagers;
std::vector<void (*)()> callbacksCallFromAfterHVACManagers;
std::vector<void (*)()> callbacksCallFromHVACIterationLoop;
std::vector<void (*)()> callbacksCallFromBeginZoneTimestepBeforeInitHeatBalance;
std::vector<void (*)()> callbacksCallFromBeginZoneTimestepAfterInitHeatBalance;
std::vector<void (*)()> callbacksCallFromEndZoneTimestepBeforeZoneReporting;
std::vector<void (*)()> callbacksCallFromBeginNewEnvironmentAfterWarmUp;
std::vector<void (*)()> callbacksCallFromUnitarySizing;
std::vector<void (*)()> callbacksCallFromExternalInterface;
std::vector<void (*)()> callbacksCallFromEndZoneTimestepAfterZoneReporting;

void registerNewCallback(int iCalledFrom, void (*f)()) {
    if (iCalledFrom == EnergyPlus::DataGlobals::emsCallFromSetupSimulation) {
        return;
    } else if (iCalledFrom == EnergyPlus::DataGlobals::emsCallFromBeginNewEvironment) {
        callbacksCallFromBeginNewEvironment.push_back(f);
    } else if (iCalledFrom == EnergyPlus::DataGlobals::emsCallFromZoneSizing) {
        callbacksCallFromZoneSizing.push_back(f);
    } else if (iCalledFrom == EnergyPlus::DataGlobals::emsCallFromSystemSizing) {
        callbacksCallFromSystemSizing.push_back(f);
    } else if (iCalledFrom == EnergyPlus::DataGlobals::emsCallFromBeginTimestepBeforePredictor) {
        callbacksCallFromBeginTimestepBeforePredictor.push_back(f);
    } else if (iCalledFrom == EnergyPlus::DataGlobals::emsCallFromEndSystemTimestepBeforeHVACReporting) {
        callbacksCallFromEndSystemTimestepBeforeHVACReporting.push_back(f);
    } else if (iCalledFrom == EnergyPlus::DataGlobals::emsCallFromEndSystemTimestepAfterHVACReporting) {
        callbacksCallFromEndSystemTimestepAfterHVACReporting.push_back(f);
    } else if (iCalledFrom == EnergyPlus::DataGlobals::emsCallFromBeforeHVACManagers) {
        callbacksCallFromBeforeHVACManagers.push_back(f);
    } else if (iCalledFrom == EnergyPlus::DataGlobals::emsCallFromAfterHVACManagers) {
        callbacksCallFromAfterHVACManagers.push_back(f);
    } else if (iCalledFrom == EnergyPlus::DataGlobals::emsCallFromHVACIterationLoop) {
        callbacksCallFromHVACIterationLoop.push_back(f);
    } else if (iCalledFrom == EnergyPlus::DataGlobals::emsCallFromBeginZoneTimestepBeforeInitHeatBalance) {
        callbacksCallFromBeginZoneTimestepBeforeInitHeatBalance.push_back(f);
    } else if (iCalledFrom == EnergyPlus::DataGlobals::emsCallFromBeginZoneTimestepAfterInitHeatBalance) {
        callbacksCallFromBeginZoneTimestepAfterInitHeatBalance.push_back(f);
    } else if (iCalledFrom == EnergyPlus::DataGlobals::emsCallFromEndZoneTimestepBeforeZoneReporting) {
        callbacksCallFromEndZoneTimestepBeforeZoneReporting.push_back(f);
    } else if (iCalledFrom == EnergyPlus::DataGlobals::emsCallFromEndZoneTimestepAfterZoneReporting) {
        callbacksCallFromEndZoneTimestepAfterZoneReporting.push_back(f);
    } else if (iCalledFrom == EnergyPlus::DataGlobals::emsCallFromBeginNewEvironmentAfterWarmUp) {
        callbacksCallFromBeginNewEnvironmentAfterWarmUp.push_back(f);
    } else if (iCalledFrom == EnergyPlus::DataGlobals::emsCallFromUnitarySystemSizing) {
        callbacksCallFromUnitarySizing.push_back(f);
    } else if (iCalledFrom == EnergyPlus::DataGlobals::emsCallFromExternalInterface) {
        callbacksCallFromExternalInterface.push_back(f);
    }
}

void runAnyRegisteredCallbacks(int iCalledFrom) {
    if (iCalledFrom == EnergyPlus::DataGlobals::emsCallFromSetupSimulation) {
        return;
    } else if (iCalledFrom == EnergyPlus::DataGlobals::emsCallFromBeginNewEvironment) {
        for (auto const & cb : callbacksCallFromBeginNewEvironment) {
            cb();
        }
    } else if (iCalledFrom == EnergyPlus::DataGlobals::emsCallFromZoneSizing) {
        for (auto const & cb : callbacksCallFromZoneSizing) {
            cb();
        }
    } else if (iCalledFrom == EnergyPlus::DataGlobals::emsCallFromSystemSizing) {
        for (auto const & cb : callbacksCallFromSystemSizing) {
            cb();
        }
    } else if (iCalledFrom == EnergyPlus::DataGlobals::emsCallFromBeginTimestepBeforePredictor) {
        for (auto const & cb : callbacksCallFromBeginTimestepBeforePredictor) {
            cb();
        }
    } else if (iCalledFrom == EnergyPlus::DataGlobals::emsCallFromEndSystemTimestepBeforeHVACReporting) {
        for (auto const & cb : callbacksCallFromEndSystemTimestepBeforeHVACReporting) {
            cb();
        }
    } else if (iCalledFrom == EnergyPlus::DataGlobals::emsCallFromEndSystemTimestepAfterHVACReporting) {
        for (auto const & cb : callbacksCallFromEndSystemTimestepAfterHVACReporting) {
            cb();
        }
    } else if (iCalledFrom == EnergyPlus::DataGlobals::emsCallFromBeforeHVACManagers) {
        for (auto const & cb : callbacksCallFromBeforeHVACManagers) {
            cb();
        }
    } else if (iCalledFrom == EnergyPlus::DataGlobals::emsCallFromAfterHVACManagers) {
        for (auto const & cb : callbacksCallFromAfterHVACManagers) {
            cb();
        }
    } else if (iCalledFrom == EnergyPlus::DataGlobals::emsCallFromHVACIterationLoop) {
        for (auto const & cb : callbacksCallFromHVACIterationLoop) {
            cb();
        }
    } else if (iCalledFrom == EnergyPlus::DataGlobals::emsCallFromBeginZoneTimestepBeforeInitHeatBalance) {
        for (auto const & cb : callbacksCallFromBeginZoneTimestepBeforeInitHeatBalance) {
            cb();
        }
    } else if (iCalledFrom == EnergyPlus::DataGlobals::emsCallFromBeginZoneTimestepAfterInitHeatBalance) {
        for (auto const & cb : callbacksCallFromBeginZoneTimestepAfterInitHeatBalance) {
            cb();
        }
    } else if (iCalledFrom == EnergyPlus::DataGlobals::emsCallFromEndZoneTimestepBeforeZoneReporting) {
        for (auto const & cb : callbacksCallFromEndZoneTimestepBeforeZoneReporting) {
            cb();
        }
    } else if (iCalledFrom == EnergyPlus::DataGlobals::emsCallFromEndZoneTimestepAfterZoneReporting) {
        for (auto const & cb : callbacksCallFromEndZoneTimestepAfterZoneReporting) {
            cb();
        }
    } else if (iCalledFrom == EnergyPlus::DataGlobals::emsCallFromBeginNewEvironmentAfterWarmUp) {
        for (auto const & cb : callbacksCallFromBeginNewEnvironmentAfterWarmUp) {
            cb();
        }
    } else if (iCalledFrom == EnergyPlus::DataGlobals::emsCallFromUnitarySystemSizing) {
        for (auto const & cb : callbacksCallFromUnitarySizing) {
            cb();
        }
    } else if (iCalledFrom == EnergyPlus::DataGlobals::emsCallFromExternalInterface) {
        for (auto const & cb : callbacksCallFromExternalInterface) {
            cb();
        }
    }
}

void clear_state() {
    callbacksCallFromBeginNewEvironment.empty();
    callbacksCallFromZoneSizing.empty();
    callbacksCallFromSystemSizing.empty();
    callbacksCallFromBeginTimestepBeforePredictor.empty();
    callbacksCallFromEndSystemTimestepBeforeHVACReporting.empty();
    callbacksCallFromEndSystemTimestepAfterHVACReporting.empty();
    callbacksCallFromBeforeHVACManagers.empty();
    callbacksCallFromAfterHVACManagers.empty();
    callbacksCallFromHVACIterationLoop.empty();
    callbacksCallFromBeginZoneTimestepBeforeInitHeatBalance.empty();
    callbacksCallFromBeginZoneTimestepAfterInitHeatBalance.empty();
    callbacksCallFromEndZoneTimestepBeforeZoneReporting.empty();
    callbacksCallFromBeginNewEnvironmentAfterWarmUp.empty();
    callbacksCallFromUnitarySizing.empty();
    callbacksCallFromExternalInterface.empty();
    callbacksCallFromEndZoneTimestepAfterZoneReporting.empty();
}


}
}
