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

#include <EnergyPlus/api/datatransfer.h>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataRuntimeLanguage.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/PluginManager.hh>

void dataTransferNoOp() {}

void requestVariable(const char* type, const char* key) {
    // allow specifying a request for an output variable, so that E+ does not have to keep all of them in memory
    // should be called before energyplus is run!
    EnergyPlus::OutputProcessor::APIOutputVariableRequest request;
    request.varName = type;
    request.varKey = key;
    EnergyPlus::OutputProcessor::apiVarRequests.push_back(request);
}

int getVariableHandle(const char* type, const char* key) {
    std::string const typeUC = EnergyPlus::UtilityRoutines::MakeUPPERCase(type);
    std::string const keyUC = EnergyPlus::UtilityRoutines::MakeUPPERCase(key);
    int handle;
    handle = 0;
    if (!EnergyPlus::OutputProcessor::RVariableTypes.allocated()) {
        return -2;
    }
    for (auto const & availOutputVar : EnergyPlus::OutputProcessor::RVariableTypes) {
        handle++;
        if (typeUC == availOutputVar.VarNameOnlyUC && keyUC == availOutputVar.KeyNameOnlyUC) {
            return handle;
        }
    }
    return -1;
}

int getMeterHandle(const char* meterName) {
    std::string const meterNameUC = EnergyPlus::UtilityRoutines::MakeUPPERCase(meterName);
    auto i = EnergyPlus::GetMeterIndex(meterNameUC);
    if (i == 0) {
        // inside E+, zero is meaningful, but through the API, I want to use negative one as a signal of a bad lookup
        return -1;
    } else {
        return i;
    }
}

int getActuatorHandle(const char* uniqueKey, const char* componentType, const char* controlType) {
    int handle;
    handle = 0;
    std::string const typeUC = EnergyPlus::UtilityRoutines::MakeUPPERCase(componentType);
    std::string const keyUC = EnergyPlus::UtilityRoutines::MakeUPPERCase(uniqueKey);
    std::string const controlUC = EnergyPlus::UtilityRoutines::MakeUPPERCase(controlType);
    for (auto const & availActuator : EnergyPlus::DataRuntimeLanguage::EMSActuatorAvailable) {
        handle++;
        std::string const actuatorTypeUC = EnergyPlus::UtilityRoutines::MakeUPPERCase(availActuator.ComponentTypeName);
        std::string const actuatorIDUC = EnergyPlus::UtilityRoutines::MakeUPPERCase(availActuator.UniqueIDName);
        std::string const actuatorControlUC = EnergyPlus::UtilityRoutines::MakeUPPERCase(availActuator.ControlTypeName);
        if (typeUC == actuatorTypeUC && keyUC == actuatorIDUC && controlUC == actuatorControlUC) {
            return handle;
        }
    }
    return -1;
}

double getVariableValue(const int handle) {
    // I'm not sure whether to validate the handle range here, even if I check for positive, they could still go
    // out of range too high.  I'm inclined to just let it fail?  Maybe?
    // If it is to be checked, just do
    //    if (handle < 0) {
    //        return -999;
    //    }
    return EnergyPlus::OutputProcessor::RVariableTypes(handle).VarPtr().Which;
}

double getMeterValue(int handle) {
    // I'm not sure whether to validate the handle range here, even if I check for positive, they could still go
    // out of range too high.  I'm inclined to just let it fail?  Maybe?
    // If it is to be checked, just do
    //    if (handle < 0) {
    //        return -999;
    //    }
    return EnergyPlus::GetCurrentMeterValue(handle);
}

void setActuatorValue(const int handle, const double value) {
    // I could imagine returning a 0 or 1, but it would really only be validating the handle was in range
    // the handle is based on the available actuator list
    auto & theActuator(EnergyPlus::DataRuntimeLanguage::EMSActuatorAvailable(handle));
    if (theActuator.RealValue.associated()) {
        theActuator.RealValue = value;
    } else {
        // try falling back to integer assignment; // TODO: Address this later
        theActuator.IntValue = value;
    }
    theActuator.Actuated = true;
}

void resetActuator(int handle) {
    // resets the actuator so that E+ will use the internally calculated value again
    auto & theActuator(EnergyPlus::DataRuntimeLanguage::EMSActuatorAvailable(handle));
    theActuator.Actuated = false;
}


int getPluginGlobalVariableHandle(const char* name) {
    return EnergyPlus::PluginManagement::pluginManager->getGlobalVariableHandle(name);
}

Real64 getPluginGlobalVariableValue(int handle) {
    return EnergyPlus::PluginManagement::pluginManager->getGlobalVariableValue(handle);
}

void setPluginGlobalVariableValue(int handle, Real64 value) {
    EnergyPlus::PluginManagement::pluginManager->setGlobalVariableValue(handle, value);
}

int getPluginTrendVariableHandle(const char* name) {
    return EnergyPlus::PluginManagement::pluginManager->getTrendVariableHandle(name);
}
Real64 getPluginTrendVariableValue(int handle, int timeIndex) {
    return EnergyPlus::PluginManagement::pluginManager->getTrendVariableValue(handle, timeIndex);
}

int getInternalVariableHandle(const char* type, const char* key) {
    int handle;
    handle = 0;
    std::string const typeUC = EnergyPlus::UtilityRoutines::MakeUPPERCase(type);
    std::string const keyUC = EnergyPlus::UtilityRoutines::MakeUPPERCase(key);
    for (auto const & availVariable : EnergyPlus::DataRuntimeLanguage::EMSInternalVarsAvailable) {
        handle++;
        std::string const variableTypeUC = EnergyPlus::UtilityRoutines::MakeUPPERCase(availVariable.DataTypeName);
        std::string const variableIDUC = EnergyPlus::UtilityRoutines::MakeUPPERCase(availVariable.UniqueIDName);
        if (typeUC == variableTypeUC && keyUC == variableIDUC) {
            return handle;
        }
    }
    return 0;
}

Real64 getInternalVariableValue(int handle) {
    if (handle == 0) {
        return 1;
    }
    auto thisVar = EnergyPlus::DataRuntimeLanguage::EMSInternalVarsAvailable(handle);
    if (thisVar.PntrVarTypeUsed == EnergyPlus::DataRuntimeLanguage::PntrReal) {
        return thisVar.RealValue();
    } else if (thisVar.PntrVarTypeUsed == EnergyPlus::DataRuntimeLanguage::PntrInteger) {
        return (Real64)thisVar.IntValue();
    } // Doesn't look like this struct actually has a logical member type, so skipping that
    return 1;
}

int year() {
    return EnergyPlus::DataEnvironment::Year;
}
int month() {
    return EnergyPlus::DataEnvironment::Month;
}
int dayOfMonth() {
    return EnergyPlus::DataEnvironment::DayOfMonth;
}
int dayOfWeek() {
    return EnergyPlus::DataEnvironment::DayOfWeek;
}
int dayOfYear() {
    return EnergyPlus::DataEnvironment::DayOfYear;
}
int daylightSavingsTimeIndicator() {
    return EnergyPlus::DataEnvironment::DSTIndicator;
}
int hour() {
    return EnergyPlus::DataGlobals::HourOfDay - 1; // no, just stay on 0..23+ DSTadjust ! offset by 1 and daylight savings time
}
Real64 currentTime() {
    if (EnergyPlus::DataHVACGlobals::TimeStepSys < EnergyPlus::DataGlobals::TimeStepZone) {
        // CurrentTime is for end of zone timestep, need to account for system timestep
        return EnergyPlus::DataGlobals::CurrentTime - EnergyPlus::DataGlobals::TimeStepZone + EnergyPlus::DataHVACGlobals::SysTimeElapsed + EnergyPlus::DataHVACGlobals::TimeStepSys;
    } else {
        return EnergyPlus::DataGlobals::CurrentTime;
    }
}
int minutes() {
    // the -1 is to push us to the right minute, but this should be handled cautiously because if we are inside the HVAC iteration loop,
    // currentTime() returns a floating point fractional hour, so truncation could put this a few seconds from the expected minute.
    return ((int)std::round(currentTime()) - EnergyPlus::DataGlobals::HourOfDay - 1) * 60;
}
int holidayIndex() {
    return EnergyPlus::DataEnvironment::HolidayIndex;
}
int sunIsUp() { // maintain response convention from previous (EMS) implementation
    if (EnergyPlus::DataEnvironment::SunIsUp) {
        return 1;
    } else {
        return 0;
    }
}
int isRaining() {
    if (EnergyPlus::DataEnvironment::IsRain) {
        return 1;
    } else {
        return 0;
    }
}
int warmupFlag() {
    if (EnergyPlus::DataGlobals::WarmupFlag) {
        return 1;
    } else {
        return 0;
    }
}
Real64 systemTimeStep() {
    return EnergyPlus::DataHVACGlobals::TimeStepSys;
}
int currentEnvironmentNum() {
    return EnergyPlus::DataEnvironment::CurEnvirNum;
}
int kindOfSim() {
    return EnergyPlus::DataGlobals::KindOfSim;
}
int getConstructionHandle(const char* constructionName) {
    int handle;
    handle = 0;
    std::string const nameUC = EnergyPlus::UtilityRoutines::MakeUPPERCase(constructionName);
    for (auto const & construct : EnergyPlus::DataHeatBalance::Construct) {
        handle++;
        std::string const thisNameUC = EnergyPlus::UtilityRoutines::MakeUPPERCase(construct.Name);
        if (nameUC == thisNameUC) {
            return handle;
        }
    }
    return 0;
}
