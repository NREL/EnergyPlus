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

#include <cmath>

#include <ObjexxFCL/ArrayS.functions.hh>
#include <ObjexxFCL/time.hh>

#include <EnergyPlus/Construction.hh>
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataRuntimeLanguage.hh>
#include <EnergyPlus/HeatBalFiniteDiffManager.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/PluginManager.hh>
#include <EnergyPlus/RuntimeLanguageProcessor.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/WeatherManager.hh>
#include <EnergyPlus/api/datatransfer.h>
#include <EnergyPlus/api/runtime.h>

char *listAllAPIDataCSV(EnergyPlusState state)
{
    auto thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    std::string output = "**ACTUATORS**\n";
    for (auto const &availActuator : thisState->dataRuntimeLang->EMSActuatorAvailable) {
        if (availActuator.ComponentTypeName.empty() && availActuator.UniqueIDName.empty() && availActuator.ControlTypeName.empty()) {
            break;
        }
        output.append("Actuator,");
        output.append(availActuator.ComponentTypeName).append(",");
        output.append(availActuator.ControlTypeName).append(",");
        output.append(availActuator.UniqueIDName).append(";\n");
    }
    output.append("**INTERNAL_VARIABLES**\n");
    for (auto const &availVariable : thisState->dataRuntimeLang->EMSInternalVarsAvailable) {
        if (availVariable.DataTypeName.empty() && availVariable.UniqueIDName.empty()) {
            break;
        }
        output.append("InternalVariable,");
        output.append(availVariable.DataTypeName).append(",");
        output.append(availVariable.UniqueIDName).append("\n");
    }
    output.append("**PLUGIN_GLOBAL_VARIABLES**\n");
    for (auto const &gVarName : thisState->dataPluginManager->globalVariableNames) {
        output.append("PluginGlobalVariable,");
        output.append(gVarName).append("\n");
    }
    output.append("**TRENDS**\n");
    for (auto const &trend : thisState->dataPluginManager->trends) {
        output.append("PluginTrendVariable,");
        output.append(trend.name).append("\n");
    }
    output.append("**METERS**\n");
    for (auto const &meter : thisState->dataOutputProcessor->EnergyMeters) {
        if (meter.Name.empty()) {
            break;
        }
        output.append("OutputMeter,");
        output.append(meter.Name).append("\n");
    }
    output.append("**VARIABLES**\n");
    for (auto const &variable : thisState->dataOutputProcessor->RVariableTypes) {
        if (variable.VarNameOnly.empty() && variable.KeyNameOnlyUC.empty()) {
            break;
        }
        output.append("OutputVariable,");
        output.append(variable.VarNameOnly).append(",");
        output.append(variable.KeyNameOnlyUC).append("\n");
    }
    // note that we cannot just return a c_str to the local string, as the string will be destructed upon leaving
    // this function, and undefined behavior will occur.
    // instead make a deep copy, and the user must manage the new char * pointer
    char *p = new char[std::strlen(output.c_str())];
    std::strcpy(p, output.c_str());
    return p;
}

int apiDataFullyReady(EnergyPlusState state)
{
    auto thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    if (thisState->dataPluginManager->fullyReady) {
        return 0;
    }
    return 1;
}

int apiErrorFlag(EnergyPlusState state)
{
    auto thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    if (thisState->dataPluginManager->apiErrorFlag) {
        return 1;
    } else {
        return 0;
    }
}

void resetErrorFlag(EnergyPlusState state)
{
    auto thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    thisState->dataPluginManager->apiErrorFlag = false;
}

int getNumNodesInCondFDSurfaceLayer(EnergyPlusState state, const char *surfName, const char *matName)
{
    auto thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    auto UCsurfName = EnergyPlus::UtilityRoutines::MakeUPPERCase(surfName);
    auto UCmatName = EnergyPlus::UtilityRoutines::MakeUPPERCase(matName);
    return EnergyPlus::HeatBalFiniteDiffManager::numNodesInMaterialLayer(*thisState, UCsurfName, UCmatName);
}

void requestVariable(EnergyPlusState state, const char *type, const char *key)
{
    // allow specifying a request for an output variable, so that E+ does not have to keep all of them in memory
    // should be called before energyplus is run!
    // note that the variable request array is cleared during clear_state, so if you run multiple E+ runs, these must be requested again each time.
    auto thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    EnergyPlus::OutputProcessor::APIOutputVariableRequest request;
    request.varName = type;
    request.varKey = key;
    thisState->dataOutputProcessor->apiVarRequests.push_back(request);
}

int getVariableHandle(EnergyPlusState state, const char *type, const char *key)
{
    // Variables are accessed through a single integer ID, but there are multiple internal types: real and integer.
    // I am going to make the integer handle span all both types, by carefully defining the handle.
    // basically, the handles are contiguous, with:
    //  - index 1 being the first real variable handle
    //  - index N being the highest real variable handle
    //  - index N+1 being the first integer variable handle
    //  - index N+M being the highest integer variable handle
    // In this function, it is as simple as looping over both types and continuing to increment
    // the handle carefully.  In the getValue function it is just a matter of checking array sizes.
    auto thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    std::string const typeUC = EnergyPlus::UtilityRoutines::MakeUPPERCase(type);
    std::string const keyUC = EnergyPlus::UtilityRoutines::MakeUPPERCase(key);
    int handle = -1; // initialize to -1 as a flag
    if (thisState->dataOutputProcessor->RVariableTypes.allocated()) {
        handle = 0; // initialize to 0 to get a 1 based Array1D index
        for (int i = 1; i <= thisState->dataOutputProcessor->NumOfRVariable; i++) {
            auto &availOutputVar = thisState->dataOutputProcessor->RVariableTypes(i);
            handle++;
            if (typeUC == availOutputVar.VarNameOnlyUC && keyUC == availOutputVar.KeyNameOnlyUC) {
                return handle;
            }
        }
    }
    if (thisState->dataOutputProcessor->IVariableTypes.allocated()) {
        // now, if real variables *were* searched, we need to pick up the handle where it left off, otherwise initialize it to zero
        if (handle == -1) {
            // real variables were not searched, init to zero
            handle = 0;
        } else {
            // real variables where searched, let it just continue where it left off
        }
        for (int i = 1; i <= thisState->dataOutputProcessor->NumOfIVariable; i++) {
            auto &availOutputVar = thisState->dataOutputProcessor->IVariableTypes(i);
            handle++;
            if (typeUC == availOutputVar.VarNameOnlyUC && keyUC == availOutputVar.KeyNameOnlyUC) {
                return handle;
            }
        }
    }
    return -1; // return -1 if it wasn't found
}

Real64 getVariableValue(EnergyPlusState state, const int handle)
{
    // this function works in conjunction with the plan set up in getVariableHandle
    // basically, the handles are contiguous, with:
    //  - index 1 being the first real variable handle
    //  - index N being the highest real variable handle
    //  - index N+1 being the first integer variable handle
    //  - index N+M being the highest integer variable handle
    // note that this function will return -1 if it cannot
    auto thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    if (handle > 0 && handle <= thisState->dataOutputProcessor->NumOfRVariable) {
        auto &thisOutputVar = thisState->dataOutputProcessor->RVariableTypes(handle);
        return *thisOutputVar.VarPtr.Which;
    } else if (handle > thisState->dataOutputProcessor->NumOfRVariable &&
               handle <= thisState->dataOutputProcessor->NumOfRVariable + thisState->dataOutputProcessor->NumOfIVariable) {
        int thisHandle = handle - thisState->dataOutputProcessor->NumOfRVariable;
        auto &thisOutputVar = thisState->dataOutputProcessor->IVariableTypes(thisHandle);
        return (Real64)*thisOutputVar.VarPtr.Which;
    } else {
        if (thisState->dataGlobal->errorCallback) {
            std::cout << "ERROR: Variable handle out of range in getVariableValue, returning zero but caller should take note and likely abort."
                      << std::endl;
        } else {
            // must be running from python plugin, need to fatal out once the plugin is done
            // throw an error, set the fatal flag, and then return zero
            EnergyPlus::ShowSevereError(*thisState, fmt::format("Data Exchange API: Index error in getVariableValue; received handle: {}", handle));
            EnergyPlus::ShowContinueError(
                *thisState, "The getVariableValue function will return 0 for now to allow the plugin to finish, then EnergyPlus will abort");
        }
        thisState->dataPluginManager->apiErrorFlag = true;
        return 0;
    }
}

int getMeterHandle(EnergyPlusState state, const char *meterName)
{
    auto thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    std::string const meterNameUC = EnergyPlus::UtilityRoutines::MakeUPPERCase(meterName);
    auto i = EnergyPlus::GetMeterIndex(*thisState, meterNameUC);
    if (i == 0) {
        // inside E+, zero is meaningful, but through the API, I want to use negative one as a signal of a bad lookup
        return -1;
    } else {
        return i;
    }
}

Real64 getMeterValue(EnergyPlusState state, int handle)
{
    auto thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    if (handle >= 1 && handle <= (int)thisState->dataOutputProcessor->EnergyMeters.size()) {
        return EnergyPlus::GetCurrentMeterValue(*thisState, handle);
    } else {
        if (thisState->dataGlobal->errorCallback) {
            std::cout << "ERROR: Meter handle out of range in getMeterValue, returning zero but caller should take note and likely abort."
                      << std::endl;
        } else {
            // must be running from python plugin, need to fatal out once the plugin is done
            // throw an error, set the fatal flag, and then return zero
            EnergyPlus::ShowSevereError(*thisState, fmt::format("Data Exchange API: Index error in getMeterValue; received handle: {}", handle));
            EnergyPlus::ShowContinueError(
                *thisState, "The getMeterValue function will return 0 for now to allow the plugin to finish, then EnergyPlus will abort");
        }
        thisState->dataPluginManager->apiErrorFlag = true;
        return 0;
    }
}

int getActuatorHandle(EnergyPlusState state, const char *componentType, const char *controlType, const char *uniqueKey)
{
    int handle = 0;
    std::string const typeUC = EnergyPlus::UtilityRoutines::MakeUPPERCase(componentType);
    std::string const keyUC = EnergyPlus::UtilityRoutines::MakeUPPERCase(uniqueKey);
    std::string const controlUC = EnergyPlus::UtilityRoutines::MakeUPPERCase(controlType);
    auto thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    for (int ActuatorLoop = 1; ActuatorLoop <= thisState->dataRuntimeLang->numEMSActuatorsAvailable; ++ActuatorLoop) {
        auto &availActuator = thisState->dataRuntimeLang->EMSActuatorAvailable(ActuatorLoop);
        handle++;
        std::string const actuatorTypeUC = EnergyPlus::UtilityRoutines::MakeUPPERCase(availActuator.ComponentTypeName);
        std::string const actuatorIDUC = EnergyPlus::UtilityRoutines::MakeUPPERCase(availActuator.UniqueIDName);
        std::string const actuatorControlUC = EnergyPlus::UtilityRoutines::MakeUPPERCase(availActuator.ControlTypeName);
        if (typeUC == actuatorTypeUC && keyUC == actuatorIDUC && controlUC == actuatorControlUC) {

            if (availActuator.handleCount > 0) {
                // If the handle is already used by an IDF EnergyManagementSystem:Actuator, we should warn the user
                bool foundActuator = false;
                for (int ActuatorLoopUsed = 1; ActuatorLoopUsed <= thisState->dataRuntimeLang->numActuatorsUsed; ++ActuatorLoopUsed) {
                    auto const &usedActuator = thisState->dataRuntimeLang->EMSActuatorUsed(ActuatorLoopUsed);
                    if (usedActuator.ActuatorVariableNum == handle) {
                        EnergyPlus::ShowWarningError(
                            *thisState,
                            "Data Exchange API: An EnergyManagementSystem:Actuator seems to be already defined in the EnergyPlus File and named '" +
                                usedActuator.Name + "'.");
                        EnergyPlus::ShowContinueError(
                            *thisState, "Occurred for componentType='" + typeUC + "', controlType='" + controlUC + "', uniqueKey='" + keyUC + "'.");
                        EnergyPlus::ShowContinueError(*thisState,
                                                      fmt::format("The getActuatorHandle function will still return the handle (= {}) but caller "
                                                                  "should take note that there is a risk of overwritting.",
                                                                  handle));
                        foundActuator = true;
                        break;
                    }
                }
                if (!foundActuator) {
                    EnergyPlus::ShowWarningError(*thisState,
                                                 "Data Exchange API: You seem to already have tried to get an Actuator Handle on this one.");
                    EnergyPlus::ShowContinueError(
                        *thisState, "Occurred for componentType='" + typeUC + "', controlType='" + controlUC + "', uniqueKey='" + keyUC + "'.");
                    EnergyPlus::ShowContinueError(*thisState,
                                                  fmt::format("The getActuatorHandle function will still return the handle (= {}) but caller should "
                                                              "take note that there is a risk of overwritting.",
                                                              handle));
                }
            }
            ++availActuator.handleCount;

            return handle;
        }
    }
    return -1;
}

void resetActuator(EnergyPlusState state, int handle)
{
    // resets the actuator so that E+ will use the internally calculated value again
    auto thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    if (handle >= 1 && handle <= thisState->dataRuntimeLang->numEMSActuatorsAvailable) {
        auto &theActuator(thisState->dataRuntimeLang->EMSActuatorAvailable(handle));
        *theActuator.Actuated = false;
    } else {
        if (thisState->dataGlobal->errorCallback) {
            std::cout << "ERROR: Actuator handle out of range in resetActuator, returning but caller should take note and likely abort." << std::endl;
        } else {
            // must be running from python plugin, need to fatal out once the plugin is done
            // throw an error, set the fatal flag, and then return
            EnergyPlus::ShowSevereError(*thisState, fmt::format("Data Exchange API: index error in resetActuator; received handle: {}", handle));
            EnergyPlus::ShowContinueError(*thisState,
                                          "The resetActuator function will return to allow the plugin to finish, then EnergyPlus will abort");
        }
        thisState->dataPluginManager->apiErrorFlag = true;
    }
}

void setActuatorValue(EnergyPlusState state, const int handle, const Real64 value)
{
    auto thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    if (handle >= 1 && handle <= thisState->dataRuntimeLang->numEMSActuatorsAvailable) {
        auto &theActuator(thisState->dataRuntimeLang->EMSActuatorAvailable(handle));
        if (theActuator.RealValue) {
            *theActuator.RealValue = value;
        } else if (theActuator.IntValue) {
            *theActuator.IntValue = (int)std::lround(value);
        } else {
            // follow protocol from EMS manager, where 1.0 is true, 0.0 is false, and anything else is also false
            *theActuator.LogValue = value > 0.99999 && value < 1.00001; // allow small tolerance while passing between languages and types
        }
        *theActuator.Actuated = true;
    } else {
        if (thisState->dataGlobal->errorCallback) {
            std::cout << "ERROR: Actuator handle out of range in setActuatorValue, returning but caller should take note and likely abort."
                      << std::endl;
        } else {
            // must be running from python plugin, need to fatal out once the plugin is done
            // throw an error, set the fatal flag, and then return
            EnergyPlus::ShowSevereError(*thisState, fmt::format("Data Exchange API: index error in setActuatorValue; received handle: {}", handle));
            EnergyPlus::ShowContinueError(*thisState,
                                          "The setActuatorValue function will return to allow the plugin to finish, then EnergyPlus will abort");
        }
        thisState->dataPluginManager->apiErrorFlag = true;
    }
}

Real64 getActuatorValue(EnergyPlusState state, const int handle)
{
    auto thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    if (handle >= 1 && handle <= thisState->dataRuntimeLang->numEMSActuatorsAvailable) {
        auto &theActuator(thisState->dataRuntimeLang->EMSActuatorAvailable(handle));
        if (theActuator.RealValue) {
            return *theActuator.RealValue;
        } else if (theActuator.IntValue) {
            return (float)*theActuator.IntValue;
        } else {
            // follow protocol from EMS manager, where 1.0 is true, 0.0 is false, and anything else is also false
            if (*theActuator.LogValue) {
                return 1;
            } else {
                return 0;
            }
        }
    } else {
        if (thisState->dataGlobal->errorCallback) {
            std::cout << "ERROR: Actuator handle out of range in getActuatorValue, returning zero but caller should take note and likely abort."
                      << std::endl;
        } else {
            // must be running from python plugin, need to fatal out once the plugin is done
            // throw an error, set the fatal flag, and then return 0
            EnergyPlus::ShowSevereError(*thisState, fmt::format("Data Exchange API: index error in getActuatorValue; received handle: {}", handle));
            EnergyPlus::ShowContinueError(
                *thisState, "The getActuatorValue function will return 0 for now to allow the plugin to finish, then EnergyPlus will abort");
        }
        thisState->dataPluginManager->apiErrorFlag = true;
        return 0;
    }
}

int getInternalVariableHandle(EnergyPlusState state, const char *type, const char *key)
{
    int handle = 0;
    std::string const typeUC = EnergyPlus::UtilityRoutines::MakeUPPERCase(type);
    std::string const keyUC = EnergyPlus::UtilityRoutines::MakeUPPERCase(key);
    auto thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    for (auto const &availVariable : thisState->dataRuntimeLang->EMSInternalVarsAvailable) { // TODO: this should stop at numEMSInternalVarsAvailable
        handle++;
        std::string const variableTypeUC = EnergyPlus::UtilityRoutines::MakeUPPERCase(availVariable.DataTypeName);
        std::string const variableIDUC = EnergyPlus::UtilityRoutines::MakeUPPERCase(availVariable.UniqueIDName);
        if (typeUC == variableTypeUC && keyUC == variableIDUC) {
            return handle;
        }
    }
    return -1;
}

Real64 getInternalVariableValue(EnergyPlusState state, int handle)
{
    auto thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    if (handle >= 1 && handle <= (int)thisState->dataRuntimeLang->numEMSInternalVarsAvailable) {
        auto thisVar = thisState->dataRuntimeLang->EMSInternalVarsAvailable(handle);
        if (thisVar.PntrVarTypeUsed == EnergyPlus::DataRuntimeLanguage::PtrDataType::Real) {
            return *thisVar.RealValue;
        } else if (thisVar.PntrVarTypeUsed == EnergyPlus::DataRuntimeLanguage::PtrDataType::Integer) {
            return (Real64)(*thisVar.IntValue);
        } else {
            // Doesn't look like this struct actually has a logical member type, so uh, throw here?
            std::cout << "ERROR: Invalid internal variable type here, developer issue., returning zero but caller should take note and likely abort."
                      << std::endl;
            thisState->dataPluginManager->apiErrorFlag = true;
            return 0;
        }
    } else {
        if (thisState->dataGlobal->errorCallback) {
            std::cout << "ERROR: Internal variable handle out of range in getInternalVariableValue, returning zero but caller should take note and "
                         "likely abort."
                      << std::endl;
        } else {
            // must be running from python plugin, need to fatal out once the plugin is done
            // throw an error, set the fatal flag, and then return 0
            EnergyPlus::ShowSevereError(*thisState,
                                        fmt::format("Data Exchange API: index error in getInternalVariableValue; received handle: {}", handle));
            EnergyPlus::ShowContinueError(
                *thisState, "The getInternalVariableValue function will return 0 for now to allow the plugin to finish, then EnergyPlus will abort");
        }
        thisState->dataPluginManager->apiErrorFlag = true;
        return 0;
    }
}

int getPluginGlobalVariableHandle(EnergyPlusState state, const char *name)
{
    auto thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    return thisState->dataPluginManager->pluginManager->getGlobalVariableHandle(*thisState, name);
}

Real64 getPluginGlobalVariableValue(EnergyPlusState state, int handle)
{
    auto thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    if (handle < 0 || handle > thisState->dataPluginManager->pluginManager->maxGlobalVariableIndex) {
        // need to fatal out once the plugin is done
        // throw an error, set the fatal flag, and then return 0
        EnergyPlus::ShowSevereError(
            *thisState, fmt::format("Data Exchange API: Problem -- index error in getPluginGlobalVariableValue; received handle: {}", handle));
        EnergyPlus::ShowContinueError(
            *thisState, "The getPluginGlobalVariableValue function will return 0 for now to allow the plugin to finish, then EnergyPlus will abort");
        thisState->dataPluginManager->apiErrorFlag = true;
        return 0;
    }
    return thisState->dataPluginManager->pluginManager->getGlobalVariableValue(*thisState, handle);
}

void setPluginGlobalVariableValue(EnergyPlusState state, int handle, Real64 value)
{
    auto thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    if (handle < 0 || handle > thisState->dataPluginManager->pluginManager->maxGlobalVariableIndex) {
        // need to fatal out once the plugin is done
        // throw an error, set the fatal flag, and then return
        EnergyPlus::ShowSevereError(
            *thisState, fmt::format("Data Exchange API: Problem -- index error in setPluginGlobalVariableValue; received handle: {}", handle));
        EnergyPlus::ShowContinueError(
            *thisState, "The getPluginGlobalVariableValue function will return to allow the plugin to finish, then EnergyPlus will abort");
        thisState->dataPluginManager->apiErrorFlag = true;
    }
    thisState->dataPluginManager->pluginManager->setGlobalVariableValue(*thisState, handle, value);
}

int getPluginTrendVariableHandle(EnergyPlusState state, const char *name)
{
    auto thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    return EnergyPlus::PluginManagement::PluginManager::getTrendVariableHandle(*thisState, name);
}

Real64 getPluginTrendVariableValue(EnergyPlusState state, int handle, int timeIndex)
{
    auto thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    if (handle < 0 || handle > thisState->dataPluginManager->pluginManager->maxTrendVariableIndex) {
        // need to fatal out once the plugin is done
        // throw an error, set the fatal flag, and then return
        EnergyPlus::ShowSevereError(
            *thisState, fmt::format("Data Exchange API: Problem -- index error in getPluginTrendVariableValue; received handle: {}", handle));
        EnergyPlus::ShowContinueError(
            *thisState, "The getPluginTrendVariableValue function will return 0 to allow the plugin to finish, then EnergyPlus will abort");
        thisState->dataPluginManager->apiErrorFlag = true;
        return 0;
    }
    if (timeIndex < 1 || timeIndex > ((int)EnergyPlus::PluginManagement::PluginManager::getTrendVariableHistorySize(*thisState, handle))) {
        // need to fatal out once the plugin is done
        // throw an error, set the fatal flag, and then return
        EnergyPlus::ShowSevereError(
            *thisState,
            fmt::format("Data Exchange API: Problem -- trend history count argument out of range in getPluginTrendVariableValue; received value: {}",
                        timeIndex));
        EnergyPlus::ShowContinueError(
            *thisState, "The getPluginTrendVariableValue function will return 0 to allow the plugin to finish, then EnergyPlus will abort");
        thisState->dataPluginManager->apiErrorFlag = true;
        return 0;
    }
    return EnergyPlus::PluginManagement::PluginManager::getTrendVariableValue(*thisState, handle, timeIndex);
}

Real64 getPluginTrendVariableAverage(EnergyPlusState state, int handle, int count)
{
    auto thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    if (handle < 0 || handle > thisState->dataPluginManager->pluginManager->maxTrendVariableIndex) {
        // need to fatal out once the plugin is done
        // throw an error, set the fatal flag, and then return
        EnergyPlus::ShowSevereError(
            *thisState, fmt::format("Data Exchange API: Problem -- index error in getPluginTrendVariableAverage; received handle: {}", handle));
        EnergyPlus::ShowContinueError(
            *thisState, "The getPluginTrendVariableAverage function will return 0 to allow the plugin to finish, then EnergyPlus will abort");
        thisState->dataPluginManager->apiErrorFlag = true;
        return 0;
    }
    if (count < 2 || count > ((int)EnergyPlus::PluginManagement::PluginManager::getTrendVariableHistorySize(*thisState, handle))) {
        // need to fatal out once the plugin is done
        // throw an error, set the fatal flag, and then return
        EnergyPlus::ShowSevereError(
            *thisState,
            fmt::format(
                "Data Exchange API: Problem -- trend history count argument out of range in getPluginTrendVariableAverage; received value: {}",
                count));
        EnergyPlus::ShowContinueError(
            *thisState, "The getPluginTrendVariableAverage function will return 0 to allow the plugin to finish, then EnergyPlus will abort");
        thisState->dataPluginManager->apiErrorFlag = true;
        return 0;
    }
    return EnergyPlus::PluginManagement::PluginManager::getTrendVariableAverage(*thisState, handle, count);
}

Real64 getPluginTrendVariableMin(EnergyPlusState state, int handle, int count)
{
    auto thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    if (handle < 0 || handle > thisState->dataPluginManager->pluginManager->maxTrendVariableIndex) {
        // need to fatal out once the plugin is done
        // throw an error, set the fatal flag, and then return
        EnergyPlus::ShowSevereError(
            *thisState, fmt::format("Data Exchange API: Problem -- index error in getPluginTrendVariableMin; received handle: {}", handle));
        EnergyPlus::ShowContinueError(
            *thisState, "The getPluginTrendVariableMin function will return 0 to allow the plugin to finish, then EnergyPlus will abort");
        thisState->dataPluginManager->apiErrorFlag = true;
        return 0;
    }
    if (count < 2 || count > ((int)EnergyPlus::PluginManagement::PluginManager::getTrendVariableHistorySize(*thisState, handle))) {
        // need to fatal out once the plugin is done
        // throw an error, set the fatal flag, and then return
        EnergyPlus::ShowSevereError(
            *thisState,
            fmt::format("Data Exchange API: Problem -- trend history count argument out of range in getPluginTrendVariableMin; received value: {}",
                        count));
        EnergyPlus::ShowContinueError(
            *thisState, "The getPluginTrendVariableMin function will return 0 to allow the plugin to finish, then EnergyPlus will abort");
        thisState->dataPluginManager->apiErrorFlag = true;
        return 0;
    }
    return EnergyPlus::PluginManagement::PluginManager::getTrendVariableMin(*thisState, handle, count);
}

Real64 getPluginTrendVariableMax(EnergyPlusState state, int handle, int count)
{
    auto thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    if (handle < 0 || handle > thisState->dataPluginManager->pluginManager->maxTrendVariableIndex) {
        // need to fatal out once the plugin is done
        // throw an error, set the fatal flag, and then return
        EnergyPlus::ShowSevereError(
            *thisState, fmt::format("Data Exchange API: Problem -- index error in getPluginTrendVariableMax; received handle: {}", handle));
        EnergyPlus::ShowContinueError(
            *thisState, "The getPluginTrendVariableMax function will return 0 to allow the plugin to finish, then EnergyPlus will abort");
        thisState->dataPluginManager->apiErrorFlag = true;
        return 0;
    }
    if (count < 2 || count > ((int)EnergyPlus::PluginManagement::PluginManager::getTrendVariableHistorySize(*thisState, handle))) {
        // need to fatal out once the plugin is done
        // throw an error, set the fatal flag, and then return
        EnergyPlus::ShowSevereError(
            *thisState,
            fmt::format("Data Exchange API: Problem -- trend history count argument out of range in getPluginTrendVariableMax; received value: {}",
                        count));
        EnergyPlus::ShowContinueError(
            *thisState, "The getPluginTrendVariableMax function will return 0 to allow the plugin to finish, then EnergyPlus will abort");
        thisState->dataPluginManager->apiErrorFlag = true;
        return 0;
    }
    return EnergyPlus::PluginManagement::PluginManager::getTrendVariableMax(*thisState, handle, count);
}

Real64 getPluginTrendVariableSum(EnergyPlusState state, int handle, int count)
{
    auto thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    if (handle < 0 || handle > thisState->dataPluginManager->pluginManager->maxTrendVariableIndex) {
        // need to fatal out once the plugin is done
        // throw an error, set the fatal flag, and then return
        EnergyPlus::ShowSevereError(
            *thisState, fmt::format("Data Exchange API: Problem -- index error in getPluginTrendVariableSum; received handle: {}", handle));
        EnergyPlus::ShowContinueError(
            *thisState, "The getPluginTrendVariableSum function will return 0 to allow the plugin to finish, then EnergyPlus will abort");
        thisState->dataPluginManager->apiErrorFlag = true;
        return 0;
    }
    if (count < 2 || count > ((int)EnergyPlus::PluginManagement::PluginManager::getTrendVariableHistorySize(*thisState, handle))) {
        // need to fatal out once the plugin is done
        // throw an error, set the fatal flag, and then return
        EnergyPlus::ShowSevereError(
            *thisState,
            fmt::format("Data Exchange API: Problem -- trend history count argument out of range in getPluginTrendVariableSum; received value: {}",
                        count));
        EnergyPlus::ShowContinueError(
            *thisState, "The getPluginTrendVariableSum function will return 0 to allow the plugin to finish, then EnergyPlus will abort");
        thisState->dataPluginManager->apiErrorFlag = true;
        return 0;
    }
    return EnergyPlus::PluginManagement::PluginManager::getTrendVariableSum(*thisState, handle, count);
}

Real64 getPluginTrendVariableDirection(EnergyPlusState state, int handle, int count)
{
    auto thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    if (handle < 0 || handle > thisState->dataPluginManager->pluginManager->maxTrendVariableIndex) {
        // need to fatal out once the plugin is done
        // throw an error, set the fatal flag, and then return
        EnergyPlus::ShowSevereError(
            *thisState, fmt::format("Data Exchange API: Problem -- index error in getPluginTrendVariableDirection; received handle: {}", handle));
        EnergyPlus::ShowContinueError(
            *thisState, "The getPluginTrendVariableDirection function will return 0 to allow the plugin to finish, then EnergyPlus will abort");
        thisState->dataPluginManager->apiErrorFlag = true;
        return 0;
    }
    if (count < 2 || count > ((int)EnergyPlus::PluginManagement::PluginManager::getTrendVariableHistorySize(*thisState, handle))) {
        // need to fatal out once the plugin is done
        // throw an error, set the fatal flag, and then return
        EnergyPlus::ShowSevereError(
            *thisState,
            fmt::format(
                "Data Exchange API: Problem -- trend history count argument out of range in getPluginTrendVariableDirection; received value: {}",
                count));
        EnergyPlus::ShowContinueError(
            *thisState, "The getPluginTrendVariableDirection function will return 0 to allow the plugin to finish, then EnergyPlus will abort");
        thisState->dataPluginManager->apiErrorFlag = true;
        return 0;
    }
    return EnergyPlus::PluginManagement::PluginManager::getTrendVariableDirection(*thisState, handle, count);
}

int year(EnergyPlusState state)
{
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    return thisState->dataEnvrn->Year;
}

int month(EnergyPlusState state)
{
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    return thisState->dataEnvrn->Month;
}

int dayOfMonth(EnergyPlusState state)
{
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    return thisState->dataEnvrn->DayOfMonth;
}

int dayOfWeek(EnergyPlusState state)
{
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    return thisState->dataEnvrn->DayOfWeek;
}

int dayOfYear(EnergyPlusState state)
{
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    return thisState->dataEnvrn->DayOfYear;
}

int daylightSavingsTimeIndicator(EnergyPlusState state)
{
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    return thisState->dataEnvrn->DSTIndicator;
}

int hour(EnergyPlusState state)
{
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    return thisState->dataGlobal->HourOfDay - 1; // no, just stay on 0..23+ DSTadjust ! offset by 1 and daylight savings time
}

Real64 currentTime(EnergyPlusState state)
{
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    if (thisState->dataHVACGlobal->TimeStepSys < thisState->dataGlobal->TimeStepZone) {
        // CurrentTime is for end of zone timestep, need to move back to beginning of current zone timestep, then add on system time elapsed already
        // plus current system timestep
        return thisState->dataGlobal->CurrentTime - thisState->dataGlobal->TimeStepZone + thisState->dataHVACGlobal->SysTimeElapsed +
               thisState->dataHVACGlobal->TimeStepSys;
    } else {
        return thisState->dataGlobal->CurrentTime;
    }
}

int minutes(EnergyPlusState state)
{
    // the -1 is to push us to the right minute, but this should be handled cautiously because if we are inside the HVAC iteration loop,
    // currentTime() returns a floating point fractional hour, so truncation could put this a few seconds from the expected minute.
    Real64 currentTimeVal = currentTime(state);
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    Real64 fractionalHoursIntoTheDay = currentTimeVal - double(thisState->dataGlobal->HourOfDay - 1);
    Real64 fractionalMinutesIntoTheDay = fractionalHoursIntoTheDay * 60.0;
    return (int)(fractionalMinutesIntoTheDay);
}

int numTimeStepsInHour([[maybe_unused]] EnergyPlusState state)
{
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    return thisState->dataGlobal->NumOfTimeStepInHour;
}

int zoneTimeStepNum([[maybe_unused]] EnergyPlusState state)
{
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    return thisState->dataGlobal->TimeStep;
}

int holidayIndex(EnergyPlusState state)
{
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    return thisState->dataEnvrn->HolidayIndex;
}

int sunIsUp(EnergyPlusState state)
{ // maintain response convention from previous (EMS) implementation
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    if (thisState->dataEnvrn->SunIsUp) {
        return 1;
    } else {
        return 0;
    }
}

int isRaining(EnergyPlusState state)
{
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    if (thisState->dataEnvrn->IsRain) {
        return 1;
    } else {
        return 0;
    }
}

int warmupFlag(EnergyPlusState state)
{
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    if (thisState->dataGlobal->WarmupFlag) {
        return 1;
    } else {
        return 0;
    }
}

Real64 zoneTimeStep(EnergyPlusState state)
{
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    return thisState->dataGlobal->TimeStepZone;
}

Real64 systemTimeStep(EnergyPlusState state)
{
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    return thisState->dataHVACGlobal->TimeStepSys;
}

int currentEnvironmentNum(EnergyPlusState state)
{
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    return thisState->dataEnvrn->CurEnvirNum;
}

int kindOfSim(EnergyPlusState state)
{
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    return static_cast<int>(thisState->dataGlobal->KindOfSim);
}

int getConstructionHandle(EnergyPlusState state, const char *constructionName)
{
    int handle = 0;
    std::string const nameUC = EnergyPlus::UtilityRoutines::MakeUPPERCase(constructionName);
    auto thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    for (auto const &construct : thisState->dataConstruction->Construct) {
        handle++;
        std::string const thisNameUC = EnergyPlus::UtilityRoutines::MakeUPPERCase(construct.Name);
        if (nameUC == thisNameUC) {
            return handle;
        }
    }
    return 0;
}

int actualTime(EnergyPlusState)
{
    std::string datestring;
    Array1D_int datevalues(8);
    ObjexxFCL::date_and_time(datestring, _, _, datevalues);
    return double(sum(datevalues({5, 8})));
}

int actualDateTime(EnergyPlusState)
{
    std::string datestring;
    Array1D_int datevalues(8);
    ObjexxFCL::date_and_time(datestring, _, _, datevalues);
    return double(sum(datevalues));
}

int todayWeatherIsRainAtTime(EnergyPlusState state, int hour, int timeStepNum)
{
    int value = 0;
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    int returnStatus =
        EnergyPlus::RuntimeLanguageProcessor::TodayTomorrowWeather(*thisState, hour, timeStepNum, thisState->dataWeatherManager->TodayIsRain, value);
    if (returnStatus != 0) {
        EnergyPlus::ShowSevereError(*thisState, "Invalid return from weather lookup, check hour and time step argument values are in range.");
        thisState->dataPluginManager->apiErrorFlag = true;
    }
    return value;
}
int todayWeatherIsSnowAtTime(EnergyPlusState state, int hour, int timeStepNum)
{
    int value = 0;
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    int returnStatus =
        EnergyPlus::RuntimeLanguageProcessor::TodayTomorrowWeather(*thisState, hour, timeStepNum, thisState->dataWeatherManager->TodayIsSnow, value);
    if (returnStatus != 0) {
        EnergyPlus::ShowSevereError(*thisState, "Invalid return from weather lookup, check hour and time step argument values are in range.");
        thisState->dataPluginManager->apiErrorFlag = true;
    }
    return value;
}
Real64 todayWeatherOutDryBulbAtTime(EnergyPlusState state, int hour, int timeStepNum)
{
    Real64 value = 0;
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    int returnStatus = EnergyPlus::RuntimeLanguageProcessor::TodayTomorrowWeather(
        *thisState, hour, timeStepNum, thisState->dataWeatherManager->TodayOutDryBulbTemp, value);
    if (returnStatus != 0) {
        EnergyPlus::ShowSevereError(*thisState, "Invalid return from weather lookup, check hour and time step argument values are in range.");
        thisState->dataPluginManager->apiErrorFlag = true;
    }
    return value;
}
Real64 todayWeatherOutDewPointAtTime(EnergyPlusState state, int hour, int timeStepNum)
{
    Real64 value = 0;
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    int returnStatus = EnergyPlus::RuntimeLanguageProcessor::TodayTomorrowWeather(
        *thisState, hour, timeStepNum, thisState->dataWeatherManager->TodayOutDewPointTemp, value);
    if (returnStatus != 0) {
        EnergyPlus::ShowSevereError(*thisState, "Invalid return from weather lookup, check hour and time step argument values are in range.");
        thisState->dataPluginManager->apiErrorFlag = true;
    }
    return value;
}
Real64 todayWeatherOutBarometricPressureAtTime(EnergyPlusState state, int hour, int timeStepNum)
{
    Real64 value = 0;
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    int returnStatus = EnergyPlus::RuntimeLanguageProcessor::TodayTomorrowWeather(
        *thisState, hour, timeStepNum, thisState->dataWeatherManager->TodayOutBaroPress, value);
    if (returnStatus != 0) {
        EnergyPlus::ShowSevereError(*thisState, "Invalid return from weather lookup, check hour and time step argument values are in range.");
        thisState->dataPluginManager->apiErrorFlag = true;
    }
    return value;
}
Real64 todayWeatherOutRelativeHumidityAtTime(EnergyPlusState state, int hour, int timeStepNum)
{
    Real64 value = 0;
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    int returnStatus = EnergyPlus::RuntimeLanguageProcessor::TodayTomorrowWeather(
        *thisState, hour, timeStepNum, thisState->dataWeatherManager->TodayOutRelHum, value);
    if (returnStatus != 0) {
        EnergyPlus::ShowSevereError(*thisState, "Invalid return from weather lookup, check hour and time step argument values are in range.");
        thisState->dataPluginManager->apiErrorFlag = true;
    }
    return value;
}
Real64 todayWeatherWindSpeedAtTime(EnergyPlusState state, int hour, int timeStepNum)
{
    Real64 value = 0;
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    int returnStatus = EnergyPlus::RuntimeLanguageProcessor::TodayTomorrowWeather(
        *thisState, hour, timeStepNum, thisState->dataWeatherManager->TodayWindSpeed, value);
    if (returnStatus != 0) {
        EnergyPlus::ShowSevereError(*thisState, "Invalid return from weather lookup, check hour and time step argument values are in range.");
        thisState->dataPluginManager->apiErrorFlag = true;
    }
    return value;
}
Real64 todayWeatherWindDirectionAtTime(EnergyPlusState state, int hour, int timeStepNum)
{
    Real64 value = 0;
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    int returnStatus =
        EnergyPlus::RuntimeLanguageProcessor::TodayTomorrowWeather(*thisState, hour, timeStepNum, thisState->dataWeatherManager->TodayWindDir, value);
    if (returnStatus != 0) {
        EnergyPlus::ShowSevereError(*thisState, "Invalid return from weather lookup, check hour and time step argument values are in range.");
        thisState->dataPluginManager->apiErrorFlag = true;
    }
    return value;
}
Real64 todayWeatherSkyTemperatureAtTime(EnergyPlusState state, int hour, int timeStepNum)
{
    Real64 value = 0;
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    int returnStatus =
        EnergyPlus::RuntimeLanguageProcessor::TodayTomorrowWeather(*thisState, hour, timeStepNum, thisState->dataWeatherManager->TodaySkyTemp, value);
    if (returnStatus != 0) {
        EnergyPlus::ShowSevereError(*thisState, "Invalid return from weather lookup, check hour and time step argument values are in range.");
        thisState->dataPluginManager->apiErrorFlag = true;
    }
    return value;
}
Real64 todayWeatherHorizontalIRSkyAtTime(EnergyPlusState state, int hour, int timeStepNum)
{
    Real64 value = 0;
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    int returnStatus = EnergyPlus::RuntimeLanguageProcessor::TodayTomorrowWeather(
        *thisState, hour, timeStepNum, thisState->dataWeatherManager->TodayHorizIRSky, value);
    if (returnStatus != 0) {
        EnergyPlus::ShowSevereError(*thisState, "Invalid return from weather lookup, check hour and time step argument values are in range.");
        thisState->dataPluginManager->apiErrorFlag = true;
    }
    return value;
}
Real64 todayWeatherBeamSolarRadiationAtTime(EnergyPlusState state, int hour, int timeStepNum)
{
    Real64 value = 0;
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    int returnStatus = EnergyPlus::RuntimeLanguageProcessor::TodayTomorrowWeather(
        *thisState, hour, timeStepNum, thisState->dataWeatherManager->TodayBeamSolarRad, value);
    if (returnStatus != 0) {
        EnergyPlus::ShowSevereError(*thisState, "Invalid return from weather lookup, check hour and time step argument values are in range.");
        thisState->dataPluginManager->apiErrorFlag = true;
    }
    return value;
}
Real64 todayWeatherDiffuseSolarRadiationAtTime(EnergyPlusState state, int hour, int timeStepNum)
{
    Real64 value = 0;
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    int returnStatus = EnergyPlus::RuntimeLanguageProcessor::TodayTomorrowWeather(
        *thisState, hour, timeStepNum, thisState->dataWeatherManager->TodayDifSolarRad, value);
    if (returnStatus != 0) {
        EnergyPlus::ShowSevereError(*thisState, "Invalid return from weather lookup, check hour and time step argument values are in range.");
        thisState->dataPluginManager->apiErrorFlag = true;
    }
    return value;
}
Real64 todayWeatherAlbedoAtTime(EnergyPlusState state, int hour, int timeStepNum)
{
    Real64 value = 0;
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    int returnStatus =
        EnergyPlus::RuntimeLanguageProcessor::TodayTomorrowWeather(*thisState, hour, timeStepNum, thisState->dataWeatherManager->TodayAlbedo, value);
    if (returnStatus != 0) {
        EnergyPlus::ShowSevereError(*thisState, "Invalid return from weather lookup, check hour and time step argument values are in range.");
        thisState->dataPluginManager->apiErrorFlag = true;
    }
    return value;
}
Real64 todayWeatherLiquidPrecipitationAtTime(EnergyPlusState state, int hour, int timeStepNum)
{
    Real64 value = 0;
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    int returnStatus = EnergyPlus::RuntimeLanguageProcessor::TodayTomorrowWeather(
        *thisState, hour, timeStepNum, thisState->dataWeatherManager->TodayLiquidPrecip, value);
    if (returnStatus != 0) {
        EnergyPlus::ShowSevereError(*thisState, "Invalid return from weather lookup, check hour and time step argument values are in range.");
        thisState->dataPluginManager->apiErrorFlag = true;
    }
    return value;
}

int tomorrowWeatherIsRainAtTime(EnergyPlusState state, int hour, int timeStepNum)
{
    int value = 0;
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    int returnStatus = EnergyPlus::RuntimeLanguageProcessor::TodayTomorrowWeather(
        *thisState, hour, timeStepNum, thisState->dataWeatherManager->TomorrowIsRain, value);
    if (returnStatus != 0) {
        EnergyPlus::ShowSevereError(*thisState, "Invalid return from weather lookup, check hour and time step argument values are in range.");
        thisState->dataPluginManager->apiErrorFlag = true;
    }
    return value;
}
int tomorrowWeatherIsSnowAtTime(EnergyPlusState state, int hour, int timeStepNum)
{
    int value = 0;
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    int returnStatus = EnergyPlus::RuntimeLanguageProcessor::TodayTomorrowWeather(
        *thisState, hour, timeStepNum, thisState->dataWeatherManager->TomorrowIsSnow, value);
    if (returnStatus != 0) {
        EnergyPlus::ShowSevereError(*thisState, "Invalid return from weather lookup, check hour and time step argument values are in range.");
        thisState->dataPluginManager->apiErrorFlag = true;
    }
    return value;
}
Real64 tomorrowWeatherOutDryBulbAtTime(EnergyPlusState state, int hour, int timeStepNum)
{
    Real64 value = 0;
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    int returnStatus = EnergyPlus::RuntimeLanguageProcessor::TodayTomorrowWeather(
        *thisState, hour, timeStepNum, thisState->dataWeatherManager->TomorrowOutDryBulbTemp, value);
    if (returnStatus != 0) {
        EnergyPlus::ShowSevereError(*thisState, "Invalid return from weather lookup, check hour and time step argument values are in range.");
        thisState->dataPluginManager->apiErrorFlag = true;
    }
    return value;
}
Real64 tomorrowWeatherOutDewPointAtTime(EnergyPlusState state, int hour, int timeStepNum)
{
    Real64 value = 0;
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    int returnStatus = EnergyPlus::RuntimeLanguageProcessor::TodayTomorrowWeather(
        *thisState, hour, timeStepNum, thisState->dataWeatherManager->TomorrowOutDewPointTemp, value);
    if (returnStatus != 0) {
        EnergyPlus::ShowSevereError(*thisState, "Invalid return from weather lookup, check hour and time step argument values are in range.");
        thisState->dataPluginManager->apiErrorFlag = true;
    }
    return value;
}
Real64 tomorrowWeatherOutBarometricPressureAtTime(EnergyPlusState state, int hour, int timeStepNum)
{
    Real64 value = 0;
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    int returnStatus = EnergyPlus::RuntimeLanguageProcessor::TodayTomorrowWeather(
        *thisState, hour, timeStepNum, thisState->dataWeatherManager->TomorrowOutBaroPress, value);
    if (returnStatus != 0) {
        EnergyPlus::ShowSevereError(*thisState, "Invalid return from weather lookup, check hour and time step argument values are in range.");
        thisState->dataPluginManager->apiErrorFlag = true;
    }
    return value;
}
Real64 tomorrowWeatherOutRelativeHumidityAtTime(EnergyPlusState state, int hour, int timeStepNum)
{
    Real64 value = 0;
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    int returnStatus = EnergyPlus::RuntimeLanguageProcessor::TodayTomorrowWeather(
        *thisState, hour, timeStepNum, thisState->dataWeatherManager->TomorrowOutRelHum, value);
    if (returnStatus != 0) {
        EnergyPlus::ShowSevereError(*thisState, "Invalid return from weather lookup, check hour and time step argument values are in range.");
        thisState->dataPluginManager->apiErrorFlag = true;
    }
    return value;
}
Real64 tomorrowWeatherWindSpeedAtTime(EnergyPlusState state, int hour, int timeStepNum)
{
    Real64 value = 0;
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    int returnStatus = EnergyPlus::RuntimeLanguageProcessor::TodayTomorrowWeather(
        *thisState, hour, timeStepNum, thisState->dataWeatherManager->TomorrowWindSpeed, value);
    if (returnStatus != 0) {
        EnergyPlus::ShowSevereError(*thisState, "Invalid return from weather lookup, check hour and time step argument values are in range.");
        thisState->dataPluginManager->apiErrorFlag = true;
    }
    return value;
}
Real64 tomorrowWeatherWindDirectionAtTime(EnergyPlusState state, int hour, int timeStepNum)
{
    Real64 value = 0;
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    int returnStatus = EnergyPlus::RuntimeLanguageProcessor::TodayTomorrowWeather(
        *thisState, hour, timeStepNum, thisState->dataWeatherManager->TomorrowWindDir, value);
    if (returnStatus != 0) {
        EnergyPlus::ShowSevereError(*thisState, "Invalid return from weather lookup, check hour and time step argument values are in range.");
        thisState->dataPluginManager->apiErrorFlag = true;
    }
    return value;
}
Real64 tomorrowWeatherSkyTemperatureAtTime(EnergyPlusState state, int hour, int timeStepNum)
{
    Real64 value = 0;
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    int returnStatus = EnergyPlus::RuntimeLanguageProcessor::TodayTomorrowWeather(
        *thisState, hour, timeStepNum, thisState->dataWeatherManager->TomorrowSkyTemp, value);
    if (returnStatus != 0) {
        EnergyPlus::ShowSevereError(*thisState, "Invalid return from weather lookup, check hour and time step argument values are in range.");
        thisState->dataPluginManager->apiErrorFlag = true;
    }
    return value;
}
Real64 tomorrowWeatherHorizontalIRSkyAtTime(EnergyPlusState state, int hour, int timeStepNum)
{
    Real64 value = 0;
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    int returnStatus = EnergyPlus::RuntimeLanguageProcessor::TodayTomorrowWeather(
        *thisState, hour, timeStepNum, thisState->dataWeatherManager->TomorrowHorizIRSky, value);
    if (returnStatus != 0) {
        EnergyPlus::ShowSevereError(*thisState, "Invalid return from weather lookup, check hour and time step argument values are in range.");
        thisState->dataPluginManager->apiErrorFlag = true;
    }
    return value;
}
Real64 tomorrowWeatherBeamSolarRadiationAtTime(EnergyPlusState state, int hour, int timeStepNum)
{
    Real64 value = 0;
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    int returnStatus = EnergyPlus::RuntimeLanguageProcessor::TodayTomorrowWeather(
        *thisState, hour, timeStepNum, thisState->dataWeatherManager->TomorrowBeamSolarRad, value);
    if (returnStatus != 0) {
        EnergyPlus::ShowSevereError(*thisState, "Invalid return from weather lookup, check hour and time step argument values are in range.");
        thisState->dataPluginManager->apiErrorFlag = true;
    }
    return value;
}
Real64 tomorrowWeatherDiffuseSolarRadiationAtTime(EnergyPlusState state, int hour, int timeStepNum)
{
    Real64 value = 0;
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    int returnStatus = EnergyPlus::RuntimeLanguageProcessor::TodayTomorrowWeather(
        *thisState, hour, timeStepNum, thisState->dataWeatherManager->TomorrowDifSolarRad, value);
    if (returnStatus != 0) {
        EnergyPlus::ShowSevereError(*thisState, "Invalid return from weather lookup, check hour and time step argument values are in range.");
        thisState->dataPluginManager->apiErrorFlag = true;
    }
    return value;
}
Real64 tomorrowWeatherAlbedoAtTime(EnergyPlusState state, int hour, int timeStepNum)
{
    Real64 value = 0;
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    int returnStatus = EnergyPlus::RuntimeLanguageProcessor::TodayTomorrowWeather(
        *thisState, hour, timeStepNum, thisState->dataWeatherManager->TomorrowAlbedo, value);
    if (returnStatus != 0) {
        EnergyPlus::ShowSevereError(*thisState, "Invalid return from weather lookup, check hour and time step argument values are in range.");
        thisState->dataPluginManager->apiErrorFlag = true;
    }
    return value;
}
Real64 tomorrowWeatherLiquidPrecipitationAtTime(EnergyPlusState state, int hour, int timeStepNum)
{
    Real64 value = 0;
    auto *thisState = reinterpret_cast<EnergyPlus::EnergyPlusData *>(state);
    int returnStatus = EnergyPlus::RuntimeLanguageProcessor::TodayTomorrowWeather(
        *thisState, hour, timeStepNum, thisState->dataWeatherManager->TomorrowLiquidPrecip, value);
    if (returnStatus != 0) {
        EnergyPlus::ShowSevereError(*thisState, "Invalid return from weather lookup, check hour and time step argument values are in range.");
        thisState->dataPluginManager->apiErrorFlag = true;
    }
    return value;
}
