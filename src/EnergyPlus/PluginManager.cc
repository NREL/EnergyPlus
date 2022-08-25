// EnergyPlus, Copyright (c) 1996-2022, The Board of Trustees of the University of Illinois,
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

#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataStringGlobals.hh>
#include <EnergyPlus/FileSystem.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/PluginManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

#include <algorithm>
#include <nlohmann/json.hpp>

namespace EnergyPlus::PluginManagement {

PluginTrendVariable::PluginTrendVariable(EnergyPlusData &state, std::string _name, int _numValues, int _indexOfPluginVariable)
    : name(std::move(_name)), numValues(_numValues), indexOfPluginVariable(_indexOfPluginVariable)
{
    // initialize the deque so it can be queried immediately, even with just zeroes
    for (int i = 1; i <= this->numValues; i++) {
        this->values.push_back(0);
    }
    for (int loop = 1; loop <= _numValues; ++loop) {
        this->times.push_back(-loop * state.dataGlobal->TimeStepZone);
    }
}

void registerNewCallback(EnergyPlusData &state, EMSManager::EMSCallFrom iCalledFrom, const std::function<void(void *)> &f)
{
    state.dataPluginManager->callbacks[iCalledFrom].push_back(f);
}

void onBeginEnvironment(EnergyPlusData &state)
{
    // reset vars and trends -- sensors and actuators are reset by EMS
    for (auto &v : state.dataPluginManager->globalVariableValues) {
        v = 0;
    }
    // reinitialize trend variables so old data are purged
    for (auto &tr : state.dataPluginManager->trends) {
        tr.reset();
    }
}

int PluginManager::numActiveCallbacks(EnergyPlusData &state)
{
    return (int)state.dataPluginManager->callbacks.size();
}

void runAnyRegisteredCallbacks(EnergyPlusData &state, EMSManager::EMSCallFrom iCalledFrom, bool &anyRan)
{
    if (state.dataGlobal->KickOffSimulation) return;
    for (auto const &cb : state.dataPluginManager->callbacks[iCalledFrom]) {
        cb((void *)&state);
        anyRan = true;
    }
#if LINK_WITH_PYTHON
    for (auto &plugin : state.dataPluginManager->plugins) {
        if (plugin.runDuringWarmup || !state.dataGlobal->WarmupFlag) {
            bool const didOneRun = plugin.run(state, iCalledFrom);
            if (didOneRun) anyRan = true;
        }
    }
#endif
}

#if LINK_WITH_PYTHON
std::string pythonStringForUsage(EnergyPlusData &state)
{
    if (state.dataGlobal->errorCallback) {
        return "Python Version not accessible during API calls";
    }
    std::string sVersion = Py_GetVersion();
    // 3.8.3 (default, Jun  2 2020, 15:25:16) \n[GCC 7.5.0]
    // Remove the '\n'
    sVersion.erase(std::remove(sVersion.begin(), sVersion.end(), '\n'), sVersion.end());
    return "Linked to Python Version: \"" + sVersion + "\"";
}
#else
std::string pythonStringForUsage([[maybe_unused]] EnergyPlusData &state)
{
    return "This version of EnergyPlus not linked to Python library.";
}
#endif

void PluginManager::setupOutputVariables([[maybe_unused]] EnergyPlusData &state)
{
#if LINK_WITH_PYTHON
    // with the PythonPlugin:Variables all set in memory, we can now set them up as outputs as needed
    std::string const sOutputVariable = "PythonPlugin:OutputVariable";
    int outputVarInstances = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, sOutputVariable);
    if (outputVarInstances > 0) {
        auto const instances = state.dataInputProcessing->inputProcessor->epJSON.find(sOutputVariable);
        if (instances == state.dataInputProcessing->inputProcessor->epJSON.end()) {
            ShowSevereError(state, sOutputVariable + ": Somehow getNumObjectsFound was > 0 but epJSON.find found 0"); // LCOV_EXCL_LINE
        }
        auto &instancesValue = instances.value();
        for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
            auto const &fields = instance.value();
            auto const &thisObjectName = instance.key();
            auto const objNameUC = EnergyPlus::UtilityRoutines::MakeUPPERCase(thisObjectName);
            // no need to validate name, the JSON will validate that.
            state.dataInputProcessing->inputProcessor->markObjectAsUsed(sOutputVariable, thisObjectName);
            std::string varName = fields.at("python_plugin_variable_name").get<std::string>();
            std::string avgOrSum = EnergyPlus::UtilityRoutines::MakeUPPERCase(fields.at("type_of_data_in_variable").get<std::string>());
            std::string updateFreq = EnergyPlus::UtilityRoutines::MakeUPPERCase(fields.at("update_frequency").get<std::string>());
            std::string units;
            if (fields.find("units") != fields.end()) {
                units = fields.at("units").get<std::string>();
            }
            // get the index of the global variable, fatal if it doesn't mach one
            // validate type of data, update frequency, and look up units enum value
            // call setup output variable - variable TYPE is "PythonPlugin:OutputVariable"
            int variableHandle = EnergyPlus::PluginManagement::PluginManager::getGlobalVariableHandle(state, varName);
            if (variableHandle == -1) {
                EnergyPlus::ShowSevereError(state, "Failed to match Python Plugin Output Variable");
                EnergyPlus::ShowContinueError(state, "Trying to create output instance for variable name \"" + varName + "\"");
                EnergyPlus::ShowContinueError(state, "No match found, make sure variable is listed in PythonPlugin:Variables object");
                EnergyPlus::ShowFatalError(state, "Python Plugin Output Variable problem causes program termination");
            }
            bool isMetered = false;
            OutputProcessor::SOVStoreType sAvgOrSum = OutputProcessor::SOVStoreType::Average;
            if (avgOrSum == "SUMMED") {
                sAvgOrSum = OutputProcessor::SOVStoreType::Summed;
            } else if (avgOrSum == "METERED") {
                sAvgOrSum = OutputProcessor::SOVStoreType::Summed;
                isMetered = true;
            }
            OutputProcessor::SOVTimeStepType sUpdateFreq = OutputProcessor::SOVTimeStepType::Zone;
            if (updateFreq == "SYSTEMTIMESTEP") {
                sUpdateFreq = OutputProcessor::SOVTimeStepType::System;
            }
            OutputProcessor::Unit thisUnit = OutputProcessor::Unit::None;
            if (!units.empty()) {
                thisUnit = OutputProcessor::unitStringToEnum(units);
                if (thisUnit == OutputProcessor::Unit::unknown) {
                    thisUnit = OutputProcessor::Unit::customEMS;
                }
            }
            if (!isMetered) {
                // regular output variable, ignore the meter/resource stuff and register the variable
                if (thisUnit != OutputProcessor::Unit::customEMS) {
                    SetupOutputVariable(state,
                                        sOutputVariable,
                                        thisUnit,
                                        state.dataPluginManager->globalVariableValues[variableHandle],
                                        sUpdateFreq,
                                        sAvgOrSum,
                                        thisObjectName);
                } else {
                    SetupOutputVariable(state,
                                        sOutputVariable,
                                        thisUnit,
                                        state.dataPluginManager->globalVariableValues[variableHandle],
                                        sUpdateFreq,
                                        sAvgOrSum,
                                        thisObjectName,
                                        _,
                                        _,
                                        _,
                                        _,
                                        _,
                                        _,
                                        _,
                                        _,
                                        _,
                                        units);
                }
            } else {
                // We are doing a metered type, we need to get the extra stuff
                // Resource Type
                if (fields.find("resource_type") == fields.end()) {
                    EnergyPlus::ShowSevereError(state, "Input error on PythonPlugin:OutputVariable = " + thisObjectName);
                    EnergyPlus::ShowContinueError(state, "The variable was marked as metered, but did not define a resource type");
                    EnergyPlus::ShowContinueError(state,
                                                  "For metered variables, the resource type, group type, and end use category must be defined");
                    EnergyPlus::ShowFatalError(state, "Input error on PythonPlugin:OutputVariable causes program termination");
                }
                std::string const resourceType = EnergyPlus::UtilityRoutines::MakeUPPERCase(fields.at("resource_type").get<std::string>());
                std::string sResourceType;
                if (resourceType == "ELECTRICITY") {
                    sResourceType = "Electricity";
                } else if (resourceType == "NATURALGAS") {
                    sResourceType = "NaturalGas";
                } else if (resourceType == "GASOLINE") {
                    sResourceType = "Gasoline";
                } else if (resourceType == "DIESEL") {
                    sResourceType = "Diesel";
                } else if (resourceType == "COAL") {
                    sResourceType = "Coal";
                } else if (resourceType == "FUELOILNO1") {
                    sResourceType = "FuelOilNo1";
                } else if (resourceType == "FUELOILNO2") {
                    sResourceType = "FuelOilNo2";
                } else if (resourceType == "OTHERFUEL1") {
                    sResourceType = "OtherFuel1";
                } else if (resourceType == "OTHERFUEL2") {
                    sResourceType = "OtherFuel2";
                } else if (resourceType == "PROPANE") {
                    sResourceType = "Propane";
                } else if (resourceType == "WATERUSE") {
                    sResourceType = "Water";
                } else if (resourceType == "ONSITEWATERPRODUCED") {
                    sResourceType = "OnSiteWater";
                } else if (resourceType == "MAINSWATERSUPPLY") {
                    sResourceType = "MainsWater";
                } else if (resourceType == "RAINWATERCOLLECTED") {
                    sResourceType = "RainWater";
                } else if (resourceType == "WELLWATERDRAWN") {
                    sResourceType = "WellWater";
                } else if (resourceType == "CONDENSATEWATERCOLLECTED") {
                    sResourceType = "Condensate";
                } else if (resourceType == "ENERGYTRANSFER") {
                    sResourceType = "EnergyTransfer";
                } else if (resourceType == "STEAM") {
                    sResourceType = "Steam";
                } else if (resourceType == "DISTRICTCOOLING") {
                    sResourceType = "DistrictCooling";
                } else if (resourceType == "DISTRICTHEATING") {
                    sResourceType = "DistrictHeating";
                } else if (resourceType == "ELECTRICITYPRODUCEDONSITE") {
                    sResourceType = "ElectricityProduced";
                } else if (resourceType == "SOLARWATERHEATING") {
                    sResourceType = "SolarWater";
                } else if (resourceType == "SOLARAIRHEATING") {
                    sResourceType = "SolarAir";
                } else {
                    ShowSevereError(state, "Invalid input for PythonPlugin:OutputVariable, unexpected Resource Type = " + resourceType);
                    ShowFatalError(state, "Python plugin output variable input problem causes program termination");
                }

                // Group Type
                if (fields.find("group_type") == fields.end()) {
                    EnergyPlus::ShowSevereError(state, "Input error on PythonPlugin:OutputVariable = " + thisObjectName);
                    EnergyPlus::ShowContinueError(state, "The variable was marked as metered, but did not define a group type");
                    EnergyPlus::ShowContinueError(state,
                                                  "For metered variables, the resource type, group type, and end use category must be defined");
                    EnergyPlus::ShowFatalError(state, "Input error on PythonPlugin:OutputVariable causes program termination");
                }
                std::string const groupType = EnergyPlus::UtilityRoutines::MakeUPPERCase(fields.at("group_type").get<std::string>());
                std::string sGroupType;
                if (groupType == "BUILDING") {
                    sGroupType = "Building";
                } else if (groupType == "HVAC") {
                    sGroupType = "HVAC";
                } else if (groupType == "PLANT") {
                    sGroupType = "Plant";
                } else if (groupType == "SYSTEM") {
                    sGroupType = "System";
                } else {
                    ShowSevereError(state, "Invalid input for PythonPlugin:OutputVariable, unexpected Group Type = " + groupType);
                    ShowFatalError(state, "Python plugin output variable input problem causes program termination");
                }

                // End Use Type
                if (fields.find("end_use_category") == fields.end()) {
                    EnergyPlus::ShowSevereError(state, "Input error on PythonPlugin:OutputVariable = " + thisObjectName);
                    EnergyPlus::ShowContinueError(state, "The variable was marked as metered, but did not define an end-use category");
                    EnergyPlus::ShowContinueError(state,
                                                  "For metered variables, the resource type, group type, and end use category must be defined");
                    EnergyPlus::ShowFatalError(state, "Input error on PythonPlugin:OutputVariable causes program termination");
                }
                std::string const endUse = EnergyPlus::UtilityRoutines::MakeUPPERCase(fields.at("end_use_category").get<std::string>());
                std::string sEndUse;
                if (endUse == "HEATING") {
                    sEndUse = "Heating";
                } else if (endUse == "COOLING") {
                    sEndUse = "Cooling";
                } else if (endUse == "INTERIORLIGHTS") {
                    sEndUse = "InteriorLights";
                } else if (endUse == "EXTERIORLIGHTS") {
                    sEndUse = "ExteriorLights";
                } else if (endUse == "INTERIOREQUIPMENT") {
                    sEndUse = "InteriorEquipment";
                } else if (endUse == "EXTERIOREQUIPMENT") {
                    sEndUse = "ExteriorEquipment";
                } else if (endUse == "FANS") {
                    sEndUse = "Fans";
                } else if (endUse == "PUMPS") {
                    sEndUse = "Pumps";
                } else if (endUse == "HEATREJECTION") {
                    sEndUse = "HeatRejection";
                } else if (endUse == "HUMIDIFIER") {
                    sEndUse = "Humidifier";
                } else if (endUse == "HEATRECOVERY") {
                    sEndUse = "HeatRecovery";
                } else if (endUse == "WATERSYSTEMS") {
                    sEndUse = "WaterSystems";
                } else if (endUse == "REFRIGERATION") {
                    sEndUse = "Refrigeration";
                } else if (endUse == "ONSITEGENERATION") {
                    sEndUse = "Cogeneration";
                } else if (endUse == "HEATINGCOILS") {
                    sEndUse = "HeatingCoils";
                } else if (endUse == "COOLINGCOILS") {
                    sEndUse = "CoolingCoils";
                } else if (endUse == "CHILLERS") {
                    sEndUse = "Chillers";
                } else if (endUse == "BOILERS") {
                    sEndUse = "Boilers";
                } else if (endUse == "BASEBOARD") {
                    sEndUse = "Baseboard";
                } else if (endUse == "HEATRECOVERYFORCOOLING") {
                    sEndUse = "HeatRecoveryForCooling";
                } else if (endUse == "HEATRECOVERYFORHEATING") {
                    sEndUse = "HeatRecoveryForHeating";
                } else {
                    ShowSevereError(state, "Invalid input for PythonPlugin:OutputVariable, unexpected End-use Subcategory = " + groupType);
                    ShowFatalError(state, "Python plugin output variable input problem causes program termination");
                }

                // Additional End Use Types Only Used for EnergyTransfer
                if ((sResourceType != "EnergyTransfer") &&
                    (sEndUse == "HeatingCoils" || sEndUse == "CoolingCoils" || sEndUse == "Chillers" || sEndUse == "Boilers" ||
                     sEndUse == "Baseboard" || sEndUse == "HeatRecoveryForCooling" || sEndUse == "HeatRecoveryForHeating")) {
                    ShowWarningError(state, "Inconsistent resource type input for PythonPlugin:OutputVariable = " + thisObjectName);
                    ShowContinueError(state, "For end use subcategory = " + sEndUse + ", resource type must be EnergyTransfer");
                    ShowContinueError(state, "Resource type is being reset to EnergyTransfer and the simulation continues...");
                    sResourceType = "EnergyTransfer";
                }

                std::string sEndUseSubcategory;
                if (fields.find("end_use_subcategory") != fields.end()) {
                    sEndUseSubcategory = fields.at("end_use_subcategory").get<std::string>();
                }

                if (sEndUseSubcategory.empty()) { // no subcategory
                    SetupOutputVariable(state,
                                        sOutputVariable,
                                        thisUnit,
                                        state.dataPluginManager->globalVariableValues[variableHandle],
                                        sUpdateFreq,
                                        sAvgOrSum,
                                        thisObjectName,
                                        _,
                                        sResourceType,
                                        sEndUse,
                                        _,
                                        sGroupType);
                } else { // has subcategory
                    SetupOutputVariable(state,
                                        sOutputVariable,
                                        thisUnit,
                                        state.dataPluginManager->globalVariableValues[variableHandle],
                                        sUpdateFreq,
                                        sAvgOrSum,
                                        thisObjectName,
                                        _,
                                        sResourceType,
                                        sEndUse,
                                        sEndUseSubcategory,
                                        sGroupType);
                }
            }
        }
    }
#endif
}

PluginManager::PluginManager(EnergyPlusData &state) : eplusRunningViaPythonAPI(state.dataPluginManager->eplusRunningViaPythonAPI)
{
#if LINK_WITH_PYTHON
    // this frozen flag tells Python that the package and library have been frozen for embedding, so it shouldn't warn about missing prefixes
    Py_FrozenFlag = 1;

    // we'll need the program directory for a few things so get it once here at the top and sanitize it
    fs::path programDir;
    if (state.dataGlobal->installRootOverride) {
        programDir = state.dataStrGlobals->exeDirectoryPath;
    } else {
        programDir = FileSystem::getParentDirectoryPath(FileSystem::getAbsolutePath(FileSystem::getProgramPath()));
    }
    fs::path sanitizedProgramDir = PluginManager::sanitizedPath(programDir);

    // I think we need to set the python path before initializing the library
    // make this relative to the binary
    fs::path pathToPythonPackages = FileSystem::makeNativePath(sanitizedProgramDir / "python_standard_lib");
    wchar_t *a = Py_DecodeLocale(pathToPythonPackages.string().c_str(), nullptr);
    Py_SetPath(a);
    Py_SetPythonHome(a);

    // must be called before Py_Initialize
    // tells the interpreter the value of argv[0] to the main() function
    // used by some functions to find run-time libraries relative to the interpreter executable
    Py_SetProgramName((wchar_t *)programName);

    // now that we have set the path, we can initialize python
    // from https://docs.python.org/3/c-api/init.html
    // If arg 0, it skips init registration of signal handlers, which might be useful when Python is embedded.
    bool alreadyInitialized = (Py_IsInitialized() != 0);
    if (!alreadyInitialized) {
        Py_InitializeEx(0);
    }

    // Take control of the global interpreter lock while we are here, make sure to release it...
    PyGILState_STATE gil = PyGILState_Ensure();

    // call this once to allow us to add to, and report, sys.path later as needed
    PyRun_SimpleString("import sys"); // allows us to report sys.path later

    // we also need to set an extra import path to find some dynamic library loading stuff, again make it relative to the binary
    fs::path pathToDynLoad = FileSystem::makeNativePath(sanitizedProgramDir / "python_standard_lib/lib-dynload");
    fs::path libDirDynLoad = PluginManager::sanitizedPath(pathToDynLoad);
    PluginManager::addToPythonPath(state, libDirDynLoad, false);

    // now for additional paths:
    // we'll always want to add the program executable directory to PATH so that Python can find the installed pyenergyplus package
    // we will then optionally add the current working directory to allow Python to find scripts in the current directory
    // we will then optionally add the directory of the running IDF to allow Python to find scripts kept next to the IDF
    // we will then optionally add any additional paths the user specifies on the search paths object

    // so add the executable directory here
    PluginManager::addToPythonPath(state, sanitizedProgramDir, false);

    // Read all the additional search paths next
    std::string const sPaths = "PythonPlugin:SearchPaths";
    int searchPaths = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, sPaths);
    if (searchPaths == 0) {
        // no search path objects in the IDF, just do the default behavior: add the current working dir and the input file dir
        PluginManager::addToPythonPath(state, ".", false);
        fs::path sanitizedInputFileDir = PluginManager::sanitizedPath(state.dataStrGlobals->inputDirPath);
        PluginManager::addToPythonPath(state, sanitizedInputFileDir, false);
    }
    if (searchPaths > 0) {
        auto const instances = state.dataInputProcessing->inputProcessor->epJSON.find(sPaths);
        if (instances == state.dataInputProcessing->inputProcessor->epJSON.end()) {
            ShowSevereError(state,                                                                                   // LCOV_EXCL_LINE
                            "PythonPlugin:SearchPaths: Somehow getNumObjectsFound was > 0 but epJSON.find found 0"); // LCOV_EXCL_LINE
        }
        auto &instancesValue = instances.value();
        for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
            // This is a unique object, so we should have one, but this is fine
            auto const &fields = instance.value();
            auto const &thisObjectName = instance.key();
            state.dataInputProcessing->inputProcessor->markObjectAsUsed(sPaths, thisObjectName);
            std::string workingDirFlagUC = "YES";
            try {
                workingDirFlagUC =
                    EnergyPlus::UtilityRoutines::MakeUPPERCase(fields.at("add_current_working_directory_to_search_path").get<std::string>());
            } catch (nlohmann::json::out_of_range &e) {
                // defaulted to YES
            }
            if (workingDirFlagUC == "YES") {
                PluginManager::addToPythonPath(state, ".", false);
            }
            std::string inputFileDirFlagUC = "YES";
            try {
                inputFileDirFlagUC =
                    EnergyPlus::UtilityRoutines::MakeUPPERCase(fields.at("add_input_file_directory_to_search_path").get<std::string>());
            } catch (nlohmann::json::out_of_range &e) {
                // defaulted to YES
            }
            if (inputFileDirFlagUC == "YES") {
                fs::path sanitizedInputFileDir = PluginManager::sanitizedPath(state.dataStrGlobals->inputDirPath);
                PluginManager::addToPythonPath(state, sanitizedInputFileDir, false);
            }

            std::string epInDirFlagUC = "YES";
            try {
                epInDirFlagUC =
                    EnergyPlus::UtilityRoutines::MakeUPPERCase(fields.at("add_epin_environment_variable_to_search_path").get<std::string>());
            } catch (nlohmann::json::out_of_range &e) {
                // defaulted to YES
            }
            if (epInDirFlagUC == "YES") {
                std::string epin_path;
                get_environment_variable("epin", epin_path);
                fs::path epinPathObject = fs::path(epin_path);
                if (epinPathObject.empty()) {
                    EnergyPlus::ShowWarningMessage(
                        state,
                        "PluginManager: Search path inputs requested adding epin variable to Python path, but epin variable was empty, skipping.");
                } else {
                    auto epinRootDir = FileSystem::getParentDirectoryPath(fs::path(epinPathObject));
                    if (FileSystem::pathExists(epinRootDir)) {
                        fs::path sanitizedEnvInputDir = PluginManager::sanitizedPath(epinRootDir);
                        PluginManager::addToPythonPath(state, sanitizedEnvInputDir, true);
                    } else {
                        EnergyPlus::ShowWarningMessage(state,
                                                       "PluginManager: Search path inputs requested adding epin variable to Python path, but epin "
                                                       "variable value is not a valid existent path, skipping.");
                    }
                }
            }

            try {
                auto const vars = fields.at("py_search_paths");
                for (const auto &var : vars) {
                    try {
                        PluginManager::addToPythonPath(state, PluginManager::sanitizedPath(fs::path{var.at("search_path").get<std::string>()}), true);
                    } catch (nlohmann::json::out_of_range &e) {
                        // empty entry
                    }
                }
            } catch (nlohmann::json::out_of_range &e) {
                // catch when no paths are passed
                // nothing to do here
            }
        }
    } else {
        // if no search path objects exist, we still need to do the default searching
        PluginManager::addToPythonPath(state, ".", false);
        fs::path sanitizedInputFileDir = PluginManager::sanitizedPath(state.dataStrGlobals->inputDirPath);
        PluginManager::addToPythonPath(state, sanitizedInputFileDir, false);
        std::string epin_path;
        get_environment_variable("epin", epin_path);
        fs::path epinPathObject = fs::path(epin_path);
        if (!epinPathObject.empty()) {
            auto epinRootDir = FileSystem::getParentDirectoryPath(fs::path(epinPathObject));
            if (FileSystem::pathExists(epinRootDir)) {
                fs::path sanitizedEnvInputDir = PluginManager::sanitizedPath(epinRootDir);
                PluginManager::addToPythonPath(state, sanitizedEnvInputDir, true);
            }
        }
    }

    // Now read all the actual plugins and interpret them
    // IMPORTANT -- DO NOT CALL setup() UNTIL ALL INSTANCES ARE DONE
    std::string const sPlugins = "PythonPlugin:Instance";
    int pluginInstances = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, sPlugins);
    if (pluginInstances > 0) {
        auto const instances = state.dataInputProcessing->inputProcessor->epJSON.find(sPlugins);
        if (instances == state.dataInputProcessing->inputProcessor->epJSON.end()) {
            ShowSevereError(state,                                                                                // LCOV_EXCL_LINE
                            "PythonPlugin:Instance: Somehow getNumObjectsFound was > 0 but epJSON.find found 0"); // LCOV_EXCL_LINE
        }
        auto &instancesValue = instances.value();
        for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
            auto const &fields = instance.value();
            auto const &thisObjectName = instance.key();
            state.dataInputProcessing->inputProcessor->markObjectAsUsed(sPlugins, thisObjectName);
            fs::path modulePath(fields.at("python_module_name").get<std::string>());
            std::string className = fields.at("plugin_class_name").get<std::string>();
            std::string sWarmup = EnergyPlus::UtilityRoutines::MakeUPPERCase(fields.at("run_during_warmup_days").get<std::string>());
            bool warmup = false;
            if (sWarmup == "YES") {
                warmup = true;
            }
            state.dataPluginManager->plugins.emplace_back(modulePath, className, thisObjectName, warmup);
        }
    }

    // IMPORTANT - CALL setup() HERE ONCE ALL INSTANCES ARE CONSTRUCTED TO AVOID DESTRUCTOR/MEMORY ISSUES DURING VECTOR RESIZING
    for (auto &plugin : state.dataPluginManager->plugins) {
        plugin.setup(state);
    }

    std::string const sGlobals = "PythonPlugin:Variables";
    int globalVarInstances = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, sGlobals);
    if (globalVarInstances > 0) {
        auto const instances = state.dataInputProcessing->inputProcessor->epJSON.find(sGlobals);
        if (instances == state.dataInputProcessing->inputProcessor->epJSON.end()) {
            ShowSevereError(state, sGlobals + ": Somehow getNumObjectsFound was > 0 but epJSON.find found 0"); // LCOV_EXCL_LINE
        }
        std::set<std::string> uniqueNames;
        auto &instancesValue = instances.value();
        for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
            auto const &fields = instance.value();
            auto const &thisObjectName = instance.key();
            state.dataInputProcessing->inputProcessor->markObjectAsUsed(sGlobals, thisObjectName);
            auto const vars = fields.at("global_py_vars");
            for (const auto &var : vars) {
                std::string const varNameToAdd = var.at("variable_name").get<std::string>();
                if (uniqueNames.find(varNameToAdd) == uniqueNames.end()) {
                    this->addGlobalVariable(state, varNameToAdd);
                    uniqueNames.insert(varNameToAdd);
                } else {
                    ShowWarningMessage(state,
                                       format("Found duplicate variable name in PythonPLugin:Variables objects, ignoring: \"{}\"", varNameToAdd));
                }
            }
        }
    }

    // PythonPlugin:TrendVariable,
    //       \memo This object sets up a Python plugin trend variable from an Python plugin variable
    //       \memo A trend variable logs values across timesteps
    //       \min-fields 3
    //  A1 , \field Name
    //       \required-field
    //       \type alpha
    //  A2 , \field Name of a Python Plugin Variable
    //       \required-field
    //       \type alpha
    //  N1 ; \field Number of Timesteps to be Logged
    //       \required-field
    //       \type integer
    //       \minimum 1
    std::string const sTrends = "PythonPlugin:TrendVariable";
    int trendInstances = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, sTrends);
    if (trendInstances > 0) {
        auto const instances = state.dataInputProcessing->inputProcessor->epJSON.find(sTrends);
        if (instances == state.dataInputProcessing->inputProcessor->epJSON.end()) {
            ShowSevereError(state, sTrends + ": Somehow getNumObjectsFound was > 0 but epJSON.find found 0"); // LCOV_EXCL_LINE
        }
        auto &instancesValue = instances.value();
        for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
            auto const &fields = instance.value();
            auto const &thisObjectName = EnergyPlus::UtilityRoutines::MakeUPPERCase(instance.key());
            state.dataInputProcessing->inputProcessor->markObjectAsUsed(sGlobals, thisObjectName);
            std::string variableName = fields.at("name_of_a_python_plugin_variable").get<std::string>();
            int variableIndex = EnergyPlus::PluginManagement::PluginManager::getGlobalVariableHandle(state, variableName);
            int numValues = fields.at("number_of_timesteps_to_be_logged").get<int>();
            state.dataPluginManager->trends.emplace_back(state, thisObjectName, numValues, variableIndex);
            this->maxTrendVariableIndex++;
        }
    }

    // Release the global interpreter lock
    PyGILState_Release(gil);
    // setting up output variables deferred until later in the simulation setup process
#else
    // need to alert only if a plugin instance is found
    std::string const sPlugins = "PythonPlugin:Instance";
    int pluginInstances = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, sPlugins);
    if (pluginInstances > 0) {
        EnergyPlus::ShowFatalError(state, "Python Plugin instance found, but this build of EnergyPlus is not compiled with Python.");
    }
#endif
}

PluginManager::~PluginManager()
{
#if LINK_WITH_PYTHON
    if (!this->eplusRunningViaPythonAPI) {
        bool alreadyInitialized = (Py_IsInitialized() != 0);
        if (alreadyInitialized) {
            if (Py_FinalizeEx() < 0) {
                exit(120);
            }
        }
    }
#endif // LINK_WITH_PYTHON
}

#if LINK_WITH_PYTHON
fs::path PluginManager::sanitizedPath(fs::path const &path)
{
    // there are parts of this program that need to write out a string to execute in Python
    // because of that, escaped backslashes actually need double escaping
    // plus, the string cannot end with a backslash
    // sanitize the path to remove any trailing backslash
    if (path.empty()) {
        // this is really only likely to occur during unit testing, just return the original blank path
        return path;
    }
    std::string pathStr = path.string();
    if (pathStr.back() == '\\') {
        pathStr.erase(pathStr.size() - 1);
    }
    // then sanitize it to escape the backslashes for writing the string literal to Python
    std::string sanitizedDir;
    for (char i : pathStr) {
        if (i == '\\') {
            sanitizedDir += "\\\\";
        } else {
            sanitizedDir += i;
        }
    }
    return fs::path(sanitizedDir);
}
#else
fs::path PluginManager::sanitizedPath([[maybe_unused]] fs::path const &path)
{
    return fs::path();
}
#endif

void PluginInstance::reportPythonError([[maybe_unused]] EnergyPlusData &state)
{
#if LINK_WITH_PYTHON
    PyObject *exc_type = nullptr;
    PyObject *exc_value = nullptr;
    PyObject *exc_tb = nullptr;
    PyErr_Fetch(&exc_type, &exc_value, &exc_tb);
    // Normalizing the exception is needed. Without it, our custom EnergyPlusException go through just fine
    // but any ctypes built-in exception for eg will have wrong types
    PyErr_NormalizeException(&exc_type, &exc_value, &exc_tb);
    PyObject *str_exc_value = PyObject_Repr(exc_value); // Now a unicode object
    PyObject *pyStr2 = PyUnicode_AsEncodedString(str_exc_value, "utf-8", "Error ~");
    Py_DECREF(str_exc_value);
    char *strExcValue = PyBytes_AsString(pyStr2); // NOLINT(hicpp-signed-bitwise)
    Py_DECREF(pyStr2);
    EnergyPlus::ShowContinueError(state, "Python error description follows: ");
    EnergyPlus::ShowContinueError(state, strExcValue);

    // See if we can get a full traceback.
    // Calls into python, and does the same as capturing the exception in `e`
    // then `print(traceback.format_exception(e.type, e.value, e.tb))`
    PyObject *pModuleName = PyUnicode_DecodeFSDefault("traceback");
    PyObject *pyth_module = PyImport_Import(pModuleName);
    Py_DECREF(pModuleName);

    if (pyth_module == nullptr) {
        EnergyPlus::ShowContinueError(state, "Cannot find 'traceback' module in reportPythonError(), this is weird");
        return;
    }

    PyObject *pyth_func = PyObject_GetAttrString(pyth_module, "format_exception");
    Py_DECREF(pyth_module); // PyImport_Import returns a new reference, decrement it

    if (pyth_func || PyCallable_Check(pyth_func)) {

        PyObject *pyth_val = PyObject_CallFunction(pyth_func, "OOO", exc_type, exc_value, exc_tb);

        // traceback.format_exception returns a list, so iterate on that
        if (!pyth_val || !PyList_Check(pyth_val)) { // NOLINT(hicpp-signed-bitwise)
            EnergyPlus::ShowContinueError(state, "In reportPythonError(), traceback.format_exception did not return a list.");
            return;
        }

        unsigned long numVals = PyList_Size(pyth_val);
        if (numVals == 0) {
            EnergyPlus::ShowContinueError(state, "No traceback available");
            return;
        }

        EnergyPlus::ShowContinueError(state, "Python traceback follows: ");

        EnergyPlus::ShowContinueError(state, "```");

        for (unsigned long itemNum = 0; itemNum < numVals; itemNum++) {
            PyObject *item = PyList_GetItem(pyth_val, itemNum);
            if (PyUnicode_Check(item)) { // NOLINT(hicpp-signed-bitwise) -- something inside Python code causes warning
                std::string traceback_line = PyUnicode_AsUTF8(item);
                if (!traceback_line.empty() && traceback_line[traceback_line.length() - 1] == '\n') {
                    traceback_line.erase(traceback_line.length() - 1);
                }
                EnergyPlus::ShowContinueError(state, " >>> " + traceback_line);
            }
            // PyList_GetItem returns a borrowed reference, do not decrement
        }

        EnergyPlus::ShowContinueError(state, "```");

        // PyList_Size returns a borrowed reference, do not decrement
        Py_DECREF(pyth_val); // PyObject_CallFunction returns new reference, decrement
    }
    Py_DECREF(pyth_func); // PyObject_GetAttrString returns a new reference, decrement it
#endif
}

void PluginInstance::setup([[maybe_unused]] EnergyPlusData &state)
{
#if LINK_WITH_PYTHON
    // this first section is really all about just ultimately getting a full Python class instance
    // this answer helped with a few things: https://ru.stackoverflow.com/a/785927

    PyObject *pModuleName = PyUnicode_DecodeFSDefault(this->modulePath.string().c_str());
    this->pModule = PyImport_Import(pModuleName);
    // PyUnicode_DecodeFSDefault documentation does not explicitly say whether it returns a new or borrowed reference,
    // but other functions in that section say they return a new reference, and that makes sense to me, so I think we
    // should decrement it.
    Py_DECREF(pModuleName);
    if (!this->pModule) {
        EnergyPlus::ShowSevereError(state, "Failed to import module \"" + this->modulePath.string() + "\"");
        // ONLY call PyErr_Print if PyErr has occurred, otherwise it will cause other problems
        if (PyErr_Occurred()) {
            PluginInstance::reportPythonError(state);
        } else {
            EnergyPlus::ShowContinueError(state, "It could be that the module could not be found, or that there was an error in importing");
        }
        EnergyPlus::ShowFatalError(state, "Python import error causes program termination");
    }
    PyObject *pModuleDict = PyModule_GetDict(this->pModule);
    if (!pModuleDict) {
        EnergyPlus::ShowSevereError(state, "Failed to read module dictionary from module \"" + this->modulePath.string() + "\"");
        if (PyErr_Occurred()) {
            PluginInstance::reportPythonError(state);
        } else {
            EnergyPlus::ShowContinueError(state, "It could be that the module was empty");
        }
        EnergyPlus::ShowFatalError(state, "Python module error causes program termination");
    }
    std::string fileVarName = "__file__";
    PyObject *pFullPath = PyDict_GetItemString(pModuleDict, fileVarName.c_str());
    if (!pFullPath) {
        // something went really wrong, this should only happen if you do some *weird* python stuff like
        // import from database or something
        ShowFatalError(state, "Could not get full path");
    } else {
        PyObject *pStrObj = PyUnicode_AsUTF8String(pFullPath);
        char *zStr = PyBytes_AsString(pStrObj);
        std::string s(zStr);
        Py_DECREF(pStrObj); // PyUnicode_AsUTF8String returns a new reference, decrement it
        ShowMessage(state, "PythonPlugin: Class " + className + " imported from: " + s);
    }
    PyObject *pClass = PyDict_GetItemString(pModuleDict, className.c_str());
    // Py_DECREF(pModuleDict);  // PyModule_GetDict returns a borrowed reference, DO NOT decrement
    if (!pClass) {
        EnergyPlus::ShowSevereError(state, "Failed to get class type \"" + className + "\" from module \"" + modulePath.string() + "\"");
        if (PyErr_Occurred()) {
            PluginInstance::reportPythonError(state);
        } else {
            EnergyPlus::ShowContinueError(state, "It could be the class name is misspelled or missing.");
        }
        EnergyPlus::ShowFatalError(state, "Python class import error causes program termination");
    }
    if (!PyCallable_Check(pClass)) {
        EnergyPlus::ShowSevereError(state, "Got class type \"" + className + "\", but it cannot be called/instantiated");
        if (PyErr_Occurred()) {
            PluginInstance::reportPythonError(state);
        } else {
            EnergyPlus::ShowContinueError(state, "Is it possible the class name is actually just a variable?");
        }
        EnergyPlus::ShowFatalError(state, "Python class check error causes program termination");
    }
    this->pClassInstance = PyObject_CallObject(pClass, nullptr);
    // Py_DECREF(pClass);  // PyDict_GetItemString returns a borrowed reference, DO NOT decrement
    if (!this->pClassInstance) {
        EnergyPlus::ShowSevereError(state, "Something went awry calling class constructor for class \"" + className + "\"");
        if (PyErr_Occurred()) {
            PluginInstance::reportPythonError(state);
        } else {
            EnergyPlus::ShowContinueError(state, "It is possible the plugin class constructor takes extra arguments - it shouldn't.");
        }
        EnergyPlus::ShowFatalError(state, "Python class constructor error causes program termination");
    }
    // PyObject_CallObject returns a new reference, that we need to manage
    // I think we need to keep it around in memory though so the class methods can be called later on,
    // so I don't intend on decrementing it, at least not until the manager destructor
    // In any case, it will be an **extremely** tiny memory use if we hold onto it a bit too long

    // check which methods are overridden in the derived class
    std::string const detectOverriddenFunctionName = "_detect_overridden";
    PyObject *detectFunction = PyObject_GetAttrString(this->pClassInstance, detectOverriddenFunctionName.c_str());
    if (!detectFunction || !PyCallable_Check(detectFunction)) {
        EnergyPlus::ShowSevereError(state,
                                    "Could not find or call function \"" + detectOverriddenFunctionName + "\" on class \"" +
                                        this->modulePath.string() + "." + this->className + "\"");
        if (PyErr_Occurred()) {
            PluginInstance::reportPythonError(state);
        } else {
            EnergyPlus::ShowContinueError(state, "This function should be available on the base class, so this is strange.");
        }
        EnergyPlus::ShowFatalError(state, "Python _detect_overridden() function error causes program termination");
    }
    PyObject *pFunctionResponse = PyObject_CallFunction(detectFunction, nullptr);
    Py_DECREF(detectFunction); // PyObject_GetAttrString returns a new reference, decrement it
    if (!pFunctionResponse) {
        EnergyPlus::ShowSevereError(state, "Call to _detect_overridden() on " + this->stringIdentifier + " failed!");
        if (PyErr_Occurred()) {
            PluginInstance::reportPythonError(state);
        } else {
            EnergyPlus::ShowContinueError(state, "This is available on the base class and should not be overridden...strange.");
        }
        EnergyPlus::ShowFatalError(state, "Program terminates after call to _detect_overridden() on " + this->stringIdentifier + " failed!");
    }
    if (!PyList_Check(pFunctionResponse)) { // NOLINT(hicpp-signed-bitwise)
        EnergyPlus::ShowFatalError(state, "Invalid return from _detect_overridden() on class \"" + this->stringIdentifier + ", this is weird");
    }
    unsigned long numVals = PyList_Size(pFunctionResponse);
    // at this point we know which base class methods are being overridden by the derived class
    // we can loop over them and based on the name check the appropriate flag and assign the function pointer
    if (numVals == 0) {
        EnergyPlus::ShowFatalError(
            state, "Python plugin \"" + this->stringIdentifier + "\" did not override any base class methods; must override at least one");
    }
    for (unsigned long itemNum = 0; itemNum < numVals; itemNum++) {
        PyObject *item = PyList_GetItem(pFunctionResponse, itemNum);
        if (PyUnicode_Check(item)) { // NOLINT(hicpp-signed-bitwise) -- something inside Python code causes warning
            std::string functionName = PyUnicode_AsUTF8(item);
            if (functionName == this->sHookBeginNewEnvironment) {
                this->bHasBeginNewEnvironment = true;
                this->pBeginNewEnvironment = PyUnicode_FromString(functionName.c_str());
            } else if (functionName == this->sHookBeginZoneTimestepBeforeSetCurrentWeather) {
                this->bHasBeginZoneTimestepBeforeSetCurrentWeather = true;
                this->pBeginZoneTimestepBeforeSetCurrentWeather = PyUnicode_FromString(functionName.c_str());
            } else if (functionName == this->sHookAfterNewEnvironmentWarmUpIsComplete) {
                this->bHasAfterNewEnvironmentWarmUpIsComplete = true;
                this->pAfterNewEnvironmentWarmUpIsComplete = PyUnicode_FromString(functionName.c_str());
            } else if (functionName == this->sHookBeginZoneTimestepBeforeInitHeatBalance) {
                this->bHasBeginZoneTimestepBeforeInitHeatBalance = true;
                this->pBeginZoneTimestepBeforeInitHeatBalance = PyUnicode_FromString(functionName.c_str());
            } else if (functionName == this->sHookBeginZoneTimestepAfterInitHeatBalance) {
                this->bHasBeginZoneTimestepAfterInitHeatBalance = true;
                this->pBeginZoneTimestepAfterInitHeatBalance = PyUnicode_FromString(functionName.c_str());
            } else if (functionName == this->sHookBeginTimestepBeforePredictor) {
                this->bHasBeginTimestepBeforePredictor = true;
                this->pBeginTimestepBeforePredictor = PyUnicode_FromString(functionName.c_str());
            } else if (functionName == this->sHookAfterPredictorBeforeHVACManagers) {
                this->bHasAfterPredictorBeforeHVACManagers = true;
                this->pAfterPredictorBeforeHVACManagers = PyUnicode_FromString(functionName.c_str());
            } else if (functionName == this->sHookAfterPredictorAfterHVACManagers) {
                this->bHasAfterPredictorAfterHVACManagers = true;
                this->pAfterPredictorAfterHVACManagers = PyUnicode_FromString(functionName.c_str());
            } else if (functionName == this->sHookInsideHVACSystemIterationLoop) {
                this->bHasInsideHVACSystemIterationLoop = true;
                this->pInsideHVACSystemIterationLoop = PyUnicode_FromString(functionName.c_str());
            } else if (functionName == this->sHookEndOfZoneTimestepBeforeZoneReporting) {
                this->bHasEndOfZoneTimestepBeforeZoneReporting = true;
                this->pEndOfZoneTimestepBeforeZoneReporting = PyUnicode_FromString(functionName.c_str());
            } else if (functionName == this->sHookEndOfZoneTimestepAfterZoneReporting) {
                this->bHasEndOfZoneTimestepAfterZoneReporting = true;
                this->pEndOfZoneTimestepAfterZoneReporting = PyUnicode_FromString(functionName.c_str());
            } else if (functionName == this->sHookEndOfSystemTimestepBeforeHVACReporting) {
                this->bHasEndOfSystemTimestepBeforeHVACReporting = true;
                this->pEndOfSystemTimestepBeforeHVACReporting = PyUnicode_FromString(functionName.c_str());
            } else if (functionName == this->sHookEndOfSystemTimestepAfterHVACReporting) {
                this->bHasEndOfSystemTimestepAfterHVACReporting = true;
                this->pEndOfSystemTimestepAfterHVACReporting = PyUnicode_FromString(functionName.c_str());
            } else if (functionName == this->sHookEndOfZoneSizing) {
                this->bHasEndOfZoneSizing = true;
                this->pEndOfZoneSizing = PyUnicode_FromString(functionName.c_str());
            } else if (functionName == this->sHookEndOfSystemSizing) {
                this->bHasEndOfSystemSizing = true;
                this->pEndOfSystemSizing = PyUnicode_FromString(functionName.c_str());
            } else if (functionName == this->sHookAfterComponentInputReadIn) {
                this->bHasAfterComponentInputReadIn = true;
                this->pAfterComponentInputReadIn = PyUnicode_FromString(functionName.c_str());
            } else if (functionName == this->sHookUserDefinedComponentModel) {
                this->bHasUserDefinedComponentModel = true;
                this->pUserDefinedComponentModel = PyUnicode_FromString(functionName.c_str());
            } else if (functionName == this->sHookUnitarySystemSizing) {
                this->bHasUnitarySystemSizing = true;
                this->pUnitarySystemSizing = PyUnicode_FromString(functionName.c_str());
            } else {
                // the Python _detect_function worker is supposed to ignore any other functions so they don't show up at this point
                // I don't think it's appropriate to warn here, so just ignore and move on
            }
        }
        // PyList_GetItem returns a borrowed reference, do not decrement
    }
    // PyList_Size returns a borrowed reference, do not decrement
    Py_DECREF(pFunctionResponse); // PyObject_CallFunction returns new reference, decrement
#endif
}

void PluginInstance::shutdown() const
{
#if LINK_WITH_PYTHON
    Py_DECREF(this->pClassInstance);
    Py_DECREF(this->pModule); // PyImport_Import returns a new reference, decrement it
    if (this->bHasBeginNewEnvironment) Py_DECREF(this->pBeginNewEnvironment);
    if (this->bHasAfterNewEnvironmentWarmUpIsComplete) Py_DECREF(this->pAfterNewEnvironmentWarmUpIsComplete);
    if (this->bHasBeginZoneTimestepBeforeInitHeatBalance) Py_DECREF(this->pBeginZoneTimestepBeforeInitHeatBalance);
    if (this->bHasBeginZoneTimestepAfterInitHeatBalance) Py_DECREF(this->pBeginZoneTimestepAfterInitHeatBalance);
    if (this->bHasBeginTimestepBeforePredictor) Py_DECREF(this->pBeginTimestepBeforePredictor);
    if (this->bHasAfterPredictorBeforeHVACManagers) Py_DECREF(this->pAfterPredictorBeforeHVACManagers);
    if (this->bHasAfterPredictorAfterHVACManagers) Py_DECREF(this->pAfterPredictorAfterHVACManagers);
    if (this->bHasInsideHVACSystemIterationLoop) Py_DECREF(this->pInsideHVACSystemIterationLoop);
    if (this->bHasEndOfZoneTimestepBeforeZoneReporting) Py_DECREF(this->pEndOfZoneTimestepBeforeZoneReporting);
    if (this->bHasEndOfZoneTimestepAfterZoneReporting) Py_DECREF(this->pEndOfZoneTimestepAfterZoneReporting);
    if (this->bHasEndOfSystemTimestepBeforeHVACReporting) Py_DECREF(this->pEndOfSystemTimestepBeforeHVACReporting);
    if (this->bHasEndOfSystemTimestepAfterHVACReporting) Py_DECREF(this->pEndOfSystemTimestepAfterHVACReporting);
    if (this->bHasEndOfZoneSizing) Py_DECREF(this->pEndOfZoneSizing);
    if (this->bHasEndOfSystemSizing) Py_DECREF(this->pEndOfSystemSizing);
    if (this->bHasAfterComponentInputReadIn) Py_DECREF(this->pAfterComponentInputReadIn);
    if (this->bHasUserDefinedComponentModel) Py_DECREF(this->pUserDefinedComponentModel);
    if (this->bHasUnitarySystemSizing) Py_DECREF(this->pUnitarySystemSizing);
#endif
}

#if LINK_WITH_PYTHON
bool PluginInstance::run(EnergyPlusData &state, EMSManager::EMSCallFrom iCalledFrom) const
{
    // returns true if a plugin actually ran
    PyObject *pFunctionName = nullptr;
    const char *functionName = nullptr;
    if (iCalledFrom == EMSManager::EMSCallFrom::BeginNewEnvironment) {
        if (this->bHasBeginNewEnvironment) {
            pFunctionName = this->pBeginNewEnvironment;
            functionName = this->sHookBeginNewEnvironment;
        }
    } else if (iCalledFrom == EMSManager::EMSCallFrom::BeginZoneTimestepBeforeSetCurrentWeather) {
        if (this->bHasBeginZoneTimestepBeforeSetCurrentWeather) {
            pFunctionName = this->pBeginZoneTimestepBeforeSetCurrentWeather;
            functionName = this->sHookBeginZoneTimestepBeforeSetCurrentWeather;
        }
    } else if (iCalledFrom == EMSManager::EMSCallFrom::ZoneSizing) {
        if (this->bHasEndOfZoneSizing) {
            pFunctionName = this->pEndOfZoneSizing;
            functionName = this->sHookEndOfZoneSizing;
        }
    } else if (iCalledFrom == EMSManager::EMSCallFrom::SystemSizing) {
        if (this->bHasEndOfSystemSizing) {
            pFunctionName = this->pEndOfSystemSizing;
            functionName = this->sHookEndOfSystemSizing;
        }
    } else if (iCalledFrom == EMSManager::EMSCallFrom::BeginNewEnvironmentAfterWarmUp) {
        if (this->bHasAfterNewEnvironmentWarmUpIsComplete) {
            pFunctionName = this->pAfterNewEnvironmentWarmUpIsComplete;
            functionName = this->sHookAfterNewEnvironmentWarmUpIsComplete;
        }
    } else if (iCalledFrom == EMSManager::EMSCallFrom::BeginTimestepBeforePredictor) {
        if (this->bHasBeginTimestepBeforePredictor) {
            pFunctionName = this->pBeginTimestepBeforePredictor;
            functionName = this->sHookBeginTimestepBeforePredictor;
        }
    } else if (iCalledFrom == EMSManager::EMSCallFrom::BeforeHVACManagers) {
        if (this->bHasAfterPredictorBeforeHVACManagers) {
            pFunctionName = this->pAfterPredictorBeforeHVACManagers;
            functionName = this->sHookAfterPredictorBeforeHVACManagers;
        }
    } else if (iCalledFrom == EMSManager::EMSCallFrom::AfterHVACManagers) {
        if (this->bHasAfterPredictorAfterHVACManagers) {
            pFunctionName = this->pAfterPredictorAfterHVACManagers;
            functionName = this->sHookAfterPredictorAfterHVACManagers;
        }
    } else if (iCalledFrom == EMSManager::EMSCallFrom::HVACIterationLoop) {
        if (this->bHasInsideHVACSystemIterationLoop) {
            pFunctionName = this->pInsideHVACSystemIterationLoop;
            functionName = this->sHookInsideHVACSystemIterationLoop;
        }
    } else if (iCalledFrom == EMSManager::EMSCallFrom::EndSystemTimestepBeforeHVACReporting) {
        if (this->bHasEndOfSystemTimestepBeforeHVACReporting) {
            pFunctionName = this->pEndOfSystemTimestepBeforeHVACReporting;
            functionName = this->sHookEndOfSystemTimestepBeforeHVACReporting;
        }
    } else if (iCalledFrom == EMSManager::EMSCallFrom::EndSystemTimestepAfterHVACReporting) {
        if (this->bHasEndOfSystemTimestepAfterHVACReporting) {
            pFunctionName = this->pEndOfSystemTimestepAfterHVACReporting;
            functionName = this->sHookEndOfSystemTimestepAfterHVACReporting;
        }
    } else if (iCalledFrom == EMSManager::EMSCallFrom::EndZoneTimestepBeforeZoneReporting) {
        if (this->bHasEndOfZoneTimestepBeforeZoneReporting) {
            pFunctionName = this->pEndOfZoneTimestepBeforeZoneReporting;
            functionName = this->sHookEndOfZoneTimestepBeforeZoneReporting;
        }
    } else if (iCalledFrom == EMSManager::EMSCallFrom::EndZoneTimestepAfterZoneReporting) {
        if (this->bHasEndOfZoneTimestepAfterZoneReporting) {
            pFunctionName = this->pEndOfZoneTimestepAfterZoneReporting;
            functionName = this->sHookEndOfZoneTimestepAfterZoneReporting;
        }
    } else if (iCalledFrom == EMSManager::EMSCallFrom::ComponentGetInput) {
        if (this->bHasAfterComponentInputReadIn) {
            pFunctionName = this->pAfterComponentInputReadIn;
            functionName = this->sHookAfterComponentInputReadIn;
        }
    } else if (iCalledFrom == EMSManager::EMSCallFrom::UserDefinedComponentModel) {
        if (this->bHasUserDefinedComponentModel) {
            pFunctionName = this->pUserDefinedComponentModel;
            functionName = this->sHookUserDefinedComponentModel;
        }
    } else if (iCalledFrom == EMSManager::EMSCallFrom::UnitarySystemSizing) {
        if (this->bHasUnitarySystemSizing) {
            pFunctionName = this->pUnitarySystemSizing;
            functionName = this->sHookUnitarySystemSizing;
        }
    } else if (iCalledFrom == EMSManager::EMSCallFrom::BeginZoneTimestepBeforeInitHeatBalance) {
        if (this->bHasBeginZoneTimestepBeforeInitHeatBalance) {
            pFunctionName = this->pBeginZoneTimestepBeforeInitHeatBalance;
            functionName = this->sHookBeginZoneTimestepBeforeInitHeatBalance;
        }
    } else if (iCalledFrom == EMSManager::EMSCallFrom::BeginZoneTimestepAfterInitHeatBalance) {
        if (this->bHasBeginZoneTimestepAfterInitHeatBalance) {
            pFunctionName = this->pBeginZoneTimestepAfterInitHeatBalance;
            functionName = this->sHookBeginZoneTimestepAfterInitHeatBalance;
        }
    }

    // leave if we didn't find a match
    if (!pFunctionName) {
        return false;
    }

    // Get control of the global interpreter lock
    PyGILState_STATE gil = PyGILState_Ensure();

    // then call the main function
    // static const PyObject oneArgObjFormat = Py_BuildValue)("O");
    PyObject *pStateInstance = PyLong_FromVoidPtr((void *)&state);
    PyObject *pFunctionResponse = PyObject_CallMethodObjArgs(this->pClassInstance, pFunctionName, pStateInstance, nullptr);
    Py_DECREF(pStateInstance);
    if (!pFunctionResponse) {
        std::string const functionNameAsString(functionName); // only convert to string if an error occurs
        EnergyPlus::ShowSevereError(state, "Call to " + functionNameAsString + "() on " + this->stringIdentifier + " failed!");
        if (PyErr_Occurred()) {
            PluginInstance::reportPythonError(state);
        } else {
            EnergyPlus::ShowContinueError(state, "This could happen for any number of reasons, check the plugin code.");
        }
        PyGILState_Release(gil);
        EnergyPlus::ShowFatalError(state,
                                   "Program terminates after call to " + functionNameAsString + "() on " + this->stringIdentifier + " failed!");
    }
    if (PyLong_Check(pFunctionResponse)) { // NOLINT(hicpp-signed-bitwise)
        auto exitCode = PyLong_AsLong(pFunctionResponse);
        if (exitCode == 0) {
            // success
        } else if (exitCode == 1) {
            PyGILState_Release(gil);
            EnergyPlus::ShowFatalError(state, "Python Plugin \"" + this->stringIdentifier + "\" returned 1 to indicate EnergyPlus should abort");
        }
    } else {
        std::string const functionNameAsString(functionName); // only convert to string if an error occurs
        PyGILState_Release(gil);
        EnergyPlus::ShowFatalError(state,
                                   "Invalid return from " + functionNameAsString + "() on class \"" + this->stringIdentifier +
                                       ", make sure it returns an integer exit code, either zero (success) or one (failure)");
    }
    Py_DECREF(pFunctionResponse); // PyObject_CallFunction returns new reference, decrement
    if (state.dataPluginManager->apiErrorFlag) {
        PyGILState_Release(gil);
        EnergyPlus::ShowFatalError(state, "API problems encountered while running plugin cause program termination.");
    }
    PyGILState_Release(gil);
    return true;
}
#else
bool PluginInstance::run([[maybe_unused]] EnergyPlusData &state, [[maybe_unused]] EMSManager::EMSCallFrom iCalledFrom) const
{
    return false;
}
#endif

#if LINK_WITH_PYTHON
void PluginManager::addToPythonPath(EnergyPlusData &state, const fs::path &path, bool userDefinedPath)
{
    if (path.empty()) return;

    std::string command = "sys.path.insert(0, \"" + path.string() + "\")";
    if (PyRun_SimpleString(command.c_str()) == 0) {
        if (userDefinedPath) {
            EnergyPlus::ShowMessage(state, "Successfully added path \"" + path.string() + "\" to the sys.path in Python");
        }
        // PyRun_SimpleString)("print(' EPS : ' + str(sys.path))");
    } else {
        EnergyPlus::ShowFatalError(state, "ERROR adding \"" + path.string() + "\" to the sys.path in Python");
    }
}
#else
void PluginManager::addToPythonPath([[maybe_unused]] EnergyPlusData &state,
                                    [[maybe_unused]] const fs::path &path,
                                    [[maybe_unused]] bool userDefinedPath)
{
}
#endif

#if LINK_WITH_PYTHON
void PluginManager::addGlobalVariable(EnergyPlusData &state, const std::string &name)
{
    std::string const varNameUC = EnergyPlus::UtilityRoutines::MakeUPPERCase(name);
    state.dataPluginManager->globalVariableNames.push_back(varNameUC);
    state.dataPluginManager->globalVariableValues.push_back(Real64());
    this->maxGlobalVariableIndex++;
}
#else
void PluginManager::addGlobalVariable([[maybe_unused]] EnergyPlusData &state, [[maybe_unused]] const std::string &name)
{
}
#endif

#if LINK_WITH_PYTHON
int PluginManager::getGlobalVariableHandle(EnergyPlusData &state, const std::string &name, bool const suppress_warning)
{ // note zero is a valid handle
    std::string const varNameUC = EnergyPlus::UtilityRoutines::MakeUPPERCase(name);
    auto const it = std::find(state.dataPluginManager->globalVariableNames.begin(), state.dataPluginManager->globalVariableNames.end(), varNameUC);
    if (it != state.dataPluginManager->globalVariableNames.end()) {
        return std::distance(state.dataPluginManager->globalVariableNames.begin(), it);
    } else {
        if (suppress_warning) {
            return -1;
        } else {
            EnergyPlus::ShowSevereError(state, "Tried to retrieve handle for a nonexistent plugin global variable");
            EnergyPlus::ShowContinueError(state, "Name looked up: \"" + varNameUC + "\", available names: ");
            for (auto const &gvName : state.dataPluginManager->globalVariableNames) {
                EnergyPlus::ShowContinueError(state, "    \"" + gvName + "\"");
            }
            EnergyPlus::ShowFatalError(state, "Plugin global variable problem causes program termination");
            return -1; // hush the compiler warning
        }
    }
}
#else
int PluginManager::getGlobalVariableHandle([[maybe_unused]] EnergyPlusData &state,
                                           [[maybe_unused]] const std::string &name,
                                           [[maybe_unused]] bool const suppress_warning)
{
    return -1;
}
#endif

#if LINK_WITH_PYTHON
int PluginManager::getTrendVariableHandle(EnergyPlusData &state, const std::string &name)
{
    std::string const varNameUC = EnergyPlus::UtilityRoutines::MakeUPPERCase(name);
    for (size_t i = 0; i < state.dataPluginManager->trends.size(); i++) {
        auto &thisTrend = state.dataPluginManager->trends[i];
        if (thisTrend.name == varNameUC) {
            return i;
        }
    }
    return -1;
}
#else
int PluginManager::getTrendVariableHandle([[maybe_unused]] EnergyPlusData &state, [[maybe_unused]] const std::string &name)
{
    return -1;
}
#endif

#if LINK_WITH_PYTHON
Real64 PluginManager::getTrendVariableValue(EnergyPlusData &state, int handle, int timeIndex)
{
    return state.dataPluginManager->trends[handle].values[timeIndex];
}
#else
Real64 PluginManager::getTrendVariableValue([[maybe_unused]] EnergyPlusData &state, [[maybe_unused]] int handle, [[maybe_unused]] int timeIndex)
{
    return 0.0;
}
#endif

#if LINK_WITH_PYTHON
Real64 PluginManager::getTrendVariableAverage(EnergyPlusData &state, int handle, int count)
{
    Real64 sum = 0;
    for (int i = 0; i < count; i++) {
        sum += state.dataPluginManager->trends[handle].values[i];
    }
    return sum / count;
}
#else
Real64 PluginManager::getTrendVariableAverage([[maybe_unused]] EnergyPlusData &state, [[maybe_unused]] int handle, [[maybe_unused]] int count)
{
    return 0.0;
}
#endif

#if LINK_WITH_PYTHON
Real64 PluginManager::getTrendVariableMin(EnergyPlusData &state, int handle, int count)
{
    Real64 minimumValue = 9999999999999;
    for (int i = 0; i < count; i++) {
        if (state.dataPluginManager->trends[handle].values[i] < minimumValue) {
            minimumValue = state.dataPluginManager->trends[handle].values[i];
        }
    }
    return minimumValue;
}
#else
Real64 PluginManager::getTrendVariableMin([[maybe_unused]] EnergyPlusData &state, [[maybe_unused]] int handle, [[maybe_unused]] int count)
{
    return 0.0;
}
#endif

#if LINK_WITH_PYTHON
Real64 PluginManager::getTrendVariableMax(EnergyPlusData &state, int handle, int count)
{
    Real64 maximumValue = -9999999999999;
    for (int i = 0; i < count; i++) {
        if (state.dataPluginManager->trends[handle].values[i] > maximumValue) {
            maximumValue = state.dataPluginManager->trends[handle].values[i];
        }
    }
    return maximumValue;
}
#else
Real64 PluginManager::getTrendVariableMax([[maybe_unused]] EnergyPlusData &state, [[maybe_unused]] int handle, [[maybe_unused]] int count)
{
    return 0.0;
}
#endif

#if LINK_WITH_PYTHON
Real64 PluginManager::getTrendVariableSum(EnergyPlusData &state, int handle, int count)
{
    Real64 sum = 0.0;
    for (int i = 0; i < count; i++) {
        sum += state.dataPluginManager->trends[handle].values[i];
    }
    return sum;
}
#else
Real64 PluginManager::getTrendVariableSum([[maybe_unused]] EnergyPlusData &state, [[maybe_unused]] int handle, [[maybe_unused]] int count)
{
    return 0.0;
}
#endif

#if LINK_WITH_PYTHON
Real64 PluginManager::getTrendVariableDirection(EnergyPlusData &state, int handle, int count)
{
    auto &trend = state.dataPluginManager->trends[handle];
    Real64 timeSum = 0.0;
    Real64 valueSum = 0.0;
    Real64 crossSum = 0.0;
    Real64 powSum = 0.0;
    for (int i = 0; i < count; i++) {
        timeSum += trend.times[i];
        valueSum += trend.values[i];
        crossSum += trend.times[i] * trend.values[i];
        powSum += pow2(trend.times[i]);
    }
    Real64 numerator = timeSum * valueSum - count * crossSum;
    Real64 denominator = pow_2(timeSum) - count * powSum;
    return numerator / denominator;
}
#else
Real64 PluginManager::getTrendVariableDirection([[maybe_unused]] EnergyPlusData &state, [[maybe_unused]] int handle, [[maybe_unused]] int count)
{
    return 0.0;
}
#endif

#if LINK_WITH_PYTHON
size_t PluginManager::getTrendVariableHistorySize(EnergyPlusData &state, int handle)
{
    return state.dataPluginManager->trends[handle].values.size();
}
#else
size_t PluginManager::getTrendVariableHistorySize([[maybe_unused]] EnergyPlusData &state, [[maybe_unused]] int handle)
{
    return 0;
}
#endif

void PluginManager::updatePluginValues([[maybe_unused]] EnergyPlusData &state)
{
#if LINK_WITH_PYTHON
    for (auto &trend : state.dataPluginManager->trends) {
        Real64 newVarValue = PluginManager::getGlobalVariableValue(state, trend.indexOfPluginVariable);
        trend.values.push_front(newVarValue);
        trend.values.pop_back();
    }
#endif
}

#if LINK_WITH_PYTHON
Real64 PluginManager::getGlobalVariableValue(EnergyPlusData &state, int handle)
{
    if (state.dataPluginManager->globalVariableValues.empty()) {
        EnergyPlus::ShowFatalError(
            state,
            "Tried to access plugin global variable but it looks like there aren't any; use the PythonPlugin:Variables object to declare them.");
    }
    try {
        return state.dataPluginManager->globalVariableValues[handle]; // TODO: This won't be caught as an exception I think
    } catch (...) {
        EnergyPlus::ShowSevereError(state, format("Tried to access plugin global variable value at index {}", handle));
        EnergyPlus::ShowContinueError(state,
                                      format("Available handles range from 0 to {}", state.dataPluginManager->globalVariableValues.size() - 1));
        EnergyPlus::ShowFatalError(state, "Plugin global variable problem causes program termination");
    }
    return 0.0;
}
#else
Real64 PluginManager::getGlobalVariableValue([[maybe_unused]] EnergyPlusData &state, [[maybe_unused]] int handle)
{
    return 0.0;
}
#endif

#if LINK_WITH_PYTHON
void PluginManager::setGlobalVariableValue(EnergyPlusData &state, int handle, Real64 value)
{
    if (state.dataPluginManager->globalVariableValues.empty()) {
        EnergyPlus::ShowFatalError(state,
                                   "Tried to set plugin global variable but it looks like there aren't any; use the PythonPlugin:GlobalVariables "
                                   "object to declare them.");
    }
    try {
        state.dataPluginManager->globalVariableValues[handle] = value; // TODO: This won't be caught as an exception I think
    } catch (...) {
        EnergyPlus::ShowSevereError(state, format("Tried to set plugin global variable value at index {}", handle));
        EnergyPlus::ShowContinueError(state,
                                      format("Available handles range from 0 to {}", state.dataPluginManager->globalVariableValues.size() - 1));
        EnergyPlus::ShowFatalError(state, "Plugin global variable problem causes program termination");
    }
}
#else
void PluginManager::setGlobalVariableValue([[maybe_unused]] EnergyPlusData &state, [[maybe_unused]] int handle, [[maybe_unused]] Real64 value)
{
}
#endif

#if LINK_WITH_PYTHON
int PluginManager::getLocationOfUserDefinedPlugin(EnergyPlusData &state, std::string const &_programName)
{
    for (size_t handle = 0; handle < state.dataPluginManager->plugins.size(); handle++) {
        auto const thisPlugin = state.dataPluginManager->plugins[handle];
        if (EnergyPlus::UtilityRoutines::MakeUPPERCase(thisPlugin.emsAlias) == EnergyPlus::UtilityRoutines::MakeUPPERCase(_programName)) {
            return handle;
        }
    }
    return -1;
}
#else
int PluginManager::getLocationOfUserDefinedPlugin([[maybe_unused]] EnergyPlusData &state, [[maybe_unused]] std::string const &_programName)
{
    return -1;
}
#endif

#if LINK_WITH_PYTHON
void PluginManager::runSingleUserDefinedPlugin(EnergyPlusData &state, int index)
{
    state.dataPluginManager->plugins[index].run(state, EMSManager::EMSCallFrom::UserDefinedComponentModel);
}
#else
void PluginManager::runSingleUserDefinedPlugin([[maybe_unused]] EnergyPlusData &state, [[maybe_unused]] int index)
{
}
#endif

#if LINK_WITH_PYTHON
bool PluginManager::anyUnexpectedPluginObjects(EnergyPlusData &state)
{
    int numTotalThings = 0;
    for (auto const &objToFind : state.dataPluginManager->objectsToFind) {
        int instances = state.dataInputProcessing->inputProcessor->getNumObjectsFound(state, objToFind);
        numTotalThings += instances;
        if (numTotalThings == 1) {
            ShowSevereMessage(state, "Found PythonPlugin objects in an IDF that is running in an API/Library workflow...this is invalid");
        }
        if (instances > 0) {
            ShowContinueError(state, "Invalid PythonPlugin object type: " + objToFind);
        }
    }
    return numTotalThings > 0;
}
#else
bool PluginManager::anyUnexpectedPluginObjects([[maybe_unused]] EnergyPlusData &state)
{
    return false;
}
#endif

} // namespace EnergyPlus::PluginManagement
