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

#include <map>

#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataStringGlobals.hh>
#include <EnergyPlus/FileSystem.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/PluginManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

#ifdef _DEBUG
// We don't want to try to import a debug build of Python here
// so if we are building a Debug build of the C++ code, we need
// to undefine _DEBUG during the #include command for Python.h.
// Otherwise it will fail
#undef _DEBUG
#include <Python.h>
#define _DEBUG
#else
#include <Python.h>
#endif

namespace EnergyPlus {
namespace PluginManager {
    std::unique_ptr<PluginManager> pluginManager;

    std::map<int, std::vector<void (*)()>> callbacks;
    std::map<int, std::vector<PluginInstance>> plugins;

    void registerNewCallback(int iCalledFrom, void (*f)())
    {
        callbacks[iCalledFrom].push_back(f);
    }

    void runAnyRegisteredCallbacks(int iCalledFrom)
    {
        for (auto const &cb : callbacks[iCalledFrom]) {
            cb();
        }
        for (auto &plugin : plugins[iCalledFrom]) {
            if (plugin.runDuringWarmup || !DataGlobals::WarmupFlag) plugin.run();
        }
    }

    void clear_state()
    {
        callbacks.clear();
        plugins.clear();
        pluginManager.reset(); // delete the current plugin manager instance, which was created in simulation manager, this clean up Python
    }

    // Python Plugin stuff here

    PluginManager::PluginManager()
    {
        // from https://docs.python.org/3/c-api/init.html
        // If arg 0, it skips init registration of signal handlers, which might be useful when Python is embedded.
        Py_InitializeEx(0);

        PyRun_SimpleString("import sys"); // allows us to report sys.path later

        // we'll always want to add the program executable directory to PATH so that Python can find the installed pyenergyplus package
        // we will then optionally add the current working directory to allow Python to find scripts in the current directory
        // we will then optionally add the directory of the running IDF to allow Python to find scripts kept next to the IDF
        // we will then optionally add any additional paths the user specifies on the search paths object
        std::string programPath = FileSystem::getProgramPath();
        std::string programDir = FileSystem::getParentDirectoryPath(programPath);
        std::string sanitizedDir = PluginManager::sanitizedPath(programDir);
        PluginManager::addToPythonPath(sanitizedDir, false);

        // Read all the additional search paths next
        std::string const sPaths = "PythonPlugin:SearchPaths";
        int searchPaths = inputProcessor->getNumObjectsFound(sPaths);
        if (searchPaths > 0) {
            auto const instances = inputProcessor->epJSON.find(sPaths);
            if (instances == inputProcessor->epJSON.end()) {
                ShowSevereError(                                                                             // LCOV_EXCL_LINE
                    "PythonPlugin:SearchPaths: Somehow getNumObjectsFound was > 0 but epJSON.find found 0"); // LCOV_EXCL_LINE
            }
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                // This is a unique object, so we should have one, but this is fine
                auto const &fields = instance.value();
                auto const &thisObjectName = instance.key();
                inputProcessor->markObjectAsUsed(sPaths, thisObjectName);
                std::string workingDirFlagUC = EnergyPlus::UtilityRoutines::MakeUPPERCase(fields.at("add_current_working_directory_to_search_path"));
                if (workingDirFlagUC == "YES") {
                    PluginManager::addToPythonPath(".", false);
                }
                std::string inputFileDirFlagUC = EnergyPlus::UtilityRoutines::MakeUPPERCase(fields.at("add_input_file_directory_to_search_path"));
                if (inputFileDirFlagUC == "YES") {
                    std::string sanitizedInputFileDir = PluginManager::sanitizedPath(DataStringGlobals::inputDirPathName);
                    PluginManager::addToPythonPath(sanitizedInputFileDir, false);
                }
                if (fields.find("search_path_1") != fields.end()) {
                    PluginManager::addToPythonPath(PluginManager::sanitizedPath(fields.at("search_path_1")), true);
                }
                if (fields.find("search_path_2") != fields.end()) {
                    PluginManager::addToPythonPath(PluginManager::sanitizedPath(fields.at("search_path_2")), true);
                }
                if (fields.find("search_path_3") != fields.end()) {
                    PluginManager::addToPythonPath(PluginManager::sanitizedPath(fields.at("search_path_3")), true);
                }
                if (fields.find("search_path_4") != fields.end()) {
                    PluginManager::addToPythonPath(PluginManager::sanitizedPath(fields.at("search_path_4")), true);
                }
                if (fields.find("search_path_5") != fields.end()) {
                    PluginManager::addToPythonPath(PluginManager::sanitizedPath(fields.at("search_path_5")), true);
                }
            }
        }

        // Now read all the actual plugins and interpret them
        std::string const sPlugins = "PythonPlugin:Instance";
        int pluginInstances = inputProcessor->getNumObjectsFound(sPaths);
        if (pluginInstances > 0) {
            auto const instances = inputProcessor->epJSON.find(sPlugins);
            if (instances == inputProcessor->epJSON.end()) {
                ShowSevereError(                                                                          // LCOV_EXCL_LINE
                    "PythonPlugin:Instance: Somehow getNumObjectsFound was > 0 but epJSON.find found 0"); // LCOV_EXCL_LINE
            }
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                auto const &fields = instance.value();
                auto const &thisObjectName = instance.key();
                inputProcessor->markObjectAsUsed(sPlugins, thisObjectName);
                std::string fileName = fields.at("python_module_name");
                std::string className = fields.at("plugin_class_name");
                std::string sCallingPoint = EnergyPlus::UtilityRoutines::MakeUPPERCase(fields.at("calling_point"));
                std::string sWarmup = EnergyPlus::UtilityRoutines::MakeUPPERCase(fields.at("run_during_warmup_days"));
                bool warmup = false;
                if (sWarmup == "YES") {
                    warmup = true;
                }
                int iCalledFrom = PluginManager::calledFromFromString(sCallingPoint);
                plugins[iCalledFrom].emplace_back(fileName, className, thisObjectName, warmup);
            }
        }

        std::string const sGlobals = "PythonPlugin:Variables";
        int globalVarInstances = inputProcessor->getNumObjectsFound(sGlobals);
        if (globalVarInstances > 0) {
            auto const instances = inputProcessor->epJSON.find(sGlobals);
            if (instances == inputProcessor->epJSON.end()) {
                ShowSevereError(sGlobals + ": Somehow getNumObjectsFound was > 0 but epJSON.find found 0"); // LCOV_EXCL_LINE
            }
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                auto const &fields = instance.value();
                auto const &thisObjectName = instance.key();
                inputProcessor->markObjectAsUsed(sGlobals, thisObjectName);
                if (fields.find("variable_name_1") != fields.end()) {
                    PluginManager::addGlobalVariable(fields.at("variable_name_1"));
                }
                if (fields.find("variable_name_2") != fields.end()) {
                    PluginManager::addGlobalVariable(fields.at("variable_name_2"));
                }
                if (fields.find("variable_name_3") != fields.end()) {
                    PluginManager::addGlobalVariable(fields.at("variable_name_3"));
                }
                if (fields.find("variable_name_4") != fields.end()) {
                    PluginManager::addGlobalVariable(fields.at("variable_name_4"));
                }
                if (fields.find("variable_name_5") != fields.end()) {
                    PluginManager::addGlobalVariable(fields.at("variable_name_5"));
                }
            }

        }

        // with the PythonPlugin:Variables all set in memory, we can now set them up as outputs as needed
        std::string const sOutputVariable = "PythonPlugin:OutputVariable";
        int outputVarInstances = inputProcessor->getNumObjectsFound(sOutputVariable);
        if (outputVarInstances > 0) {
            auto const instances = inputProcessor->epJSON.find(sOutputVariable);
            if (instances == inputProcessor->epJSON.end()) {
                ShowSevereError(sOutputVariable + ": Somehow getNumObjectsFound was > 0 but epJSON.find found 0"); // LCOV_EXCL_LINE
            }
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                auto const &fields = instance.value();
                auto const &thisObjectName = instance.key();
                auto const objNameUC = EnergyPlus::UtilityRoutines::MakeUPPERCase(thisObjectName);
                // no need to validate name, the JSON will validate that.
                inputProcessor->markObjectAsUsed(sOutputVariable, thisObjectName);
                std::string varName = fields.at("python_plugin_variable_name");
                std::string avgOrSum = EnergyPlus::UtilityRoutines::MakeUPPERCase(fields.at("type_of_data_in_variable"));
                std::string updateFreq = EnergyPlus::UtilityRoutines::MakeUPPERCase(fields.at("update_frequency"));
                std::string units;
                if (fields.find("units") != fields.end()) {
                    units = fields.at("units");
                }
                // get the index of the global variable, fatal if it doesn't mach one
                // validate type of data, update frequency, and look up units enum value
                // call setup output variable - variable TYPE is "PythonPlugin:OutputVariable"
                int variableHandle = this->getGlobalVariableHandle(varName);
                if (variableHandle == -1) {
                    EnergyPlus::ShowSevereError("Failed to match Python Plugin Output Variable");
                    EnergyPlus::ShowContinueError("Trying to create output instance for variable name \"" + varName + "\"");
                    EnergyPlus::ShowContinueError("No match found, make sure variable is listed in PythonPlugin:Variables object");
                    EnergyPlus::ShowFatalError("Python Plugin Output Variable problem causes program termination");
                }
                bool isMetered = false;
                std::string sAvgOrSum = "Average";
                if (avgOrSum == "SUMMED") {
                    sAvgOrSum = "Sum";
                } else if (avgOrSum == "METERED") {
                    sAvgOrSum = "Sum";
                    isMetered = true;
                }
                std::string sUpdateFreq = "Zone";
                if (updateFreq == "SYSTEMTIMESTEP") {
                    sUpdateFreq = "System";
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
                        SetupOutputVariable(sOutputVariable, thisUnit, this->globalVariableValues[variableHandle], sUpdateFreq, sAvgOrSum, thisObjectName);
                    } else {
                        SetupOutputVariable(sOutputVariable,
                                            thisUnit,
                                            this->globalVariableValues[variableHandle],
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
                        EnergyPlus::ShowSevereError("Input error on PythonPlugin:OutputVariable = " + thisObjectName);
                        EnergyPlus::ShowContinueError("The variable was marked as metered, but did not define a resource type");
                        EnergyPlus::ShowContinueError("For metered variables, the resource type, group type, and end use category must be defined");
                        EnergyPlus::ShowFatalError("Input error on PythonPlugin::OutputVariable causes program termination");
                    }
                    std::string const resourceType = EnergyPlus::UtilityRoutines::MakeUPPERCase(fields.at("resource_type"));
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
                    } else if (resourceType == "FUELOIL#1") {
                        sResourceType = "FuelOil#1";
                    } else if (resourceType == "FUELOIL#2") {
                        sResourceType = "FuelOil#2";
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
                        ShowSevereError("Invalid input for PythonPlugin:OutputVariable, unexpected Resource Type = " + resourceType);
                        ShowFatalError("Python plugin output variable input problem causes program termination");
                    }

                    // Group Type
                    if (fields.find("group_type") == fields.end()) {
                        EnergyPlus::ShowSevereError("Input error on PythonPlugin:OutputVariable = " + thisObjectName);
                        EnergyPlus::ShowContinueError("The variable was marked as metered, but did not define a group type");
                        EnergyPlus::ShowContinueError("For metered variables, the resource type, group type, and end use category must be defined");
                        EnergyPlus::ShowFatalError("Input error on PythonPlugin::OutputVariable causes program termination");
                    }
                    std::string const groupType = EnergyPlus::UtilityRoutines::MakeUPPERCase(fields.at("group_type"));
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
                        ShowSevereError("Invalid input for PythonPlugin:OutputVariable, unexpected Group Type = " + groupType);
                        ShowFatalError("Python plugin output variable input problem causes program termination");
                    }

                    // End Use Type
                    if (fields.find("end_use_category") == fields.end()) {
                        EnergyPlus::ShowSevereError("Input error on PythonPlugin:OutputVariable = " + thisObjectName);
                        EnergyPlus::ShowContinueError("The variable was marked as metered, but did not define an end-use category");
                        EnergyPlus::ShowContinueError("For metered variables, the resource type, group type, and end use category must be defined");
                        EnergyPlus::ShowFatalError("Input error on PythonPlugin::OutputVariable causes program termination");
                    }
                    std::string const endUse = EnergyPlus::UtilityRoutines::MakeUPPERCase(fields.at("end_use_category"));
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
                        ShowSevereError(
                                "Invalid input for PythonPlugin:OutputVariable, unexpected End-use Subcategory = " +
                                groupType);
                        ShowFatalError("Python plugin output variable input problem causes program termination");
                    }

                    // Additional End Use Types Only Used for EnergyTransfer
                    if ((sResourceType != "EnergyTransfer") &&
                        (sEndUse == "HeatingCoils" || sEndUse == "CoolingCoils" ||
                                sEndUse == "Chillers" ||
                                sEndUse == "Boilers" || sEndUse == "Baseboard" ||
                                sEndUse == "HeatRecoveryForCooling" ||
                                sEndUse == "HeatRecoveryForHeating")) {
                        ShowWarningError("Inconsistent resource type input for PythonPlugin:OutputVariable = " + thisObjectName);
                        ShowContinueError("For end use subcategory = " + sEndUse + ", resource type must be EnergyTransfer");
                        ShowContinueError("Resource type is being reset to EnergyTransfer and the simulation continues...");
                        sResourceType = "EnergyTransfer";
                    }

                    std::string sEndUseSubcategory;
                    if (fields.find("end_use_subcategory") != fields.end()) {
                        sEndUseSubcategory = fields.at("end_use_subcategory");
                    }

                    if (sEndUseSubcategory.empty()) { // no subcategory
                        SetupOutputVariable(sOutputVariable,
                                            thisUnit,
                                            this->globalVariableValues[variableHandle],
                                            sUpdateFreq,
                                            sAvgOrSum,
                                            thisObjectName,
                                            _,
                                            sResourceType,
                                            sEndUse,
                                            _,
                                            sGroupType);
                    } else { // has subcategory
                        SetupOutputVariable(sOutputVariable,
                                            thisUnit,
                                            this->globalVariableValues[variableHandle],
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

    }

    std::string PluginManager::sanitizedPath(std::string path)
    {
        // there are parts of this program that need to write out a string to execute in Python
        // because of that, escaped backslashes actually need double escaping
        // plus, the string cannot end with a backslash
        // sanitize the path to remove any trailing backslash
        if (path.substr(path.length() - 1, path.length()) == "\\") {
            path = path.substr(0, path.length() - 1);
        }
        // then sanitize it to escape the backslashes for writing the string literal to Python
        std::string sanitizedDir;
        for (char i : path) {
            if (i == '\\') {
                sanitizedDir += "\\\\";
            } else {
                sanitizedDir += i;
            }
        }
        return sanitizedDir;
    }

    void PluginInstance::reportPythonError() {
        PyObject *exc_type = nullptr, *exc_value = nullptr, *exc_tb = nullptr;
        PyErr_Fetch(&exc_type, &exc_value, &exc_tb);
        PyObject* str_exc_value = PyObject_Repr(exc_value); //Now a unicode object
        PyObject* pyStr2 = PyUnicode_AsEncodedString(str_exc_value, "utf-8", "Error ~");
        char *strExcValue =  PyBytes_AS_STRING(pyStr2);
        EnergyPlus::ShowContinueError("Python error description follows: ");
        EnergyPlus::ShowContinueError(strExcValue);
    }

    PluginInstance::PluginInstance(const std::string &moduleName, const std::string &className, const std::string &emsName, const bool runPluginDuringWarmup)
    {
        // this first section is really all about just ultimately getting a full Python class instance
        // this answer helped with a few things: https://ru.stackoverflow.com/a/785927

        PyObject *pModuleName = PyUnicode_DecodeFSDefault(moduleName.c_str());
        PyObject *pModule = PyImport_Import(pModuleName);
        // PyUnicode_DecodeFSDefault documentation does not explicitly say whether it returns a new or borrowed reference,
        // but other functions in that section say they return a new reference, and that makes sense to me, so I think we
        // should decrement it.
        Py_DECREF(pModuleName);
        if (!pModule) {
            EnergyPlus::ShowSevereError("Failed to import module \"" + moduleName + "\"");
            // ONLY call PyErr_Print if PyErr has occurred, otherwise it will cause other problems
            if (PyErr_Occurred()) {
                PluginInstance::reportPythonError();
            } else {
                EnergyPlus::ShowContinueError(
                        "It could be that the module could not be found, or that there was an error in importing");
            }
            EnergyPlus::ShowFatalError("Python import error causes program termination");
        }
        PyObject *pModuleDict = PyModule_GetDict(pModule);
        Py_DECREF(pModule); // PyImport_Import returns a new reference, decrement it
        if (!pModuleDict) {
            EnergyPlus::ShowSevereError("Failed to read module dictionary from module \"" + moduleName + "\"");
            if (PyErr_Occurred()) {
                PluginInstance::reportPythonError();
            } else {
                EnergyPlus::ShowContinueError("It could be that the module was empty");
            }
            EnergyPlus::ShowFatalError("Python module error causes program termination");
        }
        PyObject *pClass = PyDict_GetItemString(pModuleDict, className.c_str());
        // Py_DECREF(pModuleDict);  // PyModule_GetDict returns a borrowed reference, DO NOT decrement
        if (!pClass) {
            EnergyPlus::ShowSevereError("Failed to get class type \"" + className + "\" from module \"" + moduleName + "\"");
            if (PyErr_Occurred()) {
                PluginInstance::reportPythonError();
            } else {
                EnergyPlus::ShowContinueError("It could be the class name is misspelled or missing.");
            }
            EnergyPlus::ShowFatalError("Python class import error causes program termination");
        }
        if (!PyCallable_Check(pClass)) {
            EnergyPlus::ShowSevereError("Got class type \"" + className + "\", but it cannot be called/instantiated");
            if (PyErr_Occurred()) {
                PluginInstance::reportPythonError();
            } else {
                EnergyPlus::ShowContinueError("Is it possible the class name is actually just a variable?");
            }
            EnergyPlus::ShowFatalError("Python class check error causes program termination");
        }
        PyObject *pClassInstance = PyObject_CallObject(pClass, nullptr);
        // Py_DECREF(pClass);  // PyDict_GetItemString returns a borrowed reference, DO NOT decrement
        if (!pClassInstance) {
            EnergyPlus::ShowSevereError("Something went awry calling class constructor for class \"" + className + "\"");
            if (PyErr_Occurred()) {
                PluginInstance::reportPythonError();
            } else {
                EnergyPlus::ShowContinueError("It is possible the plugin class constructor takes extra arguments - it shouldn't.");
            }
            EnergyPlus::ShowFatalError("Python class constructor error causes program termination");
        }
        // PyObject_CallObject returns a new reference, that we need to manage
        // I think we need to keep it around in memory though so the class methods can be called later on,
        // so I don't intend on decrementing it, at least not until the manager destructor
        // In any case, it will be an **extremely** tiny memory use if we hold onto it a bit too long

        // now grab the function pointers to the main call function
        std::string const mainFunctionName = "main";
        this->pPluginMainFunction = PyObject_GetAttrString(pClassInstance, mainFunctionName.c_str());
        if (!this->pPluginMainFunction || !PyCallable_Check(this->pPluginMainFunction)) {
            EnergyPlus::ShowSevereError("Could not find or call function \"" + mainFunctionName + "\" on class \"" + moduleName + "." + className + "\"");
            if (PyErr_Occurred()) {
                PluginInstance::reportPythonError();
            } else {
                EnergyPlus::ShowContinueError("Make sure the plugin class overrides the main() function, and does not require arguments.");
            }
            EnergyPlus::ShowFatalError("Python main() function error causes program termination");
        }

        // update the rest of the plugin call instance and store it
        this->stringIdentifier = moduleName + "." + className;
        this->emsAlias = emsName;
        this->runDuringWarmup = runPluginDuringWarmup;
    }

    void PluginInstance::run()
    {
        // then call the main function
        PyObject *pFunctionResponse = PyObject_CallFunction(this->pPluginMainFunction, nullptr);
        if (!pFunctionResponse) {
            EnergyPlus::ShowSevereError("Call to " + this->stringIdentifier + " failed!");
            if (PyErr_Occurred()) {
                PluginInstance::reportPythonError();
            } else {
                EnergyPlus::ShowContinueError("This could happen for any number of reasons, check the plugin code.");
            }
            EnergyPlus::ShowFatalError("Program terminates after call to " + this->stringIdentifier + " failed!");
        }
        if (PyLong_Check(pFunctionResponse)) {
            int exitCode = (int)PyLong_AsLong(pFunctionResponse);
            if (exitCode == 0) {
                // success
            } else if (exitCode == 1) {
                EnergyPlus::ShowFatalError("Python Plugin \"" + this->stringIdentifier + "\" returned 1 to indicate EnergyPlus should abort");
            }
        } else {
            EnergyPlus::ShowFatalError("Invalid return from main() on class \"" + this->stringIdentifier +
                                       ", make sure it returns an integer exit code");
        }
        Py_DECREF(pFunctionResponse); // PyObject_CallFunction returns new reference, decrement
    }

    void PluginManager::addToPythonPath(const std::string &path, bool userDefinedPath)
    {
        std::string command = "sys.path.insert(0, \"" + path + "\")";
        if (PyRun_SimpleString(command.c_str()) == 0) {
            if (userDefinedPath) {
                EnergyPlus::ShowMessage("Successfully added path \"" + path + "\" to the sys.path in Python");
            }
            // DEBUGGING PATHS: PyRun_SimpleString("print(' EPS : ' + str(sys.path))");
        } else {
            EnergyPlus::ShowFatalError("ERROR adding \"" + path + "\" to the sys.path in Python");
        }
    }

    void PluginManager::addGlobalVariable(const std::string &name) {
        std::string const varNameUC = EnergyPlus::UtilityRoutines::MakeUPPERCase(name);
        this->globalVariableNames.push_back(varNameUC);
        this->globalVariableValues.push_back(Real64());
    }

    int PluginManager::getGlobalVariableHandle(const std::string& name, bool const suppress_warning) { // note zero is a valid handle
        std::string const varNameUC = EnergyPlus::UtilityRoutines::MakeUPPERCase(name);
        auto const it = std::find(this->globalVariableNames.begin(), this->globalVariableNames.end(), varNameUC);
        if (it != this->globalVariableNames.end()) {
            return std::distance(this->globalVariableNames.begin(), it);
        } else {
            if (suppress_warning) {
                return -1;
            } else {
                EnergyPlus::ShowSevereError("Tried to retrieve handle for a nonexistent plugin global variable");
                EnergyPlus::ShowContinueError("Name looked up: \"" + varNameUC + "\", available names: ");
                for (auto const &gvName : this->globalVariableNames) {
                    EnergyPlus::ShowContinueError("    \"" + gvName + "\"");
                }
                EnergyPlus::ShowFatalError("Plugin global variable problem causes program termination");
                return -1; // hush the compiler warning
            }
        }
    }

    Real64 PluginManager::getGlobalVariableValue(int handle) {
        if (this->globalVariableValues.empty()) {
            EnergyPlus::ShowFatalError("Tried to access plugin global variable but it looks like there aren't any; use the PythonPlugin:GlobalVariables object to declare them.");
        }
        try {
            return this->globalVariableValues[handle];
        } catch (...) {
            EnergyPlus::ShowSevereError("Tried to access plugin global variable value at index " + std::to_string(handle));
            EnergyPlus::ShowContinueError("Available handles range from 0 to " + std::to_string(this->globalVariableValues.size()-1));
            EnergyPlus::ShowFatalError("Plugin global variable problem causes program termination");
        }
        return 0;
    }

    void PluginManager::setGlobalVariableValue(int handle, Real64 value) {
        if (this->globalVariableValues.empty()) {
            EnergyPlus::ShowFatalError("Tried to set plugin global variable but it looks like there aren't any; use the PythonPlugin:GlobalVariables object to declare them.");
        }
        try {
            this->globalVariableValues[handle] = value;
        } catch (...) {
            EnergyPlus::ShowSevereError("Tried to set plugin global variable value at index " + std::to_string(handle));
            EnergyPlus::ShowContinueError("Available handles range from 0 to " + std::to_string(this->globalVariableValues.size()-1));
            EnergyPlus::ShowFatalError("Plugin global variable problem causes program termination");
        }
    }

    int PluginManager::calledFromFromString(std::string const &calledFrom) {
        if (calledFrom == "BEGINNEWENVIRONMENT") {
            return DataGlobals::emsCallFromBeginNewEvironment;
        } else if (calledFrom == "AFTERNEWENVIRONMENTWARMUPISCOMPLETE") {
            return DataGlobals::emsCallFromBeginNewEvironmentAfterWarmUp;
        } else if (calledFrom == "BEGINZONETIMESTEPBEFOREINITHEATBALANCE") {
            return DataGlobals::emsCallFromBeginZoneTimestepBeforeInitHeatBalance;
        } else if (calledFrom == "BEGINZONETIMESTEPAFTERINITHEATBALANCE") {
            return DataGlobals::emsCallFromBeginZoneTimestepAfterInitHeatBalance;
        } else if (calledFrom == "BEGINTIMESTEPBEFOREPREDICTOR") {
            return DataGlobals::emsCallFromBeginTimestepBeforePredictor;
        } else if (calledFrom == "AFTERPREDICTORBEFOREHVACMANAGERS") {
            return DataGlobals::emsCallFromBeforeHVACManagers;
        } else if (calledFrom == "AFTERPREDICTORAFTERHVACMANAGERS") {
            return DataGlobals::emsCallFromAfterHVACManagers;
        } else if (calledFrom == "INSIDEHVACSYSTEMITERATIONLOOP") {
            return DataGlobals::emsCallFromHVACIterationLoop;
        } else if (calledFrom == "ENDOFZONETIMESTEPBEFOREZONEREPORTING") {
            return DataGlobals::emsCallFromEndZoneTimestepBeforeZoneReporting;
        } else if (calledFrom == "ENDOFZONETIMESTEPAFTERZONEREPORTING") {
            return DataGlobals::emsCallFromEndZoneTimestepAfterZoneReporting;
        } else if (calledFrom == "ENDOFSYSTEMTIMESTEPBEFOREHVACREPORTING") {
            return DataGlobals::emsCallFromEndSystemTimestepBeforeHVACReporting;
        } else if (calledFrom == "ENDOFSYSTEMTIMESTEPAFTERHVACREPORTING") {
            return DataGlobals::emsCallFromEndSystemTimestepAfterHVACReporting;
        } else if (calledFrom == "ENDOFZONESIZING") {
            return DataGlobals::emsCallFromZoneSizing;
        } else if (calledFrom == "ENDOFSYSTEMSIZING") {
            return DataGlobals::emsCallFromSystemSizing;
        } else if (calledFrom == "AFTERCOMPONENTINPUTREADIN") {
            return DataGlobals::emsCallFromComponentGetInput;
        } else if (calledFrom == "USERDEFINEDCOMPONENTMODEL") {
            return DataGlobals::emsCallFromUserDefinedComponentModel;
        } else if (calledFrom == "UNITARYSYSTEMSIZING") {
            return DataGlobals::emsCallFromUnitarySystemSizing;
        } else {
            EnergyPlus::ShowFatalError("Invalid calledFrom string passed to calledFromFromString");
            return -1;
        }
    }

} // namespace PluginManager
} // namespace EnergyPlus
