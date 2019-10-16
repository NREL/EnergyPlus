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
#include <EnergyPlus/FileSystem.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
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

    std::vector<void (*)()> callbacksCallFromEndOfHour;
    std::vector<void (*)()> callbacksCallFromBeginningOfHour;
    std::vector<void (*)()> callbacksCallFromBeginningOfZoneTimeStep;
    std::vector<void (*)()> callbacksCallFromEndOfZoneTimeStep;

    std::vector<PluginInstance> pluginsCallFromEndOfHour;
    std::vector<PluginInstance> pluginsCallFromBeginningOfHour;
    std::vector<PluginInstance> pluginsCallFromBeginningOfZoneTimeStep;
    std::vector<PluginInstance> pluginsCallFromEndOfZoneTimeStep;

    void registerNewCallback(EnergyPlus::PluginManager::PluginCallingPoints iCalledFrom, void (*f)())
    {
        if (iCalledFrom == EnergyPlus::PluginManager::PluginCallingPoints::EndOfHour) {
            callbacksCallFromEndOfHour.push_back(f);
        } else if (iCalledFrom == EnergyPlus::PluginManager::PluginCallingPoints::BeginningOfHour) {
            callbacksCallFromBeginningOfHour.push_back(f);
        } else if (iCalledFrom == EnergyPlus::PluginManager::PluginCallingPoints::BeginningOfZoneTimeStep) {
            callbacksCallFromBeginningOfZoneTimeStep.push_back(f);
        } else if (iCalledFrom == EnergyPlus::PluginManager::PluginCallingPoints::EndOfZoneTimeStep) {
            callbacksCallFromEndOfZoneTimeStep.push_back(f);
        }
    }

    void runAnyRegisteredCallbacks(PluginCallingPoints iCalledFrom)
    {
        if (iCalledFrom == EnergyPlus::PluginManager::PluginCallingPoints::EndOfHour) {
            for (auto const &cb : callbacksCallFromEndOfHour) {
                cb();
            }
            for (auto &plugin : pluginsCallFromEndOfHour) {
                plugin.run();
            }
        } else if (iCalledFrom == EnergyPlus::PluginManager::PluginCallingPoints::BeginningOfHour) {
            for (auto const &cb : callbacksCallFromBeginningOfHour) {
                cb();
            }
            for (auto &plugin : pluginsCallFromBeginningOfHour) {
                plugin.run();
            }
        } else if (iCalledFrom == EnergyPlus::PluginManager::PluginCallingPoints::BeginningOfZoneTimeStep) {
            for (auto const &cb : callbacksCallFromBeginningOfZoneTimeStep) {
                cb();
            }
            for (auto &plugin : pluginsCallFromBeginningOfZoneTimeStep) {
                plugin.run();
            }
        } else if (iCalledFrom == EnergyPlus::PluginManager::PluginCallingPoints::EndOfZoneTimeStep) {
            for (auto const &cb : callbacksCallFromEndOfZoneTimeStep) {
                cb();
            }
            for (auto &plugin : pluginsCallFromEndOfZoneTimeStep) {
                plugin.run();
            }
        }
    }

    void clear_state()
    {
        callbacksCallFromEndOfHour.clear();
        callbacksCallFromBeginningOfHour.clear();
        callbacksCallFromBeginningOfZoneTimeStep.clear();
        callbacksCallFromEndOfZoneTimeStep.clear();
        pluginsCallFromEndOfHour.clear();
        pluginsCallFromBeginningOfHour.clear();
        pluginsCallFromBeginningOfZoneTimeStep.clear();
        pluginsCallFromEndOfZoneTimeStep.clear();
        pluginManager.reset(); // delete the current plugin manager instance, which was created in simulation manager, this clean up Python
    }

    // Python Plugin stuff here

    PluginManager::PluginManager()
    {
        // from https://docs.python.org/3/c-api/init.html
        // If arg 0, it skips init registration of signal handlers, which might be useful when Python is embedded.
        Py_InitializeEx(0);
        PyRun_SimpleString("import sys"); // allows us to report sys.path later
        // we need to figure out the right way to find the path to the binary itself
        // we want to point Python to a folder containing a pyms folder in it
        // for the case of this prototype, it's the root of the repo
        // for the case of an E+ distribution, it will be probably the root of the E+ install
        // I think this is already available from the command line interface work
        PluginManager::addToPythonPath(".");

        // so I think we'll want to add to the front of path, in this order:
        // - the program executable parent directory, so that it could find the installed pyms,
        // - the current working directory, so if they have pyms installed locally it will find that one first
        // - any additional paths they ask for in the input file
        std::string programPath = FileSystem::getProgramPath();
        std::string programDir = FileSystem::getParentDirectoryPath(programPath);
        std::string sanitizedDir = PluginManager::sanitizedPath(programDir);
        PluginManager::addToPythonPath(sanitizedDir);

        // Read all the additional search paths next
        std::string const sPaths = "PythonPlugin:SearchPaths";
        bool errorsFound = false;
        int searchPaths = inputProcessor->getNumObjectsFound(sPaths);
        if (searchPaths > 0) {
            auto const instances = inputProcessor->epJSON.find(sPaths);
            if (instances == inputProcessor->epJSON.end()) {
                ShowSevereError(                                                                             // LCOV_EXCL_LINE
                    "PythonPlugin:SearchPaths: Somehow getNumObjectsFound was > 0 but epJSON.find found 0"); // LCOV_EXCL_LINE
                errorsFound = true;                                                                          // LCOV_EXCL_LINE
            }
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                // This is a unique object, so we should have one, but this is fine
                auto const &fields = instance.value();
                auto const &thisObjectName = instance.key();
                inputProcessor->markObjectAsUsed(sPaths, thisObjectName);
                if (fields.find("search_path_1") != fields.end()) {
                    PluginManager::addToPythonPath(fields.at("search_path_1"));
                }
                if (fields.find("search_path_2") != fields.end()) {
                    PluginManager::addToPythonPath(fields.at("search_path_2"));
                }
                if (fields.find("search_path_3") != fields.end()) {
                    PluginManager::addToPythonPath(fields.at("search_path_3"));
                }
                if (fields.find("search_path_4") != fields.end()) {
                    PluginManager::addToPythonPath(fields.at("search_path_4"));
                }
                if (fields.find("search_path_5") != fields.end()) {
                    PluginManager::addToPythonPath(fields.at("search_path_5"));
                }
            }
        }

        // Now read all the actual plugins and interpret them
        std::string const sPlugins = "PythonPlugin:Instance";
        int plugins = inputProcessor->getNumObjectsFound(sPaths);
        if (plugins > 0) {
            auto const instances = inputProcessor->epJSON.find(sPlugins);
            if (instances == inputProcessor->epJSON.end()) {
                ShowSevereError(                                                                          // LCOV_EXCL_LINE
                    "PythonPlugin:Instance: Somehow getNumObjectsFound was > 0 but epJSON.find found 0"); // LCOV_EXCL_LINE
                errorsFound = true;                                                                       // LCOV_EXCL_LINE
            }
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                // This is a unique object, so we should have one, but this is fine
                auto const &fields = instance.value();
                auto const &thisObjectName = instance.key();
                inputProcessor->markObjectAsUsed(sPlugins, thisObjectName);
                std::string fileName = fields.at("python_file_name");
                std::string className = fields.at("plugin_class_name");
                std::string sCallingPoint = EnergyPlus::UtilityRoutines::MakeUPPERCase(fields.at("calling_point"));
                if (sCallingPoint == "BEGINNINGOFHOUR") {
                    pluginsCallFromBeginningOfHour.emplace_back(fileName, className);
                } else if (sCallingPoint == "BEGINNINGOFZONETIMESTEP") {
                    pluginsCallFromBeginningOfZoneTimeStep.emplace_back(fileName, className);
                } else if (sCallingPoint == "ENDOFZONETIMESTEP") {
                    pluginsCallFromEndOfZoneTimeStep.emplace_back(fileName, className);
                } else if (sCallingPoint == "ENDOFHOUR") {
                    pluginsCallFromEndOfHour.emplace_back(fileName, className);
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

    PluginInstance::PluginInstance(const std::string &moduleName, const std::string &className)
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
            EnergyPlus::ShowFatalError("Failed to import module \"" + moduleName + "\"");
        }
        PyObject *pModuleDict = PyModule_GetDict(pModule);
        Py_DECREF(pModule); // PyImport_Import returns a new reference, decrement it
        if (!pModuleDict) {
            EnergyPlus::ShowFatalError("Failed to read module dictionary from module \"" + moduleName + "\"");
        }
        PyObject *pClass = PyDict_GetItemString(pModuleDict, className.c_str());
        // Py_DECREF(pModuleDict);  // PyModule_GetDict returns a borrowed reference, DO NOT decrement
        if (!pClass) {
            EnergyPlus::ShowFatalError("Failed to get class type \"" + className + "\" from module \"" + moduleName + "\"");
        }
        if (!PyCallable_Check(pClass)) {
            EnergyPlus::ShowFatalError("Got class type \"" + className + "\", but it cannot be called/instantiated");
        }
        PyObject *pClassInstance = PyObject_CallObject(pClass, nullptr);
        // Py_DECREF(pClass);  // PyDict_GetItemString returns a borrowed reference, DO NOT decrement
        if (!pClassInstance) {
            EnergyPlus::ShowFatalError("Something went awry calling class constructor for class \"" + className + "\"");
        }
        // PyObject_CallObject returns a new reference, that we need to manage
        // I think we need to keep it around in memory though so the class methods can be called later on,
        // so I don't intend on decrementing it, at least not until the manager destructor
        // In any case, it will be an **extremely** tiny memory use if we hold onto it a bit too long

        // now grab the function pointers to the main call function
        std::string const mainFunctionName = "main";
        PyObject *pPluginMainFunction = PyObject_GetAttrString(pClassInstance, mainFunctionName.c_str());
        if (!pPluginMainFunction || !PyCallable_Check(pPluginMainFunction)) {
            if (PyErr_Occurred()) {
                PyErr_Print();
            }
            EnergyPlus::ShowFatalError("Could not find function \"" + mainFunctionName + "\" on class \"" + moduleName + "." + className + "\"");
        }

        // update the rest of the plugin call instance and store it
        this->stringIdentifier = moduleName + "." + className;
        this->pPluginMainFunction = pPluginMainFunction;
    }

    void PluginInstance::run()
    {
        // then call the main function
        PyObject *pFunctionResponse = PyObject_CallFunction(this->pPluginMainFunction, nullptr);
        if (!pFunctionResponse) {
            PyErr_Print();
            EnergyPlus::ShowFatalError("Call to " + this->stringIdentifier + " failed!");
        }
        if (PyLong_Check(pFunctionResponse)) {
            int exitCode = (int)PyLong_AsLong(pFunctionResponse);
            if (exitCode == 0) {
                // success
            } else if (exitCode == 1) {
                // we should FATAL ERROR HERE
            }
        } else {
            EnergyPlus::ShowFatalError("Invalid return from main() on class \"" + this->stringIdentifier +
                                       ", make sure it returns an integer exit code");
        }
        Py_DECREF(pFunctionResponse); // PyObject_CallFunction returns new reference, decrement
    }

    void PluginManager::addToPythonPath(const std::string &path)
    {
        std::string command = "sys.path.insert(0, \"" + path + "\")";
        if (PyRun_SimpleString(command.c_str()) == 0) {
            EnergyPlus::ShowMessage("Successfully added path \"" + path + "\" to the sys.path in Python");
            PyRun_SimpleString("print(' EPS : ' + str(sys.path))");
        } else {
            EnergyPlus::ShowFatalError("ERROR adding \"" + path + "\" to the sys.path in Python");
        }
    }

} // namespace PluginManager
} // namespace EnergyPlus
