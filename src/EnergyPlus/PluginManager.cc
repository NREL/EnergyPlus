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

#ifdef _WIN32
#include <windows.h>
#else
#include <dlfcn.h>
#endif

#include <map>

#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataStringGlobals.hh>
#include <EnergyPlus/FileSystem.hh>
#include <EnergyPlus/InputProcessing/InputProcessor.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/PluginManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>

#include <nlohmann/json.hpp>

namespace EnergyPlus {
namespace PluginManagement {
    std::unique_ptr<PluginManager> pluginManager;

    std::map<int, std::vector<std::function<void(void *)>>> callbacks;
    std::vector<PluginInstance> plugins;
    std::vector<PluginTrendVariable> trends;
    std::vector<std::string> globalVariableNames;
    std::vector<Real64> globalVariableValues;

    // some flags
    bool fullyReady = false;
    bool apiErrorFlag = false;

    // function pointers into the PythonWrapper DLL
    void *wrapperDLLHandle = nullptr;
    void (*EP_Py_SetPath)(wchar_t *) = nullptr;
    char *(*EP_Py_GetVersion)() = nullptr;
    wchar_t *(*EP_Py_DecodeLocale)(const char *, unsigned long *) = nullptr;
    void (*EP_Py_InitializeEx)(int) = nullptr;
    int (*EP_PyRun_SimpleString)(const char *) = nullptr;
    int (*EP_Py_FinalizeEx)() = nullptr;
    void (*EP_PyErr_Fetch)(PyObjectWrap **, PyObjectWrap **, PyObjectWrap **) = nullptr;
    void (*EP_PyErr_NormalizeException)(PyObjectWrap **, PyObjectWrap **, PyObjectWrap **) = nullptr;
    PyObjectWrap (*EP_PyObject_Repr)(PyObjectWrap) = nullptr;
    PyObjectWrap (*EP_PyUnicode_AsEncodedString)(PyObjectWrap, const char *, const char *) = nullptr;
    char *(*EP_PyBytes_AsString)(PyObjectWrap) = nullptr;
    PyObjectWrap (*EP_PyUnicode_DecodeFSDefault)(const char *) = nullptr;
    PyObjectWrap (*EP_PyImport_Import)(PyObjectWrap) = nullptr;
    void (*EP_Py_DECREF)(PyObjectWrap) = nullptr;
    PyObjectWrap (*EP_PyErr_Occurred)() = nullptr;
    PyObjectWrap (*EP_PyModule_GetDict)(PyObjectWrap) = nullptr;
    PyObjectWrap (*EP_PyDict_GetItemString)(PyObjectWrap, const char *) = nullptr;
    const char *(*EP_PyUnicode_AsUTF8)(PyObjectWrap) = nullptr;
    PyObjectWrap (*EP_PyUnicode_AsUTF8String)(PyObjectWrap) = nullptr;
    int (*EP_PyCallable_Check)(PyObjectWrap) = nullptr;
    PyObjectWrap (*EP_PyObject_CallObject)(PyObjectWrap, PyObjectWrap) = nullptr;
    PyObjectWrap (*EP_PyObject_GetAttrString)(PyObjectWrap, const char *) = nullptr;
    PyObjectWrap (*EP_PyObject_CallFunction)(PyObjectWrap, const char *) = nullptr;
    PyObjectWrap (*EP_PyObject_CallFunction3Args)(PyObjectWrap, const char *, PyObjectWrap, PyObjectWrap, PyObjectWrap) = nullptr;
    PyObjectWrap (*EP_PyObject_CallMethod)(PyObjectWrap, const char *, const char *) = nullptr;
    PyObjectWrap (*EP_PyObject_CallMethod1Arg)(PyObjectWrap, const char *, const char *, PyObjectWrap) = nullptr;
    PyObjectWrap (*EP_PyObject_CallMethod2ObjArg)(PyObjectWrap, PyObjectWrap, PyObjectWrap, PyObjectWrap) = nullptr;
    int (*EP_PyList_Check)(PyObjectWrap) = nullptr;
    unsigned long (*EP_PyList_Size)(PyObjectWrap) = nullptr;
    PyObjectWrap (*EP_PyList_GetItem)(PyObjectWrap, size_t) = nullptr;
    int (*EP_PyUnicode_Check)(PyObjectWrap) = nullptr;
    int (*EP_PyLong_Check)(PyObjectWrap) = nullptr;
    long (*EP_PyLong_AsLong)(PyObjectWrap) = nullptr;
    void (*EP_Py_SetPythonHome)(const wchar_t *) = nullptr;
    PyObjectWrap (*EP_Py_BuildValue)(const char *) = nullptr;
    PyObjectWrap (*EP_PyLong_FromVoidPtr)(void *) = nullptr;
    PyObjectWrap (*EP_PyString_FromString)(const char *) = nullptr;

    void checkWrapperDLLFunction(const char *err, std::string const &pyFuncName, bool &errFlag)
    {
        if (err) {
            std::string c = "Problem processing function \"" + pyFuncName + "\" in Python wrapper shared library";
            ShowSevereError(c);
            ShowContinueError(err);
            errFlag = true;
        }
    }

    void loadWrapperDLL()
    {
#if LINK_WITH_PYTHON == 1
        // There are two ways to call E+: as an executable and as a library
        // When calling E+ as a library, the plugin system is not available
        // So when calling for plugins, the user must be calling as energyplus.exe
        // In this case, the current executable path will be the E+ install root
        // Which is where we find the Python Wrapper Shared Library
        if (wrapperDLLHandle) return; // already found
        std::string programPath = FileSystem::getAbsolutePath(FileSystem::getProgramPath());
        std::string programDir = FileSystem::getParentDirectoryPath(programPath);
        std::string sanitizedProgramDir = PluginManager::sanitizedPath(programDir);
        // set the path to the python library
#ifdef _WIN32
        std::string pythonLibPath = programDir + DataStringGlobals::pathChar + "pythonwrapper.dll";
#elif __linux__
        std::string pythonLibPath = programDir + DataStringGlobals::pathChar + "libpythonwrapper.so";
#else // Apple
        std::string pythonLibPath = programDir + DataStringGlobals::pathChar + "libpythonwrapper.dylib";
#endif
        std::string pythonWrapper = PluginManager::sanitizedPath(pythonLibPath);
        bool dll_errors = false;
        char *err = nullptr;
        // now actually open the library and assign the functions
#ifdef _WIN32
        wrapperDLLHandle = (void*)LoadLibrary(pythonWrapper.c_str());
        if (!wrapperDLLHandle) {
            ShowFatalError("Could not load python library at: " + pythonWrapper);
        }
        *(void **)(&EP_Py_SetPath) = (void *)GetProcAddress((HINSTANCE)wrapperDLLHandle, "EP_Wrap_Py_SetPath");
        if (!EP_Py_SetPath) {
            ShowSevereError("Could not load Python function: EP_Wrap_Py_SetPath");
            dll_errors = true;
        }
        *(void **)(&EP_Py_GetVersion) = (void *)GetProcAddress((HINSTANCE)wrapperDLLHandle, "EP_Wrap_Py_GetVersion");
        if (!EP_Py_GetVersion) {
            ShowSevereError("Could not load Python function: EP_Wrap_Py_GetVersion");
            dll_errors = true;
        }
        *(void **)(&EP_Py_DecodeLocale) = (void *)GetProcAddress((HINSTANCE)wrapperDLLHandle, "EP_Wrap_Py_DecodeLocale");
        if (!EP_Py_DecodeLocale) {
            ShowSevereError("Could not load Python function: EP_Wrap_Py_DecodeLocale");
            dll_errors = true;
        }
        *(void **)(&EP_Py_InitializeEx) = (void *)GetProcAddress((HINSTANCE)wrapperDLLHandle, "EP_Wrap_Py_InitializeEx");
        if (!EP_Py_InitializeEx) {
            ShowSevereError("Could not load Python function: EP_Wrap_Py_InitializeEx");
            dll_errors = true;
        }
        *(void **)(&EP_Py_FinalizeEx) = (void *)GetProcAddress((HINSTANCE)wrapperDLLHandle, "EP_Wrap_Py_FinalizeEx");
        if (!EP_Py_FinalizeEx) {
            ShowSevereError("Could not load Python function: EP_Wrap_Py_FinalizeEx");
            dll_errors = true;
        }
        *(void **)(&EP_PyRun_SimpleString) = (void *)GetProcAddress((HINSTANCE)wrapperDLLHandle, "EP_Wrap_PyRun_SimpleString");
        if (!EP_PyRun_SimpleString) {
            ShowSevereError("Could not load Python function: EP_Wrap_PyRun_SimpleString");
            dll_errors = true;
        }
        *(void **)(&EP_PyErr_Fetch) = (void *)GetProcAddress((HINSTANCE)wrapperDLLHandle, "EP_Wrap_PyErr_Fetch");
        if (!EP_PyErr_Fetch) {
            ShowSevereError("Could not load Python function: EP_Wrap_PyErr_Fetch");
            dll_errors = true;
        }
        *(void **)(&EP_PyErr_NormalizeException) = (void *)GetProcAddress((HINSTANCE)wrapperDLLHandle, "EP_Wrap_PyErr_NormalizeException");
        if (!EP_PyErr_NormalizeException) {
            ShowSevereError("Could not load Python function: EP_Wrap_PyErr_NormalizeException");
            dll_errors = true;
        }
        *(void **)(&EP_PyObject_Repr) = (void *)GetProcAddress((HINSTANCE)wrapperDLLHandle, "EP_Wrap_PyObject_Repr");
        if (!EP_PyObject_Repr) {
            ShowSevereError("Could not load Python function: EP_Wrap_PyObject_Repr");
            dll_errors = true;
        }
        *(void **)(&EP_PyUnicode_AsEncodedString) = (void *)GetProcAddress((HINSTANCE)wrapperDLLHandle, "EP_Wrap_PyUnicode_AsEncodedString");
        if (!EP_PyUnicode_AsEncodedString) {
            ShowSevereError("Could not load Python function: EP_Wrap_PyUnicode_AsEncodedString");
            dll_errors = true;
        }
        *(void **)(&EP_PyBytes_AsString) = (void *)GetProcAddress((HINSTANCE)wrapperDLLHandle, "EP_Wrap_PyBytes_AsString");
        if (!EP_PyBytes_AsString) {
            ShowSevereError("Could not load Python function: EP_Wrap_PyBytes_AsString");
            dll_errors = true;
        }
        *(void **)(&EP_PyUnicode_DecodeFSDefault) = (void *)GetProcAddress((HINSTANCE)wrapperDLLHandle, "EP_Wrap_PyUnicode_DecodeFSDefault");
        if (!EP_PyUnicode_DecodeFSDefault) {
            ShowSevereError("Could not load Python function: EP_Wrap_PyUnicode_DecodeFSDefault");
            dll_errors = true;
        }
        *(void **)(&EP_PyImport_Import) = (void *)GetProcAddress((HINSTANCE)wrapperDLLHandle, "EP_Wrap_PyImport_Import");
        if (!EP_PyImport_Import) {
            ShowSevereError("Could not load Python function: EP_Wrap_PyImport_Import");
            dll_errors = true;
        }
        *(void **)(&EP_Py_DECREF) = (void *)GetProcAddress((HINSTANCE)wrapperDLLHandle, "EP_Wrap_Py_DECREF");
        if (!EP_Py_DECREF) {
            ShowSevereError("Could not load Python function: EP_Wrap_Py_DECREF");
            dll_errors = true;
        }
        *(void **)(&EP_PyErr_Occurred) = (void *)GetProcAddress((HINSTANCE)wrapperDLLHandle, "EP_Wrap_PyErr_Occurred");
        if (!EP_PyErr_Occurred) {
            ShowSevereError("Could not load Python function: EP_Wrap_PyErr_Occurred");
            dll_errors = true;
        }
        *(void **)(&EP_PyModule_GetDict) = (void *)GetProcAddress((HINSTANCE)wrapperDLLHandle, "EP_Wrap_PyModule_GetDict");
        if (!EP_PyModule_GetDict) {
            ShowSevereError("Could not load Python function: EP_Wrap_PyModule_GetDict");
            dll_errors = true;
        }
        *(void **)(&EP_PyDict_GetItemString) = (void *)GetProcAddress((HINSTANCE)wrapperDLLHandle, "EP_Wrap_PyDict_GetItemString");
        if (!EP_PyDict_GetItemString) {
            ShowSevereError("Could not load Python function: EP_Wrap_PyDict_GetItemString");
            dll_errors = true;
        }
        *(void **)(&EP_PyUnicode_AsUTF8) = (void *)GetProcAddress((HINSTANCE)wrapperDLLHandle, "EP_Wrap_PyUnicode_AsUTF8");
        if (!EP_PyUnicode_AsUTF8) {
            ShowSevereError("Could not load Python function: EP_Wrap_PyUnicode_AsUTF8");
            dll_errors = true;
        }
        *(void **)(&EP_PyUnicode_AsUTF8String) = (void *)GetProcAddress((HINSTANCE)wrapperDLLHandle, "EP_Wrap_PyUnicode_AsUTF8String");
        if (!EP_PyUnicode_AsUTF8String) {
            ShowSevereError("Could not load Python function: EP_Wrap_PyUnicode_AsUTF8String");
            dll_errors = true;
        }
        *(void **)(&EP_PyCallable_Check) = (void *)GetProcAddress((HINSTANCE)wrapperDLLHandle, "EP_Wrap_PyCallable_Check");
        if (!EP_PyCallable_Check) {
            ShowSevereError("Could not load Python function: EP_Wrap_PyCallable_Check");
            dll_errors = true;
        }
        *(void **)(&EP_PyObject_CallObject) = (void *)GetProcAddress((HINSTANCE)wrapperDLLHandle, "EP_Wrap_PyObject_CallObject");
        if (!EP_PyObject_CallObject) {
            ShowSevereError("Could not load Python function: EP_Wrap_PyObject_CallObject");
            dll_errors = true;
        }
        *(void **)(&EP_PyObject_GetAttrString) = (void *)GetProcAddress((HINSTANCE)wrapperDLLHandle, "EP_Wrap_PyObject_GetAttrString");
        if (!EP_PyObject_GetAttrString) {
            ShowSevereError("Could not load Python function: EP_Wrap_PyObject_GetAttrString");
            dll_errors = true;
        }
        *(void **)(&EP_PyObject_CallFunction) = (void *)GetProcAddress((HINSTANCE)wrapperDLLHandle, "EP_Wrap_PyObject_CallFunction");
        if (!EP_PyObject_CallFunction) {
            ShowSevereError("Could not load Python function: EP_Wrap_PyObject_CallFunction");
            dll_errors = true;
        }
        *(void **)(&EP_PyObject_CallFunction3Args) = (void *)GetProcAddress((HINSTANCE)wrapperDLLHandle, "EP_Wrap_PyObject_CallFunction3Args");
        if (!EP_PyObject_CallFunction3Args) {
            ShowSevereError("Could not load Python function: EP_Wrap_PyObject_CallFunction3Args");
            dll_errors = true;
        }
        *(void **)(&EP_PyObject_CallMethod) = (void *)GetProcAddress((HINSTANCE)wrapperDLLHandle, "EP_Wrap_PyObject_CallMethod");
        if (!EP_PyObject_CallMethod) {
            ShowSevereError("Could not load Python function: EP_Wrap_PyObject_CallMethod");
            dll_errors = true;
        }
        *(void **)(&EP_PyObject_CallMethod1Arg) = (void *)GetProcAddress((HINSTANCE)wrapperDLLHandle, "EP_Wrap_PyObject_CallMethod1Arg");
        if (!EP_PyObject_CallMethod1Arg) {
            ShowSevereError("Could not load Python function: EP_Wrap_PyObject_CallMethod1Arg");
            dll_errors = true;
        }
        *(void **)(&EP_PyObject_CallMethod2ObjArg) = (void *)GetProcAddress((HINSTANCE)wrapperDLLHandle, "EP_Wrap_PyObject_CallMethod2ObjArg");
        if (!EP_PyObject_CallMethod2ObjArg) {
            ShowSevereError("Could not load Python function: EP_Wrap_PyObject_CallMethod2ObjArg");
            dll_errors = true;
        }
        *(void **)(&EP_PyList_Check) = (void *)GetProcAddress((HINSTANCE)wrapperDLLHandle, "EP_Wrap_PyList_Check");
        if (!EP_PyList_Check) {
            ShowSevereError("Could not load Python function: EP_Wrap_PyList_Check");
            dll_errors = true;
        }
        *(void **)(&EP_PyList_Size) = (void *)GetProcAddress((HINSTANCE)wrapperDLLHandle, "EP_Wrap_PyList_Size");
        if (!EP_PyList_Size) {
            ShowSevereError("Could not load Python function: EP_Wrap_PyList_Size");
            dll_errors = true;
        }
        *(void **)(&EP_PyList_GetItem) = (void *)GetProcAddress((HINSTANCE)wrapperDLLHandle, "EP_Wrap_PyList_GetItem");
        if (!EP_PyList_GetItem) {
            ShowSevereError("Could not load Python function: EP_Wrap_PyList_GetItem");
            dll_errors = true;
        }
        *(void **)(&EP_PyUnicode_Check) = (void *)GetProcAddress((HINSTANCE)wrapperDLLHandle, "EP_Wrap_PyUnicode_Check");
        if (!EP_PyUnicode_Check) {
            ShowSevereError("Could not load Python function: EP_Wrap_PyUnicode_Check");
            dll_errors = true;
        }
        *(void **)(&EP_PyLong_Check) = (void *)GetProcAddress((HINSTANCE)wrapperDLLHandle, "EP_Wrap_PyLong_Check");
        if (!EP_PyLong_Check) {
            ShowSevereError("Could not load Python function: EP_Wrap_PyLong_Check");
            dll_errors = true;
        }
        *(void **)(&EP_PyLong_AsLong) = (void *)GetProcAddress((HINSTANCE)wrapperDLLHandle, "EP_Wrap_PyLong_AsLong");
        if (!EP_PyLong_AsLong) {
            ShowSevereError("Could not load Python function: EP_Wrap_PyLong_AsLong");
            dll_errors = true;
        }
        *(void **)(&EP_Py_SetPythonHome) = (void *)GetProcAddress((HINSTANCE)wrapperDLLHandle, "EP_Wrap_Py_SetPythonHome");
        if (!EP_Py_SetPythonHome) {
            ShowSevereError("Could not load Python function: EP_Wrap_Py_SetPythonHome");
            dll_errors = true;
        }
        *(void **)(&EP_Py_BuildValue) = (void *)GetProcAddress((HINSTANCE)wrapperDLLHandle, "EP_Wrap_Py_BuildValue");
        if (!EP_Py_BuildValue) {
            ShowSevereError("Could not load Python function: EP_Wrap_Py_BuildValue");
            dll_errors = true;
        }
        *(void **)(&EP_PyLong_FromVoidPtr) = (void *)GetProcAddress((HINSTANCE)wrapperDLLHandle, "EP_Wrap_PyLong_FromVoidPtr");
        if (!EP_PyLong_FromVoidPtr) {
            ShowSevereError("Could not load Python function: EP_Wrap_PyLong_FromVoidPtr");
            dll_errors = true;
        }
        *(void **)(&EP_PyString_FromString) = (void *)GetProcAddress((HINSTANCE)wrapperDLLHandle, "EP_Wrap_PyString_FromString");
        if (!EP_PyString_FromString) {
            ShowSevereError("Could not load Python function: EP_Wrap_PyString_FromString");
            dll_errors = true;
        }
#else
        wrapperDLLHandle = dlopen(pythonWrapper.c_str(), RTLD_LAZY | RTLD_GLOBAL);
        if (!wrapperDLLHandle) {
            err = dlerror();
            ShowSevereError("Could not load Python library at: " + pythonWrapper);
            ShowContinueError(err);
            ShowFatalError("DLL problem cause program termination");
        }
        *(void **)(&EP_Py_SetPath) = dlsym(wrapperDLLHandle, "EP_Wrap_Py_SetPath");
        err = dlerror();
        checkWrapperDLLFunction(err, "EP_Wrap_Py_SetPath", dll_errors);
        *(void **)(&EP_Py_GetVersion) = dlsym(wrapperDLLHandle, "EP_Wrap_Py_GetVersion");
        err = dlerror();
        checkWrapperDLLFunction(err, "EP_Wrap_Py_GetVersion", dll_errors);
        *(void **)(&EP_Py_DecodeLocale) = dlsym(wrapperDLLHandle, "EP_Wrap_Py_DecodeLocale");
        err = dlerror();
        checkWrapperDLLFunction(err, "EP_Wrap_Py_DecodeLocale", dll_errors);
        *(void **)(&EP_Py_InitializeEx) = dlsym(wrapperDLLHandle, "EP_Wrap_Py_InitializeEx");
        err = dlerror();
        checkWrapperDLLFunction(err, "EP_Wrap_Py_InitializeEx", dll_errors);
        *(void **)(&EP_Py_FinalizeEx) = dlsym(wrapperDLLHandle, "EP_Wrap_Py_FinalizeEx");
        err = dlerror();
        checkWrapperDLLFunction(err, "EP_Wrap_Py_FinalizeEx", dll_errors);
        *(void **)(&EP_PyRun_SimpleString) = dlsym(wrapperDLLHandle, "EP_Wrap_PyRun_SimpleString");
        err = dlerror();
        checkWrapperDLLFunction(err, "EP_Wrap_PyRun_SimpleString", dll_errors);
        *(void **)(&EP_PyErr_Fetch) = dlsym(wrapperDLLHandle, "EP_Wrap_PyErr_Fetch");
        err = dlerror();
        checkWrapperDLLFunction(err, "EP_Wrap_PyErr_Fetch", dll_errors);
        *(void **)(&EP_PyErr_NormalizeException) = dlsym(wrapperDLLHandle, "EP_Wrap_PyErr_NormalizeException");
        err = dlerror();
        checkWrapperDLLFunction(err, "EP_Wrap_PyErr_NormalizeException", dll_errors);
        *(void **)(&EP_PyObject_Repr) = dlsym(wrapperDLLHandle, "EP_Wrap_PyObject_Repr");
        err = dlerror();
        checkWrapperDLLFunction(err, "EP_Wrap_PyObject_Repr", dll_errors);
        *(void **)(&EP_PyUnicode_AsEncodedString) = dlsym(wrapperDLLHandle, "EP_Wrap_PyUnicode_AsEncodedString");
        err = dlerror();
        checkWrapperDLLFunction(err, "EP_Wrap_PyUnicode_AsEncodedString", dll_errors);
        *(void **)(&EP_PyBytes_AsString) = dlsym(wrapperDLLHandle, "EP_Wrap_PyBytes_AsString");
        err = dlerror();
        checkWrapperDLLFunction(err, "EP_Wrap_PyBytes_AsString", dll_errors);
        *(void **)(&EP_PyUnicode_DecodeFSDefault) = dlsym(wrapperDLLHandle, "EP_Wrap_PyUnicode_DecodeFSDefault");
        err = dlerror();
        checkWrapperDLLFunction(err, "EP_Wrap_PyUnicode_DecodeFSDefault", dll_errors);
        *(void **)(&EP_PyImport_Import) = dlsym(wrapperDLLHandle, "EP_Wrap_PyImport_Import");
        err = dlerror();
        checkWrapperDLLFunction(err, "EP_Wrap_PyImport_Import", dll_errors);
        *(void **)(&EP_Py_DECREF) = dlsym(wrapperDLLHandle, "EP_Wrap_Py_DECREF");
        err = dlerror();
        checkWrapperDLLFunction(err, "EP_Wrap_Py_DECREF", dll_errors);
        *(void **)(&EP_PyErr_Occurred) = dlsym(wrapperDLLHandle, "EP_Wrap_PyErr_Occurred");
        err = dlerror();
        checkWrapperDLLFunction(err, "EP_Wrap_PyErr_Occurred", dll_errors);
        *(void **)(&EP_PyModule_GetDict) = dlsym(wrapperDLLHandle, "EP_Wrap_PyModule_GetDict");
        err = dlerror();
        checkWrapperDLLFunction(err, "EP_Wrap_PyModule_GetDict", dll_errors);
        *(void **)(&EP_PyDict_GetItemString) = dlsym(wrapperDLLHandle, "EP_Wrap_PyDict_GetItemString");
        err = dlerror();
        checkWrapperDLLFunction(err, "EP_Wrap_PyDict_GetItemString", dll_errors);
        *(void **)(&EP_PyUnicode_AsUTF8) = dlsym(wrapperDLLHandle, "EP_Wrap_PyUnicode_AsUTF8");
        err = dlerror();
        checkWrapperDLLFunction(err, "EP_Wrap_PyUnicode_AsUTF8", dll_errors);
        *(void **)(&EP_PyUnicode_AsUTF8String) = dlsym(wrapperDLLHandle, "EP_Wrap_PyUnicode_AsUTF8String");
        err = dlerror();
        checkWrapperDLLFunction(err, "EP_Wrap_PyUnicode_AsUTF8String", dll_errors);
        *(void **)(&EP_PyCallable_Check) = dlsym(wrapperDLLHandle, "EP_Wrap_PyCallable_Check");
        err = dlerror();
        checkWrapperDLLFunction(err, "EP_Wrap_PyCallable_Check", dll_errors);
        *(void **)(&EP_PyObject_CallObject) = dlsym(wrapperDLLHandle, "EP_Wrap_PyObject_CallObject");
        err = dlerror();
        checkWrapperDLLFunction(err, "EP_Wrap_PyObject_CallObject", dll_errors);
        *(void **)(&EP_PyObject_GetAttrString) = dlsym(wrapperDLLHandle, "EP_Wrap_PyObject_GetAttrString");
        err = dlerror();
        checkWrapperDLLFunction(err, "EP_Wrap_PyObject_GetAttrString", dll_errors);
        *(void **)(&EP_PyObject_CallFunction) = dlsym(wrapperDLLHandle, "EP_Wrap_PyObject_CallFunction");
        err = dlerror();
        checkWrapperDLLFunction(err, "EP_Wrap_PyObject_CallFunction", dll_errors);
        *(void **)(&EP_PyObject_CallFunction3Args) = dlsym(wrapperDLLHandle, "EP_Wrap_PyObject_CallFunction3Args");
        err = dlerror();
        checkWrapperDLLFunction(err, "EP_Wrap_PyObject_CallFunction3Args", dll_errors);
        *(void **)(&EP_PyObject_CallMethod) = dlsym(wrapperDLLHandle, "EP_Wrap_PyObject_CallMethod");
        err = dlerror();
        checkWrapperDLLFunction(err, "EP_Wrap_PyObject_CallMethod", dll_errors);
        *(void **)(&EP_PyObject_CallMethod1Arg) = dlsym(wrapperDLLHandle, "EP_Wrap_PyObject_CallMethod1Arg");
        err = dlerror();
        checkWrapperDLLFunction(err, "EP_Wrap_PyObject_CallMethod1Arg", dll_errors);
        *(void **)(&EP_PyObject_CallMethod2ObjArg) = dlsym(wrapperDLLHandle, "EP_Wrap_PyObject_CallMethod2ObjArg");
        err = dlerror();
        checkWrapperDLLFunction(err, "EP_Wrap_PyObject_CallMethod2ObjArg", dll_errors);
        *(void **)(&EP_PyList_Check) = dlsym(wrapperDLLHandle, "EP_Wrap_PyList_Check");
        err = dlerror();
        checkWrapperDLLFunction(err, "EP_Wrap_PyList_Check", dll_errors);
        *(void **)(&EP_PyList_Size) = dlsym(wrapperDLLHandle, "EP_Wrap_PyList_Size");
        err = dlerror();
        checkWrapperDLLFunction(err, "EP_Wrap_PyList_Size", dll_errors);
        *(void **)(&EP_PyList_GetItem) = dlsym(wrapperDLLHandle, "EP_Wrap_PyList_GetItem");
        err = dlerror();
        checkWrapperDLLFunction(err, "EP_Wrap_PyList_GetItem", dll_errors);
        *(void **)(&EP_PyUnicode_Check) = dlsym(wrapperDLLHandle, "EP_Wrap_PyUnicode_Check");
        err = dlerror();
        checkWrapperDLLFunction(err, "EP_Wrap_PyUnicode_Check", dll_errors);
        *(void **)(&EP_PyLong_Check) = dlsym(wrapperDLLHandle, "EP_Wrap_PyLong_Check");
        err = dlerror();
        checkWrapperDLLFunction(err, "EP_Wrap_PyLong_Check", dll_errors);
        *(void **)(&EP_PyLong_AsLong) = dlsym(wrapperDLLHandle, "EP_Wrap_PyLong_AsLong");
        err = dlerror();
        checkWrapperDLLFunction(err, "EP_Wrap_PyLong_AsLong", dll_errors);
        *(void **)(&EP_Py_SetPythonHome) = dlsym(wrapperDLLHandle, "EP_Wrap_Py_SetPythonHome");
        err = dlerror();
        checkWrapperDLLFunction(err, "EP_Wrap_Py_SetPythonHome", dll_errors);
        *(void **)(&EP_Py_BuildValue) = dlsym(wrapperDLLHandle, "EP_Wrap_Py_BuildValue");
        err = dlerror();
        checkWrapperDLLFunction(err, "EP_Wrap_Py_BuildValue", dll_errors);
        *(void **)(&EP_PyLong_FromVoidPtr) = dlsym(wrapperDLLHandle, "EP_Wrap_PyLong_FromVoidPtr");
        err = dlerror();
        checkWrapperDLLFunction(err, "EP_Wrap_PyLong_FromVoidPtr", dll_errors);
        *(void **)(&EP_PyString_FromString) = dlsym(wrapperDLLHandle, "EP_Wrap_PyString_FromString");
        err = dlerror();
        checkWrapperDLLFunction(err, "EP_Wrap_PyString_FromString", dll_errors);
#endif
        if (dll_errors) {
            ShowFatalError("Python DLL problem causes program termination");
        }
#endif
    }

    void registerNewCallback(EnergyPlusData &EP_UNUSED(state), int iCalledFrom, const std::function<void(void *)> &f)
    {
        callbacks[iCalledFrom].push_back(f);
    }

    void onBeginEnvironment() {
        // reset vars and trends -- sensors and actuators are reset by EMS
        for (auto & v : globalVariableValues) {
            v = 0;
        }
        // reinitialize trend variables so old data are purged
        for (auto & tr : trends) {
            tr.reset();
        }
    }

    int PluginManager::numActiveCallbacks()
    {
        return (int)callbacks.size();
    }

    void runAnyRegisteredCallbacks(EnergyPlusData &state, int iCalledFrom, bool &anyRan)
    {
        if (DataGlobals::KickOffSimulation) return;
        for (auto const &cb : callbacks[iCalledFrom]) {
            cb((void *) &state);
            anyRan = true;
        }
#if LINK_WITH_PYTHON == 1
        for (auto &plugin : plugins) {
            if (plugin.runDuringWarmup || !DataGlobals::WarmupFlag) {
                bool const didOneRun = plugin.run(state, iCalledFrom);
                if (didOneRun) anyRan = true;
            }
        }
#endif
    }

#if LINK_WITH_PYTHON == 1
    std::string pythonStringForUsage()
    {
        if (DataGlobals::eplusRunningViaAPI) {
            return "Python Version not accessible during API calls";
        }
        loadWrapperDLL();
        std::string sVersion = (*EP_Py_GetVersion)();
        return "Linked to Python Version: \"" + sVersion + "\"";
    }
#else
    std::string pythonStringForUsage()
    {
        return "This version of EnergyPlus not linked to Python library.";
    }
#endif

    void PluginManager::setupOutputVariables()
    {
#if LINK_WITH_PYTHON == 1
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
                    units = fields.at("units").get<std::string>();
                }
                // get the index of the global variable, fatal if it doesn't mach one
                // validate type of data, update frequency, and look up units enum value
                // call setup output variable - variable TYPE is "PythonPlugin:OutputVariable"
                int variableHandle = EnergyPlus::PluginManagement::PluginManager::getGlobalVariableHandle(varName);
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
                        SetupOutputVariable(sOutputVariable,
                                            thisUnit,
                                            PluginManagement::globalVariableValues[variableHandle],
                                            sUpdateFreq,
                                            sAvgOrSum,
                                            thisObjectName);
                    } else {
                        SetupOutputVariable(sOutputVariable,
                                            thisUnit,
                                            PluginManagement::globalVariableValues[variableHandle],
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
                        EnergyPlus::ShowFatalError("Input error on PythonPlugin:OutputVariable causes program termination");
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
                        ShowSevereError("Invalid input for PythonPlugin:OutputVariable, unexpected Resource Type = " + resourceType);
                        ShowFatalError("Python plugin output variable input problem causes program termination");
                    }

                    // Group Type
                    if (fields.find("group_type") == fields.end()) {
                        EnergyPlus::ShowSevereError("Input error on PythonPlugin:OutputVariable = " + thisObjectName);
                        EnergyPlus::ShowContinueError("The variable was marked as metered, but did not define a group type");
                        EnergyPlus::ShowContinueError("For metered variables, the resource type, group type, and end use category must be defined");
                        EnergyPlus::ShowFatalError("Input error on PythonPlugin:OutputVariable causes program termination");
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
                        EnergyPlus::ShowFatalError("Input error on PythonPlugin:OutputVariable causes program termination");
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
                        ShowSevereError("Invalid input for PythonPlugin:OutputVariable, unexpected End-use Subcategory = " + groupType);
                        ShowFatalError("Python plugin output variable input problem causes program termination");
                    }

                    // Additional End Use Types Only Used for EnergyTransfer
                    if ((sResourceType != "EnergyTransfer") &&
                        (sEndUse == "HeatingCoils" || sEndUse == "CoolingCoils" || sEndUse == "Chillers" || sEndUse == "Boilers" ||
                         sEndUse == "Baseboard" || sEndUse == "HeatRecoveryForCooling" || sEndUse == "HeatRecoveryForHeating")) {
                        ShowWarningError("Inconsistent resource type input for PythonPlugin:OutputVariable = " + thisObjectName);
                        ShowContinueError("For end use subcategory = " + sEndUse + ", resource type must be EnergyTransfer");
                        ShowContinueError("Resource type is being reset to EnergyTransfer and the simulation continues...");
                        sResourceType = "EnergyTransfer";
                    }

                    std::string sEndUseSubcategory;
                    if (fields.find("end_use_subcategory") != fields.end()) {
                        sEndUseSubcategory = fields.at("end_use_subcategory").get<std::string>();
                    }

                    if (sEndUseSubcategory.empty()) { // no subcategory
                        SetupOutputVariable(sOutputVariable,
                                            thisUnit,
                                            PluginManagement::globalVariableValues[variableHandle],
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
                                            PluginManagement::globalVariableValues[variableHandle],
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

    void clear_state()
    {
        callbacks.clear();
#if LINK_WITH_PYTHON == 1
        for (auto &plugin : plugins) {
            plugin.shutdown(); // clear unmanaged memory first
        }
        trends.clear();
        globalVariableNames.clear();
        globalVariableValues.clear();
        plugins.clear();
        PluginManagement::fullyReady = false;
        PluginManagement::apiErrorFlag = false;
        PluginManager * p = PluginManagement::pluginManager.release();
        delete p;
        wrapperDLLHandle = nullptr;
        EP_Py_SetPath = nullptr;
        EP_Py_GetVersion = nullptr;
        EP_Py_DecodeLocale = nullptr;
        EP_Py_InitializeEx = nullptr;
        EP_PyRun_SimpleString = nullptr;
        EP_Py_FinalizeEx = nullptr;
        EP_PyErr_Fetch = nullptr;
        EP_PyErr_NormalizeException = nullptr;
        EP_PyObject_Repr = nullptr;
        EP_PyUnicode_AsEncodedString = nullptr;
        EP_PyBytes_AsString = nullptr;
        EP_PyUnicode_DecodeFSDefault = nullptr;
        EP_PyImport_Import = nullptr;
        EP_Py_DECREF = nullptr;
        EP_PyErr_Occurred = nullptr;
        EP_PyModule_GetDict = nullptr;
        EP_PyDict_GetItemString = nullptr;
        EP_PyUnicode_AsUTF8 = nullptr;
        EP_PyUnicode_AsUTF8String = nullptr;
        EP_PyCallable_Check = nullptr;
        EP_PyObject_CallObject = nullptr;
        EP_PyObject_GetAttrString = nullptr;
        EP_PyObject_CallFunction = nullptr;
        EP_PyObject_CallFunction3Args = nullptr;
        EP_PyObject_CallMethod = nullptr;
        EP_PyList_Check = nullptr;
        EP_PyList_Size = nullptr;
        EP_PyList_GetItem = nullptr;
        EP_PyUnicode_Check = nullptr;
        EP_PyLong_Check = nullptr;
        EP_PyLong_AsLong = nullptr;
        EP_Py_SetPythonHome = nullptr;
#endif
    }

    PluginManager::PluginManager()
    {
#if LINK_WITH_PYTHON == 1
        // we'll need the program directory for a few things so get it once here at the top and sanitize it
        std::string programPath = FileSystem::getAbsolutePath(FileSystem::getProgramPath());
        std::string programDir = FileSystem::getParentDirectoryPath(programPath);
        std::string sanitizedProgramDir = PluginManager::sanitizedPath(programDir);

        // so first things first is we need to link up with the Python DLL file and get refs to the functions in there
        loadWrapperDLL();

        // I think we need to set the python path before initializing the library
        // make this relative to the binary
        std::string pathToPythonPackages = sanitizedProgramDir + DataStringGlobals::pathChar + "python_standard_lib";
        FileSystem::makeNativePath(pathToPythonPackages);
        wchar_t *a = (*EP_Py_DecodeLocale)(pathToPythonPackages.c_str(), nullptr);
        (*EP_Py_SetPath)(a);
        (*EP_Py_SetPythonHome)(a);

        // now that we have set the path, we can initialize python
        // from https://docs.python.org/3/c-api/init.html
        // If arg 0, it skips init registration of signal handlers, which might be useful when Python is embedded.
        (*EP_Py_InitializeEx)(0);

        (*EP_PyRun_SimpleString)("import sys"); // allows us to report sys.path later

        // we also need to set an extra import path to find some dynamic library loading stuff, again make it relative to the binary
        std::string pathToDynLoad = sanitizedProgramDir + "python_standard_lib/lib-dynload";
        FileSystem::makeNativePath(pathToDynLoad);
        std::string libDirDynLoad = PluginManager::sanitizedPath(pathToDynLoad);
        PluginManager::addToPythonPath(libDirDynLoad, false);

        // now for additional paths:
        // we'll always want to add the program executable directory to PATH so that Python can find the installed pyenergyplus package
        // we will then optionally add the current working directory to allow Python to find scripts in the current directory
        // we will then optionally add the directory of the running IDF to allow Python to find scripts kept next to the IDF
        // we will then optionally add any additional paths the user specifies on the search paths object

        // so add the executable directory here
        PluginManager::addToPythonPath(sanitizedProgramDir, false);

        // Read all the additional search paths next
        std::string const sPaths = "PythonPlugin:SearchPaths";
        int searchPaths = inputProcessor->getNumObjectsFound(sPaths);
        if (searchPaths == 0) {
            // no search path objects in the IDF, just do the default behavior: add the current working dir and the input file dir
            PluginManager::addToPythonPath(".", false);
            std::string sanitizedInputFileDir = PluginManager::sanitizedPath(DataStringGlobals::inputDirPathName);
            PluginManager::addToPythonPath(sanitizedInputFileDir, false);
        }
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
                std::string workingDirFlagUC = "YES";
                try {
                    workingDirFlagUC = EnergyPlus::UtilityRoutines::MakeUPPERCase(fields.at("add_current_working_directory_to_search_path"));
                } catch (nlohmann::json::out_of_range &e) {
                    // defaulted to YES
                }
                if (workingDirFlagUC == "YES") {
                    PluginManager::addToPythonPath(".", false);
                }
                std::string inputFileDirFlagUC = "YES";
                try {
                    inputFileDirFlagUC = EnergyPlus::UtilityRoutines::MakeUPPERCase(fields.at("add_input_file_directory_to_search_path"));
                } catch (nlohmann::json::out_of_range &e) {
                    // defaulted to YES
                }
                if (inputFileDirFlagUC == "YES") {
                    std::string sanitizedInputFileDir = PluginManager::sanitizedPath(DataStringGlobals::inputDirPathName);
                    PluginManager::addToPythonPath(sanitizedInputFileDir, false);
                }
                try {
                    auto const vars = fields.at("py_search_paths");
                    for (const auto &var : vars) {
                        try {
                            PluginManager::addToPythonPath(PluginManager::sanitizedPath(var.at("search_path")), true);
                        } catch (nlohmann::json::out_of_range &e) {
                            // empty entry
                        }
                    }
                } catch (nlohmann::json::out_of_range& e) {
                    // catch when no paths are passed
                    // nothing to do here
                }
            }
        }

        // Now read all the actual plugins and interpret them
        // IMPORTANT -- DO NOT CALL setup() UNTIL ALL INSTANCES ARE DONE
        std::string const sPlugins = "PythonPlugin:Instance";
        int pluginInstances = inputProcessor->getNumObjectsFound(sPlugins);
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
                std::string sWarmup = EnergyPlus::UtilityRoutines::MakeUPPERCase(fields.at("run_during_warmup_days"));
                bool warmup = false;
                if (sWarmup == "YES") {
                    warmup = true;
                }
                plugins.emplace_back(fileName, className, thisObjectName, warmup);
            }
        }

        // IMPORTANT - CALL setup() HERE ONCE ALL INSTANCES ARE CONSTRUCTED TO AVOID DESTRUCTOR/MEMORY ISSUES DURING VECTOR RESIZING
        for (auto &plugin : plugins) {
            plugin.setup();
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
                auto const vars = fields.at("global_py_vars");
                for (const auto &var : vars) {
                    this->addGlobalVariable(var.at("variable_name"));
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
        int trendInstances = inputProcessor->getNumObjectsFound(sTrends);
        if (trendInstances > 0) {
            auto const instances = inputProcessor->epJSON.find(sTrends);
            if (instances == inputProcessor->epJSON.end()) {
                ShowSevereError(sTrends + ": Somehow getNumObjectsFound was > 0 but epJSON.find found 0"); // LCOV_EXCL_LINE
            }
            auto &instancesValue = instances.value();
            for (auto instance = instancesValue.begin(); instance != instancesValue.end(); ++instance) {
                auto const &fields = instance.value();
                auto const &thisObjectName = EnergyPlus::UtilityRoutines::MakeUPPERCase(instance.key());
                inputProcessor->markObjectAsUsed(sGlobals, thisObjectName);
                std::string variableName = fields.at("name_of_a_python_plugin_variable");
                int variableIndex = EnergyPlus::PluginManagement::PluginManager::getGlobalVariableHandle(variableName);
                int numValues = fields.at("number_of_timesteps_to_be_logged");
                trends.emplace_back(thisObjectName, numValues, variableIndex);
                this->maxTrendVariableIndex++;
            }
        }

        // setting up output variables deferred until later in the simulation setup process
#else
        // need to alert only if a plugin instance is found
        std::string const sPlugins = "PythonPlugin:Instance";
        int pluginInstances = inputProcessor->getNumObjectsFound(sPlugins);
        if (pluginInstances > 0) {
            EnergyPlus::ShowFatalError("Python Plugin instance found, but this build of EnergyPlus is not compiled with Python.");
        }
#endif
    }

    PluginManager::~PluginManager()
    {
#if LINK_WITH_PYTHON
        if (EP_Py_FinalizeEx) (*EP_Py_FinalizeEx)();
#ifdef _WIN32
        if (wrapperDLLHandle) FreeLibrary((HINSTANCE)wrapperDLLHandle);
#else
        if (wrapperDLLHandle) dlclose(wrapperDLLHandle);
#endif  // PLATFORM
#endif  // LINK_WITH_PYTHON
    }

#if LINK_WITH_PYTHON == 1
    std::string PluginManager::sanitizedPath(std::string path)
    {
        // there are parts of this program that need to write out a string to execute in Python
        // because of that, escaped backslashes actually need double escaping
        // plus, the string cannot end with a backslash
        // sanitize the path to remove any trailing backslash
        if (path.empty()) {
            // this is really only likely to occur during unit testing, just return the original blank path
            return path;
        }
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
#else
    std::string PluginManager::sanitizedPath(std::string EP_UNUSED(path))
    {
        return "";
    }
#endif

    void PluginInstance::reportPythonError()
    {
#if LINK_WITH_PYTHON == 1
        PyObjectWrap *exc_type = nullptr;
        PyObjectWrap *exc_value = nullptr;
        PyObjectWrap *exc_tb = nullptr;
        (*EP_PyErr_Fetch)(&exc_type, &exc_value, &exc_tb);
        // Normalizing the exception is needed. Without it, our custom EnergyPlusException go through just fine
        // but any ctypes built-in exception for eg will have wrong types
        (*EP_PyErr_NormalizeException)(&exc_type, &exc_value, &exc_tb);
        PyObjectWrap str_exc_value = (*EP_PyObject_Repr)(exc_value); // Now a unicode object
        PyObjectWrap pyStr2 = (*EP_PyUnicode_AsEncodedString)(str_exc_value, "utf-8", "Error ~");
        (*EP_Py_DECREF)(str_exc_value);
        char *strExcValue = (*EP_PyBytes_AsString)(pyStr2); // NOLINT(hicpp-signed-bitwise)
        (*EP_Py_DECREF)(pyStr2);
        EnergyPlus::ShowContinueError("Python error description follows: ");
        EnergyPlus::ShowContinueError(strExcValue);

        // See if we can get a full traceback.
        // Calls into python, and does the same as capturing the exception in `e`
        // then `print(traceback.format_exception(e.type, e.value, e.tb))`
        PyObjectWrap pModuleName = (*EP_PyUnicode_DecodeFSDefault)("traceback");
        PyObjectWrap pyth_module = (*EP_PyImport_Import)(pModuleName);
        (*EP_Py_DECREF)(pModuleName);

        if (pyth_module == nullptr) {
            EnergyPlus::ShowFatalError("Cannot find 'traceback' module in reportPythonError(), this is weird");
            return;
        }

        PyObjectWrap pyth_func = (*EP_PyObject_GetAttrString)(pyth_module, "format_exception");
        (*EP_Py_DECREF)(pyth_module); // PyImport_Import returns a new reference, decrement it

        if (pyth_func || (*EP_PyCallable_Check)(pyth_func)) {

            PyObjectWrap pyth_val = (*EP_PyObject_CallFunction3Args)(pyth_func, "OOO", exc_type, exc_value, exc_tb);

            // traceback.format_exception returns a list, so iterate on that
            if (!pyth_val || !(*EP_PyList_Check)(pyth_val)) { // NOLINT(hicpp-signed-bitwise)
                EnergyPlus::ShowFatalError("In reportPythonError(), traceback.format_exception did not return a list.");
            }

            unsigned long numVals = (*EP_PyList_Size)(pyth_val);
            if (numVals == 0) {
                EnergyPlus::ShowFatalError("No traceback available");
                return;
            }

            EnergyPlus::ShowContinueError("Python traceback follows: ");

            EnergyPlus::ShowContinueError("```");

            for (unsigned long itemNum = 0; itemNum < numVals; itemNum++) {
                PyObjectWrap item = (*EP_PyList_GetItem)(pyth_val, itemNum);
                if ((*EP_PyUnicode_Check)(item)) { // NOLINT(hicpp-signed-bitwise) -- something inside Python code causes warning
                    std::string traceback_line = (*EP_PyUnicode_AsUTF8)(item);
                    if (!traceback_line.empty() && traceback_line[traceback_line.length()-1] == '\n') {
                        traceback_line.erase(traceback_line.length()-1);
                    }
                    EnergyPlus::ShowContinueError(" >>> " + traceback_line);
                }
                // PyList_GetItem returns a borrowed reference, do not decrement
            }

            EnergyPlus::ShowContinueError("```");

            // PyList_Size returns a borrowed reference, do not decrement
            (*EP_Py_DECREF)(pyth_val); // PyObject_CallFunction returns new reference, decrement
        }
        (*EP_Py_DECREF)(pyth_func); // PyObject_GetAttrString returns a new reference, decrement it
#endif
    }

    void PluginInstance::setup()
    {
#if LINK_WITH_PYTHON == 1
        // this first section is really all about just ultimately getting a full Python class instance
        // this answer helped with a few things: https://ru.stackoverflow.com/a/785927

        PyObjectWrap pModuleName = (*EP_PyUnicode_DecodeFSDefault)(this->moduleName.c_str());
        this->pModule = (*EP_PyImport_Import)(pModuleName);
        // PyUnicode_DecodeFSDefault documentation does not explicitly say whether it returns a new or borrowed reference,
        // but other functions in that section say they return a new reference, and that makes sense to me, so I think we
        // should decrement it.
        (*EP_Py_DECREF)(pModuleName);
        if (!this->pModule) {
            EnergyPlus::ShowSevereError("Failed to import module \"" + this->moduleName + "\"");
            // ONLY call PyErr_Print if PyErr has occurred, otherwise it will cause other problems
            if ((*EP_PyErr_Occurred)()) {
                PluginInstance::reportPythonError();
            } else {
                EnergyPlus::ShowContinueError("It could be that the module could not be found, or that there was an error in importing");
            }
            EnergyPlus::ShowFatalError("Python import error causes program termination");
        }
        PyObjectWrap pModuleDict = (*EP_PyModule_GetDict)(this->pModule);
        if (!pModuleDict) {
            EnergyPlus::ShowSevereError("Failed to read module dictionary from module \"" + this->moduleName + "\"");
            if ((*EP_PyErr_Occurred)()) {
                PluginInstance::reportPythonError();
            } else {
                EnergyPlus::ShowContinueError("It could be that the module was empty");
            }
            EnergyPlus::ShowFatalError("Python module error causes program termination");
        }
        std::string fileVarName = "__file__";
        PyObjectWrap pFullPath = (*EP_PyDict_GetItemString)(pModuleDict, fileVarName.c_str());
        if (!pFullPath) {
            // something went really wrong, this should only happen if you do some *weird* python stuff like
            // import from database or something
            ShowFatalError("Could not get full path");
        } else {
            PyObjectWrap pStrObj = (*EP_PyUnicode_AsUTF8String)(pFullPath);
            char *zStr = (*EP_PyBytes_AsString)(pStrObj);
            std::string s(zStr);
            (*EP_Py_DECREF)(pStrObj); // PyUnicode_AsUTF8String returns a new reference, decrement it
            ShowMessage("PythonPlugin: Class " + className + " imported from: " + s);
        }
        PyObjectWrap pClass = (*EP_PyDict_GetItemString)(pModuleDict, className.c_str());
        // Py_DECREF(pModuleDict);  // PyModule_GetDict returns a borrowed reference, DO NOT decrement
        if (!pClass) {
            EnergyPlus::ShowSevereError("Failed to get class type \"" + className + "\" from module \"" + moduleName + "\"");
            if ((*EP_PyErr_Occurred)()) {
                PluginInstance::reportPythonError();
            } else {
                EnergyPlus::ShowContinueError("It could be the class name is misspelled or missing.");
            }
            EnergyPlus::ShowFatalError("Python class import error causes program termination");
        }
        if (!(*EP_PyCallable_Check)(pClass)) {
            EnergyPlus::ShowSevereError("Got class type \"" + className + "\", but it cannot be called/instantiated");
            if ((*EP_PyErr_Occurred)()) {
                PluginInstance::reportPythonError();
            } else {
                EnergyPlus::ShowContinueError("Is it possible the class name is actually just a variable?");
            }
            EnergyPlus::ShowFatalError("Python class check error causes program termination");
        }
        this->pClassInstance = (*EP_PyObject_CallObject)(pClass, nullptr);
        // Py_DECREF(pClass);  // PyDict_GetItemString returns a borrowed reference, DO NOT decrement
        if (!this->pClassInstance) {
            EnergyPlus::ShowSevereError("Something went awry calling class constructor for class \"" + className + "\"");
            if ((*EP_PyErr_Occurred)()) {
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

        // check which methods are overridden in the derived class
        std::string const detectOverriddenFunctionName = "_detect_overridden";
        PyObjectWrap detectFunction = (*EP_PyObject_GetAttrString)(this->pClassInstance, detectOverriddenFunctionName.c_str());
        if (!detectFunction || !(*EP_PyCallable_Check)(detectFunction)) {
            EnergyPlus::ShowSevereError("Could not find or call function \"" + detectOverriddenFunctionName + "\" on class \"" + this->moduleName +
                                        "." + this->className + "\"");
            if ((*EP_PyErr_Occurred)()) {
                PluginInstance::reportPythonError();
            } else {
                EnergyPlus::ShowContinueError("This function should be available on the base class, so this is strange.");
            }
            EnergyPlus::ShowFatalError("Python _detect_overridden() function error causes program termination");
        }
        PyObjectWrap pFunctionResponse = (*EP_PyObject_CallFunction)(detectFunction, nullptr);
        (*EP_Py_DECREF)(detectFunction); // PyObject_GetAttrString returns a new reference, decrement it
        if (!pFunctionResponse) {
            EnergyPlus::ShowSevereError("Call to _detect_overridden() on " + this->stringIdentifier + " failed!");
            if ((*EP_PyErr_Occurred)()) {
                PluginInstance::reportPythonError();
            } else {
                EnergyPlus::ShowContinueError("This is available on the base class and should not be overridden...strange.");
            }
            EnergyPlus::ShowFatalError("Program terminates after call to _detect_overridden() on " + this->stringIdentifier + " failed!");
        }
        if (!(*EP_PyList_Check)(pFunctionResponse)) { // NOLINT(hicpp-signed-bitwise)
            EnergyPlus::ShowFatalError("Invalid return from _detect_overridden() on class \"" + this->stringIdentifier + ", this is weird");
        }
        unsigned long numVals = (*EP_PyList_Size)(pFunctionResponse);
        // at this point we know which base class methods are being overridden by the derived class
        // we can loop over them and based on the name check the appropriate flag and assign the function pointer
        if (numVals == 0) {
            EnergyPlus::ShowFatalError("Python plugin \"" + this->stringIdentifier +
                                       "\" did not override any base class methods; must override at least one");
        }
        for (unsigned long itemNum = 0; itemNum < numVals; itemNum++) {
            PyObjectWrap item = (*EP_PyList_GetItem)(pFunctionResponse, itemNum);
            if ((*EP_PyUnicode_Check)(item)) { // NOLINT(hicpp-signed-bitwise) -- something inside Python code causes warning
                std::string functionName = (*EP_PyUnicode_AsUTF8)(item);
                if (functionName == this->sHookBeginNewEnvironment) {
                    this->bHasBeginNewEnvironment = true;
                    this->pBeginNewEnvironment = (*EP_PyString_FromString)(functionName.c_str());
                } else if (functionName == this->sHookAfterNewEnvironmentWarmUpIsComplete) {
                    this->bHasAfterNewEnvironmentWarmUpIsComplete = true;
                    this->pAfterNewEnvironmentWarmUpIsComplete = (*EP_PyString_FromString)(functionName.c_str());
                } else if (functionName == this->sHookBeginZoneTimestepBeforeInitHeatBalance) {
                    this->bHasBeginZoneTimestepBeforeInitHeatBalance = true;
                    this->pBeginZoneTimestepBeforeInitHeatBalance = (*EP_PyString_FromString)(functionName.c_str());
                } else if (functionName == this->sHookBeginZoneTimestepAfterInitHeatBalance) {
                    this->bHasBeginZoneTimestepAfterInitHeatBalance = true;
                    this->pBeginZoneTimestepAfterInitHeatBalance = (*EP_PyString_FromString)(functionName.c_str());
                } else if (functionName == this->sHookBeginTimestepBeforePredictor) {
                    this->bHasBeginTimestepBeforePredictor = true;
                    this->pBeginTimestepBeforePredictor = (*EP_PyString_FromString)(functionName.c_str());
                } else if (functionName == this->sHookAfterPredictorBeforeHVACManagers) {
                    this->bHasAfterPredictorBeforeHVACManagers = true;
                    this->pAfterPredictorBeforeHVACManagers = (*EP_PyString_FromString)(functionName.c_str());
                } else if (functionName == this->sHookAfterPredictorAfterHVACManagers) {
                    this->bHasAfterPredictorAfterHVACManagers = true;
                    this->pAfterPredictorAfterHVACManagers = (*EP_PyString_FromString)(functionName.c_str());
                } else if (functionName == this->sHookInsideHVACSystemIterationLoop) {
                    this->bHasInsideHVACSystemIterationLoop = true;
                    this->pInsideHVACSystemIterationLoop = (*EP_PyString_FromString)(functionName.c_str());
                } else if (functionName == this->sHookEndOfZoneTimestepBeforeZoneReporting) {
                    this->bHasEndOfZoneTimestepBeforeZoneReporting = true;
                    this->pEndOfZoneTimestepBeforeZoneReporting = (*EP_PyString_FromString)(functionName.c_str());
                } else if (functionName == this->sHookEndOfZoneTimestepAfterZoneReporting) {
                    this->bHasEndOfZoneTimestepAfterZoneReporting = true;
                    this->pEndOfZoneTimestepAfterZoneReporting = (*EP_PyString_FromString)(functionName.c_str());
                } else if (functionName == this->sHookEndOfSystemTimestepBeforeHVACReporting) {
                    this->bHasEndOfSystemTimestepBeforeHVACReporting = true;
                    this->pEndOfSystemTimestepBeforeHVACReporting = (*EP_PyString_FromString)(functionName.c_str());
                } else if (functionName == this->sHookEndOfSystemTimestepAfterHVACReporting) {
                    this->bHasEndOfSystemTimestepAfterHVACReporting = true;
                    this->pEndOfSystemTimestepAfterHVACReporting = (*EP_PyString_FromString)(functionName.c_str());
                } else if (functionName == this->sHookEndOfZoneSizing) {
                    this->bHasEndOfZoneSizing = true;
                    this->pEndOfZoneSizing = (*EP_PyString_FromString)(functionName.c_str());
                } else if (functionName == this->sHookEndOfSystemSizing) {
                    this->bHasEndOfSystemSizing = true;
                    this->pEndOfSystemSizing = (*EP_PyString_FromString)(functionName.c_str());
                } else if (functionName == this->sHookAfterComponentInputReadIn) {
                    this->bHasAfterComponentInputReadIn = true;
                    this->pAfterComponentInputReadIn = (*EP_PyString_FromString)(functionName.c_str());
                } else if (functionName == this->sHookUserDefinedComponentModel) {
                    this->bHasUserDefinedComponentModel = true;
                    this->pUserDefinedComponentModel = (*EP_PyString_FromString)(functionName.c_str());
                } else if (functionName == this->sHookUnitarySystemSizing) {
                    this->bHasUnitarySystemSizing = true;
                    this->pUnitarySystemSizing = (*EP_PyString_FromString)(functionName.c_str());
                } else {
                    // the Python _detect_function worker is supposed to ignore any other functions so they don't show up at this point
                    // I don't think it's appropriate to warn here, so just ignore and move on
                }
            }
            // PyList_GetItem returns a borrowed reference, do not decrement
        }
        // PyList_Size returns a borrowed reference, do not decrement
        (*EP_Py_DECREF)(pFunctionResponse); // PyObject_CallFunction returns new reference, decrement
#endif
    }

    void PluginInstance::shutdown() const
    {
#if LINK_WITH_PYTHON == 1
        (*EP_Py_DECREF)(this->pClassInstance);
        (*EP_Py_DECREF)(this->pModule); // PyImport_Import returns a new reference, decrement it
        if (this->bHasBeginNewEnvironment) (*EP_Py_DECREF)(this->pBeginNewEnvironment);
        if (this->bHasAfterNewEnvironmentWarmUpIsComplete) (*EP_Py_DECREF)(this->pAfterNewEnvironmentWarmUpIsComplete);
        if (this->bHasBeginZoneTimestepBeforeInitHeatBalance) (*EP_Py_DECREF)(this->pBeginZoneTimestepBeforeInitHeatBalance);
        if (this->bHasBeginZoneTimestepAfterInitHeatBalance) (*EP_Py_DECREF)(this->pBeginZoneTimestepAfterInitHeatBalance);
        if (this->bHasBeginTimestepBeforePredictor) (*EP_Py_DECREF)(this->pBeginTimestepBeforePredictor);
        if (this->bHasAfterPredictorBeforeHVACManagers) (*EP_Py_DECREF)(this->pAfterPredictorBeforeHVACManagers);
        if (this->bHasAfterPredictorAfterHVACManagers) (*EP_Py_DECREF)(this->pAfterPredictorAfterHVACManagers);
        if (this->bHasInsideHVACSystemIterationLoop) (*EP_Py_DECREF)(this->pInsideHVACSystemIterationLoop);
        if (this->bHasEndOfZoneTimestepBeforeZoneReporting) (*EP_Py_DECREF)(this->pEndOfZoneTimestepBeforeZoneReporting);
        if (this->bHasEndOfZoneTimestepAfterZoneReporting) (*EP_Py_DECREF)(this->pEndOfZoneTimestepAfterZoneReporting);
        if (this->bHasEndOfSystemTimestepBeforeHVACReporting) (*EP_Py_DECREF)(this->pEndOfSystemTimestepBeforeHVACReporting);
        if (this->bHasEndOfSystemTimestepAfterHVACReporting) (*EP_Py_DECREF)(this->pEndOfSystemTimestepAfterHVACReporting);
        if (this->bHasEndOfZoneSizing) (*EP_Py_DECREF)(this->pEndOfZoneSizing);
        if (this->bHasEndOfSystemSizing) (*EP_Py_DECREF)(this->pEndOfSystemSizing);
        if (this->bHasAfterComponentInputReadIn) (*EP_Py_DECREF)(this->pAfterComponentInputReadIn);
        if (this->bHasUserDefinedComponentModel) (*EP_Py_DECREF)(this->pUserDefinedComponentModel);
        if (this->bHasUnitarySystemSizing) (*EP_Py_DECREF)(this->pUnitarySystemSizing);
#endif
    }

#if LINK_WITH_PYTHON == 1
    bool PluginInstance::run(EnergyPlusData &state, int iCalledFrom) const
    {
        // returns true if a plugin actually ran
        PyObjectWrap pFunctionName = nullptr;
        const char * functionName = nullptr;
        if (iCalledFrom == DataGlobals::emsCallFromBeginNewEvironment) {
            if (this->bHasBeginNewEnvironment) {
                pFunctionName = this->pBeginNewEnvironment;
                functionName = this->sHookBeginNewEnvironment;
            }
        } else if (iCalledFrom == DataGlobals::emsCallFromZoneSizing) {
            if (this->bHasEndOfZoneSizing) {
                pFunctionName = this->pEndOfZoneSizing;
                functionName = this->sHookEndOfZoneSizing;
            }
        } else if (iCalledFrom == DataGlobals::emsCallFromSystemSizing) {
            if (this->bHasEndOfSystemSizing) {
                pFunctionName = this->pEndOfSystemSizing;
                functionName = this->sHookEndOfSystemSizing;
            }
        } else if (iCalledFrom == DataGlobals::emsCallFromBeginNewEvironmentAfterWarmUp) {
            if (this->bHasAfterNewEnvironmentWarmUpIsComplete) {
                pFunctionName = this->pAfterNewEnvironmentWarmUpIsComplete;
                functionName = this->sHookAfterNewEnvironmentWarmUpIsComplete;
            }
        } else if (iCalledFrom == DataGlobals::emsCallFromBeginTimestepBeforePredictor) {
            if (this->bHasBeginTimestepBeforePredictor) {
                pFunctionName = this->pBeginTimestepBeforePredictor;
                functionName = this->sHookBeginTimestepBeforePredictor;
            }
        } else if (iCalledFrom == DataGlobals::emsCallFromBeforeHVACManagers) {
            if (this->bHasAfterPredictorBeforeHVACManagers) {
                pFunctionName = this->pAfterPredictorBeforeHVACManagers;
                functionName = this->sHookAfterPredictorBeforeHVACManagers;
            }
        } else if (iCalledFrom == DataGlobals::emsCallFromAfterHVACManagers) {
            if (this->bHasAfterPredictorAfterHVACManagers) {
                pFunctionName = this->pAfterPredictorAfterHVACManagers;
                functionName = this->sHookAfterPredictorAfterHVACManagers;
            }
        } else if (iCalledFrom == DataGlobals::emsCallFromHVACIterationLoop) {
            if (this->bHasInsideHVACSystemIterationLoop) {
                pFunctionName = this->pInsideHVACSystemIterationLoop;
                functionName = this->sHookInsideHVACSystemIterationLoop;
            }
        } else if (iCalledFrom == DataGlobals::emsCallFromEndSystemTimestepBeforeHVACReporting) {
            if (this->bHasEndOfSystemTimestepBeforeHVACReporting) {
                pFunctionName = this->pEndOfSystemTimestepBeforeHVACReporting;
                functionName = this->sHookEndOfSystemTimestepBeforeHVACReporting;
            }
        } else if (iCalledFrom == DataGlobals::emsCallFromEndSystemTimestepAfterHVACReporting) {
            if (this->bHasEndOfSystemTimestepAfterHVACReporting) {
                pFunctionName = this->pEndOfSystemTimestepAfterHVACReporting;
                functionName = this->sHookEndOfSystemTimestepAfterHVACReporting;
            }
        } else if (iCalledFrom == DataGlobals::emsCallFromEndZoneTimestepBeforeZoneReporting) {
            if (this->bHasEndOfZoneTimestepBeforeZoneReporting) {
                pFunctionName = this->pEndOfZoneTimestepBeforeZoneReporting;
                functionName = this->sHookEndOfZoneTimestepBeforeZoneReporting;
            }
        } else if (iCalledFrom == DataGlobals::emsCallFromEndZoneTimestepAfterZoneReporting) {
            if (this->bHasEndOfZoneTimestepAfterZoneReporting) {
                pFunctionName = this->pEndOfZoneTimestepAfterZoneReporting;
                functionName = this->sHookEndOfZoneTimestepAfterZoneReporting;
            }
        } else if (iCalledFrom == DataGlobals::emsCallFromComponentGetInput) {
            if (this->bHasAfterComponentInputReadIn) {
                pFunctionName = this->pAfterComponentInputReadIn;
                functionName = this->sHookAfterComponentInputReadIn;
            }
        } else if (iCalledFrom == DataGlobals::emsCallFromUserDefinedComponentModel) {
            if (this->bHasUserDefinedComponentModel) {
                pFunctionName = this->pUserDefinedComponentModel;
                functionName = this->sHookUserDefinedComponentModel;
            }
        } else if (iCalledFrom == DataGlobals::emsCallFromUnitarySystemSizing) {
            if (this->bHasUnitarySystemSizing) {
                pFunctionName = this->pUnitarySystemSizing;
                functionName = this->sHookUnitarySystemSizing;
            }
        } else if (iCalledFrom == DataGlobals::emsCallFromBeginZoneTimestepBeforeInitHeatBalance) {
            if (this->bHasBeginZoneTimestepBeforeInitHeatBalance) {
                pFunctionName = this->pBeginZoneTimestepBeforeInitHeatBalance;
                functionName = this->sHookBeginZoneTimestepBeforeInitHeatBalance;
            }
        } else if (iCalledFrom == DataGlobals::emsCallFromBeginZoneTimestepAfterInitHeatBalance) {
            if (this->bHasBeginZoneTimestepAfterInitHeatBalance) {
                pFunctionName = this->pBeginZoneTimestepAfterInitHeatBalance;
                functionName = this->sHookBeginZoneTimestepAfterInitHeatBalance;
            }
        }

        // leave if we didn't find a match
        if (!pFunctionName) {
            return false;
        }

        // then call the main function
        //static const PyObjectWrap oneArgObjFormat = (*EP_Py_BuildValue)("O");
        PyObjectWrap pStateInstance = (*EP_PyLong_FromVoidPtr)((void*)&state);
        PyObjectWrap pFunctionResponse = (*EP_PyObject_CallMethod2ObjArg)(this->pClassInstance, pFunctionName, pStateInstance, nullptr);
        (*EP_Py_DECREF)(pStateInstance);
        if (!pFunctionResponse) {
            std::string const functionNameAsString(functionName); // only convert to string if an error occurs
            EnergyPlus::ShowSevereError("Call to " + functionNameAsString + "() on " + this->stringIdentifier + " failed!");
            if ((*EP_PyErr_Occurred)()) {
                PluginInstance::reportPythonError();
            } else {
                EnergyPlus::ShowContinueError("This could happen for any number of reasons, check the plugin code.");
            }
            EnergyPlus::ShowFatalError("Program terminates after call to " + functionNameAsString + "() on " + this->stringIdentifier + " failed!");
        }
        if ((*EP_PyLong_Check)(pFunctionResponse)) { // NOLINT(hicpp-signed-bitwise)
            auto exitCode = (*EP_PyLong_AsLong)(pFunctionResponse);
            if (exitCode == 0) {
                // success
            } else if (exitCode == 1) {
                EnergyPlus::ShowFatalError("Python Plugin \"" + this->stringIdentifier + "\" returned 1 to indicate EnergyPlus should abort");
            }
        } else {
            std::string const functionNameAsString(functionName); // only convert to string if an error occurs
            EnergyPlus::ShowFatalError("Invalid return from " + functionNameAsString + "() on class \"" + this->stringIdentifier +
                                       ", make sure it returns an integer exit code, either zero (success) or one (failure)");
        }
        (*EP_Py_DECREF)(pFunctionResponse); // PyObject_CallFunction returns new reference, decrement
        if (EnergyPlus::PluginManagement::apiErrorFlag) {
            EnergyPlus::ShowFatalError("API problems encountered while running plugin cause program termination.");
        }
        return true;
    }
#else
    bool PluginInstance::run(int EP_UNUSED(iCalledFrom)) const
    {
        return false;
    }
#endif

#if LINK_WITH_PYTHON == 1
    void PluginManager::addToPythonPath(const std::string &path, bool userDefinedPath)
    {
        if (path.empty()) return;

        std::string command = "sys.path.insert(0, \"" + path + "\")";
        if ((*EP_PyRun_SimpleString)(command.c_str()) == 0) {
            if (userDefinedPath) {
                EnergyPlus::ShowMessage("Successfully added path \"" + path + "\" to the sys.path in Python");
            }
            //(*EP_PyRun_SimpleString)("print(' EPS : ' + str(sys.path))");
        } else {
            EnergyPlus::ShowFatalError("ERROR adding \"" + path + "\" to the sys.path in Python");
        }
    }
#else
    void PluginManager::addToPythonPath(const std::string &EP_UNUSED(path), bool EP_UNUSED(userDefinedPath))
    {
    }
#endif

#if LINK_WITH_PYTHON == 1
    void PluginManager::addGlobalVariable(const std::string &name)
    {
        std::string const varNameUC = EnergyPlus::UtilityRoutines::MakeUPPERCase(name);
        PluginManagement::globalVariableNames.push_back(varNameUC);
        PluginManagement::globalVariableValues.push_back(Real64());
        this->maxGlobalVariableIndex++;
    }
#else
    void PluginManager::addGlobalVariable(const std::string &EP_UNUSED(name))
    {
    }
#endif

#if LINK_WITH_PYTHON == 1
    int PluginManager::getGlobalVariableHandle(const std::string &name, bool const suppress_warning)
    { // note zero is a valid handle
        std::string const varNameUC = EnergyPlus::UtilityRoutines::MakeUPPERCase(name);
        auto const it = std::find(PluginManagement::globalVariableNames.begin(), PluginManagement::globalVariableNames.end(), varNameUC);
        if (it != PluginManagement::globalVariableNames.end()) {
            return std::distance(PluginManagement::globalVariableNames.begin(), it);
        } else {
            if (suppress_warning) {
                return -1;
            } else {
                EnergyPlus::ShowSevereError("Tried to retrieve handle for a nonexistent plugin global variable");
                EnergyPlus::ShowContinueError("Name looked up: \"" + varNameUC + "\", available names: ");
                for (auto const &gvName : PluginManagement::globalVariableNames) {
                    EnergyPlus::ShowContinueError("    \"" + gvName + "\"");
                }
                EnergyPlus::ShowFatalError("Plugin global variable problem causes program termination");
                return -1; // hush the compiler warning
            }
        }
    }
#else
    int PluginManager::getGlobalVariableHandle(const std::string &EP_UNUSED(name), bool const EP_UNUSED(suppress_warning))
    {
        return -1;
    }
#endif

#if LINK_WITH_PYTHON == 1
    int PluginManager::getTrendVariableHandle(const std::string &name)
    {
        std::string const varNameUC = EnergyPlus::UtilityRoutines::MakeUPPERCase(name);
        for (size_t i = 0; i < trends.size(); i++) {
            auto &thisTrend = trends[i];
            if (thisTrend.name == varNameUC) {
                return i;
            }
        }
        return -1;
    }
#else
    int PluginManager::getTrendVariableHandle(const std::string &EP_UNUSED(name))
    {
        return -1;
    }
#endif

#if LINK_WITH_PYTHON == 1
    Real64 PluginManager::getTrendVariableValue(int handle, int timeIndex)
    {
        return trends[handle].values[timeIndex];
    }
#else
    Real64 PluginManager::getTrendVariableValue(int EP_UNUSED(handle), int EP_UNUSED(timeIndex))
    {
        return 0.0;
    }
#endif

#if LINK_WITH_PYTHON == 1
    Real64 PluginManager::getTrendVariableAverage(int handle, int count)
    {
        Real64 sum = 0;
        for (int i = 0; i < count; i++) {
            sum += trends[handle].values[i];
        }
        return sum / count;
    }
#else
    Real64 PluginManager::getTrendVariableAverage(int EP_UNUSED(handle), int EP_UNUSED(count))
    {
        return 0.0;
    }
#endif

#if LINK_WITH_PYTHON == 1
    Real64 PluginManager::getTrendVariableMin(int handle, int count)
    {
        Real64 minimumValue = 9999999999999;
        for (int i = 0; i < count; i++) {
            if (trends[handle].values[i] < minimumValue) {
                minimumValue = trends[handle].values[i];
            }
        }
        return minimumValue;
    }
#else
    Real64 PluginManager::getTrendVariableMin(int EP_UNUSED(handle), int EP_UNUSED(count))
    {
        return 0.0;
    }
#endif

#if LINK_WITH_PYTHON == 1
    Real64 PluginManager::getTrendVariableMax(int handle, int count)
    {
        Real64 maximumValue = -9999999999999;
        for (int i = 0; i < count; i++) {
            if (trends[handle].values[i] > maximumValue) {
                maximumValue = trends[handle].values[i];
            }
        }
        return maximumValue;
    }
#else
    Real64 PluginManager::getTrendVariableMax(int EP_UNUSED(handle), int EP_UNUSED(count))
    {
        return 0.0;
    }
#endif

#if LINK_WITH_PYTHON == 1
    Real64 PluginManager::getTrendVariableSum(int handle, int count)
    {
        Real64 sum = 0.0;
        for (int i = 0; i < count; i++) {
            sum += trends[handle].values[i];
        }
        return sum;
    }
#else
    Real64 PluginManager::getTrendVariableSum(int EP_UNUSED(handle), int EP_UNUSED(count))
    {
        return 0.0;
    }
#endif

#if LINK_WITH_PYTHON == 1
    Real64 PluginManager::getTrendVariableDirection(int handle, int count)
    {
        auto &trend = trends[handle];
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
    Real64 PluginManager::getTrendVariableDirection(int EP_UNUSED(handle), int EP_UNUSED(count))
    {
        return 0.0;
    }
#endif

#if LINK_WITH_PYTHON == 1
    size_t PluginManager::getTrendVariableHistorySize(int handle)
    {
        return trends[handle].values.size();
    }
#else
    size_t PluginManager::getTrendVariableHistorySize(int EP_UNUSED(handle))
    {
        return 0;
    }
#endif

    void PluginManager::updatePluginValues()
    {
#if LINK_WITH_PYTHON == 1
        for (auto &trend : trends) {
            Real64 newVarValue = PluginManager::getGlobalVariableValue(trend.indexOfPluginVariable);
            trend.values.push_front(newVarValue);
            trend.values.pop_back();
        }
#endif
    }

#if LINK_WITH_PYTHON == 1
    Real64 PluginManager::getGlobalVariableValue(int handle)
    {
        if (PluginManagement::globalVariableValues.empty()) {
            EnergyPlus::ShowFatalError(
                "Tried to access plugin global variable but it looks like there aren't any; use the PythonPlugin:Variables object to declare them.");
        }
        try {
            return PluginManagement::globalVariableValues[handle];
        } catch (...) {
            EnergyPlus::ShowSevereError("Tried to access plugin global variable value at index " + std::to_string(handle));
            EnergyPlus::ShowContinueError("Available handles range from 0 to " + std::to_string(PluginManagement::globalVariableValues.size() - 1));
            EnergyPlus::ShowFatalError("Plugin global variable problem causes program termination");
        }
        return 0.0;
    }
#else
    Real64 PluginManager::getGlobalVariableValue(int EP_UNUSED(handle))
    {
        return 0.0;
    }
#endif

#if LINK_WITH_PYTHON == 1
    void PluginManager::setGlobalVariableValue(int handle, Real64 value)
    {
        if (PluginManagement::globalVariableValues.empty()) {
            EnergyPlus::ShowFatalError("Tried to set plugin global variable but it looks like there aren't any; use the PythonPlugin:GlobalVariables "
                                       "object to declare them.");
        }
        try {
            PluginManagement::globalVariableValues[handle] = value;
        } catch (...) {
            EnergyPlus::ShowSevereError("Tried to set plugin global variable value at index " + std::to_string(handle));
            EnergyPlus::ShowContinueError("Available handles range from 0 to " + std::to_string(PluginManagement::globalVariableValues.size() - 1));
            EnergyPlus::ShowFatalError("Plugin global variable problem causes program termination");
        }
    }
#else
    void PluginManager::setGlobalVariableValue(int EP_UNUSED(handle), Real64 EP_UNUSED(value))
    {
    }
#endif

#if LINK_WITH_PYTHON == 1
    int PluginManager::getLocationOfUserDefinedPlugin(std::string const &programName)
    {
        for (size_t handle = 0; handle < plugins.size(); handle++) {
            auto const thisPlugin = plugins[handle];
            if (EnergyPlus::UtilityRoutines::MakeUPPERCase(thisPlugin.emsAlias) == EnergyPlus::UtilityRoutines::MakeUPPERCase(programName)) {
                return handle;
            }
        }
        return -1;
    }
#else
    int PluginManager::getLocationOfUserDefinedPlugin(std::string const &EP_UNUSED(programName))
    {
        return -1;
    }
#endif

#if LINK_WITH_PYTHON == 1
    void PluginManager::runSingleUserDefinedPlugin(EnergyPlusData &state, int index)
    {
        plugins[index].run(state, DataGlobals::emsCallFromUserDefinedComponentModel);
    }
#else
    void PluginManager::runSingleUserDefinedPlugin(EnergyPlusData &EP_UNUSED(state), int EP_UNUSED(index))
    {
    }
#endif

#if LINK_WITH_PYTHON == 1
    bool PluginManager::anyUnexpectedPluginObjects()
    {
        static std::vector<std::string> objectsToFind = {"PythonPlugin:OutputVariable",
                                                         "PythonPlugin:SearchPaths",
                                                         "PythonPlugin:Instance",
                                                         "PythonPlugin:Variables",
                                                         "PythonPlugin:TrendVariable"};
        int numTotalThings = 0;
        for (auto const &objToFind : objectsToFind) {
            int instances = inputProcessor->getNumObjectsFound(objToFind);
            numTotalThings += instances;
            if (numTotalThings == 1) {
                ShowSevereMessage("Found PythonPlugin objects in an IDF that is running in an API/Library workflow...this is invalid");
            }
            if (instances > 0) {
                ShowContinueError("Invalid PythonPlugin object type: " + objToFind);
            }
        }
        return numTotalThings > 0;
    }
#else
    bool PluginManager::anyUnexpectedPluginObjects()
    {
        return false;
    }
#endif

} // namespace PluginManagement
} // namespace EnergyPlus
