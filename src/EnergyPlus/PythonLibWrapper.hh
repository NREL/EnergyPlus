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

#ifndef EPLUS_PYTHON_LIB_WRAPPER_HH
#define EPLUS_PYTHON_LIB_WRAPPER_HH

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

#if _WIN32 || _MSC_VER
#define PYTHONWRAP_API __declspec(dllexport)
#else
#define PYTHONWRAP_API
#endif

extern "C" {
    PYTHONWRAP_API const char * EP_Wrap_Py_GetVersion();
    PYTHONWRAP_API wchar_t * EP_Wrap_Py_DecodeLocale(const char * s, unsigned long * size);
    PYTHONWRAP_API void EP_Wrap_Py_InitializeEx(int i);
    PYTHONWRAP_API int EP_Wrap_PyRun_SimpleString(const char * c);
    PYTHONWRAP_API int EP_Wrap_Py_FinalizeEx();
    PYTHONWRAP_API void EP_Wrap_PyErr_Fetch(PyObject **pType, PyObject **pValue, PyObject **pTrace);
    PYTHONWRAP_API void EP_Wrap_PyErr_NormalizeException(PyObject **pType, PyObject **pValue, PyObject **pTrace);
    PYTHONWRAP_API PyObject * EP_Wrap_PyObject_Repr(PyObject * o);
    PYTHONWRAP_API PyObject * EP_Wrap_PyUnicode_AsEncodedString(PyObject *unicode, const char *encoding, const char *errors);
    PYTHONWRAP_API char * EP_Wrap_PyBytes_AsString(PyObject *o);
    PYTHONWRAP_API PyObject * EP_Wrap_PyUnicode_DecodeFSDefault(const char *s);
    PYTHONWRAP_API PyObject * EP_Wrap_PyImport_Import(PyObject *name);
    PYTHONWRAP_API void EP_Wrap_Py_DECREF(PyObject *o);
    PYTHONWRAP_API PyObject * EP_Wrap_PyErr_Occurred();
    PYTHONWRAP_API PyObject * EP_Wrap_PyModule_GetDict(PyObject *module);
    PYTHONWRAP_API PyObject * EP_Wrap_PyDict_GetItemString(PyObject *p, const char *key);
    PYTHONWRAP_API const char * EP_Wrap_PyUnicode_AsUTF8(PyObject *unicode);
    PYTHONWRAP_API PyObject * EP_Wrap_PyUnicode_AsUTF8String(PyObject *unicode);
    PYTHONWRAP_API int EP_Wrap_PyCallable_Check(PyObject *o);
    PYTHONWRAP_API PyObject * EP_Wrap_PyObject_CallObject(PyObject *callable, PyObject *args);
    PYTHONWRAP_API PyObject * EP_Wrap_PyObject_GetAttrString(PyObject *o, const char *attr_name);
    PYTHONWRAP_API PyObject * EP_Wrap_PyObject_CallFunction(PyObject *callable, const char *format);
    PYTHONWRAP_API PyObject * EP_Wrap_PyObject_CallFunction3Args(PyObject *callable, const char *format, PyObject * arg1, PyObject * arg2, PyObject * arg3);
    PYTHONWRAP_API PyObject * EP_Wrap_PyObject_CallMethod(PyObject *obj, const char *name, const char *format);
    PYTHONWRAP_API int EP_Wrap_PyList_Check(PyObject *p);
    PYTHONWRAP_API unsigned long EP_Wrap_PyList_Size(PyObject *list);
    PYTHONWRAP_API PyObject * EP_Wrap_PyList_GetItem(PyObject *list, Py_ssize_t index);
    PYTHONWRAP_API int EP_Wrap_PyUnicode_Check(PyObject *o);
    PYTHONWRAP_API int EP_Wrap_PyLong_Check(PyObject *p);
    PYTHONWRAP_API long EP_Wrap_PyLong_AsLong(PyObject *obj);
    PYTHONWRAP_API void EP_Wrap_Py_SetPath(const wchar_t *);
    PYTHONWRAP_API void EP_Wrap_Py_SetPythonHome(const wchar_t *home);
};


#endif // EPLUS_PYTHON_LIB_WRAPPER_HH
