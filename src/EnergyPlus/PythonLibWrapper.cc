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
    PYTHONWRAP_API const char * EP_Wrap_Py_GetVersion() {return Py_GetVersion();}
    PYTHONWRAP_API wchar_t * EP_Wrap_Py_DecodeLocale(const char * s, size_t * size) {return Py_DecodeLocale(s, size);}
    PYTHONWRAP_API void EP_Wrap_Py_InitializeEx(int i) {Py_InitializeEx(i);}
    PYTHONWRAP_API int EP_Wrap_PyRun_SimpleString(const char * c) {return PyRun_SimpleString(c);}
    PYTHONWRAP_API int EP_Wrap_Py_FinalizeEx() {return Py_FinalizeEx();}
    PYTHONWRAP_API void EP_Wrap_PyErr_Fetch(PyObject **a, PyObject **b, PyObject **c) {PyErr_Fetch(a, b, c);}
    PYTHONWRAP_API void EP_Wrap_PyErr_NormalizeException(PyObject **a, PyObject **b, PyObject **c) {PyErr_NormalizeException(a, b, c);}
    PYTHONWRAP_API PyObject * EP_Wrap_PyObject_Repr(PyObject * o) {return PyObject_Repr(o);}
    PYTHONWRAP_API PyObject * EP_Wrap_PyUnicode_AsEncodedString(PyObject *a, const char *b, const char *c) {return PyUnicode_AsEncodedString(a, b, c);}
    PYTHONWRAP_API char * EP_Wrap_PyBytes_AsString(PyObject *o) {return PyBytes_AsString(o);}
    PYTHONWRAP_API PyObject * EP_Wrap_PyUnicode_DecodeFSDefault(const char *s) {return PyUnicode_DecodeFSDefault(s);}
    PYTHONWRAP_API PyObject * EP_Wrap_PyImport_Import(PyObject *name) {return PyImport_Import(name);}
    PYTHONWRAP_API void EP_Wrap_Py_DECREF(PyObject *o) {Py_DECREF(o);}
    PYTHONWRAP_API PyObject * EP_Wrap_PyErr_Occurred() {return PyErr_Occurred();}
    PYTHONWRAP_API PyObject * EP_Wrap_PyModule_GetDict(PyObject *module) {return PyModule_GetDict(module);}
    PYTHONWRAP_API PyObject * EP_Wrap_PyDict_GetItemString(PyObject *p, const char *key) {return PyDict_GetItemString(p, key);}
    PYTHONWRAP_API const char * EP_Wrap_PyUnicode_AsUTF8(PyObject *unicode) {return PyUnicode_AsUTF8(unicode);}
    PYTHONWRAP_API PyObject * EP_Wrap_PyUnicode_AsUTF8String(PyObject *unicode) {return PyUnicode_AsUTF8String(unicode);}
    PYTHONWRAP_API int EP_Wrap_PyCallable_Check(PyObject *o) {return PyCallable_Check(o);}
    PYTHONWRAP_API PyObject * EP_Wrap_PyObject_CallObject(PyObject *callable, PyObject *args) {return PyObject_CallObject(callable, args);}
    PYTHONWRAP_API PyObject * EP_Wrap_PyObject_GetAttrString(PyObject *o, const char *attr_name) {return PyObject_GetAttrString(o, attr_name);}
    PYTHONWRAP_API PyObject * EP_Wrap_PyObject_CallFunction(PyObject *callable, const char *format) {return PyObject_CallFunction(callable, format);}
    PYTHONWRAP_API PyObject * EP_Wrap_PyObject_CallFunction3Args(PyObject *callable, const char *format, PyObject * arg1, PyObject * arg2, PyObject * arg3) {return PyObject_CallFunction(callable, format, arg1, arg2, arg3);}
    PYTHONWRAP_API PyObject * EP_Wrap_PyObject_CallMethod(PyObject *obj, const char *name, const char *format) {return PyObject_CallMethod(obj, name, format);}
    PYTHONWRAP_API PyObject * EP_Wrap_PyObject_CallMethod1Arg(PyObject *p, const char *name, const char *format, PyObject *arg1) {return PyObject_CallMethod(p, name, format, arg1);}
    PYTHONWRAP_API PyObject * EP_Wrap_PyObject_CallMethod2ObjArg(PyObject *p, PyObject *name, PyObject *arg1, PyObject *argNull) {return PyObject_CallMethodObjArgs(p, name, arg1, argNull);}
    PYTHONWRAP_API int EP_Wrap_PyList_Check(PyObject *p) {return PyList_Check(p);} // NOLINT(hicpp-signed-bitwise)
    PYTHONWRAP_API unsigned long EP_Wrap_PyList_Size(PyObject *list) {return PyList_Size(list);}
    PYTHONWRAP_API PyObject * EP_Wrap_PyList_GetItem(PyObject *list, Py_ssize_t index) {return PyList_GetItem(list, index);}
    PYTHONWRAP_API int EP_Wrap_PyUnicode_Check(PyObject *o) {return PyUnicode_Check(o);} // NOLINT(hicpp-signed-bitwise)
    PYTHONWRAP_API int EP_Wrap_PyLong_Check(PyObject *p) {return PyLong_Check(p);} // NOLINT(hicpp-signed-bitwise)
    PYTHONWRAP_API long EP_Wrap_PyLong_AsLong(PyObject *obj) {return PyLong_AsLong(obj);}
    PYTHONWRAP_API void EP_Wrap_Py_SetPath(const wchar_t *t) {return Py_SetPath(t);}
    PYTHONWRAP_API void EP_Wrap_Py_SetPythonHome(wchar_t *home) {return Py_SetPythonHome(home);}
    PYTHONWRAP_API PyObject * EP_Wrap_Py_BuildValue(const char *format) {return Py_BuildValue(format);}
    PYTHONWRAP_API PyObject * EP_Wrap_PyLong_FromVoidPtr(void *v) {return PyLong_FromVoidPtr(v);}
    PYTHONWRAP_API PyObject * EP_Wrap_PyString_FromString(const char *s) {return PyUnicode_FromString(s);}
}
