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

#ifndef EnergyPlusAPIDataTransfer_h_INCLUDED
#define EnergyPlusAPIDataTransfer_h_INCLUDED

#include <EnergyPlus/TypeDefs.h>
#include <EnergyPlus/api/EnergyPlusAPI.hh>

#ifdef __cplusplus
extern "C" {
#endif

ENERGYPLUSLIB_API void dataTransferNoOp();
ENERGYPLUSLIB_API void requestVariable(const char* type, const char* key);
ENERGYPLUSLIB_API int getVariableHandle(const char* type, const char* key);
ENERGYPLUSLIB_API int getMeterHandle(const char* meterName);
ENERGYPLUSLIB_API int getActuatorHandle(const char* uniqueKey, const char* componentType, const char* controlType);
ENERGYPLUSLIB_API void resetActuator(int handle);
ENERGYPLUSLIB_API Real64 getVariableValue(int handle);
ENERGYPLUSLIB_API Real64 getMeterValue(int handle);
ENERGYPLUSLIB_API void setActuatorValue(int handle, Real64 value);
ENERGYPLUSLIB_API int getInternalVariableHandle(const char* type, const char* key);
ENERGYPLUSLIB_API Real64 getInternalVariableValue(int handle);

// There are also a few API points for Python Plugins specifically.  It is expected these will only
// be exposed through the plugin base class, not through the regular Python bindings.
ENERGYPLUSLIB_API int getPluginGlobalVariableHandle(const char* name);
ENERGYPLUSLIB_API Real64 getPluginGlobalVariableValue(int handle);
ENERGYPLUSLIB_API void setPluginGlobalVariableValue(int handle, Real64 value);
ENERGYPLUSLIB_API int getPluginTrendVariableHandle(const char* name);
ENERGYPLUSLIB_API Real64 getPluginTrendVariableValue(int handle, int timeIndex);

// Then there are a plethora of specialty one-off EMSVariable values that need to be accessible
// We could certainly add to this list
ENERGYPLUSLIB_API int year();
ENERGYPLUSLIB_API int month();
ENERGYPLUSLIB_API int dayOfMonth();
ENERGYPLUSLIB_API int dayOfWeek();
ENERGYPLUSLIB_API int dayOfYear();
ENERGYPLUSLIB_API int daylightSavingsTimeIndicator();
ENERGYPLUSLIB_API int hour();
ENERGYPLUSLIB_API Real64 currentTime();
ENERGYPLUSLIB_API int minutes();
ENERGYPLUSLIB_API int holidayIndex();
ENERGYPLUSLIB_API int sunIsUp();
ENERGYPLUSLIB_API int isRaining();
ENERGYPLUSLIB_API Real64 systemTimeStep();
ENERGYPLUSLIB_API int currentEnvironmentNum();
ENERGYPLUSLIB_API int warmupFlag();
ENERGYPLUSLIB_API int kindOfSim();
ENERGYPLUSLIB_API int getConstructionHandle(const char* constructionName);

#ifdef __cplusplus
}
#endif

#endif // EnergyPlusAPIDataTransfer_h_INCLUDED
