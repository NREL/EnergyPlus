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

#ifndef ENERGYPLUS_AUTOSIZING_H
#define ENERGYPLUS_AUTOSIZING_H

#include <EnergyPlus/api/TypeDefs.h>
#include <EnergyPlus/api/EnergyPlusAPI.h>

#ifdef __cplusplus
extern "C" {
#endif

/// \file autosizing.h
/// \brief This "Auto-sizing" API category provides access to auto-sizing calculations inside EnergyPlus.
/// \details Auto-sizing is an important aspect of an EnergyPlus simulation, allowing users to gather simulation results
///          for cases where the actual size of components and systems are not known ahead of time.

/// \brief This typedef is a convenient pointer to an internal sizer class.
/// \details Just one is declared here as any sizer object can fit in it.  An instance can be found by calling any
///          sizer***New() function
ENERGYPLUSLIB_API typedef void * Sizer;

/// \brief Returns a new reference to a HeatingAirflowUA Sizer class
/// \details The sizer class allows access to sizer stuff.
/// \returns This functions allocates a new sizer class and returns a Sizer, which is a pointer to that new instance.
/// \remark In API applications, when the calling script is done with the sizer instance, call `sizerHeatingAirflowUADelete` to clean up the instance.
/// \see Sizer
/// \see sizerHeatingAirflowUADelete
ENERGYPLUSLIB_API Sizer sizerHeatingAirflowUANew();
/// \brief Deletes an instance of a HeatingAirflowUA Sizer class
/// \details When an instance of a Sizer class is created using `sizerHeatingAirflowUANew`, it should be cleaned up when totally done with it.
/// \param[in] sizer An instance of a Sizer class to be deleted.  The Sizer class is initially created by calling `sizerHeatingAirflowUANew`.
/// \see Sizer
/// \see sizerHeatingAirflowUANew
ENERGYPLUSLIB_API void sizerHeatingAirflowUADelete(Sizer sizer);

// TODO: These need work.  Need to decide if they should be more/less specific, and we need to test all of them
// TODO: Also, some of the code paths appear to want a mass flow rate while others want a volume flow rate, need to verify that

/// \brief Initializes the HeatingAirflowUA sizer class
/// \details All required data for setting up the HeatingAirflowUA sizer is passed in as arguments to this function.
/// \param[in] sizer An instance of a Sizer class, which can be created by calling `sizerHeatingAirflowUANew`.
/// \param[in] elevation The elevation above sea level for evaluating fluid properties, in m
/// \remark This must be called prior to each call to sizerHeatingAirflowUACalculate
/// \see Sizer
/// \see sizerHeatingAirflowUACalculate
/// \see sizerHeatingAirflowUADelete

ENERGYPLUSLIB_API void sizerHeatingAirflowUAInitializeForSingleDuctZoneTerminal(Sizer sizer, Real64 elevation, Real64 mainFlowRate);
ENERGYPLUSLIB_API void sizerHeatingAirflowUAInitializeForZoneInductionUnit(Sizer sizer, Real64 elevation, Real64 mainFlowRate, Real64 reheatMultiplier);
ENERGYPLUSLIB_API void sizerHeatingAirflowUAInitializeForZoneFanCoil(Sizer sizer, Real64 elevation, Real64 designHeatVolumeFlowRate);
ENERGYPLUSLIB_API void sizerHeatingAirflowUAInitializeForSystemOutdoorAir(Sizer sizer, Real64 elevation, Real64 overallSystemMassFlowRate, int DOAS);
ENERGYPLUSLIB_API void sizerHeatingAirflowUAInitializeForSystemMainDuct(Sizer sizer, Real64 elevation, Real64 overallSystemVolFlow, Real64 minFlowRateRatio);
ENERGYPLUSLIB_API void sizerHeatingAirflowUAInitializeForSystemCoolingDuct(Sizer sizer, Real64 elevation);
ENERGYPLUSLIB_API void sizerHeatingAirflowUAInitializeForSystemHeatingDuct(Sizer sizer, Real64 elevation);
ENERGYPLUSLIB_API void sizerHeatingAirflowUAInitializeForSystemOtherDuct(Sizer sizer, Real64 elevation);

/// \brief Does calculation of the HeatingAirflowUA sizer
/// \param[in] sizer An instance of a HeatingAirflowUA Sizer class, which can be created by calling `sizerHeatingAirflowUANew`.
/// \returns This function returns true (0) if the autosizing calculation was successful, or false (1) if not.
/// \see Sizer
/// \see sizerHeatingAirflowUANew
ENERGYPLUSLIB_API int sizerHeatingAirflowUACalculate(Sizer sizer);
/// \brief Returns the resulting autosized value after sizerHeatingAirflowUACalculate() is called.
/// \param[in] sizer An instance of a HeatingAirflowUA Sizer class, which can be created by calling `sizerHeatingAirflowUANew`.
/// \returns Autosized Heating Air Mass Flow Rate, in kg/s
/// \see Sizer
/// \see sizerHeatingAirflowUANew
/// \see sizerHeatingAirflowUACalculate
ENERGYPLUSLIB_API Real64 sizerHeatingAirflowUAValue(Sizer sizer);

#ifdef __cplusplus
}
#endif

#endif //ENERGYPLUS_AUTOSIZING_H
