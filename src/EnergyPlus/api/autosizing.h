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
/// \details This should be described further
/// \see sizerNew
ENERGYPLUSLIB_API typedef void * Sizer;
/// \brief Returns a new reference to a Sizer class
/// \details The sizer class allows access to sizer stuff.
/// \param[in] temperature The fluid temperature for property evaluation, in degrees Celsius.
/// \returns This functions allocates a new sizer class and returns a Sizer, which is a pointer to that new instance.
/// \remark In API applications, when the calling script is done with the sizer instance, call `sizerDelete` to clean up the instance.
/// \see Sizer
/// \see sizerDelete
ENERGYPLUSLIB_API Sizer sizerNew(Real64 temperature);
/// \brief Deletes an instance of a Sizer class
/// \details When an instance of a Sizer class is created using `sizerNew`, it should be cleaned up when totally done with it.
/// \param[in] sizer An instance of a Sizer class to be deleted.  The Sizer class is initially created by calling `sizerNew`.
/// \see Sizer
/// \see sizerNew
ENERGYPLUSLIB_API void sizerDelete(Sizer sizer);
/// \brief Does calculation of the sizer
/// \param[in] sizer An instance of a Sizer class, which can be created by calling `sizerNew`.
/// \returns This function returns true (0) if the autosizing calculation was successful, or false (1) if not.
/// \see Sizer
/// \see sizerNew
ENERGYPLUSLIB_API int sizerCalculate(Sizer sizer);
/// \brief Returns the resulting autosized value after sizerCalculate() is called.
/// \param[in] sizer An instance of a Sizer class, which can be created by calling `sizerNew`.
/// \returns Autosized value, in J/kgK
/// \see Sizer
/// \see sizerNew
/// \see sizerCalculate
ENERGYPLUSLIB_API Real64 sizerValue(Sizer sizer);

#ifdef __cplusplus
}
#endif

#endif //ENERGYPLUS_AUTOSIZING_H
