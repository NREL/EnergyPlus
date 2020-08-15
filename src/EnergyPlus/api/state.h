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

#ifndef EnergyPlusAPIState_h_INCLUDED
#define EnergyPlusAPIState_h_INCLUDED

#include <EnergyPlus/api/EnergyPlusAPI.h>


#ifdef __cplusplus
extern "C" {
#endif

/// \brief This is typedef for an instance that stores the "state" of an EnergyPlus simulation.
/// \details The state of an EnergyPlus simulation is held heavily in the global program state.  To alleviate issues
///          with this design in the context of new applications and workflows, the global program state is being
///          refactored into a single variable which will ultimately own and manage the entire state of the simulation.
///          In order to keep track of the simulation during API calls, a reference to a state must be kept and
///          managed by the API client.  The client must call `stateNew` to create a new state instance, and pass this
///          to any functions that need a state instance.  Once a simulation is complete, the client can call
///          `stateReset`, which will reset the state instance passed in, along with any global variables that are not
///          yet refactored into this state instance.  Once complete, the client has the ability to free the memory of
///          the state by using the `stateDelete` function..
typedef void * EnergyPlusState;

/// \brief Creates a new simulation state instance and returns it for the client to store while running simulations
/// \details This function creates a new instance that is used in running simulations from the API.  The state created
///          in this function is used by passing it into specific run functions.  When a simulation is complete, the
///          state can be reset using the `stateReset` function, or deleted completely with the `stateDelete` function.
/// \see EnergyPlusState
/// \see stateReset
/// \see stateDelete
ENERGYPLUSLIB_API EnergyPlusState stateNew();
/// \brief Resets the simulation state of EnergyPlus
/// \details A simulation state is created by calling the `stateNew` function.  After a simulation is complete, if a
///          second is to be run using the same memory space, the simulation state must be cleared with this function,
///          or unexpected errors will occur.  Once the client is fully finished with the state, it can be deleted
///          entirely with `stateDelete`.
/// \param[in] state The simulation state of the simulation to clear.
/// \remark This function will also clear any callback functions, so callback functions must be registered again.
ENERGYPLUSLIB_API void stateReset(EnergyPlusState state);
/// \brief Deletes a simulation state instance once the client is fully finished with it.
/// \details A simulation state is created by calling the `stateNew` function.  After the client is finished, the memory
///          can be reset by calling the `stateReset` function, or released entirely by calling this delete function.
/// \see EnergyPlusState
/// \see stateNew
/// \see stateReset
ENERGYPLUSLIB_API void stateDelete(EnergyPlusState state);

#ifdef __cplusplus
}
#endif

#endif
