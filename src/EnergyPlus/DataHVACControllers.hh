// EnergyPlus, Copyright (c) 1996-2021, The Board of Trustees of the University of Illinois,
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

#ifndef DataHVACControllers_hh_INCLUDED
#define DataHVACControllers_hh_INCLUDED

// ObjexxFCL Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus/Data/BaseData.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

namespace DataHVACControllers {

    int constexpr ControllerSimple_Type(1);

    // Controller action used in modules HVACControllers and ZoneControllers
    int constexpr iNoAction(0);
    int constexpr iReverseAction(1);
    int constexpr iNormalAction(2);

    // Controller mode used in modules HVACControllers and ZoneControllers
    int constexpr iModeWrongAction(-2); // Controller error. E.g., bad action
    int constexpr iModeNone(-1);        // Controller mode not yet determined
    int constexpr iModeOff(0);          // Controller off (no air flow in loop)
    int constexpr iModeInactive(1);     // Controller inactive (equip not available for current step)
    int constexpr iModeActive(2);       // Controller active (schedule>0 and min<actuated<max)
    int constexpr iModeMinActive(3);    // Controller active and min-constrained (equip available and actuated=min)
    int constexpr iModeMaxActive(4);    // Controller active and max-constrained (equip available and actuated=max)

    int constexpr iFirstMode(iModeWrongAction); // First operating mode in range
    int constexpr iLastMode(iModeMaxActive);    // Last operating mode in range

    // Controller operation used in module HVACControllers
    int constexpr iControllerOpColdStart(1);   // Reset for cold start
    int constexpr iControllerOpWarmRestart(2); // Reset for warm restart with previous solution
    int constexpr iControllerOpIterate(3);     // Check convergence and estimate next guess if needed
    int constexpr iControllerOpEnd(4);         // Check convergence only and trace

    // Controller restart flag used in module HVACControllers
    int constexpr iControllerWarmRestartNone(-1);   // Indicates that warm restart was not attempted
    int constexpr iControllerWarmRestartFail(0);    // Indicates that warm restart failed
    int constexpr iControllerWarmRestartSuccess(1); // Indicates that warm restart was successful

} // namespace DataHVACControllers

struct HVACCtrlData : BaseGlobalStruct
{

    Array1D_string const ControllerTypes = Array1D_string(1, std::string("Controller:WaterCoil"));
    Array1D_string const ActionTypes = Array1D_string({0, 2}, {"No action", "Reverse action", "Normal action"});
    Array1D_string const ControllerModeTypes = Array1D_string({-2, 4},
                                                              {"Wrong action mode",
                                                               "No controller mode",
                                                               "Off controller mode",
                                                               "Inactive controller mode",
                                                               "Active unconstrained controller mode",
                                                               "Active min-constrained controller mode",
                                                               "Active max-constrained controller mode"});

    void clear_state() override
    {
        // nothing to clear, it's all constant
    }
};

} // namespace EnergyPlus

#endif
