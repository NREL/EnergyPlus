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

// C++ Headers
#include <algorithm>

// EnergyPlus Headers
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/Plant/PlantLocation.hh>

namespace EnergyPlus {

namespace PlantLoopEquip {

    // MODULE INFORMATION:
    //       AUTHOR         Sankaranarayanan K P
    //       DATE WRITTEN   July 2005
    //       MODIFIED       na
    //       RE-ENGINEERED  na

    void SimPlantEquip(int const LoopNum,     // loop counter
                       int const LoopSideNum, // loop counter
                       int const BranchNum,
                       int const Num,
                       bool const FirstHVACIteration, // TRUE if First iteration of simulation
                       bool &InitLoopEquip,
                       bool const GetCompSizFac // Tells component routine to return the component sizing fraction
    )
    {

        // SUBROUTINE INFORMATION:
        //       AUTHOR         Dan Fisher
        //       DATE WRITTEN   July 1998
        //       MODIFIED       June 2000  -Brandon Anderson
        //                             Changed to Group Similar Components.  Components will
        //                         be defined by ComponentType:SpecificComponent.
        //                         The colon will act as the type delimeter, So all
        //                         components of one type will be grouped. ex.(Boilers,Chillers)
        //                       May 2003 - Simon Rees
        //                         Added initial loop to force free cooling chiller etc to be
        //                         simulated before other components.
        //       RE-ENGINEERED  na

        // PURPOSE OF THIS SUBROUTINE:
        // This subroutine calls the appropriate routines to simulate
        // the equipment on the plant.

        // METHODOLOGY EMPLOYED:
        // This subroutine employs a rule-based
        // scheme to operate the plant equipment simulation without
        // requiring a detailed flow network solver.  The scheme is based
        // on several restrictive assumptions which may be relaxed when
        // a more detailed solution technique is developed.  The current
        // assumptions are:
        //    1.   All loop cooling/heating equipment is connected
        //         in parallel.
        //    2.   Only one circulation pump may be specified per loop.
        //    3.   The circulation pump must be specified first in the
        //         simulation order and is assumed to be connected in
        //         series with the cooling/heating equipment.
        //    4.   The Circ. pump determines the maximum flow rate for
        //         the loop.
        // The scheme is valid only for Part Load based plant equipment
        // models (currently the only type implemented).  Each equipment
        // simulation updates its outlet node temperature, estimates its
        // flow rate and returns a remaining loop demand which is passed
        // on to the other available equipment.

        // NOTE: All Equipment return the index of their lists during "InitLoopEquip"
        // as a time reduction measure.  Specific ifs are set to catch those modules that don't.
        // If you add a module or new equipment type, you must set up this structure.

        // SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        EnergyPlus::PlantLocation sim_component_location(LoopNum, LoopSideNum, BranchNum, Num);
        auto &sim_component(DataPlant::PlantLoop(LoopNum).LoopSide(LoopSideNum).Branch(BranchNum).Comp(Num));
        static std::vector<int> compsToSimAfterInitLoopEquip = {DataPlant::TypeOf_Pipe, DataPlant::TypeOf_PipeSteam, DataPlant::TypeOf_SolarCollectorICS, DataPlant::TypeOf_SolarCollectorFlatPlate};
        if (sim_component.compPtr != nullptr) {
            if (InitLoopEquip) {
                sim_component.compPtr->onInitLoopEquip(sim_component_location);
                sim_component.compPtr->getDesignCapacities(
                    sim_component_location, sim_component.MaxLoad, sim_component.MinLoad, sim_component.OptLoad);
                sim_component.compPtr->getDesignTemperatures(sim_component.TempDesCondIn, sim_component.TempDesEvapOut);

                if (GetCompSizFac) {
                    sim_component.compPtr->getSizingFactor(sim_component.SizFac);
                }

                // KLUGEY HACK ALERT!!!
                // Some components before transition were never checking InitLoopEquip, and each call to SimXYZ would actually just pass through the
                // calculation Other components, on the other hand, would check InitLoopEquip, do a few things, then exit early without doing any
                // calculation This may be wrong...but during this transition, it would be very nice to keep no diffs Thus, I will return here for all
                // components that actually returned after their onInitLoopEquip stuff
                //   and I will fall through and actually call simulate on the components that did that before
                // I anticipate the list of components that fall through to be very small, so that is the check I will do.
                // If std::find returns the .end() iterator, that means it didn't find it in the list, which means it's not one of the ones to fall
                // through, so RETURN
                if (std::find(compsToSimAfterInitLoopEquip.begin(), compsToSimAfterInitLoopEquip.end(), sim_component.TypeOf_Num) ==
                    compsToSimAfterInitLoopEquip.end()) {
                    return;
                }
            }
            sim_component.compPtr->simulate(sim_component_location, FirstHVACIteration, sim_component.MyLoad, sim_component.ON);
        }
    }

} // namespace PlantLoopEquip

} // namespace EnergyPlus
