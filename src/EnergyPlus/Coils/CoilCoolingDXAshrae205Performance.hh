// EnergyPlus, Copyright (c) 1996-2023, The Board of Trustees of the University of Illinois,
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

#ifndef ENERGYPLUS_COILS_COIL_COOLING_DX_ASHRAE205_PERFORMANCE
#define ENERGYPLUS_COILS_COIL_COOLING_DX_ASHRAE205_PERFORMANCE

#include <string>
#include <vector>

#include "rs0004.h"
#include <EnergyPlus/Coils/CoilCoolingDXPerformanceBase.hh>
#include <EnergyPlus/DataGlobalConstants.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

// Forward declarations
struct EnergyPlusData;

struct CoilCoolingDX205Performance : public CoilCoolingDXPerformanceBase
{
    CoilCoolingDX205Performance(EnergyPlus::EnergyPlusData &state,
                                const std::string &name_to_find);

    static constexpr std::string_view object_name = "Coil:DX:ASHRAE205:Performance";

    std::shared_ptr<tk205::rs0004_ns::RS0004> representation; // ASHRAE205 representation instance
    std::pair<EnergyPlusData *, std::string> logger_context;
    Btwxt::InterpolationMethod interpolation_type{Btwxt::InterpolationMethod::linear};
    Real64 rated_total_cooling_capacity;
    Real64 rated_steady_state_heating_capacity;

    int NumSpeeds() override
    {
        return representation->performance.performance_map_cooling.grid_variables.compressor_sequence_number.size();
    }

    Real64 RatedTotalCapacityAtSpeed(EnergyPlusData &, int) override;

    Real64 RatedGrossTotalCap() override
    {
        return rated_total_cooling_capacity;
    }

    Real64 calculate_rated_capacity(EnergyPlus::EnergyPlusData &state, int speed);

    void size(EnergyPlusData &state) override;

    void simulate(EnergyPlusData &state,
                  const DataLoopNode::NodeData &inletNode,
                  DataLoopNode::NodeData &outletNode,
                  HVAC::CoilMode mode,
                  // Real64 &PLR,
                  int speedNum,
                  Real64 speedRatio,
                  HVAC::FanOp const fanOpMode,
                  DataLoopNode::NodeData &condInletNode,
                  DataLoopNode::NodeData &condOutletNode,
                  bool const singleMode,
                  Real64 LoadSHR = 0.0) override;

    void calculate_cycling_capcacity(EnergyPlus::EnergyPlusData &state,
                                     const DataLoopNode::NodeData &inletNode,
                                     DataLoopNode::NodeData &outletNode,
                                     Real64 const gross_power,
                                     Real64 const ratio,
                                     HVAC::FanOp const fanOpMode);

    void calculate(EnergyPlusData &state,
                   const DataLoopNode::NodeData &inletNode,
                   DataLoopNode::NodeData &outletNode,
                   // Real64 &PLR,
                   int speedNum,
                   Real64 speedRatio,
                   HVAC::FanOp fanOpMode,
                   DataLoopNode::NodeData &condInletNode,
                   DataLoopNode::NodeData &);

private:
    void calculate_output_nodes(EnergyPlusData &state,
                                const DataLoopNode::NodeData &inletNode,
                                DataLoopNode::NodeData &outletNode,
                                Real64 gross_total_capacity,
                                Real64 gross_sensible_capacity,
                                Real64 air_mass_flow_rate);
};
} // namespace EnergyPlus
#endif // ENERGYPLUS_COILS_COIL_COOLING_DX_ASHRAE205_PERFORMANCE
