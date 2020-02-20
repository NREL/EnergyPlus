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

#ifndef ENERGYPLUS_COILS_COILCOOLINGDX
#define ENERGYPLUS_COILS_COILCOOLINGDX

#include <string>
#include <vector>

#include <EnergyPlus/Coils/CoilCoolingDXCurveFitPerformance.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {

struct CoilCoolingDXInputSpecification
{
    std::string name;
    std::string evaporator_inlet_node_name;
    std::string evaporator_outlet_node_name;
    std::string availability_schedule_name;
    std::string condenser_zone_name;
    std::string condenser_inlet_node_name;
    std::string condenser_outlet_node_name;
    std::string performance_object_name;
    std::string condensate_collection_water_storage_tank_name;
    std::string evaporative_condenser_supply_water_storage_tank_name;
};

struct CoilCoolingDX
{
    CoilCoolingDX() = default;
    static int factory(std::string const &coilName);
    static void getInput();
    static void clear_state();
    void instantiateFromInputSpec(const CoilCoolingDXInputSpecification &input_data);
    void oneTimeInit();
    void simulate(bool useAlternateMode, Real64 PLR, int speedNum, Real64 speedRatio, int fanOpMode);
    void setData(int fanIndex, int fanType, std::string const &fanName, int airLoopNum);
    void getFixedData(int &evapInletNodeIndex,
                      int &evapOutletNodeIndex,
                      int &condInletNodeIndex,
                      int &normalModeNumSpeeds,
                      CoilCoolingDXCurveFitPerformance::CapControlMethod &capacityControlMethod,
                      Real64 &minOutdoorDryBulb);
    void getDataAfterSizing(Real64 &normalModeRatedEvapAirFlowRate,
                            Real64 &normalModeRatedCapacity,
                            std::vector<Real64> &normalModeFlowRates,
                            std::vector<Real64> &normalModeRatedCapacities);
    static void inline passThroughNodeData(DataLoopNode::NodeData &in, DataLoopNode::NodeData &out);
    void size();

    CoilCoolingDXInputSpecification original_input_specs;
    std::string name;
    bool myOneTimeInitFlag = true;
    int evapInletNodeIndex = 0;
    int evapOutletNodeIndex = 0;
    int availScheduleIndex = 0;
    // int condZoneIndex = 0;
    int condInletNodeIndex = 0;
    int condOutletNodeIndex = 0;
    CoilCoolingDXCurveFitPerformance performance;
    int condensateTankIndex = 0;
    int condensateTankSupplyARRID = 0;
    Real64 condensateVolumeFlow = 0.0;
    Real64 condensateVolumeConsumption = 0.0;
    int evaporativeCondSupplyTankIndex = 0;
    int evaporativeCondSupplyTankARRID = 0;
    Real64 evaporativeCondSupplyTankVolumeFlow = 0.0;
    // Real64 evaporativeCondSupplyTankVolumeConsumption = 0.0;
    Real64 evapCondPumpElecPower = 0.0;
    Real64 evapCondPumpElecConsumption = 0.0;
    int airLoopNum = 0; // Add for AFN compatibility, revisit at a later date
    int supplyFanIndex = 0;
    int supplyFanType = 0;
    std::string supplyFanName = "";
    bool doStandardRatingFlag = true;

    // report variables
    Real64 totalCoolingEnergyRate = 0.0;
    Real64 totalCoolingEnergy = 0.0;
    Real64 sensCoolingEnergyRate = 0.0;
    Real64 sensCoolingEnergy = 0.0;
    Real64 latCoolingEnergyRate = 0.0;
    Real64 latCoolingEnergy = 0.0;

    Real64 coolingCoilRuntimeFraction = 0.0;
    Real64 elecCoolingPower = 0.0;
    Real64 elecCoolingConsumption = 0.0;

    Real64 airMassFlowRate = 0.0;
    Real64 inletAirDryBulbTemp = 0.0;
    Real64 inletAirHumRat = 0.0;
    Real64 outletAirDryBulbTemp = 0.0;
    Real64 outletAirHumRat = 0.0;
    Real64 partLoadRatioReport = 0.0;
    Real64 runTimeFraction = 0.0;
    int speedNumReport = 0;
    Real64 speedRatioReport = 0.0;
    Real64 wasteHeatEnergyRate = 0.0;
    Real64 wasteHeatEnergy = 0.0;
};

extern std::vector<CoilCoolingDX> coilCoolingDXs;
extern bool coilCoolingDXGetInputFlag;
extern std::string const coilCoolingDXObjectName;

} // namespace EnergyPlus

#endif // ENERGYPLUS_COILS_COILCOOLINGDX
