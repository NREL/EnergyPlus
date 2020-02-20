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

#ifndef ENERGYPLUS_COILS_COILCOOLINGDXCURVEFITPERFORMANCE
#define ENERGYPLUS_COILS_COILCOOLINGDXCURVEFITPERFORMANCE

#include <string>
#include <vector>

#include <EnergyPlus/Coils/CoilCoolingDXCurveFitOperatingMode.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/EnergyPlus.hh>

namespace EnergyPlus {
class OutputFiles;

struct CoilCoolingDXCurveFitPerformanceInputSpecification
{
    std::string name;
    Real64 crankcase_heater_capacity;
    Real64 minimum_outdoor_dry_bulb_temperature_for_compressor_operation;
    Real64 maximum_outdoor_dry_bulb_temperature_for_crankcase_heater_operation;
    Real64 unit_internal_static_air_pressure;
    Real64 basin_heater_capacity;
    Real64 basin_heater_setpoint_temperature;
    std::string basin_heater_operating_shedule_name;
    int compressor_fuel_type;
    std::string base_operating_mode_name;
    std::string alternate_operating_mode_name;
    std::string capacity_control;
};

struct CoilCoolingDXCurveFitPerformance
{
    std::string object_name = "Coil:Cooling:DX:CurveFit:Performance";
    void instantiateFromInputSpec(const CoilCoolingDXCurveFitPerformanceInputSpecification &input_data);
    void simulate(const DataLoopNode::NodeData &inletNode,
                  DataLoopNode::NodeData &outletNode,
                  bool useAlternateMode,
                  Real64 &PLR,
                  int &speedNum,
                  Real64 &speedRatio,
                  int &fanOpMode,
                  DataLoopNode::NodeData &condInletNode,
                  DataLoopNode::NodeData &condOutletNode);

    void calculate(CoilCoolingDXCurveFitOperatingMode &currentMode,
                   const DataLoopNode::NodeData &inletNode,
                   DataLoopNode::NodeData &outletNode,
                   Real64 &PLR,
                   int &speedNum,
                   Real64 &speedRatio,
                   int &fanOpMode,
                   DataLoopNode::NodeData &condInletNode,
                   DataLoopNode::NodeData &condOutletNode);
    void calcStandardRatings(
        int supplyFanIndex, int supplyFanType, std::string const &supplyFanName, int condInletNodeIndex, EnergyPlus::OutputFiles &outputFiles);
    Real64 calcIEERResidual(Real64 const SupplyAirMassFlowRate, std::vector<Real64> const &Par);
    CoilCoolingDXCurveFitPerformanceInputSpecification original_input_specs;
    CoilCoolingDXCurveFitPerformance() = default;
    explicit CoilCoolingDXCurveFitPerformance(const std::string &name);
    void size();

    std::string name;
    Real64 crankcaseHeaterCap = 0.0;
    Real64 crankcaseHeaterPower = 0.0;
    Real64 crankcaseHeaterElectricityConsumption = 0.0;
    Real64 minOutdoorDrybulb = 0.0;
    Real64 maxOutdoorDrybulbForBasin = 0.0;
    Real64 unitStatic = 0.0;
    bool mySizeFlag = true;

    enum CapControlMethod
    {
        CONTINUOUS,
        DISCRETE
    };
    CapControlMethod capControlMethod = CapControlMethod::DISCRETE;

    Real64 evapCondBasinHeatCap = 0.0;
    Real64 evapCondBasinHeatSetpoint = 0.0;
    int evapCondBasinHeatSchedulIndex = 0;
    Real64 basinHeaterElectricityConsumption = 0.0;
    Real64 basinHeaterPower = 0.0;
    Real64 powerUse = 0.0;
    Real64 electricityConsumption = 0.0;
    Real64 RTF = 0.0;
    bool oneTimeEIOHeaderWrite = true;
    Real64 wasteHeatRate = 0.0;

    CoilCoolingDXCurveFitOperatingMode normalMode;
    bool hasAlternateMode = false;
    CoilCoolingDXCurveFitOperatingMode alternateMode; // enhanced dehumidifcation
};

} // namespace EnergyPlus

#endif // ENERGYPLUS_COILS_COILCOOLINGDXCURVEFITPERFORMANCE
