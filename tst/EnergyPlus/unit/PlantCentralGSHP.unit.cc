// EnergyPlus, Copyright (c) 1996-2018, The Board of Trustees of the University of Illinois,
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

// EnergyPlus::PlantCentralGSHP Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/PlantCentralGSHP.hh>
#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/FluidProperties.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;

TEST_F(EnergyPlusFixture, ChillerHeater_Autosize)
{

    // Allocate One Wrapper which One module (=distinct ChillerHeaterPerformance:Electric:EIR)
    // but with a number of identical number module of 2 in CentralHeatPumpSystem
    int NumWrappers = 1;
    PlantCentralGSHP::NumWrappers = NumWrappers;
    PlantCentralGSHP::Wrapper.allocate(NumWrappers);
    PlantCentralGSHP::WrapperReport.allocate(NumWrappers);

    int NumberOfComp = 1;
    PlantCentralGSHP::Wrapper(1).NumOfComp = NumberOfComp;
    PlantCentralGSHP::Wrapper(1).WrapperComp.allocate(NumberOfComp);

    PlantCentralGSHP::Wrapper(1).WrapperComp(1).WrapperPerformanceObjectType = "CHILLERHEATERPERFORMANCE:ELECTRIC:EIR";
    PlantCentralGSHP::Wrapper(1).WrapperComp(1).WrapperIdenticalObjectNum = 2;
    PlantCentralGSHP::Wrapper(1).WrapperComp(1).CHSchedPtr = DataGlobals::ScheduleAlwaysOn;
    PlantCentralGSHP::Wrapper(1).ChillerHeaterNums = 2;
    PlantCentralGSHP::Wrapper(1).ChillerHeater.allocate(2);
    PlantCentralGSHP::Wrapper(1).ChillerHeaterReport.allocate(2);
    // First test in SizeWrapper, so need to set that
    PlantCentralGSHP::Wrapper(1).ControlMode = PlantCentralGSHP::SmartMixing;

    int NumChillerHeaters = 1;
    PlantCentralGSHP::NumChillerHeaters = NumChillerHeaters;
    PlantCentralGSHP::ChillerHeater.allocate(NumChillerHeaters);
    PlantCentralGSHP::ChillerHeaterReport.allocate(NumChillerHeaters);
    PlantCentralGSHP::ChillerHeater(1).ConstantFlow = false;
    PlantCentralGSHP::ChillerHeater(1).VariableFlow = true;
    PlantCentralGSHP::ChillerHeater(1).CondenserType = PlantCentralGSHP::WaterCooled;

    PlantCentralGSHP::ChillerHeater(1).SizFac = 1.2;

    PlantCentralGSHP::ChillerHeater(1).RefCapCooling = DataSizing::AutoSize;
    PlantCentralGSHP::ChillerHeater(1).RefCapCoolingWasAutoSized = true;

    PlantCentralGSHP::ChillerHeater(1).EvapVolFlowRate = DataSizing::AutoSize;
    PlantCentralGSHP::ChillerHeater(1).EvapVolFlowRateWasAutoSized = true;

    PlantCentralGSHP::ChillerHeater(1).CondVolFlowRate = DataSizing::AutoSize;
    PlantCentralGSHP::ChillerHeater(1).CondVolFlowRateWasAutoSized = true;

    // Needed for calcs
    PlantCentralGSHP::ChillerHeater(1).RefCOPCooling =  1.5;
    PlantCentralGSHP::ChillerHeater(1).OpenMotorEff =  0.98;
    PlantCentralGSHP::ChillerHeater(1).TempRefCondIn = 29.4;
    PlantCentralGSHP::ChillerHeater(1).ClgHtgToCoolingCapRatio = 0.74;
    PlantCentralGSHP::ChillerHeater(1).ClgHtgtoCogPowerRatio = 1.38;

    // Add the References onto the wrapper
    PlantCentralGSHP::Wrapper(1).ChillerHeater(1) = PlantCentralGSHP::ChillerHeater(1);
    PlantCentralGSHP::Wrapper(1).ChillerHeater(2) = PlantCentralGSHP::ChillerHeater(1);
    PlantCentralGSHP::Wrapper(1).ChillerHeaterReport(1) = PlantCentralGSHP::ChillerHeaterReport(1);
    PlantCentralGSHP::Wrapper(1).ChillerHeaterReport(2) = PlantCentralGSHP::ChillerHeaterReport(1);

    // Assign a Chilled Water loop to the wrapper
    PlantCentralGSHP::Wrapper(1).CWLoopNum = 1;
    // Assign a Condenser Water loop to the wrapper... Instead of creating another one, we use the same for testing
    PlantCentralGSHP::Wrapper(1).GLHELoopNum = 1;

    DataPlant::PlantLoop.allocate(1);
    DataSizing::PlantSizData.allocate(1);
    // Hot Water Loop
    DataPlant::PlantLoop(1).PlantSizNum = 1;
    DataPlant::PlantLoop(1).FluidIndex = 1;
    DataPlant::PlantLoop(1).FluidName = "WATER";
    DataSizing::PlantSizData(1).DesVolFlowRate = 1.0;
    DataSizing::PlantSizData(1).DeltaT = 10.0;
    DataPlant::PlantFirstSizesOkayToFinalize = true;


    // Calculate expected values
    Real64 rho = FluidProperties::GetDensityGlycol("WATER",
                                                   DataGlobals::CWInitConvTemp,
                                                   DataPlant::PlantLoop(1).FluidIndex,
                                                   "ChillerHeater_Autosize_TEST");
    Real64 Cp_evap = FluidProperties::GetSpecificHeatGlycol("WATER", DataGlobals::CWInitConvTemp,
                                                            DataPlant::PlantLoop(1).FluidIndex,
                                                            "ChillerHeater_Autosize_TEST");
    Real64 Cp_cond = FluidProperties::GetSpecificHeatGlycol("WATER", PlantCentralGSHP::ChillerHeater(1).TempRefCondIn,
                                                            DataPlant::PlantLoop(1).FluidIndex,
                                                            "ChillerHeater_Autosize_TEST");

    // Note: Each individual chiller heater module is sized to be capable of supporting the total load on the wrapper

    // Flow is multiplied by the SizFac
    Real64 EvapVolFlowRateExpected = DataSizing::PlantSizData(1).DesVolFlowRate * PlantCentralGSHP::ChillerHeater(1).SizFac;

    Real64 RefCapCoolingExpected = rho * Cp_evap * EvapVolFlowRateExpected *  DataSizing::PlantSizData(1).DeltaT;
    Real64 CondVolFlowRateExpected = RefCapCoolingExpected *
        (1.0 + (1.0 / PlantCentralGSHP::ChillerHeater(1).RefCOPCooling) * PlantCentralGSHP::ChillerHeater(1).OpenMotorEff) /
        (rho * Cp_cond * EvapVolFlowRateExpected *  DataSizing::PlantSizData(1).DeltaT);


    // now call sizing routine
    PlantCentralGSHP::SizeWrapper(1);

    EXPECT_DOUBLE_EQ(EvapVolFlowRateExpected, PlantCentralGSHP::ChillerHeater(1).EvapVolFlowRate);
    EXPECT_DOUBLE_EQ(RefCapCoolingExpected, PlantCentralGSHP::ChillerHeater(1).RefCapCooling);
    EXPECT_DOUBLE_EQ(CondVolFlowRateExpected, PlantCentralGSHP::ChillerHeater(1).CondVolFlowRate);

    // Ensure that stuff that depends on RefCapCooling are also initialized properly
    // Heating Cap
    Real64 RefCapClgHtgExpected = RefCapCoolingExpected * PlantCentralGSHP::ChillerHeater(1).ClgHtgToCoolingCapRatio;
    EXPECT_DOUBLE_EQ(RefCapClgHtgExpected, PlantCentralGSHP::ChillerHeater(1).RefCapClgHtg);

    // Heating Power: Calc cooling Power = Cap / COP, and multiply by ratio
    Real64 RefPowerClgHtgExpected = (RefCapCoolingExpected / PlantCentralGSHP::ChillerHeater(1).RefCOPCooling)
                                  * PlantCentralGSHP::ChillerHeater(1).ClgHtgtoCogPowerRatio;
    EXPECT_DOUBLE_EQ(RefPowerClgHtgExpected, PlantCentralGSHP::ChillerHeater(1).RefPowerClgHtg);

    // Heating COP = Heating Cap / Heating Power
    Real64 RefCOPClgHtgExpected = RefCapClgHtgExpected * RefPowerClgHtgExpected;
    EXPECT_DOUBLE_EQ(RefCOPClgHtgExpected, PlantCentralGSHP::ChillerHeater(1).RefCOPClgHtg);
}





