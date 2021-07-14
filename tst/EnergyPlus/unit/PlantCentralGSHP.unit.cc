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

// EnergyPlus::PlantCentralGSHP Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/Data/EnergyPlusData.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/FluidProperties.hh>
#include <EnergyPlus/Plant/DataPlant.hh>
#include <EnergyPlus/PlantCentralGSHP.hh>

using namespace EnergyPlus;

TEST_F(EnergyPlusFixture, ChillerHeater_Autosize)
{
    // Allocate One Wrapper with One module (=distinct ChillerHeaterPerformance:Electric:EIR)
    // but with a number of identical number module of 2 in CentralHeatPumpSystem
    int NumWrappers = 1;
    state->dataPlantCentralGSHP->numWrappers = NumWrappers;
    state->dataPlantCentralGSHP->Wrapper.allocate(NumWrappers);

    int NumberOfComp = 1;
    state->dataPlantCentralGSHP->Wrapper(1).NumOfComp = NumberOfComp;
    state->dataPlantCentralGSHP->Wrapper(1).WrapperComp.allocate(NumberOfComp);

    state->dataPlantCentralGSHP->Wrapper(1).WrapperComp(1).WrapperPerformanceObjectType = "CHILLERHEATERPERFORMANCE:ELECTRIC:EIR";
    state->dataPlantCentralGSHP->Wrapper(1).WrapperComp(1).WrapperIdenticalObjectNum = 2;
    state->dataPlantCentralGSHP->Wrapper(1).WrapperComp(1).CHSchedPtr = DataGlobalConstants::ScheduleAlwaysOn;
    state->dataPlantCentralGSHP->Wrapper(1).ChillerHeaterNums = 2;
    state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater.allocate(2);
    // First test in SizeWrapper, so need to set that
    state->dataPlantCentralGSHP->Wrapper(1).ControlMode = PlantCentralGSHP::iCondType::SmartMixing;

    int NumChillerHeaters = 1;
    state->dataPlantCentralGSHP->numChillerHeaters = NumChillerHeaters;
    state->dataPlantCentralGSHP->ChillerHeater.allocate(NumChillerHeaters);
    state->dataPlantCentralGSHP->ChillerHeater(1).ConstantFlow = false;
    state->dataPlantCentralGSHP->ChillerHeater(1).VariableFlow = true;
    state->dataPlantCentralGSHP->ChillerHeater(1).CondenserType = PlantCentralGSHP::iCondType::WaterCooled;

    state->dataPlantCentralGSHP->ChillerHeater(1).SizFac = 1.2;

    state->dataPlantCentralGSHP->ChillerHeater(1).RefCapCooling = DataSizing::AutoSize;
    state->dataPlantCentralGSHP->ChillerHeater(1).RefCapCoolingWasAutoSized = true;

    state->dataPlantCentralGSHP->ChillerHeater(1).EvapVolFlowRate = DataSizing::AutoSize;
    state->dataPlantCentralGSHP->ChillerHeater(1).EvapVolFlowRateWasAutoSized = true;

    state->dataPlantCentralGSHP->ChillerHeater(1).CondVolFlowRate = DataSizing::AutoSize;
    state->dataPlantCentralGSHP->ChillerHeater(1).CondVolFlowRateWasAutoSized = true;

    // Needed for calcs
    state->dataPlantCentralGSHP->ChillerHeater(1).RefCOPCooling = 1.5;
    state->dataPlantCentralGSHP->ChillerHeater(1).OpenMotorEff = 0.98;
    state->dataPlantCentralGSHP->ChillerHeater(1).TempRefCondInCooling = 29.4;
    state->dataPlantCentralGSHP->ChillerHeater(1).ClgHtgToCoolingCapRatio = 0.74;
    state->dataPlantCentralGSHP->ChillerHeater(1).ClgHtgtoCogPowerRatio = 1.38;

    // Add the References onto the wrapper
    state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater(1) = state->dataPlantCentralGSHP->ChillerHeater(1);
    state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater(2) = state->dataPlantCentralGSHP->ChillerHeater(1);

    // De-allocate temporary arrays (happens in GetInput too...)
    state->dataPlantCentralGSHP->ChillerHeater.deallocate();

    state->dataPlnt->PlantLoop.allocate(2);
    state->dataSize->PlantSizData.allocate(2);

    // Chilled Water Loop
    int PltSizNum = 1;
    state->dataPlnt->PlantLoop(PltSizNum).PlantSizNum = 1;
    state->dataPlnt->PlantLoop(PltSizNum).FluidIndex = 1;
    state->dataPlnt->PlantLoop(PltSizNum).FluidName = "WATER";
    state->dataSize->PlantSizData(PltSizNum).DesVolFlowRate = 1.0;
    state->dataSize->PlantSizData(PltSizNum).DeltaT = 10.0;
    state->dataSize->PlantSizData(PltSizNum).LoopType = DataSizing::CoolingLoop;
    // Assign to the wrapper
    state->dataPlantCentralGSHP->Wrapper(1).CWLoopNum = PltSizNum;

    // Condenser Loop
    int PltSizCondNum = 2;
    state->dataPlnt->PlantLoop(PltSizCondNum).PlantSizNum = PltSizCondNum;
    state->dataPlnt->PlantLoop(PltSizCondNum).FluidIndex = 1;
    state->dataPlnt->PlantLoop(PltSizCondNum).FluidName = "WATER";
    state->dataSize->PlantSizData(PltSizCondNum).DeltaT = 5.6;
    state->dataSize->PlantSizData(PltSizCondNum).LoopType = DataSizing::CondenserLoop;
    // Assign to the wrapper
    state->dataPlantCentralGSHP->Wrapper(1).GLHELoopNum = PltSizCondNum;

    // Calculate expected values
    Real64 rho_evap = FluidProperties::GetDensityGlycol(*state,
                                                        state->dataPlnt->PlantLoop(PltSizNum).FluidName,
                                                        DataGlobalConstants::CWInitConvTemp,
                                                        state->dataPlnt->PlantLoop(PltSizNum).FluidIndex,
                                                        "ChillerHeater_Autosize_TEST");

    Real64 Cp_evap = FluidProperties::GetSpecificHeatGlycol(*state,
                                                            state->dataPlnt->PlantLoop(PltSizNum).FluidName,
                                                            DataGlobalConstants::CWInitConvTemp,
                                                            state->dataPlnt->PlantLoop(PltSizNum).FluidIndex,
                                                            "ChillerHeater_Autosize_TEST");

    Real64 rho_cond = FluidProperties::GetDensityGlycol(*state,
                                                        state->dataPlnt->PlantLoop(PltSizCondNum).FluidName,
                                                        DataGlobalConstants::CWInitConvTemp,
                                                        state->dataPlnt->PlantLoop(PltSizCondNum).FluidIndex,
                                                        "ChillerHeater_Autosize_TEST");

    Real64 Cp_cond = FluidProperties::GetSpecificHeatGlycol(*state,
                                                            state->dataPlnt->PlantLoop(PltSizCondNum).FluidName,
                                                            state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater(1).TempRefCondInCooling,
                                                            state->dataPlnt->PlantLoop(PltSizCondNum).FluidIndex,
                                                            "ChillerHeater_Autosize_TEST");

    // Note: Each individual chiller heater module is sized to be capable of supporting the total load on the wrapper

    // Flow is multiplied by the SizFac
    Real64 EvapVolFlowRateExpected =
        state->dataSize->PlantSizData(PltSizNum).DesVolFlowRate * state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater(1).SizFac;

    Real64 RefCapCoolingExpected = rho_evap * Cp_evap * EvapVolFlowRateExpected * state->dataSize->PlantSizData(PltSizNum).DeltaT;

    Real64 CondVolFlowRateExpected = RefCapCoolingExpected *
                                     (1.0 + (1.0 / state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater(1).RefCOPCooling) *
                                                state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater(1).OpenMotorEff) /
                                     (rho_cond * Cp_cond * state->dataSize->PlantSizData(PltSizCondNum).DeltaT);

    // now call sizing routine
    state->dataPlnt->PlantFirstSizesOkayToFinalize = true;
    state->dataPlantCentralGSHP->Wrapper(1).SizeWrapper(*state);

    // Careful of actually using PlantCentralGSHP::Wrapper(1).ChillerHeater(1) and not PlantCentralGSHP::ChillerHeater since this array isn't used
    // anymore by the module
    EXPECT_DOUBLE_EQ(EvapVolFlowRateExpected, state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater(1).EvapVolFlowRate);
    EXPECT_DOUBLE_EQ(RefCapCoolingExpected, state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater(1).RefCapCooling);

    EXPECT_DOUBLE_EQ(CondVolFlowRateExpected, state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater(1).CondVolFlowRate);
    EXPECT_DOUBLE_EQ(CondVolFlowRateExpected, state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater(2).CondVolFlowRate);

    // Ensure that stuff that other quantities that depends on RefCapCooling are also initialized properly
    // Heating Cap
    Real64 RefCapClgHtgExpected = RefCapCoolingExpected * state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater(1).ClgHtgToCoolingCapRatio;
    EXPECT_DOUBLE_EQ(RefCapClgHtgExpected, state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater(1).RefCapClgHtg);

    // Heating Power: Calc cooling Power = Cap / COP, and multiply by ratio
    Real64 RefPowerClgHtgExpected = (RefCapCoolingExpected / state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater(1).RefCOPCooling) *
                                    state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater(1).ClgHtgtoCogPowerRatio;
    EXPECT_DOUBLE_EQ(RefPowerClgHtgExpected, state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater(1).RefPowerClgHtg);

    // Heating COP = Heating Cap / Heating Power
    Real64 RefCOPClgHtgExpected = RefCapClgHtgExpected / RefPowerClgHtgExpected;
    EXPECT_DOUBLE_EQ(RefCOPClgHtgExpected, state->dataPlantCentralGSHP->Wrapper(1).ChillerHeater(1).RefCOPClgHtg);
}
