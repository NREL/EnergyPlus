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

// EnergyPlus::HeatingCoils Unit Tests

#include <exception>

// Google Test Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/DataGlobalConstants.hh>
#include <EnergyPlus/HeatingCoils.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <gtest/gtest.h>

namespace EnergyPlus {

TEST_F(EnergyPlusFixture, HeatingCoils_FuelTypeInput)
{
    std::string const idf_objects = delimited_string(
        {"Coil:Heating:Fuel,", "  Furnace Coil,            !- Name", "  ,    !- Availability Schedule Name", "  OtherFuel1,              !- FuelType",
         "  0.8,                     !- Gas Burner Efficiency", "  20000,                   !- Nominal Capacity {W}",
         "  Heating Coil Air Inlet Node,  !- Air Inlet Node Name", "  Air Loop Outlet Node;    !- Air Outlet Node Name"});

    ASSERT_TRUE(process_idf(idf_objects));

    ASSERT_NO_THROW(HeatingCoils::GetHeatingCoilInput());

    EXPECT_EQ(HeatingCoils::HeatingCoil(1).FuelType_Num, DataGlobalConstants::iRT_OtherFuel1);
}

TEST_F(EnergyPlusFixture, HeatingCoils_FuelTypeInputError)
{
    std::string const idf_objects = delimited_string({"Coil:Heating:Fuel,",
                                                      "  Furnace Coil,            !- Name",
                                                      "  ,    !- Availability Schedule Name",
                                                      "  Electricity,              !- FuelType",
                                                      "  0.8,                     !- Gas Burner Efficiency",
                                                      "  20000,                   !- Nominal Capacity {W}",
                                                      "  Heating Coil Air Inlet Node,  !- Air Inlet Node Name",
                                                      "  Air Loop Outlet Node;    !- Air Outlet Node Name"});

    EXPECT_FALSE(process_idf(idf_objects, false));
    ASSERT_THROW(HeatingCoils::GetHeatingCoilInput(), std::runtime_error);

    std::string const error_string = delimited_string({
        "   ** Severe  ** <root>[Coil:Heating:Fuel][Furnace Coil][fuel_type] - \"Electricity\" - Failed to match against any enum values.",
        "   ** Severe  ** GetHeatingCoilInput: Coil:Heating:Fuel: Invalid Fuel Type entered =ELECTRICITY for Name=FURNACE COIL",
        "   **  Fatal  ** GetHeatingCoilInput: Errors found in input.  Program terminates.",
        "   ...Summary of Errors that led to program termination:",
        "   ..... Reference severe error count=2",
        "   ..... Last severe error=GetHeatingCoilInput: Coil:Heating:Fuel: Invalid Fuel Type entered =ELECTRICITY for Name=FURNACE COIL",
    });

    EXPECT_TRUE(compare_err_stream(error_string, true));
}

TEST_F(EnergyPlusFixture, HeatingCoils_FuelTypePropaneGas)
{
    std::string const idf_objects = delimited_string(
        {"Coil:Heating:Fuel,", "  Furnace Coil,            !- Name", "  ,    !- Availability Schedule Name", "  Propane,                 !- FuelType",
         "  0.8,                     !- Gas Burner Efficiency", "  20000,                   !- Nominal Capacity {W}",
         "  Heating Coil Air Inlet Node,  !- Air Inlet Node Name", "  Air Loop Outlet Node;    !- Air Outlet Node Name"});

    ASSERT_TRUE(process_idf(idf_objects));

    ASSERT_NO_THROW(HeatingCoils::GetHeatingCoilInput());

    EXPECT_EQ(HeatingCoils::HeatingCoil(1).FuelType_Num, DataGlobalConstants::iRT_Propane);
}

TEST_F(EnergyPlusFixture, HeatingCoils_OutletAirPropertiesTest)
{
    // 7391 Test outlet air properties for MultiStageGasHeatingCoil
    int CoilNum = 1;
    Real64 OffMassFlowrate = 0.2;
    Real64 OnMassFlowrate = 0.6;

    HeatingCoils::HeatingCoil.allocate(CoilNum);
    HeatingCoils::HeatingCoil(CoilNum).InletAirTemp = 0.0;
    HeatingCoils::HeatingCoil(CoilNum).InletAirHumRat = 0.001;
    HeatingCoils::HeatingCoil(CoilNum).InletAirEnthalpy = Psychrometrics::PsyHFnTdbW(HeatingCoils::HeatingCoil(CoilNum).InletAirTemp, HeatingCoils::HeatingCoil(CoilNum).InletAirHumRat);
    DataEnvironment::OutBaroPress = 101325.0;
    HeatingCoils::HeatingCoil(CoilNum).SchedPtr = 1;
    ScheduleManager::Schedule.allocate(1);
    ScheduleManager::Schedule(1).CurrentValue = 1.0;
    DataHVACGlobals::MSHPMassFlowRateLow = OnMassFlowrate;
    HeatingCoils::HeatingCoil(CoilNum).MSNominalCapacity.allocate(1);
    HeatingCoils::HeatingCoil(CoilNum).MSNominalCapacity(1) = 10000;
    HeatingCoils::HeatingCoil(CoilNum).MSEfficiency.allocate(1);
    HeatingCoils::HeatingCoil(CoilNum).MSEfficiency(1) = 0.9;
    HeatingCoils::HeatingCoil(CoilNum).AirInletNodeNum = 1;
    HeatingCoils::HeatingCoil(CoilNum).AirOutletNodeNum = 2;
    DataLoopNode::Node.allocate(2);
    HeatingCoils::HeatingCoil(CoilNum).MSParasiticElecLoad.allocate(1);
    HeatingCoils::HeatingCoil(CoilNum).MSParasiticElecLoad(1) = 0.0;

    HeatingCoils::HeatingCoil(CoilNum).InletAirMassFlowRate = OffMassFlowrate;
    HeatingCoils::CalcMultiStageGasHeatingCoil(CoilNum, 0.0, 0.0, 1, 2);
    Real64 HeatLoad00 =
        HeatingCoils::HeatingCoil(CoilNum).InletAirMassFlowRate *
        (Psychrometrics::PsyHFnTdbW(HeatingCoils::HeatingCoil(CoilNum).OutletAirTemp, HeatingCoils::HeatingCoil(CoilNum).OutletAirHumRat) - 
            HeatingCoils::HeatingCoil(CoilNum).InletAirEnthalpy);
    EXPECT_NEAR(HeatLoad00, HeatingCoils::HeatingCoil(CoilNum).HeatingCoilLoad, 0.0001);

    HeatingCoils::HeatingCoil(CoilNum).InletAirMassFlowRate = 0.5 * OnMassFlowrate + (1.0 - 0.5) * OffMassFlowrate;
    HeatingCoils::CalcMultiStageGasHeatingCoil(CoilNum, 0.0, 0.5, 1, 2);
    Real64 HeatLoad05 =
        HeatingCoils::HeatingCoil(CoilNum).InletAirMassFlowRate *
              (Psychrometrics::PsyHFnTdbW(HeatingCoils::HeatingCoil(CoilNum).OutletAirTemp, HeatingCoils::HeatingCoil(CoilNum).OutletAirHumRat) -
               HeatingCoils::HeatingCoil(CoilNum).InletAirEnthalpy);
    EXPECT_NEAR(HeatLoad05, HeatingCoils::HeatingCoil(CoilNum).HeatingCoilLoad, 0.0001);

    HeatingCoils::HeatingCoil(CoilNum).InletAirMassFlowRate = OnMassFlowrate;
    HeatingCoils::CalcMultiStageGasHeatingCoil(CoilNum, 0.0, 1.0, 1, 2);
    Real64 HeatLoad10 =
        HeatingCoils::HeatingCoil(CoilNum).InletAirMassFlowRate *
              (Psychrometrics::PsyHFnTdbW(HeatingCoils::HeatingCoil(CoilNum).OutletAirTemp, HeatingCoils::HeatingCoil(CoilNum).OutletAirHumRat) -
               HeatingCoils::HeatingCoil(CoilNum).InletAirEnthalpy);
    EXPECT_NEAR(HeatLoad10, HeatingCoils::HeatingCoil(CoilNum).HeatingCoilLoad, 0.0001);

    // check linear relationship at PLR = 0.5
    EXPECT_NEAR(HeatLoad05, 0.5 * HeatingCoils::HeatingCoil(CoilNum).MSNominalCapacity(1), 0.0001);
}

} // namespace EnergyPlus
