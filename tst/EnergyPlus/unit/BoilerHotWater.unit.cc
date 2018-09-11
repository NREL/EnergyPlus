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

// EnergyPlus::Boilers Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/Boilers.hh>
#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/FluidProperties.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using EnergyPlus::EnergyPlusFixture;
using EnergyPlus::Boilers::Boiler;
using EnergyPlus::DataPlant::PlantLoop;
using EnergyPlus::DataPlant::PlantFirstSizesOkayToFinalize;
using EnergyPlus::DataSizing::AutoSize;
using EnergyPlus::DataSizing::PlantSizData;

class BoilerSizingFixture : public EnergyPlusFixture
{
    protected:
        virtual void SetUp()
        {
            // call the parent setup
            EnergyPlusFixture::SetUp();

            // global variables
            PlantFirstSizesOkayToFinalize = true;

            // sizing data
            PlantSizData.allocate(1);
            PlantSizData(1).DesVolFlowRate = 1.0;
            PlantSizData(1).DeltaT = 10.0;

            // hot water loop
            PlantLoop.allocate(1);
            PlantLoop(1).PlantSizNum = 1;
            PlantLoop(1).FluidIndex = 1;
            PlantLoop(1).FluidName = "WATER";
            
            // setup required boiler data
            Boiler.allocate(1);
            Boiler(1).setLoopNumber(1);
            Boiler(1).setDesignSizingFactor(1.2);
            Boiler(1).setDesignOutletTemperature(82.0);
        }

        virtual void TearDown()
        {
            PlantLoop.deallocate();
            PlantSizData.deallocate();
            Boiler.deallocate();

            // call the parent tear down
            EnergyPlusFixture::TearDown();
        }
};

TEST_F(BoilerSizingFixture, BoilerHotWaterSizingWhenNotRequired)
{
    // set the hardsized values
    Boiler(1).setDesignVolumeFlowRate(1.0);
    Boiler(1).setDesignNominalCapacity(40000.0);

    // confirm the values are set as expected
    EXPECT_DOUBLE_EQ(Boiler(1).getDesignVolumeFlowRate(), 1.0);
    EXPECT_DOUBLE_EQ(Boiler(1).getDesignNominalCapacity(), 40000.0);

    // do the sizing
    Boiler(1).doSizing();

    // confirm the values have not been changed by the sizing
    EXPECT_DOUBLE_EQ(Boiler(1).getDesignVolumeFlowRate(), 1.0);
    EXPECT_DOUBLE_EQ(Boiler(1).getDesignNominalCapacity(), 40000.0);
}

TEST_F(BoilerSizingFixture, BoilerHotWaterSizingWhenRequired)
{
    // set the values to AutoSize
    Boiler(1).setDesignVolumeFlowRate(AutoSize);
    Boiler(1).setDesignNominalCapacity(AutoSize);

    // confirm the values are set as expected
    EXPECT_DOUBLE_EQ(Boiler(1).getDesignVolumeFlowRate(), AutoSize);
    EXPECT_DOUBLE_EQ(Boiler(1).getDesignNominalCapacity(), AutoSize);

    // do the sizing
    Boiler(1).doSizing();

    // confirm the values have been set by the sizing
    EXPECT_NEAR(Boiler(1).getDesignVolumeFlowRate(), 1.2, 0.000001);
    EXPECT_NEAR(Boiler(1).getDesignNominalCapacity(), 50409257.0, 1.0);
}
TEST_F(EnergyPlusFixture, Boiler_HotWaterAutoSizeTempTest)
{
    // unit test for checking hot water temperature for autosizing
    // boiler nominal capacity in Boiler:HotWater

    Boilers::Boiler.allocate(1);
    // Autosized Hot Water Boiler
    Boilers::Boiler(1).LoopNum = 1;
    Boilers::Boiler(1).SizFac = 1.2;
    Boilers::Boiler(1).NomCap = DataSizing::AutoSize;
    Boilers::Boiler(1).NomCapWasAutoSized = true;
    Boilers::Boiler(1).VolFlowRate = DataSizing::AutoSize;
    Boilers::Boiler(1).VolFlowRateWasAutoSized = true;

    DataPlant::PlantLoop.allocate(1);
    DataSizing::PlantSizData.allocate(1);
    // Hot Water Loop
    DataPlant::PlantLoop(1).PlantSizNum = 1;
    DataPlant::PlantLoop(1).FluidIndex = 1;
    DataPlant::PlantLoop(1).FluidName = "WATER";
    DataSizing::PlantSizData(1).DesVolFlowRate = 1.0;
    DataSizing::PlantSizData(1).DeltaT = 10.0;
    DataPlant::PlantFirstSizesOkayToFinalize = true;

    // calculate nominal capacity at 60.0 C hot water temperature
    Real64 rho = FluidProperties::GetDensityGlycol(DataPlant::PlantLoop(Boiler(1).LoopNum).FluidName,
                                                   60.0,
                                                   DataPlant::PlantLoop(Boiler(1).LoopNum).FluidIndex,
                                                   "Boiler_HotWaterAutoSizeTempTest");
    Real64 Cp = FluidProperties::GetSpecificHeatGlycol(DataPlant::PlantLoop(Boiler(1).LoopNum).FluidName,
                                                       60.0,
                                                       DataPlant::PlantLoop(Boiler(1).LoopNum).FluidIndex,
                                                       "Boiler_HotWaterAutoSizeTempTest");

    Real64 NomCapBoilerExpected = rho * PlantSizData(1).DesVolFlowRate * Cp * PlantSizData(1).DeltaT * Boiler(1).SizFac;

    // now call sizing routine
    Boilers::SizeBoiler(1);
    // see if boiler volume flow rate returned is autosized value
    EXPECT_DOUBLE_EQ(Boilers::Boiler(1).VolFlowRate, 1.2);
    // see if boiler nominal capacity returned is autosized value
    EXPECT_DOUBLE_EQ(Boilers::Boiler(1).NomCap, NomCapBoilerExpected);
}
