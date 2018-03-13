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

// EnergyPlus::Pumps Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

#include "Fixtures/EnergyPlusFixture.hh"
#include <DataPlant.hh>
#include <PlantComponentTemperatureSources.hh>

namespace EnergyPlus {

	TEST_F(EnergyPlusFixture, TestPlantComponentTemperatureSource) {

		// Setup the Plant temperature source object from IDF
		std::string const idf_objects = delimited_string({
                     "PlantComponent:TemperatureSource,",
                     " FluidSource,             !- Name",
                     " FluidSource Inlet Node,  !- Inlet Node",
                     " FluidSource Outlet Node, !- Outlet Node",
                     " 0.001,                   !- Design Volume Flow Rate {m3/s}",
                     " Constant,                !- Temperature Specification Type",
                     " 8,                       !- Source Temperature {C}",
                     " ;                        !- Source Temperature Schedule Name"
		});
		ASSERT_TRUE(process_idf(idf_objects));

		// Setup the plant itself manually
		DataPlant::TotNumLoops = 1;
		DataPlant::PlantLoop.allocate(1);
		DataPlant::PlantLoop(1).LoopSide.allocate(2);
		DataPlant::PlantLoop(1).LoopSide(1).TotalBranches = 1;
		DataPlant::PlantLoop(1).LoopSide(1).Branch.allocate(1);
		DataPlant::PlantLoop(1).LoopSide(1).Branch(1).TotalComponents = 1;
		DataPlant::PlantLoop(1).LoopSide(1).Branch(1).Comp.allocate(1);
		DataPlant::PlantLoop(1).LoopSide(2).TotalBranches = 1;
		DataPlant::PlantLoop(1).LoopSide(2).Branch.allocate(1);
		DataPlant::PlantLoop(1).LoopSide(2).Branch(1).TotalComponents = 1;
		DataPlant::PlantLoop(1).LoopSide(2).Branch(1).Comp.allocate(1);
		DataPlant::PlantLoop(1).LoopSide(2).Branch(1).Comp(1).TypeOf_Num = DataPlant::TypeOf_WaterSource;
		DataPlant::PlantLoop(1).LoopSide(2).Branch(1).Comp(1).Name = "FLUIDSOURCE";
		DataPlant::PlantLoop(1).LoopSide(2).Branch(1).Comp(1).NodeNumIn = 1;

		// define some arguments that aren't used but listed as parameters
		int dummyInteger = 0;
		bool dummyRunFlag = true;

		// define the INOUT variables that are passed back
		int compIndex = 0; // use this to call back into Sim multiple times
		Real64 myLoad = 0.0;
		Real64 maxLoad = 0.0, minLoad = 0.0, optLoad = 0.0;
		Real64 sizingFactor = 0.0;

		// First call is for initialization only
		bool firstHVACIteration = true;
		bool initLoopEquip = true;
		bool getSizingFactor = true;
		DataGlobals::BeginEnvrnFlag = true;
		DataPlant::PlantFirstSizesOkayToFinalize = true;
		PlantComponentTemperatureSources::SimWaterSource("FLUIDSOURCE", dummyInteger, compIndex, dummyRunFlag, firstHVACIteration, initLoopEquip, myLoad, maxLoad, minLoad, optLoad, getSizingFactor, sizingFactor);

		// We can check that GetInput happened properly here
		EXPECT_EQ(1u, PlantComponentTemperatureSources::WaterSource.size());
		auto & waterSource1 = PlantComponentTemperatureSources::WaterSource(1);
		EXPECT_EQ(PlantComponentTemperatureSources::TempSpecType_Constant, waterSource1.TempSpecType);
		EXPECT_EQ(1, waterSource1.InletNodeNum);
		EXPECT_EQ(2, waterSource1.OutletNodeNum);

		// Second call is on firstHVAC, no load at the moment
		firstHVACIteration = true;
		initLoopEquip = false;
		getSizingFactor = false;
		PlantComponentTemperatureSources::SimWaterSource("FLUIDSOURCE", dummyInteger, compIndex, dummyRunFlag, firstHVACIteration, initLoopEquip, myLoad, maxLoad, minLoad, optLoad, getSizingFactor, sizingFactor);
		EXPECT_NEAR(0.0, waterSource1.MassFlowRate, 0.00001);

		// Third call is no longer firstHVAC, and we now have a load
		firstHVACIteration = false;
		myLoad = 1696.55;
		PlantComponentTemperatureSources::SimWaterSource("FLUIDSOURCE", dummyInteger, compIndex, dummyRunFlag, firstHVACIteration, initLoopEquip, myLoad, maxLoad, minLoad, optLoad, getSizingFactor, sizingFactor);
		EXPECT_NEAR(0.05, waterSource1.MassFlowRate, 0.001);

		// Do this for scheduled temperature
		// NumOfTimeStepInHour = 1; // must initialize this to get schedules initialized
		// MinutesPerTimeStep = 60; // must initialize this to get schedules initialized
		// ProcessScheduleInput(); // read schedules

	}
}
