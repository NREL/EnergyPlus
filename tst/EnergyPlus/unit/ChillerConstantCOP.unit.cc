// EnergyPlus, Copyright (c) 1996-2017, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
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

// EnergyPlus::PlantChillers, Chiller:ConstantCOP Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/PlantChillers.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace EnergyPlus::DataLoopNode;
using namespace EnergyPlus::NodeInputManager;
using namespace EnergyPlus::PlantChillers;


TEST_F( EnergyPlusFixture, ChillerConstantCOP_WaterCooled_Autosize )
{

		DataPlant::TotNumLoops = 4;
		DataEnvironment::OutBaroPress = 101325.0;
		DataEnvironment::StdRhoAir = 1.20;
		DataGlobals::NumOfTimeStepInHour = 1;
		DataGlobals::TimeStep = 1;
		DataGlobals::MinutesPerTimeStep = 60;

		std::string const idf_objects = delimited_string({
		"  Chiller:ConstantCOP,",
		"    Chiller,                 !- Name",
		"    autosize,                !- Nominal Capacity {W}",
		"    4.0,                     !- Nominal COP {W/W}",
		"    autosize,                !- Design Chilled Water Flow Rate {m3/s}",
		"    autosize,                !- Design Condenser Water Flow Rate {m3/s}",
		"    Chiller ChW Inlet,       !- Chilled Water Inlet Node Name",
		"    Chiller ChW Outlet,      !- Chilled Water Outlet Node Name",
		"    Chiller Cnd Inlet,       !- Condenser Inlet Node Name",
		"    Chiller Cnd Outlet,      !- Condenser Outlet Node Name",
		"    WaterCooled,             !- Condenser Type",
		"    ConstantFlow,            !- Chiller Flow Mode",
		"    1,                       !- Sizing Factor",
		"    ,                        !- Basin Heater Capacity {W/K}",
		"    2;                       !- Basin Heater Setpoint Temperature {C}",
		} );

		EXPECT_FALSE( process_idf( idf_objects, false ) );

		DataPlant::PlantLoop.allocate( DataPlant::TotNumLoops );
		DataPlant::PlantLoop.allocate( DataPlant::TotNumLoops );
		for( int l = 1; l <= DataPlant::TotNumLoops; ++l ) {
			auto & loop( DataPlant::PlantLoop( l ) );
			loop.LoopSide.allocate( 2 );
			auto & loopside( DataPlant::PlantLoop( l ).LoopSide( 1 ) );
			loopside.TotalBranches = 1;
			loopside.Branch.allocate( 1 );
			auto & loopsidebranch( DataPlant::PlantLoop( l ).LoopSide( 1 ).Branch( 1 ) );
			loopsidebranch.TotalComponents = 1;
			loopsidebranch.Comp.allocate( 1 );
		}

		GetConstCOPChillerInput();
		
		DataPlant::PlantLoop( 1 ).Name = "ChilledWaterLoop";
		DataPlant::PlantLoop( 1 ).FluidName = "ChilledWater";
		DataPlant::PlantLoop( 1 ).FluidIndex = 1;
		DataPlant::PlantLoop( 1 ).PlantSizNum = 1;
		DataPlant::PlantLoop( 1 ).FluidName = "WATER";
		DataPlant::PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).Name = ConstCOPChiller( 1 ).Base.Name;
		DataPlant::PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).TypeOf_Num = DataPlant::TypeOf_Chiller_ConstCOP;
		DataPlant::PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumIn = ConstCOPChiller( 1 ).Base.EvapInletNodeNum;
		DataPlant::PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumOut = ConstCOPChiller( 1 ).Base.EvapOutletNodeNum;

		DataPlant::PlantLoop( 2 ).Name = "CondenserWaterLoop";
		DataPlant::PlantLoop( 2 ).FluidName = "CondenserWater";
		DataPlant::PlantLoop( 2 ).FluidIndex = 1;
		DataPlant::PlantLoop( 2 ).PlantSizNum = 2;
		DataPlant::PlantLoop( 2 ).FluidName = "WATER";
		DataPlant::PlantLoop( 2 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).Name = ConstCOPChiller( 1 ).Base.Name;
		DataPlant::PlantLoop( 2 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).TypeOf_Num = DataPlant::TypeOf_Chiller_ConstCOP;
		DataPlant::PlantLoop( 2 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumIn = ConstCOPChiller( 1 ).Base.CondInletNodeNum;
		DataPlant::PlantLoop( 2 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumOut = ConstCOPChiller( 1 ).Base.CondOutletNodeNum;

		DataSizing::PlantSizData.allocate( 2 );
		DataSizing::PlantSizData( 1 ).DesVolFlowRate = 0.001;
		DataSizing::PlantSizData( 1 ).DeltaT = 5.0;

		DataSizing::PlantSizData( 2 ).DesVolFlowRate = 0.001;
		DataSizing::PlantSizData( 2 ).DeltaT = 5.0;

		DataPlant::PlantFirstSizesOkayToFinalize = true;
		DataPlant::PlantFirstSizesOkayToReport = true;
		DataPlant::PlantFinalSizesOkayToReport = true;

		bool RunFlag( true );
		Real64 MyLoad( -20000.0 );

		Psychrometrics::InitializePsychRoutines();
		InitConstCOPChiller( 1, RunFlag, MyLoad );
		SizeConstCOPChiller( 1 );

		// run init again after sizing is complete to set mass flow rate
		DataGlobals::BeginEnvrnFlag = true;
		InitConstCOPChiller( 1, RunFlag, MyLoad );

		// check autocalculate chiller nominal capacity
		EXPECT_NEAR( ConstCOPChiller( 1 ).Base.NomCap, 20987.5090557, 0.000001 );
		// check autocalculate chiller side evap water flow rate
		EXPECT_NEAR( ConstCOPChiller( 1 ).Base.EvapVolFlowRate, 0.001,  0.000001 );
		EXPECT_NEAR( ConstCOPChiller( 1 ).Base.EvapMassFlowRateMax, 0.999898, 0.0000001 );
		// check autocalculate chiller side cond water flow rate
		EXPECT_NEAR( ConstCOPChiller( 1 ).Base.CondVolFlowRate, 0.0012606164769923673, 0.0000001 );
		EXPECT_NEAR( ConstCOPChiller( 1 ).Base.CondMassFlowRateMax, 1.2604878941117141, 0.0000001 );
}
