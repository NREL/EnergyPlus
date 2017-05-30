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

// EnergyPlus::Standalone ERV Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/ChillerElectricEIR.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/Psychrometrics.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace EnergyPlus::ChillerElectricEIR;
using namespace EnergyPlus::DataLoopNode;

TEST_F( EnergyPlusFixture, ChillerElectricEIR_TestOutletNodeConditions )
{

	int Num = 1;

	ElectricEIRChiller.allocate( Num );
	ElectricEIRChillerReport.allocate( Num );

	ElectricEIRChiller( Num ).EvapInletNodeNum = 1;
	ElectricEIRChiller( Num ).EvapOutletNodeNum = 2;
	ElectricEIRChiller( Num ).CondInletNodeNum = 3;
	ElectricEIRChiller( Num ).CondOutletNodeNum = 4;
	ElectricEIRChiller( Num ).HeatRecInletNodeNum = 5;
	ElectricEIRChiller( Num ).HeatRecOutletNodeNum = 6;

	Node.allocate( 6 );
	Node( ElectricEIRChiller( Num ).EvapInletNodeNum ).Temp = 18.0;
	Node( ElectricEIRChiller( Num ).CondInletNodeNum ).Temp = 35.0;

	CondMassFlowRate = 0.0;
	EvapMassFlowRate = 0.0;

	UpdateElectricEIRChillerRecords( -2000, true, 1 );

	EXPECT_EQ( 18, ElectricEIRChillerReport( Num ).EvapOutletTemp );
	EXPECT_EQ( 35, ElectricEIRChillerReport( Num ).CondOutletTemp );

	Node.deallocate();
	ElectricEIRChiller.deallocate();
	ElectricEIRChillerReport.deallocate();
}

TEST_F( EnergyPlusFixture, ElectricEIRChiller_HeatRecoveryAutosizeTest )
{
	// unit test for autosizing heat recovery in Chiller:Electric:EIR
	ChillerElectricEIR::ElectricEIRChiller.allocate( 1 );

	ChillerElectricEIR::ElectricEIRChiller( 1 ).SizFac = 1.0;
	ChillerElectricEIR::ElectricEIRChiller( 1 ).DesignHeatRecVolFlowRateWasAutoSized = true;
	ChillerElectricEIR::ElectricEIRChiller( 1 ).HeatRecCapacityFraction = 0.5;
	ChillerElectricEIR::ElectricEIRChiller( 1 ).HeatRecActive = true;
	ChillerElectricEIR::ElectricEIRChiller( 1 ).CondenserType = ChillerElectricEIR::WaterCooled;
	ChillerElectricEIR::ElectricEIRChiller( 1 ).CWLoopNum = 1;
	ChillerElectricEIR::ElectricEIRChiller( 1 ).CDLoopNum = 2;
	ChillerElectricEIR::ElectricEIRChiller( 1 ).EvapVolFlowRate = 1.0;
	ChillerElectricEIR::ElectricEIRChiller( 1 ).CondVolFlowRate = 1.0;
	ChillerElectricEIR::ElectricEIRChiller( 1 ).RefCap = 10000;
	ChillerElectricEIR::ElectricEIRChiller( 1 ).RefCOP = 3.0;

	DataPlant::PlantLoop.allocate( 2 );
	DataSizing::PlantSizData.allocate( 2 );
	// chilled water loop
	DataPlant::PlantLoop( 1 ).PlantSizNum = 1;
	DataPlant::PlantLoop( 1 ).FluidIndex = 1;
	DataPlant::PlantLoop( 1 ).FluidName = "WATER";
	DataSizing::PlantSizData( 1 ).DesVolFlowRate = 1.0;
	DataSizing::PlantSizData( 1 ).DeltaT = 5.0;
	// condenser water loop
	DataPlant::PlantLoop( 2 ).PlantSizNum = 2;
	DataPlant::PlantLoop( 2 ).FluidIndex = 1;
	DataPlant::PlantLoop( 2 ).FluidName = "WATER";
	DataSizing::PlantSizData( 2 ).DesVolFlowRate = 1.0;
	DataSizing::PlantSizData( 2 ).DeltaT = 5.0;

	DataPlant::PlantFirstSizesOkayToFinalize = true;

	//now call sizing routine
	ChillerElectricEIR::SizeElectricEIRChiller( 1 );
	// see if heat recovery flow rate is as expected
	EXPECT_NEAR( ChillerElectricEIR::ElectricEIRChiller( 1 ).DesignHeatRecVolFlowRate, 0.5, 0.00001 );

	ChillerElectricEIR::ElectricEIRChiller.deallocate();
	DataSizing::PlantSizData.deallocate();
	DataPlant::PlantLoop.deallocate();

}

TEST_F( EnergyPlusFixture, ChillerElectricEIR_AirCooledChiller )
{

		bool RunFlag( true );
		Real64 MyLoad( -10000.0 );

		DataPlant::TotNumLoops = 2;
		DataEnvironment::OutBaroPress = 101325.0;
		DataEnvironment::StdRhoAir = 1.20;
		DataGlobals::NumOfTimeStepInHour = 1;
		DataGlobals::TimeStep = 1;
		DataGlobals::MinutesPerTimeStep = 60;

		Psychrometrics::InitializePsychRoutines();

		std::string const idf_objects = delimited_string({
		"Chiller:Electric:EIR,",
		"  AirCooledChiller,                   !- Name",
		"  autosize,                           !- Reference Capacity {W}",
		"  5.50,                               !- Reference COP {W/W}",
		"  6.67,                               !- Reference Leaving Chilled Water Temperature {C}",
		"  29.40,                              !- Reference Entering Condenser Fluid Temperature {C}",
		"  autosize,                           !- Reference Chilled Water Flow Rate {m3/s}",
		"  autosize,                           !- Reference Condenser Fluid Flow Rate {m3/s}",
		"  Air cooled CentCapFT,               !- Cooling Capacity Function of Temperature Curve Name",
		"  Air cooled CentEIRFT,               !- Electric Input to Cooling Output Ratio Function of Temperature Curve Name",
		"  Air cooled CentEIRFPLR,             !- Electric Input to Cooling Output Ratio Function of Part Load Ratio Curve Name",
		"  0.10,                               !- Minimum Part Load Ratio",
		"  1.00,                               !- Maximum Part Load Ratio",
		"  1.00,                               !- Optimum Part Load Ratio",
		"  0.25,                               !- Minimum Unloading Ratio",
		"  CHW Inlet Node,                     !- Chilled Water Inlet Node Name",
		"  CHW Outlet Node,                    !- Chilled Water Outlet Node Name",
		"  Outdoor Air Condenser Inlet Node,   !- Condenser Inlet Node Name",
		"  Outdoor Air Condenser Outlet Node,  !- Condenser Outlet Node Name",
		"  AirCooled,                          !- Condenser Type",
		"  0.04,                               !- Condenser Fan Power Ratio {W/W}",
		"  1.00,                               !- Fraction of Compressor Electric Consumption Rejected by Condenser",
		"  5.00,                               !- Leaving Chilled Water Lower Temperature Limit {C}",
		"  NotModulated,                       !- Chiller Flow Mode",
		"  0.0,                                !- Design Heat Recovery Water Flow Rate {m3/s}",
		"  ,                                   !- Heat Recovery Inlet Node Name",
		"  ,                                   !- Heat Recovery Outlet Node Name",
		"  1.00,                               !- Sizing Factor",
		"  0.00,                               !- Basin Heater Capacity {W/K}",
		"  2.00,                               !- Basin Heater Setpoint Temperature {C}",
		"  ,                                   !- Basin Heater Operating Schedule Name",
		"  1.00,                               !- Condenser Heat Recovery Relative Capacity Fraction",
		"  ,                                   !- Heat Recovery Inlet High Temperature Limit Schedule Name",
		"  ;                                   !- Heat Recovery Leaving Temperature Setpoint Node Name",

		"Curve:Biquadratic, Air cooled CentCapFT, 0.257896, 0.0389016, -0.00021708, 0.0468684, -0.00094284, -0.00034344, 5, 10, 24, 35, , , , , ;",
		"Curve:Biquadratic, Air cooled CentEIRFT, 0.933884, -0.058212,  0.00450036, 0.00243,    0.000486,   -0.001215,   5, 10, 24, 35, , , , , ;",
		"Curve:Quadratic, Air cooled CentEIRFPLR, 0.222903,  0.313387,  0.46371,    0, 1, , , , ;",

	});

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

		GetElectricEIRChillerInput();

		DataPlant::PlantLoop( 1 ).Name = "ChilledWaterLoop";
		DataPlant::PlantLoop( 1 ).FluidName = "ChilledWater";
		DataPlant::PlantLoop( 1 ).FluidIndex = 1;
		DataPlant::PlantLoop( 1 ).PlantSizNum = 1;
		DataPlant::PlantLoop( 1 ).FluidName = "WATER";
		DataPlant::PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).Name = ElectricEIRChiller( 1 ).Name;
		DataPlant::PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).TypeOf_Num = DataPlant::TypeOf_Chiller_ElectricEIR;
		DataPlant::PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumIn = ElectricEIRChiller( 1 ).EvapInletNodeNum;
		DataPlant::PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumOut = ElectricEIRChiller( 1 ).EvapOutletNodeNum;

		DataSizing::PlantSizData.allocate( 1 );
		DataSizing::PlantSizData( 1 ).DesVolFlowRate = 0.001;
		DataSizing::PlantSizData( 1 ).DeltaT = 5.0;

		DataPlant::PlantFirstSizesOkayToFinalize = true;
		DataPlant::PlantFirstSizesOkayToReport = true;
		DataPlant::PlantFinalSizesOkayToReport = true;

		InitElectricEIRChiller( 1, RunFlag, MyLoad );
		SizeElectricEIRChiller( 1 );

		// run through init again after sizing is complete to set mass flow rate
		DataGlobals::BeginEnvrnFlag = true;
		InitElectricEIRChiller( 1, RunFlag, MyLoad );

		// check chiller water side evap flow rate is non-zero
		EXPECT_NEAR( ElectricEIRChiller( 1 ).EvapMassFlowRateMax, 0.999898, 0.0000001 );

		// check autocalculate for air-cooled or evap-cooled chiller condenser side fluid flow rate
		Real64 CalcCondVolFlow = ElectricEIRChiller( 1 ).RefCap * 0.000114;
		EXPECT_EQ( CalcCondVolFlow, ElectricEIRChiller( 1 ).CondVolFlowRate );
		EXPECT_NEAR( ElectricEIRChiller( 1 ).CondVolFlowRate, 2.3925760323498, 0.0000001 );
		EXPECT_NEAR( ElectricEIRChiller( 1 ).CondMassFlowRateMax, 2.7918772761695, 0.0000001 );

}
