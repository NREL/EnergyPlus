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

// EnergyPlus::SwimmingPool Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/SwimmingPool.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataPlant.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::SwimmingPool;
using namespace EnergyPlus::DataSurfaces;
using namespace EnergyPlus::DataPlant;

TEST_F( EnergyPlusFixture, SwimmingPool_MakeUpWaterVolFlow )
{

	//Tests for MakeUpWaterVolFlowFunct
	EXPECT_EQ( 0.05, MakeUpWaterVolFlowFunct( 5, 100 ) );
	EXPECT_NEAR( 0.00392, MakeUpWaterVolFlowFunct( 0.1, 25.5 ), .0001 );
	EXPECT_EQ( -180, MakeUpWaterVolFlowFunct( -9, .05 ) );
	EXPECT_NE( 10, MakeUpWaterVolFlowFunct( 10, 0.01 ) );

	//Tests for MakeUpWaterVolFunct
	EXPECT_EQ( 0.05, MakeUpWaterVolFunct( 5, 100 ) );
	EXPECT_NEAR( 0.00392, MakeUpWaterVolFunct( 0.1, 25.5 ), .0001 );
	EXPECT_EQ( -180, MakeUpWaterVolFunct( -9, .05 ) );
	EXPECT_NE( 10, MakeUpWaterVolFunct( 10, 0.01 ) );

}

TEST_F( EnergyPlusFixture, SwimmingPool_CalcSwimmingPoolEvap )
{
	int SurfNum;
	int PoolNum;
	Real64 MAT;
	Real64 HumRat;
	Real64 EvapRate;
	
	// Tests for CalcSwimmingPoolEvap--Evaporate Rate Calculation for Swimming Pools
	SwimmingPool::clear_state();
	DataSurfaces::clear_state();

	NumSwimmingPools = 1;
	Pool.allocate( 1 );
	DataSurfaces::Surface.allocate( 1 );
	Surface( 1 ).Area = 10.0;
	SurfNum = 1;
	PoolNum = 1;
	DataEnvironment::OutBaroPress = 101400.0;
	
	// Test 1
	Pool( PoolNum ).PoolWaterTemp = 30.0;
	MAT = 20.0;
	HumRat = 0.005;
	Pool( PoolNum ).CurActivityFactor = 0.5;
	Pool( PoolNum ).CurCoverEvapFac = 0.3;
	CalcSwimmingPoolEvap( EvapRate, PoolNum, SurfNum, MAT, HumRat );
	EXPECT_NEAR( 0.000207, EvapRate, 0.000001 );
	EXPECT_NEAR( 4250.0, Pool( PoolNum ).SatPressPoolWaterTemp, 10.0 );
	EXPECT_NEAR( 810.0, Pool( PoolNum ).PartPressZoneAirTemp, 10.0 );

	// Test 2
	Pool( PoolNum ).PoolWaterTemp = 27.0;
	MAT = 22.0;
	HumRat = 0.010;
	Pool( PoolNum ).CurActivityFactor = 1.0;
	Pool( PoolNum ).CurCoverEvapFac = 1.0;
	CalcSwimmingPoolEvap( EvapRate, PoolNum, SurfNum, MAT, HumRat );
	EXPECT_NEAR( 0.000788, EvapRate, 0.000001 );
	EXPECT_NEAR( 3570.0, Pool( PoolNum ).SatPressPoolWaterTemp, 10.0 );
	EXPECT_NEAR( 1600.0, Pool( PoolNum ).PartPressZoneAirTemp, 10.0 );
	
	
}

TEST_F( EnergyPlusFixture, SwimmingPool_InitSwimmingPoolPlantLoopIndex )
{

	bool MyPlantScanFlagPool;
	
	// Tests for CalcSwimmingPoolEvap--Evaporate Rate Calculation for Swimming Pools
	SwimmingPool::clear_state();
	DataPlant::clear_state();


	NumSwimmingPools = 2;
	TotNumLoops = 2;
	Pool.allocate( NumSwimmingPools );
	MyPlantScanFlagPool	= true;
	Pool( 1 ).Name = "FirstPool";
	Pool( 2 ).Name = "SecondPool";
	Pool( 1 ).WaterInletNode = 1;
	Pool( 2 ).WaterInletNode = 11;
	PlantLoop.allocate( TotNumLoops );
	PlantLoop( 1 ).LoopSide.allocate( 2 );
	PlantLoop( 2 ).LoopSide.allocate( 2 );
	PlantLoop( 1 ).LoopSide( 1 ).Branch.allocate( 1 );
	PlantLoop( 1 ).LoopSide( 2 ).Branch.allocate( 1 );
	PlantLoop( 2 ).LoopSide( 1 ).Branch.allocate( 1 );
	PlantLoop( 2 ).LoopSide( 2 ).Branch.allocate( 1 );
	PlantLoop( 1 ).LoopSide( 1 ).TotalBranches = 1;
	PlantLoop( 1 ).LoopSide( 2 ).TotalBranches = 1;
	PlantLoop( 2 ).LoopSide( 1 ).TotalBranches = 1;
	PlantLoop( 2 ).LoopSide( 2 ).TotalBranches = 1;
	PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).TotalComponents = 1;
	PlantLoop( 1 ).LoopSide( 2 ).Branch( 1 ).TotalComponents = 1;
	PlantLoop( 2 ).LoopSide( 1 ).Branch( 1 ).TotalComponents = 1;
	PlantLoop( 2 ).LoopSide( 2 ).Branch( 1 ).TotalComponents = 1;
	PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp.allocate( 1 );
	PlantLoop( 1 ).LoopSide( 2 ).Branch( 1 ).Comp.allocate( 1 );
	PlantLoop( 2 ).LoopSide( 1 ).Branch( 1 ).Comp.allocate( 1 );
	PlantLoop( 2 ).LoopSide( 2 ).Branch( 1 ).Comp.allocate( 1 );
	PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp.allocate( 1 );
	PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).TypeOf_Num = TypeOf_SwimmingPool_Indoor;
	PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).Name = "FirstPool";
	PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumIn = 1;
	PlantLoop( 2 ).LoopSide( 2 ).Branch( 1 ).Comp( 1 ).TypeOf_Num = TypeOf_SwimmingPool_Indoor;
	PlantLoop( 2 ).LoopSide( 2 ).Branch( 1 ).Comp( 1 ).Name = "SecondPool";
	PlantLoop( 2 ).LoopSide( 2 ).Branch( 1 ).Comp( 1 ).NodeNumIn = 11;
	
	// Test 1
	InitSwimmingPoolPlantLoopIndex( 1, MyPlantScanFlagPool );
	EXPECT_EQ( Pool( 1 ).HWLoopNum, 1 );
	EXPECT_EQ( Pool( 1 ).HWLoopSide, 1 );
	EXPECT_EQ( Pool( 1 ).HWBranchNum, 1 );
	EXPECT_EQ( Pool( 1 ).HWCompNum, 1 );
	EXPECT_EQ( MyPlantScanFlagPool, false );
	
	// Test 2
	MyPlantScanFlagPool	= true;
	InitSwimmingPoolPlantLoopIndex( 2, MyPlantScanFlagPool );
	EXPECT_EQ( Pool( 2 ).HWLoopNum, 2 );
	EXPECT_EQ( Pool( 2 ).HWLoopSide, 2 );
	EXPECT_EQ( Pool( 2 ).HWBranchNum, 1 );
	EXPECT_EQ( Pool( 2 ).HWCompNum, 1 );
	EXPECT_EQ( MyPlantScanFlagPool, false );
	
}
