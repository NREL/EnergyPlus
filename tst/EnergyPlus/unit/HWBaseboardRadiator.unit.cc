// EnergyPlus, Copyright (c) 1996-2016, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
//
// If you have questions about your rights to use or distribute this software, please contact
// Berkeley Lab's Innovation & Partnerships Office at IPO@lbl.gov.
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
//     similar designation, without Lawrence Berkeley National Laboratory's prior written consent.
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
//
// You are under no obligation whatsoever to provide any bug fixes, patches, or upgrades to the
// features, functionality or performance of the source code ("Enhancements") to anyone; however,
// if you choose to make your Enhancements available either publicly, or directly to Lawrence
// Berkeley National Laboratory, without imposing a separate written license agreement for such
// Enhancements, then you hereby grant the following license: a non-exclusive, royalty-free
// perpetual license to install, use, modify, prepare derivative works, incorporate into other
// computer software, distribute, and sublicense such enhancements or derivative works thereof,
// in binary and source code form.

// EnergyPlus::Stand alone unit test of Issue4347; i.e., CalcHWBaseboard NTU-eff calculation

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <DataZoneEnergyDemands.hh>
#include <DataLoopNode.hh>
#include <ScheduleManager.hh>
#include <Psychrometrics.hh>
#include <InputProcessor.hh>
#include <HWBaseboardRadiator.hh>
#include <FluidProperties.hh>
#include <DataPlant.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace DataZoneEnergyDemands;
using namespace ScheduleManager;
using namespace Psychrometrics;
using namespace InputProcessor;
using namespace HWBaseboardRadiator;
using namespace DataLoopNode;
using namespace FluidProperties;
using namespace DataPlant;

using namespace ObjexxFCL;

TEST_F( EnergyPlusFixture, HWBaseboardRadiator_CalcHWBaseboard)
{
	Real64 LoadMet;
	int BBNum;
	
	Node.allocate( 1 );
	HWBaseboard.allocate( 1 );
	ZoneSysEnergyDemand.allocate( 1 );
	CurDeadBandOrSetback.allocate( 1 );
	PlantLoop.allocate( 1 );
	QBBRadSource.allocate( 1 );
	
	Node( 1 ).MassFlowRate = 0.40;
	CurDeadBandOrSetback( 1 ) = false;
	ZoneSysEnergyDemand( 1 ).RemainingOutputReqToHeatSP = 12000.;
	BBNum = 1;
	LoadMet = 0.0;
	HWBaseboard( 1 ).ZonePtr = 1;
	HWBaseboard( 1 ).AirInletTemp = 21.;
	HWBaseboard( 1 ).WaterInletTemp = 82.;
	HWBaseboard( 1 ).WaterInletNode = 1;
	HWBaseboard( 1 ).WaterMassFlowRateMax = 0.40;
	HWBaseboard( 1 ).AirMassFlowRateStd = 0.5;
	HWBaseboard( 1 ).SchedPtr = -1;
	HWBaseboard( 1 ).LoopNum = 1;
	HWBaseboard( 1 ).UA = 370;
	PlantLoop( 1 ).FluidName = "Water";
	PlantLoop( 1 ).FluidIndex = 1;
	PlantLoop( 1 ).FluidType = 2;
	QBBRadSource( 1 ) = 0.0;
	
	CalcHWBaseboard( BBNum, LoadMet );

	EXPECT_NEAR( 14746.226690452937, HWBaseboard( 1 ).TotPower, 0.000001);
	EXPECT_NEAR( 50.349854486072232, HWBaseboard( 1 ).AirOutletTemp, 0.000001 );
	EXPECT_NEAR( 73.224991258180438, HWBaseboard( 1 ).WaterOutletTemp, 0.000001 );
	EXPECT_NEAR( 0.5, HWBaseboard( 1 ).AirMassFlowRate, 0.000001 );
	
	Node.deallocate();
	HWBaseboard.deallocate();
	ZoneSysEnergyDemand.deallocate();
	CurDeadBandOrSetback.deallocate();
	PlantLoop.deallocate();
	QBBRadSource.deallocate();
	
}

TEST_F( EnergyPlusFixture, HWBaseboardRadiator_HWBaseboardWaterFlowResetTest ) {
	Real64 LoadMet;
	int BBNum;

	BBNum = 1;
	LoadMet = 0.0;

	Node.allocate( 2 );
	HWBaseboard.allocate( 1 );
	ZoneSysEnergyDemand.allocate( 1 );
	CurDeadBandOrSetback.allocate( 1 );
	PlantLoop.allocate( 1 );
	QBBRadSource.allocate( 1 );
	

	CurDeadBandOrSetback( 1 ) = false;
	ZoneSysEnergyDemand( 1 ).RemainingOutputReqToHeatSP = 0.0; // zero load test
	
	HWBaseboard( 1 ).EquipID = "HWRadiativeConvectiveBB";
	HWBaseboard( 1 ).EquipType = TypeOf_Baseboard_Rad_Conv_Water;
	HWBaseboard( 1 ).ZonePtr = 1;
	HWBaseboard( 1 ).AirInletTemp = 21.0;
	HWBaseboard( 1 ).WaterInletTemp = 82.;
	HWBaseboard( 1 ).WaterInletNode = 1;
	HWBaseboard( 1 ).WaterOutletNode = 2;
	HWBaseboard( 1 ).WaterMassFlowRateMax = 0.40;
	HWBaseboard( 1 ).AirMassFlowRateStd = 0.5;
	HWBaseboard( 1 ).SchedPtr = -1;
	HWBaseboard( 1 ).LoopNum = 1;
	HWBaseboard( 1 ).LoopSideNum = 1;
	HWBaseboard( 1 ).BranchNum = 1;
	HWBaseboard( 1 ).UA = 400.0;
	PlantLoop( 1 ).FluidName = "Water";
	PlantLoop( 1 ).FluidIndex = 1;
	PlantLoop( 1 ).FluidType = 2;
	QBBRadSource( 1 ) = 0.0;

	Node( HWBaseboard( 1 ).WaterInletNode ).MassFlowRate = 0.2;
	Node( HWBaseboard( 1 ).WaterInletNode ).MassFlowRateMax = 0.4;
	Node( HWBaseboard( 1 ).WaterOutletNode ).MassFlowRate = 0.2;
	Node( HWBaseboard( 1 ).WaterOutletNode ).MassFlowRateMax = 0.4;

	TotNumLoops = 1;
	PlantLoop.allocate( TotNumLoops );
	for ( int l = 1; l <= TotNumLoops; ++l ) {
		auto & loop( PlantLoop( l ) );
		loop.LoopSide.allocate( 1 );
		auto & loopside( PlantLoop( l ).LoopSide( 1 ) );
		loopside.TotalBranches = 1;
		loopside.Branch.allocate( 1 );
		auto & loopsidebranch( PlantLoop( l ).LoopSide( 1 ).Branch( 1 ) );
		loopsidebranch.TotalComponents = 1;
		loopsidebranch.Comp.allocate( 1 );
	}
	PlantLoop( 1 ).Name = "HotWaterLoop";
	PlantLoop( 1 ).FluidName = "HotWater";
	PlantLoop( 1 ).FluidIndex = 1;
	PlantLoop( 1 ).FluidName = "WATER";
	PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).Name = HWBaseboard( 1 ).EquipID;
	PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).TypeOf_Num = HWBaseboard( 1 ).EquipType;
	PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumIn = HWBaseboard( 1 ).WaterInletNode;
	PlantLoop( 1 ).LoopSide( 1 ).Branch( 1 ).Comp( 1 ).NodeNumOut = HWBaseboard( 1 ).WaterOutletNode;

	// zero zone load case, so zero LoadMet must be returned
	CalcHWBaseboard( BBNum, LoadMet );

	EXPECT_EQ( 0.0, LoadMet );
	EXPECT_EQ( 0.0, HWBaseboard( 1 ).TotPower );
	EXPECT_EQ( 0.0, Node( HWBaseboard( 1 ).WaterInletNode ).MassFlowRate );
	EXPECT_EQ( HWBaseboard( 1 ).AirInletTemp, HWBaseboard( 1 ).AirOutletTemp );
	EXPECT_EQ( HWBaseboard( 1 ).WaterInletTemp, HWBaseboard( 1 ).WaterOutletTemp);
	EXPECT_EQ( 0.0, HWBaseboard( 1 ).AirMassFlowRate );
	
	// clear
	Node.deallocate();
	HWBaseboard.deallocate();
	ZoneSysEnergyDemand.deallocate();
	CurDeadBandOrSetback.deallocate();
	PlantLoop.deallocate();
	QBBRadSource.deallocate();

}