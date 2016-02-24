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

// EnergyPlus::Standalone ERV Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/Furnaces.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <ScheduleManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <ObjexxFCL/gio.hh>
#include <EnergyPlus/DataAirLoop.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace EnergyPlus::Furnaces;
using namespace ObjexxFCL;
using namespace EnergyPlus::DataHVACGlobals;
using namespace EnergyPlus::DataLoopNode;
using namespace DataZoneEnergyDemands;
using namespace DataGlobals;
using namespace ScheduleManager;
using namespace EnergyPlus::DataAirLoop;

namespace EnergyPlus {

	TEST_F( EnergyPlusFixture, SetVSHPAirFlowTest_VSFurnaceFlowTest )
	{

		int FurnaceNum( 1 );
		Real64 OnOffAirFlowRatio; // This is a return value
		Real64 PartLoadRatio( 1.0 );
		Node.allocate( 2 );
		CurDeadBandOrSetback.allocate( 1 );
		Schedule.allocate( 1 );

		MSHPMassFlowRateLow = 0.0;
		MSHPMassFlowRateHigh = 0.0;

		Furnace.allocate( 1 );

		Furnace( FurnaceNum ).FurnaceType_Num = UnitarySys_HeatCool;

		Furnace( FurnaceNum ).FurnaceInletNodeNum = 1;
		Furnace( FurnaceNum ).FurnaceOutletNodeNum = 2;
		Furnace( FurnaceNum ).ControlZoneNum = 1;

		Furnace( FurnaceNum ).MaxHeatAirMassFlow = 0.5;
		Furnace( FurnaceNum ).MaxCoolAirMassFlow = 0.75;

		Furnace( FurnaceNum ).HeatMassFlowRate.allocate( 3 );
		Furnace( FurnaceNum ).CoolMassFlowRate.allocate( 3 );
		Furnace( FurnaceNum ).MSHeatingSpeedRatio.allocate( 3 );
		Furnace( FurnaceNum ).MSCoolingSpeedRatio.allocate( 3 );

		Furnace( FurnaceNum ).LastMode = HeatingMode;
		Furnace( FurnaceNum ).IdleMassFlowRate = 0.2;
		Furnace( FurnaceNum ).IdleSpeedRatio = 0.2;
		Furnace( FurnaceNum ).FanAvailSchedPtr = ScheduleAlwaysOn;
		Furnace( FurnaceNum ).FurnaceInletNodeNum = 1;

		Furnace( FurnaceNum ).HeatMassFlowRate( 1 ) = 0.25;
		Furnace( FurnaceNum ).MSHeatingSpeedRatio( 1 ) = 0.25;
		Furnace( FurnaceNum ).HeatMassFlowRate( 2 ) = 0.5;
		Furnace( FurnaceNum ).MSHeatingSpeedRatio( 2 ) = 0.5;
		Furnace( FurnaceNum ).HeatMassFlowRate( 3 ) = 1.0;
		Furnace( FurnaceNum ).MSHeatingSpeedRatio( 3 ) = 1.0;

		Furnace( FurnaceNum ).CoolMassFlowRate( 1 ) = 0.3;
		Furnace( FurnaceNum ).MSCoolingSpeedRatio( 1 ) = 0.3;
		Furnace( FurnaceNum ).CoolMassFlowRate( 2 ) = 0.6;
		Furnace( FurnaceNum ).MSCoolingSpeedRatio( 2 ) = 0.6;
		Furnace( FurnaceNum ).CoolMassFlowRate( 3 ) = 1.2;
		Furnace( FurnaceNum ).MSCoolingSpeedRatio( 3 ) = 1.2;

		CurDeadBandOrSetback( 1 ) = false;

		Furnace( FurnaceNum ).OpMode = CycFanCycCoil;
		// heating air flow at various speeds

		Furnace( FurnaceNum ).NumOfSpeedHeating = 0;
		Furnace( FurnaceNum ).NumOfSpeedCooling = 0;
		//	Furnace( FurnaceNum ).SchedPtr = 0; // denotes incorrect schedule name in Furnace input ( returns 0.0 )
		Furnace( FurnaceNum ).SchedPtr = -1; // denotes missing schedule name in Furnace input ( returns 1.0 )
		HeatingLoad = true;
		CoolingLoad = false;
		SetVSHPAirFlow( FurnaceNum, PartLoadRatio, OnOffAirFlowRatio );

		EXPECT_DOUBLE_EQ( 0.0, CompOffMassFlow );
		EXPECT_DOUBLE_EQ( 0.5, CompOnMassFlow );
		EXPECT_DOUBLE_EQ( 1.0, OnOffAirFlowRatio );
		EXPECT_DOUBLE_EQ( 0.5, Node( Furnace( FurnaceNum ).FurnaceInletNodeNum ).MassFlowRate );

		Furnace( FurnaceNum ).NumOfSpeedHeating = 1;
		Furnace( FurnaceNum ).NumOfSpeedCooling = 0;
		HeatingLoad = true;
		CoolingLoad = false;
		SetVSHPAirFlow( FurnaceNum, PartLoadRatio, OnOffAirFlowRatio );
		EXPECT_DOUBLE_EQ( 0.25, MSHPMassFlowRateLow );
		EXPECT_DOUBLE_EQ( 0.25, MSHPMassFlowRateHigh );
		EXPECT_DOUBLE_EQ( 0.0, CompOffMassFlow );
		EXPECT_DOUBLE_EQ( 0.25, CompOnMassFlow );
		EXPECT_DOUBLE_EQ( 1.0, OnOffAirFlowRatio );
		EXPECT_DOUBLE_EQ( 0.25, Node( Furnace( FurnaceNum ).FurnaceInletNodeNum ).MassFlowRate );

		Furnace( FurnaceNum ).NumOfSpeedHeating = 2;
		Furnace( FurnaceNum ).NumOfSpeedCooling = 0;
		HeatingLoad = true;
		CoolingLoad = false;
		SetVSHPAirFlow( FurnaceNum, PartLoadRatio, OnOffAirFlowRatio );
		EXPECT_DOUBLE_EQ( 0.5, MSHPMassFlowRateLow );
		EXPECT_DOUBLE_EQ( 0.5, MSHPMassFlowRateHigh );
		EXPECT_DOUBLE_EQ( 0.0, CompOffMassFlow );
		EXPECT_DOUBLE_EQ( 0.5, CompOnMassFlow );
		EXPECT_DOUBLE_EQ( 1.0, OnOffAirFlowRatio );
		EXPECT_DOUBLE_EQ( 0.5, Node( Furnace( FurnaceNum ).FurnaceInletNodeNum ).MassFlowRate );

		Furnace( FurnaceNum ).NumOfSpeedHeating = 3;
		Furnace( FurnaceNum ).NumOfSpeedCooling = 0;
		HeatingLoad = true;
		CoolingLoad = false;
		SetVSHPAirFlow( FurnaceNum, PartLoadRatio, OnOffAirFlowRatio );
		EXPECT_DOUBLE_EQ( 1.0, MSHPMassFlowRateLow );
		EXPECT_DOUBLE_EQ( 1.0, MSHPMassFlowRateHigh );
		EXPECT_DOUBLE_EQ( 0.0, CompOffMassFlow );
		EXPECT_DOUBLE_EQ( 1.0, CompOnMassFlow );
		EXPECT_DOUBLE_EQ( 1.0, OnOffAirFlowRatio );
		EXPECT_DOUBLE_EQ( 1.0, Node( Furnace( FurnaceNum ).FurnaceInletNodeNum ).MassFlowRate );

		Furnace( FurnaceNum ).NumOfSpeedHeating = 0;
		Furnace( FurnaceNum ).NumOfSpeedCooling = 1;
		HeatingLoad = false;
		CoolingLoad = true;
		SetVSHPAirFlow( FurnaceNum, PartLoadRatio, OnOffAirFlowRatio );
		EXPECT_DOUBLE_EQ( 0.3, MSHPMassFlowRateLow );
		EXPECT_DOUBLE_EQ( 0.3, MSHPMassFlowRateHigh );
		EXPECT_DOUBLE_EQ( 0.0, CompOffMassFlow );
		EXPECT_DOUBLE_EQ( 0.3, CompOnMassFlow );
		EXPECT_DOUBLE_EQ( 1.0, OnOffAirFlowRatio );
		EXPECT_DOUBLE_EQ( 0.3, Node( Furnace( FurnaceNum ).FurnaceInletNodeNum ).MassFlowRate );

		Furnace( FurnaceNum ).NumOfSpeedHeating = 0;
		Furnace( FurnaceNum ).NumOfSpeedCooling = 2;
		HeatingLoad = false;
		CoolingLoad = true;
		SetVSHPAirFlow( FurnaceNum, PartLoadRatio, OnOffAirFlowRatio );
		EXPECT_DOUBLE_EQ( 0.6, MSHPMassFlowRateLow );
		EXPECT_DOUBLE_EQ( 0.6, MSHPMassFlowRateHigh );
		EXPECT_DOUBLE_EQ( 0.0, CompOffMassFlow );
		EXPECT_DOUBLE_EQ( 0.6, CompOnMassFlow );
		EXPECT_DOUBLE_EQ( 1.0, OnOffAirFlowRatio );
		EXPECT_DOUBLE_EQ( 0.6, Node( Furnace( FurnaceNum ).FurnaceInletNodeNum ).MassFlowRate );

		Furnace( FurnaceNum ).NumOfSpeedHeating = 0;
		Furnace( FurnaceNum ).NumOfSpeedCooling = 3;
		HeatingLoad = false;
		CoolingLoad = true;
		SetVSHPAirFlow( FurnaceNum, PartLoadRatio, OnOffAirFlowRatio );
		EXPECT_DOUBLE_EQ( 1.2, MSHPMassFlowRateLow );
		EXPECT_DOUBLE_EQ( 1.2, MSHPMassFlowRateHigh );
		EXPECT_DOUBLE_EQ( 0.0, CompOffMassFlow );
		EXPECT_DOUBLE_EQ( 1.2, CompOnMassFlow );
		EXPECT_DOUBLE_EQ( 1.0, OnOffAirFlowRatio );
		EXPECT_DOUBLE_EQ( 1.2, Node( Furnace( FurnaceNum ).FurnaceInletNodeNum ).MassFlowRate );

		// constant fan mode should drop to idle flow rate
		Furnace( FurnaceNum ).OpMode = ContFanCycCoil;

		Furnace( FurnaceNum ).NumOfSpeedHeating = 0;
		Furnace( FurnaceNum ).NumOfSpeedCooling = 0;
		HeatingLoad = true;
		CoolingLoad = false;
		SetVSHPAirFlow( FurnaceNum, PartLoadRatio, OnOffAirFlowRatio );
		EXPECT_EQ( 0.0, MSHPMassFlowRateLow );
		EXPECT_EQ( 0.0, MSHPMassFlowRateHigh );
		EXPECT_DOUBLE_EQ( 0.2, CompOffMassFlow );
		EXPECT_DOUBLE_EQ( 0.5, CompOnMassFlow );
		EXPECT_DOUBLE_EQ( 1.0, OnOffAirFlowRatio );
		EXPECT_DOUBLE_EQ( 0.5, Node( Furnace( FurnaceNum ).FurnaceInletNodeNum ).MassFlowRate );

		// Clean up
		Node.deallocate();
		Furnace.deallocate();
		CurDeadBandOrSetback.deallocate();
		Schedule.deallocate();

	}

	TEST_F( EnergyPlusFixture, FurnaceTest_PartLoadRatioTest )
	{
		// Test passing variables between Furnace and AirflowNetwork #5134

		using DataAirLoop::LoopSystemOnMassFlowrate;
		using DataAirLoop::LoopSystemOffMassFlowrate;
		using DataAirLoop::LoopFanOperationMode;
		using DataAirLoop::LoopOnOffFanPartLoadRatio;

		int FurnaceNum;

		FurnaceNum = 1;
		Furnace.allocate( 1 );
		Furnace( FurnaceNum ).FurnaceType_Num = UnitarySys_HeatPump_AirToAir;

		CompOnMassFlow = 2.0;
		CompOffMassFlow = 0.0;
		Furnace( FurnaceNum ).OpMode = 1;
		Furnace( FurnaceNum ).MdotFurnace = 2.0;
		Furnace( FurnaceNum ).DesignMassFlowRate = 2.2;
		Furnace( FurnaceNum ).HeatPartLoadRatio = 1.0;
		Furnace( FurnaceNum ).CoolPartLoadRatio = 0.0;

		ReportFurnace( FurnaceNum );

		EXPECT_EQ( 2.0, LoopSystemOnMassFlowrate );
		EXPECT_EQ( 0.0, LoopSystemOffMassFlowrate );
		EXPECT_EQ( 1.0, LoopFanOperationMode );
		EXPECT_EQ( 1.0, LoopOnOffFanPartLoadRatio );

		Furnace( FurnaceNum ).FurnaceType_Num = UnitarySys_HeatCool;
		Furnace( FurnaceNum ).HeatPartLoadRatio = 0.0;
		Furnace( FurnaceNum ).CoolPartLoadRatio = 0.0;
		Furnace( FurnaceNum ).MaxCoolAirMassFlow = 2.2;
		Furnace( FurnaceNum ).MaxHeatAirMassFlow = 2.0;

		ReportFurnace( FurnaceNum );

		EXPECT_EQ( 1.0, LoopOnOffFanPartLoadRatio );

		Furnace.deallocate();

	}
}

