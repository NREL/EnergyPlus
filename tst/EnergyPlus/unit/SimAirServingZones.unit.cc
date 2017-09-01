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

// EnergyPlus::SimAirServingZones Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// C++ Headers
#include <cassert>
#include <cmath>
#include <string>

// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <DataAirSystems.hh>
#include <DataSizing.hh>
#include <SimAirServingZones.hh>
#include <MixedAir.hh>
#include <UtilityRoutines.hh>
#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace DataAirSystems;
using namespace DataSizing;
using namespace ObjexxFCL;
using namespace SimAirServingZones;

namespace EnergyPlus {

	TEST( SimAirServingZones, ReheatCoilSizing )
	{
		// PURPOSE OF THIS SUBROUTINE:
		// This subroutine test the GetHeatingSATempForSizing & GetHeatingSATempHumRatForSizing methods, 
		// which are designed to get the proper reheat coil inlet temperature/humidity ratio for sizing
		// depending on the system configurations
	
		ShowMessage( "Begin Test: SimAirServingZones, ReheatCoilSizing" );
				
		int NumPrimaryAirSys = 4; // total number of air loops
		int AirLoopNum; // index of air loops
		int CtrlZoneNum; // index of zones
		
		// Allocate
		CalcSysSizing.allocate( NumPrimaryAirSys );
		FinalSysSizing.allocate( NumPrimaryAirSys );
		FinalZoneSizing.allocate( NumPrimaryAirSys );
		PrimaryAirSystem.allocate( NumPrimaryAirSys );
		
		// Inputs: system configurations: 
		// 	(1) Central heating coils exist
		// 	(2) No central heating coils, but preheating coils exist
		// 	(3) No central heating coils, but OA heat-exchangers exist
		// 	(4) No central heating coils; No preheating coils or OA heat-exchangers

		PrimaryAirSystem( 1 ).CentralHeatCoilExists = true;
		PrimaryAirSystem( 2 ).CentralHeatCoilExists = false;
		PrimaryAirSystem( 3 ).CentralHeatCoilExists = false;
		PrimaryAirSystem( 4 ).CentralHeatCoilExists = false;
		
		PrimaryAirSystem( 1 ).NumOAHeatCoils = 0;
		PrimaryAirSystem( 2 ).NumOAHeatCoils = 1;
		PrimaryAirSystem( 3 ).NumOAHeatCoils = 0;
		PrimaryAirSystem( 4 ).NumOAHeatCoils = 0;
		
		PrimaryAirSystem( 1 ).NumOAHXs = 0;
		PrimaryAirSystem( 2 ).NumOAHXs = 0;
		PrimaryAirSystem( 3 ).NumOAHXs = 1;
		PrimaryAirSystem( 4 ).NumOAHXs = 0;
		
		// Inputs: sizing parameters
		for ( AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum ) {
			FinalSysSizing( AirLoopNum ).DesOutAirVolFlow = 0.25;
			FinalSysSizing( AirLoopNum ).DesHeatVolFlow = 0.50; 
			
			FinalSysSizing( AirLoopNum ).PreheatTemp = 7;
			FinalSysSizing( AirLoopNum ).HeatRetTemp = 22; 
			FinalSysSizing( AirLoopNum ).HeatMixTemp = 10;
			CalcSysSizing( AirLoopNum ).HeatSupTemp = 17;
			
			FinalSysSizing( AirLoopNum ).PreheatHumRat = 0.003;
			FinalSysSizing( AirLoopNum ).HeatRetHumRat = 0.008; 
			FinalSysSizing( AirLoopNum ).HeatMixHumRat = 0.004;
			CalcSysSizing( AirLoopNum ).HeatSupHumRat = 0.006;

		}
		
		// Run 
		for ( AirLoopNum = 1; AirLoopNum <= NumPrimaryAirSys; ++AirLoopNum ) {
			CtrlZoneNum = AirLoopNum;

			FinalZoneSizing( CtrlZoneNum ).DesHeatCoilInTempTU = GetHeatingSATempForSizing( AirLoopNum );
			FinalZoneSizing( CtrlZoneNum ).DesHeatCoilInHumRatTU = GetHeatingSATempHumRatForSizing( AirLoopNum );
		}
		
		// Check
		EXPECT_EQ( 17.0, FinalZoneSizing( 1 ).DesHeatCoilInTempTU );
		EXPECT_NEAR( 14.5, FinalZoneSizing( 2 ).DesHeatCoilInTempTU, 0.05 );
		EXPECT_NEAR( 14.5, FinalZoneSizing( 3 ).DesHeatCoilInTempTU, 0.05 );
		EXPECT_EQ( 10.0, FinalZoneSizing( 4 ).DesHeatCoilInTempTU );
		EXPECT_EQ( 0.006, FinalZoneSizing( 1 ).DesHeatCoilInHumRatTU );
		EXPECT_EQ( 0.0055, FinalZoneSizing( 2 ).DesHeatCoilInHumRatTU );
		EXPECT_EQ( 0.0055, FinalZoneSizing( 3 ).DesHeatCoilInHumRatTU );
		EXPECT_EQ( 0.004, FinalZoneSizing( 4 ).DesHeatCoilInHumRatTU );
		
		// Clean up
		CalcSysSizing.deallocate( );
		FinalSysSizing.deallocate( );
		FinalZoneSizing.deallocate( );
		PrimaryAirSystem.deallocate( ); 

	}

	TEST_F( EnergyPlusFixture, SimAirServingZones_LimitZoneVentEff ) {
		int CtrlZoneNum = 1;
		FinalZoneSizing.allocate( 1 );

		// Test case 1, low OA, low zoneventilationeff, no change in SysCoolingEv
		Real64 StartingDesCoolVolFlow = 1.0;
		Real64 StartingDesCoolVolFlowMin = 0.2;
		Real64 UncorrectedOAFlow = 0.1;
		FinalZoneSizing( CtrlZoneNum ).DesCoolVolFlow = StartingDesCoolVolFlow;
		FinalZoneSizing( CtrlZoneNum ).DesCoolVolFlowMin = StartingDesCoolVolFlowMin;
		FinalZoneSizing( CtrlZoneNum ).ZoneSecondaryRecirculation = 0.0;
		FinalZoneSizing( CtrlZoneNum ).ZoneVentilationEff = 0.5;
		Real64 Xs = 0.25; // uncorrected system outdoor air fraction
		Real64 VozClg = UncorrectedOAFlow; // corrected (for ventilation efficiency) zone outside air flow rate [m3/s] 
		Real64 ZoneOAFrac = UncorrectedOAFlow / StartingDesCoolVolFlowMin; // zone OA fraction

		Real64 SysCoolingEv = 1.0 + Xs - ZoneOAFrac; // System level ventilation effectiveness for cooling (from SimAirServingZone::UpdateSysSizing right before call to LimitZoneVentEff)
		Real64 StartingSysCoolingEv = SysCoolingEv;
		LimitZoneVentEff( Xs, VozClg, CtrlZoneNum, SysCoolingEv );
		EXPECT_EQ( StartingSysCoolingEv, SysCoolingEv );
		EXPECT_EQ( StartingDesCoolVolFlow, FinalZoneSizing( CtrlZoneNum ).DesCoolVolFlow );
		EXPECT_EQ( StartingDesCoolVolFlowMin, FinalZoneSizing( CtrlZoneNum ).DesCoolVolFlowMin );

		// Test case 2, low OA, high zoneventilationeff, increase SysCoolingEv and DesCoolVolFlowMin
		StartingDesCoolVolFlow = 1.0;
		StartingDesCoolVolFlowMin = 0.2;
		UncorrectedOAFlow = 0.1;
		FinalZoneSizing( CtrlZoneNum ).DesCoolVolFlow = StartingDesCoolVolFlow;
		FinalZoneSizing( CtrlZoneNum ).DesCoolVolFlowMin = StartingDesCoolVolFlowMin;
		FinalZoneSizing( CtrlZoneNum ).ZoneSecondaryRecirculation = 0.0;
		FinalZoneSizing( CtrlZoneNum ).ZoneVentilationEff = 0.9;
		Xs = 0.25; // uncorrected system outdoor air fraction
		VozClg = UncorrectedOAFlow; // corrected (for ventilation efficiency) zone outside air flow rate [m3/s] 
		ZoneOAFrac = UncorrectedOAFlow / StartingDesCoolVolFlowMin; // zone OA fraction

		SysCoolingEv = 1.0 + Xs - ZoneOAFrac; // System level ventilation effectiveness for cooling (from SimAirServingZone::UpdateSysSizing right before call to LimitZoneVentEff)
		StartingSysCoolingEv = SysCoolingEv;
		LimitZoneVentEff( Xs, VozClg, CtrlZoneNum, SysCoolingEv );
		EXPECT_EQ( FinalZoneSizing( CtrlZoneNum ).ZoneVentilationEff, SysCoolingEv );
		EXPECT_EQ( StartingDesCoolVolFlow, FinalZoneSizing( CtrlZoneNum ).DesCoolVolFlow );
		EXPECT_NEAR( 0.2857, FinalZoneSizing( CtrlZoneNum ).DesCoolVolFlowMin, 0.001 );

		// Test case 3, high OA, high zoneventilationeff, increase SysCoolingEv, DesCoolVolFlowMin, and DesCoolVolFlow
		StartingDesCoolVolFlow = 1.0;
		StartingDesCoolVolFlowMin = 0.8;
		UncorrectedOAFlow = 0.8;
		FinalZoneSizing( CtrlZoneNum ).DesCoolVolFlow = StartingDesCoolVolFlow;
		FinalZoneSizing( CtrlZoneNum ).DesCoolVolFlowMin = StartingDesCoolVolFlowMin;
		FinalZoneSizing( CtrlZoneNum ).ZoneSecondaryRecirculation = 0.0;
		FinalZoneSizing( CtrlZoneNum ).ZoneVentilationEff = 0.9;
		Xs = 0.25; // uncorrected system outdoor air fraction
		VozClg = UncorrectedOAFlow; // corrected (for ventilation efficiency) zone outside air flow rate [m3/s] 
		ZoneOAFrac = UncorrectedOAFlow / StartingDesCoolVolFlowMin; // zone OA fraction

		SysCoolingEv = 1.0 + Xs - ZoneOAFrac; // System level ventilation effectiveness for cooling (from SimAirServingZone::UpdateSysSizing right before call to LimitZoneVentEff)
		StartingSysCoolingEv = SysCoolingEv;
		LimitZoneVentEff( Xs, VozClg, CtrlZoneNum, SysCoolingEv );
		EXPECT_EQ( FinalZoneSizing( CtrlZoneNum ).ZoneVentilationEff, SysCoolingEv );
		EXPECT_NEAR( 2.2857, FinalZoneSizing( CtrlZoneNum ).DesCoolVolFlow, 0.001 );
		EXPECT_NEAR( 2.2857, FinalZoneSizing( CtrlZoneNum ).DesCoolVolFlowMin, 0.001 );


	}

	TEST_F( EnergyPlusFixture, SizingSystem_FlowPerCapacityMethodTest1 ) {
		// this unit test is related to issue #5835
		// when system capacit is hard sized user input
		int AirLoopNum( 0 ); // index of air loops
		Real64 ScaledCoolDesignFlowRate( 0.0 ); //system cooling design flow rate
		Real64 ScaledHeatDesignFlowRate( 0.0 ); //system heating design flow rate

		AirLoopNum = 1;
		CalcSysSizing.allocate( AirLoopNum );
		FinalSysSizing.allocate( AirLoopNum );
		
		// set system flow sizing method for cooling
		FinalSysSizing( AirLoopNum ).ScaleCoolSAFMethod = FlowPerCoolingCapacity;
		FinalSysSizing( AirLoopNum ).CoolingCapMethod = CoolingDesignCapacity;
		FinalSysSizing( AirLoopNum ).ScaledCoolingCapacity = 12500.0;
		FinalSysSizing( AirLoopNum ).FlowPerCoolingCapacity = 0.00006041;
		// scale cooling flow rate using user input capacity
		ScaledCoolDesignFlowRate = FinalSysSizing( AirLoopNum ).ScaledCoolingCapacity * FinalSysSizing( AirLoopNum ).FlowPerCoolingCapacity;
		// do scaleable flow sizing
		UpdateSysSizingForScalableInputs( AirLoopNum );
		EXPECT_DOUBLE_EQ( 0.755125, ScaledCoolDesignFlowRate );
		EXPECT_DOUBLE_EQ( 0.755125, FinalSysSizing( AirLoopNum ).InpDesCoolAirFlow );

		// set system flow sizing method for heating
		FinalSysSizing( AirLoopNum ).ScaleHeatSAFMethod = FlowPerHeatingCapacity;
		FinalSysSizing( AirLoopNum ).HeatingCapMethod = HeatingDesignCapacity;
		FinalSysSizing( AirLoopNum ).ScaledHeatingCapacity = 14400.0;
		FinalSysSizing( AirLoopNum ).FlowPerHeatingCapacity = 0.00006041;
		// scale heating flow rate using user input capacity
		ScaledHeatDesignFlowRate = FinalSysSizing( AirLoopNum ).ScaledHeatingCapacity * FinalSysSizing( AirLoopNum ).FlowPerHeatingCapacity;
		// do scaleable flow sizing
		UpdateSysSizingForScalableInputs( AirLoopNum );
		EXPECT_DOUBLE_EQ( 0.869904, ScaledHeatDesignFlowRate );
		EXPECT_DOUBLE_EQ( 0.869904, FinalSysSizing( AirLoopNum ).InpDesHeatAirFlow );

	}

	TEST_F( EnergyPlusFixture, SizingSystem_FlowPerCapacityMethodTest2 ) {
		// this unit test is related to issue #5835
		// when system capacity is scaled using floor area
		int AirLoopNum( 0 ); // index of air loops
		Real64 ScaledCoolDesignFlowRate( 0.0 ); // system cooling design flow rate
		Real64 ScaledHeatDesignFlowRate( 0.0 ); // system heating design flow rate
		Real64 ScaledCoolDesignCapacity( 0.0 ); // system cooling design capacity
		Real64 ScaledHeatDesignCapacity( 0.0 ); // system heating design capacity

		AirLoopNum = 1;
		CalcSysSizing.allocate( AirLoopNum );
		FinalSysSizing.allocate( AirLoopNum );

		// set system flow sizing method for cooling
		FinalSysSizing( AirLoopNum ).ScaleCoolSAFMethod = FlowPerCoolingCapacity;
		FinalSysSizing( AirLoopNum ).CoolingCapMethod = CapacityPerFloorArea;
		FinalSysSizing( AirLoopNum ).ScaledCoolingCapacity = 10.4732; // Watts per m2 floor area
		FinalSysSizing( AirLoopNum ).FlowPerCoolingCapacity = 0.00006041;
		FinalSysSizing( AirLoopNum ).FloorAreaOnAirLoopCooled = 61.450534421531373;
		// scale cooling capacity using floor area
		ScaledCoolDesignCapacity = FinalSysSizing( AirLoopNum ).ScaledCoolingCapacity * FinalSysSizing( AirLoopNum ).FloorAreaOnAirLoopCooled;
		ScaledCoolDesignFlowRate = FinalSysSizing( AirLoopNum ).FlowPerCoolingCapacity * ScaledCoolDesignCapacity;
		// do scaleable flow sizing
		UpdateSysSizingForScalableInputs( AirLoopNum );
		EXPECT_DOUBLE_EQ( 0.038878893558427413, ScaledCoolDesignFlowRate );
		EXPECT_DOUBLE_EQ( 0.038878893558427413, FinalSysSizing( AirLoopNum ).InpDesCoolAirFlow );

		// set system flow sizing method for heating
		FinalSysSizing( AirLoopNum ).ScaleHeatSAFMethod = FlowPerHeatingCapacity;
		FinalSysSizing( AirLoopNum ).HeatingCapMethod = CapacityPerFloorArea;
		FinalSysSizing( AirLoopNum ).ScaledHeatingCapacity = 32.0050; // Watts per m2 floor area
		FinalSysSizing( AirLoopNum ).FlowPerHeatingCapacity = 0.00006041;
		FinalSysSizing( AirLoopNum ).FloorAreaOnAirLoopCooled = 61.450534421531373;
		// scale heating capacity using floor area
		ScaledHeatDesignCapacity = FinalSysSizing( AirLoopNum ).ScaledHeatingCapacity * FinalSysSizing( AirLoopNum ).FloorAreaOnAirLoopCooled;
		ScaledHeatDesignFlowRate = FinalSysSizing( AirLoopNum ).FlowPerHeatingCapacity * ScaledHeatDesignCapacity;
		// do scaleable flow sizing
		UpdateSysSizingForScalableInputs( AirLoopNum );
		EXPECT_DOUBLE_EQ( 0.11880981823487276, ScaledHeatDesignFlowRate );
		EXPECT_DOUBLE_EQ( 0.11880981823487276, FinalSysSizing( AirLoopNum ).InpDesHeatAirFlow );

	}

}

