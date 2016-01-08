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

// EnergyPlus::WaterToAirHeatPumpSimple Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

#include "Fixtures/EnergyPlusFixture.hh"

// EnergyPlus Headers
#include <ObjexxFCL/gio.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/General.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/WaterToAirHeatPumpSimple.hh>

using namespace EnergyPlus;
using namespace ObjexxFCL;
using namespace EnergyPlus::DataEnvironment;
using namespace EnergyPlus::DataHVACGlobals;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::Psychrometrics;
using namespace EnergyPlus::WaterToAirHeatPumpSimple;
using General::RoundSigDigits;

TEST_F( EnergyPlusFixture, WaterToAirHeatPumpSimpleTest_SizeHVACWaterToAir )
{
	// This unit test is intended to check if supply air Humidity ratio used in the cooling sizing calculation is 
	// reset to the minimum of entering mixed air humidity ratio and the user specified supply air design Humidity  
	// ratio such that the total cooling capacity is always greater than or equal to the sensible cooling capacity.
	// This test was added to test bug issue #4893 fix, a defect that resulted in SHR greater than 1.0.

	int HPNum( 1 );

	SysSizingRunDone = true;
	ZoneSizingRunDone = true;
	CurSysNum = 0;
	CurZoneEqNum = 1;

	SimpleWatertoAirHP.allocate( HPNum );
	FinalZoneSizing.allocate( CurZoneEqNum );
	ZoneEqSizing.allocate( CurZoneEqNum );
	DesDayWeath.allocate( 1 );
	DesDayWeath( 1 ).Temp.allocate( 24 );

	SimpleWatertoAirHP( HPNum ).WatertoAirHPType = "COOLING";
	SimpleWatertoAirHP( HPNum ).RatedAirVolFlowRate = AutoSize;
	SimpleWatertoAirHP( HPNum ).RatedCapCoolTotal = AutoSize;
	SimpleWatertoAirHP( HPNum ).RatedCapCoolSens = AutoSize;
	SimpleWatertoAirHP( HPNum ).RatedWaterVolFlowRate = 0.0;

	FinalZoneSizing( CurZoneEqNum ).DesCoolVolFlow = 0.20;
	FinalZoneSizing( CurZoneEqNum ).DesHeatVolFlow = 0.20;
	FinalZoneSizing( CurZoneEqNum ).CoolDesTemp = 13.0;
	FinalZoneSizing( CurZoneEqNum ).CoolDesHumRat = 0.0075;
	FinalZoneSizing( CurZoneEqNum ).TimeStepNumAtCoolMax = 15;
	FinalZoneSizing( CurZoneEqNum ).CoolDDNum = 1;
	FinalZoneSizing( CurZoneEqNum ).DesCoolCoilInTemp = 25.5;
	FinalZoneSizing( CurZoneEqNum ).DesCoolCoilInHumRat = 0.0045;
	FinalZoneSizing( CurZoneEqNum ).ZoneRetTempAtCoolPeak = 25.5;
	FinalZoneSizing( CurZoneEqNum ).ZoneHumRatAtCoolPeak = 0.0045;
	ZoneEqSizing( CurZoneEqNum ).OAVolFlow = 0.0;

	// performance curve coefficients
	SimpleWatertoAirHP( HPNum ).TotalCoolCap1 = -9.149069561;
	SimpleWatertoAirHP( HPNum ).TotalCoolCap2 = 10.878140260;
	SimpleWatertoAirHP( HPNum ).TotalCoolCap3 = -1.718780157;
	SimpleWatertoAirHP( HPNum ).TotalCoolCap4 =  0.746414818;
	SimpleWatertoAirHP( HPNum ).TotalCoolCap5 =  0.0;

	SimpleWatertoAirHP( HPNum ).RatedCOPCool = 5.12;
	SimpleWatertoAirHP( HPNum ).SensCoolCap1 = -5.462690012;
	SimpleWatertoAirHP( HPNum ).SensCoolCap2 = 17.95968138;
	SimpleWatertoAirHP( HPNum ).SensCoolCap3 =-11.87818402;
	SimpleWatertoAirHP( HPNum ).SensCoolCap4 = -0.980163419;
	SimpleWatertoAirHP( HPNum ).SensCoolCap5 =  0.767285761;
	SimpleWatertoAirHP( HPNum ).SensCoolCap6 = 0.0;

	DesDayWeath( 1 ).Temp( 15 ) = 32.0;
	StdBaroPress = 101325.0;
	ZoneEqDXCoil = true;

	WaterToAirHeatPumpSimple::SizeHVACWaterToAir( HPNum );

	// check that the design oulet air humidity ratio did not change
	EXPECT_DOUBLE_EQ( 0.0075, FinalZoneSizing( CurZoneEqNum ).CoolDesHumRat );

	// check that the total cooling capacity is >= the sensible cooling capacity
	EXPECT_GE( SimpleWatertoAirHP( HPNum ).RatedCapCoolTotal, SimpleWatertoAirHP( HPNum ).RatedCapCoolSens );

	if ( SimpleWatertoAirHP( HPNum ).RatedCapCoolTotal != 0.0 ) {
		ShowMessage( "SizeHVACWaterToAir: Rated Sensible Heat Ratio = " + RoundSigDigits( SimpleWatertoAirHP( HPNum ).RatedCapCoolSens / SimpleWatertoAirHP( HPNum ).RatedCapCoolTotal, 2 ) + " [-]" );
	} 

	// clean up
	SimpleWatertoAirHP.deallocate();
	FinalZoneSizing.deallocate();
	ZoneEqSizing.deallocate();
	DesDayWeath( 1 ).Temp.deallocate();
	DesDayWeath.deallocate();
}
