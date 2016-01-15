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

// EnergyPlus::Heat Recovery Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/Fans.hh>
#include <EnergyPlus/HeatRecovery.hh>
#include <Psychrometrics.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <ObjexxFCL/gio.hh>

using namespace EnergyPlus;
using namespace DataEnvironment;
using namespace DataGlobals;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::DataHVACGlobals;
using namespace EnergyPlus::DataLoopNode;
using namespace EnergyPlus::Fans;
using namespace EnergyPlus::HeatRecovery;
using namespace EnergyPlus::Psychrometrics;

TEST_F( EnergyPlusFixture, HeatRecovery_HRTest )
{
	CurZoneEqNum = 0;
	CurSysNum = 0;
	CurOASysNum = 0;
	NumHeatExchangers = 1;
	ExchCond.allocate( NumHeatExchangers );
	Node.allocate( 4 );
	OutBaroPress = 101325.0;

	int ExchNum = 1;
	int CompanionCoilNum = 0;
	bool HXUnitOn = false;
	bool FirstHVACIteration = false;
	bool EconomizerFlag = false;
	bool HighHumCtrlFlag = false;
	int FanOpMode = 2; // 1 = cycling fan, 2 = constant fan
	Real64 Toutlet = 0.0;
	Real64 Tnode = 0.0;
	Real64 SetPointTemp = 19.0;
	Real64 PartLoadRatio = 0.25;

	CurZoneEqNum = 0;
	CurSysNum = 0;
	CurOASysNum = 0;

	ExchCond( ExchNum ).NomSupAirVolFlow = 1.0;
	ExchCond( ExchNum ).SupInMassFlow = 1.0;
	ExchCond( ExchNum ).SecInMassFlow = 1.0;
	ExchCond( ExchNum ).SupInletNode = 1;
	ExchCond( ExchNum ).SupOutletNode = 2;
	ExchCond( ExchNum ).SecInletNode = 3;
	ExchCond( ExchNum ).SecOutletNode = 4;
	ExchCond( ExchNum ).SchedPtr = -1;
	ExchCond( ExchNum ).HeatEffectSensible75 = 0.75;
	ExchCond( ExchNum ).HeatEffectSensible100 = 0.75;
	ExchCond( ExchNum ).HeatEffectLatent75 = 0.0;
	ExchCond( ExchNum ).HeatEffectLatent100 = 0.0;
	ExchCond( ExchNum ).CoolEffectSensible75 = 0.75;
	ExchCond( ExchNum ).CoolEffectSensible100 = 0.75;
	ExchCond( ExchNum ).CoolEffectLatent75 = 0.0;
	ExchCond( ExchNum ).CoolEffectLatent100 = 0.0;

	ExchCond( ExchNum ).Name = "Test Heat Recovery 1";
	ExchCond( ExchNum ).ExchTypeNum = HX_AIRTOAIR_GENERIC;
	ExchCond( ExchNum ).SupInTemp = 24.0;
	ExchCond( ExchNum ).SecInTemp = 15.0;
	ExchCond( ExchNum ).SupInHumRat = 0.01;
	ExchCond( ExchNum ).SecInHumRat = 0.01;
	ExchCond( ExchNum ).SupInEnth = PsyHFnTdbW( ExchCond( ExchNum ).SupInTemp, ExchCond( ExchNum ).SupInHumRat );
	ExchCond( ExchNum ).SecInEnth = PsyHFnTdbW( ExchCond( ExchNum ).SecInTemp, ExchCond( ExchNum ).SecInHumRat );
	Node( ExchCond( ExchNum ).SupInletNode ).Temp = ExchCond( ExchNum ).SupInTemp;
	Node( ExchCond( ExchNum ).SecInletNode ).Temp = ExchCond( ExchNum ).SecInTemp;
	Node( ExchCond( ExchNum ).SupInletNode ).HumRat = ExchCond( ExchNum ).SupInHumRat;
	Node( ExchCond( ExchNum ).SecInletNode ).HumRat = ExchCond( ExchNum ).SecInHumRat;
	Node( ExchCond( ExchNum ).SupInletNode ).Enthalpy = ExchCond( ExchNum ).SupInEnth;
	Node( ExchCond( ExchNum ).SecInletNode ).Enthalpy = ExchCond( ExchNum ).SecInEnth;
	Node( ExchCond( ExchNum ).SupInletNode ).MassFlowRate = ExchCond( ExchNum ).SupInMassFlow;
	Node( ExchCond( ExchNum ).SecInletNode ).MassFlowRate = ExchCond( ExchNum ).SecInMassFlow;

	// HXUnitOn is false so expect outlet = inlet
	InitHeatRecovery( ExchNum, CompanionCoilNum );
	CalcAirToAirGenericHeatExch( ExchNum, HXUnitOn, FirstHVACIteration, FanOpMode, EconomizerFlag, HighHumCtrlFlag );
	UpdateHeatRecovery( ExchNum );
	Toutlet = ExchCond( ExchNum ).SupInTemp;
	Tnode = ExchCond( ExchNum ).SupOutTemp;
	EXPECT_DOUBLE_EQ( Toutlet, Tnode );

	ExchCond( ExchNum ).ControlToTemperatureSetPoint = false;
	Node( ExchCond( ExchNum ).SupOutletNode ).TempSetPoint = SetPointTemp;

	// HXUnitOn is true and ControlToTemperatureSetPoint is false so expect outlet = temperature based on effectiveness
	HXUnitOn = true;
	ExchCond( ExchNum ).ExchConfigNum = Plate;
	InitHeatRecovery( ExchNum, CompanionCoilNum );
	CalcAirToAirGenericHeatExch( ExchNum, HXUnitOn, FirstHVACIteration, FanOpMode, EconomizerFlag, HighHumCtrlFlag );
	UpdateHeatRecovery( ExchNum );
	Toutlet = ( ExchCond( ExchNum ).SupInTemp + ( ExchCond( ExchNum ).CoolEffectSensible75 * ( ExchCond( ExchNum ).SecInTemp - ExchCond( ExchNum ).SupInTemp ) ) );
	Tnode = ExchCond( ExchNum ).SupOutTemp;
	EXPECT_DOUBLE_EQ( Toutlet, Tnode );

	ExchCond( ExchNum ).ExchConfigNum = Rotary;
	HXUnitOn = true;
	InitHeatRecovery( ExchNum, CompanionCoilNum );
	CalcAirToAirGenericHeatExch( ExchNum, HXUnitOn, FirstHVACIteration, FanOpMode, EconomizerFlag, HighHumCtrlFlag );
	UpdateHeatRecovery( ExchNum );
	Toutlet = ( ExchCond( ExchNum ).SupInTemp + ( ExchCond( ExchNum ).CoolEffectSensible75 * ( ExchCond( ExchNum ).SecInTemp - ExchCond( ExchNum ).SupInTemp ) ) );
	Tnode = ExchCond( ExchNum ).SupOutTemp;
	EXPECT_DOUBLE_EQ( Toutlet, Tnode );

	ExchCond( ExchNum ).ControlToTemperatureSetPoint = true;
	Node( ExchCond( ExchNum ).SupOutletNode ).TempSetPoint = 19.0;

	// HXUnitOn is true and ControlToTemperatureSetPoint is true so expect outlet = set point temperature
	HXUnitOn = true;
	ExchCond( ExchNum ).ExchConfigNum = Plate;
	InitHeatRecovery( ExchNum, CompanionCoilNum );
	CalcAirToAirGenericHeatExch( ExchNum, HXUnitOn, FirstHVACIteration, FanOpMode, EconomizerFlag, HighHumCtrlFlag );
	UpdateHeatRecovery( ExchNum );
	Toutlet = SetPointTemp;
	Tnode = ExchCond( ExchNum ).SupOutTemp;
	EXPECT_DOUBLE_EQ( Toutlet, Tnode );

	ExchCond( ExchNum ).ExchConfigNum = Rotary;
	HXUnitOn = true;
	InitHeatRecovery( ExchNum, CompanionCoilNum );
	CalcAirToAirGenericHeatExch( ExchNum, HXUnitOn, FirstHVACIteration, FanOpMode, EconomizerFlag, HighHumCtrlFlag );
	UpdateHeatRecovery( ExchNum );
	Toutlet = Node( ExchCond( ExchNum ).SupOutletNode ).TempSetPoint;
	Tnode = Node( ExchCond( ExchNum ).SupOutletNode ).Temp;
	EXPECT_DOUBLE_EQ( Toutlet, Tnode );

	ExchCond( ExchNum ).Name = "Test Heat Recovery 2";
	ExchCond( ExchNum ).ExchTypeNum = HX_AIRTOAIR_GENERIC;
	ExchCond( ExchNum ).SupInTemp = 15.0;
	ExchCond( ExchNum ).SecInTemp = 24.0;
	ExchCond( ExchNum ).SupInHumRat = 0.01;
	ExchCond( ExchNum ).SecInHumRat = 0.01;
	ExchCond( ExchNum ).SupInEnth = PsyHFnTdbW( ExchCond( ExchNum ).SupInTemp, ExchCond( ExchNum ).SupInHumRat );
	ExchCond( ExchNum ).SecInEnth = PsyHFnTdbW( ExchCond( ExchNum ).SecInTemp, ExchCond( ExchNum ).SecInHumRat );
	Node( ExchCond( ExchNum ).SupInletNode ).Temp = ExchCond( ExchNum ).SupInTemp;
	Node( ExchCond( ExchNum ).SecInletNode ).Temp = ExchCond( ExchNum ).SecInTemp;
	Node( ExchCond( ExchNum ).SupInletNode ).HumRat = ExchCond( ExchNum ).SupInHumRat;
	Node( ExchCond( ExchNum ).SecInletNode ).HumRat = ExchCond( ExchNum ).SecInHumRat;
	Node( ExchCond( ExchNum ).SupInletNode ).Enthalpy = ExchCond( ExchNum ).SupInEnth;
	Node( ExchCond( ExchNum ).SecInletNode ).Enthalpy = ExchCond( ExchNum ).SecInEnth;
	Node( ExchCond( ExchNum ).SupInletNode ).MassFlowRate = ExchCond( ExchNum ).SupInMassFlow;
	Node( ExchCond( ExchNum ).SecInletNode ).MassFlowRate = ExchCond( ExchNum ).SecInMassFlow;

	// HXUnitOn is false so expect outlet = inlet
	HXUnitOn = false;
	InitHeatRecovery( ExchNum, CompanionCoilNum );
	CalcAirToAirGenericHeatExch( ExchNum, HXUnitOn, FirstHVACIteration, FanOpMode, EconomizerFlag, HighHumCtrlFlag );
	UpdateHeatRecovery( ExchNum );
	EXPECT_DOUBLE_EQ( ExchCond( ExchNum ).SupInTemp, Node( ExchCond( ExchNum ).SupOutletNode ).Temp );

	ExchCond( ExchNum ).ControlToTemperatureSetPoint = false;
	Node( ExchCond( ExchNum ).SupOutletNode ).TempSetPoint = 19.0;

	// HXUnitOn is true and ControlToTemperatureSetPoint is false so expect outlet = temperature based on effectiveness
	HXUnitOn = true;
	ExchCond( ExchNum ).ExchConfigNum = Plate;
	InitHeatRecovery( ExchNum, CompanionCoilNum );
	CalcAirToAirGenericHeatExch( ExchNum, HXUnitOn, FirstHVACIteration, FanOpMode, EconomizerFlag, HighHumCtrlFlag );
	UpdateHeatRecovery( ExchNum );
	EXPECT_DOUBLE_EQ( ( ExchCond( ExchNum ).SupInTemp + ( ExchCond( ExchNum ).CoolEffectSensible75 * ( ExchCond( ExchNum ).SecInTemp - ExchCond( ExchNum ).SupInTemp ) ) ), Node( ExchCond( ExchNum ).SupOutletNode ).Temp );

	ExchCond( ExchNum ).ExchConfigNum = Rotary;
	HXUnitOn = true;
	InitHeatRecovery( ExchNum, CompanionCoilNum );
	CalcAirToAirGenericHeatExch( ExchNum, HXUnitOn, FirstHVACIteration, FanOpMode, EconomizerFlag, HighHumCtrlFlag );
	UpdateHeatRecovery( ExchNum );
	EXPECT_DOUBLE_EQ( ( ExchCond( ExchNum ).SupInTemp + ( ExchCond( ExchNum ).CoolEffectSensible75 * ( ExchCond( ExchNum ).SecInTemp - ExchCond( ExchNum ).SupInTemp ) ) ), Node( ExchCond( ExchNum ).SupOutletNode ).Temp );

	ExchCond( ExchNum ).ControlToTemperatureSetPoint = true;
	Node( ExchCond( ExchNum ).SupOutletNode ).TempSetPoint = 19.0;

	// HXUnitOn is true and ControlToTemperatureSetPoint is true so expect outlet = set point temperature
	HXUnitOn = true;
	ExchCond( ExchNum ).ExchConfigNum = Plate;
	InitHeatRecovery( ExchNum, CompanionCoilNum );
	CalcAirToAirGenericHeatExch( ExchNum, HXUnitOn, FirstHVACIteration, FanOpMode, EconomizerFlag, HighHumCtrlFlag );
	UpdateHeatRecovery( ExchNum );
	EXPECT_DOUBLE_EQ( Node( ExchCond( ExchNum ).SupOutletNode ).TempSetPoint, Node( ExchCond( ExchNum ).SupOutletNode ).Temp );

	ExchCond( ExchNum ).ExchConfigNum = Rotary;
	HXUnitOn = true;
	InitHeatRecovery( ExchNum, CompanionCoilNum );
	CalcAirToAirGenericHeatExch( ExchNum, HXUnitOn, FirstHVACIteration, FanOpMode, EconomizerFlag, HighHumCtrlFlag );
	UpdateHeatRecovery( ExchNum );
	EXPECT_DOUBLE_EQ( Node( ExchCond( ExchNum ).SupOutletNode ).TempSetPoint, Node( ExchCond( ExchNum ).SupOutletNode ).Temp );

	// test cycling fan case
	FanOpMode = DataHVACGlobals::CycFanCycCoil;
	Node( ExchCond( ExchNum ).SupInletNode ).MassFlowRate = ExchCond( ExchNum ).SupInMassFlow / 4.0;
	Node( ExchCond( ExchNum ).SecInletNode ).MassFlowRate = ExchCond( ExchNum ).SecInMassFlow / 4.0;
	ExchCond( ExchNum ).ControlToTemperatureSetPoint = false;
	InitHeatRecovery( ExchNum, CompanionCoilNum );
	CalcAirToAirGenericHeatExch( ExchNum, HXUnitOn, FirstHVACIteration, FanOpMode, EconomizerFlag, HighHumCtrlFlag, PartLoadRatio );
	UpdateHeatRecovery( ExchNum );
	EXPECT_DOUBLE_EQ( ( ExchCond( ExchNum ).SupInTemp + ( ExchCond( ExchNum ).CoolEffectSensible75 * ( ExchCond( ExchNum ).SecInTemp - ExchCond( ExchNum ).SupInTemp ) ) ), Node( ExchCond( ExchNum ).SupOutletNode ).Temp );

}
