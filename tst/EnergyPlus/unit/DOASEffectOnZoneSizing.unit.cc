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

// EnergyPlus::Standalone unit tests of DOAS effect on zone sizing feature

// Google Test Headers
#include <gtest/gtest.h>

#include "Fixtures/EnergyPlusFixture.hh"

// EnergyPlus Headers
#include <ZoneEquipmentManager.hh>
#include <InputProcessor.hh>
#include <DataStringGlobals.hh>
#include <DataLoopNode.hh>
#include <DataSizing.hh>
#include <DataZoneEquipment.hh>
#include <DataEnvironment.hh>
#include <DataZoneEnergyDemands.hh>
#include <Psychrometrics.hh>
#include <DataHeatBalFanSys.hh>
#include <DataHeatBalance.hh>
#include <DataAirflowNetwork.hh>

using namespace EnergyPlus;
using namespace ZoneEquipmentManager;
using namespace InputProcessor;
using namespace DataLoopNode;
using namespace DataSizing;
using namespace DataZoneEquipment;
using namespace DataEnvironment;
using namespace DataZoneEnergyDemands;
using namespace Psychrometrics;
using namespace DataHeatBalFanSys;
using namespace DataHeatBalance;
using DataAirflowNetwork::AirflowNetworkNumOfExhFan;

using namespace ObjexxFCL;

TEST_F( EnergyPlusFixture, DOASEffectOnZoneSizing_CalcDOASSupCondsForSizing)
{
	// locals
	Real64 OutDB; // outside air temperature [C]
	Real64 OutHR; // outside humidity ratio [kg Water / kg Dry Air]
	int DOASControl; // dedicated outside air control strategy
	Real64 DOASLowTemp; // DOAS low setpoint [C]
	Real64 DOASHighTemp; // DOAS high setpoint [C]
	Real64 DOASSupTemp;  // DOAS supply temperature [C]
	Real64 DOASSupHR; // DOAS supply humidity ratio [kg H2O / kg dry air]
	// neutral supply air
	DOASControl = 1;
	DOASLowTemp = 21.1;
	DOASHighTemp = 23.9;
	OutDB = 10.0;
	OutHR = 0.005;
	CalcDOASSupCondsForSizing( OutDB, OutHR, DOASControl, DOASLowTemp, DOASHighTemp, 0.016, 0.0143, DOASSupTemp, DOASSupHR );
	EXPECT_DOUBLE_EQ( 21.1, DOASSupTemp );
	EXPECT_DOUBLE_EQ( 0.005, DOASSupHR );
	OutDB = 35.6;
	OutHR = 0.0185;
	CalcDOASSupCondsForSizing( OutDB, OutHR, DOASControl, DOASLowTemp, DOASHighTemp, 0.016, 0.0143, DOASSupTemp, DOASSupHR );
	EXPECT_DOUBLE_EQ( 23.9, DOASSupTemp );
	EXPECT_DOUBLE_EQ( 0.016, DOASSupHR );
	OutDB = 22.3;
	OutHR = 0.0085;
	CalcDOASSupCondsForSizing( OutDB, OutHR, DOASControl, DOASLowTemp, DOASHighTemp, 0.016, 0.0143, DOASSupTemp, DOASSupHR );
	EXPECT_DOUBLE_EQ( 22.3, DOASSupTemp );
	EXPECT_DOUBLE_EQ( 0.0085, DOASSupHR );
	// neutral dehumidified supply air
	DOASControl = 2;
	DOASLowTemp = 14.4;
	DOASHighTemp = 22.2;
	OutDB = 11;
	OutHR = 0.004;
	CalcDOASSupCondsForSizing( OutDB, OutHR, DOASControl, DOASLowTemp, DOASHighTemp, 0.0153, 0.0092, DOASSupTemp, DOASSupHR );
	EXPECT_DOUBLE_EQ( 22.2, DOASSupTemp );
	EXPECT_DOUBLE_EQ( 0.004, DOASSupHR );
	OutDB = 35.6;
	OutHR = 0.0185;
	CalcDOASSupCondsForSizing( OutDB, OutHR, DOASControl, DOASLowTemp, DOASHighTemp, 0.0153, 0.0092, DOASSupTemp, DOASSupHR );
	EXPECT_DOUBLE_EQ( 22.2, DOASSupTemp );
	EXPECT_DOUBLE_EQ( 0.0092, DOASSupHR );
	// cold supply air
	DOASControl = 3;
	DOASLowTemp = 12.2;
	DOASHighTemp = 14.4;
	OutDB = 11;
	OutHR = 0.005;
	CalcDOASSupCondsForSizing( OutDB, OutHR, DOASControl, DOASLowTemp, DOASHighTemp, 0.0092, 0.008, DOASSupTemp, DOASSupHR );
	EXPECT_DOUBLE_EQ( 14.4, DOASSupTemp );
	EXPECT_DOUBLE_EQ( 0.005, DOASSupHR );
	OutDB = 35.6;
	OutHR = 0.0185;
	CalcDOASSupCondsForSizing( OutDB, OutHR, DOASControl, DOASLowTemp, DOASHighTemp, 0.0092, 0.008, DOASSupTemp, DOASSupHR );
	EXPECT_DOUBLE_EQ( 12.2, DOASSupTemp );
	EXPECT_DOUBLE_EQ( 0.008, DOASSupHR );
}

TEST_F( EnergyPlusFixture, DOASEffectOnZoneSizing_SizeZoneEquipment )
{

	Node.allocate( 10 );
	ZoneEqSizing.allocate( 2 );
	Zone.allocate( 2 );
	CalcZoneSizing.allocate( 1, 2 );
	CalcFinalZoneSizing.allocate( 2 );
	NonAirSystemResponse.allocate( 2 );
	SysDepZoneLoads.allocate( 2 );
	ZoneEquipConfig.allocate( 2 );
	TempControlType.allocate( 2 );
	TempZoneThermostatSetPoint.allocate( 2 );
	ZoneThermostatSetPointLo.allocate( 2 );
	ZoneThermostatSetPointHi.allocate( 2 );
	ZoneSysEnergyDemand.allocate( 2 );
	ZoneSysMoistureDemand.allocate( 2 );
	DeadBandOrSetback.allocate( 2 );
	CurDeadBandOrSetback.allocate( 2 );
	ZoneEquipConfig( 1 ).InletNode.allocate( 2 );
	ZoneEquipConfig( 2 ).InletNode.allocate( 2 );
	ZoneEquipConfig( 1 ).ExhaustNode.allocate( 1 );
	ZoneEquipConfig( 2 ).ExhaustNode.allocate( 1 );
	ZoneMassBalanceFlag.allocate( 2 );
	NumOfZones = 2;
	MassConservation.allocate( NumOfZones );

	AirflowNetworkNumOfExhFan = 0;
	TempControlType( 1 ) = 4;
	TempControlType( 2 ) = 4;
	TempZoneThermostatSetPoint( 1 ) = 0.0;
	TempZoneThermostatSetPoint( 2 ) = 0.0;
	ZoneThermostatSetPointLo( 1 ) = 22.;
	ZoneThermostatSetPointLo( 2 ) = 22.;
	ZoneThermostatSetPointHi( 1 ) = 24.;
	ZoneThermostatSetPointHi( 2 ) = 24.;
	CurOverallSimDay = 1;
	ZoneEquipConfig( 1 ).IsControlled = true;
	ZoneEquipConfig( 2 ).IsControlled = true;
	CalcZoneSizing( 1, 1 ).ActualZoneNum = 1;
	CalcZoneSizing( 1, 2 ).ActualZoneNum = 2;
	CalcZoneSizing( 1, 1 ).AccountForDOAS = true;
	CalcZoneSizing( 1, 2 ).AccountForDOAS = true;
	CurOverallSimDay = 1;
	ZoneSysEnergyDemand( 2 ).TotalOutputRequired = -2600;
	ZoneSysEnergyDemand( 2 ).OutputRequiredToHeatingSP = -21100;
	ZoneSysEnergyDemand( 2 ).OutputRequiredToCoolingSP = -2600;
	ZoneSysEnergyDemand( 1 ).TotalOutputRequired = 3600;
	ZoneSysEnergyDemand( 1 ).OutputRequiredToHeatingSP = 3600;
	ZoneSysEnergyDemand( 1 ).OutputRequiredToCoolingSP = 22000.;
	ZoneSysMoistureDemand( 1 ).TotalOutputRequired = 0.0;
	ZoneSysMoistureDemand( 1 ).OutputRequiredToHumidifyingSP = 0.0;
	ZoneSysMoistureDemand( 1 ).OutputRequiredToDehumidifyingSP = 0.0;
	ZoneSysMoistureDemand( 2 ).TotalOutputRequired = 0.0;
	ZoneSysMoistureDemand( 2 ).OutputRequiredToHumidifyingSP = 0.0;
	ZoneSysMoistureDemand( 2 ).OutputRequiredToDehumidifyingSP = 0.0;
	DeadBandOrSetback( 1 ) = false;
	DeadBandOrSetback( 2 ) = false;
	CurDeadBandOrSetback( 1 ) = false;
	CurDeadBandOrSetback( 2 ) = false;
	ZoneEquipConfig( 1 ).ZoneNode = 4;
	ZoneEquipConfig( 2 ).ZoneNode = 9;
	ZoneEquipConfig( 1 ).NumInletNodes = 2;
	ZoneEquipConfig( 2 ).NumInletNodes = 2;
	ZoneEquipConfig( 1 ).NumExhaustNodes = 1;
	ZoneEquipConfig( 2 ).NumExhaustNodes = 1;
	ZoneEquipConfig( 1 ).InletNode( 1 ) = 1;
	ZoneEquipConfig( 1 ).InletNode( 2 ) = 2;
	ZoneEquipConfig( 2 ).InletNode( 1 ) = 6;
	ZoneEquipConfig( 2 ).InletNode( 2 ) = 7;
	ZoneEquipConfig( 1 ).ExhaustNode( 1 ) = 3;
	ZoneEquipConfig( 2 ).ExhaustNode( 1 ) = 8;
	ZoneEquipConfig( 1 ).ReturnAirNode = 0;
	ZoneEquipConfig( 2 ).ReturnAirNode = 0;
	ZoneEquipConfig( 1 ).ActualZoneNum = 1;
	ZoneEquipConfig( 2 ).ActualZoneNum = 2;
	CalcZoneSizing( CurOverallSimDay, 1 ).DOASHighSetpoint = 14.4;
	CalcZoneSizing( CurOverallSimDay, 1 ).DOASLowSetpoint = 12.2;
	CalcZoneSizing( CurOverallSimDay, 2 ).DOASHighSetpoint = 14.4;
	CalcZoneSizing( CurOverallSimDay, 2 ).DOASLowSetpoint = 12.2;
	StdBaroPress = 101325.;
	CalcFinalZoneSizing( 1 ).MinOA = 0.1;
	CalcFinalZoneSizing( 2 ).MinOA = 0.11;
	CalcZoneSizing( CurOverallSimDay, 1 ).DOASControlStrategy = 3;
	CalcZoneSizing( CurOverallSimDay, 2 ).DOASControlStrategy = 3;
	OutDryBulbTemp = 28.;
	OutHumRat = 0.017;
	Node( 4 ).Temp = 22;
	Node( 4 ).HumRat = 0.008;
	Node( 9 ).Temp = 22.5;
	Node( 9 ).HumRat = 0.0085;
	CalcZoneSizing( CurOverallSimDay, 1 ).ZnCoolDgnSAMethod = 1;
	CalcZoneSizing( CurOverallSimDay, 2 ).ZnCoolDgnSAMethod = 2;
	CalcZoneSizing( CurOverallSimDay, 1 ).ZnHeatDgnSAMethod = 1;
	CalcZoneSizing( CurOverallSimDay, 2 ).ZnHeatDgnSAMethod = 2;
	CalcZoneSizing( CurOverallSimDay, 1 ).CoolDesTemp = 12.5;
	CalcZoneSizing( CurOverallSimDay, 2 ).CoolDesTemp = 12.5;
	CalcZoneSizing( CurOverallSimDay, 1 ).CoolDesTempDiff = 11.11;
	CalcZoneSizing( CurOverallSimDay, 2 ).CoolDesTempDiff = 11.11;
	CalcZoneSizing( CurOverallSimDay, 1 ).CoolDesHumRat = 0.008;
	CalcZoneSizing( CurOverallSimDay, 2 ).CoolDesHumRat = 0.008;
	CalcZoneSizing( CurOverallSimDay, 1 ).HeatDesHumRat = 0.008;
	CalcZoneSizing( CurOverallSimDay, 2 ).HeatDesHumRat = 0.008;
	CalcZoneSizing( CurOverallSimDay, 1 ).HeatDesTemp = 50.0;
	CalcZoneSizing( CurOverallSimDay, 2 ).HeatDesTemp = 50.0;
	CalcZoneSizing( CurOverallSimDay, 1 ).HeatDesTempDiff = 30.0;
	CalcZoneSizing( CurOverallSimDay, 2 ).HeatDesTempDiff = 30.0;
	CalcZoneSizing( CurOverallSimDay, 1 ).SupplyAirAdjustFactor = 1.0;
	CalcZoneSizing( CurOverallSimDay, 2 ).SupplyAirAdjustFactor = 1.0;
	ZoneAirMassFlow.EnforceZoneMassBalance = false;
	ZoneMassBalanceFlag( 1 ) = false;
	ZoneMassBalanceFlag( 2 ) = false;
	Node( 1 ).MassFlowRateMin = 0.0;
	Node( 1 ).MassFlowRateMinAvail = 0.0;
	Node( 1 ).MassFlowRateMaxAvail = 0.0;
	Node( 1 ).MassFlowRateMax = 0.0;
	Node( 2 ).MassFlowRateMin = 0.0;
	Node( 2 ).MassFlowRateMinAvail = 0.0;
	Node( 2 ).MassFlowRateMaxAvail = 0.0;
	Node( 2 ).MassFlowRateMax = 0.0;
	Node( 3 ).MassFlowRateMin = 0.0;
	Node( 3 ).MassFlowRateMinAvail = 0.0;
	Node( 3 ).MassFlowRateMaxAvail = 0.0;
	Node( 3 ).MassFlowRateMax = 0.0;
	Node( 6 ).MassFlowRateMin = 0.0;
	Node( 6 ).MassFlowRateMinAvail = 0.0;
	Node( 6 ).MassFlowRateMaxAvail = 0.0;
	Node( 6 ).MassFlowRateMax = 0.0;
	Node( 7 ).MassFlowRateMin = 0.0;
	Node( 7 ).MassFlowRateMinAvail = 0.0;
	Node( 7 ).MassFlowRateMaxAvail = 0.0;
	Node( 7 ).MassFlowRateMax = 0.0;
	Node( 8 ).MassFlowRateMin = 0.0;
	Node( 8 ).MassFlowRateMinAvail = 0.0;
	Node( 8 ).MassFlowRateMaxAvail = 0.0;
	Node( 8 ).MassFlowRateMax = 0.0;
	ZoneEquipConfig( 1 ).AirLoopNum = 0;
	ZoneEquipConfig( 2 ).AirLoopNum = 0;
	ZoneEquipConfig( 1 ).ZoneExh = 0.0;
	ZoneEquipConfig( 1 ).ZoneExhBalanced = 0.0;
	ZoneEquipConfig( 1 ).PlenumMassFlow = 0.0;
	ZoneEquipConfig( 2 ).ZoneExh = 0.0;
	ZoneEquipConfig( 2 ).ZoneExhBalanced = 0.0;
	ZoneEquipConfig( 2 ).PlenumMassFlow = 0.0;
	MassConservation( 1 ).MixingMassFlowRate = 0.0;
	MassConservation( 2 ).MixingMassFlowRate = 0.0;
	Zone( 1 ).Multiplier = 1.0;
	Zone( 2 ).Multiplier = 1.0;
	Zone( 1 ).ListMultiplier = 1;
	Zone( 2 ).ListMultiplier = 1;

	SizeZoneEquipmentOneTimeFlag = false;
	SizeZoneEquipment();

	EXPECT_DOUBLE_EQ( 12.2, CalcZoneSizing( 1, 1 ).DOASSupTemp );
	EXPECT_NEAR( .00795195, CalcZoneSizing( 1, 1 ).DOASSupHumRat, .00000001 );
	EXPECT_DOUBLE_EQ( 0.1, CalcZoneSizing( 1, 1 ).DOASSupMassFlow );
	EXPECT_NEAR( -999.229, CalcZoneSizing( 1, 1 ).DOASHeatAdd, .001 );
	EXPECT_DOUBLE_EQ( 0.0, CalcZoneSizing( 1, 1 ).DOASHeatLoad );
	EXPECT_NEAR( -999.229, CalcZoneSizing( 1, 1 ).DOASCoolLoad, .001 );
	EXPECT_NEAR( -1011.442, CalcZoneSizing( 1, 1 ).DOASTotCoolLoad, .001 );
	EXPECT_NEAR( 4599.229, CalcZoneSizing( 1, 1 ).HeatLoad, .001 );
	EXPECT_NEAR( .161083, CalcZoneSizing( 1, 1 ).HeatMassFlow, .00001 );
	EXPECT_DOUBLE_EQ( 0.0, CalcZoneSizing( 1, 1 ).CoolLoad );
	EXPECT_DOUBLE_EQ( 0.0, CalcZoneSizing( 1, 1 ).CoolMassFlow );

	EXPECT_DOUBLE_EQ( 12.2, CalcZoneSizing( 1, 2 ).DOASSupTemp );
	EXPECT_NEAR( .00795195, CalcZoneSizing( 1, 2 ).DOASSupHumRat, .00000001 );
	EXPECT_DOUBLE_EQ( 0.11, CalcZoneSizing( 1, 2 ).DOASSupMassFlow );
	EXPECT_NEAR( -1155.232, CalcZoneSizing( 1, 2 ).DOASHeatAdd, .001 );
	EXPECT_DOUBLE_EQ( 0.0, CalcZoneSizing( 1, 2 ).DOASHeatLoad );
	EXPECT_NEAR( -1155.232, CalcZoneSizing( 1, 2 ).DOASCoolLoad, .001 );
	EXPECT_NEAR( -1308.522, CalcZoneSizing( 1, 2 ).DOASTotCoolLoad, .001 );
	EXPECT_DOUBLE_EQ( 0.0, CalcZoneSizing( 1, 2 ).HeatLoad );
	EXPECT_DOUBLE_EQ( 0.0, CalcZoneSizing( 1, 2 ).HeatMassFlow );
	EXPECT_NEAR( 1444.767, CalcZoneSizing( 1, 2 ).CoolLoad, .001 );
	EXPECT_NEAR( .127528, CalcZoneSizing( 1, 2 ).CoolMassFlow, .000001 );

	Node.deallocate();
	ZoneEqSizing.deallocate();
	Zone.deallocate();
	CalcZoneSizing.deallocate();
	NonAirSystemResponse.deallocate();
	SysDepZoneLoads.deallocate();
	ZoneEquipConfig( 1 ).InletNode.deallocate();
	ZoneEquipConfig( 2 ).InletNode.deallocate();
	ZoneEquipConfig( 1 ).ExhaustNode.deallocate();
	ZoneEquipConfig( 2 ).ExhaustNode.deallocate();
	ZoneEquipConfig.deallocate();
	TempControlType.deallocate();
	TempZoneThermostatSetPoint.deallocate();
	ZoneThermostatSetPointLo.deallocate();
	ZoneThermostatSetPointHi.deallocate();
	ZoneSysEnergyDemand.deallocate();
	ZoneSysMoistureDemand.deallocate();
	DeadBandOrSetback.deallocate();
	CurDeadBandOrSetback.deallocate();
	ZoneMassBalanceFlag.deallocate();
	MassConservation.deallocate();

}
