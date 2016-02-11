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

// EnergyPlus::ThermalComfort Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// ObjexxFCL Headers
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/ThermalComfort.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataRoomAirModel.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::ThermalComfort;
using namespace EnergyPlus::DataEnvironment;
using namespace EnergyPlus::DataGlobals;
using namespace EnergyPlus::DataHeatBalance;
using namespace EnergyPlus::DataHVACGlobals;
using namespace EnergyPlus::DataRoomAirModel;
using namespace EnergyPlus::DataHeatBalFanSys;
using namespace ObjexxFCL;

using DataZoneEnergyDemands::ZoneSysEnergyDemand;


TEST_F( EnergyPlusFixture, ThermalComfort_CalcIfSetPointMetTest1 )
{
	NumOfZones = 1;
	ZoneSysEnergyDemand.allocate( NumOfZones );
	ThermalComfortSetPoint.allocate( NumOfZones );
	TempControlType.allocate( 1 );
	AirModel.allocate( NumOfZones );
	AirModel( 1 ).AirModelType = RoomAirModel_Mixing;
	ZTAV.allocate( NumOfZones );
	ZoneThermostatSetPointLo.allocate( NumOfZones );
	ZoneThermostatSetPointHi.allocate( NumOfZones );
	TimeStepZone = 0.25;
	ThermalComfortInASH55.allocate( NumOfZones );
	ThermalComfortInASH55( 1 ).ZoneIsOccupied = true;

	// SingleHeatingSetPoint thermostat

	TempControlType( 1 ) = SingleHeatingSetPoint;

	//heating
	ZTAV( 1 ) = 21.1; // 70F
	ZoneThermostatSetPointLo( 1 ) = 22.2; // 72F
	ZoneSysEnergyDemand( 1 ).TotalOutputRequired = 500.0; // must be greater than zero
	CalcIfSetPointMet();
	EXPECT_EQ( TimeStepZone, ThermalComfortSetPoint( 1 ).notMetHeating );
	EXPECT_EQ( TimeStepZone, ThermalComfortSetPoint( 1 ).notMetHeatingOccupied );
	EXPECT_EQ( 0., ThermalComfortSetPoint( 1 ).notMetCooling );
	EXPECT_EQ( 0., ThermalComfortSetPoint( 1 ).notMetCoolingOccupied );

	//cooling
	ZTAV( 1 ) = 25.0; // 77F
	ZoneThermostatSetPointHi( 1 ) = 23.9; // 75F
	ZoneSysEnergyDemand( 1 ).TotalOutputRequired = -500.0; // must be less than zero
	CalcIfSetPointMet();
	EXPECT_EQ( 0, ThermalComfortSetPoint( 1 ).notMetHeating );
	EXPECT_EQ( 0, ThermalComfortSetPoint( 1 ).notMetHeatingOccupied );
	EXPECT_EQ( 0., ThermalComfortSetPoint( 1 ).notMetCooling );
	EXPECT_EQ( 0., ThermalComfortSetPoint( 1 ).notMetCoolingOccupied );

	// SingleCoolingSetPoint thermostat

	TempControlType( 1 ) = SingleCoolingSetPoint;

	//heating
	ZTAV( 1 ) = 21.1; // 70F
	ZoneThermostatSetPointLo( 1 ) = 22.2; // 72F
	ZoneSysEnergyDemand( 1 ).TotalOutputRequired = 500.0; // must be greater than zero
	CalcIfSetPointMet();
	EXPECT_EQ( 0, ThermalComfortSetPoint( 1 ).notMetHeating );
	EXPECT_EQ( 0, ThermalComfortSetPoint( 1 ).notMetHeatingOccupied );
	EXPECT_EQ( 0., ThermalComfortSetPoint( 1 ).notMetCooling );
	EXPECT_EQ( 0., ThermalComfortSetPoint( 1 ).notMetCoolingOccupied );

	//cooling
	ZTAV( 1 ) = 25.0; // 77F
	ZoneThermostatSetPointHi( 1 ) = 23.9; // 75F
	ZoneSysEnergyDemand( 1 ).TotalOutputRequired = -500.0; // must be less than zero
	CalcIfSetPointMet();
	EXPECT_EQ( 0, ThermalComfortSetPoint( 1 ).notMetHeating );
	EXPECT_EQ( 0, ThermalComfortSetPoint( 1 ).notMetHeatingOccupied );
	EXPECT_EQ( TimeStepZone, ThermalComfortSetPoint( 1 ).notMetCooling );
	EXPECT_EQ( TimeStepZone, ThermalComfortSetPoint( 1 ).notMetCoolingOccupied );

	// SingleHeatCoolSetPoint thermostat

	TempControlType( 1 ) = SingleHeatCoolSetPoint;

	//heating
	ZTAV( 1 ) = 21.1; // 70F
	ZoneThermostatSetPointLo( 1 ) = 22.2; // 72F
	ZoneSysEnergyDemand( 1 ).TotalOutputRequired = 500.0; // must be greater than zero
	CalcIfSetPointMet();
	EXPECT_EQ( TimeStepZone, ThermalComfortSetPoint( 1 ).notMetHeating );
	EXPECT_EQ( TimeStepZone, ThermalComfortSetPoint( 1 ).notMetHeatingOccupied );
	EXPECT_EQ( 0., ThermalComfortSetPoint( 1 ).notMetCooling );
	EXPECT_EQ( 0., ThermalComfortSetPoint( 1 ).notMetCoolingOccupied );

	//cooling
	ZTAV( 1 ) = 25.0; // 77F
	ZoneThermostatSetPointHi( 1 ) = 23.9; // 75F
	ZoneSysEnergyDemand( 1 ).TotalOutputRequired = -500.0; // must be less than zero
	CalcIfSetPointMet();
	EXPECT_EQ( 0, ThermalComfortSetPoint( 1 ).notMetHeating );
	EXPECT_EQ( 0, ThermalComfortSetPoint( 1 ).notMetHeatingOccupied );
	EXPECT_EQ( TimeStepZone, ThermalComfortSetPoint( 1 ).notMetCooling );
	EXPECT_EQ( TimeStepZone, ThermalComfortSetPoint( 1 ).notMetCoolingOccupied );

	// DualSetPointWithDeadBand thermostat

	TempControlType( 1 ) = DualSetPointWithDeadBand;

	//heating
	ZTAV( 1 ) = 21.1; // 70F
	ZoneThermostatSetPointLo( 1 ) = 22.2; // 72F
	ZoneSysEnergyDemand( 1 ).TotalOutputRequired = 500.0; // must be greater than zero
	CalcIfSetPointMet();
	EXPECT_EQ( TimeStepZone, ThermalComfortSetPoint( 1 ).notMetHeating );
	EXPECT_EQ( TimeStepZone, ThermalComfortSetPoint( 1 ).notMetHeatingOccupied );
	EXPECT_EQ( 0., ThermalComfortSetPoint( 1 ).notMetCooling );
	EXPECT_EQ( 0., ThermalComfortSetPoint( 1 ).notMetCoolingOccupied );

	//cooling
	ZTAV( 1 ) = 25.0; // 77F
	ZoneThermostatSetPointHi( 1 ) = 23.9; // 75F
	ZoneSysEnergyDemand( 1 ).TotalOutputRequired = -500.0; // must be less than zero
	CalcIfSetPointMet();
	EXPECT_EQ( 0, ThermalComfortSetPoint( 1 ).notMetHeating );
	EXPECT_EQ( 0, ThermalComfortSetPoint( 1 ).notMetHeatingOccupied );
	EXPECT_EQ( TimeStepZone, ThermalComfortSetPoint( 1 ).notMetCooling );
	EXPECT_EQ( TimeStepZone, ThermalComfortSetPoint( 1 ).notMetCoolingOccupied );
}

TEST_F( EnergyPlusFixture, ThermalComfort_CalcThermalComfortFanger )
{
	TotPeople = 1;
	People.allocate( TotPeople );
	People( 1 ).Fanger = true;
	People( 1 ).ZonePtr = 1;
	People( 1 ).MRTCalcType = ZoneAveraged;
	People( 1 ).ActivityLevelPtr = 0;
	People( 1 ).ClothingType = 1;  //ClothingInsulationSchedule

	NumOfZones = 1;
	Zone.allocate( NumOfZones );

	IsZoneCV.allocate(NumOfZones);
	IsZoneCV( 1 ) = false;
	IsZoneDV.allocate( NumOfZones );
	IsZoneDV( 1 ) = false;
	IsZoneUI.allocate( NumOfZones );
	IsZoneUI( 1 ) = false;
	ZTAVComf.allocate( NumOfZones );
	MRT.allocate( NumOfZones );

	QHTRadSysToPerson.allocate( NumOfZones);
	QHTRadSysToPerson( 1 ) = 0.0;
	QHWBaseboardToPerson.allocate( NumOfZones );
	QHWBaseboardToPerson( 1 ) = 0.0;
	QSteamBaseboardToPerson.allocate( NumOfZones );
	QSteamBaseboardToPerson( 1 ) = 0.0;
	QElecBaseboardToPerson.allocate( NumOfZones );
	QElecBaseboardToPerson( 1 ) = 0.0;
	ZoneAirHumRatAvgComf.allocate( NumOfZones );
	OutBaroPress = 101325.; // sea level

	ThermalComfortData.allocate( TotPeople );

	Real64 sActLevel = 70.; // 50 to 150
	Real64 sWorkEff = 0.0;
	Real64 sCloUnit = 1.0;  //0.5 to 1.0

	ZTAVComf( 1 ) = 21.0;
	MRT( 1 ) = 19.0;
	ZoneAirHumRatAvgComf( 1 ) = 0.00529; // 0.002 to 0.006

	CalcThermalComfortFanger(_,_,_, sActLevel, sWorkEff, sCloUnit);

	EXPECT_NEAR( ThermalComfortData( 1 ).FangerPMV, -0.955, 0.005 );
	EXPECT_NEAR( ThermalComfortData( 1 ).FangerPPD, 24.3, 0.1 );

	ZTAVComf( 1 ) = 22.0;
	MRT( 1 ) = 24.0;
	ZoneAirHumRatAvgComf( 1 ) = 0.00529; // 0.002 to 0.006

	CalcThermalComfortFanger( _, _, _, sActLevel, sWorkEff, sCloUnit );

	EXPECT_NEAR( ThermalComfortData( 1 ).FangerPMV, -0.450, 0.005 );
	EXPECT_NEAR( ThermalComfortData( 1 ).FangerPPD, 9.2, 0.1 );

	ZTAVComf( 1 ) = 24.0;
	MRT( 1 ) = 25.0;
	ZoneAirHumRatAvgComf( 1 ) = 0.00529; // 0.002 to 0.006

	CalcThermalComfortFanger( _, _, _, sActLevel, sWorkEff, sCloUnit );

	EXPECT_NEAR( ThermalComfortData( 1 ).FangerPMV, -0.003, 0.005 );
	EXPECT_NEAR( ThermalComfortData( 1 ).FangerPPD, 5.0, 0.1 );

	ZTAVComf( 1 ) = 25.0;
	MRT( 1 ) = 26.0;
	ZoneAirHumRatAvgComf( 1 ) = 0.00629; // 0.002 to 0.006

	CalcThermalComfortFanger( _, _, _, sActLevel, sWorkEff, sCloUnit );

	EXPECT_NEAR( ThermalComfortData( 1 ).FangerPMV, 0.288, 0.005 );
	EXPECT_NEAR( ThermalComfortData( 1 ).FangerPPD, 6.7, 0.1 );
}

