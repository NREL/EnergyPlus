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

// EnergyPlus::ZoneTempPredictorCorrector Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

#include "Fixtures/EnergyPlusFixture.hh"

// EnergyPlus Headers
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataZoneControls.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/DataZoneEnergyDemands.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <EnergyPlus/ZonePlenum.hh>
#include <EnergyPlus/ZoneTempPredictorCorrector.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataAirflowNetwork.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/DataRoomAirModel.hh>

using namespace EnergyPlus;
using namespace ObjexxFCL;
using namespace EnergyPlus::DataHeatBalance;
using namespace EnergyPlus::DataHeatBalFanSys;
using namespace DataGlobals;
using namespace EnergyPlus::DataZoneControls;
using namespace EnergyPlus::DataZoneEquipment;
using namespace EnergyPlus::DataZoneEnergyDemands;
using namespace EnergyPlus::DataSizing;
using namespace EnergyPlus::HeatBalanceManager;
using namespace EnergyPlus::ZonePlenum;
using namespace EnergyPlus::ZoneTempPredictorCorrector;
using namespace EnergyPlus::DataLoopNode;
using namespace EnergyPlus::DataHVACGlobals;
using namespace EnergyPlus::DataSurfaces;
using namespace EnergyPlus::DataEnvironment;
using namespace EnergyPlus::DataAirflowNetwork;
using namespace EnergyPlus::Psychrometrics;
using namespace EnergyPlus::ScheduleManager;
using namespace EnergyPlus::DataRoomAirModel;

TEST_F( EnergyPlusFixture, ZoneTempPredictorCorrector_CorrectZoneHumRatTest )
{

	TimeStepSys = 15.0 / 60.0; // System timestep in hours

	ZoneEquipConfig.allocate( 1 );
	ZoneEquipConfig( 1 ).ZoneName = "Zone 1";
	ZoneEquipConfig( 1 ).ActualZoneNum = 1;
	std::vector< int > controlledZoneEquipConfigNums;
	controlledZoneEquipConfigNums.push_back( 1 );

	ZoneEquipConfig( 1 ).NumInletNodes = 2;
	ZoneEquipConfig( 1 ).InletNode.allocate( 2 );
	ZoneEquipConfig( 1 ).InletNode( 1 ) = 1;
	ZoneEquipConfig( 1 ).InletNode( 2 ) = 2;
	ZoneEquipConfig( 1 ).NumExhaustNodes = 1;
	ZoneEquipConfig( 1 ).ExhaustNode.allocate( 1 );
	ZoneEquipConfig( 1 ).ExhaustNode( 1 ) = 3;
	ZoneEquipConfig( 1 ).ReturnAirNode = 4;

	Node.allocate( 5 );

	Zone.allocate( 1 );
	Zone( 1 ).Name = ZoneEquipConfig( 1 ).ZoneName;
	ZoneEqSizing.allocate( 1 );
	CurZoneEqNum = 1;
	Zone( 1 ).Multiplier = 1.0;
	Zone( 1 ).Volume = 1000.0;
	Zone( 1 ).SystemZoneNodeNumber = 5;
	ZoneVolCapMultpMoist = 1.0;
	ZoneLatentGain.allocate( 1 );
	ZoneLatentGain( 1 ) = 0.0;
	SumLatentHTRadSys.allocate( 1 );
	SumLatentHTRadSys( 1 ) = 0.0;
	SumLatentPool.allocate( 1 );
	SumLatentPool( 1 ) = 0.0;
	OutBaroPress = 101325.0;
	ZT.allocate( 1 ); // Zone temperature C
	ZT( 1 ) = 24.0;
	ZoneAirHumRat.allocate( 1 );

	Zone( 1 ).SurfaceFirst = 1;
	Zone( 1 ).SurfaceLast = 2;
	Surface.allocate( 2);

	NumZoneReturnPlenums = 0;
	NumZoneSupplyPlenums = 0;

	OAMFL.allocate( 1 );
	VAMFL.allocate( 1 );
	EAMFL.allocate( 1 );
	CTMFL.allocate( 1 );

	SumHmARaW.allocate( 1 );
	SumHmARa.allocate( 1 );
	MixingMassFlowXHumRat.allocate( 1 );
	MixingMassFlowZone.allocate( 1 );
	SimulateAirflowNetwork = 0;
	MDotOA.allocate( 1 );

	ZoneAirSolutionAlgo = UseEulerMethod;
	ZoneAirHumRatTemp.allocate( 1 );
	ZoneW1.allocate( 1 );

	AirModel.allocate( 1 );


// Case 1 - All flows at the same humrat
	ZoneW1( 1 ) = 0.008;
	Node( 1 ).MassFlowRate = 0.01; // Zone inlet node 1
	Node( 1 ).HumRat = 0.008;
	Node( 2 ).MassFlowRate = 0.02; // Zone inlet node 2
	Node( 2 ).HumRat = 0.008;
	ZoneEquipConfig( 1 ).ZoneExhBalanced = 0.0;
	Node( 3 ).MassFlowRate = 0.00; // Zone exhaust node 1
	Node( 3 ).HumRat = ZoneW1( 1 );
	Node( 4 ).MassFlowRate = 0.03; // Zone return node
	Node( 4 ).HumRat = 0.000;
	Node( 5 ).HumRat = 0.000;
	ZoneAirHumRat( 1 ) = 0.008;
	OAMFL( 1 ) = 0.0;
	VAMFL( 1 ) = 0.0;
	EAMFL( 1 ) = 0.0;
	CTMFL( 1 ) = 0.0;
	OutHumRat = 0.008;
	MixingMassFlowXHumRat( 1 ) = 0.0;
	MixingMassFlowZone( 1 ) = 0.0;
	MDotOA( 1 ) = 0.0;

	CorrectZoneHumRat( 1, controlledZoneEquipConfigNums );
	EXPECT_EQ( 0.008, Node( 5 ).HumRat );

	// Case 2 - Unbalanced exhaust flow
	ZoneW1( 1 ) = 0.008;
	Node( 1 ).MassFlowRate = 0.01; // Zone inlet node 1
	Node( 1 ).HumRat = 0.008;
	Node( 2 ).MassFlowRate = 0.02; // Zone inlet node 2
	Node( 2 ).HumRat = 0.008;
	ZoneEquipConfig( 1 ).ZoneExhBalanced = 0.0;
	Node( 3 ).MassFlowRate = 0.02; // Zone exhaust node 1
	Node( 3 ).HumRat = ZoneW1( 1 );
	Node( 4 ).MassFlowRate = 0.01; // Zone return node
	Node( 4 ).HumRat = ZoneW1( 1 );
	Node( 5 ).HumRat = 0.000;
	ZoneAirHumRat( 1 ) = 0.008;
	OAMFL( 1 ) = 0.0;
	VAMFL( 1 ) = 0.0;
	EAMFL( 1 ) = 0.0;
	CTMFL( 1 ) = 0.0;
	OutHumRat = 0.004;
	MixingMassFlowXHumRat( 1 ) = 0.0;
	MixingMassFlowZone( 1 ) = 0.0;
	MDotOA( 1 ) = 0.0;

	CorrectZoneHumRat( 1, controlledZoneEquipConfigNums );
	EXPECT_EQ( 0.008, Node( 5 ).HumRat );

	// Case 3 - Balanced exhaust flow with proper source flow from mixing
	ZoneW1( 1 ) = 0.008;
	Node( 1 ).MassFlowRate = 0.01; // Zone inlet node 1
	Node( 1 ).HumRat = 0.008;
	Node( 2 ).MassFlowRate = 0.02; // Zone inlet node 2
	Node( 2 ).HumRat = 0.008;
	ZoneEquipConfig( 1 ).ZoneExhBalanced = 0.02;
	Node( 3 ).MassFlowRate = 0.02; // Zone exhaust node 1
	Node( 3 ).HumRat = ZoneW1( 1 );
	Node( 4 ).MassFlowRate = 0.03; // Zone return node
	Node( 4 ).HumRat = ZoneW1( 1 );
	Node( 5 ).HumRat = 0.000;
	ZoneAirHumRat( 1 ) = 0.008;
	OAMFL( 1 ) = 0.0;
	VAMFL( 1 ) = 0.0;
	EAMFL( 1 ) = 0.0;
	CTMFL( 1 ) = 0.0;
	OutHumRat = 0.004;
	MixingMassFlowXHumRat( 1 ) = 0.02 * 0.008;
	MixingMassFlowZone( 1 ) = 0.02;
	MDotOA( 1 ) = 0.0;

	CorrectZoneHumRat( 1, controlledZoneEquipConfigNums );
	EXPECT_EQ( 0.008, Node( 5 ).HumRat );

	// Case 4 - Balanced exhaust flow without source flow from mixing
	ZoneW1( 1 ) = 0.008;
	Node( 1 ).MassFlowRate = 0.01; // Zone inlet node 1
	Node( 1 ).HumRat = 0.008;
	Node( 2 ).MassFlowRate = 0.02; // Zone inlet node 2
	Node( 2 ).HumRat = 0.008;
	ZoneEquipConfig( 1 ).ZoneExhBalanced = 0.02;
	Node( 3 ).MassFlowRate = 0.02; // Zone exhaust node 1
	Node( 3 ).HumRat = ZoneW1( 1 );
	Node( 4 ).MassFlowRate = 0.01; // Zone return node
	Node( 4 ).HumRat = ZoneW1( 1 );
	Node( 5 ).HumRat = 0.000;
	ZoneAirHumRat( 1 ) = 0.008;
	OAMFL( 1 ) = 0.0;
	VAMFL( 1 ) = 0.0;
	EAMFL( 1 ) = 0.0;
	CTMFL( 1 ) = 0.0;
	OutHumRat = 0.004;
	MixingMassFlowXHumRat( 1 ) = 0.0;
	MixingMassFlowZone( 1 ) = 0.0;
	MDotOA( 1 ) = 0.0;

	CorrectZoneHumRat( 1, controlledZoneEquipConfigNums );
	EXPECT_FALSE( (0.008 == Node( 5 ).HumRat) );

	// Deallocate everything
	ZoneEquipConfig( 1 ).InletNode.deallocate();
	ZoneEquipConfig( 1 ).ExhaustNode.deallocate();
	ZoneEquipConfig.deallocate();
	Node.deallocate();
	Zone.deallocate();
	ZoneLatentGain.deallocate();
	ZoneEqSizing.deallocate();
	SumLatentHTRadSys.deallocate();
	SumLatentPool.deallocate();
	ZT.deallocate(); // Zone temperature C
	ZoneAirHumRat.deallocate();
	Surface.deallocate();
	OAMFL.deallocate();
	VAMFL.deallocate();
	EAMFL.deallocate();
	CTMFL.deallocate();
	SumHmARaW.deallocate();
	SumHmARa.deallocate();
	MixingMassFlowXHumRat.deallocate();
	MixingMassFlowZone.deallocate();
	MDotOA.deallocate();
	ZoneAirHumRatTemp.deallocate();
	ZoneW1.deallocate();
	AirModel.deallocate();

}

	TEST_F( EnergyPlusFixture, ZoneTempPredictorCorrector_ReportingTest )
	{
		// AUTHOR: R. Raustad, FSEC
		// DATE WRITTEN: Aug 2015

		std::string const idf_objects = delimited_string({
			"Version,8.3;",
			" ",
			"Zone,",
			"  Core_top,             !- Name",
			"  0.0000,                  !- Direction of Relative North {deg}",
			"  0.0000,                  !- X Origin {m}",
			"  0.0000,                  !- Y Origin {m}",
			"  0.0000,                  !- Z Origin {m}",
			"  1,                       !- Type",
			"  1,                       !- Multiplier",
			"  ,                        !- Ceiling Height {m}",
			"  ,                        !- Volume {m3}",
			"  autocalculate,           !- Floor Area {m2}",
			"  ,                        !- Zone Inside Convection Algorithm",
			"  ,                        !- Zone Outside Convection Algorithm",
			"  Yes;                     !- Part of Total Floor Area",
			" ",
			"ZoneControl:Thermostat,",
			"  Core_top Thermostat,     !- Name",
			"  Core_top,                !- Zone or ZoneList Name",
			"  Single Heating Control Type Sched,  !- Control Type Schedule Name",
			"  ThermostatSetpoint:SingleHeating,  !- Control 1 Object Type",
			"  Core_top HeatSPSched;    !- Control 1 Name",
			" ",
			"Schedule:Compact,",
			"  Single Heating Control Type Sched,  !- Name",
			"  Control Type,            !- Schedule Type Limits Name",
			"  Through: 12/31,          !- Field 1",
			"  For: AllDays,            !- Field 2",
			"  Until: 24:00,1;          !- Field 3",
			" ",
			"ThermostatSetpoint:SingleHeating,",
			"  Core_top HeatSPSched,    !- Name",
			"  SNGL_HTGSETP_SCH;        !- Heating Setpoint Temperature Schedule Name",
			" ",
			"Schedule:Compact,",
			"  SNGL_HTGSETP_SCH,        !- Name",
			"  Temperature,             !- Schedule Type Limits Name",
 			"  Through: 12/31,          !- Field 1",
			"  For: AllDays,            !- Field 2",
			"  Until: 24:00,15.0;       !- Field 3",
			" ",
			"Zone,",
			"  Core_middle,             !- Name",
			"  0.0000,                  !- Direction of Relative North {deg}",
			"  0.0000,                  !- X Origin {m}",
			"  0.0000,                  !- Y Origin {m}",
			"  0.0000,                  !- Z Origin {m}",
			"  1,                       !- Type",
			"  1,                       !- Multiplier",
			"  ,                        !- Ceiling Height {m}",
			"  ,                        !- Volume {m3}",
			"  autocalculate,           !- Floor Area {m2}",
			"  ,                        !- Zone Inside Convection Algorithm",
			"  ,                        !- Zone Outside Convection Algorithm",
			"  Yes;                     !- Part of Total Floor Area",
			" ",
			"ZoneControl:Thermostat,",
			"  Core_middle Thermostat,  !- Name",
			"  Core_middle,             !- Zone or ZoneList Name",
			"  Single Cooling Control Type Sched,  !- Control Type Schedule Name",
			"  ThermostatSetpoint:SingleCooling,  !- Control 1 Object Type",
			"  Core_middle CoolSPSched; !- Control 1 Name",
			" ",
			"Schedule:Compact,",
			"  Single Cooling Control Type Sched,  !- Name",
			"  Control Type,            !- Schedule Type Limits Name",
			"  Through: 12/31,          !- Field 1",
			"  For: AllDays,            !- Field 2",
			"  Until: 24:00,2;          !- Field 3",
			" ",
			"ThermostatSetpoint:SingleCooling,",
			"  Core_middle CoolSPSched, !- Name",
			"  SNGL_CLGSETP_SCH;        !- Cooling Setpoint Temperature Schedule Name",
			" ",
			"Schedule:Compact,",
			"  SNGL_CLGSETP_SCH,        !- Name",
			"  Temperature,             !- Schedule Type Limits Name",
			"  Through: 12/31,          !- Field 1",
			"  For: AllDays,            !- Field 2",
			"  Until: 24:00,24.0;       !- Field 3",
			" ",
			"Zone,",
			"  Core_basement,             !- Name",
			"  0.0000,                  !- Direction of Relative North {deg}",
			"  0.0000,                  !- X Origin {m}",
			"  0.0000,                  !- Y Origin {m}",
			"  0.0000,                  !- Z Origin {m}",
			"  1,                       !- Type",
			"  1,                       !- Multiplier",
			"  ,                        !- Ceiling Height {m}",
			"  ,                        !- Volume {m3}",
			"  autocalculate,           !- Floor Area {m2}",
			"  ,                        !- Zone Inside Convection Algorithm",
			"  ,                        !- Zone Outside Convection Algorithm",
			"  Yes;                     !- Part of Total Floor Area",
			" ",
			"ZoneControl:Thermostat,",
			"  Core_basement Thermostat,  !- Name",
			"  Core_basement,             !- Zone or ZoneList Name",
			"  Single Cooling Heating Control Type Sched,  !- Control Type Schedule Name",
			"  ThermostatSetpoint:SingleHeatingOrCooling,  !- Control 1 Object Type",
			"  Core_basement CoolHeatSPSched; !- Control 1 Name",
			" ",
			"Schedule:Compact,",
			"  Single Cooling Heating Control Type Sched,  !- Name",
			"  Control Type,            !- Schedule Type Limits Name",
			"  Through: 12/31,          !- Field 1",
			"  For: AllDays,            !- Field 2",
			"  Until: 24:00,3;          !- Field 3",
			" ",
			"ThermostatSetpoint:SingleHeatingOrCooling,",
			"  Core_basement CoolHeatSPSched, !- Name",
			"  CLGHTGSETP_SCH;             !- Heating Setpoint Temperature Schedule Name",
			" ",
			"Zone,",
			"  Core_bottom,             !- Name",
			"  0.0000,                  !- Direction of Relative North {deg}",
			"  0.0000,                  !- X Origin {m}",
			"  0.0000,                  !- Y Origin {m}",
			"  0.0000,                  !- Z Origin {m}",
			"  1,                       !- Type",
			"  1,                       !- Multiplier",
			"  ,                        !- Ceiling Height {m}",
			"  ,                        !- Volume {m3}",
			"  autocalculate,           !- Floor Area {m2}",
			"  ,                        !- Zone Inside Convection Algorithm",
			"  ,                        !- Zone Outside Convection Algorithm",
			"  Yes;                     !- Part of Total Floor Area",
			" ",
			"ZoneControl:Thermostat,",
			"  Core_bottom Thermostat,  !- Name",
			"  Core_bottom,             !- Zone or ZoneList Name",
			"  Dual Zone Control Type Sched,  !- Control Type Schedule Name",
			"  ThermostatSetpoint:DualSetpoint,  !- Control 1 Object Type",
			"  Core_bottom DualSPSched; !- Control 1 Name",
			" ",
			"Schedule:Compact,",
			"  Dual Zone Control Type Sched,  !- Name",
			"  Control Type,            !- Schedule Type Limits Name",
			"  Through: 12/31,          !- Field 1",
			"  For: AllDays,            !- Field 2",
			"  Until: 24:00,4;          !- Field 3",
			" ",
			"ThermostatSetpoint:DualSetpoint,",
			"  Core_bottom DualSPSched, !- Name",
			"  HTGSETP_SCH,             !- Heating Setpoint Temperature Schedule Name",
			"  CLGSETP_SCH;             !- Cooling Setpoint Temperature Schedule Name",
			" ",
			"Schedule:Compact,",
			"  CLGSETP_SCH,             !- Name",
			"  Temperature,             !- Schedule Type Limits Name",
			"  Through: 12/31,          !- Field 1",
			"  For: AllDays,            !- Field 2",
			"  Until: 24:00,24.0;       !- Field 3",
			" ",
			"Schedule:Compact,",
			"  HTGSETP_SCH,             !- Name",
			"  Temperature,             !- Schedule Type Limits Name",
 			"  Through: 12/31,          !- Field 1",
			"  For: AllDays,            !- Field 2",
			"  Until: 24:00,15.0;       !- Field 3",
			" ",
			"Schedule:Compact,",
			"  CLGHTGSETP_SCH,          !- Name",
			"  Temperature,             !- Schedule Type Limits Name",
 			"  Through: 12/31,          !- Field 1",
			"  For: AllDays,            !- Field 2",
			"  Until: 24:00,24.0;       !- Field 3",
		});

		ASSERT_FALSE( process_idf( idf_objects ) );

		bool ErrorsFound( false ); // If errors detected in input
		GetZoneData( ErrorsFound );
		ASSERT_FALSE( ErrorsFound );

		int HeatZoneNum( 1 );
		int CoolZoneNum( 2 );
		int CoolHeatZoneNum( 3 );
		int DualZoneNum( 4 );

		NumOfTimeStepInHour = 1; // must initialize this to get schedules initialized
		MinutesPerTimeStep = 60; // must initialize this to get schedules initialized
		ProcessScheduleInput(); // read schedules

		DaySchedule( 1 ).TSValue = 1;
		DaySchedule( 3 ).TSValue = 2;
		DaySchedule( 5 ).TSValue = 3;
		DaySchedule( 6 ).TSValue = 4;

		GetZoneAirSetPoints();

		DeadBandOrSetback.allocate( NumTempControlledZones );
		CurDeadBandOrSetback.allocate( NumTempControlledZones );
		TempControlType.allocate( NumTempControlledZones );
		ZoneSysEnergyDemand.allocate( NumTempControlledZones );
		TempZoneThermostatSetPoint.allocate( NumTempControlledZones );
		ZoneSetPointLast.allocate( NumTempControlledZones );
		Setback.allocate( NumTempControlledZones );
		ZoneThermostatSetPointLo.allocate( NumTempControlledZones );
		ZoneThermostatSetPointHi.allocate( NumTempControlledZones );
		TempDepZnLd.allocate( NumTempControlledZones );
		TempIndZnLd.allocate( NumTempControlledZones );
		TempDepZnLd = 0.0;
		TempIndZnLd = 0.0;

		SNLoadPredictedRate.allocate( NumTempControlledZones );
		LoadCorrectionFactor.allocate( NumTempControlledZones );
		SNLoadPredictedHSPRate.allocate( NumTempControlledZones );
		SNLoadPredictedCSPRate.allocate( NumTempControlledZones );

		LoadCorrectionFactor( HeatZoneNum ) = 1.0;
		LoadCorrectionFactor( CoolZoneNum ) = 1.0;
		LoadCorrectionFactor( CoolHeatZoneNum ) = 1.0;
		LoadCorrectionFactor( DualZoneNum ) = 1.0;

		// The following parameters describe the setpoint types in TempControlType(ActualZoneNum)
		//	extern int const SingleHeatingSetPoint; = 1
		//	extern int const SingleCoolingSetPoint; = 2
		//	extern int const SingleHeatCoolSetPoint; = 3
		//	extern int const DualSetPointWithDeadBand; = 4
		Schedule( TempControlledZone( HeatZoneNum ).CTSchedIndex ).CurrentValue = DataHVACGlobals::SingleHeatingSetPoint;
		Schedule( TempControlledZone( CoolZoneNum ).CTSchedIndex ).CurrentValue = DataHVACGlobals::SingleCoolingSetPoint;
		Schedule( TempControlledZone( CoolHeatZoneNum ).CTSchedIndex ).CurrentValue = DataHVACGlobals::SingleHeatCoolSetPoint;

		Schedule( TempControlledZone( DualZoneNum ).CTSchedIndex ).CurrentValue = 0; // simulate no thermostat or non-controlled zone

		ZoneSysEnergyDemand( DualZoneNum ).TotalOutputRequired = 0.0; // no load and no thermostat since control type is set to 0 above
		CalcZoneAirTempSetPoints();
		CalcPredictedSystemLoad( DualZoneNum, 1.0 );

		EXPECT_EQ( 0.0, TempZoneThermostatSetPoint( DualZoneNum ) ); // Set point initialized to 0 and never set since thermostat control type = 0

		Schedule( TempControlledZone( DualZoneNum ).CTSchedIndex ).CurrentValue = DataHVACGlobals::DualSetPointWithDeadBand; // reset Tstat control schedule to dual thermostat control

		// set up a back calculated load
		// for the first few, TempIndZnLd() = 0.0
		// LoadToHeatingSetPoint = ( TempDepZnLd( ZoneNum ) * ( TempZoneThermostatSetPoint( ZoneNum ) ) - TempIndZnLd( ZoneNum ) );
		// LoadToCoolingSetPoint = ( TempDepZnLd( ZoneNum ) * ( TempZoneThermostatSetPoint( ZoneNum ) ) - TempIndZnLd( ZoneNum ) );
		int SetPointTempSchedIndex = SetPointSingleHeating( TempControlledZone( HeatZoneNum ).ControlTypeSchIndx( TempControlledZone( HeatZoneNum ).SchIndx_SingleHeatSetPoint ) ).TempSchedIndex;
		Schedule( SetPointTempSchedIndex ).CurrentValue = 20.0;
		ZoneSysEnergyDemand( HeatZoneNum ).TotalOutputRequired = -1000.0; // cooling load
		TempDepZnLd( HeatZoneNum ) = ZoneSysEnergyDemand( HeatZoneNum ).TotalOutputRequired / Schedule( SetPointTempSchedIndex ).CurrentValue;

		CalcZoneAirTempSetPoints();
		CalcPredictedSystemLoad( HeatZoneNum, 1.0 );

		EXPECT_EQ( 20.0, TempZoneThermostatSetPoint( HeatZoneNum ) );
		EXPECT_EQ( -1000.0, ZoneSysEnergyDemand( HeatZoneNum ).TotalOutputRequired ); // TotalOutputRequired gets updated in CalcPredictedSystemLoad based on the load
		EXPECT_TRUE( CurDeadBandOrSetback( HeatZoneNum ) ); // Tstat should show there is no load on a single heating SP

		SetPointTempSchedIndex = SetPointSingleHeating( TempControlledZone( HeatZoneNum ).ControlTypeSchIndx( TempControlledZone( HeatZoneNum ).SchIndx_SingleHeatSetPoint ) ).TempSchedIndex;
		Schedule( SetPointTempSchedIndex ).CurrentValue = 21.0;
		ZoneSysEnergyDemand( HeatZoneNum ).TotalOutputRequired = 1000.0; // heating load
		TempDepZnLd( HeatZoneNum ) = ZoneSysEnergyDemand( HeatZoneNum ).TotalOutputRequired / Schedule( SetPointTempSchedIndex ).CurrentValue;

		SetPointTempSchedIndex = SetPointSingleCooling( TempControlledZone( CoolZoneNum ).ControlTypeSchIndx( TempControlledZone( CoolZoneNum ).SchIndx_SingleCoolSetPoint ) ).TempSchedIndex;
		Schedule( SetPointTempSchedIndex ).CurrentValue = 23.0;
		ZoneSysEnergyDemand( CoolZoneNum ).TotalOutputRequired = -3000.0; // cooling load
		TempDepZnLd( CoolZoneNum ) = ZoneSysEnergyDemand( CoolZoneNum ).TotalOutputRequired / Schedule( SetPointTempSchedIndex ).CurrentValue;

		SetPointTempSchedIndex = SetPointSingleHeatCool( TempControlledZone( CoolHeatZoneNum ).ControlTypeSchIndx( TempControlledZone( CoolHeatZoneNum ).SchIndx_SingleHeatCoolSetPoint ) ).TempSchedIndex;
		Schedule( SetPointTempSchedIndex ).CurrentValue = 22.0;
		ZoneSysEnergyDemand( CoolHeatZoneNum ).TotalOutputRequired = -4000.0; // cooling load
		TempDepZnLd( CoolHeatZoneNum ) = ZoneSysEnergyDemand( CoolHeatZoneNum ).TotalOutputRequired / Schedule( SetPointTempSchedIndex ).CurrentValue;

		SetPointTempSchedIndex = SetPointDualHeatCool( TempControlledZone( DualZoneNum ).ControlTypeSchIndx( TempControlledZone( DualZoneNum ).SchIndx_DualSetPointWDeadBand ) ).CoolTempSchedIndex;
		Schedule( SetPointTempSchedIndex ).CurrentValue = 24.0;
		SetPointTempSchedIndex = SetPointDualHeatCool( TempControlledZone( DualZoneNum ).ControlTypeSchIndx( TempControlledZone( DualZoneNum ).SchIndx_DualSetPointWDeadBand ) ).HeatTempSchedIndex;
		Schedule( SetPointTempSchedIndex ).CurrentValue = 20.0;
		ZoneSysEnergyDemand( DualZoneNum ).TotalOutputRequired = 2500.0; // heating load
		TempDepZnLd( DualZoneNum ) = ZoneSysEnergyDemand( DualZoneNum ).TotalOutputRequired / Schedule( SetPointTempSchedIndex ).CurrentValue;

		CalcZoneAirTempSetPoints();
		CalcPredictedSystemLoad( HeatZoneNum, 1.0 );

		EXPECT_EQ( 21.0, TempZoneThermostatSetPoint( HeatZoneNum ) );
		EXPECT_FALSE( CurDeadBandOrSetback( HeatZoneNum ) ); // Tstat should show there is load on a single heating SP
		EXPECT_EQ( 1000.0, ZoneSysEnergyDemand( HeatZoneNum ).TotalOutputRequired ); // TotalOutputRequired gets updated in CalcPredictedSystemLoad based on the load

		CalcPredictedSystemLoad( CoolZoneNum, 1.0 );

		EXPECT_EQ( 23.0, TempZoneThermostatSetPoint( CoolZoneNum ) );
		EXPECT_FALSE( CurDeadBandOrSetback( CoolZoneNum ) ); // Tstat should show there is load on a single cooling SP
		EXPECT_EQ( -3000.0, ZoneSysEnergyDemand( CoolZoneNum ).TotalOutputRequired ); // TotalOutputRequired gets updated in CalcPredictedSystemLoad based on the load

		CalcPredictedSystemLoad( CoolHeatZoneNum, 1.0 );

		ASSERT_EQ( 22.0, TempZoneThermostatSetPoint( CoolHeatZoneNum ) );
		EXPECT_FALSE( CurDeadBandOrSetback( CoolHeatZoneNum ) ); // Tstat should show there is load on a single heating or cooling SP
		EXPECT_EQ( -4000.0, ZoneSysEnergyDemand( CoolHeatZoneNum ).TotalOutputRequired ); // TotalOutputRequired gets updated in CalcPredictedSystemLoad based on the load

		CalcPredictedSystemLoad( DualZoneNum, 1.0 );

		EXPECT_EQ( 20.0, TempZoneThermostatSetPoint( DualZoneNum ) );
		EXPECT_FALSE( CurDeadBandOrSetback( DualZoneNum ) ); // Tstat should show there is load on a dual SP
		EXPECT_EQ( 2500.0, ZoneSysEnergyDemand( DualZoneNum ).TotalOutputRequired ); // TotalOutputRequired gets updated in CalcPredictedSystemLoad based on the load

		SetPointTempSchedIndex = SetPointDualHeatCool( TempControlledZone( DualZoneNum ).ControlTypeSchIndx( TempControlledZone( DualZoneNum ).SchIndx_DualSetPointWDeadBand ) ).CoolTempSchedIndex;
		Schedule( SetPointTempSchedIndex ).CurrentValue = 25.0;
		ZoneSysEnergyDemand( DualZoneNum ).TotalOutputRequired = 1000.0;
		// LoadToCoolingSetPoint = ( TempDepZnLd( ZoneNum ) * ( TempZoneThermostatSetPoint( ZoneNum ) ) - TempIndZnLd( ZoneNum ) );
		TempDepZnLd( DualZoneNum ) = ZoneSysEnergyDemand( DualZoneNum ).TotalOutputRequired / Schedule( SetPointTempSchedIndex ).CurrentValue;
		TempIndZnLd( DualZoneNum ) = 3500.0; // results in a cooling load

		CalcZoneAirTempSetPoints();
		CalcPredictedSystemLoad( DualZoneNum, 1.0 );

		EXPECT_EQ( 25.0, TempZoneThermostatSetPoint( DualZoneNum ) );
		EXPECT_FALSE( CurDeadBandOrSetback( DualZoneNum ) ); // Tstat should show there is load on a dual SP
		EXPECT_EQ( -2500.0, ZoneSysEnergyDemand( DualZoneNum ).TotalOutputRequired ); // should show a cooling load

		NumTempControlledZones = 0;
		Zone.deallocate();
		DeadBandOrSetback.deallocate();
		CurDeadBandOrSetback.deallocate();
		TempControlType.deallocate();
		TempControlledZone.deallocate();
		ZoneSysEnergyDemand.deallocate();
		TempZoneThermostatSetPoint.deallocate();
		ZoneSetPointLast.deallocate();
		Setback.deallocate();
		ZoneThermostatSetPointLo.deallocate();
		ZoneThermostatSetPointHi.deallocate();
		SNLoadPredictedRate.deallocate();
		LoadCorrectionFactor.deallocate();
		SNLoadPredictedHSPRate.deallocate();
		SNLoadPredictedCSPRate.deallocate();
		TempDepZnLd.deallocate();
		TempIndZnLd.deallocate();
		OccRoomTSetPointHeat.deallocate();
		OccRoomTSetPointCool.deallocate();

	}
