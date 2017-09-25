// EnergyPlus, Copyright (c) 1996-2017, The Board of Trustees of the University of Illinois,
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy), Oak Ridge
// National Laboratory, managed by UT-Battelle, Alliance for Sustainable Energy, LLC, and other
// contributors. All rights reserved.
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

// EnergyPlus::ZoneEquipmentManager Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/ZoneEquipmentManager.hh>
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataAirSystems.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/HeatBalanceAirManager.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/DataEnvironment.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace ObjexxFCL;
using namespace EnergyPlus::ZoneEquipmentManager;
using namespace EnergyPlus::DataLoopNode;
using namespace EnergyPlus::DataZoneEquipment;
using namespace EnergyPlus::HeatBalanceAirManager;
using namespace EnergyPlus::HeatBalanceManager;

TEST_F( EnergyPlusFixture, ZoneEquipmentManager_CalcZoneMassBalanceTest )
{

	std::string const idf_objects = delimited_string({
		" Version,8.4;",

		"Zone,",
		"  Space;                   !- Name",

		"ZoneHVAC:EquipmentConnections,",
		" Space,                    !- Zone Name",
		" Space Equipment,          !- Zone Conditioning Equipment List Name",
		" Space In Node,            !- Zone Air Inlet Node or NodeList Name",
		" Space Exh Nodes,           !- Zone Air Exhaust Node or NodeList Name",
		" Space Node,               !- Zone Air Node Name",
		" Space Ret Node;           !- Zone Return Air Node Name",

		"ZoneHVAC:EquipmentList,",
		" Space Equipment,          !- Name",
		" Fan:ZoneExhaust,          !- Zone Equipment 1 Object Type",
		" Exhaust Fan,              !- Zone Equipment 1 Name",
		" 1,                        !- Zone Equipment 1 Cooling Sequence",
		" 1;                        !- Zone Equipment 1 Heating or No - Load Sequence",

		"Fan:ZoneExhaust,",
		"Exhaust Fan,               !- Name",
		",                          !- Availability Schedule Name",
		"0.338,                     !- Fan Total Efficiency",
		"125.0000,                  !- Pressure Rise{Pa}",
		"0.3000,                    !- Maximum Flow Rate{m3/s}",
		"Exhaust Fan Inlet Node,    !- Air Inlet Node Name",
		"Exhaust Fan Outlet Node,   !- Air Outlet Node Name",
		"Zone Exhaust Fans;         !- End - Use Subcategory",

		"NodeList,",
		"  Space Exh Nodes,  !- Name",
		"  Space ZoneHVAC Exh Node, !- Node 1 Name",
		"  Exhaust Fan Inlet Node; !- Node 1 Name",

	} );

	ASSERT_FALSE( process_idf( idf_objects ) );
	EXPECT_FALSE( has_err_output() );
	bool ErrorsFound = false;
	GetZoneData( ErrorsFound );
	AllocateHeatBalArrays();
	GetZoneEquipmentData1();
	ZoneEquipInputsFilled = true;
	GetSimpleAirModelInputs( ErrorsFound );
	int ZoneNum = 1;
	int NodeNum;
	for ( NodeNum = 1; NodeNum <= ZoneEquipConfig( ZoneNum ).NumInletNodes; ++NodeNum ) {
		Node( ZoneEquipConfig( ZoneNum ).InletNode( NodeNum ) ).MassFlowRate = 1.0;
	}

	ZoneEquipConfig( ZoneNum ).ReturnNodeAirLoopNum( 1 ) = 0;
	ZoneEquipConfig( ZoneNum ).ReturnNodeInletNum ( 1 ) = 1;

	// Test here - if zone equipment exhausts slightly more than it supplies, there should be no unbalanced exhaust flow warning
	Node( ZoneEquipConfig( ZoneNum ).ExhaustNode( 1 ) ).MassFlowRate = 1.000000001;
	CalcZoneMassBalance( );
	EXPECT_FALSE( has_err_output() );

	// Add excess balanced zone exhaust from exhaust fan, still no warning
	ZoneEquipConfig( ZoneNum ).ZoneExh = 0.5;
	ZoneEquipConfig( ZoneNum ).ZoneExhBalanced = 0.5;
	Node( ZoneEquipConfig( ZoneNum ).ExhaustNode( 2 ) ).MassFlowRate = 0.5;
	CalcZoneMassBalance();
	EXPECT_FALSE( has_err_output() );

	// Add excess unbalanced zone exhaust from exhaust fan, now there should be warning
	ZoneEquipConfig( ZoneNum ).ZoneExh = 0.5;
	ZoneEquipConfig( ZoneNum ).ZoneExhBalanced = 0.0;
	Node( ZoneEquipConfig( ZoneNum ).ExhaustNode( 2 ) ).MassFlowRate = 0.5;
	CalcZoneMassBalance();
	EXPECT_TRUE( has_err_output() );

	// Deallocate everything - should all be taken care of in clear_states
}

TEST_F( EnergyPlusFixture, ZoneEquipmentManager_MultiCrossMixingTest )
{

	std::string const idf_objects = delimited_string( {
		" Version,8.5;",

		"  Zone,",
		"    SPACE1-1,                !- Name",
		"    0,                       !- Direction of Relative North {deg}",
		"    0,                       !- X Origin {m}",
		"    0,                       !- Y Origin {m}",
		"    0,                       !- Z Origin {m}",
		"    1,                       !- Type",
		"    1,                       !- Multiplier",
		"    2.438400269,             !- Ceiling Height {m}",
		"    239.247360229;           !- Volume {m3}",

		"  Zone,",
		"    SPACE2-1,                !- Name",
		"    0,                       !- Direction of Relative North {deg}",
		"    0,                       !- X Origin {m}",
		"    0,                       !- Y Origin {m}",
		"    0,                       !- Z Origin {m}",
		"    1,                       !- Type",
		"    1,                       !- Multiplier",
		"    2.438400269,             !- Ceiling Height {m}",
		"    103.311355591;           !- Volume {m3}",

		"  Zone,",
		"    SPACE3-1,                !- Name",
		"    0,                       !- Direction of Relative North {deg}",
		"    0,                       !- X Origin {m}",
		"    0,                       !- Y Origin {m}",
		"    0,                       !- Z Origin {m}",
		"    1,                       !- Type",
		"    1,                       !- Multiplier",
		"    2.438400269,             !- Ceiling Height {m}",
		"    239.247360229;           !- Volume {m3}",

		"  Zone,",
		"    SPACE4-1,                !- Name",
		"    0,                       !- Direction of Relative North {deg}",
		"    0,                       !- X Origin {m}",
		"    0,                       !- Y Origin {m}",
		"    0,                       !- Z Origin {m}",
		"    1,                       !- Type",
		"    1,                       !- Multiplier",
		"    2.438400269,             !- Ceiling Height {m}",
		"    103.311355591;           !- Volume {m3}",

		"  Zone,",
		"    SPACE5-1,                !- Name",
		"    0,                       !- Direction of Relative North {deg}",
		"    0,                       !- X Origin {m}",
		"    0,                       !- Y Origin {m}",
		"    0,                       !- Z Origin {m}",
		"    1,                       !- Type",
		"    1,                       !- Multiplier",
		"    2.438400269,             !- Ceiling Height {m}",
		"    447.682556152;           !- Volume {m3}",

		"  Schedule:Compact,",
		"    MixingAvailSched,        !- Name",
		"    Fraction,                !- Schedule Type Limits Name",
		"    Through: 3/31,           !- Field 1",
		"    For: AllDays,            !- Field 2",
		"    Until: 24:00,1.00,       !- Field 3",
		"    Through: 9/30,           !- Field 5",
		"    For: Weekdays,           !- Field 6",
		"    Until: 7:00,1.00,        !- Field 7",
		"    Until: 17:00,1.00,       !- Field 9",
		"    Until: 24:00,1.00,       !- Field 11",
		"    For: Weekends Holidays CustomDay1 CustomDay2, !- Field 13",
		"    Until: 24:00,1.00,       !- Field 14",
		"    For: SummerDesignDay WinterDesignDay, !- Field 16",
		"    Until: 24:00,1.00,       !- Field 17",
		"    Through: 12/31,          !- Field 19",
		"    For: AllDays,            !- Field 20",
		"    Until: 24:00,1.00;       !- Field 21",

		"  Schedule:Compact,",
		"    MinIndoorTemp,           !- Name",
		"    Any Number,              !- Schedule Type Limits Name",
		"    Through: 12/31,          !- Field 1",
		"    For: AllDays,            !- Field 2",
		"    Until: 24:00,18;         !- Field 3",

		"  Schedule:Compact,",
		"    MaxIndoorTemp,           !- Name",
		"    Any Number,              !- Schedule Type Limits Name",
		"    Through: 12/31,          !- Field 1",
		"    For: AllDays,            !- Field 2",
		"    Until: 24:00,100;        !- Field 3",

		"  Schedule:Compact,",
		"    DeltaTemp,               !- Name",
		"    Any Number,              !- Schedule Type Limits Name",
		"    Through: 12/31,          !- Field 1",
		"    For: AllDays,            !- Field 2",
		"    Until: 24:00,2;          !- Field 3",

		"  Schedule:Compact,",
		"    MinOutdoorTemp,          !- Name",
		"    Any Number,              !- Schedule Type Limits Name",
		"    Through: 12/31,          !- Field 1",
		"    For: AllDays,            !- Field 2",
		"    Until: 24:00,-100;       !- Field 3",

		"  Schedule:Compact,",
		"    MaxOutdoorTemp,          !- Name",
		"    Any Number,              !- Schedule Type Limits Name",
		"    Through: 12/31,          !- Field 1",
		"    For: AllDays,            !- Field 2",
		"    Until: 24:00,100;        !- Field 3",

		"  ZoneCrossMixing,",
		"    SPACE2-4 XMixng 1,       !- Name",
		"    SPACE2-1,                !- Zone Name",
		"    MixingAvailSched,        !- Schedule Name",
		"    flow/zone,               !- Design Flow Rate Calculation Method",
		"    0.1,                     !- Design Flow Rate {m3/s}",
		"    ,                        !- Flow Rate per Zone Floor Area {m3/s-m2}",
		"    ,                        !- Flow Rate per Person {m3/s-person}",
		"    ,                        !- Air Changes per Hour {1/hr}",
		"    SPACE4-1,                !- Source Zone Name",
		"    1.0,                     !- Delta Temperature {deltaC}",
		"    ,                        !- Delta Temperature Schedule Name",
		"    MinIndoorTemp,           !- Minimum Zone Temperature Schedule Name",
		"    MaxIndoorTemp,           !- Maximum Zone Temperature Schedule Name",
		"    MinIndoorTemp,           !- Minimum Source Zone Temperature Schedule Name",
		"    MaxIndoorTemp,           !- Maximum Source Zone Temperature Schedule Name",
		"    MinOutdoorTemp,          !- Minimum Outdoor Temperature Schedule Name",
		"    MaxOutdoorTemp;          !- Maximum Outdoor Temperature Schedule Name",

		"  ZoneCrossMixing,",
		"    SPACE4-2 XMixng 1,       !- Name",
		"    SPACE4-1,                !- Zone Name",
		"    MixingAvailSched,        !- Schedule Name",
		"    flow/zone,               !- Design Flow Rate Calculation Method",
		"    0.1,                     !- Design Flow Rate {m3/s}",
		"    ,                        !- Flow Rate per Zone Floor Area {m3/s-m2}",
		"    ,                        !- Flow Rate per Person {m3/s-person}",
		"    ,                        !- Air Changes per Hour {1/hr}",
		"    SPACE2-1,                !- Source Zone Name",
		"    1.0,                     !- Delta Temperature {deltaC}",
		"    ,                        !- Delta Temperature Schedule Name",
		"    MinIndoorTemp,           !- Minimum Zone Temperature Schedule Name",
		"    MaxIndoorTemp,           !- Maximum Zone Temperature Schedule Name",
		"    MinIndoorTemp,           !- Minimum Source Zone Temperature Schedule Name",
		"    MaxIndoorTemp,           !- Maximum Source Zone Temperature Schedule Name",
		"    MinOutdoorTemp,          !- Minimum Outdoor Temperature Schedule Name",
		"    MaxOutdoorTemp;          !- Maximum Outdoor Temperature Schedule Name",

		"  ZoneCrossMixing,",
		"    SPACE3-4 XMixng 1,       !- Name",
		"    SPACE3-1,                !- Zone Name",
		"    MixingAvailSched,        !- Schedule Name",
		"    flow/zone,               !- Design Flow Rate Calculation Method",
		"    0.2,                     !- Design Flow Rate {m3/s}",
		"    ,                        !- Flow Rate per Zone Floor Area {m3/s-m2}",
		"    ,                        !- Flow Rate per Person {m3/s-person}",
		"    ,                        !- Air Changes per Hour {1/hr}",
		"    SPACE4-1,                !- Source Zone Name",
		"    0.0,                     !- Delta Temperature {deltaC}",
		"    ,                        !- Delta Temperature Schedule Name",
		"    MinIndoorTemp,           !- Minimum Zone Temperature Schedule Name",
		"    MaxIndoorTemp,           !- Maximum Zone Temperature Schedule Name",
		"    MinIndoorTemp,           !- Minimum Source Zone Temperature Schedule Name",
		"    MaxIndoorTemp,           !- Maximum Source Zone Temperature Schedule Name",
		"    MinOutdoorTemp,          !- Minimum Outdoor Temperature Schedule Name",
		"    MaxOutdoorTemp;          !- Maximum Outdoor Temperature Schedule Name",

		"  ZoneCrossMixing,",
		"    SPACE1-4 XMixng 1,       !- Name",
		"    SPACE1-1,                !- Zone Name",
		"    MixingAvailSched,        !- Schedule Name",
		"    flow/zone,               !- Design Flow Rate Calculation Method",
		"    0.3,                     !- Design Flow Rate {m3/s}",
		"    ,                        !- Flow Rate per Zone Floor Area {m3/s-m2}",
		"    ,                        !- Flow Rate per Person {m3/s-person}",
		"    ,                        !- Air Changes per Hour {1/hr}",
		"    SPACE4-1,                !- Source Zone Name",
		"    0.0,                     !- Delta Temperature {deltaC}",
		"    ,                        !- Delta Temperature Schedule Name",
		"    MinIndoorTemp,           !- Minimum Zone Temperature Schedule Name",
		"    MaxIndoorTemp,           !- Maximum Zone Temperature Schedule Name",
		"    MinIndoorTemp,           !- Minimum Source Zone Temperature Schedule Name",
		"    MaxIndoorTemp,           !- Maximum Source Zone Temperature Schedule Name",
		"    MinOutdoorTemp,          !- Minimum Outdoor Temperature Schedule Name",
		"    MaxOutdoorTemp;          !- Maximum Outdoor Temperature Schedule Name",

		"  ZoneCrossMixing,",
		"    SPACE1-3 XMixng 1,       !- Name",
		"    SPACE1-1,                !- Zone Name",
		"    MixingAvailSched,        !- Schedule Name",
		"    flow/zone,               !- Design Flow Rate Calculation Method",
		"    0.3,                     !- Design Flow Rate {m3/s}",
		"    ,                        !- Flow Rate per Zone Floor Area {m3/s-m2}",
		"    ,                        !- Flow Rate per Person {m3/s-person}",
		"    ,                        !- Air Changes per Hour {1/hr}",
		"    SPACE3-1,                !- Source Zone Name",
		"    0.0,                     !- Delta Temperature {deltaC}",
		"    ,                        !- Delta Temperature Schedule Name",
		"    MinIndoorTemp,           !- Minimum Zone Temperature Schedule Name",
		"    MaxIndoorTemp,           !- Maximum Zone Temperature Schedule Name",
		"    MinIndoorTemp,           !- Minimum Source Zone Temperature Schedule Name",
		"    MaxIndoorTemp,           !- Maximum Source Zone Temperature Schedule Name",
		"    MinOutdoorTemp,          !- Minimum Outdoor Temperature Schedule Name",
		"    MaxOutdoorTemp;          !- Maximum Outdoor Temperature Schedule Name",

	} );

	ASSERT_FALSE( process_idf( idf_objects ) );
	EXPECT_FALSE( has_err_output( ) );
	bool ErrorsFound = false;
	ScheduleManager::ProcessScheduleInput( );
	GetZoneData( ErrorsFound );
	DataHeatBalFanSys::ZoneReOrder.allocate( NumOfZones );

	GetSimpleAirModelInputs( ErrorsFound );

	EXPECT_FALSE( ErrorsFound );

	DataHeatBalFanSys::MAT.allocate( NumOfZones );
	DataHeatBalFanSys::ZoneAirHumRat.allocate( NumOfZones );
	DataHeatBalFanSys::MCPM.allocate( NumOfZones );
	DataHeatBalFanSys::MCPTM.allocate( NumOfZones );

	DataHeatBalFanSys::MCPI.allocate( NumOfZones );
	DataHeatBalFanSys::OAMFL.allocate( NumOfZones );
	DataHeatBalFanSys::MCPTI.allocate( NumOfZones );

	DataHeatBalFanSys::MixingMassFlowZone.allocate( NumOfZones );
	DataHeatBalFanSys::MixingMassFlowXHumRat.allocate( NumOfZones );

	DataHeatBalFanSys::MAT( 1 ) = 21.0;
	DataHeatBalFanSys::MAT( 2 ) = 22.0;
	DataHeatBalFanSys::MAT( 3 ) = 23.0;
	DataHeatBalFanSys::MAT( 4 ) = 24.0;
	DataHeatBalFanSys::MAT( 5 ) = 25.0;
	DataHeatBalFanSys::ZoneAirHumRat( 1 ) = 0.001;
	DataHeatBalFanSys::ZoneAirHumRat( 2 ) = 0.001;
	DataHeatBalFanSys::ZoneAirHumRat( 3 ) = 0.001;
	DataHeatBalFanSys::ZoneAirHumRat( 4 ) = 0.001;
	DataHeatBalFanSys::ZoneAirHumRat( 5 ) = 0.001;

	DataHeatBalance::AirFlowFlag = 1;
	ScheduleManager::Schedule( 1 ).CurrentValue = 1.0;
	ScheduleManager::Schedule( 2 ).CurrentValue = 18.0;
	ScheduleManager::Schedule( 3 ).CurrentValue = 100.0;
	ScheduleManager::Schedule( 4 ).CurrentValue = 2.0;
	ScheduleManager::Schedule( 5 ).CurrentValue = -100.0;
	ScheduleManager::Schedule( 6 ).CurrentValue = 100.0;
	DataEnvironment::OutBaroPress = 101325.0;

	InitSimpleMixingConvectiveHeatGains( );

	CalcAirFlowSimple( 2 );

	EXPECT_NEAR( 720.738493, DataHeatBalFanSys::MCPM( 1 ), 0.00001 );
	EXPECT_NEAR( 119.818784, DataHeatBalFanSys::MCPM( 2 ), 0.00001 );
	EXPECT_NEAR( 599.907893, DataHeatBalFanSys::MCPM( 3 ), 0.00001 );
	EXPECT_NEAR( 719.116710, DataHeatBalFanSys::MCPM( 4 ), 0.00001 );
	EXPECT_NEAR( 16937.0496, DataHeatBalFanSys::MCPTM( 1 ), 0.001 );
	EXPECT_NEAR( 2875.6508, DataHeatBalFanSys::MCPTM( 2 ), 0.001 );
	EXPECT_NEAR( 13315.7667, DataHeatBalFanSys::MCPTM( 3 ), 0.001 );
	EXPECT_NEAR( 15699.7370, DataHeatBalFanSys::MCPTM( 4 ), 0.001 );
	EXPECT_NEAR( 0.71594243, DataHeatBalFanSys::MixingMassFlowZone( 1 ), 0.00001 );
	EXPECT_NEAR( 0.11902146, DataHeatBalFanSys::MixingMassFlowZone( 2 ), 0.00001 );
	EXPECT_NEAR( 0.59591588, DataHeatBalFanSys::MixingMassFlowZone( 3 ), 0.00001 );
	EXPECT_NEAR( 0.71433143, DataHeatBalFanSys::MixingMassFlowZone( 4 ), 0.00001 );
	EXPECT_NEAR( 0.00071594243, DataHeatBalFanSys::MixingMassFlowXHumRat( 1 ), 0.0000001 );
	EXPECT_NEAR( 0.00011902146, DataHeatBalFanSys::MixingMassFlowXHumRat( 2 ), 0.0000001 );
	EXPECT_NEAR( 0.00059591588, DataHeatBalFanSys::MixingMassFlowXHumRat( 3 ), 0.0000001 );
	EXPECT_NEAR( 0.00071433143, DataHeatBalFanSys::MixingMassFlowXHumRat( 4 ), 0.0000001 );

	// Deallocate everything - should all be taken care of in clear_states

	DataHeatBalFanSys::MAT.deallocate( );
	DataHeatBalFanSys::ZoneAirHumRat.deallocate( );
	DataHeatBalFanSys::MCPM.deallocate( );
	DataHeatBalFanSys::MCPTM.deallocate( );
	DataHeatBalFanSys::MCPI.deallocate( );
	DataHeatBalFanSys::OAMFL.deallocate( );
	DataHeatBalFanSys::MCPTI.deallocate( );
	DataHeatBalFanSys::MixingMassFlowZone.deallocate( );
	DataHeatBalFanSys::MixingMassFlowXHumRat.deallocate( );
	DataHeatBalFanSys::ZoneReOrder.deallocate( );

}

TEST_F( EnergyPlusFixture, ZoneEquipmentManager_CalcZoneMassBalanceTest2 )
{

	std::string const idf_objects = delimited_string({
		" Version,8.4;",

		"Zone,",
		"  Space;                   !- Name",

		"ZoneHVAC:EquipmentConnections,",
		" Space,                    !- Zone Name",
		" Space Equipment,          !- Zone Conditioning Equipment List Name",
		" Space Inlet Nodes,            !- Zone Air Inlet Node or NodeList Name",
		" Space Exh Nodes,           !- Zone Air Exhaust Node or NodeList Name",
		" Space Node,               !- Zone Air Node Name",
		" Space Return Nodes;           !- Zone Return Air Node or NodeList Name",

		"ZoneHVAC:EquipmentList,",
		" Space Equipment,          !- Name",
		" Fan:ZoneExhaust,          !- Zone Equipment 1 Object Type",
		" Exhaust Fan,              !- Zone Equipment 1 Name",
		" 1,                        !- Zone Equipment 1 Cooling Sequence",
		" 1;                        !- Zone Equipment 1 Heating or No - Load Sequence",

		"Fan:ZoneExhaust,",
		"Exhaust Fan,               !- Name",
		",                          !- Availability Schedule Name",
		"0.338,                     !- Fan Total Efficiency",
		"125.0000,                  !- Pressure Rise{Pa}",
		"0.3000,                    !- Maximum Flow Rate{m3/s}",
		"Exhaust Fan Inlet Node,    !- Air Inlet Node Name",
		"Exhaust Fan Outlet Node,   !- Air Outlet Node Name",
		"Zone Exhaust Fans;         !- End - Use Subcategory",

		"NodeList,",
		"  Space Exh Nodes,  !- Name",
		"  Space ZoneHVAC Exh Node, !- Node 1 Name",
		"  Exhaust Fan Inlet Node; !- Node 2 Name",

		"NodeList,",
		"  Space Inlet Nodes,  !- Name",
		"  Space Inlet Node 1, !- Node 1 Name",
		"  Space Inlet Node 2, !- Node 2 Name",
		"  Space Inlet Node 3; !- Node 3 Name",

		"NodeList,",
		"  Space Return Nodes,  !- Name",
		"  Space Return Node 1, !- Node 1 Name",
		"  Space Return Node 2, !- Node 2 Name",
		"  Space Return Node 3; !- Node 3 Name",

	} );

	ASSERT_FALSE( process_idf( idf_objects ) );
	EXPECT_FALSE( has_err_output() );
	bool ErrorsFound = false;
	GetZoneData( ErrorsFound );
	AllocateHeatBalArrays();
	GetZoneEquipmentData1();
	ZoneEquipInputsFilled = true;
	GetSimpleAirModelInputs( ErrorsFound );

	int ZoneNum = 1;
	for ( int NodeNum = 1; NodeNum <= ZoneEquipConfig( ZoneNum ).NumInletNodes; ++NodeNum ) {
		Node( ZoneEquipConfig ( ZoneNum ).InletNode( NodeNum ) ).MassFlowRate = 1.0;
	}
	ZoneEquipConfig( ZoneNum ).InletNodeAirLoopNum( 1 ) = 2; // Intentionally not in 1,2,3 order
	ZoneEquipConfig( ZoneNum ).InletNodeAirLoopNum( 2 ) = 3;
	ZoneEquipConfig( ZoneNum ).InletNodeAirLoopNum( 3 ) = 1;
	ZoneEquipConfig( ZoneNum ).ReturnNodeAirLoopNum( 1 ) = 3; // Intentionally in a different order
	ZoneEquipConfig( ZoneNum ).ReturnNodeAirLoopNum( 2 ) = 2;
	ZoneEquipConfig( ZoneNum ).ReturnNodeAirLoopNum( 3 ) = 1;
	int inletNode1 = ZoneEquipConfig( ZoneNum ).InletNode( 1 );
	int inletNode2 = ZoneEquipConfig( ZoneNum ).InletNode( 2 );
	int inletNode3 = ZoneEquipConfig( ZoneNum ).InletNode( 3 );
	ZoneEquipConfig( ZoneNum ).ReturnNodeInletNum( 1 ) = 2; // Intentionally in a different order
	ZoneEquipConfig( ZoneNum ).ReturnNodeInletNum( 2 ) = 1;
	ZoneEquipConfig( ZoneNum ).ReturnNodeInletNum( 3 ) = 3;
	int returnNode1 = ZoneEquipConfig( ZoneNum ).ReturnNode( 1 );
	int returnNode2 = ZoneEquipConfig( ZoneNum ).ReturnNode( 2 );
	int returnNode3 = ZoneEquipConfig( ZoneNum ).ReturnNode( 3 );

	DataHVACGlobals::NumPrimaryAirSys = 3;
	DataAirSystems::PrimaryAirSystem.allocate( 3 );
	DataAirLoop::AirLoopFlow.allocate( 3 );

	DataAirSystems::PrimaryAirSystem( 1 ).OASysExists = false;
	DataAirLoop::AirLoopFlow( 1 ).DesReturnFrac = 1.0;
	DataAirSystems::PrimaryAirSystem( 2 ).OASysExists = false;
	DataAirLoop::AirLoopFlow( 2 ).DesReturnFrac = 1.0;
	DataAirSystems::PrimaryAirSystem( 3 ).OASysExists = false;
	DataAirLoop::AirLoopFlow( 3 ).DesReturnFrac = 1.0;
	DataGlobals::DoingSizing = false;
	DataGlobals::isPulseZoneSizing = false;

	// Case 1 - send zero, expect zero back
	Node( inletNode1 ).MassFlowRate = 0.0;
	Node( inletNode2 ).MassFlowRate = 0.0;
	Node( inletNode3 ).MassFlowRate = 0.0;
	Node( returnNode1 ).MassFlowRate = 0.12; // Set to random values to make sure they get reset properly
	Node( returnNode2 ).MassFlowRate = 0.32;
	Node( returnNode3 ).MassFlowRate = 0.45;

	Real64 StdTotalReturnMassFlow = 0.0;
	Real64 FinalTotalReturnMassFlow = 0.0;

	CalcZoneReturnFlows( ZoneNum, StdTotalReturnMassFlow, FinalTotalReturnMassFlow );
	EXPECT_EQ( FinalTotalReturnMassFlow, 0.0 );
	EXPECT_EQ( Node( returnNode1 ).MassFlowRate, 0.0 );
	EXPECT_EQ( Node( returnNode2 ).MassFlowRate, 0.0 );
	EXPECT_EQ( Node( returnNode3 ).MassFlowRate, 0.0 );


	// Case 2 - send zero, expect sum of inlet flow back
	StdTotalReturnMassFlow = 0.0;
	FinalTotalReturnMassFlow = 0.0;

	Node( inletNode2 ).MassFlowRate = 2.0;
	Node( inletNode1 ).MassFlowRate = 1.0;
	Node( inletNode3 ).MassFlowRate = 3.0;
	Node( returnNode1 ).MassFlowRate = 0.12; // Set to random values to make sure they get reset properly
	Node( returnNode2 ).MassFlowRate = 0.32;
	Node( returnNode3 ).MassFlowRate = 0.45;

	StdTotalReturnMassFlow = 0.0;
	FinalTotalReturnMassFlow = 0.0;

	CalcZoneReturnFlows( ZoneNum, StdTotalReturnMassFlow, FinalTotalReturnMassFlow );
	EXPECT_EQ( FinalTotalReturnMassFlow, 6.0 );
	EXPECT_EQ( Node( returnNode1 ).MassFlowRate, 2.0 );
	EXPECT_EQ( Node( returnNode2 ).MassFlowRate, 1.0 );
	EXPECT_EQ( Node( returnNode3 ).MassFlowRate, 3.0 );


	// Deallocate everything - should all be taken care of in clear_states
}
