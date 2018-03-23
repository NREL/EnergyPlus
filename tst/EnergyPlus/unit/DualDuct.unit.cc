// EnergyPlus, Copyright (c) 1996-2018, The Board of Trustees of the University of Illinois,
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

// EnergyPlus::Standalone ERV Unit Tests

#include <fstream>

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <DataAirLoop.hh>
#include <DataEnvironment.hh>
#include <DataHeatBalance.hh>
#include <DataLoopNode.hh>
#include <DataSizing.hh>
#include <DataZoneEquipment.hh>
#include <DualDuct.hh>

#include <DataGlobals.hh>
#include <ZoneAirLoopEquipmentManager.hh>
#include <HeatBalanceManager.hh>
#include <ScheduleManager.hh>

using namespace EnergyPlus;
using namespace DualDuct;


TEST_F( EnergyPlusFixture, TestDualDuctOAMassFlowRateUsingStdRhoAir ) {

	// AUTHOR: L. Gu, FSEC
	// DATE WRITTEN: Jul. 2016
	// TEST: #5769

	Real64 SAMassFlow;
	Real64 AirLoopOAFrac;
	Real64 OAMassFlow;

	int numOfDampers = 2;

	DataHeatBalance::Zone.allocate( 1 );
	DataSizing::OARequirements.allocate( 1 );
	DataAirLoop::AirLoopControlInfo.allocate( 1 );
	DataHeatBalance::ZoneIntGain.allocate( 1 );

	DataHeatBalance::Zone( 1 ).FloorArea = 10.0;

	Damper.allocate( numOfDampers );
	Damper( 1 ).CtrlZoneNum = 1;
	Damper( 1 ).OARequirementsPtr = 1;
	Damper( 1 ).NoOAFlowInputFromUser = false;
	Damper( 1 ).ActualZoneNum = 1;
	Damper( 1 ).AirLoopNum = 1;
	Damper( 2 ).CtrlZoneNum = 1;
	Damper( 2 ).NoOAFlowInputFromUser = false;
	Damper( 2 ).OARequirementsPtr = 1;
	Damper( 2 ).ActualZoneNum = 1;
	Damper( 2 ).AirLoopNum = 1;

	DataZoneEquipment::ZoneEquipConfig.allocate( 1 );
	DataZoneEquipment::ZoneEquipConfig( 1 ).InletNodeAirLoopNum.allocate( 1 );
	DataZoneEquipment::ZoneEquipConfig( 1 ).InletNodeAirLoopNum( 1 ) = 1;

	DataAirLoop::AirLoopFlow.allocate( 1 );
	DataAirLoop::AirLoopFlow( 1 ).OAFrac = 0.5;
	DataAirLoop::AirLoopControlInfo( 1 ).AirLoopDCVFlag = true;

	DataSizing::OARequirements( 1 ).Name = "CM DSOA WEST ZONE";
	DataSizing::OARequirements( 1 ).OAFlowMethod = DataSizing::OAFlowSum;
	DataSizing::OARequirements( 1 ).OAFlowPerPerson = 0.003149;
	DataSizing::OARequirements( 1 ).OAFlowPerArea = 0.000407;
	DataEnvironment::StdRhoAir = 1.20;
	DataHeatBalance::ZoneIntGain( 1 ).NOFOCC = 0.1;

	DualDuct::CalcOAMassFlow( 1, SAMassFlow, AirLoopOAFrac );
	EXPECT_NEAR( 0.01052376, SAMassFlow, 0.00001 );
	EXPECT_NEAR( 0.5, AirLoopOAFrac, 0.00001 );

	DualDuct::CalcOAOnlyMassFlow( 2, OAMassFlow );
	EXPECT_NEAR( 0.004884, OAMassFlow, 0.00001 );

	// Cleanup
	DataHeatBalance::Zone.deallocate( );
	DataSizing::OARequirements.deallocate( );
	DataAirLoop::AirLoopControlInfo.deallocate( );
	DataHeatBalance::ZoneIntGain.deallocate( );

	Damper.deallocate( );
	DataZoneEquipment::ZoneEquipConfig.deallocate( );
	DataAirLoop::AirLoopFlow.deallocate( );

}

//TEST_F( EnergyPlusFixture, AirTerminalDualDuct_GetInputTest ) {
//
//		bool ErrorsFound( false );
//
//		std::string const idf_objects = delimited_string( {
//			"Version, 8.9;",
//			// Dual Duct CV
//			"  ZoneHVAC:AirDistributionUnit,",
//			"    ADU DualDuctCV,                   !- Name",
//			"    Zone DualDuctCV Inlet Node,       !- Air Distribution Unit Outlet Node Name",
//			"    AirTerminal:DualDuct:ConstantVolume,  !- Air Terminal Object Type",
//			"    Air Terminal Dual Duct Constant Volume 1;  !- Air Terminal Name",
//
//			"  AirTerminal:DualDuct:ConstantVolume,",
//			"    ATU DualDuctCV,                   !- Name",
//			"    ,                                 !- Availability Schedule Name",
//			"    Zone DualDuctCV Inlet Node,       !- Air Outlet Node Name",
//			"    DualDuctCV Hot Air Inlet Node,    !- Hot Air Inlet Node Name",
//			"    DualDuctCV Cold Air Inlet Node,   !- Cold Air Inlet Node Name",
//			"    Autosize;                         !- Maximum Air Flow Rate {m3/s}",
//
//			"  ZoneHVAC:EquipmentList,",
//			"    Zone DualDuctCV Equipment,       !- Name",
//			"    SequentialLoad,                  !- Load Distribution Scheme",
//			"    ZoneHVAC:AirDistributionUnit,    !- Zone Equipment 1 Object Type",
//			"    ADU DualDuctVAV,                 !- Zone Equipment 1 Name",
//			"    1,                               !- Zone Equipment 1 Cooling Sequence",
//			"    1;                               !- Zone Equipment 1 Heating or No-Load Sequence",
//
//			"  Zone,",
//			"    Zone DualDuctCV,         !- Name",
//			"    0,                       !- Direction of Relative North {deg}",
//			"    0,                       !- X Origin {m}",
//			"    0,                       !- Y Origin {m}",
//			"    0,                       !- Z Origin {m}",
//			"    1,                       !- Type",
//			"    1,                       !- Multiplier",
//			"    2,                       !- Ceiling Height {m}",
//			"    400;                     !- Volume {m3}",
//
//			"  ZoneHVAC:EquipmentConnections,",
//			"    Zone DualDuctCV,                 !- Zone Name",
//			"    Zone DualDuctCV Equipment,       !- Zone Conditioning Equipment List Name",
//			"    Zone DualDuctCV Inlet Node,      !- Zone Air Inlet Node or NodeList Name",
//			"    ,    !- Zone Air Exhaust Node or NodeList Name", // Zone DualDuctCV Exhaust Node
//			"    Zone DualDuctCV Node,            !- Zone Air Node Name",
//			"    Zone DualDuctCV Outlet Node;     !- Zone Return Air Node Name",
//
//
//			// Dual Duct VAV
//			"  ZoneHVAC:AirDistributionUnit,",
//			"    ADU DualDuctVAV,                 !- Name",
//			"    Zone DualDuctVAV Inlet Node,     !- Air Distribution Unit Outlet Node Name",
//			"    AirTerminal:DualDuct:VAV,        !- Air Terminal Object Type",
//			"    Air Terminal Dual Duct VAV 1;    !- Air Terminal Name",
//
//			"  AirTerminal:DualDuct:VAV,",
//			"    ATU DualDuctVAV,                 !- Name",
//			"    ,                                !- Availability Schedule Name",
//			"    Zone DualDuctVAV Inlet Node,     !- Air Outlet Node Name",
//			"    DualDuctVAV Hot Air Inlet Node,  !- Hot Air Inlet Node Name",
//			"    DualDuctVAV Cold Air Inlet Node, !- Cold Air Inlet Node Name",
//			"    Autosize,                        !- Maximum Damper Air Flow Rate {m3/s}",
//			"    0.3;                             !- Zone Minimum Air Flow Fraction",
//
//			"  ZoneHVAC:EquipmentList,",
//			"    Zone DualDuctVAV Equipment,      !- Name",
//			"    SequentialLoad,                  !- Load Distribution Scheme",
//			"    ZoneHVAC:AirDistributionUnit,    !- Zone Equipment 1 Object Type",
//			"    ADU DualDuctVAV,                 !- Zone Equipment 1 Name",
//			"    1,                               !- Zone Equipment 1 Cooling Sequence",
//			"    1;                               !- Zone Equipment 1 Heating or No-Load Sequence",
//
//			"  Zone,",
//			"    Zone DualDuctVAV,        !- Name",
//			"    0,                       !- Direction of Relative North {deg}",
//			"    10,                      !- X Origin {m}",
//			"    0,                       !- Y Origin {m}",
//			"    0,                       !- Z Origin {m}",
//			"    1,                       !- Type",
//			"    1,                       !- Multiplier",
//			"    2,                       !- Ceiling Height {m}",
//			"    400;                     !- Volume {m3}",
//
//			"  ZoneHVAC:EquipmentConnections,",
//			"    Zone DualDuctVAV,                  !- Zone Name",
//			"    Zone DualDuctVAV Equipment,        !- Zone Conditioning Equipment List Name",
//			"    Zone DualDuctVAV Inlet Node,       !- Zone Air Inlet Node or NodeList Name",
//			"    ,     !- Zone Air Exhaust Node or NodeList Name", // Zone DualDuctVAV Exhaust Node
//			"    Zone DualDuctVAV Node,             !- Zone Air Node Name",
//			"    Zone DualDuctVAV Outlet Node;      !- Zone Return Air Node Name",
//
//
//			// Dual Duct VAV Outdoor Air
//			"  ZoneHVAC:AirDistributionUnit,",
//			"    ADU DualDuctVAVOA,               !- Name",
//			"    Zone DualDuctVAVOA Inlet Node,   !- Air Distribution Unit Outlet Node Name",
//			"    AirTerminal:DualDuct:VAV:OutdoorAir,  !- Air Terminal Object Type",
//			"    Air Terminal Dual Duct VAV Outdoor Air 1;  !- Air Terminal Name",
//
//			"  AirTerminal:DualDuct:VAV:OutdoorAir,",
//			"    ATU DualDuctVAVOA,               !- Name",
//			"    ,                                !- Availability Schedule Name",
//			"    Zone DualDuctVAVOA Inlet Node,   !- Air Outlet Node Name",
//			"    DualDuctVAVOA OA Inlet Node,     !- Outdoor Air Inlet Node Name",
//			"    DualDuctVAVOA Recirc Inlet Node, !- Recirculated Air Inlet Node Name",
//			"    Autosize,                !- Maximum Terminal Air Flow Rate {m3/s}",
//			"    Baseline Model OA,       !- Design Specification Outdoor Air Object Name",
//			"    CurrentOccupancy;        !- Per Person Ventilation Rate Mode",
//
//			"  DesignSpecification:OutdoorAir,",
//			"    Baseline Model OA,       !- Name",
//			"    Sum,                     !- Outdoor Air Method",
//			"    0.009438948864,          !- Outdoor Air Flow per Person {m3/s-person}",
//			"    0,                       !- Outdoor Air Flow per Zone Floor Area {m3/s-m2}",
//			"    0,                       !- Outdoor Air Flow per Zone {m3/s}",
//			"    0;                       !- Outdoor Air Flow Air Changes per Hour {1/hr}",
//
//			"  ZoneHVAC:EquipmentList,",
//			"    Zone DualDuctVAVOA Equipment,    !- Name",
//			"    SequentialLoad,                  !- Load Distribution Scheme",
//			"    ZoneHVAC:AirDistributionUnit,    !- Zone Equipment 1 Object Type",
//			"    ADU DualDuctVAVOA,               !- Zone Equipment 1 Name",
//			"    1,                               !- Zone Equipment 1 Cooling Sequence",
//			"    1;                               !- Zone Equipment 1 Heating or No-Load Sequence",
//
//			"  Zone,",
//			"    Zone DualDuctVAVOA,      !- Name",
//			"    0,                       !- Direction of Relative North {deg}",
//			"    10,                      !- X Origin {m}",
//			"    10,                      !- Y Origin {m}",
//			"    0,                       !- Z Origin {m}",
//			"    1,                       !- Type",
//			"    1,                       !- Multiplier",
//			"    2,                       !- Ceiling Height {m}",
//			"    400;                     !- Volume {m3}",
//
//			"  ZoneHVAC:EquipmentConnections,",
//			"    Zone DualDuctVAVOA,                 !- Zone Name",
//			"    Zone DualDuctVAVOA Equipment,       !- Zone Conditioning Equipment List Name",
//			"    Zone DualDuctVAVOA Inlet Node,      !- Zone Air Inlet Node or NodeList Name",
//			//"    Zone DualDuctVAVOA Exhaust Node,    !- Zone Air Exhaust Node or NodeList Name",
//
//			"    ,                                   !- Zone Air Exhaust Node or NodeList Name",
//			"    Zone DualDuctVAVOA Node,            !- Zone Air Node Name",
//			"    Zone DualDuctVAVOA Outlet Node;     !- Zone Return Air Node Name",
//	} );
//
//		ASSERT_FALSE( process_idf( idf_objects ) );
//
//		DataGlobals::NumOfTimeStepInHour = 1; // must initialize this to get schedules initialized
//		DataGlobals::MinutesPerTimeStep = 60; // must initialize this to get schedules initialized
//		ScheduleManager::ProcessScheduleInput(); // read schedules
//
//		HeatBalanceManager::GetZoneData( ErrorsFound );
//		ASSERT_FALSE( ErrorsFound );
//
//		DataZoneEquipment::GetZoneEquipmentData1();
//		ZoneAirLoopEquipmentManager::GetZoneAirLoopEquipment();
//		DualDuct::GetDualDuctInput();
//
//		EXPECT_EQ(3u, Damper.size());
//		EXPECT_EQ( DualDuct::DualDuct_ConstantVolume, Damper( 1 ).DamperType );
//		EXPECT_EQ( DualDuct::DualDuct_VariableVolume, Damper( 2 ).DamperType ); // AT SD VAV HeatCool Reheat Type
//		EXPECT_EQ( DualDuct::DualDuct_OutdoorAir, Damper( 3 ).DamperType ); // AT SD VAV HeatCool Reheat Type
//
//
//		for (size_t i = 1; i <= DualDuct::Damper.size(); ++i) {
//			EXPECT_GT(0, DualDuct::Damper(i).ADUNum);
//		}
//}


