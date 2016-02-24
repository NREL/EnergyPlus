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

// EnergyPlus::AirTerminal SingleDuct Series PIU Reheat Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// ObjexxFCL Headers

#include "Fixtures/EnergyPlusFixture.hh"


#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/PoweredInductionUnits.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/ZoneAirLoopEquipmentManager.hh>

// EnergyPlus Headers
using namespace EnergyPlus::DataGlobals;
using namespace EnergyPlus::HeatBalanceManager;
using namespace EnergyPlus::PoweredInductionUnits;
using namespace EnergyPlus::ScheduleManager;
using namespace EnergyPlus::DataZoneEquipment;
using namespace EnergyPlus::ZoneAirLoopEquipmentManager;

namespace EnergyPlus {


	TEST_F( EnergyPlusFixture, AirTerminalSingleDuctSeriesPIUReheat_GetInputtest ) {

		bool ErrorsFound( false );

		std::string const idf_objects = delimited_string({
			"Version,8.4;",
			"  AirTerminal:SingleDuct:SeriesPIU:Reheat,",
			"    SPACE1-1 VAV Reheat,     !- Name",
			"    ReheatCoilAvailSched,    !- Availability Schedule Name",
			"    autosize,                !- Maximum Air Flow Rate {m3/s}",
			"    autosize,                !- Maximum Primary Air Flow Rate {m3/s}",
			"    autosize,                !- Minimum Primary Air Flow Fraction",
			"    SPACE1-1 ATU In Node,    !- Supply Air Inlet Node Name",
			"    SPACE1-1 ATU Sec Node,   !- Secondary Air Inlet Node Name",
			"    SPACE1-1 In Node,        !- Outlet Node Name",
			"    SPACE1-1 Zone Coil Air In Node,  !- Reheat Coil Air Inlet Node Name",
			"    SPACE1-1 PIU Mixer,      !- Zone Mixer Name",
			"    SPACE1-1 PIU Fan,        !- Fan Name",
			"    Coil:Heating:Water,      !- Reheat Coil Object Type",
			"    SPACE1-1 Zone Coil,      !- Reheat Coil Name",
			"    autosize,                !- Maximum Hot Water or Steam Flow Rate {m3/s}",
			"    0.0,                     !- Minimum Hot Water or Steam Flow Rate {m3/s}",
			"    0.0001;                  !- Convergence Tolerance",

			"  Coil:Heating:Water,",
			"    SPACE1-1 Zone Coil,      !- Name",
			"    ReheatCoilAvailSched,    !- Availability Schedule Name",
			"    autosize,                !- U-Factor Times Area Value {W/K}",
			"    autosize,                !- Maximum Water Flow Rate {m3/s}",
			"    SPACE1-1 Zone Coil Water In Node,  !- Water Inlet Node Name",
			"    SPACE1-1 Zone Coil Water Out Node, !- Water Outlet Node Name",
			"    SPACE1-1 Zone Coil Air In Node,    !- Air Inlet Node Name",
			"    SPACE1-1 In Node,        !- Air Outlet Node Name",
			"    UFactorTimesAreaAndDesignWaterFlowRate,  !- Performance Input Method",
			"    autosize,                !- Rated Capacity {W}",
			"    82.2,                    !- Rated Inlet Water Temperature {C}",
			"    16.6,                    !- Rated Inlet Air Temperature {C}",
			"    71.1,                    !- Rated Outlet Water Temperature {C}",
			"    32.2,                    !- Rated Outlet Air Temperature {C}",
			"    ;                        !- Rated Ratio for Air and Water Convection",

			"  Schedule:Compact,",
			"    ReheatCoilAvailSched,    !- Name",
			"    Fraction,                !- Schedule Type Limits Name",
			"    Through: 12/31,           !- Field 1",
			"    For: AllDays,            !- Field 2",
			"    Until: 24:00,1.0;        !- Field 3",

			"  ZoneHVAC:EquipmentList,",
			"    SPACE1-1 Eq,             !- Name",
			"    ZoneHVAC:AirDistributionUnit,  !- Zone Equipment 1 Object Type",
			"    SPACE1-1 ATU,            !- Zone Equipment 1 Name",
			"    1,                       !- Zone Equipment 1 Cooling Sequence",
			"    1;                       !- Zone Equipment 1 Heating or No-Load Sequence",

			"  ZoneHVAC:AirDistributionUnit,",
			"    SPACE1-1 ATU,            !- Name",
			"    SPACE1-1 In Node,        !- Air Distribution Unit Outlet Node Name",
			"    AirTerminal:SingleDuct:SeriesPIU:Reheat,  !- Air Terminal Object Type",
			"    SPACE1-1 VAV Reheat;     !- Air Terminal Name",

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

			"  OutdoorAir:NodeList,",
			"    OutsideAirInletNodes;    !- Node or NodeList Name 1",

			"  NodeList,",
			"    OutsideAirInletNodes,    !- Name",
			"    Outside Air Inlet Node 1;!- Node 1 Name",

			"  ZoneHVAC:EquipmentConnections,",
			"    SPACE1-1,                !- Zone Name",
			"    SPACE1-1 Eq,             !- Zone Conditioning Equipment List Name",
			"    SPACE1-1 In Node,        !- Zone Air Inlet Node or NodeList Name",
			"    ,                        !- Zone Air Exhaust Node or NodeList Name",
			"    SPACE1-1 Node,           !- Zone Air Node Name",
			"    SPACE1-1 Out Node;       !- Zone Return Air Node Name",

		});

		ASSERT_FALSE( process_idf( idf_objects ) );

		NumOfTimeStepInHour = 1; // must initialize this to get schedules initialized
		MinutesPerTimeStep = 60; // must initialize this to get schedules initialized
		ProcessScheduleInput(); // read schedules

		GetZoneData( ErrorsFound );
		ASSERT_FALSE( ErrorsFound );

		GetZoneEquipmentData1();
		GetZoneAirLoopEquipment();

		GetPIUs();

		ASSERT_EQ( 1, NumSeriesPIUs );
		EXPECT_EQ( "SPACE1-1 ZONE COIL", PIU( 1 ).HCoil ); // heating coil name
		EXPECT_EQ( "COIL:HEATING:WATER", PIU( 1 ).HCoilType ); // hot water heating coil
		EXPECT_GT( PIU( 1 ).HotControlNode, 0 );  // none zero integer node index is expected

	}
}
