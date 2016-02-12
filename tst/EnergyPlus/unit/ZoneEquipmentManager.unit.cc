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

	ASSERT_FALSE(process_idf(idf_objects));
	EXPECT_FALSE(has_err_output());
	bool ErrorsFound = false;
	GetZoneData(ErrorsFound);
	AllocateHeatBalArrays();
	GetZoneEquipmentData1();
	GetSimpleAirModelInputs(ErrorsFound);

	int ZoneNum = 1;
	int NodeNum;
	for (NodeNum = 1; NodeNum <= ZoneEquipConfig(ZoneNum).NumInletNodes; ++NodeNum) {
		Node(ZoneEquipConfig(ZoneNum).InletNode(NodeNum)).MassFlowRate = 1.0;
	}
	// Test here - if zone equipment exhausts slightly more than it supplies, there should be no unbalanced exhaust flow warning
	Node(ZoneEquipConfig(ZoneNum).ExhaustNode(1)).MassFlowRate = 1.000000001;
	DataAirSystems::PrimaryAirSystem.allocate( 1 );
	DataAirLoop::AirLoopFlow.allocate( 1 );
	DataHVACGlobals::NumPrimaryAirSys = 1;

	ZoneEquipConfig(ZoneNum).AirLoopNum = 1;
	DataHVACGlobals::AirLoopsSimOnce = true;
	CalcZoneMassBalance( );
	EXPECT_FALSE(has_err_output());

	// Add true zone exhuast from exhuast fan, now there should be warning
	ZoneEquipConfig(ZoneNum).ZoneExh = 0.1;
	CalcZoneMassBalance();
	EXPECT_TRUE(has_err_output());

	// Deallocate everything - should all be taken care of in clear_states
}
