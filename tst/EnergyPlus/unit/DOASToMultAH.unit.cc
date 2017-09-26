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

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <General.hh>
#include <DataZoneEquipment.hh>
#include <MixedAir.hh>
#include <SplitterComponent.hh>
#include <MixerComponent.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus::DataGlobals;
using namespace EnergyPlus::DataZoneEquipment;
using namespace EnergyPlus::MixedAir;
using namespace EnergyPlus::SplitterComponent;
using namespace EnergyPlus::MixerComponent;

namespace EnergyPlus {
	TEST_F( EnergyPlusFixture, DOASToMultAH1 ) {
	
			std::string const idf_objects = delimited_string( {
				"Version,8.5;",
				" AirLoopHVAC:SupplyPath,",
				" AHU Supply Air Path 1, !- Name",
				" DOAS Outlet Node, !- Supply Air Path Inlet Node Name",
				" AirLoopHVAC:ZoneSplitter, !- Component 1 Object Type",
				" DOAS Supply Air Splitter 1; !- Component 1 Name",
				" AirLoopHVAC:ReturnPath,",
				" AHU Return Air Path 1, !- Name",
				" DOAS Return Node, !- Return Air Path Outlet Node Name",
				" AirLoopHVAC:ZoneMixer, !- Component 1 Object Type",
				" DOAS Return Air Mixer 1; !- Component 1 Name",
				" AirLoopHVAC:ZoneSplitter,",
				" DOAS Supply Air Splitter 1, !- Name",
				" DOAS Outlet Node, !- Inlet Node Name",
				" AHU 1 Vent Air In Node, !- Outlet 1 Node Name",
				" AHU 2 Vent Air In Node, !- Outlet 2 Node Name",
				" AHU 3 Vent Air In Node; !- Outlet 3 Node Name",
				" AirLoopHVAC:ZoneMixer,",
				" DOAS Return Air Mixer 1, !- Name",
				" DOAS Return Node, !- Outlet Node Name",
				" AHU 1 Relief Air Node, !- Inlet 1 Node Name",
				" AHU 2 Relief Air Node, !- Inlet 2 Node Name",
				" AHU 3 Relief Air Node; !- Inlet 3 Node Name",
				" AirLoopHVAC:OutdoorAirSystem,",
				" DOAS 1, !- Name",
				" DOAS 1 Controllers, !- Controller List Name",
				" DOAS 1 Equipment, !- Outdoor Air Equipment List Name",
				" DOAS 1 Avail List, !- Availability Manager List Name",
				" AHU Supply Air Path 1, !- Supply Air Path Name",
				" AHU Return Air Path 1; !- Return Air Path Name",
				" AirLoopHVAC:ControllerList,",
				" DOAS 1 Controllers, !- Name",
				" Controller:OutdoorAir, !- Controller 1 Object Type",
				" DOAS 1 OA Controller, !- Controller 1 Name",
				" Controller:WaterCoil, !- Controller 2 Object Type",
				" DOAS 1 Cooling Coil Controller;  !- Controller 2 Name",
				" AirLoopHVAC:OutdoorAirSystem:EquipmentList,",
				" DOAS 1 Equipment, !- Name",
				" HeatExchanger:AirToAir : SensibleAndLatent, !- Component 1 Object Type",
				" OA 1 Heat Recovery, !- Component 1 Name",
				" Coil:Heating:Water, !- Component 2 Object Type",
				" DOAS 1 Heating Coil, !- Component 2 Name",
				" Coil:Cooling:Water, !- Component 3 Object Type",
				" DOAS 1 Cooling Coil, !- Component 3 Name",
				" Fan:VariableVolume, !- Component 4 Object Type",
				" DOAS 1 Supply Fan, !- Component 4 Name",
				" Fan:VariableVolume, !- Component 5 Object Type",
				" DOAS 1 Return Fan; !- Component 5 Name",
			
		} );

		ASSERT_FALSE( process_idf( idf_objects ) );
		
		GetSplitterInput();
		GetMixerInput();
		GetSupplyAirPath();
		GetReturnAirPath();
		EXPECT_EQ( SupplyAirPath( 1 ).NumOfComponents, 1 );
		EXPECT_EQ( SupplyAirPath( 1 ).ComponentType_Num( 1 ), ZoneSplitter_Type);
		EXPECT_EQ( SupplyAirPath( 1 ).Name, "AHU SUPPLY AIR PATH 1" );
		EXPECT_EQ( SupplyAirPath( 1 ).ComponentName( 1 ), "DOAS SUPPLY AIR SPLITTER 1" );
		EXPECT_GT( SupplyAirPath( 1 ).InletNodeNum, 0 );

		EXPECT_EQ( ReturnAirPath( 1 ).NumOfComponents, 1 );
		EXPECT_EQ( ReturnAirPath( 1 ).ComponentType_Num( 1 ), ZoneMixer_Type);
		EXPECT_EQ( ReturnAirPath( 1 ).Name, "AHU RETURN AIR PATH 1" );
		EXPECT_EQ( ReturnAirPath( 1 ).ComponentName( 1 ), "DOAS RETURN AIR MIXER 1" );
		EXPECT_GT( ReturnAirPath( 1 ).OutletNodeNum, 0 );

		GetOASysInputFlag = true;
		GetOutsideAirSysInputs();

	}
	
}