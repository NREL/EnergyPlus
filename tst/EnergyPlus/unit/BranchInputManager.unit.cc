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

// EnergyPlus::BranchNodeConnections Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

#include "Fixtures/EnergyPlusFixture.hh"

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
// EnergyPlus Headers
#include <EnergyPlus/BranchInputManager.hh>
#include <EnergyPlus/DataSizing.hh>

using namespace EnergyPlus;
using namespace EnergyPlus::BranchInputManager;

namespace EnergyPlus {

	TEST_F( EnergyPlusFixture, BranchInputManager_FindAirLoopBranchConnection)
	{

		std::string const idf_objects = delimited_string( {
			" Version,8.5;",

			"AirLoopHVAC,",
			"  DOAS,                    !- Name",
			"  ,                        !- Controller List Name",
			"  DOAS Availability Managers,  !- Availability Manager List Name",
			"  autosize,                !- Design Supply Air Flow Rate {m3/s}",
			"  DOAS Branches,           !- Branch List Name",
			"  ,                        !- Connector List Name",
			"  DOAS Air Loop Inlet,     !- Supply Side Inlet Node Name",
			"  DOAS Return Air Outlet,  !- Demand Side Outlet Node Name",
			"  DOAS Supply Path Inlet,  !- Demand Side Inlet Node Names",
			"  DOAS Supply Fan Outlet;  !- Supply Side Outlet Node Names",

			"AirLoopHVAC,",
			"  Air Loop 1,                    !- Name",
			"  ,                        !- Controller List Name",
			"  Air Loop 1 Availability Managers,  !- Availability Manager List Name",
			"  50.0,                !- Design Supply Air Flow Rate {m3/s}",
			"  Air Loop 1 Branches,           !- Branch List Name",
			"  ,                        !- Connector List Name",
			"  Air Loop 1 Air Loop Inlet,     !- Supply Side Inlet Node Name",
			"  Air Loop 1 Return Air Outlet,  !- Demand Side Outlet Node Name",
			"  Air Loop 1 Supply Path Inlet,  !- Demand Side Inlet Node Names",
			"  Air Loop 1 Supply Fan Outlet;  !- Supply Side Outlet Node Names",

		} );

		ASSERT_FALSE( process_idf( idf_objects ) );
		std::string BranchListName;
		std::string FoundLoopName;
		int FoundLoopNum;
		std::string LoopType;
		Real64 FoundLoopVolFlowRate;
		bool MatchedLoop;

		// Case 1 Find Air Loop 1 Branches
		// Note the strings need to be uppercase at this point
		BranchListName = "AIR LOOP 1 BRANCHES";
		FoundLoopName = "None";
		FoundLoopNum = 0;
		LoopType = "None";
		FoundLoopVolFlowRate = 0.0;
		MatchedLoop = false;


		FindAirLoopBranchConnection( BranchListName, FoundLoopName, FoundLoopNum, LoopType, FoundLoopVolFlowRate, MatchedLoop );

		EXPECT_EQ( "AIR LOOP 1", FoundLoopName );
		EXPECT_EQ( 2 , FoundLoopNum );
		EXPECT_EQ( "Air", LoopType );
		EXPECT_EQ( 50.0, FoundLoopVolFlowRate );
		EXPECT_TRUE( MatchedLoop );

		// Case 2 Find DOAS Branches
		BranchListName = "DOAS BRANCHES";
		FoundLoopName = "None";
		FoundLoopNum = 0;
		LoopType = "None";
		FoundLoopVolFlowRate = 0.0;
		MatchedLoop = false;


		FindAirLoopBranchConnection( BranchListName, FoundLoopName, FoundLoopNum, LoopType, FoundLoopVolFlowRate, MatchedLoop );

		EXPECT_EQ( "DOAS", FoundLoopName );
		EXPECT_EQ( 1 , FoundLoopNum );
		EXPECT_EQ( "Air", LoopType );
		EXPECT_EQ( DataSizing::AutoSize, FoundLoopVolFlowRate );
		EXPECT_TRUE( MatchedLoop );

		// Case 3 Not found
		BranchListName = "Not There";
		FoundLoopName = "None";
		FoundLoopNum = 0;
		LoopType = "None";
		FoundLoopVolFlowRate = 0.0;
		MatchedLoop = false;


		FindAirLoopBranchConnection( BranchListName, FoundLoopName, FoundLoopNum, LoopType, FoundLoopVolFlowRate, MatchedLoop );

		EXPECT_EQ( "None", FoundLoopName );
		EXPECT_EQ( 0 , FoundLoopNum );
		EXPECT_EQ( "None", LoopType );
		EXPECT_EQ( 0.0, FoundLoopVolFlowRate );
		EXPECT_FALSE( MatchedLoop );

	}

	TEST_F( EnergyPlusFixture, BranchInputManager_GetAirBranchIndex)
	{

		std::string const idf_objects = delimited_string( {
			" Version,8.5;",

			"Branch,",
			"  DOAS Main Branch,        !- Name",
			"  autosize,                !- Maximum Flow Rate {m3/s}",
			"  ,                        !- Pressure Drop Curve Name",
			"  AirLoopHVAC:OutdoorAirSystem,  !- Component 1 Object Type",
			"  DOAS OA System,          !- Component 1 Name",
			"  DOAS Air Loop Inlet,     !- Component 1 Inlet Node Name",
			"  DOAS Mixed Air Outlet,   !- Component 1 Outlet Node Name",
			"  Passive,                 !- Component 1 Branch Control Type",
			"  CoilSystem:Cooling:DX,   !- Component 2 Object Type",
			"  DOAS Cooling Coil,       !- Component 2 Name",
			"  DOAS Mixed Air Outlet,   !- Component 2 Inlet Node Name",
			"  DOAS Cooling Coil Outlet,!- Component 2 Outlet Node Name",
			"  Passive,                 !- Component 2 Branch Control Type",
			"  Coil:Heating:Gas,        !- Component 2 Object Type",
			"  DOAS Heating Coil,       !- Component 2 Name",
			"  DOAS Cooling Coil Outlet,  !- Component 2 Inlet Node Name",
			"  DOAS Heating Coil Outlet,!- Component 2 Outlet Node Name",
			"  Passive,                 !- Component 2 Branch Control Type",
			"  Fan:VariableVolume,      !- Component 3 Object Type",
			"  DOAS Supply Fan,         !- Component 3 Name",
			"  DOAS Heating Coil Outlet,!- Component 3 Inlet Node Name",
			"  DOAS Supply Fan Outlet,  !- Component 3 Outlet Node Name",
			"  Active;                  !- Component 3 Branch Control Type",

			"  Branch,",
			"    TowerWaterSys Demand Bypass Branch,  !- Name",
			"    ,                        !- Maximum Flow Rate {m3/s}",
			"    ,                        !- Pressure Drop Curve Name",
			"    Pipe:Adiabatic,          !- Component 1 Object Type",
			"    TowerWaterSys Demand Bypass Pipe,  !- Component 1 Name",
			"    TowerWaterSys Demand Bypass Pipe Inlet Node,  !- Component 1 Inlet Node Name",
			"    TowerWaterSys Demand Bypass Pipe Outlet Node,  !- Component 1 Outlet Node Name",
			"    Bypass;                  !- Component 1 Branch Control Type",

		} );

		ASSERT_FALSE( process_idf( idf_objects ) );
		std::string CompType;
		std::string CompName;
		int BranchIndex;

		// Case 1 Find OA System on DOAS branch
		// Note the strings need to be uppercase at this point
		CompType = "AIRLOOPHVAC:OUTDOORAIRSYSTEM";
		CompName = "DOAS OA SYSTEM";
		BranchIndex = 0;

		BranchIndex = GetAirBranchIndex( CompType, CompName );

		EXPECT_EQ( 1 , BranchIndex );

		// Case 3 Find pipe
		CompType = "PIPE:ADIABATIC";
		CompName = "TOWERWATERSYS DEMAND BYPASS PIPE";
		BranchIndex = 0;

		BranchIndex = GetAirBranchIndex( CompType, CompName );

		EXPECT_EQ( 2, BranchIndex );

		// Case 4 Not found
		CompType = "PIPE:ADIABATIC";
		CompName = "TOWERWATERSYS DEMAND BYPASS PIPE NOT THERE";
		BranchIndex = 0;

		BranchIndex = GetAirBranchIndex( CompType, CompName );

		EXPECT_EQ( 0, BranchIndex );

	}
}
