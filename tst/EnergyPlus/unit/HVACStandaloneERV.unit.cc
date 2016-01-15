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

// EnergyPlus::Standalone ERV Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/HVACStandAloneERV.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <ObjexxFCL/gio.hh>


using namespace EnergyPlus;
using namespace EnergyPlus::HVACStandAloneERV;
using namespace ObjexxFCL;
using namespace EnergyPlus::DataHeatBalance;
using namespace DataGlobals;
using namespace EnergyPlus::DataZoneEquipment;
using namespace EnergyPlus::DataSizing;


TEST_F( EnergyPlusFixture, HVACStandAloneERV_Test1 )
{
	ZoneEquipConfig.allocate( 1 );
	ZoneEquipConfig( 1 ).ZoneName = "Zone 1";
	ZoneEquipConfig( 1 ).ActualZoneNum = 1;

	Zone.allocate( 1 );
	Zone( 1 ).Name = ZoneEquipConfig( 1 ).ZoneName;
	ZoneEqSizing.allocate( 1 );
	CurZoneEqNum = 1;

	TotPeople = 2; // Total number of people statements
	People.allocate(TotPeople);
	People( 1 ).ZonePtr = 1;
	People( 1 ).NumberOfPeople = 100.0;
	People( 1 ).NumberOfPeoplePtr = ScheduleAlwaysOn; // From dataglobals, always returns a 1 for schedule value
	People( 2 ).ZonePtr = 1;
	People( 2 ).NumberOfPeople = 200.0;
	People( 2 ).NumberOfPeoplePtr = ScheduleAlwaysOn; // From dataglobals, always returns a 1 for schedule value

	StandAloneERV.allocate( 1 );

	StandAloneERV( 1 ).SupplyAirVolFlow = AutoSize;
	StandAloneERV( 1 ).AirVolFlowPerFloorArea = 1.0;
	StandAloneERV( 1 ).AirVolFlowPerOccupant = 0.0;
	Zone( 1 ).Multiplier = 1.0;
	Zone( 1 ).FloorArea = 1000.0;
	SizeStandAloneERV( 1 );
	EXPECT_EQ( 1000.0, StandAloneERV( 1 ).SupplyAirVolFlow );

	StandAloneERV( 1 ).SupplyAirVolFlow = AutoSize; // Need to reset this for each pass
	StandAloneERV( 1 ).AirVolFlowPerFloorArea = 0.0;
	StandAloneERV( 1 ).AirVolFlowPerOccupant = 10.0;
	Zone( 1 ).Multiplier = 1.0;
	Zone( 1 ).FloorArea = 1000.0;
	SizeStandAloneERV( 1 );
	EXPECT_EQ( 3000.0, StandAloneERV( 1 ).SupplyAirVolFlow );

	StandAloneERV( 1 ).SupplyAirVolFlow = AutoSize;
	StandAloneERV( 1 ).AirVolFlowPerFloorArea = 1.0;
	StandAloneERV( 1 ).AirVolFlowPerOccupant = 10.0;
	Zone( 1 ).Multiplier = 1.0;
	Zone( 1 ).FloorArea = 1000.0;
	SizeStandAloneERV( 1 );
	EXPECT_EQ( 4000.0, StandAloneERV( 1 ).SupplyAirVolFlow );

	StandAloneERV( 1 ).SupplyAirVolFlow = AutoSize;
	Zone( 1 ).Multiplier = 5.0;
	SizeStandAloneERV( 1 );
	EXPECT_EQ( 20000.0, StandAloneERV( 1 ).SupplyAirVolFlow );

}
