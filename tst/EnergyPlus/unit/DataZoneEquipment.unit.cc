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

// EnergyPlus::ExteriorEnergyUse Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/UtilityRoutines.hh>
#include <DataHeatBalance.hh>
#include <DataSizing.hh>
#include <DataContaminantBalance.hh>
#include <ScheduleManager.hh>
#include <DataEnvironment.hh>

#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus;
using namespace EnergyPlus::DataZoneEquipment;
using namespace ObjexxFCL;

TEST_F( EnergyPlusFixture, DataZoneEquipment_TestGetSystemNodeNumberForZone )
{

	NumOfZones = 2;
	ZoneEquipConfig.allocate( NumOfZones ); 

	ZoneEquipConfig( 1 ).ZoneName = "Zone1";
	ZoneEquipConfig( 1 ).ActualZoneNum = 1;
	ZoneEquipConfig( 1 ).ZoneNode = 1;

	ZoneEquipConfig( 2 ).ZoneName = "Zone2";
	ZoneEquipConfig( 2 ).ActualZoneNum = 2;
	ZoneEquipConfig( 2 ).ZoneNode = 2;

	ZoneEquipInputsFilled = true;

	EXPECT_EQ( 0, GetSystemNodeNumberForZone( "NonExistingZone" ) );
	EXPECT_EQ( 1, GetSystemNodeNumberForZone( "Zone1" ) );

	ZoneEquipConfig.deallocate();
}

TEST_F( EnergyPlusFixture, DataZoneEquipment_TestCalcDesignSpecificationOutdoorAir )
{
	// #6225

	DataHeatBalance::Zone.allocate( 1 );
	DataSizing::OARequirements.allocate( 1 );
	DataHeatBalance::ZoneIntGain.allocate( 1 );
	DataHeatBalance::People.allocate( 1 );
	ScheduleManager::Schedule.allocate( 2 );
	DataContaminantBalance::ZoneCO2GainFromPeople.allocate( 1 );
	DataContaminantBalance::ZoneAirCO2.allocate( 1 );
	DataContaminantBalance::ZoneSysContDemand.allocate( 1 );

	DataEnvironment::StdRhoAir = 1.20;

	DataHeatBalance::Zone( 1 ).FloorArea = 10.0;
	DataHeatBalance::Zone( 1 ).TotOccupants = 5.0;
	DataHeatBalance::Zone( 1 ).ZoneContamControllerSchedIndex = 1;
	DataHeatBalance::People( 1 ).ZonePtr = 1;
	DataHeatBalance::TotPeople = 1;
	DataHeatBalance::People( 1 ).ActivityLevelPtr = 2;
	DataHeatBalance::People( 1 ).CO2RateFactor = 3.82e-8;
	DataHeatBalance::People( 1 ).NumberOfPeople = DataHeatBalance::Zone( 1 ).TotOccupants;

	DataContaminantBalance::Contaminant.CO2Simulation = true;
	DataContaminantBalance::OutdoorCO2 = 400.0;
	DataContaminantBalance::ZoneCO2GainFromPeople( 1 ) = 3.82E-8 * 5.0;

	DataSizing::NumOARequirements = 1;
	DataSizing::OARequirements( 1 ).Name = "ZONE OA";
	DataSizing::OARequirements( 1 ).OAFlowMethod = DataSizing::ZOAM_ProportionalControlSchOcc;
	DataSizing::OARequirements( 1 ).OAFlowPerPerson = 0.002;
	DataSizing::OARequirements( 1 ).OAFlowPerArea = 0.003;
	DataHeatBalance::ZoneIntGain( 1 ).NOFOCC = 0.5;
	ScheduleManager::Schedule( 1 ).CurrentValue = 1.0;
	ScheduleManager::Schedule( 2 ).CurrentValue = 131.881995;

	Real64 OAVolumeFlowRate;
	// Test ZOAM_ProportionalControlSchOcc
	DataContaminantBalance::ZoneAirCO2( 1 ) = 500.0;
	OAVolumeFlowRate = CalcDesignSpecificationOutdoorAir( 1, 1, false, false );
	EXPECT_NEAR( 0.031, OAVolumeFlowRate, 0.00001 );

	DataContaminantBalance::ZoneAirCO2( 1 ) = 405.0;
	OAVolumeFlowRate = CalcDesignSpecificationOutdoorAir( 1, 1, false, false );
	EXPECT_NEAR( 0.0308115, OAVolumeFlowRate, 0.00001 );

	// Test ZOAM_ProportionalControlDesOcc
	DataContaminantBalance::ZoneAirCO2( 1 ) = 500.0;
	DataSizing::OARequirements( 1 ).OAFlowMethod = DataSizing::ZOAM_ProportionalControlDesOcc;
	OAVolumeFlowRate = CalcDesignSpecificationOutdoorAir( 1, 1, false, false );
	EXPECT_NEAR( 0.0315879, OAVolumeFlowRate, 0.00001 );

	// Test ZOAM_IAQP
	DataSizing::OARequirements( 1 ).OAFlowMethod = DataSizing::ZOAM_IAQP;
	DataContaminantBalance::ZoneSysContDemand( 1 ).OutputRequiredToCO2SP = 0.2 * DataEnvironment::StdRhoAir;
	OAVolumeFlowRate = CalcDesignSpecificationOutdoorAir( 1, 1, false, false );
	EXPECT_NEAR( 0.2, OAVolumeFlowRate, 0.00001 );

	// Cleanup
	DataHeatBalance::Zone.deallocate( );
	DataSizing::OARequirements.deallocate( );
	DataHeatBalance::ZoneIntGain.deallocate( );
	ScheduleManager::Schedule.deallocate( );
	DataHeatBalance::People.deallocate( );
	DataContaminantBalance::ZoneCO2GainFromPeople.deallocate( );
	DataContaminantBalance::ZoneAirCO2.deallocate( );
	DataContaminantBalance::ZoneSysContDemand.deallocate( );

}
