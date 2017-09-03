// EnergyPlus, Copyright (c) 1996-2017, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
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

