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

// EnergyPlus::ChilledCeilingPanelSimple Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataHeatBalSurface.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataHeatBalFanSys.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/ChilledCeilingPanelSimple.hh>

using namespace EnergyPlus::CoolingPanelSimple;

namespace EnergyPlus {

	TEST_F( EnergyPlusFixture, SetCoolingPanelControlTemp )
	{

		Real64 ControlTemp; // Temperature that is controlling the panel
		int CoolingPanelNum; // Cooling panel number
		int ZoneNum; // Zone number for the cooling panel

		ControlTemp = 0.0;
		CoolingPanelNum = 1;
		ZoneNum = 1;
		
		CoolingPanelSimple::CoolingPanel.allocate( 1 );
		DataHeatBalFanSys::MAT.allocate( 1 );
		DataHeatBalFanSys::MAT( 1 ) = 22.0;
		DataHeatBalance::MRT.allocate( 1 );
		DataHeatBalance::MRT( 1 ) = 20.0;
		DataHeatBalance::Zone.allocate( 1 );
		DataHeatBalance::Zone( 1 ).OutDryBulbTemp = 10.0;
		DataHeatBalance::Zone( 1 ).OutWetBulbTemp = 5.0;
		
		CoolingPanelSimple::CoolingPanel( CoolingPanelNum ).ControlType = CoolingPanelSimple::MATControl;
		SetCoolingPanelControlTemp( ControlTemp, CoolingPanelNum, ZoneNum );
		EXPECT_EQ( ControlTemp, 22.0 );

		CoolingPanelSimple::CoolingPanel( CoolingPanelNum ).ControlType = CoolingPanelSimple::MRTControl;
		SetCoolingPanelControlTemp( ControlTemp, CoolingPanelNum, ZoneNum );
		EXPECT_EQ( ControlTemp, 20.0 );

		CoolingPanelSimple::CoolingPanel( CoolingPanelNum ).ControlType = CoolingPanelSimple::OperativeControl;
		SetCoolingPanelControlTemp( ControlTemp, CoolingPanelNum, ZoneNum );
		EXPECT_EQ( ControlTemp, 21.0 );

		CoolingPanelSimple::CoolingPanel( CoolingPanelNum ).ControlType = CoolingPanelSimple::ODBControl;
		SetCoolingPanelControlTemp( ControlTemp, CoolingPanelNum, ZoneNum );
		EXPECT_EQ( ControlTemp, 10.0 );

		CoolingPanelSimple::CoolingPanel( CoolingPanelNum ).ControlType = CoolingPanelSimple::OWBControl;
		SetCoolingPanelControlTemp( ControlTemp, CoolingPanelNum, ZoneNum );
		EXPECT_EQ( ControlTemp, 5.0 );
				
	}

	TEST_F( EnergyPlusFixture, SizeCoolingPanelUA )
	{
		
		int CoolingPanelNum; // Cooling panel number
		bool SizeCoolingPanelUASuccess;
		
		CoolingPanelNum = 1;
		SizeCoolingPanelUASuccess = true;

		CoolingPanelSimple::CoolingPanel.allocate( CoolingPanelNum );

		CoolingPanel( CoolingPanelNum ).RatedWaterFlowRate = 1.0;

		// No error case
		CoolingPanel( CoolingPanelNum ).RatedWaterFlowRate = 1.0;
		CoolingPanel( CoolingPanelNum ).ScaledCoolingCapacity = 4000.0;
		CoolingPanel( CoolingPanelNum ).RatedWaterTemp = 20.0;
		CoolingPanel( CoolingPanelNum ).RatedZoneAirTemp = 21.0;
		SizeCoolingPanelUASuccess = SizeCoolingPanelUA( CoolingPanelNum );
		EXPECT_EQ( SizeCoolingPanelUASuccess, true );

		// Capacity too high case
		CoolingPanel( CoolingPanelNum ).RatedWaterFlowRate = 1.0;
		CoolingPanel( CoolingPanelNum ).ScaledCoolingCapacity = 5000.0;
		CoolingPanel( CoolingPanelNum ).RatedWaterTemp = 20.0;
		CoolingPanel( CoolingPanelNum ).RatedZoneAirTemp = 21.0;
		SizeCoolingPanelUASuccess = SizeCoolingPanelUA( CoolingPanelNum );
		EXPECT_EQ( SizeCoolingPanelUASuccess, false );

		// Water temperature higher than zone temperature (not cooling) case
		CoolingPanel( CoolingPanelNum ).RatedWaterFlowRate = 1.0;
		CoolingPanel( CoolingPanelNum ).ScaledCoolingCapacity = 4000.0;
		CoolingPanel( CoolingPanelNum ).RatedWaterTemp = 21.0;
		CoolingPanel( CoolingPanelNum ).RatedZoneAirTemp = 20.0;
		SizeCoolingPanelUASuccess = SizeCoolingPanelUA( CoolingPanelNum );
		EXPECT_EQ( SizeCoolingPanelUASuccess, false );
		
	}
}
