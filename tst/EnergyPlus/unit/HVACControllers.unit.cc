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

// EnergyPlus::HVACControllers Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus/HVACControllers.hh>
#include <EnergyPlus/MixedAir.hh>
#include <EnergyPlus/ScheduleManager.hh>
#include <EnergyPlus/SetPointManager.hh>
#include <EnergyPlus/WaterCoils.hh>


#include "Fixtures/EnergyPlusFixture.hh"

using namespace EnergyPlus::MixedAir;
using namespace EnergyPlus::HVACControllers;
using namespace EnergyPlus::ScheduleManager;
using namespace EnergyPlus::SetPointManager;
using namespace EnergyPlus::WaterCoils;

namespace EnergyPlus {


	TEST_F( EnergyPlusFixture, HVACControllers_ResetHumidityRatioCtrlVarType ) {
		std::string const idf_objects = delimited_string( {
		" Version,8.3;",
		" Coil:Cooling:Water,",
		"	Chilled Water Coil,	!- Name",
		"	AvailSched,			!- Availability Schedule Name",
		"	autosize,			!- Design Water Flow Rate { m3 / s }",
		"	autosize,			!- Design Air Flow Rate { m3 / s }",
		"	autosize,			!- Design Inlet Water Temperature { C }",
		"	autosize,			!- Design Inlet Air Temperature { C }",
		"	autosize,			!- Design Outlet Air Temperature { C }",
		"	autosize,			!- Design Inlet Air Humidity Ratio { kgWater / kgDryAir }",
		"	autosize,			!- Design Outlet Air Humidity Ratio { kgWater / kgDryAir }",
		"	Water Inlet Node,	!- Water Inlet Node Name",
		"	Water Outlet Node,  !- Water Outlet Node Name",
		"	Air Inlet Node,		!- Air Inlet Node Name",
		"	Air Outlet Node,	!- Air Outlet Node Name",
		"	SimpleAnalysis,		!- Type of Analysis",
		"	CrossFlow;          !- Heat Exchanger Configuration",
		" Controller:WaterCoil,",
		"	CW Coil Controller, !- Name",
		"	HumidityRatio,		!- Control Variable",
		"	Reverse,			!- Action",
		"	FLOW,				!- Actuator Variable",
		"	Air Outlet Node,	!- Sensor Node Name",
		"	Water Inlet Node,	!- Actuator Node Name",
		"	autosize,			!- Controller Convergence Tolerance { deltaC }",
		"	autosize,			!- Maximum Actuated Flow { m3 / s }",
		"	0.0;				!- Minimum Actuated Flow { m3 / s }",
		" SetpointManager:Scheduled,",
		"	HumRatSPManager,	!- Name",
		"	HumidityRatio,		!- Control Variable",
		"	HumRatioSched,		!- Schedule Name",
		"	Air Outlet Node;	!- Setpoint Node or NodeList Name",
		" Schedule:Compact,",
		"   HumRatioSched,		!- Name",
		"	Any Number,			!- Schedule Type Limits Name",
		"	Through: 12/31,		!- Field 1",
		"	For: AllDays,		!- Field 2",
		"	Until: 24:00, 0.015; !- Field 3",
		" Schedule:Compact,",
		"   AvailSched,			!- Name",
		"	Fraction,			!- Schedule Type Limits Name",
		"	Through: 12/31,		!- Field 1",
		"	For: AllDays,		!- Field 2",
		"	Until: 24:00, 1.0;  !- Field 3",
		" AirLoopHVAC:ControllerList,",
		"	CW Coil Controller, !- Name",
		"	Controller:WaterCoil, !- Controller 1 Object Type",
		"	CW Coil Controller; !- Controller 1 Name",
		});

		ASSERT_FALSE( process_idf( idf_objects ) );

		GetSetPointManagerInputs();
		// check specified control variable type is "HumidityRatio"
		ASSERT_EQ( iCtrlVarType_HumRat, AllSetPtMgr( 1 ).CtrlTypeMode );

		GetControllerInput();
		// check control variable type in AllSetPtMgr is reset to "MaximumHumidityRatio"
		ASSERT_EQ( iCtrlVarType_MaxHumRat, AllSetPtMgr( 1 ).CtrlTypeMode );

		// ControllerProps always expects the control variable type to be "HumididtyRatio"
		ControllerProps( 1 ).HumRatCntrlType = GetHumidityRatioVariableType( ControllerProps( 1 ).SensedNode );
		ASSERT_EQ( iCtrlVarType_HumRat, ControllerProps( 1 ).HumRatCntrlType );
	
	}

	TEST_F( EnergyPlusFixture, HVACControllers_TestTempAndHumidityRatioCtrlVarType ) {
		std::string const idf_objects = delimited_string( {
			" Version,8.3;",
			" Coil:Cooling:Water,",
			"	Chilled Water Coil,	!- Name",
			"	AvailSched,			!- Availability Schedule Name",
			"	autosize,			!- Design Water Flow Rate { m3 / s }",
			"	autosize,			!- Design Air Flow Rate { m3 / s }",
			"	autosize,			!- Design Inlet Water Temperature { C }",
			"	autosize,			!- Design Inlet Air Temperature { C }",
			"	autosize,			!- Design Outlet Air Temperature { C }",
			"	autosize,			!- Design Inlet Air Humidity Ratio { kgWater / kgDryAir }",
			"	autosize,			!- Design Outlet Air Humidity Ratio { kgWater / kgDryAir }",
			"	Water Inlet Node,	!- Water Inlet Node Name",
			"	Water Outlet Node,  !- Water Outlet Node Name",
			"	Air Inlet Node,		!- Air Inlet Node Name",
			"	Air Outlet Node,	!- Air Outlet Node Name",
			"	SimpleAnalysis,		!- Type of Analysis",
			"	CrossFlow;          !- Heat Exchanger Configuration",
			" Controller:WaterCoil,",
			"	CW Coil Controller, !- Name",
			"	TemperatureAndHumidityRatio,		!- Control Variable",
			"	Reverse,			!- Action",
			"	FLOW,				!- Actuator Variable",
			"	Air Outlet Node,	!- Sensor Node Name",
			"	Water Inlet Node,	!- Actuator Node Name",
			"	autosize,			!- Controller Convergence Tolerance { deltaC }",
			"	autosize,			!- Maximum Actuated Flow { m3 / s }",
			"	0.0;				!- Minimum Actuated Flow { m3 / s }",
			" SetpointManager:Scheduled,",
			"	HumRatSPManager,	!- Name",
			"	MaximumHumidityRatio,  !- Control Variable",
			"	HumRatioSched,		!- Schedule Name",
			"	Air Outlet Node;	!- Setpoint Node or NodeList Name",
			" Schedule:Compact,",
			"   HumRatioSched,		!- Name",
			"	Any Number,			!- Schedule Type Limits Name",
			"	Through: 12/31,		!- Field 1",
			"	For: AllDays,		!- Field 2",
			"	Until: 24:00, 0.015; !- Field 3",
			" Schedule:Compact,",
			"   AvailSched,			!- Name",
			"	Fraction,			!- Schedule Type Limits Name",
			"	Through: 12/31,		!- Field 1",
			"	For: AllDays,		!- Field 2",
			"	Until: 24:00, 1.0;  !- Field 3",
			" AirLoopHVAC:ControllerList,",
			"	CW Coil Controller, !- Name",
			"	Controller:WaterCoil, !- Controller 1 Object Type",
			"	CW Coil Controller; !- Controller 1 Name",
		} );

		ASSERT_FALSE( process_idf( idf_objects ) );

		GetSetPointManagerInputs();
		// check specified control variable type is "HumidityRatio"
		ASSERT_EQ( iCtrlVarType_MaxHumRat, AllSetPtMgr( 1 ).CtrlTypeMode );

		GetControllerInput();
		// check control variable type in AllSetPtMgr is reset to "MaximumHumidityRatio"
		ASSERT_EQ( iCtrlVarType_MaxHumRat, AllSetPtMgr( 1 ).CtrlTypeMode );

		// ControllerProps expects the control variable type to be "MaximumHumididtyRatio"
		ControllerProps( 1 ).HumRatCntrlType = GetHumidityRatioVariableType( ControllerProps( 1 ).SensedNode );
		ASSERT_EQ( iCtrlVarType_MaxHumRat, ControllerProps( 1 ).HumRatCntrlType );

	}

}
