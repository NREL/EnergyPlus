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
	
		// clear
		ControllerProps.deallocate();
		ControllerLists.deallocate();
		AllSetPtMgr.deallocate();
		SchSetPtMgr.deallocate();
		WaterCoil.deallocate();
	}


}
