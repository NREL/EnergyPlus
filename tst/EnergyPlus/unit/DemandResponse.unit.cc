// EnergyPlus::DemandManager ventilation test

// Google test headers
#include <gtest/gtest.h>

#include <DataGlobals.hh>
#include <MixedAir.hh>
#include <DemandManager.hh>
#include "Fixtures/HVACFixture.hh"

using namespace EnergyPlus;
using namespace ObjexxFCL;
using namespace EnergyPlus::MixedAir;
using namespace EnergyPlus::DemandManager;

namespace EnergyPlus {

TEST_F(HVACFixture, DemandManagerGetInput)
{
	// Test input processing for DemandManager:Ventilation

	std::string const idf_objects = delimited_string({
		"DemandManager:Ventilation,",
		" Ventilation Manager,",
		" ,",
		" FIXEDRATE,",
		" 60,",
		" 0.2,",
		" ,", // N3 left blank because Numbers was only assigned up to 2
		" ,", // N4 left blank because Numbers was only assigned up to 2
		" ALL,",
		" ,",
		" OA CONTROLLER 1;"});

	ASSERT_FALSE( process_idf( idf_objects ) );

	NumOAControllers = 1;
	OAController.allocate( NumOAControllers );
	OAController(1).Name = "OA CONTROLLER 1";

	GetDemandManagerInput();

	EXPECT_EQ( DataGlobals::ScheduleAlwaysOn, DemandMgr( 1 ).AvailSchedule );
	EXPECT_EQ( ManagerLimitFixed, DemandMgr( 1 ).LimitControl );
	EXPECT_DOUBLE_EQ( 60.0, DemandMgr( 1 ).LimitDuration );
	EXPECT_DOUBLE_EQ( 0.2, DemandMgr( 1 ).FixedRate );
	EXPECT_EQ( ManagerSelectionAll, DemandMgr( 1 ).SelectionControl );
	EXPECT_EQ( 1, DemandMgr( 1 ).NumOfLoads );

}

}
