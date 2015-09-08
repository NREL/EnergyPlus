#ifndef HVACFixture_hh_INCLUDED
#define HVACFixture_hh_INCLUDED

// Google test headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "EnergyPlusFixture.hh"
#include <EnergyPlus/Psychrometrics.hh>

namespace EnergyPlus {

	class HVACFixture : public EnergyPlusFixture
	{
	protected:
		static void SetUpTestCase() { }
		static void TearDownTestCase() { }
		
		virtual void SetUp() {
			EnergyPlusFixture::SetUp();  // Sets up the base fixture first.

			Psychrometrics::InitializePsychRoutines();
		}

		virtual void TearDown() {
			Psychrometrics::cached_Twb.deallocate();
			Psychrometrics::cached_Psat.deallocate();

			DataAirLoop::clear_state();
			DataBranchNodeConnections::clear_state();
			DataHVACGlobals::clear_state();
			DataLoopNode::clear_state();
			DataSizing::clear_state();
			DXCoils::clear_state();
			ExteriorEnergyUse::clear_state();
			Fans::clear_state();
			GlobalNames::clear_state();
			Humidifiers::clear_state();
			HVACVariableRefrigerantFlow::clear_state();
			MixedAir::clear_state();
			NodeInputManager::clear_state();
			OutAirNodeManager::clear_state();

			EnergyPlusFixture::TearDown();  // Remember to tear down the base fixture after cleaning up derived fixture!
		}
	};

}

#endif
