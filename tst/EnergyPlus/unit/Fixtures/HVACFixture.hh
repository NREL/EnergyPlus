#ifndef HVACFixture_hh_INCLUDED
#define HVACFixture_hh_INCLUDED

// Google test headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "EnergyPlusFixture.hh"
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataBranchNodeConnections.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/Humidifiers.hh>
#include <EnergyPlus/MixedAir.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutAirNodeManager.hh>
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
			Humidifiers::clear_state();
			MixedAir::clear_state();
			NodeInputManager::clear_state();
			OutAirNodeManager::clear_state();
			
			EnergyPlusFixture::TearDown();  // Remember to tear down the base fixture after cleaning up derived fixture!
		}
	};

}

#endif
