#ifndef HVACFixture_hh_INCLUDED
#define HVACFixture_hh_INCLUDED

// Google test headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "EnergyPlusFixture.hh"
#include <EnergyPlus/DataBranchNodeConnections.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/Humidifiers.hh>
#include <EnergyPlus/NodeInputManager.hh>
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

			DataBranchNodeConnections::clear_state();
			DataHVACGlobals::clear_state();
			DataSizing::clear_state();
			Humidifiers::clear_state();
			NodeInputManager::clear_state();
			
			EnergyPlusFixture::TearDown();  // Remember to tear down the base fixture after cleaning up derived fixture!
		}
	};

	typedef HVACFixture HVACDeathTestFixture;

}

#endif
