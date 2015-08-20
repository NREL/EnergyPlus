#ifndef GroundTempsFixture_hh_INCLUDED
#define GroundTempsFixture_hh_INCLUDED

// Google test headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "EnergyPlusFixture.hh"
#include <EnergyPlus/GroundTemperatureModeling/GroundTemperatureModelManager.hh>

namespace EnergyPlus {

	class GroundTempsFixture : public EnergyPlusFixture
	{
	protected:
		static void SetUpTestCase() { }
		static void TearDownTestCase() { }
		
		virtual void SetUp() {
			EnergyPlusFixture::SetUp();  // Sets up the base fixture first.       
		}

		virtual void TearDown() {
			GroundTemperatureManager::clear_state();		

			EnergyPlusFixture::TearDown();  // Remember to tear down the base fixture after cleaning up derived fixture!
		}
	};
}

#endif
