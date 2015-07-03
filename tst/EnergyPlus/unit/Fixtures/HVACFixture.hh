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
			
			EnergyPlusFixture::TearDown();  // Remember to tear down the base fixture after cleaning up derived fixture!
		}
	};

	typedef HVACFixture HVACDeathTestFixture;

}

#endif
