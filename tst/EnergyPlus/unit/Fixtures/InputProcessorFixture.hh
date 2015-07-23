#ifndef InputProcessorFixture_hh_INCLUDED
#define InputProcessorFixture_hh_INCLUDED

// Google test headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "EnergyPlusFixture.hh"

namespace EnergyPlus {

	class InputProcessorFixture : public EnergyPlusFixture
	{
	protected:
		static void SetUpTestCase() { }
		static void TearDownTestCase() { }
		
		virtual void SetUp() {
			EnergyPlusFixture::SetUp();  // Sets up the base fixture first.
		}

		virtual void TearDown() {			
			EnergyPlusFixture::TearDown();  // Remember to tear down the base fixture after cleaning up derived fixture!
		}

		bool process_idd( std::string const & idd, bool & errors_found ) {
			return EnergyPlusFixture::process_idd( idd, errors_found );
		}
	};

}

#endif
