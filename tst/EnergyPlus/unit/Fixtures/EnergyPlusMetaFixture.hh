#ifndef EnergyPlusMetaFixture_hh_INCLUDED
#define EnergyPlusMetaFixture_hh_INCLUDED

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "EnergyPlusFixture.hh"

namespace EnergyPlus {

	class EnergyPlusMetaFixture : public EnergyPlusFixture
	{
	protected:
		static void SetUpTestCase() {
			// Needed to initialize IDD cache
			EnergyPlusFixture::SetUpTestCase();
		}
		static void TearDownTestCase() { }

		virtual void SetUp() {
			EnergyPlusFixture::SetUp();  // Sets up the base fixture first.
		}

		virtual void TearDown() {
			EnergyPlusFixture::TearDown();  // Remember to tear down the base fixture after cleaning up derived fixture!
		}

		void use_cached_idd() {
			EnergyPlusFixture::m_idd_cache->use_cache();
		}

		bool process_idd( std::string const & idd, bool & errors_found ) {
			return EnergyPlusFixture::process_idd( idd, errors_found );
		}
	};

	typedef EnergyPlusMetaFixture EnergyPlusMetaDeathTestFixture;
}

#endif
