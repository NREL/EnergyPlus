#ifndef OutputProcessorFixture_hh_INCLUDED
#define OutputProcessorFixture_hh_INCLUDED

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "SQLiteFixture.hh"
#include <EnergyPlus/OutputProcessor.hh>

#include <ObjexxFCL/gio.hh>

namespace EnergyPlus {

	namespace OutputProcessor {

		class OutputProcessorFixture : public SQLiteFixture
		{

		protected:
			static void SetUpTestCase() {
				// Needed to initialize IDD cache
				EnergyPlusFixture::SetUpTestCase();
			}
			static void TearDownTestCase() { }
			
			virtual void SetUp() {
				SQLiteFixture::SetUp();  // Sets up the base fixture first.

				// might want to call InitializeOutput()...
				ReportList.allocate( 500 );

				FreqNotice = Array2D_string( {1,2}, {-1,4} );
			}

			virtual void TearDown() {
				OutputProcessor::clear_state();

				{ IOFlags flags; flags.DISPOSE( "DELETE" ); gio::close( OutputFileMeterDetails, flags ); }

				SQLiteFixture::TearDown();  // Remember to tear down the base fixture after cleaning up derived fixture!
			}

		};

		typedef OutputProcessorFixture OutputProcessorDeathTestFixture;

	}

}

#endif
