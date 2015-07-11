#ifndef EnergyPlusFixture_hh_INCLUDED
#define EnergyPlusFixture_hh_INCLUDED

// Google Test Headers
#include <gtest/gtest.h>

// ObjexxFCL Headers
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataOutputs.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/InputProcessor.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/ScheduleManager.hh>

#include "../TestHelpers/IOHelper.hh"

namespace EnergyPlus {

	class EnergyPlusFixture : public testing::Test
	{
	protected:
		static void SetUpTestCase() { }
		static void TearDownTestCase() { }

		// This is run every unit test for this fixture.
		// It sets up the IOHelper class for all the necessary added functionality.
		// It also calls show_message every unit test to output a begin message to the error file.
		virtual void SetUp() {
			show_message();
			this->ioHelper = std::unique_ptr< IOHelper >( new IOHelper );
		}

		// This is run every unit test and makes sure to clear all state in global variables this fixture touches.
		virtual void TearDown() {
			CurveManager::clear_state();
			DataEnvironment::clear_state();
			DataGlobals::clear_state();
			DataHeatBalance::clear_state();
			DataIPShortCuts::clear_state();
			DataOutputs::clear_state();
			DataSurfaces::clear_state();
			HeatBalanceManager::clear_state();
			InputProcessor::clear_state();
			OutputProcessor::clear_state();
			ScheduleManager::clear_state();

			{ 
				IOFlags flags; 
				flags.DISPOSE( "DELETE" );
				gio::close( OutputProcessor::OutputFileMeterDetails, flags );
				gio::close( DataGlobals::OutputFileStandard, flags ); 
				gio::close( DataGlobals::OutputFileInits, flags );
				gio::close( DataGlobals::OutputFileDebug, flags );
				gio::close( DataGlobals::OutputFileZoneSizing, flags );
				gio::close( DataGlobals::OutputFileSysSizing, flags );
				gio::close( DataGlobals::OutputFileMeters, flags );
				gio::close( DataGlobals::OutputFileBNDetails, flags );
				gio::close( DataGlobals::OutputFileZonePulse, flags );

			}
		}

		// This will output the "Begin Test" ShowMessage for every unit test that uses or inherits from this fixture.
		// Now this does not need to be manually entered for every unit test as well as it will automatically be updated as the
		// unit test names change.
		void show_message() {
			// Gets information about the currently running test.
			// Do NOT delete the returned object - it's managed by the UnitTest class.
			const ::testing::TestInfo* const test_info = ::testing::UnitTest::GetInstance()->current_test_info();
			ShowMessage( "Begin Test: " + std::string( test_info->test_case_name() ) + ", " + std::string( test_info->name() ) );
		}

		static void use_cached_idd() {
			IOHelper::use_cache();
		}

		// This will compare either a STL container or ObjexxFCL container
		// Pass a forward iterator of a container you want to compare against an expected container. You can pass in an existing 
		// container or use an initializer list like below.
		// I had to only do one template typename due to wrapping this in Google Test macros causes the macro to fail. It thinks
		// there are more macro arguments than expected. This is a way around that restriction.
		// Example Usage: 
		// 		EXPECT_TRUE( compare_containers< std::vector< bool > >( { true } , ObjectDef( index ).AlphaOrNumeric ) );
		// 		EXPECT_TRUE( compare_containers< Array1D_bool >( { true } , ObjectDef( index ).AlphaOrNumeric ) );
		template < typename T, class T2 >
		bool compare_containers( T const & expected_container, T2 const & actual_container ) {
			return ioHelper->compare_containers< T, T2 >( expected_container, actual_container );
		}

		// This function creates a string based on a vector of string inputs that is delimited by DataStringGlobals::NL by default, but any 
		// delimiter can be passed in to this funciton. This allows for cross platform output string comparisons.
		std::string delimited_string( std::vector<std::string> const & strings, std::string const & delimiter = DataStringGlobals::NL ) {
			return ioHelper->delimited_string( strings, delimiter );
		}

		// Compare an expected string against the ESO stream. The default is to reset the ESO stream after every call.
		// It is easier to test successive functions if the ESO stream is 'empty' before the next call.
		bool compare_eso_stream( std::string const & expected_string, bool reset_stream = true ) {
			return ioHelper->compare_eso_stream( expected_string, reset_stream );
		}

		// Compare an expected string against the MTR stream. The default is to reset the MTR stream after every call.
		// It is easier to test successive functions if the MTR stream is 'empty' before the next call.
		bool compare_mtr_stream( std::string const & expected_string, bool reset_stream = true ) {
			return ioHelper->compare_mtr_stream( expected_string, reset_stream );
		}

		// Compare an expected string against the ECHO stream. The default is to reset the ECHO stream after every call.
		// It is easier to test successive functions if the ECHO stream is 'empty' before the next call.
		bool compare_echo_stream( std::string const & expected_string, bool reset_stream = true ) {
			return ioHelper->compare_echo_stream( expected_string, reset_stream );
		}

		// Compare an expected string against the COUT stream. The default is to reset the COUT stream after every call.
		// It is easier to test successive functions if the COUT stream is 'empty' before the next call.
		bool compare_cout_stream( std::string const & expected_string, bool reset_stream = true ) {
			return ioHelper->compare_cout_stream( expected_string, reset_stream );
		}

		// Compare an expected string against the CERR stream. The default is to reset the CERR stream after every call.
		// It is easier to test successive functions if the CERR stream is 'empty' before the next call.
		bool compare_cerr_stream( std::string const & expected_string, bool reset_stream = true ) {
			return ioHelper->compare_cerr_stream( expected_string, reset_stream );
		}

		// Check if ESO stream has any output. Useful to make sure there are or are not outputs to ESO.
		bool has_eso_output() {
			return ioHelper->has_eso_output();
		}

		// Check if MTR stream has any output. Useful to make sure there are or are not outputs to MTR.
		bool has_mtr_output() {
			return ioHelper->has_mtr_output();
		}

		// Check if ECHO stream has any output. Useful to make sure there are or are not outputs to ECHO.
		bool has_echo_output() {
			return ioHelper->has_echo_output();
		}

		// Check if COUT stream has any output. Useful to make sure there are or are not outputs to COUT.
		bool has_cout_output() {
			return ioHelper->has_cout_output();
		}

		// Check if CERR stream has any output. Useful to make sure there are or are not outputs to CERR.
		bool has_cerr_output() {
			return ioHelper->has_cerr_output();
		}

		// This function processes an idf snippet and defaults to using the idd cache for the fixture.
		// The cache should be used for nearly all calls to this function.
		// This more or less replicates InputProcessor::ProcessInput() but in a more usable fashion for unit testing
		bool process_idf( std::string const & idf_snippet, bool use_idd_cache = true ) {
			return ioHelper->process_idf( idf_snippet, use_idd_cache );
		}

		// This is a helper function to easily compare an expected IDF data structure with the actual IDFRecords data structure
		// Usage (assuming "Version,8.3;" was parsed as an idf snippet):
		// 		EXPECT_FALSE( compare_idf( "VERSION", 1, 0, 1, { "8.3" }, { false }, {}, {} ) );
		bool compare_idf( 
			std::string const & name, 
			int const num_alphas, 
			int const num_numbers, 
			int const object_def_ptr, 
			std::vector< std::string > const & alphas, 
			std::vector< bool > const & alphas_blank, 
			std::vector< Real64 > const & numbers, 
			std::vector< bool > const & numbers_blank 
		)
		{
			return ioHelper->compare_idf( name, num_alphas, num_numbers, object_def_ptr, alphas, alphas_blank, numbers, numbers_blank );
		}

	private:
		std::unique_ptr< IOHelper > ioHelper;

	};

	typedef EnergyPlusFixture EnergyPlusDeathTestFixture;

}

#endif
