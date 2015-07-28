#ifndef EnergyPlusFixture_hh_INCLUDED
#define EnergyPlusFixture_hh_INCLUDED

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/EnergyPlus.hh>
#include <EnergyPlus/DataStringGlobals.hh>
#include <EnergyPlus/InputProcessor.hh>

#include <memory>
#include <ostream>

namespace EnergyPlus {

	// This is a helper struct to redirect std::cout. This makes sure std::cout is redirected back and
	// everything is cleaned up properly
	struct RedirectCout
	{
		RedirectCout( std::unique_ptr<std::ostringstream> const & m_buffer ) 
		: m_old_buffer( std::cout.rdbuf( m_buffer->rdbuf() ) )
		{ }

		~RedirectCout( )
		{
			std::cout.rdbuf( m_old_buffer.release() );
		}

		private:
			std::unique_ptr<std::streambuf> m_old_buffer;
	};

	// This is a helper struct to redirect std::cerr. This makes sure std::cerr is redirected back and
	// everything is cleaned up properly
	struct RedirectCerr
	{
		RedirectCerr( std::unique_ptr<std::ostringstream> const & m_buffer ) 
		: m_old_buffer( std::cerr.rdbuf( m_buffer->rdbuf() ) )
		{ }

		~RedirectCerr( )
		{
			std::cerr.rdbuf( m_old_buffer.release() );
		}

		private:
			std::unique_ptr<std::streambuf> m_old_buffer;
	};

	// struct to implement the caching of InputProcessor namespace variables
	// This reads all the variables in the InputProcessor namespace in the constructor and stores them in member variables.
	// Then when use_cached_namespace_variables() is called, it copies the member variables into the InputProcessor namespace variables.
	struct InputProcessorCache {
		InputProcessorCache();

		void use_cached_namespace_variables();

	private:
		int m_NumObjectDefs = 0;
		int m_NumSectionDefs = 0;
		int m_MaxObjectDefs = 0;
		int m_MaxSectionDefs = 0;
		int m_NumLines = 0;
		int m_MaxIDFRecords = 0;
		int m_NumIDFRecords = 0;
		int m_MaxIDFSections = 0;
		int m_NumIDFSections = 0;
		int m_EchoInputFile = 0;
		int m_InputLineLength = 0;
		int m_MaxAlphaArgsFound = 0;
		int m_MaxNumericArgsFound = 0;
		int m_NumAlphaArgsFound = 0;
		int m_NumNumericArgsFound = 0;
		int m_MaxAlphaIDFArgsFound = 0;
		int m_MaxNumericIDFArgsFound = 0;
		int m_MaxAlphaIDFDefArgsFound = 0;
		int m_MaxNumericIDFDefArgsFound = 0;
		int m_NumOutOfRangeErrorsFound = 0;
		int m_NumBlankReqFieldFound = 0;
		int m_NumMiscErrorsFound = 0;
		int m_MinimumNumberOfFields = 0;
		int m_NumObsoleteObjects = 0;
		int m_TotalAuditErrors = 0;
		int m_NumSecretObjects = 0;
		bool m_ProcessingIDD = false;
		std::string m_InputLine;
		Array1D_string m_ListOfSections;
		Array1D_string m_ListOfObjects;
		Array1D_int m_iListOfObjects;
		Array1D_int m_ObjectGotCount;
		Array1D_int m_ObjectStartRecord;
		std::string m_CurrentFieldName;
		Array1D_string m_ObsoleteObjectsRepNames;
		std::string m_ReplacementName;
		bool m_OverallErrorFlag = false;
		bool m_EchoInputLine = true;
		bool m_ReportRangeCheckErrors = true;
		bool m_FieldSet = false;
		bool m_RequiredField = false;
		bool m_RetainCaseFlag = false;
		bool m_ObsoleteObject = false;
		bool m_RequiredObject = false;
		bool m_UniqueObject = false;
		bool m_ExtensibleObject = false;
		int m_ExtensibleNumFields = 0;
		Array1D_bool m_IDFRecordsGotten;
		Array1D< InputProcessor::ObjectsDefinition > m_ObjectDef;
		Array1D< InputProcessor::SectionsDefinition > m_SectionDef;
		Array1D< InputProcessor::FileSectionsDefinition > m_SectionsOnFile;
		InputProcessor::LineDefinition m_LineItem;
		Array1D< InputProcessor::LineDefinition > m_IDFRecords;
		Array1D< InputProcessor::SecretObjects > m_RepObjects;
		Array1D_string m_cAlphaFieldNames;
		Array1D_string m_cNumericFieldNames;
		Array1D_bool m_lNumericFieldBlanks;
		Array1D_bool m_lAlphaFieldBlanks;
		Array1D_string m_cAlphaArgs;
		Array1D< Real64 > m_rNumericArgs;
	};

	class EnergyPlusFixture : public testing::Test
	{
	protected:
		static void SetUpTestCase() { }
		static void TearDownTestCase() { }

		// This is run every unit test for this fixture.
		// It sets up the various stream redirections.
		// It also calls show_message every unit test to output a begin message to the error file.
		virtual void SetUp();

		// This is run every unit test and makes sure to clear all state in global variables this fixture touches.
		virtual void TearDown();

		// This will output the "Begin Test" ShowMessage for every unit test that uses or inherits from this fixture.
		// Now this does not need to be manually entered for every unit test as well as it will automatically be updated as the
		// unit test names change.
		inline
		void show_message()
		{
			// Gets information about the currently running test.
			// Do NOT delete the returned object - it's managed by the UnitTest class.
			const ::testing::TestInfo* const test_info = ::testing::UnitTest::GetInstance()->current_test_info();
			ShowMessage( "Begin Test: " + std::string( test_info->test_case_name() ) + ", " + std::string( test_info->name() ) );
		}

		// This first lazy initializes the IDD cache. It is only initialized if it is needed and used by any unit test.
		// After the first initialization, it uses the cached IDD. The run time of individual unit tests can vary depending on
		// which unit tests first calls use_cached_idd(), since that first call will take more time.
		static void use_cached_idd();

		// This will compare either a STL container or ObjexxFCL container
		// Pass a container you want to compare against an expected container. You can pass in an existing 
		// container or use an initializer list like below.
		// This calls EXPECT_* within the function as well as returns a boolean so you can call [ASSERT/EXPECT]_[TRUE/FALSE] depending
		// if it makes sense for the unit test to continue after returning from function.
		// Will return true if containers are equal and false if they are not.
		// Example Usage: 
		// 		EXPECT_TRUE( compare_containers( std::vector< bool >( { true } ) , ObjectDef( index ).AlphaOrNumeric ) );
		// 		EXPECT_TRUE( compare_containers( Array1D_bool( { true } ) , ObjectDef( index ).AlphaOrNumeric ) );
		template < class T, class T2 >
		bool compare_containers( T const & expected_container, T2 const & actual_container );

		// This function creates a string based on a vector of string inputs that is delimited by DataStringGlobals::NL by default, but any 
		// delimiter can be passed in to this funciton. This allows for cross platform output string comparisons.
		std::string delimited_string( std::vector<std::string> const & strings, std::string const & delimiter = DataStringGlobals::NL );

		// Compare an expected string against the ESO stream. The default is to reset the ESO stream after every call.
		// It is easier to test successive functions if the ESO stream is 'empty' before the next call.
		// This calls EXPECT_* within the function as well as returns a boolean so you can call [ASSERT/EXPECT]_[TRUE/FALSE] depending
		// if it makes sense for the unit test to continue after returning from function.
		// Will return true if string matches the stream and false if it does not
		bool compare_eso_stream( std::string const & expected_string, bool reset_stream = true );

		// Compare an expected string against the MTR stream. The default is to reset the MTR stream after every call.
		// It is easier to test successive functions if the MTR stream is 'empty' before the next call.
		// This calls EXPECT_* within the function as well as returns a boolean so you can call [ASSERT/EXPECT]_[TRUE/FALSE] depending
		// if it makes sense for the unit test to continue after returning from function.
		// Will return true if string matches the stream and false if it does not
		bool compare_mtr_stream( std::string const & expected_string, bool reset_stream = true );

		// Compare an expected string against the ECHO stream. The default is to reset the ECHO stream after every call.
		// It is easier to test successive functions if the ECHO stream is 'empty' before the next call.
		// This calls EXPECT_* within the function as well as returns a boolean so you can call [ASSERT/EXPECT]_[TRUE/FALSE] depending
		// if it makes sense for the unit test to continue after returning from function.
		// Will return true if string matches the stream and false if it does not
		bool compare_echo_stream( std::string const & expected_string, bool reset_stream = true );

		// Compare an expected string against the COUT stream. The default is to reset the COUT stream after every call.
		// It is easier to test successive functions if the COUT stream is 'empty' before the next call.
		// This calls EXPECT_* within the function as well as returns a boolean so you can call [ASSERT/EXPECT]_[TRUE/FALSE] depending
		// if it makes sense for the unit test to continue after returning from function.
		// Will return true if string matches the stream and false if it does not
		bool compare_cout_stream( std::string const & expected_string, bool reset_stream = true );

		// Compare an expected string against the CERR stream. The default is to reset the CERR stream after every call.
		// It is easier to test successive functions if the CERR stream is 'empty' before the next call.
		// This calls EXPECT_* within the function as well as returns a boolean so you can call [ASSERT/EXPECT]_[TRUE/FALSE] depending
		// if it makes sense for the unit test to continue after returning from function.
		// Will return true if string matches the stream and false if it does not
		bool compare_cerr_stream( std::string const & expected_string, bool reset_stream = true );

		// Check if ESO stream has any output. Useful to make sure there are or are not outputs to ESO.
		inline
		bool has_eso_output()
		{
			return this->eso_stream->str().size() > 0;
		}

		// Check if MTR stream has any output. Useful to make sure there are or are not outputs to MTR.
		inline
		bool has_mtr_output()
		{
			return this->mtr_stream->str().size() > 0;
		}

		// Check if ECHO stream has any output. Useful to make sure there are or are not outputs to ECHO.
		inline
		bool has_echo_output()
		{
			return this->echo_stream->str().size() > 0;
		}

		// Check if COUT stream has any output. Useful to make sure there are or are not outputs to COUT.
		inline
		bool has_cout_output()
		{
			return this->m_cout_buffer->str().size() > 0;
		}

		// Check if CERR stream has any output. Useful to make sure there are or are not outputs to CERR.
		inline
		bool has_cerr_output()
		{
			return this->m_cerr_buffer->str().size() > 0;
		}

		// This function processes an idf snippet and defaults to using the idd cache for the fixture.
		// The cache should be used for nearly all calls to this function.
		// This more or less replicates InputProcessor::ProcessInput() but in a more usable fashion for unit testing
		// This calls EXPECT_* within the function as well as returns a boolean so you can call [ASSERT/EXPECT]_[TRUE/FALSE] depending
		// if it makes sense for the unit test to continue after returning from function.
		// Will return false if no errors found and true if errors found
		bool process_idf( std::string const & idf_snippet, bool use_idd_cache = true );

		// This is a helper function to easily compare an expected IDF data structure with the actual IDFRecords data structure
		// This calls EXPECT_* within the function as well as returns a boolean so you can call [ASSERT/EXPECT]_[TRUE/FALSE] depending
		// if it makes sense for the unit test to continue after returning from function.
		// Will return true if data structures match and false if they do not.
		// Usage (assuming "Version,8.3;" was parsed as an idf snippet):
		// 		EXPECT_TRUE( compare_idf( "VERSION", 1, 0, 1, { "8.3" }, { false }, {}, {} ) );
		bool compare_idf( 
			std::string const & name, 
			int const num_alphas, 
			int const num_numbers, 
			std::vector< std::string > const & alphas, 
			std::vector< bool > const & alphas_blank, 
			std::vector< Real64 > const & numbers, 
			std::vector< bool > const & numbers_blank 
		);

	private:
		friend class InputProcessorFixture;

		// Function to process the Energy+.idd, should not normally be called.
		// This will always grab the Energy+.idd that is part of the Products folder
		// This function should be called by process_idf() so unit tests can take advantage of caching
		// To test this function use InputProcessorFixture
		// This calls EXPECT_* within the function as well as returns a boolean so you can call [ASSERT/EXPECT]_[TRUE/FALSE] depending
		// if it makes sense for the unit test to continue after returning from function.
		// Will return false if no errors found and true if errors found
		static bool process_idd( std::string const & idd, bool & errors_found );

		// This sets up the InputProcessor cache specificaly to store the processed full IDD for subsequent runs.
		static void setup_cache();

		std::unique_ptr< std::ostringstream > eso_stream;
		std::unique_ptr< std::ostringstream > mtr_stream;
		std::unique_ptr< std::ostringstream > echo_stream;
		std::unique_ptr< std::ostringstream > m_cout_buffer;
		std::unique_ptr< std::ostringstream > m_cerr_buffer;
		std::unique_ptr< RedirectCout > m_redirect_cout;
		std::unique_ptr< RedirectCerr > m_redirect_cerr;
		static std::unique_ptr< InputProcessorCache > m_idd_cache;

	};

}

#endif
