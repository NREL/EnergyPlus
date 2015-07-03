#ifndef EnergyPlusFixture_hh_INCLUDED
#define EnergyPlusFixture_hh_INCLUDED

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/EnergyPlus.hh>
#include <EnergyPlus/DataStringGlobals.hh>
#include <EnergyPlus/DataSystemVariables.hh>
#include <EnergyPlus/FileSystem.hh>
#include <EnergyPlus/InputProcessor.hh>
#include <EnergyPlus/SortAndStringUtilities.hh>
#include <EnergyPlus/DataIPShortCuts.hh>

#include <fstream>
#include <memory>
#include <algorithm>

using namespace EnergyPlus;
using namespace ObjexxFCL;

namespace EnergyPlus {

	class EnergyPlusFixture : public testing::Test
	{
		struct RedirectCout {
			RedirectCout( std::unique_ptr<std::ostringstream> const & m_buffer ) 
			: m_old_buffer( std::cout.rdbuf( m_buffer->rdbuf() ) )
			{ }

			~RedirectCout( ) {
				std::cout.rdbuf( m_old_buffer.release() );
			}

			private:
				std::unique_ptr<std::streambuf> m_old_buffer;
		};

		struct RedirectCerr {
			RedirectCerr( std::unique_ptr<std::ostringstream> const & m_buffer ) 
			: m_old_buffer( std::cerr.rdbuf( m_buffer->rdbuf() ) )
			{ }

			~RedirectCerr( ) {
				std::cerr.rdbuf( m_old_buffer.release() );
			}

			private:
				std::unique_ptr<std::streambuf> m_old_buffer;
		};

	protected:
		friend class InputProcessorFixture;

		static void SetUpTestCase() { }

		static void TearDownTestCase() { }

		virtual void SetUp() {
			m_cout_buffer = std::unique_ptr<std::ostringstream>( new std::ostringstream );
			m_redirect_cout = std::unique_ptr<RedirectCout>( new RedirectCout( m_cout_buffer ) );

			m_cerr_buffer = std::unique_ptr<std::ostringstream>( new std::ostringstream );
			m_redirect_cerr = std::unique_ptr<RedirectCerr>( new RedirectCerr( m_cerr_buffer ) );
		}

		virtual void TearDown() {
			m_redirect_cout.reset();
			m_redirect_cout = nullptr;
			m_redirect_cerr.reset();
			m_redirect_cerr = nullptr;

			m_cout_buffer.reset();
			m_cout_buffer = nullptr;
			m_cerr_buffer.reset();
			m_cerr_buffer = nullptr;

			clearInputProcessorState();
		}

		// This will compare either a STL container or ObjexxFCL container
		// Pass a forward iterator of a container you want to compare against a "correct" container. You can pass in an existing 
		// container or use an initializer list like below.
		// I had to only do one template typename due to wrapping this in Google Test macros causes the macro to fail. It thinks
		// there are more macro arguments than expected. This is a way around that restriction.
		// Example Usage: 
		// 		EXPECT_TRUE( compareContainers< std::vector< bool > >( { true } , ObjectDef( index ).AlphaOrNumeric.begin() ) );
		// 		EXPECT_TRUE( compareContainers< Array1D_bool >( { true } , ObjectDef( index ).AlphaOrNumeric.begin() ) );
		template < typename T, class ForwardIterator >
		bool compareContainers( T const & correctVector, ForwardIterator test_vector_forward_iter ) {
			return std::equal( correctVector.begin(), correctVector.end(), test_vector_forward_iter );
		}

		std::string delimitedString( std::vector<std::string> const & strings, std::string const & delimiter = DataStringGlobals::NL ) {
			std::unique_ptr<std::ostringstream> compare_text(new std::ostringstream);
			for( auto const & str : strings ) {
				* compare_text << str << delimiter;
			}
			return compare_text->str();
		}

		bool compareCOUTStream( std::string const & correctString, bool resetStream = true ) {
			bool are_equal = ( correctString == this->m_cout_buffer->str() );
			if ( resetStream ) this->m_cout_buffer->str( std::string() );
			return are_equal;
		}

		bool compareCERRStream( std::string const & correctString, bool resetStream = true ) {
			bool are_equal = ( correctString == this->m_cerr_buffer->str() );
			if ( resetStream ) this->m_cerr_buffer->str( std::string() );
			return are_equal;
		}

		bool hasCoutOutput(){
			return this->m_cout_buffer->str().size() > 0;
		}

		bool hasCerrOutput(){
			return this->m_cerr_buffer->str().size() > 0;
		}

		bool processIDF( std::string const & idf, std::string const & idd = "" ) {
			using namespace InputProcessor;

			auto errors_found = false;
			processIDD( idd, errors_found );

			if ( errors_found ) return errors_found;

			auto idf_stream = std::unique_ptr<std::stringstream>( new std::stringstream( idf ) );
			NumLines = 0;
			ProcessInputDataFile( *idf_stream );

			ListOfSections.allocate( NumSectionDefs );
			ListOfSections = SectionDef( {1,NumSectionDefs} ).Name();

			DataIPShortCuts::cAlphaFieldNames.allocate( MaxAlphaIDFDefArgsFound );
			DataIPShortCuts::cAlphaArgs.allocate( MaxAlphaIDFDefArgsFound );
			DataIPShortCuts::lAlphaFieldBlanks.dimension( MaxAlphaIDFDefArgsFound, false );
			DataIPShortCuts::cNumericFieldNames.allocate( MaxNumericIDFDefArgsFound );
			DataIPShortCuts::rNumericArgs.dimension( MaxNumericIDFDefArgsFound, 0.0 );
			DataIPShortCuts::lNumericFieldBlanks.dimension( MaxNumericIDFDefArgsFound, false );

			IDFRecordsGotten.dimension( NumIDFRecords, false );

			return errors_found;
		}

	private:
		std::unique_ptr<std::ostringstream> m_cout_buffer;
		std::unique_ptr<std::ostringstream> m_cerr_buffer;
		std::unique_ptr<RedirectCout> m_redirect_cout;
		std::unique_ptr<RedirectCerr> m_redirect_cerr;

		bool processIDD( std::string const & idd, bool & errors_found ) {
			using namespace InputProcessor;

			std::unique_ptr< std::istream > idd_stream;
			if( !idd.empty() ) {
				idd_stream = std::unique_ptr<std::istringstream>( new std::istringstream( idd ) );
			} else {
				auto const exeDirectory = FileSystem::getParentDirectoryPath( FileSystem::getAbsolutePath( FileSystem::getProgramPath() ) );
				auto const idd_location = exeDirectory + "Energy+.idd";

				EXPECT_TRUE( FileSystem::fileExists( idd_location ) ) << 
					"Energy+.idd does not exist at search location." << std::endl << "IDD search location: \"" << idd_location << "\""; 

				idd_stream = std::unique_ptr<std::ifstream>( new std::ifstream( idd_location, std::ios_base::in | std::ios_base::binary ) );
			}

			if ( ! idd_stream->good() ) {
				errors_found = true;
				return errors_found;
			}

			ProcessingIDD = true;
			DataSystemVariables::SortedIDD = true;
			ProcessDataDicFile( *idd_stream, errors_found );
			ProcessingIDD = false;

			if( !errors_found ) {
				ListOfObjects.allocate( NumObjectDefs );
				ListOfObjects = ObjectDef( {1,NumObjectDefs} ).Name();
				if ( DataSystemVariables::SortedIDD ) {
					iListOfObjects.allocate( NumObjectDefs );
					SortAndStringUtilities::SetupAndSort( ListOfObjects, iListOfObjects );
				}
			}

			ObjectStartRecord.dimension( NumObjectDefs, 0 );
			ObjectGotCount.dimension( NumObjectDefs, 0 );

			return errors_found;
		}

		void clearInputProcessorState() {
			using namespace InputProcessor;

			ObjectDef.deallocate();
			SectionDef.deallocate();
			SectionsOnFile.deallocate();
			ObjectStartRecord.deallocate();
			ObjectGotCount.deallocate();
			ObsoleteObjectsRepNames.deallocate();
			ListOfSections.deallocate();
			ListOfObjects.deallocate();
			iListOfObjects.deallocate();
			IDFRecordsGotten.deallocate();
			IDFRecords.deallocate();
			RepObjects.deallocate();
			LineItem.Numbers.deallocate();
			LineItem.NumBlank.deallocate();
			LineItem.Alphas.deallocate();
			LineItem.AlphBlank.deallocate();

			DataIPShortCuts::cAlphaFieldNames.deallocate();
			DataIPShortCuts::cAlphaArgs.deallocate();
			DataIPShortCuts::lAlphaFieldBlanks.deallocate();
			DataIPShortCuts::cNumericFieldNames.deallocate();
			DataIPShortCuts::rNumericArgs.deallocate();
			DataIPShortCuts::lNumericFieldBlanks.deallocate();

			NumObjectDefs = 0;
			NumSectionDefs = 0;
			MaxObjectDefs = 0;
			MaxSectionDefs = 0;
			NumLines = 0;
			MaxIDFRecords = 0;
			NumIDFRecords = 0;
			MaxIDFSections = 0;
			NumIDFSections = 0;
			EchoInputFile = 0;
			InputLineLength = 0;
			MaxAlphaArgsFound = 0;
			MaxNumericArgsFound = 0;
			NumAlphaArgsFound = 0;
			NumNumericArgsFound = 0;
			MaxAlphaIDFArgsFound = 0;
			MaxNumericIDFArgsFound = 0;
			MaxAlphaIDFDefArgsFound = 0;
			MaxNumericIDFDefArgsFound = 0;
			NumOutOfRangeErrorsFound = 0;
			NumBlankReqFieldFound = 0;
			NumMiscErrorsFound = 0;
			MinimumNumberOfFields = 0;
			NumObsoleteObjects = 0;
			TotalAuditErrors = 0;
			NumSecretObjects = 0;
			ProcessingIDD = false;
			echo_stream = nullptr;

			InputLine = std::string();
			CurrentFieldName = std::string();
			ReplacementName = std::string();

			OverallErrorFlag = false;
			EchoInputLine = true;
			ReportRangeCheckErrors = true;
			FieldSet = false;
			RequiredField = false;
			RetainCaseFlag = false;
			ObsoleteObject = false;
			RequiredObject = false;
			UniqueObject = false;
			ExtensibleObject = false;
			ExtensibleNumFields = 0;
		}

	};

}

#endif
