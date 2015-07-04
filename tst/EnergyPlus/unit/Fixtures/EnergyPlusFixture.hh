#ifndef EnergyPlusFixture_hh_INCLUDED
#define EnergyPlusFixture_hh_INCLUDED

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/EnergyPlus.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataOutputs.hh>
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

		static void SetUpTestCase() {
			static auto errors_found = false;
			static auto const idd = "";
			process_idd( idd, errors_found );
			if ( errors_found ) {
				clear_InputProcessor_state();
				return;
			}
			m_idd_cache = std::unique_ptr< InputProcessorCache >( new InputProcessorCache );
			clear_InputProcessor_state();
		}

		static void TearDownTestCase() { }

		virtual void SetUp() {
			show_message();

			this->eso_stream = std::unique_ptr< std::ostringstream >( new std::ostringstream );
			this->mtr_stream = std::unique_ptr< std::ostringstream >( new std::ostringstream );
			this->echo_stream = std::unique_ptr< std::ostringstream >( new std::ostringstream );

			DataGlobals::eso_stream = this->eso_stream.get();
			DataGlobals::mtr_stream = this->mtr_stream.get();
			InputProcessor::echo_stream = this->echo_stream.get();

			m_cout_buffer = std::unique_ptr< std::ostringstream >( new std::ostringstream );
			m_redirect_cout = std::unique_ptr< RedirectCout >( new RedirectCout( m_cout_buffer ) );

			m_cerr_buffer = std::unique_ptr< std::ostringstream >( new std::ostringstream );
			m_redirect_cerr = std::unique_ptr< RedirectCerr >( new RedirectCerr( m_cerr_buffer ) );
		}

		virtual void TearDown() {
			clear_InputProcessor_state();
			clear_DataGlobals_state();
			clear_DataOutputs_state();
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

		// This will compare either a STL container or ObjexxFCL container
		// Pass a forward iterator of a container you want to compare against a "correct" container. You can pass in an existing 
		// container or use an initializer list like below.
		// I had to only do one template typename due to wrapping this in Google Test macros causes the macro to fail. It thinks
		// there are more macro arguments than expected. This is a way around that restriction.
		// Example Usage: 
		// 		EXPECT_TRUE( compare_containers< std::vector< bool > >( { true } , ObjectDef( index ).AlphaOrNumeric.begin() ) );
		// 		EXPECT_TRUE( compare_containers< Array1D_bool >( { true } , ObjectDef( index ).AlphaOrNumeric.begin() ) );
		template < typename T, class T2 >
		bool compare_containers( T const & correct_container, T2 test_container ) {
			bool is_valid = ( correct_container.size() == test_container.size() );
			EXPECT_TRUE( is_valid ) << "Containers are not equal size. Correct: " << correct_container.size() << " Test: " << test_container.size();
			auto it = correct_container.begin();
			auto it2 = test_container.begin();
			for ( ; it != correct_container.end(); ++it, ++it2 ) {
				// This may fail due to floating point issues...
				is_valid = ( *it == *it2 );
				EXPECT_TRUE( is_valid ) << "Correct element: \"" << *it << "\"" << std::endl << "Test element: \"" << *it2 << "\"";
			}
			return is_valid;
		}

		std::string delimited_string( std::vector<std::string> const & strings, std::string const & delimiter = DataStringGlobals::NL ) {
			std::unique_ptr<std::ostringstream> compare_text(new std::ostringstream);
			for( auto const & str : strings ) {
				* compare_text << str << delimiter;
			}
			return compare_text->str();
		}

		bool compare_eso_stream( std::string const & correctString, bool resetStream = true ) {
			auto const stream_str = this->eso_stream->str();
			EXPECT_EQ( correctString, stream_str );
			bool are_equal = ( correctString == stream_str );
			if ( resetStream ) this->eso_stream->str( std::string() );
			return are_equal;
		}

		bool compare_mtr_stream( std::string const & correctString, bool resetStream = true ) {
			auto const stream_str = this->mtr_stream->str();
			EXPECT_EQ( correctString, stream_str );
			bool are_equal = ( correctString == stream_str );
			if ( resetStream ) this->mtr_stream->str( std::string() );
			return are_equal;
		}

		bool compare_echo_stream( std::string const & correctString, bool resetStream = true ) {
			auto const stream_str = this->echo_stream->str();
			EXPECT_EQ( correctString, stream_str );
			bool are_equal = ( correctString == stream_str );
			if ( resetStream ) this->echo_stream->str( std::string() );
			return are_equal;
		}

		bool compare_cout_stream( std::string const & correctString, bool resetStream = true ) {
			auto const stream_str = this->m_cout_buffer->str();
			EXPECT_EQ( correctString, stream_str );
			bool are_equal = ( correctString == stream_str );
			if ( resetStream ) this->m_cout_buffer->str( std::string() );
			return are_equal;
		}

		bool compare_cerr_stream( std::string const & correctString, bool resetStream = true ) {
			auto const stream_str = this->m_cerr_buffer->str();
			EXPECT_EQ( correctString, stream_str );
			bool are_equal = ( correctString == stream_str );
			if ( resetStream ) this->m_cerr_buffer->str( std::string() );
			return are_equal;
		}

		bool has_cout_output(){
			return this->m_cout_buffer->str().size() > 0;
		}

		bool has_cerr_output(){
			return this->m_cerr_buffer->str().size() > 0;
		}

		bool process_idf( std::string const & idf, bool use_idd_cache = true ) {
			using namespace InputProcessor;

			auto errors_found = false;

			if ( use_idd_cache && m_idd_cache ) {
				m_idd_cache->fill_InputProcessor_global_data();
			} else {
				auto const idd = "";
				process_idd( idd, errors_found );
			}

			if ( errors_found ) return errors_found;

			auto idf_stream = std::unique_ptr<std::stringstream>( new std::stringstream( idf ) );
			NumLines = 0;
			ProcessInputDataFile( *idf_stream );

			ListOfSections.allocate( NumSectionDefs );
			ListOfSections = SectionDef( { 1, NumSectionDefs } ).Name();

			DataIPShortCuts::cAlphaFieldNames.allocate( MaxAlphaIDFDefArgsFound );
			DataIPShortCuts::cAlphaArgs.allocate( MaxAlphaIDFDefArgsFound );
			DataIPShortCuts::lAlphaFieldBlanks.dimension( MaxAlphaIDFDefArgsFound, false );
			DataIPShortCuts::cNumericFieldNames.allocate( MaxNumericIDFDefArgsFound );
			DataIPShortCuts::rNumericArgs.dimension( MaxNumericIDFDefArgsFound, 0.0 );
			DataIPShortCuts::lNumericFieldBlanks.dimension( MaxNumericIDFDefArgsFound, false );

			IDFRecordsGotten.dimension( NumIDFRecords, false );

			PreScanReportingVariables();

			return errors_found;
		}

	private:
		struct InputProcessorCache;

		std::unique_ptr< std::ostringstream > eso_stream;
		std::unique_ptr< std::ostringstream > mtr_stream;
		std::unique_ptr< std::ostringstream > echo_stream;
		std::unique_ptr< std::ostringstream > m_cout_buffer;
		std::unique_ptr< std::ostringstream > m_cerr_buffer;
		std::unique_ptr< RedirectCout > m_redirect_cout;
		std::unique_ptr< RedirectCerr > m_redirect_cerr;
		static std::unique_ptr< InputProcessorCache > m_idd_cache;

		static bool process_idd( std::string const & idd, bool & errors_found ) {
			using namespace InputProcessor;

			std::unique_ptr< std::istream > idd_stream;
			if( !idd.empty() ) {
				idd_stream = std::unique_ptr<std::istringstream>( new std::istringstream( idd ) );
			} else {
				static auto const exeDirectory = FileSystem::getParentDirectoryPath( FileSystem::getAbsolutePath( FileSystem::getProgramPath() ) );
				static auto const idd_location = exeDirectory + "Energy+.idd";
				static auto const file_exists = FileSystem::fileExists( idd_location );

				if ( ! file_exists ) {
					EXPECT_TRUE( file_exists ) << 
						"Energy+.idd does not exist at search location." << std::endl << "IDD search location: \"" << idd_location << "\"";
					errors_found = true;
					return errors_found;
				}

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

		static void clear_DataOutputs_state() {
			using namespace DataOutputs;

			MaxConsideredOutputVariables = 0;
			NumConsideredOutputVariables = 0;
			iNumberOfRecords = int();
			iNumberOfDefaultedFields = int();
			iTotalFieldsWithDefaults = int();
			iNumberOfAutoSizedFields = int();
			iTotalAutoSizableFields = int();
			iNumberOfAutoCalcedFields = int();
			iTotalAutoCalculatableFields = int();
			OutputVariablesForSimulation.deallocate();
		}

		static void clear_DataGlobals_state() {
			using namespace DataGlobals;

			runReadVars = false;
			DDOnlySimulation = false;
			AnnualSimulation = false;
			BeginDayFlag = false;
			BeginEnvrnFlag = false;
			BeginHourFlag = false;
			BeginSimFlag = false;
			BeginFullSimFlag = false;
			BeginTimeStepFlag = false;
			DayOfSim = 0;
			DayOfSimChr = "0";
			EndEnvrnFlag = false;
			EndDesignDayEnvrnsFlag = false;
			EndDayFlag = false;
			EndHourFlag = false;
			PreviousHour = 0;
			HourOfDay = 0;
			WeightPreviousHour = 0.0;
			WeightNow = 0.0;
			NumOfDayInEnvrn = 0;
			NumOfTimeStepInHour = 0;
			NumOfZones = 0;
			TimeStep = 0;
			TimeStepZone = 0.0;
			WarmupFlag = false;
			OutputFileStandard = 0;
			StdOutputRecordCount = 0;
			OutputFileInits = 0;
			OutputFileDebug = 0;
			OutputFileZoneSizing = 0;
			OutputFileSysSizing = 0;
			OutputFileMeters = 0;
			StdMeterRecordCount = 0;
			OutputFileBNDetails = 0;
			ZoneSizingCalc = false;
			SysSizingCalc = false;
			DoZoneSizing = false;
			DoSystemSizing = false;
			DoPlantSizing = false;
			DoDesDaySim = false;
			DoWeathSim = false;
			DoHVACSizingSimulation = false;
			HVACSizingSimMaxIterations = 0;
			WeathSimReq = false;
			KindOfSim = 0;
			DoOutputReporting = false;
			DoingSizing = false;
			DoingHVACSizingSimulations = false;
			DoingInputProcessing = false;
			DisplayAllWarnings = false;
			DisplayExtraWarnings = false;
			DisplayUnusedObjects = false;
			DisplayUnusedSchedules = false;
			DisplayAdvancedReportVariables = false;
			DisplayZoneAirHeatBalanceOffBalance = false;
			DisplayInputInAudit = false;
			CreateMinimalSurfaceVariables = false;
			CurrentTime = 0.0;
			SimTimeSteps = 0;
			MinutesPerTimeStep = 0;
			TimeStepZoneSec = 0.0;
			MetersHaveBeenInitialized = false;
			KickOffSimulation = false;
			KickOffSizing = false;
			RedoSizesHVACSimulation = false;
			FinalSizingHVACSizingSimIteration = false;
			AnyEnergyManagementSystemInModel = false;
			AnyPlantInModel = false;
			CacheIPErrorFile = 0;
			AnyIdealCondEntSetPointInModel = false;
			RunOptCondEntTemp = false;
			CompLoadReportIsReq = false;
			isPulseZoneSizing = false;
			OutputFileZonePulse = 0;
			doLoadComponentPulseNow = false;
			ShowDecayCurvesInEIO = false;
			AnySlabsInModel = false;
			AnyBasementsInModel = false;
			Progress = 0;
		}

		static void clear_InputProcessor_state() {
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

		struct InputProcessorCache {
			InputProcessorCache()
			{
				using namespace InputProcessor;

				m_ObjectDef = ObjectDef;
				m_SectionDef = SectionDef;
				m_SectionsOnFile = SectionsOnFile;
				m_ObjectStartRecord = ObjectStartRecord;
				m_ObjectGotCount = ObjectGotCount;
				m_ObsoleteObjectsRepNames = ObsoleteObjectsRepNames;
				m_ListOfSections = ListOfSections;
				m_ListOfObjects = ListOfObjects;
				m_iListOfObjects = iListOfObjects;
				m_IDFRecordsGotten = IDFRecordsGotten;
				m_IDFRecords = IDFRecords;
				m_RepObjects = RepObjects;
				m_LineItem = LineItem;
				m_cAlphaFieldNames = DataIPShortCuts::cAlphaFieldNames;
				m_cAlphaArgs = DataIPShortCuts::cAlphaArgs;
				m_lAlphaFieldBlanks = DataIPShortCuts::lAlphaFieldBlanks;
				m_cNumericFieldNames = DataIPShortCuts::cNumericFieldNames;
				m_rNumericArgs = DataIPShortCuts::rNumericArgs;
				m_lNumericFieldBlanks = DataIPShortCuts::lNumericFieldBlanks;
				m_NumObjectDefs = NumObjectDefs;
				m_NumSectionDefs = NumSectionDefs;
				m_MaxObjectDefs = MaxObjectDefs;
				m_MaxSectionDefs = MaxSectionDefs;
				m_NumLines = NumLines;
				m_MaxIDFRecords = MaxIDFRecords;
				m_NumIDFRecords = NumIDFRecords;
				m_MaxIDFSections = MaxIDFSections;
				m_NumIDFSections = NumIDFSections;
				m_EchoInputFile = EchoInputFile;
				m_InputLineLength = InputLineLength;
				m_MaxAlphaArgsFound = MaxAlphaArgsFound;
				m_MaxNumericArgsFound = MaxNumericArgsFound;
				m_NumAlphaArgsFound = NumAlphaArgsFound;
				m_NumNumericArgsFound = NumNumericArgsFound;
				m_MaxAlphaIDFArgsFound = MaxAlphaIDFArgsFound;
				m_MaxNumericIDFArgsFound = MaxNumericIDFArgsFound;
				m_MaxAlphaIDFDefArgsFound = MaxAlphaIDFDefArgsFound;
				m_MaxNumericIDFDefArgsFound = MaxNumericIDFDefArgsFound;
				m_NumOutOfRangeErrorsFound = NumOutOfRangeErrorsFound;
				m_NumBlankReqFieldFound = NumBlankReqFieldFound;
				m_NumMiscErrorsFound = NumMiscErrorsFound;
				m_MinimumNumberOfFields = MinimumNumberOfFields;
				m_NumObsoleteObjects = NumObsoleteObjects;
				m_TotalAuditErrors = TotalAuditErrors;
				m_NumSecretObjects = NumSecretObjects;
				m_ProcessingIDD = ProcessingIDD;
				m_InputLine = InputLine;
				m_CurrentFieldName = CurrentFieldName;
				m_ReplacementName = ReplacementName;
				m_OverallErrorFlag = OverallErrorFlag;
				m_EchoInputLine = EchoInputLine;
				m_ReportRangeCheckErrors = ReportRangeCheckErrors;
				m_FieldSet = FieldSet;
				m_RequiredField = RequiredField;
				m_RetainCaseFlag = RetainCaseFlag;
				m_ObsoleteObject = ObsoleteObject;
				m_RequiredObject = RequiredObject;
				m_UniqueObject = UniqueObject;
				m_ExtensibleObject = ExtensibleObject;
				m_ExtensibleNumFields = ExtensibleNumFields;
			}

			void fill_InputProcessor_global_data()
			{
				using namespace InputProcessor;

				ObjectDef = m_ObjectDef;
				SectionDef = m_SectionDef;
				SectionsOnFile = m_SectionsOnFile;
				ObjectStartRecord = m_ObjectStartRecord;
				ObjectGotCount = m_ObjectGotCount;
				ObsoleteObjectsRepNames = m_ObsoleteObjectsRepNames;
				ListOfSections = m_ListOfSections;
				ListOfObjects = m_ListOfObjects;
				iListOfObjects = m_iListOfObjects;
				IDFRecordsGotten = m_IDFRecordsGotten;
				IDFRecords = m_IDFRecords;
				RepObjects = m_RepObjects;
				LineItem = m_LineItem;
				DataIPShortCuts::cAlphaFieldNames = m_cAlphaFieldNames;
				DataIPShortCuts::cAlphaArgs = m_cAlphaArgs;
				DataIPShortCuts::lAlphaFieldBlanks = m_lAlphaFieldBlanks;
				DataIPShortCuts::cNumericFieldNames = m_cNumericFieldNames;
				DataIPShortCuts::rNumericArgs = m_rNumericArgs;
				DataIPShortCuts::lNumericFieldBlanks = m_lNumericFieldBlanks;
				NumObjectDefs = m_NumObjectDefs;
				NumSectionDefs = m_NumSectionDefs;
				MaxObjectDefs = m_MaxObjectDefs;
				MaxSectionDefs = m_MaxSectionDefs;
				NumLines = m_NumLines;
				MaxIDFRecords = m_MaxIDFRecords;
				NumIDFRecords = m_NumIDFRecords;
				MaxIDFSections = m_MaxIDFSections;
				NumIDFSections = m_NumIDFSections;
				EchoInputFile = m_EchoInputFile;
				InputLineLength = m_InputLineLength;
				MaxAlphaArgsFound = m_MaxAlphaArgsFound;
				MaxNumericArgsFound = m_MaxNumericArgsFound;
				NumAlphaArgsFound = m_NumAlphaArgsFound;
				NumNumericArgsFound = m_NumNumericArgsFound;
				MaxAlphaIDFArgsFound = m_MaxAlphaIDFArgsFound;
				MaxNumericIDFArgsFound = m_MaxNumericIDFArgsFound;
				MaxAlphaIDFDefArgsFound = m_MaxAlphaIDFDefArgsFound;
				MaxNumericIDFDefArgsFound = m_MaxNumericIDFDefArgsFound;
				NumOutOfRangeErrorsFound = m_NumOutOfRangeErrorsFound;
				NumBlankReqFieldFound = m_NumBlankReqFieldFound;
				NumMiscErrorsFound = m_NumMiscErrorsFound;
				MinimumNumberOfFields = m_MinimumNumberOfFields;
				NumObsoleteObjects = m_NumObsoleteObjects;
				TotalAuditErrors = m_TotalAuditErrors;
				NumSecretObjects = m_NumSecretObjects;
				ProcessingIDD = m_ProcessingIDD;
				InputLine = m_InputLine;
				CurrentFieldName = m_CurrentFieldName;
				ReplacementName = m_ReplacementName;
				OverallErrorFlag = m_OverallErrorFlag;
				EchoInputLine = m_EchoInputLine;
				ReportRangeCheckErrors = m_ReportRangeCheckErrors;
				FieldSet = m_FieldSet;
				RequiredField = m_RequiredField;
				RetainCaseFlag = m_RetainCaseFlag;
				ObsoleteObject = m_ObsoleteObject;
				RequiredObject = m_RequiredObject;
				UniqueObject = m_UniqueObject;
				ExtensibleObject = m_ExtensibleObject;
				ExtensibleNumFields = m_ExtensibleNumFields;
			}

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

	};

	typedef EnergyPlusFixture EnergyPlusDeathTestFixture;

}

#endif
