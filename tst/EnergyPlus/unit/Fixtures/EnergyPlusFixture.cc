// Google Test Headers
#include <gtest/gtest.h>

// ObjexxFCL Headers
#include <ObjexxFCL/gio.hh>

// EnergyPlus Headers
#include "EnergyPlusFixture.hh"
#include "../TestHelpers/IdfParser.hh"
#include <EnergyPlus/CurveManager.hh>
#include <EnergyPlus/DataEnvironment.hh>
#include <EnergyPlus/DataGlobals.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataIPShortCuts.hh>
#include <EnergyPlus/DataOutputs.hh>
#include <EnergyPlus/DataSurfaces.hh>
#include <EnergyPlus/DataZoneControls.hh>
#include <EnergyPlus/ExteriorEnergyUse.hh>
#include <EnergyPlus/HeatBalanceManager.hh>
#include <EnergyPlus/InputProcessor.hh>
#include <EnergyPlus/OutputProcessor.hh>
#include <EnergyPlus/ScheduleManager.hh>

#include <EnergyPlus/DataSystemVariables.hh>
#include <EnergyPlus/FileSystem.hh>
#include <EnergyPlus/SortAndStringUtilities.hh>
#include <EnergyPlus/DataAirLoop.hh>
#include <EnergyPlus/DataBranchNodeConnections.hh>
#include <EnergyPlus/DataHVACGlobals.hh>
#include <EnergyPlus/DataLoopNode.hh>
#include <EnergyPlus/DataSizing.hh>
#include <EnergyPlus/Humidifiers.hh>
#include <EnergyPlus/MixedAir.hh>
#include <EnergyPlus/NodeInputManager.hh>
#include <EnergyPlus/OutAirNodeManager.hh>
#include <EnergyPlus/Psychrometrics.hh>
#include <EnergyPlus/DataDefineEquip.hh>
#include <EnergyPlus/BranchNodeConnections.hh>
#include <EnergyPlus/DataZoneEquipment.hh>
#include <EnergyPlus/DataHeatBalance.hh>
#include <EnergyPlus/DataPlant.hh>
#include <EnergyPlus/GlobalNames.hh>
#include <EnergyPlus/PlantManager.hh>
#include <EnergyPlus/SetPointManager.hh>
#include <EnergyPlus/OutsideEnergySources.hh>
#include <EnergyPlus/HVACManager.hh>
#include <EnergyPlus/BranchInputManager.hh>
#include <EnergyPlus/SimulationManager.hh>

#include <EnergyPlus/DataConvergParams.hh>
#include <EnergyPlus/SizingManager.hh>
#include <EnergyPlus/WeatherManager.hh>
#include <EnergyPlus/OutputReportPredefined.hh>

#include <fstream>
#include <algorithm>

struct InputProcessorCache;
std::unique_ptr<EnergyPlus::InputProcessorCache> EnergyPlus::EnergyPlusFixture::m_idd_cache = nullptr;

namespace EnergyPlus {

	void EnergyPlusFixture::SetUp() {
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

	void EnergyPlusFixture::TearDown() {
		CurveManager::clear_state();
		DataEnvironment::clear_state();
		DataGlobals::clear_state();
		DataHeatBalance::clear_state();
		DataIPShortCuts::clear_state();
		DataOutputs::clear_state();
		DataSurfaces::clear_state();
		DataZoneControls::clear_state();
		ExteriorEnergyUse::clear_state();
		HeatBalanceManager::clear_state();
		InputProcessor::clear_state();
		OutputProcessor::clear_state();
		ScheduleManager::clear_state();
		DataAirLoop::clear_state();
		DataBranchNodeConnections::clear_state();
		DataHVACGlobals::clear_state();
		DataLoopNode::clear_state();
		DataSizing::clear_state();
		Humidifiers::clear_state();
		MixedAir::clear_state();
		NodeInputManager::clear_state();
		OutAirNodeManager::clear_state();
		DataDefineEquip::clear_state();
		DataZoneEquipment::clear_state();
		DataPlant::clear_state();
		GlobalNames::clear_state();
		SetPointManager::clear_state();
		OutsideEnergySources::clear_state();
		HVACManager::clear_state();
		BranchInputManager::clear_state();
		SimulationManager::clear_state();
		SizingManager::clear_state();
		DataConvergParams::clear_state();
		WeatherManager::clear_state();
		OutputReportPredefined::clear_state();

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

	void EnergyPlusFixture::setup_cache()
	{
		if ( ! m_idd_cache ) {
			static auto errors_found = false;
			static auto const idd = "";
			InputProcessor::clear_state();
			process_idd( idd, errors_found );
			if ( errors_found ) {
				InputProcessor::clear_state();
				return;
			}
			m_idd_cache = std::unique_ptr< InputProcessorCache >( new InputProcessorCache );
			InputProcessor::clear_state();
		}
	}

	void EnergyPlusFixture::use_cached_idd()
	{
		setup_cache();
		if ( m_idd_cache ) {
			m_idd_cache->use_cached_namespace_variables();
		}
	}

	template < typename T, class T2 >
	bool EnergyPlusFixture::compare_containers( T const & expected_container, T2 const & actual_container ) {
		bool is_valid = ( expected_container.size() == actual_container.size() );
		EXPECT_EQ( expected_container.size(), actual_container.size() ) << "Containers are not equal size.";
		auto expected = expected_container.begin();
		auto actual = actual_container.begin();
		for ( ; expected != expected_container.end(); ++expected, ++actual ) {
			// This may fail due to floating point issues for float and double...
			EXPECT_EQ( *expected, *actual ) << "Incorrect 0-based index: " << ( expected - expected_container.begin() );
			is_valid = ( *expected == *actual );
		}
		return is_valid;
	}

	std::string EnergyPlusFixture::delimited_string( std::vector<std::string> const & strings, std::string const & delimiter ) {
		std::ostringstream compare_text;
		for( auto const & str : strings ) {
			compare_text << str << delimiter;
		}
		return compare_text.str();
	}

	bool EnergyPlusFixture::compare_eso_stream( std::string const & expected_string, bool reset_stream ) {
		auto const stream_str = this->eso_stream->str();
		EXPECT_EQ( expected_string, stream_str );
		bool are_equal = ( expected_string == stream_str );
		if ( reset_stream ) this->eso_stream->str( std::string() );
		return are_equal;
	}

	bool EnergyPlusFixture::compare_mtr_stream( std::string const & expected_string, bool reset_stream ) {
		auto const stream_str = this->mtr_stream->str();
		EXPECT_EQ( expected_string, stream_str );
		bool are_equal = ( expected_string == stream_str );
		if ( reset_stream ) this->mtr_stream->str( std::string() );
		return are_equal;
	}

	bool EnergyPlusFixture::compare_echo_stream( std::string const & expected_string, bool reset_stream ) {
		auto const stream_str = this->echo_stream->str();
		EXPECT_EQ( expected_string, stream_str );
		bool are_equal = ( expected_string == stream_str );
		if ( reset_stream ) this->echo_stream->str( std::string() );
		return are_equal;
	}

	bool EnergyPlusFixture::compare_cout_stream( std::string const & expected_string, bool reset_stream ) {
		auto const stream_str = this->m_cout_buffer->str();
		EXPECT_EQ( expected_string, stream_str );
		bool are_equal = ( expected_string == stream_str );
		if ( reset_stream ) this->m_cout_buffer->str( std::string() );
		return are_equal;
	}

	bool EnergyPlusFixture::compare_cerr_stream( std::string const & expected_string, bool reset_stream ) {
		auto const stream_str = this->m_cerr_buffer->str();
		EXPECT_EQ( expected_string, stream_str );
		bool are_equal = ( expected_string == stream_str );
		if ( reset_stream ) this->m_cerr_buffer->str( std::string() );
		return are_equal;
	}

	bool EnergyPlusFixture::process_idf( std::string const & idf_snippet, bool use_idd_cache ) {
		using namespace InputProcessor;

		// Parse idf snippet to look for Building and GlobalGeometryRules. If not present then this adds a default implementation
		// otherwise it will use the objects in the snippet. This is done because there is a check for required objects.
		// Right now, IdfParser::decode returns a very naive data structure for objects but it works for this purpose.
		IdfParser parser;
		bool success = false;
		auto const parsed_idf = parser.decode( idf_snippet, success );
		EXPECT_TRUE( success ) << "IDF snippet didn't parse properly. Assuming Building and GlobalGeometryRules are not in snippet.";
		bool found_building = false;
		bool found_global_geo = false;
		if ( success ) {
			for ( auto const obj : parsed_idf ) {
				if ( ! obj.empty() ) {
					if ( SameString( obj[ 0 ], "Building" ) ) {
						found_building = true;
					}
					if ( SameString( obj[ 0 ], "GlobalGeometryRules" ) ) {
						found_global_geo = true;
					}
					if ( found_building && found_global_geo ) break;
				}
			}
		}
		std::string idf = idf_snippet;
		if ( ! found_building ) {
			idf += "Building,Bldg,0.0,Suburbs,.04,.4,FullExterior,25,6;" + DataStringGlobals::NL;
		}
		if ( ! found_global_geo ) {
			idf += "GlobalGeometryRules,UpperLeftCorner,Counterclockwise,Relative;" + DataStringGlobals::NL;
		}

		auto errors_found = false;

		if ( use_idd_cache ) {
			use_cached_idd();
		} else {
			auto const idd = "";
			process_idd( idd, errors_found );
		}

		if ( errors_found ) return errors_found;

		auto idf_stream = std::unique_ptr<std::stringstream>( new std::stringstream( idf ) );
		NumLines = 0;
		InitSecretObjects();
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

		int count_err = 0;
		std::string error_string;
		for ( int loop = 1; loop <= NumIDFSections; ++loop ) {
			if ( SectionsOnFile( loop ).LastRecord != 0 ) continue;
			if ( equali( SectionsOnFile( loop ).Name, "REPORT VARIABLE DICTIONARY" ) ) continue;
			if ( count_err == 0 ) {
				error_string += " Potential errors in IDF processing:" + DataStringGlobals::NL;
			}
			++count_err;
			int which = SectionsOnFile( loop ).FirstRecord;
			if ( which > 0 ) {
				int num_1 = 0;
				if ( DataSystemVariables::SortedIDD ) {
					num_1 = FindItemInSortedList( IDFRecords( which ).Name, ListOfObjects, NumObjectDefs );
					if ( num_1 != 0 ) num_1 = iListOfObjects( num_1 );
				} else {
					num_1 = FindItemInList( IDFRecords( which ).Name, ListOfObjects, NumObjectDefs );
				}
				if ( ObjectDef( num_1 ).NameAlpha1 && IDFRecords( which ).NumAlphas > 0 ) {
					error_string += " Potential \"semi-colon\" misplacement=" + SectionsOnFile( loop ).Name + 
									", at about line number=[" + IPTrimSigDigits( SectionsOnFile( loop ).FirstLineNo ) + 
									"], Object Type Preceding=" + IDFRecords( which ).Name + ", Object Name=" + IDFRecords( which ).Alphas( 1 ) + DataStringGlobals::NL;
				} else {
					error_string += " Potential \"semi-colon\" misplacement=" + SectionsOnFile( loop ).Name + 
									", at about line number=[" + IPTrimSigDigits( SectionsOnFile( loop ).FirstLineNo ) + 
									"], Object Type Preceding=" + IDFRecords( which ).Name + ", Name field not recorded for Object." + DataStringGlobals::NL;
				}
			} else {
				error_string += " Potential \"semi-colon\" misplacement=" + SectionsOnFile( loop ).Name + 
								", at about line number=[" + IPTrimSigDigits( SectionsOnFile( loop ).FirstLineNo ) + 
								"], No prior Objects." + DataStringGlobals::NL;
			}
		}
		EXPECT_EQ( 0, count_err ) << error_string;

		if ( NumIDFRecords == 0 ) {
			EXPECT_GT( NumIDFRecords, 0 ) << "The IDF file has no records.";
			++NumMiscErrorsFound;
			errors_found = true;
		}

		for ( auto const obj_def : ObjectDef ) {
			if ( ! obj_def.RequiredObject ) continue;
			if ( obj_def.NumFound > 0 ) continue;
			EXPECT_GT( obj_def.NumFound, 0 ) << "Required Object=\"" + obj_def.Name + "\" not found in IDF.";
			++NumMiscErrorsFound;
			errors_found = true;
		}

		if ( TotalAuditErrors > 0 ) {
			EXPECT_EQ( 0, TotalAuditErrors ) << "Note -- Some missing fields have been filled with defaults.";
			errors_found = true;
		}

		if ( NumOutOfRangeErrorsFound > 0 ) {
			EXPECT_EQ( 0, NumOutOfRangeErrorsFound ) << "Out of \"range\" values found in input";
			errors_found = true;
		}

		if ( NumBlankReqFieldFound > 0 ) {
			EXPECT_EQ( 0, NumBlankReqFieldFound ) << "Blank \"required\" fields found in input";
			errors_found = true;
		}

		if ( NumMiscErrorsFound > 0 ) {
			EXPECT_EQ( 0, NumMiscErrorsFound ) << "Other miscellaneous errors found in input";
			errors_found = true;
		}

		if ( OverallErrorFlag ) {
			EXPECT_FALSE( OverallErrorFlag ) << "Error processing IDF snippet.";

			// check if IDF version matches IDD version
			// this really shouldn't be an issue but i'm keeping it just in case a unit test is written against a specific IDF version
			// This fixture will always use the most up to date version of the IDD regardless.
			bool found_version = false;
			for ( auto const idf_record : IDFRecords ) {
				if ( "VERSION" == idf_record.Name ) {
					bool bad_version = false;
					auto const version_length( len( DataStringGlobals::MatchVersion ) );
					if ( ( version_length > 0 ) && ( DataStringGlobals::MatchVersion[ version_length - 1 ] == '0' ) ) {
						bad_version = ( DataStringGlobals::MatchVersion.substr( 0, version_length - 2 ) == idf_record.Alphas( 1 ).substr( 0, version_length - 2 ) );
					} else {
						bad_version = ( DataStringGlobals::MatchVersion == idf_record.Alphas( 1 ) );
					}
					found_version = true;
					EXPECT_FALSE( bad_version ) << "Version in IDF=\"" + idf_record.Alphas( 1 ) + "\" not the same as expected=\"" + DataStringGlobals::MatchVersion + "\"";
					break;
				}
			}
			EXPECT_TRUE( found_version ) << "Unknown IDF Version, expected version is \"" + DataStringGlobals::MatchVersion + "\"";
			errors_found = true;
		}

		if ( errors_found ) return errors_found;

		PreScanReportingVariables();

		return errors_found;
	}

	bool EnergyPlusFixture::process_idd( std::string const & idd, bool & errors_found ) {
		using namespace InputProcessor;

		std::unique_ptr< std::istream > idd_stream;
		if( !idd.empty() ) {
			idd_stream = std::unique_ptr<std::istringstream>( new std::istringstream( idd ) );
		} else {
			static auto const exeDirectory = FileSystem::getParentDirectoryPath( FileSystem::getAbsolutePath( FileSystem::getProgramPath() ) );
			static auto idd_location = exeDirectory + "Energy+.idd";
			static auto file_exists = FileSystem::fileExists( idd_location );

			if ( ! file_exists ) {
				// Energy+.idd is in parent Products folder instead of Debug/Release/RelWithDebInfo/MinSizeRel folder of exe
				idd_location = FileSystem::getParentDirectoryPath( exeDirectory ) + "Energy+.idd";
				file_exists = FileSystem::fileExists( idd_location );
			}

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

			ObjectStartRecord.dimension( NumObjectDefs, 0 );
			ObjectGotCount.dimension( NumObjectDefs, 0 );
		}

		return errors_found;
	}

	bool EnergyPlusFixture::compare_idf( 
		std::string const & name, 
		int const num_alphas, 
		int const num_numbers, 
		std::vector< std::string > const & alphas, 
		std::vector< bool > const & alphas_blank, 
		std::vector< Real64 > const & numbers, 
		std::vector< bool > const & numbers_blank 
	)
	{
		using namespace InputProcessor;

		bool has_error = OverallErrorFlag;

		EXPECT_FALSE( OverallErrorFlag );

		auto index = FindItemInSortedList( name, ListOfObjects, NumObjectDefs );

		EXPECT_GT( index, 0 ) << "Could not find \"" << name << "\". Make sure to run process_idf first.";
		if ( index < 1 ) return false;
		
		index = iListOfObjects( index );
		index = ObjectStartRecord( index );

		EXPECT_EQ( name, IDFRecords( index ).Name );
		if ( name != IDFRecords( index ).Name ) has_error = true;
		EXPECT_EQ( num_alphas, IDFRecords( index ).NumAlphas );
		if ( num_alphas != IDFRecords( index ).NumAlphas ) has_error = true;
		EXPECT_EQ( num_numbers, IDFRecords( index ).NumNumbers );
		if ( num_numbers != IDFRecords( index ).NumNumbers ) has_error = true;
		if ( ! compare_containers( alphas, IDFRecords( index ).Alphas ) ) has_error = true;
		if ( ! compare_containers( alphas_blank, IDFRecords( index ).AlphBlank ) ) has_error = true;
		if ( ! compare_containers( numbers, IDFRecords( index ).Numbers ) ) has_error = true;
		if ( ! compare_containers( numbers_blank, IDFRecords( index ).NumBlank ) ) has_error = true;

		return ! has_error;
	}

	InputProcessorCache::InputProcessorCache()
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

	void InputProcessorCache::use_cached_namespace_variables()
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

}
