#ifndef EnergyPlusFixture_hh_INCLUDED
#define EnergyPlusFixture_hh_INCLUDED

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include <EnergyPlus/EnergyPlus.hh>
#include <EnergyPlus/DataEnvironment.hh>
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

namespace EnergyPlus {

	class EnergyPlusFixture : public testing::Test
	{
	private:
		friend class IdfParserFixture;
		struct RedirectCout;
		struct RedirectCerr;
		struct InputProcessorCache;
		class IdfParser;

	protected:
		friend class InputProcessorFixture;
		friend class EnergyPlusMetaFixture;

		// This is run once per fixture (either this one or any derived fixtures) for all tests that use that fixture.
		// This implements caching for the IDD state. This means every unit test can use the cached IDD state instead 
		// of reading the full IDD again.
		// One thing is that the IDD is parsed for caching for any Fixture that inherits from EnergyPlusFixture as well.
		// This happens by default IF static void SetUpTestCase() {} is not part of the derived class. If
		// that code is in the derived class then it will not initialize the m_idd_cache. This is important to know 
		// because if you need to parse an IDF then you need to add EnergyPlusFixture::SetUpTestCase(); to that
		// function like so: 
		// 		static void SetUpTestCase() {  EnergyPlusFixture::SetUpTestCase(); }
		static void SetUpTestCase() {
			static auto errors_found = false;
			static auto const idd = "";
			clear_InputProcessor_state();
			process_idd( idd, errors_found );
			if ( errors_found ) {
				clear_InputProcessor_state();
				return;
			}
			m_idd_cache = std::unique_ptr< InputProcessorCache >( new InputProcessorCache );
			clear_InputProcessor_state();
		}

		static void TearDownTestCase() { }

		// This is run every unit test for this fixture.
		// It sets up the various streams as unique_ptrs for proper memory management and lifetime.
		// It also calls show_message every unit test to output a begin message to the error file.
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

		// This is run every unit test and makes sure to clear all state in global variables this fixture touches.
		virtual void TearDown() {
			clear_InputProcessor_state();
			clear_DataEnvironment_state();
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
		// Pass a forward iterator of a container you want to compare against an expected container. You can pass in an existing 
		// container or use an initializer list like below.
		// I had to only do one template typename due to wrapping this in Google Test macros causes the macro to fail. It thinks
		// there are more macro arguments than expected. This is a way around that restriction.
		// Example Usage: 
		// 		EXPECT_TRUE( compare_containers< std::vector< bool > >( { true } , ObjectDef( index ).AlphaOrNumeric.begin() ) );
		// 		EXPECT_TRUE( compare_containers< Array1D_bool >( { true } , ObjectDef( index ).AlphaOrNumeric.begin() ) );
		template < typename T, class T2 >
		bool compare_containers( T const & expected_container, T2 actual_container ) {
			bool is_valid = ( expected_container.size() == actual_container.size() );
			// EXPECT_TRUE( expected_container.size() == actual_container.size() ) << "Containers are not equal size. Expected: " << expected_container.size() << " Actual: " << actual_container.size();
			EXPECT_EQ( expected_container.size(), actual_container.size() ) << "Containers are not equal size.";
			auto expected = expected_container.begin();
			auto actual = actual_container.begin();
			for ( ; expected != expected_container.end(); ++expected, ++actual ) {
				// This may fail due to floating point issues for float and double...
				EXPECT_EQ( *expected, *actual );
				is_valid = ( *expected == *actual );
			}
			return is_valid;
		}

		// This function creates a string based on a vector of string inputs that is delimited by DataStringGlobals::NL by default, but any 
		// delimiter can be passed in to this funciton. This allows for cross platform output string comparisons.
		std::string delimited_string( std::vector<std::string> const & strings, std::string const & delimiter = DataStringGlobals::NL ) {
			std::unique_ptr<std::ostringstream> compare_text(new std::ostringstream);
			for( auto const & str : strings ) {
				* compare_text << str << delimiter;
			}
			return compare_text->str();
		}

		// Compare an expected string against the ESO stream. The default is to reset the ESO stream after every call.
		// It is easier to test successive functions if the ESO stream is 'empty' before the next call.
		bool compare_eso_stream( std::string const & expected_string, bool reset_stream = true ) {
			auto const stream_str = this->eso_stream->str();
			EXPECT_EQ( expected_string, stream_str );
			bool are_equal = ( expected_string == stream_str );
			if ( reset_stream ) this->eso_stream->str( std::string() );
			return are_equal;
		}

		// Compare an expected string against the MTR stream. The default is to reset the MTR stream after every call.
		// It is easier to test successive functions if the MTR stream is 'empty' before the next call.
		bool compare_mtr_stream( std::string const & expected_string, bool reset_stream = true ) {
			auto const stream_str = this->mtr_stream->str();
			EXPECT_EQ( expected_string, stream_str );
			bool are_equal = ( expected_string == stream_str );
			if ( reset_stream ) this->mtr_stream->str( std::string() );
			return are_equal;
		}

		// Compare an expected string against the ECHO stream. The default is to reset the ECHO stream after every call.
		// It is easier to test successive functions if the ECHO stream is 'empty' before the next call.
		bool compare_echo_stream( std::string const & expected_string, bool reset_stream = true ) {
			auto const stream_str = this->echo_stream->str();
			EXPECT_EQ( expected_string, stream_str );
			bool are_equal = ( expected_string == stream_str );
			if ( reset_stream ) this->echo_stream->str( std::string() );
			return are_equal;
		}

		// Compare an expected string against the COUT stream. The default is to reset the COUT stream after every call.
		// It is easier to test successive functions if the COUT stream is 'empty' before the next call.
		bool compare_cout_stream( std::string const & expected_string, bool reset_stream = true ) {
			auto const stream_str = this->m_cout_buffer->str();
			EXPECT_EQ( expected_string, stream_str );
			bool are_equal = ( expected_string == stream_str );
			if ( reset_stream ) this->m_cout_buffer->str( std::string() );
			return are_equal;
		}

		// Compare an expected string against the CERR stream. The default is to reset the CERR stream after every call.
		// It is easier to test successive functions if the CERR stream is 'empty' before the next call.
		bool compare_cerr_stream( std::string const & expected_string, bool reset_stream = true ) {
			auto const stream_str = this->m_cerr_buffer->str();
			EXPECT_EQ( expected_string, stream_str );
			bool are_equal = ( expected_string == stream_str );
			if ( reset_stream ) this->m_cerr_buffer->str( std::string() );
			return are_equal;
		}

		// Check if ESO stream has any output. Useful to make sure there are or are not outputs to ESO.
		bool has_eso_output() {
			return this->eso_stream->str().size() > 0;
		}

		// Check if MTR stream has any output. Useful to make sure there are or are not outputs to MTR.
		bool has_mtr_output() {
			return this->mtr_stream->str().size() > 0;
		}

		// Check if ECHO stream has any output. Useful to make sure there are or are not outputs to ECHO.
		bool has_echo_output() {
			return this->echo_stream->str().size() > 0;
		}

		// Check if COUT stream has any output. Useful to make sure there are or are not outputs to COUT.
		bool has_cout_output() {
			return this->m_cout_buffer->str().size() > 0;
		}

		// Check if CERR stream has any output. Useful to make sure there are or are not outputs to CERR.
		bool has_cerr_output() {
			return this->m_cerr_buffer->str().size() > 0;
		}

		int check_for_object( std::string const & object_name, std::string const & idf_snippet ) {
			auto idf_size = idf_snippet.size();
			auto object_size = object_name.size();
			if ( object_name.empty() ) return 0;
			size_t index = 0;
			int object_count = 0;

			while ( index < idf_size )
			{
				if ( toupper( idf_snippet[ index ] ) == toupper( object_name[ 0 ] ) && ( index + object_size < idf_size ) ) {
					bool is_valid = true;
					for ( size_t i = 0; i < object_size; i++ ) {
						if ( toupper( idf_snippet[ i + index ] ) != toupper( object_name[ i ] ) ) {
							index += i;
							is_valid = false;
							break;
						}
					}
					if ( is_valid ) {
						index += object_size;
						for ( ; index < idf_size; index++ ) {
							if ( idf_snippet[ index ] == ' ' ) {
								continue;
							} else if ( idf_snippet[ index ] == ',' ) {
								object_count++;
								break;
							} else {
								break;
							}
						}
					}
				}
				++index;
			}

			return object_count;
		}

		bool process_idf( std::string const & idf_snippet, bool use_idd_cache = true ) {
			using namespace InputProcessor;

			std::string idf = idf_snippet;

			IdfParser parser;
			bool success = true;
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
			if ( ! found_building ) {
				idf += "Building, Bldg, 0.0, Suburbs, .04, .4, FullExterior, 25, 6;" + DataStringGlobals::NL;
			}
			if ( ! found_global_geo ) {
				idf += "GlobalGeometryRules, UpperLeftCorner, Counterclockwise, Relative;" + DataStringGlobals::NL;
			}

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

			// CountErr = 0;
			// for ( Loop = 1; Loop <= NumIDFSections; ++Loop ) {
			// 	if ( SectionsOnFile( Loop ).LastRecord != 0 ) continue;
			// 	if ( equali( SectionsOnFile( Loop ).Name, "REPORT VARIABLE DICTIONARY" ) ) continue;
			// 	if ( CountErr == 0 ) {
			// 		ShowSevereError( "IP: Potential errors in IDF processing -- see .audit file for details." );
			// 		gio::write( EchoInputFile, fmtA ) << " Potential errors in IDF processing:";
			// 	}
			// 	++CountErr;
			// 	Which = SectionsOnFile( Loop ).FirstRecord;
			// 	if ( Which > 0 ) {
			// 		if ( SortedIDD ) {
			// 			Num1 = FindItemInSortedList( IDFRecords( Which ).Name, ListOfObjects, NumObjectDefs );
			// 			if ( Num1 != 0 ) Num1 = iListOfObjects( Num1 );
			// 		} else {
			// 			Num1 = FindItemInList( IDFRecords( Which ).Name, ListOfObjects, NumObjectDefs );
			// 		}
			// 		if ( ObjectDef( Num1 ).NameAlpha1 && IDFRecords( Which ).NumAlphas > 0 ) {
			// 			gio::write( EchoInputFile, fmtA ) << " Potential \"semi-colon\" misplacement=" + SectionsOnFile( Loop ).Name + ", at about line number=[" + IPTrimSigDigits( SectionsOnFile( Loop ).FirstLineNo ) + "], Object Type Preceding=" + IDFRecords( Which ).Name + ", Object Name=" + IDFRecords( Which ).Alphas( 1 );
			// 		} else {
			// 			gio::write( EchoInputFile, fmtA ) << " Potential \"semi-colon\" misplacement=" + SectionsOnFile( Loop ).Name + ", at about line number=[" + IPTrimSigDigits( SectionsOnFile( Loop ).FirstLineNo ) + "], Object Type Preceding=" + IDFRecords( Which ).Name + ", Name field not recorded for Object.";
			// 		}
			// 	} else {
			// 		gio::write( EchoInputFile, fmtA ) << " Potential \"semi-colon\" misplacement=" + SectionsOnFile( Loop ).Name + ", at about line number=[" + IPTrimSigDigits( SectionsOnFile( Loop ).FirstLineNo ) + "], No prior Objects.";
			// 	}
			// }

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
				EXPECT_GT( TotalAuditErrors, 0 ) << "Note -- Some missing fields have been filled with defaults.";
				errors_found = true;
			}

			if ( NumOutOfRangeErrorsFound > 0 ) {
				EXPECT_GT( NumOutOfRangeErrorsFound, 0 ) << "Out of \"range\" values found in input";
				errors_found = true;
			}

			if ( NumBlankReqFieldFound > 0 ) {
				EXPECT_GT( NumBlankReqFieldFound, 0 ) << "Blank \"required\" fields found in input";
				errors_found = true;
			}

			if ( NumMiscErrorsFound > 0 ) {
				EXPECT_GT( NumMiscErrorsFound, 0 ) << "Other miscellaneous errors found in input";
				errors_found = true;
			}

			if ( OverallErrorFlag ) {
				EXPECT_FALSE( OverallErrorFlag ) << "Error processing IDF snippet.";

				// check if IDF version matches IDD version
				// this really shouldn't be an issue but i'm keeping it just in case a unit test is written against a specific IDF version
				// This fixture will always use the most up to date version of the IDD regardless.
				for ( auto const idf_record : IDFRecords ) {
					if ( "VERSION" == idf_record.Name ) {
						auto const version_length( len( DataStringGlobals::MatchVersion ) );
						if ( ( version_length > 0 ) && ( DataStringGlobals::MatchVersion[ version_length - 1 ] == '0' ) ) {
							EXPECT_EQ( DataStringGlobals::MatchVersion.substr( 0, version_length - 2 ), idf_record.Alphas( 1 ).substr( 0, version_length - 2 ) ) << "Version in IDF=\"" + idf_record.Alphas( 1 ) + "\" not the same as expected=\"" + DataStringGlobals::MatchVersion + "\"";
						} else {
							EXPECT_EQ( DataStringGlobals::MatchVersion, idf_record.Alphas( 1 ) ) << "Version in IDF=\"" + idf_record.Alphas( 1 ) + "\" not the same as expected=\"" + DataStringGlobals::MatchVersion + "\"";
						}
						break;
					}
				}

				errors_found = true;
			}

			if ( errors_found ) return errors_found;

			PreScanReportingVariables();

			return errors_found;
		}

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
			using namespace InputProcessor;

			EXPECT_FALSE( OverallErrorFlag );

			auto index = FindItemInSortedList( name, ListOfObjects, NumObjectDefs );

			EXPECT_GT( index, 0 ) << "Could not find \"" << name << "\". Make sure to run process_idf first.";
			if ( index < 1 ) return false;
			
			index = iListOfObjects( index );
			index = ObjectStartRecord( index );

			EXPECT_EQ( name, IDFRecords( index ).Name );
			EXPECT_EQ( num_alphas, IDFRecords( index ).NumAlphas );
			EXPECT_EQ( num_numbers, IDFRecords( index ).NumNumbers );
			EXPECT_EQ( object_def_ptr, IDFRecords( index ).ObjectDefPtr );
			EXPECT_TRUE( compare_containers< std::vector< std::string > >( alphas, IDFRecords( index ).Alphas ) );
			EXPECT_TRUE( compare_containers< std::vector< bool > >( alphas_blank, IDFRecords( index ).AlphBlank ) );
			EXPECT_TRUE( compare_containers< std::vector< Real64 > >( numbers, IDFRecords( index ).Numbers ) );
			EXPECT_TRUE( compare_containers< std::vector< bool > >( numbers_blank, IDFRecords( index ).NumBlank ) );

			return OverallErrorFlag && HasFailure();
		}

	private:
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

		static void clear_DataEnvironment_state() {
			using namespace DataEnvironment;

			BeamSolarRad = Real64();
			EMSBeamSolarRadOverrideOn = false;
			EMSBeamSolarRadOverrideValue = Real64();
			DayOfMonth = int();
			DayOfMonthTomorrow = int();
			DayOfWeek = int();
			DayOfWeekTomorrow = int();
			DayOfYear = int();
			DayOfYear_Schedule = int();
			DifSolarRad = Real64();
			EMSDifSolarRadOverrideOn = false;
			EMSDifSolarRadOverrideValue = Real64();
			DSTIndicator = int();
			Elevation = Real64();
			EndMonthFlag = bool();
			GndReflectanceForDayltg = Real64();
			GndReflectance = Real64();
			GndSolarRad = Real64();
			GroundTemp = Real64();
			GroundTempKelvin = Real64();
			GroundTempFC = Real64();
			GroundTemp_Surface = Real64();
			GroundTemp_Deep = Real64();
			PubGroundTempSurface = Array1D< Real64 >( 12 );
			PubGroundTempSurfFlag = bool();
			HolidayIndex = int();
			HolidayIndexTomorrow = int();
			IsRain = bool();
			IsSnow = bool();
			Latitude = Real64();
			Longitude = Real64();
			Month = int();
			MonthTomorrow = int();
			OutBaroPress = Real64();
			OutDryBulbTemp = Real64();
			EMSOutDryBulbOverrideOn = false;
			EMSOutDryBulbOverrideValue = Real64();
			OutHumRat = Real64();
			OutRelHum = Real64();
			OutRelHumValue = Real64();
			EMSOutRelHumOverrideOn = false;
			EMSOutRelHumOverrideValue = Real64();
			OutEnthalpy = Real64();
			OutAirDensity = Real64();
			OutWetBulbTemp = Real64();
			OutDewPointTemp = Real64();
			EMSOutDewPointTempOverrideOn = false;
			EMSOutDewPointTempOverrideValue = Real64();
			SkyTemp = Real64();
			SkyTempKelvin = Real64();
			LiquidPrecipitation = Real64();
			SunIsUp = bool();
			WindDir = Real64();
			EMSWindDirOverrideOn = false;
			EMSWindDirOverrideValue = Real64();
			WindSpeed = Real64();
			EMSWindSpeedOverrideOn = false;
			EMSWindSpeedOverrideValue = Real64();
			WaterMainsTemp = Real64();
			Year = int();
			YearTomorrow = int();
			SOLCOS = Array1D< Real64 >( 3 );
			CloudFraction = Real64();
			HISKF = Real64();
			HISUNF = Real64();
			HISUNFnorm = Real64();
			PDIRLW = Real64();
			PDIFLW = Real64();
			SkyClearness = Real64();
			SkyBrightness = Real64();
			StdBaroPress = 101325.0;
			StdRhoAir = Real64();
			TimeZoneNumber = Real64();
			TimeZoneMeridian = Real64();
			EnvironmentName = std::string();
			WeatherFileLocationTitle = std::string();
			CurMnDyHr = std::string();
			CurMnDy = std::string();
			CurEnvirNum = int();
			TotDesDays = 0;
			TotRunDesPersDays = 0;
			CurrentOverallSimDay = int();
			TotalOverallSimDays = int();
			MaxNumberSimYears = int();
			RunPeriodStartDayOfWeek = int();
			CosSolarDeclinAngle = Real64();
			EquationOfTime = Real64();
			SinLatitude = Real64();
			CosLatitude = Real64();
			SinSolarDeclinAngle = Real64();
			TS1TimeOffset = -0.5;
			WeatherFileWindModCoeff = 1.5863;
			WeatherFileTempModCoeff = 0.0;
			SiteWindExp = 0.22;
			SiteWindBLHeight = 370.0;
			SiteTempGradient = 0.0065;
			GroundTempObjInput = false;
			GroundTemp_SurfaceObjInput = false;
			GroundTemp_DeepObjInput = false;
			FCGroundTemps = false;
			DisplayWeatherMissingDataWarnings = false;
			IgnoreSolarRadiation = false;
			IgnoreBeamRadiation = false;
			IgnoreDiffuseRadiation = false;
			PrintEnvrnStampWarmup = false;
			PrintEnvrnStampWarmupPrinted = false;
			RunPeriodEnvironment = false;
			EnvironmentStartEnd = std::string();
			CurrentYearIsLeapYear = false;
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

		// This is a helper struct to redirect std::cout. This makes sure std::cout is redirected back and
		// everything is cleaned up properly
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

		// This is a helper struct to redirect std::cerr. This makes sure std::cerr is redirected back and
		// everything is cleaned up properly
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

		class IdfParser
		{
		public:
			std::vector< std::vector< std::string > > decode( std::string const & idf )
			{
				bool success = true;
				return decode( idf, success );
			}

			std::vector< std::vector< std::string > > decode( std::string const & idf, bool & success )
			{
				success = true;
				if ( idf.empty() ) return std::vector< std::vector< std::string > >();

				size_t index = 0;
				return parse_array( idf, index, success );
			}

		private:
			friend class IdfParserFixture;
			enum Token : size_t { NONE = 0, END = 1, EXCLAMATION = 2, COMMA = 3, SEMICOLON = 4, STRING = 5 };

			std::vector< std::vector< std::string > > parse_array( std::string const & idf, size_t & index, bool & success )
			{
				std::vector< std::vector< std::string > > obj;
				int token;

				// next_token(json, ref index);

				bool done = false;
				while ( !done ) {
					token = look_ahead( idf, index );
					if ( token == Token::END ) {
						break;
					} else if ( token == Token::NONE ) {
						success = false;
						return std::vector< std::vector< std::string > >();
					} else if ( token == Token::EXCLAMATION ) {
						eat_comment( idf, index );
					} else {

						auto const array = parse_object( idf, index, success );
						if ( ! array.empty() ) obj.push_back( array );
					}
				}

				return obj;
			}

			std::vector< std::string > parse_object( std::string const & idf, size_t & index, bool & success )
			{
				std::vector< std::string > array;
				Token token;

				// next_token( idf, index );

				bool done = false;
				while ( !done ) {
					token = look_ahead( idf, index );
					if ( token == Token::NONE ) {
						success = false;
						return std::vector< std::string >();
					} else if ( token == Token::COMMA ) {
						next_token( idf, index );
					} else if ( token == Token::SEMICOLON ) {
                        next_token( idf, index );
						break;
					} else if ( token == Token::EXCLAMATION ) {
						eat_comment( idf, index );
					} else {
						std::string const value = parse_value( idf, index, success );
						if ( !success ) {
							return std::vector< std::string >();
						}
						array.push_back( value );
					}
				}

				return array;
			}

			std::string parse_value( std::string const & idf, size_t & index, bool & success)
			{
				switch ( look_ahead( idf, index ) ) {
					case Token::STRING:
						return parse_string( idf, index, success );
					case Token::NONE:
						break;
					// done to silence compiler warning
					default:
						break;
				}

				success = false;
				return std::string();
			}

			std::string parse_string( std::string const & idf, size_t & index, bool & success)
			{
				eat_whitespace( idf, index );

				std::string s;
				char c;

				auto const idf_size = idf.size();

				bool complete = false;
				while ( !complete ) {
					if ( index == idf_size ) {
						complete = true;
						break;
					}

					c = idf[ index++ ];
					if ( c == ',' ) {
						complete = true;
						index--;
						break;
					} else if ( c == ';' ) {
						complete = true;
						index--;
						break;
					} else if ( c == '!' ) {
						complete = true;
						index--;
						break;
					} else if ( c == '\\' ) {
						if ( index == idf_size ) break;
						c = idf[ index++ ];
						if ( c == '"' ) {
							s += '"';
						} else if ( c == '\\' ) {
							s += '\\';
						} else if ( c == '/' ) {
							s += '/';
						} else if ( c == 'b' ) {
							s += '\b';
						} else if ( c == 't' ) {
							s += '\t';
						} else if ( c == 'n' ) {
							complete = false;
							break;
						} else if ( c == 'r' ) {
							complete = false;
							break;
						}
					} else {
						s += c;
					}
				}

				if ( !complete ) {
					success = false;
					return std::string();
				}

				return s;
			}

			void eat_whitespace( std::string const & idf, size_t & index)
			{
				for (; index < idf.size(); index++) {
					switch( idf[ index ] ) {
						case ' ': case '\n': case '\r': case '\t':
							continue;
						default:
							return;
					}
				}
			}

			void eat_comment( std::string const & idf, size_t & index)
			{
				auto const idf_size = idf.size();
				while ( true ) {
					if ( index == idf_size ) break;
					if ( idf[ index++ ] == '\n' ) break;
				}
			}

			Token look_ahead( std::string const & idf, size_t index)
			{
				size_t save_index = index;
				return next_token( idf, save_index );
			}

			Token next_token( std::string const & idf, size_t & index)
			{
				eat_whitespace( idf, index );

				if ( index == idf.size() ) {
					return Token::END;
				}

				char const c = idf[ index ];
				index++;
				switch (c) {
					case '!':
						return Token::EXCLAMATION;
					case ',':
						return Token::COMMA;
					case ';':
						return Token::SEMICOLON;
					case '\n': case '\r':
						break;
					default:
						static std::string const search_chars( "-:.#/\\[]{}_@$%^&*()|+=<>?'\"" );
						if ( isalnum( c ) || ( std::string::npos != search_chars.find_first_of( c ) ) ) {
							return Token::STRING;
						}
						break;
				}
				index--;
				return Token::NONE;
			}
		};

	};

	typedef EnergyPlusFixture EnergyPlusDeathTestFixture;

}

#endif
