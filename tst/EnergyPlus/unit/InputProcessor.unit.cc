// EnergyPlus::InputProcessor Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/EnergyPlusFixture.hh"

using namespace InputProcessor;

namespace EnergyPlus {

	class InputProcessorFixture : public EnergyPlusFixture
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

		bool process_idd( std::string const & idd, bool & errors_found ) {
			return EnergyPlusFixture::process_idd( idd, errors_found );
		}
	};

	typedef InputProcessorFixture InputProcessorDeathTestFixture;
}

// These must be here otherwise the linker will complain. This is so the shared resource m_idd_cache can be used by unit tests.
struct InputProcessorCache;
std::unique_ptr<EnergyPlus::EnergyPlusFixture::InputProcessorCache> EnergyPlusFixture::m_idd_cache = nullptr;

TEST_F( InputProcessorFixture, findItemInSortedList )
{
	InputProcessor::ListOfObjects = Array1D_string ({
		"OUTPUT:METER",
		"OUTPUT:METER:CUMULATIVE",
		"OUTPUT:METER:CUMULATIVE:METERFILEONLY",
		"OUTPUT:METER:METERFILEONLY",
		"OUTPUT:SQLITE",
		"OUTPUT:VARIABLE"
	});

	InputProcessor::NumObjectDefs = ListOfObjects.size();

	iListOfObjects.allocate( NumObjectDefs );
	SortAndStringUtilities::SetupAndSort( ListOfObjects, iListOfObjects );

	auto index1 = FindItemInSortedList( "OUTPUT:METER", ListOfObjects, NumObjectDefs );
	auto index2 = FindItemInSortedList( "OUTPUT:METER:CUMULATIVE", ListOfObjects, NumObjectDefs );
	auto index3 = FindItemInSortedList( "OUTPUT:METER:CUMULATIVE:METERFILEONLY", ListOfObjects, NumObjectDefs );
	auto index4 = FindItemInSortedList( "OUTPUT:METER:METERFILEONLY", ListOfObjects, NumObjectDefs );
	auto index5 = FindItemInSortedList( "OUTPUT:SQLITE", ListOfObjects, NumObjectDefs );
	auto index6 = FindItemInSortedList( "OUTPUT:VARIABLE", ListOfObjects, NumObjectDefs );


	EXPECT_EQ( 1, index1 );
	EXPECT_EQ( 2, index2 );
	EXPECT_EQ( 3, index3 );
	EXPECT_EQ( 4, index4 );
	EXPECT_EQ( 5, index5 );
	EXPECT_EQ( 6, index6 );

	EXPECT_EQ( 1, iListOfObjects( index1 ) );
	EXPECT_EQ( 2, iListOfObjects( index2 ) );
	EXPECT_EQ( 3, iListOfObjects( index3 ) );
	EXPECT_EQ( 4, iListOfObjects( index4 ) );
	EXPECT_EQ( 5, iListOfObjects( index5 ) );
	EXPECT_EQ( 6, iListOfObjects( index6 ) );

}

TEST_F( InputProcessorFixture, findItemInSortedList_unsorted )
{
	InputProcessor::ListOfObjects = Array1D_string ({
		"OUTPUT:VARIABLE",
		"OUTPUT:METER",
		"OUTPUT:METER:METERFILEONLY",
		"OUTPUT:METER:CUMULATIVE",
		"OUTPUT:METER:CUMULATIVE:METERFILEONLY",
		"OUTPUT:SQLITE"
	});

	InputProcessor::NumObjectDefs = ListOfObjects.size();

	iListOfObjects.allocate( NumObjectDefs );
	SortAndStringUtilities::SetupAndSort( ListOfObjects, iListOfObjects );

	auto index1 = FindItemInSortedList( "OUTPUT:METER", ListOfObjects, NumObjectDefs );
	auto index2 = FindItemInSortedList( "OUTPUT:METER:CUMULATIVE", ListOfObjects, NumObjectDefs );
	auto index3 = FindItemInSortedList( "OUTPUT:METER:CUMULATIVE:METERFILEONLY", ListOfObjects, NumObjectDefs );
	auto index4 = FindItemInSortedList( "OUTPUT:METER:METERFILEONLY", ListOfObjects, NumObjectDefs );
	auto index5 = FindItemInSortedList( "OUTPUT:SQLITE", ListOfObjects, NumObjectDefs );
	auto index6 = FindItemInSortedList( "OUTPUT:VARIABLE", ListOfObjects, NumObjectDefs );


	EXPECT_EQ( 1, index1 );
	EXPECT_EQ( 2, index2 );
	EXPECT_EQ( 3, index3 );
	EXPECT_EQ( 4, index4 );
	EXPECT_EQ( 5, index5 );
	EXPECT_EQ( 6, index6 );

	EXPECT_EQ( 2, iListOfObjects( index1 ) );
	EXPECT_EQ( 4, iListOfObjects( index2 ) );
	EXPECT_EQ( 5, iListOfObjects( index3 ) );
	EXPECT_EQ( 3, iListOfObjects( index4 ) );
	EXPECT_EQ( 6, iListOfObjects( index5 ) );
	EXPECT_EQ( 1, iListOfObjects( index6 ) );

}

TEST_F( InputProcessorFixture, findItemInList )
{
	InputProcessor::ListOfObjects = Array1D_string ({
		"OUTPUT:VARIABLE",
		"OUTPUT:METER",
		"OUTPUT:METER:METERFILEONLY",
		"OUTPUT:METER:CUMULATIVE",
		"OUTPUT:METER:CUMULATIVE:METERFILEONLY",
		"OUTPUT:SQLITE"
	});

	InputProcessor::NumObjectDefs = ListOfObjects.size();

	EXPECT_EQ( 2, FindItemInList( "OUTPUT:METER", ListOfObjects, NumObjectDefs ) );
	EXPECT_EQ( 4, FindItemInList( "OUTPUT:METER:CUMULATIVE", ListOfObjects, NumObjectDefs ) );
	EXPECT_EQ( 5, FindItemInList( "OUTPUT:METER:CUMULATIVE:METERFILEONLY", ListOfObjects, NumObjectDefs ) );
	EXPECT_EQ( 3, FindItemInList( "OUTPUT:METER:METERFILEONLY", ListOfObjects, NumObjectDefs ) );
	EXPECT_EQ( 6, FindItemInList( "OUTPUT:SQLITE", ListOfObjects, NumObjectDefs ) );
	EXPECT_EQ( 1, FindItemInList( "OUTPUT:VARIABLE", ListOfObjects, NumObjectDefs ) );

}

TEST_F( InputProcessorFixture, processIDD_Full_IDD )
{
	std::string const idd_objects;

	bool errors_found = false;

	ASSERT_FALSE( process_idd( idd_objects, errors_found ) );

	EXPECT_EQ( 745, NumObjectDefs );
	ASSERT_EQ( static_cast<unsigned long>( NumObjectDefs ), ListOfObjects.size() );

	std::string const name( "OUTPUT:SQLITE" );

	auto index = FindItemInSortedList( name, ListOfObjects, NumObjectDefs );
	if ( index != 0 ) index = iListOfObjects( index );

	EXPECT_EQ( name, ObjectDef( index ).Name );
	EXPECT_EQ( 1, ObjectDef( index ).NumParams );
	EXPECT_EQ( 1, ObjectDef( index ).NumAlpha );
	EXPECT_EQ( 0, ObjectDef( index ).NumNumeric );
	EXPECT_EQ( 0, ObjectDef( index ).MinNumFields );
	EXPECT_FALSE( ObjectDef( index ).NameAlpha1 );
	EXPECT_TRUE( ObjectDef( index ).UniqueObject );
	EXPECT_FALSE( ObjectDef( index ).RequiredObject );
	EXPECT_FALSE( ObjectDef( index ).ExtensibleObject );
	EXPECT_EQ( 0, ObjectDef( index ).ExtensibleNum );
	EXPECT_EQ( 0, ObjectDef( index ).LastExtendAlpha );
	EXPECT_EQ( 0, ObjectDef( index ).LastExtendNum );
	EXPECT_EQ( 0, ObjectDef( index ).ObsPtr );
	EXPECT_TRUE( compare_containers< std::vector< bool > >( { true } , ObjectDef( index ).AlphaOrNumeric.begin() ) );
	EXPECT_TRUE( compare_containers< std::vector< bool > >( { false } , ObjectDef( index ).ReqField.begin() ) );
	EXPECT_TRUE( compare_containers< std::vector< bool > >( { false } , ObjectDef( index ).AlphRetainCase.begin() ) );
	EXPECT_TRUE( compare_containers< std::vector< std::string > >( { "Option Type" }, ObjectDef( index ).AlphFieldChks.begin() ) );
	EXPECT_TRUE( compare_containers< std::vector< std::string > >( { "" }, ObjectDef( index ).AlphFieldDefs.begin() ) );
	// EXPECT_TRUE( compare_containers< Array1D< RangeCheckDef > >( { RangeCheckDef() }, ObjectDef( index ).NumRangeChks ) );
	EXPECT_EQ( 0, ObjectDef( index ).NumFound );

}

TEST_F( InputProcessorFixture, processIDD )
{
	std::string const idd_objects = delimited_string({
		"Output:SQLite,",
		"       \\memo Output from EnergyPlus can be written to an SQLite format file.",
		"       \\unique-object",
		"  A1 ; \\field Option Type",
		"       \\type choice",
		"       \\key Simple",
		"       \\key SimpleAndTabular"
	});

	bool errors_found = false;

	ASSERT_FALSE( process_idd( idd_objects, errors_found ) );

	EXPECT_EQ( 1, NumObjectDefs );

	EXPECT_EQ( "OUTPUT:SQLITE", ObjectDef( 1 ).Name );
	EXPECT_EQ( 1, ObjectDef( 1 ).NumParams );
	EXPECT_EQ( 1, ObjectDef( 1 ).NumAlpha );
	EXPECT_EQ( 0, ObjectDef( 1 ).NumNumeric );
	EXPECT_EQ( 0, ObjectDef( 1 ).MinNumFields );
	EXPECT_FALSE( ObjectDef( 1 ).NameAlpha1 );
	EXPECT_TRUE( ObjectDef( 1 ).UniqueObject );
	EXPECT_FALSE( ObjectDef( 1 ).RequiredObject );
	EXPECT_FALSE( ObjectDef( 1 ).ExtensibleObject );
	EXPECT_EQ( 0, ObjectDef( 1 ).ExtensibleNum );
	EXPECT_EQ( 0, ObjectDef( 1 ).LastExtendAlpha );
	EXPECT_EQ( 0, ObjectDef( 1 ).LastExtendNum );
	EXPECT_EQ( 0, ObjectDef( 1 ).ObsPtr );
	EXPECT_TRUE( compare_containers< std::vector< bool > >( { true } , ObjectDef( 1 ).AlphaOrNumeric.begin() ) );
	EXPECT_TRUE( compare_containers< std::vector< bool > >( { false } , ObjectDef( 1 ).ReqField.begin() ) );
	EXPECT_TRUE( compare_containers< std::vector< bool > >( { false } , ObjectDef( 1 ).AlphRetainCase.begin() ) );
	EXPECT_TRUE( compare_containers< std::vector< std::string > >( { "Option Type" }, ObjectDef( 1 ).AlphFieldChks.begin() ) );
	EXPECT_TRUE( compare_containers< std::vector< std::string > >( { "" }, ObjectDef( 1 ).AlphFieldDefs.begin() ) );
	// EXPECT_TRUE( compare_containers< Array1D< RangeCheckDef > >( { RangeCheckDef() }, ObjectDef( 1 ).NumRangeChks ) );
	EXPECT_EQ( 0, ObjectDef( 1 ).NumFound );

}

TEST_F( InputProcessorFixture, addObjectDefandParse )
{
	std::string const idd_objects = 
		"Output:SQLite,\n"
		"       \\memo Output from EnergyPlus can be written to an SQLite format file.\n"
		"       \\unique-object\n"
		"  A1 ; \field Option Type\n"
		"       \type choice\n"
		"       \\key Simple\n"
		"       \\key SimpleAndTabular\n";

	auto idd_stream = std::unique_ptr<std::stringstream>( new std::stringstream( idd_objects ) );

	MaxSectionDefs = SectionDefAllocInc;
	MaxObjectDefs = ObjectDefAllocInc;

	SectionDef.allocate( MaxSectionDefs );
	ObjectDef.allocate( MaxObjectDefs );

	NumObjectDefs = 0;
	NumSectionDefs = 0;

	ProcessingIDD = true;

	bool errors_found( false );
	bool end_of_file( false );
	bool blank_line( false );
	std::string::size_type current_pos;

	ReadInputLine( *idd_stream, current_pos, blank_line, end_of_file );
	current_pos = scan( InputLine, ",;" );
	AddObjectDefandParse( *idd_stream, InputLine.substr( 0, current_pos ), current_pos, end_of_file, errors_found );

	EXPECT_EQ( "OUTPUT:SQLITE", ObjectDef( 1 ).Name );
	EXPECT_EQ( 1, ObjectDef( 1 ).NumParams );
	EXPECT_EQ( 1, ObjectDef( 1 ).NumAlpha );
	EXPECT_EQ( 0, ObjectDef( 1 ).NumNumeric );
	EXPECT_EQ( 0, ObjectDef( 1 ).MinNumFields );
	EXPECT_FALSE( ObjectDef( 1 ).NameAlpha1 );
	EXPECT_TRUE( ObjectDef( 1 ).UniqueObject );
	EXPECT_FALSE( ObjectDef( 1 ).RequiredObject );
	EXPECT_FALSE( ObjectDef( 1 ).ExtensibleObject );
	EXPECT_EQ( 0, ObjectDef( 1 ).ExtensibleNum );
	EXPECT_EQ( 0, ObjectDef( 1 ).LastExtendAlpha );
	EXPECT_EQ( 0, ObjectDef( 1 ).LastExtendNum );
	EXPECT_EQ( 0, ObjectDef( 1 ).ObsPtr );
	EXPECT_TRUE( compare_containers< std::vector< bool > >( { true } , ObjectDef( 1 ).AlphaOrNumeric.begin() ) );
	EXPECT_TRUE( compare_containers< std::vector< bool > >( { false } , ObjectDef( 1 ).ReqField.begin() ) );
	EXPECT_TRUE( compare_containers< std::vector< bool > >( { false } , ObjectDef( 1 ).AlphRetainCase.begin() ) );
	EXPECT_TRUE( compare_containers< std::vector< std::string > >( { "" }, ObjectDef( 1 ).AlphFieldChks.begin() ) );
	EXPECT_TRUE( compare_containers< std::vector< std::string > >( { "" }, ObjectDef( 1 ).AlphFieldDefs.begin() ) );
	// EXPECT_TRUE( compare_containers< Array1D< RangeCheckDef > >( { RangeCheckDef() }, ObjectDef( 1 ).NumRangeChks ) );
	EXPECT_EQ( 0, ObjectDef( 1 ).NumFound );

}

TEST_F( InputProcessorFixture, validateObjectandParse )
{
	std::string const idf_objects = delimited_string({
		"Version,",
		"8.3;",
		"Output:SQLite,",
		"SimpleAndTabular;"
	});

	NumIDFRecords = 2;
	IDFRecords.allocate( 2 );

	auto idf_stream = std::unique_ptr<std::stringstream>( new std::stringstream( idf_objects ) );

	std::string const idd_objects = delimited_string({
		"Version,",
		"      \\memo Specifies the EnergyPlus version of the IDF file.",
		"      \\unique-object",
		"      \\format singleLine",
		"  A1 ; \\field Version Identifier",
		"      \\required-field",
		"      \\default 8.3",
		"Output:SQLite,",
		"       \\memo Output from EnergyPlus can be written to an SQLite format file.",
		"       \\unique-object",
		"  A1 ; \\field Option Type",
		"       \\type choice",
		"       \\key Simple",
		"       \\key SimpleAndTabular"
	});

	bool errors_found = false;

	ASSERT_FALSE( process_idd( idd_objects, errors_found ) ) << "Error processing IDD.";

	NumLines = 0;

	MaxIDFRecords = ObjectsIDFAllocInc;
	NumIDFRecords = 0;
	MaxIDFSections = SectionsIDFAllocInc;
	NumIDFSections = 0;

	SectionsOnFile.allocate( MaxIDFSections );
	IDFRecords.allocate( MaxIDFRecords );
	LineItem.Numbers.allocate( MaxNumericArgsFound );
	LineItem.NumBlank.allocate( MaxNumericArgsFound );
	LineItem.Alphas.allocate( MaxAlphaArgsFound );
	LineItem.AlphBlank.allocate( MaxAlphaArgsFound );

	bool end_of_file( false );
	bool blank_line( false );
	std::string::size_type current_pos;

	while ( ! end_of_file ) {
		ReadInputLine( *idf_stream, current_pos, blank_line, end_of_file );
		if ( blank_line || end_of_file ) continue;
		current_pos = scan( InputLine, ",;" );
		ValidateObjectandParse( *idf_stream, InputLine.substr( 0, current_pos ), current_pos, end_of_file );
	}

	EXPECT_FALSE( OverallErrorFlag );

	EXPECT_FALSE( has_cout_output() );
	EXPECT_FALSE( has_cerr_output() );

	std::string const version_name( "VERSION" );

	auto index = FindItemInSortedList( version_name, ListOfObjects, NumObjectDefs );
	if ( index != 0 ) index = iListOfObjects( index );

	index = ObjectStartRecord( index );

	EXPECT_EQ( 1, index );

	EXPECT_EQ( version_name, IDFRecords( index ).Name );
	EXPECT_EQ( 1, IDFRecords( index ).NumAlphas );
	EXPECT_EQ( 0, IDFRecords( index ).NumNumbers );
	EXPECT_EQ( 1, IDFRecords( index ).ObjectDefPtr );
	EXPECT_TRUE( compare_containers< std::vector< std::string > >( { "8.3" }, IDFRecords( index ).Alphas.begin() ) );
	EXPECT_TRUE( compare_containers< std::vector< bool > >( { false }, IDFRecords( index ).AlphBlank.begin() ) );
	EXPECT_TRUE( compare_containers< std::vector< Real64 > >( {}, IDFRecords( index ).Numbers.begin() ) );
	EXPECT_TRUE( compare_containers< std::vector< bool > >( {}, IDFRecords( index ).NumBlank.begin() ) );

	std::string const sqlite_name( "OUTPUT:SQLITE" );

	index = FindItemInSortedList( sqlite_name, ListOfObjects, NumObjectDefs );
	if ( index != 0 ) index = iListOfObjects( index );

	index = ObjectStartRecord( index );

	EXPECT_EQ( 2, index );

	EXPECT_EQ( sqlite_name, IDFRecords( index ).Name );
	EXPECT_EQ( 1, IDFRecords( index ).NumAlphas );
	EXPECT_EQ( 0, IDFRecords( index ).NumNumbers );
	EXPECT_EQ( 2, IDFRecords( index ).ObjectDefPtr );
	EXPECT_TRUE( compare_containers< std::vector< std::string > >( { "SIMPLEANDTABULAR" }, IDFRecords( index ).Alphas.begin() ) );
	EXPECT_TRUE( compare_containers< std::vector< bool > >( { false }, IDFRecords( index ).AlphBlank.begin() ) );
	EXPECT_TRUE( compare_containers< std::vector< Real64 > >( {}, IDFRecords( index ).Numbers.begin() ) );
	EXPECT_TRUE( compare_containers< std::vector< bool > >( {}, IDFRecords( index ).NumBlank.begin() ) );

}

TEST_F( InputProcessorFixture, processIDF )
{
	std::string const idf_objects = delimited_string({
		"Version,",
		"8.3;",
		"Output:SQLite,",
		"SimpleAndTabular;"
	});

	ASSERT_FALSE( process_idf( idf_objects, false ) );

	EXPECT_FALSE( OverallErrorFlag );

	EXPECT_FALSE( has_cout_output() );
	EXPECT_FALSE( has_cerr_output() );

	std::string const version_name( "VERSION" );

	auto index = FindItemInSortedList( version_name, ListOfObjects, NumObjectDefs );
	if ( index != 0 ) index = iListOfObjects( index );

	index = ObjectStartRecord( index );

	EXPECT_EQ( 1, index );

	EXPECT_EQ( version_name, IDFRecords( index ).Name );
	EXPECT_EQ( 1, IDFRecords( index ).NumAlphas );
	EXPECT_EQ( 0, IDFRecords( index ).NumNumbers );
	EXPECT_EQ( 1, IDFRecords( index ).ObjectDefPtr );
	EXPECT_TRUE( compare_containers< std::vector< std::string > >( { "8.3" }, IDFRecords( index ).Alphas.begin() ) );
	EXPECT_TRUE( compare_containers< std::vector< bool > >( { false }, IDFRecords( index ).AlphBlank.begin() ) );
	EXPECT_TRUE( compare_containers< std::vector< Real64 > >( {}, IDFRecords( index ).Numbers.begin() ) );
	EXPECT_TRUE( compare_containers< std::vector< bool > >( {}, IDFRecords( index ).NumBlank.begin() ) );

	std::string const sqlite_name( "OUTPUT:SQLITE" );

	index = FindItemInSortedList( sqlite_name, ListOfObjects, NumObjectDefs );
	if ( index != 0 ) index = iListOfObjects( index );

	index = ObjectStartRecord( index );

	EXPECT_EQ( 2, index );

	EXPECT_EQ( sqlite_name, IDFRecords( index ).Name );
	EXPECT_EQ( 1, IDFRecords( index ).NumAlphas );
	EXPECT_EQ( 0, IDFRecords( index ).NumNumbers );
	EXPECT_EQ( 739, IDFRecords( index ).ObjectDefPtr );
	EXPECT_TRUE( compare_containers< std::vector< std::string > >( { "SIMPLEANDTABULAR" }, IDFRecords( index ).Alphas.begin() ) );
	EXPECT_TRUE( compare_containers< std::vector< bool > >( { false }, IDFRecords( index ).AlphBlank.begin() ) );
	EXPECT_TRUE( compare_containers< std::vector< Real64 > >( {}, IDFRecords( index ).Numbers.begin() ) );
	EXPECT_TRUE( compare_containers< std::vector< bool > >( {}, IDFRecords( index ).NumBlank.begin() ) );

}

TEST_F( InputProcessorFixture, processIDF_Cached )
{
	std::string const idf_objects = delimited_string({
		"Version,",
		"8.3;",
		"Output:SQLite,",
		"SimpleAndTabular;"
	});

	ASSERT_FALSE( process_idf( idf_objects ) );

	EXPECT_FALSE( OverallErrorFlag );

	EXPECT_FALSE( has_cout_output() );
	EXPECT_FALSE( has_cerr_output() );

	std::string const version_name( "VERSION" );

	auto index = FindItemInSortedList( version_name, ListOfObjects, NumObjectDefs );
	if ( index != 0 ) index = iListOfObjects( index );

	index = ObjectStartRecord( index );

	EXPECT_EQ( 1, index );

	EXPECT_EQ( version_name, IDFRecords( index ).Name );
	EXPECT_EQ( 1, IDFRecords( index ).NumAlphas );
	EXPECT_EQ( 0, IDFRecords( index ).NumNumbers );
	EXPECT_EQ( 1, IDFRecords( index ).ObjectDefPtr );
	EXPECT_TRUE( compare_containers< std::vector< std::string > >( { "8.3" }, IDFRecords( index ).Alphas.begin() ) );
	EXPECT_TRUE( compare_containers< std::vector< bool > >( { false }, IDFRecords( index ).AlphBlank.begin() ) );
	EXPECT_TRUE( compare_containers< std::vector< Real64 > >( {}, IDFRecords( index ).Numbers.begin() ) );
	EXPECT_TRUE( compare_containers< std::vector< bool > >( {}, IDFRecords( index ).NumBlank.begin() ) );

	std::string const sqlite_name( "OUTPUT:SQLITE" );

	index = FindItemInSortedList( sqlite_name, ListOfObjects, NumObjectDefs );
	if ( index != 0 ) index = iListOfObjects( index );

	index = ObjectStartRecord( index );

	EXPECT_EQ( 2, index );

	EXPECT_EQ( sqlite_name, IDFRecords( index ).Name );
	EXPECT_EQ( 1, IDFRecords( index ).NumAlphas );
	EXPECT_EQ( 0, IDFRecords( index ).NumNumbers );
	EXPECT_EQ( 739, IDFRecords( index ).ObjectDefPtr );
	EXPECT_TRUE( compare_containers< std::vector< std::string > >( { "SIMPLEANDTABULAR" }, IDFRecords( index ).Alphas.begin() ) );
	EXPECT_TRUE( compare_containers< std::vector< bool > >( { false }, IDFRecords( index ).AlphBlank.begin() ) );
	EXPECT_TRUE( compare_containers< std::vector< Real64 > >( {}, IDFRecords( index ).Numbers.begin() ) );
	EXPECT_TRUE( compare_containers< std::vector< bool > >( {}, IDFRecords( index ).NumBlank.begin() ) );

}

TEST_F( InputProcessorFixture, getNumObjectsFound )
{
	std::string const idf_objects = delimited_string({
		"Version,8.3;",
		"Output:SQLite,SimpleAndTabular;",
		"Output:Meter:MeterFileOnly,Electricity:Facility,timestep;",
		"Output:Meter:MeterFileOnly,Electricity:Facility,hourly;",
		"Output:Meter:MeterFileOnly,Electricity:Facility,daily;",
		"Output:Meter:MeterFileOnly,Electricity:Facility,monthly;",
		"Output:Meter:MeterFileOnly,Electricity:Facility,runperiod;",
	});

	ASSERT_FALSE( process_idf( idf_objects ) );

	EXPECT_EQ( 1, GetNumObjectsFound( "VERSION" ) );
	EXPECT_EQ( 1, GetNumObjectsFound( "OUTPUT:SQLITE" ) );
	EXPECT_EQ( 5, GetNumObjectsFound( "OUTPUT:METER:METERFILEONLY" ) );
	EXPECT_EQ( 0, GetNumObjectsFound( "OUTPUT:VARIABLE" ) );

	EXPECT_FALSE( has_cout_output() );
	EXPECT_FALSE( has_cerr_output() );

}
