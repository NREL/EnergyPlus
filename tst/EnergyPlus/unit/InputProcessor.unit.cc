// EnergyPlus::InputProcessor Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/InputProcessorFixture.hh"

#include <tuple>
#include <map>

// These must be here otherwise the linker will complain. This is so the shared resource m_idd_cache can be used by unit tests.
struct InputProcessorCache;
std::unique_ptr<EnergyPlus::EnergyPlusFixture::InputProcessorCache> EnergyPlusFixture::m_idd_cache = nullptr;

using namespace InputProcessor;

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
	EXPECT_TRUE( compare_containers< std::vector< bool > >( { true } , ObjectDef( index ).AlphaOrNumeric ) );
	EXPECT_TRUE( compare_containers< std::vector< bool > >( { false } , ObjectDef( index ).ReqField ) );
	EXPECT_TRUE( compare_containers< std::vector< bool > >( { false } , ObjectDef( index ).AlphRetainCase ) );
	EXPECT_TRUE( compare_containers< std::vector< std::string > >( { "Option Type" }, ObjectDef( index ).AlphFieldChks ) );
	EXPECT_TRUE( compare_containers< std::vector< std::string > >( { "" }, ObjectDef( index ).AlphFieldDefs ) );
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
	EXPECT_TRUE( compare_containers< std::vector< bool > >( { true } , ObjectDef( 1 ).AlphaOrNumeric ) );
	EXPECT_TRUE( compare_containers< std::vector< bool > >( { false } , ObjectDef( 1 ).ReqField ) );
	EXPECT_TRUE( compare_containers< std::vector< bool > >( { false } , ObjectDef( 1 ).AlphRetainCase ) );
	EXPECT_TRUE( compare_containers< std::vector< std::string > >( { "Option Type" }, ObjectDef( 1 ).AlphFieldChks ) );
	EXPECT_TRUE( compare_containers< std::vector< std::string > >( { "" }, ObjectDef( 1 ).AlphFieldDefs ) );
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
	EXPECT_TRUE( compare_containers< std::vector< bool > >( { true } , ObjectDef( 1 ).AlphaOrNumeric ) );
	EXPECT_TRUE( compare_containers< std::vector< bool > >( { false } , ObjectDef( 1 ).ReqField ) );
	EXPECT_TRUE( compare_containers< std::vector< bool > >( { false } , ObjectDef( 1 ).AlphRetainCase ) );
	EXPECT_TRUE( compare_containers< std::vector< std::string > >( { "" }, ObjectDef( 1 ).AlphFieldChks ) );
	EXPECT_TRUE( compare_containers< std::vector< std::string > >( { "" }, ObjectDef( 1 ).AlphFieldDefs ) );
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
	EXPECT_TRUE( compare_containers< std::vector< std::string > >( { "8.3" }, IDFRecords( index ).Alphas ) );
	EXPECT_TRUE( compare_containers< std::vector< bool > >( { false }, IDFRecords( index ).AlphBlank ) );
	EXPECT_TRUE( compare_containers< std::vector< Real64 > >( {}, IDFRecords( index ).Numbers ) );
	EXPECT_TRUE( compare_containers< std::vector< bool > >( {}, IDFRecords( index ).NumBlank ) );

	std::string const sqlite_name( "OUTPUT:SQLITE" );

	index = FindItemInSortedList( sqlite_name, ListOfObjects, NumObjectDefs );
	if ( index != 0 ) index = iListOfObjects( index );

	index = ObjectStartRecord( index );

	EXPECT_EQ( 2, index );

	EXPECT_EQ( sqlite_name, IDFRecords( index ).Name );
	EXPECT_EQ( 1, IDFRecords( index ).NumAlphas );
	EXPECT_EQ( 0, IDFRecords( index ).NumNumbers );
	EXPECT_EQ( 2, IDFRecords( index ).ObjectDefPtr );
	EXPECT_TRUE( compare_containers< std::vector< std::string > >( { "SIMPLEANDTABULAR" }, IDFRecords( index ).Alphas ) );
	EXPECT_TRUE( compare_containers< std::vector< bool > >( { false }, IDFRecords( index ).AlphBlank ) );
	EXPECT_TRUE( compare_containers< std::vector< Real64 > >( {}, IDFRecords( index ).Numbers ) );
	EXPECT_TRUE( compare_containers< std::vector< bool > >( {}, IDFRecords( index ).NumBlank ) );

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
	EXPECT_TRUE( compare_containers< std::vector< std::string > >( { "8.3" }, IDFRecords( index ).Alphas ) );
	EXPECT_TRUE( compare_containers< std::vector< bool > >( { false }, IDFRecords( index ).AlphBlank ) );
	EXPECT_TRUE( compare_containers< std::vector< Real64 > >( {}, IDFRecords( index ).Numbers ) );
	EXPECT_TRUE( compare_containers< std::vector< bool > >( {}, IDFRecords( index ).NumBlank ) );

	std::string const sqlite_name( "OUTPUT:SQLITE" );

	index = FindItemInSortedList( sqlite_name, ListOfObjects, NumObjectDefs );
	if ( index != 0 ) index = iListOfObjects( index );

	index = ObjectStartRecord( index );

	EXPECT_EQ( 2, index );

	EXPECT_EQ( sqlite_name, IDFRecords( index ).Name );
	EXPECT_EQ( 1, IDFRecords( index ).NumAlphas );
	EXPECT_EQ( 0, IDFRecords( index ).NumNumbers );
	EXPECT_EQ( 739, IDFRecords( index ).ObjectDefPtr );
	EXPECT_TRUE( compare_containers< std::vector< std::string > >( { "SIMPLEANDTABULAR" }, IDFRecords( index ).Alphas ) );
	EXPECT_TRUE( compare_containers< std::vector< bool > >( { false }, IDFRecords( index ).AlphBlank ) );
	EXPECT_TRUE( compare_containers< std::vector< Real64 > >( {}, IDFRecords( index ).Numbers ) );
	EXPECT_TRUE( compare_containers< std::vector< bool > >( {}, IDFRecords( index ).NumBlank ) );

}

TEST_F( InputProcessorFixture, processIDF_Cached )
{
	std::string const idf_objects = delimited_string({
		"Version,8.3;",
		"Output:SQLite,SimpleAndTabular;",
		"  BuildingSurface:Detailed,",
		"    Zn001:Wall001,           !- Name",
		"    Wall,                    !- Surface Type",
		"    R13WALL,                 !- Construction Name",
		"    Main Zone,               !- Zone Name",
		"    Outdoors,                !- Outside Boundary Condition",
		"    ,                        !- Outside Boundary Condition Object",
		"    SunExposed,              !- Sun Exposure",
		"    WindExposed,             !- Wind Exposure",
		"    0.5000000,               !- View Factor to Ground",
		"    4,                       !- Number of Vertices",
		"    0,0,4.572000,  !- X,Y,Z ==> Vertex 1 {m}",
		"    0,0,0,  !- X,Y,Z ==> Vertex 2 {m}",
		"    15.24000,0,0,  !- X,Y,Z ==> Vertex 3 {m}",
		"    15.24000,0,4.572000;  !- X,Y,Z ==> Vertex 4 {m}",
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
	EXPECT_TRUE( compare_containers< std::vector< std::string > >( { "8.3" }, IDFRecords( index ).Alphas ) );
	EXPECT_TRUE( compare_containers< std::vector< bool > >( { false }, IDFRecords( index ).AlphBlank ) );
	EXPECT_TRUE( compare_containers< std::vector< Real64 > >( {}, IDFRecords( index ).Numbers ) );
	EXPECT_TRUE( compare_containers< std::vector< bool > >( {}, IDFRecords( index ).NumBlank ) );

	std::string const sqlite_name( "OUTPUT:SQLITE" );

	index = FindItemInSortedList( sqlite_name, ListOfObjects, NumObjectDefs );
	if ( index != 0 ) index = iListOfObjects( index );

	index = ObjectStartRecord( index );

	EXPECT_EQ( 2, index );

	EXPECT_EQ( sqlite_name, IDFRecords( index ).Name );
	EXPECT_EQ( 1, IDFRecords( index ).NumAlphas );
	EXPECT_EQ( 0, IDFRecords( index ).NumNumbers );
	EXPECT_EQ( 739, IDFRecords( index ).ObjectDefPtr );
	EXPECT_TRUE( compare_containers< std::vector< std::string > >( { "SIMPLEANDTABULAR" }, IDFRecords( index ).Alphas ) );
	EXPECT_TRUE( compare_containers< std::vector< bool > >( { false }, IDFRecords( index ).AlphBlank ) );
	EXPECT_TRUE( compare_containers< std::vector< Real64 > >( {}, IDFRecords( index ).Numbers ) );
	EXPECT_TRUE( compare_containers< std::vector< bool > >( {}, IDFRecords( index ).NumBlank ) );

	std::string const building_surface_detailed_name( "BUILDINGSURFACE:DETAILED" );

	index = FindItemInSortedList( building_surface_detailed_name, ListOfObjects, NumObjectDefs );
	if ( index != 0 ) index = iListOfObjects( index );

	index = ObjectStartRecord( index );

	EXPECT_EQ( 3, index );

	EXPECT_EQ( building_surface_detailed_name, IDFRecords( index ).Name );
	EXPECT_EQ( 8, IDFRecords( index ).NumAlphas );
	EXPECT_EQ( 14, IDFRecords( index ).NumNumbers );
	EXPECT_EQ( 98, IDFRecords( index ).ObjectDefPtr );
	EXPECT_TRUE( compare_containers< std::vector< std::string > >( { "ZN001:WALL001", "WALL", "R13WALL", "MAIN ZONE", "OUTDOORS", "", "SUNEXPOSED", "WINDEXPOSED" }, IDFRecords( index ).Alphas ) );
	EXPECT_TRUE( compare_containers< std::vector< bool > >( { false, false, false, false, false, true, false, false }, IDFRecords( index ).AlphBlank ) );
	EXPECT_TRUE( compare_containers< std::vector< Real64 > >( { 0.5, 4, 0, 0, 4.572, 0, 0, 0, 15.24, 0, 0, 15.24, 0, 4.572 }, IDFRecords( index ).Numbers ) );
	EXPECT_TRUE( compare_containers< std::vector< bool > >( { false, false, false, false, false, false, false, false, false, false, false, false, false, false }, IDFRecords( index ).NumBlank ) );

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

TEST_F( InputProcessorFixture, addRecordToOutputVariableStructure )
{
	using namespace DataOutputs;

	AddRecordToOutputVariableStructure( "*", "ZONE AIR SYSTEM SENSIBLE COOLING RATE" );
	AddRecordToOutputVariableStructure( "*", "SITE OUTDOOR AIR DRYBULB TEMPERATURE" );
	AddRecordToOutputVariableStructure( "*", "SITE OUTDOOR AIR WETBULB TEMPERATURE" );
	AddRecordToOutputVariableStructure( "*", "ZONE TOTAL INTERNAL LATENT GAIN ENERGY" );
	AddRecordToOutputVariableStructure( "*", "ZONE TOTAL INTERNAL LATENT GAIN RATE" );

	EXPECT_EQ( "*", OutputVariablesForSimulation( 1 ).Key );
	EXPECT_EQ( "ZONE AIR SYSTEM SENSIBLE COOLING RATE", OutputVariablesForSimulation( 1 ).VarName );
	EXPECT_EQ( 0, OutputVariablesForSimulation( 1 ).Previous );
	EXPECT_EQ( 0, OutputVariablesForSimulation( 1 ).Next );

	EXPECT_EQ( "*", OutputVariablesForSimulation( 2 ).Key );
	EXPECT_EQ( "SITE OUTDOOR AIR DRYBULB TEMPERATURE", OutputVariablesForSimulation( 2 ).VarName );
	EXPECT_EQ( 0, OutputVariablesForSimulation( 2 ).Previous );
	EXPECT_EQ( 0, OutputVariablesForSimulation( 2 ).Next );

	EXPECT_EQ( "*", OutputVariablesForSimulation( 3 ).Key );
	EXPECT_EQ( "SITE OUTDOOR AIR WETBULB TEMPERATURE", OutputVariablesForSimulation( 3 ).VarName );
	EXPECT_EQ( 0, OutputVariablesForSimulation( 3 ).Previous );
	EXPECT_EQ( 0, OutputVariablesForSimulation( 3 ).Next );

	EXPECT_EQ( "*", OutputVariablesForSimulation( 4 ).Key );
	EXPECT_EQ( "ZONE TOTAL INTERNAL LATENT GAIN ENERGY", OutputVariablesForSimulation( 4 ).VarName );
	EXPECT_EQ( 0, OutputVariablesForSimulation( 4 ).Previous );
	EXPECT_EQ( 0, OutputVariablesForSimulation( 4 ).Next );

	EXPECT_EQ( "*", OutputVariablesForSimulation( 5 ).Key );
	EXPECT_EQ( "ZONE TOTAL INTERNAL LATENT GAIN RATE", OutputVariablesForSimulation( 5 ).VarName );
	EXPECT_EQ( 0, OutputVariablesForSimulation( 5 ).Previous );
	EXPECT_EQ( 0, OutputVariablesForSimulation( 5 ).Next );

}

TEST_F( InputProcessorFixture, addVariablesForMonthlyReport )
{
	auto const results_map = std::map< std::string, std::vector< std::tuple< std::string, std::string, int, int > > >({
		{
			"ZONECOOLINGSUMMARYMONTHLY",
			{
				{ "*", "ZONE AIR SYSTEM SENSIBLE COOLING RATE", 0, 0 },
				{ "*", "SITE OUTDOOR AIR DRYBULB TEMPERATURE", 0, 0 },
				{ "*", "SITE OUTDOOR AIR WETBULB TEMPERATURE", 0, 0 },
				{ "*", "ZONE TOTAL INTERNAL LATENT GAIN ENERGY", 0, 0 },
				{ "*", "ZONE TOTAL INTERNAL LATENT GAIN RATE", 0, 0 },
			}
		},
		{
			"ZONEHEATINGSUMMARYMONTHLY",
			{
				{ "*", "ZONE AIR SYSTEM SENSIBLE HEATING ENERGY", 0, 0 },
				{ "*", "ZONE AIR SYSTEM SENSIBLE HEATING RATE", 0, 0 },
				{ "*", "SITE OUTDOOR AIR DRYBULB TEMPERATURE", 0, 0 },
			}
		},
		{
			"ZONEELECTRICSUMMARYMONTHLY",
			{
				{ "*", "ZONE LIGHTS ELECTRIC ENERGY", 0, 0 },
				{ "*", "ZONE ELECTRIC EQUIPMENT ELECTRIC ENERGY", 0, 0 },
			}
		},
		{
			"SPACEGAINSMONTHLY",
			{
				{ "*", "ZONE PEOPLE TOTAL HEATING ENERGY", 0, 0 },
				{ "*", "ZONE LIGHTS TOTAL HEATING ENERGY", 0, 0 },
				{ "*", "ZONE ELECTRIC EQUIPMENT TOTAL HEATING ENERGY", 0, 0 },
				{ "*", "ZONE GAS EQUIPMENT TOTAL HEATING ENERGY", 0, 0 },
				{ "*", "ZONE HOT WATER EQUIPMENT TOTAL HEATING ENERGY", 0, 0 },
				{ "*", "ZONE STEAM EQUIPMENT TOTAL HEATING ENERGY", 0, 0 },
				{ "*", "ZONE OTHER EQUIPMENT TOTAL HEATING ENERGY", 0, 0 },
				{ "*", "ZONE INFILTRATION SENSIBLE HEAT GAIN ENERGY", 0, 0 },
				{ "*", "ZONE INFILTRATION SENSIBLE HEAT LOSS ENERGY", 0, 0 },
			}
		},
		{
			"PEAKSPACEGAINSMONTHLY",
			{
				{ "*", "ZONE PEOPLE TOTAL HEATING ENERGY", 0, 0 },
				{ "*", "ZONE LIGHTS TOTAL HEATING ENERGY", 0, 0 },
				{ "*", "ZONE ELECTRIC EQUIPMENT TOTAL HEATING ENERGY", 0, 0 },
				{ "*", "ZONE GAS EQUIPMENT TOTAL HEATING ENERGY", 0, 0 },
				{ "*", "ZONE HOT WATER EQUIPMENT TOTAL HEATING ENERGY", 0, 0 },
				{ "*", "ZONE STEAM EQUIPMENT TOTAL HEATING ENERGY", 0, 0 },
				{ "*", "ZONE OTHER EQUIPMENT TOTAL HEATING ENERGY", 0, 0 },
				{ "*", "ZONE INFILTRATION SENSIBLE HEAT GAIN ENERGY", 0, 0 },
				{ "*", "ZONE INFILTRATION SENSIBLE HEAT LOSS ENERGY", 0, 0 },
			}
		},
		{
			"SPACEGAINCOMPONENTSATCOOLINGPEAKMONTHLY",
			{
				{ "*", "ZONE AIR SYSTEM SENSIBLE COOLING RATE", 0, 0 },
				{ "*", "ZONE PEOPLE TOTAL HEATING ENERGY", 0, 0 },
				{ "*", "ZONE LIGHTS TOTAL HEATING ENERGY", 0, 0 },
				{ "*", "ZONE ELECTRIC EQUIPMENT TOTAL HEATING ENERGY", 0, 0 },
				{ "*", "ZONE GAS EQUIPMENT TOTAL HEATING ENERGY", 0, 0 },
				{ "*", "ZONE HOT WATER EQUIPMENT TOTAL HEATING ENERGY", 0, 0 },
				{ "*", "ZONE STEAM EQUIPMENT TOTAL HEATING ENERGY", 0, 0 },
				{ "*", "ZONE OTHER EQUIPMENT TOTAL HEATING ENERGY", 0, 0 },
				{ "*", "ZONE INFILTRATION SENSIBLE HEAT GAIN ENERGY", 0, 0 },
				{ "*", "ZONE INFILTRATION SENSIBLE HEAT LOSS ENERGY", 0, 0 },
			}
		},
		{
			"SETPOINTSNOTMETWITHTEMPERATURESMONTHLY",
			{
				{ "*", "ZONE HEATING SETPOINT NOT MET TIME", 0, 0 },
				{ "*", "ZONE MEAN AIR TEMPERATURE", 0, 0 },
				{ "*", "ZONE HEATING SETPOINT NOT MET WHILE OCCUPIED TIME", 0, 0 },
				{ "*", "ZONE COOLING SETPOINT NOT MET TIME", 0, 0 },
				{ "*", "ZONE COOLING SETPOINT NOT MET WHILE OCCUPIED TIME", 0, 0 },
			}
		},
		{
			"COMFORTREPORTSIMPLE55MONTHLY",
			{
				{ "*", "ZONE THERMAL COMFORT ASHRAE 55 SIMPLE MODEL SUMMER CLOTHES NOT COMFORTABLE TIME", 0, 0 },
				{ "*", "ZONE MEAN AIR TEMPERATURE", 0, 0 },
				{ "*", "ZONE THERMAL COMFORT ASHRAE 55 SIMPLE MODEL WINTER CLOTHES NOT COMFORTABLE TIME", 0, 0 },
				{ "*", "ZONE THERMAL COMFORT ASHRAE 55 SIMPLE MODEL SUMMER OR WINTER CLOTHES NOT COMFORTABLE TIME", 0, 0 },
			}
		},
		{
			"UNGLAZEDTRANSPIREDSOLARCOLLECTORSUMMARYMONTHLY",
			{
				{ "*", "SOLAR COLLECTOR SYSTEM EFFICIENCY", 0, 0 },
				{ "*", "SOLAR COLLECTOR OUTSIDE FACE SUCTION VELOCITY", 0, 0 },
				{ "*", "SOLAR COLLECTOR SENSIBLE HEATING RATE", 0, 0 },
			}
		},
		{
			"OCCUPANTCOMFORTDATASUMMARYMONTHLY",
			{
				{ "*", "PEOPLE OCCUPANT COUNT", 0, 0 },
				{ "*", "PEOPLE AIR TEMPERATURE", 0, 0 },
				{ "*", "PEOPLE AIR RELATIVE HUMIDITY", 0, 0 },
				{ "*", "ZONE THERMAL COMFORT FANGER MODEL PMV", 0, 0 },
				{ "*", "ZONE THERMAL COMFORT FANGER MODEL PPD", 0, 0 },
			}
		},
		{
			"CHILLERREPORTMONTHLY",
			{
				{ "*", "CHILLER ELECTRIC ENERGY", 0, 0 },
				{ "*", "CHILLER ELECTRIC POWER", 0, 0 },
				{ "*", "CHILLER EVAPORATOR COOLING ENERGY", 0, 0 },
				{ "*", "CHILLER CONDENSER HEAT TRANSFER ENERGY", 0, 0 },
				{ "*", "CHILLER COP", 0, 0 },
			}
		},
		{
			"TOWERREPORTMONTHLY",
			{
				{ "*", "COOLING TOWER FAN ELECTRIC ENERGY", 0, 0 },
				{ "*", "COOLING TOWER FAN ELECTRIC POWER", 0, 0 },
				{ "*", "COOLING TOWER HEAT TRANSFER RATE", 0, 0 },
				{ "*", "COOLING TOWER INLET TEMPERATURE", 0, 0 },
				{ "*", "COOLING TOWER OUTLET TEMPERATURE", 0, 0 },
				{ "*", "COOLING TOWER MASS FLOW RATE", 0, 0 },
			}
		},
		{
			"BOILERREPORTMONTHLY",
			{
				{ "*", "BOILER HEATING ENERGY", 0, 0 },
				{ "*", "BOILER GAS CONSUMPTION", 0, 0 },
				{ "*", "BOILER HEATING RATE", 0, 0 },
				{ "*", "BOILER GAS CONSUMPTION RATE", 0, 0 },
				{ "*", "BOILER INLET TEMPERATURE", 0, 0 },
				{ "*", "BOILER OUTLET TEMPERATURE", 0, 0 },
				{ "*", "BOILER MASS FLOW RATE", 0, 0 },
				{ "*", "BOILER ANCILLARY ELECTRIC POWER", 0, 0 },
			}
		},
		{
			"DXREPORTMONTHLY",
			{
				{ "*", "COOLING COIL TOTAL COOLING ENERGY", 0, 0 },
				{ "*", "COOLING COIL ELECTRIC ENERGY", 0, 0 },
				{ "*", "COOLING COIL SENSIBLE COOLING ENERGY", 0, 0 },
				{ "*", "COOLING COIL LATENT COOLING ENERGY", 0, 0 },
				{ "*", "COOLING COIL CRANKCASE HEATER ELECTRIC ENERGY", 0, 0 },
				{ "*", "COOLING COIL RUNTIME FRACTION", 0, 0 },
				{ "*", "COOLING COIL TOTAL COOLING RATE", 0, 0 },
				{ "*", "COOLING COIL SENSIBLE COOLING RATE", 0, 0 },
				{ "*", "COOLING COIL LATENT COOLING RATE", 0, 0 },
				{ "*", "COOLING COIL ELECTRIC POWER", 0, 0 },
				{ "*", "COOLING COIL CRANKCASE HEATER ELECTRIC POWER", 0, 0 },
			}
		},
		{
			"WINDOWREPORTMONTHLY",
			{
				{ "*", "SURFACE WINDOW TRANSMITTED SOLAR RADIATION RATE", 0, 0 },
				{ "*", "SURFACE WINDOW TRANSMITTED BEAM SOLAR RADIATION RATE", 0, 0 },
				{ "*", "SURFACE WINDOW TRANSMITTED DIFFUSE SOLAR RADIATION RATE", 0, 0 },
				{ "*", "SURFACE WINDOW HEAT GAIN RATE", 0, 0 },
				{ "*", "SURFACE WINDOW HEAT LOSS RATE", 0, 0 },
				{ "*", "SURFACE WINDOW INSIDE FACE GLAZING CONDENSATION STATUS", 0, 0 },
				{ "*", "SURFACE SHADING DEVICE IS ON TIME FRACTION", 0, 0 },
				{ "*", "SURFACE STORM WINDOW ON OFF STATUS", 0, 0 },
			}
		},
		{
			"WINDOWENERGYREPORTMONTHLY",
			{
				{ "*", "SURFACE WINDOW TRANSMITTED SOLAR RADIATION ENERGY", 0, 0 },
				{ "*", "SURFACE WINDOW TRANSMITTED BEAM SOLAR RADIATION ENERGY", 0, 0 },
				{ "*", "SURFACE WINDOW TRANSMITTED DIFFUSE SOLAR RADIATION ENERGY", 0, 0 },
				{ "*", "SURFACE WINDOW HEAT GAIN ENERGY", 0, 0 },
				{ "*", "SURFACE WINDOW HEAT LOSS ENERGY", 0, 0 },
			}
		},
		{
			"WINDOWZONESUMMARYMONTHLY",
			{
				{ "*", "ZONE WINDOWS TOTAL HEAT GAIN RATE", 0, 0 },
				{ "*", "ZONE WINDOWS TOTAL HEAT LOSS RATE", 0, 0 },
				{ "*", "ZONE WINDOWS TOTAL TRANSMITTED SOLAR RADIATION RATE", 0, 0 },
				{ "*", "ZONE EXTERIOR WINDOWS TOTAL TRANSMITTED BEAM SOLAR RADIATION RATE", 0, 0 },
				{ "*", "ZONE EXTERIOR WINDOWS TOTAL TRANSMITTED DIFFUSE SOLAR RADIATION RATE", 0, 0 },
				{ "*", "ZONE INTERIOR WINDOWS TOTAL TRANSMITTED DIFFUSE SOLAR RADIATION RATE", 0, 0 },
				{ "*", "ZONE INTERIOR WINDOWS TOTAL TRANSMITTED BEAM SOLAR RADIATION RATE", 0, 0 },
			}
		},
		{
			"WINDOWENERGYZONESUMMARYMONTHLY",
			{
				{ "*", "ZONE WINDOWS TOTAL HEAT GAIN ENERGY", 0, 0 },
				{ "*", "ZONE WINDOWS TOTAL HEAT LOSS ENERGY", 0, 0 },
				{ "*", "ZONE WINDOWS TOTAL TRANSMITTED SOLAR RADIATION ENERGY", 0, 0 },
				{ "*", "ZONE EXTERIOR WINDOWS TOTAL TRANSMITTED BEAM SOLAR RADIATION ENERGY", 0, 0 },
				{ "*", "ZONE EXTERIOR WINDOWS TOTAL TRANSMITTED DIFFUSE SOLAR RADIATION ENERGY", 0, 0 },
				{ "*", "ZONE INTERIOR WINDOWS TOTAL TRANSMITTED DIFFUSE SOLAR RADIATION ENERGY", 0, 0 },
				{ "*", "ZONE INTERIOR WINDOWS TOTAL TRANSMITTED BEAM SOLAR RADIATION ENERGY", 0, 0 },
			}
		},
		{
			"AVERAGEOUTDOORCONDITIONSMONTHLY",
			{
				{ "*", "SITE OUTDOOR AIR DRYBULB TEMPERATURE", 0, 0 },
				{ "*", "SITE OUTDOOR AIR WETBULB TEMPERATURE", 0, 0 },
				{ "*", "SITE OUTDOOR AIR DEWPOINT TEMPERATURE", 0, 0 },
				{ "*", "SITE WIND SPEED", 0, 0 },
				{ "*", "SITE SKY TEMPERATURE", 0, 0 },
				{ "*", "SITE DIFFUSE SOLAR RADIATION RATE PER AREA", 0, 0 },
				{ "*", "SITE DIRECT SOLAR RADIATION RATE PER AREA", 0, 0 },
				{ "*", "SITE RAIN STATUS", 0, 0 },
			}
		},
		{
			"OUTDOORCONDITIONSMAXIMUMDRYBULBMONTHLY",
			{
				{ "*", "SITE OUTDOOR AIR DRYBULB TEMPERATURE", 0, 0 },
				{ "*", "SITE OUTDOOR AIR WETBULB TEMPERATURE", 0, 0 },
				{ "*", "SITE OUTDOOR AIR DEWPOINT TEMPERATURE", 0, 0 },
				{ "*", "SITE WIND SPEED", 0, 0 },
				{ "*", "SITE SKY TEMPERATURE", 0, 0 },
				{ "*", "SITE DIFFUSE SOLAR RADIATION RATE PER AREA", 0, 0 },
				{ "*", "SITE DIRECT SOLAR RADIATION RATE PER AREA", 0, 0 },
			}
		},
		{
			"OUTDOORCONDITIONSMINIMUMDRYBULBMONTHLY",
			{
				{ "*", "SITE OUTDOOR AIR DRYBULB TEMPERATURE", 0, 0 },
				{ "*", "SITE OUTDOOR AIR WETBULB TEMPERATURE", 0, 0 },
				{ "*", "SITE OUTDOOR AIR DEWPOINT TEMPERATURE", 0, 0 },
				{ "*", "SITE WIND SPEED", 0, 0 },
				{ "*", "SITE SKY TEMPERATURE", 0, 0 },
				{ "*", "SITE DIFFUSE SOLAR RADIATION RATE PER AREA", 0, 0 },
				{ "*", "SITE DIRECT SOLAR RADIATION RATE PER AREA", 0, 0 },
			}
		},
		{
			"OUTDOORCONDITIONSMAXIMUMWETBULBMONTHLY",
			{
				{ "*", "SITE OUTDOOR AIR WETBULB TEMPERATURE", 0, 0 },
				{ "*", "SITE OUTDOOR AIR DRYBULB TEMPERATURE", 0, 0 },
				{ "*", "SITE OUTDOOR AIR DEWPOINT TEMPERATURE", 0, 0 },
				{ "*", "SITE WIND SPEED", 0, 0 },
				{ "*", "SITE SKY TEMPERATURE", 0, 0 },
				{ "*", "SITE DIFFUSE SOLAR RADIATION RATE PER AREA", 0, 0 },
				{ "*", "SITE DIRECT SOLAR RADIATION RATE PER AREA", 0, 0 },
			}
		},
		{
			"OUTDOORCONDITIONSMAXIMUMDEWPOINTMONTHLY",
			{
				{ "*", "SITE OUTDOOR AIR DEWPOINT TEMPERATURE", 0, 0 },
				{ "*", "SITE OUTDOOR AIR DRYBULB TEMPERATURE", 0, 0 },
				{ "*", "SITE OUTDOOR AIR WETBULB TEMPERATURE", 0, 0 },
				{ "*", "SITE WIND SPEED", 0, 0 },
				{ "*", "SITE SKY TEMPERATURE", 0, 0 },
				{ "*", "SITE DIFFUSE SOLAR RADIATION RATE PER AREA", 0, 0 },
				{ "*", "SITE DIRECT SOLAR RADIATION RATE PER AREA", 0, 0 },
			}
		},
		{
			"OUTDOORGROUNDCONDITIONSMONTHLY",
			{
				{ "*", "SITE GROUND TEMPERATURE", 0, 0 },
				{ "*", "SITE SURFACE GROUND TEMPERATURE", 0, 0 },
				{ "*", "SITE DEEP GROUND TEMPERATURE", 0, 0 },
				{ "*", "SITE MAINS WATER TEMPERATURE", 0, 0 },
				{ "*", "SITE GROUND REFLECTED SOLAR RADIATION RATE PER AREA", 0, 0 },
				{ "*", "SITE SNOW ON GROUND STATUS", 0, 0 },
			}
		},
		{
			"WINDOWACREPORTMONTHLY",
			{
				{ "*", "ZONE WINDOW AIR CONDITIONER TOTAL COOLING ENERGY", 0, 0 },
				{ "*", "ZONE WINDOW AIR CONDITIONER ELECTRIC ENERGY", 0, 0 },
				{ "*", "ZONE WINDOW AIR CONDITIONER SENSIBLE COOLING ENERGY", 0, 0 },
				{ "*", "ZONE WINDOW AIR CONDITIONER LATENT COOLING ENERGY", 0, 0 },
				{ "*", "ZONE WINDOW AIR CONDITIONER TOTAL COOLING RATE", 0, 0 },
				{ "*", "ZONE WINDOW AIR CONDITIONER SENSIBLE COOLING RATE", 0, 0 },
				{ "*", "ZONE WINDOW AIR CONDITIONER LATENT COOLING RATE", 0, 0 },
				{ "*", "ZONE WINDOW AIR CONDITIONER ELECTRIC POWER", 0, 0 },
			}
		},
		{
			"WATERHEATERREPORTMONTHLY",
			{
				{ "*", "WATER HEATER TOTAL DEMAND HEAT TRANSFER ENERGY", 0, 0 },
				{ "*", "WATER HEATER USE SIDE HEAT TRANSFER ENERGY", 0, 0 },
				{ "*", "WATER HEATER BURNER HEATING ENERGY", 0, 0 },
				{ "*", "WATER HEATER GAS CONSUMPTION", 0, 0 },
				{ "*", "WATER HEATER LOSS DEMAND ENERGY", 0, 0 },
				{ "*", "WATER HEATER HEAT LOSS ENERGY", 0, 0 },
				{ "*", "WATER HEATER TANK TEMPERATURE", 0, 0 },
				{ "*", "WATER HEATER HEAT RECOVERY SUPPLY ENERGY", 0, 0 },
				{ "*", "WATER HEATER SOURCE ENERGY", 0, 0 },
			}
		},
		{
			"GENERATORREPORTMONTHLY",
			{
				{ "*", "GENERATOR PRODUCED ELECTRIC ENERGY", 0, 0 },
				{ "*", "GENERATOR DIESEL CONSUMPTION", 0, 0 },
				{ "*", "GENERATOR GAS CONSUMPTION", 0, 0 },
				{ "*", "GENERATOR TOTAL HEAT RECOVERY", 0, 0 },
				{ "*", "GENERATOR JACKET HEAT RECOVERY ENERGY", 0, 0 },
				{ "*", "GENERATOR LUBE HEAT RECOVERY", 0, 0 },
				{ "*", "GENERATOR EXHAUST HEAT RECOVERY ENERGY", 0, 0 },
				{ "*", "GENERATOR EXHAUST AIR TEMPERATURE", 0, 0 },
			}
		},
		{
			"DAYLIGHTINGREPORTMONTHLY",
			{
				{ "*", "SITE EXTERIOR BEAM NORMAL ILLUMINANCE", 0, 0 },
				{ "*", "DAYLIGHTING LIGHTING POWER MULTIPLIER", 0, 0 },
				{ "*", "DAYLIGHTING REFERENCE POINT 1 ILLUMINANCE", 0, 0 },
				{ "*", "DAYLIGHTING REFERENCE POINT 1 GLARE INDEX", 0, 0 },
				{ "*", "DAYLIGHTING REFERENCE POINT 1 GLARE INDEX SETPOINT EXCEEDED TIME", 0, 0 },
				{ "*", "DAYLIGHTING REFERENCE POINT 1 DAYLIGHT ILLUMINANCE SETPOINT EXCEEDED TIME", 0, 0 },
				{ "*", "DAYLIGHTING REFERENCE POINT 2 ILLUMINANCE", 0, 0 },
				{ "*", "DAYLIGHTING REFERENCE POINT 2 GLARE INDEX", 0, 0 },
				{ "*", "DAYLIGHTING REFERENCE POINT 2 GLARE INDEX SETPOINT EXCEEDED TIME", 0, 0 },
				{ "*", "DAYLIGHTING REFERENCE POINT 2 DAYLIGHT ILLUMINANCE SETPOINT EXCEEDED TIME", 0, 0 },
			}
		},
		{
			"COILREPORTMONTHLY",
			{
				{ "*", "HEATING COIL HEATING ENERGY", 0, 0 },
				{ "*", "HEATING COIL HEATING RATE", 0, 0 },
				{ "*", "COOLING COIL SENSIBLE COOLING ENERGY", 0, 0 },
				{ "*", "COOLING COIL TOTAL COOLING ENERGY", 0, 0 },
				{ "*", "COOLING COIL TOTAL COOLING RATE", 0, 0 },
				{ "*", "COOLING COIL SENSIBLE COOLING RATE", 0, 0 },
				{ "*", "COOLING COIL WETTED AREA FRACTION", 0, 0 },
			}
		},
		{
			"PLANTLOOPDEMANDREPORTMONTHLY",
			{
				{ "*", "PLANT SUPPLY SIDE COOLING DEMAND RATE", 0, 0 },
				{ "*", "PLANT SUPPLY SIDE HEATING DEMAND RATE", 0, 0 },
			}
		},
		{
			"FANREPORTMONTHLY",
			{
				{ "*", "FAN ELECTRIC ENERGY", 0, 0 },
				{ "*", "FAN RISE IN AIR TEMPERATURE", 0, 0 },
				{ "*", "FAN ELECTRIC POWER", 0, 0 },
			}
		},
		{
			"PUMPREPORTMONTHLY",
			{
				{ "*", "PUMP ELECTRIC ENERGY", 0, 0 },
				{ "*", "PUMP FLUID HEAT GAIN ENERGY", 0, 0 },
				{ "*", "PUMP ELECTRIC POWER", 0, 0 },
				{ "*", "PUMP SHAFT POWER", 0, 0 },
				{ "*", "PUMP FLUID HEAT GAIN RATE", 0, 0 },
				{ "*", "PUMP OUTLET TEMPERATURE", 0, 0 },
				{ "*", "PUMP MASS FLOW RATE", 0, 0 },
			}
		},
		{
			"CONDLOOPDEMANDREPORTMONTHLY",
			{
				{ "*", "PLANT SUPPLY SIDE COOLING DEMAND RATE", 0, 0 },
				{ "*", "PLANT SUPPLY SIDE HEATING DEMAND RATE", 0, 0 },
				{ "*", "PLANT SUPPLY SIDE INLET TEMPERATURE", 0, 0 },
				{ "*", "PLANT SUPPLY SIDE OUTLET TEMPERATURE", 0, 0 },
			}
		},
		{
			"ZONETEMPERATUREOSCILLATIONREPORTMONTHLY",
			{
				{ "*", "ZONE OSCILLATING TEMPERATURES TIME", 0, 0 },
				{ "*", "ZONE PEOPLE OCCUPANT COUNT", 0, 0 },
			}
		},
		{
			"AIRLOOPSYSTEMENERGYANDWATERUSEMONTHLY",
			{
				{ "*", "AIR SYSTEM HOT WATER ENERGY", 0, 0 },
				{ "*", "AIR SYSTEM STEAM ENERGY", 0, 0 },
				{ "*", "AIR SYSTEM CHILLED WATER ENERGY", 0, 0 },
				{ "*", "AIR SYSTEM ELECTRIC ENERGY", 0, 0 },
				{ "*", "AIR SYSTEM GAS ENERGY", 0, 0 },
				{ "*", "AIR SYSTEM WATER VOLUME", 0, 0 },
			}
		},
		{
			"AIRLOOPSYSTEMCOMPONENTLOADSMONTHLY",
			{
				{ "*", "AIR SYSTEM FAN AIR HEATING ENERGY", 0, 0 },
				{ "*", "AIR SYSTEM COOLING COIL TOTAL COOLING ENERGY", 0, 0 },
				{ "*", "AIR SYSTEM HEATING COIL TOTAL HEATING ENERGY", 0, 0 },
				{ "*", "AIR SYSTEM HEAT EXCHANGER TOTAL HEATING ENERGY", 0, 0 },
				{ "*", "AIR SYSTEM HEAT EXCHANGER TOTAL COOLING ENERGY", 0, 0 },
				{ "*", "AIR SYSTEM HUMIDIFIER TOTAL HEATING ENERGY", 0, 0 },
				{ "*", "AIR SYSTEM EVAPORATIVE COOLER TOTAL COOLING ENERGY", 0, 0 },
				{ "*", "AIR SYSTEM DESICCANT DEHUMIDIFIER TOTAL COOLING ENERGY", 0, 0 },
			}
		},
		{
			"AIRLOOPSYSTEMCOMPONENTENERGYUSEMONTHLY",
			{
				{ "*", "AIR SYSTEM FAN ELECTRIC ENERGY", 0, 0 },
				{ "*", "AIR SYSTEM HEATING COIL HOT WATER ENERGY", 0, 0 },
				{ "*", "AIR SYSTEM COOLING COIL CHILLED WATER ENERGY", 0, 0 },
				{ "*", "AIR SYSTEM DX HEATING COIL ELECTRIC ENERGY", 0, 0 },
				{ "*", "AIR SYSTEM DX COOLING COIL ELECTRIC ENERGY", 0, 0 },
				{ "*", "AIR SYSTEM HEATING COIL ELECTRIC ENERGY", 0, 0 },
				{ "*", "AIR SYSTEM HEATING COIL GAS ENERGY", 0, 0 },
				{ "*", "AIR SYSTEM HEATING COIL STEAM ENERGY", 0, 0 },
				{ "*", "AIR SYSTEM HUMIDIFIER ELECTRIC ENERGY", 0, 0 },
				{ "*", "AIR SYSTEM EVAPORATIVE COOLER ELECTRIC ENERGY", 0, 0 },
				{ "*", "AIR SYSTEM DESICCANT DEHUMIDIFIER ELECTRIC ENERGY", 0, 0 },
			}
		},
		{
			"MECHANICALVENTILATIONLOADSMONTHLY",
			{
				{ "*", "ZONE MECHANICAL VENTILATION NO LOAD HEAT REMOVAL ENERGY", 0, 0 },
				{ "*", "ZONE MECHANICAL VENTILATION COOLING LOAD INCREASE ENERGY", 0, 0 },
				{ "*", "ZONE MECHANICAL VENTILATION COOLING LOAD INCREASE DUE TO OVERHEATING ENERGY", 0, 0 },
				{ "*", "ZONE MECHANICAL VENTILATION COOLING LOAD DECREASE ENERGY", 0, 0 },
				{ "*", "ZONE MECHANICAL VENTILATION NO LOAD HEAT ADDITION ENERGY", 0, 0 },
				{ "*", "ZONE MECHANICAL VENTILATION HEATING LOAD INCREASE ENERGY", 0, 0 },
				{ "*", "ZONE MECHANICAL VENTILATION HEATING LOAD INCREASE DUE TO OVERCOOLING ENERGY", 0, 0 },
				{ "*", "ZONE MECHANICAL VENTILATION HEATING LOAD DECREASE ENERGY", 0, 0 },
				{ "*", "ZONE MECHANICAL VENTILATION AIR CHANGES PER HOUR", 0, 0 },
			}
		}
	});

	for ( auto const result : results_map ) {
		int index = 1;
		DataOutputs::OutputVariablesForSimulation.allocate( 10000 );
		AddVariablesForMonthlyReport( result.first );
		for ( auto const result_tuple : result.second ) {
			EXPECT_EQ( std::get<0>( result_tuple ), DataOutputs::OutputVariablesForSimulation( index ).Key );
			EXPECT_EQ( std::get<1>( result_tuple ), DataOutputs::OutputVariablesForSimulation( index ).VarName );
			EXPECT_EQ( std::get<2>( result_tuple ), DataOutputs::OutputVariablesForSimulation( index ).Previous );
			EXPECT_EQ( std::get<3>( result_tuple ), DataOutputs::OutputVariablesForSimulation( index ).Next );
			++index;
		}
		DataOutputs::OutputVariablesForSimulation.deallocate();
		DataOutputs::NumConsideredOutputVariables = 0;
	}

}

TEST_F( InputProcessorFixture, preScanReportingVariables )
{
	std::string const idf_objects = delimited_string({
		"Output:Variable,*,Site Outdoor Air Drybulb Temperature,timestep;",
		"Output:Variable,,Site Outdoor Air Drybulb Temperature,hourly;",
	});

	ASSERT_FALSE( process_idf( idf_objects ) );

	// reset global variable since process_idf() calls PreScanReportingVariables()
	DataOutputs::NumConsideredOutputVariables = 0;

	PreScanReportingVariables();

	EXPECT_FALSE( has_cout_output() );
	EXPECT_FALSE( has_cerr_output() );

	EXPECT_EQ( "*", DataOutputs::OutputVariablesForSimulation( 1 ).Key );
	EXPECT_EQ( "SITE OUTDOOR AIR DRYBULB TEMPERATURE", DataOutputs::OutputVariablesForSimulation( 1 ).VarName );
	EXPECT_EQ( 0, DataOutputs::OutputVariablesForSimulation( 1 ).Previous );
	EXPECT_EQ( 0, DataOutputs::OutputVariablesForSimulation( 1 ).Next );

}
