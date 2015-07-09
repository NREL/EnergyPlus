// EnergyPlusFixture Unit Tests

// Google Test Headers
#include <gtest/gtest.h>

// EnergyPlus Headers
#include "Fixtures/IdfParserFixture.hh"
#include "Fixtures/EnergyPlusMetaFixture.hh"

// These must be here otherwise the linker will complain. This is so the shared resource m_idd_cache can be used by unit tests.
struct InputProcessorCache;
std::unique_ptr<EnergyPlus::EnergyPlusFixture::InputProcessorCache> EnergyPlus::EnergyPlusFixture::m_idd_cache = nullptr;

namespace EnergyPlus {

	TEST_F( EnergyPlusMetaFixture, compareIDF )
	{
		std::string const idf_objects = delimited_string({
			"Version,8.3;",
			"	 BUILDING, Bldg2, 0.0, Suburbs, .04, .4, FullExterior, 25, 6;",
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

		EXPECT_FALSE( compare_idf( "VERSION", 1, 0, 1, { "8.3" }, { false }, {}, {} ) );

		EXPECT_FALSE( compare_idf( "OUTPUT:SQLITE", 1, 0, 739, { "SIMPLEANDTABULAR" }, { false }, {}, {} ) );

		EXPECT_FALSE( compare_idf( 
			"BUILDINGSURFACE:DETAILED", 
			8, 
			14, 
			98, 
			{ "ZN001:WALL001", "WALL", "R13WALL", "MAIN ZONE", "OUTDOORS", "", "SUNEXPOSED", "WINDEXPOSED" }, 
			{ false, false, false, false, false, true, false, false }, 
			{ 0.5, 4, 0, 0, 4.572, 0, 0, 0, 15.24, 0, 0, 15.24, 0, 4.572 }, 
			{ false, false, false, false, false, false, false, false, false, false, false, false, false, false }
		) );

	}

	TEST_F( EnergyPlusMetaFixture, processIDD_Full_IDD )
	{
		using namespace InputProcessor;
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

	TEST_F( EnergyPlusMetaFixture, processIDD )
	{
		using namespace InputProcessor;
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

	TEST_F( EnergyPlusMetaFixture, processIDF )
	{
		using namespace InputProcessor;
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

	TEST_F( EnergyPlusMetaFixture, processIDF_Cached )
	{
		using namespace InputProcessor;
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

	TEST_F( IdfParserFixture, decode ) {
		std::string const test_object( delimited_string( {
			"Version,8.3;",
			"  Building,",
			"    Ref Bldg Medium Office New2004_v1.3_5.0,  !- Name",
			"    0.0000,                  !- North Axis {deg}",
			"    City,                    !- Terrain",
			"    0.0400,                  !- Loads Convergence Tolerance Value",
			"    0.2000,                  !- Temperature Convergence Tolerance Value {deltaC}",
			"    FullInteriorAndExterior, !- Solar Distribution",
			"    25,                      !- Maximum Number of Warmup Days",
			"    6;  ",
		} ) );

		auto const output = decode( test_object );

		EXPECT_EQ( std::vector< std::vector< std::string > >({ { "Version", "8.3" }, { "Building", "Ref Bldg Medium Office New2004_v1.3_5.0", "0.0000", "City", "0.0400", "0.2000", "FullInteriorAndExterior", "25", "6" } }), output );
	}

	TEST_F( IdfParserFixture, decode_success ) {
		bool success = true;
		std::string const test_object( delimited_string( {
			"Version,8.3;",
			"  Building,",
			"    Ref Bldg Medium Office New2004_v1.3_5.0,  !- Name",
			"    0.0000,                  !- North Axis {deg}",
			"    City,                    !- Terrain",
			"    0.0400,                  !- Loads Convergence Tolerance Value",
			"    0.2000,                  !- Temperature Convergence Tolerance Value {deltaC}",
			"    FullInteriorAndExterior, !- Solar Distribution",
			"    25,                      !- Maximum Number of Warmup Days",
			"    6;  ",
		} ) );

		auto const output = decode( test_object, success );

		EXPECT_EQ( std::vector< std::vector< std::string > >({ { "Version", "8.3" }, { "Building", "Ref Bldg Medium Office New2004_v1.3_5.0", "0.0000", "City", "0.0400", "0.2000", "FullInteriorAndExterior", "25", "6" } }), output );
		EXPECT_TRUE( success );
	}

	TEST_F( IdfParserFixture, parse_idf ) {
		size_t index = 0;
		bool success = true;
		std::string const test_object( delimited_string( {
			"Version,8.3;",
			"  Building,",
			"    Ref Bldg Medium Office New2004_v1.3_5.0,  !- Name",
			"    0.0000,                  !- North Axis {deg}",
			"    City,                    !- Terrain",
			"    0.0400,                  !- Loads Convergence Tolerance Value",
			"    0.2000,                  !- Temperature Convergence Tolerance Value {deltaC}",
			"    FullInteriorAndExterior, !- Solar Distribution",
			"    25,                      !- Maximum Number of Warmup Days",
			"    6;  ",
		} ) );

		auto const output = parse_idf( test_object, index, success );

		EXPECT_EQ( std::vector< std::vector< std::string > >({ { "Version", "8.3" }, { "Building", "Ref Bldg Medium Office New2004_v1.3_5.0", "0.0000", "City", "0.0400", "0.2000", "FullInteriorAndExterior", "25", "6" } }), output );
		EXPECT_EQ( 434ul, index );
		EXPECT_TRUE( success );
	}

	TEST_F( IdfParserFixture, parse_object ) {
		size_t index = 0;
		bool success = true;
		std::string const test_object( delimited_string( {
			"  Building,",
			"    Ref Bldg Medium Office New2004_v1.3_5.0,  !- Name",
			"    0.0000,                  !- North Axis {deg}",
			"    City,                    !- Terrain",
			"    0.0400,                  !- Loads Convergence Tolerance Value",
			"    0.2000,                  !- Temperature Convergence Tolerance Value {deltaC}",
			"    FullInteriorAndExterior, !- Solar Distribution",
			"    25,                      !- Maximum Number of Warmup Days",
			"    6;",
		} ) );

		auto const output_vector = parse_object( test_object, index, success );
		EXPECT_EQ( std::vector< std::string >({ "Building", "Ref Bldg Medium Office New2004_v1.3_5.0", "0.0000", "City", "0.0400", "0.2000", "FullInteriorAndExterior", "25", "6" }), output_vector );
		EXPECT_EQ( test_object.size() - 1, index );
		EXPECT_TRUE( success );
	}

	TEST_F( IdfParserFixture, eat_whitespace ) {
		size_t index = 0;
		eat_whitespace( "    test", index );
		EXPECT_EQ( 4ul, index );

		index = 0;
		eat_whitespace( "t   test", index );
		EXPECT_EQ( 0ul, index );
	}

	TEST_F( IdfParserFixture, eat_comment ) {
		size_t index = 0;
		eat_comment( "!- North Axis {deg}\n", index );
		EXPECT_EQ( 20ul, index );

		index = 0;
		eat_comment( "                    !- Terrain\n", index );
		EXPECT_EQ( 31ul, index );

		index = 0;
		eat_comment( "  !- Name\n    0.0000", index );
		EXPECT_EQ( 10ul, index );

		index = 0;
		eat_comment( "  !- Name\n\r    0.0000", index );
		EXPECT_EQ( 10ul, index );
	}

	TEST_F( IdfParserFixture, parse_string ) {
		size_t index = 0;
		bool success = true;
		std::string output_string;

		output_string = parse_string( "test_string", index, success );
		EXPECT_EQ( "test_string", output_string );
		EXPECT_EQ( 11ul, index );
		EXPECT_TRUE( success );

		index = 0;
		success = true;
		output_string = parse_string( "test string", index, success );
		EXPECT_EQ( "test string", output_string );
		EXPECT_EQ( 11ul, index );
		EXPECT_TRUE( success );

		index = 0;
		success = true;
		output_string = parse_string( "-1234.1234", index, success );
		EXPECT_EQ( "-1234.1234", output_string );
		EXPECT_EQ( 10ul, index );
		EXPECT_TRUE( success );

		index = 0;
		success = true;
		output_string = parse_string( R"(\b\t/\\\";)", index, success );
		EXPECT_EQ( "\b\t/\\\"", output_string );
		EXPECT_EQ( 9ul, index );
		EXPECT_TRUE( success );

		index = 0;
		success = true;
		output_string = parse_string( R"(test \n string)", index, success );
		EXPECT_EQ( "", output_string );
		EXPECT_EQ( 7ul, index );
		EXPECT_FALSE( success );

		index = 0;
		success = true;
		output_string = parse_string( R"(! this is a comment \n)", index, success );
		EXPECT_EQ( "", output_string );
		EXPECT_EQ( 0ul, index );
		EXPECT_TRUE( success );
	}

	TEST_F( IdfParserFixture, parse_value ) {
		size_t index = 0;
		bool success = true;
		std::string output_string;

		output_string = parse_value( "test_string", index, success );
		EXPECT_EQ( "test_string", output_string );
		EXPECT_EQ( 11ul, index );
		EXPECT_TRUE( success );

		index = 0;
		success = true;
		output_string = parse_value( ", test_string", index, success );
		EXPECT_EQ( "", output_string );
		EXPECT_EQ( 0ul, index );
		EXPECT_FALSE( success );

		index = 0;
		success = true;
		output_string = parse_value( R"(test \n string)", index, success );
		EXPECT_EQ( "", output_string );
		EXPECT_EQ( 7ul, index );
		EXPECT_FALSE( success );

		index = 0;
		success = true;
		output_string = parse_value( "; test_string", index, success );
		EXPECT_EQ( "", output_string );
		EXPECT_EQ( 0ul, index );
		EXPECT_FALSE( success );

		index = 0;
		success = true;
		output_string = parse_value( "! test_string", index, success );
		EXPECT_EQ( "", output_string );
		EXPECT_EQ( 0ul, index );
		EXPECT_FALSE( success );
	}

	TEST_F( IdfParserFixture, look_ahead ) {
		std::string const test_input( "B , ! t ; `" );
		size_t index = 0;
		size_t token = look_ahead( test_input, index );
		EXPECT_EQ( 0ul, index );
		EXPECT_EQ( 5ul, token );
		index = 2;
		token = look_ahead( test_input, index );
		EXPECT_EQ( 2ul, index );
		EXPECT_EQ( 3ul, token );
		index = 3;
		token = look_ahead( test_input, index );
		EXPECT_EQ( 3ul, index );
		EXPECT_EQ( 2ul, token );
		index = 5;
		token = look_ahead( test_input, index );
		EXPECT_EQ( 5ul, index );
		EXPECT_EQ( 5ul, token );
		index = 7;
		token = look_ahead( test_input, index );
		EXPECT_EQ( 7ul, index );
		EXPECT_EQ( 4ul, token );
		index = 9;
		token = look_ahead( test_input, index );
		EXPECT_EQ( 9ul, index );
		EXPECT_EQ( 0ul, token );
		index = test_input.size();
		token = look_ahead( test_input, index );
		EXPECT_EQ( test_input.size(), index );
		EXPECT_EQ( 1ul, token );

	}

	TEST_F( IdfParserFixture, next_token ) {
		size_t index = 0;

		std::string const test_input( "B , ! t ; `" );
		size_t token = next_token( test_input, index );
		EXPECT_EQ( 1ul, index );
		EXPECT_EQ( 5ul, token );
		token = next_token( test_input, index );
		EXPECT_EQ( 3ul, index );
		EXPECT_EQ( 3ul, token );
		token = next_token( test_input, index );
		EXPECT_EQ( 5ul, index );
		EXPECT_EQ( 2ul, token );
		token = next_token( test_input, index );
		EXPECT_EQ( 7ul, index );
		EXPECT_EQ( 5ul, token );
		token = next_token( test_input, index );
		EXPECT_EQ( 9ul, index );
		EXPECT_EQ( 4ul, token );
		token = next_token( test_input, index );
		EXPECT_EQ( 10ul, index );
		EXPECT_EQ( 0ul, token );
		index = test_input.size();
		token = next_token( test_input, index );
		EXPECT_EQ( test_input.size() , index );
		EXPECT_EQ( 1ul, token );

	}

}
